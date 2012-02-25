/*
** $Id: lvm.c,v 2.147 2011/12/07 14:43:55 roberto Exp $
** Lua virtual machine
** See Copyright Notice in lua.h
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define lvm_c
#define LUA_CORE

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"
#include "lvm.h"
#include "lvmspec.h"


// #define DEBUG_PRINT


/* limit for table tag-method chains (to avoid loops) */
#define MAXTAGLOOP	100


const TValue *luaV_tonumber (const TValue *obj, TValue *n) {
  lua_Number num;
  if (ttisnumber(obj)) return obj;
  if (ttisstring(obj) && luaO_str2d(svalue(obj), tsvalue(obj)->len, &num)) {
    setnvalue(n, num);
    return n;
  }
  else
    return NULL;
}


int luaV_tostring (lua_State *L, StkId obj) {
  if (!ttisnumber(obj))
    return 0;
  else {
    char s[LUAI_MAXNUMBER2STR];
    lua_Number n = nvalue(obj);
    int l = lua_number2str(s, n);
    setsvalue2s(L, obj, luaS_newlstr(L, s, l));
    return 1;
  }
}


static void traceexec (lua_State *L) {
  CallInfo *ci = L->ci;
  lu_byte mask = L->hookmask;
  if ((mask & LUA_MASKCOUNT) && L->hookcount == 0) {
    resethookcount(L);
    luaD_hook(L, LUA_HOOKCOUNT, -1);
  }
  if (mask & LUA_MASKLINE) {
    Proto *p = ci_func(ci)->p;
    int npc = pcRel(ci->u.l.savedpc, p);
    int newline = getfuncline(p, npc);
    if (npc == 0 ||  /* call linehook when enter a new function, */
        ci->u.l.savedpc <= L->oldpc ||  /* when jump back (loop), or when */
        newline != getfuncline(p, pcRel(L->oldpc, p)))  /* enter a new line */
      luaD_hook(L, LUA_HOOKLINE, newline);
  }
  L->oldpc = ci->u.l.savedpc;
  if (L->status == LUA_YIELD) {  /* did hook yield? */
    ci->u.l.savedpc--;  /* undo increment (resume will increment it again) */
    luaD_throw(L, LUA_YIELD);
  }
}


static void callTM (lua_State *L, const TValue *f, const TValue *p1,
                    const TValue *p2, TValue *p3, int hasres) {
  ptrdiff_t result = savestack(L, p3);
  setobj2s(L, L->top++, f);  /* push function */
  setobj2s(L, L->top++, p1);  /* 1st argument */
  setobj2s(L, L->top++, p2);  /* 2nd argument */
  if (!hasres)  /* no result? 'p3' is third argument */
    setobj2s(L, L->top++, p3);  /* 3rd argument */
  luaD_checkstack(L, 0);
  /* metamethod may yield only when called from Lua code */
  luaD_call(L, L->top - (4 - hasres), hasres, isLua(L->ci));
  if (hasres) {  /* if has result, move it to its place */
    p3 = restorestack(L, result);
    setobjs2s(L, p3, --L->top);
  }
}


void luaV_gettable (lua_State *L, const TValue *t, TValue *key, StkId val) {
  int loop;
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    const TValue *tm;
    if (ttistable(t)) {  /* `t' is a table? */
      Table *h = hvalue(t);
      const TValue *res = luaH_get(h, key);
      if (!ttisnil(res) ||  /* result is not nil? */
          (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) { /* no TM? */
        setobj2s(L, val, res);
        return;
      }
      /* else will try the tag method */
    }
    else if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_INDEX)))
      luaG_typeerror(L, t, "index");
    if (ttisfunction(tm)) {
      callTM(L, tm, t, key, val, 1);
      return;
    }
    t = tm;  /* else repeat with 'tm' */
  }
  luaG_runerror(L, "loop in gettable");
}

void gettable_tm (lua_State *L, const TValue *t, const TValue *tm, 
                  TValue *key, StkId val) {
  int loop;
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    if (ttisfunction(tm)) {
      callTM(L, tm, t, key, val, 1);
      return;
    }
    t = tm;
    if (ttistable(t)) {
      Table *h = hvalue(t);
      const TValue *res = luaH_get(h, key);
      if (!ttisnil(res) ||
          (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) {
        setobj2s(L, val, res);
        return;
      }
      /* else will try the tag method */
    }
    else if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_INDEX)))
      luaG_typeerror(L, t, "index");
  }
  luaG_runerror(L, "loop in gettable");
}


void luaV_settable (lua_State *L, const TValue *t, TValue *key, StkId val) {
  int loop;
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    const TValue *tm;
    if (ttistable(t)) {  /* `t' is a table? */
      Table *h = hvalue(t);
      TValue *oldval = cast(TValue *, luaH_get(h, key));
      /* if previous value is not nil, there must be a previous entry
         in the table; moreover, a metamethod has no relevance */
      if (!ttisnil(oldval) ||
         /* previous value is nil; must check the metamethod */
         ((tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL &&
         /* no metamethod; is there a previous entry in the table? */
         (oldval != luaO_nilobject ||
         /* no previous entry; must create one. (The next test is
            always true; we only need the assignment.) */
         (oldval = luaH_newkey(L, h, key), 1)))) {
        /* no metamethod and (now) there is an entry with given key */
        setobj2t(L, oldval, val);  /* assign new value to that entry */
        invalidateTMcache(h);
        luaC_barrierback(L, obj2gco(h), val);
        return;
      }
      /* else will try the metamethod */
    }
    else  /* not a table; check metamethod */
      if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX)))
        luaG_typeerror(L, t, "index");
    /* there is a metamethod */
    if (ttisfunction(tm)) {
      callTM(L, tm, t, key, val, 0);
      return;
    }
    t = tm;  /* else repeat with 'tm' */
  }
  luaG_runerror(L, "loop in settable");
}

void settable_tm (lua_State *L, const TValue *t, const TValue *tm, 
                  TValue *key, StkId val) {
  int loop;
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    if (ttisfunction(tm)) {
      callTM(L, tm, t, key, val, 0);
      return;
    }
    t = tm;
    if (ttistable(t)) {
      Table *h = hvalue(t);
      TValue *oldval = cast(TValue *, luaH_get(h, key));
      if (!ttisnil(oldval) ||
         ((tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL &&
         (oldval != luaO_nilobject ||
         (oldval = luaH_newkey(L, h, key), 1)))) {
        setobj2t(L, oldval, val);
        invalidateTMcache(h);
        luaC_barrierback(L, obj2gco(h), val);
        return;
      }
      /* else will try the metamethod */
    }
    else if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX)))
        luaG_typeerror(L, t, "index");
  }
  luaG_runerror(L, "loop in settable");
}


static int call_binTM (lua_State *L, const TValue *p1, const TValue *p2,
                       StkId res, TMS event) {
  const TValue *tm = luaT_gettmbyobj(L, p1, event);  /* try first operand */
  if (ttisnil(tm))
    tm = luaT_gettmbyobj(L, p2, event);  /* try second operand */
  if (ttisnil(tm)) return 0;
  callTM(L, tm, p1, p2, res, 1);
  return 1;
}


static const TValue *get_equalTM (lua_State *L, Table *mt1, Table *mt2,
                                  TMS event) {
  const TValue *tm1 = fasttm(L, mt1, event);
  const TValue *tm2;
  if (tm1 == NULL) return NULL;  /* no metamethod */
  if (mt1 == mt2) return tm1;  /* same metatables => same metamethods */
  tm2 = fasttm(L, mt2, event);
  if (tm2 == NULL) return NULL;  /* no metamethod */
  if (luaV_rawequalobj(tm1, tm2))  /* same metamethods? */
    return tm1;
  return NULL;
}


static int call_orderTM (lua_State *L, const TValue *p1, const TValue *p2,
                         TMS event) {
  if (!call_binTM(L, p1, p2, L->top, event))
    return -1;  /* no metamethod */
  else
    return !l_isfalse(L->top);
}


static int l_strcmp (const TString *ls, const TString *rs) {
  const char *l = getstr(ls);
  size_t ll = ls->tsv.len;
  const char *r = getstr(rs);
  size_t lr = rs->tsv.len;
  for (;;) {
    int temp = strcoll(l, r);
    if (temp != 0) return temp;
    else {  /* strings are equal up to a `\0' */
      size_t len = strlen(l);  /* index of first `\0' in both strings */
      if (len == lr)  /* r is finished? */
        return (len == ll) ? 0 : 1;
      else if (len == ll)  /* l is finished? */
        return -1;  /* l is smaller than r (because r is not finished) */
      /* both strings longer than `len'; go on comparing (after the `\0') */
      len++;
      l += len; ll -= len; r += len; lr -= len;
    }
  }
}


int luaV_lessthan (lua_State *L, const TValue *l, const TValue *r) {
  int res;
  if (ttisnumber(l) && ttisnumber(r))
    return luai_numlt(L, nvalue(l), nvalue(r));
  else if (ttisstring(l) && ttisstring(r))
    return l_strcmp(rawtsvalue(l), rawtsvalue(r)) < 0;
  else if ((res = call_orderTM(L, l, r, TM_LT)) < 0)
    luaG_ordererror(L, l, r);
  return res;
}


static int lessthan_tm (lua_State *L, const TValue *l, const TValue *r) {
  int res = call_orderTM(L, l, r, TM_LT);
  if (res < 0) luaG_ordererror(L, l, r);
  return res;
}


int luaV_lessequal (lua_State *L, const TValue *l, const TValue *r) {
  int res;
  if (ttisnumber(l) && ttisnumber(r))
    return luai_numle(L, nvalue(l), nvalue(r));
  else if (ttisstring(l) && ttisstring(r))
    return l_strcmp(rawtsvalue(l), rawtsvalue(r)) <= 0;
  else if ((res = call_orderTM(L, l, r, TM_LE)) >= 0)  /* first try `le' */
    return res;
  else if ((res = call_orderTM(L, r, l, TM_LT)) < 0)  /* else try `lt' */
    luaG_ordererror(L, l, r);
  return !res;
}


static int lessequal_tm (lua_State *L, const TValue *l, const TValue *r) {
  int res = call_orderTM(L, l, r, TM_LE);  /* first try `le' */
  if (res >= 0) return res;
  res = call_orderTM(L, r, l, TM_LT);  /* else try `lt' */
  if (res < 0) luaG_ordererror(L, l, r);
  return !res;
}


/*
** equality of Lua values. L == NULL means raw equality (no metamethods)
*/
int luaV_equalobj_ (lua_State *L, const TValue *t1, const TValue *t2) {
  const TValue *tm;
  lua_assert(ttisequal(t1, t2));
  switch (ttype(t1)) {
    case LUA_TNIL: return 1;
    case LUA_TNUMBER: return luai_numeq(nvalue(t1), nvalue(t2));
    case LUA_TBOOLEAN: return bvalue(t1) == bvalue(t2);  /* true must be 1 !! */
    case LUA_TLIGHTUSERDATA: return pvalue(t1) == pvalue(t2);
    case LUA_TLCF: return fvalue(t1) == fvalue(t2);
    case LUA_TSTRING: return eqstr(rawtsvalue(t1), rawtsvalue(t2));
    case LUA_TUSERDATA: {
      if (uvalue(t1) == uvalue(t2)) return 1;
      else if (L == NULL) return 0;
      tm = get_equalTM(L, uvalue(t1)->metatable, uvalue(t2)->metatable, TM_EQ);
      break;  /* will try TM */
    }
    case LUA_TTABLE: {
      if (hvalue(t1) == hvalue(t2)) return 1;
      else if (L == NULL) return 0;
      tm = get_equalTM(L, hvalue(t1)->metatable, hvalue(t2)->metatable, TM_EQ);
      break;  /* will try TM */
    }
    default:
      lua_assert(iscollectable(t1));
      return gcvalue(t1) == gcvalue(t2);
  }
  if (tm == NULL) return 0;  /* no TM? */
  callTM(L, tm, t1, t2, L->top, 1);  /* call TM */
  return !l_isfalse(L->top);
}


static int equaltab (lua_State *L, const TValue *t1, const TValue *t2) {
  const Table *h1 = hvalue(t1);
  const Table *h2 = hvalue(t2);
  if (h1 == h2) return 1;
  const TValue *tm = get_equalTM(L, h1->metatable, h2->metatable, TM_EQ);
  if (tm == NULL) return 0;  /* no TM? */
  callTM(L, tm, t1, t2, L->top, 1);  /* call TM */
  return !l_isfalse(L->top);
}


void luaV_concat (lua_State *L, int total) {
  lua_assert(total >= 2);
  do {
    StkId top = L->top;
    int n = 2;  /* number of elements handled in this pass (at least 2) */
    if (!(ttisstring(top-2) || ttisnumber(top-2)) || !tostring(L, top-1)) {
      if (!call_binTM(L, top-2, top-1, top-2, TM_CONCAT))
        luaG_concaterror(L, top-2, top-1);
    }
    else if (tsvalue(top-1)->len == 0)  /* second operand is empty? */
      (void)tostring(L, top - 2);  /* result is first operand */
    else if (ttisstring(top-2) && tsvalue(top-2)->len == 0) {
      setsvalue2s(L, top-2, rawtsvalue(top-1));  /* result is second op. */
    }
    else {
      /* at least two non-empty string values; get as many as possible */
      size_t tl = tsvalue(top-1)->len;
      char *buffer;
      int i;
      /* collect total length */
      for (i = 1; i < total && tostring(L, top-i-1); i++) {
        size_t l = tsvalue(top-i-1)->len;
        if (l >= (MAX_SIZET/sizeof(char)) - tl)
          luaG_runerror(L, "string length overflow");
        tl += l;
      }
      buffer = luaZ_openspace(L, &G(L)->buff, tl);
      tl = 0;
      n = i;
      do {  /* concat all strings */
        size_t l = tsvalue(top-i)->len;
        memcpy(buffer+tl, svalue(top-i), l * sizeof(char));
        tl += l;
      } while (--i > 0);
      setsvalue2s(L, top-n, luaS_newlstr(L, buffer, tl));
    }
    total -= n-1;  /* got 'n' strings to create 1 new */
    L->top -= n-1;  /* popped 'n' strings and pushed one */
  } while (total > 1);  /* repeat until only 1 result left */
}


void luaV_objlen (lua_State *L, StkId ra, const TValue *rb) {
  const TValue *tm;
  switch (ttypenv(rb)) {
    case LUA_TTABLE: {
      Table *h = hvalue(rb);
      tm = fasttm(L, h->metatable, TM_LEN);
      if (tm) break;  /* metamethod? break switch to call it */
      setnvalue(ra, cast_num(luaH_getn(h)));  /* else primitive len */
      return;
    }
    case LUA_TSTRING: {
      setnvalue(ra, cast_num(tsvalue(rb)->len));
      return;
    }
    default: {  /* try metamethod */
      tm = luaT_gettmbyobj(L, rb, TM_LEN);
      if (ttisnil(tm))  /* no metamethod? */
        luaG_typeerror(L, rb, "get length of");
      break;
    }
  }
  callTM(L, tm, rb, rb, ra, 1);
}


void luaV_arith (lua_State *L, StkId ra, const TValue *rb,
                 const TValue *rc, TMS op) {
  TValue tempb, tempc;
  const TValue *b, *c;
  if ((b = luaV_tonumber(rb, &tempb)) != NULL &&
      (c = luaV_tonumber(rc, &tempc)) != NULL) {
    lua_Number res = luaO_arith(op - TM_ADD + LUA_OPADD, nvalue(b), nvalue(c));
    setnvalue(ra, res);
  }
  else if (!call_binTM(L, rb, rc, ra, op))
    luaG_aritherror(L, rb, rc);
}


/*
** check whether cached closure in prototype 'p' may be reused, that is,
** whether there is a cached closure with the same upvalues needed by
** new closure to be created.
*/
static Closure *getcached (Proto *p, UpVal **encup, StkId base) {
  Closure *c = p->cache;
  if (c != NULL) {  /* is there a cached closure? */
    int nup = p->sizeupvalues;
    Upvaldesc *uv = p->upvalues;
    int i;
    for (i = 0; i < nup; i++) {  /* check whether it has right upvalues */
      TValue *v = uv[i].instack ? base + uv[i].idx : encup[uv[i].idx]->v;
      if (c->l.upvals[i]->v != v)
        return NULL;  /* wrong upvalue; cannot reuse closure */
    }
  }
  return c;  /* return cached closure (or NULL if no cached closure) */
}


/*
** create a new Lua closure, push it in the stack, and initialize
** its upvalues. Note that the call to 'luaC_barrierproto' must come
** before the assignment to 'p->cache', as the function needs the
** original value of that field.
*/
static void pushclosure (lua_State *L, Proto *p, UpVal **encup, StkId base,
                         StkId ra) {
  int nup = p->sizeupvalues;
  Upvaldesc *uv = p->upvalues;
  int i;
  Closure *ncl = luaF_newLclosure(L, p);
  setclLvalue(L, ra, ncl);  /* anchor new closure in stack */
  for (i = 0; i < nup; i++) {  /* fill in its upvalues */
    if (uv[i].instack)  /* upvalue refers to local variable? */
      ncl->l.upvals[i] = luaF_findupval(L, base + uv[i].idx);
    else  /* get upvalue from enclosing function */
      ncl->l.upvals[i] = encup[uv[i].idx];
  }
  luaC_barrierproto(L, p, ncl);
  p->cache = ncl;  /* save it on cache for reuse */
}


#define num_check if (!ttisnumber(ra)) { \
  luaVS_despecialize(L, GETARG_A(i)); }

#define str_check if (!ttisstring(ra)) { \
  luaVS_despecialize(L, GETARG_A(i)); }

#define tab_check if (!ttistable(ra)) { \
  luaVS_despecialize(L, GETARG_A(i)); }


/*
** finish execution of an opcode interrupted by an yield
*/
void luaV_finishOp (lua_State *L) {
  CallInfo *ci = L->ci;
  StkId base = ci->u.l.base;
  Instruction i = *(ci->u.l.savedpc - 1);  /* interrupted instruction */
  OpCode op = GET_OPCODE(i);
#ifdef DEBUG_PRINT
    int _pc = pcRel(ci->u.l.savedpc-1, ci_func(ci)->p);
    printf("%s <%s:%i>[%i] ", __func__, getstr(ci_func(ci)->p->source), 
      getfuncline(ci_func(ci)->p, _pc), _pc);  
    printop(op); printf("\n");  
#endif
  switch (luaP_opcode2group[op]) {  /* finish its execution */
    case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: 
    case OP_MOD: case OP_POW: case OP_UNM: case OP_LEN:     
    case OP_GETTABUP: case OP_GETTABLE: case OP_SELF: {
      StkId ra = base + GETARG_A(i);
      setobjs2s(L, ra, --L->top);
      switch (opout(op)) {
        case OpType_num: num_check break;
        case OpType_str: str_check break;
        case OpType_tab: tab_check break;
        default: break;
      }
      break;
    }
    case OP_LE: case OP_LT: case OP_EQ: {
      int res = !l_isfalse(L->top - 1);
      L->top--;
      /* metamethod should not be called when operand is K */
      lua_assert(!ISK(GETARG_B(i)));
      if (op2grp(op) == OP_LE &&  /* "<=" using "<" iead? */
          ttisnil(luaT_gettmbyobj(L, base + GETARG_B(i), TM_LE)))
        res = !res;  /* invert result */
      lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(JMP));
      if (res != GETARG_A(i))  /* condition failed? */
        ci->u.l.savedpc++;  /* skip jump instruction */
      break;
    }
    case OP_CONCAT: {
      StkId top = L->top - 1;  /* top when 'call_binTM' was called */
      int b = GETARG_B(i);      /* first element to concatenate */
      int total = cast_int(top - 1 - (base + b));  /* yet to concatenate */
      setobj2s(L, top - 2, top);  /* put TM result in proper position */
      if (total > 1) {  /* are there elements to concat? */
        L->top = top - 1;  /* top is one after last element (at top-2) */
        luaV_concat(L, total);  /* concat them (may yield again) */
      }
      /* move final result to final position */
      StkId ra = ci->u.l.base + GETARG_A(i);
      setobj2s(L, ra, L->top - 1);
      switch (opout(op)) {
        case OpType_num: num_check break;
        case OpType_str: str_check break;
        case OpType_tab: tab_check break;
        default: break;
      }
      L->top = ci->top;  /* restore top */
      break;
    }
    case OP_TFORCALL: {
      L->top = ci->top;  /* correct top */
      break;
    }
    case OP_CALL: {
      if (GETARG_C(i) - 1 >= 0)  /* nresults >= 0? */
        L->top = ci->top;  /* adjust results */
      break;
    }
    case OP_TAILCALL: case OP_SETTABUP: case OP_SETTABLE:
      break;
    default: lua_assert(0);
  }
}



/*
** some macros for common tasks in `luaV_execute'
*/

#if !defined luai_runtimecheck
#define luai_runtimecheck(L, c)		/* void */
#endif


#define RA(i) (base+GETARG_A(i))
/* to be used after possible stack reallocation */
#define RB(i) check_exp(getBMode(GET_OPCODE(i)) == OpArgR, base+GETARG_B(i))
#define RC(i) check_exp(getCMode(GET_OPCODE(i)) == OpArgR, base+GETARG_C(i))
#define RKB(i)  check_exp(getBMode(GET_OPCODE(i)) == OpArgK, \
  ISK(GETARG_B(i)) ? k+INDEXK(GETARG_B(i)) : base+GETARG_B(i))
#define RKC(i)  check_exp(getCMode(GET_OPCODE(i)) == OpArgK, \
  ISK(GETARG_C(i)) ? k+INDEXK(GETARG_C(i)) : base+GETARG_C(i))
#define KBx(i)  \
  (k + (GETARG_Bx(i) != 0 ? GETARG_Bx(i) - 1 : GETARG_Ax(*ci->u.l.savedpc++)))


/* execute a jump instruction */
#define dojump(ci,i,e) \
  { int a = GETARG_A(i); \
    if (a > 0) luaF_close(L, ci->u.l.base + a - 1); \
    ci->u.l.savedpc += GETARG_sBx(i) + e; }

/* for test instructions, execute the jump instruction that follows it */
#define donextjump(ci)	{ i = *ci->u.l.savedpc; dojump(ci, i, 1); }


#define Protect(x)	{ {x;}; base = ci->u.l.base; }

#define checkGC(L,c)	Protect(luaC_condGC(L, c); luai_threadyield(L);)


#define vmdispatch(o) switch(o)
#define vmcase(op,out,in,b)   case OP(op,out,in): {b}  break;  
#define vmcasenb(op,out,in,b) case OP(op,out,in): {b}  /* nb = no break */


void luaV_execute (lua_State *L) {

#ifdef LUA_THREADED_DISPATCH

  static void *disptab[] = {
  #define OPENUM(op,out,in) &&L_OP_##op##_##out##_##in,
    OPDEF(OPENUM)
  #undef OPENUM
  };

  #undef vmdispatch
#ifdef DEBUG_PRINT
    #define getfuncline(f,pc) (((f)->lineinfo) ? (f)->lineinfo[pc] : 0)
    #define vmdispatch(x) { \
    int _pc = pcRel(ci->u.l.savedpc, ci_func(ci)->p); \
    printf("<%s:%i>[%i] ", getstr(cl->p->source), \
      getfuncline(cl->p, _pc), _pc); \
    printop(GET_OPCODE(i)); \
    printf(" %i %i %i",GETARG_A(i),GETARG_B(i),GETARG_C(i)); \
    printf("\n"); } \
    goto *disptab[x];
#else
    #define vmdispatch(x) goto *disptab[x];
#endif

  #undef vmcasenb
  #define vmcasenb(op,out,in,b) L_OP_##op##_##out##_##in: {b};

  #undef vmcase
  #define vmcase(op,out,in,b)   L_OP_##op##_##out##_##in: {b};  \
    i = *(ci->u.l.savedpc++);                                   \
    if ((L->hookmask & (LUA_MASKLINE | LUA_MASKCOUNT)) &&       \
        (--L->hookcount == 0 || L->hookmask & LUA_MASKLINE)) {  \
      Protect(traceexec(L));                                    \
    }                                                           \
    ra = RA(i);                                                 \
    vmdispatch(GET_OPCODE(i));

#endif

  CallInfo *ci = L->ci;
  LClosure *cl;
  TValue *k;
  StkId base;

#ifdef DEBUG_PRINT
  printf("\n---\n");
#endif

newframe:  /* reentry point when frame changes (call/return) */
  lua_assert(ci == L->ci);
  cl = clLvalue(ci->func);
  k = cl->p->k;
  base = ci->u.l.base;
  /* main loop of interpreter */
  for (;;) {
    Instruction i = *(ci->u.l.savedpc++);
    StkId ra;
    if ((L->hookmask & (LUA_MASKLINE | LUA_MASKCOUNT)) &&
        (--L->hookcount == 0 || L->hookmask & LUA_MASKLINE)) {
      Protect(traceexec(L));
    }
l_dispatch_again:
    /* WARNING: several calls may realloc the stack and invalidate `ra' */
    ra = RA(i);
    lua_assert(base == ci->u.l.base);
    lua_assert(base <= L->top && L->top < L->stack + L->stacksize);
    vmdispatch (GET_OPCODE(i)) {
/* ------------------------------------------------------------------------ */
      vmcasenb(MOVE,     ___, chk,)
      vmcasenb(MOVE,     num, chk,)
      vmcasenb(MOVE,     str, chk,)
      vmcasenb(MOVE,     tab, chk,)
      vmcasenb(GETTABUP, ___, chk,)
      vmcasenb(GETTABUP, num, chk,)
      vmcasenb(GETTABUP, str, chk,)
      vmcasenb(GETTABUP, tab, chk,)
      vmcasenb(GETTABLE, ___, chk,)
      vmcasenb(GETTABLE, num, chk,)
      vmcasenb(GETTABLE, str, chk,)
      vmcasenb(GETTABLE, tab, chk,)
      vmcasenb(SETTABUP, ___, chk,)
      vmcasenb(SETTABLE, ___, chk,)
      vmcasenb(SELF,     ___, chk,)
      vmcasenb(ADD,      ___, chk,)
      vmcasenb(ADD,      num, chk,)
      vmcasenb(ADD,      str, chk,)
      vmcasenb(ADD,      tab, chk,)
      vmcasenb(SUB,      ___, chk,)
      vmcasenb(SUB,      num, chk,)
      vmcasenb(SUB,      str, chk,)
      vmcasenb(SUB,      tab, chk,)
      vmcasenb(MUL,      ___, chk,)
      vmcasenb(MUL,      num, chk,)
      vmcasenb(MUL,      str, chk,)
      vmcasenb(MUL,      tab, chk,)
      vmcasenb(DIV,      ___, chk,)
      vmcasenb(DIV,      num, chk,)
      vmcasenb(DIV,      str, chk,)
      vmcasenb(DIV,      tab, chk,)
      vmcasenb(MOD,      ___, chk,)
      vmcasenb(MOD,      num, chk,)
      vmcasenb(MOD,      str, chk,)
      vmcasenb(MOD,      tab, chk,)
      vmcasenb(POW,      ___, chk,)
      vmcasenb(POW,      num, chk,)
      vmcasenb(POW,      str, chk,)
      vmcasenb(POW,      tab, chk,)
      vmcasenb(UNM,      ___, chk,)
      vmcasenb(UNM,      num, chk,)
      vmcasenb(UNM,      str, chk,)
      vmcasenb(UNM,      tab, chk,)
      vmcasenb(LEN,      ___, chk,)
      vmcasenb(LEN,      num, chk,)
      vmcasenb(LEN,      str, chk,)
      vmcasenb(LEN,      tab, chk,)
      vmcasenb(EQ,       ___, chk,)
      vmcasenb(LT,       ___, chk,)
      vmcasenb(LE,       ___, chk,
        luaVS_specialize(L);
        i = *(ci->u.l.savedpc-1); /* stay on the same instruction */
        goto l_dispatch_again;
      )
/* ------------------------------------------------------------------------ */      
#define vmcase_move_raw(out, guard) \
      vmcase(MOVE,out,___,          \
        setobjs2s(L, ra, RB(i));    \
        {guard;}                    \
      )

      vmcase_move_raw(___, )
      vmcase_move_raw(num, num_check)
      vmcase_move_raw(str, str_check)
      vmcase_move_raw(tab, tab_check)

      vmcase(MOVE,___,num,
        setnvalue(ra, nvalue(RB(i)));
      )    
      vmcase(MOVE,___,str, 
        setsvalue2s(L, ra, rawtsvalue(RB(i)));
      )
      vmcase(MOVE,___,tab,
        setobjs2s(L, ra, RB(i));
      )      
/* ------------------------------------------------------------------------ */
      vmcase(LOADK,___,num,
        TValue *rb = k + GETARG_Bx(i);
        setnvalue(ra, nvalue(rb));
      )
      vmcase(LOADK,___,str,
        TValue *rb = k + GETARG_Bx(i);
        setsvalue2s(L, ra, rawtsvalue(rb));
      )
/* ------------------------------------------------------------------------ */
      vmcase(LOADKX,___,num,
        TValue *rb;
        lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(EXTRAARG));
        rb = k + GETARG_Ax(*ci->u.l.savedpc++);
        setnvalue(ra, nvalue(rb));
      )
      vmcase(LOADKX,___,str,
        TValue *rb;
        lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(EXTRAARG));
        rb = k + GETARG_Ax(*ci->u.l.savedpc++);
        setsvalue2s(L, ra, rawtsvalue(rb));
      )
/* ------------------------------------------------------------------------ */
      vmcase(LOADBOOL,___,___,
        setbvalue(ra, GETARG_B(i));        
        if (GETARG_C(i)) ci->u.l.savedpc++; /* skip next instruction (if C) */
      )
/* ------------------------------------------------------------------------ */
      vmcase(LOADNIL,___,___,
        int b = GETARG_B(i);
        do {
          setnilvalue(ra++);
        } while (b--);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_getupval(out,guard)                    \
      vmcase(GETUPVAL,out,___,                    \
        setobj2s(L, ra, cl->upvals[GETARG_B(i)]->v);  \
        {guard;}                                      \
      )

      vmcase_getupval(___, )
      vmcase_getupval(num, num_check)
      vmcase_getupval(str, str_check)
      vmcase_getupval(tab, tab_check)
/* ------------------------------------------------------------------------ */
#define vmcase_gettab_raw(op,b,out,guard)         \
      vmcase(op,out,___,                          \
        Protect(luaV_gettable(L, b, RKC(i), ra)); \
        {guard;}                                  \
      )
#define vmcase_gettable_spec(out,in,getfunc,guard)              \
      vmcase(GETTABLE, out, in,                                 \
        TValue *b = RB(i);                                      \
        TValue *key = RKC(i);                                   \
        Table *h = hvalue(b);                                   \
        const TValue *res = getfunc(h, key);                    \
        const TValue *tm;                                       \
        if (!ttisnil(res) ||                                    \
            (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) { \
          setobj2s(L, ra, res);                                 \
        } else {                                                \
          gettable_tm(L, b, tm, key, ra);                       \
        }                                                       \
        {guard;}                                                \
      )
#define vmcase_gettabup_spec(out, in, getfunc, guard)             \
      vmcase(GETTABUP, out, in,                                   \
        TValue *b = cl->upvals[GETARG_B(i)]->v;                   \
        if (ttistable(b)) {                                       \
          TValue *key = RKC(i);                                   \
          Table *h = hvalue(b);                                   \
          const TValue *res = getfunc(h, key);                    \
          const TValue *tm;                                       \
          if (!ttisnil(res) ||                                    \
              (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) { \
            setobj2s(L, ra, res);                                 \
          } else {                                                \
            gettable_tm(L, b, tm, key, ra);                       \
          }                                                       \
        } else {                                                  \
          Protect(luaV_gettable(L, b, RKC(i), ra));               \
        }                                                         \
        {guard;}                                                  \
      )
  
  #define luaH_getstr_(h,key) luaH_getstr(h, rawtsvalue(key))

      vmcase_gettab_raw(GETTABLE, RB(i), ___, )
      vmcase_gettab_raw(GETTABLE, RB(i), num, num_check)
      vmcase_gettab_raw(GETTABLE, RB(i), str, str_check)
      vmcase_gettab_raw(GETTABLE, RB(i), tab, tab_check)
      vmcase_gettable_spec(___, num, luaH_getnum, )
      vmcase_gettable_spec(num, num, luaH_getnum, num_check)
      vmcase_gettable_spec(str, num, luaH_getnum, str_check)
      vmcase_gettable_spec(tab, num, luaH_getnum, tab_check)
      vmcase_gettable_spec(___, str, luaH_getstr_, )
      vmcase_gettable_spec(num, str, luaH_getstr_, num_check)
      vmcase_gettable_spec(str, str, luaH_getstr_, str_check)
      vmcase_gettable_spec(tab, str, luaH_getstr_, tab_check)

      vmcase_gettab_raw(GETTABUP, cl->upvals[GETARG_B(i)]->v, ___, )
      vmcase_gettab_raw(GETTABUP, cl->upvals[GETARG_B(i)]->v, num, num_check)
      vmcase_gettab_raw(GETTABUP, cl->upvals[GETARG_B(i)]->v, str, str_check)
      vmcase_gettab_raw(GETTABUP, cl->upvals[GETARG_B(i)]->v, tab, tab_check)
      vmcase_gettabup_spec(___, num, luaH_getnum, )
      vmcase_gettabup_spec(num, num, luaH_getnum, num_check)
      vmcase_gettabup_spec(str, num, luaH_getnum, str_check)
      vmcase_gettabup_spec(tab, num, luaH_getnum, tab_check)
      vmcase_gettabup_spec(___, str, luaH_getstr_, )
      vmcase_gettabup_spec(num, str, luaH_getstr_, num_check)
      vmcase_gettabup_spec(str, str, luaH_getstr_, str_check)
      vmcase_gettabup_spec(tab, str, luaH_getstr_, tab_check)
/* ------------------------------------------------------------------------ */
#define vmcase_settabup_spec(in,getfunc)                                \
      vmcase(SETTABUP, ___, in,                                         \
        TValue *a = cl->upvals[GETARG_A(i)]->v;                         \
        TValue *key = RKB(i);                                           \
        StkId val = RKC(i);                                             \
        if (ttistable(a)) {                                             \
          Table *h = hvalue(a);                                         \
          TValue *oldval = cast(TValue *, getfunc(h, key));             \
          const TValue *tm;                                             \
          if (!ttisnil(oldval) ||                                       \
              ((tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL &&   \
              (oldval != luaO_nilobject ||                              \
              (oldval = luaH_newkey(L, h, key), 1)))) {                 \
            setobj2t(L, oldval, val);                                   \
            invalidateTMcache(h);                                       \
            luaC_barrierback(L, obj2gco(h), val);                       \
          } else {                                                      \
            settable_tm(L, a, tm, key, val);                            \
          }                                                             \
        } else {                                                        \
          Protect(luaV_settable(L, a, key, val));                       \
        }                                                               \
      )

#define vmcase_settable_spec(in,getfunc)                              \
      vmcase(SETTABLE, ___, in,                                       \
        Table *h = hvalue(ra);                                        \
        TValue *key = RKB(i);                                         \
        StkId val = RKC(i);                                           \
        TValue *oldval = cast(TValue *, getfunc(h, key));             \
        const TValue *tm;                                             \
        if (!ttisnil(oldval) ||                                       \
            ((tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL &&   \
            (oldval != luaO_nilobject ||                              \
            (oldval = luaH_newkey(L, h, key), 1)))) {                 \
          setobj2t(L, oldval, val);                                   \
          invalidateTMcache(h);                                       \
          luaC_barrierback(L, obj2gco(h), val);                       \
        } else {                                                      \
          settable_tm(L, ra, tm, key, val);                           \
        }                                                             \
      )

      vmcase(SETTABUP,___,___,
        Protect(luaV_settable(L, cl->upvals[GETARG_A(i)]->v, RKB(i), RKC(i)));
      )
      vmcase_settabup_spec(num, luaH_getnum)
      vmcase_settabup_spec(str, luaH_getstr_)

      vmcase(SETTABLE,___,___,
        Protect(luaV_settable(L, ra, RKB(i), RKC(i)));
      )
      vmcase_settable_spec(num, luaH_getnum)
      vmcase_settable_spec(str, luaH_getstr_)
/* ------------------------------------------------------------------------ */
#define vmcase_setupval(out, guard)   \
      vmcase(SETUPVAL,out,___,    \
        int idx = GETARG_B(i);        \
        UpVal *uv = cl->upvals[idx];  \
        setobj(L, uv->v, ra);         \
        luaC_barrier(L, uv, ra);      \
        {guard;}                      \
      )

      vmcase_setupval(___, )
      vmcase_setupval(num,
        if (!ttisnumber(ra)) luaVS_despecialize_upval(L, cl->p, idx);
      )
      vmcase_setupval(str,
        if (!ttisstring(ra)) luaVS_despecialize_upval(L, cl->p, idx);
      )
      vmcase_setupval(tab,
        if (!ttistable(ra)) luaVS_despecialize_upval(L, cl->p, idx);
      )
/* ------------------------------------------------------------------------ */
      vmcase(NEWTABLE,___,___,
        int b = GETARG_B(i);
        int c = GETARG_C(i);
        Table *t = luaH_new(L);
        sethvalue(L, ra, t);
        if (b != 0 || c != 0)
          luaH_resize(L, t, luaO_fb2int(b), luaO_fb2int(c));
        checkGC(L,
          L->top = ra + 1;  /* limit of live values */
          luaC_step(L);
          L->top = ci->top;  /* restore top */
        )
      )
/* ------------------------------------------------------------------------ */
      vmcase(SELF,___,___,
        StkId rb = RB(i);
        setobjs2s(L, ra+1, rb);
        Protect(luaV_gettable(L, rb, RKC(i), ra));
      )
      vmcase(SELF,___,tab,
        StkId rb = RB(i);
        setobjs2s(L, ra+1, rb);
        TValue *key = RKC(i);
        Table *h = hvalue(rb);
        const TValue *res = luaH_getstr(h, rawtsvalue(key));
        const TValue *tm;
        if (!ttisnil(res) ||
            (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) {
          setobj2s(L, ra, res);
        } else {
          gettable_tm(L, rb, tm, key, ra);
        }
      )
/* ------------------------------------------------------------------------ */
#define vmcase_arith_raw(op,func,tm,out,guard)            \
      vmcase(op,out,___,                              \
        TValue *rb = RKB(i);                              \
        TValue *rc = RKC(i);                              \
        if (ttisnumber(rb) && ttisnumber(rc)) {           \
          lua_Number nb = nvalue(rb);                     \
          lua_Number nc = nvalue(rc);                     \
          setnvalue(ra, func(L, nb, nc));                 \
        }                                                 \
        else { Protect(luaV_arith(L, ra, rb, rc, tm)); }  \
        {guard;}                                          \
      )
#define vmcase_arith_num(op,func)                       \
      vmcase(op,___,num,                            \
        TValue *rb = RKB(i);                            \
        TValue *rc = RKC(i);                            \
        setnvalue(ra, func(L, nvalue(rb), nvalue(rc))); \
      )
#define vmcase_arith(op,func,tm)                      \
      vmcase_arith_raw(op, func, tm, ___, )           \
      vmcase_arith_raw(op, func, tm, num, num_check)  \
      vmcase_arith_raw(op, func, tm, str, str_check)  \
      vmcase_arith_raw(op, func, tm, tab, tab_check)  \
      vmcase_arith_num(op, func)

      vmcase_arith(ADD, luai_numadd, TM_ADD)
      vmcase_arith(SUB, luai_numsub, TM_SUB)
      vmcase_arith(MUL, luai_nummul, TM_MUL)
      vmcase_arith(DIV, luai_numdiv, TM_DIV)
      vmcase_arith(MOD, luai_nummod, TM_MOD)
      vmcase_arith(POW, luai_numpow, TM_POW)
/* ------------------------------------------------------------------------ */
#define vmcase_unm_raw(out,guard)                   \
      vmcase(UNM,out,___,                       \
        TValue *rb = RB(i);                         \
        Protect(luaV_arith(L, ra, rb, rb, TM_UNM)); \
        {guard;}                                    \
      )

      vmcase_unm_raw(___, )
      vmcase_unm_raw(num, num_check)
      vmcase_unm_raw(str, str_check)
      vmcase_unm_raw(tab, tab_check)
      vmcase(UNM,___,num,
        setnvalue(ra, luai_numunm(L, nvalue(RB(i))));
      )
/* ------------------------------------------------------------------------ */
      vmcase(NOT,___,___,
        setbvalue(ra, l_isfalse(RB(i)));
      )
/* ------------------------------------------------------------------------ */
#define vmcase_len_raw(out,guard)           \
      vmcase(LEN,out,___,                   \
        Protect(luaV_objlen(L, ra, RB(i))); \
        {guard;}                            \
      )

      vmcase_len_raw(___, )
      vmcase_len_raw(num, num_check)
      vmcase_len_raw(str, str_check)
      vmcase_len_raw(tab, tab_check)

      vmcase(LEN,___,str,
        setnvalue(ra, cast_num(tsvalue(RB(i))->len));
      )
      vmcase(LEN,___,tab,
        TValue *rb = RB(i);
        Table *h = hvalue(rb);
        const TValue *tm = fasttm(L, h->metatable, TM_LEN);
        if (tm) callTM(L, tm, rb, rb, ra, 1);
        else setnvalue(ra, cast_num(luaH_getn(h)));
      )
/* ------------------------------------------------------------------------ */
#define vmcase_concat(out,guard)                                            \
      vmcase(CONCAT,out,___,                                                \
        int b = GETARG_B(i);                                                \
        int c = GETARG_C(i);                                                \
        StkId rb;                                                           \
        L->top = base + c + 1;  /* mark the end of concat operands */       \
        Protect(luaV_concat(L, c - b + 1));                                 \
        ra = RA(i);  /* 'luav_concat' may invoke TMs and move the stack */  \
        rb = b + base;                                                      \
        setobjs2s(L, ra, rb);                                               \
        checkGC(L,                                                          \
          L->top = (ra >= rb ? ra + 1 : rb);  /* limit of live values */    \
          luaC_step(L);                                                     \
        )                                                                   \
        L->top = ci->top;  /* restore top */                                \
        {guard;}                                                            \
      )

      vmcase_concat(___, )
      vmcase_concat(num, num_check)
      vmcase_concat(str, str_check)
      vmcase_concat(tab, tab_check)
/* ------------------------------------------------------------------------ */
      vmcase(JMP,___,___,
        dojump(ci, i, 0);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_cmp(op,in,f,g)           \
      vmcase(op,___,in,                 \
        int a = GETARG_A(i);            \
        TValue *rb = RKB(i);            \
        TValue *rc = RKC(i);            \
        Protect(                        \
          if (f(L, g(rb), g(rc)) != a)  \
            ci->u.l.savedpc++;          \
          else                          \
            donextjump(ci);             \
        )                               \
      )

#define luai_numeq_(L,b,c) (luai_numeq(b,c))
#define eqstr_(L,b,c) (eqstr(b,c))
#define l_strcmp_lt(L,b,c) (l_strcmp(b,c) <  0)
#define l_strcmp_le(L,b,c) (l_strcmp(b,c) <= 0)

      vmcase_cmp(EQ, ___, equalobj,)
      vmcase_cmp(EQ, num, luai_numeq_, nvalue)
      vmcase_cmp(EQ, str, eqstr_, rawtsvalue)
      vmcase_cmp(EQ, tab, equaltab, )

      vmcase_cmp(LT, ___, luaV_lessthan,)
      vmcase_cmp(LT, num, luai_numlt, nvalue)
      vmcase_cmp(LT, str, l_strcmp_lt, rawtsvalue)
      vmcase_cmp(LT, tab, lessthan_tm, )
      
      vmcase_cmp(LE, ___, luaV_lessequal,)
      vmcase_cmp(LE, num, luai_numle, nvalue)
      vmcase_cmp(LE, str, l_strcmp_le, rawtsvalue)
      vmcase_cmp(LE, tab, lessequal_tm, )
/* ------------------------------------------------------------------------ */
      vmcase(TEST,___,___,
        if (GETARG_C(i) ? l_isfalse(ra) : !l_isfalse(ra))
            ci->u.l.savedpc++;
        else
          donextjump(ci);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_testset(out,guard)                         \
      vmcase(TESTSET,out,___,                         \
        TValue *rb = RB(i);                               \
        if (GETARG_C(i) ? l_isfalse(rb) : !l_isfalse(rb)) \
          ci->u.l.savedpc++;                              \
        else {                                            \
          setobjs2s(L, ra, rb);                           \
          {guard;}                                        \
          donextjump(ci);                                 \
        }                                                 \
      )

      vmcase_testset(___, )
      vmcase_testset(num, num_check)
      vmcase_testset(str, str_check)
      vmcase_testset(tab, tab_check)
/* ------------------------------------------------------------------------ */
      vmcase(CALL,___,___,
        int b = GETARG_B(i);
        int nresults = GETARG_C(i) - 1;
        if (b != 0) L->top = ra+b;  /* else previous instruction set top */
        if (luaD_precall(L, ra, nresults)) {  /* C function? */
          if (nresults >= 0) L->top = ci->top;  /* adjust results */
          base = ci->u.l.base;
        }
        else {  /* Lua function */
          ci = L->ci;
          ci->callstatus |= CIST_REENTRY;
          /* restart luaV_execute over new Lua function */
          goto newframe;
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(TAILCALL,___,___,
        int b = GETARG_B(i);
        if (b != 0) L->top = ra+b;  /* else previous instruction set top */
        lua_assert(GETARG_C(i) - 1 == LUA_MULTRET);
        if (luaD_precall(L, ra, LUA_MULTRET))  /* C function? */
          base = ci->u.l.base;
        else {
          /* tail call: put called frame (n) in place of caller one (o) */
          CallInfo *nci = L->ci;  /* called frame */
          CallInfo *oci = nci->previous;  /* caller frame */
          StkId nfunc = nci->func;  /* called function */
          StkId ofunc = oci->func;  /* caller function */
          /* last stack slot filled by 'precall' */
          StkId lim = nci->u.l.base + getproto(nfunc)->numparams;
          int aux;
          /* close all upvalues from previous call */
          if (cl->p->sizep > 0) luaF_close(L, oci->u.l.base);
          /* move new frame into old one */
          for (aux = 0; nfunc + aux < lim; aux++)
            setobjs2s(L, ofunc + aux, nfunc + aux);
          oci->u.l.base = ofunc + (nci->u.l.base - nfunc);  /* correct base */
          oci->top = L->top = ofunc + (L->top - nfunc);  /* correct top */
          oci->u.l.savedpc = nci->u.l.savedpc;
          oci->callstatus |= CIST_TAIL;  /* function was tail called */
          ci = L->ci = oci;  /* remove new frame */
          lua_assert(L->top == oci->u.l.base + getproto(ofunc)->maxstacksize);
          /* restart luaV_execute over new Lua function */
          goto newframe;
        }
      )
/* ------------------------------------------------------------------------ */
      vmcasenb(RETURN,___,___,
        int b = GETARG_B(i);
        if (b != 0) L->top = ra+b-1;
        if (cl->p->sizep > 0) luaF_close(L, base);
        b = luaD_poscall(L, ra);
        if (!(ci->callstatus & CIST_REENTRY)) { /* 'ci' still the called one */
          return;  /* external invocation: return */
        }
        else {  /* invocation via reentry: continue execution */
          ci = L->ci;
          if (b) L->top = ci->top;
          lua_assert(isLua(ci));
          // TODO: this assert needs to take CHKTYPEs into account
          lua_assert(GET_OPGROUP(*((ci)->u.l.savedpc - 1)) == OP_CALL);
          goto newframe;  /* restart luaV_execute over new Lua function */
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(FORLOOP,___,___,
        lua_Number step = nvalue(ra+2);
        lua_Number idx = luai_numadd(L, nvalue(ra), step); /* increment index */
        lua_Number limit = nvalue(ra+1);
        if (luai_numlt(L, 0, step) ? luai_numle(L, idx, limit)
                                   : luai_numle(L, limit, idx)) {
          ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
          setnvalue(ra, idx);  /* update internal index... */
          setnvalue(ra+3, idx);  /* ...and external index */
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(FORPREP,___,___,
        const TValue *init = ra;
        const TValue *plimit = ra+1;
        const TValue *pstep = ra+2;
        if (!tonumber(init, ra))
          luaG_runerror(L, LUA_QL("for") " initial value must be a number");
        else if (!tonumber(plimit, ra+1))
          luaG_runerror(L, LUA_QL("for") " limit must be a number");
        else if (!tonumber(pstep, ra+2))
          luaG_runerror(L, LUA_QL("for") " step must be a number");
        setnvalue(ra, luai_numsub(L, nvalue(ra), nvalue(pstep)));
        ci->u.l.savedpc += GETARG_sBx(i);
      )
/* ------------------------------------------------------------------------ */
      vmcase(TFORCALL,___,___,
        StkId cb = ra + 3;  /* call base */
        setobjs2s(L, cb+2, ra+2);
        setobjs2s(L, cb+1, ra+1);
        setobjs2s(L, cb, ra);
        L->top = cb + 3;  /* func. + 2 args (state and index) */
        int nresults = GETARG_C(i);
        Protect(luaD_call(L, cb, nresults, 1));
        L->top = ci->top;
        // TODO: cannot use this optimization due to CHKTYPE
        // i = *(ci->u.l.savedpc++);  /* go to next instruction */
        // ra = RA(i);
        // lua_assert(GET_OPCODE(i) == sOP(TFORLOOP));
        // goto l_tforloop;
      )
/* ------------------------------------------------------------------------ */
      vmcase(TFORLOOP,___,___,
        // l_tforloop:
        if (!ttisnil(ra + 1)) {  /* continue loop? */
          setobjs2s(L, ra, ra + 1);  /* save control variable */
          ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(SETLIST,___,___,
        int n = GETARG_B(i);
        int c = GETARG_C(i);
        int last;
        Table *h;
        if (n == 0) n = cast_int(L->top - ra) - 1;
        if (c == 0) {
          lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(EXTRAARG));
          c = GETARG_Ax(*ci->u.l.savedpc++);
        }
        luai_runtimecheck(L, ttistable(ra));
        h = hvalue(ra);
        last = ((c-1)*LFIELDS_PER_FLUSH) + n;
        if (last > h->sizearray)  /* needs more space? */
          luaH_resizearray(L, h, last);  /* pre-allocate it at once */
        for (; n > 0; n--) {
          TValue *val = ra+n;
          luaH_setint(L, h, last--, val);
          luaC_barrierback(L, obj2gco(h), val);
        }
        L->top = ci->top;  /* correct top (in case of previous open call) */
      )
/* ------------------------------------------------------------------------ */
      vmcase(CLOSURE,___,___,
        Proto *p = cl->p->p[GETARG_Bx(i)];
        Closure *ncl = getcached(p, cl->upvals, base);  /* cached closure */
        if (ncl == NULL)  /* no match? */
          pushclosure(L, p, cl->upvals, base, ra);  /* create a new one */
        else
          setclLvalue(L, ra, ncl);  /* push cashed closure */
        checkGC(L,
          L->top = ra + 1;  /* limit of live values */
          luaC_step(L);
          L->top = ci->top;  /* restore top */
        )
      )
/* ------------------------------------------------------------------------ */
      vmcase(VARARG,___,___,
        int b = GETARG_B(i) - 1;
        int j;
        int n = cast_int(base - ci->func) - cl->p->numparams - 1;
        if (b < 0) {  /* B == 0? */
          b = n;  /* get all var. arguments */
          Protect(luaD_checkstack(L, n));
          ra = RA(i);  /* previous call may change the stack */
          L->top = ra + n;
        }
        for (j = 0; j < b; j++) {
          if (j < n) {
            setobjs2s(L, ra + j, base - n + j);
          }
          else {
            setnilvalue(ra + j);
          }
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(CHKTYPE,___,___, /* nop */)
      vmcase(CHKTYPE,num,___, num_check)
      vmcase(CHKTYPE,str,___, str_check)
      vmcase(CHKTYPE,tab,___, tab_check)
/* ------------------------------------------------------------------------ */
      vmcase(EXTRAARG,___,___,
        lua_assert(0);
      )
    }
  }
}

