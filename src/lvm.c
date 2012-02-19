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

#define _luaV_gettable_def(name, getfunc, keyfunc)                          \
void name (lua_State *L, const TValue *t, TValue *key, StkId val) {         \
  int loop;                                                                 \
  for (loop = 0; loop < MAXTAGLOOP; loop++) {                               \
    const TValue *tm;                                                       \
    if (ttistable(t)) {  /* `t' is a table? */                              \
      Table *h = hvalue(t);                                                 \
      const TValue *res = getfunc(h, keyfunc(key));                         \
      if (!ttisnil(res) ||  /* result is not nil? */                        \
          (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) { /* no TM? */  \
        setobj2s(L, val, res);                                              \
        return;                                                             \
      }                                                                     \
      /* else will try the tag method */                                    \
    }                                                                       \
    else if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_INDEX)))                 \
      luaG_typeerror(L, t, "index");                                        \
    if (ttisfunction(tm)) {                                                 \
      callTM(L, tm, t, key, val, 1);                                        \
      return;                                                               \
    }                                                                       \
    t = tm;  /* else repeat with 'tm' */                                    \
  }                                                                         \
  luaG_runerror(L, "loop in gettable");                                     \
}

_luaV_gettable_def(luaV_gettable, luaH_get,);
_luaV_gettable_def(luaV_gettable_num, luaH_getnum, );
_luaV_gettable_def(luaV_gettable_str, luaH_getstr, rawtsvalue);
_luaV_gettable_def(luaV_gettable_obj, luaH_getobj,);

#define luaV_gettable____(L,t,key,val) luaV_gettable(L,t,key,val)


#define _luaV_settable_def(name, getfunc, keyfunc)                        \
void name (lua_State *L, const TValue *t, TValue *key, StkId val) {       \
  int loop;                                                               \
  for (loop = 0; loop < MAXTAGLOOP; loop++) {                             \
    const TValue *tm;                                                     \
    if (ttistable(t)) {  /* `t' is a table? */                            \
      Table *h = hvalue(t);                                               \
      TValue *oldval = cast(TValue *, getfunc(h, keyfunc(key)));          \
      /* if previous value is not nil, there must be a previous entry
         in the table; moreover, a metamethod has no relevance */         \
      if (!ttisnil(oldval) ||                                             \
         /* previous value is nil; must check the metamethod */           \
         ((tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL &&          \
         /* no metamethod; is there a previous entry in the table? */     \
         (oldval != luaO_nilobject ||                                     \
         /* no previous entry; must create one. (The next test is
            always true; we only need the assignment.) */                 \
         (oldval = luaH_newkey(L, h, key), 1)))) {                        \
        /* no metamethod and (now) there is an entry with given key */    \
        setobj2t(L, oldval, val);  /* assign new value to that entry */   \
        invalidateTMcache(h);                                             \
        luaC_barrierback(L, obj2gco(h), val);                             \
        return;                                                           \
      }                                                                   \
      /* else will try the metamethod */                                  \
    }                                                                     \
    else  /* not a table; check metamethod */                             \
      if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX)))               \
        luaG_typeerror(L, t, "index");                                    \
    /* there is a metamethod */                                           \
    if (ttisfunction(tm)) {                                               \
      callTM(L, tm, t, key, val, 0);                                      \
      return;                                                             \
    }                                                                     \
    t = tm;  /* else repeat with 'tm' */                                  \
  }                                                                       \
  luaG_runerror(L, "loop in settable");                                   \
}

_luaV_settable_def(luaV_settable, luaH_get,);
_luaV_settable_def(luaV_settable_num, luaH_getnum, );
_luaV_settable_def(luaV_settable_str, luaH_getstr, rawtsvalue);
_luaV_settable_def(luaV_settable_obj, luaH_getobj,);

#define luaV_settable____(L,t,key,val) luaV_settable(L,t,key,val)



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

#define obj_check if (ttisnumber(ra) || ttisstring(ra)) { \
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
    printf("%s <%s:%i>[%i] ", __func__, getstr(ci_func(ci)->p->source), getfuncline(ci_func(ci)->p, _pc), _pc);  
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
        case OpType_obj: obj_check break;
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
        case OpType_obj: obj_check break;
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


#define arith_op(op,tm,b,c) { \
        TValue *rb = b; \
        TValue *rc = c; \
        if (ttisnumber(rb) && ttisnumber(rc)) { \
          lua_Number nb = nvalue(rb), nc = nvalue(rc); \
          setnvalue(ra, op(L, nb, nc)); \
        } \
        else { Protect(luaV_arith(L, ra, rb, rc, tm)); } }


#define vmdispatch(o)	switch(o)
#define vmcase(o,b)	case o: {b}  break;
#define vmcasenb(o,b)	case o: {b}		/* nb = no break */



//#define dispatch_again { ci->u.l.savedpc--; continue; }
#define dispatch_again { i = *(ci->u.l.savedpc-1); goto l_dispatch_again; }
// TODO: is there a nicer way to do this?
//       - we could specialize in-place and jump directly to labels...


void luaV_execute (lua_State *L) {
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
  
    
#ifdef DEBUG_PRINT
    #define getfuncline(f,pc) (((f)->lineinfo) ? (f)->lineinfo[pc] : 0)
    int _pc = pcRel(ci->u.l.savedpc, ci_func(ci)->p);
    printf("<%s:%i>[%i] ", getstr(cl->p->source), getfuncline(cl->p, _pc), _pc);
    printop(GET_OPCODE(i));
    printf(" %i %i %i",GETARG_A(i),GETARG_B(i),GETARG_C(i));
    printf("\n");
#endif

    vmdispatch (GET_OPCODE(i)) {
/* ------------------------------------------------------------------------ */
#define vmcase_move(out,spec,guard) \
      vmcase(OP(MOVE,out,spec),     \
        setobjs2s(L, ra, RB(i));    \
        {guard;}                    \
      )
#define vmcase_move_chk(out)    \
      vmcase(OP(MOVE,out,chk),  \
        luaVS_specialize(L);    \
        dispatch_again          \
      )

      vmcase_move(___, ___, )
      vmcase_move(num, ___, num_check)
      vmcase_move(str, ___, str_check)
      vmcase_move(obj, ___, obj_check)
      vmcase_move(___, num, )
      vmcase_move(___, str, )
      vmcase_move(___, obj, )
      vmcase_move_chk(___)
      vmcase_move_chk(num)
      vmcase_move_chk(str)
      vmcase_move_chk(obj)
/* ------------------------------------------------------------------------ */
#define vmcase_loadk(in)                    \
      vmcase(OP(LOADK,___,in),              \
        setobj2s(L, ra, k + GETARG_Bx(i));  \
      )

      vmcase_loadk(num)
      vmcase_loadk(str)
/* ------------------------------------------------------------------------ */
#define vmcase_loadkx(in)                                           \
      vmcase(OP(LOADKX,___,in),                                     \
        TValue *rb;                                                 \
        lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(EXTRAARG));  \
        rb = k + GETARG_Ax(*ci->u.l.savedpc++);                     \
        setobj2s(L, ra, rb);                                        \
      )

      vmcase_loadkx(num)
      vmcase_loadkx(str)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(LOADBOOL),
        setbvalue(ra, GETARG_B(i));        
        if (GETARG_C(i)) ci->u.l.savedpc++; /* skip next instruction (if C) */
      )
/* ------------------------------------------------------------------------ */
      vmcase(sOP(LOADNIL),
        int b = GETARG_B(i);
        do {
          setnilvalue(ra++);
        } while (b--);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_getupval(out,guard)                    \
      vmcase(OP(GETUPVAL,out,___),                    \
        setobj2s(L, ra, cl->upvals[GETARG_B(i)]->v);  \
        {guard;}                                      \
      )

      vmcase_getupval(___, )
      vmcase_getupval(num, num_check)
      vmcase_getupval(str, str_check)
      vmcase_getupval(obj, obj_check)      
/* ------------------------------------------------------------------------ */
#define _vmcase_gettab_spec(op,b,spec,out,guard)        \
    vmcase(OP(op,out,spec),                             \
      Protect(luaV_gettable_##spec(L, b, RKC(i), ra));  \
      {guard;}                                          \
    )
#define vmcase_gettab_spec(op,b,spec)                 \
    _vmcase_gettab_spec(op, b, spec, ___, )           \
    _vmcase_gettab_spec(op, b, spec, num, num_check)  \
    _vmcase_gettab_spec(op, b, spec, str, str_check)  \
    _vmcase_gettab_spec(op, b, spec, obj, obj_check)

#define _vmcase_gettab_chk(op,out)  \
    vmcase(OP(op,out,chk),          \
      luaVS_specialize(L);          \
      dispatch_again                \
    )
#define vmcase_gettab_chk(op)   \
    _vmcase_gettab_chk(op, ___) \
    _vmcase_gettab_chk(op, num) \
    _vmcase_gettab_chk(op, str) \
    _vmcase_gettab_chk(op, obj)

#define vmcase_gettab(op,b)         \
    vmcase_gettab_spec(op, b, ___)  \
    vmcase_gettab_spec(op, b, num)  \
    vmcase_gettab_spec(op, b, str)  \
    vmcase_gettab_spec(op, b, obj)  \
    vmcase_gettab_chk(op)

    vmcase_gettab(GETTABLE, RB(i))
    vmcase_gettab(GETTABUP, cl->upvals[GETARG_B(i)]->v)
/* ------------------------------------------------------------------------ */
#define vmcase_settab_spec(op,spec,a)                         \
      vmcase(OP(op,___,spec),                                 \
        Protect(luaV_settable_##spec(L, a, RKB(i), RKC(i)));  \
      )
#define vmcase_settab_chk(op) \
      vmcase(OP(op,___,chk),  \
        luaVS_specialize(L);  \
        dispatch_again        \
      )      
#define vmcase_settab(op,a)           \
      vmcase_settab_spec(op, ___, a)  \
      vmcase_settab_spec(op, num, a)  \
      vmcase_settab_spec(op, str, a)  \
      vmcase_settab_spec(op, obj, a)  \
      vmcase_settab_chk(op) \

      vmcase_settab(SETTABLE, ra)
      vmcase_settab(SETTABUP, cl->upvals[GETARG_A(i)]->v)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(SETUPVAL),
        int idx = GETARG_B(i);
        UpVal *uv = cl->upvals[idx];
        setobj(L, uv->v, ra);
        luaC_barrier(L, uv, ra);

        switch (cl->p->upvalues[idx].expected_type) {
          case OpType_num: if (ttisnumber(ra)) break;
          case OpType_str: if (ttisstring(ra)) break;
          case OpType_obj: if (!ttisnumber(ra) && !ttisstring(ra)) break;
            luaVS_despecialize_upval(L, cl->p, idx);
          default: break;
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(sOP(NEWTABLE),
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
      vmcase(OP(SELF,___,___),
        StkId rb = RB(i);
        setobjs2s(L, ra+1, rb);
        Protect(luaV_gettable_str(L, rb, RKC(i), ra));
      )
/* ------------------------------------------------------------------------ */
#define _vmcase_arith_raw(op,func,tm,out,guard)           \
      vmcase(OP(op,out,___),                              \
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
#define vmcase_arith_raw(op,func,tm)                  \
      _vmcase_arith_raw(op, func, tm, ___, )          \
      _vmcase_arith_raw(op, func, tm, num, num_check) \
      _vmcase_arith_raw(op, func, tm, str, str_check) \
      _vmcase_arith_raw(op, func, tm, obj, obj_check)

#define _vmcase_arith_num(op,func,out,guard)            \
      vmcase(OP(op,out,num),                            \
        TValue *rb = RKB(i);                            \
        TValue *rc = RKC(i);                            \
        setnvalue(ra, func(L, nvalue(rb), nvalue(rc))); \
        {guard;}                                        \
      )
#define vmcase_arith_num(op,func)                 \
      _vmcase_arith_num(op, func, ___, )          \

#define _vmcase_arith_obj(op,tm,out,guard)    \
      vmcase(OP(op,out,obj),                  \
        TValue *rb = RKB(i);                  \
        TValue *rc = RKC(i);                  \
        Protect(                              \
          if (!call_binTM(L, rb, rc, ra, tm)) \
            luaG_aritherror(L, rb, rc);       \
        )                                     \
        {guard;}                              \
      )
#define vmcase_arith_obj(op,tm)                 \
      _vmcase_arith_obj(op, tm, ___, )          \
      _vmcase_arith_obj(op, tm, num, num_check) \
      _vmcase_arith_obj(op, tm, str, str_check) \
      _vmcase_arith_obj(op, tm, obj, obj_check) \

#define _vmcase_arith_chk(op,out) \
      vmcase(OP(op,out,chk),      \
        luaVS_specialize(L);      \
        dispatch_again            \
      )
#define vmcase_arith_chk(op)      \
      _vmcase_arith_chk(op, ___)  \
      _vmcase_arith_chk(op, num)  \
      _vmcase_arith_chk(op, str)  \
      _vmcase_arith_chk(op, obj)

#define vmcase_arith(op,func,tm)      \
      vmcase_arith_raw(op, func, tm)  \
      vmcase_arith_num(op, func)      \
      vmcase_arith_obj(op, tm)        \
      vmcase_arith_chk(op)

      vmcase_arith(ADD, luai_numadd, TM_ADD)
      vmcase_arith(SUB, luai_numsub, TM_SUB)
      vmcase_arith(MUL, luai_nummul, TM_MUL)
      vmcase_arith(DIV, luai_numdiv, TM_DIV)
      vmcase_arith(MOD, luai_nummod, TM_MOD)
      vmcase_arith(POW, luai_numpow, TM_POW)
/* ------------------------------------------------------------------------ */
#define vmcase_unm_raw(out,guard)                   \
      vmcase(OP(UNM,out,___),                       \
        TValue *rb = RB(i);                         \
        Protect(luaV_arith(L, ra, rb, rb, TM_UNM)); \
        {guard;}                                    \
      )
#define vmcase_unm_num(out,guard)                   \
      vmcase(OP(UNM,out,num),                       \
        TValue *rb = RB(i);                         \
        setnvalue(ra, luai_numunm(L, nvalue(rb)));  \
        {guard;}                                    \
      )
#define vmcase_unm_chk(out)           \
      vmcase(OP(UNM,out,chk),         \
        luaVS_specialize(L);          \
        dispatch_again                \
      )

      vmcase_unm_raw(___, )
      vmcase_unm_raw(num, num_check)
      vmcase_unm_raw(str, str_check)
      vmcase_unm_raw(obj, obj_check)
      vmcase_unm_num(___, )
      vmcase_unm_chk(___)
      vmcase_unm_chk(num)
      vmcase_unm_chk(str)
      vmcase_unm_chk(obj)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(NOT),
        setbvalue(ra, l_isfalse(RB(i)));
      )
/* ------------------------------------------------------------------------ */
#define vmcase_len_raw(out,guard)           \
      vmcase(OP(LEN,out,___),               \
        Protect(luaV_objlen(L, ra, RB(i))); \
        {guard;}                            \
      )
#define vmcase_len_str(out,guard)                     \
      vmcase(OP(LEN,out,str),                         \
        setnvalue(ra, cast_num(tsvalue(RB(i))->len))  \
        {guard;}                                      \
      )
#define vmcase_len_chk(out)   \
      vmcase(OP(LEN,out,chk), \
        luaVS_specialize(L);  \
        dispatch_again        \
      )

      vmcase_len_raw(___, )
      vmcase_len_raw(num, num_check)
      vmcase_len_raw(str, str_check)
      vmcase_len_raw(obj, obj_check)
      vmcase_len_str(___, )
      vmcase_len_chk(___)
      vmcase_len_chk(num)
      vmcase_len_chk(str)
      vmcase_len_chk(obj)    
/* ------------------------------------------------------------------------ */
#define vmcase_concat(out,guard)                                            \
      vmcase(OP(CONCAT,out,___),                                            \
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
      vmcase_concat(obj, obj_check)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(JMP),
        dojump(ci, i, 0);
      )
/* ------------------------------------------------------------------------ */
      vmcase(OP(EQ,___,___),
        Protect(
          if (cast_int(equalobj(L, RKB(i), RKC(i))) != GETARG_A(i))
            ci->u.l.savedpc++;
          else
            donextjump(ci);
        )
      )
      vmcase(OP(EQ,___,chk),
        luaVS_specialize(L);
        dispatch_again
      )
      vmcase(OP(EQ,___,num),
        if (luai_numeq(nvalue(RKB(i)), nvalue(RKC(i))) != GETARG_A(i))
          ci->u.l.savedpc++;
        else
          donextjump(ci);
      )
      vmcase(OP(EQ,___,str),
        if (eqstr(rawtsvalue(RKB(i)), rawtsvalue(RKC(i))) != GETARG_A(i))
          ci->u.l.savedpc++;
        else
          donextjump(ci);
      )
      vmcase(OP(EQ,___,obj),
        if (luaV_equalobj_(L, RKB(i), RKC(i)) != GETARG_A(i))
          ci->u.l.savedpc++;
        else
          donextjump(ci);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_less_raw(op,func)                      \
      vmcase(OP(op,___,___),                          \
        Protect(                                      \
          if (func(L, RKB(i), RKC(i)) != GETARG_A(i)) \
            ci->u.l.savedpc++;                        \
          else                                        \
            donextjump(ci);                           \
        )                                             \
      )

#define vmcase_less_num(op,numfunc)               \
      vmcase(OP(op,___,num),                      \
        lua_Number nb = nvalue(RKB(i));           \
        lua_Number nc = nvalue(RKC(i));           \
        Protect(                                  \
          if (numfunc(L, nb, nc) != GETARG_A(i))  \
            ci->u.l.savedpc++;                    \
          else                                    \
            donextjump(ci);                       \
        )                                         \
      )

#define vmcase_less_str(op,cmpop)               \
      vmcase(OP(op,___,str),                    \
        const TString *sb = rawtsvalue(RKB(i)); \
        const TString *sc = rawtsvalue(RKC(i)); \
        Protect(                                \
          int res = l_strcmp(sb, sc) cmpop 0;   \
          if (res != GETARG_A(i))               \
            ci->u.l.savedpc++;                  \
          else                                  \
            donextjump(ci);                     \
        )                                       \
      )

#define vmcase_less_chk(op)   \
      vmcase(OP(op,___,chk),  \
        luaVS_specialize(L);  \
        dispatch_again        \
      )

#define vmcase_less(op, func, numfunc, cmpop) \
      vmcase_less_raw(op, func)               \
      vmcase_less_num(op, numfunc)            \
      vmcase_less_str(op, cmpop)              \
      vmcase_less_chk(op)

      vmcase_less(LT, luaV_lessthan, luai_numlt, <)
      vmcase_less(LE, luaV_lessequal, luai_numle, <=)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(TEST),
        if (GETARG_C(i) ? l_isfalse(ra) : !l_isfalse(ra))
            ci->u.l.savedpc++;
        else
          donextjump(ci);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_testset(out,guard)                         \
      vmcase(OP(TESTSET,out,___),                         \
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
      vmcase_testset(obj, obj_check)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(CALL),
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
      vmcase(sOP(TAILCALL),
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
      vmcasenb(sOP(RETURN),
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
      vmcase(sOP(FORLOOP),
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
      vmcase(sOP(FORPREP),
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
      vmcase(sOP(TFORCALL),
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
      vmcase(sOP(TFORLOOP),
        // l_tforloop:
        if (!ttisnil(ra + 1)) {  /* continue loop? */
          setobjs2s(L, ra, ra + 1);  /* save control variable */
          ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(sOP(SETLIST),
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
      vmcase(sOP(CLOSURE),
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
      vmcase(sOP(VARARG),
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
      vmcase(OP(CHKTYPE,___,___), /* nop */)
      vmcase(OP(CHKTYPE,num,___), num_check)
      vmcase(OP(CHKTYPE,str,___), str_check)
      vmcase(OP(CHKTYPE,obj,___), obj_check)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(EXTRAARG),
        lua_assert(0);
      )
/* ------------------------------------------------------------------------ */
      default:
        printf("*** ILLEGAL OP: %i (%i)", GET_OPCODE(i), i);
        lua_assert(0);
        break;
    }
  }
}

