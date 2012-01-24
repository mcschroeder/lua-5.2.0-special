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

#define _luaV_gettable_def(name, getfunc, keyfunc) \
void name (lua_State *L, const TValue *t, TValue *key, StkId val) { \
  int loop; \
  for (loop = 0; loop < MAXTAGLOOP; loop++) { \
    const TValue *tm; \
    if (ttistable(t)) {  /* `t' is a table? */ \
      Table *h = hvalue(t); \
      const TValue *res = getfunc(h, keyfunc(key)); \
      if (!ttisnil(res) ||  /* result is not nil? */ \
          (tm = fasttm(L, h->metatable, TM_INDEX)) == NULL) { /* no TM? */ \
        setobj2s(L, val, res); \
        return; \
      } \
      /* else will try the tag method */ \
    } \
    else if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_INDEX))) \
      luaG_typeerror(L, t, "index"); \
    if (ttisfunction(tm)) { \
      callTM(L, tm, t, key, val, 1); \
      return; \
    } \
    t = tm;  /* else repeat with 'tm' */ \
  } \
  luaG_runerror(L, "loop in gettable"); \
}

_luaV_gettable_def(luaV_gettable, luaH_get,);
_luaV_gettable_def(luaV_gettable_int, luaH_getint, nvalue);
_luaV_gettable_def(luaV_gettable_str, luaH_getstr, rawtsvalue);
_luaV_gettable_def(luaV_gettable_obj, luaH_getobj,);

#define luaV_gettable_raw(L,t,key,val) luaV_gettable(L,t,key,val)


#define _luaV_settable_def(name, getfunc, keyfunc) \
void name (lua_State *L, const TValue *t, TValue *key, StkId val) { \
  int loop; \
  for (loop = 0; loop < MAXTAGLOOP; loop++) { \
    const TValue *tm; \
    if (ttistable(t)) {  /* `t' is a table? */ \
      Table *h = hvalue(t); \
      TValue *oldval = cast(TValue *, getfunc(h, keyfunc(key))); \
      /* if previous value is not nil, there must be a previous entry
         in the table; moreover, a metamethod has no relevance */ \
      if (!ttisnil(oldval) || \
         /* previous value is nil; must check the metamethod */ \
         ((tm = fasttm(L, h->metatable, TM_NEWINDEX)) == NULL && \
         /* no metamethod; is there a previous entry in the table? */ \
         (oldval != luaO_nilobject || \
         /* no previous entry; must create one. (The next test is
            always true; we only need the assignment.) */ \
         (oldval = luaH_newkey(L, h, key), 1)))) { \
        /* no metamethod and (now) there is an entry with given key */ \
        setobj2t(L, oldval, val);  /* assign new value to that entry */ \
        invalidateTMcache(h); \
        luaC_barrierback(L, obj2gco(h), val); \
        return; \
      } \
      /* else will try the metamethod */ \
    } \
    else  /* not a table; check metamethod */ \
      if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX))) \
        luaG_typeerror(L, t, "index"); \
    /* there is a metamethod */ \
    if (ttisfunction(tm)) { \
      callTM(L, tm, t, key, val, 0); \
      return; \
    } \
    t = tm;  /* else repeat with 'tm' */ \
  } \
  luaG_runerror(L, "loop in settable"); \
}

_luaV_settable_def(luaV_settable, luaH_get,);
_luaV_settable_def(luaV_settable_int, luaH_getint, nvalue);
_luaV_settable_def(luaV_settable_str, luaH_getstr, rawtsvalue);
_luaV_settable_def(luaV_settable_obj, luaH_getobj,);

#define luaV_settable_raw(L,t,key,val) luaV_settable(L,t,key,val)



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


/*
** finish execution of an opcode interrupted by an yield
*/
void luaV_finishOp (lua_State *L) {
  CallInfo *ci = L->ci;
  StkId base = ci->u.l.base;
  Instruction inst = *(ci->u.l.savedpc - 1);  /* interrupted instruction */
  OpCode op = GET_OPCODE(inst);
#ifdef DEBUG_PRINT
    int _pc = pcRel(ci->u.l.savedpc-1, ci_func(ci)->p);
    printf("%s <%s:%i>[%i] ", __func__, getstr(ci_func(ci)->p->source), getfuncline(ci_func(ci)->p, _pc), _pc);  
    printop(op); printf("\n");  
#endif
  switch (luaP_opcode2group[op]) {  /* finish its execution */
    case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: 
    case OP_MOD: case OP_POW: case OP_UNM: case OP_LEN:     
    case OP_GETTABUP: case OP_GETTABLE: case OP_SELF: {
      StkId ra = base + GETARG_A(inst);
      setobjs2s(L, ra, --L->top);
      if (opout(op) == OpType_chk) {
        LClosure *cl = clLvalue(ci->func);
        int pc = pcRel(ci->u.l.savedpc, cl->p);
        if (rttype(ra) != cl->p->exptypes[pc].t)
          luaVS_despecialize(L, GETARG_A(inst));
      }
      break;
    }
    case OP_LE: case OP_LT: case OP_EQ: {
      int res = !l_isfalse(L->top - 1);
      L->top--;
      /* metamethod should not be called when operand is K */
      lua_assert(!opbk(op));
      if (op2grp(op) == OP_LE &&  /* "<=" using "<" instead? */
          ttisnil(luaT_gettmbyobj(L, base + GETARG_B(inst), TM_LE)))
        res = !res;  /* invert result */
      lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(JMP));
      if (res != GETARG_A(inst))  /* condition failed? */
        ci->u.l.savedpc++;  /* skip jump instruction */
      break;
    }
    case OP_CONCAT: {
      StkId top = L->top - 1;  /* top when 'call_binTM' was called */
      int b = GETARG_B(inst);      /* first element to concatenate */
      int total = cast_int(top - 1 - (base + b));  /* yet to concatenate */
      setobj2s(L, top - 2, top);  /* put TM result in proper position */
      if (total > 1) {  /* are there elements to concat? */
        L->top = top - 1;  /* top is one after last element (at top-2) */
        luaV_concat(L, total);  /* concat them (may yield again) */
      }
      /* move final result to final position */
      StkId ra = ci->u.l.base + GETARG_A(inst);
      setobj2s(L, ra, L->top - 1);
      if (opout(op) == OpType_chk) {
        LClosure *cl = clLvalue(ci->func);
        int pc = pcRel(ci->u.l.savedpc, cl->p);
        if (rttype(ra) != cl->p->exptypes[pc].t)
          luaVS_despecialize(L, GETARG_A(inst));
      }
      L->top = ci->top;  /* restore top */
      break;
    }
    case OP_TFORCALL: {
      lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(TFORLOOP));
      L->top = ci->top;  /* correct top */
      break;
    }
    case OP_CALL: {
      if (GETARG_C(inst) - 1 >= 0)  /* nresults >= 0? */
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


#define RA(i)	(base+GETARG_A(i))
/* to be used after possible stack reallocation */
/*
#define RB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgR, base+GETARG_B(i))
#define RC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgR, base+GETARG_C(i))
#define KB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgK, k+GETARG_B(i))
#define KC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgK, k+GETARG_C(i))
*/
/*
#define KBx(i)  \
  (k + (GETARG_Bx(i) != 0 ? GETARG_Bx(i) - 1 : GETARG_Ax(*ci->u.l.savedpc++)))
*/
// TODO: the reasons for this not working are a little unclear...
#define KBx(i) (k+GETARG_Bx(i))

// TODO: the modes checking is now a little trickier...
#define RB(i) (base+GETARG_B(i))
#define RC(i) (base+GETARG_C(i))
#define KB(i) (k+GETARG_B(i))
#define KC(i) (k+GETARG_C(i))


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




#define TypeGuard \
  if (rttype(ra) != cl->p->exptypes[pcRel(ci->u.l.savedpc, cl->p)].t) { \
    luaVS_despecialize(L, GETARG_A(i)); }



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

newframe_param_guard: { /* guard against polymorphic parameters */
  Proto *p = clLvalue(ci->func)->p;
  int reg;
  for (reg = 0; reg < p->numparams; reg++) {
    int t = p->paramtypes[reg];
    if (t != LUA_TNONE && t != rttype(ci->u.l.base+reg)) {
      luaVS_despecialize_param(p, reg);
    }
  }
}

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
    //printf("%s %i ", luaP_opnames[GET_OPGROUP(i)], GET_OPCODE(i));
    printop(GET_OPCODE(i));
    printf("\n");
    #endif

    vmdispatch (GET_OPCODE(i)) {
/* ------------------------------------------------------------------------ */
#define vmcase_move(ret,guard)          \
      vmcase(OP(MOVE,ret,___,___,___),  \
        setobjs2s(L, ra, RB(i));        \
        {guard;}                        \
      )

      vmcase_move(raw, )
      vmcase_move(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
#define vmcase_loadk(ret,guard)         \
      vmcase(OP(LOADK,ret,___,___,___), \
        setobj2s(L, ra, KBx(i));        \
        {guard;}                        \
      )

      vmcase_loadk(raw, )
      vmcase_loadk(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
#define vmcase_loadkx(ret,guard)                                    \
      vmcase(OP(LOADKX,ret,___,___,___),                            \
        TValue *rb;                                                 \
        lua_assert(GET_OPCODE(*ci->u.l.savedpc) == sOP(EXTRAARG));  \
        rb = k + GETARG_Ax(*ci->u.l.savedpc++);                     \
        setobj2s(L, ra, rb);                                        \
        {guard;}                                                    \
      )

      vmcase_loadkx(raw, )
      vmcase_loadkx(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
#define vmcase_loadbool(ret,guard)          \
      vmcase(OP(LOADBOOL,ret,___,___,___),  \
        setbvalue(ra, GETARG_B(i));         \
        {guard;}                            \
        /* skip next instruction (if C) */  \
        if (GETARG_C(i)) ci->u.l.savedpc++; \
      )

      vmcase_loadbool(raw, )
      vmcase_loadbool(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
#define vmcase_loadnil(ret,guard)         \
      vmcase(OP(LOADNIL,ret,___,___,___), \
        int b = GETARG_B(i);              \
        do {                              \
          setnilvalue(ra++);              \
        } while (b--);                    \
        {guard;}                          \
      )

      vmcase_loadnil(raw, )
      vmcase_loadnil(chk, 
        ra = RA(i);
        b = GETARG_B(i);
        int a = GETARG_A(i);
        int *exptypes = cl->p->exptypes[pcRel(ci->u.l.savedpc, cl->p)].ts;
        int j;
        for (j = 0; j < b; j++) {
          int t = exptypes[j];
          if (t != LUA_TNONE && t != rttype(ra+j))
            luaVS_despecialize(L, a+j);
        }
      )
/* ------------------------------------------------------------------------ */
#define vmcase_getupval(ret,guard)                    \
      vmcase(OP(GETUPVAL,ret,___,___,___),            \
        setobj2s(L, ra, cl->upvals[GETARG_B(i)]->v);  \
        {guard;}                                      \
      )

      vmcase_getupval(raw, )
      vmcase_getupval(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
#define vmcase_gettab_spec(op,ret,spec,ck,b,c,guard)  \
    vmcase(OP(op,ret,spec,___,ck),                    \
      Protect(luaV_gettable_##spec(L, b, c, ra));     \
      {guard;}                                        \
    )
#define vmcase_gettab_chk(op,ret)   \
    vmcase(OP(op,ret,chk,___,reg),  \
      luaVS_specialize(L);          \
      dispatch_again                \
    )
#define vmcase_gettab(op,b)                                     \
    vmcase_gettab_spec(op, raw, raw, reg, b, RC(i), )           \
    vmcase_gettab_spec(op, raw, raw, kst, b, KC(i), )           \
    vmcase_gettab_spec(op, raw, int, reg, b, RC(i), )           \
    vmcase_gettab_spec(op, raw, int, kst, b, KC(i), )           \
    vmcase_gettab_spec(op, raw, str, reg, b, RC(i), )           \
    vmcase_gettab_spec(op, raw, str, kst, b, KC(i), )           \
    vmcase_gettab_spec(op, raw, obj, reg, b, RC(i), )           \
    vmcase_gettab_spec(op, raw, obj, kst, b, KC(i), )           \
    vmcase_gettab_chk(op, raw)                                  \
    vmcase_gettab_spec(op, chk, raw, reg, b, RC(i), TypeGuard)  \
    vmcase_gettab_spec(op, chk, raw, kst, b, KC(i), TypeGuard)  \
    vmcase_gettab_spec(op, chk, int, reg, b, RC(i), TypeGuard)  \
    vmcase_gettab_spec(op, chk, int, kst, b, KC(i), TypeGuard)  \
    vmcase_gettab_spec(op, chk, str, reg, b, RC(i), TypeGuard)  \
    vmcase_gettab_spec(op, chk, str, kst, b, KC(i), TypeGuard)  \
    vmcase_gettab_spec(op, chk, obj, reg, b, RC(i), TypeGuard)  \
    vmcase_gettab_spec(op, chk, obj, kst, b, KC(i), TypeGuard)  \
    vmcase_gettab_chk(op, chk)

    vmcase_gettab(GETTABLE, RB(i))
    vmcase_gettab(GETTABUP, cl->upvals[GETARG_B(i)]->v)
/* ------------------------------------------------------------------------ */
#define vmcase_settab_spec(op,spec,bk,ck,b,c,a)     \
      vmcase(OP(op,___,spec,bk,ck),                 \
        Protect(luaV_settable_##spec(L, a, b, c));  \
      )
#define vmcase_settab_chk(op,bk,ck) \
      vmcase(OP(op,___,chk,bk,ck),  \
        luaVS_specialize(L);        \
        dispatch_again              \
      )      
#define vmcase_settab(op,a)                                   \
      vmcase_settab_spec(op, raw, reg, reg, RB(i), RC(i), a)  \
      vmcase_settab_spec(op, raw, reg, kst, RB(i), KC(i), a)  \
      vmcase_settab_spec(op, raw, kst, reg, KB(i), RC(i), a)  \
      vmcase_settab_spec(op, raw, kst, kst, KB(i), KC(i), a)  \
      vmcase_settab_spec(op, int, reg, reg, RB(i), RC(i), a)  \
      vmcase_settab_spec(op, int, reg, kst, RB(i), KC(i), a)  \
      vmcase_settab_spec(op, int, kst, reg, KB(i), RC(i), a)  \
      vmcase_settab_spec(op, int, kst, kst, KB(i), KC(i), a)  \
      vmcase_settab_spec(op, str, reg, reg, RB(i), RC(i), a)  \
      vmcase_settab_spec(op, str, reg, kst, RB(i), KC(i), a)  \
      vmcase_settab_spec(op, str, kst, reg, KB(i), RC(i), a)  \
      vmcase_settab_spec(op, str, kst, kst, KB(i), KC(i), a)  \
      vmcase_settab_spec(op, obj, reg, reg, RB(i), RC(i), a)  \
      vmcase_settab_spec(op, obj, reg, kst, RB(i), KC(i), a)  \
      vmcase_settab_spec(op, obj, kst, reg, KB(i), RC(i), a)  \
      vmcase_settab_spec(op, obj, kst, kst, KB(i), KC(i), a)  \
      vmcase_settab_chk(op, reg, reg)                         \
      vmcase_settab_chk(op, reg, kst)

      vmcase_settab(SETTABLE, ra)
      vmcase_settab(SETTABUP, cl->upvals[GETARG_A(i)]->v)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(SETUPVAL),
        UpVal *uv = cl->upvals[GETARG_B(i)];
        setobj(L, uv->v, ra);
        luaC_barrier(L, uv, ra);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_newtable(ret,guard)                            \
      vmcase(OP(NEWTABLE,ret,___,___,___),                    \
        int b = GETARG_B(i);                                  \
        int c = GETARG_C(i);                                  \
        Table *t = luaH_new(L);                               \
        sethvalue(L, ra, t);                                  \
        if (b != 0 || c != 0)                                 \
          luaH_resize(L, t, luaO_fb2int(b), luaO_fb2int(c));  \
        checkGC(L,                                            \
          L->top = ra + 1;  /* limit of live values */        \
          luaC_step(L);                                       \
          L->top = ci->top;  /* restore top */                \
        )                                                     \
        {guard;}                                              \
      )

      vmcase_newtable(raw, )
      vmcase_newtable(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
#define vmcase_self(ret,ck,c,guard)               \
      vmcase(OP(SELF,ret,___,___,ck),             \
        StkId rb = RB(i);                         \
        setobjs2s(L, ra+1, rb);                   \
        Protect(luaV_gettable_str(L, rb, c, ra)); \
        {guard;}                                  \
      )

      vmcase_self(raw, reg, RC(i), )
      vmcase_self(raw, kst, KC(i), )
      vmcase_self(chk, reg, RC(i), TypeGuard)
      vmcase_self(chk, kst, KC(i), TypeGuard)
/* ------------------------------------------------------------------------ */
#define _vmcase_arith_raw(op,ret,bk,ck,b,c,func,tm,guard) \
      vmcase(OP(op,ret,raw,bk,ck),                        \
        TValue *rb = b;                                   \
        TValue *rc = c;                                   \
        if (ttisnumber(rb) && ttisnumber(rc)) {           \
          lua_Number nb = nvalue(rb);                     \
          lua_Number nc = nvalue(rc);                     \
          setnvalue(ra, func(L, nb, nc));                 \
        }                                                 \
        else { Protect(luaV_arith(L, ra, rb, rc, tm)); }  \
        {guard;}                                          \
      )
#define vmcase_arith_raw(op,func,tm)                                          \
      _vmcase_arith_raw(op, raw, reg, reg, RB(i), RC(i), func, tm, )          \
      _vmcase_arith_raw(op, raw, reg, kst, RB(i), KC(i), func, tm, )          \
      _vmcase_arith_raw(op, raw, kst, reg, KB(i), RC(i), func, tm, )          \
      _vmcase_arith_raw(op, raw, kst, kst, KB(i), KC(i), func, tm, )          \
      _vmcase_arith_raw(op, chk, reg, reg, RB(i), RC(i), func, tm, TypeGuard) \
      _vmcase_arith_raw(op, chk, reg, kst, RB(i), KC(i), func, tm, TypeGuard) \
      _vmcase_arith_raw(op, chk, kst, reg, KB(i), RC(i), func, tm, TypeGuard) \
      _vmcase_arith_raw(op, chk, kst, kst, KB(i), KC(i), func, tm, TypeGuard)

#define _vmcase_arith_num(op,ret,bk,ck,b,c,func,guard)  \
      vmcase(OP(op,ret,num,bk,ck),                      \
        setnvalue(ra, func(L, nvalue(b), nvalue(c)));   \
        {guard;}                                        \
      )
#define vmcase_arith_num(op,func)                                         \
      _vmcase_arith_num(op, raw, reg, reg, RB(i), RC(i), func, )          \
      _vmcase_arith_num(op, raw, reg, kst, RB(i), KC(i), func, )          \
      _vmcase_arith_num(op, raw, kst, reg, KB(i), RC(i), func, )          \
      _vmcase_arith_num(op, chk, reg, reg, RB(i), RC(i), func, TypeGuard) \
      _vmcase_arith_num(op, chk, reg, kst, RB(i), KC(i), func, TypeGuard) \
      _vmcase_arith_num(op, chk, kst, reg, KB(i), RC(i), func, TypeGuard)

#define _vmcase_arith_obj(op,ret,bk,ck,b,c,tm,guard)  \
      vmcase(OP(op,ret,obj,bk,ck),                    \
        Protect(                                      \
          if (!call_binTM(L, b, c, ra, tm))           \
            luaG_aritherror(L, b, c);                 \
        )                                             \
        {guard;}                                      \
      )
#define vmcase_arith_obj(op,tm)                                         \
      _vmcase_arith_obj(op, raw, reg, reg, RB(i), RC(i), tm, )          \
      _vmcase_arith_obj(op, raw, reg, kst, RB(i), KC(i), tm, )          \
      _vmcase_arith_obj(op, raw, kst, reg, KB(i), RC(i), tm, )          \
      _vmcase_arith_obj(op, chk, reg, reg, RB(i), RC(i), tm, TypeGuard) \
      _vmcase_arith_obj(op, chk, reg, kst, RB(i), KC(i), tm, TypeGuard) \
      _vmcase_arith_obj(op, chk, kst, reg, KB(i), RC(i), tm, TypeGuard)


#define _vmcase_arith_chk(op,ret,bk,ck) \
      vmcase(OP(op,ret,chk,bk,ck),      \
        luaVS_specialize(L);            \
        dispatch_again                  \
      )
#define vmcase_arith_chk(op)                \
      _vmcase_arith_chk(op, raw, reg, reg)  \
      _vmcase_arith_chk(op, raw, reg, kst)  \
      _vmcase_arith_chk(op, raw, kst, reg)  \
      _vmcase_arith_chk(op, chk, reg, reg)  \
      _vmcase_arith_chk(op, chk, reg, kst)  \
      _vmcase_arith_chk(op, chk, kst, reg)

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
#define vmcase_unm_raw(ret,guard)                   \
      vmcase(OP(UNM,ret,raw,___,___),               \
        TValue *rb = RB(i);                         \
        Protect(luaV_arith(L, ra, rb, rb, TM_UNM)); \
        {guard;}                                    \
      )
#define vmcase_unm_num(ret,guard)                   \
      vmcase(OP(UNM,ret,num,___,___),               \
        TValue *rb = RB(i);                         \
        setnvalue(ra, luai_numunm(L, nvalue(rb)));  \
        {guard;}                                    \
      )
#define vmcase_unm_chk(ret)           \
      vmcase(OP(UNM,ret,chk,___,___), \
        luaVS_specialize(L);          \
        dispatch_again                \
      )

      vmcase_unm_raw(raw, )
      vmcase_unm_raw(chk, TypeGuard)
      vmcase_unm_num(raw, )
      vmcase_unm_num(chk, TypeGuard)
      vmcase_unm_chk(raw)
      vmcase_unm_chk(chk)
/* ------------------------------------------------------------------------ */
#define vmcase_not(ret,guard)             \
      vmcase(OP(NOT,ret,___,___,___),     \
        setbvalue(ra, l_isfalse(RB(i)));  \
        {guard;}                          \
      )

      vmcase_not(raw, )
      vmcase_not(chk, TypeGuard)      
/* ------------------------------------------------------------------------ */
#define vmcase_len_raw(ret,guard)           \
      vmcase(OP(LEN,ret,raw,___,___),       \
        Protect(luaV_objlen(L, ra, RB(i))); \
        {guard;}                            \
      )
#define vmcase_len_str(ret,guard)                     \
      vmcase(OP(LEN,ret,str,___,___),                 \
        setnvalue(ra, cast_num(tsvalue(RB(i))->len))  \
        {guard;}                                      \
      )
#define vmcase_len_tab(ret,guard)                           \
      vmcase(OP(LEN,ret,tab,___,___),                       \
        TValue *rb = RB(i);                                 \
        Table *h = hvalue(rb);                              \
        const TValue *tm = fasttm(L, h->metatable, TM_LEN); \
        if (tm) callTM(L, tm, rb, rb, ra, 1);               \
        else setnvalue(ra, cast_num(luaH_getn(h)));         \
        {guard;}                                            \
      )
#define vmcase_len_chk(ret)           \
      vmcase(OP(LEN,ret,chk,___,___), \
        luaVS_specialize(L);          \
        dispatch_again                \
      )

      vmcase_len_raw(raw, )
      vmcase_len_raw(chk, TypeGuard)
      vmcase_len_str(raw, )
      vmcase_len_str(chk, TypeGuard)
      vmcase_len_tab(raw, )
      vmcase_len_tab(chk, TypeGuard)
      vmcase_len_chk(raw)
      vmcase_len_chk(chk)    
/* ------------------------------------------------------------------------ */
#define vmcase_concat(ret,guard)                                            \
      vmcase(OP(CONCAT,ret,___,___,___),                                    \
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

      vmcase_concat(raw, )
      vmcase_concat(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
      vmcase(sOP(JMP),
        dojump(ci, i, 0);
      )
/* ------------------------------------------------------------------------ */
#define vmcase_eq(bk,ck,b,c)                              \
      vmcase(OP(EQ,___,___,bk,ck),                        \
        Protect(                                          \
          if (cast_int(equalobj(L, b, c)) != GETARG_A(i)) \
            ci->u.l.savedpc++;                            \
          else                                            \
            donextjump(ci);                               \
        )                                                 \
      )

      vmcase_eq(reg, reg, RB(i), RC(i))
      vmcase_eq(reg, kst, RB(i), KC(i))
      vmcase_eq(kst, reg, KB(i), RC(i))
      vmcase_eq(kst, kst, KB(i), KC(i))
/* ------------------------------------------------------------------------ */
#define _vmcase_less_raw(op,bk,ck,b,c,func) \
      vmcase(OP(op,___,raw,bk,ck),          \
        Protect(                            \
          if (func(L, b, c) != GETARG_A(i)) \
            ci->u.l.savedpc++;              \
          else                              \
            donextjump(ci);                 \
        )                                   \
      )
#define vmcase_less_raw(op,func)                          \
      _vmcase_less_raw(op, reg, reg, RB(i), RC(i), func)  \
      _vmcase_less_raw(op, reg, kst, RB(i), KC(i), func)  \
      _vmcase_less_raw(op, kst, reg, KB(i), RC(i), func)  \
      _vmcase_less_raw(op, kst, kst, KB(i), KC(i), func)

#define _vmcase_less_num(op,bk,ck,b,c,numfunc)                  \
      vmcase(OP(op,___,num,bk,ck),                              \
        Protect(                                                \
          if (numfunc(L, nvalue(b), nvalue(c)) != GETARG_A(i))  \
            ci->u.l.savedpc++;                                  \
          else                                                  \
            donextjump(ci);                                     \
        )                                                       \
      )
#define vmcase_less_num(op,numfunc)                         \
      _vmcase_less_num(op, reg, reg, RB(i), RC(i), numfunc) \
      _vmcase_less_num(op, reg, kst, RB(i), KC(i), numfunc) \
      _vmcase_less_num(op, kst, reg, KB(i), RC(i), numfunc) \
      _vmcase_less_num(op, kst, kst, KB(i), KC(i), numfunc)

#define _vmcase_less_str(op,bk,ck,b,c,cmpop)                        \
      vmcase(OP(op,___,str,bk,ck),                                  \
        Protect(                                                    \
          int res = l_strcmp(rawtsvalue(b), rawtsvalue(c)) cmpop 0; \
          if (res != GETARG_A(i))                                   \
            ci->u.l.savedpc++;                                      \
          else                                                      \
            donextjump(ci);                                         \
        )                                                           \
      )
#define vmcase_less_str(op,cmpop)                         \
      _vmcase_less_str(op, reg, reg, RB(i), RC(i), cmpop) \
      _vmcase_less_str(op, reg, kst, RB(i), KC(i), cmpop) \
      _vmcase_less_str(op, kst, reg, KB(i), RC(i), cmpop) \
      _vmcase_less_str(op, kst, kst, KB(i), KC(i), cmpop)

#define _vmcase_less_chk(op,bk,ck)  \
      vmcase(OP(op,___,chk,bk,ck),  \
        luaVS_specialize(L);        \
        dispatch_again              \
      )
#define vmcase_less_chk(op)           \
      _vmcase_less_chk(op, reg, reg)  \
      _vmcase_less_chk(op, reg, kst)  \
      _vmcase_less_chk(op, kst, reg)

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
#define vmcase_testset(ret,guard)                         \
      vmcase(OP(TESTSET,ret,___,___,___),                 \
        TValue *rb = RB(i);                               \
        if (GETARG_C(i) ? l_isfalse(rb) : !l_isfalse(rb)) \
          ci->u.l.savedpc++;                              \
        else {                                            \
          setobjs2s(L, ra, rb);                           \
          {guard;}                                        \
          donextjump(ci);                                 \
        }                                                 \
      )

      vmcase_testset(raw, )
      vmcase_testset(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
// TODO: it would maybe pay off to have separate 0-return and 1-return CALL
//        variations (to avoid the exptypes.ts stuff...)
      vmcasenb(OP(CALL,chk,___,___,___),
        ci->callstatus |= CIST_SPECRES;
        /* fall through */
      )
      vmcase(OP(CALL,raw,___,___,___),
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
          goto newframe_param_guard;
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
          goto newframe_param_guard;
        }
      )
/* ------------------------------------------------------------------------ */
      vmcasenb(sOP(RETURN),
        /* remember parameter types */
        Proto *p = clLvalue(ci->func)->p;
        int arg;
        for (arg=0; arg < p->numparams; arg++)
          p->paramtypes[arg] = rttype(base+arg);

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
          lua_assert(GET_OPGROUP(*((ci)->u.l.savedpc - 1)) == OP_CALL);
          goto newframe;  /* restart luaV_execute over new Lua function */
        }
      )
/* ------------------------------------------------------------------------ */
// TODO: specialize external index / variables
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
#define vmcase_tforcall(ret,guard)                                \
      vmcase(OP(TFORCALL,ret,___,___,___),                        \
        StkId cb = ra + 3;  /* call base */                       \
        setobjs2s(L, cb+2, ra+2);                                 \
        setobjs2s(L, cb+1, ra+1);                                 \
        setobjs2s(L, cb, ra);                                     \
        L->top = cb + 3;  /* func. + 2 args (state and index) */  \
        int nresults = GETARG_C(i);                               \
        Protect(luaD_call(L, cb, nresults, 1));                   \
        {guard;}                                                  \
        L->top = ci->top;                                         \
        i = *(ci->u.l.savedpc++);  /* go to next instruction */   \
        ra = RA(i);                                               \
        lua_assert(GET_OPCODE(i) == sOP(TFORLOOP));               \
        goto l_tforloop;                                          \
      )

      vmcase_tforcall(raw, )
      vmcase_tforcall(chk,
        int pc = pcRel(ci->u.l.savedpc, clLvalue(ci->func)->p);
        int *exptypes = clLvalue(ci->func)->p->exptypes[pc].ts;        
        int reg = cb - ci->u.l.base;
        int j;
        for (j = 0; j < nresults; j++) {
          if (exptypes[j] != rttype(cb++))
            luaVS_despecialize(L, reg);
          reg++;
        }
      )
/* ------------------------------------------------------------------------ */
      vmcase(sOP(TFORLOOP),
        l_tforloop:
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
#define vmcase_closure(ret,guard)                                             \
      vmcase(OP(CLOSURE,ret,___,___,___),                                     \
        Proto *p = cl->p->p[GETARG_Bx(i)];                                    \
        Closure *ncl = getcached(p, cl->upvals, base);  /* cached closure */  \
        if (ncl == NULL)  /* no match? */                                     \
          pushclosure(L, p, cl->upvals, base, ra);  /* create a new one */    \
        else                                                                  \
          setclLvalue(L, ra, ncl);  /* push cashed closure */                 \
        checkGC(L,                                                            \
          L->top = ra + 1;  /* limit of live values */                        \
          luaC_step(L);                                                       \
          L->top = ci->top;  /* restore top */                                \
        )                                                                     \
        {guard;}                                                              \
      )

      vmcase_closure(raw, )
      vmcase_closure(chk, TypeGuard)
/* ------------------------------------------------------------------------ */
#define vmcase_vararg(ret,guard)                                  \
      vmcase(OP(VARARG,ret,___,___,___),                          \
        int b = GETARG_B(i) - 1;                                  \
        int j;                                                    \
        int n = cast_int(base - ci->func) - cl->p->numparams - 1; \
        if (b < 0) {  /* B == 0? */                               \
          b = n;  /* get all var. arguments */                    \
          Protect(luaD_checkstack(L, n));                         \
          ra = RA(i);  /* previous call may change the stack */   \
          L->top = ra + n;                                        \
        }                                                         \
        for (j = 0; j < b; j++) {                                 \
          if (j < n) {                                            \
            setobjs2s(L, ra + j, base - n + j);                   \
          }                                                       \
          else {                                                  \
            setnilvalue(ra + j);                                  \
          }                                                       \
        }                                                         \
        {guard;}                                                  \
      )

      vmcase_vararg(raw, )
      vmcase_vararg(chk,
        int a = GETARG_A(i);
        int *exptypes = cl->p->exptypes[pcRel(ci->u.l.savedpc, cl->p)].ts;
        for (j = 0; j < b; j++) {
          int t = exptypes[j];          
          if (t != LUA_TNONE && t != rttype(ra+j))
            luaVS_despecialize(L, a+j);
        }
      )
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

