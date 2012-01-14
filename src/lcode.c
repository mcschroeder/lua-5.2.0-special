/*
** $Id: lcode.c,v 2.60 2011/08/30 16:26:41 roberto Exp $
** Code generator for Lua
** See Copyright Notice in lua.h
*/


#include <stdlib.h>

#define lcode_c
#define LUA_CORE

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"



#define hasjumps(e)	((e)->t != (e)->f)
#define isconst(e) ((e)->k == VK)


static int isnumeral(expdesc *e) {
  return (e->k == VKNUM && e->t == NO_JUMP && e->f == NO_JUMP);
}


void luaK_nil (FuncState *fs, int from, int n) {
  Instruction *previous;
  int l = from + n - 1;  /* last register to set nil */
  if (fs->pc > fs->lasttarget) {  /* no jumps to current position? */
    previous = &fs->f->code[fs->pc-1];
    if (GET_OPCODE(*previous) == OP_LOADNIL) {
      int pfrom = GETARG_A(*previous);
      int pl = pfrom + GETARG_B(*previous);
      if ((pfrom <= from && from <= pl + 1) ||
          (from <= pfrom && pfrom <= l + 1)) {  /* can connect both? */
        if (pfrom < from) from = pfrom;  /* from = min(from, pfrom) */
        if (pl > l) l = pl;  /* l = max(l, pl) */
        SETARG_A(*previous, from);
        SETARG_B(*previous, l - from);
        int ra;
        for (ra = from; ra <= l - from; ra++)
          reginfo_insert_store(fs, fs->pc-1, ra);
        return;
      }
    }  /* else go through */
  }  
  int ra;
  for (ra = from; ra <= from+n; ra++)
    reginfo_add_store(fs, ra);
  luaK_codeABC(fs, OP_LOADNIL, 1, from, n - 1, 0);  /* else no optimization */
}


int luaK_jump (FuncState *fs) {
  int jpc = fs->jpc;  /* save list of jumps to here */
  int j;
  fs->jpc = NO_JUMP;
  j = luaK_codeAsBx(fs, OP_JMP, 0, 0, NO_JUMP);
  luaK_concat(fs, &j, jpc);  /* keep them on hold */
  return j;
}


void luaK_ret (FuncState *fs, int first, int nret) {
  luaK_codeABC(fs, OP_RETURN, 0, first, nret+1, 0);
}


static int condjump (FuncState *fs, OpCode op, int sp, int A, int B, int C) {
  luaK_codeABC(fs, op, sp, A, B, C);
  return luaK_jump(fs);
}


static void fixjump (FuncState *fs, int pc, int dest) {
  Instruction *jmp = &fs->f->code[pc];
  int offset = dest-(pc+1);
  lua_assert(dest != NO_JUMP);
  if (abs(offset) > MAXARG_sBx)
    luaX_syntaxerror(fs->ls, "control structure too long");
  SETARG_sBx(*jmp, offset);
}


/*
** returns current `pc' and marks it as a jump target (to avoid wrong
** optimizations with consecutive instructions not in the same basic block).
*/
int luaK_getlabel (FuncState *fs) {
  fs->lasttarget = fs->pc;
  return fs->pc;
}


static int getjump (FuncState *fs, int pc) {
  int offset = GETARG_sBx(fs->f->code[pc]);
  if (offset == NO_JUMP)  /* point to itself represents end of list */
    return NO_JUMP;  /* end of list */
  else
    return (pc+1)+offset;  /* turn offset into absolute position */
}


static Instruction *getjumpcontrol (FuncState *fs, int pc) {
  Instruction *pi = &fs->f->code[pc];
  if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
    return pi-1;
  else
    return pi;
}


/*
** check whether list has any jump that do not produce a value
** (or produce an inverted value)
*/
static int need_value (FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getjump(fs, list)) {
    Instruction i = *getjumpcontrol(fs, list);
    if (GET_OPCODE(i) != OP_TESTSET) return 1;
  }
  return 0;  /* not found */
}


static int patchtestreg (FuncState *fs, int node, int reg) {
  Instruction *i = getjumpcontrol(fs, node);
  if (GET_OPCODE(*i) != OP_TESTSET)
    return 0;  /* cannot patch other instructions */
  if (reg != NO_REG && reg != GETARG_B(*i)) {
    SETARG_A(*i, reg);
    reginfo_insert_store(fs, testTMode(GET_OPCODE(*i)) ? node-1 : node, reg);
  }
  else  /* no register to put value or register already has the value */
    *i = CREATE_ABC(OP_TEST, 0, GETARG_B(*i), 0, GETARG_C(*i));  

  return 1;
}


static void removevalues (FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getjump(fs, list))
      patchtestreg(fs, list, NO_REG);
}


static void patchlistaux (FuncState *fs, int list, int vtarget, int reg,
                          int dtarget) {
  while (list != NO_JUMP) {
    int next = getjump(fs, list);
    if (patchtestreg(fs, list, reg))
      fixjump(fs, list, vtarget);
    else
      fixjump(fs, list, dtarget);  /* jump to default target */
    list = next;
  }
}


static void dischargejpc (FuncState *fs) {
  patchlistaux(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
  fs->jpc = NO_JUMP;
}


void luaK_patchlist (FuncState *fs, int list, int target) {
  if (target == fs->pc)
    luaK_patchtohere(fs, list);
  else {
    lua_assert(target < fs->pc);
    patchlistaux(fs, list, target, NO_REG, target);
  }
}


LUAI_FUNC void luaK_patchclose (FuncState *fs, int list, int level) {
  level++;  /* argument is +1 to reserve 0 as non-op */
  while (list != NO_JUMP) {
    int next = getjump(fs, list);
    lua_assert(GET_OPCODE(fs->f->code[list]) == OP_JMP &&
                (GETARG_A(fs->f->code[list]) == 0 ||
                 GETARG_A(fs->f->code[list]) >= level));
    SETARG_A(fs->f->code[list], level);
    list = next;
  }
}


void luaK_patchtohere (FuncState *fs, int list) {
  luaK_getlabel(fs);
  luaK_concat(fs, &fs->jpc, list);
}


void luaK_concat (FuncState *fs, int *l1, int l2) {
  if (l2 == NO_JUMP) return;
  else if (*l1 == NO_JUMP)
    *l1 = l2;
  else {
    int list = *l1;
    int next;
    while ((next = getjump(fs, list)) != NO_JUMP)  /* find last element */
      list = next;
    fixjump(fs, list, l2);
  }
}


/* -- register info ------------------------------------------------------ */

// TODO: naming: it doesn't grow the reginfo scope list it grows fs->reginfos
void reginfo_grow(FuncState *fs, int reg) {
  if (reg < fs->f->sizereginfos) return;  
  int oldsize = fs->f->sizereginfos;
  int newsize = reg+1;
  luaM_reallocvector(fs->ls->L, fs->f->reginfos, oldsize, newsize, RegInfo);
  fs->f->sizereginfos = newsize;
  while (oldsize < newsize) {
    printf("%s %i\n", __func__, oldsize);
    fs->f->reginfos[oldsize].next = NULL;
    fs->f->reginfos[oldsize++].state = REGINFO_STATE_UNUSED;    
  }
}

RegInfo *reginfo_get_last(FuncState *fs, int reg) {
  lua_assert(reg < fs->f->sizereginfos);
  RegInfo *reginfo = &(fs->f->reginfos[reg]);
  if (reginfo->state != REGINFO_STATE_UNUSED &&
      reginfo->state != REGINFO_STATE_LOCAL_UNUSED) {
    while (reginfo->next != NULL)
      reginfo = reginfo->next;
  }
  return reginfo;
}

void reginfo_insert(FuncState *fs, int pc, int reg, int store) {
  printf("%s: [%i] %i %s\t\n", __func__, pc, reg, store ? "store" : "load");
  reginfo_grow(fs, reg);
  RegInfo *reginfo = reginfo_get_last(fs, reg);
  switch (reginfo->state) {
    case REGINFO_STATE_TEMP:
      if (store) goto l_add_scope;
      /* else fall through */
    case REGINFO_STATE_LOCAL_OPEN: /* extend scope */
      if (reginfo->endpc < pc) { 
        reginfo->endpc = pc;
        reginfo->lastuse = store;
      }
      break;
    case REGINFO_STATE_LOCAL_CLOSED: /* add new scope */
    l_add_scope:
      reginfo->next = luaM_new(fs->ls->L, RegInfo);
      reginfo = reginfo->next;
      /* fall through */
    case REGINFO_STATE_UNUSED: /* initialize scope */      
      reginfo->startpc = pc;
      reginfo->endpc = pc;
      reginfo->state = REGINFO_STATE_TEMP;
      reginfo->nspec = 0;
      reginfo->firstuse = store;
      reginfo->lastuse = store;
      reginfo->next = NULL;      
      break;
    case REGINFO_STATE_LOCAL_UNUSED:
      reginfo->startpc = pc;
      reginfo->endpc = pc;
      reginfo->state = REGINFO_STATE_LOCAL_OPEN;
      reginfo->nspec = 0;
      reginfo->firstuse = store;
      reginfo->lastuse = store;
      reginfo->next = NULL;      
      break;      
  }
}

void reginfo_adjustlocal(FuncState *fs, int reg) {
  printf("%s: %i\n", __func__, reg);
  reginfo_grow(fs, reg);
  RegInfo *reginfo = reginfo_get_last(fs, reg);
  
  if (reginfo->state == REGINFO_STATE_UNUSED) /* function argument */ {
    //reginfo_insert(fs, /*-1*/0, reg, 1);
    reginfo->state = REGINFO_STATE_LOCAL_UNUSED;
    return;
  }
    // TODO: how do we do this so it makes sense?
    // is a pc of 0 (or -1 for that matter) potentially dangerous?
    // can there be temp uses before the first use of func arg reg?
  
  // OK, so apparently this function can get called before there is
  // a temp scope at reg (either the reg is a hitherto unusued funcarg or
  // we have a previously closed local and start a semantically new scope
  // in the same reg)
  // SOLUTIONS: we need to ensure theres a temp scope before calling this
  //  1) either we do it
  //  2) or the caller does it
  // in both cases we need the pc of where we at
  // OR
  //     additional uninitialized state either in state or startpc = -1
  // TODO: blergh
  if (reginfo->state == REGINFO_STATE_LOCAL_CLOSED) {
    reginfo_insert(fs, 0, reg, 1);
    reginfo_get_last(fs, reg)->state = REGINFO_STATE_LOCAL_UNUSED;
    return;
  }


  //printf("state=%i\n", reginfo->state);

  // adjustlocalvars is often called after the locals have been used and
  // their reginfos inserted, but at that point there were still known as 
  // temps. so now we simply rebrand them.
  lua_assert(reginfo->state == REGINFO_STATE_TEMP);
  reginfo->state = REGINFO_STATE_LOCAL_OPEN;
}

void reginfo_removelocal(FuncState *fs, int reg) {
  printf("%s: %i\n", __func__, reg);
  RegInfo *reginfo = reginfo_get_last(fs, reg);
  if (reginfo->state == REGINFO_STATE_LOCAL_UNUSED) {
    reginfo->state = REGINFO_STATE_UNUSED;
    return;
    // TODO: remove reginfo? 
    // NO, since reginfos needs to have at least one entry per register
  }
  lua_assert(reginfo->state == REGINFO_STATE_LOCAL_OPEN);
  reginfo->state = REGINFO_STATE_LOCAL_CLOSED;
}

/* ----------------------------------------------------------------------- */


static int luaK_code (FuncState *fs, Instruction i) {
  Proto *f = fs->f;
  dischargejpc(fs);  /* `pc' will change */
  /* put new instruction in code array */
  luaM_growvector(fs->ls->L, f->code, fs->pc, f->sizecode, Instruction,
                  MAX_INT, "opcodes");
  f->code[fs->pc] = i;
  /* save corresponding line information */
  luaM_growvector(fs->ls->L, f->lineinfo, fs->pc, f->sizelineinfo, int,
                  MAX_INT, "opcodes");
  f->lineinfo[fs->pc] = fs->ls->lastline;
  // printf("[%i] %i (code=%i/%s spec=%i)\n", 
  //          fs->pc,
  //          GET_OP(i), GET_OPCODE(i), GET_OPCODE(i) < NUM_OPCODES ? 
  //          luaP_opnames[GET_OPCODE(i)] : "unknown", GET_OPSPEC(i));
  return fs->pc++;
}


int luaK_codeABC (FuncState *fs, OpCode o, int s, int a, int b, int c) {
  lua_assert(getOpMode(o) == iABC);
  lua_assert(getBMode(o) != OpArgN || b == 0);
  lua_assert(getCMode(o) != OpArgN || c == 0);
  lua_assert(a <= MAXARG_A && b <= MAXARG_B && c <= MAXARG_C);
  return luaK_code(fs, CREATE_ABC(o, s, a, b, c));
}


int luaK_codeABx (FuncState *fs, OpCode o, int s, int a, unsigned int bc) {
  lua_assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
  lua_assert(getCMode(o) == OpArgN);
  lua_assert(a <= MAXARG_A && bc <= MAXARG_Bx);
  return luaK_code(fs, CREATE_ABx(o, s, a, bc));
}


static int codeextraarg (FuncState *fs, int a) {
  lua_assert(a <= MAXARG_Ax);
  return luaK_code(fs, CREATE_Ax(OP_EXTRAARG, 0, a));
}


int luaK_codek (FuncState *fs, int reg, int k) {
  if (k <= MAXARG_Bx) {
    reginfo_add_store(fs, reg);
    return luaK_codeABx(fs, OP_LOADK, 1, reg, k);
  }
  else {
    reginfo_add_store(fs, reg);
    int p = luaK_codeABx(fs, OP_LOADKX, 1, reg, 0);
    codeextraarg(fs, k);
    return p;
  }
}


void luaK_checkstack (FuncState *fs, int n) {
  int newstack = fs->freereg + n;
  if (newstack > fs->f->maxstacksize) {
    if (newstack >= MAXSTACK)
      luaX_syntaxerror(fs->ls, "function or expression too complex");
    fs->f->maxstacksize = cast_byte(newstack);
  }
}


void luaK_reserveregs (FuncState *fs, int n) {
  // printf("%s n=%i | freereg=%i nactvar=%i\n", __func__, n, fs->freereg, fs->nactvar);
  luaK_checkstack(fs, n);
  fs->freereg += n;
  // printf("\t\tfreereg=%i nactvar=%i\n", fs->freereg, fs->nactvar);
}


/* note: reg must not be a constant register */
static void freereg (FuncState *fs, int reg) {
  // printf("%s reg=%i | freereg=%i nactvar=%i\n", __func__, reg, fs->freereg, fs->nactvar);
  if (reg >= fs->nactvar) {
    fs->freereg--;
    lua_assert(reg == fs->freereg);
  }
  // printf("\t\tfreereg=%i nactvar=%i\n", fs->freereg, fs->nactvar);
}


static void freeexp (FuncState *fs, expdesc *e) {
  if (e->k == VNONRELOC) {    
    freereg(fs, e->u.info);
  }
}


static int addk (FuncState *fs, TValue *key, TValue *v) {
  lua_State *L = fs->ls->L;
  TValue *idx = luaH_set(L, fs->h, key);
  Proto *f = fs->f;
  int k, oldsize;
  if (ttisnumber(idx)) {
    lua_Number n = nvalue(idx);
    lua_number2int(k, n);
    if (luaV_rawequalobj(&f->k[k], v))
      return k;
    /* else may be a collision (e.g., between 0.0 and "\0\0\0\0\0\0\0\0");
       go through and create a new entry for this value */
  }
  /* constant not found; create a new entry */
  oldsize = f->sizek;
  k = fs->nk;
  /* numerical value does not need GC barrier;
     table has no metatable, so it does not need to invalidate cache */
  setnvalue(idx, cast_num(k));
  luaM_growvector(L, f->k, k, f->sizek, TValue, MAXARG_Ax, "constants");
  while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
  setobj(L, &f->k[k], v);
  fs->nk++;
  luaC_barrier(L, f, v);
  return k;
}


int luaK_stringK (FuncState *fs, TString *s) {
  TValue o;
  setsvalue(fs->ls->L, &o, s);
  return addk(fs, &o, &o);
}


int luaK_numberK (FuncState *fs, lua_Number r) {
  int n;
  lua_State *L = fs->ls->L;
  TValue o;
  setnvalue(&o, r);
  if (r == 0 || luai_numisnan(NULL, r)) {  /* handle -0 and NaN */
    /* use raw representation as key to avoid numeric problems */
    setsvalue(L, L->top, luaS_newlstr(L, (char *)&r, sizeof(r)));
     incr_top(L);
     n = addk(fs, L->top - 1, &o);
     L->top--;
  }
  else
    n = addk(fs, &o, &o);  /* regular case */
  return n;
}


static int boolK (FuncState *fs, int b) {
  TValue o;
  setbvalue(&o, b);
  return addk(fs, &o, &o);
}


static int nilK (FuncState *fs) {
  TValue k, v;
  setnilvalue(&v);
  /* cannot use nil as key; instead use table itself to represent nil */
  sethvalue(fs->ls->L, &k, fs->h);
  return addk(fs, &k, &v);
}


void luaK_setreturns (FuncState *fs, expdesc *e, int nresults) {  
  if (e->k == VCALL) {  /* expression is an open function call? */
    SETARG_C(getcode(fs, e), nresults+1);
  }
  else if (e->k == VVARARG) {
    SETARG_B(getcode(fs, e), nresults+1);
    SETARG_A(getcode(fs, e), fs->freereg);
    luaK_reserveregs(fs, 1);
  }
  else
    return;
  
  int ra = GETARG_A(getcode(fs, e));
  while (nresults-- > 0)
    reginfo_insert_store(fs, e->u.info, ra++);
}


void luaK_setoneret (FuncState *fs, expdesc *e) {
  if (e->k == VCALL) {  /* expression is an open function call? */
    e->k = VNONRELOC;
    e->u.info = GETARG_A(getcode(fs, e));
  }
  else if (e->k == VVARARG) {
    SETARG_B(getcode(fs, e), 2);
    reginfo_insert_store(fs, e->u.info, GETARG_A(getcode(fs, e)));
    e->k = VRELOCABLE;  /* can relocate its simple result */
  }
}


void luaK_dischargevars (FuncState *fs, expdesc *e) {
  switch (e->k) {
    case VLOCAL: {
      e->k = VNONRELOC;
      break;
    }
    case VUPVAL: {
      e->u.info = luaK_codeABC(fs, OP_GETUPVAL, 1, 0, e->u.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    case VINDEXED: {      
      OpCode op = OP_GETTABUP;  /* assume 't' is in an upvalue */      
      if (!e->u.ind.isk) freereg(fs, e->u.ind.idx);
      if (e->u.ind.vt == VLOCAL) {  /* 't' is in a register? */
        freereg(fs, e->u.ind.t);
        op = OP_GETTABLE;
        reginfo_add_load(fs, e->u.ind.t);
      }      
      
      int sp; /* specialization */
      if (e->u.ind.isk) {
        TValue *k = fs->f->k + e->u.ind.idx;
        if (ttisnumber(k)) {
          int ik;
          lua_number2int(ik, nvalue(k));
          if (luai_numeq(cast_num(ik), nvalue(k))) {
            sp = CREATE_OPSPEC_GETTAB(1, OPSPEC_TAB_KEY_int, OPSPEC_kst);
          } else {
            sp = CREATE_OPSPEC_GETTAB(1, OPSPEC_TAB_KEY_obj, OPSPEC_kst);
          }
        } else {
          lua_assert(ttisstring(k));
          sp = CREATE_OPSPEC_GETTAB(1, OPSPEC_TAB_KEY_str, OPSPEC_kst);
        }
      } else {
        reginfo_add_load(fs, e->u.ind.idx);
        sp = CREATE_OPSPEC_GETTAB(1, OPSPEC_TAB_KEY_chk, OPSPEC_reg);
      }
      
      e->u.info = luaK_codeABC(fs, op, sp, 0, e->u.ind.t, e->u.ind.idx);
      e->k = VRELOCABLE;
      break;
    }
    case VVARARG:
    case VCALL: {
      luaK_setoneret(fs, e);
      break;
    }
    default: break;  /* there is one value available (somewhere) */
  }
}


static int code_label (FuncState *fs, int A, int b, int jump) {
  luaK_getlabel(fs);  /* those instructions may be jump targets */
  return luaK_codeABC(fs, OP_LOADBOOL, 1, A, b, jump);
}


static void discharge2reg (FuncState *fs, expdesc *e, int reg) {
  // printf("%s %i", __func__, reg);
  // printf("\t[%i] nactvar=%i\n", fs->pc, fs->nactvar);
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VNIL: {
      luaK_nil(fs, reg, 1);
      break;
    }
    case VFALSE:  case VTRUE: {
      reginfo_add_store(fs, reg);
      luaK_codeABC(fs, OP_LOADBOOL, 1, reg, e->k == VTRUE, 0);
      break;
    }
    case VK: {
      luaK_codek(fs, reg, e->u.info);
      break;
    }
    case VKNUM: {
      luaK_codek(fs, reg, luaK_numberK(fs, e->u.nval));
      break;
    }
    case VRELOCABLE: {
      Instruction *pc = &getcode(fs, e);
      SETARG_A(*pc, reg);
      reginfo_insert_store(fs, e->u.info, reg);
      break;
    }
    case VNONRELOC: {
      if (reg != e->u.info) {
        reginfo_add_load(fs, e->u.info);
        reginfo_add_store(fs, reg);        
        luaK_codeABC(fs, OP_MOVE, 1, reg, e->u.info, 0);
      }
      break;
    }
    default: {
      lua_assert(e->k == VVOID || e->k == VJMP);
      return;  /* nothing to do... */
    }
  }
  e->u.info = reg;
  e->k = VNONRELOC;
}


static void discharge2anyreg (FuncState *fs, expdesc *e) {
  if (e->k != VNONRELOC) {
    luaK_reserveregs(fs, 1);
    discharge2reg(fs, e, fs->freereg-1);
  }
}


static void exp2reg (FuncState *fs, expdesc *e, int reg) {
  discharge2reg(fs, e, reg);
  if (e->k == VJMP)
    luaK_concat(fs, &e->t, e->u.info);  /* put this jump in `t' list */
  if (hasjumps(e)) {
    int final;  /* position after whole expression */
    int p_f = NO_JUMP;  /* position of an eventual LOAD false */
    int p_t = NO_JUMP;  /* position of an eventual LOAD true */
    if (need_value(fs, e->t) || need_value(fs, e->f)) {
      int fj = (e->k == VJMP) ? NO_JUMP : luaK_jump(fs);
      reginfo_add_store(fs, reg);
      p_f = code_label(fs, reg, 0, 1);
      reginfo_add_load(fs, reg); /* not really a load, but it extends the
                                    temp scope to the second LOADBOOL */
      p_t = code_label(fs, reg, 1, 0);
      luaK_patchtohere(fs, fj);
    }
    final = luaK_getlabel(fs);
    patchlistaux(fs, e->f, final, reg, p_f);
    patchlistaux(fs, e->t, final, reg, p_t);
  }
  e->f = e->t = NO_JUMP;
  e->u.info = reg;
  e->k = VNONRELOC;
}


void luaK_exp2nextreg (FuncState *fs, expdesc *e) { 
  luaK_dischargevars(fs, e);
  freeexp(fs, e);
  luaK_reserveregs(fs, 1);
  exp2reg(fs, e, fs->freereg - 1);
}


int luaK_exp2anyreg (FuncState *fs, expdesc *e) {  
  luaK_dischargevars(fs, e);
  if (e->k == VNONRELOC) {
    if (!hasjumps(e)) return e->u.info;  /* exp is already in a register */
    if (e->u.info >= fs->nactvar) {  /* reg. is not a local? */
      exp2reg(fs, e, e->u.info);  /* put value on it */
      return e->u.info;
    }    
  }  
  luaK_exp2nextreg(fs, e);  /* default */
  return e->u.info;
}


void luaK_exp2anyregup (FuncState *fs, expdesc *e) {
  if (e->k != VUPVAL || hasjumps(e))
    luaK_exp2anyreg(fs, e);
}


void luaK_exp2val (FuncState *fs, expdesc *e) {
  if (hasjumps(e))
    luaK_exp2anyreg(fs, e);
  else
    luaK_dischargevars(fs, e);
}


int luaK_exp2RK (FuncState *fs, expdesc *e) {
  luaK_exp2val(fs, e);
  switch (e->k) {
    case VTRUE:
    case VFALSE:
    case VNIL: {
      if (fs->nk <= MAXARG_B) {  /* constant fits in B operand? */
        e->u.info = (e->k == VNIL) ? nilK(fs) : boolK(fs, (e->k == VTRUE));
        e->k = VK;
        return e->u.info;
      }
      else break;
    }
    case VKNUM: {
      e->u.info = luaK_numberK(fs, e->u.nval);
      e->k = VK;
      /* go through */
    }
    case VK: {
      if (e->u.info <= MAXARG_C)  /* constant fits in argC? */
        return e->u.info;
      else break;
    }
    default: break;
  }
  /* not a constant in the right range: put it in a register */
  return luaK_exp2anyreg(fs, e);
}


void luaK_storevar (FuncState *fs, expdesc *var, expdesc *ex) {
  switch (var->k) {
    case VLOCAL: {
      // printf("%s VLOCAL\n", __func__);
      freeexp(fs, ex);
      exp2reg(fs, ex, var->u.info);
      return;
    }
    case VUPVAL: {
      int e = luaK_exp2anyreg(fs, ex);
      reginfo_add_load(fs, e);
      luaK_codeABC(fs, OP_SETUPVAL, 0, e, var->u.info, 0);
      break;
    }
    case VINDEXED: {      
      int e = luaK_exp2RK(fs, ex);

      OpCode op;
      int sp; /* specialization */
      int ck;
      if (isconst(ex)) {        
        ck = OPSPEC_kst;
      } else {
        reginfo_add_load(fs, e);
        ck = OPSPEC_reg;
      }
      if (var->u.ind.isk) {
        TValue *k = fs->f->k + var->u.ind.idx;
        if (ttisnumber(k)) {
          int ik;
          lua_number2int(ik, nvalue(k));
          if (luai_numeq(cast_num(ik), nvalue(k))) {
            sp = CREATE_OPSPEC_SETTAB(OPSPEC_TAB_KEY_int, OPSPEC_kst, ck);
          } else {
            sp = CREATE_OPSPEC_SETTAB(OPSPEC_TAB_KEY_obj, OPSPEC_kst, ck);
          }
        } else {
          lua_assert(ttisstring(k));
          sp = CREATE_OPSPEC_SETTAB(OPSPEC_TAB_KEY_str, OPSPEC_kst, ck);
        }
      } else {
        reginfo_add_load(fs, var->u.ind.idx);
        sp = CREATE_OPSPEC_SETTAB(OPSPEC_TAB_KEY_chk, OPSPEC_reg, ck);
      }      
      if (var->u.ind.vt == VLOCAL) {
        op = OP_SETTABLE;
        reginfo_add_load(fs, var->u.ind.t);
      } else {
        op = OP_SETTABUP;
      }      

      luaK_codeABC(fs, op, sp, var->u.ind.t, var->u.ind.idx, e);
      break;
    }
    default: {
      lua_assert(0);  /* invalid var kind to store */
      break;
    }
  }
  freeexp(fs, ex);
}


void luaK_self (FuncState *fs, expdesc *e, expdesc *key) {
  int ereg;
  luaK_exp2anyreg(fs, e);
  ereg = e->u.info;  /* register where 'e' was placed */
  freeexp(fs, e);
  e->u.info = fs->freereg;  /* base register for op_self */
  e->k = VNONRELOC;
  luaK_reserveregs(fs, 2);  /* function and 'self' produced by op_self */
  int rkkey = luaK_exp2RK(fs, key);
  int sp = (isconst(key)<<4)|1;
  reginfo_add_load(fs, ereg);
  if (!isconst(key)) reginfo_add_load(fs, rkkey);
  reginfo_add_store(fs, e->u.info);
  reginfo_add_store(fs, e->u.info+1);  
  luaK_codeABC(fs, OP_SELF, sp, e->u.info, ereg, rkkey);
  freeexp(fs, key);
}


static void invertjump (FuncState *fs, expdesc *e) {
  Instruction *pc = getjumpcontrol(fs, e->u.info);  
  lua_assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
                                           GET_OPCODE(*pc) != OP_TEST);
  SETARG_A(*pc, !(GETARG_A(*pc)));
}


static int jumponcond (FuncState *fs, expdesc *e, int cond) {
  if (e->k == VRELOCABLE) {
    Instruction ie = getcode(fs, e);
    if (GET_OPCODE(ie) == OP_NOT) {
      fs->pc--;  /* remove previous OP_NOT */
      // reginfo for OP_NOT is still valid!
      return condjump(fs, OP_TEST, 0, GETARG_B(ie), 0, !cond);
    }
    /* else go through */
  }
  discharge2anyreg(fs, e);
  freeexp(fs, e);
  reginfo_add_load(fs, e->u.info);
  return condjump(fs, OP_TESTSET, 1, NO_REG, e->u.info, cond);
}


void luaK_goiftrue (FuncState *fs, expdesc *e) {
  int pc;  /* pc of last jump */
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VJMP: {
      invertjump(fs, e);
      pc = e->u.info;
      break;
    }
    case VK: case VKNUM: case VTRUE: {
      pc = NO_JUMP;  /* always true; do nothing */
      break;
    }
    default: {
      pc = jumponcond(fs, e, 0);
      break;
    }
  }
  luaK_concat(fs, &e->f, pc);  /* insert last jump in `f' list */
  luaK_patchtohere(fs, e->t);
  e->t = NO_JUMP;
}


void luaK_goiffalse (FuncState *fs, expdesc *e) {
  int pc;  /* pc of last jump */
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VJMP: {
      pc = e->u.info;
      break;
    }
    case VNIL: case VFALSE: {
      pc = NO_JUMP;  /* always false; do nothing */
      break;
    }
    default: {
      pc = jumponcond(fs, e, 1);
      break;
    }
  }
  luaK_concat(fs, &e->t, pc);  /* insert last jump in `t' list */
  luaK_patchtohere(fs, e->f);
  e->f = NO_JUMP;
}


static void codenot (FuncState *fs, expdesc *e) {
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VNIL: case VFALSE: {
      e->k = VTRUE;
      break;
    }
    case VK: case VKNUM: case VTRUE: {
      e->k = VFALSE;
      break;
    }
    case VJMP: {
      invertjump(fs, e);
      break;
    }
    case VRELOCABLE:
    case VNONRELOC: {
      discharge2anyreg(fs, e);
      freeexp(fs, e);
      reginfo_add_load(fs, e->u.info);
      e->u.info = luaK_codeABC(fs, OP_NOT, 1, 0, e->u.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    default: {
      lua_assert(0);  /* cannot happen */
      break;
    }
  }
  /* interchange true and false lists */
  { int temp = e->f; e->f = e->t; e->t = temp; }
  removevalues(fs, e->f);
  removevalues(fs, e->t);
}


void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k) {
  lua_assert(!hasjumps(t));
  t->u.ind.t = t->u.info;
  t->u.ind.idx = luaK_exp2RK(fs, k);
  t->u.ind.isk = (k->k == VK);
  t->u.ind.vt = (t->k == VUPVAL) ? VUPVAL
                                 : check_exp(vkisinreg(t->k), VLOCAL);
  t->k = VINDEXED;
}


static int constfolding (BinOpr opr, expdesc *e1, expdesc *e2) {
  if (!isnumeral(e1) || !isnumeral(e2)) return 0;
  if ((opr == OPR_DIV || opr == OPR_MOD) && e2->u.nval == 0)
    return 0;  /* do not attempt to divide by 0 */
  if (opr > OPR_POW) return 0;
  e1->u.nval = luaO_arith(opr, e1->u.nval, e2->u.nval);
  return 1;
}


static void codearith (FuncState *fs, BinOpr opr,
                       expdesc *e1, expdesc *e2, int line) {
  if (constfolding(opr, e1, e2))
    return;
  else {
    int o2 = luaK_exp2RK(fs, e2);
    int o1 = luaK_exp2RK(fs, e1);
    if (o1 > o2) {
      freeexp(fs, e1);
      freeexp(fs, e2);
    }
    else {
      freeexp(fs, e2);
      freeexp(fs, e1);
    }

    OpCode op = OP_ADD + opr; /* ORDER OP */

    int sp;
    if (isconst(e1) && isconst(e2)) {
      sp = CREATE_OPSPEC_ARITH(OPSPEC_OUT_chk, OPSPEC_ARITH_IN_raw,
                               OPSPEC_kst, OPSPEC_kst);
    } else {
      sp = CREATE_OPSPEC_ARITH(OPSPEC_OUT_chk, OPSPEC_ARITH_IN_chk,
                               isconst(e1) ? OPSPEC_kst : OPSPEC_reg,
                               isconst(e2) ? OPSPEC_kst : OPSPEC_reg);
      if (!isconst(e1)) reginfo_add_load(fs, o1);
      if (!isconst(e2)) reginfo_add_load(fs, o2);
    }

    e1->u.info = luaK_codeABC(fs, op, sp, 0, o1, o2);
    e1->k = VRELOCABLE;
    luaK_fixline(fs, line);
  }
}


static void codeunm (FuncState *fs, expdesc *e, int line) {
  if (isnumeral(e)) /* minus constant? */
    e->u.nval = luai_numunm(NULL, e->u.nval); /* fold it */
  else {
    luaK_exp2anyreg(fs, e);
    int o = luaK_exp2RK(fs, e);
    freeexp(fs, e);
    int sp = (OPSPEC_ARITH_IN_chk<<1 | OPSPEC_OUT_chk);
    reginfo_add_load(fs, o);
    e->u.info = luaK_codeABC(fs, OP_UNM, sp, 0, o, 0);
    e->k = VRELOCABLE;
    luaK_fixline(fs, line);
  }
}


static void codelen (FuncState *fs, expdesc *e, int line) {
  luaK_exp2anyreg(fs, e); /* cannot operate on constants */
  int o = luaK_exp2RK(fs, e);
  freeexp(fs, e);
  int sp = (OPSPEC_ARITH_IN_chk<<1 | OPSPEC_OUT_chk);
  reginfo_add_load(fs, o);
  e->u.info = luaK_codeABC(fs, OP_LEN, sp, 0, o, 0);
  e->k = VRELOCABLE;
  luaK_fixline(fs, line);
}


static void codeconcat (FuncState *fs, expdesc *e1, expdesc *e2, int line) {
  luaK_exp2val(fs, e2);
  if (e2->k == VRELOCABLE && GET_OPCODE(getcode(fs, e2)) == OP_CONCAT) {
    lua_assert(e1->u.info == GETARG_B(getcode(fs, e2))-1);
    freeexp(fs, e1);
    SETARG_B(getcode(fs, e2), e1->u.info);
    e1->k = VRELOCABLE; e1->u.info = e2->u.info;
  }
  else {
    luaK_exp2nextreg(fs, e2);  /* operand must be on the 'stack' */
    int o2 = luaK_exp2RK(fs, e2);
    int o1 = luaK_exp2RK(fs, e1);
    if (o1 > o2) {
      freeexp(fs, e1);
      freeexp(fs, e2);
    }
    else {
      freeexp(fs, e2);
      freeexp(fs, e1);
    }
    int r;
    for (r = o1; r <= o2; r++) reginfo_add_load(fs, r);
    e1->u.info = luaK_codeABC(fs, OP_CONCAT, 1, 0, o1, o2);
    e1->k = VRELOCABLE;
    luaK_fixline(fs, line);
  }
}


static void codeeq (FuncState *fs, int cond, expdesc *e1, expdesc *e2) {
  int o1 = luaK_exp2RK(fs, e1);
  int o2 = luaK_exp2RK(fs, e2);
  freeexp(fs, e2);
  freeexp(fs, e1);

  int bk = isconst(e1) ? OPSPEC_kst : OPSPEC_reg;
  int ck = isconst(e2) ? OPSPEC_kst : OPSPEC_reg;
  int sp = (ck<<4 | bk<<3);

  if (bk == OPSPEC_reg) reginfo_add_load(fs, o1);
  if (ck == OPSPEC_reg) reginfo_add_load(fs, o2);
  e1->u.info = condjump(fs, OP_EQ, sp, cond, o1, o2);
  e1->k = VJMP;
}


static void codecomp (FuncState *fs, BinOpr opr, expdesc *e1, expdesc *e2) {
  int o1 = luaK_exp2RK(fs, e1);
  int o2 = luaK_exp2RK(fs, e2);
  freeexp(fs, e2);
  freeexp(fs, e1);

  int const1, const2;
  if (opr == OPR_GT || opr == OPR_GE) {
    /* exchange args to replace by '<' or '<=' */
    int tmp = o1; o1 = o2; o2 = tmp;
    const1 = isconst(e2);
    const2 = isconst(e1);
  } else {
    const1 = isconst(e1);
    const2 = isconst(e2);
  }
  
  OpCode op = (opr == OPR_GT || opr == OPR_LT) ? OP_LT : OP_LE;
  
  int sp;
  if (const1 && const2) {
    TValue *k1 = fs->f->k + o1;
    TValue *k2 = fs->f->k + o2;
    if (ttisequal(k1, k2)) {
      if (ttisnumber(k1)) {
        sp = CREATE_OPSPEC_LESS(OPSPEC_LESS_TYPE_num, OPSPEC_kst, OPSPEC_kst);
      } else {
        sp = CREATE_OPSPEC_LESS(OPSPEC_LESS_TYPE_str, OPSPEC_kst, OPSPEC_kst);
      }
    } else {
      sp = CREATE_OPSPEC_LESS(OPSPEC_LESS_TYPE_raw, OPSPEC_kst, OPSPEC_kst);
    }
  } else {
    sp = CREATE_OPSPEC_LESS(OPSPEC_LESS_TYPE_chk, 
                            const1 ? OPSPEC_kst : OPSPEC_reg, 
                            const2 ? OPSPEC_kst : OPSPEC_reg);
    if (!const1) reginfo_add_load(fs, o1);
    if (!const2) reginfo_add_load(fs, o2);
  }

  e1->u.info = condjump(fs, op, sp, 1, o1, o2);
  e1->k = VJMP;
}


void luaK_prefix (FuncState *fs, UnOpr op, expdesc *e, int line) {
  switch (op) {
    case OPR_MINUS: codeunm(fs, e, line); break;
    case OPR_NOT: codenot(fs, e); break;
    case OPR_LEN: codelen(fs, e, line); break;
    default: lua_assert(0);
  }
}


void luaK_infix (FuncState *fs, BinOpr op, expdesc *v) {
  switch (op) {
    case OPR_AND: {
      luaK_goiftrue(fs, v);
      break;
    }
    case OPR_OR: {
      luaK_goiffalse(fs, v);
      break;
    }
    case OPR_CONCAT: {
      luaK_exp2nextreg(fs, v);  /* operand must be on the `stack' */
      break;
    }
    case OPR_ADD: case OPR_SUB: case OPR_MUL: case OPR_DIV:
    case OPR_MOD: case OPR_POW: {
      if (!isnumeral(v)) luaK_exp2RK(fs, v);
      break;
    }
    default: {
      luaK_exp2RK(fs, v);
      break;
    }
  }
}


void luaK_posfix (FuncState *fs, BinOpr op,
                  expdesc *e1, expdesc *e2, int line) {
  switch (op) {
    case OPR_AND: {
      lua_assert(e1->t == NO_JUMP);  /* list must be closed */
      luaK_dischargevars(fs, e2);
      luaK_concat(fs, &e2->f, e1->f);
      *e1 = *e2;
      break;
    }
    case OPR_OR: {
      lua_assert(e1->f == NO_JUMP);  /* list must be closed */
      luaK_dischargevars(fs, e2);
      luaK_concat(fs, &e2->t, e1->t);
      *e1 = *e2;
      break;
    }
    case OPR_CONCAT: codeconcat(fs, e1, e2, line); break;
    case OPR_ADD: case OPR_SUB: case OPR_MUL: case OPR_DIV:
    case OPR_MOD: case OPR_POW: {
      codearith(fs, op, e1, e2, line);
      break;
    }
    case OPR_EQ: codeeq(fs, 1, e1, e2); break;
    case OPR_NE: codeeq(fs, 0, e1, e2); break;
    case OPR_LT: case OPR_LE: case OPR_GT: case OPR_GE: {
      codecomp(fs, op, e1, e2);
      break;
    }
    default: lua_assert(0);
  }
}


void luaK_fixline (FuncState *fs, int line) {
  fs->f->lineinfo[fs->pc - 1] = line;
}


void luaK_setlist (FuncState *fs, int base, int nelems, int tostore) {
  int c =  (nelems - 1)/LFIELDS_PER_FLUSH + 1;
  int b = (tostore == LUA_MULTRET) ? 0 : tostore;
  lua_assert(tostore != 0);
  if (c <= MAXARG_C)
    luaK_codeABC(fs, OP_SETLIST, 0, base, b, c);
  else if (c <= MAXARG_Ax) {
    luaK_codeABC(fs, OP_SETLIST, 0, base, b, 0);
    codeextraarg(fs, c);
  }
  else
    luaX_syntaxerror(fs->ls, "constructor too long");
  fs->freereg = base + 1;  /* free registers with list values */
}

