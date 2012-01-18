/*
** Lua virtual machine bytecode specialization
*/

#define lvmspec_c
#define LUA_CORE

#include <stdio.h>
#include "lua.h"

#include "lopcodes.h"
#include "lstate.h"
#include "lvmspec.h"



#define DEBUG_PRINT



#define use_possible(reginfo, pc, use) \
        ((reginfo->startpc < pc && reginfo->endpc > pc) || \
         (reginfo->startpc == pc && reginfo->firstuse == use) || \
         (reginfo->endpc == pc && reginfo->lastuse == use))

static RegInfo *findreginfo (Proto *p, int reg, int pc, int use) {
  lua_assert(reg < p->sizereginfos);
  RegInfo *reginfo = &(p->reginfos[reg]);
  while (reginfo) {
    if (reginfo->state == REGINFO_STATE_UNUSED) return NULL;
    if (reginfo->state == REGINFO_STATE_LOCAL_UNUSED) return NULL;    
    if (use_possible(reginfo, pc, use)) break;
    reginfo = reginfo->next;
  }
  return reginfo;
}

// TODO: hoist to llimits.h?
#define MAX_SPEC 2

#define A (GETARG_A(*i))
#define B (GETARG_B(*i))
#define C (GETARG_C(*i))

#define RA (base+GETARG_A(*i))
#define RB (base+GETARG_B(*i))
#define RC (base+GETARG_C(*i))

#define KB (k+GETARG_B(*i))
#define KC (k+GETARG_C(*i))

#define isconst_B (GET_OPSPEC_BK(*i) == OPSPEC_kst)
#define isconst_C (GET_OPSPEC_CK(*i) == OPSPEC_kst)

#define ispoly(reg, use) (findreginfo(p, reg, pc, use)->nspec > MAX_SPEC)

#define store_possible use_possible(reginfo, pc, REGINFO_USE_STORE)
#define load_possible  use_possible(reginfo, pc, REGINFO_USE_LOAD)

static int ttisint (TValue *v) {
  if (!ttisnumber(v)) return 0;
  int k;
  lua_Number n = nvalue(v);  
  lua_number2int(k, n);
  return luai_numeq(cast_num(k), n);
}

#define spdispatch(o) switch(o)
#define spcase(o,b) case o: {b}  break;
#define spcasem(o) case o:    /* m = multiple */

static void _luaVS_specialize (lua_State *L, int reg, RegInfo *reginfo) {
  lua_assert(reginfo->state == REGINFO_STATE_TEMP ||
             reginfo->state == REGINFO_STATE_LOCAL_CLOSED);
#ifdef DEBUG_PRINT
  if (reginfo->nspec > MAX_SPEC) { printf("> %i\n", MAX_SPEC); return; }
#else
  if (reginfo->nspec > MAX_SPEC) return;
#endif
  int ispoly = (reginfo->nspec == MAX_SPEC);
  reginfo->nspec++;

  Proto *p = clLvalue(L->ci->func)->p;
  StkId base = L->ci->u.l.base;
  TValue *k = p->k;

#ifdef DEBUG_PRINT
  printf("%s %i (%i,%i) %s%s %i%s\n", __func__, reg, reginfo->startpc, reginfo->endpc, reginfo->firstuse ? "S" : "L", reginfo->lastuse ? "S" : "L", reginfo->nspec, ispoly ? " (poly)" : "");
#endif

  int pc;
  for (pc = reginfo->startpc; pc <= reginfo->endpc; pc++) {
    Instruction *i = &(p->code[pc]);
    
#ifdef DEBUG_PRINT    
    printf("\t[%i] ", pc);
    PrintOp(*i);
#endif
    
    switch (GET_OPCODE(*i)) {
/* ------------------------------------------------------------------------ */
      spcase(OP_MOVE,
        if (A == reg && store_possible) {
          if (ispoly)                 SET_OPSPEC(*i, 0); /* Ra   <- Rb   */
          else if (ttisequal(RA, RB)) SET_OPSPEC(*i, 2); /* Ra:x <- Rb:x */
          else                        SET_OPSPEC(*i, 1); /* Ra:? <- Rb   */
        }
        if (B == reg && load_possible) {
          if (ispoly) {
            if (ispoly(A, REGINFO_USE_STORE)) 
              SET_OPSPEC(*i, 0); /* Ra   <- Rb */
            else
              SET_OPSPEC(*i, 1); /* Ra:? <- Rb */
          }
          else if (ttisequal(RA, RB)) SET_OPSPEC(*i, 2); /* Ra:x <- Ra:x */
          else                        SET_OPSPEC(*i, 1); /* Ra:? <- Rb   */
        }
      )
/* ------------------------------------------------------------------------ */
      spcase(OP_LOADK,
        if (A == reg && store_possible) {
          if (ispoly)                 SET_OPSPEC(*i, 0); /* Ra   <- Kb   */
          else if (ttisequal(RA, KB)) SET_OPSPEC(*i, 2); /* Ra:x <- Kb:x */
          else                        SET_OPSPEC(*i, 1); /* Ra:? <- Kb   */
        }
      )
/* ------------------------------------------------------------------------ */
      spcasem(OP_LOADKX)
      spcasem(OP_GETUPVAL)
      spcasem(OP_NOT)
      spcasem(OP_CONCAT)
      spcasem(OP_SELF)
      spcasem(OP_TESTSET)
      spcase(OP_CLOSURE,
        if (A == reg && store_possible) {
          if (ispoly) SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          else        SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
        }
      )
/* ------------------------------------------------------------------------ */
      spcase(OP_LOADBOOL,
        if (A == reg && store_possible) {
          if (ispoly)               SET_OPSPEC(*i, 0); /* Ra   <- Ib:b */
          else if (ttisboolean(RA)) SET_OPSPEC(*i, 2); /* Ra:b <- Ib:b */
          else                      SET_OPSPEC(*i, 1); /* Ra:? <- Ib:b */
        }
      )
/* ------------------------------------------------------------------------ */
      spcase(OP_LOADNIL,
        int a = A;
        int b = B;
        if (a <= reg && a+b >= reg && store_possible) {
          int allpoly = 1;
          do {
            if (!ispoly(a, REGINFO_USE_STORE) || !ttisnil(base+a)) {
              allpoly = 0;
              break;
            }
            a++;
          } while (b--);
          if (allpoly) SET_OPSPEC(*i, 1); /* R(a...a+b)   <- nil */
          else         SET_OPSPEC(*i, 1); /* R(a...a+b):? <- nil */
        }
      )
/* ------------------------------------------------------------------------ */
      spcasem(OP_GETTABUP)
      spcase(OP_GETTABLE,      
        if (A == reg && store_possible) {
          if (ispoly) SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);            
          else        SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
        }
        if (!isconst_C && C == reg && load_possible) {
          TValue *rc = RC;
          if (ispoly) 
            SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_raw);
          else if (ttisstring(rc)) 
            SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_str);
          else if (ttisint(rc)) 
            SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_int);
          else
            SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
        }
      )
/* ------------------------------------------------------------------------ */
      spcasem(OP_SETTABUP)
      spcase(OP_SETTABLE,
        if (!isconst_B && B == reg && load_possible) {
          TValue *rb = RB;
          if (ispoly)
            SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_raw);            
          else if (ttisstring(rb))
            SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_str);
          else if (ttisint(rb))
            SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_int);
          else
            SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
        }
      )
/* ------------------------------------------------------------------------ */
      spcase(OP_NEWTABLE,
        if (A == reg && store_possible) {
          if (ispoly || ttistable(RA)) SET_OPSPEC(*i, 0); /* Ra   <- {} */
          else                         SET_OPSPEC(*i, 1); /* Ra:? <- {} */
        }
      )
/* ------------------------------------------------------------------------ */
      spcasem(OP_ADD)
      spcasem(OP_SUB)
      spcasem(OP_MUL)
      spcasem(OP_DIV)
      spcasem(OP_MOD)
      spcase(OP_POW,
        if (A == reg && store_possible) {
          if (ispoly) SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);            
          else        SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
        }
        if (((!isconst_B && B == reg) || 
             (!isconst_C && C == reg)) &&
            load_possible) {
          if (ispoly) {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          } else {
            int bn = isconst_B ? ttisnumber(KB) : ttisnumber(RB);
            int cn = isconst_C ? ttisnumber(KC) : ttisnumber(RC);
            if (bn && cn) {
              SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_num);              
            } else {
              SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_obj);
            }
          }
        }
      )
/* ------------------------------------------------------------------------ */
      spcase(OP_UNM,
        if (A == reg && store_possible) {
          if (ispoly) SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          else        SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
        }
        if (B == reg && load_possible) {
          if (ispoly) 
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          else if (ttisnumber(RB)) 
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_num);
          else
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        }
      )
/* ------------------------------------------------------------------------ */
      spcase(OP_LEN,
        if (A == reg && store_possible) {
          if (ispoly) SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          else        SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
        }
        if (B == reg && load_possible) {
          TValue *rb = RB;
          if (ispoly)
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          else if (ttisstring(rb)) 
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_str);
          else if (ttistable(rb))
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_tab);
          else
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        }
      )
/* ------------------------------------------------------------------------ */
      spcasem(OP_LT)
      spcase(OP_LE,
        TValue *rb = isconst_B ? KB : RB;
        TValue *rc = isconst_C ? KC : RC;
        if (((!isconst_B && B == reg) ||
             (!isconst_C && C == reg)) &&
            load_possible) {
          if (ispoly) {
            SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_raw);
          } else if (ttisequal(rb, rc)) {
            if (ttisnumber(rb)) {
              SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_num);
            } else if (ttisstring(rb)) {
              SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_str);
            } else {
              SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_raw);
            }
          } else {
            SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_raw);
          }
        }
      )
/* ------------------------------------------------------------------------ */
      spcase(OP_VARARG,
        int a = A;
        int b = B - 1;
        int j;
        int n = cast_int(base - L->ci->func) - p->numparams - 1;
        if (b < 0) b = n;
        if (a <= reg && a+b >= reg && store_possible) {
          int allpoly = 1;
          for (j = 0; j < b; j++) {
            if (!ispoly(a+j, REGINFO_USE_STORE)) {
              allpoly = 0;
              break;
            }
          }
          if (allpoly) SET_OPSPEC(*i, 1); /* R(a...a+b-2)   <- ... */
          else         SET_OPSPEC(*i, 1); /* R(a...a+b-2):? <- ... */
        }
      )
/* ------------------------------------------------------------------------ */
      default:
      #ifdef DEBUG_PRINT
        printf("\n");
        continue;
      #endif
        break;
    }
#ifdef DEBUG_PRINT
    printf(" --> ");
    PrintOp(*i);
    printf("\n");
#endif
  }
}

  }

void luaVS_specialize (lua_State *L, int reg, int use) {
  #ifdef DEBUG_PRINT
  printf("%s %i %i ", __func__, reg, use);
  #endif
  Proto *p = clLvalue(L->ci->func)->p;
  int pc = L->ci->u.l.savedpc - p->code - 1;
  RegInfo *reginfo = findreginfo(p, reg, pc, use);
  if (reginfo) _luaVS_specialize(L, reg, reginfo);
  #ifdef DEBUG_PRINT
  else printf("\n");
  #endif
}
