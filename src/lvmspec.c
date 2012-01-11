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


// TODO:
// forward-specialization won't work since the second argument of an op
// may not yet have been assigned so it won't have a type so we'll spec it
// to raw and after a while the reg gets polymorphic needlessly


#define RA(i) (base+GETARG_A(i))
#define RB(i) (base+GETARG_B(i))
#define RC(i) (base+GETARG_C(i))
#define KB(i) (k+GETARG_B(i))
#define KC(i) (k+GETARG_C(i))

#define b_is_const(i) (GET_OPSPEC_BK(i) == OPSPEC_kst)
#define c_is_const(i) (GET_OPSPEC_CK(i) == OPSPEC_kst)

RegInfo *getreginfo(Proto *f, int pc, int reg) {
  lua_assert(reg < f->sizereginfos);
  RegInfo *reginfo = &(f->reginfos[reg]);
  if (reginfo->state != REGINFO_STATE_UNUSED) {
    while (reginfo) {
      if (reginfo->startpc <= pc && reginfo->endpc >= pc)
        return reginfo;
      reginfo = reginfo->next;
    }
  }
  return NULL;
}

int is_polymorphic(Proto *f, int pc, int reg) {
  RegInfo *reginfo = getreginfo(f, pc, reg);
  return reginfo->nspec > 2;
}

void luaVS_specialize(lua_State *L, int reg) {
  Proto *p = clLvalue(L->ci->func)->p;
  int pc = L->ci->u.l.savedpc - p->code - 1;
  printf("%s [%i] %i", __func__, pc, reg);
  RegInfo *reginfo = getreginfo(p, pc, reg);  
  if (!reginfo) {
    printf(" NULL\n");
    return;
  }
  printf(" (%i,%i)", reginfo->startpc, reginfo->endpc);
  if (reginfo->state == REGINFO_STATE_LOCAL_CLOSED) printf(" local");  
  lua_assert(reginfo->state != REGINFO_STATE_LOCAL_OPEN);
  lua_assert(reginfo->state != REGINFO_STATE_UNUSED);
  printf(" nspec=%i", reginfo->nspec);

  int reg_is_polymorphic;
  if (reginfo->nspec > 2) {
    printf(" CANCELLED\n");
    return;
  } else if (reginfo->nspec == 2) {
    printf(" POLYMORPHIC, specialize to raw\n");
    reg_is_polymorphic = 1;    
  } else {
    printf("\n");
    reg_is_polymorphic = 0;
  }
  reginfo->nspec++;  

  StkId base = L->ci->u.l.base;
  TValue *k = p->k;
  for (pc = reginfo->startpc; pc <= reginfo->endpc; pc++) {
    Instruction *i = &(p->code[pc]);
    switch (GET_OPCODE(*i)) {
/* ------------------------------------------------------------------------ */
      case OP_MOVE: { 
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC(*i, 0); /* Ra <- Rb */
          } else if (ttisequal(RA(*i), RB(*i))) {
            SET_OPSPEC(*i, 2); /* Ra:x <- Rb:x */
          } else {
            SET_OPSPEC(*i, 1); /* Ra:? <- Rb */
          }
        }
        if (reg == GETARG_B(*i)) {
          if (reg_is_polymorphic) {
            if (is_polymorphic(p, pc, GETARG_A(*i))) {
              SET_OPSPEC(*i, 0); /* Ra <- Rb */
            } else {
              SET_OPSPEC(*i, 1); /* Ra:? <- Rb */
            }
          } else if (ttisequal(RA(*i), RB(*i))) {
              SET_OPSPEC(*i, 2); /* Ra:x <- Rb:x */
          } else {
            SET_OPSPEC(*i, 1); /* Ra:? <- Rb */
          }
        }
        break;
      }     
/* ------------------------------------------------------------------------ */
      case OP_LOADK: {
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC(*i, 0); /* Ra <- Kb */
          } else if (ttisequal(RA(*i), KB(*i))) {
            SET_OPSPEC(*i, 2); /* Ra:x <- Kb:x */
          } else {
            SET_OPSPEC(*i, 1); /* Ra:? <- Kb */
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_LOADKX:      
      case OP_GETUPVAL:
      case OP_NOT:
      case OP_CONCAT:
      case OP_SELF:
      case OP_TESTSET:
      case OP_CLOSURE: {
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          } else {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
          }
        }
      }
/* ------------------------------------------------------------------------ */
      case OP_LOADBOOL: {
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC(*i, 0); /* Ra <- Ib:bool */
          } else if (ttisboolean(RA(*i))) {
            SET_OPSPEC(*i, 2); /* Ra:bool <- Ib:bool */
          } else {
            SET_OPSPEC(*i, 1); /* Ra:? <- Ib:bool */
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_LOADNIL: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i);
        if (reg >= a && reg <= a+b) {
          int flag = 0;
          do {
            if (!is_polymorphic(p, pc, a) || !ttisnil(base+a)) {
              flag = 1;
              break;
            }
            a++;
          } while (b--);
          if (flag)
            SET_OPSPEC(*i, 1); /* R(a...a+b):? <- nil */
          else
            SET_OPSPEC(*i, 0); /* R(a...a+b) <- nil */
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_GETTABUP:
      case OP_GETTABLE: {
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);            
          } else {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
          }
        }
        if (!c_is_const(*i) && reg == GETARG_C(*i)) {
          TValue *rc = RC(*i);
          if (reg_is_polymorphic) {
            SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_raw);            
          } else if (ttisstring(rc)) {
            SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_str);
          } else if (ttisnumber(rc)) {
            int k;
            lua_Number n = nvalue(rc);
            lua_number2int(k, n);
            if (luai_numeq(cast_num(k), nvalue(rc))) {
              SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_int);
            } else {
              SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
            }
          } else {
            SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_SETTABUP:
      case OP_SETTABLE: {        
        if (!b_is_const(*i) && reg == GETARG_B(*i)) {
          TValue *rb = RB(*i);
          if (reg_is_polymorphic) {
            SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_raw);            
          } else if (ttisstring(rb)) {
            SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_str);
          } else if (ttisnumber(rb)) {
            int k;
            lua_Number n = nvalue(rb);
            lua_number2int(k, n);
            if (luai_numeq(cast_num(k), nvalue(rb))) {
              SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_int);
            } else {
              SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
            }
          } else {
            SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
          }          
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_NEWTABLE: {
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic || ttistable(RA(*i))) {
            SET_OPSPEC(*i, 0); /* Ra <- {} */
          } else {
            SET_OPSPEC(*i, 1); /* Ra:? <- {} */
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_ADD:
      case OP_SUB:
      case OP_MUL:
      case OP_DIV:
      case OP_MOD:
      case OP_POW: {
        if (reg == GETARG_A(*i)) {          
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);            
          } else {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
          }
        }
        if ((!b_is_const(*i) && reg == GETARG_B(*i)) || 
            (!c_is_const(*i) && reg == GETARG_C(*i))) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          } else {
            int bn = b_is_const(*i) ? ttisnumber(KB(*i)) : ttisnumber(RB(*i));
            int cn = c_is_const(*i) ? ttisnumber(KC(*i)) : ttisnumber(RC(*i));
            if (bn && cn) {
              SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_num);              
            } else {
              SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_obj);
            }
          }
        }
        break;
      }      
/* ------------------------------------------------------------------------ */
      case OP_UNM: {
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          } else {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
          }
        }
        if (reg == GETARG_B(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          } else if (ttisnumber(RB(*i))) {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_num);
          } else {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_LEN: {
        if (reg == GETARG_A(*i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          } else {
            SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
          }
        }
        if (reg == GETARG_B(*i)) {
          TValue *rb = RB(*i);
          if (reg_is_polymorphic) {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          } else if (ttisstring(rb)) {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_str);
          } else if (ttistable(rb)) {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_tab);
          } else {
            SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_LT:
      case OP_LE: {
        TValue *rb = b_is_const(*i) ? KB(*i) : RB(*i);
        TValue *rc = c_is_const(*i) ? KC(*i) : RC(*i);
        if ((!b_is_const(*i) && reg == GETARG_B(*i)) ||
            (!c_is_const(*i) && reg == GETARG_C(*i))) {
          if (reg_is_polymorphic) {
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
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_VARARG: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i) - 1;
        int j;
        int n = cast_int(base - L->ci->func) - p->numparams - 1;
        if (b < 0) b = n;
        if (reg >= a && reg <= a+b) {
          int flag = 0;
          for (j = 0; j < b; j++) {
            if (!is_polymorphic(p, pc, a+b)) {
              flag = 1;
            }
          }
          if (flag) {
            SET_OPSPEC(*i, 1); /* R(a...a+b-2):? <- ... */
          } else {
            SET_OPSPEC(*i, 0); /* R(a...a+b-2) <- ... */
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      default:
        break;
    }
  }
}