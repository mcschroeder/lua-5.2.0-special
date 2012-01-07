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


#define RA(i) (base+GETARG_A(i))
#define RB(i) (base+GETARG_B(i))
#define RC(i) (base+GETARG_C(i))
#define KB(i) (k+GETARG_B(i))
#define KC(i) (k+GETARG_C(i))

#define c_is_const(i) (GET_OPSPEC_BK(i) == OPSPEC_kst)
#define b_is_const(i) (GET_OPSPEC_CK(i) == OPSPEC_kst)

int is_polymorphic(lua_State *L, int reg) {
  Proto *p = clLvalue(L->ci->func)->p;
  int pc = L->ci->u.l.savedpc - p->code - 1;
  int j, local_number = reg-1;
  for (j = 0; j < p->sizelocvars && p->locvars[j].startpc <= pc; j++) {
    if (pc < p->locvars[j].endpc) {
      local_number--;
      if (local_number == 0)
        return p->locvars[j].speccount == 2;        
    }
  }    
  return 1;
}

void luaVS_specialize(lua_State *L, int reg) {

  // NEW APPROACH: we don't emit chk specs for non-locals during codegen
  // now all we have to do here is make sure we don't respecialize a raw
  // register into a chk one...
  /* invariant: reg must be a local variable! */
  
  Proto *p = clLvalue(L->ci->func)->p;
  int pc = L->ci->u.l.savedpc - p->code - 1;
  int j, local_number = reg-1;
  for (j = 0; j < p->sizelocvars && p->locvars[j].startpc <= pc; j++) {
    if (pc < p->locvars[j].endpc) {
      local_number--;
      if (local_number == 0)
        break;
    }
  }

  printf("specialize %i at [%i]", reg, pc);

  int reg_is_polymorphic, first_occurence, last_occurence;  
  if (local_number == 0) {
    first_occurence = p->locvars[j].startpc;
    last_occurence = p->locvars[j].endpc;

    if (p->locvars[j].speccount > 2) {
      printf(" ... speccount > 2, cancelled\n");
      return;
    }
    else if (p->locvars[j].speccount == 2) {
      printf(" ... speccount == 2, specialize to raw");
      reg_is_polymorphic = 1;
    }
    else {
      p->locvars[j].speccount++;
      reg_is_polymorphic = 0;
    }      
  } else {
    printf(" ... temp, specialize to raw");
   
    reg_is_polymorphic = 1;
    first_occurence = 0;
    last_occurence = p->sizecode-1;
  }

  printf(" (range: %i-%i)\n", first_occurence, last_occurence);

  StkId base = L->ci->u.l.base;
  TValue *k = p->k;
  for (pc = first_occurence; pc <= last_occurence; pc++) {
    Instruction i = p->code[pc];
    switch (GET_OPCODE(i)) {
/* ------------------------------------------------------------------------ */
      case OP_MOVE: { 
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC(i, 0); /* Ra <- Rb */
          } else if (ttisequal(RA(i), RB(i))) {
            SET_OPSPEC(i, 2); /* Ra:x <- Rb:x */
          } else {
            SET_OPSPEC(i, 1); /* Ra:? <- Rb */
          }
        } else if (reg == GETARG_B(i)) {
          if (reg_is_polymorphic) {
            if (is_polymorphic(L, GETARG_A(i))) {
              SET_OPSPEC(i, 0); /* Ra <- Rb */
            } else {
              SET_OPSPEC(i, 1); /* Ra:? <- Rb */
            }
          } else if (ttisequal(RA(i), RB(i))) {
              SET_OPSPEC(i, 2); /* Ra:x <- Rb:x */
          } else {
            SET_OPSPEC(i, 1); /* Ra:? <- Rb */
          }
        }
        break;
      }     
/* ------------------------------------------------------------------------ */
      case OP_LOADK: {
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC(i, 0); /* Ra <- Kb */
          } else if (ttisequal(RA(i), KB(i))) {
            SET_OPSPEC(i, 2); /* Ra:x <- Kb:x */
          } else {
            SET_OPSPEC(i, 1); /* Ra:? <- Kb */
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
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_raw);
          } else {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_chk);
          }
        }
      }
/* ------------------------------------------------------------------------ */
      case OP_LOADBOOL: {
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC(i, 0); /* Ra <- Ib:bool */
          } else if (ttisboolean(RA(i))) {
            SET_OPSPEC(i, 2); /* Ra:bool <- Ib:bool */
          } else {
            SET_OPSPEC(i, 1); /* Ra:? <- Ib:bool */
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_LOADNIL: {
        int a = GETARG_A(i);
        int b = GETARG_B(i);
        if (reg >= a && reg <= a+b) {
          int flag = 0;
          do {
            if (!is_polymorphic(L, a) || !ttisnil(base+a)) {
              flag = 1;
              break;
            }
            a++;
          } while (b--);
          if (flag)
            SET_OPSPEC(i, 1); /* R(a...a+b):? <- nil */
          else
            SET_OPSPEC(i, 0); /* R(a...a+b) <- nil */
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_GETTABUP:
      case OP_GETTABLE: {
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_raw);            
          } else {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_chk);
          }
        } else if (!c_is_const(i) && reg == GETARG_C(i)) {
          TValue *rc = RC(i);
          if (reg_is_polymorphic) {
            SET_OPSPEC_GETTAB_KEY(i, OPSPEC_TAB_KEY_raw);            
          } else if (ttisstring(rc)) {
            SET_OPSPEC_GETTAB_KEY(i, OPSPEC_TAB_KEY_str);
          } else if (ttisnumber(rc)) {
            int k;
            lua_Number n = nvalue(rc);
            lua_number2int(k, n);
            if (luai_numeq(cast_num(k), nvalue(rc))) {
              SET_OPSPEC_GETTAB_KEY(i, OPSPEC_TAB_KEY_int);
            } else {
              SET_OPSPEC_GETTAB_KEY(i, OPSPEC_TAB_KEY_obj);
            }
          } else {
            SET_OPSPEC_GETTAB_KEY(i, OPSPEC_TAB_KEY_obj);
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_SETTABUP:
      case OP_SETTABLE: {        
        if (!b_is_const(i) && reg == GETARG_B(i)) {
          TValue *rb = RB(i);
          if (reg_is_polymorphic) {
            SET_OPSPEC_SETTAB_KEY(i, OPSPEC_TAB_KEY_raw);            
          } else if (ttisstring(rb)) {
            SET_OPSPEC_SETTAB_KEY(i, OPSPEC_TAB_KEY_str);
          } else if (ttisnumber(rb)) {
            int k;
            lua_Number n = nvalue(rb);
            lua_number2int(k, n);
            if (luai_numeq(cast_num(k), nvalue(rb))) {
              SET_OPSPEC_SETTAB_KEY(i, OPSPEC_TAB_KEY_int);
            } else {
              SET_OPSPEC_SETTAB_KEY(i, OPSPEC_TAB_KEY_obj);
            }
          } else {
            SET_OPSPEC_SETTAB_KEY(i, OPSPEC_TAB_KEY_obj);
          }          
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_NEWTABLE: {
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic || ttistable(RA(i))) {
            SET_OPSPEC(i, 0); /* Ra <- {} */
          } else {
            SET_OPSPEC(i, 1); /* Ra:? <- {} */
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
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_raw);            
          } else {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_chk);
          }
        } else if ((!b_is_const(i) && reg == GETARG_B(i)) || 
                   (!c_is_const(i) && reg == GETARG_C(i))) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_raw);
          } else if (ttisnumber(RB(i)) && ttisnumber(RC(i))) {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_num);
          } else {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_obj);
          }
        }
        break;
      }      
/* ------------------------------------------------------------------------ */
      case OP_UNM: {
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_raw);
          } else {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_chk);
          }
        } else if (reg == GETARG_B(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_raw);
          } else if (ttisnumber(RB(i))) {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_num);
          } else {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_raw);
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_LEN: {
        if (reg == GETARG_A(i)) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_raw);
          } else {
            SET_OPSPEC_OUT(i, OPSPEC_OUT_chk);
          }
        } else if (reg == GETARG_B(i)) {
          TValue *rb = RB(i);
          if (reg_is_polymorphic) {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_raw);
          } else if (ttisstring(rb)) {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_str);
          } else if (ttistable(rb)) {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_tab);
          } else {
            SET_OPSPEC_ARITH_IN(i, OPSPEC_ARITH_IN_raw);
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_LT:
      case OP_LE: {
        TValue *rb = b_is_const(i) ? KB(i) : RB(i);
        TValue *rc = c_is_const(i) ? KC(i) : RC(i);
        if ((!b_is_const(i) && reg == GETARG_B(i)) ||
            (!c_is_const(i) && reg == GETARG_C(i))) {
          if (reg_is_polymorphic) {
            SET_OPSPEC_LESS_TYPE(i, OPSPEC_LESS_TYPE_raw);
          } else if (ttisequal(rb, rc)) {
            if (ttisnumber(rb)) {
              SET_OPSPEC_LESS_TYPE(i, OPSPEC_LESS_TYPE_num);
            } else if (ttisstring(rb)) {
              SET_OPSPEC_LESS_TYPE(i, OPSPEC_LESS_TYPE_str);
            } else {
              SET_OPSPEC_LESS_TYPE(i, OPSPEC_LESS_TYPE_raw);
            }
          } else {
            SET_OPSPEC_LESS_TYPE(i, OPSPEC_LESS_TYPE_raw);
          }
        }
        break;
      }
/* ------------------------------------------------------------------------ */
      case OP_VARARG: {
        int a = GETARG_A(i);
        int b = GETARG_B(i) - 1;
        int j;
        int n = cast_int(base - L->ci->func) - p->numparams - 1;
        if (b < 0) b = n;
        if (reg >= a && reg <= a+b) {
          int flag = 0;
          for (j = 0; j < b; j++) {
            if (!is_polymorphic(L, a+b)) {
              flag = 1;
            }
          }
          if (flag) {
            SET_OPSPEC(i, 1); /* R(a...a+b-2):? <- ... */
          } else {
            SET_OPSPEC(i, 0); /* R(a...a+b-2) <- ... */
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