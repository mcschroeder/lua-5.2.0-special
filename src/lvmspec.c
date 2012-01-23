/*
** Lua virtual machine bytecode specialization
*/

#define lvmspec_c
#define LUA_CORE

#include <stdio.h>
#include "lua.h"

#include "lopcodes.h"
#include "lstate.h"
#include "ldebug.h"  // TODO: for pcRel; can we do without this?
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


#define store_possible use_possible(reginfo, pc, REGINFO_USE_STORE)
#define load_possible  use_possible(reginfo, pc, REGINFO_USE_LOAD)

#define ISK_B(i) (GET_OPSPEC_BK(i) == OPSPEC_kst)
#define ISK_C(i) (GET_OPSPEC_CK(i) == OPSPEC_kst)


void despecialize (Proto *p, int reg, RegInfo *reginfo) {
  int pc;
  for (pc = reginfo->startpc; pc <= reginfo->endpc; pc++) {
    Instruction *i = &(p->code[pc]);
    switch (GET_OPCODE(*i)) {
      case OP_MOVE:
      case OP_LOADK:
      case OP_LOADKX:
      case OP_LOADBOOL:
      case OP_GETUPVAL:
      case OP_NEWTABLE:
      case OP_SELF:
      case OP_CONCAT:
      case OP_TESTSET:
      case OP_CLOSURE:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPSPEC(*i, 0);
          p->exptypes[pc].t = LUA_TNONE;
        }
        break;
      case OP_LOADNIL: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i);
        if (a <= reg && reg <= a+b && store_possible) {
          int *exptypes = p->exptypes[pc].ts;
          exptypes[reg-a] = LUA_TNONE;
          int j;
          for (j = 0; j < b; j++) {
            if (exptypes[j] != LUA_TNONE) {
              SET_OPSPEC(*i, 0);
              break;
            }
          }
        }
        break;
      }
      case OP_GETTABLE: case OP_GETTABUP:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          p->exptypes[pc].t = LUA_TNONE;
        }
        if (!ISK_C(*i) && GETARG_C(*i) == reg && load_possible) {
          SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_raw);          
        }
        break;
      case OP_SETTABLE: case OP_SETTABUP:
        if (!ISK_B(*i) && GETARG_B(*i) == reg && load_possible) {
          SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_raw);
        }
        break;
      case OP_ADD: case OP_SUB: case OP_MUL:
      case OP_DIV: case OP_MOD: case OP_POW:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          p->exptypes[pc].t = LUA_TNONE;
        }
        if (!ISK_B(*i) && GETARG_B(*i) == reg && load_possible) {
          SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        }
        if (!ISK_C(*i) && GETARG_C(*i) == reg && load_possible) {
          SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        }
        break;
      case OP_UNM: case OP_LEN:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPSPEC_OUT(*i, OPSPEC_OUT_raw);
          p->exptypes[pc].t = LUA_TNONE;
        }
        if (!ISK_B(*i) && GETARG_B(*i) == reg && load_possible) {
          SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        }
        break;
      case OP_LE: case OP_LT:
        if (!ISK_B(*i) && GETARG_B(*i) == reg && load_possible) {
          SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_raw);
        }
        if (!ISK_C(*i) && GETARG_C(*i) == reg && load_possible) {
          SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_raw);
        }
        break;
      case OP_CALL: {
        int a = GETARG_A(*i);
        int nresults = GETARG_C(*i) - 1;
        lua_assert(nresults != LUA_MULTRET);        
        if (a <= reg && reg <= a+nresults) {
          int *exptypes = p->exptypes[pc].ts;
          exptypes[reg-a] = LUA_TNONE;
          int j;
          for (j = 0; j < nresults; j++) {
            if (exptypes[j] != LUA_TNONE) {
              SET_OPSPEC(*i, 0);
              break;
            }
          }
        }
        break;
      }
      case OP_VARARG: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i) - 1;
        lua_assert(b != -1);        
        if (a <= reg && reg <= a+b) {
          int *exptypes = p->exptypes[pc].ts;
          exptypes[reg-a] = LUA_TNONE;
          int j;
          for (j = 0; j < b; j++) {
            if (exptypes[j] != LUA_TNONE) {
              SET_OPSPEC(*i, 0);
              break;
            }
          }
        }
        break;        
      }
      default:
        break;
    }
  }
}

void luaVS_despecialize (lua_State *L, int reg) {
  Proto *p = clLvalue(L->ci->func)->p;
  int pc = pcRel(L->ci->u.l.savedpc, p);
  RegInfo *reginfo = findreginfo(p, reg, pc, REGINFO_USE_STORE);
  lua_assert(reginfo != NULL);
  despecialize(p, reg, reginfo);
}

void luaVS_despecialize_param (lua_State *L, int reg) {
  Proto *p = clLvalue(L->ci->func)->p;
  lua_assert(reg < p->numparams);
  RegInfo *reginfo = &p->reginfos[reg];
  if (reginfo->state == REGINFO_STATE_UNUSED) return;
  despecialize(p, reg, reginfo);
}

/* add type guards to all stores of the register within the given scope */
void add_guards (Proto *p, int reg, RegInfo *reginfo, int type) {  
  int pc;
  for (pc = reginfo->startpc; pc <= reginfo->endpc; pc++) {
    Instruction *i = &(p->code[pc]);
    switch (GET_OPCODE(*i)) {
      case OP_MOVE:
      case OP_LOADK:
      case OP_LOADKX:
      case OP_LOADBOOL:
      case OP_GETUPVAL:
      case OP_NEWTABLE:
      case OP_NOT:
      case OP_CONCAT:
      case OP_TESTSET:
      case OP_CLOSURE:
        if (GETARG_A(*i) == reg) {
          SET_OPSPEC(*i, 1);
          p->exptypes[pc].t = type;
        }
        break;
      case OP_GETTABLE: case OP_GETTABUP:
      case OP_SELF:
      case OP_ADD: case OP_SUB: case OP_MUL: 
      case OP_DIV: case OP_MOD: case OP_POW:
      case OP_UNM: case OP_LEN:
        if (GETARG_A(*i) == reg) {
          SET_OPSPEC_OUT(*i, OPSPEC_OUT_chk);
          p->exptypes[pc].t = type;
        }
        break;
      case OP_LOADNIL: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i);
        if (a <= reg && reg <= a+b) {
          SET_OPSPEC(*i, 1);
          p->exptypes[pc].ts[reg-a] = type;
        }
        break;      
      }
      case OP_CALL: {
        int a = GETARG_A(*i);
        int nresults = GETARG_C(*i) - 1;
        if (nresults == LUA_MULTRET) break; /* direct input to another call */
        if (a <= reg && reg <= a+nresults) {
          SET_OPSPEC(*i, 1);
          p->exptypes[pc].ts[reg-a] = type;
        }
        break;
      }
      case OP_VARARG: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i) - 1;
        if (b == -1) break; /* direct input to a call */
        if (a <= reg && reg <= a+b) {
          SET_OPSPEC(*i, 1);
          p->exptypes[pc].ts[reg-a] = type;
        }
        break;
      }
      default:
        break;
    }
  }
}


static int ttisint (TValue *v) {
  if (!ttisnumber(v)) return 0;
  int k;
  lua_Number n = nvalue(v);  
  lua_number2int(k, n);
  return luai_numeq(cast_num(k), n);
}


#define RB(i) (base+GETARG_B(i))
#define RC(i) (base+GETARG_C(i))
#define KB(i) (k+GETARG_B(i))
#define KC(i) (k+GETARG_C(i))
#define RKB(i) (ISK_B(i) ? KB(i) : RB(i))
#define RKC(i) (ISK_C(i) ? KC(i) : RC(i))


#define _add_guards(r,t) { int _r = r; \
    RegInfo *_reginfo = findreginfo(p, _r, pc, REGINFO_USE_LOAD); \
    lua_assert(_reginfo != NULL); \
    add_guards(p, _r, _reginfo, t); } \

void luaVS_specialize (lua_State *L) {
  Proto *p = clLvalue(L->ci->func)->p;
  int pc = pcRel(L->ci->u.l.savedpc, p);
  StkId base = L->ci->u.l.base;
  TValue *k = p->k;  
  Instruction *i = &(p->code[pc]);
  switch (GET_OPCODE(*i)) {
    case OP_GETTABLE: case OP_GETTABUP: {
      TValue *rc = RKC(*i);
      if (ttisstring(rc))   SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_str);
      else if (ttisint(rc)) SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_int);
      else                  SET_OPSPEC_GETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
      if (!ISK_C(*i)) _add_guards(GETARG_C(*i), rttype(rc));
      break;
    }
    case OP_SETTABLE: case OP_SETTABUP: {
      TValue *rb = RKB(*i);
      if (ttisstring(rb))   SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_str);
      else if (ttisint(rb)) SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_int);
      else                  SET_OPSPEC_SETTAB_KEY(*i, OPSPEC_TAB_KEY_obj);
      if (!ISK_B(*i)) _add_guards(GETARG_B(*i), rttype(rb));
      break;
    }
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW: {
      TValue *rb = RKB(*i);
      TValue *rc = RKC(*i);
      if (ttisnumber(rb)) {
        if (ttisnumber(rc))       SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_num);
        else if (ttisstring(rc))  SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        else                      SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_obj);
      }
      else if (ttisstring(rb)) {
        if (ttisnumber(rc))       SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        else if (ttisstring(rc))  SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
        else                      SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_obj);
      }
      else                        SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_obj);
      if (!ISK_B(*i)) _add_guards(GETARG_B(*i), rttype(rb));
      if (!ISK_C(*i)) _add_guards(GETARG_C(*i), rttype(rc));
      break;
    }
    case OP_UNM: {
      TValue *rb = RB(*i);
      if (ttisnumber(rb)) SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_num);
      else                SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
      _add_guards(GETARG_B(*i), rttype(rb));
      break;
    }
    case OP_LEN: {
      TValue *rb = RB(*i);
      if (ttisstring(rb))     SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_str);
      else if (ttistable(rb)) SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_tab);
      else                    SET_OPSPEC_ARITH_IN(*i, OPSPEC_ARITH_IN_raw);
      _add_guards(GETARG_B(*i), rttype(rb));
      break;
    }
    case OP_LT: case OP_LE: {
      TValue *rb = RKB(*i);
      TValue *rc = RKC(*i);
      if (ttisequal(rb, rc)) {
        if (ttisnumber(rb))       SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_num);
        else if (ttisstring(rb))  SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_str);
        else                      SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_raw);
      } 
      else                        SET_OPSPEC_LESS_TYPE(*i, OPSPEC_LESS_TYPE_raw);
      if (!ISK_B(*i)) _add_guards(GETARG_B(*i), rttype(rb));
      if (!ISK_C(*i)) _add_guards(GETARG_C(*i), rttype(rc));
      break;
    }
    default:
      lua_assert(0);
      break;
  }
}



