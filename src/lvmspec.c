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



 // #define DEBUG_PRINT



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


#define store_possible (use_possible(reginfo, pc, REGINFO_USE_STORE))
#define load_possible  (use_possible(reginfo, pc, REGINFO_USE_LOAD))


void despecialize (Proto *p, int reg, RegInfo *reginfo) {
  if (reg < p->numparams && &(p->reginfos[reg]) == reginfo) {
    p->paramtypes[reg] = LUA_TNOSPEC;
  }

  int pc;
  for (pc = reginfo->startpc; pc <= reginfo->endpc; pc++) {
    Instruction *i = &(p->code[pc]);
    OpCode op = GET_OPCODE(*i);
    #ifdef DEBUG_PRINT
    printf("\t[%i] ", pc);
    printop(op);
    #endif
    switch (luaP_opcode2group[op]) {
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
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out(op, OpType_raw));
          p->exptypes[pc].t = LUA_TNONE;
        }
        break;
      case OP_SELF:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_self(op, OpType_raw));
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
              SET_OPCODE(*i, set_out(op, OpType_raw));
              break;
            }
          }
        }
        break;
      }
      case OP_GETTABLE: case OP_GETTABUP:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_gettab(op, OpType_raw));
          p->exptypes[pc].t = LUA_TNONE;
        }
        if (!opck(op) && GETARG_C(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_gettab(op, OpType_raw));
        }
        break;
      case OP_SETTABLE: case OP_SETTABUP:
        if (!opbk(op) && GETARG_B(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_settab(op, OpType_raw));
        }
        break;
      case OP_ADD: case OP_SUB: case OP_MUL:
      case OP_DIV: case OP_MOD: case OP_POW:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_arith(op, OpType_raw));
          p->exptypes[pc].t = LUA_TNONE;
        }
        if (!opbk(op) && GETARG_B(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_arith(op, OpType_raw));
        }
        if (!opck(op) && GETARG_C(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_arith(op, OpType_raw));
        }
        break;
      case OP_UNM: 
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_unm(op, OpType_raw));
          p->exptypes[pc].t = LUA_TNONE;
        }
        if (GETARG_B(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_unm(op, OpType_raw));
        }
        break;
      case OP_LEN:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_len(op, OpType_raw));
          p->exptypes[pc].t = LUA_TNONE;
        }
        if (GETARG_B(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_len(op, OpType_raw));
        }
        break;
      case OP_LE: case OP_LT:
        if (!opbk(op) && GETARG_B(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_less(op, OpType_raw));
        }
        if (!opck(op) && GETARG_C(*i) == reg && load_possible) {
          SET_OPCODE(*i, set_in_less(op, OpType_raw));
        }
        break;
      case OP_CALL: {
        int a = GETARG_A(*i);
        int nresults = GETARG_C(*i) - 1;
        if (nresults < 1) break; /* also handles LUA_MULTRET */
        if (a <= reg && reg <= a+nresults && store_possible) {
          int *exptypes = p->exptypes[pc].ts;
          exptypes[reg-a] = LUA_TNONE;
          int j;
          for (j = 0; j < nresults; j++) {
            if (exptypes[j] != LUA_TNONE) {
              SET_OPCODE(*i, set_out(op, OpType_raw));
              break;
            }
          }
        }
        break;
      }
      case OP_TFORCALL: {
        int a = GETARG_A(*i);
        int nresults = GETARG_C(*i);
        int cb = a + 3; /* call base */
        if (cb <= reg && reg <= cb+nresults-1 && store_possible) {
          int *exptypes = p->exptypes[pc].ts;
          exptypes[reg-cb] = LUA_TNONE;
          int j;
          for (j = 0; j < nresults; j++) {
            if (exptypes[j] != LUA_TNONE) {
              SET_OPCODE(*i, set_out(op, OpType_raw));
              break;
            }
          }
        }
        break;
      }
      case OP_VARARG: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i) - 1;
        if (b < 1) break;
        if (a <= reg && reg <= a+b && store_possible) {
          int *exptypes = p->exptypes[pc].ts;
          exptypes[reg-a] = LUA_TNONE;
          int j;
          for (j = 0; j < b; j++) {
            if (exptypes[j] != LUA_TNONE) {
              SET_OPCODE(*i, set_out(op, OpType_raw));
              break;
            }
          }
        }
        break;        
      }
      default:
      #ifdef DEBUG_PRINT
      printf("\n"); continue;
      #endif
        break;
    }
    #ifdef DEBUG_PRINT
    op = GET_OPCODE(*i);
    printf(" --> "); printop(op); printf("\n");
    #endif
  }
}

void luaVS_despecialize (lua_State *L, int reg) {
  #ifdef DEBUG_PRINT
  printf("%s %i\n",__func__,reg);
  #endif

  Proto *p = clLvalue(L->ci->func)->p;
  int pc = pcRel(L->ci->u.l.savedpc, p);
  RegInfo *reginfo = findreginfo(p, reg, pc, REGINFO_USE_STORE);
  lua_assert(reginfo != NULL);
  despecialize(p, reg, reginfo);
}

void luaVS_despecialize_param (Proto *p, int reg) {
  #ifdef DEBUG_PRINT
  printf("%s %i\n",__func__,reg);
  #endif

  lua_assert(reg < p->numparams);
  RegInfo *reginfo = &p->reginfos[reg];
  if (reginfo->state == REGINFO_STATE_UNUSED) return;
  despecialize(p, reg, reginfo);
}

void luaVS_despecialize_upval (Proto *p, int idx) {
  #ifdef DEBUG_PRINT
  printf("%s %p %i\n", __func__, p, idx);
  #endif
  /* find the function that has the local that is the origin of the upvalue */
  int chaini = 0;
  Upvaldesc desc = p->upvalues[idx];
  for (;;) {
    p = p->encp;
    if (desc.instack) { chaini = desc.reginfo_idx; break; } 
    else desc = p->upvalues[desc.idx];
  }
  /* find the right reginfo for the local */
  RegInfo *reginfo = &p->reginfos[desc.idx];
  while (chaini-- > 0) reginfo = reginfo->next;
  lua_assert(reginfo->state == REGINFO_STATE_LOCAL_CLOSED);
  despecialize(p, desc.idx, reginfo);
}


void _add_upvalue_guards (Proto *p, int idx, int instack, int type) {
  #ifdef DEBUG_PRINT
  printf("%s %p %i %i %i\n", __func__, p, idx, instack, type);
  #endif
  int i ;
  for (i = 0; i < p->sizeupvalues; i++) {
    Upvaldesc *desc = &p->upvalues[i];
    if (desc->idx == idx && desc->instack == instack) {
      desc->expected_type = type;
      int n = p->sizep;
      while (n > 0)
        _add_upvalue_guards(p->p[--n], i, 0, type);
      break;
    }
  }
}


/* add type guards to all stores of the register within the given scope */
void add_guards (Proto *p, int reg, RegInfo *reginfo, int type) {
  #ifdef DEBUG_PRINT
  printf("\t%s %i %i\n",__func__,reg,type);
  #endif

  int n = p->sizep;
  while (n > 0)
    _add_upvalue_guards(p->p[--n], reg, 1, type);

  int pc;
  for (pc = reginfo->startpc; pc <= reginfo->endpc; pc++) {
    Instruction *i = &(p->code[pc]);
    OpCode op = GET_OPCODE(*i);
    #ifdef DEBUG_PRINT
    printf("\t\t[%i] ", pc);
    printop(op);
    #endif
    switch (luaP_opcode2group[op]) {
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
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out(op, OpType_chk));
          p->exptypes[pc].t = type;
        }
        break;
      case OP_GETTABLE: case OP_GETTABUP: {
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_gettab(op, OpType_chk));
          p->exptypes[pc].t = type;
        }
        break;
      }      
      case OP_ADD: case OP_SUB: case OP_MUL: 
      case OP_DIV: case OP_MOD: case OP_POW: {
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_arith(op, OpType_chk));
          p->exptypes[pc].t = type;
        }
        break;
      }
      case OP_UNM:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_unm(op, OpType_chk));
          p->exptypes[pc].t = type;
        }
        break;
      case OP_SELF:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_self(op, OpType_chk));
          p->exptypes[pc].t = type;
        }
        break;
      case OP_LEN:
        if (GETARG_A(*i) == reg && store_possible) {
          SET_OPCODE(*i, set_out_len(op, OpType_chk));
          p->exptypes[pc].t = type;
        }
        break;
      case OP_LOADNIL: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i);
        if (a <= reg && reg <= a+b && store_possible) {
          SET_OPCODE(*i, set_out(op, OpType_chk));
          p->exptypes[pc].ts[reg-a] = type;
        }
        break;      
      }
      case OP_CALL: {
        int a = GETARG_A(*i);
        int nresults = GETARG_C(*i) - 1;
        if (nresults < 1) break; /* also handles LUA_MULTRET */
        if (a <= reg && reg <= a+nresults && store_possible) {
          SET_OPCODE(*i, set_out(op, OpType_chk));
          p->exptypes[pc].ts[reg-a] = type;
        }
        break;
      }
      case OP_TFORCALL: {
        int a = GETARG_A(*i);
        int nresults = GETARG_C(*i);
        int cb = a + 3; /* call base */
        if (cb <= reg && reg <= cb+nresults-1 && store_possible) {
          SET_OPCODE(*i, set_out(op, OpType_chk));
          p->exptypes[pc].ts[reg-cb] = type;
        }
        break;
      }
      case OP_VARARG: {
        int a = GETARG_A(*i);
        int b = GETARG_B(*i) - 1;
        if (b == -1) break; /* direct input to a call */
        if (a <= reg && reg <= a+b && store_possible) {
          SET_OPCODE(*i, set_out(op, OpType_chk));
          p->exptypes[pc].ts[reg-a] = type;
        }
        break;
      }
      default:
      #ifdef DEBUG_PRINT
      printf("\n"); continue;
      #endif
        break;
    }

    #ifdef DEBUG_PRINT
    printf(" --> "); printop(GET_OPCODE(*i)); printf("\n");
    #endif
  }
}


#define RB(i) (base+GETARG_B(i))
#define RC(i) (base+GETARG_C(i))
#define KB(i) (k+GETARG_B(i))
#define KC(i) (k+GETARG_C(i))


#define _add_guards(r,t) { int _r = r; \
    RegInfo *_reginfo = findreginfo(p, _r, pc, REGINFO_USE_LOAD); \
    lua_assert(_reginfo != NULL); \
    add_guards(p, _r, _reginfo, t); } \

void luaVS_specialize (lua_State *L) {
  #ifdef DEBUG_PRINT
  printf("%s\n",__func__);
  #endif

  Proto *p = clLvalue(L->ci->func)->p;
  int pc = pcRel(L->ci->u.l.savedpc, p);
  StkId base = L->ci->u.l.base;
  TValue *k = p->k;  
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
  #ifdef DEBUG_PRINT
  printf("\t[%i] ", pc);
  printop(op);
  printf("\n");
  #endif
  switch (luaP_opcode2group[op]) {
    case OP_GETTABLE: 
    case OP_GETTABUP: {
      TValue *rc = opck(op) ? KC(*i) : RC(*i);
      int type = rttype(rc);      
      if (ttisint(rc)) {op = set_in_gettab(op, OpType_int); type = LUA_TINT;}
      else if (ttisstring(rc))   op = set_in_gettab(op, OpType_str);
      else if (ttisnil(rc)) op = set_in_gettab(op, OpType_raw);
      else                  op = set_in_gettab(op, OpType_obj);
      SET_OPCODE(*i, op);
      if (!opck(op)) _add_guards(GETARG_C(*i), type);
      break;
    }
    case OP_SETTABLE: 
    case OP_SETTABUP: {
      TValue *rb = opbk(op) ? KB(*i) : RB(*i);
      int type = rttype(rb);      
      if (ttisint(rb)) {op = set_in_settab(op, OpType_int); type = LUA_TINT;}
      else if (ttisstring(rb))   op = set_in_settab(op, OpType_str);
      else if (ttisnil(rb)) op = set_in_settab(op, OpType_raw);
      else                  op = set_in_settab(op, OpType_obj);
      SET_OPCODE(*i, op);
      if (!opbk(op)) _add_guards(GETARG_B(*i), type);
      break;
    }
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW: {
      TValue *rb = opbk(op) ? KB(*i) : RB(*i);
      TValue *rc = opck(op) ? KC(*i) : RC(*i);
      if (ttisnumber(rb)) {        
        if (ttisnumber(rc))       op = set_in_arith(op, OpType_num);
        else if (ttisstring(rc))  op = set_in_arith(op, OpType_raw);
        else                      op = set_in_arith(op, OpType_obj);
      }
      else if (ttisstring(rb)) {
        if (ttisnumber(rc))       op = set_in_arith(op, OpType_raw);
        else if (ttisstring(rc))  op = set_in_arith(op, OpType_raw);
        else                      op = set_in_arith(op, OpType_obj);
      }
      else                        op = set_in_arith(op, OpType_obj);
      SET_OPCODE(*i, op);
      if (!opbk(op)) _add_guards(GETARG_B(*i), rttype(rb));
      if (!opck(op)) _add_guards(GETARG_C(*i), rttype(rc));      
      break;
    }
    case OP_UNM: {
      TValue *rb = RB(*i);
      if (ttisnumber(rb)) op = set_in_unm(op, OpType_num);
      else                op = set_in_unm(op, OpType_raw);
      SET_OPCODE(*i, op);
      _add_guards(GETARG_B(*i), rttype(rb));
      break;
    }
    case OP_LEN: {
      TValue *rb = RB(*i);
      if (ttisstring(rb))     op = set_in_len(op, OpType_str);
      else if (ttistable(rb)) op = set_in_len(op, OpType_tab);
      else                    op = set_in_len(op, OpType_raw);
      SET_OPCODE(*i, op);
      _add_guards(GETARG_B(*i), rttype(rb));
      break;
    }
    case OP_LT: case OP_LE: {
      TValue *rb = opbk(op) ? KB(*i) : RB(*i);
      TValue *rc = opck(op) ? KC(*i) : RC(*i);
      if (ttisequal(rb, rc)) {
        if (ttisnumber(rb))       op = set_in_less(op, OpType_num);
        else if (ttisstring(rb))  op = set_in_less(op, OpType_str);
        else                      op = set_in_less(op, OpType_raw);
      } else                      op = set_in_less(op, OpType_raw);
      SET_OPCODE(*i, op);
      if (!opbk(op)) _add_guards(GETARG_B(*i), rttype(rb));
      if (!opck(op)) _add_guards(GETARG_C(*i), rttype(rc));
      break;
    }
    default:
      lua_assert(0);
      break;
  }
  #ifdef DEBUG_PRINT
  op = GET_OPCODE(*i);
  printf("\t-->");
  printop(op);
  printf("\n");
  #endif

}



