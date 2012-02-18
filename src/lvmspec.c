/*
** Lua virtual machine bytecode specialization
*/

#define lvmspec_c
#define LUA_CORE

#include <stdio.h>
#include "lua.h"

#include "lopcodes.h"
#include "lstate.h"
#include "ldebug.h"  // TODO: for pcRel; can we do without this? (e.g. get pc from callinfo?)
#include "lvmspec.h"



 // #define DEBUG_PRINT




static RegInfo *findreginfo (Proto *p, int pc, int reg, int use) {
  lua_assert(reg < p->sizereginfos);
  RegInfo *reginfo = &(p->reginfos[reg]);
  while (reginfo) {
    if (reginfo->state == REGINFO_STATE_UNUSED) return NULL;
    if (reginfo->state == REGINFO_STATE_LOCAL_UNUSED) return NULL;
    if (pc > reginfo->startpc && pc < reginfo->endpc) break;
    if (pc == reginfo->startpc && (reginfo->firstuse & use) != 0) break;
    if (pc == reginfo->endpc && (reginfo->lastuse & use) != 0) break;
    reginfo = reginfo->next;
  }
  return reginfo;
}


static void remove_guard (Proto *p, int pc, int reg) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
#ifdef DEBUG_PRINT
  printf("\t%s [%i] %i ",__func__,pc,reg);
  printop(op);
#endif
  int a = GETARG_A(*i);
  switch (op2grp(op)) {
    case OP_MOVE:
      if (a == reg) SET_OPCODE(*i, set_out_move(op, OpType_raw));
      break;
    case OP_GETUPVAL:
    case OP_CONCAT:
    case OP_TESTSET:
    case OP_CHKTYPE:
      if (a == reg) SET_OPCODE(*i, set_out(op, OpType_raw));
      break;
    case OP_GETTABLE:
    case OP_GETTABUP:
      if (a == reg) SET_OPCODE(*i, set_out_gettab(op, OpType_raw));
      break;
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW:
      if (a == reg) SET_OPCODE(*i, set_out_arith(op, OpType_raw));
      break;
    case OP_UNM:
      if (a == reg) SET_OPCODE(*i, set_out_unm(op, OpType_raw));
      break;
    case OP_LEN:
      if (a == reg) SET_OPCODE(*i, set_out_len(op, OpType_raw));
      break;
    default:
#ifdef DEBUG_PRINT
      printf("\n");
#endif
      return;
  }
#ifdef DEBUG_PRINT
  printf(" --> ");
  printop(GET_OPCODE(*i));
  printf("\n");
#endif
}


static int add_guard (Proto *p, int pc, int reg, OpType type) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
#ifdef DEBUG_PRINT
  printf("\t%s [%i] %i %i ",__func__,pc,reg,type);
  printop(op);
#endif
  int a = GETARG_A(*i);
  switch (op2grp(op)) {
    case OP_MOVE:
      if (a == reg) {
        int intype = opin(op);
        if (intype == OpType_raw || intype == OpType_chk)
          SET_OPCODE(*i, set_out_move(op, type));
        else if (intype == type)
          SET_OPCODE(*i, set_out_move(op, OpType_raw));
        else
          return 0;
      }
      break;
    case OP_LOADK:
    case OP_LOADKX:
      if (a == reg && opin(op) != type) return 0;
      break;
    case OP_LOADBOOL:
      if (a == reg) return 0;
      break;
    case OP_LOADNIL:
      if (a <= reg && reg <= a+GETARG_B(*i)) return 0;
      break;
    case OP_GETUPVAL:
    case OP_CONCAT:
    case OP_TESTSET:
    case OP_CHKTYPE:
      if (a == reg) SET_OPCODE(*i, set_out(op, type));
      break;
    case OP_GETTABLE:
    case OP_GETTABUP:
      if (a == reg) SET_OPCODE(*i, set_out_gettab(op, type));
      break;
    case OP_NEWTABLE:
    case OP_CLOSURE:
      if (a == reg && type != OpType_obj) return 0;
      break;
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW:
      if (a == reg) {
        if (opin(op) == OpType_num) {
          if (type == OpType_num)
            SET_OPCODE(*i, set_out_arith(op, OpType_raw));
          else
            return 0;
        } else {
          SET_OPCODE(*i, set_out_arith(op, type));
        }
      }
      break;
    case OP_UNM:
      if (a == reg) {
        if (opin(op) == OpType_num) {
          if (type == OpType_num)
            SET_OPCODE(*i, set_out_unm(op, OpType_raw));
          else
            return 0;
        } else {
          SET_OPCODE(*i, set_out_unm(op, type));
        }
      }
      break;
    case OP_LEN:
      if (a == reg) {
        if (opin(op) == OpType_str) {
          if (type == OpType_num)
            SET_OPCODE(*i, set_out_len(op, OpType_raw));
          else
            return 0;
        } else {
          SET_OPCODE(*i, set_out_len(op, type));
        }
      }
      break;
    default:
#ifdef DEBUG_PRINT
      printf("\n");
      return 1;
#endif
      break;
    }
#ifdef DEBUG_PRINT
  printf(" --> ");
  printop(GET_OPCODE(*i));
  printf("\n");
#endif
  return 1;
}


static void remove_guards (Proto *p, int reg, RegInfo *reginfo) {
#ifdef DEBUG_PRINT
  printf("\t%s %i\n",__func__,reg);
#endif
  int pc = reginfo->startpc;
  if (reginfo->firstuse & REGINFO_USE_STORE) remove_guard(p, pc, reg);
  while (++pc < reginfo->endpc)              remove_guard(p, pc, reg);
  if (reginfo->lastuse & REGINFO_USE_STORE)  remove_guard(p, pc, reg);
}


static void add_upvalue_guards (Proto *p, int idx, int instack, OpType type) {
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
        add_upvalue_guards(p->p[--n], i, 0, type);
      break;
    }
  }
}


#define _add_guard_or_abort           \
  if (!add_guard(p, pc, reg, type)) { \
    remove_guards(p, reg, reginfo);   \
    return 0;                         \
  }

static int add_guards (Proto *p, int reg, RegInfo *reginfo, OpType type) {
#ifdef DEBUG_PRINT
  printf("\t%s %i %i\n",__func__,reg,type);
#endif

  lua_assert(reginfo != NULL);
  int n = p->sizep;
  while (n > 0)
    add_upvalue_guards(p->p[--n], reg, 1, type);

  int pc = reginfo->startpc;
  if (reginfo->firstuse & REGINFO_USE_STORE) _add_guard_or_abort
  while (++pc < reginfo->endpc)              _add_guard_or_abort
  if (reginfo->lastuse & REGINFO_USE_STORE)  _add_guard_or_abort
  return 1;
}


static void despecialize_all (Proto *p, int reg, RegInfo *reginfo);

static void despecialize_store (Proto *p, int pc, int a) {
#ifdef DEBUG_PRINT
  printf("\n\t\t%s [%i] %i\n",__func__,pc,a);
#endif
  RegInfo *reginfo = findreginfo(p, pc, a, REGINFO_USE_STORE);
  lua_assert(reginfo != NULL);
  if ((reginfo->startpc == pc && (reginfo->firstuse & REGINFO_USE_LOAD)) ||
      (reginfo->endpc == pc && (reginfo->lastuse & REGINFO_USE_LOAD)))
      return; /* avoid infinite loop */
  despecialize_all(p, a, reginfo);
  remove_guards(p, a, reginfo);
}

static void despecialize (Proto *p, int pc, int reg) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
#ifdef DEBUG_PRINT
  printf("\t%s [%i] %i ",__func__,pc,reg);
  printop(op);
#endif
  int a = GETARG_A(*i);
  int b = GETARG_B(*i);
  int c = GETARG_C(*i);
  switch (op2grp(op)) {
    case OP_MOVE:
      if (b == reg) {
        SET_OPCODE(*i, set_in_move(op, OpType_raw));
        despecialize_store(p, pc, a);
      }
      break;
    case OP_GETTABLE:
    case OP_GETTABUP:
      if (!ISK(c) && c == reg) {
        SET_OPCODE(*i, set_in_gettab(op, OpType_raw));
      }
      break;
    case OP_SETTABLE:
    case OP_SETTABUP:
      if (!ISK(b) && b == reg) {
        SET_OPCODE(*i, set_in_settab(op, OpType_raw));
      }
      break;
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW:
      if ((!ISK(b) && b == reg) || (!ISK(c) && c == reg)) {
        SET_OPCODE(*i, set_in_arith(op, OpType_raw));
        despecialize_store(p, pc, a);
      }
      break;
    case OP_UNM:
      if (b == reg) {
        SET_OPCODE(*i, set_in_unm(op, OpType_raw));
        despecialize_store(p, pc, a);
      }
      break;
    case OP_LEN:
      if (b == reg) {
        SET_OPCODE(*i, set_in_len(op, OpType_raw));
        despecialize_store(p, pc, a);
      }
      break;
    case OP_LT: case OP_LE:
      if ((!ISK(b) && b == reg) || (!ISK(c) && c == reg)) {
        SET_OPCODE(*i, set_in_less(op, OpType_raw));
      }
      break;
    default: 
#ifdef DEBUG_PRINT
      printf("\n");
#endif
      return;
  }
#ifdef DEBUG_PRINT
  printf(" --> ");
  printop(GET_OPCODE(*i));
  printf("\n");
#endif
}


static void despecialize_all (Proto *p, int reg, RegInfo *reginfo) {
#ifdef DEBUG_PRINT
  printf("%s %p reg=%i\n",__func__,p,reg);
#endif
  
  int pc = reginfo->startpc;
  if (reginfo->firstuse & REGINFO_USE_LOAD) despecialize(p, pc, reg);
  while (++pc < reginfo->endpc)             despecialize(p, pc, reg);
  if (reginfo->lastuse & REGINFO_USE_LOAD)  despecialize(p, pc, reg);
}


#define _add_guards(r,t) \
  (add_guards(p, r, findreginfo(p, pc, r, REGINFO_USE_LOAD), t))

#define _remove_guards(r) \
  (remove_guards(p, r, findreginfo(p, pc, r, REGINFO_USE_LOAD)))


void luaVS_specialize (lua_State *L) {
  CallInfo *ci = L->ci;
  Proto *p = clLvalue(ci->func)->p;
  StkId base = ci->u.l.base;
  int pc = pcRel(ci->u.l.savedpc, p);
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
#ifdef DEBUG_PRINT
  printf("\t%s [%i] ",__func__,pc);
  printop(op);
#endif
  int a = GETARG_A(*i);
  int b = GETARG_B(*i);
  int c = GETARG_C(*i);
  TValue *rb = ISK(b) ? p->k+INDEXK(b) : base+b;
  TValue *rc = ISK(c) ? p->k+INDEXK(c) : base+c;
  switch (op2grp(op)) {
    case OP_MOVE: {
      OpType type = OpType_obj;
      if (ttisnumber(rb)) type = OpType_num;
      else if (ttisstring(rb)) type = OpType_str;
      if (_add_guards(b, type)) {
        if (opout(op) != OpType_raw)
          luaVS_despecialize(L, a);
        SET_OPCODE(*i, set_in_move(op, type));
      } else {
        SET_OPCODE(*i, set_in_move(op, OpType_raw));
      }
      break;
    }
    case OP_GETTABLE:
    case OP_GETTABUP: {
      OpType type = OpType_obj;
      if (ttisnumber(rc)) type = OpType_num;
      else if (ttisstring(rc)) type = OpType_str;
      else if (ttisnil(rc)) type = OpType_raw;
      if (type != OpType_raw && !ISK(c) && !_add_guards(c, type))
        SET_OPCODE(*i, set_in_gettab(op, OpType_raw));
      else
        SET_OPCODE(*i, set_in_gettab(op, type));
      break;
    }
    case OP_SETTABLE: 
    case OP_SETTABUP: {
      OpType type = OpType_obj;
      if (ttisnumber(rb)) type = OpType_num;
      else if (ttisstring(rb)) type = OpType_str;
      else if (ttisnil(rb)) type = OpType_raw;
      if (type != OpType_raw && !ISK(b) && !_add_guards(b, type))
        SET_OPCODE(*i, set_in_settab(op, OpType_raw));
      else
        SET_OPCODE(*i, set_in_settab(op, type));
      break;
    }
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW: {
      OpType type = OpType_raw;
      if (ttisnumber(rb)) {
        if (ttisnumber(rc)) {
          int status = 1;
          if (!ISK(b)) status = _add_guards(b, OpType_num);
          if (status && !ISK(c)) {
            if (!_add_guards(c, OpType_num)) {
              _remove_guards(b);
              status = 0;
            }
          }
          if (status) {
            type = OpType_num;
            if (opout(op) == OpType_num)
              op = set_out_arith(op, OpType_raw);
            else if (opout(op) == OpType_str || opout(op) == OpType_obj)
              luaVS_despecialize(L, a);
          }
        } 
        else if (!ttisstring(rc)) {
          if (ISK(c) || _add_guards(c, OpType_obj))
            type = OpType_obj;
        }
      }
      else if (ttisstring(rb)) {
        if (!ttisnumber(rc) && !ttisstring(rc)) {
          if (ISK(c) || _add_guards(c, OpType_obj))
            type = OpType_obj;
        }
      }
      else {
        if (ISK(b) || _add_guards(b, OpType_obj))
          type = OpType_obj;
      }
      SET_OPCODE(*i, set_in_arith(op, type));
      break;
    }
    case OP_UNM: {
      OpType type = OpType_raw;
      if (ttisnumber(rb)) {
        if (_add_guards(b, OpType_num)) {
          type = OpType_num;
          if (opout(op) == OpType_num)
            op = set_out_unm(op, OpType_raw);
          else if (opout(op) == OpType_str || opout(op) == OpType_obj)
            luaVS_despecialize(L, a);          
        }
      }
      SET_OPCODE(*i, set_in_unm(op, type));
      break;
    }
    case OP_LEN: {
      OpType type = OpType_raw;
      if (ttisstring(rb)) {
        if (_add_guards(b, OpType_str)) {
          type = OpType_str;
          if (opout(op) == OpType_num)
            op = set_out_unm(op, OpType_raw);
          else if (opout(op) == OpType_str || opout(op)== OpType_obj)
            luaVS_despecialize(L, a);
        }
      }
      SET_OPCODE(*i, set_in_len(op, type));
      break;
    }
    case OP_LT: case OP_LE: {
      OpType type = OpType_raw;
      if (ttisequal(rb, rc)) {
        if (ttisnumber(rb)) type = OpType_num;
        else if (ttisstring(rb)) type = OpType_str;
      }
      if (type != OpType_raw) {
        int status = 1;
        if (!ISK(b)) status = _add_guards(b, type);
        if (status && !ISK(c)) {
          if (!_add_guards(c, OpType_num)) {
            _remove_guards(b);
            status = 0;
          }
        }
        if (!status) type = OpType_raw;
      }
      SET_OPCODE(*i, set_in_less(op, type));
      break;
    }
    default:
      lua_assert(0);
      break;
  }

#ifdef DEBUG_PRINT
  printf(" --> ");
  printop(GET_OPCODE(*i));
  printf("\n");
#endif
}


void luaVS_despecialize (lua_State *L, int reg) {
  Proto *p = clLvalue(L->ci->func)->p;
  int pc = pcRel(L->ci->u.l.savedpc, p);
#ifdef DEBUG_PRINT
  printf("%s [%i] %i\n",__func__,pc,reg);
#endif
  RegInfo *reginfo = findreginfo(p, pc, reg, REGINFO_USE_STORE);
  lua_assert(reginfo != NULL);
  despecialize_all(p, reg, reginfo);
  remove_guards(p, reg, reginfo);
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
  lua_assert(reginfo != NULL);
  while (chaini-- > 0) reginfo = reginfo->next;
  lua_assert(reginfo->state == REGINFO_STATE_LOCAL_CLOSED);
  despecialize_all(p, desc.idx, reginfo);
  remove_guards(p, desc.idx, reginfo);
}
