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


static int findupval (Proto *p, int outer_idx) {
  int idx = 0;
  while (idx < p->sizeupvalues) {    
    if (p->upvalues[idx].idx == outer_idx)
      return idx;
    idx++;
  }
  return -1;
}


static int *init_visited (lua_State *L, Proto *p) {
  int n = p->sizereginfos;
  int *visited = luaM_newvector(L, n, int);
  while (n > 0) visited[--n] = 0;
  return visited;
}


static void free_visited (lua_State *L, Proto *p, int *visited) {
  luaM_freearray(L, visited, p->sizereginfos);
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
      if (a == reg && type != OpType_tab) return 0;
      break;
    case OP_CLOSURE:
      if (a == reg) return 0;
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
        if (opin(op) == OpType_str || opin(op) == OpType_tab) {
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

static void remove_guard_upvalue_use (Proto *p, int pc, int idx) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
#ifdef DEBUG_PRINT
  printf("\t%s [%i] %i ",__func__,pc,idx);
  printop(op);
#endif
  switch (op2grp(op)) {
    case OP_SETUPVAL:
      if (GETARG_B(*i) == idx) SET_OPCODE(*i, set_out_setupval(op, OpType_raw));
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

static void remove_guards_upvalue_use (Proto *p, int outer_idx) {
#ifdef DEBUG_PRINT
  printf("\t%s %i\n",__func__,outer_idx);
#endif  
  /* find the upvalue in this function's list */
  int idx = findupval(p, outer_idx);
  if (idx < 0) return;
  Upvaldesc desc = p->upvalues[idx];

  /* remove guards for this function */
  int pc = desc.startpc;
  while (pc <= desc.endpc) remove_guard_upvalue_use(p, pc++, idx);

  /* remove guards for enclosed functions */
  int n = p->sizep;
  while (n > 0)
    remove_guards_upvalue_use(p->p[--n], idx);
}


static void remove_guards (Proto *p, int reg, RegInfo *reginfo) {
#ifdef DEBUG_PRINT
  printf("\t%s %i\n",__func__,reg);
#endif
  int pc = reginfo->startpc;
  if (reginfo->firstuse & REGINFO_USE_STORE) remove_guard(p, pc, reg);
  while (++pc < reginfo->endpc)              remove_guard(p, pc, reg);
  if (reginfo->lastuse & REGINFO_USE_STORE)  remove_guard(p, pc, reg);

  /* remove guards from upvalue uses of reg in enclosed functions */
  int n = p->sizep;
  while (n > 0)
    remove_guards_upvalue_use(p->p[--n], reg);
}



static int add_guard_upvalue_use (Proto *p, int pc, int idx, OpType type) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
#ifdef DEBUG_PRINT
  printf("\t%s [%i] %i %i ",__func__,pc,idx,type);
  printop(op);
#endif
  switch (op2grp(op)) {
    case OP_SETUPVAL:
      if (GETARG_B(*i) == idx) {
        int intype = opin(op);
        if (intype == OpType_raw || intype == OpType_chk)
          SET_OPCODE(*i, set_out_setupval(op, type));
        else if (intype == type)
          SET_OPCODE(*i, set_out_setupval(op, OpType_raw));
        else
          return 0;
      }
      break;
    default:
      return 1;
  }
  return 1;
}

static int add_guards_upvalue_use (Proto *p, int outer_idx, OpType type) {
#ifdef DEBUG_PRINT
  printf("\t%s %i %i\n",__func__,outer_idx,type);
#endif  
  /* find the upvalue in this function's list */
  int idx = findupval(p, outer_idx);
  if (idx < 0) return 1;
  Upvaldesc desc = p->upvalues[idx];

  /* add guards to upvalue usage in this function */
  int pc = desc.startpc;
  while (pc <= desc.endpc) {
    if (!add_guard_upvalue_use(p, pc++, idx, type)) {
      remove_guards_upvalue_use(p, outer_idx);
      return 0;
    }
  }

  /* add guards to upvalue usage in enclosed functions */
  int n = p->sizep;
  while (n > 0) {
    if (!add_guards_upvalue_use(p->p[--n], idx, type)) {
      remove_guards_upvalue_use(p, outer_idx);
      return 0;
    }
  }

  return 1;
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

  int pc = reginfo->startpc;
  if (reginfo->firstuse & REGINFO_USE_STORE) _add_guard_or_abort
  while (++pc < reginfo->endpc)              _add_guard_or_abort
  if (reginfo->lastuse & REGINFO_USE_STORE)  _add_guard_or_abort

  /* try to add guards to all upvalue uses of reg in enclosed functions */
  int n = p->sizep;
  while (n > 0) {
    if (!add_guards_upvalue_use(p->p[--n], reg, type)) {
      remove_guards(p, reg, reginfo);
      return 0;
    }
  }

  return 1;
}






static void despecialize_all (lua_State *L, Proto *p, int reg, RegInfo *reginfo, 
                              int *visited);

static void despecialize_store (lua_State *L, Proto *p, int pc, int a, int *visited) {  
#ifdef DEBUG_PRINT
  printf("\n\t\t%s [%i] %i\n",__func__,pc,a);
#endif
  if (visited[a]) return; /* avoid loop */
  RegInfo *reginfo = findreginfo(p, pc, a, REGINFO_USE_STORE);
  lua_assert(reginfo != NULL);    
  despecialize_all(L, p, a, reginfo, visited);
  remove_guards(p, a, reginfo);
}


static void despecialize (lua_State *L, Proto *p, int pc, int reg, int *visited) {
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
        despecialize_store(L, p, pc, a, visited);
      }
      break;
    case OP_GETTABLE:
      if ((!ISK(c) && c == reg) || (b == reg)) {
        SET_OPCODE(*i, set_in_gettab(op, OpType_raw));
      }
      break;
    case OP_GETTABUP:
      if (!ISK(c) && c == reg) {
        SET_OPCODE(*i, set_in_gettab(op, OpType_raw));
      }
      break;
    case OP_SETTABLE:
      if ((!ISK(b) && b == reg) || (a == reg)) {
        SET_OPCODE(*i, set_in_settab(op, OpType_raw));
      }
    case OP_SETTABUP:
      if (!ISK(b) && b == reg) {
        SET_OPCODE(*i, set_in_settab(op, OpType_raw));
      }
      break;
    case OP_SETUPVAL:
      if (a == reg) {
        SET_OPCODE(*i, set_in_setupval(op, OpType_raw));
        SETARG_A(*i, NO_REG); // TODO: hack to prevent loops
        luaVS_despecialize_upval_origin(L, p, b);
        SETARG_A(*i, a);
      }
      break;
    case OP_SELF:
      if (b == reg) {
        SET_OPCODE(*i, OP(SELF,___,___));
      }
      break;
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW:
      if ((!ISK(b) && b == reg) || (!ISK(c) && c == reg)) {
        SET_OPCODE(*i, set_in_arith(op, OpType_raw));
        despecialize_store(L, p, pc, a, visited);
      }
      break;
    case OP_UNM:
      if (b == reg) {
        SET_OPCODE(*i, set_in_unm(op, OpType_raw));
        despecialize_store(L, p, pc, a, visited);
      }
      break;
    case OP_LEN:
      if (b == reg) {
        SET_OPCODE(*i, set_in_len(op, OpType_raw));
        despecialize_store(L, p, pc, a, visited);
      }
      break;
    case OP_EQ:
    case OP_LT: case OP_LE:
      if ((!ISK(b) && b == reg) || (!ISK(c) && c == reg)) {
        SET_OPCODE(*i, set_in_cmp(op, OpType_raw));
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


static void despecialize_all (lua_State *L, Proto *p, int reg, RegInfo *reginfo, 
                              int *visited) {
#ifdef DEBUG_PRINT
  printf("%s %p reg=%i\n",__func__,p,reg);
#endif
  visited[reg] = 1;
  int pc = reginfo->startpc;
  if (reginfo->firstuse & REGINFO_USE_LOAD) despecialize(L, p, pc, reg, visited);
  while (++pc < reginfo->endpc)             despecialize(L, p, pc, reg, visited);
  if (reginfo->lastuse & REGINFO_USE_LOAD)  despecialize(L, p, pc, reg, visited);
}


static void despecialize_upval (lua_State *L, Proto *p, int pc, int idx, 
                                int *visited) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
#ifdef DEBUG_PRINT
  printf("\t%s [%i] %i ",__func__,pc,idx);
  printop(op);
#endif
  int a = GETARG_A(*i);
  int b = GETARG_B(*i);
  switch (op2grp(op)) {
    case OP_GETUPVAL:
      if (b == idx) {
        SET_OPCODE(*i, set_in_getupval(op, OpType_raw));
        despecialize_store(L, p, pc, a, visited);
      }
      break;
    case OP_GETTABUP:
      if (b == idx) {
        SET_OPCODE(*i, set_in_gettab(op, OpType_raw));
      }
      break;
    case OP_SETTABUP:
      if (a == idx) {
        SET_OPCODE(*i, set_in_settab(op, OpType_raw));
      }
      break;
    default:
      return;
  }
}

static void despecialize_all_upvals (lua_State *L, Proto *p, int outer_idx) {
#ifdef DEBUG_PRINT
  printf("\t%s %i\n",__func__,outer_idx);
#endif  
  /* find the upvalue in this function's list */
  int idx = findupval(p, outer_idx);
  if (idx < 0) return;
  Upvaldesc desc = p->upvalues[idx];

  /* despecialize upval uses in this function */
  if (desc.startpc != -1) {
    lua_assert(desc.endpc != -1);
    int *visited = init_visited(L, p);
    int pc = desc.startpc;
    while (pc <= desc.endpc) despecialize_upval(L, p, pc++, idx, visited);
    free_visited(L, p, visited);
  }

  /* despecialize uses in enclosed functions */
  int n = p->sizep;
  while (n > 0) {
    despecialize_all_upvals(L, p->p[--n], idx);
  }
}



static int add_guards_upval_origin (Proto *p, Upvaldesc desc, OpType type) {
#ifdef DEBUG_PRINT
  printf("\t%s %i %i\n",__func__,desc.idx,type);
#endif  
  /* find the function that has the local that is the origin of the upvalue */
  for (;;) {
    p = p->encp;
    if (p == NULL || desc.instack) break;
    else desc = p->upvalues[desc.idx];
  }
  if (p == NULL) return 1;
  /* find the right reginfo for the local */
  RegInfo *reginfo = findreginfo(p, desc.regpc, desc.idx, 
                                 REGINFO_USE_STORE | REGINFO_USE_LOAD);
  if (reginfo == NULL) return 1;
  return add_guards(p, desc.idx, reginfo, type);
}

#define _add_guards_upval_origin(idx,t) \
  (add_guards_upval_origin(p, p->upvalues[idx], t))

#define _add_guards(r,t) \
  (add_guards(p, r, findreginfo(p, pc, r, REGINFO_USE_LOAD), t))

#define _remove_guards(r) \
  (remove_guards(p, r, findreginfo(p, pc, r, REGINFO_USE_LOAD)))


void luaVS_specialize (lua_State *L) {
  CallInfo *ci = L->ci;
  LClosure *cl = clLvalue(ci->func);
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
  TValue *ra = base+a;
  TValue *rb = ISK(b) ? p->k+INDEXK(b) : base+b;
  TValue *rc = ISK(c) ? p->k+INDEXK(c) : base+c;
  switch (op2grp(op)) {
    case OP_MOVE: {
      OpType type = OpType_raw;
      if (ttisnumber(rb)) type = OpType_num;
      else if (ttisstring(rb)) type = OpType_str;
      else if (ttistable(rb)) type = OpType_tab;
      if (_add_guards(b, type)) {
        if (opout(op) != OpType_raw && opout(op) != type)
          luaVS_despecialize(L, a);
        SET_OPCODE(*i, create_op_move(type, OpType_raw));
      } else {
        SET_OPCODE(*i, set_in_move(op, OpType_raw));
      }
      break;
    }
    case OP_GETUPVAL: {
      OpType type = OpType_raw;
      rb = cl->upvals[b]->v;
      if (ttisnumber(rb)) type = OpType_num;
      else if (ttisstring(rb)) type = OpType_str;
      else if (ttistable(rb)) type = OpType_tab;
      if (_add_guards_upval_origin(b, type)) {
        if (opout(op) != OpType_raw && opout(op) != type)
          luaVS_despecialize(L, a);
        SET_OPCODE(*i, create_op_getupval(type, OpType_raw));
      } else {
        SET_OPCODE(*i, set_in_getupval(op, OpType_raw));
      }
      break;
    }
    case OP_GETTABLE: {
      OpType type = OpType_raw;
      if (ttistable(rb)) {
        if (ttisnumber(rc)) type = OpType_num;
        else if (ttisstring(rc)) type = OpType_str;
      }
      if (type != OpType_raw) {        
        int status = 1;
        if (!ISK(c)) status = _add_guards(c, type);
        if (status) {
          if (!_add_guards(b, OpType_tab)) {
            if (!ISK(c)) _remove_guards(c);
            status = 0;
          }
        }
        if (!status)
          type = OpType_raw;
      }
      SET_OPCODE(*i, set_in_gettab(op, type));
      break;
    }
    case OP_GETTABUP: {
      OpType type = OpType_raw;
      rb = cl->upvals[b]->v;
      if (ttistable(rb)) {
        if (ttisnumber(rc)) type = OpType_num;
        else if (ttisstring(rc)) type = OpType_str;
      }
      if (type != OpType_raw) {        
        int status = 1;
        if (!ISK(c)) status = _add_guards(c, type);
        if (status) {
          if (!_add_guards_upval_origin(b, OpType_tab)) {
            if (!ISK(c)) _remove_guards(c);
            status = 0;
          }
        }
        if (!status)
          type = OpType_raw;
      }
      SET_OPCODE(*i, set_in_gettab(op, type));
      break;
    }
    case OP_SETTABLE: {
      OpType type = OpType_raw;
      if (ttistable(ra)) {
        if (ttisnumber(rb)) type = OpType_num;
        else if (ttisstring(rb)) type = OpType_str;
      }
      if (type != OpType_raw) {
        int status = 1;
        if (!ISK(b)) status = _add_guards(b, type);
        if (status) {
          if (!_add_guards(a, OpType_tab)) {
            if (!ISK(b)) _remove_guards(b);
            status = 0;
          }
        }
        if (!status)
          type = OpType_raw;
      }
      SET_OPCODE(*i, set_in_settab(op, type));
      break;
    }
    case OP_SETTABUP: {
      OpType type = OpType_raw;
      ra = cl->upvals[a]->v;
      if (ttistable(ra)) {
        if (ttisnumber(rb)) type = OpType_num;
        else if (ttisstring(rb)) type = OpType_str;
      }
      if (type != OpType_raw) {
        int status = 1;
        if (!ISK(b)) status = _add_guards(b, type);
        if (status) {
          if (!_add_guards_upval_origin(a, OpType_tab)) {
            if (!ISK(b)) _remove_guards(b);
            status = 0;
          }
        }
        if (!status)
          type = OpType_raw;
      }
      SET_OPCODE(*i, set_in_settab(op, type));
      break;
    }
    case OP_SETUPVAL: {
      OpType type = OpType_raw;
      if (ttisnumber(ra)) type = OpType_num;
      else if (ttisstring(ra)) type = OpType_str;
      else if (ttistable(ra)) type = OpType_tab;
      if (_add_guards(a, type)) {
        if (opout(op) != OpType_raw && opout(op) != type)
          luaVS_despecialize_upval_origin(L, p, b);
        SET_OPCODE(*i, create_op_setupval(type, OpType_raw));
      } else {
        SET_OPCODE(*i, set_in_setupval(op, OpType_raw));
      }
      break;
    }
    case OP_SELF: {
      if (ttistable(rb) && _add_guards(b, OpType_tab))
        SET_OPCODE(*i, OP(SELF,___,tab));
      else
        SET_OPCODE(*i, OP(SELF,___,___));
      break;
    }
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW: {
      OpType type = OpType_raw;
      if (ttisnumber(rb) && ttisnumber(rc)) {
        int status = 1;
        if (!ISK(b)) status = _add_guards(b, OpType_num);
        if (status && !ISK(c)) {
          if (!_add_guards(c, OpType_num)) {
            if (!ISK(b)) _remove_guards(b);
            status = 0;
          }
        }
        if (status) {
          type = OpType_num;
          if (opout(op) == OpType_num)
            op = set_out_arith(op, OpType_raw);
          else if (opout(op) == OpType_str)
            luaVS_despecialize(L, a);
        }
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
          else if (opout(op) == OpType_str)
            luaVS_despecialize(L, a);          
        }
      }
      SET_OPCODE(*i, set_in_unm(op, type));
      break;
    }
    case OP_LEN: {
      OpType type = OpType_raw;
      if (ttisstring(rb)) type = OpType_str;
      if (ttistable(rb)) type = OpType_tab;
      if (type != OpType_raw) {
        if (_add_guards(b, type)) {
          if (opout(op) == OpType_num)
            op = set_out_unm(op, OpType_raw);
          else if (opout(op) != OpType_raw)
            luaVS_despecialize(L, a);
        }
      }
      SET_OPCODE(*i, set_in_len(op, type));
      break;
    }
    case OP_EQ:
    case OP_LT: case OP_LE: {
      OpType type = OpType_raw;
      if (ttisequal(rb, rc)) {
        if (ttisnumber(rb)) type = OpType_num;
        else if (ttisstring(rb)) type = OpType_str;
        else if (ttistable(rb)) type = OpType_tab;
      }
      if (type != OpType_raw) {
        int status = 1;
        if (!ISK(b)) status = _add_guards(b, type);
        if (status && !ISK(c)) {
          if (!_add_guards(c, type)) {
            if (!ISK(b)) _remove_guards(b);
            status = 0;
          }
        }
        if (!status) type = OpType_raw;
      }
      SET_OPCODE(*i, set_in_cmp(op, type));
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
  int *visited = init_visited(L, p);
  despecialize_all(L, p, reg, reginfo, visited);
  free_visited(L, p, visited);
  remove_guards(p, reg, reginfo);

  int n = p->sizep;
  while (n > 0)
    despecialize_all_upvals(L, p->p[--n], reg);
}


void luaVS_despecialize_upval_origin (lua_State *L, Proto *p, int idx) {
#ifdef DEBUG_PRINT
  printf("%s %p %i\n", __func__, p, idx);
#endif  
  /* find the function that has the local that is the origin of the upvalue */
  Upvaldesc desc = p->upvalues[idx];
  for (;;) {
    p = p->encp;
    if (desc.instack) break;
    else desc = p->upvalues[desc.idx];
  }
  if (p == NULL) return;
  /* find the right reginfo for the local */
  RegInfo *reginfo = findreginfo(p, desc.regpc, desc.idx, 
                                 REGINFO_USE_STORE | REGINFO_USE_LOAD);
  lua_assert(reginfo != NULL);
  lua_assert(reginfo->state == REGINFO_STATE_LOCAL_CLOSED);
  int *visited = init_visited(L, p);
  despecialize_all(L, p, desc.idx, reginfo, visited);
  free_visited(L, p, visited);
  remove_guards(p, desc.idx, reginfo);

  int n = p->sizep;
  while (n > 0)
    despecialize_all_upvals(L, p->p[--n], desc.idx);
}
