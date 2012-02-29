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



static RegInfo *findreginfo (Proto *p, int pc, int reg, int use) {
  lua_assert(reg < p->sizereginfos);
  RegInfo *reginfo = &(p->reginfos[reg]);
  while (reginfo) {
    if (reginfo->state == RI_UNUSED) return NULL;
    if (reginfo->state == RI_LOCAL_UNUSED) return NULL;
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


/* ==== removing guards =================================================== */

static void remove_guard_upvalue_use (Proto *p, int pc, int idx) {
  Instruction *i = &(p->code[pc]);
  if (GET_OPGROUP(*i) == OP_SETUPVAL && GETARG_B(*i) == idx)
    MODIFY_OPCODE_GUARD(*i, OpType_raw);
}


static void remove_guards_upvalue_use (Proto *p, int outer_idx) {
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


static void remove_guard (Proto *p, int pc, int reg) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
  switch (op2grp(op)) {
    case OP_MOVE: case OP_GETUPVAL:
    case OP_GETTABLE: case OP_GETTABUP:
    case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: 
    case OP_MOD: case OP_POW: case OP_UNM: case OP_LEN:
    case OP_CONCAT: case OP_TESTSET: case OP_CHKTYPE:
      if (GETARG_A(*i) == reg)
        MODIFY_OPCODE_GUARD(*i, OpType_raw);
    default: return;
  }
}


static void remove_guards (Proto *p, int reg, RegInfo *reginfo) {
  int pc = reginfo->startpc;
  if (reginfo->firstuse & RI_STORE) remove_guard(p, pc, reg);
  while (++pc < reginfo->endpc)     remove_guard(p, pc, reg);
  if (reginfo->lastuse & RI_STORE)  remove_guard(p, pc, reg);

  /* remove guards from upvalue uses of reg in enclosed functions */
  int n = p->sizep;
  while (n > 0)
    remove_guards_upvalue_use(p->p[--n], reg);
}


/* ==== adding guards ===================================================== */

static int add_guard_upvalue_use (Proto *p, int pc, int idx, OpType guard) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
  if (op2grp(op) == OP_SETUPVAL && GETARG_B(*i) == idx) {
      OpType spec = opspec(op);
      if (spec != OpType_raw && spec != OpType_chk) {
        if (spec != guard) return 0;
        else guard = OpType_raw;
      }
      MODIFY_OPCODE_GUARD(*i, guard);
  }
  return 1;
}


static int add_guards_upvalue_use (Proto *p, int outer_idx, OpType guard) {
  /* find the upvalue in this function's list */
  int idx = findupval(p, outer_idx);
  if (idx < 0) return 1;
  Upvaldesc desc = p->upvalues[idx];

  /* add guards to upvalue usage in this function */
  int pc = desc.startpc;
  while (pc <= desc.endpc) {
    if (!add_guard_upvalue_use(p, pc++, idx, guard)) goto fail;
  }

  /* add guards to upvalue usage in enclosed functions */
  int n = p->sizep;
  while (n > 0) {
    if (!add_guards_upvalue_use(p->p[--n], idx, guard)) goto fail;
  }

  return 1;

fail:
  remove_guards_upvalue_use(p, outer_idx);
  return 0;
}


static int add_guard (Proto *p, int pc, int reg, OpType guard) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
  int a = GETARG_A(*i);
  switch (op2grp(op)) {
/* ------------------------------------------------------------------------ */
    case OP_MOVE:
      if (a == reg) {
        OpType spec = opspec(op);
        if (spec != OpType_raw && spec != OpType_chk) {
          if (spec != guard) return 0;
          else guard = OpType_raw;
        }
        MODIFY_OPCODE_GUARD(*i, guard);
      }
      break;
/* ------------------------------------------------------------------------ */
    case OP_LOADK: case OP_LOADKX:
      return !(a == reg && opspec(op) != guard);
/* ------------------------------------------------------------------------ */
    case OP_LOADBOOL: case OP_CLOSURE:
      return !(a == reg);
/* ------------------------------------------------------------------------ */
    case OP_LOADNIL:
      return !(a <= reg && reg <= a+GETARG_B(*i));
/* ------------------------------------------------------------------------ */
    case OP_NEWTABLE:
      return !(a == reg && guard != OpType_tab);
/* ------------------------------------------------------------------------ */
    case OP_GETUPVAL: case OP_GETTABLE: case OP_GETTABUP:
    case OP_CONCAT: case OP_TESTSET: case OP_CHKTYPE:
      if (a == reg) 
        MODIFY_OPCODE_GUARD(*i, guard);
      break;
/* ------------------------------------------------------------------------ */
    case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: 
    case OP_MOD: case OP_POW: case OP_UNM:
      if (a == reg) {
        if (opspec(op) == OpType_num) {
          if (guard != OpType_num) return 0;
          else guard = OpType_raw;
        }
        MODIFY_OPCODE_GUARD(*i, guard);
      }
      break;
/* ------------------------------------------------------------------------ */
    case OP_LEN:
      if (a == reg) {
        if (opspec(op) == OpType_str || opspec(op) == OpType_tab) {
          if (guard != OpType_num) return 0;
          else guard = OpType_raw;
        }
        MODIFY_OPCODE_GUARD(*i, guard);
      }
      break;
/* ------------------------------------------------------------------------ */
    default:
      break;
  }
  return 1;
}


static int add_guards (Proto *p, int reg, RegInfo *reginfo, OpType guard) {
  lua_assert(reginfo != NULL);
  lua_assert(guard != OpType_raw && guard != OpType_chk);

  int pc = reginfo->startpc;
  if (reginfo->firstuse & RI_STORE)
    if (!add_guard(p, pc, reg, guard)) goto fail;

  while (++pc < reginfo->endpc)
    if (!add_guard(p, pc, reg, guard)) goto fail;
  
  if (reginfo->lastuse & RI_STORE)
    if (!add_guard(p, pc, reg, guard)) goto fail;

  /* try to add guards to all upvalue uses of reg in enclosed functions */
  int n = p->sizep;
  while (n > 0)
    if (!add_guards_upvalue_use(p->p[--n], reg, guard)) goto fail;

  return 1;

fail:
  remove_guards(p, reg, reginfo);
  return 0;
}


static int add_guards_upvalue_origin (Proto *p, Upvaldesc desc, OpType guard) {
  /* find the function that has the local that is the origin of the upvalue */
  for (;;) {
    p = p->encp;
    if (p == NULL || desc.instack) break;
    else desc = p->upvalues[desc.idx];
  }
  if (p == NULL) return 1;
  /* find the right reginfo for the local */
  RegInfo *reginfo = findreginfo(p, desc.regpc, desc.idx, RI_STORE);
  if (reginfo == NULL) return 1;
  return add_guards(p, desc.idx, reginfo, guard);
}


/* ==== despecializing ==================================================== */

static void despecialize_all (lua_State *L, Proto *p, int reg, 
                              RegInfo *reginfo, int *visited);


static void despecialize (lua_State *L, Proto *p, int pc, int reg, 
                          int *visited) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
  int a = GETARG_A(*i);
  int b = GETARG_B(*i);
  int c = GETARG_C(*i);
  switch (op2grp(op)) {
/* ------------------------------------------------------------------------ */
    case OP_MOVE: case OP_SELF: case OP_UNM: case OP_LEN:
      if (b == reg) {
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
        RegInfo *outreg = findreginfo(p, pc, a, RI_STORE);
        despecialize_all(L, p, a, outreg, visited);
      }
      break;
/* ------------------------------------------------------------------------ */
    case OP_GETTABLE:
      if ((!ISK(c) && c == reg) || (b == reg))
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
/* ------------------------------------------------------------------------ */
    case OP_GETTABUP:
      if (!ISK(c) && c == reg)
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
/* ------------------------------------------------------------------------ */
    case OP_SETTABLE:
      if ((!ISK(b) && b == reg) || (a == reg))
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
/* ------------------------------------------------------------------------ */
    case OP_SETTABUP:
      if (!ISK(b) && b == reg)
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
/* ------------------------------------------------------------------------ */
    case OP_SETUPVAL:
      if (a == reg) {
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
        SETARG_A(*i, NO_REG); // HACK to prevent loops
        luaVS_despecialize_upvalue_origin(L, p, b);
        SETARG_A(*i, a);
      }
      break;
/* ------------------------------------------------------------------------ */
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW:
      if ((!ISK(b) && b == reg) || (!ISK(c) && c == reg)) {
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
        RegInfo *outreg = findreginfo(p, pc, a, RI_STORE);
        despecialize_all(L, p, a, outreg, visited);
      }
      break;
/* ------------------------------------------------------------------------ */
    case OP_EQ: case OP_LT: case OP_LE:
      if ((!ISK(b) && b == reg) || (!ISK(c) && c == reg))
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
/* ------------------------------------------------------------------------ */
    default: 
      return;
  }
}


static void despecialize_upvalue (lua_State *L, Proto *p, int pc, int idx, 
                                int *visited) {
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
  int a = GETARG_A(*i);
  int b = GETARG_B(*i);
  switch (op2grp(op)) {
/* ------------------------------------------------------------------------ */
    case OP_GETUPVAL:
      if (b == idx) {
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
        RegInfo *outreg = findreginfo(p, pc, a, RI_STORE);
        despecialize_all(L, p, a, outreg, visited);
      }
      break;
/* ------------------------------------------------------------------------ */
    case OP_GETTABUP:
      if (b == idx)
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
/* ------------------------------------------------------------------------ */
    case OP_SETTABUP:
      if (a == idx)
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
/* ------------------------------------------------------------------------ */
    default:
      return;
  }
}


static void despecialize_all (lua_State *L, Proto *p, int reg, 
                              RegInfo *reginfo, int *visited) {
  lua_assert(reginfo != NULL);
  lua_assert(visited != NULL);

  if (visited[reg]) return; /* avoid loop */
  visited[reg] = 1;

  int pc = reginfo->startpc;
  if (reginfo->firstuse & RI_LOAD)  despecialize(L, p, pc, reg, visited);
  while (++pc < reginfo->endpc)     despecialize(L, p, pc, reg, visited);
  if (reginfo->lastuse & RI_LOAD)   despecialize(L, p, pc, reg, visited);

  remove_guards(p, reg, reginfo);
}


static void despecialize_all_upvalues (lua_State *L, Proto *p, int outer_idx) {
  /* find the upvalue in this function's list */
  int idx = findupval(p, outer_idx);
  if (idx < 0) return;
  Upvaldesc desc = p->upvalues[idx];

  /* despecialize upval uses in this function */
  if (desc.startpc != -1) {
    lua_assert(desc.endpc != -1);

    int n = p->sizereginfos;
    int *visited = luaM_newvector(L, n, int);
    while (n > 0) visited[--n] = 0;

    int pc = desc.startpc;
    while (pc <= desc.endpc) 
      despecialize_upvalue(L, p, pc++, idx, visited);

    luaM_freearray(L, visited, p->sizereginfos);
  }

  /* despecialize uses in enclosed functions */
  int n = p->sizep;
  while (n > 0) {
    despecialize_all_upvalues(L, p->p[--n], idx);
  }
}


void luaVS_despecialize (lua_State *L, Proto *p, int pc, int reg) {
  RegInfo *reginfo = findreginfo(p, pc, reg, RI_STORE);
  lua_assert(reginfo != NULL);
  
  int n = p->sizereginfos;
  int *visited = luaM_newvector(L, n, int);
  while (n > 0) visited[--n] = 0;

  despecialize_all(L, p, reg, reginfo, visited);
  
  luaM_freearray(L, visited, p->sizereginfos);

  n = p->sizep;
  while (n > 0)
    despecialize_all_upvalues(L, p->p[--n], reg);
}


void luaVS_despecialize_upvalue_origin (lua_State *L, Proto *p, int idx) {
  /* find the function that has the local that is the origin of the upvalue */
  Upvaldesc desc = p->upvalues[idx];
  for (;;) {
    p = p->encp;
    if (desc.instack) break;
    else desc = p->upvalues[desc.idx];
  }
  if (p == NULL) return;

  luaVS_despecialize(L, p, desc.regpc, desc.idx);
}



/* ==== specializing ====================================================== */

#define _add_guards_upvalue_origin(idx,t) \
  (add_guards_upvalue_origin(p, p->upvalues[idx], t))

#define _add_guards(r,t) \
  (add_guards(p, r, findreginfo(p, pc, r, RI_LOAD), t))

#define _remove_guards(r) \
  (remove_guards(p, r, findreginfo(p, pc, r, RI_LOAD)))

void luaVS_specialize (lua_State *L, Proto *p, int pc) {
  CallInfo *ci = L->ci;
  LClosure *cl = clLvalue(ci->func);
  StkId base = ci->u.l.base;
  Instruction *i = &(p->code[pc]);
  OpCode op = GET_OPCODE(*i);
  OpType spec = opspec(op);
  lua_assert(spec == OpType_chk);
  int a = GETARG_A(*i);
  int b = GETARG_B(*i);
  int c = GETARG_C(*i);
  TValue *ra = base+a;
  TValue *rb = ISK(b) ? p->k+INDEXK(b) : base+b;
  TValue *rc = ISK(c) ? p->k+INDEXK(c) : base+c;
  switch (op2grp(op)) {
/* ------------------------------------------------------------------------ */
    case OP_MOVE: {
      OpType guard = opguard(op);
      if (ttisnumber(rb))       spec = OpType_num;
      else if (ttisstring(rb))  spec = OpType_str;
      else if (ttistable(rb))   spec = OpType_tab;
      if (spec != OpType_chk && _add_guards(b, spec)) {
        if (guard != OpType_raw && guard != spec)
          luaVS_despecialize(L, p, pc, a);
        guard = OpType_raw;
      }
      MODIFY_OPCODE(*i, guard, OpType_raw);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_GETUPVAL: {
      OpType guard = opguard(op);
      rb = cl->upvals[b]->v;      
      if (ttisnumber(rb))       spec = OpType_num;
      else if (ttisstring(rb))  spec = OpType_str;
      else if (ttistable(rb))   spec = OpType_tab;
      if (spec != OpType_chk && _add_guards_upvalue_origin(b, spec)) {
        if (guard != OpType_raw && guard != spec)
          luaVS_despecialize(L, p, pc, a);
        guard = OpType_raw;
      }
      MODIFY_OPCODE(*i, guard, OpType_raw);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_GETTABLE: {
      spec = OpType_raw;
      if (ttistable(rb)) {
        if (ttisnumber(rc))       spec = OpType_num;
        else if (ttisstring(rc))  spec = OpType_str;
      }
      if (spec != OpType_raw) {
        int safe = 1;
        if (!ISK(c)) safe = _add_guards(c, spec);
        if (safe && !_add_guards(b, OpType_tab)) {
          if (!ISK(c)) _remove_guards(c);
          safe = 0;
        }
        if (!safe) spec = OpType_raw;
      }
      MODIFY_OPCODE_SPEC(*i, spec);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_GETTABUP: {
      spec = OpType_raw;
      rb = cl->upvals[b]->v;
      if (ttistable(rb)) {
        if (ttisnumber(rc))       spec = OpType_num;
        else if (ttisstring(rc))  spec = OpType_str;
      }
      if (spec != OpType_raw) { 
        int safe = 1;
        if (!ISK(c)) safe = _add_guards(c, spec);
        if (safe && !_add_guards_upvalue_origin(b, OpType_tab)) {
          if (!ISK(c)) _remove_guards(c);
          safe = 0;
        }
        if (!safe) spec = OpType_raw;
      }
      MODIFY_OPCODE_SPEC(*i, spec);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_SETTABLE: {
      spec = OpType_raw;
      if (ttistable(ra)) {
        if (ttisnumber(rb))       spec = OpType_num;
        else if (ttisstring(rb))  spec = OpType_str;
      }
      if (spec != OpType_raw) {
        int safe = 1;
        if (!ISK(b)) safe = _add_guards(b, spec);
        if (safe && !_add_guards(a, OpType_tab)) {
          if (!ISK(b)) _remove_guards(b);
          safe = 0;
        }
        if (!safe) spec = OpType_raw;
      }
      MODIFY_OPCODE_SPEC(*i, spec);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_SETTABUP: {
      spec = OpType_raw;
      ra = cl->upvals[a]->v;
      if (ttistable(ra)) {
        if (ttisnumber(rb))       spec = OpType_num;
        else if (ttisstring(rb))  spec = OpType_str;
      }
      if (spec != OpType_raw) {
        int safe = 1;
        if (!ISK(b)) safe = _add_guards(b, spec);
        if (safe && !_add_guards_upvalue_origin(a, OpType_tab)) {
          if (!ISK(b)) _remove_guards(b);
          safe = 0;
        }
        if (!safe) spec = OpType_raw;
      }
      MODIFY_OPCODE_SPEC(*i, spec);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_SETUPVAL: {
      OpType guard = opguard(op);
      spec = OpType_raw;
      if (ttisnumber(ra))       spec = OpType_num;
      else if (ttisstring(ra))  spec = OpType_str;
      else if (ttistable(ra))   spec = OpType_tab;
      if (spec != OpType_raw && _add_guards(a, spec)) {
        if (guard != OpType_raw && guard != spec)
          luaVS_despecialize_upvalue_origin(L, p, b);
        guard = OpType_raw;
      }      
      MODIFY_OPCODE(*i, guard, spec);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_SELF: {
      if (ttistable(rb) && _add_guards(b, OpType_tab))
        MODIFY_OPCODE_SPEC(*i, OpType_tab);
      else
        MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_ADD: case OP_SUB: case OP_MUL:
    case OP_DIV: case OP_MOD: case OP_POW: {
      if (ttisnumber(rb) && ttisnumber(rc)) {
        int safe = 1;
        if (!ISK(b)) safe = _add_guards(b, OpType_num);
        if (safe && !ISK(c) && !_add_guards(c, OpType_num)) {
          if (!ISK(b)) _remove_guards(b);
          safe = 0;
        }
        if (safe) {          
          if (opguard(op) != OpType_raw && opguard(op) != OpType_num)
            luaVS_despecialize(L, p, pc, a);
          MODIFY_OPCODE(*i, OpType_raw, OpType_num);
          break;
        }
      }
      MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_UNM: {
      if (ttisnumber(rb) && _add_guards(b, OpType_num)) {
        if (opguard(op) != OpType_raw && opguard(op) != OpType_num)
          luaVS_despecialize(L, p, pc, a);
        MODIFY_OPCODE(*i, OpType_raw, OpType_num);
        break;
      }
      MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_LEN: {
      if (ttisstring(rb)) spec = OpType_str;
      if (ttistable(rb))  spec = OpType_tab;
      if (spec != OpType_chk && _add_guards(b, spec)) {
        if (opguard(op) != OpType_raw && opguard(op) != OpType_num)
          luaVS_despecialize(L, p, pc, a);
        MODIFY_OPCODE(*i, OpType_raw, spec);
        break;
      }
      MODIFY_OPCODE_SPEC(*i, OpType_raw);
      break;
    }
/* ------------------------------------------------------------------------ */
    case OP_EQ:
    case OP_LT: case OP_LE: {
      spec = OpType_raw;
      if (ttisequal(rb, rc)) {
        if (ttisnumber(rb))       spec = OpType_num;
        else if (ttisstring(rb))  spec = OpType_str;
        else if (ttistable(rb))   spec = OpType_tab;
      }
      if (spec != OpType_raw) {
        int safe = 1;
        if (!ISK(b)) safe = _add_guards(b, spec);
        if (safe && !ISK(c) && !_add_guards(c, spec)) {
          if (!ISK(b)) _remove_guards(b);
          safe = 0;
        }
        if (!safe) spec = OpType_raw;
      }
      MODIFY_OPCODE_SPEC(*i, spec);
      break;
    }
/* ------------------------------------------------------------------------ */
    default:
      lua_assert(0);
      break;
  }
}

