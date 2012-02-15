/*
** $Id: lopcodes.c,v 1.48 2011/04/19 16:22:13 roberto Exp $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE


#include "lopcodes.h"

LUAI_DDEF OpGroup luaP_opcode2group[NUM_OPCODES] = {
#define OPENUM(op,out,in) OP_##op,
  OPDEF(OPENUM)
#undef OPENUM
};

LUAI_DDEF OpCode luaP_opgroup2code[NUM_OPGROUPS] = {
  OP(MOVE,raw,___),
  OP(LOADK,raw,___),
  OP(LOADKX,raw,___),
  OP(LOADBOOL,raw,___),
  OP(LOADNIL,raw,___),
  OP(GETUPVAL,raw,___),
  OP(GETTABUP,raw,raw),
  OP(GETTABLE,raw,raw),
  OP(SETTABUP,___,raw),
  OP(SETUPVAL,___,___),
  OP(SETTABLE,___,raw),
  OP(NEWTABLE,raw,___),
  OP(SELF,raw,___),
  OP(ADD,raw,raw),
  OP(SUB,raw,raw),
  OP(MUL,raw,raw),
  OP(DIV,raw,raw),
  OP(MOD,raw,raw),
  OP(POW,raw,raw),
  OP(UNM,raw,raw),
  OP(NOT,raw,___),
  OP(LEN,raw,raw),
  OP(CONCAT,raw,___),
  OP(JMP,___,___),
  OP(EQ,___,___),
  OP(LT,___,raw),
  OP(LE,___,raw),
  OP(TEST,___,___),
  OP(TESTSET,raw,___),
  OP(CALL,raw,___),
  OP(TAILCALL,___,___),
  OP(RETURN,___,___),
  OP(FORLOOP,___,___),
  OP(FORPREP,___,___),
  OP(TFORCALL,raw,___),
  OP(TFORLOOP,___,___),
  OP(SETLIST,___,___),
  OP(CLOSURE,raw,___),
  OP(VARARG,raw,___),
  OP(EXTRAARG,___,___)
};


#define OpType____ OpType_none
LUAI_DDEF OpType luaP_opout[NUM_OPCODES] = {
#define OPENUM(op,out,in) OpType_##out,
  OPDEF(OPENUM)
#undef OPENUM  
};
LUAI_DDEF OpType luaP_opin[NUM_OPCODES] = {
#define OPENUM(op,out,in) OpType_##in,
  OPDEF(OPENUM)
#undef OPENUM  
};
#undef OpType____


OpCode create_op_settab (OpGroup grp, OpType in) {
  lua_assert(grp == OP_SETTABLE || grp == OP_SETTABUP);
  OpCode op = grp2op(grp);
  switch (in) {
    case OpType_raw: op += 0; break;
    case OpType_int: op += 1; break;
    case OpType_str: op += 2; break;
    case OpType_obj: op += 3; break;
    case OpType_chk: op += 4; break;
    default: lua_assert(0); break;
  }
  return op;
}

OpCode create_op_gettab (OpGroup grp, OpType out, OpType in) {
  lua_assert(grp == OP_GETTABLE || grp == OP_GETTABUP);
  OpCode op = grp2op(grp);
  if (out == OpType_chk) op += 5;
  switch (in) {
    case OpType_raw: op += 0; break;
    case OpType_int: op += 1; break;
    case OpType_str: op += 2; break;
    case OpType_obj: op += 3; break;
    case OpType_chk: op += 4; break;
    default: lua_assert(0); break;
  }
  return op;
}

OpCode create_op_self (OpType out) {
  OpCode op = grp2op(OP_SELF);
  if (out == OpType_chk) op += 1;
  return op;
}

OpCode create_op_arith (OpGroup grp, OpType out, OpType in) {
  lua_assert(grp >= OP_ADD && grp <= OP_POW);
  OpCode op = grp2op(grp);  
  if (out == OpType_chk) op += 4;
  switch (in) {
    case OpType_raw: op += 0; break;
    case OpType_num: op += 1; break;
    case OpType_obj: op += 2; break;
    case OpType_chk: op += 3; break;
    default: lua_assert(0); break;
  }
  return op;
}

OpCode create_op_unm (OpType out, OpType in) {
  OpCode op = grp2op(OP_UNM);
  if (out == OpType_chk) op += 3;
  switch (in) {
    case OpType_raw: op += 0; break;
    case OpType_num: op += 1; break;
    case OpType_chk: op += 2; break;
    default: lua_assert(0); break;
  }
  return op;
}

OpCode create_op_len (OpType out, OpType in) {
  OpCode op = grp2op(OP_LEN);
  if (out == OpType_chk) op += 4;
  switch (in) {
    case OpType_raw: op += 0; break;
    case OpType_str: op += 1; break;
    case OpType_tab: op += 2; break;
    case OpType_chk: op += 3; break;
    default: lua_assert(0); break;
  }
  return op;
}

OpCode create_op_less (OpGroup grp, OpType in) {
  lua_assert(grp == OP_LT || grp == OP_LE);
  OpCode op = grp2op(grp);
  switch (in) {
    case OpType_raw: op += 0; break;
    case OpType_num: op += 1; break;
    case OpType_str: op += 2; break;
    case OpType_chk: op += 3; break;
    default: lua_assert(0); break;
  }
  return op;
}

OpCode create_op_out (OpGroup grp, OpType out) { 
  lua_assert(out == OpType_raw || out == OpType_chk); 
  OpCode op = grp2op(grp);
  if (out == OpType_chk) op += 1;
  return op;
}




const char * const optypenames[7] = {
  "raw","num","int","str","tab","obj","chk"
};
void printop(OpCode op) {  
  OpGroup grp = op2grp(op);
  OpType out = opout(op);
  OpType in = opin(op);
  printf("%-9s", luaP_opnames[grp]);
  if (out != OpType_none) printf(" %s",optypenames[out]);
  if (in != OpType_none) printf(" %s",optypenames[in]);
}





/* ORDER OP */

LUAI_DDEF const char *const luaP_opnames[NUM_OPGROUPS+1] = {
  "MOVE",
  "LOADK",
  "LOADKX",
  "LOADBOOL",
  "LOADNIL",
  "GETUPVAL",
  "GETTABUP",
  "GETTABLE",
  "SETTABUP",
  "SETUPVAL",
  "SETTABLE",
  "NEWTABLE",
  "SELF",
  "ADD",
  "SUB",
  "MUL",
  "DIV",
  "MOD",
  "POW",
  "UNM",
  "NOT",
  "LEN",
  "CONCAT",
  "JMP",
  "EQ",
  "LT",
  "LE",
  "TEST",
  "TESTSET",
  "CALL",
  "TAILCALL",
  "RETURN",
  "FORLOOP",
  "FORPREP",
  "TFORCALL",
  "TFORLOOP",
  "SETLIST",
  "CLOSURE",
  "VARARG",
  "EXTRAARG",
  NULL
};



#define opmode(t,a,b,c,m) (((t)<<7) | ((a)<<6) | ((b)<<4) | ((c)<<2) | (m))

LUAI_DDEF const lu_byte luaP_opmodes[NUM_OPGROUPS] = {
/*       T  A    B       C     mode      opcode */
  opmode(0, 1, OpArgR, OpArgN, iABC)    /* OP_MOVE */
 ,opmode(0, 1, OpArgK, OpArgN, iABx)    /* OP_LOADK */
 ,opmode(0, 1, OpArgN, OpArgN, iABx)    /* OP_LOADKX */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)    /* OP_LOADBOOL */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)    /* OP_LOADNIL */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)    /* OP_GETUPVAL */
 ,opmode(0, 1, OpArgU, OpArgK, iABC)    /* OP_GETTABUP */
 ,opmode(0, 1, OpArgR, OpArgK, iABC)    /* OP_GETTABLE */
 ,opmode(0, 0, OpArgK, OpArgK, iABC)    /* OP_SETTABUP */
 ,opmode(0, 0, OpArgU, OpArgN, iABC)    /* OP_SETUPVAL */
 ,opmode(0, 0, OpArgK, OpArgK, iABC)    /* OP_SETTABLE */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)    /* OP_NEWTABLE */
 ,opmode(0, 1, OpArgR, OpArgK, iABC)    /* OP_SELF */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)    /* OP_ADD */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)    /* OP_SUB */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)    /* OP_MUL */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)    /* OP_DIV */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)    /* OP_MOD */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)    /* OP_POW */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)    /* OP_UNM */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)    /* OP_NOT */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)    /* OP_LEN */
 ,opmode(0, 1, OpArgR, OpArgR, iABC)    /* OP_CONCAT */
 ,opmode(0, 0, OpArgR, OpArgN, iAsBx)   /* OP_JMP */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)    /* OP_EQ */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)    /* OP_LT */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)    /* OP_LE */
 ,opmode(1, 0, OpArgN, OpArgU, iABC)    /* OP_TEST */
 ,opmode(1, 1, OpArgR, OpArgU, iABC)    /* OP_TESTSET */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)    /* OP_CALL */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)    /* OP_TAILCALL */
 ,opmode(0, 0, OpArgU, OpArgN, iABC)    /* OP_RETURN */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)   /* OP_FORLOOP */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)   /* OP_FORPREP */
 ,opmode(0, 0, OpArgN, OpArgU, iABC)    /* OP_TFORCALL */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)   /* OP_TFORLOOP */
 ,opmode(0, 0, OpArgU, OpArgU, iABC)    /* OP_SETLIST */
 ,opmode(0, 1, OpArgU, OpArgN, iABx)    /* OP_CLOSURE */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)    /* OP_VARARG */
 ,opmode(0, 0, OpArgU, OpArgU, iAx)   /* OP_EXTRAARG */
};


