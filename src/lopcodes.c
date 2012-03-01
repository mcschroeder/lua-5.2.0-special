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
  OP(MOVE,___,___),
  OP(LOADK,___,num),
  OP(LOADKX,___,num),
  OP(LOADBOOL,___,___),
  OP(LOADNIL,___,___),
  OP(GETUPVAL,___,___),
  OP(GETTABUP,___,___),
  OP(GETTABLE,___,___),
  OP(SETTABUP,___,___),
  OP(SETUPVAL,___,___),
  OP(SETTABLE,___,___),
  OP(NEWTABLE,___,___),
  OP(SELF,___,___),
  OP(ADD,___,___),
  OP(SUB,___,___),
  OP(MUL,___,___),
  OP(DIV,___,___),
  OP(MOD,___,___),
  OP(POW,___,___),
  OP(UNM,___,___),
  OP(NOT,___,___),
  OP(LEN,___,___),
  OP(CONCAT,___,___),
  OP(JMP,___,___),
  OP(EQ,___,___),
  OP(LT,___,___),
  OP(LE,___,___),
  OP(TEST,___,___),
  OP(TESTSET,___,___),
  OP(CALL,___,___),
  OP(TAILCALL,___,___),
  OP(RETURN,___,___),
  OP(FORLOOP,___,___),
  OP(FORPREP,___,___),
  OP(TFORCALL,___,___),
  OP(TFORLOOP,___,___),
  OP(SETLIST,___,___),
  OP(CLOSURE,___,___),
  OP(VARARG,___,___),
  OP(CHKTYPE,___,___),
  OP(EXTRAARG,___,___)
};


LUAI_DDEF OpType luaP_opguards[NUM_OPCODES] = {
#define OPENUM(op,guard,spec) OpType_##guard,
  OPDEF(OPENUM)
#undef OPENUM  
};
LUAI_DDEF OpType luaP_opspecs[NUM_OPCODES] = {
#define OPENUM(op,guard,spec) OpType_##spec,
  OPDEF(OPENUM)
#undef OPENUM  
};


/* raw, num, str, tab, chk */
LUAI_DDEF const int OpSpecOffsets[NUM_OPGROUPS][5] = {
  {0,  4,  5,  6,  7}, /* MOVE */
  {0,  0,  1, -1, -1}, /* LOADK */
  {0,  0,  1, -1, -1}, /* LOADKX */
  {0, -1, -1, -1, -1}, /* LOADBOOL */
  {0, -1, -1, -1, -1}, /* LOADNIL */
  {0,  4,  5,  6,  7}, /* GETUPVAL */
  {0,  4,  8, -1, 12}, /* GETTABUP */
  {0,  4,  8, -1, 12}, /* GETTABLE */
  {0,  1,  2, -1,  3}, /* SETTABUP */
  {0,  4,  5,  6,  7}, /* SETUPVAL */
  {0,  1,  2, -1,  3}, /* SETTABLE */
  {0, -1, -1, -1, -1}, /* NEWTABLE */
  {0, -1, -1,  1,  2}, /* SELF */
  {0,  4, -1, -1,  5}, /* ADD */
  {0,  4, -1, -1,  5}, /* SUB */
  {0,  4, -1, -1,  5}, /* MUL */
  {0,  4, -1, -1,  5}, /* DIV */
  {0,  4, -1, -1,  5}, /* MOD */
  {0,  4, -1, -1,  5}, /* POW */
  {0,  4, -1, -1,  5}, /* UNM */
  {0, -1, -1, -1, -1}, /* NOT */
  {0, -1,  4,  5,  6}, /* LEN */
  {0, -1, -1, -1, -1}, /* CONCAT */
  {0, -1, -1, -1, -1}, /* JMP */
  {0,  1,  2,  3,  4}, /* EQ */
  {0,  1,  2,  3,  4}, /* LT */
  {0,  1,  2,  3,  4}, /* LE */
  {0, -1, -1, -1, -1}, /* TEST */
  {0, -1, -1, -1, -1}, /* TESTSET */
  {0, -1, -1, -1, -1}, /* CALL */
  {0, -1, -1, -1, -1}, /* TAILCALL */
  {0, -1, -1, -1, -1}, /* RETURN */
  {0, -1, -1, -1, -1}, /* FORLOOP */
  {0,  1, -1, -1,  2}, /* FORPREP */
  {0, -1, -1, -1, -1}, /* TFORCALL */
  {0, -1, -1, -1, -1}, /* TFORLOOP */
  {0, -1, -1, -1, -1}, /* SETLIST */
  {0, -1, -1, -1, -1}, /* CLOSURE */
  {0, -1, -1, -1, -1}, /* VARARG */
  {0, -1, -1, -1, -1}, /* CHKTYPE */
  {0, -1, -1, -1, -1}, /* EXTRAARG */
};


OpCode createop (OpGroup grp, OpType guard, OpType spec) {
  lua_assert(OpSpecOffsets[grp][spec] != -1);
  OpCode op = grp2op(grp)+OpSpecOffsets[grp][spec]+guard;
  lua_assert(op2grp(op) == grp);
  return op;
}




const char * const optypenames[7] = {
  "___","num","str","tab","chk"
};
void printop(OpCode op) {
  printf("%-9s", luaP_opnames[op2grp(op)]);
  printf(" %s",optypenames[opguard(op)]);
  printf(" %s",optypenames[opspec(op)]);
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
  "CHKTYPE",
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
 ,opmode(0, 0, OpArgN, OpArgN, iABC)    /* OP_CHKTYPE */
 ,opmode(0, 0, OpArgU, OpArgU, iAx)   /* OP_EXTRAARG */
};


