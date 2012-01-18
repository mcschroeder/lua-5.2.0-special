/*
** $Id: lopcodes.c,v 1.48 2011/04/19 16:22:13 roberto Exp $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE


#include "lopcodes.h"


/* ORDER OP */

LUAI_DDEF const char *const luaP_opnames[NUM_OPCODES+1] = {
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

LUAI_DDEF const lu_byte luaP_opmodes[NUM_OPCODES] = {
/*       T  A    B       C     mode		   opcode	*/
  opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_MOVE */
 ,opmode(0, 1, OpArgK, OpArgN, iABx)		/* OP_LOADK */
 ,opmode(0, 1, OpArgN, OpArgN, iABx)		/* OP_LOADKX */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_LOADBOOL */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)		/* OP_LOADNIL */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)		/* OP_GETUPVAL */
 ,opmode(0, 1, OpArgU, OpArgK, iABC)		/* OP_GETTABUP */
 ,opmode(0, 1, OpArgR, OpArgK, iABC)		/* OP_GETTABLE */
 ,opmode(0, 0, OpArgK, OpArgK, iABC)		/* OP_SETTABUP */
 ,opmode(0, 0, OpArgU, OpArgN, iABC)		/* OP_SETUPVAL */
 ,opmode(0, 0, OpArgK, OpArgK, iABC)		/* OP_SETTABLE */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_NEWTABLE */
 ,opmode(0, 1, OpArgR, OpArgK, iABC)		/* OP_SELF */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_ADD */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_SUB */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_MUL */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_DIV */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_MOD */
 ,opmode(0, 1, OpArgK, OpArgK, iABC)		/* OP_POW */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_UNM */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_NOT */
 ,opmode(0, 1, OpArgR, OpArgN, iABC)		/* OP_LEN */
 ,opmode(0, 1, OpArgR, OpArgR, iABC)		/* OP_CONCAT */
 ,opmode(0, 0, OpArgR, OpArgN, iAsBx)		/* OP_JMP */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)		/* OP_EQ */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)		/* OP_LT */
 ,opmode(1, 0, OpArgK, OpArgK, iABC)		/* OP_LE */
 ,opmode(1, 0, OpArgN, OpArgU, iABC)		/* OP_TEST */
 ,opmode(1, 1, OpArgR, OpArgU, iABC)		/* OP_TESTSET */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_CALL */
 ,opmode(0, 1, OpArgU, OpArgU, iABC)		/* OP_TAILCALL */
 ,opmode(0, 0, OpArgU, OpArgN, iABC)		/* OP_RETURN */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)		/* OP_FORLOOP */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)		/* OP_FORPREP */
 ,opmode(0, 0, OpArgN, OpArgU, iABC)		/* OP_TFORCALL */
 ,opmode(0, 1, OpArgR, OpArgN, iAsBx)		/* OP_TFORLOOP */
 ,opmode(0, 0, OpArgU, OpArgU, iABC)		/* OP_SETLIST */
 ,opmode(0, 1, OpArgU, OpArgN, iABx)		/* OP_CLOSURE */
 ,opmode(0, 1, OpArgU, OpArgN, iABC)		/* OP_VARARG */
 ,opmode(0, 0, OpArgU, OpArgU, iAx)		/* OP_EXTRAARG */
};

void PrintSpec(Instruction i) {
  switch (GET_OPCODE(i)) {
    case OP_GETTABLE:
    case OP_GETTABUP:
      printf("%s %s", SpecNamesOut[GET_OPSPEC_OUT(i)],
                         SpecNamesTabKey[GET_OPSPEC_GETTAB_KEY(i)]);
      break;
    case OP_SETTABLE:
    case OP_SETTABUP:
      printf("%s", SpecNamesTabKey[GET_OPSPEC_SETTAB_KEY(i)]);
      break;
    case OP_ADD: case OP_SUB: case OP_MUL: 
    case OP_DIV: case OP_MOD: case OP_POW:
    case OP_UNM:
      printf("%s %s", SpecNamesOut[GET_OPSPEC_OUT(i)],
                         SpecNamesArithIn[GET_OPSPEC_ARITH_IN(i)]);
      break;
    case OP_LEN:
      printf("%s %s", SpecNamesOut[GET_OPSPEC_OUT(i)],
                         SpecNamesLenIn[GET_OPSPEC_ARITH_IN(i)]);
      break;
    case OP_LT: case OP_LE:
      printf("%s", SpecNamesLessType[GET_OPSPEC_LESS_TYPE(i)]);
      break;      
    default:
      printf("%i", GET_OPSPEC(i));      
      break;
  }
}

extern void PrintOp(Instruction i) {  
  printf("%i", GET_OP(i));

  int opcode = GET_OPCODE(i);
  printf(" (%i", opcode);
  if (opcode < NUM_OPCODES)
    printf("/%s", luaP_opnames[opcode]);
  
  printf(" %i", GET_OPSPEC(i));
  switch (opcode) {
    case OP_GETTABLE:
    case OP_GETTABUP:
      printf("/%s %s", SpecNamesOut[GET_OPSPEC_OUT(i)],
                         SpecNamesTabKey[GET_OPSPEC_GETTAB_KEY(i)]);
      break;
    case OP_SETTABLE:
    case OP_SETTABUP:
      printf("/%s", SpecNamesTabKey[GET_OPSPEC_SETTAB_KEY(i)]);
      break;
    case OP_ADD: case OP_SUB: case OP_MUL: 
    case OP_DIV: case OP_MOD: case OP_POW:
    case OP_UNM:
      printf("/%s %s", SpecNamesOut[GET_OPSPEC_OUT(i)],
                         SpecNamesArithIn[GET_OPSPEC_ARITH_IN(i)]);
      break;
    case OP_LEN:
      printf("/%s %s", SpecNamesOut[GET_OPSPEC_OUT(i)],
                         SpecNamesLenIn[GET_OPSPEC_ARITH_IN(i)]);
      break;
    case OP_LT: case OP_LE:
      printf("/%s", SpecNamesLessType[GET_OPSPEC_LESS_TYPE(i)]);
      break;      
    default:
      break;
  }
  printf(")");
}
