/*
** $Id: lopcodes.h,v 1.142 2011/07/15 12:50:29 roberto Exp $
** Opcodes for Lua virtual machine
** See Copyright Notice in lua.h
*/

#ifndef lopcodes_h
#define lopcodes_h

#include <stdio.h> /* TODO: remove */
#include "llimits.h"

/*===========================================================================
  We assume that instructions are unsigned numbers.
  All instructions have an opcode in the first 9 bits.
  Instructions can have the following fields:
  `A' : 7 bits
  `B' : 8 bits
  `C' : 8 bits
  'Ax' : 23 bits ('A', 'B', and 'C' together)
  `Bx' : 16 bits (`B' and `C' together)
  `sBx' : signed Bx

  A signed argument is represented in excess K; that is, the number
  value is the unsigned value minus K. K is exactly the maximum value
  for that argument (so that -max is represented by 0, and +max is
  represented by 2*max), which is half the maximum for the corresponding
  unsigned argument.
===========================================================================*/

enum OpMode {iABC, iABx, iAsBx, iAx};  /* basic instruction format */


/*
** size and position of opcode arguments.
*/
#define SIZE_C		8
#define SIZE_B		8
#define SIZE_Bx		(SIZE_C + SIZE_B)
#define SIZE_A		7
#define SIZE_Ax		(SIZE_C + SIZE_B + SIZE_A)

#define SIZE_OP   9

#define POS_OP	0
#define POS_A		(POS_OP + SIZE_OP)
#define POS_C		(POS_A + SIZE_A)
#define POS_B		(POS_C + SIZE_C)
#define POS_Bx		POS_C
#define POS_Ax		POS_A


/*
** limits for opcode arguments.
** we use (signed) int to manipulate most arguments,
** so they must fit in LUAI_BITSINT-1 bits (-1 for sign)
*/
#if SIZE_Bx < LUAI_BITSINT-1
#define MAXARG_Bx        ((1<<SIZE_Bx)-1)
#define MAXARG_sBx        (MAXARG_Bx>>1)         /* `sBx' is signed */
#else
#define MAXARG_Bx        MAX_INT
#define MAXARG_sBx        MAX_INT
#endif

#if SIZE_Ax < LUAI_BITSINT-1
#define MAXARG_Ax	((1<<SIZE_Ax)-1)
#else
#define MAXARG_Ax	MAX_INT
#endif


#define MAXARG_A        ((1<<SIZE_A)-1)
#define MAXARG_B        ((1<<SIZE_B)-1)
#define MAXARG_C        ((1<<SIZE_C)-1)


/* creates a mask with `n' 1 bits at position `p' */
#define MASK1(n,p)	((~((~(Instruction)0)<<(n)))<<(p))

/* creates a mask with `n' 0 bits at position `p' */
#define MASK0(n,p)	(~MASK1(n,p))

/*
** the following macros help to manipulate instructions
*/

#define GET_OPCODE(i)	(cast(int, ((i)>>POS_OP) & MASK1(SIZE_OP,0)))
#define SET_OPCODE(i,o)	((i) = (((i)&MASK0(SIZE_OP,POS_OP)) | \
		((cast(Instruction, o)<<POS_OP)&MASK1(SIZE_OP,POS_OP))))

#define getarg(i,pos,size)	(cast(int, ((i)>>pos) & MASK1(size,0)))
#define setarg(i,v,pos,size)	((i) = (((i)&MASK0(size,pos)) | \
                ((cast(Instruction, v)<<pos)&MASK1(size,pos))))

#define GETARG_A(i)	getarg(i, POS_A, SIZE_A)
#define SETARG_A(i,v)	setarg(i, v, POS_A, SIZE_A)

#define GETARG_B(i)	getarg(i, POS_B, SIZE_B)
#define SETARG_B(i,v)	setarg(i, v, POS_B, SIZE_B)

#define GETARG_C(i)	getarg(i, POS_C, SIZE_C)
#define SETARG_C(i,v)	setarg(i, v, POS_C, SIZE_C)

#define GETARG_Bx(i)	getarg(i, POS_Bx, SIZE_Bx)
#define SETARG_Bx(i,v)	setarg(i, v, POS_Bx, SIZE_Bx)

#define GETARG_Ax(i)	getarg(i, POS_Ax, SIZE_Ax)
#define SETARG_Ax(i,v)	setarg(i, v, POS_Ax, SIZE_Ax)

#define GETARG_sBx(i)	(GETARG_Bx(i)-MAXARG_sBx)
#define SETARG_sBx(i,b)	SETARG_Bx((i),cast(unsigned int, (b)+MAXARG_sBx))


#define CREATE_ABC(o,a,b,c)	((cast(Instruction, o)<<POS_OP) \
			| (cast(Instruction, a)<<POS_A) \
			| (cast(Instruction, b)<<POS_B) \
			| (cast(Instruction, c)<<POS_C))

#define CREATE_ABx(o,a,bc)	((cast(Instruction, o)<<POS_OP) \
			| (cast(Instruction, a)<<POS_A) \
			| (cast(Instruction, bc)<<POS_Bx))

#define CREATE_Ax(o,a)		((cast(Instruction, o)<<POS_OP) \
			| (cast(Instruction, a)<<POS_Ax))


/*
** Macros to operate RK indices
*/

/* this bit 1 means constant (0 means register) */
#define BITRK   (1 << (SIZE_B - 1))

/* test whether value is a constant */
#define ISK(x)    ((x) & BITRK)

/* gets the index of the constant */
#define INDEXK(r) ((int)(r) & ~BITRK)

#define MAXINDEXRK  (BITRK - 1)

/* code a constant index as a RK value */
#define RKASK(x)  ((x) | BITRK)


/*
** invalid register that fits in 7 bits
*/
#define NO_REG		MAXARG_A


/*
** R(x) - register
** Kst(x) - constant (in constant table)
** RK(x) == if ISK(x) then Kst(INDEXK(x)) else R(x)
*/

/*
** grep "ORDER OP" if you change these enums
*/

typedef enum {
/*----------------------------------------------------------------------
name    args  description
------------------------------------------------------------------------*/
OP_MOVE,/*  A B R(A) := R(B)          */
OP_LOADK,/* A Bx  R(A) := Kst(Bx)         */
OP_LOADKX,/*  A   R(A) := Kst(extra arg)        */
OP_LOADBOOL,/*  A B C R(A) := (Bool)B; if (C) pc++      */
OP_LOADNIL,/* A B R(A), R(A+1), ..., R(A+B) := nil    */
OP_GETUPVAL,/*  A B R(A) := UpValue[B]        */

OP_GETTABUP,/*  A B C R(A) := UpValue[B][RK(C)]     */
OP_GETTABLE,/*  A B C R(A) := R(B)[RK(C)]       */

OP_SETTABUP,/*  A B C UpValue[A][RK(B)] := RK(C)      */
OP_SETUPVAL,/*  A B UpValue[B] := R(A)        */
OP_SETTABLE,/*  A B C R(A)[RK(B)] := RK(C)        */

OP_NEWTABLE,/*  A B C R(A) := {} (size = B,C)       */

OP_SELF,/*  A B C R(A+1) := R(B); R(A) := R(B)[RK(C)]   */

OP_ADD,/* A B C R(A) := RK(B) + RK(C)       */
OP_SUB,/* A B C R(A) := RK(B) - RK(C)       */
OP_MUL,/* A B C R(A) := RK(B) * RK(C)       */
OP_DIV,/* A B C R(A) := RK(B) / RK(C)       */
OP_MOD,/* A B C R(A) := RK(B) % RK(C)       */
OP_POW,/* A B C R(A) := RK(B) ^ RK(C)       */
OP_UNM,/* A B R(A) := -R(B)         */
OP_NOT,/* A B R(A) := not R(B)        */
OP_LEN,/* A B R(A) := length of R(B)        */

OP_CONCAT,/*  A B C R(A) := R(B).. ... ..R(C)     */

OP_JMP,/* A sBx pc+=sBx; if (A) close all upvalues >= R(A) + 1  */
OP_EQ,/*  A B C if ((RK(B) == RK(C)) ~= A) then pc++    */
OP_LT,/*  A B C if ((RK(B) <  RK(C)) ~= A) then pc++    */
OP_LE,/*  A B C if ((RK(B) <= RK(C)) ~= A) then pc++    */

OP_TEST,/*  A C if not (R(A) <=> C) then pc++     */
OP_TESTSET,/* A B C if (R(B) <=> C) then R(A) := R(B) else pc++ */

OP_CALL,/*  A B C R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1)) */
OP_TAILCALL,/*  A B C return R(A)(R(A+1), ... ,R(A+B-1))    */
OP_RETURN,/*  A B return R(A), ... ,R(A+B-2)  (see note)  */

OP_FORLOOP,/* A sBx R(A)+=R(A+2);
      if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }*/
OP_FORPREP,/* A sBx R(A)-=R(A+2); pc+=sBx       */

OP_TFORCALL,/*  A C R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));  */
OP_TFORLOOP,/*  A sBx if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }*/

OP_SETLIST,/* A B C R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B  */

OP_CLOSURE,/* A Bx  R(A) := closure(KPROTO[Bx])     */

OP_VARARG,/*  A B R(A), R(A+1), ..., R(A+B-2) = vararg    */

OP_CHKTYPE,

OP_EXTRAARG/* Ax  extra (larger) argument for previous opcode */
} OpGroup;


#define NUM_OPGROUPS (cast(int, OP_EXTRAARG) + 1)


/*===========================================================================
  Notes:
  (*) In OP_CALL, if (B == 0) then B = top. If (C == 0), then `top' is
  set to last_result+1, so next open instruction (OP_CALL, OP_RETURN,
  OP_SETLIST) may use `top'.

  (*) In OP_VARARG, if (B == 0) then use actual number of varargs and
  set top (like in OP_CALL with C == 0).

  (*) In OP_RETURN, if (B == 0) then return up to `top'.

  (*) In OP_SETLIST, if (B == 0) then B = `top'; if (C == 0) then next
  'instruction' is EXTRAARG(real C).

  (*) In OP_LOADKX, the next 'instruction' is always EXTRAARG.

  (*) For comparisons, A specifies what condition the test should accept
  (true or false).

  (*) All `skips' (pc++) assume that next instruction is a jump.

===========================================================================*/



/* (expected output type, input type)*/

#define OPDEF(_) \
_(MOVE, ___, ___) /* vanilla */ \
_(MOVE, num, ___) /* type check (number) */ \
_(MOVE, str, ___) /* type check (string) */ \
_(MOVE, obj, ___) /* type check (object) */ \
_(MOVE, ___, num) /* move number */ \
_(MOVE, ___, str) /* move string */ \
_(MOVE, ___, obj) /* move object */ \
_(MOVE, ___, chk) /* specialize */ \
_(MOVE, num, chk) /* specialize + keep type check (number) */ \
_(MOVE, str, chk) /* specialize + keep type check (string) */ \
_(MOVE, obj, chk) /* specialize + keep type check (object) */ \
\
_(LOADK, ___, num) /* load number */ \
_(LOADK, ___, str) /* load string */ \
\
_(LOADKX, ___, num) /* load number */ \
_(LOADKX, ___, str) /* load string */ \
\
_(LOADBOOL, ___, ___) \
\
_(LOADNIL, ___, ___) \
\
_(GETUPVAL, ___, ___) /* vanilla */ \
_(GETUPVAL, num, ___) /* type check (number) */ \
_(GETUPVAL, str, ___) /* type check (string) */ \
_(GETUPVAL, obj, ___) /* type check (object) */ \
\
_(GETTABUP, ___, ___) /* vanilla */ \
_(GETTABUP, num, ___) /* type check (number) */ \
_(GETTABUP, str, ___) /* type check (string) */ \
_(GETTABUP, obj, ___) /* type check (object) */ \
_(GETTABUP, ___, num) /* fast number key */ \
_(GETTABUP, num, num) /* fast number key + type check (number) */ \
_(GETTABUP, str, num) /* fast number key + type check (string) */ \
_(GETTABUP, obj, num) /* fast number key + type check (object) */ \
_(GETTABUP, ___, str) /* fast string key */ \
_(GETTABUP, num, str) /* fast string key + type check (number) */ \
_(GETTABUP, str, str) /* fast string key + type check (string) */ \
_(GETTABUP, obj, str) /* fast string key + type check (object) */ \
_(GETTABUP, ___, obj) /* fast object key */ \
_(GETTABUP, num, obj) /* fast object key + type check (number) */ \
_(GETTABUP, str, obj) /* fast object key + type check (string) */ \
_(GETTABUP, obj, obj) /* fast object key + type check (object) */ \
_(GETTABUP, ___, chk) /* specialize */ \
_(GETTABUP, num, chk) /* specialize + keep type check (number) */ \
_(GETTABUP, str, chk) /* specialize + keep type check (string) */ \
_(GETTABUP, obj, chk) /* specialize + keep type check (object) */ \
\
_(GETTABLE, ___, ___) /* vanilla */ \
_(GETTABLE, num, ___) /* type check (number) */ \
_(GETTABLE, str, ___) /* type check (string) */ \
_(GETTABLE, obj, ___) /* type check (object) */ \
_(GETTABLE, ___, num) /* fast number key */ \
_(GETTABLE, num, num) /* fast number key + type check (number) */ \
_(GETTABLE, str, num) /* fast number key + type check (string) */ \
_(GETTABLE, obj, num) /* fast number key + type check (object) */ \
_(GETTABLE, ___, str) /* fast string key */ \
_(GETTABLE, num, str) /* fast string key + type check (number) */ \
_(GETTABLE, str, str) /* fast string key + type check (string) */ \
_(GETTABLE, obj, str) /* fast string key + type check (object) */ \
_(GETTABLE, ___, obj) /* fast object key */ \
_(GETTABLE, num, obj) /* fast object key + type check (number) */ \
_(GETTABLE, str, obj) /* fast object key + type check (string) */ \
_(GETTABLE, obj, obj) /* fast object key + type check (object) */ \
_(GETTABLE, ___, chk) /* specialize */ \
_(GETTABLE, num, chk) /* specialize + keep type check (number) */ \
_(GETTABLE, str, chk) /* specialize + keep type check (string) */ \
_(GETTABLE, obj, chk) /* specialize + keep type check (object) */ \
\
_(SETTABUP, ___, ___) /* vanilla */ \
_(SETTABUP, ___, num) /* fast number key */ \
_(SETTABUP, ___, str) /* fast string key */ \
_(SETTABUP, ___, obj) /* fast object key */ \
_(SETTABUP, ___, chk) /* specialize */ \
\
_(SETUPVAL, ___, ___) \
\
_(SETTABLE, ___, ___) /* vanilla */ \
_(SETTABLE, ___, num) /* fast number key */ \
_(SETTABLE, ___, str) /* fast string key */ \
_(SETTABLE, ___, obj) /* fast object key */ \
_(SETTABLE, ___, chk) /* specialize */ \
\
_(NEWTABLE, ___, ___) \
\
_(SELF, ___, ___) \
\
_(ADD, ___, ___) /* vanilla */ \
_(ADD, num, ___) /* type check (number) */ \
_(ADD, str, ___) /* type check (string) */ \
_(ADD, obj, ___) /* type check (object) */ \
_(ADD, ___, num) /* fast number operation */ \
_(ADD, ___, obj) /* fast metamethod */ \
_(ADD, num, obj) /* fast metamethod + type check (number) */ \
_(ADD, str, obj) /* fast metamethod + type check (string) */ \
_(ADD, obj, obj) /* fast metamethod + type check (object) */ \
_(ADD, ___, chk) /* specialize */ \
_(ADD, num, chk) /* specialize + keep type check (number) */ \
_(ADD, str, chk) /* specialize + keep type check (string) */ \
_(ADD, obj, chk) /* specialize + keep type check (object) */ \
\
_(SUB, ___, ___) /* vanilla */ \
_(SUB, num, ___) /* type check (number) */ \
_(SUB, str, ___) /* type check (string) */ \
_(SUB, obj, ___) /* type check (object) */ \
_(SUB, ___, num) /* fast number operation */ \
_(SUB, ___, obj) /* fast metamethod */ \
_(SUB, num, obj) /* fast metamethod + type check (number) */ \
_(SUB, str, obj) /* fast metamethod + type check (string) */ \
_(SUB, obj, obj) /* fast metamethod + type check (object) */ \
_(SUB, ___, chk) /* specialize */ \
_(SUB, num, chk) /* specialize + keep type check (number) */ \
_(SUB, str, chk) /* specialize + keep type check (string) */ \
_(SUB, obj, chk) /* specialize + keep type check (object) */ \
\
_(MUL, ___, ___) /* vanilla */ \
_(MUL, num, ___) /* type check (number) */ \
_(MUL, str, ___) /* type check (string) */ \
_(MUL, obj, ___) /* type check (object) */ \
_(MUL, ___, num) /* fast number operation */ \
_(MUL, ___, obj) /* fast metamethod */ \
_(MUL, num, obj) /* fast metamethod + type check (number) */ \
_(MUL, str, obj) /* fast metamethod + type check (string) */ \
_(MUL, obj, obj) /* fast metamethod + type check (object) */ \
_(MUL, ___, chk) /* specialize */ \
_(MUL, num, chk) /* specialize + keep type check (number) */ \
_(MUL, str, chk) /* specialize + keep type check (string) */ \
_(MUL, obj, chk) /* specialize + keep type check (object) */ \
\
_(DIV, ___, ___) /* vanilla */ \
_(DIV, num, ___) /* type check (number) */ \
_(DIV, str, ___) /* type check (string) */ \
_(DIV, obj, ___) /* type check (object) */ \
_(DIV, ___, num) /* fast number operation */ \
_(DIV, ___, obj) /* fast metamethod */ \
_(DIV, num, obj) /* fast metamethod + type check (number) */ \
_(DIV, str, obj) /* fast metamethod + type check (string) */ \
_(DIV, obj, obj) /* fast metamethod + type check (object) */ \
_(DIV, ___, chk) /* specialize */ \
_(DIV, num, chk) /* specialize + keep type check (number) */ \
_(DIV, str, chk) /* specialize + keep type check (string) */ \
_(DIV, obj, chk) /* specialize + keep type check (object) */ \
\
_(MOD, ___, ___) /* vanilla */ \
_(MOD, num, ___) /* type check (number) */ \
_(MOD, str, ___) /* type check (string) */ \
_(MOD, obj, ___) /* type check (object) */ \
_(MOD, ___, num) /* fast number operation */ \
_(MOD, ___, obj) /* fast metamethod */ \
_(MOD, num, obj) /* fast metamethod + type check (number) */ \
_(MOD, str, obj) /* fast metamethod + type check (string) */ \
_(MOD, obj, obj) /* fast metamethod + type check (object) */ \
_(MOD, ___, chk) /* specialize */ \
_(MOD, num, chk) /* specialize + keep type check (number) */ \
_(MOD, str, chk) /* specialize + keep type check (string) */ \
_(MOD, obj, chk) /* specialize + keep type check (object) */ \
\
_(POW, ___, ___) /* vanilla */ \
_(POW, num, ___) /* type check (number) */ \
_(POW, str, ___) /* type check (string) */ \
_(POW, obj, ___) /* type check (object) */ \
_(POW, ___, num) /* fast number operation */ \
_(POW, ___, obj) /* fast metamethod */ \
_(POW, num, obj) /* fast metamethod + type check (number) */ \
_(POW, str, obj) /* fast metamethod + type check (string) */ \
_(POW, obj, obj) /* fast metamethod + type check (object) */ \
_(POW, ___, chk) /* specialize */ \
_(POW, num, chk) /* specialize + keep type check (number) */ \
_(POW, str, chk) /* specialize + keep type check (string) */ \
_(POW, obj, chk) /* specialize + keep type check (object) */ \
\
_(UNM, ___, ___) /* vanilla */ \
_(UNM, num, ___) /* type check (number) */ \
_(UNM, str, ___) /* type check (string) */ \
_(UNM, obj, ___) /* type check (object) */ \
_(UNM, ___, num) /* fast number operation */ \
_(UNM, ___, chk) /* specialize */ \
_(UNM, num, chk) /* specialize + keep type check (number) */ \
_(UNM, str, chk) /* specialize + keep type check (string) */ \
_(UNM, obj, chk) /* specialize + keep type check (object) */ \
\
_(NOT, ___, ___) \
\
_(LEN, ___, ___) /* vanilla */ \
_(LEN, num, ___) /* type check (number) */ \
_(LEN, str, ___) /* type check (string) */ \
_(LEN, obj, ___) /* type check (object) */ \
_(LEN, ___, str) /* fast string length */ \
_(LEN, ___, chk) /* specialize */ \
_(LEN, num, chk) /* specialize + keep type check (number) */ \
_(LEN, str, chk) /* specialize + keep type check (string) */ \
_(LEN, obj, chk) /* specialize + keep type check (object) */ \
\
_(CONCAT, ___, ___) /* vanilla */ \
_(CONCAT, num, ___) /* type check (number) */ \
_(CONCAT, str, ___) /* type check (string) */ \
_(CONCAT, obj, ___) /* type check (object) */ \
\
_(JMP, ___, ___) \
\
_(EQ, ___, ___) /* vanilla */ \
_(EQ, ___, num) /* two number values */ \
_(EQ, ___, str) /* two string values */ \
_(EQ, ___, obj) /* two object values */ \
_(EQ, ___, chk) /* specialize */ \
\
_(LT, ___, ___) /* vanilla */ \
_(LT, ___, num) /* fast number comparison */ \
_(LT, ___, str) /* fast string comparison */ \
_(LT, ___, chk) /* specialize */ \
\
_(LE, ___, ___) /* vanilla */ \
_(LE, ___, num) /* fast number comparison */ \
_(LE, ___, str) /* fast string comparison */ \
_(LE, ___, chk) /* specialize */ \
\
_(TEST, ___, ___) \
\
_(TESTSET,  ___, ___) /* vanilla */ \
_(TESTSET,  num, ___) /* type check (number) */ \
_(TESTSET,  str, ___) /* type check (string) */ \
_(TESTSET,  obj, ___) /* type check (object) */ \
\
_(CALL, ___, ___) /* vanilla */ \
\
_(TAILCALL, ___, ___) \
_(RETURN, ___, ___) \
_(FORLOOP, ___, ___) \
_(FORPREP, ___, ___) \
\
_(TFORCALL, ___, ___) \
\
_(TFORLOOP, ___, ___) \
_(SETLIST, ___, ___) \
_(CLOSURE, ___, ___) \
\
_(VARARG, ___, ___) \
\
_(CHKTYPE, ___, ___) /* no-op */ \
_(CHKTYPE, num, ___) /* type check (number) */ \
_(CHKTYPE, str, ___) /* type check (str) */ \
_(CHKTYPE, obj, ___) /* type check (obj) */ \
\
_(EXTRAARG, ___, ___)


#define OP(op,out,in) OP_##op##_##out##_##in

#define sOP(op) OP(op,___,___)

typedef enum {
#define OPENUM(op,out,in) OP(op,out,in),
  OPDEF(OPENUM)
#undef OPENUM
  NUM_OPCODES
} OpCode;

LUAI_DDEC OpGroup luaP_opcode2group[NUM_OPCODES];
LUAI_DDEC OpCode luaP_opgroup2code[NUM_OPGROUPS];

#define op2grp(op) luaP_opcode2group[op]
#define grp2op(grp) luaP_opgroup2code[grp]

#define GET_OPGROUP(i) luaP_opcode2group[GET_OPCODE(i)]

typedef enum {  
  OpType_raw,
  OpType_num,
  OpType_str,
  OpType_obj,
  OpType_chk
} OpType;

#define OpType____ OpType_raw

LUAI_DDEC OpType luaP_opout[NUM_OPCODES];
LUAI_DDEC OpType luaP_opin[NUM_OPCODES];

#define opout(op) luaP_opout[op]
#define opin(op) luaP_opin[op]


LUAI_FUNC OpCode create_op_move (OpType in, OpType out);
LUAI_FUNC OpCode create_op_settab (OpGroup grp, OpType in);
LUAI_FUNC OpCode create_op_gettab (OpGroup grp, OpType out, OpType in);
LUAI_FUNC OpCode create_op_self (OpType out);
LUAI_FUNC OpCode create_op_arith (OpGroup grp, OpType in, OpType out);
LUAI_FUNC OpCode create_op_unm (OpType out, OpType in);
LUAI_FUNC OpCode create_op_len (OpType out, OpType in);
LUAI_FUNC OpCode create_op_cmp (OpGroup grp, OpType in);
LUAI_FUNC OpCode create_op_out (OpGroup grp, OpType out);


#define set_out_move(op,out) create_op_move(opin(op),out)
#define set_out_gettab(op,out) \
  create_op_gettab(op2grp(op),out,opin(op))
#define set_out_arith(op,out) \
  create_op_arith(op2grp(op),out,opin(op))
#define set_out_unm(op,out) create_op_unm(out,opin(op))
#define set_out_len(op,out) create_op_len(out,opin(op))
#define set_out(op,out) create_op_out(op2grp(op),out)

#define set_in_move(op,in) create_op_move(in,opout(op))
#define set_in_settab(op,in) create_op_settab(op2grp(op),in)
#define set_in_gettab(op,in) \
  create_op_gettab(op2grp(op),opout(op),in)
#define set_in_arith(op,in) \
  create_op_arith(op2grp(op),opout(op),in)
#define set_in_unm(op,in) create_op_unm(opout(op),in)
#define set_in_len(op,in) create_op_len(opout(op),in)
#define set_in_cmp(op,in) create_op_cmp(op2grp(op),in)



extern void printop(OpCode op);


/*
** masks for instruction properties. The format is:
** bits 0-1: op mode
** bits 2-3: C arg mode
** bits 4-5: B arg mode
** bit 6: instruction set register A
** bit 7: operator is a test (next instruction must be a jump)
*/

enum OpArgMask {
  OpArgN,  /* argument is not used */
  OpArgU,  /* argument is used */
  OpArgR,  /* argument is a register or a jump offset */
  OpArgK   /* argument is a constant or register/constant */
};

LUAI_DDEC const lu_byte luaP_opmodes[NUM_OPGROUPS];

#define getOpMode(m)  (cast(enum OpMode, luaP_opmodes[op2grp(m)] & 3))
#define getBMode(m) (cast(enum OpArgMask, (luaP_opmodes[op2grp(m)] >> 4) & 3))
#define getCMode(m) (cast(enum OpArgMask, (luaP_opmodes[op2grp(m)] >> 2) & 3))
#define testAMode(m)  (luaP_opmodes[op2grp(m)] & (1 << 6))
#define testTMode(m)  (luaP_opmodes[op2grp(m)] & (1 << 7))


LUAI_DDEC const char *const luaP_opnames[NUM_OPGROUPS+1];  /* opcode names */


/* number of list items to accumulate before a SETLIST instruction */
#define LFIELDS_PER_FLUSH 50



#endif
