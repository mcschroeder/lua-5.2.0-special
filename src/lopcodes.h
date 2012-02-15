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



/* op out in bk ck */
#define OPDEF(_) \
_(MOVE,     raw, ___) \
_(MOVE,     chk, ___) \
\
_(LOADK,    raw, ___) \
_(LOADK,    chk, ___) \
\
_(LOADKX,   raw, ___) \
_(LOADKX,   chk, ___) \
\
_(LOADBOOL, raw, ___) \
_(LOADBOOL, chk, ___) \
\
_(LOADNIL,  raw, ___) \
_(LOADNIL,  chk, ___) \
\
_(GETUPVAL, raw, ___) \
_(GETUPVAL, chk, ___) \
\
_(GETTABLE, raw, raw) \
_(GETTABLE, raw, int) \
_(GETTABLE, raw, str) \
_(GETTABLE, raw, obj) \
_(GETTABLE, raw, chk) \
_(GETTABLE, chk, raw) \
_(GETTABLE, chk, int) \
_(GETTABLE, chk, str) \
_(GETTABLE, chk, obj) \
_(GETTABLE, chk, chk) \
\
_(GETTABUP, raw, raw) \
_(GETTABUP, raw, int) \
_(GETTABUP, raw, str) \
_(GETTABUP, raw, obj) \
_(GETTABUP, raw, chk) \
_(GETTABUP, chk, raw) \
_(GETTABUP, chk, int) \
_(GETTABUP, chk, str) \
_(GETTABUP, chk, obj) \
_(GETTABUP, chk, chk) \
\
_(SETTABLE, ___, raw) \
_(SETTABLE, ___, int) \
_(SETTABLE, ___, str) \
_(SETTABLE, ___, obj) \
_(SETTABLE, ___, chk) \
\
_(SETTABUP, ___, raw) \
_(SETTABUP, ___, int) \
_(SETTABUP, ___, str) \
_(SETTABUP, ___, obj) \
_(SETTABUP, ___, chk) \
\
_(SETUPVAL, ___, ___) \
\
_(NEWTABLE, raw, ___) \
_(NEWTABLE, chk, ___) \
\
_(SELF, raw, ___) \
_(SELF, chk, ___) \
\
_(ADD, raw, raw) \
_(ADD, raw, num) \
_(ADD, raw, obj) \
_(ADD, raw, chk) \
_(ADD, chk, raw) \
_(ADD, chk, num) \
_(ADD, chk, obj) \
_(ADD, chk, chk) \
\
_(SUB, raw, raw) \
_(SUB, raw, num) \
_(SUB, raw, obj) \
_(SUB, raw, chk) \
_(SUB, chk, raw) \
_(SUB, chk, num) \
_(SUB, chk, obj) \
_(SUB, chk, chk) \
\
_(MUL, raw, raw) \
_(MUL, raw, num) \
_(MUL, raw, obj) \
_(MUL, raw, chk) \
_(MUL, chk, raw) \
_(MUL, chk, num) \
_(MUL, chk, obj) \
_(MUL, chk, chk) \
\
_(DIV, raw, raw) \
_(DIV, raw, num) \
_(DIV, raw, obj) \
_(DIV, raw, chk) \
_(DIV, chk, raw) \
_(DIV, chk, num) \
_(DIV, chk, obj) \
_(DIV, chk, chk) \
\
_(MOD, raw, raw) \
_(MOD, raw, num) \
_(MOD, raw, obj) \
_(MOD, raw, chk) \
_(MOD, chk, raw) \
_(MOD, chk, num) \
_(MOD, chk, obj) \
_(MOD, chk, chk) \
\
_(POW, raw, raw) \
_(POW, raw, num) \
_(POW, raw, obj) \
_(POW, raw, chk) \
_(POW, chk, raw) \
_(POW, chk, num) \
_(POW, chk, obj) \
_(POW, chk, chk) \
\
_(UNM, raw, raw) \
_(UNM, raw, num) \
_(UNM, raw, chk) \
_(UNM, chk, raw) \
_(UNM, chk, num) \
_(UNM, chk, chk) \
\
_(NOT, raw, ___) \
_(NOT, chk, ___) \
\
_(LEN, raw, raw) \
_(LEN, raw, str) \
_(LEN, raw, tab) \
_(LEN, raw, chk) \
_(LEN, chk, raw) \
_(LEN, chk, str) \
_(LEN, chk, tab) \
_(LEN, chk, chk) \
\
_(CONCAT, raw, ___) \
_(CONCAT, chk, ___) \
\
_(JMP, ___, ___) \
\
_(EQ, ___, ___) \
\
_(LT, ___, raw) \
_(LT, ___, num) \
_(LT, ___, str) \
_(LT, ___, chk) \
\
_(LE, ___, raw) \
_(LE, ___, num) \
_(LE, ___, str) \
_(LE, ___, chk) \
\
_(TEST,     ___, ___) \
_(TESTSET,  raw, ___) \
_(TESTSET,  chk, ___) \
_(CALL,     raw, ___) \
_(CALL,     chk, ___) \
_(TAILCALL, ___, ___) \
_(RETURN,   ___, ___) \
_(FORLOOP,  ___, ___) \
_(FORPREP,  ___, ___) \
_(TFORCALL, raw, ___) \
_(TFORCALL, chk, ___) \
_(TFORLOOP, ___, ___) \
_(SETLIST,  ___, ___) \
_(CLOSURE,  raw, ___) \
_(CLOSURE,  chk, ___) \
_(VARARG,   raw, ___) \
_(VARARG,   chk, ___) \
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
  OpType_int,  
  OpType_str,
  OpType_tab,
  OpType_obj,
  OpType_chk,  
  OpType_none
} OpType;

LUAI_DDEC OpType luaP_opout[NUM_OPCODES];
LUAI_DDEC OpType luaP_opin[NUM_OPCODES];

#define opout(op) luaP_opout[op]
#define opin(op) luaP_opin[op]


LUAI_FUNC OpCode create_op_settab (OpGroup grp, OpType in);
LUAI_FUNC OpCode create_op_gettab (OpGroup grp, OpType out, OpType in);
LUAI_FUNC OpCode create_op_self (OpType out);
LUAI_FUNC OpCode create_op_arith (OpGroup grp, OpType in, OpType out);
LUAI_FUNC OpCode create_op_unm (OpType out, OpType in);
LUAI_FUNC OpCode create_op_len (OpType out, OpType in);
LUAI_FUNC OpCode create_op_less (OpGroup grp, OpType in);
LUAI_FUNC OpCode create_op_out (OpGroup grp, OpType out);

#define set_out_gettab(op,out) \
  create_op_gettab(op2grp(op),out,opin(op))
#define set_out_self(op,out) create_op_self(out)
#define set_out_arith(op,out) \
  create_op_arith(op2grp(op),out,opin(op))
#define set_out_unm(op,out) create_op_unm(opin(op),out)
#define set_out_len(op,out) create_op_len(opin(op),out)
#define set_out(op,out) create_op_out(op2grp(op),out)

#define set_in_settab(op,in) create_op_settab(op2grp(op),in)
#define set_in_gettab(op,in) \
  create_op_gettab(op2grp(op),opout(op),in)
#define set_in_arith(op,in) \
  create_op_arith(op2grp(op),opout(op),in)
#define set_in_unm(op,in) create_op_unm(in,opout(op))
#define set_in_len(op,in) create_op_len(in,opout(op))
#define set_in_less(op,in) create_op_less(op2grp(op),in)



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

#define getOpMode(m)  (cast(enum OpMode, luaP_opmodes[luaP_opcode2group[m]] & 3))
#define getBMode(m) (cast(enum OpArgMask, (luaP_opmodes[luaP_opcode2group[m]] >> 4) & 3))
#define getCMode(m) (cast(enum OpArgMask, (luaP_opmodes[luaP_opcode2group[m]] >> 2) & 3))
#define testAMode(m)  (luaP_opmodes[luaP_opcode2group[m]] & (1 << 6))
#define testTMode(m)  (luaP_opmodes[luaP_opcode2group[m]] & (1 << 7))


LUAI_DDEC const char *const luaP_opnames[NUM_OPGROUPS+1];  /* opcode names */


/* number of list items to accumulate before a SETLIST instruction */
#define LFIELDS_PER_FLUSH 50



#endif
