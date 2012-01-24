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
  All instructions are unsigned numbers (32 bit wide).
  There are three possible field layouts:

  +----+----+----+----+----+
  | B  | C  | A  |   OP    | iABC
  +----+----+----+----+----+
  |  (s)Bx  | A  |   OP    | iABx / iAsBx
  +---------+----+----+----+
  |      Ax      |   OP    | iAx
  +--------------+----+----+
  msb                    lsb

  OP : 11 bit opcode

  A,B,C : 7 bits
  Ax : 21 bits (A,B and C together)
  Bx : 14 bits (B and C together)
  sBx : signed Bx

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
#define SIZE_C		7
#define SIZE_B		7
#define SIZE_Bx		(SIZE_C + SIZE_B)
#define SIZE_A		7
#define SIZE_Ax		(SIZE_C + SIZE_B + SIZE_A)

#define SIZE_OP   11

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
_(MOVE, raw, ___, ___, ___) \
_(MOVE, chk, ___, ___, ___) \
_(LOADK, raw, ___, ___, ___) \
_(LOADK, chk, ___, ___, ___) \
_(LOADKX, raw, ___, ___, ___) \
_(LOADKX, chk, ___, ___, ___) \
_(LOADBOOL, raw, ___, ___, ___) \
_(LOADBOOL, chk, ___, ___, ___) \
_(LOADNIL, raw, ___, ___, ___) \
_(LOADNIL, chk, ___, ___, ___) \
_(GETUPVAL, raw, ___, ___, ___) \
_(GETUPVAL, chk, ___, ___, ___) \
\
_(GETTABLE, raw, raw, ___, reg) \
_(GETTABLE, raw, raw, ___, kst) \
_(GETTABLE, raw, int, ___, reg) \
_(GETTABLE, raw, int, ___, kst) \
_(GETTABLE, raw, str, ___, reg) \
_(GETTABLE, raw, str, ___, kst) \
_(GETTABLE, raw, obj, ___, reg) \
_(GETTABLE, raw, obj, ___, kst) \
_(GETTABLE, raw, chk, ___, reg) \
_(GETTABLE, chk, raw, ___, reg) \
_(GETTABLE, chk, raw, ___, kst) \
_(GETTABLE, chk, int, ___, reg) \
_(GETTABLE, chk, int, ___, kst) \
_(GETTABLE, chk, str, ___, reg) \
_(GETTABLE, chk, str, ___, kst) \
_(GETTABLE, chk, obj, ___, reg) \
_(GETTABLE, chk, obj, ___, kst) \
_(GETTABLE, chk, chk, ___, reg) \
_(GETTABUP, raw, raw, ___, reg) \
_(GETTABUP, raw, raw, ___, kst) \
_(GETTABUP, raw, int, ___, reg) \
_(GETTABUP, raw, int, ___, kst) \
_(GETTABUP, raw, str, ___, reg) \
_(GETTABUP, raw, str, ___, kst) \
_(GETTABUP, raw, obj, ___, reg) \
_(GETTABUP, raw, obj, ___, kst) \
_(GETTABUP, raw, chk, ___, reg) \
_(GETTABUP, chk, raw, ___, reg) \
_(GETTABUP, chk, raw, ___, kst) \
_(GETTABUP, chk, int, ___, reg) \
_(GETTABUP, chk, int, ___, kst) \
_(GETTABUP, chk, str, ___, reg) \
_(GETTABUP, chk, str, ___, kst) \
_(GETTABUP, chk, obj, ___, reg) \
_(GETTABUP, chk, obj, ___, kst) \
_(GETTABUP, chk, chk, ___, reg) \
\
_(SETTABLE, ___, raw, reg, reg) \
_(SETTABLE, ___, raw, reg, kst) \
_(SETTABLE, ___, raw, kst, reg) \
_(SETTABLE, ___, raw, kst, kst) \
_(SETTABLE, ___, int, reg, reg) \
_(SETTABLE, ___, int, reg, kst) \
_(SETTABLE, ___, int, kst, reg) \
_(SETTABLE, ___, int, kst, kst) \
_(SETTABLE, ___, str, reg, reg) \
_(SETTABLE, ___, str, reg, kst) \
_(SETTABLE, ___, str, kst, reg) \
_(SETTABLE, ___, str, kst, kst) \
_(SETTABLE, ___, obj, reg, reg) \
_(SETTABLE, ___, obj, reg, kst) \
_(SETTABLE, ___, obj, kst, reg) \
_(SETTABLE, ___, obj, kst, kst) \
_(SETTABLE, ___, chk, reg, reg) \
_(SETTABLE, ___, chk, reg, kst) \
_(SETTABUP, ___, raw, reg, reg) \
_(SETTABUP, ___, raw, reg, kst) \
_(SETTABUP, ___, raw, kst, reg) \
_(SETTABUP, ___, raw, kst, kst) \
_(SETTABUP, ___, int, reg, reg) \
_(SETTABUP, ___, int, reg, kst) \
_(SETTABUP, ___, int, kst, reg) \
_(SETTABUP, ___, int, kst, kst) \
_(SETTABUP, ___, str, reg, reg) \
_(SETTABUP, ___, str, reg, kst) \
_(SETTABUP, ___, str, kst, reg) \
_(SETTABUP, ___, str, kst, kst) \
_(SETTABUP, ___, obj, reg, reg) \
_(SETTABUP, ___, obj, reg, kst) \
_(SETTABUP, ___, obj, kst, reg) \
_(SETTABUP, ___, obj, kst, kst) \
_(SETTABUP, ___, chk, reg, reg) \
_(SETTABUP, ___, chk, reg, kst) \
\
_(SETUPVAL, ___, ___, ___, ___) \
\
_(NEWTABLE, raw, ___, ___, ___) \
_(NEWTABLE, chk, ___, ___, ___) \
\
_(SELF, raw, ___, ___, reg) \
_(SELF, raw, ___, ___, kst) \
_(SELF, chk, ___, ___, reg) \
_(SELF, chk, ___, ___, kst) \
\
_(ADD, raw, raw, reg, reg) \
_(ADD, raw, raw, reg, kst) \
_(ADD, raw, raw, kst, reg) \
_(ADD, raw, raw, kst, kst) \
_(ADD, raw, num, reg, reg) \
_(ADD, raw, num, reg, kst) \
_(ADD, raw, num, kst, reg) \
_(ADD, raw, obj, reg, reg) \
_(ADD, raw, obj, reg, kst) \
_(ADD, raw, obj, kst, reg) \
_(ADD, raw, chk, reg, reg) \
_(ADD, raw, chk, reg, kst) \
_(ADD, raw, chk, kst, reg) \
_(ADD, chk, raw, reg, reg) \
_(ADD, chk, raw, reg, kst) \
_(ADD, chk, raw, kst, reg) \
_(ADD, chk, raw, kst, kst) \
_(ADD, chk, num, reg, reg) \
_(ADD, chk, num, reg, kst) \
_(ADD, chk, num, kst, reg) \
_(ADD, chk, obj, reg, reg) \
_(ADD, chk, obj, reg, kst) \
_(ADD, chk, obj, kst, reg) \
_(ADD, chk, chk, reg, reg) \
_(ADD, chk, chk, reg, kst) \
_(ADD, chk, chk, kst, reg) \
\
_(SUB, raw, raw, reg, reg) \
_(SUB, raw, raw, reg, kst) \
_(SUB, raw, raw, kst, reg) \
_(SUB, raw, raw, kst, kst) \
_(SUB, raw, num, reg, reg) \
_(SUB, raw, num, reg, kst) \
_(SUB, raw, num, kst, reg) \
_(SUB, raw, obj, reg, reg) \
_(SUB, raw, obj, reg, kst) \
_(SUB, raw, obj, kst, reg) \
_(SUB, raw, chk, reg, reg) \
_(SUB, raw, chk, reg, kst) \
_(SUB, raw, chk, kst, reg) \
_(SUB, chk, raw, reg, reg) \
_(SUB, chk, raw, reg, kst) \
_(SUB, chk, raw, kst, reg) \
_(SUB, chk, raw, kst, kst) \
_(SUB, chk, num, reg, reg) \
_(SUB, chk, num, reg, kst) \
_(SUB, chk, num, kst, reg) \
_(SUB, chk, obj, reg, reg) \
_(SUB, chk, obj, reg, kst) \
_(SUB, chk, obj, kst, reg) \
_(SUB, chk, chk, reg, reg) \
_(SUB, chk, chk, reg, kst) \
_(SUB, chk, chk, kst, reg) \
\
_(MUL, raw, raw, reg, reg) \
_(MUL, raw, raw, reg, kst) \
_(MUL, raw, raw, kst, reg) \
_(MUL, raw, raw, kst, kst) \
_(MUL, raw, num, reg, reg) \
_(MUL, raw, num, reg, kst) \
_(MUL, raw, num, kst, reg) \
_(MUL, raw, obj, reg, reg) \
_(MUL, raw, obj, reg, kst) \
_(MUL, raw, obj, kst, reg) \
_(MUL, raw, chk, reg, reg) \
_(MUL, raw, chk, reg, kst) \
_(MUL, raw, chk, kst, reg) \
_(MUL, chk, raw, reg, reg) \
_(MUL, chk, raw, reg, kst) \
_(MUL, chk, raw, kst, reg) \
_(MUL, chk, raw, kst, kst) \
_(MUL, chk, num, reg, reg) \
_(MUL, chk, num, reg, kst) \
_(MUL, chk, num, kst, reg) \
_(MUL, chk, obj, reg, reg) \
_(MUL, chk, obj, reg, kst) \
_(MUL, chk, obj, kst, reg) \
_(MUL, chk, chk, reg, reg) \
_(MUL, chk, chk, reg, kst) \
_(MUL, chk, chk, kst, reg) \
\
_(DIV, raw, raw, reg, reg) \
_(DIV, raw, raw, reg, kst) \
_(DIV, raw, raw, kst, reg) \
_(DIV, raw, raw, kst, kst) \
_(DIV, raw, num, reg, reg) \
_(DIV, raw, num, reg, kst) \
_(DIV, raw, num, kst, reg) \
_(DIV, raw, obj, reg, reg) \
_(DIV, raw, obj, reg, kst) \
_(DIV, raw, obj, kst, reg) \
_(DIV, raw, chk, reg, reg) \
_(DIV, raw, chk, reg, kst) \
_(DIV, raw, chk, kst, reg) \
_(DIV, chk, raw, reg, reg) \
_(DIV, chk, raw, reg, kst) \
_(DIV, chk, raw, kst, reg) \
_(DIV, chk, raw, kst, kst) \
_(DIV, chk, num, reg, reg) \
_(DIV, chk, num, reg, kst) \
_(DIV, chk, num, kst, reg) \
_(DIV, chk, obj, reg, reg) \
_(DIV, chk, obj, reg, kst) \
_(DIV, chk, obj, kst, reg) \
_(DIV, chk, chk, reg, reg) \
_(DIV, chk, chk, reg, kst) \
_(DIV, chk, chk, kst, reg) \
\
_(MOD, raw, raw, reg, reg) \
_(MOD, raw, raw, reg, kst) \
_(MOD, raw, raw, kst, reg) \
_(MOD, raw, raw, kst, kst) \
_(MOD, raw, num, reg, reg) \
_(MOD, raw, num, reg, kst) \
_(MOD, raw, num, kst, reg) \
_(MOD, raw, obj, reg, reg) \
_(MOD, raw, obj, reg, kst) \
_(MOD, raw, obj, kst, reg) \
_(MOD, raw, chk, reg, reg) \
_(MOD, raw, chk, reg, kst) \
_(MOD, raw, chk, kst, reg) \
_(MOD, chk, raw, reg, reg) \
_(MOD, chk, raw, reg, kst) \
_(MOD, chk, raw, kst, reg) \
_(MOD, chk, raw, kst, kst) \
_(MOD, chk, num, reg, reg) \
_(MOD, chk, num, reg, kst) \
_(MOD, chk, num, kst, reg) \
_(MOD, chk, obj, reg, reg) \
_(MOD, chk, obj, reg, kst) \
_(MOD, chk, obj, kst, reg) \
_(MOD, chk, chk, reg, reg) \
_(MOD, chk, chk, reg, kst) \
_(MOD, chk, chk, kst, reg) \
\
_(POW, raw, raw, reg, reg) \
_(POW, raw, raw, reg, kst) \
_(POW, raw, raw, kst, reg) \
_(POW, raw, raw, kst, kst) \
_(POW, raw, num, reg, reg) \
_(POW, raw, num, reg, kst) \
_(POW, raw, num, kst, reg) \
_(POW, raw, obj, reg, reg) \
_(POW, raw, obj, reg, kst) \
_(POW, raw, obj, kst, reg) \
_(POW, raw, chk, reg, reg) \
_(POW, raw, chk, reg, kst) \
_(POW, raw, chk, kst, reg) \
_(POW, chk, raw, reg, reg) \
_(POW, chk, raw, reg, kst) \
_(POW, chk, raw, kst, reg) \
_(POW, chk, raw, kst, kst) \
_(POW, chk, num, reg, reg) \
_(POW, chk, num, reg, kst) \
_(POW, chk, num, kst, reg) \
_(POW, chk, obj, reg, reg) \
_(POW, chk, obj, reg, kst) \
_(POW, chk, obj, kst, reg) \
_(POW, chk, chk, reg, reg) \
_(POW, chk, chk, reg, kst) \
_(POW, chk, chk, kst, reg) \
\
_(UNM, raw, raw, ___, ___) \
_(UNM, raw, num, ___, ___) \
_(UNM, raw, chk, ___, ___) \
_(UNM, chk, raw, ___, ___) \
_(UNM, chk, num, ___, ___) \
_(UNM, chk, chk, ___, ___) \
\
_(NOT, raw, ___, ___, ___) \
_(NOT, chk, ___, ___, ___) \
\
_(LEN, raw, raw, ___, ___) \
_(LEN, raw, str, ___, ___) \
_(LEN, raw, tab, ___, ___) \
_(LEN, raw, chk, ___, ___) \
_(LEN, chk, raw, ___, ___) \
_(LEN, chk, str, ___, ___) \
_(LEN, chk, tab, ___, ___) \
_(LEN, chk, chk, ___, ___) \
\
_(CONCAT, raw, ___, ___, ___) \
_(CONCAT, chk, ___, ___, ___) \
\
_(JMP, ___, ___, ___, ___) \
\
_(EQ, ___, ___, reg, reg) \
_(EQ, ___, ___, reg, kst) \
_(EQ, ___, ___, kst, reg) \
_(EQ, ___, ___, kst, kst) \
\
_(LT, ___, raw, reg, reg) \
_(LT, ___, raw, reg, kst) \
_(LT, ___, raw, kst, reg) \
_(LT, ___, raw, kst, kst) \
_(LT, ___, num, reg, reg) \
_(LT, ___, num, reg, kst) \
_(LT, ___, num, kst, reg) \
_(LT, ___, num, kst, kst) \
_(LT, ___, str, reg, reg) \
_(LT, ___, str, reg, kst) \
_(LT, ___, str, kst, reg) \
_(LT, ___, str, kst, kst) \
_(LT, ___, chk, reg, reg) \
_(LT, ___, chk, reg, kst) \
_(LT, ___, chk, kst, reg) \
\
_(LE, ___, raw, reg, reg) \
_(LE, ___, raw, reg, kst) \
_(LE, ___, raw, kst, reg) \
_(LE, ___, raw, kst, kst) \
_(LE, ___, num, reg, reg) \
_(LE, ___, num, reg, kst) \
_(LE, ___, num, kst, reg) \
_(LE, ___, num, kst, kst) \
_(LE, ___, str, reg, reg) \
_(LE, ___, str, reg, kst) \
_(LE, ___, str, kst, reg) \
_(LE, ___, str, kst, kst) \
_(LE, ___, chk, reg, reg) \
_(LE, ___, chk, reg, kst) \
_(LE, ___, chk, kst, reg) \
\
_(TEST, ___, ___, ___, ___) \
_(TESTSET, raw, ___, ___, ___) \
_(TESTSET, chk, ___, ___, ___) \
_(CALL, raw, ___, ___, ___) \
_(CALL, chk, ___, ___, ___) \
_(TAILCALL, ___, ___, ___, ___) \
_(RETURN, ___, ___, ___, ___) \
_(FORLOOP, ___, ___, ___, ___) \
_(FORPREP, ___, ___, ___, ___) \
_(TFORCALL, raw, ___, ___, ___) \
_(TFORCALL, chk, ___, ___, ___) \
_(TFORLOOP, ___, ___, ___, ___) \
_(SETLIST, ___, ___, ___, ___) \
_(CLOSURE, raw, ___, ___, ___) \
_(CLOSURE, chk, ___, ___, ___) \
_(VARARG, raw, ___, ___, ___) \
_(VARARG, chk, ___, ___, ___) \
_(EXTRAARG, ___, ___, ___, ___)


#define OP(op,ret,spec,bk,ck) OP_##op##_##ret##_##spec##_##bk##_##ck

#define sOP(op) OP(op,___,___,___,___)

typedef enum {
#define OPENUM(op,ret,spec,bk,ck) OP(op,ret,spec,bk,ck),
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
LUAI_DDEC int luaP_opbk[NUM_OPCODES];
LUAI_DDEC int luaP_opck[NUM_OPCODES];

#define opout(op) luaP_opout[op]
#define opin(op) luaP_opin[op]
#define opbk(op) luaP_opbk[op]
#define opck(op) luaP_opck[op]


LUAI_FUNC OpCode create_op_settab (OpGroup grp, OpType in, int bk, int ck);
LUAI_FUNC OpCode create_op_gettab (OpGroup grp, OpType out, OpType in, 
                                   int ck);
LUAI_FUNC OpCode create_op_self (OpType out, int ck);
LUAI_FUNC OpCode create_op_arith (OpGroup grp, OpType in, OpType out, 
                                  int bk, int ck);
LUAI_FUNC OpCode create_op_unm (OpType out, OpType in);
LUAI_FUNC OpCode create_op_len (OpType out, OpType in);
LUAI_FUNC OpCode create_op_eq (int bk, int ck);
LUAI_FUNC OpCode create_op_less (OpGroup grp, OpType in, int bk, int ck);
LUAI_FUNC OpCode create_op_out (OpGroup grp, OpType out);

#define set_out_gettab(op,out) \
  create_op_gettab(op2grp(op),out,opin(op),opck(op))
#define set_out_self(op,out) create_op_self(out,luaP_opck[op])
#define set_out_arith(op,out) \
  create_op_arith(op2grp(op),out,opin(op),opbk(op),opck(op))
#define set_out_unm(op,out) create_op_unm(opin(op),out)
#define set_out_len(op,out) create_op_len(opin(op),out)
#define set_out(op,out) create_op_out(op2grp(op),out)

#define set_in_settab(op,in) create_op_settab(op2grp(op),in,opbk(op),opck(op))
#define set_in_gettab(op,in) \
  create_op_gettab(op2grp(op),opout(op),in,opck(op))
#define set_in_arith(op,in) \
  create_op_arith(op2grp(op),opout(op),in,opbk(op),opck(op))
#define set_in_unm(op,in) create_op_unm(in,opout(op))
#define set_in_len(op,in) create_op_len(in,opout(op))
#define set_in_less(op,in) create_op_less(op2grp(op),in,opbk(op),opck(op))



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
