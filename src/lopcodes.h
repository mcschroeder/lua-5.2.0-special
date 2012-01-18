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
  | B  | C  | A  | SP | OP | iABC
  +----+----+----+----+----+
  |  (s)Bx  | A  | SP | OP | iABx / iAsBx
  +---------+----+----+----+
  |      Ax      | SP | OP | iAx
  +--------------+----+----+
  msb                    lsb

  OP : 6 bit operation identifier (OPCODE)
  SP : 5 bit type specialization code (OPSPEC)

  The VM dispatches on the combined 11 bits of SP + OP. The semantics of the
  specialization bitmask depend on the operation (see below).

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

#define SIZE_OP 6
#define SIZE_SP 5

#define POS_OP	0
#define POS_SP  (POS_OP + SIZE_OP)
#define POS_A		(POS_SP + SIZE_SP)
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

#define GET_OPSPEC(i) (cast(int, ((i)>>POS_SP) & MASK1(SIZE_SP,0)))
#define SET_OPSPEC(i,s) ((i) = (((i)&MASK0(SIZE_SP,POS_SP)) | \
    ((cast(Instruction, s)<<POS_SP)&MASK1(SIZE_SP,POS_SP))))

/* this is what the VM dispatches on */
#define GET_OP(i) (cast(int, ((i>>POS_OP) & MASK1(SIZE_OP+SIZE_SP,0))))
#define OP_MAKE(op,sp) (cast(int, ((sp)<<POS_SP) | (op)))

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


#define CREATE_ABC(o,s,a,b,c)	((cast(Instruction, o)<<POS_OP) \
      | (cast(Instruction, s)<<POS_SP) \
			| (cast(Instruction, a)<<POS_A) \
			| (cast(Instruction, b)<<POS_B) \
			| (cast(Instruction, c)<<POS_C))

#define CREATE_ABx(o,s,a,bc)	((cast(Instruction, o)<<POS_OP) \
      | (cast(Instruction, s)<<POS_SP) \
			| (cast(Instruction, a)<<POS_A) \
			| (cast(Instruction, bc)<<POS_Bx))

#define CREATE_Ax(o,s,a)		((cast(Instruction, o)<<POS_OP) \
      | (cast(Instruction, s)<<POS_SP) \
			| (cast(Instruction, a)<<POS_Ax))



/*
** invalid register that fits in 7 bits
*/
#define NO_REG		MAXARG_A


/* -- specialization mask constants -------------------------------------- */

/* OUT is always the lowest bit in the spec mask */
#define GET_OPSPEC_OUT(i) (((i)&MASK1(1,POS_SP))>>POS_SP)
#define SET_OPSPEC_OUT(i,o) ((i) = (((i)&MASK0(1,POS_SP)) | \
    (((o)<<POS_SP)&MASK1(1,POS_SP))))

#define OPSPEC_OUT_raw 0
#define OPSPEC_OUT_chk 1

/* BK and CK should be in the two most significant bits of the spec mask */
#define GET_OPSPEC_BK(i) (((i)&MASK1(1,POS_SP+3))>>(POS_SP+3))
#define GET_OPSPEC_CK(i) (((i)&MASK1(1,POS_SP+4))>>(POS_SP+4))

#define OPSPEC_reg 0
#define OPSPEC_kst 1

/* used by SETTABLE, SETTABUP, GETTABLE, GETTABUP */
#define OPSPEC_TAB_KEY_raw 0
#define OPSPEC_TAB_KEY_str 1
#define OPSPEC_TAB_KEY_int 2
#define OPSPEC_TAB_KEY_obj 3
#define OPSPEC_TAB_KEY_chk 4

/* used by arithmetic operations (ADD, SUB, MUL, DIV, MOD, POW, UNM, LEN) */
#define OPSPEC_ARITH_IN_raw 0
#define OPSPEC_ARITH_IN_num 1
#define OPSPEC_ARITH_IN_obj 2 /* only for binary ops */
#define OPSPEC_ARITH_IN_chk 3

/* only for LEN */
#define OPSPEC_ARITH_IN_str 1
#define OPSPEC_ARITH_IN_tab 2

/* used by LT, LE */
#define OPSPEC_LESS_TYPE_raw 0
#define OPSPEC_LESS_TYPE_num 1
#define OPSPEC_LESS_TYPE_str 2
#define OPSPEC_LESS_TYPE_chk 3


/* -- GETTABLE & GETTABUP specialization ------------------------------------

  +----+-----+-----+
  | CK | KEY | OUT |
  +----+-----+-----+
  msb            lsb

  OUT : 1 bit, determines if A needs a specialization check
  KEY : 3 bit, type of C (the table key)
  CK  : 1 bit, determines if C is a constant index

*/

#define CREATE_OPSPEC_GETTAB(out,key,ck) \
        (((ck)<<4) | ((key)<<1) | (out))

#define GET_OPSPEC_GETTAB_KEY(i) (((i)&MASK1(3,POS_SP+1))>>(POS_SP+1))
#define SET_OPSPEC_GETTAB_KEY(i,key) ((i) = (((i)&MASK0(3,POS_SP+1)) | \
    (((key)<<(POS_SP+1))&MASK1(3,POS_SP+1))))


/* -- SETTABLE & SETTABUP specialization ------------------------------------
       
  +----+----+-----+
  | CK | BK | KEY |
  +----+----+-----+
  msb             lsb

  KEY : 3 bit, type of B (the table key)
  BK : 1 bit, determines if B is a constant index  
  CK : 1 bit, determines if C is a constant index  

*/

#define CREATE_OPSPEC_SETTAB(key,bk,ck) \
        (((ck)<<4) | ((bk)<<3) | (key))

#define GET_OPSPEC_SETTAB_KEY(i) (((i)&MASK1(3,POS_SP))>>POS_SP)
#define SET_OPSPEC_SETTAB_KEY(i,key) ((i) = (((i)&MASK0(3,POS_SP)) | \
    (((key)<<POS_SP)&MASK1(3,POS_SP))))


/* -- ADD, SUB, MUL, DIV, MOD, POW, UNM specialization --------------------

  +----+----+----+-----+
  | CK | BK | IN | OUT |
  +----+----+----+-----+
  msb                lsb

  OUT : 1 bit, determines if A needs a specialization check
  IN  : 2 bit, input type
  BK  : 1 bit, if B is a constant index
  CK  : 1 bit, if C is a constant index

*/

#define CREATE_OPSPEC_ARITH(out,in,bk,ck) \
        (((ck)<<4) | ((bk)<<3) | ((in)<<1) | (out))

#define GET_OPSPEC_ARITH_IN(i) (((i)&MASK1(2,POS_SP+1))>>(POS_SP+1))
#define SET_OPSPEC_ARITH_IN(i,in) ((i) = (((i)&MASK0(2,POS_SP+1)) | \
    (((in)<<(POS_SP+1))&MASK1(2,POS_SP+1))))


/* -- LE & LT specialization ----------------------------------------------

  +----+----+------+
  | CK | BK | TYPE |
  +----+----+------+
  msb            lsb
  
  TYPE : 3 bit, input type
  BK   : 1 bit, if B is a constant index
  CK   : 1 bit, if C is a constant index

*/

#define CREATE_OPSPEC_LESS(type,bk,ck) \
        (((ck)<<4) | ((bk)<<3) | (type))

#define GET_OPSPEC_LESS_TYPE(i) (((i)&MASK1(3,POS_SP))>>POS_SP)
#define SET_OPSPEC_LESS_TYPE(i,t) ((i) = (((i)&MASK0(3,POS_SP)) | \
    (((t)<<POS_SP)&MASK1(3,POS_SP))))

/* ----------------------------------------------------------------------- */

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
} OpCode;


#define NUM_OPCODES (cast(int, OP_EXTRAARG) + 1)



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

LUAI_DDEC const lu_byte luaP_opmodes[NUM_OPCODES];

#define getOpMode(m)  (cast(enum OpMode, luaP_opmodes[m] & 3))
#define getBMode(m) (cast(enum OpArgMask, (luaP_opmodes[m] >> 4) & 3))
#define getCMode(m) (cast(enum OpArgMask, (luaP_opmodes[m] >> 2) & 3))
#define testAMode(m)  (luaP_opmodes[m] & (1 << 6))
#define testTMode(m)  (luaP_opmodes[m] & (1 << 7))


LUAI_DDEC const char *const luaP_opnames[NUM_OPCODES+1];  /* opcode names */


/* number of list items to accumulate before a SETLIST instruction */
#define LFIELDS_PER_FLUSH 50



extern void PrintOp(Instruction i);



/* TODO */
static const char * const SpecNamesOut[2] = { "raw", "chk" };
static const char * const SpecNamesTabKey[5] = { 
"raw", "str", "int", "obj", "chk" };
static const char * const SpecNamesArithIn[4] = { 
"raw", "num", "obj", "chk" };
static const char * const SpecNamesLenIn[4] = { 
"raw", "str", "tab", "chk" };
static const char * const SpecNamesLessType[4] = { 
"raw", "num", "str", "chk" };
extern void PrintSpec(Instruction i);



#endif
