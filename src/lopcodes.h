/*
** $Id: lopcodes.h,v 1.142 2011/07/15 12:50:29 roberto Exp $
** Opcodes for Lua virtual machine
** See Copyright Notice in lua.h
*/

#ifndef lopcodes_h
#define lopcodes_h

#include "llimits.h"

/*===========================================================================
  All instructions are unsigned numbers (32 bit wide).
  There are three possible field layouts:

  +----+----+----+----+
  | B  | C  | A  | OP | iABC
  +----+----+----+----+
  |  (s)Bx  | A  | OP | iABx / iAsBx
  +---------+----+----+
  |      Ax      | OP | iAx
  +--------------+----+
  msb               lsb

  OP : 8 bit operand
  A,B,C : 8 bits
  Ax : 24 bits (A,B and C together)
  Bx : 16 bits (B and C together)
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
#define SIZE_C		8
#define SIZE_B		8
#define SIZE_Bx		(SIZE_C + SIZE_B)
#define SIZE_A		8
#define SIZE_Ax		(SIZE_C + SIZE_B + SIZE_A)

#define SIZE_OP		8

#define POS_OP		0
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

#define GET_OPCODE(i)	(cast(OpCode, ((i)>>POS_OP) & MASK1(SIZE_OP,0)))
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
** invalid register that fits in 8 bits
*/
#define NO_REG		MAXARG_A


/*
** R(x) - register
** K(x) - constant (in constant table)
*/


/*
** grep "ORDER OP" if you change these enums
*/

/* (name, mode,a,b,c,setsa,test) */
#define OPDEF(_) \
_(MOVE_x, ABC,dst,reg,___,1,0) /* Ra:? <- Rb */ \
_(MOVE_r, ABC,dst,reg,___,1,0) /* Ra   <- Rb */ \
\
_(LOADK_x, ABx,dst,kst,___,1,0) /* Ra:? <- Kb */ \
_(LOADK_r, ABx,dst,kst,___,1,0) /* Ra   <- Kb */ \
\
_(LOADKX_x, ABx,dst,___,___,1,0) /* Ra:? <- K(extra arg) */ \
_(LOADKX_r, ABx,dst,___,___,1,0) /* Ra   <- K(extra arg) */ \
\
_(LOADBOOL_x, ABC,dst,use,use,1,0) /* Ra:? <- (Bool)b; if (c) pc++ */ \
_(LOADBOOL_r, ABC,dst,use,use,1,0) /* Ra   <- (Bool)b; if (c) pc++ */ \
\
_(LOADNIL_x, ABC,dst,use,___,1,0) /* R(a...a+b):? <- nil */ \
_(LOADNIL_r, ABC,dst,use,___,1,0) /* R(a...a+b)   <- nil */ \
\
_(GETUPVAL_x, ABC,dst,upv,___,1,0) /* Ra:? <- Ub */ \
_(GETUPVAL_r, ABC,dst,upv,___,1,0) /* Ra   <- Ub */ \
\
_(GETTABUP_xux, ABC,dst,upv,reg,1,0) /* Ra:? <- Ub[Rc:?] */ \
_(GETTABUP_xui, ABC,dst,upv,reg,1,0) /* Ra:? <- Ub[Rc:i] */ \
_(GETTABUP_xus, ABC,dst,upv,reg,1,0) /* Ra:? <- Ub[Rc:s] */ \
_(GETTABUP_xur, ABC,dst,upv,reg,1,0) /* Ra:? <- Ub[Rc]   */ \
_(GETTABUP_rux, ABC,dst,upv,reg,1,0) /* Ra   <- Ub[Rc:?] */ \
_(GETTABUP_rui, ABC,dst,upv,reg,1,0) /* Ra   <- Ub[Rc:s] */ \
_(GETTABUP_rus, ABC,dst,upv,reg,1,0) /* Ra   <- Ub[Rc:s] */ \
_(GETTABUP_rur, ABC,dst,upv,reg,1,0) /* Ra   <- Ub[Rc]   */ \
_(GETTABUP_xuI, ABC,dst,upv,kst,1,0) /* Ra:? <- Ub[Kc:i] */ \
_(GETTABUP_xuF, ABC,dst,upv,kst,1,0) /* Ra:? <- Ub[Kc:f] */ \
_(GETTABUP_xuS, ABC,dst,upv,kst,1,0) /* Ra:? <- Ub[Kc:s] */ \
/*_(GETTABUP_ruI, ABC,dst,upv,kst,1,0)*/ /* Ra   <- Ub[Kc:i] */ \
/*_(GETTABUP_ruF, ABC,dst,upv,kst,1,0)*/ /* Ra   <- Ub[Kc:f] */ \
/*_(GETTABUP_ruS, ABC,dst,upv,kst,1,0)*/ /* Ra   <- Ub[Kc:s] */ \
/* TODO: remove the following */ \
_(GETTABUP_ruK, ABC,dst,upv,kst,1,0) /* Ra   <- Ub[Kc]   */ \
\
_(GETTABLE_xrx, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb[Rc:?] */ \
_(GETTABLE_xri, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb[Rc:i] */ \
_(GETTABLE_xrs, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb[Rc:s] */ \
_(GETTABLE_xrr, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb[Rc]   */ \
_(GETTABLE_rrx, ABC,dst,reg,reg,1,0) /* Ra   <- Rb[Rc:?] */ \
_(GETTABLE_rri, ABC,dst,reg,reg,1,0) /* Ra   <- Rb[Rc:s] */ \
_(GETTABLE_rrs, ABC,dst,reg,reg,1,0) /* Ra   <- Rb[Rc:s] */ \
_(GETTABLE_rrr, ABC,dst,reg,reg,1,0) /* Ra   <- Rb[Rc]   */ \
_(GETTABLE_xrI, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb[Kc:i] */ \
_(GETTABLE_xrF, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb[Kc:f] */ \
_(GETTABLE_xrS, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb[Kc:s] */ \
/*_(GETTABLE_rrI, ABC,dst,reg,kst,1,0)*/ /* Ra   <- Rb[Kc:i] */ \
/*_(GETTABLE_rrF, ABC,dst,reg,kst,1,0)*/ /* Ra   <- Rb[Kc:f] */ \
/*_(GETTABLE_rrS, ABC,dst,reg,kst,1,0)*/ /* Ra   <- Rb[Kc:s] */ \
/* TODO: remove the following */ \
_(GETTABLE_rrK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb[Kc]   */ \
\
_(SETTABUP_uxr, ABC,upv,reg,reg,0,0) /* Ua[Rb:?] <- Rc */ \
_(SETTABUP_uir, ABC,upv,reg,reg,0,0) /* Ua[Rb:i] <- Rc */ \
_(SETTABUP_usr, ABC,upv,reg,reg,0,0) /* Ua[Rb:s] <- Rc */ \
_(SETTABUP_urr, ABC,upv,reg,reg,0,0) /* Ua[Rb]   <- Rc */ \
_(SETTABUP_uxK, ABC,upv,reg,kst,0,0) /* Ua[Rb:?] <- Kc */ \
_(SETTABUP_uiK, ABC,upv,reg,kst,0,0) /* Ua[Rb:i] <- Kc */ \
_(SETTABUP_usK, ABC,upv,reg,kst,0,0) /* Ua[Rb:s] <- Kc */ \
_(SETTABUP_urK, ABC,upv,reg,kst,0,0) /* Ua[Rb]   <- Kc */ \
/*_(SETTABUP_uIr, ABC,upv,kst,reg,0,0)*/ /* Ua[Kb:i] <- Rc */ \
/*_(SETTABUP_uFr, ABC,upv,kst,reg,0,0)*/ /* Ua[Kb:f] <- Rc */ \
/*_(SETTABUP_uSr, ABC,upv,kst,reg,0,0)*/ /* Ua[Kb:s] <- Rc */ \
/*_(SETTABUP_uIK, ABC,upv,kst,kst,0,0)*/ /* Ua[Kb:i] <- Kc */ \
/*_(SETTABUP_uFK, ABC,upv,kst,kst,0,0)*/ /* Ua[Kb:f] <- Kc */ \
/*_(SETTABUP_uSK, ABC,upv,kst,kst,0,0)*/ /* Ua[Kb:s] <- Kc */ \
/* TODO: remove the following two */ \
_(SETTABUP_uKr, ABC,upv,kst,reg,0,0) /* Ua[Kb]   <- Rc */ \
_(SETTABUP_uKK, ABC,upv,kst,kst,0,0) /* Ua[Kb]   <- Kc */ \
\
_(SETUPVAL, ABC,reg,upv,___,0,0) /* Ub <- Ra */ \
\
_(SETTABLE_rxr, ABC,reg,reg,reg,0,0) /* Ra[Rb:?] <- Rc */ \
_(SETTABLE_rir, ABC,reg,reg,reg,0,0) /* Ra[Rb:i] <- Rc */ \
_(SETTABLE_rsr, ABC,reg,reg,reg,0,0) /* Ra[Rb:s] <- Rc */ \
_(SETTABLE_rrr, ABC,reg,reg,reg,0,0) /* Ra[Rb]   <- Rc */ \
_(SETTABLE_rxK, ABC,reg,reg,kst,0,0) /* Ra[Rb:?] <- Kc */ \
_(SETTABLE_riK, ABC,reg,reg,kst,0,0) /* Ra[Rb:i] <- Kc */ \
_(SETTABLE_rsK, ABC,reg,reg,kst,0,0) /* Ra[Rb:s] <- Kc */ \
_(SETTABLE_rrK, ABC,reg,reg,kst,0,0) /* Ra[Rb]   <- Kc */ \
/*_(SETTABLE_rIr, ABC,reg,kst,reg,0,0)*/ /* Ra[Kb:i] <- Rc */ \
/*_(SETTABLE_rFr, ABC,reg,kst,reg,0,0)*/ /* Ra[Kb:f] <- Rc */ \
/*_(SETTABLE_rSr, ABC,reg,kst,reg,0,0)*/ /* Ra[Kb:s] <- Rc */ \
/*_(SETTABLE_rIK, ABC,reg,kst,kst,0,0)*/ /* Ra[Kb:i] <- Kc */ \
/*_(SETTABLE_rFK, ABC,reg,kst,kst,0,0)*/ /* Ra[Kb:f] <- Kc */ \
/*_(SETTABLE_rSK, ABC,reg,kst,kst,0,0)*/ /* Ra[Kb:s] <- Kc */ \
/* TODO: remove the following two */ \
_(SETTABLE_rKr, ABC,reg,kst,reg,0,0) /* Ra[Kb]   <- Rc */ \
_(SETTABLE_rKK, ABC,reg,kst,kst,0,0) /* Ra[Kb]   <- Kc */ \
\
_(NEWTABLE_x, ABC,dst,use,use,1,0) /* Ra:? <- {} (size=b,c) */ \
_(NEWTABLE_r, ABC,dst,use,use,1,0) /* Ra   <- {} (size=b,c) */ \
\
_(SELF_xr, ABC,dst,reg,reg,1,0) /* R(a+1) <- Rb; Ra:? <- Rb[Rc] */ \
_(SELF_rr, ABC,dst,reg,reg,1,0) /* R(a+1) <- Rb; Ra   <- Rb[Rc] */ \
_(SELF_xK, ABC,dst,reg,kst,1,0) /* R(a+1) <- Rb; Ra:? <- Rb[Kc] */ \
_(SELF_rK, ABC,dst,reg,kst,1,0) /* R(a+1) <- Rb; Ra   <- Rb[Kc] */ \
\
_(ADD_xxx, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:? + Rc:? */ \
_(ADD_xnn, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:n + Rc:n */ \
_(ADD_xrr, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb   + Rc   */ \
_(ADD_rxx, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:? + Rc:? */ \
_(ADD_rnn, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:n + Rc:n */ \
_(ADD_rrr, ABC,dst,reg,reg,1,0) /* Ra   <- Rb   + Rc   */ \
_(ADD_xxK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:? + Kc   */ \
_(ADD_xnN, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:n + Kc:n */ \
_(ADD_xrK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb   + Kc   */ \
_(ADD_rxK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:? + Kc   */ \
_(ADD_rnN, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:n + Rc:n */ \
_(ADD_rrK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb   + Kc   */ \
_(ADD_xKx, ABC,dst,kst,reg,1,0) /* Ra:? <- Kc   + Rb:? */ \
_(ADD_xNn, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb:n + Rc:n */ \
_(ADD_xKr, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb   + Rc   */ \
_(ADD_rKx, ABC,dst,kst,reg,1,0) /* Ra   <- Kc   + Rb:? */ \
_(ADD_rNn, ABC,dst,kst,reg,1,0) /* Ra   <- Kb:n + Rc:n */ \
_(ADD_rKr, ABC,dst,kst,reg,1,0) /* Ra   <- Kb   + Rc   */ \
_(ADD_xKK, ABC,dst,kst,kst,1,0) /* Ra:? <- Kb   + Kc   */ \
_(ADD_rKK, ABC,dst,kst,kst,1,0) /* Ra   <- Kb   + Kc   */ \
\
_(SUB_xxx, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:? - Rc:? */ \
_(SUB_xnn, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:n - Rc:n */ \
_(SUB_xrr, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb   - Rc   */ \
_(SUB_rxx, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:? - Rc:? */ \
_(SUB_rnn, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:n - Rc:n */ \
_(SUB_rrr, ABC,dst,reg,reg,1,0) /* Ra   <- Rb   - Rc   */ \
_(SUB_xxK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:? - Kc   */ \
_(SUB_xnN, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:n - Kc:n */ \
_(SUB_xrK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb   - Kc   */ \
_(SUB_rxK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:? - Kc   */ \
_(SUB_rnN, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:n - Rc:n */ \
_(SUB_rrK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb   - Kc   */ \
_(SUB_xKx, ABC,dst,kst,reg,1,0) /* Ra:? <- Kc   - Rb:? */ \
_(SUB_xNn, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb:n - Rc:n */ \
_(SUB_xKr, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb   - Rc   */ \
_(SUB_rKx, ABC,dst,kst,reg,1,0) /* Ra   <- Kc   - Rb:? */ \
_(SUB_rNn, ABC,dst,kst,reg,1,0) /* Ra   <- Kb:n - Rc:n */ \
_(SUB_rKr, ABC,dst,kst,reg,1,0) /* Ra   <- Kb   - Rc   */ \
_(SUB_xKK, ABC,dst,kst,kst,1,0) /* Ra:? <- Kb   - Kc   */ \
_(SUB_rKK, ABC,dst,kst,kst,1,0) /* Ra   <- Kb   - Kc   */ \
\
_(MUL_xxx, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:? * Rc:? */ \
_(MUL_xnn, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:n * Rc:n */ \
_(MUL_xrr, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb   * Rc   */ \
_(MUL_rxx, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:? * Rc:? */ \
_(MUL_rnn, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:n * Rc:n */ \
_(MUL_rrr, ABC,dst,reg,reg,1,0) /* Ra   <- Rb   * Rc   */ \
_(MUL_xxK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:? * Kc   */ \
_(MUL_xnN, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:n * Kc:n */ \
_(MUL_xrK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb   * Kc   */ \
_(MUL_rxK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:? * Kc   */ \
_(MUL_rnN, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:n * Rc:n */ \
_(MUL_rrK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb   * Kc   */ \
_(MUL_xKx, ABC,dst,kst,reg,1,0) /* Ra:? <- Kc   * Rb:? */ \
_(MUL_xNn, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb:n * Rc:n */ \
_(MUL_xKr, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb   * Rc   */ \
_(MUL_rKx, ABC,dst,kst,reg,1,0) /* Ra   <- Kc   * Rb:? */ \
_(MUL_rNn, ABC,dst,kst,reg,1,0) /* Ra   <- Kb:n * Rc:n */ \
_(MUL_rKr, ABC,dst,kst,reg,1,0) /* Ra   <- Kb   * Rc   */ \
_(MUL_xKK, ABC,dst,kst,kst,1,0) /* Ra:? <- Kb   * Kc   */ \
_(MUL_rKK, ABC,dst,kst,kst,1,0) /* Ra   <- Kb   * Kc   */ \
\
_(DIV_xxx, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:? / Rc:? */ \
_(DIV_xnn, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:n / Rc:n */ \
_(DIV_xrr, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb   / Rc   */ \
_(DIV_rxx, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:? / Rc:? */ \
_(DIV_rnn, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:n / Rc:n */ \
_(DIV_rrr, ABC,dst,reg,reg,1,0) /* Ra   <- Rb   / Rc   */ \
_(DIV_xxK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:? / Kc   */ \
_(DIV_xnN, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:n / Kc:n */ \
_(DIV_xrK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb   / Kc   */ \
_(DIV_rxK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:? / Kc   */ \
_(DIV_rnN, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:n / Rc:n */ \
_(DIV_rrK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb   / Kc   */ \
_(DIV_xKx, ABC,dst,kst,reg,1,0) /* Ra:? <- Kc   / Rb:? */ \
_(DIV_xNn, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb:n / Rc:n */ \
_(DIV_xKr, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb   / Rc   */ \
_(DIV_rKx, ABC,dst,kst,reg,1,0) /* Ra   <- Kc   / Rb:? */ \
_(DIV_rNn, ABC,dst,kst,reg,1,0) /* Ra   <- Kb:n / Rc:n */ \
_(DIV_rKr, ABC,dst,kst,reg,1,0) /* Ra   <- Kb   / Rc   */ \
_(DIV_xKK, ABC,dst,kst,kst,1,0) /* Ra:? <- Kb   / Kc   */ \
_(DIV_rKK, ABC,dst,kst,kst,1,0) /* Ra   <- Kb   / Kc   */ \
\
_(MOD_xxx, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:? % Rc:? */ \
_(MOD_xnn, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:n % Rc:n */ \
_(MOD_xrr, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb   % Rc   */ \
_(MOD_rxx, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:? % Rc:? */ \
_(MOD_rnn, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:n % Rc:n */ \
_(MOD_rrr, ABC,dst,reg,reg,1,0) /* Ra   <- Rb   % Rc   */ \
_(MOD_xxK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:? % Kc   */ \
_(MOD_xnN, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:n % Kc:n */ \
_(MOD_xrK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb   % Kc   */ \
_(MOD_rxK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:? % Kc   */ \
_(MOD_rnN, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:n % Rc:n */ \
_(MOD_rrK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb   % Kc   */ \
_(MOD_xKx, ABC,dst,kst,reg,1,0) /* Ra:? <- Kc   % Rb:? */ \
_(MOD_xNn, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb:n % Rc:n */ \
_(MOD_xKr, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb   % Rc   */ \
_(MOD_rKx, ABC,dst,kst,reg,1,0) /* Ra   <- Kc   % Rb:? */ \
_(MOD_rNn, ABC,dst,kst,reg,1,0) /* Ra   <- Kb:n % Rc:n */ \
_(MOD_rKr, ABC,dst,kst,reg,1,0) /* Ra   <- Kb   % Rc   */ \
_(MOD_xKK, ABC,dst,kst,kst,1,0) /* Ra:? <- Kb   % Kc   */ \
_(MOD_rKK, ABC,dst,kst,kst,1,0) /* Ra   <- Kb   % Kc   */ \
\
_(POW_xxx, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:? ^ Rc:? */ \
_(POW_xnn, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb:n ^ Rc:n */ \
_(POW_xrr, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb   ^ Rc   */ \
_(POW_rxx, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:? ^ Rc:? */ \
_(POW_rnn, ABC,dst,reg,reg,1,0) /* Ra   <- Rb:n ^ Rc:n */ \
_(POW_rrr, ABC,dst,reg,reg,1,0) /* Ra   <- Rb   ^ Rc   */ \
_(POW_xxK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:? ^ Kc   */ \
_(POW_xnN, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb:n ^ Kc:n */ \
_(POW_xrK, ABC,dst,reg,kst,1,0) /* Ra:? <- Rb   ^ Kc   */ \
_(POW_rxK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:? ^ Kc   */ \
_(POW_rnN, ABC,dst,reg,kst,1,0) /* Ra   <- Rb:n ^ Rc:n */ \
_(POW_rrK, ABC,dst,reg,kst,1,0) /* Ra   <- Rb   ^ Kc   */ \
_(POW_xKx, ABC,dst,kst,reg,1,0) /* Ra:? <- Kc   ^ Rb:? */ \
_(POW_xNn, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb:n ^ Rc:n */ \
_(POW_xKr, ABC,dst,kst,reg,1,0) /* Ra:? <- Kb   ^ Rc   */ \
_(POW_rKx, ABC,dst,kst,reg,1,0) /* Ra   <- Kc   ^ Rb:? */ \
_(POW_rNn, ABC,dst,kst,reg,1,0) /* Ra   <- Kb:n ^ Rc:n */ \
_(POW_rKr, ABC,dst,kst,reg,1,0) /* Ra   <- Kb   ^ Rc   */ \
_(POW_xKK, ABC,dst,kst,kst,1,0) /* Ra:? <- Kb   ^ Kc   */ \
_(POW_rKK, ABC,dst,kst,kst,1,0) /* Ra   <- Kb   ^ Kc   */ \
\
_(UNM_xx, ABC,dst,reg,___,1,0) /* Ra:? <- -Rb:? */ \
_(UNM_xn, ABC,dst,reg,___,1,0) /* Ra:? <- -Rb:n */ \
_(UNM_xr, ABC,dst,reg,___,1,0) /* Ra:? <- -Rb   */ \
_(UNM_rx, ABC,dst,reg,___,1,0) /* Ra   <- -Rb:? */ \
_(UNM_rn, ABC,dst,reg,___,1,0) /* Ra   <- -Rb:n */ \
_(UNM_rr, ABC,dst,reg,___,1,0) /* Ra   <- -Rb   */ \
\
_(NOT_xr, ABC,dst,reg,___,1,0) /* Ra:? <- not Rb */ \
_(NOT_rr, ABC,dst,reg,___,1,0) /* Ra   <- not Rb */ \
\
_(LEN_xx, ABC,dst,reg,___,1,0) /* Ra:? <- #Rb:? */ \
_(LEN_xs, ABC,dst,reg,___,1,0) /* Ra:? <- #Rb:s */ \
_(LEN_xt, ABC,dst,reg,___,1,0) /* Ra:? <- #Rb:t */ \
_(LEN_xr, ABC,dst,reg,___,1,0) /* Ra:? <- #Rb   */ \
_(LEN_rx, ABC,dst,reg,___,1,0) /* Ra   <- #Rb:? */ \
_(LEN_rs, ABC,dst,reg,___,1,0) /* Ra   <- #Rb:s */ \
_(LEN_rt, ABC,dst,reg,___,1,0) /* Ra   <- #Rb:t */ \
_(LEN_rr, ABC,dst,reg,___,1,0) /* Ra   <- #Rb   */ \
\
_(CONCAT_x, ABC,dst,reg,reg,1,0) /* Ra:? <- Rb.. ... ..Rc */ \
_(CONCAT_r, ABC,dst,reg,reg,1,0) /* Ra   <- Rb.. ... ..Rc */ \
\
_(JMP, AsBx,use,use,___,0,0) /* pc += b; if (a) close upvalues >= U(a-1) */ \
\
_(EQ_rr, ABC,use,reg,reg,0,1) /* if ((Rb == Rc) != a) then pc++ */ \
_(EQ_rK, ABC,use,reg,kst,0,1) /* if ((Rb == Kc) != a) then pc++ */ \
_(EQ_KK, ABC,use,kst,kst,0,1) /* if ((Kb == Kc) != a) then pc++ */ \
\
_(LT_xx, ABC,use,reg,reg,0,1) /* if ((Rb:? < Rc:?) != a) then pc++ */ \
_(LT_nn, ABC,use,reg,reg,0,1) /* if ((Rb:n < Rc:n) != a) then pc++ */ \
_(LT_ss, ABC,use,reg,reg,0,1) /* if ((Rb:s < Rc:s) != a) then pc++ */ \
_(LT_rr, ABC,use,reg,reg,0,1) /* if ((Rb   < Rc  ) != a) then pc++ */ \
_(LT_xK, ABC,use,reg,kst,0,1) /* if ((Rb:? < Kc  ) != a) then pc++ */ \
_(LT_nN, ABC,use,reg,kst,0,1) /* if ((Rb:n < Kc:n) != a) then pc++ */ \
_(LT_sS, ABC,use,reg,kst,0,1) /* if ((Rb:s < Kc:s) != a) then pc++ */ \
_(LT_rK, ABC,use,reg,kst,0,1) /* if ((Rb   < Kc  ) != a) then pc++ */ \
_(LT_NN, ABC,use,kst,kst,0,1) /* if ((Kb:n < Kc:n) != a) then pc++ */ \
_(LT_SS, ABC,use,kst,kst,0,1) /* if ((Kb:s < Kc:s) != a) then pc++ */ \
_(LT_KK, ABC,use,kst,kst,0,1) /* if ((Kb   < Kc  ) != a) then pc++ */ \
\
_(LE_xx, ABC,use,reg,reg,0,1) /* if ((Rb:? <= Rc:?) != a) then pc++ */ \
_(LE_nn, ABC,use,reg,reg,0,1) /* if ((Rb:n <= Rc:n) != a) then pc++ */ \
_(LE_ss, ABC,use,reg,reg,0,1) /* if ((Rb:s <= Rc:s) != a) then pc++ */ \
_(LE_rr, ABC,use,reg,reg,0,1) /* if ((Rb   <= Rc  ) != a) then pc++ */ \
_(LE_xK, ABC,use,reg,kst,0,1) /* if ((Rb:? <= Kc  ) != a) then pc++ */ \
_(LE_nN, ABC,use,reg,kst,0,1) /* if ((Rb:n <= Kc:n) != a) then pc++ */ \
_(LE_sS, ABC,use,reg,kst,0,1) /* if ((Rb:s <= Kc:s) != a) then pc++ */ \
_(LE_rK, ABC,use,reg,kst,0,1) /* if ((Rb   <= Kc  ) != a) then pc++ */ \
_(LE_NN, ABC,use,kst,kst,0,1) /* if ((Kb:n <= Kc:n) != a) then pc++ */ \
_(LE_SS, ABC,use,kst,kst,0,1) /* if ((Kb:s <= Kc:s) != a) then pc++ */ \
_(LE_KK, ABC,use,kst,kst,0,1) /* if ((Kb   <= Kc  ) != a) then pc++ */ \
\
_(TEST, ABC,reg,___,use,0,1) /* if not (Ra != c) pc++ */  \
\
_(TESTSET_x, ABC,dst,reg,use,1,1) /* if (Rb != c) Ra:? <- Rb else pc++ */ \
_(TESTSET_r, ABC,dst,reg,use,1,1) /* if (Rb != c) Ra   <- Rb else pc++ */ \
\
_(CALL_xx, ABC,dst,use,use,1,0) /* R(a...a+c-2):? <- Ra:?(R(a+1...a+b-1)) */ \
_(CALL_xl, ABC,dst,use,use,1,0) /* R(a...a+c-2):? <- Ra:l(R(a+1...a+b-1)) */ \
_(CALL_xc, ABC,dst,use,use,1,0) /* R(a...a+c-2):? <- Ra:c(R(a+1...a+b-1)) */ \
_(CALL_xr, ABC,dst,use,use,1,0) /* R(a...a+c-2):? <- Ra  (R(a+1...a+b-1)) */ \
_(CALL_rx, ABC,dst,use,use,1,0) /* R(a...a+c-2)   <- Ra:?(R(a+1...a+b-1)) */ \
_(CALL_rl, ABC,dst,use,use,1,0) /* R(a...a+c-2)   <- Ra:l(R(a+1...a+b-1)) */ \
_(CALL_rc, ABC,dst,use,use,1,0) /* R(a...a+c-2)   <- Ra:c(R(a+1...a+b-1)) */ \
_(CALL_rr, ABC,dst,use,use,1,0) /* R(a...a+c-2)   <- Ra  (R(a+1...a+b-1)) */ \
\
_(TAILCALL_x, ABC,dst,use,use,1,0) /* return Ra:?(R(a+1...a+b-1)) */ \
_(TAILCALL_l, ABC,dst,use,use,1,0) /* return Ra:l(R(a+1...a+b-1)) */ \
_(TAILCALL_c, ABC,dst,use,use,1,0) /* return Ra:c(R(a+1...a+b-1)) */ \
_(TAILCALL_r, ABC,dst,use,use,1,0) /* return Ra  (R(a+1...a+b-1)) */ \
\
_(RETURN, ABC,reg,use,___,0,0) /* return R(a...a+b-2) */ \
\
_(FORLOOP, AsBx,dst,reg,___,1,0) /* Ra += R(a+2); if (Ra <= R(a+1)) then 
                                    { pc += b; R(a+3) <- R(a) } */ \
_(FORPREP, AsBx,dst,reg,___,1,0) /* Ra -= R(a+2); pc += b */ \
\
_(TFORCALL, ABC,reg,___,use,0,0) /* R(a+3...a+2+c) <- Ra(R(a+1),R(a+2)) */ \
_(TFORLOOP, ABC,dst,reg,___,1,0) /* if (R(a+1) != nil) then 
                                    { Ra <- R(a+1); pc += b } */ \
\
_(SETLIST, ABC,reg,use,use,0,0) /* Ra[(c-1)*FPF+i] <- R(a+i), 1 <= i <= b */ \
\
_(CLOSURE_x, ABx,dst,use,___,1,0) /* Ra:? <- closure(b) */ \
_(CLOSURE_r, ABx,dst,use,___,1,0) /* Ra   <- closure(b) */ \
\
_(VARARG_x, ABC,dst,use,___,1,0) /* R(a+1...a+b-2):? = vararg */ \
_(VARARG_r, ABC,dst,use,___,1,0) /* R(a+1...a+b-2)   = vararg */ \
\
_(EXTRAARG, Ax,use,use,use,0,0) /* extra argument for previous opcode */

typedef enum {
#define OPENUM(name,m,a,b,c,sa,t) OP_##name,
  OPDEF(OPENUM)
#undef OPENUM 
  NUM_OPCODES 
} OpCode;

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

#define getOpMode(m)	(cast(enum OpMode, luaP_opmodes[m] & 3))
#define getBMode(m)	(cast(enum OpArgMask, (luaP_opmodes[m] >> 4) & 3))
#define getCMode(m)	(cast(enum OpArgMask, (luaP_opmodes[m] >> 2) & 3))
#define testAMode(m)	(luaP_opmodes[m] & (1 << 6))
#define testTMode(m)	(luaP_opmodes[m] & (1 << 7))


LUAI_DDEC const char *const luaP_opnames[NUM_OPCODES+1];  /* opcode names */


/* number of list items to accumulate before a SETLIST instruction */
#define LFIELDS_PER_FLUSH	50


#endif
