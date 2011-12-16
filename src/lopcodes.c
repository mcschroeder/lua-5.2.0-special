/*
** $Id: lopcodes.c,v 1.48 2011/04/19 16:22:13 roberto Exp $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE


#include "lopcodes.h"


/* ORDER OP */

LUAI_DDEF const char *const luaP_opnames[NUM_OPCODES+1] = {
#define OPNAME(name,m,a,b,c,sa,t) #name,
  OPDEF(OPNAME)
#undef OPNAME
  NULL
};

// TODO
#define oparg_reg OpArgR
#define oparg_kst OpArgK
#define oparg_upv OpArgU
#define oparg_use OpArgU
#define oparg____ OpArgN

LUAI_DDEF const lu_byte luaP_opmodes[NUM_OPCODES] = {
#define OPMODE(name,m,a,b,c,sa,t) (((t)<<7) | ((sa)<<6) | ((oparg_##b)<<4) | ((oparg_##c)<<2) | (i##m)),
  OPDEF(OPMODE)
#undef OPMODE
};

