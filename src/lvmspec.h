/*
** Lua virtual machine bytecode specialization
*/

#ifndef lvmspec_h
#define lvmspec_h

#include "lobject.h"


LUAI_FUNC void luaVS_specialize (lua_State *L, int reg, int use);
LUAI_FUNC void luaVS_specialize_params (lua_State *L, Proto *p);


#endif
