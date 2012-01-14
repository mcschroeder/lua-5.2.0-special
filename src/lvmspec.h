/*
** Lua virtual machine bytecode specialization
*/

#ifndef lvmspec_h
#define lvmspec_h


#include "lobject.h"

LUAI_FUNC void luaVS_specialize(lua_State *L, int reg, int store);
#define luaVS_specialize_load(L,reg) luaVS_specialize(L,reg,0)
#define luaVS_specialize_store(L,reg) luaVS_specialize(L,reg,1)

#endif
