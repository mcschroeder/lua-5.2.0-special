/*
** Lua virtual machine bytecode specialization
*/

#ifndef lvmspec_h
#define lvmspec_h

#include "lobject.h"


LUAI_FUNC void luaVS_specialize (lua_State *L, Proto *p, int pc);
LUAI_FUNC void luaVS_despecialize (lua_State *L, Proto *p, int pc, int reg);
LUAI_FUNC void luaVS_despecialize_upvalue_origin (lua_State *L, Proto *p, int idx);


#endif
