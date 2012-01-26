/*
** Lua virtual machine bytecode specialization
*/

#ifndef lvmspec_h
#define lvmspec_h

#include "lobject.h"


LUAI_FUNC void luaVS_despecialize (lua_State *L, int reg);
LUAI_FUNC void luaVS_despecialize_param (Proto *p, int reg);
LUAI_FUNC void luaVS_specialize (lua_State *L);

/* used in p->paramtypes to indicate polymorphic parameter */
#define LUA_TNOSPEC (-2)

#endif
