/*
** $Id: ldump.c,v 1.19 2011/11/23 17:48:18 lhf Exp $
** save precompiled Lua chunks
** See Copyright Notice in lua.h
*/

#include <stddef.h>

#define ldump_c
#define LUA_CORE

#include "lua.h"

#include "lobject.h"
#include "lstate.h"
#include "lundump.h"
#include "lopcodes.h" // TODO: needed for exptypes; remove if possible

typedef struct {
 lua_State* L;
 lua_Writer writer;
 void* data;
 int strip;
 int status;
} DumpState;

#define DumpMem(b,n,size,D)	DumpBlock(b,(n)*(size),D)
#define DumpVar(x,D)		DumpMem(&x,1,sizeof(x),D)

static void DumpBlock(const void* b, size_t size, DumpState* D)
{
 if (D->status==0)
 {
  lua_unlock(D->L);
  D->status=(*D->writer)(D->L,b,size,D->data);
  lua_lock(D->L);
 }
}

static void DumpChar(int y, DumpState* D)
{
 char x=(char)y;
 DumpVar(x,D);
}

static void DumpInt(int x, DumpState* D)
{
 DumpVar(x,D);
}

static void DumpNumber(lua_Number x, DumpState* D)
{
 DumpVar(x,D);
}

static void DumpVector(const void* b, int n, size_t size, DumpState* D)
{
 DumpInt(n,D);
 DumpMem(b,n,size,D);
}

static void DumpString(const TString* s, DumpState* D)
{
 if (s==NULL)
 {
  size_t size=0;
  DumpVar(size,D);
 }
 else
 {
  size_t size=s->tsv.len+1;		/* include trailing '\0' */
  DumpVar(size,D);
  DumpBlock(getstr(s),size*sizeof(char),D);
 }
}

#define DumpCode(f,D)	 DumpVector(f->code,f->sizecode,sizeof(Instruction),D)

static void DumpFunction(const Proto* f, DumpState* D);

static void DumpConstants(const Proto* f, DumpState* D)
{
 int i,n=f->sizek;
 DumpInt(n,D);
 for (i=0; i<n; i++)
 {
  const TValue* o=&f->k[i];
  DumpChar(ttype(o),D);
  switch (ttype(o))
  {
   case LUA_TNIL:
	break;
   case LUA_TBOOLEAN:
	DumpChar(bvalue(o),D);
	break;
   case LUA_TNUMBER:
	DumpNumber(nvalue(o),D);
	break;
   case LUA_TSTRING:
	DumpString(rawtsvalue(o),D);
	break;
  }
 }
 n=f->sizep;
 DumpInt(n,D);
 for (i=0; i<n; i++) DumpFunction(f->p[i],D);
}

static void DumpUpvalues(const Proto* f, DumpState* D)
{
 int i,n=f->sizeupvalues;
 DumpInt(n,D);
 for (i=0; i<n; i++)
 {
  DumpChar(f->upvalues[i].instack,D);
  DumpChar(f->upvalues[i].idx,D);
 }
}

static void DumpDebug(const Proto* f, DumpState* D)
{
 int i,n;
 DumpString((D->strip) ? NULL : f->source,D);
 n= (D->strip) ? 0 : f->sizelineinfo;
 DumpVector(f->lineinfo,n,sizeof(int),D);
 n= (D->strip) ? 0 : f->sizelocvars;
 DumpInt(n,D);
 for (i=0; i<n; i++)
 {
  DumpString(f->locvars[i].varname,D);
  DumpInt(f->locvars[i].startpc,D);
  DumpInt(f->locvars[i].endpc,D);
 }
 n= (D->strip) ? 0 : f->sizeupvalues;
 DumpInt(n,D);
 for (i=0; i<n; i++) DumpString(f->upvalues[i].name,D);
}

static void DumpReginfos(const Proto *f, DumpState *D)
{
  int i,n=f->sizereginfos;
  DumpInt(n,D);
  for (i=0; i<n; i++)
  {
    RegInfo *reginfo = &f->reginfos[i];
    while (reginfo != NULL) {
      DumpNumber(reginfo->startpc, D);
      DumpNumber(reginfo->endpc, D);
      DumpChar(reginfo->state, D);
      // DumpChar(reginfo->nspec, D);
      DumpChar(reginfo->firstuse, D);
      DumpChar(reginfo->lastuse, D);
      if (reginfo->state == REGINFO_STATE_UNUSED ||
          reginfo->state == REGINFO_STATE_LOCAL_UNUSED) break;
      reginfo = reginfo->next;
    }
    DumpNumber(-42,D); /* magic sentinel number */
  }    
}

static void DumpExptypes(const Proto *f, DumpState *D)
{
  int pc,n=f->sizecode;
  for (pc=0; pc<n; pc++)  
  {
    int size;    
    Instruction i = f->code[pc];
    switch (GET_OPGROUP(i)) {
      case OP_LOADNIL:  size = GETARG_B(i)+1; goto dumpts;
      case OP_CALL:     size = GETARG_C(i)-1; goto dumpts;
      case OP_TFORCALL: size = GETARG_C(i);   goto dumpts;
      case OP_VARARG:   size = GETARG_B(i)-1;
      dumpts:
        if (size < 0) size = 0;
        DumpVector(f->exptypes[pc].ts,size,sizeof(int),D); 
        break;
      default:
        DumpNumber(f->exptypes[pc].t,D);
        break;
    }
  }
}

static void DumpFunction(const Proto* f, DumpState* D)
{
 DumpInt(f->linedefined,D);
 DumpInt(f->lastlinedefined,D);
 DumpChar(f->numparams,D);
 DumpChar(f->is_vararg,D);
 DumpChar(f->maxstacksize,D);
 DumpCode(f,D);
 DumpConstants(f,D);
 DumpUpvalues(f,D);
 DumpReginfos(f,D);
 DumpVector(f->paramtypes,f->numparams,sizeof(int),D);
 DumpExptypes(f,D);
 DumpDebug(f,D);
}

static void DumpHeader(DumpState* D)
{
 lu_byte h[LUAC_HEADERSIZE];
 luaU_header(h);
 DumpBlock(h,LUAC_HEADERSIZE,D);
}

/*
** dump Lua function as precompiled chunk
*/
int luaU_dump (lua_State* L, const Proto* f, lua_Writer w, void* data, int strip)
{
 DumpState D;
 D.L=L;
 D.writer=w;
 D.data=data;
 D.strip=strip;
 D.status=0;
 DumpHeader(&D);
 DumpFunction(f,&D);
 return D.status;
}
