{
  This unit is part of the Lua4Delphi Source Code

  Copyright (C) 2009-2012, LaKraven Studios Ltd.
  Copyright Protection Packet(s): L4D014

  www.Lua4Delphi.com
  www.LaKraven.com
  --------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
  the License for the specific language governing rights and
  limitations under the License.
  --------------------------------------------------------------------
  Unit: L4D.Lua.StaticLua51.pas
  Released: 5th February 2012

  Changelog:
    22nd February 2012:
      - Added method "lua_pushliteral" (initially forgotten)
      - Added support for "direct-to-file" API call logging!
    5th February 2012:
      - Released
}
unit L4D.Lua.StaticLua51;

interface

{$I Lua4Delphi.inc}

{$WEAKPACKAGEUNIT ON}

uses
  L4D.Lua.Intf, L4D.Lua.Lua51, L4D.Lua.Constants{$IFDEF L4D_API_LOGGING}, L4D.Debug.Logging{$ENDIF};
type
  {
    TLua51Static
      - Lua 5.1 Static Link
  }
  {$REGION 'TLua51Static'}
    TLua51Static = class(TLua51Common)
    protected
      procedure SetLuaLibRefs; override;
      {$REGION 'ILuaLibCommon'}
        function lua_atpanic(L: PLuaState; panicf: TLuaDelphiFunction): TLuaDelphiFunction; override;
        function lua_checkstack(L: PLuaState; sz: Integer): LongBool; override;
        procedure lua_close(L: PLuaState); override;
        procedure lua_concat(L: PLuaState; n: Integer); override;
        procedure lua_createtable(L: PLuaState; narr, nrec: Integer); override;
        function lua_dump(L: PLuaState; writer: TLuaWriterFunction; data: Pointer): Integer; override;
        function lua_error(L: PLuaState): Integer; override;
        function lua_gc(L: PLuaState; what, data: Integer): Integer; override;
        function lua_getallocf(L: PLuaState; ud: PPointer): TLuaAllocFunction; override;
        procedure lua_getfield(L: PLuaState; idx: Integer; k: PAnsiChar); override;
        function lua_gethook(L: PLuaState): TLuaHookFunction; override;
        function lua_gethookcount(L: PLuaState): Integer; override;
        function lua_gethookmask(L: PLuaState): Integer; override;
        function lua_getinfo(L: PLuaState; const what: PAnsiChar; ar: PLuaDebug): Integer; override;
        function lua_getlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar; override;
        function lua_getmetatable(L: PLuaState; objindex: Integer): LongBool; override;
        function lua_getstack(L: PLuaState; level: Integer; ar: PLuaDebug): Integer; override;
        procedure lua_gettable(L: PLuaState ; idx: Integer); override;
        function lua_gettop(L: PLuaState): Integer; override;
        function lua_getupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar; override;
        procedure lua_insert(L: PLuaState; idx: Integer); override;
        function lua_iscfunction(L: PLuaState; idx: Integer): LongBool; override;
        function lua_isnumber(L: PLuaState; idx: Integer): LongBool; override;
        function lua_isstring(L: PLuaState; idx: Integer): LongBool; override;
        function lua_isuserdata(L: PLuaState; idx: Integer): LongBool; override;
        function lua_newthread(L: PLuaState): PLuaState; override;
      public
        function lua_newstate(f: TLuaAllocFunction; ud: Pointer): PLuaState; override;
      protected
        function lua_newuserdata(L: PLuaState; sz: Cardinal): Pointer; override;
        function lua_next(L: PLuaState; idx: Integer): Integer; override;
        procedure lua_pushboolean(L: PLuaState; b: LongBool); override;
        procedure lua_pushcclosure(L: PLuaState; fn: TLuaDelphiFunction; n: Integer); override;
        function lua_pushfstring(L: PLuaState; const fmt: PAnsiChar): PAnsiChar; {varargs;} override;
        procedure lua_pushinteger(L: PLuaState; n: Integer); override;
        procedure lua_pushlightuserdata(L: PLuaState; p: Pointer); override;
        procedure lua_pushnil(L: PLuaState); override;
        procedure lua_pushnumber(L: PLuaState; n: Double); override;
        function lua_pushstring(L: PLuaState; const s: PAnsiChar): PAnsiChar; override;
        function lua_pushthread(L: PLuaState): LongBool; override;
        procedure lua_pushvalue(L: PLuaState; idx: Integer); override;
        function lua_pushvfstring(L: PLuaState; const fmt: PAnsiChar; argp: Pointer): PAnsiChar; override;
        function lua_rawequal(L: PLuaState; idx1, idx2: Integer): LongBool; override;
        procedure lua_rawget(L: PLuaState; idx: Integer); override;
        procedure lua_rawgeti(L: PLuaState; idx, n: Integer); override;
        procedure lua_rawset(L: PLuaState; idx: Integer); override;
        procedure lua_rawseti(L: PLuaState; idx , n: Integer); override;
        procedure lua_remove(L: PLuaState; idx: Integer); override;
        procedure lua_replace(L: PLuaState; idx: Integer); override;
        procedure lua_setallocf(L: PLuaState; f: TLuaAllocFunction; ud: Pointer); override;
        procedure lua_setfield(L: PLuaState; idx: Integer; const k: PAnsiChar); override;
        function lua_sethook(L: PLuaState; func: TLuaHookFunction; mask, count: Integer): Integer; override;
        function lua_setlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar; override;
        procedure lua_settable(L: PLuaState; idx: Integer); override;
        procedure lua_settop(L: PLuaState; idx: Integer); override;
        function lua_setupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar; override;
        function lua_status(L: PLuaState): Integer; override;
        function lua_toboolean(L: PLuaState; idx: Integer): LongBool; override;
        function lua_tocfunction(L: PLuaState; idx: Integer): TLuaDelphiFunction; override;
        function lua_tolstring(L: PLuaState; idx: Integer; len: PCardinal): PAnsiChar; override;
        function lua_topointer(L: PLuaState; idx: Integer): Pointer; override;
        function lua_tothread(L: PLuaState; idx: Integer): PLuaState; override;
        function lua_touserdata(L: PLuaState; idx: Integer): Pointer; override;
        function lua_type(L: PLuaState; idx: Integer): Integer; override;
        function lua_typename(L: PLuaState; tp: Integer): PAnsiChar; override;
        procedure lua_xmove(src, dest: PLuaState; n: Integer); override;
        function luaopen_base(L: PLuaState): Integer; override;
        function luaopen_debug(L: PLuaState): Integer; override;
        function luaopen_io(L: PLuaState): Integer; override;
        function luaopen_math(L: PLuaState): Integer; override;
        function luaopen_os(L: PLuaState): Integer; override;
        function luaopen_package(L: PLuaState): Integer; override;
        function luaopen_string(L: PLuaState): Integer; override;
        function luaopen_table(L: PLuaState): Integer; override;
      {$ENDREGION}
      {$REGION 'ILuaAuxCommon'}
        procedure luaL_addlstring(B: PLuaLBuffer; const s: PAnsiChar; ls: Cardinal); override;
        procedure luaL_addstring(B: PLuaLBuffer; const s: PAnsiChar); override;
        procedure luaL_addvalue(B: PLuaLBuffer); override;
        function luaL_argerror(L: PLuaState; numarg: Integer; const extramsg: PAnsiChar): Integer; override;
        procedure luaL_buffinit(L: PLuaState; B: PLuaLBuffer); override;
        function luaL_callmeta(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer; override;
        procedure luaL_checkany(L: PLuaState; narg: Integer); override;
        function luaL_checkinteger(L: PLuaState; numArg: Integer): Integer; override;
        function luaL_checklstring(L: PLuaState; numArg: Integer; ls: PCardinal): PAnsiChar; override;
        function luaL_checknumber(L: PLuaState; numArg: Integer): Double; override;
        function luaL_checkoption(L: PLuaState; narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer; override;
        procedure luaL_checkstack(L: PLuaState; sz: Integer; const msg: PAnsiChar); override;
        procedure luaL_checktype(L: PLuaState; narg, t: Integer); override;
        function luaL_checkudata(L: PLuaState; ud: Integer; const tname: PAnsiChar): Pointer; override;
        function luaL_error(L: PLuaState; const fmt: PAnsiChar): Integer; {varargs;} override;
        function luaL_getmetafield(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer; override;
        function luaL_gsub(L: PLuaState; const s, p, r: PAnsiChar): PAnsiChar; override;
        function luaL_loadstring(L: PLuaState; const s: PAnsiChar): Integer; override;
        function luaL_newmetatable(L: PLuaState; const tname: PAnsiChar): Integer; override;
        function luaL_newstate: PLuaState; override;
        procedure luaL_openlibs(L: PLuaState); override;
        function luaL_optinteger(L: PLuaState; nArg: Integer; def: Integer): Integer; override;
        function luaL_optlstring(L: PLuaState; numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar; override;
        function luaL_optnumber(L: PLuaState; nArg: Integer; def: Double): Double; override;
        procedure luaL_pushresult(B: PLuaLBuffer); override;
        function luaL_ref(L: PLuaState; t: Integer): Integer; override;
        procedure luaL_unref(L: PLuaState; t, ref: Integer); override;
        procedure luaL_where(L: PLuaState; lvl: Integer); override;
      {$ENDREGION}
      {$REGION 'IluaInterchange'}
        procedure lua_call(L: PLuaState; nargs, nresults: Integer); override;                                 // External "lua_call" in 5.1, External "lua_callk" in 5.2
        function lua_cpcall(L: PLuaState; func: TLuaDelphiFunction; ud: Pointer): Integer; override;          // External in 5.1, "lua_pcall" in 5.2
        function lua_equal(L: PLuaState; idx1, idx2: Integer): LongBool; override;                            // External in 5.1, "lua_compare" in 5.2
        procedure lua_getfenv(L: PLuaState; idx: Integer); override;                                          // External in 5.1, "lua_getuservalue" in 5.2
        function lua_lessthan(L: PLuaState; idx1, idx2: Integer): LongBool; override;                         // External in 5.1, "lua_compare" in 5.2
        function lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer; overload; override; // Extra parameter in 5.2 (mode)... 5.1 to pass "nil" to operate normally
        function lua_objlen(L: PLuaState; idx: Integer): Cardinal; override;                                  // External in 5.1, "lua_rawlen" in 5.2
        function lua_pcall(L: PLuaState; nargs, nresults, errfunc: Integer): Integer; override;               // External in 5.1, "lua_pcallk" in 5.2
        function lua_pushlstring(L: PLuaState; const s: PAnsiChar; ls: Cardinal): PAnsiChar; overload; override;
        function lua_resume(L: PLuaState; narg: Integer): Integer; override;  // 5.1 version                  // Commonized Externals
        function lua_setfenv(L: PLuaState; idx: Integer): LongBool; override;                                 // External in 5.1, "lua_setuservalue" in 5.2
        function lua_setmetatable(L: PLuaState; objindex: Integer): LongBool; override;                       // Function in 5.1, Procedure in 5.2... using a Function for both (saves hardship)
        function lua_tointeger(L: PLuaState; idx: Integer): Integer; override;                                // In 5.1
        function lua_tonumber(L: PLuaState; idx: Integer): Double; override;                                  // In 5.1
        function lua_yield(L: PLuaState; nresults: Integer): Integer; override;                               // "lua_yield" in 5.1, "lua_yieldk" in 5.2
      {$ENDREGION}
      {$REGION 'ILuaAuxInterchange'}
        function luaL_loadbuffer(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; override;
        function luaL_loadfile(L: PLuaState; filename: PAnsiChar): Integer; override;
        function luaL_prepbuffer(B: TLuaLBuffer): PAnsiChar; override;
        procedure luaL_register(L: PLuaState; libname: PAnsiChar; lib: PluaLReg); override;
      {$ENDREGION}
      {$REGION 'ILua51Aux'}
        procedure luaL_openlib(L: PLuaState; const libname: PAnsiChar; const lr: PLuaLReg; nup: Integer); overload; override;
      {$ENDREGION}
    end;
  {$ENDREGION}

implementation

const
{$IFDEF MSWINDOWS}
  LUA_lua_cpcall = 'lua_cpcall';
  LUA_lua_equal = 'lua_equal';
  LUA_luaL_findtable = 'luaL_findtable';
  LUA_lua_getfenv = 'lua_getfenv';
  LUA_lua_lessthan = 'lua_lessthan';
  LUA_lua_objlen = 'lua_objlen';
  LUA_lua_setfenv = 'lua_setfenv';
{$ELSE}
  LUA_lua_cpcall = '_lua_cpcall';
  LUA_lua_equal = '_lua_equal';
  LUA_luaL_findtable = '_luaL_findtable';
  LUA_lua_getfenv = '_lua_getfenv';
  LUA_lua_lessthan = '_lua_lessthan';
  LUA_lua_objlen = '_lua_objlen';
  LUA_lua_setfenv = '_lua_setfenv';
{$ENDIF}

{$REGION 'Lua Lib Common External Methods'}
  function EXT_lua_atpanic(L: PLuaState; panicf: TLuaDelphiFunction): TLuaDelphiFunction; cdecl; external LUA_DLL name LUA_lua_atpanic;
  function EXT_lua_checkstack(L: PLuaState; sz: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_checkstack;
  procedure EXT_lua_close(L: PLuaState); cdecl; external LUA_DLL name LUA_lua_close;
  procedure EXT_lua_concat(L: PLuaState; n: Integer); cdecl; external LUA_DLL name LUA_lua_concat;
  procedure EXT_lua_createtable(L: PLuaState; narr, nrec: Integer); cdecl; external LUA_DLL name LUA_lua_createtable;
  function EXT_lua_dump(L: PLuaState; writer: TLuaWriterFunction; data: Pointer): Integer; cdecl; external LUA_DLL name LUA_lua_dump;
  function EXT_lua_error(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_lua_error;
  function EXT_lua_gc(L: PLuaState; what, data: Integer): Integer; cdecl; external LUA_DLL name LUA_lua_gc;
  function EXT_lua_getallocf(L: PLuaState; ud: PPointer): TLuaAllocFunction; cdecl; external LUA_DLL name LUA_lua_getallocf;
  procedure EXT_lua_getfield(L: PLuaState; idx: Integer; k: PAnsiChar); cdecl; external LUA_DLL name LUA_lua_getfield;
  function EXT_lua_gethook(L: PLuaState): TLuaHookFunction; cdecl; external LUA_DLL name LUA_lua_gethook;
  function EXT_lua_gethookcount(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_lua_gethookcount;
  function EXT_lua_gethookmask(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_lua_gethookmask;
  function EXT_lua_getinfo(L: PLuaState; const what: PAnsiChar; ar: PLuaDebug): Integer; cdecl; external LUA_DLL name LUA_lua_getinfo;
  function EXT_lua_getlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar; cdecl; external LUA_DLL name LUA_lua_getlocal;
  function EXT_lua_getmetatable(L: PLuaState; objindex: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_getmetatable;
  function EXT_lua_getstack(L: PLuaState; level: Integer; ar: PLuaDebug): Integer; cdecl; external LUA_DLL name LUA_lua_getstack;
  procedure EXT_lua_gettable(L: PLuaState ; idx: Integer); cdecl; external LUA_DLL name LUA_lua_gettable;
  function EXT_lua_gettop(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_lua_gettop;
  function EXT_lua_getupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar; cdecl; external LUA_DLL name LUA_lua_getupvalue;
  procedure EXT_lua_insert(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_insert;
  function EXT_lua_iscfunction(L: PLuaState; idx: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_iscfunction;
  function EXT_lua_isnumber(L: PLuaState; idx: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_isnumber;
  function EXT_lua_isstring(L: PLuaState; idx: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_isstring;
  function EXT_lua_isuserdata(L: PLuaState; idx: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_isuserdata;
  function EXT_lua_newthread(L: PLuaState): PLuaState; cdecl; external LUA_DLL name LUA_lua_newthread;
  function EXT_lua_newstate(f: TLuaAllocFunction; ud: Pointer): PLuaState; cdecl; external LUA_DLL name LUA_lua_newstate;
  function EXT_lua_newuserdata(L: PLuaState; sz: Cardinal): Pointer; cdecl; external LUA_DLL name LUA_lua_newuserdata;
  function EXT_lua_next(L: PLuaState; idx: Integer): Integer; cdecl; external LUA_DLL name LUA_lua_next;
  procedure EXT_lua_pushboolean(L: PLuaState; b: LongBool); cdecl; external LUA_DLL name LUA_lua_pushboolean;
  procedure EXT_lua_pushcclosure(L: PLuaState; fn: TLuaDelphiFunction; n: Integer); cdecl; external LUA_DLL name LUA_lua_pushcclosure;
  function EXT_lua_pushfstring(L: PLuaState; const fmt: PAnsiChar): PAnsiChar; {varargs;} cdecl; external LUA_DLL name LUA_lua_pushfstring;
  procedure EXT_lua_pushinteger(L: PLuaState; n: Integer); cdecl; external LUA_DLL name LUA_lua_pushinteger;
  procedure EXT_lua_pushlightuserdata(L: PLuaState; p: Pointer); cdecl; external LUA_DLL name LUA_lua_pushlightuserdata;
  procedure EXT_lua_pushlstring(L: PLuaState; s: PAnsiChar; len: Cardinal); cdecl; external Lua_DLL name LUA_lua_pushlstring;
  procedure EXT_lua_pushnil(L: PLuaState); cdecl; external LUA_DLL name LUA_lua_pushnil;
  procedure EXT_lua_pushnumber(L: PLuaState; n: Double); cdecl; external LUA_DLL name LUA_lua_pushnumber;
  procedure EXT_lua_pushstring(L: PLuaState; const s: PAnsiChar); cdecl; external LUA_DLL name LUA_lua_pushstring;
  function EXT_lua_pushthread(L: PLuaState): LongBool; cdecl; external LUA_DLL name LUA_lua_pushthread;
  procedure EXT_lua_pushvalue(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_pushvalue;
  function EXT_lua_pushvfstring(L: PLuaState; const fmt: PAnsiChar; argp: Pointer): PAnsiChar; cdecl; external LUA_DLL name LUA_lua_pushvfstring;
  function EXT_lua_rawequal(L: PLuaState; idx1, idx2: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_rawequal;
  procedure EXT_lua_rawget(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_rawget;
  procedure EXT_lua_rawgeti(L: PLuaState; idx, n: Integer); cdecl; external LUA_DLL name LUA_lua_rawgeti;
  procedure EXT_lua_rawset(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_rawset;
  procedure EXT_lua_rawseti(L: PLuaState; idx , n: Integer); cdecl; external LUA_DLL name LUA_lua_rawseti;
  procedure EXT_lua_remove(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_remove;
  procedure EXT_lua_replace(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_replace;
  procedure EXT_lua_setallocf(L: PLuaState; f: TLuaAllocFunction; ud: Pointer); cdecl; external LUA_DLL name LUA_lua_setallocf;
  procedure EXT_lua_setfield(L: PLuaState; idx: Integer; const k: PAnsiChar); cdecl; external LUA_DLL name LUA_lua_setfield;
  function EXT_lua_sethook(L: PLuaState; func: TLuaHookFunction; mask, count: Integer): Integer; cdecl; external LUA_DLL name LUA_lua_sethook;
  function EXT_lua_setlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar; cdecl; external LUA_DLL name LUA_lua_setlocal;
  procedure EXT_lua_settable(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_settable;
  procedure EXT_lua_settop(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_settop;
  function EXT_lua_setupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar; cdecl; external LUA_DLL name LUA_lua_setupvalue;
  function EXT_lua_status(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_lua_status;
  function EXT_lua_toboolean(L: PLuaState; idx: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_toboolean;
  function EXT_lua_tocfunction(L: PLuaState; idx: Integer): TLuaDelphiFunction; cdecl; external LUA_DLL name LUA_lua_tocfunction;
  function EXT_lua_tolstring(L: PLuaState; idx: Integer; len: PCardinal): PAnsiChar; cdecl; external LUA_DLL name LUA_lua_tolstring;
  function EXT_lua_topointer(L: PLuaState; idx: Integer): Pointer; cdecl; external LUA_DLL name LUA_lua_topointer;
  function EXT_lua_tothread(L: PLuaState; idx: Integer): PLuaState; cdecl; external LUA_DLL name LUA_lua_tothread;
  function EXT_lua_touserdata(L: PLuaState; idx: Integer): Pointer; cdecl; external LUA_DLL name LUA_lua_touserdata;
  function EXT_lua_type(L: PLuaState; idx: Integer): Integer; cdecl; external LUA_DLL name LUA_lua_type;
  function EXT_lua_typename(L: PLuaState; tp: Integer): PAnsiChar; cdecl; external LUA_DLL name LUA_lua_typename;
  procedure EXT_lua_xmove(src, dest: PLuaState; n: Integer); cdecl; external LUA_DLL name LUA_lua_xmove;
  function EXT_luaopen_base(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_base;
  function EXT_luaopen_debug(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_debug;
  function EXT_luaopen_io(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_io;
  function EXT_luaopen_math(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_math;
  function EXT_luaopen_os(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_os;
  function EXT_luaopen_package(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_package;
  function EXT_luaopen_string(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_string;
  function EXT_luaopen_table(L: PLuaState): Integer; cdecl; external LUA_DLL name LUA_luaopen_table;
{$ENDREGION}

{$REGION 'Lua Auxiliary Common External Methods'}
  procedure EXT_luaL_addlstring(B: PLuaLBuffer; const s: PAnsiChar; ls: Cardinal); cdecl; external LUA_DLL name LUA_luaL_addlstring;
  procedure EXT_luaL_addstring(B: PLuaLBuffer; const s: PAnsiChar); cdecl; external LUA_DLL name LUA_luaL_addstring;
  procedure EXT_luaL_addvalue(B: PLuaLBuffer); cdecl; external LUA_DLL name LUA_luaL_addvalue;
  function EXT_luaL_argerror(L: PLuaState; numarg: Integer; const extramsg: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_argerror;
  procedure EXT_luaL_buffinit(L: PLuaState; B: PLuaLBuffer); cdecl; external LUA_DLL name LUA_luaL_buffinit;
  function EXT_luaL_callmeta(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_callmeta;
  procedure EXT_luaL_checkany(L: PLuaState; narg: Integer); cdecl; external LUA_DLL name LUA_luaL_checkany;
  function EXT_luaL_checkinteger(L: PLuaState; numArg: Integer): Integer; cdecl; external LUA_DLL name LUA_luaL_checkinteger;
  function EXT_luaL_checklstring(L: PLuaState; numArg: Integer; ls: PCardinal): PAnsiChar; cdecl; external LUA_DLL name LUA_luaL_checklstring;
  function EXT_luaL_checknumber(L: PLuaState; numArg: Integer): Double; cdecl; external LUA_DLL name LUA_luaL_checknumber;
  function EXT_luaL_checkoption(L: PLuaState; narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_checkoption;
  procedure EXT_luaL_checkstack(L: PLuaState; sz: Integer; const msg: PAnsiChar); cdecl; external LUA_DLL name LUA_luaL_checkstack;
  procedure EXT_luaL_checktype(L: PLuaState; narg, t: Integer); cdecl; external LUA_DLL name LUA_luaL_checktype;
  function EXT_luaL_checkudata(L: PLuaState; ud: Integer; const tname: PAnsiChar): Pointer; cdecl; external LUA_DLL name LUA_luaL_checkudata;
  function EXT_luaL_error(L: PLuaState; const fmt: PAnsiChar): Integer; varargs; cdecl; external LUA_DLL name LUA_luaL_error;
  function EXT_luaL_findtable(L: PLuaState; idx: Integer; fname: PAnsiChar; szhint: Integer): PAnsiChar; cdecl; external LUA_DLL name LUA_luaL_findtable;
  function EXT_luaL_getmetafield(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_getmetafield;
  function EXT_luaL_gsub(L: PLuaState; const s, p, r: PAnsiChar): PAnsiChar; cdecl; external LUA_DLL name LUA_luaL_gsub;
  function EXT_luaL_loadstring(L: PLuaState; const s: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_loadstring;
  function EXT_luaL_newmetatable(L: PLuaState; const tname: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_newmetatable;
  function EXT_luaL_newstate: PLuaState; cdecl; external LUA_DLL name LUA_luaL_newstate;
  procedure EXT_luaL_openlib(L: PLuaState; libname: PAnsiChar; lr: PLuaLReg; nup: Integer); cdecl; external LUA_DLL name LUA_luaL_openlib;
  procedure EXT_luaL_openlibs(L: PLuaState); cdecl; external LUA_DLL name LUA_luaL_openlibs;
  function EXT_luaL_optinteger(L: PLuaState; nArg: Integer; def: Integer): Integer; cdecl; external LUA_DLL name LUA_luaL_optinteger;
  function EXT_luaL_optlstring(L: PLuaState; numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar; cdecl; external LUA_DLL name LUA_luaL_optlstring;
  function EXT_luaL_optnumber(L: PLuaState; nArg: Integer; def: Double): Double; cdecl; external LUA_DLL name LUA_luaL_optnumber;
  procedure EXT_luaL_pushresult(B: PLuaLBuffer); cdecl; external LUA_DLL name LUA_luaL_pushresult;
  function EXT_luaL_ref(L: PLuaState; t: Integer): Integer; cdecl; external LUA_DLL name LUA_luaL_ref;
  procedure EXT_luaL_unref(L: PLuaState; t, ref: Integer); cdecl; external LUA_DLL name LUA_luaL_unref;
  procedure EXT_luaL_where(L: PLuaState; lvl: Integer); cdecl; external LUA_DLL name LUA_luaL_where;
{$ENDREGION}

{$REGION 'Lua 5.1 Specific External Methods'}
  procedure EXT_lua_call(L: PLuaState; nargs, nresults: Integer); cdecl; external LUA_DLL name LUA_lua_call;
  function EXT_lua_cpcall(L: PLuaState; func: TLuaDelphiFunction; ud: Pointer): Integer; cdecl; external LUA_DLL name LUA_lua_cpcall;
  function EXT_lua_equal(L: PLuaState; idx1, idx2: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_equal;
  procedure EXT_lua_getfenv(L: PLuaState; idx: Integer); cdecl; external LUA_DLL name LUA_lua_getfenv;
  function EXT_lua_lessthan(L: PLuaState; idx1, idx2: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_lessthan;
  function EXT_lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_lua_load;
  function ext_lua_objlen(L: PLuaState; idx: Integer): Cardinal; cdecl; external LUA_DLL name LUA_lua_objlen;
  function EXT_lua_pcall(L: PLuaState; nargs, nresults, errfunc: Integer): Integer; cdecl; external LUA_DLL name LUA_lua_pcall;
  function EXT_lua_resume(L: PLuaState; narg: Integer): Integer; cdecl; external LUA_DLL name LUA_lua_resume;
  function EXT_lua_setfenv(L: PLuaState; idx: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_setfenv;
  function EXT_lua_setmetatable(L: PLuaState; objindex: Integer): LongBool; cdecl; external LUA_DLL name LUA_lua_setmetatable;
  function EXT_lua_tointeger(L: PLuaState; idx: Integer): TLuaInteger; cdecl; external LUA_DLL name LUA_lua_tointeger;
  function EXT_lua_tonumber(L: PLuaState; idx: Integer): TLuaNumber; cdecl; external LUA_DLL name LUA_lua_tonumber;
  function EXT_lua_yield(L: PLuaState; nresults: Integer): Integer; cdecl; external LUA_DLL name LUA_lua_yield;
{$ENDREGION}

{$REGION 'Lua 5.1 Auxiliary Specific External Methods'}
  function EXT_luaL_loadbuffer(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_loadbuffer;
  function EXT_luaL_loadfile(L: PLuaState; filename: PAnsiChar): Integer; cdecl; external LUA_DLL name LUA_luaL_loadfile;
  function EXT_luaL_prepbuffer(B: TLuaLBuffer): PAnsiChar; cdecl; external LUA_DLL name LUA_luaL_prepbuffer;
  procedure EXT_luaL_register(L: PLuaState; libname: PAnsiChar; lib: PluaLReg); cdecl; external LUA_DLL name LUA_luaL_register;
{$ENDREGION}

{$REGION 'TLua51Static - Lua 5.1 Static Link'}
  { TLua51Static }

  procedure TLua51Static.luaL_addlstring(B: PLuaLBuffer; const s: PAnsiChar; ls: Cardinal);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_addlstring, nil, '%p, "%s", %n', [B, s, ls]);{$ENDIF}
    EXT_luaL_addlstring(B, s, ls);
  end;

  procedure TLua51Static.luaL_addstring(B: PLuaLBuffer; const s: PAnsiChar);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_addstring, nil, '%p, "%s"', [B, s]);{$ENDIF}
    EXT_luaL_addstring(B, s);
  end;

  procedure TLua51Static.luaL_addvalue(B: PLuaLBuffer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_addvalue, nil, '%p', [B]);{$ENDIF}
    EXT_luaL_addvalue(B);
  end;

  function TLua51Static.luaL_argerror(L: PLuaState; numarg: Integer; const extramsg: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_argerror, L, ', %d, "%s"', [numarg, extramsg]);{$ENDIF}
    Result := EXT_luaL_argerror(L, numarg, extramsg);
  end;

  procedure TLua51Static.luaL_buffinit(L: PLuaState; B: PLuaLBuffer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_buffinit, L, ', %p', [B]);{$ENDIF}
    EXT_luaL_buffinit(L, B);
  end;

  function TLua51Static.luaL_callmeta(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_callmeta, L, ', %d, "%s"', [obj, e]);{$ENDIF}
    Result := EXT_luaL_callmeta(L, obj, e);
  end;

  procedure TLua51Static.luaL_checkany(L: PLuaState; narg: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checkany, L, ', %d', [narg]);{$ENDIF}
    EXT_luaL_checkany(L, narg);
  end;

  function TLua51Static.luaL_checkinteger(L: PLuaState; numArg: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checkinteger, L, ', %d', [numArg]);{$ENDIF}
    Result := EXT_luaL_checkinteger(L, numArg);
  end;

  function TLua51Static.luaL_checklstring(L: PLuaState; numArg: Integer; ls: PCardinal): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checklstring, L, ', %d, %d', [numArg, ls]);{$ENDIF}
    Result := EXT_luaL_checklstring(L, numArg, ls);
  end;

  function TLua51Static.luaL_checknumber(L: PLuaState; numArg: Integer): Double;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checknumber, L, ', %d', [numArg]);{$ENDIF}
    Result := EXT_luaL_checknumber(L, numArg);
  end;

  function TLua51Static.luaL_checkoption(L: PLuaState; narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checkoption, L, ', %d, "%s", %d', [narg, def, Length(lst)]);{$ENDIF}
    Result := EXT_luaL_checkoption(L, narg, def, lst);
  end;

  procedure TLua51Static.luaL_checkstack(L: PLuaState; sz: Integer; const msg: PAnsiChar);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checkstack, L, ', %d, "%s"', [sz, msg]);{$ENDIF}
    EXT_luaL_checkstack(L, sz, msg);
  end;

  procedure TLua51Static.luaL_checktype(L: PLuaState; narg, t: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checktype, L, ', %d, %d', [narg, t]);{$ENDIF}
    EXT_luaL_checktype(L, narg, t);
  end;

  function TLua51Static.luaL_checkudata(L: PLuaState; ud: Integer; const tname: PAnsiChar): Pointer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_checkudata, L, ', %d, "%s"', [ud, tname]);{$ENDIF}
    Result := EXT_luaL_checkudata(L, ud, tname);
  end;

  function TLua51Static.luaL_error(L: PLuaState; const fmt: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_error, L, ', "%s"', [fmt]);{$ENDIF}
    Result := EXT_luaL_error(L, fmt);
  end;

  function TLua51Static.luaL_getmetafield(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_getmetafield, L, ', %d, "%s"', [obj, e]);{$ENDIF}
    Result := EXT_luaL_getmetafield(L, obj, e);
  end;

  function TLua51Static.luaL_gsub(L: PLuaState; const s, p, r: PAnsiChar): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_gsub, L, ', "%s", "%s", "%s"', [s, p, r]);{$ENDIF}
    Result := EXT_luaL_gsub(L, s, p, r);
  end;

  function TLua51Static.luaL_loadbuffer(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_loadbuffer, L, ', "%s", %d, "%s"', [buff, sz, name]);{$ENDIF}
    Result := EXT_luaL_loadbuffer(L, buff, sz, name);
  end;

  function TLua51Static.luaL_loadfile(L: PLuaState; filename: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_loadfile, L, ', "%s"', [filename]);{$ENDIF}
    Result := EXT_luaL_loadfile(L, filename);
  end;

  function TLua51Static.luaL_loadstring(L: PLuaState; const s: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_loadstring, L, ', "%s"', [s]);{$ENDIF}
    Result := EXT_luaL_loadstring(L, s);
  end;

  function TLua51Static.luaL_newmetatable(L: PLuaState; const tname: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_newmetatable, L, ', "%s"', [tname]);{$ENDIF}
    Result := EXT_luaL_newmetatable(L, tname);
  end;

  function TLua51Static.luaL_newstate: PLuaState;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_newstate, nil, '', []);{$ENDIF}
    Result := EXT_luaL_newstate;
  end;

  procedure TLua51Static.luaL_openlib(L: PLuaState; const libname: PAnsiChar; const lr: PLuaLReg; nup: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_openlib, L, ', "%s", %p, %d', [libname, lr, nup]);{$ENDIF}
    EXT_luaL_openlib(L, libname, lr, nup);
  end;

  procedure TLua51Static.luaL_openlibs(L: PLuaState);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_openlibs, L, '', []);{$ENDIF}
    EXT_luaL_openlibs(L);
  end;

  function TLua51Static.luaL_optinteger(L: PLuaState; nArg, def: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_optinteger, L, ', %d, %d', [nArg, def]);{$ENDIF}
    Result := EXT_luaL_optinteger(L, nArg, def);
  end;

  function TLua51Static.luaL_optlstring(L: PLuaState; numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_optlstring, L, ', %d, "%s", %p', [numArg, def, ls]);{$ENDIF}
    Result := EXT_luaL_optlstring(L, numArg, def, ls);
  end;

  function TLua51Static.luaL_optnumber(L: PLuaState; nArg: Integer; def: Double): Double;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_optnumber, L, ', %d, %n', [nArg, def]);{$ENDIF}
    Result := EXT_luaL_optnumber(L, nArg, def);
  end;

  function TLua51Static.luaL_prepbuffer(B: TLuaLBuffer): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_prepbuffer, nil, '%p', [@B]);{$ENDIF}
    Result := EXT_luaL_prepbuffer(B);
  end;

  procedure TLua51Static.luaL_pushresult(B: PLuaLBuffer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_pushresult, nil, '%p', [B]);{$ENDIF}
    EXT_luaL_pushresult(B);
  end;

  function TLua51Static.luaL_ref(L: PLuaState; t: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_ref, L, ', %d', [t]);{$ENDIF}
    Result := EXT_luaL_ref(L, t);
  end;

  procedure TLua51Static.luaL_register(L: PLuaState; libname: PAnsiChar; lib: PluaLReg);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_register, L, ', "%s", %p', [libname, lib]);{$ENDIF}
    EXT_luaL_register(L, libname, lib);
  end;

  procedure TLua51Static.luaL_unref(L: PLuaState; t, ref: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_unref, L, ', %d, %d', [t, ref]);{$ENDIF}
    EXT_luaL_unref(L, t, ref);
  end;

  procedure TLua51Static.luaL_where(L: PLuaState; lvl: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_luaL_where, L, ', %d', [lvl]);{$ENDIF}
    EXT_luaL_where(L, lvl);
  end;

  function TLua51Static.luaopen_base(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_COLIBNAME, EXT_luaopen_base);
  end;

  function TLua51Static.luaopen_debug(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_DBLIBNAME, EXT_luaopen_debug);
  end;

  function TLua51Static.luaopen_io(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_IOLIBNAME, EXT_luaopen_io);
  end;

  function TLua51Static.luaopen_math(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_MATHLIBNAME, EXT_luaopen_math);
  end;

  function TLua51Static.luaopen_os(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_OSLIBNAME, EXT_luaopen_os);
  end;

  function TLua51Static.luaopen_package(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_LOADLIBNAME, EXT_luaopen_package);
  end;

  function TLua51Static.luaopen_string(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_STRLIBNAME, EXT_luaopen_string);
  end;

  function TLua51Static.luaopen_table(L: PLuaState): Integer;
  begin
    Result := LoadLuaLibrary(L, LUA_TABLIBNAME, EXT_luaopen_table);
  end;

  function TLua51Static.lua_atpanic(L: PLuaState; panicf: TLuaDelphiFunction): TLuaDelphiFunction;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_atpanic, L, ', %s', ['<TLuaDelphiFunction value>']);{$ENDIF}
    Result := EXT_lua_atpanic(L, panicf);
  end;

  procedure TLua51Static.lua_call(L: PLuaState; nargs, nresults: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_call, L, ', %d, %d', [nargs, nresults]);{$ENDIF}
    EXT_lua_call(L, nargs, nresults);
  end;

  function TLua51Static.lua_checkstack(L: PLuaState; sz: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_checkstack, L, ', %d', [sz]);{$ENDIF}
    Result := EXT_lua_checkstack(L, sz);
  end;

  procedure TLua51Static.lua_close(L: PLuaState);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_close, L, '', []);{$ENDIF}
    EXT_lua_close(L);
  end;

  procedure TLua51Static.lua_concat(L: PLuaState; n: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_concat, L, ', %d', [n]);{$ENDIF}
    EXT_lua_concat(L, n);
  end;

  function TLua51Static.lua_cpcall(L: PLuaState; func: TLuaDelphiFunction; ud: Pointer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_cpcall, L, ', %s, %p', ['<TLuaDelphiFunction value>', ud]);{$ENDIF}
    Result := EXT_lua_cpcall(L, func, ud);
  end;

  procedure TLua51Static.lua_createtable(L: PLuaState; narr, nrec: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_createtable, L, ', %d, %d', [narr, nrec]);{$ENDIF}
    EXT_lua_createtable(L, narr, nrec);
  end;

  function TLua51Static.lua_dump(L: PLuaState; writer: TLuaWriterFunction; data: Pointer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_dump, L, ', %s, %d', ['<TLuaWriterFunction value>', data]);{$ENDIF}
    Result := EXT_lua_dump(L, writer, data);
  end;

  function TLua51Static.lua_equal(L: PLuaState; idx1, idx2: Integer): LongBool;
  begin
  {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_equal, L, ', %d, %d', [idx1, idx2]);{$ENDIF}
    Result := EXT_lua_equal(L, idx1, idx2);
  end;

  function TLua51Static.lua_error(L: PLuaState): Integer;
  begin
  {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_error, L, '', []);{$ENDIF}
    Result := EXT_lua_error(L);
  end;

  function TLua51Static.lua_gc(L: PLuaState; what, data: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_gc, L, ', %d, %d', [what, data]);{$ENDIF}
    Result := EXT_lua_gc(L, what, data);
  end;

  function TLua51Static.lua_getallocf(L: PLuaState; ud: PPointer): TLuaAllocFunction;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getallocf, L, ', %p', [ud]);{$ENDIF}
    Result := EXT_lua_getallocf(L, ud);
  end;

  procedure TLua51Static.lua_getfenv(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getfenv, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_getfenv(L, idx);
  end;

  procedure TLua51Static.lua_getfield(L: PLuaState; idx: Integer; k: PAnsiChar);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getfield, L, ', %d, "%s"', [idx, k]);{$ENDIF}
    EXT_lua_getfield(L, idx, k);
  end;

  function TLua51Static.lua_gethook(L: PLuaState): TLuaHookFunction;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_gethook, L, '', []);{$ENDIF}
    Result := EXT_lua_gethook(L);
  end;

  function TLua51Static.lua_gethookcount(L: PLuaState): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_gethookcount, L, '', []);{$ENDIF}
    Result := EXT_lua_gethookcount(L);
  end;

  function TLua51Static.lua_gethookmask(L: PLuaState): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_gethookmask, L, '', []);{$ENDIF}
    Result := EXT_lua_gethookmask(L);
  end;

  function TLua51Static.lua_getinfo(L: PLuaState; const what: PAnsiChar; ar: PLuaDebug): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getinfo, L, ', "%s", %p', [what, ar]);{$ENDIF}
    Result := EXT_lua_getinfo(L, what, ar);
  end;

  function TLua51Static.lua_getlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getlocal, L, ', %p, %d', [ar, n]);{$ENDIF}
    Result := EXT_lua_getlocal(L, ar, n);
  end;

  function TLua51Static.lua_getmetatable(L: PLuaState; objindex: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getmetatable, L, ', %d', [objindex]);{$ENDIF}
    Result := EXT_lua_getmetatable(L, objindex);
  end;

  function TLua51Static.lua_getstack(L: PLuaState; level: Integer; ar: PLuaDebug): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getstack, L, ', %d, %p', [level, ar]);{$ENDIF}
    Result := EXT_lua_getstack(L, level, ar);
  end;

  procedure TLua51Static.lua_gettable(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_gettable, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_gettable(L, idx);
  end;

  function TLua51Static.lua_gettop(L: PLuaState): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_gettop, L, '', []);{$ENDIF}
    Result := EXT_lua_gettop(L);
  end;

  function TLua51Static.lua_getupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_getupvalue, L, ', %d, %d', [funcindex, n]);{$ENDIF}
    Result := EXT_lua_getupvalue(L, funcindex, n);
  end;

  procedure TLua51Static.lua_insert(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_insert, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_insert(L, idx);
  end;

  function TLua51Static.lua_iscfunction(L: PLuaState; idx: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_iscfunction, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_iscfunction(L, idx);
  end;

  function TLua51Static.lua_isnumber(L: PLuaState; idx: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_isnumber, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_isnumber(L, idx);
  end;

  function TLua51Static.lua_isstring(L: PLuaState; idx: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_isstring, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_isstring(L, idx);
  end;

  function TLua51Static.lua_isuserdata(L: PLuaState; idx: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_isuserdata, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_isuserdata(L, idx);
  end;

  function TLua51Static.lua_lessthan(L: PLuaState; idx1, idx2: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_lessthan, L, ', %d, %d', [idx1, idx2]);{$ENDIF}
    Result := EXT_lua_lessthan(L, idx1, idx2);
  end;

  function TLua51Static.lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_load, L, ', %s, %p, "%s"', ['<TLuaReaderFunction value>', dt, chunkname]);{$ENDIF}
    Result := EXT_lua_load(L, reader, dt, chunkname);
  end;

  function TLua51Static.lua_newstate(f: TLuaAllocFunction; ud: Pointer): PLuaState;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_newstate, nil, '%s, %p', ['<TLuaAllocFunction value>', ud]);{$ENDIF}
    Result := EXT_lua_newstate(f, ud);
  end;

  function TLua51Static.lua_newthread(L: PLuaState): PLuaState;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_newthread, L, '', []);{$ENDIF}
    Result := EXT_lua_newthread(L);
  end;

  function TLua51Static.lua_newuserdata(L: PLuaState; sz: Cardinal): Pointer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_newuserdata, L, ', %d', [sz]);{$ENDIF}
    Result := EXT_lua_newuserdata(L, sz);
  end;

  function TLua51Static.lua_next(L: PLuaState; idx: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_next, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_next(L, idx);
  end;

  function TLua51Static.lua_objlen(L: PLuaState; idx: Integer): Cardinal;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_objlen, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_objlen(L, idx);
  end;

  function TLua51Static.lua_pcall(L: PLuaState; nargs, nresults, errfunc: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pcall, L, ', %d, %d, %d', [nargs, nresults, errfunc]);{$ENDIF}
    Result := EXT_lua_pcall(L, nargs, nresults, errfunc);
  end;

  procedure TLua51Static.lua_pushboolean(L: PLuaState; b: LongBool);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushboolean, L, ', "%s"', [L4DBoolStrings[b]]);{$ENDIF}
    EXT_lua_pushboolean(L, b);
  end;

  procedure TLua51Static.lua_pushcclosure(L: PLuaState; fn: TLuaDelphiFunction; n: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushcclosure, L, ', %s', ['<TLuaDelphiFunction value>']);{$ENDIF}
    EXT_lua_pushcclosure(L, fn, n);
  end;

  function TLua51Static.lua_pushfstring(L: PLuaState; const fmt: PAnsiChar): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushfstring, L, ', "%s"', [fmt]);{$ENDIF}
    Result := EXT_lua_pushfstring(L, fmt);
  end;

  procedure TLua51Static.lua_pushinteger(L: PLuaState; n: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushinteger, L, ', %d', [n]);{$ENDIF}
    EXT_lua_pushinteger(L, n);
  end;

  procedure TLua51Static.lua_pushlightuserdata(L: PLuaState; p: Pointer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushlightuserdata, L, ', %p', [p]);{$ENDIF}
    EXT_lua_pushlightuserdata(L, p);
  end;

  function TLua51Static.lua_pushlstring(L: PLuaState; const s: PAnsiChar; ls: Cardinal): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushlstring, L, ', "%s", %d', [s, ls]);{$ENDIF}
    EXT_lua_pushlstring(L, s, ls);
    Result := s;
  end;

  procedure TLua51Static.lua_pushnil(L: PLuaState);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushnil, L, '', []);{$ENDIF}
    EXT_lua_pushnil(L);
  end;

  procedure TLua51Static.lua_pushnumber(L: PLuaState; n: Double);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushnumber, L, ', %g', [n]);{$ENDIF}
    EXT_lua_pushnumber(L, n);
  end;

  function TLua51Static.lua_pushstring(L: PLuaState; const s: PAnsiChar): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushstring, L, ', "%s"', [s]);{$ENDIF}
    EXT_lua_pushstring(L, s);
    Result := s;
  end;

  function TLua51Static.lua_pushthread(L: PLuaState): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushthread, L, '', []);{$ENDIF}
    Result := EXT_lua_pushthread(L);
  end;

  procedure TLua51Static.lua_pushvalue(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushvalue, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_pushvalue(L, idx);
  end;

  function TLua51Static.lua_pushvfstring(L: PLuaState; const fmt: PAnsiChar; argp: Pointer): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_pushvfstring, L, ', "%s", %p', [fmt, argp]);{$ENDIF}
    Result := EXT_lua_pushvfstring(L, fmt, argp);
  end;

  function TLua51Static.lua_rawequal(L: PLuaState; idx1, idx2: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_rawequal, L, ', %d, %d', [idx1, idx2]);{$ENDIF}
    Result := EXT_lua_rawequal(L, idx1, idx2);
  end;

  procedure TLua51Static.lua_rawget(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_rawget, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_rawget(L, idx);
  end;

  procedure TLua51Static.lua_rawgeti(L: PLuaState; idx, n: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_rawgeti, L, ', %d, %d', [idx, n]);{$ENDIF}
    EXT_lua_rawgeti(L, idx, n);
  end;

  procedure TLua51Static.lua_rawset(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_rawset, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_rawset(L, idx);
  end;

  procedure TLua51Static.lua_rawseti(L: PLuaState; idx, n: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_rawseti, L, ', %d, %d', [idx, n]);{$ENDIF}
    EXT_lua_rawseti(L, idx, n);
  end;

  procedure TLua51Static.lua_remove(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_remove, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_remove(L, idx);
  end;

  procedure TLua51Static.lua_replace(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_replace, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_replace(L, idx);
  end;

  function TLua51Static.lua_resume(L: PLuaState; narg: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_resume, L, ', %d', [narg]);{$ENDIF}
    Result := EXT_lua_resume(L, narg);
  end;

  procedure TLua51Static.lua_setallocf(L: PLuaState; f: TLuaAllocFunction; ud: Pointer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_setallocf, L, ', %s, %p', ['<TLuaAllocFunction value>', ud]);{$ENDIF}
    EXT_lua_setallocf(L, f, ud);
  end;

  function TLua51Static.lua_setfenv(L: PLuaState; idx: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_setfenv, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_setfenv(L, idx);
  end;

  procedure TLua51Static.lua_setfield(L: PLuaState; idx: Integer; const k: PAnsiChar);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_setfield, L, ', %d, "%s"', [idx, k]);{$ENDIF}
    EXT_lua_setfield(L, idx, k);
  end;

  function TLua51Static.lua_sethook(L: PLuaState; func: TLuaHookFunction; mask, count: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_sethook, L, ', %s, %d, %d', ['<TLuaHookFunction value>', mask, count]);{$ENDIF}
    Result := EXT_lua_sethook(L, func, mask, count);
  end;

  function TLua51Static.lua_setlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_setlocal, L, ', %p, %d', [ar, n]);{$ENDIF}
    Result := EXT_lua_setlocal(L, ar, n);
  end;

  function TLua51Static.lua_setmetatable(L: PLuaState; objindex: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_setmetatable, L, ', %d', [objindex]);{$ENDIF}
    Result := EXT_lua_setmetatable(L, objindex);
  end;

  procedure TLua51Static.lua_settable(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_settable, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_settable(L, idx);
  end;

  procedure TLua51Static.lua_settop(L: PLuaState; idx: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_settop, L, ', %d', [idx]);{$ENDIF}
    EXT_lua_settop(L, idx);
  end;

  function TLua51Static.lua_setupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_setupvalue, L, ', %d, %d', [funcindex, n]);{$ENDIF}
    Result := EXT_lua_setupvalue(L, funcindex, n);
  end;

  function TLua51Static.lua_status(L: PLuaState): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_status, L, '', []);{$ENDIF}
    Result := EXT_lua_status(L);
  end;

  function TLua51Static.lua_toboolean(L: PLuaState; idx: Integer): LongBool;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_toboolean, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_toboolean(L, idx);
  end;

  function TLua51Static.lua_tocfunction(L: PLuaState; idx: Integer): TLuaDelphiFunction;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_tocfunction, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_tocfunction(L, idx);
  end;

  function TLua51Static.lua_tointeger(L: PLuaState; idx: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_tointeger, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_tointeger(L, idx);
  end;

  function TLua51Static.lua_tolstring(L: PLuaState; idx: Integer; len: PCardinal): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_tolstring, L, ', %d, %p', [idx, len]);{$ENDIF}
    Result := EXT_lua_tolstring(L, idx, len);
  end;

  function TLua51Static.lua_tonumber(L: PLuaState; idx: Integer): Double;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_tonumber, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_tonumber(L, idx);
  end;

  function TLua51Static.lua_topointer(L: PLuaState; idx: Integer): Pointer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_topointer, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_topointer(L, idx);
  end;

  function TLua51Static.lua_tothread(L: PLuaState; idx: Integer): PLuaState;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_tothread, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_tothread(L, idx);
  end;

  function TLua51Static.lua_touserdata(L: PLuaState; idx: Integer): Pointer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_touserdata, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_touserdata(L, idx);
  end;

  function TLua51Static.lua_type(L: PLuaState; idx: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_type, L, ', %d', [idx]);{$ENDIF}
    Result := EXT_lua_type(L, idx);
  end;

  function TLua51Static.lua_typename(L: PLuaState; tp: Integer): PAnsiChar;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_typename, L, ', %d', [tp]);{$ENDIF}
    Result := EXT_lua_typename(L, tp);
  end;

  procedure TLua51Static.lua_xmove(src, dest: PLuaState; n: Integer);
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_xmove, src, ', $%p, %d', [dest, n]);{$ENDIF}
    EXT_lua_xmove(src, dest, n);
  end;

  function TLua51Static.lua_yield(L: PLuaState; nresults: Integer): Integer;
  begin
    {$IFDEF L4D_API_LOGGING}L4DLogger.AddAPICall(LUA_lua_yield, L, ', %d', [nresults]);{$ENDIF}
    Result := EXT_lua_yield(L, nresults);
  end;

  procedure TLua51Static.SetLuaLibRefs;
  begin
    inherited;
    FLibBase := EXT_luaopen_base;
    FLibDebug := EXT_luaopen_debug;
    FLibIO := EXT_luaopen_io;
    FLibMath := EXT_luaopen_math;
    FLibOS := EXT_luaopen_os;
    FLibPackage := EXT_luaopen_package;
    FLibString := EXT_luaopen_string;
    FLibTable := EXT_luaopen_table;
  end;
{$ENDREGION}

end.
