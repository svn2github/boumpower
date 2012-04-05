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
  Unit: L4D.Lua.Common.pas
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
unit L4D.Lua.Common;

interface

{$I Lua4Delphi.inc}

uses
  {$IFDEF DELPHIXE2}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$ENDIF}
  L4D.Lua.Intf;

type
  { Forward Declarations }
  TLuaBase = class;
  TLuaCommon = class;
  TLuaBaseMember = class;

  { Class Ref }
  TLuaBaseType = class of TLuaCommon;

  {
    TLuaBase
      - All Common (Abstract) Methods (excluding Localizers)
  }
  {$REGION 'Base Lua Class (no Localizers)'}
    TLuaBase = class(TInterfacedObject, ILuaLibCommonMacros, ILuaLibInterchange, ILuaLibCommon, ILuaAuxCommon, ILuaAuxCommonMacros, ILuaAuxInterchange)
    protected
      FLibBase,
      FLibDebug,
      FLibIO,
      FLibMath,
      FLibOS,
      FLibPackage,
      FLibString,
      FLibTable: TLuaDelphiFunction;
      function LoadLuaLibrary(const L: PLuaState; const ALibName: AnsiString; ALibFunc: TLuaDelphiFunction): Integer; inline;
      procedure SetLuaLibRefs; virtual; abstract;
      {$REGION 'ILuaLibCommonMacros'}
        function lua_isboolean(L: PLuaState; idx: Integer): LongBool; overload; inline;
        function lua_isfunction(L: PLuaState; idx: Integer): LongBool; overload; inline;
        function lua_islightuserdata(L: PLuaState; idx: Integer): LongBool; overload; inline;
        function lua_isnil(L: PLuaState; idx: Integer): LongBool; overload; inline;
        function lua_isnone(L: PLuaState; idx: Integer): LongBool; overload; inline;
        function lua_isnoneornil(L: PLuaState; idx: Integer): LongBool; overload; inline;
        function lua_istable(L: PLuaState; idx: Integer): LongBool; overload; inline;
        function lua_isthread(L: PLuaState; idx: Integer): LongBool; overload; inline;
        procedure lua_newtable(L: PLuaState); overload; inline;
        procedure lua_pop(L: PLuaState; n: Integer); overload; inline;
        procedure lua_pushcfunction(L: PLuaState; f: TLuaDelphiFunction); overload; inline;
        function lua_pushliteral(L: PLuaState; s: PAnsiChar): PAnsiChar; overload; virtual; abstract;
        function lua_pushlstring(L: PLuaState; const s: PAnsiChar; ls: Cardinal): PAnsiChar; overload; virtual; abstract;
        procedure lua_register(L: PLuaState; name: PAnsiChar; f: TLuaDelphiFunction); overload; inline;
        function lua_tostring(L: PLuaState; idx: Integer): PAnsiChar; overload; inline;
      {$ENDREGION}
      {$REGION 'ILuaLibInterchange'}
        procedure lua_call(L: PLuaState; nargs, nresults: Integer); overload; virtual; abstract;                                 // External "lua_call" in 5.1, External "lua_callk" in 5.2
        procedure lua_callk(L: PLuaState; nargs, nresults, ctx: Integer; k: TLuaDelphiFunction); overload; virtual; abstract;    // External "lua_call" in 5.1, External "lua_callk" in 5.2
        function lua_compare(L: PLuaState; idx1, idx2, op: Integer): LongBool; overload; virtual; abstract;                      // "lua_equal" OR "lua_lessthan" in 5.1, External in 5.2
        function lua_cpcall(L: PLuaState; func: TLuaDelphiFunction; ud: Pointer): Integer; overload; virtual; abstract;          // External in 5.1, "lua_pcall" in 5.2
        function lua_equal(L: PLuaState; idx1, idx2: Integer): LongBool; overload; virtual; abstract;                            // External in 5.1, "lua_compare" in 5.2
        procedure lua_getfenv(L: PLuaState; idx: Integer); overload; virtual; abstract;                                          // External in 5.1, "lua_getuservalue" in 5.2
        procedure lua_getglobal(L: PLuaState; name: PAnsiChar); overload; virtual; abstract;                                     // "lua_getfield" in 5.1, External in 5.2
        procedure lua_getuservalue(L: PLuaState; idx: Integer); overload; virtual; abstract;                                     // "lua_getfenv" in 5.1, External in 5.2
        function lua_lessthan(L: PLuaState; idx1, idx2: Integer): LongBool; overload; virtual; abstract;                         // External in 5.1, "lua_compare" in 5.2
        function lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer; overload; virtual; abstract; // Extra parameter in 5.2 (mode)... 5.1 to pass "nil" to operate normally
        function lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; Source, Mode: PAnsiChar): Integer; overload; virtual; abstract;    // Extra parameter in 5.2 (mode)... 5.1 to pass "nil" to operate normally
        function lua_objlen(L: PLuaState; idx: Integer): Cardinal; overload; virtual; abstract;                                  // External in 5.1, "lua_rawlen" in 5.2
        function lua_pcall(L: PLuaState; nargs, nresults, errfunc: Integer): Integer; overload; virtual; abstract;               // External in 5.1, "lua_pcallk" in 5.2
        function lua_pcallk(L: PLuaState; nargs, nresults, arrfunc, ctx: Integer; k: TLuaDelphiFunction): Integer; overload; virtual; abstract;  // External in 5.1, "lua_pcallk" in 5.2
        function lua_rawlen(L: PLuaState; idx: Integer): Cardinal; overload; virtual; abstract;                                  // "lua_objlen" in 5.1, External in 5.2
        function lua_resume(L: PLuaState; narg: Integer): Integer; overload; virtual; abstract;  // 5.1 version                  // Commonized Externals
        function lua_setfenv(L: PLuaState; idx: Integer): LongBool; overload; virtual; abstract;                                 // External in 5.1, "lua_setuservalue" in 5.2
        procedure lua_setglobal(L: PLuaState; name: PAnsiChar); overload; virtual; abstract;                                     // "lua_setfield" in 5.1, External in 5.2
        function lua_setmetatable(L: PLuaState; objindex: Integer): LongBool; overload; virtual; abstract;                       // Function in 5.1, Procedure in 5.2... using a Function for both (saves hardship)
        procedure lua_setuservalue(L: PLuaState; idx: Integer); overload; virtual; abstract;                                     // "lua_getfenv" & "lua_setfenv" in 5.1, External in 5.2
        function lua_tointeger(L: PLuaState; idx: Integer): Integer; overload; virtual; abstract;                                // 5.1
        function lua_tointegerx(L: PLuaState; idx: Integer; isnum: PInteger): Integer; overload; virtual; abstract;              // 5.2
        function lua_tonumber(L: PLuaState; idx: Integer): Double; overload; virtual; abstract;                                  // 5.1
        function lua_tonumberx(L: PLuaState; idx: Integer; isnum: PInteger): Double; overload; virtual; abstract;                // 5.2
        function lua_yield(L: PLuaState; nresults: Integer): Integer; overload; virtual; abstract;                               // "lua_yield" in 5.1, "lua_yieldk" in 5.2
        function lua_yieldk(L: PLuaState; nresults, ctx: Integer; k: TLuaDelphiFunction): Integer; overload; virtual; abstract;  // "lua_yield" in 5.1, "lua_yieldk" in 5.2
      {$ENDREGION}
      {$REGION 'ILuaLibCommon'}
        function lua_atpanic(L: PLuaState; panicf: TLuaDelphiFunction): TLuaDelphiFunction; overload; virtual; abstract;
        function lua_checkstack(L: PLuaState; sz: Integer): LongBool; overload; virtual; abstract;
        procedure lua_close(L: PLuaState); overload; virtual; abstract;
        procedure lua_concat(L: PLuaState; n: Integer); overload; virtual; abstract;
        procedure lua_createtable(L: PLuaState; narr, nrec: Integer); overload; virtual; abstract;
        function lua_dump(L: PLuaState; writer: TLuaWriterFunction; data: Pointer): Integer; overload; virtual; abstract;
        function lua_error(L: PLuaState): Integer; overload; virtual; abstract;
        function lua_gc(L: PLuaState; what, data: Integer): Integer; overload; virtual; abstract;
        function lua_getallocf(L: PLuaState; ud: PPointer): TLuaAllocFunction; overload; virtual; abstract;
        procedure lua_getfield(L: PLuaState; idx: Integer; k: PAnsiChar); overload; virtual; abstract;
        function lua_gethook(L: PLuaState): TLuaHookFunction; overload; virtual; abstract;
        function lua_gethookcount(L: PLuaState): Integer; overload; virtual; abstract;
        function lua_gethookmask(L: PLuaState): Integer; overload; virtual; abstract;
        function lua_getinfo(L: PLuaState; const what: PAnsiChar; ar: PLuaDebug): Integer; overload; virtual; abstract;
        function lua_getlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar; overload; virtual; abstract;
        function lua_getmetatable(L: PLuaState; objindex: Integer): LongBool; overload; virtual; abstract;
        function lua_getstack(L: PLuaState; level: Integer; ar: PLuaDebug): Integer; overload; virtual; abstract;
        procedure lua_gettable(L: PLuaState ; idx: Integer); overload; virtual; abstract;
        function lua_gettop(L: PLuaState): Integer; overload; virtual; abstract;
        function lua_getupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar; overload; virtual; abstract;
        procedure lua_insert(L: PLuaState; idx: Integer); overload; virtual; abstract;
        function lua_iscfunction(L: PLuaState; idx: Integer): LongBool; overload; virtual; abstract;
        function lua_isnumber(L: PLuaState; idx: Integer): LongBool; overload; virtual; abstract;
        function lua_isstring(L: PLuaState; idx: Integer): LongBool; overload; virtual; abstract;
        function lua_isuserdata(L: PLuaState; idx: Integer): LongBool; overload; virtual; abstract;
        function lua_newthread(L: PLuaState): PLuaState; overload; virtual; abstract;
      public
        function lua_newstate(f: TLuaAllocFunction; ud: Pointer): PLuaState; virtual; abstract;
      protected
        function lua_newuserdata(L: PLuaState; sz: Cardinal): Pointer; overload; virtual; abstract;
        function lua_next(L: PLuaState; idx: Integer): Integer; overload; virtual; abstract;
        procedure lua_pushboolean(L: PLuaState; b: LongBool); overload; virtual; abstract;
        procedure lua_pushcclosure(L: PLuaState; fn: TLuaDelphiFunction; n: Integer); overload; virtual; abstract;
        function lua_pushfstring(L: PLuaState; const fmt: PAnsiChar): PAnsiChar; {varargs;} overload; virtual; abstract;
        procedure lua_pushinteger(L: PLuaState; n: Integer); overload; virtual; abstract;
        procedure lua_pushlightuserdata(L: PLuaState; p: Pointer); overload; virtual; abstract;
        procedure lua_pushnil(L: PLuaState); overload; virtual; abstract;
        procedure lua_pushnumber(L: PLuaState; n: Double); overload; virtual; abstract;
        function lua_pushstring(L: PLuaState; const s: PAnsiChar): PAnsiChar; overload; virtual; abstract;
        function lua_pushthread(L: PLuaState): LongBool; overload; virtual; abstract;
        procedure lua_pushvalue(L: PLuaState; idx: Integer); overload; virtual; abstract;
        function lua_pushvfstring(L: PLuaState; const fmt: PAnsiChar; argp: Pointer): PAnsiChar; overload; virtual; abstract;
        function lua_rawequal(L: PLuaState; idx1, idx2: Integer): LongBool; overload; virtual; abstract;
        procedure lua_rawget(L: PLuaState; idx: Integer); overload; virtual; abstract;
        procedure lua_rawgeti(L: PLuaState; idx, n: Integer); overload; virtual; abstract;
        procedure lua_rawset(L: PLuaState; idx: Integer); overload; virtual; abstract;
        procedure lua_rawseti(L: PLuaState; idx , n: Integer); overload; virtual; abstract;
        procedure lua_remove(L: PLuaState; idx: Integer); overload; virtual; abstract;
        procedure lua_replace(L: PLuaState; idx: Integer); overload; virtual; abstract;
        procedure lua_setallocf(L: PLuaState; f: TLuaAllocFunction; ud: Pointer); overload; virtual; abstract;
        procedure lua_setfield(L: PLuaState; idx: Integer; const k: PAnsiChar); overload; virtual; abstract;
        function lua_sethook(L: PLuaState; func: TLuaHookFunction; mask, count: Integer): Integer; overload; virtual; abstract;
        function lua_setlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar; overload; virtual; abstract;
        procedure lua_settable(L: PLuaState; idx: Integer); overload; virtual; abstract;
        procedure lua_settop(L: PLuaState; idx: Integer); overload; virtual; abstract;
        function lua_setupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar; overload; virtual; abstract;
        function lua_status(L: PLuaState): Integer; overload; virtual; abstract;
        function lua_toboolean(L: PLuaState; idx: Integer): LongBool; overload; virtual; abstract;
        function lua_tocfunction(L: PLuaState; idx: Integer): TLuaDelphiFunction; overload; virtual; abstract;
        function lua_tolstring(L: PLuaState; idx: Integer; len: PCardinal): PAnsiChar; overload; virtual; abstract;
        function lua_topointer(L: PLuaState; idx: Integer): Pointer; overload; virtual; abstract;
        function lua_tothread(L: PLuaState; idx: Integer): PLuaState; overload; virtual; abstract;
        function lua_touserdata(L: PLuaState; idx: Integer): Pointer; overload; virtual; abstract;
        function lua_type(L: PLuaState; idx: Integer): Integer; overload; virtual; abstract;
        function lua_typename(L: PLuaState; tp: Integer): PAnsiChar; overload; virtual; abstract;
        procedure lua_xmove(src, dest: PLuaState; n: Integer); overload; virtual; abstract;
        function luaopen_base(L: PLuaState): Integer; overload; virtual; abstract;
        function luaopen_debug(L: PLuaState): Integer; overload; virtual; abstract;
        function luaopen_io(L: PLuaState): Integer; overload; virtual; abstract;
        function luaopen_math(L: PLuaState): Integer; overload; virtual; abstract;
        function luaopen_os(L: PLuaState): Integer; overload; virtual; abstract;
        function luaopen_package(L: PLuaState): Integer; overload; virtual; abstract;
        function luaopen_string(L: PLuaState): Integer; overload; virtual; abstract;
        function luaopen_table(L: PLuaState): Integer; overload; virtual; abstract;
      {$ENDREGION}
      {$REGION 'ILuaAuxCommon'}
        procedure luaL_addlstring(B: PLuaLBuffer; const s: PAnsiChar; ls: Cardinal); overload; virtual; abstract;
        procedure luaL_addstring(B: PLuaLBuffer; const s: PAnsiChar); overload; virtual; abstract;
        procedure luaL_addvalue(B: PLuaLBuffer); overload; virtual; abstract;
        function luaL_argerror(L: PLuaState; numarg: Integer; const extramsg: PAnsiChar): Integer; overload; virtual; abstract;
        procedure luaL_buffinit(L: PLuaState; B: PLuaLBuffer); overload; virtual; abstract;
        function luaL_callmeta(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer; overload; virtual; abstract;
        procedure luaL_checkany(L: PLuaState; narg: Integer); overload; virtual; abstract;
        function luaL_checkinteger(L: PLuaState; numArg: Integer): Integer; overload; virtual; abstract;
        function luaL_checklstring(L: PLuaState; numArg: Integer; ls: PCardinal): PAnsiChar; overload; virtual; abstract;
        function luaL_checknumber(L: PLuaState; numArg: Integer): Double; overload; virtual; abstract;
        function luaL_checkoption(L: PLuaState; narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer; overload; virtual; abstract;
        procedure luaL_checkstack(L: PLuaState; sz: Integer; const msg: PAnsiChar); overload; virtual; abstract;
        procedure luaL_checktype(L: PLuaState; narg, t: Integer); overload; virtual; abstract;
        function luaL_checkudata(L: PLuaState; ud: Integer; const tname: PAnsiChar): Pointer; overload; virtual; abstract;
        function luaL_error(L: PLuaState; const fmt: PAnsiChar): Integer; {varargs;} overload; virtual; abstract;
        function luaL_getmetafield(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer; overload; virtual; abstract;
        function luaL_gsub(L: PLuaState; const s, p, r: PAnsiChar): PAnsiChar; overload; virtual; abstract;
        function luaL_loadstring(L: PLuaState; const s: PAnsiChar): Integer; overload; virtual; abstract;
        function luaL_newmetatable(L: PLuaState; const tname: PAnsiChar): Integer; overload; virtual; abstract;
        function luaL_newstate: PLuaState; overload; virtual; abstract;
        procedure luaL_openlibs(L: PLuaState); overload; virtual; abstract;
        function luaL_optinteger(L: PLuaState; nArg: Integer; def: Integer): Integer; overload; virtual; abstract;
        function luaL_optlstring(L: PLuaState; numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar; overload; virtual; abstract;
        function luaL_optnumber(L: PLuaState; nArg: Integer; def: Double): Double; overload; virtual; abstract;
        procedure luaL_pushresult(B: PLuaLBuffer); overload; virtual; abstract;
        function luaL_ref(L: PLuaState; t: Integer): Integer; overload; virtual; abstract;
        procedure luaL_unref(L: PLuaState; t, ref: Integer); overload; virtual; abstract;
        procedure luaL_where(L: PLuaState; lvl: Integer); overload; virtual; abstract;
      {$ENDREGION}
      {$REGION 'ILuaAuxCommonMacros'}
        function luaL_checkint(L: PLuaState; narg: Integer): Integer; overload; inline;
        function luaL_checklong(L: PLuaState; narg: Cardinal): Cardinal; overload; inline;
        function luaL_checkstring(L: PLuaState; narg: Integer): PAnsiChar; overload; inline;
        function luaL_dofile(L: PLuaState; filename: PAnsiChar): Integer; overload; inline;
        function luaL_dostring(L: PLuaState; str: PAnsiChar): Integer; overload; inline;
        procedure luaL_getmetatable(L: PLuaState; tname: PAnsiChar); overload; inline;
        function luaL_optint(L: PLuaState; narg, d: Integer): Integer; overload; inline;
        function luaL_optlong(L: PLuaState; narg: Integer; d: Cardinal): Cardinal; overload; inline;
        function luaL_optstring(L: PLuaState; narg: Integer; d: PAnsiChar): PAnsiChar; overload; inline;
        function luaL_typename(L: PLuaState; index: Integer): PAnsiChar; overload; inline;
      {$ENDREGION}
      {$REGION 'ILuaAuxInterchange'}
        function luaL_loadbuffer(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; overload; virtual; abstract;
        function luaL_loadbufferx(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer; overload; virtual; abstract;
        function luaL_loadfile(L: PLuaState; filename: PAnsiChar): Integer; overload; virtual; abstract;
        function luaL_loadfilex(L: PLuaState; filename, mode: PAnsiChar): Integer; overload; virtual; abstract;
        function luaL_prepbuffer(B: TLuaLBuffer): PAnsiChar; overload; virtual; abstract;
        function luaL_prepbuffsize(B: TLuaLBuffer; sz: Cardinal): PAnsiChar; overload; virtual; abstract;
        procedure luaL_register(L: PLuaState; libname: PAnsiChar; lib: PluaLReg); overload; virtual; abstract;
      {$ENDREGION}
    end;
  {$ENDREGION}


  {
    TLuaCommon
      - A Base Class (Abstract) for all Lua Classes.
  }
  {$REGION 'Base Class (Abstract) for all Lua Classes.'}
    TLuaCommon = class(TLuaBase, ILuaLibCommonMacrosLocal, ILuaLibInterchangeLocal, ILuaLibCommonLocal, ILuaCommonAuxLocal, ILuaAuxCommonMacrosLocal, ILuaAuxInterchangeLocal)
    protected
      FLuaState: PLuaState;
      function GetLinkType: TLuaBaseType; virtual; abstract;
    public
      {$REGION 'ILuaLibCommonMacrosLocal'}
        function lua_isboolean(idx: Integer): LongBool; overload; inline;
        function lua_isfunction(idx: Integer): LongBool; overload; inline;
        function lua_islightuserdata(idx: Integer): LongBool; overload; inline;
        function lua_isnil(idx: Integer): LongBool; overload; inline;
        function lua_isnone(idx: Integer): LongBool; overload; inline;
        function lua_isnoneornil(idx: Integer): LongBool; overload; inline;
        function lua_istable(idx: Integer): LongBool; overload; inline;
        function lua_isthread(idx: Integer): LongBool; overload; inline;
        procedure lua_newtable; overload; inline;
        procedure lua_pop(n: Integer); overload; inline;
        procedure lua_pushcfunction(f: TLuaDelphiFunction); overload; inline;
        function lua_pushliteral(s: PAnsiChar): PAnsiChar; overload; inline;
        function lua_pushlstring(const s: PAnsiChar; ls: Cardinal): PAnsiChar; overload; inline;
        procedure lua_register(name: PAnsiChar; f: TLuaDelphiFunction); overload; inline;
        function lua_tostring(idx: Integer): PAnsiChar; overload; inline;
      {$ENDREGION}
      {$REGION 'ILuaLibInterchangeLocal'}
        procedure lua_call(nargs, nresults: Integer); overload; inline;
        procedure lua_callk(nargs, nresults, ctx: Integer; k: TLuaDelphiFunction); overload; inline;
        function lua_compare(idx1, idx2, op: Integer): LongBool; overload; inline;
        function lua_cpcall(func: TLuaDelphiFunction; ud: Pointer): Integer; overload; inline;
        function lua_equal(idx1, idx2: Integer): LongBool; overload; inline;
        procedure lua_getfenv(idx: Integer); overload; inline;
        procedure lua_getglobal(name: PAnsiChar); overload; inline;
        procedure lua_getuservalue(idx: Integer); overload; inline;
        function lua_lessthan(idx1, idx2: Integer): LongBool; overload; inline;
        function lua_load(reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer; overload; inline; // 5.1 version
        function lua_load(reader: TLuaReaderFunction; dt: Pointer; Source, Mode: PAnsiChar): Integer; overload; inline; // 5.2 version
        function lua_objlen(idx: Integer): Cardinal; overload; inline;
        function lua_pcall(nargs, nresults, errfunc: Integer): Integer; overload; inline;
        function lua_pcallk(nargs, nresults, errfunc, ctx: Integer; k: TLuaDelphiFunction): Integer; overload; inline;
        function lua_rawlen(idx: Integer): Cardinal; overload; inline;
        function lua_resume(narg: Integer): Integer; overload; inline; // 5.1 version
        function lua_setfenv(idx: Integer): LongBool; overload; inline;
        procedure lua_setglobal(name: PAnsiChar); overload; inline;
        function lua_setmetatable(objindex: Integer): LongBool; overload; inline;
        procedure lua_setuservalue(idx: Integer); overload; inline;
        function lua_tointeger(idx: Integer): Integer; overload; inline;
        function lua_tointegerx(idx: Integer; isnum: PInteger): Integer; overload; inline;
        function lua_tonumber(idx: Integer): Double; overload; inline;
        function lua_tonumberx(idx: Integer; isnum: PInteger): Double; overload; inline;
        function lua_yield(nresults: Integer): Integer; overload; inline;
        function lua_yieldk(nresults, ctx: Integer; k: TLuaDelphiFunction): Integer; overload; inline;
      {$ENDREGION}
      {$REGION 'ILuaLibCommonLocal'}
        function lua_atpanic(panicf: TLuaDelphiFunction): TLuaDelphiFunction; overload; inline;
        function lua_checkstack(sz: Integer): LongBool; overload; inline;
        procedure lua_close; overload; inline;
        procedure lua_concat(n: Integer); overload; inline;
        procedure lua_createtable(narr, nrec: Integer); overload; inline;
        function lua_dump(writer: TLuaWriterFunction; data: Pointer): Integer; overload; inline;
        function lua_error: Integer; overload; inline;
        function lua_gc(what, data: Integer): Integer; overload; inline;
        function lua_getallocf(ud: PPointer): TLuaAllocFunction; overload; inline;
        procedure lua_getfield(idx: Integer; k: PAnsiChar); overload; inline;
        function lua_gethook: TLuaHookFunction; overload; inline;
        function lua_gethookcount: Integer; overload; inline;
        function lua_gethookmask: Integer; overload; inline;
        function lua_getinfo(const what: PAnsiChar; ar: PLuaDebug): Integer; overload; inline;
        function lua_getlocal(ar: PLuaDebug; n: Integer): PAnsiChar; overload; inline;
        function lua_getmetatable(objindex: Integer): LongBool; overload; inline;
        function lua_getstack(level: Integer; ar: PLuaDebug): Integer; overload; inline;
        procedure lua_gettable( idx: Integer); overload; inline;
        function lua_gettop: Integer; overload; inline;
        function lua_getupvalue(funcindex, n: Integer): PAnsiChar; overload; inline;
        procedure lua_insert(idx: Integer); overload; inline;
        function lua_iscfunction(idx: Integer): LongBool; overload; inline;
        function lua_isnumber(idx: Integer): LongBool; overload; inline;
        function lua_isstring(idx: Integer): LongBool; overload; inline;
        function lua_isuserdata(idx: Integer): LongBool; overload; inline;
        function lua_newthread: PLuaState; overload; inline;
        function lua_newuserdata(sz: Cardinal): Pointer; overload; inline;
        function lua_next(idx: Integer): Integer; overload; inline;
        procedure lua_pushboolean(b: LongBool); overload; inline;
        procedure lua_pushcclosure(fn: TLuaDelphiFunction; n: Integer); overload; inline;
        function lua_pushfstring(const fmt: PAnsiChar): PAnsiChar; {varargs;} overload; inline;
        procedure lua_pushinteger(n: Integer); overload; inline;
        procedure lua_pushlightuserdata(p: Pointer); overload; inline;
        procedure lua_pushnil; overload; inline;
        procedure lua_pushnumber(n: Double); overload; inline;
        function lua_pushstring(const s: PAnsiChar): PAnsiChar; overload; inline;
        function lua_pushthread: LongBool; overload; inline;
        procedure lua_pushvalue(idx: Integer); overload; inline;
        function lua_pushvfstring(const fmt: PAnsiChar; argp: Pointer): PAnsiChar; overload; inline;
        function lua_rawequal(idx1, idx2: Integer): LongBool; overload; inline;
        procedure lua_rawget(idx: Integer); overload; inline;
        procedure lua_rawgeti(idx, n: Integer); overload; inline;
        procedure lua_rawset(idx: Integer); overload; inline;
        procedure lua_rawseti(idx , n: Integer); overload; inline;
        procedure lua_remove(idx: Integer); overload; inline;
        procedure lua_replace(idx: Integer); overload; inline;
        procedure lua_setallocf(f: TLuaAllocFunction; ud: Pointer); overload; inline;
        procedure lua_setfield(idx: Integer; const k: PAnsiChar); overload; inline;
        function lua_sethook(func: TLuaHookFunction; mask, count: Integer): Integer; overload; inline;
        function lua_setlocal(ar: PLuaDebug; n: Integer): PAnsiChar; overload; inline;
        procedure lua_settable(idx: Integer); overload; inline;
        procedure lua_settop(idx: Integer); overload; inline;
        function lua_setupvalue(funcindex, n: Integer): PAnsiChar; overload; inline;
        function lua_status: Integer; overload; inline;
        function lua_toboolean(idx: Integer): LongBool; overload; inline;
        function lua_tocfunction(idx: Integer): TLuaDelphiFunction; overload; inline;
        function lua_tolstring(idx: Integer; len: PCardinal): PAnsiChar; overload; inline;
        function lua_topointer(idx: Integer): Pointer; overload; inline;
        function lua_tothread(idx: Integer): PLuaState; overload; inline;
        function lua_touserdata(idx: Integer): Pointer; overload; inline;
        function lua_type(idx: Integer): Integer; overload; inline;
        function lua_typename(tp: Integer): PAnsiChar; overload; inline;
        procedure lua_xmove(dest: PLuaState; n: Integer); overload; inline;
        function luaopen_base: Integer; overload; inline;
        function luaopen_debug: Integer; overload; inline;
        function luaopen_io: Integer; overload; inline;
        function luaopen_math: Integer; overload; inline;
        function luaopen_os: Integer; overload; inline;
        function luaopen_package: Integer; overload; inline;
        function luaopen_string: Integer; overload; inline;
        function luaopen_table: Integer; overload; inline;
      {$ENDREGION}
      {$REGION 'ILuaAuxCommonLocal'}
        function luaL_argerror(numarg: Integer; const extramsg: PAnsiChar): Integer; overload; inline;
        procedure luaL_buffinit(B: PLuaLBuffer); overload; inline;
        function luaL_callmeta(obj: Integer; const e: PAnsiChar): Integer; overload; inline;
        procedure luaL_checkany(narg: Integer); overload; inline;
        function luaL_checkinteger(numArg: Integer): Integer; overload; inline;
        function luaL_checklstring(numArg: Integer; ls: PCardinal): PAnsiChar; overload; inline;
        function luaL_checknumber(numArg: Integer): Double; overload; inline;
        function luaL_checkoption(narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer; overload;
        procedure luaL_checkstack(sz: Integer; const msg: PAnsiChar); overload; inline;
        procedure luaL_checktype(narg, t: Integer); overload; inline;
        function luaL_checkudata(ud: Integer; const tname: PAnsiChar): Pointer; overload; inline;
        function luaL_error(const fmt: PAnsiChar): Integer; {varargs;} overload; inline;
        function luaL_getmetafield(obj: Integer; const e: PAnsiChar): Integer; overload; inline;
        function luaL_gsub(const s, p, r: PAnsiChar): PAnsiChar; overload; inline;
        function luaL_loadstring(const s: PAnsiChar): Integer; overload; inline;
        function luaL_newmetatable(const tname: PAnsiChar): Integer; overload; inline;
        procedure luaL_openlibs; overload; inline;
        function luaL_optinteger(nArg: Integer; def: Integer): Integer; overload; inline;
        function luaL_optnumber(nArg: Integer; def: Double): Double; overload; inline;
        function luaL_optlstring(numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar; overload; inline;
        function luaL_ref(t: Integer): Integer; overload; inline;
        procedure luaL_unref(t, ref: Integer); overload; inline;
        procedure luaL_where(lvl: Integer); overload; inline;
      {$ENDREGION}
      {$REGION 'ILuaAuxCommonMacrosLocal'}
        function luaL_checkint(narg: Integer): Integer; overload; inline;
        function luaL_checklong(narg: Cardinal): Cardinal; overload; inline;
        function luaL_checkstring(narg: Integer): PAnsiChar; overload; inline;
        function luaL_dofile(filename: PAnsiChar): Integer; overload; inline;
        function luaL_dostring(str: PAnsiChar): Integer; overload; inline;
        procedure luaL_getmetatable(tname: PAnsiChar); overload; inline;
        function luaL_optint(narg, d: Integer): Integer; overload; inline;
        function luaL_optlong(narg: Integer; d: Cardinal): Cardinal; overload; inline;
        function luaL_optstring(narg: Integer; d: PAnsiChar): PAnsiChar; overload; inline;
        function luaL_typename(index: Integer): PAnsiChar; overload; inline;
      {$ENDREGION}
      {$REGION 'ILuaAuxInterchangeLocal'}
        function luaL_loadbuffer(buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; overload; inline;
        function luaL_loadbufferx(buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer; overload; inline;
        function luaL_loadfile(filename: PAnsiChar): Integer; overload; inline;
        function luaL_loadfilex(filename, mode: PAnsiChar): Integer; overload; inline;
        procedure luaL_register(libname: PAnsiChar; lib: PluaLReg); overload; inline;
      {$ENDREGION}
      constructor Create; overload;
      constructor Create(const ALuaState: PLuaState); overload;
      property LuaState: PLuaState read FLuaState;
    end;
  {$ENDREGION}

  {
    TLuaBaseMember
      - A Base Class for all those linking back to TLuaBase!
  }
  {$REGION 'Base Class for all those linking back to TLuaCommon!'}
    TLuaBaseMember = class(TPersistent)
    protected
      FLua: TLuaCommon;
    public
      constructor Create(const ALua: TLuaCommon);
    end;
  {$ENDREGION}

var
  LuaLinkType: TLuaBaseType;

implementation

{$REGION 'TLuaBase - Lua Base Type (no Localizers)'}
  { TLuaBase }

  function TLuaBase.LoadLuaLibrary(const L: PLuaState; const ALibName: AnsiString; ALibFunc: TLuaDelphiFunction): Integer;
  begin
    lua_pushcfunction(L, ALibFunc);
    lua_pushstring(L, PAnsiChar(ALibName));
    Result := (lua_pcall(L, 1, 0, 0));
  end;

function TLuaBase.luaL_checkint(L: PLuaState; narg: Integer): Integer;
  begin
    Result := luaL_checkinteger(L, narg);
  end;

  function TLuaBase.luaL_checklong(L: PLuaState; narg: Cardinal): Cardinal;
  begin
    Result := Cardinal(luaL_checkinteger(L, narg));
  end;

  function TLuaBase.luaL_checkstring(L: PLuaState; narg: Integer): PAnsiChar;
  begin
    Result := luaL_checklstring(L, narg, nil);
  end;

  function TLuaBase.luaL_dofile(L: PLuaState; filename: PAnsiChar): Integer;
  begin
    luaL_loadfile(L, filename);
    Result := lua_pcall(L, 0, 0, 0);
  end;

  function TLuaBase.luaL_dostring(L: PLuaState; str: PAnsiChar): Integer;
  begin
    luaL_loadstring(L, str);
    Result := lua_pcall(L, 0, 0, 0);
  end;

  procedure TLuaBase.luaL_getmetatable(L: PLuaState; tname: PAnsiChar);
  begin
    lua_getfield(L, LUA_REGISTRYINDEX, tname);
  end;

  function TLuaBase.luaL_optint(L: PLuaState; narg, d: Integer): Integer;
  begin
    Result := luaL_optinteger(L, narg, d);
  end;

  function TLuaBase.luaL_optlong(L: PLuaState; narg: Integer; d: Cardinal): Cardinal;
  begin
    Result := Cardinal(luaL_optinteger(L, narg, d));
  end;

  function TLuaBase.luaL_optstring(L: PLuaState; narg: Integer; d: PAnsiChar): PAnsiChar;
  begin
    Result := luaL_optlstring(L, narg, d, nil);
  end;

  function TLuaBase.luaL_typename(L: PLuaState; index: Integer): PAnsiChar;
  begin
    Result := lua_typename(L, lua_type(L, index));
  end;

  function TLuaBase.lua_isboolean(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_type(L, idx) = LUA_TBOOLEAN);
  end;

  function TLuaBase.lua_isfunction(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_type(L, idx) = LUA_TFUNCTION);
  end;

  function TLuaBase.lua_islightuserdata(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_type(L, idx) = LUA_TLIGHTUSERDATA);
  end;

  function TLuaBase.lua_isnil(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_type(L, idx) = LUA_TNIL);
  end;

  function TLuaBase.lua_isnone(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_type(L, idx) = LUA_TNONE);
  end;

  function TLuaBase.lua_isnoneornil(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_isnone(L, idx) or lua_isnil(L, idx));
  end;

  function TLuaBase.lua_istable(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_type(L, idx) = LUA_TTABLE);
  end;

  function TLuaBase.lua_isthread(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := (lua_type(L, idx) = LUA_TTHREAD);
  end;

  procedure TLuaBase.lua_newtable(L: PLuaState);
  begin
    lua_createtable(L, 0, 0);
  end;

  procedure TLuaBase.lua_pop(L: PLuaState; n: Integer);
  begin
    lua_settop(L, {-(n)-1} n + 1);
  end;

  procedure TLuaBase.lua_pushcfunction(L: PLuaState; f: TLuaDelphiFunction);
  begin
    lua_pushcclosure(L, f, 0);
  end;

  procedure TLuaBase.lua_register(L: PLuaState; name: PAnsiChar; f: TLuaDelphiFunction);
  begin
    lua_pushcfunction(L, f);
    lua_setglobal(L, name);
  end;

  function TLuaBase.lua_tostring(L: PLuaState; idx: Integer): PAnsiChar;
  begin
    Result := lua_tolstring(L, idx, nil);
  end;

{$ENDREGION}

{$REGION 'TLuaCommon - Lua Common Type (with Localizers)'}
  { TLuaCommon }

  constructor TLuaCommon.Create;
  begin
    inherited;
    SetLuaLibRefs;
    FLuaState := luaL_newstate;
    lua_settop(0);
  end;

  constructor TLuaCommon.Create(const ALuaState: PLuaState);
  begin
    inherited Create;
    SetLuaLibRefs;
    FLuaState := ALuaState;
  end;

function TLuaCommon.luaL_argerror(numarg: Integer; const extramsg: PAnsiChar): Integer;
  begin
    Result := luaL_argerror(FLuaState, numarg, extramsg);
  end;

  procedure TLuaCommon.luaL_buffinit(B: PLuaLBuffer);
  begin
    luaL_buffinit(FLuaState, B);
  end;

  function TLuaCommon.luaL_callmeta(obj: Integer; const e: PAnsiChar): Integer;
  begin
    Result := luaL_callmeta(FLuaState, obj, e);
  end;

  procedure TLuaCommon.luaL_checkany(narg: Integer);
  begin
    luaL_checkany(FLuaState, narg);
  end;

  function TLuaCommon.luaL_checkint(narg: Integer): Integer;
  begin
    Result := luaL_checkint(FLuaState, narg);
  end;

  function TLuaCommon.luaL_checkinteger(numArg: Integer): Integer;
  begin
    Result := luaL_checkinteger(FLuaState, numArg);
  end;

  function TLuaCommon.luaL_checklong(narg: Cardinal): Cardinal;
  begin
    Result := luaL_checklong(FLuaState, narg);
  end;

  function TLuaCommon.luaL_checklstring(numArg: Integer; ls: PCardinal): PAnsiChar;
  begin
    Result := luaL_checklstring(FLuaState, numArg, ls);
  end;

  function TLuaCommon.luaL_checknumber(numArg: Integer): Double;
  begin
    Result := luaL_checknumber(FLuaState, numArg);
  end;

  function TLuaCommon.luaL_checkoption(narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer;
  begin
    Result := luaL_checkoption(FLuaState, narg, def, lst);
  end;

  procedure TLuaCommon.luaL_checkstack(sz: Integer; const msg: PAnsiChar);
  begin
    luaL_checkstack(FLuaState, sz, msg);
  end;

  function TLuaCommon.luaL_checkstring(narg: Integer): PAnsiChar;
  begin
    Result := luaL_checkstring(FLuaState, narg);
  end;

  procedure TLuaCommon.luaL_checktype(narg, t: Integer);
  begin
    luaL_checktype(FLuaState, narg, t);
  end;

  function TLuaCommon.luaL_checkudata(ud: Integer; const tname: PAnsiChar): Pointer;
  begin
    Result := luaL_checkudata(FLuaState, ud, tname);
  end;

  function TLuaCommon.luaL_dofile(filename: PAnsiChar): Integer;
  begin
    Result := luaL_dofile(FLuaState, filename);
  end;

  function TLuaCommon.luaL_dostring(str: PAnsiChar): Integer;
  begin
    Result := luaL_dostring(FLuaState, str);
  end;

  function TLuaCommon.luaL_error(const fmt: PAnsiChar): Integer;
  begin
    Result := luaL_error(FLuaState, fmt);
  end;

  function TLuaCommon.luaL_getmetafield(obj: Integer; const e: PAnsiChar): Integer;
  begin
    Result := luaL_getmetafield(FLuaState, obj, e);
  end;

  procedure TLuaCommon.luaL_getmetatable(tname: PAnsiChar);
  begin
    luaL_getmetatable(FLuaState, tname);
  end;

  function TLuaCommon.luaL_gsub(const s, p, r: PAnsiChar): PAnsiChar;
  begin
    Result := luaL_gsub(FLuaState, s, p, r);
  end;

  function TLuaCommon.luaL_loadbuffer(buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer;
  begin
    Result := luaL_loadbuffer(FLuaState, buff, sz, name);
  end;

  function TLuaCommon.luaL_loadbufferx(buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer;
  begin
    Result := luaL_loadbufferx(FLuaState, buff, sz, name, mode);
  end;

  function TLuaCommon.luaL_loadfile(filename: PAnsiChar): Integer;
  begin
    Result := luaL_loadfile(FLuaState, filename);
  end;

  function TLuaCommon.luaL_loadfilex(filename, mode: PAnsiChar): Integer;
  begin
    Result := luaL_loadfilex(FLuaState, filename, mode);
  end;

  function TLuaCommon.luaL_loadstring(const s: PAnsiChar): Integer;
  begin
    Result := luaL_loadstring(FLuaState, s);
  end;

  function TLuaCommon.luaL_newmetatable(const tname: PAnsiChar): Integer;
  begin
    Result := luaL_newmetatable(FLuaState, tname);
  end;

  procedure TLuaCommon.luaL_openlibs;
  begin
    luaL_openlibs(FLuaState);
  end;

  function TLuaCommon.luaL_optint(narg, d: Integer): Integer;
  begin
    Result := luaL_optint(FLuaState, narg, d);
  end;

  function TLuaCommon.luaL_optinteger(nArg, def: Integer): Integer;
  begin
    Result := luaL_optinteger(FLuaState, nArg, def);
  end;

  function TLuaCommon.luaL_optlong(narg: Integer; d: Cardinal): Cardinal;
  begin
    Result := luaL_optlong(FLuaState, narg, d);
  end;

  function TLuaCommon.luaL_optlstring(numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar;
  begin
    Result := luaL_optlstring(FLuaState, numArg, def, ls);
  end;

  function TLuaCommon.luaL_optnumber(nArg: Integer; def: Double): Double;
  begin
    Result := luaL_optnumber(FLuaState, nArg, def);
  end;

  function TLuaCommon.luaL_optstring(narg: Integer; d: PAnsiChar): PAnsiChar;
  begin
    Result := luaL_optstring(FLuaState, narg, d);
  end;

  function TLuaCommon.luaL_ref(t: Integer): Integer;
  begin
    Result := luaL_ref(FLuaState, t);
  end;

  procedure TLuaCommon.luaL_register(libname: PAnsiChar; lib: PluaLReg);
  begin
    luaL_register(FLuaState, libname, lib);
  end;

  function TLuaCommon.luaL_typename(index: Integer): PAnsiChar;
  begin
    Result := luaL_typename(FLuaState, index);
  end;

  procedure TLuaCommon.luaL_unref(t, ref: Integer);
  begin
    luaL_unref(FLuaState, t, ref);
  end;

  procedure TLuaCommon.luaL_where(lvl: Integer);
  begin
    luaL_where(FLuaState, lvl);
  end;

  function TLuaCommon.luaopen_base: Integer;
  begin
    Result := luaopen_base(FLuaState);
  end;

  function TLuaCommon.luaopen_debug: Integer;
  begin
    Result := luaopen_debug(FLuaState);
  end;

  function TLuaCommon.luaopen_io: Integer;
  begin
    Result := luaopen_io(FLuaState);
  end;

  function TLuaCommon.luaopen_math: Integer;
  begin
    Result := luaopen_math(FLuaState);
  end;

  function TLuaCommon.luaopen_os: Integer;
  begin
    Result := luaopen_os(FLuaState);
  end;

  function TLuaCommon.luaopen_package: Integer;
  begin
    Result := luaopen_package(FLuaState);
  end;

  function TLuaCommon.luaopen_string: Integer;
  begin
    Result := luaopen_string(FLuaState);
  end;

  function TLuaCommon.luaopen_table: Integer;
  begin
    Result := luaopen_table(FLuaState);
  end;

  function TLuaCommon.lua_atpanic(panicf: TLuaDelphiFunction): TLuaDelphiFunction;
  begin
    Result := lua_atpanic(FLuaState, panicf);
  end;

  procedure TLuaCommon.lua_call(nargs, nresults: Integer);
  begin
    lua_call(FLuaState, nargs, nresults);
  end;

  procedure TLuaCommon.lua_callk(nargs, nresults, ctx: Integer; k: TLuaDelphiFunction);
  begin
    lua_callk(FLuastate, nargs, nresults, ctx, k);
  end;

  function TLuaCommon.lua_checkstack(sz: Integer): LongBool;
  begin
    Result := lua_checkstack(FLuaState, sz);
  end;

  procedure TLuaCommon.lua_close;
  begin
    lua_close(FLuaState);
  end;

  function TLuaCommon.lua_compare(idx1, idx2, op: Integer): LongBool;
  begin
    Result := lua_compare(FLuastate, idx1, idx2, op);
  end;

  procedure TLuaCommon.lua_concat(n: Integer);
  begin
    lua_concat(FLuaState, n);
  end;

  function TLuaCommon.lua_cpcall(func: TLuaDelphiFunction; ud: Pointer): Integer;
  begin
    Result := lua_cpcall(FLuaState, func, ud);
  end;

  procedure TLuaCommon.lua_createtable(narr, nrec: Integer);
  begin
    lua_createtable(FLuaState, narr, nrec);
  end;

  function TLuaCommon.lua_dump(writer: TLuaWriterFunction; data: Pointer): Integer;
  begin
    Result := lua_dump(FLuaState, writer, data);
  end;

  function TLuaCommon.lua_equal(idx1, idx2: Integer): LongBool;
  begin
    Result := lua_equal(FLuaState, idx1, idx2);
  end;

  function TLuaCommon.lua_error: Integer;
  begin
    Result := lua_error(FLuaState);
  end;

  function TLuaCommon.lua_gc(what, data: Integer): Integer;
  begin
    Result := lua_gc(FLuaState, what, data);
  end;

  function TLuaCommon.lua_getallocf(ud: PPointer): TLuaAllocFunction;
  begin
    Result := lua_getallocf(FLuaState, ud);
  end;

  procedure TLuaCommon.lua_getfenv(idx: Integer);
  begin
    lua_getfenv(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_getfield(idx: Integer; k: PAnsiChar);
  begin
    lua_getfield(FLuaState, idx, k);
  end;

  procedure TLuaCommon.lua_getglobal(name: PAnsiChar);
  begin
    lua_getglobal(FLuaState, name);
  end;

  function TLuaCommon.lua_gethook: TLuaHookFunction;
  begin
    Result := lua_gethook(FLuaState);
  end;

  function TLuaCommon.lua_gethookcount: Integer;
  begin
    Result := lua_gethookcount(FLuaState);
  end;

  function TLuaCommon.lua_gethookmask: Integer;
  begin
    Result := lua_gethookmask(FLuaState);
  end;

  function TLuaCommon.lua_getinfo(const what: PAnsiChar; ar: PLuaDebug): Integer;
  begin
    Result := lua_getinfo(FLuaState, what, ar);
  end;

  function TLuaCommon.lua_getlocal(ar: PLuaDebug; n: Integer): PAnsiChar;
  begin
    Result := lua_getlocal(FLuaState, ar, n);
  end;

  function TLuaCommon.lua_getmetatable(objindex: Integer): LongBool;
  begin
    Result := lua_getmetatable(FLuaState, objindex);
  end;

  function TLuaCommon.lua_getstack(level: Integer; ar: PLuaDebug): Integer;
  begin
    Result := lua_getstack(FLuaState, level, ar);
  end;

  procedure TLuaCommon.lua_gettable(idx: Integer);
  begin
    lua_gettable(FLuaState, idx);
  end;

  function TLuaCommon.lua_gettop: Integer;
  begin
    Result := lua_gettop(FLuaState);
  end;

  function TLuaCommon.lua_getupvalue(funcindex, n: Integer): PAnsiChar;
  begin
    Result := lua_getupvalue(FLuaState, funcindex, n);
  end;

  procedure TLuaCommon.lua_getuservalue(idx: Integer);
  begin
    lua_getuservalue(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_insert(idx: Integer);
  begin
    lua_insert(FLuaState, idx);
  end;

  function TLuaCommon.lua_isboolean(idx: Integer): LongBool;
  begin
    Result := lua_isboolean(FLuaState, idx);
  end;

  function TLuaCommon.lua_iscfunction(idx: Integer): LongBool;
  begin
    Result := lua_iscfunction(FLuaState, idx);
  end;

  function TLuaCommon.lua_isfunction(idx: Integer): LongBool;
  begin
    Result := lua_isfunction(FLuaState, idx);
  end;

  function TLuaCommon.lua_islightuserdata(idx: Integer): LongBool;
  begin
    Result := lua_islightuserdata(FLuaState, idx);
  end;

  function TLuaCommon.lua_isnil(idx: Integer): LongBool;
  begin
    Result := lua_isnil(FLuastate, idx);
  end;

  function TLuaCommon.lua_isnone(idx: Integer): LongBool;
  begin
    Result := lua_isnone(FLuaState, idx);
  end;

  function TLuaCommon.lua_isnoneornil(idx: Integer): LongBool;
  begin
    Result := lua_isnoneornil(FLuaState, idx);
  end;

  function TLuaCommon.lua_isnumber(idx: Integer): LongBool;
  begin
    Result := lua_isnumber(FLuaState, idx);
  end;

  function TLuaCommon.lua_isstring(idx: Integer): LongBool;
  begin
    Result := lua_isstring(FLuaState, idx);
  end;

  function TLuaCommon.lua_istable(idx: Integer): LongBool;
  begin
    Result := lua_istable(FLuaState, idx);
  end;

  function TLuaCommon.lua_isthread(idx: Integer): LongBool;
  begin
    Result := lua_isthread(FLuaState, idx);
  end;

  function TLuaCommon.lua_isuserdata(idx: Integer): LongBool;
  begin
    Result := lua_isuserdata(FLuaState, idx);
  end;

  function TLuaCommon.lua_lessthan(idx1, idx2: Integer): LongBool;
  begin
    Result := lua_lessthan(FLuaState, idx1, idx2);
  end;

  function TLuaCommon.lua_load(reader: TLuaReaderFunction; dt: Pointer; Source, Mode: PAnsiChar): Integer;
  begin
    Result := lua_load(FLuaState, reader, dt, source, mode);
  end;

  function TLuaCommon.lua_load(reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer;
  begin
    Result := lua_load(FLuaState, reader, dt, chunkname, nil);
  end;

  procedure TLuaCommon.lua_newtable;
  begin
    lua_newtable(FLuaState);
  end;

  function TLuaCommon.lua_newthread: PLuaState;
  begin
    Result := lua_newthread(FLuaState);
  end;

  function TLuaCommon.lua_newuserdata(sz: Cardinal): Pointer;
  begin
    Result := lua_newuserdata(FLuaState, sz);
  end;

  function TLuaCommon.lua_next(idx: Integer): Integer;
  begin
    Result := lua_next(FLuaState, idx);
  end;

  function TLuaCommon.lua_objlen(idx: Integer): Cardinal;
  begin
    Result := lua_objlen(FLuaState, idx);
  end;

  function TLuaCommon.lua_pcall(nargs, nresults, errfunc: Integer): Integer;
  begin
    Result := lua_pcall(FLuaState, nargs, nresults, errfunc);
  end;

  function TLuaCommon.lua_pcallk(nargs, nresults, errfunc, ctx: Integer; k: TLuaDelphiFunction): Integer;
  begin
    Result := lua_pcallk(FLuaState, nargs, nresults, errfunc, ctx, k);
  end;

  procedure TLuaCommon.lua_pop(n: Integer);
  begin
    lua_pop(FLuaState, n);
  end;

  procedure TLuaCommon.lua_pushboolean(b: LongBool);
  begin
    lua_pushboolean(FLuaState, b);
  end;

  procedure TLuaCommon.lua_pushcclosure(fn: TLuaDelphiFunction; n: Integer);
  begin
    lua_pushcclosure(FLuaState, fn, n);
  end;

  procedure TLuaCommon.lua_pushcfunction(f: TLuaDelphiFunction);
  begin
    lua_pushcfunction(FLuaState, f);
  end;

  function TLuaCommon.lua_pushfstring(const fmt: PAnsiChar): PAnsiChar;
  begin
    Result := lua_pushfstring(FLuaState, fmt);
  end;

  procedure TLuaCommon.lua_pushinteger(n: Integer);
  begin
    lua_pushinteger(FLuaState, n);
  end;

  procedure TLuaCommon.lua_pushlightuserdata(p: Pointer);
  begin
    lua_pushlightuserdata(FLuaState, p);
  end;

  function TLuaCommon.lua_pushliteral(s: PAnsiChar): PAnsiChar;
  begin
    Result := lua_pushliteral(FLuaState, s);
  end;

  function TLuaCommon.lua_pushlstring(const s: PAnsiChar; ls: Cardinal): PAnsiChar;
  begin
    Result := lua_pushlstring(FLuaState, s, ls);
  end;

  procedure TLuaCommon.lua_pushnil;
  begin
    lua_pushnil(FLuaState);
  end;

  procedure TLuaCommon.lua_pushnumber(n: Double);
  begin
    lua_pushnumber(FLuaState, n);
  end;

  function TLuaCommon.lua_pushstring(const s: PAnsiChar): PAnsiChar;
  begin
    Result := lua_pushstring(FLuaState, s);
  end;

  function TLuaCommon.lua_pushthread: LongBool;
  begin
    Result := lua_pushthread(FLuaState);
  end;

  procedure TLuaCommon.lua_pushvalue(idx: Integer);
  begin
    lua_pushvalue(FLuaState, idx);
  end;

  function TLuaCommon.lua_pushvfstring(const fmt: PAnsiChar; argp: Pointer): PAnsiChar;
  begin
    Result := lua_pushvfstring(FLuaState, fmt, argp);
  end;

  function TLuaCommon.lua_rawequal(idx1, idx2: Integer): LongBool;
  begin
    Result := lua_rawequal(FLuaState, idx1, idx2);
  end;

  procedure TLuaCommon.lua_rawget(idx: Integer);
  begin
    lua_rawget(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_rawgeti(idx, n: Integer);
  begin
    lua_rawgeti(FLuaState, idx, n);
  end;

  function TLuaCommon.lua_rawlen(idx: Integer): Cardinal;
  begin
    Result := lua_rawlen(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_rawset(idx: Integer);
  begin
    lua_rawset(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_rawseti(idx, n: Integer);
  begin
    lua_rawseti(FLuaState, idx, n);
  end;

  procedure TLuaCommon.lua_register(name: PAnsiChar; f: TLuaDelphiFunction);
  begin
    lua_register(FLuaState, name, f);
  end;

  procedure TLuaCommon.lua_remove(idx: Integer);
  begin
    lua_remove(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_replace(idx: Integer);
  begin
    lua_replace(FLuaState, idx);
  end;

  function TLuaCommon.lua_resume(narg: Integer): Integer;
  begin
    Result := lua_resume(FLuaState, narg);
  end;

  procedure TLuaCommon.lua_setallocf(f: TLuaAllocFunction; ud: Pointer);
  begin
    lua_setallocf(FLuaState, f, ud);
  end;

  function TLuaCommon.lua_setfenv(idx: Integer): LongBool;
  begin
    Result := lua_setfenv(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_setfield(idx: Integer; const k: PAnsiChar);
  begin
    lua_setfield(FLuaState, idx, k);
  end;

  procedure TLuaCommon.lua_setglobal(name: PAnsiChar);
  begin
    lua_setglobal(FLuaState, name);
  end;

  function TLuaCommon.lua_sethook(func: TLuaHookFunction; mask, count: Integer): Integer;
  begin
    Result := lua_Sethook(FLuaState, func, mask, count);
  end;

  function TLuaCommon.lua_setlocal(ar: PLuaDebug; n: Integer): PAnsiChar;
  begin
    Result := lua_setlocal(FLuaState, ar, n);
  end;

  function TLuaCommon.lua_setmetatable(objindex: Integer): LongBool;
  begin
    Result := lua_setmetatable(FLuaState, objindex);
  end;

  procedure TLuaCommon.lua_settable(idx: Integer);
  begin
    lua_settable(FLuaState, idx);
  end;

  procedure TLuaCommon.lua_settop(idx: Integer);
  begin
    lua_settop(FLuaState, idx);
  end;

  function TLuaCommon.lua_setupvalue(funcindex, n: Integer): PAnsiChar;
  begin
    Result := lua_setupvalue(FLuaState, funcindex, n);
  end;

  procedure TLuaCommon.lua_setuservalue(idx: Integer);
  begin
    lua_setuservalue(FLuaState, idx);
  end;

  function TLuaCommon.lua_status: Integer;
  begin
    Result := lua_status(FLuaState);
  end;

  function TLuaCommon.lua_toboolean(idx: Integer): LongBool;
  begin
    Result := lua_toboolean(FLuaState, idx);
  end;

  function TLuaCommon.lua_tocfunction(idx: Integer): TLuaDelphiFunction;
  begin
    Result := lua_tocfunction(FLuaState, idx);
  end;

  function TLuaCommon.lua_tointeger(idx: Integer): Integer;
  begin
    Result := lua_tointeger(FLuaState, idx);
  end;

  function TLuaCommon.lua_tointegerx(idx: Integer; isnum: PInteger): Integer;
  begin
    Result := lua_tointegerx(FLuaState, idx, isnum);
  end;

  function TLuaCommon.lua_tolstring(idx: Integer; len: PCardinal): PAnsiChar;
  begin
    Result := lua_tolstring(FLuaState, idx, len);
  end;

  function TLuaCommon.lua_tonumber(idx: Integer): Double;
  begin
    Result := lua_tonumber(FLuaState, idx);
  end;

  function TLuaCommon.lua_tonumberx(idx: Integer; isnum: PInteger): Double;
  begin
    Result := lua_tonumberx(FLuaState, idx, isnum);
  end;

  function TLuaCommon.lua_topointer(idx: Integer): Pointer;
  begin
    Result := lua_topointer(FLuaState, idx);
  end;

  function TLuaCommon.lua_tostring(idx: Integer): PAnsiChar;
  begin
    Result := lua_tostring(FLuaState, idx);
  end;

  function TLuaCommon.lua_tothread(idx: Integer): PLuaState;
  begin
    Result := lua_tothread(FLuaState, idx);
  end;

  function TLuaCommon.lua_touserdata(idx: Integer): Pointer;
  begin
    Result := lua_touserdata(FLuaState, idx);
  end;

  function TLuaCommon.lua_type(idx: Integer): Integer;
  begin
    Result := lua_type(FLuaState, idx);
  end;

  function TLuaCommon.lua_typename(tp: Integer): PAnsiChar;
  begin
    Result := lua_typename(FLuaState, tp);
  end;

  procedure TLuaCommon.lua_xmove(dest: PLuaState; n: Integer);
  begin
    lua_xmove(FLuaState, dest, n);
  end;

  function TLuaCommon.lua_yield(nresults: Integer): Integer;
  begin
    Result := lua_yield(FLuaState, nresults);
  end;

  function TLuaCommon.lua_yieldk(nresults, ctx: Integer; k: TLuaDelphiFunction): Integer;
  begin
    Result := lua_yieldk(FLuaState, nresults, ctx, k);
  end;
{$ENDREGION}

{$REGION 'TLuaBaseMember - Lua Base Member Type'}
  { TLuaBaseMember }

  constructor TLuaBaseMember.Create(const ALua: TLuaCommon);
  begin
    inherited Create;
    FLua := ALua;
  end;
{$ENDREGION}

end.
