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
  Unit: L4D.Lua.Lua52.pas
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
unit L4D.Lua.Lua52;

interface

{$I Lua4Delphi.inc}

uses
  L4D.Lua.Common, L4D.Lua.Intf;

const
{$REGION 'DLL name defines'}
  {$IFDEF MSWINDOWS}
    LUA_DLL = 'lua5.2.dll';
  {$ENDIF}
  {$IFDEF LINUX}
    LUA_DLL = 'lua5.2.so';
  {$ENDIF}
  {$IFDEF MACOS}
    LUA_DLL = 'liblua5.2.dylib';
  {$ENDIF}
{$ENDREGION}
  // Thread State
  LUA_ERRGCMM = 5;
  LUA_ERRERR = 6;
  // Basic Lua Type IDs
  LUA_NUMTAGS = 9;
  // predefined values in the registry
  LUA_RIDX_MAINTHREAD = 1;
  LUA_RIDX_GLOBALS = 2;
  LUA_RIDX_LAST = LUA_RIDX_GLOBALS;
  // Arithmetic functions
  LUA_OPADD = 0; // ORDER TM
  LUA_OPSUB = 1;
  LUA_OPMUL = 2;
  LUA_OPDIV = 3;
  LUA_OPMOD = 4;
  LUA_OPPOW = 5;
  LUA_OPUNM = 6;
  // Garbage Collection State IDs
  LUA_GCSETMAJORINC = 8;
  LUA_GCISRUNNING = 9;
  LUA_GCGEN = 10;
  LUA_GCINC = 11;
  // Lua Library Names
  LUA_BITLIBNAME = 'bit32';
  // Extra Error Code for 'LuaL_load'
  LUA_ERRFILE = LUA_ERRERR + 1;

type
  {
    TLua52Base
      - The Base Class for Lua 5.2
  }
  TLua52Base = class(TLuaCommon, ILua52Lib, ILua52Aux)
  protected
    {$REGION 'ILuaLibInterchange'}
      procedure lua_call(L: PLuaState; nargs, nresults: Integer); overload; override;// Macro in 5.2
      function lua_cpcall(L: PLuaState; func: TLuaDelphiFunction; ud: Pointer): Integer; overload; override; // Macro in 5.2
      function lua_equal(L: PLuaState; idx1, idx2: Integer): LongBool; overload; override; // Macro in 5.2
      procedure lua_getfenv(L: PLuaState; idx: Integer); overload; override; // Macro in 5.2
      function lua_lessthan(L: PLuaState; idx1, idx2: Integer): LongBool; overload; override; // Macro in 5.2
      function lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer; overload; override; // 5.1 version
      function lua_objlen(L: PLuaState; idx: Integer): Cardinal; overload; override;
      function lua_pcall(L: PLuaState; nargs, nresults, errfunc: Integer): Integer; overload; override; // Macro in 5.2
      function lua_setfenv(L: PLuaState; idx: Integer): LongBool; overload; override; // External in 5.1, "lua_setuservalue" in 5.2
      function lua_setmetatable(L: PLuaState; objindex: Integer): LongBool; overload; override; // 5.2 version
      function lua_tointeger(L: PLuaState; idx: Integer): Integer; overload; override; // In 5.1
      function lua_tonumber(L: PLuaState; idx: Integer): Double; overload; override; // In 5.1
      function lua_yield(L: PLuaState; nresults: Integer): Integer; overload; override; // External in 5.1, "lua_yieldk" in 5.2
    {$ENDREGION}
    {$REGION 'ILua52Lib'}
      function lua_absindex(L: PLuaState; idx: Integer): Integer; overload; virtual; abstract;
      function lua_arith(L: PLuaState; op: Integer): Integer; overload; virtual; abstract;
      procedure lua_copy(L: PLuaState; fromidx, toidx: Integer); overload; virtual; abstract;
      function lua_getctx(L: PLuaState; ctx: PInteger): Integer; overload; virtual; abstract;
      procedure lua_len(L: PLuaState; idx: Integer); overload; virtual; abstract;
      procedure lua_pushunsigned(L: PLuaState; u: Cardinal); overload; virtual; abstract;
      procedure lua_rawgetp(L: PLuaState; idx: Integer; p: Pointer); overload; virtual; abstract;
      procedure lua_rawsetp(L: PLuaState; idx: Integer; p: Pointer); overload; virtual; abstract;
      function lua_resume(L, from: PLuaState; narg: Integer): Integer; overload; virtual; abstract;
      function lua_tounsignedx(L: PLuaState; idx: Integer; isnum: PInteger): Cardinal; overload; virtual; abstract;
      function lua_upvalueid(L: PLuaState; funcidx, n: Integer): Pointer; overload; virtual; abstract;
      procedure lua_upvaluejoin(L: PLuaState; fidx1, n1, fidx2, n2: Integer); overload; virtual; abstract;
      function lua_version(L: PLuaState): PInteger; overload; virtual; abstract;
      function luaopen_bit32(L: PLuaState): Integer; overload; virtual; abstract;
      function luaopen_coroutine(L: PLuaState): Integer; overload; virtual; abstract;
    {$ENDREGION}
    {$REGION 'ILuaAux'}
      function luaL_buffinitsize(L: PLuaState; B: PLuaLBuffer; sz: Cardinal): PAnsiChar; overload; virtual; abstract;
      function luaL_checkunsigned(L: PLuaState; narg: Integer): Cardinal; overload; virtual; abstract;
      procedure luaL_checkversion(L: PLuaState); overload; virtual; abstract;
      function luaL_execresult(L: PLuaState; stat: Integer): Integer; overload; virtual; abstract;
      function luaL_fileresult(L: PLuaState; stat: Integer; fname: PAnsiChar): Integer; overload; virtual; abstract;
      function luaL_getsubtable(L: PLuaState; idx: Integer; fname: PAnsiChar): Integer; overload; virtual; abstract;
      function luaL_len(L: PLuaState; idx: Integer): Integer; overload; virtual; abstract;
      function luaL_loadbufferx(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer; overload; virtual; abstract;
      function luaL_loadfilex(L: PLuaState; filename, mode: PAnsiChar): Integer; overload; virtual; abstract;
      function luaL_optunsigned(L: PLuaState; narg: Integer; u: Cardinal): Cardinal; overload; virtual; abstract;
      function luaL_prepbuffsize(B: PLuaLBuffer; sz: Cardinal): PAnsiChar; overload; virtual; abstract;
      procedure luaL_pushresultsize(B: PLuaLBuffer; sz: Cardinal); overload; virtual; abstract;
      procedure luaL_requiref(L: PLuaState; modname: PansiChar; openf: TLuaDelphiFunction; glb: Integer); overload; virtual; abstract;
      procedure luaL_setfuncs(L: PLuaState; lreg: PluaLReg; nup: Integer); overload; virtual; abstract;
      procedure luaL_setmetatable(L: PluaState; tname: PAnsiChar); overload; virtual; abstract;
      function luaL_testudata(L: PLuaState; narg: Integer; tname: PAnsiChar): Pointer; overload; virtual; abstract;
      procedure luaL_traceback(L, L1: PLuaState; msg: PAnsiChar; level: Integer); overload; virtual; abstract;
    {$ENDREGION}
  end;

  {
    TLua52Common
      - The COMMON PARENT ANCESTOR for Lua 5.2 classes.
      - Inherited by TLua52Static, TLua52Dynamic, TLua52Embedded
  }
  TLua51Common = class(TLua52Base, ILua52LibLocal, ILua52AuxLocal)
  public
    {$REGION 'ILua52LibLocal'}
      function lua_absindex(idx: Integer): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function lua_arith(op: Integer): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure lua_copy(fromidx, toidx: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function lua_getctx(ctx: PInteger): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure lua_len(idx: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure lua_pushunsigned(u: Cardinal); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure lua_rawgetp(idx: Integer; p: Pointer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure lua_rawsetp(idx: Integer; p: Pointer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function lua_resume(from: PLuaState; narg: Integer; const UNUSED_PROPERTY: Boolean = True): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF} // 5.2 version
      function lua_tounsignedx(idx: Integer; isnum: PInteger): Cardinal; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function lua_upvalueid(funcidx, n: Integer): Pointer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure lua_upvaluejoin(fidx1, n1, fidx2, n2: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function lua_version: PInteger; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaopen_bit32: Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaopen_coroutine: Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
    {$ENDREGION}
    {$REGION 'ILua52AuxLocal'}
      function luaL_buffinitsize(B: PLuaLBuffer; sz: Cardinal): PAnsiChar; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_checkunsigned(narg: Integer): Cardinal; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure luaL_checkversion; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_execresult(stat: Integer): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_fileresult(stat: Integer; fname: PAnsiChar): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_getsubtable(idx: Integer; fname: PAnsiChar): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_len(idx: Integer): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_loadbufferx(buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_loadfilex(filename, mode: PAnsiChar): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_optunsigned(narg: Integer; u: Cardinal): Cardinal; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure luaL_requiref(modname: PansiChar; openf: TLuaDelphiFunction; glb: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure luaL_setfuncs(lreg: PluaLReg; nup: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure luaL_setmetatable(tname: PAnsiChar); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_testudata(narg: Integer; tname: PAnsiChar): Pointer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure luaL_traceback(L1: PLuaState; msg: PAnsiChar; level: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
    {$ENDREGION}
  end;

implementation

{$REGION 'Lua 5.2 Base Type'}
  { TLua52Base }

  procedure TLua52Base.lua_call(L: PLuaState; nargs, nresults: Integer);
  begin
    lua_callk(L, nargs, nresults, 0, nil);
  end;

  function TLua52Base.lua_cpcall(L: PLuaState; func: TLuaDelphiFunction; ud: Pointer): Integer;
  begin
    // Transparent Call
    lua_checkstack(1);
    lua_pushcfunction(func);
    lua_pushlightuserdata(ud);
    Result := lua_pcall(L, 0, 0, 0);
  end;

  function TLua52Base.lua_equal(L: PLuaState; idx1, idx2: Integer): LongBool;
  begin
    // Transparent Call
    Result := lua_compare(L, idx1, idx2, LUA_OPEQ);
  end;

  procedure TLua52Base.lua_getfenv(L: PLuaState; idx: Integer);
  begin
    lua_getuservalue(L, idx);
  end;

  function TLua52Base.lua_lessthan(L: PLuaState; idx1, idx2: Integer): LongBool;
  begin
    Result := lua_compare(L, idx1, idx2, LUA_OPLT);
  end;

  function TLua52Base.lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer;
  begin
    // Transparent Call
    Result := lua_load(L, reader, dt, chunkname, nil);
  end;

  function TLua52Base.lua_objlen(L: PLuaState; idx: Integer): Cardinal;
  begin
    // Transparent Call
    Result := lua_rawlen(L, idx);
  end;

  function TLua52Base.lua_pcall(L: PLuaState; nargs, nresults, errfunc: Integer): Integer;
  begin
    // Transparent Call
    Result := lua_pcallk(L, nargs, nresults, errfunc, 0, nil);
  end;

  function TLua52Base.lua_setfenv(L: PLuaState; idx: Integer): LongBool;
  begin
    Result := True;
    lua_setuservalue(L, idx);
  end;

  function TLua52Base.lua_setmetatable(L: PLuaState; objindex: Integer): LongBool;
  begin
    Result := True;
    lua_setmetatable(FLuaState, objindex);
  end;

  function TLua52Base.lua_tointeger(L: PLuaState; idx: Integer): Integer;
  begin
    Result := lua_tointegerx(L, idx, nil);
  end;

  function TLua52Base.lua_tonumber(L: PLuaState; idx: Integer): Double;
  begin
    Result := lua_tonumberx(L, idx, nil);
  end;

  function TLua52Base.lua_yield(L: PLuaState; nresults: Integer): Integer;
  begin
    // Transparent Call
    Result := lua_yieldk(L, nresults, 0, nil);
  end;
{$ENDREGION}

{$REGION 'Lua 5.2 Common Type'}
  { TLua51Common }

  function TLua51Common.luaL_buffinitsize(B: PLuaLBuffer; sz: Cardinal): PAnsiChar;
  begin
    Result := luaL_buffinitsize(FLuaState, B, sz);
  end;

  function TLua51Common.luaL_checkunsigned(narg: Integer): Cardinal;
  begin
    Result := luaL_checkunsigned(FLuaState, narg);
  end;

  procedure TLua51Common.luaL_checkversion;
  begin
    luaL_checkversion(FLuaState);
  end;

  function TLua51Common.luaL_execresult(stat: Integer): Integer;
  begin
    Result := luaL_execresult(FLuaState, stat);
  end;

  function TLua51Common.luaL_fileresult(stat: Integer; fname: PAnsiChar): Integer;
  begin
    Result := luaL_fileresult(FLuaState, stat, fname);
  end;

  function TLua51Common.luaL_getsubtable(idx: Integer; fname: PAnsiChar): Integer;
  begin
    Result := luaL_getsubtable(FLuaState, idx, fname);
  end;

  function TLua51Common.luaL_len(idx: Integer): Integer;
  begin
    Result := luaL_len(FLuaState, idx);
  end;

  function TLua51Common.luaL_loadbufferx(buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer;
  begin
    Result := luaL_loadbufferx(FLuaState, sz, name, mode);
  end;

  function TLua51Common.luaL_loadfilex(filename, mode: PAnsiChar): Integer;
  begin
    Result := luaL_loadfilex(FLuaState, filename, mode);
  end;

  function TLua51Common.luaL_optunsigned(narg: Integer; u: Cardinal): Cardinal;
  begin
    Result := luaL_optunsigned(FLuaState, narg, u);
  end;

  procedure TLua51Common.luaL_requiref(modname: PansiChar; openf: TLuaDelphiFunction; glb: Integer);
  begin
    luaL_requiref(FLuaState, modname, openf, glb);
  end;

  procedure TLua51Common.luaL_setfuncs(lreg: PluaLReg; nup: Integer);
  begin
    luaL_setfuncs(FLuaState, lreg, nup);
  end;

  procedure TLua51Common.luaL_setmetatable(tname: PAnsiChar);
  begin
    luaL_setmetatable(FLuaState, tname);
  end;

  function TLua51Common.luaL_testudata(narg: Integer; tname: PAnsiChar): Pointer;
  begin
    Result := luaL_testudata(FLuaState, narg, tname);
  end;

  procedure TLua51Common.luaL_traceback(L1: PLuaState; msg: PAnsiChar; level: Integer);
  begin
    luaL_traceback(FLuaState, L1, msg, level);
  end;

  function TLua51Common.luaopen_bit32: Integer;
  begin
    Result := luaopen_bit32(FLuaState);
  end;

  function TLua51Common.luaopen_coroutine: Integer;
  begin
    Result := luaopen_coroutine(FLuaState);
  end;

  function TLua51Common.lua_absindex(idx: Integer): Integer;
  begin
    Result := lua_absindex(FLuaState, idx);
  end;

  function TLua51Common.lua_arith(op: Integer): Integer;
  begin
    Result := lua_arith(FLuaState, op);
  end;

  procedure TLua51Common.lua_copy(fromidx, toidx: Integer);
  begin
    lua_copy(FLuaState, fromidx, toidx);
  end;

  function TLua51Common.lua_getctx(ctx: PInteger): Integer;
  begin
    Result := lua_getctx(FLuaState, ctx);
  end;

  procedure TLua51Common.lua_len(idx: Integer);
  begin
    lua_len(FLuaState, idx);
  end;

  procedure TLua51Common.lua_pushunsigned(u: Cardinal);
  begin
    lua_pushunsigned(FLuaState, u);
  end;

  procedure TLua51Common.lua_rawgetp(idx: Integer; p: Pointer);
  begin
    lua_rawgetp(FLuaState, idx, p);
  end;

  procedure TLua51Common.lua_rawsetp(idx: Integer; p: Pointer);
  begin
    lua_rawsetp(FLuaState, idx, p);
  end;

  function TLua51Common.lua_resume(from: PLuaState; narg: Integer; const UNUSED_PROPERTY: Boolean): Integer;
  begin
    Result := lua_resume(FLuaState, from, narg);
  end;

  function TLua51Common.lua_tounsignedx(idx: Integer; isnum: PInteger): Cardinal;
  begin
    Result := lua_tounsignedx(FLuaState, idx, isnum);
  end;

  function TLua51Common.lua_upvalueid(funcidx, n: Integer): Pointer;
  begin
    Result := lua_upvalueid(FLuaState, funcidx, n);
  end;

  procedure TLua51Common.lua_upvaluejoin(fidx1, n1, fidx2, n2: Integer);
  begin
    lua_upvaluejoin(FLuaState, fidx1, n1, fidx2, n2);
  end;

  function TLua51Common.lua_version: PInteger;
  begin
    Result := lua_version(FLuaState);
  end;
{$ENDREGION}

end.
