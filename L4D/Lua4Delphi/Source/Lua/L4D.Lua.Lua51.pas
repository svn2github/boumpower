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
unit L4D.Lua.Lua51;

interface

{$I Lua4Delphi.inc}

uses
  L4D.Lua.Common, L4D.Lua.Intf;

const
{$REGION 'DLL name defines'}
  {$IFDEF MSWINDOWS}
    LUA_DLL = 'lua5.1.dll';
  {$ENDIF}
  {$IFDEF LINUX}
    LUA_DLL = 'lua5.1.so';
  {$ENDIF}
  {$IFDEF MACOS}
    LUA_DLL = 'liblua5.1.dylib';
  {$ENDIF}
{$ENDREGION}
  // Thread State
  LUA_ERRERR = 5;
  // Extra Error Code for 'LuaL_load'
  LUA_ERRFILE = LUA_ERRERR + 1;

type
  {
    TLua51Base
      - The Base Class for Lua 5.1
  }
  TLua51Base = class(TLuaCommon, ILua51Lib, ILua51Aux)
  protected
    {$REGION 'ILuaLibInterchange'}
      procedure lua_callk(L: PLuaState; nargs, nresults, ctx: Integer; k: TLuaDelphiFunction); overload; override;    // External "lua_call" in 5.1, External "lua_callk" in 5.2
      function lua_compare(L: PLuaState; idx1, idx2, op: Integer): LongBool; overload; override;                      // "lua_equal" OR "lua_lessthan" in 5.1, External in 5.2
      procedure lua_getglobal(L: PLuaState; name: PAnsiChar); overload; override;                                     // "lua_getfield" in 5.1, External in 5.2
      procedure lua_getuservalue(L: PLuaState; idx: Integer); overload; override;                                     // "lua_getfenv" in 5.1, External in 5.2
      function lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; Source, Mode: PAnsiChar): Integer; overload; override;    // Extra parameter in 5.2 (mode)... 5.1 to pass "nil" to operate normally
      function lua_pcallk(L: PLuaState; nargs, nresults, arrfunc, ctx: Integer; k: TLuaDelphiFunction): Integer; overload; override;  // "lua_pcall" in 5.1, "lua_pcallk" in 5.2
      function lua_rawlen(L: PLuaState; idx: Integer): Cardinal; overload; override;                                  // "lua_objlen" in 5.1, External in 5.2
      procedure lua_setglobal(L: PLuaState; name: PAnsiChar); overload; override;                                     // "lua_setfield" in 5.1, External in 5.2
      procedure lua_setuservalue(L: PLuaState; idx: Integer); overload; override;                                     // "lua_getfenv" & "lua_setfenv" in 5.1, External in 5.2
      function lua_tointegerx(L: PLuaState; idx: Integer; isnum: PInteger): Integer; overload; override;              // In 5.2
      function lua_tonumberx(L: PLuaState; idx: Integer; isnum: PInteger): Double; overload; override;                // In 5.2
      function lua_yieldk(L: PLuaState; nresults, ctx: Integer; k: TLuaDelphiFunction): Integer; overload; override;  // "lua_yield" in 5.1, "lua_yieldk" in 5.2
    {$ENDREGION}
    {$REGION 'ILuaAuxInterchange'}
      function luaL_loadbufferx(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer; overload; override;
      function luaL_loadfilex(L: PLuaState; filename, mode: PAnsiChar): Integer; overload; override;
    {$ENDREGION}
    {$REGION 'ILua51Aux'}
      function luaL_findtable(L: PLuaState; idx: Integer; const fname: PAnsiChar; szhint: Integer): PAnsiChar; overload; virtual; abstract;
      procedure luaL_openlib(L: PLuaState; const libname: PAnsiChar; const lr: PLuaLReg; nup: Integer); overload; virtual; abstract;
      function luaL_prepbuffer(B: PLuaLBuffer): PAnsiChar; overload; virtual; abstract;
      function luaL_typerror(L: PLuaState; narg: Integer; const tname: PAnsiChar): Integer; overload; virtual; abstract;
    {$ENDREGION}
  end;

  {
    TLua51Common
      - The COMMON PARENT ANCESTOR for Lua 5.1 classes.
      - Inherited by TLua51Static, TLua51Dynamic, TLua51Embedded
  }
  TLua51Common = class(TLua51Base, ILua51LibLocal, ILua51AuxLocal)
  public
    {$REGION 'ILua51AuxLocal'}
      function luaL_findtable(idx: Integer; const fname: PAnsiChar; szhint: Integer): PAnsiChar; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_loadbuffer(const buff: PAnsiChar; sz: Cardinal; const name: PAnsiChar): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_loadfile(const filename: PAnsiChar): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure luaL_openlib(const libname: PAnsiChar; const lr: PLuaLReg; nup: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure luaL_register(const libname: PAnsiChar; const lr: PLuaLReg); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function luaL_typerror(narg: Integer; const tname: PAnsiChar): Integer; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
    {$ENDREGION}
  end;

implementation

{$REGION 'Lua 5.1 Base Type'}
  { TLua51Base }

  function TLua51Base.luaL_loadbufferx(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer;
  begin
    Result := luaL_loadbuffer(L, buff, sz, name);
  end;

  function TLua51Base.luaL_loadfilex(L: PLuaState; filename, mode: PAnsiChar): Integer;
  begin
    Result := luaL_loadfile(L, filename);
  end;

  procedure TLua51Base.lua_callk(L: PLuaState; nargs, nresults, ctx: Integer; k: TLuaDelphiFunction);
  begin
    lua_call(L, nargs, nresults);
  end;

  function TLua51Base.lua_compare(L: PLuaState; idx1, idx2, op: Integer): LongBool;
  begin
    case op of
      LUA_OPEQ: Result := lua_equal(L, idx1, idx2);
      LUA_OPLT: Result := lua_lessthan(L, idx1, idx2);
      LUA_OPLE: Result := (lua_equal(L, idx1, idx2) or lua_lessthan(L, idx1, idx2));
    else
      Result := False;
    end;
  end;

  procedure TLua51Base.lua_getglobal(L: PLuaState; name: PAnsiChar);
  begin
    lua_getfield(L, LUA_GLOBALSINDEX, name);
  end;

  procedure TLua51Base.lua_getuservalue(L: PLuaState; idx: Integer);
  begin
    lua_getfenv(L, idx);
  end;

  function TLua51Base.lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; Source, Mode: PAnsiChar): Integer;
  begin
    Result := lua_load(L, reader, dt, source);
  end;

  function TLua51Base.lua_pcallk(L: PLuaState; nargs, nresults, arrfunc, ctx: Integer; k: TLuaDelphiFunction): Integer;
  begin
    Result := lua_pcall(L, nargs, nresults, arrfunc);
  end;

  function TLua51Base.lua_rawlen(L: PLuaState; idx: Integer): Cardinal;
  begin
    Result := lua_objlen(L, idx);
  end;

  procedure TLua51Base.lua_setglobal(L: PLuaState; name: PAnsiChar);
  begin
    lua_setfield(L, LUA_GLOBALSINDEX, name);
  end;

  procedure TLua51Base.lua_setuservalue(L: PLuaState; idx: Integer);
  begin
    lua_setfenv(L, idx);
  end;

  function TLua51Base.lua_tointegerx(L: PLuaState; idx: Integer; isnum: PInteger): Integer;
  begin
    Result := lua_tointeger(L, idx);
  end;

  function TLua51Base.lua_tonumberx(L: PLuaState; idx: Integer; isnum: PInteger): Double;
  begin
    Result := lua_tonumber(L, idx);
  end;

  function TLua51Base.lua_yieldk(L: PLuaState; nresults, ctx: Integer; k: TLuaDelphiFunction): Integer;
  begin
    Result := lua_yield(L, nresults);
  end;
{$ENDREGION}

{$REGION 'Lua 5.1 Common Type'}
  { TLua51Common }

  function TLua51Common.luaL_findtable(idx: Integer; const fname: PAnsiChar; szhint: Integer): PAnsiChar;
  begin
    Result := luaL_findtable(FLuaState, idx, fname, szhint);
  end;

  function TLua51Common.luaL_loadbuffer(const buff: PAnsiChar; sz: Cardinal; const name: PAnsiChar): Integer;
  begin
    Result := luaL_loadbuffer(FLuaState, buff, sz, name);
  end;

  function TLua51Common.luaL_loadfile(const filename: PAnsiChar): Integer;
  begin
    Result := luaL_loadfile(FLuaState, filename);
  end;

  procedure TLua51Common.luaL_openlib(const libname: PAnsiChar; const lr: PLuaLReg; nup: Integer);
  begin
    luaL_openlib(FLuaState, libname, lr, nup);
  end;

  procedure TLua51Common.luaL_register(const libname: PAnsiChar; const lr: PLuaLReg);
  begin
    luaL_register(FLuaState, libname, lr);
  end;

  function TLua51Common.luaL_typerror(narg: Integer; const tname: PAnsiChar): Integer;
  begin
    Result := luaL_typerror(FLuaState, narg, tname);
  end;
{$ENDREGION}

end.
