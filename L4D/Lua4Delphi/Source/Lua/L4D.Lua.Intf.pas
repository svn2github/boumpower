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
  Unit: L4D.Lua.Intf.pas
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
unit L4D.Lua.Intf;

interface

{$I Lua4Delphi.inc}

const
{$REGION 'Lua Constants'}
    // From the Lua Headers...
    LUA_IDSIZE = 60; // gives the maximum size for the description of the source of a function in debug information. CHANGE it if you want a different size.
    LUAL_BUFFERSIZE = 1024; // the buffer size used by the lauxlib buffer system.
    LUA_SIGNATURE = #27'Lua'; // mark for precompiled code (`<esc>Lua')
    LUA_MULTRET = -1; // option for multiple returns in `lua_pcall' and `lua_call'
    // Basic Lua Type IDs
    LUA_TNONE = -1;
    LUA_TNIL = 0;
    LUA_TBOOLEAN = 1;
    LUA_TLIGHTUSERDATA = 2;
    LUA_TNUMBER = 3;
    LUA_TSTRING = 4;
    LUA_TTABLE = 5;
    LUA_TFUNCTION = 6;
    LUA_TUSERDATA	= 7;
    LUA_TTHREAD = 8;
    // Pseduo Indices
    LUA_REGISTRYINDEX = -10000;
    LUA_ENVIRONINDEX = -10001;
    LUA_GLOBALSINDEX = -10002;
    // Thread State
    LUA_OK = 0;
    LUA_YIELD_ = 1;     // Note: the underscore suffix is needed in Pascal
    LUA_ERRRUN = 2;
    LUA_ERRSYNTAX = 3;
    LUA_ERRMEM = 4;
    //
    LUA_MINSTACK = 20; // Minimum Lua Stack available to a C (Delphi) Function
    // Garbage Collection State IDs
    LUA_GCSTOP = 0;
    LUA_GCRESTART = 1;
    LUA_GCCOLLECT = 2;
    LUA_GCCOUNT = 3;
    LUA_GCCOUNTB = 4;
    LUA_GCSTEP = 5;
    LUA_GCSETPAUSE = 6;
    LUA_GCSETSTEPMUL = 7;
    // Comparison functions
    LUA_OPEQ  = 0;
    LUA_OPLT  = 1;
    LUA_OPLE  = 2;
    // Event codes
    LUA_HOOKCALL = 0;
    LUA_HOOKRET = 1;
    LUA_HOOKLINE = 2;
    LUA_HOOKCOUNT = 3;
    LUA_HOOKTAILRET = 4;
    // Event masks
    LUA_MASKCALL = 1 shl LUA_HOOKCALL;
    LUA_MASKRET = 1 shl LUA_HOOKRET;
    LUA_MASKLINE = 1 shl LUA_HOOKLINE;
    LUA_MASKCOUNT = 1 shl LUA_HOOKCOUNT;
    // Key to file-handle type
    LUA_FILEHANDLE = 'FILE*';
    // Lua Library Names
    LUA_COLIBNAME = 'coroutine';
    LUA_TABLIBNAME = 'table';
    LUA_IOLIBNAME = 'io';
    LUA_OSLIBNAME = 'os';
    LUA_STRLIBNAME = 'string';
    LUA_MATHLIBNAME = 'math';
    LUA_DBLIBNAME = 'debug';
    LUA_LOADLIBNAME = 'package';
    // Pre-defined references
    LUA_NOREF = -2;
    LUA_REFNIL = -1;
{$ENDREGION}

type
{$REGION 'Lua Types'}
    { Pointer Types }
    PLuaState = Pointer;
    PLuaDebug = ^TLuaDebug;
    PluaLReg = ^TluaLReg;
    PLuaLBuffer = ^TLuaLBuffer;

    { Standard Typedefs }
    TLuaInteger = type Integer;
    TLuaNumber = type Double;
    PLuaNumber = ^TLuaNumber;

    { TLuaDebug }
    TLuaDebug = packed record
      event: Integer;
      name: PAnsiChar;      (* (n) *)
      namewhat: PAnsiChar;  (* (n) `global', `local', `field', `method' *)
      what: PAnsiChar;      (* (S) `Lua', `C', `main', `tail' *)
      source: PAnsiChar;    (* (S) *)
      currentline: Integer; (* (l) *)
      nups: Integer;        (* (u) number of upvalues *)
      linedefined: Integer; (* (S) *)
      short_src: array [0..LUA_IDSIZE-1] of Char; (* (S) *)
      (* private part *)
      i_ci: Integer;        (* active function *)
    end;

    { Method Types }
    TLuaDelphiFunction = function(ALuaState: PLuaState) : Integer; cdecl;
    TLuaAllocFunction = function (AUserData, APtr: Pointer; AOSize, ANSize: Cardinal): Pointer; cdecl;
    TLuaReaderFunction = function (ALuaState: PLuaState; AUserData: Pointer; ASize: PCardinal): PAnsiChar; cdecl;
    TLuaWriterFunction = function (ALuaState: PLuaState; const APtr: Pointer; ASize: Cardinal; AUserData: Pointer): Integer; cdecl;
    TLuaHookFunction = procedure (L: PluaState; ar: PLuaDebug); cdecl;

    { TluaLReg }
    TluaLReg = packed record
      name: PAnsiChar;
      func: TLuaDelphiFunction;
    end;

    { TLuaLBuffer }
    TLuaLBuffer = packed record
      p: PAnsiChar;   (* current position in buffer *)
      lvl: Integer;   (* number of strings in the stack (level) *)
      L: PluaState;
      buffer: Array [0..LUAL_BUFFERSIZE-1] of Char;
    end;
{$ENDREGION}

  {
    ILuaLibCommonMacros
      - Provides a common interface for Lua Macros (5.1 and 5.2)
  }
  {$REGION 'Lua Common Macros Interface'}
    ILuaLibCommonMacros = interface
    ['{834959B0-6150-480F-BC3C-092ACEBF396C}']
      function lua_isboolean(L: PLuaState; idx: Integer): LongBool;
      function lua_isfunction(L: PLuaState; idx: Integer): LongBool;
      function lua_islightuserdata(L: PLuaState; idx: Integer): LongBool;
      function lua_isnil(L: PLuaState; idx: Integer): LongBool;
      function lua_isnone(L: PLuaState; idx: Integer): LongBool;
      function lua_isnoneornil(L: PLuaState; idx: Integer): LongBool;
      function lua_istable(L: PLuaState; idx: Integer): LongBool;
      function lua_isthread(L: PLuaState; idx: Integer): LongBool;
      procedure lua_newtable(L: PLuaState);
      procedure lua_pop(L: PLuaState; n: Integer);
      procedure lua_pushcfunction(L: PLuaState; f: TLuaDelphiFunction);
      function lua_pushliteral(L: PLuaState; s: PAnsiChar): PAnsiChar;
      function lua_pushlstring(L: PLuaState; const s: PAnsiChar; ls: Cardinal): PAnsiChar;
      procedure lua_register(L: PLuaState; name: PAnsiChar; f: TLuaDelphiFunction);
      function lua_tostring(L: PLuaState; idx: Integer): PAnsiChar;
    end;
  {$ENDREGION}

  {
    ILuaLibCommonMacrosLocal
      - Provides a common interface for Lua Macros (5.1 and 5.2) (LOCALIZED)
  }
  {$REGION 'Lua Common Macros Localized Interface'}
    ILuaLibCommonMacrosLocal = interface(ILuaLibCommonMacros)
    ['{DE490F66-1489-4A30-8A83-29591E84E5BA}']
      function lua_isboolean(idx: Integer): LongBool;
      function lua_isfunction(idx: Integer): LongBool;
      function lua_islightuserdata(idx: Integer): LongBool;
      function lua_isnil(idx: Integer): LongBool;
      function lua_isnone(idx: Integer): LongBool;
      function lua_isnoneornil(idx: Integer): LongBool;
      function lua_istable(idx: Integer): LongBool;
      function lua_isthread(idx: Integer): LongBool;
      procedure lua_newtable;
      procedure lua_pop(n: Integer);
      procedure lua_pushcfunction(f: TLuaDelphiFunction);
      function lua_pushliteral(s: PAnsiChar): PAnsiChar;
      function lua_pushlstring(const s: PAnsiChar; ls: Cardinal): PAnsiChar;
      procedure lua_register(name: PAnsiChar; f: TLuaDelphiFunction);
      function lua_tostring(idx: Integer): PAnsiChar;
    end;
  {$ENDREGION}

  {
    ILuaAuxCommonMacros
      - Provides a common interface for Lua Auxiliary Macros (5.1 and 5.2)
  }
  {$REGION 'Lua Auxiliary Macros Interface'}
    ILuaAuxCommonMacros = interface
    ['{A66E5F38-D89F-47C4-96ED-AEF1CEDA7B2D}']
      function luaL_checkint(L: PLuaState; narg: Integer): Integer; overload;
      function luaL_checklong(L: PLuaState; narg: Cardinal): Cardinal; overload;
      function luaL_checkstring(L: PLuaState; narg: Integer): PAnsiChar; overload;
      function luaL_dofile(L: PLuaState; filename: PAnsiChar): Integer; overload;
      function luaL_dostring(L: PLuaState; str: PAnsiChar): Integer; overload;
      procedure luaL_getmetatable(L: PLuaState; tname: PAnsiChar); overload;
      function luaL_optint(L: PLuaState; narg, d: Integer): Integer; overload;
      function luaL_optlong(L: PLuaState; narg: Integer; d: Cardinal): Cardinal; overload;
      function luaL_optstring(L: PLuaState; narg: Integer; d: PAnsiChar): PAnsiChar; overload;
      function luaL_typename(L: PLuaState; index: Integer): PAnsiChar; overload;
    end;
  {$ENDREGION}

  {
    ILuaAuxCommonMacrosLocal
      - Provides a common interface for Lua Auxiliary Macros (5.1 and 5.2) (LOCALIZED)
  }
  {$REGION 'Lua Auxiliary Macros Localized Interface'}
    ILuaAuxCommonMacrosLocal = interface
    ['{3764DEA4-AAC3-4FE3-9D59-8B2A0B0448D8}']
      function luaL_checkint(narg: Integer): Integer; overload;
      function luaL_checklong(narg: Cardinal): Cardinal; overload;
      function luaL_checkstring(narg: Integer): PAnsiChar; overload;
      function luaL_dofile(filename: PAnsiChar): Integer; overload;
      function luaL_dostring(str: PAnsiChar): Integer; overload;
      procedure luaL_getmetatable(tname: PAnsiChar); overload;
      function luaL_optint(narg, d: Integer): Integer; overload;
      function luaL_optlong(narg: Integer; d: Cardinal): Cardinal; overload;
      function luaL_optstring(narg: Integer; d: PAnsiChar): PAnsiChar; overload;
      function luaL_typename(index: Integer): PAnsiChar; overload;
    end;
  {$ENDREGION}

  {
    ILuaLibInterchange
      - Provides a common interface for normally version-specific Lua methods
  }
  {$REGION 'Lua Lib Interchange Interface'}
    ILuaLibInterchange = interface
    ['{428860B0-0569-4A30-AF36-EB25734E1CD7}']
      procedure lua_call(L: PLuaState; nargs, nresults: Integer);                                 // External "lua_call" in 5.1, External "lua_callk" in 5.2
      procedure lua_callk(L: PLuaState; nargs, nresults, ctx: Integer; k: TLuaDelphiFunction);    // External "lua_call" in 5.1, External "lua_callk" in 5.2
      function lua_compare(L: PLuaState; idx1, idx2, op: Integer): LongBool;                      // "lua_equal" OR "lua_lessthan" in 5.1, External in 5.2
      function lua_cpcall(L: PLuaState; func: TLuaDelphiFunction; ud: Pointer): Integer;          // External in 5.1, "lua_pcall" in 5.2
      function lua_equal(L: PLuaState; idx1, idx2: Integer): LongBool;                            // External in 5.1, "lua_compare" in 5.2
      procedure lua_getfenv(L: PLuaState; idx: Integer);                                          // External in 5.1, "lua_getuservalue" in 5.2
      procedure lua_getglobal(L: PLuaState; name: PAnsiChar);                                     // "lua_getfield" in 5.1, External in 5.2
      procedure lua_getuservalue(L: PLuaState; idx: Integer);                                     // "lua_getfenv" in 5.1, External in 5.2
      function lua_lessthan(L: PLuaState; idx1, idx2: Integer): LongBool;                         // External in 5.1, "lua_compare" in 5.2
      function lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer; overload; // Extra parameter in 5.2 (mode)... 5.1 to pass "nil" to operate normally
      function lua_load(L: PLuaState; reader: TLuaReaderFunction; dt: Pointer; Source, Mode: PAnsiChar): Integer; overload;    // Extra parameter in 5.2 (mode)... 5.1 to pass "nil" to operate normally
      function lua_objlen(L: PLuaState; idx: Integer): Cardinal;                                  // External in 5.1, "lua_rawlen" in 5.2
      function lua_pcall(L: PLuaState; nargs, nresults, errfunc: Integer): Integer;               // External in 5.1, "lua_pcallk" in 5.2
      function lua_pcallk(L: PLuaState; nargs, nresults, arrfunc, ctx: Integer; k: TLuaDelphiFunction): Integer;  // External in 5.1, "lua_pcallk" in 5.2
      function lua_rawlen(L: PLuaState; idx: Integer): Cardinal;                                  // "lua_objlen" in 5.1, External in 5.2
      function lua_resume(L: PLuaState; narg: Integer): Integer;  // 5.1 version                  // Commonized Externals
      function lua_setfenv(L: PLuaState; idx: Integer): LongBool;                                 // External in 5.1, "lua_setuservalue" in 5.2
      procedure lua_setglobal(L: PLuaState; name: PAnsiChar);                                     // "lua_setfield" in 5.1, External in 5.2
      function lua_setmetatable(L: PLuaState; objindex: Integer): LongBool;                       // Function in 5.1, Procedure in 5.2... using a Function for both (saves hardship)
      procedure lua_setuservalue(L: PLuaState; idx: Integer);                                     // "lua_getfenv" & "lua_setfenv" in 5.1, External in 5.2
      function lua_tointeger(L: PLuaState; idx: Integer): Integer;                                // In 5.1
      function lua_tointegerx(L: PLuaState; idx: Integer; isnum: PInteger): Integer;              // In 5.2
      function lua_tonumber(L: PLuaState; idx: Integer): Double;                                  // In 5.1
      function lua_tonumberx(L: PLuaState; idx: Integer; isnum: PInteger): Double;                // In 5.2
      function lua_yield(L: PLuaState; nresults: Integer): Integer;                               // "lua_yield" in 5.1, "lua_yieldk" in 5.2
      function lua_yieldk(L: PLuaState; nresults, ctx: Integer; k: TLuaDelphiFunction): Integer;  // "lua_yield" in 5.1, "lua_yieldk" in 5.2
    end;
  {$ENDREGION}

  {
    ILuaLibInterchangeLocal
      - Provides a common interface for normally version-specific Lua methods (LOCALIZED)
  }
  {$REGION 'Lua Lib Interchange Localized Interface'}
    ILuaLibInterchangeLocal = interface(ILuaLibInterchange)
    ['{F92CB292-4EF6-4B68-8AE7-5040AD907761}']
      procedure lua_call(nargs, nresults: Integer);
      procedure lua_callk(nargs, nresults, ctx: Integer; k: TLuaDelphiFunction);
      function lua_compare(idx1, idx2, op: Integer): LongBool;
      function lua_cpcall(func: TLuaDelphiFunction; ud: Pointer): Integer;
      function lua_equal(idx1, idx2: Integer): LongBool;
      procedure lua_getfenv(idx: Integer);
      procedure lua_getglobal(name: PAnsiChar);
      procedure lua_getuservalue(idx: Integer);
      function lua_lessthan(idx1, idx2: Integer): LongBool;
      function lua_load(reader: TLuaReaderFunction; dt: Pointer; const chunkname: PAnsiChar): Integer; overload; // 5.1 version
      function lua_load(reader: TLuaReaderFunction; dt: Pointer; Source, Mode: PAnsiChar): Integer; overload; // 5.2 version
      function lua_objlen(idx: Integer): Cardinal;
      function lua_pcall(nargs, nresults, errfunc: Integer): Integer;
      function lua_pcallk(nargs, nresults, arrfunc, ctx: Integer; k: TLuaDelphiFunction): Integer;
      function lua_rawlen(idx: Integer): Cardinal;
      function lua_resume(narg: Integer): Integer;  // 5.1 version
      function lua_setfenv(idx: Integer): LongBool;
      procedure lua_setglobal(name: PAnsiChar);
      function lua_setmetatable(objindex: Integer): LongBool;
      procedure lua_setuservalue(idx: Integer);
      function lua_tointeger(idx: Integer): Integer;
      function lua_tointegerx(idx: Integer; isnum: PInteger): Integer;
      function lua_tonumber(idx: Integer): Double;
      function lua_tonumberx(idx: Integer; isnum: PInteger): Double;
      function lua_yield(nresults: Integer): Integer;
      function lua_yieldk(nresults, ctx: Integer; k: TLuaDelphiFunction): Integer;
    end;
  {$ENDREGION}

  {
    ILuaAuxInterchange
      - Provides a common interface for normally version-specific Lua Aux methods
  }
  {$REGION 'Lua Auxiliary Interchange Interface'}
    ILuaAuxInterchange = interface
    ['{6E7D4871-291F-4563-AA84-B032414A3BCA}']
      function luaL_loadbuffer(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; overload;
      function luaL_loadbufferx(L: PLuaState; buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer; overload;
      function luaL_loadfile(L: PLuaState; filename: PAnsiChar): Integer; overload;
      function luaL_loadfilex(L: PLuaState; filename, mode: PAnsiChar): Integer; overload;
      function luaL_prepbuffer(B: TLuaLBuffer): PAnsiChar; overload;
      function luaL_prepbuffsize(B: TLuaLBuffer; sz: Cardinal): PAnsiChar; overload;
      procedure luaL_register(L: PLuaState; libname: PAnsiChar; lib: PluaLReg); overload;
    end;
  {$ENDREGION}

  {
    ILuaAuxInterchangeLocal
      - Provides a common interface for normally version-specific Lua Aux methods (LOCALIZED)
  }
  {$REGION 'Lua Auxiliary Interchange Localized Interface'}
    ILuaAuxInterchangeLocal = interface(ILuaAuxInterchange)
    ['{94033932-FB59-499C-B14D-515A56130846}']
      function luaL_loadbuffer(buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; overload;
      function luaL_loadbufferx(buff: PAnsiChar; sz: Cardinal; name, mode: PAnsiChar): Integer; overload;
      function luaL_loadfile(filename: PAnsiChar): Integer; overload;
      function luaL_loadfilex(filename, mode: PAnsiChar): Integer; overload;
      procedure luaL_register(libname: PAnsiChar; lib: PluaLReg); overload;
    end;
  {$ENDREGION}

  {
    ILuaLibCommon
      - Provides a common interface for core Lua 5.1 and 5.2 methods
  }
  {$REGION 'Lua Common Lib Interface'}
    ILuaLibCommon = interface
    ['{9734B0E4-9D9E-46FF-83BE-2AE1A2A9E916}']
      function lua_atpanic(L: PLuaState; panicf: TLuaDelphiFunction): TLuaDelphiFunction;
      function lua_checkstack(L: PLuaState; sz: Integer): LongBool;
      procedure lua_close(L: PLuaState);
      procedure lua_concat(L: PLuaState; n: Integer);
      procedure lua_createtable(L: PLuaState; narr, nrec: Integer);
      function lua_dump(L: PLuaState; writer: TLuaWriterFunction; data: Pointer): Integer;
      function lua_error(L: PLuaState): Integer;
      function lua_gc(L: PLuaState; what, data: Integer): Integer;
      function lua_getallocf(L: PLuaState; ud: PPointer): TLuaAllocFunction;
      procedure lua_getfield(L: PLuaState; idx: Integer; k: PAnsiChar);
      function lua_gethook(L: PLuaState): TLuaHookFunction;
      function lua_gethookcount(L: PLuaState): Integer;
      function lua_gethookmask(L: PLuaState): Integer;
      function lua_getinfo(L: PLuaState; const what: PAnsiChar; ar: PLuaDebug): Integer;
      function lua_getlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar;
      function lua_getmetatable(L: PLuaState; objindex: Integer): LongBool;
      function lua_getstack(L: PLuaState; level: Integer; ar: PLuaDebug): Integer;
      procedure lua_gettable(L: PLuaState ; idx: Integer);
      function lua_gettop(L: PLuaState): Integer;
      function lua_getupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar;
      procedure lua_insert(L: PLuaState; idx: Integer);
      function lua_iscfunction(L: PLuaState; idx: Integer): LongBool;
      function lua_isnumber(L: PLuaState; idx: Integer): LongBool;
      function lua_isstring(L: PLuaState; idx: Integer): LongBool;
      function lua_isuserdata(L: PLuaState; idx: Integer): LongBool;
      function lua_newthread(L: PLuaState): PLuaState;
      function lua_newstate(f: TLuaAllocFunction; ud: Pointer): PLuaState;
      function lua_newuserdata(L: PLuaState; sz: Cardinal): Pointer;
      function lua_next(L: PLuaState; idx: Integer): Integer;
      procedure lua_pushboolean(L: PLuaState; b: LongBool);
      procedure lua_pushcclosure(L: PLuaState; fn: TLuaDelphiFunction; n: Integer);
      function lua_pushfstring(L: PLuaState; const fmt: PAnsiChar): PAnsiChar; {varargs;}
      procedure lua_pushinteger(L: PLuaState; n: Integer);
      procedure lua_pushlightuserdata(L: PLuaState; p: Pointer);
      procedure lua_pushnil(L: PLuaState);
      procedure lua_pushnumber(L: PLuaState; n: Double);
      function lua_pushstring(L: PLuaState; const s: PAnsiChar): PAnsiChar;
      function lua_pushthread(L: PLuaState): LongBool;
      procedure lua_pushvalue(L: PLuaState; idx: Integer);
      function lua_pushvfstring(L: PLuaState; const fmt: PAnsiChar; argp: Pointer): PAnsiChar;
      function lua_rawequal(L: PLuaState; idx1, idx2: Integer): LongBool;
      procedure lua_rawget(L: PLuaState; idx: Integer);
      procedure lua_rawgeti(L: PLuaState; idx, n: Integer);
      procedure lua_rawset(L: PLuaState; idx: Integer);
      procedure lua_rawseti(L: PLuaState; idx , n: Integer);
      procedure lua_remove(L: PLuaState; idx: Integer);
      procedure lua_replace(L: PLuaState; idx: Integer);
      procedure lua_setallocf(L: PLuaState; f: TLuaAllocFunction; ud: Pointer);
      procedure lua_setfield(L: PLuaState; idx: Integer; const k: PAnsiChar);
      function lua_sethook(L: PLuaState; func: TLuaHookFunction; mask, count: Integer): Integer;
      function lua_setlocal(L: PLuaState; ar: PLuaDebug; n: Integer): PAnsiChar;
      procedure lua_settable(L: PLuaState; idx: Integer);
      procedure lua_settop(L: PLuaState; idx: Integer);
      function lua_setupvalue(L: PLuaState; funcindex, n: Integer): PAnsiChar;
      function lua_status(L: PLuaState): Integer;
      function lua_toboolean(L: PLuaState; idx: Integer): LongBool;
      function lua_tocfunction(L: PLuaState; idx: Integer): TLuaDelphiFunction;
      function lua_tolstring(L: PLuaState; idx: Integer; len: PCardinal): PAnsiChar;
      function lua_topointer(L: PLuaState; idx: Integer): Pointer;
      function lua_tothread(L: PLuaState; idx: Integer): PLuaState;
      function lua_touserdata(L: PLuaState; idx: Integer): Pointer;
      function lua_type(L: PLuaState; idx: Integer): Integer;
      function lua_typename(L: PLuaState; tp: Integer): PAnsiChar;
      procedure lua_xmove(src, dest: PLuaState; n: Integer);
      function luaopen_base(L: PLuaState): Integer;
      function luaopen_debug(L: PLuaState): Integer;
      function luaopen_io(L: PLuaState): Integer;
      function luaopen_math(L: PLuaState): Integer;
      function luaopen_os(L: PLuaState): Integer;
      function luaopen_package(L: PLuaState): Integer;
      function luaopen_string(L: PLuaState): Integer;
      function luaopen_table(L: PLuaState): Integer;
    end;
  {$ENDREGION}

  {
    ILuaLibCommonLocal
      - A common interface for Lua LOCALIZED
  }
  {$REGION 'Lua Common Lib Localized Interface'}
    ILuaLibCommonLocal = interface(ILuaLibCommon)
    ['{03D3A047-007B-4678-B013-CEA6B8986079}']
      function lua_atpanic(panicf: TLuaDelphiFunction): TLuaDelphiFunction;
      function lua_checkstack(sz: Integer): LongBool;
      procedure lua_close;
      procedure lua_concat(n: Integer);
      procedure lua_createtable(narr, nrec: Integer);
      function lua_dump(writer: TLuaWriterFunction; data: Pointer): Integer;
      function lua_error: Integer;
      function lua_gc(what, data: Integer): Integer;
      function lua_getallocf(ud: PPointer): TLuaAllocFunction;
      procedure lua_getfield(idx: Integer; k: PAnsiChar);
      function lua_gethook: TLuaHookFunction;
      function lua_gethookcount: Integer;
      function lua_gethookmask: Integer;
      function lua_getinfo(const what: PAnsiChar; ar: PLuaDebug): Integer;
      function lua_getlocal(ar: PLuaDebug; n: Integer): PAnsiChar;
      function lua_getmetatable(objindex: Integer): LongBool;
      function lua_getstack(level: Integer; ar: PLuaDebug): Integer;
      procedure lua_gettable( idx: Integer);
      function lua_gettop: Integer;
      function lua_getupvalue(funcindex, n: Integer): PAnsiChar;
      procedure lua_insert(idx: Integer);
      function lua_iscfunction(idx: Integer): LongBool;
      function lua_isnumber(idx: Integer): LongBool;
      function lua_isstring(idx: Integer): LongBool;
      function lua_isuserdata(idx: Integer): LongBool;
      function lua_newthread: PLuaState;
      function lua_newuserdata(sz: Cardinal): Pointer;
      function lua_next(idx: Integer): Integer;
      procedure lua_pushboolean(b: LongBool);
      procedure lua_pushcclosure(fn: TLuaDelphiFunction; n: Integer);
      function lua_pushfstring(const fmt: PAnsiChar): PAnsiChar; {varargs;}
      procedure lua_pushinteger(n: Integer);
      procedure lua_pushlightuserdata(p: Pointer);
      procedure lua_pushnil;
      procedure lua_pushnumber(n: Double);
      function lua_pushstring(const s: PAnsiChar): PAnsiChar;
      function lua_pushthread: LongBool;
      procedure lua_pushvalue(idx: Integer);
      function lua_pushvfstring(const fmt: PAnsiChar; argp: Pointer): PAnsiChar;
      function lua_rawequal(idx1, idx2: Integer): LongBool;
      procedure lua_rawget(idx: Integer);
      procedure lua_rawgeti(idx, n: Integer);
      procedure lua_rawset(idx: Integer);
      procedure lua_rawseti(idx , n: Integer);
      procedure lua_remove(idx: Integer);
      procedure lua_replace(idx: Integer);
      procedure lua_setallocf(f: TLuaAllocFunction; ud: Pointer);
      procedure lua_setfield(idx: Integer; const k: PAnsiChar);
      function lua_sethook(func: TLuaHookFunction; mask, count: Integer): Integer;
      function lua_setlocal(ar: PLuaDebug; n: Integer): PAnsiChar;
      procedure lua_settable(idx: Integer);
      procedure lua_settop(idx: Integer);
      function lua_setupvalue(funcindex, n: Integer): PAnsiChar;
      function lua_status: Integer;
      function lua_toboolean(idx: Integer): LongBool;
      function lua_tocfunction(idx: Integer): TLuaDelphiFunction;
      function lua_tolstring(idx: Integer; len: PCardinal): PAnsiChar;
      function lua_topointer(idx: Integer): Pointer;
      function lua_tothread(idx: Integer): PLuaState;
      function lua_touserdata(idx: Integer): Pointer;
      function lua_type(idx: Integer): Integer;
      function lua_typename(tp: Integer): PAnsiChar;
      procedure lua_xmove(dest: PLuaState; n: Integer);
      function luaopen_base: Integer;
      function luaopen_debug: Integer;
      function luaopen_io: Integer;
      function luaopen_math: Integer;
      function luaopen_os: Integer;
      function luaopen_package: Integer;
      function luaopen_string: Integer;
      function luaopen_table: Integer;
    end;
  {$ENDREGION}

  {
    ILuaAuxCommon
      - Provides a common interface between Lua 5.1 and Lua 5.2 for the Auxiliary Library
  }
  {$REGION 'Lua Auxiliary Common Interface'}
    ILuaAuxCommon = interface
    ['{3B3FA3F4-25D2-4C4C-82FF-A61F9A4BFA53}']
      procedure luaL_addlstring(B: PLuaLBuffer; const s: PAnsiChar; ls: Cardinal);
      procedure luaL_addstring(B: PLuaLBuffer; const s: PAnsiChar);
      procedure luaL_addvalue(B: PLuaLBuffer);
      function luaL_argerror(L: PLuaState; numarg: Integer; const extramsg: PAnsiChar): Integer;
      procedure luaL_buffinit(L: PLuaState; B: PLuaLBuffer);
      function luaL_callmeta(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer;
      procedure luaL_checkany(L: PLuaState; narg: Integer);
      function luaL_checkinteger(L: PLuaState; numArg: Integer): Integer;
      function luaL_checklstring(L: PLuaState; numArg: Integer; ls: PCardinal): PAnsiChar;
      function luaL_checknumber(L: PLuaState; numArg: Integer): Double;
      function luaL_checkoption(L: PLuaState; narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer;
      procedure luaL_checkstack(L: PLuaState; sz: Integer; const msg: PAnsiChar);
      procedure luaL_checktype(L: PLuaState; narg, t: Integer);
      function luaL_checkudata(L: PLuaState; ud: Integer; const tname: PAnsiChar): Pointer;
      function luaL_error(L: PLuaState; const fmt: PAnsiChar): Integer; {varargs;}
      function luaL_getmetafield(L: PLuaState; obj: Integer; const e: PAnsiChar): Integer;
      function luaL_gsub(L: PLuaState; const s, p, r: PAnsiChar): PAnsiChar;
      function luaL_loadstring(L: PLuaState; const s: PAnsiChar): Integer;
      function luaL_newmetatable(L: PLuaState; const tname: PAnsiChar): Integer;
      function luaL_newstate: PLuaState;
      procedure luaL_openlibs(L: PLuaState);
      function luaL_optinteger(L: PLuaState; nArg: Integer; def: Integer): Integer;
      function luaL_optlstring(L: PLuaState; numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar;
      function luaL_optnumber(L: PLuaState; nArg: Integer; def: Double): Double;
      procedure luaL_pushresult(B: PLuaLBuffer);
      function luaL_ref(L: PLuaState; t: Integer): Integer;
      procedure luaL_unref(L: PLuaState; t, ref: Integer);
      procedure luaL_where(L: PLuaState; lvl: Integer);
    end;
  {$ENDREGION}

  {
    ILuaAuxCommonLocal
      - A common interface for Lua LOCALIZED
  }
  {$REGION 'Lua Auxiliary Common Localized Interface'}
    ILuaCommonAuxLocal = interface(ILuaAuxCommon)
      function luaL_argerror(numarg: Integer; const extramsg: PAnsiChar): Integer;
      procedure luaL_buffinit(B: PLuaLBuffer);
      function luaL_callmeta(obj: Integer; const e: PAnsiChar): Integer;
      procedure luaL_checkany(narg: Integer);
      function luaL_checkinteger(numArg: Integer): Integer;
      function luaL_checklstring(numArg: Integer; ls: PCardinal): PAnsiChar;
      function luaL_checknumber(numArg: Integer): Double;
      function luaL_checkoption(narg: Integer; const def: PAnsiChar; const lst: array of PAnsiChar): Integer;
      procedure luaL_checkstack(sz: Integer; const msg: PAnsiChar);
      procedure luaL_checktype(narg, t: Integer);
      function luaL_checkudata(ud: Integer; const tname: PAnsiChar): Pointer;
      function luaL_error(const fmt: PAnsiChar): Integer; {varargs;}
      function luaL_getmetafield(obj: Integer; const e: PAnsiChar): Integer;
      function luaL_gsub(const s, p, r: PAnsiChar): PAnsiChar;
      function luaL_loadstring(const s: PAnsiChar): Integer;
      function luaL_newmetatable(const tname: PAnsiChar): Integer;
      procedure luaL_openlibs;
      function luaL_optinteger(nArg: Integer; def: Integer): Integer;
      function luaL_optnumber(nArg: Integer; def: Double): Double;
      function luaL_optlstring(numArg: Integer; const def: PAnsiChar; ls: PCardinal): PAnsiChar;
      function luaL_ref(t: Integer): Integer;
      procedure luaL_unref(t, ref: Integer);
      procedure luaL_where(lvl: Integer);
    end;
  {$ENDREGION}

  {
    ILua51Lib
      - Interface for Lua 5.1 specific methods
  }
  {$REGION 'Lua 5.1 Interface'}
    ILua51Lib = interface
    ['{30DF6960-F4AF-47ED-AC27-78146F0F419F}']

    end;
  {$ENDREGION}

  {
    ILua51LibLocal
      - Interface for Lua 5.1 specific methods (LOCALIZED)
  }
  {$REGION 'Lua 5.1 Localized Interface'}
    ILua51LibLocal = interface(ILua51Lib)
    ['{690B5B7D-FDEE-4FCC-BE86-5592CAFD765D}']

    end;
  {$ENDREGION}

    {
    ILua51Aux
      - Interface for Lua 5.1 Auxiliary specific methods
  }
  {$REGION 'Lua 5.1 Auxiliary Interface'}
    ILua51Aux = interface
    ['{C05C9794-AD51-4FC4-B01C-97B38E2710EB}']
      function luaL_findtable(L: PLuaState; idx: Integer; const fname: PAnsiChar; szhint: Integer): PAnsiChar;
      procedure luaL_openlib(L: PLuaState; const libname: PAnsiChar; const lr: PLuaLReg; nup: Integer);
      function luaL_typerror(L: PLuaState; narg: Integer; const tname: PAnsiChar): Integer;
    end;
  {$ENDREGION}

  {
    ILua51AuxLocal
      - Interface for Lua 5.1 specific Auxiliary methods (LOCALIZED)
  }
  {$REGION 'Lua 5.1 Auxiliary Interface ((LOCALIZED)'}
    ILua51AuxLocal = interface(ILua51Aux)
    ['{9B1CDACF-BCE0-4A8C-BE4F-53C4276B884F}']
      function luaL_findtable(idx: Integer; const fname: PAnsiChar; szhint: Integer): PAnsiChar;
      procedure luaL_openlib(const libname: PAnsiChar; const lr: PLuaLReg; nup: Integer);
      function luaL_typerror(narg: Integer; const tname: PAnsiChar): Integer;
    end;
  {$ENDREGION}

  {
    ILua52Lib
      - Interface for Lua 5.2 specific methods
  }
  {$REGION 'Lua 5.2 Interface'}
    ILua52Lib = interface
    ['{801F2000-D8E0-4DB5-9392-514DAE48411F}']
      function lua_absindex(L: PLuaState; idx: Integer): Integer;
      function lua_arith(L: PLuaState; op: Integer): Integer;
      procedure lua_copy(L: PLuaState; fromidx, toidx: Integer);
      function lua_getctx(L: PLuaState; ctx: PInteger): Integer;
      procedure lua_len(L: PLuaState; idx: Integer);
      procedure lua_pushunsigned(L: PLuaState; u: Cardinal);
      procedure lua_rawgetp(L: PLuaState; idx: Integer; p: Pointer);
      procedure lua_rawsetp(L: PLuaState; idx: Integer; p: Pointer);
      function lua_resume(L, from: PLuaState; narg: Integer): Integer; // 5.2 version
      function lua_tounsignedx(L: PLuaState; idx: Integer; isnum: PInteger): Cardinal;
      function lua_upvalueid(L: PLuaState; funcidx, n: Integer): Pointer;
      procedure lua_upvaluejoin(L: PLuaState; fidx1, n1, fidx2, n2: Integer);
      function lua_version(L: PLuaState): PInteger;
      function luaopen_bit32(L: PLuaState): Integer;
      function luaopen_coroutine(L: PLuaState): Integer;
    end;
  {$ENDREGION}

  {
    ILua52LibLocal
      - Interface for Lua 5.2 specific methods (LOCALIZED)
  }
  {$REGION 'Lua 5.2 Localized Interface'}
    ILua52LibLocal = interface(ILua52Lib)
    ['{01288353-6B86-4754-B5AA-449C308E32D2}']
      function lua_absindex(idx: Integer): Integer;
      function lua_arith(op: Integer): Integer;
      procedure lua_copy(fromidx, toidx: Integer);
      function lua_getctx(ctx: PInteger): Integer;
      procedure lua_len(idx: Integer);
      procedure lua_pushunsigned(u: Cardinal);
      procedure lua_rawgetp(idx: Integer; p: Pointer);
      procedure lua_rawsetp(idx: Integer; p: Pointer);
      function lua_resume(from: PLuaState; narg: Integer; const UNUSED_PROPERTY: Boolean = True): Integer; // 5.2 version
      function lua_tounsignedx(idx: Integer; isnum: PInteger): Cardinal;
      function lua_upvalueid(funcidx, n: Integer): Pointer;
      procedure lua_upvaluejoin(fidx1, n1, fidx2, n2: Integer);
      function lua_version: PInteger;
      function luaopen_bit32: Integer;
      function luaopen_coroutine: Integer;
    end;
  {$ENDREGION}

  {
    ILua52Lib
      - Interface for Lua 5.2 specific methods
  }
  {$REGION 'Lua 5.2 Auxiliary Interface'}
    ILua52Aux = interface
    ['{1066BF7A-2BDC-4851-946E-0DF000AAB560}']
      function luaL_checkunsigned(L: PLuaState; narg: Integer): Cardinal;
      procedure luaL_checkversion(L: PLuaState);
      function luaL_execresult(L: PLuaState; stat: Integer): Integer;
      function luaL_fileresult(L: PLuaState; stat: Integer; fname: PAnsiChar): Integer;
      function luaL_getsubtable(L: PLuaState; idx: Integer; fname: PAnsiChar): Integer;
      function luaL_len(L: PLuaState; idx: Integer): Integer;
      function luaL_optunsigned(L: PLuaState; narg: Integer; u: Cardinal): Cardinal;
      function luaL_buffinitsize(L: PLuaState; B: PLuaLBuffer; sz: Cardinal): PAnsiChar;
      procedure luaL_requiref(L: PLuaState; modname: PansiChar; openf: TLuaDelphiFunction; glb: Integer);
      procedure luaL_setfuncs(L: PLuaState; lreg: PluaLReg; nup: Integer);
      procedure luaL_setmetatable(L: PluaState; tname: PAnsiChar); // 5.2 version
      function luaL_testudata(L: PLuaState; narg: Integer; tname: PAnsiChar): Pointer;
      procedure luaL_traceback(L, L1: PLuaState; msg: PAnsiChar; level: Integer);
    end;
  {$ENDREGION}

  {
    ILua52LibLocal
      - Interface for Lua 5.2 specific methods (LOCALIZED)
  }
  {$REGION 'Lua 5.2 Auxiliary Localized Interface'}
    ILua52AuxLocal = interface(ILua52Aux)
    ['{12F3D79C-339A-4135-92D6-53E9D81A5441}']
      function luaL_checkunsigned(narg: Integer): Cardinal;
      procedure luaL_checkversion;
      function luaL_execresult(stat: Integer): Integer;
      function luaL_fileresult(stat: Integer; fname: PAnsiChar): Integer;
      function luaL_getsubtable(idx: Integer; fname: PAnsiChar): Integer;
      function luaL_len(idx: Integer): Integer;
      function luaL_optunsigned(narg: Integer; u: Cardinal): Cardinal;
      function luaL_buffinitsize(B: PLuaLBuffer; sz: Cardinal): PAnsiChar;
      function luaL_prepbuffsize(B: PLuaLBuffer; sz: Cardinal): PAnsiChar;
      procedure luaL_pushresultsize(B: PLuaLBuffer; sz: Cardinal);
      procedure luaL_requiref(modname: PansiChar; openf: TLuaDelphiFunction; glb: Integer);
      procedure luaL_setfuncs(lreg: PluaLReg; nup: Integer);
      procedure luaL_setmetatable(tname: PAnsiChar);
      function luaL_testudata(narg: Integer; tname: PAnsiChar): Pointer;
      procedure luaL_traceback(L1: PLuaState; msg: PAnsiChar; level: Integer);
    end;
  {$ENDREGION}

implementation

end.
