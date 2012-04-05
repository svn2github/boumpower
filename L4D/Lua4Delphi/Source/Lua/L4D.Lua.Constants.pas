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
  Unit: L4D.Lua.Constants.pas
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
unit L4D.Lua.Constants;

interface

uses
  L4D.Lua.Common, L4D.Lua.Intf;
const
{$IFDEF MSWINDOWS}
  {$REGION 'Lua Lib Common External Method Names'}
    LUA_lua_atpanic = 'lua_atpanic';
    LUA_lua_call = 'lua_call';
    LUA_lua_checkstack = 'lua_checkstack';
    LUA_lua_close = 'lua_close';
    LUA_lua_concat = 'lua_concat';
    LUA_lua_cpcall = 'lua_cpcall';
    LUA_lua_createtable = 'lua_createtable';
    LUA_lua_dump = 'lua_dump';
    LUA_lua_error = 'lua_error';
    LUA_lua_equal = 'lua_equal';
    LUA_lua_gc = 'lua_gc';
    LUA_lua_getallocf = 'lua_getallocf';
    LUA_lua_getfenv = 'lua_getfenv';
    LUA_lua_getfield = 'lua_getfield';
    LUA_lua_gethook = 'lua_gethook';
    LUA_lua_gethookcount = 'lua_gethookcount';
    LUA_lua_gethookmask = 'lua_gethookmask';
    LUA_lua_getinfo = 'lua_getinfo';
    LUA_lua_getlocal = 'lua_getlocal';
    LUA_lua_getmetatable = 'lua_getmetatable';
    LUA_lua_getstack = 'lua_getstack';
    LUA_lua_gettable = 'lua_gettable';
    LUA_lua_gettop = 'lua_gettop';
    LUA_lua_getupvalue = 'lua_getupvalue';
    LUA_lua_insert = 'lua_insert';
    LUA_lua_iscfunction = 'lua_iscfunction';
    LUA_lua_isnumber = 'lua_isnumber';
    LUA_lua_isstring = 'lua_isstring';
    LUA_lua_isuserdata = 'lua_isuserdata';
    LUA_lua_lessthan = 'lua_lessthan';
    LUA_lua_load = 'lua_load';
    LUA_lua_newthread = 'lua_newthread';
    LUA_lua_newstate = 'lua_newstate';
    LUA_lua_newuserdata = 'lua_newuserdata';
    LUA_lua_next = 'lua_next';
    LUA_lua_objlen = 'lua_objlen';
    LUA_lua_pcall = 'lua_pcall';
    LUA_lua_pushboolean = 'lua_pushboolean';
    LUA_lua_pushcclosure = 'lua_pushcclosure';
    LUA_lua_pushfstring = 'lua_pushfstring';
    LUA_lua_pushinteger = 'lua_pushinteger';
    LUA_lua_pushlightuserdata = 'lua_pushlightuserdata';
    LUA_lua_pushnil = 'lua_pushnil';
    LUA_lua_pushnumber = 'lua_pushnumber';
    LUA_lua_pushstring = 'lua_pushstring';
    LUA_lua_pushthread = 'lua_pushthread';
    LUA_lua_pushvalue = 'lua_pushvalue';
    LUA_lua_pushvfstring = 'lua_pushvfstring';
    LUA_lua_rawequal = 'lua_rawequal';
    LUA_lua_rawget = 'lua_rawget';
    LUA_lua_rawgeti = 'lua_rawgeti';
    LUA_lua_rawset = 'lua_rawset';
    LUA_lua_rawseti = 'lua_rawseti';
    LUA_lua_remove = 'lua_remove';
    LUA_lua_replace = 'lua_replace';
    LUA_lua_resume  = 'lua_resume';
    LUA_lua_setallocf = 'lua_setallocf';
    LUA_lua_setfenv = 'lua_setfenv';
    LUA_lua_setfield = 'lua_setfield';
    LUA_lua_sethook = 'lua_sethook';
    LUA_lua_setlocal = 'lua_setlocal';
    LUA_lua_setmetatable = 'lua_setmetatable';
    LUA_lua_settable = 'lua_settable';
    LUA_lua_settop = 'lua_settop';
    LUA_lua_setupvalue = 'lua_setupvalue';
    LUA_lua_status = 'lua_status';
    LUA_lua_toboolean = 'lua_toboolean';
    LUA_lua_tocfunction = 'lua_tocfunction';
    LUA_lua_tointeger = 'lua_tointeger';
    LUA_lua_tolstring = 'lua_tolstring';
    LUA_lua_tonumber = 'lua_tonumber';
    LUA_lua_topointer = 'lua_topointer';
    LUA_lua_tothread = 'lua_tothread';
    LUA_lua_touserdata = 'lua_touserdata';
    LUA_lua_type = 'lua_type';
    LUA_lua_typename = 'lua_typename';
    LUA_lua_xmove = 'lua_xmove';
    LUA_lua_yield = 'lua_yield';
    LUA_luaopen_base = 'luaopen_base';
    LUA_luaopen_debug = 'luaopen_debug';
    LUA_luaopen_io = 'luaopen_io';
    LUA_luaopen_math = 'luaopen_math';
    LUA_luaopen_os = 'luaopen_os';
    LUA_luaopen_package = 'luaopen_package';
    LUA_luaopen_string = 'luaopen_string';
    LUA_luaopen_table = 'luaopen_table';
  {$ENDREGION}
  {$REGION 'Lua Auxiliary Common External Method Names'}
    LUA_luaL_addlstring = 'luaL_addlstring';
    LUA_luaL_addstring = 'luaL_addstring';
    LUA_luaL_addvalue = 'luaL_addvalue';
    LUA_luaL_argerror = 'luaL_argerror';
    LUA_luaL_buffinit = 'luaL_buffinit';
    LUA_luaL_callmeta = 'luaL_callmeta';
    LUA_luaL_checkany = 'luaL_checkany';
    LUA_luaL_checkinteger = 'luaL_checkinteger';
    LUA_luaL_checklstring = 'luaL_checklstring';
    LUA_luaL_checknumber = 'luaL_checknumber';
    LUA_luaL_checkoption = 'luaL_checkoption';
    LUA_luaL_checkstack = 'luaL_checkstack';
    LUA_luaL_checktype = 'luaL_checktype';
    LUA_luaL_checkudata = 'luaL_checkudata';
    LUA_luaL_error = 'luaL_error';
    LUA_luaL_getmetafield = 'luaL_getmetafield';
    LUA_luaL_gsub = 'luaL_gsub';
    LUA_luaL_loadbuffer = 'luaL_loadbuffer';
    LUA_luaL_loadfile = 'luaL_loadfile';
    LUA_luaL_loadstring = 'luaL_loadstring';
    LUA_luaL_newmetatable = 'luaL_newmetatable';
    LUA_luaL_newstate = 'luaL_newstate';
    LUA_luaL_openlib = 'luaL_openlib';
    LUA_luaL_openlibs = 'luaL_openlibs';
    LUA_luaL_optinteger = 'luaL_optinteger';
    LUA_luaL_optlstring = 'luaL_optlstring';
    LUA_luaL_optnumber = 'luaL_optnumber';
    LUA_luaL_prepbuffer = 'luaL_prepbuffer';
    LUA_luaL_pushresult = 'luaL_pushresult';
    LUA_luaL_ref = 'luaL_ref';
    LUA_luaL_register = 'luaL_register';
    LUA_luaL_unref = 'luaL_unref';
    LUA_luaL_where = 'luaL_where';
  {$ENDREGION}
{$ELSE}
  {$REGION 'Lua Lib Common External Method Names'}
    LUA_lua_atpanic = '_lua_atpanic';
    LUA_lua_call = '_lua_call';
    LUA_lua_checkstack = '_lua_checkstack';
    LUA_lua_close = '_lua_close';
    LUA_lua_concat = '_lua_concat';
    LUA_lua_cpcall = '_lua_cpcall';
    LUA_lua_createtable = '_lua_createtable';
    LUA_lua_dump = '_lua_dump';
    LUA_lua_equal = '_lua_equal';
    LUA_lua_error = '_lua_error';
    LUA_lua_gc = '_lua_gc';
    LUA_lua_getallocf = '_lua_getallocf';
    LUA_lua_getfenv = '_lua_getfenv';
    LUA_lua_getfield = '_lua_getfield';
    LUA_lua_gethook = '_lua_gethook';
    LUA_lua_gethookcount = '_lua_gethookcount';
    LUA_lua_gethookmask = '_lua_gethookmask';
    LUA_lua_getinfo = '_lua_getinfo';
    LUA_lua_getlocal = '_lua_getlocal';
    LUA_lua_getmetatable = '_lua_getmetatable';
    LUA_lua_getstack = '_lua_getstack';
    LUA_lua_gettable = '_lua_gettable';
    LUA_lua_gettop = '_lua_gettop';
    LUA_lua_getupvalue = '_lua_getupvalue';
    LUA_lua_insert = '_lua_insert';
    LUA_lua_iscfunction = '_lua_iscfunction';
    LUA_lua_isnumber = '_lua_isnumber';
    LUA_lua_isstring = '_lua_isstring';
    LUA_lua_isuserdata = '_lua_isuserdata';
    LUA_lua_lessthan = '_lua_lessthan';
    LUA_lua_load = '_lua_load';
    LUA_lua_newthread = '_lua_newthread';
    LUA_lua_newstate = '_lua_newstate';
    LUA_lua_newuserdata = '_lua_newuserdata';
    LUA_lua_next = '_lua_next';
    LUA_lua_objlen = '_lua_objlen';
    LUA_lua_pcall = '_lua_pcall';
    LUA_lua_pushboolean = '_lua_pushboolean';
    LUA_lua_pushcclosure = '_lua_pushcclosure';
    LUA_lua_pushcclosure = '_lua_pushcclosure';
    LUA_lua_pushfstring = '_lua_pushfstring';
    LUA_lua_pushinteger = '_lua_pushinteger';
    LUA_lua_pushlightuserdata = '_lua_pushlightuserdata';
    LUA_lua_pushnil = '_lua_pushnil';
    LUA_lua_pushnumber = '_lua_pushnumber';
    LUA_lua_pushstring = 'lua_pushstring';
    LUA_lua_pushthread = '_lua_pushthread';
    LUA_lua_pushvalue = '_lua_pushvalue';
    LUA_lua_pushvfstring = '_lua_pushvfstring';
    LUA_lua_rawequal = '_lua_rawequal';
    LUA_lua_rawget = '_lua_rawget';
    LUA_lua_rawgeti = '_lua_rawgeti';
    LUA_lua_rawset = '_lua_rawset';
    LUA_lua_rawseti = '_lua_rawseti';
    LUA_lua_remove = '_lua_remove';
    LUA_lua_replace = '_lua_replace';
    LUA_lua_resume  = '_lua_resume';
    LUA_lua_setallocf = '_lua_setallocf';
    LUA_lua_setfenv = '_lua_setfenv';
    LUA_lua_setfield = '_lua_setfield';
    LUA_lua_sethook = '_lua_sethook';
    LUA_lua_setlocal = '_lua_setlocal';
    LUA_lua_setmetatable = '_lua_setmetatable';
    LUA_lua_settable = '_lua_settable';
    LUA_lua_settop = '_lua_settop';
    LUA_lua_setupvalue = '_lua_setupvalue';
    LUA_lua_status = '_lua_status';
    LUA_lua_toboolean = '_lua_toboolean';
    LUA_lua_tocfunction = '_lua_tocfunction';
    LUA_lua_tointeger = '_lua_tointeger';
    LUA_lua_tolstring = '_lua_tolstring';
    LUA_lua_tonumber = '_lua_tonumber';
    LUA_lua_topointer = '_lua_topointer';
    LUA_lua_tothread = '_lua_tothread';
    LUA_lua_touserdata = '_lua_touserdata';
    LUA_lua_type = '_lua_type';
    LUA_lua_typename = '_lua_typename';
    LUA_lua_xmove = '_lua_xmove';
    LUA_lua_yield = '_lua_yield';
    LUA_luaopen_base = '_luaopen_base';
    LUA_luaopen_debug = '_luaopen_debug';
    LUA_luaopen_io = '_luaopen_io';
    LUA_luaopen_math = '_luaopen_math';
    LUA_luaopen_os = '_luaopen_os';
    LUA_luaopen_package = '_luaopen_package';
    LUA_luaopen_string = '_luaopen_string';
    LUA_luaopen_table = '_luaopen_table';
  {$ENDREGION}
  {$REGION 'Lua Auxiliary Common External Method Names'}
    LUA_luaL_addlstring = '_luaL_addlstring';
    LUA_luaL_addstring = '_luaL_addstring';
    LUA_luaL_addvalue = '_luaL_addvalue';
    LUA_luaL_argerror = '_luaL_argerror';
    LUA_luaL_buffinit = '_luaL_buffinit';
    LUA_luaL_callmeta = '_luaL_callmeta';
    LUA_luaL_checkany = '_luaL_checkany';
    LUA_luaL_checkinteger = '_luaL_checkinteger';
    LUA_luaL_checklstring = '_luaL_checklstring';
    LUA_luaL_checknumber = '_luaL_checknumber';
    LUA_luaL_checkoption = '_luaL_checkoption';
    LUA_luaL_checkstack = '_luaL_checkstack';
    LUA_luaL_checktype = '_luaL_checktype';
    LUA_luaL_checkudata = '_luaL_checkudata';
    LUA_luaL_error = '_luaL_error';
    LUA_luaL_getmetafield = '_luaL_getmetafield';
    LUA_luaL_gsub = '_luaL_gsub';
    LUA_luaL_loadbuffer = '_luaL_loadbuffer';
    LUA_luaL_loadfile = '_luaL_loadfile';
    LUA_luaL_loadstring = '_luaL_loadstring';
    LUA_luaL_newmetatable = '_luaL_newmetatable';
    LUA_luaL_newstate = '_luaL_newstate';
    LUA_luaL_openlib = '_luaL_openlib';
    LUA_luaL_openlibs = '_luaL_openlibs';
    LUA_luaL_optinteger = '_luaL_optinteger';
    LUA_luaL_optlstring = '_luaL_optlstring';
    LUA_luaL_optnumber = '_luaL_optnumber';
    LUA_luaL_prepbuffer = '_luaL_prepbuffer';
    LUA_luaL_pushresult = '_luaL_pushresult';
    LUA_luaL_ref = '_luaL_ref';
    LUA_luaL_register = '_luaL_register';
    LUA_luaL_unref = '_luaL_unref';
    LUA_luaL_where = '_luaL_where';
  {$ENDREGION}
{$ENDIF}

implementation

end.
