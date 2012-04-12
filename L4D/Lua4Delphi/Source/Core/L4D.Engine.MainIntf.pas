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
  Unit: L4D.Engine.MainIntf.pas
  Released: 22nd February 2012

  Changelog:
    13th March 2012:
      - Added "IL4D<interface>Internal" as appropriate to separate inner workings from public view
      - Added access to IL4DEngine interface to all Stack Managers!
    12th March 2012:
      - Refactored a lot related to Stacks, Values and Tables
      - Removed unnecessary methods related to Methods (such as Offset getter/setter).
      - Other changes (too many to enumerate)
    22nd February 2012:
      - Created
}
unit L4D.Engine.MainIntf;

interface

{$I Lua4Delphi.inc}

uses
  {$IFDEF DELPHIXE2}
    System.Classes,
  {$ELSE}
    Classes,
  {$ENDIF}
  L4D.Lua.Intf, L4D.Lua.Common;

type
  { Lua4Delphi-specific Enums }
  TL4DStackValueType = (svtNone = -1, svtNil = 0, svtBoolean, svtLightUserData, svtNumber, svtString, svtTable, svtFunction, svtUserdata, svtThread);

const
  // Lua4Delphi-specific Type Constants...
  LUA_TYPES: Array[0..9] of TL4DStackValueType = (svtNone, svtNil, svtBoolean, svtLightUserData, svtNumber, svtString, svtTable, svtFunction, svtUserdata, svtThread);
  LUA_TYPE_NAMES: Array[TL4DStackValueType] of String = ('None', 'Nil', 'Boolean', 'Light Userdata', 'Number', 'String', 'Table', 'Function', 'Userdata', 'Thread');

type
  { Forward Declarations }
  IL4DEngineMember = interface;
  IL4DMethod = interface;
  IL4DMethodObject = interface;
  IL4DStack = interface;
  IL4DStackIndexed = interface;
  IL4DStackKeyed = interface;
  IL4DStackCombo = interface;
  IL4DStackValue = interface;
  IL4DTableValue = interface;
  IL4DTable = interface;
  IL4DMethodResultStackValue = interface;
  IL4DMethodResultStack = interface;
  IL4DMethodStackValue = interface;
  IL4DMethodStack = interface;
  IL4DGlobalStackValue = interface;
  IL4DGlobalStack = interface;
  IL4DEngineManager = interface;
  IL4DEngine = interface;

  { Method Types }
  TL4DDelphiFunction = procedure(var ALuaStack: IL4DMethodStack);
  TL4DDelphiObjectFunction = procedure(var ALuaStack: IL4DMethodStack) of object;

  { Internal-use OnEvent Method Types }
  TL4DTableChangeEvent = procedure(const ATable: IL4DTable) of object;

  {
    IL4DEngineMember
  }
  IL4DEngineMember = interface
  ['{13D03502-588B-4904-872C-3B260E33F94F}']
    function GetEngine: IL4DEngine;
    property Engine: IL4DEngine read GetEngine;
  end;

  {
    IL4DMethod
  }
  IL4DMethod = interface
  ['{E8C80E1B-A5D1-480F-B14E-055898A28C10}']

  end;

  {
    IL4DMethodObject
  }
  IL4DMethodObject = interface
  ['{9C3B9942-40B4-49F0-95CC-C49F805971E3}']

  end;

  {
    IL4DStack
  }
  IL4DStack = interface
  ['{B4A38302-6D60-4217-B8A8-792FE4999888}']
    function GetCount: Integer;
    function GetEngine: IL4DEngine;
    property Count: Integer read GetCount;
    property Engine: IL4DEngine read GetEngine;
  end;

  {
    IL4DStackIndexed
  }
  IL4DStackIndexed = interface(IL4DStack)
  ['{66CFB82A-D726-48E7-923B-6C0C9BEFDD30}']
    function NewTable: IL4DTable;
    procedure PushAnsiChar(const AValue: AnsiChar);
    procedure PushAnsiString(const AValue: AnsiString);
    procedure PushBoolean(const AValue: Boolean);
    procedure PushChar(const AValue: Char);
    procedure PushDouble(const AValue: Double);
    procedure PushExtended(const AValue: Extended);
    procedure PushFunction(const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
    procedure PushFunction(const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
    procedure PushFunction(const AValue: TLuaDelphiFunction); overload;  // Standard Lua "C" Function
    procedure PushInteger(const AValue: Integer);
    procedure PushPAnsiChar(const AValue: PAnsiChar);
    procedure PushPChar(const AValue: PChar);
    procedure PushPointer(const AValue: Pointer);
    procedure PushSingle(const AValue: Single);
    procedure PushString(const AValue: WideString);
    procedure PushVariant(const AValue: Variant);
    procedure PushWideString(const AValue: WideString);
  end;

  {
    IL4DStackKeyed
  }
  IL4DStackKeyed = interface(IL4DStack)
  ['{6D889C0C-4611-49D7-841E-1B76C29CE15E}']
    function NewTable(const AKey: AnsiString): IL4DTable;
    procedure PushAnsiChar(const AKey: AnsiString; const AValue: AnsiChar);
    procedure PushAnsiString(const AKey: AnsiString; const AValue: AnsiString);
    procedure PushBoolean(const AKey: AnsiString; const AValue: Boolean);
    procedure PushChar(const AKey: AnsiString; const AValue: Char);
    procedure PushDouble(const AKey: AnsiString; const AValue: Double);
    procedure PushExtended(const AKey: AnsiString; const AValue: Extended);
    procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
    procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
    procedure PushFunction(const AKey: AnsiString; const AValue: TLuaDelphiFunction); overload;  // Standard Lua "C" Function
    procedure PushInteger(const AKey: AnsiString; const AValue: Integer);
    procedure PushPAnsiChar(const AKey: AnsiString; const AValue: PAnsiChar);
    procedure PushPChar(const AKey: AnsiString; const AValue: PChar);
    procedure PushPointer(const AKey: AnsiString; const AValue: Pointer);
    procedure PushSingle(const AKey: AnsiString; const AValue: Single);
    procedure PushString(const AKey: AnsiString; const AValue: WideString);
    procedure PushVariant(const AKey: AnsiString; const AValue: Variant);
    procedure PushWideString(const AKey: AnsiString; const AValue: WideString);
  end;

  {
    IL4DStackCombo
    NOTE: METHODS CONTAINED WITHIN MUST BE A DIRECT CLONE OF "IL4DStackKeyed".
    This is due to an annoying limitation on Inheritence of Interfaces in Delphi.
  }
  IL4DStackCombo = interface(IL4DStackIndexed)
  ['{3AC69963-8142-44F7-8C10-0CE82C6A5401}']
    function NewTable(const AKey: AnsiString): IL4DTable; overload;
    procedure PushAnsiChar(const AKey: AnsiString; const AValue: AnsiChar); overload;
    procedure PushAnsiString(const AKey: AnsiString; const AValue: AnsiString); overload;
    procedure PushBoolean(const AKey: AnsiString; const AValue: Boolean); overload;
    procedure PushChar(const AKey: AnsiString; const AValue: Char); overload;
    procedure PushDouble(const AKey: AnsiString; const AValue: Double); overload;
    procedure PushExtended(const AKey: AnsiString; const AValue: Extended); overload;
    procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
    procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
    procedure PushFunction(const AKey: AnsiString; const AValue: TLuaDelphiFunction); overload;  // Standard Lua "C" Function
    procedure PushInteger(const AKey: AnsiString; const AValue: Integer); overload;
    procedure PushPAnsiChar(const AKey: AnsiString; const AValue: PAnsiChar); overload;
    procedure PushPChar(const AKey: AnsiString; const AValue: PChar); overload;
    procedure PushPointer(const AKey: AnsiString; const AValue: Pointer); overload;
    procedure PushSingle(const AKey: AnsiString; const AValue: Single); overload;
    procedure PushString(const AKey: AnsiString; const AValue: WideString); overload;
    procedure PushVariant(const AKey: AnsiString; const AValue: Variant); overload;
    procedure PushWideString(const AKey: AnsiString; const AValue: WideString); overload;
  end;

  {
    IL4DStackValue
  }
  IL4DStackValue = interface
  ['{42CFEE5D-56E9-4A22-B4F7-AA90E5389CB9}']
    // Getters
    function GetAsAnsiString: AnsiString;
    function GetAsBoolean: Boolean;
    function GetAsChar: Char;
    function GetAsDouble: Double;
    function GetAsExtended: Extended;
    function GetAsInteger: Integer;
    function GetAsPAnsiChar: PAnsiChar;
    function GetAsPChar: PChar;
    function GetAsPointer: Pointer;
    function GetAsSingle: Single;
    function GetAsString: String;
    function GetAsTable: IL4DTable;
    function GetAsVariant: Variant;
    function GetAsWideString: WideString;
    function GetCanBeAnsiString: Boolean;
    function GetCanBeBoolean: Boolean;
    function GetCanBeChar: Boolean;
    function GetCanBeDouble: Boolean;
    function GetCanBeExtended: Boolean;
    function GetCanBeInteger: Boolean;
    function GetCanBePAnsiChar: Boolean;
    function GetCanBePChar: Boolean;
    function GetCanBePointer: Boolean;
    function GetCanBeSingle: Boolean;
    function GetCanBeString: Boolean;
    function GetCanBeTable: Boolean;
    function GetCanBeVariant: Boolean;
    function GetCanBeWideString: Boolean;
    function GetType: TL4DStackValueType;
    function GetTypeName: String;
    // Public & Properties
    function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
    property AsAnsiString: AnsiString read GetAsAnsiString;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsChar: Char read GetAsChar;
    property AsDouble: Double read GetAsDouble;
    property AsExtended: Extended read GetAsExtended;
    property AsInteger: Integer read GetAsInteger;
    property AsPAnsiChar: PAnsiChar read GetAsPAnsiChar;
    property AsPChar: PChar read GetAsPChar;
    property AsPointer: Pointer read GetAsPointer;
    property AsSingle: Single read GetAsSingle;
    property AsString: String read GetAsString;
    property AsTable: IL4DTable read GetAsTable;
    property AsVariant: Variant read GetAsVariant;
    property AsWideString: WideString read GetAsWideString;
    property CanBeAnsiString: Boolean read GetCanBeAnsiString;
    property CanBeBoolean: Boolean read GetCanBeBoolean;
    property CanBeChar: Boolean read GetCanBeChar;
    property CanBeDouble: Boolean read GetCanBeDouble;
    property CanBeExtended: Boolean read GetCanBeExtended;
    property CanBeInteger: Boolean read GetCanBeInteger;
    property CanBePAnsiChar: Boolean read GetCanBePAnsiChar;
    property CanBePChar: Boolean read GetCanBePChar;
    property CanBePointer: Boolean read GetCanBePointer;
    property CanBeSingle: Boolean read GetCanBeSingle;
    property CanBeString: Boolean read GetCanBeString;
    property CanBeTable: Boolean read GetCanBeTable;
    property CanBeVariant: Boolean read GetCanBeVariant;
    property CanBeWideString: Boolean read GetCanBeWideString;
    property LuaType: TL4DStackValueType read GetType;
    property LuaTypeName: String read GetTypeName;
  end;

  {
    IL4DTableValueInternal
  }
  IL4DTableValueInternal = interface
  ['{2F117FB3-92F6-4B05-AD30-AFF8EFCA2715}']
    procedure SetIndex(const AIndex: Integer);
    procedure SetKey(const AKey: AnsiString);
  end;

  {
    IL4DTableValue
  }
  IL4DTableValue = interface(IL4DStackValue)
  ['{6782A114-18C5-4E1F-8FA3-E9E175735961}']
    property AsAnsiString: AnsiString read GetAsAnsiString;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsChar: Char read GetAsChar;
    property AsDouble: Double read GetAsDouble;
    property AsExtended: Extended read GetAsExtended;
    property AsInteger: Integer read GetAsInteger;
    property AsPAnsiChar: PAnsiChar read GetAsPAnsiChar;
    property AsPChar: PChar read GetAsPChar;
    property AsPointer: Pointer read GetAsPointer;
    property AsSingle: Single read GetAsSingle;
    property AsString: String read GetAsString;
    property AsTable: IL4DTable read GetAsTable;
    property AsVariant: Variant read GetAsVariant;
    property AsWideString: WideString read GetAsWideString;
    property CanBeAnsiString: Boolean read GetCanBeAnsiString;
    property CanBeBoolean: Boolean read GetCanBeBoolean;
    property CanBeChar: Boolean read GetCanBeChar;
    property CanBeDouble: Boolean read GetCanBeDouble;
    property CanBeExtended: Boolean read GetCanBeExtended;
    property CanBeInteger: Boolean read GetCanBeInteger;
    property CanBePAnsiChar: Boolean read GetCanBePAnsiChar;
    property CanBePChar: Boolean read GetCanBePChar;
    property CanBePointer: Boolean read GetCanBePointer;
    property CanBeSingle: Boolean read GetCanBeSingle;
    property CanBeString: Boolean read GetCanBeString;
    property CanBeTable: Boolean read GetCanBeTable;
    property CanBeVariant: Boolean read GetCanBeVariant;
    property CanBeWideString: Boolean read GetCanBeWideString;
    property LuaType: TL4DStackValueType read GetType;
    property LuaTypeName: String read GetTypeName;
  end;

  {
    IL4DTableInternal
  }
  IL4DTableInternal = interface
  ['{2929F799-A7F2-48A8-8DB0-1C6DBFD9141F}']
    function GetName: AnsiString;
    procedure PushTable;
    procedure SetIndex(const AIndex: Integer); overload;
    procedure SetName(const AName: AnsiString); overload;
    procedure SetOnPushTable(const AEvent: TL4DTableChangeEvent); overload;
  end;

  {
    IL4DTable
  }
  IL4DTable = interface(IL4DStackCombo)
  ['{FCFD27AC-D5EC-42A4-81EF-E1288A6A1EB6}']
    procedure Close;
    function GetValueByIndex(const AIndex: Integer): IL4DTableValue;
    function GetValueByName(const AName: AnsiString): IL4DTableValue;
    procedure Push;
    property Value[const AIndex: Integer]: IL4DTableValue read GetValueByIndex; default;
    property Value[const AName: AnsiString]: IL4DTableValue read GetValueByName; default;
  end;

  {
    IL4DMethodResultStackValue
  }
  IL4DMethodResultStackValue = interface(IL4DStackValue)
  ['{97D4EEDC-D104-42C0-8C5D-DC098ED87B0B}']
    property AsAnsiString: AnsiString read GetAsAnsiString;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsChar: Char read GetAsChar;
    property AsDouble: Double read GetAsDouble;
    property AsExtended: Extended read GetAsExtended;
    property AsInteger: Integer read GetAsInteger;
    property AsPAnsiChar: PAnsiChar read GetAsPAnsiChar;
    property AsPChar: PChar read GetAsPChar;
    property AsPointer: Pointer read GetAsPointer;
    property AsSingle: Single read GetAsSingle;
    property AsString: String read GetAsString;
    property AsTable: IL4DTable read GetAsTable;
    property AsVariant: Variant read GetAsVariant;
    property AsWideString: WideString read GetAsWideString;
    property CanBeAnsiString: Boolean read GetCanBeAnsiString;
    property CanBeBoolean: Boolean read GetCanBeBoolean;
    property CanBeChar: Boolean read GetCanBeChar;
    property CanBeDouble: Boolean read GetCanBeDouble;
    property CanBeExtended: Boolean read GetCanBeExtended;
    property CanBeInteger: Boolean read GetCanBeInteger;
    property CanBePAnsiChar: Boolean read GetCanBePAnsiChar;
    property CanBePChar: Boolean read GetCanBePChar;
    property CanBePointer: Boolean read GetCanBePointer;
    property CanBeSingle: Boolean read GetCanBeSingle;
    property CanBeString: Boolean read GetCanBeString;
    property CanBeTable: Boolean read GetCanBeTable;
    property CanBeVariant: Boolean read GetCanBeVariant;
    property CanBeWideString: Boolean read GetCanBeWideString;
    property LuaType: TL4DStackValueType read GetType;
    property LuaTypeName: String read GetTypeName;
  end;

  {
    IL4DMethodResultStack
  }
  IL4DMethodResultStack = interface(IL4DStack)
  ['{443213D7-773C-413A-8680-CFEF834CCC15}']
    procedure Cleanup;
    function GetValue(const AIndex: Integer): IL4DMethodResultStackValue;
    property Count: Integer read GetCount;
    property Value[const AIndex: Integer]: IL4DMethodResultStackValue read GetValue; default;
  end;

  {
    IL4DMethodStackValue
  }
  IL4DMethodStackValue = interface(IL4DStackValue)
  ['{DF44A959-F488-4970-A74A-3E73E782E388}']
    property AsAnsiString: AnsiString read GetAsAnsiString;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsChar: Char read GetAsChar;
    property AsDouble: Double read GetAsDouble;
    property AsExtended: Extended read GetAsExtended;
    property AsInteger: Integer read GetAsInteger;
    property AsPAnsiChar: PAnsiChar read GetAsPAnsiChar;
    property AsPChar: PChar read GetAsPChar;
    property AsPointer: Pointer read GetAsPointer;
    property AsSingle: Single read GetAsSingle;
    property AsString: String read GetAsString;
    property AsTable: IL4DTable read GetAsTable;
    property AsVariant: Variant read GetAsVariant;
    property AsWideString: WideString read GetAsWideString;
    property CanBeAnsiString: Boolean read GetCanBeAnsiString;
    property CanBeBoolean: Boolean read GetCanBeBoolean;
    property CanBeChar: Boolean read GetCanBeChar;
    property CanBeDouble: Boolean read GetCanBeDouble;
    property CanBeExtended: Boolean read GetCanBeExtended;
    property CanBeInteger: Boolean read GetCanBeInteger;
    property CanBePAnsiChar: Boolean read GetCanBePAnsiChar;
    property CanBePChar: Boolean read GetCanBePChar;
    property CanBePointer: Boolean read GetCanBePointer;
    property CanBeSingle: Boolean read GetCanBeSingle;
    property CanBeString: Boolean read GetCanBeString;
    property CanBeTable: Boolean read GetCanBeTable;
    property CanBeVariant: Boolean read GetCanBeVariant;
    property CanBeWideString: Boolean read GetCanBeWideString;
    property LuaType: TL4DStackValueType read GetType;
    property LuaTypeName: String read GetTypeName;
  end;

  {
    IL4DMethodInternal
  }
  IL4DMethodStackInternal = interface
  ['{06D1CD69-EBF5-4260-A724-B34AB7363AA3}']
    function GetPushCount: Integer;
  end;

  {
    IL4DMethodStack
  }
  IL4DMethodStack = interface(IL4DStackIndexed)
  ['{424F2054-837C-4CAA-AE38-386E247A52CD}']
    function GetValue(const AIndex: Integer): IL4DMethodStackValue;
    property Value[const AIndex: Integer]: IL4DMethodStackValue read GetValue; default;
  end;

  {
    IL4DGlobalStackValue
  }
  IL4DGlobalStackValue = interface(IL4DStackValue)
  ['{849E7A91-C78A-4A1A-B82A-B9B895339198}']
    procedure Delete;
    procedure SetAnsiString(const AValue: AnsiString);
    procedure SetBoolean(const AValue: Boolean);
    procedure SetChar(const AValue: Char);
    procedure SetDouble(const AValue: Double);
    procedure SetExtended(const AValue: Extended);
    procedure SetInteger(const AValue: Integer);
    procedure SetPAnsiChar(const AValue: PAnsiChar);
    procedure SetPChar(const AValue: PChar);
    procedure SetPointer(const AValue: Pointer);
    procedure SetSingle(const AValue: Single);
    procedure SetString(const AValue: String);
    procedure SetVariant(const AValue: Variant);
    procedure SetWideString(const AValue: WideString);
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAnsiString;
    property AsBoolean: Boolean read GetAsBoolean write SetBoolean;
    property AsChar: Char read GetAsChar write SetChar;
    property AsDouble: Double read GetAsDouble write SetDouble;
    property AsExtended: Extended read GetAsExtended write SetExtended;
    property AsInteger: Integer read GetAsInteger write SetInteger;
    property AsPAnsiChar: PAnsiChar read GetAsPAnsiChar write SetPAnsiChar;
    property AsPChar: PChar read GetAsPChar write SetPChar;
    property AsPointer: Pointer read GetAsPointer write SetPointer;
    property AsSingle: Single read GetAsSingle write SetSingle;
    property AsString: String read GetAsString write SetString;
    property AsTable: IL4DTable read GetAsTable;
    property AsVariant: Variant read GetAsVariant write SetVariant;
    property AsWideString: WideString read GetAsWideString write SetWideString;
    property CanBeAnsiString: Boolean read GetCanBeAnsiString;
    property CanBeBoolean: Boolean read GetCanBeBoolean;
    property CanBeChar: Boolean read GetCanBeChar;
    property CanBeDouble: Boolean read GetCanBeDouble;
    property CanBeExtended: Boolean read GetCanBeExtended;
    property CanBeInteger: Boolean read GetCanBeInteger;
    property CanBePAnsiChar: Boolean read GetCanBePAnsiChar;
    property CanBePChar: Boolean read GetCanBePChar;
    property CanBePointer: Boolean read GetCanBePointer;
    property CanBeSingle: Boolean read GetCanBeSingle;
    property CanBeString: Boolean read GetCanBeString;
    property CanBeTable: Boolean read GetCanBeTable;
    property CanBeVariant: Boolean read GetCanBeVariant;
    property CanBeWideString: Boolean read GetCanBeWideString;
    property LuaType: TL4DStackValueType read GetType;
    property LuaTypeName: String read GetTypeName;
  end;

  {
    IL4DGlobalStack
  }
  IL4DGlobalStack = interface(IL4DStack)
  ['{37D5DF87-8737-4512-A8DC-791C26C9D089}']
    function GetGlobal(const AKey: AnsiString): IL4DGlobalStackValue;
    procedure SetGlobal(const AKey: AnsiString);
    property Value[const AKey: AnsiString]: IL4DGlobalStackValue read GetGlobal; default;
  end;

  {
    IL4DEngineManager
  }
  IL4DEngineManager = interface
  ['{09629B49-C4FB-466B-8513-19BF612EE9D4}']

  end;

  {
    IL4DEngineInternal
  }
  IL4DEngineInternal = interface
  ['{F3E9410B-C1C6-4533-BCF6-FD04085F8062}']
    function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
    function GetAsAnsiString(const AIndex: Integer): AnsiString;
    function GetAsBoolean(const AIndex: Integer): Boolean;
    function GetAsChar(const AIndex: Integer): Char;
    function GetAsDouble(const AIndex: Integer): Double;
    function GetAsExtended(const AIndex: Integer): Extended;
    function GetAsInteger(const AIndex: Integer): Integer;
    function GetAsPAnsiChar(const AIndex: Integer): PAnsiChar;
    function GetAsPChar(const AIndex: Integer): PChar;
    function GetAsPointer(const AIndex: Integer): Pointer;
    function GetAsSingle(const AIndex: Integer): Single;
    function GetAsString(const AIndex: Integer): WideString;
    function GetAsTable(const AIndex: Integer): IL4DTable;
    function GetAsWideString(const AIndex: Integer): WideString;
    function GetAsVariant(const AIndex: Integer): Variant;
    function GetLuaType(const AIndex: Integer): TL4DStackValueType; //inline;
    function GetLuaTypeName(const AIndex: Integer): String;
    function LoadLuaCode(const ACode, AName: WideString; const AAutoExecute: Boolean = True): Boolean;
    function LoadLuaFile(const AFileName: WideString; const AAutoExecute: Boolean = True): Boolean;
    function NewTable: IL4DTable;
    procedure Pop(const ANumber: Integer);
    procedure PushAnsiChar(const AValue: AnsiChar);
    procedure PushAnsiString(const AValue: AnsiString);
    procedure PushBoolean(const AValue: Boolean);
    procedure PushChar(const AValue: Char);
    procedure PushDouble(const AValue: Double);
    procedure PushExtended(const AValue: Extended);
    procedure PushFunction(const AValue: TL4DDelphiFunction); overload;         // Stack-Managed Delphi Procedure
    procedure PushFunction(const AValue: TL4DDelphiObjectFunction); overload;   // Stack-Managed Delphi Object Procedure
    procedure PushFunction(const AValue: TLuaDelphiFunction); overload;  // Standard Lua "C" Function
    procedure PushInteger(const AValue: Integer);
    procedure PushPAnsiChar(const AValue: PAnsiChar);
    procedure PushPChar(const AValue: PChar);
    procedure PushPointer(const AValue: Pointer);
    procedure PushSingle(const AValue: Single);
    procedure PushString(const AValue: WideString);
    procedure PushVariant(const AValue: Variant);
    procedure PushWideString(const AValue: WideString);
    procedure Remove(const AIndex: Integer);
    function SafeLuaExecute(const ANumArgs: Integer = 0; const ANumResults: Integer = 0; const AErrorFunc: Integer = 0): Integer; // Handles exceptions when executing Lua code
  end;

  {
    IL4DEngine
  }
  IL4DEngine = interface
  ['{E9C6CD0C-0CE0-4073-AA1B-217520E93929}']
    function GetLua: TLuaCommon;
    property Lua: TLuaCommon read GetLua;
  end;

implementation

end.
