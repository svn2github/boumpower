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
  Unit: L4D.Engine.Main.pas
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
unit L4D.Engine.Main;

interface

{$I Lua4Delphi.inc}

uses
  {$IFDEF DELPHIXE2}
    System.Classes, System.SysUtils, System.Variants,
  {$ELSE}
    Classes, SysUtils, Variants,
  {$ENDIF}
  LKSL.Strings.Conversion, LKSL.Strings.Utils,
  L4D.Lua.Intf, L4D.Lua.Common, L4D.Engine.Constants;

type
  { Lua4Delphi-specific Enums }
  TL4DStackValueType = (svtNone = -1, svtNil = 0, svtBoolean, svtLightUserData, svtNumber, svtString, svtTable, svtFunction, svtUserdata, svtThread);

const
  // Lua4Delphi-specific Type Constants...
  LUA_TYPES: Array[0..9] of TL4DStackValueType = (svtNone, svtNil, svtBoolean, svtLightUserData, svtNumber, svtString, svtTable, svtFunction, svtUserdata, svtThread);
  LUA_TYPE_NAMES: Array[TL4DStackValueType] of String = ('None', 'Nil', 'Boolean', 'Light Userdata', 'Number', 'String', 'Table', 'Function', 'Userdata', 'Thread');

type
  { Forward Declarations }
  TL4DMethodStack = class;
  TL4DGlobalStack = class;
  TL4DEngine = class;
  TL4DEngineMember = class;
  TL4DOptions = class;
  TL4DLibraryOptions = class;
  TLua4DelphiCommon = class;

  { Enum Types }
  TL4DInstanceType = (litNew, litExisting);

  { Exceptions }
  ELuaException = class(Exception); { TODO 1 -cException Handling : Define ELuaException Type }
  EL4DException = class(Exception); // Our "Base Exception Type"
  EL4DLuaException = class(EL4DException);
  EL4DFileNotFound = class(EL4DException);
  EL4DClassAlreadyRegistered = class(EL4DException);

  { Class References }
  EL4DExceptionClass = class of EL4DException;

  { "OnEvent" Method Types }
  TL4DExceptionEvent = procedure(const AExceptionType: EL4DExceptionClass; AMessage: String; var ARaise: Boolean) of object;
  TL4DLuaErrorEvent = procedure(const ATitle, AMessage: String; const ALine: Integer; var ARaise: Boolean) of object;

  { Method Types }
  PL4DDelphiFunction = ^TL4DDelphiFunction;
  TL4DDelphiFunction = procedure(var ALuaStack: TL4DMethodStack);
  PL4DDelphiObjectFunction = ^TL4DDelphiObjectFunction;
  TL4DDelphiObjectFunction = procedure(var ALuaStack: TL4DMethodStack) of object;

  { Array Types }
  TPointerArray = Array of Pointer;

  {
    TL4DMethodResultStack
  }
  {$REGION 'Method Result Stack Type'}
    PL4DMethodResultStack = ^TL4DMethodResultStack;
    TL4DMethodResultStack = record
      type TL4DMethodResultStackValue = record
      private
        FIndex: Integer;
        FStack: PL4DMethodResultStack;
        // Getters
        function GetAsAnsiString: AnsiString; inline;
        function GetAsBoolean: Boolean; inline;
        function GetAsChar: Char; inline;
        function GetAsDouble: Double; inline;
        function GetAsExtended: Extended; inline;
        function GetAsInteger: Integer; inline;
        function GetAsPAnsiChar: PAnsiChar; inline;
        function GetAsPChar: PChar; inline;
        function GetAsPointer: Pointer; inline;
        function GetAsSingle: Single; inline;
        function GetAsString: String; inline;
        function GetAsVariant: Variant; inline;
        function GetAsWideString: WideString; inline;
        function GetCanBeAnsiString: Boolean; inline;
        function GetCanBeBoolean: Boolean; inline;
        function GetCanBeChar: Boolean; inline;
        function GetCanBeDouble: Boolean; inline;
        function GetCanBeExtended: Boolean; inline;
        function GetCanBeInteger: Boolean; inline;
        function GetCanBePAnsiChar: Boolean; inline;
        function GetCanBePChar: Boolean; inline;
        function GetCanBePointer: Boolean; inline;
        function GetCanBeSingle: Boolean; inline;
        function GetCanBeString: Boolean; inline;
        function GetCanBeVariant: Boolean; inline;
        function GetCanBeWideString: Boolean; inline;
        function GetType: TL4DStackValueType; inline;
        function GetTypeName: String; inline;
      public
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): TL4DMethodResultStack;
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
        property CanBeVariant: Boolean read GetCanBeVariant;
        property CanBeWideString: Boolean read GetCanBeWideString;
        property LuaType: TL4DStackValueType read GetType;
        property LuaTypeName: String read GetTypeName;
      end;
    private
      FLua: TL4DEngine;
      FOffset: Integer;
      function GetCount: Integer; inline;
      function GetValue(const AIndex: Integer): TL4DMethodResultStackValue; {inline;}
    public
      procedure Cleanup;
      property Count: Integer read GetCount;
      property Value[const AIndex: Integer]: TL4DMethodResultStackValue read GetValue; default;
    end;
  {$ENDREGION}

  {
    TL4DMethodStack
  }
  {$REGION 'TL4DMethodStack- Method Stack Type'}
    TL4DMethodStack = class(TPersistent)
      type TL4DMethodStackValue = record
      private
        FIndex: Integer;
        FStack: TL4DMethodStack;
        // Getters
        function GetAsAnsiString: AnsiString; inline;
        function GetAsBoolean: Boolean; inline;
        function GetAsChar: Char; inline;
        function GetAsDouble: Double; inline;
        function GetAsExtended: Extended; inline;
        function GetAsInteger: Integer; inline;
        function GetAsPAnsiChar: PAnsiChar; inline;
        function GetAsPChar: PChar; inline;
        function GetAsPointer: Pointer; inline;
        function GetAsSingle: Single; inline;
        function GetAsString: String; inline;
        function GetAsVariant: Variant; inline;
        function GetAsWideString: WideString; inline;
        function GetCanBeAnsiString: Boolean; inline;
        function GetCanBeBoolean: Boolean; inline;
        function GetCanBeChar: Boolean; inline;
        function GetCanBeDouble: Boolean; inline;
        function GetCanBeExtended: Boolean; inline;
        function GetCanBeInteger: Boolean; inline;
        function GetCanBePAnsiChar: Boolean; inline;
        function GetCanBePChar: Boolean; inline;
        function GetCanBePointer: Boolean; inline;
        function GetCanBeSingle: Boolean; inline;
        function GetCanBeString: Boolean; inline;
        function GetCanBeVariant: Boolean; inline;
        function GetCanBeWideString: Boolean; inline;
        function GetType: TL4DStackValueType; inline;
        function GetTypeName: String; inline;
      public
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): TL4DMethodResultStack;
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
        property CanBeVariant: Boolean read GetCanBeVariant;
        property CanBeWideString: Boolean read GetCanBeWideString;
        property LuaType: TL4DStackValueType read GetType;
        property LuaTypeName: String read GetTypeName;
      end;
    private
      FLua: TL4DEngine;
      FPushCount: Integer;
      function GetCount: Integer; inline;
      function GetValue(const AIndex: Integer): TL4DMethodStackValue; inline;
    public
      constructor Create(const ALua: TL4DEngine);
      procedure PushAnsiChar(const AValue: AnsiChar); inline;
      procedure PushAnsiString(const AValue: AnsiString); inline;
      procedure PushBoolean(const AValue: Boolean); inline;
      procedure PushChar(const AValue: Char); inline;
      procedure PushDouble(const AValue: Double); inline;
      procedure PushExtended(const AValue: Extended); inline;
      procedure PushFunction(const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
      procedure PushFunction(const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
      procedure PushFunction(const AValue: TLuaDelphiFunction); overload; inline; // Standard Lua "C" Function
      procedure PushInteger(const AValue: Integer); inline;
      procedure PushPAnsiChar(const AValue: PAnsiChar); inline;
      procedure PushPChar(const AValue: PChar); inline;
      procedure PushPointer(const AValue: Pointer); inline;
      procedure PushSingle(const AValue: Single); inline;
      procedure PushString(const AValue: WideString); inline;
      procedure PushVariant(const AValue: Variant); inline;
      procedure PushWideString(const AValue: WideString); inline;
      property Count: Integer read GetCount;
      property Value[const AIndex: Integer]: TL4DMethodStackValue read GetValue; default;
    end;
  {$ENDREGION}

  {
    TL4DGlobalStack
      - Manages the stack for Globals
  }
  {$REGION 'TL4DGlobalStack - Global Stack Type'}
    TL4DGlobalStack = class(TPersistent)
      type TL4DGlobalStackValue = record
      private
        FKey: AnsiString;
        FStack: TL4DGlobalStack;
        // Getters
        function GetAsAnsiString: AnsiString; inline;
        function GetAsBoolean: Boolean; inline;
        function GetAsChar: Char; inline;
        function GetAsDouble: Double; inline;
        function GetAsExtended: Extended; inline;
        function GetAsInteger: Integer; inline;
        function GetAsPAnsiChar: PAnsiChar; inline;
        function GetAsPChar: PChar; inline;
        function GetAsPointer: Pointer; inline;
        function GetAsSingle: Single; inline;
        function GetAsString: String; inline;
        function GetAsVariant: Variant; inline;
        function GetAsWideString: WideString; inline;
        function GetType: TL4DStackValueType; inline;
        function GetTypeName: String; inline;
        // Setters
        procedure SetAnsiString(const AValue: AnsiString); inline;
        procedure SetBoolean(const AValue: Boolean); inline;
        procedure SetChar(const AValue: Char); inline;
        procedure SetDouble(const AValue: Double); inline;
        procedure SetExtended(const AValue: Extended); inline;
        procedure SetInteger(const AValue: Integer); inline;
        procedure SetPAnsiChar(const AValue: PAnsiChar); inline;
        procedure SetPChar(const AValue: PChar); inline;
        procedure SetPointer(const AValue: Pointer); inline;
        procedure SetSingle(const AValue: Single); inline;
        procedure SetString(const AValue: String); inline;
        procedure SetVariant(const AValue: Variant); inline;
        procedure SetWideString(const AValue: WideString); inline;
      public
        procedure Delete; inline;
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): TL4DMethodResultStack;
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
        property AsVariant: Variant read GetAsVariant write SetVariant;
        property AsWideString: WideString read GetAsWideString write SetWideString;
        property LuaType: TL4DStackValueType read GetType;
        property LuaTypeName: String read GetTypeName;
      end;
    private
      FLua: TL4DEngine;
      function GetGlobal(const AKey: AnsiString): TL4DGlobalStackValue; inline;
      procedure SetGlobal(const AKey: AnsiString); inline;
    public
      constructor Create(const ALua: TL4DEngine);
      procedure PushAnsiChar(const AKey: AnsiString; const AValue: AnsiChar); inline;
      procedure PushAnsiString(const AKey: AnsiString; const AValue: AnsiString); inline;
      procedure PushBoolean(const AKey: AnsiString; const AValue: Boolean); inline;
      procedure PushChar(const AKey: AnsiString; const AValue: Char); inline;
      procedure PushDouble(const AKey: AnsiString; const AValue: Double); inline;
      procedure PushExtended(const AKey: AnsiString; const AValue: Extended); inline;
      procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
      procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
      procedure PushFunction(const AKey: AnsiString; const AValue: TLuaDelphiFunction); overload; inline; // Standard Lua "C" Function
      procedure PushInteger(const AKey: AnsiString; const AValue: Integer); inline;
      procedure PushPAnsiChar(const AKey: AnsiString; const AValue: PAnsiChar); inline;
      procedure PushPChar(const AKey: AnsiString; const AValue: PChar); inline;
      procedure PushPointer(const AKey: AnsiString; const AValue: Pointer); inline;
      procedure PushSingle(const AKey: AnsiString; const AValue: Single); inline;
      procedure PushString(const AKey: AnsiString; const AValue: WideString); inline;
      procedure PushVariant(const AKey: AnsiString; const AValue: Variant); inline;
      procedure PushWideString(const AKey: AnsiString; const AValue: WideString); inline;
      property Lua: TL4DEngine read FLua;
      property Value[const AKey: AnsiString]: TL4DGlobalStackValue read GetGlobal; default;
    end;
  {$ENDREGION}

  {
    TL4DEngine
      - Sits Lua4Delphi methods on top of Lua API
  }
  {$REGION 'TL4DEngine - Sits Lua4Delphi methods on top of Lua API'}
    TL4DEngine = class(TPersistent)
    private
      FDesignMode: Boolean;
      FGlobals: TL4DGlobalStack;
      FLua: TLuaCommon;
      FInstanceType: TL4DInstanceType;
      //FMethods: TPointerArray;
      FOptions: TL4DOptions;
      FOnException: TL4DExceptionEvent;
      FOnLuaError: TL4DLuaErrorEvent;
    public
      // Lua4Delphi Custom Macros
      function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): TL4DMethodResultStack;
      function GetAsAnsiString(const AIndex: Integer): AnsiString; inline;
      function GetAsBoolean(const AIndex: Integer): Boolean; inline;
      function GetAsChar(const AIndex: Integer): Char; inline;
      function GetAsDouble(const AIndex: Integer): Double; inline;
      function GetAsExtended(const AIndex: Integer): Extended; inline;
      function GetAsInteger(const AIndex: Integer): Integer; inline;
      function GetAsPAnsiChar(const AIndex: Integer): PAnsiChar; inline;
      function GetAsPChar(const AIndex: Integer): PChar; inline;
      function GetAsPointer(const AIndex: Integer): Pointer; inline;
      function GetAsSingle(const AIndex: Integer): Single; inline;
      function GetAsString(const AIndex: Integer): WideString; inline;
      function GetAsWideString(const AIndex: Integer): WideString; inline;
      function GetAsVariant(const AIndex: Integer): Variant; inline;
      function GetLuaType(const AIndex: Integer): TL4DStackValueType; //inline;
      function GetLuaTypeName(const AIndex: Integer): String; inline;
      function LoadLuaCode(const ACode, AName: WideString; const AAutoExecute: Boolean = True): Boolean;
      function LoadLuaFile(const AFileName: WideString; const AAutoExecute: Boolean = True): Boolean;
      procedure Pop(const ANumber: Integer); inline;
      procedure PushAnsiChar(const AValue: AnsiChar); inline;
      procedure PushAnsiString(const AValue: AnsiString); inline;
      procedure PushBoolean(const AValue: Boolean); inline;
      procedure PushChar(const AValue: Char); inline;
      procedure PushDouble(const AValue: Double); inline;
      procedure PushExtended(const AValue: Extended); inline;
      procedure PushFunction(const AValue: TL4DDelphiFunction); overload;         // Stack-Managed Delphi Procedure
      procedure PushFunction(const AValue: TL4DDelphiObjectFunction); overload;   // Stack-Managed Delphi Object Procedure
      procedure PushFunction(const AValue: TLuaDelphiFunction); overload; inline; // Standard Lua "C" Function
      procedure PushInteger(const AValue: Integer); inline;
      procedure PushPAnsiChar(const AValue: PAnsiChar); inline;
      procedure PushPChar(const AValue: PChar); inline;
      procedure PushPointer(const AValue: Pointer); inline;
      procedure PushSingle(const AValue: Single); inline;
      procedure PushString(const AValue: WideString); inline;
      procedure PushVariant(const AValue: Variant); inline;
      procedure PushWideString(const AValue: WideString); inline;
      procedure Remove(const AIndex: Integer); inline;
      function SafeLuaExecute(const ANumArgs: Integer = 0; const ANumResults: Integer = 0; const AErrorFunc: Integer = 0): Integer; // Handles exceptions when executing Lua code
      // Base Methods
      constructor Create(ALuaType: TLuaBaseType); overload;
      constructor Create(ALuaType: TLuaBaseType; const ALuaState: PLuaState); overload;
      destructor Destroy; override;
    published
      property Globals: TL4DGlobalStack read FGlobals write FGlobals;
      property Lua: TLuaCommon read FLua write FLua;
      property Options: TL4DOptions read FOptions write FOptions;
      property OnException: TL4DExceptionEvent read FOnException write FOnException;
      property OnLuaError: TL4DLuaErrorEvent read FOnLuaError write FOnLuaError;
    end;
  {$ENDREGION}

  {
    TL4DEngineMember
      - An object which links back to TL4DEngine
  }
  {$REGION 'TL4DEngineMember - An object which links back to TL4DEngine'}
  TL4DEngineMember = class(TPersistent)
  protected
    FLua: TL4DEngine;
  public
    constructor Create(const ALua: TL4DEngine); virtual;
    property Lua: TL4DEngine read FLua write FLua;
  end;
  {$ENDREGION}

    {
    TL4DOptions
      - Contains our various Instance Options
  }
  {$REGION 'TL4DOptions - Lua4Delphi Options Type'}
    TL4DOptions = class(TL4DEngineMember)
    private
      FLibraries: TL4DLibraryOptions;
    public
      constructor Create(const ALua: TL4DEngine); override;
      destructor Destroy; override;
    published
      property Libraries: TL4DLibraryOptions read FLibraries write FLibraries;
    published

    end;
  {$ENDREGION}

  {
    TL4DLibraries
      - Object containing Boolean Switches for the various Lua Libraries
  }
  {$REGION 'TL4DLibraryOptions - Lua4Delphi Library Options Type'}
    TL4DLibraryOptions = class(TL4DEngineMember)
    private
      FBase,
      FDebug,
      FIO,
      FMath,
      FOS,
      FPackage,
      FString,
      FTable: Boolean;
      function GetBase: Boolean;
      function GetDebug: Boolean;
      function GetIO: Boolean;
      function GetMath: Boolean;
      function GetOS: Boolean;
      function GetPackage: Boolean;
      function GetString: Boolean;
      function GetTable: Boolean;
      procedure SetBase(const AValue: Boolean);
      procedure SetDebug(const AValue: Boolean);
      procedure SetIO(const AValue: Boolean);
      procedure SetMath(const AValue: Boolean);
      procedure SetOS(const AValue: Boolean);
      procedure SetPackage(const AValue: Boolean);
      procedure SetString(const AValue: Boolean);
      procedure SetTable(const AValue: Boolean);
    public
      constructor Create(const ALua: TL4DEngine); override;
    published
      property Base: Boolean read GetBase write SetBase default True;
      property Debug: Boolean read GetDebug write SetDebug default False;
      property IO: Boolean read GetIO write SetIO default True;
      property Math: Boolean read GetMath write SetMath default True;
      property OS: Boolean read GetOS write SetOS default True;
      property Package: Boolean read GetPackage write SetPackage default True;
      property Strings: Boolean read GetString write SetString default True;
      property Table: Boolean read GetTable write SetTable default True;
    end;
  {$ENDREGION}


  {
    TLua4DelphiCommon
      - The Base Component for Lua4Delphi variants
  }
  {$REGION 'TLua4DelphiCommon - The Base Component for Lua4Delphi variants'}
    TLua4DelphiCommon = class(TComponent)
    protected
      FEngine: TL4DEngine;
      FLuaType: TLuaBaseType;
      FOptions: TL4DOptions;
      procedure AssignLinkType; virtual; abstract;
    private
      FInstanceType: TL4DInstanceType;
      FOnCreate: TNotifyEvent;
      FOnDestroy: TNotifyEvent;
      procedure DoError(const AExceptionType: EL4DExceptionClass; const AMessage: String); // Delphi-side error
      function GetExceptionEvent: TL4DExceptionEvent;
      function GetGlobals: TL4DGlobalStack;
      function GetLuaExceptionEvent: TL4DLuaErrorEvent;
      function GetOptions: TL4DOptions;
      procedure SetExceptionEvent(const AValue: TL4DExceptionEvent);
      procedure SetLuaExceptionEvent(const AValue: TL4DLuaErrorEvent);
      procedure SetOptions(const AOptions: TL4DOptions);
    public
      procedure Loaded; override; // for OnCreate event call
      function InjectLuaCode(const ACode, AName: WideString; const AAutoExecute: Boolean = True): Boolean;
      function InjectLuaFile(const AFileName: WideString; const AAutoExecute: Boolean = True): Boolean;
      constructor Create(AOwner: TComponent); override;
      constructor CreateFromState(const ALuaState: PLuaState);
      destructor Destroy; override;
      property Engine: TL4DEngine read FEngine write FEngine;
      property Globals: TL4DGlobalStack read GetGlobals;
    published
      property Options: TL4DOptions read GetOptions write SetOptions;
      property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
      property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
      property OnException: TL4DExceptionEvent read GetExceptionEvent write SetExceptionEvent;
      property OnLuaError: TL4DLuaErrorEvent read GetLuaExceptionEvent write SetLuaExceptionEvent;
    end;
  {$ENDREGION}

implementation

{$REGION 'Lua Call Methods'}
  function L4D_CallDelphiFunction(L: PLuaState): Integer; cdecl;
  var
    LLua: TL4DEngine;
    LMethod: PL4DDelphiFunction;
    LMethodStack: TL4DMethodStack;
  begin
    LLua := TL4DEngine.Create(LuaLinkType, L);
    try
      LMethodStack := TL4DMethodStack.Create(LLua);
      try
        LMethodStack.FPushCount := 0;
        LMethod := PL4DDelphiFunction(LLua.FLua.lua_touserdata(LUA_GLOBALSINDEX - 1));
        TL4DDelphiFunction(LMethod^)(LMethodStack);
        Result := LMethodStack.FPushCount;
      finally
        LMethodStack.Free;
      end;
    finally
      LLua.Free;
    end;
  end;

  function L4D_CallClassFunction(L: PLuaState): Integer; cdecl;
  var
    LLua: TL4DEngine;
    LMethod: PL4DDelphiObjectFunction;
    LMethodStack: TL4DMethodStack;
  begin
    LLua := TL4DEngine.Create(LuaLinkType, L);
    try
      LMethodStack := TL4DMethodStack.Create(LLua);
      try
        LMethodStack.FPushCount := 0;
        LMethod := PL4DDelphiObjectFunction(LLua.FLua.lua_touserdata(LUA_GLOBALSINDEX - 1));
        TL4DDelphiObjectFunction(LMethod^)(LMethodStack);
        Result := LMethodStack.FPushCount;
      finally
        LMethodStack.Free;
      end;
    finally
      LLua.Free;
    end;
  end;
{$ENDREGION}

{$REGION 'TL4DMethodResultStack - Method Result Stack Type'}
  { TL4DMethodResultStack }

  procedure TL4DMethodResultStack.Cleanup;
  begin
    FLua.Pop(-FLua.FLua.lua_gettop);
  end;

  function TL4DMethodResultStack.GetCount: Integer;
  begin
    Result := FLua.FLua.lua_gettop - FOffset;
  end;

  function TL4DMethodResultStack.GetValue(const AIndex: Integer): TL4DMethodResultStackValue;
  begin
    Result.FStack := @Self;
    Result.FIndex := AIndex + FOffset;
  end;
{$ENDREGION}

{$REGION 'TL4DMethodResultStack.TL4DMethodResultStackValue - Method Result Stack Value Type'}
  { TL4DMethodResultStack.TL4DMethodResultStackValue }

  function TL4DMethodResultStack.TL4DMethodResultStackValue.CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): TL4DMethodResultStack;
  begin
    Result := FStack.FLua.CallFunction(AParameters, AResultCount);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsAnsiString: AnsiString;
  begin
    Result := FStack.FLua.GetAsAnsiString(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsBoolean: Boolean;
  begin
    Result := FStack.FLua.GetAsBoolean(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsChar: Char;
  begin
    Result := FStack.FLua.GetAsChar(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsDouble: Double;
  begin
    Result := FStack.FLua.GetAsDouble(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsExtended: Extended;
  begin
    Result := FStack.FLua.GetAsExtended(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsInteger: Integer;
  begin
    Result := FStack.FLua.GetAsInteger(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsPAnsiChar: PAnsiChar;
  begin
    Result := FStack.FLua.GetAsPAnsiChar(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsPChar: PChar;
  begin
    Result := FStack.FLua.GetAsPChar(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsPointer: Pointer;
  begin
    Result := FStack.FLua.GetAsPointer(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsSingle: Single;
  begin
    Result := FStack.FLua.GetAsSingle(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsString: String;
  begin
    Result := FStack.FLua.GetAsString(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsVariant: Variant;
  begin
    Result := FStack.FLua.GetAsVariant(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetAsWideString: WideString;
  begin
    Result := FStack.FLua.GetAsWideString(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeAnsiString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeBoolean: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtBoolean);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeDouble: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeExtended: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeInteger: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBePAnsiChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBePChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBePointer: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtLightUserData) or (LLuaType = svtUserdata);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeSingle: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeString: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeVariant: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtBoolean) or (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetCanBeWideString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetType: TL4DStackValueType;
  begin
    Result := FStack.FLua.GetLuaType(FIndex);
  end;

  function TL4DMethodResultStack.TL4DMethodResultStackValue.GetTypeName: String;
  begin
    Result := FStack.FLua.GetLuaTypeName(FIndex);
  end;
{$ENDREGION}

{$REGION 'TL4DMethodStack - Method Stack Type'}
{ TL4DMethodStack }

  function TL4DMethodStack.GetValue(const AIndex: Integer): TL4DMethodStackValue;
  begin
    Result.FIndex := AIndex;
    Result.FStack := Self;
  end;

  procedure TL4DMethodStack.PushAnsiChar(const AValue: AnsiChar);
  begin
    FLua.PushAnsiChar(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushAnsiString(const AValue: AnsiString);
  begin
    FLua.PushAnsiString(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushBoolean(const AValue: Boolean);
  begin
    FLua.PushBoolean(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushChar(const AValue: Char);
  begin
    FLua.PushChar(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushDouble(const AValue: Double);
  begin
    FLua.PushDouble(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushExtended(const AValue: Extended);
  begin
    FLua.PushExtended(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushFunction(const AValue: TLuaDelphiFunction);
  begin
    FLua.PushFunction(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushFunction(const AValue: TL4DDelphiObjectFunction);
  begin
    FLua.PushFunction(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushFunction(const AValue: TL4DDelphiFunction);
  begin
    FLua.PushFunction(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushInteger(const AValue: Integer);
  begin
    FLua.PushInteger(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushPAnsiChar(const AValue: PAnsiChar);
  begin
    FLua.PushPAnsiChar(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushPChar(const AValue: PChar);
  begin
    FLua.PushPChar(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushPointer(const AValue: Pointer);
  begin
    FLua.PushPointer(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushSingle(const AValue: Single);
  begin
    FLua.PushSingle(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushString(const AValue: WideString);
  begin
    FLua.PushString(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushVariant(const AValue: Variant);
  begin
    FLua.PushVariant(AValue);
    Inc(FPushCount);
  end;

  procedure TL4DMethodStack.PushWideString(const AValue: WideString);
  begin
    FLua.PushWideString(AValue);
    Inc(FPushCount);
  end;

  constructor TL4DMethodStack.Create(const ALua: TL4DEngine);
  begin
    inherited Create;
    FLua := ALua;
  end;

  function TL4DMethodStack.GetCount: Integer;
  begin
    Result := FLua.FLua.lua_gettop;
  end;
{$ENDREGION}

{$REGION 'TL4DMethodStack.TL4DMethodStackValue - Method Stack Value Type'}
  { TL4DMethodStack.TL4DMethodStackValue }

  function TL4DMethodStack.TL4DMethodStackValue.CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): TL4DMethodResultStack;
  begin
    Result := FStack.FLua.CallFunction(AParameters, AResultCount);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsAnsiString: AnsiString;
  begin
    Result := FStack.FLua.GetAsAnsiString(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsBoolean: Boolean;
  begin
    Result := FStack.FLua.GetAsBoolean(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsChar: Char;
  begin
    Result := FStack.FLua.GetAsChar(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsDouble: Double;
  begin
    Result := FStack.FLua.GetAsDouble(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsExtended: Extended;
  begin
    Result := FStack.FLua.GetAsExtended(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsInteger: Integer;
  begin
    Result := FStack.FLua.GetAsInteger(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsPAnsiChar: PAnsiChar;
  begin
    Result := FStack.FLua.GetAsPAnsiChar(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsPChar: PChar;
  begin
    Result := FStack.FLua.GetAsPChar(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsPointer: Pointer;
  begin
    Result := FStack.FLua.GetAsPointer(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsSingle: Single;
  begin
    Result := FStack.FLua.GetAsSingle(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsString: String;
  begin
    Result := FStack.FLua.GetAsString(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsVariant: Variant;
  begin
    Result := FStack.FLua.GetAsVariant(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetAsWideString: WideString;
  begin
    Result := FStack.FLua.GetAsWideString(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeAnsiString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeBoolean: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtBoolean);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeDouble: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeExtended: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeInteger: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBePAnsiChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBePChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBePointer: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtLightUserData) or (LLuaType = svtUserdata);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeSingle: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeString: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeVariant: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtBoolean) or (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetCanBeWideString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetType: TL4DStackValueType;
  begin
    Result := FStack.FLua.GetLuaType(FIndex);
  end;

  function TL4DMethodStack.TL4DMethodStackValue.GetTypeName: String;
  begin
    Result := FStack.FLua.GetLuaTypeName(FIndex);
  end;
{$ENDREGION}

{$REGION 'TL4DGlobalStack - Global Stack Type'}
  { TL4DGlobalStack }

  constructor TL4DGlobalStack.Create(const ALua: TL4DEngine);
  begin
    inherited Create;
    FLua := ALua;
  end;

  function TL4DGlobalStack.GetGlobal(const AKey: AnsiString): TL4DGlobalStackValue;
  begin
    Result.FKey := AKey;
    Result.FStack := Self;
    FLua.FLua.lua_getglobal(PAnsiChar(AKey));
  end;

  procedure TL4DGlobalStack.PushAnsiChar(const AKey: AnsiString; const AValue: AnsiChar);
  begin
    FLua.PushAnsiChar(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushAnsiString(const AKey, AValue: AnsiString);
  begin
    FLua.PushAnsiString(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushBoolean(const AKey: AnsiString; const AValue: Boolean);
  begin
    FLua.PushBoolean(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushChar(const AKey: AnsiString; const AValue: Char);
  begin
    FLua.PushChar(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushDouble(const AKey: AnsiString; const AValue: Double);
  begin
    FLua.PushDouble(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushExtended(const AKey: AnsiString; const AValue: Extended);
  begin
    FLua.PushExtended(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiFunction);
  begin
    FLua.PushFunction(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushFunction(const AKey: AnsiString; const AValue: TLuaDelphiFunction);
  begin
    FLua.PushFunction(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiObjectFunction);
  begin
    FLua.PushFunction(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushInteger(const AKey: AnsiString; const AValue: Integer);
  begin
    FLua.PushInteger(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushPAnsiChar(const AKey: AnsiString; const AValue: PAnsiChar);
  begin
    FLua.PushPAnsiChar(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushPChar(const AKey: AnsiString; const AValue: PChar);
  begin
    FLua.PushPChar(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushPointer(const AKey: AnsiString; const AValue: Pointer);
  begin
    FLua.PushPointer(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushSingle(const AKey: AnsiString; const AValue: Single);
  begin
    FLua.PushSingle(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushString(const AKey: AnsiString; const AValue: WideString);
  begin
    FLua.PushString(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushVariant(const AKey: AnsiString; const AValue: Variant);
  begin
    FLua.PushVariant(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.PushWideString(const AKey: AnsiString; const AValue: WideString);
  begin
    FLua.PushWideString(AValue);
    SetGlobal(AKey);
  end;

  procedure TL4DGlobalStack.SetGlobal(const AKey: AnsiString);
  begin
      FLua.FLua.lua_setglobal(PAnsiChar(AKey));
      FLua.Pop(1);
  end;
{$ENDREGION}

{$REGION 'TL4DGlobalStack.TL4DGlobalStackValue - Global Stack Value Type'}
  { TL4DGlobalStack.TL4DGlobalStackValue }

  function TL4DGlobalStack.TL4DGlobalStackValue.CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): TL4DMethodResultStack;
  begin
    Result := FStack.FLua.CallFunction(AParameters, AResultCount);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.Delete;
  begin
    FStack.FLua.Remove(-1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsAnsiString: AnsiString;
  begin
    Result := FStack.FLua.GetAsAnsiString(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsBoolean: Boolean;
  begin
    Result := FStack.FLua.GetAsBoolean(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsChar: Char;
  begin
    Result := FStack.FLua.GetAsChar(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsDouble: Double;
  begin
    Result := FStack.FLua.GetAsDouble(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsExtended: Extended;
  begin
    Result := FStack.FLua.GetAsExtended(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsInteger: Integer;
  begin
    Result := FStack.FLua.GetAsInteger(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsPAnsiChar: PAnsiChar;
  begin
    Result := FStack.FLua.GetAsPAnsiChar(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsPChar: PChar;
  begin
    Result := FStack.FLua.GetAsPChar(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsPointer: Pointer;
  begin
    Result := FStack.FLua.GetAsPointer(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsSingle: Single;
  begin
    Result := FStack.FLua.GetAsSingle(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsString: String;
  begin
    Result := FStack.FLua.GetAsString(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsVariant: Variant;
  begin
    Result := FStack.FLua.GetAsVariant(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetAsWideString: WideString;
  begin
    Result := FStack.FLua.GetAsWideString(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetType: TL4DStackValueType;
  begin
    Result := FStack.FLua.GetLuaType(-1);
  end;

  function TL4DGlobalStack.TL4DGlobalStackValue.GetTypeName: String;
  begin
    Result := FStack.FLua.GetLuaTypeName(-1);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetAnsiString(const AValue: AnsiString);
  begin
    FStack.PushAnsiString(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetBoolean(const AValue: Boolean);
  begin
    FStack.PushBoolean(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetChar(const AValue: Char);
  begin
    FStack.PushChar(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetDouble(const AValue: Double);
  begin
    FStack.PushDouble(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetExtended(const AValue: Extended);
  begin
    FStack.PushExtended(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetInteger(const AValue: Integer);
  begin
    FStack.PushInteger(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetPAnsiChar(const AValue: PAnsiChar);
  begin
    FStack.PushPAnsiChar(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetPChar(const AValue: PChar);
  begin
    FStack.PushPChar(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetPointer(const AValue: Pointer);
  begin
    FStack.PushPointer(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetSingle(const AValue: Single);
  begin
    FStack.PushSingle(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetString(const AValue: String);
  begin
    FStack.PushString(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetVariant(const AValue: Variant);
  begin
    FStack.PushVariant(FKey, AValue);
  end;

  procedure TL4DGlobalStack.TL4DGlobalStackValue.SetWideString(const AValue: WideString);
  begin
    FStack.PushWideString(FKey, AValue);
  end;
{$ENDREGION}

{$REGION 'TL4DEngine - Sits Lua4Delphi methods on top of Lua API'}
  { TL4DEngine }

  function TL4DEngine.CallFunction(AParameters: array of const; const AResultCount: Integer): TL4DMethodResultStack;
  var
    I: Integer;
  begin
    for I := Low(AParameters) to High(AParameters) do
      case AParameters[I].VType of
        vtInteger: PushInteger(Integer(PInteger(AParameters[I].VInteger)));
        vtBoolean: PushBoolean(AParameters[I].VBoolean);
        vtChar: PushAnsiChar(AParameters[I].VChar);
        vtExtended: PushExtended(AParameters[I].VExtended^);
        vtString: PushString(String(AParameters[I].VString));
        vtPointer: PushPointer(AParameters[I].VPointer);
        vtPChar: PushString(String(AParameters[I].VPChar));
        vtWideChar: PushString(String(AParameters[I].VWideChar));
        vtPWideChar: PushString(String(AParameters[I].VPWideChar));
        vtAnsiString: PushString(String(AParameters[I].VAnsiString^));
        vtCurrency: PushVariant(AParameters[I].VCurrency^);
        vtVariant: PushVariant(AParameters[I].VVariant^);
        vtWideString: PushString(String(AParameters[I].VWideString^));
        vtInt64: PushVariant(AParameters[I].VInt64^);
        vtUnicodeString: PushString(String(PWideChar(AParameters[I].VUnicodeString))); // GOOD
        vtObject: ; // TODO -oSimon -cL4D Core Stack : Insert Object-type Push Handler Here
        vtClass: ; // TODO -oSimon -cL4D Core Stack : Insert Class-Reference Push Handler Here
        vtInterface: ; // TODO -oSimon -cL4D Core Stack : Insert Interface-type Push Handler Here
      end;
    SafeLuaExecute(High(AParameters) + 1, AResultCount);
    // Offset Calculation
    Result.FOffset := 0;
    for I := 1 to FLua.lua_gettop do
      if GetLuaType(I) = svtNil then
        Inc(Result.FOffset)
      else
        Break;
    Result.FLua := Self;
  end;

  constructor TL4DEngine.Create(ALuaType: TLuaBaseType; const ALuaState: PLuaState);
  begin
    inherited Create;
    FDesignMode := False;
    FInstanceType := litExisting;
    LuaLinkType := ALuaType;
    FLua := ALuaType.Create(ALuaState);
    FGlobals := TL4DGlobalStack.Create(Self);
    FOptions := TL4DOptions.Create(Self);
  end;

  constructor TL4DEngine.Create(ALuaType: TLuaBaseType);
  begin
    inherited Create;
    FDesignMode := False;
    FInstanceType := litNew;
    LuaLinkType := ALuaType;
    FLua := ALuaType.Create;
    FGlobals := TL4DGlobalStack.Create(Self);
    FOptions := TL4DOptions.Create(Self);
  end;

  destructor TL4DEngine.Destroy;
  begin
    FOptions.Free;
    FGlobals.Free;
    FLua.Free;
    inherited;
  end;

  function TL4DEngine.GetAsAnsiString(const AIndex: Integer): AnsiString;
  begin
    Result := AnsiString(FLua.lua_tostring(AIndex));
  end;

  function TL4DEngine.GetAsBoolean(const AIndex: Integer): Boolean;
  begin
    Result := FLua.lua_toboolean(AIndex);
  end;

  function TL4DEngine.GetAsChar(const AIndex: Integer): Char;
  begin
    Result := GetAsString(AIndex)[1];
  end;

  function TL4DEngine.GetAsDouble(const AIndex: Integer): Double;
  begin
    Result := FLua.lua_tonumber(AIndex);
  end;

  function TL4DEngine.GetAsExtended(const AIndex: Integer): Extended;
  begin
    Result := FLua.lua_tonumber(AIndex);
  end;

  function TL4DEngine.GetAsInteger(const AIndex: Integer): Integer;
  begin
    Result := FLua.lua_tointeger(AIndex);
  end;

  function TL4DEngine.GetAsPAnsiChar(const AIndex: Integer): PAnsiChar;
  begin
    Result := FLua.lua_tostring(AIndex);
  end;

  function TL4DEngine.GetAsPChar(const AIndex: Integer): PChar;
  begin
    Result := PChar(GetAsString(AIndex));
  end;

  function TL4DEngine.GetAsPointer(const AIndex: Integer): Pointer;
  begin
    Result := FLua.lua_touserdata(AIndex);
  end;

  function TL4DEngine.GetAsSingle(const AIndex: Integer): Single;
  begin
    Result := FLua.lua_tonumber(AIndex);
  end;

  function TL4DEngine.GetAsString(const AIndex: Integer): WideString;
  begin
    Result := UTF8ToWideString(GetAsAnsiString(AIndex));
  end;

  function TL4DEngine.GetAsVariant(const AIndex: Integer): Variant;
  begin
    case GetLuaType(AIndex) of
      svtBoolean: Result := VarAsType(GetAsBoolean(AIndex), varBoolean);
      svtNumber: Result := VarAsType(GetAsDouble(AIndex), varDouble);
      svtString: Result := VarAsType(GetAsString(AIndex), varString);
      svtLightUserData,
      svtTable,
      svtFunction,
      svtUserdata,
      svtThread,
      svtNone,
      svtNil: Result := varNull;
    end;
  end;

  function TL4DEngine.GetAsWideString(const AIndex: Integer): WideString;
  begin
    Result := GetAsString(AIndex);
  end;

  function TL4DEngine.GetLuaType(const AIndex: Integer): TL4DStackValueType;
  begin
    Result := LUA_TYPES[Flua.lua_type(AIndex) + 1];
  end;

  function TL4DEngine.GetLuaTypeName(const AIndex: Integer): String;
  begin
    Result := LUA_TYPE_NAMES[GetLuaType(AIndex)];
  end;

  function TL4DEngine.LoadLuaCode(const ACode, AName: WideString; const AAutoExecute: Boolean): Boolean;
  begin
    Result := (FLua.luaL_loadbuffer(PAnsiChar(WideStringToAnsiString(ACode)), Length(ACode), PAnsiChar(WideStringToAnsiString(AName))) = 0);
    if AAutoExecute then
    begin
      Result := (SafeLuaExecute = 0);
      FLua.lua_pop(FLua.lua_gettop);
    end;
  end;

  function TL4DEngine.LoadLuaFile(const AFileName: WideString; const AAutoExecute: Boolean): Boolean;
  begin
    Result := (FLua.luaL_loadfile(PAnsiChar(WideStringToAnsiString(AFileName))) = 0);
    if AAutoExecute then
    begin
      Result := (SafeLuaExecute = 0);
      FLua.lua_pop(FLua.lua_gettop);
    end;
  end;

  procedure TL4DEngine.Pop(const ANumber: Integer);
  begin
    FLua.lua_pop(ANumber);
  end;

  procedure TL4DEngine.PushAnsiChar(const AValue: AnsiChar);
  begin
    PushAnsiString(AnsiStrinG(AValue));
  end;

  procedure TL4DEngine.PushAnsiString(const AValue: AnsiString);
  begin
    PushPAnsiChar(PAnsiChar(AValue));
  end;

  procedure TL4DEngine.PushBoolean(const AValue: Boolean);
  begin
    FLua.lua_pushboolean(AValue);
  end;

  procedure TL4DEngine.PushChar(const AValue: Char);
  begin
    PushString(String(AValue));
  end;

  procedure TL4DEngine.PushDouble(const AValue: Double);
  begin
    FLua.lua_pushnumber(AValue);
  end;

  procedure TL4DEngine.PushExtended(const AValue: Extended);
  begin
    FLua.lua_pushnumber(AValue);
  end;

  procedure TL4DEngine.PushFunction(const AValue: TL4DDelphiObjectFunction);
  var
    LMethod: PL4DDelphiObjectFunction;
//    LIndex: Integer;
  begin
    LMethod := FLua.lua_newuserdata(SizeOf(PL4DDelphiObjectFunction));
//    LIndex := Length(FMethods);
//    SetLength(FMethods, LIndex + 1);
//    FMethods[LIndex] := LMethod;
    TL4DDelphiObjectFunction(LMethod^) := AValue;
    FLua.lua_pushcclosure(L4D_CallClassFunction, 1);
  end;

  procedure TL4DEngine.PushFunction(const AValue: TLuaDelphiFunction);
  begin
    FLua.lua_pushcfunction(AValue);
  end;

  procedure TL4DEngine.PushFunction(const AValue: TL4DDelphiFunction);
  var
    LMethod: PL4DDelphiFunction;
//    LIndex: Integer;
  begin
    LMethod := FLua.lua_newuserdata(SizeOf(PL4DDelphiFunction));
//    LIndex := Length(FMethods);
//    SetLength(FMethods, LIndex + 1);
//    FMethods[LIndex] := LMethod;
    TL4DDelphiFunction(LMethod^) := AValue;
    FLua.lua_pushcclosure(L4D_CallDelphiFunction, 1);
  end;

  procedure TL4DEngine.PushInteger(const AValue: Integer);
  begin
    FLua.lua_pushinteger(AValue);
  end;

  procedure TL4DEngine.PushPAnsiChar(const AValue: PAnsiChar);
  begin
    FLua.lua_pushstring(AValue);
  end;

  procedure TL4DEngine.PushPChar(const AValue: PChar);
  begin
    PushChar(Char(AValue));
  end;

  procedure TL4DEngine.PushPointer(const AValue: Pointer);
  begin
    FLua.lua_pushlightuserdata(AValue);
  end;

  procedure TL4DEngine.PushSingle(const AValue: Single);
  begin
    FLua.lua_pushnumber(AValue);
  end;

  procedure TL4DEngine.PushString(const AValue: WideString);
  begin
    PushAnsiString(WideStringToAnsiString(AValue));
  end;

  procedure TL4DEngine.PushVariant(const AValue: Variant);
  begin
    case VarType(AValue) of
      vtInteger       : PushInteger(AValue);
      vtBoolean       : PushBoolean(AValue);
      vtExtended      : PushExtended(AValue);
      vtPChar,
      vtPWideChar,
      vtChar,
      vtWideChar,
      vtString,
      vtUnicodeString,
      vtWideString    : PushString(WideString(AValue));
      vtAnsiString    : PushAnsiString(AnsiString(AValue));
      vtCurrency      : PushDouble(AValue);
      vtVariant       : { TODO 3 -cLua Stack Management : Figure out what to do with vtVariant };
      vtInt64         : PushDouble(AValue);
    end;
  end;

  procedure TL4DEngine.PushWideString(const AValue: WideString);
  begin
    PushString(AValue);
  end;

  procedure TL4DEngine.Remove(const AIndex: Integer);
  begin
    FLua.lua_remove(AIndex);
  end;

  function TL4DEngine.SafeLuaExecute(const ANumArgs, ANumResults, AErrorFunc: Integer): Integer;
    function ProcessLuaMessage(var ATitle, AMessage: WideString; var ALine: Integer): Boolean; // Returns True for Multi-line Message
    var
      LMessage: WideString;
      LPortion: TWideStringArray;
    begin
      LMessage := GetAsString(FLua.lua_gettop);
      Result := (CountCharInWideString(LMessage, ':') = 2);
      if Result then
      begin
        LPortion := ExplodeWideString(':', LMessage, 0);
        ATitle := LPortion[0];
        ALine := StrToIntDef(LPortion[1], 0);
        AMessage := LPortion[2];
        SetLength(LPortion, 0); // Deallocate Array
      end
      else
        AMessage := LMessage;
    end;

    procedure RaiseException(const ATitle, AMessage: WideString; const ALine: Integer);
    begin
      if ALine = 0 then
        raise EL4DLuaException.CreateFmt(L4D_LUA_ERROR_ONELINE, [AMessage])
      else
        raise EL4DLuaException.CreateFmt(L4D_LUA_ERROR_MULTILINE, [ATitle, ALine, AMessage]);
    end;
  var
    LTitle, LMessage: WideString;
    LLine: Integer;
    LRaise: Boolean;
  begin
    Result := FLua.lua_pcall(ANumArgs, ANumResults, AErrorFunc);
    // Handle Lua Error Message
    if (Result <> 0) then
    begin
      if (not ProcessLuaMessage(LTitle, LMessage, LLine)) then
        LLine := 0;
      if Assigned(FOnLuaError) then
      begin
        LRaise := False;
        FOnLuaError(LTitle, LMessage, LLine, LRaise);
        if LRaise then
          RaiseException(LTitle, LMessage, LLine);
      end
      else
        RaiseException(LTitle, LMessage, LLine);
    end;
  end;
{$ENDREGION}

{$REGION 'TL4DEngineMember - An object which links back to TL4DEngine'}
  { TL4DEngineMember }
  
  constructor TL4DEngineMember.Create(const ALua: TL4DEngine);
  begin
    inherited Create;
    FLua := ALua;
  end;
{$ENDREGION}

{$REGION 'TL4DOptions - Lua4Delphi Options Type'}
  { TL4DOptions }

  constructor TL4DOptions.Create(const ALua: TL4DEngine);
  begin
    inherited;
    FLibraries := TL4DLibraryOptions.Create(FLua);
  end;

  destructor TL4DOptions.Destroy;
  begin
    FLibraries.Free;
    inherited;
  end;
{$ENDREGION}

{$REGION 'TL4DLibraryOptions - Lua4Delphi Library Options Type'}
  { TL4DLibraryOptions }

  constructor TL4DLibraryOptions.Create(const ALua: TL4DEngine);
  begin
    inherited;
      if FLua.FInstanceType = litNew then
      begin
        SetBase(True);
        SetDebug(False);
        SetIO(True);
        SetMath(True);
        SetOS(True);
        SetPackage(True);
        SetString(True);
        SetTable(True);
      end;
  end;

  function TL4DLibraryOptions.GetBase: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FBase
    else
      Result := not (FLua.Globals[LUA_COLIBNAME].LuaType = svtNil);
  end;

  function TL4DLibraryOptions.GetDebug: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FDebug
    else
      Result := not (FLua.Globals[LUA_DBLIBNAME].LuaType = svtNil);
  end;

  function TL4DLibraryOptions.GetIO: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FIO
    else
      Result := not (FLua.Globals[LUA_IOLIBNAME].LuaType = svtNil);
  end;

  function TL4DLibraryOptions.GetMath: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FMath
    else
      Result := not (FLua.Globals[LUA_MATHLIBNAME].LuaType = svtNil);
  end;

  function TL4DLibraryOptions.GetOS: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FOS
    else
      Result := not (FLua.Globals[LUA_OSLIBNAME].LuaType = svtNil);
  end;

  function TL4DLibraryOptions.GetPackage: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FPackage
    else
      Result := not (FLua.Globals[LUA_LOADLIBNAME].LuaType = svtNil);
  end;

  function TL4DLibraryOptions.GetString: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FString
    else
      Result := not (FLua.Globals[LUA_STRLIBNAME].LuaType = svtNil);
  end;

  function TL4DLibraryOptions.GetTable: Boolean;
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      Result := FTable
    else
      Result := not (FLua.Globals[LUA_TABLIBNAME].LuaType = svtNil);
  end;

  procedure TL4DLibraryOptions.SetBase(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FBase := AValue
    else begin
      if AValue = GetBase then
        Exit;
      if AValue then
        FLua.FLua.luaopen_base
      else
        FLua.FGlobals[LUA_COLIBNAME].Delete;
    end;
  end;

  procedure TL4DLibraryOptions.SetDebug(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FDebug := AValue
    else begin
      if AValue = GetDebug then
        Exit;
      if AValue then
        FLua.FLua.luaopen_debug
      else
        FLua.FGlobals[LUA_DBLIBNAME].Delete;
    end;
  end;

  procedure TL4DLibraryOptions.SetIO(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FIO := AValue
    else begin
      if AValue = GetIO then
        Exit;
      if AValue then
        FLua.FLua.luaopen_io
      else
        FLua.FGlobals[LUA_IOLIBNAME].Delete;
    end;
  end;

  procedure TL4DLibraryOptions.SetMath(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FMath := AValue
    else begin
      if AValue = GetMath then
        Exit;
      if AValue then
        FLua.FLua.luaopen_math
      else
        FLua.FGlobals[LUA_MATHLIBNAME].Delete;
    end;
  end;

  procedure TL4DLibraryOptions.SetOS(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FOS := AValue
    else begin
      if AValue = GetOS then
        Exit;
      if AValue then
        FLua.FLua.luaopen_os
      else
        FLua.FGlobals[LUA_OSLIBNAME].Delete;
    end;
  end;

  procedure TL4DLibraryOptions.SetPackage(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FPackage := AValue
    else begin
      if AValue = FPackage then
        Exit;
      if AValue then
        FLua.FLua.luaopen_package
      else
        FLua.FGlobals[LUA_LOADLIBNAME].Delete;
    end;
  end;

  procedure TL4DLibraryOptions.SetString(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FString := AValue
    else begin
      if AValue = GetString then
        Exit;
      if AValue then
        FLua.FLua.luaopen_string
      else
        FLua.FGlobals[LUA_STRLIBNAME].Delete;
    end;
  end;

  procedure TL4DLibraryOptions.SetTable(const AValue: Boolean);
  begin
    if (FLua.FDesignMode) or (FLua.FLua = nil) then
      FTable := AValue
    else begin
      if AValue = GetTable then
        Exit;
      if AValue then
        FLua.FLua.luaopen_table
      else
        FLua.FGlobals[LUA_TABLIBNAME].Delete;
    end;
  end;
{$ENDREGION}

{$REGION 'TLua4DelphiCommon - The Base Component for Lua4Delphi variants'}
  { TLua4DelphiCommon }

  constructor TLua4DelphiCommon.Create(AOwner: TComponent);
  begin
    inherited;
    FInstanceType := litNew;
    AssignLinkType;
    FEngine := TL4DEngine.Create(FLuaType);
    FEngine.FDesignMode := (csDesigning in Self.ComponentState);
    FOptions := FEngine.FOptions;
  end;

  constructor TLua4DelphiCommon.CreateFromState(const ALuaState: PLuaState);
  begin
    inherited Create(nil);
    FInstanceType := litExisting;
    AssignLinkType;
    FEngine := TL4DEngine.Create(FLuaType, ALuaState);
    FEngine.FDesignMode := (csDesigning in Self.ComponentState);
    FOptions := FEngine.FOptions;
  end;
  
  destructor TLua4DelphiCommon.Destroy;
  begin
    FEngine.Free;
    inherited;
  end;
  
  procedure TLua4DelphiCommon.DoError(const AExceptionType: EL4DExceptionClass; const AMessage: String);
  var
    LRaise: Boolean;
  begin
    LRaise := False;
    if Assigned(OnException) then
    begin
      OnException(AExceptionType, AMessage, LRaise);
      if LRaise then
        raise AExceptionType.Create(AMessage);
    end else
      raise AExceptionType.Create(AMessage);
  end;
  
  function TLua4DelphiCommon.GetExceptionEvent: TL4DExceptionEvent;
  begin
    Result := FEngine.FOnException;
  end;
  
  function TLua4DelphiCommon.GetGlobals: TL4DGlobalStack;
  begin
    Result := FEngine.FGlobals;
  end;
  
  function TLua4DelphiCommon.GetLuaExceptionEvent: TL4DLuaErrorEvent;
  begin
    Result := FEngine.FOnLuaError;
  end;

  function TLua4DelphiCommon.GetOptions: TL4DOptions;
  begin
    Result := FEngine.FOptions;
  end;

function TLua4DelphiCommon.InjectLuaCode(const ACode, AName: WideString; const AAutoExecute: Boolean): Boolean;
  begin
    Result := FEngine.LoadLuaCode(ACode, AName, AAutoExecute);
  end;

  function TLua4DelphiCommon.InjectLuaFile(const AFileName: WideString; const AAutoExecute: Boolean): Boolean;
  begin
    Result := FileExists(AFileName);
    if Result then
    begin
      Result := FEngine.LoadLuaFile(AFileName, AAutoExecute);
    end else
      DoError(EL4DFileNotFound, Format(L4D_EXCEPTION_SCRIPTNOTFOUND, [AFileName]));
  end;

  procedure TLua4DelphiCommon.Loaded;
  begin
    inherited;
    if (Assigned(FOnCreate)) and (FEngine.FLua.LuaState <> nil) then
      FOnCreate(Self);
  end;

  procedure TLua4DelphiCommon.SetExceptionEvent(const AValue: TL4DExceptionEvent);
  begin
    FEngine.FOnException := AValue;
  end;

  procedure TLua4DelphiCommon.SetLuaExceptionEvent(const AValue: TL4DLuaErrorEvent);
  begin
    FEngine.FOnLuaError := AValue;
  end;

  procedure TLua4DelphiCommon.SetOptions(const AOptions: TL4DOptions);
  begin
    FEngine.FOptions := AOptions;
  end;

{$ENDREGION}

end.
