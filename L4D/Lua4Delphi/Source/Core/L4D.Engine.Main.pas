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
    12th March 2012:
      - HUGE number of bugs fixed
      - Table Reading/Writing now work properly
      - Too many changes to enumerate
    23rd February 2012:
      - Everything uses INTERFACES now!
    22nd February 2012:
      - Added TL4DEngineManager and the global instance "EngineManager"
        - This addresses a big bug where Methods pushed back to Lua from another Method lose their reference
          when their Stack Manager and Engine Instance are subsequently Freed.
        - Not yet tested with Multithreading, but don't worry if it causes deadlock as full threading support
          is a major focus of this development. It shouldn't be an issue if the Lua Engine Instance is created
          before the Thread's Execute method begins.
      - Nested Table Writing now works (Tables within Tables, n-tier)
    16th February 2012:
      - Completed TL4DTable
      - Completed TL4DTableValue
    13th February 2012:
      - Implemented TL4DTable (incomplete)
      - Added TL4DTableValue (incomplete)
      - Added "NewTable" method to all Stack Managers
      - Added "AsTable" to all Stack Value Types
    11th February 2012:
      - Added "Engine" property to TL4DEngineMember
    10th February 2012:
      - Made all Stack Managers inherit from TL4DEngineMember instead of TPersistent
      - Changed TL4DMethodResultStack from Record to Class
    8th February 2012:
      - Added "Sender: TObject" parameter to TL4DExceptionEvent and TL4DLuaErrorEvent event method types
    7th February 2012:
      - Added TL4DTable class (not implemented yet)
      - Fixed some bugs which could crash the IDE
    6th February 2012:
      - Methods and Object Methods now use a "Container Object" in an Array.
      - Fixed bug with reading returned values in Delphi
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
  LKSL.Managed.Arrays,
  LKSL.Strings.Conversion, LKSL.Strings.Utils,
  L4D.Lua.Intf, L4D.Lua.Common, L4D.Engine.Constants, L4D.Engine.MainIntf;

type
  { Forward Declarations }
  TL4DEngineMember = class;
  TL4DMethod = class;
  TL4DMethodObject = class;
  TL4DTableValue = class;
  TL4DTable = class;
  TL4DMethodResultStackValue = class;
  TL4DMethodResultStack = class;
  TL4DMethodStackValue = class;
  TL4DMethodStack = class;
  TL4DGlobalStackValue = class;
  TL4DGlobalStack = class;
  TL4DEngineManager = class;
  TL4DEngine = class;
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
  TL4DExceptionEvent = procedure(Sender: TObject; const AExceptionType: EL4DExceptionClass; AMessage: String; var ARaise: Boolean) of object;
  TL4DLuaErrorEvent = procedure(Sender: TObject; const ATitle, AMessage: String; const ALine: Integer; var ARaise: Boolean) of object;

  { Array Types }
  TL4DMethodArray = Array of TL4DMethod;
  TL4DMethodObjectArray = Array of TL4DMethodObject;
  TL4DEngineArray = Array of TL4DEngine;

  { Pointer Types }
  PL4DMethod = ^TL4DMethod;
  PL4DMethodObject = ^TL4DMethodObject;

  {
    TL4DEngineMember
      - An object which links back to TL4DEngine
  }
  {$REGION 'TL4DEngineMember - An object which links back to TL4DEngine'}
    TL4DEngineMember = class(TInterfacedObject, IL4DEngineMember)
    protected
      FLua: TL4DEngine;
      function GetEngine: IL4DEngine;
    public
      constructor Create(const ALua: TL4DEngine); virtual;
      property Engine: IL4DEngine read GetEngine;
    end;
  {$ENDREGION}

  {
    TL4DMethod
      - A container for Delphi Methods registered with Lua
  }
  {$REGION 'TL4DMethod - A container for Delphi Methods registered with Lua'}
    TL4DMethod = class(TInterfacedObject, IL4DMethod)
    private
      FMethod: TL4DDelphiFunction;
    public
      constructor Create(const AMethod: TL4DDelphiFunction);
      property Method: TL4DDelphiFunction read FMethod;
    end;
  {$ENDREGION}

  {
    TL4DMethodObject
    - A container for Object-bound Delphi Methods registered with Lua
  }
  {$REGION 'TL4DMethodObject - A container for Object-bound Delphi Methods registered with Lua'}
    TL4DMethodObject = class(TInterfacedObject, IL4DMethodObject)
    private
      FMethod: TL4DDelphiObjectFunction;
    public
      constructor Create(const AMethod: TL4DDelphiObjectFunction);
      property Method: TL4DDelphiObjectFunction read FMethod;
    end;
  {$ENDREGION}

  {
    TL4DTableValue
      - A reader for Lua Table/Metatable Values
  }
  {$REGION 'TL4DTableValue - A reader for Lua Table/Metatable Values'}
    TL4DTableValue = class(TInterfacedObject, IL4DTableValue)
    private
      FIndex: Integer;
      FKey: AnsiString;
      FStack: TL4DTable;
      {$REGION 'IL4DTableValue'}
        procedure SetIndex(const AIndex: Integer);
        procedure SetKey(const AKey: AnsiString);
      {$ENDREGION}
      {$REGION 'IL4DStackValue'}
        function GetAsAnsiString: AnsiString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsChar: Char; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsDouble: Double; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsExtended: Extended; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsInteger: Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPAnsiChar: PAnsiChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPChar: PChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPointer: Pointer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsSingle: Single; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsString: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsTable: IL4DTable; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsVariant: Variant; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsWideString: WideString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeAnsiString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeDouble: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeExtended: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeInteger: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePAnsiChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePointer: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeSingle: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeTable: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeVariant: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeWideString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetType: TL4DStackValueType; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetTypeName: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      public
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
      {$ENDREGION}
      constructor Create(const AStack: TL4DTable; const AIndex: Integer);
    end;
  {$ENDREGION}

  {
    TL4DTable
      - Composer/Reader for Lua Tables/Metatables
      CRITICAL: DO NOT MODIFY THIS TYPE IN ANY WAY!
  }
  {$REGION 'TL4DTable - Composer/Reader for Lua Tables/Metatables'}
    TL4DTable = class(TL4DEngineMember, IL4DTable)
    private
      FClosed: Boolean;
      FFieldIndex: Integer;
      FIndex: Integer;
      FName: AnsiString;
      FNew: Boolean;
      FOnPushTable: TL4DTableChangeEvent;
      FTableSet: Boolean;
      procedure OnPushTable(const ATable: IL4DTable);
      {$REGION 'IL4DStack'}
        function GetCount: Integer;
      {$ENDREGION}
      {$REGION 'IL4DTable'}
        function GetName: AnsiString;
        function GetValueByIndex(const AIndex: Integer): IL4DTableValue;
        function GetValueByName(const AName: AnsiString): IL4DTableValue;
        procedure PushTable; inline;
        procedure SetIndex(const AIndex: Integer);
        procedure SetName(const AName: AnsiString);
        procedure SetOnPushTable(const AEvent: TL4DTableChangeEvent);
      {$ENDREGION}
    public
      constructor Create(const ALua: TL4DEngine; const ANew: Boolean = False); reintroduce;
      destructor Destroy; override;
      {$REGION 'IL4DStackIndexed'}
        function NewTable: IL4DTable; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiChar(const AValue: AnsiChar); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiString(const AValue: AnsiString); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushBoolean(const AValue: Boolean); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushChar(const AValue: Char); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushDouble(const AValue: Double); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushExtended(const AValue: Extended); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushFunction(const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
        procedure PushFunction(const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
        procedure PushFunction(const AValue: TLuaDelphiFunction); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF} // Standard Lua "C" Function
        procedure PushInteger(const AValue: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPAnsiChar(const AValue: PAnsiChar); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPChar(const AValue: PChar); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPointer(const AValue: Pointer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushSingle(const AValue: Single); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushString(const AValue: WideString); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushVariant(const AValue: Variant); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushWideString(const AValue: WideString); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      {$ENDREGION}
      {$REGION 'IL4DStackKeyed'}
        function NewTable(const AKey: AnsiString): IL4DTable; overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiChar(const AKey: AnsiString; const AValue: AnsiChar); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiString(const AKey: AnsiString; const AValue: AnsiString); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushBoolean(const AKey: AnsiString; const AValue: Boolean); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushChar(const AKey: AnsiString; const AValue: Char); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushDouble(const AKey: AnsiString; const AValue: Double); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushExtended(const AKey: AnsiString; const AValue: Extended); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
        procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
        procedure PushFunction(const AKey: AnsiString; const AValue: TLuaDelphiFunction); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF} // Standard Lua "C" Function
        procedure PushInteger(const AKey: AnsiString; const AValue: Integer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPAnsiChar(const AKey: AnsiString; const AValue: PAnsiChar); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPChar(const AKey: AnsiString; const AValue: PChar); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPointer(const AKey: AnsiString; const AValue: Pointer); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushSingle(const AKey: AnsiString; const AValue: Single); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushString(const AKey: AnsiString; const AValue: WideString); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushVariant(const AKey: AnsiString; const AValue: Variant); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushWideString(const AKey: AnsiString; const AValue: WideString); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      {$ENDREGION}
      {$REGION 'IL4DStack'}
        property Count: Integer read GetCount;
      {$ENDREGION}
      {$REGION 'IL4DTable'}
        procedure Close;
        procedure Push;
        property Value[const AIndex: Integer]: IL4DTableValue read GetValueByIndex; default;
        property Value[const AName: AnsiString]: IL4DTableValue read GetValueByName; default;
      {$ENDREGION}
    end;
  {$ENDREGION}

  {
    TL4DMethodResultStackValue
  }
  {$REGION 'TL4DMethodResultStackValue - Method Result Stack Value Type'}
    TL4DMethodResultStackValue = class(TInterfacedObject, IL4DMethodResultStackValue)
    private
      FIndex: Integer;
      FStack: TL4DMethodResultStack;
      {$REGION 'IL4DMethodResultStackValue'}
        procedure SetIndex(const AIndex: Integer);
      {$ENDREGION}
      {$REGION 'IL4DStackValue'}
        function GetAsAnsiString: AnsiString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsChar: Char; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsDouble: Double; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsExtended: Extended; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsInteger: Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPAnsiChar: PAnsiChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPChar: PChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPointer: Pointer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsSingle: Single; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsString: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsTable: IL4DTable; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsVariant: Variant; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsWideString: WideString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeAnsiString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeDouble: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeExtended: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeInteger: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePAnsiChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePointer: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeSingle: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeTable: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeVariant: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeWideString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetType: TL4DStackValueType; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetTypeName: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      public
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
      {$ENDREGION}
      constructor Create(const AStack: TL4DMethodResultStack; const AIndex: Integer);
    end;
  {$ENDREGION}

  {
    TL4DMethodResultStack
  }
  {$REGION 'TL4DMethodResultStack - Method Result Stack Type'}
    TL4DMethodResultStack = class(TL4DEngineMember, IL4DMethodResultStack)
    private
      FCleanedUp: Boolean;
      function GetCount: Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function GetValue(const AIndex: Integer): IL4DMethodResultStackValue; {inline;}
    public
      constructor Create(const ALua: TL4DEngine); override;
      destructor Destroy; override;
      procedure Cleanup;
      property Count: Integer read GetCount;
      property Value[const AIndex: Integer]: IL4DMethodResultStackValue read GetValue; default;
    end;
  {$ENDREGION}

  {
    TL4DMethodStackValue
  }
  {$REGION 'TL4DMethodStackValue - Method Stack Value Type'}
    TL4DMethodStackValue = class(TInterfacedObject, IL4DMethodStackValue)
    private
      FIndex: Integer;
      FStack: TL4DMethodStack;
      {$REGION 'IL4DStackValue'}
        // Getters
        function GetAsAnsiString: AnsiString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsChar: Char; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsDouble: Double; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsExtended: Extended; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsInteger: Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPAnsiChar: PAnsiChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPChar: PChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPointer: Pointer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsSingle: Single; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsString: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsTable: IL4DTable; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsVariant: Variant; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsWideString: WideString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeAnsiString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeDouble: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeExtended: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeInteger: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePAnsiChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePointer: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeSingle: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeTable: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeVariant: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeWideString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetType: TL4DStackValueType; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetTypeName: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      public
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
      {$ENDREGION}
      constructor Create(const AStack: TL4DMethodStack; const AIndex: Integer);
    end;
  {$ENDREGION}

  {
    TL4DMethodStack
  }
  {$REGION 'TL4DMethodStack - Method Stack Type'}
    TL4DMethodStack = class(TL4DEngineMember, IL4DMethodStack)
    private
      FPushCount: Integer;
      function GetCount: Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function GetPushCount: Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      function GetValue(const AIndex: Integer): IL4DMethodStackValue; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
    public
      constructor Create(const ALua: TL4DEngine); override;
      function NewTable: IL4DTable; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushAnsiChar(const AValue: AnsiChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushAnsiString(const AValue: AnsiString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushBoolean(const AValue: Boolean); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushChar(const AValue: Char); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushDouble(const AValue: Double); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushExtended(const AValue: Extended); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushFunction(const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
      procedure PushFunction(const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
      procedure PushFunction(const AValue: TLuaDelphiFunction); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF} // Standard Lua "C" Function
      procedure PushInteger(const AValue: Integer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushPAnsiChar(const AValue: PAnsiChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushPChar(const AValue: PChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushPointer(const AValue: Pointer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushSingle(const AValue: Single); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushString(const AValue: WideString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushVariant(const AValue: Variant); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      procedure PushWideString(const AValue: WideString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      property Count: Integer read GetCount;
      property Value[const AIndex: Integer]: IL4DMethodStackValue read GetValue; default;
    end;
  {$ENDREGION}

  {
    TL4DGlobalStackValue
  }
  {$REGION 'TL4DGlobalStackValue - Global Stack Value Type'}
    TL4DGlobalStackValue = class(TInterfacedObject, IL4DGlobalStackValue)
    private
      FKey: AnsiString;
      FStack: TL4DGlobalStack;
      {$REGION 'IL4DStackValue'}
        // Getters
        function GetAsAnsiString: AnsiString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsChar: Char; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsDouble: Double; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsExtended: Extended; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsInteger: Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPAnsiChar: PAnsiChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPChar: PChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPointer: Pointer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsSingle: Single; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsString: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsTable: IL4DTable; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsVariant: Variant; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsWideString: WideString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetType: TL4DStackValueType; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetTypeName: String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        // Type Check
        function GetCanBeAnsiString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeBoolean: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeDouble: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeExtended: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeInteger: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePAnsiChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePChar: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBePointer: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeSingle: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeTable: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeVariant: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetCanBeWideString: Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        // Setters
        procedure SetAnsiString(const AValue: AnsiString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetBoolean(const AValue: Boolean); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetChar(const AValue: Char); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetDouble(const AValue: Double); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetExtended(const AValue: Extended); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetInteger(const AValue: Integer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetPAnsiChar(const AValue: PAnsiChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetPChar(const AValue: PChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetPointer(const AValue: Pointer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetSingle(const AValue: Single); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetString(const AValue: String); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetVariant(const AValue: Variant); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetWideString(const AValue: WideString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      public
        procedure Delete; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
      {$ENDREGION}
      constructor Create(const AStack: TL4DGlobalStack; const AKey: AnsiString);
    end;
  {$ENDREGION}

  {
    TL4DGlobalStack
      - Manages the stack for Globals
  }
  {$REGION 'TL4DGlobalStack - Global Stack Type'}
    TL4DGlobalStack = class(TL4DEngineMember, IL4DGlobalStack, IL4DStackKeyed)
    private
      {$REGION 'IL4DStack'}
        function GetCount: Integer;
        procedure OnPushTable(const ATable: IL4DTable);
      {$ENDREGION}
      {$REGION 'IL4DGlobalStack'}
        function GetGlobal(const AKey: AnsiString): IL4DGlobalStackValue; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure SetGlobal(const AKey: AnsiString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
    public
        property Value[const AKey: AnsiString]: IL4DGlobalStackValue read GetGlobal; default;
      {$ENDREGION}
    public
      {$REGION 'IL4DStackKeyed'}
        function NewTable(const AKey: AnsiString): IL4DTable; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiChar(const AKey: AnsiString; const AValue: AnsiChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiString(const AKey: AnsiString; const AValue: AnsiString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushBoolean(const AKey: AnsiString; const AValue: Boolean); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushChar(const AKey: AnsiString; const AValue: Char); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushDouble(const AKey: AnsiString; const AValue: Double); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushExtended(const AKey: AnsiString; const AValue: Extended); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiFunction); overload;  // Stack-Managed Delphi Procedure
        procedure PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiObjectFunction); overload;  // Stack-Managed Delphi Object Procedure
        procedure PushFunction(const AKey: AnsiString; const AValue: TLuaDelphiFunction); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF} // Standard Lua "C" Function
        procedure PushInteger(const AKey: AnsiString; const AValue: Integer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPAnsiChar(const AKey: AnsiString; const AValue: PAnsiChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPChar(const AKey: AnsiString; const AValue: PChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPointer(const AKey: AnsiString; const AValue: Pointer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushSingle(const AKey: AnsiString; const AValue: Single); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushString(const AKey: AnsiString; const AValue: WideString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushVariant(const AKey: AnsiString; const AValue: Variant); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushWideString(const AKey: AnsiString; const AValue: WideString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      {$ENDREGION}
    end;
  {$ENDREGION}

  {
    TL4DEngineManager
      - A simple Array to hold TL4DEngine instances. Needed to prevent lost Method and Object references!
      CRITICAL: DO NOT MODIFY THIS TYPE IN ANY WAY!
  }
  {$REGION 'TL4DEngineManager - A simple Array to hold TL4DEngine instances'}
    TL4DEngineManager = class(TPersistent)
    private
      FEngines: TL4DEngineArray;
      function GetEngineIdByState(const ALuaState: PLuaState): Integer;
      procedure DeleteItemByIndex(const AIndex: Integer);
    public
      destructor Destroy; override;
      procedure AddExistingEngine(const AEngine: TL4DEngine);
      procedure DeleteItem(const ALuaState: PLuaState);
      function EngineExists(const ALuaState: PLuaState): Boolean;
      function GetEngineByState(const ALuaState: PLuaState; const ACreateIfNecessary: Boolean = True): TL4DEngine;
    end;
  {$ENDREGION}

  {
    TL4DEngine
      - Sits Lua4Delphi methods on top of Lua API
      CRITICAL: DO NOT MODIFY THIS TYPE IN ANY WAY!
  }
  {$REGION 'TL4DEngine - Sits Lua4Delphi methods on top of Lua API'}
    TL4DEngine = class(TInterfacedObject, IL4DEngine)
    private
      FDesignMode: Boolean;
      FGlobals: TL4DGlobalStack;
      FLua: TLuaCommon;
      FInstanceType: TL4DInstanceType;
      FMethods: TL4DMethodArray;
      FObjectMethods: TL4DMethodObjectArray;
      FOptions: TL4DOptions;
      FOnException: TL4DExceptionEvent;
      FOnLuaError: TL4DLuaErrorEvent;
      {$REGION 'IL4DEngine'}
        // Lua4Delphi Custom Macros
        function CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
        function GetAsAnsiString(const AIndex: Integer): AnsiString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsBoolean(const AIndex: Integer): Boolean; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsChar(const AIndex: Integer): Char; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsDouble(const AIndex: Integer): Double; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsExtended(const AIndex: Integer): Extended; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsInteger(const AIndex: Integer): Integer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPAnsiChar(const AIndex: Integer): PAnsiChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPChar(const AIndex: Integer): PChar; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsPointer(const AIndex: Integer): Pointer; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsSingle(const AIndex: Integer): Single; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsString(const AIndex: Integer): WideString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsTable(const AIndex: Integer): IL4DTable; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsWideString(const AIndex: Integer): WideString; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function GetAsVariant(const AIndex: Integer): Variant; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function LoadLuaCode(const ACode, AName: WideString; const AAutoExecute: Boolean = True): Boolean;
        function LoadLuaFile(const AFileName: WideString; const AAutoExecute: Boolean = True): Boolean;
        function NewTable: IL4DTable;
        procedure Pop(const ANumber: Integer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiChar(const AValue: AnsiChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushAnsiString(const AValue: AnsiString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushBoolean(const AValue: Boolean); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushChar(const AValue: Char); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushDouble(const AValue: Double); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushExtended(const AValue: Extended); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushFunction(const AValue: TL4DDelphiFunction); overload;         // Stack-Managed Delphi Procedure
        procedure PushFunction(const AValue: TL4DDelphiObjectFunction); overload;   // Stack-Managed Delphi Object Procedure
        procedure PushFunction(const AValue: TLuaDelphiFunction); overload; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF} // Standard Lua "C" Function
        procedure PushInteger(const AValue: Integer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPAnsiChar(const AValue: PAnsiChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPChar(const AValue: PChar); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushPointer(const AValue: Pointer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushSingle(const AValue: Single); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushString(const AValue: WideString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushVariant(const AValue: Variant); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure PushWideString(const AValue: WideString); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        procedure Remove(const AIndex: Integer); {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
        function SafeLuaExecute(const ANumArgs: Integer = 0; const ANumResults: Integer = 0; const AErrorFunc: Integer = 0): Integer; // Handles exceptions when executing Lua code
      {$ENDREGION}
    public
      // Base Methods
      constructor Create(ALuaType: TLuaBaseType); overload;
      constructor Create(ALuaType: TLuaBaseType; const ALuaState: PLuaState); overload;
      destructor Destroy; override;
      {$REGION 'IL4DEngine'}
        function GetLuaType(const AIndex: Integer): TL4DStackValueType; //inline;
        function GetLuaTypeName(const AIndex: Integer): String; {$IFDEF L4D_USE_INLINE}inline;{$ENDIF}
      {$ENDREGION}
      property Globals: TL4DGlobalStack read FGlobals write FGlobals;
      property Lua: TLuaCommon read FLua write FLua;
      property Options: TL4DOptions read FOptions write FOptions;
      property OnException: TL4DExceptionEvent read FOnException write FOnException;
      property OnLuaError: TL4DLuaErrorEvent read FOnLuaError write FOnLuaError;
    end;
  {$ENDREGION}

  {
    TL4DOptions
      - Contains our various Instance Options
  }
  {$REGION 'TL4DOptions - Lua4Delphi Options Type'}
    TL4DOptions = class(TPersistent)
    private
      FLibraries: TL4DLibraryOptions;
      FLua: TL4DEngine;
    public
      constructor Create(const ALua: TL4DEngine);
      destructor Destroy; override;
      property Engine: TL4DEngine read FLua write FLua;
    published
      property Libraries: TL4DLibraryOptions read FLibraries write FLibraries;
    end;
  {$ENDREGION}

  {
    TL4DLibraries
      - Object containing Boolean Switches for the various Lua Libraries
  }
  {$REGION 'TL4DLibraryOptions - Lua4Delphi Library Options Type'}
    TL4DLibraryOptions = class(TPersistent)
    private
      FLua: TL4DEngine;
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
      constructor Create(const ALua: TL4DEngine);
      property Engine: TL4DEngine read FLua;
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

var
  EngineManager: TL4DEngineManager;

{$REGION 'Lua Call Methods'}
  function L4D_CallDelphiFunction(L: PLuaState): Integer; cdecl;
  var
    LLua: TL4DEngine;
    LMethod: PL4DMethod;
    LMethodStack: IL4DMethodStack;
  begin
    LLua := EngineManager.GetEngineByState(L);
    LMethodStack := TL4DMethodStack.Create(LLua);
    LMethod := PL4DMethod(LLua.FLua.lua_touserdata(LUA_GLOBALSINDEX - 1));
    LMethod^.FMethod(LMethodStack);
    Result := LMethodStack.GetPushCount;
  end;

  function L4D_CallClassFunction(L: PLuaState): Integer; cdecl;
  var
    LLua: TL4DEngine;
    LMethod: PL4DMethodObject;
    LMethodStack: IL4DMethodStack;
  begin
    LLua := EngineManager.GetEngineByState(L);
    LMethodStack := TL4DMethodStack.Create(LLua);
    LMethod := PL4DMethodObject(LLua.FLua.lua_touserdata(LUA_GLOBALSINDEX - 1));
    LMethod^.FMethod(LMethodStack);
    Result := LMethodStack.GetPushCount;
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

{$REGION 'TL4DMethod - A container for Delphi Methods registered with Lua'}
  function TL4DEngineMember.GetEngine: IL4DEngine;
  begin
    Result := FLua;
  end;

{ TL4DMethod }

  constructor TL4DMethod.Create(const AMethod: TL4DDelphiFunction);
  begin
    inherited Create;
    FMethod := AMethod;
  end;
{$ENDREGION}

{$REGION 'TL4DMethodObject - A container for Object-bound Delphi Methods registered with Lua'}
  { TL4DMethodObject }

  constructor TL4DMethodObject.Create(const AMethod: TL4DDelphiObjectFunction);
  begin
    inherited Create;
    FMethod := AMethod;
  end;
{$ENDREGION}

{$REGION 'TL4DMethodResultStackValue - Method Result Stack Value Type'}
  { TL4DMethodResultStackValue }

  function TL4DMethodResultStackValue.CallFunction(AParameters: array of const; const AResultCount: Integer): IL4DMethodResultStack;
  begin
    Result := FStack.FLua.CallFunction(AParameters, AResultCount);
  end;

  constructor TL4DMethodResultStackValue.Create(const AStack: TL4DMethodResultStack; const AIndex: Integer);
  begin
    inherited Create;
    FStack := AStack;
    FIndex := AIndex;
  end;

  function TL4DMethodResultStackValue.GetAsAnsiString: AnsiString;
  begin
    Result := FStack.FLua.GetAsAnsiString(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsBoolean: Boolean;
  begin
    Result := FStack.FLua.GetAsBoolean(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsChar: Char;
  begin
    Result := FStack.FLua.GetAsChar(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsDouble: Double;
  begin
    Result := FStack.FLua.GetAsDouble(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsExtended: Extended;
  begin
    Result := FStack.FLua.GetAsExtended(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsInteger: Integer;
  begin
    Result := FStack.FLua.GetAsInteger(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsPAnsiChar: PAnsiChar;
  begin
    Result := FStack.FLua.GetAsPAnsiChar(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsPChar: PChar;
  begin
    Result := FStack.FLua.GetAsPChar(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsPointer: Pointer;
  begin
    Result := FStack.FLua.GetAsPointer(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsSingle: Single;
  begin
    Result := FStack.FLua.GetAsSingle(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsString: String;
  begin
    Result := FStack.FLua.GetAsString(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsTable: IL4DTable;
  begin
    Result := FStack.FLua.GetAsTable(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsVariant: Variant;
  begin
    Result := FStack.FLua.GetAsVariant(FIndex);
  end;

  function TL4DMethodResultStackValue.GetAsWideString: WideString;
  begin
    Result := FStack.FLua.GetAsWideString(FIndex);
  end;

  function TL4DMethodResultStackValue.GetCanBeAnsiString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStackValue.GetCanBeBoolean: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtBoolean);
  end;

  function TL4DMethodResultStackValue.GetCanBeChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStackValue.GetCanBeDouble: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStackValue.GetCanBeExtended: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStackValue.GetCanBeInteger: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStackValue.GetCanBePAnsiChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStackValue.GetCanBePChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStackValue.GetCanBePointer: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtLightUserData) or (LLuaType = svtUserdata);
  end;

  function TL4DMethodResultStackValue.GetCanBeSingle: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodResultStackValue.GetCanBeString: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodResultStackValue.GetCanBeTable: Boolean;
  begin
    Result := (GetType = svtTable);
  end;

  function TL4DMethodResultStackValue.GetCanBeVariant: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtBoolean) or (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodResultStackValue.GetCanBeWideString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodResultStackValue.GetType: TL4DStackValueType;
  begin
    Result := FStack.FLua.GetLuaType(FIndex);
  end;

  function TL4DMethodResultStackValue.GetTypeName: String;
  begin
    Result := FStack.FLua.GetLuaTypeName(FIndex);
  end;

  procedure TL4DMethodResultStackValue.SetIndex(const AIndex: Integer);
  begin
    FIndex := AIndex;
  end;

{$ENDREGION}

{$REGION 'TL4DMethodResultStack - Method Result Stack Type'}
  { TL4DMethodResultStack }

  procedure TL4DMethodResultStack.Cleanup;
  begin
    FLua.Pop(FLua.FLua.lua_gettop);
    FCleanedUp := True;
  end;

  constructor TL4DMethodResultStack.Create(const ALua: TL4DEngine);
  begin
    inherited;
    FCleanedUp := False;
  end;

  destructor TL4DMethodResultStack.Destroy;
  begin
    if not (FCleanedUp) then
      Cleanup;
    inherited;
  end;

  function TL4DMethodResultStack.GetCount: Integer;
  begin
    Result := FLua.FLua.lua_gettop;
  end;

  function TL4DMethodResultStack.GetValue(const AIndex: Integer): IL4DMethodResultStackValue;
  begin
    Result := TL4DMethodResultStackValue.Create(Self, AIndex);
  end;

{$ENDREGION}

{$REGION 'TL4DTableValue - A reader for Lua Table/Metatable Values'}
  { TL4DTableValue }

  function TL4DTableValue.CallFunction(AParameters: array of const; const AResultCount: Integer): IL4DMethodResultStack;
  begin
    Result := FStack.FLua.CallFunction(AParameters, AResultCount);
  end;

  constructor TL4DTableValue.Create(const AStack: TL4DTable; const AIndex: Integer);
  begin
    inherited Create;
    FStack := AStack;
    FIndex := AIndex;
  end;

  function TL4DTableValue.GetAsAnsiString: AnsiString;
  begin
    Result := FStack.FLua.GetAsAnsiString(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsBoolean: Boolean;
  begin
    Result := FStack.FLua.GetAsBoolean(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsChar: Char;
  begin
    Result := FStack.FLua.GetAsChar(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsDouble: Double;
  begin
    Result := FStack.FLua.GetAsDouble(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsExtended: Extended;
  begin
    Result := FStack.FLua.GetAsExtended(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsInteger: Integer;
  begin
    Result := FStack.FLua.GetAsInteger(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsPAnsiChar: PAnsiChar;
  begin
    Result := FStack.FLua.GetAsPAnsiChar(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsPChar: PChar;
  begin
    Result := FStack.FLua.GetAsPChar(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsPointer: Pointer;
  begin
    Result := FStack.FLua.GetAsPointer(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsSingle: Single;
  begin
    Result := FStack.FLua.GetAsSingle(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsString: String;
  begin
    Result := FStack.FLua.GetAsString(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsTable: IL4DTable;
  begin
    Result := FStack.FLua.GetAsTable(FIndex);
  end;

  function TL4DTableValue.GetAsVariant: Variant;
  begin
    Result := FStack.FLua.GetAsVariant(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetAsWideString: WideString;
  begin
    Result := FStack.FLua.GetAsWideString(FIndex);
    FStack.FLua.Pop(1);
  end;

  function TL4DTableValue.GetCanBeAnsiString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DTableValue.GetCanBeBoolean: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtBoolean);
  end;

  function TL4DTableValue.GetCanBeChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DTableValue.GetCanBeDouble: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DTableValue.GetCanBeExtended: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DTableValue.GetCanBeInteger: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DTableValue.GetCanBePAnsiChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DTableValue.GetCanBePChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DTableValue.GetCanBePointer: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtLightUserData) or (LLuaType = svtUserdata);
  end;

  function TL4DTableValue.GetCanBeSingle: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DTableValue.GetCanBeString: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DTableValue.GetCanBeTable: Boolean;
  begin
    Result := (GetType = svtTable);
  end;

  function TL4DTableValue.GetCanBeVariant: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtBoolean) or (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DTableValue.GetCanBeWideString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DTableValue.GetType: TL4DStackValueType;
  begin
    Result := FStack.FLua.GetLuaType(FIndex);
  end;

  function TL4DTableValue.GetTypeName: String;
  begin
    Result := FStack.FLua.GetLuaTypeName(FIndex);
  end;

  procedure TL4DTableValue.SetIndex(const AIndex: Integer);
  begin
    FIndex := AIndex;
  end;

  procedure TL4DTableValue.SetKey(const AKey: AnsiString);
  begin
    FKey := AKey;
  end;

{$ENDREGION}

{$REGION 'TL4DTable - Composer/Reader for Lua Tables/Metatables'}
  { TL4DTable }

  procedure TL4DTable.Push;
  begin
    PushTable;
    FTableSet := True;
  end;

  procedure TL4DTable.Close;
  begin
    FLua.Pop(1);
    FClosed := True;
  end;

  constructor TL4DTable.Create(const ALua: TL4DEngine; const ANew: Boolean = False);
  begin
    inherited Create(ALua);
    FFieldIndex := 0;
    FTableSet := False;
    FNew := ANew;
    FClosed := False;
  end;

  destructor TL4DTable.Destroy;
  begin
    if (FNew) and (not FTableSet) then
      PushTable;
    if (not FNew) and (not FClosed) then
      Close;
    inherited;
  end;

  function TL4DTable.GetCount: Integer;
  begin
    Result := FLua.FLua.lua_objlen(FIndex);
  end;

  function TL4DTable.GetName: AnsiString;
  begin
    Result := FName;
  end;

  function TL4DTable.GetValueByIndex(const AIndex: Integer): IL4DTableValue;
  begin
    FLua.PushInteger(AIndex);
    FLua.FLua.lua_gettable(FIndex);
    Result := TL4DTableValue.Create(Self, AIndex);
  end;

  function TL4DTable.GetValueByName(const AName: AnsiString): IL4DTableValue;
  begin
    FLua.FLua.lua_getfield(-1, PAnsiChar(AName));
    Result := TL4DTableValue.Create(Self, -1);
    Result.SetKey(AName);
  end;

  function TL4DTable.NewTable(const AKey: AnsiString): IL4DTable;
  begin
    FLua.PushAnsiString(AKey);
    Result := FLua.NewTable;
    Result.SetOnPushTable(OnPushTable);
  end;

  procedure TL4DTable.OnPushTable(const ATable: IL4DTable);
  begin
    FLua.FLua.lua_settable(FIndex);
  end;

  function TL4DTable.NewTable: IL4DTable;
  begin
    FLua.PushInteger(FFieldIndex);
    Result := FLua.NewTable;
    Result.SetOnPushTable(OnPushTable);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushAnsiChar(const AValue: AnsiChar);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushAnsiChar(AValue);
//    FLua.FLua.lua_settable(FIndex);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushAnsiChar(const AKey: AnsiString; const AValue: AnsiChar);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushAnsiChar(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushAnsiString(const AKey, AValue: AnsiString);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushAnsiString(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushAnsiString(const AValue: AnsiString);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushAnsiString(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushBoolean(const AValue: Boolean);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushBoolean(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushBoolean(const AKey: AnsiString; const AValue: Boolean);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushBoolean(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushChar(const AKey: AnsiString; const AValue: Char);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushChar(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushChar(const AValue: Char);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushChar(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushDouble(const AKey: AnsiString; const AValue: Double);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushDouble(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushDouble(const AValue: Double);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushDouble(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushExtended(const AValue: Extended);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushExtended(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushExtended(const AKey: AnsiString; const AValue: Extended);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushExtended(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushFunction(const AValue: TLuaDelphiFunction);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushFunction(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiFunction);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushFunction(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushFunction(const AValue: TL4DDelphiFunction);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushFunction(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushFunction(const AKey: AnsiString; const AValue: TL4DDelphiObjectFunction);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushFunction(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushFunction(const AValue: TL4DDelphiObjectFunction);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushFunction(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushFunction(const AKey: AnsiString; const AValue: TLuaDelphiFunction);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushFunction(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushInteger(const AValue: Integer);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushInteger(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushInteger(const AKey: AnsiString; const AValue: Integer);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushInteger(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushPAnsiChar(const AKey: AnsiString; const AValue: PAnsiChar);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushPAnsiChar(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushPAnsiChar(const AValue: PAnsiChar);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushPAnsiChar(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushPChar(const AValue: PChar);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushPChar(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushPChar(const AKey: AnsiString; const AValue: PChar);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushPChar(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushPointer(const AKey: AnsiString; const AValue: Pointer);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushPointer(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushPointer(const AValue: Pointer);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushPointer(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushSingle(const AValue: Single);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushSingle(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushSingle(const AKey: AnsiString; const AValue: Single);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushSingle(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushString(const AKey: AnsiString; const AValue: WideString);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushString(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushString(const AValue: WideString);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushString(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushVariant(const AValue: Variant);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushVariant(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushVariant(const AKey: AnsiString; const AValue: Variant);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushVariant(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.PushWideString(const AKey: AnsiString; const AValue: WideString);
  begin
    FLua.PushAnsiString(AKey);
    FLua.PushWideString(AValue);
    FLua.FLua.lua_rawset(FIndex);
  end;

  procedure TL4DTable.SetIndex(const AIndex: Integer);
  begin
    FIndex := AIndex;
  end;

  procedure TL4DTable.SetName(const AName: AnsiString);
  begin
    FName := AName;
  end;

  procedure TL4DTable.SetOnPushTable(const AEvent: TL4DTableChangeEvent);
  begin
    FOnPushTable := AEvent;
  end;

  procedure TL4DTable.PushWideString(const AValue: WideString);
  begin
    FLua.PushInteger(FFieldIndex);
    FLua.PushWideString(AValue);
    FLua.FLua.lua_rawset(FIndex);
    Inc(FFieldIndex);
  end;

  procedure TL4DTable.PushTable;
  begin
//    FLua.FLua.lua_rawset(FIndex);
    if Assigned(FOnPushTable) then
      FOnPushTable(Self);
  end;

{$ENDREGION}

{$REGION 'TL4DMethodStackValue - Method Stack Value Type'}
  { TL4DMethodStackValue }

  function TL4DMethodStackValue.CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
  begin
    Result := FStack.FLua.CallFunction(AParameters, AResultCount);
  end;

  constructor TL4DMethodStackValue.Create(const AStack: TL4DMethodStack; const AIndex: Integer);
  begin
    inherited Create;
    FStack := AStack;
    FIndex := AIndex;
  end;

  function TL4DMethodStackValue.GetAsAnsiString: AnsiString;
  begin
    Result := FStack.FLua.GetAsAnsiString(FIndex);
  end;

  function TL4DMethodStackValue.GetAsBoolean: Boolean;
  begin
    Result := FStack.FLua.GetAsBoolean(FIndex);
  end;

  function TL4DMethodStackValue.GetAsChar: Char;
  begin
    Result := FStack.FLua.GetAsChar(FIndex);
  end;

  function TL4DMethodStackValue.GetAsDouble: Double;
  begin
    Result := FStack.FLua.GetAsDouble(FIndex);
  end;

  function TL4DMethodStackValue.GetAsExtended: Extended;
  begin
    Result := FStack.FLua.GetAsExtended(FIndex);
  end;

  function TL4DMethodStackValue.GetAsInteger: Integer;
  begin
    Result := FStack.FLua.GetAsInteger(FIndex);
  end;

  function TL4DMethodStackValue.GetAsPAnsiChar: PAnsiChar;
  begin
    Result := FStack.FLua.GetAsPAnsiChar(FIndex);
  end;

  function TL4DMethodStackValue.GetAsPChar: PChar;
  begin
    Result := FStack.FLua.GetAsPChar(FIndex);
  end;

  function TL4DMethodStackValue.GetAsPointer: Pointer;
  begin
    Result := FStack.FLua.GetAsPointer(FIndex);
  end;

  function TL4DMethodStackValue.GetAsSingle: Single;
  begin
    Result := FStack.FLua.GetAsSingle(FIndex);
  end;

  function TL4DMethodStackValue.GetAsString: String;
  begin
    Result := FStack.FLua.GetAsString(FIndex);
  end;

  function TL4DMethodStackValue.GetAsTable: IL4DTable;
  begin
    Result := FStack.FLua.GetAsTable(FIndex);
  end;

  function TL4DMethodStackValue.GetAsVariant: Variant;
  begin
    Result := FStack.FLua.GetAsVariant(FIndex);
  end;

  function TL4DMethodStackValue.GetAsWideString: WideString;
  begin
    Result := FStack.FLua.GetAsWideString(FIndex);
  end;

  function TL4DMethodStackValue.GetCanBeAnsiString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStackValue.GetCanBeBoolean: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtBoolean);
  end;

  function TL4DMethodStackValue.GetCanBeChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStackValue.GetCanBeDouble: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStackValue.GetCanBeExtended: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStackValue.GetCanBeInteger: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStackValue.GetCanBePAnsiChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStackValue.GetCanBePChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStackValue.GetCanBePointer: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtLightUserData) or (LLuaType = svtUserdata);
  end;

  function TL4DMethodStackValue.GetCanBeSingle: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DMethodStackValue.GetCanBeString: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodStackValue.GetCanBeTable: Boolean;
  begin
    Result := (GetType = svtTable);
  end;

  function TL4DMethodStackValue.GetCanBeVariant: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtBoolean) or (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DMethodStackValue.GetCanBeWideString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DMethodStackValue.GetType: TL4DStackValueType;
  begin
    Result := FStack.FLua.GetLuaType(FIndex);
  end;

  function TL4DMethodStackValue.GetTypeName: String;
  begin
    Result := FStack.FLua.GetLuaTypeName(FIndex);
  end;
{$ENDREGION}

{$REGION 'TL4DMethodStack - Method Stack Type'}
{ TL4DMethodStack }

  function TL4DMethodStack.GetValue(const AIndex: Integer): IL4DMethodStackValue;
  begin
    Result := TL4DMethodStackValue.Create(Self, AIndex);
  end;

  function TL4DMethodStack.NewTable: IL4DTable;
  begin
    Result := FLua.NewTable;
    Inc(FPushCount);
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
  inherited;
    FPushCount := 0;
  end;

  function TL4DMethodStack.GetCount: Integer;
  begin
    Result := FLua.FLua.lua_gettop;
  end;

  function TL4DMethodStack.GetPushCount: Integer;
  begin
    Result := FPushCount;
  end;

{$ENDREGION}

{$REGION 'TL4DGlobalStackValue - Global Stack Value Type'}
  { TL4DGlobalStack.TL4DGlobalStackValue }

  function TL4DGlobalStackValue.CallFunction(AParameters: array of const; const AResultCount: Integer = LUA_MULTRET): IL4DMethodResultStack;
  begin
    Result := FStack.FLua.CallFunction(AParameters, AResultCount);
  end;

  constructor TL4DGlobalStackValue.Create(const AStack: TL4DGlobalStack; const AKey: AnsiString);
  begin
    inherited Create;
    FStack := AStack;
    FKey := AKey;
  end;

  procedure TL4DGlobalStackValue.Delete;
  begin
    FStack.FLua.Remove(-1);
  end;

  function TL4DGlobalStackValue.GetAsAnsiString: AnsiString;
  begin
    Result := FStack.FLua.GetAsAnsiString(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsBoolean: Boolean;
  begin
    Result := FStack.FLua.GetAsBoolean(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsChar: Char;
  begin
    Result := FStack.FLua.GetAsChar(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsDouble: Double;
  begin
    Result := FStack.FLua.GetAsDouble(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsExtended: Extended;
  begin
    Result := FStack.FLua.GetAsExtended(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsInteger: Integer;
  begin
    Result := FStack.FLua.GetAsInteger(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsPAnsiChar: PAnsiChar;
  begin
    Result := FStack.FLua.GetAsPAnsiChar(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsPChar: PChar;
  begin
    Result := FStack.FLua.GetAsPChar(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsPointer: Pointer;
  begin
    Result := FStack.FLua.GetAsPointer(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsSingle: Single;
  begin
    Result := FStack.FLua.GetAsSingle(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsString: String;
  begin
    Result := FStack.FLua.GetAsString(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsTable: IL4DTable;
  begin
    Result := FStack.FLua.GetAsTable(-1);
  end;

  function TL4DGlobalStackValue.GetAsVariant: Variant;
  begin
    Result := FStack.FLua.GetAsVariant(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetAsWideString: WideString;
  begin
    Result := FStack.FLua.GetAsWideString(-1);
    FStack.FLua.Pop(1);
  end;

  function TL4DGlobalStackValue.GetType: TL4DStackValueType;
  begin
    Result := FStack.FLua.GetLuaType(-1);
    FStack.FLua.Pop(-1);
  end;

  function TL4DGlobalStackValue.GetTypeName: String;
  begin
    Result := FStack.FLua.GetLuaTypeName(-1);
  end;

  procedure TL4DGlobalStackValue.SetAnsiString(const AValue: AnsiString);
  begin
    FStack.PushAnsiString(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetBoolean(const AValue: Boolean);
  begin
    FStack.PushBoolean(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetChar(const AValue: Char);
  begin
    FStack.PushChar(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetDouble(const AValue: Double);
  begin
    FStack.PushDouble(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetExtended(const AValue: Extended);
  begin
    FStack.PushExtended(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetInteger(const AValue: Integer);
  begin
    FStack.PushInteger(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetPAnsiChar(const AValue: PAnsiChar);
  begin
    FStack.PushPAnsiChar(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetPChar(const AValue: PChar);
  begin
    FStack.PushPChar(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetPointer(const AValue: Pointer);
  begin
    FStack.PushPointer(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetSingle(const AValue: Single);
  begin
    FStack.PushSingle(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetString(const AValue: String);
  begin
    FStack.PushString(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetVariant(const AValue: Variant);
  begin
    FStack.PushVariant(FKey, AValue);
  end;

  procedure TL4DGlobalStackValue.SetWideString(const AValue: WideString);
  begin
    FStack.PushWideString(FKey, AValue);
  end;

  function TL4DGlobalStackValue.GetCanBeAnsiString: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DGlobalStackValue.GetCanBeBoolean: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtBoolean);
  end;

  function TL4DGlobalStackValue.GetCanBeChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DGlobalStackValue.GetCanBeDouble: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DGlobalStackValue.GetCanBeExtended: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DGlobalStackValue.GetCanBeInteger: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DGlobalStackValue.GetCanBePAnsiChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DGlobalStackValue.GetCanBePChar: Boolean;
  begin
    Result := GetCanBeString;
  end;

  function TL4DGlobalStackValue.GetCanBePointer: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtLightUserData) or (LLuaType = svtUserdata);
  end;

  function TL4DGlobalStackValue.GetCanBeSingle: Boolean;
  begin
    Result := (GetType = svtNumber);
  end;

  function TL4DGlobalStackValue.GetCanBeString: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DGlobalStackValue.GetCanBeTable: Boolean;
  begin
    Result := (GetType = svtTable);
  end;

  function TL4DGlobalStackValue.GetCanBeVariant: Boolean;
  var
    LLuaType: TL4DStackValueType;
  begin
    LLuaType := GetType;
    Result := (LLuaType = svtBoolean) or (LLuaType = svtNumber) or (LLuaType = svtString);
  end;

  function TL4DGlobalStackValue.GetCanBeWideString: Boolean;
  begin
    Result := GetCanBeString;
  end;
{$ENDREGION}

{$REGION 'TL4DGlobalStack - Global Stack Type'}
  { TL4DGlobalStack }

  function TL4DGlobalStack.GetCount: Integer;
  begin
    Result := FLua.FLua.lua_gettop;
  end;

  function TL4DGlobalStack.GetGlobal(const AKey: AnsiString): IL4DGlobalStackValue;
  begin
    Result := TL4DGlobalStackValue.Create(Self, AKey);
    FLua.FLua.lua_getglobal(PAnsiChar(AKey));
  end;

  function TL4DGlobalStack.NewTable(const AKey: AnsiString): IL4DTable;
  begin
    Result := FLua.NewTable;
    Result.SetName(AKey);
    Result.SetOnPushTable(OnPushTable);
  end;

  procedure TL4DGlobalStack.OnPushTable(const ATable: IL4DTable);
  begin
    SetGlobal(ATable.GetName);
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
//      FLua.Pop(1);
  end;
{$ENDREGION}

{$REGION 'TL4DEngineMember - A simple Array to hold TL4DEngine instances'}
  { TL4DEngineManager }
  procedure TL4DEngineManager.AddExistingEngine(const AEngine: TL4DEngine);
  var
    LIndex: Integer;
  begin
    LIndex := GetEngineIdByState(AEngine.FLua.LuaState);
    if LIndex = -1 then
    begin
      LIndex := Length(FEngines);
      SetLength(FEngines, LIndex + 1);
      FEngines[LIndex] := AEngine;
    end;
  end;

  procedure TL4DEngineManager.DeleteItem(const ALuaState: PLuaState);
  var
    LIndex: Integer;
  begin
    LIndex := GetEngineIdByState(ALuaState);
    if LIndex > -1 then
      DeleteItemByIndex(LIndex);
  end;

  procedure TL4DEngineManager.DeleteItemByIndex(const AIndex: Integer);
  var
   LCount, I: Integer;
  begin
   LCount := Length(FEngines);
   if (AIndex < 0) or (AIndex > LCount - 1) then
     Exit;
   if (AIndex < (LCount - 1)) then
     for I := AIndex to LCount - 2 do
       FEngines[I] := FEngines[I + 1];
   SetLength(FEngines, LCount - 1);
  end;

  destructor TL4DEngineManager.Destroy;
  var
    I: Integer;
  begin
    for I := High(FEngines) downto Low(FEngines) do
      FEngines[I].Free;
    inherited;
  end;

  function TL4DEngineManager.EngineExists(const ALuaState: PLuaState): Boolean;
  begin
    Result := (GetEngineIdByState(ALuaState) > -1);
  end;

  function TL4DEngineManager.GetEngineByState(const ALuaState: PLuaState; const ACreateIfNecessary: Boolean = True): TL4DEngine;
  var
    LIndex: Integer;
  begin
    LIndex := GetEngineIdByState(ALuaState);
    if LIndex = -1 then
    begin
      if ACreateIfNecessary then
      begin
        LIndex := Length(FEngines);
        SetLength(FEngines, LIndex + 1);
        FEngines[LIndex] := TL4DEngine.Create(LuaLinkType, ALuaState);
        Result := FEngines[LIndex];
      end else
        Result := nil;
    end else
      Result := FEngines[LIndex];
  end;

  function TL4DEngineManager.GetEngineIdByState(const ALuaState: PLuaState): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := Low(FEngines) to High(FEngines) do
      if (FEngines[I].FLua.LuaState = ALuaState) then
      begin
        Result := I;
        Break;
      end;
  end;
{$ENDREGION}

{$REGION 'TL4DEngine - Sits Lua4Delphi methods on top of Lua API'}
  { TL4DEngine }

  function TL4DEngine.CallFunction(AParameters: array of const; const AResultCount: Integer): IL4DMethodResultStack;
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
    Result := TL4DMethodResultStack.Create(Self);
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
    EngineManager.AddExistingEngine(Self);
  end;

  destructor TL4DEngine.Destroy;
  var
    I: Integer;
  begin
    for I := Low(FObjectMethods) to High(FObjectMethods) do
      FObjectMethods[I].Free;
    SetLength(FObjectMethods, 0);

    for I := Low(FMethods) to High(FMethods) do
      FMethods[I].Free;
    SetLength(FMethods, 0);

    FOptions.Free;
    FGlobals.Free;
    FLua.Free;

    EngineManager.DeleteItem(FLua.LuaState);
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

  function TL4DEngine.GetAsTable(const AIndex: Integer): IL4DTable;
  begin
    Result := TL4DTable.Create(Self, False);
//    FLua.lua_gettable(AIndex);
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

  function TL4DEngine.NewTable: IL4DTable;
  begin
    Result := TL4DTable.Create(Self, True);
    FLua.lua_newtable;
    Result.SetIndex(FLua.lua_gettop);
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
    LMethod: PL4DMethodObject;
    LIndex: Integer;
  begin
    LMethod := FLua.lua_newuserdata(SizeOf(PL4DMethodObject));
    LMethod^ := TL4DMethodObject.Create(AValue);
    LIndex := Length(FObjectMethods);
    SetLength(FObjectMethods, LIndex + 1);
    FObjectMethods[LIndex] := LMethod^;
    FLua.lua_pushcclosure(L4D_CallClassFunction, 1);
  end;

  procedure TL4DEngine.PushFunction(const AValue: TLuaDelphiFunction);
  begin
    FLua.lua_pushcfunction(AValue);
  end;

  procedure TL4DEngine.PushFunction(const AValue: TL4DDelphiFunction);
  var
    LMethod: PL4DMethod;
    LIndex: Integer;
  begin
    LMethod := FLua.lua_newuserdata(SizeOf(PL4DMethod));
    LMethod^ := TL4DMethod.Create(AValue);
    LIndex := Length(FMethods);
    SetLength(FMethods, LIndex + 1);
    FMethods[LIndex] := LMethod^;
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
        FOnLuaError(Self, LTitle, LMessage, LLine, LRaise);
        if LRaise then
          RaiseException(LTitle, LMessage, LLine);
      end
      else
        RaiseException(LTitle, LMessage, LLine);
    end;
  end;
{$ENDREGION}

{$REGION 'TL4DOptions - Lua4Delphi Options Type'}
  { TL4DOptions }

  constructor TL4DOptions.Create(const ALua: TL4DEngine);
  begin
    inherited Create;
    FLua := ALua;
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
    inherited Create;
    FLua := ALua;
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
      OnException(Self, AExceptionType, AMessage, LRaise);
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

initialization
  EngineManager := TL4DEngineManager.Create;
finalization
  EngineManager.Free;

end.
