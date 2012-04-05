{
  This unit is part of the LaKraven Studios Standard Library (LKSL).

  Copyright (C) 2011, LaKraven Studios Ltd.
  Copyright Protection Packet(s): LKSL001
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
  Unit: LKSL.Managed.Arrays.pas
  Created: 31st October 2011
  Modified: 16th November 2011
  
  Changelog:
    16th November 2011
      - Added an Integer-indexed Managed Array
      - Added an Object-indexed Managed Array
      - Added a Variant-indexed Managed Array
      - Added an Int64-indexed Managed Array
      - Added a Single-indexed Managed Array
      - Added a Double-indexed Managed Array
      - Added an Extended-indexed Managed Array
      - Added a TDate-indexed Managed Array
      - Added a TTime-indexed Managed Array
      - Added a TDateTime-indexed Managed Array
    15th November 2011
      - Added Base Classes, modified all Array Types to use common Base
        behaviors
    7th November 2011
      - Added conditional "Uses" declaration for Delphi XE2 and above.
}
unit LKSL.Managed.Arrays;

interface

{$I LKSL.inc}

uses
  {$IFDEF SCOPED}
  System.Classes, System.StrUtils;
  {$ELSE}
  Classes, StrUtils;
  {$ENDIF}

type
  { Forward Declarations }
  TBaseArrayManager = class; // Base Class for all Array Managers
  TBaseArrayObject = class; // Base Class for all Managed Array Objects (Item)
  TStringArrayManager = class; // A Manager for String-Indexed Arrays
  TStringArrayObject = class; // A String-Indexed Managed Array Object (Item)
  TIntegerArrayManager = class; // A Manager for Integer-Indexed Arrays
  TIntegerArrayObject = class; // An Integer-Indexed Managed Array Object (Item)
  TObjectArrayManager = class; // A Manager for Object-Indexed Arrays
  TObjectArrayObject = class; // An Object-Indexed Managed Array Object (Item)
  TVariantArrayManager = class; // A Manager for Variant-Indexed Arrays
  TVariantArrayObject = class; // A Variant-Indexed Managed Array Object (Item)
  TInt64ArrayManager = class; // A Manager for Int64-Indexed Arrays
  TInt64ArrayObject = class; // An Int64-Indexed Managed Array Object (Item)
  TSingleArrayManager = class; // A Manager for Single-indexed Arrays
  TSingleArrayObject = class; // A Single-Indexed Managed Array Object (Item)
  TDoubleArrayManager = class; // A Manager for Double-indexed Arrays
  TDoubleArrayObject = class; // A Double-Indexed Managed Array Object (Item)
  TExtendedArrayManager = class; // A Manager for Extended-indexed Arrays
  TExtendedArrayObject = class; // An Extended-Indexed Managed Array Object (Item)
  TDateArrayManager = class; // A Manager for Date-indexed Arrays
  TDateArrayObject = class; // A Date-Indexed Managed Array Object (Item)
  TTimeArrayManager = class; // A Manager for Time-indexed Arrays
  TTimeArrayObject = class; // A Time-Indexed Managed Array Object (Item)
  TDateTimeArrayManager = class; // A Manager for DateTime-indexed Arrays
  TDateTimeArrayObject = class; // A DateTime-Indexed Managed Array Object (Item)

  { Class Ref Declarations }
  TArrayObjectClass = class of TBaseArrayObject; // Class declaration (for memory allocation/release purposes)

  { Array Declarations }
  TArrayObjectArray = Array of TBaseArrayObject; // Base Array Type

  {
    TBaseArrayManager
      - A Base Class for all Array Managers
  }
  TBaseArrayManager = class(TPersistent)
  protected
    FCount: Integer;
    FObjects: TArrayObjectArray;
    FObjectType: TArrayObjectClass;
    function GetObjectByIndex(const AIndex: Integer): TStringArrayObject; virtual;
    procedure DeleteObject(const AIndex: Integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
  published
    property Count: Integer read FCount;
  end;

  {
    TBaseArrayObject
      - A Base Class for all Managed Array Objects
  }
  TBaseArrayObject = class(TPersistent)
  protected
    FIndex: Integer;
    FManager: TBaseArrayManager;
  public
    constructor Create(const AIndex: Integer; const AManager: TBaseArrayManager); virtual;
    destructor Destroy; override;
  published
    property Index: Integer read FIndex;
  end;

  {
    TStringArrayManager
      - A String-indexed Array Manager
  }
  TStringArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AName: String): TStringArrayObject; virtual;
    function GetObjectByName(const AName: String): TStringArrayObject; virtual;
    function GetObjectIndexByName(const AName: String): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TStringArrayObject
      - A String-indexed Array Object, housed within a TStringArrayManager
  }
  TStringArrayObject = class(TBaseArrayObject)
  protected
    FName: String;
  published
    property Name: String read FName;
  end;

  {
    TIntegerArrayManager
      - An Integer-indexed Array Manager
  }
  TIntegerArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: Integer): TIntegerArrayObject; virtual;
    function GetObjectByID(const AID: Integer): TIntegerArrayObject; virtual;
    function GetObjectIndexByID(const AID: Integer): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TIntegerArrayObject
      - An Integer-indexed Array Object, housed within a TIntegerArrayManager
  }
  TIntegerArrayObject = class(TBaseArrayObject)
  protected
    FID: Integer;
  published
    property ID: Integer read FID;
  end;

  {
    TObjectArrayManager
      - An Object-indexed Array Manager
  }
  TObjectArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AObject: TObject): TObjectArrayObject; virtual;
    function GetObjectByID(const AObject: NativeInt): TObjectArrayObject; virtual;
    function GetObjectIndexByID(const AObject: NativeInt): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TObjectArrayObject
      - An Object-indexed Array Object, housed within a TObjectArrayManager
  }
  TObjectArrayObject = class(TBaseArrayObject)
  protected
    FObject: NativeInt;
    function GetObject: TObject; virtual;
  published
    property IDObject: TObject read GetObject; // Had to use "IDObject" because "Object" is a reserved word!
  end;

  {
    TVariantArrayManager
      - A Variant-indexed Array Manager
  }
  TVariantArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: Variant): TVariantArrayObject; virtual;
    function GetObjectByID(const AID: Variant): TVariantArrayObject; virtual;
    function GetObjectIndexByID(const AID: Variant): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TVariantArrayObject
      - A Variant-indexed Array Object, housed within a TVariantArrayManager
  }
  TVariantArrayObject = class(TBaseArrayObject)
  protected
    FID: Variant;
  published
    property ID: Variant read FID;
  end;

  {
    TInt64ArrayManager
      - An Int64-indexed Array Manager
  }
  TInt64ArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: Int64): TInt64ArrayObject; virtual;
    function GetObjectByID(const AID: Int64): TInt64ArrayObject; virtual;
    function GetObjectIndexByID(const AID: Int64): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TInt64ArrayObject
      - An Int64-indexed Array Object, housed within a TInt64ArrayManager
  }
  TInt64ArrayObject = class(TBaseArrayObject)
  protected
    FID: Int64;
  published
    property ID: Int64 read FID;
  end;

  {
    TSingleArrayManager
      - A Single-indexed Array Manager
  }
  TSingleArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: Single): TSingleArrayObject; virtual;
    function GetObjectByID(const AID: Single): TSingleArrayObject; virtual;
    function GetObjectIndexByID(const AID: Single): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TSingleArrayObject
      - A Single-indexed Array Object, housed within a TSingleArrayManager
  }
  TSingleArrayObject = class(TBaseArrayObject)
  protected
    FID: Single;
  published
    property ID: Single read FID;
  end;

  {
    TDoubleArrayManager
      - A Double-indexed Array Manager
  }
  TDoubleArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: Double): TDoubleArrayObject; virtual;
    function GetObjectByID(const AID: Double): TDoubleArrayObject; virtual;
    function GetObjectIndexByID(const AID: Double): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TDoubleArrayObject
      - A Double-indexed Array Object, housed within a TDoubleArrayManager
  }
  TDoubleArrayObject = class(TBaseArrayObject)
  protected
    FID: Double;
  published
    property ID: Double read FID;
  end;

  {
    TExtendedArrayManager
      - An Extended-indexed Array Manager
  }
  TExtendedArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: Extended): TExtendedArrayObject; virtual;
    function GetObjectByID(const AID: Extended): TExtendedArrayObject; virtual;
    function GetObjectIndexByID(const AID: Extended): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TExtendedArrayObject
      - An Extended-indexed Array Object, housed within a TExtendedArrayManager
  }
  TExtendedArrayObject = class(TBaseArrayObject)
  protected
    FID: Extended;
  published
    property ID: Extended read FID;
  end;

  {
    TDateArrayManager
      - A TDate-indexed Array Manager
  }
  TDateArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: TDate): TDateArrayObject; virtual;
    function GetObjectByID(const AID: TDate): TDateArrayObject; virtual;
    function GetObjectIndexByID(const AID: TDate): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TDateArrayObject
      - A TDate-indexed Array Object, housed within a TDateArrayManager
  }
  TDateArrayObject = class(TBaseArrayObject)
  protected
    FID: TDate;
  published
    property ID: TDate read FID;
  end;

  {
    TTimeArrayManager
      - A TTime-indexed Array Manager
  }
  TTimeArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: TTime): TTimeArrayObject; virtual;
    function GetObjectByID(const AID: TTime): TTimeArrayObject; virtual;
    function GetObjectIndexByID(const AID: TTime): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TTimeArrayObject
      - A TTime-indexed Array Object, housed within a TTimeArrayManager
  }
  TTimeArrayObject = class(TBaseArrayObject)
  protected
    FID: TTime;
  published
    property ID: TTime read FID;
  end;

  {
    TDateTimeArrayManager
      - A TDateTime-indexed Array Manager
  }
  TDateTimeArrayManager = class(TBaseArrayManager)
  protected
    function AddObject(const AID: TDateTime): TDateTimeArrayObject; virtual;
    function GetObjectByID(const AID: TDateTime): TDateTimeArrayObject; virtual;
    function GetObjectIndexByID(const AID: TDateTime): Integer; virtual;
  public
    constructor Create; override;
  end;

  {
    TDateTimeArrayObject
      - A TDateTime-indexed Array Object, housed within a TDateTimeArrayManager
  }
  TDateTimeArrayObject = class(TBaseArrayObject)
  protected
    FID: TDateTime;
  published
    property ID: TDateTime read FID;
  end;

implementation

{ TBaseArrayManager }

procedure TBaseArrayManager.Clear;
begin
  while FCount > 0 do
    FObjects[FCount - 1].Free;
end;

constructor TBaseArrayManager.Create;
begin
  inherited;
  FCount := 0;
end;

procedure TBaseArrayManager.DeleteObject(const AIndex: Integer);
var
 LCount, I: Integer;
begin
 LCount := Length(FObjects);
 if (AIndex < 0) or (AIndex > LCount - 1) then
   Exit;
 if (AIndex < (LCount - 1)) then
   for I := AIndex to LCount - 2 do
   begin
     FObjects[I] := FObjects[I + 1];
     FObjects[I].FIndex := I;
   end;
 SetLength(FObjects, LCount - 1);
 FCount := LCount - 1;
end;

destructor TBaseArrayManager.Destroy;
begin
  Clear;
  inherited;
end;

function TBaseArrayManager.GetObjectByIndex(const AIndex: Integer): TStringArrayObject;
begin
  if (AIndex > -1) and (AIndex < FCount) then
    Result := TStringArrayObject(FObjects[AIndex])
  else
    Result := nil;
end;

{ TBaseArrayObject }

constructor TBaseArrayObject.Create(const AIndex: Integer; const AManager: TBaseArrayManager);
begin
  inherited Create;
  FIndex := AIndex;
  FManager := AManager;
end;

destructor TBaseArrayObject.Destroy;
begin
  FManager.DeleteObject(FIndex);
  inherited;
end;

{ TStringArrayManager }

function TStringArrayManager.AddObject(const AName: String): TStringArrayObject;
  function GetSortedPosition(const AName: String): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AName <= TStringArrayObject(FObjects[LIndex]).FName then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TStringArrayObject(FObjects[LHigh]).FName < AName) then
      Result := LHigh + 1
    else if (TStringArrayObject(FObjects[LLow]).FName < AName) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByName(AName);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AName);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TStringArrayObject(FObjects[LIndex]).FName := AName;
    Result := TStringArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TStringArrayObject(FObjects[LIndex]);
end;

constructor TStringArrayManager.Create;
begin
  inherited;
  FObjectType := TStringArrayObject;
end;

function TStringArrayManager.GetObjectByName(const AName: String): TStringArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByName(AName);
  if LIndex = -1 then
    Result := nil
  else
    Result := TStringArrayObject(FObjects[LIndex]);
end;

function TStringArrayManager.GetObjectIndexByName(const AName: String): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AName <= TStringArrayObject(FObjects[LIndex]).FName then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TStringArrayObject(FObjects[LHigh]).FName = AName) then
    Result := LHigh
  else if (TStringArrayObject(FObjects[LLow]).FName = AName) then
    Result := LLow;
end;

{ TIntegerArrayManager }

function TIntegerArrayManager.AddObject(const AID: Integer): TIntegerArrayObject;
  function GetSortedPosition(const AID: Integer): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TIntegerArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TIntegerArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TIntegerArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TIntegerArrayObject(FObjects[LIndex]).FID := AID;
    Result := TIntegerArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TIntegerArrayObject(FObjects[LIndex]);
end;

constructor TIntegerArrayManager.Create;
begin
  inherited;
  FObjectType := TIntegerArrayObject;
end;

function TIntegerArrayManager.GetObjectByID(const AID: Integer): TIntegerArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TIntegerArrayObject(FObjects[LIndex]);
end;

function TIntegerArrayManager.GetObjectIndexByID(const AID: Integer): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TIntegerArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TIntegerArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TIntegerArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TObjectArrayManager }

function TObjectArrayManager.AddObject(const AObject: TObject): TObjectArrayObject;
  function GetSortedPosition(const AObject: NativeInt): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AObject <= TObjectArrayObject(FObjects[LIndex]).FObject then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TObjectArrayObject(FObjects[LHigh]).FObject < AObject) then
      Result := LHigh + 1
    else if (TObjectArrayObject(FObjects[LLow]).FObject < AObject) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
  LStoredID: NativeInt;
begin
  LStoredID := NativeInt(@AObject);
  LIndex := GetObjectIndexByID(LStoredID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(LStoredID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TObjectArrayObject(FObjects[LIndex]).FObject := LStoredID;
    Result := TObjectArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TObjectArrayObject(FObjects[LIndex]);
end;

constructor TObjectArrayManager.Create;
begin
  inherited;
  FObjectType := TObjectArrayObject;
end;

function TObjectArrayManager.GetObjectByID(const AObject: NativeInt): TObjectArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AObject);
  if LIndex = -1 then
    Result := nil
  else
    Result := TObjectArrayObject(FObjects[LIndex]);
end;

function TObjectArrayManager.GetObjectIndexByID(const AObject: NativeInt): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AObject <= TObjectArrayObject(FObjects[LIndex]).FObject then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TObjectArrayObject(FObjects[LHigh]).FObject = AObject) then
    Result := LHigh
  else if (TObjectArrayObject(FObjects[LLow]).FObject = AObject) then
    Result := LLow;
end;

{ TObjectArrayObject }

function TObjectArrayObject.GetObject: TObject;
begin
  Result := TObject(Pointer(FObject)^);
end;

{ TVariantArrayManager }

function TVariantArrayManager.AddObject(const AID: Variant): TVariantArrayObject;
  function GetSortedPosition(const AID: Variant): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TIntegerArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TIntegerArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TIntegerArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TIntegerArrayObject(FObjects[LIndex]).FID := AID;
    Result := TVariantArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TVariantArrayObject(FObjects[LIndex]);
end;

constructor TVariantArrayManager.Create;
begin
  inherited;
  FObjectType := TVariantArrayObject;
end;

function TVariantArrayManager.GetObjectByID(const AID: Variant): TVariantArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TVariantArrayObject(FObjects[LIndex]);
end;

function TVariantArrayManager.GetObjectIndexByID(const AID: Variant): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TVariantArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TVariantArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TVariantArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TInt64ArrayManager }

function TInt64ArrayManager.AddObject(const AID: Int64): TInt64ArrayObject;
  function GetSortedPosition(const AID: Int64): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TInt64ArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TInt64ArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TInt64ArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TInt64ArrayObject(FObjects[LIndex]).FID := AID;
    Result := TInt64ArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TInt64ArrayObject(FObjects[LIndex]);
end;

constructor TInt64ArrayManager.Create;
begin
  inherited;
  FObjectType := TInt64ArrayObject;
end;

function TInt64ArrayManager.GetObjectByID(const AID: Int64): TInt64ArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TInt64ArrayObject(FObjects[LIndex]);
end;

function TInt64ArrayManager.GetObjectIndexByID(const AID: Int64): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TInt64ArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TInt64ArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TInt64ArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TSingleArrayManager }

function TSingleArrayManager.AddObject(const AID: Single): TSingleArrayObject;
  function GetSortedPosition(const AID: Single): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TSingleArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TSingleArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TSingleArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TSingleArrayObject(FObjects[LIndex]).FID := AID;
    Result := TSingleArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TSingleArrayObject(FObjects[LIndex]);
end;

constructor TSingleArrayManager.Create;
begin
  inherited;
  FObjectType := TSingleArrayObject;
end;

function TSingleArrayManager.GetObjectByID(const AID: Single): TSingleArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TSingleArrayObject(FObjects[LIndex]);
end;

function TSingleArrayManager.GetObjectIndexByID(const AID: Single): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TSingleArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TSingleArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TSingleArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TDoubleArrayManager }

function TDoubleArrayManager.AddObject(const AID: Double): TDoubleArrayObject;
  function GetSortedPosition(const AID: Double): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TDoubleArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TDoubleArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TDoubleArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TDoubleArrayObject(FObjects[LIndex]).FID := AID;
    Result := TDoubleArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TDoubleArrayObject(FObjects[LIndex]);
end;

constructor TDoubleArrayManager.Create;
begin
  inherited;
  FObjectType := TDoubleArrayObject;
end;

function TDoubleArrayManager.GetObjectByID(const AID: Double): TDoubleArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TDoubleArrayObject(FObjects[LIndex]);
end;

function TDoubleArrayManager.GetObjectIndexByID(const AID: Double): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TDoubleArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TDoubleArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TDoubleArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TExtendedArrayManager }

function TExtendedArrayManager.AddObject(const AID: Extended): TExtendedArrayObject;
  function GetSortedPosition(const AID: Extended): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TExtendedArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TExtendedArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TExtendedArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TExtendedArrayObject(FObjects[LIndex]).FID := AID;
    Result := TExtendedArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TExtendedArrayObject(FObjects[LIndex]);
end;

constructor TExtendedArrayManager.Create;
begin
  inherited;
  FObjectType := TExtendedArrayObject;
end;

function TExtendedArrayManager.GetObjectByID(const AID: Extended): TExtendedArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TExtendedArrayObject(FObjects[LIndex]);
end;

function TExtendedArrayManager.GetObjectIndexByID(const AID: Extended): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TExtendedArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TExtendedArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TExtendedArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TDateArrayManager }

function TDateArrayManager.AddObject(const AID: TDate): TDateArrayObject;
  function GetSortedPosition(const AID: TDate): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TDateArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TDateArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TDateArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TDateArrayObject(FObjects[LIndex]).FID := AID;
    Result := TDateArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TDateArrayObject(FObjects[LIndex]);
end;

constructor TDateArrayManager.Create;
begin
  inherited;
  FObjectType := TDateArrayObject;
end;

function TDateArrayManager.GetObjectByID(const AID: TDate): TDateArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TDateArrayObject(FObjects[LIndex]);
end;

function TDateArrayManager.GetObjectIndexByID(const AID: TDate): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TDateArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TDateArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TDateArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TTimeArrayManager }

function TTimeArrayManager.AddObject(const AID: TTime): TTimeArrayObject;
  function GetSortedPosition(const AID: TTime): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TTimeArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TTimeArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TTimeArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TTimeArrayObject(FObjects[LIndex]).FID := AID;
    Result := TTimeArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TTimeArrayObject(FObjects[LIndex]);
end;

constructor TTimeArrayManager.Create;
begin
  inherited;
  FObjectType := TTimeArrayObject;
end;

function TTimeArrayManager.GetObjectByID(const AID: TTime): TTimeArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TTimeArrayObject(FObjects[LIndex]);
end;

function TTimeArrayManager.GetObjectIndexByID(const AID: TTime): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TTimeArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TTimeArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TTimeArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

{ TDateTimeArrayManager }

function TDateTimeArrayManager.AddObject(const AID: TDateTime): TDateTimeArrayObject;
  function GetSortedPosition(const AID: TDateTime): Integer;
  var
    LIndex, LLow, LHigh: Integer;
  begin
    Result := 0;
    LLow := 0;
    LHigh := Length(FObjects) - 1;
    if LHigh = - 1 then
      Exit;
    if LLow < LHigh then
    begin
      while (LHigh - LLow > 1) do
      begin
        LIndex := (LHigh + LLow) div 2;
        if AID <= TDateTimeArrayObject(FObjects[LIndex]).FID then
          LHigh := LIndex
        else
          LLow := LIndex;
      end;
    end;
    if (TDateTimeArrayObject(FObjects[LHigh]).FID < AID) then
      Result := LHigh + 1
    else if (TDateTimeArrayObject(FObjects[LLow]).FID < AID) then
      Result := LLow + 1
    else
      Result := LLow;
  end;
var
  I, LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
  begin
    LIndex := GetSortedPosition(AID);
    SetLength(FObjects, Length(FObjects) + 1);
    // Shift elements RIGHT
    if LIndex < Length(FObjects) - 1 then
      for I := Length(FObjects) - 1 downto LIndex + 1 do
      begin
        FObjects[I] := FObjects[I - 1];
        FObjects[I].FIndex := I;
      end;
    // Insert new item now
    FObjects[LIndex] := FObjectType.Create(LIndex, Self);
    TDateTimeArrayObject(FObjects[LIndex]).FID := AID;
    Result := TDateTimeArrayObject(FObjects[LIndex]);
    FCount := Length(FObjects);
  end else
    Result := TDateTimeArrayObject(FObjects[LIndex]);
end;

constructor TDateTimeArrayManager.Create;
begin
  inherited;
  FObjectType := TDateTimeArrayObject;
end;

function TDateTimeArrayManager.GetObjectByID(const AID: TDateTime): TDateTimeArrayObject;
var
  LIndex: Integer;
begin
  LIndex := GetObjectIndexByID(AID);
  if LIndex = -1 then
    Result := nil
  else
    Result := TDateTimeArrayObject(FObjects[LIndex]);
end;

function TDateTimeArrayManager.GetObjectIndexByID(const AID: TDateTime): Integer;
var
  LIndex, LLow, LHigh: Integer;
begin
  Result := -1;
  LLow := 0;
  LHigh := Length(FObjects) - 1;
  if LHigh = -1 then
    Exit;
  if LLow < LHigh then
  begin
    while (LHigh - LLow > 1) do
    begin
      LIndex := (LHigh + LLow) div 2;
      if AID <= TDateTimeArrayObject(FObjects[LIndex]).FID then
        LHigh := LIndex
      else
        LLow := LIndex;
    end;
  end;
  if (TDateTimeArrayObject(FObjects[LHigh]).FID = AID) then
    Result := LHigh
  else if (TDateTimeArrayObject(FObjects[LLow]).FID = AID) then
    Result := LLow;
end;

end.
