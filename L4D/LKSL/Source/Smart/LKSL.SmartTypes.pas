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
  Unit: LaKraven.SmartTypes.pas
  Created: 16th November 2011
  Modified: 9th December 2011
  Minimum Version: Delphi 2006

  Changelog:
    11th December 2011
      - Smart Types on indefinite hold due to major performance problems
    8th December 2011
      - TString changed to SmartString
      - Operator Overloads added to provide "true String type" behaviour
    16th November 2011
      - Added "TString"
}
unit LKSL.SmartTypes;

{$I LKSL.inc}

{$IFNDEF SUPPORTS_ADVANCED_RECORDS}
  {$MESSAGE FATAL 'This unit will only work with Delphi 2006 and above due to Language Feature Restrictions!'}
{$ENDIF}

interface

uses
  {$IFDEF SCOPED}
  System.Classes, System.StrUtils, System.SysUtils;
  {$ELSE}
  Classes, StrUtils, SysUtils;
  {$ENDIF}

type
  {
    SmartString
      - A "Smart" version of the standard String Type!
  }
  PSmartString = ^SmartString;
  SmartString = record
  private
    FValue: String;
    function GetLength: Integer;
    function GetLowerCase: SmartString;
    function GetUpperCase: SmartString;
  public
    // Value manipulation
    class operator Add(const AValue1, AValue2: SmartString): SmartString; overload;
    class operator Subtract(const AValue1, AValue2: SmartString): SmartString; overload;
    // Logical operations
    class operator Equal(const AValue1, AValue2: SmartString): Boolean; overload;
    class operator GreaterThan(const AValue1, AValue2: SmartString): Boolean; overload;
    class operator GreaterThanOrEqual(const AValue1, AValue2: SmartString): Boolean; overload;
    class operator LessThan(const AValue1, AValue2: SmartString): Boolean; overload;
    class operator LessThanOrEqual(const AValue1, AValue2: SmartString): Boolean; overload;
    class operator NotEqual(const AValue1, AValue2: SmartString): Boolean; overload;
    // To assign "Dumb Values"
    class operator Implicit(const AValue: Boolean): SmartString;
    class operator Implicit(const AValue: Byte): SmartString; overload;
    class operator Implicit(const AValue: Cardinal): SmartString; overload;
    class operator Implicit(const AValue: Char): SmartString; overload;
    class operator Implicit(const AValue: Currency): SmartString; overload;
    class operator Implicit(const AValue: Double): SmartString; overload;
    class operator Implicit(const AValue: Extended): SmartString; overload;
    class operator Implicit(const AValue: Int64): SmartString; overload;
    class operator Implicit(const AValue: Integer): SmartString; overload;
    class operator Implicit(const AValue: NativeInt): SmartString; overload;
    class operator Implicit(const AValue: NativeUInt): SmartString; overload;
    class operator Implicit(const AValue: OleVariant): SmartString; overload;
    class operator Implicit(const AValue: PChar): SmartString; overload;
    class operator Implicit(const AValue: Real): SmartString; overload;
    class operator Implicit(const AValue: ShortInt): SmartString; overload;
    class operator Implicit(const AValue: Single): SmartString; overload;
    class operator Implicit(const AValue: SmallInt): SmartString; overload;
    class operator Implicit(const AValue: String): SmartString; overload;
    class operator Implicit(const AValue: UInt64): SmartString; overload;
    class operator Implicit(const AValue: Variant): SmartString; overload;
    class operator Implicit(const AValue: WideString): SmartString; overload;
    class operator Implicit(const AValue: Word): SmartString; overload;
    // To assign "Smart Values" to "Dumb Values"
    class operator Implicit(const AValue: SmartString): AnsiChar; overload;
    class operator Implicit(const AValue: SmartString): AnsiString; overload;
    class operator Implicit(const AValue: SmartString): Boolean; overload;
    class operator Implicit(const AValue: SmartString): Byte; overload;
    class operator Implicit(const AValue: SmartString): Cardinal; overload;
    class operator Implicit(const AValue: SmartString): Char; overload;
    class operator Implicit(const AValue: SmartString): Currency; overload;
    class operator Implicit(const AValue: SmartString): Double; overload;
    class operator Implicit(const AValue: SmartString): Extended; overload;
    class operator Implicit(const AValue: SmartString): Int64; overload;
    class operator Implicit(const AValue: SmartString): Integer; overload;
    class operator Implicit(const AValue: SmartString): NativeInt; overload;
    class operator Implicit(const AValue: SmartString): NativeUInt; overload;
    class operator Implicit(const AValue: SmartString): OleVariant; overload;
    class operator Implicit(const AValue: SmartString): PAnsiChar; overload;
    class operator Implicit(const AValue: SmartString): PChar; overload;
    class operator Implicit(const AValue: SmartString): Real; overload;
    class operator Implicit(const AValue: SmartString): ShortInt; overload;
    class operator Implicit(const AValue: SmartString): Single; overload;
    class operator Implicit(const AValue: SmartString): SmallInt; overload;
    class operator Implicit(const AValue: SmartString): String; overload;
    class operator Implicit(const AValue: SmartString): UInt64; overload;
    class operator Implicit(const AValue: SmartString): Variant; overload;
    class operator Implicit(const AValue: SmartString): WideString; overload;
    class operator Implicit(const AValue: SmartString): Word; overload;
    // Operate on and build String values
    function Append(const AValue: SmartString): SmartString;
    function Compare(const AValue: SmartString; const ACaseSensitive: Boolean = True): Boolean;
    function CompareOrdinal(const AValue: SmartString): Boolean;
    function Concat(const AValues: Array of SmartString): SmartString;
    function Contains(const ASubString: SmartString): Boolean;
    function Format(const AString: SmartString; const AArgs: Array of const): SmartString; overload;
    function Insert(const AValue: SmartString; const AIndex: Integer): SmartString;
    function Pos(const ASubString: SmartString): Integer;
    property Length: Integer read GetLength;
    property LowerCase: SmartString read GetLowerCase;
    property UpperCase: SmartString read GetUpperCase;
  end;
  TSmartStringArray = Array of SmartString;

implementation

const
  STR_ECONVERT_STRINGTYPE = '"%s" is not a valid %s value';
  BOOLEAN_NAMES: Array[Boolean] of String = ('False', 'True');

{ SmartString }

// Value manipulation

class operator SmartString.Add(const AValue1, AValue2: SmartString): SmartString;
begin
  inherited;
  Result.Concat([AValue1.FValue, AValue2.FValue]);
end;

class operator SmartString.Subtract(const AValue1, AValue2: SmartString): SmartString;
begin
  inherited;
  Result := {$IFDEF SCOPED}System.{$ENDIF}StrUtils.ReplaceText(AValue1, AValue2, EmptyStr);
end;

// Logical operations

class operator SmartString.Equal(const AValue1, AValue2: SmartString): Boolean;
begin
  inherited;
  Result := (AValue1.FValue = AValue2.FValue);
end;

class operator SmartString.GreaterThan(const AValue1, AValue2: SmartString): Boolean;
begin
  inherited;
  Result := (AValue1.FValue > AValue2.FValue);
end;

class operator SmartString.GreaterThanOrEqual(const AValue1, AValue2: SmartString): Boolean;
begin
  inherited;
  Result := (AValue1.FValue >= AValue2.FValue);
end;

class operator SmartString.LessThan(const AValue1, AValue2: SmartString): Boolean;
begin
  inherited;
  Result := (AValue1.FValue < AValue2.FValue);
end;

class operator SmartString.LessThanOrEqual(const AValue1, AValue2: SmartString): Boolean;
begin
  inherited;
  Result := (AValue1.FValue <= AValue2.FValue);
end;

class operator SmartString.NotEqual(const AValue1, AValue2: SmartString): Boolean;
begin
  inherited;
  Result := (AValue1.FValue <> AValue2.FValue);
end;

// To cast normal Types to SmartString

class operator SmartString.Implicit(const AValue: Boolean): SmartString;
begin
  inherited;
  Result.FValue := BOOLEAN_NAMES[AValue];
end;

class operator SmartString.Implicit(const AValue: Byte): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Cardinal): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Char): SmartString;
begin
  inherited;
  Result.FValue := String(AValue);
end;

class operator SmartString.Implicit(const AValue: Currency): SmartString;
begin
  inherited;
  Result.FValue := CurrToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Double): SmartString;
begin
  inherited;
  Result.FValue := FloatToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Extended): SmartString;
begin
  inherited;
  Result.FValue := FloatToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Int64): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Integer): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: NativeInt): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: NativeUInt): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: OleVariant): SmartString;
begin
  inherited;
  Result.FValue := AValue;
end;

class operator SmartString.Implicit(const AValue: PChar): SmartString;
begin
  inherited;
  Result.FValue := String(AValue);
end;

class operator SmartString.Implicit(const AValue: Real): SmartString;
begin
  inherited;
  Result.FValue := FloatToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: ShortInt): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Single): SmartString;
begin
  inherited;
  Result.FValue := FloatToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: SmallInt): SmartString;
begin
  inherited;
  Result.FValue := FloatToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: String): SmartString;
begin
  inherited;
  Result.FValue := AValue;
end;

class operator SmartString.Implicit(const AValue: UInt64): SmartString;
begin
  inherited;
  Result.FValue := FloatToStr(AValue);
end;

class operator SmartString.Implicit(const AValue: Variant): SmartString;
begin
  inherited;
  Result.FValue := AValue;
  { TODO -cSmart Types : Variant to String conversion }
end;

class operator SmartString.Implicit(const AValue: WideString): SmartString;
begin
  inherited;
  Result.FValue := String(AValue);
end;

class operator SmartString.Implicit(const AValue: Word): SmartString;
begin
  inherited;
  Result.FValue := IntToStr(Integer(AValue));
end;

// To cast SmartString to normal Types

class operator SmartString.Implicit(const AValue: SmartString): AnsiChar;
begin
  inherited;
  if AValue.Length = 1 then
    Result := AnsiChar(AnsiString(AValue.FValue)[1])
  else
    raise EConvertError.CreateFmt(STR_ECONVERT_STRINGTYPE, [AValue.FValue, 'AnsiChar']);
end;

class operator SmartString.Implicit(const AValue: SmartString): AnsiString;
begin
  inherited;
  Result := AnsiString(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Boolean;
begin
  inherited;
  Result := (AValue.FValue = BOOLEAN_NAMES[True]);
end;

class operator SmartString.Implicit(const AValue: SmartString): Byte;
begin
  inherited;
  Result := StrToInt(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Cardinal;
begin
  inherited;
  Result := StrToInt(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Char;
begin
  inherited;
  if AValue.Length = 1 then
    Result := Char(AnsiString(AValue.FValue)[1])
  else
    raise EConvertError.CreateFmt(STR_ECONVERT_STRINGTYPE, [AValue.FValue, 'Char']);
end;

class operator SmartString.Implicit(const AValue: SmartString): Currency;
begin
  inherited;
  Result := StrToCurr(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Double;
begin
  inherited;
  Result := StrToFloat(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Extended;
begin
  inherited;
  Result := StrToFloat(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Int64;
begin
  inherited;
  Result := StrToInt64(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Integer;
begin
  inherited;
  Result := StrToInt(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): NativeInt;
begin
  inherited;
  Result := StrToInt64(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): NativeUInt;
begin
  inherited;
  Result := StrToInt64(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): OleVariant;
begin
  inherited;
  Result := AValue.FValue;
end;

class operator SmartString.Implicit(const AValue: SmartString): PAnsiChar;
begin
  inherited;
  Result := PAnsiChar(AnsiString(AValue.FValue));
end;

class operator SmartString.Implicit(const AValue: SmartString): PChar;
begin
  inherited;
  Result := PChar(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Real;
begin
  inherited;
  Result := StrToFloat(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): ShortInt;
begin
  inherited;
  Result := StrToInt(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Single;
begin
  inherited;
  Result := StrToFloat(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): SmallInt;
begin
  inherited;
  Result := StrToInt(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): String;
begin
  inherited;
  Result := AValue.FValue;
end;

class operator SmartString.Implicit(const AValue: SmartString): UInt64;
begin
  inherited;
  Result := StrToInt64(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Variant;
begin
  inherited;
  Result := AValue.FValue;
end;

class operator SmartString.Implicit(const AValue: SmartString): WideString;
begin
  inherited;
  Result := WideString(AValue.FValue);
end;

class operator SmartString.Implicit(const AValue: SmartString): Word;
begin
  inherited;
  Result := Word(StrToInt(AValue.FValue));
end;

// Build and operate on String values

function SmartString.Append(const AValue: SmartString): SmartString;
begin
  FValue := FValue + AValue.FValue;
  Result := FValue;
end;

function SmartString.Compare(const AValue: SmartString; const ACaseSensitive: Boolean = True): Boolean;
begin
  if ACaseSensitive then
    Result := ({$IFDEF SCOPED}System.{$ENDIF}SysUtils.CompareStr(FValue, AValue.FValue) = 0)
  else
    Result := ({$IFDEF SCOPED}System.{$ENDIF}SysUtils.AnsiCompareText(FValue, AValue.FValue) = 0);
end;

function SmartString.CompareOrdinal(const AValue: SmartString): Boolean;
var
  I: Integer;
begin
  Result := System.Length(AValue.FValue) = System.Length(FValue);
  if (not Result) then
    Exit;

  for I := 0 to System.Length(FValue) - 1 do
    if Ord(FValue[I]) <> Ord(AValue.FValue[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function SmartString.Concat(const AValues: Array of SmartString): SmartString;
var
  I: Integer;
begin
  FValue := EmptyStr;
  for I := 0 to System.Length(AValues) - 1 do
    FValue := FValue + AValues[I].FValue;
  Result := FValue;
end;

function SmartString.Contains(const ASubString: SmartString): Boolean;
begin
  Result := Pos(ASubString) > 0;
end;

function SmartString.Format(const AString: SmartString; const AArgs: array of const): SmartString;
begin
  FValue := {$IFDEF SCOPED}System.{$ENDIF}SysUtils.Format(AString.FValue, AArgs);
  Result := FValue;
end;

function SmartString.GetLength: Integer;
begin
  Result := System.Length(FValue);
end;

function SmartString.GetLowerCase: SmartString;
begin
  Result := {$IFDEF SCOPED}System.{$ENDIF}SysUtils.LowerCase(FValue);
end;

function SmartString.GetUpperCase: SmartString;
begin
  Result := {$IFDEF SCOPED}System.{$ENDIF}SysUtils.UpperCase(FValue);
end;

function SmartString.Insert(const AValue: SmartString; const AIndex: Integer): SmartString;
begin

end;

function SmartString.Pos(const ASubString: SmartString): Integer;
begin
  Result := System.Pos(ASubString.FValue, FValue);
end;

end.
