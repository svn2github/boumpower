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
  Unit: LKSL.Strings.Utils.pas
  Created: 15th December 2011
  Modified: 15th December 2011

  Changelog:
    15th December 2011
      - Added "ExplodeWideString" (Splits a string into an Array of String based on a delimiter)
      - Added "ConcatenateWideString" (Takes an Array of String and concatenates all entries into a String with delimiters)
      - Added "CountCharInWideString" (Counts the number of occurences of a Char in a String, returns the number)
}
unit LKSL.Strings.Utils;

interface

{$I LKSL.inc}

uses
  {$IFDEF DELPHIXE2}
    System.SysUtils;
  {$ELSE}
    SysUtils;
  {$ENDIF}

type
  TWideStringArray = Array of WideString;

// Separates a String into an Array of String by a Delimiter
function ExplodeWideString(const ADelimiter, AValue: WideString; ACount: integer): TWideStringArray; inline;
// Concatenates a String Array (with an optional Delimiter) into a WideString
function ConcatenateWideString(const AExplodedArray: TWideStringArray; const ADelimiter: WideString = ''): WideString;
// Count occurences of a Char/Sub-string in a String
function CountCharInWideString(const AString: WideString; AChar: WideChar): Integer;

implementation

function ExplodeWideString(const ADelimiter, AValue: WideString; ACount: integer): TWideStringArray;
var
  LTempVal: WideString;
  I, LPos: integer;
begin
  LTempVal := AValue;
  I := 0;
  while Length(LTempVal) > 0 do
  begin
    Inc(I);
    SetLength(Result, I);
    LPos := Pos(ADelimiter, LTempVal);

    if (LPos > 0) and ((I < ACount) or (ACount = 0)) then
    begin
      Result[I - 1] := Copy(LTempVal, 0, LPos - 1);
      LTempVal := Copy(LTempVal, LPos + Length(ADelimiter), Length(LTempVal));
    end
    else
    begin
      Result[I - 1] := LTempVal;
      LTempVal := EmptyStr;
    end;
  end;
end;

function ConcatenateWideString(const AExplodedArray: TWideStringArray; const ADelimiter: WideString = ''): WideString;
var
  I: Integer;
begin
  for I := Low(AExplodedArray) to High(AExplodedArray) do
    if I = Low(AExplodedArray) then
      Result := AExplodedArray[I]
    else
      Result := Result + ADelimiter + AExplodedArray[I];
end;

function CountCharInWideString(const AString: WideString; AChar: WideChar): Integer;
var
  LPChar: PWideChar;
begin
  Result := 0;
  LPChar := PWideChar(Pointer(AString));
  while LPChar <> nil do
  begin
    LPChar := StrScan(LPChar, AChar);
    if LPChar <> nil then
    begin
      Inc(Result);
      Inc(LPChar);
    end;
  end;
end;

end.
