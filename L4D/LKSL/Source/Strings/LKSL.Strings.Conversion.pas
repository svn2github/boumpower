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
  Unit: LKSL.Strings.Conversion.pas
  Created: 15th December 2011
  Modified: 15th December 2011

  Changelog:
    15th December 2011
      - Added "WideStringToAnsiString" (safe conversion of WideString to AnsiString)
}
unit LKSL.Strings.Conversion;

interface

{$I LKSL.inc}

{$IFDEF MSWINDOWS}
uses
  {$IFDEF DELPHIXE2}
    Winapi.Windows;
  {$ELSE}
    Windows;
  {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHIXE}
type
  PLongBool = ^LongBool;

  function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
    UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
    LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload; inline;
{$ENDIF}

// A SAFE way to convert from WideString to AnsiString (UTF-8 chars preserved)
function WideStringToAnsiString(const S: WideString): AnsiString; inline;

implementation

{$IFNDEF DELPHIXE}
  function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
    UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
    LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;
  begin
    Result := WideCharToMultiByte(CodePage, Flags, UnicodeStr, UnicodeStrLen, LocaleStr,
      LocaleStrLen, DefaultChar, PBOOL(UsedDefaultChar));
  end;
{$ENDIF}

function WideStringToAnsiString(const S: WideString): AnsiString;
var
  BufSize: Integer;
begin
  {
    NOTE: May need to use "WideCharToMultiByte" on older versions in place of "LocaleCharsFromUnicode"
  }
  Result := '';
  if Length(S) = 0 then
    Exit;
  BufSize := LocaleCharsFromUnicode(CP_UTF8, 0, PWideChar(S), Length(S) , nil, 0, nil, nil);
  SetLength(result, BufSize);
  LocaleCharsFromUnicode(CP_UTF8, 0, PWideChar(S), Length(S) , PAnsiChar(Result), BufSize, nil, nil);
end;

end.
