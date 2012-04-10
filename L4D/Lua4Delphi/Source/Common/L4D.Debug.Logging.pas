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
  Unit: L4D.Debug.Logging.pas
  Released: 22nd February 2012

  NOTE: THIS UNIT IS FOR INTERNAL DEVELOPMENT USE ONLY.
        IT IS DESIGNED TO CHECK LUA C API CALL ORDERING
        FOR CORRECTNESS!

  Changelog:
    22nd February 2012:
      - Added
}
unit L4D.Debug.Logging;

interface

{$I Lua4Delphi.inc}

uses
  {$IFDEF DELPHIXE2}
    System.Classes, System.SysUtils,
  {$ELSE}
    Classes, SysUtils,
  {$ENDIF}
  L4D.Lua.Intf;

{$IFDEF L4D_API_LOGGING}
type
  TL4DLogger = class(TPersistent)
  private
    FActive: Boolean;
    FLog: TStringList;
    FFileName: String;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    procedure AddAPICall(const AMethodName: String; const ALuaState: PLuaState; const AParamStr: String; AParamValues: Array of Const);
    procedure AddCustomMessage(const AMessage: String);
  published
    property Active: Boolean read FActive write FActive default False;
  end;

var
  L4DLogger: TL4DLogger;

const
  L4DBoolStrings: Array[Boolean] of String = ('False', 'True');
{$ENDIF}

implementation

{$IFDEF L4D_API_LOGGING}
  { TL4DLogger }

  procedure TL4DLogger.AddAPICall(const AMethodName: String; const ALuaState: PLuaState; const AParamStr: String; AParamValues: array of Const);
  const
    L4D_MESSAGE_FMT = '[%s] - %s($%p%s);';
    L4D_MESSAGE_NOSTATE_FMT = '[%s] - %s(%s);';
  begin
    if not (FActive) then
      Exit;
    if ALuaState = nil then
      FLog.Add(Format(L4D_MESSAGE_NOSTATE_FMT, [DateTimeToStr(Now), AMethodName, Format(AParamStr, AParamValues)]))
    else
      FLog.Add(Format(L4D_MESSAGE_FMT, [DateTimeToStr(Now), AMethodName, ALuaState, Format(AParamStr, AParamValues)]));
    FLog.SaveToFile(FFileName);
  end;

  procedure TL4DLogger.AddCustomMessage(const AMessage: String);
  const
    L4D_MESSAGE_FMT = '[%s] - %s';
  begin
    FLog.Add(Format(L4D_MESSAGE_FMT, [DateTimeToStr(Now), AMessage]));
    FLog.SaveToFile(FFileName);
  end;

  constructor TL4DLogger.Create(const AFileName: String);
  begin
    inherited Create;
    FActive := False;
    FLog := TStringList.Create;
    FFileName := AFileName;
  end;

  destructor TL4DLogger.Destroy;
  begin
    FLog.Free;
    inherited;
  end;

  initialization
    L4DLogger := TL4DLogger.Create('C:\Lua4Delphi.log');
  finalization
    L4DLogger.Free;
{$ENDIF}

end.
