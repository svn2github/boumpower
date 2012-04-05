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
  Unit: L4D.Engine.StaticLua51.pas
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
unit L4D.Engine.StaticLua51;

interface

{$I Lua4Delphi.inc}

uses
  L4D.Lua.StaticLua51, L4D.Engine.Main;

type
  TL4D51Static = class(TLua4DelphiCommon)
  protected
    procedure AssignLinkType; override;
  end;

implementation

{ TL4D51Static }

procedure TL4D51Static.AssignLinkType;
begin
  inherited;
  FLuaType := TLua51Static;
end;

end.
