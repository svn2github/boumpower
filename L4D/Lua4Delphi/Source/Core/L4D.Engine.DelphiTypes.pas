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
  Unit: L4D.Engine.DelphiTypes.pas
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
unit L4D.Engine.DelphiTypes;

interface

uses
  {$IFDEF DELPHIXE2}
    System.Classes;
  {$ELSE}
    Classes;
  {$ENDIF}

// Delphi Unicode String type for use by Lua (replicate Lua String Methods, hold String value

type
  { Forward Declarations }
  TL4DClass = class;

  TL4DClassArray = Array of TL4DClass;

  TL4DClass = class(TPersistent)

  end;

implementation

end.
