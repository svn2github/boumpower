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
  Unit: LKSL.Editor.PropertyClassReferenceIntf.pas
  Created: 15th December 2011
  Modified: 15th December 2011

  Changelog:
    15th December 2011
      - Begun "IClassRefProperty" (INCOMPLETE)

  UNIT MARKED AS "IN DEVELOPMENT" (NOT IN USE AT THIS TIME)
}
unit LKSL.Editor.PropertyClassReferenceIntf;

interface

type
  IClassRefProperty = interface
  ['{F86D34BD-5E82-4D8D-89B6-7969F4332BDF}']
    function GetSelectorCaption: String;
    function GetBaseType: TClass;
    function GetScopeFilter: String;
  end;

implementation

end.
