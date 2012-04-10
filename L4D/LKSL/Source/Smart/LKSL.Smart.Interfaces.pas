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
  Unit: LKSL.Smart.Interfaces.pas
  Created: 16th March 2012
  Modified: 16th March 2012

  Changelog:
    16th March 2012
      - Added TInterfacedPersistent
}
unit LKSL.Smart.Interfaces;

interface

{$I LKSL.inc}

uses
  {$IFDEF DELPHIXE2}
    System.Classes;
  {$ELSE}
    Classes;
  {$ENDIF}

type
  {
    TInterfacedPersistent
      - Basically a base object for non-reference-counted Interfaced Objects.
        Makes Interfaced Objects behave like Persistent Objects, so you need to
        call .Free MANUALLY (great for ensuring references can only be freed
        when YOU want them to be (and not when Delphi's MM thinks they should)
  }
  {$REGION 'TInterfacedPersistent'}
    TInterfacedPersistent = class(TPersistent, IInterface)
    protected
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
    end;
  {$ENDREGION}

implementation

{$REGION 'TInterfacedPersistent'}
  { TInterfacedPersistent }

  function TInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    if GetInterface(IID, Obj) then
      Result := 0
    else
      Result := E_NOINTERFACE;
  end;

  function TInterfacedPersistent._AddRef: Integer;
  begin
    Result := 0;
  end;

  function TInterfacedPersistent._Release: Integer;
  begin
    Result := 0;
  end;
{$ENDREGION}

end.
