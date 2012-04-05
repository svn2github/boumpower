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
  Unit: LKSL.Packages.AutoSuffix.pas
  Created: 1st January 2012

  Changelog:
    1st January 2012
      - Added defines for Delphi 1 to XE3

  UNIT MARKED AS "IN DEVELOPMENT" (NOT IN USE YET)
}
unit LKSL.Packages.AutoSuffix;

interface

{$I LKSL.inc}

{$IFDEF DELPHI1}
  {$LIBSUFFIX '10'}
{$ENDIF}

{$IFDEF DELPHI2}
  {$LIBSUFFIX '20'}
{$ENDIF}

{$IFDEF DELPHI3}
  {$LIBSUFFIX '30'}
{$ENDIF}

{$IFDEF DELPHI4}
  {$LIBSUFFIX '40'}
{$ENDIF}

{$IFDEF DELPHI5}
  {$LIBSUFFIX '50'}
{$ENDIF}

{$IFDEF DELPHI6}
  {$LIBSUFFIX '60'}
{$ENDIF}

{$IFDEF DELPHI7}
  {$LIBSUFFIX '70'}
{$ENDIF}

{$IFDEF DELPHI8}
  {$LIBSUFFIX '80'}
{$ENDIF}

{$IFDEF DELPHI2005}
  {$LIBSUFFIX '90'}
{$ENDIF}

{$IFDEF DELPHI2006}
  {$LIBSUFFIX '100'}
{$ENDIF}

{$IFDEF DELPHI2007}
  {$LIBSUFFIX '100'} // Confirmed
{$ENDIF}

{$IFDEF DELPHI2007_NET}
  {$LIBSUFFIX '110'} // ?
{$ENDIF}

{$IFDEF DELPHI2009}
  {$LIBSUFFIX '120'} // Confirmed
{$ENDIF}

{$IFDEF DELPHI2010}
  {$LIBSUFFIX '140'} // Confirmed
{$ENDIF}

{$IFDEF DELPHIXE}
  {$LIBSUFFIX '150'} // Confirmed
{$ENDIF}

{$IFDEF DELPHIXE2}
  {$LIBSUFFIX '160'} // Confirmed
{$ENDIF}

{$IFDEF DELPHIXE3}
  {$LIBSUFFIX '170'}
{$ENDIF}

implementation

end.
