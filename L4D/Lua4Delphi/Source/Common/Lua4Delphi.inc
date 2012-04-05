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
  Unit: Lua4Delphi.inc
  Released: 5th February 2012

  Changelog:
    5th February 2012:
      - Released
}
{$I LKSL.inc}

{$IFNDEF DELPHI2010}
  {$MESSAGE WARN 'Lua4Delphi has not been tested on your version of Delphi!'}
{$ENDIF}

// Settings

{$IFDEF DEBUG}
  {$INLINE OFF} // Good for debugging (not so great for performance)
  {$DEFINE L4D_DEVELOPER_WARNINGS} // Shows Developer Warnings
  {$DEFINE L4D_DEVELOPER_HINTS} // Shows Developer Hints (intended for development of Lua4Delphi itself)
{$ELSE}
  {$INLINE ON} // Good for optimization (minimal size cost)
{$ENDIF}
