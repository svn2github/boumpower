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
    12th March 2012:
      - Added "L4D_API_LOGGING" define (disabled by default)
    5th February 2012:
      - Released
    6th February 2012:
      - Added "L4D_USE_INLINE" define
}
{$I LKSL.inc}

{$IFNDEF DELPHI2010}
  {$MESSAGE WARN 'Lua4Delphi has not been tested on your version of Delphi!'}
{$ENDIF}

// Settings

{.$DEFINE L4D_USE_INLINE} // This will FORCE inlining of Lua4Delphi's source

{$IFDEF DEBUG}
  {$INLINE OFF} // Good for debugging (not so great for performance)
  {.$DEFINE L4D_DEVELOPER_WARNINGS} // Shows Developer Warnings (intended for development of Lua4Delphi itself)
  {.$DEFINE L4D_DEVELOPER_HINTS} // Shows Developer Hints (intended for development of Lua4Delphi itself)
  {.$DEFINE L4D_API_LOGGING} // Output all API calls to a Log File! (intended for development of Lua4Delphi itself)
{$ELSE}
  {$IFDEF L4D_USE_INLINE}
    {$INLINE ON} // Good for optimization (minimal size cost)
  {$ELSE}
    {$INLINE AUTO} // Standard Behaviour (more stable)
  {$ENDIF}
{$ENDIF}
