unit L4D.Engine.Constants;

interface

{$I Lua4Delphi.inc}

const
  L4D_CODESEGMENTNAME = 'Lua Code Segment';
  L4D_LUA_ERROR_MULTILINE = 'An error occured while executing Lua code...' + #13#10 + #13#10 + 'Title: %s' + #13#10 + 'Line: %d' + #13#10 + 'Message: %s';
  L4D_LUA_ERROR_ONELINE = 'An error occured while executing Lua code:' + #13#10 + '%s';
  L4D_EXCEPTION_SCRIPTNOTFOUND = 'Lua Script not found: "%s"';

implementation

end.
