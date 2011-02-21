program Win2kApp;

uses
  Forms,
  W2kMain in 'W2kMain.pas' {Win2kAppForm},
  About in 'About.pas' {AboutBox},
  HexDump in 'HexDump.pas' {HexDumpDialog},
  LuaWrapper in '..\..\..\..\..\..\Program Files\Borland\pLua\LuaWrapper.pas',
  lua in '..\..\..\..\..\..\Program Files\Borland\pLua\lua.pas',
  pLuaObject in '..\..\..\..\..\..\Program Files\Borland\pLua\pLuaObject.pas',
  uWordList in '..\..\..\..\..\..\Program Files\Borland\pLua\uWordList.pas',
  pLua in '..\..\..\..\..\..\Program Files\Borland\pLua\pLua.pas',
  pLuaRecord in '..\..\..\..\..\..\Program Files\Borland\pLua\pLuaRecord.pas',
  SDSOS in 'SDSOS.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWin2kAppForm, Win2kAppForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(THexDumpDialog, HexDumpDialog);
  Application.Run;
end.
 
