program LuaTest;

uses
  Forms,
  LuaTestmain in 'LuaTestmain.pas' {Form1},
  ScriptableObject in '..\..\..\..\Comuni\Script\ScriptableObject.pas',
  Script_Classes in '..\..\..\..\Comuni\Script\VCL\Script_Classes.pas',
  Script_Controls in '..\..\..\..\Comuni\Script\VCL\Script_Controls.pas',
  LuaConf in '..\..\..\..\Comuni\Script\Lua\LuaConf.pas',
  Lua_System in '..\..\..\..\Comuni\Script\Lua\Lua_System.pas',
  Script_DB in '..\..\..\..\Comuni\Script\VCL\Script_DB.pas',
  Script_DBTables in '..\..\..\..\Comuni\Script\VCL\Script_DBTables.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
