program LKSL_SmartTypesDemo;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  LKSL.SmartTypes in '..\..\..\Source\Managers\LKSL.SmartTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
