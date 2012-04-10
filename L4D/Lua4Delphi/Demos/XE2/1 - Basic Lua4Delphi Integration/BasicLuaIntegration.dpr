program BasicLuaIntegration;

uses
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
