program LKSL_ManagedStringArrayDemo;

{$I LKSL.inc}

uses
  {$IFDEF SCOPED}
    Vcl.Forms,
  {$ELSE}
    Forms,
  {$ENDIF}
  uMainForm in 'uMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  {$IFDEF DEBUG}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
