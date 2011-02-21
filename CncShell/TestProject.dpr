program TestProject;

uses
  Forms,
  Test in 'Test.pas' {Form1},
  CncShellDlgs in 'CncShellDlgs.pas' {MetadataDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TMetadataDlg, MetadataDlg);
  Application.Run;
end.
