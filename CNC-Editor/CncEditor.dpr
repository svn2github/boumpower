program CncEditor;

uses
  Forms,
  MAIN in 'MAIN.PAS' {MainForm},
  CHILDWIN in 'CHILDWIN.PAS' {MDIChild},
  about in 'about.pas' {AboutBox},
  EditorWin in 'EditorWin.pas' {MDIEditor};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CNC-Editor';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TMDIEditor, MDIEditor);
  Application.Run;
end.
