program HexManager;

uses
  Forms,
  W2kMain in 'W2kMain.pas' {Win2kAppForm},
  About in 'About.pas' {AboutBox},
  IntelHEX in 'IntelHEX.pas',
  MergeDialog in 'MergeDialog.pas' {MergeDlg},
  RelocateDialog in 'RelocateDialog.pas' {RelocateDlg},
  BootloaderDialog in 'BootloaderDialog.pas' {BootloaderDlg},
  Internal in 'Internal.pas',
  ConfigDialog in 'ConfigDialog.pas' {PICConfigDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWin2kAppForm, Win2kAppForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TMergeDlg, MergeDlg);
  Application.CreateForm(TRelocateDlg, RelocateDlg);
  Application.CreateForm(TBootloaderDlg, BootloaderDlg);
  Application.CreateForm(TPICConfigDlg, PICConfigDlg);
  Application.Run;
end.

