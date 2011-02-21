program LogoApp;

{%File '..\..\..\SmileNG\Proc\Pic16c84.pro'}
{%File 'A:\IntelHEX.txt'}
{%File '..\..\..\Clients\Z&B\Diviseur\Tempo.hex'}
{%File 'Pic2CALM.ini'}
{%File '..\..\..\SmileNG\Modules\LangAsm.ini'}
{%File '..\..\..\SmileNG\Modules\Dlpic4.ini'}
{%File '..\..\..\SmileNG\Proc\16f84.asm'}
{%File '..\..\..\SmileNG\Ref\16f84.ref'}

uses
  Forms,
  LogoMain in 'LogoMain.pas' {LogoAppForm},
  About in 'ABOUT.PAS' {AboutBox},
  PIC in 'PIC.pas',
  Unit1 in 'Unit1.pas' {EditSymbolDlg},
  IntelHex in 'IntelHex.pas',
  ProgressDialog in 'ProgressDialog.pas' {ProgressDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Pic2CALM';
  Application.CreateForm(TLogoAppForm, LogoAppForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TEditSymbolDlg, EditSymbolDlg);
  Application.CreateForm(TProgressDlg, ProgressDlg);
  Application.Run;
end.
 
