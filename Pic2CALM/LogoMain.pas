unit LogoMain;

interface

uses Windows, Classes, Graphics, Forms, Controls, Menus,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList, StdActns,
  ActnList, ToolWin, ClipBrd;

type
  TLogoAppForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton8: TToolButton;
    ActionList1: TActionList;
    FileNew1: TAction;
    FileOpen1: TAction;
    FileSave1: TAction;
    FileSaveAs1: TAction;
    FileSend1: TAction;
    FileExit1: TAction;
    EditCopy1: TEditCopy;
    HelpAbout1: TAction;
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    FileExitItem: TMenuItem;
    Edit1: TMenuItem;
    CopyItem: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    ListBoxPrg: TListBox;
    Splitter1: TSplitter;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    LabelSize: TLabel;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileSaveAs1Execute(Sender: TObject);
    procedure FileSend1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FFileName: String;
    FSize : integer;
    FPrgHEX: pointer;
  public
    { Public declarations }
    procedure LoadFromFileOUT(FName : String);
    procedure LoadFromFileHEX(FName : String);
    procedure Pass1;
  end;

var
  LogoAppForm: TLogoAppForm;

implementation

uses Math,SysUtils, Mapi, About, PIC, Unit1, IntelHex, ProgressDialog;

{$R *.DFM}

resourcestring
  SUntitled  = 'SansTitre';
  SOverwrite = 'OK pour remplacer %s';
  SSendError = 'Erreur à l''envoi du courrier';

procedure TLogoAppForm.FileNew1Execute(Sender: TObject);
begin
  FFileName := SUntitled;
//  RichEdit1.Lines.Clear;
//  RichEdit1.Modified := False;
end;

procedure TLogoAppForm.FileOpen1Execute(Sender: TObject);
var
  Ext : string;
begin
  if OpenDialog.Execute then
  begin
    Ext := ExtractFileExt(OpenDialog.FileName);
    If CompareText(Ext, '.HEX') = 0 then
      LoadFromFileHEX(OpenDialog.FileName)
    else
      LoadFromFileOUT(OpenDialog.FileName);
    FFileName := OpenDialog.FileName;
    Pass1;
    ListBoxPrg.SetFocus;
//    RichEdit1.Modified := False;
  end;
end;

procedure TLogoAppForm.FileSave1Execute(Sender: TObject);
begin
  FileSaveAs1Execute(Sender)
end;

procedure TLogoAppForm.FileSaveAs1Execute(Sender: TObject);
begin
  If SaveDialog.FileName = '' then
    With SaveDialog do
      FileName := ChangeFileExt(FFileName, '.'+DefaultExt);
  if SaveDialog.Execute then
  begin
    if FileExists(SaveDialog.FileName) then
      if MessageDlg(Format(SOverwrite, [SaveDialog.FileName]),
        mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
        ListBoxPrg.Items.SaveToFile(SaveDialog.FileName);
//    RichEdit1.Modified := False;
  end;
end;

procedure TLogoAppForm.FileSend1Execute(Sender: TObject);
var
  MapiMessage: TMapiMessage;
  MError: Cardinal;
begin
  with MapiMessage do
  begin
    ulReserved := 0;
    lpszSubject := nil;
//    lpszNoteText := PChar(RichEdit1.Lines.Text);
    lpszMessageType := nil;
    lpszDateReceived := nil;
    lpszConversationID := nil; 
    flFlags := 0;
    lpOriginator := nil; 
    nRecipCount := 0;
    lpRecips := nil; 
    nFileCount := 0;
    lpFiles := nil; 
  end;

  MError := MapiSendMail(0, 0, MapiMessage, 
    MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0);
  if MError <> 0 then MessageDlg(SSendError, mtError, [mbOK], 0);
end;

procedure TLogoAppForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TLogoAppForm.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TLogoAppForm.LoadFromFileOUT(FName : String);
var
  f : file;
  BytesRead : integer;
begin
  if FPrgHEX<>nil then Dispose(FPrgHEX);
  AssignFile(f, FName);
  Reset(f, 1);
  FSize := FileSize(f);
  If FSize>0 then begin
    GetMem(FPrgHEX, FSize);
    BlockRead(f, FPrgHEX^, FSize, BytesRead);
    CloseFile(f);
  end else begin
    CloseFile(f);
    Exit;
  end;
end;

procedure TLogoAppForm.LoadFromFileHEX(FName : String);
var
  IntelHexFile : TIntelHex;
begin
  StatusBar.SimpleText := 'Lecture fichier au format Intel(R) Hex';
  if FPrgHEX<>nil then Dispose(FPrgHEX);
  IntelHexFile := TIntelHex.Create(FName);
  FSize := IntelHexFile.BinSize;
  If FSize>0 then begin
    GetMem(FPrgHEX, FSize);
    IntelHexFile.CopyToBuffer(FPrgHex, FSize);
    IntelHexFile.Free;
  end else begin
    IntelHexFile.Free;
    Exit;
  end;
end;

procedure TLogoAppForm.Pass1;
// première passe pour déterminer les labels
var
  i   : word;
begin
  if FSize=0 then exit;
  LabelSize.Caption := 'Taille programme : '+IntToStr(FSize div 2)+' mots';

  With ProgressDlg do begin
    Label1.Caption := '1ère passe...';
    Label2.Caption := 'Définition des symboles';
    ProgressBar1.Min := 0;
    ProgressBar1.Max := FSize div 2;
    ProgressBar1.Position := 1;
    Show;
  end;

  For i := 0 to pred(FSize div 2) do Begin
    ProgressDlg.Label2.Caption := ReverseCALM.GetCALM(FPrgHEX^ , i);
    ProgressDlg.ProgressBar1.Position := i;
    ProgressDlg.Update;
  end;

  With ProgressDlg do begin
    Label1.Caption := '2ère passe, formatage CALM';
    Label2.Caption := 'Définition des symboles';
    ProgressBar1.Min := 0;
    ProgressBar1.Max := FSize div 2;
    ProgressBar1.Position := 1;
    Update;
  end;

  ListBoxPrg.Items.Clear;
  ListBoxPrg.Items.Add(';'+Tab+'Reversed from '+FFileName);

  ReverseCALM.BuildSourceCALM(FPrgHEX^, FSize, ListBoxPrg.Items);

  ProgressDlg.Hide;
end;

procedure TLogoAppForm.FormCreate(Sender: TObject);
begin
  FPrgHEX := nil;
  FFileName := SUntitled;
end;

procedure TLogoAppForm.Button1Click(Sender: TObject);
begin
  if EditSymbolDlg.EditList(ReverseCALM.Labels) then Pass1;
end;

procedure TLogoAppForm.Button3Click(Sender: TObject);
begin
  if EditSymbolDlg.EditList(ReverseCALM.Symbols) then Pass1;
end;

procedure TLogoAppForm.EditCopy1Execute(Sender: TObject);
begin
  ClipBoard.AsText := ListBoxPrg.Items.Text;
end;

procedure TLogoAppForm.Button2Click(Sender: TObject);
begin
  if EditSymbolDlg.EditList(ReverseCALM.Registers) then Pass1;
end;

end.
