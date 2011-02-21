unit W2kMain;

interface

uses Windows, Classes, Graphics, Forms, Controls, Menus,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList, StdActns,
  ActnList, ToolWin, JvComponentBase, JvHidControllerClass,
  PICkit2, LMDControl, LMDBaseControl, LMDBaseGraphicControl,
  LMDGraphicControl, LMDLEDCustomLabel, LMDLEDLabel, JvExStdCtrls,
  JvHtControls, JvgListBox, JvListComb,
  Lua, LuaWrapper, CPortCtl, CPort, SDSOS;

type
  TWin2kAppForm = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionList1: TActionList;
    FileNew1: TAction;
    FileOpen1: TAction;
    FileSave1: TAction;
    FileSaveAs1: TAction;
    FileSend1: TAction;
    FileExit1: TAction;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    HelpAbout1: TAction;
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    N1: TMenuItem;
    FileSendItem: TMenuItem;
    N2: TMenuItem;
    FileExitItem: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    PopupMenu1: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    JvHidDeviceController1: TJvHidDeviceController;
    VDDLabel: TLMDLEDLabel;
    FirmwareLabel: TJvHTLabel;
    RefreshBtn: TBitBtn;
    ResetBtn: TBitBtn;
    HistoryList: TJvImageListBox;
    RunScriptBtn: TButton;
    ComComboBox1: TComComboBox;
    ComPort1: TComPort;
    SpeedButton1: TSpeedButton;
    StatusBox: TJvgCheckListBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ComTerminal1: TComTerminal;
    ComPort2: TComPort;
    ComComboBox2: TComComboBox;
    SpeedButton2: TSpeedButton;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileSaveAs1Execute(Sender: TObject);
    procedure FileSend1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function JvHidDeviceController1Enumerate(HidDev: TJvHidDevice;
      const Idx: Integer): Boolean;
    procedure JvHidDeviceController1Arrival(HidDev: TJvHidDevice);
    procedure FormDestroy(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure HistoryListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RunScriptBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
  private
    FTerminalServer : TSmallDeviceTerminalOS;
    FFileName: string;
  public
    procedure DisplayScriptMsg(Sender : TObject; const Msg : string);
    procedure AfterAddFirmwareCommand(Sender : TPICkit2; const ID : TFirmwareCommands; const CommandText : string);
    procedure AfterReadFirmware(Sender : TPICkit2; const Major, Minor, dot : byte);
    procedure AfterReadStatus(Sender : TPICkit2; const Status : word);
    procedure AfterReadVoltages(Sender : TPICkit2; const VDD, VPP : real);
  end;

var
  Win2kAppForm: TWin2kAppForm;
  Lua : TLua;
  KIT : TPICkit2;

implementation

uses
  SysUtils, Mapi, about, SHFolder;

{$R *.dfm}

resourcestring
  SUntitled  = 'Sans titre';
  SOverwrite = 'OK pour remplacer %s';
  SSendError = 'Erreur à l''envoi du courrier';
  SRefreshBtn = 'Rafraichir';
  SRefreshBtnHint = 'Lit la version du Firmware et le Status';
  SResetBtn = 'Reset';
  SResetBtnHint = 'Reset du PICkit2';
  SHistoryListHint = 'Double click pour afficher les détails';

function DefaultSaveLocation: string;
var
  P: PChar;
begin
  {
    renvoie l'emplacement de 'Mes Documents' s'il existe, sinon renvoie
    le répertoire en cours.
  }
  P := nil;
  try
    P := AllocMem(MAX_PATH);
    if SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, P) = S_OK then
      Result := P
    else
      Result := GetCurrentDir;
  finally
    FreeMem(P);
  end;
end;

procedure TWin2kAppForm.DisplayScriptMsg(Sender : TObject; const Msg : string);
begin
  HistoryList.Items.InsertText(0, Msg);
end;

procedure TWin2kAppForm.FileNew1Execute(Sender: TObject);
begin
  SaveDialog.InitialDir := DefaultSaveLocation;
  FFileName := SUntitled;
end;

procedure TWin2kAppForm.FileOpen1Execute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FFileName := OpenDialog.FileName;
    KIT.Scripts.LoadScriptsFromFile(FFileName);
    if KIT.Scripts.Count<>0 then begin
      RunScriptBtn.Enabled := true;
      DisplayScriptMsg(nil, 'Loaded : '+FFileName);
    end;
  end;
end;

procedure TWin2kAppForm.FileSave1Execute(Sender: TObject);
begin
  if (FFileName = SUntitled) or (FFileName = '') then
    FileSaveAs1Execute(Sender)
  else
  begin
  end;
end;

procedure TWin2kAppForm.FileSaveAs1Execute(Sender: TObject);
begin
  with SaveDialog do
  begin
    FileName := FFileName;
    if Execute then
    begin
      if FileExists(FileName) then
        if MessageDlg(Format(SOverwrite, [FileName]),
          mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
      FFileName := FileName;
    end;
  end;
end;

procedure TWin2kAppForm.FileSend1Execute(Sender: TObject);
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

  MError := MapiSendMail(0, Application.Handle, MapiMessage,
    MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0);
  if MError <> 0 then MessageDlg(SSendError, mtError, [mbOK], 0);
end;

procedure TWin2kAppForm.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TWin2kAppForm.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TWin2kAppForm.FormCreate(Sender: TObject);
begin
  FTerminalServer := TSmallDeviceTerminalOS.Create(SmallDeviceDemoStream, ComPort1);
//  FComPort.OnRxChar := ComPortRxChar;
  RefreshBtn.Caption := SRefreshBtn;
  RefreshBtn.Hint := SRefreshBtnHint;
  ResetBtn.Caption := SResetBtn;
  ResetBtn.Hint := SResetBtnHint;
  HistoryList.Hint := SHistoryListHint;
  Lua := TLua.Create(Self);
  HistoryList.Items.InsertText(0, LUA_VERSION + ' '+LUA_COPYRIGHT);
  KIT := TPICkit2.Create(self);
  KIT.OnAddFirmwareCommand := AfterAddFirmwareCommand;
  KIT.OnAfterReadFirmwareVersion := AfterReadFirmware;
  KIT.OnAfterReadStatus := AfterReadStatus;
  KIT.OnAfterReadVoltages := AfterReadVoltages;
  FileNew1.Execute; { définit le nom de fichier par défaut et efface le contrôle RichEdit }
end;

function TWin2kAppForm.JvHidDeviceController1Enumerate(
  HidDev: TJvHidDevice; const Idx: Integer): Boolean;
begin
  HistoryList.Items.InsertText(0, 'Enumerate : ' + HidDev.ProductName);
end;

procedure TWin2kAppForm.JvHidDeviceController1Arrival(
  HidDev: TJvHidDevice);
begin
  HistoryList.Items.InsertText(0, 'Arrival : ' + HidDev.ProductName
    +' '+HidDev.SerialNumber);
  if HidDev.ProductName = 'PICkit 2 Microcontroller Programmer' then begin
    HistoryList.Items.InsertText(0, 'Arrival : ' + HidDev.ProductName);
    KIT.HidDevice := HidDev;
//    KIT.Scripts.OnDebugMsg := DisplayScriptMsg;
    KIT.Scripts.OnWarningMsg := DisplayScriptMsg;
    KIT.Scripts.OnErrorMsg := DisplayScriptMsg;
    KIT.SetVDD(5.0);
    KIT.ReadFirmware(DelayedResponse);
    KIT.ReadStatus(DelayedResponse);
    KIT.ReadVoltages;
    KIT.ReadResponses;
    KIT.FlushFirmwareCommands;
  end;
end;

procedure TWin2kAppForm.FormDestroy(Sender: TObject);
begin
  KIT.Free;
  Lua.Free;
end;

procedure TWin2kAppForm.AfterReadFirmware(Sender : TPICkit2; const Major, Minor, dot : byte);
begin
  FirmwareLabel.Caption := Format('Firmware %d.%d.%d', [Major, Minor, Dot]);
  HistoryList.Items.InsertText(0, FirmwareLabel.Caption);
end;

procedure TWin2kAppForm.AfterReadStatus(Sender : TPICkit2; const Status : word);
var
  i : integer;
begin
  i := 0;
  while i<StatusBox.Count do begin
    if (Status and (1 shl i)) <> 0 then
      StatusBox.Checked[i] := cbChecked
    else
      StatusBox.Checked[i] := cbUnChecked;
    inc(i);
  end;
  StatusBox.Refresh;
  HistoryList.Items.InsertText(0, 'Status : $' + IntToHex(Status, 4));
end;

procedure TWin2kAppForm.AfterReadVoltages(Sender : TPICkit2; const VDD, VPP : real);
begin
  VDDLabel.Caption := FloatToStr(VDD);
  HistoryList.Items.InsertText(0, Format('Voltages : VDD = %g VPP := %g', [VDD, VPP]));
end;

procedure TWin2kAppForm.AfterAddFirmwareCommand(Sender : TPICkit2; const ID : TFirmwareCommands; Const CommandText : string);
begin
  HistoryList.Items.InsertText(0, CommandText);
end;


procedure TWin2kAppForm.RefreshBtnClick(Sender: TObject);
begin
  KIT.ReadFirmware(DelayedResponse);
  KIT.ReadVoltages;
  KIT.ReadStatus;
end;

procedure TWin2kAppForm.ResetBtnClick(Sender: TObject);
begin
  KIT.Reset;
end;

procedure TWin2kAppForm.HistoryListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i : integer;
begin
  i := HistoryList.ItemAtPos(Point(X, Y), true);
  HistoryList.Hint := 'Mouse over '+IntToStr(i);
end;

procedure TWin2kAppForm.RunScriptBtnClick(Sender: TObject);
begin
  DisplayScriptMsg(nil, 'Execute Script');
  KIT.ExecuteScript(KIT.Scripts.Scripts[KIT.Scripts.Count-1].ScriptData);
  DisplayScriptMsg(nil, 'Status   = ' + IntToHex(KIT.Status, 4));
  KIT.FlushFirmwareCommands;
end;

procedure TWin2kAppForm.SpeedButton1Click(Sender: TObject);
begin
  if SpeedButton1.Down then begin
    ComComboBox1.Enabled := false;
    FTerminalServer.Run;
  end else begin
    FTerminalServer.Stop;
    ComComboBox1.Enabled := true;
  end;
end;

procedure TWin2kAppForm.SpeedButton2Click(Sender: TObject);
begin
  if SpeedButton2.Down then begin
    ComTerminal1.Connected := true;
    ComComboBox2.Enabled := false;
  end else begin
    ComTerminal1.Connected := false;
    ComComboBox2.Enabled := true;
  end;
end;

procedure TWin2kAppForm.TabSheet2Show(Sender: TObject);
begin
  ComTerminal1.SetFocus;
end;

end.
