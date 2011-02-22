unit W2kMain;

interface

uses Windows, Classes, Graphics, Forms, Controls, Menus,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList, StdActns,
  ActnList, ToolWin, RelocateDialog, MergeDialog, BootloaderDialog,
  IntelHex, JvExControls, JvxCheckListBox, Mask, JvExMask, JvToolEdit,
  JvCombobox, JvExComCtrls, JvComCtrls, JvCheckTreeView, JvComponentBase,
  JvAppStorage, JvAppIniStorage, Internal, IniFiles, Contnrs, ConfigDialog;

type
  TRelocationItem = class
  private
    FNumBlocks  : word;
    FSrcAddr    : cardinal;
    FDstAddr    : cardinal;
    function GetAsLittleEndian24Hex : string;
  public
    constructor Create(NumBlocks : word; SrcAddr : cardinal; DstAddr : cardinal);
    property    AsLittleEndian24Hex : string read GetAsLittleEndian24Hex;
  end;
  TRelocationItemList = class(TObjectList)
    procedure AddItem(NumBlocks : word; SrcAddr : cardinal; DstAddr : cardinal);
    procedure CopyTo(Address : cardinal; HexFile : TIntelHexFile);
  end;

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
    RichEdit1: TRichEdit;
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
    Image1: TImage;
    HexTree: TJvCheckTreeView;
    HexMerge: TAction;
    Fusionne1: TMenuItem;
    HexRelocate: TAction;
    Relocation1: TMenuItem;
    ProfileStorage: TJvAppIniFileStorage;
    FileProjectFromFile: TMenuItem;
    N3: TMenuItem;
    FileNewBootloaderUpgade: TAction;
    FileNewUpgradeBootloader: TMenuItem;
    HexConfig: TAction;
    ToolButton10: TToolButton;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileSaveAs1Execute(Sender: TObject);
    procedure FileSend1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HexMergeExecute(Sender: TObject);
    procedure HexRelocateExecute(Sender: TObject);
    procedure HexTreeSelectionChange(Sender: TObject);
    procedure FileNewBootloaderUpgadeExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HexConfigExecute(Sender: TObject);
  private
    FFileName: String;
    ResultHexFile        : TIntelHexFile;
    RelocationItems : TRelocationItemList;
  protected
    procedure HexDataOverlap(Sender : TObject; Address : cardinal; var ExistingData; const DataSource; Count : integer;
                            var OverlapMode : TOverlapMode);
    procedure HexDataOverflow(Sender : TObject; Address : cardinal; const Data; Count : integer;
                             const SectionName : string; RangeMin, RangeMax : cardinal;
                             var OverflowMode : TOverflowMode);
  public
    procedure CreateSectionsFromResourceDef(HexFile : TIntelHexFile; const ResourceName : string);
  end;

var
  Win2kAppForm: TWin2kAppForm;

implementation

uses
  SysUtils, Mapi, about, SHFolder;

{$R *.dfm}

resourcestring
  SUntitled  = 'Sans titre';
  SOverwrite = 'OK pour remplacer %s';
  SSendError = 'Erreur à l''envoi du courrier';

constructor TRelocationItem.Create(NumBlocks : word; SrcAddr : cardinal; DstAddr : cardinal);
begin
  inherited Create;
  FNumBlocks := NumBlocks;
  FSrcAddr := SrcAddr;
  FDstAddr := DstAddr;
end;

  function ToLittleEndianHex(x : cardinal; Digits : integer) : string;
  var
    B : TByteArray absolute x;
    i : integer;
  begin
    if Odd(Digits) then raise EConvertError.Create('BigToLittleEndianHex Odd digits number');
    if (Digits < 8) and (B[3] <> 0) then raise EConvertError.Create('Cardinal to INT24');
    if (Digits < 6) and ((B[3] <> 0) or (B[2] <> 0)) then raise EConvertError.Create('Cardinal to INT16');
    if (Digits < 4) and (x > 255) then raise EConvertError.Create('Cardinal to INT8');
    result := '';
    for i := 0 to Pred(Digits div 2) do
      result := result + IntToHex(B[i], 2);
  end;

function TRelocationItem.GetAsLittleEndian24Hex : string;
begin
  result := ToLittleEndianHex(FNumBlocks, 4)+    // 16 bits
            ToLittleEndianHex(FSrcAddr, 6)+      // 24 bits
            ToLittleEndianHex(FDstAddr, 6);      // 24 bits
end;

procedure TRelocationItemList.AddItem(NumBlocks : word; SrcAddr : cardinal; DstAddr : cardinal);
begin
  Add(TRelocationItem.Create(NumBlocks, SrcAddr, DstAddr));
end;

procedure TRelocationItemList.CopyTo(Address : cardinal; HexFile : TIntelHexFile);
var
  i : integer;
  s : string;
begin
  i := 0;
  while i < Count do begin
    s := TRelocationItem(Items[i]).AsLittleEndian24Hex;
    HexFile.AddData(Address, s);
    HexFile.Log.Add(Format('  %s %u bytes', [s, Length(s) div 2]));
    Inc(Address, Length(s) div 2);
    Inc(i);
  end;
end;


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

procedure TWin2kAppForm.FileNew1Execute(Sender: TObject);
begin
  SaveDialog.InitialDir := DefaultSaveLocation;
  FFileName := SUntitled;
  RichEdit1.Lines.Clear;
  RichEdit1.Modified := False;
  RichEdit1.Lines.Add(ProfileStorage.AsString);
end;

procedure TWin2kAppForm.FileOpen1Execute(Sender: TObject);
var
  HexFile : TIntelHexFile;
  i : integer;
begin
  if OpenDialog.Execute then
  begin
    i := 0;
    while i < OpenDialog.Files.Count do begin
      FFileName := OpenDialog.Files[i];
      HexFile := TIntelHexFile.Create(self);
//      CreateSections(HexFile);
      HexFile.LoadFromFile(FFileName);

      HexTree.Items.AddObject(nil, ExtractFileName(FFileName), HexFile);
      inc(i);
    end;
  end;
end;

procedure TWin2kAppForm.FileSave1Execute(Sender: TObject);
begin
  if (FFileName = SUntitled) or (FFileName = '') then
    FileSaveAs1Execute(Sender)
  else
  begin
    FileSaveAs1Execute(Sender)
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
      ResultHexFile.SaveToFile(FileName);
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
    lpszNoteText := PChar(RichEdit1.Lines.Text);
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
  ProfileStorage.FileName := 'PIC18F4550.ini';
  ProfileStorage.Reload;
  RelocationItems := TRelocationItemList.Create(True);
//  FileNew1.Execute; { définit le nom de fichier par défaut et efface le contrôle RichEdit }
end;

procedure TWin2kAppForm.CreateSectionsFromResourceDef(HexFile : TIntelHexFile; const ResourceName : string);
var
  Def         : TMemIniFile;
  S           : TStrings;
  Path        : string;
  NumSections : integer;
  i           : integer;
  Section     : TIntelHexSection;
begin
  Def := TMemIniFile.Create('');
  S := TStringList.Create;
  if not ReadResource(ResourceName, S) then begin
    Application.MessageBox(PChar(Format('Resource missing : %s', [ResourceName])), 'Error', MB_OK+MB_ICONERROR);
    exit;
  end;
  Def.SetStrings(S);

  i := 0;
  NumSections := Def.ReadInteger('SECTIONS', 'Count', 0);
  while i < NumSections do begin
    Path := Format('SECTION.%u', [i]);
    Section := HexFile.HexSections.Add(
        Def.ReadString(Path, 'SectionName', 'NoName'),
        Def.ReadInteger(Path, 'RangeMin', 0),
        Def.ReadInteger(Path, 'RangeMax', 0)
      );
    inc(i);
  end;

  S. Free;
  Def.Free;
  HexFile.OnOverlap := self.HexDataOverlap;
  HexFile.OnOverflow := self.HexDataOverflow;
end;



procedure TWin2kAppForm.HexDataOverlap(Sender : TObject; Address : cardinal; var ExistingData; const DataSource; Count : integer;
                         var OverlapMode : TOverlapMode);
begin
  case Application.MessageBox(PChar(Format('Overlap Offset 0x%x, Count %u : replace ?', [Address, Count])),
                            'Warning : Data Overflow', MB_YESNO + MB_ICONQUESTION	) of
    IDYES    : OverlapMode := OverlapMode + [ovlOverwrite] - [ovlUnmodified];
    IDNO     : OverlapMode := OverlapMode + [ovlUnmodified] - [ovlOverwrite];
  end;
end;

procedure TWin2kAppForm.HexDataOverflow(Sender : TObject; Address : cardinal; const Data; Count : integer;
                          const SectionName : string; RangeMin, RangeMax : cardinal;
                          var OverflowMode : TOverflowMode);
begin
  case Application.MessageBox(PChar(Format('Data don''t fit in section %s Offset 0x%x, Count %u : Split ?', [SectionName, Address, Count])),
                            'Warning : Data Overflow', MB_YESNOCANCEL + MB_ICONQUESTION	) of
    IDCANCEL : OverflowMode := OverflowMode + [ovfIgnore] - [ovfSplit, ovfCut];
    IDYES    : OverflowMode := OverflowMode + [ovfSplit] - [ovfCut, ovfIgnore];
    IDNO     : OverflowMode := OverflowMode + [ovfCut] - [ovfSplit, ovfIgnore];
  end;
end;


procedure TWin2kAppForm.HexMergeExecute(Sender: TObject);
var
  ResultHex : TIntelHexFile;
  ResultNode : TTreeNode;
  TmpList : TList;
  HexList : TList;
  i : integer;
begin
  // Merge checked files in tree
  ResultHex := TIntelHexFile.Create(self);
//  CreateSections(ResultHex);
  ResultNode := HexTree.Items.AddObject(nil, 'Merged files', ResultHex);

  // Build the list of selected files
  TmpList := TList.Create;
  HexList := TList.Create;
  i := 0;
  while i < HexTree.Items.Count do begin
    if HexTree.Checked[HexTree.Items[i]] then begin
      TmpList.Add(HexTree.Items[i]);
      HexList.Add(HexTree.Items[i].Data);
    end;
    HexTree.Checked[HexTree.Items[i]] := false;    // Uncheck
    inc(i);
  end;

  MergeDlg.Execute(HexList, ResultHex);

  // Update the tree
  i := 0;
  while i < TmpList.Count do begin
    with TTreeNode(TmpList.Items[i]) do begin
 //     TIntelHexFile(Data).MergeTo(ResultHex);
      MoveTo(ResultNode, naAddChild);
    end;
    inc(i);
  end;
  HexList.Free;
  TmpList.Free;

  // Uncheck all...
  i := 0;
  while i <  HexTree.Items.Count do begin
    HexTree.Checked[HexTree.Items[i]] := false;    // Uncheck
    inc(i);
  end;
end;

procedure TWin2kAppForm.HexRelocateExecute(Sender: TObject);
var
  ResultHex : TIntelHexFile;
  RelocatePatchHex : TIntelHexFile;
  ResultNode : TTreeNode;
  i : integer;
begin
  if HexTree.Selected = nil then exit;
  ResultHex := TIntelHexFile.Create(self);
  RelocatePatchHex := TIntelHexFile.Create(self);
//  CreateSections(ResultHex);
//  CreateSections(RelocatePatchHex);
  
  if RelocateDialog.RelocateDlg.Execute(TIntelHexFile(HexTree.Selected.Data), ResultHex) = mrOK then begin
    ResultNode := HexTree.Items.AddObject(nil, 'Relocated', ResultHex);
    HexTree.Selected.MoveTo(ResultNode, naAddChild);
    // Create the Patch to relocate
    HexTree.Items.AddChildObject(ResultNode, 'Relocate patch', RelocatePatchHex);
  end else begin
    ResultHex.Free;
    RelocatePatchHex.Free;
  end;


  // Uncheck all...
  i := 0;
  while i <  HexTree.Items.Count do begin
    HexTree.Checked[HexTree.Items[i]] := false;    // Uncheck
    inc(i);
  end;
end;

procedure TWin2kAppForm.HexTreeSelectionChange(Sender: TObject);
var
  MemoryMap : TBitmap;
begin
  if HexTree.Selected = nil then exit;
  // Erase
  MemoryMap := TBitmap.Create;
  MemoryMap.Width := Image1.Width;
  MemoryMap.Height := Image1.Height;
  MemoryMap.Canvas.Brush.Color := clWhite;
  MemoryMap.Canvas.Pen.Color := clWhite;
  MemoryMap.Canvas.Rectangle(MemoryMap.Canvas.ClipRect);
  TIntelHexFile(HexTree.Selected.Data).DrawMemoryMap(MemoryMap, 128);
  Image1.Stretch := false;
  Image1.Picture.Assign(MemoryMap);
  MemoryMap.Free;

  RichEdit1.Lines.Clear;
//  TIntelHexFile(HexTree.Selected.Data).Report(RichEdit1.Lines);
  RichEdit1.Lines.Assign(TIntelHexFile(HexTree.Selected.Data).Log);
  TIntelHexFile(HexTree.Selected.Data).ReportStats(RichEdit1.Lines, []);
  RichEdit1.SetFocus;
  RichEdit1.Modified := False;
  RichEdit1.ReadOnly := ofReadOnly in OpenDialog.Options;
end;

procedure TWin2kAppForm.FileNewBootloaderUpgadeExecute(Sender: TObject);
var
  Settings             : TMemIniFile;
//  ResultHexFile        : TIntelHexFile;
  RelocateHexFile      : TIntelHexFile;
  BootloaderHexFile    : TIntelHexFile;
  ApplicationHexFile   : TIntelHexFile;
  ConfigHexFile        : TIntelHexFile;
  ResultNode           : TTreeNode;
  ConfigNode             : TTreeNode;
  S                    : TStrings;
  FlashBlockSize       : integer;
  RelocationItem       : TRelocationItem;

begin
  // Load project settings
  Settings := TMemIniFile.Create('');
  S := TStringList.Create;
  if not ReadResource('BOOTLOADER_UPGRADE.INI', S) then begin
    Application.MessageBox('Resource missing : BOOTLOADER_UPGRADE.INI', 'Error', MB_OK+MB_ICONERROR);
    exit;
  end;
  Settings.SetStrings(S);

  FlashBlockSize := Settings.ReadInteger('RESULT', 'FlashBlockSize', 0);
  if FlashBlockSize = 0 then begin
    Application.MessageBox('Illegal FlashBlockSize : BOOTLOADER_UPGRADE.INI', 'Error', MB_OK+MB_ICONERROR);
    exit;
  end;

  if BootloaderDlg.ShowModal <> mrOK then exit;

  // Build the project
  ResultHexFile := TIntelHexFile.Create(self);
  CreateSectionsFromResourceDef(ResultHexFile, Settings.ReadString('RESULT', 'Sections', 'BOOTLOADER_UPGRADE_SECTIONS.INI'));
  ResultNode := HexTree.Items.AddObject(nil, BootloaderDlg.FileSelectCB.Text, ResultHexFile);

  // Relocating code
  RelocateHexFile := TIntelHexFile.Create(self);
  CreateSectionsFromResourceDef(RelocateHexFile, Settings.ReadString('RESULT', 'Sections', 'BOOTLOADER_UPGRADE_SECTIONS.INI'));
  if BootloaderDlg.IsResourceFile[0] then
    RelocateHexFile.LoadFromResource(BootloaderDlg.FileName[0])
  else
    RelocateHexFile.LoadFromFile(BootloaderDlg.FileName[0]);
  relocateHexFile.HexSections.IgnoreSection('Relocation Records');
  ResultHexFile.Log.Add(Format('==> Add relocating code from %s...', [RelocateHexFile.FileName]));
  RelocateHexFile.HexSections.IgnoreSection('Config');
  RelocateHexFile.CopyTo(ResultHexFile, 0);
  HexTree.Items.AddChildObjectFirst(ResultNode, BootloaderDlg.RelocSelectCB.Text, RelocateHexFile);

  // Bootloader
  BootloaderHexFile := TIntelHexFile.Create(self);
  CreateSectionsFromResourceDef(BootloaderHexFile,
    Settings.ReadString(BootloaderDlg.BootloaderType, 'Sections', 'BOOTLOADER_xxx_SECTIONS.INI'));
  if BootloaderDlg.IsResourceFile[1] then
    BootloaderHexFile.LoadFromResource(BootloaderDlg.FileName[1])
  else
    BootloaderHexFile.LoadFromFile(BootloaderDlg.FileName[1]);
  HexTree.Items.AddChildObject(ResultNode, BootloaderDlg.FileSelectCB.Text, BootloaderHexFile);

  if CompareText(BootloaderDlg.BootloaderType, 'MCHPUSB')=0 then begin
  end else  if CompareText(BootloaderDlg.BootloaderType, 'HID')=0 then begin
      // Application - Protect vectors or load application
      ApplicationHexFile := TIntelHexFile.Create(self);
      CreateSectionsFromResourceDef(ApplicationHexFile,
        Settings.ReadString(BootloaderDlg.BootloaderType, 'Sections', 'BOOTLOADER_xxx_SECTIONS.INI'));
      if BootloaderDlg.IsResourceFile[2] then
        ApplicationHexFile.LoadFromResource(BootloaderDlg.FileName[2])
      else
        ApplicationHexFile.LoadFromFile(BootloaderDlg.FileName[2]);
      HexTree.Items.AddChildObject(ResultNode, 'Application code', ApplicationHexFile);


      // Select Configuration
      if (BootloaderDlg.ConfigIndex <> 0) and (BootloaderDlg.ConfigIndex < 2) then ResultHexFile.Log.Add('==> Add configurations bits...');
      if BootloaderDlg.ConfigIndex = 1 then BootloaderHexFile.CopySectionTo('Config', ResultHexFile, 0);
      if BootloaderDlg.ConfigIndex = 2 then ApplicationHexFile.CopySectionTo('Config', ResultHexFile, 0);
      if BootloaderDlg.ConfigIndex > 2 then begin
        ConfigHexFile := TIntelHexFile.Create(self);
        CreateSectionsFromResourceDef(ConfigHexFile, Settings.ReadString('RESULT', 'Sections', 'BOOTLOADER_UPGRADE_SECTIONS.INI'));
        if BootloaderDlg.IsResourceFile[3] then
          ConfigHexFile.LoadFromResource(BootloaderDlg.FileName[3])
        else
          ConfigHexFile.LoadFromFile(BootloaderDlg.FileName[3]);
        ResultHexFile.Log.Add(Format('==> Add configurations bits from %s...', [ConfigHexFile.FileName]));
        ConfigHexFile.CopySectionTo('Config', ResultHexFile, 0);
        ConfigNode := HexTree.Items.AddChildObject(ResultNode, 'Configuration bits', ConfigHexFile);
      end;

      // Build code....
      ResultHexFile.Log.Add(Format('==> Add relocated code from %s and %s ...', [BootloaderHexFile.FileName, ApplicationHexFile.FileName]));
      BootloaderHexFile.HexSections.IgnoreSection('Config');
      ApplicationHexFile.HexSections.IgnoreSection('Config');
      BootloaderHexFile.CopyTo(ResultHexFile, $4000);
      ApplicationHexFile.CopyTo(ResultHexFile, $4000);

      ResultHexFile.Log.Add('==> Add relocation records...');
      RelocationItems.Clear;
      RelocationItems.AddItem(($1040-$0000+FlashBlockSize-1) div FlashBlockSize, $4000, $0000);
      RelocationItems.AddItem(0, 0, 0);
      RelocationItems.CopyTo($3F00, ResultHexFile);
  end else if CompareText(BootloaderDlg.BootloaderType, 'VASCO')=0 then begin
      // Select Configuration
      if (BootloaderDlg.ConfigIndex <> 0) and (BootloaderDlg.ConfigIndex < 2) then ResultHexFile.Log.Add('==> Add configurations bits...');
      if BootloaderDlg.ConfigIndex = 1 then BootloaderHexFile.CopySectionTo('Config', ResultHexFile, 0);
      if BootloaderDlg.ConfigIndex = 2 then ApplicationHexFile.CopySectionTo('Config', ResultHexFile, 0);
      if BootloaderDlg.ConfigIndex > 2 then begin
        ConfigHexFile := TIntelHexFile.Create(self);
        CreateSectionsFromResourceDef(ConfigHexFile, Settings.ReadString('RESULT', 'Sections', 'BOOTLOADER_UPGRADE_SECTIONS.INI'));
        if BootloaderDlg.IsResourceFile[3] then
          ConfigHexFile.LoadFromResource(BootloaderDlg.FileName[3])
        else
          ConfigHexFile.LoadFromFile(BootloaderDlg.FileName[3]);
        ResultHexFile.Log.Add(Format('==> Add configurations bits from %s...', [ConfigHexFile.FileName]));
        ConfigHexFile.CopySectionTo('Config', ResultHexFile, 0);
        ConfigNode := HexTree.Items.AddChildObject(ResultNode, 'Configuration bits', ConfigHexFile);
      end;

      // Build code....
      ResultHexFile.Log.Add(Format('==> Add relocated code from %s ...', [BootloaderHexFile.FileName]));
      BootloaderHexFile.CopySectionTo('Bootloader', ResultHexFile, $4000);
      BootloaderHexFile.CopySectionTo('Application', ResultHexFile, $2100-$2000);
      BootloaderHexFile.CopySectionTo('ISR', ResultHexFile, $3000-$4000);

      ResultHexFile.Log.Add('==> Add relocation records...');
      RelocationItems.Clear;
      RelocationItems.AddItem(($2000-$0000+FlashBlockSize-1) div FlashBlockSize, $4000, $0000);
      RelocationItems.AddItem(($2100-$2000+FlashBlockSize-1) div FlashBlockSize, $2100, $2000);
      RelocationItems.AddItem(($4100-$4000+FlashBlockSize-1) div FlashBlockSize, $3000, $4000);
      RelocationItems.AddItem(0, 0, 0);
      RelocationItems.CopyTo($3F00, ResultHexFile);
  end;

  S.Free;
  Settings.Free;
end;

procedure TWin2kAppForm.FormDestroy(Sender: TObject);
begin
  RelocationItems.Free;
  FreeAndNil(ResultHexFile);
end;

procedure TWin2kAppForm.HexConfigExecute(Sender: TObject);
var
  S : TStringList;
  Ini : TMemIniFile;
begin
  S := TStringList.Create;
  Ini := TMemIniFile.Create('');
  ReadResource('PIC18F4550.INI', S);
  Ini.SetStrings(S);
  PICConfigDlg.CreatePages(Ini);
  PICConfigDlg.ShowModal;
  Ini.Free;
  S.Free;
end;

end.
