unit uMainForm;

interface

{$I Lua4Delphi.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Layouts, FMX.Memo, FMX.TabControl, FMX.Edit, FMX.ListBox,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani,
  L4D.Engine.MainIntf, L4D.Engine.Main, L4D.Engine.StaticLua51{$IFDEF L4D_API_LOGGING}, L4D.Debug.Logging{$ENDIF};

type
  TfrmMain = class(TForm)
    memCode: TMemo;
    Splitter1: TSplitter;
    pnlTop: TPanel;
    btnExecute: TButton;
    memOutput: TMemo;
    OpenDialog1: TOpenDialog;
    btnLoadFromFile: TButton;
    tcTop: TTabControl;
    tabCode: TTabItem;
    tabGlobals: TTabItem;
    cpException: TCalloutPanel;
    Panel1: TPanel;
    StyleBook1: TStyleBook;
    lblException: TLabel;
    btnExceptionHide: TSpeedButton;
    edGlobalName: TEdit;
    Label1: TLabel;
    edGlobalValue: TEdit;
    Label2: TLabel;
    btnGlobalSetValue: TButton;
    cbGlobalType: TComboBox;
    Label3: TLabel;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    btnGlobalNew: TButton;
    btnGlobalCancel: TButton;
    GradientAnimation1: TGradientAnimation;
    Lua4Delphi1: TL4D51Static;
    Button1: TButton;
    Button2: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure Lua4Delphi1Exception(Sender: TObject; const AExceptionType: EL4DExceptionClass; AMessage: string; var ARaise: Boolean);
    procedure Lua4Delphi1LuaError(Sender: TObject; const ATitle, AMessage: string; const ALine: Integer; var ARaise: Boolean);
    procedure btnLoadFromFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnExceptionHideClick(Sender: TObject);
    procedure Lua4Delphi1Create(Sender: TObject);
    procedure btnGlobalNewClick(Sender: TObject);
    procedure btnGlobalCancelClick(Sender: TObject);
    procedure btnGlobalSetValueClick(Sender: TObject);
    procedure edGlobalNameChange(Sender: TObject);
    procedure cbGlobalTypeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FPrints: Integer;
    procedure ShowExceptionPanel;
    procedure LuaPrint(var AStack: IL4DMethodStack); // < Called exclusively from Lua!
    procedure LuaTableTest(var AStack: IL4DMethodStack); // < Called exclusively from Lua!
    procedure LuaTableRead(var AStack: IL4DMethodStack); // < Called exclusively from Lua!
    procedure NewGlobalEnablement(const AEnabled: Boolean);
    procedure LogCount;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.LogCount;
{$IFDEF L4D_API_LOGGING}
  const
    COUNT_STR = 'Top is %d %s';
    TYPE_STR = #13#10 + '%d - %s';
  var
    I: Integer;
    LTypes: String;
    LState: Boolean;
{$ENDIF}
begin
  {$IFDEF L4D_API_LOGGING}
    LState := L4DLogger.Active;
    L4DLogger.Active := False;
    for I := 1 to Lua4Delphi1.Engine.Lua.lua_gettop do
      LTypes := LTypes + Format(TYPE_STR, [-I, (Lua4Delphi1.Engine as IL4DEngineInternal).GetLuaTypeName(-I)]);
    L4DLogger.AddCustomMessage(Format(COUNT_STR, [Lua4Delphi1.Engine.Lua.lua_Gettop, LTypes]));
    L4DLogger.Active := LState;
  {$ENDIF}
end;

procedure TfrmMain.LuaPrint(var AStack: IL4DMethodStack);
var
  I: Integer;
begin
  Inc(FPrints);
  {$IFDEF L4D_API_LOGGING}L4DLogger.AddCustomMessage('Print #' + IntToStr(FPrints));{$ENDIF}
  LogCount;
  for I := 1 to AStack.Count do
  begin
    if AStack[I].CanBeString then
      memOutput.Lines.Add(Format('Call %d - [Value %d] %s = %s', [FPrints, I, AStack[I].LuaTypeName, AStack[I].AsString]))
    else
      memOutput.Lines.Add(Format('Call %d - [Value %d] %s (cannot be printed)', [FPrints, I, AStack[I].LuaTypeName]));
  end;
  LogCount;
end;

procedure TfrmMain.LuaTableRead(var AStack: IL4DMethodStack);
begin
  if AStack[1].CanBeTable then
  begin
    ShowMessage(AStack[1].AsTable['Hello'].AsString);
  end;
end;

procedure TfrmMain.LuaTableTest(var AStack: IL4DMethodStack);
begin
  with AStack.NewTable do
  begin
    PushString('Field1', 'Value1');
    PushString('Field2', 'Value2');
    PushString('Field3', 'Value3');
    PushString('Field4', 'Value4');
    PushFunction('print', LuaPrint);
    with NewTable('Field5') do
    begin
      PushString('FieldA', 'ValueA');
      PushString('FieldB', 'ValueB');
      PushString('FieldC', 'ValueC');
      PushString('FieldD', 'ValueD');
      Push;
    end;
    PushString('Field6', 'Value6');
    PushString('Field6', 'Value7');
    Push;
  end;
end;

procedure TfrmMain.NewGlobalEnablement(const AEnabled: Boolean);
begin
  btnGlobalNew.Enabled := (not AEnabled);
  btnGlobalCancel.Enabled := AEnabled;
  edGlobalValue.Enabled := AEnabled;
  cbGlobalType.Enabled := AEnabled;
end;

procedure TfrmMain.btnExecuteClick(Sender: TObject);
begin
  Lua4Delphi1.InjectLuaCode(memCode.Text, 'Lua4Delphi Sandbox Code')
end;

procedure TfrmMain.btnGlobalCancelClick(Sender: TObject);
begin
  NewGlobalEnablement(False);
end;

procedure TfrmMain.btnGlobalNewClick(Sender: TObject);
begin
  NewGlobalEnablement(True);
end;

procedure TfrmMain.btnGlobalSetValueClick(Sender: TObject);
begin
  case cbGlobalType.ItemIndex of
    2: Lua4Delphi1.Globals[AnsiString(edGlobalName.Text)].AsBoolean := (edGlobalValue.Text = 'true'); //'Boolean'
    4: Lua4Delphi1.Globals[AnsiString(edGlobalName.Text)].AsExtended := StrToFloat(edGlobalValue.Text); //'Number'
    5: Lua4Delphi1.Globals[AnsiString(edGlobalName.Text)].AsString := edGlobalValue.Text; //'String'
  else
    begin
      lblException.Text := 'You can only post Booleans, Numbers or Strings in this program';
      ShowExceptionPanel;
      Exit;
    end;
  end;
  btnGlobalCancelClick(Self);
end;

procedure TfrmMain.btnLoadFromFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Lua4Delphi1.InjectLuaFile(OpenDialog1.FileName);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  with Lua4Delphi1.Globals['Add'].CallFunction([10, 1337, 3.14159]) do
  begin
    memOutput.Lines.Add(IntToStr(Count) + ' - ' + Value[1].AsString);
    Cleanup;
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  {$IFDEF L4D_API_LOGGING}L4DLogger.Active := True;{$ENDIF}
  with Lua4Delphi1.Globals['ATable'].AsTable do
  begin
    with Value['a'].AsTable do
    begin
      with Value['c'].AsTable do
      begin
        with Value['c'].AsTable do
        begin
          memOutput.Lines.Add(Value['Field1'].AsString);
          memOutput.Lines.Add(Value['Field2'].AsString);
          Close; // Must be called to correctly align the Lua stack
        end;
        memOutput.Lines.Add(Value['a'].AsString);
        Close; // Must be called to correctly align the Lua stack
      end;
      memOutput.Lines.Add(Value['a'].AsString);
      Close; // As this is the end of the method, we need not necessarily call "Close" here, but it's best to call it anyway!
    end;
  end;
  {$IFDEF L4D_API_LOGGING}L4DLogger.Active := False;{$ENDIF}
end;

procedure TfrmMain.cbGlobalTypeChange(Sender: TObject);
const
  STYLE_NAME: Array[Boolean] of String = ('cbGlobalTypeStyleInvalid', 'cbGlobalTypeStyleHighlight');
begin
  if cbGlobalType.Enabled then
    cbGlobalType.StyleLookup := STYLE_NAME[(cbGlobalType.ItemIndex = 2) or (cbGlobalType.ItemIndex = 4) or (cbGlobalType.ItemIndex = 5)];
end;

procedure TfrmMain.edGlobalNameChange(Sender: TObject);
const
  STYLE_NAME: Array[Boolean] of String = ('', 'cbGlobalTypeStyleHighlight');
begin
  edGlobalValue.Text := Lua4Delphi1.Globals[AnsiString(edGlobalName.Text)].AsString;
  if btnGlobalNew.Enabled then
  begin
    cbGlobalType.ItemIndex := Integer(Lua4Delphi1.Globals[AnsiString(edGlobalName.Text)].LuaType) + 1;
    cbGlobalType.StyleLookup := STYLE_NAME[cbGlobalType.ItemIndex > 1];
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cpException.Position.Y := -51;
  FPrints := 0;
  tcTop.TabIndex := 0;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  cpException.Width := Width;
end;

procedure TfrmMain.Lua4Delphi1Create(Sender: TObject);
begin
  Lua4Delphi1.Globals.PushString('Symbols', '♫♪☺');
  Lua4Delphi1.Globals.PushFunction('print', LuaPrint);
  Lua4Delphi1.Globals.PushFunction('TableTest', LuaTableTest);
  Lua4Delphi1.Globals.PushFunction('TableRead', LuaTableRead);
  {$IFDEF L4D_API_LOGGINGz}L4DLogger.Active := True;{$ENDIF}
  with Lua4Delphi1.Globals.NewTable('ATable') do
  begin
    PushString('Hello', 'World');
    with NewTable('a') do
    begin
      PushDouble('a', 4);
      PushDouble('b', 2);
      with NewTable('c') do
      begin
        PushDouble('a', 55);
        PushDouble('b', 66);
        with NewTable('c') do
        begin
          PushString('Field1', 'Value1');
          PushString('Field2', 'Value2');
          PushFunction('TestMethod', LuaPrint);
          Push;
        end;
        Push;
      end;
      Push;
    end;
    with NewTable('b') do
    begin
      PushDouble('a', 13);
      PushDouble('b', 37);
      Push;
    end;
    Push;
  end;
  {$IFDEF L4D_API_LOGGINGz}L4DLogger.Active := False;{$ENDIF}
end;

procedure TfrmMain.Lua4Delphi1Exception(Sender: TObject; const AExceptionType: EL4DExceptionClass; AMessage: string; var ARaise: Boolean);
begin
  lblException.Text := TimeToStr(Now) + ' [' + AExceptionType.ClassName + '] - ' + AMessage;
  ShowExceptionPanel;
end;

procedure TfrmMain.Lua4Delphi1LuaError(Sender: TObject; const ATitle, AMessage: string; const ALine: Integer; var ARaise: Boolean);
begin
  lblException.Text := Format('%s [Lua Error] Title: "%s", Line: %d, Message: %s', [TimeToStr(Now), ATitle, ALine, AMessage]);
  ShowExceptionPanel;
end;

procedure TfrmMain.ShowExceptionPanel;
begin
  cpException.Width := Width;
  cpException.BringToFront;
  cpException.AnimateFloat('Position.Y', 0.10, 0.25);
  cpException.AnimateFloatDelay('Position.Y', -51.00, 0.50, 5.25);
end;

procedure TfrmMain.btnExceptionHideClick(Sender: TObject);
begin
  cpException.AnimateFloat('Position.Y', -51.00, 0.25);
end;

end.
