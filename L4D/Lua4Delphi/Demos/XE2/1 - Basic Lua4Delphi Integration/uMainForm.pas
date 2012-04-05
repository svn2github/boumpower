unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Layouts, FMX.Memo, FMX.TabControl, FMX.Edit, FMX.ListBox,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Ani,
  L4D.Engine.Main, L4D.Engine.StaticLua51;

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
    procedure btnExecuteClick(Sender: TObject);
    procedure Lua4Delphi1Exception(const AExceptionType: EL4DExceptionClass; AMessage: string; var ARaise: Boolean);
    procedure Lua4Delphi1LuaError(const ATitle, AMessage: string; const ALine: Integer; var ARaise: Boolean);
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
  private
    FPrints: Integer;
    procedure ShowExceptionPanel;
    procedure LuaPrint(var AStack: TL4DMethodStack); // < Called exclusively from Lua!
    procedure NewGlobalEnablement(const AEnabled: Boolean);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.LuaPrint(var AStack: TL4DMethodStack);
var
  I: Integer;
begin
  Inc(FPrints);
  for I := 1 to AStack.Count do
  begin
    if AStack[I].CanBeString then
      memOutput.Lines.Add(Format('Call %d - [Value %d] %s = %s', [FPrints, I, AStack[I].LuaTypeName, AStack[I].AsString]))
    else
      memOutput.Lines.Add(Format('Call %d - [Value %d] %s (cannot be printed)', [FPrints, I, AStack[I].LuaTypeName]));
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
end;

procedure TfrmMain.Lua4Delphi1Exception(const AExceptionType: EL4DExceptionClass; AMessage: string; var ARaise: Boolean);
begin
  lblException.Text := TimeToStr(Now) + ' [' + AExceptionType.ClassName + '] - ' + AMessage;
  ShowExceptionPanel;
end;

procedure TfrmMain.Lua4Delphi1LuaError(const ATitle, AMessage: string; const ALine: Integer; var ARaise: Boolean);
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
