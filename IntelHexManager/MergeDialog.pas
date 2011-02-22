unit MergeDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls,
  Buttons, IntelHEX;

type
  TMergeDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    SrcList: TListBox;
    DstList: TListBox;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    SourceBox: TComboBox;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure SourceBoxChange(Sender: TObject);
  private
    { déclarations privées }
  public
    { déclarations publiques }
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
    procedure SetSrcList(Source : TIntelHexFile);
    function Execute(Sources : TList; Dest : TIntelHexFile) : integer;
  end;

var
  MergeDlg: TMergeDlg;

implementation

{$R *.dfm}

procedure TMergeDlg.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  DstList.Items.AddObject(Format('[%s]%s', [SourceBox.Text , SrcList.Items[Index]]),
                          SrcList.Items.Objects[Index]);
  SrcList.Items.Delete(Index);
  SetItem(SrcList, Index);
end;

procedure TMergeDlg.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  DstList.Items.Delete(Index);
  SetSrcList(TIntelHexFile(SourceBox.Items.Objects[SourceBox.ItemIndex]));
  SetItem(DstList, Index);
end;

procedure TMergeDlg.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
end;

function TMergeDlg.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

procedure TMergeDlg.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
    else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;

procedure TMergeDlg.SetSrcList(Source : TIntelHexFile);
var
  i : integer;
begin
  SrcList.Clear;
  i := 0;
  while i < Source.HexSections.Count do begin
    if Source.HexSections.Item[i].Count>0 then
      if DstList.Items.IndexOf(Format('[%s]%s', [ExtractFileName(Source.FileName),Source.HexSections.Item[i].SectionName])) = -1 then
        SrcList.AddItem(Source.HexSections.Item[i].SectionName, Source.HexSections.Item[i]);
    inc(i);
  end;
  SetButtons;
end;

function TMergeDlg.Execute(Sources : TList; Dest : TIntelHexFile) : integer;
var
  i,j : integer;
  Offset : integer;
begin
  SourceBox.Items.Clear;
  i := 0;
  while i < Sources.Count do begin
    SourceBox.Items.AddObject(ExtractFileName(TIntelHexFile(Sources[i]).FileName), Sources[i]);
    inc(i);
  end;
  if Sources.Count>0 then begin
    SetSrcList(TIntelHexFile(Sources[0]));
    SourceBox.ItemIndex := 0;
  end;

  result := ShowModal;
  if result <> mrOK then exit;
  i := 0;
  while i < DstList.Items.Count do begin
    j := 0;
    while j< TIntelHexSection(DstList.Items.Objects[i]).Count do begin
      with TIntelHexSection(DstList.Items.Objects[i]).Item[j] do
          Dest.AddData(Address, DataPtr^, ByteCount);
      inc(j);
    end;
    inc(i);
  end;
end;

procedure TMergeDlg.SourceBoxChange(Sender: TObject);
begin
  SetSrcList(TIntelHexFile(SourceBox.Items.Objects[SourceBox.ItemIndex]));
end;

end.
