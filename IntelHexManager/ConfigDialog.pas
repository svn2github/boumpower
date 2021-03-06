unit ConfigDialog;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, IniFiles, StrUtils, Contnrs;

type
  TPICConfigDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
  private
  public
    procedure CreatePages(Ini : TMemIniFile);
  end;

var
  PICConfigDlg: TPICConfigDlg;

implementation

{$R *.dfm}

{
  The Tag field of Control contains :
  For CheckBox :
  - bit# int the byte
  For ComboBox :
  - LSB byte : LSB bit# of config bits in the byte
  - next byte : MSB bit# of config bits in the byte
  The ComboBox Items.Object contain the value of bits for the corresponding option
}

type // centralize bit change event and notify other components
  TConfigSheetData = class(TComponent)
  private
    FControls : TComponentList;
    FBits : TBits;
    function GetAsByte : byte;
    procedure SetAsByte(Value : byte);
  protected
    procedure BitChangeEvent(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    Destructor  Destroy; override;
    procedure RegisterControl(AControl : TControl);
    property AsByte : byte read GetAsByte write SetAsByte;
  end;

constructor TConfigSheetData.Create(AOwner: TComponent);
begin
  Inherited Create(Aowner);
  FControls := TComponentList.Create(false);
  FBits := TBits.Create;
  FBits.Size := 8;
end;

Destructor  TConfigSheetData.Destroy;
begin
  FBits.Free;
  FControls.Free;
  inherited Destroy;
end;

function TConfigSheetData.GetAsByte : byte;
var
  i : integer;
begin
  result := 0;
  for i := 0 to 7 do
    if FBits[i] then result := result or (1 shl i);
end;

procedure TConfigSheetData.SetAsByte(Value : byte);
var
  i : integer;
begin
  for i := 0 to 7 do
    FBits[i]:=  (Value and (1 shl i)) <> 0;
  BitChangeEvent(nil);
end;


procedure TConfigSheetData.BitChangeEvent(Sender : TObject);
var
  i,j : integer;
  b : byte;
  Lsb : integer;
  Msb : integer;
begin
  if Assigned(Sender) then begin
    if Sender is TCheckBox then
      with Sender as TCheckBox do begin
        FBits[Tag] := Checked;
      end
    else if Sender is TComboBox then
      with Sender as TComboBox do begin
        Lsb := Tag mod 256;
        Msb := Tag div 256;
        if ItemIndex<>-1 then b := Byte(Items.Objects[ItemIndex]) shl Lsb;
        for j := Lsb to Msb do
          FBits[j] := (b and (1 shl j)) <> 0;
      end;
  end;
  // Notify
  i := 0;
  while i < FControls.Count do begin
    if FControls[i] <> Sender then begin
      if FControls[i] is TCheckBox then
        with FControls[i] as TCheckBox do Checked := FBits[Tag];
      if FControls[i] is TComboBox then
        with FControls[i] as TComboBox do begin
          // Extract bits
          b := 0;
          Lsb := Tag mod 256;
          Msb := Tag div 256;
          for j := Lsb to Msb do
            if FBits[j] then b := b or (1 shl (j-Lsb));
          ItemIndex := Items.IndexOfObject(TObject(b));
          if ItemIndex=-1 then Text := Format('Unknow configuration bits : 0x%x', [b]);
        end;
    end;
    inc(i);
  end;
end;


procedure TConfigSheetData.RegisterControl(AControl : TControl);
begin
  FControls.Add(AControl);
  if AControl is TCheckBox then (AControl as TCheckBox).OnClick := self.BitChangeEvent;
  if AControl is TComboBox then (AControl as TComboBox).OnChange := self.BitChangeEvent;
end;


procedure TPICConfigDlg.CreatePages(Ini : TMemIniFile);
var
  Sections  : TStringList;
  Bits      : TStringList;
  ConfigSheetData : TConfigSheetData;
  i,j,k : integer;
  TabSheet : TTabSheet;
  AControl : TControl;
  BitName  : string;
  Value    : integer;
  BitMin, BitMax : integer;
  PointPos : integer;
  CommaPos : integer;
  MinusPos : integer;
  PosY     : integer;
begin
  Sections := TStringList.Create;
  Bits := TStringList.Create;
  Caption := Ini.ReadString('ROOT', 'Proc', 'Unknow processor');
  // Create pages
  Ini.ReadSections(Sections);
  i := 0;
  while i < Sections.Count do begin
    if AnsiStartsText('CONFIG_REGISTER.', Sections[i]) then begin
      TabSheet := TTabSheet.Create(PageControl1);
      TabSheet.PageControl := PageControl1;
      TabSheet.Caption := Copy(Sections[i], Pos('.', Sections[i])+1, 255);
      ConfigSheetData := TConfigSheetData.Create(TabSheet);

      AControl := TLabel.Create(TabSheet);
      TabSheet.Tag := Ini.ReadInteger(Sections[i], 'Address', 0);
      With AControl as TLabel do begin
        Parent := TabSheet;
        Left := 210;
        Top := 5;
        Caption := Format('0x%x', [TabSheet.Tag]);
      end;
      for j := 7 downto 0 do begin
        AControl := TCheckBox.Create(TabSheet);
        With AControl as TCheckBox do begin
          Name := Format('_BIT%u', [j]);
          Caption := '';
          Tag := j;
          Parent := TabSheet;
          Top := 5;
          Left := 180-(25*j);
          Width := 15;
          Enabled := false;
        end;
        ConfigSheetData.RegisterControl(AControl);
      end;
      Ini.ReadSectionValues(Sections[i], Bits);
      Bits.Values['Address'] := '';
      PosY := 40;
      j := 0;
      while j < Bits.Count do begin
        PointPos := Pos('.', Bits.Names[j]);
        if PointPos = 0 then begin
          // Create the control
          BitName := Bits.Names[j];
          CommaPos := Pos(',', Bits.ValueFromIndex[j]);
          if CommaPos <> 0 then begin
            AControl := TCheckBox.Create(TabSheet);
            with AControl as TCheckbox do begin
              Tag := StrToIntDef(Copy(Bits.ValueFromIndex[j], 1, CommaPos-1), -1);
              if Tag = -1 then raise EConvertError.CreateFmt('Configuration bit %s', [BitName]);
              Caption := BitName+' : '+Trim(Copy(Bits.ValueFromIndex[j], CommaPos+1, 255));
              if Tag in [0..7] then
                With TabSheet.FindChildControl(Format('_BIT%u', [AControl.Tag])) as TCheckBox do begin
                  Hint := BitName;
                  Enabled := true;
                end;
            end;
          end else begin
            AControl := TComboBox.Create(TabSheet);
            with AControl as TComboBox do begin
              Style := csDropDownList;
              MinusPos := Pos('-', Bits.ValueFromIndex[j]);
              if MinusPos = 0 then begin
                BitMin := StrToIntDef(Bits.ValueFromIndex[j], -1);
                BitMax := BitMin;
                if BitMin < 0 then raise EConvertError.CreateFmt('Configuration bit %s', [BitName]);
                if BitMin in [0..7] then
                  With TabSheet.FindChildControl(Format('_BIT%u', [BitMin])) as TCheckBox do begin
                    Hint := BitName;
                    Enabled := true;
                  end;
              end else begin
                BitMax := StrToIntDef(Copy(Bits.ValueFromIndex[j], 1, MinusPos-1), -1);
                BitMin := StrToIntDef(Copy(Bits.ValueFromIndex[j], MinusPos+1, 255), -1);
                if BitMin > BitMax then begin
                  Value := BitMin;
                  BitMin := BitMax;
                  BitMax := Value;
                end;
                if BitMin < 0 then raise EConvertError.CreateFmt('Configuration bit %s', [BitName]);
                for k :=BitMin to BitMax do
                  if k in [0..7] then
                    With TabSheet.FindChildControl(Format('_BIT%u', [k])) as TCheckBox do begin
                      Hint := Format('%s%u', [BitName, k-BitMin]);
                      Enabled := true;
                    end;
              end;
              Tag := BitMin+256*BitMax;
            end;
          end;
          AControl.Name := BitName;
          AControl.Parent := TabSheet;
          AControl.Top := PosY;
          AControl.Width := 350;
          ConfigSheetData.RegisterControl(AControl);
          Inc(PosY, 25);
        end else begin
          // Add value to the
          BitName := Copy(Bits.Names[j], 1, PointPos-1);
          Value := StrToIntDef(Copy(Bits.Names[j], PointPos+1, 255), -1);
          if Value = -1 then raise EConvertError.CreateFmt('Configuration bit %s', [Bits.Names[j]]);
          AControl := TabSheet.FindChildControl(BitName);
          if Assigned(AControl) and (AControl is TComboBox) then
            with AControl as TComboBox do begin
              Items.AddObject(Bits.ValueFromIndex[j], TObject(Value));
            end;
        end;
        Inc(j);
      end;
      ConfigSheetData.AsByte := $FF;
    end;
    inc(i);
  end;
  Bits.Free;
  Sections.Free;
end;


end.

