unit CncShellDlgs;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Grids, ValEdit, ComCtrls, Math;

type
  TLineAlignement = (laUndefined, laOdd, laEven, laNone);

type
  TMetadataDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    MetaEditor: TValueListEditor;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Image1: TImage;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FLineEnding : string;
    FLineAlignement : TLineAlignement;
  public
    function  ScanFile(const FileName : TFileName) : boolean; virtual;
    function  Execute(const FileName : TFileName) : boolean;
    procedure AddMetadata(const KeyName, Desc, Value, Mask: string; MaxLen : integer; CanEdit : boolean);
  end;

var
  MetadataDlg: TMetadataDlg;

implementation

{$R *.dfm}

function TMetadataDlg.ScanFile(const FileName : TFileName) : boolean;
const
  BufferSize = 2048;
  LineDelim = [#10, #13];
var
  s          : TFileStream;
  Buffer     : PChar;
  BytesRead  : integer;
  i          : integer;
begin
  result := false;
  Buffer := StrAlloc(BufferSize+1);
  s := TFileStream.Create(FileName, fmOpenRead);
  BytesRead := s.Read(Buffer[0], BufferSize);
  Buffer[BytesRead] := #0;
  s.Free;

  // Check for line ending
  FLineEnding := '';
  i :=0;
  while (i<Min(BytesRead, 255)) and not (Buffer[i] in LineDelim) do inc(i);
  while (i<Min(BytesRead, 255)) and (Buffer[i] in LineDelim) do begin
    FLineEnding := FLineEnding+Buffer[i];
    inc(i);
  end;
  if Length(FLineEnding)=0 then exit; // no line found....

  // Check for line alignement
  FLineAlignement := laUndefined;

  StrDispose(Buffer);
  result := true;
end;


function TMetadataDlg.Execute(const FileName : TFileName) : boolean;
begin
  result := false;
  OKBtn.Enabled := ScanFile(FileName);
  Case ShowModal of
    mrOK : result := true;
    mrCancel : result := false;
  end;
end;


procedure TMetadataDlg.FormCreate(Sender: TObject);
begin
  AddMetadata('Machine',   'Machine', '', '', 18, true);
  AddMetadata('Customer',  'Client', '', '', 18, true);
  AddMetadata('Object',    'Objet', '', '', 18, true);
  AddMetadata('Drawing',   'Plan', '', '', 18, true);
  AddMetadata('Thumbnail', 'Aperçu', '', '', 18, false);
end;

procedure TMetadataDlg.AddMetadata(const KeyName, Desc, Value, Mask: string; MaxLen : integer; CanEdit : boolean);
begin
  MetaEditor.InsertRow(KeyName, Value, true);
  with MetaEditor.ItemProps[KeyName] do begin
    KeyDesc := Desc;
    MaxLength := MaxLen;
    EditMask := Mask;
    ReadOnly := not CanEdit;
  end;
end;


end.
