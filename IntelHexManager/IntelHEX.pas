unit IntelHEX;

interface

uses
  SysUtils, Math,  Classes, Contnrs, Windows, Graphics;

type
  EIntelHex = class(Exception)
  end;

type
  TReportMode = set of (rmVerbose, rmHexData, rmStrData);
  TOverlapMode = set of (ovlNone, ovlUnmodified, ovlOverwrite, ovlAlways);
  TOverflowMode = set of (ovfIgnore, ovfSplit, ovfCut, ovfAlways);
  TOverlapEvent = procedure(Sender : TObject; Address : cardinal; var ExistingData; const DataSource; Count : integer;
                            var OverlapMode : TOverlapMode) of object;
  TOverflowEvent = procedure(Sender : TObject; Address : cardinal; const Data; Count : integer;
                             const SectionName : string; RangeMin, RangeMax : cardinal;
                             var OverflowMode : TOverflowMode) of object;

type
  TIntelHexFile = class;
  TIntelHexRecord = class
  private
    FByteCount  : byte;
    FAddress    : cardinal;
    PData       : PByteArray;
  public
    constructor Create(Address : cardinal; const Data; Count : integer);
    destructor  Destroy; override;
    procedure   ReportStats(S : TStrings; var TotalBytes : cardinal; ReportMode : TReportMode);
    procedure   DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
  public
    property Address : cardinal read FAddress;
    property ByteCount : byte  read FByteCount;
    property DataPtr : PByteArray read PData;
  end;

  TIntelHexSection = class
  private
    FItems        : TObjectList;
    FSectionName  : string;
    FRangeMin     : cardinal;
    FRangeMax     : cardinal;
    FAddressMin   : cardinal;
    FAddressMax   : cardinal;
    FIgnore       : boolean;
    function GetItem(Index : integer) : TIntelHexRecord;
    function GetCount : integer;
  public
    constructor Create(SectionName : string; RangeMin, RangeMax : cardinal);
    destructor  Destroy; override;
    function    FindFirst(Offset : cardinal; Count : integer; var Rec : TIntelHexRecord) : boolean;
    function    FindNext(Const Rec : TIntelHexRecord; var NextRec : TIntelHexRecord) : boolean;
    procedure   Sort;
    procedure   AddData(Offset : cardinal; const Buffer; Count : integer);
    procedure   CopyTo(HexFile : TIntelHexFile; RelocateOffset : integer);
    procedure   ReportStats(S : TStrings; var TotalBytes : cardinal; ReportMode : TReportMode);
    procedure   DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
  public
    property Item[Index : integer] : TIntelHexRecord read GetItem;
    property Count : integer read GetCount;
    property SectionName : string read FSectionName write FSectionName;
    property RangeMin : cardinal read FRangeMin;
    property RangeMax : cardinal read FRangeMax;
    property AddressMin : cardinal read FAddressMin;
    property AddressMax : cardinal read FAddressMax;
    property Ignore : boolean read FIgnore write FIgnore;
  end;

  TIntelHexSections = class
  private
    FItems  : TObjectList;
    function  GetItem(Index : Integer) : TIntelHexSection;
    function  GetCount : integer;
  public
    constructor Create;
    destructor  Destroy; override;
    function   Add(SectionName : string; RangeMin, RangeMax : cardinal) : TIntelHexSection;
    function    FindSectionIndex(Address : cardinal) : integer;
    function    FindSection(Address : cardinal) : TIntelHexSection;
    function    FindSectionByName(const SectionName : string; var Dest : TIntelHexSection) : boolean;
    procedure   IgnoreSection(const SectionName : string);
    procedure   CopyTo(HexFile : TIntelHexFile; RelocateOffset : integer);
    procedure   ReportStats(S : TStrings; var TotalBytes : cardinal; ReportMode : TReportMode);
    procedure   DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
  public
    property Item[Index : integer] : TIntelHexSection read GetItem;
    property Count : integer read GetCount;
  end;

  TIntelHexFile = class(TComponent)
  private
    FFileName     : string;
    FStartAddress : cardinal;
    FSections : TIntelHexSections;
    FOverlapMode : TOverlapMode;
    FOnOverlap : TOverlapEvent;
    FOverflowMode : TOverflowMode;
    FOnOverflow : TOverflowEvent;
    FLog  : TStrings;
    FEOL  : string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AddData(Offset : cardinal; Nibbles : string); overload;
    procedure   AddData(Offset : cardinal; const Buffer; Count : integer); overload;
    procedure   CopyTo(HexFile : TIntelHexFile; RelocateOffset : integer);
    function    CopySectionTo(const SectionName : string; HexFile : TIntelHexFile; RelocateOffset : integer) : boolean;
    procedure   LoadFromFile(const FileName: string);
    procedure   LoadFromResource(const ResourceName: string);
    procedure   LoadFromStream(Stream: TStream);
    procedure   SaveToFile(const FileName : string);
    procedure   SaveToStream(Stream : TStream);
    procedure   ReportStats(S : TStrings; ReportMode : TReportMode);
    procedure   DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
  public
    property FileName : string read FFileName;
    property HexSections : TIntelHexSections read FSections;
    property Log : TStrings read FLog;
  published
    property EOL : string read FEOL write FEOL;
    property OverlapMode : TOverlapMode read FOverlapMode write FOverlapMode;
    property OverflowMode : TOverflowMode read FOverflowMode write FOverflowMode;
    property OnOverlap : TOverlapEvent read FOnOverlap write FOnOverlap;
    property OnOverflow : TOverflowEvent read FOnOverflow write FOnOverflow;
  end;

procedure DrawMemoryBlockOnMap(ABitMap : TBitMap; BytesOnLine : integer; Address : cardinal; Count : integer);
function BufferToHex(const Buffer; Count : integer) : string;
function BufferToString(const Buffer; Count : integer) : string;

implementation

constructor TIntelHexRecord.Create(Address : cardinal; const Data; Count : integer);
begin
  inherited Create;
  FAddress := Address;
  FByteCount := Count;
  GetMem(PData, Count);
  Move(Data, PData^, Count);
end;

destructor  TIntelHexRecord.Destroy;
begin
  FreeMem(PData);
  inherited Destroy;
end;

procedure TIntelHexRecord.ReportStats(S : TStrings; var TotalBytes : cardinal; ReportMode : TReportMode);
var
  Str : string;
begin
  Inc(TotalBytes, ByteCount);
  if not (rmVerbose in ReportMode) then exit;
  Str := Format('[0x%8.8x:0x%8.8x] (%3u bytes)',[Address, Address+ByteCount-1, ByteCount]);
  if rmHexData in ReportMode then Str := Str+' '+BufferToHex(DataPtr^, ByteCount);
  if rmStrData in ReportMode then Str := Str+' '+BufferToString(DataPtr^, ByteCount);
  S.Add(Str);
end;

procedure TIntelHexRecord.DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
begin
  DrawMemoryBlockOnMap(ABitmap, BytesOnLine, FAddress, FByteCount);
end;

{--------------------------------------------------------------------------------------------------------}

constructor TIntelHexSection.Create(SectionName : string; RangeMin, RangeMax : cardinal);
begin
  Inherited Create;
  FSectionName := SectionName;
  FRangeMin := Min(RangeMin, RangeMax);
  FRangeMax := Max(RangeMin, RangeMax);
  FAddressMin := High(FAddressMin);
  FAddressMax := Low(FAddressMax);
  FIgnore := false;
  FItems := TObjectList.Create;
end;

destructor  TIntelHexSection.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TIntelHexSection.GetItem(Index : integer) : TIntelHexRecord;
begin
  result := TIntelHexRecord(FItems[Index]);
end;

function TIntelHexSection.GetCount : integer;
begin
  result := FItems.Count;
end;

function  TIntelHexSection.FindFirst(Offset : cardinal; Count : integer; var Rec : TIntelHexRecord) : boolean;
var
  i : integer;
begin
  result := false;
  Rec := nil;
  Sort;
  i := 0;
  while i < FItems.Count do begin
    if Item[i].Address >= Offset+Count then exit;
    if Item[i].Address+Item[i].ByteCount-1 >= Offset then begin
      Rec := Item[i];
      result := true;
      exit;
    end;
    inc(i);
  end;
end;

function  TIntelHexSection.FindNext(Const Rec : TIntelHexRecord; var NextRec : TIntelHexRecord) : boolean;
var
  i : integer;
begin
  result := false;
  NextRec := nil;
  Sort;
  i := FItems.IndexOf(Rec);
  if (i = -1) or (i = Count-1) then exit;
  NextRec := Item[i+1];
  result := true;
end;

  function CompareFunc(Item1, Item2: Pointer): Integer;
  begin
    if TIntelHexRecord(Item1).Address = TIntelHexRecord(Item2).Address then
      result := 0
    else if TIntelHexRecord(Item1).Address < TIntelHexRecord(Item2).Address then
      result := -1
    else
      result := +1;
  end;

procedure TIntelHexSection.Sort;
begin
  FItems.Sort(CompareFunc);
end;



procedure TIntelHexSection.AddData(Offset : cardinal; const Buffer; Count : integer);
begin
  FAddressMin := Min(Offset, FAddressMin);
  FAddressMax := Max(Offset+Count-1, FAddressMax);
  FItems.Add(TIntelHexRecord.Create(Offset, Buffer, Count));
end;

procedure TIntelHexSection.CopyTo(HexFile : TIntelHexFile; RelocateOffset : integer);
var
  i : integer;
  TotalBytes : cardinal;
begin
  TotalBytes := 0;
  i := 0;
  while i<Count do begin
    HexFile.AddData(Item[i].Address+RelocateOffset, Item[i].DataPtr^, Item[i].ByteCount);
    inc(TotalBytes, Item[i].ByteCount);
    inc(i);
  end;
  if TotalBytes>0 then
    if RelocateOffset = 0 then
      HexFile.Log.Add(Format('Copy %s, %u bytes', [SectionName, TotalBytes]))
    else if RelocateOffset > 0 then
      HexFile.Log.Add(Format('Relocate %s RelocateOffset = 0x%x, %u bytes', [SectionName, RelocateOffset, TotalBytes]))
    else
      HexFile.Log.Add(Format('Relocate %s RelocateOffset = -0x%x, %u bytes', [SectionName, -RelocateOffset, TotalBytes]));
end;

procedure TIntelHexSection.ReportStats(S : TStrings; var TotalBytes : cardinal; ReportMode : TReportMode);
var
  i : integer;
  SectionBytes : cardinal;
  Report : string;
  ReportPos : integer;
begin
  ReportPos := S.Count; // keep position in S to insert report Title
  SectionBytes := 0;
  i := 0;
  while i<Count do begin
    Item[i].ReportStats(S, SectionBytes, ReportMode);
    inc(i);
  end;
  Report := Format('[%8.8x:%8.8x] %s ', [RangeMin, RangeMax, SectionName]);
  if Count>0 then begin
    report := report + Format('[%8.8x..%8.8x] : %u blocks, %u bytes', [AddressMin, AddressMax, Count, SectionBytes]);
    S.Insert(ReportPos, report);
  end else
    if rmVerbose in ReportMode then S.Insert(ReportPos, Report+'Empty section');
  Inc(TotalBytes, SectionBytes);
end;

procedure TIntelHexSection.DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
var
  i : integer;
begin
  i := 0;
  while i < FItems.Count do begin
    Item[i].DrawMemoryMap(ABitmap, BytesOnLine);
    Inc(i);
  end;
end;

{--------------------------------------------------------------------------------------------------------}

constructor TIntelHexSections.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  FItems.Add(TIntelHexSection.Create('N/A', 0, 0));
end;

destructor  TIntelHexSections.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TIntelHexSections.GetItem(Index : Integer) : TIntelHexSection;
begin
  result := TIntelHexSection(FItems[Index]);
end;

function  TIntelHexSections.GetCount : integer;
begin
  result := FItems.Count;
end;

function TIntelHexSections.Add(SectionName : string; RangeMin, RangeMax : cardinal) : TIntelHexSection;
begin
  result := nil;
  if RangeMin = RangeMax then begin
    raise EIntelHex.CreateFmt('IntelHexSections error : range overlap %u..%u', [RangeMin, RangeMax]);
    exit;// Only one default section
  end;
  if (FItems.Count=0) or ((FindSectionIndex(RangeMin)=0)and (FindSectionIndex(RangeMax)=0)) then
    result := TIntelHexSection(FItems.Add(TIntelHexSection.Create(SectionName, RangeMin, RangeMax)))
  else
    raise EIntelHex.CreateFmt('IntelHexSections error : range overlap 0x%x .. 0x%x', [RangeMin, RangeMax]);
end;

function TIntelHexSections.FindSectionIndex(Address : cardinal) : integer;
begin
  result := FItems.Count-1;
  if result <= 0 then exit; // Default section
  repeat
    with Item[result] do
      if (Address >= RangeMin) and (Address <= RangeMax) then exit; // Found
    Dec(result);
  until result=0;
end;

function TIntelHexSections.FindSection(Address : cardinal) : TIntelHexSection;
begin
  result := Item[FindSectionIndex(Address)];
end;

function TIntelHexSections.FindSectionByName(const SectionName : string; var Dest : TIntelHexSection) : boolean;
var
  i : integer;
begin
  result := false;
  i := 0;
  while i<Count do begin
    if CompareText(Item[i].SectionName, SectionName) = 0 then begin
      Dest := Item[i];
      result := true;
      exit;
    end;
    inc(i);
  end;
end;

procedure TIntelHexSections.IgnoreSection(const SectionName : string);
var
  Section : TIntelHexSection;
begin
  if FindSectionByName(SectionName, Section) then Section.Ignore := true;
end;


procedure  TIntelHexSections.CopyTo(HexFile : TIntelHexFile; RelocateOffset : integer);
var
  i : integer;
begin
  i := 0;
  while i < Count do begin
    if not Item[i].Ignore then Item[i].CopyTo(HexFile, RelocateOffset);
    Inc(i);
  end;
end;

procedure TIntelHexSections.ReportStats(S : TStrings; var TotalBytes : cardinal; ReportMode : TReportMode);
var
  i : integer;
begin
  i := 0;
  while i < Count do begin
    Item[i].ReportStats(S, TotalBytes, ReportMode);
    Inc(i);
  end;
end;

procedure TIntelHexSections.DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
var
  i : integer;
begin
  i := 0;
  while i < FItems.Count do begin
    Case i of
      0 : ABitmap.Canvas.Pen.Color := clRed;
      1 : ABitmap.Canvas.Pen.Color := clLime;
      2 : ABitmap.Canvas.Pen.Color := clGreen;
      3 : ABitmap.Canvas.Pen.Color := clBlue;
      else
          ABitmap.Canvas.Brush.Color := clBlack;
    end;
    Item[i].DrawMemoryMap(ABitmap, BytesOnLine);
    Inc(i);
  end;
end;

{--------------------------------------------------------------------------------------------------------}

constructor TIntelHexFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName := 'NoName.hex';
  FStartAddress := 0;
  FSections := TIntelHexSections.Create;
  FOverlapMode := [ovlNone];
  FOverflowMode := [ovfSplit];
  FOnOverlap := nil;
  FOnOverflow := nil;
  FLog := TStringList.Create;
  FEOL := #13#10;
end;

destructor  TIntelHexFile.Destroy;
begin
  FLog.Free;
  FSections.Free;
  inherited Destroy;
end;

procedure TIntelHexFile.LoadFromFile(const FileName: string);
var
  Stream : TFileStream;
begin
  FFileName := FileName;
  Log.Add('LoadFromFile : '+FFileName);
  Stream := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(Stream);
  Stream.Free;
end;

procedure TIntelHexFile.LoadFromResource(const ResourceName: string);
var
  Stream : TResourceStream;
begin
  if FindResource(HInstance, PChar(ResourceName), RT_RCDATA)<>0 then begin
    FFileName := ResourceName;
    Log.Add('LoadFromResource : '+FFileName);
    Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    LoadFromStream(Stream);
    Stream.Free;
  end;
end;


procedure TIntelHexFile.LoadFromStream(Stream: TStream);
const
  EOF_Found     = -1;
  IllegalChar   = -2;
  CheckSumWrong = -3;
  BadRecord     = -4;

var
  Offset      : cardinal;
  LineCounter : integer;
  RecStr      : string;

  function ReadRecordStr : integer;
  var
    Nibble  : char;
    i       : integer;
    Chk     : word;
  begin
    result := EOF_Found;
    RecStr := '';
    // Find start code ':'
    repeat
      if Stream.Read(Nibble, 1) < 1 then exit;  // EOF
    until Nibble = ':';

    // Read the record until end of record
    while true do begin
      if Stream.Read(Nibble, 1) < 1 then break;  // EOF, we will see if record is coorect...
      if Nibble in [#13, #10, #0] then begin
        Inc(LineCounter);
        break;    // we are at End of line
      end;
      if Nibble = ':' then begin
        Stream.Seek(-1, soFromCurrent);         // we are at begining of next record
        break;
      end;
      if Nibble in ['0'..'9', 'A'..'F', 'a'..'f'] then
        RecStr := RecStr + Nibble
      else begin
        result := IllegalChar;
        exit;
      end;
    end; // while

    // Now check if record size is correct...
    if (Length(RecStr)< 10) or Odd(Length(RecStr)) then begin
      result := BadRecord;
      exit;
    end;
    if Length(RecStr) - (StrToInt('$'+Copy(RecStr, 1, 2)) * 2) <> 10 then begin
      result := BadRecord;
      exit;
    end;

    // Now check checksum...
    Chk := 0;
    i := 1;
    repeat
      Chk := Chk + StrToInt('$'+Copy(RecStr, i, 2));
      i := i+2;
    until (i = Length(RecStr) -1 );
    Chk := ($0100 - (Chk and $00FF)) and $00FF;
    if Chk <> StrToInt('$'+Copy(RecStr, i, 2)) then begin
      result := CheckSumWrong;
      exit;
    end;
    // Finally extract Record type
    result := StrToInt('$'+Copy(RecStr, 7, 2));
  end;

  function GetWordAt(Index : integer) : word;
  begin
    result := StrToInt('$'+ Copy(RecStr, Index, 4));
  end;

begin
  Offset := 0;
  LineCounter := 1;
  while true do begin
    Case ReadRecordStr of
      EOF_Found :  begin
          raise EIntelHex.Create('IntelHex read error : EOF ');
          exit;
        end;
      IllegalChar :  begin
          raise EIntelHex.CreateFmt('IntelHex read error : Illegal char in line %u %s', [LineCounter, RecStr]);
          exit;
        end;
      CheckSumWrong : begin
          raise EIntelHex.CreateFmt('IntelHex read error : CheckSum in line %u %s', [LineCounter, RecStr]);
          exit;
        end;
      BadRecord : begin
          raise EIntelHex.CreateFmt('IntelHex read error : Bad record in line %u %s', [LineCounter, RecStr]);
          exit;
        end;
      $00 : begin // Data record
          AddData(Offset+GetWordAt(3), Copy(RecStr, 9, Length(RecStr)-10));
        end;
      $01 : begin // EOF record
          FStartAddress := GetWordAt(3);
          exit;
        end;
      $02 : begin // Extended Segement
          MessageBox(0, PChar(RecStr), 'Extended Segment', MB_OK);
        end;
      $03 : begin // Start Segment Address
          MessageBox(0, PChar(RecStr), 'Start Segment Address', MB_OK);
        end;
      $04 : begin // Extended Linear Address
          Offset := GetWordAt(9) shl 16;
//          MessageBox(0, PChar('0x'+IntToHex(Offset, 8)), 'Extended Linear Address', MB_OK);
        end;
      $05 : begin // Start Linear Address
          MessageBox(0, PChar(RecStr), 'Start Linear Address', MB_OK);
        end;
      else begin
          raise EIntelHex.CreateFmt('IntelHex read error : Bad record type in line %u %s', [LineCounter, RecStr]);
          exit;
        end;
    end;
  end;
end;

procedure TIntelHexFile.SaveToFile(const FileName : string);
var
  Stream : TFileStream;
begin
  FFileName := FileName;
  Log.Add('SaveFromFile : '+FFileName);
  Stream := TFileStream.Create(FileName, fmCreate);
  SaveToStream(Stream);
  Stream.Free;
end;

procedure TIntelHexFile.SaveToStream(Stream : TStream);
var
  i, j : integer;
  Offset : word;
  SwapedOffset : word;
  Buffer : ShortString;

  function HexRecord(ByteCount : byte; Address : word; RecType : byte; PData : PByteArray) : string;
  var
    Chk    : word;
    i      : integer;
  begin
    Chk := 0;
    result := Format(':%2.2x%4.4x%2.2x', [ByteCount, Address, RecType]);
    Chk := ByteCount+Lo(Address)+Hi(Address)+RecType;
    i := 0;
    while i<ByteCount do begin
      result := result+IntToHex(PData^[i], 2);
      Inc(Chk, PData^[i]);
      inc(i);
    end;
    Chk := ($0100 - (Chk and $00FF)) and $00FF;
    result := Uppercase(result + IntToHex(Chk, 2));
  end;

begin
  Offset := $FFFF; // Force Extended Linear Addres record
  i := 0;
  while i < HexSections.Count do begin
    HexSections.Item[i].Sort;
    j := 0;
    while j < HexSections.Item[i].Count do begin
      with HexSections.Item[i].Item[j] do begin
        if (Address shr 16) <> Offset then begin
          // Change Extended Linear Address
          Offset := Address shr 16;
          SwapedOffset := Swap(Offset);
          Buffer := HexRecord(2, 0, $04, @SwapedOffset)+EOL;
          Stream.WriteBuffer(Buffer[1], Length(Buffer));
        end;
        Buffer := HexRecord(ByteCount, Address and $FFFF, $00, DataPtr)+EOL;
        Stream.WriteBuffer(Buffer[1], Length(Buffer));
      end;
      inc(j);
    end;
    inc(i);
  end;
  Buffer := HexRecord(0, 0, $01, nil)+EOL;
  Stream.WriteBuffer(Buffer[1], Length(Buffer));
end;


procedure TIntelHexFile.AddData(Offset : cardinal; Nibbles : string);
var
  Buffer : array[0..255] of byte;
  i : integer;
begin
  i := 0;
  while i < Length(Nibbles) div 2 do begin
    Buffer[i] := StrToInt('$'+Copy(Nibbles, Succ(i*2), 2));
    inc(i);
  end;
  AddData(Offset, Buffer, Length(Nibbles) div 2);
end;

procedure TIntelHexFile.AddData(Offset : cardinal; const Buffer; Count : integer);
var
  Section : TIntelHexSection;
  HexRec  : TIntelHexRecord;
  Count1, Remainder : integer;
begin
  if Count = 0 then exit;
  Section := FSections.FindSection(Offset);
  if Section = FSections.FindSection(Offset+Count-1) then begin
    // Check if overlapping...
    if Section.FindFirst(Offset, Count, HexRec) then begin
      if Assigned(FOnOverlap) and not (ovlAlways in FOverlapMode) then
        FOnOverlap(self, Offset, HexRec.DataPtr^, Buffer, Count, FOverlapMode);
    end else begin
      Section.AddData(Offset, Buffer, Count);
//      Log.Add(Format('Add %3u bytes [0x%8x..0x%8x] %s %s', [Count, Offset, Offset+Count-1, BufferToHex(Buffer, Count), BufferToString(Buffer, Count)]));
    end;
  end else begin // Split Data to fit in section
    if Assigned(FOnOverflow) and not (ovfAlways in FOverflowMode) then
      FOnOverflow(self, Offset, Buffer, Count,
                  Section.SectionName, Section.RangeMin, Section.RangeMax, FOverflowMode);
    Count1 := Section.RangeMax-Offset;
    Remainder := Count-Count1;
    if ovfSplit in FOverflowMode then begin
      // Split
      Log.Add('==> Split Data Begin');
      AddData(Offset, Buffer, Count1);
      AddData(Offset+Count1+1, TByteArray(Buffer)[Count1], Remainder);
      Log.Add('==> Split Data End');
    end else if ovfCut in FOverflowMode then begin
      // Cut
      AddData(Offset, Buffer, Count1);
      Log.Add(Format('==> Drop out %u bytes', [Remainder]));
    end else begin
      // Ignore
      Log.Add(Format('==> Drop out %u bytes', [Count]));
    end;
  end;
end;

procedure TIntelHexFile.CopyTo(HexFile : TIntelHexFile; RelocateOffset : integer);
begin
  HexSections.CopyTo(HexFile, RelocateOffset);
end;

function TIntelHexFile.CopySectionTo(const SectionName : string; HexFile : TIntelHexFile; RelocateOffset : integer) : boolean;
var
  Section : TIntelHexSection;
begin
  result := HexSections.FindSectionByName(SectionName, Section);
  if not result then exit;
  Section.CopyTo(HexFile, RelocateOffset);
end;


procedure TIntelHexFile.ReportStats(S : TStrings; ReportMode : TReportMode);
var
  TotalBytes : cardinal;
begin
  TotalBytes := 0;
  S.Add(Format('Stats "%s"', [FFileName]));
  HexSections.ReportStats(S, TotalBytes, ReportMode);
  S.Add(Format('%u bytes in "%s"', [TotalBytes, ExtractFileName(FFileName)]));
  S.Add('');
end;

procedure TIntelHexFile.DrawMemoryMap(ABitMap : TBitMap; BytesOnLine : integer);
begin
  FSections.DrawMemoryMap(ABitmap, BytesOnLine);
end;

{--------------------------------------------------------------------------------------------------------}

procedure DrawMemoryBlockOnMap(ABitmap : TBitmap; BytesOnLine : integer; Address : cardinal; Count : integer);
const
  XMult = 1;
  YMult = 2;
var
  X1, Y1 : Integer;     // From
  FirstLineLeft : integer;
  i : integer;
begin
  Y1 := (ABitmap.Height-1) - (Address div BytesOnLine)*YMult; // inverted
  FirstLineLeft := (Address mod BytesOnLine);
  X1 := FirstLineLeft * XMult;
  if FirstLineLeft+Count <= BytesOnLine then
    // Just one segment on the line
    for i := 0 to pred(YMult) do begin
      ABitmap.Canvas.MoveTo(X1, Y1-i);
      ABitmap.Canvas.LineTo(X1+(Count*XMult), Y1-i);
    end
  else begin
    // First line
    for i := 0 to pred(YMult) do begin
      ABitmap.Canvas.MoveTo(X1, Y1-i);
      ABitmap.Canvas.LineTo(BytesOnLine*XMult, Y1-i);
    end;
    Dec(Y1, YMult); // Next Line
    Dec(Count, BytesOnLine-FirstLineLeft); // We have already drawn that...
    while Count div BytesOnLine > 0 do begin // Full lines
      for i := 0 to pred(YMult) do begin
        ABitmap.Canvas.MoveTo(0, Y1-i);
        ABitmap.Canvas.LineTo(BytesOnLine*XMult, Y1-i);
      end;
      Dec(Y1, YMult); // Next Line
      Dec(Count, BytesOnLine); // We have already drawn that...
    end;
    // Last line
    for i := 0 to pred(YMult) do begin
      ABitmap.Canvas.MoveTo(0, Y1-i);
      ABitmap.Canvas.LineTo(Count*XMult, Y1-i);
    end;
  end;
end;

function BufferToHex(const Buffer; Count : integer) : string;
var
  i : integer;
begin
  result := '';
  i := 0;
  while i < Count do begin
    if i = 0 then
      result := Format('0x%2x', [TByteArray(Buffer)[0]])
    else
      result := result + Format(' 0x%2x', [TByteArray(Buffer)[i]]);
    inc(i);
  end;
end;

function BufferToString(const Buffer; Count : integer) : string;
var
  i : integer;
begin
  result := '';
  i := 0;
  while i < Count do begin
    if char(TByteArray(Buffer)[i]) in [' '..'}'] then
      result := result + char(TByteArray(Buffer)[i])
    else
      result := result + '~';
    inc(i);
  end;
end;

end.
