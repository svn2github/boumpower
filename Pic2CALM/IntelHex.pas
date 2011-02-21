unit IntelHex;

interface

uses SysUtils, Windows, Math;

const
  DataRecord         = $00;
  EofRecord          = $01;
  ExtSegAddrRecord   = $02;  // pas implémenté
  StartSegAddrRecord = $03;  // pas implémenté
  ExtLinAddrRecord   = $04;  // pas implémenté
  StartLinAddrRecord = $05;  // pas implémenté


{$A-}
Type
  TIntelHexRec = record
    RecLen     : byte;
    Offset     : word;
    RecType    : byte;
    Data       : array[0..255+1] of byte;
  end;
{$A+}

Type
  TIntelHex = class(Tobject)
      function    ASCII2Byte(S : String) : byte;
      procedure   Str2IntelHexRec(S : string; var Rec : TIntelHexRec);
      procedure   CopyToBuffer(Buffer : PByteArray; BufferSize : word);
      constructor Create(FName : string);
      destructor  Destroy; override;
      procedure   GotError(ProcName, Msg : string);
    private
      f         : TextFile;
      fBinSize  : word;
    published
      property BinSize : word read fBinSize;
  end;

implementation


// Conversion de 2 nibbles texte en byte
function TIntelHex.ASCII2Byte(S : string) : byte;
Const
  HEXA = '0123456789ABCDEF';
begin
  If Length(S) <> 2 then GotError('ASCII2Byte', S);
  Result := 16*pred(Pos(S[1], HEXA))+pred(Pos(S[2], HEXA));
end;

// Conversion d'un bloc IntelHEX dans un Buffer
procedure TIntelHex.Str2IntelHexRec(S : string; var Rec : TIntelHexRec);
var
  i      : word;
  P      : ^TByteArray;
  ChkSum : byte;
begin
  P := @Rec.RecLen;
  S := Trim(S);
  If Length(S)=0 then GotError('Str2IntelRec', 'vide');
  If S[1] <> ':' then GotError('Str2IntelRec', '":" manque"'+S+'"');
  Delete(S, 1, 1);
  If Length(S)=0 then GotError('Str2IntelRec', 'vide');
  If Pos(':', S)<>0 then GotError('Str2IntelRec', '":" double"'+S+'"');
  If (Length(S) mod 2) <> 0 then GotError('Str2IntelRec', 'impaire"'+S+'"');
  If (Length(S) div 2) > SizeOf(Rec) then GotError('Str2IntelRec', 'trop grand"'+S+'"');
  i := 0;
  while i<Length(S) do begin { Conversion du block en Bin}
    P^[i div 2] := ASCII2Byte(Copy(S, succ(i), 2));
    Inc(i, 2);
  end;
  With Rec do begin
    ChkSum := RecLen+Lo(Offset)+Hi(Offset)+RecType;
    for i := 0 to RecLen do ChkSum := ChkSum+Data[i];
    If ChkSum<>0 then GotError('Str2IntelRec', 'ChkSum"'+S+'"');
  end;
  Rec.Offset:= Swap(Rec.Offset);
end;

constructor TIntelHEX.Create(FName : String);
var
  S   : string;
  Rec : TIntelHexRec;
begin
  inherited Create;
  fBinSize := 0;
  AssignFile(f, FName);
  Reset(f);
  While Not Eof(f) do begin
    ReadLn(f, S);
    Str2IntelHexRec(S, Rec);
    With Rec do
      Case RecType of
        DataRecord         : if Offset+RecLen<2*1024 then fBinSize := Max(fBinSize, Offset+RecLen+1);
        EofRecord          : ;
      else
        GotError('Create', 'Rec="'+S+
            '" RecLen='+IntToHex(RecLen, 2)+
            ' Offset='+IntToHex(Offset, 4)+
            ' RecType='+IntToHex(RecType, 2));
      end;
  end;
end;

procedure TIntelHex.CopyToBuffer(Buffer : PByteArray; BufferSize : Word);
var
  S   : string;
  Rec : TIntelHexRec;
  i   : word;
begin
  Reset(f);
  FillChar(Buffer^ , BufferSize, #0);
  While Not Eof(f) do begin
    ReadLn(f, S);
    Str2IntelHexRec(S, Rec);
    With Rec do
      Case RecType of
        DataRecord : if Offset+RecLen<BufferSize then
          System.Move(Data, Buffer^[Offset], RecLen);
        EofRecord  : ;
      else
        GotError('CopyToBuffer', 'Rec="'+S+
            '" RecLen='+IntToHex(RecLen, 2)+
            ' Offset='+IntToHex(Offset, 4)+
            ' RecType='+IntToHex(RecType, 2));
      end;
//    GotError(IntToHex(rec.Offset, 4), IntToHex(Buffer^[0], 2)+IntToHex(Buffer^[1], 2));
  end;
end;


destructor TIntelHEX.Destroy;
begin
  CloseFile(f);
  inherited Destroy;
end;

procedure TIntelHex.GotError(ProcName, Msg : string);
begin
  MessageBox(0, PChar(ProcName+':'+Msg), 'IntelHex', MB_OK+MB_ICONERROR);
end;

end.
