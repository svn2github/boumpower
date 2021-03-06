unit SDSOS;

interface

uses SysUtils, Classes, Windows, Math, CPort;

type
  TCmdProc = procedure(Params : TStrings) of object;
  TSmallDeviceOS = class(TObject)
  protected
    FCmdList : TStringList;
    FDeviceStream : TStream;
    FCurrentDIR : string;
  protected
    procedure RegisterCmd(Name, Description : string; Proc : TCmdProc);
    procedure osWriteStr(Str : PChar); virtual;
    procedure osWriteLn(Str : PChar);
  public
    constructor Create(DeviceStream : TStream);
    destructor  Destroy; override;
    procedure   Execute(Cmd : string);
    procedure osMountDevice;
    procedure osHELP(Params : TStrings);
    procedure osDIR(Params : TStrings);
    procedure osCD(Params : TStrings);
    procedure osUPLOAD(Params : TStrings);
    procedure osDOWNLOAD(Params : TStrings);
  end;

type
  TSmallDeviceTerminalOS = class(TSmallDeviceOS)
  private
    FComPort  : TCustomComPort;
    FIncomingLine : string;
  protected
    procedure TerminalRxChar(Sender: TObject; Count: Integer);
    procedure ProcessIncomingLine;
    procedure osWriteStr(Str : PChar); override;
    procedure osWritePrompt;
  public
    constructor Create(DeviceStream : TStream; AComPort : TCustomComPort);
    destructor  Destroy; override;
    procedure Run;
    procedure Stop;
  end;

var
  SmallDeviceDemoStream : TStream;

implementation

type
  TosFileRec = record                 //  Total 32 bytes for Small Device
    Next      : Cardinal;             //  2 Next record
    Child     : Cardinal;             //  2 Child recod
    Start     : Cardinal;             //  2 Start of file
    Size      : Integer;              //  2 File size (-1 if unknown)
    FileDate  : TDateTime;            //  4
    Name      : string;               // 12 8.3 format
    Attr      : word;                 //  2 File attrs
    Extra     : Array[0..5] of byte;  //  6 bytes
  end;

type
  TOsCmd = class(TObject)
  private
    FName        : string;
    FDescription : string;
    FProc        : TCmdProc;
  public
    constructor Create(Name, Description : String; Proc : TCmdProc);
    procedure Execute(Params : TStrings);
    property Name : string read FName;
    property Description : string read FDescription write FDescription;
  end;

Type
  TInquiryEvent = procedure(Sender : TObject; var Device, Params : string);
  TPicDemoStream = class(TStream)
  private
    FReadBuf   : PByteArray;
    FReadLen   : Integer;
    FRAM       : PByteArray;
    FFlash     : PWordArray;
    FEE        : PByteArray;
    FOnInquiry : TInquiryEvent;
  protected
    procedure Respond(Str : string);
    procedure osInquiry;                           // Inquiry command
    procedure osReadRAM(Addrs : word);             // Read Register
    procedure osWriteRAM(Addrs : word; B : Byte);  // Write Register
  public
    constructor Create;
    destructor  Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  public
    property OnInquiry : TInquiryEvent read FOnInquiry write FOnInquiry;
  end;

constructor TPicDemoStream.Create;
const
  Welcome = 'Microchip PIC demo'#13;
begin
  inherited Create;
  GetMem(FReadBuf, 512);
  GetMem(FRAM, 4*128);
  GetMem(FFlash, 8192*2);
  GetMem(FEE, 256);
  FReadLen := 0;
  FOnInquiry := nil;
  Respond(Welcome);
end;

destructor TPicDemoStream.Destroy;
begin
  FreeMem(FEE);
  FreeMem(FFlash);
  FreeMem(FRAM);
  FreeMem(FReadBuf);
  inherited Destroy;
end;

function TPicDemoStream.Write(const Buffer; Count: Longint): Longint;
var
  B : TByteArray absolute Buffer;
begin
  Case Char(B[0]) of
    '@' : osInquiry;
    'A' : osReadRAM(B[1]+256*B[2]);
    'B' : osWriteRAM(B[1]+256*B[2], B[3]);
  end;
  result := Count;
end;

function TPicDemoStream.Read(var Buffer; Count: Longint): Longint;
begin
  Count := Min(Count, FReadLen);
  Move(FReadBuf^, Buffer, Count);
  if Count<FReadLen then Move(FReadBuf^[Count], FReadBuf^, FReadLen-Count);
  Dec(FReadLen, Count);
  result := Count;
end;

procedure TPicDemoStream.Respond(Str : string);
begin
  Move(Str[1], FReadBuf^[FReadLen], Length(Str));
  Inc(FReadLen, Length(Str));
end;


procedure TPicDemoStream.osInquiry; // Inquiry
var
  Device, Params : string;
begin
  Device := 'PIC16F877';
  Params := '-B-LAB';
  If Assigned(FOnInquiry) then FOnInquiry(Self, Device, Params);
  Respond(Device+' '+Params+#13);
end;

procedure TPicDemoStream.osReadRAM(Addrs : word);
begin
  if Addrs<4*128 then
    Respond(Char(FRAM^[Addrs]))
  else
    Respond(#$FF);
end;

procedure TPicDemoStream.osWriteRAM(Addrs : word; B : Byte);
begin
  if Addrs>=4*128 then exit;
  FRAM^[Addrs] := B;
end;


{*****************************************************************************}

constructor TOsCmd.Create(Name, Description : String; Proc : TCmdProc);
begin
  inherited Create;
  FName := Name;
  FDescription := Description;
  FProc := Proc;
end;

procedure TOsCmd.Execute(Params : TStrings);
begin
  if Assigned(FProc) then FProc(Params);
end;

constructor TSmallDeviceOS.Create(DeviceStream : TStream);
begin
  inherited Create;
  FCmdList := TStringList.Create;
  FCmdList.Sorted := true;
  FCurrentDIR := '';
  FDeviceStream := DeviceStream;
  RegisterCmd('HELP',    'Type HELP cmd for more informations', osHELP);
  RegisterCmd('DIR',     'Directory listing', osDIR);
  RegisterCmd('LS',      'Directory listing', osDIR);
  RegisterCmd('CD',      'Change directory', osCD);
  RegisterCmd('UPLOAD',  'Upload a file', osUPLOAD);
  RegisterCmd('DOWNLOAD','Download a file', osDOWNLOAD);
end;

destructor  TSmallDeviceOS.Destroy;
begin
  FCmdList.Free;
end;

procedure TSmallDeviceOS.RegisterCmd(Name, Description : string; Proc : TCmdProc);
var
  osCmd : TosCmd;
begin
  osCmd := TosCmd.Create(Name, Description, Proc);
  FCmdList.AddObject(Name, osCmd);
end;

procedure TSmallDeviceOS.osWriteStr(Str : PChar);
begin
  MessageBox(0, Str, 'Small Device Scalable OS', MB_OK);
end;

procedure TSmallDeviceOS.osWriteLn(Str : PChar);
begin
  osWriteStr(Str);
  osWriteStr(#13);
end;

procedure TSmallDeviceOS.osMountDevice;
var
  Buffer  : PChar;
  Len     : Integer;
  S       : TStringList;
const
  cmdInquiry : char = '@';
begin
  Buffer := StrAlloc(1024);
  S := TStringList.Create;
  Len := FDeviceStream.Read(Buffer^, 1023);
  Buffer[Len] := #0;
  osWriteStr(Buffer);
  FDeviceStream.Write(cmdInquiry, 1);
  Len := FDeviceStream.Read(Buffer^, 1023);
  Buffer[Len] := #0;

  ExtractStrings([' ', '-', '/'], [' '], Buffer, S);
  FCurrentDIR := S[0]+'/';
  osWriteStr(Buffer);
  S.Free;
  StrDispose(Buffer);
end;


procedure TSmallDeviceOS.Execute(Cmd : string);
var
  i : integer;
  S : TStringList;
begin
  S := TStringList.Create;
  if ExtractStrings([' ', '-', '/'], [' '], PChar(Cmd), S) = 0 then exit;
  if FCmdList.Find(S[0], i) then
    TosCmd(FCmdList.Objects[i]).Execute(S);
  osWriteLn('');
  S.Free;
end;


procedure TSmallDeviceOS.osHELP(Params : TStrings);
var
  i : integer;
begin
  i := 0;
  while i<FCmdList.Count do begin
    with TosCmd(FCmdList.Objects[i]) do begin
      osWriteStr(PChar(Name));
      osWriteStr(PChar(StringOfChar(' ', 10-Length(Name))));
      osWriteLn(PChar(Description));
    end;
    Inc(i);
  end;
end;

procedure TSmallDeviceOS.osDIR(Params : TStrings);
const
  fmtDateTime = 'dd.mm.yyyy  hh:nn';
  fmtDir  = '%-12.12s  <DIR>';
  fmtFile = '%-12.12s        %s';
  fmtDev  = '%-12.12s  <DEV>';
begin
  osWriteLn(PChar('Directory of '+FCurrentDIR));
  osWriteLn(PChar(Format(fmtDev, ['I2C'])));
  osWriteLn(PChar(Format(fmtDir, ['EEPROM'])));
  osWriteLn(PChar(Format(fmtFile, ['OS.hex', FormatDateTime(fmtDateTime, Now())])));
  osWriteLn(PChar(Format(fmtFile, ['Main.hex', FormatDateTime(fmtDateTime, Now())])));
  osWriteLn(PChar(Format(fmtFile, ['Readme.htm', FormatDateTime(fmtDateTime, Now())])));
end;

procedure TSmallDeviceOS.osCD(Params : TStrings);
begin
  if Params.Count<>2 then exit;
  if Params[1]='.' then exit;
  if Params[1]='..' then exit;
  FCurrentDIR := FCurrentDIR+Params[1]+'/';
end;

procedure TSmallDeviceOS.osUPLOAD(Params : TStrings);
begin
  osWriteLn('-> Upload file with ZModem protocol');
end;

procedure TSmallDeviceOS.osDOWNLOAD(Params : TStrings);
begin
  osWriteLn('-> Download file with ZModem protocol');
end;

{**************************************************************}

constructor TSmallDeviceTerminalOS.Create(DeviceStream : TStream; AComPort : TCustomComPort);
begin
  inherited Create(DeviceStream);
  FIncomingLine := '';
  FComPort := AComPort;
  FComPort.OnRxChar := TerminalRxChar;
end;

destructor TSmallDeviceTerminalOS.Destroy;
begin
  FComPort.OnRxChar := nil;
  inherited Destroy;
end;

procedure TSmallDeviceTerminalOS.TerminalRxChar(Sender: TObject; Count: Integer);
var
  C : char;
begin
  while Count>0 do begin
    FComPort.Read(C, 1);
    case C of
      #8  : if Length(FIncomingLine)>0 then begin // Backspace
              Delete(FIncomingLine, Length(FIncomingLine), 1);
              FComPort.WriteStr(#8' '#8);
            end else
              FComPort.WriteStr(#7); // Bell
      #13 :  begin
              FComPort.WriteStr(#13); // CR
              ProcessIncomingLine;
             end;
    else begin
             FComPort.Write(C, 1); // Echo
             FIncomingLine := FIncomingLine+C;
      end;
    end;
    Dec(Count);
  end;
end;

procedure TSmallDeviceTerminalOS.ProcessIncomingLine;
begin

  Execute(FIncomingLine);
  FIncomingLine := '';
  osWritePrompt;
end;

procedure TSmallDeviceTerminalOS.osWritePrompt;
begin
  osWriteStr(PChar(FCurrentDIR+' $ '));
end;


procedure TSmallDeviceTerminalOS.osWriteStr(Str : PChar);
begin
  FComPort.WriteStr(Str);
end;

procedure TSmallDeviceTerminalOS.Run;
begin
  FComPort.Open;
  osWriteLn('Small Device Scalable OS - Terminal Server');
  osMountDevice;
  osWriteLn('Type ''help'' to know available commands');
  osWritePrompt;
end;

procedure TSmallDeviceTerminalOS.Stop;
begin
  osWriteLn(#13'Terminal Server : shut down');
  FComPort.Close;
end;

initialization

  SmallDeviceDemoStream := TPicDemoStream.Create;

end.
