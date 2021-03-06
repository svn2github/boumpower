unit PICkit2;

interface

uses
  Windows, {Debug}
  SysUtils, Classes, Contnrs, Math, JvHidControllerClass, JvInterpreterParser;

type
  TFirmwareCommands = (
    ID_ENTER_BOOTLOADER = $42,
    ID_NO_OPERATION = $5A,
    ID_FIRMWARE_VERSION = $76,
    ID_SETVDD = $A0,
    ID_SETVPP = $A1,
    ID_READ_STATUS = $A2,
    ID_READ_VOLTAGES = $A3,
    ID_DOWNLOAD_SCRIPT = $A4,
    ID_RUN_SCRIPT = $A5,
    ID_EXECUTE_SCRIPT = $A6,
    ID_CLR_DOWNLOAD_BUFFER = $A7,
    ID_DOWNLOAD_DATA = $A8,
    ID_CLR_UPLOAD_BUFFER = $A9,
    ID_UPLOAD_DATA = $AA,
    ID_CLR_SCRIPT_BUFFER = $AD,
    ID_UPLOAD_DATA_NOLEN = $AC,
    ID_END_OF_BUFFER = $AD,
    ID_RESET = $AE,
    ID_SCRIPT_BUFFER_CHKSM =$AF,
    ID_SET_VOLTAGE_CALS = $B0,
    ID_WR_INTERNAL_EE = $B1,
    ID_RD_INTERNAL_EE = $B2,
    ID_ENTER_UART_MODE = $B3,
    ID_EXIT_UART_MODE = $B4,
    ID_ENTER_LEARN_MODE = $B5,
    ID_EXIT_LEARN_MODE =$B6,
    ID_ENABLE_PK2GO_MODE = $B7,
    ID_LOGIC_ANALYSER_GO = $B8,
    ID_COPY_RAM_UPLOAD = $B9);

var
  SFirmwareCommands : array[TFirmwareCommands] of PChar;


const
  MaxScriptSize = 64;
  DelayedResponse = true;

type
{ Help creating PICkit2 scripts
  Script[0] = Script Length N
  Script[1] = Script byte 1
  Script[N] = Last script byte
}
  TPICkit2Script = class(TCollectionItem)
    constructor Create(Collection: TCollection); override;
    destructor  Destroy; override;
  private
    FScriptName : string;
    FScriptData : PByteArray;
    function GetScriptLength : byte;
  public
    property ScriptName : string read FScriptName write FScriptName;
    property ScriptData : PByteArray read FScriptData;
    property ScriptLength : byte read GetScriptLength;
  end;

  TPICkit2ScriptEvent  = procedure(Sender : TObject; const Msg : String) of object;
  TScriptFunction = class(TObject)
    FuncName  : string;
    FuncKind  : TTokenKind;
    FuncID    : integer;
    ArgsCount : integer;
//    ArgNames  : TStringList;
  end;

  TPICkit2Scripts = class(TCollection)
  private
    FOwner: TPersistent;
    FParser : TJvInterpreterParser;
    FGlobalFunctions : TStringList;
    FGlobalConstants : TStringList;
    FLabels          : TStringList;
    FConstants       : TStringList;
    FDoLoopStack     : TStack;
    FDoLoopNesting   : Integer;
    FOnDebugMsg : TPICkit2ScriptEvent;
    FOnWarningMsg : TPICkit2ScriptEvent;
    FOnErrorMsg : TPICkit2ScriptEvent;
    function  GetScript(Index: Integer): TPICkit2Script;
    procedure SetScript(Index: Integer; Value: TPICkit2Script);
    function  GetScriptFunction(Name : string) : TScriptFunction;
  protected
    function GetOwner: TPersistent; override;
    procedure ScriptDebugMsg(const Msg : string);
    procedure ScriptWarningMsg(const Msg : string);
    procedure ScriptErrorMsg(const Msg : string);
    procedure AddLabel(Name : string; PC : integer);
    function  LabelOffset(Name : string; PC : integer; var Offset : integer) : boolean;
    procedure AddConstant(Name : string; Value : integer);
    function  GetConstant(Name : string) : integer;
    function  TokenKind(const Token : string) : TTokenKind; virtual;
  public
    constructor Create(Owner: TPersistent); overload;
    destructor  Destroy; override;
    function    Add : TPICkit2Script; overload;
    procedure   AddScriptFunction(ID : integer; Name : string; ArgList : array of string; Kind : TTokenKind = ttFunction);
    function    CompileScripts(ScriptSource : PChar) : boolean;
    procedure   LoadScriptsFromStream(Stream : TStream);
    procedure   LoadScriptsFromFile(FileName : string);
  public
    property Scripts[Index : Integer] : TPICkit2Script read GetScript write SetScript;
  published
    property DoLoopNesting : integer read FDoLoopNesting;
    property OnDebugMsg : TPICkit2ScriptEvent read FOnDebugMsg write FOnDebugMsg;
    property OnWarningMsg : TPICkit2ScriptEvent read FOnWarningMsg write FOnWarningMsg;
    property OnErrorMsg : TPICkit2ScriptEvent read FOnErrorMsg write FOnErrorMsg;
  end;

type
  TPICkit2 = class;
  TResponseProc = procedure of object; // used to queue the Delayed Responses
  TPICkit2FirmwareCommandEvent = procedure(Sender : TPICkit2; const ID : TFirmwareCommands; Const CommandText : string) of object;
  TPICkit2FirmwareVersionEvent = procedure(Sender : TPICkit2; const Major, Minor, dot : byte) of object;
  TPICkit2StatusEvent = procedure(Sender : TPICkit2; const Status : word) of object;
  TPICkit2VoltagesEvent = procedure(Sender : TPICkit2; const VDD, VPP : real) of object;
  TPICkit2UploadDataEvent = procedure(Sender : TPICkit2; const Data : PByteArray; const Size : integer) of object;
  TPICkit2ScriptBufferChksmEvent = procedure(Sender : TPICkit2; const LengthSum, BufferSum : word) of object;
  TPICkit2ReadInternalEEEvent = procedure(Sender : TPICkit2; const Data : PByteArray; const Size : integer) of object;
  TPICkit2LogicAnalyserEvent = procedure(Sender : TPICkit2; const TrigLoc : word) of object;
  TPICkit2 = class(TComponent)
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  private
    FHidDevice   : TJvHidDevice;
    FScripts     : TPICkit2Scripts;
    FResponseQueue : TQueue; // Queue the Delayed Responses
    FFirmware  : string;
    FStatus    : word;
    FVDD, FVPP : real;
    FScriptLengthSum : word;
    FScriptBufferSum : word;
    FTrigLoc : word;
    CommandBuffer      : PByteArray;
    ResponseBuffer     : PByteArray;
    CommandBufferSize  : integer;
    ResponseBufferSize : integer;
    CommandBufferIndex : integer;
    FOnAddFirmwareCommand     : TPICkit2FirmwareCommandEvent;
    FOnAfterReadFirmware      : TPICkit2FirmwareVersionEvent;
    FOnAfterReadStatus        : TPICkit2StatusEvent;
    FOnAfterReadVoltages      : TPICkit2VoltagesEvent;
    FOnAfterUploadData        : TPICkit2UploadDataEvent;
    FOnAfterScriptBufferChksm : TPICkit2ScriptBufferChksmEvent;
    FOnAfterReadInternalEE    : TPICkit2ReadInternalEEEvent;
    FOnAfterLogicAnalyser     : TPICkit2LogicAnalyserEvent;
    function  GetConnected : boolean;
    procedure SetHidDevice(Device : TJvHidDevice);
  protected
    procedure ErrorMsg(const Msg : string);
    function  NeedFirmwareCommandsFlush : boolean;
    procedure GetCommandBufferMem(var P : PByteArray; const bytes : integer);
    procedure AddFirmwareCommand(ID : TFirmwareCommands); overload;
    procedure AddFirmwareCommand(ID : TFirmwareCommands; const Data; const DataSize : integer); overload;
    procedure AddFirmwareCommand(ID : TFirmwareCommands; const Data1 : byte; const Data; const DataSize : integer); overload;
    procedure ResponseReadFirmware;
    procedure ResponseReadStatus;
    procedure ResponseReadVoltages;
    procedure ResponseUploadData;
    procedure ResponseUploadDataNoLen;
    procedure ResponseScriptBufferChksm;
    procedure ResponseReadInternalEE;
    procedure ResponseLogicAnalyser;
  public
    procedure FlushFirmwareCommands;
    function  ReadResponses : boolean;
    procedure QueueResponseProc(ResponseProc : TResponseProc);
    property  Connected : boolean read GetConnected;
    procedure Nop;
    procedure SetVDD(VDD : real); overload;
    procedure SetVDD(VDD, Vfault : real); overload;
    procedure SetVPP(VPP : real); overload;
    procedure SetVPP(VPP, Vfault : real); overload;
    function  ReadFirmware(const DelayedResponse : boolean = false) : string;
    function  ReadStatus(const DelayedResponse : boolean = false) : word;
    procedure ReadVoltages;
    procedure DownloadScript(const Num : byte; const ScriptData : PByteArray); overload;
    procedure Run_Script(const Num, Times : byte);
    procedure ExecuteScript(ScriptData : PByteArray);
    procedure ClrDownloadBuffer;
    procedure DownloadData(const Data : PByteArray; const DataSize : Integer);
    procedure ClrUploadBuffer;
    procedure UploadData(const Dest : PByteArray; const DataSize : Integer; var BytesRead : Integer; const DelayedResponse : boolean = false);
    procedure EndOfBuffer;
    procedure Reset;
    procedure ScriptBufferChksm;
    procedure SetVoltageCals(const CalFactor : word; const VddOffset, VddCalFactor : byte);
    procedure WriteInternalEE(const Addrs : byte; const Data : PByteArray; const DataSize : integer);
    procedure ReadInternalEE(const Addrs : byte; const Dest : PByteArray; const BytesToRead : integer);
    procedure EnterUART(const Baud : word);
    procedure ExitUART;
    procedure EnterLEARN;
    procedure ExitLEARN;
    procedure EnablePK2GO(const EEPROM : byte);
    procedure LogicAnalyser(const Edge, TrigMask, TrigStates, EdgeMask, TrigCount : byte;
                            const PostTrigCount : Word;
                            const SampleRateFactor : byte);
    procedure CopyRamUpload(const Addrs : word);
  published
    property HidDevice : TJvHidDevice read FHidDevice write SetHidDevice;
    property Scripts : TPICkit2Scripts read FScripts;
  published
    property FirmwareVersion : string read FFirmware;
    property Status : word read FStatus;
//    property OnAfterFlush : TNotifyEvent read
    property OnAddFirmwareCommand : TPICkit2FirmwareCommandEvent read FOnAddFirmwareCommand write FOnAddFirmwareCommand;
    property OnAfterReadFirmwareVersion : TPICkit2FirmwareVersionEvent read FOnAfterReadFirmware write FOnAfterReadFirmware;
    property OnAfterReadStatus : TPICkit2StatusEvent read FOnAfterReadStatus write FOnAfterReadStatus;
    property OnAfterReadVoltages  : TPICkit2VoltagesEvent read FOnAfterReadVoltages write FOnAfterReadVoltages;
    property OnAfterUploadData : TPICkit2UploadDataEvent read FOnAfterUploadData write FOnAfterUploadData;
    property OnAfterScriptBufferChksm : TPICkit2ScriptBufferChksmEvent read FOnAfterScriptBufferChksm write FOnAfterScriptBufferChksm;
    property OnAfterReadInternalEE : TPICkit2ReadInternalEEEvent read FOnAfterReadInternalEE write FOnAfterReadInternalEE;
    property OnAfterLogicAnalyser : TPICkit2LogicAnalyserEvent read FOnAfterLogicAnalyser write FOnAfterLogicAnalyser;
  end;

procedure Register;

implementation

resourcestring
  pkErrorNotConnected = 'PICkit2 non connect�';
  pkErrorDoLoopNesting = 'Trop de ''Do..Loop'' imbriqu�es';
  pkErrorNoDoInLoop = '%s sans ''Do''';
  pkErrorArgsNumber = '%s nombre ill�gal d''arguments %d au lieu de %d';
  pkErrorNullStr = 'Cha�ne vide';
  pkErrorIllegalIdentifier = 'Identifier invalide ''%s''';
  pkErrorLabelAlreadyExists = 'L''�tiquette ''%s'' existe d�j�';
  pkErrorScriptSize = 'Taille %d bytes du script trop grande (max %d bytes)';
  pkErrorUnknowFirmwareCommand = 'Commande Firmware inconnue';
  pkWarningStrTruncated = 'Cha�ne tronqu�e %s en ''%s''';
  pkWarningDelayedResponse = 'R�ponse diff�r�e';

const // Adds to JvInterpreterParser
  ttScript      = 200;
  ttLabel       = ttScript+1;
  ttConstant    = ttScript+2;
  ttLoop        = ttScript+3;
  ttLoopBuffer  = ttScript+4;
  ttGoto        = ttScript+5;
  ttGotoEQ      = ttScript+6;
  ttGotoGT      = ttScript+7;

type
  TQueuedResponseProc = class(TObject)
    FProc : TResponseProc;
    constructor Create(Proc : TResponseProc);
    procedure Execute;
  end;

function FirmwareCommandText(const ID : TFirmwareCommands) : string; overload; forward;
function FirmwareCommandText(const ID : TFirmwareCommands; ParamsText : string) : string; overload; forward;


constructor TQueuedResponseProc.Create(Proc : TResponseProc);
begin
  inherited Create;
  FProc := Proc;
end;

procedure TQueuedResponseProc.Execute;
begin
  if Assigned(FProc) then FProc;
end;

constructor TPICkit2Script.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  GetMem(FScriptData, MaxScriptSize);
  FScriptData^[0] := 0; // Set ScriptLength to 0
end;

destructor  TPICkit2Script.Destroy;
begin
  FreeMem(FScriptData);
  FScriptData := nil; // Parano
  inherited Destroy;
end;

function TPICkit2Script.GetScriptLength : byte;
begin
  result := FScriptData^[0];
end;

{**********************************************************}

constructor TPICkit2Scripts.Create(Owner: TPersistent);
begin
  inherited Create(TPICkit2Script);
  FOwner := Owner;
  FOnDebugMsg := nil;
  FOnWarningMsg := nil;
  FOnErrorMsg := nil;
  FParser := TJvInterpreterParser.Create;
  FGlobalFunctions := TStringList.Create;
  FGlobalFunctions.Sorted := true;
  FGlobalConstants := TStringList.Create;
  FGlobalConstants.Sorted := true;
  FLabels := TStringList.Create;
  FLabels.Sorted := true;
  FConstants := TStringList.Create;
  FConstants.Sorted := true;
  FDoLoopStack := TStack.Create;
  FDoLoopNesting := 2;
  AddScriptFunction( -1, 'Script', ['ScriptName'], ttScript);
  AddScriptFunction($FF, 'VddON', []);
  AddScriptFunction($FE, 'VddOFF', []);
  AddScriptFunction($FD, 'VddGndON', []);
  AddScriptFunction($FC, 'VddGndOFF', []);
  AddScriptFunction($FB, 'VppON', []);
  AddScriptFunction($FA, 'VppOFF', []);
  AddScriptFunction($F9, 'VppPwmON', []);
  AddScriptFunction($F8, 'VppPwmOFF', []);
  AddScriptFunction($F7, 'MClrGndON', []);
  AddScriptFunction($F6, 'MClrGndOFF', []);
  AddScriptFunction($F5, 'BusyLedOn', []);
  AddScriptFunction($F4, 'BusyLedOff', []);
  AddScriptFunction($F3, 'ICSP_SetPins', ['PinStates']);
  AddScriptFunction($F2, 'WriteByteLiteral', ['DataByte']);
  AddScriptFunction($F1, 'WriteByteBuffer', []);
  AddScriptFunction($F0, 'ReadByteBuffer', []);
  AddScriptFunction($EF, 'ReadByte', []);
  AddScriptFunction($EE, 'WriteBitsLiteral', ['NBits', 'DataByte']);
  AddScriptFunction($ED, 'WriteBitsBuffer', ['NBits']);
  AddScriptFunction($EC, 'ReadBitsBuffer', ['NBits']);
  AddScriptFunction($EB, 'ReadBits', ['NBits']);
  AddScriptFunction($EA, 'ICSP_SetSpeed', ['Rate']);
  AddScriptFunction($E9, 'Loop', ['_LoopOffset', 'Iterations'], ttLoop);
  AddScriptFunction($E8, 'DelayLong', ['Units']);
  AddScriptFunction($E7, 'DelayShort', ['Units']);
  AddScriptFunction($E6, 'If_EQ_Goto', ['ByteForComparison', '_Offset'], ttGotoEQ);
  AddScriptFunction($E5, 'If_GT_Goto', ['ByteForComparison', '_Offset'], ttGotoGT);
  AddScriptFunction($E4, 'Goto', ['_Offset'], ttGoto);
  AddScriptFunction($E3, 'ExitScript', []);
  AddScriptFunction($E2, 'PeekSFR', ['Addrs']);
  AddScriptFunction($E1, 'PokeSFR', ['Addrs', 'DataByte']);
  AddScriptFunction($E0, 'ICD_SlaveRX', []);
  AddScriptFunction($DF, 'ICD_SlaveLiteralTX', ['DataByte']);
  AddScriptFunction($DE, 'ICD_SlaveBufferTX', []);
  AddScriptFunction($DD, 'LoopBuffer', ['_Offset'], ttLoop);
  AddScriptFunction($DC, 'ICSP_StatesBuffer', []);
  AddScriptFunction($DB, 'PopDownload', []);
  AddScriptFunction($DA, 'CoreInst18', ['LSB', 'MSB']);
  AddScriptFunction($D9, 'CoreInst24', ['LSB', 'MID', 'MSB']);
  AddScriptFunction($D8, 'NOP24', []);
  AddScriptFunction($D7, 'VISI24', []);
  AddScriptFunction($D6, 'RD2_ByteBuffer', []);
  AddScriptFunction($D5, 'RD2_BitsBuffer', ['NBits']);
  AddScriptFunction($D4, 'WriteBufWordW', ['PIC24W']);
  AddScriptFunction($D3, 'WriteBufByteW', ['PIC24W']);
  AddScriptFunction($D2, 'ConstWriteDL', ['DataByte']);
  AddScriptFunction($D1, 'WriteBitsLitHld', ['NBits', 'Literal']);
  AddScriptFunction($D0, 'WriteBitsBufHld', ['NBits']);
  AddScriptFunction($CF, 'SetAux', ['PinState']);
  AddScriptFunction($CE, 'AuxStateBuffer', []);
  AddScriptFunction($CD, 'I2C_START', []);
  AddScriptFunction($CC, 'I2C_STOP', []);
  AddScriptFunction($CB, 'I2C_WriteByteLit', ['DataByte']);
  AddScriptFunction($CA, 'I2C_WriteByteBuf', []);
  AddScriptFunction($C9, 'I2C_ReadByteACK', []);
  AddScriptFunction($C8, 'I2C_ReadByteNACK', []);
  AddScriptFunction($C7, 'SPI_WriteByteLit', ['DataByte']);
  AddScriptFunction($C6, 'SPI_WriteByteBuf', []);
  AddScriptFunction($C5, 'SPI_ReadByteBuf', []);
  AddScriptFunction($C4, 'SPI_ReadWriteByteLit', ['DataByteToSend']);
  AddScriptFunction($C3, 'SPI_ReadWriteByteBuf', []);
  AddScriptFunction($C2, 'ICD_SlaveRxBL', []);
  AddScriptFunction($C1, 'ICD_SlaveTxLitBL', ['DataByte']);
  AddScriptFunction($C0, 'ICD_SlaveTxBufBL', []);
  AddScriptFunction($BF, 'MeasurePulse', []);
  AddScriptFunction($BE, 'UNIO_Tx', ['DeviceAddrs', 'NBytes']);
  AddScriptFunction($BD, 'UNIO_TxRx', ['DeviceAddrs', 'NBytesToSend', 'NBytesToReceive']);
  AddScriptFunction($BC, 'JT2_SetMode', ['NBits', 'TMS']);
  AddScriptFunction($BB, 'JT2_SendCMD', ['Cmd']);
  AddScriptFunction($BA, 'JT2_XferData8Lit', ['DataByte']);
  AddScriptFunction($B9, 'JT2_XferData32Lit', ['LSB', 'Byte2', 'Byte3', 'MSB']);
  AddScriptFunction($B8, 'JT2_XferFastDatLit', ['LSB', 'Byte2', 'Byte3', 'MSB']);
  AddScriptFunction($B7, 'JT2_XferFastDatBuf', []);
  AddScriptFunction($B6, 'JT2_XferInstBuf', []);
  AddScriptFunction($B5, 'JT2_GetPeResp', []);
  AddScriptFunction($B4, 'JT2_WaitPeResp', []);
  AddScriptFunction($B3, 'JT2_PeProgResp', []);
end;

destructor TPICkit2Scripts.Destroy;
begin
  FreeAndNil(FDoLoopStack);
  FreeAndNil(FConstants);
  FreeAndNil(FLabels);
  while FGlobalFunctions.Count> 0 do FGlobalFunctions.Delete(0);
  FreeAndNil(FGlobalConstants);
  FreeAndNil(FGlobalFunctions);
  FParser.Free;
  inherited Destroy;
end;

function TPICkit2Scripts.GetScript(Index: Integer): TPICkit2Script;
begin
  Result := TPICkit2Script(Items[Index]);
end;

procedure TPICkit2Scripts.SetScript(Index: Integer; Value: TPICkit2Script);
begin
  SetItem(Index, TCollectionItem(Value));
end;

function TPICkit2Scripts.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TPICkit2Scripts.Add : TPICkit2Script;
begin
  result := TPICkit2Script(inherited Add);
end;


procedure TPICkit2Scripts.AddScriptFunction(ID : integer; Name : string; ArgList : array of string; Kind : TTokenKind = ttFunction);
var
  ScriptFunction : TScriptFunction;
begin
  ScriptFunction := TScriptFunction.Create;
  ScriptFunction.FuncName := Name;
  ScriptFunction.FuncKind := Kind;
  ScriptFunction.FuncID := ID;
  ScriptFunction.ArgsCount := Length(ArgList);
  FGlobalFunctions.AddObject(Name, ScriptFunction);
end;

function TPICkit2Scripts.GetScriptFunction(Name : string) : TScriptFunction;
begin
  result := TScriptFunction(FGlobalFunctions.Objects[FGlobalFunctions.IndexOf(Name)]);
end;

procedure TPICkit2Scripts.AddLabel(Name : string; PC : integer);
begin
  ScriptDebugMsg('AddLabel ''' + Name + '''');
  FLabels.AddObject(Name, TObject(PC));
end;

function TPICkit2Scripts.LabelOffset(Name : string; PC : integer; var Offset : integer) : boolean;
var
  i : integer;
begin
  result := false;
  if not FLabels.Find(Name, i) then exit;
  Offset := Integer(FLabels.Objects[i])-PC;
  result := true;
end;

procedure TPICkit2Scripts.AddConstant(Name : string; Value : integer);
begin
  ScriptDebugMsg('AddConstant ''' + Name + ''' = '+IntToStr(Value));
  FConstants.AddObject(Name, TObject(Value));
end;

function TPICkit2Scripts.GetConstant(Name : string) : integer;
var
  i : integer;
begin
  if FConstants.Find(Name, i) then begin
    result := Integer(FConstants.Objects[i]);
    exit;
  end;
  if FGlobalConstants.Find(Name, i) then begin
    result := Integer(FGlobalConstants.Objects[i]);
    exit;
  end;
  {Error}
  result := $FFFFFF;
end;

function TPICkit2Scripts.TokenKind(const Token : string) : TTokenKind;
// Use TokenTyp, but overwrite result for this purpose...
var
  i : Integer;
begin
  result := TokenTyp(Token);
//  MessageBox(0, PChar('TokenKind '+IntToStr(result)+' '+Token), 'Debug', MB_OK	);
  if result <> ttIdentifier then exit;

  if FGlobalFunctions.Find(Token, i) then begin
    result := TScriptFunction(FGlobalFunctions.Objects[i]).FuncKind;
    exit;
  end;
  if FLabels.Find(Token, i) then begin
    result := ttLabel;
    exit;
  end;
  if FConstants.Find(Token, i) then begin
    result := ttConstant;
    exit;
  end;
  if FGlobalConstants.Find(Token, i) then begin
    result := ttConstant;
    exit;
  end;
end;

procedure TPICkit2Scripts.LoadScriptsFromStream(Stream : TStream);
var
  SourceText : PChar;
//  Source : T
begin
  SourceText := StrAlloc(Stream.Size+1);
  Stream.ReadBuffer(SourceText^, Stream.Size);
  SourceText[Stream.Size] := #0;
  CompileScripts(SourceText);
  StrDispose(SourceText);
end;

procedure TPICkit2Scripts.LoadScriptsFromFile(FileName : string);
var
  S : TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead);
  LoadScriptsFromStream(S);
  FreeAndNil(S);
end;

function TPICkit2Scripts.CompileScripts(ScriptSource : PChar) : boolean;
var
  Token, Identifier : string;
  Kind      : TTokenKind;
  State     : TTokenKind; // ttEmpty, ttScript, ttConst, ttBegin, ttEnd
  PC        : Integer;
  Value     : Integer;
  ArgsList  : TQueue;

  procedure NextToken;
  begin
    Token := FParser.Token;
    Kind := TokenKind(Token);
  end;

  function NextTokenAsIntegerValue(var Value : Integer) : boolean;
  begin
    result := true;
    NextToken;
    Case Kind of
      ttInteger  : Value := StrToInt(Token);
      ttString   : begin
          Value := Byte(Token[2]);
          Case Length(Token) of
            2  : ScriptErrorMsg(pkErrorNullStr);
            3  : {OK};
            else ScriptWarningMsg(Format(pkWarningStrTruncated, [Token, Token[2]]));
          end;
        end;
      ttConstant : Value := GetConstant(Token);
    else
      result := false;
    end; // Case
  end;

  function GetFunctionArgsValues : boolean;
  begin
    result := false;
    NextToken;
    if Kind<>ttLB then begin
      MessageBox(0, PChar(Token+' '+IntToStr(Kind)+' : "(" expected'), 'Script Error', MB_OK	);
      Exit;
    end;
    while Kind<>ttRB do begin
      if not NextTokenAsIntegerValue(Value) then begin
        MessageBox(0, PChar(Token+' : byte value or constant expected for function arg'), 'Script Error', MB_OK	);
        Exit;
      end;
      ArgsList.Push(Pointer(Value));
      NextToken;
      if (Kind<>ttRB) and (Kind<>ttCol) then begin
        MessageBox(0, PChar(Token+' : "," or ")" expected'), 'Script Error', MB_OK	);
        Exit;
      end;
    end;
    result := true;
  end;

  function Pass(Level : integer; Script : TPICkit2Script) : boolean;
  // Level 1 : Build Label and Constant Table, no code
  // Level 2 : Generate code
  label SkipConstDefinition;
  var
    i : integer;
  begin
    ScriptDebugMsg(Format('*** Compile Pass %d', [Level]));
    result := false;
    Identifier := '';
    PC := 1;
    State := ttEmpty;
    if Level = 1 then begin
      FLabels.Clear;
      FConstants.Clear;
    end;
    NextToken;
    while Kind<>ttEmpty do begin
      if (State<>ttBegin) and (Level=2) and (Kind<>ttBegin) then goto SkipConstDefinition;
      Case Kind of
        ttScript : begin
            State := ttScript;
            NextToken;
            if Level=2 then Script.FScriptName := Token;
          end;
        ttConst  : State := ttConst;
        ttBegin  : State := ttBegin;
        ttEnd    : State := ttEnd;
        ttDo     : With FDoLoopStack do begin
            Push(Pointer(PC));
            If Count>DoLoopNesting then begin
              ScriptErrorMsg(pkErrorDoLoopNesting);
              exit;
            end;
          end;
        ttIdentifier : begin
            Identifier := Token; // Constant or Label
            NextToken;  // Must be '=' or ':'
            if (State=ttConst) and (Kind = ttEqu) then begin
              if NextTokenAsIntegerValue(Value) then
                AddConstant(Identifier, Value) else exit;
            end else if (State=ttBegin) and (Kind = ttColon) then begin
              AddLabel(Identifier, PC);
            end else begin
              ScriptErrorMsg(Format(pkErrorIllegalIdentifier, [Identifier]));
            end;
          end;
          ttLabel : if Level=1 then begin
              ScriptErrorMsg(Format(pkErrorLabelAlreadyExists, [Token]));
              exit;
            end else NextToken; // Read ':'
          ttLoop : With GetScriptFunction(Token) do begin
              ScriptDebugMsg(Token);
              if FDoLoopStack.Count = 0 then begin
                ScriptErrorMsg(Format(pkErrorNoDoInLoop, [Token]));
                exit;
              end;
              if Level=2 then Script.ScriptData^[PC] := FuncID;
              // Compute relative offset and push it as args
              ArgsList.Push(Pointer(PC-Integer(FDoLoopStack.Pop)));
              Inc(PC);
              if ArgsCount>1 then begin
                if not GetFunctionArgsValues then exit; // error
                if ArgsList.Count<>ArgsCount then begin
                  ScriptErrorMsg(Format(pkErrorArgsNumber, [FuncName, ArgsList.Count, ArgsCount-1]));
                  exit;
                end;
                for i := 1 to ArgsCount do begin
                  If Level=2 then
                    Script.ScriptData^[PC] := Integer(ArgsList.Pop)
                  else
                    ArgsList.Pop;
                  Inc(PC);
                end;
              end; // Args processing
            end;
          ttFunction : With GetScriptFunction(Token) do begin
              ScriptDebugMsg(Token);
              if Level=2 then Script.ScriptData^[PC] := FuncID;
              Inc(PC);
              if ArgsCount>0 then begin
                if not GetFunctionArgsValues then exit; // error
                if ArgsList.Count<>ArgsCount then begin
                  ScriptErrorMsg(Format(pkErrorArgsNumber, [FuncName, ArgsList.Count, ArgsCount]));
                  exit;
                end;
                for i := 1 to ArgsCount do begin
                  If Level=2 then
                    Script.ScriptData^[PC] := Integer(ArgsList.Pop)
                  else
                    ArgsList.Pop;
                  Inc(PC);
                end;
              end; // Args processing
            end;
        else begin
          MessageBox(0, PChar(IntToStr(Kind) + ':'+Token), 'Script Error', MB_OK	);
          exit;
        end;
      end; // Case
SkipConstDefinition : NextToken;
    end; // while
    if PC>MaxScriptSize then begin
      ScriptErrorMsg(Format(pkErrorScriptSize, [PC-1, MaxScriptSize]));
      exit;
    end;
    If Level=2 then Script.ScriptData^[0] := PC-1;
    ScriptDebugMsg(Format('Script size = %d', [PC]));
    result := true;
  end;

begin
  result := false;
  FParser.Source := ScriptSource;
  ArgsList := TQueue.Create;
  if Pass(1, nil) then begin
    FParser.Init;
    Pass(2, Add);
    result := true;
  end;
  ArgsList.Free;
end;



procedure TPICkit2Scripts.ScriptDebugMsg(const Msg : string);
resourcestring
  Prefix = '[Debug]';
begin
  if Assigned(FOnDebugMsg) then FOnDebugMsg(Self, Prefix+Msg);
end;

procedure TPICkit2Scripts.ScriptWarningMsg(const Msg : string);
resourcestring
  Prefix = '[Avertissement]';
begin
  if Assigned(FOnWarningMsg) then FOnWarningMsg(Self, Prefix+Msg);
end;

procedure TPICkit2Scripts.ScriptErrorMsg(const Msg : string);
resourcestring
  Prefix = '[Erreur]';
begin
  if Assigned(FOnErrorMsg) then FOnErrorMsg(Self, Prefix+Msg);
end;


{**********************************************************}

constructor TPICkit2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHidDevice := nil;
  FResponseQueue := TQueue.Create;
  FScripts := TPICkit2Scripts.Create(nil);
  FOnAddFirmwareCommand := nil;
  FOnAfterReadFirmware := nil;
  FOnAfterReadStatus   := nil;
  FOnAfterReadVoltages := nil;
  FOnAfterUploadData := nil;
  FOnAfterScriptBufferChksm := nil;
  FOnAfterReadInternalEE := nil;
  FOnAfterLogicAnalyser := nil;

  CommandBufferSize  := 0;
  CommandBufferIndex := 1;
  CommandBuffer := nil;
  ResponseBufferSize := 0;
  ResponseBuffer := nil;
  FFirmware := 'Use ReadFirmware first!';
  FStatus := $FFFF;
end;

destructor TPICkit2.Destroy;
begin
  FreeMem(ResponseBuffer);
  FreeMem(CommandBuffer);
  FreeAndNil(FScripts);
  FreeAndNil(FResponseQueue);
  inherited Destroy;
end;

procedure TPICkit2.ErrorMsg(const Msg : string);
begin
end;


procedure TPICkit2.SetHidDevice(Device : TJvHidDevice);
begin
  FHidDevice := Device;
  CommandBufferSize := FHidDevice.Caps.OutputReportByteLength;
  ResponseBufferSize := FHidDevice.Caps.InputReportByteLength;
  GetMem(CommandBuffer, CommandBufferSize);
  GetMem(ResponseBuffer, ResponseBufferSize);
end;


function TPICkit2.GetConnected : boolean;
begin
  result := FHidDevice <> nil;
end;

function  TPICkit2.NeedFirmwareCommandsFlush : boolean;
begin
  result := (CommandBufferIndex=CommandBufferSize);
end;


procedure TPICkit2.FlushFirmwareCommands;
var
  BytesTransfered : cardinal;
begin
  if CommandBufferIndex <= 1 then exit;
  CommandBuffer^[0] := $00;
  if CommandBufferIndex<CommandBufferSize then
    TFirmwareCommands(CommandBuffer^[CommandBufferIndex]) := ID_END_OF_BUFFER;
  FHidDevice.WriteFile(CommandBuffer^, CommandBufferSize, BytesTransfered);
  CommandBufferIndex := 1;
end;

procedure TPICkit2.GetCommandBufferMem(var P : PByteArray; const Bytes : integer);
begin
  // Check if enough space in buffer, if not flush
  if CommandBufferIndex+Bytes >= CommandBufferSize then FlushFirmwareCommands;
  // returns the begining of reserved space
  P := @CommandBuffer^[CommandBufferIndex];
  // update the index
  Inc(CommandBufferIndex, Bytes);
end;

procedure TPICkit2.AddFirmwareCommand(ID : TFirmwareCommands);
var
  P : PByteArray;
begin
  GetCommandBufferMem(P, 1);
  P^[0] := Byte(ID);
  if NeedFirmwareCommandsFlush then FlushFirmwareCommands;
end;

procedure TPICkit2.AddFirmwareCommand(ID : TFirmwareCommands; const Data; const DataSize : integer);
var
  P : PByteArray;
begin
  GetCommandBufferMem(P, 1+DataSize);
  P^[0] := Byte(ID);
  Move(Data, P^[1], DataSize);
  if NeedFirmwareCommandsFlush then FlushFirmwareCommands;
end;

procedure TPICkit2.AddFirmwareCommand(ID : TFirmwareCommands; const Data1 : byte; const Data; const DataSize : integer);
var
  P : PByteArray;
begin
  GetCommandBufferMem(P, 2+DataSize);
  P^[0] := Byte(ID);
  P^[1] := Data1;
  Move(Data, P^[2], DataSize);
  if NeedFirmwareCommandsFlush then FlushFirmwareCommands;
end;

procedure TPICkit2.QueueResponseProc(ResponseProc : TResponseProc);
var
  Q : TQueuedResponseProc;
begin
  Q := TQueuedResponseProc.Create(ResponseProc);
  FResponseQueue.Push(Q);
end;

function TPICkit2.ReadResponses : boolean;
var
  BytesRead : cardinal;
  Q : TQueuedResponseProc;
begin
  result := false;
  FlushFirmwareCommands;
  while FResponseQueue.Count>0 do begin
    FHidDevice.ReadFile(ResponseBuffer^, ResponseBufferSize, BytesRead);
    if BytesRead<ResponseBufferSize then begin
      // Error
      messagebox(0, PChar(format('ReadResponses : %d bytes read instead of %d', [BytesRead, ResponseBufferSize])), 'Error', MB_OK);
      exit;
    end;
    Q := FResponseQueue.Pop;
    Q.Execute;
    FreeAndNil(Q);
  end;
  result := true;
end;

procedure TPICkit2.ResponseReadVoltages;
begin
  FVDD := RoundTo(MakeWord(ResponseBuffer^[2], ResponseBuffer^[1])/65536*5, -1);
  FVPP := RoundTo(MakeWord(ResponseBuffer^[4], ResponseBuffer^[3])/65536*13.7, -1);
  if Assigned(FOnAfterReadVoltages) then
    FOnAfterReadVoltages(Self, FVDD, FVPP);
end;

procedure TPICkit2.ResponseReadFirmware;
begin
  if Assigned(FOnAfterReadFirmware) then
    FOnAfterReadFirmware(Self, ResponseBuffer^[1], ResponseBuffer^[2], ResponseBuffer^[3]);
  FFirmware  := IntToStr(ResponseBuffer^[1])+'.'+
                IntToStr(ResponseBuffer^[2])+'.'+
                IntToStr(ResponseBuffer^[3]);
end;

procedure TPICkit2.ResponseReadStatus;
begin
  FStatus := MakeWord(ResponseBuffer^[2], ResponseBuffer^[1]);
  if Assigned(FOnAfterReadStatus) then
    FOnAfterReadStatus(Self, FStatus);
end;

procedure TPICkit2.ResponseUploadData;
begin
  if Assigned(FOnAfterUploadData) then
    FOnAfterUploadData(Self, @ResponseBuffer^[2], ResponseBuffer^[1]);
end;

procedure TPICkit2.ResponseUploadDataNoLen;
begin
  if Assigned(FOnAfterUploadData) then
    FOnAfterUploadData(Self, @ResponseBuffer^[1], -1);
end;

procedure TPICkit2.ResponseScriptBufferChksm;
begin
  FScriptLengthSum := MakeWord(ResponseBuffer^[2], ResponseBuffer^[1]);
  FScriptBufferSum := MakeWord(ResponseBuffer^[4], ResponseBuffer^[3]);
  if Assigned(FOnAfterScriptBufferChksm) then
    FOnAfterScriptBufferChksm(Self, FScriptLengthSum, FScriptBufferSum);
end;

procedure TPICkit2.ResponseReadInternalEE;
begin
  if Assigned(FOnAfterReadInternalEE) then
    FOnAfterReadInternalEE(Self, @ResponseBuffer^[1], -1);
end;

procedure TPICkit2.ResponseLogicAnalyser;
begin
  FTrigLoc := MakeWord(ResponseBuffer^[2], ResponseBuffer^[1]);
  if Assigned(FOnAfterLogicAnalyser) then
    FOnAfterLogicAnalyser(Self, FTrigLoc);
end;

procedure TPICkit2.Nop;
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_NO_OPERATION,
      FirmwareCommandText(ID_NO_OPERATION));
  AddFirmwareCommand(ID_NO_OPERATION);
end;

procedure TPICkit2.SetVDD(VDD : real);
var
  Data : packed record
    CPP : word;
    VDDLim : byte;
  end;
begin
  Data.CPP := Trunc((VDD*32)+10.5) shl 6;
  Data.VDDLim := Trunc(0.7*VDD/5*255);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_SETVDD,
      FirmwareCommandText(ID_SETVDD, '$'+IntToHex(Data.CPP, 4)+', $'+IntToHex(Data.VDDLim, 2)));
  AddFirmwareCommand(ID_SETVDD, Data, 3);
end;

procedure TPICkit2.SetVDD(VDD, Vfault : real);
var
  Data : packed record
    CPP : word;
    VDDLim : byte;
  end;
begin
  Data.CPP := Trunc((VDD*32)+10.5) shl 6;
  Data.VDDLim := Trunc(Vfault/5*255);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_SETVDD,
      FirmwareCommandText(ID_SETVDD, '$'+IntToHex(Data.CPP, 4)+', $'+IntToHex(Data.VDDLim, 2)));
  AddFirmwareCommand(ID_SETVDD, Data, 3);
end;

procedure TPICkit2.SetVPP(VPP : real);
var
  Data : packed record
    CCPR2L : byte;
    VPPADC : byte;
    VPPLim : byte;
  end;
begin
  Data.CCPR2L := $40;
  Data.VPPADC := Trunc(VPP * 18.61);
  Data.VPPLim := Trunc(0.7*VPP*18.61);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_SETVPP,
      FirmwareCommandText(ID_SETVPP,
        '$'  +IntToHex(Data.CCPR2L, 2)+
        ', $'+IntToHex(Data.VPPADC, 2)+
        ', $'+IntToHex(Data.VPPLim, 2)));
  AddFirmwareCommand(ID_SETVPP, Data, 3);
end;

procedure TPICkit2.SetVPP(VPP, Vfault : real);
var
  Data : packed record
    CCPR2L : byte;
    VPPADC : byte;
    VPPLim : byte;
  end;
begin
  Data.CCPR2L := $40;
  Data.VPPADC := Trunc(VPP * 18.61);
  Data.VPPLim := Trunc(Vfault*18.61);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_SETVPP,
      FirmwareCommandText(ID_SETVPP,
        '$'  +IntToHex(Data.CCPR2L, 2)+
        ', $'+IntToHex(Data.VPPADC, 2)+
        ', $'+IntToHex(Data.VPPLim, 2)));
  AddFirmwareCommand(ID_SETVPP, Data, 3);
end;


function TPICkit2.ReadFirmware(const DelayedResponse : boolean = false) : string;
ResourceString
  pkFirmwareNA = 'Firmware N/A : ';
begin
  if not Connected then begin
    result := pkFirmwareNA+pkErrorNotConnected;
    ErrorMsg(Result);
    exit;
  end;
  QueueResponseProc(ResponseReadFirmware);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_FIRMWARE_VERSION,
      FirmwareCommandText(ID_FIRMWARE_VERSION));
  AddFirmwareCommand(ID_FIRMWARE_VERSION);
  if DelayedResponse then begin
    result := pkFirmwareNA+pkWarningDelayedResponse;
    exit;
  end;
  ReadResponses;
  result := FFirmware;
end;

function TPICkit2.ReadStatus(const DelayedResponse : boolean = false) : word;
begin
  result := $FFFF;
  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  QueueResponseProc(ResponseReadStatus);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_READ_STATUS,
      FirmwareCommandText(ID_READ_STATUS));
  AddFirmwareCommand(ID_READ_STATUS);
  if DelayedResponse then exit;
  ReadResponses;
  result := FStatus;
end;

procedure TPICkit2.ReadVoltages;
begin
  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  QueueResponseProc(ResponseReadVoltages);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_READ_VOLTAGES,
      FirmwareCommandText(ID_READ_VOLTAGES));
  AddFirmwareCommand(ID_READ_VOLTAGES);
end;

procedure TPICkit2.DownloadScript(const Num : byte; const ScriptData : PByteArray);
begin
  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_DOWNLOAD_SCRIPT,
      FirmwareCommandText(ID_DOWNLOAD_SCRIPT,
        IntToStr(Num)+
        ', $'+IntToHex(Integer(ScriptData), SizeOf(Integer))));
  AddFirmwareCommand(ID_DOWNLOAD_SCRIPT, Num, ScriptData^, ScriptData^[0]);
end;

procedure TPICkit2.Run_Script(const Num, Times : byte);
var
  Data : packed record
    Num : byte;
    Times : byte;
  end;
begin
  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  Data.Num := Num;
  Data.Times := Times;
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_RUN_SCRIPT,
      FirmwareCommandText(ID_RUN_SCRIPT, IntToStr(Num)+', '+IntToStr(Times)));
  AddFirmwareCommand(ID_RUN_SCRIPT, Data, 2);
end;

procedure TPICkit2.ExecuteScript(ScriptData : PByteArray);
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_EXECUTE_SCRIPT,
      FirmwareCommandText(ID_EXECUTE_SCRIPT, '$'+IntToHex(Integer(ScriptData), SizeOf(Integer))));
  AddFirmwareCommand(ID_EXECUTE_SCRIPT, ScriptData^, ScriptData^[0]+1);
end;

procedure TPICkit2.ClrDownloadBuffer;
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_CLR_DOWNLOAD_BUFFER,
      FirmwareCommandText(ID_CLR_DOWNLOAD_BUFFER));
  AddFirmwareCommand(ID_CLR_DOWNLOAD_BUFFER);
end;

procedure TPICkit2.DownloadData(const Data : PByteArray; const DataSize : Integer);
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_DOWNLOAD_DATA,
      FirmwareCommandText(ID_DOWNLOAD_DATA));
  AddFirmwareCommand(ID_DOWNLOAD_DATA, DataSize, Data^, DataSize);
end;

procedure TPICkit2.ClrUploadBuffer;
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_CLR_UPLOAD_BUFFER,
      FirmwareCommandText(ID_CLR_UPLOAD_BUFFER));
  AddFirmwareCommand(ID_CLR_UPLOAD_BUFFER);
end;

procedure TPICkit2.UploadData(const Dest : PByteArray; const DataSize : Integer; var BytesRead : Integer; const DelayedResponse : boolean = false);
begin
  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  QueueResponseProc(ResponseUploadData);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_UPLOAD_DATA,
      FirmwareCommandText(ID_UPLOAD_DATA, '!!!! � programmer !!!'));
  AddFirmwareCommand(ID_UPLOAD_DATA);
  if DelayedResponse then exit;
  ReadResponses;
end;

procedure TPICkit2.EndOfBuffer;
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_END_OF_BUFFER,
      FirmwareCommandText(ID_END_OF_BUFFER));
  AddFirmwareCommand(ID_END_OF_BUFFER);
  FlushFirmwareCommands;
end;

procedure TPICkit2.Reset;
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_RESET,
      FirmwareCommandText(ID_RESET));
  AddFirmwareCommand(ID_RESET);
end;

procedure TPICkit2.ScriptBufferChksm;
begin
  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  QueueResponseProc(ResponseScriptBufferChksm);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_SCRIPT_BUFFER_CHKSM,
      FirmwareCommandText(ID_SCRIPT_BUFFER_CHKSM));
  AddFirmwareCommand(ID_SCRIPT_BUFFER_CHKSM);
  if DelayedResponse then exit;
  ReadResponses;
end;

procedure TPICkit2.SetVoltageCals(const CalFactor : word; const VddOffset, VddCalFactor : byte);
var
  Data : packed record
    CalFactor    : word;
    VddOffset    : byte;
    VddCalFactor : byte;
  end;
begin
  Data.CalFactor := CalFactor;
  Data.VddOffset := VddOffset;
  Data.VddCalFactor := VddCalFactor;
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_SET_VOLTAGE_CALS,
      FirmwareCommandText(ID_SET_VOLTAGE_CALS));
  AddFirmwareCommand(ID_SET_VOLTAGE_CALS, Data, 4);
end;

procedure TPICkit2.WriteInternalEE(const Addrs : byte; const Data : PByteArray; const DataSize : integer);
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_WR_INTERNAL_EE,
      FirmwareCommandText(ID_WR_INTERNAL_EE));
  AddFirmwareCommand(ID_WR_INTERNAL_EE, Addrs, Data^, DataSize);
end;

procedure TPICkit2.ReadInternalEE(const Addrs : byte; const Dest : PByteArray; const BytesToRead : integer);
begin
  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  QueueResponseProc(ResponseReadInternalEE);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_RD_INTERNAL_EE,
      FirmwareCommandText(ID_RD_INTERNAL_EE));
  AddFirmwareCommand(ID_RD_INTERNAL_EE, Addrs, BytesToRead, 1);
  if DelayedResponse then exit;
  ReadResponses;
end;

procedure TPICkit2.EnterUART(const Baud : word);
var
  BaudValue : word;
begin
  BaudValue := 65536-Trunc((((1/Baud)-3e-6)/1.67e-7));
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_ENTER_UART_MODE,
      FirmwareCommandText(ID_ENTER_UART_MODE));
  AddFirmwareCommand(ID_ENTER_UART_MODE, BaudValue, 2);
end;

procedure TPICkit2.ExitUART;
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_EXIT_UART_MODE,
      FirmwareCommandText(ID_EXIT_UART_MODE));
  AddFirmwareCommand(ID_EXIT_UART_MODE);
end;

procedure TPICkit2.EnterLEARN;
const
  Key : Packed array [1..3] of byte = ($50, $4B, $32);
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_ENTER_LEARN_MODE,
      FirmwareCommandText(ID_ENTER_LEARN_MODE));
  AddFirmwareCommand(ID_ENTER_LEARN_MODE, Key, 3);
end;

procedure TPICkit2.ExitLEARN;
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_EXIT_LEARN_MODE,
      FirmwareCommandText(ID_EXIT_LEARN_MODE));
  AddFirmwareCommand(ID_EXIT_LEARN_MODE);
end;

procedure TPICkit2.EnablePK2GO(const EEPROM : byte);
var
  Data : packed record
    Key : packed array[1..3] of byte;
    EEPROM : byte;
  end;
begin
  Data.Key[1] := $50;
  Data.Key[2] := $4B;
  Data.Key[3] := $32;
  Data.EEPROM := EEPROM;
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_ENABLE_PK2GO_MODE,
      FirmwareCommandText(ID_ENABLE_PK2GO_MODE));
  AddFirmwareCommand(ID_ENABLE_PK2GO_MODE, Data, 4);
end;

procedure TPICkit2.LogicAnalyser(const Edge, TrigMask, TrigStates, EdgeMask, TrigCount : byte;
                        const PostTrigCount : Word;
                        const SampleRateFactor : byte);
var
  Data : packed record
    Edge : byte;
    TrigMask   : byte;
    TrigStates : byte;
    EdgeMask   : byte;
    TrigCount  : byte;
    PostTrigCount : word;
    SampleRateFactor : byte;
  end;
begin
  Data.Edge := Edge;
  Data.TrigMask := TrigMask;
  Data.TrigStates := TrigStates;
  Data.EdgeMask := EdgeMask;
  Data.TrigCount := TrigCount;
  Data.PostTrigCount := PostTrigCount;
  Data.SampleRateFactor := SampleRateFactor;

  if not Connected then begin
    ErrorMsg(pkErrorNotConnected);
    exit;
  end;
  QueueResponseProc(ResponseLogicAnalyser);
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_LOGIC_ANALYSER_GO,
      FirmwareCommandText(ID_LOGIC_ANALYSER_GO));
  AddFirmwareCommand(ID_LOGIC_ANALYSER_GO, Data, 8);
  if DelayedResponse then exit;
  ReadResponses;
end;

procedure TPICkit2.CopyRamUpload(const Addrs : word);
begin
  if Assigned(FOnAddFirmwareCommand) then
    FOnAddFirmwareCommand(Self, ID_COPY_RAM_UPLOAD,
      FirmwareCommandText(ID_COPY_RAM_UPLOAD));
  AddFirmwareCommand(ID_COPY_RAM_UPLOAD, Addrs, 2);
end;

function FirmwareCommandText(const ID : TFirmwareCommands) : string;
begin
  result := SFirmwareCommands[ID];
end;

function FirmwareCommandText(const ID : TFirmwareCommands; ParamsText : string) : string;
begin
  result := SFirmwareCommands[ID]+'('+ParamsText+')';
end;

procedure Register;
begin
  RegisterComponents('PICKit2', [TPICkit2]);
end;

initialization

  Fillchar(SFirmwareCommands, SizeOf(SFirmwareCommands), 0);
    SFirmwareCommands[ID_ENTER_BOOTLOADER] := 'ENTER_BOOTLOADER';
    SFirmwareCommands[ID_NO_OPERATION] := 'NO_OPERATION';
    SFirmwareCommands[ID_FIRMWARE_VERSION] := 'FIRMWARE_VERSION';
    SFirmwareCommands[ID_SETVDD] := 'SETVDD';
    SFirmwareCommands[ID_SETVPP] := 'SETVPP';
    SFirmwareCommands[ID_READ_STATUS] := 'READ_STATUS';
    SFirmwareCommands[ID_READ_VOLTAGES] := 'READ_VOLTAGES';
    SFirmwareCommands[ID_DOWNLOAD_SCRIPT] := 'DOWNLOAD_SCRIPT';
    SFirmwareCommands[ID_RUN_SCRIPT] := 'RUN_SCRIPT';
    SFirmwareCommands[ID_EXECUTE_SCRIPT] := 'EXECUTE_SCRIPT';
    SFirmwareCommands[ID_CLR_DOWNLOAD_BUFFER] := 'CLR_DOWNLOAD_BUFFER';
    SFirmwareCommands[ID_DOWNLOAD_DATA] := 'DOWNLOAD_DATA';
    SFirmwareCommands[ID_CLR_UPLOAD_BUFFER] := 'CLR_UPLOAD_BUFFER';
    SFirmwareCommands[ID_UPLOAD_DATA] := 'UPLOAD_DATA';
    SFirmwareCommands[ID_CLR_SCRIPT_BUFFER] := 'CLR_SCRIPT_BUFFER';
    SFirmwareCommands[ID_UPLOAD_DATA_NOLEN] := 'UPLOAD_DATA_NOLEN';
    SFirmwareCommands[ID_END_OF_BUFFER] := 'END_OF_BUFFER';
    SFirmwareCommands[ID_RESET] := 'RESET';
    SFirmwareCommands[ID_SCRIPT_BUFFER_CHKSM] := 'SCRIPT_BUFFER_CHKSM';
    SFirmwareCommands[ID_SET_VOLTAGE_CALS] := 'SET_VOLTAGE_CALS';
    SFirmwareCommands[ID_WR_INTERNAL_EE] := 'WR_INTERNAL_EE';
    SFirmwareCommands[ID_RD_INTERNAL_EE] := 'RD_INTERNAL_EE';
    SFirmwareCommands[ID_ENTER_UART_MODE] := 'ENTER_UART_MODE';
    SFirmwareCommands[ID_EXIT_UART_MODE] := 'EXIT_UART_MODE';
    SFirmwareCommands[ID_ENTER_LEARN_MODE] := 'ENTER_LEARN_MODE';
    SFirmwareCommands[ID_EXIT_LEARN_MODE] := 'EXIT_LEARN_MODE';
    SFirmwareCommands[ID_ENABLE_PK2GO_MODE] := 'ENABLE_PK2GO_MODE';
    SFirmwareCommands[ID_LOGIC_ANALYSER_GO] := 'LOGIC_ANALYSER_GO';
    SFirmwareCommands[ID_COPY_RAM_UPLOAD] := 'COPY_RAM_UPLOAD';

finalization

end.
