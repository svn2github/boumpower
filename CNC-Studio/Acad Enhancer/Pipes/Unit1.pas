unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TAcadEnhThread = class;
  TAcadEnhThreadCallBack = function(Sender : TAcadEnhThread; Buffer : pointer; BufferSize : integer) : string of object;
  TAcadEnhThread = class(TThread)
  private
    fPID        : cardinal;
    hPipeArgs   : THandle;
    hPipeResult : THandle;
    hPipeEvent  : THandle;
    fCallBack   : TAcadEnhThreadCallBack;
  protected
    procedure BuildPipes;
    procedure ProcessPipeMessage;
    procedure SendResult(Buffer : pointer; BufferSize : cardinal);
    procedure Execute; override;
  public
    constructor Create(APID : cardinal; ACallBack : TAcadEnhThreadCallBack); overload;
    destructor Destroy; override;
  end;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Button3: TButton;
    Edit2: TEdit;
    Button4: TButton;
    CheckBox1: TCheckBox;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    fAcadEnhThread : TAcadEnhThread;
    function ArgsPipe(Sender : TAcadEnhThread; Buffer : pointer; BufferSize : integer) : string;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  PipeName = '\\.\pipe\AcadEnh';

constructor TAcadEnhThread.Create(APID : cardinal; ACallBack : TAcadEnhThreadCallBack);
begin
  Inherited Create(True);
  fPID := APID;
  FCallBack := ACallBack;
  BuildPipes;
  Resume;
end;

destructor TAcadEnhThread.Destroy;
begin
  CloseHandle(hPipeArgs);
  CloseHandle(hPipeResult);
  CloseHandle(hPipeEvent);
  Inherited Destroy;
end;

procedure TAcadEnhThread.BuildPipes;
begin
  hPipeArgs := CreateNamedPipe(
    PChar(PipeName+'_ARGS'),
    PIPE_ACCESS_INBOUND,
    PIPE_TYPE_MESSAGE,
    1,
    1024, 1024,
    100,
    nil);

    hPipeResult := CreateNamedPipe(
    PChar(PipeName+'_RESULT'),
    PIPE_ACCESS_OUTBOUND,
    PIPE_TYPE_MESSAGE,
    1,
    1024, 1024,
    100,
    nil);

  hPipeEvent := CreateEvent(nil, false, false, PChar(Pipename+'_CompletionEvent'));
end;

procedure TAcadEnhThread.ProcessPipeMessage;
var
  Buffer : pointer;
  BufferSize : cardinal;
  ByteRead : Cardinal;
  ByteWritten : cardinal;
  Result : string;
begin
  BufferSize := 0;
  Result := '';
  While BufferSize=0 do
    PeekNamedPipe(hPipeArgs, nil, 0, nil, @BufferSize, nil);
  GetMem(Buffer, BufferSize);
  try
    ReadFile(hPipeArgs, Buffer^, BufferSize, ByteRead, nil);
    if Assigned(FCallBack) then
      Result := FCallBack(Self, Buffer, BufferSize-2);
    Result := Result+#13#10;
    WriteFile(hPipeResult, PChar(Result)^, Length(Result), ByteWritten, nil);
  finally
    DisconnectNamedPipe(hPipeArgs);
    FreeMem(Buffer);
  end;
end;

procedure TAcadEnhThread.SendResult(Buffer : pointer; BufferSize : cardinal);
var
  ByteWritten : cardinal;
begin
//  WriteFile(hResultPipe, Buffer^, BufferSize, ByteWritten, nil);
end;


procedure TAcadEnhThread.Execute;
var
  Overlapped : TOverlapped;
  ErrCode    : integer;
begin
  while not Terminated do begin
    ConnectNamedPipe(hPipeArgs, nil);
    ProcessPipeMessage;
  end;
  exit;

  if hPipeArgs<>INVALID_HANDLE_VALUE then begin
    ZeroMemory(@Overlapped, SizeOf(Overlapped));
    Overlapped.hEvent := HPipeEvent;
    ConnectNamedPipe(hPipeArgs, @Overlapped);
    while not Terminated do begin
      Case WaitForSingleObject(hPipeEvent, 100) of
        WAIT_TIMEOUT   : {No connection} ;
        WAIT_OBJECT_0  : ProcessPipeMessage;
        WAIT_ABANDONED : Terminate;
        WAIT_FAILED    : begin
//            ErrCode := GetLastError;
//            raise exception.CreateFmt('Wait failed. Error: %s', [SysErrorMessage(ErrCode)+' '+IntToStr(ErrCode)]);
          end;
      end;
    end;
  end;
end;

function TForm1.ArgsPipe(Sender : TAcadEnhThread; Buffer : pointer; BufferSize : integer) : string;
var
  S : string;
begin
  result := '"OK"';
  SetString(S, PChar(Buffer), BufferSize);
  Edit1.Text := S;
  Edit2.Text := Result;
  Edit1.Update;
  Edit2.Update;
end;


procedure TForm1.Button3Click(Sender: TObject);
begin
  fAcadEnhThread.Terminate;
  CheckBox1.Checked :=  false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  fAcadEnhThread := TAcadEnhThread.Create(0, ArgsPipe);
  FAcadEnhThread.FreeOnTerminate := true;
  CheckBox1.Checked := true;
end;

end.
