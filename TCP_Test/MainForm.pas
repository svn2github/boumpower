unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sockets, StdCtrls, JvExStdCtrls, JvRadioButton, JvComponentBase,
  JvAppStorage, JvAppXMLStorage, JvExControls, JvStaticText, JvEdit,
  Buttons, JvExButtons, JvBitBtn, JvCheckBox, JvMemo, IdBaseComponent,
  IdComponent, IdTCPServer, IdTCPConnection, IdTCPClient;

type
  TForm1 = class(TForm)
    JvAppXMLFileStorage1: TJvAppXMLFileStorage;
    ClientRB: TJvRadioButton;
    ServerRB: TJvRadioButton;
    HostEdit: TJvEdit;
    HostText: TJvStaticText;
    PortText: TJvStaticText;
    PortEdit: TJvEdit;
    RemoteEdit: TJvEdit;
    RemoteText: TJvStaticText;
    ConnectBtn: TJvBitBtn;
    StatusCB: TJvCheckBox;
    Memo: TJvMemo;
    SendEdit: TJvEdit;
    SendBtn: TJvBitBtn;
    IdTCPServer1: TIdTCPServer;
    IdTCPClient1: TIdTCPClient;
    RcvBtn: TJvBitBtn;
    procedure ClientServerClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure TcpClient1Error(Sender: TObject; SocketError: Integer);
    procedure SendBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IdTCPServer1Execute(AThread: TIdPeerThread);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IdTCPClient1Work(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
    procedure IdTCPClient1Connected(Sender: TObject);
    procedure IdTCPClient1Disconnected(Sender: TObject);
    procedure IdTCPClient1Status(ASender: TObject;
      const AStatus: TIdStatus; const AStatusText: String);
    procedure IdTCPServer1Connect(AThread: TIdPeerThread);
    procedure IdTCPServer1Disconnect(AThread: TIdPeerThread);
    procedure RcvBtnClick(Sender: TObject);
  private
    PeerThread: TIdPeerThread;
  public
    procedure Connect;
    procedure Disconnect;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ClientServerClick(Sender: TObject);
begin
  if ClientRB.Checked then begin
    // Client
    HostEdit.Enabled := true;
    RcvBtn.Visible := true;
    HostEdit.Text := 'Type server name or IP';
    with IdTCPClient1 do begin
      RemoteEdit.Text := Host;
    end;
  end else begin
    // Server
    HostEdit.Enabled := false;
    RcvBtn.Visible := false;
    with IdTCPServer1 do begin
      HostEdit.Text := LocalName;
      RemoteEdit.Text := 'not connected';
    end;
  end;
end;

procedure TForm1.ConnectBtnClick(Sender: TObject);
begin
  if ConnectBtn.Caption = 'Connect' then
    Connect
  else
    Disconnect;
end;

procedure TForm1.Connect;
var
  Port : integer;
begin
  ClientRB.Enabled := false;
  ServerRB.Enabled := false;
  HostEdit.Enabled := false;
  PortEdit.Enabled := false;
  SendEdit.Enabled := false;
  SendBtn.Enabled := false;

  TryStrToInt(PortEdit.Text, Port);
  PortEdit.Text := IntToStr(Port);

  StatusCB.Checked := false;
  StatusCB.Caption := 'Connecté';
  if ClientRB.Checked then begin
    IdTCPServer1.Active := false;
    IdTCPClient1.Host := HostEdit.Text;
    IdTCPClient1.Port := Port;
    IdTCPClient1.Connect;
  end else begin
    IdTCPClient1.Disconnect;
    IdTCPServer1.DefaultPort := Port;
    IdTCPServer1.Active := true;
  end;
  ConnectBtn.Caption := 'Disconnect'
end;

procedure TForm1.Disconnect;
begin
  IdTCPClient1.Disconnect;
  IdTCPServer1.Active := false;
  ClientRB.Enabled := true;
  ServerRB.Enabled := true;
  HostEdit.Enabled := false;
  PortEdit.Enabled := true;
  SendEdit.Enabled := false;
  SendBtn.Enabled := false;

  StatusCB.Checked := false;
  StatusCB.Caption := 'Connecté';
  ConnectBtn.Caption := 'Connect';
  ClientServerClick(nil);
end;

procedure TForm1.TcpClient1Error(Sender: TObject; SocketError: Integer);
begin
  StatusCB.Checked := false;
  StatusCB.Caption := 'Connecté : Erreur '+IntToStr(SocketError);
  SendEdit.Enabled := false;
  SendBtn.Enabled := false;
end;

procedure TForm1.SendBtnClick(Sender: TObject);
begin
  if ClientRB.Checked then begin
    Memo.Lines.Add(IdTCPClient1.LocalName+' send : '+SendEdit.Text);
    IdTCPClient1.WriteLn(SendEdit.Text);
  end else begin
    if Assigned(PeerThread) then begin
      Memo.Lines.Add(PeerThread.Connection.LocalName+' send : '+SendEdit.Text);
      PeerThread.Connection.WriteLn(SendEdit.Text);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PeerThread := nil;
  self.ClientServerClick(nil);
end;

procedure TForm1.IdTCPServer1Execute(AThread: TIdPeerThread);
var
  s : string;
begin
  s := AThread.Connection.ReadLn;
  // Answer
  AThread.Connection.WriteLn(SendEdit.Text);
  Memo.Lines.Add('Rec : '+s);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Disconnect;
end;

procedure TForm1.IdTCPClient1Work(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
begin
  Memo.Lines.Add('ClientWork : '+IntToStr(AWorkCount)+' bytes');
  Case AWorkMode of
    wmRead : ;
    wmWrite : ;
  end;

end;

procedure TForm1.IdTCPClient1Connected(Sender: TObject);
begin
  StatusCB.Checked := true;
  StatusCB.Caption := 'Connecté';
  SendEdit.Enabled := true;
  SendBtn.Enabled := true;
  RcvBtn.Enabled := true;
end;

procedure TForm1.IdTCPClient1Disconnected(Sender: TObject);
begin
  StatusCB.Checked := false;
  StatusCB.Caption := 'Connecté';
  SendEdit.Enabled := false;
  SendBtn.Enabled := false;
  RcvBtn.Enabled := false;
end;

procedure TForm1.IdTCPClient1Status(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: String);
begin
  Memo.Lines.Add('ClientStatus : '+AStatusText);
end;

procedure TForm1.IdTCPServer1Connect(AThread: TIdPeerThread);
var
  s : string;
begin
  PeerThread := AThread;
  s := AThread.Connection.LocalName;
  Memo.Lines.Add(s+' connected');
  SendEdit.Enabled := true;
  SendBtn.Enabled := true;
  RcvBtn.Enabled := true;
end;

procedure TForm1.IdTCPServer1Disconnect(AThread: TIdPeerThread);
var
  s : string;
begin
  PeerThread := nil;
  s := AThread.Connection.LocalName;
  Memo.Lines.Add(s+' disconnected');
  SendEdit.Enabled := false;
  SendBtn.Enabled := false;
  RcvBtn.Enabled := false;
end;

procedure TForm1.RcvBtnClick(Sender: TObject);
begin
    Memo.Lines.Add('Rcv  : '+IdTCPClient1.ReadLn);
end;

end.
