unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sockets, StdCtrls, JvExStdCtrls, JvRadioButton, JvComponentBase,
  JvAppStorage, JvAppXMLStorage, JvExControls, JvStaticText, JvEdit,
  Buttons, JvExButtons, JvBitBtn, JvCheckBox, JvMemo,
  IdStack, IdBaseComponent,
  IdComponent, IdTCPServer, IdTCPConnection, IdTCPClient, IdTelnetServer,
  IdTelnet, IdIPWatch;

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
    RcvBtn: TJvBitBtn;
    GroupBox1: TGroupBox;
    TelnetCB: TCheckBox;
    IdIPWatch1: TIdIPWatch;
    procedure ClientServerClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure TcpClient1Error(Sender: TObject; SocketError: Integer);
    procedure SendBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IdTCPServer1Execute(AThread: TIdPeerThread);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IdTCPClient1Connected(Sender: TObject);
    procedure IdTCPClient1Disconnected(Sender: TObject);
    procedure IdTCPClient1Status(ASender: TObject;
      const AStatus: TIdStatus; const AStatusText: String);
    procedure IdTCPServer1Connect(AThread: TIdPeerThread);
    procedure IdTCPServer1Disconnect(AThread: TIdPeerThread);
    procedure RcvBtnClick(Sender: TObject);
    procedure IdTelnet1DataAvailable(Sender: TIdTelnet;
      const Buffer: String);
  private
    PeerThread: TIdPeerThread;
  public
    IdTCPServer: TIdTCPServer;
    IdTCPClient: TIdTCPClient;
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
    RemoteEdit.Text := '';
  end else begin
    // Server
    HostEdit.Enabled := false;
    RcvBtn.Visible := false;
//    HostEdit.Text := GStack.WSGetHostName;
    HostEdit.Text := IdIPWatch1.LocalName;
    RemoteEdit.Text := 'not connected';
  end;
end;

procedure TForm1.ConnectBtnClick(Sender: TObject);
begin
  if ConnectBtn.Caption = 'Connect' then begin
    Disconnect;
    Connect;
    ConnectBtn.Caption := 'Disconnect'
  end else begin
    Disconnect;
    ClientServerClick(nil);
    ConnectBtn.Caption := 'Connect';
  end;
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
  // Create and activate
  if ClientRB.Checked then begin
    if TelnetCB.Checked then begin
      IdTCPClient := TIdTelnet.Create(self);
      with IdTCPClient as TIdTelnet do
        OnDataAvailable := self.IdTelnet1DataAvailable;
    end else
      IdTCPClient := TIdTCPClient.Create(self);
    IdTCPClient.OnConnected    := self.IdTCPClient1Connected;
    IdTCPClient.OnDisconnected := self.IdTCPClient1Disconnected;
    IdTCPClient.OnStatus       := self.IdTCPClient1Status;
    IdTCPClient.Host           := HostEdit.Text;
    IdTCPClient.Port           := Port;
    IdTCPClient.Connect;
  end else begin
    if TelnetCB.Checked then
      IdTCPServer := TIdTelnetServer.Create(self)
    else
      IdTCPServer := TIdTCPServer.Create(self);
    IdTCPServer.OnConnect    := self.IdTCPServer1Connect;
    IdTCPServer.OnDisconnect := self.IdTCPServer1Disconnect;
    IdTCPServer.OnExecute    := self.IdTCPServer1Execute;
    IdTCPServer.DefaultPort := Port;
    IdTCPServer.MaxConnections := 1;
    IdTCPServer.Active := true;
  end;
end;

procedure TForm1.Disconnect;
begin
  if Assigned(IdTCPClient) then begin
    IdTCPClient.Disconnect;
    FreeAndNil(IdTCPClient);
  end;
  if Assigned(IdTCPServer) then begin
    IdTCPServer.Active := false;
    FreeAndNil(IdTCPServer);
  end;
  ClientRB.Enabled := true;
  ServerRB.Enabled := true;
  HostEdit.Enabled := false;
  PortEdit.Enabled := true;
  SendEdit.Enabled := false;
  SendBtn.Enabled := false;

  StatusCB.Checked := false;
  StatusCB.Caption := 'Connecté';
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
    Memo.Lines.Add(IdTCPClient.LocalName+' send : '+SendEdit.Text);
    IdTCPClient.WriteLn(SendEdit.Text);
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
  IdTCPServer := nil;
  IdTCPClient := nil;
  ClientServerClick(nil);
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

procedure TForm1.IdTCPClient1Connected(Sender: TObject);
begin
  StatusCB.Checked := true;
  StatusCB.Caption := 'Connecté';
  SendEdit.Enabled := true;
  SendBtn.Enabled := true;
  if not TelnetCB.Checked then RcvBtn.Enabled := true;
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
    Memo.Lines.Add('Rcv  : '+IdTCPClient.ReadLn);
end;

procedure TForm1.IdTelnet1DataAvailable(Sender: TIdTelnet;
  const Buffer: String);
begin
    Memo.Lines.Add('Rcv  : '+Buffer);
end;

end.
