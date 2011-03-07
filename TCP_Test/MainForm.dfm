object Form1: TForm1
  Left = 198
  Top = 114
  Width = 706
  Height = 308
  Caption = 'TCP/IP Tester'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object HostEdit: TJvEdit
    Left = 96
    Top = 56
    Width = 241
    Height = 21
    TabOrder = 0
    Text = 'HostEdit'
  end
  object HostText: TJvStaticText
    Left = 16
    Top = 56
    Width = 35
    Height = 17
    Caption = 'Server'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Layout = tlTop
    TabOrder = 1
    TextMargins.X = 0
    TextMargins.Y = 0
    WordWrap = False
  end
  object PortText: TJvStaticText
    Left = 16
    Top = 120
    Width = 23
    Height = 17
    Caption = 'Port'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Layout = tlTop
    TabOrder = 2
    TextMargins.X = 0
    TextMargins.Y = 0
    WordWrap = False
  end
  object PortEdit: TJvEdit
    Left = 96
    Top = 120
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '4000'
  end
  object RemoteEdit: TJvEdit
    Left = 96
    Top = 88
    Width = 241
    Height = 21
    Enabled = False
    TabOrder = 4
    Text = 'Remote'
  end
  object RemoteText: TJvStaticText
    Left = 16
    Top = 88
    Width = 30
    Height = 17
    Caption = 'Client'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Layout = tlTop
    TabOrder = 5
    TextMargins.X = 0
    TextMargins.Y = 0
    WordWrap = False
  end
  object ConnectBtn: TJvBitBtn
    Left = 16
    Top = 160
    Width = 185
    Height = 25
    Caption = 'Connect'
    TabOrder = 6
    OnClick = ConnectBtnClick
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object StatusCB: TJvCheckBox
    Left = 216
    Top = 160
    Width = 65
    Height = 17
    Caption = 'StatusCB'
    Enabled = False
    TabOrder = 7
    LinkedControls = <>
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object Memo: TJvMemo
    Left = 376
    Top = 16
    Width = 297
    Height = 249
    Enabled = False
    TabOrder = 8
  end
  object SendEdit: TJvEdit
    Left = 16
    Top = 192
    Width = 225
    Height = 21
    Enabled = False
    TabOrder = 9
  end
  object SendBtn: TJvBitBtn
    Left = 248
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Send'
    Enabled = False
    TabOrder = 10
    OnClick = SendBtnClick
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object RcvBtn: TJvBitBtn
    Left = 248
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Rcv'
    Enabled = False
    TabOrder = 11
    Visible = False
    OnClick = RcvBtnClick
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 8
    Width = 289
    Height = 41
    Caption = 'Mode'
    TabOrder = 12
    object ServerRB: TJvRadioButton
      Left = 56
      Top = 16
      Width = 52
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Server'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ClientServerClick
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      LinkedControls = <>
    end
    object ClientRB: TJvRadioButton
      Left = 8
      Top = 16
      Width = 47
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Client'
      TabOrder = 1
      OnClick = ClientServerClick
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      LinkedControls = <>
    end
    object TelnetCB: TCheckBox
      Left = 128
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Telnet'
      TabOrder = 2
    end
  end
  object JvAppXMLFileStorage1: TJvAppXMLFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.InvalidCharReplacement = '_'
    RootNodeName = 'Configuration'
    SubStorages = <>
    Left = 528
    Top = 40
  end
  object IdIPWatch1: TIdIPWatch
    Active = False
    HistoryFilename = 'iphist.dat'
    Left = 456
    Top = 40
  end
  object IdTelnet1: TIdTelnet
    MaxLineAction = maException
    ReadTimeout = 0
    Port = 23
    OnDataAvailable = IdTelnet1DataAvailable
    Terminal = 'dumb'
    Left = 408
    Top = 80
  end
end
