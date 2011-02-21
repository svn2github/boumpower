object Form1: TForm1
  Left = 244
  Top = 114
  Width = 696
  Height = 480
  Caption = 'Lecteur  de discussions'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Fils: TCheckListBox
    Left = 384
    Top = 16
    Width = 297
    Height = 241
    ItemHeight = 13
    TabOrder = 2
  end
  object WebBrowser: TWebBrowser
    Left = 8
    Top = 16
    Width = 369
    Height = 241
    TabOrder = 0
    OnNavigateComplete2 = WebBrowserNavigateComplete2
    OnDocumentComplete = WebBrowserDocumentComplete
    ControlData = {
      4C00000023260000E81800000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 425
    Width = 681
    Height = 19
    Panels = <>
  end
  object Log: TRichEdit
    Left = 8
    Top = 272
    Width = 673
    Height = 153
    Lines.Strings = (
      'Log')
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object HTMLParser: TJvHTMLParser
    Parser.Strings = (
      'FIL'
      'litfil.asp'
      '"'
      '0'
      '0')
    OnKeyFound = FeedRootFound
    Left = 648
    Top = 8
  end
  object FeedSettings: TJvSimpleXML
    IndentString = '  '
    Left = 616
    Top = 8
  end
  object HTTP: TIdHTTP
    MaxLineAction = maException
    ReadTimeout = 0
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = 0
    Request.ContentRangeStart = 0
    Request.ContentType = 'text/html'
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 584
    Top = 8
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 552
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 520
    Top = 8
    object Fichier1: TMenuItem
      Caption = '&Fichier'
      object Executer1: TMenuItem
        Caption = '&Executer'
        OnClick = Executer1Click
      end
      object Quiter1: TMenuItem
        Caption = '&Quiter'
        OnClick = Quiter1Click
      end
    end
  end
end
