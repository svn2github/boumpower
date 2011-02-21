object Form1: TForm1
  Left = 0
  Top = 0
  Width = 583
  Height = 323
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    575
    294)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 456
    Top = 128
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Memo1: TMemo
    Left = 24
    Top = 16
    Width = 433
    Height = 89
    Lines.Strings = (
      'F = GetObject('#39'Form1'#39');'
      'F.Font.Name = '#39'Times New Roman'#39';'
      'F.Label1.Caption = "Component n'#176' 3 is ".. F.Components[3].Name')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 352
    Top = 120
    Width = 89
    Height = 25
    Caption = 'List of Params'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 0
    Top = 120
    Width = 345
    Height = 153
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'Memo2')
    TabOrder = 2
  end
  object ButtonLua: TButton
    Left = 464
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Exec Lua'
    TabOrder = 3
    OnClick = ButtonLuaClick
  end
  object Panel1: TPanel
    Left = 352
    Top = 176
    Width = 217
    Height = 97
    Caption = 'Panel1'
    TabOrder = 4
  end
  object Button2: TButton
    Left = 464
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Sfoglia...'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 352
    Top = 144
    Width = 153
    Height = 25
    Caption = 'List of Params TComponent'
    TabOrder = 6
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Left = 408
    Top = 72
  end
end
