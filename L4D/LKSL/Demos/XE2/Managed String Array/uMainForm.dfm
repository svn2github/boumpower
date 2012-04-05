object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 
    'LaKraven Studios Standard Library (LKSL) - Managed String Array ' +
    'Demo'
  ClientHeight = 559
  ClientWidth = 760
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object memValues: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 280
    Width = 754
    Height = 226
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 754
    Height = 246
    Align = alTop
    Caption = 'Managed String Array Tests'
    TabOrder = 1
    object GroupBox2: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 744
      Height = 106
      Align = alTop
      Caption = 'Add Key && Value'
      TabOrder = 0
      DesignSize = (
        744
        106)
      object Label1: TLabel
        Left = 3
        Top = 24
        Width = 55
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Name:'
      end
      object Label2: TLabel
        Left = 3
        Top = 48
        Width = 55
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Value:'
      end
      object edKey: TEdit
        Left = 64
        Top = 21
        Width = 670
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object edValue: TEdit
        Left = 64
        Top = 45
        Width = 670
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object Button2: TButton
        Left = 64
        Top = 72
        Width = 329
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Insert Key/Value Pair into Managed Array'
        TabOrder = 2
        OnClick = Button2Click
      end
      object Button4: TButton
        Left = 399
        Top = 72
        Width = 335
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Load a Comma-separated Key/Value CSV File'
        TabOrder = 3
        OnClick = Button4Click
      end
    end
    object GroupBox3: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 130
      Width = 744
      Height = 113
      Align = alTop
      Caption = 'Search Value by Key'
      TabOrder = 1
      DesignSize = (
        744
        113)
      object Label3: TLabel
        Left = 3
        Top = 24
        Width = 55
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Name:'
      end
      object Label4: TLabel
        Left = 3
        Top = 84
        Width = 55
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Passes:'
      end
      object edSearchKey: TEdit
        Left = 64
        Top = 21
        Width = 670
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object Button1: TButton
        Left = 64
        Top = 48
        Width = 329
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Search using Optimized Array Manager'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button7: TButton
        Left = 399
        Top = 48
        Width = 335
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Search using Iterative Lookup (like TList does, with a "For loop' +
          '")'
        TabOrder = 2
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 152
        Top = 79
        Width = 582
        Height = 25
        Caption = 
          'Benchmark Optimized Array Manager against Iterative Lookup (TLis' +
          't/For-loop)'
        TabOrder = 3
        OnClick = Button8Click
      end
      object sePasses: TSpinEdit
        Left = 64
        Top = 81
        Width = 82
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 10000
      end
    end
  end
  object Button3: TButton
    Left = 0
    Top = 509
    Width = 760
    Height = 25
    Align = alBottom
    Caption = 'List all Key/Value Pairs (in order)'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 0
    Top = 534
    Width = 760
    Height = 25
    Align = alBottom
    Caption = 'Save all Key/Value Pairs to CSV File'
    TabOrder = 3
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 0
    Top = 252
    Width = 760
    Height = 25
    Align = alTop
    Caption = 'Clear Array'
    TabOrder = 4
    OnClick = Button6Click
  end
  object odCSV: TOpenDialog
    Left = 168
    Top = 8
  end
  object sdCSV: TSaveDialog
    Left = 232
    Top = 8
  end
end
