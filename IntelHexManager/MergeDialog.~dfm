object RelocateDlg: TRelocateDlg
  Left = 418
  Top = 307
  BorderStyle = bsDialog
  Caption = 'Intel HEX : Relocate'
  ClientHeight = 279
  ClientWidth = 536
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object SrcLabel: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'Liste source :'
  end
  object DstLabel: TLabel
    Left = 376
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'Liste destination :'
  end
  object IncludeBtn: TSpeedButton
    Left = 208
    Top = 32
    Width = 81
    Height = 24
    Caption = '>'
    OnClick = IncludeBtnClick
  end
  object CopyBtn: TSpeedButton
    Left = 208
    Top = 64
    Width = 81
    Height = 24
    Caption = 'Copy'
    OnClick = CopyBtnClick
  end
  object ExcludeBtn: TSpeedButton
    Left = 208
    Top = 96
    Width = 24
    Height = 24
    Caption = '<'
    Enabled = False
    OnClick = ExcludeBtnClick
  end
  object OKBtn: TButton
    Left = 5
    Top = 228
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 365
    Top = 228
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 445
    Top = 228
    Width = 75
    Height = 25
    Caption = 'Aide'
    TabOrder = 4
  end
  object SrcList: TListBox
    Left = 8
    Top = 24
    Width = 193
    Height = 185
    ItemHeight = 13
    Items.Strings = (
      'El'#233'ment1'
      'El'#233'ment2'
      'El'#233'ment3'
      'El'#233'ment4'
      'El'#233'ment5')
    MultiSelect = True
    TabOrder = 0
  end
  object DstList: TListBox
    Left = 296
    Top = 24
    Width = 224
    Height = 185
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
  object EditOffset: TEdit
    Left = 88
    Top = 232
    Width = 49
    Height = 21
    Hint = 'Offset'
    TabOrder = 5
    Text = '$0000'
  end
end
