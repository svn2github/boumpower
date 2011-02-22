object MergeDlg: TMergeDlg
  Left = 418
  Top = 307
  BorderStyle = bsDialog
  Caption = 'Intel HEX : Merge'
  ClientHeight = 279
  ClientWidth = 639
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object DstLabel: TLabel
    Left = 296
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
  object ExcludeBtn: TSpeedButton
    Left = 208
    Top = 96
    Width = 81
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
    Left = 469
    Top = 228
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 3
  end
  object HelpBtn: TButton
    Left = 549
    Top = 228
    Width = 75
    Height = 25
    Caption = 'Aide'
    TabOrder = 4
  end
  object SrcList: TListBox
    Left = 8
    Top = 32
    Width = 193
    Height = 177
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
    Top = 32
    Width = 337
    Height = 177
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
  object SourceBox: TComboBox
    Left = 8
    Top = 8
    Width = 193
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = SourceBoxChange
  end
end
