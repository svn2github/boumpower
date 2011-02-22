object BootloaderDlg: TBootloaderDlg
  Left = 301
  Top = 264
  BorderStyle = bsDialog
  Caption = 'Relocated Bootloader Wizard'
  ClientHeight = 379
  ClientWidth = 488
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 167
    Top = 348
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 247
    Top = 348
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 1
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 64
    Width = 473
    Height = 121
    Caption = 'Booloader'
    TabOrder = 2
  end
  object MCHPUSB: TRadioButton
    Left = 32
    Top = 80
    Width = 337
    Height = 17
    Caption = 'Microchip MCHPUSB Bootloader [0x0000 .. 0x07FF]'
    TabOrder = 3
    OnClick = BootloaderCheckClick
  end
  object HID: TRadioButton
    Left = 32
    Top = 104
    Width = 337
    Height = 17
    Caption = 'Microchip HID Bootloader [0x0000 .. 0x0FFF]'
    TabOrder = 4
    OnClick = BootloaderCheckClick
  end
  object VASCO: TRadioButton
    Left = 32
    Top = 128
    Width = 337
    Height = 17
    Caption = 'VASCO Bootloader [0x0000 .. 0x1FFF]'
    TabOrder = 5
    OnClick = BootloaderCheckClick
  end
  object FileSelectCB: TComboBox
    Left = 32
    Top = 152
    Width = 433
    Height = 21
    ItemHeight = 13
    TabOrder = 6
    OnChange = SelectCBChange
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 192
    Width = 473
    Height = 65
    Caption = 'Application'
    TabOrder = 7
  end
  object AppSelectCB: TComboBox
    Left = 32
    Top = 216
    Width = 433
    Height = 21
    Enabled = False
    ItemHeight = 13
    TabOrder = 8
    OnChange = SelectCBChange
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 272
    Width = 473
    Height = 65
    Caption = 'Configuration bits'
    TabOrder = 9
  end
  object ConfigSelectCB: TComboBox
    Left = 32
    Top = 296
    Width = 433
    Height = 21
    Enabled = False
    ItemHeight = 13
    TabOrder = 10
    OnChange = SelectCBChange
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 473
    Height = 49
    Caption = 'Relocator'
    TabOrder = 11
    object RelocSelectCB: TComboBox
      Left = 24
      Top = 20
      Width = 433
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = SelectCBChange
    end
  end
  object OpenDialog: TJvOpenDialog
    DefaultExt = 'hex'
    Filter = 'Fichier hex (*.hex)|*.hex'
    Height = 0
    Width = 0
    Left = 432
    Top = 80
  end
end
