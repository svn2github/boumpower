object MetadataDlg: TMetadataDlg
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Edit CNC metadata'
  ClientHeight = 432
  ClientWidth = 718
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    718
    432)
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 188
    Top = 398
    Width = 90
    Height = 25
    Anchors = [akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 361
    Top = 398
    Width = 90
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Annuler'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 718
    Height = 374
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 257
      Top = 0
      Height = 374
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 257
      Height = 374
      Align = alLeft
      Caption = 'Panel2'
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 45
        Height = 13
        Caption = 'Metadata'
      end
      object MetaEditor: TValueListEditor
        Left = 1
        Top = 24
        Width = 255
        Height = 349
        Align = alBottom
        TabOrder = 0
        ColWidths = (
          100
          149)
      end
    end
    object Panel3: TPanel
      Left = 260
      Top = 0
      Width = 458
      Height = 374
      Align = alClient
      Caption = 'Panel3'
      TabOrder = 1
      object Image1: TImage
        Left = 8
        Top = 8
        Width = 313
        Height = 161
      end
      object TrackBar1: TTrackBar
        Left = 352
        Top = 8
        Width = 25
        Height = 161
        Max = 255
        Orientation = trVertical
        TabOrder = 0
      end
    end
  end
end
