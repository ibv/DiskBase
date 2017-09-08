object FormScanArchive: TFormScanArchive
  Left = 383
  Top = 290
  HelpContext = 120
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Archive Found'
  ClientHeight = 156
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMsg: TLabel
    Left = 16
    Top = 16
    Width = 333
    Height = 69
    AutoSize = False
    ShowAccelChar = False
    WordWrap = True
  end
  object ButtonYes: TButton
    Left = 90
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Yes'
    Default = True
    TabOrder = 0
    OnClick = ButtonYesClick
  end
  object ButtonNo: TButton
    Left = 90
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'No'
    TabOrder = 2
    OnClick = ButtonNoClick
  end
  object ButtonYesAll: TButton
    Left = 174
    Top = 88
    Width = 100
    Height = 25
    Caption = 'Yes for all'
    TabOrder = 1
    OnClick = ButtonYesAllClick
  end
  object ButtonNoAll: TButton
    Left = 174
    Top = 120
    Width = 100
    Height = 25
    Caption = 'No for all'
    TabOrder = 3
    OnClick = ButtonNoAllClick
  end
end
