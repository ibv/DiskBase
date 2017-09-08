object FormDiskPrintSelect: TFormDiskPrintSelect
  Left = 396
  Height = 120
  Top = 245
  Width = 301
  HelpContext = 320
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Selection of Print'
  ClientHeight = 120
  ClientWidth = 301
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.3'
  object LabelWhat: TLabel
    Left = 12
    Height = 13
    Top = 8
    Width = 30
    Caption = 'Print:'
    ParentColor = False
  end
  object RadioButtonActDiskDir: TRadioButton
    Left = 20
    Height = 26
    Top = 28
    Width = 181
    Caption = 'This disk - current folder'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object RadioButtonActDiskWhole: TRadioButton
    Left = 20
    Height = 26
    Top = 47
    Width = 131
    Caption = 'This disk - whole'
    TabOrder = 1
  end
  object RadioButtonSelectedDisks: TRadioButton
    Left = 20
    Height = 26
    Top = 66
    Width = 162
    Caption = 'All the selected disks'
    TabOrder = 2
  end
  object ButtonOK: TButton
    Left = 73
    Height = 25
    Top = 88
    Width = 75
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 3
  end
  object ButtonCancel: TButton
    Left = 153
    Height = 25
    Top = 88
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    OnClick = ButtonCancelClick
    TabOrder = 4
  end
end
