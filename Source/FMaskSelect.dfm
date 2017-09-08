object FormMaskSelect: TFormMaskSelect
  Left = 423
  Height = 99
  Top = 305
  Width = 311
  HelpContext = 140
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Selection'
  ClientHeight = 99
  ClientWidth = 311
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Sans'
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object LabelMask: TLabel
    Left = 12
    Height = 14
    Top = 12
    Width = 118
    Caption = 'Mask for selection:'
    ParentColor = False
  end
  object ButtonOK: TButton
    Left = 144
    Height = 25
    Top = 64
    Width = 75
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 224
    Height = 25
    Top = 64
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    OnClick = ButtonCancelClick
    TabOrder = 2
  end
  object ButtonHelp: TButton
    Left = 12
    Height = 25
    Top = 64
    Width = 75
    HelpContext = 220
    Caption = 'Help'
    OnClick = ButtonHelpClick
    TabOrder = 0
  end
  object ComboBoxMaskDisks: TComboBox
    Left = 24
    Height = 20
    Top = 31
    Width = 277
    ItemHeight = 0
    TabOrder = 3
    Text = '*'
    Visible = False
  end
  object ComboBoxMaskFiles: TComboBox
    Left = 24
    Height = 20
    Top = 31
    Width = 277
    ItemHeight = 0
    TabOrder = 4
    Text = '*'
    Visible = False
  end
end
