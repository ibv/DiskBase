object FormAskForLabel: TFormAskForLabel
  Left = 298
  Height = 105
  Top = 223
  Width = 404
  HelpContext = 110
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Disk Name'
  ClientHeight = 105
  ClientWidth = 404
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.3'
  object Label1: TLabel
    Left = 12
    Height = 13
    Top = 8
    Width = 204
    Caption = 'Save disk to database under name:'
    ParentColor = False
  end
  object LabelGenerated: TLabel
    Left = 12
    Height = 16
    Top = 51
    Width = 373
    AutoSize = False
    Caption = 'Generated name: '
    ParentColor = False
    Visible = False
  end
  object EditDiskName: TEdit
    Left = 20
    Height = 19
    Top = 25
    Width = 369
    MaxLength = 250
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 236
    Height = 25
    Top = 71
    Width = 75
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 315
    Height = 25
    Top = 71
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    OnClick = ButtonCancelClick
    TabOrder = 3
  end
  object ButtonUseGenerated: TButton
    Left = 19
    Height = 25
    Top = 71
    Width = 100
    Caption = 'Use generated'
    OnClick = ButtonUseGeneratedClick
    TabOrder = 1
    Visible = False
  end
end
