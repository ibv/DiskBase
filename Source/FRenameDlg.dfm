object FormRenameDlg: TFormRenameDlg
  Left = 368
  Height = 107
  Top = 388
  Width = 437
  HelpContext = 110
  ActiveControl = Edit1
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'xxx'
  ClientHeight = 107
  ClientWidth = 437
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Sans'
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object Label1: TLabel
    Left = 8
    Height = 15
    Top = 24
    Width = 101
    Caption = 'New disk name:'
    ParentColor = False
  end
  object Label2: TLabel
    Left = 8
    Height = 15
    Top = 8
    Width = 21
    Caption = 'xxx'
    ParentColor = False
  end
  object ButtonOK: TButton
    Left = 268
    Height = 25
    Top = 72
    Width = 75
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 350
    Height = 25
    Top = 72
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    OnClick = ButtonCancelClick
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 20
    Height = 21
    Top = 44
    Width = 405
    TabOrder = 0
  end
end
