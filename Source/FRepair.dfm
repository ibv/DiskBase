object FormRepair: TFormRepair
  Left = 325
  Height = 87
  Top = 299
  Width = 443
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Repairing Database'
  ClientHeight = 87
  ClientWidth = 443
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.3'
  object Gauge: TPanel
    Left = 363
    Height = 75
    Top = 8
    Width = 75
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object LabelInfo: TLabel
    Left = 12
    Height = 16
    Top = 16
    Width = 333
    AutoSize = False
    Caption = 'LabelInfo'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
  end
  object ButtonCancel: TButton
    Left = 184
    Height = 25
    Top = 52
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    Default = True
    OnClick = ButtonCancelClick
    TabOrder = 0
  end
end
