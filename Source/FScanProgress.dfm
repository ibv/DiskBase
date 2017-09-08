object FormScanProgress: TFormScanProgress
  Left = 360
  Height = 101
  Top = 423
  Width = 523
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Scanning Disk'
  ClientHeight = 101
  ClientWidth = 523
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.3'
  object LabelInfo: TLabel
    Left = 16
    Height = 37
    Top = 34
    Width = 409
    AutoSize = False
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
    ShowAccelChar = False
    WordWrap = True
  end
  object Gauge: TPanel
    Left = 436
    Height = 75
    Top = 16
    Width = 75
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    Visible = False
  end
  object LabelDoing: TLabel
    Left = 16
    Height = 16
    Top = 12
    Width = 409
    AutoSize = False
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    ShowAccelChar = False
    WordWrap = True
  end
  object ButtonCancel: TButton
    Left = 224
    Height = 25
    Top = 72
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    Default = True
    OnClick = ButtonCancelClick
    TabOrder = 0
  end
end
