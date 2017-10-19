object FormReindex: TFormReindex
  Left = 333
  Height = 96
  Top = 339
  Width = 298
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 96
  ClientWidth = 298
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object LabelInfo: TLabel
    Left = 12
    Height = 16
    Top = 16
    Width = 193
    AutoSize = False
    Caption = 'Info'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
  end
  object ButtonCancel: TButton
    Left = 111
    Height = 25
    Top = 60
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    Default = True
    OnClick = ButtonCancelClick
    TabOrder = 0
  end
  object Gauge: TGauge
    Left = 207
    Height = 81
    Top = 8
    Width = 85
    BorderStyle = bsNone
    Color = clBtnFace
    DoubleBuffered = True
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Kind = gkPie
    ShowTextInverted = True
  end
end
