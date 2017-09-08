object FormSearchEmpty: TFormSearchEmpty
  Left = 595
  Height = 150
  Top = 260
  Width = 114
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Searching...'
  ClientHeight = 150
  ClientWidth = 114
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Sans'
  OnActivate = FormActivate
  OnCreate = FormCreate
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object Gauge1: TPanel
    Left = 17
    Height = 80
    Top = 14
    Width = 80
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object ButtonStop: TButton
    Left = 20
    Height = 25
    Top = 112
    Width = 75
    Cancel = True
    Caption = 'Stop'
    Default = True
    OnClick = ButtonStopClick
    TabOrder = 0
  end
end
