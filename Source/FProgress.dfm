object FormProgress: TFormProgress
  Left = 519
  Height = 163
  Top = 403
  Width = 130
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 163
  ClientWidth = 130
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object ButtonStop: TButton
    Left = 27
    Height = 25
    Top = 128
    Width = 75
    Cancel = True
    Caption = 'Stop'
    Default = True
    OnClick = ButtonStopClick
    TabOrder = 0
  end
  object Gauge1: TGauge
    Left = 12
    Height = 105
    Top = 12
    Width = 105
    BorderStyle = bsNone
    Color = clBtnFace
    DoubleBuffered = True
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Sans'
    ParentColor = False
    ParentFont = False
    Kind = gkPie
    Progress = 6
    ShowTextInverted = True
  end
end
