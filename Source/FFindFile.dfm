object FormSearchName: TFormSearchName
  Left = 396
  Height = 94
  Top = 432
  Width = 466
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Searching'
  ClientHeight = 94
  ClientWidth = 466
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Sans'
  OnActivate = FormActivate
  OnCreate = FormCreate
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object Label1: TLabel
    Left = 16
    Height = 14
    Top = 40
    Width = 129
    Caption = 'Disks/files searched:'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    ParentColor = False
    ParentFont = False
  end
  object LabelSearched: TLabel
    Left = 188
    Height = 13
    Top = 40
    Width = 6
    Caption = 'x'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
  end
  object Label2: TLabel
    Left = 284
    Height = 13
    Top = 40
    Width = 65
    Caption = 'Files found:'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
  end
  object LabelFound: TLabel
    Left = 412
    Height = 13
    Top = 40
    Width = 6
    Caption = 'x'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
  end
  object ButtonStop: TButton
    Left = 196
    Height = 25
    Top = 62
    Width = 75
    Cancel = True
    Caption = 'Stop'
    Default = True
    OnClick = ButtonStopClick
    TabOrder = 0
  end
  object Gauge1: TCDProgressBar
    Left = 37
    Height = 21
    Top = 12
    Width = 393
    BarShowText = False
    DrawStyle = dsDefault
    Max = 100
    Orientation = pbHorizontal
    Position = 0
    Smooth = False
    Style = pbstNormal
  end
end
