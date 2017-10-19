object FormDiskPrint: TFormDiskPrint
  Left = 406
  Height = 115
  Top = 177
  Width = 337
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Disk Print'
  ClientHeight = 115
  ClientWidth = 337
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object LabelSearched: TLabel
    Left = 164
    Height = 16
    Top = 40
    Width = 77
    AutoSize = False
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 12
    Height = 13
    Top = 40
    Width = 93
    Caption = 'Disks/files read:'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
  end
  object LabelWhat: TLabel
    Left = 12
    Height = 16
    Top = 12
    Width = 317
    Alignment = taCenter
    AutoSize = False
    ParentColor = False
  end
  object ButtonCancel: TButton
    Left = 131
    Height = 25
    Top = 80
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    Default = True
    OnClick = ButtonCancelClick
    TabOrder = 0
  end
  object Gauge: TGauge
    Left = 248
    Height = 76
    Top = 32
    Width = 76
    BorderStyle = bsNone
    Color = clBtnFace
    DoubleBuffered = True
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Sans'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Kind = gkPie
    ShowTextInverted = True
  end
  object PrintDialog: TPrintDialog
    FromPage = 1
    MinPage = 1
    MaxPage = 1000
    Options = [poPageNums, poWarning, poHelp]
    ToPage = 1
    left = 32
    top = 68
  end
end
