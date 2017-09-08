object FormDBaseInfo: TFormDBaseInfo
  Left = 660
  Height = 164
  Top = 303
  Width = 335
  HelpContext = 280
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Database Information'
  ClientHeight = 164
  ClientWidth = 335
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Sans'
  Position = poDefault
  LCLVersion = '1.8.0.3'
  Scaled = False
  object Label1: TLabel
    Left = 10
    Height = 14
    Top = 12
    Width = 65
    Caption = 'Database:'
    ParentColor = False
  end
  object Label2: TLabel
    Left = 10
    Height = 14
    Top = 32
    Width = 28
    Caption = 'Size:'
    ParentColor = False
  end
  object Label3: TLabel
    Left = 10
    Height = 14
    Top = 72
    Width = 173
    Caption = 'Number of deleted records:'
    ParentColor = False
  end
  object Label4: TLabel
    Left = 10
    Height = 14
    Top = 52
    Width = 153
    Caption = 'Total number of records:'
    ParentColor = False
  end
  object Label5: TLabel
    Left = 10
    Height = 14
    Top = 92
    Width = 179
    Caption = 'Compression recommended:'
    ParentColor = False
  end
  object Label6: TLabel
    Left = 10
    Height = 14
    Top = 112
    Width = 63
    Caption = 'Read-only:'
    ParentColor = False
  end
  object LabelDBaseName: TLabel
    Left = 208
    Height = 13
    Top = 12
    Width = 7
    Alignment = taRightJustify
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelSize: TLabel
    Left = 208
    Height = 13
    Top = 32
    Width = 7
    Alignment = taRightJustify
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelRecords: TLabel
    Left = 208
    Height = 13
    Top = 52
    Width = 7
    Alignment = taRightJustify
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelRecordsDeleted: TLabel
    Left = 208
    Height = 13
    Top = 72
    Width = 7
    Alignment = taRightJustify
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelReindex: TLabel
    Left = 208
    Height = 13
    Top = 92
    Width = 7
    Alignment = taRightJustify
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelReadOnly: TLabel
    Left = 208
    Height = 13
    Top = 112
    Width = 7
    Alignment = taRightJustify
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object ButtonOK: TButton
    Left = 140
    Height = 25
    Top = 136
    Width = 75
    Cancel = True
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 0
  end
end
