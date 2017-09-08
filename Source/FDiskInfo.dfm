object FormDiskInfo: TFormDiskInfo
  Left = 451
  Height = 219
  Top = 248
  Width = 437
  HelpContext = 280
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Disk Information'
  ClientHeight = 219
  ClientWidth = 437
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Sans'
  Position = poDefault
  LCLVersion = '1.8.0.4'
  object Label1: TLabel
    Left = 12
    Height = 14
    Top = 196
    Width = 79
    Caption = 'Last update:'
    ParentColor = False
  end
  object Label2: TLabel
    Left = 12
    Height = 14
    Top = 64
    Width = 80
    Caption = 'Total folders:'
    ParentColor = False
  end
  object Label3: TLabel
    Left = 12
    Height = 14
    Top = 80
    Width = 62
    Caption = 'Total files:'
    ParentColor = False
  end
  object Label4: TLabel
    Left = 12
    Height = 14
    Top = 96
    Width = 89
    Caption = 'Total archives:'
    ParentColor = False
  end
  object Label7: TLabel
    Left = 204
    Height = 14
    Top = 64
    Width = 157
    Caption = 'Sum of physical file sizes:'
    ParentColor = False
  end
  object Label10: TLabel
    Left = 204
    Height = 14
    Top = 80
    Width = 57
    Caption = 'Disk size:'
    ParentColor = False
  end
  object Label11: TLabel
    Left = 204
    Height = 14
    Top = 96
    Width = 86
    Caption = 'Disk free size:'
    ParentColor = False
  end
  object Label0: TLabel
    Left = 12
    Height = 14
    Top = 8
    Width = 30
    Caption = 'Disk:'
    ParentColor = False
  end
  object LabelScanDate: TLabel
    Left = 94
    Height = 13
    Top = 196
    Width = 7
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelDirs: TLabel
    Left = 170
    Height = 13
    Top = 64
    Width = 7
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelFiles: TLabel
    Left = 170
    Height = 13
    Top = 80
    Width = 7
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelArchives: TLabel
    Left = 170
    Height = 13
    Top = 96
    Width = 7
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelDiskName: TLabel
    Left = 108
    Height = 16
    Top = 8
    Width = 309
    AutoSize = False
    Caption = 'x'
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelDataSize: TLabel
    Left = 422
    Height = 13
    Top = 64
    Width = 7
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelDiskSize: TLabel
    Left = 422
    Height = 13
    Top = 80
    Width = 7
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelDiskFree: TLabel
    Left = 422
    Height = 13
    Top = 96
    Width = 7
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 12
    Height = 14
    Top = 24
    Width = 83
    Caption = 'Volume label:'
    ParentColor = False
  end
  object Label6: TLabel
    Left = 12
    Height = 14
    Top = 40
    Width = 84
    Caption = 'Original path:'
    ParentColor = False
  end
  object LabelVolumeLabel: TLabel
    Left = 108
    Height = 16
    Top = 24
    Width = 317
    AutoSize = False
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelOriginalFolder: TLabel
    Left = 108
    Height = 16
    Top = 40
    Width = 317
    AutoSize = False
    Caption = 'x'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label8: TLabel
    Left = 12
    Height = 14
    Top = 120
    Width = 133
    Caption = 'Total files in archives:'
    ParentColor = False
  end
  object Label9: TLabel
    Left = 12
    Height = 14
    Top = 136
    Width = 151
    Caption = 'Total folders in archives:'
    ParentColor = False
  end
  object Label12: TLabel
    Left = 12
    Height = 14
    Top = 152
    Width = 174
    Caption = 'Sum of file sizes in archives:'
    ParentColor = False
  end
  object Label13: TLabel
    Left = 12
    Height = 14
    Top = 168
    Width = 93
    Caption = 'Total data size:'
    ParentColor = False
  end
  object LabelArcFiles: TLabel
    Left = 202
    Height = 13
    Top = 120
    Width = 86
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'LabelArcFiles'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelArcDirs: TLabel
    Left = 208
    Height = 13
    Top = 136
    Width = 80
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'LabelArcDirs'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelArcSize: TLabel
    Left = 206
    Height = 13
    Top = 152
    Width = 82
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'LabelArcSize'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object LabelTotSize: TLabel
    Left = 206
    Height = 13
    Top = 168
    Width = 82
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'LabelTotSize'
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object ButtonOK: TButton
    Left = 350
    Height = 25
    Top = 188
    Width = 75
    Cancel = True
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 0
  end
  object ButtonHelp: TButton
    Left = 271
    Height = 25
    Top = 188
    Width = 75
    Caption = 'Help'
    OnClick = ButtonHelpClick
    TabOrder = 1
  end
end
