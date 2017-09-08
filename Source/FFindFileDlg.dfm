object FormSearchFileDlg: TFormSearchFileDlg
  Left = 347
  Height = 295
  Top = 228
  Width = 449
  HelpContext = 230
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 295
  ClientWidth = 449
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Sans'
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object Label1: TLabel
    Left = 19
    Height = 14
    Top = 8
    Width = 57
    Caption = 'Find text:'
    ParentColor = False
  end
  object Label3: TLabel
    Left = 28
    Height = 14
    Top = 192
    Width = 92
    Caption = 'Search only in:'
    ParentColor = False
  end
  object RadioGroupSearch: TRadioGroup
    Left = 19
    Height = 91
    Top = 54
    Width = 197
    AutoFill = True
    Caption = 'Go through'
    ChildSizing.LeftRightSpacing = 6
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 61
    ClientWidth = 193
    ItemIndex = 0
    Items.Strings = (
      'whole database'
      'current disk'
      'selected disks'
      'not selected disks'
    )
    TabOrder = 1
  end
  object GroupBoxMaxLines: TGroupBox
    Left = 19
    Height = 68
    Top = 215
    Width = 165
    Caption = 'Stop search after'
    ClientHeight = 38
    ClientWidth = 161
    TabOrder = 8
    object Label4: TLabel
      Left = 35
      Height = 14
      Top = 24
      Width = 88
      Caption = 'records found'
      ParentColor = False
    end
    object SpinEditMaxRecs: TSpinEdit
      Left = 40
      Height = 20
      Top = -3
      Width = 73
      Increment = 100
      MaxValue = 16000
      MinValue = 1
      TabOrder = 0
      Value = 1000
    end
  end
  object ButtonOK: TButton
    Left = 280
    Height = 25
    Top = 258
    Width = 75
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 12
  end
  object ButtonCancel: TButton
    Left = 357
    Height = 25
    Top = 258
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    OnClick = ButtonCancelClick
    TabOrder = 13
  end
  object CheckBoxCaseSensitive: TCheckBox
    Left = 28
    Height = 26
    Top = 163
    Width = 195
    Caption = 'Case sensitive comparison'
    TabOrder = 5
  end
  object ComboBoxMask: TComboBox
    Left = 19
    Height = 20
    Top = 24
    Width = 413
    ItemHeight = 0
    OnChange = ComboBoxMaskChange
    TabOrder = 0
  end
  object ButtonOptions: TButton
    Left = 336
    Height = 25
    Top = 217
    Width = 96
    Caption = 'More options...'
    OnClick = ButtonOptionsClick
    TabOrder = 10
  end
  object ButtonHelp: TButton
    Left = 195
    Height = 25
    Top = 258
    Width = 75
    Caption = 'Help'
    OnClick = ButtonHelpClick
    TabOrder = 11
  end
  object GroupBoxWhere: TGroupBox
    Left = 232
    Height = 91
    Top = 54
    Width = 200
    Caption = 'Find text in:'
    ClientHeight = 61
    ClientWidth = 196
    TabOrder = 2
    object CheckBoxFileNames: TCheckBox
      Left = 12
      Height = 26
      Top = 7
      Width = 96
      Caption = 'File names'
      TabOrder = 1
    end
    object CheckBoxFolderNames: TCheckBox
      Left = 12
      Height = 26
      Top = 23
      Width = 115
      Caption = 'Folder names'
      TabOrder = 2
    end
    object CheckBoxDescriptions: TCheckBox
      Left = 12
      Height = 26
      Top = 39
      Width = 107
      Caption = 'Descriptions'
      TabOrder = 3
    end
    object CheckBoxDiskNames: TCheckBox
      Left = 12
      Height = 26
      Top = -9
      Width = 103
      Caption = 'Disk names'
      TabOrder = 0
    end
  end
  object CheckBoxAddWildcards: TCheckBox
    Left = 28
    Height = 26
    Top = 145
    Width = 215
    Caption = 'Search also as parts of words'
    TabOrder = 3
  end
  object CheckBoxPhrase: TCheckBox
    Left = 264
    Height = 26
    Top = 145
    Width = 139
    Caption = 'Search as phrase'
    TabOrder = 4
  end
  object CheckBoxUseMoreOptions: TCheckBox
    Left = 192
    Height = 26
    Top = 220
    Width = 142
    Caption = 'Use more options'
    TabOrder = 9
  end
  object ComboBoxScanDirLevel: TComboBox
    Left = 127
    Height = 20
    Top = 189
    Width = 249
    ItemHeight = 0
    Items.Strings = (
      '(unlimited)'
      'the root folder'
      'subfolders down to 1. level'
      'subfolders down to 2. level'
      'subfolders down to 3. level'
      'subfolders down to 4. level'
      'subfolders down to 5. level'
      'subfolders down to 6. level'
      'subfolders down to 7. level'
      'subfolders down to 8. level'
      'subfolders down to 9. level'
    )
    TabOrder = 7
  end
  object CheckBoxStrictMask: TCheckBox
    Left = 264
    Height = 26
    Top = 163
    Width = 98
    Caption = 'Strict mask'
    TabOrder = 6
  end
end
