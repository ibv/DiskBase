object FormMoreOptions: TFormMoreOptions
  Left = 338
  Height = 333
  Top = 185
  Width = 482
  HelpContext = 230
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'More Options For Search'
  ClientHeight = 333
  ClientWidth = 482
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.3'
  object Label4: TLabel
    Left = 13
    Height = 13
    Top = 10
    Width = 75
    Caption = 'Exclude files:'
    ParentColor = False
  end
  object Label7: TLabel
    Left = 13
    Height = 13
    Top = 207
    Width = 128
    Caption = 'Sort found records by:'
    ParentColor = False
  end
  object Label8: TLabel
    Left = 13
    Height = 13
    Top = 164
    Width = 129
    Caption = 'Search only in folders:'
    ParentColor = False
  end
  object ButtonOK: TButton
    Left = 308
    Height = 25
    Top = 299
    Width = 75
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 10
  end
  object ButtonCancel: TButton
    Left = 392
    Height = 25
    Top = 299
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    OnClick = ButtonCancelClick
    TabOrder = 11
  end
  object GroupBox1: TGroupBox
    Left = 16
    Height = 100
    Top = 60
    Width = 241
    Caption = 'File date must be:'
    ClientHeight = 71
    ClientWidth = 237
    TabOrder = 1
    object Label1: TLabel
      Left = 164
      Height = 13
      Top = -4
      Width = 7
      Caption = 'd'
      ParentColor = False
    end
    object Label2: TLabel
      Left = 188
      Height = 13
      Top = -4
      Width = 11
      Caption = 'm'
      ParentColor = False
    end
    object Label3: TLabel
      Left = 216
      Height = 13
      Top = -4
      Width = 6
      Caption = 'y'
      ParentColor = False
    end
    object SpinButtonD1: TSpinEdit
      Left = 160
      Height = 19
      Top = 11
      Width = 20
      TabOrder = 2
    end
    object SpinButtonM1: TSpinEdit
      Left = 184
      Height = 19
      Top = 11
      Width = 20
      TabOrder = 3
    end
    object SpinButtonY1: TSpinEdit
      Left = 208
      Height = 19
      Top = 11
      Width = 20
      TabOrder = 4
    end
    object SpinButtonD2: TSpinEdit
      Left = 160
      Height = 19
      Top = 43
      Width = 20
      TabOrder = 7
    end
    object SpinButtonM2: TSpinEdit
      Left = 184
      Height = 19
      Top = 43
      Width = 20
      TabOrder = 8
    end
    object SpinButtonY2: TSpinEdit
      Left = 208
      Height = 19
      Top = 43
      Width = 20
      TabOrder = 9
    end
    object CheckBoxDateFrom: TCheckBox
      Left = 16
      Height = 26
      Top = 11
      Width = 60
      Caption = 'From:'
      TabOrder = 0
    end
    object CheckBoxDateTo: TCheckBox
      Left = 16
      Height = 26
      Top = 43
      Width = 44
      Caption = 'To:'
      TabOrder = 5
    end
    object EditDateTo: TEdit
      Left = 76
      Height = 19
      Top = 43
      Width = 77
      TabOrder = 6
    end
    object EditDateFrom: TEdit
      Left = 76
      Height = 19
      Top = 11
      Width = 77
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 264
    Height = 95
    Top = 60
    Width = 201
    Caption = 'File size must be:'
    ClientHeight = 66
    ClientWidth = 197
    TabOrder = 2
    object Label5: TLabel
      Left = 168
      Height = 13
      Top = 14
      Width = 14
      Caption = 'kB'
      ParentColor = False
    end
    object Label6: TLabel
      Left = 168
      Height = 13
      Top = 47
      Width = 14
      Caption = 'kB'
      ParentColor = False
    end
    object CheckBoxSizeFrom: TCheckBox
      Left = 16
      Height = 26
      Top = 11
      Width = 60
      Caption = 'From:'
      TabOrder = 0
    end
    object CheckBoxSizeTo: TCheckBox
      Left = 16
      Height = 26
      Top = 43
      Width = 44
      Caption = 'To:'
      TabOrder = 1
    end
    object SpinEditSizeFrom: TSpinEdit
      Left = 76
      Height = 19
      Top = 11
      Width = 81
      Increment = 100
      MaxValue = 0
      OnChange = SpinEditSizeFromChange
      TabOrder = 2
    end
    object SpinEditSizeTo: TSpinEdit
      Left = 76
      Height = 19
      Top = 43
      Width = 81
      Increment = 100
      MaxValue = 0
      OnChange = SpinEditSizeToChange
      TabOrder = 3
    end
  end
  object ComboBoxExclude: TComboBox
    Left = 16
    Height = 20
    Top = 28
    Width = 449
    ItemHeight = 0
    TabOrder = 0
  end
  object ButtonHelp: TButton
    Left = 392
    Height = 25
    Top = 267
    Width = 75
    Caption = 'Help'
    OnClick = ButtonHelpClick
    TabOrder = 9
  end
  object RadioGroupKey1: TRadioGroup
    Left = 16
    Height = 101
    Top = 223
    Width = 29
    AutoFill = True
    Caption = '1'
    ChildSizing.LeftRightSpacing = 6
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 72
    ClientWidth = 25
    ItemIndex = 1
    Items.Strings = (
      ''
      ''
      ''
      ''
      ''
    )
    TabOrder = 4
  end
  object RadioGroupKey2: TRadioGroup
    Left = 48
    Height = 101
    Top = 223
    Width = 29
    AutoFill = True
    Caption = '2'
    ChildSizing.LeftRightSpacing = 6
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 72
    ClientWidth = 25
    ItemIndex = 4
    Items.Strings = (
      ''
      ''
      ''
      ''
      ''
    )
    TabOrder = 5
  end
  object RadioGroupKey3: TRadioGroup
    Left = 80
    Height = 101
    Top = 223
    Width = 153
    AutoFill = True
    Caption = '3'
    ChildSizing.LeftRightSpacing = 6
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 72
    ClientWidth = 149
    ItemIndex = 2
    Items.Strings = (
      'Name+Extension'
      'Extension+Name'
      'Date and time'
      'Size'
      'Disk+Folder'
    )
    TabOrder = 6
  end
  object ComboBoxDirMask: TComboBox
    Left = 16
    Height = 20
    Top = 177
    Width = 449
    ItemHeight = 0
    TabOrder = 3
  end
  object CheckBoxSearchAsAnsi: TCheckBox
    Left = 256
    Height = 26
    Top = 218
    Width = 156
    Caption = 'Search as ANSI text'
    Checked = True
    OnClick = CheckBoxSearchAsAnsiClick
    State = cbChecked
    TabOrder = 7
  end
  object CheckBoxSearchAsOem: TCheckBox
    Left = 256
    Height = 26
    Top = 236
    Width = 154
    Caption = 'Search as OEM text'
    OnClick = CheckBoxSearchAsOemClick
    TabOrder = 8
  end
end
