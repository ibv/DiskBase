object FormSettings: TFormSettings
  Left = 569
  Height = 327
  Top = 281
  Width = 486
  HelpContext = 290
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Program Settings'
  ClientHeight = 327
  ClientWidth = 486
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Sans'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poDefault
  LCLVersion = '1.8.0.3'
  object TabbedNotebook: TPageControl
    Left = 1
    Height = 284
    Top = 0
    Width = 485
    ActivePage = Page1
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Sans'
    ParentFont = False
    TabIndex = 0
    TabOrder = 0
    object Page1: TTabSheet
      Caption = '&Display'
      ClientHeight = 247
      ClientWidth = 477
      object RadioGroupSortCrit: TRadioGroup
        Left = 12
        Height = 85
        Top = 68
        Width = 205
        AutoFill = True
        Caption = 'Sort files by'
        ChildSizing.LeftRightSpacing = 6
        ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
        ChildSizing.EnlargeVertical = crsHomogenousChildResize
        ChildSizing.ShrinkHorizontal = crsScaleChilds
        ChildSizing.ShrinkVertical = crsScaleChilds
        ChildSizing.Layout = cclLeftToRightThenTopToBottom
        ChildSizing.ControlsPerLine = 1
        ClientHeight = 54
        ClientWidth = 201
        ItemIndex = 0
        Items.Strings = (
          'name'
          'extension'
          'date and time'
          'size'
        )
        TabOrder = 1
      end
      object GroupBoxFileDisplay: TGroupBox
        Left = 224
        Height = 115
        Top = 4
        Width = 209
        Caption = 'Display files'
        ClientHeight = 84
        ClientWidth = 205
        TabOrder = 2
        object RadioButtonBrief: TRadioButton
          Left = 8
          Height = 26
          Top = -7
          Width = 177
          Caption = 'Names only in columns'
          OnClick = RadioButtonBriefClick
          TabOrder = 0
        end
        object RadioButtonDetailed: TRadioButton
          Left = 8
          Height = 26
          Top = 10
          Width = 105
          Caption = 'With details:'
          Checked = True
          OnClick = RadioButtonDetailedClick
          TabOrder = 1
          TabStop = True
        end
        object CheckBoxShowSize: TCheckBox
          Left = 44
          Height = 26
          Top = 28
          Width = 55
          Caption = 'Size'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 2
        end
        object CheckBoxShowTime: TCheckBox
          Left = 44
          Height = 26
          Top = 45
          Width = 120
          Caption = 'Date and time'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 3
        end
        object CheckBoxShowDescr: TCheckBox
          Left = 44
          Height = 26
          Top = 62
          Width = 100
          Caption = 'Description'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 4
        end
      end
      object RadioGroupSizeDisplay: TRadioGroup
        Left = 12
        Height = 62
        Top = 157
        Width = 205
        AutoFill = True
        Caption = 'File size'
        ChildSizing.LeftRightSpacing = 6
        ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
        ChildSizing.EnlargeVertical = crsHomogenousChildResize
        ChildSizing.ShrinkHorizontal = crsScaleChilds
        ChildSizing.ShrinkVertical = crsScaleChilds
        ChildSizing.Layout = cclLeftToRightThenTopToBottom
        ChildSizing.ControlsPerLine = 1
        ClientHeight = 31
        ClientWidth = 201
        Enabled = False
        ItemIndex = 1
        Items.Strings = (
          'display in bytes'
          'display in kB, MB'
        )
        TabOrder = 3
      end
      object CheckBoxShowIcons: TCheckBox
        Left = 232
        Height = 26
        Top = 125
        Width = 165
        Caption = 'Show icons by folders'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object GroupBoxTree: TGroupBox
        Left = 12
        Height = 59
        Top = 4
        Width = 205
        Caption = 'Tree'
        ClientHeight = 28
        ClientWidth = 201
        TabOrder = 0
        object CheckBoxExpandTree: TCheckBox
          Left = 12
          Height = 26
          Top = 9
          Width = 175
          Caption = 'Show always expanded'
          TabOrder = 0
        end
        object CheckBoxShowTree: TCheckBox
          Left = 12
          Height = 26
          Top = -7
          Width = 92
          Caption = 'Show tree'
          Checked = True
          OnClick = CheckBoxShowTreeClick
          State = cbChecked
          TabOrder = 1
        end
      end
      object CheckBoxShowSeconds: TCheckBox
        Left = 232
        Height = 26
        Top = 142
        Width = 145
        Caption = 'Time with seconds'
        TabOrder = 5
      end
      object CheckBoxWinDateFormat: TCheckBox
        Left = 232
        Height = 26
        Top = 159
        Width = 191
        Caption = 'Use Windows date format'
        TabOrder = 6
      end
      object CheckBoxOneKbIs1024: TCheckBox
        Left = 232
        Height = 26
        Top = 176
        Width = 206
        Caption = 'Calculate: 1 kB = 1024 byte'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object CheckBoxShowFileHints: TCheckBox
        Left = 232
        Height = 26
        Top = 193
        Width = 145
        Caption = 'Show hints by files'
        Checked = True
        State = cbChecked
        TabOrder = 8
      end
    end
    object Page2: TTabSheet
      Caption = '&Fonts'
      ClientHeight = 247
      ClientWidth = 477
      object LabelDiskFont: TLabel
        Left = 44
        Height = 15
        Top = 36
        Width = 21
        Caption = 'xxx'
        ParentColor = False
      end
      object Label12: TLabel
        Left = 20
        Height = 15
        Top = 16
        Width = 70
        Caption = 'Disk panel:'
        ParentColor = False
      end
      object LabelTreeFont: TLabel
        Left = 44
        Height = 15
        Top = 77
        Width = 21
        Caption = 'xxx'
        ParentColor = False
      end
      object LabelFileFont: TLabel
        Left = 44
        Height = 15
        Top = 118
        Width = 21
        Caption = 'xxx'
        ParentColor = False
      end
      object LabelFoundFont: TLabel
        Left = 44
        Height = 15
        Top = 159
        Width = 21
        Caption = 'xxx'
        ParentColor = False
      end
      object LabelDescFont: TLabel
        Left = 44
        Height = 15
        Top = 199
        Width = 21
        Caption = 'xxx'
        ParentColor = False
      end
      object Label8: TLabel
        Left = 20
        Height = 15
        Top = 57
        Width = 69
        Caption = 'Tree panel:'
        ParentColor = False
      end
      object Label9: TLabel
        Left = 20
        Height = 15
        Top = 98
        Width = 63
        Caption = 'File panel:'
        ParentColor = False
      end
      object Label10: TLabel
        Left = 20
        Height = 15
        Top = 139
        Width = 160
        Caption = 'Window with found items:'
        ParentColor = False
      end
      object Label11: TLabel
        Left = 20
        Height = 15
        Top = 179
        Width = 117
        Caption = 'Description editor:'
        ParentColor = False
      end
      object ButtonChangeDiskFont: TButton
        Left = 328
        Height = 25
        Top = 20
        Width = 75
        Caption = 'Change...'
        OnClick = ButtonChangeDiskFontClick
        TabOrder = 0
      end
      object ButtonChangeTreeFont: TButton
        Left = 328
        Height = 25
        Top = 61
        Width = 75
        Caption = 'Change...'
        OnClick = ButtonChangeTreeFontClick
        TabOrder = 1
      end
      object ButtonChangeFileFont: TButton
        Left = 328
        Height = 25
        Top = 102
        Width = 75
        Caption = 'Change...'
        OnClick = ButtonChangeFileFontClick
        TabOrder = 2
      end
      object ButtonChangeFoundFont: TButton
        Left = 328
        Height = 25
        Top = 143
        Width = 75
        Caption = 'Change...'
        OnClick = ButtonChangeFoundFontClick
        TabOrder = 3
      end
      object ButtonChangeDescFont: TButton
        Left = 328
        Height = 25
        Top = 183
        Width = 75
        Caption = 'Change...'
        OnClick = ButtonChangeDescFontClick
        TabOrder = 4
      end
    end
    object Page3: TTabSheet
      Caption = '&Print'
      ClientHeight = 247
      ClientWidth = 477
      object Label5: TLabel
        Left = 8
        Height = 15
        Top = 74
        Width = 134
        Caption = 'Header text for print:'
        ParentColor = False
      end
      object GroupBoxMargins: TGroupBox
        Left = 296
        Height = 129
        Top = 87
        Width = 141
        Caption = 'Page margins [mm]'
        ClientHeight = 112
        ClientWidth = 137
        TabOrder = 3
        object Label1: TLabel
          Left = 12
          Height = 15
          Top = 3
          Width = 25
          Caption = 'Top:'
          ParentColor = False
        end
        object Label2: TLabel
          Left = 12
          Height = 15
          Top = 28
          Width = 52
          Caption = 'Bottom:'
          ParentColor = False
        end
        object Label3: TLabel
          Left = 12
          Height = 15
          Top = 53
          Width = 28
          Caption = 'Left:'
          ParentColor = False
        end
        object Label4: TLabel
          Left = 12
          Height = 15
          Top = 78
          Width = 36
          Caption = 'Right:'
          ParentColor = False
        end
        object SpinEditTop: TSpinEdit
          Left = 83
          Height = 21
          Top = -1
          Width = 49
          TabOrder = 0
          Value = 15
        end
        object SpinEditBottom: TSpinEdit
          Left = 83
          Height = 21
          Top = 24
          Width = 49
          TabOrder = 1
          Value = 15
        end
        object SpinEditLeft: TSpinEdit
          Left = 83
          Height = 21
          Top = 49
          Width = 49
          MaxValue = 50
          TabOrder = 2
          Value = 15
        end
        object SpinEditRight: TSpinEdit
          Left = 83
          Height = 21
          Top = 74
          Width = 49
          MaxValue = 50
          TabOrder = 3
          Value = 15
        end
      end
      object GroupBoxPrintFont: TGroupBox
        Left = 12
        Height = 56
        Top = 8
        Width = 425
        Caption = 'Print font'
        ClientHeight = 39
        ClientWidth = 421
        TabOrder = 0
        object LabelPrintFont: TLabel
          Left = 12
          Height = 15
          Top = 5
          Width = 21
          Caption = 'xxx'
          ParentColor = False
        end
        object ButtonChangePrintFont: TButton
          Left = 336
          Height = 25
          Top = -2
          Width = 75
          Caption = 'Change...'
          OnClick = ButtonChangePrintFontClick
          TabOrder = 0
        end
      end
      object EditPrintHeader: TEdit
        Left = 12
        Height = 21
        Top = 89
        Width = 273
        TabOrder = 1
      end
      object CheckBoxAdjustNameWidth: TCheckBox
        Left = 17
        Height = 26
        Top = 118
        Width = 197
        Caption = 'Adjust name column width'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
    object Page4: TTabSheet
      Caption = '&CD/DVD'
      ClientHeight = 247
      ClientWidth = 477
      object CheckBoxEjectAfterCdScan: TCheckBox
        Left = 24
        Height = 26
        Top = 45
        Width = 392
        Caption = 'Eject the disk from the drive after its contents is scanned'
        Checked = True
        OnClick = CheckBoxEjectAfterCdScanClick
        State = cbChecked
        TabOrder = 0
      end
      object CheckBoxScanAfterCdInsert: TCheckBox
        Left = 24
        Height = 26
        Top = 62
        Width = 457
        Caption = 'Automatically start scanning the disk after it is inserted to the drive'
        OnClick = CheckBoxScanAfterCdInsertClick
        TabOrder = 1
      end
      object CheckBoxNoQueries: TCheckBox
        Left = 24
        Height = 26
        Top = 79
        Width = 446
        Caption = 'No queries (no query for the disk name and no overwrite warning)'
        OnClick = CheckBoxNoQueriesClick
        TabOrder = 2
      end
      object CheckBoxDisableCdAutoRun: TCheckBox
        Left = 24
        Height = 26
        Top = 28
        Width = 433
        Caption = 'Disable autorun of programs after a disk is inserted to the drive'
        Checked = True
        OnClick = CheckBoxDisableCdAutoRunClick
        State = cbChecked
        TabOrder = 3
      end
    end
    object Page5: TTabSheet
      Caption = '&Other'
      ClientHeight = 247
      ClientWidth = 477
      object Label6: TLabel
        Left = 12
        Height = 15
        Top = 11
        Width = 169
        Caption = 'Open database at startup:'
        ParentColor = False
      end
      object CheckBoxFoundToNewWin: TCheckBox
        Left = 19
        Height = 26
        Top = 69
        Width = 213
        Caption = 'Each search to a new window'
        TabOrder = 3
      end
      object CheckBoxAutoSave: TCheckBox
        Left = 19
        Height = 26
        Top = 85
        Width = 309
        Caption = 'Save settings automatically at program end'
        TabOrder = 4
      end
      object CheckBoxBackupDBase: TCheckBox
        Left = 19
        Height = 26
        Top = 101
        Width = 333
        Caption = 'Make backup copy when compressing database'
        TabOrder = 5
      end
      object ButtonBrowse: TButton
        Left = 344
        Height = 25
        Top = 25
        Width = 75
        Caption = 'Browse...'
        OnClick = ButtonBrowseClick
        TabOrder = 1
      end
      object EditAutoLoadDBase: TEdit
        Left = 16
        Height = 21
        Top = 27
        Width = 317
        TabOrder = 0
      end
      object CheckBoxNotScanDriveNames: TCheckBox
        Left = 19
        Height = 26
        Top = 135
        Width = 370
        Caption = 'Do not read volume names for "Scan Disk" dialog box'
        TabOrder = 7
      end
      object CheckBoxPersistentBlocks: TCheckBox
        Left = 19
        Height = 26
        Top = 118
        Width = 164
        Caption = 'Persistent selections '
        Checked = True
        State = cbChecked
        TabOrder = 6
        Visible = False
      end
      object CheckBoxOpenLastOpened: TCheckBox
        Left = 19
        Height = 26
        Top = 52
        Width = 272
        Caption = 'Open last opened database at startup'
        OnClick = CheckBoxOpenLastOpenedClick
        TabOrder = 2
      end
      object CheckBoxEnableShellExecute: TCheckBox
        Left = 19
        Height = 26
        Top = 152
        Width = 208
        Caption = 'Enable opening existing files'
        Checked = True
        State = cbChecked
        TabOrder = 8
      end
    end
  end
  object ButtonOK: TButton
    Left = 261
    Height = 25
    Top = 294
    Width = 75
    Caption = 'OK'
    Default = True
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Sans'
    OnClick = ButtonOKClick
    ParentFont = False
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 335
    Height = 25
    Top = 294
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Sans'
    OnClick = ButtonCancelClick
    ParentFont = False
    TabOrder = 2
  end
  object ButtonHelp: TButton
    Left = 409
    Height = 25
    Top = 294
    Width = 75
    Caption = 'Help'
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Sans'
    OnClick = ButtonHelpClick
    ParentFont = False
    TabOrder = 3
  end
  object FontDialogDisk: TFontDialog
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    MinFontSize = 8
    MaxFontSize = 20
    Options = [fdAnsiOnly, fdForceFontExist, fdNoSimulations, fdShowHelp, fdLimitSize]
    left = 48
    top = 264
  end
  object FontDialogTree: TFontDialog
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    MinFontSize = 8
    MaxFontSize = 20
    Options = [fdAnsiOnly, fdForceFontExist, fdNoSimulations, fdShowHelp, fdLimitSize]
    left = 16
    top = 264
  end
  object FontDialogFile: TFontDialog
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    MinFontSize = 8
    MaxFontSize = 20
    Options = [fdAnsiOnly, fdForceFontExist, fdNoSimulations, fdShowHelp, fdLimitSize]
    left = 80
    top = 264
  end
  object FontDialogFound: TFontDialog
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    MinFontSize = 8
    MaxFontSize = 20
    Options = [fdAnsiOnly, fdForceFontExist, fdNoSimulations, fdShowHelp, fdLimitSize]
    left = 112
    top = 265
  end
  object FontDialogPrint: TFontDialog
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    MinFontSize = 6
    MaxFontSize = 24
    Options = [fdAnsiOnly, fdLimitSize]
    left = 208
    top = 264
  end
  object OpenDialogDBase: TOpenDialog
    HelpContext = 290
    Title = 'Open Database At Startup'
    DefaultExt = '.QDR'
    FileName = '*.qdr'
    Filter = 'DiskBase database (*.qdr)|*.qdr'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist]
    left = 176
    top = 265
  end
  object FontDialogDesc: TFontDialog
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    MinFontSize = 6
    MaxFontSize = 24
    Options = [fdAnsiOnly, fdLimitSize]
    left = 144
    top = 264
  end
end
