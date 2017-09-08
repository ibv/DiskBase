object FormLocalOptions: TFormLocalOptions
  Left = 394
  Height = 333
  Top = 193
  Width = 474
  HelpContext = 300
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Database Settings'
  ClientHeight = 333
  ClientWidth = 474
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'FreeSans'
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poDefault
  LCLVersion = '1.8.0.3'
  object TabbedNotebook: TPageControl
    Left = 3
    Height = 297
    Top = 2
    Width = 465
    ActivePage = Page1
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    ParentFont = False
    TabIndex = 0
    TabOrder = 0
    object Page1: TTabSheet
      Caption = 'Descriptions'
      ClientHeight = 261
      ClientWidth = 457
      object Label2: TLabel
        Left = 8
        Height = 41
        Top = 33
        Width = 141
        AutoSize = False
        Caption = 'Scan file for description if the name is:'
        ParentColor = False
        WordWrap = True
      end
      object Label1: TLabel
        Left = 8
        Height = 14
        Top = 110
        Width = 140
        Caption = 'Read max. characters:'
        ParentColor = False
      end
      object Label3: TLabel
        Left = 8
        Height = 14
        Top = 160
        Width = 34
        Caption = 'Filter:'
        ParentColor = False
      end
      object CheckBoxScanDesc: TCheckBox
        Left = 12
        Height = 26
        Top = 8
        Width = 140
        Caption = 'Read descriptions'
        Checked = True
        OnClick = CheckBoxScanDescClick
        State = cbChecked
        TabOrder = 0
      end
      object EditNewFileName: TEdit
        Left = 16
        Height = 20
        Top = 79
        Width = 137
        TabOrder = 1
        Text = '*.txt'
      end
      object SpinEditMaxDescrSize: TSpinEdit
        Left = 16
        Height = 20
        Top = 131
        Width = 69
        Increment = 100
        MaxValue = 32000
        MinValue = 100
        TabOrder = 2
        Value = 500
      end
      object ComboBoxConv: TComboBox
        Left = 16
        Height = 20
        Top = 180
        Width = 137
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        ItemHeight = 0
        ItemIndex = 0
        Items.Strings = (
          '(no filter)'
          'Uni'
          'HTML'
          'RTF'
        )
        ParentFont = False
        TabOrder = 3
        Text = '(no filter)'
      end
      object ButtonAdd: TButton
        Left = 69
        Height = 25
        Top = 209
        Width = 84
        Caption = 'Add >>'
        OnClick = ButtonAddClick
        TabOrder = 5
      end
      object StringGridMasks: TStringGrid
        Left = 164
        Height = 241
        Top = 16
        Width = 281
        ColCount = 4
        DefaultRowHeight = 14
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowMoving, goRowSelect, goThumbTracking]
        ParentFont = False
        RowCount = 31
        ScrollBars = ssVertical
        TabOrder = 6
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        OnKeyDown = StringGridMasksKeyDown
        ColWidths = (
          37
          108
          46
          70
        )
      end
      object ButtonDiscard: TButton
        Left = 69
        Height = 25
        Top = 233
        Width = 84
        Caption = 'Remove <<'
        OnClick = ButtonDiscardClick
        TabOrder = 4
      end
    end
    object Page2: TTabSheet
      Caption = 'Archives'
      ClientHeight = 261
      ClientWidth = 457
      object RadioGroupScanArchives: TRadioGroup
        Left = 16
        Height = 78
        Top = 12
        Width = 309
        AutoFill = True
        Caption = 'Scan archives'
        ChildSizing.LeftRightSpacing = 6
        ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
        ChildSizing.EnlargeVertical = crsHomogenousChildResize
        ChildSizing.ShrinkHorizontal = crsScaleChilds
        ChildSizing.ShrinkVertical = crsScaleChilds
        ChildSizing.Layout = cclLeftToRightThenTopToBottom
        ChildSizing.ControlsPerLine = 1
        ClientHeight = 48
        ClientWidth = 305
        ItemIndex = 0
        Items.Strings = (
          'yes, without prompt'
          'yes, with prompt'
          'no'
        )
        OnClick = RadioGroupScanArchivesClick
        TabOrder = 0
      end
      object Label4: TLabel
        Left = 24
        Height = 14
        Top = 221
        Width = 228
        Caption = 'Do not extract from files bigger than'
        ParentColor = False
      end
      object Label6: TLabel
        Left = 276
        Height = 14
        Top = 221
        Width = 69
        Caption = 'megabytes'
        ParentColor = False
      end
      object CheckBoxScanZipExeArchives: TCheckBox
        Left = 24
        Height = 26
        Top = 97
        Width = 278
        Caption = 'Scan self-extracting archives of ZIP type'
        TabOrder = 1
      end
      object CheckBoxScanOtherExeArchives: TCheckBox
        Left = 24
        Height = 26
        Top = 114
        Width = 368
        Caption = 'Scan self-extracting archives of ZIP, ARJ and RAR types'
        TabOrder = 2
      end
      object CheckBoxAlwaysRescanArchives: TCheckBox
        Left = 24
        Height = 26
        Top = 131
        Width = 172
        Caption = 'Always rescan archives'
        TabOrder = 3
      end
      object CheckBoxExtractFromZips: TCheckBox
        Left = 24
        Height = 26
        Top = 158
        Width = 265
        Caption = 'Extract descriptions from ZIP archives'
        TabOrder = 4
      end
      object CheckBoxExtractFromRars: TCheckBox
        Left = 24
        Height = 26
        Top = 175
        Width = 269
        Caption = 'Extract descriptions from RAR archives'
        TabOrder = 5
      end
      object CheckBoxExtractFromAces: TCheckBox
        Left = 24
        Height = 26
        Top = 192
        Width = 269
        Caption = 'Extract descriptions from ACE archives'
        TabOrder = 6
      end
      object SpinEditSizeLimit: TSpinEdit
        Left = 209
        Height = 20
        Top = 217
        Width = 57
        Increment = 10
        MaxValue = 1000
        MinValue = 1
        TabOrder = 7
        Value = 20
      end
    end
    object Page3: TTabSheet
      Caption = 'Options'
      ClientHeight = 261
      ClientWidth = 457
      object Label5: TLabel
        Left = 24
        Height = 14
        Top = 57
        Width = 222
        Caption = 'Template for disk name generation:'
        ParentColor = False
      end
      object CheckBoxOverwriteWarning: TCheckBox
        Left = 24
        Height = 26
        Top = 16
        Width = 315
        Caption = 'Warning before overwriting a disk in database'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object CheckBoxShowDeleted: TCheckBox
        Left = 24
        Height = 26
        Top = 124
        Width = 160
        Caption = 'Display deleted disks'
        TabOrder = 4
      end
      object CheckBoxSimulateDiskInfo: TCheckBox
        Left = 24
        Height = 26
        Top = 143
        Width = 192
        Caption = 'Simulate disk descriptions'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object CheckBoxDisableVolNameChange: TCheckBox
        Left = 24
        Height = 26
        Top = 35
        Width = 241
        Caption = 'Do not offer volume name change'
        TabOrder = 1
      end
      object EditDiskNamePattern: TEdit
        Left = 40
        Height = 20
        Top = 76
        Width = 265
        MaxLength = 99
        TabOrder = 2
      end
      object ButtonResetCounter: TButton
        Left = 312
        Height = 25
        Top = 74
        Width = 100
        Caption = 'Reset counter'
        OnClick = ButtonResetCounterClick
        TabOrder = 3
      end
      object CheckBoxGenerateFolderDesc: TCheckBox
        Left = 24
        Height = 26
        Top = 183
        Width = 373
        Caption = 'Automaticky přiřazovat popisky k adres?řům a archivům'
        TabOrder = 7
        Visible = False
      end
      object CheckBoxImportFilesBbs: TCheckBox
        Left = 24
        Height = 26
        Top = 202
        Width = 219
        Caption = 'Importovat popisky z FILES.BBS'
        TabOrder = 8
        Visible = False
      end
      object CheckBoxAlwaysCreateDesc: TCheckBox
        Left = 24
        Height = 26
        Top = 163
        Width = 269
        Caption = 'Always recreate descriptions from files'
        TabOrder = 6
      end
    end
  end
  object ButtonOK: TButton
    Left = 316
    Height = 25
    Top = 303
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
    Left = 394
    Height = 25
    Top = 303
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
    Left = 5
    Height = 25
    Top = 303
    Width = 75
    Caption = 'Help'
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Sans'
    OnClick = ButtonHelpClick
    ParentFont = False
    TabOrder = 3
  end
end
