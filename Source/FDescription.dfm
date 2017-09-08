object FormDescription: TFormDescription
  Left = 375
  Height = 379
  Top = 256
  Width = 545
  HelpContext = 180
  ActiveControl = MemoDesc
  Caption = 'Description'
  ClientHeight = 360
  ClientWidth = 545
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Menu = MainMenu1
  OnShow = FormShow
  LCLVersion = '1.8.0.3'
  object MemoDesc: TMemo
    Left = 0
    Height = 323
    Top = 0
    Width = 545
    Align = alClient
    BorderStyle = bsNone
    HideSelection = False
    OnChange = MemoDescChange
    OnKeyDown = MemoDescKeyDown
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Height = 37
    Top = 323
    Width = 545
    Align = alBottom
    BorderStyle = bsSingle
    ClientHeight = 35
    ClientWidth = 543
    TabOrder = 1
    object LabelFile: TLabel
      Left = 242
      Height = 13
      Top = 9
      Width = 48
      Caption = 'LabelFile'
      ParentColor = False
    end
    object ButtonOK: TButton
      Left = 4
      Height = 25
      Top = 4
      Width = 75
      Caption = 'OK'
      Default = True
      OnClick = MenuExitAndSaveClick
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 81
      Height = 25
      Top = 4
      Width = 75
      Cancel = True
      Caption = 'Cancel'
      OnClick = MenuExitClick
      TabOrder = 1
    end
    object ButtonLast: TButton
      Left = 159
      Height = 25
      Top = 4
      Width = 75
      Caption = '&Paste last'
      TabOrder = 2
    end
  end
  object MainMenu1: TMainMenu
    left = 424
    top = 26
    object MenuFile: TMenuItem
      Caption = '&File'
      object MenuExitAndSave: TMenuItem
        Caption = '&Save and exit (OK)'
        ShortCut = 16467
        OnClick = MenuExitAndSaveClick
      end
      object MenuExit: TMenuItem
        Caption = 'E&xit without save (Cancel)'
        ShortCut = 27
        OnClick = MenuExitClick
      end
    end
    object MenuEdit: TMenuItem
      Caption = '&Edit'
      OnClick = MenuEditClick
      object MenuCut: TMenuItem
        Caption = 'Cu&t'
        ShortCut = 16472
        OnClick = MenuCutClick
      end
      object MenuCopy: TMenuItem
        Caption = '&Copy'
        ShortCut = 16451
        OnClick = MenuCopyClick
      end
      object MenuPaste: TMenuItem
        Caption = '&Paste'
        ShortCut = 16470
        OnClick = MenuPasteClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuSelectAll: TMenuItem
        Caption = 'Select &all'
        ShortCut = 16449
        OnClick = MenuSelectAllClick
      end
    end
    object MenuSearch: TMenuItem
      Caption = '&Search'
      object MenuFind: TMenuItem
        Caption = '&Find'
        ShortCut = 16454
        OnClick = MenuFindClick
      end
      object MenuNext: TMenuItem
        Caption = 'Find &next'
        ShortCut = 114
        OnClick = MenuNextClick
      end
    end
  end
  object FindDialog: TFindDialog
    Options = [frDown, frHideWholeWord, frHideUpDown, frShowHelp]
    OnFind = FindDialogFind
    left = 268
    top = 28
  end
end
