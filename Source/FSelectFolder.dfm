object FormSelectFolder: TFormSelectFolder
  Left = 479
  Height = 333
  Top = 260
  Width = 261
  BorderIcons = [biSystemMenu]
  Caption = 'Select folder'
  ClientHeight = 333
  ClientWidth = 261
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnShow = FormShow
  LCLVersion = '1.8.0.3'
  object Panel1: TPanel
    Left = 0
    Height = 31
    Top = 302
    Width = 261
    Align = alBottom
    ClientHeight = 31
    ClientWidth = 261
    TabOrder = 0
    object ButtonOK: TButton
      Left = 3
      Height = 25
      Top = 3
      Width = 75
      Caption = 'OK'
      Default = True
      OnClick = ButtonOKClick
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 79
      Height = 25
      Top = 3
      Width = 75
      Cancel = True
      Caption = 'Cancel'
      OnClick = ButtonCancelClick
      TabOrder = 1
    end
  end
  object DirTreeView: TTreeView
    Left = 0
    Height = 302
    Top = 0
    Width = 261
    Align = alClient
    HideSelection = False
    Indent = 19
    TabOrder = 1
    OnClick = DirTreeViewClick
    OnCollapsed = DirTreeViewCollapsed
    OnExpanded = DirTreeViewExpanded
    Options = [tvoAutoItemHeight, tvoKeepCollapsedNodes, tvoShowButtons, tvoShowLines, tvoShowRoot, tvoToolTips, tvoThemedDraw]
  end
end
