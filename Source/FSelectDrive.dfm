object FormSelectDrive: TFormSelectDrive
  Left = 386
  Height = 222
  Top = 389
  Width = 217
  HelpContext = 110
  BorderIcons = [biSystemMenu]
  Caption = 'Scan disk:'
  ClientHeight = 222
  ClientWidth = 217
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnShow = FormShow
  LCLVersion = '1.8.0.3'
  object ListBoxDrives: TListBox
    Left = 0
    Height = 171
    Top = 0
    Width = 217
    Align = alClient
    ItemHeight = 0
    OnDblClick = ListBoxDrivesDblClick
    ScrollWidth = 213
    TabOrder = 0
    TopIndex = -1
  end
  object Panel1: TPanel
    Left = 0
    Height = 51
    Top = 171
    Width = 217
    Align = alBottom
    ClientHeight = 51
    ClientWidth = 217
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    ParentFont = False
    TabOrder = 1
    object ButtonCancel: TButton
      Left = 112
      Height = 25
      Top = 22
      Width = 75
      Cancel = True
      Caption = 'Cancel'
      OnClick = ButtonCancelClick
      TabOrder = 0
    end
    object ButtonOK: TButton
      Left = 34
      Height = 25
      Top = 22
      Width = 75
      Caption = 'OK'
      Default = True
      OnClick = ButtonOKClick
      TabOrder = 1
    end
    object CheckBoxNoQueries: TCheckBox
      Left = 9
      Height = 26
      Top = 3
      Width = 96
      Caption = 'No queries'
      OnClick = CheckBoxNoQueriesClick
      TabOrder = 2
    end
  end
end
