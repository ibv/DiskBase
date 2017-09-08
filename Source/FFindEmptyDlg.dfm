object FormSearchEmptyDlg: TFormSearchEmptyDlg
  Left = 478
  Height = 118
  Top = 282
  Width = 189
  HelpContext = 260
  ActiveControl = RadioGroupSort
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'List Of Free Space'
  ClientHeight = 118
  ClientWidth = 189
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.3'
  object RadioGroupSort: TRadioGroup
    Left = 9
    Height = 69
    Top = 6
    Width = 169
    AutoFill = True
    Caption = 'Sort list according to: '
    ChildSizing.LeftRightSpacing = 6
    ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
    ChildSizing.EnlargeVertical = crsHomogenousChildResize
    ChildSizing.ShrinkHorizontal = crsScaleChilds
    ChildSizing.ShrinkVertical = crsScaleChilds
    ChildSizing.Layout = cclLeftToRightThenTopToBottom
    ChildSizing.ControlsPerLine = 1
    ClientHeight = 40
    ClientWidth = 165
    ItemIndex = 0
    Items.Strings = (
      'size of free space'
      'disk name'
      'size of disk'
    )
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 9
    Height = 25
    Top = 84
    Width = 75
    Caption = 'OK'
    Default = True
    OnClick = ButtonOKClick
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 103
    Height = 25
    Top = 84
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    OnClick = ButtonCancelClick
    TabOrder = 2
  end
end
