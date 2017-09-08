object FormAbout: TFormAbout
  Left = 540
  Height = 137
  Top = 267
  Width = 425
  HelpContext = 10
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About DiskBase 5'
  ClientHeight = 137
  ClientWidth = 425
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object LabelAbout: TLabel
    Left = 16
    Height = 45
    Top = 12
    Width = 397
    Alignment = taCenter
    AutoSize = False
    Caption = 'x'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object ButtonOK: TButton
    Left = 232
    Height = 25
    Top = 105
    Width = 75
    Cancel = True
    Caption = 'OK'
    Default = True
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    OnClick = ButtonOKClick
    ParentFont = False
    TabOrder = 0
  end
  object ButtonMoreInfo: TButton
    Left = 312
    Height = 25
    Top = 105
    Width = 100
    Caption = 'More info'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    OnClick = ButtonMoreInfoClick
    ParentFont = False
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 12
    Height = 33
    Top = 65
    Width = 401
    BevelInner = bvRaised
    BevelOuter = bvNone
    Caption = 'sourceforge.net'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Sans'
    ParentFont = False
    TabOrder = 2
  end
end
