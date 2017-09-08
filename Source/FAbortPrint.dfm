object FormAbortPrint: TFormAbortPrint
  Left = 313
  Height = 74
  Top = 117
  Width = 365
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Printing'
  ClientHeight = 74
  ClientWidth = 365
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  FormStyle = fsStayOnTop
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.3'
  object LabelProgress: TLabel
    Left = 12
    Height = 16
    Top = 16
    Width = 337
    Alignment = taCenter
    AutoSize = False
    ParentColor = False
  end
  object ButtonAbort: TButton
    Left = 145
    Height = 25
    Top = 40
    Width = 75
    Caption = 'Cancel'
    OnClick = ButtonAbortClick
    TabOrder = 0
  end
end
