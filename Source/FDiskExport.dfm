object FormDiskExport: TFormDiskExport
  Left = 344
  Height = 72
  Top = 248
  Width = 389
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Export Database To Text Format'
  ClientHeight = 72
  ClientWidth = 389
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Sans'
  OnCreate = FormCreate
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.0.4'
  object LabelWhatDoing: TLabel
    Left = 0
    Height = 17
    Top = 16
    Width = 389
    Alignment = taCenter
    AutoSize = False
    Caption = 'LabelWhatDoing'
    ParentColor = False
  end
  object ButtonCancel: TButton
    Left = 157
    Height = 25
    Top = 40
    Width = 75
    Cancel = True
    Caption = 'Cancel'
    Default = True
    OnClick = ButtonCancelClick
    TabOrder = 0
  end
  object SaveDialogExportToText: TSaveDialog
    Title = 'Save Exported Data To File'
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    left = 8
    top = 40
  end
  object OpenDialogExportFormat: TOpenDialog
    Title = 'Select Export Format'
    Filter = 'DiskBase Export Format (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    left = 40
    top = 40
  end
end
