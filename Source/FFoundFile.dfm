object FormFoundFileList: TFormFoundFileList
  Left = 349
  Height = 382
  Top = 181
  Width = 656
  HelpContext = 250
  ClientHeight = 382
  ClientWidth = 656
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Sans'
  Icon.Data = {
    360400000000010002002020100000000000E802000026000000101010000000
    0000280100000E03000028000000200000004000000001000400000000008002
    0000000000000000000000000000000000000000000000008000008000000080
    8000800000008000800080800000C0C0C000808080000000FF0000FF000000FF
    FF00FF000000FF00FF00FFFF0000FFFFFF006666666666666666666666666666
    66666FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF66FFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFF66FF88888888880000000000000000FF66FFFFFFFFFFFF078888888888888
    0FF66FFFFFFFFFFFF0F77770007799780FF66FF88888888880F7700FFF007778
    0FF66FFFFFFFFFFFF0F70FFF0FFF07780FF66FFFFFFFFFFFF0F0000000000078
    0FF66FF88888888880F77777777777780FF66FFFFFFF0000000000000000FFF7
    0FF66FFFFFFF078888888888888000000FF66FF888880F799777777777808888
    8FF66FFFFFFF0F00000000000780FFFFFFF66FFFFFFF0F77777777777780FFFF
    FFF66FF888880F0000000000078088888FF66FFFFFFF0F77777777777780FFFF
    FFF66FF0000000000000000FFF70FFFFFFF66FF0788888888888880000008888
    8FF66FF0F77777777777780FFFFFFFFFFFF66FF0F77700000077780FFFFFFFFF
    FFF66FF0F000000000000808888888888FF66FF0F77700000077780FFFFFFFFF
    FFF66FF0F77777777799780FFFFFFFFFFFF66FF0FFFFFFFFFFFFF70888888888
    8FF66FF0000000000000000FFFFFFFFFFFF66FFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFF66FF77F77F77F77F77F77F77F77F77FF66FF07F07F07F07F07F07F07F07F0
    7FF66FF0FF0FF0FF0FF0FF0FF0FF0FF0FFF66666066066066066066066066066
    0666000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000F6DB6DB72800000010000000200000000100
    040000000000C000000000000000000000000000000000000000000000000000
    80000080000000808000800000008000800080800000C0C0C000808080000000
    FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000009006F7777
    77770009906FFFFFFFFF0999996F777777770909906FFFFFFFFF0909006F0F0F
    0F0F090000666060606000000000000000000000000000000000078888888888
    88800F777700077AA7800F7700FFF00777800F70FFF0FFF077800F0000000000
    00800F777777777777800FFFFFFFFFFFFF700000000000000000EC000000E400
    000080000000A4000000AC000000BC000000FFFF000000000000000000000000
    0000000000000000000000000000000000000000000000000000
  }
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PopupMenu = PopupMenu
  Position = poDefault
  LCLVersion = '1.8.0.4'
  Visible = True
  object DrawGrid: TDrawGrid
    Left = 0
    Height = 382
    Top = 0
    Width = 656
    Align = alClient
    ColCount = 6
    DefaultRowHeight = 16
    ExtendedSelect = False
    FixedCols = 0
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Sans'
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect, goThumbTracking]
    ParentFont = False
    RowCount = 100
    TabOrder = 0
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Sans'
    OnDblClick = DrawGridDblClick
    OnDrawCell = DrawGridDrawCell
    OnKeyDown = DrawGridKeyDown
    OnMouseDown = DrawGridMouseDown
    OnMouseMove = DrawGridMouseMove
    OnSelectCell = DrawGridSelectCell
    ColWidths = (
      81
      123
      99
      67
      98
      337
    )
  end
  object PopupMenu: TPopupMenu
    left = 276
    top = 108
    object MenuGoTo: TMenuItem
      Caption = '&Go to disk in database'
      ShortCut = 13
      OnClick = MenuGoToClick
    end
    object MenuCopy: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = MenuCopyClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuSelectAll: TMenuItem
      Caption = 'Select &all'
      OnClick = MenuSelectAllClick
    end
    object MenuUnselectAll: TMenuItem
      Caption = '&Unselect all'
      ShortCut = 16469
      OnClick = MenuUnselectAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuPrint: TMenuItem
      Caption = '&Print...'
      ShortCut = 16464
      OnClick = MenuPrintClick
    end
    object MenuHelp: TMenuItem
      Caption = '&Help'
      HelpContext = 250
      ShortCut = 112
      OnClick = MenuHelpClick
    end
  end
  object PrintDialog: TPrintDialog
    HelpContext = 320
    FromPage = 1
    MinPage = 1
    MaxPage = 1000
    Options = [poPageNums, poSelection, poWarning, poHelp]
    ToPage = 1
    left = 152
    top = 108
  end
end
