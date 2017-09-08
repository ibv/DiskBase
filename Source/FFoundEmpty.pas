unit FFoundEmpty;
(*====================================================================
MDI window with found files
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages, PrintersDlgs,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, Menus,
  UTypes, UApiTypes, UCollections, FSettings, FMain, {FDBase,} UStringList;

type
  TOneELine = class
    DiskSize : longint;
    DiskFree : longint;
    Disk     : TPQString;
    ShortDesc: TPQString;
    Id       : longint;
    ExtAttr  : byte;
    constructor Create(ADiskFree, ADiskSize: longint;
                       var ADisk, AShortDesc: ShortString;
                       AID: longint; AExtAttr: byte);
    destructor  Destroy; override;
    end;


  { TFormFoundEmptyList }

  TFormFoundEmptyList = class(TForm)
    DrawGrid: TDrawGrid;
    PopupMenu: TPopupMenu;
    MenuGoTo: TMenuItem;
    MenuPrint: TMenuItem;
    MenuHelp: TMenuItem;
    MenuCopy: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    MenuSelectAll: TMenuItem;
    MenuUnselectAll: TMenuItem;
    PrintDialog: TPrintDialog;
    ///PrintDialog: TPrintDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridDblClick(Sender: TObject);
    procedure MenuGoToClick(Sender: TObject);
    procedure MenuPrintClick(Sender: TObject);
    procedure MenuHelpClick(Sender: TObject);
    procedure DrawGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DrawGridSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuUnselectAllClick(Sender: TObject);
  private
    ///FoundList: TQStringList;
    FoundList: TStringList;
    NeedReSort   : boolean;
    ReversedSort : boolean;
    CanDraw      : boolean;
    FormIsClosed : boolean;
    LastFileSelected : Integer;
    MakeSelection: boolean;
    DisableNextDoubleClick: boolean;
    function  GetSortString(OneELine: TOneELine): ShortString;
    procedure ResortFoundList;
    procedure ResetFontAndSize;
  public
    QGlobalOptions: TGlobalOptions;
    ///DBaseWindow: TFormDBase;
    DBaseWindow: TMainForm;
    SortCriteria : Integer; {0 - free, 1 - name, 2, size}
    procedure GetList(DBaseHandle: PDBaseHandle);
    procedure LocalIdle;
    procedure LocalTimer;
    procedure SetNeedResort(Sort: Integer);
    procedure ChangeGlobalOptions;
    procedure SelectAll;
    procedure UnselectAll;
    procedure MakeCopy;
    procedure MakePrint;
    procedure ExportToOtherFormat;
  end;

//---------------------------------------------------------------------------

implementation

uses Clipbrd, {Printers,} UExceptions, UAPi, UBaseUtils, FFindEmptyDlg,
     FProgress, FAbortPrint,
     {FMain,} ULang, UPrinter,
{$ifdef LOGFONT}
     UFont,
{$endif}
     UExport, FFoundExport;


{$R *.dfm}

const
  ClipboardLimit = 32 * 1024 - 10;

{---TOneELine---------------------------------------------------------}

constructor TOneELine.Create(ADiskFree, ADiskSize: longint;
                            var ADisk, AShortDesc: ShortString;
                            AID: longint; AExtAttr: byte);

  begin
  DiskFree  := ADiskFree;
  DiskSize  := ADiskSize;
  Disk      := QNewStr(aDisk);
  ShortDesc := QNewStr(aShortDesc);
  ID        := AID;
  ExtAttr   := AExtAttr;
  end;

//---------------------------------------------------------------------------

destructor TOneELine.Destroy;

  begin
  QDisposeStr(Disk);
  QDisposeStr(ShortDesc);
  end;

//---TFormFoundList----------------------------------------------------------

procedure TFormFoundEmptyList.FormClose(Sender: TObject;
  var Action: TCloseAction);

  begin
  if FormIsClosed then Action := caFree;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

  begin
  CanClose := true;
  FormIsClosed := true;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.GetList(DBaseHandle: PDBaseHandle);

  var
    OneFile     : TOneFile;
    Disk, Dir, ShortDesc: ShortString;
    OneELine    : TOneELine;
    Index       : Integer;
    Total       : Integer;
    PercentPie  : Integer;
    ShowProgress: boolean;

  begin
  FoundList.Clear;
  try
    Total := QI_GetFoundCount(DBaseHandle);
    ShowProgress := Total > 700;
    if Total < 2000
      then PercentPie := Total div 10 + 1
      else PercentPie := Total div 50 + 1;
    if ShowProgress then FormProgress.ResetAndShow(lsSorting);
    for Index := 0 to pred(Total) do
      begin
      if QI_GetSearchItemAt (DBaseHandle, Index, OneFile, Disk, Dir, ShortDesc) then
        begin
        if ShowProgress and ((Index mod PercentPie) = 0) then
          FormProgress.SetProgress((longint(Index) * 100) div Total);
        OneELine := TOneELine.Create(OneFile.Time, OneFile.Size, Disk,
                                     ShortDesc, Index, 0);
        FoundList.AddObject(GetSortString(OneELine), OneELine);
        if ShowProgress and FormProgress.StopIt then break;
        end;
      end;
    DrawGrid.RowCount := FoundList.Count + 1;
    CanDraw := true;
    if ShowProgress then FormProgress.Hide;
    QI_ClearFoundList(DBaseHandle);
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.FormCreate(Sender: TObject);

  begin
  FormIsClosed := false;
  LastFileSelected := 1;
  MakeSelection := false;
  ///FoundList := TQStringList.Create;
  FoundList := TStringList.Create;
  ///FoundList.Sorted := true;
  ///FoundList.Duplicates := qdupAccept;
  SortCriteria := 1;
  NeedResort := false;
  ReversedSort := false;
  QGlobalOptions := TGlobalOptions.Create;
  FormSettings.GetOptions(QGlobalOptions);
  ResetFontAndSize;
  CanDraw := false;
  DisableNextDoubleClick := false;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.DrawGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);

  var
    S: ShortString;
    StartX     : Integer;
    OneELine   : TOneELine;
    DiskSize   : longint;
    DiskFree   : longint;
    Percent    : longint;
    TmpComp    : comp;

  begin
  if not CanDraw or FormIsClosed then exit;
  if Row <= FoundList.Count then
    begin

    case Col of
      0:
        if Row = 0
          then
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top, lsDisk2)
          else
            begin
            OneELine := TOneELine(FoundList.Objects[Row-1]);
            if (OneELine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top,
              GetPQString(OneELine.Disk));
            end;
      1:
        begin
        if Row = 0
          then
            begin
            S := lsSize;
            StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end
          else
            begin
            OneELine := TOneELine(FoundList.Objects[Row-1]);
            if (OneELine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DiskSize := OneELine.DiskSize;
            TmpComp := DiskSize;
            S := FormatBigSize(TmpComp * 1024);
            StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end;
        end;
      2:
        begin
        if Row = 0
          then
            begin
            S := lsFreeSpace;
            StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end
          else
            begin
            OneELine := TOneELine(FoundList.Objects[Row-1]);
            if (OneELine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DiskFree := OneELine.DiskFree;
            TmpComp := DiskFree;
            S := FormatBigSize(TmpComp * 1024);
            StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end;
        end;
      3:
        begin
        if Row = 0
          then
            begin
            S := lsFreeSpacePerCent;
            StartX := Rect.Left + (Rect.Right - Rect.Left) div 2 -
              DrawGrid.Canvas.TextWidth(S) div 2 - 2;
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end
          else
            begin
            OneELine := TOneELine(FoundList.Objects[Row-1]);
            if (OneELine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DiskFree := OneELine.DiskFree;
            DiskSize := OneELine.DiskSize;
            if DiskSize > 0
              then Percent := (DiskFree * 100) div DiskSize
              else Percent := 0;
            S := IntToStr(Percent) + '%';
            StartX := Rect.Left + (Rect.Right - Rect.Left) div 2 -
              DrawGrid.Canvas.TextWidth(S) div 2 - 2;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end;
        end;
      4:
        if Row = 0
          then
            //DrawGrid.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top, lsDescription)
          else
            begin
            OneELine := TOneELine(FoundList.Objects[Row-1]);
            if (OneELine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top,
              GetPQString(OneELine.ShortDesc));
            end;
      end;
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.FormDestroy(Sender: TObject);

  begin
  FreeObjects(FoundList);
  FoundList.Free;
  QGlobalOptions.Free;
  end;

//---------------------------------------------------------------------------
// Gets the sort string

function TFormFoundEmptyList.GetSortString(OneELine: TOneELine): ShortString;

  var
    TmpL : longint;

  begin
  Result := '';
  with OneELine do
    begin
    case SortCriteria of
      0:
        begin  // sort by disk free space
        TmpL := MaxLongInt - DiskFree;
        Result := Format('%10.10d', [TmpL]) + ' ' + GetPQString(Disk);
        end;
      1:
        begin // sort by disk names
        TmpL := MaxLongInt - DiskFree;
        Result := GetPQString(Disk) + ' ' + Format('%10.10d', [TmpL]);
        end;
      2:
        begin  // sort by disk sizes
        TmpL := MaxLongInt - DiskSize;
        Result := Format('%10.10d', [TmpL]) + ' ' + GetPQString(Disk);
        end;
      3:
        begin // sort by percentage of free space
        if DiskSize > 0
          then TmpL := MaxLongInt - (DiskFree * 100) div DiskSize
          else TmpL := 0;
        Result := Format('%10.10d', [TmpL]) + ' ' + GetPQString(Disk);
        end;
      end;
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.LocalIdle;

  begin
  if NeedReSort then
    begin
    NeedReSort := false;
    ResortFoundList;
    end;
  end;

//---------------------------------------------------------------------------
// Called from the main form

procedure TFormFoundEmptyList.LocalTimer;

  begin
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.SetNeedResort(Sort: Integer);

  begin
  if Sort <> SortCriteria
    then
      begin
      SortCriteria := Sort;
      ReversedSort := false;
      end
    else
      ReversedSort := not ReversedSort;
  NeedResort := true;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  var
    i: Integer;
    OneColWidth, TotalWidth: Integer;

  begin
  DisableNextDoubleClick := false;
  with DrawGrid do
    if Button = mbLeft then
      begin
      if Y < DefaultRowHeight
        then
          begin
          TotalWidth := 0;
          DisableNextDoubleClick := true;
          for i := 0 to pred(ColCount) do
            begin
            OneColWidth := DrawGrid.ColWidths[i];
            inc(TotalWidth, OneColWidth);
            if X < TotalWidth then
              begin
              case i of
                0: SetNeedResort(1);
                1: SetNeedResort(2);
                2: SetNeedResort(0);
                3: SetNeedResort(3);
                end;
              break;
              end;
            end;
          exit;
          end
        else
          begin
          if LastFileSelected >= DrawGrid.RowCount then
            LastFileSelected := pred(DrawGrid.RowCount);
          if ssShift in Shift then
            begin
            for i := 1 to pred(DrawGrid.RowCount) do
              begin
              with TOneELine(FoundList.Objects[i-1]) do
                ExtAttr := ExtAttr and not eaSelected;
              end;
            if LastFileSelected <= DrawGrid.Row
              then
                for i := LastFileSelected to DrawGrid.Row do
                  begin
                  with TOneELine(FoundList.Objects[i-1]) do
                    if ExtAttr and eaSelected <> 0
                      then ExtAttr := ExtAttr and not eaSelected
                      else ExtAttr := ExtAttr or eaSelected;
                  end
              else
                for i := LastFileSelected downto DrawGrid.Row do
                  begin
                  with TOneELine(FoundList.Objects[i-1]) do
                    if ExtAttr and eaSelected <> 0
                      then ExtAttr := ExtAttr and not eaSelected
                      else ExtAttr := ExtAttr or eaSelected;
                  end;
            DrawGrid.Repaint;
            end;
          if ssCtrl in Shift then
            begin
            with TOneELine(FoundList.Objects[DrawGrid.Row-1]) do
              if ExtAttr and eaSelected <> 0
                then ExtAttr := ExtAttr and not eaSelected
                else ExtAttr := ExtAttr or eaSelected;
          end;
      end;
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.ResortFoundList;

  var
    ///NewFoundList  : TQStringList;
    NewFoundList  : TStringList;
    Index         : Integer;
    OneELine      : TOneELine;
    ADisk, AShortDesc: ShortString;
    Total         : Integer;
    PercentPie    : Integer;
    ShowProgress  : boolean;
    SaveID        : longint;

  begin
  SaveID := TOneELine(FoundList.Objects[DrawGrid.Row-1]).ID;
  Total := FoundList.Count;
  ShowProgress := Total > 700;
  if Total < 2000
    then PercentPie := Total div 10 + 1
    else PercentPie := Total div 50 + 1;
  if ShowProgress then FormProgress.ResetAndShow(lsSorting);
  ///NewFoundList := TQStringList.Create;
  NewFoundList := TStringList.Create;
  NewFoundList.Capacity := FoundList.Count;
  NewFoundList.Sorted := true;
  NewFoundList.Duplicates := dupAccept;
  ///NewFoundList.Reversed := ReversedSort;
  ///NewFoundList.Duplicates := qdupAccept;
  for Index := 0 to pred(Total) do
    with TOneELine(FoundList.Objects[Index]) do
      begin
      if ShowProgress and ((Index mod PercentPie) = 0) then
        FormProgress.SetProgress((longint(Index) * 100) div Total);
      ADisk := GetPQString(Disk);
      AShortDesc := GetPQString(ShortDesc);
      OneELine := TOneELine.Create(DiskFree, DiskSize, ADisk, AShortDesc,
                                   ID, ExtAttr);
      NewFoundList.AddObject(GetSortString(OneELine), OneELine);
      if ShowProgress and FormProgress.StopIt then break;
      end;
  if ShowProgress and FormProgress.StopIt
    then
      begin
      FreeObjects(NewFoundList);
      NewFoundList.Free
      end
    else
      begin
      FreeObjects(FoundList);
      FoundList.Free;
      FoundList := NewFoundList;
      for Index := 0 to pred(Total) do
        if SaveID = TOneELine(FoundList.Objects[Index]).ID then
          begin
          DrawGrid.Row := Index + 1;
          break;
          end;
      end;
  if ShowProgress then FormProgress.Hide;
  DrawGrid.Refresh;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.ResetFontAndSize;

  var TmpComp: comp;
  begin
  with DrawGrid do
    begin
    {$ifndef LOGFONT}
    Font.Assign       (QGlobalOptions.FoundFont);
    Canvas.Font.Assign(QGlobalOptions.FoundFont);
    {$else}
    SetFontFromLogFont(Font, QGlobalOptions.FoundLogFont);
    SetFontFromLogFont(Canvas.Font, QGlobalOptions.FoundLogFont);
    {$endif}
    DefaultRowHeight := Canvas.TextHeight('My');
    ColWidths[0] := Canvas.TextWidth('Mmmmxxxx.mxx') + 2;
    TmpComp := 200000000;
    TmpComp := TmpComp * 10000;
    ColWidths[1] := Canvas.TextWidth(FormatBigSize(TmpComp)) + 2;
    ColWidths[2] := ColWidths[1];
    ColWidths[3] := Canvas.TextWidth(lsFreeSpacePerCent1) + 2;
    ColWidths[4] := 25 * Canvas.TextWidth('Mmmmxxxxx ');
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.ChangeGlobalOptions;

  begin
  FormSettings.GetOptions(QGlobalOptions);
  ResetFontAndSize;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.DrawGridDblClick(Sender: TObject);

  begin
  if DisableNextDoubleClick then exit;
  with TOneELine(FoundList.Objects[DrawGrid.Row-1]) do
    begin
    DBaseWindow.BringToFront;
    DBaseWindow.JumpTo(GetPQString(Disk), '', '');
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.MenuGoToClick(Sender: TObject);

  begin
  DrawGridDblClick(Sender);
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.MenuPrintClick(Sender: TObject);

  begin
  MakePrint;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.MenuHelpClick(Sender: TObject);

  begin
  Application.HelpContext(270);
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.DrawGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);

  begin
  if (Shift = []) and ((Key = vk_Insert) or (Key = vk_Space)) then
    begin
    with TOneELine(FoundList.Objects[pred(DrawGrid.Row)]) do
      if ExtAttr and eaSelected <> 0
        then ExtAttr := ExtAttr and not eaSelected
        else ExtAttr := ExtAttr or eaSelected;
    if (DrawGrid.Row + 1) < DrawGrid.RowCount then
      DrawGrid.Row := DrawGrid.Row + 1;
    end;
  if (ssShift in Shift) and
     ((Key = vk_Down) or (Key = vk_Up) or (Key = vk_Prior) or
      (Key = vk_Next) or (Key = vk_Home) or (Key = vk_End))
        then
          begin
          LastFileSelected := DrawGrid.Selection.Top;
          MakeSelection := true;
          end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.SelectAll;

  var
    i: Integer;
    AlreadySelected: boolean;

  begin
  if UsePersistentBlocks
    then
      begin
      AlreadySelected := true;
      for i := 0 to pred(FoundList.Count) do
        with TOneELine(FoundList.Objects[i]) do
          if (ExtAttr and eaSelected) = 0 then
            begin
            AlreadySelected := false;
            break;
            end;
      if AlreadySelected
        then
          for i := 0 to pred(FoundList.Count) do
            with TOneELine(FoundList.Objects[i]) do
              ExtAttr := ExtAttr and not eaSelected
        else
          for i := 0 to pred(FoundList.Count) do
            with TOneELine(FoundList.Objects[i]) do
              ExtAttr := ExtAttr or eaSelected;
      end
    else
      begin
      for i := 0 to pred(FoundList.Count) do
        with TOneELine(FoundList.Objects[i]) do
          ExtAttr := ExtAttr or eaSelected;
      end;
  DrawGrid.Repaint;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.UnselectAll;

  var
    i: Integer;

  begin
  for i := 0 to pred(FoundList.Count) do
    begin
    with TOneELine(FoundList.Objects[i]) do
      ExtAttr := ExtAttr and not eaSelected;
    end;
  DrawGrid.Repaint;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.DrawGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);

  var
    i: Integer;

  begin
  if MakeSelection then
    begin
    MakeSelection := false;
    if LastFileSelected <= Row
      then
        for i := LastFileSelected to Row-1 do
          begin
          with TOneELine(FoundList.Objects[i-1]) do
            if ExtAttr and eaSelected <> 0
              then ExtAttr := ExtAttr and not eaSelected
              else ExtAttr := ExtAttr or eaSelected;
          end
      else
        for i := LastFileSelected downto Row+1 do
          begin
          with TOneELine(FoundList.Objects[i-1]) do
            if ExtAttr and eaSelected <> 0
              then ExtAttr := ExtAttr and not eaSelected
              else ExtAttr := ExtAttr or eaSelected;
          end;
    if abs(LastFileSelected - Row) > 1 then
      DrawGrid.Repaint;
    end;
  LastFileSelected := DrawGrid.Selection.Top;
  end;

//---------------------------------------------------------------------------
// Copies to the clipboard

procedure TFormFoundEmptyList.MakeCopy;

  var
    CopyBuffer : PChar;
    hCopyBuffer: THandle;
    TotalLength: longint;

  procedure Run(CalcOnly: boolean);
    var
      i: Integer;
      S: ShortString;
      OneLine: TOneELine;
      Percent: Integer;
      TmpComp: comp;

    begin
    for i := 0 to pred(FoundList.Count) do
      begin
      OneLine := TOneELine(FoundList.Objects[i]);
      if (OneLine.ExtAttr and eaSelected <> 0) or
       (i = pred(DrawGrid.Row)) then
        begin
        S := GetPQString(OneLine.Disk);
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        TmpComp := OneLine.DiskSize;
        S := FormatBigSize(TmpComp * 1024);
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        TmpComp := OneLine.DiskFree;
        S := FormatBigSize(TmpComp * 1024);
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        if OneLine.DiskSize > 0
          then Percent := (OneLine.DiskFree * 100) div OneLine.DiskSize
          else Percent := 0;
        S := IntToStr(Percent) + '%';
        AddToBuffer(CalcOnly, S + #13#10, CopyBuffer, TotalLength);
        end;
      end;
    end;

  begin
  TotalLength := 0;
  Run(true);
  ///hCopyBuffer  := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, TotalLength+1);
  ///CopyBuffer   := GlobalLock(hCopyBuffer);
  Run(false);
  ///GlobalUnlock(hCopyBuffer);
  ///Clipboard.SetAsHandle(CF_TEXT, hCopyBuffer);
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.MenuCopyClick(Sender: TObject);

  begin
  MakeCopy;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.MenuSelectAllClick(Sender: TObject);

  begin
  SelectAll;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundEmptyList.MenuUnselectAllClick(Sender: TObject);

  begin
  UnselectAll;
  end;

//---------------------------------------------------------------------------
// implements printing of the list

procedure TFormFoundEmptyList.MakePrint;

  const NoOfColumns = 4;

  var
    AmountPrintedPx: Integer;
    PageCounter: Integer;
    ColWidthsPx: array [0..NoOfColumns-1] of Integer;
         {Disk, Dir, Name, Size, Time, Desc}

  {------}

  function CanPrintOnThisPage: boolean;

    begin
    {$ifdef mswindwos}
    Result := true;
    if PrintDialog.PrintRange <> prPageNums then exit;
    with PrintDialog do
      if (PageCounter >= FromPage) and (PageCounter <= ToPage) then
        exit;
    Result := false;
    {$endif}
    end;


  {------}

  procedure GoToNewPage;

    begin
    {$ifdef mswindows}
    if PrintDialog.PrintRange <> prPageNums
      then
        QPrinterNewPage
      else
        begin
        with PrintDialog do
          if (PageCounter >= FromPage) and (PageCounter < ToPage) then
            QPrinterNewPage;
        end;
    {$endif}
    end;

  {------}

  procedure PrintHeaderAndFooter;

    var
      OutRect : TRect;
      S       : ShortString;

    begin
    if FormAbortPrint.Aborted then exit;
    {$ifdef mswindows}
    QPrinterSaveAndSetFont(pfsItalic);
    {header}
    OutRect.Left   := LeftPrintAreaPx;
    OutRect.Right  := RightPrintAreaPx;
    OutRect.Top    := TopPrintAreaPx;
    OutRect.Bottom := OutRect.Top + LineHeightPx;
    if CanPrintOnThisPage then
      begin
      S := QGlobalOptions.PrintHeader;
      if S = '' then S := Caption;
      QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
      QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 50)); {0.2 mm};
      QPrinterMoveTo(LeftPrintAreaPx,  OutRect.Bottom + YPxPer1mm);
      QPrinterLineTo(RightPrintAreaPx, OutRect.Bottom + YPxPer1mm);
      end;
    {footer}
    OutRect.Bottom := BottomPrintAreaPx;
    OutRect.Top    := OutRect.Bottom - LineHeightPx;
    if CanPrintOnThisPage then
      begin
      QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, TimePrinted);
      S := lsPage + IntToStr(PageCounter);
      OutRect.Left := OutRect.Right - QPrinterGetTextWidth(S);
      QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
      QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 50)); {0.2 mm};
      QPrinterMoveTo(LeftPrintAreaPx,  OutRect.Top - YPxPer1mm);
      QPrinterLineTo(RightPrintAreaPx, OutRect.Top - YPxPer1mm);
      end;
    QPrinterRestoreFont;
    {$endif}
    end;

  {------}

  procedure PrintColumnNames;

    var
      OutRect : TRect;
      S       : ShortString;
      i       : Integer;
      StartX  : Integer;

    begin
    if FormAbortPrint.Aborted then exit;
    {$ifdef mswindows}
    QPrinterSaveAndSetFont(pfsBold);
    OutRect.Left   := LeftPrintAreaPx;
    OutRect.Top    := AmountPrintedPx;
    OutRect.Bottom := OutRect.Top + LineHeightPx;
    for i := 0 to NoOfColumns-1 do
    if CanPrintOnThisPage then
      begin
      OutRect.Right := OutRect.Left + ColWidthsPx[i] - 3*XPxPer1mm;
      if (OutRect.Right > RightPrintAreaPx) then
        OutRect.Right := RightPrintAreaPx;
      if (OutRect.Left < OutRect.Right) then
        begin
        case i of
          0: begin
             S := lsDisk2;
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
             end;
          1: begin
             S := lsSize;
             StartX := OutRect.Right - QPrinterGetTextWidth(S);
             QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
             end;
          2: begin
             S := lsFreeSpace;
             StartX := OutRect.Right - QPrinterGetTextWidth(S);
             QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
             end;
          3: begin
             S := lsFreeSpacePerCent;
             StartX := OutRect.Right - QPrinterGetTextWidth(S);
             QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
             end;
          end;
        end;
      inc(OutRect.Left,ColWidthsPx[i]);
      end;
    inc(AmountPrintedPx, LineHeightPx + YPxPer1mm);
    QPrinterRestoreFont;
    {$endif}
    end;

  {------}

  procedure PrintOneLine(Index: Integer);

    var
      OneLine: TOneELine;
      OutRect : TRect;
      S       : ShortString;
      i       : Integer;
      StartX  : Integer;
      Percent : Integer;
      TmpComp : comp;

    begin
    if FormAbortPrint.Aborted then exit;
    OneLine := TOneELine(FoundList.Objects[Index]);
    {$ifdef mswindows}
    if (PrintDialog.PrintRange = prSelection) and
       (OneLine.ExtAttr and eaSelected = 0) and
       (Index <> pred(DrawGrid.Row))
          then exit;
    OutRect.Left   := LeftPrintAreaPx;
    OutRect.Top    := AmountPrintedPx;
    OutRect.Bottom := OutRect.Top + LineHeightPx;
    for i := 0 to NoOfColumns-1 do
    if CanPrintOnThisPage then
      begin
      OutRect.Right := OutRect.Left + ColWidthsPx[i] - 3*XPxPer1mm;
      if (OutRect.Right > RightPrintAreaPx) or (i = NoOfColumns-1)
        then OutRect.Right := RightPrintAreaPx;
      if OutRect.Left >= OutRect.Right then break;
      case i of
        0: begin
           S := GetPQString(OneLine.Disk);
           QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
           end;
        1: begin
           TmpComp := OneLine.DiskSize;
           S := FormatBigSize(TmpComp * 1024);
           StartX := OutRect.Right - QPrinterGetTextWidth(S);
           QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
           end;
        2: begin
           TmpComp := OneLine.DiskFree;
           S := FormatBigSize(TmpComp * 1024);
           StartX := OutRect.Right - QPrinterGetTextWidth(S);
           QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
           end;
        3: begin
           if OneLine.DiskSize > 0
             then Percent := (OneLine.DiskFree * 100) div OneLine.DiskSize
             else Percent := 0;
           S := IntToStr(Percent) + '%';
           StartX := OutRect.Right - QPrinterGetTextWidth(S);
           QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
           end;
        end;
      inc(OutRect.Left,ColWidthsPx[i]);
      end;
    inc(AmountPrintedPx, LineHeightPx);
    {$endif}
    end;

  {------}

  var
    i: Integer;
    Ratio: double;
    TmpS: array[0..256] of char;
    Copies: Integer;

  begin
  {$ifdef mswindows}
  if PrintDialog.Execute then
    begin
    QPrinterReset(QGlobalOptions);
    AmountPrintedPx    := TopPrintAreaPx + 2*LineHeightPx;

    Ratio := QPrinterGetTextWidth('M') / DrawGrid.Canvas.TextWidth('M');
    for i := 0 to NoOfColumns-1 do
      ColWidthsPx[i] := round(DrawGrid.ColWidths[i] * Ratio) + 3*XPxPer1mm;

    FormAbortPrint.LabelProgress.Caption := lsPreparingToPrint;
    FormAbortPrint.Show;
    MainForm.Enabled :=false;
    try
      Application.ProcessMessages;
      for Copies := 1 to PrintDialog.Copies do
        begin
        PageCounter := 1;
        QPrinterBeginDoc(lsQuickDir);
        PrintHeaderAndFooter;
        PrintColumnNames;
        FormAbortPrint.LabelProgress.Caption :=
          lsPrintingPage + IntToStr(PageCounter);
        for i := 0 to pred(FoundList.Count) do
          begin
          Application.ProcessMessages;
          if FormAbortPrint.Aborted then break;
          PrintOneLine(i);
          if (AmountPrintedPx + 3*LineHeightPx) > BottomPrintAreaPx then
            begin
            if not FormAbortPrint.Aborted then
              begin
              GoToNewPage;
              inc(PageCounter);
              end;
            FormAbortPrint.LabelProgress.Caption :=
              lsPrintingPage + IntToStr(PageCounter);
            Application.ProcessMessages;
            AmountPrintedPx  := TopPrintAreaPx + 2*LineHeightPx;
            PrintHeaderAndFooter;
            PrintColumnNames;
            end;
          end;
        QPrinterEndDoc;
        if FormAbortPrint.Aborted then break;
        end;
    except
      on E: Exception do Application.MessageBox(StrPCopy(TmpS, E.Message), lsError,
          mb_Ok or mb_IconExclamation);
        end;
    MainForm.Enabled :=true;
    FormAbortPrint.Hide;
    end;
  {$endif}
  end;

//---------------------------------------------------------------------------
// Calls the FormFoundExport for exporting to a text format

procedure TFormFoundEmptyList.ExportToOtherFormat;

  begin
  FormFoundExport.IsFileFoundList := false;
  FormFoundExport.FoundList       := FoundList;
  FormFoundExport.DBaseHandle     := DBaseWindow.DBaseHandle;
  FormFoundExport.DBaseFileName   := DBaseWindow.DBaseFileName;
  FormFoundExport.ShowModal;
  end;

//---------------------------------------------------------------------------



end.
