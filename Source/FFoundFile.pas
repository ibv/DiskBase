unit FFoundFile;
(*====================================================================
MDI window with found files
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, Menus, PrintersDlgs,
  UTypes, UApiTypes, UCollections, FSettings, FMain, {FDBase,} UStringList;

type
  TOneFLine = class    // class for one found file (=line)
    POneFile : TPOneFile;
    Disk     : TPQString;
    Dir      : TPQString;
    ShortDesc: TPQString;
    Id       : longint;
    ExtAttr  : byte;
    constructor Create(aOneFile: TOneFile; var aDisk, aDir, aShortDesc: ShortString;
                       aID: longint; AExtAttr: byte);
    destructor  Destroy; override;
    end;


  { TFormFoundFileList }

  TFormFoundFileList = class(TForm)
    DrawGrid: TDrawGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu: TPopupMenu;
    MenuGoTo: TMenuItem;
    MenuPrint: TMenuItem;
    MenuHelp: TMenuItem;
    MenuCopy: TMenuItem;
    MenuSelectAll: TMenuItem;
    N1: TMenuItem;
    MenuUnselectAll: TMenuItem;
    N2: TMenuItem;
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
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuPrintClick(Sender: TObject);
    procedure MenuHelpClick(Sender: TObject);
    procedure DrawGridSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure DrawGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuUnselectAllClick(Sender: TObject);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    ///FoundList          : TQStringList;
    FoundList          : TStringList;
    TimeWidth          : Integer;
    SortArr            : TSortArr;
    NeedReSort         : boolean; {if SortArr changes, resort in idle time}
    ReversedSort       : boolean;
    CanDraw            : boolean;
    FormIsClosed       : boolean;
    LastFileSelected   : Integer;
    MakeSelection      : boolean;
    MaxFileNameLength  : Integer; {max. delka jmena v akt. seznamu}
    MaxFileNamePxLength: Integer;
    LastMouseMoveTime  : longint;
    FilesHintDisplayed : boolean;
    LastMouseXFiles    : integer;
    LastMouseYFiles    : integer;
    LastHintRect       : TRect;
    DisableHints       : integer;
    DisableNextDoubleClick: boolean;
    MousePosAlreadyChecked: boolean;
    function  GetSortString(OneLine: TOneFLine): ShortString;
    procedure ResortFoundList;
    procedure ResetFontAndSize;
    procedure SetFileColWidth;
    procedure ShowFileHint;
    procedure EraseFileHint;
    function  FindOneLine(Point: TPoint; var Rect: TRect): TOneFLine;
  public
    QGlobalOptions: TGlobalOptions;
    ///DBaseWindow: TFormDBase;
    DBaseWindow: TMainForm;
    procedure GetList(DBaseHandle: PDBaseHandle);
    procedure LocalIdle;
    procedure LocalTimer;
    procedure SetNeedResort(Sort: TSort);
    procedure ChangeGlobalOptions;
    procedure SelectAll;
    procedure UnselectAll;
    procedure MakeCopy;
    procedure MakePrint;
    procedure ExportToOtherFormat;

  end;

//---------------------------------------------------------------------------

implementation

uses
  Clipbrd, {Printers,}
  UExceptions, UApi, UBaseUtils, FFindFileDlg, FProgress, FAbortPrint,
  {FMain,} ULang, UPrinter, FHidden,
{$ifdef LOGFONT}
  UFont,
{$endif}
  UExport, FFoundExport;

{$R *.dfm}

const
  ClipboardLimit = 32 * 1024 - 10;

{---TOneFLine---------------------------------------------------------}

constructor TOneFLine.Create(aOneFile: TOneFile; var aDisk, aDir, aShortDesc: ShortString;
                            aID: longint; AExtAttr: byte);

  begin
  GetMemOneFile(POneFile, aOneFile);
  Disk      := QNewStr(aDisk);
  Dir       := QNewStr(aDir);
  ShortDesc := QNewStr(aShortDesc);
  ID        := aID;
  ExtAttr   := AExtAttr;
  end;

//---------------------------------------------------------------------------

destructor TOneFLine.Destroy;

  begin
  FreeMemOneFile(POneFile);
  QDisposeStr(Disk);
  QDisposeStr(Dir);
  QDisposeStr(ShortDesc);
  end;

//---TFormFoundList----------------------------------------------------------

procedure TFormFoundFileList.FormClose(Sender: TObject;
  var Action: TCloseAction);

  begin
  if FormIsClosed then Action := caFree;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

  begin
  CanClose := true;
  FormIsClosed := true;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.SetFileColWidth;

  begin
  DrawGrid.ColWidths[2] := MaxFileNamePxLength + 5;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.GetList(DBaseHandle: PDBaseHandle);

  var
    OneFile: TOneFile;
    Disk, Dir, ShortDesc: ShortString;
    OneLine: TOneFLine;
    Index  : Integer;
    Total  : Integer;
    PercentPie  : Integer;
    ShowProgress: boolean;
    TmpInt      : Integer;

  begin
  FoundList.Clear;
  MaxFileNameLength := 12;
  MaxFileNamePxLength := 50;
  try
    // if there is a large number of items, we should show a progress window
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
        OneLine := TOneFLine.Create(OneFile, Disk, Dir, ShortDesc, Index, 0);
        FoundList.AddObject(GetSortString(OneLine), OneLine);
        if ShowProgress and FormProgress.StopIt then break;
        TmpInt := length(OneFile.LongName) + length(OneFile.Ext);
        if TmpInt > MaxFileNameLength then MaxFileNameLength := TmpInt;
        TmpInt := DrawGrid.Canvas.TextWidth(OneFile.LongName+OneFile.Ext);
        if TmpInt > MaxFileNamePxLength then MaxFileNamePxLength := TmpInt;
        end;
      end;
    DrawGrid.RowCount := FoundList.Count + 1;
    CanDraw := true;
    if ShowProgress then FormProgress.Hide;
    QI_ClearFoundList(DBaseHandle);
    SetFileColWidth;
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

procedure TFormFoundFileList.FormCreate(Sender: TObject);

  begin
  FormIsClosed := false;
  ///FoundList := TQStringList.Create;
  FoundList := TStringList.Create;
  FoundList.Sorted := true;
  ///FoundList.Duplicates := qdupAccept;
  FoundList.Duplicates := dupAccept;
  SortArr := FormSearchFileDlg.DlgData.SortArr;
  NeedResort := false;
  ReversedSort := false;
  QGlobalOptions := TGlobalOptions.Create;
  FormSettings.GetOptions(QGlobalOptions);
  ResetFontAndSize;
  CanDraw := false;
  LastFileSelected := 1;
  MakeSelection := false;
  MaxFileNameLength := 12;
  MaxFileNamePxLength := 50;
  LastMouseMoveTime := $0FFFFFFF;
  FilesHintDisplayed := false;
  DisableHints := 0;
  SetRectEmpty(LastHintRect);
  DisableNextDoubleClick := false;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.DrawGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);

  var
    S: ShortString;
    StartX     : Integer;
    DosDateTime: longint;
    PartRect   : TRect;
    OneLine    : TOneFLine;
    Size       : longint;
    Width      : Integer;
    CutIt      : boolean;

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
            OneLine := TOneFLine(FoundList.Objects[Row-1]);
            if (OneLine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top,
              GetPQString(OneLine.Disk));
            end;
      1:
        if Row = 0
          then
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top, lsFolder3)
          else
            begin
            OneLine := TOneFLine(FoundList.Objects[Row-1]);
            if (OneLine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            S := GetPQString(OneLine.Dir);
            Width := DrawGrid.Canvas.TextWidth(S);
            if Width > (Rect.Right - Rect.Left - 2)
              then
                begin
                StartX := Rect.Right - Width - 2;
                CutIt := true;
                end
              else
                begin
                StartX := Rect.Left + 2;
                CutIt := false;
                end;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            if CutIt then
              begin
              S := '< ';
              Width := DrawGrid.Canvas.TextWidth(S);
              DrawGrid.Canvas.TextRect(
                Classes.Rect(Rect.Left, Rect.Top, Rect.Left + Width + 2, Rect.Bottom),
                Rect.Left+2, Rect.Top, '< ')
              end;
            end;
      2:
        if Row = 0
          then
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top, lsFile)
          else
            begin
            OneLine := TOneFLine(FoundList.Objects[Row-1]);
            if (OneLine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top,
              OneLine.POneFile^.LongName + OneLine.POneFile^.Ext);
            end;
      3:
        begin
        if Row = 0
          then
            begin
            S := lsSize;
            StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end
          else
            begin
            OneLine := TOneFLine(FoundList.Objects[Row-1]);
            if (OneLine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            Size := OneLine.POneFile^.Size;
            if OneLine.POneFile^.Attr and faDirectory = faDirectory
              then S := lsFolder
              else
                if (OneLine.POneFile^.Size=0) and (OneLine.POneFile^.Time=0)
                  then S := lsDisk3
                  else S := FormatSize(Size, QGlobalOptions.ShowInKb);
            StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end;
        end;
      4:
        begin
        if Row = 0
          then
            begin
            S := lsDateTime;
            StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
            DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
            end
          else
            begin
            OneLine := TOneFLine(FoundList.Objects[Row-1]);
            if (OneLine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DrawGrid.Canvas.FillRect(Rect);
            DosDateTime := OneLine.POneFile^.Time;
            if DosDateTime <> 0
              then
                begin
                S := DosTimeToStr(DosDateTime, QGlobalOptions.ShowSeconds);
                StartX := Rect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
                DrawGrid.Canvas.TextRect(Rect, StartX, Rect.Top, S);
                S := DosDateToStr(DosDateTime);
                PartRect := Rect;
                PartRect.Right := PartRect.Right - TimeWidth;
                if PartRect.Right < PartRect.Left then PartRect.Right := PartRect.Left;
                StartX := PartRect.Right - DrawGrid.Canvas.TextWidth(S) - 2;
                DrawGrid.Canvas.TextRect(PartRect, StartX, Rect.Top, S);
                end
              else
                DrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top, '');
            end;
        if Row > 0 then
        end;
      5:
        if Row = 0
          then
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top, lsDescription)
          else
            begin
            OneLine := TOneFLine(FoundList.Objects[Row-1]);
            if (OneLine.ExtAttr and eaSelected <> 0)
              and not(gdSelected in State) then
                begin
                DrawGrid.Canvas.Brush.Color := clQSelectedBack;
                DrawGrid.Canvas.Font.Color  := clQSelectedText;
                end;
            DrawGrid.Canvas.FillRect(Rect);
            DrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top,
              GetPQString(OneLine.ShortDesc));
            end;
      end;
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.FormDestroy(Sender: TObject);

  begin
  FoundList.Free;
  QGlobalOptions.Free;
  end;

//---------------------------------------------------------------------------
// returns the string for sorting the lines

function TFormFoundFileList.GetSortString(OneLine: TOneFLine): ShortString;

  var
    TmpL : longint;
    Index: Integer;

  begin
  Result := '';
  for Index := 1 to 3 do
    with OneLine do
      begin
      case SortArr[Index] of
        soName:
          begin
          Result := Result + POneFile^.LongName + ' ' + POneFile^.Ext;
          end;
        soExt:
          begin
          Result := Result + POneFile^.Ext + ' ' + POneFile^.LongName;
          end;
        soTime:
          begin
          TmpL := MaxLongInt - POneFile^.Time;
          Result := Result + Format('%10.10d', [TmpL]) + POneFile^.Ext + POneFile^.LongName;
          end;
        soSize:
          begin
          TmpL := MaxLongInt - POneFile^.Size;
          Result := Result + Format('%10.10d', [TmpL]) + POneFile^.Ext + POneFile^.LongName;
          end;
        soKey:
          begin
          Result := Result + GetPQString(Disk) + ' ' + GetPQString(Dir);
          end;
        soDir:
          begin
          Result := Result + GetPQString(Dir) + ' ' + GetPQString(Disk);
          end;
        end;
      end;
  end;

//---------------------------------------------------------------------------
// called from the main form when there is nothing better to do

procedure TFormFoundFileList.LocalIdle;

  begin
  if NeedReSort then
    begin
    NeedReSort := false;
    ResortFoundList;
    end;
  end;

//---------------------------------------------------------------------------
// called from the main form

procedure TFormFoundFileList.LocalTimer;

  begin
  if FormIsClosed then exit;
  if QGlobalOptions.ShowFileHints and
    ((longint(GetTickCount) - LastMouseMoveTime) > defHintDelayPeriod) then
      ShowFileHint;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.SetNeedResort(Sort: TSort);

  begin
  if Sort <> SortArr[1]
    then
      begin
      SortArr[3] := SortArr[2];
      SortArr[2] := SortArr[1];
      SortArr[1] := Sort;
      ReversedSort := false;
      end
    else
      ReversedSort := not ReversedSort;
  NeedResort := true;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  var
    i: Integer;
    OneColWidth, TotalWidth: Integer;

  begin
  DisableNextDoubleClick := false;
  with DrawGrid do
    if Button = mbLeft then
      begin
      if Y < DefaultRowHeight then
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
              0: SetNeedResort(soKey);
              1: SetNeedResort(soDir);
              2: if X > TotalWidth - OneColWidth div 2
                  then SetNeedResort(soExt)
                  else SetNeedResort(soName);
              3: SetNeedResort(soSize);
              4: SetNeedResort(soTime);
              end;
            break;
            end;
          end;
        exit;
        end;

      if LastFileSelected >= DrawGrid.RowCount then
        LastFileSelected := pred(DrawGrid.RowCount);
      if ssShift in Shift then
        begin
        for i := 1 to pred(DrawGrid.RowCount) do
          begin
          with TOneFLine(FoundList.Objects[i-1]) do
            ExtAttr := ExtAttr and not eaSelected;
          end;
        if LastFileSelected <= DrawGrid.Row
          then
            for i := LastFileSelected to DrawGrid.Row do
              begin
              with TOneFLine(FoundList.Objects[i-1]) do
                if ExtAttr and eaSelected <> 0
                  then ExtAttr := ExtAttr and not eaSelected
                  else ExtAttr := ExtAttr or eaSelected;
              end
          else
            for i := LastFileSelected downto DrawGrid.Row do
              begin
              with TOneFLine(FoundList.Objects[i-1]) do
                if ExtAttr and eaSelected <> 0
                  then ExtAttr := ExtAttr and not eaSelected
                  else ExtAttr := ExtAttr or eaSelected;
              end;
        DrawGrid.Repaint;
        end;
      if ssCtrl in Shift then
        begin
        with TOneFLine(FoundList.Objects[DrawGrid.Row-1]) do
          if ExtAttr and eaSelected <> 0
            then ExtAttr := ExtAttr and not eaSelected
            else ExtAttr := ExtAttr or eaSelected;
        end;
      end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.ResortFoundList;

  var
    ///NewFoundList  : TQStringList;
    NewFoundList  : TStringList;
    Index         : Integer;
    OneLine       : TOneFLine;
    OneFile       : TOneFile;
    ADisk, ADir, AShortDesc: ShortString;
    Total         : Integer;
    PercentPie    : Integer;
    ShowProgress  : boolean;
    SaveID        : longint;

  begin
  SaveID := TOneFLine(FoundList.Objects[DrawGrid.Row-1]).ID;
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
    with TOneFLine(FoundList.Objects[Index]) do
      begin
      if ShowProgress and ((Index mod PercentPie) = 0) then
        FormProgress.SetProgress((longint(Index) * 100) div Total);
      MoveOneFile(POneFile^, OneFile);
      ADisk := GetPQString(Disk);
      ADir  := GetPQString(Dir);
      AShortDesc := GetPQString(ShortDesc);
      OneLine := TOneFLine.Create(OneFile, ADisk, ADir, AShortDesc, ID, ExtAttr);
      NewFoundList.AddObject(GetSortString(OneLine), OneLine);
      if ShowProgress and FormProgress.StopIt then break;
      end;
  if ShowProgress and FormProgress.StopIt
    then
      begin
      FreeObjects(NewFoundList);
      NewFoundList.Free;
      end
    else
      begin
      FoundList.Free;
      FreeObjects(FoundList);
      FoundList := NewFoundList;
      for Index := 0 to pred(Total) do
        if SaveID = TOneFLine(FoundList.Objects[Index]).ID then
          begin
          DrawGrid.Row := Index + 1;
          break;
          end;
      end;
  if ShowProgress then FormProgress.Hide;
  DrawGrid.Refresh;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.ResetFontAndSize;

  begin
  with DrawGrid do
    begin
    {$ifndef LOGFONT}
    ///Font.Assign       (QGlobalOptions.FoundFont);
    ///Canvas.Font.Assign(QGlobalOptions.FoundFont);
    {$else}
    SetFontFromLogFont(Font, QGlobalOptions.FoundLogFont);
    SetFontFromLogFont(Canvas.Font, QGlobalOptions.FoundLogFont);
    {$endif}
    DefaultRowHeight := Canvas.TextHeight('My');
    TimeWidth := Canvas.TextWidth(DosTimeToStr(longint(23) shl 11,
                                                QGlobalOptions.ShowSeconds));
    TimeWidth := TimeWidth + TimeWidth div 6;

    ColWidths[0] := Canvas.TextWidth('Mmmmxxxx.mxx') + 2;
    ColWidths[1] := 2 * Canvas.TextWidth('Mmmmmxxxxx ');
    SetFileColWidth;
    ColWidths[3] := Canvas.TextWidth(FormatSize(2000000000,
                                          QGlobalOptions.ShowInKb)) + 2;
    if QGlobalOptions.ShowTime
      then ColWidths[4] := Canvas.TextWidth(' ' + DosDateToStr (694026240)) + TimeWidth + 2
      else ColWidths[4] := Canvas.TextWidth(' ' + DosDateToStr (694026240)) + 2;
    ColWidths[5] := 25 * Canvas.TextWidth('Mmmmxxxxx ');
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.ChangeGlobalOptions;

  begin
  FormSettings.GetOptions(QGlobalOptions);
  ResetFontAndSize;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.DrawGridDblClick(Sender: TObject);

  var
    LongName: ShortString;

  begin
  if DisableNextDoubleClick then exit;
  with TOneFLine(FoundList.Objects[DrawGrid.Row-1]) do
    begin
    LongName := POneFile^.LongName + POneFile^.Ext;
    DBaseWindow.BringToFront;
    DBaseWindow.JumpTo(GetPQString(Disk), GetPQString(Dir), LongName);
    DBaseWindow.PageControl1.TabIndex:=0;
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.MenuGoToClick(Sender: TObject);

  begin
  DrawGridDblClick(Sender);
  end;

procedure TFormFoundFileList.MenuItem2Click(Sender: TObject);
begin
  close;
end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.DrawGridSelectCell(Sender: TObject; Col,
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
          with TOneFLine(FoundList.Objects[i-1]) do
            if ExtAttr and eaSelected <> 0
              then ExtAttr := ExtAttr and not eaSelected
              else ExtAttr := ExtAttr or eaSelected;
          end
      else
        for i := LastFileSelected downto Row+1 do
          begin
          with TOneFLine(FoundList.Objects[i-1]) do
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

procedure TFormFoundFileList.DrawGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);

  begin
  if (Shift = []) and ((Key = vk_Insert) or (Key = vk_Space)) then
    begin
    with TOneFLine(FoundList.Objects[pred(DrawGrid.Row)]) do
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

procedure TFormFoundFileList.SelectAll;

  var
    i: Integer;
    AlreadySelected: boolean;

  begin
  if UsePersistentBlocks
    then
      begin
      AlreadySelected := true;
      for i := 0 to pred(FoundList.Count) do
        with TOneFLine(FoundList.Objects[i]) do
          if (ExtAttr and eaSelected) = 0 then
            begin
            AlreadySelected := false;
            break;
            end;
      if AlreadySelected
        then
          for i := 0 to pred(FoundList.Count) do
            with TOneFLine(FoundList.Objects[i]) do
              ExtAttr := ExtAttr and not eaSelected
        else
          for i := 0 to pred(FoundList.Count) do
            with TOneFLine(FoundList.Objects[i]) do
              ExtAttr := ExtAttr or eaSelected;
      end
    else
      begin
      for i := 0 to pred(FoundList.Count) do
        with TOneFLine(FoundList.Objects[i]) do
          ExtAttr := ExtAttr or eaSelected;
      end;
  DrawGrid.Repaint;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.UnselectAll;

  var
    i: Integer;

  begin
  for i := 0 to pred(FoundList.Count) do
    begin
    with TOneFLine(FoundList.Objects[i]) do
      ExtAttr := ExtAttr and not eaSelected;
    end;
  DrawGrid.Repaint;
  end;

//---------------------------------------------------------------------------
// copies selected lines to clipboard as text

procedure TFormFoundFileList.MakeCopy;

  var
    CopyBuffer : PChar;
    hCopyBuffer: THandle;
    TotalLength: longint;

  procedure Run(CalcOnly: boolean);
    var
      i: Integer;
      S: ShortString;
      OneLine: TOneFLine;

    begin
    for i := 0 to pred(FoundList.Count) do
      begin
      OneLine := TOneFLine(FoundList.Objects[i]);
      if (OneLine.ExtAttr and eaSelected <> 0) or
       (i = pred(DrawGrid.Row)) then
        begin
        S := GetPQString(OneLine.Disk);
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        S := GetPQString(OneLine.Dir);
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        S := OneLine.POneFile^.LongName + OneLine.POneFile^.Ext;
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        if OneLine.POneFile^.Attr and faDirectory = faDirectory
          then S := lsFolder
          else
            if (OneLine.POneFile^.Size=0) and (OneLine.POneFile^.Time=0)
              then S := lsDisk3
              else S := FormatSize(OneLine.POneFile^.Size, QGlobalOptions.ShowInKb);
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        if OneLine.POneFile^.Time <> 0
          then
            S := DosDateToStr(OneLine.POneFile^.Time) + #9 +
                 DosTimeToStr(OneLine.POneFile^.Time, QGlobalOptions.ShowSeconds)
          else
            S := #9;
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);

        S := GetPQString(OneLine.ShortDesc);
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

procedure TFormFoundFileList.MenuCopyClick(Sender: TObject);

  begin
  MakeCopy;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.MenuSelectAllClick(Sender: TObject);

  begin
  SelectAll;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.MenuUnselectAllClick(Sender: TObject);

  begin
  UnselectAll;
  end;

//---------------------------------------------------------------------------
// implements printing of the lines

procedure TFormFoundFileList.MakePrint;

  const NoOfColumns = 6;

  var
    AmountPrintedPx: Integer;
    PageCounter: Integer;
    ColWidthsPx: array [0..NoOfColumns-1] of Integer;
    TimeWidthPx: Integer;
         {Disk, Dir, Name, Size, Time, Desc}

  {------}

  function CanPrintOnThisPage: boolean;

    begin
    Result := true;
    {$ifdef mswindows}
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
      if (OutRect.Right > RightPrintAreaPx) or (i = NoOfColumns-1)
        then OutRect.Right := RightPrintAreaPx;
      if (OutRect.Left < OutRect.Right) then
        begin
        case i of
          0: begin
             S := lsDisk2;
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
             end;
          1: begin
             S := lsFolder3;
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
             end;
          2: begin
             S := lsFile;
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
             end;
          3: begin
             S := lsSize;
             StartX := OutRect.Right - QPrinterGetTextWidth(S);
             QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
             end;
          4: begin
             S := lsDateTime;
             StartX := OutRect.Right - QPrinterGetTextWidth(S);
             QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
             end;
          5: begin
             S := lsDescription;
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
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

  function CutStringToDesiredWidth (var S: ShortString; DesiredPxWidth: Integer): Integer;
  // cuts the string so that it has limited length
    var
      Len, PxWidth, EstimatedCut: Integer;
      TmpS: ShortString;


    begin
    ///PxWidth  := QPrinterGetTextWidth(S);
    Len      := length(S);
    Result   := DesiredPxWidth;
    if PxWidth <= 0 then exit;
    if DesiredPxWidth <= 0 then exit;
    if PxWidth <= DesiredPxWidth then exit;
    EstimatedCut := (longint(Len) * DesiredPxWidth) div PxWidth;
    TmpS := ShortCopy(S, Len-EstimatedCut+1, EstimatedCut);
    ///PxWidth := QPrinterGetTextWidth(TmpS);
    if PxWidth > DesiredPxWidth then
      begin
      while PxWidth > DesiredPxWidth do
        begin
        dec(EstimatedCut);
        if EstimatedCut = 0 then exit;
        TmpS := ShortCopy(S, Len-EstimatedCut+1, EstimatedCut);
        ///PxWidth := QPrinterGetTextWidth(TmpS);
        end;
      if EstimatedCut >= 2 then dec(EstimatedCut, 2);
      S := ShortCopy(S, Len-EstimatedCut+1, EstimatedCut);
      ///PxWidth := QPrinterGetTextWidth(S);
      Result := PxWidth;
      exit;
      end;
    if PxWidth <= DesiredPxWidth then
      begin
      while PxWidth <= DesiredPxWidth do
        begin
        inc(EstimatedCut);
        if EstimatedCut > Len then exit;
        TmpS := ShortCopy(S, Len-EstimatedCut+1, EstimatedCut);
        ///PxWidth := QPrinterGetTextWidth(TmpS);
        end;
      if EstimatedCut >=3 then dec(EstimatedCut, 3);
      S := ShortCopy(S, Len-EstimatedCut+1, EstimatedCut);
      ///PxWidth := QPrinterGetTextWidth(S);
      Result := PxWidth;
      exit;
      end;
    end;

  {------}

  procedure PrintOneLine(Index: Integer);

    const
      ArrowS = '« ';

    var
      OneLine: TOneFLine;
      OutRect : TRect;
      TmpRect : TRect;
      S       : ShortString;
      i       : Integer;
      StartX  : Integer;
      PxWidth   : Integer;
      DesiredPxWidth: Integer;
      CutIt   : boolean;

    begin
    if FormAbortPrint.Aborted then exit;
    OneLine := TOneFLine(FoundList.Objects[Index]);
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
           S := GetPQString(OneLine.Dir);
           PxWidth := QPrinterGetTextWidth(S);
           DesiredPxWidth := OutRect.Right - OutRect.Left;
           TmpRect := OutRect;
           if PxWidth > DesiredPxWidth
             then
               begin
               PxWidth := CutStringToDesiredWidth (S, DesiredPxWidth);
               StartX := OutRect.Right - PxWidth;
               CutIt := true;
               end
             else
               begin
               StartX := OutRect.Left;
               CutIt := false;
               end;
           if CutIt then
             begin
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, ArrowS);
             end;
           QPrinterTextRect(TmpRect, StartX, OutRect.Top, S);
           end;
        2: begin
           S := OneLine.POneFile^.LongName + OneLine.POneFile^.Ext;
           QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
           end;
        3: begin
           if OneLine.POneFile^.Attr and faDirectory = faDirectory
             then S := lsFolder
             else
               if (OneLine.POneFile^.Size=0) and (OneLine.POneFile^.Time=0)
                 then S := lsDisk3
                 else S := FormatSize(OneLine.POneFile^.Size, QGlobalOptions.ShowInKb);
           StartX := OutRect.Right - QPrinterGetTextWidth(S);
           QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
           end;
        4: begin
           if OneLine.POneFile^.Time <> 0 then
             begin
             S := DosTimeToStr(OneLine.POneFile^.Time, QGlobalOptions.ShowSeconds);
             StartX := OutRect.Right - QPrinterGetTextWidth(S);
             QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
             S := DosDateToStr(OneLine.POneFile^.Time);
             TmpRect := OutRect;
             TmpRect.Right := TmpRect.Right - TimeWidthPx;
             if TmpRect.Right > TmpRect.Left then
               begin
               StartX := TmpRect.Right - QPrinterGetTextWidth(S);
               QPrinterTextRect(TmpRect, StartX, TmpRect.Top, S);
               end;
             end;
           end;
        5: begin
           S := GetPQString(OneLine.ShortDesc);
           QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
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

    Ratio := QPrinterGetTextWidth('MMMAaBbCcIi') / DrawGrid.Canvas.TextWidth('MMMAaBbCcIi');
    for i := 0 to NoOfColumns-1 do
      ColWidthsPx[i] := round(DrawGrid.ColWidths[i] * Ratio) + 3*XPxPer1mm;
    TimeWidthPx := QPrinterGetTextWidth(DosTimeToStr(longint(23) shl 11,
                                          QGlobalOptions.ShowSeconds));
    TimeWidthPx := TimeWidthPx + TimeWidthPx div 6;

    FormAbortPrint.LabelProgress.Caption := lsPreparingToPrint;
    FormAbortPrint.Show;
    MainForm.Enabled :=false;
    try
      Application.ProcessMessages;
      for Copies := 1 to PrintDialog.Copies do
        begin
        PageCounter := 1;
        QPrinterBeginDoc (lsQuickDir);
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

procedure TFormFoundFileList.MenuPrintClick(Sender: TObject);

  begin
  MakePrint;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.MenuHelpClick(Sender: TObject);

  begin
  Application.HelpContext(250);
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.DrawGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

  begin
  if (LastMouseXFiles <> X) or (LastMouseYFiles <> Y) then
    begin
    MousePosAlreadyChecked := false;
    EraseFileHint;
    LastMouseXFiles := X;
    LastMouseYFiles := Y;
    LastMouseMoveTime := GetTickCount;
    end;
  end;

//---------------------------------------------------------------------------
// shows the yellow bubble hint

procedure TFormFoundFileList.ShowFileHint;

  var
    Point      : TPoint;
    Rect       : TRect;
    HintWidth  : integer;
    LineHeight : integer;
    OneLine    : TOneFLine;
    S          : ShortString;
    i, TmpInt  : integer;
    PolygonPts : array[0..2] of TPoint;
    XPosition  : integer;

  begin
  if MousePosAlreadyChecked then exit;
  if DisableHints <> 0 then exit;
  if FilesHintDisplayed then exit;
  MousePosAlreadyChecked := true;
  GetCursorPos(Point);
  Point := DrawGrid.ScreenToClient(Point);
  // is the cursor still in the window?
  if (LastMouseXFiles <> Point.X) or (LastMouseYFiles <> Point.Y) then exit;

  OneLine := FindOneLine(Point, Rect);
  if OneLine = nil then exit;
  HiddenForm.MemoForHints.Lines.Clear;

  S := GetPQString(OneLine.ShortDesc);

  if length(S) > 0 then
    HiddenForm.MemoForHints.Lines.Insert(0, S);

  if HiddenForm.MemoForHints.Lines.Count = 0 then exit;

  with DrawGrid.Canvas do
    begin
    HintWidth   := 0;
    LineHeight  := 0;
    for i := 0 to pred(HiddenForm.MemoForHints.Lines.Count) do
      begin
      TmpInt := TextWidth(HiddenForm.MemoForHints.Lines[i]);
      if HintWidth < TmpInt then HintWidth := TmpInt;
      TmpInt := TextHeight(HiddenForm.MemoForHints.Lines[i]);
      if LineHeight < TmpInt then LineHeight := TmpInt;
      end;

    XPosition := (Rect.Left + Rect.Right) div 2;
    LastHintRect.Left   := XPosition - HintWidth + 5;
    LastHintRect.Right  := XPosition + 11;
    if (LastHintRect.Left <= 0) then
      OffsetRect(LastHintRect, -LastHintRect.Left+3, 0);
    PolygonPts[0].X := LastHintRect.Right - 17;
    PolygonPts[1].X := LastHintRect.Right - 11;
    PolygonPts[2].X := LastHintRect.Right - 5;

    if Rect.Top < (DrawGrid.Height div 2)
      then
        begin
        LastHintRect.Top    := Rect.Bottom + 3;
        LastHintRect.Bottom := Rect.Bottom + HiddenForm.MemoForHints.Lines.Count * LineHeight + 5;
        PolygonPts[0].Y := LastHintRect.Top;
        PolygonPts[1].Y := LastHintRect.Top - 6;
        PolygonPts[2].Y := LastHintRect.Top;
        end
      else
        begin
        LastHintRect.Top    := Rect.Top - HiddenForm.MemoForHints.Lines.Count * LineHeight - 5;
        LastHintRect.Bottom := Rect.Top - 3;
        PolygonPts[0].Y := LastHintRect.Bottom-1;
        PolygonPts[1].Y := LastHintRect.Bottom + 5;
        PolygonPts[2].Y := LastHintRect.Bottom-1;
        end;

    Brush.Color := $CFFFFF;
    Font.Color  := clBlack;
    Pen.Color   := clBlack;
    RoundRect(LastHintRect.Left, LastHintRect.Top,
              LastHintRect.Right, LastHintRect.Bottom, 4, 4);

    Polygon(PolygonPts);

    Pen.Color   := $CFFFFF;
    MoveTo(PolygonPts[0].X+1, PolygonPts[0].Y);
    LineTo(PolygonPts[2].X, PolygonPts[2].Y);
    for i := 0 to pred(HiddenForm.MemoForHints.Lines.Count) do
      TextOut(LastHintRect.Left+3, LastHintRect.Top+1+i*LineHeight,
              HiddenForm.MemoForHints.Lines[i]);
    end;
  FilesHintDisplayed := true;
  end;

//---------------------------------------------------------------------------
// finds the line according to its coordinates

function  TFormFoundFileList.FindOneLine(Point: TPoint; var Rect: TRect): TOneFLine;

  var
    ACol, ARow: longint;

  begin
  DrawGrid.MouseToCell(Point.X, Point.Y, ACol, ARow);
  Rect := DrawGrid.CellRect(5, ARow);
  if IsRectEmpty(Rect) then
    begin
    Rect := DrawGrid.CellRect(ACol, ARow);
    Rect.Left := 0;
    Rect.Right := DrawGrid.Width;
    end;
  if (ARow > 0) and (ARow <= FoundList.Count) then
    begin
    Result := TOneFLine(FoundList.Objects[ARow-1]);
    exit;
    end;
  Result := nil;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.EraseFileHint;

  begin
  if FilesHintDisplayed then
    begin
    FilesHintDisplayed := false;
    InflateRect(LastHintRect, 0, 6);
    InvalidateRect(DrawGrid.Handle, @LastHintRect, false);
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormFoundFileList.ExportToOtherFormat;

  begin
  FormFoundExport.IsFileFoundList := true;
  FormFoundExport.FoundList       := FoundList;
  FormFoundExport.DBaseHandle     := DBaseWindow.DBaseHandle;
  FormFoundExport.DBaseFileName   := DBaseWindow.DBaseFileName;
  FormFoundExport.ShowModal;
  end;

//---------------------------------------------------------------------------

end.
