unit FDiskPrint;
(*====================================================================
Progress window for printing disks, implements the printing in the
Run procedure
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages, ExtCtrls, PrintersDlgs,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, {Gauges,}
  UTypes, UAPiTypes, FSettings, UCollections, UStringList;

type
  TFormDiskPrint = class(TForm)
    ButtonCancel: TButton;
    LabelSearched: TLabel;
    Label1: TLabel;
    ///Gauge: TGauge;
    Gauge: TPanel;
    PrintDialog: TPrintDialog;
    LabelWhat: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CanRun     : boolean;
    QGlobalOptions: TGlobalOptions;
    FilesList  : TQStringList; {for capturing and sorting files}
    function  CanPrintOnThisPage: boolean;
    procedure GoToNewPage;
    procedure PrintHeaderAndFooter;
    procedure PrintColumnNames;
    procedure CheckAmountPrinted;
  public
    StopIt     : boolean;
    DBaseHandle: PDBaseHandle;
    ShortDBaseFileName: ShortString;
    procedure Run(var Info); message WM_User + 109;
    procedure UpdateCounters;
    procedure PrintOneDisk (Disk: ShortString);
    procedure PrintOneDir  (Dir: ShortString);
    procedure AddOneFileToList (var OneFile: TOneFile);
    procedure PrintFilesInDir;
  end;

var
  FormDiskPrint: TFormDiskPrint;

implementation

uses {Printers,} UExceptions, UApi, UBaseUtils, FDiskPrintSelect, ULang, UPrinter,
     {FDBase} Fmain
{$ifdef LOGFONT}
  , UFont
{$endif}
;

{$R *.dfm}


const NoOfColumns = 4;

var
  AmountPrintedPx: Integer;
  PageCounter: Integer;
  ColWidthsPx: array [0..NoOfColumns-1] of Integer;
  TimeWidthPx: Integer;
  MWidthPx: Integer;

//-----------------------------------------------------------------------------
// callback made when the window progress needs to be updated

procedure CallBackWhenChange(var Stop: boolean);

  begin
  FormDiskPrint.UpdateCounters;
  Application.ProcessMessages;
  Stop := FormDiskPrint.StopIt;
  end;

//-----------------------------------------------------------------------------
// used from the Engine to send single disk name

procedure SendOneDiskFunct (Disk: ShortString);

  begin
  FormDiskPrint.PrintOneDisk (Disk);
  end;

//-----------------------------------------------------------------------------
// used from the Engine to send single folder name

procedure SendOneDirFunct (Dir: ShortString);
  begin
  FormDiskPrint.PrintOneDir (Dir);
  end;

//-----------------------------------------------------------------------------
// used from the Engine to send single file info

procedure SendOneFileFunct (var OneFile: TOneFile);
  begin
  FormDiskPrint.AddOneFileToList (OneFile);
  end;

//-----------------------------------------------------------------------------
// Calculates the width in pixels needed for the string

procedure GetNameWidthFunct (Name: ShortString; var Width: integer);
  begin
  ///Width := QPrinterGetTextWidth(Name);
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.FormCreate(Sender: TObject);

  begin
  CanRun := false;
  QGlobalOptions := TGlobalOptions.Create;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.FormShow(Sender: TObject);

  begin
  FormSettings.GetOptions(QGlobalOptions);
  LabelWhat.Caption := lsCalculatingPrintExtent;
  StopIt := false;
  CanRun := true;
  // this starts the Run procedure after the window is displayed
  PostMessage(Self.Handle, WM_User + 109, 0, 0);
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.ButtonCancelClick(Sender: TObject);

  begin
  StopIt := true;
  CanRun := false;
  // this cuases assigning ModalResult in the Run procedure
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.UpdateCounters;

  var
    Percent: Integer;
    DisksCount, DirsCount, FilesCount: longint;

  begin
  QI_GetSearchProgress (DBaseHandle, Percent, DisksCount, DirsCount, FilesCount);
  LabelSearched.Caption := IntToStr(DisksCount) + '/' + IntToStr(FilesCount);
  ///Gauge.Progress := Percent;
  end;

//-----------------------------------------------------------------------------
// determines, whether this page is included in the print range

function TFormDiskPrint.CanPrintOnThisPage: boolean;

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

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.GoToNewPage;

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

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.PrintHeaderAndFooter;

  var
    OutRect : TRect;
    S, S1   : ShortString;
    Attr    : Word;
	TmpIndex: Integer;

  begin
  if StopIt then exit;
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
    if S = '' then S := lsDatabase + ShortDBaseFileName;
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
    QI_GetCurrentKey(DBaseHandle, S1, Attr, TmpIndex);
    S1 := lsDisk + S1;
    OutRect.Left := OutRect.Right - QPrinterGetTextWidth(S1);
    if OutRect.Left < (LeftPrintAreaPx + QPrinterGetTextWidth(S))
      then OutRect.Left := LeftPrintAreaPx + QPrinterGetTextWidth(S) + 3*YPxPer1mm;
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S1);
    QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 50)); {0.2 mm};
    QPrinterMoveTo(LeftPrintAreaPx,  OutRect.Bottom + YPxPer1mm);
    QPrinterLineTo(RightPrintAreaPx, OutRect.Bottom + YPxPer1mm);
    end;

  {footer}
  OutRect.Left   := LeftPrintAreaPx;
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

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.PrintColumnNames;

  var
    OutRect : TRect;
    S       : ShortString;
    i       : Integer;
    StartX  : Integer;

  begin
  if StopIt then exit;
  {$ifdef mswindows}
  QPrinterSaveAndSetFont(pfsBold);
  OutRect.Left   := LeftPrintAreaPx + 3*MWidthPx;
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
           S := lsName;
           QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
           end;
        1: begin
           S := lsSize;
           StartX := OutRect.Right - QPrinterGetTextWidth(S);
           QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
           end;
        2: begin
           S := lsDateTime;
           StartX := OutRect.Right - QPrinterGetTextWidth(S);
           QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
           end;
        3: begin
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

//-----------------------------------------------------------------------------
// Makes the print

procedure TFormDiskPrint.Run (var Info);

  var
    MaxNameLength, AvgNameLength, MaxNamePxWidth: Integer;
    Percent, ExpectedPages: Integer;
    DisksCount, DirsCount, FilesCount: longint;
    MsgText: array[0..256] of char;
    Copies: Integer;
    SearchIn: TSearchIn;

  begin
  if not CanRun then exit;
  {$ifdef mswindows}
  FilesList            := TQStringList.Create;
  FilesList.Sorted     := true;
  FilesList.Duplicates := dupAccept;

  CanRun := false;
  try
    FormDiskPrintSelect.RadioButtonSelectedDisks.Enabled :=
      QI_GetSelectedCount (DBaseHandle) > 0;
    FormDiskPrintSelect.Caption := lsPrintSelection;
    FormDiskPrintSelect.LabelWhat.Caption := lsPrintWhat;
    if FormDiskPrintSelect.ShowModal <> mrOk then
      begin
      FilesList.Free;
      ModalResult := mrCancel;
      exit;
      end;
    with FormDiskPrintSelect do
      begin
      SearchIn := siActualDiskDir;
      if RadioButtonActDiskWhole.Checked  then SearchIn := siActualDisk;
      if RadioButtonSelectedDisks.Checked then SearchIn := siSelectedDisks;
      end;
    if not PrintDialog.Execute then
      begin
      FilesList.Free;
      ModalResult := mrCancel;
      exit;
      end;

    QPrinterReset(QGlobalOptions);
    AmountPrintedPx    := TopPrintAreaPx + 2*LineHeightPx;

    TimeWidthPx := QPrinterGetTextWidth(DosTimeToStr(longint(23) shl 11,
                                          QGlobalOptions.ShowSeconds));
    TimeWidthPx := TimeWidthPx + TimeWidthPx div 6;
    MWidthPx := QPrinterGetTextWidth('M');

    Gauge.Progress := 0;
    Gauge.Show;
    Application.ProcessMessages;
    QI_SetIterateCallback (DBaseHandle, CallBackWhenChange,
                           SendOneDiskFunct, SendOneDirFunct, SendOneFileFunct,
                           GetNameWidthFunct);
    QI_IterateFiles(DBaseHandle, SearchIn, stNothing);
    QI_DelIterateCallback(DBaseHandle);

    QI_GetSearchProgress (DBaseHandle, Percent, DisksCount, DirsCount, FilesCount);
    QI_GetMaxNameLength (DBaseHandle, MaxNameLength, AvgNameLength, MaxNamePxWidth);

    ExpectedPages := FilesCount div ((HeightPx - 3*LineHeightPx) div LineHeightPx) + 1;

    if (ExpectedPages > 5) and (PrintDialog.PrintRange = prAllPages) then
      if Application.MessageBox(
         StrPCopy(MsgText, lsExpectedNoOfPages + IntToStr(ExpectedPages) +
         lsReallyPrintThem),
         lsWarning, mb_YesNoCancel) <> IdYes
        then
          begin
          FilesList.Free;
          ModalResult := mrCancel;
          exit;
          end;
    Gauge.Hide;
    LabelWhat.Caption := lsPreparingPrint;

    if QGlobalOptions.AdjustNameWidth
      then
        begin
        MaxNameLength := (AvgNameLength *4 + MaxNameLength) div 5 + 4; {4 pro ext}
        if MaxNameLength <= 12
          then
            ColWidthsPx[0] := QPrinterGetTextWidth('MMMMxxxx.mxx') + 3*XPxPer1mm
          else
            begin
            ColWidthsPx[0] := QPrinterGetTextWidth('MMMMxiii.mxi');
            ColWidthsPx[0] := (ColWidthsPx[0] * MaxNameLength) div 12 + 3*XPxPer1mm;
            end;
        end
      else
        begin
        ColWidthsPx[0] := MaxNamePxWidth + QPrinterGetTextWidth('.MMM');
        end;

    ColWidthsPx[1] := QPrinterGetTextWidth(FormatSize(2000000000,
                      QGlobalOptions.ShowInKb)) + 3*XPxPer1mm;
    ColWidthsPx[2] := QPrinterGetTextWidth(' ' + DosDateToStr (694026240)) +
                            TimeWidthPx  + 3*XPxPer1mm;
    ColWidthsPx[3] := 25 * QPrinterGetTextWidth('Mmmmxxxxx ');

    Application.ProcessMessages;
    for Copies := 1 to PrintDialog.Copies do
      begin
      PageCounter := 1;
      QPrinterBeginDoc(lsQuickDir);
      PrintHeaderAndFooter;
      PrintColumnNames;
      LabelWhat.Caption :=
        lsPrintingPage + IntToStr(PageCounter);

      QI_SetIterateCallback (DBaseHandle, CallBackWhenChange,
                             SendOneDiskFunct, SendOneDirFunct, SendOneFileFunct, GetNameWidthFunct);
      QI_IterateFiles(DBaseHandle, SearchIn, stDataCallback);
      QI_DelIterateCallback(DBaseHandle);
      PrintFilesInDir; {clear the files list}
      Application.ProcessMessages;
      if StopIt then break;
      QPrinterEndDoc;
      if StopIt then break;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      FatalErrorMessage(EFatal.Message);
    on E: Exception do Application.MessageBox(StrPCopy(MsgText, E.Message), lsError,
        mb_Ok or mb_IconExclamation);
    end;
  FilesList.Free;
  {$endif}
  ModalResult := mrOk;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.FormDestroy(Sender: TObject);

  begin
  QGlobalOptions.Free;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.PrintOneDisk (Disk: ShortString);

  var
    OutRect : TRect;

  begin
  if StopIt then exit;
  {$ifdef mswindows}
  PrintFilesInDir;
  QPrinterSaveAndSetFont(pfsBold);
  inc(AmountPrintedPx, LineHeightPx);
  CheckAmountPrinted;
  OutRect.Left   := LeftPrintAreaPx;
  OutRect.Right  := RightPrintAreaPx;
  OutRect.Top    := AmountPrintedPx;
  OutRect.Bottom := OutRect.Top + LineHeightPx;
  if CanPrintOnThisPage then
    begin
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, Disk);
    QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 100)); {0.1 mm};
    QPrinterMoveTo(LeftPrintAreaPx,  OutRect.Bottom);
    QPrinterLineTo(RightPrintAreaPx, OutRect.Bottom);
    end;
  inc(AmountPrintedPx, LineHeightPx);
  CheckAmountPrinted;
  QPrinterRestoreFont;
  {$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.PrintOneDir (Dir: ShortString);

  var
    OutRect : TRect;

  begin
  if StopIt then exit;
  {$ifdef mswindows}
  PrintFilesInDir;
  QPrinterSaveAndSetFont(pfsItalic);
  inc(AmountPrintedPx, YPxPer1mm);
  OutRect.Left   := LeftPrintAreaPx + MWidthPx;
  OutRect.Right  := RightPrintAreaPx;
  OutRect.Top    := AmountPrintedPx;
  OutRect.Bottom := OutRect.Top + LineHeightPx;
  if CanPrintOnThisPage then
    begin
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, Dir);
    QPrinterSetLineWidth(1); {hairline};
    QPrinterMoveTo(OutRect.Left,  OutRect.Bottom);
    QPrinterLineTo(OutRect.Right, OutRect.Bottom);
    end;
  inc(AmountPrintedPx, LineHeightPx);
  inc(AmountPrintedPx, YPxPer1mm);
  CheckAmountPrinted;
  QPrinterRestoreFont;
  {$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.AddOneFileToList (var OneFile: TOneFile);

  var
    OneFileLine: TOneFileLine; //pointer

  begin
  if StopIt then exit;
  OneFileLine := TOneFileLine.Create(OneFile);
  FilesList.AddObject(GetSortString(OneFile, OneFileLine.FileType,
                                    QGlobalOptions.SortCrit,
                                    QGlobalOptions.ReversedSort),
                      OneFileLine);
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.PrintFilesInDir;

  var
    OutRect  : TRect;
    TmpRect  : TRect;
    S        : ShortString;
    i        : Integer;
    StartX   : Integer;
    Index    : Integer;
    OneFileLine: TOneFileLine;
    ShortDesc: ShortString;

  begin
  {$ifdef mswidnows}
  for Index := 0 to pred(FilesList.Count) do
    begin
    OutRect.Left   := LeftPrintAreaPx + 3*MWidthPx;;
    OutRect.Top    := AmountPrintedPx;
    OutRect.Bottom := OutRect.Top + LineHeightPx;
    OneFileLine := TOneFileLine(FilesList.Objects[Index]);
    if CanPrintOnThisPage then
      for i := 0 to NoOfColumns-1 do
        begin
        OutRect.Right := OutRect.Left + ColWidthsPx[i] - 3*XPxPer1mm;
        if (OutRect.Right > RightPrintAreaPx) or (i = NoOfColumns-1)
          then OutRect.Right := RightPrintAreaPx;
        if OutRect.Left >= OutRect.Right then break;
        case i of
          0: begin
             S := OneFileLine.POneFile^.LongName
                  + OneFileLine.POneFile^.Ext;
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
             end;
          1: begin
             if OneFileLine.POneFile^.LongName = '..'
               then
                 S := ''
               else
                 if OneFileLine.POneFile^.Attr and faDirectory = faDirectory
                   then S := lsFolder
                   else S := FormatSize(OneFileLine.POneFile^.Size,
                              QGlobalOptions.ShowInKb);
             StartX := OutRect.Right - QPrinterGetTextWidth(S);
             QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
             end;
          2: begin
             if OneFileLine.POneFile^.Time <> 0 then
               begin
               S := DosTimeToStr(OneFileLine.POneFile^.Time, QGlobalOptions.ShowSeconds);
               StartX := OutRect.Right - QPrinterGetTextWidth(S);
               QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
               S := DosDateToStr(OneFileLine.POneFile^.Time);
               TmpRect := OutRect;
               TmpRect.Right := TmpRect.Right - TimeWidthPx;
               if TmpRect.Right > TmpRect.Left then
                 begin
                 StartX := TmpRect.Right - QPrinterGetTextWidth(S);
                 QPrinterTextRect(TmpRect, StartX, TmpRect.Top, S);
                 end;
               end;
             end;
          3: begin
             ShortDesc[0] := #0;
             if OneFileLine.POneFile^.Description <> 0 then
               QI_GetShortDesc(DBaseHandle, OneFileLine.POneFile^.Description, ShortDesc);
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, ShortDesc);
             end;
          end; {case}
        inc(OutRect.Left,ColWidthsPx[i]);
        end; {for i}
    inc(AmountPrintedPx, LineHeightPx);
    CheckAmountPrinted;
    end; {for Index}
  {$endif}
  FilesList.Clear;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrint.CheckAmountPrinted;

  begin
  {$ifdef mswindows}
  if (AmountPrintedPx + 3*LineHeightPx) > BottomPrintAreaPx then
    begin
    if not StopIt then
      begin
      GoToNewPage;
      inc(PageCounter);
      end;
    if (CanPrintOnThisPage)
      then LabelWhat.Caption := lsPrintingPage + IntToStr(PageCounter)
      else LabelWhat.Caption := lsSkippingPage + IntToStr(PageCounter);
    Application.ProcessMessages;
    AmountPrintedPx  := TopPrintAreaPx + 2*LineHeightPx;
    PrintHeaderAndFooter;
    PrintColumnNames;
    end;
  {$endif}
  end;

//-----------------------------------------------------------------------------

end.

