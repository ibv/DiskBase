unit FDBase;
(*====================================================================
MDI Window with one database. Here is the top level implementation of
most DiskBase commands.
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages, ComCtrls,
  {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, Grids, Outline,
  StdCtrls, ExtCtrls, Menus, FSettings, UCollectionsExt, UApiTypes, UStringList,
  UTypes, PrintersDlgs
{$ifdef LOGFONT}
  , UFont
{$endif}
  ;

const
  MaxTreeLevels = 32;
  maxColumnAttribs = 6;
  defHintDelayPeriod = 100;

type
  TOneColumn = record
    Width  : Integer;
    Content: (coName, coTime, coSize, coDesc);
    end;

  TPrintWhat = (prSelectedPanel, prDisks, prTree, prFiles);

  // class representing one displayed line of the folder tree
  TOneTreeLine = class(TObject)
    Level : Integer;
    Line : string[MaxTreeLevels];
    end;

  // class representing one displayed line of the folders
  TOneFileLine = class(TObject)
    POneFile : TPOneFile;
    FileType : TFileType;
    ExtAttr  : byte;
    constructor Create(aOneFile: TOneFile);
    destructor  Destroy; override;
    end;

  { TFormDBase }

  TFormDBase = class(TForm)
    ///HeaderTop: THeader;
    ///OutlineTree: TOutline;
    HeaderTop: THeaderControl;
    ImageList: TImageList;
    OutlineTree: TTreeView;
    MenuDeleteFile: TMenuItem;
    DrawGridDisks: TDrawGrid;
    DrawGridFiles: TDrawGrid;
    PopupMenuDisk: TPopupMenu;
    PopupMenuTree: TPopupMenu;
    PopupMenuFiles: TPopupMenu;
    MenuDeleteDisk: TMenuItem;
    MenuUndeleteDisk: TMenuItem;
    MenuRenameDisk: TMenuItem;
    MenuSelectDisks: TMenuItem;
    MenuDeselectDisk: TMenuItem;
    MenuShowTree: TMenuItem;
    MenuExpandTree: TMenuItem;
    MenuBrief: TMenuItem;
    MenuDetailed: TMenuItem;
    N1: TMenuItem;
    MenuSortName: TMenuItem;
    MenuSortExt: TMenuItem;
    MenuSortTime: TMenuItem;
    MenuSortSize: TMenuItem;
    N2: TMenuItem;
    MenuHelpFiles: TMenuItem;
    N3: TMenuItem;
    MenuEditDesc: TMenuItem;
    MenuPrintFiles: TMenuItem;
    MenuCopyFile: TMenuItem;
    N4: TMenuItem;
    MenuHelpTree: TMenuItem;
    MenuHelpDisks: TMenuItem;
    N6: TMenuItem;
    MenuPrintDisks: TMenuItem;
    MenuInfoDisk: TMenuItem;
    MenuCollapseTree: TMenuItem;
    MenuSelectFiles: TMenuItem;
    MenuUnselectFiles: TMenuItem;
    MenuSelectAllFiles: TMenuItem;
    N5: TMenuItem;
    N7: TMenuItem;
    MenuSelectAllDisks: TMenuItem;
    MenuCopyDisk: TMenuItem;
    N8: TMenuItem;
    ///PrintDialog: TPrintDialog;
    MenuPrintTree: TMenuItem;
    MenuRescan: TMenuItem;
    HeaderBottom: THeaderControl;
    MenuOpen: TMenuItem;
    PrintDialog: TPrintDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure HeaderTopSized(Sender: TObject; ASection,
                AWidth: Integer);
    procedure FormCreate(Sender: TObject);
    procedure ChangePanel(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DrawGridDisksDrawCell(Sender: TObject; Col, Row: Longint;
                Rect: TRect; State: TGridDrawState);
    procedure DrawGridDisksDblClick(Sender: TObject);
    procedure DrawGridDisksKeyDown(Sender: TObject; var Key: Word;
                Shift: TShiftState);
    procedure DrawGridFilesKeyDown(Sender: TObject; var Key: Word;
                Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure DrawGridFilesDrawCell(Sender: TObject; Col, Row: Longint;
                Rect: TRect; State: TGridDrawState);
    procedure DrawGridFilesSelectCell(Sender: TObject; Col, Row: Longint;
                var CanSelect: Boolean);
    procedure DrawGridDisksSelectCell(Sender: TObject; Col, Row: Longint;
                var CanSelect: Boolean);
    procedure OutlineTreeClick(Sender: TObject);
    procedure DrawGridFilesDblClick(Sender: TObject);
    procedure DrawGridFilesMouseDown(Sender: TObject; Button: TMouseButton;
                Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DrawGridDisksMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuDeleteDiskClick(Sender: TObject);
    procedure MenuUndeleteDiskClick(Sender: TObject);
    procedure MenuRenameDiskClick(Sender: TObject);
    procedure MenuSelectDisksClick(Sender: TObject);
    procedure MenuDeselectDiskClick(Sender: TObject);
    procedure MenuInfoDiskClick(Sender: TObject);
    procedure MenuPrintDisksClick(Sender: TObject);
    procedure MenuHelpDisksClick(Sender: TObject);
    procedure PopupMenuTreePopup(Sender: TObject);
    procedure MenuShowTreeClick(Sender: TObject);
    procedure MenuExpandTreeClick(Sender: TObject);
    procedure MenuHelpTreeClick(Sender: TObject);
    procedure MenuCollapseTreeClick(Sender: TObject);
    procedure PopupMenuFilesPopup(Sender: TObject);
    procedure MenuCopyFileClick(Sender: TObject);
    procedure MenuEditDescClick(Sender: TObject);
    procedure MenuBriefClick(Sender: TObject);
    procedure MenuDetailedClick(Sender: TObject);
    procedure MenuSortNameClick(Sender: TObject);
    procedure MenuSortExtClick(Sender: TObject);
    procedure MenuSortTimeClick(Sender: TObject);
    procedure MenuSortSizeClick(Sender: TObject);
    procedure MenuPrintFilesClick(Sender: TObject);
    procedure MenuHelpFilesClick(Sender: TObject);
    procedure PopupMenuDiskPopup(Sender: TObject);
    procedure MenuSelectFilesClick(Sender: TObject);
    procedure MenuUnselectFilesClick(Sender: TObject);
    procedure MenuSelectAllFilesClick(Sender: TObject);
    procedure MenuSelectAllDisksClick(Sender: TObject);
    procedure MenuCopyDiskClick(Sender: TObject);
    procedure MenuPrintTreeClick(Sender: TObject);
    procedure MenuRescanClick(Sender: TObject);
    procedure DrawGridFilesMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure MenuOpenClick(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuDeleteFileClick(Sender: TObject);
  private
    FilesList          : TQStringList;
    NeedReSort         : boolean; {if QOptions.SortCrit changes, resort in idle time}
    TreePtrCol         : PQPtrCollection;
    DirToScrollHandle  : PDirColHandle;
    ColumnAttrib       : array[0..maxColumnAttribs] of TOneColumn;
    HeaderWidths       : TPanelHeaderWidths;
    TimeWidth          : Integer;
    BitmapFolder       : TBitmap;
    BitmapParent       : TBitmap;
    BitmapArchive      : TBitmap;
    StatusLineFiles    : ShortString;
    StatusLineSubTotals: ShortString;
    LastSelectedFile   : ShortString; {name of the current file - from the status line}
    FormIsClosed       : boolean;     {because Form gets Resize Event after closing!}
    PanelsLocked       : boolean;
    LastDiskSelected   : Integer;     {for undo}
    LastFileSelected   : Integer;
    CurFileSelected    : Integer;
    ScrollBarHeight    : Integer;
    MakeDiskSelection  : boolean;     {set when shift is pressed}
    MakeFileSelection  : boolean;
    MaxFileNameLength  : Integer;     {max. length of name in current list}
    MaxFileNamePxLength: Integer;
    FormWasResized     : Integer;     {the ResizeEvent appears multiple times, so we must limit the response}
    SubTotals          : TSubTotals;
    StatusSection0Size : Integer;
    LastMouseMoveTime  : longint;
    FilesHintDisplayed : boolean;
    LastMouseXFiles    : integer;
    LastMouseYFiles    : integer;
    LastHintRect       : TRect;
    DisableHints       : integer;
    DisableNextDoubleClick: boolean;
    MousePosAlreadyChecked: boolean;
    ShiftState         : TShiftState; // for mouse double click check
    procedure ResetDrawGridFiles;
    procedure UpdateDiskWindow;
    procedure UpdateFileWindowGrid(SelPos: Integer);
    procedure ScanTree(DirColHandle: PDirColHandle; Level: Integer;
                       var TotalItemCount: Integer);
    procedure ReloadTree;
    procedure RescanTree(SavePosition: boolean);
    function  GetSelectedFileName: ShortString;
    procedure ReloadFileCol(SaveFileName: ShortString);
    ///procedure ExpandBranch(AnItem: TOutlineNode);
    procedure ExpandBranch(AnItem: TTreeNode);
    procedure JumpInOutline(SubDirCol: pointer);
    procedure ResetFontsAndRowHeights;
    procedure UpdateStatusLine(Index: Integer; SubTotalsToo: boolean);
    procedure UpdateHeader;
    procedure ResizeHeaderBottom;
    function  DoScan (Drive: char;
                      StartPath: ShortString; DiskName: ShortString;
                      AutoRun, NoOverWarning: boolean): boolean;
    procedure ScanQDir4Database;
    procedure FindConvertDLLs;
    procedure DrawGridFilesToggleSelection;
    procedure ShowFileHint;
    procedure EraseFileHint;
    function  FindOneFileLine(Point: TPoint; var Rect: TRect): TOneFileLine;
    procedure ExecuteUserCommand(sDll: AnsiString; sParams: AnsiString; bTestExist: boolean);
  public
    DBaseHandle        : PDBaseHandle;
    QGlobalOptions     : TGlobalOptions; {class}
    QLocalOptions      : TLocalOptions;  {record}
    ConvertDLLs        : TStringList;
    DBaseFileName      : ShortString;
    ShortDBaseFileName : ShortString;
    DBaseIsReadOnly    : boolean;
    function  AttachDBase(FName: ShortString): boolean;
    procedure LocalIdle;
    procedure LocalTimer;
    procedure ChangeGlobalOptions;
    procedure ChangeLocalOptions;
    procedure SetBriefFileDisplay(Brief: boolean);
    procedure ShowOrHideTreePanel;
    procedure SetNeedResort(SortCrit: TSortCrit);
    function  DBaseIsEmpty: boolean;
    procedure MsgShouldExit;
    function  ScanDisk (Drive: char; AutoRun: boolean): boolean; // returns false when interrupted
    procedure RescanDisk;
    procedure ScanFolder(Directory, DiskName, VolumeLabel: ShortString;
                         NoOverWarning: boolean);
    procedure ImportFromQDir41(FileName: ShortString);
    procedure DeleteRecord;
    function  CanUndeleteRecord: boolean;
    procedure UndeleteRecord;
    procedure ChangeDiskName;
    procedure SearchName;
    procedure SearchSelected;
    procedure SearchParam(ParamFindFile: ShortString);
    procedure SearchEmpty;
    procedure ShowDiskInfo;
    procedure ShowDBaseInfo;
    procedure JumpTo(Disk, Dir, FileName: ShortString);
    procedure EditDescription;
    function  GetDBaseHandle: PDBaseHandle;
    procedure SelectDisksOrFiles    (Disks: boolean);
    procedure SelectAllDisksOrFiles (Disks: boolean);
    procedure UnselectDisksOrFiles  (Disks: boolean);
    procedure GoToParent;
    procedure DoSelection;
    procedure SelectAll;
    procedure UnselectAll;
    procedure MakeCopy;
    procedure MakeCopyDisks;
    procedure MakeCopyFiles;
    procedure DeleteFiles;
    function  ActivePanel: Integer;
    procedure MakePrintDisk;
    procedure MakePrintTree;
    procedure MakePrintFiles;
    procedure DoPrint(PrintWhat: TPrintWhat);
    procedure ExportToOtherFormat;
    procedure ExecFile(POneFile: TPOneFile);
    procedure OpenExplorer(POneFile: TPOneFile);
    procedure DiscGearPrint(hWindow: HWND);
  end;

function GetSortString(OneFile: TOneFile; FileType: TFileType;
                       SortCrit: TSortCrit; Reversed: boolean): ShortString;

var
  g_bShowHelpAfterClose: boolean;
  g_PanelHeaderWidths  : TPanelHeaderWidths;

//====================================================================

implementation

uses
  Clipbrd, {Printers,} UExceptions, {ShellApi,}
  FAskForLabel, FScanProgress, FDiskInfo, FDBInfo, FDescription, UBaseUtils,
  FMain, FFindFile, FFindEmpty, FFindFileDlg, FFindEmptyDlg, FFoundFile, FFoundEmpty,
  FRenameDlg, FLocalOptions,
  FMaskSelect, UCollections, FAbortPrint, FDiskPrint, UApi, ULang,
  UPrinter,
  FScanFolder, UDrives, FHidden, UExport, FDiskExport, UDebugLog, UUserDll, UPluginDiscGear;
{$R *.dfm}

const
  ClipboardLimit: longint = 63 * 1024 + 1000;
  boSavePos   = true;
  boJumpToDir = false;

var
  TmpLocalOptions: TLocalOptions;

//--------------------------------------------------------------------
// creates sort string from the name - assures that folders are always first

function GetSortString(OneFile: TOneFile; FileType: TFileType;
                       SortCrit: TSortCrit; Reversed: boolean): ShortString;

  var TmpL: longint;

  begin
  Result[0] := #1;
  with OneFile do
    begin
    if SortCrit <> scUnsort then
      begin
      if Reversed
        then
          begin
          Result[1] := '1';
          if FileType = ftParent   then Result[1] := '4';
          if FileType = ftDir      then Result[1] := '3';
          if (LongName+Ext) = lsDiskInfo then Result[1] := '5';
          end
        else
          begin
          Result[1] := '5';
          if FileType = ftParent   then Result[1] := '2';
          if FileType = ftDir      then Result[1] := '3';
          if (LongName+Ext) = lsDiskInfo then Result[1] := '1';
          end;
      end;
    case SortCrit of
      scName:
        begin
        Result := Result + LongName + Ext;
        end;
      scExt:
        begin
        Result := Result + Ext + LongName;
        end;
      scTime:
        begin
        TmpL := MaxLongInt - Time;
        Result := Result + Format('%10.10d', [TmpL]) + Ext + LongName;
        end;
      scSize:
        begin
        TmpL := MaxLongInt - Size;
        Result := Result + Format('%10.10d', [TmpL]) + Ext + LongName;
        end;
      end;
    end;
  end;

//--------------------------------------------------------------------

function GridRect(Left, Top, Right, Bottom: longint): TGridRect;
  begin
  Result.Top    := Top;
  Result.Left   := Left;
  Result.Bottom := Bottom;
  Result.Right  := Right;
  end;

//=TOneFileLine=======================================================
// TOneFileLine constructor - gets the file type

constructor TOneFileLine.Create(aOneFile: TOneFile);

  begin
  GetMemOneFile(POneFile, aOneFile);
  ExtAttr := 0;
  with POneFile^ do
    begin
    if LongName = '..'
      then
        FileType := ftParent
      else
        if Attr and faQArchive = faQArchive
          then
            FileType := ftArc
          else
            if Attr and faDirectory = faDirectory
              then FileType := ftDir
              else FileType := ftFile;

    end;
  end;

//--------------------------------------------------------------------
// TOneFileLine destructor

destructor TOneFileLine.Destroy;

  begin
  FreeMemOneFile(POneFile);
  end;

//===TFormDBase=======================================================
// event -issued when the user resizes the top bar with Disk-Tree-Files
// - the panels are resized accordingly and saved to global options

procedure TFormDBase.HeaderTopSized(Sender: TObject; ASection,
  AWidth: Integer);

var
  i: Integer;

begin
if FormIsClosed then exit;
///for i := 0 to 2 do HeaderWidths[i] := HeaderTop.SectionWidth[i];
for i := 0 to 2 do HeaderWidths[i] := HeaderTop.Sections[i].Width;
QGlobalOptions.PanelHeaderWidths := HeaderWidths;
g_PanelHeaderWidths := HeaderWidths;
ResizeHeaderBottom;
case ASection of
  0: begin
     DrawGridDisks.Width := AWidth;
     if AWidth > 50
       then DrawGridDisks.ColWidths[0] := DrawGridDisks.Width-2
       else DrawGridDisks.ColWidths[0] := 50;
     end;
  1: begin
     if QGlobalOptions.ShowTree then
       OutlineTree.Width := AWidth;
     end;
  2: begin
     end;
  end;

end;

//--------------------------------------------------------------------
// event issued when the user moves with the splitter between panels

procedure TFormDBase.SplitterMoved(Sender: TObject);

  begin
  if FormIsClosed then exit;
  HeaderWidths[0] := DrawGridDisks.Width + Splitter1.Width div 2;
  HeaderWidths[1] := OutlineTree.Width + Splitter1.Width div 2 + Splitter2.Width div 2;
  HeaderWidths[2] := DrawGridFiles.Width;
  QGlobalOptions.PanelHeaderWidths := HeaderWidths;
  g_PanelHeaderWidths := HeaderWidths;
  ResizeHeaderBottom;
  if DrawGridDisks.Width > 50
    then DrawGridDisks.ColWidths[0] := DrawGridDisks.Width-2
    else DrawGridDisks.ColWidths[0] := 50;
  ///HeaderTop.SectionWidth[0] := HeaderWidths[0];
  HeaderTop.Sections[0].Width := HeaderWidths[0];
  if not OutlineTree.Visible
    then
      begin
      ///HeaderTop.SectionWidth[1] := 0;
      ///eaderTop.SectionWidth[2] := HeaderWidths[1] + HeaderWidths[2];
      HeaderTop.Sections[1].Width := 0;
      HeaderTop.Sections[2].Width := HeaderWidths[1] + HeaderWidths[2];
      end
    else
      begin
      ///HeaderTop.SectionWidth[1] := HeaderWidths[1];
      ///HeaderTop.SectionWidth[2] := HeaderWidths[2];
      HeaderTop.Sections[1].Width := HeaderWidths[1];
      HeaderTop.Sections[2].Width := HeaderWidths[2];
      end;
  end;

//--------------------------------------------------------------------
// Called when the form is created or display options are changed
// recalculates the properties of the Files panel

procedure TFormDBase.ResetDrawGridFiles;
  var
    i            : Integer;
    WidthInPixels: Integer;
    //S: shortString;
  begin
  ResetFontsAndRowHeights;
  if QGlobalOptions.FileDisplayType = fdBrief then
    with DrawGridFiles do
      begin
      Options := [goFixedVertLine, goFixedHorzLine, goVertLine,
                  goDrawFocusSelected];
      FixedRows  := 0;
      ColCount   := 1;
      ScrollBars := ssHorizontal;
      end;
  if QGlobalOptions.FileDisplayType = fdDetailed then
    with DrawGridFiles do
      begin
      Options := [goFixedVertLine, goFixedHorzLine, goVertLine,
                  goDrawFocusSelected, goRowSelect, goColSizing];
      RowCount   := 2;
      FixedRows  := 1;
      ScrollBars := ssBoth;
      i := 0;
      WidthInPixels := MaxFileNamePxLength + 5;
      if QGlobalOptions.ShowIcons then inc(WidthInPixels, BitmapFolder.Width);
      ColumnAttrib[i].Width := WidthInPixels;
      ColumnAttrib[i].Content := coName;
      inc(i);
      if QGlobalOptions.ShowSize then
        begin
        ColumnAttrib[i].Width := DrawGridFiles.Canvas.TextWidth(FormatSize(2000000000,
                                                      QGlobalOptions.ShowInKb)) + 2;
        ColumnAttrib[i].Content := coSize;
        inc(i);
        end;
      if QGlobalOptions.ShowTime then
        begin
        ColumnAttrib[i].Width :=
        DrawGridFiles.Canvas.TextWidth(' ' + DosDateToStr (694026240)) { 30.10.2000 }
          + TimeWidth + 2;
          {timewidth is set in ResetFont}
        ColumnAttrib[i].Content := coTime;
        inc(i);
        end;
      if QGlobalOptions.ShowDescr then
        begin
        ColumnAttrib[i].Width := 25 * DrawGridFiles.Canvas.TextWidth('Mmmmxxxxx ');
        ColumnAttrib[i].Content := coDesc;
        inc(i);
        end;
      ColCount := i;
      for i := 0 to pred(ColCount) do
        ColWidths[i] := ColumnAttrib[i].Width;
      end;
  end;

//--------------------------------------------------------------------
// initialization done, when the form is created

procedure TFormDBase.FormCreate(Sender: TObject);

  var
    i: Integer;
  begin
  FormIsClosed := false;
  PanelsLocked := false;
  StatusSection0Size := 0;
  ScrollBarHeight := GetSystemMetrics(SM_CXHTHUMB);
        {should be equal to vertical size of ScrollBar}
  QGlobalOptions := TGlobalOptions.Create;
  FormSettings.GetOptions(QGlobalOptions);

  HeaderTop.Font.Size := 8;
  HeaderBottom.Font.Size := 8;
  ///for i := 0 to 2 do HeaderTop.SectionWidth[i] := QGlobalOptions.PanelHeaderWidths[i];
  ///for i := 0 to 2 do HeaderWidths[i] := HeaderTop.SectionWidth[i];
  for i := 0 to 2 do HeaderTop.Sections[i].Width := QGlobalOptions.PanelHeaderWidths[i];
  for i := 0 to 2 do HeaderWidths[i] := HeaderTop.Sections[i].Width;
  ///DrawGridDisks.Width        := HeaderTop.SectionWidth[0];
  DrawGridDisks.Width        := HeaderTop.Sections[0].Width;
  DrawGridDisks.ColWidths[0] := DrawGridDisks.Width-2;

  ///OutlineTree.Width          := HeaderTop.SectionWidth[1];
  OutlineTree.Width          := HeaderTop.Sections[1].Width;

  FilesList            := TQStringList.Create;
  FilesList.Sorted     := true;
  FilesList.Reversed   := false;
  FilesList.Duplicates := qdupAccept;
  NeedReSort           := false;
  ConvertDLLs          := TStringList.Create;
  ConvertDLLs.Sorted   := true;

  TreePtrCol      := New(PQPtrCollection, Init(1000, 1000));
  ///BitmapFolder    := OutlineTree.PictureLeaf;
  ///BitmapParent    := OutlineTree.PicturePlus;
  ///BitmapArchive   := OutlineTree.PictureMinus;
  ResetDrawGridFiles;
  ShowOrHideTreePanel;
  FindConvertDLLs;
  Tag := GetNewTag;
  MakeDiskSelection := false;
  MakeFileSelection := false;
  MaxFileNameLength := 12;
  MaxFileNamePxLength := 50;
  FillChar(SubTotals, SizeOf(SubTotals), 0);
  LastMouseMoveTime := $0FFFFFFF;
  FilesHintDisplayed := false;
  DisableHints := 0;
  DisableNextDoubleClick := false;
  SetRectEmpty(LastHintRect);
  end;

//--------------------------------------------------------------------
// Event issued when the MDI window with the database is to be closed

procedure TFormDBase.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

  begin
  MainForm.AddToLastFilesList(DBaseFileName);
  CanClose := true;
  QI_CloseDatabase(DBaseHandle, false);
  FormIsClosed := true;
  QGlobalOptions.Free;
  FilesList.Free;
  ConvertDLLs.Free;
  if TreePtrCol <> nil then
    Dispose(TreePtrCol, Done);
  TreePtrCol := nil;
  end;

//--------------------------------------------------------------------
// event issued when the user changes focus between panels - either by Tab key,
// or by mouse

procedure TFormDBase.ChangePanel(Sender: TObject);
  var
    APanel: Integer;

  begin
  APanel := ActivePanel;
  EraseFileHint;
  if APanel = 1
    ///then HeaderTop.Sections.Strings[0] := ShortDBaseFileName
    ///else HeaderTop.Sections.Strings[0] := '';
    then HeaderTop.Sections[0].Text := ShortDBaseFileName
    else HeaderTop.Sections[0].Text := '';
  if APanel = 2
    then UpdateHeader
    ///else HeaderTop.Sections.Strings[1] := '';
    else HeaderTop.Sections[1].Text := '';
  if APanel = 3
    then UpdateHeader
    ///else HeaderTop.Sections.Strings[2] := '';
    else HeaderTop.Sections[2].Text := '';
  ///HeaderTop.SectionWidth[0] := HeaderWidths[0];
  ///HeaderTop.SectionWidth[2] := HeaderWidths[2];
  HeaderTop.Sections[0].Width := HeaderWidths[0];
  HeaderTop.Sections[2].Width := HeaderWidths[2];
  if QGlobalOptions.ShowTree
    ///then HeaderTop.SectionWidth[1] := HeaderWidths[1]
    ///else HeaderTop.SectionWidth[1] := 0;
    then HeaderTop.Sections[1].Width := HeaderWidths[1]
    else HeaderTop.Sections[1].Width := 0;
  end;

//--------------------------------------------------------------------
// event issued when the window with the database is closed

procedure TFormDBase.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  if FormIsClosed then Action := caFree;
  end;

//--------------------------------------------------------------------
// attaches a database to the MDI window

function TFormDBase.AttachDBase(FName: ShortString): boolean;
  var
    SaveSelected   : Integer;
    TmpKey         : ShortString;
    TmpAttr        : Word;
    Dir, Name, Ext : ShortString;
    DBaseInfo      : TDBaseInfo;
    sMessage       : ShortString;
    MsgText        : array[0..256] of char;

  begin
  Result := false;
  try
    if not QI_OpenDatabase(FName, false, DBaseHandle, sMessage) then
      begin
      FormIsClosed := true;  // this must be before the message box
      Application.MessageBox(StrPCopy(MsgText, sMessage), lsError, mb_OK or mb_IconStop);
      exit;
      end;
    //Expired := QI_DBaseExpired(DBaseHandle);
    //if (Expired <> 0) then g_bShowHelpAfterClose := true;
    DBaseFileName := FName;
    FSplit(FName, Dir, Name, Ext);
    ShortDBaseFileName := AnsiLowerCase(Name);
    ShortDBaseFileName[1] := Name[1];
    Caption := ShortDBaseFileName;
    QI_GetLocalOptions (DBaseHandle, QLocalOptions);
    QI_GetCurrentKey   (DBaseHandle, TmpKey, TmpAttr, SaveSelected);
    QI_SetCurrentKeyPos(DBaseHandle, SaveSelected);
    LastDiskSelected := SaveSelected;
    UpdateDiskWindow;
    Result := true;
    QI_GetDBaseInfo(DBaseHandle, DBaseInfo);
    DBaseIsReadOnly := DBaseInfo.ReadOnly;

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// should be called when something changes in the Disk list
// - if only selection changes, it is not a reason to call this

procedure TFormDBase.UpdateDiskWindow;

var
  CountToShow : Integer;
  KeyIndex    : Integer;
  Key         : ShortString;
  Attr        : Word;

  begin
  if FormIsClosed then exit;
  try
    QI_ClearNeedUpdDiskWin(DBaseHandle);
    CountToShow := QI_GetCountToShow (DBaseHandle);
    if CountToShow = 0 then
      begin
      DrawGridDisks.Row := 0;
      DrawGridDisks.RowCount := 1;
      DrawGridDisks.Refresh;
      exit;
      end;
    if QI_GetCurrentKey (DbaseHandle, Key, Attr, KeyIndex) then
      begin
      if (DrawGridDisks.Row >= CountToShow) then
        DrawGridDisks.Row := pred(CountToShow);
      DrawGridDisks.RowCount := CountToShow;
      if DrawGridDisks.Row <> KeyIndex then
        if KeyIndex < DrawGridDisks.RowCount
          //KeyIndex value can be bigger from the past - if deleted disks were displayed
          then DrawGridDisks.Row := KeyIndex;
      DrawGridDisks.Repaint;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// called to draw one cell of the disks list

procedure TFormDBase.DrawGridDisksDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);

  var
    Key        : ShortString;
    Attr       : Word;
    IsDeleted  : boolean;
    IsSelected : boolean;

  begin
  //LOG('DrawGridDisksDrawCell', []);
  if FormIsClosed then exit;
  try
    if QI_GetKeyAt(DBaseHandle, Key, Attr, Row) then
      begin
      IsDeleted   := Attr and kaDeleted <> 0;
      IsSelected  := Attr and kaSelected <> 0;
      if IsDeleted then Key:= '(' + Key + ')';
      if IsSelected then Key := Key + #164;
      if IsDeleted and not(gdSelected in State) then
        DrawGridDisks.Canvas.Font.Color := clQDeletedText;
      if IsSelected and not(gdSelected in State) then
        begin
        DrawGridDisks.Canvas.Brush.Color := clQSelectedBack;
        DrawGridDisks.Canvas.Font.Color  := clQSelectedText;
        end;
      DrawGridDisks.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top, Key);
      //LOG('DrawGridDisks.Canvas.TextRect', []);
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// issued when the user double-clicks on the disk panel - changes
// the selection flag

procedure TFormDBase.DrawGridDisksDblClick(Sender: TObject);

  begin
  if FormIsClosed then exit;
  try
    QI_ToggleSelectionFlag(DBaseHandle, DrawGridDisks.Row);
    DrawGridDisks.Repaint;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// event issued when the user presses a key in the Disks panel

procedure TFormDBase.DrawGridDisksKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  begin
  if FormIsClosed then exit;
  if (Shift = []) then
    begin
    if ((Key = vk_Insert) or (Key = vk_Space)) then
      begin
      DrawGridDisksDblClick(Sender);
      if (DrawGridDisks.Row + 1) < DrawGridDisks.RowCount then
        DrawGridDisks.Row := DrawGridDisks.Row + 1;
      end;
    with DrawGridDisks do
      if RowCount > 0 then
        begin
        if Key = vk_Next then
          begin
          if (Row + VisibleRowCount) < RowCount
            then Row := Row + VisibleRowCount
            else Row := pred(RowCount);
          end;
        if Key = vk_Prior then
          begin
          if (Row - VisibleRowCount) > 0
            then Row := Row - VisibleRowCount
            else Row := 0;
          end;
        if Key = vk_Home then Row := 0;
        if Key = vk_End then Row := pred(RowCount);
        end;
    end;
  if Key = vk_Delete then DeleteRecord;
  if (ssShift in Shift) and
    ((Key = vk_Down) or (Key = vk_Up) or (Key = vk_Prior) or
     (Key = vk_Next) or (Key = vk_Home) or (Key = vk_End))
        then
          begin
          LastDiskSelected := DrawGridDisks.Selection.Top;
          MakeDiskSelection := true;
          end;
  end;

//--------------------------------------------------------------------
// called when the file collection is reloaded, or the type of display is changed,
// also called in idle time
// SelPos = - 1 -> do not change position

procedure TFormDBase.UpdateFileWindowGrid (SelPos: Integer);
  var
    WidthInPixels: Integer;
    CalcRows     : Integer;
    CalcCols     : Integer;
    SpaceForRows : Integer;

  begin
  if FormIsClosed then exit;
  WidthInPixels := MaxFileNamePxLength + 5;
  ///if QGlobalOptions.ShowIcons then inc(WidthInPixels, BitmapFolder.Width);
  if QGlobalOptions.FileDisplayType = fdBrief
    then
      begin
      if SelPos = -1 then
        with DrawGridFiles do
          SelPos := Col*RowCount + Row;
      SpaceForRows := DrawGridFiles.Height - 1;
      if SpaceForRows < 0 then SpaceForRows := 50;
      CalcRows := SpaceForRows div succ(DrawGridFiles.DefaultRowHeight);
      {succ includes the line between}
      if CalcRows = 0 then CalcRows := 1;
      CalcCols := (FilesList.Count + pred(CalcRows)) div CalcRows;
      if (CalcCols * succ(WidthInPixels)) >= DrawGridFiles.Width then
        begin {correction because of Scroll Bar}
        dec(SpaceForRows, ScrollBarHeight);
        if SpaceForRows < 0 then SpaceForRows := 50;
        CalcRows := SpaceForRows div succ(DrawGridFiles.DefaultRowHeight);
        if CalcRows = 0 then CalcRows := 1;
        CalcCols := (FilesList.Count + pred(CalcRows)) div CalcRows;
        end;
      with DrawGridFiles do
        begin
        Selection := GridRect(0, 0, 0, 0); {this must be here, otherwise}
        LeftCol := 0;                      {exception "Grid out of bound" happens}
        TopRow  := 0;
        DefaultColWidth := WidthInPixels;
        RowCount := CalcRows;
        ColCount := CalcCols;
        Col := SelPos div CalcRows;
        Row := SelPos mod CalcRows;
        end;
      end
    else
      begin
      with DrawGridFiles do
        begin
        ColWidths[0] := WidthInPixels;
        if SelPos = -1 then SelPos := pred(Row);
        if FilesList.Count > 0
          then RowCount := FilesList.Count + 1
          else RowCount := FilesList.Count + 2;
        if SelPos >= FilesList.Count then SelPos := 0; {to be sure}
        Row := succ(SelPos);
        end;
      end;
  DrawGridFiles.Invalidate;
  end;

//--------------------------------------------------------------------
// event issued when the user presses a key in the Files panel

procedure TFormDBase.DrawGridFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

begin
if FormIsClosed then exit;
ShiftState := Shift;
EraseFileHint;
with DrawGridFiles do
  begin
  if (ssShift in Shift) and
    ((Key = vk_Down)  or (Key = vk_Up) or
     (Key = vk_Left)  or (Key = vk_Right) or
     (Key = vk_Prior) or (Key = vk_Next))
        then MakeFileSelection := true;
  if (Key = vk_Space) or (Key = vk_Insert) then
    begin
    DrawGridFilesToggleSelection;
    Key := vk_Down;
    end;
  if Key = vk_Back then
    begin
    GoToParent;
    Key := 0;
    exit;
    end;
  if (Key = vk_Return) then
    begin
    DrawGridFilesDblClick(Sender);
    Key := 0;
    exit;
    end;
  if QGlobalOptions.FileDisplayType = fdBrief then
    begin
    if FilesList.Count > 0 then
      begin
      if (Key = vk_Down) and (Row = pred(RowCount)) then
        if Col < pred(ColCount) then
          begin
          Row := 0;
          Col := Col + 1;
          Key := 0;
          end;

      if (Key = vk_Up) and (Row = 0) then
        if Col > 0 then
          begin
          Col := Col - 1;
          Row := pred(RowCount);
          Key := 0;
          end;

      if (Key = vk_Right) or ((Key = vk_Next) and (Row = pred(RowCount))) then
        if (Col < pred(ColCount))
          then
            begin
            if succ(Col)*RowCount + Row >= FilesList.Count then
              Row := FilesList.Count - succ(Col)*RowCount - 1;
            Col := Col + 1;
            Key := 0;
            end
          else
            begin
            Row := FilesList.Count - pred(ColCount)*RowCount - 1;
            Key := 0;
            end;

      if (Key = vk_Left) or ((Key = vk_Prior) and (Row = 0)) then
        if (Col > 0)
          then
            begin
            Col := Col - 1;
            Key := 0;
            end
          else
            begin
            Row := 0;
            Key := 0;
            end;

      if (Key = vk_Home) then
        begin
        Col := 0;
        Row := 0;
        Key := 0;
        end;
      if (Key = vk_End) then
        begin
        Row := FilesList.Count - pred(ColCount)*RowCount - 1;
        Col := pred(ColCount);
        Key := 0;
        end;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------
// event issued when the user resizes the database window.
// Update of panels is made in idle time

procedure TFormDBase.FormResize(Sender: TObject);
  begin
  if FormIsClosed then exit;
  FormWasResized := 1;
  end;

//--------------------------------------------------------------------
// called to draw a cell of the Files panel

procedure TFormDBase.DrawGridFilesDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);

var
  i: Integer;
  S: ShortString;
  StartX     : Integer;
  DosDateTime: longint;
  PartRect   : TRect;
  FileType   : TFileType;
  OneFileLine: TOneFileLine;
  Offset     : Integer;
  Bitmap     : TBitmap;
  Description: TFilePointer;

  begin
  //LOG('DrawGridFilesDrawCell', []);
  if FormIsClosed then exit;
  try
    if QGlobalOptions.FileDisplayType = fdBrief
      then
        begin
        i := Col*DrawGridFiles.RowCount + Row;
        if i < FilesList.Count then
          begin
          OneFileLine := TOneFileLine(FilesList.Objects[i]);
          Offset := 0;
          if (OneFileLine.ExtAttr and eaSelected <> 0)
           and not(gdSelected in State) then
            begin
            DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
            DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
            end;
          if QGlobalOptions.ShowIcons then
            begin
            ///inc(Offset, BitmapFolder.Width);
            case OneFileLine.FileType of
              ftDir   : Bitmap := BitmapFolder;
              ftParent: Bitmap := BitmapParent;
              ftArc   : Bitmap := BitmapArchive;
              else      Bitmap := BitmapFolder;
              end;
            if (OneFileLine.ExtAttr and eaSelected <> 0)
             and not(gdSelected in State)  then
              DrawGridFiles.Canvas.FillRect(Classes.Rect(Rect.Left, Rect.Top,
                                            Rect.Left + Offset, Rect.Bottom));
            ///if (OneFileLine.FileType <> ftFile) then
            ///DrawGridFiles.Canvas.BrushCopy(
            ///        Bounds(Rect.Left + 1, Rect.Top, Bitmap.Width, Bitmap.Height),
            ///        Bitmap, Bounds(0, 0, Bitmap.Width, Bitmap.Height), clOlive);
            end;
          DrawGridFiles.Canvas.TextRect(Classes.Rect(Rect.Left + Offset, Rect.Top,
                                         Rect.Right, Rect.Bottom), Rect.Left + Offset + 1, Rect.Top,
                                         OneFileLine.POneFile^.LongName
                                         + OneFileLine.POneFile^.Ext);
          end;
        end
      else
        begin
        if Col <= maxColumnAttribs then
          begin
          if Row = 0 then ColumnAttrib[Col].Width := DrawGridFiles.ColWidths[Col];
          {this is becuase of non-existence of event on resizing columns}
          if Row <= FilesList.Count then
            begin
            case ColumnAttrib[Col].Content of
              coName:
                if Row = 0
                  then
                    DrawGridFiles.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top,
                      lsName)
                  else
                    begin
                    OneFileLine := TOneFileLine(FilesList.Objects[Row-1]);
                    Offset := 0;
                    if (OneFileLine.ExtAttr and eaSelected <> 0)
                     and not(gdSelected in State) then
                      begin
                      DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                      DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                      end;
                    if QGlobalOptions.ShowIcons then
                      begin
                      inc(Offset, BitmapFolder.Width);
                      case OneFileLine.FileType of
                        ftParent: Bitmap := BitmapParent;
                        ftArc   : Bitmap := BitmapArchive;
                        else      Bitmap := BitmapFolder;
                        end;
                      if OneFileLine.ExtAttr and eaSelected <> 0 then
                      DrawGridFiles.Canvas.FillRect(Classes.Rect(Rect.Left, Rect.Top,
                                                    Rect.Left + Offset, Rect.Bottom));
                      if OneFileLine.FileType <> ftFile then
                      DrawGridFiles.Canvas.BrushCopy(
                              Bounds(Rect.Left + 1, Rect.Top, Bitmap.Width, Bitmap.Height),
                              Bitmap, Bounds(0, 0, Bitmap.Width, Bitmap.Height), clOlive);
                      end;
                    DrawGridFiles.Canvas.TextRect(Classes.Rect(Rect.Left + Offset, Rect.Top,
                                          Rect.Right, Rect.Bottom), Rect.Left + Offset + 1,
                                          Rect.Top,
                                          OneFileLine.POneFile^.LongName
                                           + OneFileLine.POneFile^.Ext);
                    //LOG('DrawGridFiles.Canvas.TextRect', []);
                    end;
              coTime:
                begin
                if Row = 0
                  then
                    begin
                    S := lsDateAndTime;
                    StartX := Rect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                    DrawGridFiles.Canvas.TextRect(Rect, StartX, Rect.Top, S);
                    end
                  else
                    begin
                    OneFileLine := TOneFileLine(FilesList.Objects[Row-1]);
                    DosDateTime := OneFileLine.POneFile^.Time;
                    if (OneFileLine.ExtAttr and eaSelected <> 0)
                     and not(gdSelected in State) then
                      begin
                      DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                      DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                      end;
                    if DosDateTime <> 0
                      then
                        begin
                        S := DosTimeToStr(DosDateTime, QGlobalOptions.ShowSeconds);
                        StartX := Rect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                        DrawGridFiles.Canvas.TextRect(Rect, StartX, Rect.Top, S);
                        S := DosDateToStr(DosDateTime);
                        PartRect := Rect;
                        PartRect.Right := PartRect.Right - TimeWidth;
                        if PartRect.Right < PartRect.Left then PartRect.Right := PartRect.Left;
                        StartX := PartRect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                        DrawGridFiles.Canvas.TextRect(PartRect, StartX, Rect.Top, S);
                        end
                      else
                        if (OneFileLine.ExtAttr and eaSelected <> 0)
                         and not(gdSelected in State) then
                          DrawGridFiles.Canvas.FillRect(Classes.Rect(Rect.Left, Rect.Top,
                                                        Rect.Right, Rect.Bottom));

                    end;
                end;
              coSize:
                begin
                if Row = 0
                  then
                    S := lsSize
                  else
                    begin
                    OneFileLine := TOneFileLine(FilesList.Objects[Row-1]);
                    FileType := OneFileLine.FileType;
                    if (OneFileLine.ExtAttr and eaSelected <> 0)
                     and not(gdSelected in State) then
                      begin
                      DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                      DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                      end;
                    case FileType of
                      ftDir   : S := lsFolder;
                      ftParent: S := '';
                      else      S := FormatSize(
                                    TOneFileLine(FilesList.Objects[Row-1]).POneFile^.Size,
                                    QGlobalOptions.ShowInKb);
                      end;
                    end;
                StartX := Rect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                DrawGridFiles.Canvas.TextRect(Rect, StartX, Rect.Top, S);
                end;
              coDesc:
                if Row = 0
                  then
                    DrawGridFiles.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top,
                      lsDescription)
                  else
                    begin
                    OneFileLine := TOneFileLine(FilesList.Objects[Row-1]);
                    Description := OneFileLine.POneFile^.Description;
                    if (OneFileLine.ExtAttr and eaSelected <> 0)
                     and not(gdSelected in State) then
                      begin
                      DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                      DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                      end;
                    if Description <> 0
                      then
                        begin
                        DrawGridFiles.Canvas.FillRect(Classes.Rect(Rect.Left, Rect.Top,
                                                      Rect.Right, Rect.Bottom));
                        if QI_GetShortDesc(DBaseHandle, Description, S) then
                          DrawGridFiles.Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top, S);
                        end
                      else
                        if (OneFileLine.ExtAttr and eaSelected <> 0)
                         and not(gdSelected in State) then
                          DrawGridFiles.Canvas.FillRect(Classes.Rect(Rect.Left, Rect.Top,
                                                        Rect.Right, Rect.Bottom));
                    end;
              end;
            end;
          end;
        end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Issued when the user selects a cell in the Files panel

procedure TFormDBase.DrawGridFilesSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);

  var
    i            : Integer;
    bNeedRepaint : boolean;

  begin
  if FormIsClosed then exit;
  EraseFileHint;
  if QGlobalOptions.FileDisplayType = fdBrief
    then
      begin
      i := Col*DrawGridFiles.RowCount + Row;
      CanSelect := i < FilesList.Count;
      if CanSelect then
        begin
        UpdateStatusLine(i, false);
        LastFileSelected := CurFileSelected;
        CurFileSelected  := i;
        end;
      end
    else
      begin
      CanSelect := Row <= FilesList.Count;
      if CanSelect then
        begin
        UpdateStatusLine(Row-1, false);
        LastFileSelected := CurFileSelected;
        CurFileSelected  := Row-1;
        end;
      end;

  if MakeFileSelection
    then
      begin
      MakeFileSelection := false;
      if CurFileSelected <= LastFileSelected
        then
          for i := succ(CurFileSelected) to LastFileSelected do
            with TOneFileLine(FilesList.Objects[i]) do
              if ExtAttr and eaSelected <> 0
                then ExtAttr := ExtAttr and not eaSelected
                else ExtAttr := ExtAttr or eaSelected
        else
          for i := pred(CurFileSelected) downto LastFileSelected do
            with TOneFileLine(FilesList.Objects[i]) do
              if ExtAttr and eaSelected <> 0
                then ExtAttr := ExtAttr and not eaSelected
                else ExtAttr := ExtAttr or eaSelected;
      if abs(CurFileSelected - LastFileSelected) > 1 then
        DrawGridFiles.Repaint;
      end
    else
      if not UsePersistentBlocks then
        begin
        bNeedRepaint := false;
        for i := 0 to pred(FilesList.Count) do
          with TOneFileLine(FilesList.Objects[i]) do
            if (ExtAttr and eaSelected) <> 0 then 
              begin
              bNeedRepaint := true;
              ExtAttr := ExtAttr and not eaSelected;
              end;
        if bNeedRepaint then DrawGridFiles.Repaint;
        end;
  end;

//--------------------------------------------------------------------

procedure TFormDBase.ScanTree(DirColHandle: PDirColHandle; Level: Integer;
                              var TotalItemCount: Integer);
  var
    i               : Integer;
    OneFile         : TOneFile;
    OneDir          : TOneDir;
    Count           : Integer;
    SubDirColHandle : PDirColHandle;
    Node            : TTreeNode;

  begin
  try
    Count := QI_GetDirColCount(DirColHandle); {returns 0, if it is nil}
    Node:=nil;
    for i := 0 to pred(Count) do
      begin
      FillChar(OneFile, SizeOf(TOneFile), 0);
      SubDirColHandle := QI_GetDirColAt(DirColHandle, i);
      QI_GetOneDirDta(SubDirColHandle, OneDir);
      OneFile.LongName := OneDir.LongName;
      OneFile.Ext := OneDir.Ext;
      OneFile.Size := OneDir.Size; {to distinguish between folder and archive}
      ///OutLineTree.AddChildObject(Level, OneFile.LongName + OneFile.Ext,
      ///  SubDirColHandle);
      Node:=OutLineTree.Items.AddChildObject(Node, OneFile.LongName + OneFile.Ext,
        SubDirColHandle);
      TreePtrCol^.Insert(SubDirColHandle);
      inc(TotalItemCount);
      if SubDirColHandle <> nil then
        begin
        ScanTree(SubDirColHandle, TotalItemCount, TotalItemCount);
        end;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Reloads a tree from the database. Called when the disk selection changes
// - call is made in the idle time, so that the user can browse disks fast

procedure TFormDBase.ReloadTree;
  var
    DirsInTree: Integer;
    RootDirColHandle: PDirColHandle;
  begin
  if PanelsLocked then exit;
  try
    if QI_GetCountToShow(DBaseHandle) = 0 then
      begin
      ///OutLineTree.Clear;
      OutLineTree.Items.Clear;
      TreePtrCol^.DeleteAll;
      ///OutLineTree.AddObject (0, '\', nil);
      OutLineTree.Items.AddObject (nil, '\', nil);
      UpdateHeader;
      exit;
      end;
    QI_GoToKeyAt (DBaseHandle, DrawGridDisks.Selection.Top);
    if not QGlobalOptions.ShowTree then exit;
    ///OutLineTree.Clear;
    OutLineTree.Items.Clear;
    TreePtrCol^.DeleteAll;
    RootDirColHandle := QI_GetRootDirCol(DBaseHandle);
    ///OutLineTree.AddObject (0, '\', RootDirColHandle);
    OutLineTree.Items.AddObject (nil, '\', RootDirColHandle);
    TreePtrCol^.Insert(RootDirColHandle);
    DirsInTree := 1;
    try
      ScanTree (RootDirColHandle, 1, DirsInTree);
    except
      ///on Error: EOutlineError do
      ///  NormalErrorMessage(Error.Message);
      end;
    if QGlobalOptions.ExpandTree
      then
        with OutlineTree do
          begin
          OutlineTree.BeginUpdate;
          OutlineTree.FullExpand;
          OutlineTree.EndUpdate;
          end
      else
        ///OutLineTree.Items[1].Expand;
        OutLineTree.Items[1].Expand(false);
    UpdateHeader;

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Expandes one branch in the tree - used when some items in the tree
// has to be found and selected - typically when the user double-clicks
// in the window with fould files and the location of the file is to be
// displayed.

///procedure TFormDBase.ExpandBranch(AnItem: TOutlineNode);
procedure TFormDBase.ExpandBranch(AnItem: TTreeNode);

  begin
  if FormIsClosed then exit;
  if not AnItem.IsVisible then ExpandBranch(AnItem.Parent);
  AnItem.Expand(false);
  end;

//--------------------------------------------------------------------
// Rescans current tree - does not change it - used when the panel
// with the tree is shown/hidden

procedure TFormDBase.RescanTree(SavePosition: boolean);

  var
    DirsInTree: Integer;
    RootDirColHandle: PDirColHandle;
    SaveSelected: longint;

  begin
  if FormIsClosed then exit;
  if PanelsLocked then exit;
  if DBaseIsEmpty then exit;
  try
    if not QGlobalOptions.ShowTree then exit;
    ///SaveSelected := OutlineTree.SelectedItem;
    OutLineTree.Items.Clear;
    TreePtrCol^.DeleteAll;
    RootDirColHandle := QI_GetRootDirCol(DBaseHandle);
    ///OutLineTree.AddObject (0, '\', RootDirColHandle);
    OutLineTree.Items.AddObject (nil, '\', RootDirColHandle);
    TreePtrCol^.Insert(RootDirColHandle);
    DirsInTree := 1;

    try
      ScanTree (RootDirColHandle, 1, DirsInTree);
    except
      ///on Error: EOutlineError do
      ///  NormalErrorMessage(Error.Message);
      end;

    if QGlobalOptions.ExpandTree
      then
        with OutlineTree do
          begin
          OutlineTree.BeginUpdate;
          OutlineTree.FullExpand;
          OutlineTree.EndUpdate;
          end
      else
        OutLineTree.Items[1].Expand(false);
    if SavePosition and (SaveSelected > 0) then
      begin
      ExpandBranch(OutlineTree.Items[SaveSelected]);
      ///OutlineTree.SelectedItem := SaveSelected;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Event issued when the user selects a cell in the Disks list,
// the update of other pnales is made in idle time

procedure TFormDBase.DrawGridDisksSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);

  var i: Integer;

  begin
  if FormIsClosed then exit;
  try
    if MakeDiskSelection then
      begin
      MakeDiskSelection := false;
      if LastDiskSelected <= Row
        then
          for i := LastDiskSelected to pred(Row) do
            QI_ToggleSelectionFlag(DBaseHandle, i)
        else
          for i := LastDiskSelected downto succ(Row) do
            QI_ToggleSelectionFlag(DBaseHandle, i);
      if abs(LastDiskSelected - Row) > 1 then
        DrawGridDisks.Repaint;
      end;
    if DrawGridDisks.Selection.Top <> Row then
      begin
      LastDiskSelected := DrawGridDisks.Selection.Top;
      QI_SetCurrentKeyPos(DBaseHandle, Row);
      QI_SetNeedUpdTreeWin(DBaseHandle);
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Fucntion called when there is no message in the message queue. Time-consuming
// updates are located here, so that the program can respond to users events
// quickly.

procedure TFormDBase.LocalIdle;

  begin
  if FormIsClosed then exit;
  if PanelsLocked then exit;
  try
    if QI_NeedUpdDiskWin(DBaseHandle) then
      UpdateDiskWindow;
    if QI_NeedUpdTreeWin(DBaseHandle) then
      begin
      ReloadTree;
      QI_ClearNeedUpdTreeWin(DBaseHandle);
      end;
    if QI_NeedUpdFileWin(DBaseHandle) then
      begin
      ReloadFileCol('');
      QI_ClearNeedUpdFileWin(DBaseHandle);
      end;
    if NeedReSort then
      begin
      NeedReSort := false;
      ReloadFileCol(GetSelectedFileName);
      end;
    if FormWasResized > 0 then
      begin
      Dec(FormWasResized);
      if FormWasResized = 0 then
        begin
        UpdateFileWindowGrid(-1);
        ResizeHeaderBottom;
        end;
      end;

    if HeaderBottom.Sections[0].Text <> StatusLineSubTotals then
      begin
      if StatusSection0Size = 0
        then
          begin //workaround - it made problems in the first run
          StatusSection0Size := 1;
          HeaderBottom.Sections[0].Text := '-';
          end
        else
          begin
          HeaderBottom.Sections[0].Text := StatusLineSubTotals;
          StatusSection0Size := HeaderBottom.Sections[0].Width;
          end;
      ResizeHeaderBottom;
      end;
    if HeaderBottom.Sections[1].Text <> StatusLineFiles then
      begin
      HeaderBottom.Sections[1].Text := StatusLineFiles;
      ResizeHeaderBottom;
      end;

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;

  if QI_GetErrorCounter(DBaseHandle) > 0 then
    begin
    QI_ClearErrorCounter(DBaseHandle);
    NormalErrorMessage(lsErrorInDBaseFound);
    end;

  end;

//--------------------------------------------------------------------
// Timer procedure called from the timer in the main application window
// - thi assures only one timer per application is used.

procedure TFormDBase.LocalTimer;

  begin
  if FormIsClosed then exit;
  if PanelsLocked then exit;
  if QGlobalOptions.ShowFileHints and
    ((longint(GetTickCount) - LastMouseMoveTime) > defHintDelayPeriod) then
      ShowFileHint;
  end;

//--------------------------------------------------------------------
// Calculates, which file is selected in the file panel and returns its name+ext

function TFormDBase.GetSelectedFileName: ShortString;

  var
    i: Integer;

  begin
  Result := '';
  if FormIsClosed then exit;
  if FilesList.Count = 0 then exit;
  if QGlobalOptions.FileDisplayType = fdBrief
    then i := DrawGridFiles.Selection.Left*DrawGridFiles.RowCount +
          DrawGridFiles.Selection.Top
    else i := pred(DrawGridFiles.Selection.Top);
  if (i >= 0) and (i < FilesList.Count) then
    with TOneFileLine(FilesList.Objects[i]).POneFile^ do
      Result := LongName + Ext;
  end;

//--------------------------------------------------------------------
// Reloads the collection of files from the database. Used when the
// the collection is to be re-sorted or updated

procedure TFormDBase.ReloadFileCol(SaveFileName: ShortString);
  var
    FileColHandle: PFileColHandle;
    i, Count   : Integer;
    OneFile    : TOneFile;
    OneFileLine: TOneFileLine; {ukazatel}
    DirPos     : Integer;
    Jump       : boolean;
    TmpInt     : Integer;

  begin
  if FormIsClosed then exit;
  if PanelsLocked then exit;
  inc(DisableHints);
  try
    LastFileSelected := 0;
    CurFileSelected  := 0;
    if QI_GetCountToShow(DBaseHandle) = 0 then
      begin
      FilesList.Clear;
      FillChar(SubTotals, SizeOf(SubTotals), 0);
      UpdateFileWindowGrid(0);
      UpdateStatusLine(0, true);
      dec(DisableHints);
      exit;
      end;

    FilesList.Clear;
    FilesList.Reversed := QGlobalOptions.ReversedSort;
    Jump := false;
    QI_GetCurDirColSubTotals(DBaseHandle, SubTotals);
    FileColHandle := QI_GetFileCol(DBaseHandle);
    Count := QI_GetFileColCount(FileColHandle);
    MaxFileNameLength := 12;
    MaxFileNamePxLength := DrawGridFiles.Canvas.TextWidth('MMMMMMMM.MMM');
    for i := 0 to pred(Count) do
      begin
      QI_GetOneFileFromCol(FileColHandle, i, OneFile);
      TmpInt := length(OneFile.LongName) + length(OneFile.Ext);
      if TmpInt > MaxFileNameLength then MaxFileNameLength := TmpInt;
      TmpInt := DrawGridFiles.Canvas.TextWidth(OneFile.LongName + OneFile.Ext);
      if TmpInt > MaxFileNamePxLength then MaxFileNamePxLength := TmpInt;
      OneFileLine := TOneFileLine.Create(OneFile);
      if (OneFile.LongName + OneFile.Ext) = SaveFileName then
        begin
        OneFileLine.ExtAttr := OneFileLine.ExtAttr or eaToBeSelected;
        Jump := true;
        end;
      if (SaveFileName[0] = #0) and (DirToScrollHandle <> nil) and
          (OneFileLine.FileType <> ftFile) and
          (OneFile.SubDirCol = DirToScrollHandle) then
        begin
        OneFileLine.ExtAttr := OneFileLine.ExtAttr or eaToBeSelected;
        DirToScrollHandle := nil;
        Jump := true;
        end;
      FilesList.AddObject(GetSortString(OneFile, OneFileLine.FileType,
                                        QGlobalOptions.SortCrit,
                                        QGlobalOptions.ReversedSort),
                          OneFileLine);
      end;

    DirPos := 0;
    if Jump then
      for i := 0 to pred(Count) do
        if (TOneFileLine(FilesList.Objects[i]).ExtAttr and eaToBeSelected) <> 0 then
          begin
          DirPos := i;
          break;
          end;
    UpdateFileWindowGrid(DirPos);
    UpdateStatusLine(DirPos, true);

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  dec(DisableHints);
  end;

//--------------------------------------------------------------------
// Event issued when the user clicks to the tree panel

procedure TFormDBase.OutlineTreeClick(Sender: TObject);
  begin
  if FormIsClosed then exit;
  try
    if not QGlobalOptions.ShowTree then exit;
    with OutlineTree do
      begin
      ///if SelectedItem > 0 then
      if Selected.Count > 0 then
        begin
        ///if Items[SelectedItem].Data <> nil then
        if Items[Selected.Index].Data <> nil then
          begin
          ///QI_SetCurDirCol (DBaseHandle, Items[SelectedItem].Data);
          QI_SetCurDirCol (DBaseHandle, Items[Selected.Index].Data);
          QI_UpdateFileCol(DBaseHandle);
          UpdateHeader;
          end;
        end;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Procedure used to open/execute selected file. Verifies the presence
// of the file and then uses shell to make the action appropriate to the
// file extension.

procedure TFormDBase.ExecFile(POneFile: TPOneFile);
  var
    Msg            : AnsiString;
    RelativePath   : ShortString;
    OrigDiskName   : ShortString;
    OrigVolumeLabel: ShortString;
    BasePath       : ShortString;
    Index          : Integer;
    Attr           : Word;
    FileSystem     : TFileSystem;
    VolumeLabel    : ShortString;
    DriveType      : integer;
    Exists         : boolean;
    InArchive      : boolean;
    DriveAccessible: boolean;
    DriveRemovable : boolean;
    DriveNetwork   : boolean;
    VolumeLabelDiffers: boolean;
    Retry          : boolean;
    iResult        : integer;

  begin
  Retry := true;
  LOG('->ExecFile', []);
  while Retry do
    begin
    Retry := false;
    QI_GetCurrentKeyEx  (DBaseHandle, OrigDiskName, OrigVolumeLabel,
                         BasePath, Attr, Index);
    LOG('BasePath=%s, OrigDiskName=%s, OrigVolumeLabel=%s',
        [BasePath, OrigDiskName, OrigVolumeLabel]);
    QI_GetFullDirPath(DBaseHandle, RelativePath);
    InArchive := QI_IsInArchive (DBaseHandle);
    If InArchive then RelativePath := QI_GetArchivePath(DBaseHandle);

    if length(BasePath) > 0 then // cut the last backslash
      if BasePath[length(BasePath)] = '\' then
        BasePath[0] := char(byte(BasePath[0])-1);

    if (InArchive)
      then
        begin
        // relative path does not have backslash at the end
        LOG('Is in archive', []);
        Msg := BasePath + RelativePath;
        end
      else
        begin
        if RelativePath[length(RelativePath)] <> '\' then RelativePath := RelativePath + '\';
        Msg := BasePath + RelativePath + POneFile^.LongName + POneFile^.Ext;
        end;
    LOG('Msg=%s', [Msg]);
    Exists := FileExists(Msg);

    VolumeLabel := '';
    DriveAccessible := ReadVolumeLabel (BasePath[1] + ':', VolumeLabel,
                                        FileSystem, DriveType);

    ///DriveRemovable := (DriveType = DRIVE_REMOVABLE) or
    ///                  (DriveType = DRIVE_CDROM);

    ///DriveNetwork := DriveType = DRIVE_REMOTE;

    VolumeLabelDiffers := false;
    // OrigVolumeLabel - by the older version of DiskBase is '', otherwise VolumeLabel
    if (length(VolumeLabel) > 0) then
      VolumeLabelDiffers := AnsiUppercase(OrigVolumeLabel) <> AnsiUppercase(VolumeLabel);

    if not Exists
      then
        begin
        Msg := Msg + lsFileCannotBeOpen;
        if not DriveAccessible then
          Msg := Msg + lsDiskNotAccessible;
        if DriveRemovable then
          Msg := Msg + lsDifferentDisk;
        if InArchive then
          Msg := Msg + lsFileInArchive;
        if DriveNetwork then
          Msg := Msg + lsDiskIsNetworkDrive;
        if VolumeLabelDiffers then
          Msg := Msg + lsVolumeLabelDiffers;
        Msg := Msg + lsFileProbablyDeleted;
        Retry := Application.MessageBox(PChar(Msg), lsFileNotFound, MB_RETRYCANCEL) = IDRETRY;
        LOG('Does not exist: %s', [Msg]);
        end
      else
        begin
        LOG('Executing shell: %s', [Msg]);
        {$ifdef mswindows}
        iResult := ShellExecute(Handle, nil, PChar(Msg), nil, nil, SW_SHOWNORMAL);
        if (iResult <= 32) then // failed
          if iResult = SE_ERR_NOASSOC then // no association
            begin
            LOG('No association, retry with relative path', []);
            if RelativePath[length(RelativePath)] = '\' then
              RelativePath[0] := char(byte(RelativePath[0])-1);
            Msg := BasePath + RelativePath;
            iResult := ShellExecute(Handle, nil, PChar(Msg), nil, nil, SW_SHOWNORMAL);
            LOG('Executing shell: %s, result=%d', [Msg, iResult]);
            end;
        {$endif}
        end;
    end;
  end;

//--------------------------------------------------------------------
// Used to open Windows Explorer to display the contents of the folder

procedure TFormDBase.OpenExplorer(POneFile: TPOneFile);
  var
    Msg            : AnsiString;
    RelativePath   : ShortString;
    OrigDiskName   : ShortString;
    OrigVolumeLabel: ShortString;
    BasePath       : ShortString;
    Index          : Integer;
    Attr           : Word;
    FileSystem     : TFileSystem;
    VolumeLabel    : ShortString;
    DriveType      : integer;
    Exists         : boolean;
    InArchive      : boolean;
    DriveAccessible: boolean;
    DriveRemovable : boolean;
    DriveNetwork   : boolean;
    VolumeLabelDiffers: boolean;
    Retry          : boolean;

  begin
  Retry := true;
  LOG('->OpenExplorer', []);
  while Retry do
    begin
    Retry := false;
    InArchive := QI_IsInArchive (DBaseHandle);
    if (InArchive) then
      begin
      ExecFile(POneFile);
      exit;
      end;
    QI_GetCurrentKeyEx  (DBaseHandle, OrigDiskName, OrigVolumeLabel,
                         BasePath, Attr, Index);
    QI_GetFullDirPath(DBaseHandle, RelativePath);
    if length(BasePath) > 0 then // cut the last backslash
      if BasePath[length(BasePath)] = '\' then
        BasePath[0] := char(byte(BasePath[0])-1);
    if RelativePath[length(RelativePath)] <> '\' then RelativePath := RelativePath + '\';
    Msg := BasePath + RelativePath + POneFile^.LongName + POneFile^.Ext;
    Exists := DirExists(Msg);

    VolumeLabel := '';
    DriveAccessible := ReadVolumeLabel (BasePath[1] + ':', VolumeLabel,
                                        FileSystem, DriveType);

    ///DriveRemovable := (DriveType = DRIVE_REMOVABLE) or
    ///                  (DriveType = DRIVE_CDROM);

    ///DriveNetwork := DriveType = DRIVE_REMOTE;

    VolumeLabelDiffers := false;
    if (length(VolumeLabel) > 0) then
      VolumeLabelDiffers := AnsiUppercase(OrigVolumeLabel) <> AnsiUppercase(VolumeLabel);

    if not Exists
      then
        begin
        Msg := Msg + lsFolderCannotBeOpen;
        if not DriveAccessible then
          Msg := Msg + lsDiskNotAccessible;
        if DriveRemovable then
          Msg := Msg + lsDifferentDisk;
        if InArchive then
          Msg := Msg + lsFolderInArchive;
        if DriveNetwork then
          Msg := Msg + lsDiskIsNetworkDrive;
        if VolumeLabelDiffers then
          Msg := Msg + lsVolumeLabelDiffers;
        Msg := Msg + lsFolderProbablyDeleted;
        Retry := Application.MessageBox(PChar(Msg), lsFolderNotFound, MB_RETRYCANCEL) = IDRETRY;
        LOG('Does not exist: %s', [Msg]);
        end
      else
        begin
        LOG('Executing shell: %s', [Msg]);
        ///ShellExecute(Handle, nil, PChar(Msg), nil, nil, SW_SHOWNORMAL);
        end;
    end;
  end;

//--------------------------------------------------------------------
// Event issued when the user double-clicks on the files panel.

procedure TFormDBase.DrawGridFilesDblClick(Sender: TObject);
  var
    i   : Integer;

  begin
  if FormIsClosed then exit;
  if DisableNextDoubleClick then exit;
  EraseFileHint;
  try
    if QGlobalOptions.FileDisplayType = fdBrief
      then
        i := DrawGridFiles.Selection.Left*DrawGridFiles.RowCount +
              DrawGridFiles.Selection.Top
      else
        i := pred(DrawGridFiles.Row);
    if i < FilesList.Count then
      with TOneFileLine(FilesList.Objects[i]) do
        begin
        DirToScrollHandle := nil;
        if FileType = ftParent then
          DirToScrollHandle := QI_GetCurDirCol(DBaseHandle);
        // ShiftState set in Mouse and Key down
        if ssShift in ShiftState // shift - open file or explore folder
          then
            begin
            if (POneFile^.Attr and faDirectory <> 0) and (POneFile^.Attr and faQArchive = 0)
              then
                begin
                if FileType <> ftParent then OpenExplorer(POneFile);
                end
              else
                begin // click on file
                ExecFile(POneFile);
                end;
            end
          else
            if ssCtrl in ShiftState
              then
                begin
                EditDescription;
                end
              else // no shift state
                begin
                if (POneFile^.Attr and (faDirectory or faQArchive) <> 0)
                  then
                    begin
                    if (POneFile^.SubDirCol <> nil) then
                      begin
                      QI_SetCurDirCol (DBaseHandle, POneFile^.SubDirCol);
                      QI_UpdateFileCol(DBaseHandle);
                      UpdateHeader;
                      JumpInOutline(POneFile^.SubDirCol);
                      end;
                    end
                  else
                    begin // click on file
                    if QGlobalOptions.EnableShellExecute then
                      ExecFile(POneFile);
                    end;
                end;
        end; // with
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Procedure used to inverse selection

procedure TFormDBase.DrawGridFilesToggleSelection;
var
  i   : Integer;
begin
if FormIsClosed then exit;
if QGlobalOptions.FileDisplayType = fdBrief
  then
    i := DrawGridFiles.Selection.Left*DrawGridFiles.RowCount +
          DrawGridFiles.Selection.Top
  else
    i := pred(DrawGridFiles.Row);
if i < FilesList.Count then
  begin
  with TOneFileLine(FilesList.Objects[i]) do
    if ExtAttr and eaSelected <> 0
      then ExtAttr := ExtAttr and not eaSelected
      else ExtAttr := ExtAttr or eaSelected;
  end;
end;

//--------------------------------------------------------------------
// Used to select programatically the folder in the tree panel - typically
// when the user double-clicks on a found file, the appropriate disk is
// selected, folder in the tree panel selcted (by this function) and the
// file in the file panel selected

procedure TFormDBase.JumpInOutline(SubDirCol: pointer);

var
  i : Integer;
  Found: boolean;

  begin
  if FormIsClosed then exit;
  if not QGlobalOptions.ShowTree then exit;
  Found := false;
  with OutlineTree do
    ///if SelectedItem > 0 then
    if Selected.Count > 0 then
      ///if Items[SelectedItem].Level > 1 then
      ///  Items[SelectedItem].Collapse;
      if Items[Selected.Index].Level > 1 then
        Items[Selected.Index].Collapse(true);

  for i := 0 to pred(TreePtrCol^.Count) do
    if TreePtrCol^.At(i) = SubDirCol then
      begin
      Found := true;
      break;
      end;
  if Found then
    begin
    inc(i);
    if i > 1 then
      ExpandBranch(OutlineTree.Items[i]);
    ///OutlineTree.SelectedItem := i;
    OutlineTree.Selected.Index := i;
    end;
  end;

//--------------------------------------------------------------------
// Called when global options are changed, so the view of the tree and files
// can be updated

procedure TFormDBase.ChangeGlobalOptions;

  var
    SaveFileName   : ShortString;
    SaveExpandTree : boolean;

  begin
  if FormIsClosed then exit;
  SaveExpandTree := QGlobalOptions.ExpandTree;
  FormSettings.GetOptions(QGlobalOptions);
  SaveFileName := GetSelectedFileName;
  if SaveExpandTree <> QGlobalOptions.ExpandTree then
    begin
    with OutlineTree do
      if QGlobalOptions.ExpandTree
        then
          begin
          BeginUpdate;
          FullExpand;
          EndUpdate;
          end
        else
          begin
          BeginUpdate;
          FullCollapse;
          EndUpdate;
          Items[1].Expand(false);
          end;
    end;
  ShowOrHideTreePanel;
  ResetDrawGridFiles;
  ReloadFileCol(SaveFileName);
  end;

//--------------------------------------------------------------------
// Called when global options are changed, so the view of the deleted
// disks can be updated

procedure TFormDBase.ChangeLocalOptions;

  begin
  if FormIsClosed then exit;
  try
    FormLocalOptions.GetOptions(QLocalOptions);
    QI_GetLocalOptions (DBaseHandle, TmpLocalOptions);
    QI_SetLocalOptions (DBaseHandle, QLocalOptions);
    if QLocalOptions.ShowDeleted <> TmpLocalOptions.ShowDeleted then
      begin
      UpdateDiskWindow;
      QI_SetNeedUpdTreeWin(DBaseHandle);
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Changes the type of display of files to brief

procedure TFormDBase.SetBriefFileDisplay(Brief: boolean);

  var
    SavePos : Integer;

  begin
  if FormIsClosed then exit;
  if Brief and (QGlobalOptions.FileDisplayType = fdBrief) then exit;
  if QGlobalOptions.FileDisplayType = fdBrief
    then with DrawGridFiles do SavePos := Col*RowCount + Row
    else SavePos := pred(DrawGridFiles.Row);
  if Brief
    then QGlobalOptions.FileDisplayType := fdBrief
    else QGlobalOptions.FileDisplayType := fdDetailed;
  ResetDrawGridFiles;
  UpdateFileWindowGrid(SavePos);
  end;

//--------------------------------------------------------------------
// Hides or shows the tree panel

procedure TFormDBase.ShowOrHideTreePanel;

  begin
  if FormIsClosed then exit;
  try
    if QGlobalOptions.ShowTree and not OutlineTree.Visible
      then
        begin
        RescanTree(false);
        if not DBaseIsEmpty then
          JumpInOutline(QI_GetCurDirCol(DBaseHandle));
        OutlineTree.Left := Splitter1.Left + Splitter1.Width + 1;
        OutlineTree.Show;
        // make sure it is on right
        Splitter2.Align := alNone;
        Splitter2.Left := OutlineTree.Left + OutlineTree.Width + 1;
        Splitter2.Align := alLeft;
        Splitter2.Show;
        QI_ClearNeedUpdFileWin(DBaseHandle); // already updated
        HeaderTop.Sections[1].Width := HeaderWidths[1];
        ResizeHeaderBottom;
        RescanTree(true);
        exit;
        end;
    if not QGlobalOptions.ShowTree and OutlineTree.Visible
      then
        begin
        OutlineTree.Hide;
        Splitter2.Hide;
        OutLineTree.Items.Clear;
        HeaderTop.Sections[1].Width := 0;
        ResizeHeaderBottom;
        RescanTree(true);
        exit;
        end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;


//--------------------------------------------------------------------
// Set fonts from global options and recalculates the sizes. Used after
// the global options are changed and at the program startup.

procedure TFormDBase.ResetFontsAndRowHeights;
  begin
  if FormIsClosed then exit;
  with QGlobalOptions do
    begin
{$ifndef LOGFONT}
    DrawGridDisks.Font.Assign       (DiskFont);
    DrawGridDisks.Canvas.Font.Assign(DiskFont);
    OutlineTree.Font.Assign         (TreeFont);
    OutlineTree.Canvas.Font.Assign  (TreeFont);
    DrawGridFiles.Font.Assign       (FileFont);
    DrawGridFiles.Canvas.Font.Assign(FileFont);
    FormDescription.MemoDesc.Font.Assign(DescFont);
{$else}
    SetFontFromLogFont(DrawGridDisks.Font, DiskLogFont);
    SetFontFromLogFont(DrawGridDisks.Canvas.Font, DiskLogFont);

    SetFontFromLogFont(OutlineTree.Font, TreeLogFont);
    SetFontFromLogFont(OutlineTree.Canvas.Font, TreeLogFont);

    SetFontFromLogFont(DrawGridFiles.Font, FileLogFont);
    SetFontFromLogFont(DrawGridFiles.Canvas.Font, FileLogFont);

    SetFontFromLogFont(FormDescription.MemoDesc.Font, DescLogFont);
{$endif}
    end;
  DrawGridDisks.DefaultRowHeight := DrawGridDisks.Canvas.TextHeight('My');
  DrawGridFiles.DefaultRowHeight := DrawGridFiles.Canvas.TextHeight('My');
  TimeWidth := DrawGridFiles.Canvas.TextWidth(DosTimeToStr(longint(23) shl 11,
                                              QGlobalOptions.ShowSeconds));
  TimeWidth := TimeWidth + TimeWidth div 6;
  ///if OutlineTree.Font.Size < 9
  ///  then OutlineTree.OutlineStyle := osTreeText
  ///  else OutlineTree.OutlineStyle := osTreePictureText;
  end;

//--------------------------------------------------------------------
// Updates the status line with the information about the currently selected file

procedure TFormDBase.UpdateStatusLine(Index: Integer; SubTotalsToo: boolean);

  var
    OneFileLine: TOneFileLine;
    DosDateTime: longint;
    Description: TFilePointer;
    S          : ShortString;

  begin
  if FormIsClosed then exit;
  try
    StatusLineFiles := ' ';
    if (FilesList.Count > 0) and (Index >= 0) and (Index < FilesList.Count) then
      begin
      OneFileLine := TOneFileLine(FilesList.Objects[Index]);
      with OneFileLine do
        begin
        LastSelectedFile := POneFile^.LongName + POneFile^.Ext;
        StatusLineFiles  := StatusLineFiles + POneFile^.LongName + POneFile^.Ext + '   ';
        DosDateTime := POneFile^.Time;
        case FileType of
          ftDir   : StatusLineFiles := StatusLineFiles + lsFolder1;
          ftParent: ;
          else      StatusLineFiles := StatusLineFiles + FormatSize(POneFile^.Size, QGlobalOptions.ShowInKb) + '   ';
          end;
        if DosDateTime <> 0 then
          StatusLineFiles := StatusLineFiles + DosDateToStr(DosDateTime) + ' ' +
                   DosTimeToStr(DosDateTime, QGlobalOptions.ShowSeconds) + '   ';
        Description := POneFile^.Description;
        if Description <> 0 then
          begin
          if QI_GetShortDesc(DBaseHandle, Description, S) then
            StatusLineFiles := StatusLineFiles + '   ' + S;
          end;
        end;
      end;
    if SubTotalsToo then
      begin
      StatusLineSubTotals := ' ';
      if (SubTotals.DataFileSize > 0) then
        begin
        if (SubTotals.PhysDirs = SubTotals.DataDirs) and
           (SubTotals.PhysFiles = SubTotals.DataFiles) and
           (SubTotals.PhysFileSize = SubTotals.DataFileSize)
             then
               begin
               StatusLineSubTotals := lsTotalFolders +
               IntToStr(SubTotals.PhysDirs) +
               lsTotalFiles +
               IntToStr(SubTotals.PhysFiles) +
               lsTotalSize +
               FormatBigSize(SubTotals.PhysFileSize);
               end
             else
               begin
               StatusLineSubTotals := lsTotalFolders +
               IntToStr(SubTotals.PhysDirs) +
               '/' + IntToStr(SubTotals.DataDirs) +
               lsTotalFiles +
               IntToStr(SubTotals.PhysFiles) +
               '/' + IntToStr(SubTotals.DataFiles) +
               lsTotalSize +
               FormatBigSize(SubTotals.PhysFileSize)+
               '/' + FormatBigSize(SubTotals.DataFileSize);
               end;
        end;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Updates the header of the panels

procedure TFormDBase.UpdateHeader;

  var
    S      : ShortString;
    Attr   : Word;
    Index  : Integer;

  begin
  if FormIsClosed then exit;
  try
    if (ActiveControl is TDrawGrid) and
      (TDrawGrid(ActiveControl).Tag = 1) and
        (HeaderTop.Sections[0].Text <> ShortDBaseFileName) then
          begin
          HeaderTop.Sections[0].Text := ShortDBaseFileName;
          HeaderTop.Sections[0].Width := HeaderWidths[0];
          end;
    {$ifdef mswindows}
    if QGlobalOptions.ShowTree and (ActiveControl is TTreeView) and
      (TOutline(ActiveControl).Tag = 2) then
        begin
        if QI_GetCurrentKey(DBaseHandle, S, Attr, Index) then
          begin
          if HeaderTop.Sections.Strings[1] <> S then
            begin
            HeaderTop.Sections.Strings[1] := S;
            HeaderTop.SectionWidth[1] := HeaderWidths[1];
            end;
          end;
        end;
    if (ActiveControl is TDrawGrid) and
      (TDrawGrid(ActiveControl).Tag = 3) then
        begin
        QI_GetFullDirPath(DBaseHandle, S);
        if HeaderTop.Sections.Strings[2] <> S then
          begin
          HeaderTop.Sections.Strings[2] := S;
          HeaderTop.SectionWidth[2] := HeaderWidths[2];
          end;
        end;
    {$endif}
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;


//--------------------------------------------------------------------
// Updates the size of the sections of the bottom header (containing the status line)

procedure TFormDBase.ResizeHeaderBottom;

  var
    SizeNeeded: Integer;

  begin
  SizeNeeded := HeaderTop.Sections[0].Width+HeaderTop.Sections[1].Width;
  if SizeNeeded < StatusSection0Size then SizeNeeded := StatusSection0Size;
  HeaderBottom.Sections[0].Width := SizeNeeded;
  end;

//--------------------------------------------------------------------
// Sets the flag for resorting in the idle time

procedure TFormDBase.SetNeedResort(SortCrit: TSortCrit);

  begin
  NeedResort := true;
  if QGlobalOptions.SortCrit = SortCrit
    then QGlobalOptions.ReversedSort := not QGlobalOptions.ReversedSort
    else QGlobalOptions.ReversedSort := false;
  QGlobalOptions.SortCrit := SortCrit;
  end;

//--------------------------------------------------------------------
// Handles the event issued when the user clicks by mouse in the file panel

procedure TFormDBase.DrawGridFilesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  var
    i, Current: Integer;
    TotalWidth: Integer;

  begin
  if FormIsClosed then exit;
  ShiftState := Shift;
  EraseFileHint;
  DisableNextDoubleClick := false;
  if QGlobalOptions.FileDisplayType = fdDetailed then
    with DrawGridFiles do
      if Y < DefaultRowHeight then
        begin
        DisableNextDoubleClick := true;
        TotalWidth := 0;
        for i := 0 to pred(ColCount) do
          with ColumnAttrib[i] do
            begin
            inc(TotalWidth, Width);
            if X < TotalWidth then
              begin
              case Content of
                coName:
                  if X > TotalWidth - Width div 2
                    then SetNeedResort(scExt)
                    else SetNeedResort(scName);
                coTime     : SetNeedResort(scTime);
                coSize     : SetNeedResort(scSize);
                end;
              break;
              end;
            end;
        exit;
        end;

  if ssCtrl in Shift then DrawGridFilesToggleSelection;

  if ssShift in Shift then
    begin
    if LastFileSelected >= FilesList.Count then
      LastFileSelected := pred(FilesList.Count);
    for i := 0 to pred(FilesList.Count) do
      with TOneFileLine(FilesList.Objects[i]) do
        ExtAttr := ExtAttr and not eaSelected;
    if QGlobalOptions.FileDisplayType = fdBrief
      then Current := DrawGridFiles.Selection.Left*DrawGridFiles.RowCount +
              DrawGridFiles.Selection.Top
      else Current := pred(DrawGridFiles.Row);

    if LastFileSelected <= Current
      then
        for i := LastFileSelected to Current do
          with TOneFileLine(FilesList.Objects[i]) do
            ExtAttr := ExtAttr or eaSelected
      else
        for i := LastFileSelected downto Current do
          with TOneFileLine(FilesList.Objects[i]) do
            ExtAttr := ExtAttr or eaSelected;
    DrawGridFiles.Repaint;
    end;

  end;

//--------------------------------------------------------------------
// Determines, whether the database is empty

function TFormDBase.DBaseIsEmpty: boolean;

  begin
  Result := true;
  if FormIsClosed then exit;
  Result := QI_GetCountToShow(DBaseHandle) = 0;
  end;

//--------------------------------------------------------------------
// Error message displayed when a critical error was encountered

procedure TFormDBase.MsgShouldExit;

  begin
  Application.MessageBox(lsSeriousProblemWritingToDBase,
      lsError, mb_Ok or mb_IconStop);
  end;

//--------------------------------------------------------------------
// Scans a disk to the database.

function TFormDBase.ScanDisk (Drive: char; AutoRun: boolean): boolean;

  begin
  Result := true;
  if FormIsClosed then exit;
  if DBaseIsReadOnly then exit;
  try
    PanelsLocked := true;
    Result := DoScan(Drive, '', '', AutoRun, false);
    PanelsLocked := false;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Rescans existing disk in the database

procedure TFormDBase.RescanDisk;

  var
    OrigDiskName, OrigVolumeLabel, OrigPath: ShortString;
    Index: Integer;
    Attr : Word;
    RealVolumeLabel: ShortString;
    SaveStr1, SaveStr2: ShortString;
    MsgText: array[0..1024] of char;
    OK: boolean;

  begin
  QI_GetCurrentKeyEx  (DBaseHandle, OrigDiskName, OrigVolumeLabel,
                       OrigPath, Attr, Index);
  if Attr and kaDeleted <> 0 then exit;
  if length(OrigPath) = 2 then OrigPath := OrigPath + '\';
  // save the contents of the form for usage as scan folder
  SaveStr1 := FormScanFolder.EditFolder.Text;
  SaveStr2 := FormScanFolder.EditDiskName.Text;
  FormScanFolder.EditFolder.Text    := OrigPath;
  FormScanFolder.EditDiskName.Text  := OrigDiskName;
  FormScanFolder.SetAppearance(false);
  OK := FormScanFolder.ShowModal = mrOk;
  FormScanFolder.SetAppearance(true);
  OrigPath        := FormScanFolder.Directory;
  OrigDiskName    := FormScanFolder.DiskName;
  RealVolumeLabel := FormScanFolder.VolumeLabel;
  FormScanFolder.EditFolder.Text   := SaveStr1;
  FormScanFolder.EditDiskName.Text := SaveStr2;
  if OK then
    begin
    if AnsiUpperCase(OrigVolumeLabel) <> AnsiUpperCase(RealVolumeLabel) then
      begin
      StrPCopy(MsgText, Format(lsVolumesDifferent1, [RealVolumeLabel, OrigVolumeLabel]));
      StrCat(MsgText, lsVolumesDifferent2);
      StrCat(MsgText, lsVolumesDifferent3);
      if Application.MessageBox(MsgText, lsWarning, MB_YESNOCANCEL) <> IDYES then exit;
      end;
    ScanFolder(OrigPath, OrigDiskName, RealVolumeLabel, true);
    end;
  end;

//--------------------------------------------------------------------
// Scans a folder to database as a disk

procedure TFormDBase.ScanFolder(Directory, DiskName, VolumeLabel: ShortString;
                                NoOverWarning: boolean);

  begin
  if FormIsClosed then exit;
  if DBaseIsReadOnly then exit;
  try
    PanelsLocked := true;
    DoScan(Directory[1], Directory, DiskName, false, NoOverWarning);
    PanelsLocked := false;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Does the actual scan of the disk, returns false when interrupted by the user

function TFormDBase.DoScan (Drive: char;
                            StartPath: ShortString; DiskName: ShortString;
                            AutoRun, NoOverWarning: boolean): boolean;

  var
    VolumeLabel        : ShortString;
    MsgText, MsgCaption: array[0..256] of char;
    TmpKey             : ShortString;
    KeyPos             : Integer;
    SaveKey            : ShortString;
    SaveSelected       : Integer;
    SaveAttr           : Word;
    SavePath           : ShortString;
    SaveFile           : ShortString;
    FileSystem         : TFileSystem;
    DriveType          : integer;
    DoRepeat           : boolean;
    KeyToBeDeletedAtExit: integer;
    TmpStr10           : String[10]; 
    i                  : integer;

  begin
  // the exceptions are handled in the caller functions (ScanDisk, ...)
  Result := false;
  if FormIsClosed then exit;
  if DBaseIsReadOnly then exit;
  SaveKey := '';
  KeyToBeDeletedAtExit := -1;
  SaveAttr := 0;
  SaveSelected := 0;
  SavePath := '';
  QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
  QI_GetFullDirPath (DBaseHandle, SavePath);
  SaveFile := LastSelectedFile;

  if not ReadVolumeLabel (Drive + ':', VolumeLabel, FileSystem, DriveType) then
    begin
    Application.MessageBox(
      StrPCopy(MsgText, lsDrive + Drive + ':' + lsIsNotAccesible),
      StrPCopy(MsgCaption, lsError), mb_Ok);
    exit;
    end;

  if DiskName = '' then
    begin
    DiskName := VolumeLabel;
    with FormAskForLabel do
      begin
      Caption := Drive + ':';
      if DiskName <> '' then
        begin
        TmpKey := AnsiUpperCase(DiskName);
        if TmpKey = DiskName then
          begin
          DiskName := AnsiLowerCase(DiskName);
          DiskName[1] := TmpKey[1];
          end;
        end;
      GeneratedName := '';
      if QLocalOptions.DiskNamePattern <> '' then
        begin
        GeneratedName := QLocalOptions.DiskNamePattern;
        i := Pos('#N', GeneratedName);
        if i = 0 then i := Pos('#n', GeneratedName);
        if (i>0) then
          begin
          ShortDelete(GeneratedName, i, 2);
          TmpStr10 := IntToStr(QLocalOptions.DiskCounter+1);
          ShortInsert(TmpStr10, GeneratedName, i);
          end;
        i := Pos('#D', GeneratedName);
        if i = 0 then i := Pos('#d', GeneratedName);
        if (i>0) then
          begin
          ShortDelete(GeneratedName, i, 2);
          ShortInsert(DiskName, GeneratedName, i);
          end;
        end;
      EditDiskName.Text := TrimSpaces(DiskName);
      DoRepeat := true;
      while DoRepeat do
        begin
        DoRepeat := false;
        if not AutoRun or (EditDiskName.Text = '') then
          if ShowModal <> mrOk then exit;
        EditDiskName.Text := TrimSpaces(EditDiskName.Text);
        if EditDiskName.Text = '' then exit;
        {$ifdef mswindows}
        {$ifndef DELPHI1}
        if (AnsiLowerCase(VolumeLabel) <> AnsiLowerCase(EditDiskName.Text))
          and ((DriveType=0) or (DriveType=DRIVE_REMOVABLE) or (DriveType=DRIVE_FIXED)
          and not QLocalOptions.DisableVolNameChange)
         then
          begin
          i := Application.MessageBox(
            StrPCopy(MsgText, lsLabelDifferent_Change),
            StrPCopy(MsgCaption, lsDifferentNames), mb_YesNoCancel);
          if i = IDCANCEL then exit;
          if i = IDYES then
              begin
              if (FileSystem = FAT) and (length(EditDiskName.Text) > 12)
                then
                  begin
                  Application.MessageBox(
                    StrPCopy(MsgText, lsOnDisk + Drive + ':' +
                            lsLimitedLength),
                    StrPCopy(MsgCaption, lsError), mb_Ok);
                  DoRepeat := true;
                  end
                else
                  begin
                  if not WriteVolumeLabel (Drive + ':', EditDiskName.Text)
                    then
                      Application.MessageBox(
                        StrPCopy(MsgText, lsVolumeLabel + Drive + ':' + lsCannotBeChanged),
                        StrPCopy(MsgCaption, lsError), mb_Ok)
                    else
                      VolumeLabel := EditDiskName.Text; // we need to save the proper volume label
                  end;
              end;
          end;
      {$endif}
      {$endif}
        end; {while}
      DiskName := EditDiskName.Text;
      if GeneratedNameUsed then inc(QLocalOptions.DiskCounter);
      end; {with}
    end;

  TmpKey := DiskName;
  if QI_GetCountToShow(DBaseHandle) > 0 then // to delete
    begin
    if QI_KeySearch(DBaseHandle, TmpKey, KeyPos) then // existing found
      begin
      // TmpKey is filled by the item from KeyField - by the QI_KeySearch proc.}
      if not AutoRun and QLocalOptions.OverwriteWarning and not NoOverWarning then
        if Application.MessageBox(
             StrPCopy(MsgText, lsOverwriteExistingRecord + DiskName + '?'),
             StrPCopy(MsgCaption, lsWarning), mb_YesNoCancel) <> IdYes
               then exit;
      QI_ReadDBaseEntryToSaved(DBaseHandle, KeyPos);
      // we save the tree for faster scanning
      KeyToBeDeletedAtExit := KeyPos;
      end;
    end;

  QI_ClearSearchCol(DBaseHandle);
  QI_ClearTreeStruct(DBaseHandle);

  FormScanProgress.DBaseHandle   := DBaseHandle;
  FormScanProgress.Drive         := Drive;
  FormScanProgress.DiskName      := DiskName;
  FormScanProgress.VolumeLabel   := VolumeLabel;
  FormScanProgress.StartPath     := StartPath;
  FormScanProgress.ScanningQDir4 := false;
  FormScanProgress.ShowModal;
  if FormScanProgress.Success then
    begin
    if (KeyToBeDeletedAtExit <> -1) then
      begin
      // firts we must save TreeStruct, otherwise it would be deleted when the disk is deleted
      QI_SaveTreeStruct(DBaseHandle);
      QI_DeleteDBaseEntry(DBaseHandle, KeyToBeDeletedAtExit); //no error checking
      // restore TreeStruct from the backup
      QI_RestoreTreeStruct(DBaseHandle);
      end;
    if not QI_AppendNewDBaseEntry(DBaseHandle) then // here is also the change in keyfield
      begin
      MsgShouldExit;
      QI_ClearTreeStruct(DBaseHandle);
      JumpTo(SaveKey, SavePath, SaveFile);
      exit;
      end;
    end;
  QI_ClearTreeStruct(DBaseHandle);
  QI_ClearSavedTreeStruct(DBaseHandle);
  QI_GoToKey(DBaseHandle, TmpKey);
  Result := FormScanProgress.Success;
  end;

//--------------------------------------------------------------------
// Import from QuickDir 4.1 - predecessor of DiskBase

procedure TFormDBase.ImportFromQDir41(FileName: ShortString);

  var
    Response: integer;

  begin
  {$ifdef CZECH}
  if FormIsClosed then exit;
  if DBaseIsReadOnly then exit;
  Response := Application.MessageBox(lsImportConversion,
    lsImportFrom4, MB_YESNOCANCEL or MB_DEFBUTTON2);
  if Response = IDCANCEL then exit;
  QI_SetQDir4CharConversion (Response = IDYES);
  try
    PanelsLocked := true;
    QI_OpenQDir4Database(FileName);
    ScanQDir4Database;
    QI_CloseQDir4Database;
    PanelsLocked := false;
    UpdateDiskWindow;
    ReloadTree;
    ReloadFileCol('');
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  {$endif}
  end;

//--------------------------------------------------------------------
// Import from QuickDir 4.1 - predecessor of DiskBase

procedure TFormDBase.ScanQDir4Database;

  var
    TmpKey             : ShortString;
    SaveKey            : ShortString;
    SaveSelected       : Integer;
    SaveAttr           : Word;
    SavePath           : ShortString;
    SaveFile           : ShortString;

  begin
  SaveKey := '';
  SaveAttr := 0;
  SaveSelected := 0;
  SavePath := '';
  SaveFile := LastSelectedFile;
  if FormIsClosed then exit;
  if DBaseIsReadOnly then exit;
  QI_ClearSearchCol(DBaseHandle);
  QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
  QI_GetFullDirPath (DBaseHandle, SavePath);

  FormScanProgress.DBaseHandle := DBaseHandle;
  FormScanProgress.Drive := 'X';
  FormScanProgress.ScanningQDir4 := true;
  FormScanProgress.ShowModal;

  if not FormScanProgress.Success
    then
      begin
      QI_ClearTreeStruct(DBaseHandle);
      QI_ClearSavedTreeStruct(DBaseHandle);
      JumpTo(SaveKey, SavePath, SaveFile);
      end
    else
      begin
      QI_ClearTreeStruct(DBaseHandle);
      QI_ClearSavedTreeStruct(DBaseHandle);
      QI_GoToKey(DBaseHandle, TmpKey);
      end;
  end;

//--------------------------------------------------------------------
// Finds all filters available. Filters are DLLs with the extension renamed to q32

procedure TFormDBase.FindConvertDLLs;

var
  FindMask   : ShortString;
  QSearchRec : TSearchRec;
  OneFileName: ShortString;
  QResult    : Integer;

begin
FindMask := GetProgramDir + '*.q32';
QResult := SysUtils.FindFirst (FindMask, faArchive or faReadOnly, QSearchRec);
while QResult = 0 do
  begin
  OneFileName := QSearchRec.Name;
  Dec(OneFileName[0], 4);
  ConvertDLLs.Add(OneFileName);
  QResult := SysUtils.FindNext(QSearchRec);
  end;
SysUtils.FindClose(QSearchRec);
end;

//--------------------------------------------------------------------
// Deletes a disk from the database

procedure TFormDBase.DeleteRecord;

  var
    Key   : ShortString;
    Attr  : Word;
    KeyPos: Integer;
    MsgText, MsgCaption: array[0..256] of char;
    SelectedCount : longint;

  begin
  if FormIsClosed then exit;
  if DBaseIsReadOnly then exit;
  try
    if not QI_GetCurrentKey(DBaseHandle, Key, Attr, KeyPos) then exit;
    SelectedCount := QI_GetSelectedCount(DBaseHandle);
    if SelectedCount > 0
      then
        begin
        if Application.MessageBox(
           StrPCopy(MsgText, lsRecordsSelected + IntToStr(SelectedCount) +
                    lsDeleteAll),
           StrPCopy(MsgCaption, lsConfirmation), mb_YesNo) <> IdYes
                   then exit;

        QI_DeleteSelectedDBaseEntries(DBaseHandle);
        end
      else
        begin
        if Attr and kaDeleted <> 0 then exit;
           //DBaseHandle is empty
        if Application.MessageBox(
             StrPCopy(MsgText, lsDeleteRecord + Key + '"?'),
             StrPCopy(MsgCaption, lsConfirmation), mb_YesNo) <> IdYes
                     then exit;
        QI_DeleteDBaseEntry(DBaseHandle, KeyPos);
        end;
    QI_ClearTreeStruct(DBaseHandle);
    QI_GoToKey(DBaseHandle, Key);
    if QI_GetCountToShow(DBaseHandle) = 0 then
      begin
      QI_SetNeedUpdDiskWin (DBaseHandle);
      QI_SetNeedUpdTreeWin (DBaseHandle);
      QI_SetNeedUpdFileWin (DBaseHandle);
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Determines whether the current disk can be undeleted

function TFormDBase.CanUndeleteRecord: boolean;

  var
    Key   : ShortString;
    Attr  : Word;
    KeyPos: Integer;

  begin
  Result := false;
  if FormIsClosed then exit;
  if DBaseIsEmpty then exit;
  if not QI_GetCurrentKey(DBaseHandle, Key, Attr, KeyPos) then exit;
  Result := Attr and kaDeleted <> 0;
  end;

//--------------------------------------------------------------------
// Undeletes a disk

procedure TFormDBase.UndeleteRecord;

  var
    Key   : ShortString;
    Attr  : Word;
    KeyPos: Integer;
    TmpKey        : ShortString;
    TmpKeyPos     : Integer;

  begin
  if FormIsClosed then exit;
  if DBaseIsReadOnly then exit;
  try
    if not QI_GetCurrentKey(DBaseHandle, Key, Attr, KeyPos) then exit;
    if Attr and kaDeleted = 0 then exit;
    TmpKey := Key;
    while QI_KeySearch(DBaseHandle, TmpKey, TmpKeyPos) do
      begin
      with FormRenameDlg do
        begin
        Caption := lsNewNameForUndeletedDisk;
        Edit1.Text := TmpKey;
        Label2.Caption := lsDiskWithName + TmpKey + lsAlreadyExists;
        if ShowModal <> mrOK then exit;
        TmpKey := Edit1.Text;
        end;
      end;

    QI_RenameDBaseEntry(DBaseHandle, KeyPos, TmpKey, true);
    QI_ClearTreeStruct(DBaseHandle);
    QI_GoToKey(DBaseHandle, TmpKey);
    if QI_GetCountToShow(DBaseHandle) = 0 then
      begin
      QI_SetNeedUpdDiskWin (DBaseHandle);
      QI_SetNeedUpdTreeWin (DBaseHandle);
      QI_SetNeedUpdFileWin (DBaseHandle);
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Changes the disk name in the database

procedure TFormDBase.ChangeDiskName;

  var
    Key   : ShortString;
    Attr  : Word;
    KeyPos: Integer;
    TmpKey   : ShortString;
    TmpKeyPos: Integer;

  begin
  if FormIsClosed then exit;
  try
    if not QI_GetCurrentKey(DBaseHandle, Key, Attr, KeyPos) then exit;
    if Attr and kaDeleted <> 0 then exit;
    with FormRenameDlg do
      begin
      Edit1.Text := Key;
      Caption := lsDiskNameChange;
      Label2.Caption :=  '';
      if ShowModal <> mrOK then exit;
      TmpKey := Edit1.Text;
      while (AnsiUpperCase(Key) <> AnsiUpperCase(TmpKey))
       and QI_KeySearch(DBaseHandle, TmpKey, TmpKeyPos) do
        begin
        Caption := lsNameMustBeUnique;
        Edit1.Text := TmpKey;
        Label2.Caption := lsDiskWithName + TmpKey + lsAlreadyExists;
        if ShowModal <> mrOK then exit;
        TmpKey := Edit1.Text;
        end;
      QI_RenameDBaseEntry(DBaseHandle, KeyPos, TmpKey, false);
      QI_ClearTreeStruct(DBaseHandle);
      QI_GoToKey(DBaseHandle, Key);
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Searches for the folder/file/description by the text

procedure TFormDBase.SearchName;

  var
    FormFoundFileList: TFormFoundFileList;
    FoundTitle  : ShortString;
    SaveKey     : ShortString;
    SaveSelected: Integer;
    SaveAttr    : Word;
    SavePath    : ShortString;
    SaveFile    : ShortString;
    DBaseInfo   : TDBaseInfo;

  begin
  if FormIsClosed then exit;
  if FormSearchFileDlg.ShowModal <> mrOK then exit;

  // verify, whether the user wants to search in selected disks and
  // does not have any disk selected
  QI_GetDBaseInfo(DBaseHandle, DBaseInfo);
  if (DBaseInfo.iSelectedDisks = 0) and
     (FormSearchFileDlg.DlgData.SearchIn = siSelectedDisks) then
    begin
    Application.MessageBox(lsNoSelectedDisk, lsCannotSearch, mb_OK);
    exit;
    end;

  if (DBaseInfo.iNotSelectedDisks = 0) and
     (FormSearchFileDlg.DlgData.SearchIn = siNotSelectedDisks) then
    begin
    Application.MessageBox(lsAllDisksSelected, lsCannotSearch, mb_OK);
    exit;
    end;

  FormSearchName.DBaseHandle := DBaseHandle;
  try
    QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
    QI_GetFullDirPath (DBaseHandle, SavePath);
    SaveFile := LastSelectedFile;

    if FormSearchName.ShowModal = mrOk then
      begin
      if QGlobalOptions.FoundToNewWin
        then FormFoundFileList := nil
        else FormFoundFileList := TFormFoundFileList(MainForm.GetFoundForm(foFoundFile));
      if FormFoundFileList = nil
        then
          begin
          FormFoundFileList := TFormFoundFileList.Create(Self);
          FormFoundFileList.Tag := Self.Tag;
          FormFoundFileList.DBaseWindow := Self;
          end
        else
          FormFoundFileList.BringToFront;
      FoundTitle := Caption + ': ' + FormSearchFileDlg.ComboBoxMask.Text;
      if length(FoundTitle) > 30 then
        FoundTitle := ShortCopy(FoundTitle, 1, 26) + ' ...';
      FormFoundFileList.Caption := FoundTitle;
      FormFoundFileList.GetList(DBaseHandle);
      end;

    JumpTo(SaveKey, SavePath, SaveFile);

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Search for the files selected in the file panel

procedure TFormDBase.SearchSelected;

var
  FormFoundFileList: TFormFoundFileList;
  FoundTitle  : ShortString;
  SaveKey     : ShortString;
  SaveSelected: Integer;
  SaveAttr    : Word;
  SavePath    : ShortString;
  SaveFile    : ShortString;
  TmpS        : ShortString;
  i           : Integer;

  begin
  if FormIsClosed then exit;
  try
    with FormSearchFileDlg.DlgData do
      begin
      MaxLines     := 10000;
      CaseSensitive:= false ;
      SearchIn     := siWholedBase;
      ScanFileNames:= true;
      ScanDirNames := true;
      ScanDirLevel := -1;
      ScanDesc     := false;
      MoreOptions  := false;
      AddWildCards := false;
      AsPhrase     := false;

      Mask := GetSelectedFileName;
      if pos(' ', Mask) > 0
        then Mask :=  '"' + Mask + '" '
        else Mask := Mask + ' ';
      for i := 0 to pred(FilesList.Count) do
        with TOneFileLine(FilesList.Objects[i]) do
          if ExtAttr and eaSelected <> 0 then
            begin
            TmpS := POneFile^.LongName + POneFile^.Ext;
            if pos(' ', TmpS) > 0
              then TmpS :=  '"' + TmpS + '" '
              else TmpS := TmpS + ' ';
            { --- ifdef DELPHI1}
            if (length(TmpS) + length(Mask)) < 255
              then
                Mask := Mask + TmpS
              else
                begin
                Application.MessageBox(lsTooManyFilesSelected,
                  lsNotification, mb_OK or mb_IconInformation);
                break;
                end;
            end;
      end;
    FormSearchName.DBaseHandle := DBaseHandle;

    QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
    QI_GetFullDirPath (DBaseHandle, SavePath);
    SaveFile := LastSelectedFile;

    if FormSearchName.ShowModal = mrOk then
      begin
      if QGlobalOptions.FoundToNewWin
        then FormFoundFileList := nil
        else FormFoundFileList := TFormFoundFileList(MainForm.GetFoundForm(foFoundFile));
      if FormFoundFileList = nil
        then
          begin
          FormFoundFileList := TFormFoundFileList.Create(Self);
          FormFoundFileList.Tag := Self.Tag;
          FormFoundFileList.DBaseWindow := Self;
          end
        else
          FormFoundFileList.BringToFront;
      FoundTitle := Caption + ': ' + FormSearchFileDlg.DlgData.Mask;
      if length(FoundTitle) > 30 then
        FoundTitle := ShortCopy(FoundTitle, 1, 26) + ' ...';
      FormFoundFileList.Caption := FoundTitle;
      FormFoundFileList.GetList(DBaseHandle);
      end;

    JumpTo(SaveKey, SavePath, SaveFile);

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Search called when the user specifies it on the command line

procedure TFormDBase.SearchParam(ParamFindFile: ShortString);

var
  FormFoundFileList: TFormFoundFileList;
  FoundTitle  : ShortString;
  SaveKey     : ShortString;
  SaveSelected: Integer;
  SaveAttr    : Word;
  SavePath    : ShortString;
  SaveFile    : ShortString;

  begin
  if FormIsClosed then exit;
  with FormSearchFileDlg.DlgData do
    begin
    SortArr[1]   := soName;
    SortArr[2]   := soTime;
    SortArr[3]   := soKey;
    MaxLines     := 1000;
    CaseSensitive:= false ;
    SearchIn     := siWholedBase;
    ScanFileNames:= true;
    ScanDirNames := true;
    ScanDesc     := false;
    ScanDirLevel := -1;
    MoreOptions  := false;
    Mask         := ParamFindFile;
    AddWildCards:= false;
    AsPhrase     := false;
    end;
  FormSearchName.DBaseHandle := DBaseHandle;
  try
    QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
    QI_GetFullDirPath (DBaseHandle, SavePath);
    SaveFile := LastSelectedFile;

    if FormSearchName.ShowModal = mrOk then
      begin
      if QGlobalOptions.FoundToNewWin
        then FormFoundFileList := nil
        else FormFoundFileList := TFormFoundFileList(MainForm.GetFoundForm(foFoundFile));
      if FormFoundFileList = nil
        then
          begin
          FormFoundFileList := TFormFoundFileList.Create(Self);
          FormFoundFileList.Tag := Self.Tag;
          FormFoundFileList.DBaseWindow := Self;
          end
        else
          FormFoundFileList.BringToFront;
      FoundTitle := Caption + ': ' + FormSearchFileDlg.DlgData.Mask;
      if length(FoundTitle) > 30 then
        FoundTitle := ShortCopy(FoundTitle, 1, 26) + ' ...';
      FormFoundFileList.Caption := FoundTitle;
      FormFoundFileList.GetList(DBaseHandle);
      end;

    JumpTo(SaveKey, SavePath, SaveFile);

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Search for disks according to the sizes

procedure TFormDBase.SearchEmpty;

var
  FormFoundEmptyList: TFormFoundEmptyList;
  FoundTitle  : ShortString;
  SaveKey     : ShortString;
  SaveSelected: Integer;
  SaveAttr    : Word;
  SavePath    : ShortString;
  SaveFile    : ShortString;

  begin
  if FormIsClosed then exit;
  FormSearchEmpty.DBaseHandle := DBaseHandle;
  try
    QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
    QI_GetFullDirPath (DBaseHandle, SavePath);
    SaveFile := LastSelectedFile;

    if FormSearchEmpty.ShowModal = mrOk then
      begin
      if {QGlobalOptions.FoundToNewWin} false // not necessary at all
        then FormFoundEmptyList := nil
        else FormFoundEmptyList := TFormFoundEmptyList(MainForm.GetFoundForm(foFoundEmpty));
      if FormFoundEmptyList = nil
        then
          begin
          FormFoundEmptyList := TFormFoundEmptyList.Create(Self);
          FormFoundEmptyList.Tag := Self.Tag;
          FormFoundEmptyList.DBaseWindow := Self;
          end
        else
          FormFoundEmptyList.BringToFront;
      FoundTitle := Caption + lsListOfFreeSpace;
      FormFoundEmptyList.Caption := FoundTitle;
      FormFoundEmptyList.GetList(DBaseHandle);
      end;

    JumpTo(SaveKey, SavePath, SaveFile);

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Shows the window with the information about the disk

procedure TFormDBase.ShowDiskInfo;

  var
    TreeInfo: TTreeInfo;

  begin
  if FormIsClosed then exit;
  try
    QI_GetTreeInfo(DBaseHandle, TreeInfo);
    FormDiskInfo.SetValues(TreeInfo);
    FormDiskInfo.ShowModal;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Shows the window with the information about the database

procedure TFormDBase.ShowDBaseInfo;

  var
    DBaseInfo: TDBaseInfo;

  begin
  if FormIsClosed then exit;
  try
    QI_GetDBaseInfo(DBaseHandle, DBaseInfo);
    FormDBaseInfo.SetValues(DBaseInfo, ShortDBaseFileName);
    FormDBaseInfo.ShowModal;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Jumps to the selected disk-folder-file

procedure TFormDBase.JumpTo(Disk, Dir, FileName: ShortString);

var
  KeyIndex    : Integer;
  Key         : ShortString;
  Attr        : Word;

  begin
  if FormIsClosed then exit;
  try
    QI_GoToKey(DBaseHandle, Disk);
    QI_ClearNeedUpdDiskWin(DBaseHandle);
    if QI_GetCurrentKey (DbaseHandle, Key, Attr, KeyIndex) then
      if DrawGridDisks.Row <> KeyIndex then
        if KeyIndex < DrawGridDisks.RowCount
          then DrawGridDisks.Row := KeyIndex;

    // the change of Row causes SelectCell call - need to reset the NeedUpdTree
    ReloadTree;
    QI_ClearNeedUpdTreeWin(DBaseHandle);
    QI_GoToDir(DBaseHandle, Dir);
    JumpInOutline(QI_GetCurDirCol(DBaseHandle));

    ReloadFileCol(FileName);
    QI_ClearNeedUpdFileWin(DBaseHandle);
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Shows the window for editing the description

procedure TFormDBase.EditDescription;

  var
    Path, FileName: ShortString;
    i   : Integer;
    Buf : PChar;
    Len : longint;
    FilePointer: TFilePointer;

  begin
  if FormIsClosed then exit;
  try
    if QGlobalOptions.FileDisplayType = fdBrief
      then
        i := DrawGridFiles.Selection.Left*DrawGridFiles.RowCount +
              DrawGridFiles.Selection.Top
      else
        i := pred(DrawGridFiles.Row);
    if i < FilesList.Count then
      with TOneFileLine(FilesList.Objects[i]) do
        begin
        if POneFile^.LongName = '..' then
          begin
          Application.MessageBox(lsDescriptionCannotBeByParentDir,
            lsNotification, mb_OK or mb_IconInformation);
          exit;
          end;
        FileName := POneFile^.LongName + POneFile^.Ext;
        QI_GetFullDirPath (DBaseHandle, Path);
        if ShortCopy(Path, length(Path), 1) <> '\' then Path := Path + '\';
        with FormDescription do
          begin
          MemoDesc.ReadOnly := DBaseIsReadOnly;
          MenuExitAndSave.Enabled := not DBaseIsReadOnly;
          ButtonOK.Enabled        := not DBaseIsReadOnly;
          LabelFile.Caption := Path + FileName;
          QI_LoadDescToBuf(DBaseHandle, POneFile^.Description, Buf);
          MemoDesc.SetTextBuf(Buf);
          StrDispose(Buf);
          if (ShowModal = mrOK) and MemoDesc.Modified and not DBaseIsReadOnly then
            begin
            Len := MemoDesc.GetTextLen;
            if Len = 0
              then
                begin
                FilePointer := POneFile^.SelfFilePos;
                if not QI_SaveBufToDesc(DBaseHandle, FilePointer, nil)
                  then Application.MessageBox(lsDescriptionCannotBeSaved,
                    lsError, mb_OK or mb_IconStop)
                  else
                    begin
                    POneFile^.Description := FilePointer;
                    QI_UpdateDescInCurList(DBaseHandle, POneFile);
                    end;
                end
              else
                begin
                Buf := StrAlloc(Len + 1);
                MemoDesc.GetTextBuf(Buf, Len + 1);
                FilePointer := POneFile^.SelfFilePos;
                if not QI_SaveBufToDesc(DBaseHandle, FilePointer, Buf)
                  then Application.MessageBox(lsDescriptionCannotBeSaved,
                    lsError, mb_OK or mb_IconStop)
                  else
                    begin
                    POneFile^.Description := FilePointer;
                    QI_UpdateDescInCurList(DBaseHandle, POneFile);
                    end;
                StrDispose(Buf);
                end;
            end;
          end;
        end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Returns the database handle

function TFormDBase.GetDBaseHandle: PDBaseHandle;

  begin
  Result := DBaseHandle;
  end;

//--------------------------------------------------------------------
// Handles event issued when the user clicks in the disk panel

procedure TFormDBase.DrawGridDisksMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  var
    i: Integer;

  begin
  if FormIsClosed then exit;
  try
    if Button = mbLeft then
      begin
      if LastDiskSelected >= DrawGridDisks.RowCount then
        LastDiskSelected := pred(DrawGridDisks.RowCount);
      if ssShift in Shift then
        begin
        for i := 0 to pred(DrawGridDisks.RowCount) do
          begin
          QI_SetSelectionFlag(DBaseHandle, false, i);
          end;
        if LastDiskSelected <= DrawGridDisks.Row
          then
            for i := LastDiskSelected to DrawGridDisks.Row do
              begin
              QI_ToggleSelectionFlag(DBaseHandle, i);
              end
          else
            for i := LastDiskSelected downto DrawGridDisks.Row do
              begin
              QI_ToggleSelectionFlag(DBaseHandle, i);
              end;
        DrawGridDisks.Repaint;
        end;
      if ssCtrl in Shift then
        begin
        QI_ToggleSelectionFlag(DBaseHandle, DrawGridDisks.Row);
        end;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Displays the dislaog box for selecting disks/files by a mask

procedure TFormDBase.SelectDisksOrFiles(Disks: boolean);

  var
    i, j   : Integer;
    MaskCol: TPQCollection;
    Key    : ShortString;
    Attr   : Word;
    Matching: boolean;

  begin
  if Disks
    then
      begin
      FormMaskSelect.Caption := lsSelectionOfDisks;
      FormMaskSelect.LabelMask.Caption := lsEnterMaskForSelectionOfDisks;
      FormMaskSelect.ComboBoxMaskFiles.Visible := false;
      FormMaskSelect.ComboBoxMaskDisks.Visible := true;
      FormMaskSelect.ActiveControl := FormMaskSelect.ComboBoxMaskDisks;
      end
    else
      begin
      FormMaskSelect.Caption := lsSelectionOfFiles;
      FormMaskSelect.LabelMask.Caption := lsEnterMaskForSelectionOfFiles;
      FormMaskSelect.ComboBoxMaskDisks.Visible := false;
      FormMaskSelect.ComboBoxMaskFiles.Visible := true;
      FormMaskSelect.ActiveControl := FormMaskSelect.ComboBoxMaskFiles;
      end;
  if FormMaskSelect.ShowModal <> mrOK then exit;
  MaskCol := New(TPQCollection, Init(30, 50));
  try
    if Disks
      then
        begin
        if FormMaskSelect.ComboBoxMaskDisks.Text = '' then
          FormMaskSelect.ComboBoxMaskDisks.Text := '*';
        TokenFindMask(FormMaskSelect.ComboBoxMaskDisks.Text, MaskCol, false, false, false);
        for i := 0 to pred(DrawGridDisks.RowCount) do
          begin
          if (QI_GetKeyAt(DBaseHandle, Key, Attr, i)) and
           (Attr and kaSelected = 0) then
             begin
             Matching := false;
             for j := 0 to pred(MaskCol^.Count) do
               if MaskCompare (GetPQString(POneMask(MaskCol^.At(j))^.MaskName),
                Key, false, false) then
                 begin
                 Matching := true;
                 break;
                 end;
             if Matching then
               begin
               QI_SetSelectionFlag(DBaseHandle, true, i);
               end;
             end;
          end;
        DrawGridDisks.Repaint;
        end
      else
        begin
        if FormMaskSelect.ComboBoxMaskFiles.Text = '' then
          FormMaskSelect.ComboBoxMaskFiles.Text := '*';
        TokenFindMask(FormMaskSelect.ComboBoxMaskFiles.Text, MaskCol, false, false, false);
        for i := 0 to pred(FilesList.Count) do
          with TOneFileLine(FilesList.Objects[i]) do
            begin
            Matching := false;
            for j := 0 to pred(MaskCol^.Count) do
              if MaskCompare (GetPQString(POneMask(MaskCol^.At(j))^.MaskName),
               POneFile^.LongName + POneFile^.Ext, false, false) then
                begin
                Matching := true;
                break;
                end;
            if Matching then ExtAttr := ExtAttr or eaSelected;
            end;
        DrawGridFiles.Repaint;
        end;
    if MaskCol <> nil then Dispose(MaskCol, Done);
    MaskCol := nil;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Unselects all disks or files

procedure TFormDBase.UnselectDisksOrFiles(Disks: boolean);

  var
    i: Integer;

  begin
  try
  if Disks
    then
      begin
      for i := 0 to pred(DrawGridDisks.RowCount) do
        QI_SetSelectionFlag(DBaseHandle, false, i);
      DrawGridDisks.Repaint;
      end
    else
      begin
      for i := 0 to pred(FilesList.Count) do
        with TOneFileLine(FilesList.Objects[i]) do
          ExtAttr := ExtAttr and not eaSelected;
      DrawGridFiles.Repaint;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Selects all disks or files

procedure TFormDBase.SelectAllDisksOrFiles(Disks: boolean);

  var
    i: Integer;
    AlreadySelected: boolean;

  begin
  try
    if Disks
      then
        begin
        if UsePersistentBlocks
          then
            begin
            {first try if all files are already selected}
            AlreadySelected := true;
            for i := 0 to pred(DrawGridDisks.RowCount) do
              if not QI_GetSelectionFlag(DBaseHandle, i) then
                begin
                AlreadySelected := false;
                break;
                end;
            for i := 0 to pred(DrawGridDisks.RowCount) do
              QI_SetSelectionFlag(DBaseHandle, not AlreadySelected, i);
            end
          else
            begin
            for i := 0 to pred(DrawGridDisks.RowCount) do
              QI_SetSelectionFlag(DBaseHandle, true, i);
            end;
        DrawGridDisks.Repaint;
        end
      else
        begin
        if UsePersistentBlocks
          then
            begin
            AlreadySelected := true;
            for i := 0 to pred(FilesList.Count) do
              with TOneFileLine(FilesList.Objects[i]) do
                if (ExtAttr and eaSelected) = 0 then
                  begin
                  AlreadySelected := false;
                  break;
                  end;
            if AlreadySelected
              then
                for i := 0 to pred(FilesList.Count) do
                  with TOneFileLine(FilesList.Objects[i]) do
                    ExtAttr := ExtAttr and not eaSelected
              else
                for i := 0 to pred(FilesList.Count) do
                  with TOneFileLine(FilesList.Objects[i]) do
                    ExtAttr := ExtAttr or eaSelected;
            end
          else
            begin
            for i := 0 to pred(FilesList.Count) do
              with TOneFileLine(FilesList.Objects[i]) do
                ExtAttr := ExtAttr or eaSelected;
            end;
        DrawGridFiles.Repaint;
        end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Jumps one level up in the tree to the parent of the current file

procedure TFormDBase.GoToParent;
  var
    i   : Integer;

  begin
  if FormIsClosed then exit;
  try
    for i := 0 to pred(FilesList.Count) do
      with TOneFileLine(FilesList.Objects[i]) do
        if FileType = ftParent then
          begin
          DirToScrollHandle := QI_GetCurDirCol(DBaseHandle);
          if (POneFile^.Attr and faDirectory <> 0)
           and (POneFile^.SubDirCol <> nil) then
            begin
            QI_SetCurDirCol (DBaseHandle, POneFile^.SubDirCol);
            QI_UpdateFileCol(DBaseHandle);
            UpdateHeader;
            JumpInOutline(POneFile^.SubDirCol);
            end;
          exit;
          end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Handles event issued when the user clicks with the right mouse button
// in the disk panel to get a pop-up menu.

procedure TFormDBase.PopupMenuDiskPopup(Sender: TObject);
  begin
  MenuDeleteDisk.Enabled := not DBaseIsReadOnly;
  MenuRenameDisk.Enabled := not DBaseIsReadOnly;
  MenuUndeleteDisk.Enabled := CanUndeleteRecord and not DBaseIsReadOnly;
  end;

//--------------------------------------------------------------------
// Menu handler - Rescans the current disk

procedure TFormDBase.MenuRescanClick(Sender: TObject);
  begin
  RescanDisk;
  end;

//--------------------------------------------------------------------
// Menu handler - deletes the current disk

procedure TFormDBase.MenuDeleteDiskClick(Sender: TObject);
  begin
  DeleteRecord;
  end;

//--------------------------------------------------------------------
// Menu handler - undeletes the disk

procedure TFormDBase.MenuUndeleteDiskClick(Sender: TObject);
begin
UndeleteRecord;
end;

//--------------------------------------------------------------------
// Menu handler - renames disk in the database

procedure TFormDBase.MenuRenameDiskClick(Sender: TObject);
begin
ChangeDiskName;
end;

//--------------------------------------------------------------------
// Menu handler - select disks by a mask

procedure TFormDBase.MenuSelectDisksClick(Sender: TObject);
begin
SelectDisksOrFiles(true);
end;

//--------------------------------------------------------------------
// Menu handler - unselect disks

procedure TFormDBase.MenuDeselectDiskClick(Sender: TObject);
begin
UnselectDisksOrFiles(true);
end;

//--------------------------------------------------------------------
// Menu handler - display disk info

procedure TFormDBase.MenuInfoDiskClick(Sender: TObject);
begin
ShowDiskInfo;
end;

//--------------------------------------------------------------------
// Menu handler - print disks

procedure TFormDBase.MenuPrintDisksClick(Sender: TObject);
begin
MakePrintDisk;
end;

//--------------------------------------------------------------------
// Menu handler - display help for disk panel

procedure TFormDBase.MenuHelpDisksClick(Sender: TObject);
begin
Application.HelpContext(150);
end;

//--------------------------------------------------------------------
// Menu handler - called when the popup menu (on right mouse button)
// is called for the tree panel

procedure TFormDBase.PopupMenuTreePopup(Sender: TObject);
begin
MenuShowTree.Checked := QGlobalOptions.ShowTree;
end;

//--------------------------------------------------------------------
// Menu handler - shows or hides the tree

procedure TFormDBase.MenuShowTreeClick(Sender: TObject);
begin
MenuShowTree.Checked := not MenuShowTree.Checked;
QGlobalOptions.ShowTree := not QGlobalOptions.ShowTree;
ShowOrHideTreePanel;
end;

//--------------------------------------------------------------------
// Menu handler - fully expands the tree

procedure TFormDBase.MenuExpandTreeClick(Sender: TObject);
begin
OutlineTree.BeginUpdate;
OutlineTree.FullExpand;
OutlineTree.EndUpdate;
end;

//--------------------------------------------------------------------
// Menu handler - collapses the tree

procedure TFormDBase.MenuCollapseTreeClick(Sender: TObject);
begin
OutlineTree.BeginUpdate;
OutlineTree.FullCollapse;
OutlineTree.Items[1].Expand(false);
OutlineTree.EndUpdate;
end;

//--------------------------------------------------------------------
// Menu handler - displays the help for the tree panel

procedure TFormDBase.MenuHelpTreeClick(Sender: TObject);
begin
Application.HelpContext(160);
end;

//--------------------------------------------------------------------
// Menu handler - called when the popup menu (on right mouse button)
// is called for the files panel

procedure TFormDBase.PopupMenuFilesPopup(Sender: TObject);
begin
MenuSortName.Checked := false;
MenuSortExt.Checked  := false;
MenuSortTime.Checked := false;
MenuSortSize.Checked := false;
case QGlobalOptions.SortCrit of
  scName: MenuSortName.Checked := true;
  scExt : MenuSortExt.Checked  := true;
  scTime: MenuSortTime.Checked := true;
  scSize: MenuSortSize.Checked := true;
  end;
case QGlobalOptions.FileDisplayType of
  fdBrief:
    begin
    MenuBrief.Checked := true;
    MenuDetailed.Checked := false;
    end;
  fdDetailed:
    begin
    MenuDetailed.Checked := true;
    MenuBrief.Checked := false;
    end;
  end;
end;

//--------------------------------------------------------------------
// Menu handler - copy files to clipboard

procedure TFormDBase.MenuCopyFileClick(Sender: TObject);
begin
MakeCopyFiles;
end;

//--------------------------------------------------------------------
// Menu handler - Delete File

procedure TFormDBase.MenuDeleteFileClick(Sender: TObject);
begin
DeleteFiles;
end;


//--------------------------------------------------------------------
// Menu handler - edit the description

procedure TFormDBase.MenuEditDescClick(Sender: TObject);
begin
EditDescription;
end;

//--------------------------------------------------------------------
// Menu handler - display files in brief list

procedure TFormDBase.MenuBriefClick(Sender: TObject);
begin
SetBriefFileDisplay(true);
PopupMenuFilesPopup(Sender);
end;

//--------------------------------------------------------------------
// Menu handler - display files in detailed list

procedure TFormDBase.MenuDetailedClick(Sender: TObject);
begin
SetBriefFileDisplay(false);
PopupMenuFilesPopup(Sender);
end;

//--------------------------------------------------------------------
// Menu handler - sort files by names

procedure TFormDBase.MenuSortNameClick(Sender: TObject);
begin
SetNeedResort(scName);
PopupMenuFilesPopup(Sender);
end;

//--------------------------------------------------------------------
// Menu handler - sort files by extension

procedure TFormDBase.MenuSortExtClick(Sender: TObject);
begin
SetNeedResort(scExt);
PopupMenuFilesPopup(Sender);
end;

//--------------------------------------------------------------------
// Menu handler - sort files by date/time

procedure TFormDBase.MenuSortTimeClick(Sender: TObject);
begin
SetNeedResort(scTime);
PopupMenuFilesPopup(Sender);
end;

//--------------------------------------------------------------------
// Menu handler - sort files by size

procedure TFormDBase.MenuSortSizeClick(Sender: TObject);
begin
SetNeedResort(scSize);
PopupMenuFilesPopup(Sender);
end;

//--------------------------------------------------------------------
// Menu handler - print files

procedure TFormDBase.MenuPrintFilesClick(Sender: TObject);
begin
MakePrintFiles;
end;

//--------------------------------------------------------------------
// Menu handler - display help for file panel

procedure TFormDBase.MenuHelpFilesClick(Sender: TObject);
begin
Application.HelpContext(170);
end;

//--------------------------------------------------------------------
// called when the user selects Mask Select from the Main Form menu

procedure TFormDBase.DoSelection;
begin
if (ActiveControl is TDrawGrid) then
  begin
  if TDrawGrid(ActiveControl).Tag = 1 then SelectDisksOrFiles(true);
  if TDrawGrid(ActiveControl).Tag = 3 then SelectDisksOrFiles(false);
  end;
end;

//--------------------------------------------------------------------
// called when the user selects Select All from the Main Form menu

procedure TFormDBase.SelectAll;
begin
if (ActiveControl is TDrawGrid) then
  begin
  if TDrawGrid(ActiveControl).Tag = 1 then SelectAllDisksOrFiles(true);
  if TDrawGrid(ActiveControl).Tag = 3 then SelectAllDisksOrFiles(false);
  end;
end;

//--------------------------------------------------------------------
// called when the user selects Unselect All from the Main Form menu

procedure TFormDBase.UnselectAll;
begin
if (ActiveControl is TDrawGrid) then
  begin
  if TDrawGrid(ActiveControl).Tag = 1 then UnselectDisksOrFiles(true);
  if TDrawGrid(ActiveControl).Tag = 3 then UnselectDisksOrFiles(false);
  end;
end;

//--------------------------------------------------------------------
// Menu handler - select disks/files

procedure TFormDBase.MenuSelectFilesClick(Sender: TObject);
begin
SelectDisksOrFiles(false);
end;

//--------------------------------------------------------------------
// Menu handler - unselect disks/files

procedure TFormDBase.MenuUnselectFilesClick(Sender: TObject);
begin
UnselectDisksOrFiles(false);
end;

//--------------------------------------------------------------------
// Returns the tag of the active panel

function TFormDBase.ActivePanel: Integer;
begin
Result := 0;
if (ActiveControl is TDrawGrid)
  then Result := TDrawGrid(ActiveControl).Tag;
if (ActiveControl is TTreeView)
  then Result := TTreeView(ActiveControl).Tag;
end;

//--------------------------------------------------------------------
// Menu handler - select all files

procedure TFormDBase.MenuSelectAllFilesClick(Sender: TObject);
begin
SelectAllDisksOrFiles(false);
end;

//--------------------------------------------------------------------
// Menu handler - select all disks

procedure TFormDBase.MenuSelectAllDisksClick(Sender: TObject);
begin
SelectAllDisksOrFiles(true);
end;

//--------------------------------------------------------------------
// Menu handler - copy disk to clipboard

procedure TFormDBase.MenuCopyDiskClick(Sender: TObject);
begin
MakeCopyDisks;
end;

//--------------------------------------------------------------------
// called from the Main Form's menu - copies disks or files to clipboard

procedure TFormDBase.MakeCopy;
begin
if (ActiveControl is TDrawGrid) then
  begin
  if TDrawGrid(ActiveControl).Tag = 1 then MakeCopyDisks;
  if TDrawGrid(ActiveControl).Tag = 3 then MakeCopyFiles;
  end;
end;

//--------------------------------------------------------------------
// Copies disks to clipboard

procedure TFormDBase.MakeCopyDisks;

  var
    Key        : ShortString;
    Attr       : Word;
    Current, i : Integer;
    CopyBuffer : PChar;
    hCopyBuffer: THandle;
    TotalLength: longint;

  begin
  try
    TotalLength := 0;
    QI_GetCurrentKey(DBaseHandle, Key, Attr, Current);
    for i := 0 to pred(QI_GetCountToShow(DBaseHandle)) do
      if QI_GetKeyAt(DBaseHandle, Key, Attr, i) then
        if (Attr and kaSelected <> 0) or (i = Current) then
          AddToBuffer(true, Key + #13#10, CopyBuffer, TotalLength);
    ///hCopyBuffer   := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, TotalLength+1);
    ///CopyBuffer    := GlobalLock(hCopyBuffer);
    for i := 0 to pred(QI_GetCountToShow(DBaseHandle)) do
      if QI_GetKeyAt(DBaseHandle, Key, Attr, i) then
        if (Attr and kaSelected <> 0) or (i = Current) then
          AddToBuffer(false, Key + #13#10, CopyBuffer, TotalLength);
    ///GlobalUnlock(hCopyBuffer);
    ///Clipboard.SetAsHandle(CF_TEXT, hCopyBuffer);
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Copies files to clipboard

procedure TFormDBase.MakeCopyFiles;

  var
    CopyBuffer    : PChar;
    TotalLength   : longint;
    hCopyBuffer   : THandle;


    procedure Run(CalcOnly: boolean);
      var
        i: Integer;
        S: ShortString;
        OneFileLine: TOneFileLine;
    begin
    for i := 0 to pred(FilesList.Count) do
      begin
      OneFileLine := TOneFileLine(FilesList.Objects[i]);
      if (OneFileLine.ExtAttr and eaSelected <> 0) or (i = CurFileSelected) then
        begin
        S := OneFileLine.POneFile^.LongName + OneFileLine.POneFile^.Ext;
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);
        if OneFileLine.POneFile^.Time <> 0
          then
            S := DosDateToStr(OneFileLine.POneFile^.Time) + #9 +
                 DosTimeToStr(OneFileLine.POneFile^.Time, QGlobalOptions.ShowSeconds)
          else
            S := #9;
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);
        case OneFileLine.FileType of
          ftDir   : S := lsFolder;
          ftParent: S := '';
          else      S := FormatSize(OneFileLine.POneFile^.Size,
                               QGlobalOptions.ShowInKb);
          end;
        AddToBuffer(CalcOnly, S + #9, CopyBuffer, TotalLength);
        S := '';
        if OneFileLine.POneFile^.Description <> 0 then
          QI_GetShortDesc(DBaseHandle, OneFileLine.POneFile^.Description, S);
        AddToBuffer(CalcOnly, S + #13#10, CopyBuffer, TotalLength);
        end;
      end;
    end;

  begin
  try
    TotalLength := 0;
    Run(true); // calculate the size needed

    ///CopyBuffer   := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, TotalLength+1);
    ///CopyBuffer    := GlobalLock(hCopyBuffer);
    Run(false);
    ///GlobalUnlock(hCopyBuffer);
    ///Clipboard.SetAsHandle(CF_TEXT, hCopyBuffer);
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Deletes files
procedure TFormDBase.DeleteFiles;
begin
  if (CurFileSelected >= 1) and (FilesList.count > CurFileSelected) then
    begin
      with TOneFileLine(FilesList.Objects[CurFileSelected]).POneFile^ do
        QI_DeleteFile(DBaseHandle,LongName);

    QI_UpdateFileCol(DBaseHandle);

//    FilesList.Delete(CurFileSelected);
//    UpdateFileWindowGrid(CurFileSelected-1);

  end;
end;




//--------------------------------------------------------------------
// Prints files

procedure TFormDBase.MakePrintFiles;

  const NoOfColumns = 4;

  var
    AmountPrintedPx: Integer;
    PageCounter: Integer;
    ColWidthsPx: array [0..NoOfColumns-1] of Integer;
    TimeWidthPx: Integer;
         // Disk, Dir, Name, Size, Time, Desc

  //------

  function CanPrintOnThisPage: boolean;

    begin
    Result := true;
    if PrintDialog.PrintRange <> prPageNums then exit;
    with PrintDialog do
      if (PageCounter >= FromPage) and (PageCounter <= ToPage) then
        exit;
    Result := false;
    end;


  //------

  procedure GoToNewPage;

    begin
    if PrintDialog.PrintRange <> prPageNums
      then
        ///QPrinterNewPage
      else
        begin
        with PrintDialog do
          if (PageCounter >= FromPage) and (PageCounter < ToPage) then
            ///QPrinterNewPage;
        end;
    end;

  //------

  procedure PrintHeaderAndFooter;

  var
    OutRect : TRect;
    S, S1   : ShortString;
    Attr    : Word;
    TmpIndex: Integer;

  begin
  if FormAbortPrint.Aborted then exit;
  {$ifdef mswindows}
  QPrinterSaveAndSetFont(pfsItalic);

  // header
  OutRect.Left   := LeftPrintAreaPx;
  OutRect.Right  := RightPrintAreaPx;
  OutRect.Top    := TopPrintAreaPx;
  OutRect.Bottom := OutRect.Top + LineHeightPx;
  if CanPrintOnThisPage then
    begin
    S := QGlobalOptions.PrintHeader;
    if S = '' then
      begin
      QI_GetCurrentKey(DBaseHandle, S, Attr, TmpIndex);
      S := lsDisk1 + S;
      end;
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
    QI_GetFullDirPath(DBaseHandle, S1);
    S1 := lsFolder2 + S1;
    OutRect.Left := OutRect.Right - QPrinterGetTextWidth(S1);
    if OutRect.Left < (LeftPrintAreaPx + QPrinterGetTextWidth(S))
      then OutRect.Left := LeftPrintAreaPx + QPrinterGetTextWidth(S) + 3*YPxPer1mm;
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S1);
    QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 50)); {0.2 mm};
    QPrinterMoveTo(LeftPrintAreaPx,  OutRect.Bottom + YPxPer1mm);
    QPrinterLineTo(RightPrintAreaPx, OutRect.Bottom + YPxPer1mm);
    end;

  // footer
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

  //------

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
           S := lsName;
           QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
           end;
        1: begin
           S := lsSize;
           StartX := OutRect.Right - QPrinterGetTextWidth(S);
           QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
           end;
        2: begin
           S := lsDateAndTime;
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

  //------

  procedure PrintOneLine(Index: Integer);

  var
    OneLine : TOneFileLine;
    OutRect : TRect;
    TmpRect : TRect;
    S       : ShortString;
    i       : Integer;
    StartX  : Integer;

  begin
  if FormAbortPrint.Aborted then exit;
  {$ifdef mswindows}
  OneLine := TOneFileLine(FilesList.Objects[Index]);
  if (PrintDialog.PrintRange = prSelection) and
     (OneLine.ExtAttr and eaSelected = 0) and
     (Index <> pred(DrawGridFiles.Row))
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
         S := OneLine.POneFile^.LongName + OneLine.POneFile^.Ext;
         QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
         end;
      1: begin
         case OneLine.FileType of
           ftDir   : S := lsFolder;
           ftParent: S := '';
           else S := FormatSize(OneLine.POneFile^.Size, QGlobalOptions.ShowInKb);
           end;
         StartX := OutRect.Right - QPrinterGetTextWidth(S);
         QPrinterTextRect(OutRect, StartX, OutRect.Top, S);
         end;
      2: begin
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
      3: begin
         if OneLine.POneFile^.Description <> 0 then
           begin
           if QI_GetShortDesc(DBaseHandle, OneLine.POneFile^.Description, S) then
             QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
           end
         end;
      end;
    inc(OutRect.Left,ColWidthsPx[i]);
    end;
  inc(AmountPrintedPx, LineHeightPx);
  {$endif}
  end;

  //------

  var
    i: Integer;
    TmpS: array[0..256] of char;
    Copies: Integer;
    PxWidth : integer;
    TmpInt  : integer;
    OneFileLine: TOneFileLine;

begin
if PrintDialog.Execute then
  begin
  {$ifdef mswindows}
  QPrinterReset(QGlobalOptions);
  AmountPrintedPx    := TopPrintAreaPx + 2*LineHeightPx;

  TimeWidthPx := QPrinterGetTextWidth(DosTimeToStr(longint(23) shl 11,
                                        QGlobalOptions.ShowSeconds));
  TimeWidthPx := TimeWidthPx + TimeWidthPx div 6;

  // calculate Pixel width
  PxWidth := 100;
  for i := 0 to pred (FilesList.Count) do
    begin
    OneFileLine := TOneFileLine(FilesList.Objects[i]);
    TmpInt := QPrinterGetTextWidth (OneFileLine.POneFile^.LongName + OneFileLine.POneFile^.Ext);
    if TmpInt > PxWidth then PxWidth := TmpInt;
    end;

  ColWidthsPx[0] := PxWidth + 3*XPxPer1mm;

  ColWidthsPx[1] := QPrinterGetTextWidth(FormatSize(2000000000,
                    QGlobalOptions.ShowInKb)) + 3*XPxPer1mm;
  ColWidthsPx[2] := QPrinterGetTextWidth(' ' + DosDateToStr (694026240)) +
                          TimeWidthPx  + 3*XPxPer1mm;
  ColWidthsPx[3] := 25 * QPrinterGetTextWidth('Mmmmxxxxx ');

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
      for i := 0 to pred(FilesList.Count) do
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
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    on EOther: Exception do
       Application.MessageBox(StrPCopy(TmpS, EOther.Message), lsError,
         mb_Ok or mb_IconExclamation);
    end;
  MainForm.Enabled :=true;
  FormAbortPrint.Hide;
  {$endif}
  end;
end;

//--------------------------------------------------------------------
// Prints the tree

procedure TFormDBase.MakePrintTree;

  var
    AmountPrintedPx: Integer;
    PageCounter  : Integer;
    TreeList     : TQStringList;
    MWidthPx     : Integer;
    ColumnWidthPx: Integer;
    Column       : Integer;
    NoOfColumns  : Integer;

  //------

  function CanPrintOnThisPage: boolean;

    begin
    Result := true;
    if PrintDialog.PrintRange <> prPageNums then exit;
    with PrintDialog do
      if (PageCounter >= FromPage) and (PageCounter <= ToPage) then
        exit;
    Result := false;
    end;


  //------

  procedure GoToNewPage;

    begin
    if PrintDialog.PrintRange <> prPageNums
      then
        ///QPrinterNewPage
      else
        begin
        with PrintDialog do
          if (PageCounter >= FromPage) and (PageCounter < ToPage) then
            ///QPrinterNewPage;
        end;
    end;

  //------

  procedure PrintHeaderAndFooter;

  var
    OutRect : TRect;
    S, S1   : ShortString;
    Attr    : Word;
    TmpIndex: Integer;

  begin
  if FormAbortPrint.Aborted then exit;
  {$ifdef mswindows}
  if CanPrintOnThisPage then
    begin
    QPrinterSaveAndSetFont(pfsItalic);
    // header
    OutRect.Left   := LeftPrintAreaPx;
    OutRect.Right  := RightPrintAreaPx;
    OutRect.Top    := TopPrintAreaPx;
    OutRect.Bottom := OutRect.Top + LineHeightPx;
    S := QGlobalOptions.PrintHeader;
    if S = '' then S := lsDatabase + ShortDBaseFileName;
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
    QI_GetCurrentKey(DBaseHandle, S1, Attr, TmpIndex);
    S1 := lsDisk1 + S1;
    OutRect.Left := OutRect.Right - QPrinterGetTextWidth(S1);
    if OutRect.Left < (LeftPrintAreaPx + QPrinterGetTextWidth(S))
      then OutRect.Left := LeftPrintAreaPx + QPrinterGetTextWidth(S) + 3*YPxPer1mm;
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S1);
    QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 50)); {0.2 mm};
    QPrinterMoveTo(LeftPrintAreaPx,  OutRect.Bottom + YPxPer1mm);
    QPrinterLineTo(RightPrintAreaPx, OutRect.Bottom + YPxPer1mm);

    // footer
    OutRect.Left   := LeftPrintAreaPx;
    OutRect.Bottom := BottomPrintAreaPx;
    OutRect.Top    := OutRect.Bottom - LineHeightPx;
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, TimePrinted);
    S := lsPage + IntToStr(PageCounter);
    OutRect.Left := OutRect.Right - QPrinterGetTextWidth(S);
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top, S);
    QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 50)); {0.2 mm};
    QPrinterMoveTo(LeftPrintAreaPx,  OutRect.Top - YPxPer1mm);
    QPrinterLineTo(RightPrintAreaPx, OutRect.Top - YPxPer1mm);

    QPrinterRestoreFont;
    end;
  {$endif}
  end;

  //------

  procedure PrintOneLine(Index: Integer);


  var
    OneTreeLine : TOneTreeLine;
    OutRect : TRect;
    j       : Integer;

  begin
  if FormAbortPrint.Aborted then exit;
  {$ifdef mswindows}
  OneTreeLine := TOneTreeLine(TreeList.Objects[Index]);
  OutRect.Top    := AmountPrintedPx;
  OutRect.Right  := LeftPrintAreaPx + succ(Column)*ColumnWidthPx;
  OutRect.Bottom := OutRect.Top + LineHeightPx;
  if CanPrintOnThisPage then
    begin
    QPrinterSetLineWidth(MaxI(1, YPxPer1cm div 100)); {0.1 mm};
    OutRect.Left := LeftPrintAreaPx + Column * ColumnWidthPx + MWidthPx div 2;
    for j := 1 to length(OneTreeLine.Line) do
      begin
      case OneTreeLine.Line[j] of
        '+': begin
             QPrinterMoveTo(OutRect.Left, OutRect.Top);
             QPrinterLineTo(OutRect.Left, OutRect.Bottom);
             QPrinterMoveTo(OutRect.Left, OutRect.Top + LineHeightPx div 2);
             QPrinterLineTo(OutRect.Left + MWidthPx, OutRect.Top + LineHeightPx div 2);
             end;
        'L': begin
             QPrinterMoveTo(OutRect.Left, OutRect.Top);
             QPrinterLineTo(OutRect.Left, OutRect.Top + LineHeightPx div 2);
             QPrinterLineTo(OutRect.Left + MWidthPx, OutRect.Top + LineHeightPx div 2);
             end;
        'I': begin
             QPrinterMoveTo(OutRect.Left, OutRect.Top);
             QPrinterLineTo(OutRect.Left, OutRect.Bottom);
             end;
        end;
      inc(OutRect.Left, 2*MWidthPx);
      end;
    dec(OutRect.Left, MWidthPx div 2);
    QPrinterTextRect(OutRect, OutRect.Left, OutRect.Top,
             TreeList.Strings[Index]);
    end;
  inc(AmountPrintedPx, LineHeightPx);
  {$endif}
  end;

  //------

  var
    i, j: Integer;
    TmpS: array[0..256] of char;
    Copies: Integer;
    LastLine: string[MaxTreeLevels];
    OneTreeLine, NextTreeLine: TOneTreeLine;
    StartLevel, StartItem: LongInt;
    MaxTextLengthPx: LongInt;
    MaxLevel       : LongInt;
    TmpInt         : LongInt;

begin
if PrintDialog.Execute then
  begin
  {$ifdef mswindows}
  QPrinterReset(QGlobalOptions);
  AmountPrintedPx    := TopPrintAreaPx + 2*LineHeightPx;
  MWidthPx           := QPrinterGetTextWidth('M');

  TreeList := TQStringList.Create;
  MaxTextLengthPx := 0;
  MaxLevel        := 0;
  with OutlineTree do
    begin
    if PrintDialog.PrintRange = prSelection
      then StartItem := SelectedItem
      else StartItem := 1;
    if StartItem = 0 then StartItem := 1;
    StartLevel := Items[StartItem].Level;
    for i := StartItem to ItemCount do
      begin
      if (i > StartItem) and (PrintDialog.PrintRange = prSelection)
        and (longint(Items[i].Level) <= StartLevel) then break;
      OneTreeLine := TOneTreeLine.Create;
      OneTreeLine.Level := Items[i].Level - 1;
      fillchar(OneTreeLine.Line, sizeof(OneTreeLine.Line), ' ');
      OneTreeLine.Line[0] := char(OneTreeLine.Level);
      TreeList.AddObject(Items[i].Text, OneTreeLine);
      TmpInt := QPrinterGetTextWidth(Items[i].Text);
      if TmpInt > MaxTextLengthPx then
        MaxTextLengthPx := TmpInt;
      if OneTreeLine.Level > MaxLevel then MaxLevel := OneTreeLine.Level;
      end;
    end;
  FillChar(LastLine, sizeof(LastLine), ' ');
  for i := pred(TreeList.Count) downto 0 do
    begin
    OneTreeLine := TOneTreeLine(TreeList.Objects[i]);
    for j := 1 to OneTreeLine.Level-1 do
      if (LastLine[j]='I') or (LastLine[j]='L') then
        OneTreeLine.Line[j] := 'I';
    if OneTreeLine.Level > 0 then
      OneTreeLine.Line[OneTreeLine.Level] := 'L';
    LastLine := OneTreeLine.Line;
    for j := length(LastLine) + 1 to MaxTreeLevels do
      LastLine[j] := ' ';
    end;
  for i := 0 to TreeList.Count-2 do
    begin
    OneTreeLine := TOneTreeLine(TreeList.Objects[i]);
    NextTreeLine := TOneTreeLine(TreeList.Objects[i+1]);
    j := OneTreeLine.Level;
    if j > 0 then
      if (OneTreeLine.Line[j]='L') and ((NextTreeLine.Line[j]='I') or
         (NextTreeLine.Line[j]='L')) then
           OneTreeLine.Line[j] := '+';
    end;

  MaxTextLengthPx := MaxTextLengthPx + MaxLevel*MWidthPx*2 + 3*XPxPer1mm;
  NoOfColumns := (RightPrintAreaPx - LeftPrintAreaPx) div MaxTextLengthPx;
  if NoOfColumns < 1 then NoOfColumns := 1;
  ColumnWidthPx := (RightPrintAreaPx - LeftPrintAreaPx) div NoOfColumns;
  Column := 0;

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
      FormAbortPrint.LabelProgress.Caption :=
        lsPrintingPage + IntToStr(PageCounter);
      for i := 0 to pred(TreeList.Count) do
        begin
        Application.ProcessMessages;
        if FormAbortPrint.Aborted then break;
        PrintOneLine(i);
        if (AmountPrintedPx + 3*LineHeightPx) > BottomPrintAreaPx then
          begin
          if Column < pred(NoOfColumns)
            then
              begin
              AmountPrintedPx  := TopPrintAreaPx + 2*LineHeightPx;
              inc(Column);
              end
            else
              begin
              if not FormAbortPrint.Aborted then
                begin
                GoToNewPage;
                Column := 0;
                inc(PageCounter);
                end;
              FormAbortPrint.LabelProgress.Caption :=
                lsPrintingPage + IntToStr(PageCounter);
              Application.ProcessMessages;
              AmountPrintedPx  := TopPrintAreaPx + 2*LineHeightPx;
              PrintHeaderAndFooter;
              end;
          end;
        end;
      QPrinterEndDoc;
      if FormAbortPrint.Aborted then break;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    on E: Exception do Application.MessageBox(StrPCopy(TmpS, E.Message), lsError,
        mb_Ok or mb_IconExclamation);
      end;

  TreeList.Free;
  MainForm.Enabled :=true;
  FormAbortPrint.Hide;
  {$endif}
  end;
end;

//--------------------------------------------------------------------
// Menu handler - print the tree

procedure TFormDBase.MenuPrintTreeClick(Sender: TObject);
begin
MakePrintTree;
end;

//--------------------------------------------------------------------
// Prints the disk

procedure TFormDBase.MakePrintDisk;

var
  SaveKey     : ShortString;
  SaveSelected: Integer;
  SaveAttr    : Word;
  SavePath    : ShortString;
  SaveFile    : ShortString;

  begin
  if FormIsClosed then exit;
  FormDiskPrint.DBaseHandle := DBaseHandle;
  try
    QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
    QI_GetFullDirPath (DBaseHandle, SavePath);
    SaveFile := LastSelectedFile;
    FormDiskPrint.ShortDBaseFileName := ShortDBaseFileName;
    FormDiskPrint.ShowModal;
    JumpTo(SaveKey, SavePath, SaveFile);
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Called from the Main From to print the selected panel

procedure TFormDBase.DoPrint(PrintWhat: TPrintWhat);
begin
case PrintWhat of
  prSelectedPanel:
    begin
    if (ActiveControl is TDrawGrid) then
      begin
      if TDrawGrid(ActiveControl).Tag = 1 then MakePrintDisk;
      if TDrawGrid(ActiveControl).Tag = 3 then MakePrintFiles;
      end;
    if (ActiveControl is TTreeView) then
      if TTreeView(ActiveControl).Tag = 2 then MakePrintTree;
    end;
  prDisks: MakePrintDisk;
  prTree:  MakePrintFiles;
  prFiles: MakePrintFiles;
  end;
end;


//--------------------------------------------------------------------
// Handles the event issued when the mouse moves over the file panel
// USed for displaying the bubbles with descriptions

procedure TFormDBase.DrawGridFilesMouseMove(Sender: TObject;
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

//--------------------------------------------------------------------
// Shows the bubble file hint

procedure TFormDBase.ShowFileHint;

  var
    Point      : TPoint;
    Rect       : TRect;
    HintWidth  : integer;
    LineHeight : integer;
    OneFileLine: TOneFileLine; // pointer
    S          : ShortString;
    DosDateTime: longint;
    i, TmpInt  : integer;
    PolygonPts : array[0..2] of TPoint;
    XPosition  : integer;

  begin
  if MousePosAlreadyChecked then exit;
  if DisableHints <> 0 then exit;
  if FilesHintDisplayed then exit;
  MousePosAlreadyChecked := true;
  GetCursorPos(Point);
  Point := DrawGridFiles.ScreenToClient(Point);
  // is the mouse cursor still in the window?
  if (LastMouseXFiles <> Point.X) or (LastMouseYFiles <> Point.Y) then exit;
  OneFileLine := FindOneFileLine(Point, Rect);
  if OneFileLine = nil then exit;
  if OneFileLine.FileType = ftDir then exit;
  if OneFileLine.FileType = ftParent then exit;

  HiddenForm.MemoForHints.Lines.Clear;

  if OneFileLine.POneFile^.Description <> 0 then
    begin
    if QI_GetShortDesc(DBaseHandle, OneFileLine.POneFile^.Description, S) then
      HiddenForm.MemoForHints.Lines.Insert(0, S);
    end;

  if QGlobalOptions.FileDisplayType = fdBrief then
    begin
    DosDateTime := OneFileLine.POneFile^.Time;
    if DosDateTime <> 0 then
      HiddenForm.MemoForHints.Lines.Insert(0, DosDateToStr(DosDateTime) + ' ' +
                      DosTimeToStr(DosDateTime, QGlobalOptions.ShowSeconds));

    HiddenForm.MemoForHints.Lines.Insert(0,
              FormatSize(OneFileLine.POneFile^.Size, QGlobalOptions.ShowInKb));

    HiddenForm.MemoForHints.Lines.Insert(0, OneFileLine.POneFile^.LongName+OneFileLine.POneFile^.Ext);
    end;

  if HiddenForm.MemoForHints.Lines.Count = 0 then exit;

  with DrawGridFiles.Canvas do
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

    // find the position
    XPosition := (Rect.Left + Rect.Right) div 2;
    if XPosition < (DrawGridFiles.Width div 2)
      then
        begin
        LastHintRect.Left   := XPosition - 11;
        LastHintRect.Right  := XPosition + HintWidth -5;
        if (LastHintRect.Right >= DrawGridFiles.Width) then
          OffsetRect(LastHintRect, DrawGridFiles.Width-LastHintRect.Right-3, 0);
        PolygonPts[0].X := LastHintRect.Left + 5;
        PolygonPts[1].X := LastHintRect.Left + 11;
        PolygonPts[2].X := LastHintRect.Left + 17;
        end
      else
        begin
        LastHintRect.Left   := XPosition - HintWidth + 5;
        LastHintRect.Right  := XPosition + 11;
        if (LastHintRect.Left <= 0) then
          OffsetRect(LastHintRect, -LastHintRect.Left+3, 0);
        PolygonPts[0].X := LastHintRect.Right - 17;
        PolygonPts[1].X := LastHintRect.Right - 11;
        PolygonPts[2].X := LastHintRect.Right - 5;
        end;

    if Rect.Top < (DrawGridFiles.Height div 2)
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

//--------------------------------------------------------------------
// Locates the line in the file list according to the coordinates

function TFormDBase.FindOneFileLine(Point: TPoint; var Rect: TRect): TOneFileLine;

  var
    i: integer;
    ACol, ARow: longint;

  begin
  DrawGridFiles.MouseToCell(Point.X, Point.Y, ACol, ARow);
  Rect := DrawGridFiles.CellRect(ACol, ARow);
  if QGlobalOptions.FileDisplayType = fdBrief
    then
      begin
      i := ACol*DrawGridFiles.RowCount + ARow;
      if (i >= 0) and (i < FilesList.Count) then
        begin
        Result := TOneFileLine(FilesList.Objects[i]);
        exit;
        end;
      end
    else
      begin
      Rect.Left := 0;
      Rect.Right := DrawGridFiles.Width;
      if (ARow > 0) and (ARow <= FilesList.Count) then
        begin
        Result := TOneFileLine(FilesList.Objects[ARow-1]);
        exit;
        end;
      end;
  Result := nil;
  end;

//--------------------------------------------------------------------
// Erases the bubble file hint

procedure TFormDBase.EraseFileHint;

  begin
  if FilesHintDisplayed then
    begin
    FilesHintDisplayed := false;
    InflateRect(LastHintRect, 0, 6);
    InvalidateRect(DrawGridFiles.Handle, @LastHintRect, false);
    end;
  end;

//--------------------------------------------------------------------
// Exports to the text format

procedure TFormDBase.ExportToOtherFormat;

  var
    SaveKey     : ShortString;
    SaveSelected: Integer;
    SaveAttr    : Word;
    SavePath    : ShortString;
    SaveFile    : ShortString;

  begin
  if FormIsClosed then exit;
  try
    QI_GetCurrentKey  (DBaseHandle, SaveKey, SaveAttr, SaveSelected);
    QI_GetFullDirPath (DBaseHandle, SavePath);
    SaveFile := LastSelectedFile;
    FormDiskExport.DBaseHandle := DBaseHandle;
    FormDiskExport.ShowModal;
    JumpTo(SaveKey, SavePath, SaveFile);
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Menu handler - open file

procedure TFormDBase.MenuOpenClick(Sender: TObject);

  begin
  ShiftState := [ssShift];
  DrawGridFilesDblClick(Sender);
  end;

//--------------------------------------------------------------------
// Handles keystrokes, enables user DLL commands

procedure TFormDBase.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var
  i: integer;
  pUserCommand: TPUserCommand;

begin
for i:=1 to g_UserCommandList.Count do
  begin
  pUserCommand := TPUserCommand(g_UserCommandList.Items[i-1]);
  if (Key <> pUserCommand.m_wKey) then
    begin
    continue;
    end;
  if (pUserCommand.m_bShift   and not (ssShift in ShiftState)) then continue;
  if (pUserCommand.m_bControl and not (ssCtrl  in ShiftState)) then continue;
  if (pUserCommand.m_bAlt     and not (ssAlt   in ShiftState)) then continue;
  ExecuteUserCommand(pUserCommand.m_sDll, pUserCommand.m_sParams, pUserCommand.m_bTestExist);
  Key := 0;
  end;
end;

//-----------------------------------------------------------------------------
// Executes a use command in the user DLL

procedure TFormDBase.ExecuteUserCommand(sDll: AnsiString; sParams: AnsiString; bTestExist: boolean);

  var
    i: integer;
    sMsg           : AnsiString;

    sBasePath      : ShortString; // root path
    sRelativePath  : ShortString; // path from root to the file
    sFullPath      : AnsiString;  // full path to the physical file, including file name
    sPathInArchive : AnsiString;  // if the file is archive, the path in archive

    OrigDiskName   : ShortString;
    OrigVolumeLabel: ShortString;
    Index          : Integer;
    Attr           : Word;
    FileSystem     : TFileSystem;
    VolumeLabel    : ShortString;
    DriveType      : integer;
    Exists         : boolean;
    InArchive      : boolean;
    DriveAccessible: boolean;
    DriveRemovable : boolean;
    DriveNetwork   : boolean;
    VolumeLabelDiffers: boolean;
    Retry          : boolean;
    dwDiskBaseAttr : DWORD;
    pszDescription : PChar;

  begin
  LOG('ExecuteUserCommand(sDll=%s, sParams=%s)', [PChar(sDll), PChar(sParams)]);
  if QGlobalOptions.FileDisplayType = fdBrief
    then
      i := DrawGridFiles.Selection.Left*DrawGridFiles.RowCount +
           DrawGridFiles.Selection.Top
    else
      i := pred(DrawGridFiles.Row);
  if i < FilesList.Count then
    with TOneFileLine(FilesList.Objects[i]) do
      begin
      if ((POneFile^.Attr and faDirectory = 0) or (POneFile^.Attr and faQArchive <> 0)) then
        begin
        Retry := true;
        while Retry do
          begin
          Retry := false;
          sPathInArchive := '';
          QI_GetCurrentKeyEx  (DBaseHandle, OrigDiskName, OrigVolumeLabel,
                               sBasePath, Attr, Index);
          if (length(sBasePath) > 0) and
             (sBasePath[length(sBasePath)] = '\') then
              SetLength(sBasePath, Length(sBasePath)-1);

          QI_GetFullDirPath(DBaseHandle, sRelativePath);
          InArchive := QI_IsInArchive (DBaseHandle);
          If InArchive
            then
              begin
              sPathInArchive := sRelativePath;
              sRelativePath := QI_GetArchivePath(DBaseHandle);
              Delete(sPathInArchive, 1, Length(sRelativePath));
              if (Length(sPathInArchive) > 0) and
                 (sPathInArchive[length(sPathInArchive)] <> '\') then
                sPathInArchive := sPathInArchive + '\';
              sPathInArchive := sPathInArchive + POneFile^.LongName + POneFile^.Ext;
              end
            else
              begin
              if (Length(sRelativePath) > 0) and
                 (sRelativePath[length(sRelativePath)] <> '\') then
                sRelativePath := sRelativePath + '\';
              sRelativePath := sRelativePath + POneFile^.LongName + POneFile^.Ext;
              end;
          sFullPath := sBasePath + sRelativePath;
          LOG('sFullPath=%s', [sFullPath]);
          LOG('sPathInArchive=%s', [sPathInArchive]);
          Exists := FileExists(sFullPath);
          VolumeLabel := '';
          DriveAccessible := ReadVolumeLabel (sBasePath[1] + ':', VolumeLabel,
                                              FileSystem, DriveType);
          ///DriveRemovable := (DriveType = DRIVE_REMOVABLE) or
          ///                  (DriveType = DRIVE_CDROM);
          ///DriveNetwork := DriveType = DRIVE_REMOTE;
          VolumeLabelDiffers := false;
          if (length(VolumeLabel) > 0) then
            VolumeLabelDiffers := AnsiUppercase(OrigVolumeLabel) <> AnsiUppercase(VolumeLabel);
          if (not Exists and bTestExist)
            then
              begin
              sMsg := sFullPath + lsFileCannotBeOpen;
              if not DriveAccessible then
                sMsg := sFullPath + lsDiskNotAccessible;
              if DriveRemovable then
                sMsg := sFullPath + lsDifferentDisk;
              if InArchive then
                sMsg := sFullPath + lsFileInArchive;
              if DriveNetwork then
                sMsg := sFullPath + lsDiskIsNetworkDrive;
              if VolumeLabelDiffers then
                sMsg := sFullPath + lsVolumeLabelDiffers;
              sMsg := sFullPath + lsFileProbablyDeleted;
              Retry := Application.MessageBox(PChar(sMsg), lsFileNotFound, MB_RETRYCANCEL) = IDRETRY;
              LOG('Does not exist: %s', [sFullPath]);
              end
            else
              begin
              dwDiskBaseAttr := 0;
              if (Exists            ) then dwDiskBaseAttr := dwDiskBaseAttr or daExists;
              if (DriveAccessible   ) then dwDiskBaseAttr := dwDiskBaseAttr or daDriveAccessible;
              if (DriveRemovable    ) then dwDiskBaseAttr := dwDiskBaseAttr or daDriveRemovable;
              if (DriveNetwork      ) then dwDiskBaseAttr := dwDiskBaseAttr or daDriveNetwork;
              if (VolumeLabelDiffers) then dwDiskBaseAttr := dwDiskBaseAttr or daVolumeLabelDiffers;
              if (InArchive         ) then dwDiskBaseAttr := dwDiskBaseAttr or daInArchive;
              QI_LoadDescToBuf(DBaseHandle, POneFile^.Description, pszDescription);

              if Pos('\', sDll) = 0 then sDll := ExtractFilePath(ParamStr(0)) + sDll;
              if (LoadUserDll(sDll)) then
                begin
                UserCommand(MainForm.Handle,
                            PChar(sParams),
                            PChar(sFullPath),
                            PChar(sPathInArchive), DWORD(POneFile^.Time),
                            DWORD(POneFile^.Size), DWORD(POneFile^.Attr and $FF),
                            pszDescription,
                            PChar(g_sTempFolder), dwDiskBaseAttr, nil);
                StrDispose(pszDescription);
                FreeUserDll();
                end;
              end;
          end; // while
        end; // if
      end; // with
  end;

//-----------------------------------------------------------------------------

procedure TFormDBase.DiscGearPrint(hWindow: HWND);

  var
    Key  : ShortString;
    Attr : Word;
    sText: AnsiString;
    i: integer;


  begin
  if FormIsClosed then exit;
  sText := '';
  try
    for i := 0 to pred(DrawGridDisks.RowCount) do
      begin
      if (QI_GetKeyAt(DBaseHandle, Key, Attr, i)) and (Attr and kaSelected <> 0) then
        sText := sText + Key + #13#10;
      end;
    DiscGearPrintRun(hWindow, sText);
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// initialization of the unit

begin
g_bShowHelpAfterClose := false;
end.



