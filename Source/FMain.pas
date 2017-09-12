unit FMain;
(*====================================================================
DiskBase main application window
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls,
  Buttons, ComCtrls, Grids, PrintersDlgs, IniFiles, UStringList,
  UCollectionsExt, UTypes, FSettings, UApiTypes, FLocalOptions, Types;


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

type

  { TMainForm }

  TMainForm = class(TForm)
    Disks: TDrawGrid;
    DrawGridFiles: TDrawGrid;
    HeaderTop: THeaderControl;
    ImageList: TImageList;
    MainMenu1: TMainMenu;
    MenuBrief1: TMenuItem;
    MenuCollapseTree: TMenuItem;
    MenuCopyDisk: TMenuItem;
    MenuCopyFile: TMenuItem;
    MenuDeleteDisk: TMenuItem;
    MenuDeleteFile: TMenuItem;
    MenuDeselectDisk: TMenuItem;
    MenuDetailed1: TMenuItem;
    MenuEditDesc: TMenuItem;
    MenuExpandTree: TMenuItem;
    MenuFile: TMenuItem;
    MenuHelpDisks: TMenuItem;
    MenuHelpFiles: TMenuItem;
    MenuHelpTree: TMenuItem;
    MenuInfoDisk: TMenuItem;
    MenuOpen: TMenuItem;
    MenuPrintDisks1: TMenuItem;
    MenuPrintFiles1: TMenuItem;
    MenuPrintTree1: TMenuItem;
    MenuRenameDisk: TMenuItem;
    MenuRescan: TMenuItem;
    MenuScanDisk: TMenuItem;
    MenuScanA: TMenuItem;
    MenuScanB: TMenuItem;
    MenuDirDescr: TMenuItem;
    MenuDelRecord: TMenuItem;
    MenuChgLabel: TMenuItem;
    MenuSelectAllDisks: TMenuItem;
    MenuSelectAllFiles: TMenuItem;
    MenuSelectDisks: TMenuItem;
    MenuSelectFiles: TMenuItem;
    MenuSepar3: TMenuItem;
    MenuCloseDBase: TMenuItem;
    MenuOpenDBase: TMenuItem;
    MenuReindex: TMenuItem;
    MenuExit: TMenuItem;
    MenuSearch: TMenuItem;
    MenuSettings: TMenuItem;
    MenuHelp: TMenuItem;
    MenuSearchName: TMenuItem;
    MenuSearchEmpty: TMenuItem;
    MenuSepar4: TMenuItem;
    MenuDiskInfo: TMenuItem;
    MenuConfig: TMenuItem;
    MenuHelpContent: TMenuItem;
    MenuAbout: TMenuItem;
    MainPanel: TPanel;
    MenuNew: TMenuItem;
    DiskTree: TTreeView;
    MenuShowTree1: TMenuItem;
    MenuSortExt1: TMenuItem;
    MenuSortName1: TMenuItem;
    MenuSortSize1: TMenuItem;
    MenuSortTime1: TMenuItem;
    MenuUndeleteDisk: TMenuItem;
    MenuUnselectFiles: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N3: TMenuItem;
    N6: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    PageControl1: TPageControl;
    PopupMenuDisk: TPopupMenu;
    PopupMenuFiles: TPopupMenu;
    PopupMenuTree: TPopupMenu;
    PrintDialog: TPrintDialog;
    SpeedButtonOpenDBase: TSpeedButton;
    SpeedButtonPrint: TSpeedButton;
    SpeedButtonScanA: TSpeedButton;
    SpeedButtonScanDisk: TSpeedButton;
    MenuPrint: TMenuItem;
    SpeedButtonDirDescr: TSpeedButton;
    SpeedButtonSearchFile: TSpeedButton;
    SpeedButtonSearchEmpty: TSpeedButton;
    SpeedButtonDiskInfo: TSpeedButton;
    NewDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    SpeedButtonCollapse: TSpeedButton;
    SpeedButtonExpand: TSpeedButton;
    SpeedButtonShowTree: TSpeedButton;
    SpeedButtonBrief: TSpeedButton;
    SpeedButtonDetailed: TSpeedButton;
    MenuSepar1: TMenuItem;
    MenuSortName: TMenuItem;
    MenuSortExt: TMenuItem;
    MenuSortTime: TMenuItem;
    MenuSortSize: TMenuItem;
    MenuSepar5: TMenuItem;
    MenuShowTree: TMenuItem;
    MenuSepar7: TMenuItem;
    MenuBrief: TMenuItem;
    MenuDetailed: TMenuItem;
    MenuUndelRecord: TMenuItem;
    MenuLocalOptions: TMenuItem;
    SpeedButtonShowFound: TSpeedButton;
    SpeedButtonShowDBase: TSpeedButton;
    ReindexDialog: TOpenDialog;
    N1: TMenuItem;
    MenuExport: TMenuItem;
    MenuImport: TMenuItem;
    MenuEdit: TMenuItem;
    MenuCopy: TMenuItem;
    N2: TMenuItem;
    SaveDialogExport: TSaveDialog;
    OpenDialogImport: TOpenDialog;
    N4: TMenuItem;
    MenuMaskSelectDisks: TMenuItem;
    MenuDelDiskSelection: TMenuItem;
    SpeedButtonSelect: TSpeedButton;
    SpeedButtonUnselect: TSpeedButton;
    SpeedButtonParent: TSpeedButton;
    SpeedButtonSearchSelected: TSpeedButton;
    MenuSearchSelected: TMenuItem;
    MenuDBaseInfo: TMenuItem;
    N5: TMenuItem;
    MenuSelectAll: TMenuItem;
    MenuMakeRuntime: TMenuItem;
    OpenDialogImport4: TOpenDialog;
    MenuImport4: TMenuItem;
    MenuRescanDisk: TMenuItem;
    MenuScanFolder: TMenuItem;
    MainTimer: TTimer;
    OpenRepairDialog: TOpenDialog;
    SaveRepairDialog: TSaveDialog;
    MenuRepair: TMenuItem;
    N7: TMenuItem;
    MenuLastFileSepar: TMenuItem;
    MenuLastFile1: TMenuItem;
    MenuLastFile2: TMenuItem;
    MenuLastFile3: TMenuItem;
    MenuLastFile4: TMenuItem;
    MenuLastFile5: TMenuItem;
    MenuLastFile6: TMenuItem;
    MenuLastFile7: TMenuItem;
    MenuLastFile8: TMenuItem;
    MenuLastFile9: TMenuItem;
    MenuLastFile10: TMenuItem;
    MenuExportToOtherFormat: TMenuItem;
    MenuPrintDisks: TMenuItem;
    MenuPrintTree: TMenuItem;
    MenuPrintFiles: TMenuItem;
    MenuPrintFoundFiles: TMenuItem;
    MenuPrintEmpty: TMenuItem;
    SpeedButtonEject1: TSpeedButton;
    SpeedButtonEject2: TSpeedButton;
    MenuTools: TMenuItem;
    MenuDiscGearPrint: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure DisksClick(Sender: TObject);
    procedure DisksDblClick(Sender: TObject);
    procedure DisksDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure DisksEnter(Sender: TObject);
    procedure DisksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DisksMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisksResize(Sender: TObject);
    procedure DisksSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure DiskTreeClick(Sender: TObject);
    procedure ChangePanel(Sender: TObject);
    procedure DrawGridFilesDblClick(Sender: TObject);
    procedure DrawGridFilesDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DrawGridFilesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawGridFilesSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FormResize(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuOpenDBaseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonCollapseClick(Sender: TObject);
    procedure SpeedButtonExpandClick(Sender: TObject);
    procedure MenuConfigClick(Sender: TObject);
    procedure SpeedButtonShowTreeClick(Sender: TObject);
    procedure SpeedButtonBriefClick(Sender: TObject);
    procedure SpeedButtonDetailedClick(Sender: TObject);
    procedure MenuBarClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure SpeedButtonScanDiskClick(Sender: TObject);
    procedure SpeedButtonScanAClick(Sender: TObject);
    procedure SpeedButtonScanBClick(Sender: TObject);
    procedure MenuDelRecordClick(Sender: TObject);
    procedure MenuSearchNameClick(Sender: TObject);
    procedure MenuSearchEmptyClick(Sender: TObject);
    procedure MenuDiskInfoClick(Sender: TObject);
    procedure MenuChgLabelClick(Sender: TObject);
    procedure MenuUndelRecordClick(Sender: TObject);
    procedure MenuLocalOptionsClick(Sender: TObject);
    procedure SpeedButtonDirDescrClick(Sender: TObject);
    procedure MenuCloseDBaseClick(Sender: TObject);
    procedure SpeedButtonShowFoundClick(Sender: TObject);
    procedure SpeedButtonShowDBaseClick(Sender: TObject);
    procedure MenuReindexClick(Sender: TObject);
    procedure MenuExportClick(Sender: TObject);
    procedure MenuImportClick(Sender: TObject);
    procedure MenuMaskSelectDisksClick(Sender: TObject);
    procedure MenuDelDiskSelectionClick(Sender: TObject);
    procedure SpeedButtonParentClick(Sender: TObject);
    procedure MenuSearchSelectedClick(Sender: TObject);
    procedure MenuDBaseInfoClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuMakeRuntimeClick(Sender: TObject);
    procedure MenuHelpContentClick(Sender: TObject);
    procedure MenuImport4Click(Sender: TObject);
    procedure MenuRescanDiskClick(Sender: TObject);
    procedure MenuScanFolderClick(Sender: TObject);
    procedure MainTimerTimer(Sender: TObject);
    procedure MenuRepairClick(Sender: TObject);
    procedure MenuLastFile1Click(Sender: TObject);
    procedure MenuLastFile2Click(Sender: TObject);
    procedure MenuLastFile3Click(Sender: TObject);
    procedure MenuLastFile4Click(Sender: TObject);
    procedure MenuLastFile5Click(Sender: TObject);
    procedure MenuLastFile6Click(Sender: TObject);
    procedure MenuLastFile7Click(Sender: TObject);
    procedure MenuLastFile8Click(Sender: TObject);
    procedure MenuLastFile9Click(Sender: TObject);
    procedure MenuLastFile10Click(Sender: TObject);
    procedure MenuExportToOtherFormatClick(Sender: TObject);
    procedure MenuPrintFoundFilesClick(Sender: TObject);
    procedure MenuPrintEmptyClick(Sender: TObject);
    procedure SpeedButtonPrintClick(Sender: TObject);
    procedure SpeedButtonEject1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButtonEject2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuDiscGearPrintClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);

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


  private
    EnableFirstIdle    : boolean;
    ParamDBaseName     : ShortString;
    ParamFindFile      : ShortString;
    ParamScanDisk      : ShortString;
    ParamAuto          : boolean;
    LastActiveMDIChild : TForm;
    DefaultMDIMaximized: boolean;
    LastUsedDir        : ShortString;
    LastOpenedFile     : ShortString;
    LastFiles          : array[1..10] of ShortString;
    m_EjectDriveLetter1: char;
    m_EjectDriveLetter2: char;

    //---
    QGlobalOptions     : TGlobalOptions; {class}
    HeaderWidths       : TPanelHeaderWidths;
    ColumnAttrib       : array[0..maxColumnAttribs] of TOneColumn;
    TimeWidth          : Integer;
    PanelsLocked       : boolean;
    LastMouseMoveTime  : longint;
    DisableHints       : integer;
    DisableNextDoubleClick: boolean;
    MousePosAlreadyChecked: boolean;
    FilesHintDisplayed : boolean;
    LastMouseXFiles    : integer;
    LastMouseYFiles    : integer;
    //DBaseHandle        : PDBaseHandle;
    LastHintRect       : TRect;
    ///FilesList          : TQStringList;
    FilesList          : TStringList;
    NeedReSort         : boolean; {if QOptions.SortCrit changes, resort in idle time}
    MakeDiskSelection  : boolean;     {set when shift is pressed}
    MakeFileSelection  : boolean;
    FormIsClosed       : boolean;
    MaxFileNameLength  : Integer;     {max. length of name in current list}
    MaxFileNamePxLength: Integer;
    SubTotals          : TSubTotals;
    //DBaseFileName      : ShortString;
    ShortDBaseFileName : ShortString;
    StatusLineFiles    : ShortString;
    StatusLineSubTotals: ShortString;
    LastSelectedFile   : ShortString; {name of the current file - from the status line}
    LastFileSelected   : Integer;
    CurFileSelected    : Integer;
    FormWasResized     : Integer;     {the ResizeEvent appears multiple times, so we must limit the response}
    StatusSection0Size : Integer;
    BitmapFolder       : TBitmap;
    BitmapParent       : TBitmap;
    BitmapArchive      : TBitmap;
    ShiftState         : TShiftState; // for mouse double click check



    DBaseIsReadOnly    : boolean;
    QLocalOptions      : TLocalOptions;  {record}
    LastDiskSelected   : Integer;     {for undo}
    TreePtrCol         : PQPtrCollection;
    DirToScrollHandle  : PDirColHandle;
    //---


    procedure OpenDatabase (DBFileName: ShortString);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure SetMenuFileDisplayType (FileDisplayType: TFileDisplayType);
    procedure SetMenuSortCrit (SortCrit: TSortCrit);
    procedure UpdateSpeedButtons;
    procedure FirstIdle;
    function  ProcessParams: boolean;
    procedure SetFormSize;
    procedure UpdateLastFilesList;
    procedure ReadLastFilesFromIni(var IniFile: TIniFile);
    procedure WriteLastFilesToIni(var IniFile: TIniFile);
    procedure ReadUserCommandsFromIni(var IniFile: TIniFile);
    procedure LoadSettingsIni;
    procedure SaveSettingsIni;
    procedure CreateTempFolder;
    procedure DeleteTempFolder;
    procedure SaveWinSizes;
    procedure UpdateDiskWindow;

    // --- redesigned ---
    procedure UpdateSize;
    procedure ResetDrawGridFiles;
    procedure ResetFontsAndRowHeights;
    procedure ShowFileHint;
    procedure SearchParam(ParamFindFile: ShortString);
    //procedure JumpTo(Disk, Dir, FileName: ShortString);
    procedure ReloadTree;
    procedure ScanTree(DirColHandle: PDirColHandle; Level: Integer;
                       var TotalItemCount: Integer); overload;
    procedure ScanTree(DirColHandle: PDirColHandle; Node: TTReeNode;
                       var TotalItemCount: Integer);  overload;
    procedure ExpandBranch(AnItem: TTreeNode);
    procedure JumpInOutline(SubDirCol: pointer);
    procedure UpdateHeader;
    procedure UpdateFileWindowGrid(SelPos: Integer);
    procedure RescanTree(SavePosition: boolean);
    procedure ReloadFileCol(SaveFileName: ShortString);
    procedure UpdateStatusLine(Index: Integer; SubTotalsToo: boolean);
    procedure MsgShouldExit;
    procedure ScanFolder(Directory, DiskName, VolumeLabel: ShortString;
                         NoOverWarning: boolean);
    procedure EraseFileHint;
    procedure ShowDiskInfo;
    procedure ShowDBaseInfo;
    procedure LocalIdle;
    procedure ChangeGlobalOptions;
    procedure ChangeLocalOptions;
    procedure SetBriefFileDisplay(Brief: boolean);
    procedure SetNeedResort(SortCrit: TSortCrit);
    procedure RescanDisk;
    procedure DeleteRecord;
    procedure UndeleteRecord;
    procedure SearchName;
    procedure SearchEmpty;
    procedure SearchSelected;
    procedure ChangeDiskName;
    procedure EditDescription;
    procedure ImportFromQDir41(FileName: ShortString);
    procedure GoToParent;
    procedure DoSelection;
    procedure SelectAll;
    procedure UnselectAll;
    procedure MakeCopy;
    procedure MakeCopyDisks;
    procedure MakeCopyFiles;
    procedure MakePrintDisk;
    procedure MakePrintTree;
    procedure MakePrintFiles;

    procedure DoPrint(PrintWhat: TPrintWhat);
    procedure ExportToOtherFormat;
    procedure ResizeHeaderBottom;
    procedure ScanQDir4Database;
    procedure SelectDisksOrFiles(Disk: boolean);
    procedure SelectAllDisksOrFiles(Disk: boolean);
    procedure UnselectDisksOrFiles(Disk: boolean);
    procedure DrawGridFilesToggleSelection;
    procedure ShowOrHideTreePanel;



    function  FindOneFileLine(Point: TPoint; var Rect: TRect): TOneFileLine;
    function  AttachDBase(FName: ShortString): boolean;
    function  DBaseIsEmpty: boolean;

    function  ScanDisk (Drive: char; AutoRun: boolean): boolean; // returns false when interrupted
    function  DoScan (Drive: char;
                      StartPath: ShortString; DiskName: ShortString;
                      AutoRun, NoOverWarning: boolean): boolean;
    function  ActivePanel: Integer;
    function  CanUndeleteRecord: boolean;
    function  GetDBaseHandle: PDBaseHandle;
    function  GetSelectedFileName: ShortString;

    // ------------------
  public
    DBaseHandle        : PDBaseHandle;
    DBaseFileName      : ShortString;

    function  GetFoundForm(FormType: byte): TForm;
    function  IsDBaseOpened(DBaseFileName: ShortString; ToFront: boolean): boolean;
    procedure AddToLastFilesList(FileName: ShortString);
    procedure DeleteFromLastFilesList(FileName: ShortString);
    procedure DefaultHandler(var Message); override;

    // --- redesigned
    procedure LocalTimer;
    procedure JumpTo(Disk, Dir, FileName: ShortString);


  protected
    ///procedure OnDeviceChange(var Message: TMessage); message WM_DEVICECHANGE;
  end;

var
  MainForm: TMainForm;
     g_PanelHeaderWidths  : TPanelHeaderWidths;

const
  FatalErrorHappened: boolean = false;

{$ifdef DELPHI1}
  MaxLastFilesToShow: integer = 5;
{$else}
  MaxLastFilesToShow: integer = 10;
{$endif}

function GetSortString(OneFile: TOneFile; FileType: TFileType;
                       SortCrit: TSortCrit; Reversed: boolean): ShortString;

//-----------------------------------------------------------------------------

implementation

uses {FDBase,} FFoundFile, FFoundEmpty, FSelectDrive, FReindex, FAbout,
     UBaseUtils, UApi, ULang, UDrives, FScanFolder, FRepair,
     FFindFileDlg, FMoreOptions, FDescription, FSelectFolder,
     UDskBUtl, UDebugLog, UPluginDiscGear,

     Clipbrd, {Printers,} UExceptions, {ShellApi,}
     FAskForLabel, FScanProgress, FDiskInfo, FDbInfo,
     FFindFile, FFindEmpty, FFindEmptyDlg,
     FRenameDlg,
     FMaskSelect, UCollections, FAbortPrint, FDiskPrint,
     UPrinter, StdCtrls,
     FHidden, UExport, FDiskExport, UUserDll;



{$R *.dfm}


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



//-----------------------------------------------------------------------------
// Displays error message and if the error is fatal, closes the application

procedure ErrorMsg(ErrorNo: Integer; ErrorStr: ShortString); ///far;

  var
    MsgText, MsgCaption: array[0..256] of char;

  begin
  if not FatalErrorHappened then
    begin
    Application.MessageBox(StrPCopy(MsgText, ErrorStr + lsFatalInternalError +
                           IntToStr(ErrorNo) + ')'),
                           StrPCopy(MsgCaption, lsProblem), mb_Ok);
    FatalErrorHappened := true;
    end;
  ///PostMessage(Application.Handle, WM_CLOSE, 0, 0);
  end;

//-----------------------------------------------------------------------------
// Initialization made when the form is created

procedure TMainForm.FormCreate(Sender: TObject);
  var
    i: Integer;

  begin
  FormIsClosed := false;
  PanelsLocked := false;

  EnableFirstIdle := true;
  Application.OnIdle := AppIdle;
  LastActiveMDIChild := nil;
  ParamDBaseName := '';
  ParamFindFile  := '';
  ParamScanDisk  := '';
  ParamAuto      := false;
  Application.HelpFile := GetProgramDir + helpFileName;
  LastUsedDir    := '';
  LastOpenedFile := '';
  FillChar (LastFiles, SizeOf(LastFiles), 0);
  m_EjectDriveLetter1 := ' ';
  m_EjectDriveLetter2 := ' ';
  SetFormSize; // must be here because of setting Minimized/normal/maximized state

  ///HeaderTop.Font.Size := 9;
  ///StatusBar.Font.Size := 9;
  MaxFileNamePxLength := 50;
  UpdateSize;
  PanelsLocked := false;
  ///LastMouseMoveTime := $0FFFFFFF;
  LastMouseMoveTime := $0FFFFFF;
  ///FilesHintDisplayed := false;
  FilesHintDisplayed := true;
  DisableHints := 0;
  ///FilesList            := TQStringList.Create;
  FilesList            := TStringList.Create;
  FilesList.Sorted     := true;
  ///FilesList.Reversed   := false;
  FilesList.Duplicates:= dupAccept;
  NeedReSort           := false;

  StatusSection0Size := 0;
  Tag := GetNewTag;
  MakeDiskSelection := false;
  MakeFileSelection := false;
  MaxFileNameLength := 12;
  MaxFileNamePxLength := 50;
  FillChar(SubTotals, SizeOf(SubTotals), 0);
  DisableNextDoubleClick := false;
  SetRectEmpty(LastHintRect);
  TreePtrCol      := New(PQPtrCollection, Init(1000, 1000));
  BitmapFolder := TBitmap.Create;
  BitmapParent := TBitmap.Create;
  BitmapArchive := TBitmap.Create;
  ImageList.GetBitmap(3,BitmapFolder);
  ImageList.GetBitmap(0,BitmapParent);
  ImageList.GetBitmap(2,BitmapArchive);
  //BitmapFolder    := OutlineTree.PictureLeaf;
  ///BitmapParent    := OutlineTree.PicturePlus;
  ///BitmapArchive   := OutlineTree.PictureMinus;


  end;

procedure TMainForm.PopupMenuDiskPopup(Sender: TObject);
begin
  MenuDeleteDisk.Enabled := not DBaseIsReadOnly;
  MenuRenameDisk.Enabled := not DBaseIsReadOnly;
  MenuUndeleteDisk.Enabled := CanUndeleteRecord and not DBaseIsReadOnly;
end;


//--------------------------------------------------------------------
// Menu handler - Rescans the current disk

procedure TMainForm.MenuRescanClick(Sender: TObject);
  begin
  RescanDisk;
  end;

//--------------------------------------------------------------------
// Menu handler - deletes the current disk

procedure TMainForm.MenuDeleteDiskClick(Sender: TObject);
  begin
  DeleteRecord;
  end;

//--------------------------------------------------------------------
// Menu handler - undeletes the disk

procedure TMainForm.MenuUndeleteDiskClick(Sender: TObject);
begin
UndeleteRecord;
end;

//--------------------------------------------------------------------
// Menu handler - renames disk in the database

procedure TMainForm.MenuRenameDiskClick(Sender: TObject);
begin
ChangeDiskName;
end;

//--------------------------------------------------------------------
// Menu handler - select disks by a mask

procedure TMainForm.MenuSelectDisksClick(Sender: TObject);
begin
SelectDisksOrFiles(true);
end;

//--------------------------------------------------------------------
// Menu handler - unselect disks

procedure TMainForm.MenuDeselectDiskClick(Sender: TObject);
begin
UnselectDisksOrFiles(true);
end;


//--------------------------------------------------------------------
// Menu handler - select disks/files

procedure TMainForm.MenuSelectFilesClick(Sender: TObject);
begin
SelectDisksOrFiles(false);
end;


//--------------------------------------------------------------------
// Menu handler - unselect disks/files

procedure TMainForm.MenuUnselectFilesClick(Sender: TObject);
begin
UnselectDisksOrFiles(false);
end;


//--------------------------------------------------------------------
// Menu handler - select all files

procedure TMainForm.MenuSelectAllFilesClick(Sender: TObject);
begin
SelectAllDisksOrFiles(false);
end;


//--------------------------------------------------------------------
// Menu handler - select all disks

procedure TMainForm.MenuSelectAllDisksClick(Sender: TObject);
begin
SelectAllDisksOrFiles(true);
end;



//--------------------------------------------------------------------
// Menu handler - copy disk to clipboard

procedure TMainForm.MenuCopyDiskClick(Sender: TObject);
begin
MakeCopyDisks;
end;



//--------------------------------------------------------------------
// Menu handler - display disk info

procedure TMainForm.MenuInfoDiskClick(Sender: TObject);
begin
ShowDiskInfo;
end;

//--------------------------------------------------------------------
// Menu handler - print disks

procedure TMainForm.MenuPrintDisksClick(Sender: TObject);
begin
MakePrintDisk;
end;

//--------------------------------------------------------------------
// Menu handler - display help for disk panel

procedure TMainForm.MenuHelpDisksClick(Sender: TObject);
begin
Application.HelpContext(150);
end;




//--------------------------------------------------------------------
// Menu handler - called when the popup menu (on right mouse button)
// is called for the files panel

procedure TMainForm.PopupMenuFilesPopup(Sender: TObject);
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

procedure TMainForm.PopupMenuTreePopup(Sender: TObject);
begin
  MenuShowTree.Checked := QGlobalOptions.ShowTree;
end;



//--------------------------------------------------------------------
// Copies disks to clipboard

procedure TMainForm.MakeCopyDisks;

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


//-----------------------------------------------------------------------------
// Initialization done when the form is about to show

procedure TMainForm.FormShow(Sender: TObject);

  var
    DriveNum,i: Integer;
    DriveChar: Char;
    DriveType: TDriveType;
    DriveBits: set of 0..25;

  begin
  FormDescription.SetFormSize;
  FormSelectFolder.SetFormSize;
  FormSelectDrive.SetFormSize;

  QGlobalOptions := TGlobalOptions.Create;
  FormSettings.GetOptions(QGlobalOptions);

  ShowOrHideTreePanel;

  //LoadSettingsIni;
  //if EnableFirstIdle then FirstIdle;

  for i := 0 to 2 do HeaderTop.Sections[i].Width := QGlobalOptions.PanelHeaderWidths[i];
  for i := 0 to 2 do HeaderWidths[i] := HeaderTop.Sections[i].Width;
  ///DrawGridDisks.Width        := HeaderTop.SectionWidth[0];
  Disks.Width        := HeaderTop.Sections[0].Width;
  Disks.ColWidths[0] := Disks.Width-2;

  ///OutlineTree.Width          := HeaderTop.SectionWidth[1];
  DiskTree.Width          := HeaderTop.Sections[1].Width;

  //g_PanelHeaderWidths := HeaderWidths;

  ResetDrawGridFiles;

  // determine, whether the CD eject button should be visible
  ///Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
    begin
    if not (DriveNum in DriveBits) then Continue;
    DriveType := FindDriveType(DriveNum);
    if (DriveType = dtCDROM) then
      begin
      if (not SpeedButtonEject1.Visible)
        then
          begin
          SpeedButtonEject1.Visible := true;
          DriveChar := Char(DriveNum + Ord('a'));
          DriveChar := Upcase(DriveChar);
          m_EjectDriveLetter1 := DriveChar;
          SpeedButtonEject1.Hint := lsEject + DriveChar + ':';
          Continue;
          end
        else
          if (not SpeedButtonEject2.Visible) then
            begin
            SpeedButtonEject2.Visible := true;
            DriveChar := Char(DriveNum + Ord('a'));
            DriveChar := Upcase(DriveChar);
            m_EjectDriveLetter2 := DriveChar;
            SpeedButtonEject2.Hint := lsEject + DriveChar + ':';
            Break;
            end;
      end;
    end;

  if (DiscGearPrintPluginExists()) then
    begin
    MenuTools.Visible := true;
    MenuDiscGearPrint.Visible := true;
    end;

  end;

//-----------------------------------------------------------------------------
// Handles event issued when the the form is to be closed, if unregistered version
// is used, displays help with the "Order" topic

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  var
  /// ProgramDir: array[0..256] of char;
    i: integer;
  begin
  SaveWinSizes;
  AddToLastFilesList(DBaseFileName);
  ///Application.HelpCommand(HELP_QUIT, 0);
  SaveSettingsIni;
  DeleteTempFolder();

  QI_CloseDatabase(DBaseHandle, false);
  QGlobalOptions.Free;
  FreeObjects(FilesList);
  FilesList.Free;
  if TreePtrCol <> nil then
    Dispose(TreePtrCol, Done);
  TreePtrCol := nil;
  BitmapFolder.Free;
  BitmapParent.Free;
  BitmapArchive.Free;

  CanClose := true;
  end;

//-----------------------------------------------------------------------------
// Menu handler - create a new database

procedure TMainForm.MenuNewClick(Sender: TObject);

  begin
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(LastOpenedFile);
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(FormSettings.EditAutoLoadDBase.Text);
  NewDialog.InitialDir := LastUsedDir;
  NewDialog.FileName := lsNonameFile;
  if NewDialog.Execute then
    begin
    LastUsedDir := ExtractDir(NewDialog.FileName);
    // check if the same database is already opened
    if IsDBaseOpened (NewDialog.FileName, false) then
      begin
      Application.MessageBox(lsDbaseWithThisNameIsOpen,
        lsCannotCreate, mb_OK or mb_IconStop);
      exit;
      end;
    if not QI_CreateDatabase(NewDialog.FileName)
      then
        Application.MessageBox(lsDBaseCannotBeCreated,
          lsError, mb_OK or mb_IconStop)
      else
        begin
        OpenDatabase(NewDialog.FileName);
        LastOpenedFile := NewDialog.FileName;
        end;
    end;
  MenuBarClick(Self); // enable shortcuts in the menu
  end;

procedure TMainForm.DisksResize(Sender: TObject);
begin
  UpdateSize;
end;

procedure TMainForm.DisksSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var i: Integer;

begin
if not QI_DatabaseIsOpened (DBaseHandle) then exit;
try
  if MakeDiskSelection then
    begin
    MakeDiskSelection := false;
    if LastDiskSelected <= aRow
      then
        for i := LastDiskSelected to pred(aRow) do
          QI_ToggleSelectionFlag(DBaseHandle, i)
      else
        for i := LastDiskSelected downto succ(aRow) do
          QI_ToggleSelectionFlag(DBaseHandle, i);
    if abs(LastDiskSelected - aRow) > 1 then
      Disks.Repaint;
    end;
  if Disks.Selection.Top <> aRow then
    begin
    LastDiskSelected := Disks.Selection.Top;
    QI_SetCurrentKeyPos(DBaseHandle, aRow);
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

procedure TMainForm.DiskTreeClick(Sender: TObject);
var
  done: boolean;
begin
///if FormIsClosed then exit;
if not QI_DatabaseIsOpened (DBaseHandle) then exit;
try
  if not QGlobalOptions.ShowTree then exit;
  with DiskTree do
    begin
    ///if SelectedItem > 0 then
    if Selected.Index >= 0 then
      begin
      ///if Items[SelectedItem].Data <> nil then
      //if Items[Selected.Index].Data <> nil then
      if Items[Selected.AbsoluteIndex].Data <> nil then
        begin
        ///QI_SetCurDirCol (DBaseHandle, Items[SelectedItem].Data);
        QI_SetCurDirCol (DBaseHandle, Items[Selected.AbsoluteIndex].Data);
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

procedure TMainForm.ChangePanel(Sender: TObject);
var
  APanel: Integer;

begin
APanel := ActivePanel;
EraseFileHint;
{
if APanel = 1
  ///then HeaderTop.Sections.Strings[0] := ShortDBaseFileName
  ///else HeaderTop.Sections.Strings[0] := '';
  then HeaderTop.Sections[0].Text := ShortDBaseFileName
  else HeaderTop.Sections[0].Text := '';
if APanel = 2
  then UpdateHeader
  ///else HeaderTop.Sections.Strings[1] := '';
  else HeaderTop.Sections[1].Text := '';
}
if APanel = 3
  then UpdateHeader
  ///else HeaderTop.Sections.Strings[2] := '';
  else HeaderTop.Sections[2].Text := '';
///HeaderTop.SectionWidth[0] := HeaderWidths[0];
///HeaderTop.SectionWidth[2] := HeaderWidths[2];
HeaderTop.Sections[0].Width := HeaderWidths[0];
//Disks.ColWidths[0] := HeaderWidths[0] - 5;
HeaderTop.Sections[2].Width := HeaderWidths[2];
if QGlobalOptions.ShowTree
  ///then HeaderTop.SectionWidth[1] := HeaderWidths[1]
  ///else HeaderTop.SectionWidth[1] := 0;
  then HeaderTop.Sections[1].Width := HeaderWidths[1]
  else HeaderTop.Sections[1].Width := 0;
end;


procedure TMainForm.DrawGridFilesDblClick(Sender: TObject);
var
  i   : Integer;

begin
///if FormIsClosed then exit;
if not QI_DatabaseIsOpened (DBaseHandle) then exit;
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
              ///if FileType <> ftParent then OpenExplorer(POneFile);
              end
            else
              begin // click on file
              ///ExecFile(POneFile);
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
                    ///ExecFile(POneFile);
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

procedure TMainForm.DrawGridFilesDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
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
          i := aCol*DrawGridFiles.RowCount + aRow;
          if i < FilesList.Count then
            begin
            OneFileLine := TOneFileLine(FilesList.Objects[i]);
            Offset := 0;
            if (OneFileLine.ExtAttr and eaSelected <> 0)
             and not(gdSelected in aState) then
              begin
              DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
              DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
              DrawGridFiles.Canvas.FillRect(aRect);
              end;
            if QGlobalOptions.ShowIcons then
              begin
              inc(Offset, BitmapFolder.Width);
              case OneFileLine.FileType of
                ftDir   : Bitmap := BitmapFolder;
                ftParent: Bitmap := BitmapParent;
                ftArc   : Bitmap := BitmapArchive;
                else      Bitmap := BitmapFolder;
                end;
              if (OneFileLine.ExtAttr and eaSelected <> 0)
               and not(gdSelected in aState)  then
                DrawGridFiles.Canvas.FillRect(Rect(aRect.Left, aRect.Top,
                                              aRect.Left + Offset, aRect.Bottom));
              DrawGridFiles.Canvas.Brush.Color:=clGreen;
              if (OneFileLine.FileType <> ftFile) then
                ///DrawGridFiles.Canvas.BrushCopy(
                ///      Bounds(aRect.Left + 1, aRect.Top, Bitmap.Width, Bitmap.Height),
                ///      Bitmap, Bounds(0, 0, Bitmap.Width, Bitmap.Height), clOlive);
                DrawGridFiles.Canvas.CopyRect(
                    Bounds(aRect.Left + 1, aRect.Top, Bitmap.Width, Bitmap.Height),
                    Bitmap.Canvas, Bounds(0, 0, Bitmap.Width, Bitmap.Height));
              end;
            DrawGridFiles.Canvas.TextRect(Rect(aRect.Left + Offset, aRect.Top,
                                           aRect.Right, aRect.Bottom), aRect.Left + Offset + 1, aRect.Top,
                                           OneFileLine.POneFile^.LongName
                                           + OneFileLine.POneFile^.Ext);
            end;
          end
        else
          begin
          if aCol <= maxColumnAttribs then
            begin
            if aRow = 0 then ColumnAttrib[aCol].Width := DrawGridFiles.ColWidths[aCol];
            {this is becuase of non-existence of event on resizing columns}
            if aRow <= FilesList.Count then
              begin
              case ColumnAttrib[aCol].Content of
                coName:
                  if aRow = 0
                    then
                      DrawGridFiles.Canvas.TextRect(aRect, aRect.Left+2, aRect.Top,
                        lsName)
                    else
                      begin
                      OneFileLine := TOneFileLine(FilesList.Objects[aRow-1]);
                      Offset := 0;
                      if (OneFileLine.ExtAttr and eaSelected <> 0)
                       and not(gdSelected in aState) then
                        begin
                        DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                        DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                        DrawGridFiles.Canvas.FillRect(aRect);
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
                        DrawGridFiles.Canvas.FillRect(Classes.Rect(aRect.Left, aRect.Top,
                                                      aRect.Left + Offset, aRect.Bottom));
                        if OneFileLine.FileType <> ftFile then
                        ///DrawGridFiles.Canvas.BrushCopy(
                        ///        Bounds(aRect.Left + 1, aRect.Top, Bitmap.Width, Bitmap.Height),
                        ///        Bitmap, Bounds(0, 0, Bitmap.Width, Bitmap.Height), clGreen {clOlive});
                        DrawGridFiles.Canvas.CopyRect(
                                Bounds(aRect.Left + 1, aRect.Top, Bitmap.Width, Bitmap.Height),
                                Bitmap.Canvas, Bounds(0, 0, Bitmap.Width, Bitmap.Height));
                        end;
                      DrawGridFiles.Canvas.TextRect(Classes.Rect(aRect.Left + Offset, aRect.Top,
                                            aRect.Right, aRect.Bottom), aRect.Left + Offset + 1,
                                            aRect.Top,
                                            OneFileLine.POneFile^.LongName
                                             + OneFileLine.POneFile^.Ext);
                      //LOG('DrawGridFiles.Canvas.TextRect', []);
                      end;
                coTime:
                  begin
                  if aRow = 0
                    then
                      begin
                      S := lsDateAndTime;
                      StartX := aRect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                      DrawGridFiles.Canvas.TextRect(aRect, StartX, aRect.Top, S);
                      end
                    else
                      begin
                      OneFileLine := TOneFileLine(FilesList.Objects[aRow-1]);
                      DosDateTime := OneFileLine.POneFile^.Time;
                      if (OneFileLine.ExtAttr and eaSelected <> 0)
                       and not(gdSelected in aState) then
                        begin
                        DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                        DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                        DrawGridFiles.Canvas.FillRect(aRect);
                        end;
                      if DosDateTime <> 0
                        then
                          begin
                          S := DosTimeToStr(DosDateTime, QGlobalOptions.ShowSeconds);
                          StartX := aRect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                          DrawGridFiles.Canvas.TextRect(aRect, StartX, aRect.Top, S);
                          S := DosDateToStr(DosDateTime);
                          PartRect := aRect;
                          PartRect.Right := PartRect.Right - TimeWidth;
                          if PartRect.Right < PartRect.Left then PartRect.Right := PartRect.Left;
                          StartX := PartRect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                          DrawGridFiles.Canvas.TextRect(PartRect, StartX, aRect.Top, S);
                          end
                        else
                          if (OneFileLine.ExtAttr and eaSelected <> 0)
                           and not(gdSelected in aState) then
                            DrawGridFiles.Canvas.FillRect(Classes.Rect(aRect.Left, aRect.Top,
                                                          aRect.Right, aRect.Bottom));

                      end;
                  end;
                coSize:
                  begin
                  if aRow = 0
                    then
                      S := lsSize
                    else
                      begin
                      OneFileLine := TOneFileLine(FilesList.Objects[aRow-1]);
                      FileType := OneFileLine.FileType;
                      if (OneFileLine.ExtAttr and eaSelected <> 0)
                       and not(gdSelected in aState) then
                        begin
                        DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                        DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                        DrawGridFiles.Canvas.FillRect(aRect);
                        end;
                      case FileType of
                        ftDir   : S := lsFolder;
                        ftParent: S := '';
                        else      S := FormatSize(
                                      TOneFileLine(FilesList.Objects[aRow-1]).POneFile^.Size,
                                      QGlobalOptions.ShowInKb);
                        end;
                      end;
                  StartX := aRect.Right - DrawGridFiles.Canvas.TextWidth(S) - 2;
                  DrawGridFiles.Canvas.TextRect(aRect, StartX, aRect.Top, S);
                  end;
                coDesc:
                  if aRow = 0
                    then
                      DrawGridFiles.Canvas.TextRect(aRect, aRect.Left+2, aRect.Top,
                        lsDescription)
                    else
                      begin
                      OneFileLine := TOneFileLine(FilesList.Objects[aRow-1]);
                      Description := OneFileLine.POneFile^.Description;
                      if (OneFileLine.ExtAttr and eaSelected <> 0)
                       and not(gdSelected in aState) then
                        begin
                        DrawGridFiles.Canvas.Brush.Color := clQSelectedBack;
                        DrawGridFiles.Canvas.Font.Color  := clQSelectedText;
                        DrawGridFiles.Canvas.FillRect(aRect);
                        end;
                      if Description <> 0
                        then
                          begin
                          DrawGridFiles.Canvas.FillRect(Classes.Rect(aRect.Left, aRect.Top,
                                                        aRect.Right, aRect.Bottom));
                          if QI_GetShortDesc(DBaseHandle, Description, S) then
                            DrawGridFiles.Canvas.TextRect(aRect, aRect.Left + 1, aRect.Top, S);
                          end
                        else
                          if (OneFileLine.ExtAttr and eaSelected <> 0)
                           and not(gdSelected in aState) then
                            DrawGridFiles.Canvas.FillRect(Classes.Rect(aRect.Left, aRect.Top,
                                                          aRect.Right, aRect.Bottom));
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

procedure TMainForm.DrawGridFilesKeyDown(Sender: TObject; var Key: Word;
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



procedure TMainForm.DrawGridFilesSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
  var
    i            : Integer;
    bNeedRepaint : boolean;

  begin
  if not QI_DatabaseIsOpened (DBaseHandle) then exit;
  EraseFileHint;
  if QGlobalOptions.FileDisplayType = fdBrief
    then
      begin
      i := aCol*DrawGridFiles.RowCount + aRow;
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
      CanSelect := aRow <= FilesList.Count;
      if CanSelect then
        begin
        UpdateStatusLine(aRow-1, false);
        LastFileSelected := CurFileSelected;
        CurFileSelected  := aRow-1;
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

procedure TMainForm.FormResize(Sender: TObject);
begin
  if FormIsClosed then exit;
  FormWasResized := 1;
end;


procedure TMainForm.DisksEnter(Sender: TObject);
  begin
  ///SetBriefFileDisplay(true);
  //SetBriefFileDisplay(QGlobalOptions.FileDisplayType=fdBrief);
  PopupMenuFilesPopup(Sender);
end;

procedure TMainForm.DisksKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
  if FormIsClosed then exit;
  if (Shift = []) then
    begin
    if ((Key = vk_Insert) or (Key = vk_Space)) then
      begin
        DisksDblClick(Sender);
      if (Disks.Row + 1) < Disks.RowCount then
        Disks.Row := Disks.Row + 1;
      end;
    with Disks do
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
          LastDiskSelected := Disks.Selection.Top;
          MakeDiskSelection := true;
          end;
end;


procedure TMainForm.DisksMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  var
    i: Integer;

  begin
  if FormIsClosed then exit;
  try
    if Button = mbLeft then
      begin
      if LastDiskSelected >= Disks.RowCount then
        LastDiskSelected := pred(Disks.RowCount);
      if ssShift in Shift then
        begin
        for i := 0 to pred(Disks.RowCount) do
          begin
          QI_SetSelectionFlag(DBaseHandle, false, i);
          end;
        if LastDiskSelected <= Disks.Row
          then
            for i := LastDiskSelected to Disks.Row do
              begin
              QI_ToggleSelectionFlag(DBaseHandle, i);
              end
          else
            for i := LastDiskSelected downto Disks.Row do
              begin
              QI_ToggleSelectionFlag(DBaseHandle, i);
              end;
        Disks.Repaint;
        end;
      if ssCtrl in Shift then
        begin
        QI_ToggleSelectionFlag(DBaseHandle, Disks.Row);
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

procedure TMainForm.DisksDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
  var
    Key        : ShortString;
    Attr       : Word;
    IsDeleted  : boolean;
    IsSelected : boolean;

  begin
  //LOG('DrawGridDisksDrawCell', []);
  //if FormIsClosed then exit;
  if not QI_DatabaseIsOpened (DBaseHandle) then exit;
  try
    if QI_GetKeyAt(DBaseHandle, Key, Attr, aRow) then
      begin
      IsDeleted   := Attr and kaDeleted <> 0;
      IsSelected  := Attr and kaSelected <> 0;
      if IsDeleted then Key:= '(' + Key + ')';
      if IsSelected then Key := Key + '*';
      if IsDeleted and not(gdSelected in aState) then
        Disks.Canvas.Font.Color := clQDeletedText;
      if IsSelected and not(gdSelected in aState) then
        begin
          Disks.Canvas.Brush.Color := clQSelectedBack;
          Disks.Canvas.Font.Color  := clQSelectedText;
        end;
      Disks.Canvas.FillRect(aRect);
      Disks.Canvas.TextRect(aRect, aRect.Left+1, aRect.Top,key);
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

procedure TMainForm.DisksDblClick(Sender: TObject);
begin
if FormIsClosed then exit;
try
  QI_ToggleSelectionFlag(DBaseHandle, Disks.Row);
  Disks.Repaint;
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

procedure TMainForm.DisksClick(Sender: TObject);
var
  done: boolean;
begin
  AppIdle(sender,done);
end;


//-----------------------------------------------------------------------------
// Menu handler - Open database

procedure TMainForm.MenuOpenDBaseClick(Sender: TObject);
var
  Done: boolean;
  begin
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(LastOpenedFile);
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(FormSettings.EditAutoLoadDBase.Text);
  OpenDialog.InitialDir := LastUsedDir;
  OpenDialog.FileName := '*.qdr';
  if OpenDialog.Execute then
    begin
    LastUsedDir := ExtractDir(OpenDialog.FileName);
    if not IsDBaseOpened (OpenDialog.FileName, true) then
      begin
      FormIsClosed:=false;
      DeleteFromLastFilesList(OpenDialog.FileName);
      OpenDatabase(OpenDialog.FileName);
      LastOpenedFile := OpenDialog.FileName;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Implements opening the database

procedure TMainForm.OpenDatabase (DBFileName: ShortString);

  ///var
    ///FormDBase: TFormDBase;
    //--SaveWindowState: TWindowState;

  begin
  // create the MDI window for the database
  {
  if ActiveMDIChild = nil // it will be the first window
    then
      begin
      FormDBase := TFormDBase.Create(Self);
      if DefaultMDIMaximized
        then FormDBase.WindowState := wsMaximized
        else FormDBase.WindowState := wsNormal
      end
    else
      begin
      SaveWindowState := ActiveMDIChild.WindowState;
      FormDBase := TFormDBase.Create(Self);
      FormDBase.WindowState := SaveWindowState;
      end;
   }
  // attach the database to the MDI window
  ///if not FormDBase.AttachDBase(DBFileName) then
  if not AttachDBase(DBFileName) then
    exit;

  // check if the database was opened from the command line
  if ParamFindFile <> '' then
    begin
    Application.ProcessMessages;
    SearchParam(ParamFindFile);
    ParamFindFile := '';
    end;
  if ParamScanDisk <> '' then
    begin
    while (ParamScanDisk <> '') do
      begin
      Application.ProcessMessages;
      ScanDisk(ParamScanDisk[1], ParamAuto);
      ShortDelete (ParamScanDisk, 1, 1);
      if (not ParamAuto) then ParamScanDisk := '';
      end;
    ///if ParamAuto then PostMessage(Handle, WM_CLOSE, 0, 0);
    end;
  end;

//-----------------------------------------------------------------------------
// Called from FormCreate to set the application window size

procedure TMainForm.SetFormSize;

  var
    IniFile: TIniFile;
    SLeft, STop, SWidth, SHeight: integer;
    SMinimized, SMaximized: boolean;
    sIniFileName: AnsiString;

  begin
  sIniFileName := ChangeFileExt(ParamStr(0), '.ini');
  if FileExists(sIniFileName)
    then
      begin
      IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
      SLeft   := IniFile.ReadInteger  ('Application', 'Left',   0);
      STop    := IniFile.ReadInteger  ('Application', 'Top',    0);
      SWidth  := IniFile.ReadInteger  ('Application', 'Width',  Screen.Width);
      SHeight := IniFile.ReadInteger  ('Application', 'Height', Screen.Height - 80);
      SMaximized :=  IniFile.ReadBool ('Application', 'Maximized', WindowState = wsMaximized);
      SMinimized :=  IniFile.ReadBool ('Application', 'Minimized', WindowState = wsMinimized);
      DefaultMDIMaximized := IniFile.ReadBool ('Application', 'MDIChildMaximized', true);
      IniFile.Free;
      end
    else
      begin
      SLeft   := 0;
      STop    := 0;
      SWidth  := Screen.Width;
      SHeight := Screen.Height - 80;
      SMaximized :=  WindowState = wsMaximized;
      SMinimized :=  WindowState = wsMinimized;
      DefaultMDIMaximized := true;
      end;

  if SMaximized
    then
      WindowState := wsMaximized
    else
      if SMinimized
        then WindowState := wsMinimized
        else
          begin
          WindowState := wsNormal;
          Position := poDesigned;
          SetBounds(SLeft, STop, SWidth, SHeight);
          end;
  end;

//-----------------------------------------------------------------------------
// Called one-time, when the application does not have anything to do for the first time

procedure TMainForm.FirstIdle;

  var
    TmpSt  : array[0..256] of char;

  begin
  EnableFirstIdle := false;
  LoadSettingsIni;

  // check if Diskbase was run with command line parameters
  if ParamCount > 0
    then
      begin
      if ProcessParams and (ParamDBaseName <> '') then
        if FileExists(ParamDBaseName)
          then
            begin
            OpenDatabase(ParamDBaseName);
            LastOpenedFile := ParamDBaseName;
            DeleteFromLastFilesList(ParamDBaseName);
            end
          else
            begin
            StrPCopy(TmpSt, lsFileDoesNotExist + ParamDBaseName);
            Application.MessageBox(TmpSt, lsErrorOnCommandLine,
                                   mb_OK or mb_IconExclamation)
            end;
      end
    else
      begin
      // should we open last opened database?
      if FormSettings.CheckBoxOpenLastOpened.Checked
        then
          ParamDBaseName := LastOpenedFile
        else
          ParamDBaseName := FormSettings.EditAutoLoadDBase.Text;
      if (ParamDBaseName <> '') then
        if FileExists(ParamDBaseName)
          then
            begin
            OpenDatabase(ParamDBaseName);
            LastOpenedFile := ParamDBaseName;
            DeleteFromLastFilesList(ParamDBaseName);
            end
          else
            begin
            StrPCopy(TmpSt, lsFileDoesNotExist + ParamDBaseName);
            Application.MessageBox(TmpSt, lsErrorInProgramSettings,
                                   mb_OK or mb_IconExclamation)
            end;
      end;
  end;

//-----------------------------------------------------------------------------
// Dispatches the command line parameters

function TMainForm.ProcessParams: boolean;

  var
    i, j    : Integer;
    LastJ   : Integer;
    Options : ShortString;
    TmpSt   : array[0..256] of char;
    InBrackets: boolean;
    OneParamStr: ShortString;

  begin
  Result         := true;
  ParamDBaseName := '';
  ParamFindFile  := '';
  ParamScanDisk  := '';
  ParamAuto      := false;
  Options        := '';
  InBrackets     := false;
  for i := 1 to ParamCount do
    begin
    if InBrackets or (ShortCopy(ParamStr(i),1,1) = '/') or (ShortCopy(ParamStr(i),1,1) = '-')
      then
        begin
        OneParamStr := ParamStr(i);
        for j := 1 to length(OneParamStr) do
          if OneParamStr[j]='|' then InBrackets := not InBrackets;
        Options := Options + OneParamStr;
        if InBrackets then Options := Options + ' ';
        end
      else
        begin
        if ParamDBaseName = ''
          then
            ParamDBaseName := ExpandFileName(ParamStr(i))
          else
            begin
            StrPCopy(TmpSt, lsInvalidParameter + ParamStr(i));
            Application.MessageBox(TmpSt, lsErrorOnCommandLine,
                                   mb_Ok or mb_IconExclamation);
            Result := false;
            break;
            end;
        end;
    end;
  Options := AnsiLowerCase(Options);

  i := Pos('/f:', Options);
  if i > 0 then
    begin
    InBrackets := false;
    LastJ := i+3; // to satisfy compiler
    for j := i+3 to length(Options) do
      begin
      LastJ := j;
      if Options[j] = '|' then InBrackets := not InBrackets;
      if not InBrackets and
        ((Options[j] = ' ') or (Options[j] = '/')) then
          begin
          ShortDelete(Options, i, j-i);
          break;
          end;
      if Options[j] <> '|' then
        ParamFindFile := ParamFindFile + Options[j];
      end;
    if LastJ = length(Options) then Options := '';
    end;

  i := Pos('/s:', Options);
  if i > 0 then
    begin
    InBrackets := false;
    LastJ := i+3; // for compiler
    for j := i+3 to length(Options) do
      begin
      LastJ := j;
      if Options[j] = '|' then InBrackets := not InBrackets;
      if not InBrackets and
        ((Options[j] = ' ') or (Options[j] = '/')) then
          begin
          ShortDelete(Options, i, j-i);
          break;
          end;
      ParamScanDisk := ParamScanDisk + Options[j];
      end;
    if LastJ = length(Options) then Options := '';
    end;

  i := Pos('/a', Options);
  if i > 0 then
    begin
    ShortDelete(Options, i, 2);
    ParamAuto := true;
    end;

  if (ParamScanDisk <> '') and (ParamFindFile <> '') then
    begin
    StrPCopy(TmpSt, lsOptionsCannotBeMixed);
    Application.MessageBox(TmpSt, lsErrorOnCommandLine,
                           mb_Ok or mb_IconExclamation);
    ParamScanDisk := '';
    ParamFindFile := '';
    Result := false;
    end;

  if Options <> '' then
    begin
    StrPCopy(TmpSt, lsInvalidOption + Options);
    Application.MessageBox(TmpSt, lsErrorOnCommandLine,
                           mb_Ok or mb_IconExclamation);
    ParamScanDisk := '';
    ParamFindFile := '';
    Result := false;
    end;

  end;

//-----------------------------------------------------------------------------
// Called by VCL when there is no messages in the queue

procedure TMainForm.AppIdle(Sender: TObject; var Done: Boolean);

  begin
  if EnableFirstIdle then FirstIdle;
  {
  if Screen.ActiveForm is TFormDBase then
    with Screen.ActiveForm as TFormDBase do
      LocalIdle;
  if Screen.ActiveForm is TFormFoundFileList then
    with Screen.ActiveForm as TFormFoundFileList do
      LocalIdle;
  if Screen.ActiveForm is TFormFoundEmptyList then
    with Screen.ActiveForm as TFormFoundEmptyList do
      LocalIdle;
  }
  LocalIdle;
  UpdateSpeedButtons;
  ///if LastActiveMDIChild <> ActiveMDIChild then
  ///  begin
    ///LastActiveMDIChild := ActiveMDIChild;
    MenuBarClick(Sender);
  ///  end;
  Done := true;
  end;

//-----------------------------------------------------------------------------
// Timer is called each 700 ms, to enable time-based actions. Used for pop-up
// bubble info

procedure TMainForm.MainTimerTimer(Sender: TObject);

  begin
  {
  if Screen.ActiveForm is TFormDBase then
    with Screen.ActiveForm as TFormDBase do
      LocalTimer;
  if Screen.ActiveForm is TFormFoundFileList then
    with Screen.ActiveForm as TFormFoundFileList do
      LocalTimer;
  if Screen.ActiveForm is TFormFoundEmptyList then
    with Screen.ActiveForm as TFormFoundEmptyList do
      LocalTimer;
  }
    LocalTimer;
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - collapses the tree

procedure TMainForm.SpeedButtonCollapseClick(Sender: TObject);

  begin
  ///if Screen.ActiveForm is TFormDBase then
  ///  with Screen.ActiveForm as TFormDBase do
  ///    begin
      DiskTree.BeginUpdate;
      DiskTree.FullCollapse;
      DiskTree.Items[1].Expand(false);
      DiskTree.EndUpdate;
  ///    end;
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - expands the tree

procedure TMainForm.SpeedButtonExpandClick(Sender: TObject);
  begin
  ///if Screen.ActiveForm is TFormDBase then
  ///  with Screen.ActiveForm as TFormDBase do
  ///    begin
      DiskTree.BeginUpdate;
      DiskTree.FullExpand;
      DiskTree.EndUpdate;
  ///    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Program settings

procedure TMainForm.MenuConfigClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  with ActiveMDIChild as TFormDBase do
  ///    FormSettings.SetOptions(QGlobalOptions);
  FormSettings.SetOptions(QGlobalOptions);
  if FormSettings.ShowModal = mrOk then
    begin
    ///
    {
    if ActiveMDIChild is TFormDBase then
      TFormDBase(ActiveMDIChild).ChangeGlobalOptions;
    if ActiveMDIChild is TFormFoundFileList then
      TFormFoundFileList(ActiveMDIChild).ChangeGlobalOptions;
    if ActiveMDIChild is TFormFoundEmptyList then
      TFormFoundEmptyList(ActiveMDIChild).ChangeGlobalOptions;
    }
    ChangeGLobalOptions;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Database settings

procedure TMainForm.MenuLocalOptionsClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    begin
    with ActiveMDIChild as TFormDBase do
      begin
      FormLocalOptions.SetOptions(QLocalOptions);
      FormLocalOptions.SetDLList(ConvertDLLs);
      end;
    if FormLocalOptions.ShowModal = mrOk then
      TFormDBase(ActiveMDIChild).ChangeLocalOptions;
    end;
    }
  FormLocalOptions.SetOptions(QLocalOptions);
  if FormLocalOptions.ShowModal = mrOk then
    ChangeLocalOptions;

  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - shows/hides the tree panel

procedure TMainForm.SpeedButtonShowTreeClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    with ActiveMDIChild as TFormDBase do
      begin
      QGlobalOptions.ShowTree := not QGlobalOptions.ShowTree;
      ShowOrHideTreePanel;
      end;
    }
    QGlobalOptions.ShowTree := not QGlobalOptions.ShowTree;
    ShowOrHideTreePanel;
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - sets brief display of files in the file panel

procedure TMainForm.SpeedButtonBriefClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SetBriefFileDisplay(true);
    SetBriefFileDisplay(true)
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - sets detailed display of files in the file panel

procedure TMainForm.SpeedButtonDetailedClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SetBriefFileDisplay(false);
  SetBriefFileDisplay(false);
  end;


//-----------------------------------------------------------------------------
// Menu handler - sets the type of file display

procedure TMainForm.SetMenuFileDisplayType (FileDisplayType: TFileDisplayType);

  begin
  case FileDisplayType of
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

//-----------------------------------------------------------------------------
// Menu handler - sets brief display of files in file panel

procedure TMainForm.MenuBriefClick(Sender: TObject);

  begin
  SpeedButtonBriefClick(Sender);
  SetMenuFileDisplayType(fdBrief);
  end;

//-----------------------------------------------------------------------------
// Menu handler - sets detailed display of files in file panel

procedure TMainForm.MenuDetailedClick(Sender: TObject);

  begin
  SpeedButtonDetailedClick(Sender);
  SetMenuFileDisplayType(fdDetailed);
  end;

//-----------------------------------------------------------------------------
// Sets sorting criteria for the file panel

procedure TMainForm.SetMenuSortCrit (SortCrit: TSortCrit);

  begin
  MenuSortName.Checked := false;
  MenuSortExt.Checked  := false;
  MenuSortTime.Checked := false;
  MenuSortSize.Checked := false;
  case SortCrit of
    scName: MenuSortName.Checked := true;
    scExt : MenuSortExt.Checked  := true;
    scTime: MenuSortTime.Checked := true;
    scSize: MenuSortSize.Checked := true;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Sort by name

procedure TMainForm.MenuSortNameClick(Sender: TObject);

  begin
  SetMenuSortCrit (scName);
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SetNeedResort(scName);
    SetNeedResort(scName);
  end;

//-----------------------------------------------------------------------------
// Menu handler - Sort by extension

procedure TMainForm.MenuSortExtClick(Sender: TObject);

  begin
  SetMenuSortCrit (scExt);
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SetNeedResort(scExt);
  SetNeedResort(scExt);
  end;

//-----------------------------------------------------------------------------
// Menu handler - Sort by date/time

procedure TMainForm.MenuSortTimeClick(Sender: TObject);

  begin
  SetMenuSortCrit (scTime);
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SetNeedResort(scTime);
    SetNeedResort(scTime);
  end;

//-----------------------------------------------------------------------------
// Menu handler - Sort by size

procedure TMainForm.MenuSortSizeClick(Sender: TObject);

  begin
  SetMenuSortCrit (scSize);
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SetNeedResort(scSize);
     SetNeedResort(scSize);
  end;

//-----------------------------------------------------------------------------
// Menu handler - Show/hide tree panel

procedure TMainForm.MenuShowTreeClick(Sender: TObject);

  begin
  //SpeedButtonShowTreeClick(Sender);
  MenuShowTree.Checked := not MenuShowTree.Checked;
  QGlobalOptions.ShowTree := not QGlobalOptions.ShowTree;
  ShowOrHideTreePanel;
  end;



//--------------------------------------------------------------------
// Menu handler - fully expands the tree

procedure TMainForm.MenuExpandTreeClick(Sender: TObject);
begin
DiskTree.BeginUpdate;
DiskTree.FullExpand;
DiskTree.EndUpdate;
end;


//--------------------------------------------------------------------
// Menu handler - collapses the tree

procedure TMainForm.MenuCollapseTreeClick(Sender: TObject);
begin
DiskTree.BeginUpdate;
DiskTree.FullCollapse;
DiskTree.Items[1].Expand(false);
DiskTree.EndUpdate;
end;


//--------------------------------------------------------------------
// Menu handler - copy files to clipboard

procedure TMainForm.MenuCopyFileClick(Sender: TObject);
begin
MakeCopyFiles;
end;


//--------------------------------------------------------------------
// Menu handler - edit the description

procedure TMainForm.MenuEditDescClick(Sender: TObject);
begin
EditDescription;
end;


//--------------------------------------------------------------------
// Menu handler - display help for file panel

procedure TMainForm.MenuHelpFilesClick(Sender: TObject);
begin
Application.HelpContext(170);
end;


//--------------------------------------------------------------------
// Menu handler - displays the help for the tree panel

procedure TMainForm.MenuHelpTreeClick(Sender: TObject);
begin
Application.HelpContext(160);
end;


//-----------------------------------------------------------------------------
// Enables/disables toolbar buttons and updates their status

procedure TMainForm.UpdateSpeedButtons;

  procedure EnableSpeedButton(SpeedButton: TSpeedButton; Enable: boolean);
    begin
    if SpeedButton.Enabled <> Enable then
      SpeedButton.Enabled := Enable;
    end;

  var
    DBaseNotEmpty   : boolean;
    DBaseNotReadOnly: boolean;
    FormDBaseOnTop  : boolean;
    FormFoundOnTop  : boolean;
    ActivePanel     : Integer;

  begin
  ///FormDBaseOnTop := ActiveMDIChild is TFormDBase;
  ///FormFoundOnTop := (ActiveMDIChild is TFormFoundFileList)
  ///               or (ActiveMDIChild is TFormFoundEmptyList);

  FormDBaseOnTop := QI_DatabaseIsOpened (DBaseHandle);//true;
  FormFoundOnTop := (PageControl1.TabIndex=1) and ((TabSheet2.FindChildControl('FormFoundFileList') <> nil) or
                    (TabSheet2.FindChildControl('FormFoundEmptyList') <> nil));
  ActivePanel := 0;
  if FormDBaseOnTop
    then
      begin
      DBaseNotEmpty := not DBaseIsEmpty;
      DBaseNotReadOnly := not DBaseIsReadOnly;
      ActivePanel   := ActivePanel;
      end
    else
      begin
      DBaseNotEmpty := false;
      DBaseNotReadOnly := false;
      end;

  EnableSpeedButton(SpeedButtonPrint, FormDBaseOnTop and DBaseNotEmpty or FormFoundOnTop);
  EnableSpeedButton(SpeedButtonScanA, FormDBaseOnTop and FloppyAExists and DBaseNotReadOnly);
  EnableSpeedButton(SpeedButtonScanDisk, FormDBaseOnTop and DBaseNotReadOnly);
  EnableSpeedButton(SpeedButtonDirDescr, FormDBaseOnTop and DBaseNotEmpty);
  EnableSpeedButton(SpeedButtonSearchFile, FormDBaseOnTop and DBaseNotEmpty);
  EnableSpeedButton(SpeedButtonSearchSelected, FormDBaseOnTop and DBaseNotEmpty);
  EnableSpeedButton(SpeedButtonSearchEmpty, FormDBaseOnTop and DBaseNotEmpty);
  EnableSpeedButton(SpeedButtonDiskInfo, FormDBaseOnTop and DBaseNotEmpty);
  ///EnableSpeedButton(SpeedButtonCollapse, FormDBaseOnTop and TFormDBase(ActiveMDIChild).QGlobalOptions.ShowTree);
  ///EnableSpeedButton(SpeedButtonExpand,   FormDBaseOnTop and TFormDBase(ActiveMDIChild).QGlobalOptions.ShowTree);
  EnableSpeedButton(SpeedButtonCollapse, FormDBaseOnTop and QGlobalOptions.ShowTree);
  EnableSpeedButton(SpeedButtonExpand,   FormDBaseOnTop and QGlobalOptions.ShowTree);
  EnableSpeedButton(SpeedButtonShowFound, GetFoundForm(foBoth) <> nil);
  EnableSpeedButton(SpeedButtonShowDBase, FormFoundOnTop);
  EnableSpeedButton(SpeedButtonSelect,
                    FormDBaseOnTop and ((ActivePanel = 1) or (ActivePanel = 3)));
  EnableSpeedButton(SpeedButtonUnSelect,
                    FormDBaseOnTop and ((ActivePanel = 1) or (ActivePanel = 3))
                    or FormFoundOnTop);
  EnableSpeedButton(SpeedButtonParent, FormDBaseOnTop);

  // next lines are because of when enabling buttons, it must be pressed
  // to be down
  if FormDBaseOnTop then
    begin
    if not SpeedButtonShowTree.Enabled then
      begin
      SpeedButtonShowTree.Enabled := true;
      SpeedButtonShowTree.Down := false;
      SpeedButtonShowTree.Down := QGlobalOptions.ShowTree;
      end;
    if not SpeedButtonBrief.Enabled then
      begin
      SpeedButtonBrief.Enabled := true;
      SpeedButtonDetailed.Enabled := true;  // these crazy lines are necessary...
      SpeedButtonDetailed.Down := true;
      SpeedButtonBrief.Down := true;
      if QGlobalOptions.FileDisplayType = fdBrief
        then SpeedButtonBrief.Down := true
        else SpeedButtonDetailed.Down := true;
      end;
    if QGlobalOptions.ShowTree <> SpeedButtonShowTree.Down then
      SpeedButtonShowTree.Down := QGlobalOptions.ShowTree;
    if (QGlobalOptions.FileDisplayType = fdBrief) and not SpeedButtonBrief.Down
      then SpeedButtonBrief.Down := true;
    if (QGlobalOptions.FileDisplayType = fdDetailed) and not SpeedButtonDetailed.Down
      then SpeedButtonDetailed.Down := true;
    end
  else
    begin
    EnableSpeedButton(SpeedButtonShowTree, false);
    EnableSpeedButton(SpeedButtonBrief,    false);
    EnableSpeedButton(SpeedButtonDetailed, false);
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - called when the user click on the main menu bar - we can update the
// menu status here

procedure TMainForm.MenuBarClick(Sender: TObject);

  var
    FormDBaseOnTop  : boolean;
    DBaseNotReadOnly: boolean;
    FormFoundOnTop  : boolean;
    DBaseNotEmpty   : boolean;
    ActivePanel     : Integer;

  begin
  ActivePanel := 0;
  ///FormDBaseOnTop := ActiveMDIChild is TFormDBase;
  FormDBaseOnTop := QI_DatabaseIsOpened (DBaseHandle);
  ///FormFoundOnTop := (ActiveMDIChild is TFormFoundFileList)
  ///                  or (ActiveMDIChild is TFormFoundEmptyList);
  FormFoundOnTop := (PageControl1.TabIndex=1) and ((TabSheet2.FindChildControl('FormFoundFileList') <> nil) or
                    (TabSheet2.FindChildControl('FormFoundEmptyList') <> nil));
  //FormFoundOnTop := false;
  if FormDBaseOnTop
    then
      begin
      ///DBaseNotEmpty := not TFormDBase(ActiveMDIChild).DBaseIsEmpty;
      DBaseNotEmpty := not DBaseIsEmpty;
      ///DBaseNotReadOnly := not TFormDBase(ActiveMDIChild).DBaseIsReadOnly;
      DBaseNotReadOnly := not DBaseIsReadOnly;
      ///ActivePanel   := TFormDBase(ActiveMDIChild).ActivePanel;
      ActivePanel   := MainForm.ActivePanel;
      end
    else
      begin
      DBaseNotEmpty := false;
      DBaseNotReadOnly := false;
      end;

  MenuCloseDBase.Enabled  := FormDBaseOnTop;
  MenuScanDisk.Enabled    := FormDBaseOnTop and DBaseNotReadOnly;
  MenuRescanDisk.Enabled  := FormDBaseOnTop and DBaseNotReadOnly and DBaseNotEmpty;
  MenuScanFolder.Enabled  := FormDBaseOnTop and DBaseNotReadOnly;
  MenuScanA.Enabled       := FormDBaseOnTop and FloppyAExists and DBaseNotReadOnly;
  MenuScanB.Enabled       := FormDBaseOnTop and FloppyBExists and DBaseNotReadOnly;
  MenuDirDescr.Enabled    := FormDBaseOnTop and DBaseNotEmpty;
  MenuDelRecord.Enabled   := FormDBaseOnTop and DBaseNotEmpty and DBaseNotReadOnly;
  MenuUndelRecord.Enabled := FormDBaseOnTop and DBaseNotEmpty and DBaseNotReadOnly
                             and CanUndeleteRecord;
  MenuChgLabel.Enabled    := FormDBaseOnTop and DBaseNotEmpty and DBaseNotReadOnly;

  MenuSearchName.Enabled     := FormDBaseOnTop and DBaseNotEmpty;
  MenuSearchEmpty.Enabled    := FormDBaseOnTop and DBaseNotEmpty;
  MenuSearchSelected.Enabled := FormDBaseOnTop and DBaseNotEmpty;
  MenuDiskInfo.Enabled       := FormDBaseOnTop and DBaseNotEmpty;
  MenuDBaseInfo.Enabled      := FormDBaseOnTop;

  MenuBrief.Enabled       := FormDBaseOnTop and DBaseNotEmpty;
  MenuDetailed.Enabled    := FormDBaseOnTop and DBaseNotEmpty;
  MenuSortName.Enabled    := FormDBaseOnTop and DBaseNotEmpty;
  MenuSortExt.Enabled     := FormDBaseOnTop and DBaseNotEmpty;
  MenuSortTime.Enabled    := FormDBaseOnTop and DBaseNotEmpty;
  MenuSortSize.Enabled    := FormDBaseOnTop and DBaseNotEmpty;
  MenuShowTree.Enabled    := FormDBaseOnTop and DBaseNotEmpty;
  MenuLocalOptions.Enabled     := FormDBaseOnTop;
  MenuExport.Enabled           := FormDBaseOnTop and DBaseNotReadOnly;
  MenuExportToOtherFormat.Enabled := (FormDBaseOnTop and DBaseNotReadOnly)
                                      or FormFoundOnTop;
  MenuImport.Enabled           := FormDBaseOnTop and DBaseNotReadOnly;
  {$ifdef CZECH}
  MenuImport4.Enabled          := FormDBaseOnTop and DBaseNotReadOnly;
  {$else}
  MenuImport4.Enabled          := false;
  {$endif}
  MenuMakeRuntime.Enabled      := FormDBaseOnTop and DBaseNotReadOnly;
  MenuMaskSelectDisks.Enabled  := FormDBaseOnTop and
                                  ((ActivePanel = 1) or (ActivePanel = 3));
  MenuDelDiskSelection.Enabled := FormDBaseOnTop and
                                  ((ActivePanel = 1) or (ActivePanel = 3))
                                  or FormFoundOnTop;
  MenuSelectAll.Enabled        := FormDBaseOnTop and
                                  ((ActivePanel = 1) or (ActivePanel = 3))
                                  or FormFoundOnTop;
  MenuCopy.Enabled             := FormDBaseOnTop and
                                  ((ActivePanel = 1) or (ActivePanel = 3))
                                  or FormFoundOnTop;

  MenuPrintDisks.Enabled       := FormDBaseOnTop and DBaseNotEmpty;
  MenuPrintTree.Enabled        := FormDBaseOnTop and DBaseNotEmpty;
  MenuPrintFiles.Enabled       := FormDBaseOnTop and DBaseNotEmpty;
  MenuPrintFoundFiles.Enabled  := ActiveMDIChild is TFormFoundFileList;
  MenuPrintEmpty.Enabled       := ActiveMDIChild is TFormFoundEmptyList;

  if FormDBaseOnTop then
    begin
    ///SetMenuFileDisplayType (TFormDBase(ActiveMDIChild).QGlobalOptions.FileDisplayType);
    ///SetMenuSortCrit (TFormDBase(ActiveMDIChild).QGlobalOptions.SortCrit);
    ///MenuShowTree.Checked := TFormDBase(ActiveMDIChild).QGlobalOptions.ShowTree;
    SetMenuFileDisplayType (MainForm.QGlobalOptions.FileDisplayType);
    SetMenuSortCrit (MainForm.QGlobalOptions.SortCrit);
    MenuShowTree.Checked := MainForm.QGlobalOptions.ShowTree;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Exit

procedure TMainForm.MenuExitClick(Sender: TObject);

  begin
  Close;
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - scan disk

procedure TMainForm.SpeedButtonScanDiskClick(Sender: TObject);

  var
    TmpS       : ShortString;
    i          : Integer;
    Drive      : char;

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  begin
    FormSelectDrive.ListBoxDrives.MultiSelect := true;
    if FormSelectDrive.ShowModal = mrOk then
      with FormSelectDrive.ListBoxDrives do
        for i := 0 to Items.Count-1 do
          if Selected[i] then
            begin
            TmpS := Items.Strings[i];
            if TmpS <> '' then
              begin
              Drive := TmpS[1];
              if not ScanDisk(UpCase(Drive),
                     g_CommonOptions.bNoQueries) then break;
              if g_CommonOptions.bEjectAfterCdScan and
                 ((UpCase(Drive) = m_EjectDriveLetter1) or (UpCase(Drive) = m_EjectDriveLetter2)) then
                Eject(UpCase(Drive));
              Application.ProcessMessages;
              end;
            end;
    ///end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - rescan disk

procedure TMainForm.MenuRescanDiskClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).RescanDisk;
  RescanDisk;
  end;

//-----------------------------------------------------------------------------
// Menu handler - scan folder as disk

procedure TMainForm.MenuScanFolderClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    begin
    if FormScanFolder.ShowModal = mrOk then
      begin
      TFormDBase(ActiveMDIChild).ScanFolder(FormScanFolder.Directory,
                                            FormScanFolder.DiskName,
                                            FormScanFolder.VolumeLabel, false);
      end;
    end;
  }
  if FormScanFolder.ShowModal = mrOk then
    ScanFolder(FormScanFolder.Directory,FormScanFolder.DiskName,FormScanFolder.VolumeLabel, false);
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - Scan A:

procedure TMainForm.SpeedButtonScanAClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).ScanDisk('A', g_CommonOptions.bNoQueries);
  ScanDisk('A', g_CommonOptions.bNoQueries);
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - Scan B:

procedure TMainForm.SpeedButtonScanBClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).ScanDisk('B', g_CommonOptions.bNoQueries);
  ScanDisk('B', g_CommonOptions.bNoQueries);
  end;

//-----------------------------------------------------------------------------
// Menu handler - delete disk in database

procedure TMainForm.MenuDelRecordClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).DeleteRecord;
  DeleteRecord;
  end;

//-----------------------------------------------------------------------------
// Menu handler - search in database by text

procedure TMainForm.MenuSearchNameClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SearchName;
  SearchName;
  end;

//-----------------------------------------------------------------------------
// Menu handler - search in database by disk size

procedure TMainForm.MenuSearchEmptyClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SearchEmpty;
  SearchEmpty;
  end;

//-----------------------------------------------------------------------------
// Menu handler - display disk information

procedure TMainForm.MenuDiskInfoClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).ShowDiskInfo;
  ShowDiskInfo;
  end;

//-----------------------------------------------------------------------------
// Menu handler - rename disk

procedure TMainForm.MenuChgLabelClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).ChangeDiskName;
  ChangeDiskName;
  end;

//-----------------------------------------------------------------------------
// Menu handler - undelete disk

procedure TMainForm.MenuUndelRecordClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).UndeleteRecord;
  UndeleteRecord;
  end;

//-----------------------------------------------------------------------------
// Menu handler - display/edit description

procedure TMainForm.SpeedButtonDirDescrClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).EditDescription;
  EditDescription;
  end;

//-----------------------------------------------------------------------------
// Menu handler - close database

procedure TMainForm.MenuCloseDBaseClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).Close;

     AddToLastFilesList(DBaseFileName);
     QI_CloseDatabase(DBaseHandle, false);
     DBaseFileName:='';
     FormIsClosed := true;

     Disks.DefaultColWidth:=Disks.ColWidths[0];
     Disks.Clear;
     Disks.RowCount:=1;
     DiskTree.Items.Clear;
     DrawGridFiles.RowCount:=1;

     if TabSheet2.FindChildControl('FormFoundFileList') <> nil then
         TForm(TabSheet2.FindChildControl('FormFoundFileList')).Close;
     if TabSheet2.FindChildControl('FormFoundEmptyList') <> nil then
         TForm(TabSheet2.FindChildControl('FormFoundEmptyList')).Close;


     HeaderTop.Sections[0].Text:='';
     HeaderTop.Sections[1].Text:='';
     HeaderTop.Sections[2].Text:='';
     StatusBar.Panels[0].Text:='';
     StatusBar.Panels[1].Text:='';
  end;

//-----------------------------------------------------------------------------
// Walks through MDI windows and looks for specified type of window with the same tag
// as the active window has.

function TMainForm.GetFoundForm(FormType: byte): TForm;

  var
    i, ActiveTag: Integer;

  begin
  Result := nil;

  //if (FormType and foFoundFile <> 0) then Result:=FormFoundFileList;
  if (FormType and foFoundEmpty <> 0) then Result:=TForm(TabSheet2.FindChildControl('FormFoundEmptyList'));
  if (FormType and foFoundFile <> 0)  then Result:=TForm(TabSheet2.FindChildControl('FormFoundFileList'));
  {
  if ActiveMDIChild is TFormDBase then
    begin
    ActiveTag := TForm(ActiveMDIChild).Tag;
    for i := 0 to MDIChildCount-1 do
      if (MDIChildren[i] is TFormFoundFileList) and
         (FormType and foFoundFile <> 0) or
         (MDIChildren[i] is TFormFoundEmptyList) and
         (FormType and foFoundEmpty <> 0) then
           if MDIChildren[i].Tag = ActiveTag then
             begin
             ///Result := MDIChildren[i];
             break;
             end;
    end;
    }
  end;

//-----------------------------------------------------------------------------
// Tollbar button handler - Show found files - finds the MDI window with found files

procedure TMainForm.SpeedButtonShowFoundClick(Sender: TObject);

  var
    Form: TForm;

  begin
  Form := GetFoundForm(foBoth);
  ///if Form <> nil then Form.BringToFront;
  if Form <> nil then PageControl1.TabIndex:=1;
  end;

//-----------------------------------------------------------------------------
// Tollbar button handler - Show database - finds the MDI window with the database

procedure TMainForm.SpeedButtonShowDBaseClick(Sender: TObject);

  var
    i, ActiveTag: Integer;

  begin
  {  ///
  if (ActiveMDIChild is TFormFoundFileList) or
     (ActiveMDIChild is TFormFoundEmptyList) then
    begin
    ActiveTag := Tag;
    for i := 0 to MDIChildCount-1 do
      if (MDIChildren[i] is TFormDBase) and
         (MDIChildren[i].Tag = ActiveTag) then
        begin
        MDIChildren[i].BringToFront;
        break;
        end;
    end;
    }
    PageControl1.TabIndex:=0;
  end;

//-----------------------------------------------------------------------------
// Check if the database of this name is open. If so can place it to the front

function TMainForm.IsDBaseOpened(DBaseFileName: ShortString; ToFront: boolean): boolean;

  var
    i: Integer;

  begin
  Result := false;
  if DBaseFileName = '' then exit;
  DBaseFileName := AnsiUpperCase(DBaseFileName);
  if AnsiUpperCase(MainForm.DBaseFileName) = DBaseFileName then
    Result := true;
  {
  for i := 0 to MDIChildCount-1 do
    if (MDIChildren[i] is TFormDBase) then
      if AnsiUpperCase(TFormDBase(MDIChildren[i]).DBaseFileName) = DBaseFileName then
        begin
        if ToFront then MDIChildren[i].BringToFront;
        Result := true;
        break;
        end;
  }
  end;

//-----------------------------------------------------------------------------
// Menu handler - Reindex

procedure TMainForm.MenuReindexClick(Sender: TObject);

  var
    CheckResult : integer;

  begin
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(LastOpenedFile);
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(FormSettings.EditAutoLoadDBase.Text);
  ReindexDialog.InitialDir := LastUsedDir;
  ReindexDialog.FileName := '*.qdr';
  if ReindexDialog.Execute then
    begin
    LastUsedDir := ExtractDir(ReindexDialog.FileName);
    if IsDBaseOpened (ReindexDialog.FileName, true) then
      begin
      Application.MessageBox(
        lsDBaseIsOpenCannotCompress,
        lsCannotCompress, mb_OK or mb_IconStop);
      exit;
      end;
    CheckResult := QI_CheckDatabase (ReindexDialog.FileName);
    if CheckResult = cdItIsRunTime then
      begin
      Application.MessageBox(
        lsCannotCompressAsItIsProtected,
        lsCannotCompress, mb_OK or mb_IconStop);
      exit;
      end;
    FormReindex.ProcessType := ptReindex;
    FormReindex.SrcDatabaseName := ReindexDialog.FileName;
    FormReindex.ShowModal;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Repair

procedure TMainForm.MenuRepairClick(Sender: TObject);

  var
    CheckResult : integer;

  begin
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(LastOpenedFile);
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(FormSettings.EditAutoLoadDBase.Text);
  OpenRepairDialog.InitialDir := LastUsedDir;
  OpenRepairDialog.FileName := '*.qdr';
  if OpenRepairDialog.Execute then
    begin
    LastUsedDir := ExtractDir(OpenRepairDialog.FileName);
    if IsDBaseOpened (OpenRepairDialog.FileName, true) then
      begin
      Application.MessageBox(
        lsDBaseIsOpenCannotRepair,
        lsCannotContinue, mb_OK or mb_IconStop);
      exit;
      end;
    // CheckDatabase catches all exceptions
    CheckResult := QI_CheckDatabase (OpenRepairDialog.FileName);
    case CheckResult of
      cdOldVersion:
        begin
        Application.MessageBox(
          lsCannotRepairByOldVersion,
          lsCannotContinue, mb_OK or mb_IconStop);
        exit;
        end;
      cdNotQDirDBase,
      cdHeaderCorrupted,
      cdCannotRead:
        begin
        Application.MessageBox(
          lsCannotRepairCorruptedHeader,
          lsCannotContinue, mb_OK or mb_IconStop);
        exit;
        end;
      cdItIsRuntime:
        begin
        Application.MessageBox(
          lsCannotRepairRuntimeDatabase,
          lsCannotContinue, mb_OK or mb_IconStop);
        exit;
        end;
      cdWriteProtected:
        begin
        {no problem}
        end;
      end;
    SaveRepairDialog.InitialDir := LastUsedDir;
    if SaveRepairDialog.Execute then
      begin
      FormRepair.SrcDatabaseName := OpenRepairDialog.FileName;
      FormRepair.TarDatabaseName := SaveRepairDialog.FileName;
      FormRepair.ShowModal;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Create runtime export

procedure TMainForm.MenuMakeRuntimeClick(Sender: TObject);

  begin
  ///if not (ActiveMDIChild is TFormDBase) then exit;
  FormReindex.SrcDBaseHandle := GetDBaseHandle;
  // it must be here before the dialog
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(LastOpenedFile);
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(FormSettings.EditAutoLoadDBase.Text);
  SaveDialogExport.InitialDir := LastUsedDir;
  SaveDialogExport.FileName := lsExportFile;
  if SaveDialogExport.Execute then
    begin
    LastUsedDir := ExtractDir(SaveDialogExport.FileName);
    if IsDBaseOpened (SaveDialogExport.FileName, false) then
      begin
      Application.MessageBox(
        lsDbaseIsOpenCannotOverwrite,
        lsCannotExport, mb_OK or mb_IconStop);
      exit;
      end;
    with FormReindex do
      begin
      ProcessType := ptRunTime;
      TarDatabaseName := SaveDialogExport.FileName;
      ShowModal;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Export

procedure TMainForm.MenuExportClick(Sender: TObject);

  begin
  ///if not (ActiveMDIChild is TFormDBase) then exit;
  FormReindex.SrcDBaseHandle := GetDBaseHandle;
  // it must be here before the dialog
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(LastOpenedFile);
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(FormSettings.EditAutoLoadDBase.Text);
  SaveDialogExport.InitialDir := LastUsedDir;
  SaveDialogExport.FileName := lsExportFile;
  if SaveDialogExport.Execute then
    begin
    LastUsedDir := ExtractDir(SaveDialogExport.FileName);
    if IsDBaseOpened (SaveDialogExport.FileName, false) then
      begin
      Application.MessageBox(
        lsDbaseIsOpenCannotOverwrite,
        lsCannotExport, mb_OK or mb_IconStop);
      exit;
      end;
    with FormReindex do
      begin
      ProcessType := ptExport;
      TarDatabaseName := SaveDialogExport.FileName;
      ShowModal;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Import

procedure TMainForm.MenuImportClick(Sender: TObject);

  var
    CheckResult : integer;

  begin
  ///if not (ActiveMDIChild is TFormDBase) then exit;
  FormReindex.TarDBaseHandle  := GetDBaseHandle;
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(LastOpenedFile);
  if LastUsedDir = '' then
    LastUsedDir := ExtractDir(FormSettings.EditAutoLoadDBase.Text);
  OpenDialogImport.InitialDir := LastUsedDir;
  OpenDialogImport.FileName := '*.qdr';
  if OpenDialogImport.Execute then
    begin
    LastUsedDir := ExtractDir(OpenDialogImport.FileName);
    if IsDBaseOpened (OpenDialogImport.FileName, false) then
      begin
      Application.MessageBox(lsDBaseIsOpenMustBeClosedFirst,
        lsCannotImport, mb_OK or mb_IconStop);
      exit;
      end;
    CheckResult := QI_CheckDatabase (OpenDialogImport.FileName);
    if CheckResult = cdItIsRunTime then
      begin
      Application.MessageBox(lsCannotImportAsItIsProtected,
        lsCannotImport, mb_OK or mb_IconStop);
      exit;
      end;
    if (CheckResult <> 0) and (CheckResult <> cdWriteProtected) then
      begin
      Application.MessageBox(lsCannotImportProbablyCorrupted,
        lsCannotImport, mb_OK or mb_IconStop);
      exit;
      end;
    with FormReindex do
      begin
      ProcessType := ptImport;
      SrcDatabaseName := OpenDialogImport.FileName;
      ShowModal;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Menu handler - import from QuickDir 4

procedure TMainForm.MenuImport4Click(Sender: TObject);

  begin
  {$ifdef CZECH}
  ///if not (ActiveMDIChild is TFormDBase) then exit;
  Application.MessageBox(lsAttentionMakeReindexFirst,
      lsImportFrom4, MB_OK);
  if OpenDialogImport4.Execute then
    ImportFromQDir41(OpenDialogImport4.FileName);
  {$endif}
  end;

//-----------------------------------------------------------------------------
// Menu handler - Mask select disks

procedure TMainForm.MenuMaskSelectDisksClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).DoSelection;
  DoSelection;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Select all disks

procedure TMainForm.MenuSelectAllClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    TFormDBase(ActiveMDIChild).SelectAll;
  if ActiveMDIChild is TFormFoundFileList then
    TFormFoundFileList(ActiveMDIChild).SelectAll;
  if ActiveMDIChild is TFormFoundEmptyList then
    TFormFoundEmptyList(ActiveMDIChild).SelectAll;
  }
  SelectAll;
  end;

//-----------------------------------------------------------------------------
// // Menu handler - Unselect disks

procedure TMainForm.MenuDelDiskSelectionClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    TFormDBase(ActiveMDIChild).UnselectAll;
  if ActiveMDIChild is TFormFoundFileList then
    TFormFoundFileList(ActiveMDIChild).UnselectAll;
  if ActiveMDIChild is TFormFoundEmptyList then
    TFormFoundEmptyList(ActiveMDIChild).UnselectAll;
  }
  UnselectAll;
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - Up one level in the folder tree

procedure TMainForm.SpeedButtonParentClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).GoToParent;
  GoToParent;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Find selected files

procedure TMainForm.MenuSearchSelectedClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).SearchSelected;
  SearchSelected;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Display database info

procedure TMainForm.MenuDBaseInfoClick(Sender: TObject);

  begin
    ShowDBaseInfo;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Copy to clipboard

procedure TMainForm.MenuCopyClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    TFormDBase(ActiveMDIChild).MakeCopy;
  if ActiveMDIChild is TFormFoundFileList then
    TFormFoundFileList(ActiveMDIChild).MakeCopy;
  if ActiveMDIChild is TFormFoundEmptyList then
    TFormFoundEmptyList(ActiveMDIChild).MakeCopy;
  }
  MakeCopy;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Export to text format

procedure TMainForm.MenuExportToOtherFormatClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    TFormDBase(ActiveMDIChild).ExportToOtherFormat;
  if ActiveMDIChild is TFormFoundFileList then
    TFormFoundFileList(ActiveMDIChild).ExportToOtherFormat;
  if ActiveMDIChild is TFormFoundEmptyList then
    TFormFoundEmptyList(ActiveMDIChild).ExportToOtherFormat;
  }
  ExportToOtherFormat;
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - Print

procedure TMainForm.SpeedButtonPrintClick(Sender: TObject);

  begin
  { ///
  if ActiveMDIChild is TFormDBase then
    ///TFormDBase(ActiveMDIChild).DoPrint(prSelectedPanel);
  if ActiveMDIChild is TFormFoundFileList then
    TFormFoundFileList(ActiveMDIChild).MakePrint;
  if ActiveMDIChild is TFormFoundEmptyList then
    TFormFoundEmptyList(ActiveMDIChild).MakePrint;
  }
  DoPrint(prSelectedPanel);
  end;


//-----------------------------------------------------------------------------
// Menu handler - Print tree

procedure TMainForm.MenuPrintTreeClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
    ///TFormDBase(ActiveMDIChild).DoPrint(prTree);
    DoPrint(prTree);
  end;

//-----------------------------------------------------------------------------
// Menu handler - Print files

procedure TMainForm.MenuPrintFilesClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
    ///TFormDBase(ActiveMDIChild).DoPrint(prFiles);
    DoPrint(prFiles);
  end;

//-----------------------------------------------------------------------------
// Menu handler - Print found files

procedure TMainForm.MenuPrintFoundFilesClick(Sender: TObject);

  begin
  if ActiveMDIChild is TFormFoundFileList then
    TFormFoundFileList(ActiveMDIChild).MakePrint;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Print found disks

procedure TMainForm.MenuPrintEmptyClick(Sender: TObject);

  begin
  if ActiveMDIChild is TFormFoundEmptyList then
    TFormFoundEmptyList(ActiveMDIChild).MakePrint;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Display About dialog

procedure TMainForm.MenuAboutClick(Sender: TObject);

  begin
  FormAbout.ShowModal;
  end;

//-----------------------------------------------------------------------------
// Menu handler - Help

procedure TMainForm.MenuHelpContentClick(Sender: TObject);

  begin
  Application.HelpContext(0);
  end;

//-----------------------------------------------------------------------------
// Menu handler - Register

//-----------------------------------------------------------------------------
// Adds the last open database to the list of last 10 open files in teh File menu

procedure TMainForm.AddToLastFilesList(FileName: ShortString);

  var i: integer;

  begin
  // first delete the string from the list if it is there
  DeleteFromLastFilesList(FileName);
  // move all names down
  for i := 10 downto 2 do LastFiles[i] := LastFiles[i-1];
  LastFiles[1] := FileName;
  UpdateLastFilesList;
  end;

//-----------------------------------------------------------------------------
// Removes file name from the Last 10 Open list

procedure TMainForm.DeleteFromLastFilesList(FileName: ShortString);

  var i, j: integer;

  begin
  for i := 1 to 10 do
    if (AnsiUpperCase(LastFiles[i]) = AnsiUpperCase(FileName)) then
      begin
      for j := i to 9 do LastFiles[j] := LastFiles[j+1];
      LastFiles[10] := '';
      UpdateLastFilesList;
      exit;
      end;
  end;

//-----------------------------------------------------------------------------
// Updates the menu by the list of last 10 open files

procedure TMainForm.UpdateLastFilesList;

  var
    LastFilesToShow: integer;
    i: integer;

  begin
  MenuLastFile1.Caption  := LimitCharsInPath(LastFiles[1],50);
  MenuLastFile2.Caption  := LimitCharsInPath(LastFiles[2],50);
  MenuLastFile3.Caption  := LimitCharsInPath(LastFiles[3],50);
  MenuLastFile4.Caption  := LimitCharsInPath(LastFiles[4],50);
  MenuLastFile5.Caption  := LimitCharsInPath(LastFiles[5],50);
  MenuLastFile6.Caption  := LimitCharsInPath(LastFiles[6],50);
  MenuLastFile7.Caption  := LimitCharsInPath(LastFiles[7],50);
  MenuLastFile8.Caption  := LimitCharsInPath(LastFiles[8],50);
  MenuLastFile9.Caption  := LimitCharsInPath(LastFiles[9],50);
  MenuLastFile10.Caption := LimitCharsInPath(LastFiles[10],50);
  LastFilesToShow := 10;
  for i:=1 to 10 do
    if LastFiles[i] = '' then
      begin
      LastFilesToShow := i-1;
      break;
      end;
  if LastFilesToShow > MaxLastFilesToShow then LastFilesToShow := MaxLastFilesToShow;
  if (LastFilesToShow > 0) then MenuLastFileSepar.Visible := true else MenuLastFileSepar.Visible := false;
  if (LastFilesToShow > 0) then MenuLastFile1.Visible := true else MenuLastFile1.Visible := false;
  if (LastFilesToShow > 1) then MenuLastFile2.Visible := true else MenuLastFile2.Visible := false;
  if (LastFilesToShow > 2) then MenuLastFile3.Visible := true else MenuLastFile3.Visible := false;
  if (LastFilesToShow > 3) then MenuLastFile4.Visible := true else MenuLastFile4.Visible := false;
  if (LastFilesToShow > 4) then MenuLastFile5.Visible := true else MenuLastFile5.Visible := false;
  if (LastFilesToShow > 5) then MenuLastFile6.Visible := true else MenuLastFile6.Visible := false;
  if (LastFilesToShow > 6) then MenuLastFile7.Visible := true else MenuLastFile7.Visible := false;
  if (LastFilesToShow > 7) then MenuLastFile8.Visible := true else MenuLastFile8.Visible := false;
  if (LastFilesToShow > 8) then MenuLastFile9.Visible := true else MenuLastFile9.Visible := false;
  if (LastFilesToShow > 9) then MenuLastFile10.Visible := true else MenuLastFile10.Visible := false;
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile1Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[1]);
  LastOpenedFile := LastFiles[1];
  DeleteFromLastFilesList(LastFiles[1]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile2Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[2]);
  LastOpenedFile := LastFiles[2];
  DeleteFromLastFilesList(LastFiles[2]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile3Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[3]);
  LastOpenedFile := LastFiles[3];
  DeleteFromLastFilesList(LastFiles[3]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile4Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[4]);
  LastOpenedFile := LastFiles[4];
  DeleteFromLastFilesList(LastFiles[4]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile5Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[5]);
  LastOpenedFile := LastFiles[5];
  DeleteFromLastFilesList(LastFiles[5]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile6Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[6]);
  LastOpenedFile := LastFiles[6];
  DeleteFromLastFilesList(LastFiles[6]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile7Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[7]);
  LastOpenedFile := LastFiles[7];
  DeleteFromLastFilesList(LastFiles[7]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile8Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[8]);
  LastOpenedFile := LastFiles[8];
  DeleteFromLastFilesList(LastFiles[8]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile9Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[9]);
  LastOpenedFile := LastFiles[9];
  DeleteFromLastFilesList(LastFiles[9]);
  end;

//-----------------------------------------------------------------------------
// Menu handler - clicked on the file name of recently open database

procedure TMainForm.MenuLastFile10Click(Sender: TObject);

  begin
  OpenDatabase(LastFiles[10]);
  LastOpenedFile := LastFiles[10];
  DeleteFromLastFilesList(LastFiles[10]);
  end;

//-----------------------------------------------------------------------------
// Loads the saved values from Settings.INI file and UserCmd.INI file

procedure TMainForm.LoadSettingsIni;

  var
    IniFile: TIniFile;
    sIniFileName : AnsiString;

  begin
  sIniFileName := ExtractFilePath(ParamStr(0)) + 'Settings.ini';
  if FileExists(sIniFileName) then
    begin
    IniFile := TIniFile.Create(sIniFileName);
    ReadLastFilesFromIni(IniFile);
    FormSearchFileDlg.LoadFromIni(IniFile);
    FormMoreOptions.LoadFromIni(IniFile);
    IniFile.Free;
    end;


  sIniFileName := ExtractFilePath(ParamStr(0)) + 'UserCmd.ini';
  if FileExists(sIniFileName) then
    begin
    IniFile := TIniFile.Create(sIniFileName);
    ReadUserCommandsFromIni(IniFile);
    IniFile.Free;
    end;
  CreateTempFolder();
  end;

//-----------------------------------------------------------------------------
// Saves settings to to Settings.INI

procedure TMainForm.SaveSettingsIni;

  var
    IniFile: TIniFile;
    sIniFileName : AnsiString;

  begin
  sIniFileName := ExtractFilePath(ParamStr(0)) + 'Settings.ini';
  if not FileExists(sIniFileName)
    then // test file creation
      begin
      if not TestFileCreation(sIniFileName) then exit;
      end
    else
      begin // check if it is read-only
      ///if ((GetFileAttributes(PChar(sIniFileName)) and FILE_ATTRIBUTE_READONLY) <> 0) then exit;
      end;

  IniFile := TIniFile.Create(sIniFileName);
  WriteLastFilesToIni(IniFile);
  FormSearchFileDlg.SaveToIni(IniFile);
  FormMoreOptions.SaveToIni(IniFile);
  IniFile.Free;
  end;

//-----------------------------------------------------------------------------
// Reads the list of last 10 open files from the INI file

procedure TMainForm.ReadLastFilesFromIni(var IniFile: TIniFile);

  begin
  LastOpenedFile := IniFile.ReadString('RecentFiles', 'LastOpened',  '');
  LastUsedDir    := IniFile.ReadString('RecentFiles', 'LastDir',  '');
  MaxLastFilesToShow := IniFile.ReadInteger('RecentFiles', 'ShowMax', MaxLastFilesToShow);
  LastFiles[1]  := IniFile.ReadString('RecentFiles', 'File1',  '');
  LastFiles[2]  := IniFile.ReadString('RecentFiles', 'File2',  '');
  LastFiles[3]  := IniFile.ReadString('RecentFiles', 'File3',  '');
  LastFiles[4]  := IniFile.ReadString('RecentFiles', 'File4',  '');
  LastFiles[5]  := IniFile.ReadString('RecentFiles', 'File5',  '');
  LastFiles[6]  := IniFile.ReadString('RecentFiles', 'File6',  '');
  LastFiles[7]  := IniFile.ReadString('RecentFiles', 'File7',  '');
  LastFiles[8]  := IniFile.ReadString('RecentFiles', 'File8',  '');
  LastFiles[9]  := IniFile.ReadString('RecentFiles', 'File9',  '');
  LastFiles[10] := IniFile.ReadString('RecentFiles', 'File10', '');
  UpdateLastFilesList;
  end;

//-----------------------------------------------------------------------------
// Writes the list of last 10 open files to the INI file

procedure TMainForm.WriteLastFilesToIni(var IniFile: TIniFile);

  begin
  if not IsDBaseOpened(LastOpenedFile, false) then LastOpenedFile := '';
  IniFile.WriteString('RecentFiles', 'LastOpened',  LastOpenedFile);
  IniFile.WriteString('RecentFiles', 'LastDir',  LastUsedDir);
  IniFile.WriteInteger('RecentFiles', 'ShowMax',  MaxLastFilesToShow);
  IniFile.WriteString('RecentFiles', 'File1',  LastFiles[1]);
  IniFile.WriteString('RecentFiles', 'File2',  LastFiles[2]);
  IniFile.WriteString('RecentFiles', 'File3',  LastFiles[3]);
  IniFile.WriteString('RecentFiles', 'File4',  LastFiles[4]);
  IniFile.WriteString('RecentFiles', 'File5',  LastFiles[5]);
  IniFile.WriteString('RecentFiles', 'File6',  LastFiles[6]);
  IniFile.WriteString('RecentFiles', 'File7',  LastFiles[7]);
  IniFile.WriteString('RecentFiles', 'File8',  LastFiles[8]);
  IniFile.WriteString('RecentFiles', 'File9',  LastFiles[9]);
  IniFile.WriteString('RecentFiles', 'File10', LastFiles[10]);
  end;

//-----------------------------------------------------------------------------
// Reads user commands from the INI file

procedure TMainForm.ReadUserCommandsFromIni(var IniFile: TIniFile);

  var
    iIndex      : integer;
    sSection    : String;
    sKey        : String;
    sDll        : AnsiString;
    sParams     : AnsiString;
    wKey        : Word;
    bShift      : boolean;
    bControl    : boolean;
    bAlt        : boolean;
    bTestExist  : boolean;
    pUserCommand: TPUserCommand;
    sTempFolder : AnsiString;

  begin
  g_UserCommandList.Clear();
  iIndex := 0;
  sTempFolder := IniFile.ReadString('settings', 'tempdir', '');
  if (sTempFolder <> '') then g_sTempFolder := sTempFolder;
  LOG('TempFolder = %s', [g_sTempFolder]);
  while true do
    begin
    inc (iIndex);
    sSection := Format('cmd%d', [iIndex]);
    sKey := IniFile.ReadString(sSection, 'key', '');
    if (sKey = '') then if (iIndex < 20) then continue else break;
    wKey := 0;
    sKey := UpperCase(sKey);
    if Length(sKey) = 1 then
      begin
      if (sKey[1] >= 'A') and (sKey[1] <= 'Z') or
         (sKey[1] >= '0') and (sKey[1] <= '0') then
         wKey := ord(sKey[1]);
      end
    else
      begin
      if sKey = 'F1'  then wKey := VK_F1 else
        if sKey = 'F2'  then wKey := VK_F2 else
          if sKey = 'F3'  then wKey := VK_F3 else
            if sKey = 'F4'  then wKey := VK_F4 else
              if sKey = 'F5'  then wKey := VK_F5 else
                if sKey = 'F6'  then wKey := VK_F6 else
                  if sKey = 'F7'  then wKey := VK_F7 else
                    if sKey = 'F8'  then wKey := VK_F8 else
                      if sKey = 'F9'  then wKey := VK_F9 else
                        if sKey = 'F10' then wKey := VK_F10 else
                          if sKey = 'F11' then wKey := VK_F11 else
                            if sKey = 'F12' then wKey := VK_F12;
      end;
    bShift    := IniFile.ReadBool  (sSection, 'shift', false);
    bControl  := IniFile.ReadBool  (sSection, 'control', false);
    bAlt      := IniFile.ReadBool  (sSection, 'alt', false);
    bTestExist:= IniFile.ReadBool  (sSection, 'test', false);
    sDll      := IniFile.ReadString(sSection, 'dll', '');
    sParams   := IniFile.ReadString(sSection, 'params', '');
    New(pUserCommand);
    pUserCommand^.m_wKey       := wKey;
    pUserCommand^.m_bShift     := bShift;
    pUserCommand^.m_bControl   := bControl;
    pUserCommand^.m_bAlt       := bAlt;
    pUserCommand^.m_sDll       := sDll;
    pUserCommand^.m_bTestExist := bTestExist;
    pUserCommand^.m_sParams    := sParams;
    LOG('UserCmd.ini: wKey=%d, bShift=%d, bControl=%d, bAlt=%d, sDll=%s, sParams=%s',
        [integer(wKey), integer(bShift), integer(bControl), integer(bAlt),
         PChar(sDll), PChar(sParams)]);
    if (wKey = 0) or (sDll = '') then
      begin
      LOG('Invalid key or DLL name', []);
      continue;
      end;
    g_UserCommandList.Add(pUserCommand);
    end;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TMainForm.DefaultHandler(var Message);

  begin
  with TMessage(Message) do
    if (Msg = g_CommonOptions.dwQueryCancelAutoPlay) and g_CommonOptions.bDisableCdAutorun
      //and (ClientHandle <> 0)
      then
        begin
        Result := 1;
        //SetWindowLong(ClientHandle, DWL_MSGRESULT, 1);
        end
      else
        inherited DefaultHandler(Message)
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - Eject CD 1

procedure TMainForm.SpeedButtonEject1Click(Sender: TObject);

  begin
  Eject(m_EjectDriveLetter1);
  end;

//-----------------------------------------------------------------------------
// Toolbar button handler - Eject CD 2

procedure TMainForm.SpeedButtonEject2Click(Sender: TObject);

  begin
  Eject(m_EjectDriveLetter2);
  end;

//-----------------------------------------------------------------------------
// The following messages are for WM_DEVICECHANGE. The immediate list
// is for the wParam. ALL THESE MESSAGES PASS A POINTER TO A STRUCT
// STARTING WITH A DWORD SIZE AND HAVING NO POINTER IN THE STRUCT.

const
  DBT_DEVICEARRIVAL             =  $8000;  // system detected a new device
  DBT_DEVICEQUERYREMOVE         =  $8001;  // wants to remove, may fail
  DBT_DEVICEQUERYREMOVEFAILED   =  $8002;  // removal aborted
  DBT_DEVICEREMOVEPENDING       =  $8003;  // about to remove, still avail.
  DBT_DEVICEREMOVECOMPLETE      =  $8004;  // device is gone
  DBT_DEVICETYPESPECIFIC        =  $8005;  // type specific event
  DBT_CUSTOMEVENT               =  $8006;  // user-defined event

  DBT_DEVTYP_OEM                =  $00000000;  // oem-defined device type
  DBT_DEVTYP_DEVNODE            =  $00000001;  // devnode number
  DBT_DEVTYP_VOLUME             =  $00000002;  // logical volume
  DBT_DEVTYP_PORT               =  $00000003;  // serial, parallel
  DBT_DEVTYP_NET                =  $00000004;  // network resource

  DBT_DEVTYP_DEVICEINTERFACE    =  $00000005;  // device interface class
  DBT_DEVTYP_HANDLE             =  $00000006;  // file system handle

  DBTF_MEDIA                    =  $0001;      // media comings and goings
  DBTF_NET                      =  $0002;      // network volume

type
  DEV_BROADCAST_VOLUME = record
    dbcv_size       : DWORD;
    dbcv_devicetype : DWORD;
    dbcv_reserved   : DWORD;
    dbcv_unitmask   : DWORD;
    dbcv_flags      : WORD;
    end;
  P_DEV_BROADCAST_VOLUME = ^DEV_BROADCAST_VOLUME;


{$Ifdef mswindows}
// here we handle a message about CD-ROM inserted to the drive
procedure TMainForm.OnDeviceChange(var Message: TMessage);

  var
    pDevBroadcastVolume: P_DEV_BROADCAST_VOLUME;
    dwDrive            : DWORD;
    ucDrive            : char;

  begin
  if (not g_CommonOptions.bScanAfterCdInsert) then exit; // we do not care...
  if not (ActiveMDIChild is TFormDBase) then exit;

  if (Message.WParam <> DBT_DEVICEARRIVAL) then exit;
  pDevBroadcastVolume := P_DEV_BROADCAST_VOLUME (Message.lParam);
  if (pDevBroadcastVolume^.dbcv_devicetype <> DBT_DEVTYP_VOLUME) then exit;
  dwDrive := pDevBroadcastVolume^.dbcv_unitmask;
  ucDrive := 'A';
  while ((dwDrive and 1) = 0) and (dwDrive > 0) do
    begin
    dwDrive := dwDrive shr 1;
    inc(ucDrive);
    end;
  // check if it is a CD-ROM. m_EjectDriveLetterX is in upper case
  if (ucDrive <> m_EjectDriveLetter1) and (ucDrive <> m_EjectDriveLetter2) then exit;
  if ((pDevBroadcastVolume^.dbcv_flags and DBTF_MEDIA) <> 0) and
     ((pDevBroadcastVolume^.dbcv_flags and DBTF_NET) = 0) then
    begin
    TFormDBase(ActiveMDIChild).ScanDisk(ucDrive, g_CommonOptions.bNoQueries);
    if (g_CommonOptions.bEjectAfterCdScan) then Eject(ucDrive);
    end;
  end;
{$endif}

//-----------------------------------------------------------------------------
// Creates temporary folder in Windows TEMP folder for unpacking

procedure TMainForm.CreateTempFolder();

  begin
  if (Length(g_sTempFolder) = 0) then exit;
  if (g_sTempFolder[Length(g_sTempFolder)] <> '\') then g_sTempFolder := g_sTempFolder + '\';
  g_sTempFolder := g_sTempFolder + 'DiskBaseTemp';
  CreateDir(g_sTempFolder);
  end;

//-----------------------------------------------------------------------------
// Deletes temporary folder

procedure TMainForm.DeleteTempFolder();

  var
    SearchRec: TSearchRec;
    bFound   : boolean;
    sFileName: AnsiString;

  begin
  if (Length(g_sTempFolder) = 0) then exit;
  bFound := FindFirst(g_sTempFolder+'\*', faAnyFile, SearchRec) = 0;
  while (bFound) do
    begin
    if ((SearchRec.Attr and faDirectory) = 0) then
      begin
      sFileName := g_sTempFolder+ '\' + SearchRec.Name;
      DeleteFile(PChar(sFileName));
      end;
    bFound := FindNext(SearchRec) = 0;
    end;
  SysUtils.FindClose(SearchRec);
  RemoveDir(g_sTempFolder);
  end;

//-----------------------------------------------------------------------------
// Saves window sizes

procedure TMainForm.SaveWinSizes;

  var
    IniFile: TIniFile;
    //i      : Integer;
    sIniFileName: AnsiString;

  begin
  sIniFileName := ChangeFileExt(ParamStr(0), '.ini');
  if not FileExists(sIniFileName)
    then // test file creation
      begin
      if not TestFileCreation(sIniFileName) then exit;
      end
    else
      begin // check if it is read-only
      ///if ((GetFileAttributes(PChar(sIniFileName)) and FILE_ATTRIBUTE_READONLY) <> 0) then exit;
      end;

  IniFile := TIniFile.Create(sIniFileName);

  if MainForm.WindowState = wsNormal
    then
      begin
      IniFile.WriteInteger ('Application', 'Left',   MainForm.Left);
      IniFile.WriteInteger ('Application', 'Top',    MainForm.Top);
      IniFile.WriteInteger ('Application', 'Width',  MainForm.Width);
      IniFile.WriteInteger ('Application', 'Height', MainForm.Height);
      IniFile.WriteBool    ('Application', 'Maximized', false);
      IniFile.WriteBool    ('Application', 'Minimized', false);
      end
    else
      if MainForm.WindowState = wsMaximized
        then
          begin
          IniFile.WriteBool ('Application', 'Maximized', true);
          IniFile.WriteBool ('Application', 'Minimized', false);
          end
        else
          begin
          IniFile.WriteBool ('Application', 'Maximized', false);
          IniFile.WriteBool ('Application', 'Minimized', true);
          end;
  if MainForm.ActiveMDIChild <> nil then
    begin
    IniFile.WriteBool('Application', 'MDIChildMaximized',
      MainForm.ActiveMDIChild.WindowState = wsMaximized);
    end;
  IniFile.WriteInteger  ('Application', 'MDIChildHeader0', g_PanelHeaderWidths[0]);
  IniFile.WriteInteger  ('Application', 'MDIChildHeader1', g_PanelHeaderWidths[1]);
  IniFile.WriteInteger  ('Application', 'MDIChildHeader2', g_PanelHeaderWidths[2]);

  IniFile.WriteInteger  ('ScanDiskWindow', 'Left',   FormSelectDrive.Left);
  IniFile.WriteInteger  ('ScanDiskWindow', 'Top',    FormSelectDrive.Top);
  IniFile.WriteInteger  ('ScanDiskWindow', 'Width',  FormSelectDrive.Width);
  IniFile.WriteInteger  ('ScanDiskWindow', 'Height', FormSelectDrive.Height);
  IniFile.WriteInteger  ('ScanDiskWindow', 'Position',
                          FormSelectDrive.ListBoxDrives.ItemIndex);

  IniFile.WriteInteger  ('DescriptionWindow', 'Left',   FormDescription.Left);
  IniFile.WriteInteger  ('DescriptionWindow', 'Top',    FormDescription.Top);
  IniFile.WriteInteger  ('DescriptionWindow', 'Width',  FormDescription.Width);
  IniFile.WriteInteger  ('DescriptionWindow', 'Height', FormDescription.Height);

  IniFile.WriteInteger  ('SelectFolderWindow', 'Left',  FormSelectFolder.Left);
  IniFile.WriteInteger  ('SelectFolderWindow', 'Top',   FormSelectFolder.Top);
  IniFile.WriteInteger  ('SelectFolderWindow', 'Width', FormSelectFolder.Width);
  IniFile.WriteInteger  ('SelectFolderWindow', 'Height',FormSelectFolder.Height);

  IniFile.Free;
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


//-----------------------------------------------------------------------------

procedure TMainForm.MenuDiscGearPrintClick(Sender: TObject);

  begin
  ///if ActiveMDIChild is TFormDBase then
  ///  TFormDBase(ActiveMDIChild).DiscGearPrint(Handle);
  ///DiscGearPrint(Handle);
  end;

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
if FormIsClosed then exit;
HeaderWidths[0] := Disks.Width + Splitter1.Width ;
HeaderWidths[1] := DiskTree.Width + Splitter1.Width  + Splitter2.Width ;
HeaderWidths[2] := DrawGridFiles.Width;
QGlobalOptions.PanelHeaderWidths := HeaderWidths;
g_PanelHeaderWidths := HeaderWidths;
ResizeHeaderBottom;
if Disks.Width > 50
  then Disks.ColWidths[0] := Disks.Width-2
  else Disks.ColWidths[0] := 50;
///HeaderTop.SectionWidth[0] := HeaderWidths[0];
HeaderTop.Sections[0].Width := HeaderWidths[0];
if not DiskTree.Visible
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

//-----------------------------------------------------------------------------



procedure TmainForm.UpdateSize;
begin
HeaderTop.Sections[0].Width := Disks.Width;
HeaderTop.Sections[1].Width := DiskTree.Width;
HeaderTop.Sections[2].Width := DrawGridFiles.Width;

end;


//--------------------------------------------------------------------
// Set fonts from global options and recalculates the sizes. Used after
// the global options are changed and at the program startup.

procedure TMainForm.ResetFontsAndRowHeights;
  begin
  with QGlobalOptions do
    begin
{$ifndef LOGFONT}
    Disks.Font.Assign       (DiskFont);
    Disks.Canvas.Font.Assign(DiskFont);
    DiskTree.Font.Assign         (TreeFont);
    DiskTree.Canvas.Font.Assign  (TreeFont);
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
    TimeWidth := DrawGridFiles.Canvas.TextWidth(DosTimeToStr(longint(23) shl 11,
                                            QGlobalOptions.ShowSeconds));
    TimeWidth := TimeWidth + TimeWidth div 6;

    end;
  end;



//--------------------------------------------------------------------
// Called when the form is created or display options are changed
// recalculates the properties of the Files panel

procedure TMainForm.ResetDrawGridFiles;
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
      ScrollBars := ssAutoHorizontal;
      end;
  if QGlobalOptions.FileDisplayType = fdDetailed then
    with DrawGridFiles do
      begin
      Options := [goFixedVertLine, goFixedHorzLine, goVertLine,
                  goDrawFocusSelected, goRowSelect, goColSizing];
      RowCount   := 2;
      FixedRows  := 1;
      ScrollBars := ssAutoBoth;
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
// Timer procedure called from the timer in the main application window
// - thi assures only one timer per application is used.

procedure TMainForm.LocalTimer;
  begin
  if PanelsLocked then exit;
  if QGlobalOptions.ShowFileHints and
    ((longint(GetTickCount) - LastMouseMoveTime) > defHintDelayPeriod) then
      ShowFileHint;
  end;


//--------------------------------------------------------------------
// Handles the event issued when the mouse moves over the file panel
// USed for displaying the bubbles with descriptions

procedure TMainForm.DrawGridFilesMouseMove(Sender: TObject;
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

procedure TMainForm.ShowFileHint;

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

function TMainForm.FindOneFileLine(Point: TPoint; var Rect: TRect): TOneFileLine;

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
// attaches a database to the MDI window

function TMainForm.AttachDBase(FName: ShortString): boolean;
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

procedure TMainForm.UpdateDiskWindow;

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
    TabSheet1.Caption:=ShortDBaseFileName;
    if CountToShow = 0 then
      begin
        Disks.Row := 0;
        Disks.RowCount := 1;
        Disks.Refresh;
        exit;
      end;
    if QI_GetCurrentKey (DbaseHandle, Key, Attr, KeyIndex) then
      begin
      if (Disks.Row >= CountToShow) then
        Disks.Row := pred(CountToShow);
      Disks.RowCount := CountToShow;
      if Disks.Row <> KeyIndex then
        if KeyIndex < Disks.RowCount
          //KeyIndex value can be bigger from the past - if deleted disks were displayed
          then Disks.Row := KeyIndex;
      Disks.Repaint;
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
// Updates the status line with the information about the currently selected file

procedure TMainForm.UpdateStatusLine(Index: Integer; SubTotalsToo: boolean);

  var
    OneFileLine: TOneFileLine;
    DosDateTime: longint;
    Description: TFilePointer;
    S          : ShortString;

  begin
  ///if FormIsClosed then exit;
  if not QI_DatabaseIsOpened (DBaseHandle) then exit;
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

procedure TMainForm.UpdateHeader;

  var
    S      : ShortString;
    Attr   : Word;
    Index  : Integer;

  begin
  //if FormIsClosed then exit;
  //if not IsDBaseOpened(LastOpenedFile, false) then exit;
  if not QI_DatabaseIsOpened (DBaseHandle) then exit;
  try
    (*
     if (ActiveControl is TDrawGrid) and
       (TDrawGrid(ActiveControl).Tag = 1) and
        (HeaderTop.Sections[0].Text <> ShortDBaseFileName) then
          begin
          HeaderTop.Sections[0].Text := ShortDBaseFileName;
          HeaderTop.Sections[0].Width := HeaderWidths[0];
          end;
    {$ifndef mswindows}
    if QGlobalOptions.ShowTree and (ActiveControl is TTreeView) and
      (TTreeView(ActiveControl).Tag = 2) then
        begin
        if QI_GetCurrentKey(DBaseHandle, S, Attr, Index) then
          begin
          if HeaderTop.Sections[1].Text <> S then
            begin
            HeaderTop.Sections[1].Text := S;
            HeaderTop.Sections[1].Width := HeaderWidths[1];
            end;
          end;
        end;
    if (ActiveControl is TDrawGrid) and
      (TDrawGrid(ActiveControl).Tag = 3) then
        begin
        QI_GetFullDirPath(DBaseHandle, S);
        if HeaderTop.Sections[2].Text <> S then
          begin
          HeaderTop.Sections[2].Text := S;
          HeaderTop.Sections[1].Width := HeaderWidths[2];
          end;
        end;
    {$endif}
    *)
    if (HeaderTop.Sections[0].Text <> ShortDBaseFileName) then
    begin
      HeaderTop.Sections[0].Text := ShortDBaseFileName;
      HeaderTop.Sections[0].Width := HeaderWidths[0];
    end;
    if QGlobalOptions.ShowTree then
    begin
        if QI_GetCurrentKey(DBaseHandle, S, Attr, Index) then
          begin
          if HeaderTop.Sections[1].Text <> S then
            begin
            HeaderTop.Sections[1].Text := S;
            HeaderTop.Sections[1].Width := HeaderWidths[1];
            end;
          end;
    end;
    if (ActiveControl is TDrawGrid) and
      (TDrawGrid(ActiveControl).Tag = 3) then
    begin
      QI_GetFullDirPath(DBaseHandle, S);
      if HeaderTop.Sections[2].Text <> S then
      begin
        HeaderTop.Sections[2].Text := S;
      end;
    end
    else
    begin
      HeaderTop.Sections[2].Text := '';
    end;
    HeaderTop.Sections[2].Width := HeaderWidths[2];


  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;


//--------------------------------------------------------------------
// Determines, whether the database is empty

function TMainForm.DBaseIsEmpty: boolean;
  begin
  Result := true;
  //if FormIsClosed then exit;
  Result := QI_GetCountToShow(DBaseHandle) = 0;
  end;

//--------------------------------------------------------------------
// Error message displayed when a critical error was encountered

procedure TMainForm.MsgShouldExit;
  begin
  Application.MessageBox(lsSeriousProblemWritingToDBase,
      lsError, mb_Ok or mb_IconStop);
  end;


//--------------------------------------------------------------------

procedure TMainForm.ScanTree(DirColHandle: PDirColHandle; Level: Integer;
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
    //Node:=nil;
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
      Node:=DiskTree.Items.AddChildObject(Node, OneFile.LongName + OneFile.Ext,
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
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;


//--------------------------------------------------------------------

procedure TMainForm.ScanTree(DirColHandle: PDirColHandle; Node: TTReeNode;
                              var TotalItemCount: Integer);
  var
    i               : Integer;
    OneFile         : TOneFile;
    OneDir          : TOneDir;
    Count           : Integer;
    SubDirColHandle : PDirColHandle;
    Node2           : TTReeNode;

  begin
  try
    Node2:=nil;
    Count := QI_GetDirColCount(DirColHandle); {returns 0, if it is nil}
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
      Node2:=DiskTree.Items.AddChildObject(Node, OneFile.LongName + OneFile.Ext,
        SubDirColHandle);
      Node2.ImageIndex:=3;
      Node2.SelectedIndex:=3;
      if QI_GetDirColCount(SubDirColHandle)>0 then
      begin
        //Node2:=Node;
        Node2.ImageIndex:=4;
        Node2.SelectedIndex:=1;
      end;
      TreePtrCol^.Insert(SubDirColHandle);
      inc(TotalItemCount);
      if SubDirColHandle <> nil then
        begin
          ScanTree(SubDirColHandle, Node2, TotalItemCount);
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
// Expandes one branch in the tree - used when some items in the tree
// has to be found and selected - typically when the user double-clicks
// in the window with fould files and the location of the file is to be
// displayed.

///procedure TFormDBase.ExpandBranch(AnItem: TOutlineNode);
procedure TMainForm.ExpandBranch(AnItem: TTreeNode);

  begin
  //if FormIsClosed then exit;
  ///if not AnItem.IsVisible then ExpandBranch(AnItem.Parent);
  if (AnItem<>nil) and not AnItem.IsVisible then ExpandBranch(AnItem.Parent);
  if AnItem <> nil then AnItem.Expand(false);
  end;


//--------------------------------------------------------------------
// Scans a disk to the database.

function TMainForm.ScanDisk (Drive: char; AutoRun: boolean): boolean;

  begin
  Result := true;
  //if FormIsClosed then exit;
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
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;



//--------------------------------------------------------------------
// Rescans current tree - does not change it - used when the panel
// with the tree is shown/hidden

procedure TMainForm.RescanTree(SavePosition: boolean);

  var
    DirsInTree: Integer;
    RootDirColHandle: PDirColHandle;
    SaveSelected: longint;
    Node: TTreeNode;

  begin
  //if FormIsClosed then exit;
  if PanelsLocked then exit;
  if DBaseIsEmpty then exit;
  try
    if not QGlobalOptions.ShowTree then exit;
    ///SaveSelected := OutlineTree.SelectedItem;
    DiskTree.Items.Clear;
    TreePtrCol^.DeleteAll;
    RootDirColHandle := QI_GetRootDirCol(DBaseHandle);
    ///OutLineTree.AddObject (0, '\', RootDirColHandle);
    Node:=DiskTree.Items.AddObject (nil, '\', RootDirColHandle);
    //Node.ImageIndex:=1;
    //Node.SelectedIndex:=1;
    TreePtrCol^.Insert(RootDirColHandle);
    DirsInTree := 1;

    try
      ///ScanTree (RootDirColHandle, 1, DirsInTree);
      ScanTree (RootDirColHandle, nil, DirsInTree);
    except
      ///on Error: EOutlineError do
      on Error: ETreeViewError do
        NormalErrorMessage(Error.Message);
      end;

    if QGlobalOptions.ExpandTree
      then
        with DiskTree do
          begin
             BeginUpdate;
             FullExpand;
             EndUpdate;
          end
      else
        ///DiskTree.Items[1].Expand(false);
    if SavePosition and (SaveSelected > 0) then
      begin
      ExpandBranch(DiskTree.Items[SaveSelected]);
      DiskTree.Selected.Index := SaveSelected;
      end;
  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;




  //--------------------------------------------------------------------
// Reloads a tree from the database. Called when the disk selection changes
// - call is made in the idle time, so that the user can browse disks fast

procedure TMainForm.ReloadTree;
  var
    DirsInTree: Integer;
    RootDirColHandle: PDirColHandle;
    Node: TTreeNode;

  begin
  if PanelsLocked then exit;
  try
    if QI_GetCountToShow(DBaseHandle) = 0 then
      begin
      ///OutLineTree.Clear;
      DiskTree.Items.Clear;
      TreePtrCol^.DeleteAll;
      ///OutLineTree.AddObject (0, '\', nil);
      DiskTree.Items.AddObject (nil, '\', nil);
      UpdateHeader;
      exit;
      end;
    QI_GoToKeyAt (DBaseHandle, Disks.Selection.Top);
    if not QGlobalOptions.ShowTree then exit;
    ///OutLineTree.Clear;
    DiskTree.Items.Clear;
    TreePtrCol^.DeleteAll;
    RootDirColHandle := QI_GetRootDirCol(DBaseHandle);
    ///OutLineTree.AddObject (0, '\', RootDirColHandle);
    Node:=DiskTree.Items.AddObject (nil, '\', RootDirColHandle);
    TreePtrCol^.Insert(RootDirColHandle);
    DirsInTree := 1;
    try
      ///ScanTree (RootDirColHandle, 1, DirsInTree);
      ScanTree (RootDirColHandle, nil, DirsInTree);
    except
      ///on Error: EOutlineError do
      on Error: ETreeViewError do
        NormalErrorMessage(Error.Message);
      end;
    if QGlobalOptions.ExpandTree
      then
        with DiskTree do
          begin
            BeginUpdate;
            FullExpand;
            EndUpdate;
          end
      else
        ///OutLineTree.Items[1].Expand;
        //DiskTree.Items[1].Expand(false);
    DiskTree.Select(DiskTree.Items.GetFirstNode);
    UpdateHeader;

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      begin
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Used to select programatically the folder in the tree panel - typically
// when the user double-clicks on a found file, the appropriate disk is
// selected, folder in the tree panel selcted (by this function) and the
// file in the file panel selected

procedure TMainForm.JumpInOutline(SubDirCol: pointer);

var
  i : Integer;
  Found: boolean;

  begin
  //if FormIsClosed then exit;
  if not QGlobalOptions.ShowTree then exit;
  Found := false;
  with DiskTree do
    ///if SelectedItem > 0 then
    if Visible and (Selected.Count > 0) then
      ///if Items[SelectedItem].Level > 1 then
      ///  Items[SelectedItem].Collapse;
      //if Items[Selected.Index].Level > 1 then
      if Items[Selected.AbsoluteIndex].HasChildren then
        //Items[Selected.Index].Collapse(true);
        Items[Selected.AbsoluteIndex].Collapse(true);

  ///for i := 0 to pred(TreePtrCol^.Count) do
  for i := 0 to TreePtrCol^.Count-1 do
    if TreePtrCol^.At(i) = SubDirCol then
      begin
      Found := true;
      break;
      end;
  if Found then
    begin
    ///inc(i);
    if i > 1 then
      ExpandBranch(DiskTree.Items[i]);
    ///OutlineTree.SelectedItem := i;
    if DiskTree.Visible then
      DiskTree.Select(DiskTree.Items[i]);
    end;
  end;


//--------------------------------------------------------------------
// called when the file collection is reloaded, or the type of display is changed,
// also called in idle time
// SelPos = - 1 -> do not change position

procedure TMainForm.UpdateFileWindowGrid (SelPos: Integer);
  var
    WidthInPixels: Integer;
    CalcRows     : Integer;
    CalcCols     : Integer;
    SpaceForRows : Integer;

  begin
  if FormIsClosed then exit;
  WidthInPixels := MaxFileNamePxLength + 5;
  if QGlobalOptions.ShowIcons then inc(WidthInPixels, BitmapFolder.Width);
  DrawGridFiles.BeginUpdate;
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
        //dec(SpaceForRows, ScrollBarHeight);
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
  DrawGridFiles.EndUpdate(true);
  end;



  //--------------------------------------------------------------------
// Reloads the collection of files from the database. Used when the
// the collection is to be re-sorted or updated

procedure TMainForm.ReloadFileCol(SaveFileName: ShortString);
  var
    FileColHandle: PFileColHandle;
    i, Count   : Integer;
    OneFile    : TOneFile;
    OneFileLine: TOneFileLine; {ukazatel}
    DirPos     : Integer;
    Jump       : boolean;
    TmpInt     : Integer;

  begin
  ///if FormIsClosed then exit;
  if not QI_DatabaseIsOpened (DBaseHandle) then exit;
  ///if PanelsLocked then exit;
  inc(DisableHints);
  try
    LastFileSelected := 0;
    CurFileSelected  := 0;
    if QI_GetCountToShow(DBaseHandle) = 0 then
      begin
      FreeObjects(FilesList);
      FilesList.Clear;
      FillChar(SubTotals, SizeOf(SubTotals), 0);
      UpdateFileWindowGrid(0);
      UpdateStatusLine(0, true);
      dec(DisableHints);
      exit;
      end;
    FreeObjects(FilesList);
    FilesList.Clear;
    ///FilesList.Reversed := QGlobalOptions.ReversedSort;
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
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  dec(DisableHints);
  end;


  //--------------------------------------------------------------------
// Jumps to the selected disk-folder-file

procedure TMainForm.JumpTo(Disk, Dir, FileName: ShortString);

var
  KeyIndex    : Integer;
  Key         : ShortString;
  Attr        : Word;

  begin
  //if FormIsClosed then exit;
  try
    QI_GoToKey(DBaseHandle, Disk);
    QI_ClearNeedUpdDiskWin(DBaseHandle);

    if QI_GetCurrentKey (DbaseHandle, Key, Attr, KeyIndex) then
      if Disks.Row <> KeyIndex then
        if KeyIndex < Disks.RowCount
          then Disks.Row := KeyIndex;

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
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;



//--------------------------------------------------------------------
// Search called when the user specifies it on the command line

procedure TMainForm.SearchParam(ParamFindFile: ShortString);

var
  FormFoundFileList: TFormFoundFileList;
  FoundTitle  : ShortString;
  SaveKey     : ShortString;
  SaveSelected: Integer;
  SaveAttr    : Word;
  SavePath    : ShortString;
  SaveFile    : ShortString;

  begin
  ///if FormIsClosed then exit;
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
      ///if QGlobalOptions.FoundToNewWin
      ///  then FormFoundFileList := nil
      ///  else FormFoundFileList := TFormFoundFileList(MainForm.GetFoundForm(foFoundFile));
      FormFoundFileList := nil;
      if FormFoundFileList = nil
        then
          begin
          FormFoundFileList := TFormFoundFileList.Create(Self);
          FormFoundFileList.Tag := Self.Tag;
          ///FormFoundFileList.DBaseWindow := Self;
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
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;



//--------------------------------------------------------------------
// Scans a folder to database as a disk

procedure TMainForm.ScanFolder(Directory, DiskName, VolumeLabel: ShortString;
                                NoOverWarning: boolean);

  begin
  //if FormIsClosed then exit;
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
      //FormIsClosed := true;
      Close;
      FatalErrorMessage(EFatal.Message);
      end;
    end;
  end;

//--------------------------------------------------------------------
// Does the actual scan of the disk, returns false when interrupted by the user

function TMainForm.DoScan (Drive: char;
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
  //if FormIsClosed then exit;
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
// Erases the bubble file hint

procedure TMainForm.EraseFileHint;

  begin
  if FilesHintDisplayed then
    begin
    FilesHintDisplayed := false;
    InflateRect(LastHintRect, 0, 6);
    InvalidateRect(DrawGridFiles.Handle, @LastHintRect, false);
    end;
  end;


//--------------------------------------------------------------------
// Returns the tag of the active panel

function TMainForm.ActivePanel: Integer;
begin
Result := 0;
if (ActiveControl is TDrawGrid)
  then Result := TDrawGrid(ActiveControl).Tag;
if (ActiveControl is TTreeView)
  then Result := TTreeView(ActiveControl).Tag;
end;



//--------------------------------------------------------------------
// Shows the window with the information about the disk

procedure TMainForm.ShowDiskInfo;

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

procedure TMainForm.ShowDBaseInfo;

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
// Called when global options are changed, so the view of the tree and files
// can be updated

procedure TMainForm.ChangeGlobalOptions;

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
    with DiskTree do
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

procedure TMainForm.ChangeLocalOptions;

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

procedure TMainForm.SetBriefFileDisplay(Brief: boolean);

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

procedure TMainForm.ShowOrHideTreePanel;

  begin
  if FormIsClosed then exit;
  try
    if QGlobalOptions.ShowTree and not DiskTree.Visible
      then
        begin
        RescanTree(false);
        if not DBaseIsEmpty then
          JumpInOutline(QI_GetCurDirCol(DBaseHandle));
        DiskTree.Left := Splitter1.Left + Splitter1.Width + 1;
        DiskTree.Show;
        // make sure it is on right
        Splitter2.Align := alNone;
        Splitter2.Left := DiskTree.Left + DiskTree.Width + 1;
        Splitter2.Align := alLeft;
        Splitter2.Show;
        QI_ClearNeedUpdFileWin(DBaseHandle); // already updated
        HeaderTop.Sections[1].Width := HeaderWidths[1];
        ResizeHeaderBottom;
        RescanTree(true);
        exit;
        end;
    if not QGlobalOptions.ShowTree and DiskTree.Visible
      then
        begin
        DiskTree.Hide;
        Splitter2.Hide;
        DiskTree.Items.Clear;
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
// Sets the flag for resorting in the idle time

procedure TMainForm.SetNeedResort(SortCrit: TSortCrit);

  begin
  NeedResort := true;
  if QGlobalOptions.SortCrit = SortCrit
    then QGlobalOptions.ReversedSort := not QGlobalOptions.ReversedSort
    else QGlobalOptions.ReversedSort := false;
  QGlobalOptions.SortCrit := SortCrit;
  end;


//--------------------------------------------------------------------
// Determines whether the current disk can be undeleted

function TMainForm.CanUndeleteRecord: boolean;

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

function TMainForm.GetDBaseHandle: PDBaseHandle;
begin

  Result := DBaseHandle;

end;


//--------------------------------------------------------------------
// Rescans existing disk in the database

procedure TMainForm.RescanDisk;

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
// Deletes a disk from the database

procedure TMainForm.DeleteRecord;

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
// Undeletes a disk

procedure TMainForm.UndeleteRecord;

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
// Searches for the folder/file/description by the text

procedure TMainForm.SearchName;

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
      ///if QGlobalOptions.FoundToNewWin
      ///  then FormFoundFileList := nil
      ///  else FormFoundFileList := TFormFoundFileList(MainForm.GetFoundForm(foFoundFile));
      if QGlobalOptions.FoundToNewWin
        then FormFoundFileList := nil
        else FormFoundFileList := TFormFoundFileList(TabSheet2.FindChildControl('FormFoundFileList'));
      //FormFoundFileList := nil;
      if FormFoundFileList = nil
        then
          begin
          FormFoundFileList := TFormFoundFileList.Create(Self);
          FormFoundFileList.Tag := Self.Tag;
          FormFoundFileList.DBaseWindow := Self;
          FormFoundFileList.Parent:=TabSheet2;
          //FormFoundFileList.WindowState := wsMaximized;
          FormFoundFileList.Align:=alClient;
          end
        else
          FormFoundFileList.BringToFront;
      FoundTitle := Caption + ': ' + FormSearchFileDlg.ComboBoxMask.Text;
      if length(FoundTitle) > 30 then
        FoundTitle := ShortCopy(FoundTitle, 1, 26) + ' ...';
      ///FormFoundFileList.Caption := FoundTitle + IntToStr(FormFoundFileList.DrawGrid.RowCount);
      FormFoundFileList.GetList(DBaseHandle);
      FormFoundFileList.Caption := FoundTitle + ' , '+IntToStr(FormFoundFileList.DrawGrid.RowCount-1);
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

procedure TMainForm.SearchEmpty;

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
      ///if {QGlobalOptions.FoundToNewWin} false // not necessary at all
      ///  then FormFoundEmptyList := nil
      ///  else FormFoundEmptyList := TFormFoundEmptyList(MainForm.GetFoundForm(foFoundEmpty));
      if {QGlobalOptions.FoundToNewWin} false // not necessary at all
        then FormFoundEmptyList := nil
        else FormFoundEmptyList := TFormFoundEmptyList(TabSheet2.FindChildControl('FormFoundEmptyList'));

      //FormFoundEmptyList := nil;
      if FormFoundEmptyList = nil
        then
          begin
          FormFoundEmptyList := TFormFoundEmptyList.Create(Self);
          FormFoundEmptyList.Tag := Self.Tag;
          FormFoundEmptyList.DBaseWindow := Self;
          FormFoundEmptyList.Parent:=TabSheet2;
          FormFoundEmptyList.Align:=alClient;

          end
        else
          FormFoundEmptyList.BringToFront;
      FoundTitle := Caption + lsListOfFreeSpace;
      ///FormFoundEmptyList.Caption := FoundTitle;
      FormFoundEmptyList.GetList(DBaseHandle);
      FormFoundEmptyList.Caption := FoundTitle + ' , '+IntToStr(FormFoundEmptyList.DrawGrid.RowCount-1);
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

procedure TMainForm.SearchSelected;

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
      ///if QGlobalOptions.FoundToNewWin
      ///  then FormFoundFileList := nil
      ///  else FormFoundFileList := TFormFoundFileList(MainForm.GetFoundForm(foFoundFile));
      if QGlobalOptions.FoundToNewWin
        then FormFoundFileList := nil
        else FormFoundFileList := TFormFoundFileList(TabSheet2.FindChildControl('FormFoundFileList'));
      //FormFoundFileList := nil;
      if FormFoundFileList = nil
        then
          begin
          FormFoundFileList := TFormFoundFileList.Create(Self);
          FormFoundFileList.Tag := Self.Tag;
          FormFoundFileList.DBaseWindow := Self;
          FormFoundFileList.Parent:=TabSheet2;
          FormFoundFileList.Align:=alClient;
          FormFoundFileList.BorderIcons:=[biSystemMenu];
          end
        else
          FormFoundFileList.BringToFront;
      FoundTitle := Caption + ': ' + FormSearchFileDlg.DlgData.Mask;
      if length(FoundTitle) > 30 then
        FoundTitle := ShortCopy(FoundTitle, 1, 26) + ' ...';
      ///FormFoundFileList.Caption := FoundTitle;
      FormFoundFileList.GetList(DBaseHandle);
      FormFoundFileList.Caption := FoundTitle + ' , '+IntToStr(FormFoundFileList.DrawGrid.RowCount-1);
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
// Changes the disk name in the database

procedure TMainForm.ChangeDiskName;

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
// Shows the window for editing the description

procedure TMainForm.EditDescription;

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
// Import from QuickDir 4.1 - predecessor of DiskBase

procedure TMainForm.ScanQDir4Database;

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
// Import from QuickDir 4.1 - predecessor of DiskBase

procedure TMainForm.ImportFromQDir41(FileName: ShortString);

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
// Jumps one level up in the tree to the parent of the current file

procedure TMainForm.GoToParent;
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
// called when the user selects Mask Select from the Main Form menu

procedure TMainForm.DoSelection;
begin
  if (ActiveControl is TDrawGrid) then
    begin
    if TDrawGrid(ActiveControl).Tag = 1 then SelectDisksOrFiles(true);
    if TDrawGrid(ActiveControl).Tag = 3 then SelectDisksOrFiles(false);
    end;

end;

//--------------------------------------------------------------------
// called when the user selects Select All from the Main Form menu

procedure TMainForm.SelectAll;
begin
if (ActiveControl is TDrawGrid) then
  begin
  if TDrawGrid(ActiveControl).Tag = 1 then SelectAllDisksOrFiles(true);
  if TDrawGrid(ActiveControl).Tag = 3 then SelectAllDisksOrFiles(false);
  end;

end;

//--------------------------------------------------------------------
// called when the user selects Unselect All from the Main Form menu

procedure TMainForm.UnselectAll;
begin
if (ActiveControl is TDrawGrid) then
  begin
  if TDrawGrid(ActiveControl).Tag = 1 then UnselectDisksOrFiles(true);
  if TDrawGrid(ActiveControl).Tag = 3 then UnselectDisksOrFiles(false);
  end;

end;

procedure TMainForm.MakeCopy;
begin
if (ActiveControl is TDrawGrid) then
  begin
  if TDrawGrid(ActiveControl).Tag = 1 then MakeCopyDisks;
  if TDrawGrid(ActiveControl).Tag = 3 then MakeCopyFiles;
  end;

end;

//--------------------------------------------------------------------
// Called from the Main From to print the selected panel

procedure TMainForm.DoPrint(PrintWhat: TPrintWhat);
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
// Exports to the text format

procedure TMainForm.ExportToOtherFormat;

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
// Calculates, which file is selected in the file panel and returns its name+ext

function TMainForm.GetSelectedFileName: ShortString;

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
// Updates the size of the sections of the bottom header (containing the status line)

procedure TMainForm.ResizeHeaderBottom;

  var
    SizeNeeded: Integer;

  begin
  SizeNeeded := HeaderTop.Sections[0].Width+HeaderTop.Sections[1].Width;
  if SizeNeeded < StatusSection0Size then SizeNeeded := StatusSection0Size;
  ///HeaderBottom.Sections[0].Width := SizeNeeded;
  StatusBar.Panels[0].Width := SizeNeeded;
  end;



//--------------------------------------------------------------------
// Displays the dislaog box for selecting disks/files by a mask

procedure TMainForm.SelectDisksOrFiles(Disk: boolean);

  var
    i, j   : Integer;
    MaskCol: TPQCollection;
    Key    : ShortString;
    Attr   : Word;
    Matching: boolean;

  begin
  if Disk
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
    if Disk
      then
        begin
        if FormMaskSelect.ComboBoxMaskDisks.Text = '' then
          FormMaskSelect.ComboBoxMaskDisks.Text := '*';
        TokenFindMask(FormMaskSelect.ComboBoxMaskDisks.Text, MaskCol, false, false, false);

        for i := 0 to pred(Disks.RowCount) do
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
        Disks.Repaint;
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
// Selects all disks or files

procedure TMainForm.SelectAllDisksOrFiles(Disk: boolean);

  var
    i: Integer;
    AlreadySelected: boolean;

  begin
  try
    if Disk
      then
        begin
        if UsePersistentBlocks
          then
            begin
            {first try if all files are already selected}
            AlreadySelected := true;
            for i := 0 to pred(Disks.RowCount) do
              if not QI_GetSelectionFlag(DBaseHandle, i) then
                begin
                AlreadySelected := false;
                break;
                end;
            for i := 0 to pred(Disks.RowCount) do
              QI_SetSelectionFlag(DBaseHandle, not AlreadySelected, i);
            end
          else
            begin
            for i := 0 to pred(Disks.RowCount) do
              QI_SetSelectionFlag(DBaseHandle, true, i);
            end;
        Disks.Repaint;
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
// Unselects all disks or files

procedure TMainForm.UnselectDisksOrFiles(Disk: boolean);

  var
    i: Integer;

  begin
  try
  if Disk
    then
      begin
      for i := 0 to pred(Disks.RowCount) do
        QI_SetSelectionFlag(DBaseHandle, false, i);
      Disks.Repaint;
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
// Prints the disk

procedure TMainForm.MakePrintDisk;

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
// Prints the tree

procedure TMainForm.MakePrintTree;

  var
    AmountPrintedPx: Integer;
    PageCounter  : Integer;
    ///TreeList     : TQStringList;
    TreeList     : TStringList;
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
// Prints files

procedure TMainForm.MakePrintFiles;

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
// Procedure used to inverse selection

procedure TMainForm.DrawGridFilesToggleSelection;
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
// Copies files to clipboard

procedure TMainForm.MakeCopyFiles;

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
// Fucntion called when there is no message in the message queue. Time-consuming
// updates are located here, so that the program can respond to users events
// quickly.

procedure TMainForm.LocalIdle;

  begin
  ///if FormIsClosed then exit;
  if not QI_DatabaseIsOpened (DBaseHandle) then exit;
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

    ///if HeaderBottom.Sections[0].Text <> StatusLineSubTotals then
    if StatusBar.Panels[0].Text <> StatusLineSubTotals then
      begin
      if StatusSection0Size = 0
        then
          begin //workaround - it made problems in the first run
          StatusSection0Size := 1;
          StatusBar.Panels[0].Text:= '-';
          end
        else
          begin
          ///HeaderBottom.Sections[0].Text := StatusLineSubTotals;
          ///StatusSection0Size := HeaderBottom.Sections[0].Width;
          StatusBar.Panels[0].Text := StatusLineSubTotals;
          StatusSection0Size := StatusBAr.Panels[0].Width;
          end;
      ResizeHeaderBottom;
      end;
    ///if HeaderBottom.Sections[1].Text <> StatusLineFiles then
    if StatusBar.Panels[1].Text <> StatusLineFiles then
      begin
      ///HeaderBottom.Sections[1].Text := StatusLineFiles;
      StatusBar.Panels[1].Text := StatusLineFiles;
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


end.
