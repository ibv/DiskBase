unit UApiTypes;
(*====================================================================
Definition of public types (public goes outside of API)
======================================================================*)

{$A-}

interface
uses
  {$ifdef mswindows}
  WinTypes
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Classes,
  UTypes, UCollections;

const
// key attrib ktxxx
  kaDeleted   = $01;
  kaSelected  = $02;

// extended attrib
  eaToBeSelected = $01;
  eaSelected     = $02;

// the Attr field by files
  faQArchive      = $0100; // till $40 used by DOS attributes
  faQDescModified = $0200;
  faQExtendedRec  = $8000;

  foFoundFile  = $01;
  foFoundEmpty = $02;
  foBoth       = $03;

// CheckDatabase
  cdNotQDirDBase       = $01;
  cdOldVersion         = $02;
  cdHeaderCorrupted    = $03;
  cdItIsRuntime        = $04;
  cdCannotRead         = $05;
  cdWriteProtected     = $06;

// DiskBase attrib, used in UserCommand
  daExists             = $0001;
  daDriveAccessible    = $0002;
  daDriveRemovable     = $0004;
  daDriveNetwork       = $0008;
  daVolumeLabelDiffers = $0010;
  daInArchive          = $0020;

type
  PDBaseHandle   = pointer;
  PDirColHandle  = pointer;
  PFileColHandle = pointer;

  TFilePointer = longword;

  TFileSystem = (FAT, HPFS, NTFS, EXT4, VFAT, CDROM, Unknown);

  // TArchiveType is used to pass the info after the archive was unpacked
  TArchiveType = (atZip, atArj, atLha, atRar, atOther, atAce);

  TLoadFileSelector = (fsAll, fsWithDescOnly, fsFilesOnly);

  // be careful, the order of TSortCrit items is significant
  TSortCrit = (scName, scExt, scTime, scSize, scUnsort); // program settings
  TFileDisplayType = (fdBrief, fdDetailed);              // program settings

  TWorkSwitch = (wsInc, wsDec, wsShow, wsHide, wsStop);

  TCallBackProc            = procedure (var Stop: boolean);
  TIsTheLastCPB            = function  (FName: ShortString): boolean;
  THowToContinue           = function  (FName: ShortString): word;
  TNewLineToIndicator      = procedure (Line: ShortString);
  TAppendLineToIndicator   = procedure (Line: ShortString);
  TUpdateProgressIndicator = procedure (Phase, Progress: Integer);
  TAbortScan               = function: boolean;
  TErrorMsg                = procedure (ErrorNo: Integer; ErrorStr: ShortString);

  PKeyRecord = ^TKeyRecord;
  TKeyRecord = record
    Attr    : Word;     // kaDeleted, kaSelected
    Position: TFilePointer;
    end;

const
  treeInfoHeadSize   = 21*4 + 32;
  SizeOfTreeInfoVer0 = 21*4 + 32 + 256;
  SizeOfTreeInfoVer1 = 21*4 + 32 + 4*256 + 5*8; // added 3 strings and 5 comps
  // offset to Name - is used only when directly reading the name

type
  TTreeInfo = record // when this changes, the treeInfoHeadSize must be adjusted!
    TreeAttr    : longint; // only for kaDeleted at the moment
    ScanDate    : longint;
    Dirs        : longint;
    Files       : longint;
    Archives    : longint;
    ArcDirs     : longint;
    ArcFiles    : longint;
    PhysSizeKb  : longint; // not used nepouzivano from 5.08
    ArchSizeKb  : longint; // not used nepouzivano from 5.08
    DataSizeKb  : longint; // not used nepouzivano from 5.08

    DiskSizeKb  : longword; {kB}
    DiskFreeKb  : longword; {kB}

    SectorsPerCluster     : longword;    // info from WIN32
    BytesPerSector        : longword;
    NumberOfFreeClusters  : longword;    // not used yet
    TotalNumberOfClusters : longword;    // not used yet
    FileSystemName        : string[31];  // used to align
    FileSystemFlags       : longword;    // not used yet

    Reserved1    : longint;
    Reserved2    : longint;
    OrigDrive    : array[1..4] of char;  // used for offer when copying
    Reserved3    : longint;
    Name         : ShortString;
    VolumeLabel  : ShortString; // new from 5.08
    OriginPath   : ShortString;
    ReservedStr  : ShortString;
    PhysSizeBytes: comp; // replaces original PhysSizeKb
    ArchSizeBytes: comp; // replaces original ArchSizeKb
    DataSizeBytes: comp; // replaces original DataSizeKb
    Reserved4    : comp;
    Reserved5    : comp;
    end;

//--- TOneDir -------------------------------------------------------


const
  oneDirHeadSizeVer0 = 23;
  oneDirHeadSize = 23+16; // added 2 longints and 1 comp

type
  TExt = string[4];

  TPOneDir = ^TOneDir;
  TOneDir = record
    SelfDirPos    : TFilePointer; // shows the offset of this record, is not saved
    Description   : TFilePointer;
    Time          : longint;
    Size          : longint;      // for archives, which behave like folders
    Attr          : word;
    Ext           : TExt;
    FileList      : TFilePointer;

    // added in 5.08
    FilesCount    : longint; // files in this folder
    FilesTotalSize: comp;    // total size of files in this folder
    Reserved1     : longint;

    LongName      : ShortString;
    end;

  TSubTotals = record // part of the tree node, but it is not saved
    DataDirs         : longint;
    DataFiles        : longint;
    DataFileSize     : comp;    // comp is 8-byte longint
    PhysDirs         : longint;
    PhysFiles        : longint;
    PhysFileSize     : comp;
    end;

  procedure GetMemOneDir (var POneDir: TPOneDir; var OneDir: TOneDir);
  procedure FreeMemOneDir(var POneDir: TPOneDir);
  procedure MoveOneDir(var Source, Target: TOneDir);

//--- TOneFile -------------------------------------------------------

const
  oneFileHeadSize = 19;
type
  TPOneFile = ^TOneFile;
  TOneFile  = record   // waring: do not change order of the items, because of reading
    SubDirCol   : pointer; // not stored
    SelfFilePos : TFilePointer; // points to the beginning of this list, not stored
    Description : TFilePointer;
    Time        : Longint;
    Size        : Longint;
    Attr        : word;
    Ext         : TExt;
    LongName    : ShortString;  // allocates only necessary space
    end;

  procedure GetMemOneFile (var POneFile: TPOneFile; var OneFile: TOneFile);
  procedure FreeMemOneFile(var POneFile: TPOneFile);
  procedure MoveOneFile(var Source, Target: TOneFile);

//--------------------------------------------------------------------

{$ifdef linux}
type
   TModuleHandle = Pointer;
const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);
{$endif}


type

  TFileType = (ftFile, ftDir, ftParent, ftArc);  // do not change the roder, because of case

  TRWMode      = (rwWhole, rwHead);
                 // rwHead - reads the whole header, but for the compatibility with
                 // older formats is written only till the Name - for deleting
                 // and renaming it is enough

  TSort       = (soName, soExt, soTime, soSize, soKey, soDir);
  TSortArr    = array[1..3] of TSort;

  TFFData   = record   // datat for file search
    Mask      : ShortString;
    WSort1    : word;
    WSort2    : word;
    WSort3    : word;
    WListType : word;
    end;

  TSearchIn = (siWholeDbase, siActualDisk, siSelectedDisks, siNotSelectedDisks, siActualDiskDir);

  TSearchDlgData = record
    Mask         : ShortString;
    AddWildCards : boolean;
    AsPhrase     : boolean;
    MaxLines     : longint;
    CaseSensitive: boolean;
    StrictMask   : boolean;
    SearchIn     : TSearchIn;
    ScanDiskNames: boolean;
    ScanFileNames: boolean;
    ScanDirNames : boolean;
    ScanDesc     : boolean;
    ScanDirLevel : integer; // -1 unlimited, 0 root, 1 - 1. subfolder, etc.
    UseMoreOptions: boolean;

    MoreOptions  : boolean; // for faster search
    ExcludeMask  : ShortString;
    DateFrom     : longint;
    DateTo       : longint;
    SizeFrom     : longint;
    SizeTo       : longint;
    DirMask      : ShortString;
    SortArr      : TSortArr;
    SearchAsAnsi : boolean;
    SearchAsOem  : boolean;
    end;

  TScan = (cfScan, cfAsk, cfNoScan);  // do not change order

  TAllMasksArray = array[0..2048] of char;

  PLocalOptions = ^TLocalOptions;
  TLocalOptions = record     // when changing, preserve size of this record
    ScanArchives         : TScan;
    ScanCPB              : TScan;
    ShowDeleted          : boolean;
    ScanDesc             : boolean;
    OverwriteWarning     : boolean;
    AllMasksArray        : TAllMasksArray;
    SimulateDiskInfo     : boolean;
    ScanZipExeArchives   : boolean;
    ScanOtherExeArchives : boolean;
    AlwaysRescanArchives : boolean;
    ExtractFromZips      : boolean;

    DisableVolNameChange : boolean; // added in 5.11
    GenerateFolderDesc   : boolean;
    ImportFilesBbs       : boolean;
    DiskCounter          : word;
    DiskNamePattern      : String[100]; // long 101 chars
    AlwaysCreateDesc     : boolean;
    ExtractFromRars      : boolean;
    ExtractFromAces      : boolean;
    ExtractSizeLimit     : longint;
    Reserved             : array[0..138] of char;
    end;

  TOpenTransferFunc =
    function (FileName: PChar; var Handle: longint): longint;
  TGetOneBlockFunc =
    function (Handle: longint; Buf: PChar; BufSize: longint;
                          var CharsRead: longint): longint;
  TCloseTransferFunc =
    function (Handle: longint): longint;



  PDll = ^TDll;
  TDll = object(TQObject)
    DllName          : ShortString;
    DllHandle        : THandle;
    OpenTransfer     : TOpenTransferFunc;
    GetOneBlock      : TGetOneBlockFunc;
    CloseTransfer    : TCloseTransferFunc;
    constructor Init(Name: ShortString);
    end;

{$ifdef DELPHI1}
  TExtractFileFunc =
    function (szArchiveFileName: PChar;
              szFileToExtract: PChar;
              szTargetFileName: PChar;
              iMaxSizeToExtract: longint;
              iArchiveOffset: longint): longint;
{$else}
  TExtractFileFunc =
    function (szArchiveFileName: PChar;
              szFileToExtract: PChar;
              szTargetFileName: PChar;
              iMaxSizeToExtract: longint;
              iArchiveOffset: longint): longint; stdcall;
{$endif}

  PExtractDll = ^TExtractDll;
  TExtractDll = object(TQObject)
    DllName          : ShortString;
    ArchiveType      : TArchiveType;
    DllHandle        : THandle;
    ExtractFile      : TExtractFileFunc;
    bStatic          : boolean;
    iSizeLimit       : longint;
    constructor Init(Name: ShortString; aArchiveType: TArchiveType; Static: boolean;
                     ExtractSizeLimitMb: longint);
    end;

  POneMask = ^TOneMask;
  TOneMask = object(TQObject)
    MaskName: TPQString;
    MaxSize : longint;
    ConvDll : PDll;
    destructor Done; virtual;
    end;

  TDBaseInfo = record
    DBaseSize  : longint;
    Count      : longint;
    CountValid : longint;
    ReadOnly   : boolean;
    iSelectedDisks   : integer;
    iNotSelectedDisks: integer;
    end;

  // used in export
  TSendType = (stNothing, stDataCallback, stDataNotify); // what to send from IterateFiles
  // DataCallBack - old version, used for print
  // DataNotify   - new version, used for export

  TDatabaseData = record
    sDatabaseName : AnsiString;
    sDatabasePath : AnsiString;
    iNumberOfDisks: longint;
    end;

  TDiskData = record
    sDiskName              : AnsiString;
    iDiskSizeKb            : longint;
    iDiskFreeKb            : longint;
    iFolders               : longint;
    iFiles                 : longint;
    iArchives              : longint;
    iTotalFilesInArchives  : longint;
    iTotalFoldersInArchives: longint;
    TotalDataSize          : comp;
    PhysSize               : comp;
    ArchSize               : comp;
    sOriginalPath          : AnsiString;
    sVolumelabel           : AnsiString;
    iScanDate              : longint;
    end;

  TFolderData = record
    sPathToFolder       : AnsiString;
    sFolderName         : AnsiString;
    FolderDataSize      : comp;
    iFilesInFolder      : longint;
    iFoldersInFolder    : longint;
    PhysFolderDataSize  : comp;
    iPhysFilesInFolder  : longint;
    iPhysFoldersInFolder: longint;
    end;

  TFileData = record
    OneFile     : TOneFile;
    end;

  TFormatSettings = record
    SortCrit         : TSortCrit;
    bShowInKb        : boolean;
    bShowSeconds     : boolean;
    bReversedSort    : boolean;
   end;

  var
    gDatabaseData  : TDatabaseData;
    gDiskData      : TDiskData;
    gFolderData    : TFolderData;
    gFileData      : TFileData;
    gFormatSettings: TFormatSettings;

type
  {callback}
  TSendOneDiskFunct        = procedure (Disk: ShortString);
  TSendOneDirFunct         = procedure (Dir: ShortString);
  TSendOneFileFunct        = procedure (var OneFile: TOneFile);
  TGetNameWidthFunct       = procedure (Name: ShortString; var Width: integer);
  TNotifyProc              = procedure;


  procedure FreeListObjects(List: TStringList);

implementation

uses SysUtils;




procedure FreeListObjects(List: TStringList);
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do
      List.Objects[i].Free;
end;


//=== OneDir routines ===============================================

procedure GetMemOneDir (var POneDir: TPOneDir; var OneDir: TOneDir);

  begin
  GetMem(POneDir, 4 + OneDirHeadSize + 1 + length(OneDir.LongName));
  MoveOneDir(OneDir, POneDir^);
  end;

//--------------------------------------------------------------------

procedure FreeMemOneDir(var POneDir: TPOneDir);
  begin
  if POneDir <> nil then
    FreeMem(POneDir, 4 + OneDirHeadSize + 1 + length(POneDir^.LongName));
  POneDir := nil;
  end;

//--------------------------------------------------------------------

procedure MoveOneDir(var Source, Target: TOneDir);

  begin
  Move(Source, Target, 4 + OneDirHeadSize + 1 + length(Source.LongName));
  end;

//=== OneFile routines ===============================================

procedure GetMemOneFile (var POneFile: TPOneFile; var OneFile: TOneFile);

  begin
  ///GetMem(POneFile, 8 + oneFileHeadSize + 1 + length(OneFile.LongName));
  GetMem(POneFile, 16 + oneFileHeadSize + 1 + length(OneFile.LongName));
  MoveOneFile(OneFile, POneFile^);
  end;

//--------------------------------------------------------------------

procedure FreeMemOneFile(var POneFile: TPOneFile);
  begin
  if POneFile <> nil then
    ///FreeMem(POneFile, 8 + oneFileHeadSize + 1 + length(POneFile^.LongName));
    FreeMem(POneFile, 16 + oneFileHeadSize + 1 + length(POneFile^.LongName));
  POneFile := nil;
  end;

//--------------------------------------------------------------------

procedure MoveOneFile(var Source, Target: TOneFile);

  begin
  ///Move(Source, Target, 8 + oneFileHeadSize + 1 + length(Source.LongName));
  Move(Source, Target, 16 + oneFileHeadSize + 1 + length(Source.LongName));
  end;

//--------------------------------------------------------------------

destructor TOneMask.Done;

  begin
  QDisposeStr(MaskName);
  inherited Done;
  end;

//--------------------------------------------------------------------

constructor TDll.Init(Name: ShortString);
  begin
  DllName          := Name;
  DllHandle        := 0;
  OpenTransfer     := nil;
  GetOneBlock      := nil;
  CloseTransfer    := nil;
  end;

//--------------------------------------------------------------------

constructor TExtractDll.Init(Name: ShortString; aArchiveType: TArchiveType; Static: boolean;
                             ExtractSizeLimitMb: longint);
  begin
  DllName          := Name;
  ArchiveType      := aArchiveType;
  DllHandle        := 0;
  ExtractFile      := nil;
  bStatic          := Static; // if true, the static function is used instead of DLL
  iSizeLimit       := ExtractSizeLimitMb;
  end;

//====================================================================

begin
end.
