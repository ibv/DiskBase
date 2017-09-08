unit UEngineMain;
(*====================================================================
Main DiskBase Engine functions
======================================================================*)

{$A-}


interface

uses UTypes, UCollections, UStream, UBaseUtils, UApiTypes, UBaseTypes,
     UEngineArchives, UEngineFileFind,
     LConvEncoding
     ;

type
  TRecordHeader = record    // at the beginning of each record in the database
    Size        : longword;
    SizeInvert  : longword; // inverted size - for checking purposes}
    RecType     : byte;     // rtTree, rtTreeDesc, rtFileList, rtDirDesc, rtFileDesc
    end;


  PFileCollection = ^TFileCollection;
  TFileCollection = object(TQCollection)
    constructor InitLoad (var F: TQBufStream; SeekPos: longword;
                          AllFiles: boolean; var ErrorCounter: integer);
    procedure   StoreCol (var F: TQBufStream; var TotalSize: longword;
                          WriteToo: boolean);
    procedure   FreeItem (Item: Pointer); virtual; // will not contain objects
    end;

  PBufDescCollection = ^TBufDescCollection;
  TBufDescCollection = object(TQCollection)
    constructor Init(ALimit, ADelta: Integer);
    function  FindDesc (ID: TFilePointer): pointer;
    procedure AddItem  (ID: TFilePointer; ShortDesc: ShortString);
    procedure FreeItem (Item: Pointer); virtual; // will not contain objects
  private
    NextToAdd: Integer;
    end;

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TQSortedCollection)
    ParentCol     : PDirCollection; // not saved
    Dta           : TPOneDir;
    SubTotals     : TSubTotals;     // not saved - calculated when the tree is loaded
    InArchive     : boolean;        // not saved - determined when the tree is loaded
    constructor Init (ALimit, ADelta: Integer; aName: ShortString; aExt: TExt; aTime: longint);
    constructor InitLoad (var F: TQBufStream; aParentCol: PDirCollection;
                          bInArchive: boolean;  var ErrorCounter: integer);
    destructor  Done; virtual;
    function    Compare  (Key1, Key2: Pointer): Integer; virtual;
    procedure   StoreCol (var F: TQBufStream; var TotalSize: longword;
                          WriteToo: boolean);
    function    GetFullDirPath: ShortString;
    function    GetArchivePath: ShortString;
  private
    procedure   AddParentPath (var Path: ShortString; bWithArchive: boolean);
    end;

  PTreeStruct  = ^TTreeStruct;
  PPTreeStruct = ^PTreeStruct;
  TTreeStruct  = object(TQObject)
    RootDirCol : PDirCollection;
    CurDirCol  : PDirCollection;
    CurFileCol : PFileCollection;
    DescMaskCol: TPQCollection;
    DllCol     : TPQCollection;
    ExtractDllCol: TPQCollection;
    OwnerDBase : PDBaseHandle;
    Dta        : TTreeInfo;
    constructor Init            (aOwnerDBase: PDBaseHandle; bSubstitute: boolean);
    constructor InitLoad        (aOwnerDBase: PDBaseHandle; RWMode: TRWMode);
    destructor  Done; virtual;
    function    StoreStruct     (RWMode: TRWMode): boolean;
    procedure   LoadDlls;
    procedure   FreeDlls;
    procedure   LoadExtractDlls;
    procedure   FreeExtractDlls;
    function    ScanDisk        (Path: ShortString; DiskName: ShortString;
                                 VolumeLabel: ShortString): boolean;
    function    ScanQDir4Record (Path: ShortString; DiskName: ShortString): boolean;
    procedure   UpdateCurFileCol(WhichFiles: TLoadFileSelector);
    function    RecursScanDirs  (Col, aParentCol: PDirCollection;
                                 Path: ShortString): boolean;
    function    RecursScanDirsQDir4 (Col, aParentCol: PDirCollection;
                                 Path: ShortString): boolean;
    function    GetDescription  (var OutF: TQBufStream; var Path: ShortString;
                                 Name: ShortString; PathIsTmpFile: boolean): longint;
    function    RecursScanFiles (Col: PDirCollection; var F: TQBufStream;
                                 Path: ShortString): boolean;
    function    CopyQDir4Desc   (var OutF: TQBufStream; PDesc: pointer): longint;
    function    RecursScanFilesQDir4 (Col: PDirCollection; var F: TQBufStream;
                                 Path: ShortString): boolean;
    procedure   UpdateDescInArchiveFromSaved (ArchiveDirCol: PDirCollection;
                                              Path: ShortString; var F: TQBufStream);
    procedure   RecurseUpdateDesc (SavedTreeStruct: PTreeStruct;
                                   SavedDirCol, NewDirCol: PDirCollection; var F: TQBufStream);
    function    GetDescFromArchive(var F: TQBufStream;
                                   ArchivePath: ShortString;
                                   InternalPath: ShortString;
                                   FileName: ShortString;
                                   ExtractDll: PExtractDll;
                                   OffsetInArchive: longint;
                                   iFileSize: longint): longint;
    function    ChOrMkDir       (BaseCol: PDirCollection; DirName: ShortString): PDirCollection;
    function    PutToDirAndFileCol (Col: PDirCollection; var F: TQBufStream;
                                    FileCol: PFileCollection; Index: Integer;
                                    ArcCol: PArcCollection; ArchiveType: TArchiveType;
                                    ArchivePath: ShortString): PDirCollection;
    function    ScanForArchive  (Col: PDirCollection; Path: ShortString;
                                 var F: TQBufStream; FileCol: PFileCollection;
                                 Index: Integer): PDirCollection;
    function    RecurseFindDir  (DirCol: PDirCollection; Path: ShortString): PDirCollection;
  private
    LastResponse: boolean;
    AlreadyAsked: boolean; // do not ask twice for the same archive
    ScanStatus  : TScan;
    DirsScanned : longint;
    CharsToCutFromPath: ShortInt;
    // used when rescanning the directory - must be separated from the path
    function    ScanArcPrompt (FName: ShortString) : boolean;
    end;


  PDatabase = ^TDatabase;
  TDatabase = object
    LocalOptions      : TLocalOptions;
    DatabaseOpened    : boolean;
    NeedUpdDiskWin    : boolean;
    NeedUpdTreeWin    : boolean;
    NeedUpdFileWin    : boolean;
    SecretData        : TSecretData;
    Header            : PHeader;
    KeyField          : PKeyField;
    TreeStruct        : PTreeStruct;
    SavedTreeStruct   : PTreeStruct;
    KeyFieldModified  : boolean;
    DataFile          : TQBufStream;
    DBaseName         : ShortString;
    FileSearchCol     : TPQCollection;
    MaskCol           : TPQCollection;
    ExcludeMaskCol    : TPQCollection;
    DirMaskCol        : TPQCollection;
    DisksCount        : longint;
    DirsCount         : longint;
    FilesCount        : longint;
    MaxNameLength     : Integer; // used in iteratefiles
    MaxnamePxWidth    : Integer;
    NameLengthSum     : longint; // for calculation of average length
    UpdateFunct       : TCallBackProc; // called during search
    SendOneDiskFunct  : TSendOneDiskFunct;
    SendOneDirFunct   : TSendOneDirFunct;
    SendOneFileFunct  : TSendOneFileFunct;
    GetNameWidthFunct : TGetNameWidthFunct;
    OnDatabaseBegin   : TNotifyProc;
    OnDatabaseEnd     : TNotifyProc;
    OnDiskBegin       : TNotifyProc;
    OnDiskEnd         : TNotifyProc;
    OnFolderBegin     : TNotifyProc;
    OnFolderEnd       : TNotifyProc;
    OnFile            : TNotifyProc;
    BufDescCol        : PBufDescCollection;
    SearchDlgData     : TSearchDlgData;
    StopProcess       : boolean;
    MarkAsReadOnly    : boolean;
    Expired           : integer;
    ErrorCounter      : integer;     // used when copying or repairing database
    bRepairMode       : boolean;     // set whe the databse is copied during repair
    bReadOnly         : boolean;     // the database is Run-time, or read-only
    TmpShortDesc      : ShortString; // used in RecurseIterateFile
    constructor Init;
    destructor  Done; virtual;
    procedure   InitLocalOptions;
    function    CreateDatabase (aDBaseName: ShortString): boolean;
    procedure   OpenDatabase   (aDBaseName: ShortString; bRepair: boolean);
    function    CloseDatabase  (WriteCompleteKeyArr: boolean): boolean;
    procedure   ResortKeys;
    procedure   SetHeader;
    function    KeyCompare   (Key1, Key2: ShortString; Attr1, Attr2: Word): Integer;
    function    KeyInsert    (Position: TFilePointer; Attr: Word): boolean;
    procedure   KeyDelete    (var Key: ShortString);
    function    KeySearch    (Key: ShortString; Attr: Word; var Index: Integer): boolean;
    function    KeyAtInsert  (Index: Integer; Position: TFilePointer; Attr: Word): boolean;
    procedure   KeyAtDelete  (Index: Integer);
    procedure   KeyAtResort  (Index: Integer);
    function    KeyStrSearch (StrKey: ShortString; var Index: Integer): Boolean;
    function    GetCountToShow:   Integer;
    function    GetSelectedCount: longint;
    function    GetFirstSelectedIndex: longint;
    function    ReadKeyStr   (Position: TFilePointer): ShortString;
    procedure   ReadKeyStrEx (Position: TFilePointer; var DiskName,
                              VolumeLabel, OriginPath: ShortString);
    procedure   ReadDBaseEntry (Position: TFilePointer; RWMode: TRWMode);
    procedure   ReadDBaseEntryToSaved (Position: TFilePointer);
    function    WriteToDBaseEntry (Position: TFilePointer; RWMode: TRWMode): boolean;
    function    AppendNewDBaseEntry(CheckDuplicates: boolean): boolean;
    procedure   FindFiles;
    procedure   RecurseFindFile (DirCol: PDirCollection; var Disk: ShortString; Level: integer);
    procedure   CheckDiskName (Disk: ShortString);
    procedure   IterateFiles (SearchIn: TSearchIn; eSendType: TSendType);
    procedure   RecurseIterateFile (DirCol: PDirCollection; var Disk: ShortString;
                                    eSendType: TSendType);
    procedure   FindEmpties;
    function    CopyDescription (var TarDataFile: TQBufStream;
                                 Position: TFilePointer): TFilePointer;
    function    CopyDatabase (TargetDatabase: PDatabase; WholeDbase,
                              CheckDuplicates, CopyLocalOptions: boolean): boolean;
    procedure   RecurseCopy (TargetDatabase: PDatabase; DirCol: PDirCollection);
    procedure   ClearUpdateFunct;
    procedure   ClearSendFunct;
    procedure   ClearNotifyFunct;

procedure DeleteFile (var fileName: ShortString);

    function    GetShortDesc    (ID: TFilePointer; Offset: longint; var ShortDesc: ShortString): boolean;
    function    LoadDescToBuf   (ID: TFilePointer; var Buf: PChar): boolean;
    function    SaveBufToDesc   (var OrigFilePointer: TFilePointer; Buf: PChar): boolean;
    procedure   UpdateDescInCurList (POneFile: TPOneFile);
    procedure   GetDBaseInfo    (var DBaseInfo: TDBaseInfo);

    procedure   RepairKeys;
    end;

const TmpFileNumber: word = 1; // normally is always 1, only in exception is incremented

function  CheckDatabase    (aDBaseName: ShortString): integer;



//=============================================================================

implementation

uses
     {$ifdef mswindows}
     WinTypes,WinProcs,
     {$ELSE}
       LCLIntf, LCLType, LMessages,
     {$ENDIF}
     SysUtils, UExceptions, UCollectionsExt, UCallbacks,
     UConvQDir4, ULang, UDebugLog,
     UUnRar, UUnAce;

//============================================================================
// Dummy callback functions

procedure DummyUpdateFunct (var StopProcess: boolean); far;

  begin
  end;


procedure DummySendOneDiskFunct (Disk: ShortString); far;

  begin
  end;


procedure DummySendOneDirFunct (Dir: ShortString); far;

  begin
  end;


procedure DummySendOneFileFunct (var OneFile: TOneFile); far;

  begin
  end;


procedure DummyGetNameWidthFunct (Name: ShortString; var Width: integer); far;

  begin
  end;


procedure DummyOnDatabaseBegin;

  begin
  end;


procedure DummyOnDatabaseEnd;

  begin
  end;


procedure DummyOnDiskBegin;

  begin
  end;


procedure DummyOnDiskEnd;

  begin
  end;


procedure DummyOnFolderBegin;

  begin
  end;


procedure DummyOnFolderEnd;

  begin
  end;


procedure DummyOnFile;

  begin
  end;

//-----------------------------------------------------------------------------
// Separates extension from the file name

procedure SeparateExt(var LongName: ShortString; var Ext: TExt);

  var
    DotPos, ExtLen, i: Integer;

  begin
  fillchar(Ext, SizeOf(Ext), 0);
  DotPos := 0;
  for i := length(LongName) downto MaxI(Integer(length(LongName))-3, 1) do
    if LongName[i] = '.' then
      begin
      DotPos := i;
      break;
      end;

  if DotPos > 0 then
    begin
    ExtLen := length(LongName) - DotPos + 1;
    Ext := ShortCopy(LongName, DotPos, ExtLen);
    dec(LongName[0], ExtLen);
    end;
  end;

//-----------------------------------------------------------------------------
// Fastly removes redundand spaces

procedure FastRemoveRedundSpaces(var St: ShortString);

  var
    i: Integer;

  begin
  for i := 1 to length(St) do
    if St[i] < ' ' then
      begin
      if St[i] = #13
        then St[i] := '/'
        else St[i] := ' ';
      end;
  i := pos('    ',St);
  while i > 0 do
    begin
    ShortDelete(St,i,3);
    i := pos('    ',St);
    end;
  i := pos('   ',St);
  while i > 0 do
    begin
    ShortDelete(St,i,2);
    i := pos('   ',St);
    end;
  i := pos('  ',St);
  while i > 0 do
    begin
    ShortDelete(St,i,1);
    i := pos('  ',St);
    end;
  if (length(St) > 0) and (St[1] = ' ') then ShortDelete(St,1,1);
  if (length(St) > 0) and (St[length(St)] = ' ') then
    SetLength(St, pred(length(St)));
  end;

//-----------------------------------------------------------------------------
// Checkes if the database can be openned, contains valid header and is read-only.
// Catches the exceptions, returns value.

function CheckDatabase (aDBaseName: ShortString): integer;

  var
    TmpStrArr : array[0..256] of char;
    DataFile  : TQBufStream;
    Header    : PHeader;

  begin
  Result := 0;
  Header := New(PHeader);
  try
    DataFile.Init(StrPCopy(TmpStrArr, aDBaseName), stOpenReadNonExclusive);
    DataFile.CheckReading := true;
    DataFile.Read(Header^, SizeOf(THeader));
    with Header^ do
      begin
      if QDirLabel <> 'QuickDir Database ' then
        begin
        Result := cdNotQDirDBase;
        Dispose(Header);
        DataFile.Done;
        exit;
        end;
      if VersionNeeded > DataFormatVersion then
        begin
        Result := cdOldVersion;
        Dispose(Header);
        DataFile.Done;
        exit;
        end;
      end;
  except
    on EQDirException do Result := cdCannotRead;
    end;

  Dispose(Header);
  DataFile.Done;

  // check if the database is on read-only medium:
  if Result = 0 then
    begin
    try
      DataFile.Init(StrPCopy(TmpStrArr, aDBaseName), stOpenExclusive);
      DataFile.Seek (0);
      DataFile.Read (TmpStrArr, 4);
      DataFile.Seek (0);
      DataFile.Write(TmpStrArr, 4);
      DataFile.Modified := false; // disable setting the Archive attribute
      DataFile.Done;
    except
      on EQDirException do Result := cdWriteProtected;
      end;
    end;

  end;

//=============================================================================
//=== TFileCollection =========================================================
//=============================================================================
// TFileCollection constructor, loads file collection form database.
// When DBaseStructException is thrown, catches it and increments ErrorCounter,
// so that it should not fail on DBaseStructException

constructor TFileCollection.InitLoad (var F: TQBufStream; SeekPos: longword;
                                      AllFiles: boolean;
                                      var ErrorCounter: integer);
  var
    RecordHeader: TRecordHeader;
    ItemCount   : Longint;
    POneFile    : TPOneFile;
    OneFile     : TOneFile;
    i           : Integer;

  begin
  try
    F.Seek(SeekPos);

    {read the header}
    F.Read (RecordHeader, SizeOf(TRecordHeader));

    {check the header for consistency}
    if (RecordHeader.Size <> not RecordHeader.SizeInvert) or
       (RecordHeader.RecType <> rtFileList) then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (003)');

    {now we are sure that we are reading the correct data, so continue}
    F.Read (ItemCount, SizeOf(ItemCount));

  except
    on EQDirDBaseStructException do
      begin
      inherited Init(10, 10); {make dummy initialization}
      inc(ErrorCounter);
      exit;
      end;
    end;

  // now we can initialize for the expected number of items
  inherited Init(ItemCount, 100);

  try
    for i := 1 to ItemCount do
      begin
      OneFile.SubDirCol := nil;
      OneFile.SelfFilePos := F.GetPos; // for the modifications of descriptions
        F.Read(OneFile.Description, succ(oneFileHeadSize)); // reads including the char 0 of the name
        F.Read(OneFile.LongName[1], length(OneFile.LongName));
      if AllFiles or (OneFile.Description <> 0) or
        (OneFile.Attr and faQDescModified <> 0) then
          begin
          GetMemOneFile(POneFile, OneFile);
          Insert(POneFile);
          end;
      end;
  except
    on EQDirDBaseStructException do
      begin
      inc(ErrorCounter);
      exit;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Stores the file collection to the database

procedure TFileCollection.StoreCol (var F: TQBufStream; var TotalSize: longword;
                                    WriteToo: boolean);

  var
    RecordHeader: TRecordHeader;
    ItemCount   : longint;
    i           : Integer;
    p           : Pointer;

  begin
  RecordHeader.Size := SizeOf(TRecordHeader) + SizeOf(ItemCount);
  for i := 0 to Count-1 do
    inc(RecordHeader.Size, oneFileHeadSize + 1 + length(TPOneFile(At(i))^.LongName));
  TotalSize := TotalSize + RecordHeader.Size;
  RecordHeader.SizeInvert := not RecordHeader.Size;
  RecordHeader.RecType    := rtFileList;
  if WriteToo then
    begin
    ItemCount := Count;
    F.Write (RecordHeader, SizeOf(RecordHeader)); // standard header of the record
    F.Write (ItemCount, SizeOf(ItemCount));
    for i := 0 to Count-1 do
      with TPOneFile(At(i))^ do
        F.Write (Description, oneFileHeadSize + 1 + length(LongName));
    end;
  end;

//-----------------------------------------------------------------------------
// Frees OneFile item form the collection

procedure TFileCollection.FreeItem(Item: Pointer);

  begin
  FreeMemOneFile(TPOneFile(Item));
  end;


//=============================================================================
//=== TBufDescCollection ======================================================
//=============================================================================
// TBufDescCollection caches recently used descriptions

constructor TBufDescCollection.Init(ALimit, ADelta: Integer);

  begin
  inherited Init(ALimit, ADelta);
  NextToAdd := 0;
  end;

//-----------------------------------------------------------------------------
// Finds if the description is in the cache

function TBufDescCollection.FindDesc (ID: TFilePointer): pointer;

  var
    i: Integer;

  begin
  FindDesc := nil;
  for i := 0 to Count - 1 do
    if PBufDescItem(At(i))^.ID = ID then
      begin
      FindDesc := At(i);
      exit;
      end;
  end;

//-----------------------------------------------------------------------------
// Adds description to the cache

procedure TBufDescCollection.AddItem (ID: TFilePointer; ShortDesc: ShortString);

 var
   BufDescItem: PBufDescItem;

  begin
  if Count < Limit
    then
      begin
      BufDescItem := New(PBufDescItem);
      Insert(BufDescItem);
      end
    else
      begin
      BufDescItem := At(NextToAdd);
      inc(NextToAdd);
      if NextToAdd >= Limit then NextToAdd := 0;
      end;

  BufDescItem^.ID := ID;
  if byte(ShortDesc[0]) > MaxShortDescSize then
    ShortDesc[0] := char(MaxShortDescSize);
  BufDescItem^.ShortDesc := ShortDesc;
  end;

//-----------------------------------------------------------------------------
// Free description from the cache

procedure TBufDescCollection.FreeItem (Item: Pointer);

  begin
  if Item <> nil then Dispose(PBufDescItem(Item));
  end;


//=============================================================================
//=== TDirCollection ==========================================================
//=============================================================================
// Constructor of directory collection

constructor TDirCollection.Init (ALimit, ADelta: Integer;
                                 aName: ShortString; aExt: TExt; aTime: longint);

  var
    OneDir: TOneDir;

  begin
  inherited Init(Alimit, ADelta);
  fillchar(OneDir, SizeOf(OneDir), 0);
  OneDir.LongName := aName;
  OneDir.Ext  := aExt;
  OneDir.Attr := faDirectory;
  GetMemOneDir(Dta, OneDir);
  fillchar(SubTotals, SizeOf(SubTotals), 0);
  InArchive := false;
  ParentCol := nil;
  end;

//-----------------------------------------------------------------------------
// Constructs directory collection and load it from file. Catches
// DBaseStructException, so it should not fail and should load as much as possibe
// in case of troubles.

constructor TDirCollection.InitLoad (var F: TQBufStream;
                                      aParentCol: PDirCollection;
                                      bInArchive: boolean;
                                      var ErrorCounter: integer);
  var
    ItemCount, ItemCountInvert : SmallInt;
    i      : Integer;
    OneDir : TOneDir;
    DataFormatVersion: Integer;
    NewDirCollection: PDirCollection;

  begin
  ParentCol := aParentCol;
  InArchive := bInArchive;
  FillChar(OneDir, SizeOf(OneDir), 0);
  OneDir.Attr := faDirectory;
  FillChar(SubTotals, SizeOf(SubTotals), 0);

  // check reading
  try
    F.Read(ItemCount, SizeOf(ItemCount));
    F.Read(ItemCountInvert, SizeOf(ItemCountInvert));

    // check format version
    DataFormatVersion := -1;
    if ItemCount = not ItemCountInvert then DataFormatVersion := 0;
    if ItemCount = (not ItemCountInvert + 1) then DataFormatVersion := 1;
    if DataFormatVersion < 0 then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (004)');

  except
    on EQDirDBaseStructException do
      begin
      Init(10, 10, '?', '', 0);
      inc(ErrorCounter);
      exit;
      end;
    end;

  // now we are sure we read the proper data, so continue
  inherited Init(ItemCount, 100); // inherited, so Dta must be also initialized
  try
    OneDir.SelfDirPos := F.GetPos; // for the need of modification of descriptions
    if DataFormatVersion = 0
      then
        begin
        F.Read (OneDir.Description, oneDirHeadSizeVer0); // old version
        F.Read (OneDir.LongName[0], 1);
        end
      else
        F.Read (OneDir.Description, oneDirHeadSize + 1);
    F.Read (OneDir.LongName[1], length(OneDir.LongName));
  except
    on EQDirDBaseStructException do
      begin
      GetMemOneDir(Dta, OneDir); // make Dta valid
      inc(ErrorCounter);
      exit;
      end;
    end;

  // data was read O.K. to OneDir, so now allocate Dta
  GetMemOneDir(Dta, OneDir);

  if Dta^.Attr and faQArchive <> 0 then
    begin
    // discount the size of the file with the archive
    if ParentCol <> nil then
      begin
      ParentCol^.SubTotals.DataFileSize := ParentCol^.SubTotals.DataFileSize - Dta^.Size;
      dec(ParentCol^.SubTotals.DataFiles);
      end;
    InArchive := true;
    end;

  if not InArchive then
    begin
    inc(SubTotals.PhysFiles, Dta^.FilesCount);
    SubTotals.PhysFileSize := SubTotals.PhysFileSize + Dta^.FilesTotalSize;
    end;
  inc(SubTotals.DataFiles, Dta^.FilesCount);
  SubTotals.DataFileSize := SubTotals.DataFileSize + Dta^.FilesTotalSize;

  for i := 1 to ItemCount do
    begin
    NewDirCollection := New(PDirCollection,
                            InitLoad(F, @Self, InArchive, ErrorCounter));
    Insert(NewDirCollection);
    // once an error happens in the structure, its bad
    // - the next collections cannot be read. ErrorCounter is zeroed just
    // for this one recursive call, so we are sure, that non-zero does not
    // indicate some old error
    if ErrorCounter <> 0 then Break;
    end;

  // calculating totals must be done after the recursion, when all the SubTotals are updated
  inc(SubTotals.DataDirs);
  if not InArchive then inc(SubTotals.PhysDirs);
  if ParentCol <> nil then
    begin
    inc(ParentCol^.SubTotals.DataDirs,     SubTotals.DataDirs);
    inc(ParentCol^.SubTotals.DataFiles,    SubTotals.DataFiles);
    ParentCol^.SubTotals.DataFileSize :=
      ParentCol^.SubTotals.DataFileSize + SubTotals.DataFileSize;
    if not InArchive then
      begin
      inc(ParentCol^.SubTotals.PhysDirs,     SubTotals.PhysDirs);
      inc(ParentCol^.SubTotals.PhysFiles,    SubTotals.PhysFiles);
      ParentCol^.SubTotals.PhysFileSize :=
        ParentCol^.SubTotals.PhysFileSize + SubTotals.PhysFileSize;
      end;
    end;
  dec(SubTotals.DataDirs);
  if not InArchive then dec(SubTotals.PhysDirs);
  end;

//-----------------------------------------------------------------------------
// Destructor

destructor TDirCollection.Done;

  begin
  FreeMemOneDir(Dta);
  inherited Done;
  end;

//-----------------------------------------------------------------------------
// Compares two dir names

function TDirCollection.Compare (Key1, Key2: Pointer): Integer;

  begin
  Result := AnsiCompareText(PDirCollection(Key1)^.Dta^.LongName +
                            PDirCollection(Key1)^.Dta^.Ext,
                            PDirCollection(Key2)^.Dta^.LongName +
                            PDirCollection(Key2)^.Dta^.Ext)
  end;

//-----------------------------------------------------------------------------
// Saves the collection to the database

procedure TDirCollection.StoreCol (var F: TQBufStream; var TotalSize: longword;
                                   WriteToo: boolean);

  var
    ItemCount  : SmallInt;
    i      : Integer;

  begin
  ItemCount := Count;
  if WriteToo then F.Write (ItemCount, SizeOf(ItemCount));
  inc(TotalSize, SizeOf(ItemCount));

  // check for reading - inverted count
  ItemCount := not ItemCount + 1; // from od verze 5.08

  if WriteToo then F.Write (ItemCount, SizeOf(ItemCount));
  inc(TotalSize, SizeOf(ItemCount));

  if WriteToo then
    begin
    F.Write (Dta^.Description, oneDirHeadSize + 1);
    F.Write (Dta^.LongName[1], length(Dta^.LongName));
    end;
  inc(TotalSize, oneDirHeadSize + 1 + length(Dta^.LongName));

  for i := 0 to pred(Count) do
    PDirCollection(At(i))^.StoreCol(F, TotalSize, WriteToo);
  end;

//-----------------------------------------------------------------------------
// returns the full path to this directory

function TDirCollection.GetFullDirPath: ShortString;

 var
   Path: ShortString;

  begin
  Path := '';
  AddParentPath(Path, true);
  if Path <> '\' then dec(Path[0]);
  Result := Path;
  end;

//-----------------------------------------------------------------------------
// Returns path to the archive which contains this directroy

function TDirCollection.GetArchivePath: ShortString;

 var
   Path: ShortString;

  begin
  Path := '';
  if not InArchive then exit;
  AddParentPath(Path, false);
  if Path <> '\' then dec(Path[0]);
  Result := Path;
  end;

//-----------------------------------------------------------------------------
// Recursive function used to get the full path

procedure TDirCollection.AddParentPath (var Path: ShortString; bWithArchive: boolean);

  var
    bThisIsArchiveFile: boolean;

  begin
  bThisIsArchiveFile := false;
  if ParentCol <> nil then bThisIsArchiveFile := not ParentCol^.InArchive and InArchive;
  if not InArchive or bThisIsArchiveFile or bWithArchive then
    begin
    Path := '\' + Path;
    if Dta^.LongName <> '\' then Path := Dta^.LongName + Dta^.Ext + Path;
    end;
  if ParentCol <> nil then ParentCol^.AddParentPath(Path, bWithArchive);
  end;


//=============================================================================
//=== TTreeStruct =============================================================
//=============================================================================
// This constructor is used only in case a new disk is scanned
// or a read problem happened - then the bSubstitute is set to true

 constructor TTreeStruct.Init (aOwnerDBase: PDBaseHandle; bSubstitute: boolean);

  begin
  inherited Init;
  RootDirCol    := nil;
  CurDirCol     := nil;
  CurFileCol    := nil;
  fillchar(Dta, SizeOf(Dta), 0);
  LastResponse  := false;
  AlreadyAsked  := false;
  ScanStatus    := cfScan;
  OwnerDBase    := aOwnerDBase;
  PDatabase(OwnerDBase)^.NeedUpdTreeWin := true;
  DescMaskCol   := New(TPQCollection, Init(50,100));
  DllCol        := New(TPQCollection, Init(50,100));
  ExtractDllCol := New(TPQCollection, Init(10,10));
  if bSubstitute then // make dummy initialization
    begin
    RootDirCol    := New(PDirCollection, Init(100, 100, '\', '', 0));
    CurDirCol     := RootDirCol;
    CurFileCol    := New(PFileCollection, Init(10, 10));
    end;
  end;

//-----------------------------------------------------------------------------
// Loads the tree from database, catches DBaseStructException - and increments
// ErrorCounter instead, so it should not fail because of DBaseStructException

constructor TTreeStruct.InitLoad (aOwnerDBase: PDBaseHandle; RWMode: TRWMode);

  var
    RecordHeader: TRecordHeader;
    DataFormatVersion: Integer;
    TreeErrorCounter: integer;

  begin
  inherited Init;
  RootDirCol  := nil;
  CurDirCol   := nil;
  CurFileCol  := nil;
  DescMaskCol := nil;
  DllCol      := nil;
  ExtractDllCol := nil;
  fillchar(Dta, SizeOf(Dta), 0);
  LastResponse := false;
  AlreadyAsked := false;
  ScanStatus   := cfScan;
  OwnerDBase   := aOwnerDBase;

  // now try to load it from file
  try
    PDatabase(OwnerDBase)^.DataFile.Read (RecordHeader, SizeOf(TRecordHeader));

    DataFormatVersion := -1;
    if RecordHeader.Size = not RecordHeader.SizeInvert then DataFormatVersion := 0;
    if RecordHeader.Size = (not RecordHeader.SizeInvert + 1) then DataFormatVersion := 1;
    if (RecordHeader.RecType <> rtTree) then DataFormatVersion := -1;
    if DataFormatVersion < 0 then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (005)');

    if DataFormatVersion = 0
      then
        begin
        PDatabase(OwnerDBase)^.DataFile.Read (Dta, SizeOfTreeInfoVer0);
        Dta.PhysSizeBytes := Dta.PhysSizeKb;
        Dta.ArchSizeBytes := Dta.ArchSizeKb;
        Dta.DataSizeBytes := Dta.DataSizeKb;
        Dta.PhysSizeBytes := Dta.PhysSizeBytes * 1024;
        Dta.ArchSizeBytes := Dta.ArchSizeBytes * 1024;
        Dta.DataSizeBytes := Dta.DataSizeBytes * 1024;
        end
      else
        PDatabase(OwnerDBase)^.DataFile.Read (Dta, SizeOfTreeInfoVer1);

    if RWMode = rwHead then exit; // only head is needed, do not initialize DescCol and others
    TreeErrorCounter := 0;
    RootDirCol := New(PDirCollection, InitLoad(PDatabase(OwnerDBase)^.DataFile,
                                               nil, false,
                                               TreeErrorCounter));
    inc(PDatabase(OwnerDBase)^.ErrorCounter, TreeErrorCounter);
    CurDirCol  := RootDirCol;
    if (CurDirCol <> nil) then UpdateCurFileCol(fsAll);
  except
    on EQDirDBaseStructException do
      begin
      inc(PDatabase(OwnerDBase)^.ErrorCounter);
      if CurDirCol = nil then
        begin // make sure the collections are not nil
        RootDirCol := New(PDirCollection, Init(100, 100, '\', '', 0));
        CurDirCol  := RootDirCol;
        CurFileCol := New(PFileCollection, Init(10, 10));
        end;
      end;
    end;
  PDatabase(OwnerDBase)^.NeedUpdTreeWin := true;
  DescMaskCol   := New(TPQCollection, Init(50,100));
  DllCol        := New(TPQCollection, Init(50,100));
  ExtractDllCol := New(TPQCollection, Init(10,10));
  end;

//-----------------------------------------------------------------------------
// Saves the tree to the database

function TTreeStruct.StoreStruct (RWMode: TRWMode): boolean;

  var
    RecordHeader: TRecordHeader;
    TmpLongWord  : longword;
    BytesPerSector, SectorsPerCluster, SizeKBytes, FreeKbytes: longword;

  begin
  StoreStruct := true;
  if RWMode = rwHead then // save the header only Name - for compatibility
    begin
    PDatabase(OwnerDBase)^.DataFile.Seek  (PDatabase(OwnerDBase)^.DataFile.GetPos + SizeOf(TRecordHeader));
    PDatabase(OwnerDBase)^.DataFile.Write (Dta, SizeOfTreeInfoVer0); // incomplete header written
    PDatabase(OwnerDBase)^.DataFile.Flush;
    exit;
    end;
  // warning - when this changes, the reindex functions must be changed as well

  RecordHeader.Size := SizeOf(TRecordHeader) +  SizeOfTreeInfoVer1;
  RootDirCol^.StoreCol (PDatabase(OwnerDBase)^.DataFile, RecordHeader.Size, false); {zjisteni velikosti zaznamu}

  {$ifdef mswindows}
  QGetDiskSizes(byte(UpCase(PDatabase(OwnerDBase)^.DBaseName[1]))-byte('A')+1,
    SizeKbytes, FreeKbytes, BytesPerSector, SectorsPerCluster);
  {$else}
  QGetDiskSizes(AddDisk(PDatabase(OwnerDBase)^.DBaseName),
    SizeKbytes, FreeKbytes, BytesPerSector, SectorsPerCluster);
  {$endif}

  //if ((RecordHeader.Size + RecordHeader.Size div 5 + 50*1024) div 1024) > FreeKbytes then
  //  begin
  //  raise EQDirFatalException.Create(lsNotEnoughSpaceOnDisk);
  //  exit;
  //  end;

  RecordHeader.SizeInvert := not RecordHeader.Size + 1; {DataFormatVersion 1}

  //  Write Header->Size
       RecordHeader.RecType    := rtTree;
       PDatabase(OwnerDBase)^.DataFile.Write (RecordHeader, SizeOf(RecordHeader)); // standard header

  //  Write Header->Verinfo
      PDatabase(OwnerDBase)^.DataFile.Write (Dta,  SizeOfTreeInfoVer1);

  //  Write Files
  TmpLongWord := RecordHeader.Size;   // just to be sure
  RootDirCol^.StoreCol (PDatabase(OwnerDBase)^.DataFile, TmpLongWord, true);

  PDatabase(OwnerDBase)^.DataFile.Flush;
  end;

//-----------------------------------------------------------------------------
// Loads the filter DLLs

procedure TTreeStruct.LoadDlls;

  var
    i: Integer;
    Dll: PDll;
    ProgramDir: ShortString;
    ZString   : array[0..256] of char;
    DLLExt    : string[4];

  begin
{$ifdef DELPHI1}
  DLLExt := '.q16';
{$else}
  DLLExt := '.q32';
{$endif}
{$ifdef mswindows}
  ProgramDir := GetProgramDir;
  for i := 0 to pred(DllCol^.Count) do
    begin
    Dll := PDll(DllCol^.At(i));
    with Dll^ do
      begin
      CB_NewLineToIndicator(lsLoadingLibrary + DLLName + DLLExt);
      DllHandle := LoadLibrary(StrPCopy(ZString, ProgramDir + DLLName + DLLExt));
      if DllHandle > 32
        then
          begin
          @OpenTransfer  := GetProcAddress(DllHandle, 'OpenTransfer');
          @GetOneBlock   := GetProcAddress(DllHandle, 'GetOneBlock');
          @CloseTransfer := GetProcAddress(DllHandle, 'CloseTransfer');
          if (@OpenTransfer = nil) or (@GetOneBlock = nil) or
             (@CloseTransfer = nil) then
            begin
            FreeLibrary(DllHandle);
            DllHandle := 0;
            raise EQDirNormalException.Create(lsLibrary + DllName
                        + DLLExt + lsDoesNotContainProperFunctions);
            end;
          end
        else
          begin
          DllHandle := 0;
          raise EQDirNormalException.Create(lsLibrary1 + DllName +
                              DLLExt + lsCannotBeOpened);
          end;
      end;
    end;
  {$endif}
  end;

//-----------------------------------------------------------------------------
// Frees the filter DLLs

procedure TTreeStruct.FreeDlls;

  var
    i: Integer;
    Dll: PDll;
    DLLExt    : string[4];

  begin
{$ifdef DELPHI1}
  DLLExt := '.q16';
{$else}
  DLLExt := '.q32';
{$endif}
{$ifdef mswindows}
  for i := 0 to pred(DllCol^.Count) do
    begin
    Dll := PDll(DllCol^.At(i));
    with Dll^ do
      if DllHandle <> 0 then
        begin
        CB_NewLineToIndicator(lsReleasingLibrary + DLLName + DLLExt);
        FreeLibrary(DllHandle);
        DllHandle := 0;
        end;
    end;
  {$endif}
  end;

//-----------------------------------------------------------------------------
// Loads DLLs for extracting from archives. Used for UnZip only at the moment

 procedure TTreeStruct.LoadExtractDlls;

  var
    i: Integer;
    ExtractDll: PExtractDll;
    ProgramDir: ShortString;
    ZString   : array[0..256] of char;

  begin
{$ifdef mswindows}
{$ifndef DELPHI1}
  ProgramDir := GetProgramDir;
  for i := 0 to pred(ExtractDllCol^.Count) do
    begin
    ExtractDll := PExtractDll(ExtractDllCol^.At(i));
    with ExtractDll^ do
      begin
      if (not bStatic) then
        begin
        CB_NewLineToIndicator(lsLoadingLibrary + DLLName);
        StrPCopy(ZString, ProgramDir + DLLName);
        DllHandle := LoadLibrary(ZString);
        if DllHandle > 32
          then
            begin
            @ExtractFile  := GetProcAddress(DllHandle, 'ExtractFile');
            if (@ExtractFile = nil) then
              begin
              FreeLibrary(DllHandle);
              DllHandle := 0;
              raise EQDirNormalException.Create(lsLibrary + DllName
                          + lsDoesNotContainProperFunctions);
              end;
            end
          else
            begin
            DllHandle := 0;
            raise EQDirNormalException.Create(lsLibrary1 + DllName +
                                lsCannotBeOpened);
            end;
        end;
      end;
    end;
{$endif}
{$endif}
  end;

//-----------------------------------------------------------------------------
// Frees DLLs for extracting from archives. Used for UnZip only at the moment

procedure TTreeStruct.FreeExtractDlls;

  var
    i: Integer;
    ExtractDll: PExtractDll;

  begin
{$ifdef mswindows}
{$ifndef DELPHI1}
  for i := 0 to pred(ExtractDllCol^.Count) do
    begin
    ExtractDll := PExtractDll(ExtractDllCol^.At(i));
    with ExtractDll^ do
      if DllHandle <> 0 then
        begin
        CB_NewLineToIndicator(lsReleasingLibrary + DLLName);
        FreeLibrary(DllHandle);
        DllHandle := 0;
        end;
    end;
{$endif}
{$endif}
  end;

//-----------------------------------------------------------------------------
// Scans a disk. The Volume Label is not read here - it is expected, that
// ReadKeyString is calld before calling this.

function TTreeStruct.ScanDisk (Path: ShortString; DiskName: ShortString;
                               VolumeLabel: ShortString): boolean;

  var
    DateTime        : TDateTime;
    QDateTime       : TQDateTime;
    MSec            : word;

  begin
  CB_UpdateProgressIndicator(1, 0);
  ScanDisk := true;
  Dta.Name        := DiskName;
  Dta.VolumeLabel := VolumeLabel;
  Dta.OriginPath  := Path;

  CharsToCutFromPath := length(Path);
  if Path[length(Path)] = '\' then dec(CharsToCutFromPath);
  // e.g. if path is C:\, then CharsToCutFromPath=2}

  TokenFilterMask(PDatabase(OwnerDBase)^.LocalOptions.AllMasksArray, DescMaskCol, DllCol, false);

  //set the names in the ExtractDllCol
  ExtractDllCol^.FreeAll;
  {$ifdef DELPHI1}
  if PDatabase(OwnerDBase)^.LocalOptions.ExtractFromZips then
    ExtractDllCol^.Insert(New(PExtractDll, Init(GetProgramDir + 'UnZip16.exe', atZip)));
  {$else}
  if PDatabase(OwnerDBase)^.LocalOptions.ExtractFromZips then
    ExtractDllCol^.Insert(New(PExtractDll, Init('DUnZip32.dll', atZip, false,
                              PDatabase(OwnerDBase)^.LocalOptions.ExtractSizeLimit)));
  if PDatabase(OwnerDBase)^.LocalOptions.ExtractFromRars then
    ExtractDllCol^.Insert(New(PExtractDll, Init('', atRar, true,
                              PDatabase(OwnerDBase)^.LocalOptions.ExtractSizeLimit)));
  if PDatabase(OwnerDBase)^.LocalOptions.ExtractFromAces then
    ExtractDllCol^.Insert(New(PExtractDll, Init('', atAce, true,
                              PDatabase(OwnerDBase)^.LocalOptions.ExtractSizeLimit)));
  {$endif}

  if (UpCase(Path[1]) >= 'A') and (UpCase(Path[1]) <= 'Z') then
    begin
    Dta.OrigDrive[1] := Path[1];
    QGetDiskSizes(byte(UpCase(Path[1]))-byte('A')+1,
      Dta.DiskSizeKb, Dta.DiskFreeKb, Dta.BytesPerSector, Dta.SectorsPerCluster);
    end;
  DateTime := Now;
  with QDateTime do
    begin
    DecodeDate(DateTime, Year, Month, Day);
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    end;
  PackTime(QDateTime, Dta.ScanDate);
  ScanStatus := PDatabase(OwnerDBase)^.LocalOptions.ScanArchives;
  RootDirCol := New(PDirCollection, Init(100, 100, '\', '', 0));
  CurDirCol := RootDirCol;
  if RecursScanDirs(RootDirCol, nil, Path)
    then
      begin
      DirsScanned := 0;
      CB_UpdateProgressIndicator(2, 0);
      LoadDlls; // Must be no exit till FreeDlls!
      LoadExtractDlls;
      try
        if not RecursScanFiles(RootDirCol, PDatabase(OwnerDBase)^.DataFile, Path) then
          begin
          // cancelled by the user in the phase of scanning files
          Dispose(RootDirCol, Done);
          RootDirCol := New(PDirCollection, Init(10, 100, '\', '', 0));
          CurDirCol := RootDirCol;
          ScanDisk := false;
          end;
      finally
        FreeExtractDlls;
        FreeDlls;
        end;
      end
    else
      begin
      // cancelled by the user in the phase of scanning directories
      Dispose(RootDirCol, Done);
      RootDirCol := New(PDirCollection, Init(10, 100, '\', '', 0));
      CurDirCol := RootDirCol;
      ScanDisk := false;
      end;
  CurDirCol := RootDirCol;
  end;

//-----------------------------------------------------------------------------
// Scans the database of QuickDir4

function TTreeStruct.ScanQDir4Record (Path: ShortString; DiskName: ShortString): boolean;

  var
    WholePath : ShortString;

  begin
  CB_UpdateProgressIndicator(1, 0);
  Result := true;
  Dta.Name := DiskName;
  GetQDir4RecordAttr (Dta.DiskSizeKb, Dta.DiskFreeKb, Dta.ScanDate);
  WholePath  := Path;
  ScanStatus := cfNoScan;
  if WholePath[length(WholePath)] <> '\' then WholePath := WholePath + '\';
  WholePath := WholePath + '*.*';
  RootDirCol := New(PDirCollection, Init(100, 100, '\', '', 0));
  if RecursScanDirsQDir4(RootDirCol, nil, Path)
    then
      begin
      DirsScanned := 0;
      CB_UpdateProgressIndicator(2, 0);
      if not RecursScanFilesQDir4(RootDirCol, PDatabase(OwnerDBase)^.DataFile, Path) then
        begin
        Dispose(RootDirCol, Done);
        RootDirCol := New(PDirCollection, Init(10, 100, '\', '', 0));
        Result := false;
        end;
      end
    else
      begin
      Dispose(RootDirCol, Done);
      RootDirCol := New(PDirCollection, Init(10, 100, '\', '', 0));
      Result := false;
      end;
  CurDirCol := RootDirCol;
  end;

//-----------------------------------------------------------------------------

destructor TTreeStruct.Done;

  begin
  if CurFileCol    <> nil then Dispose(CurFileCol, Done);
  if RootDirCol    <> nil then Dispose(RootDirCol, Done);
  if DescMaskCol   <> nil then Dispose(DescMaskCol, Done);
  if DllCol        <> nil then Dispose(DllCol, Done);
  if ExtractDllCol <> nil then Dispose(ExtractDllCol, Done);
  end;

//-----------------------------------------------------------------------------
// Disposes the current file collection and loads a new from the database.
// If cannot read, at least directories are added, then exception is
// raised (from InitLoad).

procedure TTreeStruct.UpdateCurFileCol(WhichFiles: TLoadFileSelector);

  var
    POneFile: TPOneFile;
    OneFile : TOneFile;
    i       : Integer;
    TmpPDir : PDirCollection;

  begin
  if CurFileCol <> nil then Dispose(CurFileCol, Done);
  CurFileCol := nil;
  PDatabase(OwnerDBase)^.NeedUpdFileWin := true;
  if CurDirCol^.Dta^.FileList <> 0
    then
      CurFileCol := New(PFileCollection, InitLoad(PDatabase(OwnerDBase)^.DataFile,
                        CurDirCol^.Dta^.FileList,
                        (WhichFiles = fsAll) or (WhichFiles = fsFilesOnly),
                        PDatabase(OwnerDBase)^.ErrorCounter))
    else
      CurFileCol := New(PFileCollection, Init(100,100));

  if (WhichFiles <> fsFilesOnly) then
    begin
    for i := CurDirCol^.Count-1 downto 0 do
      begin
      TmpPDir := CurDirCol^.At(i);
      FillChar(OneFile, SizeOf(OneFile), 0);
      OneFile.SelfFilePos := TmpPDir^.Dta^.SelfDirPos;
      OneFile.Description := TmpPDir^.Dta^.Description;
      OneFile.Size        := TmpPDir^.Dta^.Size;
      OneFile.Time        := TmpPDir^.Dta^.Time;
      OneFile.Attr        := TmpPDir^.Dta^.Attr;
      OneFile.Ext         := TmpPDir^.Dta^.Ext;
      OneFile.LongName    := TmpPDir^.Dta^.LongName;
      OneFile.SubDirCol   := CurDirCol^.At(i);
      GetMemOneFile(POneFile, OneFile);
      CurFileCol^.AtInsert(0, POneFile);
      end;
    if (WhichFiles = fsAll) and (CurDirCol^.ParentCol <> nil) then
      begin
      FillChar(OneFile, SizeOf(OneFile), 0);
      OneFile.LongName := '..';
      OneFile.Attr := faDirectory;
      OneFile.SubDirCol := CurDirCol^.ParentCol;
      GetMemOneFile(POneFile, OneFile);
      CurFileCol^.AtInsert(0, POneFile);
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Recursively scans directories to build a tree.

function TTreeStruct.RecursScanDirs (Col, aParentCol: PDirCollection;
                                     Path: ShortString): boolean;

  var
    ExtPath        : ShortString;
    QSearchRec     : TSearchRec;
    i              : Integer;
    NewDirCol      : PDirCollection;
    LongName       : ShortString;
    Ext            : TExt;
    QResult        : Integer;
    OldPathLen     : integer;
    NewPathLen     : integer;

  begin
  RecursScanDirs := false;
  if CB_AbortScan then exit; // aborted by the user
  RecursScanDirs := true;
  Col^.ParentCol := aParentCol;
  CB_NewLineToIndicator(Path);
  if Path[length(Path)] <> '\' then Path := Path + '\';
  OldPathLen := length(Path);
  ExtPath := Path + '*.*';
  QResult := SysUtils.FindFirst (ExtPath, faAnyFile, QSearchRec);
  while QResult = 0 do
    begin
    if (QSearchRec.Attr and faDirectory = faDirectory) and
       (QSearchRec.Name <> '.') and (QSearchRec.Name <> '..') then
        begin
        inc(Dta.Dirs);
        {$ifdef DELPHI1}
        LongName := AnsiLowerCase(QSearchRec.Name);
        {$else}
        LongName := AnsiUpperCase(QSearchRec.Name);
        if (LongName = QSearchRec.Name)
          then LongName := AnsiLowerCase(QSearchRec.Name)
          else LongName := QSearchRec.Name;
        {$endif}
        SeparateExt(LongName, Ext);
        NewDirCol := New(PDirCollection, Init(4, 32, LongName, Ext, QSearchRec.Time));
        Col^.Insert(NewDirCol);
        end;
    QResult := SysUtils.FindNext(QSearchRec);
    end;
  SysUtils.FindClose(QSearchRec);

  with Col^ do
    for i := 0 to pred(Count) do
      begin
      // test if we did not exceeded the capacity of the string
      NewPathLen := integer(length(Path)) +
         integer(length(PDirCollection(At(i))^.Dta^.LongName)) +
         integer(length(PDirCollection(At(i))^.Dta^.Ext));
      if (NewPathLen < 255) and (NewPathLen > OldPathLen) then
        if not RecursScanDirs(At(i), Col,
                   Path + PDirCollection(At(i))^.Dta^.LongName +
                   PDirCollection(At(i))^.Dta^.Ext) then
          begin // aborted by the user
          RecursScanDirs := false;
          exit;
          end;
      end;
  end;

//-----------------------------------------------------------------------------
// Imports tree from QuickDir 4

function TTreeStruct.RecursScanDirsQDir4 (Col, aParentCol: PDirCollection;
                                          Path: ShortString): boolean;
  var
    ExtPath      : ShortString;
    QDir4SearchRec : TQDir4SearchRec;
    i              : Integer;
    NewDirCol      : PDirCollection;
    LongName       : ShortString;
    Ext            : TExt;
    QResult        : Integer;
    ItIsArchive    : boolean;

  begin
  Result := false;
  if CB_AbortScan then exit;
  Result := true;
  Col^.ParentCol := aParentCol;
  CB_NewLineToIndicator(Path);
  if Path[length(Path)] <> '\' then Path := Path + '\';
  ExtPath := Path + '*.*';
  QResult := FindQDir4First (ExtPath, QDir4SearchRec, ItIsArchive);
  while QResult = 0 do
    begin
    if ((QDir4SearchRec.Attr and faDirectory = faDirectory) or ItIsArchive) and
       (QDir4SearchRec.Name <> '.') and (QDir4SearchRec.Name <> '..') then
      begin
      inc(Dta.Dirs);
      {$ifdef DELPHI1}
      LongName := AnsiLowerCase(QDir4SearchRec.Name);
      {$else}
      LongName := AnsiUpperCase(QDir4SearchRec.Name);
      if (LongName = QDir4SearchRec.Name)
        then LongName := AnsiLowerCase(QDir4SearchRec.Name)
        else LongName := QDir4SearchRec.Name;
      {$endif}

      SeparateExt(LongName, Ext);

      NewDirCol := New(PDirCollection, Init(4, 32, LongName, Ext, QDir4SearchRec.Time));
      if ItIsArchive then
        begin
        NewDirCol^.Dta^.Size  := QDir4SearchRec.Size;
        NewDirCol^.Dta^.Time  := QDir4SearchRec.Time;
        NewDirCol^.Dta^.Attr  := NewDirCol^.Dta^.Attr or faQArchive;
        inc(Dta.Archives);
        end;
      Col^.Insert(NewDirCol);
      end;
    QResult := FindQDir4Next(QDir4SearchRec, ItIsArchive);
    end;

  with Col^ do
    for i := 0 to pred(Count) do
      begin
      if (integer(length(Path)) +
         integer(length(PDirCollection(At(i))^.Dta^.LongName)) +
         integer(length(PDirCollection(At(i))^.Dta^.Ext))) >= 255 then
        begin
        RecursScanDirsQDir4 := false;
        exit;
        end;
      if not RecursScanDirsQDir4(At(i), Col,
        Path + PDirCollection(At(i))^.Dta^.LongName +
          PDirCollection(At(i))^.Dta^.Ext) then
        begin
        Result := false;
        exit;
        end;
      end;
  end;

//-----------------------------------------------------------------------------
// Checks, if some of the filter mask applies and if so, extracts the description
// from the file and saves it to database.
// PathIsTmpFile: if true, then in the Path is the whole file specification
// i.e. the Name must be set to '' after the filter detection

function TTreeStruct.GetDescription (var OutF: TQBufStream; var Path: ShortString;
                                     Name: ShortString; PathIsTmpFile: boolean): longint;

  const
    BufSize = 8*1024;
  type
    TBuffer = array[1..BufSize] of byte;

  var
    i,j         : Integer;
    InpF        : TQBufStream;
    InfoSize    : longint;
    Read        : longint;
    LongRead    : longword;
    ReadCount   : longint;
    Cluster     : Integer;
    Buf         : ^TBuffer;
    PBuf        : PChar;
    RecordHeader: TRecordHeader;
    DescHeader  : TDescHeader;
    OneMask     : POneMask;
    BlockHandle : longint;
    ZString     : array[0..256] of char;
    TransferOK  : boolean;

   begin
   Result := 0;
   if not PDatabase(OwnerDBase)^.LocalOptions.ScanDesc then exit;
   if CB_AbortScan then exit; // aborted by the user
   for i := 0 to pred(DescMaskCol^.Count) do
     begin
     OneMask := POneMask(DescMaskCol^.At(i));
     if MaskCompare (GetPQString(OneMask^.MaskName), Name, false, true) then
       begin
       if PathIsTmpFile then Name := ''; // avoid appending it to the path
       if (OneMask^.ConvDll <> nil) and (OneMask^.ConvDll^.DllHandle <> 0)
         then // read the description using a filter
           begin
           if OneMask^.ConvDll^.OpenTransfer (StrPCopy(ZString, Path + Name),
             BlockHandle) <> 0 then exit;
           InfoSize := OneMask^.MaxSize; // it may change later on
           RecordHeader.Size := SizeOf(TRecordHeader) + SizeOf(TDescHeader) + InfoSize;
           RecordHeader.SizeInvert := not RecordHeader.Size;
           RecordHeader.RecType    := rtFileDesc;
           Result := OutF.GetSize;
           OutF.SeekToEnd;
           OutF.Write (RecordHeader, SizeOf(TRecordHeader));
           DescHeader.Attr := 0;
           DescHeader.Time := 0;
           OutF.Write (DescHeader, SizeOf(TDescHeader)); // not used yet
           PBuf := StrAlloc(BufSize+1);

           ReadCount  := 0;
           TransferOK := true;
           while (ReadCount < InfoSize) and TransferOK do
             begin
             TransferOK := OneMask^.ConvDll^.GetOneBlock (BlockHandle, PBuf,
               MinLI (BufSize, InfoSize-ReadCount), Read) = 0;
             ReadCount := ReadCount + Read;
             if Read > 0
               then OutF.Write(PBuf[0], Read)
               else TransferOK := false;
             end;
           OneMask^.ConvDll^.CloseTransfer(BlockHandle);
           if ReadCount <> InfoSize then // size changed, correct it in the header
             begin
             RecordHeader.Size := SizeOf(TRecordHeader) + SizeOf(TDescHeader) + ReadCount;
             RecordHeader.SizeInvert := not RecordHeader.Size;
             RecordHeader.RecType    := rtFileDesc;
             OutF.Seek(Result);
             OutF.Write (RecordHeader, SizeOf(TRecordHeader));
             end;
           StrDispose(PBuf);
           end
         else // read without filter
           begin
           if not FileExists(Path + Name) then // namely because of DiskInfo.txt
             begin
             Result := 0;
             exit;
             end;
           try
             InpF.Init(StrPCopy(ZString, Path + Name), stOpenReadNonExclusive);
           except
             on E: EQDirException do
               begin
               Result := 0;
               exit;
               end;
             end;

           InfoSize := MinW(OneMask^.MaxSize, word(InpF.GetSize));
           if InfoSize = 0 then
             begin
             InpF.Done;
             exit;
             end;

           RecordHeader.Size := SizeOf(TRecordHeader) + SizeOf(TDescHeader) + InfoSize;
           RecordHeader.SizeInvert := not RecordHeader.Size;
           RecordHeader.RecType    := rtFileDesc;
           try
             OutF.SeekToEnd;
             Result  := OutF.GetPos;
             OutF.Write (RecordHeader, SizeOf(TRecordHeader));
             DescHeader.Attr := 0;
             DescHeader.Time := 0;
             OutF.Write (DescHeader, SizeOf(TDescHeader));
           except
             on E: EQDirException do
               begin
               InpF.Done;
               raise;
               end;
             end;

           GetMem(Buf, BufSize);

           ReadCount := 0;
           while (ReadCount < InfoSize) and not InpF.Eof do
             begin
             if (InfoSize - ReadCount) >= BufSize
               then Cluster := BufSize
               else Cluster := InfoSize - ReadCount;
             try
               InpF.ReadExt(Buf^, Cluster, LongRead);
             except
               on E: EQDirException do
                 begin
                 Result := 0;
                 exit;
                 end;
               end;
             Read := LongRead;
             if Read <> Cluster then
               begin
               InpF.Done;
               Result := 0;
               exit;
               end;
             ReadCount := ReadCount + Read;
             for j:= 1 to Read do // delete control characters
               if Buf^[j] < 32 then
                 if (Buf^[j] <> 10) and (Buf^[j] <> 13) then
                   Buf^[j] := 32;
             try
               if Read > 0 then OutF.Write(Buf^, Read);
             except
               on E: EQDirException do
                 begin
                 InpF.Done;
                 raise;
                 end;
               end;
             end;
           FreeMem(Buf, BufSize);
           InpF.Done;
           end;
       exit; // exit is necesary, terminates the loop
       end;
     end;
   end;

//-----------------------------------------------------------------------------
// Recursively scans the files on the disk. We already have the directory tree
// available, now we must find files.

function TTreeStruct.RecursScanFiles (Col: PDirCollection; var F: TQBufStream;
                                      Path: ShortString): boolean;

  // copies file descriptions from the original record in the database
  procedure GetDescFromSavedFileCol(SavedCurFileCol: PFileCollection;
                                   Time: longint; var LongName: ShortString;
                                   var Ext: TExt; var FilePointer: TFilePointer;
                                   var Attr: word);
    var
      i               : Integer;
      POneFile        : TPOneFile;

    begin
    FilePointer := 0;
    for i := 0 to pred(SavedCurFileCol^.Count) do
      begin
      POneFile := TPOneFile(SavedCurFileCol^.At(i));
      if (((POneFile^.Time = Time) and not PDatabase(OwnerDBase)^.LocalOptions.AlwaysCreateDesc)
        or (POneFile^.Attr and faQDescModified <> 0))
        and (AnsiCompareText(POneFile^.LongName, LongName) = 0)
        and (AnsiCompareText(POneFile^.Ext, Ext) = 0) then
          begin
          FilePointer := POneFile^.Description;
          if (FilePointer <> 0) and (POneFile^.Attr and faQDescModified <> 0) then
            Attr := Attr or faQDescModified;
          exit;
          end;
      end;
    end;


  // copies dir descriptions from the original record in the database
  procedure GetDescFromSavedDirCol(SavedCurDirCol: PDirCollection;
                                   Time: longint; var LongName: ShortString;
                                   var Ext: TExt; var FilePointer: TFilePointer;
                                   var Attr: word);
    var
      i               : Integer;
      POneDir         : TPOneDir;

    begin
    for i := 0 to pred(SavedCurDirCol^.Count) do
      begin
      POneDir := PDirCollection(SavedCurDirCol^.At(i))^.Dta;
      if (((POneDir^.Time = Time) and not PDatabase(OwnerDBase)^.LocalOptions.AlwaysCreateDesc)
        or (POneDir^.Attr and faQDescModified <> 0))
        and (AnsiCompareText(POneDir^.LongName, LongName)=0)
        and (AnsiCompareText(POneDir^.Ext, Ext)=0) then
          begin
          FilePointer := POneDir^.Description;
          if (FilePointer <> 0) and (POneDir^.Attr and faQDescModified <> 0) then
            Attr := Attr or faQDescModified;
          exit;
          end;
      end;
    end;

  // copies contents of archives from the original record in the database
  function ArchiveExtractedFromSaved (NewDirCol, SavedDirCol: PDirCollection;
                                      FileCol: PFileCollection;
                                      Index: Integer): boolean;
    var
      i           : Integer;
      FoundPOneDir: TPOneDir;
      OneFile     : TOneFile;
      FoundDirCol : PDirCollection;
      Found       : boolean;

    begin
    Result := false;
    FoundDirCol := nil;
    if SavedDirCol = nil then exit;
    MoveOneFile(TPOneFile(FileCol^.At(Index))^, OneFile);
    Found := false;
    for i := 0 to pred(SavedDirCol^.Count) do
      begin
      FoundDirCol := PDirCollection(SavedDirCol^.At(i));
      FoundPOneDir := FoundDirCol^.Dta;
      if   ((FoundPOneDir^.Time = OneFile.Time)
        and (FoundPOneDir^.Attr and faQArchive <> 0))
        and (AnsiCompareText(FoundPOneDir^.LongName, OneFile.LongName)=0)
        and (AnsiCompareText(FoundPOneDir^.Ext, OneFile.Ext)=0) then
          begin
          // we found the folder with the same name and date and it is an archive
          Found := true;
          break;
          end;
      end;
    if not Found then exit;
    // delete - so that it is not disposed twice
    SavedDirCol^.Delete(FoundDirCol);
    FoundDirCol^.ParentCol := NewDirCol;
    NewDirCol^.Insert(FoundDirCol);
    FileCol^.AtFree(Index);

    inc(Dta.Archives);
    Dta.ArcDirs  := Dta.ArcDirs + FoundDirCol^.SubTotals.DataDirs;
    Dta.ArcFiles := Dta.ArcFiles + FoundDirCol^.SubTotals.DataFiles;
    Dta.ArchSizeBytes := Dta.ArchSizeBytes + FoundDirCol^.SubTotals.DataFileSize;
    Dta.DataSizeBytes := Dta.DataSizeBytes + FoundDirCol^.SubTotals.DataFileSize;

    Result := true;
    end;

  // compares 2 file colelctions to find if they are equal.
  function FileColsAreEqual(FileCol1: PFileCollection;
                            FileCol2: PFileCollection): boolean;
    var
      i1,i2            : Integer;
      POneFile1        : TPOneFile;
      POneFile2        : TPOneFile;
      Found            : boolean;

    begin
    Result := false;
    if (FileCol1 = nil) then exit;
    if (FileCol2 = nil) then exit;
    if (FileCol1^.Count <> FileCol2^.Count) then exit;
    for i1 := 0 to pred(FileCol1^.Count) do
      begin
      POneFile1 := TPOneFile(FileCol1^.At(i1));
      Found := false;
      // first try the fast find: the file may be on the same place
      // as in the original, so i1 should be equal to i2
      POneFile2 := TPOneFile(FileCol2^.At(i1));
      if    (POneFile1^.Time = POneFile2^.Time)
        and (POneFile1^.Attr = POneFile2^.Attr)
        and (POneFile1^.Description = POneFile2^.Description)
        and (AnsiCompareText(POneFile1^.Ext, POneFile2^.Ext) = 0)
        and (AnsiCompareText(POneFile1^.LongName, POneFile2^.LongName) = 0)then
          Found := true;

      // we did not found, so go slowly through all the files
      if not Found then
        begin
        for i2 := 0 to pred(FileCol2^.Count) do
          begin
          POneFile2 := TPOneFile(FileCol2^.At(i2));
          if    (POneFile1^.Time = POneFile2^.Time)
            and (POneFile1^.Attr = POneFile2^.Attr)
            and (POneFile1^.Description = POneFile2^.Description)
            and (AnsiCompareText(POneFile1^.Ext, POneFile2^.Ext) = 0)
            and (AnsiCompareText(POneFile1^.LongName, POneFile2^.LongName) = 0)then
              begin
              Found := true;
              break;
              end;
          end;
        if not Found then exit;
        end;
      end;
    Result := true;
    end;


 var
   ExtPath      : ShortString;
   QSearchRec     : TSearchRec;
   i              : Integer;
   FileCol        : PFileCollection;
   OneFile        : TOneFile;
   POneFile       : TPOneFile;
   POneDir        : TPOneDir;
   TotalSize      : longword;
   SaveCount      : Integer;
   QResult        : Integer;
   SavedTreeStruct: PTreeStruct;
   SavedCurDirCol : PDirCollection;
   SavedCurFileCol: PFileCollection;
   ArchiveDirCol  : PDirCollection;
   RealDiskInfoAdded   : boolean;
   SimulDiskInfoAdded  : boolean;
   AlwaysRescanArchives: boolean;

   DateTime       : TDateTime;
   QDateTime      : TQDateTime;
   MSec           : word;
   NewPathLen     : integer;
   OldPathLen     : integer;
   DateTimeForDiskInfo: longint; // we will use the date of the last file

  begin
  RecursScanFiles := false;
  if CB_AbortScan then exit;
  RecursScanFiles := true;
  SavedTreeStruct := PDatabase(OwnerDBase)^.SavedTreeStruct;
  SimulDiskInfoAdded := false;
  Col^.Dta^.FilesCount := 0;
  Col^.Dta^.FilesTotalSize := 0;
  DateTimeForDiskInfo := 0;
  RealDiskInfoAdded  := not PDatabase(OwnerDBase)^.LocalOptions.SimulateDiskInfo;
  AlwaysRescanArchives := PDatabase(OwnerDBase)^.LocalOptions.AlwaysRescanArchives;
  // first we will find the corresponding collection of dirs and collectzion of files
  // in the original tree
  SavedCurDirCol  := nil;
  SavedCurFileCol := nil;
  if SavedTreeStruct <> nil then
    begin
    SavedCurDirCol := SavedTreeStruct^.RecurseFindDir(
                          SavedTreeStruct^.RootDirCol,
                          ShortCopy(Path, CharsToCutFromPath+1, length(Path) - CharsToCutFromPath));
    if SavedCurDirCol <> nil then
      begin
      SavedTreeStruct^.CurDirCol := SavedCurDirCol;
      SavedTreeStruct^.UpdateCurFileCol(fsWithDescOnly);
      SavedCurFileCol := SavedTreeStruct^.CurFileCol;
      end;
    end;

  CB_NewLineToIndicator(Path);
  inc(DirsScanned);
  if Dta.Dirs > 0 then
    CB_UpdateProgressIndicator(2, (100*DirsScanned) div Dta.Dirs);
  if Path[length(Path)] <> '\' then Path := Path + '\';
  OldPathLen := Length(Path);
  ExtPath := Path + '*.*';

  FileCol := New(PFileCollection, Init(100,200));
  QResult := SysUtils.FindFirst (ExtPath, faAnyFile, QSearchRec);
  while QResult = 0 do
    begin
    if QSearchRec.Attr and (faDirectory or faVolumeId) = 0 then
      begin
      inc(Dta.Files);
      Dta.PhysSizeBytes := Dta.PhysSizeBytes + QSearchRec.Size;
      inc(Col^.Dta^.FilesCount);
      Col^.Dta^.FilesTotalSize := Col^.Dta^.FilesTotalSize + QSearchRec.Size;
      with OneFile do
        begin
        Attr := QSearchRec.Attr;
        Time := QSearchRec.Time;
        Size := QSearchRec.Size;
        DateTimeForDiskInfo := Time;
        OneFile.LongName := AnsiUpperCase(QSearchRec.Name);
        if OneFile.LongName = lsDiskInfoUpperCase
          then // do not add dummy file
            begin
            RealDiskInfoAdded := true;
            OneFile.LongName := lsDiskInfo;
            end
          else
            begin
            if (OneFile.LongName = QSearchRec.Name)
              then OneFile.LongName := AnsiLowerCase(QSearchRec.Name)
              else OneFile.LongName := QSearchRec.Name;
            end;
        SeparateExt(LongName, Ext);
        SubDirCol := nil;
        Description := 0;
        // take the description of the file, if it is available and the file is not changed
        if SavedCurFileCol <> nil then
          GetDescFromSavedFileCol(SavedCurFileCol,
            Time, LongName, Ext, Description, Attr);
        // if not found, try to fidn it also in the collection of dirs
        // for the case it is an archive, on disk is archive of newer date
        // but we cannot scan its contents
        if (Description = 0) and (SavedCurDirCol <> nil) then
          GetDescFromSavedDirCol(SavedCurDirCol,
                                 Time, LongName, Ext, Description, Attr);
        // not found, so try to get the description form the file
        if Description = 0 then
          Description := GetDescription(F, Path, LongName + Ext, false);
        end;
      GetMemOneFile(POneFile, OneFile);
      FileCol^.Insert(POneFile);
      end;
    if SimulDiskInfoAdded
      then QResult := -1
      else QResult := SysUtils.FindNext(QSearchRec);
    // add dummy file for description of disk
    if (QResult <> 0) and not SimulDiskInfoAdded and not RealDiskInfoAdded
     and (Col = RootDirCol) then
      begin
      SimulDiskInfoAdded := true;
      QResult := 0;
      QSearchRec.Attr := 0;
      QSearchRec.Name := lsDiskInfo;
      QSearchRec.Size := 1;
      if DateTimeForDiskInfo = 0
        then
          begin
          DateTime := Now;
          with QDateTime do
            begin
            DecodeDate(DateTime, Year, Month, Day);
            DecodeTime(DateTime, Hour, Min, Sec, MSec);
            end;
          PackTime(QDateTime, longint(QSearchRec.Time));
          end
        else
          QSearchRec.Time := DateTimeForDiskInfo;
      end;
    end;
  SysUtils.FindClose(QSearchRec);

  i := 0;
  while i < FileCol^.Count do
    begin
    SaveCount := FileCol^.Count;
    // try to copy the contents of the archive from the original record
    if not AlwaysRescanArchives and ArchiveExtractedFromSaved (Col, SavedCurDirCol, FileCol, i)
      then
        begin
        end
      else
        begin
        // we could not take it from the original, try to scan it
        ArchiveDirCol := ScanForArchive (Col, Path, F, FileCol, i);
        // if we succeeded, we must copy the descriptions
        if ArchiveDirCol <> nil then
          UpdateDescInArchiveFromSaved(ArchiveDirCol, Path, F);
          // warining: changes CurDirCol and CurFileCol -
          // SavedCurFileCol is invalid here, SavedCurDirCol not
        SavedCurFileCol := nil;
        end;
    if SaveCount = FileCol^.Count then inc(i); // otherwise the item was deleted
    end;

  if SavedCurDirCol <> nil then
    begin
    if SavedCurFileCol = nil then
      begin
      // need to update the file collection
      SavedTreeStruct^.CurDirCol := SavedCurDirCol;
      SavedTreeStruct^.UpdateCurFileCol(fsWithDescOnly);
      SavedCurFileCol := SavedTreeStruct^.CurFileCol;
      end;
    with Col^ do
      for i := 0 to pred(Count) do
        begin
        POneDir := PDirCollection(At(i))^.Dta;
        if POneDir^.Description = 0 then
          GetDescFromSavedDirCol(SavedCurDirCol,
             POneDir^.Time, POneDir^.LongName, POneDir^.Ext,
             POneDir^.Description, POneDir^.Attr);
        // not found, so try the collection of files - it can be the case of
        // archive, which was originally in the database as normal file.
        if (POneDir^.Description = 0) and (SavedCurFileCol <> nil) then
          GetDescFromSavedFileCol(SavedCurFileCol,
             POneDir^.Time, POneDir^.LongName, POneDir^.Ext,
             POneDir^.Description, POneDir^.Attr);
        end;
    end;

  // Now we have the collection before storage. Compare it with the original one,
  // so we can find, whether we can reuse the old one, if it is the same - saves
  // space in the database.

  // Before we loaded only the list oif files, which have a description -
  // UpdateCurFileCol(fsWithDescOnly), now we need ALL files: UpdateCurFileCol(fsFilesOnly)
  if SavedCurDirCol <> nil then
    begin
    SavedTreeStruct^.CurDirCol := SavedCurDirCol;
    SavedTreeStruct^.UpdateCurFileCol(fsFilesOnly);
    SavedCurFileCol := SavedTreeStruct^.CurFileCol;
    end;

  if FileColsAreEqual(FileCol, SavedCurFileCol)
    then
      begin
      // use the pointer to the original list of files
      Col^.Dta^.FileList := SavedCurDirCol^.Dta^.FileList;
      Dispose(FileCol, Done);
      end
    else
      begin
      F.SeekToEnd;
      Col^.Dta^.FileList := F.GetPos;
      TotalSize := 0;
      FileCol^.StoreCol(F, TotalSize, true);
      Dispose(FileCol, Done);
      end;

  with Col^ do
    for i := 0 to pred(Count) do
      if (PDirCollection(At(i))^.Dta^.Size = 0) and (PDirCollection(At(i))^.Dta^.Time = 0) then
        begin
        // check the string overflow
        NewPathLen := integer(length(Path)) +
           integer(length(PDirCollection(At(i))^.Dta^.LongName)) +
           integer(length(PDirCollection(At(i))^.Dta^.Ext));
        if (NewPathLen < 255) and (NewPathLen > OldPathLen) then
          if not RecursScanFiles(At(i), F,
            Path + PDirCollection(At(i))^.Dta^.LongName +
              PDirCollection(At(i))^.Dta^.Ext) then
            begin // aborted by the user
            RecursScanFiles := false;
            exit;
            end;
        end;
  end;

//-----------------------------------------------------------------------------
// Copies the descriptions from QuickDir 4 database

function TTreeStruct.CopyQDir4Desc (var OutF: TQBufStream; PDesc: pointer): longint;

  type
    TLongString = record
      Len: word;
      Dta: array[1..8*1024] of char;
      end;
    PLongString = ^TLongString;

  var
    InfoSize    : longint;
    RecordHeader: TRecordHeader;
    DescHeader  : TDescHeader;
    LStr        : PLongString;

   begin
   Result := 0;
   if PDesc = nil then exit;
   LStr := PLongString(PDesc);
   InfoSize := MinW(32*1024-1, word(LStr^.Len));
   if InfoSize = 0 then exit;
   RecordHeader.Size := SizeOf(TRecordHeader) + SizeOf(TDescHeader) + InfoSize;
   RecordHeader.SizeInvert := not RecordHeader.Size;
   RecordHeader.RecType    := rtFileDesc;
   OutF.SeekToEnd;
   Result  := OutF.GetPos;
   OutF.Write (RecordHeader, SizeOf(TRecordHeader));
   DescHeader.Attr := 0;
   DescHeader.Time := 0;
   OutF.Write (DescHeader, SizeOf(TDescHeader)); {zatim nevyuzito}
   OutF.Write(LStr^.Dta, InfoSize);
   end;

//-----------------------------------------------------------------------------
// Scans files in the QuickDir database

function TTreeStruct.RecursScanFilesQDir4 (Col: PDirCollection; var F: TQBufStream;
                                           Path: ShortString): boolean;

  var
    ExtPath      : ShortString;
    QDir4SearchRec : TQDir4SearchRec;
    i              : Integer;
    FileCol        : PFileCollection;
    OneFile        : TOneFile;
    POneFile       : TPOneFile;
    POneDir        : TPOneDir;
    TotalSize      : longword;
    QResult        : Integer;
    ItIsArchive    : boolean;
    PDesc          : pointer;

  begin
  Result := false;
  if CB_AbortScan then exit;
  Result := true;
  CB_NewLineToIndicator(Path);
  inc(DirsScanned);
  if Dta.Dirs > 0 then
    CB_UpdateProgressIndicator(2, (100*DirsScanned) div Dta.Dirs);
  if Path[length(Path)] <> '\' then Path := Path + '\';
  ExtPath := Path + '*.*';

  FileCol := New(PFileCollection, Init(100,200));
  QResult := FindQDir4First (ExtPath, QDir4SearchRec, ItIsArchive);
  while QResult = 0 do
    begin
    if (QDir4SearchRec.Attr and (faDirectory or faVolumeId) = 0) and not ItIsArchive then
      begin
      inc(Dta.Files);
      Dta.PhysSizeBytes := Dta.PhysSizeBytes + QDir4SearchRec.Size;
      with OneFile do
        begin
        Attr := QDir4SearchRec.Attr;
        Time := QDir4SearchRec.Time;
        Size := QDir4SearchRec.Size;
        {$ifdef DELPHI1}
        OneFile.LongName := AnsiLowerCase(QDir4SearchRec.Name);
        {$else}
        OneFile.LongName := AnsiUpperCase(QDir4SearchRec.Name);
        if (OneFile.LongName = QDir4SearchRec.Name)
          then OneFile.LongName := AnsiLowerCase(QDir4SearchRec.Name)
          else OneFile.LongName := QDir4SearchRec.Name;
        {$endif}
        SeparateExt(LongName, Ext);
        SubDirCol := nil;
        Description := 0;
        end;
      GetMemOneFile(POneFile, OneFile);
      FileCol^.Insert(POneFile);
      end;
    QResult := FindQDir4Next(QDir4SearchRec, ItIsArchive);
    end;

  fillchar(OneFile, sizeof(OneFile), 0);
  if length(Path) = 3 then
    begin
    PDesc := GetQDir4DirDesc (Path);
    if PDesc <> nil then
      begin
      with OneFile do
        begin
        OneFile.LongName := lsDirInfo;
        SeparateExt(LongName, Ext);
        SubDirCol := nil;
        Description := CopyQDir4Desc(F, PDesc);
        end;
      GetMemOneFile(POneFile, OneFile);
      FileCol^.Insert(POneFile);
      end;
    PDesc := GetQDir4DiskDesc;
    if PDesc <> nil then
      begin
      with OneFile do
        begin
        OneFile.LongName := lsDiskInfo;
        SeparateExt(LongName, Ext);
        SubDirCol := nil;
        Description := CopyQDir4Desc(F, PDesc);
        end;
      GetMemOneFile(POneFile, OneFile);
      FileCol^.Insert(POneFile);
      end;
    end;

  with Col^ do
    for i := 0 to pred(Count) do
      begin
      POneDir := PDirCollection(At(i))^.Dta;
      PDesc := GetQDir4DirDesc (Path+POneDir^.LongName+POneDir^.Ext);
      POneDir^.Description := CopyQDir4Desc(F, PDesc);
      end;

  F.SeekToEnd;
  Col^.Dta^.FileList := F.GetPos;
  TotalSize := 0;
  FileCol^.StoreCol(F, TotalSize, true);
  Dispose(FileCol, Done);

  with Col^ do
    for i := 0 to pred(Count) do
      if (PDirCollection(At(i))^.Dta^.Attr and faDirectory <> 0) then
        begin
        if (integer(length(Path)) +
           integer(length(PDirCollection(At(i))^.Dta^.LongName)) +
           integer(length(PDirCollection(At(i))^.Dta^.Ext))) >= 255 then
          begin
          RecursScanFilesQDir4 := false;
          exit;
          end;
        if not RecursScanFilesQDir4(At(i), F,
          Path + PDirCollection(At(i))^.Dta^.LongName +
            PDirCollection(At(i))^.Dta^.Ext) then
          begin
          Result := false;
          exit;
          end;
        end;
  end;

//-----------------------------------------------------------------------------
// Copies all the descriptions from the archive - recursively

procedure TTreeStruct.UpdateDescInArchiveFromSaved (ArchiveDirCol: PDirCollection;
                                                    Path: ShortString; var F: TQBufStream);
  var
    SavedTreeStruct: PTreeStruct;
    SavedCurDirCol : PDirCollection;

  begin
  Path := Path + ArchiveDirCol^.Dta^.LongName + ArchiveDirCol^.Dta^.Ext;
  // assures the path has the backslash at the end in the calling procedure
  SavedTreeStruct := PDatabase(OwnerDBase)^.SavedTreeStruct;
  if SavedTreeStruct <> nil then
    begin
    SavedCurDirCol := SavedTreeStruct^.RecurseFindDir(SavedTreeStruct^.RootDirCol,
       ShortCopy(Path, CharsToCutFromPath+1, length(Path) - CharsToCutFromPath));
    RecurseUpdateDesc(SavedTreeStruct, SavedCurDirCol, ArchiveDirCol, F);
    end
  end;

//-----------------------------------------------------------------------------
// Recursive procedure for copying the the descriptions from the archive

procedure TTreeStruct.RecurseUpdateDesc(SavedTreeStruct: PTreeStruct;
                                         SavedDirCol, NewDirCol: PDirCollection;
                                         var F: TQBufStream);

  var
    SavedCurFileCol: PFileCollection;
    i, j           : Integer;
    PSavedOneFile  : TPOneFile;
    PSavedOneDir   : TPOneDir;
    POneFile       : TPOneFile;
    POneDir        : TPOneDir;
    OneFile        : TOneFile;

  begin
  if SavedDirCol = nil then exit;

  SavedTreeStruct^.CurDirCol := SavedDirCol;
  SavedTreeStruct^.UpdateCurFileCol(fsWithDescOnly);
  SavedCurFileCol := SavedTreeStruct^.CurFileCol;

  CurDirCol := NewDirCol;
  UpdateCurFileCol(fsAll);

  for i := 0 to pred(SavedCurFileCol^.Count) do
    begin
    PSavedOneFile := TPOneFile(SavedCurFileCol^.At(i));
    for j := 0 to pred(CurFileCol^.Count) do
      begin
      POneFile := TPOneFile(CurFileCol^.At(j));
      if (AnsiCompareText(POneFile^.LongName, PSavedOneFile^.LongName)=0) and
        (AnsiCompareText(POneFile^.Ext, PSavedOneFile^.Ext)=0) then
          begin
          // must be written, not enough to set in memory.
          F.Seek(POneFile^.SelfFilePos);
          F.Read(OneFile.Description, oneFileHeadSize);
          if PSavedOneFile^.Description <> 0 then
            begin
            if (OneFile.Description <> 0) then
              if (PSavedOneFile^.Attr and faQDescModified) = 0 then break;
              // i.e. the original descritpion as not chaned - not need to copy, when we have a new one
            OneFile.Description := PSavedOneFile^.Description;
            OneFile.Attr        := PSavedOneFile^.Attr;
            F.Seek(POneFile^.SelfFilePos);
            F.Write(OneFile.Description, oneFileHeadSize);
            if F.Status <> stOK then exit;
            end;
          break;
          end;
      end;
    end;

  // the recursion changes CurDirCol, so we use NewDirCol
  for i := 0 to pred(SavedDirCol^.Count) do
    begin
    PSavedOneDir := PDirCollection(SavedDirCol^.At(i))^.Dta;
    for j := 0 to pred(NewDirCol^.Count) do
      begin
      POneDir := PDirCollection(NewDirCol^.At(j))^.Dta;
      if (AnsiCompareText(POneDir^.LongName, PSavedOneDir^.LongName)=0) and
        (AnsiCompareText(POneDir^.Ext, PSavedOneDir^.Ext)=0) then
          begin
          POneDir^.Description := PSavedOneDir^.Description;
          RecurseUpdateDesc(SavedTreeStruct, PDirCollection(SavedDirCol^.At(j)),
                            PDirCollection(NewDirCol^.At(i)), F);

          break;
          end;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// By using DLL for extraction reads descriptions from the ZIP, ACE and RAR archives

function  TTreeStruct.GetDescFromArchive(var F: TQBufStream;
                                         ArchivePath: ShortString;
                                         InternalPath: ShortString;
                                         FileName: ShortString;
                                         ExtractDll: PExtractDll;
                                         OffsetInArchive: longint;
                                         iFileSize: longint): longint;

  var
    i             : Integer;
    OneMask       : POneMask;
    TmpString     : ShortString;
    szTmpFileName : array[0..256] of char;
    szTmpPath     : array[0..256] of char;
    szArchivePath : array[0..256] of char;
    szFileName    : array[0..256] of char;
    {$ifdef DELPHI1}
    szWinExecParam: array[0..1024] of char;
    szNumber      : array[0..30] of char;
    {$endif}
    ExtractResult : longint;
    SizeToExtract : longint;


  begin
  Result := 0;
  if not PDatabase(OwnerDBase)^.LocalOptions.ScanDesc then exit;
  if CB_AbortScan then exit;
  if ExtractDll.iSizeLimit < (iFileSize div (1024*1024)) then
    exit; // the file is too large

  if ExtractDll <> nil then
    begin
    for i := 0 to pred(DescMaskCol^.Count) do
      begin
      OneMask := POneMask(DescMaskCol^.At(i));
      if MaskCompare (GetPQString(OneMask^.MaskName), FileName, false, true) then
         begin
         delete(InternalPath, 1, 1);
         {$ifndef DELPHI1}
         ///if (GetTempPath(255, szTmpPath) = 0) then exit; // we cannot get TMP
         GetTempFileName(szTmpPath, 'DskB', TmpFileNumber, szTmpFileName);
         {$else}
         GetTempFileName(#0, 'DskB', TmpFileNumber, szTmpFileName);
         {$endif}
         StrPCopy(szArchivePath, ArchivePath);
         StrPCopy(szFileName, InternalPath + FileName);
         CB_NewLineToIndicator(lsExtracting + FileName + lsFrom + ArchivePath);
         ExtractResult := -1;
         if OneMask^.ConvDll <> nil
           then SizeToExtract := OneMask^.MaxSize*10
           else SizeToExtract := OneMask^.MaxSize;
         try
         {$ifdef DELPHI1}
           StrPCopy(szWinExecParam, ExtractDll^.DllName);
           StrCat(szWinExecParam, ' ');
           StrCat(szWinExecParam, szArchivepath);
           StrCat(szWinExecParam, '?');
           StrCat(szWinExecParam, szFileName);
           StrCat(szWinExecParam, '?');
           StrCat(szWinExecParam, szTmpFileName);
           StrCat(szWinExecParam, '?');
           TmpString := IntToStr(SizeToExtract);
           StrPCopy(szNumber, TmpString);
           StrCat(szWinExecParam, szNumber);
           StrCat(szWinExecParam, '?');
           TmpString := IntToStr(OffsetInArchive);
           StrPCopy(szNumber, TmpString);
           StrCat(szWinExecParam, szNumber);
           ExtractResult := WinExec(szWinExecParam, SW_HIDE);
           if ExtractResult >= 32
             then ExtractResult := 0
             else ExtractResult := -1;
         {$else}

         if not (ExtractDll^.bStatic)
           then
             ExtractResult := ExtractDll^.ExtractFile(szArchivepath, szFileName, szTmpFileName,
                                                      SizeToExtract,  OffsetInArchive)
           else
             begin
             case ExtractDll^.ArchiveType of
               atRar: begin
                      ExtractResult := ExtractRarFile(szArchivepath, szFileName, szTmpFileName,
                                                      SizeToExtract,  OffsetInArchive);
                      end;
               atAce: begin
                      ExtractResult := ExtractAceFile(szArchivepath, szFileName, szTmpFileName,
                                                      SizeToExtract,  OffsetInArchive);
                      end;
               end;
             end;
         {$endif}
         except
         {$ifdef DELPHI1}
           on E: EGPFault do
         {$else}
           on E: EAccessViolation do
         {$endif}
             begin
             ExtractResult := -1;
             TmpFileNumber := (TmpFileNumber + 1) mod 100;
             end;
           on E: EDivByZero do
             begin
             ExtractResult := -1;
             TmpFileNumber := (TmpFileNumber + 1) mod 100;
             end;
           on E: EInvalidPointer do
             begin
             ExtractResult := -1;
             TmpFileNumber := (TmpFileNumber + 1) mod 100;
             end;
           {else ;}
           end;
         if ExtractResult = 0 then
           begin
           TmpString := StrPas(szTmpFileName);
           Result := GetDescription(F, TmpString, FileName, true);
           DeleteFile(StrPas(szTmpFileName));
           end;
         break;
         end;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Deletes FileCol at Index, insert new DirItem and subitems - it is used
// after archive is scanned into a list, then this archive is put to
// original file list as a folder

function TTreeStruct.PutToDirAndFileCol (Col: PDirCollection; var F: TQBufStream;
                                           FileCol: PFileCollection; Index: Integer;
                                           ArcCol: PArcCollection;
                                           ArchiveType: TArchiveType;
                                           ArchivePath: ShortString): PDirCollection;

  var
    OneFile    : TOneFile;
    POneFile   : TPOneFile;
    i          : Integer;
    LastPath   : ShortString;
    BaseDirCol : PDirCollection;
    WrkDirCol  : PDirCollection;
    WrkFileCol : PFileCollection;
    TotalSize  : longword;
    ExtractDll : PExtractDll;

  begin
  Result := nil;
  ExtractDll := nil;
  // find conversion DLL
  for i := 0 to pred(ExtractDllCol^.Count) do
    if (ArchiveType = PExtractDll(ExtractDllCol^.At(i))^.ArchiveType) then
      begin
      ExtractDll := PExtractDll(ExtractDllCol^.At(i));
      break;
      end;
  if ArcCol^.Count = 0 then exit; // no files - it is not an archive
  MoveOneFile(TPOneFile(FileCol^.At(Index))^, OneFile);
  FileCol^.AtFree(Index);
  BaseDirCol := New(PDirCollection, Init(4, 32, OneFile.LongName, OneFile.Ext, OneFile.Time));
  BaseDirCol^.ParentCol  := Col;
  BaseDirCol^.Dta^.Size  := OneFile.Size;
  BaseDirCol^.Dta^.Time  := OneFile.Time;
  BaseDirCol^.Dta^.Attr  := BaseDirCol^.Dta^.Attr or faQArchive;
  Col^.Insert(BaseDirCol);
  Result := BaseDirCol;
  WrkFileCol := New(PFileCollection, Init(100, 200));
  LastPath := '\';
  WrkDirCol := BaseDirCol;
  for i := 0 to pred(ArcCol^.Count) do
    with POneArcItem(ArcCol^.At(i))^ do
      begin
      if LastPath <> Dir^ then
        begin
        if WrkFileCol^.Count > 0 then
          begin
          F.SeekToEnd;
          WrkDirCol^.Dta^.FileList := F.GetPos;
          TotalSize := 0;
          WrkFileCol^.StoreCol(F, TotalSize, true);
          WrkFileCol^.FreeAll;
          end;
        LastPath := Dir^;
        WrkDirCol := ChOrMkDir(BaseDirCol, LastPath); // here ArcDirs are counted
        end;

      inc(Dta.ArcFiles);
      Dta.ArchSizeBytes := Dta.ArchSizeBytes + Size;
      Dta.DataSizeBytes := Dta.DataSizeBytes + Size;

      // new usage of OneFile
      OneFile.Attr := Attr;
      OneFile.Time := Time;
      OneFile.Size := Size;
      OneFile.LongName := GetPQString(Name);
      SeparateExt(OneFile.LongName, OneFile.Ext);
      OneFile.SubDirCol := nil;
      if ExtractDll <> nil
        then OneFile.Description := GetDescFromArchive(F, ArchivePath, LastPath,
                                       OneFile.LongName+OneFile.Ext, ExtractDll,
                                       OffsetInArc, OneFile.Size)
        else OneFile.Description := 0;
      GetMemOneFile(POneFile, OneFile);
      inc(WrkDirCol^.Dta^.FilesCount);
      WrkDirCol^.Dta^.FilesTotalSize := WrkDirCol^.Dta^.FilesTotalSize + Size;
      WrkFileCol^.Insert(POneFile);
      end;
    if WrkFileCol^.Count > 0 then
      begin
      F.Seek(F.GetSize);
      WrkDirCol^.Dta^.FileList := F.GetPos;
      TotalSize := 0;
      WrkFileCol^.StoreCol(F, TotalSize, true);
      WrkFileCol^.FreeAll;
      end;
  Dispose(WrkFileCol, Done);
  end;

//-----------------------------------------------------------------------------
// Goes through the file collection and checks, if it can be an archive

function TTreeStruct.ScanForArchive (Col: PDirCollection; Path: ShortString;
                                     var F: TQBufStream; FileCol: PFileCollection;
                                     Index: Integer): PDirCollection;

  var
    ArchiveScanned : boolean;
    ArcCol         : PArcCollection;
    Name           : ShortString;
    LowerCaseName  : ShortString;
    Ext            : TExt;
    LowerCaseExt   : TExt;
    Size           : longint;
    ScanZipExeArchives  : boolean;
    ScanOtherExeArchives: boolean;
    ArchiveType         : TArchiveType;

  begin
  Result := nil;
  if CB_AbortScan then exit;
  ScanZipExeArchives := PDatabase(OwnerDBase)^.LocalOptions.ScanZipExeArchives;
  ScanOtherExeArchives := PDatabase(OwnerDBase)^.LocalOptions.ScanOtherExeArchives;
  Name := TPOneFile(FileCol^.At(Index))^.LongName;
  LowerCaseName := AnsiLowerCase(Name);
  Ext  := TPOneFile(FileCol^.At(Index))^.Ext;
  LowerCaseExt := AnsiLowerCase(Ext);
  Size := TPOneFile(FileCol^.At(Index))^.Size;
  ArchiveScanned := false; // avoid further checking the archive, if already scanned
  if (PDatabase(OwnerDBase)^.LocalOptions.ScanArchives <> cfNoScan) then
    begin
    ArcCol := new(PArcCollection, Init(200,200));
    AlreadyAsked   := false;

    // test exe
    if (ScanZipExeArchives or ScanOtherExeArchives) and (LowerCaseExt = '.exe') then
      begin
      if ScanArcPrompt(Path+Name+Ext) then
        begin
        if not ArchiveScanned then
          ArchiveScanned := ScanExeFile(ArcCol, Path+Name+Ext,
                                        ScanOtherExeArchives, ArchiveType);
        end;
      if ArchiveScanned then
        Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol, ArchiveType,
                                     Path+Name+Ext);
      end;

    {--- is it ARJ ? ---}
    if not ArchiveScanned and ((LowerCaseExt = '.arj') or
     ( (LowerCaseExt[0] = #4) and (LowerCaseExt[2] = 'a')
       and (LowerCaseExt[3] >= '0') and (LowerCaseExt[3] <= '9')
       and (LowerCaseExt[4] >= '0') and (LowerCaseExt[4] <= '9')
       )) then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanARJFile(ArcCol, Path+Name+Ext, 0);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atArj, Path+Name+Ext);
        end;
    {--- is it RAR ? ---}
    if not ArchiveScanned and ((LowerCaseExt = '.rar') or
     ( (LowerCaseExt[0] = #4) and (LowerCaseExt[2] = 'r')
       and (LowerCaseExt[3] >= '0') and (LowerCaseExt[3] <= '9')
       and (LowerCaseExt[4] >= '0') and (LowerCaseExt[4] <= '9')
       )) then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanRARFile(ArcCol, Path+Name+Ext, 0);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atRar, Path+Name+Ext);
        end;
    {--- is it ACE ? ---}
    if not ArchiveScanned and ((LowerCaseExt = '.ace')
     ) then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanACEFile(ArcCol, Path+Name+Ext, 0);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atAce, Path+Name+Ext);
        end;
    {--- is it LHarc ? ---}
    if not ArchiveScanned and
      ((LowerCaseExt = '.lzh') or (LowerCaseExt = '.lha') or (LowerCaseExt = '.ice')) then
      begin
      if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanLHarcFile(ArcCol, Path+Name+Ext, 0);
          end;
      if ArchiveScanned then
        Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                     atLha, Path+Name+Ext);
      end;
    {--- is it Norton, MS-Backup ? ---}
    // must be before AR6, because it has the same extension
    if not ArchiveScanned
         and (Length(LowerCaseName)=8)
         and (LowerCaseName[1] >= 'a') and (LowerCaseName[1] <= 'z')
         and (LowerCaseName[2] >= 'a') and (LowerCaseName[2] <= 'z')
         and (LowerCaseName[3] >= '0') and (LowerCaseName[3] <= '9')
         and (LowerCaseName[4] >= '0') and (LowerCaseName[4] <= '9')
         and (LowerCaseName[5] >= '0') and (LowerCaseName[5] <= '9')
         and (LowerCaseName[6] >= '0') and (LowerCaseName[6] <= '9')
         and (LowerCaseName[7] >= '0') and (LowerCaseName[7] <= '9')
         and (LowerCaseName[8] >= 'a') and (LowerCaseName[8] <= 'z')
         and (Length(LowerCaseExt)=4)
         and (LowerCaseExt[2] >= '0') and (LowerCaseExt[2] <= '9')
         and (LowerCaseExt[3] >= '0') and (LowerCaseExt[3] <= '9')
         and (LowerCaseExt[4] >= '0') and (LowerCaseExt[4] <= '9')
      then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanNBFile(ArcCol, Path+Name+Ext);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atOther, Path+Name+Ext);
        end;
    {--- is it DOS-Backup ? ---}
    if not ArchiveScanned and (LowerCaseName='control')
         and ((Length(LowerCaseExt)=4)
         and (LowerCaseExt[2] >= '0') and (LowerCaseExt[2] <= '9')
         and (LowerCaseExt[3] >= '0') and (LowerCaseExt[3] <= '9')
         and (LowerCaseExt[4] >= '0') and (LowerCaseExt[4] <= '9'))
      then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanBakFile(ArcCol, Path+Name+Ext);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atOther, Path+Name+Ext);
        end;
    {--- is it AR6 ? ---}
    {$ifdef CZECH}
    if not ArchiveScanned and ((LowerCaseExt = '.ar6') or
         ((Length(LowerCaseExt)=4)
         and (LowerCaseExt[2] = '0')
         and (LowerCaseExt[3] >= '0') and (LowerCaseExt[3] <= '9')
         and (LowerCaseExt[4] >= '0') and (LowerCaseExt[4] <= '9')
         and (LowerCaseName <> 'control')
         and (LowerCaseName <> 'backup')))
      then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanAR6File(ArcCol, Path+Name+Ext);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atOther, Path+Name+Ext);
        end;
    {$endif}
    {--- is it ZIP ? ---}
    if not ArchiveScanned and (LowerCaseExt = '.zip') then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanZipFile(ArcCol, Path+Name+Ext, 0);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atZip, Path+Name+Ext);
        end;
    {--- is it ARC ? ---}
    if not ArchiveScanned and (LowerCaseExt = '.arc') then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanArcFile(ArcCol, Path+Name+Ext);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atOther, Path+Name+Ext);
        end;
    {--- is it PC-Backup ? ---}
    if not ArchiveScanned and (LowerCaseName='pcbackup') and (LowerCaseExt = '.dir') then
        begin
        if not ArchiveScanned then
          begin
          if ScanArcPrompt(Path+Name+Ext) then
            ArchiveScanned := ScanPCBFile(ArcCol, Path+Name+Ext, false);
          end;
        if ArchiveScanned then
          Result := PutToDirAndFileCol(Col, F, FileCol, Index, ArcCol,
                                       atOther, Path+Name+Ext);
        end;

    Dispose(ArcCol, Done);
    end;
  if ArchiveScanned
    then inc(Dta.Archives)
    else Dta.DataSizeBytes := Dta.DataSizeBytes + Size; // calculate only in case it is not an archive

  end;

//-----------------------------------------------------------------------------
// Recursively finds directory in the tree

function TTreeStruct.RecurseFindDir (DirCol: PDirCollection; Path: ShortString): PDirCollection;

  var
    i      : Integer;
    SubDir : ShortString;

  begin
  Result := nil;
  if DirCol = nil then exit;
  if ShortCopy(Path, 1, 1) = '\' then ShortDelete(Path, 1, 1);
  if Path = '' then
    begin
    Result := DirCol;
    exit;
    end;
  i := Pos('\', Path);
  if i > 0
    then
      begin
      SubDir := ShortCopy(Path, 1, pred(i));
      ShortDelete (Path, 1, i);
      end
    else
      begin
      SubDir := Path;
      Path := '';
      end;
  if SubDir = '' then
    begin
    Result := DirCol;
    exit;
    end;
  for i := 0 to pred(DirCol^.Count) do
    begin
    if AnsiCompareText(SubDir, PDirCollection(DirCol^.At(i))^.Dta^.LongName +
      PDirCollection(DirCol^.At(i))^.Dta^.Ext)=0 then
        begin
        Result := RecurseFindDir(DirCol^.At(i), Path);
        break;
        end;
    end;
  end;

//-----------------------------------------------------------------------------
// Changes the directory in the tree, if does not exist, creates a new node for it

function TTreeStruct.ChOrMkDir (BaseCol: PDirCollection; DirName: ShortString): PDirCollection;


   function ChOrMkDirOne (OneDirName: ShortString; ActCol: PDirCollection): PDirCollection;

    var
      i         : Integer;
      NewDirCol : PDirCollection;
      Ext       : TExt;

    begin
    // first check if it exists
    for i := 0 to pred(ActCol^.Count) do
      with PDirCollection(ActCol^.At(i))^ do
        if AnsiCompareText(Dta^.LongName + Dta^.Ext, OneDirName)=0 then
          begin
          Result := ActCol^.At(i);
          exit;
          end;
    // not found, create one and switch to it
    SeparateExt(OneDirName, Ext);
    NewDirCol := New(PDirCollection, Init(4, 32, OneDirName, Ext, 0));
    NewDirCol^.ParentCol := ActCol;
    ActCol^.Insert(NewDirCol);
    Result := NewDirCol;
    inc(Dta.ArcDirs);  // counter of dirs in archives
    end;


  var
    i    : Integer;
    TmpSt: ShortString;
    Col  : PDirCollection;

  begin
  Col := BaseCol;
  if ShortCopy(DirName, 1, 1) = '\' then ShortDelete(DirName, 1, 1);
  i := pos('\', DirName);
  while i > 0 do
    begin
    TmpSt := ShortCopy(DirName, 1, pred(i));
    if TmpSt <> '' then Col := ChOrMkDirOne(TmpSt, Col);
    ShortDelete(DirName, 1, i);
    i := pos('\', DirName);
    end;
  ChOrMkDir := Col;
  end;

//-----------------------------------------------------------------------------
// Uses callback to display the dialog, asking the user how to continue with scanning
// the archive

function TTreeStruct.ScanArcPrompt (FName: ShortString) : boolean;

  begin
  case ScanStatus of
    cfNoScan: LastResponse := false;
    cfScan  : LastResponse := true;
    cfAsk   :
      if not AlreadyAsked
        then
          begin
          case CB_HowToContinue(FName) of
            cmYes   : LastResponse := true;
            cmNo    : LastResponse := false;
            cmYesAll: begin
                      LastResponse := true;
                      ScanStatus   := cfScan;
                      end;
            cmNoAll : begin
                      LastResponse := false;
                      ScanStatus   := cfNoScan;
                      end;
            end;
          AlreadyAsked := true;
          end;
    end; {case}
  ScanArcPrompt := LastResponse;
  end;

//=============================================================================
//=== TDatabase ===============================================================
//=============================================================================

 constructor TDatabase.Init;

  begin
  NeedUpdDiskWin    := false;
  NeedUpdTreeWin    := false;
  NeedUpdFileWin    := false;
  TreeStruct        := nil;
  SavedTreeStruct   := nil;
  DBaseName         := '';
  DisksCount        := 0;;
  DirsCount         := 0;
  FilesCount        := 0;
  BufDescCol        := nil;
  bReadOnly         := false;
  bRepairMode       := false;
  DatabaseOpened    := false;
  MarkAsReadOnly    := false;
  Expired           := 0;
  ErrorCounter      := 0;
  Header            := new(PHeader);
  KeyField          := new(PKeyField);
  fillchar (Header^, SizeOf(THeader), 0);
  fillchar (KeyField^, SizeOf(TKeyField), 0);
  KeyField^.KeySize := maxKeySize;
  KeyFieldModified  := false;
  FileSearchCol     := New(TPQCollection, Init(1000, 1000));
  MaskCol           := New(TPQCollection, Init(30, 100));
  ExcludeMaskCol    := New(TPQCollection, Init(30, 100));
  DirMaskCol        := New(TPQCollection, Init(30, 100));
  BufDescCol        := New(PBufDescCollection, Init(MaxBufDescColItems,0));
  InitLocalOptions;
  ClearUpdateFunct;
  ClearSendFunct;
  ClearNotifyFunct;
  fillchar(SecretData, sizeof(SecretData), 0);
  end;

//-----------------------------------------------------------------------------

destructor TDatabase.Done;

  begin
  CloseDatabase(false);
  if Header          <> nil then Dispose(Header);
  if KeyField        <> nil then Dispose(KeyField);
  if TreeStruct      <> nil then Dispose(TreeStruct, Done);
  if SavedTreeStruct <> nil then Dispose(SavedTreeStruct, Done);
  if FileSearchCol   <> nil then Dispose(FileSearchCol, Done);
  if MaskCol         <> nil then Dispose(MaskCol, Done);
  if ExcludeMaskCol  <> nil then Dispose(ExcludeMaskCol, Done);
  if DirMaskCol      <> nil then Dispose(DirMaskCol, Done);
  if BufDescCol      <> nil then Dispose(BufDescCol, Done);
  end;

//-----------------------------------------------------------------------------
// Initializes local options to default values

procedure TDatabase.InitLocalOptions;

  begin
  FillChar(LocalOptions, SizeOf(LocalOptions), 0);
  with LocalOptions do
    begin
    ScanArchives    := cfScan;
    ScanCPB         := cfNoScan;
    ScanDesc        := true;
    SimulateDiskInfo:= true;
    OverwriteWarning:= true;
    AllMasksArray[0]:= #0;
    StrCat(AllMasksArray, lsDefaultMasks);
    ScanZipExeArchives   := true;
    ExtractFromZips      := true;
    ExtractFromRars      := true;
    ExtractFromAces      := true;
    end;
  end;

//-----------------------------------------------------------------------------
// COmpares 2 keys - puts deleted keys always to the end

function TDatabase.KeyCompare (Key1, Key2: ShortString; Attr1, Attr2: Word): Integer;

  begin
  if ((Attr1 and kaDeleted) = kaDeleted)
     and ((Attr2 and kaDeleted) = 0) then
       begin
       KeyCompare := 1;  {Key 1 > Key2}
       exit;
       end;
  if ((Attr2 and kaDeleted) = kaDeleted)
     and ((Attr1 and kaDeleted) = 0) then
       begin
       KeyCompare := -1;  {Key1 < Key2}
       exit;
       end;
  KeyCompare := AnsiCompareText(Key1, Key2);
  end;

//-----------------------------------------------------------------------------
// Inserts a new key. Duplicates are enabled, this must be checked before using
// this methos

function TDatabase.KeyInsert (Position: TFilePointer; Attr: Word): boolean;

 var
   i: Integer;

   begin
   KeySearch(ReadKeyStr(Position), Attr, i);
   Result := KeyAtInsert(i, Position, Attr);
   end;

//-----------------------------------------------------------------------------
// Deletes a key.

procedure TDatabase.KeyDelete (var Key: ShortString);

 var
   i: Integer;

   begin
   if KeySearch(Key, 0, i) then KeyAtDelete(i);
   end;

//-----------------------------------------------------------------------------
// Searches for a key.

function TDatabase.KeySearch (Key: ShortString; Attr: Word; var Index: Integer): boolean;

  var
    Left, Right, Middle : Integer;
    QResult : Integer;

  begin
  Result := false;
  Index := 0;
  if KeyField^.CountValid = 0 then exit;
  Left  := 0;
  Right := KeyField^.CountValid - 1;
  while Left <= Right do
    begin
    Middle := (Left + Right) shr 1;
    QResult := KeyCompare(ReadKeyStr(KeyField^.Keys[Middle].Position), Key,
                          KeyField^.Keys[Middle].Attr, Attr);
    if QResult < 0 then Left := Middle + 1 else
      begin
      Right := Middle - 1;
      if QResult = 0 then
        begin
        Result := True;
        Left := Middle;
        end;
      end;
    end;
  Index := Left;
  end;

//-----------------------------------------------------------------------------
// Inserts a key

function TDatabase.KeyAtInsert (Index: Integer; Position: TFilePointer;
                                Attr: Word): boolean;

  begin
  KeyAtInsert := false;
  // if we are full, delete the last marked as deleted
  if KeyField^.Count = KeyField^.KeySize then
    if KeyField^.Keys[KeyField^.Count-1].Attr and kaDeleted <> 0
      then dec(KeyField^.Count)
      else exit;

  if (Index < 0) then
    raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (006)');
  if (Index < KeyField^.Count) then
    move(KeyField^.Keys[Index], KeyField^.Keys[Index+1],
         (KeyField^.Count-Index)*SizeOf(TKeyRecord));
  KeyField^.Keys[Index].Position := Position;
  KeyField^.Keys[Index].Attr     := Attr;
  KeyField^.Focused := Index;
  inc(KeyField^.Count);
  if Attr and kaDeleted = 0 then inc(KeyField^.CountValid);
  KeyFieldModified  := true;
  NeedUpdDiskWin := true;
  KeyAtInsert := true;
  end;

//-----------------------------------------------------------------------------
// Marks the key as deleted and puts it back to the database

procedure TDatabase.KeyAtDelete (Index: Integer);

  var
    TmpKeyRecord : TKeyRecord;

  begin
  if KeyField^.Count = 0 then exit;
  if Index >= KeyField^.Count then exit;
  if (Index < 0) then
    raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (007)');
  TmpKeyRecord := KeyField^.Keys[Index];
  if Index < (KeyField^.Count-1) then
    move(KeyField^.Keys[Index+1], KeyField^.Keys[Index],
         (KeyField^.Count-Index-1)*SizeOf(TKeyRecord));
  dec(KeyField^.Count);
  if TmpKeyRecord.Attr and kaDeleted = 0 then dec(KeyField^.CountValid);
  // and resort
  TmpKeyRecord.Attr := TmpKeyRecord.Attr or kaDeleted;
  TmpKeyRecord.Attr := TmpKeyRecord.Attr and not kaSelected;
  KeyInsert(TmpKeyRecord.Position, TmpKeyRecord.Attr);
  KeyFieldModified  := true;
  NeedUpdDiskWin := true;
  end;

//-----------------------------------------------------------------------------
// Gets out and puts in the key, so that it is newly sored. Used when renaming
// or deleting keys.

procedure TDatabase.KeyAtResort (Index: Integer);

  var
    TmpKeyRecord : TKeyRecord;

  begin
  if KeyField^.Count = 0 then exit;
  if Index > KeyField^.Count-1 then exit;
  TmpKeyRecord := KeyField^.Keys[Index];
  if (Index < 0) then
    raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (008)');
  if Index < (KeyField^.Count-1) then
    move(KeyField^.Keys[Index+1], KeyField^.Keys[Index],
         (KeyField^.Count-Index-1)*SizeOf(TKeyRecord));
  dec(KeyField^.Count);
  if TmpKeyRecord.Attr and kaDeleted = 0 then dec(KeyField^.CountValid);
  KeyInsert(TmpKeyRecord.Position, TmpKeyRecord.Attr);
  KeyFieldModified := true;
  NeedUpdDiskWin := true;
  end;

//-----------------------------------------------------------------------------
// Searches for a key.

function TDatabase.KeyStrSearch (StrKey: ShortString; var Index: Integer): Boolean;

  begin
  KeyStrSearch := KeySearch(StrKey, 0, Index);
  end;

//-----------------------------------------------------------------------------
// According to ShowDeleted returns either count of all or count of not-deleted keys

function TDatabase.GetCountToShow: Integer;

  begin
  if LocalOptions.ShowDeleted
     then GetCountToShow := KeyField^.Count
     else GetCountToShow := KeyField^.CountValid;
  end;

//-----------------------------------------------------------------------------
// Returns count of keys masrked as selected.

function TDatabase.GetSelectedCount: longint;

  var
    i     : longint;

  begin
  Result := 0;
  for i := 0 to KeyField^.CountValid - 1 do
    if KeyField^.Keys[i].Attr and kaSelected <> 0 then inc(Result);
  end;

//-----------------------------------------------------------------------------
// Returns the index of first selected key.

function TDatabase.GetFirstSelectedIndex: longint;

  var
    i     : longint;

  begin
  Result := -1;
  for i := 0 to KeyField^.CountValid - 1 do
    if KeyField^.Keys[i].Attr and kaSelected <> 0 then
      begin
      Result := i;
      break;
      end;
  end;

//-----------------------------------------------------------------------------
// Sets the database header to default values

procedure TDatabase.SetHeader;

  begin
  fillchar(Header^, SizeOf(THeader), 0);
  with Header^ do
    begin
    QDirLabel :=  'QuickDir Database ';
    Version       := DataFormatVersion;  {version in hi byte, subversion in lo}
    VersionNeeded := DataFormatVersion;
    SecretData1   := SecretData;
    SecretData2   := SecretData1;
    end;
  end;

//-----------------------------------------------------------------------------
//  Creates and closes the database. Must be opened before usage.

function TDatabase.CreateDatabase(aDBaseName: ShortString): boolean;

  var
    TmpStrArr : array[0..256] of char;

  begin
  DBaseName := aDBaseName;
  DataFile.Init(StrPCopy(TmpStrArr, DBaseName), stCreate);
  DatabaseOpened := true;
  SetHeader;
  fillchar(KeyField^, SizeOf(TKeyField), 0);
  KeyField^.KeySize := maxKeySize;
  KeyFieldModified  := true; {aby se zapsalo KeyField}
  CreateDatabase := CloseDatabase(true);
  end;

//-----------------------------------------------------------------------------
// Scans the database for records to build a key index. Copies correct data to
// the output database.

procedure TDatabase.RepairKeys;

  const
    BufSize = 32*1024;

  type
    TBuffer = array[0..BufSize-1] of byte;

  var
    RecordHeaderBytes: array[1..sizeof(TRecordHeader)+4] of byte;
    {+ 4 for TreeAttr}
    RecordHeader : TRecordHeader absolute RecordHeaderBytes;
    AttrByte: byte;

    pBuffer   : ^TBuffer;
    iBufPos   : longword;
    iBufRead  : longword;
    iBufOffset: longword;

    FoundRestoredDisks: boolean;
    Index     : longint;
    DiskName  : ShortString;

  begin
  fillchar(KeyField^, SizeOf(TKeyField), 0);
  KeyField^.KeySize := maxKeySize;
  KeyFieldModified := true;
  DataFile.Seek(SizeOf(TKeyField)+SizeOf(THeader)+SizeOf(TLocalOptions));
  GetMem(pBuffer, BufSize);

  iBufRead := DataFile.GetSize - DataFile.GetPos;
  if iBufRead > BufSize then iBufRead := BufSize;
  iBufOffset := DataFile.GetPos;
  DataFile.Read(pBuffer^, iBufRead);
  move (pBuffer^, RecordHeaderBytes, SizeOf(RecordHeader)+4);
  iBufPos := SizeOf(RecordHeader)+4;

  while iBufRead > 0 do
    begin
    //Log('KeyField.Focused = %d', [KeyField^.Focused]);
    //Log('KeyField.Count = %d, KeyField.CountValid = %d', [KeyField.Count, KeyField^.CountValid]);

    with RecordHeader do
      begin
      if (RecType = rtTree) and
         ((Size = not SizeInvert) or (Size = (not SizeInvert + 1)))
         and ((RecordHeaderBytes[sizeof(RecordHeader)+1] = 0)
           or (RecordHeaderBytes[sizeof(RecordHeader)+1] = kaDeleted))
         and (RecordHeaderBytes[sizeof(RecordHeader)+2] = 0)
         and (RecordHeaderBytes[sizeof(RecordHeader)+3] = 0)
         and (RecordHeaderBytes[sizeof(RecordHeader)+4] = 0)
         then
        begin
        AttrByte := RecordHeaderBytes[sizeof(RecordHeader)+1];
        KeyField^.Keys[KeyField^.Count].Position := iBufOffset + iBufPos - sizeof(RecordHeader) - 4;
        if (AttrByte and kaDeleted) <> 0 then
          begin
          KeyField^.Keys[KeyField^.Count].Attr := kaDeleted;
          inc(KeyField^.CountValid);
          end;
        inc(KeyField^.Count);
        end;
      end;
    move(RecordHeaderBytes[2], RecordHeaderBytes[1], sizeof(RecordHeader)+3);

    RecordHeaderBytes[sizeof(RecordHeader)+4] := pBuffer^[iBufPos];
    inc(iBufPos);
    if iBufPos >= iBufRead then
      begin
      iBufRead := DataFile.GetSize - DataFile.GetPos;
      if iBufRead > 0 then
        begin
        if iBufRead > BufSize then iBufRead := BufSize;
        iBufOffset := DataFile.GetPos;
        DataFile.Read(pBuffer^, iBufRead);
        iBufPos := 0;
        end;
      end;
    end;
  FreeMem(pBuffer, BufSize);

  // not sorted
  ResortKeys; // calls GetKeyStr - does not crash when there is an error in data

  // Till version 5.10 there was an error in disk restore - it was still marked
  // as deleted. We will try to guess, if the disk is still in the database -
  // if it has the Deleted attribute and does not have "]" at the end, it might be such case.

  FoundRestoredDisks := false;
  for Index := KeyField^.CountValid to pred(KeyField^.Count) do
    if (KeyField^.Keys[Index].Attr and kaDeleted) > 0 then
      begin
      DiskName := CP1250ToUtf8(ReadKeyStr(KeyField^.Keys[Index].Position));
      if copy(DiskName, length(DiskName), 1) <> ']' then
        begin
        FoundRestoredDisks := true;
        KeyField^.Keys[Index].Attr := KeyField^.Keys[Index].Attr and not kaDeleted;
        end;
      end;
   if FoundRestoredDisks then ResortKeys;

   end;

//-----------------------------------------------------------------------------
// Opens the database. Checks, in which mode the database can be open.

procedure TDatabase.OpenDatabase (aDBaseName: ShortString; bRepair: boolean);

  var
    TmpStrArr : array[0..256] of char;
    CheckResult: integer;

  begin
  bRepairMode := bRepair;
  CheckResult := CheckDatabase(aDBaseName);
  case CheckResult of
    cdNotQDirDBase   :
      raise EQDirNormalException.Create(lsNotAQDirDBase);
    cdOldVersion     :
      raise EQDirNormalException.Create(lsRequiresNewerVersion);
    cdHeaderCorrupted:
      raise EQDirNormalException.Create(lsCorruptedHeader);
    cdCannotRead:
      raise EQDirNormalException.Create(lsDBaseCannotBeOpened);
    end;

  KeyFieldModified  := false;
  DatabaseOpened    := false;
  DBaseName         := aDBaseName;

  // no error checking, because we do not read anything else than in CheckDatabase

  if (CheckResult = cdItIsRuntime) or (CheckResult = cdWriteProtected)
    then
      begin
      bReadOnly := true;
      DataFile.Init(StrPCopy(TmpStrArr, DBaseName), stOpenReadNonExclusive)
      end
    else
      begin
      bReadOnly := false;
      DataFile.Init(StrPCopy(TmpStrArr, DBaseName), stOpenExclusive);
      end;

  DataFile.CheckReading := true; // check seeks and reading extent of the database

  DataFile.Read(Header^, SizeOf(THeader));

  // this already was not read in CheckDatabase, so an exception can be thrown
  // in the repair more the exceptions are catched.

  if (bRepair)
    then
      begin
      // if this fails, we cannot do anything reasonable with this record
      DataFile.Read(LocalOptions, SizeOf(TLocalOptions));
      FillChar(KeyField^, SizeOf(TKeyField), 0);
      // here some exception would happen only in serious damage
      RepairKeys;

      {
      Log('KeyField.Focused = %d', [KeyField^.Focused]);
      Log('KeyField.Count = %d, KeyField.CountValid = %d', [KeyField.Count, KeyField^.CountValid]);
      Log('KeyField.CountDeleted = %d', [KeyField^.CountDeleted]);
      for i := 0 to pred(KeyField^.Count) do
        Log('Pos = %6d, Attr = %02x', [KeyField^.Keys[i].Position, KeyField^.Keys[i].Attr]);
      }
      end
    else
      begin
      DataFile.Read(LocalOptions, SizeOf(TLocalOptions));
      FillChar(KeyField^, SizeOf(TKeyField), 0);
      DataFile.Read(KeyField^, keyHeadSize);
      DataFile.Read(KeyField^.Keys, KeyField^.Count * SizeOf(TKeyRecord));
      ResortKeys;  // will not throw exception, only in HW failure
      end;

  KeyFieldModified := false; // this flag affects only the archive attribute
  DatabaseOpened := true;
  NeedUpdDiskWin := true;
  NeedUpdTreeWin := true;
  NeedUpdFileWin := true;
  end;

//-----------------------------------------------------------------------------
// Closes the database

function TDatabase.CloseDatabase (WriteCompleteKeyArr: boolean): boolean;

  var
    i: Integer;
    SaveModified: boolean;

  begin
  DBaseName := '';
  if DatabaseOpened then
    begin
    if not bReadOnly then
      begin
      for i := 0 to KeyField^.Count - 1 do
        KeyField^.Keys[i].Attr := KeyField^.Keys[i].Attr and not kaSelected;
      DataFile.Seek(0);
      SetHeader;
      SaveModified := DataFile.Modified;
      DataFile.Write(Header^, SizeOf(THeader));
      DataFile.Write(LocalOptions, SizeOf(TLocalOptions));
      DataFile.Modified := SaveModified;  // do not consider the previous 2 writes as modifying
      if WriteCompleteKeyArr // used when database is created
        then
          DataFile.Write(KeyField^, SizeOf(TKeyField))
        else
          begin
          SaveModified := DataFile.Modified;
          DataFile.Write(KeyField^, keyHeadSize);
          DataFile.Write(KeyField^.Keys, KeyField^.Count * SizeOf(TKeyRecord));
          DataFile.Modified := SaveModified;
          // KeyFieldModified indicates changes AFTER resorting the header
          if KeyFieldModified then DataFile.Modified := true;
          end;
      end;
    DataFile.Done;
    DatabaseOpened := false;
    end;
  CloseDatabase := true;
  end;

//-----------------------------------------------------------------------------
// Resort all the keys

procedure TDatabase.ResortKeys;

  var
    OrigKeyField: PKeyField;
    i: integer;

  begin
  OrigKeyField := KeyField;
  KeyField := new(PKeyField);
  fillchar (KeyField^, SizeOf(TKeyField), 0);
  // must be of the same size
  KeyField^.KeySize := OrigKeyField^.KeySize;
  for i := 0 to pred(OrigKeyField^.Count) do
    KeyInsert (OrigKeyField^.Keys[i].Position, OrigKeyField^.Keys[i].Attr);
  KeyField^.Focused := OrigKeyField^.Focused;
  dispose(OrigKeyField);
  end;

//-----------------------------------------------------------------------------
// Reads the key from database.

function TDatabase.ReadKeyStr (Position: TFilePointer): ShortString;

  begin
  try
    DataFile.Seek (Position + SizeOf(TRecordHeader) + treeInfoHeadSize);
    DataFile.Read (Result[0], 1);
    DataFile.Read (Result[1], length(Result));
    ///---
    //Result:=CP1250ToUtf8(Result);
  except
    on EQDirDBaseStructException do
      begin
      // could not read
      Result := '???';
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Reads the key from database and returns additional parameters

procedure TDatabase.ReadKeyStrEx (Position: TFilePointer; var DiskName,
                                  VolumeLabel, OriginPath: ShortString);

  var
    DataFormatVersion: Integer;
    RecordHeader: TRecordHeader;
    TreeInfo    : TTreeInfo;

  begin
  FillChar(TreeInfo, SizeOf(TreeInfo), 0);
  DataFile.Seek (Position);
  DataFile.Read (RecordHeader, SizeOf(TRecordHeader));
  DataFormatVersion := -1;
  if RecordHeader.Size = not RecordHeader.SizeInvert then DataFormatVersion := 0;
  if RecordHeader.Size = (not RecordHeader.SizeInvert + 1) then DataFormatVersion := 1;
  if RecordHeader.RecType <> rtTree then DataFormatVersion := -1;
  if DataFormatVersion < 0 then
    begin
    raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (009)');
    exit;
    end;
  if DataFormatVersion = 0
    then DataFile.Read (TreeInfo, SizeOfTreeInfoVer0)
    else DataFile.Read (TreeInfo, SizeOfTreeInfoVer1);

  DiskName    := TreeInfo.Name;
  VolumeLabel := TreeInfo.VolumeLabel;
  OriginPath  := TreeInfo.OriginPath;
  if OriginPath = '' then
    OriginPath := AnsiUpperCase(TreeInfo.OrigDrive[1] + ':');
  end;

//-----------------------------------------------------------------------------
// Reads the tree structure from the database to current tree

procedure TDatabase.ReadDBaseEntry (Position: TFilePointer; RWMode: TRWMode);

  begin
  if TreeStruct <> nil then Dispose(TreeStruct, Done);
  TreeStruct := nil;
  try
    DataFile.Seek(Position);
    TreeStruct := New(PTreeStruct, InitLoad(@Self, RWMode));
  except
    on EQDirDBaseStructException do
      begin
      TreeStruct := New(PTreeStruct, Init(@Self, true));
      inc(ErrorCounter);
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Reads the tree structure from the database to saved tree

procedure TDatabase.ReadDBaseEntryToSaved (Position: TFilePointer);

  begin
  if SavedTreeStruct <> nil then Dispose(SavedTreeStruct, Done);
  SavedTreeStruct := nil;
  DataFile.Seek(Position);
  SavedTreeStruct := New(PTreeStruct, InitLoad(@Self, rwWhole));
  end;

//-----------------------------------------------------------------------------
// Used only for deleting records

 function TDatabase.WriteToDBaseEntry (Position: TFilePointer; RWMode: TRWMode): boolean;

  begin
  Result := true;
  if bReadOnly then exit;
  DataFile.Seek(Position);
  TreeStruct^.StoreStruct(RWMode);
  Result := true;
  end;

//-----------------------------------------------------------------------------
// Adds a new record to the database

function TDatabase.AppendNewDBaseEntry(CheckDuplicates: boolean): boolean;

  var
    TmpKey : TKeyRecord;
    i      : Integer;
    KeyName: ShortString;
    KeyNameUnifier: string[5];
    Unifier: Integer;

  begin
  AppendNewDBaseEntry := false;
  if bReadOnly then exit;
  if CheckDuplicates then
    begin
    KeyNameUnifier := '';
    Unifier := 0;
    KeyName := TreeStruct^.Dta.Name;
    while KeySearch(KeyName+KeyNameUnifier, TmpKey.Attr, i) do
      begin
      inc(Unifier);
      KeyNameUnifier := ' ~' + IntToStr(Unifier);
      end;
    TreeStruct^.Dta.Name := KeyName+KeyNameUnifier;
    end;

  TmpKey.Attr := 0;
  TmpKey.Position := DataFile.GetSize;
  DataFile.SeekToEnd;
  if not TreeStruct^.StoreStruct (rwWhole) then exit;

  if not KeyInsert(TmpKey.Position, TmpKey.Attr) then
    begin
    raise EQDirNormalException.Create(lsDBaseIsFull);
    exit;
    end;

  for i := 0 to KeyField^.Count - 1 do
    KeyField^.Keys[i].Attr := KeyField^.Keys[i].Attr and not kaSelected;
  DataFile.Seek(SizeOf(THeader)+ SizeOf(TLocalOptions));
  DataFile.Write(KeyField^, keyHeadSize + KeyField^.Count * SizeOf(TKeyRecord));
                                 // careful when KeyField struture changes
  KeyFieldModified := false;
  DataFile.Flush;
  AppendNewDBaseEntry := true;
  end;

//-----------------------------------------------------------------------------
// Checks if the disk name matches the mask

procedure TDatabase.CheckDiskName (Disk: ShortString);

  var
    j          : Integer;
    FSearchItem: PFSearchItem;

  begin
  if StopProcess then exit;
  if not SearchDlgData.ScanDiskNames then exit;
  for j := 0 to pred(MaskCol^.Count) do
    begin
    if (MaskCompare (GetPQString(POneMask(MaskCol^.At(j))^.MaskName),
       Disk, SearchDlgData.CaseSensitive, SearchDlgData.StrictMask)) then
      begin
      if FileSearchCol^.Count >= SearchDlgData.MaxLines then
        begin
        StopProcess := true;
        break;
        end;
      FSearchItem := New(PFSearchItem,
                         EmptiesInit(Disk, 0, 0, ''));
      FileSearchCol^.Insert(FSearchItem);
      if (FileSearchCol^.Count < 100) or (FileSearchCol^.Count mod 10 = 0)
        then UpdateFunct(StopProcess);
      break;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Finds files according to specified mask

procedure TDatabase.FindFiles;

  var
    Index      : Integer;
    Disk       : ShortString;

  begin
  DisksCount  := 0;
  DirsCount   := 0;
  FilesCount  := 0;
  ErrorCounter := 0;
  StopProcess  := false;
  FileSearchCol^.FreeAll;
  for Index := 0 to pred(KeyField^.CountValid) do
    begin
    if (SearchDlgData.SearchIn = siWholeDBase) or
       (SearchDlgData.SearchIn = siActualDisk)
         and (Index = KeyField^.Focused) or
       (SearchDlgData.SearchIn = siSelectedDisks)
          and (KeyField^.Keys[Index].Attr and kaSelected <> 0) or
       (SearchDlgData.SearchIn = siNotSelectedDisks)
          and (KeyField^.Keys[Index].Attr and kaSelected = 0) then
      begin
      try
        ReadDBaseEntry (KeyField^.Keys[Index].Position, rwWhole);
        Disk := ReadKeyStr(KeyField^.Keys[Index].Position);
        CheckDiskName(Disk);
        RecurseFindFile (TreeStruct^.RootDirCol, Disk, 0);
      except
        on EQDirDBaseStructException do
          inc(ErrorCounter);
        end;
      inc(DisksCount);
      if TreeStruct <> nil then Dispose(TreeStruct, Done);
      TreeStruct := nil;
      UpdateFunct(StopProcess); // callback
      if StopProcess then break;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Used for finding a file in the tree - recusive scan

procedure TDatabase.RecurseFindFile (DirCol: PDirCollection; var Disk: ShortString;
                                     Level: integer);

  var
    TmpS: ShortString;

  procedure CheckOneFile (var OneFile: TOneFile; CurDirCol: PDirCollection);

  var
    j          : Integer;
    Found      : boolean;
    Buf        : PChar;
    DescOffset : longint;
    FSearchItem: PFSearchItem;

    begin
    if SearchDlgData.MoreOptions then
      with SearchDlgData do
        begin
        if (DateFrom > 0) and (OneFile.Time < DateFrom) then exit;
        if (DateTo   > 0) and (OneFile.Time > DateTo)   then exit;
        if (SizeFrom > 0) and (OneFile.Size < SizeFrom) then exit;
        if (SizeTo   > 0) and (OneFile.Size > SizeTo)   then exit;
        for j := 0 to pred(ExcludeMaskCol^.Count) do
         if (MaskCompare (GetPQString(POneMask(ExcludeMaskCol^.At(j))^.MaskName),
           OneFile.LongName + OneFile.Ext, false, true)) then
             exit;
        end;

    for j := 0 to pred(MaskCol^.Count) do
      begin
      Found := false;
      DescOffset := 0;
      if SearchDlgData.ScanFileNames and (OneFile.Attr and faDirectory = 0) and
        (MaskCompare (GetPQString(POneMask(MaskCol^.At(j))^.MaskName),
        OneFile.LongName + OneFile.Ext, SearchDlgData.CaseSensitive,
          SearchDlgData.StrictMask)) then
          Found := true;
      if SearchDlgData.ScanDirNames and (OneFile.Attr and faDirectory <> 0) and
        (MaskCompare (GetPQString(POneMask(MaskCol^.At(j))^.MaskName),
                      OneFile.LongName + OneFile.Ext, SearchDlgData.CaseSensitive,
                      SearchDlgData.StrictMask)) then
          Found := true;
      if SearchDlgData.ScanDesc and (OneFile.Description <> 0) then
        begin
        if LoadDescToBuf(OneFile.Description, Buf) then {ShortString is allocated here}
          begin
          DescOffset := MaskCompareBuf (GetPQString(POneMask(MaskCol^.At(j))^.MaskName),
             Buf, SearchDlgData.CaseSensitive, SearchDlgData.SearchAsAnsi, SearchDlgData.SearchAsOem);
          if DescOffset <> -1 then Found := true;
          StrDispose(Buf);
          end;
        end;
      if Found then
        begin
        if FileSearchCol^.Count >= SearchDlgData.MaxLines then
          begin
          StopProcess := true;
          break;
          end;
        if OneFile.LongName <> '..' then
          begin
          GetShortDesc(OneFile.Description, DescOffset, TmpS);
          FSearchItem := New(PFSearchItem,
                         FileInit(OneFile, Disk, CurDirCol^.GetFullDirPath,
                         TmpS));
          FileSearchCol^.Insert(FSearchItem);
          if (FileSearchCol^.Count < 100) or (FileSearchCol^.Count mod 10 = 0)
            then UpdateFunct(StopProcess);
          end;
        break;
        end;
      end;
    end;



    var
      i               : Integer;
      POneFile        : TPOneFile;
      OneFile         : TOneFile;
      SearchInThisDir : boolean;

  begin
  if StopProcess then exit;
  if not (SearchDlgData.ScanFileNames or
          SearchDlgData.ScanDirNames or
          SearchDlgData.ScanDesc ) then exit;

  if DirCol = nil then exit;
  with TreeStruct^ do
    begin
    CurDirCol := DirCol;

    SearchInThisDir := true;
    if (DirMaskCol^.Count > 0) then
      begin
      SearchInThisDir := false;
      TmpS := CurDirCol^.GetFullDirPath;
      for i := 0 to pred(DirMaskCol^.Count) do
        if (MaskCompare (GetPQString(POneMask(DirMaskCol^.At(i))^.MaskName),
           TmpS, false, true)) then
             begin
             SearchInThisDir := true;
             break;
             end;
      end;

    if SearchInThisDir then
      begin
      UpdateCurFileCol(fsAll);
      inc(DirsCount);
      for i := 0 to pred(CurFileCol^.Count) do
        begin
        inc(FilesCount);
        if FilesCount mod 1000 = 0 then UpdateFunct(StopProcess); {callback}
        POneFile := TPOneFile(CurFileCol^.At(i));
        MoveOneFile(POneFile^, OneFile);
        CheckOneFile(OneFile, CurDirCol);
        if StopProcess then break;
        end;
      end;

    if not StopProcess then
      for i := 0 to pred(DirCol^.Count) do
        if (SearchDlgData.ScanDirLevel = -1) or (SearchDlgData.ScanDirLevel > Level) then
          RecurseFindFile(DirCol^.At(i), Disk, Level+1);
    end;
  end;

//-----------------------------------------------------------------------------
// Used for print and export - the files are sent via iterate callback
// (SendOneFileFunct)

procedure TDatabase.IterateFiles (SearchIn: TSearchIn; eSendType: TSendType);

  var
    Index      : Integer;
    Disk       : ShortString;
    SavePath   : ShortString;

  begin
  DisksCount    := 0;
  DirsCount     := 0;
  FilesCount    := 0;
  MaxNameLength := 0;
  MaxNamePxWidth:= 0;
  NameLengthSum := 0;
  ErrorCounter  := 0;
  StopProcess   := false;
  if (SearchIn = siActualDiskDir)
    then
      begin
      if eSendType = stDataNotify then
        begin
        gDatabaseData.sDatabaseName  := ExtractFileName(DBaseName);
        gDatabaseData.sDatabasePath  := ExtractFilePath(DBaseName);
        gDatabaseData.iNumberOfDisks := KeyField.CountValid;
        OnDatabaseBegin;
        end;
      SavePath := TreeStruct^.CurDirCol^.GetFullDirPath;
      try
        Disk := ReadKeyStr(KeyField^.Keys[KeyField^.Focused].Position);
        if eSendType = stDataCallback then SendOneDiskFunct(Disk);
        if eSendType = stDataNotify then
          begin
          gDiskData.sDiskName              := Disk;
          gDiskData.iDiskSizeKb            := TreeStruct.Dta.DiskSizeKb;
          gDiskData.iDiskFreeKb            := TreeStruct.Dta.DiskFreeKb;
          gDiskData.iFolders               := TreeStruct.Dta.Dirs;
          gDiskData.iFiles                 := TreeStruct.Dta.Files;
          gDiskData.iArchives              := TreeStruct.Dta.Archives;
          gDiskData.iTotalFilesInArchives  := TreeStruct.Dta.ArcFiles;
          gDiskData.iTotalFoldersInArchives:= TreeStruct.Dta.ArcDirs;
          gDiskData.TotalDataSize          := TreeStruct.Dta.DataSizeBytes;
          gDiskData.PhysSize               := TreeStruct.Dta.PhysSizeBytes;
          gDiskData.ArchSize               := TreeStruct.Dta.ArchSizeBytes;
          gDiskData.sOriginalPath          := TreeStruct.Dta.OriginPath;
          gDiskData.sVolumelabel           := TreeStruct.Dta.Volumelabel;
          gDiskData.iScanDate              := TreeStruct.Dta.ScanDate;
          OnDiskBegin;
          end;
        RecurseIterateFile (TreeStruct^.CurDirCol, Disk, eSendType);
      except
        on EQDirDBaseStructException do
          inc(ErrorCounter);
        end;
      inc(DisksCount);
      TreeStruct^.CurDirCol := TreeStruct^.RecurseFindDir(TreeStruct^.RootDirCol,
                               SavePath);
      if eSendType = stDataNotify then
        begin
        OnDiskEnd;
        OnDatabaseEnd;
        end;
      end
    else
      begin
      if eSendType = stDataNotify then
        begin
        gDatabaseData.sDatabaseName   := ExtractFileName(DBaseName);
        gDatabaseData.sDatabasePath   := ExtractFilePath(DBaseName);
        gDatabaseData.iNumberOfDisks  := KeyField.CountValid;
        OnDatabaseBegin;
        end;
      for Index := 0 to pred(KeyField^.CountValid) do
        begin
        if (SearchIn = siSelectedDisks)
             and (KeyField^.Keys[Index].Attr and kaSelected <> 0) or
           (SearchIn = siNotSelectedDisks)
             and (KeyField^.Keys[Index].Attr and kaSelected = 0) or
           (SearchIn = siActualDisk)
             and (Index = KeyField^.Focused) then
          begin
          try
            ReadDBaseEntry (KeyField^.Keys[Index].Position, rwWhole);
            Disk := ReadKeyStr(KeyField^.Keys[Index].Position);
            if eSendType = stDataCallback then SendOneDiskFunct(Disk);
            if eSendType = stDataNotify then
              begin
              gDiskData.sDiskName              := Disk;
              gDiskData.iDiskSizeKb            := TreeStruct.Dta.DiskSizeKb;
              gDiskData.iDiskFreeKb            := TreeStruct.Dta.DiskFreeKb;
              gDiskData.iFolders               := TreeStruct.Dta.Dirs;
              gDiskData.iFiles                 := TreeStruct.Dta.Files;
              gDiskData.iArchives              := TreeStruct.Dta.Archives;
              gDiskData.iTotalFilesInArchives  := TreeStruct.Dta.ArcFiles;
              gDiskData.iTotalFoldersInArchives:= TreeStruct.Dta.ArcDirs;
              gDiskData.TotalDataSize          := TreeStruct.Dta.DataSizeBytes;
              gDiskData.PhysSize               := TreeStruct.Dta.PhysSizeBytes;
              gDiskData.ArchSize               := TreeStruct.Dta.ArchSizeBytes;
              gDiskData.sOriginalPath          := TreeStruct.Dta.OriginPath;
              gDiskData.sVolumelabel           := TreeStruct.Dta.Volumelabel;
              gDiskData.iScanDate              := TreeStruct.Dta.ScanDate;
              OnDiskBegin;
              end;
            RecurseIterateFile (TreeStruct^.RootDirCol, Disk, eSendType);
            if eSendType = stDataNotify then OnDiskEnd;
          except
            on EQDirDBaseStructException do
              inc(ErrorCounter);
            end;
          inc(DisksCount);
          if TreeStruct <> nil then Dispose(TreeStruct, Done);
          TreeStruct := nil;
          UpdateFunct(StopProcess); {callback}
          if StopProcess then break;
          end;
        end;
      if eSendType = stDataNotify then OnDatabaseEnd;
      end;
  end;

//-----------------------------------------------------------------------------
// Used in IterateFiles - recursively goes through files

procedure TDatabase.RecurseIterateFile (DirCol: PDirCollection;
                                        var Disk: ShortString;
                                        eSendType: TSendType);
    var
      i               : Integer;
      POneFile        : TPOneFile;
      OneFile         : TOneFile;
      TmpInt          : Integer;
      sTmp            : AnsiString;

  begin
  if StopProcess then exit;
  if DirCol = nil then exit;
  with TreeStruct^ do
    begin
    CurDirCol := DirCol;
    UpdateCurFileCol(fsAll);
    if eSendType = stDataCallback then SendOneDirFunct(CurDirCol^.GetFullDirPath);
    if eSendType = stDataNotify then
      begin
      sTmp := CurDirCol^.GetFullDirPath;
      gFolderData.sPathToFolder        := ExtractFilePath(sTmp);
      gFolderData.sFolderName          := ExtractFileName(sTmp);
      gFolderData.FolderDataSize       := CurDirCol^.SubTotals.DataFileSize;
      gFolderData.iFilesInFolder       := CurDirCol^.SubTotals.DataFiles;
      gFolderData.iFoldersInFolder     := CurDirCol^.SubTotals.DataDirs;
      gFolderData.PhysFolderDataSize   := CurDirCol^.SubTotals.PhysFileSize;
      gFolderData.iPhysFilesInFolder   := CurDirCol^.SubTotals.PhysFiles;
      gFolderData.iPhysFoldersInFolder := CurDirCol^.SubTotals.PhysDirs;
      OnFolderBegin;
      end;
    inc(DirsCount);
    for i := 0 to pred(CurFileCol^.Count) do
      begin
      inc(FilesCount);
      if FilesCount mod 100 = 0 then UpdateFunct(StopProcess); {callback}
      POneFile := TPOneFile(CurFileCol^.At(i));
      MoveOneFile(POneFile^, OneFile);
      TmpInt := length(OneFile.LongName);
      if MaxNameLength < TmpInt then MaxNameLength := TmpInt;
      inc(NameLengthSum, TmpInt);
      if eSendType = stDataCallback
        then
          begin
          if OneFile.LongName <> '..' then
            SendOneFileFunct(OneFile);
          end
        else
          begin
          GetNameWidthFunct(OneFile.LongName, TmpInt);
          if MaxNamePxWidth < TmpInt then
            MaxNamePxWidth := TmpInt;
          end;
      if (eSendType = stDataNotify) and (OneFile.LongName <> '..') then
        begin
        MoveOneFile(OneFile, gFileData.OneFile);
        OnFile;
        end;
      if StopProcess then break;
      end; // for
    if eSendType = stDataNotify then OnFolderEnd;

    if not StopProcess then
      for i := 0 to pred(DirCol^.Count) do
        RecurseIterateFile(DirCol^.At(i), Disk, eSendType);
    end;
  end;

//-----------------------------------------------------------------------------
// Used in RecurseCopy. Catches DBaseStructException and in case
// of exception increments ErrorCounter

function TDatabase.CopyDescription (var TarDataFile: TQBufStream;
                                    Position: TFilePointer): TFilePointer;
  const
    BufSize = 8*1024;
  type
    TBuffer = array[1..BufSize] of byte;

  var
    ReadCount   : longint;
    Cluster     : Integer;
    Buf         : ^TBuffer;
    RecordHeader: TRecordHeader;
    DescHeader  : TDescHeader;
    DescLen     : longint;

  begin
  try
    Result := 0;
    if Position = 0 then exit;
    DataFile.Seek(Position);
    DataFile.Read(RecordHeader, SizeOf(TRecordHeader));
    if (RecordHeader.SizeInvert <> not RecordHeader.Size) or
       (RecordHeader.RecType <> rtFileDesc) then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (010)');
    DescLen := RecordHeader.Size - SizeOf(TRecordHeader) - SizeOf(TDescHeader);
    DataFile.Read(DescHeader, SizeOf(TDescHeader));

    TarDataFile.SeekToEnd;
    Result := TarDataFile.GetPos;
    TarDataFile.Write (RecordHeader, SizeOf(TRecordHeader));
    TarDataFile.Write (DescHeader, SizeOf(TDescHeader));
    GetMem(Buf, BufSize);
    ReadCount := 0;
    while (ReadCount < DescLen) do
      begin
      if (DescLen - ReadCount) >= BufSize
        then Cluster := BufSize
        else Cluster := DescLen - ReadCount;
      DataFile.Read(Buf^, Cluster);
      TarDataFile.Write(Buf^, Cluster);
      ReadCount := ReadCount + Cluster;
      end;
    FreeMem(Buf, BufSize);
  except
    on EQDirDBaseStructException do
      begin
      Result := 0; // not properly copied, will be lost
      inc(ErrorCounter);
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Copies selected or all records to another database

function TDatabase.CopyDatabase (TargetDatabase: PDatabase; WholeDbase,
                                 CheckDuplicates, CopyLocalOptions: boolean): boolean;

  var
    Index     : Integer;
    Disk      : ShortString;
    WhatToCopy: TSearchIn;

  begin
  Result       := false;
  DisksCount   := 0;
  DirsCount    := 0;
  FilesCount   := 0;
  StopProcess  := false;
  ErrorCounter := 0;
  CB_UpdateProgressIndicator(1, 0);
  if CopyLocalOptions then
    TargetDatabase^.LocalOptions := LocalOptions;
  TargetDatabase^.SetHeader;
  KeyFieldModified := true;
  TargetDatabase^.KeyFieldModified := true;

  if WholeDBase
    then
      WhatToCopy := siWholeDBase
    else
     {check if are any selected = if not, set siActualDisk}
      begin
      WhatToCopy := siActualDisk;
      for Index := 0 to pred(KeyField^.CountValid) do
        if (KeyField^.Keys[Index].Attr and kaSelected <> 0) then
          begin
          WhatToCopy := siSelectedDisks;
          break;
          end;
      end;

  for Index := 0 to pred(KeyField^.CountValid) do
    begin
    if (WhatToCopy = siWholeDBase) or
       (WhatToCopy = siSelectedDisks)
          and (KeyField^.Keys[Index].Attr and kaSelected <> 0) or
       (WhatToCopy = siNotSelectedDisks)
          and (KeyField^.Keys[Index].Attr and kaSelected = 0) or
       (WhatToCopy = siActualDisk)
         and (Index = KeyField^.Focused) then
      begin
      try
        {in the following 2 lines DBaseStructException can happen by seek}
        ReadDBaseEntry (KeyField^.Keys[Index].Position, rwWhole);
        Disk := ReadKeyStr(KeyField^.Keys[Index].Position); // exception safe
        CB_NewLineToIndicator(Disk);

        RecurseCopy (TargetDatabase, TreeStruct^.RootDirCol);

        // now descriptions and files are saved, we must save the tree
        //  - move it to the target database and save it

        if TargetDatabase^.TreeStruct <> nil then
          begin
          Dispose(TargetDatabase^.TreeStruct);
          TargetDatabase^.TreeStruct := nil;
          end;

        TargetDatabase^.TreeStruct := TreeStruct;
        TreeStruct := nil;

        TargetDatabase^.TreeStruct^.OwnerDBase := TargetDatabase;
        if not TargetDatabase^.AppendNewDBaseEntry(CheckDuplicates) then exit;
                                                  // true = do not enable duplicates
      except
        on EQdirDBaseStructException do
          begin
          // exception - we could not overcome it.
          // so we continue with the next - about the exception we have info in ErrorCounter
          end;
        end;
      inc(DisksCount);
      if TreeStruct <> nil then Dispose(TreeStruct, Done);
      TreeStruct := nil;
      UpdateFunct(StopProcess); {callback}
      if StopProcess then exit;
      end;
    if KeyField^.CountValid > 0 then
      CB_UpdateProgressIndicator(1, (succ(Index) * 100) div KeyField^.CountValid);
    end;
  Result := not StopProcess;
  end;

//-----------------------------------------------------------------------------
// Recurses the files in order to copy items

procedure TDatabase.RecurseCopy (TargetDatabase: PDatabase;
                                 DirCol: PDirCollection);

    var
      i               : Integer;
      POneFile        : TPOneFile;
      POneDir         : TPOneDir;
      TotalSize       : longword;

  begin
  if StopProcess then exit;
  if DirCol = nil then exit;
  with TreeStruct^ do
    begin
    CurDirCol := DirCol;  // in the firts call it is RootDirCol
    if CurFileCol <> nil then
      begin
      Dispose(CurFileCol, Done);
      CurFileCol := nil;
      end;
    if CurDirCol^.Dta^.FileList <> 0
      then
        CurFileCol := New(PFileCollection,
                          InitLoad(DataFile, CurDirCol^.Dta^.FileList,
                                   true, ErrorCounter))
        // FileCollection.InitLoad catches exceptions DBaseStrut
      else
        CurFileCol := New(PFileCollection, Init(100,100));

    inc(DirsCount);

    // calculate count of files and the sizes - because of older records
    // in the database, which did not have it implemented
    CurDirCol^.Dta^.FilesCount := CurFileCol^.Count;
    CurDirCol^.Dta^.FilesTotalSize := 0;
    // and add to the isze of archives, which are stored as folders
    for i := 0 to pred(CurDirCol^.Count) do
      begin
      POneDir := TPOneDir(PDirCollection(CurDirCol^.At(i))^.Dta);
      if (POneDir^.Attr and faQArchive <> 0) then
        begin
        inc(CurDirCol^.Dta^.FilesCount);
        CurDirCol^.Dta^.FilesTotalSize := CurDirCol^.Dta^.FilesTotalSize +
                                          POneDir^.Size;
        end;
      end;

    // first copy descriptions
    for i := 0 to pred(CurFileCol^.Count) do
      begin
      inc(FilesCount);
      if FilesCount mod 1000 = 0 then UpdateFunct(StopProcess); {callback}
      POneFile := TPOneFile(CurFileCol^.At(i));
      CurDirCol^.Dta^.FilesTotalSize := CurDirCol^.Dta^.FilesTotalSize +
                                        POneFile^.Size;
      // CopyDescription catches StructExceptions
      POneFile^.Description := CopyDescription(TargetDatabase^.Datafile,
                                               POneFile^.Description);
      // returns position in the Target database
      if StopProcess then break;
      end;

    // next we save the list of files
    TargetDatabase^.Datafile.SeekToEnd;
    CurDirCol^.Dta^.FileList := TargetDatabase^.Datafile.GetPos;
    TotalSize := 0; // not used in FileCol
    CurFileCol^.StoreCol(TargetDatabase^.Datafile, TotalSize, true);

    // next we copy descriptions of folders
    for i := 0 to pred(CurDirCol^.Count) do
      begin
      POneDir := TPOneDir(PDirCollection(CurDirCol^.At(i))^.Dta);
      // CopyDescription catches StructException
      POneDir^.Description := CopyDescription(TargetDatabase^.Datafile,
                                               POneDir^.Description);
      if StopProcess then break;
      end;

    if not StopProcess then
      for i := 0 to pred(DirCol^.Count) do
        RecurseCopy(TargetDatabase, DirCol^.At(i));
    end;
  end;

//-----------------------------------------------------------------------------
// Finds disks according to free space

procedure TDatabase.FindEmpties;

  var
    Index       : Integer;
    FSearchItem : PFSearchItem;
    Disk        : ShortString;
    PercentPie  : Integer;

  begin
  DisksCount  := 0;
  DirsCount   := 0;
  FilesCount  := 0;
  PercentPie  := pred(KeyField^.CountValid) div 20 + 1;
  FileSearchCol^.FreeAll;
  for Index := 0 to pred(KeyField^.CountValid) do
    begin
    try
      ReadDBaseEntry (KeyField^.Keys[Index].Position, rwHead);
      Disk := ReadKeyStr(KeyField^.Keys[Index].Position);
      inc(DisksCount);
      FSearchItem := New(PFSearchItem,
                         EmptiesInit(Disk, TreeStruct^.Dta.DiskFreeKb,
                                     TreeStruct^.Dta.DiskSizeKb,  ''));
                                     // shortdesc not used yet
      FileSearchCol^.Insert(FSearchItem);
    except
      on EQdirDBaseStructException do
        begin
        // we will ignore this exception
        end;
      end;
    if Index mod PercentPie = 0 then UpdateFunct(StopProcess);
    if StopProcess then break;
    end;
  if TreeStruct <> nil then Dispose(TreeStruct, Done);
  TreeStruct := nil; // this forces load of the full struct, not only the head
  end;

//-----------------------------------------------------------------------------
// clears the update callback

procedure TDatabase.ClearUpdateFunct;

  begin
  UpdateFunct := DummyUpdateFunct;
  end;

//-----------------------------------------------------------------------------
// clears the send callbacks

procedure TDatabase.ClearSendFunct;

  begin
  SendOneDiskFunct  := DummySendOneDiskFunct;
  SendOneDirFunct   := DummySendOneDirFunct;
  SendOneFileFunct  := DummySendOneFileFunct;
  GetNameWidthFunct := DummyGetNameWidthFunct;
  end;

//-----------------------------------------------------------------------------
// clears the notify callbacks

procedure TDatabase.ClearNotifyFunct;

  begin
  OnDatabaseBegin   := DummyOnDatabaseBegin;
  OnDatabaseEnd     := DummyOnDatabaseEnd;
  OnDiskBegin       := DummyOnDiskBegin;
  OnDiskEnd         := DummyOnDiskEnd;
  OnFolderBegin     := DummyOnFolderBegin;
  OnFolderEnd       := DummyOnFolderEnd;
  OnFile            := DummyOnFile;
  end;

//-----------------------------------------------------------------------------
// Returns the short description

function TDatabase.GetShortDesc (ID: TFilePointer; Offset: longint; var ShortDesc: ShortString): boolean;

  var
    BufDescItem : PBufDescItem;
    RecordHeader: TRecordHeader;
    DescHeader  : TDescHeader;
    DescLen     : Integer;

  begin
  GetShortDesc := false;
  ShortDesc[0] := #0;
  if ID = 0 then exit;
  if Offset <= 0 then
    begin
    BufDescItem := BufDescCol^.FindDesc(ID);
    if BufDescItem <> nil then
      begin
      ShortDesc := BufDescItem^.ShortDesc;
      GetShortDesc := true;
      exit;
      end;
    end;
  ShortDesc[0] := #0;
  try
    DataFile.Seek(ID);
    DataFile.Read(RecordHeader, SizeOf(TRecordHeader));
    if (RecordHeader.SizeInvert <> not RecordHeader.Size) or
       (RecordHeader.RecType <> rtFileDesc) then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (011)');
    DescLen := RecordHeader.Size - SizeOf(TRecordHeader) - SizeOf(TDescHeader);
    if Offset > 0 then
      DescLen := DescLen - Offset;
    if DescLen > MaxShortDescSize then DescLen := MaxShortDescSize;
    DataFile.Read(DescHeader, SizeOf(TDescHeader));
    if Offset > 0 then
      DataFile.Seek(DataFile.GetPos + LongWord(Offset));
    DataFile.Read(ShortDesc[1], DescLen);
    ShortDesc[0] := char(DescLen);
    FastRemoveRedundSpaces(ShortDesc);

    if Offset <= 0 then
      BufDescCol^.AddItem(ID, ShortDesc);
    GetShortDesc := true;
  except
    on EQDirDBaseStructException do
      begin
      inc(ErrorCounter);
      GetShortDesc := false;
      ShortDesc[0] := #0;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Loads the full description to a buffer

function TDatabase.LoadDescToBuf (ID: TFilePointer; var Buf: PChar): boolean;

  var
    RecordHeader: TRecordHeader;
    DescHeader  : TDescHeader;
    DescLen     : longint;

  begin
  Result := false;
  Buf := nil;
  if ID = 0 then exit;
  try
    DataFile.Seek(ID);
    DataFile.Read(RecordHeader, SizeOf(TRecordHeader));
    if (RecordHeader.SizeInvert <> not RecordHeader.Size) or
       (RecordHeader.RecType <> rtFileDesc) then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (012)');
    DescLen := RecordHeader.Size - SizeOf(TRecordHeader) - SizeOf(TDescHeader);
    if DescLen > 65000 then DescLen := 65000;
    DataFile.Read(DescHeader, SizeOf(TDescHeader));
    Buf := StrAlloc(DescLen+1);
    DataFile.Read(Buf[0], DescLen);
    Buf[DescLen] := #0;
    Result := true;
  except
    on EQDirDBaseStructException do
      begin
      inc(ErrorCounter);
      Result := false;
      if (Buf <> nil) then StrDispose(Buf);
      Buf := nil;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Renames FileName in the database
//
//function TDatabase.RenameFile (var OrigFilePointer: TFilePointer;
//                                  Buf: PChar): boolean;
//  begin
//
//
//  end;

//-----------------------------------------------------------------------------
// Deletes a File from the database
//
procedure TDatabase.DeleteFile (var fileName: ShortString);

  var
    Key     : TKeyRecord;
    dummy   : longword;
    i       : integer;
    filesize: integer;

  begin

  with TreeStruct.CurFileCol^ do begin
  // remove all dirs (and the file that should be deleted) from 'CurFileCol'-lection
     i:=0;
     repeat
       with TPOneFile(At(i))^ do begin
         if (SubDirCol <> nil) then
           AtDelete(i)
         else begin
           if (LongName=fileName) then begin

          // update statistics  NOT REALLY WORKING RIGHT NOW
             filesize:=Size;
//             with TreeStruct.CurDirCol.Dta^ do begin
//                 dec(Files);
//                 dec(FilesCount);
//
//                 FilesTotalSize := FilesTotalSize - filesize;
//                 PhysSizeBytes  := PhysSizeBytes - filesize;
//                 DataSizeBytes := DataSizeBytes - filesize;
//
//                 DataFile.Seek(SelfFilePos);
//             end;
//             TreeStruct.StoreStruct(rwHead);

         // Delete the file
            AtDelete(i);
           end

         else
         // move to next file record
            inc(i);
         end;
       end;  // with TreeStruct.CurFileCol(i)
     until ( i >= Count );

  // write back Change data to database
    DataFile.Seek(TreeStruct.CurDirCol.Dta^.FileList);
    StoreCol(Datafile, dummy, True );

    end; // with
  end;  // begin



//-----------------------------------------------------------------------------
// Saves the description to the database

function TDatabase.SaveBufToDesc (var OrigFilePointer: TFilePointer;
                                  Buf: PChar): boolean;

  var
    DescLen       : longint;
    RecordHeader  : TRecordHeader;
    DescHeader    : TDescHeader;
    NewFilePointer: TFilePointer;
    OneFile       : TOneFile;

  begin
  Result := false;
  if OrigFilePointer = 0 then exit;

  if (Buf <> nil)
    then
      begin
      {write the description}
      DescLen := StrLen(Buf);
      RecordHeader.Size := SizeOf(TRecordHeader) + SizeOf(TDescHeader) + DescLen;
      RecordHeader.SizeInvert := not RecordHeader.Size;
      RecordHeader.RecType    := rtFileDesc;
      DataFile.SeekToEnd;
      NewFilePointer := DataFile.GetPos;
      DataFile.Write (RecordHeader, SizeOf(TRecordHeader));
      DescHeader.Attr := 0;
      DescHeader.Time := 0; // not used yet
      DataFile.Write (DescHeader, SizeOf(TDescHeader)); // not used yet
      DataFile.Write (Buf[0], DescLen);
      if DataFile.Status <> stOK then exit;
      end
    else
      NewFilePointer := 0;

  // update the header
  DataFile.Seek(OrigFilePointer);
  DataFile.Read(OneFile.Description, oneFileHeadSize);
  OneFile.Description := NewFilePointer;
  if NewFilePointer = 0
    then OneFile.Attr := OneFile.Attr and not faQDescModified
    else OneFile.Attr := OneFile.Attr or faQDescModified;
  DataFile.Seek(OrigFilePointer);
  DataFile.Write(OneFile.Description, oneFileHeadSize);
  DataFile.Flush;
  if DataFile.Status <> stOK then exit;
  OrigFilePointer := NewFilePointer; {vraci zpet}
  Result := true;
  end;

//-----------------------------------------------------------------------------
// Adds new reference to the description to CurDirCol, CurFileCol and Tree.
// Used after the new description was assigned to OneFile record.

procedure TDatabase.UpdateDescInCurList (POneFile: TPOneFile);


  var
    TmpPFile: TPOneFile;
    i       : Integer;
    TmpPDir : PDirCollection;

  begin
  if POneFile = nil then exit;

  for i := 0 to pred(TreeStruct^.CurDirCol^.Count) do
    begin
    TmpPDir := TreeStruct^.CurDirCol^.At(i);
    if POneFile^.SelfFilePos = TmpPDir^.Dta^.SelfDirPos then
      begin
      TmpPDir^.Dta^.Description := POneFile^.Description;
      exit;
      end;
    end;

  for i := 0 to pred(TreeStruct^.CurFileCol^.Count) do
    begin
    TmpPFile := TreeStruct^.CurFileCol^.At(i);
    if POneFile^.SelfFilePos = TmpPFile^.SelfFilePos then
      begin
      TmpPFile^.Description := POneFile^.Description;
      exit;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Returns database info in the DBaseInfo structure

procedure TDatabase.GetDBaseInfo (var DBaseInfo: TDBaseInfo);

  var
    Index: integer;

  begin
  with DBaseInfo do
    begin
    DBaseSize  := DataFile.GetSize;
    Count      := KeyField^.Count;
    CountValid := KeyField^.CountValid;
    ReadOnly   := bReadOnly;
    iSelectedDisks    := 0;
    iNotSelectedDisks := 0;
    for Index := 0 to pred(KeyField^.CountValid) do
      begin
      if (KeyField^.Keys[Index].Attr and kaSelected <> 0) then inc(iSelectedDisks);
      if (KeyField^.Keys[Index].Attr and kaSelected  = 0) then inc(iNotSelectedDisks);
      end;
    end;
  end;

//-----------------------------------------------------------------------------


end.
