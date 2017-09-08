unit UApi;
(*====================================================================
API layer between visual components and the DiskBase Engine. The API
functions are prefixed with QI_
======================================================================*)

{$A-}

interface

uses UTypes, UApiTypes, UCollections;

{Callback functions}
procedure QI_RegisterCallbacks (RIsTheLastCPB: TIsTheLastCPB;
                                RHowToContinue: THowToContinue;
                                RNewLineToIndicator: TNewLineToIndicator;
                                RAppendLIneToIndicator: TAppendLineToIndicator;
                                RUpdateProgressIndicator: TUpdateProgressIndicator;
                                RAbortScan: TAbortScan);
procedure QI_UnregisterCallbacks;

{Database Functions}
function  QI_CheckDatabase       (DBaseName: ShortString): integer;
function  QI_CreateDatabase      (DBaseName: ShortString): boolean;
function  QI_OpenDatabase        (DBaseName: ShortString; bRepair: boolean;
                                  var DBHandle: PDBaseHandle;
                                  var sMessage: ShortString): boolean;
function  QI_CloseDatabase       (var DBHandle: PDBaseHandle; MarkAsReadOnly: boolean): boolean;
procedure QI_ClearTreeStruct     (DBHandle: PDBaseHandle);
procedure QI_ClearSavedTreeStruct(DBHandle: PDBaseHandle);
procedure QI_SaveTreeStruct      (DBHandle: PDBaseHandle);
procedure QI_RestoreTreeStruct   (DBHandle: PDBaseHandle);
function  QI_DatabaseIsOpened    (DBHandle: PDBaseHandle): boolean;
function  QI_KeySearch           (DBHandle: PDBaseHandle; var Key: ShortString; var Index: Integer): boolean;
procedure QI_ReadDBaseEntry      (DBHandle: PDBaseHandle; Position: longint;
                                  RWMode: TRWMode);
procedure QI_ReadDBaseEntryToSaved (DBHandle: PDBaseHandle; Index: Integer);
function  QI_DeleteDBaseEntry    (DBHandle: PDBaseHandle; Index: Integer): boolean;
function  QI_DeleteSelectedDBaseEntries (DBHandle: PDBaseHandle): boolean;
function  QI_RenameDBaseEntry    (DBHandle: PDBaseHandle; Index: Integer;
                                  NewName: ShortString; Undelete: boolean): boolean;
function  QI_AppendNewDBaseEntry (DBHandle: PDBaseHandle): boolean;
procedure QI_SetNewKeyNameToTree (DBHandle: PDBaseHandle; KeyName: ShortString);
function  QI_ScanDisk            (DBHandle: PDBaseHandle;
                                  Path, DiskName, VolumeLabel: ShortString): boolean;
function  QI_GetTreeInfo         (DBHandle: PDBaseHandle; var TreeInfo: TTreeInfo): boolean;
function  QI_GetFullDirPath      (DBHandle: PDBaseHandle; var Path: ShortString): boolean;
function  QI_IsInArchive         (DBHandle: PDBaseHandle): boolean;
function  QI_GetArchivePath      (DBHandle: PDBaseHandle): ShortString;
function  QI_GetCurDirCol        (DBHandle: PDBaseHandle): PDirColHandle;
function  QI_GetCurDirColSubTotals   (DBHandle: PDBaseHandle; var SubTotals: TSubTotals): boolean;
function  QI_GetRootDirCol       (DBHandle: PDBaseHandle): PDirColHandle;
function  QI_GetFileCol          (DBHandle: PDBaseHandle): PFileColHandle;
procedure QI_SetCurDirCol        (DBHandle: PDBaseHandle; DirColHandle: PDirColHandle);
procedure QI_UpdateFileCol       (DBHandle: PDBaseHandle);
function  QI_GetCurrentKey       (DBHandle: PDBaseHandle; var Key: ShortString;
                                  var Attr: Word; var Index: Integer): boolean;
function  QI_GetCurrentKeyEx     (DBHandle: PDBaseHandle;
                                  var DiskName, VolumeLabel, OrigPath: ShortString;
                                  var Attr: Word; var Index: Integer): boolean;
function  QI_GetKeyAt            (DBHandle: PDBaseHandle; var Key: ShortString;
                                  var Attr: Word; Index: Integer): boolean;
function  QI_GetShortDesc        (DBHandle: PDBaseHandle; ID: TFilePointer;
                                  var ShortDesc: ShortString): boolean;
function  QI_GetSelectionFlag    (DBHandle: PDBaseHandle; Index: Integer): boolean;
procedure QI_SetSelectionFlag    (DBHandle: PDBaseHandle; Selected: boolean; Index: Integer);
procedure QI_ToggleSelectionFlag (DBHandle: PDBaseHandle; Index: Integer);
function  QI_GetSelectedCount    (DBHandle: PDBaseHandle): longint;
function  QI_SetCurrentKeyPos    (DBHandle: PDBaseHandle; Index: Integer): boolean;
function  QI_GetCountToShow      (DBHandle: PDBaseHandle): Integer;
function  QI_IsCorrectTreeLoaded (DBHandle: PDBaseHandle): boolean;
function  QI_GoToDir             (DBHandle: PDBaseHandle; Path: ShortString): boolean;
function  QI_GoToKey             (DBHandle: PDBaseHandle; Key: ShortString): boolean;
function  QI_GoToKeyAt           (DBHandle: PDBaseHandle; KeyPos: Integer): boolean;
function  QI_ReloadTreeStruct    (DBHandle: PDBaseHandle; LeaveNil: boolean): boolean;

function  QI_NeedUpdDiskWin      (DBHandle: PDBaseHandle): boolean;
function  QI_NeedUpdTreeWin      (DBHandle: PDBaseHandle): boolean;
function  QI_NeedUpdFileWin      (DBHandle: PDBaseHandle): boolean;
procedure QI_ClearNeedUpdDiskWin (DBHandle: PDBaseHandle);
procedure QI_ClearNeedUpdTreeWin (DBHandle: PDBaseHandle);
procedure QI_ClearNeedUpdFileWin (DBHandle: PDBaseHandle);
procedure QI_SetNeedUpdDiskWin   (DBHandle: PDBaseHandle);
procedure QI_SetNeedUpdTreeWin   (DBHandle: PDBaseHandle);
procedure QI_SetNeedUpdFileWin   (DBHandle: PDBaseHandle);

procedure QI_SetLocalOptions     (DBHandle: PDBaseHandle; LocalOptions: TLocalOptions);
procedure QI_GetLocalOptions     (DBHandle: PDBaseHandle; var LocalOptions: TLocalOptions);

function  QI_GetDBaseFileName    (DBHandle: PDBaseHandle): ShortString;
function  QI_LoadDescToBuf       (DBHandle: PDBaseHandle; ID: TFilePointer;
                                  var Buf: PChar): boolean;
function  QI_SaveBufToDesc       (DBHandle: PDBaseHandle; var OrigFilePointer: TFilePointer;
                                  Buf: PChar): boolean;

procedure QI_DeleteFile (DBHandle: PDBaseHandle; var fileName: ShortString);

procedure QI_UpdateDescInCurList (DBHandle: PDBaseHandle; POneFile: TPOneFile);
procedure QI_GetDBaseInfo        (DBHandle: PDBaseHandle; var DBaseInfo: TDBaseInfo);
function  QI_DBaseExpired        (DBHandle: PDBaseHandle): integer;

{DirCollection functions}
function  QI_GetDirColCount   (DCHandle: PDirColHandle): Integer;
function  QI_GetOneDirDta     (DCHandle: PDirColHandle; var OneDir: TOneDir): boolean;
function  QI_GetDirColAt      (DCHandle: PDirColHandle; Index: Integer): PDirColHandle;

{FileCollection functions}
function  QI_GetFileColCount   (FCHandle: PFileColHandle): Integer;
function  QI_GetOneFileFromCol (FCHandle: PFileColHandle; Index: Integer;
                                var OneFile: TOneFile): boolean;

{FileSearch functions}
procedure QI_ClearSearchCol    (DBHandle: PDBaseHandle);
function  QI_SearchItemsAvail  (DBHandle: PDbaseHandle): boolean;
procedure QI_SetSearchCallback (DBHandle: PDBaseHandle;
                                UpdateFunct: TCallBackProc);
procedure QI_DelSearchCallback (DBHandle: PDBaseHandle);
procedure QI_FindFiles         (DBHandle: PDBaseHandle; SearchDlgData: TSearchDlgData);

procedure QI_SetIterateCallback (DBHandle: PDBaseHandle;
                                  UpdateFunct      : TCallBackProc;
                                  SendOneDiskFunct : TSendOneDiskFunct;
                                  SendOneDirFunct  : TSendOneDirFunct;
                                  SendOneFileFunct : TSendOneFileFunct;
                                  GetNameWidthFunct: TGetNameWidthFunct);
procedure QI_DelIterateCallback (DBHandle: PDBaseHandle);

procedure QI_SetNotifyProcs     (DBHandle: PDBaseHandle;
                                 UpdateFunct       : TCallBackProc;
                                 OnDatabaseBegin   : TNotifyProc;
                                 OnDatabaseEnd     : TNotifyProc;
                                 OnDiskBegin       : TNotifyProc;
                                 OnDiskEnd         : TNotifyProc;
                                 OnFolderBegin     : TNotifyProc;
                                 OnFolderEnd       : TNotifyProc;
                                 OnFile            : TNotifyProc);

procedure QI_DelNotifyProcs     (DBHandle: PDBaseHandle);

procedure QI_IterateFiles       (DBHandle: PDBaseHandle; SearchIn: TSearchIn;
                                 eSendType: TSendType);

procedure QI_FindEmpties       (DBHandle: PDBaseHandle);

function  QI_GetSearchItemAt   (DBHandle: PDBaseHandle; Index: Integer;
                                var aOneFile: TOneFile;
                                var aDisk, aDir, aShortDesc: ShortString): boolean;
function  QI_GetFoundCount     (DBHandle: PDBaseHandle): Integer;
procedure QI_ClearFoundList    (DBHandle: PDBaseHandle);
procedure QI_GetSearchProgress (DBHandle: PDBaseHandle;
                                var Percent: Integer;
                                var DisksCount, DirsCount, FilesCount: longint);
procedure QI_GetMaxNameLength  (DBHandle: PDBaseHandle;
                                var MaxNameLength, AvgNameLength, MaxNamePxWidth: Integer);

function  QI_CopyDatabase      (SrcDBHandle, TarDBHandle: PDBaseHandle;
                                WholeDbase, CheckDuplicates,
                                CopyLocalOptions: boolean): boolean;


function  QI_SetVolumeLabel (VLabel: ShortString; Drive: char): boolean;

{import from QDIR4}
procedure QI_OpenQDir4Database (Name: ShortString);
function  QI_ReadQDir4Entry    (Position: longInt): boolean;
procedure QI_CloseQDir4Database;
function  QI_GetQDir4VolumeLabel: ShortString;
procedure QI_GetQDir4RecordAttr(var aDiskSize, aDiskFree: longword;
                                aScanDate: longint);
function  QI_ScanQDir4Record   (DBHandle: PDBaseHandle; Path: ShortString;
                                VolumeLabel: ShortString): boolean;
procedure QI_SetQDir4CharConversion (Kamen: boolean);

function  QI_GetErrorCounter (DBHandle: PDBaseHandle): integer;
procedure QI_ClearErrorCounter (DBHandle: PDBaseHandle);

//-----------------------------------------------------------------------------

implementation

uses SysUtils, UExceptions, UBaseTypes, UEngineMain, UEngineFileFind, UCallbacks,
     UConvQDir4, UBaseUtils, ULang;


//-----------------------------------------------------------------------------
// Register callback functions, so that the Engine can update the progress
// indicators on screen etc.

procedure QI_RegisterCallbacks (RIsTheLastCPB: TIsTheLastCPB;
                                RHowToContinue: THowToContinue;
                                RNewLineToIndicator: TNewLineToIndicator;
                                RAppendLineToIndicator: TAppendLineToIndicator;
                                RUpdateProgressIndicator: TUpdateProgressIndicator;
                                RAbortScan: TAbortScan);

  begin
  CB_IsTheLastCPB            := RIsTheLastCPB;
  CB_HowToContinue           := RHowToContinue;
  CB_NewLineToIndicator      := RNewLineToIndicator;
  CB_AppendLineToIndicator   := RAppendLineToIndicator;
  CB_UpdateProgressIndicator := RUpdateProgressIndicator;
  CB_AbortScan               := RAbortScan;
  end;

//-----------------------------------------------------------------------------
// Unregisters all callbacks

procedure QI_UnRegisterCallbacks;

  begin
  ClearCallBacks;
  end;

//=============================================================================
// Checks if the database can be opened for writing, returns 0 in case of
// success, otherwise one of the following constants: cdNotQDirDBase, cdOldVersion         = $02;
// cdHeaderCorrupted,  cdItIsRuntime, cdCannotRead, cdWriteProtected

function QI_CheckDatabase (DBaseName: ShortString): integer;

  begin
  Result := CheckDatabase(DBaseName);
  end;

//-----------------------------------------------------------------------------
// Creates new database. Returns true if succedes.

function QI_CreateDatabase (DBaseName: ShortString): boolean;

  var
    Database: PDatabase;

  begin
  Database := New(PDatabase, Init);
  Database^.DBaseName := DBaseName;
  QI_CreateDatabase := Database^.CreateDatabase(DBaseName);
  Dispose(Database, Done);
  end;

//-----------------------------------------------------------------------------
// Opens database, returns DBHandle

function QI_OpenDatabase(DBaseName: ShortString; bRepair: boolean;
                         var DBHandle: PDBaseHandle;
                         var sMessage: ShortString): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  Result := true;
  Database := New(PDatabase, Init);
  try
    Database^.OpenDatabase(DBaseName, bRepair);
  except
    on E:EQDirException do
      begin
      sMessage := E.Message;
      Result   := false;
      Dispose(Database, Done);
      Database := nil;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Closes database

function QI_CloseDatabase (var DBHandle: PDBaseHandle; MarkAsReadOnly: boolean): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_CloseDatabase := false;
  if Database = nil then exit;
  Database^.MarkAsReadOnly := MarkAsReadOnly;
  Dispose(Database, Done);
  Database := nil;
  QI_CloseDatabase := true;
  end;

//-----------------------------------------------------------------------------
// Deletes TreeStruct

procedure QI_ClearTreeStruct (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  with Database^ do
    if TreeStruct <> nil then
      begin
      Dispose(TreeStruct, Done);
      TreeStruct := nil;
      end;
  end;

//-----------------------------------------------------------------------------
// Deletes SavedTreeStruct and replaces it by an empty one

procedure QI_ClearSavedTreeStruct (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  with Database^ do
    if SavedTreeStruct <> nil then
      begin
      Dispose(SavedTreeStruct, Done);
      SavedTreeStruct := nil;
      end;
  end;

//-----------------------------------------------------------------------------
// Moves TreeStruct to SavedTreeStruct

procedure QI_SaveTreeStruct (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  with Database^ do
    begin
    if SavedTreeStruct <> nil then
      Dispose(SavedTreeStruct, Done);
    SavedTreeStruct := TreeStruct;
    TreeStruct := nil;
    end;
  end;

//-----------------------------------------------------------------------------
// Moves TreeStruct from SavedTreeStruct

procedure QI_RestoreTreeStruct (DBHandle: PDBaseHandle);
  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  with Database^ do
    begin
    if TreeStruct <> nil then
      Dispose(TreeStruct, Done);
    TreeStruct := SavedTreeStruct;
    SavedTreeStruct := nil;
    end;
  end;

//-----------------------------------------------------------------------------
// Idicates, whether the database is open

function QI_DatabaseIsOpened (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_DatabaseIsOpened := false;
  result:=false;
  if Database <> nil then
    QI_DatabaseIsOpened := Database^.DatabaseOpened;
  end;

//-----------------------------------------------------------------------------
// Searches the nearest key. Key is filled with found string.

function QI_KeySearch (DBHandle: PDBaseHandle; var Key: ShortString;
                       var Index: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_KeySearch := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.KeySearch(Key, 0, Index) then
    begin
    Key := Database^.ReadKeyStr(Database^.KeyField^.Keys[Index].Position);
    QI_KeySearch := true;
    end;
  end;

//-----------------------------------------------------------------------------
// Reads disk from database to the work record

procedure QI_ReadDBaseEntry (DBHandle: PDBaseHandle; Position: longint;
                             RWMode: TRWMode);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.ReadDBaseEntry (Position, RWMode);
  end;

//-----------------------------------------------------------------------------
// Reads disk from database to the saved record

procedure QI_ReadDBaseEntryToSaved (DBHandle: PDBaseHandle;
                                   Index: Integer);

  var
    Database: PDatabase absolute DBHandle;
    Key     : TKeyRecord;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Key := Database^.KeyField^.Keys[Index];
  Database^.ReadDBaseEntryToSaved (Key.Position);
  end;

//-----------------------------------------------------------------------------
// Appends the work entry to the database.

function QI_AppendNewDBaseEntry (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  QI_AppendNewDBaseEntry := Database^.AppendNewDBaseEntry(false);
  end;

//-----------------------------------------------------------------------------
// Deletes the work record from the database

function QI_DeleteDBaseEntry (DBHandle: PDBaseHandle; Index: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;
    Key     : TKeyRecord;

  begin
  QI_DeleteDBaseEntry := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  with Database^ do
    begin
    Key := KeyField^.Keys[Index];
    QI_ClearTreeStruct(Database);
    ReadDBaseEntry (Key.Position, rwHead);  // read head only
    if TreeStruct = nil then
      raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
    if ShortCopy(TreeStruct^.Dta.Name, length(TreeStruct^.Dta.Name), 1) <> ']' then
      begin
      inc(KeyField^.CountDeleted);
      TreeStruct^.Dta.Name := TreeStruct^.Dta.Name + ' [' +
                              IntToStr(KeyField^.CountDeleted) + ']';
      end;
    TreeStruct^.Dta.TreeAttr := TreeStruct^.Dta.TreeAttr or kaDeleted;
    if not Database^.WriteToDBaseEntry (Key.Position, rwHead) then exit;
    KeyAtDelete(Index);
    end;
  QI_DeleteDBaseEntry := true;
  end;

//-----------------------------------------------------------------------------
// Delete selected database records

function QI_DeleteSelectedDBaseEntries (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  while Database^.GetSelectedCount > 0 do
    QI_DeleteDBaseEntry(DBHandle, Database^.GetFirstSelectedIndex);
  QI_DeleteSelectedDBaseEntries := true;
  end;

//-----------------------------------------------------------------------------
// Renames database entry

function QI_RenameDBaseEntry (DBHandle: PDBaseHandle; Index: Integer;
                              NewName: ShortString; Undelete: boolean): boolean;

  var
    Database: PDatabase absolute DBHandle;
    Key     : TKeyRecord;

  begin
  QI_RenameDBaseEntry := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  with Database^ do
    begin
    Key := KeyField^.Keys[Index];
    QI_ClearTreeStruct(Database);
    ReadDBaseEntry (Key.Position, rwHead);
    if TreeStruct = nil then
      raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
    TreeStruct^.Dta.Name := NewName;
    if Undelete then
      TreeStruct^.Dta.TreeAttr := TreeStruct^.Dta.TreeAttr and not kaDeleted;
    if not WriteToDBaseEntry (Key.Position, rwHead) then exit;
    if Undelete and (KeyField^.Keys[Index].Attr and kaDeleted <> 0) then
      begin
      KeyField^.Keys[Index].Attr := Key.Attr and not kaDeleted;
      inc(KeyField^.CountValid);
      end;
    KeyAtResort(Index);
    end;
  QI_RenameDBaseEntry := true;
  end;

//-----------------------------------------------------------------------------
// (unused)

procedure QI_SetNewKeyNameToTree (DBHandle: PDBaseHandle; KeyName: ShortString);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  Database^.TreeStruct^.Dta.Name := KeyName;
  end;

//-----------------------------------------------------------------------------
// Scans a disk

function QI_ScanDisk (DBHandle: PDBaseHandle; Path, DiskName, VolumeLabel: ShortString): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  with Database^ do
    begin
    if TreeStruct <> nil then Dispose(TreeStruct, Done);
    TreeStruct := New(PTreeStruct, Init(Database, false));
    QI_ScanDisk := TreeStruct^.ScanDisk(Path, DiskName, VolumeLabel);
    end;
  end;

//-----------------------------------------------------------------------------
// Fills the TreeInfo structure - used to show Disk Info dialog.

function QI_GetTreeInfo (DBHandle: PDBaseHandle; var TreeInfo: TTreeInfo): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  TreeInfo := Database^.TreeStruct^.Dta;
  QI_GetTreeInfo := true;
  end;

//-----------------------------------------------------------------------------
// Returns full path of the selected tree item

function QI_GetFullDirPath (DBHandle: PDBaseHandle; var Path: ShortString): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GetFullDirPath := false;
  Path := '';
  if Database = nil then exit;
  if Database^.GetCountToShow = 0 then exit; // empty database
  if Database^.TreeStruct = nil then exit;
  if Database^.TreeStruct^.CurDirCol  = nil then exit;
  Path := Database^.TreeStruct^.CurDirCol^.GetFullDirPath;
  QI_GetFullDirPath := true;
  end;

//-----------------------------------------------------------------------------
// Returns true when the current tree item is in an archive

function QI_IsInArchive (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  Result := false;
  if Database = nil then exit;
  if Database^.GetCountToShow = 0 then exit; // empty database
  if Database^.TreeStruct = nil then exit;
  if Database^.TreeStruct^.CurDirCol  = nil then exit;
  Result := Database^.TreeStruct^.CurDirCol^.InArchive;
  end;

//-----------------------------------------------------------------------------
// Returns the path to the archive, in which the current tree node is located

function QI_GetArchivePath (DBHandle: PDBaseHandle): ShortString;

  var
    Database: PDatabase absolute DBHandle;

  begin
  Result := '';
  if Database = nil then exit;
  if Database^.GetCountToShow = 0 then exit; // empty database
  if Database^.TreeStruct = nil then exit;
  if Database^.TreeStruct^.CurDirCol = nil then exit;
  Result := Database^.TreeStruct^.CurDirCol^.GetArchivePath;
  end;

//-----------------------------------------------------------------------------
// Returns the handle of the current directory collection

function QI_GetCurDirCol (DBHandle: PDBaseHandle): PDirColHandle;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  QI_GetCurDirCol := Database^.TreeStruct^.CurDirCol;
  end;

//-----------------------------------------------------------------------------
// Fills the SubTotals struture with sums of sizes of the files in subdirectories

function QI_GetCurDirColSubTotals (DBHandle: PDBaseHandle;
                                   var SubTotals: TSubTotals): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GetCurDirColSubTotals := false;
  FillChar(SubTotals, SizeOf(SubTotals), 0);
  if Database = nil then exit;
  if Database^.TreeStruct = nil then exit;
  if Database^.TreeStruct^.CurDirCol = nil then exit;
  QI_GetCurDirColSubTotals := true;
  SubTotals := Database^.TreeStruct^.CurDirCol^.SubTotals;
  end;


//-----------------------------------------------------------------------------
// Returns the handle of the root directory collection

function QI_GetRootDirCol (DBHandle: PDBaseHandle): PDirColHandle;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  QI_GetRootDirCol := Database^.TreeStruct^.RootDirCol;
  end;

//-----------------------------------------------------------------------------
// Returns the current file collection

function QI_GetFileCol (DBHandle: PDBaseHandle): PFileColHandle;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  QI_GetFileCol := Database^.TreeStruct^.CurFileCol;
  end;

//-----------------------------------------------------------------------------
// Sets the supplied directory collection as the current one

procedure QI_SetCurDirCol (DBHandle: PDBaseHandle; DirColHandle: PDirColHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  if DirColHandle = nil then
    raise EQDirFatalException.Create(lsErrorDirColHandleIsNil);
  Database^.TreeStruct^.CurDirCol := DirColHandle;
  end;

//-----------------------------------------------------------------------------
// Reloads the file collection from the database. Used after the change of the
// current directory collection.

procedure QI_UpdateFileCol (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  Database^.TreeStruct^.UpdateCurFileCol(fsAll);
  end;

//-----------------------------------------------------------------------------
// Returns the current key as string

function QI_GetCurrentKey (DBHandle: PDBaseHandle; var Key: ShortString;
                           var Attr: Word; var Index: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GetCurrentKey := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.GetCountToShow = 0 then exit;
  if Database^.KeyField = nil then
    raise EQDirFatalException.Create(lsErrorKeyFieldIsNil);
  if Database^.KeyField^.Count = 0 then exit;
  with Database^ do
    begin
    Index := KeyField^.Focused;
    Attr  := KeyField^.Keys[Index].Attr;
    Key   := ReadKeyStr(KeyField^.Keys[Index].Position);
    end;
  QI_GetCurrentKey := true;
  end;

//-----------------------------------------------------------------------------
// Returns additional parameters of the current key

function QI_GetCurrentKeyEx (DBHandle: PDBaseHandle;
                             var DiskName, VolumeLabel, OrigPath: ShortString;
                             var Attr: Word; var Index: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  Result := true;
  DiskName := '';
  VolumeLabel := '';
  OrigPath := '';
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.GetCountToShow = 0 then exit;
  if Database^.KeyField = nil then
    raise EQDirFatalException.Create(lsErrorKeyFieldIsNil);
  if Database^.KeyField^.Count = 0 then exit;
  with Database^ do
    begin
    Index := KeyField^.Focused;
    Attr  := KeyField^.Keys[Index].Attr;
    ReadKeyStrEx(KeyField^.Keys[Index].Position, DiskName, VolumeLabel, OrigPath);
    end;
  end;

//-----------------------------------------------------------------------------
// Returns a key at specified position

function QI_GetKeyAt (DBHandle: PDBaseHandle; var Key: ShortString;
                      var Attr: Word; Index: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GetKeyAt := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.KeyField = nil then
    raise EQDirFatalException.Create(lsErrorKeyFieldIsNil);
  if (Index < 0) or (Index >= Database^.GetCountToShow) then exit;
  with Database^ do
    begin
    Attr := KeyField^.Keys[Index].Attr;
    Key  := ReadKeyStr(KeyField^.Keys[Index].Position);
    end;
  QI_GetKeyAt := true;
  end;

//-----------------------------------------------------------------------------
// Returns short description for the file or directory

function QI_GetShortDesc (DBHandle: PDBaseHandle; ID: TFilePointer; var ShortDesc: ShortString): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  QI_GetShortDesc := Database^.GetShortDesc(ID, -1, ShortDesc);
  end;

//-----------------------------------------------------------------------------
// Returns selection flags of the key

function  QI_GetSelectionFlag (DBHandle: PDBaseHandle; Index: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  Result := false;
  if (Database = nil) or (Database^.KeyField = nil) or
     (Index < 0) or (Index >= Database^.KeyField^.Count) then exit;
  with Database^.KeyField^.Keys[Index] do
    Result := Attr and kaSelected <> 0;
  end;


//-----------------------------------------------------------------------------
// Sets selection flags of the key

procedure QI_SetSelectionFlag (DBHandle: PDBaseHandle; Selected: boolean;
                               Index: Integer);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.KeyField = nil then
    raise EQDirFatalException.Create(lsErrorKeyFieldIsNil);
  if (Index < 0) or (Index >= Database^.KeyField^.Count) then exit;
  with Database^.KeyField^.Keys[Index] do
    if Attr and kaDeleted = 0 then
      if Selected
        then Attr := Attr or kaSelected
        else Attr := Attr and not kaSelected;
  end;

//-----------------------------------------------------------------------------
// Toggles the selection flag on a key

procedure QI_ToggleSelectionFlag  (DBHandle: PDBaseHandle; Index: Integer);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.KeyField = nil then
    raise EQDirFatalException.Create(lsErrorKeyFieldIsNil);
  if (Index < 0) or (Index >= Database^.KeyField^.Count) then exit;
  with Database^.KeyField^.Keys[Index] do
    if Attr and kaDeleted = 0 then
      if Attr and kaSelected = 0
        then Attr := Attr or kaSelected
        else Attr := Attr and not kaSelected;
  end;

//-----------------------------------------------------------------------------
// Returns the number of selected items

function QI_GetSelectedCount (DBHandle: PDBaseHandle): longint;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  QI_GetSelectedCount := Database^.GetSelectedCount;
  end;

//-----------------------------------------------------------------------------
// Sets the current key

function QI_SetCurrentKeyPos (DBHandle: PDBaseHandle; Index: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_SetCurrentKeyPos := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.KeyField = nil then
    raise EQDirFatalException.Create(lsErrorKeyFieldIsNil);
  if (Index < 0) or (Index >= Database^.KeyField^.Count) then exit;
  with Database^ do
    begin
    if KeyField^.Focused <> Index then
      NeedUpdDiskWin := true;  {to je dulezite - aby se to nezacyklilo}
    KeyField^.Focused := Index;
    QI_SetCurrentKeyPos := true;
    end;
  end;

//-----------------------------------------------------------------------------
// Returns number of keys to be displayed: The number depends on "Show Deleted"
// option

function QI_GetCountToShow (DBHandle: PDBaseHandle): Integer;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GetCountToShow := 0;
  if Database <> nil then
    QI_GetCountToShow := Database^.GetCountToShow;
  end;

//-----------------------------------------------------------------------------
// Checks, if a tree corresponding to the key is loaded

function QI_IsCorrectTreeLoaded (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_IsCorrectTreeLoaded := false;
  if Database = nil then exit;
  if Database^.KeyField = nil then exit;
  if Database^.TreeStruct = nil then exit;
  with Database^ do
    QI_IsCorrectTreeLoaded :=
      ReadKeyStr(KeyField^.Keys[KeyField^.Focused].Position) = TreeStruct^.Dta.Name;
  end;

//-----------------------------------------------------------------------------
// Returns true, if the update of display of disks is needed (for example
// when a disk is deleted or renamed).

function  QI_NeedUpdDiskWin (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_NeedUpdDiskWin := false;
  if Database = nil then exit;
  QI_NeedUpdDiskWin := Database^.NeedUpdDiskWin;
  end;

//-----------------------------------------------------------------------------
// Returns true, if the update of the display of the tree is needed.

function  QI_NeedUpdTreeWin (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_NeedUpdTreeWin := false;
  if Database = nil then exit;
  QI_NeedUpdTreeWin := Database^.NeedUpdTreeWin;
  end;

//-----------------------------------------------------------------------------
// Returns true, if the update of the display of the files is needed.

function  QI_NeedUpdFileWin (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_NeedUpdFileWin := false;
  if Database = nil then exit;
  QI_NeedUpdFileWin := Database^.NeedUpdFileWin;
  end;

//-----------------------------------------------------------------------------
// Clears the flag indicating the update of disks display is needed

procedure QI_ClearNeedUpdDiskWin (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  Database^.NeedUpdDiskWin := false;
  end;

//-----------------------------------------------------------------------------
// Clears the flag indicating the update of files display is needed

procedure QI_ClearNeedUpdFileWin (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  Database^.NeedUpdFileWin := false;
  end;

//-----------------------------------------------------------------------------
// Clears the flag indicating the update of tree display is needed

procedure QI_ClearNeedUpdTreeWin (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  Database^.NeedUpdTreeWin := false;
  end;

//-----------------------------------------------------------------------------
// Sets the flag indicating the update of disks display is needed

procedure QI_SetNeedUpdDiskWin (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  Database^.NeedUpdDiskWin := true;
  end;

//-----------------------------------------------------------------------------
// Sets the flag indicating the update of tree display is needed

procedure QI_SetNeedUpdTreeWin (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  Database^.NeedUpdTreeWin := true;
  end;

//-----------------------------------------------------------------------------
// Sets the flag indicating the update of files display is needed

procedure QI_SetNeedUpdFileWin (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  Database^.NeedUpdFileWin := true;
  end;

//-----------------------------------------------------------------------------
// Sets the local options to the database

procedure QI_SetLocalOptions (DBHandle: PDBaseHandle; LocalOptions: TLocalOptions);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.LocalOptions := LocalOptions;
  end;

//-----------------------------------------------------------------------------
// Gets the local options from the database

procedure QI_GetLocalOptions (DBHandle: PDBaseHandle; var LocalOptions: TLocalOptions);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  LocalOptions := Database^.LocalOptions;
  end;

//-----------------------------------------------------------------------------
// Searches for the key. If does not find exact match, finds the nearest one.
// Sets the found item as current key

function QI_GoToKey (DBHandle: PDBaseHandle; Key: ShortString): boolean;

  var
    Database  : PDatabase absolute DBHandle;
    Index     : Integer;
    FoundExact: boolean;

  begin
  QI_GoToKey := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  with Database^ do
    begin
    if GetCountToShow > 0 then
      begin
      FoundExact := KeySearch (Key, 0, Index);
      QI_GoToKey := QI_GoToKeyAt(DBHandle, Index) and FoundExact;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Goes to the specified key by index. Sets the found key as the current key.

function QI_GoToKeyAt (DBHandle: PDBaseHandle; KeyPos: Integer): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GoToKeyAt := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  with Database^ do
    begin
    if GetCountToShow > 0 then
      begin
      if (KeyPos < 0) or (KeyPos >= KeyField^.Count) then exit;
      KeyField^.Focused := KeyPos;
      NeedUpdDiskWin := true;
      QI_GoToKeyAt := QI_ReloadTreeStruct(DBHandle, false);
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Reloads the tree struct from database. Reloads only in case it is not loaded yet

function QI_ReloadTreeStruct (DBHandle: PDBaseHandle; LeaveNil: boolean): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_ReloadTreeStruct := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  with Database^ do
    begin
    if LeaveNil and (TreeStruct = nil) then exit;  {je-li nil, ponechat}
    if GetCountToShow > 0 then
      begin
      if (TreeStruct = nil) or
       (ReadKeyStr(KeyField^.Keys[KeyField^.Focused].Position) <> TreeStruct^.Dta.Name)
         then ReadDBaseEntry(KeyField^.Keys[KeyField^.Focused].Position, rwWhole);
      QI_ReloadTreeStruct := true;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Returns database file name. Used to check, if the database of some name is
// already open.

function QI_GetDBaseFileName (DBHandle: PDBaseHandle): ShortString;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GetDBaseFileName := '';
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  QI_GetDBaseFileName := Database^.DBaseName;
  end;

//-----------------------------------------------------------------------------
// Loads the description to a buffer. Allocates the Buf, needed to be
// deallocated by the caller

function QI_LoadDescToBuf (DBHandle: PDBaseHandle; ID: TFilePointer;
                           var Buf: PChar): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  QI_LoadDescToBuf := Database^.LoadDescToBuf(ID, Buf);
  end;

//-----------------------------------------------------------------------------
// Saves the description to database

function  QI_SaveBufToDesc (DBHandle: PDBaseHandle; var OrigFilePointer: TFilePointer;
                            Buf: PChar): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  QI_SaveBufToDesc := Database^.SaveBufToDesc(OrigFilePointer, Buf);
  end;

//-----------------------------------------------------------------------------
// Delete a file from database

procedure QI_DeleteFile (DBHandle: PDBaseHandle; var fileName: ShortString);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
    Database^.DeleteFile(fileName);
  end;


//-----------------------------------------------------------------------------
// Adds new reference to the description to CurDirCol, CurFileCol and Tree.
// Used after the new description was assigned to OneFile record.

procedure QI_UpdateDescInCurList(DBHandle: PDBaseHandle; POneFile: TPOneFile);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.UpdateDescInCurList(POneFile);
  end;

//-----------------------------------------------------------------------------
// Returns database info in the DBaseInfo structure

procedure QI_GetDBaseInfo (DBHandle: PDBaseHandle; var DBaseInfo: TDBaseInfo);

  var
    Database: PDatabase absolute DBHandle;

  begin
  FillChar(DBaseInfo, sizeof(DBaseInfo), 0);
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.GetDBaseInfo(DBaseInfo);
  end;

//-----------------------------------------------------------------------------
// Returns non-zero value if the database is used longer than 1 month. Used to
// display Help at program exit.

function  QI_DBaseExpired (DBHandle: PDBaseHandle): integer;

  var
    Database: PDatabase absolute DBHandle;

  begin
  Result := 0;
  if Database = nil then exit;
  Result := Database^.Expired;
  end;

//==== DirCollection functions ================================================
// Returns count of items in directory collection

function QI_GetDirColCount (DCHandle: PDirColHandle): Integer;

  var
    DirCol: PDirCollection absolute DCHandle;

  begin
  if DirCol = nil then
    raise EQDirFatalException.Create(lsErrorDirColHandleIsNil);
  QI_GetDirColCount := DirCol^.Count;
  end;

//-----------------------------------------------------------------------------
// Returns the data (description, size, time etc.) from directory collection

function QI_GetOneDirDta (DCHandle: PDirColHandle; var OneDir: TOneDir): boolean;

  var
    DirCol: PDirCollection absolute DCHandle;

  begin
  if DirCol = nil then
    raise EQDirFatalException.Create(lsErrorDirColHandleIsNil);
  MoveOneDir(DirCol^.Dta^, OneDir);
  QI_GetOneDirDta := true;
  end;

//-----------------------------------------------------------------------------
// Returns the dir collection at specified position

function QI_GetDirColAt (DCHandle: PDirColHandle; Index: Integer): PDirColHandle;

  var
    DirCol: PDirCollection absolute DCHandle;

  begin
  if DirCol = nil then
    raise EQDirFatalException.Create(lsErrorDirColHandleIsNil);
  QI_GetDirColAt := DirCol^.At(Index);
  end;

//==== FileCollection functions ===============================================
// Returns the count of the file collection

function QI_GetFileColCount (FCHandle: PFileColHandle): Integer;

  var
    FileCol: PFileCollection absolute FCHandle;

  begin
  if FileCol = nil then
    raise EQDirFatalException.Create(lsErrorFileColHandleIsNil);
  QI_GetFileColCount := FileCol^.Count;
  end;

//-----------------------------------------------------------------------------
// Returns OneFile from the file collection at specified index

function QI_GetOneFileFromCol (FCHandle: PFileColHandle; Index: Integer;
                              var OneFile: TOneFile): boolean;

  var
    FileCol: PFileCollection absolute FCHandle;
    POneFile: TPOneFile;
    RealSize: Integer;

  begin
  if FileCol = nil then
    raise EQDirFatalException.Create(lsErrorFileColHandleIsNil);
  POneFile := TPOneFile(FileCol^.At(Index));
  ///RealSize := 8 + oneFileHeadSize + 1 + length(POneFile^.LongName);
  RealSize := 16 + oneFileHeadSize + 1 + length(POneFile^.LongName);
  move(POneFile^, OneFile, RealSize);
  QI_GetOneFileFromCol := true;
  end;

//=== FileSearch functions ====================================================
// Clears the search collection

procedure QI_ClearSearchCol (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  Database^.FileSearchCol^.FreeAll;
  end;

//-----------------------------------------------------------------------------
// Returns true if the search collection is not empty

function QI_SearchItemsAvail (DBHandle: PDBaseHandle): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  QI_SearchItemsAvail := Database^.FileSearchCol^.Count > 0;
  end;

//-----------------------------------------------------------------------------
// Sets the callback function for search functions

procedure QI_SetSearchCallback (DBHandle: PDBaseHandle;
                                UpdateFunct: TCallBackProc);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  Database^.UpdateFunct := UpdateFunct;
  end;

//-----------------------------------------------------------------------------
// Clears the callback function for search functions

procedure QI_DelSearchCallback (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.ClearUpdateFunct;
  end;

//-----------------------------------------------------------------------------
// Searches folders/files in the database

procedure QI_FindFiles (DBHandle: PDBaseHandle; SearchDlgData: TSearchDlgData);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  TokenFindMask(SearchDlgData.Mask, Database^.MaskCol, SearchDlgData.CaseSensitive,
                SearchDlgData.AddWildCards, SearchDlgData.AsPhrase);
  TokenFindMask(SearchDlgData.ExcludeMask, Database^.ExcludeMaskCol,
                false, false, false);
  TokenFindMask(SearchDlgData.DirMask, Database^.DirMaskCol,
                false, false, false);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  Database^.SearchDlgData := SearchDlgData;
  Database^.FindFiles;
  end;

//-----------------------------------------------------------------------------
// Sets callback function for printing disks

procedure QI_SetIterateCallback (DBHandle: PDBaseHandle;
                                 UpdateFunct: TCallBackProc;
                                 SendOneDiskFunct: TSendOneDiskFunct;
                                 SendOneDirFunct : TSendOneDirFunct;
                                 SendOneFileFunct: TSendOneFileFunct;
                                 GetNameWidthFunct: TGetNameWidthFunct);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  Database^.UpdateFunct := UpdateFunct;
  Database^.SendOneDiskFunct := SendOneDiskFunct;
  Database^.SendOneDirFunct  := SendOneDirFunct;
  Database^.SendOneFileFunct := SendOneFileFunct;
  Database^.GetNameWidthFunct := GetNameWidthFunct;
  end;

//-----------------------------------------------------------------------------
// Clears callback function for printing disks

procedure QI_DelIterateCallback (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.ClearUpdateFunct;
  Database^.ClearSendFunct;
  end;

//-----------------------------------------------------------------------------
// Sets the callback functions for export of disks

procedure QI_SetNotifyProcs (DBHandle: PDBaseHandle;
                             UpdateFunct       : TCallBackProc;
                             OnDatabaseBegin   : TNotifyProc;
                             OnDatabaseEnd     : TNotifyProc;
                             OnDiskBegin       : TNotifyProc;
                             OnDiskEnd         : TNotifyProc;
                             OnFolderBegin     : TNotifyProc;
                             OnFolderEnd       : TNotifyProc;
                             OnFile            : TNotifyProc);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.UpdateFunct       := UpdateFunct;
  Database^.OnDatabaseBegin   := OnDatabaseBegin;
  Database^.OnDatabaseEnd     := OnDatabaseEnd;
  Database^.OnDiskBegin       := OnDiskBegin;
  Database^.OnDiskEnd         := OnDiskEnd;
  Database^.OnFolderBegin     := OnFolderBegin;
  Database^.OnFolderEnd       := OnFolderEnd;
  Database^.OnFile            := OnFile;
  end;

//-----------------------------------------------------------------------------
// Clears the callback functions for export of disks

procedure QI_DelNotifyProcs (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.ClearUpdateFunct;
  Database^.ClearNotifyFunct;
  end;

//-----------------------------------------------------------------------------
// Iterates files for printing or export

procedure QI_IterateFiles (DBHandle: PDBaseHandle; SearchIn: TSearchIn;
                           eSendType: TSendType);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Database^.IterateFiles(SearchIn, eSendType);
  end;

//-----------------------------------------------------------------------------
// Finds disks sorted according to the size

procedure QI_FindEmpties (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  Database^.FindEmpties;
  end;

//-----------------------------------------------------------------------------
// Returns one found item - used in the form to build own collection of searched
// items

function QI_GetSearchItemAt (DBHandle: PDBaseHandle; Index: Integer;
                             var AOneFile: TOneFile; var ADisk,
                             ADir, AShortDesc: ShortString): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  QI_GetSearchItemAt := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  if (Index < 0) or (Index >= Database^.FileSearchCol^.Count) then exit;
  fillchar(aOneFile, SizeOf(TOneFile), 0);
  ADir := '';
  with PFSearchItem(Database^.FileSearchCol^.At(Index))^ do
    begin
    ADir          := GetPQString(Dir);
    ADisk         := GetPQString(Disk);
    AShortDesc    := GetPQString(ShortDesc);
    aOneFile.LongName := GetPQString(LongName);
    aOneFile.Ext  := Ext;
    aOneFile.Size := Size;
    aOneFile.Time := Time;
    aOneFile.Attr := Attr;
    aOneFile.Description := Description;
    aOneFile.SelfFilePos := SelfFilePos;
    end;
  QI_GetSearchItemAt := true;
  end;

//-----------------------------------------------------------------------------
// Returns the count of found files

function QI_GetFoundCount (DBHandle: PDBaseHandle): Integer;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  QI_GetFoundCount := Database^.FileSearchCol^.Count;
  end;

//-----------------------------------------------------------------------------
// Clears the collection with found items

procedure QI_ClearFoundList (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.FileSearchCol = nil then
    raise EQDirFatalException.Create(lsErrorFileSearchColIsNil);
  Database^.FileSearchCol^.FreeAll;
  end;

//-----------------------------------------------------------------------------
// Returns data for indication of search progress

procedure QI_GetSearchProgress  (DBHandle: PDBaseHandle;
                                 var Percent: Integer;
                                 var DisksCount, DirsCount, FilesCount: longint);

  var
    Database: PDatabase absolute DBHandle;

  begin
  DisksCount := 0;
  DirsCount  := 0;
  FilesCount := 0;
  Percent    := 0;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.KeyField = nil then
    raise EQDirFatalException.Create(lsErrorKeyFieldIsNil);
  DisksCount := Database^.DisksCount;
  DirsCount  := Database^.DirsCount;
  FilesCount := Database^.FilesCount;
  if Database^.KeyField^.CountValid > 0 then
    Percent := (DisksCount * 100) div Database^.KeyField^.CountValid;
  end;

//-----------------------------------------------------------------------------
// Gets the maximum disk name length - used for setting the width of the column
// when printing

procedure QI_GetMaxNameLength  (DBHandle: PDBaseHandle;
                                var MaxNameLength, AvgNameLength, MaxNamePxWidth: Integer);

  var
    Database: PDatabase absolute DBHandle;

  begin
  MaxNameLength := 0;
  MaxNamePxWidth := 0;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  MaxNameLength  := Database^.MaxNameLength;
  MaxNamePxWidth := Database^.MaxNamePxWidth;
  if Database^.FilesCount > 0
    then AvgNameLength := Database^.NameLengthSum div Database^.FilesCount
    else AvgNameLength := 0;
  end;

//-----------------------------------------------------------------------------
// Recursively finds specified directory in the tree

function QI_GoToDir (DBHandle: PDBaseHandle; Path: ShortString): boolean;

  var
    Database: PDatabase absolute DBHandle;
    DirCol  : PDirCollection;

  begin
  QI_GoToDir := false;
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  if Database^.TreeStruct = nil then
    raise EQDirFatalException.Create(lsErrorTreeStructIsNil);
  DirCol := Database^.TreeStruct^.RecurseFindDir(Database^.TreeStruct^.RootDirCol, Path);
  if DirCol <> nil then
    begin
    Database^.TreeStruct^.CurDirCol := DirCol;
    Database^.TreeStruct^.UpdateCurFileCol(fsAll);
    QI_GoToDir := Database^.TreeStruct^.CurDirCol^.GetFullDirPath = Path;
    end;
  end;

//-----------------------------------------------------------------------------
// Copies database. Used when reindexing.

function  QI_CopyDatabase (SrcDBHandle, TarDBHandle: PDBaseHandle;
                           WholeDbase, CheckDuplicates,
                           CopyLocalOptions: boolean): boolean;

  var
    SrcDatabase: PDatabase absolute SrcDBHandle;
    TarDatabase: PDatabase absolute TarDBHandle;

  begin
  if (SrcDatabase = nil) or (TarDatabase = nil) then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  Result := SrcDatabase^.CopyDatabase(TarDatabase, WholeDbase,
                                      CheckDuplicates, CopyLocalOptions);
  end;

//=============================================================================
// Not used anymore - was used to set the volume label.

function QI_SetVolumeLabel (VLabel: ShortString; Drive: char): boolean;

  begin
  Result := false;
  // QI_SetVolumeLabel := SetVolumeLabel(VLabel, Drive);
  end;


//==== Import from QuickDir 4 =================================================

procedure QI_OpenQDir4Database (Name: ShortString);

  begin
  OpenQDir4Database (Name);
  end;

//-----------------------------------------------------------------------------

function  QI_ReadQDir4Entry (Position: longInt): boolean;

  begin
  Result := ReadQDir4Entry (Position);
  end;

//-----------------------------------------------------------------------------

procedure QI_CloseQDir4Database;

  begin
  CloseQDir4Database;
  end;

//-----------------------------------------------------------------------------

function  QI_GetQDir4VolumeLabel: ShortString;

  begin
  Result := GetQDir4VolumeLabel;
  end;

//-----------------------------------------------------------------------------

procedure QI_GetQDir4RecordAttr(var aDiskSize, aDiskFree: longword; aScanDate: longint);

  begin
  GetQDir4RecordAttr(aDiskSize, aDiskFree, aScanDate);
  end;

//-----------------------------------------------------------------------------

function QI_ScanQDir4Record (DBHandle: PDBaseHandle; Path: ShortString; VolumeLabel: ShortString): boolean;

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then
    raise EQDirFatalException.Create(lsErrorDBaseIsNil);
  with Database^ do
    begin
    if TreeStruct <> nil then Dispose(TreeStruct, Done);
    TreeStruct := New(PTreeStruct, Init(Database, false));
    Result := TreeStruct^.ScanQDir4Record(Path, VolumeLabel);
    end;
  end;

//-----------------------------------------------------------------------------

procedure QI_SetQDir4CharConversion (Kamen: boolean);

  begin
  SetQDir4CharConversion (Kamen);
  end;

//-----------------------------------------------------------------------------

function  QI_GetErrorCounter (DBHandle: PDBaseHandle): integer;

  var
    Database: PDatabase absolute DBHandle;

  begin
  Result := 0;
  if Database = nil then exit;
  Result := Database^.ErrorCounter;
  end;

//-----------------------------------------------------------------------------

procedure QI_ClearErrorCounter (DBHandle: PDBaseHandle);

  var
    Database: PDatabase absolute DBHandle;

  begin
  if Database = nil then exit;
  Database^.ErrorCounter := 0;
  end;

//-----------------------------------------------------------------------------

end.
