unit UEngineArchives;
(*====================================================================
Functions for reading various types of archive files. The Scan... functions
read the archive and put the files to a unified collection of TOneArc items,
which is then given to the Engine to dispatch to tree.
======================================================================*)
{$A-}

interface

uses UTypes, UCollections, UStream, UApiTypes, UBaseTypes;

type

  TQSearchRec = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: ShortString;
    end;

  POneArcItem = ^TOneArcItem;
  TOneArcItem = object(TQObject)
    Attr       : Byte;
    Time       : Longint;
    Size       : Longint;
    Name       : TPQString;
    Dir        : TPQString;
    OffsetInArc: longint;
    constructor Init (aDir: ShortString; aName: ShortString;
                      aSize, aTime: longint; aAttr: byte;
                      aOffsetInArc: longint);
    destructor Done; virtual;
    end;

  PArcCollection = ^TArcCollection;
  TArcCollection = object(TQSortedCollection)
    constructor Init (ALimit, ADelta: Integer);
    function    Compare(Key1, Key2: Pointer): Integer; virtual;
    end;

 function ScanZipFile (Col: PArcCollection; WholeFileSpec: ShortString;
                       StartOffset: longint): boolean;
 function ScanARCFile (Col: PArcCollection; WholeFileSpec: ShortString): boolean;
 function ScanBAKFile (Col: PArcCollection; WholeFileSpec: ShortString): boolean;
 function ScanPCBFile (Col: PArcCollection; WholeFileSpec: ShortString; CPB: boolean): boolean;
 function ScanNBFile  (Col: PArcCollection; WholeFileSpec: ShortString): boolean;
 function ScanARJFile (Col: PArcCollection; WholeFileSpec: ShortString;
                       StartOffset: longint): boolean;
 function ScanLHArcFile (Col: PArcCollection; WholeFileSpec: ShortString;
                         StartOffset: longint): boolean;
 function ScanRARFile (Col: PArcCollection; WholeFileSpec: ShortString;
                       StartOffset: longint): boolean;
 function ScanACEFile (Col: PArcCollection; WholeFileSpec: ShortString;
                       StartOffset: longint): boolean;
 function ScanAR6File (Col: PArcCollection; WholeFileSpec: ShortString): boolean;
 function ScanExeFile (Col: PArcCollection; WholeFileSpec: ShortString;
                       ScanAlsoNonZipArchives: boolean; var ArchiveType: TArchiveType): boolean;

implementation

  uses
    {$ifdef mswindows}
    WinTypes,WinProcs,
    {$ELSE}
      LCLIntf, LCLType, LMessages,
    {$ENDIF}
    SysUtils, UCallbacks, UBaseUtils, ULang, UExceptions,
       UUnRar, UUnAce;

const
  labZjistujiObsah             = lsExploringContent;
  labPCBHlavaNenalezena        = lsCentralDirCPBNotFound;


//=== TOneArcItem =============================================================

constructor TOneArcItem.Init (aDir: ShortString; aName: ShortString;
                              aSize, aTime: longint; aAttr: byte;
                              aOffsetInArc: longint);

  var TmpSt: ShortString;

  begin
  inherited Init;
  TmpSt := AnsiUpperCase(aDir);
  if (TmpSt = aDir) then aDir := AnsiLowerCase(aDir);
  Dir  := QNewStr('\' + aDir); // the backslash is added so that the string is never nil
  if aName = '' then aName := ' '; // avoid nil
  TmpSt := AnsiUpperCase(aName);
  if (TmpSt = aName) then aName := AnsiLowerCase(aName);
  Name := QNewStr(aName);
  Size := aSize;
  Time := aTime;
  Attr := aAttr;
  OffsetInArc := aOffsetInArc;
  end;

//-----------------------------------------------------------------------------

destructor TOneArcItem.Done;

  begin
  QDisposeStr(Dir);
  QDisposeStr(Name);
  inherited Done;
  end;

//=== TArcCollection ==========================================================

constructor TArcCollection.Init (ALimit, ADelta: Integer);

  begin
  inherited Init (ALimit, ADelta);
  Duplicates := true;
  end;

//-----------------------------------------------------------------------------

function TArcCollection.Compare (Key1, Key2: Pointer): Integer;

  begin
  if GetPQString(POneArcItem(Key1)^.Dir) > GetPQString(POneArcItem(Key2)^.Dir)
    then Compare := 1
    else
      if GetPQString(POneArcItem(Key1)^.Dir) < GetPQString(POneArcItem(Key2)^.Dir)
        then Compare := -1
        else
          if GetPQString(POneArcItem(Key1)^.Name) > GetPQString(POneArcItem(Key2)^.Name)
            then Compare := 1
            else
              if GetPQString(POneArcItem(Key1)^.Name) < GetPQString(POneArcItem(Key2)^.Name)
                then Compare := -1
                else Compare := 0;
  end;

//=============================================================================
// Scans AR6 files - created by Manager602

function ScanAR6File (Col: PArcCollection; WholeFileSpec: ShortString): boolean;

  type
    AR6HeaderType = record
      AR  : array [1..3]  of char;
      Rest: array [1..15] of char;
      end;

    AR6RawType = record
      rHeaderFlag      : array[1..6] of char;
      rCompVersion     : word;
      rDateTime        : longint;
      rCRC             : longint;
      rCompSize        : LongWord;
      rSize            : LongWord;
      rNameLength      : word;
      rExtraLength     : word;
      end;

  var
    AR6File          : TQBufStream;
    AR6EOFStatus     : boolean;
    AR6Header        : AR6HeaderType;
    AR6Raw           : AR6RawType;
    FPath            : ShortString;
    LongName         : ShortString;


   function GetFirstFile: boolean;

    type
      Buffer = array[1..256] of char;
    var
      Buf       : ^Buffer;
      QResult   : longword;
      LastSlash : byte;
      TmpPath   : ShortString;
      i         : byte;

    begin
    GetFirstFile := false;
    AR6File.ReadExt(AR6Raw, SizeOf(AR6Raw), QResult);
    if QResult <> SizeOf(AR6Raw) then exit;
    if AR6Raw.rHeaderFlag <> 'AR6'#1'd'#0 then exit;
                             {'AR6'#2'd'#0 - is by central dir struct}
    with AR6Raw do
      begin
      GetMem(Buf,rNameLength);
      AR6File.ReadExt(Buf^, rNameLength, QResult);
      if rNameLength > 255 then
        begin
        Move(Buf^[rNameLength-255+1], Buf^[1], 255);
        rNameLength := 255;
        end;
      LongName[0] := char(rNameLength);
      move(Buf^[1], LongName[1], rNameLength);
      FreeMem(Buf,rNameLength);
      if QResult <> rNameLength then exit;

      if rExtraLength > 0 then
        begin
        GetMem(Buf,rExtraLength);
        AR6File.ReadExt(Buf^, rExtraLength, QResult);
        FreeMem(Buf,rExtraLength);
        if QResult <> rExtraLength then exit;
        end;
      end;

    GetFirstFile := true;
    with AR6Raw do
      begin
      LastSlash := 0;
      for i := 1 to length(LongName) do
        begin
        if LongName[i] = '/' then LongName[i]   := '\';
        if LongName[i] = '\' then LastSlash := i;
        end;
      if LastSlash <> 0
        then
          begin
          TmpPath := ShortCopy(LongName, 1, LastSlash);
          ShortDelete (LongName, 1, LastSlash);
          end
        else
          TmpPath := '';
      if TmpPath <> FPath then FPath := TmpPath;
      end;
    end;


   function GetNextFile: boolean;

    var
      FPos : LongWord;

    begin
    GetNextFile := true;
    FPos := AR6File.GetPos + AR6Raw.rCompSize;
    if FPos < AR6File.GetSize
      then
        begin
        AR6File.Seek(FPos);
        if not GetFirstFile then
          begin
          GetNextFile := false;
          AR6EofStatus := true;
          end;
        end
      else
        begin
        AR6EofStatus := true;
        GetNextFile  := false;
        end;
    end;


  var
    QResult  : longword;
    TmpStrArr: array[0..256] of char;

  begin
  try
    Result  := false;
    AR6EofStatus := False;
    FPath     := '';
    {--- opening ---}
    AR6File.Init(StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    AR6File.ReadExt(AR6Header, SizeOf(AR6Header), QResult);
    if AR6Header.AR <> 'AR6' then
      begin
      AR6File.Done;
      exit;
      end;
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);

    if not GetFirstFile then
      begin
      AR6File.Done;
      exit;
      end;
    while not AR6EofStatus do
      begin
      with AR6Raw do
        begin
        LongName := TrimSpaces(LongName);
        Col^.Insert(New (POneArcItem, Init(FPath, LongName, rSize, rDateTime, 0, 0)));
        Result := true;
        end;
      GetNextFile; // if not successful, set AR6EofStatus to true
      end;

    AR6File.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans ZIP file, not using the central directory at the end

function SlowScanZipFile (Col: PArcCollection; WholeFileSpec: ShortString;
                          StartOffset: longint): boolean;

  type
    ZIPLocHeadType = record
      rHeaderFlag      : array[1..4] of char;
      rVersionNeeded   : word;
      rGenPurposeFlag  : word;
      rCompVersion     : word;
      rDateTime        : longint;
      rCRC             : longint;
      rCompSize        : LongWord;
      rSize            : LongWord;
      rNameLength      : word;
      rExtraLength     : word;
      end;

    ZIPCenHeadType = record
      rHeaderFlag      : array[1..4] of char;
      rVersionMadeBy   : word;
      rVersionNeeded   : word;
      rGenPurposeFlag  : word;
      rCompVersion     : word;
      rDateTime        : longint;
      rCRC             : longint;
      rCompSize        : LongWord;
      rSize            : LongWord;
      rNameLength      : word;
      rExtraLength     : word;
      rCommentLength   : word;
      rDiskNumStart    : word;
      rIntFileAttr     : word;
      rExtFileAttr     : longint;
      rRelOfsLocHead   : LongWord;
      end;

    ZIPEndHeadType = record
      rHeaderFlag      : array[1..4] of char;
      rNumOfThisDisk   : word;
      rNumOfStartDisk  : word; // number of disk with the start of central dir
                               // must be equal to rNumOfThisDisk
      rEntrInThisDir   : word;
      rEntrInWholeDir  : word;
      rSizeOfCentralDir: LongWord;
      rCenDirOfs       : LongWord;
      rZipCommentLen   : word;
      end;

  var
    ZIPFile          : TQBufStream;
    ZIPEOFStatus     : boolean;
    ZIPLocHead       : ZIPLocHeadType;
    LongName         : ShortString;
    FPath            : ShortString;
    LocHeadOffset    : longint;


   function GetFirstFile (First: boolean): boolean;

    type
      Buffer = array[1..256] of char;
    var
      Buf       : ^Buffer;
      QResult   : longword;
      LastSlash : byte;
      TmpPath   : ShortString;
      i         : byte;

    begin
    GetFirstFile := false;
    ZIPFile.ReadExt(ZIPLocHead, SizeOf(ZIPLocHead), QResult);
    if QResult <> SizeOf(ZIPLocHead) then exit;
    if ZIPLocHead.rHeaderFlag <> 'PK'#3#4 then exit;
    LocHeadOffset := ZIPFile.GetPos - SizeOf(ZIPLocHead);
    with ZIPLocHead do
      begin
      GetMem(Buf,rNameLength);
      ZIPFile.ReadExt(Buf^, rNameLength, QResult);
      if rNameLength > 255 then
        begin
        Move(Buf^[rNameLength-255+1], Buf^[1], 255);
        rNameLength := 255;
        end;
      LongName[0] := char(rNameLength);
      move(Buf^[1], LongName[1], rNameLength);
      FreeMem(Buf,rNameLength);
      if QResult <> rNameLength then exit;
      ///OemToCharBuff(@LongName[1], @LongName[1], rNameLength);

      if rExtraLength > 0 then
        begin
        GetMem(Buf,rExtraLength);
        ZIPFile.ReadExt(Buf^, rExtraLength, QResult);
        FreeMem(Buf,rExtraLength);
        if QResult <> rExtraLength then exit;
        end;
      end;

    GetFirstFile := true;
    with ZIPLocHead do
      begin
      LastSlash := 0;
      for i := 1 to length(LongName) do
        begin
        if LongName[i] = '/' then LongName[i]   := '\';
        if LongName[i] = '\' then LastSlash := i;
        end;
      if LastSlash <> 0
        then
          begin
          TmpPath := ShortCopy(LongName, 1, LastSlash);
          ShortDelete (LongName, 1, LastSlash);
          end
        else
          TmpPath := '';
      if TmpPath <> FPath then FPath := TmpPath;
      end;
    end;


   function GetNextFile: boolean;

    var
      FPos : LongWord;

    begin
    GetNextFile := true;
    FPos := ZIPFIle.GetPos + ZIPLocHead.rCompSize;
    if FPos < ZIPFile.GetSize
      then
        begin
        ZIPFile.Seek(FPos);
        if not GetFirstFile(false) then
          begin
          GetNextFile := false;
          ZIPEofStatus := true;
          end;
        end
      else
        begin
        ZIPEofStatus := true;
        GetNextFile  := false;
        end;
    end;

  var
    TmpStrArr : array[0..256] of char;

  begin
  try
    Result := false;
    LocHeadOffset := 0;
    ZIPEofStatus := False;
    FPath     := '';
    {--- opening ---}
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    ZIPFile.Init(StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    if StartOffset > 0 then ZIPFile.Seek(StartOffset);

    if not GetFirstFile(true) then
      begin
      ZIPFile.Done;
      exit;
      end;
    while not ZIPEofStatus do
      begin
      with ZIPLocHead do
        begin
        LongName := TrimSpaces(LongName);
        if LongName <> '' then
          Col^.Insert(New (POneArcItem, Init(FPath, LongName, rSize, rDateTime,
                           0, LocHeadOffset)));
        Result := true;
        end;
      GetNextFile; // if unsuccessful, sets ZIPEofStatus to true
      end;

    ZIPFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//-----------------------------------------------------------------------------
// Scans ZIP file, using the central directory at the end

function ScanZipFile (Col: PArcCollection; WholeFileSpec: ShortString;
                      StartOffset: longint): boolean;

  type
    rHeaderFlag      = array[1..4] of char;

    ZIPLocHeadType = record
      rVersionNeeded   : word;
      rGenPurposeFlag  : word;
      rCompVersion     : word;
      rDateTime        : longint;
      rCRC             : longint;
      rCompSize        : longint;
      rSize            : longint;
      rNameLength      : word;
      rExtraLength     : word;
      end;

    ZIPCenHeadType = record
      rVersionMadeBy   : word;
      rVersionNeeded   : word;
      rGenPurposeFlag  : word;
      rCompVersion     : word;
      rDateTime        : longint;
      rCRC             : longint;
      rCompSize        : longint;
      rSize            : longint;
      rNameLength      : word;
      rExtraLength     : word;
      rCommentLength   : word;
      rDiskNumStart    : word;
      rIntFileAttr     : word;
      rExtFileAttr     : longint;
      rRelOfsLocHead   : longint;
      end;

    ZIPEndHeadType = record
      rNumOfThisDisk   : word;
      rNumOfStartDisk  : word;
      rEntrInThisDir   : word;
      rEntrInWholeDir  : word;
      rSizeOfCentralDir: longint;
      rCenDirOfs       : longint;
      rZipCommentLen   : word;
      end;

  var
    ZIPFile          : TQBufStream;
    ZIPEOFStatus     : boolean;
    ZIPCenHead       : ZIPCenHeadType;
    LongName         : ShortString;
    FPath            : ShortString;
    ZipSplitted      : boolean;


   function GetCentrDirOfset(StartOffset: longint): boolean;

    const
      HalfBufSize = 512;
    type
      Buffer = array[1..2*HalfBufSize+1] of char;
    var
      Buf       : Buffer;
      QResult   : longword;
      Header    : rHeaderFlag;
      ZSeek     : longint;
      ZSize     : longint;
      FileSmall : boolean;
      ZRead     : word;
      ZPos      : longint;
      Found     : boolean;
      EndRec    : ZIPEndHeadType;

    begin
    GetCentrDirOfset := false;
    if StartOffset = 0
      then
        begin
        ZSize := ZIPFile.GetSize;
        ZSeek := ZSize - 2 * HalfBufSize;
        if ZSeek < 0
          then
            begin
            ZSeek     := 0;
            FileSmall := true;
            ZRead     := ZSize;
            end
          else
            begin
            FileSmall := false;
            ZRead     := 2 * HalfBufSize;
            end;
        ZIPFile.Seek(ZSeek);
        ZIPFile.ReadExt(Buf, ZRead, QResult);
        if QResult <> ZRead then exit;

        ZPos := RPos(Buf, ZRead, 'PK'#5#6);
        Found := ZPos > 0;
        if not FileSmall then
          begin
          ZRead := 2 * HalfBufSize;
          while not Found and (ZSeek > 0) and ((ZSize - ZSeek) < $0FFF0) do
            begin
            ZSeek := ZSeek - HalfBufSize;
            if ZSeek < 0 then ZSeek := 0;
            ZIPFile.Seek(ZSeek);
            ZIPFile.ReadExt(Buf, ZRead, QResult);
            if QResult <> ZRead then exit;
            ZPos := RPos(Buf, ZRead, 'PK'#5#6);
            Found := ZPos > 0;
            end;
          end;

        if not Found then // central directroy not found
          begin
          Result := SlowScanZipFile (Col, WholeFileSpec, 0);
          exit;
          end;
        ZSeek := ZSeek + ZPos + 3 {- 1 + 4};
        end
      else
        ZSeek := StartOffset + 4;
    ZIPFile.Seek(ZSeek);
    ZIPFile.ReadExt(EndRec, SizeOf(EndRec), QResult);
    if QResult <>  SizeOf(EndRec) then exit;
    with EndRec do
      begin
      if rNumOfThisDisk <> rNumOfStartDisk then exit; // central dir is on multiple disks
      ZIPSplitted := rNumOfThisDisk > 0;
      ZSeek := ZSeek - rSizeOfCentralDir - 4;
      if ZSeek < 0 then exit;
      ZIPFile.Seek(ZSeek);
      ZIPFile.ReadExt(Header, SizeOf(Header), QResult);
      if Header <> 'PK'#1#2 then exit;
      ZIPFile.Seek(ZSeek);
      end;

    GetCentrDirOfset := true;
    end;


   function GetNextFile: boolean;

    type
      Buffer = array[1..256] of char;
    var
      Buf       : ^Buffer;
      QResult   : longword;
      LastSlash : byte;
      TmpPath   : ShortString;
      i         : byte;
      Header    : rHeaderFlag;

    begin
    GetNextFile := false;
    ZIPFile.ReadExt(Header, SizeOf(Header), QResult);
    if Header <> 'PK'#1#2 then exit;

    ZIPFile.ReadExt(ZIPCenHead, SizeOf(ZIPCenHead), QResult);
    if QResult <> SizeOf(ZIPCenHead) then exit;
    with ZIPCenHead do
      begin
      GetMem(Buf,rNameLength);
      ZIPFile.ReadExt(Buf^, rNameLength, QResult);
      if rNameLength > 255 then
        begin
        Move(Buf^[rNameLength-255+1], Buf^[1], 255);
        rNameLength := 255;
        end;
      LongName[0] := char(rNameLength);
      move(Buf^[1], LongName[1], rNameLength);
      FreeMem(Buf,rNameLength);
      if QResult <> rNameLength then exit;
      ///OemToCharBuff(@LongName[1], @LongName[1], rNameLength);

      if (rExtraLength > 0) or (rCommentLength > 0) then
        ZIPFile.Seek(ZIPFile.GetPos + rExtraLength + rCommentLength);
      end;

    GetNextFile := true;
    with ZIPCenHead do
      begin
      if (LongName[0] > #1) and (LongName[2]= ':') then ShortDelete(LongName,1,2);
      LastSlash := 0;
      for i := 1 to length(LongName) do
        begin
        if LongName[i] = '/' then LongName[i]   := '\';
        if LongName[i] = '\' then LastSlash := i;
        end;
      if LastSlash <> 0
        then
          begin
          TmpPath := ShortCopy(LongName, 1, LastSlash);
          ShortDelete (LongName, 1, LastSlash);
          end
        else
          TmpPath := '';
      if TmpPath <> FPath then FPath := TmpPath;
      end;
    end;



  var
    TmpStrArr : array[0..256] of char;

  begin
  try
    Result := false;
    ZIPSplitted  := true; // because of DirInfo
    ZIPEofStatus := false;
    FPath     := '';
    {--- opening ---}
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    ZIPFile.Init (StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);

    if not GetCentrDirOfset(StartOffset) then
      begin
      ZIPFile.Done;
      exit;
      end;

    if not GetNextFile then
      begin
      ZIPFile.Done;
      exit;
      end;

    while not ZIPEofStatus do
      begin
      with ZIPCenHead do
        begin
        LongName := TrimSpaces(LongName);
        if LongName <> '' then
          Col^.Insert(New (POneArcItem, Init(FPath, LongName, rSize,
                           rDateTime, 0, rRelOfsLocHead)));
        // rRelOfsLocHead is invalid for EXE files, as the first file
        // has offset 0 and not the code size
        Result := true;
        end;
      if not GetNextFile then ZIPEofStatus := true;
      end;

    ZIPFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans PkARC file

function ScanARCFile (Col: PArcCollection; WholeFileSpec: ShortString): boolean;

  type
    ARCRawType = record
      rHeaderFlag      : byte;
      rCompVersion     : byte;
      rName            : array [1..13] of char;
      rCompSize        : LongWord;
      rDate            : word;
      rTime            : word;
      rCRC             : word;
      rSize            : LongWord;
      end;

  var
    ARCFile          : TQBufStream;
    ARCEOFStatus     : boolean;
    ARCRaw           : ARCRawType;
    fName            : string[12];


   function GetFirstFile (First: boolean): boolean;

    var
      QResult   : longword;
      i         : Integer;

    begin
    GetFirstFile := false;
    ArcFile.ReadExt(ARCRaw, SizeOf(ARCRaw), QResult);
    if QResult <> SizeOf(ARCRaw) then exit;
    if ArcRaw.rHeaderFlag <> $1A then
      begin
      exit;
      end;

    with ARCRaw do
      begin
      FillChar (fName, SizeOf(fName), ' ');
      i := pos(#0, rName) - 1;
      if (i < 1) or (i > 12) then exit;
      GetFirstFile := true;
      move (rName[1], fName[1], i);
      fName[0] := chr(12);
      end;
    end;


   function GetNextFile: boolean;

    var
      FPos : LongWord;

    begin
    GetNextFile := true;
    FPos := ARCFile.GetPos + ARCRaw.rCompSize;
    if FPos < ARCFile.GetSize
      then
        begin
        ARCFile.Seek(FPos);
        if not GetFirstFile(false) then
          begin
          GetNextFile := false;
          ARCEofStatus := true;
          end;
        end
      else
        begin
        ARCEofStatus := true;
        GetNextFile  := false;
        end;
    end;


  var
    TmpStrArr: array[0..256] of char;

  begin
  try
    Result  := false;
    ARCEofStatus := False;
    {--- opening ---}
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    ARCFile.Init (StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);

    if not GetFirstFile(true) then
      begin
      ARCFile.Done;
      exit;
      end;
    while not ARCEofStatus do
      begin
      with ARCRaw do
        begin
        fName := TrimSpaces(fName);
        Col^.Insert(New (POneArcItem, Init('', fName, rSize,
                        longint(rTime) or (longint(rDate) shl 16), 0, 0)));
        Result := true;
        end;
      GetNextFile;
      end;

    ARCFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans BAK file (Microsoft old backup)

function ScanBAKFile (Col: PArcCollection; WholeFileSpec: ShortString): boolean;

  var
    BakFile     : TQBufStream;
    i           : byte;
    Line        : ShortString;
    Len         : byte;
    FPath       : ShortString;
    QSearchRec  : TQSearchRec;
    TmpStrArr   : array[0..256] of char;

  begin
  try
    Result := false;
    FPath := '';
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    BakFile.Init(StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);

    while not BakFile.Eof do
      begin
      BakFile.Read(Len,1);
      BakFile.Read(Line[1],pred(len));
      Line[0] := chr(pred(Len));
      if Len < 40
        then
          begin
          with QSearchRec do
            begin
            move(line[1],Name[1],12);
            Name[0] := #12;
            for i := 1 to length(Name) do if Name[i] < #32 then Name[i] := ' ';
            while ShortCopy(Name,length(Name),1) = ' ' do
              ShortDelete(Name,length(Name),1);
            move(line[28],Attr,1);
            move(line[24],Size,4);
            move(line[30],Time,4);
            Col^.Insert(New (POneArcItem, Init(FPath, Name, Size, Time, Attr, 0)));
            end;
          Result := true;
          end
        else
          if Len < 75
            then
              begin
              FPath := line;
              for i := 1 to length(FPath) do
                if FPath[i] < #32 then FPath[i] := ' ';
              i := 1;
              while (i < length(FPath)) and (FPath[i] <> ' ') do inc(i);
              dec(i);
              FPath[0] := chr(i);
              if FPath[length(FPath)] <> '\' then FPath := FPath + '\';
              end
            else
              begin
              for i := 1 to length(Line) do
                if Line[i] < #32 then Line[i] := ' ';
              if ShortCopy(line,1,6) <> 'BACKUP' then
                begin
                BakFile.Done;
                exit;
                end;
              end;
      end;
    BakFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans CPB file CentralPointBackup (PCTools 7)

function ScanPCBFile (Col: PArcCollection; WholeFileSpec: ShortString; CPB: boolean): boolean;

  var
    Rec       : array[0 .. 127] of byte;
    FPath     : ShortString;
    QSearchRec: TQSearchRec;

   procedure WritelnRec;

    var
      i           : byte;
      SwapTime    : array[1..2] of word;
      STime       : longint absolute SwapTime;
      Path        : ShortString;

      begin
      case Rec[0] of
        $DA:  // next subfolder
          begin
          i := 1;
          while (Rec[i+1] <> 0) and (i < 16) do
            begin
            Path[i] := chr(Rec[i+1]);
            inc(i);
            end;
          Path[0] := chr(pred(i));
          FPath := FPath + Path;
          if ShortCopy(FPath,2,1) = ':' then ShortDelete(FPath,1,2);
          if FPath[length(FPath)] <> '\' then FPath := FPath + '\';
          end;
        $DC:  // one level up
          begin
          i := pred(length(FPath));
          while (i > 1) and (FPath[i] <> '\') do dec(i);
          if i > 0 then
            ShortDelete(FPath,succ(i),length(FPath)-i);
          if FPath[length(FPath)] <> '\' then FPath := FPath + '\';
          end;
        $DB:  // file
          begin
          with QSearchRec do
            begin
            Attr := 0;
            i := 1;
            while (Rec[i+1] <> 0) and (i < 13) do
              begin
              Name[i] := chr(Rec[i+1]);
              inc(i);
              end;
            Name[0] := chr(pred(i));
            move(Rec[20],Size,4);
            move(Rec[16],SwapTime[2],2);
            move(Rec[18],SwapTime[1],2);
            Time := STime;
            Col^.Insert(New (POneArcItem, Init(FPath, Name, Size, Time, Attr, 0)));
            end;
          Result := true;
          end;
        end;
      end;


  var
    PCBFile     : TQBufStream;
    ID          : array[1..4] of char;
    Found       : boolean;
    SavePos     : longint;
    Rec512      : array[1..512] of char;
    EndOfHeader : boolean;
    QResult     : longword;
    TmpStrArr   : array[0..256] of char;

  begin
  try
    Result := false;

    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    SavePos := 0;
    Found := false;
    PCBFile.Init (StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    FPath := '';
    Rec[0] := 0;

    if CPB
      then
        begin
        while not (PCBFile.Eof or Found) do
          begin
          SavePos := PCBFile.GetPos;
          PCBFile.ReadExt(Rec512, 512, QResult);
          if (Rec512[1] =  #1) and (Rec512[2] = 'D') and
             (Rec512[3] = 'C') and (Rec512[4] = 'P') then Found := true;
          end;
        if Found then PCBFile.Seek(SavePos);
        end
      else
        begin
        PCBFile.Read(ID, 4);
        if ID = #1'DCP' then Found := true;
        end;

    if not Found then
      begin
      CB_AppendLineToIndicator(labPCBHlavaNenalezena);
      exit;
      end;

    while not PCBFile.Eof and (Rec[0] <> $DA) do
      PCBFile.Read(Rec[0], 1);

    if PCBFile.Eof then exit;
    PCBFile.Read(Rec[1], 15);
    writelnRec;

    EndOfHeader := false;
    while not (PCBFile.Eof or EndOfHeader) do
      begin
      PCBFile.Read(Rec[0], 1);
      if PCBFile.Eof then EndOfHeader := true;
      case Rec[0] of
        $DA : PCBFile.Read(Rec[1], 15);
        $DB : PCBFile.Read(Rec[1], 27);
        $DC : ;
        else EndOfHeader := true;
        end;
      writelnRec;
      end;

    PCBFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans Norton backup file

function ScanNBFile (Col: PArcCollection; WholeFileSpec: ShortString): boolean;

  const
    Delka = 32;
    Signature = #0'NORTON Ver 1';

  type
    TPolozka = array [1..Delka] of char;

  var
    NBFile      : TQBufStream;
    P           : TPolozka;
    FName       : ShortString;
    FExt        : ShortString;
    i,j         : Integer;
    FPath       : ShortString;
    CP          : array [1..length(Signature)] of char;
    QSearchRec  : TQSearchRec;
    DirLevel    : shortint;
    ActualLevel : shortint;
    ToSeek      : longint;
    TmpStrArr   : array[0..256] of char;

  begin
  try
    Result := false;
    ToSeek  := 0;
    NBFile.Init (StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    NBFile.Read(P, 1);
    move (P, CP, length(Signature));
    if CP <> Signature then
      begin
      NBFile.Done;
      exit;
      end;
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);

    inc(ToSeek, 16);
    NBFile.Seek(ToSeek);
    FPath := '';
    ActualLevel := 0;
    while not (NBFile.Eof or (P[1] = #$FF)) do
      begin
      NBFile.Read(P, 1);
      move (P, CP, length(Signature));
      if CP = Signature then
        begin
        inc(ToSeek, 16);
        NBFile.Seek(ToSeek);
        continue;
        end;
      if P[1] = #$FF then break;
      if P[1] = #0
        then
          begin
          move(P[ 2], FName[1], 8);
          i := 8;
          while (i > 0) and (FName[i] <= ' ') do dec(i);
          FName [0] := char(i);

          move(P[10], FExt [1], 3);
          i := 3;
          while (i > 0) and (FExt[i] <= ' ') do dec(i);
          FExt[0] := char(i);
          with QSearchRec do
            begin
            Name := FName + '.' + FExt;
            move(P[13], Attr, 1);
            move(P[23], Time, 4);
            move(P[29], Size, 4);
            Col^.Insert(New (POneArcItem, Init(
              ShortCopy(FPath, 2, pred(length(FPath))),
              Name, Size, Time, Attr, 0)));
            end;
          Result := true;
          end
        else
          begin
          FillChar(QSearchRec, SizeOf(QSearchRec), 0);
          move(P[ 1], FName[1], 8);
          i := 8;
          while (i > 0) and (FName[i] <= ' ') do dec(i);
          FName [0] := char(i);

          move(P[9], FExt [1], 3);
          i := 3;
          while (i > 0) and (FExt[i] <= ' ') do dec(i);
          FExt[0] := char(i);
          QSearchRec.Name := FName + '.' + FExt;
          QSearchRec.Attr := faDirectory;
          DirLevel := byte(P[12]);
          if DirLevel = 0 then Continue;

          while DirLevel <= ActualLevel do
            begin
            if FPath <> '' then dec(FPath[0]);
            for j := length(FPath) downto 1 do
              if FPath[j] = '\' then break;
            FPath[0] := char(j);
            dec(ActualLevel);
            end;
          if FPath[length(FPath)] <> '\' then FPath := FPath + '\';
          if FPath[2] = ':' then ShortDelete(FPath, 1, 2);
          FPath := FPath + FName + FExt + '\';
          Inc(ActualLevel);
          end;

      end;
    NBFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans LHarc file

function ScanLHArcFile (Col: PArcCollection; WholeFileSpec: ShortString;
                        StartOffset: longint): boolean;

  type
    TLzHead = record
      HeadSiz: byte;
      HeadChk: byte;
      HeadID : array[1..5] of char;
      PacSiz : longint;
      OrgSiz : longint;
      Ftime  : longint; {union with stamp}
      Attr   : word;
      Fname  : ShortString;
      end;

  var
    LZHFile     : TQBufStream;
    CurPos      : longint;
    MaxPos      : longint;
    Dir         : ShortString;
    LzHead      : TLzHead;
    LastSlash   : Integer;
    i           : Integer;
    OneArcItem  : POneArcItem;
    TmpStrArr   : array[0..256] of char;
    LocHeadOffset: longint;

  begin
  try
    Result  := false;
    {--- opening ---}
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    LZHFile.Init (StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    if StartOffset >= 2 then dec(StartOffset, 2);
    // the header begins by 2 bytes earlier than -lz
    CurPos := StartOffset;
    MaxPos := LZHFile.GetSize;
    if MaxPos < SizeOf(TLzHead) then exit;
    MaxPos := MaxPos - SizeOf(TLzHead);

    while not (LZHFile.Eof or (CurPos > MaxPos)) do
      begin
      LZHFile.Seek(CurPos);
      LocHeadOffset := LZHFile.GetPos;
      LZHFile.Read(LzHead, sizeOf(LzHead));
      //check if it is LHARC - HeadID should be one of: "-lz4-", "-lz5-", "-lh0-", "-lh1-"
      if (LzHead.HeadID[1] <> '-') or (LzHead.HeadID[2] <> 'l') or
         (LzHead.HeadID[5] <> '-') then
           begin
           LZHFile.Done;
           exit;
           end;

      LastSlash := 0;
      for i := 1 to length(LzHead.Fname) do
        begin
        if LzHead.Fname[i] = '/' then LzHead.Fname[i] := '\';
        if LzHead.Fname[i] = '\' then LastSlash := i;
        end;
      if LastSlash <> 0
        then
          begin
          Dir := ShortCopy(LzHead.Fname, 1, LastSlash);
          ShortDelete (LzHead.Fname, 1, LastSlash);
          end
        else
          Dir := '';

      OneArcItem := New (POneArcItem, Init(Dir, LzHead.Fname,
        LzHead.OrgSiz, LzHead.Ftime, byte(LzHead.Attr), LocHeadOffset));
      Col^.Insert(OneArcItem);
      CurPos := CurPos + LzHead.HeadSiz + LzHead.PacSiz + 2;
      end;
    Result := true;
    LZHFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans ARJ file

function ScanARJFile (Col: PArcCollection; WholeFileSpec: ShortString;
                      StartOffset: longint): boolean;

  const
    HEADER_ID       = $EA60;
    HEADER_ID_HI    =   $EA;
    HEADER_ID_LO    =   $60;
    FNAME_MAX       =   512;
    COMMENT_MAX     =  2048;
    FIRST_HDR_SIZE  =    30;
    HEADERSIZE_MAX  =  FIRST_HDR_SIZE + 10 + FNAME_MAX + COMMENT_MAX;
    HEADERSIZE_MIN  =  FIRST_HDR_SIZE + 10;
    CRC_MASK        = $FFFFFFFF;
    CRCPOLY         = $EDB88320;

    GARBLED_FLAG = $01;
    RESERVED     = $02;
    VOLUME_FLAG  = $04;
    EXTFILE_FLAG = $08;
    PATHSYM_FLAG = $10;
    BACKUP_FLAG  = $20;

    MaxPossibleHeaders = 100;
    // FType     (0 = binary, 1 = text, 2 = comment header,
    //            3 = directory, 4 = volume label)

  type
    THeader  = array[0..HEADERSIZE_MAX] of byte;
    PHeader  = ^THeader;

    THeaderStruct = record
      HeaderId     : word;
      HeaderSize   : word;
      HeaderCrc    : LongWord;
      FirstHdrSize : byte;
      ArjVer       : byte;
      ArjVerNeeded : byte;
      HostOS       : byte;
      ArjFlags     : byte;
      Method       : byte;
      FileType     : byte;
      Reserved     : byte;
      TimeStamp    : LongWord;
      CompSize     : LongWord;
      OrigSize     : LongWord;
      FileCRC      : LongWord;
      EntryPos     : word;
      FMode        : word;
      HostData     : word;
      FileName     : ShortString;
      ExtHeaderSize: word;
      end;
    PHeaderStruct = ^THeaderStruct;

  var
    CRC         : LongWord;
    CRCTable    : array [0..255] of LongWord;
    Header      : PHeader;
    HeaderStruct: PHeaderStruct;
    LastPos     : LongWord;
    PossibleHeaders: array[1..MaxPossibleHeaders] of LongWord;
    LocHeadOffset  : LongWord;


   procedure MakeCRCTable;

    var
      i, j: longint;
      r: LongWord;

    begin
    for i := 0 to 255 do
      begin
      r := i;
      for j := 8 downto 1 do
        begin
        if (r and 1) <> 0
          then r := (r shr 1) xor CRCPOLY
          else r := r shr 1;
        end;
      crctable[i] := r;
      end;
    end;


   procedure crc_buf (var str: THeader; len: LongWord);

    var
      i : longint;

    begin
    for i := 0 to len-1 do
      crc:= crctable[(crc xor str[i]) and $FF] xor (crc shr 8)
    end;


   function FindFirstPossibleHeaders(var F: TQBufStream): boolean;

    const
      MaxBytesToScan = $FFF0; {63*1024;}
    type
      TBuffer = array[0..MaxBytesToScan] of char;
      PTBuffer = ^TBuffer;
    var
      PBuffer  : PTBuffer;
      ScanBytes: LongWord;
      SavePos  : LongWord;
      i        : longint;
      QResult  : LongWord;
      FoundCounter: longint;

    begin
    Result := false;
    FillChar(PossibleHeaders, sizeof(PossibleHeaders), 0);
    SavePos := F.GetPos;
    F.Seek(0);
    ScanBytes := F.GetSize;
    if ScanBytes > MaxBytesToScan then ScanBytes := MaxBytesToScan;
    GetMem(PBuffer, ScanBytes);
    F.ReadExt(PBuffer^, ScanBytes, QResult);
    FoundCounter := 0;
    if QResult = ScanBytes then
      begin
      for i := 0 to ScanBytes-2 do
        begin
        if (byte(PBuffer^[i]) = HEADER_ID_LO) and
           (byte(PBuffer^[succ(i)]) = HEADER_ID_HI) then
          begin
          if FoundCounter < MaxPossibleHeaders then
            begin
            inc(FoundCounter);
            PossibleHeaders[FoundCounter] := i;
            Result := true;
            end;
          end;
        end;
      end;
    FreeMem(PBuffer, ScanBytes);
    F.Seek(SavePos);
    end;


   function FindHeader (var F: TQBufStream): longint;

    const
      LimitForSearch = 1024; {if not found in 1 kB, exit}
    var
      ArcPos : LongWord;
      C      : byte;
      LimitCounter: LongWord;


      begin
      FindHeader := -1;
      LimitCounter := LimitForSearch;
      ArcPos  := F.GetPos;
      while (ArcPos < LastPos) do
        begin
        F.Seek(ArcPos);
        F.Read(C, 1);
        while (ArcPos < LastPos) do
          begin
          dec(LimitCounter);
          if LimitCounter = 0 then exit;
          if (c <> HEADER_ID_LO)
              then
                F.Read(c, 1)
              else
                begin
                F.Read(c, 1);
                if (c = HEADER_ID_HI) then break;
                end;
          inc(ArcPos);
          end;
        if (ArcPos >= LastPos) then break;
        LocHeadOffset  := F.GetPos - 2;
        F.Read(HeaderStruct^.HeaderSize, 2);
        if HeaderStruct^.HeaderSize = 0 then exit; {it is the end of archive}
        if HeaderStruct^.HeaderSize <= HEADERSIZE_MAX then
          begin
          crc := CRC_MASK;
          F.Read(Header^, HeaderStruct^.HeaderSize);
          crc_buf(Header^, HeaderStruct^.HeaderSize);
          F.Read(HeaderStruct^.HeaderCrc, 4);
          if (crc xor CRC_MASK) = HeaderStruct^.HeaderCrc then
            begin
            FindHeader := ArcPos;
            exit;
            end;
          end;
      inc(ArcPos);
      end;
    end;


   function ReadHeader (var F: TQBufStream): boolean;

    var
      StartPos, i : LongWord;
      HeadPos     : longint;
      SeekPos     : LongWord;

    begin
    ReadHeader := false;
    HeadPos := FindHeader(F); // reads also Header^
    if HeadPos = -1 then exit;
    with HeaderStruct^ do
      begin
      move(Header^, HeaderStruct^.FirstHdrSize, 30); // 30 is from FirstHeaderSize to HostData}

      StartPos := FirstHdrSize;
      i := StartPos;
      while (Header^[i] <> 0) and (i < HeaderSize) do inc (i);
      if (i - StartPos) > 255 then i := StartPos + 255;
      move(Header^[StartPos], FileName[1], i - StartPos);
      FileName[0] := char(i - StartPos);

      if (ArjFlags and PATHSYM_FLAG) <> 0 then
        for i := 1 to length(FileName) do
          if FileName[i] = '/' then
            FileName[i] := '\';

      F.Read(ExtHeaderSize, 2);
      if (ExtHeaderSize <> 0) then F.Seek(F.GetPos + ExtHeaderSize + 4);
      if (FileType <> 2) then
        begin
        SeekPos := F.GetPos + CompSize;
        if SeekPos > LastPos then SeekPos := LastPos;
        F.Seek(SeekPos);
        end;
      end;
    ReadHeader := true;
    end;


  var
    ARJFile          : TQBufStream;
    Dir              : ShortString;
    TmpStrArr        : array[0..256] of char;
    i                : Integer;
    LastSlash        : Integer;
    FoundHeader      : longint;

  begin
  try
    MakeCRCTable;
    Result  := false;
    {--- opening ---}
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    ArjFile.Init(StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    if StartOffset > 0 then ArjFile.Seek(StartOffset);

    LastPos := ArjFile.GetSize - HEADERSIZE_MIN;
    if LastPos <= 0 then
      begin
      ArjFile.Done;
      exit;
      end;

    New(Header);
    New(HeaderStruct);

    FoundHeader := FindHeader(ArjFile);
    if FoundHeader = -1 then {not found}
      begin
      if FindFirstPossibleHeaders(ArjFile) then
        begin
        i := 1;
        while (PossibleHeaders[i] > 0) and (FoundHeader = -1) do
          begin
          ArjFile.Seek(PossibleHeaders[i]);
          FoundHeader := FindHeader(ArjFile);
          inc(i);
          end;
        end;
      end;
    if FoundHeader = -1 then
      begin
      ArjFile.Done;
      Dispose(HeaderStruct);
      Dispose(Header);
      exit;
      end;
    ArjFile.Seek(FoundHeader);

    while ReadHeader(ArjFile) do
      with HeaderStruct^ do
        begin
        LastSlash := 0;
        for i := length(FileName) downto 1 do
          if FileName[i] = '\' then
            begin
            LastSlash := i;
            break;
            end;
        if LastSlash <> 0
          then
            begin
            Dir := ShortCopy(FileName, 1, LastSlash);
            ShortDelete (FileName, 1, LastSlash);
            end
          else
            Dir := '';
        Col^.Insert(New (POneArcItem, Init(Dir, FileName, OrigSize,
                         TimeStamp, 0, LocHeadOffset)));
        Result := true;
        end;
    Dispose(HeaderStruct);
    Dispose(Header);
    ArjFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// Scans RAR file by supplied DLL from RAR developers

function ScanRARFileByDll (Col: PArcCollection; WholeFileSpec: ShortString;
                           StartOffset: longint): boolean;

  var
    hRarArchive: integer;
    RarOpenArchiveData: TRarOpenArchiveData;
    RarHeaderData : TRarHeaderData;
    ZString: array[0..256] of char;
    RHCode       : longint;
    PFCode       : longint;
    Dir          : ShortString;
    FileName     : ShortString;
    iLen         : integer;
    LastSlash    : integer;
    i            : integer;

  begin
  Result := false;
  if not UnRarDllLoaded then exit;

  StrPCopy(ZString, WholeFileSpec);
  RarOpenArchiveData.ArcName    := @ZString;
  RarOpenArchiveData.OpenMode   := RAR_OM_LIST;
  RarOpenArchiveData.OpenResult := 0;
  RarOpenArchiveData.CmtBuf     := nil;
  RarOpenArchiveData.CmtBufSize := 0;
  RarOpenArchiveData.CmtSize    := 0;
  RarOpenArchiveData.CmtState   := 0;

  hRarArchive := RarOpenArchive(RarOpenArchiveData);
  if (RarOpenArchiveData.OpenResult <> 0) then // unsuccessful
    begin
    RarCloseArchive(hRarArchive);
    exit;
    end;

  repeat
    RHCode:= RARReadHeader(hRarArchive, RarHeaderData);
    if RHCode<>0 then Break;

    if (RarHeaderData.FileAttr and (faDirectory or faVolumeID) = 0) then
      begin
      iLen := strlen(RarHeaderData.FileName);
      if (iLen > 254) then iLen := 254;
      FileName[0] := char (iLen);
      ///OemToCharBuff(@(RarHeaderData.FileName), @(FileName[1]), iLen);
      LastSlash := 0;
      for i := 1 to length(FileName) do
        begin
        if FileName[i] = '/' then FileName[i] := '\';
        if FileName[i] = '\' then LastSlash := i;
        end;
      if LastSlash <> 0
        then
          begin
          Dir := ShortCopy(FileName, 1, LastSlash);
          ShortDelete (FileName, 1, LastSlash);
          end
        else
          Dir := '';
      Col^.Insert(New (POneArcItem, Init(Dir, FileName, RarHeaderData.UnpSize,
                       RarHeaderData.FileTime, RarHeaderData.FileAttr,
                       0)));
      Result := true;
      end;

    PFCode:= RARProcessFile(hRarArchive, RAR_SKIP, nil, nil);
    if (PFCode<>0) then Break;
  until False;

  RarCloseArchive(hRarArchive);
  end;

//-----------------------------------------------------------------------------
// Scans RAR file internally

function ScanRARFile (Col: PArcCollection; WholeFileSpec: ShortString;
                      StartOffset: longint): boolean;


  type
    THeaderStruct = record
      HeadCRC      : word;
      HeadType     : byte;
      HeadFlags    : word;
      HeadSize     : word;
      end;

    TFHeaderStruct = record
      CompSize : longint;
      OrigSize : longint;
      HostOS   : byte;
      FileCRC  : longint;
      DateTime : longint;
      UnpVer   : byte;
      Method   : byte;
      NameSize : word;
      Attr     : longint;
      end;

  var
    HeaderStruct : THeaderStruct;
    FHeaderStruct: TFHeaderStruct;
    FileName     : ShortString;
    RARFileSize  : longint;
    LocHeadOffset: longint;


   function ReadBlock (var F: TQBufStream): boolean;

    var
      AddSize      : longint;
      SeekTo       : longint;
      SavePos      : longint;

    begin
    ReadBlock := false;
    AddSize := 0;
    SavePos := F.GetPos;
    LocHeadOffset := F.GetPos;
    if SavePos = RARFileSize then exit; // we are at the end
    F.Read(HeaderStruct, SizeOf(HeaderStruct));
    with HeaderStruct do
      begin
      case HeadType of
        $72: {marker block}
             begin
             if (HeadFlags and $8000) <> 0 then F.Read(AddSize, 4);
             SeekTo := SavePos + HeadSize + AddSize;
             end;
        $73: {archive header}
             begin
             if (HeadFlags and $8000) <> 0 then F.Read(AddSize, 4);
             SeekTo := SavePos + HeadSize + AddSize;
             end;
        $74: {file header}
             begin
             F.Read(FHeaderStruct, SizeOf(FHeaderStruct));
             with FHeaderStruct do
               begin
               if NameSize > 255 then NameSize := 255;
               FileName[0] := char(NameSize);
               F.Read(FileName[1], NameSize);
               SeekTo := SavePos + HeadSize + CompSize;
               end;
             end;
        $75: {comment header}
             begin
             if (HeadFlags and $8000) <> 0 then F.Read(AddSize, 4);
             SeekTo := SavePos + HeadSize + AddSize;
             end;
        $76: {extra information}
             begin
             if (HeadFlags and $8000) <> 0 then F.Read(AddSize, 4);
             SeekTo := SavePos + HeadSize + AddSize;
             end;
        else exit;
        end; {case}
      if SeekTo > RarFileSize then exit;
      F.Seek(SeekTo);
      end; {with}
    ReadBlock := true;
    end;

  var
    RARFile          : TQBufStream;
    Dir              : ShortString;
    Identifier       : array[1..4] of char;
    TmpStrArr        : array[0..256] of char;
    LastSlash        : Integer;
    i                : Integer;

  begin
  if ScanRARFileByDll (Col, WholeFileSpec, StartOffset) then
    begin
    Result := true;
    exit;
    end;
  try
    Result := false;
    {--- opening ---}
    CB_NewLineToIndicator(labZjistujiObsah + WholeFileSpec);
    RARFile.Init(StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    if StartOffset > 0 then RarFile.Seek(StartOffset);

    RARFileSize := RARFile.GetSize;
    RARFile.Read(Identifier, 4);
    if Identifier = 'Rar!' then
      begin
      RARFile.Seek(RARFile.GetPos-4);
      while ReadBlock(RARFile) do
        if (HeaderStruct.HeadType = $74) and
           (FHeaderStruct.Attr and (faDirectory or faVolumeID) = 0) then
          with FHeaderStruct do
            begin
            LastSlash := 0;
            for i := 1 to length(FileName) do
              begin
              if FileName[i] = '/' then FileName[i] := '\';
              if FileName[i] = '\' then LastSlash := i;
              end;
            if LastSlash <> 0
              then
                begin
                Dir := ShortCopy(FileName, 1, LastSlash);
                ShortDelete (FileName, 1, LastSlash);
                end
              else
                Dir := '';
            Col^.Insert(New (POneArcItem, Init(Dir, FileName, OrigSize,
                             DateTime, Attr, LocHeadOffset)));
            Result := true;
            end;
      end;
    RARFile.Done;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//=============================================================================
// ACE file scanning, using external DLL

var
  AceCommentBuf: array[0..COMMENTBUFSIZE-1] of char;
  AceCol: PArcCollection;


// ACE callback functions
function AceInfoProc(Info : pACEInfoCallbackProcStruc) : integer; stdcall;
  begin
  Result:=ACE_CALLBACK_RETURN_OK;
  end;

function AceErrorProc(Error : pACEErrorCallbackProcStruc) : integer; stdcall;
  begin
  Result:=ACE_CALLBACK_RETURN_CANCEL;
  end;

function AceRequestProc(Request : pACERequestCallbackProcStruc) : integer; stdcall;
  begin
  Result:=ACE_CALLBACK_RETURN_CANCEL;
  end;

function AceStateProc(State: pACEStateCallbackProcStruc) : integer; stdcall;

  var
    Dir          : ShortString;
    FileName     : ShortString;
    iLen         : integer;
    LastSlash    : integer;
    i            : integer;

  begin
  Result:=ACE_CALLBACK_RETURN_OK;
    case State^.StructureType of
      ACE_CALLBACK_TYPE_ARCHIVE:
        begin
        iLen := strlen(State^.Archive.ArchiveData^.ArchiveName);
        if (iLen > 254) then iLen := 254;
        FileName[0] := char (iLen);
        ///CopyMemory(@FileName[1], State^.Archive.ArchiveData^.ArchiveName, iLen);
        end;
      ACE_CALLBACK_TYPE_ARCHIVEDFILE:
        begin
        if ((State^.ArchivedFile.Code = ACE_CALLBACK_STATE_STARTFILE) and
            (State^.ArchivedFile.FileData^.Attributes and (faDirectory or faVolumeID) = 0)) then
          begin
          FillChar(FileName, sizeof(FileName), 0);
          iLen := strlen(State^.ArchivedFile.FileData^.SourceFileName);
          if (iLen > 254) then iLen := 254;
          FileName[0] := char (iLen);
          ///CopyMemory(@FileName[1], State^.ArchivedFile.FileData^.SourceFileName, iLen);
          LastSlash := 0;
          for i := 1 to length(FileName) do
            begin
            if FileName[i] = '/' then FileName[i] := '\';
            if FileName[i] = '\' then LastSlash := i;
            end;
          if LastSlash <> 0
            then
              begin
              Dir := ShortCopy(FileName, 1, LastSlash);
              ShortDelete (FileName, 1, LastSlash);
              end
            else
              Dir := '';
           AceCol^.Insert(New (POneArcItem, Init(Dir, FileName,
                          State^.ArchivedFile.FileData^.Size,
                          State^.ArchivedFile.FileData^.Time,
                          State^.ArchivedFile.FileData^.Attributes,
                          0)));
           exit;
           end;
        end;
      end;

  Result:=ACE_CALLBACK_RETURN_OK;
  end;

//-----------------------------------------------------------------------------

function ScanACEFile (Col: PArcCollection; WholeFileSpec: ShortString;
                      StartOffset: longint): boolean;

  var
    DllData      : tACEInitDllStruc;
    zTempDir     : array[0..255] of char;
    List         : tACEListStruc;
    ZString: array[0..256] of char;

  begin
  Result := false;
  if not UnAceDllLoaded then exit;
  FillChar(DllData, SizeOf(DllData), 0);
  DllData.GlobalData.MaxArchiveTestBytes := $1ffFF;      // search for archive
                                                         // header in first 128k
                                                         // of file
  DllData.GlobalData.MaxFileBufSize      := $2ffFF;      // read/write buffer size
                                                         // is 256k
  DllData.GlobalData.Comment.BufSize     := SizeOf(AceCommentBuf)-1;
  DllData.GlobalData.Comment.Buf         := @AceCommentBuf; // set comment bufffer
                                                         // to receive comments
                                                         // of archive and/or
                                                         // set comments
  {$ifdef mswidnows}                                                   // set comments
  GetTempPath(255, @zTempDir);
  DllData.GlobalData.TempDir             := @zTempDir;
  {$else}
  DllData.GlobalData.TempDir := PChar(GetTempDir);
  {$endif}


  // set callback function pointers
  DllData.GlobalData.InfoCallbackProc    := @AceInfoProc;
  DllData.GlobalData.ErrorCallbackProc   := @AceErrorProc;
  DllData.GlobalData.RequestCallbackProc := @AceRequestProc;
  DllData.GlobalData.StateCallbackProc   := @AceStateProc;
  if (ACEInitDll(@DllData) <> 0) then exit;
  AceCol := Col;

  FillChar(List, SizeOf(List), 0);     // set all fields to zero
  List.Files.SourceDir   := '';        // archive main directory is
                                       // base directory for FileList
  List.Files.FileList    := '';        // set FileList
  List.Files.ExcludeList := '';        // no files to exclude
  List.Files.FullMatch   := true;      // also list files partially matching
                                       // (for instance: list DIR1\TEST.DAT
                                       //  if FileList specifies TEST.DAT)
  StrPCopy(ZString, WholeFileSpec);
  Result:= ACEList(@ZString, @List) = 0;
  end;

//=============================================================================
// Tries to identify, if the EXE file is a self-extracting archive

function ScanExeFile (Col: PArcCollection; WholeFileSpec: ShortString;
                      ScanAlsoNonZipArchives: boolean;
                      var ArchiveType: TArchiveType): boolean;

  type
    String4 = String[4];

  const
    MaxBytesToScan = $FFF0; {65520}
    MaxBytesToScanZIP = 32*1024; // must be smaller than MaxBytesToScan
    RarID  : String4 = 'Rar!';
    ArjID  : String4 = #$60#$EA;
    LhaID1 : String4 = '-lh';
    LhaID2 : String4 = '-lz';
    ZipID  : String4 = 'PK'#3#4;
    ZipIDCentral : String4 = 'PK'#5#6;

  type
    TBuffer = array[0..MaxBytesToScan] of char;
    PTBuffer = ^TBuffer;

  var
    ExeFile     : TQBufStream;
    ScanBytes   : LongWord;
    ScanBytesZIP: LongWord;
    TmpStrArr   : array[0..256] of char;
    PBuffer     : PTBuffer;
    QResult     : LongWord;
    ScanString  : ShortString;
    BufScanPos  : LongWord;

    RarIDFound  : boolean;
    ArjIDFound  : boolean;
    ZipIDFound  : boolean;

    RarIDPos    : LongWord;
    ArjIDPos    : LongWord;
    ZipIDPos    : LongWord;

  procedure ScanForIDFromLeft(ID: String4; var IDFound: boolean; var IDPos: LongWord);
    var
      i : LongWord;
    begin
    if not IDFound then
      begin
      i := pos(ID, ScanString);
      if i > 0 then
        begin
        IDFound := true;
        IDPos := BufScanPos + i - 1;
        end;
      end;
    end;

  begin
  try
    Result := false;
    RarIDFound  := false;
    ArjIDFound  := false;
    ZipIDFound  := false;
    ArchiveType := atOther;
    RarIDPos    := 0;
    ArjIDPos    := 0;
    ZipIDPos    := 0;
    ExeFile.Init(StrPCopy(TmpStrArr, WholeFileSpec), stOpenReadNonExclusive);
    ScanBytes := ExeFile.GetSize;
    if ScanBytes = 0 then exit;
    if ScanBytes > MaxBytesToScan then ScanBytes := MaxBytesToScan;
    GetMem(PBuffer, ScanBytes);
    if PBuffer = nil then exit;
    // first check for central dir of ZIP file
    ScanBytesZIP := ExeFile.GetSize;
    if ScanBytesZIP > MaxBytesToScanZIP then ScanBytesZIP := MaxBytesToScanZIP;
    ExeFile.Seek(ExeFile.GetSize - ScanBytesZIP);
    ExeFile.ReadExt(PBuffer^, ScanBytesZIP, QResult);
    if QResult = ScanBytesZIP then
      begin
      ZipIDPos := RPos(PBuffer^, ScanBytesZIP, ZipIDCentral);
      if ZipIDPos <> 0 then
        begin
        ZipIDFound := true;
        ZipIDPos := ZipIDPos + ExeFile.GetSize - ScanBytesZIP - 1;
        end;
      end;

    if ZipIDFound then
      begin
      FreeMem(PBuffer, ScanBytes);
      ExeFile.Done;
      Result := ScanZIPFile(Col, WholeFileSpec, ZipIDPos);
      ArchiveType := atZip;
      exit;
      end;

    if not ScanAlsoNonZipArchives then
      begin
      FreeMem(PBuffer, ScanBytes);
      ExeFile.Done;
      Result := false;
      exit;
      end;
    // central dis not found, continue
    ExeFile.Seek(0);
    ExeFile.ReadExt(PBuffer^, ScanBytes, QResult);
    if QResult = ScanBytes then
      begin
      // find position
      ScanString[0] := #255;
      BufScanPos := 0;
      while (BufScanPos + 255) < ScanBytes do
        begin
        move(PBuffer^[BufScanPos], ScanString[1], 255);
        ScanForIDFromLeft(ZipID,  ZipIDFound, ZipIDPos);
        ScanForIDFromLeft(ArjID,  ArjIDFound, ArjIDPos);
        ScanForIDFromLeft(RarID,  RarIDFound, RarIDPos);
        inc(BufScanPos, 250);
        end;
      end;
    FreeMem(PBuffer, ScanBytes);
    ExeFile.Done;

    if ZipIDFound then
      begin
      Result := SlowScanZIPFile(Col, WholeFileSpec, ZipIDPos);
      ArchiveType := atZip;
      end;
    if (Result) then exit;

    if RarIDFound then
      begin
      Result := ScanRarFile(Col, WholeFileSpec, RarIDPos);
      ArchiveType := atRar;
      end;
    if (Result) then exit;

    if ArjIDFound then
      begin
      Result := ScanArjFile(Col, WholeFileSpec, ArjIDPos);
      ArchiveType := atArj;
      end;
    if (Result) then exit;

    Result := false;
  except
    on EQDirException do
      Result := false;
      end;
  end;

//-----------------------------------------------------------------------------

begin
end.
