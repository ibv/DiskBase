unit UConvQDir4;
(*====================================================================
Functions for converting data from QuickDir 4.0
======================================================================*)

{$A-}

interface

uses SysUtils,
     UCollections, UStream, UTypes, UBaseTypes;

procedure OpenQDir4Database (Name: ShortString);
function  ReadQDir4Entry (Position: longInt): boolean;
procedure CloseQDir4Database;
function  FindQDir4First (Path: ShortString;
                          var F: TQDir4SearchRec; var ItIsArchive: boolean): Integer;
function  FindQDir4Next   (var F: TQDir4SearchRec; var ItIsArchive: boolean): Integer;
function  GetQDir4DirDesc (Path: ShortString): pointer;

function  GetQDir4VolumeLabel: ShortString;
procedure GetQDir4RecordAttr (var aDiskSize, aDiskFree: longword; var aScanDate: longint);
procedure SetQDir4CharConversion (Kamen: boolean);
function  GetQDir4DiskDesc: pointer;

{--------------------------------------------------------------------}

implementation

uses UExceptions, ULang;

type
  TConvArr = array[#0..#255] of char;

const
  CharForArcDir = 'þ';

  KamToWin : TConvArr = ( {Kam to Win}
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,
  #16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,
  #32,#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,#47,
  #48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,#61,#62,#63,
  #64,#65,#66,#67,#68,#69,#70,#71,#72,#73,#74,#75,#76,#77,#78,#79,
  #80,#81,#82,#83,#84,#85,#86,#87,#88,#89,#90,#91,#92,#93,#94,#95,
  #96,#97,#98,#99,#100,#101,#102,#103,#104,#105,#106,#107,#108,#109,#110,#111,
  #112,#113,#114,#115,#116,#117,#118,#119,#120,#121,#122,#123,#124,#125,#126,#32,
  #200,#252,#233,#239,#228,#207,#141,#232,#236,#204,#197,#205,#190,#229,#196,#193,
  #201,#158,#142,#244,#246,#211,#249,#218,#253,#214,#220,#138,#188,#221,#216,#157,
  #225,#237,#243,#250,#242,#210,#218,#212,#154,#248,#224,#192,#32,#167,#187,#171,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#32,#32,#32,#32,#32,#181,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#177,#32,#32,#32,#32,#247,#32,#176,#32,#32,#32,#32,#32,#32,#32
  );


  LatToWin : TConvArr = (
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,
  #16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,
  #32,#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,#47,
  #48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,#61,#62,#63,
  #64,#65,#66,#67,#68,#69,#70,#71,#72,#73,#74,#75,#76,#77,#78,#79,
  #80,#81,#82,#83,#84,#85,#86,#87,#88,#89,#90,#91,#92,#93,#94,#95,
  #96,#97,#98,#99,#100,#101,#102,#103,#104,#105,#106,#107,#108,#109,#110,#111,
  #112,#113,#114,#115,#116,#117,#118,#119,#120,#121,#122,#123,#124,#125,#126,#32,
  #199,#252,#233,#226,#228,#249,#230,#231,#179,#235,#213,#245,#238,#143,#196,#198,
  #201,#197,#229,#244,#246,#188,#190,#140,#156,#214,#220,#141,#157,#163,#215,#232,
  #225,#237,#243,#250,#165,#185,#142,#158,#202,#234,#32,#159,#200,#186,#187,#171,
  #32,#32,#32,#32,#32,#193,#194,#204,#170,#32,#32,#32,#32,#175,#191,#32,
  #32,#32,#32,#32,#32,#32,#195,#227,#32,#32,#32,#32,#32,#32,#32,#164,
  #240,#208,#207,#203,#239,#210,#205,#206,#236,#32,#32,#32,#32,#222,#217,#32,
  #211,#223,#212,#209,#241,#242,#138,#154,#192,#218,#224,#219,#253,#221,#254,#180,
  #32,#189,#178,#161,#162,#167,#247,#184,#32,#168,#183,#251,#216,#248,#32,#32
  );

type
  NameStr     = string[8];
  ExtStr      = string[4];
  TKeyString  = string[12];
  DirStr      = ShortString;

  TFileType   = (ftFile, ftDir, ftParent, ftArchiveBegin, ftZip, ftArc,
                 ftArj, ftAr6, ftLzh, ftBak, ftPCB, ftNB, ftArchiveEnd);

  TLongString = record
    Len: word;
    Dta: array[1..8*1024] of char;
    end;
  PLongString = ^TLongString;

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TQCollection)
    QDirName      : PString;
    QDirInfo      : PLongString;
    ParentPos     : SmallInt;
    constructor Init(ALimit, ADelta: Integer; DName: ShortString; AParentPos: Integer);
    constructor InitLoad (var F: TQBufStream; ParentCol: PDirCollection);
    destructor  Done; virtual;
    function    FindDirCol (var Dir: ShortString; var Col: PDirCollection): boolean;
    end;

  POneFile16 = ^TOneFile16;
  TOneFile16 = object(TQObject)
    FileType : TFileType;
    Attr     : Byte;
    Time     : Longint;
    Size     : Longint;
    Name     : NameStr;
    Ext      : ExtStr;
    DirCol   : PDirCollection;
    constructor Init(SR: TQDir4SearchRec);
    constructor InitLoad  (var F: TQBufStream; ParentCol, OwnerCol: PDirCollection);
    destructor  Done; virtual;
    function    FindDirOne (var Dir: ShortString; var Col: PDirCollection): boolean;
    end;

const
    cOneFileSize = SizeOf(TFileType)
                 + SizeOf(byte)    {= TOneFile16.Attr}
                 + SizeOf(longint) {= TOneFile16.Time}
                 + SizeOf(longint) {= TOneFile16.Size}
                 + 1;              {1 pro FName[0]}
type
  PQDirRecord = ^TQDirRecord;
  TQDirRecord = object(TQObject)
    QDeleted    : boolean;
    KeyString   : TKeyString;
    QDiskSize   : longint;
    QDiskFree   : longint;
    QScanDate   : longint;
    QDescription: PLongString;
    CurrentCol  : PDirCollection;
    RootCol     : PDirCollection;
    constructor InitLoad (var F: TQBufStream);
    destructor  Done; virtual;
    end;

var
  QDir4Record : PQDirRecord;
  MainFile    : TQBufStream;
  ConvArr     : TConvArr;

//----------------------------------------------------------------------------

procedure FSplit (Path: ShortString; var Dir: DirStr;
                  var Name: NameStr; var Ext: ExtStr);

 var i: Integer;

  begin
  for i := length(Path) downto 1 do
    if (Path[i] = '.') or (Path[i]='\') then break;
  if Path[i] = '.'
    then
      begin
      Ext := ShortCopy(Path, i, length(Path)-i+1);
      ShortDelete(Path, i, length(Path)-i+1);
      end
    else
      Ext := '';
  for i := length(Path) downto 1 do
    if Path[i]='\' then break;
  if Path[i] = '\'
    then
      begin
      Name := ShortCopy(Path, i+1, length(Path)-i);
      ShortDelete(Path, i+1, length(Path)-i);
      end
    else
      Name := '';
  Dir := Path;
  end;

//----------------------------------------------------------------------------

function ConvStr(S: ShortString): ShortString;

var i: Integer;

  begin
  for i := 1 to length(S) do
    S[i] := ConvArr[S[i]];
  Result := S;
  end;

//=== TDirCollection ==========================================================

constructor TDirCollection.Init (ALimit, ADelta: Integer;
                                 DName: ShortString;
                                 AParentPos: Integer);

  begin
  TQCollection.Init(ALimit, ADelta);
  QDirName      := nil;
  QDirInfo      := nil;
  ParentPos     := 0;
  ShortDelete(DName,1,2);
  QDirName  := NewStr(DName);
  ParentPos := AParentPos;
  QDirInfo  := nil;
  end;

//----------------------------------------------------------------------------

destructor TDirCollection.Done;

  begin
  if QDirName  <> nil then DisposeStr(QDirName);
  if QDirInfo  <> nil then
    FreeMem(QDirInfo, QDirInfo^.Len+2);
  TQCollection.Done;
  end;

//----------------------------------------------------------------------------

constructor TDirCollection.InitLoad (var F: TQBufStream; ParentCol: PDirCollection);

  var
    ALimit : SmallInt;
    S      : ShortString;
    i      : SmallInt;
    TmpWord: word;

  begin
  QDirName      := nil;
  QDirInfo      := nil;
  ParentPos     := 0;

  F.Read(ALimit, SizeOf(ALimit));
  F.Read(i, SizeOf(i));
  if ALimit <> not i then
    begin
    raise EQDirFatalException.Create(lsInconsistencyOfDataInDBase+' (1)');
    TQCollection.Init(10, 10);
    exit;
    end;

  TQCollection.Init(ALimit, 100);

  F.Read (S[0], 1);
  if S[0] > #0 then F.Read (S[1], byte(S[0]));
  if S[length(S)] <> '\' then S := S  + '\';
  i := Pos(CharForArcDir, S);
  while i > 0 do
    begin
    S[i] := '.';
    i := Pos(CharForArcDir, S);
    end;
  S := AnsiUpperCase(ConvStr(S));
  QDirName := NewStr(S);

  F.Read (TmpWord, SizeOf(TmpWord));
  QDirInfo := nil;
  if TmpWord > 0 then
    begin
    GetMem(QDirInfo, TmpWord+2);
    QDirInfo^.Len := TmpWord;
    F.Read (QDirInfo^.Dta, QDirInfo^.Len);
    for i := 1 to QDirInfo^.Len do
      QDirInfo^.Dta[i] := ConvArr[QDirInfo^.Dta[i]];
    end;

  F.Read (ParentPos, SizeOf(ParentPos));

  for i := 1 to ALimit do
    begin
    Insert(New(POneFile16, InitLoad(F, ParentCol, @Self)));
    end;
  end;

//----------------------------------------------------------------------------

function  TDirCollection.FindDirCol (var Dir: ShortString; var Col: PDirCollection): boolean;

  var
    i: Integer;

  begin
  FindDirCol := false;
  if Col <> nil then exit;
  if Dir = QDirName^
    then
      begin
      Col := @Self;
      FindDirCol := true;
      end
    else
      begin
      FindDirCol := true;
      for i := 0 to pred(Count) do
        if POneFile16(At(i))^.FindDirOne(Dir, Col) then exit;
      FindDirCol := false;
      end;
  end;

//=== TOneFile16 ==============================================================

constructor TOneFile16.Init (SR: TQDir4SearchRec);

  var
    Dir: DirStr;

  begin
  TQObject.Init;

  FileType := TFileType(0);
  Attr     := 0;
  Time     := 0;
  Size     := 0;
  Name     := '';
  Ext      := '';
  DirCol := nil;

  Attr := SR.Attr;
  Time := SR.Time;
  Size := SR.Size;
  if Attr and faDirectory <> 0
    then
      begin
      if SR.Name = '..'
        then
          begin
          Name := SR.Name;
          Ext := '';
          FileType := ftParent;
          end
        else
          begin
          FSplit (SR.Name, Dir, Name, Ext);
          FileType := ftDir;
          end
      end
    else
      begin
      FSplit (SR.Name, Dir, Name, Ext);
      FileType := ftFile;
      end;
  end;

//----------------------------------------------------------------------------

destructor TOneFile16.Done;

  begin
  if ((FileType = ftDir) or
     ((FileType > ftArchiveBegin) and (FileType < ftArchiveEnd)))
        and (DirCol <> nil) then Dispose(DirCol, Done);
  TQObject.Done;
  end;

//----------------------------------------------------------------------------

constructor TOneFile16.InitLoad (var F: TQBufStream; ParentCol, OwnerCol: PDirCollection);

  var
    TmpByte: byte;

  begin
  TQObject.Init;
  FileType := TFileType(0);
  Attr     := 0;
  Time     := 0;
  Size     := 0;
  Name     := '';
  Ext      := '';
  DirCol := nil;

  F.Read (TmpByte, SizeOf(TmpByte));
  F.Read (FileType, cOneFileSize);
  if Name[0] > #0 then F.Read (Name[1], byte(Name[0]));
  F.Read (Ext[0], 1);
  if Ext[0] > #0 then F.Read (Ext[1], byte(Ext[0]));
  if (FileType = ftDir) or
     ((FileType > ftArchiveBegin) and (FileType < ftArchiveEnd))
        then DirCol := New(PDirCollection, InitLoad(F, OwnerCol));
  if FileType = ftParent then DirCol := ParentCol;
  end;

//----------------------------------------------------------------------------

function  TOneFile16.FindDirOne (var Dir: ShortString; var Col: PDirCollection): boolean;

  begin
  FindDirOne := false;
  if Col <> nil then exit;
  if ((FileType = ftDir) or
     ((FileType > ftArchiveBegin) and (FileType < ftArchiveEnd)))
       and (DirCol <> nil) {pro jistotu - nemˆlo by nastat}
         then FindDirOne := DirCol^.FindDirCol(Dir, Col);
  end;

//----------------------------------------------------------------------------

destructor TQDirRecord.Done;

  begin
  if RootCol <> nil then Dispose(RootCol, Done);
  if QDescription  <> nil then
    FreeMem(QDescription, QDescription^.Len+2);
  end;

//----------------------------------------------------------------------------

constructor TQDirRecord.InitLoad (var F: TQBufStream);

  var
    TotalSize : longint;
    NTotalSize: longint;
    TmpWord   : word;
    i         : word;

  begin
  TQObject.Init;

  QDeleted    := false;
  KeyString   := '';
  QDiskSize   := 0;
  QDiskFree   := 0;
  QScanDate   := 0;
  QDescription:= nil;
  CurrentCol  := nil;
  RootCol     := nil;

  F.Read (TotalSize, SizeOf(TotalSize));
  F.Read (NTotalSize, SizeOf(TotalSize));
  if TotalSize <> not NTotalSize then
    begin
    raise EQDirFatalException.Create(lsInconsistencyOfDataInDBase+' (2)');
    exit;
    end;

  F.Read (QDeleted, SizeOf(QDeleted));
  F.Read (KeyString[0], 1);
  if KeyString[0] > #0 then F.Read (KeyString[1], byte(KeyString[0]));

  F.Read (QDiskSize, SizeOf(QDiskSize));
  F.Read (QDiskFree, SizeOf(QDiskFree));
  F.Read (QScanDate, SizeOf(QScanDate));
  F.Read (TmpWord, SizeOf(TmpWord));
  if TmpWord > 0 then
    begin
    GetMem(QDescription, TmpWord+2);
    QDescription^.Len := TmpWord;
    F.Read (QDescription^.Dta, QDescription^.Len);
    for i := 1 to QDescription^.Len do
      QDescription^.Dta[i] := ConvArr[QDescription^.Dta[i]];
    end;
  RootCol      := New(PDirCollection, InitLoad(F, nil));
  CurrentCol   := RootCol;
  end;

//----------------------------------------------------------------------------

function ReadQDir4Entry (Position: longInt): boolean;

  begin
  Result := false;
  if MainFile.GetPos >= MainFile.GetSize then exit;
  Result := true;
  if Position >= 0 then MainFile.Seek(Position);
  if QDir4Record <> nil then Dispose(QDir4Record, Done);
  QDir4Record := New(PQDirRecord, InitLoad(MainFile));
  if MainFile.Status <> stOk then
    begin
    QDir4Record := nil;
    Result := false;
    exit;
    end;
  end;

//----------------------------------------------------------------------------

procedure OpenQDir4Database(Name: ShortString);

  var TmpSt: array[0..256] of char;

  begin
  MainFile.Init(StrPCopy(TmpSt, Name), stOpenReadNonExclusive);
  end;

//----------------------------------------------------------------------------

procedure CloseQDir4Database;

  begin
  MainFile.Done;
  end;

//----------------------------------------------------------------------------

function FindQDir4First(Path: ShortString; var F: TQDir4SearchRec;
                        var ItIsArchive: boolean): Integer;

  var
    Dir : DirStr;
    Name: NameStr;
    Ext : ExtStr;
    Col : PDirCollection;
    OneFilePtr: POneFile16;

  begin
  Result := 0;
  ShortDelete(Path, 1, 2); // Path is in Windows encoding
  Path := AnsiUpperCase(Path);
  FSplit(Path, Dir, Name, Ext);
  Col := nil;
  with QDir4Record^ do
    begin
    if not RootCol^.FindDirCol (Dir, Col) then
      begin
      Result := -1;
      exit;
      end;
    CurrentCol := Col;
    F.FindIterator := 0;
    if CurrentCol^.Count > 0
      then
        begin
        OneFilePtr := POneFile16(CurrentCol^.At(F.FindIterator));
        Ext := OneFilePtr^.Ext;
        ItIsArchive := false;
        if (OneFilePtr^.FileType <> ftFile) and
           (length(Ext) > 0) and
           (Ext[1] = CharForArcDir) then
          begin
          Ext[1] := '.';
          ItIsArchive := true;
          end;
        F.Name := ConvStr(OneFilePtr^.Name + Ext);
        F.Attr := OneFilePtr^.Attr;
        if ItIsArchive then F.Attr := F.Attr or faDirectory;
        F.Time := OneFilePtr^.Time;
        F.Size := OneFilePtr^.Size;
        end
      else
        Result := -1;
    end;
  end;

//----------------------------------------------------------------------------

function FindQDir4Next(var F: TQDir4SearchRec; var ItIsArchive: boolean): Integer;

  var
    OneFilePtr: POneFile16;
    Ext: ExtStr;

  begin
  inc(F.FindIterator);
  if F.FindIterator < QDir4Record^.CurrentCol^.Count
    then
      begin
      OneFilePtr := POneFile16(QDir4Record^.CurrentCol^.At(F.FindIterator));
      Ext := OneFilePtr^.Ext;
      ItIsArchive := false;
      if (OneFilePtr^.FileType <> ftFile) and
         (length(Ext) > 0) and
         (Ext[1] = CharForArcDir) then
        begin
        Ext[1] := '.';
        ItIsArchive := true;
        end;
      F.Name := ConvStr(OneFilePtr^.Name + Ext);
      F.Attr := OneFilePtr^.Attr;
      if ItIsArchive then F.Attr := F.Attr or faDirectory;
      F.Time := OneFilePtr^.Time;
      F.Size := OneFilePtr^.Size;
      Result := 0;
      end
    else
      Result := -1;
  end;


//----------------------------------------------------------------------------

function  GetQDir4DirDesc (Path: ShortString): pointer;

  var
    Col : PDirCollection;

  begin
  Result := nil;
  ShortDelete(Path, 1, 2);
  Path := AnsiUpperCase(Path);
  if ShortCopy(Path, length(Path), 1) <> '\' then Path := Path + '\';
  Col := nil;
  with QDir4Record^ do
    begin
    if not RootCol^.FindDirCol (Path, Col) then exit;
    Result := Col^.QDirInfo;
    end;
  end;

//----------------------------------------------------------------------------

function  GetQDir4VolumeLabel: ShortString;

  begin
  Result := ConvStr(QDir4Record^.KeyString);
  end;

//----------------------------------------------------------------------------

procedure GetQDir4RecordAttr (var aDiskSize, aDiskFree: longword; var aScanDate: longint);

  begin
  aDiskSize   := QDir4Record^.QDiskSize div 1024;
  aDiskFree   := QDir4Record^.QDiskFree div 1024;
  aScanDate   := QDir4Record^.QScanDate;
  end;

//----------------------------------------------------------------------------

procedure SetQDir4CharConversion (Kamen: boolean);

  begin
  if Kamen
    then ConvArr := KamToWin
    else ConvArr := LatToWin;
  end;

//----------------------------------------------------------------------------

function GetQDir4DiskDesc: pointer;

  begin
  Result := QDir4Record^.QDescription;
  end;

//----------------------------------------------------------------------------

begin
QDir4Record   := nil;
SetQDir4CharConversion (false);
end.
