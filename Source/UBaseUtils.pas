unit UBaseUtils;
(*====================================================================
Useful functions of general usage
======================================================================*)

interface

uses UTypes, UCollections, UApiTypes, UBaseTypes, UCollectionsExt;

  const
    // for remove spaces
    rsLeft    = 1;
    rsRight   = 2;
    rsAll     = 4;

    ByteRandSeed: word = $FF;

  type
    TDisks = array['A'..'Z'] of boolean;
    TExtStr = string[4];

    TQDateTime = record
      Year, Month, Day, Hour, Min, Sec: Word;
      end;

  var
    WinDateFormat, SaveWinDateFormat: (wd_mdy, wd_ymd, wd_dmy, wd_WinShort);
    kBDivider: Integer;

  function  ReplaceExt      (FileName: ShortString; NewExt: TExtStr;
                             Force: Boolean): ShortString;
  function  FileExists      (FName: ShortString): boolean;
  function  DirExists       (FName: ShortString): boolean;
  function  TestFileCreation (sFileName: AnsiString): boolean;
  {$ifdef mswindows}
  procedure QGetDiskSizes   (DriveNum: Byte; var lSizeKbytes, lFreeKbytes,
                             lBytesPerSector, lSectorsPerCluster: longword);
  {$else}
  procedure QGetDiskSizes   (DriveNum: ShortString; var lSizeKbytes, lFreeKbytes,
                             lBytesPerSector, lSectorsPerCluster: longword);
  {$endif}
  procedure FSplit          (Path: ShortString; var Dir, Name, Ext: ShortString);
  function  ExtractDir      (Path: ShortString): ShortString;
  function  GetProgramDir: ShortString;
  function  ChrUpCase       (Ch: char)   : char;
  function  ChrLoCase       (Ch: char)   : char;

  function  ConvertCh       (Line: ShortString; Old, New: char): ShortString;
  function  DeleteAllSpaces (St: ShortString) : ShortString;
  function  TrimSpaces      (St: ShortString) : ShortString;
  function  RemoveRedundSpaces (St: ShortString): ShortString;
  function  RemoveSpaces    (St: ShortString; Which: byte): ShortString;
  function  LimitCharsInPath(St: ShortString; Number: integer): ShortString;
  function  BlockCompare    (var Buf1, Buf2; BufSize : Word): Boolean;
  function  BlockCompare2   (var Buf1, Buf2; BufSize : Word): Boolean;

  function  HexStr          (Number:longint; Len: byte) : ShortString;
  function  OctStr          (Number:longint; Len: byte) : ShortString;
  function  NumStr          (Number:longint; Format: byte): ShortString;
  function  LeadZeroStr     (Number:longint; Len: byte) : ShortString;
  function  RemoveTabs      (S: ShortString): ShortString;
  function  InsertTabs      (S: ShortString): ShortString;
  function  ByteRandom:      byte;
  function  MinI            (X, Y: Integer): Integer;
  function  MaxI            (X, Y: Integer): Integer;
  function  MinW            (X, Y: Word): Word;
  function  MaxW            (X, Y: Word): Word;
  function  MinLI           (X, Y: Longint): Longint;
  function  MaxLI           (X, Y: Longint): Longint;
  function  LPos            (var Block; Size: Word; Str: ShortString): Word;
  function  RPos            (var Block; Size: Word; Str: ShortString): Word;
  function  GetPQString     (P: TPQString): ShortString;

  function  DosTimeToStr (DosDateTime: longint; IncludeSeconds: boolean): ShortString;
  function  DosDateToStr (DosDateTime: longint): ShortString;
  function  FormatSize   (Size: longint; ShortFormat: boolean): ShortString;
  function  FormatBigSize (Size: comp): ShortString;
  function  FormatNumber (Number: longint): ShortString;
  function  GetNewTag: Integer;

  procedure TokenFilterMask (var AllMasksArray: TAllMasksArray; var MaskCol, DllCol: TPQCollection;
                             CaseSensitive: boolean);
  procedure TokenFindMask   (Mask: ShortString; var MaskCol: TPQCollection;
                             CaseSensitive: boolean; AddWildCards: boolean;
                             AsPhrase: boolean);
  function  MaskCompare     (OneMask, S: ShortString; CaseSensitive: boolean;
                             Strict: boolean): boolean;
  function  MaskCompareBuf  (OneMask: ShortString; Buf: PChar; CaseSensitive: boolean;
                             AnsiCompare: boolean; OemCompare: boolean): longint;

  // UnpackTime converts a 4-byte packed date/time returned by
  // FindFirst, FindNext or GetFTime into a TQDateTime record.
  procedure UnpackTime(P: Longint; var T: TQDateTime);

  // PackTime converts a TQDateTime record into a 4-byte packed
  // date/time used by SetFTime.
  procedure PackTime(var T: TQDateTime; var P: Longint);

  procedure AddToBuffer(CalcOnly: boolean;
                        S: ShortString;
                        var BufferPos: PChar;
                        var TotalLength: longint);



//-----------------------------------------------------------------------------

implementation


uses SysUtils,
  {$ifdef mswindows}
  WinProcs
  {$ELSE}
    LCLIntf, LCLType, LMessages
  {$ENDIF}
   ;

type
  TGetDiskFreeSpaceEx = function  (lpDirectoryName: PAnsiChar;
                                   var FreeBytesAvailableToCaller: Comp;
                                   var TotalNumberOfBytes: Comp;
                                   var TotalNumberOfFreeBytes: Comp): longint; stdcall;
var
  GetDiskFreeSpaceEx: TGetDiskFreeSpaceEx;


//-----------------------------------------------------------------------------
//  Replace the extension of the given file with the given extension.
//  If the an extension already exists Force indicates if it should be
//  replaced anyway.


function ReplaceExt (FileName: ShortString; NewExt: TExtStr; Force: Boolean): ShortString;

  var
    Dir : ShortString;
    Name: ShortString;
    Ext : ShortString;

  begin
  FSplit(FileName, Dir, Name, Ext);
  if Force or (Ext = '')
    then ReplaceExt := Dir + Name + NewExt
    else ReplaceExt := FileName;
  end;

//-----------------------------------------------------------------------------

function FileExists(FName: ShortString): boolean;

   var
     DirInfo: TSearchRec;

   begin
   Result := SysUtils.FindFirst(FName, faReadOnly or faSysfile or faArchive
                           or faHidden, DirInfo) = 0;
   if (Result) then Result := (DirInfo.Attr and faDirectory) = 0;
   SysUtils.FindClose(DirInfo);
   end;

//-----------------------------------------------------------------------------

function DirExists(FName: ShortString): boolean;

   var
     DirInfo: TSearchRec;

   begin
   {$ifdef mswindows}
   if (length(FName) = 3) and (FName[2] = ':') and (FName[3] = '\')
     then
       begin
       Result := true; // it is root
       exit;
       end;
   if FName[length(FName)] = '\' then dec(FName[0]);
   Result := SysUtils.FindFirst(FName, faDirectory, DirInfo) = 0;
   if (Result) then Result := (DirInfo.Attr and faDirectory) <> 0;
   SysUtils.FindClose(DirInfo);
   {$else}
   Result:=DirectoryExists(FNAme);
   {$endif}
   end;

//-----------------------------------------------------------------------------

function TestFileCreation (sFileName: AnsiString): boolean;

  var
    Handle: longword;

  begin
  {$ifdef mswindows}
  Handle := CreateFile(PChar(sFileName), GENERIC_READ or GENERIC_WRITE,
                       0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
    begin
    Result := false;
    exit;
    end;
  CloseHandle(Handle);
  DeleteFile(PChar(sFileName));
  {$endif}
  Result := true;
  end;

//-----------------------------------------------------------------------------

function IsWin95Osr2OrLater: boolean;
  {$ifdef mswindows}
  var
    OsVersionInfo: TOSVersionInfo;

  begin
  OsVersionInfo.dwOSVersionInfoSize := sizeof(OsVersionInfo);
  GetVersionEx(OsVersionInfo);
  if (OsVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and
     ((OsVersionInfo.dwBuildNumber and $0000FFFF) > 1000)
       then Result := true
       else Result := false;
  {$else}
  begin
    result:=true ;
  {$endif}
  end;

//-----------------------------------------------------------------------------

{$ifdef mswindows}
procedure QGetDiskSizes (DriveNum: Byte; var lSizeKbytes, lFreeKbytes,
                         lBytesPerSector, lSectorsPerCluster: longword);

  var
    DriveSpec: array[0..3] of char;
    SectorsPerCluster     : longword;
    BytesPerSector        : longword;
    NumberOfFreeClusters  : longword;
    TotalNumberOfClusters : longword;
    RSectorsPerCluster    : comp;
    RBytesPerSector       : comp;
    RNumberOfFreeClusters : comp;
    RTotalNumberOfClusters: comp;
    RFreeKb               : comp;
    RSizeKb               : comp;

    FreeBytesAvailableToCaller  : Comp;
    TotalNumberOfBytes          : Comp;
    TotalNumberOfFreeBytes      : Comp;

  begin
  DriveSpec[0] := Char(DriveNum + Ord('A')-1);
  DriveSpec[1] := ':';
  DriveSpec[2] := '\';
  DriveSpec[3] := #0;

  if Assigned(GetDiskFreeSpaceEx)
    then
      begin
      if GetDiskFreeSpaceEx(DriveSpec,
                            FreeBytesAvailableToCaller,
                            TotalNumberOfBytes,
                            TotalNumberOfFreeBytes) <> 0

        then
          begin
          RSizeKb := TotalNumberOfBytes / 1024;
          RFreeKb := TotalNumberOfFreeBytes / 1024;
          lSizeKbytes := round(RSizeKb);
          lFreeKbytes := round(RFreeKb);
          lBytesPerSector    := 0;
          lSectorsPerCluster := 0;
         end
        else
          begin
          lSizeKbytes := 0;
          lFreeKbytes := 0;
          lBytesPerSector := 0;
          lSectorsPerCluster := 0;
          end;
      end
    else
      begin
      if GetDiskFreeSpace(DriveSpec,
          SectorsPerCluster, BytesPerSector,
          NumberOfFreeClusters, TotalNumberOfClusters)
        then
          begin
          RSectorsPerCluster     := SectorsPerCluster;
          RBytesPerSector        := BytesPerSector;
          RNumberOfFreeClusters  := NumberOfFreeClusters;
          RTotalNumberOfClusters := TotalNumberOfClusters;
          RSizeKb := (RSectorsPerCluster*RBytesPerSector*RTotalNumberOfClusters) / 1024;
          RFreeKb := (RSectorsPerCluster*RBytesPerSector*RNumberOfFreeClusters) / 1024;
          lSizeKbytes := round(RSizeKb);
          lFreeKbytes := round(RFreeKb);
          lBytesPerSector := BytesPerSector;
          lSectorsPerCluster := SectorsPerCluster;
          end
        else
          begin
          lSizeKbytes := 0;
          lFreeKbytes := 0;
          lBytesPerSector := 0;
          lSectorsPerCluster := 0;
          end;
      end;
  end;

    {$else}
procedure QGetDiskSizes (DriveNum: ShortString; var lSizeKbytes, lFreeKbytes,
                         lBytesPerSector, lSectorsPerCluster: longword);
var
  i: byte;
    ///lSizeKbytes := 0;
    ///lFreeKbytes := 0;
 begin
    i:=AddDisk(DriveNum);
    lBytesPerSector := 0;
    lSectorsPerCluster := 0;
    lSizeKbytes := round(DiskSize(i) / 1024);
    lFreeKbytes := round(DiskFree(i) / 1024);
    {$endif}
  end;

//-----------------------------------------------------------------------------
// Splits path to Dir, Name and Ext

procedure FSplit (Path: ShortString; var Dir, Name, Ext: ShortString);

  var i, DotPos, SlashPos: Integer;

  begin
  Dir  := '';
  Name := '';
  Ext  := '';
  {$ifdef mswindows}
  i := pos(':', Path);
  if i > 0 then
    begin
    Dir := ShortCopy(Path, 1, i);
    ShortDelete (Path, 1, i);
    end;
  DotPos := 0;
  for i:=length(Path) downto 1 do
    if Path[i] = '.' then
      begin
      if i >= length(Path) - 3 then DotPos := i;
      break;
      end;
  SlashPos := 0;
  for i:=length(Path) downto 1 do
    if Path[i] = '\' then
      begin
      SlashPos := i;
      break;
      end;
  if DotPos > SlashPos then
    begin
    Ext := ShortCopy(Path, DotPos, length(Path)-DotPos+1);
    ShortDelete(Path, DotPos, length(Path)-DotPos+1);
    end;
  if SlashPos > 0
    then
      begin
      Name := ShortCopy(Path, SlashPos+1, length(Path)-SlashPos);
      ShortDelete(Path, SlashPos+1, length(Path)-SlashPos);
      Dir := Dir + Path;
      end
    else
      Name := Path;
    {$else}
    dir  := ExtractFileDir(path);
    Name := ExtractFileName(path);
    ext  := ExtractFileExt(path);
    Name := copy(Name,1,length(Name)-length(Ext));
    {$endif}
  end;

//-----------------------------------------------------------------------------
// Extracts dir from path

function ExtractDir (Path: ShortString): ShortString;

  var i, SlashPos: Integer;

  begin
  Result  := '';
  SlashPos := 0;
  for i:=length(Path) downto 1 do
    if Path[i] = '\' then
      begin
      SlashPos := i;
      break;
      end;
  if SlashPos > 1 then
    Result := ShortCopy(Path, 1, SlashPos-1);
  end;

//-----------------------------------------------------------------------------

function GetProgramDir: ShortString;

  var
    Dir : ShortString;
    Name: ShortString;
    Ext : ShortString;

  begin
  FSplit(ParamStr(0),Dir,Name,Ext);
  GetProgramDir := Dir;
  end;

//-----------------------------------------------------------------------------
// Creates directory including all subfolders

function MakeDirectory (Dir: ShortString): boolean;

  var
    TmpDir, SaveDir : ShortString;
    i               : byte;

  begin
  MakeDirectory := false;
  GetDir (0,SaveDir);

  i := pos('\',Dir);
  if i = 0 then
    begin
    ChDir(SaveDir);
    exit;
    end;
  TmpDir := ShortCopy(Dir,1,i);
  ShortDelete(Dir,1,i);
  ChDir(TmpDir);
  if IOResult <> 0 then
    begin
    ChDir(SaveDir);
    exit;
    end;

  while Dir <> '' do
    begin
    i := pos('\',Dir);
    if i=0
      then
        begin
        TmpDir := Dir;
        Dir := '';
        end
      else
        begin
        TmpDir := ShortCopy(Dir,1,pred(i));
        ShortDelete(Dir,1,i);
        end;
    ChDir(TmpDir);
    if IOResult <> 0 then
      begin
      MkDir(TmpDir);
      ChDir(TmpDir);
      end;
    if IOResult <> 0 then
      begin
      ChDir(SaveDir);
      exit;
      end;
    end;
  ChDir(SaveDir);
  MakeDirectory := true;
  end;

//-----------------------------------------------------------------------------

function ChrUpCase (Ch: char): char;

  begin
  ChrUpCase := UpCase(Ch);
  end;

//-----------------------------------------------------------------------------

function ChrLoCase (Ch: char): char;

  begin
  if (Ch >= 'a') and (Ch <= 'z')
    then ChrLoCase := char(byte(Ch)+32)
    else ChrLoCase := Ch;
  end;

//-----------------------------------------------------------------------------
// Replaces all specified chars in the string

function ConvertCh (Line: ShortString; Old, New: char): ShortString;

 var
   i : byte;

  begin
  for i := 1 to length(Line) do
    if Line[i] = Old then Line[i] := New;
  ConvertCh := Line;
  end;

//-----------------------------------------------------------------------------
// Delets all spaces in the string


function DeleteAllSpaces (St: ShortString): ShortString;

  var
    i : byte;

  begin
  repeat
    i := pos(' ',St);
    if i > 0 then ShortDelete(St,i,1);
  until i = 0;
  DeleteAllSpaces := St;
  end;

//-----------------------------------------------------------------------------

function TrimSpaces (St: ShortString): ShortString;

  begin
  while (length(St) > 0) and (St[1] = ' ') do ShortDelete(St,1,1);
  while (length(St) > 0) and (St[length(St)] = ' ') do ShortDelete(St,length(St),1);
  TrimSpaces := St;
  end;

//-----------------------------------------------------------------------------
// Trims spaces at the beginning and at the end, and removes redundant spaces

function RemoveRedundSpaces (St: ShortString): ShortString;

  var
    i: byte;

  begin
  St := TrimSpaces(St);
  i := pos('  ',St);
  while i > 0 do
    begin
    ShortDelete(St,i,1);
    i := pos('  ',St);
    end;
  RemoveRedundSpaces := St;
  end;

//-----------------------------------------------------------------------------
// Removes all spaces

function RemoveSpaces (St: ShortString; Which: byte): ShortString;

  var
    i  : byte;

  begin
  if Which and rsLeft <> 0 then
    while ShortCopy(St,1,1) = ' ' do ShortDelete(St,1,1);
  if Which and rsRight <> 0 then
    while ShortCopy(St,length(St),1) = ' ' do ShortDelete(St,length(St),1);
  if Which and rsAll <> 0 then
    repeat
      i := pos(' ', St);
      if i > 0 then ShortDelete(St,i,1);
    until i = 0;
  RemoveSpaces := St;
  end;

//-----------------------------------------------------------------------------
// Ff the path is too long, replaces the middle part with "..."

function LimitCharsInPath(St: ShortString; Number: integer): ShortString;

  var
    iFirstBackSlashPos: integer;
    iSecondBackSlashPos: integer;
    iLastSecondBackSlashPos: integer;
    DirChar: char;

  begin
  Result := St;
  {$ifdef mswindows}
  DirChar:='\';
  {$else}
  DirChar:='/';
  {$endif}
  if (length(St) <= Number) then exit;
  iLastSecondBackSlashPos := 0;
  repeat
    iFirstBackSlashPos := pos(DirChar, St);
    if (iFirstBackSlashPos = 0) then exit; // this should not happen
    St[iFirstBackSlashPos] := '|';
    if (iLastSecondBackSlashPos > 0) then St[iLastSecondBackSlashPos] := '|';
    iSecondBackSlashPos := pos(DirChar, St);
    if (iLastSecondBackSlashPos > 0) then St[iLastSecondBackSlashPos] := DirChar;
    St[iFirstBackSlashPos] := DirChar;
    if iSecondBackSlashPos > 0 then
      begin
      delete(St, iFirstBackSlashPos+1, iSecondBackSlashPos-iFirstBackSlashPos-1);
      insert('...', St, iFirstBackSlashPos+1);
      iLastSecondBackSlashPos := iFirstBackSlashPos + 4;
      end;
  until ((length(St)<=Number) or (iSecondBackSlashPos=0));
  Result := St;
  end;

//-----------------------------------------------------------------------------
// Compares 2 blocks of memory

function BlockCompare (var Buf1, Buf2; BufSize : Word): Boolean;

  var
    ABuf1: array[1..64*1024-1] of byte absolute Buf1;
    ABuf2: array[1..64*1024-1] of byte absolute Buf2;
    i : Integer;

  begin
  Result := false;
  for i := 1 to BufSize do
    if ABuf1[i] <> ABuf2[i] then exit;
  Result := true;
  end;

//-----------------------------------------------------------------------------
// Compares 2 blocks of memory starting with 2nd byte

function BlockCompare2 (var Buf1, Buf2; BufSize : Word): Boolean;

  var
    ABuf1: array[1..64*1024-1] of byte absolute Buf1;
    ABuf2: array[1..64*1024-1] of byte absolute Buf2;
    i : Integer;

  begin
  Result := false;
  for i := 2 to BufSize do
    if ABuf1[i] <> ABuf2[i] then exit;
  Result := true;
  end;


//=============================================================================
// Converts number to hex string.

function HexStr (Number:longint; Len: byte) : ShortString;

 var
  S : ShortString;
  i : byte;
  DigitNum : byte;

  begin
  S := '';
  for i := 1 to 8 do
    begin
    DigitNum := Number and $0000000F;
    Number := Number shr 4;
    if DigitNum > 9 then DigitNum := DigitNum + 7;
    S := chr(DigitNum + ord('0')) + S;
    end;
  while (length(S) < Len) do S := '0' + S;
  while (length(S) > Len) and (S[1] = '0') do ShortDelete(S,1,1);
  HexStr := S;
  end;

//-----------------------------------------------------------------------------
// Converts number to octal string.

function OctStr (Number:longint; Len: byte) : ShortString;

 var
  S : ShortString;
  i : byte;
  DigitNum : byte;

  begin
  S := '';
  for i := 1 to 11 do
    begin
    DigitNum := Number and $00000007;
    Number := Number shr 3;
    S := chr(DigitNum + ord('0')) + S;
    end;
  while (length(S) < Len) do S := '0' + S;
  while (length(S) > Len) and (S[1] = '0') do ShortDelete(S,1,1);
  OctStr := S;
  end;

//-----------------------------------------------------------------------------
// Converts number to decimal string.

function NumStr (Number: longint; Format: byte): ShortString;

  var
    S : ShortString;

  begin
  Str(Number:Format, S);
  NumStr := S;
  end;

//-----------------------------------------------------------------------------
// Adds leading zeros to specified length

function LeadZeroStr(Number: longint; Len: byte): ShortString;

 var
  St: ShortString;

  begin
  Str(Number:1,St);
  while length(St) < Len do St := '0' + St;
  LeadZeroStr := St;
  end;

//-----------------------------------------------------------------------------

function RemoveTabs (S: ShortString): ShortString;

  var
   i,j : Integer;

  begin
  i := pos(^I, S);
  while i > 0 do
    begin
    ShortDelete(S,i,1);
    for j:= 1 to 8 do ShortInsert(' ',S,i);
    i := i + 8;
    while pred(i) mod 8 > 0 do
      begin
      ShortDelete(S,pred(i),1);
      dec(i);
      end;
    i := pos(^I,S);
    end;
  RemoveTabs := S;
  end;

//-----------------------------------------------------------------------------
// Replaces every 8 spaces by tab

 function InsertTabs (S: ShortString): ShortString;
{===================
}
 var
   i,j       : Integer;
   Octal    : ShortString;
   OutS      : ShortString;

  begin
  OutS := '';
  for i := 0 to length(S) div 8 do
    begin
    Octal := ShortCopy(S,i*8+1,8);
    if length(Octal) = 8 then
      if ShortCopy(Octal,7,2) = '  ' then
        begin
        j:=8;
        while (j > 0) and (Octal[j]=' ') do dec(j);
        ShortDelete(Octal,succ(j),8-j);
        Octal := Octal + ^I;
        end;
    OutS := OutS + Octal;
    end;
  InsertTabs := OutS;
  end;

//-----------------------------------------------------------------------------

function MinI (X, Y: Integer): Integer;

  begin
  if X > Y then Result := Y else Result := X;
  end;

//-----------------------------------------------------------------------------

function MaxI (X, Y: Integer): Integer;

  begin
  if X < Y then Result := Y else Result := X;
  end;

//-----------------------------------------------------------------------------

function MinW (X, Y: Word): Word;

  begin
  if X > Y then Result := Y else Result := X;
  end;


//-----------------------------------------------------------------------------

function MaxW (X, Y: Word): Word;

  begin
  if X < Y then Result := Y else Result := X;
  end;

//-----------------------------------------------------------------------------

function MinLI (X, Y: longint): longint;

  begin
  if X > Y then Result := Y else Result := X;
  end;


//-----------------------------------------------------------------------------

 function MaxLI (X, Y: longint): longint;

  begin
  if X < Y then Result := Y else Result := X;
  end;

//-----------------------------------------------------------------------------
// Finds a string in a binary block of data, first position is 1, 0 means not found

function LPos (var Block; Size: Word; Str: ShortString): Word;

  var
    Buf   : array[1..$FFFF] of char absolute Block;
    i,j   : word;
    Found : boolean;

  begin
  LPos := 0;
  if Str[0] = #0 then exit;
  for i := 1 to Size-length(Str)+1 do
    begin
    if Buf[i] = Str[1] then
      begin
      Found := (i - 1 + length(Str)) <= Size;
      j := 2;
      while Found and (j <= length(Str)) do
        begin
        if Buf[i+j-1] <> Str[j] then Found := false;
        inc(j);
        end;
      if Found then
        begin
        LPos := i;
        exit;
        end;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// Finds a string in a binary block of data from right, first
// position is 1, 0 means not found

function RPos (var Block; Size: Word; Str: ShortString): Word;

  var
    Buf   : array[1..$FFFF] of char absolute Block;
    i,j   : longint;
    Found : boolean;

  begin
  RPos := 0;
  if Str[0] = #0 then exit;
  if Size < length(Str) then exit;
  for i := Size-length(Str)+1 downto 1 do
    begin
    if Buf[i] = Str[1] then
      begin
      Found := (i - 1 + length(Str)) <= Size;
      j := 2;
      while Found and (j <= length(Str)) do
        begin
        if Buf[i+j-1] <> Str[j] then Found := false;
        inc(j);
        end;
      if Found then
        begin
        RPos := i;
        exit;
        end;
      end;
    end;
  end;

//-----------------------------------------------------------------------------
// deletes Ctrl-C form the string and adjusts single CRs to CRLFs

function ConvertCtrlCAndCRLF (S: ShortString): ShortString;

  var
    i: byte;

   begin
   i := pos(^C, S);
   while i > 0 do
     begin
     ShortDelete(S, i, 1);
     i := pos(^C, S);
     end;
   i := 1;
   while i <= length(S) do
     begin
     if S[i] = #13 then
       ShortInsert(#10, S, succ(i));
     inc(i);
     end;
   ConvertCtrlCAndCRLF := S;
   end;

//-----------------------------------------------------------------------------
// Converts dynammically allocated string to normal string, avoids crash if the
// string is nil

function GetPQString (P: TPQString): ShortString;

  begin
  if P = nil then GetPQString := '' else GetPQString := P^;
  end;

//-----------------------------------------------------------------------------

function DosTimeToStr (DosDateTime: longint; IncludeSeconds: boolean): ShortString;
  var
    Hour, Min, Sec: word;
  begin
  Sec         := (DosDateTime and $1F) shl 1;
  DosDateTime := DosDateTime shr 5;
  Min         := DosDateTime and $3F;
  DosDateTime := DosDateTime shr 6;
  Hour        := DosDateTime and $1F;
  if IncludeSeconds
    then DosTimeToStr := Format('%d'+TimeSeparator+'%2.2d'+TimeSeparator+'%2.2d', [Hour, Min, Sec])
    else DosTimeToStr := Format('%d'+TimeSeparator+'%2.2d', [Hour, Min]);

  end;

//-----------------------------------------------------------------------------

function DosDateToStr (DosDateTime: longint): ShortString;
  var
    Year, Year2Digits, Month, Day: word;

  begin
  DateSeparator:='.';
  DosDateTime := DosDateTime shr 16;
  Day         := DosDateTime and $1F;
  DosDateTime := DosDateTime shr 5;
  Month       := DosDateTime and $0F;
  DosDateTime := DosDateTime shr 4;
  Year        := DosDateTime + 1980;
  Year2Digits := Year mod 100;
  case WinDateFormat of
    wd_mdy: DosDateToStr := Format('%d'+DateSeparator+'%d'+DateSeparator+'%2.2d', [Month, Day, Year2Digits]);
    wd_ymd: DosDateToStr := Format('%2.2d'+DateSeparator+'%d'+DateSeparator+'%d', [Year2Digits, Month, Day]);
    ///wd_dmy: DosDateToStr := Format('%d'+DateSeparator+'%d'+DateSeparator+'%2.2d', [Day, Month, Year2Digits]);
    wd_dmy: DosDateToStr := Format('%d'+DateSeparator+'%d'+DateSeparator+'%d', [Day, Month, Year]);
    else {WinShort}
      try
        DosDateToStr := DateToStr(EncodeDate(Year, Month, Day));
      except
        On EConvertError do
          DosDateToStr := Format('%d'+DateSeparator+'%d'+DateSeparator+'%2.2d', [Day, Month, Year2Digits]);
        end;
    end;
  end;

//-----------------------------------------------------------------------------
// Formats Size to displayable string (in bytes, kB, MB, ...)

function FormatSize(Size: longint; ShortFormat: boolean): ShortString;

  var
    FSize      : real;
    S          : ShortString;
  begin
  if ShortFormat
    then
      begin
      FSize := Size;
      FSize := FSize / kBDivider;
      if FSize >= kBDivider
        then
          begin
          FSize := FSize / kBDivider;
          if FSize < 10
            then S := Format('%2.2n MB', [FSize])
            else S := Format('%2.1n MB', [FSize]);
          end
        else
          if FSize < 10
            then S := Format('%2.2n kB', [FSize])
            else S := Format('%2.1n kB', [FSize]);
      end
    else
      begin
      S := IntToStr(Size);
      if S[0] >  #3 then ShortInsert(' ', S, length(S) -2);
      if S[0] >  #7 then ShortInsert(' ', S, length(S) -6);
      if S[0] > #11 then ShortInsert(' ', S, length(S)-10);
      end;
  FormatSize := S;
  end;

//-----------------------------------------------------------------------------
// Formats big size to displayable string (in MB or GB)

function FormatBigSize(Size: comp): ShortString;

  var
    FSize      : Double;
    S          : ShortString;

  begin
  FormatSettings.DecimalSeparator:=',';
  if Size > kBDivider
    then
      begin
      FSize := Size;
      FSize := FSize / kBDivider;
      if FSize >= kBDivider
        then
          begin
          FSize := FSize / kBDivider;
          if FSize >= kBDivider
            then
              begin
              FSize := FSize / kBDivider;
              if FSize < 10
                then S := Format('%2.2n GB', [FSize])
                else S := Format('%2.1n GB', [FSize]);
              end
            else
              begin
              if FSize < 10
                then S := Format('%2.2n MB', [FSize])
                else S := Format('%2.1n MB', [FSize]);
              end
          end
        else
          if FSize < 10
            then S := Format('%2.2n kB', [FSize])
            else S := Format('%2.1n kB', [FSize]);
      end
    else
      begin
      if Size = 0
        then S := '0'
        else S := Format('%2.0n B', [Size]);
      end;
  FormatBigSize := S;
  end;

//-----------------------------------------------------------------------------
// inserts spaces at thousands, millions etc.

function FormatNumber(Number: longint): ShortString;

  begin
  Result := IntToStr(Number);
  if Result[0] >  #3 then ShortInsert(' ', Result, length(Result) -2);
  if Result[0] >  #7 then ShortInsert(' ', Result, length(Result) -6);
  if Result[0] > #11 then ShortInsert(' ', Result, length(Result)-10);
  end;

//-----------------------------------------------------------------------------

const
  TagCounter: Integer = 0;

// returns unique number

function GetNewTag: Integer;

  begin
  inc(TagCounter);
  GetNewTag := TagCounter;
  end;

//-----------------------------------------------------------------------------
// Dispatches the filter mask string to a collection of single masks

procedure TokenFilterMask (var AllMasksArray: TAllMasksArray;
                           var MaskCol, DllCol: TPQCollection;
                           CaseSensitive: boolean);

  var
    i, j       : Integer;
    OneMask    : POneMask;
    Mask       : ShortString;
    kBytes     : ShortString;
    ConvDllName: ShortString;
    Dll        : PDll;
    Found      : boolean;

  begin
  MaskCol^.FreeAll;
  if not CaseSensitive then AnsiLowerCase(AllMasksArray);

  i := 0;
  while AllMasksArray[i] <> #0 do
    begin
    Mask := '';
    while (AllMasksArray[i] <> '|') and (AllMasksArray[i] <> #0) do
      begin
      Mask := Mask + AllMasksArray[i];
      inc(i);
      end;

    if (AllMasksArray[i] <> #0) then inc(i);
    kBytes := '';
    while (AllMasksArray[i] <> '|') and (AllMasksArray[i] <> #0) do
      begin
      kBytes := kBytes + AllMasksArray[i];
      inc(i);
      end;

    if (AllMasksArray[i] <> #0) then inc(i);
    ConvDllName := '';
    while (AllMasksArray[i] <> '|') and (AllMasksArray[i] <> #0) do
      begin
      ConvDllName := ConvDllName + AllMasksArray[i];
      inc(i);
      end;

    if (AllMasksArray[i] <> #0) then inc(i);
    OneMask := New(POneMask, Init);
    OneMask^.MaskName := QNewStr(Mask);
    OneMask^.MaxSize  := StrToInt(kBytes);
    if OneMask^.MaxSize < 100 then         {kvuli starym kB}
      OneMask^.MaxSize := OneMask^.MaxSize * 1000;
    if ConvDllName = ''
      then
        OneMask^.ConvDll := nil
      else
        begin
        Found := false;
        for j := 0 to pred(DllCol^.Count) do
          if PDll(DllCol^.At(j))^.DllName = ConvDllName then
            begin
            Found := true;
            OneMask^.ConvDll := PDll(DllCol^.At(j));
            break;
            end;
        if not Found then
          begin
          Dll := New(PDll, Init(ConvDLLName));
          OneMask^.ConvDll := Dll;
          DllCol^.Insert(Dll);
          end;
        end;
    MaskCol^.Insert(OneMask);
    end;
  end;

//----------------------------------------------------------------------------
// Dispatches the find mask string to a collection of single masks
// Syntax: the masks are separated by spaces or semicolons. When the space
// is not a separator, the phrase must be in doublequotes.
// To search a doublequote, write is twice.

procedure TokenFindMask (Mask: ShortString; var MaskCol: TPQCollection;
                         CaseSensitive: boolean; AddWildCards: boolean;
                         AsPhrase: boolean);
  var
    i,j    : byte;
    OneMask: POneMask;
    S      : ShortString;
    IsBetweenQuot: boolean;

  begin
  MaskCol^.FreeAll;

  if AsPhrase then
    begin
    if ShortCopy(Mask, 1, 1) <> '"' then Mask := '"' + Mask;
    if ShortCopy(Mask, length(Mask), 1) <> '"' then Mask := Mask + '"';
    end;

  i := pos(';', Mask);
  while i > 0 do
    begin
    if AsPhrase
      then
        begin
        ShortDelete(Mask, i, 1);
        ShortInsert('" "', Mask, i);
        end
      else
        Mask[i] := ' ';
    i := pos(';', Mask);
    end;

  // " replaced by #1
  i := pos('""', Mask);
  while i > 0 do
    begin
    Mask[i] := #1;
    ShortDelete(Mask, i+1, 1);
    i := pos('""', Mask);
    end;

  // spaces replaced by #2
  IsBetweenQuot := false;
  for i := 1 to length(Mask) do
    begin
    if Mask[i] = '"' then IsBetweenQuot := not IsBetweenQuot;
    if (Mask[i] = ' ') and IsBetweenQuot then Mask[i] := #2;
    end;

  Mask := RemoveRedundSpaces(Mask) + ' ';
  if not CaseSensitive then Mask := AnsiLowerCase(Mask);
  // the space is after EACH mask
  while Mask <> '' do
    begin
    i := pos(' ', Mask);
    S := ShortCopy(Mask,1,pred(i));
    if ShortCopy(S, 1, 1) = '"' then ShortDelete(S, 1, 1);
    if ShortCopy(S, length(S), 1) = '"' then dec(S[0]);
    for j := 1 to length(S) do
      begin
      if S[j] = #1 then S[j] := '"';
      if S[j] = #2 then S[j] := ' ';
      end;
    ShortDelete(Mask,1,i);

    if AddWildCards then
      begin
      if ShortCopy(S, 1, 1) <> '*' then S := '*' + S;
      if ShortCopy(S, length(S), 1) <> '*' then S := S + '*';
      end;

    if (length(S) > 0) then
      begin
      OneMask := New(POneMask, Init);
      OneMask^.MaskName := QNewStr(S);
      MaskCol^.Insert(OneMask);
      end;
    end;
  end;

//-----------------------------------------------------------------------------

function IsLetter(Ch: char): boolean;

  begin
  Result := (Ch >= 'A') and (Ch <= 'Z') or
            (Ch >= 'a') and (Ch <= 'z') or
            (Ch >= #128);
  end;

//-----------------------------------------------------------------------------
// Compares the text with a mask

function MaskCompare(OneMask, S: ShortString; CaseSensitive: boolean;
                     Strict: boolean): boolean;

  function PosQ(SubStr: ShortString; S: ShortString): Byte;
    var
      Offset: Integer;
      Found : boolean;
      i     : Integer;

    begin
    Found := false;
    for Offset := 0 to Integer(length(S)) - length(SubStr) do
      begin
      Found := true;
      for i := 1 to length(SubStr) do
        if (SubStr[i] <> S[Offset + i]) and (SubStr[i] <> '?') then
           begin
           Found := false;
           break;
           end;
      if Found then break;
      end;
    if Found
      then PosQ := succ(Offset)
      else PosQ := 0;
    end;

  var
    AsterixAtBegin, AsterixAtEnd: boolean;
    SubMask: ShortString;
    i: Integer;
    AreQMark: boolean;

  begin
  MaskCompare := true;
  if not CaseSensitive then S := AnsiLowerCase(S);
  if S = OneMask then exit;
  if OneMask = '*' then exit;
  AreQMark := pos('?', OneMask) > 0;  {to speed up}
  MaskCompare := false;
  if Strict and (pos('*', OneMask) = 0) then
    begin
    if not AreQMark then exit;
    MaskCompare := (S[0] = OneMask[0]) and (PosQ(OneMask, S) = 1);
    exit;
    end;
  AsterixAtBegin := false;
  AsterixAtEnd := false;
  if ShortCopy(OneMask, 1, 1) = '*' then
    begin
    AsterixAtBegin := true;
    ShortDelete(OneMask, 1, 1);
    end;
  if OneMask = '' then exit;
  if ShortCopy(OneMask, length(OneMask), 1) = '*' then
    begin
    AsterixAtEnd := true;
    ShortDelete(OneMask, length(OneMask), 1);
    end;
  if OneMask = '' then exit;
  while OneMask <> '' do
    begin
    i := pos('*', OneMask);
    if i > 0 then
      begin
      SubMask := ShortCopy(OneMask, 1, pred(i));
      ShortDelete(OneMask, 1, i);
      end
    else
      begin
      SubMask := OneMask;
      OneMask := '';
      end;
    if Submask = '' then exit;
    if AreQMark and (pos('?', SubMask) > 0)
      then i := PosQ (SubMask, S)
      else i := pos(SubMask, S);
    if i = 0 then exit;
    if not AsterixAtBegin and (i > 1) and (Strict or IsLetter(S[i-1])) then exit;
    AsterixAtBegin := true;
    ShortDelete(S, 1, i + length(SubMask) - 1);
    end;
  if not AsterixAtEnd and (S <> '') and (Strict or IsLetter(S[1])) then exit;
  MaskCompare := true;
  end;

//-----------------------------------------------------------------------------
// Compares block of data with a mask

function MaskCompareBuf(OneMask: ShortString; Buf: PChar; CaseSensitive: boolean;
                        AnsiCompare: boolean; OemCompare: boolean): longint;

  function PosQBuf(SubStr: ShortString; Buf: PChar): longint;
    var
      Offset: longint;
      Found : boolean;
      i     : longint;

    begin
    Found := false;
    for Offset := 0 to longint(StrLen(Buf)) - length(SubStr) do
      begin
      Found := true;
      for i := 1 to length(SubStr) do
        begin
        if (SubStr[i] <> Buf[Offset + i - 1]) and (SubStr[i] <> '?') then
           begin
           Found := false;
           break;
           end;
        end;
      if Found then break;
      end;
    if Found
      then Result := Offset
      else Result := -1;
    end;

  // this function makes lowercase itself - the Buf must not be lowercased,
  // otherwise it will corrupt the OEM conversion
  function PosQBufEx(SubStr: ShortString; Buf: PChar; bOemCompare: boolean): longint;
    var
      Offset: longint;
      Found : boolean;
      i     : longint;
      Ch    : char;

    begin
    Found := false;
    for Offset := 0 to longint(StrLen(Buf)) - length(SubStr) do
      begin
      Found := true;
      for i := 1 to length(SubStr) do
        begin
        Ch := Buf[Offset + i - 1];
        ///if bOemCompare then OemToCharBuff(@Ch, @Ch, 1);
        if not CaseSensitive then CharLowerBuff(@Ch, 1);
        if (SubStr[i] <> Ch) and (SubStr[i] <> '?') then
           begin
           Found := false;
           break;
           end;
        end;
      if Found then break;
      end;
    if Found
      then Result := Offset
      else Result := -1;
    end;

  var
    AsterixAtBegin, AsterixAtEnd: boolean;
    SubMask : ShortString;
    i       : longint;
    TmpBuf  : PChar;
    FirstPos: longint;
    SaveMask: ShortString;

  begin
  Result := 0;

  if OneMask = '*' then exit;
  Result := -1;
  AsterixAtBegin := false;
  AsterixAtEnd := false;
  if ShortCopy(OneMask, 1, 1) = '*' then
    begin
    AsterixAtBegin := true;
    ShortDelete(OneMask, 1, 1);
    end;
  if OneMask = '' then exit;
  if ShortCopy(OneMask, length(OneMask), 1) = '*' then
    begin
    AsterixAtEnd := true;
    ShortDelete(OneMask, length(OneMask), 1);
    end;
  if OneMask = '' then exit;

  if not OemCompare then // faster way
    begin
    if not CaseSensitive then AnsiLowerCase(Buf);
    TmpBuf := Buf;
    FirstPos := -1;
    while OneMask <> '' do
      begin
      i := pos('*', OneMask);
      if i > 0 then
        begin
        SubMask := ShortCopy(OneMask, 1, pred(i));
        ShortDelete(OneMask, 1, i);
        end
      else
        begin
        SubMask := OneMask;
        OneMask := '';
        end;
      if Submask = '' then exit;
      i := PosQBuf (SubMask, TmpBuf);
      if i = -1 then exit;
      if FirstPos = -1 then FirstPos := i;
      if not AsterixAtBegin and (i <> 0) and
        IsLetter(TmpBuf[pred(i)]) then exit;
      AsterixAtBegin := true;
      TmpBuf := @TmpBuf[i + length(SubMask)];
      end;
    if not AsterixAtEnd and (StrLen(TmpBuf) <> 0) and
      IsLetter(TmpBuf[0]) then exit;
    Result := FirstPos;
    end

  else

    begin
    // here we do not lowercase the block, but use PosQBufEx which does
    // so for each character
    TmpBuf := Buf;
    FirstPos := -1;
    i := -1;
    SaveMask := OneMask;
    // First we should check the AnsiCompare
    if AnsiCompare then
      begin
      while OneMask <> '' do
        begin
        i := pos('*', OneMask);
        if i > 0 then
          begin
          SubMask := ShortCopy(OneMask, 1, pred(i));
          ShortDelete(OneMask, 1, i);
          end
        else
          begin
          SubMask := OneMask;
          OneMask := '';
          end;
        if Submask = '' then exit;
        i := PosQBufEx (SubMask, TmpBuf, false);
        if i = -1 then break;
        if FirstPos = -1 then FirstPos := i;
        if not AsterixAtBegin and (i <> 0) and
          IsLetter(TmpBuf[pred(i)]) then exit;
        AsterixAtBegin := true;
        TmpBuf := @TmpBuf[i + length(SubMask)];
        end;
      if (i <> -1) then
        begin
        if AsterixAtEnd or (StrLen(TmpBuf) = 0) or not IsLetter(TmpBuf[0])
          then Result := FirstPos;
        end;
      end;

    if Result = -1 then // still not found, try it in OEM
      begin
      OneMask := SaveMask;
      TmpBuf := Buf;
      FirstPos := -1;
      while OneMask <> '' do
        begin
        i := pos('*', OneMask);
        if i > 0 then
          begin
          SubMask := ShortCopy(OneMask, 1, pred(i));
          ShortDelete(OneMask, 1, i);
          end
        else
          begin
          SubMask := OneMask;
          OneMask := '';
          end;
        if Submask = '' then exit;
        i := PosQBufEx (SubMask, TmpBuf, true);
        if i = -1 then exit;
        if FirstPos = -1 then FirstPos := i;
        if not AsterixAtBegin and (i <> 0) and
          IsLetter(TmpBuf[pred(i)]) then exit;
        AsterixAtBegin := true;
        TmpBuf := @TmpBuf[i + length(SubMask)];
        end;
      if not AsterixAtEnd and (StrLen(TmpBuf) <> 0) and
        IsLetter(TmpBuf[0]) then exit;
      Result := FirstPos;
      end;

    end;
  end;

{$R-}
{$Q-}

//-----------------------------------------------------------------------------

function ByteRandom: byte;

   begin
   ByteRandSeed := succ($8405 * ByteRandSeed);
   ByteRandom   := byte(ByteRandSeed);
   end;

//-----------------------------------------------------------------------------

procedure UnpackTime(P: Longint; var T: TQDateTime);

  begin
  with T do
    begin
    Sec := (P and 31) * 2;
    P := P shr 5;
    Min := P and 63;
    P := P shr 6;
    Hour := P and 31;
    P := P shr 5;
    Day := P and 31;
    P := P shr 5;
    Month := P and 15;
    P := P shr 4;
    Year := P + 1980;
    end;
  end;

//-----------------------------------------------------------------------------

procedure PackTime(var T: TQDateTime; var P: Longint);

  begin
  with T do
    begin
    P := Year - 1980;
    P := P shl 4;
    P := P or Month;
    P := P shl 5;
    P := P or Day;
    P := P shl 5;
    P := P or Hour;
    P := P shl 6;
    P := P or Min;
    P := P shl 5;
    P := P or (Sec shr 1);
    end;
  end;

//-----------------------------------------------------------------------------

procedure AddToBuffer(CalcOnly: boolean;
                      S: ShortString;
                      var BufferPos: PChar;
                      var TotalLength: longint);

  begin
  inc(TotalLength, length(S));
  if not CalcOnly then
    begin
    Move(S[1], BufferPos^, length(S));
    BufferPos := BufferPos + length(S);
    end;
  end;

//---initialization------------------------------------------------------------

var
  KernelHandle: THandle;

begin
kBDivider := 1024;
WinDateFormat := wd_dmy;
{
if (pos('d', ShortDateFormat) > pos('M', ShortDateFormat)) and
   (pos('y', ShortDateFormat) > pos('d', ShortDateFormat)) then
     WinDateFormat := wd_mdy;
if (pos('d', ShortDateFormat) > pos('M', ShortDateFormat)) and
   (pos('M', ShortDateFormat) > pos('y', ShortDateFormat)) then
     WinDateFormat := wd_ymd;
}

SaveWinDateFormat := WinDateFormat;
Randomize;
ByteRandSeed := random(256);


@GetDiskFreeSpaceEx := nil;
{$ifdef mswindows}
KernelHandle := GetModuleHandle(kernel32);
if KernelHandle <> 0 then
  @GetDiskFreeSpaceEx := GetProcAddress(KernelHandle, 'GetDiskFreeSpaceExA');
{$endif}
end.


