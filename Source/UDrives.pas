unit UDrives;
(*====================================================================
Functions to get the list of available drives
======================================================================*)

interface

uses
  Classes,
  UStringList, UTypes, UApiTypes;

type
  TDriveType = (dtUnknown, dtNoDrive, dtFloppy, dtFixed,
                dtNetwork, dtCDROM, dtRAM);

type
  TPDrive = ^TDrive;
  TDrive  = record
    Name     : AnsiString;
    Fstype   : AnsiString;
    Mount    : AnsiString;
    Lbl      : AnsiString;
    end;


const
  FloppyAExists    : boolean = false;
  FloppyBExists    : boolean = false;
  ScanAllDriveNames: boolean = true;    {set by Program Settings dialog}

var
  ///DriveList      : TQStringList;
  DriveList      : TStringList;

procedure BuildDriveList;
{$ifdef mswindows}
function  QGetDriveName(DriveChar: char): ShortString;
{$else}
function  QGetDriveName(Drive: string): AnsiString;
{$endif}
function  ReadVolumeLabel(Disk: ShortString; var Name: ShortString;
                          var FileSystem: TFileSystem; var DriveType: integer): boolean;
function  WriteVolumeLabel (Disk: ShortString; Name: ShortString): boolean;
function  FindDriveType(DriveNum: Integer): TDriveType;




implementation

  uses
    {$ifdef mswindows}
    WinTypes,WinProcs,
    {$ELSE}
      Process,  Strutils, LCLIntf, LCLType, LMessages,
    {$ENDIF}
    SysUtils;



//----------------------------------------------------------------------------

function VolumeID(DriveChar: Char): ShortString;

  var
    OldErrorMode: Integer;
    NotUsed, VolFlags: longword;
    Buf: array [0..MAX_PATH] of Char;
    DriveSpec: array[0..3] of char;

  begin
  Result := '';
  {$ifdef mswindows}
  DriveSpec[0] := DriveChar;
  DriveSpec[1] := ':';
  DriveSpec[2] := '\';
  DriveSpec[3] := #0;
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(DriveSpec, Buf, sizeof(Buf),
      nil, NotUsed, VolFlags, nil, 0) then
    Result := StrPas(Buf);
  finally
    SetErrorMode(OldErrorMode);
    end;
  {$endif}
  end;

//----------------------------------------------------------------------------

function NetworkVolume(DriveChar: Char): ShortString;

  var
    Buf: Array [0..MAX_PATH] of Char;
    DriveStr: array [0..3] of Char;
    BufferSize: longword;

  begin
  {$ifdef mswindows}
  BufferSize := sizeof(Buf);
  DriveStr[0] := DriveChar;
  DriveStr[1] := ':';
  DriveStr[2] := #0;
  if WNetGetConnection(DriveStr, Buf, BufferSize) = WN_SUCCESS
    then Result := StrPas(Buf)
    else Result := VolumeID(DriveChar);
  {$endif}
  Result := VolumeID(DriveChar);
  end;

//----------------------------------------------------------------------------

function FindDriveType(DriveNum: Integer): TDriveType;

  var
    DriveSpec: array[0..3] of char;

  begin
  DriveSpec[0] := Char(DriveNum + Ord('a'));
  DriveSpec[1] := ':';
  DriveSpec[2] := '\';
  DriveSpec[3] := #0;
  ///Result := TDriveType(GetDriveType(DriveSpec));
  end;

//----------------------------------------------------------------------------
{$ifdef windows}
procedure BuildDriveList;

  var
    DriveNum: Integer;
    DriveChar: Char;
    DriveType: TDriveType;
    DriveBits: set of 0..25;

  begin
  DriveList.Clear;
  ///Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
    begin
    if not (DriveNum in DriveBits) then Continue;
    DriveChar := Char(DriveNum + Ord('a'));
    DriveType := FindDriveType(DriveNum);
    DriveChar := Upcase(DriveChar);
    if ScanAllDriveNames
      then
        case DriveType of
          dtFloppy:   DriveList.Add(DriveChar + ':');
          dtFixed:    DriveList.Add(DriveChar + ': [' + VolumeID(DriveChar) + ']');
          dtNetwork:  DriveList.Add(DriveChar + ': [' + NetworkVolume(DriveChar) + ']');
          dtCDROM:    DriveList.Add(DriveChar + ': [' + VolumeID(DriveChar) + ']');
          dtRAM:      DriveList.Add(DriveChar + ': [' + VolumeID(DriveChar) + ']');
          end
      else
        case DriveType of
          dtFloppy:   DriveList.Add(DriveChar + ':');
          dtFixed:    DriveList.Add(DriveChar + ':');
          dtNetwork:  DriveList.Add(DriveChar + ':');
          dtCDROM:    DriveList.Add(DriveChar + ':');
          dtRAM:      DriveList.Add(DriveChar + ':');
          end
    end;
  end;
{$else}

type
  TSarray = array of string;

  function Split(Text, Delimiter: string): TSarray;
  var
    i: integer;
    Len: integer;
    PosStart: integer;
    PosDel: integer;
  begin
    i := 0;
    SetLength(Result, 1);
    Len := Length(Delimiter);
    PosStart := 1;
    PosDel := Pos(Delimiter, Text);
    while PosDel > 0 do
      begin
        Result[i] := Copy(Text, PosStart, PosDel - PosStart);
        Result[i]:=StringReplace(Result[i],'"','',[rfReplaceAll]);
        ///PosStart := PosDel + Len;
        Text:=copy(text,PosDel+1,length(text));
        PosDel := Pos(Delimiter, Text);//, PosStart);
        inc(i);
        SetLength(Result, i + 1);
      end;
    Result[i] := Copy(Text, PosStart, Length(Text));
    Result[i]:=StringReplace(Result[i],'"','',[rfReplaceAll]);
  end;


// NAME="sda1" FSTYPE="ext4" MOUNTPOINT="/mnt/sdc1" LABEL="SDC1"
procedure BuildDriveList;
var
  s: string;
  name,fstype,mount,lbl : string;
  list: TStringList;
  i: integer;
  l: TSarray;
  Drive: TPDrive;
  begin
    //FreeObjects(DriveList);
    for i := 0 to DriveList.Count - 1 do
      Dispose(TPDrive(DriveList.Objects[i]));

    DriveList.Clear;
    List:=TStringList.Create;
    //RunCommand('/usr/bin/lsblk', ['-nP', '-o NAME,FSTYPE,MOUNTPOINT,LABEL'], s);
    RunCommand('/usr/bin/sudo', ['/usr/bin/lsblk','-nP','-o', 'NAME,FSTYPE,MOUNTPOINT,LABEL'], s);
    ExtractStrings([#10], [], PChar(s), List);
    for i:=0 to List.Count -1 do
    begin
      l:=Split(list[i], ' ');
      //s:=StringReplace(list[i],'"','',[rfReplaceAll]);
      name:=StringReplace(l[0],'NAME=','',[rfReplaceAll]);
      fstype:=StringReplace(l[1],'FSTYPE=','',[rfReplaceAll]);
      mount:=StringReplace(l[2],'MOUNTPOINT=','',[rfReplaceAll]);
      lbl:=StringReplace(l[3],'LABEL=','',[rfReplaceAll]);
      //SScanf(List[i],'NAME="%s" FSTYPE="%s" MOUNTPOINT="%s" LABEL="%s"',[@name,@fstype,@mount,@lbl]);
      if mount <> '' then
      begin
        New(Drive);
        Drive^.Name:=name;
        Drive^.Fstype:=fstype;
        Drive^.Mount:=mount;
        Drive^.Lbl:=lbl;
        DriveList.AddObject(mount,TObject(Drive));
      end;
    end;
    List.Free;
  end;
{$endif}




//----------------------------------------------------------------------------
{$ifdef mswindows}

function QGetDriveName(DriveChar: char): ShortString;
  var
    DriveNum : Integer;
    DriveType: TDriveType;

  begin
    DriveChar := UpCase(DriveChar);
    DriveNum  := integer(byte(DriveChar) - byte('A'));
    DriveType := FindDriveType(DriveNum);
    Result := '';
    case DriveType of
      dtFloppy:   Result := VolumeID(DriveChar);
      dtFixed:    Result := VolumeID(DriveChar);
      dtNetwork:  Result := NetworkVolume(DriveChar);
      dtCDROM:    Result := VolumeID(DriveChar);
      dtRAM:      Result := VolumeID(DriveChar);
      end;
  end;
{$else}
function QGetDriveName(Drive: String): AnsiString;
var
  i: integer;
  s,t: string;
begin
  if DriveList.Count < 1 then BuildDriveList;
  // first is root
  result:= TPDrive(DriveList.Objects[0])^.Lbl;
  for i:=1 to  DriveList.Count-1 do
  begin
    s:=TPDrive(DriveList.Objects[i])^.Mount;
    t:=copy(Drive,1,length(s));
    if s=t then
    begin
      Result:=TPDrive(DriveList.Objects[i])^.Lbl;
      break;
    end;
  end;
  for i := 0 to DriveList.Count - 1 do
    Dispose(TPDrive(DriveList.Objects[i]));
  DriveList.Clear;
end;
{$endif}

//----------------------------------------------------------------------------
//  Disk is ShortString in format 'A:'
{$ifdef mswindows}
function ReadVolumeLabel (Disk: ShortString; var Name: ShortString;
                          var FileSystem: TFileSystem; var DriveType: integer): boolean;

  var
    RootPathName: array[0..255] of char;
    VolumeNameBuffer: array[0..255] of char;
    VolumeFileSystem: array[0..50] of char;
    MaximumComponentLength: DWORD;
    FileSystemFlags: DWORD;

  begin
  StrPCopy(RootPathName, Disk + '\');

  ///Result := GetVolumeInformation (RootPathName, VolumeNameBuffer, 255,
  ///           nil, MaximumComponentLength, FileSystemFlags, VolumeFileSystem, 50);
  Name := StrPas(VolumeNameBuffer);
  FileSystem := Unknown;
  if StrComp(VolumeFileSystem, 'FAT') = 0  then FileSystem := FAT;
  if StrComp(VolumeFileSystem, 'HPFS') = 0 then FileSystem := HPFS;
  if StrComp(VolumeFileSystem, 'NTFS') = 0 then FileSystem := NTFS;
  ///DriveType := GetDriveType(RootPathName);
  end;
{$else}
function ReadVolumeLabel (Disk: ShortString; var Name: ShortString;
                          var FileSystem: TFileSystem; var DriveType: integer): boolean;

var
  i: integer;
  s,t: string;
begin
  if DriveList.Count < 1 then BuildDriveList;
  // first is root
  result:= false;
  DriveType:=0 ;
  for i:=1 to  DriveList.Count-1 do
  begin
    s:=TPDrive(DriveList.Objects[i])^.Mount;
    t:=copy(Disk,1,length(s));
    if s=t then
    begin
      Name:=TPDrive(DriveList.Objects[i])^.Lbl;
      FileSystem:=Unknown;
      if TPDrive(DriveList.Objects[i])^.Fstype = 'fat'    then FileSystem:=FAT;
      if TPDrive(DriveList.Objects[i])^.Fstype = 'vfat'   then FileSystem:=VFAT;
      if TPDrive(DriveList.Objects[i])^.Fstype = 'hpfs'   then FileSystem:=HPFS;
      if TPDrive(DriveList.Objects[i])^.Fstype = 'ntfs'   then FileSystem:=NTFS;
      if TPDrive(DriveList.Objects[i])^.Fstype = 'ext4'   then FileSystem:=EXT4;
      if TPDrive(DriveList.Objects[i])^.Fstype = 'cdrom'  then FileSystem:=CDROM;
      Result:=true;
      break;
    end;
  end;
  for i := 0 to DriveList.Count - 1 do
    Dispose(TPDrive(DriveList.Objects[i]));
  DriveList.Clear;
end;

{$endif}

//----------------------------------------------------------------------------
//  Disk is ShortString in format 'A:'

function WriteVolumeLabel (Disk: ShortString; Name: ShortString): boolean;

  var
    RootPathName: array[0..255] of char;
    VolumeNameBuffer: array[0..255] of char;

  begin
  StrPCopy(RootPathName, Disk + '\');
  StrPCopy(VolumeNameBuffer, Name);
  ///Result := SetVolumeLabel(RootPathName, VolumeNameBuffer);
  end;

//----------------------------------------------------------------------------

begin
///DriveList := TQStringList.Create;
DriveList := TStringList.Create;
DriveList.Sorted     := true;
FloppyAExists := FindDriveType(0) = dtFloppy;
FloppyBExists := FindDriveType(1) = dtFloppy;
end.
