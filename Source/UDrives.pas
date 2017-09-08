unit UDrives;
(*====================================================================
Functions to get the list of available drives
======================================================================*)

interface

uses UStringList, UTypes, UApiTypes;

type
  TDriveType = (dtUnknown, dtNoDrive, dtFloppy, dtFixed,
                dtNetwork, dtCDROM, dtRAM);
const
  FloppyAExists    : boolean = false;
  FloppyBExists    : boolean = false;
  ScanAllDriveNames: boolean = true;    {set by Program Settings dialog}

var
  DriveList      : TQStringList;

procedure BuildDriveList;
function  QGetDriveName(DriveChar: char): ShortString;

function  ReadVolumeLabel(Disk: ShortString; var Name: ShortString;
                          var FileSystem: TFileSystem; var DriveType: integer): boolean;
function  WriteVolumeLabel (Disk: ShortString; Name: ShortString): boolean;
function  FindDriveType(DriveNum: Integer): TDriveType;



implementation

  uses
    {$ifdef mswindows}
    WinTypes,WinProcs,
    {$ELSE}
      LCLIntf, LCLType, LMessages,
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

//----------------------------------------------------------------------------

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

//----------------------------------------------------------------------------
//  Disk is ShortString in format 'A:'

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
DriveList := TQStringList.Create;
FloppyAExists := FindDriveType(0) = dtFloppy;
FloppyBExists := FindDriveType(1) = dtFloppy;
end.
