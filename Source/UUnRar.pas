unit UUnRar;
(*====================================================================
RAR files decompression
======================================================================*)

interface

const
  ERAR_END_ARCHIVE     = 10;
  ERAR_NO_MEMORY       = 11;
  ERAR_BAD_DATA        = 12;
  ERAR_BAD_ARCHIVE     = 13;
  ERAR_UNKNOWN_FORMAT  = 14;
  ERAR_EOPEN           = 15;
  ERAR_ECREATE         = 16;
  ERAR_ECLOSE          = 17;
  ERAR_EREAD           = 18;
  ERAR_EWRITE          = 19;
  ERAR_SMALL_BUF       = 20;
  ERAR_UNKNOWN         = 21;

  RAR_OM_LIST          =  0;
  RAR_OM_EXTRACT       =  1;

  RAR_SKIP             =  0;
  RAR_TEST             =  1;
  RAR_EXTRACT          =  2;

  RAR_VOL_ASK          =  0;
  RAR_VOL_NOTIFY       =  1;

type
  TRarOpenArchiveData = record
    ArcName   : PChar;
    OpenMode  : longword;
    OpenResult: longword;
    CmtBuf    : PChar;
    CmtBufSize: longword;
    CmtSize   : longword;
    CmtState  : longword;
    end;

  TRarHeaderData = record
    ArcName   : array[0..259] of char;
    FileName  : array[0..259] of char;
    Flags     : longword;
    PackSize  : longword;
    UnpSize   : longword;
    HostOS    : longword;
    FileCRC   : longword;
    FileTime  : longword;
    UnpVer    : longword;
    Method    : longword;
    FileAttr  : longword;
    CmtBuf    : pChar;
    CmtBufSize: longword;
    CmtSize   : longword;
    CmtState  : longword;
    end;


  TRarOpenArchive   = function (var ArchiveData: TRarOpenArchiveData): longword; stdcall;
  TRarCloseArchive  = function (hArcData: longword): longint; stdcall;
  TRarReadHeader    = function (hArcData: longword; var HeaderData: TRarHeaderData): longint; stdcall;
  TRarProcessFile   = function (hArcData: longword; Operation: longint;
                                DestPath: pChar; DestName: pChar): longint; stdcall;
  TRarGetDllVersion = function (): longint; stdcall;

var
  RarOpenArchive  : TRarOpenArchive;
  RarCloseArchive : TRarCloseArchive;
  RarReadHeader   : TRarReadHeader;
  RarProcessFile  : TRarProcessFile;
  RarGetDllVersion: TRarGetDllVersion;

function UnRarDllLoaded(): boolean;
function ExtractRarFile(szArchiveFileName: PChar;
                        szFileToExtract: PChar;
                        szTargetFileName: PChar;
                        iMaxSizeToExtract: longint;
                        iArchiveOffset: longint): longint;

implementation

uses
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  SysUtils, UBaseUtils;

var
  DllHandle: THandle;
  //RarOpenArchiveData: TRarOpenArchiveData;
  //RarHeaderData     : TRarHeaderData;
  DllName: String;
  ZString: array[0..256] of char;
  iRarDllLoaded : integer;

function UnRarDllLoaded(): boolean;
  begin
  Result := false;
  {$ifdef mswindows}
  if (iRarDllLoaded = -1) then // we did not try it yet
    begin
    iRarDllLoaded := 0;
    DllName := 'unrar.dll';
    DllHandle := LoadLibrary(StrPCopy(ZString, GetProgramDir + DllName));
    if DllHandle <= 32 then exit;
    @RarOpenArchive  := GetProcAddress(DllHandle, 'RAROpenArchive' );
    @RarCloseArchive := GetProcAddress(DllHandle, 'RARCloseArchive');
    @RarReadHeader   := GetProcAddress(DllHandle, 'RARReadHeader'  );
    @RarProcessFile  := GetProcAddress(DllHandle, 'RARProcessFile' );
    @RarGetDllVersion:= GetProcAddress(DllHandle, 'RARGetDllVersion');

    if ((@RarOpenArchive  = nil) or
        (@RarCloseArchive = nil) or
        (@RarReadHeader   = nil) or
        (@RarProcessFile  = nil) or
        (@RarGetDllVersion= nil)) then
      begin
      FreeLibrary(DllHandle);
      exit;
      end;
    iRarDllLoaded := 1;
    end;
  Result := iRarDllLoaded = 1;
  {$else}
  {$endif}
  end;

//-----------------------------------------------------------------------------

function ExtractRarFile(szArchiveFileName: PChar;
                        szFileToExtract: PChar;
                        szTargetFileName: PChar;
                        iMaxSizeToExtract: longint;
                        iArchiveOffset: longint): longint;
  var
    hRarArchive: integer;
    RarOpenArchiveData: TRarOpenArchiveData;
    RarHeaderData : TRarHeaderData;
    RHCode       : longint;
    PFCode       : longint;
    //Dir          : ShortString;
    //FileName     : ShortString;
    //OrigSize     : longint;
    //DateTime     : longint;
    //Attr         : longint;
    //LocHeadOffset: longint;
    iLen         : integer;
    //LastSlash    : integer;
    //i            : integer;
    szFileBuf       : array[0..512] of char;

  begin
  Result := 100; // dummy error value
  if not UnRarDllLoaded then exit;

  RarOpenArchiveData.ArcName    := szArchiveFileName;
  RarOpenArchiveData.OpenMode   := RAR_OM_EXTRACT;
  RarOpenArchiveData.OpenResult := 0;
  RarOpenArchiveData.CmtBuf     := nil;
  RarOpenArchiveData.CmtBufSize := 0;
  RarOpenArchiveData.CmtSize    := 0;
  RarOpenArchiveData.CmtState   := 0;

  hRarArchive := RarOpenArchive(RarOpenArchiveData);
  if (RarOpenArchiveData.OpenResult <> 0) then // unsuccessful
    begin
    RarCloseArchive(hRarArchive);
    end;

  repeat
    RHCode:= RARReadHeader(hRarArchive, RarHeaderData);
    if RHCode<>0 then Break;

    if (RarHeaderData.FileAttr and
       (faDirectory or faVolumeID) = 0) and
       (strlen(RarHeaderData.FileName) < 512) then
      begin
      ///OemToCharBuff(@(RarHeaderData.FileName), @szFileBuf, strlen(RarHeaderData.FileName)+1);
      if StrIComp(szFileBuf, szFileToExtract) = 0 then
        begin
        // we found it
        iLen := strlen(szTargetFileName);
        //CopyMemory(@szFile, , iLen+1);
        ///CharToOemBuff(szTargetFileName, @szFileBuf, iLen+1);
        PFCode:= RARProcessFile(hRarArchive, RAR_EXTRACT, nil, szFileBuf);
        Result := PFCode;
        RarCloseArchive(hRarArchive);
        exit;
        end;
      end;

    PFCode:= RARProcessFile(hRarArchive, RAR_SKIP, nil, nil);
    if (PFCode<>0) then Break;
  until False;

  RarCloseArchive(hRarArchive);
  end;

//-----------------------------------------------------------------------------

begin
iRarDllLoaded := -1;
end.
