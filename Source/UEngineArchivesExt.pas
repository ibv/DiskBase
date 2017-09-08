unit UEngineArchivesExt;
(*====================================================================
Functions for accessing RAR and ACE archives via DLLs from RAR and ACE
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

//-----------------------------------------------------------------------------


implementation

uses
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages, ComCtrls,
  {$ENDIF}
  SysUtils, UBaseUtils;

var
  DllHandle: THandle;
  RarOpenArchiveData: TRarOpenArchiveData;
  RarHeaderData     : TRarHeaderData;
  DllName: String;
  ZString: array[0..256] of char;
  iRarDllLoaded : integer;
  iVersion: longint;

  iAceDllLoaded : integer;

//-----------------------------------------------------------------------------

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
    iVersion := RarGetDllVersion;
    end;
  Result := iRarDllLoaded = 1;
  {$endif}
  end;

//-----------------------------------------------------------------------------


begin
iRarDllLoaded := -1;
iAceDllLoaded := -1;
end.
