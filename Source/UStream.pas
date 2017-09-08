unit UStream;
(*====================================================================
Implementation of a stream, used for all file reads/writes
======================================================================*)

interface

uses UTypes, Classes;

const

  HFILE_ERROR = -1;
  stCreate               = 1;
  stOpenExclusive        = 2;
  stOpenReadNonExclusive = 3;

{ TQStream error codes }
  stOk         =  0;
  stError      = -1;
  stInitError  = -2;
  stReadError  = -3;
  stWriteError = -4;
  stSeekError  = -5;


type

  TQBufStream = object
    Status      : Integer;
    {$ifdef mswindows}
    Handle      : LongWord;
    {$else}
    Stream      : TFileStream;
    {$endif}
    StreamMode  : Integer;
    CurrentPos  : longword;
    CurrentSize : longword;
    CheckReading: boolean; // if true, the read and seek are checked if they
                           // go outside the file size, but the error does not
                           // close the stream
    Modified    : boolean;
    SaveArchiveAttrib: boolean;
    FileName    : array [0..255] of char;
    constructor Init    (aFileName: PChar; aStreamMode: Integer);
    destructor  Done;
    procedure   Error   (Code: Integer);
    procedure   ReadExt (var Buf; Count: longword; var WasRead: longword);
    procedure   Read    (var Buf; Count: longword);
    procedure   Write   (var Buf; Count: longword);
    function    GetPos : Longword;
    function    GetSize: Longword;
    procedure   Seek    (Pos: Longword);
    procedure   SeekToEnd;
    procedure   Flush;
    function    Eof    : boolean;
  private
    procedure   StreamOpen;
    procedure   StreamClose;
  end;


//=============================================================================

implementation

uses
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  SysUtils, UExceptions, ULang;

{$R-}

const
  stStreamClosed = 0;

//-----------------------------------------------------------------------------

procedure TQBufStream.Error(Code: Integer);

  var
    ErrorMsg: ShortString;
    SaveStreamMode : Integer;

  begin
  Status := Code;
  case Status of
    stOk        : exit;
    stError     : ErrorMsg := lsFileAccessError;
    stInitError : ErrorMsg := lsFileCreationError;
    stReadError : ErrorMsg := lsFileReadingError;
    stWriteError: ErrorMsg := lsFileWritingError;
    stSeekError : ErrorMsg := lsFileSeekingError;
    else ErrorMsg := lsFileError + IntToStr(Status);
    end;
  SaveStreamMode := StreamMode;
  StreamMode := stStreamClosed;
  StreamClose;
  if SaveStreamMode = stOpenReadNonExclusive
    then raise EQDirNormalException.Create(ErrorMsg)
    else raise EQDirFatalException.Create(ErrorMsg);
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.StreamOpen;


  begin
  {$ifndef mswindows}
  case StreamMode of
    stCreate:
      begin
      Stream :=  TFileStream.Create(FileName, fmCreate);
      SaveArchiveAttrib := true;
      end;
    stOpenExclusive:
      begin
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive);
      SaveArchiveAttrib := true;
      ///SaveArchiveAttrib := (GetFileAttributes(FileName) and FILE_ATTRIBUTE_ARCHIVE) <> 0;
      end;
    stOpenReadNonExclusive:
      begin
      Stream := TFileStream.Create(FileName, fmOpenRead);
      SaveArchiveAttrib := true;
      end;
    end;
    CurrentSize := Stream.Size;
    if CurrentSize = $FFFFFFFF then Error(stSeekError);
    CurrentPos  := Stream.Seek(0,0);
    if CurrentPos = $FFFFFFFF then Error(stSeekError);

  {$else}
  {$ifdef WIN32}
  case StreamMode of
    stCreate:
      begin
      Handle := CreateFile(FileName, GENERIC_READ or GENERIC_WRITE,
                           0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      SaveArchiveAttrib := true;
      end;
    stOpenExclusive:
      begin
      Handle := CreateFile(FileName, GENERIC_READ or GENERIC_WRITE,
                           0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      SaveArchiveAttrib := (GetFileAttributes(FileName) and FILE_ATTRIBUTE_ARCHIVE) <> 0;
      end;
    stOpenReadNonExclusive:
      begin
      Handle := CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ,
                           nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      SaveArchiveAttrib := true;
      end;
    end;
  if Handle = INVALID_HANDLE_VALUE
    then
      begin
      {
      LastError := GetLastError;
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LastError,
                    0, szBuffer, 200, nil);
      }
      Error(stInitError);
      end
    else
      begin
      CurrentSize := SetFilePointer(Handle, 0, nil, FILE_END);
      if CurrentSize = $FFFFFFFF then Error(stSeekError);
      CurrentPos  := SetFilePointer(Handle, 0, nil, FILE_BEGIN);
      if CurrentPos = $FFFFFFFF then Error(stSeekError);
      end;
  {$else}
  case StreamMode of
    stCreate:
      Handle := _lcreat(FileName, 0);
    stOpenExclusive:
      Handle := _lopen(FileName, OF_READWRITE or OF_SHARE_EXCLUSIVE);
    stOpenReadNonExclusive:
      Handle := _lopen(FileName, OF_READ or OF_SHARE_DENY_NONE);
    end;
  if Handle = HFILE_ERROR
    then
      Error(stInitError)
    else
      begin
      CurrentSize := _llseek(Handle, 0, 2);
      CurrentPos  := _llseek(Handle, 0, 0);
      end;
  {$endif}
  {$endif}
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.StreamClose;

  begin
  {$ifdef mswindows}
  {$ifdef WIN32}
  if Handle <> INVALID_HANDLE_VALUE then CloseHandle(Handle);
  Handle := INVALID_HANDLE_VALUE;
  if ((GetFileAttributes(FileName) and FILE_ATTRIBUTE_ARCHIVE) <> 0) then
    begin
    // Archive attrib was set to true
    if (not Modified and not SaveArchiveAttrib) then
      begin
      // file was not modified and Archive was not set before opening, so repair
      // the situation - we do not set the Modified flag in case only header is
      // updated
      SetFileAttributes(FileName,
                        GetFileAttributes(FileName) and not FILE_ATTRIBUTE_ARCHIVE);
      end;
    end;
  {$else}
  if Handle <> HFILE_ERROR then _lclose(Handle);
  Handle := HFILE_ERROR;
  {$endif}
  {$else}
   Stream.Free;
  {$endif}
  end;

//-----------------------------------------------------------------------------

constructor TQBufStream.Init(aFileName: PChar; aStreamMode: Integer);

  begin
  Status       := stOK;
  StreamMode   := aStreamMode;
  CurrentPos   := 0;
  CurrentSize  := 0;
  CheckReading := false;
  Modified     := false;
  SaveArchiveAttrib := true;
  StrCopy(FileName, aFileName);
  StreamOpen;
  end;

//-----------------------------------------------------------------------------

destructor TQBufStream.Done;
  begin
  if StreamMode = stStreamClosed then exit;
  StreamClose;
  StreamMode := stStreamClosed;
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.Flush;

  begin
  if StreamMode = stStreamClosed then exit;
  {$ifdef mswindows}
  {$ifdef WIN32}
  FlushFileBuffers(Handle);
  {$else}
  StreamClose;
  StreamOpen;
  {$endif}
  {$endif}
  end;

//-----------------------------------------------------------------------------

function TQBufStream.GetPos: Longword;

  begin
  Result := 0;
  if StreamMode = stStreamClosed then exit;
  Result := CurrentPos;
  end;

//-----------------------------------------------------------------------------

function TQBufStream.GetSize: Longword;

  begin
  Result := 0;
  if StreamMode = stStreamClosed then exit;
  Result := CurrentSize;
  end;

//-----------------------------------------------------------------------------

function TQBufStream.Eof: boolean;

  begin
  Result := true;
  if StreamMode = stStreamClosed then exit;
  Result := CurrentPos >= CurrentSize;
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.Seek(Pos: Longword);

  ///var
  ///  DistanceHigh: longword;

  begin
  // if CheckReading is set, the error does not closes the database
  if CheckReading then
    if (Pos > CurrentSize) then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (001)');
  if StreamMode = stStreamClosed then exit;
  {$ifdef mswindows}
  {$ifdef WIN32}
    DistanceHigh := 0;
    CurrentPos  := SetFilePointer(Handle, Pos, @DistanceHigh, FILE_BEGIN);
    if CurrentPos = $FFFFFFFF then Error(stSeekError);
  {$else}
    CurrentPos := _llseek(Handle, Pos, 0);
    if CurrentPos = HFILE_ERROR then Error(stSeekError);
  {$endif}
  {$else}
   ///DistanceHigh := 0;
   Stream.Seek(pos,0);
   CurrentPos := Stream.Position;
   if CurrentPos = $FFFFFFFF then Error(stSeekError);
  {$endif}
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.SeekToEnd;

  begin
  if StreamMode = stStreamClosed then exit;
  {$ifdef mswindows}
  {$ifdef WIN32}
  if CurrentPos <> CurrentSize then
    CurrentPos  := SetFilePointer(Handle, 0, nil, FILE_END);
  if CurrentPos = $FFFFFFFF then Error(stSeekError);
  {$else}
  if CurrentPos <> CurrentSize then
    CurrentPos := _llseek(Handle, 0, 2);
  if CurrentPos = HFILE_ERROR then Error(stSeekError);
  {$endif}
  {$else}
  if CurrentPos <> CurrentSize then
  begin
    Stream.Seek(Stream.Size,0);
    CurrentPos := Stream.Position;
  end;
  if CurrentPos = $FFFFFFFF then Error(stSeekError);

  {$endif}
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.ReadExt(var Buf; Count: longword; var WasRead: longword);

  begin
  // if CheckReading is set, the error does not closes the database
  if CheckReading then
    if (CurrentPos+Count) > CurrentSize then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (002)');
  if StreamMode = stStreamClosed then Error(stReadError);
  {$ifdef mswindows}
  {$ifdef WIN32}
  if not ReadFile(Handle, Buf, Count, DWORD(WasRead), nil) then
    Error(stReadError);
  {$else}
  WasRead := _lread(Handle, @Buf, Count);
  if Integer(WasRead) = HFILE_ERROR then Error(stReadError);
  {$endif}
  {$else}
  WasRead := Stream.Read(Buf,Count);
  {$endif}
  inc(CurrentPos, WasRead);
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.Read(var Buf; Count: longword);

  var
    WasRead: longword;

  begin
  ReadExt(Buf, Count, WasRead);
  if WasRead <> Count then Error(stReadError);
  end;

//-----------------------------------------------------------------------------

procedure TQBufStream.Write(var Buf; Count: longword);

  var
    WasWritten: longword;

  begin
  if (StreamMode = stStreamClosed) or
     (StreamMode = stOpenReadNonExclusive) then exit;
  {$ifdef mswindows}
  {$ifdef WIN32}
  if not WriteFile(Handle, Buf, Count, DWORD(WasWritten), nil) then
    Error(stWriteError);
  if WasWritten <> Count then Error(stWriteError);
  {$else}
  WasWritten := _lwrite(Handle, @Buf, Count);
  if Integer(WasWritten) = HFILE_ERROR then Error(stWriteError);
  if WasWritten <> Count then Error(stWriteError);
  {$endif}
  {$else}
  WasWritten := Stream.Write(buf,Count);
  if WasWritten <> Count then Error(stWriteError);
  {$endif}
  Modified := true;
  inc(CurrentPos, WasWritten);
  if CurrentPos > CurrentSize then CurrentSize := CurrentPos;
  end;

//-----------------------------------------------------------------------------

end.
