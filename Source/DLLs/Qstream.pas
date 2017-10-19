unit Qstream;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface
  uses Classes;

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
    Status     : Integer;
    {$ifdef mswindows}
    Handle      : Integer;
    {$else}
    Stream      : TFileStream;
    {$endif}
    StreamMode : Integer;
    CurrentPos : longint;
    CurrentSize: longint;
    CheckReading: boolean; {pokud true, bude se kontrolovat seek and read, zda
                            nejde mimo soubor - ale soubor se neuzavira}
    FileName   : array [0..255] of char;
    constructor Init    (aFileName: PChar; aStreamMode: Integer);
    destructor  Done;
    procedure   Error   (Code: Integer);
    procedure   ReadExt (var Buf; Count: longint; var WasRead: longint);
    procedure   Read    (var Buf; Count: longint);
    procedure   Write   (var Buf; Count: longint);
    function    GetPos : Longint;
    function    GetSize: Longint;
    procedure   Seek    (Pos: Longint);
    procedure   SeekToEnd;
    procedure   Flush;
    function    Eof    : boolean;
  private
    procedure   StreamOpen;
    procedure   StreamClose;
  end;


{==================================================================}

implementation

uses
  SysUtils, Qexcept;

{$R-}

const
  stStreamClosed = 0;

  lsFileAccessError = 'File access error.';
  lsFileCreationError = 'File creation or open error.';
  lsFileReadingError = 'File reading error.';
  lsFileWritingError = 'File writing error.';
  lsFileSeekingError = 'File seeking error.';
  lsFileError = 'File error: ';
  lsDBaseStructError = 'Error in database structure found. Close the database and call Repair from the File menu. ';

{------------------------------------------------------------------}

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

{------------------------------------------------------------------}

procedure TQBufStream.StreamOpen;

  var
    LastError: longint;
    szBuffer: array[0..200] of char;

  begin
  {$ifndef mswindows}
  case StreamMode of
    stCreate:
      begin
      Stream :=  TFileStream.Create(FileName, fmCreate);
      end;
    stOpenExclusive:
      begin
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive);
      end;
    stOpenReadNonExclusive:
      begin
      Stream := TFileStream.Create(FileName, fmOpenRead);
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
      Handle := CreateFile(FileName, GENERIC_READ or GENERIC_WRITE,
                           0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    stOpenExclusive:
      Handle := CreateFile(FileName, GENERIC_READ or GENERIC_WRITE,
                           0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    stOpenReadNonExclusive:
      Handle := CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ,
                           nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
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

{------------------------------------------------------------------}

procedure TQBufStream.StreamClose;

  begin
  {$ifdef mswindows}
  {$ifdef WIN32}
  if Handle <> INVALID_HANDLE_VALUE then FileClose(Handle);{ *Převedeno z CloseHandle* }
  Handle := INVALID_HANDLE_VALUE;
  {$else}
  if Handle <> HFILE_ERROR then _lclose(Handle);
  Handle := HFILE_ERROR;
  {$endif}
  {$else}
  if Stream <> nil then
  try
    Stream.Free;
  except
  end;
  {$endif}
  end;

{------------------------------------------------------------------}

constructor TQBufStream.Init(aFileName: PChar; aStreamMode: Integer);

  begin
  Status      := stOK;
  StreamMode  := aStreamMode;
  CurrentPos  := 0;
  CurrentSize := 0;
  CheckReading := false;
  StrCopy(FileName, aFileName);
  StreamOpen;
  end;

{------------------------------------------------------------------}

destructor TQBufStream.Done;
  begin
  if StreamMode = stStreamClosed then exit;
  StreamClose;
  StreamMode := stStreamClosed;
  end;

{------------------------------------------------------------------}

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

{------------------------------------------------------------------}

function TQBufStream.GetPos: Longint;

  begin
  Result := 0;
  if StreamMode = stStreamClosed then exit;
  Result := CurrentPos;
  end;

{------------------------------------------------------------------}

function TQBufStream.GetSize: Longint;

  begin
  Result := 0;
  if StreamMode = stStreamClosed then exit;
  Result := CurrentSize;
  end;

{------------------------------------------------------------------}

function TQBufStream.Eof: boolean;

  begin
  Result := true;
  if StreamMode = stStreamClosed then exit;
  Result := CurrentPos >= CurrentSize;
  end;

{------------------------------------------------------------------}

procedure TQBufStream.Seek(Pos: Longint);

  begin
  {pokud kontrola cteni, pak chyba nezpusobi zavreni databaze}
  if CheckReading then
    if (Pos < 0) or (Pos > CurrentSize) then
      raise EQDirDBaseStructException.Create(lsDBaseStructError + ' (001)');
  if StreamMode = stStreamClosed then exit;
  {$ifdef mswindows}
  {$ifdef WIN32}
    CurrentPos  := SetFilePointer(Handle, Pos, nil, FILE_BEGIN);
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

{------------------------------------------------------------------}

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

{------------------------------------------------------------------}

procedure TQBufStream.ReadExt(var Buf; Count: longint; var WasRead: longint);

  begin
  {pokud kontrola cteni, pak chyba nezpusobi zavreni databaze}
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

{------------------------------------------------------------------}

procedure TQBufStream.Read(var Buf; Count: longint);

  var
    WasRead: longint;

  begin
  ReadExt(Buf, Count, WasRead);
  if WasRead <> Count then Error(stReadError);
  end;

{------------------------------------------------------------------}

procedure TQBufStream.Write(var Buf; Count: longint);

  var
    WasWritten: longint;

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
  inc(CurrentPos, WasWritten);
  if CurrentPos > CurrentSize then CurrentSize := CurrentPos;
  end;

{------------------------------------------------------------------}

end.
