unit DoIt;

interface

{$ifdef DLLDEBUG}

function OpenTransfer (FileName: PChar; var Handle: longword): longint;
function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint;
function CloseTransfer(Handle: longint): longint;

{$else}

function OpenTransfer (FileName: PChar; var Handle: longword): longint; export;
function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint; export;
function CloseTransfer(Handle: longint): longint; export;

{$endif}

{====================================================================}

implementation

uses
  WinTypes, WinProcs, SysUtils;

const
  ReadBufSize = 8*1024;

type
  TTransferClass = class
    ReadBuffer    : array [0..ReadBufSize-1] of char;
    ReadBufPos    : longint;
    WasRead       : longint;
    hInputFile    : LongWord;
    LastReadChar  : integer;
    end;

{--------------------------------------------------------------------}

function OpenTransfer (FileName: PChar; var Handle: longword): longint;

  var
    TC  : TTransferClass;

  begin
  Result := -1;
  Handle := 0;
  TC     := TTransferClass.Create;
  TC.ReadBufPos  := 0;
  TC.WasRead     := 0;
  TC.hInputFile  := CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ,
                               nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if TC.hInputFile = INVALID_HANDLE_VALUE then
    begin
    // if OpenTransfer is not successful, CloseTransfer is not called
    // so make a cleanup here
    TC.Free;
    exit;
    end;
  if not ReadFile(TC.hInputFile, TC.ReadBuffer[0], ReadBufSize, DWORD(TC.WasRead), nil) then
    begin
    CloseHandle(TC.hInputFile);
    TC.Free;
    exit;
    end;
  Handle := longint(TC);
  Result := 0;
  end;

{--------------------------------------------------------------------}

function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint;

  var
    TC: TTransferClass absolute Handle;

  function GetNextChar(var Ch: char): boolean;
    begin
    with TC do
      begin
      if ReadBufPos >= WasRead then
        begin
        if not ReadFile(hInputFile, ReadBuffer[0], ReadBufSize, DWORD(WasRead), nil) then
          raise EInOutError.Create('Cannot read.');
        ReadBufPos := 0;
        end;
      if ReadBufPos >= WasRead then
        begin
        Result := false;
        exit;
        end;
      Ch := ReadBuffer[ReadBufPos];
      inc(ReadBufPos);
      end;
    Result := true;
    end;


  var
    OK    : boolean;
    MaxLen: integer;
    Ch    : Char;

  begin
  CharsRead := 0;
  Result := -1;
  try
    MaxLen := pred(BufSize);
    OK := true;
    while (CharsRead < MaxLen) and OK do
      begin
      try
        OK := GetNextChar(Ch);
      except
        on Exception do
          begin
          Result := -1;
          exit;
          end;
        end;
      if OK then
        begin
        // do some filtering here...
        if (Ch < ' ') and (Ch <> #13) and (Ch <> #10) then Ch := ' ';
        Buf[CharsRead] := Ch;
        inc(CharsRead);
        end;
      end;
  except
    on Exception do
      begin
      Buf[CharsRead] := #0;
      exit;
      end;
    end;
  Buf[CharsRead] := #0;
  Result := 0;
  end;

{--------------------------------------------------------------------}

function CloseTransfer(Handle: longint): longint;

  var
    TC: TTransferClass absolute Handle;

  begin
  Result := -1;
  try
    if TC.hInputFile <> INVALID_HANDLE_VALUE then CloseHandle(TC.hInputFile);
    TC.Free;
  except
    on Exception do exit;
    end;
  Result := 0;
  end;

{--------------------------------------------------------------------}

end.
