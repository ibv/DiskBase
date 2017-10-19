unit DoIt;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$ifdef DLLDEBUG}

function OpenTransfer (FileName: PChar; var Handle: longint): longint;
function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint;
function CloseTransfer(Handle: longint): longint;

{$else}

function OpenTransfer (FileName: PChar; var Handle: longint): longint; export;
function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint; export;
function CloseTransfer(Handle: longint): longint; export;

{$endif}

{====================================================================}

implementation

uses SysUtils,
     Qstream;

const
  ReadBufSize = 8*1024;

type

  TTransferClass = class
    ReadBuffer    : array [0..ReadBufSize-1] of char;
    ReadBufPos    : longint;
    WasRead       : longint;
    InputFile     : TQBufStream;
    FileFormat    : (unknown, WinWord);

    ReturnLF      : boolean;
    SavedFCh      : char;
    OneWordBuffer : ShortString;
    LastReadChar  : integer;
    end;

{--------------------------------------------------------------------}

function OpenTransfer (FileName: PChar; var Handle: longint): longint;

  var
    TC: TTransferClass;

  begin
  Result := -1;
  try
    TC := TTransferClass.Create;
    TC.ReadBufPos  := 0;
    TC.WasRead := 0;
    TC.OneWordBuffer := '';
    Handle := longint(TC);
    TC.ReturnLF := false;
    TC.InputFile.Init(FileName, stOpenReadNonExclusive);
    TC.FileFormat := Unknown;
    TC.InputFile.ReadExt(TC.ReadBuffer[0], ReadBufSize, TC.WasRead);
    TC.SavedFCh := #0;
  except
    on Exception do exit;
    end;
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
        InputFile.ReadExt(ReadBuffer[0], {Read}BufSize, WasRead);
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


  function GetNextFilteredChar(var FCh: char): boolean;

    var
      FInt, SInt: SmallInt;
      Ch        : char;
      Tag       : ShortString;
      TagBreak  : boolean;

    begin
    Result := true;
    if TC.ReturnLF then
      begin
      FCh := #10;
      TC.ReturnLF := false;
      exit;
      end;
    try
    with TC do
      begin
      if SavedFCh <> #0
        then FCh := SavedFCh
        else Result := GetNextChar(FCh);
      SavedFCh := #0;
      if Result then
        begin
        if FCh = '<' then
          begin
          Tag := '';
          TagBreak := false;
          while Result and (FCh <> '>') do
            begin
            Result := GetNextChar(FCh);
            if (FCh = ' ') or (FCh = '>') then TagBreak := true;
            if (length(Tag) < 255) and (FCh <> '/')
              and not TagBreak then
                begin
                inc(Tag[0]);
                Tag[length(Tag)] := UpCase(FCh);
                end;
            end;
          if
             (Tag = 'P') or
             (Tag = 'TITLE') or
             (Tag = 'BR') or
             (Tag = 'TR') or
             (Tag = 'TD') or
             (Tag = 'HR') or
             (Tag = 'LI') or
             (Tag = 'H1') or
             (Tag = 'H2') or
             (Tag = 'H3') or
             (Tag = 'H4') or
             (Tag = 'H5') or
             (Tag = 'H6') or
             (Tag = 'DT') or
             (Tag = 'DD')
            then
              begin
              FCh := #13;
              ReturnLF := true;
              exit;
              end
            else
              begin
              FCh := #0;
              exit;
              end;
          end;

        if FCh = '&' then
          begin
          Tag := '';
          TagBreak := false;
          while Result and (FCh <> ';') do
            begin
            Result := GetNextChar(FCh);
            if FCh = ';' then TagBreak := true;
            if (length(Tag) < 255) and not TagBreak then
                begin
                inc(Tag[0]);
                Tag[length(Tag)] := UpCase(FCh);
                end;
            end;
          FCh := ' ';
          if (Tag = 'LT') then FCh := '<';
          if (Tag = 'GT') then FCh := '>';
          if (Tag = 'AMP') then FCh := '&';
          if (Tag = 'QUOT') then FCh := '"';
          if (Tag = 'NBSP') then FCh := ' ';
          end;

        if FCh = #13 then FCh := ' ';
        if FCh < ' ' then FCh := #0;
        end;
      end;
    except
      on E: EInOutError do
        begin
        Result := false;
        exit;
        end;
      end;
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
      OK := GetNextFilteredChar(Ch);
      except
        on E: EInOutError do
          begin
          Result := -1;
          exit;
          end;
        end;
      if OK and (Ch <> #0) then
        begin
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
    TC.InputFile.Done;
    TC.Free;
  except
    on Exception do exit;
    end;
  Result := 0;
  end;

{--------------------------------------------------------------------}

end.
