unit DoIt;

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

uses
  WinTypes, WinProcs, SysUtils;

const
  BufSize = 8*1024;

type
  THeader = array[1..4] of char;

  TTransferClass = class
    ReadBuffer2   : array [1..128] of char;
    WriteBuffer   : array [0..BufSize-1] of char;
    WriteBufPos   : longint;
    WasWritten    : longint;
    hInputFile    : integer;
    end;

type
  TGenreTable   = array [0..125] of String[25];

const

GenreTable: TGenreTable = (
'Blues','Classic Rock','Country','Dance','Disco','Funk','Grunge',
'Hip-Hop','Jazz','Metal','New Age','Oldies','Other','Pop','R&B',
'Rap','Reggae','Rock','Techno','Industrial','Alternative','Ska',
'Death Metal','Pranks','Soundtrack','Euro-Techno','Ambient',
'Trip-Hop','Vocal','Jazz+Funk','Fusion','Trance','Classical',
'Instrumental','Acid','House','Game','Sound Clip','Gospel',
'Noise','AlternRock','Bass','Soul','Punk','Space','Meditative',
'Instrumental Pop','Instrumental Rock','Ethnic','Gothic',
'Darkwave','Techno-Industrial','Electronic','Pop-Folk',
'Eurodance','Dream','Southern Rock','Comedy','Cult','Gangsta',
'Top 40','Christian Rap','Pop/Funk','Jungle','Native American',
'Cabaret','New Wave','Psychadelic','Rave','Showtunes','Trailer',
'Lo-Fi','Tribal','Acid Punk','Acid Jazz','Polka','Retro',
'Musical','Rock & Roll','Hard Rock','Folk','Folk-Rock',
'National Folk','Swing','Fast Fusion','Bebob','Latin','Revival',
'Celtic','Bluegrass','Avantgarde','Gothic Rock','Progressive Rock',
'Psychedelic Rock','Symphonic Rock','Slow Rock','Big Band',
'Chorus','Easy Listening','Acoustic','Humour','Speech','Chanson',
'Opera','Chamber Music','Sonata','Symphony','Booty Bass','Primus',
'Porn Groove','Satire','Slow Jam','Club','Tango','Samba',
'Folklore','Ballad','Power Ballad','Rhythmic Soul','Freestyle',
'Duet','Punk Rock','Drum Solo','Acapella','Euro-House','Dance Hall');


{--------------------------------------------------------------------}

procedure AddLine(var TC: TTransferClass; S: ShortString);

  var i: integer;
    begin
    //S := S + #13#10;
    for i := 1 to length(S) do
      if (TC.WasWritten < BufSize) then
        begin
        TC.WriteBuffer[TC.WasWritten] := S[i];
        inc(TC.WasWritten);
        end;
    end;

{--------------------------------------------------------------------}

function GetMpegTag (Handle: longint): boolean;

  var
    TC: TTransferClass absolute Handle;
    sTitle  : ShortString;
    sArtist : ShortString;
    sAlbum  : ShortString;
    sTag    : ShortString;
    sYear   : String[4];
    sComment: ShortString;
    Genre   : byte;
    Buf     : array[0..128] of char;

  begin
  Result := false;
  if (TC.ReadBuffer2[1] <> 'T') or
     (TC.ReadBuffer2[2] <> 'A') or
     (TC.ReadBuffer2[3] <> 'G') then
    exit;
  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[4], Buf, 30);
  sTitle := StrPas(Buf);
  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[34], Buf, 30);
  sArtist := StrPas(Buf);

  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[64], Buf, 30);
  sAlbum := StrPas(Buf);

  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[94], Buf, 4);
  sYear := StrPas(Buf);
  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[98], Buf, 29);
  sComment := StrPas(Buf);

  sTag := '';
  //if sTitle <> '' then
    sTag := sTag + sTitle;
  //if sArtist <> '' then
    begin
    if sTag <> '' then sTag := sTag + ' - ';
    sTag := sTag + sArtist;
    end;

  //if sAlbum <> '' then
    begin
    if sTag <> '' then sTag := sTag + ' - ';
    sTag := sTag + sAlbum;
    end;

  //if sYear <> '' then
    begin
    if sTag <> '' then sTag := sTag + ' - ';
    sTag := sTag + sYear;
    end;

  Genre := byte(TC.ReadBuffer2[128]);
  if Genre <= 125 then
    begin
    if sTag <> '' then sTag := sTag + ' - ';
    sTag := sTag + GenreTable[Genre];
    end
  else
    begin
    if sTag <> '' then sTag := sTag + ' - ';
    end;

  //if sComment <> '' then
    begin
    if sTag <> '' then sTag := sTag + ' - ';
    sTag := sTag + sComment;
    end;

  //if sTag <> '' then
    begin
    AddLine(TC, sTag);
    Result := true;
    end;
  end;

{--------------------------------------------------------------------}

function OpenTransfer (FileName: PChar; var Handle: longint): longint;

  var
    TC           : TTransferClass;
    dwWasRead    : DWORD;
    lSize        : longint;
    SuccessTag   : boolean;
    SuccessData  : boolean;

  begin
  Result := -1;
  try
    dwWasRead := 0;
    TC := TTransferClass.Create;
    TC.WriteBufPos  := 0;
    TC.WasWritten   := 0;
    Handle          := longint(TC);
    TC.hInputFile   := CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ,
                                  nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if TC.hInputFile = INVALID_HANDLE_VALUE then
      begin
      TC.Free; // if this function returns nonzero value, CloseTransfer is not called
      exit;
      end;
    lSize := GetFileSize(TC.hInputFile, nil);
    if (lSize) < 132 then // too small for mpeg file
      begin
      CloseHandle(TC.hInputFile);
      TC.Free;
      exit;
      end;
    SetFilePointer(TC.hInputFile, -128, nil, FILE_END);
    if not ReadFile(TC.hInputFile, TC.ReadBuffer2[1], 128, dwWasRead, nil) then
      begin
      CloseHandle(TC.hInputFile);
      TC.Free;
      exit;
      end;
  except
    on Exception do
      begin
      if TC.hInputFile <> INVALID_HANDLE_VALUE then CloseHandle(TC.hInputFile);
      TC.Free;
      exit;
      end;
    end;
  SuccessTag := GetMpegTag(Handle);
  if not (SuccessTag or SuccessData) then
    begin
    CloseHandle(TC.hInputFile);
    TC.hInputFile := INVALID_HANDLE_VALUE;
    exit;
    end;
  Result := 0;
  end;

{--------------------------------------------------------------------}

function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint;

  var
    TC: TTransferClass absolute Handle;

  var
    MaxLen : integer;

  begin
  CharsRead := 0;
  Result := -1;
  MaxLen := pred(BufSize);
  if MaxLen > (TC.WasWritten - TC.WriteBufPos) then
    MaxLen := (TC.WasWritten - TC.WriteBufPos);
  while (CharsRead < MaxLen) do
    begin
    Buf[CharsRead] := TC.WriteBuffer[TC.WriteBufPos];
    inc(TC.WriteBufPos);
    inc(CharsRead);
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
