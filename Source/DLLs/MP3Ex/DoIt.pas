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
  BufSize = 8*1024;

type
  THeader = array[1..4] of char;

  TTransferClass = class
    ReadBuffer1   : array [1..4] of char;
    ReadBuffer2   : array [1..128] of char;
    WriteBuffer   : array [0..BufSize-1] of char;
    WriteBufPos   : longint;
    WasWritten    : longint;
    ///hInputFile    : integer;
    InputFile     : TQBufStream;
    end;

type
  TBitrateTable = array [0..1, 0..2, 0..14] of integer;
  TFreqTable    = array [0..3, 0..2] of integer;
  TGenreTable   = array [0..125] of String[25];

const
  BitrateTable: TBitrateTable =
    (
     //MPEG 1
        ((0, 32, 64, 96,128,160,192,224,256,288,320,352,384,416,448), //Layer I
         (0, 32, 48, 56, 64, 80, 96,112,128,160,192,224,256,320,384), //Layer II
         (0, 32, 40, 48, 56, 64, 80, 96,112,128,160,192,224,256,320)),  //Layer III
     //MPEG 2 & 2.5
       ((0, 32, 48, 56, 64, 80, 96,112,128,144,160,176,192,224,256), //Layer I
        (0,  8, 16, 24, 32, 40, 48, 56, 64, 80, 96,112,128,144,160), //Layer II
        (0,  8, 16, 24, 32, 40, 48, 56, 64, 80, 96,112,128,144,160))  //Layer III
    );

  FreqTable : TFreqTable =
   ((32000, 16000,  8000),  //MPEG 2.5
    (    0,     0,     0),
    (22050, 24000, 16000),  //MPEG 2
    (44100, 48000, 32000)); //MPEG 1


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

{$ifdef ENGLISH}

c_sTitle  = 'Title: ';
c_sArtist = 'Artist: ';
c_sAlbum  = 'Album: ';
c_sYear   = 'Year: ';
c_sComment = 'Comment: ';
c_sGenre   = 'Genre: ';

c_sMpeg25 = 'MPEG 2.5, ';
c_sMpeg2  = 'MPEG Version 2 (ISO/IEC 13818-3), ';
c_sMpeg1  = 'MPEG Version 1 (ISO/IEC 11172-3), ';
c_sProtected = 'Protected by CRC.';
c_sBitrate   = 'Bitrate: ';
c_sSampling  = 'Sampling frequency: ';

c_sStereo        = 'Channel mode: Stereo';
c_sJointStereo   = 'Channel mode: Joint stereo';
c_sDualChannel   = 'Channel mode: Dual channel';
c_sSingleChannel = 'Channel mode: Single channel';

c_sCopyrighted   = 'Audio is copyrighted.';

{$else}

c_sTitle  = 'Titul: ';
c_sArtist = 'Umělec: ';
c_sAlbum  = 'Album: ';
c_sYear   = 'Rok: ';
c_sComment = 'Poznámka: ';
c_sGenre   = 'Styl: ';

c_sMpeg25 = 'MPEG 2.5, ';
c_sMpeg2  = 'MPEG Verze 2 (ISO/IEC 13818-3), ';
c_sMpeg1  = 'MPEG Verze 1 (ISO/IEC 11172-3), ';
c_sProtected = 'Chráněno pomocí CRC.';
c_sBitrate   = 'Datový tok: ';
c_sSampling  = 'Vzorkovací frekvence: ';

c_sStereo        = 'Kanálový režim: Stereo';
c_sJointStereo   = 'Kanálový režim: Joint stereo';
c_sDualChannel   = 'Kanálový režim: Dual channel';
c_sSingleChannel = 'Kanálový režim: Single channel';

c_sCopyrighted   = 'Nahrávka je chráněna copyrightem.';

{$endif}

{--------------------------------------------------------------------}

 function ExtractBits(lHeader: longint; Position: integer; Length: integer): longint;
 // position of the MSB - max is 31, min is 0

   begin
   lHeader := lHeader shl (31-Position); // zero bits right
   lHeader := lHeader shr (31-Position);
   Result  := lHeader shr (Position-Length+1);
   end;

{--------------------------------------------------------------------}

procedure AddLine(var TC: TTransferClass; S: ShortString);

  var i: integer;
    begin
    S := S + #13#10;
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
  //if sTitle <> '' then
    AddLine(TC, c_sTitle + sTitle);

  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[34], Buf, 30);
  sArtist := StrPas(Buf);
  //if sArtist <> '' then
    AddLine(TC, c_sArtist + sArtist);

  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[64], Buf, 30);
  sAlbum := StrPas(Buf);
  //if sAlbum <> '' then
    AddLine(TC, c_sAlbum + sAlbum);

  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[94], Buf, 4);
  sYear := StrPas(Buf);
  //if sYear <> '' then
    AddLine(TC, c_sYear + sYear);

  fillchar(Buf, sizeof(Buf), 0);
  move(TC.ReadBuffer2[98], Buf, 29);
  sComment := StrPas(Buf);
  //if sComment <> '' then
    AddLine(TC, c_sComment + sComment);

  Genre := byte(TC.ReadBuffer2[128]);
  if Genre <= 125 then
    AddLine(TC, c_sGenre + GenreTable[Genre])
  else
    AddLine(TC, c_sGenre);

  AddLine(TC, '');
  Result := true;
  end;

{--------------------------------------------------------------------}

function GetMpegData  (Handle: longint): boolean;

  var
    TC: TTransferClass absolute Handle;

    Header : THeader;
    lHeader: longint absolute Header;

    lFrameSync           : longint;
    lMpegAudioVersionId  : longint;
    lLayerDescription    : longint;
    lProtectionBit       : longint;
    lBitrateIndex        : longint;
    lSamplingFreqIndex   : longint;
    lChannelMode         : longint;
    lCopyright           : longint;
    lOriginal            : longint;
    lEmphasis            : longint;

    iVersionIndex        : integer;
    iLayerIndex          : integer;
    S                    : ShortString;

  begin
  Result := false;
  try
    Header[1] := TC.ReadBuffer1[4];
    Header[2] := TC.ReadBuffer1[3];
    Header[3] := TC.ReadBuffer1[2];
    Header[4] := TC.ReadBuffer1[1];
    lFrameSync := ExtractBits(lHeader, 31, 11);
    if lFrameSync = $7FF then
      begin
      lMpegAudioVersionId  := ExtractBits(lHeader, 20,  2);
      case lMpegAudioVersionId of
        0: S := c_sMpeg25;
        1: S := '';
        2: S := c_sMpeg2;
        3: S := c_sMpeg1;
        end;

      lLayerDescription    := ExtractBits(lHeader, 18,  2);
      case lLayerDescription of
        0: S := S + '';
        1: S := S + 'Layer III';
        2: S := S + 'Layer II';
        3: S := S + 'Layer I';
        end;
      Addline (TC, S);

      lProtectionBit := ExtractBits(lHeader, 16,  1);
      if lProtectionBit = 0 then AddLine(TC, c_sProtected);

      if lMpegAudioVersionId = 3
        then iVersionIndex := 0  // MPEG 1
        else iVersionIndex := 1; // MPEG 2 and 2.5
      if lLayerDescription > 0
        then iLayerIndex := 3 - lLayerDescription
        else iLayerIndex := 0; // dummy
      lBitrateIndex := ExtractBits(lHeader, 15,  4);
      AddLine (TC, c_sBitrate + IntToStr(BitRateTable[iVersionIndex, iLayerIndex, lBitrateIndex]) + ' kbit/sec');

      lSamplingFreqIndex   := ExtractBits(lHeader, 11,  2);

      AddLine (TC, c_sSampling + IntToStr(FreqTable[lMpegAudioVersionId, lSamplingFreqIndex]) + ' Hz');

      lChannelMode         := ExtractBits(lHeader,  7,  2);
      case lLayerDescription of
        0: AddLine (TC, c_sStereo        );
        1: AddLine (TC, c_sJointStereo   );
        2: AddLine (TC, c_sDualChannel   );
        3: AddLine (TC, c_sSingleChannel );
        end;

      lCopyright := ExtractBits(lHeader,  3,  1);
      if lCopyright = 1 then AddLine(TC, c_sCopyrighted);
      end;
  except
    on Exception do
      exit;
    end;
  Result := true;
  end;

{--------------------------------------------------------------------}

function OpenTransfer (FileName: PChar; var Handle: longint): longint;
(*
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
                                  nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
                                  FILE_FLAG_RANDOM_ACCESS, 0);
    if TC.hInputFile = INVALID_HANDLE_VALUE then
      begin
      TC.Free; // if this function returns nonzero value, CloseTransfer is not called
      exit;
      end;
    lSize := FileSize(TC.hInputFile);{ *Převedeno z GetFileSize* }
    if (lSize) < 132 then // too small for mpeg file
      begin
      FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
      TC.Free;
      exit;
      end;
    if not ReadFile(TC.hInputFile, TC.ReadBuffer1[1], 4, dwWasRead, nil) then
      begin
      FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
      TC.Free;
      exit;
      end;
    SetFilePointer(TC.hInputFile, -128, nil, FILE_END);
    if not ReadFile(TC.hInputFile, TC.ReadBuffer2[1], 128, dwWasRead, nil) then
      begin
      FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
      TC.Free;
      exit;
      end;
  except
    on Exception do
      begin
      if TC.hInputFile <> INVALID_HANDLE_VALUE then FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
      TC.Free;
      exit;
      end;
    end;
  SuccessTag := GetMpegTag(Handle);
  SuccessData := GetMpegData(Handle);
  if not (SuccessTag or SuccessData) then
    begin
    FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
    TC.hInputFile := INVALID_HANDLE_VALUE;
    exit;
    end;
  Result := 0;
*)

    var
      TC           : TTransferClass;
      dwWasRead    : longint;{DWORD;}
      lSize        : longint;
      SuccessTag   : boolean;
      SuccessData  : boolean;

    begin
    Result := -1;
    try

      //TC.InputFile.ReadExt(TC.ReadBuffer[0], ReadBufSize, TC.WasRead);
      dwWasRead := 0;
      TC := TTransferClass.Create;
      TC.InputFile.Init(FileName, stOpenReadNonExclusive);
      TC.WriteBufPos  := 0;
      TC.WasWritten   := 0;
      Handle          := longint(TC);
      ///TC.hInputFile   := CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ,
      ///                              nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
      ///                              FILE_FLAG_RANDOM_ACCESS, 0);
      ///if TC.hInputFile = INVALID_HANDLE_VALUE then
      if TC.InputFile.Stream =nil then
        begin
        TC.Free; // if this function returns nonzero value, CloseTransfer is not called
        exit;
        end;
      ///lSize := FileSize(TC.hInputFile);{ *Převedeno z GetFileSize* }
      lsize := TC.InputFile.Getsize;
      if (lSize) < 132 then // too small for mpeg file
        begin
        ///FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
        TC.Free;
        exit;
        end;
      ///if not ReadFile(TC.hInputFile, TC.ReadBuffer1[1], 4, dwWasRead, nil) then
        TC.InputFile.ReadExt(TC.ReadBuffer1[1], 4, dwWasRead);
        if dwWasRead <> 4 then
        begin
        ///FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
        TC.Free;
        exit;
        end;
      ///SetFilePointer(TC.hInputFile, -128, nil, FILE_END);
      TC.InputFile.Seek(lsize-128);
      ///if not ReadFile(TC.hInputFile, TC.ReadBuffer2[1], 128, dwWasRead, nil) then
      TC.InputFile.ReadExt(TC.ReadBuffer2[1], 128, dwWasRead);
      if dwWasRead <> 128 then
        begin
        ///FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
        TC.Free;
        exit;
        end;
    except
      on Exception do
        begin
        ///if TC.hInputFile <> INVALID_HANDLE_VALUE then FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
        TC.Free;
        exit;
        end;
      end;
    SuccessTag := GetMpegTag(Handle);
    SuccessData := GetMpegData(Handle);
    if not (SuccessTag or SuccessData) then
      begin
      ///FileClose(TC.hInputFile);{ *Převedeno z CloseHandle* }
      ///TC.hInputFile := INVALID_HANDLE_VALUE;
      TC.free;
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
    TC.InputFile.Done;
    TC.Free;
  except
    on Exception do exit;
    end;
  Result := 0;
  end;

{--------------------------------------------------------------------}

end.
