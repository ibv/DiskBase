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

uses WinTypes, WinProcs, SysUtils;

const
  ReadBufSize = 8*1024;

  KamToWin : array[#0..#255] of char = ( {Kam to Win}
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,
  #16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,
  #32,#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,#47,
  #48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,#61,#62,#63,
  #64,#65,#66,#67,#68,#69,#70,#71,#72,#73,#74,#75,#76,#77,#78,#79,
  #80,#81,#82,#83,#84,#85,#86,#87,#88,#89,#90,#91,#92,#93,#94,#95,
  #96,#97,#98,#99,#100,#101,#102,#103,#104,#105,#106,#107,#108,#109,#110,#111,
  #112,#113,#114,#115,#116,#117,#118,#119,#120,#121,#122,#123,#124,#125,#126,#32,
  #200,#252,#233,#239,#228,#207,#141,#232,#236,#204,#197,#205,#190,#229,#196,#193,
  #201,#158,#142,#244,#246,#211,#249,#218,#253,#214,#220,#138,#188,#221,#216,#157,
  #225,#237,#243,#250,#242,#210,#218,#212,#154,#248,#224,#192,#32,#167,#187,#171,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#32,#32,#32,#32,#32,#181,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#177,#32,#32,#32,#32,#247,#32,#176,#32,#32,#32,#32,#32,#32,#32
  );


  LatToWin : array[#0..#255] of char = (
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,
  #16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,
  #32,#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,#47,
  #48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,#61,#62,#63,
  #64,#65,#66,#67,#68,#69,#70,#71,#72,#73,#74,#75,#76,#77,#78,#79,
  #80,#81,#82,#83,#84,#85,#86,#87,#88,#89,#90,#91,#92,#93,#94,#95,
  #96,#97,#98,#99,#100,#101,#102,#103,#104,#105,#106,#107,#108,#109,#110,#111,
  #112,#113,#114,#115,#116,#117,#118,#119,#120,#121,#122,#123,#124,#125,#126,#32,
  #199,#252,#233,#226,#228,#249,#230,#231,#179,#235,#213,#245,#238,#143,#196,#198,
  #201,#197,#229,#244,#246,#188,#190,#140,#156,#214,#220,#141,#157,#163,#215,#232,
  #225,#237,#243,#250,#165,#185,#142,#158,#202,#234,#32,#159,#200,#186,#187,#171,
  #32,#32,#32,#32,#32,#193,#194,#204,#170,#32,#32,#32,#32,#175,#191,#32,
  #32,#32,#32,#32,#32,#32,#195,#227,#32,#32,#32,#32,#32,#32,#32,#164,
  #240,#208,#207,#203,#239,#210,#205,#206,#236,#32,#32,#32,#32,#222,#217,#32,
  #211,#223,#212,#209,#241,#242,#138,#154,#192,#218,#224,#219,#253,#221,#254,#180,
  #32,#189,#178,#161,#162,#167,#247,#184,#32,#168,#183,#251,#216,#248,#32,#32
  );

  MacToWin: array[#0..#255] of char = (
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,
  #16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,
  #32,#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,#47,
  #48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,#61,#62,#63,
  #64,#65,#66,#67,#68,#69,#70,#71,#72,#73,#74,#75,#76,#77,#78,#79,
  #80,#81,#82,#83,#84,#85,#86,#87,#88,#89,#90,#91,#92,#93,#94,#95,
  #96,#97,#98,#99,#100,#101,#102,#103,#104,#105,#106,#107,#108,#109,#110,#111,
  #112,#113,#114,#115,#116,#117,#118,#119,#120,#121,#122,#123,#124,#125,#126,#32,
  #196,#65,#97,#201,#165,#214,#220,#225,#185,#200,#228,#232,#198,#230,#233,#143,
  #159,#207,#237,#239,#69,#101,#69,#243,#101,#244,#246,#111,#250,#204,#236,#252,
  #134,#176,#202,#32,#167,#149,#182,#223,#174,#169,#153,#234,#168,#32,#103,#73,
  #105,#73,#32,#32,#105,#75,#32,#32,#179,#76,#108,#188,#190,#197,#229,#78,
  #110,#209,#172,#32,#241,#210,#32,#32,#32,#133,#160,#242,#213,#79,#245,#79,
  #150,#151,#147,#148,#145,#146,#247,#32,#111,#192,#224,#216,#139,#155,#248,#82,
  #114,#138,#130,#132,#154,#140,#156,#193,#141,#157,#205,#142,#158,#85,#211,#212,
  #117,#217,#218,#249,#219,#251,#85,#117,#221,#253,#107,#175,#163,#191,#71,#161
  );

  UnixToWin : array[#0..#255] of char = (
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,#12,#13,#14,#15,
  #16,#17,#18,#19,#20,#21,#22,#23,#24,#25,#26,#27,#28,#29,#30,#31,
  #32,#33,#34,#35,#36,#37,#38,#39,#40,#41,#42,#43,#44,#45,#46,#47,
  #48,#49,#50,#51,#52,#53,#54,#55,#56,#57,#58,#59,#60,#61,#62,#63,
  #64,#65,#66,#67,#68,#69,#70,#71,#72,#73,#74,#75,#76,#77,#78,#79,
  #80,#81,#82,#83,#84,#85,#86,#87,#88,#89,#90,#91,#92,#93,#94,#95,
  #96,#97,#98,#99,#100,#101,#102,#103,#104,#105,#106,#107,#108,#109,#110,#111,
  #112,#113,#114,#115,#116,#117,#118,#119,#120,#121,#122,#123,#124,#125,#126,#32,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,#32,
  #160,#165,#162,#163,#164,#188,#140,#167,#168,#138,#170,#141,#143,#173,#142,#175,
  #176,#185,#178,#179,#180,#190,#156,#161,#184,#154,#186,#157,#159,#189,#158,#191,
  #192,#193,#194,#195,#196,#197,#198,#199,#200,#201,#202,#203,#204,#205,#206,#207,
  #208,#209,#210,#211,#212,#213,#214,#215,#216,#217,#218,#219,#220,#221,#222,#223,
  #224,#225,#226,#227,#228,#229,#230,#231,#232,#233,#234,#235,#236,#237,#238,#239,
  #240,#241,#242,#243,#244,#245,#246,#247,#248,#249,#250,#251,#252,#253,#254,#183
  );

type

  TTransferClass = class
    ReadBuffer    : array [0..ReadBufSize-1] of char;
    ReadBufPos    : longint;
    WasRead       : longint;
    hInputFile    : integer;
    OneWordBuffer : ShortString;
    LastReadChar  : integer;
    FileFormat    : (unknown, WinWord, T602, RTF, HTML);
    {RTF}
    Level         : integer;
    ReturnLF      : boolean;
    SavedFCh      : char;
    {T602}
    OneLineBuffer : ShortString;
    CurrentBufPos : integer;
    FileEncoding  : (NoIdea, Kamen, Latin);
    Eof           : boolean;
    LastWasEmpty  : boolean;
    ConvArray     : array[#0..#255] of char;
    end;

{--------------------------------------------------------------------}

procedure ChangeCharToChar(var S: ShortString; T602Char: char; HTMLChar: char);
  var i : integer;
    begin
    i := pos(T602Char, S);
    while i > 0 do
      begin
      S[i] := HTMLChar;
      i := pos(T602Char, S);
      end;
    end;

{--------------------------------------------------------------------}

function OpenTransfer (FileName: PChar; var Handle: longint): longint;

  var
    TC: TTransferClass;
    i : integer;
    TmpS: ShortString;
    ContainsBinary: boolean;

  begin
  Result := -1;
  try
    TC := TTransferClass.Create;
    TC.ReadBufPos  := 0;
    TC.WasRead := 0;
    TC.OneWordBuffer := '';
    Handle := longint(TC);
    TC.hInputFile := CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ,
                                nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if TC.hInputFile = INVALID_HANDLE_VALUE then exit;
    TC.FileFormat := Unknown;
    if not ReadFile(TC.hInputFile, TC.ReadBuffer[0], ReadBufSize, DWORD(TC.WasRead), nil) then exit;
    {T602}
    TC.CurrentBufPos := 1;
    TC.FileEncoding  := NoIdea;
    TC.OneLineBuffer := '';
    TC.Eof := false;
    TC.LastWasEmpty := false;
    for i := 0 to 255 do TC.ConvArray[char(i)] := char(i);
    {RTF}
    TC.Level := 0;
    TC.ReturnLF := false;
    TC.SavedFCh := #0;

    with TC do
      begin
      if
        (ReadBuffer[0] = #208) and
        (ReadBuffer[1] = #207) and
        (ReadBuffer[2] =  #17) and
        (ReadBuffer[3] = #224) and
        (ReadBuffer[4] = #161) and
        (ReadBuffer[5] = #177)
        then FileFormat := WinWord;
      if
        (ReadBuffer[0] = '{') and
        (ReadBuffer[1] = '\') and
        (ReadBuffer[2] = 'r') and
        (ReadBuffer[3] = 't') and
        (ReadBuffer[4] = 'f')
        then FileFormat := RTF;
      if
        (ReadBuffer[0] = '@') and
        (ReadBuffer[1] = 'C') and
        (ReadBuffer[2] = 'T') and
        (ReadBuffer[3] = ' ')
        then
          begin
          FileFormat := T602;
          move(LatToWin, TC.ConvArray, sizeof(TC.ConvArray));
          end;
      if FileFormat = Unknown then
        begin
        move (ReadBuffer[0], TmpS[1], 255);
        if WasRead < 255
          then TmpS[0] := char(WasRead)
          else TmpS[0] := #255;
        ContainsBinary := false;
        for i := 1 to length(TmpS) do
          begin
          if (TmpS[i] < ' ') and (TmpS[i] <> #13)
             and (TmpS[i] <> #10) and (TmpS[i] <> #9) then
               ContainsBinary := true;
          TmpS[i] := UpCase(TmpS[i]);
          end;
        if not ContainsBinary and ((pos('<HTML>', TmpS) > 0)
          or (pos('<HEAD>', TmpS) > 0)) then
          FileFormat := HTML;
        end;
      end;
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
      if Ch = '{' then inc(Level);
      if Ch = '}' then dec(Level);
      inc(ReadBufPos);

      // if the char is 0 - then it may be UNICODE upper char - ignore it.
      if Ch = #0 then
        begin // read again
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
        if Ch = '{' then inc(Level);
        if Ch = '}' then dec(Level);
        inc(ReadBufPos);
        end;
      end;
    Result := true;
    end;


  function GetNextFilteredChar(var FCh: char): boolean;
    const
      MaxWordLenght = 30;
    var
      OK, WordEnd: boolean;
      ItIsInvalid, LastWordWasInvalid: boolean;
      ItIsSpace, LastWasSpace: boolean;
      WordContainsChars: boolean;
      Ch, ConvCh       : Char;
      i, CharsCounter  : integer;

    begin
    try
      WordEnd := true;
      with TC do
        begin
        if LastReadChar >= length(OneWordBuffer) then
          begin
          OK := true;
          WordEnd := false;
          WordContainsChars := false;
          OneWordBuffer[0] := #0;
          LastReadChar := 0;
          CharsCounter := 0;
          LastWordWasInvalid := true;
          while OK and not WordEnd do
            begin
            OK := GetNextChar(Ch);
            if OK then
              begin
              {ConvCh := KamToWin[Ch];}
              ConvCh := Ch;
              if FileFormat = WinWord then
                begin
                if ConvCh = #11 then ConvCh := #13; {Shift-Enter}
                if ConvCh = #12 then ConvCh := #13; {Ctrl-Enter}
                end;
              if ConvCh = #9  then ConvCh := ' ';   {tabelator}
              if ConvCh = #10 then ConvCh := #13;
              ItIsSpace := (ConvCh = #13) or (ConvCh = ' ');
              if (ConvCh < ' ') and not ItIsSpace
                then
                  begin
                  ItIsInvalid := true;
                  ConvCh := ' ';
                  end
                else
                  ItIsInvalid := false;
              if (OneWordBuffer[0] < #255) then
                begin
                inc(OneWordBuffer[0]);
                if not ItIsSpace then inc(CharsCounter);
                OneWordBuffer[length(OneWordBuffer)] := ConvCh;
                end;

              if CharsCounter > MaxWordLenght then
                begin
                ItIsInvalid := true;
                WordContainsChars := false;
                end;

              if ItIsInvalid
                then
                  begin
                  if WordContainsChars and not LastWordWasInvalid and
                    (CharsCounter > 1)
                    then
                      begin
                      WordEnd := true;
                      end
                    else
                      begin
                      WordEnd := false;
                      WordContainsChars := false;
                      OneWordBuffer[0] := #0;
                      LastReadChar := 0;
                      CharsCounter := 0;
                      LastWordWasInvalid := true;
                      end
                  end
                else
                  begin
                  if ItIsSpace and WordContainsChars then
                    begin
                    WordEnd := true;
                    LastWordWasInvalid := false;
                    end;
                  end;
              if not WordContainsChars then
                WordContainsChars := not (ItIsSpace or ItIsInvalid);
              end;
            end;
          {zmena CR na CRLF}
          for i := 1 to length(OneWordBuffer) do
            if OneWordBuffer[i] = #13 then
              OneWordBuffer [i] := #0;
          i := pos (#0#0, OneWordBuffer);
          while i > 0 do
            begin
            OneWordBuffer[i] :=#13;
            OneWordBuffer[succ(i)] :=#10;
            i := pos (#0#0, OneWordBuffer);
            end;
          i := pos (#0, OneWordBuffer);
          while i > 0 do
            begin
            OneWordBuffer[i] :=#13;
            insert(#10, OneWordBuffer, succ(i));
            i := pos (#0, OneWordBuffer);
            end;
          end;

        if WordEnd
          then
            begin
            Result := true;
            inc(LastReadChar);
            FCh := OneWordBuffer[LastReadChar];
            end
          else
            Result := false;
        end;
    except
      on Exception do
        begin
        Result := false;
        exit;
        end;
      end;
    end;



  function GetNextRTFChar(var FCh: char): boolean;

    var
      FInt, SInt: SmallInt;
      Ch        : char;
      FirstDigit, SecondDigit: char;
      ThisLevel : integer;
      Tag       : ShortString;

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
        if FCh = '}' then
          Result := GetNextChar(FCh);
        if FCh = '{' then
          begin
          Result := GetNextChar(FCh);
          while FCh <= ' ' do
            Result := GetNextChar(FCh);
          if FCh = '\' then
            begin
            Result := GetNextChar(FCh);
            Tag := '';
            while Result and ((FCh >= 'a') or (FCh = '*'))
                         and (FCh <> '{') and (FCh <> '\') do
              begin
              Tag := Tag + FCh;
              Result := GetNextChar(FCh);
              end;
            if
              (Tag = '*') or
              (Tag = 'ansi') or
              (Tag = 'bin') or
              (Tag = 'box') or
              (Tag = 'brdrb') or
              (Tag = 'brdrbar') or
              (Tag = 'brdrdb') or
              (Tag = 'brdrdot') or
              (Tag = 'brdrl') or
              (Tag = 'brdrr') or
              (Tag = 'brdrs') or
              (Tag = 'brdrt') or
              (Tag = 'brdrth') or
              (Tag = 'cell') or
              (Tag = 'cellx') or
              (Tag = 'cf') or
              (Tag = 'colortbl') or
              (Tag = 'deff') or
              (Tag = 'fonttbl') or
              (Tag = 'footnote') or
              (Tag = 'fi') or
              (Tag = 'li') or
              (Tag = 'mac') or
              (Tag = 'pc') or
              (Tag = 'pich') or
              (Tag = 'pichgoal') or
              (Tag = 'picscalex') or
              (Tag = 'picscaley') or
              (Tag = 'pict') or
              (Tag = 'picw') or
              (Tag = 'picwgoal') or
              (Tag = 'ri') or
              (Tag = 's') or
              (Tag = 'sl') or
              (Tag = 'strike') or
              (Tag = 'trgaph') or
              (Tag = 'trleft') or
              (Tag = 'trowd') or
              (Tag = 'trqc') or
              (Tag = 'tx') or
              (Tag = 'v') or
              (Tag = 'wbitmap') or
              (Tag = 'wbmbitspixel') or
              (Tag = 'wbmplanes') or
              (Tag = 'wbmwidthbytes') or
              (Tag = 'wmetafile') or
              (Tag = 'operator') or
              (Tag = 'creatim') or
              (Tag = 'revtim')
              then
                 begin
                 ThisLevel := Level;
                 if FCh = '{' then dec(ThisLevel);
                  while Result and (Level >= ThisLevel) do
                    Result := GetNextChar(FCh);
                 FCh := #0;
                 exit;
                 end;

            if (FCh = '\') or (FCh = '{') then SavedFCh := FCh;
            if
              (Tag = 'title') or
              (Tag = 'author') then
                begin
                FCh := #13; {nutno poté doplnit #10}
                ReturnLF := true;
                exit;
                end;

            while Result and (FCh > ' ') and (FCh <> '{') and (FCh <> '\') do
              Result := GetNextChar(FCh);
            if (FCh = '\') or (FCh = '{') then SavedFCh := FCh;
            FCh := #0;
            exit;
            end;
          end;
        if FCh = '\' then
          begin
          Result := GetNextChar(FCh);
          if FCh = ''''
            then
              begin
              Result := GetNextChar(FirstDigit);
              Result := GetNextChar(SecondDigit);
              Ch := UpCase(FirstDigit);
              if Ch >= 'A'
                then FInt := 10 + ord(Ch) - ord('A')
                else FInt := ord(Ch) - ord('0');
              Ch := UpCase(SecondDigit);
              if Ch >= 'A'
                then SInt := 10 + ord(Ch) - ord('A')
                else SInt := ord(Ch) - ord('0');
              FCh := char(FInt * 16 + SInt);
              exit;
              end
            else
              begin
              Tag := '';
              while Result and (FCh > ' ') and (FCh <> '{') and (FCh <> '\') do
                begin
                Tag := Tag + FCh;
                Result := GetNextChar(FCh);
                end;
              if (FCh = '\') or (FCh = '{') then SavedFCh := FCh;
              if
                (Tag = 'line') or
                (Tag = 'page') or
                (Tag = 'par')  then
                  begin
                  FCh := #13; {nutno poté doplnit #10}
                  ReturnLF := true;
                  exit;
                  end;
              FCh := #0;
              exit;
              end;
          end;
        if FCh < ' ' then FCh := #0;
        end;
      end;
    except
      on Exception do 
        begin
        Result := false;
        exit;
        end;
      end;
    end;


  function GetNextT602Char(var Ch: char): boolean;

    var
      XCh: char;
      i: integer;
      Success: boolean;

    begin
    Result := true;
    with TC do
      begin
      if CurrentBufPos > length(OneLineBuffer) then
        begin
        if Eof then
          begin
          Ch := #0;
          Result := false;
          exit;
          end;
        OneLineBuffer := '';
        OneLineBuffer[0] := #0;
        XCh := ' ';
        i := 0;
        Success := true;
        while Success and (XCh <> #$0A) and (i < 255) do
          begin
          Success := GetNextChar(XCh);
          if Success
            then
              begin
              inc(i);
              OneLineBuffer[i] := XCh;
              OneLineBuffer[0] := char(i);
              end
            else
              begin
              Eof := true;
              end;
          end;
        if length(OneLineBuffer) > 0 then dec(OneLineBuffer[0]);
        if length(OneLineBuffer) > 1 then
          if (OneLineBuffer[length(OneLineBuffer)] = #$8D)
           and (OneLineBuffer[length(OneLineBuffer)-1] = #$AD)
            then OneLineBuffer[length(OneLineBuffer)-1] := '-'; {rozdeleni}
        if length(OneLineBuffer) > 0 then dec(OneLineBuffer[0]);
        while (length(OneLineBuffer) > 0)
         and (OneLineBuffer[length(OneLineBuffer)]=' ') do
          dec(OneLineBuffer[0]);

        ChangeCharToChar(OneLineBuffer, 'Ä', '-');
        ChangeCharToChar(OneLineBuffer, '³', '|');
        ChangeCharToChar(OneLineBuffer, 'Å', '+');
        ChangeCharToChar(OneLineBuffer, 'Ã', '|');
        ChangeCharToChar(OneLineBuffer, '´', '|');
        ChangeCharToChar(OneLineBuffer, 'Ú', '-');
        ChangeCharToChar(OneLineBuffer, 'Â', '-');
        ChangeCharToChar(OneLineBuffer, '¿', '-');
        ChangeCharToChar(OneLineBuffer, 'À', '-');
        ChangeCharToChar(OneLineBuffer, 'Á', '-');
        ChangeCharToChar(OneLineBuffer, 'Ù', '-');

        for i := 1 to length(OneLineBuffer) do
          if OneLineBuffer[i] < ' ' then OneLineBuffer[i] := #0;

        OneLineBuffer := OneLineBuffer + #13#10;
        CurrentBufPos := 1;

        if LastWasEmpty and (OnelineBuffer = #13#10) then
          begin
          Ch := #0;
          OneLineBuffer := '';
          exit;
          end;
        LastWasEmpty := OneLineBuffer = #13#10;
        end;

      if copy(OneLineBuffer, 1, 1) = '@'
        then
          begin
          if copy(OneLineBuffer, 1, 5) = '@CT 1' then FileEncoding := Latin;
          if copy(OneLineBuffer, 1, 5) = '@CT 0' then
            begin
            FileEncoding := Kamen;
            move(KamToWin, ConvArray, sizeof(ConvArray));
            end;
          Ch := #0;
          OneLineBuffer[0] := #0;
          CurrentBufPos := 1;
          if Eof then
            begin
            Result := false;
            exit;
            end;
          end
        else
          begin
          Ch := OneLineBuffer[CurrentBufPos];
          inc(CurrentBufPos);
          end;
      end;
    end;


  function GetNextHTMLChar(var FCh: char): boolean;

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
      on Exception do 
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
      if TC.FileFormat = T602
        then
          OK := GetNextT602Char(Ch)
        else
          if TC.FileFormat = RTF
            then OK := GetNextRTFChar(Ch)
            else
              if TC.FileFormat = HTML
                then OK := GetNextHTMLChar(Ch)
                else OK := GetNextFilteredChar(Ch);
      Ch := TC.ConvArray[Ch];
      except
        on Exception do 
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
    if TC.hInputFile <> INVALID_HANDLE_VALUE then CloseHandle(TC.hInputFile);
    TC.Free;
  except
    on Exception do exit;
    end;
  Result := 0;
  end;

{--------------------------------------------------------------------}

end.
