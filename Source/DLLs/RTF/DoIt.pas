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

{
\line	Breaks the current line
\page	Ends current topic
\par	Marks the end of a paragraph


\ansi	Specifies the ANSI character set
\bin	Specifies binary picture data
\box	Draws a box
\brdrb	Draws a bottom border
\brdrbar	Draws a vertical bar
\brdrdb	Sets double-lined borders
\brdrdot	Sets dotted border
\brdrl	Draws a left border
\brdrr	Draws a right border
\brdrs	Sets standard borders
\brdrt	Draws a top border
\brdrth	Sets thick borders
\cell	Marks end of table cell
\cellx	Sets the position of a cell's right edge
\cf	Sets the foreground color
\colortbl	Creates the color table
\deff	Sets default font
\fonttbl	Creates the font table
\footnote	Defines topic-specific information
\fi	Sets the first-line indent
\li	Sets the left indent
\mac	Sets the Apple MacIntosh character set
\pc	Sets the PC character set
\pich	Specifies the picture height
\pichgoal	Specifies the desired picture height
\picscalex	Specifies the horizontal scaling value
\picscaley	Specifies the vertical scaling value
\pict	Creates a picture
\picw	Specifies the picture width
\picwgoal	Specifies the desired picture width
\ri	Sets the right indent
\sl	Sets the spacing between lines
\strike	Creates a hotspot
\trgaph	Sets space between text columns in a table
\trleft	Sets left margin for the first cell
\trowd	Sets table defaults
\trqc	Sets relative column widths
\tx	Sets a tab stop
\ul	Creates a link to a pop-up topic
\uldb	Creates a hot spot
\v	Creates a link to a topic
\wbitmap	Specifies a Windows bitmap
\wbmbitspixel	Specifies the number of bits per pixel
\wbmplanes	Specifies the number of planes
\wbmwidthbytes	Specifies the bitmap width in bytes
\wmetafile	Specifies a Windows metafile


\b	Starts bold text
\f	Sets the font
\fldrslt	Result of a field
\fs	Sets the font size
\'	Inserts a character by value
\i	Starts italic text
\intbl	Marks paragraph as in table
\keep	Makes text non-wrapping
\keepn	Creates a non-scrolling region
\pard	Restores default paragraph properties
\plain	Restores default character properties
\qc	Centers text
\ql	Aligns text left
\qr	Aligns text right
\row	Marks end of a table row
\rtf	Specifies the RTF version
\sa	Sets the spacing after a paragraph
\sb	Sets space before
\scaps	Starts small capitals
\sect	Marks the end of a section and paragraph
\tab	Inserts a tab character
\tqc	Tabs and centers text
\tqr	Tabs and aligns text right
\trql	Left-aligns table row
}

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

    Level         : integer;
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
    TC.Level := 0;
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
        InputFile.ReadExt(ReadBuffer[0], ReadBufSize, WasRead);
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
    Result := true;
    end;


  function GetNextFilteredChar(var FCh: char): boolean;

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
