unit UFont;
(*====================================================================
Workaround for Delphi 2.0 inability to work with font scripts
======================================================================*)

interface

uses
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Graphics;

type
  TLogFontStyle = (lfsNormal, lfsBold, lfsItalic);

  function  QGetScriptString (CharSet: byte): ShortString;
  function  QGetScriptNumber (ScriptString: ShortString): byte;
  function  QSelectLogFontDialog (var LogFont: TLogFont; MinSize, MaxSize: longint;
                                 ForPrinter: boolean; var LogFontSize: Integer): boolean;
  function  QHeightFromSize(Size: integer): longint;
  procedure InitLogFontStruct (var LogFont: TLogFont; Name: ShortString;
                               Size: integer; Bold: boolean;
                               Italics: boolean; Script: ShortString);
  procedure SetFontFromLogFont(Font: TFont; var LogFont: TLogFont);

//-----------------------------------------------------------

implementation

  uses Forms, {CommDlg,} SysUtils{, Printers};

//-----------------------------------------------------------


(*
  TChooseFont = packed record
    lStructSize: DWORD;
    hWndOwner: HWnd;            { caller's window handle }
    hDC: HDC;                   { printer DC/IC or nil }
    lpLogFont: PLogFontA;       { pointer to a LOGFONT struct }
    iPointSize: Integer;        { 10 * size in points of selected font }
    Flags: DWORD;               { dialog flags }
    rgbColors: COLORREF;        { returned text color }
    lCustData: LPARAM;          { data passed to hook function }
    lpfnHook: function(Wnd: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
                                { pointer to hook function }
    lpTemplateName: PAnsiChar;  { custom template name }
    hInstance: HINST;           { instance handle of EXE that contains
                                  custom dialog template }
    lpszStyle: PAnsiChar;       { return the style field here
                                  must be lf_FaceSize or bigger }
    nFontType: Word;            { same value reported to the EnumFonts
                                  call back with the extra fonttype_
                                  bits added }
    wReserved: Word;
    nSizeMin: Integer;          { minimum point size allowed and }
    nSizeMax: Integer;          { maximum point size allowed if
                                  cf_LimitSize is used }
  end;

//-----------------------------------------------------------

  TLogFont = packed record
    lfHeight: Longint;
    lfWidth: Longint;
    lfEscapement: Longint;
    lfOrientation: Longint;
    lfWeight: Longint;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;
*)

//-----------------------------------------------------------

function QTaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
  type
    TDialogFunc = function(var DialogData): Bool stdcall;
  var
    ActiveWindow: HWnd;
    WindowList: Pointer;

  begin
  ActiveWindow := GetActiveWindow;
  ///WindowList := DisableTaskWindows(0);
  try
    Result := TDialogFunc(DialogFunc)(DialogData);
  finally
    ///EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    end;
  end;

//-----------------------------------------------------------

function QGetScriptString (CharSet: byte): ShortString;

  begin
  case CharSet of
    ANSI_CHARSET       : Result := 'Ansi';
    DEFAULT_CHARSET    : Result := 'Default';
    SYMBOL_CHARSET     : Result := 'Symbol';
    SHIFTJIS_CHARSET   : Result := 'ShiftJis';
    HANGEUL_CHARSET    : Result := 'Hangeul';
    GB2312_CHARSET     : Result := 'GB2312';
    CHINESEBIG5_CHARSET: Result := 'ChineseBig5';
    OEM_CHARSET        : Result := 'OEM';
    JOHAB_CHARSET      : Result := 'Johab';
    HEBREW_CHARSET     : Result := 'Hebrew';
    ARABIC_CHARSET     : Result := 'Arabic';
    GREEK_CHARSET      : Result := 'Greek';
    TURKISH_CHARSET    : Result := 'Turkish';
    THAI_CHARSET       : Result := 'Thai';
    EASTEUROPE_CHARSET : Result := 'CentralEurope';
    RUSSIAN_CHARSET    : Result := 'Russian';
    MAC_CHARSET        : Result := 'Mac';
    BALTIC_CHARSET     : Result := 'Baltic';
    end;
  end;

//-----------------------------------------------------------

function QGetScriptNumber (ScriptString: ShortString): byte;

  begin
  if AnsiLowerCase(ScriptString) = 'centraleurope' then
    begin Result := EASTEUROPE_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'ansi'          then
    begin Result := ANSI_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'default'       then
    begin Result := DEFAULT_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'symbol'        then
    begin Result := SYMBOL_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'shiftjis'      then
    begin Result := SHIFTJIS_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'hangeul'       then
    begin Result := HANGEUL_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'gb2312'        then
    begin Result := GB2312_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'chinesebig5'   then
    begin Result := CHINESEBIG5_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'oem'           then
    begin Result := OEM_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'johab'         then
    begin Result := JOHAB_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'hebrew'        then
    begin Result := HEBREW_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'arabic'        then
    begin Result := ARABIC_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'greek'         then
    begin Result := GREEK_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'turkish'       then
    begin Result := TURKISH_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'thai'          then
    begin Result := THAI_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'russian'       then
    begin Result := RUSSIAN_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'mac'           then
    begin Result := MAC_CHARSET; exit; end;
  if AnsiLowerCase(ScriptString) = 'baltic'        then
    begin Result := BALTIC_CHARSET; exit; end;
  Result := DEFAULT_CHARSET;
  end;

//-----------------------------------------------------------

function QHeightFromSize(Size: integer): longint;
  var
    DC: HDC;
  begin
  DC := GetDC(0);
  Result := -MulDiv(Size, GetDeviceCaps(DC, LOGPIXELSY), 72);
  ReleaseDC(0,DC);
  end;

//-----------------------------------------------------------

function  QSizeFromHeight(Height: longint): integer;
  var
    DC: HDC;
  begin
  DC := GetDC(0);
  Result := -MulDiv(Height, 72, GetDeviceCaps(DC, LOGPIXELSY));
  ReleaseDC(0,DC);
  end;

//-----------------------------------------------------------

function QSelectLogFontDialog (var LogFont: TLogFont; MinSize, MaxSize: longint;
                               ForPrinter: boolean; var LogFontSize: Integer): boolean;
{$ifdef mswindows}
  var
    ChooseFontRec: TChooseFont;

  begin
  FillChar(ChooseFontRec, SizeOf(ChooseFontRec), 0);
  with ChooseFontRec do
    begin
    lStructSize := SizeOf(ChooseFontRec);
    hDC := 0;
    lpLogFont := @LogFont;
    Flags := Flags or CF_INITTOLOGFONTSTRUCT;
    nSizeMin := MinSize;
    nSizeMax := MaxSize;
    Flags := Flags or CF_LIMITSIZE;
    Flags := Flags or CF_NOVECTORFONTS or CF_NOSIMULATIONS or CF_BOTH;
    if ForPrinter then Flags := Flags or CF_SCALABLEONLY;
    hWndOwner := Application.Handle;
    Result := QTaskModalDialog(@ChooseFont, ChooseFontRec);
    LogFontSize := iPointSize div 10;
    end;
  end;
{$endif}
begin

end;

//-----------------------------------------------------------

procedure InitLogFontStruct (var LogFont: TLogFont; Name: ShortString;
  Size: integer; Bold: boolean; Italics: boolean; Script: ShortString);

  begin
  FillChar(LogFont, SizeOf(LogFont), 0);
  StrPCopy(LogFont.lfFaceName, Name);
  LogFont.lfHeight  := QHeightFromSize(Size);
  LogFont.lfWeight  := FW_NORMAL;
  if Bold then LogFont.lfWeight  := FW_BOLD;
  if Italics then LogFont.lfItalic := 1;
  LogFont.lfCharSet := QGetScriptNumber (Script);
  end;

//-----------------------------------------------------------

procedure SetFontFromLogFont(Font: TFont; var LogFont: TLogFont);
  begin
  Font.Name := StrPas(LogFont.lfFaceName);
  Font.Height := LogFont.lfHeight;
  Font.Style := [];
  if LogFont.lfWeight = FW_BOLD
    then Font.Style := Font.Style + [fsBold];
  if LogFont.lfItalic = 1 then Font.Style := Font.Style + [fsItalic];
  Font.Handle := CreateFontIndirect(LogFont);
  end;

//-----------------------------------------------------------


end.
