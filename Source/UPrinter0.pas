unit UPrinter;
(*====================================================================
Implementation of printing
======================================================================*)

{$ifndef DELPHI1}
{$LONGSTRINGS ON}
{$endif}

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Graphics, FSettings,
  UTypes
{$ifndef DELPHI1}
  , UFont {Windows}
{$endif}
  ;

type
  TPrintFontStyle = (pfsNormal, pfsBold, pfsItalic);

{ define FULLCTL - if defined, the Printer object is not used at all
 if not defined, the printer object is used, but between BeginDoc and
 EndDoc no Printer properties or methods are used, so it cannot
 influence the font
 Full Control is not finished yet - does not recognize orientation
 }

var
  TopPrintAreaPx, BottomPrintAreaPx: Integer;
  LeftPrintAreaPx, RightPrintAreaPx: Integer;
  WidthPx, HeightPx: Integer;
  LineHeightPx: Integer;
  Widthmm, Heightmm: Integer;
  XPxPer1cm, YPxPer1cm: Integer;
  XPxPer1mm, YPxPer1mm: Integer;
  TimePrinted: ShortString;

procedure QPrinterReset(QGlobalOptions: TGlobalOptions);
procedure QPrinterNewPage;
procedure QPrinterSaveAndSetFont(PrintFontStyle: TPrintFontStyle);
procedure QPrinterRestoreFont;
///procedure QPrinterTextRect(Rect: TRect; X, Y: Integer; const Text: string);
procedure QPrinterMoveTo(X, Y: Integer);
procedure QPrinterLineTo(X, Y: Integer);
procedure QPrinterSetLineWidth(Width: Integer);
procedure QPrinterBeginDoc(Title: string);
procedure QPrinterEndDoc;
function  QPrinterGetTextWidth(const Text: String): Integer;

//-----------------------------------------------------------------------------

implementation

uses
  Printers, UBaseUtils, ULang;

var
  QPHandle: HDC;
{$ifdef LOGFONT}
  PrintLogFont: TLogFont;
  PrintLogFontSize: Integer;
{$else}
  PrintFont: TFont;
{$endif}

{$ifdef FULLCTL}
  Device: array[0..256] of char;
  Driver: array[0..256] of char;
  Port: array[0..256] of char;
  DeviceMode: THandle;
{$endif}

//-----------------------------------------------------------------------------

{$ifdef FULLCTL}
function AbortProc(Prn: HDC; Error: Integer): Bool; stdcall;
  begin
  //Application.ProcessMessages;
  Result := True;
  end;
{$endif}

//-----------------------------------------------------------------------------

function QPrinterGetTextWidth(const Text: String): Integer;
  var
    Extent: TSize;
  begin
{$ifdef LOGFONT}
  if Windows.GetTextExtentPoint(QPHandle, PChar(Text), Length(Text), Extent) then
    Result := Extent.cX else
    Result := 0;
{$else}
  Result := Printer.Canvas.TextWidth(Text);
{$endif}
  end;

//-----------------------------------------------------------------------------

function QPrinterGetTextHeight(const Text: String): Integer;
  var
    Extent: TSize;
  begin
{$ifdef LOGFONT}
  if Windows.GetTextExtentPoint(QPHandle, PChar(Text), Length(Text), Extent) then
    Result := Extent.cY else
    Result := 0;
{$else}
  Result := Printer.Canvas.TextHeight(Text);
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterSetFont;

{$ifdef LOGFONT}
  var
    PixelsPerInch: longint;
    FontHandle: HFont;
    PreviousFontHandle: HFont;

  begin
  PrintLogFont.lfWeight := FW_NORMAL;
  PrintLogFont.lfItalic := 0;
  PixelsPerInch := GetDeviceCaps(QPHandle, LOGPIXELSY);
  PrintLogFont.lfHeight := -MulDiv(PrintLogFontSize, PixelsPerInch, 72);
  FontHandle := CreateFontIndirect(PrintLogFont);
  PreviousFontHandle := SelectObject(QPHandle, FontHandle);
  DeleteObject(PreviousFontHandle);
  end;
{$else}
  begin
  Printer.Canvas.Font.Assign(PrintFont);
  end;
{$endif}

//-----------------------------------------------------------------------------

procedure QPrinterSaveAndSetFont(PrintFontStyle: TPrintFontStyle);

{$ifdef LOGFONT}
  var
    PixelsPerInch: longint;
    FontHandle: HFont;
    PreviousFontHandle: HFont;

  begin
  if PrintFontStyle = pfsNormal then
    begin
    PrintLogFont.lfWeight := FW_NORMAL;
    PrintLogFont.lfItalic := 0;
    end;
  if PrintFontStyle = pfsBold then
    begin
    PrintLogFont.lfWeight := FW_BOLD;
    PrintLogFont.lfItalic := 0;
    end;
  if PrintFontStyle = pfsItalic then
    begin
    PrintLogFont.lfWeight := FW_NORMAL;
    PrintLogFont.lfItalic := 1;
    end;

  PixelsPerInch := GetDeviceCaps(QPHandle, LOGPIXELSY);
  PrintLogFont.lfHeight := -MulDiv(PrintLogFontSize, PixelsPerInch, 72);
  FontHandle := CreateFontIndirect(PrintLogFont);
  PreviousFontHandle := SelectObject(QPHandle, FontHandle);
  DeleteObject(PreviousFontHandle);
  end;
{$else}
  begin
  Printer.Canvas.Font.Style := [];
  if PrintFontStyle = pfsItalic then
    Printer.Canvas.Font.Style := [fsItalic];
  if PrintFontStyle = pfsBold then
    Printer.Canvas.Font.Style := [fsBold];
  end;
{$endif}

//-----------------------------------------------------------------------------

procedure QPrinterRestoreFont;

  begin
{$ifdef LOGFONT}
  QPrinterSetFont;
{$else}
  Printer.Canvas.Font.Style := [];
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure RecheckHandle;

  begin
  {$ifdef LOGFONT}
  QPHandle := Printer.Handle;
  QPrinterSetFont;
  {$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterReset(QGlobalOptions: TGlobalOptions);

  begin
  {$ifdef FULLCTL}
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
  QPHandle := Windows.CreateIC(Driver, Device, Port, @DeviceMode);
  Widthmm  := GetDeviceCaps(QPHandle, HORZSIZE);
  Heightmm := GetDeviceCaps(QPHandle, VERTSIZE);
  WidthPx  := GetDeviceCaps(QPHandle, HORZRES);
  HeightPx := GetDeviceCaps(QPHandle, VERTRES);
  XPxPer1cm := round((WidthPx / Widthmm) * 10);
  YPxPer1cm := round((HeightPx / Heightmm) * 10);
  XPxPer1mm := XPxPer1cm div 10 + 1;
  YPxPer1mm := YPxPer1cm div 10 + 1;
  TopPrintAreaPx     := (QGlobalOptions.TopMargin * YPxPer1cm) div 10;
  BottomPrintAreaPx  := HeightPx - (QGlobalOptions.BottomMargin * YPxPer1cm) div 10;
  LeftPrintAreaPx    := (QGlobalOptions.LeftMargin * XPxPer1cm) div 10;
  RightPrintAreaPx   := WidthPx - (QGlobalOptions.RightMargin * XPxPer1cm) div 10;
  TimePrinted        := lsQDir + DateTimeToStr(Now);
  PrintLogFont := QGlobalOptions.PrintLogFont;
  PrintLogFontSize := QGlobalOptions.PrintLogFontSize;
  QPrinterSetFont;
  LineHeightPx       := QPrinterGetTextHeight('‹»¡/yp') + 1;
  if QPHandle <> 0 then DeleteDC(QPHandle);
  QPHandle := 0;

  {$else}

  Widthmm  := GetDeviceCaps(Printer.Handle, HORZSIZE);
  Heightmm := GetDeviceCaps(Printer.Handle, VERTSIZE);
  WidthPx  := GetDeviceCaps(Printer.Handle, HORZRES);
  HeightPx := GetDeviceCaps(Printer.Handle, VERTRES);
  XPxPer1cm := round((WidthPx / Widthmm) * 10);
  YPxPer1cm := round((HeightPx / Heightmm) * 10);
  XPxPer1mm := XPxPer1cm div 10 + 1;
  YPxPer1mm := YPxPer1cm div 10 + 1;
  TopPrintAreaPx     := (QGlobalOptions.TopMargin * YPxPer1cm) div 10;
  BottomPrintAreaPx  := HeightPx - (QGlobalOptions.BottomMargin * YPxPer1cm) div 10;
  LeftPrintAreaPx    := (QGlobalOptions.LeftMargin * XPxPer1cm) div 10;
  RightPrintAreaPx   := WidthPx - (QGlobalOptions.RightMargin * XPxPer1cm) div 10;
  TimePrinted        := lsQDir + DateTimeToStr(Now);
  {$ifdef LOGFONT}
  PrintLogFont := QGlobalOptions.PrintLogFont;
  PrintLogFontSize := QGlobalOptions.PrintLogFontSize;
  RecheckHandle;
  {$else}
  PrintFont := QGlobalOptions.PrintFont;
  Printer.Canvas.Font.Assign(PrintFont);
  {$endif}
  LineHeightPx       := QPrinterGetTextHeight('‹»¡/yp') + 1;
  {$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterNewPage;

  begin
{$ifdef FULLCTL}
  EndPage(QPHandle);
  StartPage(QPHandle);
{$else}
  Printer.NewPage;
  RecheckHandle;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterTextRect(Rect: TRect; X, Y: Integer; const Text: string);
  var
    Options: Integer;
  begin
{$ifdef LOGFONT}
  Options := ETO_CLIPPED;
  Windows.ExtTextOut(QPHandle, X, Y, Options, @Rect, PChar(Text),
    Length(Text), nil);
{$else}
  Printer.Canvas.TextRect(Rect, X, Y, Text);
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterMoveTo(X, Y: Integer);
  begin
{$ifdef LOGFONT}
  Windows.MoveToEx(QPHandle, X, Y, nil);
{$else}
  Printer.Canvas.MoveTo(X, Y);
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterLineTo(X, Y: Integer);
  begin
{$ifdef LOGFONT}
  Windows.LineTo(QPHandle, X, Y);
{$else}
  Printer.Canvas.LineTo(X, Y);
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterSetLineWidth(Width: Integer);

  var
    LogPen: TLogPen;
    PenHandle, PreviousHandle: HPen;

  begin
{$ifdef LOGFONT}
  with LogPen do
    begin
    lopnStyle := PS_SOLID;
    lopnWidth.X := Width;
    lopnColor := 0;
    end;
  PenHandle := CreatePenIndirect(LogPen);
  PreviousHandle := SelectObject(QPHandle, PenHandle);
  DeleteObject(PreviousHandle);
{$else}
  Printer.Canvas.Pen.Width := Width;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure QPrinterBeginDoc(Title: string);

{$ifdef FULLCTL}
  var
    CTitle: array[0..31] of Char;
    DocInfo: TDocInfo;
  begin
  if QPHandle = 0 then
    QPHandle := Windows.CreateDC(Driver, Device, Port, @DeviceMode);
  QPrinterSetFont;
  StrPLCopy(CTitle, Title, SizeOf(CTitle) - 1);
  FillChar(DocInfo, SizeOf(DocInfo), 0);
  with DocInfo do
    begin
    cbSize := SizeOf(DocInfo);
    lpszDocName := CTitle;
    lpszOutput := nil;
    end;
  SetAbortProc(QPHandle, AbortProc);
  StartDoc(QPHandle, DocInfo);
  StartPage(QPHandle);
  end;
{$else}
  begin
  Printer.Title := Title;
  Printer.BeginDoc;
  RecheckHandle;
  end;
{$endif}

//-----------------------------------------------------------------------------

procedure QPrinterEndDoc;
  var
    PreviousPenHandle: HPen;
    PreviousFontHandle: HFont;
  begin
{$ifdef FULLCTL}
  PreviousPenHandle := SelectObject(QPHandle, GetStockObject(BLACK_PEN));
  DeleteObject(PreviousPenHandle);
  PreviousFontHandle := SelectObject(QPHandle, GetStockObject(SYSTEM_FONT));
  DeleteObject(PreviousFontHandle);
  EndPage(QPHandle);
  Windows.EndDoc(QPHandle);
  if QPHandle <> 0 then DeleteDC(QPHandle);
  QPHandle := 0;
{$else}
  {$ifdef LOGFONT}
  PreviousPenHandle := SelectObject(QPHandle, GetStockObject(BLACK_PEN));
  DeleteObject(PreviousPenHandle);
  PreviousFontHandle := SelectObject(QPHandle, GetStockObject(SYSTEM_FONT));
  DeleteObject(PreviousFontHandle);
  {$endif}

  Printer.EndDoc;
{$endif}
  end;

//-----------------------------------------------------------------------------

begin
QPHandle := 0;
end.
