unit UDebugLog;
(*====================================================================
Functions for sending debug messages to external debug receiver
======================================================================*)

interface

  procedure Log(const FormatStr: String; const Args: array of const);

implementation

uses SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages;

var
  g_hLogWindow: HWND;
  g_bLoggingEnabled: boolean;
  g_bLogFileAreadyChecked: boolean;


//---------------------------------------------------------------------

function FileLoggingEnabled: boolean;

  begin
  Result := g_bLoggingEnabled;
  if g_bLogFileAreadyChecked then exit;
  // find if the LOG FILE application is running
  ///g_hLogWindow := FindWindow('TFormDebugLog'#0, 'Debug LOG Messages Receiver'#0);
  g_bLoggingEnabled := g_hLogWindow <> 0;
  g_bLogFileAreadyChecked := true; // do not initialize twice
  if not g_bLoggingEnabled then Result := false else Result := true;
  end;

//---------------------------------------------------------------------

procedure Log(const FormatStr: String; const Args: array of const);

  var
    Msg: AnsiString;
    {$ifdef mswindows}
    CopyData: COPYDATASTRUCT;
    {$endif}

  begin
  if not FileLoggingEnabled then exit;
  Msg := Format (FormatStr, Args);
  {$ifdef mswindows}
  CopyData.dwData := 0;
  CopyData.cbData := Length(Msg)+1;
  CopyData.lpData := PChar(Msg);
  SendMessage(g_hLogWindow, WM_COPYDATA, 0, integer(@CopyData));
  {$else}
  {$endif}
  end;

//-------------------------------------------------------------------

begin
g_hLogWindow := 0;
g_bLoggingEnabled := false;
g_bLogFileAreadyChecked := false;
end.
