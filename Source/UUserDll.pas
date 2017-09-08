unit UUserDll;
(*====================================================================
User DLL access
======================================================================*)

interface

uses
  {$ifdef mswindows}
  Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages;
  {$ENDIF}

var

  UserCommand: procedure(hAppWnd: HWND;
                         pszParams: PChar;
                         pszPath: PChar;
                         pszPathInArchive: PChar;
                         dwTime: DWORD;
                         dwSize: DWORD;
                         dwAttrib: DWORD;
                         pszDescription: PChar;
                         pszTempFolder: PChar;
                         dwDiskBaseAttrib: DWORD;
                         pReserved: Pointer); stdcall;

function  LoadUserDll(sDllPath: AnsiString): boolean;
procedure FreeUserDll();

implementation

uses UDebugLog;

var
  g_hUserDll: THandle;


//-----------------------------------------------------------------------------

function LoadUserDll(sDllPath: AnsiString): boolean;

  begin
  {$ifdef mswindows}
  Result := false;
  g_hUserDll := LoadLibrary(PChar(sDllPath));
  if g_hUserDll = 0 then
    begin
    LOG('Cannot load %s', [sDllPath]);
    exit;
    end;
  @UserCommand := GetProcAddress(g_hUserDll, PChar('UserCommand'));
  if @UserCommand = nil then
    begin
    LOG('Cannot loacte UserCommand in %s', [sDllPath]);
    FreeUserDll();
    exit;
    end;
  {$endif}
  Result := true;
  end;

//-----------------------------------------------------------------------------

procedure FreeUserDll();

  begin
  ///FreeLibrary(g_hUserDll);
  g_hUserDll := 0;
  end;

//-----------------------------------------------------------------------------

begin
g_hUserDll := 0;
end.
