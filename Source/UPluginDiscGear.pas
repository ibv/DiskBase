unit UPluginDiscGear;

interface

uses
{$ifdef mswindows}
Windows;
{$ELSE}
  LCLIntf, LCLType, LMessages;
{$ENDIF}

  function  DiscGearPrintPluginExists: boolean;
  procedure DiscGearPrintRun(hWindow: HWND; sText: AnsiString);

//.............................................................................

implementation

type
  TGetDiskBasePluginVersion = function : integer; stdcall;
  TSetLanguage = procedure(iLangId: integer); stdcall;
  TGetMenuLabel = procedure(pszMenu: PChar; iSize: integer); stdcall;
  TDisplayPrintDialog = procedure(hWindow: HWND; pszLines: PChar); stdcall;

//-----------------------------------------------------------------------------

function DiscGearPrintPluginExists: boolean;

  var
    DllHandle: integer;
    GetDiskBasePluginVersion: TGetDiskBasePluginVersion;
    SetLanguage             : TSetLanguage;
    GetMenuLabel            : TGetMenuLabel;
    DisplayPrintDialog      : TDisplayPrintDialog;

  begin
  Result := false;
  {$ifdef mswindows}
  DllHandle := LoadLibrary('dbp_dg.dll');
  if DllHandle > 32 then
    begin
    @GetDiskBasePluginVersion  := GetProcAddress(DllHandle, 'GetDiskBasePluginVersion');
    @SetLanguage               := GetProcAddress(DllHandle, 'SetLanguage');
    @GetMenuLabel              := GetProcAddress(DllHandle, 'GetMenuLabel');
    @DisplayPrintDialog        := GetProcAddress(DllHandle, 'DisplayPrintDialog');
    if (@GetDiskBasePluginVersion = nil) or
       (@SetLanguage = nil) or
       (@GetMenuLabel = nil) then
      begin
      FreeLibrary(DllHandle);
      exit;
      end;
    FreeLibrary(DllHandle);
    Result := true;
    end;
  {$endif}
   end;

//-----------------------------------------------------------------------------

procedure DiscGearPrintRun(hWindow: HWND; sText: AnsiString);

  var
    DllHandle: integer;
    GetDiskBasePluginVersion: TGetDiskBasePluginVersion;
    SetLanguage             : TSetLanguage;
    GetMenuLabel            : TGetMenuLabel;
    DisplayPrintDialog      : TDisplayPrintDialog;

  begin
  {$ifdef mswidnows}
  DllHandle := LoadLibrary('dbp_dg.dll');
  if DllHandle > 32 then
    begin
    @GetDiskBasePluginVersion  := GetProcAddress(DllHandle, 'GetDiskBasePluginVersion');
    @SetLanguage               := GetProcAddress(DllHandle, 'SetLanguage');
    @GetMenuLabel              := GetProcAddress(DllHandle, 'GetMenuLabel');
    @DisplayPrintDialog        := GetProcAddress(DllHandle, 'DisplayPrintDialog');
    if (@GetDiskBasePluginVersion = nil) or
       (@SetLanguage = nil) or
       (@GetMenuLabel = nil) then
      begin
      FreeLibrary(DllHandle);
      exit;
      end;
    sText := sText + #0;
    {$ifdef ENGLISH}
    SetLanguage(0);
    {$else}
    SetLanguage(1);
    {$endif}
    DisplayPrintDialog(hWindow, PChar(sText));
    FreeLibrary(DllHandle);
    end;
  {$endif}
  end;

//-----------------------------------------------------------------------------

end.
