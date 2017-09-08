#include <windows.h>
#include "DskBUtl.h"
#include "UEject9x.h"
#include "UEjectNt.h"

//---------------------------------------------------------------------------

DLLENTRY int WINAPI Eject(char cDriveLetter)
  {
  OSVERSIONINFO OsVersionInfo;
  OsVersionInfo.dwOSVersionInfoSize=sizeof(OSVERSIONINFO);
  GetVersionEx(&OsVersionInfo);
  bool bIsWindowsNT4 = OsVersionInfo.dwPlatformId==VER_PLATFORM_WIN32_NT &&
                 OsVersionInfo.dwMajorVersion >= 4;
  bool bIsWindows9x  = OsVersionInfo.dwPlatformId==VER_PLATFORM_WIN32_WINDOWS;

  if (bIsWindowsNT4)
    return EjectMediaNt(cDriveLetter);
  else
    if (bIsWindows9x)
      return EjectMedia9x(cDriveLetter);
  return 5;
  }

