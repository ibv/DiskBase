//---------------------------------------------------------------------------

#ifndef DskBUtlH
#define DskBUtlH
//---------------------------------------------------------------------------

#if defined(__cplusplus)
  #define DLLENTRY extern "C" __declspec(dllexport)
#else
  #define DLLENTRY __declspec(dllexport)
#endif

DLLENTRY int WINAPI Eject(char cDriveLetter);

#endif
