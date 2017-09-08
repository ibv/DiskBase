unit UDskBUtl;
(*====================================================================
Unit for accessing external DLL, written in C++
======================================================================*)

interface

  uses
  {$ifdef mswindows}
  Window;
  {$ELSE}
    LCLIntf, LCLType, LMessages;
  {$ENDIF}


  ///function Eject(cDriveLetter: char): integer; stdcall;
  function Eject(cDriveLetter: char): integer;

implementation

  ///function Eject; external 'DskBUtl.dll';

  function Eject(cDriveLetter: char): integer;
  begin
    result:=0;
  end;

end.
