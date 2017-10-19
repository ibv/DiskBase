unit Qexcept;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses SysUtils;

type
  EQDirException = class(Exception);
  EQDirFatalException  = class(EQDirException);
  EQDirNormalException = class(EQDirException);
  EQDirDBaseStructException = class(EQDirException);


  procedure NormalErrorMessage(Msg: ShortString);
  procedure FatalErrorMessage (Msg: ShortString);


implementation

uses
{$IFnDEF FPC}
  WinTypes, WinProcs,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Forms;

const
  lsCriticalError = 'Critical error';
  lsCriticalError1 = 'Critical error.';
  lsError = 'Error';

procedure NormalErrorMessage(Msg: ShortString);

  var
    MsgText, MsgCaption: array[0..256] of char;

  begin
  StrPCopy(MsgText, Msg);
  StrPCopy(MsgCaption, lsError);
  Application.MessageBox(MsgText, MsgCaption, mb_OK or mb_IconExclamation);
  end;

procedure FatalErrorMessage(Msg: ShortString);

  var
    MsgText, MsgCaption: array[0..256] of char;

  begin
  StrPCopy(MsgText, lsCriticalError1 + #13#10 + Msg);
  StrPCopy(MsgCaption, lsCriticalError);
  Application.MessageBox(MsgText, MsgCaption, mb_OK or mb_IconStop);
  end;


end.
