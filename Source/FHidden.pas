unit FHidden;
(*====================================================================
Hidden window - it is used for formatting of yellow quick-info windows
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  THiddenForm = class(TForm)
    MemoForHints: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HiddenForm: THiddenForm;

implementation

{$R *.dfm}

end.
