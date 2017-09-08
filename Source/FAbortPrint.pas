unit FAbortPrint;
(*====================================================================
This is a non-modal window showed during print so that the user can click on
the ABort button.
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
  Forms, Dialogs, StdCtrls,
  UTypes;

type
  TFormAbortPrint = class(TForm)
    ButtonAbort: TButton;
    LabelProgress: TLabel;
    procedure ButtonAbortClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    Aborted: boolean;
  end;

var
  FormAbortPrint: TFormAbortPrint;

implementation

{$R *.dfm}

///uses Printers;

//-----------------------------------------------------------------------------

procedure TFormAbortPrint.ButtonAbortClick(Sender: TObject);

  begin
  Aborted := true;
  end;

(*
Using printer.aborted made problems and thus it is not used:

if not Printer.Aborted then
  begin
  AbortDoc(Printer.Handle); <--- this makes crash...
  Printer.Abort;
  end;

When we use Printer.Abort, and do not call EndDoc, the print task remains active
*)

//-----------------------------------------------------------------------------

procedure TFormAbortPrint.FormShow(Sender: TObject);

  begin
  Aborted := false;
  end;

//-----------------------------------------------------------------------------

end.
