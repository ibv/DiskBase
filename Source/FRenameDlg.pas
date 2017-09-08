unit FRenameDlg;
(*====================================================================
Dialog for renaming disk
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
  TFormRenameDlg = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DefaultHandler(var Message); override;
  end;

var
  FormRenameDlg: TFormRenameDlg;

implementation

uses FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormRenameDlg.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormRenameDlg.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormRenameDlg.FormShow(Sender: TObject);

  begin
  ActiveControl := Edit1;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormRenameDlg.DefaultHandler(var Message);

  begin
  with TMessage(Message) do
    if (Msg = g_CommonOptions.dwQueryCancelAutoPlay) and
        g_CommonOptions.bDisableCdAutorun
      then
        Result := 1
      else
        inherited DefaultHandler(Message)
  end;

//-----------------------------------------------------------------------------

end.
