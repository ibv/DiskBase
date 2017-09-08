unit FScanArchive;
(*====================================================================
Dialog box for asking, whether to scan the contents of specified archive.
By default asking is disabled, and probably nobody uses it...
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
  Forms, Dialogs, StdCtrls, UTypes;

type
  TFormScanArchive = class(TForm)
    LabelMsg: TLabel;
    ButtonYes: TButton;
    ButtonNo: TButton;
    ButtonYesAll: TButton;
    ButtonNoAll: TButton;
    procedure ButtonYesClick(Sender: TObject);
    procedure ButtonYesAllClick(Sender: TObject);
    procedure ButtonNoClick(Sender: TObject);
    procedure ButtonNoAllClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    Response: word;
    procedure DefaultHandler(var Message); override;
  end;

var
  FormScanArchive: TFormScanArchive;

implementation

uses UCallbacks, FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormScanArchive.ButtonYesClick(Sender: TObject);

  begin
  Response := cmYes;
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanArchive.ButtonYesAllClick(Sender: TObject);

  begin
  Response := cmYesAll;
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanArchive.ButtonNoClick(Sender: TObject);

  begin
  Response := cmNo;
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanArchive.ButtonNoAllClick(Sender: TObject);

  begin
  Response := cmNoAll;
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanArchive.FormActivate(Sender: TObject);

  begin
  Response := cmNo;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormScanArchive.DefaultHandler(var Message);

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
