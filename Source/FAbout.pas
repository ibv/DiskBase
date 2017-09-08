unit FAbout;
(*====================================================================
The About Dialog, available in Menu->Help->About
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages, ComCtrls,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls,
  UTypes;

type
  TFormAbout = class(TForm)
    ButtonOK: TButton;
    ButtonMoreInfo: TButton;
    LabelAbout: TLabel;
    Panel1: TPanel;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonMoreInfoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

uses
  ULang;

//-----------------------------------------------------------------------------

procedure TFormAbout.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormAbout.FormCreate(Sender: TObject);

  begin
  // these constants are defined in UTypes
  LabelAbout.Caption := About2;
  end;

//-----------------------------------------------------------------------------

procedure TFormAbout.ButtonMoreInfoClick(Sender: TObject);

  begin
  Application.HelpContext(10);
  end;

//-----------------------------------------------------------------------------

procedure TFormAbout.FormShow(Sender: TObject);

  var S: ShortString;
  begin
  ActiveControl := ButtonOK;
  end;

//-----------------------------------------------------------------------------

end.
