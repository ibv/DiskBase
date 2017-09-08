unit ULicenseNumber;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TFormEnterLicNum = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    EditLicNum: TEdit;
    Label1: TLabel;
    ButtonHelp: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEnterLicNum: TFormEnterLicNum;


{--------------------------------------------------------------------}


implementation

{$R *.DFM}

{--------------------------------------------------------------------}

procedure TFormEnterLicNum.ButtonOKClick(Sender: TObject);
begin
ModalResult := mrOK;
end;

{--------------------------------------------------------------------}

procedure TFormEnterLicNum.ButtonCancelClick(Sender: TObject);
begin
ModalResult := mrCancel;
end;

{--------------------------------------------------------------------}

procedure TFormEnterLicNum.ButtonHelpClick(Sender: TObject);
begin
Application.HelpContext(410);
end;

procedure TFormEnterLicNum.FormShow(Sender: TObject);
begin
ActiveControl := EditLicNum;
end;

end.
