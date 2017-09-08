unit FFindEmptyDlg;
(*====================================================================
Options for serching disks according free space - not used
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
  Forms, Dialogs, StdCtrls, ExtCtrls,
  UTypes;

type
  TFormSearchEmptyDlg = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    RadioGroupSort: TRadioGroup;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSearchEmptyDlg: TFormSearchEmptyDlg;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormSearchEmptyDlg.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchEmptyDlg.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchEmptyDlg.FormShow(Sender: TObject);

  begin
  ActiveControl := RadioGroupSort;
  end;

//-----------------------------------------------------------------------------

end.
