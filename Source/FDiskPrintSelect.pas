unit FDiskPrintSelect;
(*====================================================================
Dialog for selection of print options
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
  TFormDiskPrintSelect = class(TForm)
    RadioButtonActDiskDir: TRadioButton;
    RadioButtonActDiskWhole: TRadioButton;
    RadioButtonSelectedDisks: TRadioButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    LabelWhat: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDiskPrintSelect: TFormDiskPrintSelect;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormDiskPrintSelect.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrintSelect.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskPrintSelect.FormShow(Sender: TObject);

  begin
  ActiveControl := RadioButtonActDiskDir;
  end;

//-----------------------------------------------------------------------------

end.
