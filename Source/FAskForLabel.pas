unit FAskForLabel;
(*====================================================================
Dialog for disk name, under which the disk will be put into the database.
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}

  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  UTypes;

type
  TFormAskForLabel = class(TForm)
    EditDiskName: TEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Label1: TLabel;
    ButtonUseGenerated: TButton;
    LabelGenerated: TLabel;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonUseGeneratedClick(Sender: TObject);
  private
    { Private declarations }
  public
    GeneratedName: ShortString;
    GeneratedNameUsed: boolean;
    procedure DefaultHandler(var Message); override;
  end;

var
  FormAskForLabel: TFormAskForLabel;

implementation

uses ULang, FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormAskForLabel.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormAskForLabel.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormAskForLabel.FormShow(Sender: TObject);

  begin
  ActiveControl := EditDiskName;
  EditDiskName.SelectAll;
  GeneratedNameUsed := false;
  if GeneratedName <> ''
    then
      begin
      ButtonUseGenerated.Visible := true;
      LabelGenerated.Visible     := true;
      LabelGenerated.Caption     := lsGenerated + GeneratedName;
      end
    else
      begin
      ButtonUseGenerated.Visible := false;
      LabelGenerated.Visible     := false;
      end;
  end;

//-----------------------------------------------------------------------------

procedure TFormAskForLabel.ButtonUseGeneratedClick(Sender: TObject);

  begin
  EditDiskName.Text := GeneratedName;
  GeneratedNameUsed := true;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormAskForLabel.DefaultHandler(var Message);

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
