unit FMaskSelect;
(*====================================================================
Dialog box for selecting set of disks/files by a mask
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
  TFormMaskSelect = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    LabelMask: TLabel;
    ButtonHelp: TButton;
    ComboBoxMaskDisks: TComboBox;
    ComboBoxMaskFiles: TComboBox;
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
  FormMaskSelect: TFormMaskSelect;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------
// Add masks to history lists

procedure TFormMaskSelect.ButtonOKClick(Sender: TObject);

  var
    Found : boolean;
    i     : Integer;

  begin
  with ComboboxMaskDisks do
    if Visible and (Text <> '') then
      begin
      Found := false;
      for i := 0 to pred(Items.Count) do
        if Items.Strings[i] = Text then
          Found := true;
      if not Found then Items.Add(Text);
      end;
  with ComboboxMaskFiles do
    if Visible and (Text <> '') then
      begin
      Found := false;
      for i := 0 to pred(Items.Count) do
        if Items.Strings[i] = Text then
          Found := true;
      if not Found then Items.Add(Text);
      end;
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormMaskSelect.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormMaskSelect.ButtonHelpClick(Sender: TObject);

  begin
  Application.HelpContext(140);
  end;

//-----------------------------------------------------------------------------

procedure TFormMaskSelect.FormShow(Sender: TObject);

  begin
  if ComboBoxMaskFiles.Visible then
    ActiveControl := ComboBoxMaskFiles;
  if ComboBoxMaskDisks.Visible then
    ActiveControl := ComboBoxMaskDisks;
  end;

//-----------------------------------------------------------------------------

end.
