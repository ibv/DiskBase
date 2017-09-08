unit FSelectDrive;
(*====================================================================
Dialog box for selecting the drive to be scanned
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
  Forms, Dialogs, StdCtrls, FileCtrl, ExtCtrls,
  UTypes;

type
  TFormSelectDrive = class(TForm)
    ListBoxDrives: TListBox;
    Panel1: TPanel;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxNoQueries: TCheckBox;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxDrivesDblClick(Sender: TObject);
    procedure CheckBoxNoQueriesClick(Sender: TObject);
  private
    SaveItemIndex: Integer;
  public
    procedure DefaultHandler(var Message); override;
    procedure SetFormSize;
    { Public declarations }
  end;

var
  FormSelectDrive: TFormSelectDrive;

implementation

uses IniFiles, UDrives, FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormSelectDrive.ButtonOKClick(Sender: TObject);

  begin
  if ListBoxDrives.MultiSelect and (ListBoxDrives.SelCount >= 0) or
    (ListBoxDrives.ItemIndex >= 0) then
    ModalResult := mrOk;
  end;

//-----------------------------------------------------------------------------

procedure TFormSelectDrive.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormSelectDrive.FormShow(Sender: TObject);

  var
    i: Integer;

  begin
  Screen.Cursor := crHourGlass; // sometimes it takes long time to create the list
  BuildDriveList;
  if (ListBoxDrives.MultiSelect)
    then
      begin
      if (ListBoxDrives.Items.Count > 0) then
        begin
        SaveItemIndex := 0;
        for i:= 0 to pred(ListBoxDrives.Items.Count) do
          if ListBoxDrives.Selected[i] then
            begin
            SaveItemIndex := i;
            break;
            end;
        end
      end
    else
      begin
      if (ListBoxDrives.ItemIndex) >= 0 then
        SaveItemIndex := ListBoxDrives.ItemIndex;
      end;

  ListBoxDrives.Items.Clear;
  for i := 0 to pred(DriveList.Count) do
    ListBoxDrives.Items.Add(DriveList.Strings[i]);
  if SaveItemIndex > pred(DriveList.Count) then SaveItemIndex := pred(DriveList.Count);

  try
  if (ListBoxDrives.MultiSelect)
    then
      ListBoxDrives.Selected[SaveItemIndex] := true
    else
      ListBoxDrives.ItemIndex := SaveItemIndex;
  except
  end;
  ActiveControl := ListBoxDrives;
  Screen.Cursor := crDefault;
  CheckBoxNoQueries.Checked := g_CommonOptions.bNoQueries;
  BringToFront;
  end;

//-----------------------------------------------------------------------------
// Resizable form - restore the size from the last time

procedure TFormSelectDrive.SetFormSize;

  var
    IniFile: TIniFile;
    SLeft, STop, SWidth, SHeight: integer;

  begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  SLeft   := IniFile.ReadInteger  ('ScanDiskWindow', 'Left',
                                   (Screen.Width - Width) div 2);
  STop    := IniFile.ReadInteger  ('ScanDiskWindow', 'Top',
                                   (Screen.Height - Height) div 2);
  SWidth  := IniFile.ReadInteger  ('ScanDiskWindow', 'Width',  Width);
  SHeight := IniFile.ReadInteger  ('ScanDiskWindow', 'Height', Height);
  SaveItemIndex := IniFile.ReadInteger ('ScanDiskWindow', 'Position', 0);
  IniFile.Free;
  SetBounds(SLeft, STop, SWidth, SHeight);
  end;

//-----------------------------------------------------------------------------

procedure TFormSelectDrive.ListBoxDrivesDblClick(Sender: TObject);

  begin
  ButtonOKClick(Sender);
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormSelectDrive.DefaultHandler(var Message);

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

procedure TFormSelectDrive.CheckBoxNoQueriesClick(Sender: TObject);
  begin
  g_CommonOptions.bNoQueries := CheckBoxNoQueries.Checked;
  end;

//-----------------------------------------------------------------------------

end.
