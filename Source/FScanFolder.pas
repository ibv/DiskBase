unit FScanFolder;
(*====================================================================
Dialog box for scanning a folder as a disk and for rescanning disk
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
  TFormScanFolder = class(TForm)
    EditFolder: TEdit;
    LabelScanFolder: TLabel;
    LabelSaveIt: TLabel;
    EditDiskName: TEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonHelp: TButton;
    ButtonBrowseFolder: TButton;
    ButtonCreateDiskName: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonBrowseFolderClick(Sender: TObject);
    procedure ButtonCreateDiskNameClick(Sender: TObject);
    procedure EditFolderChange(Sender: TObject);
    procedure EditDiskNameChange(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ScanningFolders: boolean;
    SavedPath      : ShortString;
  public
    VolumeLabel: ShortString;
    Directory  : ShortString;
    DiskName   : ShortString;
    procedure SetAppearance(ForScanningFolders: boolean);
    procedure DefaultHandler(var Message); override;
  end;

var
  FormScanFolder: TFormScanFolder;

implementation

uses FSelectDrive, UDrives, FSelectFolder, UBaseUtils, ULang, FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------
// Set the labels, so that it is for scanning folder as disk or for rescanning disk

procedure TFormScanFolder.SetAppearance(ForScanningFolders: boolean);

  begin
  ScanningFolders := ForScanningFolders;
  if ForScanningFolders
    then
      begin
      Caption := lsScanFolderAsDisk;
      LabelScanFolder.Caption := lsScanFolder;
      LabelSaveIt.Caption := lsAndSaveItAsDisk;
      ButtonCreateDiskName.Visible := true;
      EditDiskName.Enabled := true;
      EditDiskName.Font.Color := clWindowText;
      HelpContext := 116;
      end
    else
      begin
      Caption := lsRescanDisk;
      LabelScanFolder.Caption := lsRescanFolder;
      LabelSaveIt.Caption := lsAndSaveItAsUpdateOfDisk;
      ButtonCreateDiskName.Visible := false;
      EditDiskName.Enabled := false;
      EditDiskName.Font.Color := clBtnShadow;
      HelpContext := 115;
      end;
  end;

//-----------------------------------------------------------------------------
// Checks the validity of entered text

procedure TFormScanFolder.ButtonOKClick(Sender: TObject);

  var
    TmpS: ShortString;
    ValidPath: boolean;

  begin
  TmpS := EditFolder.Text;
  if (length(TmpS) > 3) and (TmpS[length(TmpS)] = '\')
    then dec(TmpS[0]);
  if not ScanningFolders and (AnsiUpperCase(SavedPath) <> AnsiUpperCase(TmpS)) then
    if Application.MessageBox(lsRescanWarning, lsWarning, MB_YESNOCANCEL) <> IDYES then exit;

    ValidPath := true;
  if length(TmpS) < 3 then ValidPath := false;
  if TmpS[2] <> ':' then ValidPath := false;
  if TmpS[3] <> '\' then ValidPath := false;
  if not DirExists(TmpS) then ValidPath := false;
  if ValidPath
    then
      begin
      TmpS[1]     := UpCase(TmpS[1]);
      Directory   := TmpS;
      VolumeLabel := QGetDriveName(TmpS[1]);
      DiskName    := EditDiskName.Text;
      ModalResult := mrOK;
      end
    else
      begin
      Application.MessageBox(
        lsInvalidFolder,
        lsError, mb_OK or mb_IconExclamation);
      end;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanFolder.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanFolder.FormShow(Sender: TObject);

  begin
  ButtonOK.Enabled := (EditFolder.Text <> '') and (EditDiskName.Text <> '');
  ActiveControl := EditFolder;
  SavedPath := EditFolder.Text;
  VolumeLabel:= '';
  Directory  := '';
  DiskName   := '';
  end;

//-----------------------------------------------------------------------------

procedure TFormScanFolder.ButtonBrowseFolderClick(Sender: TObject);

  begin
  if FormSelectFolder.ShowModal = mrOK then
    begin
    EditFolder.Text := FormSelectFolder.Directory;
    end;
  end;

//-----------------------------------------------------------------------------
// Creates disk name from the path

procedure TFormScanFolder.ButtonCreateDiskNameClick(Sender: TObject);

  const
    MaxLen = 35;

  var
    VolumeLabel: ShortString;
    TmpS, TmpS2: ShortString;
    i          : Integer;
    Len        : Integer;
    Drive      : char;

  begin
  TmpS := EditFolder.Text;
  if (TmpS <> '') and (UpCase(TmpS[1]) >= 'A') and (UpCase(TmpS[1]) <= 'Z') then
    begin
    Drive := TmpS[1];
    VolumeLabel := QGetDriveName(Drive);
    if VolumeLabel <> '' then
      begin
      TmpS2 := AnsiUpperCase(VolumeLabel);
      if TmpS2 = VolumeLabel then
        begin
        VolumeLabel := AnsiLowerCase(VolumeLabel);
        VolumeLabel[1] := TmpS2[1];
        end;
      end;
    Delete(TmpS, 1, 2);
    TmpS := VolumeLabel + ': ' + TmpS;
    Len := length(TmpS);
    if Len > MaxLen then
      begin
      delete(TmpS, MaxLen div 2, Len - MaxLen);
      insert('...', TmpS, MaxLen div 2);
      end;
    for i := 1 to length(TmpS) do
      if TmpS[i] = '\' then TmpS[i] := '/';
    EditDiskName.Text := TmpS;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanFolder.EditFolderChange(Sender: TObject);

  begin
  ButtonOK.Enabled := (EditFolder.Text <> '') and (EditDiskName.Text <> '');
  end;

//-----------------------------------------------------------------------------

procedure TFormScanFolder.EditDiskNameChange(Sender: TObject);

  begin
  ButtonOK.Enabled := (EditFolder.Text <> '') and (EditDiskName.Text <> '');
  end;

//-----------------------------------------------------------------------------

procedure TFormScanFolder.ButtonHelpClick(Sender: TObject);

  begin
  Application.HelpContext(HelpContext);
  end;

//-----------------------------------------------------------------------------

procedure TFormScanFolder.FormCreate(Sender: TObject);

  begin
  ScanningFolders := true;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormScanFolder.DefaultHandler(var Message);

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
