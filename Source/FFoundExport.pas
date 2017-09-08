unit FFoundExport;
(*====================================================================
Progress window for exporting found items to text format, implements
the export in the Run procedure
======================================================================*)

interface

uses
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  UApiTypes, UStringList;

type
  TFormFoundExport = class(TForm)
    ButtonCancel: TButton;
    LabelWhatDoing: TLabel;
    SaveDialogExportToText: TSaveDialog;
    OpenDialogExportFormat: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    bCanRun        : boolean;
  public
    bStopIt        : boolean;
    // these variables to be set from outside
    DBaseHandle    : PDBaseHandle;
    IsFileFoundList: boolean;
    ///FoundList      : TQStringList;
    FoundList      : TStringList;
    DBaseFileName  : ShortString;
    procedure Run(var Info); message WM_User;
    procedure UpdateProgress;
  end;

var
  FormFoundExport: TFormFoundExport;

implementation

uses UApi, UExport, ULang, UExceptions, FDiskPrintSelect, FSettings,
     UBaseUtils, FFoundFile, FFoundEmpty;

{$R *.dfm}

//---TFormDiskExport-----------------------------------------------------------

procedure TFormFoundExport.FormCreate(Sender: TObject);

  begin
  bCanRun := false;
  OpenDialogExportFormat.InitialDir := ExtractFilePath(ParamStr(0)) + lsExportFolder;
  SaveDialogExportToText.InitialDir := ExtractFilePath(ParamStr(0)) + lsExportFolder;
  end;

//-----------------------------------------------------------------------------

procedure TFormFoundExport.FormShow(Sender: TObject);

  begin
  bStopIt := false;
  bCanRun := true;
  LabelWhatDoing.Caption := lsPreparingToExport;
  // executes the Run procedure after the window is shown
  PostMessage(Self.Handle, WM_User, 0, 0);
  end;

//-----------------------------------------------------------------------------

procedure TFormFoundExport.Run (var Info);

  var
    MsgText : array[0..256] of char;
    OneFLine: TOneFLine;
    OneELine: TOneELine;
    Index   : integer;
    Counter : integer;
    iResult : integer;
    ExportOnlySelected: boolean;

  begin
  if not bCanRun then exit;
  bCanRun := false;
  bStopIt := false;
  FormSettings.UpdateGlobalFormatSettings;

  try
    // check if there are more files selected
    Counter := 0;
    ExportOnlySelected := false;
    for Index := 0 to FoundList.Count-1 do
      begin
      OneFLine := TOneFLine(FoundList.Objects[Index]);
      if (OneFLine.ExtAttr and eaSelected) <> 0 then inc(Counter);
      end;
    // if so, ask the user
    if Counter > 0 then
      begin
      iResult := Application.MessageBox(lsExportOnlySelected,
                                        lsMultipleItemsSelected, MB_YESNOCANCEL);
      if iResult = IDCANCEL then
        begin
        ModalResult := mrCancel;
        exit;
        end;
      ExportOnlySelected := iResult = IDYES;
      end;

    if IsFileFoundList
      then OpenDialogExportFormat.Filter := lsFilterFileList
      else OpenDialogExportFormat.Filter := lsFilterEmptyList;

    if not OpenDialogExportFormat.Execute then
      begin
      ModalResult := mrCancel;
      exit;
      end;
    OpenDialogExportFormat.InitialDir := ExtractFilePath(OpenDialogExportFormat.FileName);

    DBaseExport.Init(OpenDialogExportFormat.FileName, DBaseHandle);
    if (not DBaseExport.bFormatVerified) then
      begin
      NormalErrorMessage(lsNotValidFormatFile);
      ModalResult := mrCancel;
      exit;
      end;

    SaveDialogExportToText.DefaultExt := DBaseExport.sDefaultExt;
    if DBaseExport.sFileFilter <> ''
      then SaveDialogExportToText.Filter := DBaseExport.sFileFilter + '|' + lsDefaultFilter
      else SaveDialogExportToText.Filter := lsDefaultFilter;

    if not SaveDialogExportToText.Execute then
      begin
      ModalResult := mrCancel;
      exit;
      end;
    SaveDialogExportToText.InitialDir := ExtractFilePath(SaveDialogExportToText.FileName);

    DBaseExport.OpenOutputFile(SaveDialogExportToText.FileName);
    gDatabaseData.sDatabaseName  := ExtractFileName(DBaseFileName);
    gDatabaseData.sDatabasePath  := ExtractFilePath(DBaseFileName);

    DBaseExport.OnDatabaseBegin;

    if IsFileFoundList then
      begin
      for Index := 0 to FoundList.Count-1 do
        begin
        OneFLine                := TOneFLine(FoundList.Objects[Index]);
        if ExportOnlySelected and ((OneFLine.ExtAttr and eaSelected) <> 0) or
           not ExportOnlySelected then
             begin
             gFolderData.sFolderName := GetPQString(OneFLine.Dir);
             gDiskData.sDiskName     := GetPQString(OneFLine.Disk);
             DBaseExport.WriteOneFile(OneFLine.POneFile);
             end;
        Application.ProcessMessages;
        if bStopIt then break;
        if (Index mod 100) = 0 then UpdateProgress;
        end; // for
      end
    else  // it is a disk free list
      begin
      for Index := 0 to FoundList.Count-1 do
        begin
        OneELine                := TOneELine(FoundList.Objects[Index]);
        if ExportOnlySelected and ((OneELine.ExtAttr and eaSelected) <> 0) or
           not ExportOnlySelected then
             begin
             gDiskData.iDiskSizeKb   := OneELine.DiskSize;
             gDiskData.iDiskFreeKb   := OneELine.DiskFree;
             gDiskData.sDiskName     := GetPQString(OneELine.Disk);
             DBaseExport.OnDiskBegin;
             end;
        Application.ProcessMessages;
        if bStopIt then break;
        if (Index mod 100) = 0 then UpdateProgress;
        end; // for
      end;

    DBaseExport.OnDatabaseEnd;
    DBaseExport.CloseOutputFile;

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      FatalErrorMessage(EFatal.Message);
    on E: Exception do Application.MessageBox(StrPCopy(MsgText, E.Message), lsError,
        mb_Ok or mb_IconExclamation);
    end;

  ModalResult := mrOk;
  end;

//-----------------------------------------------------------------------------

procedure TFormFoundExport.UpdateProgress;

  begin
  LabelWhatDoing.Caption := lsProcessed + FormatNumber(DBaseExport.iFileCounter) +
                            lsFiles_Written + FormatSize(DBaseExport.iWritten, true);
  end;

//-----------------------------------------------------------------------------

procedure TFormFoundExport.ButtonCancelClick(Sender: TObject);

  begin
  bStopIt := true;
  end;

//-----------------------------------------------------------------------------

end.
