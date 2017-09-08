unit FDiskExport;
(*====================================================================
Progress info window for exporting disks to text format
======================================================================*)

interface

uses
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  UApiTypes, UStringList;

type
  TFormDiskExport = class(TForm)
    ButtonCancel: TButton;
    LabelWhatDoing: TLabel;
    SaveDialogExportToText: TSaveDialog;
    OpenDialogExportFormat: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    bCanRun       : boolean;
    ///FilesList     : TQStringList; {for capturing and sorting files}
    FilesList     : TStringList; {for capturing and sorting files}
  public
    bStopIt       : boolean;
    DBaseHandle   : PDBaseHandle;
    procedure Run(var Info); message WM_User;
    procedure UpdateProgress;
    procedure OnDatabaseBegin;
    procedure OnDatabaseEnd;
    procedure OnDiskBegin;
    procedure OnDiskEnd;
    procedure OnFolderBegin;
    procedure OnFolderEnd;
    procedure OnFirstColumnBegin;
    procedure OnEachColumnBegin;
    procedure OnFile;
    procedure OnEachColumnEnd;
    procedure OnLastColumnEnd;
  end;

var
  FormDiskExport: TFormDiskExport;

implementation

uses UAPi, UExport, ULang, UExceptions, FDiskPrintSelect, FSettings, UBaseUtils;

{$R *.dfm}

//---CallBack Functions--------------------------------------------------------
// We use pointer to functions, so that they cannot be the methods of the object.
// Thus, these callback functions pass the call to the appropriate method

procedure CallBackWhenChange(var Stop: boolean);

  begin
  FormDiskExport.UpdateProgress;
  Application.ProcessMessages;
  Stop := FormDiskExport.bStopIt;
  end;

//-----------------------------------------------------------------------------

procedure OnDatabaseBeginNotify;
  begin
  FormDiskExport.OnDatabaseBegin;
  end;

//-----------------------------------------------------------------------------

procedure OnDatabaseEndNotify;
  begin
  FormDiskExport.OnDatabaseEnd;
  end;

//-----------------------------------------------------------------------------

procedure OnDiskBeginNotify;
  begin
  FormDiskExport.OnDiskBegin;
  end;

//-----------------------------------------------------------------------------

procedure OnDiskEndNotify;
  begin
  FormDiskExport.OnDiskEnd;
  end;

//-----------------------------------------------------------------------------

procedure OnFolderBeginNotify;
  begin
  FormDiskExport.OnFolderBegin;
  end;

//-----------------------------------------------------------------------------

procedure OnFolderEndNotify;
  begin
  FormDiskExport.OnFolderEnd;
  end;

//-----------------------------------------------------------------------------

procedure OnFirstColumnBeginNotify;
  begin
  FormDiskExport.OnFirstColumnBegin;
  end;

//-----------------------------------------------------------------------------

procedure OnEachColumnBeginNotify;
  begin
  FormDiskExport.OnEachColumnBegin;
  end;

//-----------------------------------------------------------------------------

procedure OnFileNotify;
  begin
  FormDiskExport.OnFile;
  end;

//-----------------------------------------------------------------------------

procedure OnEachColumnEndNotify;
  begin
  FormDiskExport.OnEachColumnEnd;
  end;

//-----------------------------------------------------------------------------

procedure OnLastColumnEndNotify;
  begin
  FormDiskExport.OnLastColumnEnd;
  end;

//---TFormDiskExport-----------------------------------------------------------

procedure TFormDiskExport.FormCreate(Sender: TObject);

  begin
  bCanRun := false;
  OpenDialogExportFormat.InitialDir := ExtractFilePath(ParamStr(0)) + lsExportFolder;
  SaveDialogExportToText.InitialDir := ExtractFilePath(ParamStr(0)) + lsExportFolder;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.FormShow(Sender: TObject);

  begin
  bStopIt := false;
  bCanRun := true;
  LabelWhatDoing.Caption := lsPreparingToExport;
  // Posts message to self - this assures the window is displayed before
  // the export starts. The WM_User message is handled by the Run()
  PostMessage(Self.Handle, WM_User, 0, 0);
  end;

//-----------------------------------------------------------------------------
// Called by WM_User message - makes the export, by calling the engine - the
// engine calls the callback functions.

procedure TFormDiskExport.Run (var Info);

  var
    MsgText: array[0..256] of char;
    SearchIn: TSearchIn;

  begin
  if not bCanRun then exit;
  bCanRun := false;
  bStopIt := false;
  ///FilesList            := TQStringList.Create;
  FilesList            := TStringList.Create;
  ///FilesList.Sorted     := true;
  ///FilesList.Duplicates := qdupAccept;
  FormSettings.UpdateGlobalFormatSettings;

  try
    FormDiskPrintSelect.RadioButtonSelectedDisks.Enabled :=
      QI_GetSelectedCount (DBaseHandle) > 0;
    FormDiskPrintSelect.Caption := lsExportSelection;
    FormDiskPrintSelect.LabelWhat.Caption := lsExportWhat;
    if FormDiskPrintSelect.ShowModal <> mrOk then
      begin
      FreeObjects(FilesList);
      FilesList.Free;
      ModalResult := mrCancel;
      exit;
      end;
    with FormDiskPrintSelect do
      begin
      SearchIn := siActualDiskDir;
      if RadioButtonActDiskWhole.Checked  then SearchIn := siActualDisk;
      if RadioButtonSelectedDisks.Checked then SearchIn := siSelectedDisks;
      end;

    OpenDialogExportFormat.Filter := lsFilterDatabase;
    if not OpenDialogExportFormat.Execute then
      begin
      FreeObjects(FilesList);
      FilesList.Free;
      ModalResult := mrCancel;
      exit;
      end;
    OpenDialogExportFormat.InitialDir := ExtractFilePath(OpenDialogExportFormat.FileName);

    DBaseExport.Init(OpenDialogExportFormat.FileName, DBaseHandle);
    if (not DBaseExport.bFormatVerified) then
      begin
      NormalErrorMessage(lsNotValidFormatFile);
      FreeObjects(FilesList);
      FilesList.Free;
      ModalResult := mrCancel;
      exit;
      end;

    SaveDialogExportToText.DefaultExt := DBaseExport.sDefaultExt;
    if DBaseExport.sFileFilter <> ''
      then SaveDialogExportToText.Filter := DBaseExport.sFileFilter + '|' + lsDefaultFilter
      else SaveDialogExportToText.Filter := lsDefaultFilter;

    if not SaveDialogExportToText.Execute then
      begin
      FreeObjects(FilesList);
      FilesList.Free;
      ModalResult := mrCancel;
      exit;
      end;
    SaveDialogExportToText.InitialDir := ExtractFilePath(SaveDialogExportToText.FileName);

    DBaseExport.OpenOutputFile(SaveDialogExportToText.FileName);

    QI_SetNotifyProcs (DBaseHandle,
                       CallBackWhenChange,
                       OnDatabaseBeginNotify,
                       OnDatabaseEndNotify,
                       OnDiskBeginNotify,
                       OnDiskEndNotify,
                       OnFolderBeginNotify,
                       OnFolderEndNotify,
                       OnFileNotify);
    QI_IterateFiles   (DBaseHandle, SearchIn, stDataNotify);
    QI_DelNotifyProcs (DBaseHandle);

    DBaseExport.CloseOutputFile;

  except
    on ENormal: EQDirNormalException do
      NormalErrorMessage(ENormal.Message);
    on EFatal : EQDirFatalException do
      FatalErrorMessage(EFatal.Message);
    on E: Exception do Application.MessageBox(StrPCopy(MsgText, E.Message), lsError,
        mb_Ok or mb_IconExclamation);
    end;

  FreeObjects(FilesList);
  FilesList.Free;
  ModalResult := mrOk;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.UpdateProgress;

  begin
  LabelWhatDoing.Caption := lsProcessed + FormatNumber(DBaseExport.iFileCounter) +
                            lsFiles_Written + FormatSize(DBaseExport.iWritten, true);
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnDatabaseBegin;

  begin
  DBaseExport.OnDatabaseBegin;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnDatabaseEnd;

  begin
  DBaseExport.OnDatabaseEnd;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnDiskBegin;

  begin
  DBaseExport.OnDiskBegin;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnDiskEnd;

  begin
  DBaseExport.OnDiskEnd;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnFolderBegin;

  begin
  DBaseExport.OnFolderBegin;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnFolderEnd;

  begin
  DBaseExport.OnFolderEnd;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnFirstColumnBegin;

  begin
  DBaseExport.OnFirstColumnBegin;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnEachColumnBegin;

  begin
  DBaseExport.OnEachColumnBegin;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnFile;

  begin
  DBaseExport.OnFile;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnEachColumnEnd;

  begin
  DBaseExport.OnEachColumnEnd;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.OnLastColumnEnd;

  begin
  DBaseExport.OnLastColumnEnd;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskExport.ButtonCancelClick(Sender: TObject);

  begin
  bStopIt := true;
  end;

//-----------------------------------------------------------------------------





end.
