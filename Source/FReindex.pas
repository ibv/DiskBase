unit FReindex;
(*====================================================================
Reindex progress window. Implements reindex in the Run method.
Used for compressing database, exporting to run-time, exporting and
importing to and from DiskBase format
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages, ComCtrls, ExtCtrls,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, {Gauges,}
  UTypes, UApiTypes;

type
  TFormReindex = class(TForm)
    ///Gauge       : TGauge;
    Gauge       : TPanel;
    ButtonCancel: TButton;
    LabelInfo   : TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CanRun         : boolean;
  public
    SrcDBaseHandle : PDBaseHandle;
    TarDBaseHandle : PDBaseHandle;
    TarDatabaseName: ShortString;
    SrcDatabaseName: ShortString;
    StopIt         : boolean;
    ProcessType    : (ptReindex, ptRuntime, ptExport, ptImport);
    procedure Run(var Info); message WM_User + 102;
  end;

var
  FormReindex: TFormReindex;

implementation

uses
  UApi, UExceptions, FSettings, ULang;

{$R *.dfm}

//-----------------------------------------------------------------------------
// dummy callback

function IsTheLastCPB (FName: ShortString): boolean; far;

  begin
  IsTheLastCPB := true;
  end;

//-----------------------------------------------------------------------------
// dummy callback

function  HowToContinue (FName: ShortString): word; far;

  begin
  HowToContinue := 0;
  end;

//-----------------------------------------------------------------------------
// callback - displays a new line

procedure NewLineToIndicator (Line: ShortString); far;

  begin
  FormReindex.LabelInfo.Caption := Line;
  Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// callback - extends the displayed line

procedure AppendLineToIndicator (Line: ShortString); far;

  begin
  FormReindex.LabelInfo.Caption := FormReindex.LabelInfo.Caption + Line;
  Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// callback - updates progress indicator

procedure UpdateProgressIndicator (Phase, Progress: Integer); far;

  begin
  ///if (FormReindex.Gauge.Progress <> Progress) then
  ///  FormReindex.Gauge.Progress := Progress;
  ///Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// callback - returns true if the user clicked on the Abort button

function  AbortScan: boolean; far;

  begin
  AbortScan := FormReindex.StopIt;
  end;

//-----------------------------------------------------------------------------
// Sets the appropriate caption and invokes the Run method

procedure TFormReindex.FormShow(Sender: TObject);

  begin
  case ProcessType of
    ptReindex    : Caption := lsCompressingDatabase;
    ptRuntime    : Caption := lsExportingToRunTime;
    ptExport     : Caption := lsExporting;
    ptImport     : Caption := lsImporting;
    end;
  StopIt := false;
  CanRun := true;
  LabelInfo.Caption := '';
  // this starts the Run method after the form is displayed
  PostMessage(Self.Handle, WM_User + 102, 0, 0);
  end;

//-----------------------------------------------------------------------------
// Goes through the database, skips deleted records and copies the selected
// records to the output database

procedure TFormReindex.Run (var Info);

  var
    Success: boolean;
    BakDatabaseName: ShortString;
    MsgCaption, MsgText: array[0..256] of char;
    sMessage: ShortString;

  begin
  if not CanRun then exit;
  CanRun := false;
  try
    case ProcessType of
      ptReindex: StrPCopy(MsgCaption, lsCompressNotSuccessful);
      ptRunTime: StrPCopy(MsgCaption, lsExportNotSuccessful);
      ptExport : StrPCopy(MsgCaption, lsExportNotSuccessful);
      ptImport : StrPCopy(MsgCaption, lsImportNotSuccessful);
      end;

    if (ProcessType <> ptExport) and (ProcessType <> ptRunTime) then
      begin
      if not QI_OpenDatabase(SrcDatabaseName, false, SrcDBaseHandle, sMessage) then
        begin
        Application.MessageBox(StrPCopy(MsgText, lsSourceDBase +
             SrcDatabaseName + lsCannotBeOpened + #13#10 + sMessage),
             MsgCaption, mb_OK or mb_IconStop);
        ModalResult := mrCancel;
        exit;
        end;
      end;

    if ProcessType = ptReindex then
      TarDatabaseName := ChangeFileExt(SrcDatabaseName, '.$$~');
    if ProcessType <> ptImport then
      begin
      if not QI_CreateDatabase(TarDatabaseName) then
        begin
        Application.MessageBox(StrPCopy(MsgText, lsTargetDBase +
                               TarDatabaseName + lsCannotBeCreated),
                               MsgCaption, mb_OK or mb_IconStop);
        QI_CloseDatabase(SrcDBaseHandle, false);
        ModalResult := mrCancel;
        exit;
        end;
      if not QI_OpenDatabase(TarDatabaseName, false, TarDBaseHandle, sMessage) then
        begin
        Application.MessageBox(StrPCopy(MsgText, lsTargetDBase +
                               TarDatabaseName + lsCannotBeOpened + #13#10 + sMessage),
                               MsgCaption, mb_OK or mb_IconStop);
        QI_CloseDatabase(SrcDBaseHandle, false);
        ModalResult := mrCancel;
        exit;
        end;
      end;

    QI_RegisterCallbacks (IsTheLastCPB,
                          HowToContinue, NewLineToIndicator,
                          AppendLineToIndicator,
                          UpdateProgressIndicator,
                          AbortScan);

    Success := QI_CopyDatabase(SrcDBaseHandle, TarDBaseHandle,
           (ProcessType <> ptExport) and (ProcessType <> ptRunTime), {true = whole database}
            ProcessType = ptImport, ProcessType = ptReindex); {true = check duplicates}

    QI_UnregisterCallBacks;

    if (ProcessType <> ptExport) and (ProcessType <> ptRunTime)
      then QI_CloseDatabase(SrcDBaseHandle, false);
    if ProcessType <> ptImport then
      QI_CloseDatabase(TarDBaseHandle, ProcessType = ptRunTime);

    if ProcessType = ptReindex then
      begin
      if not Success then
        begin
        SysUtils.DeleteFile(TarDatabaseName);
        Application.MessageBox(lsProcessNotSuccessful,
                                MsgCaption, mb_OK or mb_IconStop);
        ModalResult := mrCancel;
        exit;
        end;

      if FormSettings.CheckBoxBackupDBase.Checked
        then
          begin
          BakDatabaseName := ChangeFileExt(SrcDatabaseName, '.~QD');
          if FileExists(BakDatabaseName) then SysUtils.DeleteFile(BakDatabaseName);
          if not RenameFile(SrcDatabaseName, BakDatabaseName) then
            begin
            Application.MessageBox(StrPCopy(MsgText, lsDbase +
                                   SrcDatabaseName + lsCannotBeRenamedTo +
                                   BakDatabaseName), MsgCaption, mb_OK or mb_IconStop);
            ModalResult := mrCancel;
            exit;
            end;
          end
        else
          begin
          if not SysUtils.DeleteFile(SrcDatabaseName) then
            begin
            Application.MessageBox(StrPCopy(MsgText, lsOriginalDBase +
                                   SrcDatabaseName + lsCannotBeDeleted),
                                   MsgCaption, mb_OK or mb_IconStop);
            ModalResult := mrCancel;
            exit;
            end;
          end;

      if not RenameFile(TarDatabaseName, SrcDatabaseName) then
        begin
        Application.MessageBox(StrPCopy(MsgText, lsDBase +
                               TarDatabaseName + lsCannotBeRenamedTo +
                               SrcDatabaseName), MsgCaption, mb_OK or mb_IconStop);
        ModalResult := mrCancel;
        exit;
        end;
      end;
  finally
    ModalResult := mrOk;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormReindex.FormCreate(Sender: TObject);

  begin
  CanRun := false;
  end;

//-----------------------------------------------------------------------------

procedure TFormReindex.ButtonCancelClick(Sender: TObject);

  begin
  StopIt := true;
  end;

//-----------------------------------------------------------------------------

end.
