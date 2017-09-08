unit FScanProgress;
(*====================================================================
Progress window for scanning the disk or importing from QuickDir 4 format.
Implements the scan in the Run procedure
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  Windows,
  {$ELSE}
    LCLIntf, LCLType, LMessages, ExtCtrls,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, {Gauges,}

  UTypes, UApiTypes;

type
  TFormScanProgress = class(TForm)
    LabelInfo: TLabel;
    ///Gauge: TGauge;
    Gauge: TPanel;
    ButtonCancel: TButton;
    LabelDoing: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CanRun     : boolean;
    function  ScanQDir4Database: boolean;
  public
    LastPhase    : Integer;
    CancelledByUser: boolean;
    Drive        : char;
    ScanningQDir4: boolean;
    DiskName     : ShortString;
    VolumeLabel  : ShortString;
    StartPath    : ShortString;
    DBaseHandle  : PDBaseHandle;
    Success      : boolean;
    procedure Run(var Info); message WM_User + 100;
    procedure DefaultHandler(var Message); override;
  end;

var
  FormScanProgress: TFormScanProgress;

implementation

uses UApi, UCallbacks, FScanArchive, ULang, FSettings;

var
  QLocalOptions : TLocalOptions;   {record}

{$R *.dfm}


//=============================================================================
// callback - support of Central Point Backup archives - not used now

function  IsTheLastCPB (FName: ShortString): boolean; far;
  begin
  {$ifdef CZECH}
  if QLocalOptions.ScanArchives <> cfNoScan
    then
      case QLocalOptions.ScanCPB of
        cfScan:   IsTheLastCPB := true;
        cfAsk:    IsTheLastCPB := Application.MessageBox(
                   lsIsItLastCPBDisk, lsCPBFound, MB_YESNOCANCEL or MB_ICONQUESTION)
                     = IDYES;
        else IsTheLastCPB := false;
        end
    else
  {$endif}
      IsTheLastCPB := false;
  end;

//-----------------------------------------------------------------------------
// Callback - optionally displays a dialog box asking, whther to scan or skip
// the contents of this archive

function  HowToContinue (FName: ShortString): word; far;

  begin
  Result := cmYesAll;
  case QLocalOptions.ScanArchives of
    cfScan:   Result := cmYes;
    cfAsk:    begin
              FormScanArchive.LabelMsg.Caption :=
                lsScanArchive + FName + '?';
              FormScanArchive.ShowModal;
              Result := FormScanArchive.Response;
              end;
    cfNoScan: Result := cmNo;
    end;
  end;

//-----------------------------------------------------------------------------
// Callback - new text to the text info

procedure NewLineToIndicator (Line: ShortString); far;

  begin
  FormScanProgress.LabelInfo.Caption := Line;
  Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// Callback - append text to the text info

procedure AppendLineToIndicator (Line: ShortString); far;

  begin
  FormScanProgress.LabelInfo.Caption := FormScanProgress.LabelInfo.Caption + Line;
  Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// Callback - updates the progress gauge

procedure UpdateProgressIndicator (Phase, Progress: Integer); far;

  begin
  if FormScanProgress.LastPhase <> Phase then
    begin
    FormScanProgress.LastPhase := Phase;
    case Phase of
      1: FormScanProgress.LabelDoing.Caption :=
                        lsScanningTree;
      2: FormScanProgress.LabelDoing.Caption :=
                        lsScanningFoldersAndFiles;
      else FormScanProgress.LabelDoing.Caption := '';
      end;
    if Phase = 2
      then FormScanProgress.Gauge.Show
      else FormScanProgress.Gauge.Hide;
    end;
  if Phase = 2 then
    ///FormScanProgress.Gauge.Progress := Progress;
  Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// Callback - returns true if the user presses the abort button

function  AbortScan: boolean; far;

  begin
  AbortScan := FormScanProgress.CancelledByUser;
  end;

//=============================================================================

procedure TFormScanProgress.FormShow(Sender: TObject);

  begin
  CancelledByUser := false;
  CanRun := true;
  Gauge.Hide;
  LastPhase := 0;
  // executes the Run procedure after the form is displayed
  PostMessage(Self.Handle, WM_User + 100, 0, 0);
  if StartPath = '' then StartPath := Drive + ':';
  end;

//-----------------------------------------------------------------------------

procedure TFormScanProgress.ButtonCancelClick(Sender: TObject);

  begin
  CancelledByUser := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanProgress.FormCreate(Sender: TObject);

  begin
  CanRun := false;
  ScanningQDir4 := false;
  end;

//-----------------------------------------------------------------------------

function TFormScanProgress.ScanQDir4Database: boolean;
  var
    TmpKey: ShortString;

  begin
  Result := false;
  while QI_ReadQDir4Entry(-1) do
    begin
    DiskName := QI_GetQDir4VolumeLabel;
    TmpKey := AnsiUpperCase(DiskName);
    if TmpKey = DiskName then
      begin
      DiskName := AnsiLowerCase(DiskName);
      DiskName[1] := TmpKey[1];
      end;
    QI_ClearTreeStruct(DBaseHandle);
    Success := QI_ScanQDir4Record(DBaseHandle, Drive + ':', DiskName);
    if Success
      then
        begin
        if not QI_AppendNewDBaseEntry(DBaseHandle) then // here is the change in the KeyField
          begin
          QI_ClearTreeStruct(DBaseHandle);
          Result := true;
          exit;
          end
        end
      else
        exit;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormScanProgress.Run (var Info);

  begin
  if not CanRun then exit;
  CanRun := false;
  QI_GetLocalOptions   (DBaseHandle, QLocalOptions);
  QI_RegisterCallbacks (IsTheLastCPB,
                        HowToContinue, NewLineToIndicator,
                        AppendLineToIndicator,
                        UpdateProgressIndicator,
                        AbortScan);
  try
    if ScanningQDir4
      then Success := ScanQDir4Database
      else Success := QI_ScanDisk(DBaseHandle, StartPath, DiskName, VolumeLabel);
  finally
    QI_UnregisterCallBacks;
    ModalResult := mrOk;
    end;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormScanProgress.DefaultHandler(var Message);

  begin
  with TMessage(Message) do
    if (Msg = g_CommonOptions.dwQueryCancelAutoPlay) and
        g_CommonOptions.bDisableCdAutorun
      then
        Result := 1
      else
        inherited DefaultHandler(Message)
  end;


end.
