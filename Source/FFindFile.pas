unit FFindFile;
(*====================================================================
Progress window for finding a file, implements the search in the
Run procedure
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Grids, ComCtrls, CustomDrawnControls, {Gauges,}
  UTypes, UApiTypes;

type

  { TFormSearchName }

  TFormSearchName = class(TForm)
    Gauge1: TCDProgressBar;
    Label1: TLabel;
    LabelSearched: TLabel;
    Label2: TLabel;
    LabelFound: TLabel;
    ButtonStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    CanRun     : boolean;
  public
    StopIt     : boolean;
    DBaseHandle: PDBaseHandle;
    procedure Run(var Info); message WM_User + 101;
    procedure UpdateCounters;
    procedure DefaultHandler(var Message); override;
  end;

var
  FormSearchName: TFormSearchName;

implementation

uses FFindFileDlg, UApi, UBaseTypes, ULang, FSettings, customdrawn_common;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure CallBackWhenChange(var Stop: boolean); far;

  begin
  FormSearchName.UpdateCounters;
  Application.ProcessMessages;
  Stop := FormSearchName.StopIt;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchName.FormCreate(Sender: TObject);

  begin
  CanRun := false;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchName.FormActivate(Sender: TObject);

  begin
  StopIt := false;
  CanRun := true;
  // This executes the Run procedure after the window is shown
  PostMessage(Self.Handle, WM_User + 101, 0, 0);
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchName.ButtonStopClick(Sender: TObject);

  begin
  StopIt := true;
  CanRun := false;
  if QI_SearchItemsAvail(DBaseHandle)
    then ModalResult := mrOK
    else ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchName.Run (var Info);

  var
    Success: boolean;

  begin
  if not CanRun then exit;
  CanRun := false;
  Success := false;
  try
    QI_ClearTreeStruct(DBaseHandle);
    QI_ClearSearchCol (DBaseHandle);
    QI_SetSearchCallback(DBaseHandle, CallBackWhenChange);
    QI_FindFiles(DBaseHandle, FormSearchFileDlg.DlgData);
    QI_DelSearchCallback(DBaseHandle);
    Success := true;
    if not QI_SearchItemsAvail(DBaseHandle) then
      begin
        Application.MessageBox(lsNoFileFound,
                               lsInformation, mb_OK or mb_IconInformation);
      Success := false;
      end;
  finally
    if Success
      then ModalResult := mrOk
      else ModalResult := mrCancel;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchName.UpdateCounters;

  var
    Percent: Integer;
    DisksCount, DirsCount, FilesCount: longint;

  begin
  QI_GetSearchProgress (DBaseHandle, Percent, DisksCount, DirsCount, FilesCount);
  LabelSearched.Caption := IntToStr(DisksCount) + '/' + IntToStr(FilesCount);
  LabelFound.Caption := IntToStr(QI_GetFoundCount (DBaseHandle));
  ///Gauge1.Progress := Percent;
  Gauge1.Position:=Percent;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormSearchName.DefaultHandler(var Message);

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
