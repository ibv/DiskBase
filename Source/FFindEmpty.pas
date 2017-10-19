unit FFindEmpty;
(*====================================================================
Progress window for searching disks according to the size of disks,
implements the searching in the Run procedure
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
  Forms, Dialogs, StdCtrls, ExtCtrls, Grids, ATGauge,
  UApiTypes, UTypes;

type

  { TFormSearchEmpty }

  TFormSearchEmpty = class(TForm)
    ButtonStop: TButton;
    Gauge1: TGauge;
    //Gauge1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    CanRun     : boolean;
  public
    StopIt     : boolean;
    DBaseHandle: PDBaseHandle;
    procedure Run(var Info); message WM_User + 102;
    procedure UpdateCounters;
  end;

var
  FormSearchEmpty: TFormSearchEmpty;

implementation

uses FFindEmptyDlg, UApi, UBaseTypes, ULang;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure CallBackWhenChange(var Stop: boolean); far;

  begin
  FormSearchEmpty.UpdateCounters;
  Application.ProcessMessages;
  Stop := FormSearchEmpty.StopIt;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchEmpty.FormCreate(Sender: TObject);

  begin
  CanRun := false;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchEmpty.FormActivate(Sender: TObject);

  begin
  StopIt := false;
  CanRun := true;
  // this causes to execure the Run procedure after the window is displayed
  PostMessage(Self.Handle, WM_User + 102, 0, 0);
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchEmpty.ButtonStopClick(Sender: TObject);

  begin
  StopIt := true;
  CanRun := false;
  if QI_SearchItemsAvail(DBaseHandle)
    then ModalResult := mrOK
    else ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormSearchEmpty.Run (var Info);

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
    QI_FindEmpties(DBaseHandle);
    QI_DelSearchCallback(DBaseHandle);
    Success := true;
    if not QI_SearchItemsAvail(DBaseHandle) then
      begin
        Application.MessageBox(lsNoDiskFound,
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

procedure TFormSearchEmpty.UpdateCounters;

  var
    Percent: Integer;
    DisksCount, DirsCount, FilesCount: longint;

  begin
  QI_GetSearchProgress (DBaseHandle, Percent, DisksCount, DirsCount, FilesCount);
  Gauge1.Progress := Percent;
  end;

//-----------------------------------------------------------------------------


end.
