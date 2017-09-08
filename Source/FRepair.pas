unit FRepair;
(*====================================================================
Progress window for repairing the database. Implements the repair in the
Run procedure
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
  TFormRepair = class(TForm)
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
    procedure Run(var Info); message WM_User + 110;
  end;

var
  FormRepair: TFormRepair;

implementation

uses
  UAPi, UExceptions, FSettings, ULang;

{$R *.dfm}

//-----------------------------------------------------------------------------
// dummy callback

function  IsTheLastCPB (FName: ShortString): boolean; far;
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
// callback - sets the new text in the progress info

procedure NewLineToIndicator (Line: ShortString); far;

  begin
  FormRepair.LabelInfo.Caption := Line;
  Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// callback - adds text to the text in the progress info

procedure AppendLineToIndicator (Line: ShortString); far;

  begin
  FormRepair.LabelInfo.Caption := FormRepair.LabelInfo.Caption + Line;
  Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// callback - updates progress gauge

procedure UpdateProgressIndicator (Phase, Progress: Integer); far;

  begin
  ///if (FormRepair.Gauge.Progress <> Progress) then
  ///  FormRepair.Gauge.Progress := Progress;
  ///Application.ProcessMessages;
  end;

//-----------------------------------------------------------------------------
// callback - returns true if the user presses Cancel

function  AbortScan: boolean; far;

  begin
  AbortScan := FormRepair.StopIt;
  end;

//=============================================================================

procedure TFormRepair.FormShow(Sender: TObject);

  begin
  LabelInfo.Caption := lsScannningDatabase;
  StopIt := false;
  CanRun := true;
  Application.ProcessMessages;
  // this causes executing the Run procedure after the form is displayed
  PostMessage(Self.Handle, WM_User + 110, 0, 0);
  end;

//-----------------------------------------------------------------------------
// Makes the repair of the database

procedure TFormRepair.Run (var Info);

  var
    MsgCaption, MsgText: array[0..256] of char;
    sMessage: ShortString;

  begin
  if not CanRun then exit;
  CanRun := false;
  StrPCopy(MsgCaption, lsDatabaseRepair);
  LabelInfo.Caption := lsRepairingIndex;
  try
    if not QI_OpenDatabase(SrcDatabaseName, true, SrcDBaseHandle, sMessage) then
      begin
      Application.MessageBox(StrPCopy(MsgText, lsSourceDBase +
           SrcDatabaseName + lsCannotBeOpened + #13#10 + sMessage),
           MsgCaption, mb_OK or mb_IconStop);
      ModalResult := mrCancel;
      exit;
      end;

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

    QI_RegisterCallbacks (IsTheLastCPB,
                          HowToContinue, NewLineToIndicator,
                          AppendLineToIndicator,
                          UpdateProgressIndicator,
                          AbortScan);

    QI_CopyDatabase(SrcDBaseHandle, TarDBaseHandle,
            true,  {whole database}
            false, {do not check duplicates}
            true); {copy local options}

    QI_UnregisterCallBacks;

    //iErrorCounter := QI_GetErrorCounter(SrcDBaseHandle);

    QI_CloseDatabase(SrcDBaseHandle, false);
    QI_CloseDatabase(TarDBaseHandle, false);

  finally
    ModalResult := mrOk;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormRepair.FormCreate(Sender: TObject);

  begin
  CanRun := false;
  end;

//-----------------------------------------------------------------------------

procedure TFormRepair.ButtonCancelClick(Sender: TObject);

  begin
  StopIt := true;
  end;

//-----------------------------------------------------------------------------

end.
