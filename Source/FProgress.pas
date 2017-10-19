unit FProgress;
(*====================================================================
Universal progress window - used when resorting window with found items
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages, ExtCtrls,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, {Gauges,} StdCtrls,
  UTypes, ATGauge;

type

  { TFormProgress }

  TFormProgress = class(TForm)
    ButtonStop: TButton;
    Gauge1: TGauge;
    procedure ButtonStopClick(Sender: TObject);
  private
    { Private declarations }
  public
    StopIt : boolean;
    procedure ResetAndShow (Title: ShortString);
    procedure SetProgress(Percent: Integer);
    procedure DefaultHandler(var Message); override;
  end;

var
  FormProgress: TFormProgress;

implementation

uses FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormProgress.ResetAndShow (Title: ShortString);

  begin
  Gauge1.Progress := 0;
  Caption := Title;
  StopIt  := false;
  Show;
  end;

//-----------------------------------------------------------------------------

procedure TFormProgress.SetProgress(Percent: Integer);

  begin
  if Percent > Gauge1.Progress then
    begin
    Gauge1.Progress := Percent;
    Application.ProcessMessages;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormProgress.ButtonStopClick(Sender: TObject);

  begin
  StopIt := true;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormProgress.DefaultHandler(var Message);

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
