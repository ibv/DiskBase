unit FDbInfo;
(*====================================================================
Dialog box displaying information about the database.
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
  UTypes, UApiTypes;

type
  TFormDBaseInfo = class(TForm)
    ButtonOK: TButton;
    LabelDBaseName: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelSize: TLabel;
    LabelRecords: TLabel;
    LabelRecordsDeleted: TLabel;
    LabelReindex: TLabel;
    Label6: TLabel;
    LabelReadOnly: TLabel;
    procedure ButtonOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetValues(var DBaseInfo: TDBaseInfo; DBaseName: ShortString);
    procedure DefaultHandler(var Message); override;
  end;

var
  FormDBaseInfo: TFormDBaseInfo;

implementation

uses UBaseUtils, ULang, FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormDBaseInfo.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormDBaseInfo.SetValues(var DBaseInfo: TDBaseInfo; DBaseName: ShortString);

  begin
  LabelRecords.Caption        := IntToStr(DBaseInfo.Count);
  LabelRecordsDeleted.Caption := IntToStr(DBaseInfo.Count-DBaseInfo.CountValid);
  LabelSize.Caption           := FormatSize(DBaseInfo.DBaseSize, true);
  if (DBaseInfo.Count > 0) and ((DBaseInfo.CountValid/DBaseInfo.Count) < 0.8) or
    ((DBaseInfo.Count-DBaseInfo.CountValid) > 100)
    then LabelReindex.Caption := lsYes
    else LabelReindex.Caption := lsNo;
  if DBaseInfo.ReadOnly
    then LabelReadOnly.Caption := lsYes
    else LabelReadOnly.Caption := lsNo;
  LabelDBaseName.Caption := DbaseName;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormDBaseInfo.DefaultHandler(var Message);

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
