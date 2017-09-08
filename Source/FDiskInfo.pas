unit FDiskInfo;
(*====================================================================
Dialog box with info about selected disk in the database.
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
  Forms, Dialogs, StdCtrls, ExtCtrls,
  UApiTypes;

type
  TFormDiskInfo = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label0: TLabel;
    LabelScanDate: TLabel;
    LabelDirs: TLabel;
    LabelFiles: TLabel;
    LabelArchives: TLabel;
    LabelDiskName: TLabel;
    LabelDataSize: TLabel;
    LabelDiskSize: TLabel;
    LabelDiskFree: TLabel;
    ButtonOK: TButton;
    Label5: TLabel;
    Label6: TLabel;
    LabelVolumeLabel: TLabel;
    LabelOriginalFolder: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    LabelArcFiles: TLabel;
    LabelArcDirs: TLabel;
    LabelArcSize: TLabel;
    LabelTotSize: TLabel;
    ButtonHelp: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetValues(var TreeInfo: TTreeInfo);
    procedure DefaultHandler(var Message); override;
  end;

var
  FormDiskInfo: TFormDiskInfo;

implementation

{$R *.dfm}

uses UBaseUtils, FSettings;

//-----------------------------------------------------------------------------

procedure TFormDiskInfo.SetValues(var TreeInfo: TTreeInfo);

  var
    TmpComp : comp;

  begin
  with TreeInfo do
    begin
    LabelScanDate.Caption := DosDateToStr(ScanDate) + '  ' + DosTimeToStr(ScanDate, false);

    LabelDirs.Caption     := FormatNumber(Dirs);
    LabelFiles.Caption    := FormatNumber(Files);
    LabelArchives.Caption := FormatNumber(Archives);
    LabelArcDirs.Caption  := FormatNumber(ArcDirs);
    LabelArcFiles.Caption := FormatNumber(ArcFiles);

    LabelDataSize.Caption := FormatBigSize(PhysSizeBytes);
    LabelArcSize.Caption  := FormatBigSize(ArchSizeBytes);
    LabelTotSize.Caption  := FormatBigSize(DataSizeBytes);

    TmpComp := DiskSizeKb;
    LabelDiskSize.Caption := FormatBigSize(TmpComp * 1024);
    TmpComp := DiskFreeKb;
    LabelDiskFree.Caption := FormatBigSize(TmpComp * 1024);

    LabelDiskName.Caption := Name;
    LabelVolumeLabel.Caption := VolumeLabel;
    if OriginPath <> ''
      then LabelOriginalFolder.Caption := OriginPath
      else LabelOriginalFolder.Caption := AnsiUpperCase(OrigDrive[1] + ':');
    end;
  end;


//-----------------------------------------------------------------------------

procedure TFormDiskInfo.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormDiskInfo.ButtonHelpClick(Sender: TObject);

  begin
  Application.HelpContext(280);
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormDiskInfo.DefaultHandler(var Message);

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
