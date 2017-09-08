program DiskBase;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
  WinTypes,
{$ELSE}
  Interfaces,  LCLIntf, LCLType, LMessages,

{$ENDIF}
  Classes,
  Forms,
  FMain in 'FMain.pas' {MainForm},
  ///FDBase in 'FDBase.pas' {FormDBase},
  FSettings in 'FSettings.pas' {FormSettings},
  FSelectDrive in 'FSelectDrive.pas' {FormSelectDrive},
  FAskForLabel in 'FAskForLabel.pas' {FormAskForLabel},
  FScanProgress in 'FScanProgress.pas' {FormScanProgress},
  FDiskInfo in 'FDiskInfo.pas' {FormDiskInfo},
  FFindFile in 'FFindFile.pas' {FormSearchName},
  FFindFileDlg in 'FFindFileDlg.pas' {FormSearchFileDlg},
  FFoundFile in 'FFoundFile.pas' {FormFoundFileList},
  FProgress in 'FProgress.pas' {FormProgress},
  FFindEmptyDlg in 'FFindEmptyDlg.pas' {FormSearchEmptyDlg},
  FFindEmpty in 'FFindEmpty.pas' {FormSearchEmpty},
  FFoundEmpty in 'FFoundEmpty.pas' {FormFoundEmptyList},
  FRenameDlg in 'FRenameDlg.pas' {FormRenameDlg},
  FLocalOptions in 'FLocalOptions.pas' {FormLocalOptions},
  FDescription in 'FDescription.pas' {FormDescription},
  FReindex in 'FReindex.pas' {FormReindex},
  FMaskSelect in 'FMaskSelect.pas' {FormMaskSelect},
  FMoreOptions in 'FMoreOptions.pas' {FormMoreOptions},
  FDbInfo in 'FDbInfo.pas' {FormDBaseInfo},
  FAbortPrint in 'FAbortPrint.pas' {FormAbortPrint},
  FDiskPrint in 'FDiskPrint.pas' {FormDiskPrint},
  FDiskPrintSelect in 'FDiskPrintSelect.pas' {FormDiskPrintSelect},
  FScanArchive in 'FScanArchive.pas' {FormScanArchive},
  FAbout in 'FAbout.pas' {FormAbout},
  FSelectFolder in 'FSelectFolder.pas' {FormSelectFolder},
  FScanFolder in 'FScanFolder.pas' {FormScanFolder},
  FHidden in 'FHidden.pas' {HiddenForm},
  FRepair in 'FRepair.pas' {FormRepair},
  FDiskExport in 'FDiskExport.pas' {FormDiskExport},
  FFoundExport in 'FFoundExport.pas' {FormFoundExport},
  UUserDll in 'UUserDll.pas',
  UApiTypes in 'UApiTypes.pas',
  UBaseTypes in 'UBaseTypes.pas',
  UBaseUtils in 'UBaseUtils.pas',
  UCallbacks in 'UCallbacks.pas',
  UCollections in 'UCollections.pas',
  UCollectionsExt in 'UCollectionsExt.pas',
  UConvQDir4 in 'UConvQDir4.pas',
  UDebugLog in 'UDebugLog.pas',
  UDrives in 'UDrives.pas',
  UDskBUtl in 'UDskBUtl.pas',
  UEngineArchives in 'UEngineArchives.pas',
  UEngineArchivesExt in 'UEngineArchivesExt.pas',
  UEngineFileFind in 'UEngineFileFind.pas',
  UEngineMain in 'UEngineMain.pas',
  UExceptions in 'UExceptions.pas',
  UExport in 'UExport.pas',
  UFont in 'UFont.pas',
  ULang in 'ULang.pas',
  UPrinter in 'UPrinter.pas',
  UStream in 'UStream.pas',
  UStringList in 'UStringList.pas',
  UTypes in 'UTypes.pas',
  UUnAce in 'UUnAce.pas',
  UUnRar in 'UUnRar.pas',
  UApi in 'UApi.pas';

{$R *.res}

begin
try
  Application.Title := 'DiskBase 5 Linux';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormSelectDrive, FormSelectDrive);
  Application.CreateForm(TFormAskForLabel, FormAskForLabel);
  Application.CreateForm(TFormScanProgress, FormScanProgress);
  Application.CreateForm(TFormDiskInfo, FormDiskInfo);
  Application.CreateForm(TFormSearchName, FormSearchName);
  Application.CreateForm(TFormSearchFileDlg, FormSearchFileDlg);
  Application.CreateForm(TFormProgress, FormProgress);
  Application.CreateForm(TFormSearchEmptyDlg, FormSearchEmptyDlg);
  Application.CreateForm(TFormSearchEmpty, FormSearchEmpty);
  Application.CreateForm(TFormRenameDlg, FormRenameDlg);
  Application.CreateForm(TFormLocalOptions, FormLocalOptions);
  Application.CreateForm(TFormDescription, FormDescription);
  Application.CreateForm(TFormReindex, FormReindex);
  Application.CreateForm(TFormMaskSelect, FormMaskSelect);
  Application.CreateForm(TFormMoreOptions, FormMoreOptions);
  Application.CreateForm(TFormDBaseInfo, FormDBaseInfo);
  Application.CreateForm(TFormAbortPrint, FormAbortPrint);
  Application.CreateForm(TFormDiskPrint, FormDiskPrint);
  Application.CreateForm(TFormDiskPrintSelect, FormDiskPrintSelect);
  Application.CreateForm(TFormScanArchive, FormScanArchive);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormSelectFolder, FormSelectFolder);
  Application.CreateForm(TFormScanFolder, FormScanFolder);
  Application.CreateForm(THiddenForm, HiddenForm);
  Application.CreateForm(TFormRepair, FormRepair);
  Application.CreateForm(TFormDiskExport, FormDiskExport);
  Application.CreateForm(TFormFoundExport, FormFoundExport);
  Application.Run;

except
  on E: EReadError do
    begin
    LOG('EReadError Exception Error', []);
    if pos('Print', E.Message) > 0
      then MessageBox(0,
      'There is no default printer currently selected. If you do not have any printer installed, please install some printer now, even if you do not have any printer physically connected to your computer. DiskBase needs it for its proper run.',
      'DiskBase needs a printer to be installed in your Windows', MB_OK)
      else Raise;
    end;
  end;
end.
