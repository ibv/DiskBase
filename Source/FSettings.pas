unit FSettings;
(*====================================================================
Dialog box for DiskBase general settings
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  WinTypes,WinProcs,
  {$ELSE}
    LCLIntf, LCLType, LMessages,ComCtrls,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, {TabNotBk,} Spin, Grids,
  UTypes, UApiTypes
{$ifdef LOGFONT}
  , ComCtrls
  , UFont {my font dialog box}
{$endif}
  ;

const
  clQSelectedBack = clGray;
  clQSelectedText = clHighlightText;
  clQDeletedText  = clGray;

type
  TPanelHeaderWidths = array[0..2] of Integer;

  TUserCommand = record
    m_wKey      : Word;
    m_bShift    : boolean;
    m_bControl  : boolean;
    m_bAlt      : boolean;
    m_sDll      : AnsiString;
    m_sParams   : AnsiString;
    m_bTestExist: boolean;
    end;
  TPUserCommand = ^TUserCommand;

  TGlobalOptions = class
  { -Display tab- }
    ShowTree        : boolean;
    ExpandTree      : boolean;

    SortCrit        : TSortCrit;
    ShowInKb        : boolean; {RadioGroupSizeDisplay}

    FileDisplayType : TFileDisplayType;
    ShowSize        : boolean;
    ShowTime        : boolean;
    ShowDescr       : boolean;

    ShowIcons       : boolean;
    ShowSeconds     : boolean;
    SystemDateFormat: boolean;
    OneKbIs1024     : boolean;
    ShowFileHints   : boolean;

    ReversedSort    : boolean; {neni nikde zobrazovano, ani ukladano}

{ -Fonts tab- }
{$ifndef LOGFONT}
    DiskFont        : TFont;
    TreeFont        : TFont;
    FileFont        : TFont;
    FoundFont       : TFont;
    DescFont        : TFont;
    PrintFont       : TFont;
{$else}
    DiskLogFont     : TLogFont;
    TreeLogFont     : TLogFont;
    FileLogFont     : TLogFont;
    FoundLogFont    : TLogFont;
    DescLogFont     : TLogFont;
    PrintLogFont    : TLogFont;

    DiskLogFontSize : Integer;
    TreeLogFontSize : Integer;
    FileLogFontSize : Integer;
    FoundLogFontSize: Integer;
    DescLogFontSize : Integer;
    PrintLogFontSize: Integer;
{$endif}

{ -Print tab- }
    PrintHeader     : ShortString;
    AdjustNameWidth : boolean;
    TopMargin       : Integer;
    BottomMargin    : Integer;
    LeftMargin      : Integer;
    RightMargin     : Integer;

{ -Other tab- }
    AutoLoadDBase     : ShortString;
    OpenLastOpened    : boolean;
    FoundToNewWin     : boolean;
    AutoSave          : boolean;
    BackupDBase       : boolean;
    PersistentBlocks  : boolean;
    NotScanDriveNames : boolean;
    EnableShellExecute: boolean;
    PanelHeaderWidths : TPanelHeaderWidths;

    EnableUTFConvert  : boolean;

    constructor Create;
    destructor  Destroy; override;
    end;


  { TFormSettings }

  TFormSettings = class(TForm)
    CheckBoxEnableUTFConvert: TCheckBox;
    GroupBoxMargins: TGroupBox;
    ///TabbedNotebook: TTabbedNotebook;
    TabbedNotebook: TPageControl;
    Page1: TTabSheet;
    Page2: TTabSheet;
    Page3: TTabSheet;
    Page4: TTabSheet;
    Page5: TTabSheet;
    ButtonOK: TButton;
    RadioGroupSortCrit: TRadioGroup;
    GroupBoxFileDisplay: TGroupBox;
    RadioButtonBrief: TRadioButton;
    RadioButtonDetailed: TRadioButton;
    CheckBoxShowSize: TCheckBox;
    CheckBoxShowTime: TCheckBox;
    CheckBoxShowDescr: TCheckBox;
    FontDialogDisk: TFontDialog;
    FontDialogTree: TFontDialog;
    FontDialogFile: TFontDialog;
    FontDialogFound: TFontDialog;
    RadioGroupSizeDisplay: TRadioGroup;
    CheckBoxShowIcons: TCheckBox;
    GroupBoxTree: TGroupBox;
    CheckBoxExpandTree: TCheckBox;
    CheckBoxShowTree: TCheckBox;
    ButtonCancel: TButton;
    SpinEditTop: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SpinEditBottom: TSpinEdit;
    SpinEditLeft: TSpinEdit;
    SpinEditRight: TSpinEdit;
    ButtonChangePrintFont: TButton;
    FontDialogPrint: TFontDialog;
    LabelPrintFont: TLabel;
    EditPrintHeader: TEdit;
    Label5: TLabel;
    OpenDialogDBase: TOpenDialog;
    ButtonHelp: TButton;
    ButtonChangeDiskFont: TButton;
    LabelDiskFont: TLabel;
    Label8: TLabel;
    LabelTreeFont: TLabel;
    ButtonChangeTreeFont: TButton;
    Label9: TLabel;
    LabelFileFont: TLabel;
    ButtonChangeFileFont: TButton;
    Label10: TLabel;
    ButtonChangeFoundFont: TButton;
    LabelFoundFont: TLabel;
    Label11: TLabel;
    ButtonChangeDescFont: TButton;
    LabelDescFont: TLabel;
    Label12: TLabel;
    FontDialogDesc: TFontDialog;
    EditAutoLoadDBase: TEdit;
    Label6: TLabel;
    ButtonBrowse: TButton;
    CheckBoxShowSeconds: TCheckBox;
    CheckBoxWinDateFormat: TCheckBox;
    CheckBoxOneKbIs1024: TCheckBox;
    CheckBoxBackupDBase: TCheckBox;
    CheckBoxFoundToNewWin: TCheckBox;
    CheckBoxAutoSave: TCheckBox;
    GroupBoxPrintFont: TGroupBox;
    CheckBoxNotScanDriveNames: TCheckBox;
    CheckBoxShowFileHints: TCheckBox;
    CheckBoxPersistentBlocks: TCheckBox;
    CheckBoxOpenLastOpened: TCheckBox;
    CheckBoxAdjustNameWidth: TCheckBox;
    CheckBoxEnableShellExecute: TCheckBox;
    CheckBoxDisableCdAutoRun: TCheckBox;
    CheckBoxEjectAfterCdScan: TCheckBox;
    CheckBoxScanAfterCdInsert: TCheckBox;
    CheckBoxNoQueries: TCheckBox;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure RadioButtonBriefClick(Sender: TObject);
    procedure RadioButtonDetailedClick(Sender: TObject);
    procedure ButtonChangeDiskFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonChangeTreeFontClick(Sender: TObject);
    procedure ButtonChangeFileFontClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonChangeFoundFontClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxShowTreeClick(Sender: TObject);
    procedure ButtonChangePrintFontClick(Sender: TObject);
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure ButtonChangeDescFontClick(Sender: TObject);
    procedure CheckBoxOpenLastOpenedClick(Sender: TObject);
    procedure CheckBoxDisableCdAutoRunClick(Sender: TObject);
    procedure CheckBoxEjectAfterCdScanClick(Sender: TObject);
    procedure CheckBoxScanAfterCdInsertClick(Sender: TObject);
    procedure CheckBoxNoQueriesClick(Sender: TObject);
  private
{$ifndef LOGFONT}
    DiskFont   : TFont;
    TreeFont   : TFont;
    FileFont   : TFont;
    FoundFont  : TFont;
    DescFont   : TFont;
    PrintFont  : TFont;
{$else}
    DiskLogFont   : TLogFont;
    TreeLogFont   : TLogFont;
    FileLogFont   : TLogFont;
    FoundLogFont  : TLogFont;
    DescLogFont   : TLogFont;
    PrintLogFont  : TLogFont;

    DiskLogFontSize : Integer;
    TreeLogFontSize : Integer;
    FileLogFontSize : Integer;
    FoundLogFontSize: Integer;
    DescLogFontSize : Integer;
    PrintLogFontSize: Integer;
{$endif}
    SaveOptions: TGlobalOptions;
    {AlreadyWarned: boolean;}
    procedure SaveToIni;
    procedure LoadFromIni;
    procedure UpdateFontLabels;
  public
    HeaderWidths: TPanelHeaderWidths;
    procedure SetOptions(GlobalOptions: TGlobalOptions);
    procedure GetOptions(GlobalOptions: TGlobalOptions);
    procedure UpdateGlobalFormatSettings;
    procedure DefaultHandler(var Message); override;
  end;

var
  FormSettings: TFormSettings;

  g_CommonOptions: record
    bDisableCdAutorun    : boolean;
    bEjectAfterCdScan    : boolean;
    bScanAfterCdInsert   : boolean;
    bNoQueries           : boolean;
    dwQueryCancelAutoPlay: DWORD;
    end;

  g_UserCommandList: TList;
  g_sTempFolder    : AnsiString;

implementation

uses IniFiles, FMain, UBaseUtils, ULang, UDrives, FSelectDrive{, FDBase};

{$R *.dfm}

//===TGlobalOptions============================================================

constructor TGlobalOptions.Create;

  begin
{$ifndef LOGFONT}
  DiskFont  := TFont.Create;
  TreeFont  := TFont.Create;
  FileFont  := TFont.Create;
  FoundFont := TFont.Create;
  DescFont  := TFont.Create;
  PrintFont := TFont.Create;
{$endif}
  end;

//-----------------------------------------------------------------------------

destructor TGlobalOptions.Destroy;

  begin
{$ifndef LOGFONT}
  PrintFont.Free;
  DescFont.Free;
  FoundFont.Free;
  FileFont.Free;
  TreeFont.Free;
  DiskFont.Free;
{$endif}
  end;

//===TFormSettings=============================================================

procedure TFormSettings.ButtonOKClick(Sender: TObject);

  begin                         
  SaveToIni;
  ModalResult := mrOK;
  UsePersistentBlocks := CheckBoxPersistentBlocks.Checked;
  ScanAllDriveNames := not CheckBoxNotScanDriveNames.Checked; {in the QDRIVES unit}
  UpdateGlobalFormatSettings;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonCancelClick(Sender: TObject);

  begin
  SetOptions(SaveOptions);
  CheckBoxPersistentBlocks.Checked := UsePersistentBlocks;
  CheckBoxNotScanDriveNames.Checked := not ScanAllDriveNames;
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.RadioButtonBriefClick(Sender: TObject);

  begin
  CheckBoxShowSize.Enabled      := false;
  CheckBoxShowTime.Enabled      := false;
  CheckBoxShowDescr.Enabled     := false;
  RadioGroupSizeDisplay.Enabled := false;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.RadioButtonDetailedClick(Sender: TObject);

  begin
  CheckBoxShowSize.Enabled      := true;
  CheckBoxShowTime.Enabled      := true;
  CheckBoxShowDescr.Enabled     := true;
  RadioGroupSizeDisplay.Enabled := true;
  end;

//-----------------------------------------------------------------------------
// global settings are used for export to text format

procedure TFormSettings.UpdateGlobalFormatSettings;
  begin
  gFormatSettings.SortCrit      := TSortCrit(RadioGroupSortCrit.ItemIndex);
  gFormatSettings.bShowInKb     := RadioGroupSizeDisplay.ItemIndex = 1;
  gFormatSettings.bShowSeconds  := CheckBoxShowSeconds.Checked;
  gFormatSettings.bReversedSort := false; // nedoreseno!
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.FormCreate(Sender: TObject);

  begin
{$ifndef LOGFONT}
  DiskFont  := TFont.Create;
  TreeFont  := TFont.Create;
  FileFont  := TFont.Create;
  FoundFont := TFont.Create;
  DescFont  := TFont.Create;
  PrintFont := TFont.Create;
{$endif}
  SaveOptions := TGlobalOptions.Create;
  LoadFromIni;
  UpdateFontLabels;
  TabbedNotebook.ActivePage := TabbedNotebook.Pages[0];
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.FormDestroy(Sender: TObject);

  begin
  if CheckBoxAutoSave.Checked then SaveToIni;
  SaveOptions.Free;
{$ifndef LOGFONT}
  PrintFont.Free;
  DescFont.Free;
  FoundFont.Free;
  FileFont.Free;
  TreeFont.Free;
  DiskFont.Free;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.UpdateFontLabels;


{$ifndef LOGFONT}
  function GetFontDescription(var Font: TFont): ShortString;
    var
      S: ShortString;
    begin
    with Font do
      begin
      S := '';
      if fsBold in Style then S := S + lsBold;
      if fsItalic in Style then S := S + lsItalics;
      Result := Name + ', ' + IntToStr(Size) + S;
      end;
    end;
{$else}
  function GetLogFontDescription(var LogFont: TLogFont; Size: Integer): ShortString;
    var
      S: ShortString;
    begin
    with LogFont do
      begin
      S := '';
      if lfWeight = FW_BOLD then S := S + lsBold;
      if lfItalic = 1 then S := S + lsItalics;
      Result := StrPas(lfFaceName) + ', '
        + QGetScriptString(lfCharSet) + ', '
        + IntToStr(Size) + S;
      end;
    end;
 {$endif}


  begin
{$ifndef LOGFONT}
  LabelDiskFont.Caption  := GetFontDescription(DiskFont);
  LabelTreeFont.Caption  := GetFontDescription(TreeFont);
  LabelFileFont.Caption  := GetFontDescription(FileFont);
  LabelFoundFont.Caption := GetFontDescription(FoundFont);
  LabelDescFont.Caption  := GetFontDescription(DescFont);
  LabelPrintFont.Caption := GetFontDescription(PrintFont);
{$else}
  LabelDiskFont.Caption  := GetLogFontDescription(DiskLogFont,  DiskLogFontSize);
  LabelTreeFont.Caption  := GetLogFontDescription(TreeLogFont,  TreeLogFontSize);
  LabelFileFont.Caption  := GetLogFontDescription(FileLogFont,  FileLogFontSize);
  LabelFoundFont.Caption := GetLogFontDescription(FoundLogFont, FoundLogFontSize);
  LabelDescFont.Caption  := GetLogFontDescription(DescLogFont,  DescLogFontSize);
  LabelPrintFont.Caption := GetLogFontDescription(PrintLogFont, PrintLogFontSize);
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonChangeDiskFontClick(Sender: TObject);

  begin
{$ifndef LOGFONT}
  FontDialogDisk.Font.Assign(DiskFont);
  if FontDialogDisk.Execute then
    begin
    DiskFont.Assign(FontDialogDisk.Font);
    UpdateFontLabels;
    end;
{$else}
  if QSelectLogFontDialog(DiskLogFont, 6, 24, false, DiskLogFontSize) then
    UpdateFontLabels;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonChangeTreeFontClick(Sender: TObject);

  begin
{$ifndef LOGFONT}
  FontDialogTree.Font.Assign(TreeFont);
  if FontDialogTree.Execute then
    begin
    TreeFont.Assign(FontDialogTree.Font);
    UpdateFontLabels;
    end;
{$else}
  if QSelectLogFontDialog(TreeLogFont, 6, 24, false, TreeLogFontSize) then
    UpdateFontLabels;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonChangeFileFontClick(Sender: TObject);

  begin
{$ifndef LOGFONT}
  FontDialogFile.Font.Assign(FileFont);
  if FontDialogFile.Execute then
    begin
    FileFont.Assign(FontDialogFile.Font);
    UpdateFontLabels;
    end;
{$else}
  if QSelectLogFontDialog(FileLogFont, 6, 24, false, FileLogFontSize) then
    UpdateFontLabels;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonChangeFoundFontClick(Sender: TObject);

  begin
{$ifndef LOGFONT}
  FontDialogFound.Font.Assign(FoundFont);
  if FontDialogFound.Execute then
    begin
    FoundFont.Assign(FontDialogFound.Font);
    UpdateFontLabels;
    end;
{$else}
  if QSelectLogFontDialog(FoundLogFont, 6, 24, false, FoundLogFontSize) then
    UpdateFontLabels;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonChangeDescFontClick(Sender: TObject);

  begin
{$ifndef LOGFONT}
  FontDialogDesc.Font.Assign(DescFont);
  if FontDialogDesc.Execute then
    begin
    DescFont.Assign(FontDialogDesc.Font);
    UpdateFontLabels;
    end;
{$else}
  if QSelectLogFontDialog(DescLogFont, 6, 24, false, DescLogFontSize) then
    UpdateFontLabels;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonChangePrintFontClick(Sender: TObject);

  begin
{$ifndef LOGFONT}
  FontDialogPrint.Font.Assign(PrintFont);
  if FontDialogPrint.Execute then
    begin
    PrintFont.Assign(FontDialogPrint.Font);
    UpdateFontLabels;
    end;
{$else}
  if QSelectLogFontDialog(PrintLogFont, 6, 24, true, PrintLogFontSize) then
    UpdateFontLabels;
{$endif}
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.SaveToIni;

  var
    IniFile: TIniFile;
    sIniFileName: AnsiString;

{$ifndef LOGFONT}
    procedure WriteFont(var Font: TFont; Section: ShortString);
      begin
      with Font do
        begin
        IniFile.WriteString (Section, 'FontName', Name);
        IniFile.WriteInteger(Section, 'FontSize', Size);
        IniFile.WriteBool   (Section, 'Bold', fsBold in Style);
        IniFile.WriteBool   (Section, 'Italic', fsItalic in Style);
        end;
      end;
{$else}
    procedure WriteLogFont(var LogFont: TLogFont; LogFontSize: ShortInt; Section: ShortString);
      begin
      with LogFont do
        begin
        IniFile.WriteString (Section, 'FontName', StrPas(lfFaceName));
        IniFile.WriteString (Section, 'FontScript', QGetScriptString(lfCharSet));
        IniFile.WriteInteger(Section, 'FontSize', LogFontSize);
        IniFile.WriteBool   (Section, 'Bold', lfWeight = FW_BOLD);
        IniFile.WriteBool   (Section, 'Italic', lfItalic = 1);
        end;
      end;
{$endif}
  begin
  sIniFileName := ChangeFileExt(ParamStr(0), '.ini');
  if not FileExists(sIniFileName)
    then // test file creation
      begin
      if not TestFileCreation(sIniFileName) then exit;
      end
    else
      begin // check if it is read-only
      ///if ((GetFileAttributes(PChar(sIniFileName)) and FILE_ATTRIBUTE_READONLY) <> 0) then exit;
      end;

  IniFile := TIniFile.Create(sIniFileName);
  IniFile.WriteBool ('General', 'Autosave',         CheckBoxAutoSave.Checked);
  IniFile.WriteBool ('General', 'FoundToNewWin',    CheckBoxFoundToNewWin.Checked);
  IniFile.WriteBool ('General', 'BackupDBase',      CheckBoxBackupDBase.Checked);
  {IniFile.WriteBool ('General', 'UseWinLocale',      CheckBoxUseWinLocale.Checked);}
  IniFile.WriteBool ('General', 'NotScanDriveNames', CheckBoxNotScanDriveNames.Checked);
  IniFile.WriteBool ('General', 'EnableShellExecute', CheckBoxEnableShellExecute.Checked);
  IniFile.WriteString ('General', 'AutoLoad',      EditAutoLoadDBase.Text);
  IniFile.WriteBool ('General', 'AutoLoadLast',    CheckBoxOpenLastOpened.Checked);
  IniFile.WriteBool ('General', 'DisableAutorun',  CheckBoxDisableCdAutoRun.Checked);
  IniFile.WriteBool ('General', 'EjectAfterScan',  CheckBoxEjectAfterCdScan.Checked);
  IniFile.WriteBool ('General', 'ScanAfterInsert', CheckBoxScanAfterCdInsert.Checked);
  IniFile.WriteBool ('General', 'NoQueries',       CheckBoxNoQueries.Checked);

  IniFile.WriteBool ('General', 'EnableUTFConvert',CheckBoxEnableUTFConvert.Checked);


  IniFile.WriteBool ('Display', 'ShowTree',        CheckBoxShowTree.Checked);
  IniFile.WriteBool ('Display', 'ExpandTree',      CheckBoxExpandTree.Checked);
  IniFile.WriteBool ('Display', 'DetailedDisplay', RadioButtonDetailed.Checked);
  IniFile.WriteBool ('Display', 'ShowSize',        CheckBoxShowSize.Checked);
  IniFile.WriteBool ('Display', 'ShowTime',        CheckBoxShowTime.Checked);
  IniFile.WriteBool ('Display', 'ShowDescr',       CheckBoxShowDescr.Checked);
  IniFile.WriteInteger ('Display', 'SortCrit',     RadioGroupSortCrit.ItemIndex);
  IniFile.WriteInteger ('Display', 'SizeDisplay',  RadioGroupSizeDisplay.ItemIndex);
  IniFile.WriteBool ('Display', 'ShowSeconds',     CheckBoxShowSeconds.Checked);
  IniFile.WriteBool ('Display', 'ShowIcons',       CheckBoxShowIcons.Checked);
  IniFile.WriteBool ('Display', 'WinDateFormat',   CheckBoxWinDateFormat.Checked);
  IniFile.WriteBool ('Display', 'OneKbIs1024',     CheckBoxOneKbIs1024.Checked);
  IniFile.WriteBool ('Display', 'ShowFileHints',   CheckBoxShowFileHints.Checked);
  IniFile.WriteBool ('Display', 'PersistentBlocks',CheckBoxPersistentBlocks.Checked);



{$ifndef LOGFONT}
  WriteFont(DiskFont,  'DiskPanel');
  WriteFont(TreeFont,  'TreePanel');
  WriteFont(FileFont,  'FilePanel');
  WriteFont(FoundFont, 'SearchWindow');
  WriteFont(DescFont,  'DescriptionWindow');
  WriteFont(PrintFont, 'PrintPanel');
{$else}
  WriteLogFont(DiskLogFont,  DiskLogFontSize,  'DiskPanel');
  WriteLogFont(TreeLogFont,  TreeLogFontSize,  'TreePanel');
  WriteLogFont(FileLogFont,  FileLogFontSize,  'FilePanel');
  WriteLogFont(FoundLogFont, FoundLogFontSize, 'SearchWindow');
  WriteLogFont(DescLogFont,  DescLogFontSize,  'DescriptionWindow');
  WriteLogFont(PrintLogFont, PrintLogFontSize, 'PrintPanel');
{$endif}

  IniFile.WriteInteger ('Print', 'TopMargin',    SpinEditTop.Value);
  IniFile.WriteInteger ('Print', 'BottomMargin', SpinEditBottom.Value);
  IniFile.WriteInteger ('Print', 'LeftMargin',   SpinEditLeft.Value);
  IniFile.WriteInteger ('Print', 'RightMargin',  SpinEditRight.Value);
  IniFile.WriteString  ('Print', 'Header',  EditPrintHeader.Text);
  IniFile.WriteBool    ('Print', 'AdjustNameWidth', CheckBoxAdjustNameWidth.Checked);
  IniFile.Free;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.LoadFromIni;

  var
    IniFile: TIniFile;
    sIniFileName: AnsiString;

{$ifndef LOGFONT}
    procedure ReadFont(var Font: TFont; Section: ShortString;
                       DefaultName: ShortString; DefaultSize: Integer);
      begin
      with Font do
        begin
        Name := IniFile.ReadString (Section, 'FontName', DefaultName);
        Size := IniFile.ReadInteger(Section, 'FontSize', DefaultSize);
        Style := [];
        if IniFile.ReadBool (Section, 'Bold', false) then
          Style := Style + [fsBold];
        if IniFile.ReadBool (Section, 'Italic', false) then
          Style := Style + [fsItalic];
        end;
      end;

    procedure SetFont(var Font: TFont; Section: ShortString;
                      DefaultName: ShortString; DefaultSize: Integer);
      begin
      with Font do
        begin
        Name := DefaultName;
        Size := DefaultSize;
        Style := [];
        end;
      end;
{$else}
    procedure ReadLogFont(var LogFont: TLogFont; var LogFontSize: Integer;
                          Section: ShortString;
                          DefaultName: ShortString; DefaultSize: Integer);
      var
        Name, Script: ShortString;
        Bold, Italics : boolean;
      begin
      with LogFont do
        begin
        Name := IniFile.ReadString (Section, 'FontName', DefaultName);
        Script := IniFile.ReadString (Section, 'FontScript', lsScript);
        LogFontSize := IniFile.ReadInteger(Section, 'FontSize', DefaultSize);
        Bold := IniFile.ReadBool (Section, 'Bold', false);
        Italics := IniFile.ReadBool (Section, 'Italic', false);
        InitLogFontStruct (LogFont, Name, LogFontSize, Bold, Italics, Script);
        end;
      end;

    procedure SetLogFont(var LogFont: TLogFont; var LogFontSize: Integer;
                          Section: ShortString;
                          DefaultName: ShortString; DefaultSize: Integer);
      var
        Name, Script: ShortString;
        Bold, Italics : boolean;
      begin
      with LogFont do
        begin
        Name := DefaultName;
        Script := lsScript;
        LogFontSize := DefaultSize;
        Bold := false;
        Italics := false;
        InitLogFontStruct (LogFont, Name, LogFontSize, Bold, Italics, Script);
        end;
      end;

{$endif}

  begin
  sIniFileName := ChangeFileExt(ParamStr(0), '.ini');
  if not FileExists(sIniFileName)
    then
      begin
      CheckBoxAutoSave.Checked           := false;
      CheckBoxFoundToNewWin.Checked      := false;
      CheckBoxBackupDBase.Checked        := false;
      CheckBoxNotScanDriveNames.Checked  := false;
      CheckBoxEnableShellExecute.Checked := false;
      CheckBoxDisableCdAutoRun.Checked   := g_CommonOptions.bDisableCdAutorun;
      CheckBoxEjectAfterCdScan.Checked   := g_CommonOptions.bEjectAfterCdScan;
      CheckBoxScanAfterCdInsert.Checked  := g_CommonOptions.bScanAfterCdInsert;
      CheckBoxNoQueries.Checked          := g_CommonOptions.bNoQueries;
      EditAutoLoadDBase.Text             := '';
      CheckBoxOpenLastOpened.Checked     := true;

      CheckBoxEnableUTFConvert.Checked    := false;

      CheckBoxShowTree.Checked   := true;
      CheckBoxExpandTree.Checked := false;
      CheckBoxExpandTree.Enabled := CheckBoxShowTree.Checked;

      RadioButtonBrief.Checked := true;
      RadioButtonBriefClick(Self);

      CheckBoxShowSize.Checked     := true;
      CheckBoxShowTime.Checked     := true;
      CheckBoxShowDescr.Checked    := true;
      RadioGroupSortCrit.ItemIndex    := 0;
      RadioGroupSizeDisplay.ItemIndex := 1;
      CheckBoxShowSeconds.Checked   := false;
      CheckBoxShowIcons.Checked     := true;
      CheckBoxWinDateFormat.Checked := false;
      CheckBoxOneKbIs1024.Checked       := true;
      CheckBoxShowFileHints.Checked     := true;
      CheckBoxPersistentBlocks.Checked  := true;

    {$ifndef LOGFONT}
      SetFont(DiskFont,  'DiskPanel', lsMSSansSerif, 9);
      SetFont(TreeFont,  'TreePanel', lsMSSansSerif, 9);
      SetFont(FileFont,  'FilePanel',  lsMSSansSerif, 9);
      SetFont(FoundFont, 'SearchWindow',  lsMSSansSerif, 9);
      SetFont(DescFont,  'DescriptionWindow',  lsMSSansSerif, 9);
      SetFont(PrintFont, 'PrintPanel', lsArial, 9);
    {$else}
      SetLogFont(DiskLogFont,  DiskLogFontSize,  'DiskPanel', lsMSSansSerif, 9);
      SetLogFont(TreeLogFont,  TreeLogFontSize,  'TreePanel', lsMSSansSerif, 9);
      SetLogFont(FileLogFont,  FileLogFontSize,  'FilePanel', lsMSSansSerif, 9);
      SetLogFont(FoundLogFont, FoundLogFontSize, 'SearchWindow', lsMSSansSerif, 9);
      SetLogFont(DescLogFont,  DescLogFontSize,  'DescriptionWindow', lsMSSansSerif, 9);
      SetLogFont(PrintLogFont, PrintLogFontSize, 'PrintPanel', lsArial, 9);
    {$endif}

      SpinEditTop.Value    := 15;
      SpinEditBottom.Value := 15;
      SpinEditLeft.Value   := 15;
      SpinEditRight.Value  := 15;
      EditPrintHeader.Text := '';
      CheckBoxAdjustNameWidth.Checked := true;

      HeaderWidths[0] := 93;
      HeaderWidths[1] := 152;
      HeaderWidths[2] := 242;
      g_PanelHeaderWidths := HeaderWidths;
      end
    else
      begin
      IniFile := TIniFile.Create(sIniFileName);
      CheckBoxAutoSave.Checked           := IniFile.ReadBool  ('General', 'Autosave', false);
      CheckBoxFoundToNewWin.Checked      := IniFile.ReadBool  ('General', 'FoundToNewWin', false);
      CheckBoxBackupDBase.Checked        := IniFile.ReadBool  ('General', 'BackupDBase', false);
      {CheckBoxUseWinLocale.Checked     := IniFile.ReadBool('General', 'UseWinLocale', true);}
      CheckBoxNotScanDriveNames.Checked  := IniFile.ReadBool  ('General', 'NotScanDriveNames', false);
      CheckBoxEnableShellExecute.Checked := IniFile.ReadBool  ('General', 'EnableShellExecute', false);
      CheckBoxDisableCdAutoRun.Checked   := IniFile.ReadBool  ('General', 'DisableAutorun', true);
      CheckBoxEjectAfterCdScan.Checked   := IniFile.ReadBool  ('General', 'EjectAfterScan', true);
      CheckBoxScanAfterCdInsert.Checked  := IniFile.ReadBool  ('General', 'ScanAfterInsert', false);
      CheckBoxNoQueries.Checked          := IniFile.ReadBool  ('General', 'NoQueries', false);
      EditAutoLoadDBase.Text             := IniFile.ReadString('General', 'AutoLoad', '');
      CheckBoxOpenLastOpened.Checked     := IniFile.ReadBool  ('General', 'AutoLoadLast', true);

      CheckBoxEnableUTFConvert.Checked   := IniFile.ReadBool  ('General', 'EnableUTFConvert', false);


      CheckBoxShowTree.Checked   := IniFile.ReadBool('Display', 'ShowTree', true);
      CheckBoxExpandTree.Checked := IniFile.ReadBool('Display', 'ExpandTree', false);
      CheckBoxExpandTree.Enabled := CheckBoxShowTree.Checked;

      if IniFile.ReadBool('Display', 'DetailedDisplay', false)
        then
          begin
          RadioButtonDetailed.Checked := true;
          RadioButtonDetailedClick(Self);
          end
        else
          begin
          RadioButtonBrief.Checked := true;
          RadioButtonBriefClick(Self);
          end;
      CheckBoxShowSize.Checked     := IniFile.ReadBool('Display', 'ShowSize', true);
      CheckBoxShowTime.Checked     := IniFile.ReadBool('Display', 'ShowTime', true);
      CheckBoxShowDescr.Checked    := IniFile.ReadBool('Display', 'ShowDescr', true);
      RadioGroupSortCrit.ItemIndex    := IniFile.ReadInteger ('Display', 'SortCrit', 0);
      RadioGroupSizeDisplay.ItemIndex := IniFile.ReadInteger ('Display', 'SizeDisplay', 1);
      CheckBoxShowSeconds.Checked   := IniFile.ReadBool('Display', 'ShowSeconds', false);
      CheckBoxShowIcons.Checked     := IniFile.ReadBool('Display', 'ShowIcons', true);
      CheckBoxWinDateFormat.Checked := IniFile.ReadBool('Display', 'WinDateFormat', false);
      CheckBoxOneKbIs1024.Checked   := IniFile.ReadBool('Display', 'OneKbIs1024', true);
      CheckBoxShowFileHints.Checked   := IniFile.ReadBool('Display', 'ShowFileHints', true);
      CheckBoxPersistentBlocks.Checked  := IniFile.ReadBool('Display', 'PersistentBlocks', true);

    {$ifndef LOGFONT}

      {$ifdef DELPHI1}
      ReadFont(DiskFont,  'DiskPanel', lsMSSansSerif, 8);
      ReadFont(TreeFont,  'TreePanel', lsMSSansSerif, 9);
      ReadFont(FileFont,  'FilePanel',  lsMSSansSerif, 8);
      ReadFont(FoundFont, 'SearchWindow',  lsMSSansSerif, 8);
      ReadFont(DescFont,  'DescriptionWindow',  lsMSSansSerif, 8);
      ReadFont(PrintFont, 'PrintPanel', lsArial, 9);
      {$else}
      ReadFont(DiskFont,  'DiskPanel', lsMSSansSerif, 8);
      ReadFont(TreeFont,  'TreePanel', lsMSSansSerif, 9);
      ReadFont(FileFont,  'FilePanel',  lsMSSansSerif, 8);
      ReadFont(FoundFont, 'SearchWindow',  lsMSSansSerif, 8);
      ReadFont(DescFont,  'DescriptionWindow',  lsMSSansSerif, 8);
      ReadFont(PrintFont, 'PrintPanel', lsArial, 9);
      {$endif}

    {$else}
      ReadLogFont(DiskLogFont,  DiskLogFontSize,  'DiskPanel', lsMSSansSerif, 8);
      ReadLogFont(TreeLogFont,  TreeLogFontSize,  'TreePanel', lsMSSansSerif, 9);
      ReadLogFont(FileLogFont,  FileLogFontSize,  'FilePanel', lsMSSansSerif, 8);
      ReadLogFont(FoundLogFont, FoundLogFontSize, 'SearchWindow', lsMSSansSerif, 8);
      ReadLogFont(DescLogFont,  DescLogFontSize,  'DescriptionWindow', lsMSSansSerif, 8);
      ReadLogFont(PrintLogFont, PrintLogFontSize, 'PrintPanel', lsArial, 9);
    {$endif}

      SpinEditTop.Value    := IniFile.ReadInteger ('Print', 'TopMargin', 15);
      SpinEditBottom.Value := IniFile.ReadInteger ('Print', 'BottomMargin', 15);
      SpinEditLeft.Value   := IniFile.ReadInteger ('Print', 'LeftMargin', 15);
      SpinEditRight.Value  := IniFile.ReadInteger ('Print', 'RightMargin', 15);
      EditPrintHeader.Text := IniFile.ReadString  ('Print', 'Header', '');
      CheckBoxAdjustNameWidth.Checked := IniFile.ReadBool ('Print', 'AdjustNameWidth', true);

      HeaderWidths[0] := IniFile.ReadInteger('Application', 'MDIChildHeader0',  93);
      if (HeaderWidths[0] < 5) then HeaderWidths[0] := 5;
      HeaderWidths[1] := IniFile.ReadInteger('Application', 'MDIChildHeader1', 152);
      if (HeaderWidths[1] < 5) then HeaderWidths[1] := 5;
      HeaderWidths[2] := IniFile.ReadInteger('Application', 'MDIChildHeader2', 242);
      if (HeaderWidths[2] < 5) then HeaderWidths[2] := 5;
      g_PanelHeaderWidths := HeaderWidths;

      IniFile.Free;
      end;

  UsePersistentBlocks := CheckBoxPersistentBlocks.Checked;
  ScanAllDriveNames := not CheckBoxNotScanDriveNames.Checked;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonSaveClick(Sender: TObject);

  begin
  SaveToIni;
  UsePersistentBlocks := CheckBoxPersistentBlocks.Checked;
  ScanAllDriveNames := not CheckBoxNotScanDriveNames.Checked;
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.SetOptions(GlobalOptions: TGlobalOptions);

  begin
  with GlobalOptions do
    begin
    RadioGroupSortCrit.ItemIndex := Integer(SortCrit);
    case FileDisplayType of
      fdBrief   : RadioButtonBrief.Checked := true;
      fdDetailed: RadioButtonDetailed.Checked := true;
      end;
    CheckBoxShowSize.Checked       := ShowSize;
    CheckBoxShowTime.Checked       := ShowTime;
    CheckBoxShowDescr.Checked      := ShowDescr;
    if ShowInKb
      then RadioGroupSizeDisplay.ItemIndex := 1
      else RadioGroupSizeDisplay.ItemIndex := 0;
    CheckBoxShowSeconds.Checked    := ShowSeconds;
    CheckBoxShowIcons.Checked      := ShowIcons;
    CheckBoxOneKbIs1024.Checked    := OneKbIs1024;
    CheckBoxShowFileHints.Checked  := ShowFileHints;
    CheckBoxWinDateFormat.Checked  := SystemDateFormat;
    CheckBoxShowTree.Checked       := ShowTree and (PanelHeaderWidths[1] > 0);
    CheckBoxExpandTree.Checked     := ExpandTree;

{$ifndef LOGFONT}
    Self.DiskFont.Assign (DiskFont);
    Self.TreeFont.Assign (TreeFont);
    Self.FileFont.Assign (FileFont);
    Self.FoundFont.Assign(FoundFont);
    Self.DescFont.Assign (DescFont);
    Self.PrintFont.Assign(PrintFont);
{$else}
    Self.DiskLogFont  := DiskLogFont;
    Self.TreeLogFont  := TreeLogFont;
    Self.FileLogFont  := FileLogFont;
    Self.FoundLogFont := FoundLogFont;
    Self.DescLogFont  := DescLogFont;
    Self.PrintLogFont := PrintLogFont;

    Self.DiskLogFontSize  := DiskLogFontSize;
    Self.TreeLogFontSize  := TreeLogFontSize;
    Self.FileLogFontSize  := FileLogFontSize;
    Self.FoundLogFontSize := FoundLogFontSize;
    Self.DescLogFontSize  := DescLogFontSize;
    Self.PrintLogFontSize := PrintLogFontSize;
{$endif}
    CheckBoxAutoSave.Checked       := AutoSave;
    CheckBoxFoundToNewWin.Checked  := FoundToNewWin;
    SpinEditTop.Value    := TopMargin;
    SpinEditBottom.Value := BottomMargin;
    SpinEditLeft.Value   := LeftMargin;
    SpinEditRight.Value  := RightMargin;
    EditPrintHeader.Text := PrintHeader;
    CheckBoxAdjustNameWidth.Checked := AdjustNameWidth;

    HeaderWidths         := PanelHeaderWidths;
    g_PanelHeaderWidths  := HeaderWidths;

    EditAutoLoadDBase.Text             := AutoLoadDBase;
    CheckBoxOpenLastOpened.Checked     := OpenLastOpened;
    CheckBoxBackupDBase.Checked        := BackupDBase;
    CheckBoxPersistentBlocks.Checked   := PersistentBlocks;
    CheckBoxNotScanDriveNames.Checked  := NotScanDriveNames;
    CheckBoxEnableShellExecute.Checked := EnableShellExecute;
    CheckBoxDisableCdAutoRun.Checked   := g_CommonOptions.bDisableCdAutorun;
    CheckBoxEjectAfterCdScan.Checked   := g_CommonOptions.bEjectAfterCdScan;
    CheckBoxScanAfterCdInsert.Checked  := g_CommonOptions.bScanAfterCdInsert;
    CheckBoxNoQueries.Checked          := g_CommonOptions.bNoQueries;
    end; {with Options}
  UpdateFontLabels;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.GetOptions(GlobalOptions: TGlobalOptions);

  begin
  with GlobalOptions do
    begin
    SortCrit := TSortCrit(RadioGroupSortCrit.ItemIndex);
    if RadioButtonBrief.Checked
      then FileDisplayType := fdBrief
      else FileDisplayType := fdDetailed;
    ShowSize         := CheckBoxShowSize.Checked;
    ShowTime         := CheckBoxShowTime.Checked;
    ShowDescr        := CheckBoxShowDescr.Checked;
    ShowInKb         := RadioGroupSizeDisplay.ItemIndex = 1;
    ShowSeconds      := CheckBoxShowSeconds.Checked;
    ShowIcons        := CheckBoxShowIcons.Checked;
    OneKbIs1024      := CheckBoxOneKbIs1024.Checked;
    ShowFileHints    := CheckBoxShowFileHints.Checked;
    SystemDateFormat := CheckBoxWinDateFormat.Checked;
    ShowTree         := CheckBoxShowTree.Checked and (HeaderWidths[1] > 0);
    ExpandTree       := CheckBoxExpandTree.Checked;
    ReversedSort     := false;
{$ifndef LOGFONT}
    DiskFont.Assign (Self.DiskFont);
    TreeFont.Assign (Self.TreeFont);
    FileFont.Assign (Self.FileFont);
    FoundFont.Assign(Self.FoundFont);
    DescFont.Assign (Self.DescFont);
    PrintFont.Assign(Self.PrintFont);
{$else}
    DiskLogFont  := Self.DiskLogFont;
    TreeLogFont  := Self.TreeLogFont;
    FileLogFont  := Self.FileLogFont;
    FoundLogFont := Self.FoundLogFont;
    DescLogFont  := Self.DescLogFont;
    PrintLogFont := Self.PrintLogFont;

    DiskLogFontSize  := Self.DiskLogFontSize;
    TreeLogFontSize  := Self.TreeLogFontSize;
    FileLogFontSize  := Self.FileLogFontSize;
    FoundLogFontSize := Self.FoundLogFontSize;
    DescLogFontSize  := Self.DescLogFontSize;
    PrintLogFontSize := Self.PrintLogFontSize;
{$endif}
    AutoSave         := CheckBoxAutoSave.Checked;
    FoundToNewWin    := CheckBoxFoundToNewWin.Checked;
    TopMargin        := SpinEditTop.Value;
    BottomMargin     := SpinEditBottom.Value;
    LeftMargin       := SpinEditLeft.Value;
    RightMargin      := SpinEditRight.Value;
    PrintHeader      := EditPrintHeader.Text;
    AdjustNameWidth  := CheckBoxAdjustNameWidth.Checked;
    PanelHeaderWidths := HeaderWidths;
    g_PanelHeaderWidths := HeaderWidths;

    {exceptional - the following settings are really global}
    If SystemDateFormat
      then WinDateFormat := wd_WinShort {is in QB_Utils}
      else WinDateFormat := SaveWinDateFormat;
    If OneKbIs1024
      then KbDivider := 1024
      else KbDivider := 1000;
    {up to here}

    AutoLoadDBase      := EditAutoLoadDBase.Text;
    OpenLastOpened     := CheckBoxOpenLastOpened.Checked;
    BackupDBase        := CheckBoxBackupDBase.Checked;
    PersistentBlocks   := CheckBoxPersistentBlocks.Checked;
    NotScanDriveNames  := CheckBoxNotScanDriveNames.Checked;
    EnableShellExecute := CheckBoxEnableShellExecute.Checked;

    EnableUTFConvert   := CheckBoxEnableUTFConvert.Checked;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.FormShow(Sender: TObject);

  begin
  ActiveControl := ButtonOK;
  {AlreadyWarned := false;}
  GetOptions(SaveOptions);
  CheckBoxNoQueries.Checked := g_CommonOptions.bNoQueries; // because it can be changed alsofrom FormSelectDrive
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.CheckBoxShowTreeClick(Sender: TObject);

  begin
  CheckBoxExpandTree.Enabled := CheckBoxShowTree.Checked;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonBrowseClick(Sender: TObject);

  begin
  if OpenDialogDBase.Execute then
    begin
    EditAutoLoadDBase.Text := OpenDialogDBase.FileName;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.ButtonHelpClick(Sender: TObject);
  begin
  Application.HelpContext(290);
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.CheckBoxOpenLastOpenedClick(Sender: TObject);

  begin
  EditAutoLoadDBase.Enabled := not CheckBoxOpenLastOpened.Checked;
  ButtonBrowse.Enabled := not CheckBoxOpenLastOpened.Checked;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.CheckBoxDisableCdAutoRunClick(Sender: TObject);

  begin
  g_CommonOptions.bDisableCdAutorun := CheckBoxDisableCdAutoRun.Checked;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.CheckBoxEjectAfterCdScanClick(Sender: TObject);

  begin
  g_CommonOptions.bEjectAfterCdScan := CheckBoxEjectAfterCdScan.Checked;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.CheckBoxScanAfterCdInsertClick(Sender: TObject);

  begin
  g_CommonOptions.bScanAfterCdInsert := CheckBoxScanAfterCdInsert.Checked;
  end;

//-----------------------------------------------------------------------------

procedure TFormSettings.CheckBoxNoQueriesClick(Sender: TObject);
  begin
  g_CommonOptions.bNoQueries := CheckBoxNoQueries.Checked;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormSettings.DefaultHandler(var Message);

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

var
  TempBuf: array [1..500] of char;

begin
g_CommonOptions.bDisableCdAutorun := true;
g_CommonOptions.bEjectAfterCdScan := true;
g_CommonOptions.bScanAfterCdInsert:= false;
g_CommonOptions.bNoQueries        := false;
///g_CommonOptions.dwQueryCancelAutoPlay := RegisterWindowMessage('QueryCancelAutoPlay');
g_UserCommandList := TList.Create();
///GetTempPath(500, @TempBuf);
g_sTempFolder := TempBuf;
end.
