unit FFindFileDlg;
(*====================================================================
Dialog box for find file options
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
  Forms, Dialogs, StdCtrls, Spin, ExtCtrls, IniFiles,
  UTypes, UApiTypes;

type
  TFormSearchFileDlg = class(TForm)
    RadioGroupSearch: TRadioGroup;
    Label1: TLabel;
    GroupBoxMaxLines: TGroupBox;
    SpinEditMaxRecs: TSpinEdit;
    Label4: TLabel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxCaseSensitive: TCheckBox;
    ComboBoxMask: TComboBox;
    ButtonOptions: TButton;
    ButtonHelp: TButton;
    CheckBoxAddWildcards: TCheckBox;
    ComboBoxScanDirLevel: TComboBox;
    Label3: TLabel;
    GroupBoxWhere: TGroupBox;
    CheckBoxFileNames: TCheckBox;
    CheckBoxFolderNames: TCheckBox;
    CheckBoxDescriptions: TCheckBox;
    CheckBoxPhrase: TCheckBox;
    CheckBoxUseMoreOptions: TCheckBox;
    CheckBoxStrictMask: TCheckBox;
    CheckBoxDiskNames: TCheckBox;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxMaskChange(Sender: TObject);
  public
    DlgData: TSearchDlgData;
    procedure RestoreDlgData (var DlgData: TSearchDlgData);
    procedure SaveDlgData    (var DlgData: TSearchDlgData);
    procedure SaveToIni      (var IniFile: TIniFile);
    procedure LoadFromIni    (var IniFile: TIniFile);
  end;

var
  FormSearchFileDlg: TFormSearchFileDlg;

implementation

uses
  FMoreOptions, ULang;

{$R *.dfm}

//------------------------------------------------------------------------

procedure TFormSearchFileDlg.ButtonOKClick(Sender: TObject);

  var
    Found : boolean;
    i     : Integer;

  begin
  SaveDlgData(DlgData);
  if ComboBoxMask.Text <> ''
    then
      begin
      Found := false;
      for i := 0 to pred(ComboBoxMask.Items.Count) do
        if ComboBoxMask.Items.Strings[i] = ComboBoxMask.Text then
          Found := true;
      if not Found then ComboBoxMask.Items.Insert(0, ComboBoxMask.Text);
      ModalResult := mrOk;
      end
    else
      begin
      Application.MessageBox(lsNoMaskEntered, lsCannotSearch, mb_OK);
      ActiveControl := ComboBoxMask;
      end;
  end;

//------------------------------------------------------------------------

procedure TFormSearchFileDlg.ButtonCancelClick(Sender: TObject);

  begin
  RestoreDlgData(DlgData);
  ModalResult := mrCancel;
  end;

//------------------------------------------------------------------------

procedure TFormSearchFileDlg.FormShow(Sender: TObject);

  begin
  SaveDlgData(DlgData);
  ActiveControl := ComboBoxMask;
  {$ifndef DELPHI1}
  SpinEditMaxRecs.MaxValue := 50000;
  {$endif}
  CheckBoxAddWildCards.Enabled := (Pos('*', ComboBoxMask.Text) = 0) and
                                  (length(ComboBoxMask.Text) > 0);
  CheckBoxPhrase.Enabled := (Pos(' ', ComboBoxMask.Text) <> 0) and
                            (Pos('"', ComboBoxMask.Text) = 0);
  end;

//------------------------------------------------------------------------
// Restores the values in the controls from saved structure - used when the
// user presses Cancel

procedure TFormSearchFileDlg.RestoreDlgData (var DlgData: TSearchDlgData);

  begin
  with DlgData do
    begin
    ComboBoxMask.Text              := Mask;
    CheckBoxAddWildCards.Checked   := AddWildCards;
    CheckBoxPhrase.Checked         := AsPhrase;
    SpinEditMaxRecs.Value          := MaxLines;
    CheckBoxCaseSensitive.Checked  := CaseSensitive;
    CheckBoxStrictMask.Checked     := StrictMask;
    RadioGroupSearch.ItemIndex     := Integer(SearchIn);
    CheckBoxDiskNames.Checked      := ScanDiskNames;
    CheckBoxFileNames.Checked      := ScanFileNames;
    CheckBoxFolderNames.Checked    := ScanDirNames;
    CheckBoxDescriptions.Checked   := ScanDesc;
    ComboBoxScanDirLevel.ItemIndex := ScanDirLevel+1;
    CheckBoxUseMoreOptions.Checked := UseMoreOptions;
    end;
  end;

//------------------------------------------------------------------------
// Puts the values from this dialog controls to a structure, used by the
// engine for searching

procedure TFormSearchFileDlg.SaveDlgData (var DlgData: TSearchDlgData);

  begin
  with DlgData do
    begin
    Mask           := ComboBoxMask.Text;
    if CheckBoxAddWildCards.Enabled
      then AddWildCards := CheckBoxAddWildCards.Checked
      else AddWildCards := false;
    if CheckBoxPhrase.Enabled
      then AsPhrase := CheckBoxPhrase.Checked
      else AsPhrase := false;
    MaxLines       := SpinEditMaxRecs.Value;
    {$ifdef DELPHI1}
    if MaxLines > 16000 then MaxLines := 16000;
    {$endif}
    CaseSensitive  := CheckBoxCaseSensitive.Checked;
    StrictMask     := CheckBoxStrictMask.Checked;
    SearchIn       := TSearchIn(RadioGroupSearch.ItemIndex);
    ScanDiskNames  := CheckBoxDiskNames.Checked;
    ScanFileNames  := CheckBoxFileNames.Checked;
    ScanDirNames   := CheckBoxFolderNames.Checked;
    ScanDesc       := CheckBoxDescriptions.Checked;
    UseMoreOptions := CheckBoxUseMoreOptions.Checked;
    if CheckBoxUseMoreOptions.Checked
      then
        begin
        ExcludeMask  := FormMoreOptions.ComboBoxExclude.Text;
        DirMask      := FormMoreOptions.ComboBoxDirMask.Text;
        DateFrom     := FormMoreOptions.DateFrom;
        DateTo       := FormMoreOptions.DateTo;
        SizeFrom     := FormMoreOptions.SizeFrom;
        SizeTo       := FormMoreOptions.SizeTo;
        SortArr[1]   := TSort(FormMoreOptions.RadioGroupKey1.ItemIndex);
        SortArr[2]   := TSort(FormMoreOptions.RadioGroupKey2.ItemIndex);
        SortArr[3]   := TSort(FormMoreOptions.RadioGroupKey3.ItemIndex);
        MoreOptions  := (DateFrom > 0) or (DateTo > 0) or
                        (SizeFrom > 0) or (SizeTo > 0) or
                        (ExcludeMask <> '');
        SearchAsAnsi := FormMoreOptions.CheckBoxSearchAsAnsi.Checked;
        SearchAsOem  := FormMoreOptions.CheckBoxSearchAsOem.Checked;
        end
      else
        begin
        ExcludeMask  := '';
        DirMask      := '';
        DateFrom     := 0;
        DateTo       := 0;
        SizeFrom     := 0;
        SizeTo       := 0;
        SortArr[1]   := soExt;
        SortArr[2]   := soKey;
        SortArr[3]   := soTime;
        MoreOptions  := false;
        SearchAsAnsi := true;
        SearchAsOem  := false;
        end;
    ScanDirLevel := ComboBoxScanDirLevel.ItemIndex-1;
    if ScanDirLevel < -1 then ScanDirLevel := -1;
    end;
  end;

//------------------------------------------------------------------------
// More options dialog

procedure TFormSearchFileDlg.ButtonOptionsClick(Sender: TObject);

  begin
  with FormMoreOptions do
    if ShowModal = mrOK then
      begin
      CheckBoxUseMoreOptions.Checked := true;
      end;
  end;

//------------------------------------------------------------------------

procedure TFormSearchFileDlg.ButtonHelpClick(Sender: TObject);

  begin
  Application.HelpContext(230);
  end;

//------------------------------------------------------------------------

procedure TFormSearchFileDlg.FormCreate(Sender: TObject);

  begin
  ComboBoxScanDirLevel.ItemIndex := 0;
  end;

//------------------------------------------------------------------------

procedure TFormSearchFileDlg.ComboBoxMaskChange(Sender: TObject);

  begin
  CheckBoxAddWildCards.Enabled := (Pos('*', ComboBoxMask.Text) = 0) and
                                  (length(ComboBoxMask.Text) > 0);
  CheckBoxPhrase.Enabled := (Pos(' ', ComboBoxMask.Text) <> 0) and
                            (Pos('"', ComboBoxMask.Text) = 0);
  end;

//------------------------------------------------------------------------
// Saves the values from this dialog box to an INI file

procedure TFormSearchFileDlg.SaveToIni(var IniFile: TIniFile);

  const
    Section = 'SearchDlg';
    MaxItems = 15;
  var
    i: integer;
    Items: integer;

  begin
  IniFile.WriteString (Section, 'Mask', ComboBoxMask.Text);

  Items := ComboBoxMask.Items.Count;
  if Items > MaxItems then Items := MaxItems;
  for i := 1 to Items do
    IniFile.WriteString (Section, 'Mask'+IntToStr(i), ComboBoxMask.Items.Strings[i-1]);
  for i := Items+1 to MaxItems do
    IniFile.WriteString (Section, 'Mask'+IntToStr(i), '');

  IniFile.WriteInteger(Section, 'Search',         RadioGroupSearch.ItemIndex);
  IniFile.WriteBool   (Section, 'DiskNames',      CheckBoxDiskNames.Checked);
  IniFile.WriteBool   (Section, 'FileNames',      CheckBoxFileNames.Checked);
  IniFile.WriteBool   (Section, 'SearchDirs',     CheckBoxFolderNames.Checked);
  IniFile.WriteBool   (Section, 'SearchDesc',     CheckBoxDescriptions.Checked);
  IniFile.WriteBool   (Section, 'AddWildcards',   CheckBoxAddWildCards.Checked);
  IniFile.WriteBool   (Section, 'Phrase',         CheckBoxPhrase.Checked);
  IniFile.WriteBool   (Section, 'CaseSensitive',  CheckBoxCaseSensitive.Checked);
  IniFile.WriteBool   (Section, 'StrictMask',     CheckBoxStrictMask.Checked);
  IniFile.WriteInteger(Section, 'ScanDirLevel',   ComboBoxScanDirLevel.ItemIndex);
  IniFile.WriteInteger(Section, 'MaxRecs',        SpinEditMaxRecs.Value);
  IniFile.WriteBool   (Section, 'UseMoreOptions', CheckBoxUseMoreOptions.Checked);
  end;

//------------------------------------------------------------------------
// Loads the values for this dialog box from an INI file

procedure TFormSearchFileDlg.LoadFromIni(var IniFile: TIniFile);

  const
    Section = 'SearchDlg';
    MaxItems = 15;
  var
    i: integer;
    //Items: integer;
    S : ShortString;

  begin
  ComboBoxMask.Text := IniFile.ReadString (Section, 'Mask', '');
  for i := 1 to MaxItems do
    begin
    S := IniFile.ReadString (Section, 'Mask'+IntToStr(i), '');
    if S <> ''
      then ComboBoxMask.Items.Add(S)
      else break;
    end;

  RadioGroupSearch.ItemIndex     := IniFile.ReadInteger(Section, 'Search', 0);
  CheckBoxDiskNames.Checked      := IniFile.ReadBool   (Section, 'DiskNames', true);
  CheckBoxFileNames.Checked      := IniFile.ReadBool   (Section, 'FileNames', true);
  CheckBoxFolderNames.Checked    := IniFile.ReadBool   (Section, 'SearchDirs', true);
  CheckBoxDescriptions.Checked   := IniFile.ReadBool   (Section, 'SearchDesc', true);
  CheckBoxAddWildCards.Checked   := IniFile.ReadBool   (Section, 'AddWildcards', false);
  CheckBoxPhrase.Checked         := IniFile.ReadBool   (Section, 'Phrase', false);
  CheckBoxCaseSensitive.Checked  := IniFile.ReadBool   (Section, 'CaseSensitive', false);
  CheckBoxStrictMask.Checked     := IniFile.ReadBool   (Section, 'StrictMask', false);
  ComboBoxScanDirLevel.ItemIndex := IniFile.ReadInteger(Section, 'ScanDirLevel', 0);
  SpinEditMaxRecs.Value          := IniFile.ReadInteger(Section, 'MaxRecs', 1000);
  CheckBoxUseMoreOptions.Checked := IniFile.ReadBool   (Section, 'UseMoreOptions', false);
  end;

//------------------------------------------------------------------------


end.
