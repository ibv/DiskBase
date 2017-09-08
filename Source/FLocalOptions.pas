unit FLocalOptions;
(*====================================================================
Dialog box for Loocal Options - which are individual for the database.
The values set are put to the LocalOptions structure
======================================================================*)

interface

uses
  SysUtils,
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Grids, Spin, {TabNotBk,}
  UTypes, UApiTypes
{$ifndef DELPHI1}
  , ComCtrls
{$endif}
  ;

type
  TFormLocalOptions = class(TForm)
    ///TabbedNotebook: TTabbedNotebook;
    TabbedNotebook: TPageControl;
    Page1: TTabSheet;
    Page2: TTabSheet;
    Page3: TTabSheet;
    CheckBoxScanDesc: TCheckBox;
    Label2: TLabel;
    EditNewFileName: TEdit;
    Label1: TLabel;
    SpinEditMaxDescrSize: TSpinEdit;
    Label3: TLabel;
    ComboBoxConv: TComboBox;
    ButtonDiscard: TButton;
    ButtonAdd: TButton;
    StringGridMasks: TStringGrid;
    CheckBoxOverwriteWarning: TCheckBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxShowDeleted: TCheckBox;
    ButtonHelp: TButton;
    CheckBoxSimulateDiskInfo: TCheckBox;
    RadioGroupScanArchives: TRadioGroup;
    CheckBoxScanZipExeArchives: TCheckBox;
    CheckBoxScanOtherExeArchives: TCheckBox;
    CheckBoxAlwaysRescanArchives: TCheckBox;
    CheckBoxExtractFromZips: TCheckBox;
    CheckBoxDisableVolNameChange: TCheckBox;
    EditDiskNamePattern: TEdit;
    CheckBoxImportFilesBbs: TCheckBox;
    CheckBoxGenerateFolderDesc: TCheckBox;
    Label5: TLabel;
    ButtonResetCounter: TButton;
    CheckBoxAlwaysCreateDesc: TCheckBox;
    CheckBoxExtractFromRars: TCheckBox;
    CheckBoxExtractFromAces: TCheckBox;
    Label6: TLabel;
    SpinEditSizeLimit: TSpinEdit;
    procedure StringGridMasksRowMoved(Sender: TObject; FromIndex,
      ToIndex: Longint);
    procedure StringGridMasksKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonDiscardClick(Sender: TObject);
    procedure CheckBoxScanDescClick(Sender: TObject);
    procedure RadioGroupScanArchivesClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure ButtonResetCounterClick(Sender: TObject);
  private
    SaveOptions  : TLocalOptions;
    m_DiskCounter: word;
    procedure DeleteRow(TheRow: Integer);
  public
    procedure SetOptions(var LocalOptions: TLocalOptions);
    procedure GetOptions(var LocalOptions: TLocalOptions);
    procedure SetDLList (ConvertDLLs: TStringList);
    procedure DefaultHandler(var Message); override;
  end;

var
  FormLocalOptions: TFormLocalOptions;

implementation

uses UBaseUtils, ULang, FSettings;

{$R *.dfm}

//---------------------------------------------------------------------------

procedure TFormLocalOptions.StringGridMasksRowMoved(Sender: TObject;
  FromIndex, ToIndex: Longint);

  var
  i: Integer;

  begin
  with StringGridMasks do
    begin
    for i := 1 to pred(RowCount) do
      Cells[0, i] := ' ' + IntToStr(i);
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.StringGridMasksKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);

  begin
  if Key = VK_DELETE then DeleteRow(StringGridMasks.Row);
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.DeleteRow(TheRow: Integer);

  var
    i : Integer;

  begin
  with StringGridMasks do
    begin
    for i := succ(TheRow) to pred(RowCount) do
      begin
      Cells[1, i-1] := Cells[1, i];
      Cells[2, i-1] := Cells[2, i];
      Cells[3, i-1] := Cells[3, i];
      end;
    Cells[1, pred(RowCount)] := '';
    Cells[2, pred(RowCount)] := '';
    Cells[3, pred(RowCount)] := '';
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.ButtonDiscardClick(Sender: TObject);

  var
    i : Integer;
    S : ShortString;

  begin
  with StringGridMasks do
    begin
    EditNewFileName.Text := Cells[1, Row];
    if Cells[2, Row] <> '' then
      SpinEditMaxDescrSize.Value := StrToInt(Cells[2, Row]);
    S := Cells[3, Row];
    if S <> '' then
      begin
      ComboBoxConv.Text := ComboBoxConv.Items[0];
      for i := 0 to pred(ComboBoxConv.Items.Count) do
        if ShortCopy(S, 1, 1) = ShortCopy(ComboBoxConv.Items[i], 1, 1) then
          begin
          ComboBoxConv.Text := ComboBoxConv.Items[i];
          break;
          end;
      end;
    DeleteRow(Row);
    end;
  ActiveControl := EditNewFileName;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.CheckBoxScanDescClick(Sender: TObject);

  begin
  if CheckBoxScanDesc.Checked
    then
      begin
      StringGridMasks.Enabled      := true;
      EditNewFileName.Enabled      := true;
      SpinEditMaxDescrSize.Enabled := true;
      ComboBoxConv.Enabled         := true;
      end
    else
      begin
      StringGridMasks.Enabled      := false;
      EditNewFileName.Enabled      := false;
      SpinEditMaxDescrSize.Enabled := false;
      ComboBoxConv.Enabled         := false;
      end;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.RadioGroupScanArchivesClick(Sender: TObject);

  var Enable: boolean;

  begin
  Enable := RadioGroupScanArchives.ItemIndex <> 2;
  CheckBoxScanZipExeArchives.Enabled   := Enable;
  CheckBoxScanOtherExeArchives.Enabled := Enable;
  CheckBoxAlwaysRescanArchives.Enabled := Enable;
  CheckBoxExtractFromZips.Enabled      := Enable;
  CheckBoxExtractFromRars.Enabled      := Enable;
  CheckBoxExtractFromAces.Enabled      := Enable;
  SpinEditSizeLimit.Enabled            := Enable;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.ButtonAddClick(Sender: TObject);

  var
    Ch: string[2];
    i, j : Integer;

  begin
  if EditNewFileName.Text <> '' then
    with StringGridMasks do
      begin
      if (Cells[1, pred(RowCount)] <> '') then exit; {je plno}
      for i := 1 to Row do
        if (Cells[1, i] = '') or (i = Row) then
          begin
          if i = Row then
            begin
            for j := pred(RowCount) downto succ(Row) do
              begin
              Cells[1, j] := Cells[1, j-1];
              Cells[2, j] := Cells[2, j-1];
              Cells[3, j] := Cells[3, j-1];
              end;
            Cells[1, i] := '';
            Cells[2, i] := '';
            Cells[3, i] := '';
            end;
          Cells[1, i] := RemoveRedundSpaces(EditNewFileName.Text);
          Cells[2, i] := IntToStr(SpinEditMaxDescrSize.Value);
          Ch := ShortCopy(ComboBoxConv.Text, 1, 1);
          if Ch <> '(' then Cells[3, i] := ComboBoxConv.Text;
          break;
          end;
      end;

  ActiveControl := EditNewFileName;
  EditNewFileName.Text := '';
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.FormCreate(Sender: TObject);

  var
    i : Integer;
  begin
  m_DiskCounter := 1;
  with StringGridMasks do
    begin
    DefaultRowHeight := Canvas.TextHeight('My')+4;
    Cells[1, 0] := lsMask;
    Cells[2, 0] := lskB;
    Cells[3, 0] := lsDLL;
    for i := 1 to pred(RowCount) do
      Cells[0, i] := ' ' + IntToStr(i);
    end;

  {$ifdef DELPHI1}
  CheckBoxDisableVolNameChange.Visible := false;
  {$endif}
  TabbedNotebook.ActivePage := TabbedNotebook.Pages[0];
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.SetOptions(var LocalOptions: TLocalOptions);

  var
    i, TmpInt: Integer;
    Mask, kBytes, CodeConv: ShortString;
    MaskCount: Integer;

  begin
  with LocalOptions do
    begin
    CheckBoxShowDeleted.Checked       := ShowDeleted;
    CheckBoxSimulateDiskInfo.Checked  := SimulateDiskInfo;
    CheckBoxAlwaysCreateDesc.Checked  := AlwaysCreateDesc;
    CheckBoxScanZipExeArchives.Checked   := ScanZipExeArchives;
    CheckBoxScanOtherExeArchives.Checked := ScanOtherExeArchives;
    CheckBoxAlwaysRescanArchives.Checked := AlwaysRescanArchives;
    CheckBoxExtractFromZips.Checked      := ExtractFromZips;
    CheckBoxExtractFromRars.Checked      := ExtractFromRars;
    CheckBoxExtractFromAces.Checked      := ExtractFromAces;
    if (ExtractSizeLimit > 0) then
      SpinEditSizeLimit.Value := ExtractSizeLimit;
    RadioGroupScanArchives.ItemIndex  := Integer(ScanArchives);
    CheckBoxScanDesc.Checked          := ScanDesc;
    CheckBoxOverwriteWarning.Checked  := OverwriteWarning;

    CheckBoxDisableVolNameChange.Checked := DisableVolNameChange;
    EditDiskNamePattern.Text             := DiskNamePattern;
    m_DiskCounter                        := DiskCounter;
    CheckBoxGenerateFolderDesc.Checked   := GenerateFolderDesc;
    CheckBoxImportFilesBbs.Checked       := ImportFilesBbs;

    for i := 1 to pred(StringGridMasks.RowCount) do
      begin
      StringGridMasks.Cells[1, i] := '';
      StringGridMasks.Cells[2, i] := '';
      StringGridMasks.Cells[3, i] := '';
      end;
    i := 0;
    MaskCount := 0;
    while AllMasksArray[i] <> #0 do
      begin
      Mask := '';
      while (AllMasksArray[i] <> '|') and (AllMasksArray[i] <> #0) do
        begin
        Mask := Mask + AllMasksArray[i];
        inc(i);
        end;

      if (AllMasksArray[i] <> #0) then inc(i);
      kBytes := '';
      while (AllMasksArray[i] <> '|') and (AllMasksArray[i] <> #0) do
        begin
        kBytes := kBytes + AllMasksArray[i];
        inc(i);
        end;

      if (AllMasksArray[i] <> #0) then inc(i);
      CodeConv := '';
      while (AllMasksArray[i] <> '|') and (AllMasksArray[i] <> #0) do
        begin
        CodeConv := CodeConv + AllMasksArray[i];
        inc(i);
        end;

      if (AllMasksArray[i] <> #0) then inc(i);
      inc(MaskCount);

      with StringGridMasks do
        begin
        Cells[1,MaskCount] := Mask;
        TmpInt := StrToInt(kBytes);
        if TmpInt < 100 then kBytes := IntToStr(TmpInt * 1000);
        // relict, from the time there were kB here, now bytes are used
        Cells[2,MaskCount] := kBytes;
        Cells[3,MaskCount] := CodeConv;
        end;
      end; {while}
    end; {with Local Options}
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.GetOptions(var LocalOptions: TLocalOptions);

  var
    i: Integer;
    PS: array[0..255] of char;
    S : ShortString;

  begin
  with LocalOptions do
    begin
    ShowDeleted          := CheckBoxShowDeleted.Checked;
    SimulateDiskInfo     := CheckBoxSimulateDiskInfo.Checked;
    AlwaysCreateDesc     := CheckBoxAlwaysCreateDesc.Checked;
    ScanZipExeArchives   := CheckBoxScanZipExeArchives.Checked;
    AlwaysRescanArchives := CheckBoxAlwaysRescanArchives.Checked;
    ExtractFromZips      := CheckBoxExtractFromZips.Checked;
    ExtractFromRars      := CheckBoxExtractFromRars.Checked;
    ExtractFromAces      := CheckBoxExtractFromAces.Checked;
    ExtractSizeLimit     := SpinEditSizeLimit.Value;
    ScanOtherExeArchives := CheckBoxScanOtherExeArchives.Checked;
    ScanArchives         := TScan(RadioGroupScanArchives.ItemIndex);
    ScanCPB              := cfNoScan;
    ScanDesc             := CheckBoxScanDesc.Checked;
    OverwriteWarning     := CheckBoxOverwriteWarning.Checked;

    DisableVolNameChange := CheckBoxDisableVolNameChange.Checked;
    DiskNamePattern      := TrimSpaces(EditDiskNamePattern.Text);
    DiskCounter          := m_DiskCounter;
    GenerateFolderDesc   := CheckBoxGenerateFolderDesc.Checked;
    ImportFilesBbs       := CheckBoxImportFilesBbs.Checked;

    AllMasksArray[0] := #0;                  
    with StringGridMasks do
      for i := 1 to pred(RowCount) do
        if Cells[1,i] <> '' then
          begin
          S := Cells[1,i]+'|'+Cells[2,i]+'|'+Cells[3,i] + '|';
          StrCat(AllMasksArray, StrPCopy(PS, S));
          end;
    end;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.FormShow(Sender: TObject);

  begin
  GetOptions(SaveOptions);
  RadioGroupScanArchivesClick(Sender);
  ActiveControl := ButtonOK;
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.ButtonCancelClick(Sender: TObject);

  begin
  SetOptions(SaveOptions);
  ModalResult := mrCancel;
  end;

//---------------------------------------------------------------------------
// set the list of all filters

procedure TFormLocalOptions.SetDLList (ConvertDLLs: TStringList);

  var
    SaveStr: ShortString;
    i      : Integer;

  begin
  SaveStr := ComboBoxConv.Items[0];
  ComboBoxConv.Items.Clear;
  ComboBoxConv.Items.Add(SaveStr);
  for i := 1 to ConvertDLLs.Count do
    ComboBoxConv.Items.Add(ConvertDLLs.Strings[pred(i)]);
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.ButtonHelpClick(Sender: TObject);

  begin
  Application.HelpContext(300);
  end;

//---------------------------------------------------------------------------

procedure TFormLocalOptions.ButtonResetCounterClick(Sender: TObject);

  begin
  m_DiskCounter := 0;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormLocalOptions.DefaultHandler(var Message);

  begin
  with TMessage(Message) do
    if (Msg = g_CommonOptions.dwQueryCancelAutoPlay) and
        g_CommonOptions.bDisableCdAutorun
      then
        Result := 1
      else
        inherited DefaultHandler(Message)
  end;

//---------------------------------------------------------------------------

end.
