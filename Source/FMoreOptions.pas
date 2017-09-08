unit FMoreOptions;
(*====================================================================
Dialog box for extended options for file search
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
  UTypes;

type
  TFormMoreOptions = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    GroupBox1: TGroupBox;
    ///SpinButtonD1: TSpinButton;
    ///SpinButtonM1: TSpinButton;
    ///SpinButtonY1: TSpinButton;
    ///SpinButtonD2: TSpinButton;
    ///SpinButtonM2: TSpinButton;
    //SpinButtonY2: TSpinButton;
    SpinButtonD1: TSpinEdit;
    SpinButtonM1: TSpinEdit;
    SpinButtonY1: TSpinEdit;
    SpinButtonD2: TSpinEdit;
    SpinButtonM2: TSpinEdit;
    SpinButtonY2: TSpinEdit;
    EditDateTo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditDateFrom: TEdit;
    GroupBox2: TGroupBox;
    CheckBoxDateFrom: TCheckBox;
    CheckBoxDateTo: TCheckBox;
    CheckBoxSizeFrom: TCheckBox;
    CheckBoxSizeTo: TCheckBox;
    ComboBoxExclude: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SpinEditSizeFrom: TSpinEdit;
    SpinEditSizeTo: TSpinEdit;
    ButtonHelp: TButton;
    RadioGroupKey3: TRadioGroup;
    RadioGroupKey2: TRadioGroup;
    RadioGroupKey1: TRadioGroup;
    Label7: TLabel;
    ComboBoxDirMask: TComboBox;
    Label8: TLabel;
    CheckBoxSearchAsAnsi: TCheckBox;
    CheckBoxSearchAsOem: TCheckBox;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinButtonD1UpClick(Sender: TObject);
    procedure SpinButtonM1UpClick(Sender: TObject);
    procedure SpinButtonY1UpClick(Sender: TObject);
    procedure SpinButtonD1DownClick(Sender: TObject);
    procedure SpinButtonM1DownClick(Sender: TObject);
    procedure SpinButtonY1DownClick(Sender: TObject);
    procedure SpinButtonD2UpClick(Sender: TObject);
    procedure SpinButtonD2DownClick(Sender: TObject);
    procedure SpinButtonM2UpClick(Sender: TObject);
    procedure SpinButtonM2DownClick(Sender: TObject);
    procedure SpinButtonY2UpClick(Sender: TObject);
    procedure SpinButtonY2DownClick(Sender: TObject);
    procedure SpinEditSizeToChange(Sender: TObject);
    procedure SpinEditSizeFromChange(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure CheckBoxSearchAsAnsiClick(Sender: TObject);
    procedure CheckBoxSearchAsOemClick(Sender: TObject);
  private
    FDateFrom: TDateTime;
    FDateTo  : TDateTime;

    SaveExclude        : ShortString;
    SaveDateFromChecked: boolean;
    SaveDateToChecked  : boolean;
    SaveDateFrom       : ShortString;
    SaveDateTo         : ShortString;
    SaveSizeFromChecked: boolean;
    SaveSizeToChecked  : boolean;
    SaveSizeFrom       : integer;
    SaveSizeTo         : integer;
    SaveDirMask        : ShortString;
    SaveKey1           : integer;
    SaveKey2           : integer;
    SaveKey3           : integer;
    SaveSearchAsAnsi   : boolean;
    SaveSearchAsOem    : boolean;

    procedure IncDate(Edit: TEdit; var FDate: TDateTime;
                      Year, Month, Day: boolean);
    procedure DecDate(Edit: TEdit; var FDate: TDateTime;
                      Year, Month, Day: boolean);
    procedure EditToDate(Edit: TEdit; var FDate: TDateTime);
    procedure SaveDlgData;
    procedure RestoreDlgData; {pro storno}
  public
    DateFrom: longint;
    DateTo  : longint;
    SizeFrom: longint;
    SizeTo  : longint;
    procedure SaveToIni   (var IniFile: TIniFile);
    procedure LoadFromIni (var IniFile: TIniFile);
    procedure DefaultHandler(var Message); override;
  end;

var
  FormMoreOptions: TFormMoreOptions;

implementation

uses FSettings;
{$R *.dfm}

//-----------------------------------------------------------------------------
// Button OK handler - process the values from the time/date options and save
// combo boxes history

procedure TFormMoreOptions.ButtonOKClick(Sender: TObject);

  var
    Found: boolean;
    i    : Integer;
  begin
  if CheckBoxDateFrom.Checked
    then DateFrom := DateTimeToFileDate(FDateFrom)
    else DateFrom := 0;
  if CheckBoxDateTo.Checked
    then DateTo   := DateTimeToFileDate(FDateTo+1) {do zac. dalsiho dne}
    else DateTo   := 0;

  if CheckBoxSizeFrom.Checked
    then SizeFrom := SpinEditSizeFrom.Value * 1024
    else SizeFrom := 0;
  if CheckBoxSizeTo.Checked
    then SizeTo   := SpinEditSizeTo.Value * 1024
    else SizeTo   := 0;

  if ComboBoxExclude.Text <> '' then
    begin
    Found := false;
    for i := 0 to pred(ComboBoxExclude.Items.Count) do
      if ComboBoxExclude.Items.Strings[i] = ComboBoxExclude.Text then
        Found := true;
    if not Found then ComboBoxExclude.Items.Insert(0, ComboBoxExclude.Text);
    end;

  if ComboBoxDirMask.Text <> '' then
    begin
    Found := false;
    for i := 0 to pred(ComboBoxDirMask.Items.Count) do
      if ComboBoxDirMask.Items.Strings[i] = ComboBoxDirMask.Text then
        Found := true;
    if not Found then ComboBoxDirMask.Items.Insert(0, ComboBoxDirMask.Text);
    end;

  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.ButtonCancelClick(Sender: TObject);

  begin
  RestoreDlgData;
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.FormCreate(Sender: TObject);

  begin
  FDateFrom := Date;
  FDateTo   := Date;
  SizeFrom  := 0;
  SizeTo    := 0;
  EditDateFrom.Text := DateToStr(FDateFrom);
  EditDateTo.Text   := DateToStr(FDateTo);
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.FormShow(Sender: TObject);

  begin
  SaveDlgData;
  ActiveControl := ComboBoxExclude;
  end;

//-----------------------------------------------------------------------------
// Increments date by 1 day or 1 month or 1 year

procedure TFormMoreOptions.IncDate(Edit: TEdit; var FDate: TDateTime;
                                   Year, Month, Day: boolean);

  var
    Y, M, D: word;
  begin
  if Day   then FDate := FDate + 1;
  if Month then
    begin
    DecodeDate(FDate, Y, M, D);
    if M = 12
      then
        FDate := FDate + 31
      else
        begin
        inc(M);
        case M of
          4, 6, 9, 11 : if D > 30 then D := 30;
          2: if Y mod 4 = 0
           then if D > 29 then D := 29 else
           else if D > 28 then D := 28;
           end;
        FDate := EncodeDate(Y, M, D);
        end;
    end;
  if Year then
    begin
    DecodeDate(FDate, Y, M, D);
    if M <= 2
      then
        if Y mod 4 = 0
          then FDate := FDate + 366
          else FDate := FDate + 365
      else
        if (Y+1) mod 4 = 0
          then FDate := FDate + 366
          else FDate := FDate + 365;
    end;
  Edit.Text := DateToStr(FDate);
  end;

//-----------------------------------------------------------------------------
// Decrements date by 1 day or 1 month or 1 year

procedure TFormMoreOptions.DecDate(Edit: TEdit; var FDate: TDateTime;
                                   Year, Month, Day: boolean);

  var
    Y, M, D: word;
  begin
  if Day   then FDate := FDate - 1;
  if Month then
    begin
    DecodeDate(FDate, Y, M, D);
    if M = 1
      then
        FDate := FDate - 31
      else
        begin
        dec(M);
        case M of
          4, 6, 9, 11 : if D > 30 then D := 30;
          2: if Y mod 4 = 0
           then if D > 29 then D := 29 else
           else if D > 28 then D := 28;
           end;
        FDate := EncodeDate(Y, M, D);
        end;
    end;
  if Year then
    begin
    DecodeDate(FDate, Y, M, D);
    if M > 2
      then
        if Y mod 4 = 0
          then FDate := FDate - 366
          else FDate := FDate - 365
      else
        if (Y-1) mod 4 = 0
          then FDate := FDate - 366
          else FDate := FDate - 365;
    end;
  Edit.Text := DateToStr(FDate);
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonD1UpClick(Sender: TObject);

  begin
  EditToDate(EditDateFrom, FDateFrom);
  IncDate(EditDateFrom, FDateFrom, false, false, true);
  CheckBoxDateFrom.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonM1UpClick(Sender: TObject);

  begin
  EditToDate(EditDateFrom, FDateFrom);
  IncDate(EditDateFrom, FDateFrom, false, true, false);
  CheckBoxDateFrom.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonY1UpClick(Sender: TObject);

  begin
  EditToDate(EditDateFrom, FDateFrom);
  IncDate(EditDateFrom, FDateFrom, true, false, false);
  CheckBoxDateFrom.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonD1DownClick(Sender: TObject);

  begin
  EditToDate(EditDateFrom, FDateFrom);
  DecDate(EditDateFrom, FDateFrom, false, false, true);
  CheckBoxDateFrom.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonM1DownClick(Sender: TObject);

  begin
  EditToDate(EditDateFrom, FDateFrom);
  DecDate(EditDateFrom, FDateFrom, false, true, false);
  CheckBoxDateFrom.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonY1DownClick(Sender: TObject);

  begin
  EditToDate(EditDateFrom, FDateFrom);
  DecDate(EditDateFrom, FDateFrom, true, false, false);
  CheckBoxDateFrom.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonD2UpClick(Sender: TObject);

  begin
  EditToDate(EditDateTo, FDateTo);
  IncDate(EditDateTo, FDateTo, false, false, true);
  CheckBoxDateTo.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonM2UpClick(Sender: TObject);

  begin
  EditToDate(EditDateTo, FDateTo);
  IncDate(EditDateTo, FDateTo, false, true, false);
  CheckBoxDateTo.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonY2UpClick(Sender: TObject);

  begin
  EditToDate(EditDateTo, FDateTo);
  IncDate(EditDateTo, FDateTo, true, false, false);
  CheckBoxDateTo.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonD2DownClick(Sender: TObject);

  begin
  EditToDate(EditDateTo, FDateTo);
  DecDate(EditDateTo, FDateTo, false, false, true);
  CheckBoxDateTo.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonM2DownClick(Sender: TObject);

  begin
  EditToDate(EditDateTo, FDateTo);
  DecDate(EditDateTo, FDateTo, false, true, false);
  CheckBoxDateTo.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinButtonY2DownClick(Sender: TObject);

  begin
  EditToDate(EditDateTo, FDateTo);
  DecDate(EditDateTo, FDateTo, true, false, false);
  CheckBoxDateTo.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.EditToDate(Edit: TEdit; var FDate: TDateTime);

  var
    SaveFDate : TDateTime;
  begin
    SaveFDate := FDate;
  try
    FDate := StrToDate(Edit.Text);
  except
    on EConvertError do
      begin
      FDate := SaveFDate;
      Edit.Text := DateToStr(FDate);
      end;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinEditSizeToChange(Sender: TObject);

  begin
  CheckBoxSizeTo.Checked := true;
  try
    if SpinEditSizeTo.Value < 0 then SpinEditSizeTo.Value := 0;
    if SpinEditSizeFrom.Value > SpinEditSizeTo.Value then
      SpinEditSizeFrom.Value := SpinEditSizeTo.Value;
  except
    on EConvertError do;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SpinEditSizeFromChange(Sender: TObject);

  begin
  CheckBoxSizeFrom.Checked := true;
  try
    if SpinEditSizeFrom.Value < 0 then SpinEditSizeFrom.Value := 0;
    if SpinEditSizeTo.Value < SpinEditSizeFrom.Value then
      SpinEditSizeTo.Value := SpinEditSizeFrom.Value;
  except
    on EConvertError do;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.ButtonHelpClick(Sender: TObject);

  begin
  Application.HelpContext(230);
  end;

//-----------------------------------------------------------------------------
// Save dialog data for the case the user presses Cancel

procedure TFormMoreOptions.SaveDlgData;

  begin
  SaveExclude        := ComboBoxExclude.Text;
  SaveDateFromChecked:= CheckBoxDateFrom.Checked;
  SaveDateToChecked  := CheckBoxDateTo.Checked;
  SaveDateFrom       := EditDateFrom.Text;
  SaveDateTo         := EditDateTo.Text;
  SaveSizeFromChecked:= CheckBoxSizeFrom.Checked;
  SaveSizeToChecked  := CheckBoxSizeTo.Checked;
  SaveSizeFrom       := SpinEditSizeFrom.Value;
  SaveSizeTo         := SpinEditSizeTo.Value;
  SaveDirMask        := ComboBoxDirMask.Text;
  SaveKey1           := RadioGroupKey1.ItemIndex;
  SaveKey2           := RadioGroupKey2.ItemIndex;
  SaveKey3           := RadioGroupKey3.ItemIndex;
  SaveSearchAsAnsi   := CheckBoxSearchAsAnsi.Checked;
  SaveSearchAsOem    := CheckBoxSearchAsOem.Checked;
  end;

//-----------------------------------------------------------------------------
// Restore dialog data from saved

procedure TFormMoreOptions.RestoreDlgData;

  begin
  ComboBoxExclude.Text     := SaveExclude;
  CheckBoxDateFrom.Checked := SaveDateFromChecked;
  CheckBoxDateTo.Checked   := SaveDateToChecked;
  EditDateFrom.Text        := SaveDateFrom;
  EditDateTo.Text          := SaveDateTo;
  CheckBoxSizeFrom.Checked := SaveSizeFromChecked;
  CheckBoxSizeTo.Checked   := SaveSizeToChecked;
  SpinEditSizeFrom.Value   := SaveSizeFrom;
  SpinEditSizeTo.Value     := SaveSizeTo;
  ComboBoxDirMask.Text     := SaveDirMask;
  RadioGroupKey1.ItemIndex := SaveKey1;
  RadioGroupKey2.ItemIndex := SaveKey2;
  RadioGroupKey3.ItemIndex := SaveKey3;
  CheckBoxSearchAsOem.Checked  := SaveSearchAsOem;
  CheckBoxSearchAsAnsi.Checked := SaveSearchAsAnsi;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.SaveToIni(var IniFile: TIniFile);

  const
    Section = 'MoreSearchDlg';
    MaxItems = 15;
  var
    i: integer;
    Items: integer;

  begin
  IniFile.WriteString (Section, 'Exclude', ComboBoxExclude.Text);
  Items := ComboBoxExclude.Items.Count;
  if Items > MaxItems then Items := MaxItems;
  for i := 1 to Items do
    IniFile.WriteString (Section, 'Exclude'+IntToStr(i), ComboBoxExclude.Items.Strings[i-1]);
  for i := Items+1 to MaxItems do
    IniFile.WriteString (Section, 'Exclude'+IntToStr(i), '');

  IniFile.WriteBool   (Section, 'DateFromChecked', CheckBoxDateFrom.Checked);
  IniFile.WriteBool   (Section, 'DateToChecked', CheckBoxDateTo.Checked);
  IniFile.WriteString (Section, 'DateFrom', EditDateFrom.Text);
  IniFile.WriteString (Section, 'DateTo', EditDateTo.Text);
  IniFile.WriteBool   (Section, 'SizeFromChecked', CheckBoxSizeFrom.Checked);
  IniFile.WriteBool   (Section, 'SizeToChecked', CheckBoxSizeTo.Checked);
  IniFile.WriteInteger(Section, 'SizeFrom', SpinEditSizeFrom.Value);
  IniFile.WriteInteger(Section, 'SizeTo', SpinEditSizeTo.Value);
  IniFile.WriteString (Section, 'DirMask', ComboBoxDirMask.Text);
  Items := ComboBoxDirMask.Items.Count;
  if Items > MaxItems then Items := MaxItems;
  for i := 1 to Items do
    IniFile.WriteString (Section, 'DirMask'+IntToStr(i), ComboBoxDirMask.Items.Strings[i-1]);
  for i := Items+1 to MaxItems do
    IniFile.WriteString (Section, 'DirMask'+IntToStr(i), '');

  IniFile.WriteInteger(Section, 'Key1', RadioGroupKey1.ItemIndex);
  IniFile.WriteInteger(Section, 'Key2', RadioGroupKey2.ItemIndex);
  IniFile.WriteInteger(Section, 'Key3', RadioGroupKey3.ItemIndex);

  IniFile.WriteBool   (Section, 'SearchAsAnsi', CheckBoxSearchAsAnsi.Checked);
  IniFile.WriteBool   (Section, 'SearchAsOem',  CheckBoxSearchAsOem.Checked);
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.LoadFromIni(var IniFile: TIniFile);

  const
    Section = 'MoreSearchDlg';
    MaxItems = 15;
  var
    i: integer;
    S : ShortString;

  begin
  ComboBoxExclude.Text := IniFile.ReadString (Section, 'Exclude', '');
  for i := 1 to MaxItems do
    begin
    S := IniFile.ReadString (Section, 'Exclude'+IntToStr(i), '');
    if S <> ''
      then ComboBoxExclude.Items.Add(S)
      else break;
    end;

  EditDateFrom.Text        := IniFile.ReadString (Section, 'DateFrom', DateToStr(FDateFrom));
  EditDateTo.Text          := IniFile.ReadString (Section, 'DateTo',   DateToStr(FDateTo));
  CheckBoxDateFrom.Checked := IniFile.ReadBool   (Section, 'DateFromChecked', false);
  CheckBoxDateTo.Checked   := IniFile.ReadBool   (Section, 'DateToChecked', false);

  SpinEditSizeFrom.Value   := IniFile.ReadInteger(Section, 'SizeFrom', 0);
  SpinEditSizeTo.Value     := IniFile.ReadInteger(Section, 'SizeTo', 0);
  // this must be AFTER SpinEdit
  CheckBoxSizeFrom.Checked := IniFile.ReadBool   (Section, 'SizeFromChecked', false);
  CheckBoxSizeTo.Checked   := IniFile.ReadBool   (Section, 'SizeToChecked', false);

  ComboBoxDirMask.Text     := IniFile.ReadString (Section, 'DirMask', '');
  for i := 1 to MaxItems do
    begin
    S := IniFile.ReadString (Section, 'DirMask'+IntToStr(i), '');
    if S <> ''
      then ComboBoxDirMask.Items.Add(S)
      else break;
    end;

  RadioGroupKey1.ItemIndex := IniFile.ReadInteger(Section, 'Key1', 1);
  RadioGroupKey2.ItemIndex := IniFile.ReadInteger(Section, 'Key2', 4);
  RadioGroupKey3.ItemIndex := IniFile.ReadInteger(Section, 'Key3', 2);

  CheckBoxSearchAsOem.Checked  := IniFile.ReadBool (Section, 'SearchAsOem', false);
  CheckBoxSearchAsAnsi.Checked := IniFile.ReadBool (Section, 'SearchAsAnsi', true);
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.CheckBoxSearchAsAnsiClick(Sender: TObject);

  begin
  if not CheckBoxSearchAsOem.Checked then CheckBoxSearchAsAnsi.Checked := true;
  end;

//-----------------------------------------------------------------------------

procedure TFormMoreOptions.CheckBoxSearchAsOemClick(Sender: TObject);

  begin
  if not CheckBoxSearchAsOem.Checked then CheckBoxSearchAsAnsi.Checked := true;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormMoreOptions.DefaultHandler(var Message);

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
