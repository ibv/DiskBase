unit FDescription;
(*====================================================================
Edit window for the descriptions
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
  Forms, Dialogs, StdCtrls, ExtCtrls, Menus,
  UTypes;

type
  TFormDescription = class(TForm)
    Panel1: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    MemoDesc: TMemo;
    LabelFile: TLabel;
    MainMenu1: TMainMenu;
    MenuEdit: TMenuItem;
    MenuCut: TMenuItem;
    MenuFile: TMenuItem;
    MenuExitAndSave: TMenuItem;
    MenuExit: TMenuItem;
    MenuCopy: TMenuItem;
    MenuPaste: TMenuItem;
    N1: TMenuItem;
    MenuSelectAll: TMenuItem;
    MenuSearch: TMenuItem;
    MenuFind: TMenuItem;
    MenuNext: TMenuItem;
    FindDialog: TFindDialog;
    ButtonLast: TButton;
    procedure MenuExitAndSaveClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuCutClick(Sender: TObject);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure MemoDescKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure MenuNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoDescChange(Sender: TObject);
    procedure ButtonLastClick(Sender: TObject);
  private
    pLastText: PChar;
    LastTextSize: integer;
  public
    procedure DefaultHandler(var Message); override;
    procedure SetFormSize;
    { Public declarations }
  end;

var
  FormDescription: TFormDescription;

implementation

uses Clipbrd, IniFiles, ULang, FSettings;

{$R *.dfm}

//--------------------------------------------------------------------
// Form creation

procedure TFormDescription.FormCreate(Sender: TObject);

  begin
  LastTextSize := 0;
  pLastText := nil;
  end;

//--------------------------------------------------------------------
// Actions done before showing the window

procedure TFormDescription.FormShow(Sender: TObject);

  begin
  ActiveControl := MemoDesc;
  ButtonLast.Visible := LastTextSize > 0;
  ButtonLast.Enabled := MemoDesc.GetTextLen = 0;
  end;

//--------------------------------------------------------------------
// Handles the Save and Exit event - saves the text to the pLastText, so that
// it can be pasted next time

procedure TFormDescription.MenuExitAndSaveClick(Sender: TObject);
  begin
  if (LastTextSize > 0) then
    begin
    FreeMem(pLastText, LastTextSize);
    pLastText := nil;
    LastTextSize := 0;
    end;
  with MemoDesc do
    if (GetTextLen > 0) then
      begin
      LastTextSize := GetTextLen + 1;
      GetMem(pLastText, LastTextSize);
      GetTextBuf(pLastText, LastTextSize);
      end;
  ModalResult := mrOK;
  end;

//--------------------------------------------------------------------
// Handles the Exit event - warns about not saving modified text

procedure TFormDescription.MenuExitClick(Sender: TObject);

  var
    Query: Integer;

  begin
  if MemoDesc.Modified
    then
      begin
      Query := Application.MessageBox(lsDescrTextModified,
                                      lsDescription, mb_YesNoCancel);
      if Query = IDYes then ModalResult := mrOK;
      if Query = IDNo  then ModalResult := mrCancel;
      end
    else
      ModalResult := mrCancel;
  end;

//--------------------------------------------------------------------

procedure TFormDescription.FormDestroy(Sender: TObject);

  begin
  if (LastTextSize > 0) then
    begin
    FreeMem(pLastText, LastTextSize);
    pLastText := nil;
    LastTextSize := 0;
    end;
  end;

//--------------------------------------------------------------------
// Cuts the text to Clipboard

procedure TFormDescription.MenuCutClick(Sender: TObject);

  begin
  MemoDesc.CutToClipboard;
  end;

//--------------------------------------------------------------------
// Copies the text to clipboard

procedure TFormDescription.MenuCopyClick(Sender: TObject);

  begin
  MemoDesc.CopyToClipboard;
  end;

//--------------------------------------------------------------------
// Pastes the text from clipboard

procedure TFormDescription.MenuPasteClick(Sender: TObject);

  begin
  MemoDesc.PasteFromClipboard;
  end;

//--------------------------------------------------------------------
// Key handler - assures closing the from on Escape or Ctrl-Enter

procedure TFormDescription.MemoDescKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  begin
  if Key = VK_Escape
    then MenuExitClick(Sender);
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    MenuExitAndSaveClick(Sender);
  end;

//--------------------------------------------------------------------
// Enables/disables items in the menu.

procedure TFormDescription.MenuEditClick(Sender: TObject);

  begin
  MenuCopy.Enabled := MemoDesc.SelLength > 0;
  MenuCut.Enabled := MemoDesc.SelLength > 0;
  MenuPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
  end;

//--------------------------------------------------------------------
// Selects all text

procedure TFormDescription.MenuSelectAllClick(Sender: TObject);

  begin
  MemoDesc.SelectAll;
  end;

//--------------------------------------------------------------------
// Displays the find dialog

procedure TFormDescription.MenuFindClick(Sender: TObject);

  begin
  if FindDialog.Execute then
    begin
    end;
  end;

//--------------------------------------------------------------------
// Handles the find action from the find dialog box

procedure TFormDescription.FindDialogFind(Sender: TObject);

  var
    Buffer  : PChar;
    Size    : longint;
    Start   : longint;
    FoundCh, StartCh : PChar;
    StringToFind: array[0..256] of char;

  begin
  ActiveControl := MemoDesc;
  with MemoDesc do
    begin
    Size := GetTextLen;
    Inc(Size);
    GetMem(Buffer, Size);
    GetTextBuf(Buffer, Size);
    Start := SelStart + SelLength;
    StartCh := @Buffer[Start];
    StrPCopy(StringToFind, FindDialog.FindText);
    if not (frMatchCase in FindDialog.Options) then
      begin
      AnsiUpperCase(StringToFind);
      AnsiUpperCase(StartCh);
      end;
    FoundCh := StrPos(StartCh, StringToFind);
    if FoundCh <> nil then
      begin
      ///Start := Start + FoundCh - StartCh;
      SelStart := Start;
      SelLength := length(FindDialog.FindText);
      end;
    FreeMem(Buffer, Size);
    end;
  end;

//--------------------------------------------------------------------
// Repeats the find action

procedure TFormDescription.MenuNextClick(Sender: TObject);

  begin
  FindDialogFind(Sender);
  end;

//--------------------------------------------------------------------
// Called on change in the text

procedure TFormDescription.MemoDescChange(Sender: TObject);

  begin
  ButtonLast.Enabled := MemoDesc.GetTextLen = 0;
  end;

//--------------------------------------------------------------------
// Pastes the text from previous description to the text

procedure TFormDescription.ButtonLastClick(Sender: TObject);

  begin
  MemoDesc.SetTextBuf(pLastText);
  ActiveControl := MemoDesc;
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormDescription.DefaultHandler(var Message);

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

procedure TFormDescription.SetFormSize;

  var
    IniFile: TIniFile;
    SLeft, STop, SWidth, SHeight: integer;

  begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  SLeft   := IniFile.ReadInteger  ('DescriptionWindow', 'Left',
                                   (Screen.Width - Width) div 2);
  STop    := IniFile.ReadInteger  ('DescriptionWindow', 'Top',
                                   (Screen.Height - Height) div 2);
  SWidth  := IniFile.ReadInteger  ('DescriptionWindow', 'Width',  Width);
  SHeight := IniFile.ReadInteger  ('DescriptionWindow', 'Height', Height);
  IniFile.Free;
  SetBounds(SLeft, STop, SWidth, SHeight);
  end;

//--------------------------------------------------------------------

end.
