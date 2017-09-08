unit FSelectFolder;
(*====================================================================
Dialog box for selecting the folder to be scanned.
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
  Forms, Dialogs, StdCtrls, Grids, Outline, {DirOutln,} ExtCtrls, ComCtrls;
  //DirTreeView;

type
  TFormSelectFolder = class(TForm)
    Panel1: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    DirTreeView: TTreeView;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure DirTreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
    procedure DirTreeViewClick(Sender: TObject);
    procedure DirTreeViewCollapsed(Sender: TObject; Node: TTreeNode);
  private
    procedure ScanSubfolders(sPath: AnsiString; pNode: TTreeNode);
    procedure InitTreeView();
    function  GetFullPath(pNode: TTreeNode): AnsiString;
  public
    Directory: AnsiString;
    procedure DefaultHandler(var Message); override;
    procedure SetFormSize;
  end;


var
  FormSelectFolder: TFormSelectFolder;

implementation

uses IniFiles, FSettings;

{$R *.dfm}

//-----------------------------------------------------------------------------

procedure TFormSelectFolder.ButtonOKClick(Sender: TObject);

  begin
  ModalResult := mrOK;
  end;

//-----------------------------------------------------------------------------

procedure TFormSelectFolder.ButtonCancelClick(Sender: TObject);

  begin
  ModalResult := mrCancel;
  end;

//-----------------------------------------------------------------------------
// Resizable form - restore the last size

procedure TFormSelectFolder.SetFormSize;

  var
    IniFile: TIniFile;
    SLeft, STop, SWidth, SHeight: integer;

  begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.INI'));
  SLeft   := IniFile.ReadInteger  ('SelectFolderWindow', 'Left',
                                   (Screen.Width - Width) div 2);
  STop    := IniFile.ReadInteger  ('SelectFolderWindow', 'Top',
                                   (Screen.Height - Height) div 2);
  SWidth  := IniFile.ReadInteger  ('SelectFolderWindow', 'Width',  Width);
  SHeight := IniFile.ReadInteger  ('SelectFolderWindow', 'Height', Height);
  IniFile.Free;
  SetBounds(SLeft, STop, SWidth, SHeight);
  end;

//-----------------------------------------------------------------------------

procedure TFormSelectFolder.ScanSubfolders(sPath: AnsiString; pNode: TTreeNode);

  var
    SearchRec: TSearchRec;
    bFound   : boolean;
    sFileName: AnsiString;
    cFirstLetter: char;

  begin
  pNode.DeleteChildren();
  if (Copy(sPath, Length(sPath), 1) <> '\')
    then sPath := sPath + '\*'
    else sPath := sPath + '*';
  bFound := SysUtils.FindFirst(sPath, faAnyFile, SearchRec) = 0;

  while (bFound) do
    begin
    sFileName := UpperCase(SearchRec.Name);
    if (sFileName = SearchRec.Name)
      then // all chars in upper case
        begin
        cFirstLetter := sFileName[1];
        sFileName := LowerCase(sFileName);
        sFileName[1] := cFirstLetter;
        end
      else
        sFileName := SearchRec.Name;
    if ((SearchRec.Attr and faDirectory) <> 0) and
        (sFileName <> '.') and
        (sFileName <> '..') and
        ((SearchRec.Attr and faHidden) = 0) then
      DirTreeView.Items.AddChild(pNode, sFileName);
    bFound := SysUtils.FindNext(SearchRec) = 0;
    end;
  SysUtils.FindClose(SearchRec);
  pNode.AlphaSort();
  end;

//---------------------------------------------------------------------------

procedure TFormSelectFolder.InitTreeView();

  var
    dwLogicalDrives         : DWORD;
    uiOldMode               : DWORD;
    i                       : integer;
    szDrive                 : array[0..4] of char;
    szVolumeName            : array[0..250] of char;
    dwMaximumComponentLength: DWORD;
    dwFileSystemFlags       : DWORD;
    szFileSystemNameBuffer  : array [0..100] of char;
    sDrive                  : AnsiString;
    pNode                   : TTreeNode;

  begin
  DirTreeView.Items.Clear(); // remove any existing nodes
  ///dwLogicalDrives := GetLogicalDrives();
  ///uiOldMode := SetErrorMode (SEM_FAILCRITICALERRORS); // hides error message box if the CD is not ready
  for i := 2 to 31 do // 2 means skip A: and B:
    begin
    if (((dwLogicalDrives shr i) and $1) = 0) then Continue;
    StrCopy(szDrive, 'C:\');
    szDrive[0] := char(ord('A') + i);
    {$ifdef mswindows}
    if (not GetVolumeInformation(
            szDrive,                    // root directory
            szVolumeName,               // volume name buffer
            250,                        // length of name buffer
            nil,                       // volume serial number
            dwMaximumComponentLength,   // maximum file name length
            dwFileSystemFlags,          // file system options
            szFileSystemNameBuffer,     // file system name buffer
            100                         // length of file system name buffer
            ))
      then StrCopy(szVolumeName, '');
    {$endif}
    StrCopy(szVolumeName, '');

    szDrive[2] := #0;
    sDrive := szDrive;
    sDrive := sDrive + '  ' + szVolumeName;
    if (DirTreeView.Items.Count = 0) then
      DirTreeView.Items.Add(nil, sDrive)
    else
      DirTreeView.Items.Add(DirTreeView.Items.Item[0], sDrive);
    end;

  pNode := DirTreeView.Items.GetFirstNode();
  while (pNode <> nil) do
    begin
    sDrive := pNode.Text;
    SetLength(sDrive, 2);
    sDrive := sDrive + '\';
    ScanSubfolders(sDrive, pNode);
    pNode := pNode.getNextSibling();
    end;

  ///SetErrorMode (uiOldMode);
  end;

//---------------------------------------------------------------------------

function TFormSelectFolder.GetFullPath(pNode: TTreeNode): AnsiString;

  begin
  if (pNode = nil)
    then
      Result := ''
    else
      if (pNode.Parent = nil)
        then // we are at top, extract the drive letter
          begin
          Result := pNode.Text;
          SetLength(Result, 2);
          exit;
          end
        else
          Result := GetFullPath(pNode.Parent) + '\' + pNode.Text;
  end;

//---------------------------------------------------------------------------

procedure TFormSelectFolder.DirTreeViewExpanded(Sender: TObject; Node: TTreeNode);
  var
    dwStartTime: DWORD;
    sPath      : AnsiString;
    sSubPath   : AnsiString;
    pChildNode : TTreeNode;
    Save_Cursor: TCursor;

  begin
  Save_Cursor := Screen.Cursor;
  dwStartTime := GetTickCount();
  sPath := GetFullPath(Node);
  pChildNode := Node.getFirstChild();
  while (pChildNode <> nil) do
    begin
    sSubPath := sPath + '\' + pChildNode.Text;
    ScanSubfolders(sSubPath, pChildNode);
    pChildNode := Node.GetNextChild(pChildNode);
    if ((GetTickCount() - dwStartTime) > 1000) then
      Screen.Cursor := crHourglass;
    end;

  Directory := GetFullPath(Node);
  Screen.Cursor := Save_Cursor;  { Always restore to normal }
  end;

//---------------------------------------------------------------------------

procedure TFormSelectFolder.DirTreeViewCollapsed(Sender: TObject;
  Node: TTreeNode);

  var
    sPath      : AnsiString;
  begin
  sPath := GetFullPath(Node);
  ScanSubfolders(sPath, Node);
  Directory := GetFullPath(Node);
  end;

//---------------------------------------------------------------------------

procedure TFormSelectFolder.FormShow(Sender: TObject);
  begin
  if (DirTreeView.Items.Count = 0) then InitTreeView();
  ActiveControl := DirTreeView;
  end;

//---------------------------------------------------------------------------

procedure TFormSelectFolder.DirTreeViewClick(Sender: TObject);
  begin
  Directory := GetFullPath(DirTreeView.Selected);
  end;

//-----------------------------------------------------------------------------
// Disables CD AutoRun feature - Windows send the message QueryCancelAutoPlay
// to the top window, so we must have this handler in all forms.

procedure TFormSelectFolder.DefaultHandler(var Message);

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
