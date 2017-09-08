unit UExport;
(*====================================================================
Implementation of the export to text format
======================================================================*)

interface

uses Classes, UStream, UStringList, UApiTypes, UBaseTypes;

const
  c_DatabaseName            =  1;
  c_DatabasePath            =  2;
  c_NumberOfDisks           =  3;
  c_DiskName                =  4;
  c_DiskSizeKb              =  5;
  c_DiskFreeKb              =  6;
  c_DiskFreePercent         =  7;
  c_Folders                 =  8;
  c_Files                   =  9;
  c_Archives                = 10;
  c_TotalFilesInArchives    = 11;
  c_TotalFoldersInArchives  = 12;
  c_TotalDataSize           = 13;
  c_OriginalPath            = 14;
  c_Volumelabel             = 15;
  c_PhysSize                = 16;
  c_ArchSize                = 17;
  c_ScanDate                = 18;
  c_PathToFolder            = 19;
  c_FolderName              = 20;
  c_FolderDataSize          = 21;
  c_FilesInFolder           = 22;
  c_FoldersInFolder         = 23;
  c_PhysFolderDataSize      = 24;
  c_PhysFilesInFolder       = 25;
  c_PhysFoldersInFolder     = 26;
  c_FileName                = 27;
  c_FileSize                = 28;
  c_FileDate                = 29;
  c_FileTime                = 30;
  c_ShortDesc               = 31;
  c_Desc                    = 32;
  c_CrLf                    = 33;
  c_Tab                     = 34;
  c_Space                   = 35;

  c_FirstId                 = c_DatabaseName;
  c_LastId                  = c_Space;

type

TIdValue = record
  sId   : AnsiString;
  sValue: AnsiString;
  end;

TDBaseExport = object
  private
    sIdentifierBegin         : AnsiString;
    sIdentifierEnd           : AnsiString;
    iMaxDescLines            : integer;
    iMaxDescSize             : integer;
    iFileColumns             : integer;

    IdValues                 : array [c_FirstId..c_LastId] of TIdValue;

    sSecDatabaseBegin     : AnsiString; // level 1
    sSecDiskBegin         : AnsiString; //   level 2
    sSecFolderBegin       : AnsiString; //     level 3
    sSecFirstColumnBegin  : AnsiString; //       level 4
    sSecEachColumnBegin   : AnsiString; //       level 4
    sSecFile              : AnsiString; //         level 5
    sSecDesc              : AnsiString; //         level 5
    sSecEachColumnEnd     : AnsiString; //       level 4
    sSecLastColumnEnd     : AnsiString; //       level 4
    sSecFolderEnd         : AnsiString; //     level 3
    sSecDiskEnd           : AnsiString; //   level 2
    sSecDatabaseEnd       : AnsiString; // level 1

    ExportFile            : TQBufStream;
    ///FilesList             : TQStringList; {for capturing and sorting files}
    FilesList             : TStringList; {for capturing and sorting files}
    DBaseHandle           : PDBaseHandle;

    procedure CheckSection(var sTrimmedLine, sLine: AnsiString);
    procedure DispatchSettings(var sOneLine: AnsiString);
    procedure SetIdentifiers;
    procedure LoCaseSections;
    procedure FillOneFile(var POneFile: TPOneFile);
    procedure WriteFiles;
    procedure WriteSection(var sSection: AnsiString);
    procedure LoCaseOneSection(var sSection: AnsiString);
    procedure ClearValuesLevel1;
    procedure ClearValuesLevel2;
    procedure ClearValuesLevel3;
    procedure ClearValuesLevel4;
  public
    bFormatVerified : boolean;
    sDefaultExt     : AnsiString;
    sFileFilter     : AnsiString;
    sExcludeChars   : AnsiString;
    sReplaceChars   : AnsiString;
    iWritten        : integer;
    iFileCounter    : integer;
    eInSection      : (secSettings,
                       secDatabaseBegin,
                       secDiskBegin,
                       secFolderBegin,
                       secFirstColumnBegin,
                       secEachColumnBegin,
                       secFile,
                       secDesc,
                       secEachColumnEnd,
                       secLastColumnEnd,
                       secFolderEnd,
                       secDiskEnd,
                       secDatabaseEnd);

    procedure Init(FormatFileName: ShortString; aDBaseHandle: PDBaseHandle);
    procedure OpenOutputFile(FileName: ShortString);
    procedure CloseOutputFile;
    function  ConvertChars(S: String): String;
    procedure OnDatabaseBegin;
    procedure OnDatabaseEnd;
    procedure OnDiskBegin;
    procedure OnDiskEnd;
    procedure OnFolderBegin;
    procedure OnFolderEnd;
    procedure OnFirstColumnBegin;
    procedure OnEachColumnBegin;
    procedure OnFile;
    procedure OnEachColumnEnd;
    procedure OnLastColumnEnd;
    procedure WriteOneFile(var POneFile: TPOneFile);
  end;

var
  DBaseExport: TDBaseExport;

implementation

uses SysUtils,
     UTypes, ULang, UCollections, UBaseUtils, LConvEncoding,
     UApi, {FDBase} FMain;

//--------------------------------------------------------------------

procedure ReplaceStr(var St: AnsiString; OrigSt, NewStr: AnsiString);

  var i: integer;

  begin
  i := pos(OrigSt, St);
  while i > 0 do
    begin
    delete(St, i, length(OrigSt));
    insert(NewStr, St, i);
    i := pos(OrigSt, St);
    end;
  end;

//-------------------------------------------------------------------
// determines in which section of the format file we are

procedure TDBaseExport.CheckSection(var sTrimmedLine, sLine: AnsiString);
  begin
  if sTrimmedLine = sIdentifierBegin + 'on database begin' + sIdentifierEnd then
    begin
    eInSection := secDatabaseBegin;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on database end' + sIdentifierEnd then
    begin
    eInSection := secDatabaseEnd;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on disk begin' + sIdentifierEnd then
    begin
    eInSection := secDiskBegin;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on disk end' + sIdentifierEnd then
    begin
    eInSection := secDiskEnd;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on folder begin' + sIdentifierEnd then
    begin
    eInSection := secFolderBegin;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on folder end' + sIdentifierEnd then
    begin
    eInSection := secFolderEnd;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on first column begin' + sIdentifierEnd then
    begin
    eInSection := secFirstColumnBegin;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on each column begin' + sIdentifierEnd then
    begin
    eInSection := secEachColumnBegin;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on file' + sIdentifierEnd then
    begin
    eInSection := secFile;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on description' + sIdentifierEnd then
    begin
    eInSection := secDesc;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on each column end' + sIdentifierEnd then
    begin
    eInSection := secEachColumnEnd;
    sLine := '';
    exit;
    end;
  if sTrimmedLine = sIdentifierBegin + 'on last column end' + sIdentifierEnd then
    begin
    eInSection := secLastColumnEnd;
    sLine := '';
    exit;
    end;
  end;

//-------------------------------------------------------------------
// Gets the settings from the format file

procedure TDBaseExport.DispatchSettings(var sOneLine: AnsiString);

  var
    sLoCaseLine : AnsiString;
    sOneLineCopy: AnsiString;
    i           : integer;

  begin
  sOneLineCopy := sOneLine;
  sLoCaseLine := AnsiLowerCase(sOneLine);

  i := pos('identifierbegin', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine, 1, i-1+length('identifierbegin'));
    sLoCaseLine := TrimLeft(sLoCaseLine);
    delete(sLoCaseLine, 1, 1); // delete the '='
    sIdentifierBegin := Trim(sLoCaseLine);
    end;

  i := pos('identifierend', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine, 1, i-1+length('identifierend'));
    sLoCaseLine := TrimLeft(sLoCaseLine);
    delete(sLoCaseLine, 1, 1); // delete the '='
    sIdentifierEnd := Trim(sLoCaseLine);
    end;

  i := pos('maxdesclines', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine, 1, i-1+length('maxdesclines'));
    sLoCaseLine := TrimLeft(sLoCaseLine);
    delete(sLoCaseLine, 1, 1); // delete the '='
    try
      iMaxDescLines := StrToInt(Trim(sLoCaseLine));
    except
      on EConvertError do iMaxDescLines := 0;
      end;
    end;

  i := pos('maxdescsize', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine, 1, i-1+length('maxdescsize'));
    sLoCaseLine := TrimLeft(sLoCaseLine);
    delete(sLoCaseLine, 1, 1); // delete the '='
    try
      iMaxDescSize := StrToInt(Trim(sLoCaseLine));
    except
      on EConvertError do iMaxDescSize := 0;
      end;
    end;

  i := pos('filecolumns', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine, 1, i-1+length('filecolumns'));
    sLoCaseLine := TrimLeft(sLoCaseLine);
    delete(sLoCaseLine, 1, 1); // delete the '='
    try
      iFileColumns := StrToInt(Trim(sLoCaseLine));
    except
      on EConvertError do iFileColumns := 1;
      end;
    end;

  i := pos('defaultext', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine,  1, i-1+length('defaultext'));
    delete(sOneLineCopy, 1, i-1+length('defaultext'));
    sLoCaseLine  := TrimLeft(sLoCaseLine);
    sOneLineCopy := TrimLeft(sOneLineCopy);
    delete(sLoCaseLine, 1, 1); // delete the '='
    delete(sOneLineCopy, 1, 1);
    sLoCaseLine  := Trim(sLoCaseLine);
    sOneLineCopy := Trim(sOneLineCopy);
    sDefaultExt  := sOneLineCopy;
    end;

  i := pos('filefilter', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine,  1, i-1+length('filefilter'));
    delete(sOneLineCopy, 1, i-1+length('filefilter'));
    sLoCaseLine  := TrimLeft(sLoCaseLine);
    sOneLineCopy := TrimLeft(sOneLineCopy);
    delete(sLoCaseLine, 1, 1); // delete the '='
    delete(sOneLineCopy, 1, 1);
    sLoCaseLine  := Trim(sLoCaseLine);
    sOneLineCopy := Trim(sOneLineCopy);
    sFileFilter  := sOneLineCopy;
    end;

  i := pos('excludechars', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine,  1, i-1+length('excludechars'));
    delete(sOneLineCopy, 1, i-1+length('excludechars'));
    sLoCaseLine  := TrimLeft(sLoCaseLine);
    sOneLineCopy := TrimLeft(sOneLineCopy);
    delete(sLoCaseLine, 1, 1); // delete the '='
    delete(sOneLineCopy, 1, 1);
    sLoCaseLine  := Trim(sLoCaseLine);
    sOneLineCopy := Trim(sOneLineCopy);
    sExcludeChars := sExcludeChars + sOneLineCopy;
    end;

  i := pos('replacechar', sLoCaseLine);
  if i > 0 then
    begin
    delete(sLoCaseLine,  1, i-1+length('replacechar'));
    delete(sOneLineCopy, 1, i-1+length('replacechar'));
    sLoCaseLine  := TrimLeft(sLoCaseLine);
    sOneLineCopy := TrimLeft(sOneLineCopy);
    delete(sLoCaseLine, 1, 1); // delete the '='
    delete(sOneLineCopy, 1, 1);
    sLoCaseLine  := Trim(sLoCaseLine);
    sOneLineCopy := Trim(sOneLineCopy);
    if length(sOneLineCopy) = 1 then sOneLineCopy := sOneLineCopy + ' ';
    sReplaceChars := sReplaceChars + sOneLineCopy;
    end;
  end;

//-------------------------------------------------------------------
// Builds the list of tags used in the format file

procedure TDBaseExport.SetIdentifiers;

  begin
  IdValues[c_DatabaseName          ].sId := sIdentifierBegin + 'databasename' + sIdentifierEnd;
  IdValues[c_DatabasePath          ].sId := sIdentifierBegin + 'databasepath' + sIdentifierEnd;
  IdValues[c_NumberOfDisks         ].sId := sIdentifierBegin + 'numberofdisks' + sIdentifierEnd;
  IdValues[c_DiskName              ].sId := sIdentifierBegin + 'diskname' + sIdentifierEnd;
  IdValues[c_DiskSizeKb            ].sId := sIdentifierBegin + 'disksizekb' + sIdentifierEnd;
  IdValues[c_DiskFreeKb            ].sId := sIdentifierBegin + 'diskfreekb' + sIdentifierEnd;
  IdValues[c_DiskFreePercent       ].sId := sIdentifierBegin + 'diskfreepercent' + sIdentifierEnd;
  IdValues[c_Folders               ].sId := sIdentifierBegin + 'folders' + sIdentifierEnd;
  IdValues[c_Files                 ].sId := sIdentifierBegin + 'files' + sIdentifierEnd;
  IdValues[c_Archives              ].sId := sIdentifierBegin + 'archives' + sIdentifierEnd;
  IdValues[c_TotalFilesInArchives  ].sId := sIdentifierBegin + 'totalfilesinarchives' + sIdentifierEnd;
  IdValues[c_TotalFoldersInArchives].sId := sIdentifierBegin + 'totalfoldersinarchives' + sIdentifierEnd;
  IdValues[c_TotalDataSize         ].sId := sIdentifierBegin + 'totaldatasize' + sIdentifierEnd;
  IdValues[c_OriginalPath          ].sId := sIdentifierBegin + 'originalpath' + sIdentifierEnd;
  IdValues[c_VolumeLabel           ].sId := sIdentifierBegin + 'volumelabel' + sIdentifierEnd;
  IdValues[c_PhysSize              ].sId := sIdentifierBegin + 'physsize' + sIdentifierEnd;
  IdValues[c_ArchSize              ].sId := sIdentifierBegin + 'archsize' + sIdentifierEnd;
  IdValues[c_ScanDate              ].sId := sIdentifierBegin + 'scandate' + sIdentifierEnd;
  IdValues[c_PathToFolder          ].sId := sIdentifierBegin + 'pathtofolder' + sIdentifierEnd;
  IdValues[c_FolderName            ].sId := sIdentifierBegin + 'foldername' + sIdentifierEnd;
  IdValues[c_FolderDataSize        ].sId := sIdentifierBegin + 'folderdatasize' + sIdentifierEnd;
  IdValues[c_FilesInFolder         ].sId := sIdentifierBegin + 'filesinfolder' + sIdentifierEnd;
  IdValues[c_FoldersInFolder       ].sId := sIdentifierBegin + 'foldersinfolder' + sIdentifierEnd;
  IdValues[c_PhysFolderDataSize    ].sId := sIdentifierBegin + 'physfolderdatasize' + sIdentifierEnd;
  IdValues[c_PhysFilesInFolder     ].sId := sIdentifierBegin + 'physfilesinfolder' + sIdentifierEnd;
  IdValues[c_PhysFoldersInFolder   ].sId := sIdentifierBegin + 'physfoldersinfolder' + sIdentifierEnd;
  IdValues[c_FileName              ].sId := sIdentifierBegin + 'filename' + sIdentifierEnd;
  IdValues[c_FileSize              ].sId := sIdentifierBegin + 'filesize' + sIdentifierEnd;
  IdValues[c_FileDate              ].sId := sIdentifierBegin + 'filedate' + sIdentifierEnd;
  IdValues[c_FileTime              ].sId := sIdentifierBegin + 'filetime' + sIdentifierEnd;
  IdValues[c_ShortDesc             ].sId := sIdentifierBegin + 'shortdesc' + sIdentifierEnd;
  IdValues[c_Desc                  ].sId := sIdentifierBegin + 'desc' + sIdentifierEnd;

  IdValues[c_CrLf                  ].sId := sIdentifierBegin + 'crlf' + sIdentifierEnd;
  IdValues[c_Tab                   ].sId := sIdentifierBegin + 'tab' + sIdentifierEnd;
  IdValues[c_Space                 ].sId := sIdentifierBegin + 'space' + sIdentifierEnd;
  end;

//-------------------------------------------------------------------
// Converts all the sections to lower case

procedure TDBaseExport.LoCaseSections;

  begin
  LoCaseOneSection(sSecDatabaseBegin);
  LoCaseOneSection(sSecDiskBegin);
  LoCaseOneSection(sSecFolderBegin);
  LoCaseOneSection(sSecFirstColumnBegin);
  LoCaseOneSection(sSecEachColumnBegin);
  LoCaseOneSection(sSecFile);
  LoCaseOneSection(sSecDesc);
  LoCaseOneSection(sSecEachColumnEnd);
  LoCaseOneSection(sSecLastColumnEnd);
  LoCaseOneSection(sSecFolderEnd);
  LoCaseOneSection(sSecDiskEnd);
  LoCaseOneSection(sSecDatabaseEnd);
  end;

//-------------------------------------------------------------------
// Converst one section to lower case

procedure TDBaseExport.LoCaseOneSection(var sSection: AnsiString);

  var
    i       : integer;
    iIdPos  : integer;
    iIndex  : integer;
    sLoCaseSection: AnsiString;

  begin
  sLoCaseSection := AnsiLowerCase(sSection);
  for iIndex := c_FirstId to c_LastId do
    begin
    iIdPos := Pos(IdValues[iIndex].sId, sLoCaseSection);
    while (iIdPos > 0) do
      begin
      for i := iIdPos to iIdPos + length(IdValues[iIndex].sId) - 1 do
        begin
        sSection[i] := sLoCaseSection[i];
        sLoCaseSection[i] := #1; // erase so it is not find in next iteration
        end;
      iIdPos := Pos(IdValues[iIndex].sId, sLoCaseSection);
      end;
    end;
  end;

//-------------------------------------------------------------------
// Replaces all tags in one section and writes it to the output file

procedure TDBaseExport.WriteSection(var sSection: AnsiString);

  var
    iIdPos  : integer;
    iIndex  : integer;
    sSectionCopy: AnsiString;
  begin
  if sSection = '' then exit;
  sSectionCopy := sSection;
  for iIndex := c_FirstId to c_LastId do
    begin
    iIdPos := Pos(IdValues[iIndex].sId, sSectionCopy);
    while (iIdPos > 0) do
      begin
      delete(sSectionCopy, iIdPos, length(IdValues[iIndex].sId));
      insert(IdValues[iIndex].sValue, sSectionCopy, iIdPos);
      iIdPos := Pos(IdValues[iIndex].sId, sSectionCopy);
      end;
    end;
  ExportFile.Write(sSectionCopy[1], length(sSectionCopy));
  inc(iWritten, length(sSectionCopy));
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.ClearValuesLevel1;

  begin
  IdValues[c_DatabaseName          ].sValue := '';
  IdValues[c_DatabasePath          ].sValue := '';
  IdValues[c_NumberOfDisks         ].sValue := '';
  IdValues[c_CrLf                  ].sValue := #13#10;
  IdValues[c_Tab                   ].sValue := #9;
  IdValues[c_Space                 ].sValue := ' ';
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.ClearValuesLevel2;

  begin
  IdValues[c_DiskName              ].sValue := '';
  IdValues[c_DiskSizeKb            ].sValue := '';
  IdValues[c_DiskFreeKb            ].sValue := '';
  IdValues[c_DiskFreePercent       ].sValue := '';
  IdValues[c_Folders               ].sValue := '';
  IdValues[c_Files                 ].sValue := '';
  IdValues[c_Archives              ].sValue := '';
  IdValues[c_TotalFilesInArchives  ].sValue := '';
  IdValues[c_TotalFoldersInArchives].sValue := '';
  IdValues[c_TotalDataSize         ].sValue := '';
  IdValues[c_OriginalPath          ].sValue := '';
  IdValues[c_VolumeLabel           ].sValue := '';
  IdValues[c_PhysSize              ].sValue := '';
  IdValues[c_ArchSize              ].sValue := '';
  IdValues[c_ScanDate              ].sValue := '';
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.ClearValuesLevel3;

  begin
  IdValues[c_PathToFolder          ].sValue := '';
  IdValues[c_FolderName            ].sValue := '';
  IdValues[c_FolderDataSize        ].sValue := '';
  IdValues[c_FilesInFolder         ].sValue := '';
  IdValues[c_FoldersInFolder       ].sValue := '';
  IdValues[c_PhysFolderDataSize    ].sValue := '';
  IdValues[c_PhysFilesInFolder     ].sValue := '';
  IdValues[c_PhysFoldersInFolder   ].sValue := '';
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.ClearValuesLevel4;

  begin
  IdValues[c_FileName              ].sValue := '';
  IdValues[c_FileSize              ].sValue := '';
  IdValues[c_FileDate              ].sValue := '';
  IdValues[c_FileTime              ].sValue := '';
  IdValues[c_ShortDesc             ].sValue := '';
  IdValues[c_Desc                  ].sValue := '';
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.Init(FormatFileName: ShortString; aDBaseHandle: PDBaseHandle);

  var
    F               : TQBufStream;
    pBuffer         : PChar;
    sBuffer         : AnsiString;
    sOneLine        : AnsiString;
    sTrimmedLine    : AnsiString;
    BufSize         : Integer;
    TmpBuf          : array[0..255] of char;
    i               : integer;

  begin
  iMaxDescLines     := 0;
  iMaxDescSize      := 0;
  iFileColumns      := 1;
  bFormatVerified   := false;
  sIdentifierBegin  := '<<';
  sIdentifierEnd    := '>>';
  eInSection        := secSettings;
  sDefaultExt       := 'txt';
  sFileFilter       := '';
  sExcludeChars     := '';
  sReplaceChars     := '';
  iWritten          := 0;
  iFileCounter      := 0;
  DBaseHandle       := aDBaseHandle;

  ClearValuesLevel1;
  ClearValuesLevel2;
  ClearValuesLevel3;
  ClearValuesLevel4;

  sSecDatabaseBegin    := '';
  sSecDiskBegin        := '';
  sSecFolderBegin      := '';
  sSecFirstColumnBegin := '';
  sSecEachColumnBegin  := '';
  sSecFile             := '';
  sSecDesc             := '';
  sSecEachColumnEnd    := '';
  sSecLastColumnEnd    := '';
  sSecFolderEnd        := '';
  sSecDiskEnd          := '';
  sSecDatabaseEnd      := '';

  ///FilesList            := TQStringList.Create;
  FilesList            := TStringList.Create;
  ///FilesList.Sorted     := true;
  ///FilesList.Duplicates := qdupAccept;

  F.Init(StrPCopy(TmpBuf, FormatFileName), stOpenReadNonExclusive);
  BufSize := F.GetSize;
  GetMem(pBuffer, BufSize+2);
  F.Read(pBuffer^, BufSize);
  F.Done;
  PBuffer[BufSize] := #0;
  sBuffer := AdjustLineBreaks(StrPas(pBuffer));
  sBuffer := StrPas(pBuffer);
  FreeMem(PBuffer, BufSize+2);

  while Length(sBuffer) > 0 do
    begin
    ///i := Pos(#$D#$A, sBuffer);
    i := Pos(#$A, sBuffer);
    if i > 0
      then
        begin
        sOneLine := copy(sBuffer, 1, i-1);
        ReplaceStr(sOneLine,#$D,'');
        //ReplaceStr(sOneLine,#10,'');
        ///delete(sBuffer, 1, i+1);
        delete(sBuffer, 1, i);
        end
      else
        begin
        sOneLine := sBuffer;
        sBuffer := '';
        end;

    // now we have one line here, check, if it begins with //
    sOneLine := TrimRight(sOneLine);
    sTrimmedLine := AnsiLowerCase(TrimLeft(sOneLine));
    if pos('//', sTrimmedLine) <> 1 then // whole line is not comment
      begin
      i := pos('//', sOneLine);
      if i>0 then SetLength(sOneLine, i-1);
      CheckSection(sTrimmedLine, sOneLine);
      if eInSection = secSettings
        then
          begin
          if pos('diskbase export format', sTrimmedLine) > 0 then bFormatVerified := true;
          DispatchSettings(sOneLine);
          end
        else // we are in section
          begin
          case eInSection of
            secDatabaseBegin:   sSecDatabaseBegin    := sSecDatabaseBegin    + sOneLine;
            secDatabaseEnd:     sSecDatabaseEnd      := sSecDatabaseEnd      + sOneLine;
            secDiskBegin:       sSecDiskBegin        := sSecDiskBegin        + sOneLine;
            secDiskEnd:         sSecDiskEnd          := sSecDiskEnd          + sOneLine;
            secFolderBegin:     sSecFolderBegin      := sSecFolderBegin      + sOneLine;
            secFolderEnd:       sSecFolderEnd        := sSecFolderEnd        + sOneLine;
            secFirstColumnBegin:sSecFirstColumnBegin := sSecFirstColumnBegin + sOneLine;
            secEachColumnBegin: sSecEachColumnBegin  := sSecEachColumnBegin  + sOneLine;
            secFile:            sSecFile             := sSecFile             + sOneLine;
            secDesc:            sSecDesc             := sSecDesc             + sOneLine;
            secEachColumnEnd:   sSecEachColumnEnd    := sSecEachColumnEnd    + sOneLine;
            secLastColumnEnd:   sSecLastColumnEnd    := sSecLastColumnEnd    + sOneLine;
            end;
          end;
      end; // if
    end; // while
  SetIdentifiers;
  LoCaseSections;
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OpenOutputFile(FileName: ShortString);

  var
    TmpStrArr : array[0..256] of char;

  begin
  ExportFile.Init(StrPCopy(TmpStrArr, FileName), stCreate);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.CloseOutputFile;

  begin
  ExportFile.Done;
  FreeListObjects(FilesList);
  FilesList.Free;
  end;

//-------------------------------------------------------------------
// Delets chars specified in ExcludeChars and replaces chars specified in
// ReplaceChars

function TDBaseExport.ConvertChars(S: String): String;

  var
    i, j: integer;

  begin
  if (sExcludeChars = '') and (sReplaceChars = '') then
    begin
    Result := S;
    // convert to unix utf8
    //Result := CP1250ToUtf8(s);
    //
    exit;
    end;
  for i := 1 to length(sExcludeChars) do
    for j := 1 to length(S) do
      if S[j] = sExcludeChars[i] then S[j] := ' ';

  for i := 1 to length(sReplaceChars) div 2 do
    for j := 1 to length(S) do
      if S[j] = sReplaceChars[2*(i-1)+1] then S[j] := sReplaceChars[2*(i-1)+2];
  Result := S;
  end;

//-------------------------------------------------------------------
// Follwoing prodeures are called from IterateFiles to produce the output
//-------------------------------------------------------------------

procedure TDBaseExport.OnDatabaseBegin;

  begin
  ClearValuesLevel1;
  ClearValuesLevel2;
  ClearValuesLevel3;
  ClearValuesLevel4;
  IdValues[c_DatabaseName ].sValue := ConvertChars(gDatabaseData.sDatabaseName);
  IdValues[c_DatabasePath ].sValue := ConvertChars(gDatabaseData.sDatabasePath);
  IdValues[c_NumberOfDisks].sValue := ConvertChars(IntToStr(gDatabaseData.iNumberOfDisks));
  WriteSection(sSecDatabaseBegin);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnDatabaseEnd;

  begin
  WriteSection(sSecDatabaseEnd);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnDiskBegin;

  var
    TmpComp: comp;

  begin
  ClearValuesLevel2;
  ClearValuesLevel3;
  ClearValuesLevel4;
  IdValues[c_DiskName              ].sValue := CP1250ToUtf8(ConvertChars(gDiskData.sDiskName));
  IdValues[c_Folders               ].sValue := CP1250ToUtf8(ConvertChars(FormatNumber(gDiskData.iFolders)));
  IdValues[c_Files                 ].sValue := CP1250ToUtf8(ConvertChars(FormatNumber(gDiskData.iFiles)));
  IdValues[c_Archives              ].sValue := ConvertChars(FormatNumber(gDiskData.iArchives));
  IdValues[c_TotalFilesInArchives  ].sValue := ConvertChars(FormatNumber(gDiskData.iTotalFilesInArchives));
  IdValues[c_TotalFoldersInArchives].sValue := ConvertChars(FormatNumber(gDiskData.iTotalFoldersInArchives));
  IdValues[c_OriginalPath          ].sValue := ConvertChars(gDiskData.sOriginalPath);
  IdValues[c_VolumeLabel           ].sValue := ConvertChars(gDiskData.sVolumelabel);
  TmpComp := gDiskData.iDiskSizeKb;
  IdValues[c_DiskSizeKb].sValue := ConvertChars(FormatBigSize(TmpComp * 1024));
  TmpComp := gDiskData.iDiskFreeKb;
  IdValues[c_DiskFreeKb].sValue := ConvertChars(FormatBigSize(TmpComp * 1024));
  if (gDiskData.iDiskSizeKb = 0)
    then IdValues[c_DiskFreePercent].sValue := '0%'
    else IdValues[c_DiskFreePercent].sValue := IntToStr((gDiskData.iDiskFreeKb*100) div gDiskData.iDiskSizeKb) + '%';
  IdValues[c_TotalDataSize         ].sValue := ConvertChars(FormatBigSize(gDiskData.TotalDataSize));
  IdValues[c_PhysSize              ].sValue := ConvertChars(FormatBigSize(gDiskData.PhysSize));
  IdValues[c_ArchSize              ].sValue := ConvertChars(FormatBigSize(gDiskData.ArchSize));
  IdValues[c_ScanDate              ].sValue := ConvertChars(DosDateToStr(gDiskData.iScanDate) + ' ' + DosTimeToStr(gDiskData.iScanDate, false));
  WriteSection(sSecDiskBegin);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnDiskEnd;

  begin
  WriteSection(sSecDiskEnd);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnFolderBegin;

  begin
  ClearValuesLevel3;
  ClearValuesLevel4;
  IdValues[c_PathToFolder          ].sValue := ConvertChars(gFolderData.sPathToFolder);
  IdValues[c_FolderName            ].sValue := ConvertChars(gFolderData.sFolderName);
  IdValues[c_FolderDataSize        ].sValue := ConvertChars(FormatBigSize(gFolderData.FolderDataSize));
  IdValues[c_FilesInFolder         ].sValue := ConvertChars(FormatNumber(gFolderData.iFilesInFolder));
  IdValues[c_FoldersInFolder       ].sValue := ConvertChars(FormatNumber(gFolderData.iFoldersInFolder));
  IdValues[c_PhysFolderDataSize    ].sValue := ConvertChars(FormatBigSize(gFolderData.PhysFolderDataSize));
  IdValues[c_PhysFilesInFolder     ].sValue := ConvertChars(FormatNumber(gFolderData.iPhysFilesInFolder));
  IdValues[c_PhysFoldersInFolder   ].sValue := ConvertChars(FormatNumber(gFolderData.iPhysFoldersInFolder));
  WriteSection(sSecFolderBegin);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnFolderEnd;

  begin
  WriteFiles;
  WriteSection(sSecFolderEnd);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnFirstColumnBegin;

  begin
  WriteSection(sSecFirstColumnBegin);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnEachColumnBegin;

  begin
  WriteSection(sSecEachColumnBegin);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnFile;

  var
    OneFileLine: TOneFileLine; // pointer

  begin
  OneFileLine := TOneFileLine.Create(gFileData.OneFile);
  FilesList.AddObject(GetSortString(gFileData.OneFile, OneFileLine.FileType,
                                    gFormatSettings.SortCrit,
                                    gFormatSettings.bReversedSort),
                      OneFileLine);
  end;

//-------------------------------------------------------------------
// Used for immediate export of the file without putting it to
// the FileList

procedure TDBaseExport.WriteOneFile(var POneFile: TPOneFile);
  begin
  ClearValuesLevel3;
  ClearValuesLevel4;
  // disk name and folder name are changing in File Found List
  IdValues[c_DiskName  ].sValue := ConvertChars(gDiskData.sDiskName);
  IdValues[c_FolderName].sValue := ConvertChars(gFolderData.sFolderName);
  FillOneFile(POneFile);
  WriteSection(sSecFile);
  if POneFile^.Description <> 0 then WriteSection(sSecDesc);
  inc(iFileCounter);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.FillOneFile(var POneFile: TPOneFile);

  var
    sShortDesc    : ShortString;
    pBuffer       : PChar;
    sTmp          : AnsiString;
    i             : integer;
    iCrLfCounter  : integer;

  begin
  IdValues[c_FileName ].sValue := ConvertChars(POneFile^.LongName +
                                               POneFile^.Ext);
  if (POneFile^.LongName = '..')
    then
      IdValues[c_FileSize].sValue := ''
    else
      if POneFile^.Attr and faDirectory = faDirectory
        then IdValues[c_FileSize].sValue := ConvertChars(lsFolder)
        else IdValues[c_FileSize].sValue := ConvertChars(FormatSize(POneFile^.Size,
                                                         gFormatSettings.bShowInKb));

  if POneFile^.Time <> 0
    then
      begin
      IdValues[c_FileTime ].sValue := ConvertChars(DosTimeToStr(POneFile^.Time,
                                                   gFormatSettings.bShowSeconds));
      IdValues[c_FileDate ].sValue := ConvertChars(DosDateToStr(POneFile^.Time));
      end
    else
      begin
      IdValues[c_FileTime ].sValue := '';
      IdValues[c_FileDate ].sValue := '';
      end;

  IdValues[c_ShortDesc].sValue := '';
  IdValues[c_Desc     ].sValue := '';
  if POneFile^.Description <> 0 then
    begin
    sShortDesc[0] := #0;
    QI_GetShortDesc(DBaseHandle, POneFile^.Description, sShortDesc);
    IdValues[c_ShortDesc].sValue := ConvertChars(sShortDesc);
    QI_LoadDescToBuf(DBaseHandle, POneFile^.Description, pBuffer);
    sTmp := AdjustLineBreaks(StrPas(pBuffer));
    StrDispose(pBuffer);
    if (iMaxDescLines > 0) then
      begin
      i := 1;
      iCrLfCounter := 0;
      while i <= length(sTmp) do
        begin
        if (sTmp[i] = #13) then inc(iCrLfCounter);
        if iCrLfCounter >= iMaxDescLines then break;
        inc(i);
        end;
      if (iCrLfCounter >= iMaxDescLines) and (i > 1) and (i < length(sTmp)) then
        SetLength(sTmp, i-1);
      end;
    if (iMaxDescSize > 0) and (iMaxDescSize < length(sTmp)) then
      SetLength(sTmp, iMaxDescSize);
    IdValues[c_Desc].sValue := ConvertChars(sTmp);
    end;
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.WriteFiles;

  var
    OneFileLine   : TOneFileLine;
    Index         : Integer;
    iFilesInColumn: integer;

  begin
  iFilesInColumn := (FilesList.Count + iFileColumns - 1) div iFileColumns;
  for Index := 0 to pred(FilesList.Count) do
    begin
    ClearValuesLevel4;
    OneFileLine := TOneFileLine(FilesList.Objects[Index]);
    FillOneFile(OneFileLine.POneFile);

    if (Index = 0) then OnFirstColumnBegin;
    if (Index = 0) or ((Index mod iFilesInColumn) = 0) then OnEachColumnBegin;

    WriteSection(sSecFile);
    if OneFileLine.POneFile^.Description <> 0 then WriteSection(sSecDesc);

    if (Index = pred(FilesList.Count)) or (((Index+1) mod iFilesInColumn) = 0)
      then OnEachColumnEnd;
    if (Index = pred(FilesList.Count)) then OnLastColumnEnd;

    inc(iFileCounter);
    end;
  FreeListObjects(FilesList);
  FilesList.Clear;
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnEachColumnEnd;

  begin
  WriteSection(sSecEachColumnEnd);
  end;

//-------------------------------------------------------------------

procedure TDBaseExport.OnLastColumnEnd;

  begin
  WriteSection(sSecLastColumnEnd);
  end;

//-------------------------------------------------------------------


end.
