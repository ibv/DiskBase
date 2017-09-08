unit UEngineFileFind;
(*====================================================================
Class for searched items.
======================================================================*)

{$A-}

interface

uses UTypes, UCollections, UApiTypes, UBaseTypes;


type
  PFSearchItem = ^TFSearchItem;
  TFSearchItem = object(TQObject)
    Disk        : TPQString;
    Dir         : TPQString;
    LongName    : TPQString;
    ShortDesc   : TPQString; // copy of the part of description
    Ext         : TExt;
    FileType    : TFileType;
    Attr        : word;
    Time        : Longint;
    Size        : Longint;
    Description : TFilePointer;
    SelfFilePos : TFilePointer; // copy from OneFile
    Position    : Integer; // position of found string
    constructor FileInit (OneFile: TOneFile; ADisk: ShortString;
                          ADir: ShortString; AShortDesc: ShortString);
    constructor EmptiesInit (ADisk: ShortString; QDiskFree, QDiskSize: longInt;
                             AShortDesc: ShortString);
    destructor  Done; virtual;
    end;

implementation

uses SysUtils, UBaseUtils, UCallbacks;

//=== TFSearchItem ===========================================================

constructor TFSearchItem.FileInit (OneFile: TOneFile;
                                   ADisk, ADir, AShortDesc: ShortString);

  begin
  inherited Init;
  LongName := QNewStr(OneFile.LongName);
  Size := OneFile.Size;
  Time := OneFile.Time;
  Attr := OneFile.Attr;
  Ext  := OneFile.Ext;
  Description := OneFile.Description;
  SelfFilePos := OneFile.SelfFilePos;
  if GetPQString(LongName) = '..'
    then FileType := ftParent
    else
      if Attr and faDirectory = faDirectory
        then FileType := ftDir
        else FileType := ftFile;
  Disk      := QNewStr(ADisk);
  Dir       := QNewStr(ADir);
  ShortDesc := QNewStr(AShortDesc);
  end;

//-----------------------------------------------------------------------------

constructor TFSearchItem.EmptiesInit (ADisk: ShortString; QDiskFree,
                                      QDiskSize: longInt; AShortDesc: ShortString);

  begin
  inherited Init;
  FileType  := TFileType(0);
  Attr      := 0;
  LongName  := nil;
  ShortDesc := QNewStr(AShortDesc);
  Dir       := nil;
  Ext       := '';
  Disk      := QNewStr(ADisk);
  Size      := QDiskSize;
  Time      := QDiskFree;
  end;

//-----------------------------------------------------------------------------

destructor TFSearchItem.Done;

  begin
  QDisposeStr(Dir);
  QDisposeStr(LongName);
  QDisposeStr(Disk);
  QDisposeStr(ShortDesc);
  end;

//-----------------------------------------------------------------------------

end.
