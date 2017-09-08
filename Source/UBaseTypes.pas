unit UBaseTypes;
(*====================================================================
Private types - internal, not published from the API
======================================================================*)

{$A-}

interface

uses
  {$ifdef mswindows}
  Windows
  {$ELSE}
    LCLIntf, LCLType, LMessages,
  {$ENDIF}
  UTypes, UCollections, UApiTypes;


const
  dfMaxDescSize      = 1024;

  erMemoryCritical    = 10000;
  erMemoryNonCritical = 10001;

{record type}
  rtTree      =  1;
  rtFileList  =  2;
  rtFileDesc  = 12;

type

//--- structure of the databaze --------------------------------------

//  1. Header   - of THeader type
//  2. KeyField - of TKeyField type

  TSecretData = record
    Dummy1  : longint;
    Dummy2  : longint;
    Dummy3  : longint;
    Dummy4  : longint; {1 = Read Only}
    Dummy5  : longint;
    Dummy6  : ShortString;
    end;

  PHeader = ^THeader;
  THeader = record
    QDirLabel     : array[1..18] of char;
    Version       : longint; {version in hi byte, subversion in lo}
    VersionNeeded : longint;
    Lock          : longint; {not used}
    SecretData1   : TSecretData;
    Reserved      : array[1..2044] of byte; {2044 is there intentionally}
    SecretData2   : TSecretData;
    end;

const
  maxKeySize = 10000;
  keyHeadSize = 6*4;  {6 longint variables before the field itself}

type

  PKeyField = ^TKeyField;
  TKeyField = record // warning - when changing also change keyHeadSize!
    KeySize      : longint;
    Focused      : longint; // position of selected
    Count        : longint; // includes deleted
    CountValid   : longint; // count without deleted
    CountDeleted : longint; // count of deleted - zeroed after reindex.
    Reserved2    : longint;
    Keys         : array [0..maxKeySize-1] of TKeyRecord;
    end;

type
  TDescHeader = record
    Attr  : word; // not used
    Time  : longint;
    end;

const
  MaxShortDescSize = 200;
  MaxBufDescColItems = 200; // max. items in BufDescCol

type
  TShortDescString = string[MaxShortDescSize];

  // buffer for storing descriptions
  TBufDescItem = record
    ID       : TFilePointer;
    ShortDesc: TShortDescString;
    end;
  PBufDescItem = ^TBufDescItem;

  TTotalSize = record
    TS, NTS: longint;
    end;

  TString80   = string[80];
  TString40   = string[40];
  TString20   = string[20];

type
  TDescLine   = string[80];

TQDir4SearchRec = record
  FindIterator: SmallInt;
  Time: longint;
  Size: longint;
  Attr: Word;
  Name: ShortString;
  end;

//====================================================================

implementation


end.
