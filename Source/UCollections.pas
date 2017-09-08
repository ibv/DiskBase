unit UCollections;
(*====================================================================
Collections - similar to Delphi Lists
======================================================================*)

interface

uses UTypes;

const

{$ifdef DELPHI1}
  MaxQCollectionSize = 65520 div SizeOf(Pointer);
{$else}
  MaxQCollectionSize = 262144; // 1 MB just for pointers...
{$endif}

type
  TPQString = ^ShortString;

  TPQObject = ^TQObject;
  TQObject = object
    constructor Init;
    procedure   Free;
    destructor  Done; virtual;
  end;

  TPItemList = ^TItemList;
  TItemList = array[0..MaxQCollectionSize - 1] of Pointer;

  TPQCollection = ^TQCollection;
  TQCollection = object(TQObject)
    Items: TPItemList;
    Count: Integer;
    Limit: Integer;
    Delta: Integer;
    constructor Init(ALimit, ADelta: Integer);
    destructor  Done; virtual;
    function    At(Index: Integer): Pointer;
    procedure   AtDelete(Index: Integer);
    procedure   AtFree(Index: Integer);
    procedure   AtInsert(Index: Integer; Item: Pointer);
    procedure   AtPut(Index: Integer; Item: Pointer);
    procedure   Delete(Item: Pointer);
    procedure   DeleteAll;
    procedure   Free(Item: Pointer);
    procedure   FreeAll;
    procedure   FreeItem(Item: Pointer); virtual;
    function    IndexOf(Item: Pointer): Integer; virtual;
    procedure   Insert(Item: Pointer); virtual;
    procedure   SetLimit(ALimit: Integer); virtual;
    end;

  TPQSortedCollection = ^TQSortedCollection;
  TQSortedCollection = object(TQCollection)
    Duplicates: Boolean;
    constructor Init(ALimit, ADelta: Integer);
    function    Compare(Key1, Key2: Pointer): Integer; virtual;
    function    IndexOf(Item: Pointer): Integer; virtual;
    procedure   Insert(Item: Pointer); virtual;
    function    KeyOf(Item: Pointer): Pointer; virtual;
    function    Search(Key: Pointer; var Index: Integer): Boolean; virtual;
  end;

  TPQStringCollection = ^TQStringCollection;
  TQStringCollection = object(TQSortedCollection)
    function  Compare (Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
  end;

function  QNewStr(const S: ShortString): TPQString;
procedure QDisposeStr(var P: TPQString);

//=============================================================================

implementation

uses UExceptions, ULang;

procedure Abstract;

  begin
  raise EQDirFatalException.Create(lsAbstractMethodUsed);
  end;

//--- TQObject ----------------------------------------------------------------

constructor TQObject.Init;

  begin
  end;

//-----------------------------------------------------------------------------

procedure TQObject.Free;

  begin
  if @Self <> nil then Dispose(TPQObject(@Self), Done);
  end;

//-----------------------------------------------------------------------------

destructor TQObject.Done;

  begin
  end;

//--- TQCollection ------------------------------------------------------------

constructor TQCollection.Init(ALimit, ADelta: Integer);

  begin
  TQObject.Init;
  Items := nil;
  Count := 0;
  Limit := 0;
  Delta := ADelta;
  SetLimit(ALimit);
  end;

//-----------------------------------------------------------------------------

destructor TQCollection.Done;

  begin
  FreeAll;
  SetLimit(0);
  end;

//-----------------------------------------------------------------------------

function TQCollection.At(Index: Integer): Pointer;

  begin
  if (Index >= 0) and (Index < Count)
    then
      Result := Items^[Index]
    else
      begin
      raise EQDirFatalException.Create(lsInvalidIndex);
      end;
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.AtDelete(Index: Integer);

  begin
  if (Index < 0) and (Index >= Count) then
    raise EQDirFatalException.Create(lsInvalidIndex);
  dec(Count);
  if Count > Index then
    move(Items^[succ(Index)], Items^[Index], (Count - Index)*SizeOf(Pointer));
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.AtFree(Index: Integer);

  var
    Item: Pointer;
  begin
  Item := At(Index);
  AtDelete(Index);
  FreeItem(Item);
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.AtInsert(Index: Integer; Item: Pointer);

  begin
  if Count = Limit then
    begin
    SetLimit(Limit + Delta);
    if Count = Limit then
      raise EQDirFatalException.Create(lsCollectionOverflow);
    end;
  if (Index < 0) and (Index > Count) then
      raise EQDirFatalException.Create(lsInvalidIndex);
  if Count > Index then
    move(Items^[Index], Items^[succ(Index)], (Count - Index)*SizeOf(Pointer));
  Items^[Index] := Item;
  inc(Count);
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.AtPut(Index: Integer; Item: Pointer);

  begin
  if (Index < 0) and (Index >= Count) then
    raise EQDirFatalException.Create(lsInvalidIndex);
  Items^[Index] := Item;
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.Delete(Item: Pointer);

  begin
  AtDelete(IndexOf(Item));
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.DeleteAll;

  begin
  Count := 0;
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.Free(Item: Pointer);

  begin
  Delete(Item);
  FreeItem(Item);
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.FreeAll;

  var
    I: Integer;

  begin
  for I := 0 to Count - 1 do FreeItem(At(I));
  Count := 0;
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.FreeItem(Item: Pointer);

  begin
  if Item <> nil then Dispose(TPQObject(Item), Done);
  end;

//-----------------------------------------------------------------------------

function TQCollection.IndexOf(Item: Pointer): Integer;

  var
    i : Integer;
  begin
  for i := 0 to pred(Count) do
    if Item = Items^[i] then
      begin
      IndexOf := i;
      exit;
      end;
  IndexOf := -1;
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.Insert(Item: Pointer);

  begin
  AtInsert(Count, Item);
  end;

//-----------------------------------------------------------------------------

procedure TQCollection.SetLimit(ALimit: Integer);

  var
    AItems: TPItemList;

  begin
  if ALimit < Count then ALimit := Count;
  if ALimit > MaxQCollectionSize then ALimit := MaxQCollectionSize;
  if ALimit <> Limit then
    begin
    if ALimit = 0 then AItems := nil else
      begin
      GetMem(AItems, ALimit * SizeOf(Pointer));
      if (Count <> 0) and (Items <> nil) then
        Move(Items^, AItems^, Count * SizeOf(Pointer));
      end;
    if Limit <> 0 then FreeMem(Items, Limit * SizeOf(Pointer));
    Items := AItems;
    Limit := ALimit;
    end;
  end;

//--- TQSortedCollection ------------------------------------------------------

constructor TQSortedCollection.Init(ALimit, ADelta: Integer);

  begin
  TQCollection.Init(ALimit, ADelta);
  Duplicates := False;
  end;

//-----------------------------------------------------------------------------

function TQSortedCollection.Compare(Key1, Key2: Pointer): Integer;

  begin
  Result := 0;
  Abstract;
  end;

//-----------------------------------------------------------------------------

function TQSortedCollection.IndexOf(Item: Pointer): Integer;

  var
    I: Integer;

  begin
  IndexOf := -1;
  if Search(KeyOf(Item), I) then
    begin
    if Duplicates then
      while (I < Count) and (Item <> Items^[I]) do Inc(I);
    if I < Count then IndexOf := I;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TQSortedCollection.Insert(Item: Pointer);

  var
    I: Integer;

  begin
  if not Search(KeyOf(Item), I) or Duplicates then AtInsert(I, Item);
  end;

//-----------------------------------------------------------------------------

function TQSortedCollection.KeyOf(Item: Pointer): Pointer;

  begin
  KeyOf := Item;
  end;

//-----------------------------------------------------------------------------

function TQSortedCollection.Search(Key: Pointer; var Index: Integer): Boolean;

  var
    L, H, I, C: Integer;

  begin
  Search := False;
  L := 0;
  H := Count - 1;
  while L <= H do
    begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(Items^[I]), Key);
    if C < 0 then L := I + 1 else
      begin
      H := I - 1;
      if C = 0 then
        begin
        Search := True;
        if not Duplicates then L := I;
        end;
      end;
    end;
  Index := L;
  end;

//--- TQStringCollection ------------------------------------------------------

function TQStringCollection.Compare(Key1, Key2: Pointer): Integer;
  begin
  if TPQString(Key1)^ > TPQString(Key2)^
    then
      Compare := 1
    else
      if TPQString(Key1)^ < TPQString(Key2)^
        then Compare := -1
        else Compare := 0;
  end;

//-----------------------------------------------------------------------------

procedure TQStringCollection.FreeItem(Item: Pointer);

  begin
  QDisposeStr(TPQString(Item));
  end;

{--- Dynamic string handling routines ------------------------------}

function QNewStr(const S: ShortString): TPQString;

  var
    P  : TPQString;
    Len: SmallInt;

  begin
  Len := ((Length(S) + 16) div 16) * 16;
  GetMem(P, Len);
  P^ := S;
  QNewStr := P;
  end;

//-----------------------------------------------------------------------------

procedure QDisposeStr(var P: TPQString);

  var
    Len: SmallInt;

  begin
  if P <> nil then
    begin
    Len := ((Length(P^) + 16) div 16) * 16;
    FreeMem(P, Len);
    P := nil;
    end;
  end;

//-----------------------------------------------------------------------------

end.
