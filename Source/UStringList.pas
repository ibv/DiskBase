unit UStringList;
(*====================================================================
Better implementation of string list than Delphi provides
in contrast to TStringList
  - deallocates also objects, not only strings
  - Capacity can be set
  - beter reallocation of the array of pointers
======================================================================*)


interface

uses UTypes, SysUtils, Classes;

const

{ Maximum TList size }
{$ifdef DELPHI1}
  QMaxListSize = 65520 div SizeOf(Pointer);
{$else}
  QMaxListSize = Maxint div SizeOf(Pointer);
{$endif}

type

{ Standard events }
  EQListError = class(Exception);

{ TList class }
  PQPointerList = ^TQPointerList;
  TQPointerList = array[0..QMaxListSize - 1] of Pointer;

  TQList = class(TObject)
  private
    FList: PQPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Error; virtual;
    function  Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function   Add(Item: Pointer): Integer;
    procedure  Clear;
    procedure  Delete(Index: Integer);
    procedure  Exchange(Index1, Index2: Integer);
    function   Expand: TQList;
    function   First: Pointer;
    function   IndexOf(Item: Pointer): Integer;
    procedure  Insert(Index: Integer; Item: Pointer);
    function   Last: Pointer;
    procedure  Move(CurIndex, NewIndex: Integer);
    function   Remove(Item: Pointer): Integer;
    procedure  Pack;
    property   Capacity: Integer read FCapacity write SetCapacity;
    property   Count: Integer read FCount write SetCount;
    property   Items[Index: Integer]: Pointer read Get write Put; default;
    property   List: PQPointerList read FList;
  end;

{ TStringList class }

  TQDuplicates = (qdupIgnore, qdupAccept, qdupError);

  TQStringList = class(TObject)
  private
    FList: TQList;
    FSorted: Boolean;
    FReversed: Boolean;
    FDuplicates: TQDuplicates;
    procedure QuickSort(L, R: Integer);
    procedure SetSorted(Value: Boolean);
    procedure SetReversed(Value: Boolean);
  protected
    function  Get(Index: Integer): ShortString; virtual;
    function  GetCount: Integer; virtual;
    function  GetObject(Index: Integer): TObject; virtual;
    procedure Put(Index: Integer; const S: ShortString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    function  GetCapacity: Integer;
    procedure SetCapacity(ACapacity: Integer);
  public
    constructor Create;
    destructor  Destroy; override;
    function    Add(const S: ShortString): Integer; virtual;
    function    AddObject(const S: ShortString; AObject: TObject): Integer; virtual;
    procedure   Clear; virtual;
    procedure   Delete(Index: Integer); virtual;
    procedure   Exchange(Index1, Index2: Integer); virtual;
    function    Find(const S: ShortString; var Index: Integer): Boolean; virtual;
    function    IndexOf(const S: ShortString): Integer; virtual;
    function    IndexOfObject(AObject: TObject): Integer;
    procedure   Insert(Index: Integer; const S: ShortString); virtual;
    procedure   InsertObject(Index: Integer; const S: ShortString;
                  AObject: TObject);
    procedure   Sort; virtual;
    property    Duplicates: TQDuplicates read FDuplicates write FDuplicates;
    property    Sorted: Boolean read FSorted write SetSorted;
    property    Reversed: Boolean read FReversed write SetReversed;
    property    Objects[Index: Integer]: TObject read GetObject {write PutObject};
    property    Strings[Index: Integer]: ShortString read Get write Put; default;
    property    Count: Integer read GetCount;
    property    Capacity: Integer read GetCapacity write SetCapacity;
  end;

{ StrItem management, shared by TQStringList and TStringSparseList }

type
  PQStrItem = ^TQStrItem;
  TQStrItem = record
    FObject: TObject;
    FString: ShortString;
  end;

function  NewQStrItem (const AString: ShortString; AObject: TObject): PQStrItem;
function  ChangeStrInQStrItem(OldQStrItem: PQStrItem; const AString: ShortString): PQStrItem;
procedure DisposeQStrItem(P: PQStrItem);

procedure FreeObjects(List:TStringList);

implementation

///uses Consts;


procedure FreeObjects(List:TStringList);
var
  i: integer;
begin
for i := 0 to List.Count - 1 do
    List.Objects[i].Free;

end;

//-----------------------------------------------------------------------------

procedure ListError(Ident: String);

  begin
  raise EQListError.Create(Ident);
  end;

//=== TQList ==================================================================

destructor TQList.Destroy;

  begin
  Clear;
  end;

//-----------------------------------------------------------------------------

function TQList.Add(Item: Pointer): Integer;

  begin
  Result := FCount;
  if Result = FCapacity then Grow;
  FList^[Result] := Item;
  Inc(FCount);
  end;

//-----------------------------------------------------------------------------

procedure TQList.Clear;

  begin
  SetCount(0);
  SetCapacity(0);
  end;

//-----------------------------------------------------------------------------

procedure TQList.Delete(Index: Integer);

  begin
  if (Index < 0) or (Index >= FCount) then Error;
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  end;

//-----------------------------------------------------------------------------

procedure TQList.Error;

  begin
  ///ListError(SListIndexError);
  ListError('SListIndexError');
  end;

//-----------------------------------------------------------------------------

procedure TQList.Exchange(Index1, Index2: Integer);

  var
    Item: Pointer;

  begin
  if (Index1 < 0) or (Index1 >= FCount) or
    (Index2 < 0) or (Index2 >= FCount) then Error;
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
  end;

//-----------------------------------------------------------------------------

function TQList.Expand: TQList;

  begin
  if FCount = FCapacity then Grow;
  Result := Self;
  end;

//-----------------------------------------------------------------------------

function TQList.First: Pointer;

  begin
  Result := Get(0);
  end;

//-----------------------------------------------------------------------------

function TQList.Get(Index: Integer): Pointer;

  begin
  if (Index < 0) or (Index >= FCount) then Error;
  Result := FList^[Index];
   end;

//-----------------------------------------------------------------------------

procedure TQList.Grow;

  var
    Delta: Integer;

  begin
  if FCapacity > 16 then Delta := FCapacity div 3 else  {add 30%}
    if FCapacity > 8 then Delta := 16 else
      if FCapacity > 4 then Delta := 8 else
        Delta := 4;
  if (Delta > (QMaxListSize-FCapacity)) and (FCapacity < QMaxListSize) then
    Delta := QMaxListSize - FCapacity;
  SetCapacity(FCapacity + Delta);
  end;

//-----------------------------------------------------------------------------

function TQList.IndexOf(Item: Pointer): Integer;

  begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do Inc(Result);
  if Result = FCount then Result := -1;
  end;

//-----------------------------------------------------------------------------

procedure TQList.Insert(Index: Integer; Item: Pointer);

  begin
  if (Index < 0) or (Index > FCount) then Error;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
  end;

//-----------------------------------------------------------------------------

function TQList.Last: Pointer;

  begin
  Result := Get(FCount - 1);
  end;

//-----------------------------------------------------------------------------

procedure TQList.Move(CurIndex, NewIndex: Integer);
  var
    Item: Pointer;

  begin
  if CurIndex <> NewIndex then
    begin
    if (NewIndex < 0) or (NewIndex >= FCount) then Error;
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
    end;
  end;

//-----------------------------------------------------------------------------

procedure TQList.Put(Index: Integer; Item: Pointer);

  begin
  if (Index < 0) or (Index >= FCount) then Error;
  FList^[Index] := Item;
  end;

//-----------------------------------------------------------------------------

function TQList.Remove(Item: Pointer): Integer;

  begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
  end;

//-----------------------------------------------------------------------------

procedure TQList.Pack;

  var
    I: Integer;

  begin
  for I := FCount - 1 downto 0 do if Items[I] = nil then Delete(I);
  end;

//-----------------------------------------------------------------------------

procedure TQList.SetCapacity(NewCapacity: Integer);

  var
    NewList: PQPointerList;

  begin
  if (NewCapacity < FCount) or (NewCapacity > QMaxListSize) then Error;
  if NewCapacity <> FCapacity then
    begin
    if NewCapacity = 0 then NewList := nil else
      begin
      GetMem(NewList, NewCapacity * SizeOf(Pointer));
      if FCount <> 0 then
        System.Move(FList^, NewList^, FCount * SizeOf(Pointer));
      end;
    if FCapacity <> 0 then FreeMem(FList, FCapacity * SizeOf(Pointer));
    FList := NewList;
    FCapacity := NewCapacity;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TQList.SetCount(NewCount: Integer);

  begin
  if (NewCount < 0) or (NewCount > QMaxListSize) then Error;
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0);
  FCount := NewCount;
  end;

//=== TQStringList ============================================================

function NewQStrItem(const AString: ShortString; AObject: TObject): PQStrItem;

  begin
  GetMem(Result, Length(AString) + 5);
  Result^.FObject := AObject;
  Result^.FString := AString;
  end;

//-----------------------------------------------------------------------------

function ChangeStrInQStrItem(OldQStrItem: PQStrItem; const AString: ShortString): PQStrItem;

  begin
  GetMem(Result, Length(AString) + 5);
  Result^.FObject := OldQStrItem^.FObject;
  Result^.FString := AString;
  FreeMem(OldQStrItem, Length(OldQStrItem^.FString) + 5);
  end;

//-----------------------------------------------------------------------------

procedure DisposeQStrItem(P: PQStrItem);

  begin
  if P^.FObject <> nil then P^.FObject.Destroy;
  FreeMem(P, Length(P^.FString) + 5);
  end;

//-----------------------------------------------------------------------------

constructor TQStringList.Create;

  begin
  FList := TQList.Create;
  end;

//-----------------------------------------------------------------------------

destructor TQStringList.Destroy;

  begin
  if FList <> nil then
    begin
    Clear;
    FList.Free;
    end;
  end;

//-----------------------------------------------------------------------------

function TQStringList.Add(const S: ShortString): Integer;

  begin
  if not Sorted then
    Result := FList.Count
  else
    if Find(S, Result) then
      case Duplicates of
        qdupIgnore: Exit;
        ///qdupError: ListError(SDuplicateString);
        qdupError: ListError('SDuplicateString');
      end;
  FList.Expand.Insert(Result, NewQStrItem(S, nil));
  end;

//-----------------------------------------------------------------------------

function TQStringList.AddObject(const S: ShortString; AObject: TObject): Integer;

  begin
  Result := Add(S);
  PutObject(Result, AObject);
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.Clear;

  var
    I: Integer;

  begin
  for I := 0 to FList.Count - 1 do DisposeQStrItem(FList[I]);
  FList.Clear;
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.Delete(Index: Integer);

  begin
  DisposeQStrItem(FList[Index]);
  FList.Delete(Index);
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.Exchange(Index1, Index2: Integer);

  begin
  FList.Exchange(Index1, Index2);
  end;

//-----------------------------------------------------------------------------

function TQStringList.Find(const S: ShortString; var Index: Integer): Boolean;

  var
    L, H, I, C: Integer;

  begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
    begin
    I := (L + H) shr 1;
    if FReversed
      then C := AnsiCompareText(S, PQStrItem(FList[I])^.FString)
      else C := AnsiCompareText(PQStrItem(FList[I])^.FString, S);
    if C < 0 then L := I + 1 else
      begin
      H := I - 1;
      if C = 0 then
        begin
        Result := True;
        if Duplicates <> qdupAccept then L := I;
        end;
      end;
    end;
  Index := L;
  end;

//-----------------------------------------------------------------------------

function TQStringList.Get(Index: Integer): ShortString;

  begin
  Result := PQStrItem(FList[Index])^.FString;
  end;

//-----------------------------------------------------------------------------

function TQStringList.GetCount: Integer;

  begin
  Result := FList.Count;
  end;

//-----------------------------------------------------------------------------

function TQStringList.GetObject(Index: Integer): TObject;

  begin
  Result := PQStrItem(FList[Index])^.FObject;
  end;

//-----------------------------------------------------------------------------

function TQStringList.IndexOf(const S: ShortString): Integer;

  begin
  if not Sorted
    then
      begin
      for Result := 0 to GetCount - 1 do
        if CompareText(Get(Result), S) = 0 then Exit;
      Result := -1;
      end
    else
      if not Find(S, Result) then Result := -1;
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.Insert(Index: Integer; const S: ShortString);

  begin
  ///if Sorted then ListError(SSortedListError);
  if Sorted then ListError('SSortedListError');
  FList.Expand.Insert(Index, NewQStrItem(S, nil));
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.InsertObject(Index: Integer; const S: ShortString;
  AObject: TObject);

  begin
  Insert(Index, S);
  PutObject(Index, AObject);
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.Put(Index: Integer; const S: ShortString);

  begin
  ///if Sorted then ListError(SSortedListError);
  if Sorted then ListError('SSortedListError');
  FList[Index] := ChangeStrInQStrItem(FList[Index], S);
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.PutObject(Index: Integer; AObject: TObject);

  begin
  PQStrItem(FList[Index])^.FObject := AObject;
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.QuickSort(L, R: Integer);

  var
    I, J: Integer;
    P: PQStrItem;

  begin
  I := L;
  J := R;
  P := PQStrItem(FList[(L + R) shr 1]);
  repeat
    if FReversed
      then
        begin
        while AnsiCompareText(PQStrItem(FList[I])^.FString, P^.FString) > 0 do Inc(I);
        while AnsiCompareText(PQStrItem(FList[J])^.FString, P^.FString) < 0 do Dec(J);
        end
      else
        begin
        while AnsiCompareText(PQStrItem(FList[I])^.FString, P^.FString) < 0 do Inc(I);
        while AnsiCompareText(PQStrItem(FList[J])^.FString, P^.FString) > 0 do Dec(J);
        end;
    if I <= J then
      begin
      FList.Exchange(I, J);
      Inc(I);
      Dec(J);
      end;
  until I > J;
  if L < J then QuickSort(L, J);
  if I < R then QuickSort(I, R);
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.SetSorted(Value: Boolean);

  begin
  if FSorted <> Value then
    begin
    if Value then Sort;
    FSorted := Value;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.SetReversed(Value: Boolean);

  begin
  if FReversed <> Value then
    begin
    FReversed := Value;
    if Sorted then Sort;
    end;
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.Sort;

  begin
  if not Sorted and (FList.Count > 1) then
    QuickSort(0, FList.Count - 1);
  end;

//-----------------------------------------------------------------------------

function TQStringList.IndexOfObject(AObject: TObject): Integer;

  begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
  end;

//-----------------------------------------------------------------------------

function TQStringList.GetCapacity: Integer;

  begin
  Result := FList.Capacity;
  end;

//-----------------------------------------------------------------------------

procedure TQStringList.SetCapacity(ACapacity: Integer);

  begin
  FList.Capacity := ACapacity;
  end;

//-----------------------------------------------------------------------------

end.
