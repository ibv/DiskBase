unit UTypes;
(*====================================================================
Common types and utility functions
======================================================================*)

interface

const
  qifFileName = 'DiskBase.Lic';
  helpFileName = 'DiskBase.HLP';

  stVersion = '5.19';
  NumVersion = 5;
  NumSubversion = 19;
  NumVersionType = 1;
  {version 5.00 - 5.07}
  {DataFormatVersion = SmallInt(5) shl 8 + 0;}
  {version 5.07 - ?}
  DataFormatVersion = SmallInt(5) shl 8 + 1;

const
  UsePersistentBlocks : boolean = true;

{$ifdef DELPHI1}
const
{$ifdef English}
  About2 = 'Version '+ stVersion +' English, for Windows 3.1x';
{$endif}
{$ifdef Czech}
  About2 = 'Verze '+ stVersion +' èeská, pro Windows 3.1x';
{$endif}
type

  ShortString = string;
  SmallInt    = integer;

procedure SetLength(var S: ShortString; NewLength: SmallInt);

{$else}

{$ifdef English}
const
  About2 = 'Version '+ stVersion +' English, for Linux';
{$endif}
{$ifdef Czech}
const
  About2 = 'Verze '+ stVersion +' česká, pro Linux';

{$endif}

{$endif}

function  ShortCopy(S: ShortString; Index, Count: SmallInt): ShortString;
procedure ShortDelete(var S: ShortString; Index, Count: SmallInt);
procedure ShortInsert(Source: ShortString; var S: ShortString; Index: SmallInt);

implementation

{$ifdef DELPHI1}

//-----------------------------------------------------------------------------

procedure SetLength(var S: ShortString; NewLength: SmallInt);

  begin
  S[0] := char(NewLength);
  end;

//-----------------------------------------------------------------------------

function  ShortCopy(S: ShortString; Index, Count: SmallInt): ShortString;

  begin
  Result := copy(S, Index, Count);
  end;

//-----------------------------------------------------------------------------

procedure ShortDelete(var S: ShortString; Index, Count: SmallInt);

  begin
  Delete(S, Index, Count);
  end;

//-----------------------------------------------------------------------------

procedure ShortInsert(Source: ShortString; var S: ShortString; Index: SmallInt);

  begin
  Insert(Source, S, Index);
  end;

//-----------------------------------------------------------------------------

{$else}

//-----------------------------------------------------------------------------

function  ShortCopy(S: ShortString; Index, Count: SmallInt): ShortString;

  var
    Len: ShortInt;

  begin
  Len := Length(S);
  Result[0] := #0;
  if (Index > Len) or (Index <= 0) or (Count <= 0) then exit;
  if (Count+Index-1) > Len then Count := Len-Index+1;
  move(S[Index], Result[1], Count);
  Result[0] := char(Count);
  end;

//-----------------------------------------------------------------------------

procedure ShortDelete(var S: ShortString; Index, Count: SmallInt);

  var
    Len: SmallInt;
    ToBeMoved: SmallInt;

  begin
  Len := byte(S[0]);
  if (Index > Len) or (Index <= 0) or (Count <= 0) then exit;
  if (Index + Count) > succ(Len) then
    begin
    S[0] := char(pred(Index));
    exit;
    end;
  S[0] := char(Len-Count);
  ToBeMoved := Len - Index - Count + 1;
  if ToBeMoved > 0 then
    move(S[Index+Count], S[Index], ToBeMoved);
  end;

//-----------------------------------------------------------------------------

procedure ShortInsert(Source: ShortString; var S: ShortString; Index: SmallInt);

  var
    Len: SmallInt;
    SourceLen: SmallInt;
    ToBeMoved: SmallInt;

  begin
  Len := byte(S[0]);
  SourceLen := byte(Source[0]);
  if (Index <= 0) then exit;
  if Index >= succ(Len) then
    begin
    S := S + Source;
    exit;
    end;
  if (Len + SourceLen) > 255 then
    begin
    SourceLen := 255 - Len;
    Source[0] := char(SourceLen);
    end;
  ToBeMoved := Len - Index + 1;
  if ToBeMoved > 0 then
    move(S[Index], S[Index+SourceLen], ToBeMoved);
  move(Source[1], S[Index], SourceLen);
  S[0] := char(Len+SourceLen);
  end;

//-----------------------------------------------------------------------------

{$endif}

begin
end.
