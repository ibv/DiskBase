unit UCollectionsExt;
(*====================================================================
Extended collections
======================================================================*)

interface

uses UTypes, UCollections;

type
{collection for storing pointers - no deallocation}

  PQPtrCollection = ^TQPtrCollection;
  TQPtrCollection = object(TQCollection)
    procedure FreeItem(Item: Pointer); virtual;
  end;

  PQUnsortStrCol = ^TQUnsortStrCol;
  TQUnsortStrCol = object(TQCollection)
    procedure FreeItem(Item: Pointer); virtual;
  end;


implementation

//-----------------------------------------------------------------------------

procedure TQPtrCollection.FreeItem(Item: Pointer);

  begin
  end;

//-----------------------------------------------------------------------------

procedure TQUnsortStrCol.FreeItem(Item: Pointer);

  begin
  QDisposeStr(TPQString(Item));
  end;

//-----------------------------------------------------------------------------

end.
