unit UCallbacks;
(*====================================================================
Defines dummy callbacks for the Engine to the user interface. Instead of
setting the calbacks to nil, we use dummy calbacks, so that the usage is
safer.
======================================================================*)

{$A-}

interface

uses UTypes, UApiTypes, UBaseTypes;

const
  cmYes            = 12; 
  cmNo             = 13;
  cmYesAll         = 150;
  cmNoAll          = 151;
  

var
  CB_IsTheLastCPB            : TIsTheLastCPB;
  CB_HowToContinue           : THowToContinue;
  CB_NewLineToIndicator      : TNewLineToIndicator;
  CB_AppendLineToIndicator   : TAppendLineToIndicator;
  CB_UpdateProgressIndicator : TUpdateProgressIndicator;
  CB_AbortScan               : TAbortScan;

procedure ClearCallBacks;

function  XIsTheLastCPB(FName: ShortString): boolean;
function  XHowToContinue (FName: ShortString): word;
procedure XNewLineToIndicator (Line: ShortString);
procedure XAppendLineToIndicator (Line: ShortString);
procedure XUpdateProgressIndicator (Phase, Progress: Integer);
function  XAbortScan: boolean;
procedure XErrorMsg (ErrorNo: Integer; ErrorStr: ShortString);


//-----------------------------------------------------------------------------

implementation

function XIsTheLastCPB (FName: ShortString): boolean;
  begin
  XIsTheLastCPB := true;
  end;

function XHowToContinue (FName: ShortString): word;
  begin
  XHowToContinue := cmYesAll;
  end;

procedure XNewLineToIndicator (Line: ShortString);
  begin
  end;

procedure XAppendLineToIndicator (Line: ShortString);
  begin
  end;

procedure XUpdateProgressIndicator (Phase, Progress: Integer);
  begin
  end;

function  XAbortScan: boolean;
  begin
  XAbortScan := false;
  end;

procedure XErrorMsg (ErrorNo: Integer; ErrorStr: ShortString);
  begin
  end;

//-----------------------------------------------------------------------------

procedure ClearCallBacks;

  begin
  CB_IsTheLastCPB            := XIsTheLastCPB;
  CB_HowToContinue           := XHowToContinue;
  CB_NewLineToIndicator      := XNewLineToIndicator;
  CB_AppendLineToIndicator   := XAppendLineToIndicator;
  CB_UpdateProgressIndicator := XUpdateProgressIndicator;
  CB_AbortScan               := XAbortScan;
  end;

//-----------------------------------------------------------------------------

begin
ClearCallBacks;
end.
