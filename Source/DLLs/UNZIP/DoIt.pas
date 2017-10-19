unit DoIt;

interface


function ExtractFile (szArchivepath,szFileName, szTmpFileName : PChar;SizeToExtract,  OffsetInArchive : longint) : longint; export;


{====================================================================}

implementation

uses
  Classes, Zipper;

function ExtractFile (szArchivepath,szFileName, szTmpFileName : PChar;SizeToExtract,  OffsetInArchive : longint) : longint;
var
  UnZipper: TUnZipper;
  Ts: TStringList;
begin
  result:=-1;
  UnZipper := TUnZipper.Create;
  Ts := TStringList.Create;
  try
    UnZipper.FileName := szArchivepath;
    UnZipper.OutputPath := szTmpFileName;
    UnZipper.Examine;
    UnZipper.BufferSize:=SizeToExtract;
    ts.Add(szFileName);
    UnZipper.UnZipFiles(ts);
    Result:=0;
  except
    begin
      Result:=-1;
      Ts.Free;
      UnZipper.Free;
    end;
  end;
end;


{--------------------------------------------------------------------}

end.
