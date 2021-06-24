unit DoIt;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$ifdef DLLDEBUG}

function OpenTransfer (FileName: PChar; var Handle: longint): longint;
function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint;
function CloseTransfer(Handle: longint): longint;

{$else}

function OpenTransfer (FileName: PChar; var Handle: longint): longint; export;
function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint; export;
function CloseTransfer(Handle: longint): longint; export;

{$endif}

{====================================================================}

implementation

uses
     SysUtils,
     Qstream,
     Classes,Process;


const
  BufSize = 8*1024;




type

  TTransferClass = class
    ReadBuffer    : array [0..BufSize-1] of char;
    ReadBufPos    : longint;
    WasRead       : longint;
    InputFile     : TQBufStream;
    FileFormat    : (unknown, WinWord);

    ReturnLF      : boolean;
    SavedFCh      : char;
    OneWordBuffer : ShortString;
    LastReadChar  : integer;
    end;



  function IndexOfSubString(List: TStrings; SubString: string): Integer;
  var
    I,
     LowIdx,
     HighIdx: Integer;
   Found: boolean;
 begin
   Found := false;
   Result := -1;
   {This type of search uses the first half
    of the TStrings list, so initialize the
    LowIdx and HighIdx to the first and approximate
    half of the list, respectively.}
   LowIdx := 0;
   HighIdx := List.Count div 2;

   {Note that Found and the LowIdx are used
    as conditionals. It's obvious why Found
    is used, but less apparent why LowIdx is
    used instead of HighIdx. The reason for
    this is that the way I've set it up here,
    HighIdx will never exceed (List.Count - 2),
    whereas LowIdx can equal (List.Count - 1)
    by nature of the assignment
    if Found remains false after the for loop.}
   while not Found and (LowIdx < (List.Count - 1)) do
   begin
     for I := LowIdx to HighIdx do
       if (Pos(SubString, List[I]) > 0) and
         not Found then
       begin
         Found := true;
         Result := I;
       end;

     if not Found then
     begin
       LowIdx := HighIdx + 1;
       HighIdx := HighIdx + ((List.Count - HighIdx) div 2);
     end;
   end;
end;




{--------------------------------------------------------------------}



function OpenTransfer (FileName: PChar; var Handle: longint): longint;

  var
    TC           : TTransferClass;

    FileNameOut  : PChar;
    i            : integer;
    s            : string;
    Duration, Video, Audio: string;
    AProcess     : TProcess;
    OutputLines  : TStringList;



begin
Result := -1;
try
  FileNameOut:='/tmp/ffprobe.out';
  ///RunCommand('/usr/bin/ffprobe', ['-hide_banner','-i', ''''+FileName+'''', '2> '''+FileNameOut+''], s);

  OutputLines:=TStringList.Create; //... a try...finally block would be nice to make sure
  AProcess := TProcess.Create(nil);
  // Tell the new AProcess what the command to execute is.
  // Let's use the Free Pascal compiler (i386 version that is)
  AProcess.Executable:= '/bin/sh';
  // Pass -h together with ppc386 so actually 'ppc386 -h' is executed:
  AProcess.Parameters.Add('-c');
  AProcess.Parameters.Add('/usr/bin/ffprobe -hide_banner -i "'+FileName+'"');
  // We will define an option for when the program
  // is run. This option will make sure that our program
  // does not continue until the program we will launch
  // has stopped running.                vvvvvvvvvvvvvv
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  // Now let AProcess run the program
  AProcess.Execute;
  // hProcess should have now run the external executable (because we use poWaitOnExit).
  // Now you can process the process output (standard output and standard error), eg:
  ///OutputLines.Add('stderr:');
  OutputLines.LoadFromStream(aProcess.Stderr);
  // Show output on screen:

  // duration
   Duration:='';
   i:=IndexOfSubString(OutputLines,'Duration:');
   if i >=0 then Duration:=trim(OutputLines.Strings[i]);
   // video
   Video:='';
   i:=IndexOfSubString(OutputLines,'Video:');
   if i >=0 then
   begin
     Video:=trim(OutputLines.Strings[i]);
     Video:=copy(Video,19,length(video));
   end;
   // audio
   Audio:='';
   i:=IndexOfSubString(OutputLines,'Audio:');
   if i >=0 then
   begin
     Audio:=trim(OutputLines.Strings[i]);
     Audio:=copy(Audio,19,length(Audio));
   end;
   OutputLines.Clear;
   OutputLines.Add(Video);
   OutputLines.Add(Audio);
   OutputLines.Add(Duration);




  OutputLines.SaveToFile(FileNameOut);
  AProcess.Free;
  OutputLines.Free;



  TC := TTransferClass.Create;
  TC.ReadBufPos  := 0;
  TC.WasRead := 0;
  TC.OneWordBuffer := '';
  Handle := longint(TC);
  TC.ReturnLF := false;
  TC.InputFile.Init(FileNameOut, stOpenReadNonExclusive);
  TC.FileFormat := Unknown;
  TC.InputFile.ReadExt(TC.ReadBuffer[0], BufSize, TC.WasRead);
  TC.SavedFCh := #0;
except
  on Exception do exit;
  end;
Result := 0;
end;


{--------------------------------------------------------------------}
function GetOneBlock  (Handle: longint; Buf: PChar; BufSize: longint;
                       var CharsRead: longint): longint;

  var
    TC: TTransferClass absolute Handle;


  function GetNextChar(var Ch: char): boolean;
    begin
    with TC do
      begin
      if ReadBufPos >= WasRead then
        begin
        InputFile.ReadExt(ReadBuffer[0], {Read}BufSize, WasRead);
        ReadBufPos := 0;
        end;
      if ReadBufPos >= WasRead then
        begin
        Result := false;
        exit;
        end;
      Ch := ReadBuffer[ReadBufPos];
      inc(ReadBufPos);
      end;
    Result := true;
    end;


  var
    OK    : boolean;
    MaxLen: integer;
    Ch    : Char;

  begin
  CharsRead := 0;
  Result := -1;
  try
    MaxLen := pred(BufSize);
    OK := true;
    while (CharsRead < MaxLen) and OK do
      begin
      try
      OK := GetNextChar(Ch);
      except
        on E: EInOutError do
          begin
          Result := -1;
          exit;
          end;
        end;
      if OK and (Ch <> #0) then
        begin
        Buf[CharsRead] := Ch;
        inc(CharsRead);
        end;
      end;
  except
    on Exception do
      begin
      Buf[CharsRead] := #0;
      exit;
      end;
    end;
  Buf[CharsRead] := #0;
  Result := 0;
  end;



{--------------------------------------------------------------------}

function CloseTransfer(Handle: longint): longint;

  var
    TC: TTransferClass absolute Handle;

  begin
  Result := -1;
  try
    TC.InputFile.Done;
    TC.Free;
  except
    on Exception do exit;
    end;
  Result := 0;
  end;

{--------------------------------------------------------------------}

end.
