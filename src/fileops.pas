{*********** File handling utilities **********}
unit fileops;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes,
  
  logger;

  function MakeDirs(Dirs: string): boolean;
  function CopyFile(FromName: string; ToName: string; Delete: boolean = False): boolean;
  function MakeBackup(FromName: string; ToName: string): boolean;
  function FindZipHdr(const FileName:string; const StartAt:Int64=0):Int64;
  function FindZipHdr(Stream:TStream; const StartAt:Int64=0):Int64;

implementation

  //Make the directories needed for file in Dirs
  function MakeDirs(Dirs: string): boolean;
  var
    i: integer;
    D: string;
    L: TStringList;
    OK: boolean;
  begin
    Result := False;
    Dirs := ExtractFilePath(Dirs);

    //Exit if directory is already there
    if DirectoryExists(Dirs) then
      Exit;

    //Exit if Dirs is not ended with the directory separator
    if (Length(Dirs) = 0) or (Dirs[Length(Dirs)] <> DirectorySeparator) then
      Exit;

    try
      //Get each one of the Paths recursively: e.g. for 'ok/ok1/ok2/'
      //make a list: [ 'ok/ok1/ok2', 'ok/ok1', 'ok', '']
      D := Dirs;
      L := TStringList.Create;
      repeat
        D := ExtractFileDir(D);
        L.Add(D);
      until (D = '') or (D='/');

      //Create each of the needed directories
      OK := True;
      for i := L.Count - 2 downto 0 do
        OK := OK and CreateDir(L[i]);
      Result := OK;
    except
    end;
  end;

  function CopyFile(FromName: string; ToName: string; Delete: boolean = False): boolean;
  var
    SourceF, DestF: TFileStream;
  begin
    //Return failed copy by default
    Result := False;
    if FromName = ToName then
      Exit;
    try
      MakeDirs(ToName);
      Writeln('Copying ', FromName, ' to ', ToName);
      SourceF := TFileStream.Create(FromName, fmOpenRead);
      DestF := TFileStream.Create(ToName, fmCreate);
      DestF.CopyFrom(SourceF, SourceF.Size);
      //Now the copy succeded
      Result := True;
    except
    end;
    if Assigned(SourceF) then
      FreeAndNil(SourceF);
    if Assigned(DestF) then
      FreeAndNil(DestF);

    //Delete original file if asked
    if Result and Delete then
      DeleteFile(FromName);
  end;

  function MakeBackup(FromName: string; ToName: string): boolean;
  var
    OK: boolean;
    Suffix: string;
    Ext: string;
    Dir: string;
    FName: string;
    BakName: string;
  begin
    Ext := ExtractFileExt(ToName);
    Dir := ExtractFilePath(ToName);
    FName := ExtractFileName(ToName);
    FName := ChangeFileExt(FName, '');
    Suffix := FormatDateTime('_YYYY_MM_DD__hh_nn_ss', Now);
    BakName := Dir + FName + Suffix + Ext;
    OK := CopyFile(FromName, BakName);
    //FIXME: Now delete old files
    Result := OK;
  end;

  function FindZipHdr(const FileName:string; const StartAt:Int64=0):Int64;
  Var
    Stream: TStream;
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Result := FindZipHdr(Stream,StartAt);
    FreeAndNil(Stream);
  end;

  function FindZipHdr(Stream:TStream; const StartAt:Int64=0):Int64;
  var
    W: DWord;
    ZipPos: Int64;
    OldPos: Int64;
  const
    //Shift the header signature one bit right because otherwise it
    //can be found in the executable also
    ZipHDR = $04034b50 shr 1;
  begin
    Result := -1;
    Stream.Seek(StartAt, soBeginning);
    repeat
      W := Stream.ReadDWord();
    until (W = ZipHDR shl 1) or (Stream.Size = Stream.Position);

    Stream.Seek(-4, soCurrent);

    if (Stream.Size = Stream.Position) then
      ZipPos := -1
    else
      ZipPos := Stream.Position;

    Result := ZipPos;
  end;
end.
