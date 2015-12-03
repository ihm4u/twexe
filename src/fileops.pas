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
    //Exit if Dirs is not ended with the directory separator
    if Dirs[Length(Dirs)] <> DirectorySeparator then
      Exit;

    try
      //Get each one of the Paths recursively: e.g. for 'ok/ok1/ok2/'
      //make a list: [ 'ok/ok1/ok2', 'ok/ok1', 'ok', '']
      D := Dirs;
      L := TStringList.Create;
      repeat
        D := ExtractFileDir(D);
        L.Add(D);
      until D = '';

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
    SourceF.Free;
    DestF.Free;

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
    //Now delete old files
    Result := OK;
  end;

end.
