{*********** File handling utilities **********}
unit fileops;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes,
  
  logger,regexpr;

type
  TFileFuncName = function (const FileName:string;
                            var Stop:boolean;
                            var Params:TStrings):integer;

  function MakeDirs(Dirs: string): boolean;
  function CopyFile(FromName: string; ToName: string; 
    Delete: boolean = False; const Count:LongInt=-1): boolean;
  function MoveFile(FromName: string; ToName: string; const DeleteFirst:boolean=False): boolean;
  function MakeBackup(const FromName: string;
    const ToName: string;
    const NoToKeep:Integer=3): boolean;
  function FindZipHdr(const FileName:string; const StartAt:Int64=0):Int64;
  function FindZipHdr(Stream:TStream; const StartAt:Int64=0):Int64;
  function ForEachFile(const WildPath:string;
    Func:TFileFuncName;
    Params: TStrings;
    const Flags:LongInt=faAnyFile and faDirectory):integer;

implementation

  function ForEachFile(const WildPath:string;
    Func:TFileFuncName; Params:TStrings;
    const Flags:LongInt=faAnyFile and faDirectory):integer;
  var
    Info : TSearchRec;
    Stop : boolean;
  begin
    //Result:=False;
    Stop := False;
    If FindFirst(WildPath,Flags,Info)=0 then
    begin
      Repeat
        Result:=Func(Info.Name,Stop,Params);
      Until (FindNext(info)<>0) or Stop;
    end;
    FindClose(Info);
  end;

  function CompareBakEntries(BakList:TStringList;i,j:Integer):Integer;
  begin
    Result := StrToInt64Def(BakList.Names[j],0)
      -StrToInt64Def(BakList.Names[i],0);
  end;

  function MakeList(const Name:String;var Stop:boolean; var Params:TStrings):integer;
  Const
    Regex='.*_(\d{4})_(\d{2})_(\d{2})__(\d{2})_(\d{2})_(\d{2}).*';
  Var
    DT: string;
    List: TStrings;
    S: string;
  begin
    Result:=0;
    List:=Params;
    S:=Name;
    If ExecRegExpr(Regex,S) then
    begin
      DT:=ReplaceRegExpr(Regex,S,'$1$2$3$4$5$6',True);
      List.Add(DT+'='+Name);
    end;
    Result:=List.Count;
  end;

  procedure DeleteOld(const BaseName:string; const NoToKeep:Integer);
  var
    List:TStringList;
    DT:Int64;
    i,DCount:Integer;
    Dir,FN:string;
    F:TFileFuncName;
  begin
    List:=TStringList.Create;
    try
      try
        //Build list of files
        Dir := ConcatPaths([ExtractFilePath(BaseName),'*']);
        ForEachFile(Dir,@MakeList,List);

        //Exit if we are asked to keep more than what already exists
        If NoToKeep >= List.Count then
          Exit;

        //Sort the list
        List.CustomSort(@CompareBakEntries);
        DCount:=0;

        //Now delete the last files in the list
        For i:=NoToKeep to List.Count-1 do
        begin
          FN:= ConcatPaths([ExtractFilePath(BaseName),List.ValueFromIndex[i]]);
          FN:= ExpandFileName(FN);
          DeleteFile(FN);
          Inc(DCount);
        end;
        Log('Deleted ('+IntToStr(DCount)+') backups.');
      finally
        If Assigned(List) then
          FreeAndNil(List);
      end;
    except on E:Exception do
       Error('Unable to delete old backups: ' + E.toString + ' - ' + E.Message);
    end;
  end;

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
    begin
      Result:=True;
      Exit;
    end;
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
      until (D = '') or (AnsiLastChar(D)=DirectorySeparator);

      //Create each of the needed directories
      OK := True;
      for i := L.Count - 2 downto 0 do
      begin
        if not DirectoryExists(L[i]) then
          OK := OK and CreateDir(L[i]);
      end;

      Result := OK;
    except
    end;
    FreeAndNil(L);
  end;

  function CopyFile(FromName: string; ToName: string; 
    Delete: boolean = False; const Count:LongInt=-1): boolean;
  var
    SourceF, DestF: TFileStream;
    _Count:LongInt;
  begin
    //Return failed copy by default
    Result := False;
    _Count := Count;
    if FromName = ToName then
      Exit;
    try
      MakeDirs(ToName);
      Log('Copying ''' + FromName + ''' to ''' + ToName + '''');
      SourceF := TFileStream.Create(FromName, fmOpenRead);
      DestF := TFileStream.Create(ToName, fmCreate);
      If _Count = -1 then
        _Count := SourceF.Size;
      DestF.CopyFrom(SourceF, _Count);
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

  function MoveFile(FromName: string; ToName: string; const DeleteFirst:boolean=False): boolean;
  Var
    Dir:string;
    OK:boolean;
  begin
    Dir := ExtractFilePath(ToName);
    OK := MakeDirs(Dir);
    if OK then
    begin
      If DeleteFirst and FileExists(ToName) then
        DeleteFile(ExpandFileName(ToName));
      OK := RenameFile(FromName,ToName);
    end;
    If not OK then
      logger.Error('Unable to move ''' + FromName + ''' to ''' + ToName + '''');
    Result:=OK;
  end;

  function MakeBackup(const FromName: string;
    const ToName: string;
    const NoToKeep:Integer=3): boolean;
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
    OK := fileops.CopyFile(FromName, BakName);
    DeleteOld(ToName,NoToKeep); //Now delete old files
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
    Buf: string;
    ZipPos, ReadCount: Int64;
    Found: boolean;
    i: Integer;
    //Shift the header signature one bit right because otherwise it
    //can be found in the executable also
    //Header is 50 4b 03 04 14 00 00 00      for our unzip utility
    ZipHdr: array[0..7] of byte = ( $50 shl 1,$4b shl 1, $03 shl 1, $04 shl 1,
                                    $14 shl 1,$00 shl 1, $00 shl 1, $00 shl 1);
    ZipHdrC: array[0..7] of AnsiChar; //To use Pos()
  const
    BUFSIZE=33*1024; //To honor Our Lord On the Vigil of Dec 8,2015

  begin
    ZipPos := -1;
    Result:=-1;
    Found := False;
    //Unshift the Zip header
    For i := 0 to 7 do
      ZipHdrC[i] := Char(ZipHdr[i] shr 1);
    //Position stream at specified offset ( to prevent going through parts
    // of the file which we know don't have the ziphdr)
    If StartAt > Stream.Size then
      Raise Exception.Create('Find Zip Header - offset '''+ IntToStr(StartAt) + '''is greater than file size!');
    Stream.Seek(StartAt, soBeginning);
    SetLength(Buf,BUFSIZE);
    repeat
      ReadCount := Stream.Read(Buf[1],BUFSIZE);
      SetLength(Buf,ReadCount);
      ZipPos := Pos(ZipHdrC,Buf);
    until (ZipPos>0) or (Stream.Size = Stream.Position);

    if (ZipPos>0) then //Zip Header found
    begin
      Stream.Seek(-ReadCount+(ZipPos-1), soCurrent);
      ZipPos := Stream.Position;
    end
    else
      ZipPos := -1;
    Log('Zip header found at: '+IntToStr(ZipPos));
    Result := ZipPos;
  end;
end.
