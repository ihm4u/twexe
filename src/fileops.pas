{*********** File handling utilities **********}
unit fileops;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  logger, regexpr;

type
  TFileFuncName = function(const FileName: string;
    Params: TFPList): boolean;

//Return the name of the file without extension or directory path
function FileNameNoExt(Name:String):String;
function MakeDirs(Dirs: string): boolean;
function CopyFile(FromName: string; ToName: string; DeleteOriginal: boolean = False;
  const Count: longint = -1): boolean;
function MoveFile(FromName: string; ToName: string;
  const DeleteFirst: boolean = False): boolean;
function MakeBackup(const FromName: string; const ToName: string;
  const NoToKeep: integer = 3): boolean;
function FindZipHdr(const FileName: string; const StartAt: int64 = 0): int64;
function FindZipHdr(Stream: TStream; const StartAt: int64 = 0): int64;
procedure ForEachFile(const WildPath: string; Func: TFileFuncName;
  Params: TFPList; const Flags: longint = faAnyFile and faDirectory);
procedure DeleteDirectory(const Dir: string);

implementation
//
// Delete directories recursively
//
procedure DeleteDirectory(const Dir: string);
var
  F: TSearchRec;
begin
  if FindFirst(Dir + DirectorySeparator +'*', faAnyFile, F) = 0 then begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then begin
          if (F.Name <> '.') and (F.Name <> '..') then begin
            DeleteDirectory(ConcatPaths([Dir,F.Name]));
          end;
        end else begin
          //LogFmt('Deleting %s', [ConcatPaths([Dir,F.Name])]);
          DeleteFile(ConcatPaths([Dir,F.Name]));
        end;
      until FindNext(F) <> 0;
    finally
      FindClose(F);
    end;
    //LogFmt('Deleting %s/', [Dir]);
    RemoveDir(Dir);
  end;
end;

//
// Get File name without extension
//
function FileNameNoExt(Name:String):String;
begin
   Result:=ChangeFileExt(ExtractFileName(Name),'');
end;

procedure ForEachFile(const WildPath: string; Func: TFileFuncName;
  Params: TFPList; const Flags: longint = faAnyFile and faDirectory);
var
  Info: TSearchRec;
  Stop: boolean;
begin
  //Result:=False;
  Stop := False;
  if FindFirst(WildPath, Flags, Info) = 0 then
  begin
    repeat
      Stop := Func(Info.Name, Params);
    until (FindNext(info) <> 0) or Stop;
  end;
  FindClose(Info);
end;

function CompareBakEntries(BakList: TStringList; i, j: integer): integer;
begin
  Result := StrToInt64Def(BakList.Names[j], 0) - StrToInt64Def(BakList.Names[i], 0);
end;

function MakeList(const Name: string; Params: TFPList): boolean;
const
  DateRegex = '_(\d{4})_(\d{2})_(\d{2})__(\d{2})_(\d{2})_(\d{2})';
var
  DT,Basename,S,Regex: string;
  List: TStrings;
begin
  Result := False;
  Basename := string(Params[0]);
  List := TStringList(Params[1]);
  Regex := QuoteRegExprMetaChars(FileNameNoExt(Basename)) + DateRegex
    + QuoteRegExprMetaChars(ExtractFileExt(Basename));
  S := Name;
  if ExecRegExpr(Regex, S) then
  begin
    DT := ReplaceRegExpr(Regex, S, '$1$2$3$4$5$6', True);
    List.Add(DT + '=' + Name);
  end;
end;

procedure DeleteOld(const BaseName: string; const NoToKeep: integer);
var
  List: TStringList;
  Params: TFPList;
  i, DCount: integer;
  Dir, FN: string;

begin
  List := TStringList.Create;
  Params := TFPList.Create;
  try
    try
      //Build list of files
      Dir := ConcatPaths([ExtractFilePath(BaseName), '*']);
      Params.Add(Pointer(BaseName));
      Params.Add(Pointer(List));
      ForEachFile(Dir, @MakeList, Params);

      //Exit if we are asked to keep more than what already exists
      if NoToKeep >= List.Count then
        Exit;

      //Sort the list
      List.CustomSort(@CompareBakEntries);
      DCount := 0;

      //Now delete the last files in the list
      for i := NoToKeep to List.Count - 1 do
      begin
        FN := ConcatPaths([ExtractFilePath(BaseName), List.ValueFromIndex[i]]);
        FN := ExpandFileName(FN);
        DeleteFile(FN);
        Inc(DCount);
      end;
      Log('Deleted (' + IntToStr(DCount) + ') backups.');
    finally
      if Assigned(List) then
        FreeAndNil(List);
      if Assigned(Params) then
        FreeAndNil(Params);
    end;
  except
    on E: Exception do
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
    Result := True;
    Exit;
  end;
  //Exit if Dirs is not ended with the directory separator
  if (Length(Dirs) = 0) or (Dirs[Length(Dirs)] <> DirectorySeparator) then
    Exit;

  try
    //Get each one of the Paths recursively: e.g. for 'ok/ok1/ok2/'
    //make a list: [ 'ok/ok1/ok2', 'ok/ok1', 'ok', '']
    try
      D := Dirs;
      L := TStringList.Create;
      repeat
        D := ExtractFileDir(D);
        L.Add(D);
      until (D = '') or (AnsiLastChar(D) = DirectorySeparator);

      //Create each of the needed directories
      OK := True;
      for i := L.Count - 2 downto 0 do
      begin
        if not DirectoryExists(L[i]) then
          OK := OK and CreateDir(L[i]);
      end;
      Result := OK;
    finally
      If Assigned(L) then
        FreeAndNil(L);
    end;
  except on E:Exception do
    Raise Exception.CreateFmt('Unable to create directory ''%s'': %s',[Dirs,E.toString]);
  end;
end;

function CopyFile(FromName: string; ToName: string; DeleteOriginal: boolean = False;
  const Count: longint = -1): boolean;
var
  SourceF, DestF: TFileStream;
  _Count: longint;
begin
  //Return failed copy by default
  Result := False;
  _Count := Count;
  if FromName = ToName then
    Exit;
  try
    try
      MakeDirs(ToName);
      Log('Copying ''' + FromName + ''' to ''' + ToName + '''');
      SourceF := TFileStream.Create(FromName, fmOpenRead);
      DestF := TFileStream.Create(ToName, fmCreate);
      if _Count = -1 then
        _Count := SourceF.Size;
      DestF.CopyFrom(SourceF, _Count);
      //Now the copy succeded
      Result := True;
    finally
      if Assigned(SourceF) then
        FreeAndNil(SourceF);
      if Assigned(DestF) then
        FreeAndNil(DestF);
    end;
  except on E:Exception do
    raise Exception.CreateFmt('Unable to copy ''%s''  to ''%s'' : %s',[FromName,ToName,E.toString]);
  end;

  //Delete original file if asked
  if Result and DeleteOriginal then
    DeleteFile(FromName);
end;

function MoveFile(FromName: string; ToName: string;
  const DeleteFirst: boolean = False): boolean;
var
  Dir: string;
  OK: boolean;
begin
  Dir := ExtractFilePath(ToName);
  OK := MakeDirs(Dir);
  if OK then
  begin
    if DeleteFirst and FileExists(ToName) then
      DeleteFile(ExpandFileName(ToName));
    OK := RenameFile(FromName, ToName);
  end;
  if not OK then
    logger.Error('Unable to move ''' + FromName + ''' to ''' + ToName + '''');
  Result := OK;
end;

function MakeBackup(const FromName: string; const ToName: string;
  const NoToKeep: integer = 3): boolean;
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
  DeleteOld(ToName, NoToKeep); //Now delete old files
  Result := OK;
end;

function FindZipHdr(const FileName: string; const StartAt: int64 = 0): int64;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Result := FindZipHdr(Stream, StartAt);
  FreeAndNil(Stream);
end;

function FindZipHdr(Stream: TStream; const StartAt: int64 = 0): int64;
var
  Buf: string;
  ZipPos, ReadCount: int64;
  i: integer;
  //Shift the header signature one bit right because otherwise it
  //can be found in the executable also
  //Header is 50 4b 03 04 14 00 00 00      for our unzip utility
  ZipHdr: array[0..7] of byte = ($50 shl 1, $4b shl 1, $03 shl 1,
    $04 shl 1, $14 shl 1, $00 shl 1,
    $00 shl 1, $00 shl 1);
  ZipHdrC: array[0..7] of AnsiChar; //To use Pos()
const
  BUFSIZE = 33 * 1024; //To honor Our Lord On the Vigil of Dec 8,2015

begin
  ZipPos := -1;
  Result := -1;

  //Unshift the Zip header
  for i := 0 to 7 do
    ZipHdrC[i] := char(ZipHdr[i] shr 1);
  //Position stream at specified offset ( to prevent going through parts
  // of the file which we know don't have the ziphdr)
  if StartAt > Stream.Size then
    raise Exception.Create('Find Zip Header - offset ''' + IntToStr(StartAt) +
      '''is greater than file size!');
  Stream.Seek(StartAt, soBeginning);
  SetLength(Buf, BUFSIZE);
  repeat
    ReadCount := Stream.Read(Buf[1], BUFSIZE);
    SetLength(Buf, ReadCount);
    ZipPos := Pos(ZipHdrC, Buf);
  until (ZipPos > 0) or (Stream.Size = Stream.Position);

  if (ZipPos > 0) then //Zip Header found
  begin
    Stream.Seek(-ReadCount + (ZipPos - 1), soCurrent);
    ZipPos := Stream.Position;
  end
  else
    ZipPos := -1;
  Log('Zip header found at: ' + IntToStr(ZipPos));
  Result := ZipPos;
end;

end.
