unit exedata;

{$mode objfpc}{$H+}

interface

uses
  // Free pascal units
  Classes, SysUtils, Zipper, Process,

  //Project units 
  logger,fileops,
  {$ifdef unix}
  unixlib
  {$endif}
  {$ifdef windows}
  windowslib
  {$endif}
  ;

type
  TCallacks = class(TObject)
    class procedure OnOpenZippedStream(UnZipper: TObject; var FZipStream: TStream);
  end;

Var
  OriginalExeFile: string;

//Executes command synchronously or asynchronously
function RunCmd(const Cmd: string; var Output: string;
  const Async: boolean = False; const Input: string = '';
  const KillAfterNSeconds:Integer=-1): integer;

//Return complete file name and path of the original executable (not shadow)
function GetEXEFile():String;

//Extract appended zip file in the executable
procedure ExtractData(ExeFile:string; const Dir: string=''; Delete: boolean = False);

//Compress data and append it to specified executable
procedure AppendFile(const ExeFile:string; const DataFile: string);

//Get Name of original executable (not shadow), without extension
function GetEXEName():String;

//Get Path of original executable (not shadow), including directory separator
function GetEXEPath():String;

//Run a shadow executable of the current one
function RunShadow(const OpenBrowser:boolean):Boolean;

//Returns True if the currently running process is a shadow
function IAmShadow():Boolean;

function GetStoragePath():string;
function GetUnZipPath():string;
function GetServerDocPath():string;
function GetShadowFile():string;

implementation

//
// Get shadow executable file name and path
//
function GetShadowFile():String;
begin
  Result:=ConcatPaths([GetStoragePath(),'_exe','_' + GetEXEName()+GetOSEXEExt()]);
end;

//
// Get executable name, without extension, if any.
//
function GetEXEFile():String;
begin
  If IAmShadow() then
    Result:=OriginalExeFile
  else 
    Result:=ParamStr(0);

  If Result='' then
    Raise Exception.Create('Can not get executable file path!!');
end;

//
// Get executable name, without extension, if any.
//
function GetEXEName():String;
begin
   Result:=FileNameNoExt(ParamStr(0));
   if (Result[1]='_') then
      Delete(Result,1,1);
end;


//
// Get executable path, including trailing separator
//
function GetEXEPath():String;
begin
   Result:=ExtractFilePath(GetEXEFile());
end;

function IAmShadow():Boolean;
Var
   ExeName:string;
begin
   Result:=False;
   ExeName:=FileNameNoExt(ParamStr(0));
   If ExeName[1]='_' then
      Result:=True;
end;

//
// Execute process
//
function RunCmd(const Cmd: string; var Output: string;
  const Async: boolean = False;
  const Input:string = '';
  const KillAfterNSeconds:Integer=-1): integer;
const
  {$ifdef windows}
  EOFInd=#26+LineEnding;
  {$endif}
  {$ifdef unix}
  EOFInd='';
  {$endif}
var
  Out: TStrings;
  AProcess: TProcess;
  RunningmSecs:Integer;
  Terminated:boolean;
begin
  Result := -1;
  Terminated := False;
  AProcess := TProcess.Create(nil);
  Out := TStringList.Create;
  try
    AProcess.CommandLine := Cmd;

    if Async then
      AProcess.Options := AProcess.Options // + [poNoConsole]
    else
      AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes,
      poNoConsole, poStderrToOutPut];

    If not Async and ((Length(Input) > 0) or (KillAfterNSeconds>0)) then
    begin
      AProcess.Options := AProcess.Options - [poWaitOnExit];// + [poNoConsole];
      AProcess.Execute;
      //Send input on stdin
      If (Length(Input) > 0) then
      begin
        Aprocess.Input.Write(Input[1],Length(Input));
        If EOFInd <> '' then
        begin
          Aprocess.Input.Write(EOFInd[1],Length(EOFInd));
          Log(Format('Wrote EOF after external proc input: %d bytes.',[Length(EOFInd)]));
        end;
        AProcess.CloseInput;
      end;

      //If KillAfterNSeconds was specified, kill process
      //if it has not ended after that number of seconds
      RunningmSecs:=0;
      If KillAfterNSeconds > 0 then
      begin
        While AProcess.Running and (RunningmSecs<KillAfterNSeconds*1000) do
        begin
          Sleep(50);
          RunningmSecs+=50;
          If RunningmSecs >= KillAfterNSeconds*1000 then
          begin
            AProcess.Terminate(1);
            Output := '';
            Terminated:=True;
            AProcess.Parameters.StrictDelimiter:=True;
            AProcess.Parameters.Delimiter:=' ';
            Show(Format('External process ''%S'' exceeded alloted time (used %.1F secs). Terminating it.',
              [FileNameNoExt(AProcess.Executable) + ' '
               + Aprocess.Parameters.DelimitedText,
               RunningmSecs / 1000]));
          end;
        end;
      end
      else //No KillAfterNSeconds;
        AProcess.WaitOnExit;
    end
    else //No input and no KillAfterNSeconds
      AProcess.Execute;

    Result:= 0;

    if not Async and not Terminated then
    begin
      Out.BeginUpdate;
      Out.Clear;
      Out.LoadFromStream(AProcess.Output);
      Out.EndUpdate;
      Output := Out.Text;
      Result := AProcess.ExitStatus;
    end
  finally
    if Assigned(AProcess) then
      FreeAndNil(AProcess);
    if Assigned(Out) then
      FreeAndNil(Out);
  end;
end;

// Make a shadow of the current executable
// A shadow is a copy of the executing code of the
// specified executable without including the appended data
function MakeShadow(ExeFN: string; ShadowFN: string):Boolean;
Var
  ZipPos: LongInt;
begin
  ZipPos:=FindZipHdr(ExeFN);
  //If ZipPos is -1 it will copy the whole file
  Result:=fileops.CopyFile( ExeFN, ShadowFN, False, ZipPos);
end;

//
// Run copy of executable to allow writing over the original executable
// The caller is responsible for exiting
//
function RunShadow(const OpenBrowser:boolean):Boolean;
Var
  OK:Boolean;
  NewExe:string;
  Out:string;
  Cmd:string;
  Opts: string;
begin
  Out:='';
  Result:=False;

  //Exit if we are already running in a shadow
  if IAmShadow() then
    Exit;

  //Propagate flag to not open the browser
  If OpenBrowser then
    Opts:=''
  else
    Opts:=' -s';

  //Copy executable to shadow file
  NewExe := GetShadowFile();
  Cmd := NewExe + ' -z "' + ParamStr(0) + '"' + Opts;
  Log('Creating shadow: '+Cmd);
  try
    //Copy executable section of exe to shadow file
    OK:=MakeShadow( ParamStr(0), NewExe );
  except
    on E:EAccessViolation do
      begin
        logger.Error(''''+ GetEXEName + ''' is already running: ' + E.Message);
        ExitCode := 3;
        Raise;
      end;
    on E: Exception do
      begin
        logger.Error('Unable to create '''+ NewExe + ''': ' + E.Message);
        ExitCode := 4;
        Raise;
      end;
  end;
  
  //Set executable bit on file and run it
  if (OK) then
  begin
    SetExecutePermission(NewExe);
    OK:= RunCmd(Cmd,Out,True) <> -1;
  end
  else
  begin
    logger.Error('Unable to copy ''' + ParamStr(0) + ''' executable to create shadow: '+NewExe);
    Raise Exception.Create(''''+ GetEXEName + ''' is already running?');
  end;
  Result:=OK;
end;


//
//Make a Memory Stream containing the data to unzip
//
class procedure TCallacks.OnOpenZippedStream(UnZipper: TObject; var FZipStream: TStream);
var
  MemStr: TMemoryStream;
  ZipPos: int64;
begin
  FZipStream := TFileStream.Create(GetEXEFile(), fmOpenRead or fmShareDenyWrite);
  //Jump to position 800000 to make the search faster, since we
  //know the executable size is larger than 800000. Note that this number
  //must be divisible by 4, otherwise the ReadDWord will be off-base
  ZipPos := FindZipHdr(FZipStream,800000);

  //Copy Zip data to a Memory Stream
  if (ZipPos <> -1) then
  begin
    Log('Found data at position: ' + IntToStr(ZipPos));
    MemStr := TMemoryStream.Create;
    MemStr.CopyFrom(FZipStream, FZipStream.Size - ZipPos);
    FreeAndNil(FZipStream);
    FZipStream := MemStr;
  end
  else
  begin
    logger.Error('No TiddlyWiki found.');
    logger.Error('Please download twexe again.');
    FreeAndNil(FZipStream);
  end;
end;

//
// Extract ZIP file contained in the executable
//
procedure ExtractData(ExeFile:string; const Dir: string=''; Delete: boolean = False);
var
  UnZipper: TUnZipper;
  CB: TCallacks;
  FDir: string;

begin
  //Error if ExeFile does not exist
  If not FileExists(ExeFile) then
  begin
    logger.Error('Executable file ''' + ExeFile + ''' not found.');
    Exit;
  end;

  //Extract zipped data from file
  FDir := Dir;
  If Dir='' then
    FDir := GetUnZipPath();
  CB := TCallacks.Create;
  UnZipper := TUnZipper.Create;
  UnZipper.OnOpenInputStream := @CB.OnOpenZippedStream;
  try
    UnZipper.FileName := ExeFile;
    UnZipper.OutputPath := FDir;
    UnZipper.UnZipAllFiles;
    Log('Extracted data in ' + UnZipper.OutputPath + ' from ' + ExeFile);
  finally
    FreeAndNil(CB);
    if Assigned(UnZipper) then
      FreeAndNil(UnZipper);
    if Delete then
      DeleteFile(PChar(ExeFile));
  end;
end;

// Zip the specified file to a stream
function ZipDataFile(const DataFile: string):String;
var
  Zipper: TZipper;
  TmpFile: string;
  S:TStrings;
  OldDir,Dir:string;

begin
  Result:='';
  //Zip data file into a temporary zip file
  Dir:=ExtractFileDir(DataFile);
  OldDir:=GetCurrentDir();
  If (Dir<>'') then
    SetCurrentDir(Dir);

  Zipper := TZipper.Create;
  S:=TStringList.Create;
  S.Add(ExtractFileName(DataFile));
  TmpFile := GetTempDir() + FileNameNoExt(DataFile) + '.zip';
  with Zipper do
  begin
    try
      ZipFiles(TmpFile, S);
    finally
      Free;
      S.Free;
    end;
  end;

  If (Dir<>'') then
    SetCurrentDir(OldDir);
  Result:=TmpFile;
end;

//
// Replace ZIP file contained in the executable with specified data file
//
procedure AppendFile(const ExeFile:string; const DataFile: string);
Var
  ZFN: string;
  ZFS: TStream;
  ExeFS: TStream;
  ZipPos: Int64;
  CleanEXESize: Int64;
begin
  ZFN:=ZipDataFile(DataFile);

  //Append zipped file to executable
  ExeFS := TFileStream.Create(ExeFile, fmOpenReadWrite);
  ZFS := TFileStream.Create(ZFN, fmOpenRead or fmShareDenyWrite);
  CleanEXESize := ExeFS.Size;
  ZipPos:=FindZipHdr(ExeFS,800000);
  if (ZipPos<>-1) then
    CleanEXESize := ZipPos;

  //FindZipHdr leaves the stream at the end of the file or 
  // at the start of the Zip header, so we can copy the zip
  // stream here
  ExeFS.CopyFrom(ZFS, ZFS.Size);

  //Make sure the file size is correct
  //so that spurious data is truncated
  ExeFS.Size := CleanEXESize + ZFS.Size;

  FreeAndNil(ExeFS);
  FreeAndNil(ZFS);

  //Delete temporary zip file
  DeleteFile(ZFN);

  //Set executable bit on exe
  SetExecutePermission(ExeFile);
end;

function GetStoragePath():string;
begin
   Result:= ConcatPaths([GetTempDir(),GetEXEName()+'.twx']) + DirectorySeparator;
end;

function GetUnZipPath():string;
begin
   Result:= GetStoragePath() + '_zip' + DirectorySeparator;
end;

function GetServerDocPath():string;
begin
   Result := GetEXEPath();
end;
end.
