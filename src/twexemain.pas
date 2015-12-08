unit twexemain;

{$mode objfpc}{$H+}
{$define UseCThreads}
interface

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  SysUtils,
  Classes,
  fphttpserver,
  fpmimetypes,
  httpdefs,
  regexpr,
  lclintf,

  twexehttpserver,exedata,logger,fileops,wikiops,version
  {$ifdef unix}
  ,unixlib
  {$endif}
  {$ifdef windows}
  ,windowslib
  {$endif};

procedure TwexeMain(const OpenBrowser: boolean;
  const OrigExeFile: string;
  const FileArgs:array of string);
procedure PrintHeader();

implementation
  var
    FOpenBrowser: boolean;
    FFileArgs : array of string;

  function TryPort(Port: word): TTwexeHTTPServer;
  var
    Serv: TTwexeHTTPServer;
  begin
    Serv := TTwexeHTTPServer.Create(nil);
    try
      Serv.OpenBrowser := FOpenBrowser;;
      Serv.BaseDir := GetServerDocPath();
      {$ifdef unix}
      Serv.MimeTypesFile := '/etc/mime.types';
      {$endif}
      Serv.Threaded := False;
      Serv.Port := Port;
      Msg('Trying port ' + IntToStr(Serv.Port),False);
      Serv.Active := True;
    except
      On Exception do
      begin
        Msg('...Busy');
        FreeAndNil(Serv);
      end;
    end;

    Result := Serv;
  end;

  function StartServer(): TTwexeHTTPServer;
  var
    Port: word;
  begin
    Port := 8080;
    repeat
      Result := TryPort(Port);
      if Result = nil then
        Inc(Port);
    until (Result <> nil) or (Port > 8095);
  end;

  procedure PrintHeader();
  begin
    Msg('▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁');
    Msg('⚜ Single File TiddlyWiki executable ⚜');
    Msg('▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔');
    Msg('Version: '+_VERSION);
  end;

  //Extract data or abort
  procedure HandleExtractData();
  Var
    WikiName: string;
    NewName: string;
  begin
    try
      WikiName:='';
      Log('Extracting data from '''+ GetEXEFile() + '''');
      ExtractData(GetEXEFile());
      //Make sure unzipped wiki file has the same name as the twixie
      FindWikiFile(GetUnZipPath(),WikiName);
      NewName := GetUnZipPath() + GetEXEName() + '.html';
      if WikiName <> NewName then
      begin
        Log('Renaming '''+WikiName+''' to '''+NewName+'''.');
        MoveFile(WikiName,NewName);
      end;
    except
      on E:Exception do
      begin
        Error('Error extracting data: '+E.Message);
        ExitCode := 2;
        Raise;
      end;
    end;
  end;

  procedure ConvertWikiToExe(DataFile:string);
  Var
    OK:Boolean;
    OutExeFN:String;
    Ext:string;
  begin
    Ext := GetOSEXEExt();
    OutExeFN := ChangeFileExt(ExpandFileName(DataFile),Ext);
    Msg('Generating ''' + OutExeFN + '''...');
    OK:=CopyFile(GetEXEFile(),OutExeFN);
    
    if (OK) then
    begin
      AppendFile(OutExeFN,DataFile);
      Msg('Congratulations! '''+ ExtractFileName(DataFile) 
              + ''' has been converted to a twixie named ''' + OutExeFN + '''.');
    end
      else
      Error('Unable to create ''' + OutExeFN + '''');
  end;

  procedure RestartEXE();
  Var
    Out:String;
  begin
    Out := '';
    //Restart executable without opening the browser
    RunCmd(GetEXEFile()+' -n',Out,True);
  end;

procedure TwexeMain(const OpenBrowser: boolean;
  const OrigExeFile: string;
  const FileArgs:array of string);
  Var
    Serv: TTwexeHTTPServer;
    WikiToConvert: string;
    i: Integer;
    Browser, O: string;
begin
  LogVerbose := True;
  LogDebug := 0;

  FOpenBrowser := OpenBrowser;
  Exedata.OriginalExeFile := OrigExeFile;
  SetLength(FFileArgs,Length(FileArgs));

  For i:=0 to Length(FileArgs) - 1 do
    FFileArgs[i] := FileArgs[i];

  //Print version info and header
  If not IAmShadow() then
    PrintHeader();

  //Convert the first found wiki file in the command line arguments
  //to a twixie in the same directory
  if (Length(FFileArgs)>0) then
  begin
    WikiToConvert := '';
    i := 0;
    repeat
      WikiToConvert := ExpandFileName(FFileArgs[i]);
      Inc(i);
    until IsWikiFile(WikiToConvert) or (i=Length(FFileArgs));
    If IsWikiFile(WikiToConvert) then
       ConvertWikiToExe(WikiToConvert)
    else
      Error('Sorry, no wiki file found in the arguments.');

    Exit;
  end;

  try
    //Run shadow and exit,
    //continue if we're not the shadow
    if RunShadow(FOpenBrowser) then
    begin
      Log('Exiting, shadow created and running.');
      Exit;
    end;
  except
    on E:EAccessViolation do //Shadow was running already
    begin
      If (FOpenBrowser) then
      begin
        OpenURL(TTwexeHTTPServer.ReadLastURL());
        Log('Trying to wake up browser...');
      end;
      Exit;
    end;
    on E:Exception do
    begin
      Error('Unable to start shadow: ' + E.Message);
      Exit;
    end;
  end;

  //Extract data bundled in executable
  HandleExtractData();

  try
    try
      //Start HTTP server
      Sleep(200); //Wait a little to give time for previous exe to end
      Serv:=StartServer();
    finally
      If Assigned(Serv) then
         FreeAndNil(Serv);
    end;
  except on E:Exception do
    begin
      Error('Server Exited abnormally: ' + E.Message);
      Error('Aborting.');
      Exit;
    end;
  end;
  //Server is single-threaded, but it stops listening after a
  // Post request, so we restart the EXE to that everything is
  // reloaded
  //FIXME: Cleanup temp files: shadow _exes in tmp, unzip dir
  RestartEXE();
end;

end.
