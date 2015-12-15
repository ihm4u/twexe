unit twexemain;

{$codepage UTF8}
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
  httpdefs,
  regexpr,
  lclintf,
  ssockets, //For ESocketError

  {twexe units}
  twexehttpserver,exedata,logger,fileops,wikiops,version,upgrade
  {$ifdef unix}
  ,unixlib
  {$endif}
  {$ifdef windows}
  ,windowslib
  ,Windows {for setconsoleoutputcp}
  {$endif};

type
  TTwexeOption = (toOpenBrowser,toAllowRemoteClients);
  TTwexeOptions = set of TTwexeOption;
var
  TwexeOptions : TTwexeOptions;
  ConversionOutDir: string='';

procedure TwexeMain(
  const OrigExeFile: string;
  const FileArgs:array of string);

procedure PrintHeader();
procedure HandleExtractData(const Dir:string='');

implementation
  var
    DoNotWaitForUser:boolean;
    FFileArgs : array of string;
    Serv: TTwexeHTTPServer;
    StoreReqFinished,StopRequested,Restarting: boolean;

  function TryPort(Port: word; var TryAgain:boolean; var StoreReqDone:boolean): TTwexeHTTPServer;
  var
    Serv: TTwexeHTTPServer;
  begin
    Serv := TTwexeHTTPServer.Create;
    try
      //Server should open browser as soon as it is listening
      Serv.OpenBrowser := (toOpenBrowser in TwexeOptions);
      Serv.BaseDir := GetServerDocPath();

      Serv.StopRequested := False;
      Serv.Threaded := False;
      Serv.Port := Port;
      TryAgain := True;
      StoreReqDone := False;
      //We put a newline in front because it is the first thing
      //reported by the shadow, which is a different process
      //and we may have the shell prompt in the middle...we
      //want it to look nice
      Log(LineEnding+'Trying port ' + IntToStr(Serv.Port) );
      try
        Serv.Active := True;
      finally
        //Read comment below about seAcceptFailed
        StopRequested:=Serv.StopRequested;
        StoreReqDone:=Serv.StoreRequestDone;
      end;
    except
      On E:ESocketError do
      begin
        If E.Code = seBindFailed then
        begin
          Log('Port ' + IntToStr(Serv.Port) + ' is busy: ' + E.Message);
          TryAgain := True
        end;
        //We get E.Code=seAcceptFailed after a handler thread
        //has called Serv.Active:=False, this is done in some
        //post requests which stop the server:
        //1. When a /store POST has been received
        //2. Whan a /twexe/api/exitserver POST has been received
        //The reason is that the thread clears the socket and
        //there is a bug in fphttpserver not handling it properly

        //Either way, free the Server
        FreeAndNil(Serv);
      end;
      On E:Exception do
        FreeAndNil(Serv);
    end;

    Result := Serv;
  end;

  function StartServer(): TTwexeHTTPServer;
  var
    Port: word;
    TryAgain : boolean;
  begin
    Port := 8080;
    TryAgain := True;
    repeat
      Result := TryPort(Port,TryAgain,StoreReqFinished);

      if Result = nil then
        Inc(Port)
    until (Result <> nil)
      or (Port > 8095)
      or not TryAgain
      or StopRequested
      or StoreReqFinished;
  end;

  procedure PrintHeader();
  begin
    {$IFDEF WINDOWS}
    SetConsoleOutputCP(CP_UTF8);
    {$ENDIF}
    { Ivrit ascii font from:                                     }
    {http://patorjk.com/software/taag/#p=display&f=Ivrit&t=TWEXE }
    WriteLn();
    TextColor(LightBlue);
    WriteLn(' _______        _________  _______  ');
    WriteLn('|_   _\ \      / / ____\ \/ / ____| ');
    WriteLn('  | |  \ \ /\ / /|  _|  \  /|  _|   ');
    WriteLn('  | |   \ V  V / | |___ /  \| |___  ');
    WriteLn('  |_|    \_/\_/  |_____/_/\_\_____| ');
    WriteLn('  Single File TiddlyWiki executable ');
    WriteLn('  Version: '+_VERSION);
    WriteLn('------------------------------------');
    WriteLn('                                    ');
    ResetColors();
  end;

  //Extract data or abort
  procedure HandleExtractData(const Dir:string='');
  Var
    WikiName: string;
    NewName: string;
    OutDir: string;
  begin
    try
      WikiName:='';
      If Dir='' then
        OutDir := GetUnZipPath()
      else
        OutDir := Dir;
      Log('Extracting data from '''+ GetEXEFile() + '''');
      ExtractData(GetEXEFile(),OutDir);
      //Make sure unzipped wiki file has the same name as the twixie
      FindWikiFile(OutDir,WikiName);
      NewName := ConcatPaths([OutDir,GetEXEName() + '.html']);
      if WikiName <> NewName then
      begin
        Log('Renaming '''+WikiName+''' to '''+NewName+'''.');
        fileops.MoveFile(WikiName,NewName,True);
      end;
    except
      on E:Exception do
      begin
        logger.Error('Error extracting data: '+E.Message);
        ExitCode := 2;
        Raise;
      end;
    end;
  end;

  function ConvertWikiToExe(DataFile:string; var OutExeFN:string):boolean;
  Var
    OK:Boolean;
    Ext:string;
  begin
    Result:=False;
    Ext := GetOSEXEExt();
    If ConversionOutDir = '' then
      ConversionOutDir := ExtractFileDir(ExpandFileName(DataFile));

    OutExeFN := ConcatPaths([ConversionOutDir,FileNameNoExt(DataFile) + Ext]);
    Show('Generating ''' + OutExeFN + '''...');
    OK:=fileops.CopyFile(GetEXEFile(),OutExeFN);
    
    if (OK) then
    begin
      AppendFile(OutExeFN,DataFile);
      Log(''''+ ExtractFileName(DataFile)
              + ''' has been converted to a twixie named ''' + OutExeFN + '''.');
      Result:=True;
      ExitCode:=0;
    end
    else
    begin
      ExitCode:=6;
      Result:=False;
      logger.Error('Unable to create ''' + OutExeFN + '''');
    end;
  end;

  procedure RestartEXE(const StartBrowser:boolean=False);
  Var
    Out,Opts:String;
  begin

    Out := '';
    //Restart executable without opening the browser
    Opts := ' -s';

    If StartBrowser then
      Opts := '';

    //This causes the server to wait for all requests
    //to be finished
    If Assigned(Serv) then
      FreeAndNil(Serv);

    RunCmd(GetEXEFile(),Opts,Out,True);
    Restarting := True;
    //Ignore WaitForUser() if somebody calls
    //it after having run RestartEXE()
    DoNotWaitForUser:=True;
  end;

procedure ShowCongrats();
begin
show('   ____                            _         _       _   _                 _');
Show('  / ___|___  _ __   __ _ _ __ __ _| |_ _   _| | __ _| |_(_) ___  _ __  ___| |');
Show(' | |   / _ \|  _ \ / _  |  __/ _  | __| | | | |/ _  | __| |/ _ \|  _ \/ __| |');
Show(' | |__| (_) | | | | (_| | | | (_| | |_| |_| | | (_| | |_| | (_) | | | \__ \_|');
Show('  \____\___/|_| |_|\__, |_|  \__,_|\__|\__,_|_|\__,_|\__|_|\___/|_| |_|___(_)');
Show('                   |___/');
end;

function WaitForUser(txt:string='Press enter to exit...'):boolean;
begin
  {$ifdef windows} // So that the console doesnt close
  If not DoNotWaitForUser then
  begin
    WriteLn(txt);
    Readln;
  end;
  {$endif}

  Result:=True;
end;

procedure StopRunningServerAndRestart();
begin
  //This is used when server is already running
  //and the user ran the executable again
  //We stop the existing server and restart
  //so that we can serve the file
  if TTwexeHTTPServer.SendStopRequest() then
  begin
    Sleep(400); //wait a little bit to give it a chance to exit
    RestartEXE(True); //Open browser also
  end;
end;

procedure TwexeMain(
  const OrigExeFile: string;
  const FileArgs:array of string);
  Var
    WikiToConvert,Twixie,O: string;
    i: Integer;

begin
  LogVerbose := True;
  LogDebug := 0;
  Twixie := '';
  DoNotWaitForUser:=False;
  Restarting := False;

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
    try
      repeat
        WikiToConvert := ExpandFileName(FFileArgs[i]);
        Inc(i);
      until IsWikiFile(WikiToConvert) or (i=Length(FFileArgs));
      If IsWikiFile(WikiToConvert) and ConvertWikiToExe(WikiToConvert,Twixie) then
      begin
        O:='';
        ShowCongrats();
        Show(Format('Your new twixie: ''%S''',[Twixie]));

        //Open browser and server unless -s flag was specified
        If toOpenBrowser in TwexeOptions then
        begin
          WaitForUser('Press enter to run your new twixie...');
          RunCmd(Twixie,'',O,True);
        end;
      end
      else
        logger.Error('Sorry, no wiki file found in the arguments.');

      Exit;
    except
      WaitForUser();
    end;
  end;

  try
    //Run shadow and exit,
    //continue if we're not the shadow
    //We sleep a little bit to let previous executable
    //finish, if we had to the case of a restart because
    //the shadow file was in use
    Sleep(200);
    if not IAmShadow()and RunShadow(toOpenBrowser in TwexeOptions) then
    begin
      Log('Exiting, shadow created and running.');
      Exit;
    end;
  except
    on E:EAccessViolation do //Shadow was running already
    begin
      StopRunningServerAndRestart;
      Exit;
    end;
    on E:Exception do
    begin
      logger.Error('Unable to start shadow: ' + E.Message);
      StopRunningServerAndRestart;
      WaitForUser();
      Exit;
    end;
  end;

  //Leave quickly if we are restarting
  If Restarting then
    Exit;

  //Extract data bundled in executable
  try
    HandleExtractData();
    //Possible upgrade needs to be done AFTER
    //wiki has been extracted
    If NeedUpgrade() then
    begin
      RestartEXE();
      Exit;
    end;
  except
    WaitForUser();
    ExitCode:=1;
    Exit;
  end;

  try
    try
      //Start HTTP server and wait
      //a little to give a little time for the previous exe
      //to finish properly (this is needed in addition to the shadow sleep)
      Sleep(200);
      Serv:=StartServer();
      //FIXME: Cleanup temp files: shadow _exes in tmp, unzip dir
      If StoreReqFinished and not StopRequested then
        RestartEXE();
    finally
      If Assigned(Serv) then
         FreeAndNil(Serv);
    end;
  except on E:Exception do
    begin
      logger.Error('Server Exited abnormally: ' + E.Message);
      logger.Error('Aborting.');
      Exit;
    end;
  end;


end;

end.
