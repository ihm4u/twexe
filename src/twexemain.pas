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
  TTwexeOption = (toOpenBrowser,toAllowRemoteClients,toProgramaticRun);
  TTwexeOptions = record
    Flags:set of TTwexeOption;
    ConversionOutDir: string;
    ServerPort: Integer;
    ServerBindAddress: string;
  end;

var
  Opts : TTwexeOptions =
    (Flags: [];
     ConversionOutDir: '';
     ServerPort: 0;
     ServerBindAddress: '');

// These are used from twexe.lpr
procedure TwexeMain(
  const OrigExeFile: string;
  const FileArgs:array of string);
procedure PrintHeader();
procedure HandleExtractData(const Dir:string='');
function WaitForUser(txt:string='Press enter to exit...'):boolean;
procedure StopRunningServer();

//used from windowslib or unixlib
procedure CleanupOnExit();

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
    Serv := TTwexeHTTPServer.Create(Opts.ServerBindAddress);
    try
      //Server should open browser as soon as it is listening
      Serv.OpenBrowser := (toOpenBrowser in Opts.Flags);
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

  function StartServer(const Port:Word=0): TTwexeHTTPServer;
  var
    FPort: word;
    TryAgain : boolean=False;
  begin
    If Port = 0 then  //Find a free port between 8080 and 8095
    begin
      FPort := 8080;
      TryAgain := True;
      repeat
        Result := TryPort(FPort,TryAgain,StoreReqFinished);

        if Result = nil then
          Inc(FPort)
      until (Result <> nil)
        or (FPort > 8095)
        or not TryAgain
        or StopRequested
        or StoreReqFinished;
    end
    else //Port specified by user
    begin
      Result := TryPort(Port,TryAgain,StoreReqFinished);
      //Try with the same port:
      //Wine takes a while to close sockets
      //since they remain in TIME_WAIT state for a time
      If Result = Nil then
      begin
        repeat
          Log('Trying again in about 20 seconds...');
          Sleep(22333); //To honor the Holy Trinity, and the union with Humanity
          Result := TryPort(Port,TryAgain,StoreReqFinished);
        until StopRequested or StoreReqFinished or not TryAgain;
      end;
    end;
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
        logger.Error('Error extracting data: '+E.toString());
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
    If Opts.ConversionOutDir = '' then
      Opts.ConversionOutDir := ExtractFileDir(ExpandFileName(DataFile));

    OutExeFN := ConcatPaths([Opts.ConversionOutDir,FileNameNoExt(DataFile) + Ext]);
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

  function GetInheritableOpts(SkipBrowserOpt:boolean=False;
                              SkipPortOpt   :boolean=False):string;
  Var
    ShOpts:string='';
  begin
    //Propagate flag to not open the browser
    If not SkipBrowserOpt and  (toOpenBrowser in Opts.Flags) then
      ShOpts:=''
    else
      ShOpts:=' -s';

    //Propagate http port flag
    If not SkipPortOpt and (Opts.ServerPort <> 0) then
      ShOpts:= ShOpts + ' -t ' + IntToStr(Opts.ServerPort);

    //Propagate bind address flag
    If toAllowRemoteClients in Opts.Flags then
      ShOpts:= ShOpts + ' -r ' + Opts.ServerBindAddress;

    Result:=ShOpts;
  end;

  procedure RestartEXE(const StartBrowser:boolean=False; const Port:word=0);
  Var
    Out,Opts:String;
  begin

    Out := '';

    If StartBrowser then
      Opts := ''
    else  //Restart executable without opening the browser
      Opts := ' -s';

    //We need to add the port on restart to assure same port
    //after store request
    If Port <> 0 then
      Opts := Opts + ' -t ' + IntToStr(Port);

    //Add all other propagated options except the browser and port options
    Opts := Opts + GetInheritableOpts(True,True);

    //This causes the server to wait for all requests
    //to be finished
    If Assigned(Serv) then
      FreeAndNil(Serv);

    LogFmt('Restarting ''%s'' ''%s''',[GetEXEFile(),Opts]);
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
  If not DoNotWaitForUser and
     not (toProgramaticRun in Opts.Flags) then
  begin
    WriteLn(txt);
    Readln;
  end;
  {$endif}

  Result:=True;
end;

procedure StopRunningServer();
begin
  TTwexeHTTPServer.SendStopRequest();
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
  //FIXME: LogVerbose needs to be behind an interface
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
        If toOpenBrowser in Opts.Flags then
        begin
          WaitForUser('Press enter to run your new twixie...');
          RunCmd(Twixie,'',O,True);
        end;
      end
      else
        logger.Error('Sorry, no wiki file found in the arguments.');
      Exit;
    finally
      CleanupOnExit();
    end;
  end;

  try
    //Run shadow and exit,
    //continue if we're not the shadow
    //We sleep a little bit to let previous executable
    //finish, if we had to the case of a restart because
    //the shadow file was in use
    Sleep(200);
    if not IAmShadow()and RunShadow(GetInheritableOpts()) then
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

  InstallExitHandler();
  //Extract data bundled in executable
  try
    HandleExtractData();
    //Possible upgrade needs to be done AFTER
    //wiki has been extracted
    If NeedUpgrade() then
    begin
      //Make sure browser is opened
      RestartEXE(True);
      Exit;
    end;
  except
    WaitForUser();
    ExitCode:=1;
    CleanupOnExit();
    Exit;
  end;

  try
    try
      //Start HTTP server and wait
      //a little to give a little time for the previous exe
      //to finish properly (this is needed in addition to the shadow sleep)
      Sleep(200);
      Serv:=StartServer(Opts.ServerPort);

      If StoreReqFinished and not StopRequested and Assigned(Serv) then
        RestartEXE(False,Serv.Port);
    finally
      If Assigned(Serv) then
         FreeAndNil(Serv);
      CleanupOnExit();
    end;
  except on E:Exception do
    begin
      logger.Error('Server Exited abnormally: ' + E.Message);
      logger.Error('Aborting.');
      Exit;
    end;
  end;
end;

procedure CleanupOnExit();
begin
    Log('Cleaning up temporary files.');
    DeleteDirectory(GetStoragePath());
end;

initialization
  LogVerbose:=True;
end.
