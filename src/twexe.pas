program twexe;

{$mode objfpc}{$H+}
{$define UseCThreads}

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
  
  exedata,logger,fileops,wikiops,textops,version
  {$ifdef unix}
  ,unixlib
  {$endif}
  {$ifdef windows}
  ,windowslib
  {$endif};

type

  { TTwexeHTTPServer }
  TTwexeHTTPServer = class(TFPHTTPServer)
  private
    FBaseDir: string;
    FCount: integer;
    FMimeLoaded: boolean;
    FMimeTypesFile: string;
    FWikiFile: string;
    FBackupDir: string;
    FUser: string;
    FUserFile: string;
    FPassword: string;
    FUploadDir: string;
    FSavingConfigUpdated:boolean;
    procedure SetBaseDir(const AValue: string);
    procedure HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandlePostReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

  protected
    procedure CheckMimeLoaded;
    property MimeLoaded: boolean read FMimeLoaded;
    function ParseUploadPlugin(const ARequest: TFPHTTPConnectionRequest): boolean;
    function FindFileForURI(const URI:string; var FN:String):Boolean;

  public
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
    property BaseDir: string read FBaseDir write SetBaseDir;
    property MimeTypesFile: string read FMimeTypesFile write FMimeTypesFile;
    property SavingConfigUpated: boolean read FSavingConfigUpdated write FSavingConfigUpdated;
  end;

  { TTwexeHTTPServer }
  //
  // Base directory for HTTP Server
  //
  procedure TTwexeHTTPServer.SetBaseDir(const AValue: string);
  begin
    if FBaseDir = AValue then
      exit;
    FBaseDir := AValue;
    if (FBaseDir <> '') then
      FBaseDir := IncludeTrailingPathDelimiter(FBaseDir);
  end;

  //Make sure mime data is loaded
  procedure TTwexeHTTPServer.CheckMimeLoaded;
  begin
    if (not MimeLoaded) and (MimeTypesFile <> '') then
    begin
      MimeTypes.LoadFromFile(MimeTypesFile);
      FMimeLoaded := True;
    end;
  end;

  // Find file for the specified URI, by first looking in the 
  // _zip dir and then in the server base directory.
  function TTwexeHTTPServer.FindFileForURI(const URI:string; var FN:String):Boolean;
  Var 
    CleanURI:String;
    TmpWikiFN: String;
  begin
    CleanURI := URI;
    if (length(URI) > 0) and (URI[1] = '/') then
      Delete(CleanURI, 1, 1);
    DoDirSeparators(CleanURI);

    //If the file is found in the zip directory return that
    // otherwise see if the file is in the server Base directory
    if FileNameNoExt(CleanURI) = GetEXEName() then
    begin
      FN := GetUnzippedWikiFile();
      WriteLn('FN=',FN);
      if FileExists(FN) then
      begin
        Result:=True;
        //Update saving config if it hasnt been done
        If not SavingConfigUpated then
        begin
          //Make sure the saving tab in the control panel is pointing 
          //to the twexe server
          try
            TmpWikiFN:=ExtractFilePath(FN)+'_orig_'+ExtractFileName(FN);
            MoveFile(FN,TmpWikiFN);
            SavingConfigUpated:=EnsureTwexeSavingConfig(TmpWikiFN,FN,Port);
            DeleteFile(TmpWikiFN);
          except on E:Exception do
            Error('Unable to update saving configuration for '''+ FN + ''': '+ E.Message);
          end;
        end;
          
        Exit; //It is the wiki file
      end;
    end;

    begin
      //Try unzip storage area
      FN := GetUnZipPath() + CleanURI;
      if FileExists(FN) then
      begin
        Result := True;
        Exit; // It is in Zip directory
      end;

      //Try BaseDir of server
      FN := BaseDir + CleanURI;
      if FileExists(FN) then
      begin
        Result := True;
        Exit; // It is in base dir of Server
      end
      else
        Result := False; // Not found anywhere
    end;
  end;

  // Parse upload dir, backup dir, etc from post request
  function TTwexeHTTPServer.ParseUploadPlugin(const ARequest: TFPHTTPConnectionRequest): boolean;
  var
    SL: TStrings;
    RegExp: string;
    Value: String;
  begin
    Value := ARequest.ContentFields.Values['UploadPlugin'];
    FUserFile := ARequest.ContentFields.Values['userfile'];
    RegExp := '(?s)backupDir=(.*?);user=(.*?);password=(.*?);uploaddir=(.*?);;';
    SL := TStringList.Create;
    if MatchRegex(RegExp, Value, SL) and (SL.Count = 4) then
    begin
      FBackupDir := SL[0];
      FUser := SL[1];
      FPassword := SL[2];
      FUploadDir := SL[3];
      Result := True;
    end
    else
    begin
      Result := False;
    end;
    FreeAndNil(SL);
  end;

  procedure TTwexeHTTPServer.HandlePostReq(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
  var
    OK: boolean;
    BakFile: string;
    ExeName: string;
    PostedFile: string;
  begin
    ExeName := GetEXEName();
    OK := ParseUploadPlugin(ARequest);
    PostedFile := ARequest.Files[0].LocalFileName; 
    FWikiFile:=ConcatPaths([GetStoragePath(),'_pst',ExeName+'.html']);
    OK := MoveFile(PostedFile,FWikiFile);
    if OK then
    begin
      OK := False;
      //Append received wiki to executable
      AppendFile(GetEXEFile(),FWikiFile);

      //Make backup in specified Backup Dir
      BakFile := ConcatPaths([FBackupDir, ExeName + '.html']);
      OK := MakeBackup(FWikiFile, BakFile);

      { **************************************************************************
        **  This code may be used later to move file to upload dir, but for now we 
        ** don't need it 
      //Move file to Uploaddir
      UserFName := FileNameNoExt(FUserFile);
      if ( UserFName <> EXEName ) then
         Log('User file "'+UserFName+'" is different from executable name, ignoring.');
      FWikiFile := ConcatPaths([FUploadDir, ExeName])+'.html';
      OK := CopyFile(ARequest.Files[0].LocalFileName, FWikiFile, True);
       ***************************************************************************}
    end;
    
    //Send 200 OK to the browser if we were able to save the file
    if OK then
      AResponse.Code := 200
    else
      AResponse.Code := 404;

    //Send response
    AResponse.SendContent;
    
    //Stop server to executable can restart
    Self.Active := False;
  end;

  procedure TTwexeHTTPServer.HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
  var
    F: TFileStream;
    FN: string;

  begin
    if FindFileForURI(ARequest.Url,FN) then
    begin
      F := TFileStream.Create(FN, fmOpenRead);
      try
        CheckMimeLoaded;
        AResponse.ContentType := MimeTypes.GetMimeType(ExtractFileExt(FN));
        Log(LineEnding + 'Serving file: "' + Fn + '". Reported Mime type: ' +
          AResponse.ContentType);
        //AResponse.ContentEncoding:='gzip';
        AResponse.ContentLength := F.Size;
        AResponse.ContentStream := F;
        AResponse.SendContent;
        AResponse.ContentStream := nil;
      finally
        F.Free;
      end;
    end
    else
    begin
      AResponse.Code := 404;
      AResponse.SendContent;
    end;
    Inc(FCount);
  end;


  procedure TTwexeHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
  begin
    Log(LineEnding + 'Method: ' + ARequest.Method);
    Log('Request URI: ' + ARequest.URI);
    if ARequest.Method = 'GET' then
    begin
      HandleGetReq(ARequest, AResponse);
      Exit;
    end;
    if (ARequest.Method = 'POST') and (ARequest.URI = '/store') then
    begin
      HandlePostReq(ARequest, AResponse);
      Exit;
    end;
  end;

  function TryPort(Port: word): TTwexeHTTPServer;
  var
    Serv: TTwexeHTTPServer;
  begin
    Serv := TTwexeHTTPServer.Create(nil);
    try
      Serv.BaseDir := GetServerDocPath();
      {$ifdef unix}
      Serv.MimeTypesFile := '/etc/mime.types';
      {$endif}
      Serv.Threaded := False;
      Serv.Port := Port;
      Msg('Serving on http://127.0.0.1:' + IntToStr(Serv.Port),False);
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
  begin
    try
      ExtractData(GetEXEFile());
    except
      on E:Exception do
      begin
        Error('Error extracting data: '+E.Message);
        Error('Aborting.');
        Halt(2);
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
    OutExeFN := FileNameNoExt(DataFile) + Ext;
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
    RunCmd(GetEXEFile(),Out,True);
  end;

begin
  LogVerbose := True;
  LogDebug := 0;

  //Print version info and header
  If not IAmShadow() then
    PrintHeader();
  
  //If a parameter is specified convert the file to an executable
  //in the same directory
  if (ParamStr(1) <> '-z')  and (ParamStr(1) <> '') then
  begin
    ConvertWikiToExe(ParamStr(1));
    Exit;
  end;

  //Run shadow and exit,
  //continue if we're not the shadow
  if RunShadow() then
  begin
     Log('Exiting, shadow created and running.');
     Exit;
  end;

  //Extract data bundled in executable
  HandleExtractData();

  //Start HTTP server
  StartServer();

  //Server is single-threaded, but it stops listening after a
  // Post request, so we restart the EXE to that everything is
  // reloaded
  //FIXME: Cleanup temp files: shadow _exes in tmp, unzip dir
  RestartEXE();
end.
