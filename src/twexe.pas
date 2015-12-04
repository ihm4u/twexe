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
  
  exedata,logger,fileops,version
  {$ifdef unix}
  ,unixlib
  {$endif}
  {$ifdef windows}
  ,windowslib
  {$endif};

type

  { TTestHTTPServer }
  TTestHTTPServer = class(TFPHTTPServer)
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
    procedure SetBaseDir(const AValue: string);
    procedure HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandlePostReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

  protected
    procedure CheckMimeLoaded;
    property MimeLoaded: boolean read FMimeLoaded;
    function ParseUploadPlugin(const ARequest: TFPHTTPConnectionRequest): boolean;

  public
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
    property BaseDir: string read FBaseDir write SetBaseDir;
    property MimeTypesFile: string read FMimeTypesFile write FMimeTypesFile;
  end;

var
  Serv: TTestHTTPServer;

  { TTestHTTPServer }
  //
  // Base directory for HTTP Server
  //
  procedure TTestHTTPServer.SetBaseDir(const AValue: string);
  begin
    if FBaseDir = AValue then
      exit;
    FBaseDir := AValue;
    if (FBaseDir <> '') then
      FBaseDir := IncludeTrailingPathDelimiter(FBaseDir);
  end;

  //Make sure mime data is loaded
  procedure TTestHTTPServer.CheckMimeLoaded;
  begin
    if (not MimeLoaded) and (MimeTypesFile <> '') then
    begin
      MimeTypes.LoadFromFile(MimeTypesFile);
      FMimeLoaded := True;
    end;
  end;

  //Return group matches from a regex
  function MatchRegex(const RegExpr: string; const Text: string;
  var Matches: TStrings): boolean;
  var
    Regex: TRegExpr;
    i: integer;
  begin
    Regex := TRegExpr.Create;
    try
      with Regex do
      begin
        Expression := RegExpr;
        //We have a match
        if Exec(Text) then
        begin
          //Add all matches
          repeat
            begin
              for i := 1 to SubExprMatchCount do
                Matches.Add(Match[i]);
            end
          until not ExecNext;
          Result := True;
        end
        else
          Result := False;
      end;
    finally
      if Assigned(Regex) then
        FreeAndNil(Regex);
    end;
  end;

  // Parse upload dir, backup dir, etc from post request
  function TTestHTTPServer.ParseUploadPlugin(const ARequest: TFPHTTPConnectionRequest): boolean;
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

  procedure TTestHTTPServer.HandlePostReq(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
  var
    OK: boolean;
    BakFile: string;
    ExeName: string;
    UserFName: string;
  begin
    OK := False;
    if ParseUploadPlugin(ARequest) then
    begin
      //Move file to Uploaddir
      UserFName := FileNameNoExt(FUserFile);
      ExeName := GetEXEName();
      if ( UserFName <> EXEName ) then
         Log('User file "'+UserFName+'" is different from executable name, ignoring.');
      FWikiFile := ConcatPaths([FUploadDir, ExeName])+'.html';
      OK := CopyFile(ARequest.Files[0].LocalFileName, FWikiFile, True);
    end;
    
    //Send 200 OK to the browser if we were able to save the file
    if OK then
      AResponse.Code := 200
    else
      AResponse.Code := 404;

    AResponse.SendContent;

    //Make backup in specified Backup Dir
    BakFile := ConcatPaths([FBackupDir, ExeName + '.html']);
    MakeBackup(FWikiFile, BakFile);

    //FIXME: Append file to executable
  end;

  procedure TTestHTTPServer.HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
  var
    F: TFileStream;
    FN: string;

  begin
    FN := ARequest.Url;
    if (length(FN) > 0) and (FN[1] = '/') then
      Delete(FN, 1, 1);
    DoDirSeparators(FN);

    //If the request is for the wiki, serve from the unzipped file
    //Otherwise server from the server base directory
    if FileNameNoExt(FN) = GetEXEName() then
       FN := GetUnZipPath() + ChangeFileExt(FN,'.html')
    else
       FN := BaseDir + FN;
 
    //Now serve the file to the client
    if FileExists(FN) then
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


  procedure TTestHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
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

  function TryPort(Port: word): TTestHTTPServer;
  var
    Serv: TTestHTTPServer;
  begin
    Serv := TTestHTTPServer.Create(nil);
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

  function StartServer(): TTestHTTPServer;
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
      ExtractData();
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
              + ''' has been converted to ''' + OutExeFN + '''.');
      SetExecutePermission(OutExeFN);
    end
      else
      Error('Unable to create ''' + OutExeFN + '''');
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
  Serv := StartServer();

  //Nothing runs here because server is single-threaded for now
end.
