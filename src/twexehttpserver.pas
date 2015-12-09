unit twexehttpserver;

{$mode objfpc}{$H+}

{$define UseCThreads}

interface

uses
  { System/Free pascal units }
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  SysUtils,
  Classes,
  fphttpserver,
  fphttpclient,
  fpmimetypes,
  httpdefs,
  regexpr,
  lclintf,

  { Twexe units }
  exedata,logger,fileops,wikiops,textops;

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
    FURL: string;
    FSavingConfigUpdated:boolean;
    FOpenBrowser,FStopRequested: boolean;
    procedure SetBaseDir(const AValue: string);
    procedure HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandlePostReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

    const
      StopURI='/twexe/api/exitserver';

  protected
    procedure CheckMimeLoaded;
    property MimeLoaded: boolean read FMimeLoaded;
    function ParseUploadPlugin(const ARequest: TFPHTTPConnectionRequest): boolean;
    function FindFileForURI(const URI:string; var FN:String):Boolean;
    procedure StartServerSocket; override;
    procedure HandleOnStartListening;

  public
    class function ReadLastURL():string;
    class function SendStopRequest(const BaseURL:string=''):boolean;
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
    constructor Create;

    property BaseDir: string read FBaseDir write SetBaseDir;
    property MimeTypesFile: string read FMimeTypesFile write FMimeTypesFile;
    property SavingConfigUpdated: boolean read FSavingConfigUpdated write FSavingConfigUpdated;
    property URL: string read FURL write FURL;
    property OpenBrowser: boolean read FOpenBrowser write FOpenBrowser;
    property StopRequested: boolean read FStopRequested write FStopRequested;


  end;

  implementation

  { Utilities }
  procedure StoreLastURL(const URL:string);
  Var UFile : Text;
  begin
   Assign(UFile, ConcatPaths([GetStoragePath(),GetEXEName()+'_lasturl']));
   ReWrite(UFile);
   Writeln(UFile,URL);
   Close(UFile);
  end;


  { TTwexeHTTPServer }

  class function TTwexeHTTPServer.SendStopRequest(const BaseURL:string=''):boolean;
  var
    SrvURL,Resp:string;
  begin
  If BaseURL='' then
    SrvURL:=ReadLastURL()
  else
    SrvURL:=BaseURL;
  With TFPHttpClient.Create(Nil) do
    try
      Result:=False;
      Resp:=TrimRight(Get(SrvURL+StopURI));
      If Resp='OK' then
        Result:=True
      else
        logger.Error(SrvURL + ' responded ''' + Resp + ''' to stop request.');
    finally
      Free;
    end;
  end;

  class function TTwexeHTTPServer.ReadLastURL():string;
  Var
    UFile : Text;
    LURL: string;
  begin
   Result:='';
   try
     System.Assign(UFile, ConcatPaths([GetStoragePath(),GetEXEName()+'_lasturl']));
     Reset(UFile);
     Readln(UFile,LURL);
     Close(UFile);
     Result:=LURL;
   except
   end;
  end;

  constructor TTwexeHTTPServer.Create;
  begin
    StopRequested:=False;
    inherited;
  end;

  procedure TTwexeHttpServer.StartServerSocket;
  begin
    InetServer.Bind;
    InetServer.Listen;
    HandleOnStartListening;
    InetServer.StartAccepting;
  end;

  procedure TTwexeHTTPServer.HandleOnStartListening;
  begin
    If Self.Address = '' then
      Self.URL := 'http://127.0.0.1:' + IntToStr(Self.Port)
    else
      Self.URL := 'http://' + Self.Address + ':' + IntToStr(Self.Port);

    Show(LineEnding + 'Serving on ' + Self.URL);
    StoreLastURL(Self.URL);

    If (FOpenBrowser) then
      OpenURL(Self.URL);
  end;

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
    if (FileNameNoExt(CleanURI) = GetEXEName()) or (CleanURI='') then
    begin
      FN := GetUnzippedWikiFile();
      if FileExists(FN) then
      begin
        Result:=True;
        //Update saving config if it hasnt been done
        If not SavingConfigUpdated then
        begin
          //Make sure the saving tab in the control panel is pointing
          //to the twexe server
          try
            TmpWikiFN:=ExtractFilePath(FN)+'_orig_'+ExtractFileName(FN);
            fileops.MoveFile(FN,TmpWikiFN);
            //FIXME: This should update saving config only if twexe is found
            SavingConfigUpdated:=EnsureTwexeSavingConfig(TmpWikiFN,FN,Port);
            DeleteFile(TmpWikiFN);
          except on E:Exception do
            logger.Error('Unable to update saving configuration for '''+ FN + ''': '+ E.Message);
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
    OK := MoveFile(PostedFile,FWikiFile,True);
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
      OK := fileops.CopyFile(ARequest.Files[0].LocalFileName, FWikiFile, True);
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
    FN:='';
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
        FreeAndNil(F);
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
    Log('Method: ' + ARequest.Method);
    Log('Request URI: ' + ARequest.URI);
    If Arequest.URI = StopURI then
    begin
      AResponse.Code:=200;
      AResponse.Content:='OK';
      AResponse.ContentType:='text/plain';
      AResponse.SendContent;
      StopRequested:=True;
      Self.Active:=False;
      exit;
    end;
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

end.

