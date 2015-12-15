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
  unixlib,
  {$ENDIF}
  {$ifdef windows}
  windowslib,
  {$endif}
  SysUtils,
  Classes,
  fphttpserver,
  fphttpclient,
  ssockets,
  httpdefs,
  regexpr,
  lclintf,

  { Twexe units }
  exedata, logger, fileops, wikiops, textops;

type

  { TTwexeHTTPServer }
  TTwexeHTTPServer = class(TFPHTTPServer)
  private
    FBaseDir: string;
    FCount: integer;
    FWikiFile: string;
    FBackupDir: string;
    FUser: string;
    FUserFile: string;
    FPassword: string;
    FUploadDir: string;
    FURL: string;
    FSavingConfigUpdated: boolean;
    FOpenBrowser, FStopRequested, FStoreRequestDone: boolean;

    procedure SetBaseDir(const AValue: string);
    procedure HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleStoreReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandleCriticalReqs(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function  HandleAPIReqs(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse):boolean;

  const
    StopURI = '/twexe/api/exitserver';
    RunProcURI = '/twexe/api/runproc';
    StoreURI = '/store';
    ProcTimeoutDef = 13; //Default timeout in seconds for external runproc processes

  protected
    function ParseUploadPlugin(const ARequest: TFPHTTPConnectionRequest): boolean;
    function FindFileForURI(const URI: string; var FN: string): boolean;
    procedure StartServerSocket; override;
    procedure HandleOnStartListening;
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;

  public
    class function ReadLastURL(): string;
    class function SendStopRequest(const BaseURL: string = ''): boolean;
    constructor Create; //no override needed b/c there is no ctor in parent;

    property BaseDir: string read FBaseDir write SetBaseDir;
    property SavingConfigUpdated: boolean read FSavingConfigUpdated
      write FSavingConfigUpdated;
    property URL: string read FURL write FURL;
    property OpenBrowser: boolean read FOpenBrowser write FOpenBrowser;
    property StopRequested: boolean read FStopRequested write FStopRequested;
    property StoreRequestDone: boolean read FStoreRequestDone write FStoreRequestDone;


  end;

implementation

{ Utilities }

procedure StoreLastURL(const URL: string);
var
  UFile: Text;
begin
  Assign(UFile, ConcatPaths([GetStoragePath(), GetEXEName() + '_lasturl']));
  ReWrite(UFile);
  Writeln(UFile, URL);
  Close(UFile);
end;


{ TTwexeHTTPServer }

class function TTwexeHTTPServer.SendStopRequest(const BaseURL: string = ''): boolean;
var
  SrvURL, Resp: string;
begin
  if BaseURL = '' then
    SrvURL := ReadLastURL()
  else
    SrvURL := BaseURL;
  with TFPHttpClient.Create(nil) do
    try
      Result := False;
      Resp := TrimRight(Get(SrvURL + StopURI));
      if Resp = 'OK' then
        Result := True
      else
        logger.Error(SrvURL + ' responded ''' + Resp + ''' to stop request.');
    finally
      Free;
    end;
end;

class function TTwexeHTTPServer.ReadLastURL(): string;
var
  UFile: Text;
  LURL: string;
begin
  Result := '';
  try
    System.Assign(UFile, ConcatPaths([GetStoragePath(), GetEXEName() + '_lasturl']));
    Reset(UFile);
    Readln(UFile, LURL);
    Close(UFile);
    Result := LURL;
  except
  end;
end;

constructor TTwexeHTTPServer.Create;
begin
  StopRequested := False;
  StoreRequestDone := False;
  Address:='127.0.0.1';
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
  if Self.Address = '' then
    Self.URL := 'http://127.0.0.1:' + IntToStr(Self.Port)
  else
    Self.URL := 'http://' + Self.Address + ':' + IntToStr(Self.Port);
  //This is the first line shown after the "Trying port" line
  Show(LineEnding + '''' + GetEXEName() + '''' + ' running on ' + Self.URL);
  StoreLastURL(Self.URL);

  if (FOpenBrowser) then
  begin
    OpenURL(Self.URL);
    Show('Starting browser please wait...');
  end;
end;


// Base directory for HTTP Server

procedure TTwexeHTTPServer.SetBaseDir(const AValue: string);
begin
  if FBaseDir = AValue then
    exit;
  FBaseDir := AValue;
  if (FBaseDir <> '') then
    FBaseDir := IncludeTrailingPathDelimiter(FBaseDir);
end;

// Find file for the specified URI, by first looking in the
// _zip dir and then in the server base directory.
function TTwexeHTTPServer.FindFileForURI(const URI: string; var FN: string): boolean;
var
  CleanURI: string;
  TmpWikiFN: string;
begin
  CleanURI := URI;
  if (length(URI) > 0) and (URI[1] = '/') then
    Delete(CleanURI, 1, 1);
  DoDirSeparators(CleanURI);

  //If the file is found in the zip directory return that
  // otherwise see if the file is in the server Base directory
  if (FileNameNoExt(CleanURI) = GetEXEName()) or (CleanURI = '') then
  begin
    FN := GetUnzippedWikiFile();
    if FileExists(FN) then
    begin
      Result := True;
      //Update saving config if it hasnt been done
      if not SavingConfigUpdated then
      begin
        //Make sure the saving tab in the control panel is pointing
        //to the twexe server
        try
          TmpWikiFN := ExtractFilePath(FN) + '_orig_' + ExtractFileName(FN);
          fileops.MoveFile(FN, TmpWikiFN);
          SavingConfigUpdated := UpdateWikiSavingConfig(TmpWikiFN, FN, Port,woIfTwexe);
          DeleteFile(TmpWikiFN);
        except
          on E: Exception do
            logger.Error('Unable to update saving configuration for ''' +
              FN + ''': ' + E.Message);
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
function TTwexeHTTPServer.ParseUploadPlugin(
  const ARequest: TFPHTTPConnectionRequest): boolean;
var
  SL: TStrings;
  RegExp: string;
  Value: string;
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

procedure TTwexeHTTPServer.HandleStoreReq(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  OK: boolean;
  BakFile: string;
  ExeName: string;
  PostedFile: string;
  ThisMoment: TDateTime;
begin
  try
    StoreRequestDone := False;
    ExeName := GetEXEName();
    OK := ParseUploadPlugin(ARequest);
    PostedFile := ARequest.Files[0].LocalFileName;
    FWikiFile := ConcatPaths([GetStoragePath(), '_pst', ExeName + '.html']);
    OK := MoveFile(PostedFile, FWikiFile, True);

    if OK then
    begin
      OK := False;
      //Make backup in specified Backup Dir
      //If not absolute, backup dir is in reference
      //to GetEXEPath() - the path of the twixie file.
      if Length(FBackupDir) = 0 then
        FBackupDir := GetEXEPath()
      else if FBackupDir[Length(FBackupDir)] <> DirectorySeparator then
        FBackupDir := ConcatPaths([GetEXEPath(), FBackupDir]);

      BakFile := ConcatPaths([FBackupDir, ExeName + GetOSEXEExt()]);
      OK := MakeBackup(GetEXEFile(), BakFile);

      //Append received wiki to executable
      AppendFile(GetEXEFile(), FWikiFile);

      //Write to HTML file in UploadDir if UploadDir is different from '.'
      //which is the default
      If FUploadDir <> '.' then
      begin
          OK := fileops.CopyFile(FWikiFile,
            ConcatPaths([FUploadDir,ExtractFileName(FUserFile)]));
      end;
    end;

    //Send 200 OK to the browser if we were able to save the file
    if OK then
    begin
      ThisMoment := Now;
      Show('''' + GetEXEName() + ''' saved on ' +
        FormatDateTime('dddd "at" hh:nn:ss.', ThisMoment));
      StoreRequestDone := True;
      AResponse.Code := 200;
    end
    else
    begin
      logger.Error('Unable to save wiki!!');
      AResponse.Code := 404;
    end;

    //Send response
    AResponse.SendContent;

    //Stop server to executable can restart
    Self.Active := False;

  except
    on E: Exception do
    begin
      Error('Unable to save wiki: ' + E.toString);
      raise;
    end;
  end;
end;

procedure TTwexeHTTPServer.HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  F: TFileStream;
  FN: string;

begin
  FN := '';
  if FindFileForURI(ARequest.Url, FN) then
  begin
    F := TFileStream.Create(FN, fmOpenRead);
    try
      AResponse.ContentType := GetMimeType(ExtractFileExt(FN));
      Log('Serving file: ''' + Fn + '''. Type: ''' + AResponse.ContentType + '''.');
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

procedure TTwexeHTTPServer.HandleCriticalReqs(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  {These are critical requests that may change the state of the server }
  //Stop server request
  if Arequest.URI = StopURI then
  begin
    AResponse.Code := 200;
    AResponse.Content := 'OK';
    AResponse.ContentType := 'text/plain';
    AResponse.SendContent;
    StopRequested := True;
    Show('Stopping ''' + GetEXEName() + ''' on ' + Self.URL);
    Self.Active := False;
    exit;
  end;

  //Store Wiki request
  if (ARequest.Method = 'POST') and (ARequest.URI = StoreURI) then
  begin
    HandleStoreReq(ARequest, AResponse);
    Exit;
  end;
end;

function TTwexeHTTPServer.HandleAPIReqs(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse):boolean;
var
  OutStr,Cmd,Exe,Args,Input,OutputF,ApiCall:string;
  OutS:TFileStream;
  Timeout:Integer;
begin
  Result:=False;
  ApiCall:=LeftStr(ARequest.URI, Length(RunProcURI));
  if ApiCall = RunProcURI then
  begin
    Result:=True;
    //Get the executable, arguments, input and timeout from the request
    if (ARequest.Method = 'GET') then
    begin
      Exe := Trim(ARequest.QueryFields.Values['cmd']);
      Args := Trim(ARequest.QueryFields.Values['args']);
      Input := Trim(ARequest.QueryFields.Values['input']);
      OutputF := Trim(ARequest.QueryFields.Values['output']);  //read this file to get output from executable
      Timeout := StrToIntDef(Trim(ARequest.QueryFields.Values['timeout']),ProcTimeoutDef);
    end
    else
    if (ARequest.Method = 'POST') then
    begin
      Exe := ARequest.ContentFields.Values['cmd'];
      Args := ARequest.ContentFields.Values['args'];
      Input := ARequest.ContentFields.Values['input'];
      OutputF := Trim(ARequest.ContentFields.Values['output']);
      Timeout := StrToIntDef(Trim(ARequest.ContentFields.Values['timeout']),ProcTimeoutDef);
    end;

    Cmd := Exe + ' ' + Args;

    Log(Format('Running ''%S'' with %D bytes of input; timeout of %D secs.',
      [TrimRight(Cmd),Length(input),Timeout]));
    If Exe = '' then
    begin //Cmd field empty
      AResponse.Code:=404;
      AResponse.SendContent;
    end
    else //Cmd field is not empty
    begin
      OutStr:='';
      try
        RunCmd(Exe,Args,OutStr,False,Input,Timeout);
        //Read output from file if specified in 'output' field
        If Length(OutputF) >0 then //Read output file
        begin
          OutS:=TFileStream.Create(OutputF,fmOpenRead);
          try
            SetLength(OutStr,OutS.Size);
            OutS.Read(OutStr[1],OutS.Size);
            AResponse.ContentType:=GetMimeType(ExtractFileExt(OutputF));
          finally
            If Assigned(OutS) then
              FreeAndNil(OutS);
          end;
        end;
        AResponse.Code := 200;
      except on E:Exception do
        begin
          Log('External process exception: ' + E.toString);
          AResponse.Code := 500;
        end;
      end;

      Log(Format('''%S'' output: ''%S''',
        [FileNameNoExt(Exe), OutStr]));
      AResponse.Content := OutStr;
      AResponse.SendContent;
    end;
  end;
end;

procedure TTwexeHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  Log('Method: ''' + ARequest.Method + ''' URI: ''' + ARequest.URI + '''');

  //Handle store and stop server requests
  HandleCriticalReqs(ARequest,AResponse);

  //Handle API requests, like runproc
  if HandleAPIReqs(ARequest,AResponse) then
    Exit;

  //Handle other requests
  if ARequest.Method = 'GET' then
  begin
    HandleGetReq(ARequest, AResponse);
    Exit;
  end;
end;

end.
