program tw2exe;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  SysUtils,
  Classes,
  fphttpserver,
  fpmimetypes,
  httpdefs,
  regexpr,
  
  exedata,logger,fileops,version;

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
    i: word;
    OK: boolean;
    BakFile: string;
  begin
    {WriteLn('ARequest.FieldCount: ', ARequest.FieldCount);
    WriteLn('ARequest.ContentFields.Count: ', ARequest.ContentFields.Count);
    //WriteLn('ARequest.Content: ', ARequest.Content);
    for i := 0 to ARequest.FieldCount - 1 do
    begin
      writeln('Received Field: ' + ARequest.FieldNames[i], ',', ARequest.FieldValues[i]);
    end;

    for i := 0 to ARequest.ContentFields.Count - 1 do
    begin
      writeln('Received Content Field: ' + ARequest.ContentFields[i]);
    end;
    for i := 0 to ARequest.Files.Count - 1 do
    begin
      writeln('Received Filename: ' + ARequest.Files[i].LocalFileName);
    end;

    WriteLn('Value of UploadPlugin -->', ARequest.ContentFields.Values['UploadPlugin']);
    WriteLn('Value of userfile     -->', ARequest.ContentFields.Values['userfile']);}
    OK := False;
    if ParseUploadPlugin(ARequest) then
    begin
      //Move file to Uploaddir
      FWikiFile := ConcatPaths([FUploadDir, FUserFile]);
      OK := CopyFile(ARequest.Files[0].LocalFileName, FWikiFile, True);
    end;
    if OK then
      AResponse.Code := 200
    else
      AResponse.Code := 404;

    AResponse.SendContent;

    //Make backup
    BakFile := ConcatPaths([FBackupDir, FUserFile]);
    MakeBackup(FWikiFile, BakFile);
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
    FN := BaseDir + FN;
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
    if ARequest.Method = 'POST' then
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
      //Serv.BaseDir := ExtractFilePath(ParamStr(0));
      Serv.BaseDir := '.' + DirectorySeparator;
      {$ifdef unix}
      Serv.MimeTypesFile := '/etc/mime.types';
{$endif}
      Serv.Threaded := False;
      Serv.Port := Port;
      Write('Serving on 127.0.0.1:', Serv.Port);
      Serv.Active := True;
    except
      On Exception do
      begin
        WriteLn('...Busy');
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
    WriteLn('▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁');
    WriteLn('⚜ Single File TiddlyWiki executable ⚜');
    WriteLn('▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔');
    WriteLn('Version: ',_VERSION);
  end;

begin
  LogVerbose := True;
  LogDebug := 0;
  PrintHeader();
  try
    ExtractData();
  except
    on Exception do
    begin
      Writeln('Aborting.');
      Halt(2);
    end;
  end;

  Serv := StartServer();
  //Nothing runs here because server is single-threaded for now
end.
