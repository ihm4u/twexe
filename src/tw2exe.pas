program tw2exe;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  SysUtils,
  Classes,
  fphttpserver,
  fpmimetypes,
  exedata,
  httpdefs,
  regexpr;

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
    function ParseUploadPlugin(Value: string): boolean;

  public
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
    property BaseDir: string read FBaseDir write SetBaseDir;
    property MimeTypesFile: string read FMimeTypesFile write FMimeTypesFile;
  end;

var
  Serv: TTestHTTPServer;

  { TTestHTTPServer }
  procedure Log(const Msg: string);
  begin
    if (Verbose) then
      WriteLn(Msg);
  end;

  procedure TTestHTTPServer.SetBaseDir(const AValue: string);
  begin
    if FBaseDir = AValue then
      exit;
    FBaseDir := AValue;
    if (FBaseDir <> '') then
      FBaseDir := IncludeTrailingPathDelimiter(FBaseDir);
  end;

  procedure TTestHTTPServer.CheckMimeLoaded;
  begin
    if (not MimeLoaded) and (MimeTypesFile <> '') then
    begin
      MimeTypes.LoadFromFile(MimeTypesFile);
      FMimeLoaded := True;
    end;
  end;

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
    //Exit if Dirs is not ended with the directory separator
    if Dirs[Length(Dirs)] <> DirectorySeparator then
      Exit;

    try
      //Get each one of the Paths recursively: e.g. for 'ok/ok1/ok2/'
      //make a list: [ 'ok/ok1/ok2', 'ok/ok1', 'ok', '']
      D := Dirs;
      L := TStringList.Create;
      repeat
        D := ExtractFileDir(D);
        L.Add(D);
      until D = '';

      //Create each of the needed directories
      OK := True;
      for i := L.Count - 2 downto 0 do
        OK := OK and CreateDir(L[i]);
      Result := OK;
    except
    end;
  end;

  function CopyFile(FromName: string; ToName: string; Delete: boolean = False): boolean;
  var
    SourceF, DestF: TFileStream;
  begin
    //Return failed copy by default
    Result := False;
    if FromName = ToName then
      Exit;
    try
      MakeDirs(ToName);
      Writeln('Copying ', FromName, ' to ', ToName);
      SourceF := TFileStream.Create(FromName, fmOpenRead);
      DestF := TFileStream.Create(ToName, fmCreate);
      DestF.CopyFrom(SourceF, SourceF.Size);
      //Now the copy succeded
      Result := True;
    except
    end;
    SourceF.Free;
    DestF.Free;

    //Delete original file if asked
    if Result and Delete then
      DeleteFile(FromName);
  end;

  function MakeBackup(FromName: string; ToName: string): boolean;
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
    OK := CopyFile(FromName, BakName);
    //Now delete old files
    Result := OK;
  end;

  function TTestHTTPServer.ParseUploadPlugin(Value: string): boolean;
  var
    SL: TStrings;
    RegExp: string;
  begin
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
    FName: string;
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
    FName := ARequest.ContentFields.Values['userfile'];
    if ParseUploadPlugin(ARequest.ContentFields.Values['UploadPlugin']) then
    begin
      //Move file to Uploaddir
      FWikiFile := ConcatPaths([FUploadDir, FName]);
      OK := CopyFile(ARequest.Files[0].LocalFileName, FWikiFile, True);
    end;
    if OK then
      AResponse.Code := 200
    else
      AResponse.Code := 404;

    AResponse.SendContent;

    //Make backup
    BakFile := ConcatPaths([FBackupDir, FName]);
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
  end;

begin
  Verbose := True;
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
