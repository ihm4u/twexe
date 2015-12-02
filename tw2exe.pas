program tweasy;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  SysUtils,
  Classes,
  fphttpserver,
  fpmimetypes,
  exedata;

type

  { TTestHTTPServer }

  TTestHTTPServer = class(TFPHTTPServer)
  private
    FBaseDir: string;
    FCount: integer;
    FMimeLoaded: boolean;
    FMimeTypesFile: string;
    procedure SetBaseDir(const AValue: string);
    procedure HandleGetReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure HandlePostReq(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);

  protected
    procedure CheckMimeLoaded;
    property MimeLoaded: boolean read FMimeLoaded;
  public
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
    property BaseDir: string read FBaseDir write SetBaseDir;
    property MimeTypesFile: string read FMimeTypesFile write FMimeTypesFile;
  end;

var
  Serv: TTestHTTPServer;

  { TTestHTTPServer }

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

  procedure TTestHTTPServer.HandlePostReq(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
  begin
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
        Write(LineEnding + 'Serving file: "', Fn, '". Reported Mime type: ',
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
  {If FCount>=5 then
    Active:=False;}
  end;

  procedure TTestHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
  begin
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
      Serv.BaseDir := ExtractFilePath(ParamStr(0));
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

begin
  ExtractData();
  Serv := StartServer();
  //Nothing runs here because server is single-threaded for now
end.
