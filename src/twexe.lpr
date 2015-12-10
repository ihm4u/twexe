program twexe;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  ,logger, twexemain, HTTPDefs, fphttpclient
  ;

type

  { TTwexeApp }

  TTwexeApp = class(TCustomApplication)
  private
    OpenBrowser: boolean;
    FileArgs: array of string;
    OrigExeFile: string;

  protected
    procedure DoRun; override;
    procedure ProcessOptions();

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTwexeApp }

procedure TTwexeApp.ProcessOptions();
Var
  Opts: TStrings;
  NonOpts: TStrings;
  i: Integer;
  ErrorMsg: string;
begin
  //Default values
  OpenBrowser := True;

  // parse options
  try
    Opts := TStringList.Create;
    NonOpts := TStringList.Create;

    // check parameters
    ErrorMsg:=CheckOptions('nz:h',[],Opts,NonOpts);
    if ErrorMsg<>'' then
    begin
      PrintHeader();
      logger.Error(ErrorMsg);
      logger.Error('Aborting.');
      Terminate;
    end;

    //Process z option
    i:=Opts.IndexOfName('z');
    If i <> -1 then
      OrigExeFile:=Opts.ValueFromIndex[i];

    //Process h option
    if HasOption('h','help') then
    begin
      PrintHeader();
      WriteHelp;
      Terminate;
      Exit;
    end;

    //Process n option
    if HasOption('n') then
      OpenBrowser := False;

  finally
    SetLength(FileArgs,NonOpts.Count);
    For i:=0 to NonOpts.Count - 1 do
       FileArgs[i] := NonOpts[i];

    If Assigned(Opts) then
      FreeAndNil(Opts);
    If Assigned(NonOpts) then
      FreeAndNil(NonOpts);
  end;
end;

procedure TTwexeApp.DoRun;
begin

  ProcessOptions();

  { Run main function }
  Twexemain.TwexeMain(OpenBrowser, OrigExeFile, FileArgs);

  // stop program loop
  Terminate;
end;

constructor TTwexeApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTwexeApp.Destroy;
begin
  inherited Destroy;
end;

procedure TTwexeApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TTwexeApp;

{$R *.res}

begin
  Application:=TTwexeApp.Create(nil);
  Application.Run;
  Application.Free;
end.

