program twexe;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { twexe units }
  ,logger, twexemain, exedata
  ;

type

  { TTwexeApp }

  TTwexeApp = class(TCustomApplication)
  private
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
  twexemain.TwexeOptions := [toOpenBrowser];

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
      TwexeOptions := TwexeOptions - [toOpenBrowser];

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

  //Handle cmdline args, and set values in twexemain.Twexeoptions
  ProcessOptions();

  { Run main function - options have been written to twexeoptions in twexemain}
  Twexemain.TwexeMain(OrigExeFile, FileArgs);

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

