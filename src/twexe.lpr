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
    ErrorMsg:=CheckOptions('vsz:hkr',[],Opts,NonOpts);
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

    //Process s option - no browser
    if HasOption('s') then
      TwexeOptions := TwexeOptions - [toOpenBrowser];

    if HasOption('r') then
      TwexeOptions := TwexeOptions + [toAllowRemoteClients];

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

  If Terminated then
    Exit;

  //Write html tiddlywiki file
  If HasOption('k','') then
  begin
    PrintHeader();
    Show('Extracting tiddlywiki5 html file.');
    ExtractData(GetEXEFile(),GetEXEPath());
    Show('Tiddlywiki written in directory ''' + GetEXEPath() + '''.');
    Terminate;
    Exit;
  end;

  //Print header (includes version number)
  If HasOption('v','') then
  begin
    PrintHeader();
    Terminate;
    Exit;
  end;

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
  Write('Usage: ');
  TextColor(Blue); Write(FileNameNoExt(ExeName));
  TextColor(Yellow);Writeln(' [options] [file]');
  ResetColors();
  Writeln();
  Writeln('  This file is a twixie, a single file tiddlywiki5 executable.');
  Writeln();
  Writeln('  If you run it with no file specified, it will open the browser');
  Writeln('  and serve the tiddlywiki5 file.');
  Writeln();
  Writeln('  If a file is specified, and it is a TiddlyWiki5 file, then');
  Writeln('  convert it into a twixie with the same file name as the ');
  Writeln('  specified file.');
  Writeln();
  TextColor(Blue);
  writeln('OPTIONS:');
  writeln(#9,'-h');
  TextColor(DarkGray);
  writeln(#9,#9,'Print this help information.');

  TextColor(Blue);
  writeln(#9,'-s');
  TextColor(DarkGray);
  writeln(#9,#9,'Do not open browser.');
  ResetColors();

  TextColor(Blue);
  writeln(#9,'-k');
  TextColor(DarkGray);
  writeln(#9,#9,Format('Write html wiki file to ''%s''.'
    ,[GetEXEName()+'.html']));

    TextColor(Blue);
  writeln(#9,'-v');
  TextColor(DarkGray);
  writeln(#9,#9,Format('Print version number and exit.'
    ,[GetEXEName()+'.html']));

  ResetColors();

end;

var
  Application: TTwexeApp;

{$R *.res}

begin
  Application:=TTwexeApp.Create(nil);
  Application.Run;
  Application.Free;
end.

