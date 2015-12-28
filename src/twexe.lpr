program twexe;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { twexe units }
  ,logger, twexemain, exedata, fileops
  ;

type

  { TTwexeApp }

  TTwexeApp = class(TCustomApplication)
  private
    FileArgs: array of string;
    OrigExeFile: string;
    OutDir: string;

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
  CmdLOpts: TStrings;
  NonOpts: TStrings;
  i: Integer;
  ErrorMsg: string;
begin
  //Default values
  Opts.Flags := [toOpenBrowser];

  // parse options
  try
    CmdLOpts := TStringList.Create;
    NonOpts := TStringList.Create;

    // check parameters
    ErrorMsg:=CheckOptions('pvo:sz:hk::r::t:',[],CmdLOpts,NonOpts);
    if ErrorMsg<>'' then
    begin
      PrintHeader();
      logger.Error(ErrorMsg);
      logger.Error('Aborting.');
      Terminate;
    end;

    //Process z option - internal option for original exe path
    i:=CmdLOpts.IndexOfName('z');
    If i <> -1 then
    begin
      OrigExeFile:=CmdLOpts.ValueFromIndex[i];
      exedata.OriginalExeFile:=OrigExeFile; //FIXME: this is lousy
    end;

    //Process o option - output directory for conversion
    i:=CmdLOpts.IndexOfName('o');
    If i <> -1 then
      Opts.ConversionOutDir:=CmdLOpts.ValueFromIndex[i];

    //Process k option - extract tiddlywiki in optional directory
    //OutDir is used to call HandleExtractData
    i:=CmdLOpts.IndexOfName('k');
    If i <> -1 then
      OutDir := CmdLOpts.ValueFromIndex[i]
    else
      OutDir := GetEXEPath();

    //Process t option - port number to listen for http server
    i:=CmdLOpts.IndexOfName('t');
    If i <> -1 then
    begin
      Opts.ServerPort := StrToInt(CmdLOpts.ValueFromIndex[i]);
      LogFmt('User specified port: %d',[Opts.ServerPort]);
    end
    else
      Opts.ServerPort := 0; //Try different ports

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
      Opts.Flags := Opts.Flags - [toOpenBrowser];

    //Process p option - programatic run from program/script
    if HasOption('p') then
    begin
      Opts.Flags := Opts.Flags + [toProgramaticRun] - [toOpenBrowser];
    end;

    //Process r option - bind to 0.0.0.0 or specified address
    //to allow remote clients
    i:=CmdLOpts.IndexOfName('r');
    if HasOption('r') then
    begin
      Opts.Flags := Opts.Flags + [toAllowRemoteClients];
      If i <> -1 then //Remote address specified
        Opts.ServerBindAddress := CmdLOpts.ValueFromIndex[i]
      else
        Opts.ServerBindAddress := '' //same as 0.0.0.0, listen on all ifaces
    end
    else //Listen only locally
      Opts.ServerBindAddress := '127.0.0.1';

  finally
    SetLength(FileArgs,NonOpts.Count);
    For i:=0 to NonOpts.Count - 1 do
       FileArgs[i] := NonOpts[i];

    If Assigned(CmdLOpts) then
      FreeAndNil(CmdLOpts);
    If Assigned(NonOpts) then
      FreeAndNil(NonOpts);
  end;
end;

procedure TTwexeApp.DoRun;
begin
  try
    //Handle cmdline args, and set values in twexemain.Opts
    ProcessOptions();

    If Terminated then
      Exit;

    //Write html tiddlywiki file
    If HasOption('k','') then
    begin
      PrintHeader();
      Show('Extracting tiddlywiki5 html file.');
      //OutDir is set by ProcessOptions()
      HandleExtractData(OutDir);
      Show('Tiddlywiki written in directory ''' + OutDir + '''.');
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

    { Run main function - options have been written to twexemain.Opts }
    Twexemain.TwexeMain(OrigExeFile, FileArgs);

    // stop program loop
    Terminate;

  except on E:Exception do
    begin
      Error('Unhandled exception: ' + E.ToString);
      CleanupOnExit();
      WaitForUser();
      Terminate;
    end;
  end;
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
  writeln(#9,'-k [dir]');
  TextColor(DarkGray);
  writeln(#9,#9,'Write html wiki file in the specified directory or ');
  writeln(#9,#9,'in the directory of this twixie if no directory is specified.');

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

