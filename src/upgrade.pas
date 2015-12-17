unit upgrade;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient,
  {twexe units}
  exedata, wikiops, fileops, logger, version,
  {$ifdef unix}
  unixlib
  {$endif}
  {$ifdef windows}
  windowslib
  {$endif}
  ;

  function NeedUpgrade():boolean;

implementation
const
  VERSION_URL = 'http://ihm4u.github.io/twexe/rel/VERSION';
  {$ifdef windows}
  UPGRADE_URL = 'http://ihm4u.github.io/twexe/rel/i386-win32/twexe.exe';
  {$endif}
  {$ifdef linux}
  UPGRADE_URL = 'http://ihm4u.github.io/twexe/rel/x86_64-linux/twexe';
  {$endif}

function GetFromURL(const URL: string; const FileName: string = ''): string;
var
  Httpc: TFPHTTPClient;
begin
  Httpc := TFPHttpClient.Create(nil);
  Result := '';
  try
    if FileName <> '' then
      Httpc.Get(URL, FileName)
    else
      Result := Httpc.Get(URL);
  finally
    FreeAndNil(Httpc);
  end;
end;

function VerToInt(const Ver:string; var Suffix:string):Integer;
Var
  poz:Integer;
  Triad:string='';
begin
  Result := 0;
  Poz := Pos('-',Ver);
  Triad := Ver;
  If Poz <> 0 then
  begin
    Suffix := RightStr(Ver,Length(Ver)-Poz); //Do not include '-'
    Delete(Triad,poz,Length(Ver)-poz+1); //Delete '-' and suffix
  end;
  Triad := StringReplace(Triad,'.','',[rfReplaceAll]);
  Result:=StrToIntDef(Triad,0);
end;

//Return true if we need to upgrage
function NewerVersionOnline(var OnlineVer:string):boolean;
Var
  CurrSfx,OnlineSfx:string;
  i,nCurrVer,nOnlineVer:Integer;
begin
  Result := False;
  OnlineVer := Trim(GetFromURL(VERSION_URL));
  nCurrVer := VerToInt(_VERSION,CurrSfx);
  nOnlineVer:= VerToInt(OnlineVer,OnlineSfx);
  LogFmt('Online version is %s, current version is %s',
    [OnlineVer,_VERSION]);
  if nCurrVer < nOnlineVer then
    Result:=True
  else if (nCurrVer = nOnlineVer) and (CurrSfx <> OnlineSfx) then
    Result:=True;
end;

procedure Upgrade(const NewEXE:string);
Var
  WikiName: string='';
  O: string='';
  Args: string;
  Ans: Integer;
begin
  If not IAmShadow() then
    Raise Exception.Create('Upgrade must be done from shadow');

  If not FileExists(NewExe) then
    Raise Exception.CreateFmt('Upgrade file ''%s'' does not exist.',[NewExe]);

  //We assume data has already been extracted in UnzippedDir
  If FindWikiFile(GetUnZipPath(),WikiName) then
  begin
    Args := '-p "' + WikiName + '"';
    LogFmt('Running ''%s'' ''%s''',[NewExe,Args]);
    Ans:=RunCmd(NewEXE,Args,O);
    Sleep(200); //Give a little time for executable to close properly
    If (Ans<>0) or
       not CopyFile(WikiNameToExeName(WikiName),GetEXEFile(),True) then
    begin
      LogFmt('Exit code from conversion %d',[Ans]);
      Raise Exception.CreateFmt('Unable to convert ''%s'' to a twixie',
        [WikiName]);
    end;
  end;

end;

function NeedUpgrade():boolean;
Var
  UpgDir,UpgFile, NewVer: string;
begin
  Result := False;
  if NewerVersionOnline(NewVer) then
  begin
    UpgDir := ConcatPaths([GetStoragePath(),'_upg']);
    UpgFile := ConcatPaths([UpgDir,ExtractFileName(GetEXEFile())]);
    try
      MakeDirs(UpgDir + DirectorySeparator);
      Log('Downloading upgrade.');
      GetFromURL(UPGRADE_URL,UpgFile);
      SetExecutePermission(UpgFile);
      Log('Download finished. Starting upgrade.');
      Upgrade(UpgFile);
      Show('Upgraded to version '+NewVer);
      Result := True;
      ExitCode := 0;
    except on E:Exception do
      Error('Unable to upgrade: '+E.ToString);
    end;
  end;
end;

end.

