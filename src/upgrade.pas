unit upgrade;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient,
  {twexe units}
  exedata, wikiops, fileops, logger, version;

  function NeedUpgrade():boolean;

implementation
const
  VERSION_URL = 'http://ihm4u.github.io/twexe/rel/VERSION';
  {$ifdef windows}
  UPGRADE_URL = 'http://ihm4u.github.io/twexe/rel/i386-linux/twexe.exe';
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
  Log(Format('Ver: %s Integer: %d Suffix: %s',[Ver,Result, Suffix]));
end;

//Return true if we need to upgrage
function NewerVersionOnline():boolean;
Var
  OnlineVer,CurrSfx,OnlineSfx:string;
  i,nCurrVer,nOnlineVer:Integer;
begin
  Result := False;
  OnlineVer := Trim(GetFromURL(VERSION_URL));
  nCurrVer := VerToInt(_VERSION,CurrSfx);
  nOnlineVer:= VerToInt(OnlineVer,OnlineSfx);
  Log(Format('Online version is ''%s'', current version is ''%s''.',
    [OnlineVer,_VERSION]));
  if nCurrVer < nOnlineVer then
    Result:=True
  else if (nCurrVer = nOnlineVer) and (CurrSfx <> OnlineSfx) then
    Result:=True;
end;

procedure Upgrade(const NewEXE:string);
Var
  WikiName: string='';
  O: string='';
begin
  If not IAmShadow() then
    Raise Exception.Create('Upgrade must be done from shadow');

  If not FileExists(NewExe) then
    Raise Exception.CreateFmt('Upgrade file ''%s'' does not exist.',[NewExe]);

  //We assume data has already been extracted in UnzippedDir
  If FindWikiFile(GetUnZipPath(),WikiName) then
  begin
    If RunCmd(NewEXE,'-s ' + WikiName,O)=0 then
      MoveFile(WikiNameToExeName(WikiName),GetEXEFile(),True)
    else
      Error(Format('Failure upgrading: unable to convert ''$s'' to a twixie',
        [WikiName]));
  end;

end;

function NeedUpgrade():boolean;
Var
  UpgDir,UpgFile: string;
begin
  Result := False;
  if NewerVersionOnline() then
  begin
    UpgDir := ConcatPaths([GetStoragePath(),'_upg']);
    UpgFile := ConcatPaths([UpgDir,ExtractFileName(GetEXEFile())]);
    MakeDirs(UpgDir);
    GetFromURL(UPGRADE_URL,UpgFile);
    Upgrade(UpgFile);
    Show('Upgrade successful.');
    Result := True;
    ExitCode := 0;
  end;
end;

end.

