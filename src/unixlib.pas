unit unixlib;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, fpmimetypes;

procedure SetExecutePermission(const FileName: string);
function GetOSEXEExt():String;
function GetMimeType(const Ext:string):string;

implementation
var
  FMimeLoaded: boolean;
  FMimeTypesFile: string;

function GetMimeType(const Ext:string):string;
begin
  if (not FMimeLoaded) then
  begin
    MimeTypes.LoadFromFile('/etc/mime.types');
    FMimeLoaded := True;
  end;
  Result:=MimeTypes.GetMimeType(Ext);
end;

procedure SetExecutePermission(const FileName: string);
begin
  fpChmod(FileName,&751);
end;

function GetOSEXEExt():String;
begin
  Result:='';
end;

end.
