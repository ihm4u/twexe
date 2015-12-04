unit unixlib;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix;

procedure SetExecutePermission(const FileName: string);
function GetOSEXEExt():String;

implementation

procedure SetExecutePermission(const FileName: string);
begin
  fpChmod(FileName,&751);
end;

function GetOSEXEExt():String;
begin
  Result:='';
end;

end.
