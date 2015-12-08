
unit windowslib;

{$mode objfpc}{$H+}

interface

uses
  Windows;

procedure SetExecutePermission(const FileName: string);
function GetOSEXEExt():String;

implementation

procedure SetExecutePermission(const FileName: string);
begin
   {not needed in windows}
end;

function GetOSEXEExt():String;
begin
  Result:='.exe';
end;

end.
