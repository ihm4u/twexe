
unit windowslib;

{$mode objfpc}{$H+}

interface

uses
  Windows,registry;

procedure SetExecutePermission(const FileName: string);
function GetMimeType(const Ext:string):string;
function GetOSEXEExt():String;

implementation

function GetMimeType(const Ext:string):string;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  Result:='';
  try
    // Navigate to proper "directory":
    Registry.RootKey := HKEY_CLASSES_ROOT;
    if Registry.OpenKeyReadOnly('\' + Ext) then
      Result:=Registry.ReadString('Content Type'); //read mime type
  finally
    Registry.Free;
  end;
end;

procedure SetExecutePermission(const FileName: string);
begin
   {not needed in windows}
end;

function GetOSEXEExt():String;
begin
  Result:='.exe';
end;

end.
