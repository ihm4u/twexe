
unit windowslib;

{$mode objfpc}{$H+}

interface

uses
  Windows,registry;

procedure SetExecutePermission(const FileName: string);
function GetMimeType(const Ext:string):string;
function GetOSEXEExt():String;
function InstallExitHandler():boolean;

implementation

uses
  Twexemain, //For CleanupOnExit()
  exedata, //For GetStoragePath()
  SysUtils; //For GetEnvironmentVariable()

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

function WindowsExitCleanup( dwCtrlType: DWORD ): BOOL; stdcall;
Var
  ComSpec, Cmd: string;
  O: string='';
begin
  case (dwCtrlType) of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT,
    CTRL_LOGOFF_EVENT,CTRL_SHUTDOWN_EVENT:
    begin
      CleanupOnExit();
      //Windows can't delete the open executable, so we have to
      //do it here:
      ComSpec := SysUtils.GetEnvironmentVariable('ComSpec');
      Cmd := ' /c rmdir /S /Q "'+GetStoragePath()+'"';

      //For safety we make sure GetStoragePath is at least 4 characters
      //so that root doesn't get deleted by a bug in GetStoragePath
      if (ComSpec<>'') and (Length(GetStoragePath())>3) then
        RunCmd(ComSpec,Cmd,O,True);
    end;
  end;

  //Continue OS processing
  Result := False;
end;

function InstallExitHandler():boolean;
begin
  Result:=Windows.SetConsoleCtrlHandler(@WindowsExitCleanup,True { add handler } );
end;

end.
