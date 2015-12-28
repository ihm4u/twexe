unit unixlib;

{$mode objfpc}{$H+}

interface

uses
  BaseUnix, fpmimetypes;

procedure SetExecutePermission(const FileName: string);
function GetOSEXEExt():String;
function GetMimeType(const Ext:string):string;
function InstallExitHandler():boolean;

implementation
Uses twexemain; //For CleanupOnExit()
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

procedure UnixHandleSignal(Sig : cint);cdecl;
begin
  CleanupOnExit();
  //Now end the process
  //This will not cause infinite recursion because we
  //installed the Signal Handler with the SA_ONESHOT flag
  FpKill(GetProcessID(),SIGTERM);

end;

function InstallExitHandler():boolean;
Var
   oa,na : PSigActionRec;

begin
   Result := False;
   new(na);
   new(oa);
   na^.sa_Handler:=SigActionHandler(@UnixHandleSignal);
   fillchar(na^.Sa_Mask,sizeof(na^.sa_mask),#0);
   na^.Sa_Flags:=SA_ONESHOT or SA_RESTART;
   {$ifdef Linux}               // Linux specific
     na^.Sa_Restorer:=Nil;
   {$endif}
   if (fpSigAction(SIGINT, na, oa)<>0)
      or (fpSigAction(SIGTERM, na, oa) <> 0)
      or (fpSigAction(SIGHUP, na, oa) <> 0)
      or (fpSigAction(SIGPIPE, na, oa) <> 0) then
     Result := False
   else
     Result := True;
end;

end.
