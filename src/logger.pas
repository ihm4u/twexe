unit logger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

var
  LogVerbose: boolean;
  LogDebug:Integer;

procedure Log(const Msg: string);

implementation

procedure Log(const Msg: string);
begin
  if (LogVerbose) then
    WriteLn(Msg);
end;

procedure Dbg(const Msg: string; const Level: Integer=1);
begin
  if (Level<=LogDebug) then
    WriteLn(Msg);
end;
end.
