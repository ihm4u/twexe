unit logger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

var
  LogVerbose: boolean;
  LogDebug:Integer;

procedure Msg(const Msg: string; const NewLine:boolean=True);
procedure Log(const Msg: string);
procedure Dbg(const Msg: string; const Level: Integer=1);
procedure Error(const Msg: string);

implementation

procedure Msg(const Msg: string; const NewLine:boolean=True);
begin
  if (NewLine) then
    WriteLn(Msg)
  else
    Write(Msg);
end;

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

procedure Error(const Msg: string);
begin
    WriteLn(Msg);
end;

end.
