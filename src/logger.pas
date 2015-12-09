unit logger;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$ifdef windows}
  ,crt
  {$endif};

{$ifdef unix}
{ Sadly we could not use the Crt unit under unix, even though
  it showed the right colors, because it would send spurious
  codes to the terminal and change the cursor position when
  shadow was executed.
  For now we leave it unimplemented in unix }
const
  White = 1;
  Black = 2;
  Green = 3;
  Yellow = 4;
  Red = 5;
{$endif}

var
  LogVerbose: boolean;
  LogDebug:Integer;

procedure Show(const Msg: string; const NewLine:boolean=True);
procedure Log(const Msg: string);
procedure Dbg(const Msg: string; const Level: Integer=1);
procedure Error(const Msg: string);

implementation

{$ifdef unix}
procedure TextColor(const Color:byte);
begin
end;

procedure TextBackground(const Color:byte);
begin
end;
{$endif}

procedure ResetColors();
begin
  TextColor(White);
  TextBackground(Black);
end;

procedure Show(const Msg: string; const NewLine:boolean=True);
begin
  if (NewLine) then
    WriteLn(Msg)
  else
    Write(Msg);
end;

procedure Log(const Msg: string);
Var
  TS: string;
begin
  if (LogVerbose) then
  begin
    TS:=FormatDateTime('   [ ddd. hh:mm:ss ] - ',Now);
    TextColor(green);
    WriteLn(TS+Msg);
    ResetColors();
  end;
end;

procedure Dbg(const Msg: string; const Level: Integer=1);
begin
  if (Level<=LogDebug) then
    WriteLn(Msg);
end;

procedure Error(const Msg: string);
begin
    TextBackground(Red);
    TextColor(Yellow);
    Write('ERROR: ');
    WriteLn(Msg);
    ResetColors();
end;

end.
