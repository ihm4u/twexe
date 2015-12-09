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
  Black = 0;
  Blue = 1;
  Green = 2;
  Cyan = 3;
  Red = 4;
  Magenta = 5;
  Brown = 6;
  LightGray = 7;
  DarkGray = 8;
  LightBlue = 9;
  LightGreen = 10;
  LightCyan = 11;
  LightRed = 12;
  LightMagenta = 13;
  Yellow = 14;
  White = 15;
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
var
  S:string;
begin
  //FIXME: most colors need to be fixed for better look
  S:=#27 + '[';
  Case Color of
    Black:        S:=S+'30';
    Blue:         S:=S+'1;'+'34';
    Green:        S:=S+'0;'+'32';
    Cyan:         S:=S+'1;'+'36';
    Red:          S:=S+'1;'+'31';
    Magenta:      S:=S+'1;'+'35';
    Brown:        S:=S+'33';      //Yellow
    LightGray:    S:=S+'37';      //White
    DarkGray:     S:=S+'1;'+'30'; //Bold black
    LightBlue:    S:=S+'34';
    LightGreen:   S:=S+'32';
    LightCyan:    S:=S+'36';
    LightRed:     S:=S+'31';
    LightMagenta: S:=S+'35';
    Yellow:       S:=S+'33';
    White:        S:=S+'1;'+'37';
  end;
  S:=S+'m';
  Write(S);
end;

procedure TextBackground(const Color:byte);
var
  S:string;
begin
  //FIXME: most colors need to be fixed for better look
  S:=#27 + '[';
  Case Color of
    Black:        S:=S+'40';
    Blue:         S:=S+'1;'+'44';
    Green:        S:=S+'0;'+'42';
    Cyan:         S:=S+'1;'+'46';
    Red:          S:=S+'1;'+'41';
    Magenta:      S:=S+'1;'+'45';
    Brown:        S:=S+'43';      //Yellow
    LightGray:    S:=S+'47';      //White
    DarkGray:     S:=S+'1;'+'40'; //Bold black
    LightBlue:    S:=S+'44';
    LightGreen:   S:=S+'42';
    LightCyan:    S:=S+'46';
    LightRed:     S:=S+'41';
    LightMagenta: S:=S+'45';
    Yellow:       S:=S+'43';
    White:        S:=S+'1;'+'47'; //Bold
  end;
  S:=S+'m';
  Write(S);
end;
{$endif}

procedure ResetColors();
begin
  {$ifdef windows}
  TextColor(White);
  TextBackground(Black);
  {$endif}

  {$ifdef unix}
  Write(#27+'[0m');
  {$endif}
end;

function Indent(const Str:string; const Nspaces:Integer; Len: Integer=77):string;
var
  Spcs,S:string;
  Poz,Lineno,nadds:integer;
begin
  Result:=Str;
  If Length(Str)+Nspaces < Len then
    Exit;

  S:=Str;
  Lineno:=1;
  nadds:=0;
  Poz:=len*lineno-Nspaces;
  Spcs:= StringOfChar(#32,Nspaces);
  repeat
    Insert(LineEnding+spcs,S,Poz);
    Inc(nadds);
    Inc(Lineno);
    Poz:=Poz+len;
  until Poz >= Length(s);
  Result:=S;
end;

procedure Show(const Msg: string; const NewLine:boolean=True);
Var
  S:string;
begin
  TextColor(DarkGray);
  S:=Indent(Msg,1,79);
  if (NewLine) then
    WriteLn(S)
  else
    Write(S);
  ResetColors();
end;

procedure Log(const Msg: string);
Var
  TS: string;
  S,NL: string;
begin
  if (LogVerbose) then
  begin
    S:=Msg;
    NL:='';
    //If message has a line ending in the beginning
    //make sure we put it before the time/date
    If (Length(Msg)>0) and (Msg[1]=LineEnding) then
    begin
      NL := LineEnding;
      Delete(S,1,1);
    end;
    TS:=FormatDateTime(' [ ddd. hh:mm:ss ] - ',Now);
    TextColor(green);
    S:=Indent(S,Length(TS),77);
    WriteLn(NL+TS+S);
    ResetColors();
  end;
end;

procedure Dbg(const Msg: string; const Level: Integer=1);
begin
  if (Level<=LogDebug) then
    WriteLn(Msg);
end;

procedure Error(const Msg: string);
Const
  HDR='ERROR: ';
begin
    TextBackground(Red);
    TextColor(Yellow);
    WriteLn(HDR+Indent(Msg,Length(HDR),77));
    ResetColors();
end;

end.
