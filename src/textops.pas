
unit textops;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes,regexpr;

//Return group matches from a regex
function MatchRegex(const RegExpr: string; const Text: string; var Matches: TStrings): boolean;

implementation

  //Return group matches from a regex
  function MatchRegex(const RegExpr: string; const Text: string;
  var Matches: TStrings): boolean;
  var
    Regex: TRegExpr;
    i: integer;
  begin
    Regex := TRegExpr.Create;
    try
      with Regex do
      begin
        Expression := RegExpr;
        //We have a match
        if Exec(Text) then
        begin
          //Add all matches
          repeat
            begin
              for i := 1 to SubExprMatchCount do
                Matches.Add(Match[i]);
            end
          until not ExecNext;
          Result := True;
        end
        else
          Result := False;
      end;
    finally
      if Assigned(Regex) then
        FreeAndNil(Regex);
    end;
  end;


end.
