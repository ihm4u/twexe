unit upgrade;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation
const
  VERSION_URL = '';
  {$ifdef windows}
  UPDATE_URL = '';
  {$endif}
  {$ifdef linux}
  UPDATE_URL = '';
  {$endif}

function GetFromURL(const URL: string; const FileName: string = ''): string;
var
  Httpc: TFPHTTPClient;
begin
  Httpc := TFPHttpClient.Create(nil);
  Result := '';
  try
    if FileName <> '' then
      Httpc.Get(URL, FileName)
    else
      Result := Httpc.Get(URL);
  finally
    FreeAndNil(Httpc);
  end;
end;


end.

