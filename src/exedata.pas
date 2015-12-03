unit exedata;

{$mode objfpc}{$H+}

interface

uses
  // Free pascal units
  Classes, SysUtils, Zipper,

  //Project units 
  logger;

type
  TCallacks = class(TObject)
    class procedure OnOpenZippedStream(UnZipper: TObject; var FZipStream: TStream);
  end;

procedure ExtractData(ExeFile: string = ''; Delete: boolean = False);

implementation

//
//Make a Memory Stream containing the data to unzip
//
class procedure TCallacks.OnOpenZippedStream(UnZipper: TObject; var FZipStream: TStream);
var
  W: DWord;
  MemStr: TMemoryStream;
  ZipPos: int64;
const
  //Shift the header signature one bit right because otherwise it
  //can be found in the executable also
  ZipHDR = $04034b50 shr 1;
begin
  FZipStream := TFileStream.Create(ParamStr(0), fmOpenRead or fmShareDenyWrite);
  repeat
    W := FZipStream.ReadDWord();
  until (W = ZipHDR shl 1) or (FZipStream.Size = FZipStream.Position);

  FZipStream.Seek(-4, soFromCurrent);
  ZipPos := FZipStream.Position;

  //Copy Zip data to a Memory Stream
  if (W = ZipHDR shl 1) then
  begin
    Log('Found data at position: ' + IntToStr(ZipPos));
    MemStr := TMemoryStream.Create;
    MemStr.CopyFrom(FZipStream, FZipStream.Size - ZipPos);
    FreeAndNil(FZipStream);
    FZipStream := MemStr;
  end
  else
  begin
    WriteLn('No TiddlyWiki found.');
    WriteLn('Please download tw2exe again.');
    FreeAndNil(FZipStream);
  end;
end;

//
// Extract ZIP file contained in the executable
//
procedure ExtractData(ExeFile: string = ''; Delete: boolean = False);
var
  UnZipper: TUnZipper;
  CB: TCallacks;

begin
  CB := TCallacks.Create;
  UnZipper := TUnZipper.Create;
  UnZipper.OnOpenInputStream := @CB.OnOpenZippedStream;
  try
    if ExeFile = '' then
      ExeFile := ParamStr(0);
    UnZipper.FileName := ExeFile;
    //UnZipper.OutputPath := GetAppConfigDir(False);
    UnZipper.UnZipAllFiles;
    Log('Extracted data in ' + GetCurrentDir());
  finally
    FreeAndNil(CB);
    if Assigned(UnZipper) then
      FreeAndNil(UnZipper);
    if Delete then
      DeleteFile(PChar(ExeFile));
  end;
end;

//
// Replace ZIP file contained in the executable with specified data file
//
procedure AppendFile(const ExeFile: string; const DataFile: string);
var
  Zipper: TZipper;
  TmpFile: string;
  S: TStringList;

begin
  //Zip data file into a temporary zip file
  Zipper := TZipper.Create;
  TmpFile := GetTempDir() + DataFile + '.zip';
  S := TStringList.Create;
  S.Add(DataFile);
  with Zipper do
  begin
    try
      ZipFiles(TmpFile, S);
    finally
      Free;
    end;
  end;

  //Append zipped file to executable
  //DeleteFile(PChar(TmpFile));
end;

end.
