unit exedata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper;


procedure ExtractData(ExeFile:String=''; Delete:boolean=FALSE);


implementation

procedure ExtractData(ExeFile:String=''; Delete:boolean=FALSE);
Var 
   UnZipper:TUnZipper;
begin
   UnZipper := TUnZipper.Create;
   try
      if ExeFile = '' then
         ExeFile:=ParamStr(0);
      UnZipper.FileName := ExeFile;
      UnZipper.OutputPath := GetAppConfigDir(False);
      Writeln('Extracted data in ',UnZipper.OutputPath);
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;
   finally
      if Assigned(UnZipper) then
         FreeAndNil(UnZipper);
      if Delete then
         DeleteFile(PChar(ExeFile));
   end;
end;

procedure AppendFile(const ExeFile:String; const DataFile:String);
Var
   Zipper: TZipper;
   TmpFile: String;
   S: TStringList;

begin
   //Zip data file into a temporary zip file
   Zipper := TZipper.Create;
   TmpFile := GetTempDir() + DataFile + '.zip';
   S:=TStringList.Create;
   S.Add(DataFile);
   With Zipper do
   begin
      try
         ZipFiles(TmpFile,S);
      Finally
         Free;
      end;  
   end;

   //Append zipped file to executable
   //DeleteFile(PChar(TmpFile));
end;

end.
