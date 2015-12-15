
{*********** Wiki file manipilation  utilities **********}
unit wikiops;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes,regexpr,
  
  logger,fileops,exedata,
  {$ifdef unix}
  unixlib
  {$endif}
  {$ifdef windows}
  windowslib
  {$endif}
  ;

  const
    woAlways = 1;
    woIfTwexe = 2;

  function GetUnzippedWikiFile():string;

  function UpdateWikiSavingConfig(const WikiFile: string; const OutFile: string; 
    const Port:Integer; When: Integer): boolean;

  function EnsureTwexeSavingConfig(const WikiFile: string; const OutFile: string; 
    const Port:Integer): boolean;

  function FindWikiFile(const Dir: string; var WikiName: string):boolean;
  function IsWikiFile(const FileName: string):boolean;
  function WikiNameToExeName(const WikiName:string):string;
  function ExeNameToWikiName(const ExeName:string):string;

implementation
  //FIXME: use these two functions any time we need a wiki name
  // from an exe name or viceversa
  function WikiNameToExeName(const WikiName:string):string;
  begin
    Result := ChangeFileExt(ExpandFileName(WikiName),GetOSEXEExt());
  end;

  function ExeNameToWikiName(const ExeName:string):string;
  begin
    Result := ChangeFileExt(ExpandFileName(ExeName),'.html');
  end;

  //Returns true if file is a TiddlyWiki; false otherwise
  function IsWikiFile(const FileName: string):boolean;
  Const
     Regex = '<!--~~ This is a Tiddlywiki file.';
  Var
     WikiS: TStream;
     Buffer: string;
  begin
    Result := False;
    If not FileExists(FileName) then
      Exit;

    SetLength(Buffer,5120);
    try
      WikiS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      WikiS.Read(Buffer[1],5120);
		Result:= ExecRegExpr(Regex,Buffer);
    except
      //This is so we catch any exceptions and Result stays false
    end;

    if Assigned(WikiS) then
      FreeAndNil(WikiS);
  end;
   
  //Return firt Wiki file found in Directory 'Dir'
  function FindWikiFile(const Dir: string; var WikiName:string):boolean;
  var 
    Info : TSearchRec;
    Path: string;
    FullName: string;
    IsWiki: boolean;
  begin
    Result:=False;
    WikiName := '';
    Path := ConcatPaths([Dir,'*']);
    If FindFirst(Path,faAnyFile,Info)=0 then
    begin
     Repeat
       FullName := ConcatPaths([Dir,Info.Name]);
       IsWiki := IsWikiFile(FullName);
     Until (FindNext(info)<>0) or IsWiki;
     If IsWiki then
     begin
       Log('Found wikifile: '''+ExtractFileName(FullName)+'''.');
       WikiName:=FullName;
       Result:=True;
     end;
    end;
    FindClose(Info);
  end;

  //Returns full path  of wiki file in unzipped directory
  function GetUnzippedWikiFile():string;
  begin
    //FIXME: maybe this could be done in a better way
    //       we do this because the POST request saves
    //       the file with an .html extension, but the 
    //       first time the file is zipped it may have 
    //       an .htm extension.
    Result:=GetUnZipPath() + GetEXEName() + '.html';
  
    If not FileExists(Result) then
      Result:=GetUnZipPath() + GetEXEName() + '.htm';

    //We dont need to check if file exists again, 
    // because the caller will deal with it
  end;

  procedure ReplaceWantPassword(var S:string);
  Const
    Regex = '(?s)(\$:\/core\/modules\/savers\/upload\.js&quot;: {.*?if)(\(!username \|\| username\.toString\(\)\.trim\(\) === \\&quot;\\&quot; \|\| !password \|\| password\.toString\(\)\.trim\(\) === \\&quot;\\&quot;\))';
  begin
    S := ReplaceRegExpr(Regex,S,'$1((username!==\\&quot;twexe\\&quot;) && $2)',True);
  end;

  //Return true if 'twexe' is found in the wiki name of 
  //the saving tab of the control panel of the wikile
  function HasTwexeUploadName(const SBuffer:string):boolean;
  Const
     Regex = '(?si)(<div .*? title="\$:/UploadName">\s*<pre>)\s*(twexe)\s*(</pre>\s*</div>)';
  begin
     Result:= ExecRegExpr(Regex,SBuffer);
  end;

  function HasUploadName(const SBuffer:string):boolean;
  Const
     Regex = '(?si)(<div .*? title="\$:/UploadName">\s*<pre>).*?(</pre>\s*</div>)';
  begin
     Result:= ExecRegExpr(Regex,SBuffer);
  end;

  //Replace upload name with 'twexe' according to 'When' parameter
  procedure ReplaceUploadName(var SBuffer:string; const When:Integer);
  Const
    RegexAlways = '(?si)(<div .*? title="\$:/UploadName">\s*<pre>)(.*?)(</pre>\s*</div>)';
    RegexIfTwexe = '(?si)(<div .*? title="\$:/UploadName">\s*<pre>)\s*(twexe)\s*(</pre>\s*</div>)';
  Var
    Regex: string;
  begin
    If When = woAlways then
      Regex := RegexAlways
    else
      Regex := RegexIfTwexe;

    SBuffer := ReplaceRegExpr(Regex,SBuffer,'$1twexe$3',True);
  end;

  //Replace saving url with the proper server/port and path to contact
  //the twexe http server
  procedure ReplaceUploadURL(var SBuffer:string; const Port: string; const When:Integer);
  Const
    Regex = '(?si)(<div .*? title="\$:/UploadURL">\s*<pre>)(.*?)(</pre>\s*</div>)';
  Var
    TwexeURL: string;
  begin
    TwexeURL := 'http://127.0.0.1:' + Port + '/store';
    If When = woAlways then
      SBuffer := ReplaceRegExpr(Regex,SBuffer,'$1'+TwexeURL+'$3',True)
    else
      If (When = woIfTwexe) and HasTwexeUploadName(SBuffer) then
        SBuffer := ReplaceRegExpr(Regex,SBuffer,'$1'+TwexeURL+'$3',True)
  end;

  procedure MaybeAddSavingConfig(var SBuffer: string; const Port: string; const When:Integer);
  Var
    NewCfg: string;
  Const
    Regex='(</div>\s*)(</div>\s*<!--~~ Library modules ~~-->\s*<div\s+id="libraryModules")';
    //FIXME: put real created and modified dates for the sake of correctness, even though
    //       it works as is
    NewCfgPre='<div created="20151205053838348" modified="20151205053838615" title="\$:/UploadName">'+ LineEnding +
              '<pre>twexe</pre>'              + LineEnding +
              '</div>'                        + LineEnding +
              '<div created="20151205053834191" modified="20151205053845194" title="\$:/UploadURL">' + LineEnding +
              '<pre>http://127.0.0.1:';

    NewCfgPost='/store</pre>' + LineEnding + '</div>' + LineEnding + LineEnding;
  begin

    //We exit if there is any wiki name already there
    if HasUploadName(SBuffer) then
      Exit;

    NewCfg := NewCfgPre + Port + NewCfgPost;
    SBuffer := ReplaceRegExpr(Regex,SBuffer,'$1'+NewCfg+'$2',True);
  end;

  procedure ReplaceOrAddConfig(var S:string; const Port:Integer; const When:Integer);
  Const
     UNTitle='title="$:/UploadName"';
     UURLTitle='title="$:/UploadURL"';
     LibModule='id="libraryModules"';
     PasswordChk='UploadPlugin';
  Var
    PortStr: string;

  begin
    PortStr := IntToStr(Port);
    //Attempt change only if we may have a section that matters
    //Otherwise return quickly without changing anything
    If (Pos(UNTitle,S)<>0)  //We may have an uploadName tiddler
       or (Pos(UURLTitle,S)<>0) //We may have an uploadURL tiddler
       or (Pos(LibModule,S)<>0) //We may need to add the config
       or (Pos(PasswordChk,S)<>0) //We have the section for password check
       then
    begin
      //The check for empty password (for automatic save)
      //is done only if the wiki name is not twexe
      //(in the saving tab of the wikis control panel)
      ReplaceWantPassword(S);
      //Add saving config if wiki doesnt have any
      MaybeAddSavingConfig(S,PortStr,When);

      //Go ahead and do all the regex checking
      //if woAlways was specified
      if When = woAlways then
      begin
        ReplaceUploadName(S,When);
        ReplaceUploadURL(S,PortStr,When);
      end
      else
      begin
        //Do the regex checking/replacing only when 'twexe' is in the
        //uploadName tiddler and the caller specified woIfTwexe
        If (When = woIfTwexe) and HasTwexeUploadName(S) then
        begin
          ReplaceUploadName(S,When);
          ReplaceUploadURL(S,PortStr,When);
        end;
      end;
    end;

  end;

  function UpdateWikiSavingConfig(const WikiFile: string; const OutFile: string; const Port:Integer; When: Integer): boolean;
  Const
     OVERLAP=3072;
     BUFSIZE=33792; //33K To honor Our Lord
  var
    WikiS,OutS: TFileStream;
    ReadCount: LongInt;
    Buffer: string;
  begin
    Result := False;
    try
      // 1. Read buffer from input wikifile
      // 2. Update buffer if necessary
      // 3. Write buffer back to output wikifile
      SetLength(Buffer,BUFSIZE);
      MakeDirs(OutFile);
      WikiS := TFileStream.Create(WikiFile, fmOpenRead or fmShareDenyWrite);
      OutS := TFileStream.Create(OutFile, fmCreate);
      Log('Updating saving configuration in ''' + OutFile + '''.');

      //We overlap the reading buffers by OVERLAP bytes in order to 
      //prevent any of the regex matches to fail because of the string
      //being split at the boundary of two buffers. OVERLAP bytes accomodates
      //enough space for the largest string matching any of the regexes
      //in the replace/add functions
      ReadCount := WikiS.Read(Buffer[1],BUFSIZE);
      SetLength(Buffer,ReadCount);
      ReplaceOrAddConfig(Buffer,Port,When);
      OutS.Write(Buffer[1],Length(Buffer)*SizeOf(Char));
      repeat
        WikiS.Seek(-OVERLAP,soCurrent);
        ReadCount := WikiS.Read(Buffer[1],BUFSIZE);
        SetLength(Buffer,ReadCount);
        ReplaceOrAddConfig(Buffer,Port,When);
        OutS.Write(Buffer[OVERLAP+1],Length(Buffer)*SizeOf(Char)-OVERLAP);
      until (WikiS.Position = WikiS.Size);
      
      //The writing of updated wiki succeded
      Result := True;
    except
    end;
    if Assigned(WikiS) then
      FreeAndNil(WikiS);
    if Assigned(OutS) then
      FreeAndNil(OutS);
  end;

  function EnsureTwexeSavingConfig(const WikiFile: string; const OutFile: string; 
    const Port:Integer): boolean;
  begin
    Result:=UpdateWikiSavingConfig(WikiFile,OutFile,Port,woAlways);
  end;
end.
