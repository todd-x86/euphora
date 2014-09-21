unit interpreter;

interface

uses SysUtils, Classes, hashmap, parser, iterators, lookup, strsim, csv, ShellAPI;

type
  TMailInfo = packed record
    SMTPServer: String;
    SMTPPort: String;
    SMTPUser: String;
    SMTPPass: String;
    MailTo: String;
    MailFrom: String;
    MailSubject: String;
    MailBody: String;
  end;

  TSetStringEvent = procedure (const S: String);
  TSetCookieEvent = procedure (const Key, Value: String);
  TSetBoolEvent = procedure (const B: Boolean);
  TScrapeEvent = function (const Start, Stop: String): String;
  TMailEvent = procedure (var MailInfo: TMailInfo);
  TURLEvent = procedure (const URL: String);
  TDownloadEvent = procedure (const URL, Fn: String);
  TParseEvent = function (const HTMLName: String): String;

  TWAInterpreter = class(TObject)
  private
    // Event list
    Immediate: Boolean;
    FOnReferrer: TSetBoolEvent;
    FOnRef: TSetStringEvent;
    FOnCookies: TSetBoolEvent;
    FOnAgent: TSetStringEvent;
    FOnSetCookie: TSetCookieEvent;
    FOnScrape: TScrapeEvent;
    FOnMail: TMailEvent;
    FOnGet: TURLEvent;
    FOnPost: TURLEvent;
    FOnField: TSetCookieEvent;
    FOnDownload: TDownloadEvent;
    FOnFile: TSetCookieEvent;
    FOnParse: TParseEvent;
    FOnEncode: TParseEvent;
    FOnDump: TSetStringEvent;
    FOnAuth: TSetBoolEvent;
    FOnConfig: TSetCookieEvent;

    FParserList: TList;
    FParser: PWAParser;
    FVars: TStringHashTable;
    FStreams: TObjectHashTable;
    FConfig: TStringHashTable;
    FTok: Integer;
    FVerbose: Boolean;
    procedure DoLookup (Fn: String; var Fields: TStringList; key,value: String);
    procedure CheckEOL;
    procedure ExecutePreProc;
    procedure Error (const Msg: String);
    procedure ExecInclude;
    procedure ExecEcho;
    procedure ExecReferrer;
    procedure ExecRef;
    procedure ExecSet;
    procedure ExecCookies;
    procedure ExecAgent;
    function ExecRealCSV: TRealCSVIterator;
    function ExecCSV: TCSVIterator;
    function ExecText: TTextIterator;
    procedure ExecLookup;
    procedure ExecVerbose;
    procedure ExecCookie;
    procedure ExecScrape;
    procedure ExecFSLookup;
    procedure ExecMail;
    procedure ExecGet;
    procedure ExecPost;
    procedure ExecForeach;
    procedure ExecBasename;
    function BuildString: String;
    function Next: Integer;
    procedure BlockExec;
    procedure ExecEncode;
    procedure Iterate (var Iter: TBaseIterator);
    procedure LoadVars (var Iter: TBaseIterator);
    function DoFSLookup (path, cmp: String): String;
    function BuildTemplate (Fn: String): String;
    function TemplateStr (S: String; Explicit: Boolean = True): String;
    procedure ExecField;
    procedure ExecConfig;
    procedure ExecDownload;
    procedure ExecFile;
    procedure ExecParse;
    procedure ExecDump;
    procedure ExecPrompt;
    procedure ExecRun;
    procedure ExecFReadLine;
    procedure ExecFWriteLine;
    procedure ExecAuth;
    procedure ExecFSeek;
    procedure SetConfig (key, value: String);
    procedure VMsg (M: String; p: array of const);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute (const Fn: String);
    function GetConfig (N: String): String;
    function GetVariable (N: String): String;
    procedure SetVariable (N, V: String);


    procedure Interpret (const Line: String);
    procedure BeginImmediate;

    property OnReferrer: TSetBoolEvent read FOnReferrer write FOnReferrer;
    property OnRef: TSetStringEvent read FOnRef write FOnRef;
    property OnCookies: TSetBoolEvent read FOnCookies write FOnCookies;
    property OnAgent: TSetStringEvent read FOnAgent write FOnAgent;
    property OnSetCookie: TSetCookieEvent read FOnSetCookie write FOnSetCookie;
    property OnScrape: TScrapeEvent read FOnScrape write FOnScrape;
    property OnMail: TMailEvent read FOnMail write FOnMail;
    property OnGet: TURLEvent read FOnGet write FOnGet;
    property OnPost: TURLEvent read FOnPost write FOnPost;
    property OnField: TSetCookieEvent read FOnField write FOnField;
    property OnDownload: TDownloadEvent read FOnDownload write FOnDownload;
    property OnFile: TSetCookieEvent read FOnFile write FOnFile;
    property OnParse: TParseEvent read FOnParse write FOnParse;
    property OnEncode: TParseEvent read FOnEncode write FOnEncode;
    property OnDump: TSetStringEvent read FOnDump write FOnDump;
    property OnAuth: TSetBoolEvent read FOnAuth write FOnAuth;
    property OnConfig: TSetCookieEvent read FOnConfig write FOnConfig;
  end;

implementation

const
  ERROR_EOL = 'End of line expected';
  ERROR_STR = 'String expected';
  ERROR_BOOL = 'Boolean value `off` or `on` expected';
  ERROR_FILE = 'Filename expected';
  ERROR_KW = 'Keyword expected';

constructor TWAInterpreter.Create;
begin
  inherited Create;
  FVerbose := False;
  FParserList := TList.Create;
  FParser := nil;
  Immediate := False;
  FVars := TStringHashTable.Create;
  FStreams := TObjectHashTable.Create;
  FConfig := TStringHashTable.Create;

  // 1 MB limit
  SetConfig('StringLengthLimit', IntToStr(1024*1024));
end;

destructor TWAInterpreter.Destroy;
begin
  if FParser <> nil then FParser^.Free;
  FParserList.Free;
  FStreams.Free;
  FVars.Free;
  inherited Destroy;
end;

procedure TWAInterpreter.VMsg (M: String; p: array of const);
begin
  Writeln(Format('[%s]: %s', [FormatDateTime('hh:nn:ss', Now), Format(M, p)]));
end;

procedure TWAInterpreter.SetConfig (key, value: String);
begin
  FConfig.Add(key, value);
  if Assigned(FOnConfig) then FOnConfig(key, value);
end;

procedure TWAInterpreter.ExecParse;
var html,id:String;
begin
  // @parse "html-name" "id"
  if Next <> T_STRING then Error(ERROR_STR);
  html := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  id := BuildString;
  if Assigned(FOnParse) then SetVariable(id, FOnParse(html));
  CheckEOL;
end;

procedure TWAInterpreter.ExecDownload;
var url,fn:String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  url := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  fn := BuildString;
  if Assigned(FOnDownload) then FOnDownload(url, fn);
  CheckEOL;
end;

procedure TWAInterpreter.ExecFile;
var id,fn:String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  id := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  fn := BuildString;
  if Assigned(FOnFile) then FOnFile(id, fn);
  CheckEOL;
end;

procedure TWAInterpreter.ExecBasename;
var id,data: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  id := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  data := BuildString;
  SetVariable(id, ExtractFileName(data));
  CheckEOL;
end;

function TWAInterpreter.BuildTemplate (Fn: String): String;
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(Fn);
  except on E: Exception do
    Error('Cannot open file "'+Fn+'"');
  end;
  Result := TemplateStr(sl.Text, False);
end;

procedure TWAInterpreter.ExecConfig;
var key, value: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  key := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  value := BuildString;
  SetConfig(key, value);
  CheckEOL;
end;

procedure TWAInterpreter.ExecField;
var id,value: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  id := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  value := BuildString;
  if Assigned(FOnField) then FOnField(id, value);
  CheckEOL;
end;

function TWAInterpreter.DoFSLookup (path, cmp: String): String;
var a: Integer;
    sr: TSearchRec;
    tmp,cscore:Single;
    bpath: String;
begin
  if FVerbose then begin
    VMsg('Filesystem lookup: "%s" in path %s', [cmp, path]);
  end;
  bpath := ExtractFilePath(path);
  a := FindFirst(path, faAnyFile, sr);
  cscore := 0;
  Result := '';
  while a = 0 do begin
    // Exclude directories
    if (sr.Name <> '.') and (sr.Name <> '..') and (not sr.Attr and faDirectory = faDirectory) then begin
      tmp := StrDiff(cmp, sr.Name);
      if tmp > cscore then begin
        Result := bpath+sr.Name;
        cscore := tmp;
        if FVerbose then begin
          VMsg(' - Top File: "%s", score %.4f', [Result, cscore]);
        end;
      end;
    end;
    a := FindNext(sr);
  end;
  FindClose(sr);
end;

procedure TWAInterpreter.LoadVars (var Iter: TBaseIterator);
var sl: PStringList;
    tmp: PChar;
    j: Integer;
    tmp2: String;
begin
  if Iter is TCSVIterator then begin
    sl := (Iter as TCSVIterator).NextValueSet;
    // Load all values from the fields into the var table...
    for j := 0 to sl^.Count-1 do begin
      SetVariable((Iter as TCSVIterator).Fields[j], sl^.Strings[j]);
    end;
  end else if Iter is TRealCSVIterator then begin
    sl := (Iter as TRealCSVIterator).NextValueSet;
    // Load all values from the fields into the var table...
    for j := 0 to sl^.Count-1 do begin
      SetVariable((Iter as TRealCSVIterator).Fields[j], sl^.Strings[j]);
    end;
  end else if Iter is TTextIterator then begin
    // Dump value from text file into variable associated with it...
    tmp := (Iter as TTextIterator).NextValueSet;
    tmp2 := StrPas(tmp);
    SetVariable((Iter as TTextIterator).VarName, tmp2);
  end;
end;

procedure TWAInterpreter.BlockExec;
begin
  while (FTok <> T_EOL) and (FTok <> T_EOF) do begin
    case FTok of
      T_PREPROC: ExecutePreProc;
      T_MAIL: ExecMail;
      T_GET: ExecGet;
      T_POST: ExecPost;
      T_FOREACH: ExecForeach;
      else begin
        Error('Unknown statement');
        Exit;
      end;
    end;
  end;
end;

procedure TWAInterpreter.DoLookup (Fn: String; var Fields: TStringList; key,value: String);
var l: TCSVLookup;
    j: Integer;
begin
  l := nil;
  try
    l := TCSVLookup.Create(fn, Fields.IndexOf(key), value);
  except on E: Exception do
    Error('Cannot open file "'+fn+'"');
  end;
  if FVerbose then begin
    VMsg('CSV Lookup (''%s'') - key: "%s", value: "%s"', [Fn, key, value]);
  end;
  // Add all values from in the lookup and store them to the variable table
  for j := 0 to Fields.Count-1 do begin
    SetVariable(Trim(Fields.Strings[j]), l.Data[j]);
  end;
end;

function TWAInterpreter.Next: Integer;
begin
  FTok := FParser^.GetToken;
  Result := FTok;
end;

procedure TWAInterpreter.CheckEOL;
begin
  if Next <> T_EOL then Error(ERROR_EOL);
end;

procedure TWAInterpreter.Error (const Msg: String);
begin
  Writeln(Format('ERROR (line %d, char %d in "%s"): %s', [FParser^.Line+1, FParser^.Char, FParser^.Filename, Msg]));
  if not Immediate then Halt;
end;

function TWAInterpreter.TemplateStr (S: String; Explicit: Boolean): String;
var j,st: Integer;
    tmp: String;
begin
  j := 1;
  while j <= Length(S) do begin
    if S[j] = '{' then begin
      Inc(j);
      st := j;
      // Parse variable
      while (j <= Length(S)) and (S[j] <> '}') do begin
        Inc(j);
      end;
      // "{...}"
      if (j <= Length(S)) and (S[j] = '}') then begin
        tmp := Copy(S, st, j-st);
        if FVars.Exists(tmp) then begin  // Check if var exists
          Result := Result + FVars.GetData(tmp);
        end else begin
          Result := Result + Copy(S, st-1, j-st+2);
        end;
      end else begin  // "{..."
        Result := Result + Copy(S, st, j-st-1);
      end;
    end else if (S[j] = '\') and (Explicit) then begin
      Inc(j);
      if (S[j] = 'r') then begin
        Result := Result + #13;
      end else if (S[j] = 'n') then begin
        Result := Result + #10;
      end else if (S[j] = 't') then begin
        Result := Result + #9;
      end else begin
        Result := Result + S[j];
      end;
    end else begin
      Result := Result + S[j];
    end;
    Inc(j);
  end;
end;

procedure TWAInterpreter.ExecEncode;
var id,txt:String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  id := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  txt := BuildString;
  if Assigned(FOnEncode) then SetVariable(id, FOnEncode(txt));
  CheckEOL;
end;

function TWAInterpreter.BuildString: String;
var buf: String;
begin
  buf := FParser^.GetString;
  Result := '';
  // Expect "
  if (buf[1] <> '"') or (buf[Length(buf)] <> '"') then begin
    Error('String builder could not determine beginning/end of string');
    Result := '';
  end
  else
    Result := TemplateStr(Copy(buf, 2, length(buf)-2));
end;

procedure TWAInterpreter.ExecDump;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  if Assigned(FOnDump) then FOnDump(BuildString);
  CheckEOL;
end;

procedure TWAInterpreter.ExecPrompt;
var kw, msg, tmp: String;
begin
  if Next <> T_STRUCTVAR then Error(ERROR_KW);
  kw := FParser^.GetString;
  if Next <> T_STRING then Error(ERROR_STR);
  msg := BuildString;
  CheckEOL;
  Write(msg);
  Readln(tmp);
  SetVariable(kw, tmp);
end;

procedure TWAInterpreter.ExecRun;
var execStr, execFile, execParams, execDir: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  execStr := BuildString;
  CheckEOL;
  execFile := ExtractFileName(execStr);
  execDir := ExtractFilePath(execStr);
  execParams := Copy(execStr, Length(execDir)+Length(execFile), Length(execStr)-Length(execDir)-Length(execFile));
  ShellExecute(0, 'open', PAnsiChar(execFile), PAnsiChar(execParams), PAnsiChar(execDir), 1);
end;

procedure TWAInterpreter.ExecFReadLine;
var kw,fn: String;
    fsobj: TFileStream;
    ch: Char;
    line: String;
begin
  // Line variable
  if Next <> T_STRUCTVAR then Error(ERROR_KW);
  kw := FParser^.GetString;
  // Filename
  if Next <> T_STRING then Error(ERROR_STR);
  fn := Lowercase(BuildString);
  CheckEOL;
  if not FStreams.Exists(fn) then begin
    if (FileExists(fn)) then
      fsobj := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone)
    else
      fsobj := TFileStream.Create(fn, fmCreate or fmOpenReadWrite or fmShareDenyNone);
    FStreams.Add(fn, fsobj);
  end else begin
    fsobj := (FStreams.GetData(fn) as TFileStream);
  end;
  line := '';
  // Read line
  fsobj.Read(ch, 1);
  while (fsobj.Position < fsobj.Size) and (ch <> #13) and (ch <> #10) do begin
    line := line + ch;
    fsobj.Read(ch, 1);
  end;

  // Skip whitespace for next line
  while (fsobj.Position < fsobj.Size) and ((ch = #13) or (ch = #10)) do
    fsobj.Read(ch, 1);

  Self.SetVariable(kw, line);
end;

procedure TWAInterpreter.ExecFWriteLine;
var fn, line: String;
    fsobj: TFileStream;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  fn := Lowercase(BuildString);
  if Next <> T_STRING then Error(ERROR_STR);
  line := BuildString;
  CheckEOL;

  if not FStreams.Exists(fn) then begin
    if (FileExists(fn)) then
      fsobj := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone)
    else
      fsobj := TFileStream.Create(fn, fmCreate or fmOpenReadWrite or fmShareDenyNone);
    FStreams.Add(fn, fsobj);
  end else begin
    fsobj := (FStreams.GetData(fn) as TFileStream);
  end;
  // Line break
  line := line + #13 + #10;

  fsobj.Write(line[1], Length(line));
end;

procedure TWAInterpreter.ExecFSeek;
var fn: String;
    fsobj: TFileStream;
    linenum: Integer;
    ch: Char;
begin
  // Filename
  if Next <> T_STRING then Error(ERROR_STR);
  fn := Lowercase(BuildString);

  // Line number
  if Next <> T_INTEGER then Error('Expected integer');
  linenum := StrToInt(FParser^.GetString);

  CheckEOL;

  if not FStreams.Exists(fn) then begin
    if (FileExists(fn)) then
      fsobj := TFileStream.Create(fn, fmOpenReadWrite or fmShareDenyNone)
    else
      fsobj := TFileStream.Create(fn, fmCreate or fmOpenReadWrite or fmShareDenyNone);
    FStreams.Add(fn, fsobj);
  end else begin
    fsobj := (FStreams.GetData(fn) as TFileStream);
  end;

  // Seek beginning
  fsobj.Seek(0, soBeginning);

  // Read one line at a time
  while (fsobj.Position < fsobj.Size) and (linenum > 0) do begin
    fsobj.Read(ch, 1);
    if ch = #10 then begin
      Dec(linenum);
      fsobj.Read(ch, 1);  // skip ahead by 1
    end;
  end;
end;

procedure TWAInterpreter.ExecAuth;
begin
  case Next of
    T_OFF: if Assigned(FOnAuth) then FOnAuth(False);
    T_ON: if Assigned(FOnAuth) then FOnAuth(True);
    else Error(ERROR_BOOL);
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecutePreProc;
var tmp: String;
begin
  // Ignore '@'
  Next;
  case FTok of
    T_PAUSE: begin
      Writeln('Press enter to continue...');
      Readln;
      CheckEOL;
    end;
    T_SLEEP: begin
      if Next <> T_INTEGER then Error('Integer value expected');
      tmp := FParser^.GetString;
      CheckEOL;
      Sleep(StrToInt(tmp)*1000);
    end;
    T_DUMP: ExecDump;
    T_PROMPT: ExecPrompt;
    T_RUN: ExecRun;
    T_QUIT: Halt;
    T_INCLUDE: ExecInclude;
    T_ECHO: ExecEcho;
    T_REFERRER: ExecReferrer;
    T_REF: ExecRef;
    T_SET: ExecSet;
    T_COOKIES: ExecCookies;
    T_AGENT: ExecAgent;
    T_LOOKUP: ExecLookup;
    T_VERBOSE: ExecVerbose;
    T_COOKIE: ExecCookie;
    T_SCRAPE: ExecScrape;
    T_FILE: ExecFile;
    T_DOWNLOAD: ExecDownload;
    T_FSLOOKUP: ExecFSLookup;
    T_FIELD: ExecField;
    T_HTMLPARSE: ExecParse;
    T_ENCODE: ExecEncode;
    T_BASENAME: ExecBasename;
    T_FREADLN: ExecFReadLine;
    T_FSEEK: ExecFSeek;
    T_FWRITELN: ExecFWriteLine;
    T_AUTH: ExecAuth;
    T_CONFIG: ExecConfig;
    else begin
      Error('Unknown preprocessor statement');
      while FTok <> T_EOL do Next;
      Exit;
    end;
  end;
end;

procedure TWAInterpreter.Iterate (var Iter: TBaseIterator);
begin
  // Inform parser we want to begin iteration of a loop (positions stored in stack)
  FParser^.PushInfo;
  while Iter.HasNext do begin
    FParser^.Reload;
    // Increments iterator position while loading variables...
    LoadVars(Iter);

    // Parse each line until encountering the '}' (need to test for nested foreach's)
    while (FParser^.Peek <> T_RBRACE) do begin
      Next;
      BlockExec;
      FParser^.NextLine;
    end;
  end;
  FParser^.PopInfo;
  //FParser^.NextLine;
end;

procedure TWAInterpreter.BeginImmediate;
begin
  if FParser <> nil then FParserList.Add(FParser);
  New(FParser);
  Immediate := True;
  FParser^ := TWAParser.Create;
end;

procedure TWAInterpreter.Interpret (const Line: String);
begin
  FParser^.InitParser;
  FParser^.SetLine(Line);
  Next;
  while FTok <> T_EOF do begin
    BlockExec;
    FParser^.NextLine;
    Next;
  end;
end;

procedure TWAInterpreter.Execute (const Fn: String);
begin
  if FParser <> nil then FParserList.Add(FParser);
  New(FParser);
  FParser^ := TWAParser.Create;
  try
    FParser^.LoadFromFile(Fn);
  except on E: Exception do
    Error('Cannot open file "'+Fn+'"');
  end;
  Next;
  while FTok <> T_EOF do begin
    BlockExec;
    FParser^.NextLine;
    Next;
  end;
end;

procedure TWAInterpreter.ExecInclude;
var fn: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);  // get include string
  fn := BuildString;
  if FVerbose then begin
    VMsg('Script push: `%s`', [fn]);
  end;
  Execute(fn);
  FParser^.Free;

  // Last parser pushed to stack is returned
  FParser := FParserList.Last;
  FParserList.Remove(FParserList.Last);
  if FParser = nil then Error('Internal include error (parser does not exist)');
  CheckEOL;
  if FVerbose then begin
    VMsg('Script pop: `%s`', [fn]);
  end;
end;

procedure TWAInterpreter.ExecEcho;
begin
  if Next <> T_STRING then
    Error(ERROR_STR)  // get echo string
  else begin
    Writeln(BuildString);
  end;                   
  CheckEOL;
end;

procedure TWAInterpreter.ExecReferrer;
begin
  // @referrer (off|on)
  case Next of
    T_OFF: if Assigned(FOnReferrer) then FOnReferrer(False);
    T_ON: if Assigned(FOnReferrer) then FOnReferrer(True);
    else Error(ERROR_BOOL);
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecRef;
begin
  // @ref "string"
  if Next <> T_STRING then Error(ERROR_STR);
  if Assigned(FOnRef) then FOnRef(BuildString);
  CheckEOL;
end;

procedure TWAInterpreter.ExecSet;
var key,value:String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  key := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  value := BuildString;
  SetVariable(key, value);
  CheckEOL;
end;

procedure TWAInterpreter.ExecCookies;
begin
  // @cookies (off|on)
  case Next of
    T_OFF: if Assigned(FOnCookies) then FOnCookies(False);
    T_ON: if Assigned(FOnCookies) then FOnCookies(True);
    else Error(ERROR_BOOL);
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecAgent;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  if Assigned(FOnAgent) then FOnAgent(BuildString);
  CheckEOL;
end;

function TWAInterpreter.ExecCSV: TCSVIterator;
var
 fields: TStringList;
 fn: String;
begin
  // ... csv "file" defined {struct,x,y,...}
  if Next <> T_STRING then Error(ERROR_FILE);
  fn := BuildString;
  if Next <> T_DEFINED then Error('Expected "defined" keyword');
  if Next <> T_LBRACE then Error('Expected "{"');
  Next;
  fields := TStringList.Create;
  if FTok <> T_STRUCTVAR then Error('Structure variable expected');
  fields.Add(lowercase(FParser^.GetString));
  Next;
  while FTok = T_COMMA do begin
    if Next <> T_STRUCTVAR then Error('Structure variable expected');
    fields.Add(lowercase(FParser^.GetString));
    Next;  // either ',' or '}'
  end;
  if FTok <> T_RBRACE then Error('Expected "}"');
  Result := nil;
  try
    Result := TCSVIterator.Create(fn, fields);
  except on E: Exception do
    Error('Cannot open file "'+Fn+'"');
  end;
end;

function TWAInterpreter.ExecRealCSV: TRealCSVIterator;
var
 fields: TStringList;
 fn: String;
begin
  // ... real-csv "file" [defined {struct,x,y,...}]
  if Next <> T_STRING then Error(ERROR_FILE);
  fn := BuildString;
  fields := TStringList.Create;
  if FParser^.Peek = T_DEFINED then begin
    Next; // "defined"
    if Next <> T_LBRACE then Error('Expected "{"');
    Next;
    if FTok <> T_STRUCTVAR then Error('Structure variable expected');
    fields.Add(lowercase(FParser^.GetString));
    Next;
    while FTok = T_COMMA do begin
      if Next <> T_STRUCTVAR then Error('Structure variable expected');
      fields.Add(lowercase(FParser^.GetString));
      Next;  // either ',' or '}'
    end;
    if FTok <> T_RBRACE then Error('Expected "}"');
  end;
  Result := nil;
  try
    Result := TRealCSVIterator.Create(fn, fields);
  except on E: Exception do
    Error('Cannot open file "'+Fn+'"');
  end;
end;

function TWAInterpreter.ExecText: TTextIterator;
var fn,vname: String;
begin
  // ... text "file" defined "line"
  if Next <> T_STRING then Error(ERROR_FILE);
  fn := BuildString;
  if Next <> T_DEFINED then Error('Expected "defined" keyword');
  if Next <> T_STRING then Error(ERROR_STR);
  vname := BuildString;
  Result := nil;
  try
    Result := TTextIterator.Create(fn, vname);
  except on E: Exception do
    Error('Cannot open file "'+Fn+'"');
  end;
end;

procedure TWAInterpreter.ExecLookup;
var tfn,key,value: String;
    fields: TStringList;
begin
  // @lookup "table" defined {...} where "loadvar" is "savevar"
  if Next <> T_STRING then Error(ERROR_FILE);
  tfn := BuildString;
  if Next <> T_DEFINED then Error('Expected "defined" keyword');

  if Next <> T_LBRACE then Error('Expected "{"');
  Next;
  fields := TStringList.Create;
  if FTok <> T_STRUCTVAR then Error('Structure variable expected');
  fields.Add(lowercase(FParser^.GetString));
  Next;
  while FTok = T_COMMA do begin
    if Next <> T_STRUCTVAR then Error('Structure variable expected');
    fields.Add(lowercase(FParser^.GetString));
    Next;  // either ',' or '}'
  end;
  if FTok <> T_RBRACE then Error('Expected "}"');
  if Next <> T_WHERE then Error('Expected "where" keyword');
  if Next <> T_STRING then Error(ERROR_STR);
  // Key to lookup
  key := lowercase(BuildString);
  // Expect "is"
  if Next <> T_IS then Error('Expected "is" keyword');

  // Value to search in key column
  if Next <> T_STRING then Error(ERROR_STR);
  value := lowercase(BuildString);
  DoLookup(tfn, fields, key, value);

  CheckEOL;
end;

procedure TWAInterpreter.ExecVerbose;
begin
  case Next of
    T_OFF: FVerbose := False;
    T_ON: FVerbose := True;
    else Error(ERROR_BOOL);
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecCookie;
var k,v: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  k := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  v := BuildString;
  if Assigned(FOnSetCookie) then FOnSetCookie(k, v);
  CheckEOL;
end;

procedure TWAInterpreter.ExecScrape;
var s1,s2,name:String;
begin
  // @scrape "start" "end" "name"
  if Next <> T_STRING then Error(ERROR_STR);
  s1 := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  s2 := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  name := BuildString;

  if Assigned(FOnScrape) then begin
    SetVariable(name, FOnScrape(s1, s2));
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecFSLookup;
var path, cmp, id: String;
begin
  // @fslookup "*.*" "similarity-keyword" "id"
  if Next <> T_STRING then Error(ERROR_STR);
  path := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  cmp := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  id := BuildString;

  SetVariable(id, DoFSLookup(path, cmp));
  CheckEOL;
end;

procedure TWAInterpreter.ExecMail;
var m: TMailInfo;
begin
  // mail "to" "subject" "template_file"
  m.SMTPServer := FConfig.GetData('MailServer');
  m.SMTPUser := FConfig.GetData('MailUsername');
  m.SMTPPass := FConfig.GetData('MailPassword');
  m.SMTPPort := FConfig.GetData('MailPort');
  m.MailFrom := FConfig.GetData('MailFrom');
  if Next <> T_STRING then Error(ERROR_STR);
  m.MailTo := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  m.MailSubject := BuildString;
  if Next <> T_STRING then Error(ERROR_STR);
  m.MailBody := BuildTemplate(BuildString);

  if Assigned(FOnMail) then begin
    if FVerbose then begin
      VMsg('SMTP Send - TO:%s, FROM:%s, SUBJ:"%s", BODYSIZE:%d', [m.MailTo, m.MailFrom, m.MailSubject, Length(m.MailBody)]);
    end;
    FOnMail(m);
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecGet;
var rqst: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  if Assigned(FOnGet) then begin
    rqst := BuildString;
    if FVerbose then begin
      VMsg('HTTP GET: `%s`', [rqst]);
    end;
    FOnGet(rqst);
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecPost;
var rqst: String;
begin
  if Next <> T_STRING then Error(ERROR_STR);
  if Assigned(FOnPost) then begin
    rqst := BuildString;
    if FVerbose then begin
      VMsg('HTTP POST: `%s`', [rqst]);
    end;
    FOnPost(rqst);
  end;
  CheckEOL;
end;

procedure TWAInterpreter.ExecForeach;
var iter: TBaseIterator;
begin
  // foreach {
  case Next of
    T_CSV: iter := ExecCSV;
    T_REALCSV: iter := ExecRealCSV;
    T_TEXT: iter := ExecText;
  end;
  if Next <> T_LBRACE then Error('Expected "{"');
  CheckEOL;
  FParser^.NextLine;
  Iterate(iter);
  //if Next <> T_RBRACE then Error('Expected "}"');
end;

procedure TWAInterpreter.SetVariable (N, V: String);
var len: Integer;
begin
  if (FConfig.Exists('StringLengthLimit')) then begin
    try
      len := StrToInt(FConfig.GetData('StringLengthLimit'));
      if (len > 0) and (Length(V) > len) then
        Error(Format('Length of `%s` exceeds StringLengthLimit config (%d bytes)', [N, len]))
      else
        FVars.Add(N, V);
    except on E: EConvertError do Error('StringLengthLimit config contains non-integer value');
    end;
  end;
end;

function TWAInterpreter.GetVariable (N: String): String;
begin
  Result := FVars.GetData(N);
end;

function TWAInterpreter.GetConfig (N: String): String;
begin
  Result := FConfig.GetData(N);
end;

end.
