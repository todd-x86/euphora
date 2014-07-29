unit parser;

interface

uses Classes, SysUtils;

type
  TPosInfo = packed record
    Line: Integer;
    Pos: Integer;
    St: Integer;
  end;

  PPosInfo = ^TPosInfo;

  TWAParser = class(TObject)
  private
    FCode: TStringList;
    FFile: String;
    FLine: Integer;
    FPos: Integer;
    FSt: Integer;
    FPosits: TList;
    function IsWS (C: Char): Boolean;
    function IsAlpha (C: Char): Boolean;
    function ParseKeyword: Integer;
    function ParseString: Integer;
    function ParseSymbol: Integer;
    function IsNumeric (C: Char): Boolean;
    function ParseNumber: Integer;
    procedure WS;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLine (const S: String);
    procedure InitParser;

    procedure LoadFromFile (const Fn: String);
    function GetToken: Integer;
    function GetString: String;
    function Peek: Integer;
    procedure NextLine;
    procedure PushInfo;
    procedure PopInfo;
    procedure Reload;

    property Line: Integer read FLine;
    property Char: Integer read FPos;
    property Filename: String read FFile;
  end;

  PWAParser = ^TWAParser;

const
  T_PREPROC = 0;
  T_GET = 1;
  T_POST = 2;
  T_FOREACH = 3;
  T_MAIL = 4;
  T_INCLUDE = 5;
  T_ECHO = 6;
  T_REFERRER = 7;
  T_REF = 8;
  T_SET = 9;
  T_COOKIES = 10;
  T_AGENT = 11;
  T_CSV = 12;
  T_TEXT = 13;
  T_LOOKUP = 14;
  T_VERBOSE = 15;
  T_COOKIE = 16;
  T_SCRAPE = 17;
  T_FILE = 18;
  T_DOWNLOAD = 19;
  T_STRING = 20;
  T_OFF = 21;
  T_ON = 22;
  T_LBRACE = 23;
  T_RBRACE = 24;
  T_COMMA = 25;
  T_STRUCTVAR = 26;
  T_DEFINED = 27;
  T_WHERE = 28;
  T_IS = 29;
  T_EOL = 30;
  T_EOF = 31;
  T_JUNK = 32;
  T_FSLOOKUP = 33;
  T_FIELD = 34;
  T_HTMLPARSE = 35;
  T_ENCODE = 36;
  T_BASENAME = 37;
  T_PAUSE = 38;
  T_SLEEP = 39;
  T_INTEGER = 40;
  T_DUMP = 41;
  T_PROMPT = 42;
  T_RUN = 43;
  T_QUIT = 44;
  T_FREADLN = 45;
  T_FSEEK = 46;
  T_FWRITELN = 47;
  T_REALCSV = 48;
  T_AUTH = 49;

implementation

constructor TWAParser.Create;
begin
  inherited Create;
  FCode := TStringList.Create;
  FPosits := TList.Create;
  FFile := 'memory://';
end;

destructor TWAParser.Destroy;
begin
  FPosits.Free;
  FCode.Free;
  inherited Destroy;
end;

function TWAParser.IsWS (C: Char): Boolean;
begin
  Result := (C = ' ') or (C = #9);
end;

function TWAParser.IsAlpha (C: Char): Boolean;
begin
  Result := ((Ord(C) >= Ord('a')) and (Ord(C) <= Ord('z'))) or ((Ord(C) >= Ord('A')) and (Ord(C) <= Ord('Z')));
end;

function TWAParser.IsNumeric (C: Char): Boolean;
begin
  Result := (Ord(C) >= Ord('0')) and (Ord(C) <= Ord('9'));
end;

procedure TWAParser.WS;
begin
  while (FPos <= Length(FCode.Strings[FLine])) and (IsWS(FCode.Strings[FLine][FPos])) do begin
    Inc(FPos);
  end;
end;

function TWAParser.GetString: String;
begin
  Result := Copy(FCode.Strings[FLine], FSt, FPos-FSt);
end;

function TWAParser.ParseKeyword: Integer;
var k: String;
begin
  FSt := FPos;
  while (FPos <= Length(FCode.Strings[FLine])) and ((IsAlpha(FCode.Strings[FLine][FPos])) or (IsNumeric(FCode.Strings[FLine][FPos])) or (FCode.Strings[FLine][FPos] = '_')) do begin
    Inc(FPos);
  end;
  if FPos > FSt then begin
    k := Lowercase(GetString);
    if k = 'include' then Result := T_INCLUDE
    else if k = 'echo' then Result := T_ECHO
    else if k = 'referrer' then Result := T_REFERRER
    else if k = 'ref' then Result := T_REF
    else if k = 'get' then Result := T_GET
    else if k = 'post' then Result := T_POST
    else if k = 'set' then Result := T_SET
    else if k = 'cookies' then Result := T_COOKIES
    else if k = 'agent' then Result := T_AGENT
    else if k = 'foreach' then Result := T_FOREACH
    else if k = 'csv' then Result := T_CSV
    else if k = 'text' then Result := T_TEXT
    else if k = 'lookup' then Result := T_LOOKUP
    else if k = 'mail' then Result := T_MAIL
    else if k = 'verbose' then Result := T_VERBOSE
    else if k = 'cookie' then Result := T_COOKIE
    else if k = 'scrape' then Result := T_SCRAPE
    else if k = 'file' then Result := T_FILE
    else if k = 'download' then Result := T_DOWNLOAD
    else if k = 'defined' then Result := T_DEFINED
    else if k = 'where' then Result := T_WHERE
    else if k = 'is' then Result := T_IS
    else if k = 'off' then Result := T_OFF
    else if k = 'on' then Result := T_ON
    else if k = 'fslookup' then Result := T_FSLOOKUP
    else if k = 'field' then Result := T_FIELD
    else if k = 'parse' then Result := T_HTMLPARSE
    else if k = 'encode' then Result := T_ENCODE
    else if k = 'basename' then Result := T_BASENAME
    else if k = 'pause' then Result := T_PAUSE
    else if k = 'sleep' then Result := T_SLEEP
    else if k = 'dump' then Result := T_DUMP
    else if k = 'prompt' then Result := T_PROMPT
    else if k = 'run' then Result := T_RUN
    else if k = 'quit' then Result := T_QUIT
    else if k = 'freadline' then Result := T_FREADLN
    else if k = 'fseek' then Result := T_FSEEK
    else if k = 'fwriteline' then Result := T_FWRITELN
    else if k = 'realcsv' then Result := T_REALCSV
    else if k = 'authorization' then Result := T_AUTH
    else Result := T_STRUCTVAR;
  end else Result := T_JUNK;
end;

procedure TWAParser.NextLine;
begin
  Inc(FLine);
  FPos := 1;
  FSt := 1;
  while (Peek <> T_EOF) and (Peek = T_EOL) do begin
    Inc(FLine);
  end;
end;

function TWAParser.ParseNumber: Integer;
begin
  if FPos > Length(FCode.Strings[FLine]) then begin
    Result := T_EOL;
  end else begin
    FSt := FPos;
    while (FPos <= Length(FCode.Strings[FLine])) and (IsNumeric(FCode.Strings[FLine][FPos])) do begin
      Inc(FPos);
    end;
    Result := T_INTEGER;
  end;
end;

function TWAParser.ParseString: Integer;
begin
  if FPos > Length(FCode.Strings[FLine]) then begin
    Result := T_EOL;
  end else if FCode.Strings[FLine][FPos] <> '"' then begin
    Result := T_JUNK;
  end else begin
    Inc(FPos);
    while (FPos <= Length(FCode.Strings[FLine])) and (FCode.Strings[FLine][FPos] <> '"') do begin
      if FCode.Strings[FLine][FPos] = '\' then Inc(FPos);
      Inc(FPos);
    end;
    if (FPos <= Length(FCode.Strings[FLine])) and (FCode.Strings[FLine][FPos] <> '"') then
      Result := T_JUNK
    else begin
      Result := T_STRING;
      Inc(FPos);
    end;
  end;
end;

function TWAParser.ParseSymbol: Integer;
begin
  FSt := FPos;
  case FCode.Strings[FLine][FPos] of
    '"': // string parsing
    begin
      Result := ParseString;
    end;
    '@':
    begin
      Result := T_PREPROC;
      Inc(FPos);
    end;
    '{':
    begin
      Result := T_LBRACE;
      Inc(FPos);
    end;
    '}':
    begin
      Result := T_RBRACE;
      Inc(FPos);
    end;
    ',':
    begin
      Result := T_COMMA;
      Inc(FPos);
    end;
    '/':
    begin
      if (FPos+1 <= Length(FCode.Strings[FLine])) and (FCode.Strings[FLine][FPos+1] = '/') then
        Result := T_EOL
      else
        Result := T_JUNK;
    end;
    else begin
      Result := T_JUNK;
      Inc(FPos);
    end;
  end;
end;

function TWAParser.GetToken: Integer;
begin
  if FLine >= FCode.Count then begin
    Result := T_EOF;
    Exit;
  end;
  WS;
  if FPos > Length(FCode.Strings[FLine]) then begin
    Result := T_EOL;
  end else begin
    if (IsAlpha(FCode.Strings[FLine][FPos])) or (FCode.Strings[FLine][FPos] = '_') then begin
      Result := ParseKeyword;
    end else if IsNumeric(FCode.Strings[FLine][FPos]) then begin
      Result := ParseNumber;
    end else begin
      Result := ParseSymbol;
    end;
  end;
end;

function TWAParser.Peek: Integer;
var tmpPos,tmpSt:Integer;
begin
  tmpPos := FPos;
  tmpSt := FSt;
  Result := GetToken;
  FPos := tmpPos;
  FSt := tmpSt;
end;

procedure TWAParser.InitParser;
begin
  FPos := 1;
  FLine := 0;
  FSt := 1;
end;

procedure TWAParser.SetLine (const S: String);
begin
  FCode.Clear;
  FCode.Add(S);
end;

procedure TWAParser.LoadFromFile (const Fn: String);
begin
  Self.FFile := Fn;
  FCode.LoadFromFile(Fn);
  InitParser;
end;

procedure TWAParser.PushInfo;
var p: PPosInfo;
begin
  New(p);
  p^.Line := FLine;
  p^.Pos := FPos;
  p^.St := FSt;
  FPosits.Add(p);
end;

procedure TWAParser.PopInfo;
begin
  //Reload;
  FPosits.Remove(FPosits.Last);
end;

procedure TWAParser.Reload;
var p: PPosInfo;
begin
  if FPosits.Count < 1 then raise Exception.Create('Parser info list is empty');
  p := FPosits.Last;
  FLine := p^.Line;
  FPos := p^.Pos;
  FSt := p^.St;
end;

end.
