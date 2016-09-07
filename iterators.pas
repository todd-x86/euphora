unit iterators;

interface

uses Classes, SysUtils, RegExpr, csv;

type
  TBaseIterator = class(TObject)
  public
    function HasNext: Boolean; virtual;
    function NextValueSet: Pointer; virtual;
    function Eof: Boolean; virtual;
  end;

  // TTextIterator
  TTextIterator = class(TBaseIterator)
  private
    FCode: TStringList;
    FLine: Integer;
    FVar: String;
  public
    constructor Create (Fn: String; VarName: String);
    destructor Destroy; override;
    function NextValueSet: Pointer; override;
    function Eof: Boolean; override;
    function HasNext: Boolean; override;
    property VarName: String read FVar;
  end;

  // TCSVIterator
  TCSVIterator = class(TBaseIterator)
  private
    FFields: TStringList;
    FCode: TStringList;
    FLine: Integer;

    function GetField (Index: Integer): String;
  public
    constructor Create (Fn: String; var Fields: TStringList);
    destructor Destroy; override;
    function NextValueSet: Pointer; override;
    function Eof: Boolean; override;
    function HasNext: Boolean; override;
    property Fields[Index: Integer]: String read GetField;
  end;

  // TRealCSVIterator
  TRealCSVIterator = class(TBaseIterator)
  private
    FFields: TStringList;
    FCode: TStringList;
    FLine: Integer;

    function GetField (Index: Integer): String;
  public
    constructor Create (Fn: String; var Fields: TStringList);
    destructor Destroy; override;
    function NextValueSet: Pointer; override;
    function Eof: Boolean; override;
    function HasNext: Boolean; override;
    property Fields[Index: Integer]: String read GetField;
  end;

  // TRegexIterator
  TRegexIterator = class(TBaseIterator)
  private
    FRegEx: TRegExpr;
    FFields: TStringList;
    FCode: TStringList;
    FLine: Integer;

    function GetField (Index: Integer): String;
  public
    constructor Create (Regex, Filename: String; var Fields: TStringList);
    destructor Destroy; override;
    function NextValueSet: Pointer; override;
    function Eof: Boolean; override;
    function HasNext: Boolean; override;
    property Fields[Index: Integer]: String read GetField;
  end;

  // TRegexContextIterator
  TRegexContextIterator = class(TBaseIterator)
  private
    FRegEx: TRegExpr;
    FFields: TStringList;
    FCode: TStringList;
    FLine, FOffset: Integer;
    FMatch: Boolean;

    procedure FindNext;
    function GetField (Index: Integer): String;
  public
    constructor Create (Regex, Filename: String; var Fields: TStringList);
    destructor Destroy; override;
    function NextValueSet: Pointer; override;
    function Eof: Boolean; override;
    function HasNext: Boolean; override;
    property Fields[Index: Integer]: String read GetField;
  end;

implementation

function TBaseIterator.NextValueSet: Pointer;
begin
  Result := nil;
end;

function TBaseIterator.Eof: Boolean;
begin
  Result := True;
end;

function TBaseIterator.HasNext: Boolean;
begin
  Result := False;
end;

// TTextIterator

constructor TTextIterator.Create (Fn: String; VarName: String);
begin
  inherited Create;
  FCode := TStringList.Create;
  FCode.LoadFromFile(Fn);
  FVar := VarName;
  FLine := -1;
end;

destructor TTextIterator.Destroy;
begin
  FCode.Free;
  inherited Destroy;
end;

function TTextIterator.HasNext: Boolean;
begin
  Result := FLine+1 < FCode.Count-1;
end;

function TTextIterator.NextValueSet: Pointer; // PChar
begin
  if Eof then begin
    Result := nil;
    Exit;
  end;
  Inc(FLine);
  Result := AllocMem(Length(FCode.Strings[FLine])+1);
  FillChar(Result^, length(FCode.Strings[FLine])+1, 0);
  StrPCopy(Result, FCode.Strings[FLine]);
end;

function TTextIterator.Eof: Boolean;
begin
  Result := FLine >= FCode.Count;
end;

// TCSVIterator

constructor TCSVIterator.Create (Fn: String; var Fields: TStringList);
begin
  inherited Create;
  FCode := TStringList.Create;
  FLine := -1;
  FCode.LoadFromFile(fn);
  FFields := TStringList.Create;
  FFields.AddStrings(Fields);
end;

destructor TCSVIterator.Destroy;
begin
  FCode.Free;
  FFields.Free;
  inherited Destroy;
end;

function TCSVIterator.GetField (Index: Integer): String;
begin
  Result := FFields.Strings[Index];
end;

function TCSVIterator.HasNext: Boolean;
begin
  Result := FLine+1 < FCode.Count;
end;

function TCSVIterator.NextValueSet: Pointer;
//var sl: PStringList;
begin
  Inc(FLine);
  if Eof then begin
    Result := nil;
    Exit;
  end;

  //New(sl);
  // Might cause some damage depending on whether Delphi decides to GC this
  //sl^ := TStringList.Create;
  Result := CSVSplit(FCode.Strings[FLine]);
  //Result := sl;
end;

function TCSVIterator.Eof: Boolean;
begin
  Result := FLine >= FCode.Count;
end;

// TRealCSVIterator

constructor TRealCSVIterator.Create (Fn: String; var Fields: TStringList);
var fl: PStringList;
begin
  inherited Create;
  FCode := TStringList.Create;
  FLine := -1;
  FCode.LoadFromFile(fn);
  FFields := TStringList.Create;
  FFields.AddStrings(Fields);

  // Read first line
  Inc(FLine);

  // Add if no fields defined
  if FFields.Count = 0 then begin
    fl := CSVSplit(FCode.Strings[FLine]);
    FFields.AddStrings(fl^);
    fl^.Free;
  end;
end;

destructor TRealCSVIterator.Destroy;
begin
  FCode.Free;
  FFields.Free;
  inherited Destroy;
end;

function TRealCSVIterator.GetField (Index: Integer): String;
begin
  Result := FFields.Strings[Index];
end;

function TRealCSVIterator.HasNext: Boolean;
begin
  Result := FLine+1 < FCode.Count;
end;

function TRealCSVIterator.NextValueSet: Pointer;
//var sl: PStringList;
begin
  Inc(FLine);
  if Eof then begin
    Result := nil;
    Exit;
  end;

  //New(sl);
  // Might cause some damage depending on whether Delphi decides to GC this
  //sl^ := TStringList.Create;
  Result := CSVSplit(FCode.Strings[FLine]);
  //Result := sl;
end;

function TRealCSVIterator.Eof: Boolean;
begin
  Result := FLine >= FCode.Count;
end;

// TRegexIterator

constructor TRegexIterator.Create (Regex, Filename: String; var Fields: TStringList);
begin
  inherited Create;
  FRegEx := TRegExpr.Create;
  FRegEx.Expression := Regex;
  FCode := TStringList.Create;
  FCode.LoadFromFile(Filename);
  FLine := -1;
  FFields := TStringList.Create;
  FFields.AddStrings(Fields);
end;

destructor TRegexIterator.Destroy;
begin
  FCode.Free;
  FFields.Free;
  FRegex.Free;
  inherited Destroy;
end;

function TRegexIterator.GetField (Index: Integer): String;
begin
  Result := FFields.Strings[Index];
end;

function TRegexIterator.HasNext: Boolean;
begin
  if FLine < 0 then FLine := 0;

  if Eof then begin
    Result := False;
    Exit;
  end;

  Result := FRegex.Exec(FCode.Strings[FLine]);
  while (not Eof) and (not Result) do begin
    Inc(FLine);
    Result := FRegex.Exec(FCode.Strings[FLine]);
  end;
end;

function TRegexIterator.NextValueSet: Pointer;
var match: Boolean;
    results: PStringList;
    j: Integer;
begin
  if Eof then begin
    Result := nil;
    Exit;
  end;

  if FLine < 0 then FLine := 0;
  match := False;
  while (not Eof) and (not match) do begin
    match := FRegex.Exec(FCode.Strings[FLine]);
    Inc(FLine);
  end;
  if match then begin
    New(results);
    results^ := TStringList.Create;
    for j := 1 to FRegex.SubExprMatchCount do begin
      results^.Add(FRegex.Match[j]);
    end;
    Result := results;
  end else Result := nil;
end;

function TRegexIterator.Eof: Boolean;
begin
  Result := FLine >= FCode.Count-1;
end;

// TRegexContextIterator

constructor TRegexContextIterator.Create (Regex, Filename: String; var Fields: TStringList);
begin
  inherited Create;
  FRegEx := TRegExpr.Create;
  FRegEx.Expression := Regex;
  FCode := TStringList.Create;
  FCode.LoadFromFile(Filename);
  FLine := 0;
  FOffset := 1;
  FFields := TStringList.Create;
  FFields.AddStrings(Fields);
  FindNext;
end;

destructor TRegexContextIterator.Destroy;
begin
  FCode.Free;
  FFields.Free;
  FRegex.Free;
  inherited Destroy;
end;

function TRegexContextIterator.GetField (Index: Integer): String;
begin
  Result := FFields.Strings[Index];
end;

function TRegexContextIterator.HasNext: Boolean;
begin
  Result := FMatch;
end;

function TRegexContextIterator.NextValueSet: Pointer;
var results: PStringList;
    j: Integer;
begin
  if not FMatch then begin
    Result := nil;
    Exit;
  end;

  New(results);
  results^ := TStringList.Create;
  for j := 1 to FRegex.SubExprMatchCount do begin
    results^.Add(FRegex.Match[j]);
  end;
  Result := results;

  FindNext;
end;

function TRegexContextIterator.Eof: Boolean;
begin
  Result := not FMatch;
end;

procedure TRegexContextIterator.FindNext;
begin
  FMatch := False;

  while FLine < FCode.Count do begin
    FRegex.InputString := FCode[FLine];
    if (FOffset <= Length(FCode[FLine])) and (FRegex.Exec(FOffset)) then begin
      FMatch := True;
      FOffset := FRegex.MatchPos[0] + FRegex.MatchLen[0];
      Exit;
    end;

    Inc(FLine);
  end;
end;

end.
