unit hashmap;

interface

uses Classes, SysUtils, Contnrs, Math;

type
  THashNode = packed record
    Key: String;
    case ValueType: Byte of
      0: (ValueString: PChar);
      1: (ValueObject: TObject);
  end;

  PHashNode = ^THashNode;

  TAbstractHashTable = class(TObject)
  protected
    FContents: array of PHashNode;
    FSize: Cardinal;
    FUsed: Cardinal;
    function AddNode (N: PHashNode): Cardinal;
    function GetIndex (const Key: String): Cardinal;
    function Find (const S: String): PHashNode;
    procedure Resize;
  public
    constructor Create;
    destructor Destroy; override;

    function Exists (const S: String): Boolean;
  end;

  TStringHashTable = class(TAbstractHashTable)
  public
    function Add (const S, Data: String): Cardinal;
    function GetData (const S: String): String;
  end;

  TObjectHashTable = class(TAbstractHashTable)
  public
    function Add (const S: String; Data: TObject): Cardinal;
    function GetData (const S: String): TObject;
  end;

function KeyHash (const Key: String): Cardinal;

implementation

function KeyHash (const Key: String): Cardinal;
var g,j: Cardinal;
begin
  // PJW Hashing Function
  Result := 0;
  for j := 1 to Length(Key) do begin
    Result := (Result shl 4) + Ord(Key[j]);
    g := Result and $f0000000;
    if g <> 0 then begin
      Result := Result xor (g shr 24);
      Result := Result xor g;
    end;
  end;
end;

{ TAbstractHashTable }

constructor TAbstractHashTable.Create;
begin
  inherited Create;
  FUsed := 0;
  FSize := 4;
  Resize;
end;

destructor TAbstractHashTable.Destroy;
var j: Cardinal;
begin
  // FSize must be at least 1, or j will loop until MAX_INT
  for j := 0 to FSize-1 do
    if (FContents[j] <> nil) then begin
      if FContents[j]^.ValueType = 1 then
        FContents[j]^.ValueObject.Free;
      FreeMem(FContents[j]);
    end;
  SetLength(FContents, 0);
end;

function TAbstractHashTable.GetIndex (const Key: String): Cardinal;
begin
  Result := KeyHash(Key) mod FSize;
end;

function TAbstractHashTable.AddNode (N: PHashNode): Cardinal;
var j: Cardinal;
begin
  j := GetIndex(N^.Key);

  // Find the next cell available (nothing fancy...)
  if (FContents[j] <> nil) and (FContents[j]^.Key <> N^.Key) then begin
    while (FContents[j] <> nil) and (FContents[j]^.Key <> N^.Key) do
      j := (j+1) mod FSize;
  end;
  if (FContents[j] = nil) then Inc(FUsed);
  FContents[j] := N;

  Result := j;

  // Resize if necessary (Result will be blown away...)
  if FUsed/FSize > 4/5 then Resize;
end;

procedure TAbstractHashTable.Resize;
var j,sz: Cardinal;
    ptrs: TList;
begin
  ptrs := TList.Create;
  sz := FSize;
  // Resize flag toggles each time a resize is performed

  FSize := Trunc(FSize * 1.66);
  FUsed := 0;
  SetLength(FContents, FSize);

  // Clear memory temporarily
  for j := 0 to sz-1 do begin
    if FContents[j] <> nil then begin
      ptrs.Add(FContents[j]);
      FContents[j] := nil;
    end;
  end;

  // Re-insert old nodes
  if ptrs.Count > 0 then begin
    for j := ptrs.Count-1 downto 0 do begin
      AddNode(ptrs[j]);
    end;
  end;

  ptrs.Free;
end;

function TAbstractHashTable.Find (const S: String): PHashNode;
var j: Cardinal;
begin
  j := GetIndex(S);
  while (FContents[j] <> nil) and (FContents[j]^.Key <> S) do begin
    j := (j+1) mod FSize;
  end;

  Result := FContents[j];
end;

function TAbstractHashTable.Exists (const S: String): Boolean;
begin
  Result := Find(S) <> nil;
end;

{ TStringHashTable }

function TStringHashTable.Add (const S, Data: String): Cardinal;
var p: PHashNode;
    strData: PChar;
begin
  New(p);
  GetMem(strData, Sizeof(Char)*(Length(Data)+1));
  StrPCopy(strData, Data);
  p^.Key := S;
  p^.ValueType := 0;
  p^.ValueString := strData;
  Result := AddNode(p);
end;

function TStringHashTable.GetData (const S: String): String;
var p: PHashNode;
begin
  p := Find(S);
  if p = nil then Result := ''
  else Result := p^.ValueString;
end;

{ TObjectHashTable }

function TObjectHashTable.Add (const S: String; Data: TObject): Cardinal;
var p: PHashNode;
begin
  New(p);
  p^.Key := S;
  p^.ValueType := 1;
  p^.ValueObject := Data;
  Result := AddNode(p);
end;

function TObjectHashTable.GetData (const S: String): TObject;
var p: PHashNode;
begin
  p := Find(S);
  if p = nil then Result := nil
  else Result := p^.ValueObject;
end;

end.
