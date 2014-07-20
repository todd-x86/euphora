unit hashmap;

interface

uses Classes, SysUtils, Contnrs;

type
  TStringHashTable = class(TObject)
  private
    FNames: TStringList;
    FVals: TStringList;
  public
    constructor Create;
    function Add (const S, Data: String): Cardinal;
    function Exists (const S: String): Boolean;
    function GetData (const S: String): String;
  end;

  TObjectHashTable = class(TObject)
  private
    FNames: TStringList;
    FVals: TObjectList;
  public
    constructor Create;
    function Add (const S: String; Data: TObject): Cardinal;
    function Exists (const S: String): Boolean;
    function GetData (const S: String): TObject;
  end;

implementation

{ TStringHashTable}

constructor TStringHashTable.Create;
begin
  FNames := TStringList.Create;
  FVals := TStringList.Create;
end;

function TStringHashTable.Exists (const S: String): Boolean;
begin
  Result := FNames.IndexOf(S) >= 0;
end;

function TStringHashTable.Add (const S, Data: String): Cardinal;
var j: Integer;
begin
  j := FNames.IndexOf(S);
  if j < 0 then begin
    FNames.Add(S);
    Result := FVals.Add(Data);
  end else begin
    FVals.Strings[j] := Data;
    Result := j;
  end;
end;

function TStringHashTable.GetData (const S: String): String;
var j: Integer;
begin
  j := FNames.IndexOf(S);
  if j < 0 then begin
    Result := '';
  end else begin
    Result := FVals.Strings[j];
  end;
end;

{ TObjectHashTable }

constructor TObjectHashTable.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
  FVals := TObjectList.Create(True);
end;

function TObjectHashTable.Add (const S: String; Data: TObject): Cardinal;
var j: Integer;
begin
  j := FNames.IndexOf(S);
  if j < 0 then begin
    FNames.Add(S);
    Result := FVals.Add(Data);
  end else begin
    FVals.Items[j] := Data;
    Result := j;
  end;
end;

function TObjectHashTable.Exists (const S: String): Boolean;
begin
  Result := FNames.IndexOf(S) >= 0;
end;

function TObjectHashTable.GetData (const S: String): TObject;
var j: Integer;
begin
  j := FNames.IndexOf(S);
  if j < 0 then begin
    Result := nil;
  end else begin
    Result := FVals.Items[j];
  end;
end;

end.
