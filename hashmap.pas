unit hashmap;

interface

uses Classes, SysUtils;

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

end.
