unit strsim;

interface

uses Classes, SysUtils, Math;

type
  TStrPair = String[2];

  PStrPair = ^TStrPair;

function StrDiff (S1, S2: String): Single;
function wlPairs (S: String): TList;
function wPairs (S: String): TList;
function Split (S: String): TStringList;
function IsWS (C: Char): Boolean;

implementation

function wPairs (S: String): TList;
var j: Integer;
    tmp: PStrPair;
begin
  Result := TList.Create;
  if Length(S) = 1 then begin
    New(tmp);
    tmp^ := S[1];
    Result.Add(tmp);
  end;
  for j := 1 to Length(S)-1 do begin
    New(tmp);
    tmp^ := S[j]+S[j+1];
    Result.Add(tmp);
  end;
end;

function IsWS (C: Char): Boolean;
begin
  Result := (C = ' ') or (C = #9) or (C = #10) or (C = #13);
end;

function Split (S: String): TStringList;
var j: Integer;
    tmp: String;
begin
  Result := TStringList.Create;
  j := 1;
  while j <= length(S) do begin
    while (j <= Length(S)) and (IsWS(S[j])) do Inc(j);
    tmp := '';
    while (j <= Length(S)) and (not IsWS(S[j])) do begin
      tmp := tmp + S[j];
      Inc(j);
    end;
    if tmp <> '' then Result.Add(tmp);
  end;
end;

function wlPairs (S: String): TList;
var w: TStringList;
    j,k: Integer;
    lst: TList;
begin
  Result := TList.Create;
  w := Split(S);
  for j := 0 to w.Count-1 do begin
    lst := wPairs(w.Strings[j]);
    for k := 0 to lst.Count-1 do begin
      Result.Add(lst.Items[k]);
    end;
    lst.Free;
  end;
  w.Free;
end;

function StrDiff (S1, S2: String): Single;
var p1, p2: TList;
    intersect,union,j,k: Integer;
    tmp1,tmp2:PStrPair;
begin
  p1 := wlPairs(lowercase(S1));
  p2 := wlPairs(lowercase(S2));
  intersect := 0;
  union := p1.Count+p2.Count;
  for j := 0 to p1.Count-1 do begin
    tmp1 := p1.Items[j];
    for k := 0 to p2.Count-1 do begin
      tmp2 := p2.Items[k];
      if tmp1^ = tmp2^ then begin
        Inc(intersect);
        p2.Delete(k);
        break;
      end;
    end;
  end;
  p1.Free;
  p2.Free;
  // Consider length of the two values compared
  if Length(S2) > 0 then
    Result := ((2*intersect)/union)
  else Result := 0;
end;

end.
