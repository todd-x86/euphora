unit csv;

interface

uses SysUtils, Classes;

type
  PStringList = ^TStringList;

function CSVSplit (S: String): PStringList;

implementation

function CSVSplit (S: String): PStringList;
var j,st: Integer;
    tmp: String;
begin
  New(Result);
  Result^ := TStringList.Create;
  tmp := '';
  j := 1;
  st := 0;
  while j <= Length(S) do begin
    st := j;
    while (j <= Length(S)) and (S[j] <> ',') do Inc(j);
    if j <= Length(S) then Result^.Add(Trim(Copy(S, st, j-st)));
    Inc(j);
  end;
  if j > 1 then Result^.Add(Trim(Copy(S, st, j-st)));
end;

end.
