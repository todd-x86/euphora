unit csv;

interface

uses SysUtils, Classes;

type
  PStringList = ^TStringList;

function CSVSplit (S: String): PStringList;

implementation

function CSVSplit (S: String): PStringList;
var j,st: Integer;
begin
  New(Result);
  Result^ := TStringList.Create;
  j := 1;

  while (j <= Length(S)) do begin
    // Skip whitespace
    while (j <= Length(S)) and ((S[j] = #9) or (S[j] = ' ')) do Inc(j);

    // Determine field type (quoted, non-quoted)
    if S[j] = '"' then begin
      // Read until '"'
      Inc(j);
      st := j;
      while (j <= Length(S)) and (S[j] <> '"') do Inc(j);

      if j > Length(S) then raise Exception.Create('CSV field is missing end quote');

      Result^.Add(Copy(S, st, j-st));
      Inc(j);

      // Skip more whitespace
      while (j <= Length(S)) and ((S[j] = #9) or (S[j] = ' ')) do Inc(j);

      if (j <= Length(S)) and (S[j] <> ',') then raise Exception.Create('CSV field missing comma delimiter');

      Inc(j);  // Skip comma (if any)
    end else begin
      // Read until EOL or ','

      // Skip whitespace
      while (j <= Length(S)) and ((S[j] = #9) or (S[j] = ' ')) do Inc(j);

      st := j;
      while (j <= Length(S)) and (S[j] <> ',') do Inc(j);

      // Add if ',' or EOL
      if (j > Length(S)) or (S[j] = ',') then Result^.Add(Copy(S, st, j-st));

      Inc(j); // skip comma (if any)
    end;
  end;
end;

end.
