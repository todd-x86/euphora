unit lookup;

interface

uses Classes, SysUtils, iterators, strsim, csv;

type
  TCSVLookup = class(TObject)
  private
    FData: PStringList;
    function GetData (F: Integer): String;
  public
    constructor Create (fn: String; key: Integer; value: String);
    destructor Destroy; override;
    property Data[Index: Integer]: String read GetData;
  end;

implementation

constructor TCSVLookup.Create (fn: String; key: Integer; value: String);
var maxset,tmp: PStringList;
    maxscore,diff: Single;
    FCSV: TStringList;
    j:Integer;
begin
  inherited Create;
  FCSV := TStringList.Create;
  FCSV.LoadFromFile(fn);
  maxset := nil;
  maxscore := -1;
  for j := 0 to FCSV.Count-1 do begin
    tmp := CSVSplit(FCSV.Strings[j]);
    diff := StrDiff(tmp.Strings[key], value);
    // Store the one that relates best to comparison value
    if diff > maxscore then begin
      if maxset <> nil then begin
        maxset^.Free;
        FreeMem(maxset);
      end;
      maxset := tmp;
      maxscore := diff;
    end else begin
      tmp^.Free;
      FreeMem(tmp);
    end;
  end;
  FCSV.Free;
  FData := maxset;
end;

destructor TCSVLookup.Destroy;
begin
  if FData <> nil then begin
    FData^.Free;
  end;
  inherited Destroy;
end;

function TCSVLookup.GetData (F: Integer): String;
begin
  Result := FData^.Strings[F];
end;

end.
