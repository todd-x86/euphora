program euphora;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  RegExpr,
  HTTPApp,
  AlStringList,
  HtmlParser,
  StrUtils,
  Classes,
  DomCore,
  AlWinHttpClient,
  AlHttpClient,
  AlFcnMime,
  DateUtils,
  Math,
  AlMultiPartFormDataParser,
  AlHttpCommon,
  OverbyteIcsHttpProt,
  OverbyteIcsSmtpProt,
  parser in 'parser.pas',
  interpreter in 'interpreter.pas',
  hashmap in 'hashmap.pas',
  iterators in 'iterators.pas',
  lookup in 'lookup.pas',
  csv in 'csv.pas',
  frmPreview in 'frmPreview.pas' {frmUIPreview};

const
  VERSION = '1.7.1';

var
 cmd: TAlWinHttpClient;
 mail: TSyncSmtpCli;
 wa: TWAInterpreter;
 membuf: TMemoryStream;
 hdr: TALHTTPResponseHeader;
 files: TALMultiPartFormDataContents;
 NoRef: Boolean;
 postdata: TALStringList;
 LastRef: String;
 tmpj: Integer;
 Auth: Boolean;
 ShowProgress: Boolean;
 PSwirlChars: array[0..3] of Char;
 PSwirlPos: Integer;
 LastRead: Integer;
 LastTime: TDateTime;

procedure PrintProgress;
var BarStr: String;
    Pcnt, Speed: Single;
    Metric: Char;
    j, TimeDiff, PPoint: Integer;
begin
  TimeDiff := MilliSecondsBetween(Now, LastTime);
  if (wa.DownloadTotal < 1) or (TimeDiff < 250) then begin
    //LastRead := wa.DownloadRead;
    //LastTime := Now;
    Exit;
  end;

  Pcnt := wa.DownloadRead/wa.DownloadTotal * 100;
  BarStr := '';
  PPoint := (Trunc(Ceil(Pcnt)) div 5);
  for j := 0 to PPoint-1 do begin
    BarStr := BarStr + '+';
  end;
  for j := PPoint to 19 do begin
    BarStr := BarStr + '-';
  end;

  Speed := ((wa.DownloadRead - LastRead) / TimeDiff) * 1000;
  Metric := 'B';
  if Speed >= 1024*1024*1024 then begin
    Speed := Speed / (1024*1024*1024);
    Metric := 'G';
  end else if Speed >= 1024*1024 then begin
    Speed := Speed / (1024*1024);
    Metric := 'M';
  end else if Speed >= 1024 then begin
    Speed := Speed / 1024;
    Metric := 'K';
  end;
  PSwirlPos := (PSwirlPos + 1) mod 4;
  LastRead := wa.DownloadRead;
  LastTime := Now;
  Write(Format('[%s] [%s] %.1f%% -- %.2f %s/s', [PSwirlChars[PSwirlPos], BarStr, Pcnt, Speed, Metric])+#13);
end;

procedure ShowHelp;
var cmd: String;
begin
  Writeln(Format('Euphora - Version %s', [VERSION]));
  Writeln('An automated web browsing language & interpreter');
  Writeln('(C) 2013-2016 Data Components Software');
  Writeln;
  Writeln('- Live Interpreter (Type "@quit" to quit) -');

  wa.BeginImmediate;

  Write('>> ');
  Readln(cmd);
  while cmd <> '' do begin
    try
      wa.Interpret(cmd);
    except on E: Exception do
      Writeln('Internal Error: '+E.Message);
    end;
    Write('>> ');
    Readln(cmd);
  end;
end;

procedure CheckRef;
begin
  if NoRef then cmd.RequestHeader.Referer := ''
  else cmd.RequestHeader.Referer := LastRef;
end;

procedure Preview(const Strings: TStringList);
begin
  membuf.Seek(0, soBeginning);
  Strings.LoadFromStream(membuf);
end;

procedure GetURL (const URL: String);
begin
  CheckRef;
  membuf.Clear;

  if hdr <> nil then hdr.Free;
  hdr := TALHTTPResponseHeader.Create;

  // TODO: Move GetConfigs to a persisted Auth object
  if Auth then begin
    cmd.RequestHeader.Authorization := 'Basic ' + EncodeStr(encBase64, wa.GetConfig('AuthUsername')+':'+wa.GetConfig('AuthPassword'));
  end;

  try
    cmd.Get(URL, membuf, hdr);
  except on E: EALHTTPClientException do
    if E.StatusCode <> 303 then Writeln('Warning: '+E.Message);
  end;

  LastRef := URL;
end;

procedure PostURL (const URL: String);
begin
  CheckRef;
  membuf.Clear;

  if hdr <> nil then hdr.Free;
  hdr := TALHTTPResponseHeader.Create;

  // TODO: Move GetConfigs to a persisted Auth object
  if Auth then begin
    cmd.RequestHeader.Authorization := 'Basic ' + EncodeStr(encBase64, wa.GetConfig('AuthUsername')+':'+wa.GetVariable('AuthPassword'));
  end;

  if files.Count > 0 then
    try
      cmd.PostMultiPartFormData(URL, postdata, files, membuf, hdr);
    except on E: EALHTTPClientException do
      if E.StatusCode <> 303 then Writeln('Warning: '+E.Message);
    end
  else
    try
      cmd.RequestHeader.ContentType := 'application/x-www-form-urlencoded';
      cmd.PostUrlEncoded(URL, postdata, membuf, hdr);
    except on E: EALHTTPClientException do
      if E.StatusCode <> 303 then Writeln('Warning: '+E.Message);
    end;

  LastRef := URL;
  files.Clear;
  postdata.Clear;
end;

procedure ToggleRef (const B: Boolean);
begin
  NoRef := not B;
end;

procedure SetReferrer (const S: String);
begin
  LastRef := S;
end;

procedure ToggleCookies (const B: Boolean);
begin
  if not B then
    cmd.InternetOptions := cmd.InternetOptions + [wHttpIo_NO_COOKIES]
  else
    cmd.InternetOptions := cmd.InternetOptions - [wHttpIo_NO_COOKIES];
end;

procedure SetAgent (const S: String);
begin
  cmd.RequestHeader.UserAgent := S;
end;

procedure SetCookie (const Key, Value: String);
begin
  cmd.RequestHeader.Cookies.Add(Key+'='+Value);
end;

procedure OnRegex(var Regex: TRegExpr);
var buf: String;
begin
  SetLength(buf, membuf.Size);
  buf := StrPas(membuf.Memory);
  Regex.Exec(buf);
end;

function ScrapeData (const Start, Stop: String): String;
var buf: String;
    a,b:Integer;
begin
  SetLength(buf, membuf.Size);
  buf := StrPas(membuf.Memory);
  Result := '';
  a := Pos(Start, buf);
  if a > 0 then begin
    b := PosEx(Stop, buf, a+length(Start));
    if b > 0 then begin
      Result := Copy(buf, a+length(Start), b-a-length(Start));
    end;
  end;
end;

procedure DoMail (var MailInfo: TMailInfo);
begin
  mail := TSyncSmtpCli.Create(nil);
  mail.Port := MailInfo.SMTPPort;
  mail.AuthType := smtpAuthNone;
  mail.Host := MailInfo.SMTPServer;
  mail.HdrSender := MailInfo.MailFrom;
  mail.XMailer := Format('Euphora/%s', [VERSION]);
  mail.Username := MailInfo.SMTPUser;
  mail.Password := MailInfo.SMTPPass;
  if not mail.ConnectSync then begin
    Writeln('ERROR: Cannot connect to SMTP server');
    Exit;
  end;
  if not mail.HeloSync then begin
    Writeln('ERROR: SMTP Server rejected HELO command');
    Exit;
  end;
  mail.RcptNameAdd(MailInfo.MailTo, '', '');
  mail.HdrFrom := MailInfo.MailFrom;
  mail.HdrSubject := MailInfo.MailSubject;
  mail.MailMessage.Text := MailInfo.MailBody;
  if not mail.MailSync then begin
    Writeln('ERROR: SMTP server failed to send message');
    Exit;
  end;

  mail.Free;
end;

procedure PostField (const Key, Value: String);
begin
  postdata.Add(Key+'='+Value);
end;

function ParseHTML (const HTMLName: String): String;
var parser: THtmlParser;
    doc: TDocument;
    nl: TNodeList;
    j: Integer;
    buf: String;
begin;
  SetLength(buf, membuf.Size);
  buf := StrPas(membuf.Memory);
  parser := THtmlParser.Create;
  doc := parser.parseString(buf);
  Result := '';
  nl := doc.getElementsByTagName('input');
  for j := 0 to nl.length-1 do begin
    if nl.item(j).attributes.getNamedItem('name') <> nil then begin
      if nl.item(j).attributes.getNamedItem('name').childNodes.item(0).nodeValue = HTMLName then
        Result := nl.item(j).attributes.getNamedItem('value').childNodes.item(0).nodeValue;
    end;
  end;
end;

procedure AddPostFile (const ID, Fn: String);
var mpfd: TALMultiPartFormDataContent;
begin
  mpfd := TALMultiPartFormDataContent.Create;
  TMemoryStream(mpfd.DataStream).LoadFromFile(fn);
  mpfd.ContentDisposition := 'form-data; name="'+ID+'"; filename="'+Fn+'"';
  mpfd.ContentType := ALGetDefaultMIMEContentTypeFromExt(ExtractFileExt(fn));
  files.Add(mpfd);
end;

procedure SetConfig (const Key, Value: String);
begin
  if Key = 'ProxyServer' then begin
    cmd.ProxyParams.ProxyBypass := Value;
    cmd.ProxyParams.ProxyServer := Value;
    cmd.AccessType := wHttpAt_NAMED_PROXY;
  end else if Key = 'ProxyUsername' then begin
    cmd.ProxyParams.ProxyUserName := Value;
  end else if Key = 'ProxyPassword' then begin
    cmd.ProxyParams.ProxyPassword := Value;
  end else if Key = 'ProxyPort' then begin
    cmd.ProxyParams.ProxyPort := StrToInt(Value);
  end else if Key = 'ProxyName' then begin
    cmd.ProxyParams.ProxyBypass := Value;
  end else if (Key = 'Download_ProgressBar') then begin
    if (Lowercase(Value) = 'true') then
      wa.OnDownloadBar := PrintProgress
    else
      wa.OnDownloadBar := nil;
  end;
end;

procedure DownloadURL (const URL, Fn: String; const ProgressBar: Boolean);
var fs: TFileStream;
begin
  CheckRef;

  if Fn = '' then begin
    Writeln('ERROR: Cannot download file to empty filename');
    Exit;
  end;

  try
    fs := TFileStream.Create(fn, fmCreate or fmOpenWrite or fmShareDenyNone);
    try
      LastTime := Now;
      cmd.Get(URL, fs, hdr);
      if Lowercase(wa.GetConfig('Download_ProgressBar')) = 'true' then
        Writeln(Format('[%s] [++++++++++++++++++++] 100%%                  ', [PSwirlChars[PSwirlPos]]));
    except on E: EALHTTPClientException do
      if E.StatusCode <> 303 then Writeln('Warning: '+E.Message);
    end;
    fs.Free;
  except on E: Exception do
    Writeln('ERROR: '+E.Message);
  end;
end;

procedure DoAuth (const Authorization: Boolean);
begin
  Auth := Authorization;
end;

procedure DumpFile (const F: String);
begin
  membuf.SaveToFile(F);
end;

function EncodeStr (const S: String): String;
begin
  Result := HTTPEncode(S);
end;

begin
  PSwirlChars[0] := '/';
  PSwirlChars[1] := '-';
  PSwirlChars[2] := '\';
  PSwirlChars[3] := '|';


  NoRef := False;
  postdata := TAlStringList.Create;
  cmd := TAlWinHttpClient.Create(nil);
  cmd.RequestHeader.UserAgent := Format('Euphora/%s (Windows)', [VERSION]);
  cmd.RequestHeader.Accept := 'text/html, */*';
  cmd.InternetOptions := [];
  hdr := nil;
  files := TALMultiPartFormDataContents.Create;
  membuf := TMemoryStream.Create;
  Auth := False;

  wa := TWAInterpreter.Create;

  for tmpj := 2 to ParamCount do begin
    wa.SetVariable('_arg'+IntToStr(tmpj-2), ParamStr(tmpj));
  end;

  wa.OnRegex := OnRegex;
  wa.OnParse := ParseHTML;
  wa.OnEncode := EncodeStr;
  wa.OnGet := GetURL;
  wa.OnPost := PostURL;
  wa.OnReferrer := ToggleRef;
  wa.OnRef := SetReferrer;
  wa.OnCookies := ToggleCookies;
  wa.OnAgent := SetAgent;
  wa.OnSetCookie := SetCookie;
  wa.OnScrape := ScrapeData;
  wa.OnMail := DoMail;
  wa.OnField := PostField;
  wa.OnFile := AddPostFile;
  wa.OnDownload := DownloadURL;
  wa.OnDump := DumpFile;
  wa.OnAuth := DoAuth;
  wa.OnConfig := SetConfig;
  wa.OnPreview := Preview;
  cmd.OnDownloadProgress := wa.OnDownloadProgress;

  if (ParamStr(1) = '') or (not FileExists(ParamStr(1))) then begin
    ShowHelp;
    Halt;
  end else begin
    wa.Execute(ParamStr(1));
  end;
end.
