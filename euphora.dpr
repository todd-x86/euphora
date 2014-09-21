program euphora;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  HTTPApp,
  AlStringList,
  HtmlParser,
  StrUtils,
  Classes,
  //Windows,
  DomCore,
  AlWinHttpClient,
  AlHttpClient,
  AlFcnMime,
  AlMultiPartFormDataParser,
  AlHttpCommon,
  OverbyteIcsHttpProt,
  OverbyteIcsSmtpProt,
  parser in 'parser.pas',
  interpreter in 'interpreter.pas',
  hashmap in 'hashmap.pas',
  iterators in 'iterators.pas',
  lookup in 'lookup.pas',
  csv in 'csv.pas';

const
  VERSION = '1.3.1';

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

procedure ShowHelp;
var cmd: String;
begin
  Writeln(Format('Euphora - Version %s', [VERSION]));
  Writeln('An automated web browsing language & interpreter');
  Writeln('(C) 2013-2014 Data Components Software');
  Writeln;
  Writeln('- Live Interpreter (Type "@quit" to quit) -');

  wa.BeginImmediate;

  Write('>> ');
  Readln(cmd);
  while cmd <> '' do begin
    wa.Interpret(cmd);
    Write('>> ');
    Readln(cmd);
  end;
end;

procedure CheckRef;
begin
  if NoRef then cmd.RequestHeader.Referer := ''
  else cmd.RequestHeader.Referer := LastRef;
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
//var j: Integer;
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
  {for j := 0 to files.Count-1 do
    if files.Items[j] <> nil then
      (files.Items[j] as TALMultiPartFormDataContent).DataStream.Free;}

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
  end;
end;

procedure DownloadURL (const URL, Fn: String);
var fs: TFileStream;
begin
  CheckRef;

  fs := TFileStream.Create(fn, fmCreate or fmOpenWrite or fmShareDenyNone);
  try
    cmd.Get(URL, fs, hdr);
  except on E: EALHTTPClientException do
    if E.StatusCode <> 303 then Writeln('Warning: '+E.Message);
  end;
  fs.Free;
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

  if (ParamStr(1) = '') or (not FileExists(ParamStr(1))) then begin
    ShowHelp;
    Halt;
  end else begin
    wa.Execute(ParamStr(1));
  end;
end.
