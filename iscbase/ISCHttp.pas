unit ISCHttp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fgl, process, HTTPDefs, BaseUnix, Sockets;

const
  MIME_JSON = 'application/json; charset=utf-8';
  MIME_HTML = 'text/html; charset=utf-8';
  MIME_TEXT = 'text/plain; charset=utf-8';
  MIME_CSS = 'text/css; charset=utf-8';
  MIME_JAVASCRIPT = 'application/x-javascript; charset=utf-8';
  MIME_JPEG = 'image/jpeg';
  MIME_PNG = 'image/png';
  MIME_GIF = 'image/gif';

type
  THttpMethod = (hmGet, hmPost, hmPut, hmDelete, hmOption);
  TISCDownloadProgress = procedure (AUrl: string; ASavePath: string; ACurrentProgress: Int64; ATotalLength: Int64);
  TISCHttpCallback = procedure(AUrl: string; ACode: Integer; ABody: string; ACookie: specialize TFPGMap<string, string>; AError: string);

  hostent = record
    h_name: PChar;      {/* official name of host *}
    h_aliases: PPChar;  {* alias list *}
    h_addrtype: cInt;   {* host address type *}
    h_length: cInt;     {* length of address *}
    h_addr_list: PPChar;{* list of addresses from name server *}
  end;
  THostEnt = hostent;
  PHostEnt = ^THostEnt;

function gethostbyname(name: PChar): PHostEnt; cdecl; external;
function inet_ntoa(__in:in_addr):Pchar;cdecl;external;

function ISCHttpGet(AUrl: string; AHeaders: specialize TFPGMap<string, string> = nil; ALookup: Boolean = False): string;
function ISCHttpPost(AUrl: string; AParam: string; AHeaders: specialize TFPGMap<string, string> = nil; ALookup: Boolean = False): string;
function ISCHttpPost(AUrl: string; AParam: specialize TFPGMap<String, String>; AHeaders: specialize TFPGMap<string, string> = nil; ALookup: Boolean = False): string;

function ISCHttpRequest(AUrl: string; AMethod: THttpMethod;
  AQueryParam: string = '';
  ADataParam: string = '{}';
  AHeaders: specialize TFPGMap<string, string> = nil;
  ACalllback: TISCHttpCallback = nil;
  ALookup: Boolean = False
): string;

function ISCDownloadFile(AUrl: string; ASavePath: string; AHeaders: specialize TFPGMap<string, string> = nil; AProgress: TISCDownloadProgress = nil; ALookup: Boolean = False): Boolean;

function ISCGetLocalIPMac(out AIp: string; out AMacAddr: string): Boolean;
procedure ISCAllowCors(AReq: TRequest; AResp: TResponse);
function ISCLookUp(ADomain: string): string;

implementation

uses
  ISCGeneric, ISCLogger;

function urlToLookupUrl(AUrl: string): string;
var
  u: string;
  ip: string;
  retU: string;
begin
  retU:= AUrl;
  u := AUrl;
  if (u.Contains('://')) then begin
    u := u.Substring(u.IndexOf('://') + 3);
  end;
  u := LeftStr(u, u.IndexOfAny([':', '/', '?']));
  ip := ISCLookUp(u);
  if (ip <> '0.0.0.0') then begin
    retU:= AUrl.Replace(u, ip);
  end;
  TLogger.info('lookup', 'url:%s, rep:%s'.Format([AUrl, retU]));
  Result := retU;
end;

procedure ISCAllowCors(AReq: TRequest; AResp: TResponse);
var
  AOri: string;
begin
  AOri:= AReq.CustomHeaders.Values['Origin'];
  if (AOri = '') then AOri:= '*';
  AResp.SetCustomHeader('Access-Control-Allow-Origin', AOri);
  AResp.SetCustomHeader('Access-Control-Allow-Credentials', 'true');
end;

function ISCLookUp(ADomain: string): string;
var
  host: PHostEnt;
  addr: in_addr;
begin
  host:= gethostbyname(PChar(ADomain));
  if (not Assigned(host)) then begin
    Exit('0.0.0.0');
  end;
  addr.s_addr:= PCardinal(host^.h_addr_list[0])^;
  Result := NetAddrToStr(addr);
end;

function ISCHttpRequest(AUrl: string; AMethod: THttpMethod;
  AQueryParam: string = '';
  ADataParam: string = '{}';
  AHeaders: specialize TFPGMap<string, string> = nil;
  ACalllback: TISCHttpCallback = nil;
  ALookup: Boolean = False
): string;
var
  http: TFPHTTPClient;
  i: Integer;
  reqUrl: string;
  retText: string;
  retError: string = '';
  retCookie: specialize TFPGMap<string, string> = nil;
begin
  Result := '';

  if (ALookup) then begin
    AUrl:= urlToLookupUrl(AUrl);
  end;

  http := TFPHTTPClient.Create(nil);
  try
    if (AHeaders <> nil) then begin
      for i:= 0 to AHeaders.Count - 1 do begin
        http.AddHeader(AHeaders.Keys[i], AHeaders.Data[i]);
      end;
    end;
    http.AllowRedirect:= True;
    if ((AMethod = hmPost) or (AMethod = hmPut)) and (ADataParam <> '') then begin
      http.AddHeader('Content-Type','application/json; charset=UTF-8');
      http.RequestBody := TRawByteStringStream.Create(ADataParam);
    end;
    reqUrl:= AUrl;
    if (AQueryParam <> '') then begin
      reqUrl += '?' + AQueryParam;
    end;

    try
      case AMethod of
      hmGet: retText := http.Get(reqUrl);
      hmPost: retText:= http.Post(reqUrl);
      hmPut: retText:= http.Put(reqUrl);
      hmDelete: retText:= http.Delete(reqUrl);
      hmOption: retText:= http.Options(reqUrl);
      end;

    except
      on E: Exception do begin
        retError:= E.Message;
      end;
    end;

    if (ACalllback <> nil) then begin
      retCookie := specialize TFPGMap<string, string>.Create;
      for i:= 0 to http.ResponseHeaders.Count - 1 do begin
        retCookie.Add(http.ResponseHeaders.Names[i], http.ResponseHeaders.ValueFromIndex[i]);
      end;
      ACalllback(reqUrl, http.ResponseStatusCode, retText, retCookie, retError);
    end;

  finally
    if ((AMethod = hmPost) or (AMethod = hmPut)) and (ADataParam <> '') then begin
      http.RequestBody.Free;
    end;
    http.Free;
  end;
end;

function ISCHttpGet(AUrl: string; AHeaders: specialize TFPGMap<string, string>; ALookup: Boolean = False): string;
var
  http: TFPHTTPClient;
  i: Integer;
begin
  Result := '';

  if (ALookup) then begin
    AUrl:= urlToLookupUrl(AUrl);
  end;

  http := TFPHTTPClient.Create(nil);
  try
    if (AHeaders <> nil) then begin
      for i:= 0 to AHeaders.Count - 1 do begin
        http.AddHeader(AHeaders.Keys[i], AHeaders.Data[i]);
      end;
    end;
    http.AllowRedirect:= True;
    try
      Result := http.Get(AUrl);
    except
      on E: Exception do begin
        TLogger.error('HTTP', 'ISCHttpGet: ' + AUrl + ', ' + E.Message);
      end;
    end;
  finally
    http.Free;
  end;
end;

function ISCHttpPost(AUrl: string; AParam: string; AHeaders: specialize TFPGMap<string, string>; ALookup: Boolean = False): string;
var
  http: TFPHTTPClient;
  i: Integer;
begin
  Result := '';

  if (ALookup) then begin
    AUrl:= urlToLookupUrl(AUrl);
  end;

  http := TFPHTTPClient.Create(nil);
  try
    http.AddHeader('Content-Type','application/json; charset=UTF-8');
    http.AddHeader('Accept', 'application/json');
    if (AHeaders <> nil) then begin
      for i:= 0 to AHeaders.Count - 1 do begin
        http.AddHeader(AHeaders.Keys[i], AHeaders.Data[i]);
      end;
    end;
    http.AllowRedirect:= True;
    http.RequestBody := TRawByteStringStream.Create(AParam);
    try
      Result := http.Post(AUrl);
    except
      on E: Exception do begin
        TLogger.error('HTTP', 'ISCHttpPost: ' + AUrl + ', ' + E.Message);
      end;
    end;
  finally
    http.RequestBody.Free;
    http.Free;
  end;
end;

function ParamToRequestQueryStringImpl(key: string; value: string): string;
begin
  Exit('%s=%s'.Format([key, value]));
end;

function ParamJoinToImpl(item: string): string;
begin
  Exit(item);
end;

function ParamToRequestQueryString(AParam: specialize TFPGMap<String, String>): string;
var
  list: specialize TFPGList<string>;
  ret: string;
begin
  list := specialize ISCMap<string, string, string>(AParam, @ParamToRequestQueryStringImpl);
  ret := specialize ISCJoinTo<string>(list, '&', @ParamJoinToImpl);
  list.Free;
  Exit(ret);
end;

function ISCHttpPost(AUrl: string; AParam: specialize TFPGMap<String, String>; AHeaders: specialize TFPGMap<string, string>; ALookup: Boolean = False): string;
var
  http: TFPHTTPClient;
  i: Integer;
begin
  Result := '';

  if (ALookup) then begin
    AUrl:= urlToLookupUrl(AUrl);
  end;

  http := TFPHTTPClient.Create(nil);
  try
    http.AllowRedirect:= True;
    if (AHeaders <> nil) then begin
      for i:= 0 to AHeaders.Count - 1 do begin
        http.AddHeader(AHeaders.Keys[i], AHeaders.Data[i]);
      end;
    end;
    try
      Result := http.FormPost(AUrl, ParamToRequestQueryString(AParam));
    except
      on E: Exception do begin
        TLogger.error('HTTP', 'ISCHttpPost: ' + AUrl + ', ' + E.Message);
      end;
    end;
  finally
    http.Free;
  end;
end;

type

  { TInnerDownloadEvent }

  TInnerDownloadEvent = class

  private
    FDownloadUrl: string;
    FSavePath: string;
    FEvent: TISCDownloadProgress;
  public
    constructor Create(AUrl: string; ASavePath: string; AEvent: TISCDownloadProgress);
    Procedure OnData(Sender : TObject; Const ContentLength, CurrentPos : Int64);
  published
    property DownloadUrl: string read FDownloadUrl write FDownloadUrl;
    property SavePath: string read FSavePath write FSavePath;
  end;

function ISCDownloadFile(AUrl: string; ASavePath: string; AHeaders: specialize
  TFPGMap<string, string>; AProgress: TISCDownloadProgress; ALookup: Boolean = False): Boolean;
var
  http: TFPHTTPClient;
  evt: TInnerDownloadEvent;
  i: Integer;
  ret: Boolean = False;
begin
  if (ALookup) then begin
    AUrl:= urlToLookupUrl(AUrl);
  end;

  http := TFPHTTPClient.Create(nil);
  evt := TInnerDownloadEvent.Create(AUrl, ASavePath, AProgress);
  http.OnDataReceived:= @evt.OnData;
  try
    if (AHeaders <> nil) then begin
      for i:= 0 to AHeaders.Count - 1 do begin
        http.AddHeader(AHeaders.Keys[i], AHeaders.Data[i]);
      end;
    end;
    http.AllowRedirect:= True;
    try
      http.Get(AUrl, ASavePath);
      ret:= True;
    except
    end;
  finally
    evt.Free;
    http.Free;
  end;
  Exit(ret);
end;

function innerGetIPMac(part: string; out AIp: string; out AMacAddr: string): Boolean;
var
  outstr: string;
  sl: TStringList;
  i: Integer;
begin
  AIp:= '';
  AMacAddr:= '';
  RunCommand('ifconfig', [part], outstr, [poWaitOnExit, poUsePipes]);
  if ((outstr.Trim = '') or (outstr.ToLower.Contains('not found')) or (outstr.ToLower.Contains('does not exist'))) then begin
    Exit(False);
  end;
  sl := TStringList.Create;
  sl.Text:= outstr;
  for i:= 0 to sl.Count - 1 do begin
    if (sl[i].Trim.StartsWith('inet ')) then begin
      AIp:= sl[i].Trim.Replace('inet ', '');
      AIp:= AIp.Substring(0, Aip.IndexOf(' '));
    end;
    if (sl[i].Trim.StartsWith('ether ')) then begin
      AMacAddr:= sl[i].Trim.Replace('ether ', '');
      if (AMacAddr.Contains(' ')) then AMacAddr:= AMacAddr.Substring(0, AMacAddr.IndexOf(' ')).Trim();
    end;
    if (sl[i].Contains('inet addr:')) then begin
      AIp:= sl[i].Replace('inet addr:', '').Trim;
      AIp:= AIp.Substring(0, Aip.IndexOf(' '));
    end;
    if (sl[i].Contains('HWaddr ')) then begin
      AMacAddr:= sl[i].Substring(sl[i].IndexOf('HWaddr') + 6).Trim;
    end;
  end;
  sl.Free;
  Exit(True);
end;

function ISCGetLocalIPMac(out AIp: string; out AMacAddr: string): Boolean;
var
  r: Boolean;
begin
  r := innerGetIPMac('eth0', AIp, AMacAddr);
  if (not r) then r := innerGetIPMac('enp2s0', AIp, AMacAddr);
  if (not r) then r := innerGetIPMac('en0', AIp, AMacAddr);
  if (not r) then r := innerGetIPMac('enp0s5', AIp, AMacAddr);
  Exit(r);
end;

{ TInnerDownloadEvent }

constructor TInnerDownloadEvent.Create(AUrl: string; ASavePath: string;
  AEvent: TISCDownloadProgress);
begin
  FDownloadUrl:= AUrl;
  FSavePath:= ASavePath;
  FEvent:= AEvent;
end;

procedure TInnerDownloadEvent.OnData(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  if (Assigned(FEvent)) then begin
    FEvent(FDownloadUrl, FSavePath, CurrentPos, ContentLength);
  end;
end;

end.

