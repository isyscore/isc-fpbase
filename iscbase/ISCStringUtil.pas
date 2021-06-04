unit ISCStringUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, md5, DateUtils, RegExpr, sha1;

function base64Eecode(str: string): string;
function base64Decode(str: string): string;
function md5Encode(str: string): string;
function sha1Encode(str: string): string;
function TimeStampToReadableDate(t: Int64): string;
function StrToJSONEncoded(AStr: string): string;
function characterToHalf(str: string): string;
function numberToHalf(str: string): string;
function toDBC(str: string): string;
function StringReplaceRegEx(const AStr: string; const ARegEx: string; AReplacement: string): string;
function StringReplaceRegEx(const AStr: string; const ARegEx: TRegExpr; AReplacement: string): string;

implementation

uses
  ISCUTF8Helper;

type
  { TReplaceObj }

  TReplaceObj = class
  private
    FReplacement: string;
  public
    constructor Create(ARep: string = '');
    function Replace(r: TRegExpr): RegExprString;
    function ReplaceStr(r: TRegExpr): RegExprString;
  end;

  { TReplaceObj }

  constructor TReplaceObj.Create(ARep: string);
  begin
    FReplacement:= ARep;
  end;

  function TReplaceObj.Replace(r: TRegExpr): RegExprString;
  var
    s: string;
  begin
    s := r.Match[0];
    s := numberToHalf(s);
    Exit(s);
  end;

  function TReplaceObj.ReplaceStr(r: TRegExpr): RegExprString;
  begin
    Exit(FReplacement);
  end;


function StringReplaceRegEx(const AStr: string;const ARegEx: string; AReplacement: string): string;
var
  reg: TRegExpr;
  ro: TReplaceObj;
  ret: string;
begin
  reg:= TRegExpr.Create(ARegEx);
  ro := TReplaceObj.Create(AReplacement);
  ret := reg.Replace(AStr, @ro.ReplaceStr);
  ro.Free;
  reg.Free;
  Exit(ret);
end;

function StringReplaceRegEx(const AStr: string;const ARegEx: TRegExpr; AReplacement: string): string;
var
  ro: TReplaceObj;
  ret: string;
begin
  ro := TReplaceObj.Create(AReplacement);
  ret := ARegEx.Replace(AStr, @ro.ReplaceStr);
  ro.Free;
  Exit(ret);
end;

function toDBC(str: string): string;
var
  s: string;
begin
  s := characterToHalf(str);
  s := numberToHalf(s);
  Exit(s);
end;

function characterToHalf(str: string): string;
var
  len: Integer;
  i: Integer;
  c: Integer;
  s: WideChar;
  ret: string = '';
  r: TRegExpr;
  ro: TReplaceObj;
begin
  len := str.ULength;
  for i := 1 to len do begin
    s := str.UChar[i];
    c := s.UCode;
    if (s = '　') then begin
      ret += ' ';
    end else if (s = '﹒') then begin
      ret += '·';
    end else if (((s = '＠') or (s = '．') or (s = '＆') or (s = '？') or (s = '！')) or ((c >= 65313) and (c <= 65338)) or ((c >= 65338) and (c <= 65370))) then begin
      ret += Char(c - 65248);
    end else begin
      ret += s.UAnsi;
    end;
  end;

  ro := TReplaceObj.Create;
  r := TRegExpr.Create('「.*?」');
  ret := r.Replace(ret, @ro.Replace);
  r.Free;
  ro.Free;

  Exit(ret);
end;

function numberToHalf(str: string): string;
var
  len: Integer;
  i: Integer;
  ret: string = '';
  s: WideChar;
  c: Integer;
begin
  len := str.ULength;
  for i := 1 to len do begin
    s := str.UChar[i];
    c := s.UCode;
    if (c >= 65296) and (c <= 65305) then begin
     ret += Char(c - 65248);
    end else begin
      ret += s.UAnsi;
    end;
  end;
  Exit(ret);
end;

function StrToJSONEncoded(AStr: string): string;
begin
  Exit(AStr
    .UReplace('"', '\"', [rfIgnoreCase, rfReplaceAll])
    .UReplace(#13#10, '\n', [rfReplaceAll, rfIgnoreCase])
    .UReplace(#10, '\n', [rfIgnoreCase, rfReplaceAll])
    .UReplace(#13, '\n', [rfIgnoreCase, rfReplaceAll])
    );
end;

function base64Eecode(str: string): string;
begin
  Exit(EncodeStringBase64(str));
end;

function base64Decode(str: string): string;
begin
  Exit(DecodeStringBase64(str));
end;

function md5Encode(str: string): string;
begin
  Exit(MD5Print(MD5String(str)));
end;

function sha1Encode(str: string): string;
begin
  Exit(SHA1Print(SHA1String(str)));
end;

function TimeStampToReadableDate(t: Int64): string;
var
  dt: TDateTime;
begin
  if (t = 0) then Exit('');
  dt := UnixToDateTime(t, False);
  Exit(FormatDateTime('yyyy-MM-dd hh:mm:ss', dt));
end;

end.

