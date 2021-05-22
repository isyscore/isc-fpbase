unit ISCUTF8Helper;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  Classes, SysUtils, RegExpr, LazUTF8;

type

  { TUTF8CharHelper }

  TUTF8CharHelper = type Helper for WideChar
  private
    function GetAnsi: string;
    function GetUCode: Integer;
  public
    class function UFromCode(ACode: Integer): WideChar; static;
  public
    property UCode: Integer read GetUCode;
    property UAnsi: string read GetAnsi;
  end;

  TWideCharArray = array of WideChar;

  TStringArrayFilterCallable = Function(item: string): Boolean;
  TStringArrayMapCharCallable = Function(item: string): WideChar;
  TStringArrayJoinCallable = Function(item: string): string;

  { TUTF8StringArrayHelper }

  TUTF8StringArrayHelper = type Helper for TStringArray
  public
    function Filter(call: TStringArrayFilterCallable): TStringArray;
    function Join(const Separator: string; call: TStringArrayJoinCallable = nil): string;
    function MapToChar(call: TStringArrayMapCharCallable): TWideCharArray;
  end;

  TUTF8StringReplaceCallable = Function (AValue: String): String;

  { TUTF8StringHelper }

  TUTF8StringHelper =  type Helper for String
  private
    function GetUChar(Index: Integer): WideChar;
    function GetULength: Integer;
  public
    const Empty = '';
    Class Function UCompare(const A: string; const B: string): Integer; overload; static; //inline;
    Class Function UCompare(const A: string; const B: string; IgnoreCase: Boolean): Integer; overload; static; //inline;
    Class Function UCompare(const A: string; const B: string; Options: TCompareOptions): Integer; overload; static; // inline;
    Class Function UCompare(const A: string; IndexA: Integer; const B: string; IndexB: Integer; ALen: Integer): Integer; overload; static; // inline;
    Class Function UCompare(const A: string; IndexA: Integer; const B: string; IndexB: Integer; ALen: Integer; IgnoreCase: Boolean): Integer; overload; static; // inline;
    Class Function UCompare(const A: string; IndexA: Integer; const B: string; IndexB: Integer; ALen: Integer; Options: TCompareOptions): Integer; overload; static;//  inline;
    Class Function UCreate(AChar: WideChar; ACount: Integer): string; overload; inline; static;
    Class Function UCreate(const AValue: array of WideChar): string; overload; static;
    Class Function UCreate(const AValue: array of WideChar; StartIndex: Integer; ALen: Integer): string; overload; static;
    Class Function UJoin(const Separator: string; const Values: array of const): string; overload; static;
    Class Function UJoin(const Separator: string; const Values: array of string): string; overload; static;
    Class Function UJoin(const Separator: string; const Values: array of string; StartIndex: Integer; ACount: Integer): string; overload; static;
    Class Function UParse(const AValue: Boolean): string; overload; static; inline;
    Class Function UParse(const AValue: Extended): string; overload; static;inline;
    Class Function UParse(const AValue: Int64): string; overload; static; inline;
    Class Function UParse(const AValue: Integer): string; overload; static; inline;
    Function UCompareTo(const B: string): Integer;
    Function UCountChar(const C: WideChar): Integer;
    Function UIsEmpty: Boolean;
    Function UEndsWith(const AValue: string): Boolean; overload; inline;
    Function UEndsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;
    Function UStartsWith(const AValue: string): Boolean; overload; inline;
    Function UStartsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;
    Function UContains(const AValue: string): Boolean;
    Function UIndexOf(AValue: WideChar): Integer; overload; inline;
    Function UIndexOf(const AValue: string): Integer; overload; inline;
    Function UIndexOf(AValue: WideChar; StartIndex: Integer): Integer; overload;
    Function UIndexOf(const AValue: string; StartIndex: Integer): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of WideChar): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of WideChar; StartIndex: Integer): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of String): Integer; overload;
    Function UIndexOfAny(const AnyOf: array of String; StartIndex: Integer): Integer; overload;
    Function ULastIndexOf(AValue: WideChar): Integer; overload; inline;
    Function ULastIndexOf(const AValue: string): Integer; overload; inline;
    Function ULastIndexOf(AValue: WideChar; StartIndex: Integer): Integer; overload;
    Function ULastIndexOf(const AValue: string; StartIndex: Integer): Integer; overload;
    Function ULastIndexOfAny(const AnyOf: array of WideChar): Integer; overload;
    Function ULastIndexOfAny(const AnyOf: array of WideChar; StartIndex: Integer): Integer; overload;
    Function ULastIndexOfAny(const AnyOf: array of String): Integer; overload;
    Function ULastIndexOfAny(const AnyOf: array of String; StartIndex: Integer): Integer; overload;
    Function UPadLeft(ATotalWidth: Integer): string; overload; inline;
    Function UPadLeft(ATotalWidth: Integer; PaddingChar: WideChar): string; overload; inline;
    Function UPadRight(ATotalWidth: Integer): string; overload; inline;
    Function UPadRight(ATotalWidth: Integer; PaddingChar: WideChar): string; overload; inline;
    Function URemove(StartIndex: Integer): string; overload; inline;
    Function URemove(StartIndex: Integer; ACount: Integer): string; overload; inline;
    Function UReplace(const OldValue: string; const NewValue: string): string; overload;
    Function UReplace(const OldValue: string; const NewValue: string; ReplaceFlags: TReplaceFlags): string; overload;
    Function UReplaceRegex(const ARegEx: string; Replacable: TUTF8StringReplaceCallable): string; overload;
    Function UReplaceRegex(const ARegEx: TRegExpr; Replacable: TUTF8StringReplaceCallable): string; overload;
    Function USubstring(AStartIndex: Integer): string; overload;
    Function USubstringLen(AStartIndex: Integer; ALen: Integer): string;
    Function USubstring(AStartIndex: Integer; AEndIndex: Integer): string; overload;
    Function USplit(const Separators: array of WideChar): TStringArray; overload;
    Function USplit(const Separators: TStringArray): TStringArray; overload;
    Function UTrim: string; overload;
    Function UTrimLeft: string; overload;
    Function UTrimRight: string; overload;
    Function UTrim(const ATrimChars: array of WideChar): string; overload;
    Function UTrimLeft(const ATrimChars: array of WideChar): string; overload;
    Function UTrimRight(const ATrimChars: array of WideChar): string; overload;
    Function UFormat(const args: array of const): string; overload;
    Function UToBoolean: Boolean; overload; inline;
    Function UToInteger: Integer; overload; inline;
    Function UToInt64: Int64; overload; inline;
    Function UToSingle: Single; overload; inline;
    Function UToDouble: Double; overload; inline;
    Function UToExtended: Extended; overload; inline;
    Function UToCharArray: TWideCharArray; overload;
    Function UToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TWideCharArray; overload;
    Function UToLower: string; overload; inline;
    Function UToUpper: string; overload; inline;
  public
    property ULength: Integer read GetULength;
    property UChar[Index: Integer]: WideChar read GetUChar;
  end;

implementation

{ TUTF8StringArrayHelper }

function TUTF8StringArrayHelper.Filter(call: TStringArrayFilterCallable
  ): TStringArray;
var
  AList: TStringList;
  i: Integer;
begin
  AList := TStringList.Create;
  for i := 0 to Length(Self) - 1 do begin
    if (call(Self[i])) then begin
      AList.Add(Self[i]);
    end;
  end;
  Result := AList.ToStringArray;
  AList.Free;
end;

function TUTF8StringArrayHelper.Join(const Separator: string;
  call: TStringArrayJoinCallable): string;
var
  ret: string = '';
  i: Integer;
begin
  for i := 0 to Length(Self) - 1 do begin
    if (i = 0) then begin
      if (call <> nil) then begin
        ret += call(Self[0]);
      end else begin
        ret += Self[0];
      end;
    end else begin
      if (call <> nil) then begin
        ret += Separator + call(Self[i]);
      end else begin
        ret += Separator + Self[i];
      end;
    end;
  end;
  Exit(ret);
end;

function TUTF8StringArrayHelper.MapToChar(call: TStringArrayMapCharCallable
  ): TWideCharArray;
var
  len: Integer;
  i: Integer;
begin
  Result := nil;
  len := Length(Self);
  SetLength(Result, len);
  for i := 0 to len - 1 do begin
    Result[i] := call(Self[i]);
  end;
end;

{ TUTF8CharHelper }

function TUTF8CharHelper.GetAnsi: string;
begin
  Exit(String(Self));
end;

function TUTF8CharHelper.GetUCode: Integer;
begin
  Exit(Integer(Self));
end;

class function TUTF8CharHelper.UFromCode(ACode: Integer): WideChar;
begin
  Exit(WideChar(ACode));
end;

{ TUTF8StringHelper }

function TUTF8StringHelper.GetUChar(Index: Integer): WideChar;
begin
  Exit(WideString(UTF8Copy(Self, Index, 1))[1]);
end;

function TUTF8StringHelper.GetULength: Integer;
begin
  Exit(UTF8Length(Self));
end;

class function TUTF8StringHelper.UCompare(const A: string; const B: string
  ): Integer;
begin
  Result:=UCompare(A,0,B,0,B.ULength,[]);
end;

class function TUTF8StringHelper.UCompare(const A: string; const B: string;
  IgnoreCase: Boolean): Integer;
begin
  if IgnoreCase then
    Result:=UCompare(A,B,[coIgnoreCase])
  else
    Result:=UCompare(A,B,[]);
end;

class function TUTF8StringHelper.UCompare(const A: string; const B: string;
  Options: TCompareOptions): Integer;
begin
  Result:=UCompare(A,0,B,0,B.ULength,Options);
end;

class function TUTF8StringHelper.UCompare(const A: string; IndexA: Integer;
  const B: string; IndexB: Integer; ALen: Integer): Integer;
begin
  Result:=UCompare(A,IndexA,B,IndexB,ALen,[]);
end;

class function TUTF8StringHelper.UCompare(const A: string; IndexA: Integer;
  const B: string; IndexB: Integer; ALen: Integer; IgnoreCase: Boolean
  ): Integer;
begin
  if IgnoreCase then
    Result:=UCompare(A,IndexA,B,IndexB,ALen,[coIgnoreCase])
  else
    Result:=UCompare(A,IndexA,B,IndexB,ALen,[])
end;

class function TUTF8StringHelper.UCompare(const A: string; IndexA: Integer;
  const B: string; IndexB: Integer; ALen: Integer; Options: TCompareOptions
  ): Integer;
Var
  L : Integer;
begin
  L:=ALen;
  If (L>A.ULength-IndexA) then
    L:=A.ULength-IndexA;
  If (L>B.ULength-IndexB) then
    L:=B.ULength-IndexB;
  if (coIgnoreCase in Options) then begin
    Result:=strlicomp(PChar(@A[IndexA+1]),PChar(@B[IndexB+1]),L)
  end else begin
    Result:=strlcomp(PChar(@A[IndexA+1]),PChar(@B[IndexB+1]),L);
  end;
end;

class function TUTF8StringHelper.UCreate(AChar: WideChar; ACount: Integer
  ): string;
begin
  Result := UTF8StringOfChar(AChar.UAnsi, ACount);
end;

class function TUTF8StringHelper.UCreate(const AValue: array of WideChar
  ): string;
begin
  Result:=UCreate(AValue,0,System.Length(AValue));
end;

class function TUTF8StringHelper.UCreate(const AValue: array of WideChar;
  StartIndex: Integer; ALen: Integer): string;
begin
  Result := '';
  SetLength(Result,ALen);
  if ALen>0 then
    Move(AValue[StartIndex],Result[1],ALen);
end;

class function TUTF8StringHelper.UJoin(const Separator: string;
  const Values: array of const): string;
Var
  SValues : Array of string;
  I,L : SizeInt;
  S : String;
  P : ^TVarRec;
begin
  L:=System.Length(Values);
  SValues := nil;
  SetLength(SValues,L);
  Dec(L);
  for I:=0 to L do
    begin
    S:='';
    P:=@Values[I];
    Case P^.VType of
      vtInteger  : S:=IntToStr(P^.VInteger);
      vtBoolean  : S:=BoolToStr(P^.VBoolean, True);
      vtChar     : S:=P^.VChar;
      vtPChar    : S:= string(P^.VPChar);
      vtExtended : S:=FloatToStr(P^.VExtended^);
      vtObject   : S:=TObject(P^.VObject).Classname;
      vtClass    : S:=P^.VClass.Classname;
      vtCurrency : S:=CurrToStr(P^.VCurrency^);
      vtVariant  : S:=(P^.VVariant^);
      vtInt64    : S:=IntToStr(PInt64(P^.VInt64)^);
      vtQword    : S:=IntToStr(PQWord(P^.VQword)^);
      {$WARNINGS OFF}
      vtWideChar     : S:=WideString(P^.VWideChar);
      vtPWideChar     : S:=WideString(P^.VPWideChar);
      vtUnicodeString : S:=UnicodeString(P^.VUnicodeString);
      {$WARNINGS ON}
      vtAnsiString    : S:=Ansistring(P^.VAnsiString);
    else
      S:=Format('Unknown type: %d',[P^.VType]);
    end;
    SValues[I]:=S;
    end;
  Result:=UJoin(Separator,SValues);
end;

class function TUTF8StringHelper.UJoin(const Separator: string;
  const Values: array of string): string;
begin
  Result:=UJoin(Separator,Values,0,System.Length(Values));
end;

class function TUTF8StringHelper.UJoin(const Separator: string;
  const Values: array of string; StartIndex: Integer; ACount: Integer): string;
Var
  I,L,VLen : Integer;
begin
  VLen:=High(Values);
  If (ACount<0) or ((StartIndex>0) and (StartIndex>VLen)) then
    raise Exception.Create('Range error');
  If (ACount=0) or (VLen<0) then
    Result:=''
  else
    begin
    L:=StartIndex+ACount-1;
    if L>Vlen then
      L:=VLen;
    Result:=Values[StartIndex];
    For I:=StartIndex+1 to L do
      Result:=Result+Separator+Values[I];
    end;
end;

class function TUTF8StringHelper.UParse(const AValue: Boolean): string;
begin
  Exit(BoolToStr(AValue));
end;

class function TUTF8StringHelper.UParse(const AValue: Extended): string;
begin
  Exit(FloatToStr(AValue));
end;

class function TUTF8StringHelper.UParse(const AValue: Int64): string;
begin
  Exit(IntToStr(AValue));
end;

class function TUTF8StringHelper.UParse(const AValue: Integer): string;
begin
  Exit(IntToStr(AValue));
end;

function TUTF8StringHelper.UCompareTo(const B: string): Integer;
begin
  Result:=sysUtils.StrComp(PChar(Self),PChar(B));
end;

function TUTF8StringHelper.UCountChar(const C: WideChar): Integer;
Var
  S : WideChar;
  i: Integer;
begin
  Result:=0;
  For i:= 1 to Self.ULength do begin
    S := Self.UChar[i];
    if (S = C) then Inc(Result);
  end;
end;

function TUTF8StringHelper.UIsEmpty: Boolean;
begin
  Exit(Self.ULength = 0);
end;

function TUTF8StringHelper.UEndsWith(const AValue: string): Boolean;
begin
  Exit(UEndsWith(AValue, False));
end;

function TUTF8StringHelper.UEndsWith(const AValue: string; IgnoreCase: Boolean
  ): Boolean;
Var
  L : Integer;
  S : String;
begin
  L:= AValue.ULength;
  Result:= L = 0;
  if Not Result then begin
    s := UTF8Copy(Self, Self.ULength - L + 1, L);
    Result:=S.ULength=L;
    if Result then begin
      if IgnoreCase then
        Result:= CompareText(S,AValue) = 0
      else
        Result:= S = AValue;
    end;
  end;
end;

function TUTF8StringHelper.UStartsWith(const AValue: string): Boolean;
begin
  Exit(UStartsWith(AValue, False));
end;

function TUTF8StringHelper.UStartsWith(const AValue: string; IgnoreCase: Boolean
  ): Boolean;
Var
  L : Integer;
  S : String;
begin
  L:= AValue.ULength;
  Result := L <= 0;
  if not Result then begin
    s := UTF8Copy(Self, 1, L);
    Result:= S.ULength = L;
    if Result then begin
      if IgnoreCase then
        Result:= SameText(S,aValue)
      else
        Result:= SameStr(S,AValue);
    end;
  end;
end;

function TUTF8StringHelper.UContains(const AValue: string): Boolean;
begin
  Exit(UIndexOf(AValue) <> -1);
end;

function TUTF8StringHelper.UIndexOf(AValue: WideChar): Integer;
var
  idx: Integer = -1;
  i: Integer;
  c: WideChar;
begin
  for i := 1 to Self.ULength do begin
    c := Self.UChar[i];
    if (c = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOf(const AValue: string): Integer;
var
  s: string;
  idx: Integer = -1;
  i: Integer;
  le: Integer;
begin
  le := AValue.ULength;
  for i := 1 to Self.ULength do begin
    s := UTF8Copy(Self, i, le);
    if (s = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOf(AValue: WideChar; StartIndex: Integer
  ): Integer;
var
  idx: Integer = -1;
  i: Integer;
  c: WideChar;
begin
  for i := StartIndex to Self.ULength do begin
    c := Self.UChar[i];
    if (c = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOf(const AValue: string; StartIndex: Integer
  ): Integer;
var
  s: string;
  idx: Integer = -1;
  i: Integer;
  le: Integer;
begin
  le := AValue.ULength;
  for i := StartIndex to Self.ULength do begin
    s := UTF8Copy(Self, i, le);
    if (s = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function InWideCharArray(c: WideChar; AnyOf: array of WideChar): Boolean;
var
  i: Integer;
  ret: Boolean = False;
begin
  for i := 0 to Length(AnyOf) - 1 do begin
    if (c = AnyOf[i]) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of WideChar): Integer;
var
  i: Integer;
  idx: Integer = -1;
  c: WideChar;
begin
  for i := 1 to Self.ULength do begin
    c := Self.UChar[i];
    if (InWideCharArray(c, AnyOf)) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of WideChar;
  StartIndex: Integer): Integer;
var
  i: Integer;
  idx: Integer = -1;
  c: WideChar;
begin
  for i := StartIndex to Self.ULength do begin
    c := Self.UChar[i];
    if (InWideCharArray(c, AnyOf)) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of String): Integer;
var
  le: Integer;
  i, j: Integer;
  sj: string;
  si: string;
  idx: Integer = -1;
begin
  for i := 1 to Self.ULength do begin
    for j := 0 to Length(AnyOf) - 1 do begin
      sj := AnyOf[j];
      le := sj.ULength;
      si := UTF8Copy(Self, i, le);
      if (si = sj) then begin
        idx := i;
        Break;
      end;
    end;
    if (idx <> -1) then Break;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UIndexOfAny(const AnyOf: array of String;
  StartIndex: Integer): Integer;
var
  le: Integer;
  i, j: Integer;
  sj: string;
  si: string;
  idx: Integer = -1;
begin
  for i := StartIndex to Self.ULength do begin
    for j := 0 to Length(AnyOf) - 1 do begin
      sj := AnyOf[j];
      le := sj.ULength;
      si := UTF8Copy(Self, i, le);
      if (si = sj) then begin
        idx := i;
        Break;
      end;
    end;
    if (idx <> -1) then Break;
  end;
  Exit(idx);

end;

function TUTF8StringHelper.ULastIndexOf(AValue: WideChar): Integer;
var
  idx: Integer = -1;
  i: Integer;
  c: WideChar;
begin
  for i := Self.ULength downto 1 do begin
    c := Self.UChar[i];
    if (c = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.ULastIndexOf(const AValue: string): Integer;
var
  s: string;
  idx: Integer = -1;
  i: Integer;
  le: Integer;
begin
  le := AValue.ULength;
  for i :=  Self.ULength downto 1 do begin
    s := UTF8Copy(Self, i, le);
    if (s = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.ULastIndexOf(AValue: WideChar; StartIndex: Integer
  ): Integer;
var
  idx: Integer = -1;
  i: Integer;
  c: WideChar;
begin
  for i := Self.ULength downto StartIndex do begin
    c := Self.UChar[i];
    if (c = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.ULastIndexOf(const AValue: string;
  StartIndex: Integer): Integer;
var
  s: string;
  idx: Integer = -1;
  i: Integer;
  le: Integer;
begin
  le := AValue.ULength;
  for i :=  Self.ULength downto StartIndex do begin
    s := UTF8Copy(Self, i, le);
    if (s = AValue) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.ULastIndexOfAny(const AnyOf: array of WideChar
  ): Integer;
var
  i: Integer;
  idx: Integer = -1;
  c: WideChar;
begin
  for i := Self.ULength downto 1 do begin
    c := Self.UChar[i];
    if (InWideCharArray(c, AnyOf)) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.ULastIndexOfAny(const AnyOf: array of WideChar;
  StartIndex: Integer): Integer;
var
  i: Integer;
  idx: Integer = -1;
  c: WideChar;
begin
  for i := Self.ULength downto StartIndex do begin
    c := Self.UChar[i];
    if (InWideCharArray(c, AnyOf)) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);

end;

function TUTF8StringHelper.ULastIndexOfAny(const AnyOf: array of String
  ): Integer;
var
  le: Integer;
  i, j: Integer;
  sj: string;
  si: string;
  idx: Integer = -1;
begin
  for i := Self.ULength downto 1 do begin
    for j := 0 to Length(AnyOf) - 1 do begin
      sj := AnyOf[j];
      le := sj.ULength;
      si := UTF8Copy(Self, i, le);
      if (si = sj) then begin
        idx := i;
        Break;
      end;
    end;
    if (idx <> -1) then Break;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.ULastIndexOfAny(const AnyOf: array of String;
  StartIndex: Integer): Integer;
var
  le: Integer;
  i, j: Integer;
  sj: string;
  si: string;
  idx: Integer = -1;
begin
  for i := Self.ULength downto StartIndex do begin
    for j := 0 to Length(AnyOf) - 1 do begin
      sj := AnyOf[j];
      le := sj.ULength;
      si := UTF8Copy(Self, i, le);
      if (si = sj) then begin
        idx := i;
        Break;
      end;
    end;
    if (idx <> -1) then Break;
  end;
  Exit(idx);
end;

function TUTF8StringHelper.UPadLeft(ATotalWidth: Integer): string;
begin
  Exit(UPadLeft(ATotalWidth, ' '));
end;

function TUTF8StringHelper.UPadLeft(ATotalWidth: Integer; PaddingChar: WideChar
  ): string;
var
  i: Integer;
  l: Integer;
  ret: string;
begin
  ret:= Self;
  l:= ATotalWidth - Self.ULength;
  If l > 0 then begin
    for i := 0 to l - 1 do begin
      ret := PaddingChar.UAnsi + ret;
    end;
  end;
  Exit(ret);
end;

function TUTF8StringHelper.UPadRight(ATotalWidth: Integer): string;
begin
  Exit(UPadRight(ATotalWidth, ' '));
end;

function TUTF8StringHelper.UPadRight(ATotalWidth: Integer; PaddingChar: WideChar
  ): string;
var
  l: Integer;
  i: Integer;
  ret: string;
begin
  ret:= Self;
  l:= ATotalWidth - Self.ULength;
  If l > 0 then begin
    for i := 0 to l - 1 do begin
      ret := ret + PaddingChar.UAnsi;
    end;
  end;
  Exit(ret);
end;

function TUTF8StringHelper.URemove(StartIndex: Integer): string;
begin
  Exit(URemove(StartIndex,Self.ULength - StartIndex));
end;

function TUTF8StringHelper.URemove(StartIndex: Integer; ACount: Integer
  ): string;
begin
  Result := Self;
  UTF8Delete(Result, StartIndex, ACount);
end;

function TUTF8StringHelper.UReplace(const OldValue: string;
  const NewValue: string): string;
begin
  Exit(UReplace(OldValue, NewValue, [rfReplaceAll]));
end;

function TUTF8StringHelper.UReplace(const OldValue: string;
  const NewValue: string; ReplaceFlags: TReplaceFlags): string;
begin
  Exit(StringReplace(Self, OldValue, NewValue, ReplaceFlags));
end;

type

  { TRegReplacable }

  TRegReplacable = class
  private
    FCallable: TUTF8StringReplaceCallable;
  public
    constructor Create(ACall: TUTF8StringReplaceCallable);
    function doReplace(r: TRegExpr): RegExprString;
  end;

{ TRegReplacable }

constructor TRegReplacable.Create(ACall: TUTF8StringReplaceCallable);
begin
  FCallable:= ACall;
end;

function TRegReplacable.doReplace(r: TRegExpr): RegExprString;
begin
  Exit(FCallable(r.Match[0]));
end;

function TUTF8StringHelper.UReplaceRegex(const ARegEx: string;
  Replacable: TUTF8StringReplaceCallable): string;
var
  reg: TRegExpr;
  ro: TRegReplacable;
  ret: string;
begin
  reg:= TRegExpr.Create(ARegEx);
  ro := TRegReplacable.Create(Replacable);
  ret := reg.Replace(Self, @ro.doReplace);
  ro.Free;
  reg.Free;
  Exit(ret);
end;

function TUTF8StringHelper.UReplaceRegex(const ARegEx: TRegExpr;
  Replacable: TUTF8StringReplaceCallable): string;
var
  ro: TRegReplacable;
  ret: string;
begin
  ro := TRegReplacable.Create(Replacable);
  ret := ARegEx.Replace(Self, @ro.doReplace);
  ro.Free;
  Exit(ret);
end;

function TUTF8StringHelper.USubstring(AStartIndex: Integer): string;
begin
  Exit(USubstringLen(AStartIndex, Self.ULength - AStartIndex));
end;

function TUTF8StringHelper.USubstringLen(AStartIndex: Integer; ALen: Integer
  ): string;
begin
  Exit(UTF8Copy(Self, AStartIndex, ALen));
end;

function TUTF8StringHelper.USubstring(AStartIndex: Integer; AEndIndex: Integer
  ): string;
begin
  Exit(USubstringLen(AStartIndex, AEndIndex - AStartIndex));
end;

function TUTF8StringHelper.USplit(const Separators: array of WideChar
  ): TStringArray;
var
  alist: TStringList;
  tmp: string;
  start: Integer;
begin
  alist := TStringList.Create;
  tmp := Self;
  while True do begin
    start:= tmp.UIndexOfAny(Separators);
    if (start = -1) then begin
      alist.Add(tmp);
      Break;
    end;
    alist.Add(tmp.USubstring(1, start));
    tmp := tmp.USubstring(start + 1);
  end;
  Result := alist.ToStringArray;
  alist.Free;
end;

function MapToCharCallable(item: string): WideChar;
begin
  Exit(WideString(item)[1]);
end;

function TUTF8StringHelper.USplit(const Separators: TStringArray): TStringArray;
var
  arr: TWideCharArray;
begin
  arr := Separators.MapToChar(@MapToCharCallable);
  Exit(USplit(arr));
end;

function TUTF8StringHelper.UTrim: string;
begin
  Exit(SysUtils.Trim(Self));
end;

function TUTF8StringHelper.UTrimLeft: string;
begin
  Exit(SysUtils.TrimLeft(Self));
end;

function TUTF8StringHelper.UTrimRight: string;
begin
  Exit(SysUtils.TrimRight(Self));
end;

Function HaveChar(AChar : WideChar; const AList: array of WideChar) : Boolean;
Var
  I : SizeInt;
begin
  I:=0;
  Result:=False;
  While (Not Result) and (I<Length(AList)) do begin
    Result:=(AList[i]=AChar);
    Inc(I);
  end;
end;

function TUTF8StringHelper.UTrim(const ATrimChars: array of WideChar): string;
begin
  Result:=Self.UTrimLeft(ATrimChars).UTrimRight(ATrimChars);
end;

function TUTF8StringHelper.UTrimLeft(const ATrimChars: array of WideChar
  ): string;
Var
  I,Len : SizeInt;

begin
  I:=1;
  Len:=Self.ULength;
  While (I<=Len) and HaveChar(Self.UChar[i],ATrimChars) do Inc(I);
  if I=1 then
    Result:=Self
  else if I>Len then
    Result:=''
  else
    Result:= UTF8Copy(Self, I, Len - I + 1);
end;

function TUTF8StringHelper.UTrimRight(const ATrimChars: array of WideChar
  ): string;
Var
  I,Len : SizeInt;

begin
  Len:=Self.ULength;
  I:=Len;
  While (I>=1) and HaveChar(Self.UChar[i],ATrimChars) do Dec(I);
  if I<1 then
    Result:=''
  else if I=Len then
    Result:=Self
  else
    Result:= UTF8Copy(Self, 1, I);

end;

function TUTF8StringHelper.UFormat(const args: array of const): string;
begin
  Exit(SysUtils.Format(Self, args));
end;

function TUTF8StringHelper.UToBoolean: Boolean;
begin
  Exit(StrToBool(Self));
end;

function TUTF8StringHelper.UToInteger: Integer;
begin
  Exit(StrToInt(Self));
end;

function TUTF8StringHelper.UToInt64: Int64;
begin
  Exit(StrToInt64(Self));
end;

function TUTF8StringHelper.UToSingle: Single;
begin
  Exit(StrToFloat(Self));
end;

function TUTF8StringHelper.UToDouble: Double;
begin
  Exit(StrToFloat(Self));
end;

function TUTF8StringHelper.UToExtended: Extended;
begin
  Exit(StrToFloat(Self));
end;

function TUTF8StringHelper.UToCharArray: TWideCharArray;
begin
  Result:=UToCharArray(0,Self.ULength);
end;

function TUTF8StringHelper.UToCharArray(AStartIndex: SizeInt; ALen: SizeInt
  ): TWideCharArray;
Var
  I : SizeInt;
begin
  Result := nil;
  SetLength(Result, ALen);
  For I:=0 to ALen-1 do
    Result[I]:=Self.UChar[AStartIndex+I+1];
end;

function TUTF8StringHelper.UToLower: string;
begin
  Exit(SysUtils.LowerCase(Self));
end;

function TUTF8StringHelper.UToUpper: string;
begin
  Exit(SysUtils.UpperCase(Self));
end;

end.

