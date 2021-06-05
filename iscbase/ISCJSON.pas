unit ISCJSON;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fpjsonrtti;

type

  { TISCJSONObject }

  TISCJSONObject = class(TJSONObject)
  private
    function GetOptArrays(AName : String): TJSONArray;
    function GetOptObjects(AName: string): TJSONObject;
  public
    function OptIntegers(AName: String; ADefault: Integer): Integer;
    function OptInt64s(AName: string; ADefault: Int64): Int64;
    function OptQWords(AName: string; ADefault: QWord): QWord;
    function OptFloats(AName: string; ADefault: Double): Double;
    function OptNulls(AName: string; ADefault: Boolean): Boolean;
    function OptStrings(AName: string; ADefault: string): string;
    function OptUnicodeStrings(AName: string; ADefault: UnicodeString): UnicodeString;
    function OptBooleans(AName: string; ADefault: Boolean): Boolean;
    Property OptArrays[AName : String] : TJSONArray Read GetOptArrays;
    property OptObjects[AName: string]: TJSONObject read GetOptObjects;
  end;

  { TISCJSONArray }

  TISCJSONArray = class(TJSONArray)
  private
    function GetOptArrays(Index: Integer): TJSONArray;
    function GetOptObjects(Index: Integer): TJSONObject;
  public
    function OptIntegers(Index: Integer; ADefault: Integer): Integer;
    function OptInt64s(Index: Integer; ADefault: Int64): Int64;
    function OptStrings(Index: Integer; ADefault: string): string;
    function OptQWords(Index: Integer; ADefault: QWord): QWord;
    function OptNulls(Index: Integer; ADefault: Boolean): Boolean;
    function OptUnicodeStrings(Index: Integer; ADefault: UnicodeString): UnicodeString;
    function OptFloats(Index: Integer; ADefault: Double): Double;
    function OptBooleans(Index: Integer; ADefault: Boolean): Boolean;
    property OptArrays[Index: Integer]: TJSONArray read GetOptArrays;
    property OptObjects[Index: Integer]: TJSONObject read GetOptObjects;
  end;

generic function ISCObjectToJSONString<T: TObject>(AObj: T): string;
generic function ISCObjectToJSONObject<T: TObject>(AObj: T): TJSONObject;
generic function ISCJSONStringToObject<T: TObject>(jsonstr: string): T;
generic function ISCJSONObjectToObject<T: TObject>(AJson: TJSONObject): T;

implementation

generic function ISCObjectToJSONString<T>(AObj: T): string;
var
  js: TJSONStreamer;
begin
  js := TJSONStreamer.Create(nil);
  js.Options:= [jsoTStringsAsArray];
  Result := js.ObjectToJSONString(AObj);
  js.Free;
end;

generic function ISCObjectToJSONObject<T>(AObj: T): TJSONObject;
var
  js: TJSONStreamer;
begin
  js := TJSONStreamer.Create(nil);
  js.Options:= [jsoTStringsAsArray];
  Result := js.ObjectToJSON(AObj);
  js.Free;
end;

generic function ISCJSONStringToObject<T>(jsonstr: string): T;
var
  ret: T = nil;
  js: TJSONDeStreamer;
begin
  ret := T.Create;
  js := TJSONDeStreamer.Create(nil);
  js.Options:= [jdoCaseInsensitive,jdoIgnorePropertyErrors];
  js.JSONToObject(jsonstr, ret);
  js.Free;
  Exit(ret);
end;

generic function ISCJSONObjectToObject<T>(AJson: TJSONObject): T;
var
  ret: T = nil;
  js: TJSONDeStreamer;
begin
  ret := T.Create;
  js := TJSONDeStreamer.Create(nil);
  js.Options:= [jdoCaseInsensitive,jdoIgnorePropertyErrors];
  js.JSONToObject(AJson, ret);
  js.Free;
  Exit(ret);
end;

{ TISCJSONArray }

function TISCJSONArray.GetOptArrays(Index: Integer): TJSONArray;
var
  ret: TJSONArray = nil;
begin
  try
    ret := Self.Arrays[Index];
  except
    ret := nil;
  end;
  Exit(ret);
end;

function TISCJSONArray.GetOptObjects(Index: Integer): TJSONObject;
var
  ret: TJSONObject = nil;
begin
  try
    ret := Self.Objects[Index];
  except
    ret := nil;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptIntegers(Index: Integer; ADefault: Integer): Integer;
var
  ret: Integer;
begin
  try
    ret := Self.Integers[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptInt64s(Index: Integer; ADefault: Int64): Int64;
var
  ret: Int64;
begin
  try
    ret := Self.Int64s[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptStrings(Index: Integer; ADefault: string): string;
var
  ret: string;
begin
  try
    ret := Self.Strings[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptQWords(Index: Integer; ADefault: QWord): QWord;
var
  ret: QWord;
begin
  try
    ret := Self.QWords[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptNulls(Index: Integer; ADefault: Boolean): Boolean;
var
  ret: Boolean;
begin
  try
    ret := Self.Nulls[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptUnicodeStrings(Index: Integer; ADefault: UnicodeString
  ): UnicodeString;
var
  ret: UnicodeString;
begin
  try
    ret := Self.UnicodeStrings[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptFloats(Index: Integer; ADefault: Double): Double;
var
  ret: Double;
begin
  try
    ret := Self.Floats[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONArray.OptBooleans(Index: Integer; ADefault: Boolean): Boolean;
var
  ret: Boolean;
begin
  try
    ret := Self.Booleans[Index];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

{ TISCJSONObject }

function TISCJSONObject.GetOptArrays(AName : String): TJSONArray;
var
  arr: TJSONArray = nil;
begin
  try
    arr := Self.Arrays[AName];
  finally
  end;
  Exit(arr);
end;

function TISCJSONObject.GetOptObjects(AName: string): TJSONObject;
var
  obj: TJSONObject = nil;
begin
  try
    obj := Self.Objects[AName];
  finally
  end;
  Exit(obj);
end;

function TISCJSONObject.OptIntegers(AName: String; ADefault: Integer): Integer;
var
  ret: Integer;
begin
  try
    ret:= Self.Integers[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONObject.OptInt64s(AName: string; ADefault: Int64): Int64;
var
  ret: Int64;
begin
  try
    ret:= Self.Int64s[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONObject.OptQWords(AName: string; ADefault: QWord): QWord;
var
  ret: QWord;
begin
  try
    ret:= Self.QWords[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONObject.OptFloats(AName: string; ADefault: Double): Double;
var
  ret: Double;
begin
  try
    ret:= Self.Floats[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONObject.OptNulls(AName: string; ADefault: Boolean): Boolean;
var
  ret: Boolean;
begin
  try
    ret:= Self.Nulls[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONObject.OptStrings(AName: string; ADefault: string): string;
var
  ret: string;
begin
  try
    ret:= Self.Strings[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONObject.OptUnicodeStrings(AName: string; ADefault: UnicodeString
  ): UnicodeString;
var
  ret: UnicodeString;
begin
  try
    ret:= Self.UnicodeStrings[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

function TISCJSONObject.OptBooleans(AName: string; ADefault: Boolean): Boolean;
var
  ret: Boolean;
begin
  try
    ret:= Self.Booleans[AName];
  except
    ret := ADefault;
  end;
  Exit(ret);
end;

end.

