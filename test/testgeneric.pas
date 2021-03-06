unit testGeneric;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ISCGeneric, fgl;

procedure doTestGeneric();
procedure doTestGeneric2();

implementation

function filterImpl(item: string): Boolean;
begin
  Exit(item.Contains('d'));
end;

function mapImpl(item: string): string;
begin
  Exit('hello ' + item);
end;

function joinImpl(item: string): string;
begin
  Exit('"' + item + '"');
end;

procedure doTestGenericList();
var
  list: specialize TISCList<string>;
  l2: specialize TISCList<string>;
  l3: specialize TFPGList<string>;
  s: string;
  str: string;
begin
  list := specialize TISCList<string>.FromItems(['abc','bcd','cde','def','efg']);
  l2 := list.Filter(@filterImpl);
  for s in l2 do begin
    WriteLn(s);
  end;
  l2.Free;
  l3 := specialize ISCMap<string, string>(list, @mapImpl);
  for s in l3 do begin
    WriteLn(s);
  end;
  l3.Free;
  str := list.JoinTo(',', '[', ']', @joinImpl);
  WriteLn(str);
  list.Free;
end;

function mapFilterImpl(k: Integer; v: string): Boolean;
begin
  Exit(k mod 2 = 0);
end;

procedure mapForEachImpl(k: Integer; v: string);
begin
  WriteLn('k = %d, v = %s'.Format([k, v]));
end;

procedure doTestGenericMap();
var
  map: specialize TISCMap<Integer, string>;
  m2: specialize TISCMap<Integer, string>;
begin
  map := specialize TISCMap<Integer, string>.FromItems([1, 2, 3, 4, 5, 6], ['a','b','c','d','e','f']);
  m2 := map.Filter(@mapFilterImpl);
  m2.ForEach(@mapForEachImpl);
  m2.Free;
  map.Free;
end;

procedure doTestGeneric();
begin
  doTestGenericList();
  doTestGenericMap();
end;

function gmap2Impl(item: Integer): string;
begin
  Exit('item %d'.Format([item]));
end;

function gjoin2Impl(item: Integer): string;
begin
  Exit(item.ToString());
end;

procedure doTestGeneric2();
var
  ia: TIntArray = (1, 2, 3, 4, 5, 4, 3, 2, 1, 0);
  ret: Boolean;
  d: TIntArray;
  str: string;
begin
  ret := specialize ISCContains<Integer>(ia, 2);
  WriteLn('c2: ' + BoolToStr(ret, 'true', 'false'));
  ret := specialize ISCContains<Integer>(ia, 6);
  WriteLn('c6: ' + BoolToStr(ret, 'true', 'false'));
  d := specialize ISCDistinct<Integer>(ia);
  str := specialize ISCJoinTo<Integer>(d, ',', '[', ']', @gjoin2Impl);
  WriteLn(str);
end;


end.

