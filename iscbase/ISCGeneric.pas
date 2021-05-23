unit ISCGeneric;

{$mode objfpc}{$H+}
{$Warnings off}
{$hints off}
{$Notes off}

interface

uses
  Classes, SysUtils, fgl;

type
  { TISCPair }

  generic TISCPair<A, B> = class
  private
    FFirst: A;
    FSecond: B;
  public
    constructor Create(AFirst: A; ASecond: B);
    property First: A read FFirst write FFirst;
    property Second: B read FSecond write FSecond;
  end;

  { TISCTriple }

  generic TISCTriple<A, B, C> = class
  private
    FFirst: A;
    FSecond: B;
    FThird: C;
  public
    constructor Create(AFirst: A; ASecond: B; Athird: C);
    property First: A read FFirst write FFirst;
    property Second: B read FSecond write FSecond;
    property Third: C read FThird write FThird;
  end;

  generic TMapMethod<T, R> = function(item: T): R;
  generic TMapIndexedMethod<T, R> = function(index: Integer; item: T): R;
  generic TFilterMethod<T> = function(item: T): Boolean;
  generic TFilterIndexedMethod<T> = function(index: Integer; item: T): Boolean;
  generic TSortMethod<T> = function(item1: T; item2: T): Integer;
  generic TForeachMethod<T> = procedure (item: T);
  generic TForeachIndexedMethod<T> = procedure (index: Integer; item: T);
  generic TReduceMethod<S, T> = function (acc: S; item: T): S;
  generic TReduceIndexedMethod<S, T> = function (index: Integer; acc: S; item: T): S;
  generic TJoinToMethod<T> = function (item: T): string;
  generic TMapConditionMethod<K, V> = function (key: K; value: V): Boolean;
  generic TMapForeachMethod<K, V> = procedure (key: K; value: V);

  { TISCList }

  generic TISCList<T> = class(specialize TFPGList<T>)
  public
    class function FromItems(i: array of T): TISCList;
    class function FromFPGList(o: specialize TFPGList<T>): TISCList;
    function Filter(block: specialize TFilterMethod<T>): TISCList;
    function Contains(item: T): Boolean;
    function LastIndexOf(item: T): Integer;
    function Find(block: specialize TFilterMethod<T>): T;
    function FindLast(block: specialize TFilterMethod<T>): T;
    function IndexOfFirst(block: specialize TFilterMethod<T>): Integer;
    function IndexOfLast(block: specialize TFilterMethod<T>): Integer;
    function Drop(n: Integer): TISCList;
    function DropLast(n: Integer): TISCList;
    function FilterIndexed(block: specialize TFilterIndexedMethod<T>): TISCList;
    function FilterNot(block: specialize TFilterMethod<T>): TISCList;
    function FilterNotIndexed(block: specialize TFilterIndexedMethod<T>): TISCList;
    function SubList(startIndex: Integer): TISCList;
    function SubList(startIndex: Integer; endIndex: Integer): TISCList;
    function Take(n: Integer): TISCList;
    function TakeLast(n: Integer): TISCList;
    function Distinct(): TISCList;
    function All(block: specialize TFilterMethod<T>) : Boolean;
    function Any(block: specialize TFilterMethod<T>) : Boolean;
    function None(block: specialize TFilterMethod<T>) : Boolean;
    function CountItem(block: specialize TFilterMethod<T>) : Integer;
    procedure ForEach(block: specialize TForeachMethod<T>);
    procedure ForEachIndexed(block: specialize TForeachIndexedMethod<T>);
    function Minus(otherList: specialize TFPGList<T>): TISCList;
    function Plus(otherList: specialize TFPGList<T>): TISCList;
    function JoinTo(separator: string; block: specialize TJoinToMethod<T>): string;
    function JoinTo(separator: string; prefix: string; postfix: string; block: specialize TJoinToMethod<T>): string;
  end;

  { TISCMap }

  generic TISCMap<K, V> = class(specialize TFPGMap<K, V>)
  public
    class function FromItems(ks: array of K; vs: array of V): TISCMap;
    class function FromFPGMap(o: specialize TFPGMap<K, V>): TISCMap;
    function All(block: specialize TMapConditionMethod<K, V>): Boolean;
    function Any(block: specialize TMapConditionMethod<K, V>): Boolean;
    function None(block: specialize TMapConditionMethod<K, V>): Boolean;
    function CountItem(block: specialize TMapConditionMethod<K, V>): Integer;
    procedure ForEach(block: specialize TMapForeachMethod<K, V>);
    function ContainsKey(key: K): Boolean;
    function ContainsValue(value: V): Boolean;
    function Filter(block: specialize TMapConditionMethod<K, V>): TISCMap;
    function FilterNot(block: specialize TMapConditionMethod<K, V>): TISCMap;
    function KeyList(): specialize TFPGList<K>;
    function ValueList(): specialize TFPGList<V>;
  end;

// of method
generic function ISCListOf<T>(items: array of T): specialize TFPGList<T>;
generic function ISCMapOf<K, V>(keys: array of K; values: array of V): specialize TFPGMap<K, V>;

// generic list
generic procedure ISCFreeList<T>(list: specialize TFPGList<T>);
generic function ISCMap<T, R>(list: specialize TFPGList<T>; block: specialize TMapMethod<T, R>): specialize TFPGList<R>;
generic function ISCFilter<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): specialize TFPGList<T>;
generic function ISCContains<T>(list: specialize TFPGList<T>; item: T): Boolean;
generic function ISCIndexOf<T>(list: specialize TFPGList<T>; item: T): Integer;
generic function ISCLastIndexOf<T>(list: specialize TFPGList<T>; item: T): Integer;
generic function ISCFind<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): T;
generic function ISCFindLast<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): T;
generic function ISCIndexOfFirst<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): Integer;
generic function ISCIndexOfLast<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): Integer;
generic function ISCDrop<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
generic function ISCDropLast<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
generic function ISCFilterIndexed<T>(list: specialize TFPGList<T>; block: specialize TFilterIndexedMethod<T>): specialize TFPGList<T>;
generic function ISCMapIndexed<T, R>(list: specialize TFPGList<T>; block: specialize TMapIndexedMethod<T, R>): specialize TFPGList<R>;
generic function ISCFilterNot<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): specialize TFPGList<T>;
generic function ISCFilterNotIndexed<T>(list: specialize TFPGList<T>; block: specialize TFilterIndexedMethod<T>): specialize TFPGList<T>;
generic function ISCSubList<T>(list: specialize TFPGList<T>; startIndex: Integer): specialize TFPGList<T>;
generic function ISCSubList<T>(list: specialize TFPGList<T>; startIndex: Integer; endIndex: Integer): specialize TFPGList<T>;
generic function ISCTake<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
generic function ISCTakeLast<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
generic function ISCDistinct<T>(list: specialize TFPGList<T>): specialize TFPGList<T>;
generic function ISCAll<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Boolean;
generic function ISCAny<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Boolean;
generic function ISCNone<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Boolean;
generic function ISCCount<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Integer;
generic procedure ISCForEach<T>(list: specialize TFPGList<T>; block: specialize TForeachMethod<T>);
generic procedure ISCForEachIndexed<T>(list: specialize TFPGList<T>; block: specialize TForeachIndexedMethod<T>);
generic function ISCReduce<S, T>(list: specialize TFPGList<T>; block: specialize TReduceMethod<S, T>): S;
generic function ISCReduceIndexed<S, T>(list: specialize TFPGList<T>; block: specialize TReduceIndexedMethod<S, T>): S;
generic function ISCMinus<T>(list: specialize TFPGList<T>; otherList: specialize TFPGList<T>): specialize TFPGList<T>;
generic function ISCPlus<T>(list: specialize TFPGList<T>; otherList: specialize TFPGList<T>): specialize TFPGList<T>;
generic function ISCJoinTo<T>(list: specialize TFPGList<T>; separator: string; block: specialize TJoinToMethod<T>): string;

// generic map
generic procedure ISCFreeMap<K, V>(map: specialize TFPGMap<K, V>);
generic procedure ISCFreeMapKey<K, V>(map: specialize TFPGMap<K, V>);
generic procedure ISCFreeValue<K, V>(map: specialize TFPGMap<K, V>);
generic function ISCToList<K, V>(map: specialize TFPGMap<K, V>): specialize TFPGList<specialize TISCPair<K, V>>;
generic function ISCAll<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Boolean;
generic function ISCAny<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Boolean;
generic function ISCNone<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Boolean;
generic function ISCCount<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Integer;
generic procedure ISCForEach<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapForeachMethod<K, V>);
generic function ISCContainsKey<K, V>(map: specialize TFPGMap<K, V>; key: K): Boolean;
generic function ISCContainsValue<K, V>(map: specialize TFPGMap<K, V>; value: V): Boolean;
generic function ISCFilter<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): specialize TFPGMap<K, V>;
generic function ISCFilterNot<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): specialize TFPGMap<K, V>;
generic function ISCKeys<K, V>(map: specialize TFPGMap<K, V>): specialize TFPGList<K>;
generic function ISCValues<K, V>(map: specialize TFPGMap<K, V>): specialize TFPGList<V>;

implementation

generic function ISCMapOf<K, V>(keys: array of K; values: array of V): specialize TFPGMap<K, V>;
var
  i: Integer;
  ret: specialize TFPGMap<K, V>;
begin
  if (Length(Keys) <> Length(Values)) then begin
    raise Exception.Create('number of keys is different from number of values.');
  end;
  ret := specialize TFPGMap<K, V>.Create;
  for i := 0 to Length(keys) - 1 do begin
    ret.Add(keys[i], values[i]);
  end;
  Exit(ret);
end;

generic function ISCListOf<T>(items: array of T): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to Length(items) - 1 do begin
    ret.Add(items[i]);
  end;
  Exit(ret);
end;

generic procedure ISCFreeMap<K, V>(map: specialize TFPGMap<K, V>);
var
  i: Integer;
begin
  for i:= 0 to map.Count - 1 do begin
    map.Keys[i].Free;
    map.Data[i].Free;
  end;
  map.Free;
end;

generic procedure ISCFreeMapKey<K, V>(map: specialize TFPGMap<K, V>);
var
  i: Integer;
begin
  for i:= 0 to map.Count - 1 do begin
    map.Keys[i].Free;
  end;
  map.Free;
end;

generic procedure ISCFreeValue<K, V>(map: specialize TFPGMap<K, V>);
var
  i: Integer;
begin
  for i:= 0 to map.Count - 1 do begin
    map.Data[i].Free;
  end;
  map.Free;
end;

generic function ISCKeys<K, V>(map: specialize TFPGMap<K, V>): specialize TFPGList<K>;
var
  ret: specialize TFPGList<K>;
  i: Integer;
begin
  ret := specialize TFPGList<K>.Create;
  for i := 0 to map.Count - 1 do begin
    ret.Add(map.Keys[i]);
  end;
  Exit(ret);
end;

generic function ISCValues<K, V>(map: specialize TFPGMap<K, V>): specialize TFPGList<V>;
var
  ret: specialize TFPGList<V>;
  i: Integer;
begin
  ret := specialize TFPGList<V>.Create;
  for i := 0 to map.Count - 1 do begin
    ret.Add(map.Data[i]);
  end;
  Exit(ret);
end;

generic function ISCContainsKey<K, V>(map: specialize TFPGMap<K, V>; key: K): Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i:= 0 to map.Count - 1 do begin
    if (map.Keys[i] = key) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCContainsValue<K, V>(map: specialize TFPGMap<K, V>; value: V): Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i:= 0 to map.Count - 1 do begin
    if (map.Data[i] = value) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCFilter<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): specialize TFPGMap<K, V>;
var
  ret: specialize TFPGMap<K, V>;
  i: Integer;
begin
  ret := specialize TFPGMap<K, V>.Create;
  for i := 0 to map.Count - 1 do begin
    if (block(map.Keys[i], map.Data[i])) then begin
      ret.Add(map.Keys[i], map.Data[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCFilterNot<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): specialize TFPGMap<K, V>;
var
  ret: specialize TFPGMap<K, V>;
  i: Integer;
begin
  ret := specialize TFPGMap<K, V>.Create;
  for i := 0 to map.Count - 1 do begin
    if (not block(map.Keys[i], map.Data[i])) then begin
      ret.Add(map.Keys[i], map.Data[i]);
    end;
  end;
  Exit(ret);
end;

generic procedure ISCForEach<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapForeachMethod<K, V>);
var
  i: Integer;
begin
  for i := 0 to map.Count - 1 do begin
   block(map.Keys[i], map.Data[i]);
  end;
end;

generic function ISCCount<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Integer;
var
  ret: Integer = 0;
  i: Integer;
begin
  for  i:= 0 to map.Count - 1 do begin
    if (block(map.Keys[i], map.Data[i])) then begin
      Inc(ret);
    end;
  end;
  Exit(ret);
end;

generic function ISCAll<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to map.Count -1 do begin
    if (not block(map.Keys[i], map.Data[i])) then begin
      ret := False;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCAny<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i := 0 to map.Count -1 do begin
    if (block(map.Keys[i], map.Data[i])) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCNone<K, V>(map: specialize TFPGMap<K, V>; block: specialize TMapConditionMethod<K, V>): Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to map.Count -1 do begin
    if (block(map.Keys[i], map.Data[i])) then begin
      Result := False;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCToList<K, V>(map: specialize TFPGMap<K, V>): specialize TFPGList<specialize TISCPair<K, V>>;
var
  i: Integer;
  ret: specialize TFPGList<specialize TISCPair<K, V>>;
  item: specialize TISCPair<K, V>;
begin
  ret := specialize TFPGList<specialize TISCPair<K, V>>.Create;
  for i := 0 to map.Count - 1 do begin
    item := specialize TISCPair<K, V>.Create(map.Keys[i], map.Data[i]);
    ret.Add(item);
  end;
  Exit(ret);
end;

generic function ISCJoinTo<T>(list: specialize TFPGList<T>; separator: string; block: specialize TJoinToMethod<T>): string;
var
  i: Integer;
  ret: string = '';
begin
  for i :=  0 to list.Count - 1 do begin
    if (i = 0) then begin
      ret += block(list[i]);
    end else begin
      ret += separator + block(list[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCMinus<T>(list: specialize TFPGList<T>; otherList: specialize TFPGList<T>): specialize TFPGList<T>;
var
  i: Integer;
  ret: specialize TFPGList<T>;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to list.Count - 1 do begin
    if (otherList.IndexOf(list[i]) = -1) then begin
      ret.Add(list[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCPlus<T>(list: specialize TFPGList<T>; otherList: specialize TFPGList<T>): specialize TFPGList<T>;
var
  i: Integer;
  ret: specialize TFPGList<T>;
begin
  ret := specialize TFPGList<T>.Create;
  ret.AddList(list);
  for i := 0 to otherList.Count - 1 do begin
    if (ret.IndexOf(otherList[i]) = -1) then begin
      ret.Add(otherList[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCReduceIndexed<S, T>(list: specialize TFPGList<T>; block: specialize TReduceIndexedMethod<S, T>): S;
var
  ret: S;
  i: Integer;
begin
  if (list.Count > 0) then begin
    ret := list[0];
  end;
  for i:= 1 to list.Count - 1 do begin
    ret := block(i, ret, list[i]);
  end;
  Exit(ret);
end;

generic function ISCReduce<S, T>(list: specialize TFPGList<T>; block: specialize TReduceMethod<S, T>): S;
var
  ret: S;
  i: Integer;
begin
  if (list.Count > 0) then begin
    ret := list[0];
  end;
  for i:= 1 to list.Count - 1 do begin
    ret := block(ret, list[i]);
  end;
  Exit(ret);
end;

generic procedure ISCForEach<T>(list: specialize TFPGList<T>; block: specialize TForeachMethod<T>);
var
  i: Integer;
begin
  for i:= 0 to list.Count - 1 do begin
    block(list[i]);
  end;
end;

generic procedure ISCForEachIndexed<T>(list: specialize TFPGList<T>; block: specialize TForeachIndexedMethod<T>);
var
  i: Integer;
begin
  for i:= 0 to list.Count - 1 do begin
    block(i, list[i]);
  end;
end;

generic function ISCCount<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Integer;
var
  ret: Integer = 0;
  i: Integer;
begin
  for i := 0 to list.Count - 1 do begin
    if (block(list[i])) then begin
      Inc(ret);
    end;
  end;
  Exit(ret);
end;

generic function ISCAll<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to list.Count - 1 do begin
    if (not block(list[i])) then begin
      ret := False;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCAny<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i := 0 to list.Count - 1 do begin
    if (block(list[i])) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCNone<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>) : Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to list.Count - 1 do begin
    if (block(list[i])) then begin
      ret := False;
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCDistinct<T>(list: specialize TFPGList<T>): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to list.Count - 1 do begin
    if (ret.IndexOf(list[i]) = -1) then begin
      ret.Add(list[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCTake<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to n - 1 do begin
    ret.Add(list[i]);
  end;
  Exit(ret);
end;

generic function ISCTakeLast<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret := specialize TFPGList<T>.Create;
  for i := list.Count - n to list.Count - 1 do begin
    ret.Add(list[i]);
  end;
  Exit(ret);
end;

generic function ISCSubList<T>(list: specialize TFPGList<T>; startIndex: Integer): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret:= specialize TFPGList<T>.Create;
  for i := startIndex to list.Count - 1 do begin
    ret.Add(list[i]);
  end;
  Exit(ret);
end;

generic function ISCSubList<T>(list: specialize TFPGList<T>; startIndex: Integer; endIndex: Integer): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret:= specialize TFPGList<T>.Create;
  for i := startIndex to endIndex - 1 do begin
    ret.Add(list[i]);
  end;
  Exit(ret);
end;

generic function ISCFilterNot<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to list.Count - 1 do begin
    if (not block(list[i])) then begin
      ret.Add(list[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCFilterNotIndexed<T>(list: specialize TFPGList<T>; block: specialize TFilterIndexedMethod<T>): specialize TFPGList<T>;
var
  ret: specialize TFPGList<T>;
  i: Integer;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to list.Count - 1 do begin
    if (not block(i, list[i])) then begin
      ret.Add(list[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCMapIndexed<T, R>(list: specialize TFPGList<T>; block: specialize TMapIndexedMethod<T, R>): specialize TFPGList<R>;
var
  i:  Integer;
  ret: specialize TFPGList<R>;
begin
  ret := specialize TFPGList<R>.Create;
  for i:= 0 to list.Count - 1 do begin
    ret.Add(block(i, list[i]));
  end;
  Exit(ret);
end;

generic function ISCFilterIndexed<T>(list: specialize TFPGList<T>; block: specialize TFilterIndexedMethod<T>): specialize TFPGList<T>;
var
  i: Integer;
  ret: specialize TFPGList<T>;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to list.Count - 1 do begin
    if (block(i, list[i])) then begin
      ret.Add(list[i]);
    end;
  end;
  Exit(ret);
end;

generic function ISCDrop<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
var
  i: Integer;
  ret: specialize TFPGList<T>;
begin
  ret := specialize TFPGList<T>.Create;
  for i := n to list.Count - 1 do begin
    ret.Add(list[i]);
  end;
  Exit(ret);
end;

generic function ISCDropLast<T>(list: specialize TFPGList<T>; n: Integer): specialize TFPGList<T>;
var
  i: Integer;
  ret: specialize TFPGList<T>;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to list.Count - 1 - n do begin
    ret.Add(list[i]);
  end;
  Exit(ret);
end;

generic function ISCIndexOfFirst<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): Integer;
var
  idx: Integer = -1;
  i: Integer;
begin
  for i := 0 to list.Count - 1 do begin
    if (block(list[i])) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

generic function ISCIndexOfLast<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): Integer;
var
  idx: Integer = -1;
  i: Integer;
begin
  for i := list.Count - 1 downto 0 do begin
    if (block(list[i])) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

generic function ISCFind<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): T;
var
  ret: T;
  i: Integer;
begin
  for i := 0 to list.Count - 1 do begin
    if (block(list[i])) then begin
      ret := list[i];
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCFindLast<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): T;
var
  ret: T;
  i: Integer;
begin
  for i := list.Count - 1 downto 0 do begin
    if (block(list[i])) then begin
      ret := list[i];
      Break;
    end;
  end;
  Exit(ret);
end;

generic function ISCIndexOf<T>(list: specialize TFPGList<T>; item: T): Integer;
begin
  Exit(list.indexOf(item));
end;

generic function ISCLastIndexOf<T>(list: specialize TFPGList<T>; item: T): Integer;
var
  idx: Integer = -1;
  i: Integer;
begin
  for i := list.Count - 1 downto 0 do begin
    if (list[i] = item) then begin
      idx:= i;
      Break;
    end;
  end;
  Exit(idx);
end;

generic procedure ISCFreeList<T>(list: specialize TFPGList<T>);
var
  i: Integer;
begin
  for i := 0 to list.Count - 1 do begin
    list[i].Free;
  end;
  list.Free;
end;

generic function ISCMap<T, R>(list: specialize TFPGList<T>; block: specialize TMapMethod<T, R>): specialize TFPGList<R>;
var
  i: Integer;
  ret: specialize TFPGList<R>;
begin
  ret := specialize TFPGList<R>.Create;
  for i:= 0 to list.Count - 1 do begin
    ret.Add(block(list[i]));
  end;
  Exit(ret);
end;

generic function ISCFilter<T>(list: specialize TFPGList<T>; block: specialize TFilterMethod<T>): specialize TFPGList<T>;
var
  i: Integer;
  ret: specialize TFPGList<T>;
begin
  ret := specialize TFPGList<T>.Create;
  for i := 0 to list.Count - 1 do begin
    if (block(list[i])) then ret.Add(list[i]);
  end;
  Exit(ret);
end;

generic function ISCContains<T>(list: specialize TFPGList<T>; item: T): Boolean;
begin
  Exit(list.indexOf(item) <> -1);
end;

{ TISCMap }

class function TISCMap.FromItems(ks: array of K; vs: array of V): TISCMap;
var
  i: Integer;
  ret: TISCMap;
begin
  if (Length(ks) <> Length(vs)) then begin
    raise Exception.Create('number of keys is different from number of values.');
  end;
  ret := TISCMap.Create;
  for i := 0 to Length(ks) - 1 do begin
    ret.Add(ks[i], vs[i]);
  end;
  Exit(ret);
end;

class function TISCMap.FromFPGMap(o: specialize TFPGMap<K, V>): TISCMap;
var
  ret: TISCMap;
begin
  ret := TISCMap.Create;
  ret.AddList(o);
  Exit(ret);
end;

function TISCMap.All(block: specialize TMapConditionMethod<K, V>): Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (not block(Self.keys[i], Self.Data[i])) then begin
      ret := False;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCMap.Any(block: specialize TMapConditionMethod<K, V>): Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (block(Self.Keys[i], Self.Data[i])) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCMap.None(block: specialize TMapConditionMethod<K, V>): Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (block(Self.Keys[i], Self.Data[i])) then begin
      ret := False;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCMap.CountItem(block: specialize TMapConditionMethod<K, V>): Integer;
var
  ret: Integer = 0;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (block(Self.Keys[i], Self.Data[i])) then begin
      Inc(ret);
    end;
  end;
  Exit(ret);
end;

procedure TISCMap.ForEach(block: specialize TMapForeachMethod<K, V>);
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    block(Self.Keys[i], Self.Data[i]);
  end;
end;

function TISCMap.ContainsKey(key: K): Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (Self.Keys[i] = key) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCMap.ContainsValue(value: V): Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (Self.Data[i] = value) then begin
      ret:= True;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCMap.Filter(block: specialize TMapConditionMethod<K, V>): TISCMap;
var
  ret: TISCMap;
  i: Integer;
begin
  ret := TISCMap.Create;
  for i := 0 to Self.Count - 1 do begin
    if (block(Self.Keys[i], Self.Data[i])) then begin
      ret.Add(Self.Keys[i], Self.Data[i]);
    end;
  end;
  Exit(ret);
end;

function TISCMap.FilterNot(block: specialize TMapConditionMethod<K, V>): TISCMap;
var
  ret: TISCMap;
  i: Integer;
begin
  ret := TISCMap.Create;
  for i := 0 to Self.Count - 1 do begin
    if (not block(Self.Keys[i], Self.Data[i])) then begin
      ret.Add(Self.Keys[i], Self.Data[i]);
    end;
  end;
  Exit(ret);
end;

function TISCMap.KeyList(): specialize TFPGList<K>;
var
  ret: specialize TISCList<K>;
  i: Integer;
begin
  ret := specialize TISCList<K>.Create;
  for i := 0 to Self.Count - 1 do begin
    ret.Add(Self.Keys[i]);
  end;
  Exit(ret);
end;

function TISCMap.ValueList(): specialize TFPGList<V>;
var
  ret: specialize TISCList<V>;
  i: Integer;
begin
  ret := specialize TISCList<V>.Create;
  for i := 0 to Self.Count - 1 do begin
    ret.Add(Self.Data[i]);
  end;
  Exit(ret);
end;

{ TISCList }

class function TISCList.FromItems(i: array of T): TISCList;
var
  ret: TISCList;
  idx: Integer;
begin
  ret := TISCList.Create;
  for  idx:= 0 to Length(i) - 1 do begin
    ret.Add(i[idx]);
  end;
  Exit(ret);
end;

class function TISCList.FromFPGList(o: specialize TFPGList<T>): TISCList;
var
  ret: TISCList;
begin
  ret := TISCList.Create;
  ret.AddList(o);
  Exit(ret);
end;

function TISCList.Filter(block: specialize TFilterMethod<T>): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for i := 0 to Self.Count - 1 do begin
    if (block(Self[i])) then begin
      ret.Add(Self[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.Contains(item: T): Boolean;
begin
  Exit(Self.IndexOf(item) <> -1);
end;

function TISCList.LastIndexOf(item: T): Integer;
var
  idx: Integer = -1;
  i: Integer;
begin
  for i := Self.Count - 1 downto 0 do begin
    if (Self[i] = item) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TISCList.Find(block: specialize TFilterMethod<T>): T;
var
  ret: T;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (block(Self[i])) then begin
      ret := Self[i];
    end;
  end;
  Exit(ret);
end;

function TISCList.FindLast(block: specialize TFilterMethod<T>): T;
var
  ret: T;
  i: Integer;
begin
  for i := Self.Count - 1 downto 0 do begin
    if (block(Self[i])) then begin
      ret := Self[i];
    end;
  end;
  Exit(ret);
end;

function TISCList.IndexOfFirst(block: specialize TFilterMethod<T>): Integer;
var
  i: Integer;
  idx: Integer = -1;
begin
  for  i := 0 to Self.Count - 1 do begin
    if (block(Self[i])) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TISCList.IndexOfLast(block: specialize TFilterMethod<T>): Integer;
var
  i: Integer;
  idx: Integer = -1;
begin
  for  i := Self.Count - 1 downto 0 do begin
    if (block(Self[i])) then begin
      idx := i;
      Break;
    end;
  end;
  Exit(idx);
end;

function TISCList.Drop(n: Integer): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for  i:= n to Self.Count - 1 do begin
    ret.Add(Self[i]);
  end;
  Exit(ret);
end;

function TISCList.DropLast(n: Integer): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for  i:= 0 to Self.Count - 1 - n do begin
    ret.Add(Self[i]);
  end;
  Exit(ret);
end;

function TISCList.FilterIndexed(block: specialize TFilterIndexedMethod<T>): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for i := 0 to Self.Count- 1 do begin
    if (block(i, Self[i])) then begin
      ret.Add(Self[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.FilterNot(block: specialize TFilterMethod<T>): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for i := 0 to Self.Count- 1 do begin
    if (not block(Self[i])) then begin
      ret.Add(Self[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.FilterNotIndexed(block: specialize TFilterIndexedMethod<T>): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for i := 0 to Self.Count- 1 do begin
    if (not block(i, Self[i])) then begin
      ret.Add(Self[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.SubList(startIndex: Integer): TISCList;
var
  i: Integer;
  ret: TISCList;
begin
  ret := TISCList.Create;
  for i := startIndex to Self.Count - 1 do begin
    ret.Add(Self[i]);
  end;
  Exit(ret);
end;

function TISCList.SubList(startIndex: Integer; endIndex: Integer): TISCList;
var
  i: Integer;
  ret: TISCList;
begin
  ret := TISCList.Create;
  for i := startIndex to endIndex - 1 do begin
    ret.Add(Self[i]);
  end;
  Exit(ret);
end;

function TISCList.Take(n: Integer): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for i := 0 to n - 1 do begin
    ret.Add(Self[i]);
  end;
  Exit(ret);
end;

function TISCList.TakeLast(n: Integer): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for i := Self.Count - n to Self.Count - 1 do begin
    ret.Add(Self[i]);
  end;
  Exit(ret);
end;

function TISCList.Distinct(): TISCList;
var
  ret: TISCList;
  i: Integer;
begin
  ret := TISCList.Create;
  for i := 0 to Self.Count - 1 do begin
    if (ret.IndexOf(Self[i]) = -1) then begin
      ret.Add(Self[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.All(block: specialize TFilterMethod<T>): Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (not block(Self[i])) then begin
      ret := False;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCList.Any(block: specialize TFilterMethod<T>): Boolean;
var
  ret: Boolean = False;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (block(Self[i])) then begin
      ret := True;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCList.None(block: specialize TFilterMethod<T>): Boolean;
var
  ret: Boolean = True;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (block(Self[i])) then begin
      ret := False;
      Break;
    end;
  end;
  Exit(ret);
end;

function TISCList.CountItem(block: specialize TFilterMethod<T>): Integer;
var
  ret: Integer = -1;
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    if (block(Self[i])) then begin
      Inc(ret);
    end;
  end;
  Exit(ret);
end;

procedure TISCList.ForEach(block: specialize TForeachMethod<T>);
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    block(Self[i]);
  end;
end;

procedure TISCList.ForEachIndexed(block: specialize TForeachIndexedMethod<T>);
var
  i: Integer;
begin
  for i := 0 to Self.Count - 1 do begin
    block(i, Self[i]);
  end;
end;

function TISCList.Minus(otherList: specialize TFPGList<T>): TISCList;
var
  i: Integer;
  ret: TISCList;
begin
  ret := TISCList.Create;
  for i := 0 to Self.Count - 1 do begin
    if (otherList.IndexOf(Self[i]) = -1) then begin
      ret.Add(Self[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.Plus(otherList: specialize TFPGList<T>): TISCList;
var
  i: Integer;
  ret: TISCList;
begin
  ret := TISCList.Create;
  ret.AddList(Self);
  for i := 0 to otherList.Count - 1 do begin
    if (ret.IndexOf(otherList[i]) = -1) then begin
      ret.Add(otherList[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.JoinTo(separator: string; block: specialize TJoinToMethod<T>): string;
var
  i: Integer;
  ret: string = '';
begin
  for i := 0 to Self.Count - 1 do begin
    if (i = 0) then begin
      ret += block(Self[i]);
    end else begin
      ret += separator + block(Self[i]);
    end;
  end;
  Exit(ret);
end;

function TISCList.JoinTo(separator: string; prefix: string; postfix: string;
  block: specialize TJoinToMethod<T>): string;
var
  i: Integer;
  ret: string = '';
begin
  for i := 0 to Self.Count - 1 do begin
    if (i = 0) then begin
      ret += block(Self[i]);
    end else begin
      ret += separator + block(Self[i]);
    end;
  end;
  Exit(prefix + ret + postfix);
end;

{ TISCTriple }

constructor TISCTriple.Create(AFirst: A; ASecond: B; Athird: C);
begin
  FFirst:= AFirst;
  FSecond:= ASecond;
  FThird:= Athird;
end;

{ TISCPair }

constructor TISCPair.Create(AFirst: A; ASecond: B);
begin
  FFirst:= AFirst;
  FSecond:= ASecond;
end;

end.

