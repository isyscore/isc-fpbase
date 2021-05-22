unit ISCGeneric;

{$mode objfpc}{$H+}

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
  generic TFilterMethod<T> = function(item: T): Boolean;

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

// drop
// dropLast
// filterIndexed
// mapIndexed
// filterNot
// filterNotIndexed
// sublist
// take
// takeLast
// sortBy
// sortByDescending
// associate
// associateBy
// flatMap
// flatMapIndexed
// groupBy
// distinct
// distinctBy
// union
// all
// any
// none
// count
// forEach
// forEachIndexed
// reduce
// reduceIndexed
// minus
// plus
// joinTo
// joinToString
// toMap

// TODO: generic map
// toList
// flatMap
// map
// all
// any
// none
// count
// forEach
// containsKey
// containsValue
// filterKeys
// filterValues
// keys
// values



implementation

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

