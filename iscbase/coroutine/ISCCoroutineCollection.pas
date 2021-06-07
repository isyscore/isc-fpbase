unit ISCCoroutineCollection;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, ISCCoroutine;

type

  IISCCollection = interface
    ['{8C7102EE-F1E2-4380-B057-B52B0844ED9E}']
    //property methods
    function GetCount: Cardinal;
    function GetGroup(const AIndex: Integer): IISCCollection;
    function GetCoroutine(const ACoroutineID: String): IISCCoroutine;
    function GetCoroutineGroups: TStringArray;

    //properties
    property CoroutineGroups : TStringArray read GetCoroutineGroups;
    property Count : Cardinal read GetCount;
    property Groups[Const AIndex:Integer] : IISCCollection read GetGroup;
    property Coroutines[Const ACoroutineID:String] : IISCCoroutine read GetCoroutine;

    //methods
    procedure Add(Const ACoroutine:IISCCoroutine);overload;
    procedure Add(Const ACoroutine:IISCCoroutine;Out Index:Integer);overload;
    procedure Remove(Const ACoroutine:IISCCoroutine);
    function Exists(Const AGroupID:String):Boolean;overload;
    function Exists(Const ACoroutine:IISCCoroutine):Boolean;overload;
    function IndexOf(Const AGroupID:String;Out Index:Integer):Boolean;overload;
  end;

  TISCCollectionImpl = class(TInterfacedObject,IISCCollection)
  strict private
    type
      TCoroutineGroup = TFPGInterfacedObjectList<IISCCoroutine>;
      TGroupMap = TFPGMapObject<String,TCoroutineGroup>;
  strict private
    FGroups: TGroupMap;
    function GetCount: Cardinal;
    function GetGroup(const AIndex: Integer): IISCCollection;
    function GetCoroutine(const ACoroutineID: String): IISCCoroutine;
    function GetCoroutineGroups: TStringArray;
  strict protected
    procedure UnsafeAdd(Const ACoroutine:IISCCoroutine; Out Index:Integer);
  public
    property CoroutineGroups : TStringArray read GetCoroutineGroups;
    property Count : Cardinal read GetCount;
    property Groups[Const AIndex:Integer] : IISCCollection read GetGroup;
    property Coroutines[Const ACoroutineID:String] : IISCCoroutine read GetCoroutine;

    procedure Add(Const ACoroutine:IISCCoroutine);overload;
    procedure Add(Const ACoroutine:IISCCoroutine;Out Index:Integer);overload;
    procedure Remove(Const ACoroutine:IISCCoroutine);
    function Exists(Const AGroupID:String):Boolean;overload;
    function Exists(Const ACoroutine:IISCCoroutine):Boolean;overload;
    function IndexOf(Const AGroupID:String;Out Index:Integer):Boolean;overload;
    constructor Create;virtual;
    destructor Destroy; override;
  end;

implementation

uses
  syncobjs;

var
  Critical : TCriticalSection;

function TISCCollectionImpl.GetCount: Cardinal;
begin
  Critical.Enter;
  try
    Result:=FGroups.Count;
  finally
    Critical.Leave;
  end;
end;

function TISCCollectionImpl.GetGroup(const AIndex: Integer): IISCCollection;
var
  I,J:Integer;
  LGroup:TCoroutineGroup;
  LResult:TISCCollectionImpl;
begin
  //create a new collection
  LResult:=TISCCollectionImpl.Create;
  Result:=LResult;

  //no need to enter if no groups exist
  if AIndex < 0 then
    Exit;

  Critical.Enter;
  try
    //one more check to make sure we aren't out of bounds after aquiring lock
    if AIndex > Pred(FGroups.Count) then
      Exit;

    LGroup:=FGroups.Data[AIndex];

    //iterate group and add coroutine to result collection
    for I := 0 to Pred(LGroup.Count) do
      LResult.UnsafeAdd(LGroup[I],J);
  finally
    Critical.Leave;
  end;
end;

function TISCCollectionImpl.GetCoroutine(const ACoroutineID: String): IISCCoroutine;
var
  I,J:Integer;
begin
  Result:=nil;

  Critical.Enter;
  try
    //iterate all groups, and items in the group until we find a matching id
    for I := 0 to Pred(FGroups.Count) do
      for J := 0 to Pred(FGroups.Data[I].Count) do
        if FGroups.Data[I].Items[J].Settings.Await.CoroutineID = ACoroutineID then
        begin
          Result:=FGroups.Data[I].Items[J];
          Exit;
        end;
  finally
    Critical.Leave;
  end;
end;

function TISCCollectionImpl.GetCoroutineGroups: TStringArray;
var
  I:Integer;
begin
  Critical.Enter;
  try
    SetLength(Result,FGroups.Count);
    for I := 0 to Pred(FGroups.Count) do
      Result[I]:=FGroups.Keys[I];
  finally
    Critical.Leave;
  end;
end;

procedure TISCCollectionImpl.UnsafeAdd(const ACoroutine: IISCCoroutine; Out Index:Integer);
var
  I:Integer;
  LGroup:TCoroutineGroup;
  LID:String;
  LCoroutine,
  LGroupCoroutine : IISCCoroutine;
begin
  LCoroutine:=ACoroutine;

  //get the group id and look inside to see if we found it
  LID:=LCoroutine.Settings.Await.GroupID;
  Index:=FGroups.IndexOf(LID);

  //if this coroutine group already exists, just add to it
  if Index >= 0 then
  begin
    //now get the coroutine id to search the group
    LID := LCoroutine.Settings.Await.CoroutineID;
    LGroup:=FGroups.Data[Index];

    //only add to the group if we haven't already done so
    for I:=0 to Pred(LGroup.Count) do
    begin
      //grab a ref to the group coroutine
      LGroupCoroutine := LGroup.Items[I];

      if not Assigned(LGroupCoroutine) then
        Continue;

      //check if this item matches the input's coroutine id, if so we can
      //avoid the add altogether
      if LGroupCoroutine.Settings.Await.CoroutineID = LID then
        Exit;
    end;

    //otherwise the coroutine id did not exist, go ahead and add it
    LGroup.Add(LCoroutine);
  end
  else
  begin
    //if the coroutine group doesn't exist, create it and
    //add the coroutine to the group
    LGroup:=TCoroutineGroup.Create;
    LGroup.Add(LCoroutine);
    Index:=FGroups.Add(
      LID,
      LGroup
    );
  end;
end;

procedure TISCCollectionImpl.Add(const ACoroutine: IISCCoroutine);
var
  I:Integer;
begin
  Add(ACoroutine,I);
end;

procedure TISCCollectionImpl.Add(const ACoroutine: IISCCoroutine; out Index: Integer);
begin
  Critical.Enter;
  try
    UnsafeAdd(ACoroutine,Index);
  finally
    Critical.Leave;
  end;
end;

procedure TISCCollectionImpl.Remove(const ACoroutine: IISCCoroutine);
var
  I,J,K:Integer;
  LGroup:TCoroutineGroup;
  LCoroutineID:String;
begin
  LCoroutineID:=ACoroutine.Settings.Await.CoroutineID;
  if IndexOf(ACoroutine.Settings.Await.GroupID,I) then
  begin
    Critical.Enter;
    try
      LGroup:=FGroups.Data[I];

      K:=-1;
      //find the index of the coroutine in the group
      for J:=0 to Pred(LGroup.Count) do
        if LGroup.Items[J].Settings.Await.CoroutineID = LCoroutineID then
        begin
          K:=J;
          break;
        end;

      //delete the coroutine from the group
      if K >= 0 then
        LGroup.Delete(K);

      //if there are no more coroutines in this group, delete the group
      if LGroup.Count < 1 then
        FGroups.Delete(I);
    finally
      Critical.Leave;
    end;
  end;
end;

function TISCCollectionImpl.Exists(const AGroupID: String): Boolean;
var
  I:Integer;
begin
  Result:=IndexOf(AGroupID,I);
end;

function TISCCollectionImpl.Exists(const ACoroutine: IISCCoroutine): Boolean;
var
  I:Integer;
begin
  Result:=IndexOf(ACoroutine.Settings.Await.GroupID,I);
end;

function TISCCollectionImpl.IndexOf(const AGroupID: String;
  out Index: Integer): Boolean;
begin
  Result:=False;

  Critical.Enter;
  try
    Index:=FGroups.IndexOf(AGroupID);
    Result:=Index >= 0;
  finally
    Critical.Leave;
  end;
end;

constructor TISCCollectionImpl.Create;
begin
  FGroups:=TGroupMap.Create(True);
end;

destructor TISCCollectionImpl.Destroy;
begin
  FGroups.Free;
  inherited Destroy;
end;

initialization
  Critical:=TCriticalSection.Create;
finalization
  if Assigned(Critical) then
    Critical.Free;
end.

