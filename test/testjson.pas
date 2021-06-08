unit testJson;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ISCJSON;

type

  { TInnerClass }

  TInnerClass = class
  private
    FInnerA: string;
    FInnerB: Integer;
  published
    property InnerA: string read FInnerA write FInnerA;
    property InnerB: Integer read FInnerB write FInnerB;
  end;

  { TSampleClass }

  TSampleClass = class
  private
    FFieldA: string;
    FFieldB: Integer;
    FFieldC: TStringList;
    FInner: TInnerClass;
  public
    constructor Create();
    constructor Create(A: string; B: Integer; C: array of string; IA: string; IB: Integer);
    destructor Destroy; override;
  published
    property FieldA: string read FFieldA write FFieldA;
    property FieldB: Integer read FFieldB write FFieldB;
    property FieldC: TStringList read FFieldC write FFieldC;
    property Inner: TInnerClass read FInner write FInner;
  end;

procedure doTestJson();

implementation

procedure doTestJson();
var
  sc: TSampleClass;
  jsonstr: string;
  nsc: TSampleClass;
begin
  sc := TSampleClass.Create('str0', 66, ['x', 'y', 'z'], 'in-str0', 777);
  jsonstr := specialize ISCObjectToJSONString<TSampleClass>(sc);
  WriteLn(jsonstr);
  sc.Free;

  nsc := specialize ISCJSONStringToObject<TSampleClass>(jsonstr);
  WriteLn('nsc.FA = ' + nsc.FieldA);
  WriteLn('nsc.FB = ' + nsc.FieldB.ToString);
  // WriteLn('nsc.FC = ' + nsc.FieldC.jo);

  nsc.Free;
end;

{ TSampleClass }

constructor TSampleClass.Create();
begin
  FieldC := TStringList.Create;
  FInner := TInnerClass.Create;
end;

constructor TSampleClass.Create(A: string; B: Integer; C: array of string;
  IA: string; IB: Integer);
var
  i: Integer;
begin
  FFieldA:= A;
  FFieldB:= B;
  FFieldC := TStringList.Create;
  for i:= 0 to Length(C) - 1 do begin
    FFieldC.Add(C[i]);
  end;
  FInner := TInnerClass.Create;
  FInner.InnerA:= IA;
  FInner.InnerB:= IB;
end;

destructor TSampleClass.Destroy;
begin
  FFieldC.Free;
  FInner.Free;
  inherited Destroy;
end;

end.

