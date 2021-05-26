unit ISCThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TISCThreadMethod<T> = function (AData: TStringArray): T;
  generic TISCTerminateMethod<T> = procedure (AData: T);

  { TInnerThread }

  generic TInnerThread<T> = class(TThread)
  private
    FData: TStringArray;
    FOutData: T;
    FExecMethod: specialize TISCThreadMethod<T>;
    FOutMethod: specialize TISCTerminateMethod<T>;
  protected
    procedure Execute; override;
  public
    constructor Create(InnerData: array of string; AMethod: specialize TISCThreadMethod<T>; ATermMethod: specialize TISCTerminateMethod<T>);
  end;

generic procedure ISCThreadExecute<U>(AData: TStringArray; AMethod: specialize TISCThreadMethod<U>; AOnTerminate: specialize TISCTerminateMethod<U>);

implementation

{ TInnerThread }

procedure TInnerThread.Execute;
begin
  FOutData := FExecMethod(FData);
  if (Assigned(FOutMethod)) then begin
    FOutMethod(FOutData);
  end;
  Self.Free;
end;

constructor TInnerThread.Create(InnerData: array of string;
  AMethod: specialize TISCThreadMethod<T>; ATermMethod: specialize TISCTerminateMethod<T>);
var
  i: Integer;
begin
  inherited Create(True);
  // FreeOnTerminate:= True;
  SetLength(FData, Length(InnerData));
  for i := 0 to Length(InnerData) -1 do begin
    FData[i] := InnerData[i];
  end;
  FExecMethod:= AMethod;
  FOutMethod:= ATermMethod;
end;

generic procedure ISCThreadExecute<U>(AData: TStringArray; AMethod: specialize TISCThreadMethod<U>; AOnTerminate: specialize TISCTerminateMethod<U>);
begin
  with specialize TInnerThread<U>.Create(AData, AMethod, AOnTerminate) do begin
    Start();
  end;
end;

end.

