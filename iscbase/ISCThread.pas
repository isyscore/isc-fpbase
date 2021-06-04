unit ISCThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic TISCThreadMethod<D, T> = function (ATID: TThreadID; AData: D): T;
  generic TISCTerminateMethod<T> = procedure (ATID: TThreadID; AData: T);
  generic TISCThreadMethodGUI<D, T> = function (ATID: TThreadID; AData: D): T of object;
  generic TISCTerminateMethodGUI<T> = procedure (ATID: TThreadID; AData: T) of object;

  { TInnerThread }

  generic TInnerThread<D, T> = class(TThread)
  private
    FData: D;
    FOutData: T;
    FExecMethod: specialize TISCThreadMethod<D, T>;
    FOutMethod: specialize TISCTerminateMethod<T>;
  protected
    procedure Execute; override;
  public
    constructor Create(InnerData: D; AMethod: specialize TISCThreadMethod<D, T>; ATermMethod: specialize TISCTerminateMethod<T>);
  end;

  { TInnerThreadGUI }

  generic TInnerThreadGUI<D, T> = class(TThread)
  private
    FData: D;
    FOutData: T;
    FExecMethod: specialize TISCThreadMethodGUI<D, T>;
    FOutMethod: specialize TISCTerminateMethodGUI<T>;
    procedure innerTerminate(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(InnerData: D; AMethod: specialize TISCThreadMethodGUI<D, T>; ATermMethod: specialize TISCTerminateMethodGUI<T>);
  end;

{ ISCThreadExecute for Non-GUI operating, "OnTerminate" callbacks in SUB-THREAD }
generic function ISCThreadExecute<E, U>(AData: E; AMethod: specialize TISCThreadMethod<E, U>; AOnTerminate: specialize TISCTerminateMethod<U> = nil; APriority: TThreadPriority = tpNormal): TThreadID;
{ ISCThreadExecuteGUI for GUI operating, "OnTerminate" callbacks in UI-THREAD }
generic function ISCThreadExecuteGUI<E, U>(AData: E; AMethod: specialize TISCThreadMethodGUI<E, U>; AOnTerminate: specialize TISCTerminateMethodGUI<U> = nil; APriority: TThreadPriority = tpNormal): TThreadID;

implementation

{ TInnerThreadGUI }

procedure TInnerThreadGUI.innerTerminate(Sender: TObject);
begin
  if (Assigned(FOutMethod)) then begin
    FOutMethod(ThreadID, FOutData);
  end;
end;

procedure TInnerThreadGUI.Execute;
begin
  FOutData:= FExecMethod(ThreadID, FData);
end;

constructor TInnerThreadGUI.Create(InnerData: D; AMethod: specialize
  TISCThreadMethodGUI<D, T>; ATermMethod: specialize TISCTerminateMethodGUI<T>);
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  OnTerminate:= @innerTerminate;
  FData:= InnerData;
  FExecMethod:= AMethod;
  FOutMethod:= ATermMethod;
end;

{ TInnerThread }

procedure TInnerThread.Execute;
begin
  FOutData := FExecMethod(ThreadID, FData);
  if (Assigned(FOutMethod)) then begin
    FOutMethod(ThreadID, FOutData);
  end;
  Self.Free;
end;

constructor TInnerThread.Create(InnerData: D;
  AMethod: specialize TISCThreadMethod<D, T>; ATermMethod: specialize TISCTerminateMethod<T>);
begin
  inherited Create(True);
  FData:= InnerData;
  FExecMethod:= AMethod;
  FOutMethod:= ATermMethod;
end;

generic function ISCThreadExecute<E, U>(AData: E; AMethod: specialize TISCThreadMethod<E, U>; AOnTerminate: specialize TISCTerminateMethod<U>; APriority: TThreadPriority): TThreadID;
begin
  with specialize TInnerThread<E, U>.Create(AData, AMethod, AOnTerminate) do begin
    Priority := APriority;
    Result := ThreadID;
    Start();
  end;
end;

generic function ISCThreadExecuteGUI<E, U>(AData: E; AMethod: specialize TISCThreadMethodGUI<E, U>; AOnTerminate: specialize TISCTerminateMethodGUI<U>; APriority: TThreadPriority): TThreadID;
begin
  with specialize TInnerThreadGUI<E, U>.Create(AData, AMEthod, AOnTerminate) do begin
    Priority := APriority;
    Result := ThreadID;
    Start();
  end;
end;

end.

