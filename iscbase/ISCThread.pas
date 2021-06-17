unit ISCThread;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils;

type
  generic TISCThreadMethod<D, T> = function (ATID: TThreadID; AData: D): T;
  generic TISCNestedThreadMethod<D, T> = function (ATID: TThreadID; AData: D): T is nested;
  generic TISCTerminateMethod<T> = procedure (ATID: TThreadID; AData: T);
  generic TISCNestedTerminateMethod<T> = procedure (ATID: TThreadID; AData: T) is nested;
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

  { TInnerNestedThread }

  generic TInnerNestedThread<D, T> = class(TThread)
  private
    FData: D;
    FOutData: T;
    FExecMethod: specialize TISCNestedThreadMethod<D, T>;
    FOutMethod: specialize TISCNestedTerminateMethod<T>;
  protected
    procedure Execute; override;
  public
    constructor Create(InnerData: D; AMethod: specialize TISCNestedThreadMethod<D, T>; ATermMethod: specialize TISCNestedTerminateMethod<T>);
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
generic function ISCThreadExecute<E, U>(AData: E; AMethod: specialize TISCThreadMethod<E, U>; AOnTerminate: specialize TISCTerminateMethod<U> = nil; APriority: TThreadPriority = tpNormal): TThread;
{ ISCThreadExecute for Non-GUI-Nested operating, "OnTerminate" callbacks in Nested-SUB-THREAD }
generic function ISCThreadExecute<E, U>(AData: E; AMethod: specialize TISCNestedThreadMethod<E, U>; AOnTerminate: specialize TISCNestedTerminateMethod<U> = nil; APriority: TThreadPriority = tpNormal): TThread;
{ ISCThreadExecuteGUI for GUI operating, "OnTerminate" callbacks in UI-THREAD }
generic function ISCThreadExecuteGUI<E, U>(AData: E; AMethod: specialize TISCThreadMethodGUI<E, U>; AOnTerminate: specialize TISCTerminateMethodGUI<U> = nil; APriority: TThreadPriority = tpNormal): TThread;

implementation

{ TInnerNestedThread }

procedure TInnerNestedThread.Execute;
begin
  FOutData:= FExecMethod(ThreadID, FData);
  if (Assigned(FOutMethod)) then begin
    FOutMethod(ThreadID, FOutData);
  end;
  Self.Free;
end;

constructor TInnerNestedThread.Create(InnerData: D; AMethod: specialize TISCNestedThreadMethod<D, T>; ATermMethod: specialize TISCNestedTerminateMethod<T>);
begin
  inherited Create(True);
  FData:= InnerData;
  FExecMethod:= AMethod;
  FOutMethod:= ATermMethod;
end;

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

generic function ISCThreadExecute<E, U>(AData: E; AMethod: specialize TISCThreadMethod<E, U>; AOnTerminate: specialize TISCTerminateMethod<U>; APriority: TThreadPriority): TThread;
var
  th: specialize TInnerThread<E, U>;
begin
  th := specialize TInnerThread<E, U>.Create(AData, AMethod, AOnTerminate);
  th.Priority := APriority;
  th.Start();
  Exit(th);
end;

generic function ISCThreadExecute<E, U>(AData: E; AMethod: specialize TISCNestedThreadMethod<E, U>; AOnTerminate: specialize TISCNestedTerminateMethod<U> = nil; APriority: TThreadPriority = tpNormal): TThread;
var
  th: specialize TInnerNestedThread<E, U>;
begin
  th := specialize TInnerNestedThread<E, U>.Create(AData, AMethod, AOnTerminate);
  th.Priority:= APriority;
  th.Start;
  Exit(th);
end;

generic function ISCThreadExecuteGUI<E, U>(AData: E; AMethod: specialize TISCThreadMethodGUI<E, U>; AOnTerminate: specialize TISCTerminateMethodGUI<U>; APriority: TThreadPriority): TThread;
var
  th: specialize TInnerThreadGUI<E, U>;
begin
  th := specialize TInnerThreadGUI<E, U>.Create(AData, AMEthod, AOnTerminate);
  th.Priority := APriority;
  th.Start();
  Exit(th);
end;

end.

