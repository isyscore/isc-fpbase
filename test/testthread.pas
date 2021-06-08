unit testthread;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, ISCThread;

procedure doTestThread();

implementation

procedure doTestThread();
var
  running: Boolean = True;
  st, et: Int64;

  function execImpl(ATID: TThreadID; AData: Integer): Integer;
  begin
    Sleep(2000);
    Exit(AData * 5);
  end;

  procedure termImpl(ATID: TThreadID; AData: Integer);
  begin
    WriteLn('Thread Terminated: %d'.Format([AData]));
    running:= False;
  end;

begin
  st := GetTickCount64;
  specialize ISCThreadExecute<Integer, Integer>(2, @execImpl, @termImpl);
  while running do;
  et:= GetTickCount64;
  WriteLn('time: %d'.Format([et - st]));
end;

end.

