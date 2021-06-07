unit testCoroutine;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, ISCCoroutine;

procedure doTestCoroutine();
procedure doTestCoroutine2();

implementation

// test no corouting dependency
procedure doTestCoroutine();
var
  st, et: Int64;
  c1, c2: IISCCoroutine;
  r1, r2: Integer;

  procedure m1(const ACorouting: IISCCoroutine);
  begin
    Sleep(2000);
    r1 := 1;
  end;

  procedure m2(const ACorouting: IISCCoroutine);
  begin
    Sleep(1000);
    r2 := 2;
  end;

begin
  st := GetTickCount64;
  c1 := TISCCoroutineImpl.Create;
  c2 := TISCCoroutineImpl.Create;
  c1.Setup(@m1).Start;
  c2.Setup(@m2).Start;
  Await();
  et := GetTickCount64;
  // time near 2000ms
  WriteLn('result: %d, time: %d'.Format([r1 + r2, et - st]));
end;

// test with corouting dependency
procedure doTestCoroutine2();
var
  st, et: Int64;
  c1, c2: IISCCoroutine;
  r1, r2: Integer;

  procedure m1(const ACorouting: IISCCoroutine);
  begin
    Sleep(2000);
    r1 := 1;
  end;

  procedure m2(const ACorouting: IISCCoroutine);
  var
    cid: string;
  begin
    cid := ACorouting['id'];
    // wait m1 complete.
    Await(cid);
    Sleep(1000);
    r2 := 2;
  end;

begin
  st := GetTickCount64;
  c1 := TISCCoroutineImpl.Create;
  c2 := TISCCoroutineImpl.Create;
  c1.Setup(@m1).Start;
  c2.AddArg('id', c1.Settings.Await.GroupID).Setup(@m2).Start;
  Await();
  et := GetTickCount64;
  // time near 3000ms
  WriteLn('result: %d, time: %d'.Format([r1 + r2, et - st]));
end;

end.

