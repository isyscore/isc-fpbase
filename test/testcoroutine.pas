unit testCoroutine;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, ISCCoroutine, DateUtils;

procedure doTestCoroutine();
procedure doTestCoroutine2();
procedure doTestCoroutine3();

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
  WriteLn('CID: %s, GID: %s'.Format([c1.Settings.Await.CoroutineID, c1.Settings.Await.GroupID]));
  c2.AddArg('id', c1.Settings.Await.GroupID).Setup(@m2).Start;
  Await();
  et := GetTickCount64;
  // time near 3000ms
  WriteLn('result: %d, time: %d'.Format([r1 + r2, et - st]));
end;

procedure doTestCoroutine3();
var
  c: IISCCoroutine;

  procedure m0(const ACorouting: IISCCoroutine);
  begin
    Sleep(ACorouting['sleep']);
  end;

  procedure ce0(const ACorouting: IISCCoroutine);
  begin
    WriteLn('Force kill elapsed: %d'.Format([MilliSecondsBetween(ACorouting['start'], Now)]));
  end;

begin
  c := TISCCoroutineImpl.Create;
  c.AddArg('sleep', 2000).AddArg('start', Now)
    .Settings.UpdateMaxRuntime(550).UpdateForceTerminate(True)
    .Coroutine
    .Events.UpdateOnStopNestedCallback(@ce0)
    .Coroutine
    .Setup(@m0).Start;

  Await();
  WriteLn('Force kill completed.');
end;

end.

