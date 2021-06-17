unit testNetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pingsend, tlntsend, ISCHttp;

procedure doTestNetwork();
procedure doTestHttp();

implementation

procedure doTestNetwork();
var
  ret: Boolean;
  t: TTelnetSend;
begin
  t := TTelnetSend.Create;
  t.TargetHost:= '127.0.0.1';
  t.TargetPort:= '9';
  ret := t.Login;
  t.Logout;
  t.Free;
  WriteLn('telnet: ' + BoolToStr(ret, 'true', 'false'));
end;

procedure doTestHttp();
var
  h : string;
begin
  h := ISCHttpGet('1.com');
  WriteLn(h);
end;

end.

