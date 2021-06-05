unit testNetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pingsend, tlntsend;

procedure doTestNetwork();

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

end.

