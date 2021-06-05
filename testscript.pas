unit testscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSComponent;

procedure doTestScript();

implementation

procedure doTestScript();
var
  s: TPSScript;
begin
  s := TPSScript.Create(nil);

  s.Free;
end;

end.

