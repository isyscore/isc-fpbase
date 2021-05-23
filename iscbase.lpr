program iscbase;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, testUTF8, testGeneric, fphttp, fphttpclient, fgl;

begin
  doTestUTF8();
  doTestGeneric();
end.

