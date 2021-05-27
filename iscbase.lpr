program iscbase;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, testUTF8, testGeneric, fphttp, fphttpclient, fgl, testthread;

begin
  doTestUTF8();
  doTestGeneric();
end.

