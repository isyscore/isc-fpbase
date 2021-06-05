program iscbase;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, testUTF8, testGeneric, fphttp, fphttpclient, fgl,
  testthread, ISCGeneric, ISCThread, ISCUTF8Helper, ISCHttp, ISCStringUtil,
  ISCConsts, ISCLogger, ISCJSON, testJson, testyaml, testdatabase, testNetwork,
  testscript;

begin
  // doTestUTF8();
  // doTestGeneric();
  // doTestJson();
  // doTestGeneric2();
  // doTestYaml();
  // doTestDatabase();
  doTestNetwork();
end.

