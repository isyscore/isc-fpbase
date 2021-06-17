program iscbase;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, sysutils, testUTF8, testGeneric, fphttp,
  fphttpclient, fgl, testthread, ISCGeneric, ISCThread, ISCUTF8Helper, ISCHttp,
  ISCStringUtil, ISCConsts, ISCLogger, ISCJSON, testJson, testyaml,
  testdatabase, testNetwork, testscript, testCoroutine, ISCWebSocketClient,
  ISCWebSocketServer, ISCJNI, ISCDL, ISCJVM;

begin
  // doTestUTF8();
  // doTestGeneric();
  // doTestJson();
  // doTestGeneric2();
  // doTestYaml();
  // doTestDatabase();
  doTestNetwork();
  doTestHttp();
  // doTestThread();
  // doTestCoroutine();
  // doTestCoroutine2();
  // doTestCoroutine3();
  //doTestCoroutine();
  //doTestCoroutine2();
  // doTestThread();
  // doTestSystem();
end.

