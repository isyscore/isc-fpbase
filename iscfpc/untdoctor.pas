unit untDoctor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untConsts, untFPCEnv, untVSCode, fphttpclient;

procedure doDoctor();

implementation

function hasNewVersion(): Boolean;
var
  v: string;
begin
  // check iscfpc's new version
  with TFPHTTPClient.Create(nil) do begin
    try
      v := string(Get(BASE_URL + '/version')).Trim;
    except
      v := VERSION;
    end;
    Free;
  end;
  Exit(v <> VERSION);
end;

function codeStatusToStr(st: TVSCodePluginStatus): string;
begin
  case st of
  psOK: Result := #27'[32m[✓]'#27'[0m';
  psConfigError: Result := #27'[33m[!]'#27'[0m';
  psNotInstalled: Result := #27'[31m[✘]'#27'[0m';
  end;
end;

procedure doDoctor();
var
  newVersion: Boolean;
  fpcInstalled, fpcSrcInstalled, lazarusInstalled: Boolean;
  stOmniPascal, stFreePascalToolkit, stPascalFormatter, stNativeDebug, stCodeLLDB,
  stVSCodeGreatIcons, stGitHistory, stBetterComments, stBookmarks, stBracketPairColorizer  : TVSCodePluginStatus;

begin
  newVersion:= hasNewVersion();
  // fpc
  fpcInstalled:= getFPCInstalled();
  fpcSrcInstalled:= getFPCSrcInstalled();
  lazarusInstalled:= getLazarusInstalled();

  // vscode plugins
  stOmniPascal := getOmniPascalStatus();
  stFreePascalToolkit := getFreePascalToolkitStatus();
  stPascalFormatter := getPascalFormatterStatus();
  stNativeDebug := getNativeDebugStatus();
  stCodeLLDB := getCodeLLDBStatus();
  stVSCodeGreatIcons := getVSCodeGreatIconsStatus();
  stGitHistory := getGitHistoryStatus();
  stBetterComments := getBetterCommentsStatus();
  stBookmarks := getBookmarksStatus();
  stBracketPairColorizer := getBracketPairColorizerStatus();

  // report
  if (newVersion) then begin
    WriteLn('╔════════════════════════════════════════════════════════════════════════════╗');
    WriteLn('║ A new version of iscfpc is available!                                      ║');
    WriteLn('║                                                                            ║');
    WriteLn('║ To update to the latest version, visit the iscfpc release page.            ║');
    WriteLn('║   https://github.com/isyscore/isc-fpbase/releases                          ║');
    WriteLn('╚════════════════════════════════════════════════════════════════════════════╝');
    WriteLn('');
    WriteLn('')
  end;
  WriteLn('Doctor summary:');
  WriteLn('%s FPC'.format([BoolToStr(fpcInstalled, #27'[32m[✓]'#27'[0m', #27'[31m[✘]'#27'[0m')]));
  WriteLn('%s FPC-SRC'.format([BoolToStr(fpcSrcInstalled, #27'[32m[✓]'#27'[0m', #27'[31m[✘]'#27'[0m')]));
  WriteLn('%s Lazarus'.format([BoolToStr(lazarusInstalled, #27'[32m[✓]'#27'[0m', #27'[31m[✘]'#27'[0m')]));
  WriteLn('%s VSCode: OmniPascal'.format([codeStatusToStr(stOmniPascal)]));
  WriteLn('%s VSCode: FreePascal Toolkit'.format([codeStatusToStr(stFreePascalToolkit)]));
  WriteLn('%s VSCode: Pascal Formatter'.format([codeStatusToStr(stPascalFormatter)]));
  WriteLn('%s VSCode: Native Debug'.format([codeStatusToStr(stNativeDebug)]));
  WriteLn('%s VSCode: CodeLLDB'.format([codeStatusToStr(stCodeLLDB)]));
  WriteLn('%s VSCode: Great Icons'.format([codeStatusToStr(stVSCodeGreatIcons)]));
  WriteLn('%s VSCode: Git History'.format([codeStatusToStr(stGitHistory)]));
  WriteLn('%s VSCode: Better Comments'.format([codeStatusToStr(stBetterComments)]));
  WriteLn('%s VSCode: Bookmarks'.format([codeStatusToStr(stBookmarks)]));
  WriteLn('%s VSCode: BracketPairColorizer'.format([codeStatusToStr(stBracketPairColorizer)]));
  WriteLn('');
end;

end.

