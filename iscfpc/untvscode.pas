unit untVSCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonConf, untConsts;

type
  TVSCodePluginStatus = (psOK, psConfigError, psNotInstalled);

function getOmniPascalStatus(): TVSCodePluginStatus;
function getFreePascalToolkitStatus(): TVSCodePluginStatus;
function getPascalFormatterStatus(): TVSCodePluginStatus;
function getNativeDebugStatus(): TVSCodePluginStatus;
function getCodeLLDBStatus(): TVSCodePluginStatus;
function getVSCodeGreatIconsStatus(): TVSCodePluginStatus;
function getGitHistoryStatus(): TVSCodePluginStatus;
function getBetterCommentsStatus(): TVSCodePluginStatus;
function getBookmarksStatus(): TVSCodePluginStatus;
function getBracketPairColorizerStatus(): TVSCodePluginStatus;

procedure fixVSCodePluginConfig();

implementation

function getExtensionPath(): string;
var
  home: string;
begin
  home := GetEnvironmentVariable(HOME_VAR);
  if (not home.EndsWith(SPL)) then home += SPL;
  Exit(home + '.vscode' + SPL + 'extensions' + SPL);
end;

function getSettingsPath(): string;
var
  home: string;
begin
  home := GetEnvironmentVariable(HOME_VAR);
  if (not home.EndsWith(SPL)) then home += SPL;
  Exit(home + SETTINGS_PATH + 'Code' + SPL + 'User' + SPL + 'settings.json');
end;

function getOmniPascalConfig(): TVSCodePluginStatus;
const
  defaultDevelopmentEnvironment = 'FreePascal';
var
  path: string;
  json: TJSONConfig;
  devEnv: UnicodeString;
  fpcSrc: UnicodeString;
  delphiIns: UnicodeString;
  sp: UnicodeString;
  searchPath: string = LAZ_UTILS_PATH;
begin
  path := getSettingsPath();
  json := TJSONConfig.Create(nil);
  json.Filename:= path;
  devEnv:= json.GetValue('omnipascal.defaultDevelopmentEnvironment', '');
  fpcSrc:= json.GetValue('omnipascal.freePascalSourcePath', '');
  delphiIns:= json.GetValue('omnipascal.delphiInstallationPath', '');
  sp := json.GetValue('omnipascal.searchPath', '');

  if (devEnv = defaultDevelopmentEnvironment) and (fpcSrc = FREEPASCAL_SOURCE_PATH) and (delphiIns = DELPHI_INSTALLATION_PATH) and (string(sp).Contains(searchPath)) then begin
    Result := psOK;
  end else begin
    Result := psConfigError;
  end;

  json.Free;
end;

function findPlugin(pkg: string): Boolean;
var
  path: string;
  src: TSearchRec;
  ret: Boolean = False;
begin
  path := getExtensionPath();
  if (FindFirst(path + pkg + '-*', faDirectory, src) = 0) then begin
    if (FileExists(path + src.Name + SPL + 'package.json')) then begin
      ret := True;
    end;
    FindClose(src);
  end;
  Exit(ret);
end;

function getOmniPascalStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('wosi.omnipascal')) then begin
    ret := getOmniPascalConfig();
  end;
  Exit(ret);
end;

function getFreePascalToolkitStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('coolchyni.fpctoolkit')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

function getPascalFormatterConfig(): TVSCodePluginStatus;
const
  formatterEngine = 'ptop';
var
  path: string;
  json: TJSONConfig;
  engine: UnicodeString;
  enginePath: UnicodeString;
begin
  path := getSettingsPath();
  json := TJSONConfig.Create(nil);
  json.Filename:= path;
  engine:= json.GetValue('pascal.formatter.engine', '');
  enginePath := json.GetValue('pascal.formatter.enginePath', '');

  if (engine = formatterEngine) and (enginePath = PTOP_PATH) then begin
  Result := psOK;
  end else begin
    Result := psConfigError;
  end;

  json.Free;
end;

function getPascalFormatterStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('alefragnani.pascal-formatter')) then begin
    ret := getPascalFormatterConfig();
  end;
  Exit(ret);
end;

function getNativeDebugStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('webfreak.debug')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

function getCodeLLDBStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('vadimcn.vscode-lldb')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

function getVSCodeGreatIconsStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('emmanuelbeziat.vscode-great-icons')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

function getGitHistoryStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('donjayamanne.githistory')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

function getBetterCommentsStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('aaron-bond.better-comments')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

function getBookmarksStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('alefragnani.bookmarks')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

function getBracketPairColorizerStatus(): TVSCodePluginStatus;
var
  ret: TVSCodePluginStatus = psNotInstalled;
begin
  if (findPlugin('coenraads.bracket-pair-colorizer')) then begin
    ret := psOK;
  end;
  Exit(ret);
end;

procedure fixVSCodePluginConfig();
var
  path: string;
  json: TJSONConfig;
  sp: string;
  searchPath: string = LAZ_UTILS_PATH;
begin
  path := getSettingsPath();
  json := TJSONConfig.Create(nil);
  json.Filename:= path;

  // omni pascal
  json.SetValue('omnipascal.defaultDevelopmentEnvironment', 'FreePascal');
  json.SetValue('omnipascal.delphiInstallationPath', DELPHI_INSTALLATION_PATH);
  json.SetValue('omnipascal.freePascalSourcePath', FREEPASCAL_SOURCE_PATH);
  json.SetValue('omnipascal.lazbuildPath', LAZ_BUILD_PATH);
  json.SetValue('omnipascal.createBuildScripts', false);
  sp := string(json.GetValue('omnipascal.searchPath', '')).Trim();
  if (sp <> '') and (not sp.EndsWith(';')) then begin
    sp += ';';
  end;
  if (not sp.Contains('components' + SPL + 'lazutils')) then begin
    sp += searchPath;
    json.SetValue('omnipascal.searchPath', sp);
  end;

  // formatter
  json.SetValue('pascal.format.indent', '2');
  json.SetValue('pascal.format.wrapLineLength', '500');
  json.SetValue('pascal.formatter.engine', 'ptop');
  json.SetValue('pascal.formatter.enginePath', PTOP_PATH);

  json.Free;
end;

end.

