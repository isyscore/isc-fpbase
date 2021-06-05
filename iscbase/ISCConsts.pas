unit ISCConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebfile, fphttpapp;

var
  MODULE_NAME: string;
  SERVICE_NAME: string;
  SERVER_PORT: Integer;
  ACTIVE_PROFILE: string;

  WORK_DIR: string;
  FILES_DIR: string;
  CONFIG_DIR: string;
  DATA_DIR: string;
  TMP_DIR: string;
  LOG_DIR: string;
  LD_DIR: string;
  BANNER_PATH: string;

  APPLICATION_YML_PATH: string;
  LOGGER_LEVEL: string = 'info';

implementation

uses ISCYaml;

procedure readYaml(y: TYamlFile);
begin
  MODULE_NAME:= y.GetValue('api-moudle', '');
  SERVER_PORT:= StrToIntDef(y.GetValue('server.port', '80'), 80);
  SERVICE_NAME:= y.GetValue('spring.application.name', '');
  if (SERVICE_NAME = '') then begin
    SERVICE_NAME:= y.GetValue('fpweb.application.name', '');
  end;
  LOGGER_LEVEL:= y.GetValue('logging.level.root', 'info');
end;

procedure preloadYaml(APath: string);
var
  y: TYamlFile;
begin
  y := TYamlFile.Create();
  y.LoadFromFile(APath);
  ACTIVE_PROFILE := y.GetValue('spring.profile.active', '');
  if (ACTIVE_PROFILE = '') then begin
    ACTIVE_PROFILE:= y.GetValue('fpweb.profile.active', '');
  end;
  readYaml(y);
  if(ACTIVE_PROFILE <> '') then begin
    // override sub profile
    APPLICATION_YML_PATH:= APPLICATION_YML_PATH.Replace('application.yml', 'application-' +  ACTIVE_PROFILE + '.yml');
    y.LoadFromFile(APPLICATION_YML_PATH);
    readYaml(y);
  end;
  y.Free;
end;

initialization
  WORK_DIR := ExtractFilePath(Application.ExeName);
  if (WORK_DIR.EndsWith('./')) then begin
    WORK_DIR := WORK_DIR.Substring(0, WORK_DIR.Length - 2);
  end;
  FILES_DIR := WORK_DIR + 'files/';
  if (not DirectoryExists(FILES_DIR)) then begin
    ForceDirectories(FILES_DIR);
  end;
  MimeTypesFile:= FILES_DIR + 'mime.txt';
  RegisterFileLocation('static', FILES_DIR);

  // create tmp folder
  TMP_DIR:= WORK_DIR + 'tmp/';
  if (not DirectoryExists(TMP_DIR)) then begin
    ForceDirectories(TMP_DIR);
  end;
  LOG_DIR := WORK_DIR + 'logs/';
  if (not DirectoryExists(LOG_DIR)) then begin
    ForceDirectories(LOG_DIR);
  end;

  LD_DIR:= WORK_DIR + 'ld/';
  if (not DirectoryExists(LD_DIR)) then begin
    ForceDirectories(LD_DIR);
  end;

  CONFIG_DIR:= WORK_DIR + 'config/';
  if (not DirectoryExists(CONFIG_DIR)) then begin
    ForceDirectories(CONFIG_DIR);
  end;

  DATA_DIR:= WORK_DIR + 'data/';
  if (not DirectoryExists(DATA_DIR)) then begin
    ForceDirectories(DATA_DIR);
  end;

  APPLICATION_YML_PATH:= WORK_DIR + 'application.yml';
  if (not FileExists(APPLICATION_YML_PATH)) then begin
    APPLICATION_YML_PATH:= CONFIG_DIR + 'application.yml';
  end;

  if (FileExists(APPLICATION_YML_PATH)) then begin
    preloadYaml(APPLICATION_YML_PATH);
  end;


end.

