unit ISCConsts;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpwebfile, fphttpapp, ISCYaml;

const
  SPL = {$IFDEF WINDOWS}'\'{$ELSE}'/'{$ENDIF};

type
  TCustomConfigMethod = procedure (AYaml: TYamlFile);

var
  MODULE_NAME: string = '';
  SERVICE_NAME: string = '';
  SERVER_PORT: Integer = 80;
  QUEUE_SIZE: Integer = 1000;
  ACTIVE_PROFILE: string = '';

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

procedure loadYamlConfig(cfg: TCustomConfigMethod = nil);

implementation

uses
  ISCLogger;

procedure readYaml(y: TYamlFile; cfg: TCustomConfigMethod);
begin
  MODULE_NAME:= y.GetValue('api-moudle', MODULE_NAME);
  SERVER_PORT:= StrToIntDef(y.GetValue('server.port', SERVER_PORT.ToString), SERVER_PORT);
  QUEUE_SIZE := StrToIntDef(y.GetValue('server.queue-size', QUEUE_SIZE.ToString), QUEUE_SIZE);
  SERVICE_NAME:= y.GetValue('spring.application.name', SERVICE_NAME);
  if (SERVICE_NAME = '') then begin
    SERVICE_NAME:= y.GetValue('fpweb.application.name', SERVICE_NAME);
  end;
  LOGGER_LEVEL:= y.GetValue('logging.level.root', LOGGER_LEVEL);
  if (cfg <> nil) then begin
    cfg(y);
  end;
end;

procedure preloadYaml(APath: string; cfg: TCustomConfigMethod);
var
  y: TYamlFile;
begin
  y := TYamlFile.Create();
  y.LoadFromFile(APath);
  ACTIVE_PROFILE := y.GetValue('spring.profiles.active', ACTIVE_PROFILE);
  if (ACTIVE_PROFILE = '') then begin
    ACTIVE_PROFILE:= y.GetValue('fpweb.profiles.active', ACTIVE_PROFILE);
  end;
  readYaml(y, cfg);
  if(ACTIVE_PROFILE <> '') then begin
    // override sub profile
    if (FileExists(WORK_DIR + 'application-' + ACTIVE_PROFILE + '.yml')) then begin
      APPLICATION_YML_PATH := WORK_DIR + 'application-' + ACTIVE_PROFILE + '.yml';
      y.LoadFromFile(APPLICATION_YML_PATH);
      readYaml(y, cfg);
    end;
    if (FileExists(CONFIG_DIR + 'application-' + ACTIVE_PROFILE + '.yml')) then begin
      APPLICATION_YML_PATH := CONFIG_DIR + 'application-' + ACTIVE_PROFILE + '.yml';
      y.LoadFromFile(APPLICATION_YML_PATH);
      readYaml(y, cfg);
    end;
  end;
  y.Free;
end;

procedure loadYamlConfig(cfg: TCustomConfigMethod);
begin
  if (FileExists(WORK_DIR + 'application.yml')) then begin
    APPLICATION_YML_PATH:= WORK_DIR + 'application.yml';
    preloadYaml(APPLICATION_YML_PATH, cfg);
  end;
  if (FileExists(CONFIG_DIR + 'application.yml')) then begin
    APPLICATION_YML_PATH:= CONFIG_DIR + 'application.yml';
    preloadYaml(APPLICATION_YML_PATH, cfg);
  end;

  TLogger.info('main', 'MODULE_NAME = ' + MODULE_NAME);
  TLogger.info('main', 'SERVICE_NAME = ' + SERVICE_NAME);
  TLogger.info('main', 'SERVER_PORT = ' + SERVER_PORT.ToString);
  TLogger.info('main', 'QUEUE_SIZE = ' + QUEUE_SIZE.ToString);
  TLogger.info('main', 'ACTIVE_PROFILE = ' + ACTIVE_PROFILE);
end;

initialization
  WORK_DIR := ExtractFilePath(Application.ExeName);
  if (WORK_DIR.EndsWith('.' + SPL)) then begin
    WORK_DIR := WORK_DIR.Substring(0, WORK_DIR.Length - 2);
  end;
  FILES_DIR := WORK_DIR + 'files' + SPL;
  if (not DirectoryExists(FILES_DIR)) then begin
    ForceDirectories(FILES_DIR);
  end;
  MimeTypesFile:= FILES_DIR + 'mime.txt';
  RegisterFileLocation('static', {$IFDEF WINDOWS}'files'{$ELSE}FILES_DIR{$ENDIF});

  // create tmp folder
  TMP_DIR:= WORK_DIR + 'tmp' + SPL;
  if (not DirectoryExists(TMP_DIR)) then begin
    ForceDirectories(TMP_DIR);
  end;
  LOG_DIR := WORK_DIR + 'logs' + SPL;
  if (not DirectoryExists(LOG_DIR)) then begin
    ForceDirectories(LOG_DIR);
  end;

  LD_DIR:= WORK_DIR + 'ld' + SPL;
  if (not DirectoryExists(LD_DIR)) then begin
    ForceDirectories(LD_DIR);
  end;

  CONFIG_DIR:= WORK_DIR + 'config' + SPL;
  if (not DirectoryExists(CONFIG_DIR)) then begin
    ForceDirectories(CONFIG_DIR);
  end;

  DATA_DIR:= WORK_DIR + 'data' + SPL;
  if (not DirectoryExists(DATA_DIR)) then begin
    ForceDirectories(DATA_DIR);
  end;

end.

