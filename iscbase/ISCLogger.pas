unit ISCLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


type
  { TLogger }

  TLogger = class
  public
    class procedure info(AModule: string; AMsg: string);
    class procedure debug(AModule: string; AMsg: string);
    class procedure warn(AModule: string; AMsg: string);
    class procedure error(AModule: string; AMsg: string);
  end;

implementation

uses
  ISCConsts;

type
  TLogType = (ltInfo, ltDebug, ltWarn, ltError);

function logTypeToStr(ALv: TLogType): string;
begin
  case ALv of
  ltInfo: Result := 'INFO';
  ltDebug: Result := 'DEBUG';
  ltWarn: Result:= 'WARN';
  ltError: Result := 'ERROR';
  end;
end;

procedure innerWriteLog(AModule: string; AMsg: string; ALevel: TLogType);
var
  log: TextFile;
  filePath: string;
  logText: string;
  colorText: string;
begin
  if (LOGGER_LEVEL = 'error') and (not (ALevel in [ltError])) then Exit;
  if (LOGGER_LEVEL = 'warn') and (not (ALevel in [ltWarn, ltError])) then Exit;
  if (LOGGER_LEVEL = 'info') and (not (ALevel in [ltInfo, ltWarn, ltError])) then Exit;
  filePath:= LOG_DIR;
  case ALevel of
  ltInfo: filePath += 'app-info.log';
  ltDebug: filePath += 'app-debug.log';
  ltWarn: filePath += 'app-warn.log';
  ltError: filePath += 'app-error.log';
  end;
  filePath += '.' + FormatDateTime('yyyyMMdd', Now);
  AssignFile(log, filePath);
  if (FileExists(filePath)) then begin
    Append(log);
  end else begin
    Rewrite(log);
  end;
  logText:= '[%s][%s] %s %s'.Format([FormatDateTime('yyyy-MM-dd hh:mm:ss', Now), AModule, logTypeToStr(ALevel), AMsg]);
  // CRT
  case ALevel of
  ltError: colorText:= #27'[31m' + logText + #27'[0m';
  ltWarn: colorText:= #27'[33m' + logText + #27'[0m';
  ltInfo: colorText:= logText;
  ltDebug: colorText:= #27'[32m' + logText + #27'[0m';
  end;
  WriteLn(colorText);

  // log file
  WriteLn(log, logText);
  Flush(log);
  CloseFile(log);
end;

class procedure TLogger.info(AModule: string; AMsg: string);
begin
  innerWriteLog(AModule, AMsg, ltInfo);
end;

class procedure TLogger.debug(AModule: string; AMsg: string);
begin
  innerWriteLog(AModule, AMsg, ltDebug);
end;

class procedure TLogger.warn(AModule: string; AMsg: string);
begin
  innerWriteLog(AModule, AMsg, ltWarn);
end;

class procedure TLogger.error(AModule: string; AMsg: string);
begin
  innerWriteLog(AModule, AMsg, ltError);
end;

end.

