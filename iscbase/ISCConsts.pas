unit ISCConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebfile, fphttpapp;

var
  WORK_DIR: string;
  FILES_DIR: string;
  TMP_DIR: string;
  LOG_DIR: string;
  LD_DIR: string;
  BANNER_PATH: string;

implementation

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

end.

