unit untUpgrade;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, untConsts, Zipper;

procedure doUpgrade();

implementation

procedure downloadBasePack(ASavePath: string);
begin
  WriteLn('Downloading ISCBASE package...');
  with TFPHTTPClient.Create(nil) do begin
    try
      Get(BASE_URL + '/base.zip', ASavePath);
      WriteLn(#27'[32mDownload ISCBASE package success.'#27'[0m');
    except
      on E: Exception do begin
        WriteLn(#27'[31mDownload ISCBASE package error: ' + E.Message + #27'[0m');
      end;
    end;
    Free;
  end;
end;

function getBasePack(AOutPath: string): Boolean;
var
  AZipPath: string;
  hasError: Boolean = False;
begin
  AZipPath:= GetEnvironmentVariable(HOME_VAR);
  if (not AZipPath.EndsWith(SPL)) then AZipPath += SPL;
  AZipPath += '.config' + SPL + 'iscfpc' + SPL;
  if (not DirectoryExists(AZipPath)) then begin
    ForceDirectories(AZipPath);
  end;
  AZipPath += 'base.zip';

  if (FileExists(AZipPath)) then begin
    DeleteFile(AZipPath);
  end;
  downloadBasePack(AZipPath);

  if (not FileExists(AZipPath)) then begin
    Exit(False);
  end;

  // unzip
  with TUnZipper.Create do begin
    Flat:= False;
    FileName:= AZipPath;
    OutputPath:= AOutPath;
    try
      UnZipAllFiles;
    except
      on E: Exception do begin
        WriteLn(#27'[31mExtract ISCBASE package error: ' + E.Message + #27'[0m');
        hasError:= True;
      end;
    end;
    Free;
  end;

  Exit(not hasError);
end;

procedure doUpgrade();
var
  src: TSearchRec;
  list: TStringList;
  cd: string;
begin
  list := TStringList.Create;
  cd := GetCurrentDir();
  if (not cd.EndsWith(SPL)) then cd += SPL;
  if (FindFirst(cd + '*.lpi', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      list.Add(cd + src.Name);
    until FindNext(src) <> 0;
    FindClose(src);
  end;

  if (list.Count <> 0) then begin
    WriteLn(#27'[33mUpgrading ISCBASE package...'#27'[0m');

    if (not getBasePack(cd)) then begin
      WriteLn(#27'[31mget ISCBASE package failed, upgrade aborted.'#27'[0m');
      WriteLn('');
    end else begin
      WriteLn(#27'[32mUpgrade ISCBase package completed.'#27'[0m');
    end;
  end else begin
    WriteLn(#27'[31mPath: "%s" is not contain any fpc projects.'#27'[0m'.Format([cd]));
  end;
  list.Free;
end;

end.

