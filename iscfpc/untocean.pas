unit untOcean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, untConsts, Zipper;

procedure doOcean(AProjName: string);

implementation

procedure getOceanProjects();
var
  list: string;
  sl: TStringList;
  i: Integer;
begin
  with TFPHTTPClient.Create(nil) do begin
    try
      list := Get(BASE_URL + '/ocean/list');
      WriteLn(#27'[33mThe projects in Code Ocean is list here:'#27'[0m');
      WriteLn('');
      sl := TStringList.Create;
      sl.Text:= list;
      for i:= 0 to sl.Count - 1 do begin
        if (sl[i].Trim <> '') then begin
          WriteLn('  [%s] %s'.format([(i + 1).ToString.PadLeft(3, '0'), sl[i]]));
        end;
      end;
      sl.Free;
    except
      on E: Exception do begin
        WriteLn(#27'[31mCan''t get project list in Code Ocean.'#27'[0m');
      end;
    end;
    Free;
  end;
end;

procedure innerDownload(ASavePath: string; AProjName: string);
begin
  WriteLn('Downloading %s package...'.format([AProjName]));
  with TFPHTTPClient.Create(nil) do begin
    try
      Get(BASE_URL + '/ocean/' + AProjName + '.zip', ASavePath);
      WriteLn(#27'[32mDownload %s package success.'#27'[0m'.format([AProjName]));
    except
      on E: Exception do begin
        WriteLn(#27'[31mDownload ' + AProjName + ' package error: ' + E.Message + #27'[0m');
      end;
    end;
    Free;
  end;
end;

procedure downloadOceanProject(AProjName: string);
var
  cd: string;
  AZipPath: string;
begin
  cd := GetCurrentDir();
  if (not cd.EndsWith(SPL)) then cd += SPL;
  if (DirectoryExists(cd + AProjName)) then begin
    WriteLn(#27'[31mProject already exists, choose a different directory please.'#27'[0m');
    Exit;
  end;

  AZipPath:= GetEnvironmentVariable(HOME_VAR);
  if (not AZipPath.EndsWith(SPL)) then AZipPath += SPL;
  AZipPath += '.config' + SPL + 'iscfpc' + SPL;
  if (not DirectoryExists(AZipPath)) then begin
    ForceDirectories(AZipPath);
  end;
  AZipPath += AProjName + '.zip';

  if (not FileExists(AZipPath)) then begin
    innerDownload(AZipPath, AProjName);
  end;

  if (not FileExists(AZipPath)) then begin
    Exit;
  end;

  // unzip
  with TUnZipper.Create do begin
    Flat:= False;
    FileName:= AZipPath;
    OutputPath:= cd;
    try
      UnZipAllFiles;
    except
      on E: Exception do begin
        WriteLn(#27'[31mExtract ' + AProjName + ' package error: ' + E.Message + #27'[0m');
      end;
    end;
    Free;
  end;
end;

procedure doOcean(AProjName: string);
begin
  if (AProjName = '') then begin
    getOceanProjects();
  end else begin
    downloadOceanProject(AProjName);
  end;
end;

end.

