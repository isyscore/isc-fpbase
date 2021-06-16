unit untbuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, untConsts;

procedure doBuild(AProj: string; isAlpine: Boolean);

implementation

function findLinkRes(path: string): string;
var
  src: TSearchRec;
  ret: string = '';
begin
  if (FindFirst(path + 'link*.res', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      ret := path + src.Name;
      Break;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
  Exit(ret);
end;

procedure changeAlpineLinkRes(lpiPath: string);
var
  sl: TStringList;
  isLib: Boolean;
  linkResPath: string;
  i: Integer;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(lpiPath);
  isLib:= sl.Text.Contains('<ExecutableType Value="Library"/>');
  sl.Free;

  if (isLib) then begin
    // remove ld-linux-x86-64.so.2
    linkResPath:= findLinkRes(ExtractFilePath(lpiPath)); // ExtractFilePath(lpiPath) + 'link.res';
    sl := TStringList.Create;
    sl.LoadFromFile(linkResPath);
    for i := 0 to sl.Count - 1 do begin
      if (sl[i].Trim = '/lib64/ld-linux-x86-64.so.2') then begin
        sl.Delete(i + 1);
        sl.Delete(i);
        sl.Delete(i - 1);
        Break;
      end;
    end;
    sl.SaveToFile(linkResPath);
    sl.Free;
  end;
end;

procedure changeAlpinePpasSh(lpiPath: string);
var
  sl: TStringList;
  ppasPath: string;
begin
  ppasPath:= ExtractFilePath(lpiPath) + 'ppas.sh';
  sl := TStringList.Create;
  sl.LoadFromFile(ppasPath);
  // remove fpc init/fini
  sl.Text:= sl.Text.Replace('-init FPC_SHARED_LIB_START', '', [rfIgnoreCase, rfReplaceAll]).Replace('-fini FPC_LIB_EXIT', '', [rfReplaceAll, rfIgnoreCase]);
  sl.SaveToFile(ppasPath);
  sl.Free;
end;

procedure removeAllLinkRes(path: string);
var
  src: TSearchRec;
begin
  if (FindFirst(path + 'link*.res', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      DeleteFile(path + src.Name);
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure doBuild(AProj: string; isAlpine: Boolean);
var
  src: TSearchRec;
  list: TStringList;
  cd: string;
  lpi: string;
  outstr: string;
  retStat: Boolean;
  pname: string;
begin
  cd := GetCurrentDir();
  if (not cd.EndsWith(SPL)) then cd += SPL;

  removeAllLinkRes(cd);

  if (AProj <> '') then begin
    lpi := cd + AProj;
    if (not lpi.EndsWith('.lpi')) then lpi += '.lpi';
    if (not FileExists(lpi)) then begin
      WriteLn(#27'[32mProject %s not found.'#27'[0m'.Format([AProj]));
      Exit;
    end;
    pname:= ExtractFileName(lpi);
    WriteLn(#27'[33mBuilding project: %s'#27'[0m'.Format([pname]));
    // compile
    retStat := RunCommandInDir(cd, LAZ_BUILD_PATH, [lpi], outstr, [poWaitOnExit, poUsePipes]);
    WriteLn(outstr);
    if (retStat) then begin
      WriteLn(#27'[32mBuild project %s completed.'#27'[0m'.Format([pname]));
        // link
        if (isAlpine) then begin
          // change link.res and ppas.sh
          changeAlpineLinkRes(lpi);
          changeAlpinePpasSh(lpi);
        end;
        // run ppas.sh
        retStat := RunCommandInDir(cd, 'sh', ['ppas.sh'], outstr, [poWaitOnExit, poUsePipes]);
        if (retStat) then begin
          WriteLn(#27'[32mLink project %s completed.'#27'[0m'.Format([pname]));
        end else begin
          WriteLn(#27'[31mLink project %s failed.'#27'[0m'.Format([pname]));
        end;
        // clean
        removeAllLinkRes(cd);
        // DeleteFile(ExtractFilePath(lpi) + 'link.res');
        DeleteFile(ExtractFilePath(lpi) + 'ppas.sh');
    end else begin
       WriteLn(#27'[31mBuild project %s failed.'#27'[0m'.Format([pname]));
    end;
    Exit;
  end;

  list := TStringList.Create;
  if (FindFirst(cd + '*.lpi', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      list.Add(cd + src.Name);
    until FindNext(src) <> 0;
    FindClose(src);
  end;

  if (list.Count <> 0) then begin
    for lpi in list do begin
      pname:= ExtractFileName(lpi);
      WriteLn(#27'[33mBuilding project: %s'#27'[0m'.Format([pname]));
      // compile
      retStat := RunCommandInDir(cd, LAZ_BUILD_PATH, [lpi], outstr, [poWaitOnExit, poUsePipes]);
      WriteLn(outstr);
      if (retStat) then begin
        WriteLn(#27'[32mBuild project %s completed.'#27'[0m'.Format([pname]));
        // link
        if (isAlpine) then begin
          // change link.res and ppas.sh
          changeAlpineLinkRes(lpi);
          changeAlpinePpasSh(lpi);
        end;
        // run ppas.sh
        retStat := RunCommandInDir(cd, 'sh', ['ppas.sh'], outstr, [poWaitOnExit, poUsePipes]);
        if (retStat) then begin
          WriteLn(#27'[32mLink project %s completed.'#27'[0m'.Format([pname]));
        end else begin
          WriteLn(#27'[31mLink project %s failed.'#27'[0m'.Format([pname]));
        end;
        // clean
        removeAllLinkRes(cd);
        // DeleteFile(ExtractFilePath(lpi) + 'link.res');
        DeleteFile(ExtractFilePath(lpi) + 'ppas.sh');
      end else begin
        WriteLn(#27'[31mBuild project %s failed.'#27'[0m'.Format([pname]));
      end;
    end;
  end else begin
    WriteLn(#27'[31mPath: "%s" is not contain any fpc projects.'#27'[0m'.Format([cd]));
  end;
  list.Free;
end;

end.

