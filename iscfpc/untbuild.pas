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

const
  CN_VALUE = '<CustomOptions Value="-Cn"/>';

procedure addLpiCn(lpi: string);
var
  i: Integer;
  changed: Boolean = False;
begin
  // add lpi -Cn
  with TStringList.Create do begin
    LoadFromFile(lpi);
    if (not Text.Contains(CN_VALUE)) then begin
      for i := 0 to Count - 1 do begin
        if (Strings[i].Contains('<Other>')) then begin
          Insert(i + 1, '      ' + CN_VALUE);
          changed:= True;
          Break;
        end;
      end;
    end;
    if (changed) then begin
      SaveToFile(lpi);
    end;
    Free;
  end;
end;

procedure removeLpiCn(lpi: string);
var
  i: Integer;
  changed: Boolean = False;
begin
  // remove lpi -Cn
  with TStringList.Create do begin
    LoadFromFile(lpi);
    if (Text.Contains(CN_VALUE)) then begin
      for i := 0 to Count - 1 do begin
        if (Strings[i].Contains(CN_VALUE)) then begin
          Delete(i);
          changed:= True;
          Break;
        end;
      end;
    end;
    if (changed) then begin
      SaveToFile(lpi);
    end;
    Free;
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
    removeLpiCn(lpi);
    if (isAlpine) then begin
      // add -Cn
      addLpiCn(lpi);
    end;
    retStat := RunCommandInDir(cd, LAZ_BUILD_PATH, ['-B', lpi], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
    if (retStat) then begin
      WriteLn(#27'[32mBuild project %s completed.'#27'[0m'.Format([pname]));
      // only ALPINE needs to manual link
      if (isAlpine) then begin
        // change link.res and ppas.sh
        changeAlpineLinkRes(lpi);
        changeAlpinePpasSh(lpi);
        // run ppas.sh
        {$IFDEF WINDOWS}
        retStat:= RunCommandInDir(cd, 'cmd', ['/C', 'ppas.bat'], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
        {$ELSE}
        retStat := RunCommandInDir(cd, 'sh', ['ppas.sh'], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
        {$ENDIF}
        if (retStat) then begin
          WriteLn(#27'[32mLink project %s completed.'#27'[0m'.Format([pname]));
        end else begin
          WriteLn(outstr);
          WriteLn(#27'[31mLink project %s failed.'#27'[0m'.Format([pname]));
        end;
        removeLpiCn(lpi);
        // clean
        removeAllLinkRes(cd);
        // DeleteFile(ExtractFilePath(lpi) + 'link.res');
        DeleteFile(ExtractFilePath(lpi) + {$IFDEF WINDOWS}'ppas.bat'{$ELSE}'ppas.sh'{$ENDIF});
        end;
    end else begin
       WriteLn(outstr);
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
      removeLpiCn(lpi);
      if (isAlpine) then begin
        addLpiCn(lpi);
      end;
      retStat := RunCommandInDir(cd, LAZ_BUILD_PATH, ['-B', lpi], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
      if (retStat) then begin
        WriteLn(#27'[32mBuild project %s completed.'#27'[0m'.Format([pname]));
        // link
        if (isAlpine) then begin
          // change link.res and ppas.sh
          changeAlpineLinkRes(lpi);
          changeAlpinePpasSh(lpi);
          // run ppas.sh
          {$IFDEF WINDOWS}
          retStat:= RunCommandInDir(cd, 'cmd', ['/C', 'ppas.bat'], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
          {$ELSE}
          retStat := RunCommandInDir(cd, 'sh', ['ppas.sh'], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
          {$ENDIF}
          if (retStat) then begin
            WriteLn(#27'[32mLink project %s completed.'#27'[0m'.Format([pname]));
          end else begin
            WriteLn(outstr);
            WriteLn(#27'[31mLink project %s failed.'#27'[0m'.Format([pname]));
          end;
          removeLpiCn(lpi);
          // clean
          removeAllLinkRes(cd);
          // DeleteFile(ExtractFilePath(lpi) + 'link.res');
          DeleteFile(ExtractFilePath(lpi) + {$IFDEF WINDOWS}'ppas.bat'{$ELSE}'ppas.sh'{$ENDIF});
        end;
      end else begin
        WriteLn(outstr);
        WriteLn(#27'[31mBuild project %s failed.'#27'[0m'.Format([pname]));
      end;
    end;
  end else begin
    WriteLn(#27'[31mPath: "%s" is not contain any fpc projects.'#27'[0m'.Format([cd]));
  end;
  list.Free;
end;

end.

