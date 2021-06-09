unit untbuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, untConsts;

procedure doBuild();

implementation

procedure doBuild();
var
  src: TSearchRec;
  list: TStringList;
  cd: string;
  lpi: string;
  outstr: string;
  retStat: Boolean;
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
    for lpi in list do begin
      WriteLn(#27'[33mBuilding project: %s'#27'[0m'.Format([ExtractFileName(lpi)]));
      retStat := RunCommandInDir(cd, LAZ_BUILD_PATH, [lpi], outstr, [poWaitOnExit, poUsePipes]);
      WriteLn(outstr);
      if (retStat) then begin
        WriteLn(#27'[32mBuild project %s completed.'#27'[0m'.Format([ExtractFileName(lpi)]));
      end else begin
        WriteLn(#27'[31mBuild project %s failed.'#27'[0m'.Format([ExtractFileName(lpi)]));
      end;
    end;
  end else begin
    WriteLn(#27'[31mPath: "%s" is not contain any fpc projects.'#27'[0m'.Format([cd]));
  end;
  list.Free;
end;

end.

