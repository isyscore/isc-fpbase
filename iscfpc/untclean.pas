unit untClean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untConsts;

procedure doClean();

implementation

var
  cleanCount: Integer = 0;

procedure doClean(ABasePath: string);
var
  src: TSearchRec;
  ext: string;
begin
  if (not ABasePath.EndsWith(SPL)) then ABasePath += SPL;
  if (FindFirst(ABasePath + '*', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      if (DirectoryExists(ABasePath + src.Name)) then begin
        doClean(ABasePath + src.Name);
      end else if (FileExists(ABasePath + src.Name)) then begin
        ext := string(ExtractFileExt(src.Name)).ToLower;
        if (ext = '.o') or (ext = '.ppu') or (ext = '.dcu') or (ext = '.compiled') or (ext = '.or') or (ext = '.rsj') or (ext = '.lps') or (ext = '.s') then begin
          DeleteFile(ABasePath + src.Name);
          Inc(cleanCount);
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

procedure doClean();
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
    WriteLn(#27'[33mCleaning project...'#27'[0m');
    cleanCount := 0;
    doClean(cd);
    WriteLn(#27'[32mClean Project completed, clean %d files.'#27'[0m'.Format([cleanCount]));
  end else begin
    WriteLn(#27'[31mPath: "%s" is not contain any fpc projects.'#27'[0m'.Format([cd]));
  end;
  list.Free;
end;

end.

