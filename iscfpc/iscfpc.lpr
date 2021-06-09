program iscfpc;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}cthreads,{$ENDIF} Classes, sysutils, fphttpclient, untHelp, untDoctor, untCreate,
  untConsts, untbuild, untClean, untUpgrade;

var
  param: string;
  outpath: string;
  projType: string;
  cur: string;
  isCreated: Boolean = False;

begin
  if (ParamCount = 0) then begin
    printHelp();
    Exit;
  end;

  param := ParamStr(1);
  case param of
  'build': doBuild();
  'clean': doClean();
  'create':
    begin
      projType := ParamStr(2);
      if (projType = '') or ((projType <> 'web') and (projType <> 'console') and (projType <> 'lib')) then begin
        WriteLn('you must specify the project type. valid option is "web", "console" and "lib".');
        WriteLn('');
        Exit;
      end;
      outpath:= ParamStr(3);
      if (outpath = '') or (outpath.Contains('..')) then begin
        WriteLn('you must specify a directory for creating a project.');
        WriteLn('');
        Exit;
      end;
      cur := GetCurrentDir();
      if (not cur.EndsWith(SPL)) then cur += SPL;
      outpath:= cur + outpath;
      if (not outpath.EndsWith(SPL)) then outpath += SPL;
      if (not DirectoryExists(outpath)) then begin
        ForceDirectories(outpath);
        isCreated := True;
      end;
      if (not DirectoryExists(outpath)) then begin
        WriteLn('you must specify a legel directory name for creating a project.');
        WriteLn('');
        Exit;
      end;
      doCreate(outpath, projType, isCreated);
    end;
  'doctor': doDoctor();
  'upgrade': doUpgrade()
  else printHelp();
  end;
end.

