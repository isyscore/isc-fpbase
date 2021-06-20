program iscfpc;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}cthreads,{$ENDIF} Classes, sysutils, fphttpclient, untHelp, untDoctor, untCreate,
  untConsts, untbuild, untClean, untUpgrade, untOcean;

var
  param: string;
  outpath: string;
  projType: string;
  cur: string;
  isCreated: Boolean = False;
  oProj: string;
  isFix: string;
  buildProj: string;
  isAlpine: Boolean;
  isJava: Boolean;
begin
  if (ParamCount = 0) then begin
    printHelp();
    Exit;
  end;

  param := ParamStr(1);
  case param of
  'build':
    begin
      buildProj := ParamStr(2);
      {$IFDEF LINUX}
      if (buildProj = 'ALPINE') then begin
        buildProj:= '';
        isAlpine:= True;
      end else begin
        isAlpine:= ParamStr(3) = 'ALPINE';
      end;
      {$ELSE}
      isAlpine := False;
      {$ENDIF}
      doBuild(buildProj, isAlpine);
    end;
  'clean': doClean();
  'create':
    begin
      projType := ParamStr(2);
      if (projType = '') or ((projType <> 'web') and (projType <> 'console') and (projType <> 'lib') and (projType <> 'jni')) then begin
        WriteLn('you must specify the project type. valid option is "web", "console", "lib" and "jni".');
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
      isJava:= ParamStr(4) = 'JAVA';
      doCreate(outpath, projType, isCreated, isJava);
    end;
  'doctor':
    begin
      isFix:= ParamStr(2);
      if (isFix = 'fix') then begin
        doDoctorFix();
      end else begin
        doDoctor();
      end;
    end;
  'upgrade': doUpgrade();
  'ocean':
    begin
      oProj:= ParamStr(2);
      doOcean(oProj);
    end;
  '--version': WriteLn(VERSION);
  else printHelp();
  end;
end.

