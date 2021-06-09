unit untCreate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untConsts, FileUtil, fphttpclient, Zipper;

procedure doCreate(AOutPath: string; AType: string; ACreated: Boolean);

implementation

function getProjectName(APath: string): string;
begin
  if (APath.EndsWith('.' + SPL)) then begin
    APath:= APath.Substring(0, APath.Length - 2);
  end;
  if (APath.EndsWith(SPL)) then begin
    APath:= APath.Substring(0, APath.Length - 1);
  end;
  Exit(APath.Substring(APath.LastIndexOf(SPL) + 1));
end;

function isValidProjectName(AName: string): Boolean;
var
  v: Boolean = True;
  i: Integer;
begin
  for i:= 1 to AName.Length do begin
    if (i=1) then begin
      if not ((AName[i] in ['A'..'Z']) or (AName[i] in ['a'..'z'])) then begin
        v := False;
        Break;
      end;
    end else begin
      if not ((AName[i] in ['A'..'Z']) or (AName[i] in ['a'..'z']) or (AName[i] in ['0'..'9'])) then begin
        v := False;
        Break;
      end;
    end;
  end;
  Exit(v);
end;

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

  if (not FileExists(AZipPath)) then begin
    downloadBasePack(AZipPath);
  end;

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

procedure createDockerFile(AOutPath: string; AProjName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('FROM 10.30.30.22:9080/library/alpine:3.12');
  sl.Add('');
  sl.Add('RUN sed -i ''s/dl-cdn.alpinelinux.org/mirrors.aliyun.com/g'' /etc/apk/repositories \');
  sl.Add('    && apk add tzdata \');
  sl.Add('    && cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime \');
  sl.Add('    && echo "Asia/Shanghai" > /etc/timezone \');
  sl.Add('    && apk del tzdata \');
  sl.Add('    && apk add dumb-init libgcc libc6-compat dbus curl yaml \');
  sl.Add('    && apk add --update --no-cache ttf-dejavu fontconfig \');
  sl.Add('    && rm -rf /var/cache/apk/*');
  sl.Add('');
  sl.Add('ARG app=isc-%s-service'.Format([AProjName]));
  sl.Add('ARG path=/home/${app}');
  sl.Add('RUN mkdir -p ${path}');
  sl.Add('COPY files ${path}/files');
  sl.Add('COPY ld ${path}/ld');
  sl.Add('COPY %s ${path}'.Format([AProjName]));
  sl.Add('WORKDIR ${path}');
  sl.Add('');
  sl.Add('ENTRYPOINT ["/usr/bin/dumb-init", "--"]');
  sl.Add('CMD ["./%s"]'.Format([AProjName]));
  sl.SaveToFile(AOutPath + 'Dockerfile');
  sl.Free;
end;

function getLazarusUnitPaths(): string;
begin
  Result := Format('%s;%s;%s;%s;%s;%s', [LAZUTIL_PPU_LIN, LAZUTIL_PPU_MAC, LAZUTIL_PPU_WIN, FCL_PPU_LIN, FCL_PPU_MAC, FCL_PPU_WIN]);
end;

procedure createLpi(AOutPath: string; AProjName: string; AProjType: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('<?xml version="1.0" encoding="UTF-8"?>');
  sl.Add('<CONFIG>');
  sl.Add('  <ProjectOptions>');
  sl.Add('    <Version Value="11"/>');
  sl.Add('    <General>');
  sl.Add('      <Flags>');
  sl.Add('        <MainUnitHasCreateFormStatements Value="False"/>');
  sl.Add('        <MainUnitHasTitleStatement Value="False"/>');
  sl.Add('        <MainUnitHasScaledStatement Value="False"/>');
  sl.Add('      </Flags>');
  sl.Add('      <SessionStorage Value="InProjectDir"/>');
  sl.Add('      <MainUnit Value="0"/>');
  sl.Add('      <Title Value="%s"/>'.format([AProjName]));
  sl.Add('      <UseAppBundle Value="False"/>');
  sl.Add('      <ResourceType Value="res"/>');
  sl.Add('    </General>');
  sl.Add('    <BuildModes Count="1">');
  sl.Add('      <Item1 Name="Default" Default="True"/>');
  sl.Add('    </BuildModes>');
  sl.Add('    <PublishOptions>');
  sl.Add('      <Version Value="2"/>');
  sl.Add('      <UseFileFilters Value="True"/>');
  sl.Add('    </PublishOptions>');
  sl.Add('    <RunParams>');
  sl.Add('      <FormatVersion Value="2"/>');
  sl.Add('      <Modes Count="0"/>');
  sl.Add('    </RunParams>');
  sl.Add('    <Units Count="2">');
  sl.Add('      <Unit0>');
  sl.Add('        <Filename Value="%s.lpr"/>'.format([AProjName]));
  sl.Add('        <IsPartOfProject Value="True"/>');
  sl.Add('      </Unit0>');
  sl.Add('      <Unit1>');
  sl.Add('        <Filename Value="src/%s.pas"/>'.format([specialize IfThen<string>(AProjType = 'web', 'untRoute', 'untFunc')]));
  sl.Add('        <IsPartOfProject Value="True"/>');
  sl.Add('        <UnitName Value="untFunc"/>');
  sl.Add('      </Unit1>');
  sl.Add('    </Units>');
  sl.Add('  </ProjectOptions>');
  sl.Add('  <CompilerOptions>');
  sl.Add('    <Version Value="11"/>');
  sl.Add('    <Target>');
  sl.Add('      <Filename Value="%s"/>'.format([AProjName]));
  sl.Add('    </Target>');
  sl.Add('    <SearchPaths>');
  sl.Add('      <IncludeFiles Value="$(ProjOutDir);iscbase;iscbase/database;iscbase/database/component;iscbase/database/core;iscbase/database/dbc;iscbase/database/parsesql;iscbase/database/plain;iscbase/network;iscbase/script;"/>');
  sl.Add('      <OtherUnitFiles Value="%s;src;test;iscbase;iscbase/database;iscbase/database/component;iscbase/database/core;iscbase/database/dbc;iscbase/database/parsesql;iscbase/database/plain;iscbase/network;iscbase/script;iscbase/coroutine;iscbase/websocket"/>'.format([getLazarusUnitPaths()]));
  sl.Add('      <UnitOutputDirectory Value="lib/$(TargetCPU)-$(TargetOS)"/>');
  sl.Add('    </SearchPaths>');
  sl.Add('    <Other>');
  sl.Add('      <CompilerMessages>');
  sl.Add('        <IgnoredMessages idx6058="True" idx3124="True" idx3123="True"/>');
  sl.Add('      </CompilerMessages>');
  sl.Add('    </Other>');
  sl.Add('  </CompilerOptions>');
  sl.Add('  <Debugging>');
  sl.Add('    <Exceptions Count="3">');
  sl.Add('      <Item1>');
  sl.Add('        <Name Value="EAbort"/>');
  sl.Add('      </Item1>');
  sl.Add('      <Item2>');
  sl.Add('        <Name Value="ECodetoolError"/>');
  sl.Add('      </Item2>');
  sl.Add('      <Item3>');
  sl.Add('        <Name Value="EFOpenError"/>');
  sl.Add('      </Item3>');
  sl.Add('    </Exceptions>');
  sl.Add('  </Debugging>');
  sl.Add('</CONFIG>');

  sl.SaveToFile(AOutPath + AProjName + '.lpi');
  sl.Free;
end;

procedure createYml(AOutPath: string; AProjName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('api-moudle: %s'.Format([AProjName]));
  sl.Add('');
  sl.Add('server:');
  sl.Add('  port: 8080');
  sl.Add('');
  sl.Add('spring:');
  sl.Add('  profile:');
  sl.Add('    active: default');
  sl.Add('  application:');
  sl.Add('    name: %s'.Format([AProjName]));
  sl.Add('');
  sl.Add('fpweb:');
  sl.Add('  profile:');
  sl.Add('    active: default');
  sl.Add('  application:');
  sl.Add('    name: %s'.Format([AProjName]));
  sl.Add('');
  sl.Add('logging:');
  sl.Add('  level:');
  sl.Add('    root: info');
  sl.Add('');
  sl.SaveToFile(AOutPath + 'application.yml');
  sl.Free;
end;

procedure createVSCodeFiles(AOutPath: string);
var
  sl: TStringList;
  fuList, fiList: string;
begin
  ForceDirectories(AOutPath + '.vscode');
  sl := TStringList.Create;
  sl.Add('{');
  sl.Add('  "version": "0.2.0",');
  sl.Add('  "configurations": [');
  sl.Add('    {');
  {$IFDEF WINDOWS}
  sl.Add('      "name": "GDB Debug",');
  sl.Add('      "type": "gdb",');
  sl.Add('      "gdbpath": "C:\\lazarus\\mingw\\x86_64-win64\\bin\\gdb.exe",');
  {$ELSE}
  sl.Add('      "name": "LLDB Debug",');
  sl.Add('      "type": "lldb",');
  {$ENDIF}
  sl.Add('      "request": "launch",');
  sl.Add('      "cwd": "${workspaceFolder}",');
  sl.Add('      "%s": ".build%sdebug%s${fileBasenameNoExtension}%s",'.Format([{$IFDEF WINDOWS}'target'{$ELSE}'program'{$ENDIF}, VSSPL, VSSPL, {$IFDEF WINDOWS}'.exe'{$ELSE}''{$ENDIF}]));
  sl.Add('      "preLaunchTask": "fpc: Build Debug",');
  sl.Add('      "postDebugTask": "fpc: Clean"');
  sl.Add('    }');
  sl.Add('  ]');
  sl.Add('}');
  sl.SaveToFile(AOutPath + '.vscode' + SPL + 'launch.json');

  sl.Clear;
  fuList:= '        "-Fu%s",'#10.Format([LAZUTIL_PPU_LIN]);
  fuList+= '        "-Fu%s",'#10.Format([FCL_PPU_LIN]);
  fuList+= '        "-Fu%s",'#10.Format([LAZUTIL_PPU_MAC]);
  fuList+= '        "-Fu%s",'#10.Format([FCL_PPU_MAC]);
  fuList+= '        "-Fu%s",'#10.Format([LAZUTIL_PPU_WIN_VS]);
  fuList+= '        "-Fu%s",'#10.Format([FCL_PPU_WIN_VS]);
  fuList+= '        "-Fusrc",'#10;
  fuList+= '        "-Futest",'#10;
  fuList+= '        "-Fuiscbase",'#10;
  fuList+= '        "-Fuiscbase/database",'#10;
  fuList+= '        "-Fuiscbase/database/component",'#10;
  fuList+= '        "-Fuiscbase/database/core",'#10;
  fuList+= '        "-Fuiscbase/database/dbc",'#10;
  fuList+= '        "-Fuiscbase/database/parsesql",'#10;
  fuList+= '        "-Fuiscbase/database/plain",'#10;
  fuList+= '        "-Fuiscbase/coroutine",'#10;
  fuList+= '        "-Fuiscbase/network",'#10;
  fuList+= '        "-Fuiscbase/script",'#10;
  fuList+= '        "-Fuiscbase/websocket",'#10;
  fiList:= '        "-Fiiscbase",'#10;
  fiList+= '        "-Fiiscbase/database",'#10;
  fiList+= '        "-Fiiscbase/database/component",'#10;
  fiList+= '        "-Fiiscbase/database/core",'#10;
  fiList+= '        "-Fiiscbase/database/dbc",'#10;
  fiList+= '        "-Fiiscbase/database/parsesql",'#10;
  fiList+= '        "-Fiiscbase/database/plain",'#10;
  fiList+= '        "-Fiiscbase/network",'#10;
  fiList+= '        "-Fiiscbase/script",'#10;

  sl.Add('{');
  sl.Add('  "version": "2.0.0",');
  // options
  sl.Add('  "options": {');
  sl.Add('    "cwd": "${workspaceFolder}",');
  {$IFDEF WINDOWS}
  sl.Add('    "shell": {');
  sl.Add('      "executable": "C:\\Windows\\System32\\cmd.exe",');
  sl.Add('      "args": [ "/C" ]');
  sl.Add('    },');
  {$ENDIF}
  sl.Add('    "env": {');
  sl.Add('      "FPC_COMMAND": "%s",'.Format([FPC_CMD]));
  sl.Add('      "PROJECTFILE": "${relativeFile}",');
  sl.Add('      "PROJECTBASE": "${fileBasenameNoExtension}",');
  sl.Add('      "OUTPUT": ".build",');
  sl.Add('      "DEBUG": ".build%sdebug",'.Format([VSSPL]));
  sl.Add('      "RELEASE": ".build%srelease"'.Format([VSSPL]));
  sl.Add('    }');
  sl.Add('  },');
  sl.Add('  "tasks": [');
  // build debug
  sl.Add('    {');
  sl.Add('      "label": "fpc: Build Debug",');
  sl.Add('      "type": "shell",');
  sl.Add('      "group": "build",');
  sl.Add('      "command": "%s",'.Format([packVar('FPC_COMMAND')]));
  sl.Add('      "args": [');
  sl.Add('        "%s",'.Format([packVar('PROJECTFILE')]));
  sl.Add('        "-Mobjfpc",');
  sl.Add('        "-Scghi",');
  sl.Add('        "-vewnhibq",');
  sl.Add('        "-O-",');
  sl.Add('        "-Xg",');
  sl.Add('        "-B",');
  sl.Add('        "-v",');
  sl.Add('        "-glpsw2",');
  sl.Add('        "-godwarfcpp",');
  sl.Add(fuList);
  sl.Add(fiList);
  sl.Add('        "-FE%s",'.Format([packVar('DEBUG')]));
  sl.Add('        "-FU%s"'.Format([packVar('OUTPUT')]));
  sl.Add('      ],');
  sl.Add('      "presentation": {');
  sl.Add('        "focus": true');
  sl.Add('      },');
  sl.Add('      "problemMatcher": {');
  sl.Add('        "owner": "objectpascal",');
  sl.Add('        "fileLocation": [');
  sl.Add('          "relative",');
  sl.Add('          "${workspaceFolder}"');
  sl.Add('        ],');
  sl.Add('        "pattern": {');
  sl.Add('          "kind": "location",');
  sl.Add('          "regexp": "^(.*)\\((\\d.*),(\\d.*)\\) (Warning|Error|Fatal): (.*)$",');
  sl.Add('          "file": 1,');
  sl.Add('          "line": 2,');
  sl.Add('          "column": 3,');
  sl.Add('          "severity": 4,');
  sl.Add('          "message": 5,');
  sl.Add('          "loop": true');
  sl.Add('        }');
  sl.Add('      }');
  sl.Add('    },');

  // build release
  sl.Add('    {');
  sl.Add('      "label": "fpc: Build Release",');
  sl.Add('      "type": "shell",');
  sl.Add('      "group": "build",');
  sl.Add('      "command": "%s",'.Format([packVar('FPC_COMMAND')]));
  sl.Add('      "args": [');
  sl.Add('        "%s",'.Format([packVar('PROJECTFILE')]));
  sl.Add('        "-Mobjfpc",');
  sl.Add('        "-Scghi",');
  sl.Add('        "-vewnhibq",');
  sl.Add('        "-CX",');
  sl.Add('        "-O3",');
  sl.Add('        "-XXs",');
  sl.Add('        "-B",');
  sl.Add('        "-v",');
  sl.Add(fuList);
  sl.Add(fiList);
  sl.Add('        "-FE%s",'.Format([packVar('RELEASE')]));
  sl.Add('        "-FU%s"'.Format([packVar('OUTPUT')]));
  sl.Add('      ],');
  sl.Add('      "presentation": {');
  sl.Add('        "focus": true');
  sl.Add('      },');
  sl.Add('      "problemMatcher": {');
  sl.Add('        "owner": "objectpascal",');
  sl.Add('        "fileLocation": [');
  sl.Add('          "relative",');
  sl.Add('          "${workspaceFolder}"');
  sl.Add('        ],');
  sl.Add('        "pattern": {');
  sl.Add('          "kind": "location",');
  sl.Add('          "regexp": "^(.*)\\((\\d.*),(\\d.*)\\) (Warning|Error|Fatal): (.*)$",');
  sl.Add('          "file": 1,');
  sl.Add('          "line": 2,');
  sl.Add('          "column": 3,');
  sl.Add('          "severity": 4,');
  sl.Add('          "message": 5,');
  sl.Add('          "loop": true');
  sl.Add('        }');
  sl.Add('      }');
  sl.Add('    },');

    // syntax check
  sl.Add('    {');
  sl.Add('      "label": "fpc: Syntax Check",');
  sl.Add('      "type": "shell",');
  sl.Add('      "group": "build",');
  sl.Add('      "command": "%s",'.Format([packVar('FPC_COMMAND')]));
  sl.Add('      "args": [');
  sl.Add('        "%s",'.Format([packVar('PROJECTFILE')]));
  sl.Add('        "-Mobjfpc",');
  sl.Add('        "-Scghi",');
  sl.Add('        "-vewnhibq",');
  sl.Add('        "-B",');
  sl.Add('        "-v",');
  sl.Add('        "-s",');
  sl.Add(fuList);
  sl.Add(fiList);
  sl.Add('        "-FE%s",'.Format([packVar('OUTPUT')]));
  sl.Add('        "-FU%s"'.Format([packVar('OUTPUT')]));
  sl.Add('      ],');
  sl.Add('      "presentation": {');
  sl.Add('        "focus": true');
  sl.Add('      },');
  sl.Add('      "problemMatcher": {');
  sl.Add('        "owner": "objectpascal",');
  sl.Add('        "fileLocation": [');
  sl.Add('          "relative",');
  sl.Add('          "${workspaceFolder}"');
  sl.Add('        ],');
  sl.Add('        "pattern": {');
  sl.Add('          "kind": "location",');
  sl.Add('          "regexp": "^(.*)\\((\\d.*),(\\d.*)\\) (Warning|Error|Fatal): (.*)$",');
  sl.Add('          "file": 1,');
  sl.Add('          "line": 2,');
  sl.Add('          "column": 3,');
  sl.Add('          "severity": 4,');
  sl.Add('          "message": 5,');
  sl.Add('          "loop": true');
  sl.Add('        }');
  sl.Add('      }');
  sl.Add('    },');

    // instant run
  sl.Add('    {');
  sl.Add('      "label": "fpc: Instant Run",');
  sl.Add('      "type": "shell",');
  sl.Add('      "group": "test",');
  sl.Add('      "command": "%s",'.Format([INSTANTFPC_CMD]));
  sl.Add('      "args": [');
  sl.Add('        "%s",'.Format([packVar('PROJECTFILE')]));
  sl.Add('        "-B"');
  sl.Add('      ],');
  sl.Add('      "presentation": {');
  sl.Add('        "focus": true');
  sl.Add('      },');
  sl.Add('      "problemMatcher": {');
  sl.Add('        "owner": "objectpascal",');
  sl.Add('        "fileLocation": [');
  sl.Add('          "relative",');
  sl.Add('          "${workspaceFolder}"');
  sl.Add('        ],');
  sl.Add('        "pattern": {');
  sl.Add('          "kind": "location",');
  sl.Add('          "regexp": "^(.*)\\((\\d.*),(\\d.*)\\) (Warning|Error|Fatal): (.*)$",');
  sl.Add('          "file": 1,');
  sl.Add('          "line": 2,');
  sl.Add('          "column": 3,');
  sl.Add('          "severity": 4,');
  sl.Add('          "message": 5,');
  sl.Add('          "loop": true');
  sl.Add('        }');
  sl.Add('      }');
  sl.Add('    },');

  // execute binary
  sl.Add('    {');
  sl.Add('      "label": "fpc: Execute Binary",');
  sl.Add('      "type": "shell",');
  sl.Add('      "group": {');
  sl.Add('        "kind": "test",');
  sl.Add('        "isDefault": true');
  sl.Add('      },');
  sl.Add('      "command": ".%s%s%s%s%s",'.Format([VSSPL, packVar('DEBUG'), VSSPL, packVar('PROJECTBASE'), {$IFDEF WINDOWS}'.exe'{$ELSE}''{$ENDIF}]));
  sl.Add('      "args": [],');
  sl.Add('      "presentation": {');
  sl.Add('        "focus": true');
  sl.Add('      },');
  sl.Add('      "problemMatcher": []');
  sl.Add('    },');

  // clean
  sl.Add('    {');
  sl.Add('      "label": "fpc: Clean",');
  sl.Add('      "type": "shell",');
  sl.Add('      "command": "%s",'.Format([{$IFDEF WINDOWS}'del /S /Q /F %OUTPUT%'{$ELSE}'rm -fr ${OUTPUT} && mkdir -p ${DEBUG} && mkdir -p ${RELEASE}'{$ENDIF}]));
  sl.Add('      "presentation": {');
  sl.Add('        "reveal": "silent"');
  sl.Add('      },');
  sl.Add('      "problemMatcher": []');
  sl.Add('    }');
  sl.Add('  ]');
  sl.Add('}');

  sl.SaveToFile(AOutPath + '.vscode' + SPL + 'tasks.json');
  sl.Free;
end;

procedure createProjectFile(AOutPath: string; AProjName: string; AProjType: string);
var
  sl: TStringList;
begin
  sl:= TStringList.Create;
  if (AProjType = 'lib') then begin
    sl.Add('library %s;'.format([AProjName]));
  end else begin
    sl.Add('program %s;'.format([AProjName]));
  end;
  sl.Add('');
  sl.Add('{$mode objfpc}{$H+}');
  sl.Add('');
  sl.Add('uses');
  sl.Add('  {$IFNDEF WINDOWS}cthreads,{$ENDIF}');
  if (AProjType = 'lib') then begin
    sl.Add('  cmem, Classes, SysUtils, dynlibs, untFunc;');
  end else if (AProjType = 'console') then begin
    sl.Add('  cmem, Classes, SysUtils, untFunc;');
  end else if (AProjType = 'web') then begin
    sl.Add('  cmem, fpwebfile, fphttpapp, HTTPDefs, httproute, fphttp, ISCConsts, untRoute;');
  end;
  sl.Add('');

  if (AProjType = 'lib') then begin
    sl.Add('exports');
    sl.Add('  sayHello;');
    sl.Add('');
    sl.Add('begin');
    sl.Add('');
    sl.Add('end.');
  end else if (AProjType = 'console') then begin
    sl.Add('begin');
    sl.Add('  WriteLn(sayHello());');
    sl.Add('end.');
  end else if (AProjType = 'web') then begin
    sl.Add('begin');
    sl.Add('  RegisterFileLocation(''static'', FILES_DIR);');
    sl.Add('  HTTPRouter.RegisterRoute(''/'', rmAll, @index);');
    sl.Add('  Application.QueueSize:= 1000;');
    sl.Add('  Application.Port := SERVER_PORT;');
    sl.Add('  Application.Threaded := True;');
    sl.Add('  Application.Initialize;');
    sl.Add('  Application.Run;');
    sl.Add('end.');
  end;

  sl.SaveToFile(AOutPath + AProjName + '.lpr');
  sl.Free;
end;

procedure createRoutingFile(AOutPath: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('unit untRoute;');
  sl.Add('');
  sl.Add('{$mode objfpc}{$H+}');
  sl.Add('');
  sl.Add('interface');
  sl.Add('');
  sl.Add('uses');
  sl.Add('  Classes, SysUtils, HTTPDefs;');
  sl.Add('');
  sl.Add('procedure index(Areq: TRequest; AResp: TResponse);');
  sl.Add('');
  sl.Add('implementation');
  sl.Add('');
  sl.Add('procedure index(Areq: TRequest; AResp: TResponse);');
  sl.Add('begin');
  sl.Add('  AResp.Code := 200;');
  sl.Add('  AResp.ContentType := ''text/html; charset=utf-8'';');
  sl.Add('  AResp.Content := ''<html><body>Hello</body></html>'';');
  sl.Add('end;');
  sl.Add('');
  sl.Add('end.');

  sl.SaveToFile(AOutPath + 'src' + SPL + 'untRoute.pas');
  sl.Free;
end;

procedure createServiceFile(AOutPath: string; AProjName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('[Unit]');
  sl.Add('Description=iSyscore %s Service'.format([AProjName]));
  sl.Add('[Service]');
  sl.Add('ExecStart=' + AOutPath + AProjName);
  sl.Add('[Install]');
  sl.Add('WantedBy=multi-user.target');
  sl.SaveToFile(AOutPath + AProjName + '.service');
  sl.Free;
end;

procedure createFunctionFile(AOutPath: string; AProjType: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Add('unit untFunc;');
  sl.Add('');
  sl.Add('{$mode objfpc}{$H+}');
  sl.Add('');
  sl.Add('interface');
  sl.Add('');
  sl.Add('uses');
  sl.Add('  Classes, SysUtils;');
  sl.Add('');
  if (AProjType = 'console') then begin
    sl.Add('function sayHello(): string;');
  end else if (AProjType = 'lib') then begin
    sl.Add('function sayHello(): PChar; cdecl;');
  end;
  sl.Add('');
  sl.Add('implementation');
  sl.Add('');
  if (AProjType = 'console') then begin
    sl.Add('function sayHello(): string;');
    sl.Add('begin');
    sl.Add('  Exit(''Hello'');');
    sl.Add('end;');
  end else if (AProjType = 'lib') then begin
    sl.Add('function sayHello(): PChar; cdecl;');
    sl.Add('var');
    sl.Add('  str: string = ''hello'';');
    sl.Add('begin');
    sl.Add('  Result := StrAlloc(str.Length);');
    sl.Add('  StrCopy(Result, PChar(str));');
    sl.Add('end;');
  end;
  sl.Add('');
  sl.Add('end.');
  sl.SaveToFile(AOutPath + 'src' + SPL + 'untFunc.pas');
  sl.Free;
end;

procedure doCreate(AOutPath: string; AType: string; ACreated: Boolean);
var
  AProjName: string;
begin
  AProjName:= getProjectName(AOutPath);
  if (not isValidProjectName(AProjName)) then begin
    WriteLn('invalid project name, the project can''t be created.');
    WriteLn('');
    if (ACreated) then begin
      DeleteDirectory(AOutPath, False);
    end;
    Exit;
  end;
  if (AOutPath.EndsWith('.' + SPL)) then begin
    AOutPath:= AOutPath.Substring(0, AOutPath.Length - 2);
  end;

  if (not getBasePack(AOutPath)) then begin
    WriteLn('get ISCBASE package failed, the project can''t be created.');
    WriteLn('');
    if (ACreated) then begin
      DeleteDirectory(AOutPath, False);
    end;
    Exit;
  end;

  ForceDirectories(AOutPath + 'src');
  ForceDirectories(AOutPath + 'test');

  if (AType = 'web') then begin
    ForceDirectories(AOutPath + 'files');
    CopyFile(AOutPath + 'iscbase' + SPL + 'mime.txt', AOutPath + 'files' + SPL + 'mime.txt');
    createProjectFile(AOutPath, AProjName, AType);
    createRoutingFile(AOutPath);
    createLpi(AOutPath, AProjName, AType);
    createVSCodeFiles(AOutPath);
    createYml(AOutPath, AProjName);
    createDockerFile(AOutPath, AProjName);
    createServiceFile(AOutPath, AProjName);
  end else if (AType = 'console') then begin
    createProjectFile(AOutPath, AProjName, AType);
    createFunctionFile(AOutPath, AType);
    createLpi(AOutPath, AProjName, AType);
    createVSCodeFiles(AOutPath);
  end else if (AType = 'lib') then begin
    createProjectFile(AOutPath, AProjName, AType);
    createFunctionFile(AOutPath, AType);
    createLpi(AOutPath, AProjName, AType);
    createVSCodeFiles(AOutPath);
  end;

  WriteLn(#27'[32mCreate project %s completed.'#27'[0m'.Format([AProjName]));
  if (AType = 'web') then begin
    WriteLn('');
    WriteLn(#27'[33m[!]'#27'[0m To make web app a system service, copy %s.service to /etc/systemd/system'.Format([AProjName]));
    WriteLn(#27'[33m[!]'#27'[0m and run "sudo systemctl enable %s.service && sudo systemctl start %s.service"'.Format([AProjName, AProjName]));
  end;
  WriteLn('');
end;

end.

