unit untConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  BASE_URL = 'http://yugioh.vip:9000/static';
  VERSION = '1.0.10';

  SPL = {$IFDEF WINDOWS}'\'{$ELSE}'/'{$ENDIF};
  VSSPL = {$IFDEF WINDOWS}'\\'{$ELSE}'/'{$ENDIF};
  HOME_VAR = {$IFDEF WINDOWS}'USERPROFILE'{$ELSE}'HOME'{$ENDIF};
  SETTINGS_PATH = {$IFDEF WINDOWS}'AppData\Roaming\'{$ELSE}{$IFDEF DARWIN}'Library/Application Support/'{$ELSE}'.config/'{$ENDIF}{$ENDIF};

  FPC_CMD = {$IFDEF WINDOWS}'C:\\lazarus\\fpc\\3.2.0\\bin\\x86_64-win64\\fpc.exe'{$ELSE}{$IFDEF DARWIN}'/usr/local/bin/fpc'{$ELSE}'/usr/bin/fpc'{$ENDIF}{$ENDIF};
  INSTANTFPC_CMD = {$IFDEF WINDOWS}'C:\\lazarus\\fpc\\3.2.0\\bin\\x86_64-win64\\instantfpc.exe'{$ELSE}'instantfpc'{$ENDIF};
  // plugin config
  FREEPASCAL_SOURCE_PATH = {$IFDEF WINDOWS}'C:\lazarus\fpc\3.2.0\source\'{$ELSE}{$IFDEF DARWIN}'/usr/local/lib/fpc/src/'{$ELSE}'/usr/lib/fpc/src/'{$ENDIF}{$ENDIF};
  DELPHI_INSTALLATION_PATH = {$IFDEF WINDOWS}'C:\lazarus\fpc\3.2.0\bin\x86_64-win64\fpc.exe'{$ELSE}{$IFDEF DARWIN}'/usr/local/bin/fpc'{$ELSE}'/usr/bin/fpc'{$ENDIF}{$ENDIF};

  LAZ_UTILS_PATH = {$IFDEF WINDOWS}'C:\lazarus\components\lazutils'{$ELSE}{$IFDEF DARWIN}'/Applications/Lazarus/components/lazutils'{$ELSE}'/usr/lib/lazarus/components/lazutils'{$ENDIF}{$ENDIF};
  PTOP_PATH = {$IFDEF WINDOWS}'C:\lazarus\fpc\3.2.0\bin\x86_64-win64\ptop.exe'{$ELSE}{$IFDEF DARWIN}'/usr/local/bin/ptop'{$ELSE}'/usr/bin/ptop'{$ENDIF}{$ENDIF};

  LAZARUS_PATH = {$IFDEF WINDOWS}'C:\lazarus'{$ELSE}{$IFDEF DARWIN}'/Applications/Lazarus'{$ELSE}'/usr/lib/lazarus'{$ENDIF}{$ENDIF};
  LAZ_BUILD_PATH = {$IFDEF WINDOWS}'C:\lazarus\lazbuild.exe'{$ELSE}{$IFDEF DARWIN}'/Applications/Lazarus/lazbuild'{$ELSE}'/usr/bin/lazbuild'{$ENDIF}{$ENDIF};

  LAZUTIL_PPU_MAC = '/Applications/Lazarus/components/lazutils/lib/x86_64-darwin';
  LAZUTIL_PPU_WIN = 'C:\lazarus\components\lazutils\lib\x86_64-win64';
  LAZUTIL_PPU_WIN_VS = 'C:\\lazarus\\components\\lazutils\\lib\\x86_64-win64';
  LAZUTIL_PPU_LIN = '/usr/lib/lazarus/components/lazutils/lib/x86_64-linux';

  FCL_PPU_MAC = '/Applications/Lazarus/packager/units/x86_64-darwin';
  FCL_PPU_WIN = 'C:\lazarus\packager\units\x86_64-win64';
  FCL_PPU_WIN_VS = 'C:\\lazarus\\packager\\units\\x86_64-win64';
  FCL_PPU_LIN = '/usr/lib/lazarus/packager/units/x86_64-linux';

function packVar(str: string): string;

implementation

function packVar(str: string): string;
begin
  {$IFDEF WINDOWS}
  Exit('%' + str + '%');
  {$ELSE}
  Exit('${' + str + '}');
  {$ENDIF}
end;

end.

