unit untHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untConsts;

procedure printHelp();

implementation

procedure printHelp();
begin
  WriteLn('iSyscore FPC Project Manager v' + VERSION);
  WriteLn('');
  WriteLn('Manage your iSyscore FPC app development.');
  WriteLn('');
  WriteLn('Common commands:');
  WriteLn('');
  WriteLn('  iscfpc create <project type> <output directory>');
  WriteLn('    Create a new iSyscore FPC project in the specified directory.');
  WriteLn('');
  WriteLn('  iscfpc build');
  WriteLn('    Build all FPC application in specified directory.');
  WriteLn('');
  WriteLn('Usage: iscfpc <command> [arguments]');
  WriteLn('');
  WriteLn('Global options:');
  WriteLn('-h, --help          Print this usage information.');
  WriteLn('    --version       Reports the version of this tool.');
  WriteLn('');
  WriteLn('Available commands:');
  WriteLn('  build             Build executable apps and libraries.');
  WriteLn('  clean             Delete the lib/ and $arch/ directories.');
  WriteLn('  create            Create a new iSyscore FPC project.');
  WriteLn('  doctor            Show information about the installed tooling.');
  WriteLn('  upgrade           Upgrade the "iscbase" dependencies for your app.');
  WriteLn('  ocean             Show or download fpc sample code in Code Ocean.');
  WriteLn('');
  WriteLn('Special commands:');
  WriteLn('  build ALPINE      Build executable apps and libraries work for Alpine, (Linux host only)');
  WriteLn('  create ... JAVA   Create a new iSysCore FPC project with Java Runtime.');
  WriteLn('  doctor fix        Fix VSCode config problem,');
  WriteLn('');
end;

end.

