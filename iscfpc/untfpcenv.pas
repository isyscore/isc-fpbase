unit untFPCEnv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untConsts;

function getFPCInstalled(): Boolean;
function getFPCSrcInstalled(): Boolean;
function getLazarusInstalled(): Boolean;

implementation

function getFPCInstalled(): Boolean;
begin
  Exit(FileExists(DELPHI_INSTALLATION_PATH));
end;

function getFPCSrcInstalled(): Boolean;
begin
  Exit(DirectoryExists(FREEPASCAL_SOURCE_PATH));
end;

function getLazarusInstalled(): Boolean;
begin
  Exit(DirectoryExists(LAZARUS_PATH));
end;

end.

