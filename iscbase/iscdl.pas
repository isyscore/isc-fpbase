unit ISCDL;

{$mode objfpc}{$H+}

{$IFDEF LINUX}
  // add libtool / libltdl to system library
  {$linklib ltdl}
  // must link musl-libc for alpine
  {$linklib c}
{$ENDIF}

interface

uses
  Classes, SysUtils{$IFNDEF LINUX}, dynlibs{$ENDIF};

{$IFDEF LINUX}
type
  TDLHandler = Pointer;
  PDLSymList = ^TDLSymList;
  TDLSymList = record
    name: PChar;
    address: Pointer;
  end;
  TDLPreloadCallback = function (handle: TDLHandler): Integer;
  PDLInfo = ^TDLInfo;
  TDLInfo = record
    filename: PChar;
    name: PChar;
    ref_count: Integer;
    is_resident: UInt32;
    is_symglobal: UInt32;
    is_symlocal: UInt32;
  end;

function dlinit(): Integer; cdecl; external name 'lt_dlinit';
function dlexit(): Integer; cdecl; external name 'lt_dlexit';

function dladdsearchdir(const searchDir: PChar): Integer; cdecl; external name 'lt_dladdsearchdir';
function dlinsertsearchdir(const before: PChar; const searchDir: PChar): Integer; cdecl; external name 'lt_dlinsertsearchdir';
function dlsetsearchpath(const searchPath: PChar): Integer; cdecl; external name 'lt_dlsetsearchpath';
function dlgetsearchpath(): PChar; cdecl; external name 'lt_dlgetsearchpath';
function dlopen(const filename: PChar): TDLHandler; cdecl; external name 'lt_dlopen';
function dlopenext(const filename: PChar): TDLHandler; cdecl; external name 'lt_dlopenext';
function dlsym(handle: TDLHandler; const name: PChar): Pointer; cdecl; external name 'lt_dlsym';
function dlerror(): PChar; cdecl; external name 'lt_dlerror';
function dlclose(handle: TDLHandler): Integer; cdecl; external name 'lt_dlclose';

function dlpreload(const preloaded: PDLSymList): Integer; cdecl; external name 'lt_dlpreload';
function dlpreload_default(const preloaded: PDLSymList): Integer; cdecl; external name 'lt_dlpreload_default';
function dlpreload_open(const originator: PChar; func: TDLPreloadCallback): Integer; cdecl; external name 'lt_dlpreload_open';

function dlgetinfo(handle: TDLHandler): PDLInfo; cdecl; external name 'lt_dlgetinfo';

{$ENDIF}

implementation

end.

