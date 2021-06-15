unit ISCJVM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ISCJNI, ISCDL;

type
  TJNICreateJavaVM = function(vm: PPJavaVM; AEnv: PPJNIEnv; p: pointer): jint; cdecl;
  PJVM = ^TJVM;
  TJVM = record
    env: PJNIEnv;
    jvm: PJavaVM;
  end;

type
  generic TISCJvmExecuteMethod<T> = function (env: PJNIEnv): T;

function ISCJvmPath:string;
function ISCInitJVM(jvmPath: string; classPath: string): Boolean;
function ISCFiniJVM(): Boolean;
generic function ISCJVMExecute<T>(block: specialize TISCJvmExecuteMethod<T>; ADefault: T): T;

var
  _jvm: TJVM;

implementation

var
  jniCreateJavaVM: TJNICreateJavaVM = nil;

function ISCJvmPath: string;
{$IFDEF WINDOWS}
const
  P1 = 'C:\Program Files\Java\jdk1.8.0_151\jre\bin\server\jvm.dll';
  P2 = 'C:\Program Files\Java\jre1.8.0_151\bin\server\jvm.dll';
{$ENDIF}
begin
  {$IFDEF DARWIN}
  Exit('/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home/jre/lib/server/libjvm.dylib');
  {$ELSE}
  {$IFDEF WINDOWS}
  if (FileExists(P2)) then Exit(P2);
  Exit(P1);
  {$ELSE}
  Exit('/usr/lib/jvm/java-8-openjdk/jre/lib/amd64/server/libjvm.so');
  {$ENDIF}
  {$ENDIF}
end;

function createJvm(mj: PJVM; classPath: string): PJNIEnv;
var
  env: PJNIEnv = nil;
  args: JavaVMInitArgs;
  options: JavaVMOption;
  ret: Integer;
begin
  args.version:= JNI_VERSION_1_6;
  args.nOptions:= 1;
  args.options:= @options;
  args.ignoreUnrecognized:= Pjboolean(0);
  options.optionString:= Pchar('-Djava.class.path=' + classPath);
  ret := jniCreateJavaVM(@(mj^.jvm), @env, @args);
  if (ret < 0) or (env = nil) then begin
    Exit(nil);
  end;
  Exit(env);
end;

function ISCInitJVM(jvmPath: string; classPath: string): Boolean;
var
  libJvm: {$IFDEF LINUX}Pointer{$ELSE}TLibHandle{$ENDIF};
begin
  if (not FileExists(jvmPath)) then Exit(False);
  {$IFDEF LINUX}
  dlinit();
  libJvm:= dlopen(PChar(jvmPath));
  jniCreateJavaVM:= TJNICreateJavaVM(dlsym(libJvm, 'JNI_CreateJavaVM'));
  {$ELSE}
  libJvm := LoadLibrary(jvmPath);
  jniCreateJavaVM:= TJNICreateJavaVM(GetProcAddress(libJvm, 'JNI_CreateJavaVM'));
  {$ENDIF}
  _jvm.env:= createJvm(@_jvm, classPath);
  Exit(_jvm.env <> nil);
end;

function ISCFiniJVM(): Boolean;
begin
  if (_jvm.jvm <> nil) then begin
    _jvm.jvm^^.DestroyJavaVM(_jvm.jvm);
    _jvm.jvm:= nil;
  end;
  Exit(False);
end;

generic function ISCJVMExecute<T>(block: specialize TISCJvmExecuteMethod<T>; ADefault: T): T;
var
  jvmPtr: PJavaVM;
  env: PJNIEnv;
begin
  jvmPtr:= _jvm.jvm;
  env := _jvm.env;
  if (env <> nil) then begin
    jvmPtr^^.AttachCurrentThread(jvmPtr, @env, nil);
    Result := block(env);
    jvmPtr^^.DetachCurrentThread(jvmPtr);
  end else begin
    Result:= ADefault;
  end;
end;

end.

