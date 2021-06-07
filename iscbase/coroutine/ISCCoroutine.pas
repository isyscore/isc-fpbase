unit ISCCoroutine;

{$mode delphi}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, variants, fgl;

type
  IISCCoroutine = interface;
  IISCCoroutineSettings = interface;

  TISCState = (esStopped, esStarted);
  TISCStates = set of TISCState;

  TCoroutineCallback = procedure(Const ACoroutine:IISCCoroutine);
  TCoroutineNestedCallback = procedure(Const ACoroutine:IISCCoroutine) is nested;
  TCoroutineMethod = procedure(Const ACoroutine:IISCCoroutine) of object;
  TArgCleanupCallback = procedure(Const AArg:Variant);
  TArgCleanupNestedCallback = procedure(Const AArg:Variant) is nested;
  TArgCleanupMethod = procedure(Const AArg:Variant) of object;

  TVariantArray = array of Variant;

  IISCAwait = interface
    ['{37AEEC70-FBB9-44FC-94FC-B27E03977D4C}']
    //property methods
    function GetGroupID: String;
    function GetSettings: IISCCoroutineSettings;
    function GetCoroutine: IISCCoroutine;
    function GetCoroutineID: String;
    property GroupID : String read GetGroupID;
    property CoroutineID : String read GetCoroutineID;
    property Settings : IISCCoroutineSettings read GetSettings;
    property Coroutine : IISCCoroutine read GetCoroutine;
    function Group(Const ACoroutine:IISCCoroutine):IISCAwait;
    function UpdateGroupID(Const AGroupID:String):IISCAwait;
  end;

  IISCCoroutineSettings = interface
    ['{1EE5C618-DA56-4101-BE75-1FD25F131ED2}']
    //property methods
    function GetAwait: IISCAwait;
    function GetMaxRunTime: Cardinal;
    function GetSynchStopEvents: Boolean;
    function GetCoroutine: IISCCoroutine;
    function GetForceTerminate: Boolean;
    function GetCoroutineName: String;
    procedure SetCoroutineName(const AValue: String);

    property MaxRuntime : Cardinal read GetMaxRunTime;
    property ForceTerminate : Boolean read GetForceTerminate;
    property SynchronizeStopEvents : Boolean read GetSynchStopEvents;
    property Name : String read GetCoroutineName write SetCoroutineName;
    property Await : IISCAwait read GetAwait;
    property Coroutine : IISCCoroutine read GetCoroutine;

    //methods
    function UpdateMaxRuntime(Const ARuntime:Cardinal):IISCCoroutineSettings;
    function UpdateForceTerminate(Const AForce:Boolean):IISCCoroutineSettings;
    function UpdateSynchronizeStopEvents(Const ASynch:Boolean):IISCCoroutineSettings;
    function UpdateCoroutineName(const AName : String) : IISCCoroutineSettings;
  end;

  IISCCoroutineEvents = interface
    ['{7709EF05-BC0F-41A2-AB2B-2FCD8F305D8D}']
    //property methods
    function GetOnStart: TCoroutineMethod;
    function GetOnStartCall: TCoroutineCallback;
    function GetOnStartNestCall: TCoroutineNestedCallback;
    function GetOnStop: TCoroutineMethod;
    function GetOnStopCall: TCoroutineCallback;
    function GetOnStopNestCall: TCoroutineNestedCallback;
    function GetCoroutine: IISCCoroutine;

    property OnStart : TCoroutineMethod read GetOnStart;
    property OnStartCallback : TCoroutineCallback read GetOnStartCall;
    property OnStartNestedCallback : TCoroutineNestedCallback read GetOnStartNestCall;

    property OnStop : TCoroutineMethod read GetOnStop;
    property OnStopCallback : TCoroutineCallback read GetOnStopCall;
    property OnStopNestedCallback : TCoroutineNestedCallback read GetOnStopNestCall;

    property Coroutine : IISCCoroutine read GetCoroutine;

    //methods
    function UpdateOnStart(Const AOnStart:TCoroutineMethod):IISCCoroutineEvents;
    function UpdateOnStartCallback(Const AOnStart:TCoroutineCallback):IISCCoroutineEvents;
    function UpdateOnStartNestedCallback(Const AOnStart:TCoroutineNestedCallback):IISCCoroutineEvents;
    function UpdateOnStop(Const AOnStop:TCoroutineMethod):IISCCoroutineEvents;
    function UpdateOnStopCallback(Const AOnStop:TCoroutineCallback):IISCCoroutineEvents;
    function UpdateOnStopNestedCallback(Const AOnStop:TCoroutineNestedCallback):IISCCoroutineEvents;
  end;

  IISCCoroutine = interface
    ['{998776BC-86C1-432D-B864-BC3A5FF6860A}']
    //property methods
    function GetSettings: IISCCoroutineSettings;
    function GetEvents: IISCCoroutineEvents;
    function GetExists(const AName: String): Boolean;
    function GetByName(const AName: String): Variant;
    function GetState: TISCState;

    //properties
    property Settings:IISCCoroutineSettings read GetSettings;
    property Events:IISCCoroutineEvents read GetEvents;
    property Exists[Const AName:String]:Boolean read GetExists;
    property ByName[Const AName:String]:Variant read GetByName;default;
    property State:TISCState read GetState;

    function AddArg(Const AName:String;Const AArg:Variant;
      Const AOnFinish:TArgCleanupMethod;
      Const AOnFinishCall:TArgCleanupCallback;
      Const AOnFinishNestedCall:TArgCleanupNestedCallback):IISCCoroutine;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      Const AOnFinish:TArgCleanupCallback):IISCCoroutine;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      Const AOnFinish:TArgCleanupNestedCallback):IISCCoroutine;overload;
    function AddArg(Const AName:String;Const AArg:Variant):IISCCoroutine;overload;

    function AddArgs(Const ANames:TStringArray;
      Const AArgs:TVariantArray):IISCCoroutine;

    function Setup(Const AStart:TCoroutineCallback;
      Const AError:TCoroutineCallback;Const ASuccess:TCoroutineCallback):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineCallback):IISCCoroutine;overload;

    function Setup(Const AStart:TCoroutineNestedCallback;
      Const AError:TCoroutineNestedCallback;Const ASuccess:TCoroutineNestedCallback):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineNestedCallback):IISCCoroutine;overload;

    function Setup(Const AStart:TCoroutineMethod;
      Const AError:TCoroutineMethod;Const ASuccess:TCoroutineMethod):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineMethod):IISCCoroutine;overload;

    procedure Start;
    procedure Stop;
  end;

  TISCArg = packed record
  private
    FCallback: TArgCleanupCallback;
    FData: Variant;
    FMethod: TArgCleanupMethod;
    FName: String;
    FNestCallback: TArgCleanupNestedCallback;
  public
    property Name : String read FName;
    property Data : Variant read FData;
    property Callback : TArgCleanupCallback read FCallback;
    property NestedCallback : TArgCleanupNestedCallback read FNestCallback;
    property Method : TArgCleanupMethod read FMethod;
    constructor Create(Const AName:String;Const AData:Variant;
      Const ACallback:TArgCleanupCallback;Const ANestedCallback:TArgCleanupNestedCallback;
      Const AMethod:TArgCleanupMethod);
  end;

  TISCArgs = array of TISCArg;
  PISCArgs = ^TISCArgs;

  TISCCoroutineImpl = class(TInterfacedObject, IISCCoroutineEvents, IISCCoroutineSettings, IISCAwait, IISCCoroutine)
  public
    type
      TInternalCoroutine = class(TThread)
      strict private
        FError: TCoroutineMethod;
        FErrorCall: TCoroutineCallback;
        FErrorNestCall: TCoroutineNestedCallback;
        FStart: TCoroutineMethod;
        FStartCall: TCoroutineCallback;
        FStartNestCall: TCoroutineNestedCallback;
        FSuccess: TCoroutineMethod;
        FSuccessCall: TCoroutineCallback;
        FSuccessNestCall: TCoroutineNestedCallback;
        FCoroutine: IISCCoroutine;
        FCaller: TThread;
        function GetCoroutine: IISCCoroutine;
        procedure SetCoroutine(Const AValue: IISCCoroutine);
      strict protected
        type
          TEventHelper = class
          strict private
            FCaller: TThread;
            FCoroutine: IISCCoroutine;
            procedure SetCoroutine(Const AValue: IISCCoroutine);
          strict protected
            procedure DoSynchMethod;virtual;abstract;
          public
            property Coroutine : IISCCoroutine read FCoroutine write SetCoroutine;
            property Caller : TThread read FCaller write FCaller;
            procedure SynchMethod;
            constructor Create(Const ACoroutine:IISCCoroutine;
              Const ACaller:TThread);virtual;overload;
            destructor Destroy; override;
          end;

          TObjHelper = class(TEventHelper)
          strict private
            FMethod: TCoroutineMethod;
          strict protected
            procedure DoSynchMethod; override;
          public
            property Method : TCoroutineMethod read FMethod write FMethod;
            constructor Create(const ACoroutine: IISCCoroutine; Const ACaller:TThread;
              Const AMethod:TCoroutineMethod);overload;
          end;

          TCallHelper = class(TEventHelper)
          strict private
            FCallback: TCoroutineCallback;
          strict protected
            procedure DoSynchMethod; override;
          public
            property Callback : TCoroutineCallback read FCallback write FCallback;
            constructor Create(const ACoroutine: IISCCoroutine; Const ACaller:TThread;
              Const ACallback:TCoroutineCallback);overload;
          end;

          TNestCallHelper = class(TEventHelper)
          strict private
            FNestCallback: TCoroutineNestedCallback;
          strict protected
            procedure DoSynchMethod; override;
          public
            property NestedCallback : TCoroutineNestedCallback read FNestCallback write FNestCallback;
            constructor Create(const ACoroutine: IISCCoroutine; Const ACaller:TThread;
              Const ANestCallback:TCoroutineNestedCallback); overload;
          end;
      protected
        procedure Execute; override;
      public
        property ISCCoroutine : IISCCoroutine read GetCoroutine write SetCoroutine;
        property Caller : TThread read FCaller write FCaller;

        property StartMethod : TCoroutineMethod read FStart write FStart;
        property StartCallback : TCoroutineCallback read FStartCall write FStartCall;
        property StartNestedCallback : TCoroutineNestedCallback read FStartNestCall write FStartNestCall;

        property ErrorMethod : TCoroutineMethod read FError write FError;
        property ErrorCallback : TCoroutineCallback read FErrorCall write FErrorCall;
        property ErrorNestedCallback : TCoroutineNestedCallback read FErrorNestCall write FErrorNestCall;

        property Success : TCoroutineMethod read FSuccess write FSuccess;
        property SuccessCallback : TCoroutineCallback read FSuccessCall write FSuccessCall;
        property SuccessNestedCallback : TCoroutineNestedCallback read FSuccessNestCall write FSuccessNestCall;

        procedure RaiseStopEvents;
        destructor Destroy; override;
      end;

      TInternalCoroutineClass = class of TInternalCoroutine;
  strict private
    type
      TMonitorThread = class;
      TMonitorList = TFPGMapObject<String,TMonitorThread>;

      TMonitorThread = class(TThread)
      public
        type
          TOnDone = procedure(ACoroutine:IISCCoroutine) of object;
          TCheckStopRequest = function : Boolean of object;
      strict private
        FCheckStop: TCheckStopRequest;
        FID: String;
        FList: TMonitorList;
        FOnDone: TOnDone;
        FCoroutine: TInternalCoroutine;
        FStopRequest: Boolean;
        FKilled: Boolean;
        FLockedEvent : Boolean;
      protected
        procedure Execute; override;
        procedure DoOnDone(ACoroutine:IISCCoroutine);
        function DoGetShouldStop : Boolean;
      public
        property OnDone : TOnDone read FOnDone write FOnDone;
        property CheckStop : TCheckStopRequest read FCheckStop write FCheckStop;
        property InternalCoroutine : TInternalCoroutine read FCoroutine write FCoroutine;
        property ID : String read FID write FID;
        property InLockedEvent : Boolean read FLockedEvent;
        property List : TMonitorList read FList write FList;
        destructor Destroy; override;
      end;
  protected
    function GetAwait: IISCAwait;
    function GetByName(const AName: String): Variant;
    function GetExists(const AName: String): Boolean;
    function GetForceTerminate: Boolean;
    function GetGroupID: String;
    function GetMaxRunTime: Cardinal;
    function GetOnStart: TCoroutineMethod;
    function GetOnStartCall: TCoroutineCallback;
    function GetOnStartNestCall: TCoroutineNestedCallback;
    function GetOnStop: TCoroutineMethod;
    function GetOnStopCall: TCoroutineCallback;
    function GetOnStopNestCall: TCoroutineNestedCallback;
    function GetState: TISCState;
    function GetSynchStopEvents: Boolean;
    function GetCoroutine: IISCCoroutine;
    function GetSettings: IISCCoroutineSettings;
    function GetEvents: IISCCoroutineEvents;
    function GetCoroutineID: String;
    function GetCoroutineName: String;
    procedure SetCoroutineName(const AValue: String);
  strict private
    FMaxRunTime: Cardinal;
    FStart,
    FError,
    FSuccess,
    FOnStart,
    FOnStop: TCoroutineMethod;
    FStartCall,
    FErrorCall,
    FSuccessCall,
    FOnStartCall,
    FOnStopCall: TCoroutineCallback;
    FStartNestCall,
    FErrorNestCall,
    FSuccessNestCall,
    FOnStartNestCall,
    FOnStopNestCall: TCoroutineNestedCallback;
    FArgs: TISCArgs;
    FMonitorThreads: TMonitorList;
    FSynchStopEvents,
    FForceTerminate,
    FStopMonitor: Boolean;
    FCoroutineID,
    FGroupID,
    FName: String;
    FState: TISCState;
    function IndexOfArg(Const AName:String):Integer;
    procedure UpdateState(ACoroutine:IISCCoroutine);
    function GetMonitorStop : Boolean;
    function InLockedEvent : Boolean;
  strict protected
    function GetArgs : PISCArgs;

    function DoGetCoroutineClass : TInternalCoroutineClass;virtual;

    procedure DoSetupInternalCoroutine(const ACoroutine : TInternalCoroutine); virtual;
    procedure DoBeforeStart(const ACoroutine : IISCCoroutine); virtual;
    procedure DoAfterStart(const ACoroutine : IISCCoroutine); virtual;
    procedure DoBeforeStop(const ACoroutine : IISCCoroutine); virtual;
    procedure DoAfterStop(const ACoroutine : IISCCoroutine); virtual;

    class procedure AddCoroutineToAwaitCollection(const ACoroutine : IISCCoroutine); static;
    class procedure RemoveCoroutineFromAwaitCollection(const ACoroutine : IISCCoroutine); static;
  public
    //events
    property OnStart : TCoroutineMethod read GetOnStart;
    property OnStartCallback : TCoroutineCallback read GetOnStartCall;
    property OnStartNestedCallback : TCoroutineNestedCallback read GetOnStartNestCall;
    property OnStop : TCoroutineMethod read GetOnStop;
    property OnStopCallback : TCoroutineCallback read GetOnStopCall;
    property OnStopNestedCallback : TCoroutineNestedCallback read GetOnStopNestCall;
  public
    //properties
    property MaxRuntime : Cardinal read GetMaxRunTime;
    property SynchronizeStopEvents : Boolean read GetSynchStopEvents;
    property ForceTerminate : Boolean read GetForceTerminate;
    property GroupID : String read GetGroupID;
    property CoroutineID : String read GetCoroutineID;
    property Name : String read GetCoroutineName write SetCoroutineName;

    property Coroutine : IISCCoroutine read GetCoroutine;
    property Settings:IISCCoroutineSettings read GetSettings;
    property Await : IISCAwait read GetAwait;
    property Events:IISCCoroutineEvents read GetEvents;
    property Exists[Const AName:String]:Boolean read GetExists;
    property ByName[Const AName:String]:Variant read GetByName;default;
    property State:TISCState read GetState;

    //methods
    function UpdateOnStart(Const AOnStart:TCoroutineMethod):IISCCoroutineEvents;
    function UpdateOnStartCallback(Const AOnStart:TCoroutineCallback):IISCCoroutineEvents;
    function UpdateOnStartNestedCallback(Const AOnStart:TCoroutineNestedCallback):IISCCoroutineEvents;
    function UpdateOnStop(Const AOnStop:TCoroutineMethod):IISCCoroutineEvents;
    function UpdateOnStopCallback(Const AOnStop:TCoroutineCallback):IISCCoroutineEvents;
    function UpdateOnStopNestedCallback(Const AOnStop:TCoroutineNestedCallback):IISCCoroutineEvents;
    function UpdateMaxRuntime(Const ARuntime:Cardinal):IISCCoroutineSettings;
    function UpdateForceTerminate(Const AForce:Boolean):IISCCoroutineSettings;
    function UpdateSynchronizeStopEvents(Const ASynch:Boolean):IISCCoroutineSettings;
    function UpdateCoroutineName(const AName : String) : IISCCoroutineSettings;
    function AddArg(Const AName:String;Const AArg:Variant;
      Const AOnFinish:TArgCleanupMethod;
      Const AOnFinishCall:TArgCleanupCallback;
      Const AOnFinishNestedCall:TArgCleanupNestedCallback):IISCCoroutine;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      Const AOnFinish:TArgCleanupCallback):IISCCoroutine;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      Const AOnFinish:TArgCleanupNestedCallback):IISCCoroutine;overload;
    function AddArg(Const AName:String;Const AArg:Variant;
      Const AOnFinish:TArgCleanupMethod):IISCCoroutine;overload;
    function AddArg(Const AName:String;Const AArg:Variant):IISCCoroutine;overload;
    function AddArgs(Const ANames:TStringArray;
      Const AArgs:TVariantArray):IISCCoroutine;
    function Setup(Const AStart:TCoroutineCallback;
      Const AError:TCoroutineCallback;Const ASuccess:TCoroutineCallback):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineCallback):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineNestedCallback;
      Const AError:TCoroutineNestedCallback;Const ASuccess:TCoroutineNestedCallback):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineNestedCallback):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineMethod;
      Const AError:TCoroutineMethod;Const ASuccess:TCoroutineMethod):IISCCoroutine;overload;
    function Setup(Const AStart:TCoroutineMethod):IISCCoroutine;overload;
    function Group(Const ACoroutine:IISCCoroutine):IISCAwait;
    function UpdateGroupID(Const AGroupID:String):IISCAwait;

    procedure Start;
    procedure Stop;

    constructor Create;virtual;
    destructor Destroy; override;
  end;

  TISCCoroutineImplClass = class of TISCCoroutineImpl;

  procedure Await(Const ASleep:Cardinal=10);overload;
  procedure Await(Const ACoroutine:IISCCoroutine; Const ASleep:Cardinal=10);overload;
  procedure Await(Const AGroupID:String;Const ASleep:Cardinal=10);overload;

function NewISCCoroutine : IISCCoroutine;

implementation
uses
  syncobjs,
  {$IfDef LCL}
  Forms,
  {$ENDIF}
  ISCCoroutineCollection,
  ISCCoroutinePool;

var
  Critical : TCriticalSection;
  Collection : IISCCollection;

procedure Await(Const ASleep:Cardinal=10);
var
  I:Integer;
  LGroups:TStringArray;
begin
  //fetch the current Coroutine groups
  LGroups:=Collection.CoroutineGroups;

  //call await for each group id
  for I:=0 to High(LGroups) do
    Await(LGroups[I],ASleep);
end;

procedure Await(const ACoroutine: IISCCoroutine; Const ASleep:Cardinal=10);
var
  LCoroutine : IISCCoroutine;
  LPool : IISCCoroutinePool;
begin
  //ensure we have a reference
  LCoroutine := ACoroutine;

  if not Assigned(LCoroutine) then
    Exit;

  //nothing to do if the Coroutine hasn't been started
  if LCoroutine.State <> esStarted then
    Exit;

  //support Coroutine pools being passed in
  if LCoroutine is IISCCoroutinePool then
  begin
    LPool := LCoroutine as IISCCoroutinePool;

    //use Coroutine pool worker group here in group await call
    Await(LPool.WorkerGroupID, ASleep);

    //stop the Coroutine pool since caller request to wait for the end of tasks
    LPool.Stop;
  end
  else
    //use Coroutine id here to check against non-nil result
    while Collection.Coroutines[LCoroutine.Settings.Await.CoroutineID] <> nil do
    begin
      {$IfDef LCL}
      if Assigned(Application) then
        Application.ProcessMessages;
      {$ENDIF}
      Sleep(ASleep);
    end;
end;

procedure Await(const AGroupID: String;Const ASleep:Cardinal);
begin
  //Coroutines add and remove themselves from the collection, so
  while Collection.Exists(AGroupID) do
  begin
    {$IfDef LCL}
    if Assigned(Application) then
      Application.ProcessMessages;
    {$ENDIF}
    Sleep(ASleep);
  end;
end;

function NewISCCoroutine: IISCCoroutine;
begin
  Result := TISCCoroutineImpl.Create;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.TNestCallHelper.DoSynchMethod;
begin
  try
    if Assigned(FNestCallback) then
      FNestCallback(Coroutine);
  finally
    Free;
  end;
end;

constructor TISCCoroutineImpl.TInternalCoroutine.TNestCallHelper.Create(
  const ACoroutine: IISCCoroutine; Const ACaller:TThread; const ANestCallback: TCoroutineNestedCallback);
begin
  inherited Create(ACoroutine, ACaller);
  FNestCallback:=ANestCallback;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.TCallHelper.DoSynchMethod;
begin
  try
    if Assigned(FCallback) then
      FCallback(Coroutine);
  finally
    Free;
  end;
end;

constructor TISCCoroutineImpl.TInternalCoroutine.TCallHelper.Create(
  const ACoroutine: IISCCoroutine;Const ACaller:TThread; const ACallback: TCoroutineCallback);
begin
  inherited Create(ACoroutine, ACaller);
  FCallback:=ACallback;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.TObjHelper.DoSynchMethod;
begin
  try
    if Assigned(FMethod) then
      FMethod(Coroutine);
  finally
    Free;
  end;
end;

constructor TISCCoroutineImpl.TInternalCoroutine.TObjHelper.Create(
  const ACoroutine: IISCCoroutine;Const ACaller:TThread; const AMethod: TCoroutineMethod);
begin
  inherited Create(ACoroutine, ACaller);
  FMethod:=AMethod;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.TEventHelper.SetCoroutine(
  const AValue: IISCCoroutine);
begin
  FCoroutine:=nil;
  FCoroutine:=AValue;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.TEventHelper.SynchMethod;
begin
  TThread.Synchronize(Caller,DoSynchMethod);
end;

constructor TISCCoroutineImpl.TInternalCoroutine.TEventHelper.Create(
  const ACoroutine: IISCCoroutine;Const ACaller:TThread);
begin
  Coroutine:=ACoroutine;
  FCaller:=ACaller;
end;

destructor TISCCoroutineImpl.TInternalCoroutine.TEventHelper.Destroy;
begin
  FCoroutine:=nil;
  inherited Destroy;
end;

procedure TISCCoroutineImpl.TMonitorThread.Execute;
var
  LElapsed,
  LSleep,
  LMax:Cardinal;
  LForceKill:Boolean;

  procedure RemoveID(Const AID:String;Const AList:TMonitorList);
  var
    LCoroutine : IISCCoroutine;
  begin
    //local ref
    LCoroutine := FCoroutine.ISCCoroutine;

    Critical.Enter;
    try
       FLockedEvent := True;

      //raise stop event safely as long as it wasn't killed forcefully
      //(in this case it will have already been called)
      try
        if not FKilled then
          FCoroutine.RaiseStopEvents;
      finally
      end;

      //call done after all other events since it's the last in the chain
      try
        DoOnDone(LCoroutine);
      finally
      end;

      //lastly, remove ourselves from the list
      if AList.IndexOf(AID) < 0 then
        Exit;

      FLockedEvent := False;
      AList.Remove(AID);
    finally
      Critical.Leave;
    end;
  end;

  (*
    checks for force killing, and handles it
  *)
  function CheckForceKill : Boolean;
  begin
    LForceKill := FCoroutine.ISCCoroutine.Settings.ForceTerminate;
    Result := LForceKill;

    //if settings say we should forcefull terminate, do so but
    //be warned, this may cause problems...
    if LForceKill then
    begin
      //if we get here, then the Coroutine has passed the alotted max, so forcefull
      //terminate it
      if not FCoroutine.Finished then
        FCoroutine.Terminate;

      //here we check one last time for finished to make sure to avoid a kill if possible
      if (not FCoroutine.Finished)
        and (LForceKill)
      then
      begin
        //since we are killing, caller will still expect
        //for their events to occur
        FCoroutine.RaiseStopEvents;

        //kill it dead
        KillThread(FCoroutine.Handle);
        FKilled:=True;
        Exit;
      end;
    end;
  end;

begin
  try

    FStopRequest := DoGetShouldStop;
    FKilled := False;
    LElapsed:=0;
    LMax := FCoroutine.ISCCoroutine.Settings.MaxRuntime;

    //if we're finished, nothing to do
    if FCoroutine.Finished or FStopRequest then
      Exit;

    //can run until the end of time
    if LMax > 0 then
    begin
      FStopRequest := DoGetShouldStop;
      LSleep := LMax div 10;

      while LElapsed < LMax do
      begin
        Sleep(LSleep);
        Inc(LElapsed,LSleep);

        if FCoroutine.Finished then
          Exit;

        //check if a request has been made
        FStopRequest := DoGetShouldStop;

        if FStopRequest then
          Break;
      end;

      CheckForceKill;
    end;

    //wait until the Coroutine is finished
    while Assigned(FCoroutine) and not FCoroutine.Finished do
    begin
      //check if we should stop
      FStopRequest := DoGetShouldStop;

      //terminate the Coroutine and wait for it to respond
      if FStopRequest then
        if CheckForceKill then
          Exit
        else
          FCoroutine.Terminate;

      //sleep smallest granularity
      Sleep(1);
    end;
  finally
    //raises stop events, and removes from list
    RemoveID(FID,FList);
  end;
end;

procedure TISCCoroutineImpl.TMonitorThread.DoOnDone(ACoroutine: IISCCoroutine);
begin
  if Assigned(FOnDone) then
    FOnDone(ACoroutine);
end;

function TISCCoroutineImpl.TMonitorThread.DoGetShouldStop: Boolean;
begin
  Result := False;

  if Assigned(FCheckStop) then
    Result := FCheckStop;
end;

destructor TISCCoroutineImpl.TMonitorThread.Destroy;
begin
  if Assigned(FCoroutine) and (not FKilled) then
    FCoroutine.Free;
  inherited Destroy;
end;

function TISCCoroutineImpl.TInternalCoroutine.GetCoroutine: IISCCoroutine;
begin
  Result := FCoroutine;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.SetCoroutine(const AValue: IISCCoroutine);
begin
  FCoroutine := nil;
  FCoroutine := AValue;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.Execute;
var
  LCoroutine : IISCCoroutine;
begin
  //local ref
  LCoroutine := FCoroutine;

  if Assigned(LCoroutine) then
  begin
    try
      //attempt to run all applicable StartMethod methods
      if Assigned(FStart) then
      begin
        FStart(LCoroutine);
        if Terminated then
          Exit;
      end;
      if Assigned(FStartCall) then
      begin
        FStartCall(LCoroutine);
        if Terminated then
          Exit;
      end;
      if Assigned(FStartNestCall) then
      begin
        FStartNestCall(LCoroutine);
        if Terminated then
          Exit;
      end;

      //now run success methods
      if Assigned(FSuccess) then
      begin
        FSuccess(LCoroutine);
        if Terminated then
          Exit;
      end;
      if Assigned(FSuccessCall) then
      begin
        FSuccessCall(LCoroutine);
        if Terminated then
          Exit;
      end;
      if Assigned(FSuccessNestCall) then
      begin
        FSuccessNestCall(LCoroutine);
        if Terminated then
          Exit;
      end;
    except on E:Exception do
    begin
      //todo - expand the ErrorMethod methods to accept either a TException or
      //just an ErrorMethod message string

      //guarantee all ErrorMethod methods are called with try..finally
      if Assigned(FError) then
        try
          FError(LCoroutine);
        finally
        end;
      if Assigned(FErrorCall) then
        try
          FErrorCall(LCoroutine);
        finally
        end;
      if Assigned(FErrorNestCall) then
        try
          FErrorNestCall(LCoroutine);
        finally
        end;
    end
    end;
  end;
end;

procedure TISCCoroutineImpl.TInternalCoroutine.RaiseStopEvents;
var
  LCoroutine : IISCCoroutine;
begin
  //local ref
  LCoroutine := ISCCoroutine;

  //below we use the appropriate synch helper object to handle the
  //method. these objects free themselves once done
  if LCoroutine.Settings.SynchronizeStopEvents then
  begin
    if Assigned(LCoroutine.Events.OnStop) then
      TObjHelper.Create(LCoroutine, FCaller, LCoroutine.Events.OnStop).SynchMethod;

    if Assigned(LCoroutine.Events.OnStopCallback) then
      TCallHelper.Create(LCoroutine, FCaller, LCoroutine.Events.OnStopCallback).SynchMethod;

    if Assigned(LCoroutine.Events.OnStopNestedCallback) then
      TNestCallHelper.Create(LCoroutine, FCaller, LCoroutine.Events.OnStopNestedCallback).SynchMethod;
  end
  //otherwise caller does not want the stop events to be raised (perhaps
  //they are in a console app? https://forum.lazarus.freepascal.org/index.php?topic=23442.0)
  else
  begin
    if Assigned(LCoroutine.Events.OnStop) then
      LCoroutine.Events.OnStop(LCoroutine);

    if Assigned(LCoroutine.Events.OnStopCallback) then
      LCoroutine.Events.OnStopCallback(LCoroutine);

    if Assigned(LCoroutine.Events.OnStopNestedCallback) then
      LCoroutine.Events.OnStopNestedCallback(LCoroutine);
  end;
end;

destructor TISCCoroutineImpl.TInternalCoroutine.Destroy;
begin
  FCoroutine:=nil;
  inherited Destroy;
end;

function TISCCoroutineImpl.GetMaxRunTime: Cardinal;
begin
  Result:=FMaxRunTime;
end;

function TISCCoroutineImpl.GetExists(const AName: String): Boolean;
var
  I:Integer;
begin
  I:=IndexOfArg(AName);
  Result:=I >= 0;
end;

function TISCCoroutineImpl.GetForceTerminate: Boolean;
begin
  Result:=FForceTerminate;
end;

function TISCCoroutineImpl.GetGroupID: String;
begin
  Result:=FGroupID;
end;

function TISCCoroutineImpl.GetByName(const AName: String): Variant;
begin
  if Exists[AName] then
    Result:=FArgs[IndexOfArg(AName)].Data
  else
    Result:=nil;
end;

function TISCCoroutineImpl.GetAwait: IISCAwait;
begin
  Result:=Self as IISCAwait;
end;

function TISCCoroutineImpl.GetOnStart: TCoroutineMethod;
begin
  Result:=FOnStart;
end;

function TISCCoroutineImpl.GetOnStartCall: TCoroutineCallback;
begin
  Result:=FOnStartCall;
end;

function TISCCoroutineImpl.GetOnStartNestCall: TCoroutineNestedCallback;
begin
  Result:=FOnStartNestCall;
end;

function TISCCoroutineImpl.GetOnStop: TCoroutineMethod;
begin
  Result:=FOnStop;
end;

function TISCCoroutineImpl.GetOnStopCall: TCoroutineCallback;
begin
  Result:=FOnStopCall;
end;

function TISCCoroutineImpl.GetOnStopNestCall: TCoroutineNestedCallback;
begin
  Result:=FOnStopNestCall;
end;

function TISCCoroutineImpl.GetState: TISCState;
begin
  Result:=FState;
end;

function TISCCoroutineImpl.GetSynchStopEvents: Boolean;
begin
  Result:=FSynchStopEvents;
end;

function TISCCoroutineImpl.GetCoroutine: IISCCoroutine;
begin
  Result:=Self as IISCCoroutine;
end;

function TISCCoroutineImpl.GetSettings: IISCCoroutineSettings;
begin
  Result:=Self as IISCCoroutineSettings;
end;

function TISCCoroutineImpl.GetEvents: IISCCoroutineEvents;
begin
  Result:=Self as IISCCoroutineEvents;
end;

function TISCCoroutineImpl.GetCoroutineID: String;
begin
  Result:=FCoroutineID;
end;

function TISCCoroutineImpl.GetCoroutineName: String;
begin
  Result := FName;
end;

procedure TISCCoroutineImpl.SetCoroutineName(const AValue: String);
begin
  FName := AValue;
end;

function TISCCoroutineImpl.IndexOfArg(const AName: String): Integer;
var
  I:Integer;
begin
  Result:=-1;

  //don't aquire lock if we are being called my an event
  if not InLockedEvent then
    Critical.Enter;
  try
    for I:=0 to High(FArgs) do
      if FArgs[I].Name=AName then
      begin
        Result:=I;
        Exit;
      end;
  finally
    if not InLockedEvent then
      Critical.Leave;
  end;
end;

procedure TISCCoroutineImpl.UpdateState(ACoroutine:IISCCoroutine);
begin
  //on done gets called before monitor Coroutine removes itself from list,
  //so count of 1 actually means we'll be stopped
  Critical.Enter;
  try
    if FMonitorThreads.Count <= 1 then
    begin
      FState:=esStopped;

      //once stopped, remove this Coroutine from the collection
      Collection.Remove(ACoroutine);
    end;
  finally
    Critical.Leave;
  end;
end;

function TISCCoroutineImpl.GetMonitorStop: Boolean;
begin
  Result := FStopMonitor;
end;

function TISCCoroutineImpl.InLockedEvent: Boolean;
var
  I: Integer;
  LMonitor: TMonitorThread;
begin
  Result := False;

  //check if at least one monitor thread has aquired an event lock,
  //if so then we need to return true to avoid deadlocks on the critical section
  for I := 0 to Pred(FMonitorThreads.Count) do
  begin
    LMonitor := FMonitorThreads.Data[I];

    //should not happen
    if not Assigned(LMonitor) then
      Continue;

    if LMonitor.InLockedEvent then
      Exit(True);
  end;
end;

function TISCCoroutineImpl.GetArgs: PISCArgs;
begin
  Result := @FArgs;
end;

function TISCCoroutineImpl.DoGetCoroutineClass: TInternalCoroutineClass;
begin
  //base class returns base internal Coroutine class
  Result:=TInternalCoroutine;
end;

procedure TISCCoroutineImpl.DoSetupInternalCoroutine(const ACoroutine: TInternalCoroutine);
begin
  //nothing in base
end;

procedure TISCCoroutineImpl.DoBeforeStart(const ACoroutine: IISCCoroutine);
begin
  //nothing in base
end;

procedure TISCCoroutineImpl.DoAfterStart(const ACoroutine: IISCCoroutine);
begin
  //nothing in base
end;

procedure TISCCoroutineImpl.DoBeforeStop(const ACoroutine: IISCCoroutine);
begin
  //nothing in base
end;

procedure TISCCoroutineImpl.DoAfterStop(const ACoroutine: IISCCoroutine);
begin
  //nothing in base
end;

class procedure TISCCoroutineImpl.AddCoroutineToAwaitCollection(
  const ACoroutine: IISCCoroutine);
begin
  if not Assigned(ACoroutine) then
    Exit;

  Collection.Add(ACoroutine);
end;

class procedure TISCCoroutineImpl.RemoveCoroutineFromAwaitCollection(
  const ACoroutine: IISCCoroutine);
begin
  Collection.Remove(ACoroutine);
end;

function TISCCoroutineImpl.UpdateOnStart(const AOnStart: TCoroutineMethod): IISCCoroutineEvents;
begin
  FOnStart:=AOnStart;
  Result:=GetEvents;
end;

function TISCCoroutineImpl.UpdateOnStartCallback(const AOnStart: TCoroutineCallback): IISCCoroutineEvents;
begin
  FOnStartCall:=AOnStart;
  Result:=GetEvents;
end;

function TISCCoroutineImpl.UpdateOnStartNestedCallback(
  const AOnStart: TCoroutineNestedCallback): IISCCoroutineEvents;
begin
  FOnStartNestCall:=AOnStart;
  Result:=GetEvents;
end;

function TISCCoroutineImpl.UpdateOnStop(const AOnStop: TCoroutineMethod): IISCCoroutineEvents;
begin
  FOnStop:=AOnStop;
  Result:=GetEvents;
end;

function TISCCoroutineImpl.UpdateOnStopCallback(const AOnStop: TCoroutineCallback): IISCCoroutineEvents;
begin
  FOnStopCall:=AOnStop;
  Result:=GetEvents;
end;

function TISCCoroutineImpl.UpdateOnStopNestedCallback(
  const AOnStop: TCoroutineNestedCallback): IISCCoroutineEvents;
begin
  FOnStopNestCall:=AOnStop;
  Result:=GetEvents;
end;

function TISCCoroutineImpl.UpdateMaxRuntime(const ARuntime: Cardinal): IISCCoroutineSettings;
begin
  FMaxRunTime:=ARunTime;
  Result:=GetSettings;
end;

function TISCCoroutineImpl.UpdateForceTerminate(const AForce: Boolean): IISCCoroutineSettings;
begin
  FForceTerminate:=AForce;
  Result:=GetSettings;
end;

function TISCCoroutineImpl.UpdateSynchronizeStopEvents(const ASynch: Boolean): IISCCoroutineSettings;
begin
  FSynchStopEvents:=ASynch;
  Result:=GetSettings;
end;

function TISCCoroutineImpl.UpdateCoroutineName(const AName: String): IISCCoroutineSettings;
begin
  Result := GetSettings;
  FName := AName;
end;

function TISCCoroutineImpl.AddArg(const AName: String; const AArg: Variant;
  const AOnFinish: TArgCleanupMethod; const AOnFinishCall: TArgCleanupCallback;
  const AOnFinishNestedCall: TArgCleanupNestedCallback): IISCCoroutine;
var
  I:Integer;
  LArg:TISCArg;
begin
  //see if this arg already exists by fetching the index
  I:=IndexOfArg(AName);

  //regardless of existing, we are either going to perform and
  //update or create, so create the argument with the params
  LArg:=TISCArg.Create(AName,AArg,AOnFinishCall,AOnFinishNestedCall,AOnFinish);

  //enter critical section to avoid collisions
  Critical.Enter;
  try
    //we need to add a new arg
    if I < 0 then
    begin
      SetLength(FArgs,Succ(Length(FArgs)));
      FArgs[High(FArgs)]:=LArg;
    end
    //update existing arg
    else
    begin
      //in the case of an update we need to make sure any cleanup methods
      //get called if specified
      try
        if Assigned(FArgs[I].Callback) then
          FArgs[I].Callback(FArgs[I].Data);
        if Assigned(FArgs[I].Method) then
          FArgs[I].Method(FArgs[I].Data);
      finally
      end;

      //rewrite the arg
      FArgs[I]:=LArg;
    end;
  finally
    Critical.Leave;
  end;

  //lastly return the thread
  Result:=GetCoroutine;
end;

function TISCCoroutineImpl.AddArg(const AName: String; const AArg: Variant;
  const AOnFinish: TArgCleanupCallback): IISCCoroutine;
begin
  Result:=AddArg(AName,AArg,nil,AOnFinish,nil);
end;

function TISCCoroutineImpl.AddArg(const AName: String; const AArg: Variant;
  const AOnFinish: TArgCleanupNestedCallback): IISCCoroutine;
begin
  Result:=AddArg(AName,AArg,nil,nil,AOnFinish);
end;

function TISCCoroutineImpl.AddArg(const AName: String; const AArg: Variant;
  const AOnFinish: TArgCleanupMethod): IISCCoroutine;
begin
  Result:=AddArg(AName,AArg,AOnFinish,nil,nil);
end;

function TISCCoroutineImpl.AddArg(const AName: String; const AArg: Variant): IISCCoroutine;
begin
  Result:=AddArg(AName,AArg,nil,nil,nil);
end;

function TISCCoroutineImpl.AddArgs(const ANames: TStringArray;
  const AArgs: TVariantArray): IISCCoroutine;
var
  I:Integer;
begin
  //assumes arrays are equal
  for I:=0 to High(AArgs) do
    AddArg(ANames[I],AArgs[I]);
  Result:=GetCoroutine;
end;

function TISCCoroutineImpl.Setup(const AStart: TCoroutineCallback;
  const AError: TCoroutineCallback; const ASuccess: TCoroutineCallback): IISCCoroutine;
begin
  FStartCall:=AStart;
  FErrorCall:=AError;
  FSuccessCall:=ASuccess;
  Result:=GetCoroutine;
end;

function TISCCoroutineImpl.Setup(const AStart: TCoroutineCallback): IISCCoroutine;
begin
  Result:=Setup(AStart,nil,nil);
end;

function TISCCoroutineImpl.Setup(const AStart: TCoroutineNestedCallback;
  const AError: TCoroutineNestedCallback; const ASuccess: TCoroutineNestedCallback): IISCCoroutine;
begin
  FStartNestCall:=AStart;
  FErrorNestCall:=AError;
  FSuccessNestCall:=ASuccess;
  Result:=GetCoroutine;
end;

function TISCCoroutineImpl.Setup(const AStart: TCoroutineNestedCallback): IISCCoroutine;
begin
  Result:=Setup(AStart,nil,nil);
end;

function TISCCoroutineImpl.Setup(const AStart: TCoroutineMethod;
  const AError: TCoroutineMethod; const ASuccess: TCoroutineMethod): IISCCoroutine;
begin
  FStart:=AStart;
  FError:=AError;
  FSuccess:=ASuccess;
  Result:=GetCoroutine;
end;

function TISCCoroutineImpl.Setup(const AStart: TCoroutineMethod): IISCCoroutine;
begin
  Result:=Setup(AStart,nil,nil);
end;

function TISCCoroutineImpl.Group(const ACoroutine: IISCCoroutine): IISCAwait;
begin
  Result:=nil;
  UpdateGroupID(ACoroutine.Settings.Await.GroupID);
  Result:=GetAwait;
end;

function TISCCoroutineImpl.UpdateGroupID(const AGroupID: String): IISCAwait;
begin
  Result:=nil;
  if FState = esStarted then
    raise Exception.Create('group id cannot be changed while coroutine is started');
  FGroupID:=AGroupID;
  Result:=GetAwait;
end;

procedure TISCCoroutineImpl.Start;
var
  LIntCoroutine:TInternalCoroutine;
  LMonThread:TMonitorThread;
  LCoroutine:IISCCoroutine;
begin
  //allow children to handle before starting
  DoBeforeStart(GetCoroutine);

  //raise on start events
  if Assigned(FOnStart) then
    FOnStart(GetCoroutine);
  if Assigned(FOnStartCall) then
    FOnStartCall(GetCoroutine);
  if Assigned(FOnStartNestCall) then
    FOnStartNestCall(GetCoroutine);

  LCoroutine:=GetCoroutine;

  //create and initialize an internal Coroutine
  LIntCoroutine:=DoGetCoroutineClass.Create(True);
  LIntCoroutine.FreeOnTerminate:=False;//we handle memory
  LIntCoroutine.ISCCoroutine:=LCoroutine;
  LIntCoroutine.StartMethod:=FStart;
  LIntCoroutine.StartCallback:=FStartCall;
  LIntCoroutine.StartNestedCallback:=FStartNestCall;
  LIntCoroutine.Success:=FSuccess;
  LIntCoroutine.SuccessCallback:=FSuccessCall;
  LIntCoroutine.SuccessNestedCallback:=FSuccessNestCall;
  LIntCoroutine.ErrorMethod:=FError;
  LIntCoroutine.ErrorCallback:=FErrorCall;
  LIntCoroutine.ErrorNestedCallback:=FErrorNestCall;
  LIntCoroutine.Caller:=TThread.CurrentThread;
  LIntCoroutine.NameThreadForDebugging(FName, LIntCoroutine.ThreadID);
  DoSetupInternalCoroutine(LIntCoroutine);

  //create and setup monitor thread
  FStopMonitor := False;
  LMonThread:=TMonitorThread.Create(True);
  LMonThread.FreeOnTerminate:=True;//memory freed automatically
  LMonThread.ID:=TGuid.NewGuid.ToString();
  FMonitorThreads.Add(LMonThread.ID, LMonThread);
  LMonThread.List:=FMonitorThreads;
  LMonThread.InternalCoroutine:=LIntCoroutine;
  LMonThread.OnDone:=UpdateState;
  LMonThread.CheckStop := GetMonitorStop;
  LMonThread.NameThreadForDebugging(FName + '_monitor', LMonThread.ThreadID);

  //for await support, add ourself to the collection
  Collection.Add(LCoroutine);

  Critical.Enter;
  try
    //update state
    FState:=esStarted;

    //start the internal Coroutine
    LIntCoroutine.Start;

    //start the monitor thread
    LMonThread.Start;
  finally
    Critical.Leave;
  end;

  //allow children to handle after starting
  DoAfterStart(GetCoroutine);
end;

procedure TISCCoroutineImpl.Stop;
var
  LCoroutine : IISCCoroutine;
begin
  LCoroutine := GetCoroutine;

  //allow children to handle before stopping
  DoBeforeStop(LCoroutine);

  //signal to remaining Coroutines to stop
  FStopMonitor := True;

  //monitor threads will remove themselves from the list
  //so wait until this has been done
  while FMonitorThreads.Count > 0 do
    Continue;

  //allow children to handle after stopping
  DoAfterStop(LCoroutine);
end;

constructor TISCCoroutineImpl.Create;
begin
  FStopMonitor := False;
  FState := esStopped;
  FMaxRunTime := 0;
  FSynchStopEvents := False;
  FForceTerminate := False;
  FOnStart := nil;
  FOnStop := nil;
  FOnStartCall := nil;
  FOnStopCall := nil;
  SetLength(FArgs,0);
  FCoroutineID := TGuid.NewGuid.ToString;
  FName := FCoroutineID;
  FGroupID := TGuid.NewGuid.ToString;
  FMonitorThreads := TMonitorList.Create(False);
end;

destructor TISCCoroutineImpl.Destroy;
begin
  Stop;
  FMonitorThreads.Free;
  SetLength(FArgs,0);
  inherited Destroy;
end;

constructor TISCArg.Create(Const AName:String;Const AData:Variant;
  Const ACallback:TArgCleanupCallback;Const ANestedCallback:TArgCleanupNestedCallback;
  Const AMethod:TArgCleanupMethod);
begin
  FName:=AName;
  FData:=AData;
  FMethod:=AMethod;
  FCallback:=ACallback;
  FNestCallback:=ANestedCallback;
end;

initialization
  Critical:=TCriticalSection.Create;
  Collection:=TISCCollectionImpl.Create;
finalization
  if Assigned(Critical) then
    Critical.Free;
  Collection:=nil;
end.

