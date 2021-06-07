unit ISCCoroutinePool;

{$mode delphi}
{$modeswitch nestedprocvars}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  fgl,
  Generics.Collections,
  ISCCoroutine;

type

  IISCCoroutinePool = interface(IISCCoroutine)
    ['{3EBD544C-B802-4BA0-B353-876D717142F1}']

    //property methods
    function GetWorkerGroup: String;
    //properties
    property WorkerGroupID : String read GetWorkerGroup;
    //methods
    function Queue(Const AStart:TCoroutineCallback;
      Const AError:TCoroutineCallback;Const ASuccess:TCoroutineCallback) : IISCCoroutinePool; overload;
    function Queue(Const AStart:TCoroutineNestedCallback;
      Const AError:TCoroutineNestedCallback;Const ASuccess:TCoroutineNestedCallback) : IISCCoroutinePool; overload;
    function Queue(Const AStart:TCoroutineMethod;
      Const AError:TCoroutineMethod;Const ASuccess:TCoroutineMethod) : IISCCoroutinePool; overload;
    function UpdateWorkerCount(const AWorkers : Cardinal) : IISCCoroutinePool;
  end;

  TISCCoroutinePoolImpl = class(TISCCoroutineImpl, IISCCoroutinePool)
  public
    function GetWorkerGroup: String;
  public
    type
      TISCCoroutines = TFPGInterfacedObjectList<IISCCoroutine>;

      TCoroutineSetup<TMethodType> = record
      strict private
        FStart,
        FError,
        FSuccess : TMethodType;
      public
        property Start : TMethodType read FStart write FStart;
        property Error : TMethodType read FError write FError;
        property Success : TMethodType read FSuccess write FSuccess;
      end;

      TCallbackSetup = TCoroutineSetup<TCoroutineCallback>;
      TNestedCallbackSetup = TCoroutineSetup<TCoroutineNestedCallback>;
      TMethodSetup = TCoroutineSetup<TCoroutineMethod>;

      { TQueueItem }

      TQueueItem = record
      private
        FCall: TCallbackSetup;
        FMethod: TMethodSetup;
        FNested: TNestedCallbackSetup;

        procedure NilWork;
      public
        property Callback : TCallbackSetup read FCall write FCall;
        property Nested : TNestedCallbackSetup read FNested write FNested;
        property Method : TMethodSetup read FMethod write FMethod;

        constructor Create(const ACallback : TCallbackSetup); overload;
        constructor Create(const ANested : TNestedCallbackSetup); overload;
        constructor Create(const AMethod : TMethodSetup); overload;
      end;

      TMethodQueue = TQueue<TQueueItem>;

      IPlaceHolderCoroutine = interface(IISCCoroutine)
        ['{29750A1E-4F19-45B4-8692-709921EE913D}']
      end;

      TPlaceHolderCoroutineImpl = class(TISCCoroutineImpl, IPlaceHolderCoroutine)
      end;
  strict private
    FWorking : TArray<Boolean>;
    FWorkers : TISCCoroutines;
    FWorkerCount : Integer;
    FWorkerGroup : String;
    FWork : TMethodQueue;
    FCritical : TCriticalSection;
    FWorkerCritical : TCriticalSection;
    FPlaceHolders : TISCCoroutines;

    procedure AddPlaceHolder;
    procedure RemovePlaceHolder;

    const
      ARGS = 'args';
  public
    type

      TPoolWorkerCoroutine = class(TISCCoroutineImpl.TInternalCoroutine)
      private
        FPool: TISCCoroutinePoolImpl;
      protected
        procedure Execute; override;
      public
        property Pool : TISCCoroutinePoolImpl read FPool write FPool;
      end;
  strict protected
    function GetFreeWorker : IISCCoroutine;
    function DoGetCoroutineClass: TInternalCoroutineClass; override;
    procedure DoBeforeStart(const ACoroutine: IISCCoroutine); override;
    procedure DoAfterStop(const ACoroutine: IISCCoroutine); override;
    procedure DoSetupInternalCoroutine(const ACoroutine: TInternalCoroutine); override;
  public
    property WorkerGroupID : String read GetWorkerGroup;
    function Queue(Const AStart:TCoroutineCallback;
      Const AError:TCoroutineCallback;Const ASuccess:TCoroutineCallback) : IISCCoroutinePool; overload;
    function Queue(Const AStart:TCoroutineNestedCallback;
      Const AError:TCoroutineNestedCallback;Const ASuccess:TCoroutineNestedCallback) : IISCCoroutinePool; overload;
    function Queue(Const AStart:TCoroutineMethod;
      Const AError:TCoroutineMethod;Const ASuccess:TCoroutineMethod) : IISCCoroutinePool; overload;
    function UpdateWorkerCount(const AWorkers : Cardinal) : IISCCoroutinePool;
    constructor Create; override;
    destructor Destroy; override;
  end;

function NewISCCoroutinePool(const AWorkers : Cardinal = 4) : IISCCoroutinePool;

implementation

uses
  ISCCoroutineCollection;

function NewISCCoroutinePool(const AWorkers: Cardinal): IISCCoroutinePool;
begin
  Result := TISCCoroutinePoolImpl.Create;
  Result.UpdateWorkerCount(AWorkers);
end;

procedure TISCCoroutinePoolImpl.TQueueItem.NilWork;
begin
  FCall.Error := nil;
  FCall.Start := nil;
  FCall.Success := nil;

  FMethod.Error := nil;
  FMethod.Start := nil;
  FMethod.Success := nil;

  FNested.Error := nil;
  FNested.Start := nil;
  FNested.Success := nil;
end;

constructor TISCCoroutinePoolImpl.TQueueItem.Create(const ACallback: TCallbackSetup);
begin
  NilWork;
  FCall := ACallback;
end;

constructor TISCCoroutinePoolImpl.TQueueItem.Create(
  const ANested: TNestedCallbackSetup);
begin
  NilWork;
  FNested := ANested;
end;

constructor TISCCoroutinePoolImpl.TQueueItem.Create(const AMethod: TMethodSetup);
begin
  NilWork;
  FMethod := AMethod;
end;

procedure TISCCoroutinePoolImpl.TPoolWorkerCoroutine.Execute;
var
  LWork : TISCCoroutinePoolImpl.TQueueItem;
  LWorker : IISCCoroutine;
  LStillWorking: Boolean;
  I: Integer;
  LArgs: PISCArgs;
  LPool: IISCCoroutinePool;
begin
  try
    //capture a reference to ensure the lifetime of this method
    LPool := FPool as IISCCoroutinePool;

    //run the start methods
    if Assigned(StartCallback) then
      StartCallback(Coroutine);

    if Assigned(StartNestedCallback) then
      StartNestedCallback(Coroutine);

    if Assigned(StartMethod) then
      StartMethod(Coroutine);

    if not Assigned(LPool) then
      Exit;

    //as long as a stop hasn't been requested continue to poll for work
    while not Terminated do
    begin
      //check if we have work
      if FPool.FWork.Count < 1 then
      begin
        Sleep(1);
        Continue;
      end;

      //aquire a lock and work
      FPool.FCritical.Enter;
      try
        //work must've been cleared before we got the lock
        if FPool.FWork.Count < 1 then
          Continue;
        //dequeue some work
        LWork := FPool.FWork.Dequeue;

        //validate we have at least one start method before we pull a worker
        if not (Assigned(LWork.Callback.Start)
          or Assigned(LWork.Nested.Start)
          or Assigned(LWork.Method.Start))
        then
          Continue;

        //get a worker (this blocks until one frees up)
        LWorker := FPool.GetFreeWorker;

        //setup and start the work
        LWorker
          .Events
            .UpdateOnStart(FPool.Events.OnStart)
            .UpdateOnStartNestedCallback(FPool.Events.OnStartNestedCallback)
            .UpdateOnStartCallback(FPool.Events.OnStartCallback)
            .Coroutine
          .Settings
            .UpdateForceTerminate(FPool.Settings.ForceTerminate)
            .Coroutine
          .Setup(LWork.Callback.Start, LWork.Callback.Error, LWork.Callback.Success)
          .Setup(LWork.Nested.Start, LWork.Nested.Error, LWork.Nested.Success)
          .Setup(LWork.Method.Start, LWork.Method.Error, LWork.Method.Success);

       LArgs := PISCArgs(PtrInt(FPool[ARGS]));
       for I := 0 to High(LArgs^) do
        LWorker.AddArg(LArgs^[I].Name, LArgs^[I].Data);

       LWorker.Start;
      finally
        FPool.FCritical.Leave;
      end;
    end;

    //terminate has been requested make sure all workers have a chance
    //to finish
    LStillWorking := False;

    repeat
      for I := 0 to High(FPool.FWorking) do
        if FPool.FWorking[I] then
          LStillWorking := True;
    until not LStillWorking;

  except on E : Exception do
    try
      //run the error methods
      if Assigned(ErrorCallback) then
        ErrorCallback(Coroutine);

      if Assigned(ErrorNestedCallback) then
        ErrorNestedCallback(Coroutine);

      if Assigned(ErrorMethod) then
        ErrorMethod(Coroutine);
    finally
    end;
  end;
end;

function TISCCoroutinePoolImpl.GetWorkerGroup: String;
begin
  Result := FWorkerGroup;
end;

procedure TISCCoroutinePoolImpl.AddPlaceHolder;
var
  LCoroutine : IPlaceHolderCoroutine;
begin
  LCoroutine := TPlaceHolderCoroutineImpl.Create;

  //update the group id to that of the workers
  LCoroutine.Settings.Await.UpdateGroupID(FWorkerGroup);

  FCritical.Enter;
  try
    //add the Coroutine id to the place holder list
    FPlaceHolders.Add(LCoroutine);

    //add the Coroutine to the await collection
    AddCoroutineToAwaitCollection(LCoroutine);
  finally
    FCritical.Leave;
  end;
end;

procedure TISCCoroutinePoolImpl.RemovePlaceHolder;
var
  LPlaceHolder : IPlaceHolderCoroutine;
begin
  FCritical.Enter;
  try
    if FPlaceHolders.Count < 1 then
      Exit;

    LPlaceHolder := FPlaceHolders.Items[0] as IPlaceHolderCoroutine;

    //remove the placeholder from the collection
    RemoveCoroutineFromAwaitCollection(LPlaceHolder);

    //delete from Coroutine list
    FPlaceHolders.Delete(0);
  finally
    FCritical.Leave;
  end;
end;

function TISCCoroutinePoolImpl.GetFreeWorker: IISCCoroutine;
var
  I: Integer;
begin
  //allow dirty reads, just find the first available free worker
  while True do
    for I := 0 to High(FWorking) do
    begin
      if not FWorking[I] then
      begin
        //picked this worker up
        FWorkerCritical.Enter;
        try
          if FWorking[I] then
            Continue;

          //mark as working and fetch the free worker
          FWorking[I] := True;
          Result := FWorkers[I];
        finally
          FWorkerCritical.Leave;
        end;

        //add the worker to the group for await support
        AddCoroutineToAwaitCollection(Result);

        //now that we've taken the place of a placeholder, remove one
        RemovePlaceHolder;

        Exit;
      end;
    end;
end;

function TISCCoroutinePoolImpl.DoGetCoroutineClass: TInternalCoroutineClass;
begin
  Result := TPoolWorkerCoroutine;
end;

procedure TISCCoroutinePoolImpl.DoBeforeStart(const ACoroutine: IISCCoroutine);
begin
  inherited DoBeforeStart(ACoroutine);

  //if a pool was stopped and didn't get released, this will ensure
  //the proper worker count gets updated before starting the pool
  if FWorkerCount <> FWorkers.Count then
    UpdateWorkerCount(FWorkerCount);
end;

procedure TISCCoroutinePoolImpl.DoAfterStop(const ACoroutine: IISCCoroutine);
var
  I: Integer;
  LWorker: IISCCoroutine;
begin
  inherited DoAfterStop(ACoroutine);

  if not Assigned(FWorkers) then
    Exit;

  //don't clear the workers until they're finished
  for I := 0 to Pred(FWorkers.Count) do
  begin
    LWorker := FWorkers[I];
    LWorker.Stop;

    while LWorker.State = esStarted do
      Continue;
  end;

  //clear workers
  FWorkers.Clear;
end;

procedure TISCCoroutinePoolImpl.DoSetupInternalCoroutine(const ACoroutine: TInternalCoroutine);
begin
  inherited DoSetupInternalCoroutine(ACoroutine);
  TPoolWorkerCoroutine(ACoroutine).Pool := Self;
end;

function TISCCoroutinePoolImpl.Queue(const AStart: TCoroutineCallback;
  const AError: TCoroutineCallback; const ASuccess: TCoroutineCallback): IISCCoroutinePool;
var
  LWork : TQueueItem;
  LCall : TCallbackSetup;
begin
  Result := Self;

  AddPlaceHolder;

  //queue up a callback work item
  FCritical.Enter;
  try
    //initialize work
    LCall.Start := AStart;
    LCall.Error := AError;
    LCall.Success := ASuccess;

    //create
    LWork := TQueueItem.Create(LCall);

    //queue the work
    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TISCCoroutinePoolImpl.Queue(const AStart: TCoroutineNestedCallback;
  const AError: TCoroutineNestedCallback; const ASuccess: TCoroutineNestedCallback): IISCCoroutinePool;
var
  LWork : TQueueItem;
  LNested : TNestedCallbackSetup;
begin
  Result := Self;

  AddPlaceHolder;

  //queue up a callback work item
  FCritical.Enter;
  try
    //initialize work
    LNested.Start := AStart;
    LNested.Error := AError;
    LNested.Success := ASuccess;

    //create
    LWork := TQueueItem.Create(LNested);

    //queue the work
    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TISCCoroutinePoolImpl.Queue(const AStart: TCoroutineMethod;
  const AError: TCoroutineMethod; const ASuccess: TCoroutineMethod): IISCCoroutinePool;
var
  LWork : TQueueItem;
  LMethod : TMethodSetup;
begin
  Result := Self;

  AddPlaceHolder;

  //queue up a callback work item
  FCritical.Enter;
  try
    //initialize work
    LMethod.Start := AStart;
    LMethod.Error := AError;
    LMethod.Success := ASuccess;

    //create
    LWork := TQueueItem.Create(LMethod);

    //queue the work
    FWork.Enqueue(LWork);
  finally
    FCritical.Leave;
  end;
end;

function TISCCoroutinePoolImpl.UpdateWorkerCount(const AWorkers: Cardinal): IISCCoroutinePool;
var
  LStarted : Boolean;
  LCount : Cardinal;
  I: Integer;
  LCoroutine : IISCCoroutine;

const
  INTERNAL_INDEX = 'internal_lock_index';
  POOL = 'pool';
  (*
    when a worker stops, write to the working array
  *)
  procedure WorkerStop(const ACoroutine : IISCCoroutine);
  var
    I : Integer;
    LCoroutine : IISCCoroutine;
    LSelf: TISCCoroutinePoolImpl;
  begin
    //local ref
    LCoroutine := ACoroutine;

    //self pointer won't work here, so cast the pool arg
    LSelf := TISCCoroutinePoolImpl(Pointer(PtrInt(LCoroutine[POOL])));

    I := LCoroutine[INTERNAL_INDEX];
    LSelf.FWorking[I] := False;
  end;

begin
  Result := Self;
  FWorkerCount := AWorkers;

  //can't have zero workers... I mean you could, that would just be stupid though
  if FWorkerCount < 1 then
    FWorkerCount := 1;

  //await current jobs
  ISCCoroutine.Await(FWorkerGroup);

  //record prior state to see if we need to start back up
  LStarted := State = esStarted;

  //stop monitoring for queued jobs
  if LStarted then
    Stop;

  //set the working array to the worker count and fill as false
  SetLength(FWorking, FWorkerCount);

  //these should be defaulted to false, but just to make sure
  for I := 0 to Pred(FWorkerCount) do
    FWorking[I] := False;

  //clear workers
  FWorkers.Clear;

  //populate workers
  for I := 0 to Pred(FWorkerCount) do
  begin
    //add a new Coroutine, making sure to copy the pool's settings that apply
    LCoroutine := NewISCCoroutine;
    LCoroutine
      .Settings
        .UpdateSynchronizeStopEvents(Settings.SynchronizeStopEvents)
        .UpdateMaxRuntime(Settings.MaxRuntime)
        .Await
          .UpdateGroupID(FWorkerGroup)
          .Coroutine
        //update the index of the worker and set the stop callback
        .AddArg(INTERNAL_INDEX, I)
        .AddArg(POOL, PtrInt(Pointer(Self)))
        .Events
          .UpdateOnStopNestedCallback(WorkerStop).Coroutine;

    FWorkers.Add(LCoroutine);
    LCoroutine := nil;//nil local ref since we re-use the variable
  end;

  //start back up if we were already started
  if LStarted then
    Start;
end;

constructor TISCCoroutinePoolImpl.Create;
begin
  inherited Create;
  FPlaceHolders := TISCCoroutines.Create;
  FWork := TMethodQueue.Create;
  FCritical := TCriticalSection.Create;
  FWorkerCritical := TCriticalSection.Create;
  FWorkers := TISCCoroutines.Create;
  FWorkerGroup := TGuid.NewGuid.ToString;
  AddArg(ARGS, PtrInt(GetArgs));
end;

destructor TISCCoroutinePoolImpl.Destroy;
begin
  Stop;
  FreeAndNil(FWork);
  FreeAndNil(FCritical);
  FreeAndNil(FWorkerCritical);
  FreeAndNil(FWorkers);
  FreeAndNil(FPlaceHolders);
  inherited Destroy;
end;

end.

