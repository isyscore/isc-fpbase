program client;

{$mode objfpc}{$H+}

uses
  cthreads, cmem, Classes, sysutils, ssockets,
  ISCWSMessages, ISCWSUtils, ISCWSStream, ISCWebSocketClient;


type

  { TClientHandler }

  TClientHandler = class
  private
    FCommunicator: TWebsocketCommunicator;
    procedure ReceiveMessage(Sender: TObject);
    procedure StreamClosed(Sender: TObject);
  public
    procedure Execute;
    constructor Create(ACommunicator: TWebsocketCommunicator);
    destructor Destroy; override;
  end;

{ TClientHandler }

procedure TClientHandler.ReceiveMessage(Sender: TObject);
var
  MsgList: TWebsocketMessageOwnerList;
  m: TWebsocketMessage;
begin
  MsgList := TWebsocketMessageOwnerList.Create(True);
  try
    FCommunicator.GetUnprocessedMessages(MsgList);
    for m in MsgList do begin
      if m is TWebsocketStringMessage then begin
        WriteLn('Message from ', FCommunicator.SocketStream.RemoteAddress.Address, ': ', TWebsocketStringMessage(m).Data)
      end else if m is TWebsocketPongMessage then begin
        WriteLn('Pong from ', FCommunicator.SocketStream.RemoteAddress.Address, ': ', TWebsocketPongMessage(m).Data);
      end;
    end;
  finally
    MsgList.Free;
  end;
end;

procedure TClientHandler.StreamClosed(Sender: TObject);
begin
  WriteLn('Connection to ', FCommunicator.SocketStream.RemoteAddress.Address, ' closed');
end;

procedure TClientHandler.Execute;
var
  str: String;
begin
  while FCommunicator.Open do begin
    ReadLn(str);
    if not FCommunicator.Open then begin
      Exit;
    end;
    if str = 'exit' then begin
      FCommunicator.WriteMessage(wmtClose).Free;
      while FCommunicator.Open do begin
        Sleep(100);
      end;
    end else if str.StartsWith('ping') then begin
      with FCommunicator.WriteMessage(wmtPing) do begin
        try
          WriteRaw(str.Substring(5));
        finally
          Free;
        end;
      end
    end else begin
      FCommunicator.WriteStringMessage(str);
    end;
  end;
end;

constructor TClientHandler.Create(ACommunicator: TWebsocketCommunicator);
begin
  FCommunicator := ACommunicator;
  FCommunicator.OnClose:=@StreamClosed;
  FCommunicator.OnReceiveMessage:=@ReceiveMessage;
  FCommunicator.StartReceiveMessageThread;
end;

destructor TClientHandler.Destroy;
begin
  FCommunicator.StopReceiveMessageThread;
  while FCommunicator.ReceiveMessageThreadRunning do begin
    Sleep(10);
  end;
  FCommunicator.Free;
  inherited Destroy;
end;


var
  cli: TWebsocketClient;
  handler: TClientHandler;
begin
  cli := TWebsocketClient.Create('127.0.0.1', 8080);
  try
    handler := TClientHandler.Create(cli.Connect(TSocketHandler.Create));
    try
      handler.Execute;
    finally
      handler.Free;
    end;
  finally
    cli.Free;
  end;
end.

