program server;

{$mode objfpc}{$H+}

uses
  cthreads, cmem, Classes, sysutils,
  ISCWSMessages, ISCWSUtils, ISCWSStream, ISCWebSocketServer;

var
  socket: TWebSocketServer;

type
  { TSocketHandler }

  TSocketHandler = class(TThreadedWebsocketHandler)
  private
    procedure ConnectionClosed(Sender: TObject);
    procedure MessageReceived(Sender: TObject);
  public
    function Accept(const ARequest: TRequestData; const ResponseHeaders: TStrings): boolean; override;
    procedure DoHandleCommunication(ACommunication: TWebsocketCommunicator); override;
  end;

{ TSocketHandler }

procedure TSocketHandler.ConnectionClosed(Sender: TObject);
var
  Comm: TWebsocketCommunicator;
begin
  Comm := TWebsocketCommunicator(Sender);
  WriteLn('Connection to ', Comm.SocketStream.RemoteAddress.Address, ' closed');
end;

procedure TSocketHandler.MessageReceived(Sender: TObject);
var
  Messages: TWebsocketMessageOwnerList;
  m: TWebsocketMessage;
  Comm: TWebsocketCommunicator;
begin
  Comm := TWebsocketCommunicator(Sender);
  Messages := TWebsocketMessageOwnerList.Create(True);
  try
    Comm.GetUnprocessedMessages(Messages);
    for m in Messages do begin
      if m is TWebsocketStringMessage then begin
        WriteLn('Message from ', Comm.SocketStream.RemoteAddress.Address, ': ', TWebsocketStringMessage(m).Data);
      end;
    end;
  finally
    Messages.Free;
  end;
end;

function TSocketHandler.Accept(const ARequest: TRequestData; const ResponseHeaders: TStrings): boolean;
begin
  Exit(True);
end;

procedure TSocketHandler.DoHandleCommunication(ACommunication: TWebsocketCommunicator);
var
  str: string;
begin
  WriteLn('Connected to ', ACommunication.SocketStream.RemoteAddress.Address);
  ACommunication.OnReceiveMessage := @MessageReceived;
  ACommunication.OnClose := @ConnectionClosed;
  while ACommunication.Open do begin
    ReadLn(str);
    if not ACommunication.Open then begin
      // could be closed by the time ReadLn takes
      Break;
    end;
    ACommunication.WriteStringMessage(str);
    WriteLn('Message to ', ACommunication.SocketStream.RemoteAddress.Address, ': ', str);
  end;
  socket.Stop(True);
end;

begin
  socket := TWebSocketServer.Create(8080);
  try
    socket.FreeHandlers := True;
    socket.AcceptingMethod:=samThreadPool;
    socket.RegisterHandler('*', '*', TSocketHandler.Create, True, True);
    socket.Start;
  finally
    socket.Free;
  end;
end.

