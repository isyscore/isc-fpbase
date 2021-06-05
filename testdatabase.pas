unit testdatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset;

procedure doTestDatabase();

implementation

procedure doTestDatabase();
var
  conn: TZConnection;
  query: TZQuery;
begin
  conn := TZConnection.Create(nil);
  conn.Protocol:= 'mysql';
  conn.HostName:= '127.0.0.1';
  conn.Port:= 3306;
  conn.User:= 'root';
  conn.Password:= 'root';
  conn.Database:= 'YugiohAPI';
  conn.ClientCodepage:= 'utf8mb4';
  conn.Connect;

  query := TZQuery.Create(nil);
  query.Connection := conn;

  query.SQL.Text:= 'select * from YGOCardName limit 0, 10';
  query.Open;

  while not query.EOF do begin
    WriteLn(query.FieldByName('kanji').AsString);
    query.Next;
  end;
  query.Close;

  query.Free;

  conn.Free;
end;

end.

