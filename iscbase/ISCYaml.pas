unit ISCYaml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TYamlFile }

  TYamlFile = class
  private
    FList: TStringList;
  public
    constructor Create();
    destructor Destroy; override;
    procedure LoadFromFile(AFilePath: string);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadString(AString: string);
    function GetValue(APath: string; ADefault: string = ''): string;
  end;

implementation

{ TYamlFile }

constructor TYamlFile.Create();
begin
  FList := TStringList.Create;
end;

destructor TYamlFile.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TYamlFile.LoadFromFile(AFilePath: string);
begin
  FList.LoadFromFile(AFilePath);
end;

procedure TYamlFile.LoadFromStream(AStream: TStream);
begin
  FList.LoadFromStream(AStream);
end;

procedure TYamlFile.LoadString(AString: string);
begin
  FList.Text:= AString;
end;

function TYamlFile.GetValue(APath: string; ADefault: string): string;
var
  i: Integer;
  ret: string = '';
  blocks: array of string;
  lv: Integer = 0;
  spcs: string;
begin
  blocks := APath.Split(['.']);
  for i := 0 to FList.Count - 1 do begin
    spcs:= ''.PadRight(lv * 2);
    if (FList[i].StartsWith(spcs + blocks[lv] + ':')) then begin
      // hit key
      if (lv = Length(blocks) - 1) then begin
        ret := FList[i].Substring(FList[i].IndexOf(':') + 1).Trim;
        Exit(ret);
      end;
      Inc(lv);
    end;
  end;
  ret := ADefault;
  Exit(ret);
end;

end.

