unit testyaml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ISCYaml;

procedure doTestYaml();

implementation

const
  SAMPLE_YAML =
    'sample:'#10 +
    '  text:'#10+
    '  - apple'#10+
    '  - orange'#10+
    '  - banana'#10+
    '  host: 0.0.0.0'#10+
    '  port: 8080'#10;

procedure doTestYaml();
var
  y: TYamlFile;
  sa: TStringArray;
  i: Integer;
begin
  y := TYamlFile.Create();
  y.LoadString(SAMPLE_YAML);
  sa := y.GetArrayValue('sample.text');
  if (sa <> nil) then begin
    for i:= 0 to Length(sa) - 1 do WriteLn(sa[i]);
  end;
  y.Free;
end;

end.

