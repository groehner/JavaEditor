unit UTabObject;

interface

uses UBaseForm;

type
  TTabObject = class
    Path: string;
    Num: Integer;
    Form: TFForm;
    constructor Create(const Path: string; Num: Integer; Form: TFForm);
  end;

implementation

constructor TTabObject.Create(const Path: string; Num: Integer; Form: TFForm);
begin
  Self.Path := Path;
  Self.Num:= Num;
  Self.Form:= Form;
end;

end.
