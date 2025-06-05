unit UTabObject;

interface

uses UBaseForm;

type TTabObject = class
       path: string;
       Num: Integer;
       Form: TFForm;
       constructor Create(const APath: string; ANum: Integer; AForm: TFForm);
     end;

implementation

constructor TTabObject.Create(const APath: string; ANum: Integer; AForm: TFForm);
begin
  Self.path := APath;
  Self.Num:= ANum;
  Self.Form:= AForm;
end;

end.
