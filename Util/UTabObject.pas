unit UTabObject;

interface

uses UBaseForm;

type TTabObject = class
       path: string;
       Nr: integer;
       Form: TFForm;
       constructor create(const aPath: string; aNr: integer; aForm: TFForm);
     end;

implementation

constructor TTabObject.create(const aPath: string; aNr: integer; aForm: TFForm);
begin
  self.path := aPath;
  self.Nr:= aNr;
  self.Form:= aForm;
end;

end.
