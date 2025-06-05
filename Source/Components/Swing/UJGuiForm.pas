unit UJGuiForm;

interface

uses
  Classes,
  UJComponents;

type

  TJGuiForm = class(TSwingComponent)
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeleteListener(const Event: string); override;
    function MakeEventProcedureName(const Event: string): string; override;
    procedure AddListener(const Event: string); override;
  end;

implementation

uses
  UObjectInspector,
  UUtils;

constructor TJGuiForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 0;
end;

procedure TJGuiForm.DeleteListener(const Event: string);
var
  EventMethod, Listener: string;
begin
  EventMethod := MakeEventProcedure(Event);
  FPartner.DeleteEventMethod(EventMethod);
  Listener := GetListener(Event);
  FPartner.DeleteListener(Listener);
end;

function TJGuiForm.MakeEventProcedureName(const Event: string): string;
begin
  if Pos(Event, WindowEvents) > 0 then
    Result := UpperLower(Event)
  else
    Result := Name + '_' + UpperLower(Event);
end;

procedure TJGuiForm.AddListener(const Event: string);
var
  EventProcedurename: string;
begin
  var
  Listener := GetListener(Event);
  FPartner.InsertListener(GetContainerAdd, Listener);
  EventProcedurename := MakeEventProcedureName(Event);
  if not FPartner.HasText('public void ' + EventProcedurename) then
    FPartner.InsertProcedure(0, MakeEventProcedure(Event));

  TThread.ForceQueue(nil,
    procedure
    begin
      FObjectInspector.ELEventInspector.SetByCaption(Event, EventProcedurename);
    end);
end;

end.
