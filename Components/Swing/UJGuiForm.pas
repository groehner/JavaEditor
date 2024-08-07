unit UJGuiForm;

interface

uses
  Classes, UJComponents;

type

  TJGuiForm = class (TSwingComponent)
  public
    constructor Create(AOwner: TComponent); override;
    procedure DeleteListener(const event: string); override;
    function MakeEventProcedureName(const event: string): string; override;
    procedure AddListener(const event: string); override;
  end;

implementation

uses UObjectInspector, UUtils;

constructor TJGuiForm.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= 0;
end;

procedure TJGuiForm.DeleteListener(const event: string);
  var EventMethod, Listener: string;
begin
  EventMethod:= MakeEventProcedure(Event);
  Partner.DeleteEventMethod(EventMethod);
  Listener:= getListener(Event);
  Partner.DeleteListener(Listener);
end;

function TJGuiForm.MakeEventProcedureName(const event: string): string;
begin
  if Pos(event, WindowEvents) > 0
    then Result:= UpperLower(event)
    else Result:= Name + '_' + UpperLower(event);
end;

procedure TJGuiForm.AddListener(const event: string);
  var EventProcedurename: string;
begin
  var Listener:= getListener(Event);
  Partner.InsertListener(GetContainerAdd, Listener);
  EventProcedureName:= MakeEventProcedureName(Event);
  if not Partner.hasText('public void ' + EventProcedureName) then
    Partner.InsertProcedure(0, MakeEventProcedure(Event));

  TThread.ForceQueue(nil, procedure
    begin
      FObjectInspector.ELEventInspector.SetByCaption(event, EventProcedureName);
    end);
end;

end.
