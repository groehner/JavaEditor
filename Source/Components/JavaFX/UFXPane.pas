unit UFXPane;

interface

uses Classes, UFXComponents;

type

  TFXPane = class (TFXRegion)
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  end;

implementation

uses Controls, Graphics;

{--- TFXPane ------------------------------------------------------------------}

constructor TFXPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls];
  Tag:= 121;
  Background:= clBtnFace;
  SendToBack;
  JavaType:= 'Pane';
end;

function TFXPane.getAttributes(ShowAttributes: integer): string;
begin
  if ShowAttributes = 1
    then Result:= '|Background' + inherited
    else Result:= '|Background|PrefHeight|PrefWidth' + inherited;
end;

procedure TFXPane.SetAttribute(Attr, Value, Typ: string);
begin
  inherited;
  invalidate;
end;

procedure TFXPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Pane '  + Name + ' = new Pane();');
end;

procedure TFXPane.Paint;
begin
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(0, 0, Width, Height);
end;

end.
