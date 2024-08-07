unit UFXToolBar;

interface

uses
  Classes, UJEComponents, UFXComponents;

type

  TFXToolBar = class (TFXControl)
  private
    FOrientation: TOrientation;
    procedure setOrientation(aValue: TOrientation);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
  published
    property Orientation: TOrientation read FOrientation write SetOrientation;
  end;

implementation

uses Windows, Controls;

{--- TFXToolBar --------------------------------------------------------------}

constructor TFXToolBar.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +134;
  PrefWidth:= 200;
  PrefHeight:= 24;
  ControlStyle := [csAcceptsControls];
  JavaType:= 'ToolBar';
end;

procedure TFXToolBar.Paint;
  var aColor, i: integer;
begin
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  aColor:= 181;
  if Orientation = HORIZONTAL then
    for i:= Height-1 downto Height-20 do begin
      Canvas.Pen.Color:= RGB(aColor, aColor, aColor);
      Canvas.MoveTo(0, i);
      Canvas.LineTo(Width, i);
      if aColor = 183 then
        aColor:= 210;
      aColor:= aColor + 2;
    end
  else
    for i:= 0 to 20 do begin
      Canvas.Pen.Color:= RGB(aColor, aColor, aColor);
      Canvas.MoveTo(i, 0);
      Canvas.LineTo(i, Height);
      if aColor = 183 then
        aColor:= 210;
      aColor:= aColor + 2;
    end
end;

procedure TFXToolBar.SetOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then  begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
    end;
    Invalidate;
  end;
end;

procedure TFXToolBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ToolBar '  + Name + ' = new ToolBar();');
  InsertImport('javafx.geometry.*');
end;

function TFXToolBar.getAttributes(ShowAttributes: integer): string;
begin
  Result:=  '|Orientation' + inherited getAttributes(ShowAttributes);
end;

end.
