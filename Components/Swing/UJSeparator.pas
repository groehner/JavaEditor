unit UJSeparator;

interface

uses
  Classes, UJEComponents, UJComponents;

type

  TJSeparator = class (TSwingComponent)
  private
    FOrientation: TOrientation;
    procedure setOrientation(aValue: TOrientation);
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Orientation: TOrientation read FOrientation write setOrientation;
  end;

implementation

uses Graphics, Controls;

{--- TJSeparator --------------------------------------------------------------}

constructor TJSeparator.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +24;
  Width:= 80;
  Height:= 8;
  Background:= clWhite;
  Foreground:= BlueColor;
  JavaType:= 'JSeparator';
end;

function TJSeparator.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Orientation' + inherited;
end;

procedure TJSeparator.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JSeparator ' + Name + ' = new JSeparator();');
end;

procedure TJSeparator.Paint;
begin
  if Orientation = HORIZONTAL then begin
    Canvas.Pen.Color:= Foreground;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width-1, 0);
    Canvas.Pen.Color:= Background;
    Canvas.MoveTo(0, 1);
    Canvas.LineTo(Width-1, 1);
  end else begin
    Canvas.Pen.Color:= Foreground;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, Height-1);
    Canvas.Pen.Color:= Background;
    Canvas.MoveTo(1, 0);
    Canvas.LineTo(1, Height-1);
  end;
end;

procedure TJSeparator.setOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
    end;
    Invalidate;
  end;
end;

end.
