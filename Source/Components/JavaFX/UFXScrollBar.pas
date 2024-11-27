unit UFXScrollBar;   // Used?

interface

uses
  Classes, UJEComponents, UFXComponents;

type

  TFXScrollBar = class (TFXControl)
  private
    FBlockIncrement: integer;
    FMax: integer;
    FMin: integer;
    FOrientation: TOrientation;
    FUnitIncrement: integer;
    FValue: integer;
    FVisibleAmount: integer;
    procedure setOrientation(aValue: TOrientation);
    procedure setValue(aValue: integer);
    procedure setVisibleAmount(aValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
  published
    property BlockIncrement: integer read FBlockIncrement write FBlockIncrement default 10;
    property Max: integer read FMax write FMax default 100;
    property Min: integer read FMin write FMin default 0;
    property Value: integer read FValue write setValue;
    property UnitIncrement: integer read FUnitIncrement write FUnitIncrement;
    property VisibleAmount: integer read FVisibleAmount write setVisibleAmount;
    property Orientation: TOrientation read FOrientation write setOrientation;
  end;

implementation

uses  Types;

{--- TFXScrollBar -------------------------------------------------------------}

constructor TFXScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +112;
  PrefWidth:= 200;
  PrefHeight:= 20;
  BlockIncrement:= 10;
  UnitIncrement:= 1;
  Value:= 0;
  VisibleAmount:= 10;
  Min:= 0;
  Max:= 100;
  JavaType:= 'ScrollBar';
end;

procedure TFXScrollBar.Paint;
  var x, y, x1, x2, dx, y1, y2, dy: integer;
      Points: array[0..2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Font.Style:= [];
  Canvas.Pen.Color:= Foreground;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  if Orientation = HORIZONTAL then begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, 16, Height));
    Canvas.Rectangle(Rect(Width-16, 0, Width, Height));
    Canvas.Pen.Color:= DefaultForeground;
    Canvas.Brush.Color:= DefaultForeground;
    x:= 4;
    y:= Height div 2;

    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x + 5, y - 5);
    Points[2]:= Point(x + 5, y + 5);
    Canvas.Polygon(Points); 
    x:= Width - 4;
    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x - 5, y + 5);
    Points[2]:= Point(x - 5, y - 5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color:= BlueColor;
    Canvas.Brush.Color:= DefaultBackground;
    dx:= round(VisibleAmount*1.68/2);
    x1:= 16 + dx;
    x2:= Width - 16 - dx;
    x:= x1 + round((x2 - x1)/((FMax - FMin)*1.0) * FValue);
    Canvas.Rectangle(Rect(x-dx, 0, x+dx, Height));

    Canvas.MoveTo(x-2, 5); Canvas.LineTo(x-2, Height-5);
    Canvas.MoveTo(x  , 5); Canvas.LineTo(x  , Height-5);
    Canvas.MoveTo(x+2, 5); Canvas.LineTo(x+2, Height-5);
  end else begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, Width, 16));
    Canvas.Rectangle(Rect(0, Height-16, Width, Height));
    Canvas.Pen.Color:= DefaultForeground;
    Canvas.Brush.Color:= DefaultForeground;
    x:= Width div 2;
    y:= 4;
    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x - 5, y + 5);
    Points[2]:= Point(x + 5, y + 5);
    Canvas.Polygon(Points);
    y:= Height - 4;
    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x - 5, y - 5);
    Points[2]:= Point(x + 5, y - 5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color:= BlueColor;
    Canvas.Brush.Color:= DefaultBackground;
    dy:= round(VisibleAmount*1.68/2);
    y1:= 16 + dy;
    y2:= Height - 16 - dy;
    y:= y1 + round((y2 - y1)/((FMax - FMin)*1.0) * FValue);
    Canvas.Rectangle(Rect(0, y - dy, Width, y + dy));
    Canvas.MoveTo(5, y-2); Canvas.LineTo(Width-5, y-2);
    Canvas.MoveTo(5, y  ); Canvas.LineTo(Width-5, y  );
    Canvas.MoveTo(5, y+2); Canvas.LineTo(Width-5, y+2);
  end;
end;

procedure TFXScrollBar.setValue(aValue: integer);
begin
  if aValue <> FValue then begin
    fValue:= aValue;
    Invalidate;
  end;
end;

procedure TFXScrollBar.setOrientation(aValue: TOrientation);
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

procedure TFXScrollBar.setVisibleAmount(aValue: integer);
begin
  if aValue <> FVisibleAmount then begin
    FVisibleAmount:= aValue;
    Invalidate;
  end;
end;

procedure TFXScrollBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ScrollBar '  + Name + ' = new ScrollBar();');
end;

function TFXScrollBar.getAttributes(ShowAttributes: integer): string;
  const ScrollBarAttributes1 = '|Max|Min|Orientation|Value';
        ScrollBarAttributes2 = ScrollBarAttributes1 + '|BlockIncrement|UnitIncrement|VisibleAmount';
begin
  if ShowAttributes = 1
    then Result:= ScrollBarAttributes1 + inherited getAttributes(3)
    else Result:= ScrollBarAttributes2 + inherited getAttributes(3)
end;

end.
