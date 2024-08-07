unit UJScrollBar;

interface

uses
  Classes, StdCtrls, Types, UJEComponents, UJComponents;

type

  TJScrollBar = class (TSwingComponent)
  private
    FBlockIncrement: integer;
    FMaximum: integer;
    FMinimum: integer;
    FUnitIncrement: integer;
    FVisibleAmount: integer;
    FValue: integer;
    FOrientation: TOrientation;
    procedure setOrientation(aValue: TOrientation);
    procedure setValue(aValue: integer);
    procedure setVisibleAmount(aValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aScrollBar: TScrollBar);
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property BlockIncrement: integer read FBlockIncrement write FBlockIncrement default 10;
    property Maximum: integer read FMaximum write FMaximum default 100;
    property Minimum: integer read FMinimum write FMinimum default 0;
    property Value: integer read FValue write setValue;
    property UnitIncrement: integer read FUnitIncrement write FUnitIncrement;
    property VisibleAmount: integer read FVisibleAmount write setVisibleAmount;
    property Orientation: TOrientation read FOrientation write setOrientation;
    property adjustmentValueChanged;
  end;

implementation

uses Graphics, Controls, Forms;

{--- TJScrollBar --------------------------------------------------------------}

constructor TJScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +10;
  Width:= 120;
  Height:= 24;
  BlockIncrement:= 10;
  UnitIncrement:= 1;
  Value:= 0;
  VisibleAmount:= 10;
  Minimum:= 0;
  Maximum:= 100;
  Orientation:= Horizontal;
  ShowFont:= false;
  JavaType:= 'JScrollBar';
end;

constructor TJScrollBar.CreateFrom(aScrollBar: TScrollBar);
begin
  Create(aScrollBar.Owner);
  CreateFromJ(aScrollBar);
  Maximum:= aScrollBar.Max;
  Minimum:= aScrollbar.Min;
  Value:= aScrollBar.Position;
  if aScrollBar.Kind = sbVertical
    then Orientation:= VERTICAL
    else Orientation:= HORIZONTAL;
end;

function TJScrollBar.getAttributes(ShowAttributes: integer): string;
  const
    scroll1 = '|Value|Minimum|Maximum|Orientation';
    scroll2 = '|BlockIncrement|UnitIncrement|VisibleAmount';
begin
  if ShowAttributes = 1
    then Result:= scroll1
    else Result:= scroll1 + scroll2;
  Result:= Result + inherited;
end;

function TJScrollBar.getEvents(ShowEvents: integer): string;
begin
  Result:= '|adjustmentValueChanged' + inherited getEvents(ShowEvents);
end;

procedure TJScrollBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JScrollBar ' + Name + ' = new JScrollBar();');
  MakeAttribut('Orientation', 'Scrollbar.HORIZONTAL');
end;

procedure TJScrollBar.Paint;
  var x, y, x1, x2, dx, y1, y2, dy: integer;
      Points: array[0..2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= Foreground;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  var p16:= PPIScale(16);
  var p5:= PPIScale(5);
  var p2:= PPIScale(2);
  if Orientation = HORIZONTAL then begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, p16, Height));
    Canvas.Rectangle(Rect(Width-p16, 0, Width, Height));
    Canvas.Pen.Color:= DefaultForeground;
    Canvas.Brush.Color:= DefaultForeground;
    x:= p5-1;
    y:= Height div 2;

    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x + p5, y - p5);
    Points[2]:= Point(x + p5, y + p5);
    Canvas.Polygon(Points); 
    x:= Width - p5 -1;
    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x - p5, y + p5);
    Points[2]:= Point(x - p5, y - p5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color:= BlueColor;
    Canvas.Brush.Color:= DefaultBackground;
    dx:= round(VisibleAmount*1.68/2);
    x1:= p16 + dx;
    x2:= Width - p16 - dx;
    x:= x1 + round((x2 - x1)/((FMaximum - FMinimum)*1.0) * FValue);
    Canvas.Rectangle(Rect(x-dx, 0, x+dx, Height));

    Canvas.MoveTo(x-p2, p5); Canvas.LineTo(x-p2, Height-p5);
    Canvas.MoveTo(x  , p5); Canvas.LineTo(x  , Height-p5);
    Canvas.MoveTo(x+p2, p5); Canvas.LineTo(x+p2, Height-p5);
  end else begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, Width, p16));
    Canvas.Rectangle(Rect(0, Height-p16, Width, Height));
    Canvas.Pen.Color:= DefaultForeground;
    Canvas.Brush.Color:= DefaultForeground;
    x:= Width div 2;
    y:= p5-1;
    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x - p5, y + p5);
    Points[2]:= Point(x + p5, y + p5);
    Canvas.Polygon(Points);
    y:= Height - 4;
    Points[0]:= Point(x + 0, y + 0);
    Points[1]:= Point(x - p5, y - p5);
    Points[2]:= Point(x + p5, y - p5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color:= BlueColor;
    Canvas.Brush.Color:= DefaultBackground;
    dy:= round(VisibleAmount*1.68/2);
    y1:= p16 + dy;
    y2:= Height - p16 - dy;
    y:= y1 + round((y2 - y1)/((FMaximum - FMinimum)*1.0) * FValue);
    Canvas.Rectangle(Rect(0, y - dy, Width, y + dy));
    Canvas.MoveTo(p5, y-p2); Canvas.LineTo(Width-p5, y-p2);
    Canvas.MoveTo(p5, y  ); Canvas.LineTo(Width-p5, y  );
    Canvas.MoveTo(p5, y+p2); Canvas.LineTo(Width-p5, y+p2);
  end;
end;

procedure TJScrollBar.setValue(aValue: integer);
begin
  if aValue <> FValue then begin
    fValue:= aValue;
    Invalidate;
  end;
end;

procedure TJScrollBar.setOrientation(aValue: TOrientation);
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

procedure TJScrollBar.setVisibleAmount(aValue: integer);
begin
  if aValue <> FVisibleAmount then begin
    FVisibleAmount:= aValue;
    Invalidate;
  end;
end;

end.
