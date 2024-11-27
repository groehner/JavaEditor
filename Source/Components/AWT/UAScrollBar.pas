unit UAScrollBar;

interface

uses
  Classes, StdCtrls, UJEComponents, UAComponents;

type

  TAScrollBar = class (TAWTComponent)
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
    procedure setAttribute(Attr, Value, Typ: string); override;
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

uses
  Windows, SysUtils, Forms, Types, Graphics, Controls, UGUIDesigner;

{--- TAScrollBar --------------------------------------------------------------}

constructor TAScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= -10;
  Width:= 120;
  Height:= 24;
  BlockIncrement:= 10;
  UnitIncrement:= 1;
  Value:= 0;
  VisibleAmount:= 10;
  Minimum:= 0;
  Maximum:= 100;
  Foreground:= DarkShadow;
  Background:= RGB(200, 200, 200); // DefaultBackground
  Orientation:= HORIZONTAL;
  ShowFont:= false;
  JavaType:= 'Scrollbar';
end;

constructor TAScrollBar.CreateFrom(aScrollBar: TScrollBar);
begin
  Create(aScrollBar.Owner);
  CreateFromA(aScrollBar);
  Maximum:= aScrollBar.Max;
  Minimum:= aScrollbar.Min;
  Value:= aScrollBar.Position;
  if aScrollBar.Kind = sbVertical
    then Orientation:= VERTICAL
    else Orientation:= HORIZONTAL;
end;

function TAScrollBar.getAttributes(ShowAttributes: integer): string;
  const
    scroll1 = '|Value|Minimum|Maximum|Orientation';
    scroll2 = '|BlockIncrement|UnitIncrement|VisibleAmount';
begin
  if ShowAttributes = 1
    then Result:= scroll1
    else Result:= scroll1 + scroll2;
  Result:= Result + inherited;
end;

procedure TAScrollBar.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Orientation' then begin
    MakeAttribut(Attr, 'Scrollbar.' + Value);
    setPositionAndSize;
  end else
    inherited;
end;

function TAScrollBar.getEvents(ShowEvents: integer): string;
begin
  Result:= '|adjustmentValueChanged' + inherited getEvents(ShowEvents);
end;

procedure TAScrollBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Scrollbar ' + Name + ' = new Scrollbar();');
  MakeAttribut('Orientation', 'Scrollbar.HORIZONTAL');
end;

procedure TAScrollBar.Paint;
  var x, y, x1, x2, dx, y1, y2, dy: integer;
begin
  // left-right
  CanvasFontAssign;
  Canvas.Brush.Color:= RGB(234,234,234);
  Canvas.FillRect(Rect(0, 0, Width, Height));
  var p16:= PPIScale(16);
  if Orientation = HORIZONTAL then begin
    Canvas.Brush.Color:= Background;
    Canvas.FillRect(Rect(p16, 0, Width-p16, Height));
    FGUIDesigner.vilControls1616.Draw(Canvas, 0, (Height - p16) div 2, 3);
    FGUIDesigner.vilControls1616.Draw(Canvas, Width - p16, (Height - p16) div 2, 2);
    // scroller
    dx:= round(VisibleAmount*1.68/2);
    x1:= p16 + dx;
    x2:= Width - p16 - dx;
    x:= x1 + round((x2 - x1)/((FMaximum - FMinimum)*1.0) * FValue);
    Canvas.Brush.Color:= RGB(240,240,240);
    Canvas.FrameRect(Rect(x-dx, 0, x+dx, Height));
    Canvas.Pen.Color:= RGB(148,148,148);
    Canvas.Brush.Color:= RGB(200,200,200);
    Canvas.Rectangle(Rect(x-dx+1, 1, x+dx-1, Height-1));
  end else begin
    Canvas.Brush.Color:= Background;
    Canvas.FillRect(Rect(0, p16, Width, Height-p16));
    FGUIDesigner.vilControls1616.Draw(Canvas, (Width - p16) div 2, 0, 4);
    FGUIDesigner.vilControls1616.Draw(Canvas, (Width - p16) div 2, Height - p16, 5);
    // scroller
    dy:= round(VisibleAmount*1.68/2);
    y1:= p16 + dy;
    y2:= Height - p16 - dy;
    y:= y1 + round((y2 - y1)/((FMaximum - FMinimum)*1.0) * FValue);
    Canvas.Brush.Color:= RGB(240, 240, 240);
    Canvas.FrameRect(Rect(0, y-dy, Width, y+dy));
    Canvas.Pen.Color:= RGB(148,148,148);
    Canvas.Brush.Color:= RGB(200,200,200);
    Canvas.Rectangle(Rect(1, y-dy+1, Width-1, y+dy-1));
  end;
end;

procedure TAScrollBar.setValue(aValue: integer);
begin
  if aValue <> FValue then begin
    fValue:= aValue;
    Invalidate;
  end;
end;

procedure TAScrollBar.setOrientation(aValue: TOrientation);
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

procedure TAScrollBar.setVisibleAmount(aValue: integer);
begin
  if aValue <> FVisibleAmount then begin
    FVisibleAmount:= aValue;
    Invalidate;
  end;
end;

end.
