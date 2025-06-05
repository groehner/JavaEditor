unit UFXScrollBar; // Used?

interface

uses
  Classes,
  UJEComponents,
  UFXComponents;

type

  TFXScrollBar = class(TFXControl)
  private
    FBlockIncrement: Integer;
    FMax: Integer;
    FMin: Integer;
    FOrientation: TOrientation;
    FUnitIncrement: Integer;
    FValue: Integer;
    FVisibleAmount: Integer;
    procedure SetOrientation(AValue: TOrientation);
    procedure SetValue(AValue: Integer);
    procedure SetVisibleAmount(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
  published
    property BlockIncrement: Integer read FBlockIncrement write FBlockIncrement
      default 10;
    property Max: Integer read FMax write FMax default 100;
    property Min: Integer read FMin write FMin default 0;
    property Value: Integer read FValue write SetValue;
    property UnitIncrement: Integer read FUnitIncrement write FUnitIncrement;
    property VisibleAmount: Integer read FVisibleAmount write SetVisibleAmount;
    property Orientation: TOrientation read FOrientation write SetOrientation;
  end;

implementation

uses Types;

{ --- TFXScrollBar ------------------------------------------------------------- }

constructor TFXScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +112;
  PrefWidth := 200;
  PrefHeight := 20;
  BlockIncrement := 10;
  UnitIncrement := 1;
  Value := 0;
  VisibleAmount := 10;
  Min := 0;
  Max := 100;
  JavaType := 'ScrollBar';
end;

procedure TFXScrollBar.Paint;
var
  XPos, YPos, X1Pos, X2Pos, DeltaX, Y1Pos, Y2Pos, DeltaY: Integer;
  Points: array [0 .. 2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Font.Style := [];
  Canvas.Pen.Color := Foreground;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  if Orientation = HORIZONTAL then
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, 16, Height));
    Canvas.Rectangle(Rect(Width - 16, 0, Width, Height));
    Canvas.Pen.Color := DefaultForeground;
    Canvas.Brush.Color := DefaultForeground;
    XPos := 4;
    YPos := Height div 2;

    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos + 5, YPos - 5);
    Points[2] := Point(XPos + 5, YPos + 5);
    Canvas.Polygon(Points);
    XPos := Width - 4;
    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos - 5, YPos + 5);
    Points[2] := Point(XPos - 5, YPos - 5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color := BlueColor;
    Canvas.Brush.Color := DefaultBackground;
    DeltaX := Round(VisibleAmount * 1.68 / 2);
    X1Pos := 16 + DeltaX;
    X2Pos := Width - 16 - DeltaX;
    XPos := X1Pos + Round((X2Pos - X1Pos) / ((FMax - FMin) * 1.0) * FValue);
    Canvas.Rectangle(Rect(XPos - DeltaX, 0, XPos + DeltaX, Height));

    Canvas.MoveTo(XPos - 2, 5);
    Canvas.LineTo(XPos - 2, Height - 5);
    Canvas.MoveTo(XPos, 5);
    Canvas.LineTo(XPos, Height - 5);
    Canvas.MoveTo(XPos + 2, 5);
    Canvas.LineTo(XPos + 2, Height - 5);
  end
  else
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, Width, 16));
    Canvas.Rectangle(Rect(0, Height - 16, Width, Height));
    Canvas.Pen.Color := DefaultForeground;
    Canvas.Brush.Color := DefaultForeground;
    XPos := Width div 2;
    YPos := 4;
    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos - 5, YPos + 5);
    Points[2] := Point(XPos + 5, YPos + 5);
    Canvas.Polygon(Points);
    YPos := Height - 4;
    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos - 5, YPos - 5);
    Points[2] := Point(XPos + 5, YPos - 5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color := BlueColor;
    Canvas.Brush.Color := DefaultBackground;
    DeltaY := Round(VisibleAmount * 1.68 / 2);
    Y1Pos := 16 + DeltaY;
    Y2Pos := Height - 16 - DeltaY;
    YPos := Y1Pos + Round((Y2Pos - Y1Pos) / ((FMax - FMin) * 1.0) * FValue);
    Canvas.Rectangle(Rect(0, YPos - DeltaY, Width, YPos + DeltaY));
    Canvas.MoveTo(5, YPos - 2);
    Canvas.LineTo(Width - 5, YPos - 2);
    Canvas.MoveTo(5, YPos);
    Canvas.LineTo(Width - 5, YPos);
    Canvas.MoveTo(5, YPos + 2);
    Canvas.LineTo(Width - 5, YPos + 2);
  end;
end;

procedure TFXScrollBar.SetValue(AValue: Integer);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Invalidate;
  end;
end;

procedure TFXScrollBar.SetOrientation(AValue: TOrientation);
var
  Tmp: Integer;
begin
  if AValue <> FOrientation then
  begin
    FOrientation := AValue;
    if not(csLoading in ComponentState) then
    begin
      Tmp := Width;
      Width := Height;
      Height := Tmp;
    end;
    Invalidate;
  end;
end;

procedure TFXScrollBar.SetVisibleAmount(AValue: Integer);
begin
  if AValue <> FVisibleAmount then
  begin
    FVisibleAmount := AValue;
    Invalidate;
  end;
end;

procedure TFXScrollBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ScrollBar ' + Name + ' = new ScrollBar();');
end;

function TFXScrollBar.GetAttributes(ShowAttributes: Integer): string;
const
  ScrollBarAttributes1 = '|Max|Min|Orientation|Value';
  ScrollBarAttributes2 = ScrollBarAttributes1 +
    '|BlockIncrement|UnitIncrement|VisibleAmount';
begin
  if ShowAttributes = 1 then
    Result := ScrollBarAttributes1 + inherited GetAttributes(3)
  else
    Result := ScrollBarAttributes2 + inherited GetAttributes(3);
end;

end.
