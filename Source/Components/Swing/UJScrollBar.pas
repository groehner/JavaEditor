unit UJScrollBar;

interface

uses
  Classes,
  StdCtrls,
  UJEComponents,
  UJComponents;

type

  TJScrollBar = class(TSwingComponent)
  private
    FBlockIncrement: Integer;
    FMaximum: Integer;
    FMinimum: Integer;
    FUnitIncrement: Integer;
    FVisibleAmount: Integer;
    FValue: Integer;
    FOrientation: TOrientation;
    procedure SetOrientation(AValue: TOrientation);
    procedure SetValue(AValue: Integer);
    procedure SetVisibleAmount(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property BlockIncrement: Integer read FBlockIncrement write FBlockIncrement
      default 10;
    property Maximum: Integer read FMaximum write FMaximum default 100;
    property Minimum: Integer read FMinimum write FMinimum default 0;
    property Value: Integer read FValue write SetValue;
    property UnitIncrement: Integer read FUnitIncrement write FUnitIncrement;
    property VisibleAmount: Integer read FVisibleAmount write SetVisibleAmount;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property adjustmentValueChanged;
  end;

implementation

uses
  Graphics,
  Controls,
  Types,
  Forms;

{ --- TJScrollBar -------------------------------------------------------------- }

constructor TJScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +10;
  Width := 120;
  Height := 24;
  BlockIncrement := 10;
  UnitIncrement := 1;
  Value := 0;
  VisibleAmount := 10;
  Minimum := 0;
  Maximum := 100;
  Orientation := HORIZONTAL;
  ShowFont := False;
  JavaType := 'JScrollBar';
end;

function TJScrollBar.GetAttributes(ShowAttributes: Integer): string;
const
  Scroll1 = '|Value|Minimum|Maximum|Orientation';
  Scroll2 = '|BlockIncrement|UnitIncrement|VisibleAmount';
begin
  if ShowAttributes = 1 then
    Result := Scroll1
  else
    Result := Scroll1 + Scroll2;
  Result := Result + inherited;
end;

function TJScrollBar.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|adjustmentValueChanged' + inherited GetEvents(ShowEvents);
end;

procedure TJScrollBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JScrollBar ' + Name + ' = new JScrollBar();');
  MakeAttribut('Orientation', 'Scrollbar.HORIZONTAL');
end;

procedure TJScrollBar.Paint;
var
  XPos, YPos, X1Pos, X2Pos, DeltaX, Y1Pos, Y2Pos, DeltaY: Integer;
  Points: array [0 .. 2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Foreground;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  var
  P16 := PPIScale(16);
  var
  Posi5 := PPIScale(5);
  var
  Posi2 := PPIScale(2);
  if Orientation = HORIZONTAL then
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, P16, Height));
    Canvas.Rectangle(Rect(Width - P16, 0, Width, Height));
    Canvas.Pen.Color := DefaultForeground;
    Canvas.Brush.Color := DefaultForeground;
    XPos := Posi5 - 1;
    YPos := Height div 2;

    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos + Posi5, YPos - Posi5);
    Points[2] := Point(XPos + Posi5, YPos + Posi5);
    Canvas.Polygon(Points);
    XPos := Width - Posi5 - 1;
    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos - Posi5, YPos + Posi5);
    Points[2] := Point(XPos - Posi5, YPos - Posi5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color := BlueColor;
    Canvas.Brush.Color := DefaultBackground;
    DeltaX := Round(VisibleAmount * 1.68 / 2);
    X1Pos := P16 + DeltaX;
    X2Pos := Width - P16 - DeltaX;
    XPos := X1Pos + Round((X2Pos - X1Pos) / ((FMaximum - FMinimum) * 1.0)
      * FValue);
    Canvas.Rectangle(Rect(XPos - DeltaX, 0, XPos + DeltaX, Height));

    Canvas.MoveTo(XPos - Posi2, Posi5);
    Canvas.LineTo(XPos - Posi2, Height - Posi5);
    Canvas.MoveTo(XPos, Posi5);
    Canvas.LineTo(XPos, Height - Posi5);
    Canvas.MoveTo(XPos + Posi2, Posi5);
    Canvas.LineTo(XPos + Posi2, Height - Posi5);
  end
  else
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, Width, P16));
    Canvas.Rectangle(Rect(0, Height - P16, Width, Height));
    Canvas.Pen.Color := DefaultForeground;
    Canvas.Brush.Color := DefaultForeground;
    XPos := Width div 2;
    YPos := Posi5 - 1;
    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos - Posi5, YPos + Posi5);
    Points[2] := Point(XPos + Posi5, YPos + Posi5);
    Canvas.Polygon(Points);
    YPos := Height - 4;
    Points[0] := Point(XPos + 0, YPos + 0);
    Points[1] := Point(XPos - Posi5, YPos - Posi5);
    Points[2] := Point(XPos + Posi5, YPos - Posi5);
    Canvas.Polygon(Points);

    // scroller
    Canvas.Pen.Color := BlueColor;
    Canvas.Brush.Color := DefaultBackground;
    DeltaY := Round(VisibleAmount * 1.68 / 2);
    Y1Pos := P16 + DeltaY;
    Y2Pos := Height - P16 - DeltaY;
    YPos := Y1Pos + Round((Y2Pos - Y1Pos) / ((FMaximum - FMinimum) * 1.0)
      * FValue);
    Canvas.Rectangle(Rect(0, YPos - DeltaY, Width, YPos + DeltaY));
    Canvas.MoveTo(Posi5, YPos - Posi2);
    Canvas.LineTo(Width - Posi5, YPos - Posi2);
    Canvas.MoveTo(Posi5, YPos);
    Canvas.LineTo(Width - Posi5, YPos);
    Canvas.MoveTo(Posi5, YPos + Posi2);
    Canvas.LineTo(Width - Posi5, YPos + Posi2);
  end;
end;

procedure TJScrollBar.SetValue(AValue: Integer);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Invalidate;
  end;
end;

procedure TJScrollBar.SetOrientation(AValue: TOrientation);
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

procedure TJScrollBar.SetVisibleAmount(AValue: Integer);
begin
  if AValue <> FVisibleAmount then
  begin
    FVisibleAmount := AValue;
    Invalidate;
  end;
end;

end.
