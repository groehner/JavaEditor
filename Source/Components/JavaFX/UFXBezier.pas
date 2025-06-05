unit UFXBezier;

interface

uses
  Classes,
  Types,
  UFXShape;

type

  TFXQuadCurve = class(TFXShape)
  private
    FStartX: Double;
    FStartY: Double;
    FEndX: Double;
    FEndY: Double;
    FControlX: Double;
    FControlY: Double;
    FPoints: array [0 .. 3] of TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property StartX: Double read FStartX write FStartX;
    property StartY: Double read FStartY write FStartY;
    property EndX: Double read FEndX write FEndX;
    property EndY: Double read FEndY write FEndY;
    property ControlX: Double read FControlX write FControlX;
    property ControlY: Double read FControlY write FControlY;
  end;

  TFXCubicCurve = class(TFXShape)
  private
    FStartX: Double;
    FStartY: Double;
    FEndX: Double;
    FEndY: Double;
    FControlX1: Double;
    FControlY1: Double;
    FControlX2: Double;
    FControlY2: Double;
    FPoints: array [0 .. 3] of TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property StartX: Double read FStartX write FStartX;
    property StartY: Double read FStartY write FStartY;
    property EndX: Double read FEndX write FEndX;
    property EndY: Double read FEndY write FEndY;
    property ControlX1: Double read FControlX1 write FControlX1;
    property ControlY1: Double read FControlY1 write FControlY1;
    property ControlX2: Double read FControlX2 write FControlX2;
    property ControlY2: Double read FControlY2 write FControlY2;
  end;

implementation

uses
  SysUtils,
  Windows;

{ --- TFXQuadCurve ------------------------------------------------------------- }

constructor TFXQuadCurve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +169;
  Width := 120;
  Height := 80;
  FStartX := 0;
  FStartY := 25;
  FEndX := 100;
  FEndY := 50;
  FControlX := 30;
  FControlY := -20;
  JavaType := 'QuadCurve';
end;

procedure TFXQuadCurve.Paint;
var
  FControlX1, FControlY1, FControlX2, FControlY2: Double;
begin
  DefaultPenBrush;
  FControlX1 := FStartX + 2.0 / 3.0 * (FControlX - FStartX);
  FControlY1 := FStartY + 2.0 / 3.0 * (FControlY - FStartY);
  FControlX2 := FEndX + 2.0 / 3.0 * (FControlX - FEndX);
  FControlY2 := FEndY + 2.0 / 3.0 * (FControlY - FEndY);
  FPoints[0] := Point(Round(FStartX), Round(FStartY));
  FPoints[1] := Point(Round(FControlX1), Round(FControlY1));
  FPoints[2] := Point(Round(FControlX2), Round(FControlY2));
  FPoints[3] := Point(Round(FEndX), Round(FEndY));
  BeginPath(Canvas.Handle);
  Canvas.PolyBezier(FPoints);
  EndPath(Canvas.Handle);
  FillPath(Canvas.Handle);
end;

procedure TFXQuadCurve.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private QuadCurve ' + Name + ' = new QuadCurve();');
  MakeAttribut('StartX', IntToStr(Round(FStartX)));
  MakeAttribut('StartY', IntToStr(Round(FStartY)));
  MakeAttribut('EndX', IntToStr(Round(FEndX)));
  MakeAttribut('EndY', IntToStr(Round(FEndY)));
  MakeAttribut('ControlX', IntToStr(Round(FControlX)));
  MakeAttribut('ControlY', IntToStr(Round(FControlY)));
end;

function TFXQuadCurve.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes = '|StartX|StartY|EndX|EndY|ControlX|ControlY|Width|Height';
begin
  Result := Attributes + inherited GetAttributes(ShowAttributes) +
    '|LayoutX|LayoutY';
end;

procedure TFXQuadCurve.SetAttribute(Attr, Value, Typ: string);
begin
  inherited;
  Invalidate;
end;

{ --- TFXCubicCurve ------------------------------------------------------------ }

constructor TFXCubicCurve.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +170;
  Width := 120;
  Height := 80;
  FStartX := 0;
  FStartY := 25;
  FEndX := 100;
  FEndY := 25;
  FControlX1 := 30;
  FControlY1 := -20;
  FControlX2 := 80;
  FControlY2 := 80;
  JavaType := 'CubicCurve';
end;

procedure TFXCubicCurve.Paint;
begin
  DefaultPenBrush;
  FPoints[0] := Point(Round(FStartX), Round(FStartY));
  FPoints[1] := Point(Round(FControlX1), Round(FControlY1));
  FPoints[2] := Point(Round(FControlX2), Round(FControlY2));
  FPoints[3] := Point(Round(FEndX), Round(FEndY));
  BeginPath(Canvas.Handle);
  Canvas.PolyBezier(FPoints);
  EndPath(Canvas.Handle);
  FillPath(Canvas.Handle);
end;

procedure TFXCubicCurve.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private CubicCurve ' + Name + ' = new CubicCurve();');
  MakeAttribut('StartX', IntToStr(Round(FStartX)));
  MakeAttribut('StartY', IntToStr(Round(FStartY)));
  MakeAttribut('EndX', IntToStr(Round(FEndX)));
  MakeAttribut('EndY', IntToStr(Round(FEndY)));
  MakeAttribut('ControlX1', IntToStr(Round(FControlX1)));
  MakeAttribut('ControlY1', IntToStr(Round(FControlY1)));
  MakeAttribut('ControlX2', IntToStr(Round(FControlX2)));
  MakeAttribut('ControlY2', IntToStr(Round(FControlY2)));
end;

function TFXCubicCurve.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes =
    '|StartX|StartY|EndX|EndY|ControlX1|ControlY1|ControlX2|ControlY2|Width|Height';
begin
  Result := Attributes + inherited GetAttributes(ShowAttributes) +
    '|LayoutX|LayoutY';
end;

procedure TFXCubicCurve.SetAttribute(Attr, Value, Typ: string);
begin
  inherited;
  Invalidate;
end;

end.
