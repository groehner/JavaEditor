unit UFXPolygon;

interface

uses Classes, Types, UFXShape;

type

  TFXPolygon = class (TFXShape)
  private
    FPoints: TStrings;
    FPointsArray: array of TPoint;
    procedure setPoints(aPoints: TStrings);
    procedure PointsToPointArray(Points: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property Points: TStrings read FPoints write setPoints;
  end;

  TFXPolyline = class (TFXPolygon)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
  published
  end;

implementation

uses SysUtils, Graphics, UJEComponents;

{--- TFXPolygon ---------------------------------------------------------------}

constructor TFXPolygon.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +164;
  Width:= 120;
  Height:= 80;
  FPoints:= TStringList.Create;
  FPoints.Text:= '10;10'#13#10'50;50'#13#10'90;10';
  PointsToPointArray(FPoints.Text);
  JavaType:= 'Polygon';
end;

destructor TFXPolygon.Destroy;
begin
  FreeAndNil(FPoints);
  inherited;
end;

procedure TFXPolygon.Paint;
begin
  DefaultPenBrush;
  Canvas.Polygon(FPointsArray);
end;

procedure TFXPolygon.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private Polygon ' + Name + ' = new Polygon();');
end;

procedure TFXPolygon.SetPositionAndSize;
  var i: integer; sPoints: string;
begin
  sPoints:= '';
  for i:= 0 to Length(FPointsArray) - 1 do
    sPoints:= sPoints + IntToStr(FPointsArray[i].x + Left) + '.0, ' + IntToStr(FPointsArray[i].y + Top) + '.0, ';
  Delete(sPoints, length(SPoints)-1, 2);
  ChangeAttributValue(Name + '.getPoints().addAll(', Name + '.getPoints().addAll(new Double[]{' + sPoints + '});');
end;

procedure TFXPolygon.setPoints(aPoints: TStrings);
begin
  if aPoints.Text <> FPoints.Text then begin
    PointsToPointArray(aPoints.Text);
    Invalidate;
  end;
end;

procedure TFXPolygon.PointsToPointArray(Points: string);
  var i, p, x, y: integer; s, sx, sy: string;
begin
  FPoints.Text:= Points;
  SetLength(FPointsArray, FPoints.Count);
  for i:= 0 to FPoints.Count - 1 do begin
    s:= FPoints[i];
    p:= Pos(';', s);
    if p = 0 then p:= Pos(':', s);
    if p = 0 then p:= Pos(',', s);
    sx:= copy(s, 1, p-1);
    sy:= copy(s, p+1, length(s));
    try
      x:= StrToInt(sx);
    except
      x:= 0;
    end;
    try
      y:= StrToInt(sy);
    except
      y:= 0;
    end;
    FPointsArray[i]:= Point(x, y);
  end;
end;

function TFXPolygon.getAttributes(ShowAttributes: integer): string;
  const PolygonAttributes = '|Points';
begin
  Result:= PolygonAttributes + inherited getAttributes(ShowAttributes)
end;

procedure TFXPolygon.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Points' then
    SetPositionAndSize
  else
    inherited;
end;

{--- TFXPolyline --------------------------------------------------------------}

constructor TFXPolyline.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +165;
  Width:= 120;
  Height:= 80;
  Stroke:= clBlack;
  Fill:= -16777201;
  FPoints:= TStringList.Create;
  FPoints.Text:= '10;10'#13#10'50;50'#13#10'90;10';
  JavaType:= 'Polyline';
end;

procedure TFXPolyline.Paint;
begin
  Canvas.Pen.Color:= Stroke;
  Canvas.Brush.Color:= Fill;
  if Fill = -16777201
    then Canvas.Polyline(FPointsArray)
    else Canvas.Polygon(FPointsArray);
end;

procedure TFXPolyline.NewControl;
begin
  DefaultComponent;
  InsertImport('javafx.scene.shape.*');
  InsertImport('javafx.scene.paint.*'); // colors
  InsertNewVariable('private Polyline ' + Name + ' = new Polyline();');
end;

end.
