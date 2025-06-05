unit UFXPolygon;

interface

uses
  Classes,
  Types,
  UFXShape;

type

  TFXPolygon = class(TFXShape)
  private
    FPoints: TStrings;
    FPointsArray: array of TPoint;
    procedure SetPoints(APoints: TStrings);
    procedure PointsToPointArray(Points: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property Points: TStrings read FPoints write SetPoints;
  end;

  TFXPolyline = class(TFXPolygon)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
  end;

implementation

uses
  SysUtils,
  Graphics,
  UJEComponents;

{ --- TFXPolygon --------------------------------------------------------------- }

constructor TFXPolygon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +164;
  Width := 120;
  Height := 80;
  FPoints := TStringList.Create;
  FPoints.Text := '10;10'#13#10'50;50'#13#10'90;10';
  PointsToPointArray(FPoints.Text);
  JavaType := 'Polygon';
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
var
  SPoints: string;
begin
  SPoints := '';
  for var I := 0 to Length(FPointsArray) - 1 do
    SPoints := SPoints + IntToStr(FPointsArray[I].X + Left) + '.0, ' +
      IntToStr(FPointsArray[I].Y + Top) + '.0, ';
  Delete(SPoints, Length(SPoints) - 1, 2);
  ChangeAttributValue(Name + '.getPoints().addAll(',
    Name + '.getPoints().addAll(new Double[]{' + SPoints + '});');
end;

procedure TFXPolygon.SetPoints(APoints: TStrings);
begin
  if APoints.Text <> FPoints.Text then
  begin
    PointsToPointArray(APoints.Text);
    Invalidate;
  end;
end;

procedure TFXPolygon.PointsToPointArray(Points: string);
var
  Posi, XPos, YPos: Integer;
  Str, StrX, StrY: string;
begin
  FPoints.Text := Points;
  SetLength(FPointsArray, FPoints.Count);
  for var I := 0 to FPoints.Count - 1 do
  begin
    Str := FPoints[I];
    Posi := Pos(';', Str);
    if Posi = 0 then
      Posi := Pos(':', Str);
    if Posi = 0 then
      Posi := Pos(',', Str);
    StrX := Copy(Str, 1, Posi - 1);
    StrY := Copy(Str, Posi + 1, Length(Str));
    try
      XPos := StrToInt(StrX);
    except
      XPos := 0;
    end;
    try
      YPos := StrToInt(StrY);
    except
      YPos := 0;
    end;
    FPointsArray[I] := Point(XPos, YPos);
  end;
end;

function TFXPolygon.GetAttributes(ShowAttributes: Integer): string;
const
  PolygonAttributes = '|Points';
begin
  Result := PolygonAttributes + inherited GetAttributes(ShowAttributes);
end;

procedure TFXPolygon.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Points' then
    SetPositionAndSize
  else
    inherited;
end;

{ --- TFXPolyline -------------------------------------------------------------- }

constructor TFXPolyline.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +165;
  Width := 120;
  Height := 80;
  Stroke := clBlack;
  Fill := -16777201;
  FPoints := TStringList.Create;
  FPoints.Text := '10;10'#13#10'50;50'#13#10'90;10';
  JavaType := 'Polyline';
end;

procedure TFXPolyline.Paint;
begin
  Canvas.Pen.Color := Stroke;
  Canvas.Brush.Color := Fill;
  if Fill = -16777201 then
    Canvas.Polyline(FPointsArray)
  else
    Canvas.Polygon(FPointsArray);
end;

procedure TFXPolyline.NewControl;
begin
  DefaultComponent;
  InsertImport('javafx.scene.shape.*');
  InsertImport('javafx.scene.paint.*'); // colors
  InsertNewVariable('private Polyline ' + Name + ' = new Polyline();');
end;

end.
