unit UFXCircle;

interface

uses
  Classes,
  UFXShape;

type
  TFXCircle = class(TFXShape)
  private
    FCenterX: Double;
    FCenterY: Double;
    FRadius: Double;

    FOldWidth: Integer;
    FOldHeight: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property CenterX: Double read FCenterX write FCenterX;
    property CenterY: Double read FCenterY write FCenterY;
    property Radius: Double read FRadius write FRadius;
  end;

  TFXEllipse = class(TFXShape)
  private
    FCenterX: Double;
    FCenterY: Double;
    FRadiusX: Double;
    FRadiusY: Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property CenterX: Double read FCenterX write FCenterX;
    property CenterY: Double read FCenterY write FCenterY;
    property RadiusX: Double read FRadiusX write FRadiusX;
    property RadiusY: Double read FRadiusY write FRadiusY;
  end;

  TArcType = (_TA_OPEN, _TA_CHORD, _TA_ROUND);

  TFXArc = class(TFXEllipse)
  private
    FStartAngle: Double;
    FLength: Double;
    FType: TArcType;
    procedure SetLength(ALength: Double);
    procedure SetStartAngle(AStartAngle: Double);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property Length: Double read FLength write SetLength;
    property StartAngle: Double read FStartAngle write SetStartAngle;
    // ULink SetChangeAttributeNames: _Type -> Type
    property _Type: TArcType read FType write FType;
  end;

implementation

uses
  Math,
  SysUtils,
  UObjectInspector;

{ --- TFXCircle ---------------------------------------------------------------- }

constructor TFXCircle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +161;
  FRadius := Width div 2;
  Height := Width;
  FOldWidth := Width;
  FOldHeight := Height;
  JavaType := 'Circle';
end;

procedure TFXCircle.Paint;
begin
  DefaultPenBrush;
  Canvas.Ellipse(0, 0, Width, Height);
end;

procedure TFXCircle.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private Circle ' + Name + ' = new Circle();');
end;

function TFXCircle.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes = '|CenterX|CenterY|Radius';
begin
  Result := Attributes + inherited GetAttributes(ShowAttributes);
end;

procedure TFXCircle.SetPositionAndSize;
var
  SCenterX, SCenterY, SRadius: string;
begin
  if FOldWidth <> Width then
    Height := Width
  else if FOldHeight <> Height then
    Width := Height;
  Radius := Width div 2;
  CenterX := Left + Radius;
  CenterY := Top + Radius;
  SCenterX := Format('%g', [CenterX]);
  SCenterY := Format('%g', [CenterY]);
  SRadius := Format('%g', [Radius]);
  ChangeAttributValue(Name + '.setCenterX(', Name + '.setCenterX(' +
    SCenterX + ');');
  ChangeAttributValue(Name + '.setCenterY(', Name + '.setCenterY(' +
    SCenterY + ');');
  ChangeAttributValue(Name + '.setRadius(', Name + '.setRadius(' +
    SRadius + ');');
  FObjectInspector.UpdatePropertyInspector;
  FOldWidth := Width;
  FOldHeight := Height;
end;

procedure TFXCircle.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'CenterX' then
    Left := Round(StrToFloat(Value) - Radius);
  if Attr = 'CenterY' then
    Top := Round(StrToFloat(Value) - Radius);
  if Attr = 'Radius' then
  begin
    Width := Round(StrToFloat(Value)) * 2;
    Height := Width;
  end;
  inherited;
  SetPositionAndSize;
end;

{ --- TFXEllipse --------------------------------------------------------------- }

constructor TFXEllipse.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +163;
  FRadiusX := Width;
  FRadiusY := Height;
  JavaType := 'Ellipse';
end;

procedure TFXEllipse.Paint;
begin
  DefaultPenBrush;
  Canvas.Ellipse(0, 0, Width, Height);
end;

procedure TFXEllipse.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private Ellipse ' + Name + ' = new Ellipse();');
end;

function TFXEllipse.GetAttributes(ShowAttributes: Integer): string;
const
  EllipseAttributes = '|CenterX|CenterY|RadiusX|RadiusY';
begin
  Result := EllipseAttributes + inherited GetAttributes(ShowAttributes);
end;

procedure TFXEllipse.SetPositionAndSize;
var
  SCenterX, SCenterY, SRadiusX, SRadiusY: string;
begin
  RadiusX := Width div 2;
  RadiusY := Height div 2;
  CenterX := Left + RadiusX;
  CenterY := Top + RadiusY;
  SCenterX := Format('%g', [CenterX]);
  SCenterY := Format('%g', [CenterY]);
  SRadiusX := Format('%g', [RadiusX]);
  SRadiusY := Format('%g', [RadiusY]);
  ChangeAttributValue(Name + '.setCenterX(', Name + '.setCenterX(' +
    SCenterX + ');');
  ChangeAttributValue(Name + '.setCenterY(', Name + '.setCenterY(' +
    SCenterY + ');');
  ChangeAttributValue(Name + '.setRadiusX(', Name + '.setRadiusX(' +
    SRadiusX + ');');
  ChangeAttributValue(Name + '.setRadiusY(', Name + '.setRadiusY(' +
    SRadiusY + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXEllipse.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'CenterX' then
    Left := Round(StrToFloat(Value) - RadiusX);
  if Attr = 'CenterY' then
    Top := Round(StrToFloat(Value) - RadiusY);
  if Attr = 'RadiusX' then
    Width := Round(StrToFloat(Value)) * 2;
  if Attr = 'RadiusY' then
    Height := Round(StrToFloat(Value)) * 2;
  inherited;
  SetPositionAndSize;
end;

{ --- TFXArc ------------------------------------------------------------------- }

constructor TFXArc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +166;
  FRadiusX := Width;
  FRadiusY := Height;
  Length := 270;
  FType := _TA_ROUND;
  JavaType := 'Arc';
end;

procedure TFXArc.Paint;
var
  X3Pos, Y3Pos, X4Pos, Y4Pos: Integer;

  function tangens(Angle: Double): Double;
  begin
    Result := Tan(Angle / 180 * Pi);
  end;

  procedure calculate(Angle: Double; var XPos, YPos: Integer);
  begin
    Angle := Angle - Int(Angle / 360) * 360;
    if Angle < 0 then
      Angle := Angle + 360;

    if (-45 <= Angle) and (Angle <= 45) then
    begin
      XPos := Width;
      YPos := Height div 2 - Round(tangens(Angle) * RadiusX);
    end
    else if (45 <= Angle) and (Angle <= 135) then
    begin
      YPos := 0;
      XPos := Width div 2 + Round(tangens(90 - Angle) * RadiusY);
    end
    else if (135 <= Angle) and (Angle <= 225) then
    begin
      XPos := 0;
      YPos := Height div 2 - Round(tangens(180 - Angle) * RadiusX);
    end
    else
    begin
      YPos := Height;
      XPos := Width div 2 - Round(tangens(270 - Angle) * RadiusY);
    end;
  end;

begin
  DefaultPenBrush;
  X3Pos := 0;
  Y3Pos := 0;
  X4Pos := 0;
  Y4Pos := 0;
  calculate(StartAngle, X3Pos, Y3Pos);
  calculate(StartAngle + Length, X4Pos, Y4Pos);
  if FType = _TA_ROUND then
    Canvas.Pie(0, 0, Width, Height, X3Pos, Y3Pos, X4Pos, Y4Pos)
  else
    Canvas.Chord(0, 0, Width, Height, X3Pos, Y3Pos, X4Pos, Y4Pos);
end;

procedure TFXArc.NewControl;
begin
  InsertImport('javafx.scene.shape.*');
  InsertImport('javafx.scene.paint.*'); // colors
  DefaultComponent;
  InsertNewVariable('private Arc ' + Name + ' = new Arc();');
  MakeAttribut('Length', '270');
  MakeAttribut('Type', 'ArcType.ROUND');
end;

function TFXArc.GetAttributes(ShowAttributes: Integer): string;
const
  ArcAttributes = '|Length|StartAngle|_Type';
begin
  Result := ArcAttributes + inherited GetAttributes(ShowAttributes);
end;

procedure TFXArc.SetAttribute(Attr, Value, Typ: string);
var
  Str1, Str2: string;
begin
  if Attr = 'Type' then
  begin
    Str1 := Name + '.setType';
    Str2 := Str1 + '(ArcType.' + Value + ');';
    SetAttributValue(Str1, Str2);
  end
  else
    inherited;
end;

procedure TFXArc.SetLength(ALength: Double);
begin
  if ALength <> FLength then
  begin
    FLength := ALength;
    Invalidate;
  end;
end;

procedure TFXArc.SetStartAngle(AStartAngle: Double);
begin
  if AStartAngle <> FStartAngle then
  begin
    FStartAngle := AStartAngle;
    Invalidate;
  end;
end;

end.
