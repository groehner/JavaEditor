unit UFXCircle;

interface

uses Classes, UFXShape;

type
  TFXCircle = class (TFXShape)
  private
    FCenterX: double;
    FCenterY: double;
    FRadius: double;

    OldWidth: integer;
    OldHeight: integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property CenterX: double read FCenterX write FCenterX;
    property CenterY: double read FCenterY write FCenterY;
    property Radius: double read FRadius write FRadius;
  end;

  TFXEllipse = class (TFXShape)
  private
    FCenterX: double;
    FCenterY: double;
    FRadiusX: double;
    FRadiusY: double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property CenterX: double read FCenterX write FCenterX;
    property CenterY: double read FCenterY write FCenterY;
    property RadiusX: double read FRadiusX write FRadiusX;
    property RadiusY: double read FRadiusY write FRadiusY;
  end;

  TArcType = (_TA_OPEN, _TA_CHORD, _TA_ROUND);

  TFXArc = class (TFXEllipse)
  private
    FStartAngle: double;
    FLength: double;
    FType: TArcType;
    procedure setLength(aLength: double);
    procedure setStartAngle(aStartAngle: double);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;    
  published
    property Length: double read FLength write setLength;
    property StartAngle: double read FStartAngle write setStartAngle;
    // ULink SetChangeAttributeNames: _Type -> Type
    property _Type: TArcType read FType write FType;
  end;

implementation

uses Math, SysUtils, Graphics, UObjectInspector;

{--- TFXCircle ----------------------------------------------------------------}

constructor TFXCircle.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +161;
  FRadius:= Width div 2;
  Height:= Width;
  OldWidth:= Width;
  OldHeight:= Height;
  JavaType:= 'Circle';
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

function TFXCircle.getAttributes(ShowAttributes: integer): string;
  const Attributes = '|CenterX|CenterY|Radius';
begin
  Result:= Attributes + inherited getAttributes(ShowAttributes);
end;

procedure TFXCircle.SetPositionAndSize;
  var sCenterX, sCenterY, sRadius: string;
begin
  if OldWidth <> Width then
    Height:= Width
  else if OldHeight <> Height then
    Width:= Height;
  Radius:= Width div 2;
  CenterX:= Left + Radius;
  CenterY:= Top + Radius;
  sCenterX:= Format('%g',[CenterX]);
  sCenterY:= Format('%g',[CenterY]);
  sRadius:=  Format('%g',[Radius]);
  ChangeAttributValue(Name + '.setCenterX(', Name + '.setCenterX(' + sCenterX + ');');
  ChangeAttributValue(Name + '.setCenterY(', Name + '.setCenterY(' + sCenterY + ');');
  ChangeAttributValue(Name + '.setRadius(',  Name + '.setRadius(' + sRadius + ');');
  FObjectInspector.UpdatePropertyInspector;
  OldWidth:= Width;
  OldHeight:= Height;
end;

procedure TFXCircle.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'CenterX' then
    Left:= Round(StrToFloat(Value) - Radius);
  if Attr = 'CenterY' then
    Top:= Round(StrToFloat(Value) - Radius);
  if Attr = 'Radius' then begin
    Width:= Round(StrToFloat(Value))*2;
    Height:= Width;
  end;
  inherited;
  SetPositionAndSize;
end;

{--- TFXEllipse ---------------------------------------------------------------}

constructor TFXEllipse.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +163;
  FRadiusX:= Width;
  FRadiusY:= Height;
  JavaType:= 'Ellipse';
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

function TFXEllipse.getAttributes(ShowAttributes: integer): string;
  const EllipseAttributes = '|CenterX|CenterY|RadiusX|RadiusY';
begin
  Result:= EllipseAttributes + inherited getAttributes(ShowAttributes);
end;

procedure TFXEllipse.SetPositionAndSize;
  var sCenterX, sCenterY, sRadiusX, sRadiusY: string;
begin
  RadiusX:= Width div 2;
  RadiusY:= Height div 2;
  CenterX:= Left + RadiusX;
  CenterY:= Top + RadiusY;
  sCenterX:= Format('%g',[CenterX]);
  sCenterY:= Format('%g',[CenterY]);
  sRadiusX:= Format('%g',[RadiusX]);
  sRadiusY:= Format('%g',[RadiusY]);
  ChangeAttributValue(Name + '.setCenterX(', Name + '.setCenterX(' + sCenterX + ');');
  ChangeAttributValue(Name + '.setCenterY(', Name + '.setCenterY(' + sCenterY + ');');
  ChangeAttributValue(Name + '.setRadiusX(', Name + '.setRadiusX(' + sRadiusX + ');');
  ChangeAttributValue(Name + '.setRadiusY(', Name + '.setRadiusY(' + sRadiusY + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXEllipse.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'CenterX' then
    Left:= Round(StrToFloat(Value) - RadiusX);
  if Attr = 'CenterY' then
    Top:= Round(StrToFloat(Value) - RadiusY);
  if Attr = 'RadiusX' then
    Width:= Round(StrToFloat(Value))*2;
  if Attr = 'RadiusY' then
    Height:= Round(StrToFloat(Value))*2;
  inherited;
  SetPositionAndSize;
end;

{--- TFXArc -------------------------------------------------------------------}

constructor TFXArc.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +166;
  FRadiusX:= Width;
  FRadiusY:= Height;
  Length:= 270;
  FType:= _TA_Round;
  JavaType:= 'Arc';
end;

procedure TFXArc.Paint;
  var x3, y3, x4, y4: integer;

  function tangens(angle: double): double;
  begin
    Result:= tan(angle/180*Pi);
  end;

  procedure calculate(Angle: double; var x, y: integer);
  begin
    Angle:= Angle - Int(Angle/360)*360;
    if Angle < 0 then Angle:= Angle + 360;

    if (-45 <= Angle) and (Angle <= 45) then begin
      x:= Width;
      y:= Height div 2 - Round(tangens(Angle)*RadiusX);
    end else
    if (45 <= Angle) and (Angle <= 135) then begin
      y:= 0;
      x:= Width div 2 + Round(tangens(90-Angle)*RadiusY);
    end else
    if (135 <= Angle) and (Angle <= 225) then begin
      x:= 0;
      y:= Height div 2 - Round(tangens(180-Angle)*RadiusX);
    end else begin
      y:= Height;
      x:= Width div 2 - Round(tangens(270-Angle)*RadiusY);
    end;
  end;

begin
  DefaultPenBrush;
  x3:= 0; y3:= 0;
  x4:= 0; y4:= 0;
  calculate(StartAngle, x3, y3);
  calculate(StartAngle + Length, x4, y4);
  if FType = _TA_ROUND
    then Canvas.Pie(0, 0, Width, Height, x3, y3, x4, y4)
    else Canvas.Chord(0, 0, Width, Height, x3, y3, x4, y4);
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

function TFXArc.getAttributes(ShowAttributes: integer): string;
  const ArcAttributes = '|Length|StartAngle|_Type';
begin
  Result:= ArcAttributes + inherited getAttributes(ShowAttributes);
end;

procedure TFXArc.SetAttribute(Attr, Value, Typ: string);
  var s1, s2: string;
begin
  if Attr = 'Type' then begin
    s1:= Name + '.setType';
    s2:= s1 + '(ArcType.' + Value + ');';
    setAttributValue(s1, s2);
  end else
    inherited;
end;

procedure TFXArc.setLength(aLength: double);
begin
  if aLength <> FLength then begin
    FLength:= aLength;
    Invalidate;
  end;
end;

procedure TFXArc.setStartAngle(aStartAngle: double);
begin
  if aStartAngle <> FStartAngle then begin
    FStartAngle:= aStartAngle;
    Invalidate;
  end;
end;

end.
