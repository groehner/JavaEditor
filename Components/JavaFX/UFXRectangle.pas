unit UFXRectangle;

interface

uses Classes, UFXShape;

type

  TFXRectangle = class (TFXShape)
  private
    FArcHeight: double;
    FArcWidth: double;
    FX: double;
    FY: double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property ArcHeight: double read FArcHeight write FArcHeight;
    property ArcWidth: double read FArcWidth write FArcWidth;
    property X: double read FX write FX;
    property Y: double read FY write FY;
  end;

implementation

uses SysUtils, UObjectInspector;

{--- TFXRectangle -------------------------------------------------------------}

constructor TFXRectangle.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +162;
  Width:= 120;
  Height:= 80;
  JavaType:= 'Rectangle';
end;

procedure TFXRectangle.Paint;
begin
  DefaultPenBrush;
  Canvas.RoundRect(0, 0, Width, Height, Round(ArcWidth), Round(ArcHeight));
end;

procedure TFXRectangle.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private Rectangle ' + Name + ' = new Rectangle();');
end;

procedure TFXRectangle.SetPositionAndSize;
  var sX, sY: string;
begin
  X:= Left;
  Y:= Top;
  sX:= Format('%g',[X]);
  sY:= Format('%g',[Y]);
  ChangeAttributValue(Name + '.setX(', Name + '.setX(' + sX + ');');
  ChangeAttributValue(Name + '.setY(', Name + '.setY(' + sY + ');');
  ChangeAttributValue(Name + '.setWidth(', Name + '.setWidth(' + IntToStr(Width) + ');');
  ChangeAttributValue(Name + '.setHeight(',Name + '.setHeight(' + IntToStr(Height) + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

function TFXRectangle.getAttributes(ShowAttributes: integer): string;
  const RectangleAttributes1 = '|Height|Width|X|Y';
        RectangleAttributes2 = RectangleAttributes1 + '|ArcHeight|ArcWidth';
begin
  if ShowAttributes = 1
    then Result:= RectangleAttributes1 + inherited getAttributes(ShowAttributes)
    else Result:= RectangleAttributes2 + inherited getAttributes(ShowAttributes);
end;

procedure TFXRectangle.SetAttribute(Attr, Value, Typ: string);
begin
  inherited;
  if Attr = 'Y' then
    Top:= Round(FY)
   else if Attr = 'X' then
    Left:= Round(FX);
  Invalidate;
end;

end.
