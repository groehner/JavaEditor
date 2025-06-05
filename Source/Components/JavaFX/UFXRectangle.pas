unit UFXRectangle;

interface

uses
  Classes,
  UFXShape;

type

  TFXRectangle = class(TFXShape)
  private
    FArcHeight: Double;
    FArcWidth: Double;
    FXPos: Double;
    FYPos: Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property ArcHeight: Double read FArcHeight write FArcHeight;
    property ArcWidth: Double read FArcWidth write FArcWidth;
    property X: Double read FXPos write FXPos;
    property Y: Double read FYPos write FYPos;
  end;

implementation

uses
  SysUtils,
  UObjectInspector;

{ --- TFXRectangle ------------------------------------------------------------- }

constructor TFXRectangle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +162;
  Width := 120;
  Height := 80;
  JavaType := 'Rectangle';
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
var
  StrX, StrY: string;
begin
  X := Left;
  Y := Top;
  StrX := Format('%g', [X]);
  StrY := Format('%g', [Y]);
  ChangeAttributValue(Name + '.setX(', Name + '.setX(' + StrX + ');');
  ChangeAttributValue(Name + '.setY(', Name + '.setY(' + StrY + ');');
  ChangeAttributValue(Name + '.setWidth(', Name + '.setWidth(' +
    IntToStr(Width) + ');');
  ChangeAttributValue(Name + '.setHeight(', Name + '.setHeight(' +
    IntToStr(Height) + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

function TFXRectangle.GetAttributes(ShowAttributes: Integer): string;
const
  RectangleAttributes1 = '|Height|Width|X|Y';
  RectangleAttributes2 = RectangleAttributes1 + '|ArcHeight|ArcWidth';
begin
  if ShowAttributes = 1 then
    Result := RectangleAttributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := RectangleAttributes2 + inherited GetAttributes(ShowAttributes);
end;

procedure TFXRectangle.SetAttribute(Attr, Value, Typ: string);
begin
  inherited;
  if Attr = 'Y' then
    Top := Round(FYPos)
  else if Attr = 'X' then
    Left := Round(FXPos);
  Invalidate;
end;

end.
