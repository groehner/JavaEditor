unit UFXSVGPath;

interface

uses
  Classes,
  UFXShape;

type

  TFillRule = (_TA_EVEN_ODD, _TA_NON_ZERO);

  TFXSVGPath = class (TFXShape)
  private
    FContent: string;
    FFillRule: TFillRule;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure Paint; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property Content: string read FContent write FContent;
    property FillRule: TFillRule read FFillRule write FFillRule;
  end;

implementation

{--- TFXRectangle -------------------------------------------------------------}

constructor TFXSVGPath.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +171;
  Width:= 120;
  Height:= 80;
  FFillRule:= _TA_NON_ZERO;
  FContent:= 'M 10 10 L 30 10 L 20 30 z';
  JavaType:= 'SVGPath';
end;

procedure TFXSVGPath.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private SVGPath ' + Name + ' = new SVGPath();');
  MakeAttribut('Content', AsString(FContent));
end;

procedure TFXSVGPath.Paint;
begin
  DefaultPenBrush;
end;

function TFXSVGPath.GetAttributes(ShowAttributes: Integer): string;
  const SVGPathAttributes = '|Content|FillRule';
begin
  Result:= SVGPathAttributes + inherited GetAttributes(ShowAttributes) + '|LayoutX|LayoutY';
end;

procedure TFXSVGPath.SetAttribute(Attr, Value, Typ: string);
begin
  inherited;
  Invalidate;
end;

end.
