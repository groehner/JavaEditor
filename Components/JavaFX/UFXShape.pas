unit UFXShape;

interface

uses Messages, Classes, Controls, Graphics, UFXComponents;

type

  TStrokeLineCap = (_TA_BUTT, _TA_ROUND, _TA_SQUARE);
  TStrokeLineJoin = (_CD_BEVEL, _CD_MITER, _CD_ROUND);
  TStrokeType = (_TA_CENTERED, _TA_INSIDE, _TA_OUTSIDE);

  TFXShape = class (TFXNode)
  private
    FFill: TColor;
    FSmooth: boolean;
    FStroke: TColor;
    FStrokeDashOffset: double;
    FStrokeLineCap: TStrokeLineCap;
    FStrokeLineJoin: TStrokeLineJoin;
    FStrokeMiterLimit: double;
    FStrokeType: TStrokeType;
    FStrokeWidth: double;
    procedure setAColor(aColor: TColor);
    procedure setStrokeColor(aColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DefaultPenBrush;

    //procedure WmEraseBkgnd (var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    //procedure CreateParams (var Params: TCreateParams); override;
  published
    property Fill: TColor read FFill write setAColor;
    property Smooth: boolean read FSmooth write fSmooth;
    property Stroke: TColor read FStroke write setStrokeColor;
    property StrokeDashOffset: double read FStrokeDashOffset write FStrokeDashOffset;
    property StrokeLineCap: TStrokeLineCap read FStrokeLineCap write FStrokeLineCap;
    property StrokeLineJoin: TStrokeLineJoin read FStrokeLineJoin write FStrokeLineJoin;
    property StrokeMiterLimit: double read FStrokeMiterLimit write FStrokeMiterLimit;
    property StrokeType: TStrokeType read FStrokeType write FStrokeType;
    property StrokeWidth: double read FStrokeWidth write FStrokeWidth;
  end;

implementation

uses Windows, SysUtils;

{--- TFXShape -----------------------------------------------------------------}

constructor TFXShape.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FFill:= clBlack;
  FSmooth:= true;
  FStroke:= -16777201; // clNone
  FStrokeLineCap:= _TA_SQUARE;
  FStrokeLineJoin:= _CD_MITER;
  FStrokeType:= _TA_CENTERED;
  FStrokeDashOffset:= 0;
  FStrokeMiterLimit:= 10;
  FStrokeWidth:= 1;
end;

procedure TFXShape.NewControl;
begin
  InsertImport('javafx.scene.shape.*');
  InsertImport('javafx.scene.paint.*'); // colors
end;

function TFXShape.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Smooth|Fill|Stroke';
        Attributes2 =  Attributes1 +
                       '|StrokeDashOffset|StrokeLineCap|StrokeLineJoin' +
                       '|StrokeMiterLimit|StrokeType|StrokeWidth';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes);
  var p:= Pos('|LayoutX|LayoutY', Result);
  if p > 0 then delete(Result, p, 16);
end;

procedure TFXShape.SetAttribute(Attr, Value, Typ: string);
  var s1, s2: string;
begin
  s1:= Name + '.set' + Attr;
  s2:= '';
  if Attr = 'StrokeLineCap' then
    s2:= '(StrokeLineCap.' + Value + ');'
  else if Attr = 'StrokeLineJoin' then
    s2:= '(StrokeLineJoin.' + Value + ');'
  else if Attr = 'StrokeType' then
    s2:= '(StrokeType.' + Value + ');';
  if s2 <> '' then begin
    s2:= s1 + s2;
    setAttributValue(s1, s2);
  end else
    inherited;
end;

procedure TFXShape.SetAColor(aColor: TColor);
begin
  if aColor <> FFill then begin
    FFill:= aColor;
    Invalidate;
  end;
end;

procedure TFXShape.SetStrokeColor(aColor: TColor);
begin
  if aColor <> FStroke then begin
    FStroke:= aColor;
    Invalidate;
  end;
end;

{
procedure TFXShape.CreateParams (var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TFXShape.WmEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1
end;
}

procedure TFXShape.DefaultPenBrush;
begin
  Canvas.Pen.Color:= Stroke;
  Canvas.Pen.Mode:= pmCopy;
  Canvas.Pen.Style:= psSolid;
  Canvas.Pen.Width:= 1;
  Canvas.Brush.Color:= Fill;
  Canvas.Brush.Style:= bsSolid;
end;

end.
