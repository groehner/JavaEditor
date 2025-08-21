unit UFXShape;

interface

uses
  Classes,
  Graphics,
  UFXComponents;

type

  TStrokeLineCap = (_TA_BUTT, _TA_ROUND, _TA_SQUARE);
  TStrokeLineJoin = (_CD_BEVEL, _CD_MITER, _CD_ROUND);
  TStrokeType = (_TA_CENTERED, _TA_INSIDE, _TA_OUTSIDE);

  TFXShape = class(TFXNode)
  private
    FFill: TColor;
    FSmooth: Boolean;
    FStroke: TColor;
    FStrokeDashOffset: Double;
    FStrokeLineCap: TStrokeLineCap;
    FStrokeLineJoin: TStrokeLineJoin;
    FStrokeMiterLimit: Double;
    FStrokeType: TStrokeType;
    FStrokeWidth: Double;
    procedure SetFill(AColor: TColor);
    procedure SetStroke(AColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DefaultPenBrush;
    procedure MakeFont; override;

    // procedure WmEraseBkgnd (var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    // procedure CreateParams (var Params: TCreateParams); override;
  published
    property Fill: TColor read FFill write SetFill;
    property Smooth: Boolean read FSmooth write FSmooth;
    property Stroke: TColor read FStroke write SetStroke;
    property StrokeDashOffset: Double read FStrokeDashOffset
      write FStrokeDashOffset;
    property StrokeLineCap: TStrokeLineCap read FStrokeLineCap
      write FStrokeLineCap;
    property StrokeLineJoin: TStrokeLineJoin read FStrokeLineJoin
      write FStrokeLineJoin;
    property StrokeMiterLimit: Double read FStrokeMiterLimit
      write FStrokeMiterLimit;
    property StrokeType: TStrokeType read FStrokeType write FStrokeType;
    property StrokeWidth: Double read FStrokeWidth write FStrokeWidth;
  end;

implementation

{ --- TFXShape ----------------------------------------------------------------- }

constructor TFXShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFill := clBlack;
  FSmooth := True;
  FStroke := -16777201; // clNone
  FStrokeLineCap := _TA_SQUARE;
  FStrokeLineJoin := _CD_MITER;
  FStrokeType := _TA_CENTERED;
  FStrokeDashOffset := 0;
  FStrokeMiterLimit := 10;
  FStrokeWidth := 1;
end;

procedure TFXShape.NewControl;
begin
  InsertImport('javafx.scene.shape.*');
  InsertImport('javafx.scene.paint.*'); // colors
end;

function TFXShape.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Smooth|Fill|Stroke';
  Attributes2 = Attributes1 + '|StrokeDashOffset|StrokeLineCap|StrokeLineJoin' +
    '|StrokeMiterLimit|StrokeType|StrokeWidth';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
  var
  Posi := Pos('|LayoutX|LayoutY', Result);
  if Posi > 0 then
    Delete(Result, Posi, 16);
end;

procedure TFXShape.SetAttribute(Attr, Value, Typ: string);
var
  Str1, Str2: string;
begin
  Str1 := Name + '.set' + Attr;
  Str2 := '';
  if Attr = 'StrokeLineCap' then
    Str2 := '(StrokeLineCap.' + Value + ');'
  else if Attr = 'StrokeLineJoin' then
    Str2 := '(StrokeLineJoin.' + Value + ');'
  else if Attr = 'StrokeType' then
    Str2 := '(StrokeType.' + Value + ');';
  if Str2 <> '' then
  begin
    Str2 := Str1 + Str2;
    SetAttributValue(Str1, Str2);
  end
  else
    inherited;
end;

procedure TFXShape.SetFill(AColor: TColor);
begin
  if AColor <> FFill then
  begin
    FFill := AColor;
    Invalidate;
  end;
end;

procedure TFXShape.SetStroke(AColor: TColor);
begin
  if AColor <> FStroke then
  begin
    FStroke := AColor;
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

procedure TFXShape.MakeFont;
begin
  // no font
end;

procedure TFXShape.DefaultPenBrush;
begin
  Canvas.Pen.Color := Stroke;
  Canvas.Pen.Mode := pmCopy;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  Canvas.Brush.Color := Fill;
  Canvas.Brush.Style := bsSolid;
end;

end.
