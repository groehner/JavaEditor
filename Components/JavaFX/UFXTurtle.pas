unit UFXTurtle;

interface

uses
  Classes, Graphics, UFXCanvas;

type

  TLineCap = (_TA_BUTT, _TA_ROUND, _TA_SQUARE);
  TLineJoin = (_CD_BEVEL, _CD_MITER, _CD_ROUND);

  TFXTurtle = class(TFXCanvas)
  private
    FAnimated: boolean;
    FAnimationspeed: double;
    FTurtleX: double;
    FTurtleY: double;
    FTurtleW: double;
    FOriginX: double;
    FOriginY: double;
    FFill: TColor;
    FStroke: TColor;
    FLineCap: TLineCap;
    FLineJoin: TLineJoin;
    FLineWidth: double;
    FMiterLimit: double;
    FSmooth: boolean;
    FLineDashOffset: double;
    FVisibleImage: boolean;
  private
    procedure setFillColor(aColor: TColor);
    procedure setStrokeColor(aColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property Animated: boolean read FAnimated write FAnimated;
    property Animationspeed: double read FAnimationspeed write FAnimationspeed;
    property TurtleX: double read FTurtleX write FTurtleX;
    property TurtleY: double read FTurtleY write FTurtleY;
    property TurtleW: double read FTurtleW write FTurtleW;

    property OriginX: double read FOriginX write FOriginX;
    property OriginY: double read FOriginY write FOriginY;
    property Fill: TColor read FFill write setFillColor;
    property Stroke: TColor read FStroke write setStrokeColor;
    property LineCap: TLineCap read FLineCap write FLineCap;
    property LineJoin: TLineJoin read FLineJoin write FLineJoin;
    property LineWidth: double read FLineWidth write FLineWidth;
    property LineDashOffset: double read FLineDashOffset write FLineDashOffset;
    property MiterLimit: double read FMiterLimit write FMiterLimit;
    property Smooth: boolean read FSmooth write FSmooth;
    property VisibleImage: boolean read FVisibleImage write FVisibleImage;
  end;

implementation

uses SysUtils, UUtils,
     UObjectInspector, UGUIDesigner, UFXComponents;

{--- TFXCanvas -----------------------------------------------------------------}

constructor TFXTurtle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width:= 120;
  Height:= 80;
  Tag:= 120;
  FOriginX:= Width div 2;
  FOriginY:= Height div 2;
  FAnimated:= true;
  FAnimationspeed:= 100;
  FFill:= clWhite;
  FStroke:= clWhite;
  FLineCap:= _TA_ROUND;
  FLineJoin:= _CD_ROUND;
  FLineWidth:= 1;
  FMiterLimit:= 10;
  FSmooth:= true;
  FLineDashOffset:= 0;
  FVisibleImage:= true;
  JavaType:= 'Turtle';
end;

procedure TFXTurtle.Paint;
begin
  Canvas.Pen.Color:= Stroke;
  Canvas.Brush.Color:= Fill;
  Canvas.Rectangle(0, 0, Width, Height);
  var aBitmap:= TBitmap.Create;
  aBitmap.Transparent:= true;
  FGUIDesigner.vilFXTurtle.GetBitmap(0, aBitmap);
  RotateBitmap(aBitmap, -TurtleW*2*Pi/360, true);
  Canvas.Draw(round(originX + turtleX - aBitmap.Width/2), round(originY - turtleY - aBitmap.Height/2), aBitmap);
  FreeAndNil(aBitmap);
end;

procedure TFXTurtle.NewControl;
  var key, s: string;
begin
  DefaultComponent;
  InsertImport('je.fx.util.Turtle');
  InsertImport('javafx.scene.canvas.*');
  InsertImport('javafx.scene.shape.*');
  InsertImport('javafx.scene.paint.*');
  InsertNewVariable('private Turtle '  + Name + ' = new Turtle();');
  // set animated must be last of all turtle statements, so no animation during setup
  key:= Name + '.setAnimated';
  s:= Name + '.setAnimated(true);';
  if Pos(Indent2, s) = 0 then
    s:= Indent2 + s;
  Partner.setAttributValue(GetContainerAdd, key, s, 1);
end;

function TFXTurtle.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Animated|TurtleX|TurtleY|TurtleW|OriginX|OriginY|Fill|Stroke|LineWidth|VisibleImage';
        Attributes2 =  Attributes1 + '|Animationspeed|LineCap|LineJoin|LineDashOffset|MiterLimit|Smooth';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited
    else Result:= Attributes2 + inherited;
end;

procedure TFXTurtle.SetPositionAndSize;
begin
  inherited;
  ChangeAttributValue(Name + '.setOriginX(', Name + '.setOriginX(' + IntToStr(PPIUnScale(Round(OriginX))) + ');');
  ChangeAttributValue(Name + '.setOriginY(', Name + '.setOriginY(' + IntToStr(PPIUnScale(Round(OriginY))) + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXTurtle.SetFillColor(aColor: TColor);
begin
  if aColor <> FFill then begin
    FFill:= aColor;
    Invalidate;
  end;
end;

procedure TFXTurtle.SetStrokeColor(aColor: TColor);
begin
  if aColor <> FStroke then begin
    FStroke:= aColor;
    Invalidate;
  end;
end;

procedure TFXTurtle.SetAttribute(Attr, Value, Typ: string);
  var s1, s2: string;
begin
  s1:= Name + '.set' + Attr;
  s2:= '';
  if Attr = 'LineCap' then
    s2:= '(StrokeLineCap.' + Value + ');'
  else if Attr = 'LineJoin' then
    s2:= '(StrokeLineJoin.' + Value + ');';
  if s2 <> '' then begin
    s2:= s1 + s2;
    setAttributValue(s1, s2);
  end else
    inherited;
  paint;
end;


end.
