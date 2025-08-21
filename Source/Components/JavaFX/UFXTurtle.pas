unit UFXTurtle;

interface

uses
  Classes,
  Graphics,
  UFXCanvas;

type

  TLineCap = (_TA_BUTT, _TA_ROUND, _TA_SQUARE);
  TLineJoin = (_CD_BEVEL, _CD_MITER, _CD_ROUND);

  TFXTurtle = class(TFXCanvas)
  private
    FAnimated: Boolean;
    FAnimationspeed: Double;
    FTurtleX: Double;
    FTurtleY: Double;
    FTurtleW: Double;
    FOriginX: Double;
    FOriginY: Double;
    FFill: TColor;
    FStroke: TColor;
    FLineCap: TLineCap;
    FLineJoin: TLineJoin;
    FLineWidth: Double;
    FMiterLimit: Double;
    FSmooth: Boolean;
    FLineDashOffset: Double;
    FVisibleImage: Boolean;
    procedure SetFill(AColor: TColor);
    procedure SetStroke(AColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property Animated: Boolean read FAnimated write FAnimated;
    property Animationspeed: Double read FAnimationspeed write FAnimationspeed;
    property TurtleX: Double read FTurtleX write FTurtleX;
    property TurtleY: Double read FTurtleY write FTurtleY;
    property TurtleW: Double read FTurtleW write FTurtleW;

    property OriginX: Double read FOriginX write FOriginX;
    property OriginY: Double read FOriginY write FOriginY;
    property Fill: TColor read FFill write SetFill;
    property Stroke: TColor read FStroke write SetStroke;
    property LineCap: TLineCap read FLineCap write FLineCap;
    property LineJoin: TLineJoin read FLineJoin write FLineJoin;
    property LineWidth: Double read FLineWidth write FLineWidth;
    property LineDashOffset: Double read FLineDashOffset write FLineDashOffset;
    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property Smooth: Boolean read FSmooth write FSmooth;
    property VisibleImage: Boolean read FVisibleImage write FVisibleImage;
  end;

implementation

uses
  SysUtils,
  UUtils,
  UObjectInspector,
  UGUIDesigner,
  UFXComponents;

{ --- TFXCanvas ----------------------------------------------------------------- }

constructor TFXTurtle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 120;
  Height := 80;
  Tag := 120;
  FOriginX := Width div 2;
  FOriginY := Height div 2;
  FAnimated := True;
  FAnimationspeed := 100;
  FFill := clWhite;
  FStroke := clWhite;
  FLineCap := _TA_ROUND;
  FLineJoin := _CD_ROUND;
  FLineWidth := 1;
  FMiterLimit := 10;
  FSmooth := True;
  FLineDashOffset := 0;
  FVisibleImage := True;
  JavaType := 'Turtle';
end;

procedure TFXTurtle.Paint;
begin
  Canvas.Pen.Color := Stroke;
  Canvas.Brush.Color := Fill;
  Canvas.Rectangle(0, 0, Width, Height);
  var
  ABitmap := TBitmap.Create;
  ABitmap.Transparent := True;
  FGUIDesigner.vilFXTurtle.GetBitmap(0, ABitmap);
  RotateBitmap(ABitmap, -TurtleW * 2 * Pi / 360, True);
  Canvas.Draw(Round(OriginX + TurtleX - ABitmap.Width / 2),
    Round(OriginY - TurtleY - ABitmap.Height / 2), aBitmap);
  FreeAndNil(ABitmap);
end;

procedure TFXTurtle.NewControl;
var
  Key, Str: string;
begin
  DefaultComponent;
  InsertImport('je.fx.util.Turtle');
  InsertImport('javafx.scene.canvas.*');
  InsertImport('javafx.scene.shape.*');
  InsertImport('javafx.scene.paint.*');
  InsertNewVariable('private Turtle ' + Name + ' = new Turtle();');
  // set animated must be last of all turtle statements, so no animation during setup
  Key := Name + '.setAnimated';
  Str := Name + '.setAnimated(true);';
  if Pos(Indent2, Str) = 0 then
    Str := Indent2 + Str;
  FPartner.SetAttributValue(GetContainerAdd, Key, Str, 1);
end;

function TFXTurtle.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 =
    '|Animated|TurtleX|TurtleY|TurtleW|OriginX|OriginY|Fill|Stroke|LineWidth|VisibleImage';
  Attributes2 = Attributes1 +
    '|Animationspeed|LineCap|LineJoin|LineDashOffset|MiterLimit|Smooth';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited
  else
    Result := Attributes2 + inherited;
end;

procedure TFXTurtle.SetPositionAndSize;
begin
  inherited;
  ChangeAttributValue(Name + '.setOriginX(', Name + '.setOriginX(' +
    IntToStr(PPIUnScale(Round(OriginX))) + ');');
  ChangeAttributValue(Name + '.setOriginY(', Name + '.setOriginY(' +
    IntToStr(PPIUnScale(Round(OriginY))) + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXTurtle.SetFill(AColor: TColor);
begin
  if AColor <> FFill then
  begin
    FFill := AColor;
    Invalidate;
  end;
end;

procedure TFXTurtle.SetStroke(AColor: TColor);
begin
  if AColor <> FStroke then
  begin
    FStroke := AColor;
    Invalidate;
  end;
end;

procedure TFXTurtle.SetAttribute(Attr, Value, Typ: string);
var
  Str1, Str2: string;
begin
  Str1 := Name + '.set' + Attr;
  Str2 := '';
  if Attr = 'LineCap' then
    Str2 := '(StrokeLineCap.' + Value + ');'
  else if Attr = 'LineJoin' then
    Str2 := '(StrokeLineJoin.' + Value + ');';
  if Str2 <> '' then
  begin
    Str2 := Str1 + Str2;
    SetAttributValue(Str1, Str2);
  end
  else
    inherited;
  Paint;
end;

end.
