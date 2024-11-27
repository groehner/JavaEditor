unit UFXText;

interface

uses Classes, UFXComponents, UFXShape;

type

  TBoundsType = (LOGICAL, LOGICAL_VERTICAL_CENTER, VISUAL);

  TFontSmoothingType = (GRAY, LCD);

  TVPos = (_CD_BASELINE, _CD_BOTTOM, _CD_CENTER, _CD_TOP);

  TFXText = class (TFXShape)
  private
    FBoundsType: TBoundsType;
    FFontSmoothingType: TFontSmoothingType;
    FLineSpacing: integer;
    FStrikethrough: boolean;
    FText: string;
    FTextAlignment: TTextAlignment;
    FTextOrigin: TVPos;
    FUnderline: boolean;
    FWrappingWidth: double;
    FX: double;
    FY: double;
    procedure setLineSpacing(aValue: integer);
    procedure setUnderline(aValue: boolean);
    procedure setText(const aValue: string);
    procedure setTextAlignment(aValue: TTextAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property BoundsType: TBoundsType read FBoundsType write FBoundsType;
    property FontSmoothingType: TFontSmoothingType read FFontSmoothingType write FFontSmoothingType;
    property LineSpacing: integer read FLineSpacing write setLineSpacing;
    property Strikethrough: boolean read FStrikethrough write FStrikethrough;
    property Text: string read FText write setText;
    property TextAlignment: TTextAlignment read FTextAlignment write setTextAlignment;
    property TextOrigin: TVPos read FTextOrigin write FTextOrigin;
    property Underline: boolean read FUnderline write setUnderline;
    property WrappingWidth: double read FWrappingWidth write FWrappingWidth;
    property X: double read FX write FX;
    property Y: double read FY write FY;
  end;

implementation

uses  Graphics, SysUtils;

{--- TFXText ------------------------------------------------------------------}

constructor TFXText.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +168;
  JavaType:= 'Text';
end;

procedure TFXText.Paint;
begin
  CanvasFontAssign;
  if Stroke = -16777201
    then Canvas.Font.Color:= Fill
  else begin
    Canvas.Font.Color:= Stroke;
    Canvas.Font.Style:= [fsBold];
  end;
  Canvas.TextOut(0, 0, Text);
end;

procedure TFXText.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private Text ' + Name + ' = new Text();');
  InsertImport('javafx.scene.text.*');
  MakeFont;
end;

procedure TFXText.SetPositionAndSize;
begin
  X:= Left;
  Y:= Top;
  MakeAttribut('X', Format('%g',[X]));
  MakeAttribut('Y', Format('%g',[Y]));
end;

function TFXText.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Font|Text';
        Attributes2 =  Attributes1 +
                      '|BoundsType|FontSmoothingType|LineSpacing|Strikethrough|TextAlignment' +
                      '|TextOrigin|Underline|WrappingWidth|X|Y';
begin
  if ShowAttributes = 1
    then Result:= Attributes1
    else Result:= Attributes2;
  Result:= Result + inherited getAttributes(ShowAttributes);
  var p:= Pos('|LayoutX|LayoutY', Result);
  if p > 0 then
    delete(Result, p, 16);
end;

procedure TFXText.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'TextAlignment' then begin
    InsertImport('javafx.scene.text.*');
    MakeAttribut(Attr, 'TextAlignment.' + Value);
  end else if Attr = 'TextOrigin' then begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'VPos.' + Value)
  end else
    inherited;
end;

procedure TFXText.setLineSpacing(aValue: integer);
begin
  if aValue <> FLineSpacing then begin
    FLineSpacing:= aValue;
    Invalidate;
  end;
end;

procedure TFXText.setUnderline(aValue: boolean);
begin
  if aValue <> FUnderline then begin
    FUnderline:= aValue;
    Invalidate;
  end;
end;

procedure TFXText.setTextAlignment(aValue: TTextAlignment);
begin
  if aValue <> FTextAlignment then begin
    FTextAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TFXText.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

end.
