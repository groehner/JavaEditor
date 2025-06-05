unit UFXText;

interface

uses
  Classes,
  UFXComponents,
  UFXShape;

type

  TBoundsType = (LOGICAL, LOGICAL_VERTICAL_CENTER, VISUAL);

  TFontSmoothingType = (GRAY, LCD);

  TVPos = (_CD_BASELINE, _CD_BOTTOM, _CD_CENTER, _CD_TOP);

  TFXText = class(TFXShape)
  private
    FBoundsType: TBoundsType;
    FFontSmoothingType: TFontSmoothingType;
    FLineSpacing: Integer;
    FStrikethrough: Boolean;
    FText: string;
    FTextAlignment: TTextAlignment;
    FTextOrigin: TVPos;
    FUnderline: Boolean;
    FWrappingWidth: Double;
    FXPos: Double;
    FYPos: Double;
    procedure SetLineSpacing(AValue: Integer);
    procedure SetUnderline(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetTextAlignment(AValue: TTextAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
  published
    property BoundsType: TBoundsType read FBoundsType write FBoundsType;
    property FontSmoothingType: TFontSmoothingType read FFontSmoothingType
      write FFontSmoothingType;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing;
    property Strikethrough: Boolean read FStrikethrough write FStrikethrough;
    property Text: string read FText write SetText;
    property TextAlignment: TTextAlignment read FTextAlignment
      write SetTextAlignment;
    property TextOrigin: TVPos read FTextOrigin write FTextOrigin;
    property Underline: Boolean read FUnderline write SetUnderline;
    property WrappingWidth: Double read FWrappingWidth write FWrappingWidth;
    property X: Double read FXPos write FXPos;
    property Y: Double read FYPos write FYPos;
  end;

implementation

uses
  Graphics,
  SysUtils;

{ --- TFXText ------------------------------------------------------------------ }

constructor TFXText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +168;
  JavaType := 'Text';
end;

procedure TFXText.Paint;
begin
  CanvasFontAssign;
  if Stroke = -16777201 then
    Canvas.Font.Color := Fill
  else
  begin
    Canvas.Font.Color := Stroke;
    Canvas.Font.Style := [fsBold];
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
  X := Left;
  Y := Top;
  MakeAttribut('X', Format('%g', [X]));
  MakeAttribut('Y', Format('%g', [Y]));
end;

function TFXText.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Font|Text';
  Attributes2 = Attributes1 +
    '|BoundsType|FontSmoothingType|LineSpacing|Strikethrough|TextAlignment' +
    '|TextOrigin|Underline|WrappingWidth|X|Y';
begin
  if ShowAttributes = 1 then
    Result := Attributes1
  else
    Result := Attributes2;
  Result := Result + inherited GetAttributes(ShowAttributes);
  var
  Posi := Pos('|LayoutX|LayoutY', Result);
  if Posi > 0 then
    Delete(Result, Posi, 16);
end;

procedure TFXText.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'TextAlignment' then
  begin
    InsertImport('javafx.scene.text.*');
    MakeAttribut(Attr, 'TextAlignment.' + Value);
  end
  else if Attr = 'TextOrigin' then
  begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'VPos.' + Value);
  end
  else
    inherited;
end;

procedure TFXText.SetLineSpacing(AValue: Integer);
begin
  if AValue <> FLineSpacing then
  begin
    FLineSpacing := AValue;
    Invalidate;
  end;
end;

procedure TFXText.SetUnderline(AValue: Boolean);
begin
  if AValue <> FUnderline then
  begin
    FUnderline := AValue;
    Invalidate;
  end;
end;

procedure TFXText.SetTextAlignment(AValue: TTextAlignment);
begin
  if AValue <> FTextAlignment then
  begin
    FTextAlignment := AValue;
    Invalidate;
  end;
end;

procedure TFXText.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

end.
