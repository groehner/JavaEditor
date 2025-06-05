unit UFXWebView;

interface

uses
  Classes,
  UFXComponents;

type
  TFontSmoothingType = (GRAY, LCD);

  TFXWebView = class(TFXNode)
  private
    FContextMenuEnabled: Boolean;
    FFontScale: Double;
    FFontSmoothingType: TFontSmoothingType;
    FURL: string;
    FPrefHeight: Double;
    FPrefWidth: Double;
    FZoom: Double;
    procedure SetPrefWidth(AValue: Double);
    function GetPrefWidth: Double;
    procedure SetPrefHeight(AValue: Double);
    function GetPrefHeight: Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetPositionAndSize; override;
  published
    property ContextMenuEnabled: Boolean read FContextMenuEnabled
      write FContextMenuEnabled;
    property FontScale: Double read FFontScale write FFontScale;
    property FontSmoothingType: TFontSmoothingType read FFontSmoothingType
      write FFontSmoothingType;
    property URL: string read FURL write FURL;
    property PrefHeight: Double read GetPrefHeight write SetPrefHeight;
    property PrefWidth: Double read GetPrefWidth write SetPrefWidth;
    property Zoom: Double read FZoom write FZoom;
  end;

implementation

uses
  Graphics,
  Controls,
  SysUtils;

{ --- TFXWebView --------------------------------------------------------------- }

constructor TFXWebView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +148;
  FZoom := 1;
  FFontScale := 1;
  JavaType := 'WebView';
end;

procedure TFXWebView.Paint;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
end;

procedure TFXWebView.NewControl;
begin
  InsertImport('javafx.scene.web.*');
  InsertNewVariable('private WebView ' + Name + ' = new WebView();');
  DefaultComponent;
end;

procedure TFXWebView.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'URL' then
    ChangeAttributValue(Name + '.getEngine(',
      Indent2 + Name + '.getEngine().load("' + Value + '");')
  else
    inherited;
end;

function TFXWebView.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|PrefHeight|PrefWidth|URL';
  Attributes2 = Attributes1 + '|Zoom|FontScale|ContextMenuEnabled';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
end;

procedure TFXWebView.SetPrefWidth(AValue: Double);
begin
  if AValue <> FPrefWidth then
  begin
    FPrefWidth := AValue;
    Width := Round(FPrefWidth);
    Invalidate;
  end;
end;

function TFXWebView.GetPrefWidth: Double;
begin
  Result := Width;
end;

procedure TFXWebView.SetPrefHeight(AValue: Double);
begin
  if AValue <> FPrefHeight then
  begin
    FPrefHeight := AValue;
    Height := Round(FPrefHeight);
    Invalidate;
  end;
end;

function TFXWebView.GetPrefHeight: Double;
begin
  Result := Height;
end;

procedure TFXWebView.SetPositionAndSize;
begin
  MakeAttribut('LayoutX', IntToStr(Left));
  MakeAttribut('LayoutY', IntToStr(Top));
  MakeAttribut('PrefHeight', IntToStr(Height));
  MakeAttribut('PrefWidth', IntToStr(Width));
end;

end.
