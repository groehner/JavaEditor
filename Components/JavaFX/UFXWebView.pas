unit UFXWebView;

interface

uses
  Classes, UFXComponents;

type
  TFontSmoothingType = (GRAY, LCD);

  TFXWebView = class (TFXNode)
  private
    FContextMenuEnabled: boolean;
    FFontScale: double;
    FFontSmoothingType: TFontSmoothingType;
    FURL: string;
    FPrefHeight: double;
    FPrefWidth: double;
    FZoom: double;
    procedure setPrefWidth(aValue: double);
    function getPrefWidth: double;
    procedure setPrefHeight(aValue: double);
    function getPrefHeight: double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure SetPositionAndSize; override;
  published
    property ContextMenuEnabled: boolean read FContextMenuEnabled write FContextMenuEnabled;
    property FontScale: double read FFontScale write FFontScale;
    property FontSmoothingType: TFontSmoothingType read FFontSmoothingType write FFontSmoothingType;
    property URL: string read FUrl write FUrl;
    property PrefHeight: double read getPrefHeight write setPrefHeight;
    property PrefWidth: double read getPrefWidth write setPrefWidth;
    property Zoom: double read FZoom write FZoom;
  end;

implementation

uses Graphics, Controls, SysUtils;

{--- TFXWebView ---------------------------------------------------------------}

constructor TFXWebView.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +148;
  FZoom:= 1;
  FFontScale:= 1;
  JavaType:= 'WebView';
end;

procedure TFXWebView.Paint;
begin
  Canvas.Brush.Color:= clWhite;
  Canvas.Pen.Color:= clWhite;
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
    ChangeAttributValue(Name + '.getEngine(', Indent2 + Name +  '.getEngine().load("' + Value + '");')
  else
    inherited;
end;

function TFXWebView.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|PrefHeight|PrefWidth|URL';
        Attributes2 = Attributes1 + '|Zoom|FontScale|ContextMenuEnabled';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes);
end;

procedure TFXWebView.setPrefWidth(aValue: double);
begin
  if aValue <> FPrefWidth then begin
    FPrefWidth:= aValue;
    Width:= round(FPrefWidth);
    Invalidate;
  end;
end;

function TFXWebView.getPrefWidth: double;
begin
  Result:= Width;
end;

procedure TFXWebView.setPrefHeight(aValue: double);
begin
  if aValue <> FPrefHeight then begin
    FPrefHeight:= aValue;
    Height:= round(FPrefHeight);
    Invalidate;
  end;
end;

function TFXWebView.getPrefHeight: double;
begin
  Result:= Height;
end;

procedure TFXWebView.SetPositionAndSize;
begin
  MakeAttribut('LayoutX', IntToStr(Left));
  MakeAttribut('LayoutY', IntToStr(Top));
  MakeAttribut('PrefHeight', IntToStr(Height));
  MakeAttribut('PrefWidth', IntToStr(Width));
end;

end.
