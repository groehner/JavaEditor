unit UFXScrollPane;

interface

uses
  Classes, UJEComponents, UFXComponents;

type

  TFXScrollPane = class (TFXControl)
  private
    FContent: string;
    FFitToHeight: boolean;
    FFitToWidth: boolean;
    FHbarPolicy: TScrollBarPolicy;
    FVbarPolicy: TScrollBarPolicy;
    FHmax: double;
    FHMin: double;
    FHvalue: double;
    FMinViewportHeight: double;
    FMinViewportWidth: double;
    FPannable: boolean;
    FPrefViewportHeight: double;
    FPrefViewportWidth: double;
    FVMax: double;
    FVmin: double;
    FVvalue: double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
  published
    property Content: string read FContent write FContent;
    property FitToHeight: boolean read FFitToHeight write FFitToHeight;
    property FitToWidth: boolean read FFitToWidth write FFitToWidth;
    property HbarPolicy: TScrollBarPolicy read FHbarPolicy write FHbarPolicy;
    property VbarPolicy: TScrollBarPolicy read FVbarPolicy write FVbarPolicy;
    property Hmax: double read FHmax write FHmax;
    property HMin: double read FHMin write FHMin;
    property Hvalue: double read FHvalue write FHvalue;
    property MinViewportHeight: double read FMinViewportHeight write FMinViewportHeight;
    property MinViewportWidth: double read FMinViewportWidth write FMinViewportWidth;
    property Pannable: boolean read FPannable write FPannable;
    property PrefViewportHeight: double read FPrefViewportHeight write FPrefViewportHeight;
    property PrefViewportWidth: double read FPrefViewportWidth write FPrefViewportWidth;
    property VMax: double read FVMax write FVMax;
    property Vmin: double read FVmin write FVmin;
    property Vvalue: double read FVvalue write FVvalue;
   // property Border;
  end;

implementation

uses Graphics, Controls;

{--- TFXScrollPane -------------------------------------------------------------}

constructor TFXScrollPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +113;
  PrefWidth:= 120;
  PrefHeight:= 80;
  ControlStyle := [csAcceptsControls];
  HbarPolicy:= AS_NEEDED;
  VbarPolicy:= AS_NEEDED;
  JavaType:= 'ScrollPane';
end;

procedure TFXScrollPane.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= DefaultBackground;
  Canvas.Rectangle(ClientRect);
  Canvas.Brush.Color:= DarkShadow;
  Canvas.FrameRect(Rect(0, 0, Width-1, Height-1));
end;

procedure TFXScrollPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ScrollPane '  + Name + ' = new ScrollPane();');
end;

procedure TFXScrollPane.SetAttribute(Attr, Value, Typ: string);
  var s1, s2: string;
begin
  if Typ = 'TScrollBarPolicy' then begin
    s1:= Name + '.set' + Attr;
    s2:=  s1 + '(ScrollPane.ScrollBarPolicy.' + Value + ');';
    setAttributValue(s1, s2);
  end else begin
    if Attr = 'Content' then
      Typ:= 'Identifier';
    inherited;
  end;
end;

function TFXScrollPane.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Content|HbarPolicy|VbarPolicy|Pannable|Name';
        Attributes2 = Attributes1 +
                      '|FitToHeight|FitToWidth|HMax|HMin|Hvalue|MinViewportHeight' +
                      '|MinViewportWidth|PrefViewportHeight|PrefViewportWidth'+
                      '|VMax|Vmin|Vvalue';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes);
end;


end.
