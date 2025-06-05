unit UFXScrollPane;

interface

uses
  Classes, UJEComponents, UFXComponents;

type

  TFXScrollPane = class (TFXControl)
  private
    FContent: string;
    FFitToHeight: Boolean;
    FFitToWidth: Boolean;
    FHbarPolicy: TScrollBarPolicy;
    FVbarPolicy: TScrollBarPolicy;
    FHmax: Double;
    FHMin: Double;
    FHvalue: Double;
    FMinViewportHeight: Double;
    FMinViewportWidth: Double;
    FPannable: Boolean;
    FPrefViewportHeight: Double;
    FPrefViewportWidth: Double;
    FVMax: Double;
    FVmin: Double;
    FVvalue: Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure MakeFont; override;
  published
    property Content: string read FContent write FContent;
    property FitToHeight: Boolean read FFitToHeight write FFitToHeight;
    property FitToWidth: Boolean read FFitToWidth write FFitToWidth;
    property HbarPolicy: TScrollBarPolicy read FHbarPolicy write FHbarPolicy;
    property VbarPolicy: TScrollBarPolicy read FVbarPolicy write FVbarPolicy;
    property Hmax: Double read FHmax write FHmax;
    property HMin: Double read FHMin write FHMin;
    property Hvalue: Double read FHvalue write FHvalue;
    property MinViewportHeight: Double read FMinViewportHeight write FMinViewportHeight;
    property MinViewportWidth: Double read FMinViewportWidth write FMinViewportWidth;
    property Pannable: Boolean read FPannable write FPannable;
    property PrefViewportHeight: Double read FPrefViewportHeight write FPrefViewportHeight;
    property PrefViewportWidth: Double read FPrefViewportWidth write FPrefViewportWidth;
    property VMax: Double read FVMax write FVMax;
    property Vmin: Double read FVmin write FVmin;
    property Vvalue: Double read FVvalue write FVvalue;
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
  var Str1, Str2: string;
begin
  if Typ = 'TScrollBarPolicy' then begin
    Str1:= Name + '.set' + Attr;
    Str2:=  Str1 + '(ScrollPane.ScrollBarPolicy.' + Value + ');';
    SetAttributValue(Str1, Str2);
  end else begin
    if Attr = 'Content' then
      Typ:= 'Identifier';
    inherited;
  end;
end;

procedure TFXScrollPane.MakeFont;
begin
  // no font
end;

function TFXScrollPane.GetAttributes(ShowAttributes: Integer): string;
  const Attributes1 = '|Content|HbarPolicy|VbarPolicy|Pannable|Name';
        Attributes2 = Attributes1 +
                      '|FitToHeight|FitToWidth|HMax|HMin|Hvalue|MinViewportHeight' +
                      '|MinViewportWidth|PrefViewportHeight|PrefViewportWidth'+
                      '|VMax|Vmin|Vvalue';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited GetAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited GetAttributes(ShowAttributes);
end;


end.
