unit UAScrollPane;

interface

uses
  Classes, Forms, UJEComponents, UAComponents;

type

  TAScrollPane = class (TAWTComponent)
  private
    FDisplayPolicy: TScrollBarPolicy;
    FWheelScrollingEnabled: boolean;
    procedure setDisplayPolicy(Value: TScrollBarPolicy);
    procedure MakeScrollbars(const Attr, Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aScrollBox: TScrollBox);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property DisplayPolicy: TScrollBarPolicy read FDisplayPolicy write setDisplayPolicy;
    property WheelScrollingEnabled: boolean read FWheelScrollingEnabled write FWheelScrollingEnabled;
    property componentAdded;
    property componentRemoved;
  end;

implementation

uses Windows, Graphics, Controls, UITypes;

{--- TAScrollPane -------------------------------------------------------------}

constructor TAScrollPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= -11;
  ControlStyle := [csAcceptsControls];
  DisplayPolicy:= AS_NEEDED;
  Background:= clBtnFace;
  ShowFont:= false;
  JavaType:= 'ScrollPane';
end;

constructor TAScrollPane.CreateFrom(aScrollBox: TScrollBox);
begin
  Create(aScrollBox.Owner);
  CreateFromA(aScrollBox);
  Background:= aScrollBox.Color;
  if Background = clBtnFace then Background:= clWhite;  
end;

function TAScrollPane.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|DisplayPolicy|WheelScrollingEnabled' + inherited;
end;

procedure TAScrollPane.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'DisplayPolicy' then
    MakeScrollbars(Attr, Value)
  else
    inherited;
end;

procedure TAScrollPane.MakeScrollbars(const Attr, Value: string);
  var s1, s2: string;
begin
  s1:= 'private ScrollPane ' + Name;
  s2:= Indent1 + s1 + ' = new ScrollPane(ScrollPane.SCROLLBARS_' + Value + ');';
  Partner.ReplaceAttribute(s1, s2);
end;

function TAScrollPane.getEvents(ShowEvents: integer): string;
begin
  Result:= '|componentAdded|componentRemoved';
  if ShowEvents = 3 then
    Result:= Result + ContainerComponentEvents;
  Result:= Result + inherited;
end;

procedure TAScrollPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ScrollPane ' + Name + ' = new ScrollPane();');
end;

procedure TAScrollPane.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= clWhite;
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TwinControl).Brush.Color
    else Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Pen.Color:= RGB(160, 160, 160);
  Canvas.MoveTo(0, Height-2);
  Canvas.LineTo(0, 0);
  Canvas.LineTo(Width-1, 0);
  Canvas.Pen.Color:= AWTDarkGray;
  Canvas.MoveTo(1, Height-2);
  Canvas.LineTo(1, 1);
  Canvas.LineTo(Width-2, 1);
  Canvas.Pen.Color:= RGB(227, 227, 227);
  Canvas.LineTo(Width-2, Height-2);
  Canvas.LineTo(0, Height-2);

  // paint scrollbars
  if DisplayPolicy = ALWAYS then begin
    var p18:= PPIScale(18);
    ScrollBar(Rect(Width-p18, 2, Width-2, Height-p18), false, false);
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.FillRect(Rect(Width-p18, Height-p18, Width-2, Height-2));
    ScrollBar(Rect(2, Height-p18, Width-p18, Height-2), true, false);
  end;
end;

procedure TAScrollPane.setDisplayPolicy(Value: TScrollBarPolicy);
begin
  if FDisplayPolicy <> Value then begin
    FDisplayPolicy:= Value;
    Invalidate;
  end;
end;

end.
