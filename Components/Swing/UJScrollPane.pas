unit UJScrollPane;

interface

uses
  Classes, Forms, UJEComponents, UJComponents;

type

  TJScrollPane = class (TSwingComponent)
  private
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    FViewportView: string;
    FAutoScrolls: boolean;
    FWheelScrollingEnabled: boolean;
    procedure MakeScrollbarDisplayPolicy(const Attr, Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aScrollBox: TScrollBox);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property HorizontalScrollBarPolicy: TScrollBarPolicy read FHorizontalScrollBarPolicy write FHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy read FVerticalScrollBarPolicy write FVerticalScrollBarPolicy;
    property ViewportView: string read FViewportView write FViewportView;
    property Autoscrolls: boolean read FAutoScrolls write FAutoScrolls;
    property WheelScrollingEnabled: boolean read FWheelScrollingEnabled write FWheelScrollingEnabled;
    property Border;
  end;

implementation

uses Graphics, Controls;

{--- TJScrollPane -------------------------------------------------------------}

constructor TJScrollPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +11;
  ControlStyle := [csAcceptsControls];
  HorizontalScrollBarPolicy:= AS_NEEDED;
  VerticalScrollBarPolicy:= AS_NEEDED;
  WheelScrollingEnabled:= true;
  ShowFont:= false;
  JavaType:= 'JScrollPane';
end;

constructor TJScrollPane.CreateFrom(aScrollBox: TScrollBox);
begin
  Create(aScrollBox.Owner);
  CreateFromJ(aScrollBox);
  Background:= aScrollBox.Color;
  ViewPortView:= aScrollBox.HelpKeyword;
end;

function TJScrollPane.getAttributes(ShowAttributes: integer): string;
  const
    scroll1 = '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
    scroll2 = '|ViewportView|Autoscrolls|WheelScrollingEnabled|Border';
begin
  if ShowAttributes = 1
    then Result:= scroll1
    else Result:= scroll1 + scroll2;
  Result:= Result + inherited;
end;

procedure TJScrollPane.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else if Attr = 'ViewportView' then
    MakeAttribut(Attr, Value)
  else
    inherited;
end;

procedure TJScrollPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JScrollPane ' + Name + ' = new JScrollPane();');
end;

procedure TJScrollPane.MakeScrollbarDisplayPolicy(const Attr, Value: string);
begin
  if Attr = 'HorizontalScrollBarPolicy'
   then MakeAttribut(Attr, 'ScrollPaneConstants.HORIZONTAL_SCROLLBAR_' + Value)
   else MakeAttribut(Attr, 'ScrollPaneConstants.VERTICAL_SCROLLBAR_' + Value);
end;

procedure TJScrollPane.Paint;
begin
  CanvasFontAssign;
  if Border.Bordertype = NoBorder then begin
    Canvas.Pen.Color:= clWhite;
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.Rectangle(ClientRect);
    Canvas.Brush.Color:= DarkShadow;
    Canvas.FrameRect(Rect(0, 0, Width-1, Height-1));
  end else
    Border.Show(Self, Canvas);

  // paint scrollbars
  var p17:= PPIScale(17);
  if (VerticalScrollBarPolicy = ALWAYS) and (HorizontalScrollBarPolicy = ALWAYS) then begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.FillRect(Rect(Width-p17, Height-p17, Width-2, Height-2));
    ScrollBar(Rect(Width-p17, 1, Width-2, Height-p17), false, true); // vsb
    ScrollBar(Rect(1, Height-p17, Width-p17, Height-2), true, true); // hsb
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-p17);
    Canvas.LineTo(Width-p17, Height-p17);
    Canvas.LineTo(Width-p17, 0);
  end else if (HorizontalScrollBarPolicy = ALWAYS) then begin
    ScrollBar(Rect(1, Height-p17, Width-2, Height-2), true, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-p17);
    Canvas.LineTo(Width-1, Height-p17);
  end else if  (VerticalScrollBarPolicy = ALWAYS) then begin
    ScrollBar(Rect(Width-p17, 1, Width-2, Height-2), false, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(Width-p17, Height-p17);
    Canvas.LineTo(Width-p17, 0);
  end;
end;

end.
