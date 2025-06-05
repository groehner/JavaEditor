unit UJScrollPane;

interface

uses
  Classes,
  Forms,
  UJEComponents,
  UJComponents;

type

  TJScrollPane = class(TSwingComponent)
  private
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    FViewportView: string;
    FAutoScrolls: Boolean;
    FWheelScrollingEnabled: Boolean;
    procedure MakeScrollbarDisplayPolicy(const Attr, Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AScrollBox: TScrollBox);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property HorizontalScrollBarPolicy: TScrollBarPolicy
      read FHorizontalScrollBarPolicy write FHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy
      read FVerticalScrollBarPolicy write FVerticalScrollBarPolicy;
    property ViewportView: string read FViewportView write FViewportView;
    property Autoscrolls: Boolean read FAutoScrolls write FAutoScrolls;
    property WheelScrollingEnabled: Boolean read FWheelScrollingEnabled
      write FWheelScrollingEnabled;
    property Border;
  end;

implementation

uses
  Graphics,
  Controls;

{ --- TJScrollPane ------------------------------------------------------------- }

constructor TJScrollPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +11;
  ControlStyle := [csAcceptsControls];
  HorizontalScrollBarPolicy := AS_NEEDED;
  VerticalScrollBarPolicy := AS_NEEDED;
  WheelScrollingEnabled := True;
  ShowFont := False;
  JavaType := 'JScrollPane';
end;

constructor TJScrollPane.CreateFrom(AScrollBox: TScrollBox);
begin
  Create(AScrollBox.Owner);
  CreateFromJ(AScrollBox);
  Background := AScrollBox.Color;
  ViewportView := AScrollBox.HelpKeyword;
end;

function TJScrollPane.GetAttributes(ShowAttributes: Integer): string;
const
  Scroll1 = '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
  Scroll2 = '|ViewportView|Autoscrolls|WheelScrollingEnabled|Border';
begin
  if ShowAttributes = 1 then
    Result := Scroll1
  else
    Result := Scroll1 + Scroll2;
  Result := Result + inherited;
end;

procedure TJScrollPane.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy')
  then
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
  if Attr = 'HorizontalScrollBarPolicy' then
    MakeAttribut(Attr, 'ScrollPaneConstants.HORIZONTAL_SCROLLBAR_' + Value)
  else
    MakeAttribut(Attr, 'ScrollPaneConstants.VERTICAL_SCROLLBAR_' + Value);
end;

procedure TJScrollPane.Paint;
begin
  CanvasFontAssign;
  if Border.BorderType = NoBorder then
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.Brush.Color := DefaultBackground;
    Canvas.Rectangle(ClientRect);
    Canvas.Brush.Color := DarkShadow;
    Canvas.FrameRect(Rect(0, 0, Width - 1, Height - 1));
  end
  else
    Border.Show(Self, Canvas);

  // paint scrollbars
  var
  P17 := PPIScale(17);
  if (VerticalScrollBarPolicy = ALWAYS) and (HorizontalScrollBarPolicy = ALWAYS)
  then
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.FillRect(Rect(Width - P17, Height - P17, Width - 2, Height - 2));
    Scrollbar(Rect(Width - P17, 1, Width - 2, Height - P17), False, True);
    // vsb
    Scrollbar(Rect(1, Height - P17, Width - P17, Height - 2), True, True);
    // hsb
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - P17);
    Canvas.LineTo(Width - P17, Height - P17);
    Canvas.LineTo(Width - P17, 0);
  end
  else if (HorizontalScrollBarPolicy = ALWAYS) then
  begin
    Scrollbar(Rect(1, Height - P17, Width - 2, Height - 2), True, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - P17);
    Canvas.LineTo(Width - 1, Height - P17);
  end
  else if (VerticalScrollBarPolicy = ALWAYS) then
  begin
    Scrollbar(Rect(Width - P17, 1, Width - 2, Height - 2), False, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(Width - P17, Height - P17);
    Canvas.LineTo(Width - P17, 0);
  end;
end;

end.
