unit UAScrollPane;

interface

uses
  Classes,
  Forms,
  UJEComponents,
  UAComponents;

type

  TAScrollPane = class(TAWTComponent)
  private
    FDisplayPolicy: TScrollBarPolicy;
    FWheelScrollingEnabled: Boolean;
    procedure SetDisplayPolicy(Value: TScrollBarPolicy);
    procedure MakeScrollbars(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property DisplayPolicy: TScrollBarPolicy read FDisplayPolicy
      write SetDisplayPolicy;
    property WheelScrollingEnabled: Boolean read FWheelScrollingEnabled
      write FWheelScrollingEnabled;
    property componentAdded;
    property componentRemoved;
  end;

implementation

uses
  Windows,
  Graphics,
  Controls,
  UITypes;

{ --- TAScrollPane ------------------------------------------------------------- }

constructor TAScrollPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -11;
  ControlStyle := [csAcceptsControls];
  DisplayPolicy := AS_NEEDED;
  Background := clBtnFace;
  ShowFont := False;
  JavaType := 'ScrollPane';
end;

function TAScrollPane.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|DisplayPolicy|WheelScrollingEnabled' + inherited;
end;

procedure TAScrollPane.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'DisplayPolicy' then
    MakeScrollbars(Value)
  else
    inherited;
end;

procedure TAScrollPane.MakeScrollbars(const Value: string);
var
  Str1, Str2: string;
begin
  Str1 := 'private ScrollPane ' + Name;
  Str2 := Indent1 + Str1 + ' = new ScrollPane(ScrollPane.SCROLLBARS_' +
    Value + ');';
  FPartner.ReplaceAttribute(Str1, Str2);
end;

function TAScrollPane.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|componentAdded|componentRemoved';
  if ShowEvents = 3 then
    Result := Result + ContainerComponentEvents;
  Result := Result + inherited;
end;

procedure TAScrollPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ScrollPane ' + Name + ' = new ScrollPane();');
end;

procedure TAScrollPane.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := clWhite;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Pen.Color := RGB(160, 160, 160);
  Canvas.MoveTo(0, Height - 2);
  Canvas.LineTo(0, 0);
  Canvas.LineTo(Width - 1, 0);
  Canvas.Pen.Color := AWTDarkGray;
  Canvas.MoveTo(1, Height - 2);
  Canvas.LineTo(1, 1);
  Canvas.LineTo(Width - 2, 1);
  Canvas.Pen.Color := RGB(227, 227, 227);
  Canvas.LineTo(Width - 2, Height - 2);
  Canvas.LineTo(0, Height - 2);

  // paint scrollbars
  if DisplayPolicy = ALWAYS then
  begin
    var
    P18 := PPIScale(18);
    Scrollbar(Rect(Width - P18, 2, Width - 2, Height - P18), False, False);
    Canvas.Brush.Color := DefaultBackground;
    Canvas.FillRect(Rect(Width - P18, Height - P18, Width - 2, Height - 2));
    Scrollbar(Rect(2, Height - P18, Width - P18, Height - 2), True, False);
  end;
end;

procedure TAScrollPane.SetDisplayPolicy(Value: TScrollBarPolicy);
begin
  if FDisplayPolicy <> Value then
  begin
    FDisplayPolicy := Value;
    Invalidate;
  end;
end;

end.
