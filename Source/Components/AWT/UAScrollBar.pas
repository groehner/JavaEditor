unit UAScrollBar;

interface

uses
  Classes,
  StdCtrls,
  UJEComponents,
  UAComponents;

type

  TAScrollBar = class(TAWTComponent)
  private
    FBlockIncrement: Integer;
    FMaximum: Integer;
    FMinimum: Integer;
    FUnitIncrement: Integer;
    FVisibleAmount: Integer;
    FValue: Integer;
    FOrientation: TOrientation;
    procedure SetOrientation(AValue: TOrientation);
    procedure SetValue(AValue: Integer);
    procedure SetVisibleAmount(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AScrollBar: TScrollBar);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property BlockIncrement: Integer read FBlockIncrement write FBlockIncrement
      default 10;
    property Maximum: Integer read FMaximum write FMaximum default 100;
    property Minimum: Integer read FMinimum write FMinimum default 0;
    property Value: Integer read FValue write SetValue;
    property UnitIncrement: Integer read FUnitIncrement write FUnitIncrement;
    property VisibleAmount: Integer read FVisibleAmount write SetVisibleAmount;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property adjustmentValueChanged;
  end;

implementation

uses
  Windows,
  Forms,
  Types,
  Graphics,
  Controls,
  UGUIDesigner;

{ --- TAScrollBar -------------------------------------------------------------- }

constructor TAScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -10;
  Width := 120;
  Height := 24;
  BlockIncrement := 10;
  UnitIncrement := 1;
  Value := 0;
  VisibleAmount := 10;
  Minimum := 0;
  Maximum := 100;
  Foreground := DarkShadow;
  Background := RGB(200, 200, 200); // DefaultBackground
  Orientation := HORIZONTAL;
  ShowFont := False;
  JavaType := 'Scrollbar';
end;

constructor TAScrollBar.CreateFrom(AScrollBar: TScrollBar);
begin
  Create(AScrollBar.Owner);
  CreateFromA(AScrollBar);
  Maximum := AScrollBar.Max;
  Minimum := AScrollBar.Min;
  Value := AScrollBar.Position;
  if AScrollBar.Kind = sbVertical then
    Orientation := VERTICAL
  else
    Orientation := HORIZONTAL;
end;

function TAScrollBar.GetAttributes(ShowAttributes: Integer): string;
const
  Scroll1 = '|Value|Minimum|Maximum|Orientation';
  Scroll2 = '|BlockIncrement|UnitIncrement|VisibleAmount';
begin
  if ShowAttributes = 1 then
    Result := Scroll1
  else
    Result := Scroll1 + Scroll2;
  Result := Result + inherited;
end;

procedure TAScrollBar.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Orientation' then
  begin
    MakeAttribut(Attr, 'Scrollbar.' + Value);
    SetPositionAndSize;
  end
  else
    inherited;
end;

function TAScrollBar.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|adjustmentValueChanged' + inherited GetEvents(ShowEvents);
end;

procedure TAScrollBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Scrollbar ' + Name + ' = new Scrollbar();');
  MakeAttribut('Orientation', 'Scrollbar.HORIZONTAL');
end;

procedure TAScrollBar.Paint;
var
  X0val, X1val, X2val, Y0val, Y1val, Y2val, DeltaX, DeltaY: Integer;
begin
  // left-right
  CanvasFontAssign;
  Canvas.Brush.Color := RGB(234, 234, 234);
  Canvas.FillRect(Rect(0, 0, Width, Height));
  var
  P16 := PPIScale(16);
  if Orientation = HORIZONTAL then
  begin
    Canvas.Brush.Color := Background;
    Canvas.FillRect(Rect(P16, 0, Width - P16, Height));
    FGUIDesigner.vilControls1616.Draw(Canvas, 0, (Height - P16) div 2, 3);
    FGUIDesigner.vilControls1616.Draw(Canvas, Width - P16,
      (Height - P16) div 2, 2);
    // scroller
    DeltaX := Round(VisibleAmount * 1.68 / 2);
    X1val := P16 + DeltaX;
    X2val := Width - P16 - DeltaX;
    X0val := X1val + Round((X2val - X1val) / ((FMaximum - FMinimum) * 1.0)
      * FValue);
    Canvas.Brush.Color := RGB(240, 240, 240);
    Canvas.FrameRect(Rect(X0val - DeltaX, 0, X0val + DeltaX, Height));
    Canvas.Pen.Color := RGB(148, 148, 148);
    Canvas.Brush.Color := RGB(200, 200, 200);
    Canvas.Rectangle(Rect(X0val - DeltaX + 1, 1, X0val + DeltaX - 1,
      Height - 1));
  end
  else
  begin
    Canvas.Brush.Color := Background;
    Canvas.FillRect(Rect(0, P16, Width, Height - P16));
    FGUIDesigner.vilControls1616.Draw(Canvas, (Width - P16) div 2, 0, 4);
    FGUIDesigner.vilControls1616.Draw(Canvas, (Width - P16) div 2,
      Height - P16, 5);
    // scroller
    DeltaY := Round(VisibleAmount * 1.68 / 2);
    Y1val := P16 + DeltaY;
    Y2val := Height - P16 - DeltaY;
    Y0val := Y1val + Round((Y2val - Y1val) / ((FMaximum - FMinimum) * 1.0)
      * FValue);
    Canvas.Brush.Color := RGB(240, 240, 240);
    Canvas.FrameRect(Rect(0, Y0val - DeltaY, Width, Y0val + DeltaY));
    Canvas.Pen.Color := RGB(148, 148, 148);
    Canvas.Brush.Color := RGB(200, 200, 200);
    Canvas.Rectangle(Rect(1, Y0val - DeltaY + 1, Width - 1,
      Y0val + DeltaY - 1));
  end;
end;

procedure TAScrollBar.SetValue(AValue: Integer);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Invalidate;
  end;
end;

procedure TAScrollBar.SetOrientation(AValue: TOrientation);
var
  Tmp: Integer;
begin
  if AValue <> FOrientation then
  begin
    FOrientation := AValue;
    if not(csLoading in ComponentState) then
    begin
      Tmp := Width;
      Width := Height;
      Height := Tmp;
    end;
    Invalidate;
  end;
end;

procedure TAScrollBar.SetVisibleAmount(AValue: Integer);
begin
  if AValue <> FVisibleAmount then
  begin
    FVisibleAmount := AValue;
    Invalidate;
  end;
end;

end.
