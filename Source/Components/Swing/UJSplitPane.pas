unit UJSplitPane;

interface

uses
  Classes,
  ExtCtrls,
  UJComponents;

type
  TJSplitPane = class(TSwingComponent)
  private
    FLeftComponent: string;
    FRightComponent: string;
    FTopComponent: string;
    FBottomComponent: string;
    FResizeWeight: Real;
    FOneTouchExpandable: Boolean;
    FContinuousLayout: Boolean;
    FDividerSize: Integer;
    FDividerLocation: Integer;
    FOrientation: TOrientation;
    procedure SetOrientation(AValue: TOrientation);
    procedure SetDividerSize(AValue: Integer);
    procedure SetDividerLocation(AValue: Integer);
    procedure SetOneTouchExpandable(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property LeftComponent: string read FLeftComponent write FLeftComponent;
    property RightComponent: string read FRightComponent write FRightComponent;
    property TopComponent: string read FTopComponent write FTopComponent;
    property BottomComponent: string read FBottomComponent
      write FBottomComponent;
    property ResizeWeight: Real read FResizeWeight write FResizeWeight;
    property OneTouchExpandable: Boolean read FOneTouchExpandable
      write SetOneTouchExpandable;
    property ContinuousLayout: Boolean read FContinuousLayout
      write FContinuousLayout;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property DividerSize: Integer read FDividerSize write SetDividerSize;
    property DividerLocation: Integer read FDividerLocation
      write SetDividerLocation;
  end;

var
  LeftKey: string;
  RightKey: string;

implementation

uses
  Windows,
  Types,
  Graphics,
  Controls;

constructor TJSplitPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 17;
  Width := 160;
  Height := 80;
  FResizeWeight := 0;
  FDividerSize := 10;
  FDividerLocation := 80;
  FOrientation := VERTICAL;
  JavaType := 'JSplitPane';
end;

function TJSplitPane.GetAttributes(ShowAttributes: Integer): string;
const
  Pane1 = '|LeftComponent|RightComponent|TopComponent|BottomComponent|Orientation';
  Pane2 = '|ResizeWeight|OneTouchExpandable|ContinuousLayout|DividerSize|DividerLocation';
begin
  if ShowAttributes = 1 then
    Result := Pane1
  else
    Result := Pane1 + Pane2;
  Result := Result + inherited;
end;

procedure TJSplitPane.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'TopComponent') or (Attr = 'BottomComponent') or
    (Attr = 'LeftComponent') or (Attr = 'RightComponent') then
    MakeAttribut(Attr, Value)
  else
    inherited;
end;

procedure TJSplitPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JSplitPane ' + Name + ' = new JSplitPane();');
  MakeAttribut('DividerLocation', '80');
end;

procedure TJSplitPane.Paint;
var
  Gray1, Gray2, Gray3, Gray4, Blue, X1Pos, X2Pos, Y1Pos, Y2Pos, TextWidth,
    TextHeight: Integer;
  Rect1, Rect2, Rect3: TRect;
  Points: array [0 .. 2] of TPoint;
  Str1, Str2: string;
begin
  Gray1 := RGB(240, 240, 240);
  Gray2 := RGB(218, 218, 218);
  Gray3 := RGB(105, 105, 105);
  Gray4 := RGB(50, 50, 50);
  Blue := BlueColor;

  Str1 := LeftKey;
  Str2 := RightKey;
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Pen.Color := Gray4;
  Canvas.Brush.Color := Gray1;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  TextHeight := Canvas.TextHeight(Str1);

  if FOrientation = VERTICAL then
  begin
    X1Pos := FDividerLocation;
    X2Pos := FDividerLocation + FDividerSize;
    Y1Pos := Height div 2;
    // 3 main rectangles
    Canvas.Pen.Color := Gray3;
    Canvas.Brush.Color := Gray1;
    Rect1 := Rect(1, 1, X1Pos, Height - 1);
    Rect2 := Rect(X1Pos, 1, X2Pos, Height - 1);
    Rect3 := Rect(X2Pos, 1, Width, Height - 1);
    Canvas.Rectangle(Rect1);
    Canvas.FillRect(Rect2);
    Canvas.Rectangle(Rect3);
    // shading
    Canvas.Brush.Color := Gray2;
    Canvas.FillRect(Rect(2, Y1Pos, X1Pos - 1, Height - 1));
    Canvas.FillRect(Rect(X2Pos + 1, Y1Pos, Width - 1, Height - 1));
    // text
    Canvas.Brush.Style := bsClear; // use transparent mode
    Canvas.Font.Color := Gray4;
    TextWidth := Canvas.TextWidth(Str1);
    Canvas.TextRect(Rect1, (X1Pos - TextWidth) div 2,
      (Height - TextHeight) div 2, Str1);
    TextWidth := Canvas.TextWidth(Str2);
    Canvas.TextRect(Rect3, X2Pos + (Width - X2Pos - TextWidth) div 2,
      (Height - TextHeight) div 2, Str2);
    // triangles
    if OneTouchExpandable then
    begin
      Canvas.Brush.Color := Blue;
      Points[0] := Point(X1Pos + 2, 8);
      Points[1] := Point(X1Pos + 6, 4);
      Points[2] := Point(X1Pos + 6, 12);
      Canvas.Polygon(Points);
      Points[0] := Point(X1Pos + 2, 15);
      Points[1] := Point(X1Pos + 6, 19);
      Points[2] := Point(X1Pos + 2, 23);
      Canvas.Polygon(Points);
    end;
  end
  else
  begin
    Y1Pos := FDividerLocation;
    Y2Pos := FDividerLocation + FDividerSize;
    // 3 main rectangles
    Canvas.Pen.Color := Gray3;
    Canvas.Brush.Color := Gray1;
    Rect1 := Rect(1, 1, Width - 1, Y1Pos);
    Rect2 := Rect(1, Y1Pos, Width - 1, Y2Pos);
    Rect3 := Rect(1, Y2Pos, Width - 1, Height);
    Canvas.Rectangle(Rect1);
    Canvas.FillRect(Rect2);
    Canvas.Rectangle(Rect3);
    // shading
    Canvas.Brush.Color := Gray2;
    Canvas.FillRect(Rect(2, Y1Pos div 2, Width - 2, Y1Pos - 1));
    Canvas.FillRect(Rect(2, Y2Pos + (Height - Y2Pos) div 2, Width - 2,
      Height - 1));
    // text
    Canvas.Brush.Style := bsClear; // use transparent mode
    Canvas.Font.Color := Gray4;
    TextWidth := Canvas.TextWidth('Linke Taste');
    Canvas.TextRect(Rect1, (Width - TextWidth) div 2, Y1Pos div 2,
      'Linke Taste');
    TextWidth := Canvas.TextWidth('Rechte Taste');
    Canvas.TextRect(Rect3, (Width - TextWidth) div 2, (Height - Y2Pos) div 2 +
      Y2Pos, 'Rechte Taste');
    // triangles
    if OneTouchExpandable then
    begin
      Canvas.Brush.Color := Blue;
      Points[0] := Point(8, Y1Pos + 2);
      Points[1] := Point(4, Y1Pos + 6);
      Points[2] := Point(12, Y1Pos + 6);
      Canvas.Polygon(Points);
      Points[0] := Point(15, Y1Pos + 2);
      Points[1] := Point(19, Y1Pos + 6);
      Points[2] := Point(23, Y1Pos + 2);
      Canvas.Polygon(Points);
    end;
  end;
end;

procedure TJSplitPane.SetOrientation(AValue: TOrientation);
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

procedure TJSplitPane.SetDividerSize(AValue: Integer);
begin
  if AValue <> FDividerSize then
  begin
    FDividerSize := AValue;
    Invalidate;
  end;
end;

procedure TJSplitPane.SetDividerLocation(AValue: Integer);
begin
  if AValue <> FDividerLocation then
  begin
    FDividerLocation := AValue;
    Invalidate;
  end;
end;

procedure TJSplitPane.SetOneTouchExpandable(AValue: Boolean);
begin
  if AValue <> FOneTouchExpandable then
  begin
    FOneTouchExpandable := AValue;
    Invalidate;
  end;
end;

initialization

LeftKey := 'Linke Taste';
RightKey := 'Rechte Taste';

end.
