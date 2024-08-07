unit UJSplitPane;

interface

uses
  Classes, ExtCtrls, UJComponents;

type
  TJSplitPane = class(TSwingComponent)
  private
    FLeftComponent: string;
    FRightComponent: string;
    FTopComponent: string;
    FBottomComponent: string;
    FResizeWeight: real;
    FOneTouchExpandable: boolean;
    FContinuousLayout: boolean;
    FDividerSize: integer;
    FDividerLocation: integer;
    FOrientation: TOrientation;
    procedure setOrientation(aValue: TOrientation);
    procedure setDividerSize(aValue: integer);
    procedure setDividerLocation(aValue: integer);
    procedure setOneTouchExpandable(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aSplitter: TSplitter);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property LeftComponent: string read FLeftComponent write FLeftComponent;
    property RightComponent: string read FRightComponent write FRightComponent;
    property TopComponent: string read FTopComponent write FTopComponent;
    property BottomComponent: string read FBottomComponent write FBottomComponent;
    property ResizeWeight: real read FResizeWeight write FResizeWeight;
    property OneTouchExpandable: boolean read FOneTouchExpandable write setOneTouchExpandable;
    property ContinuousLayout: boolean read FContinuousLayout write FContinuousLayout;
    property Orientation: TOrientation read FOrientation write setOrientation;
    property DividerSize: integer read FDividerSize write setDividerSize;
    property DividerLocation: integer read FDividerLocation write setDividerLocation;
  end;

var LeftKey: string;
    RightKey: string;

implementation

uses Windows, Types, Graphics, Controls;

constructor TJSplitPane.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 17;
  Width:= 160;
  Height:= 80;
  FResizeWeight:= 0;
  FDividerSize:= 10;
  FDividerLocation:= 80;
  FOrientation:= VERTICAL;
  JavaType:= 'JSplitPane';
end;

constructor TJSplitPane.CreateFrom(aSplitter: TSplitter);
begin
  Create(aSplitter.Owner);
  CreateFromJ(aSplitter);
end;

function TJSplitPane.getAttributes(ShowAttributes: integer): string;
  const
    Pane1 = '|LeftComponent|RightComponent|TopComponent|BottomComponent|Orientation';
    Pane2 = '|ResizeWeight|OneTouchExpandable|ContinuousLayout|DividerSize|DividerLocation';
begin
  if ShowAttributes = 1
    then Result:= Pane1
    else Result:= Pane1 + Pane2;
  Result:= Result + inherited;
end;

procedure TJSplitPane.setAttribute(Attr, Value, Typ: string);
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
  var Gray1, Gray2, Gray3, Gray4, Blue,
      x1, x2, y1, y2, tw, th: integer;
      R1, R2, R3: TRect;
      Points: array[0..2] of TPoint;
      s1, s2: string;
begin
  Gray1:= RGB(240, 240, 240);
  Gray2:= RGB(218, 218, 218);
  Gray3:= RGB(105, 105, 105);
  Gray4:= RGB(50, 50, 50);
  Blue := BlueColor;

  s1:= LeftKey;
  s2:= Rightkey;
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Pen.Color:= Gray4;
  Canvas.Brush.Color:= Gray1;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  th:= Canvas.TextHeight(s1);

  if FOrientation = VERTICAL then begin
    x1:= FDividerLocation;
    x2:= FDividerLocation + FDividerSize;
    y1:= Height div 2;
    // 3 main rectangles
    Canvas.Pen.Color:= Gray3;
    Canvas.Brush.Color:= Gray1;
    R1:= Rect( 1, 1, x1, Height-1);
    R2:= Rect(x1, 1, x2, Height-1);
    R3:= Rect(x2, 1, Width, Height-1);
    Canvas.Rectangle(R1);
    Canvas.FillRect (R2);
    Canvas.Rectangle(R3);
    // shading
    Canvas.Brush.Color:= Gray2;
    Canvas.FillRect(Rect(2, y1, x1-1, Height-1));
    Canvas.FillRect(Rect(x2+1, y1, Width-1, Height-1));
    // text
    Canvas.Brush.Style:= bsClear; // use transparent mode
    Canvas.Font.Color:= Gray4;
    tw:= Canvas.TextWidth(s1);
    Canvas.TextRect(R1, (x1 - tw) div 2, (Height - th) div 2, s1);
    tw:= Canvas.TextWidth(s2);
    Canvas.TextRect(R3, x2 + (Width-x2-tw) div 2, (Height-th) div 2, s2);
    // triangles
    if OneTouchExpandable then begin
      Canvas.Brush.Color:= Blue;
      Points[0]:= Point(x1+2, 8);
      Points[1]:= Point(x1+6, 4);
      Points[2]:= Point(x1+6, 12);
      Canvas.Polygon(Points);
      Points[0]:= Point(x1+2, 15);
      Points[1]:= Point(x1+6, 19);
      Points[2]:= Point(x1+2, 23);
      Canvas.Polygon(Points);
    end;
  end else begin
    y1:= FDividerLocation;
    y2:= FDividerLocation + FDividerSize;
    // 3 main rectangles
    Canvas.Pen.Color:= Gray3;
    Canvas.Brush.Color:= Gray1;
    R1:= Rect(1,  1, Width-1, y1);
    R2:= Rect(1, y1, Width-1, y2);
    R3:= Rect(1, y2, Width-1, Height);
    Canvas.Rectangle(R1);
    Canvas.FillRect (R2);
    Canvas.Rectangle(R3);
    // shading
    Canvas.Brush.Color:= Gray2;
    Canvas.FillRect(Rect(2, y1 div 2, Width-2, y1-1));
    Canvas.FillRect(Rect(2, y2 + (Height-y2) div 2, Width-2, Height-1));
    // text
    Canvas.Brush.Style:= bsClear; // use transparent mode
    Canvas.Font.Color:= Gray4;
    tw:= Canvas.TextWidth('Linke Taste');
    Canvas.TextRect(R1, (Width-tw) div 2, y1 div 2, 'Linke Taste');
    tw:= Canvas.TextWidth('Rechte Taste');
    Canvas.TextRect(R3, (Width-tw) div 2, (Height-y2) div 2 + y2, 'Rechte Taste');
    // triangles
    if OneTouchExpandable then begin
      Canvas.Brush.Color:= Blue;
      Points[0]:= Point(8, y1+2);
      Points[1]:= Point(4, y1+6);
      Points[2]:= Point(12, y1+6);
      Canvas.Polygon(Points);
      Points[0]:= Point(15, y1+2);
      Points[1]:= Point(19, y1+6);
      Points[2]:= Point(23, y1+2);
      Canvas.Polygon(Points);
    end;
  end;
end;

procedure TJSplitPane.setOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
    end;
    Invalidate;
  end;
end;

procedure TJSplitPane.setDividerSize(aValue: integer);
begin
  if aValue <> FDividerSize then begin
    FDividerSize:= aValue;
    Invalidate;
  end;
end;

procedure TJSplitPane.setDividerLocation(aValue: integer);
begin
  if aValue <> FDividerLocation then begin
    FDividerLocation:= aValue;
    Invalidate;
  end;
end;

procedure TJSplitPane.setOneTouchExpandable(aValue: boolean);
begin
  if aValue <> FOneTouchExpandable then begin
    FOneTouchExpandable:= aValue;
    Invalidate;
  end;
end;

initialization
  LeftKey := 'Linke Taste';
  RightKey:= 'Rechte Taste';

end.

