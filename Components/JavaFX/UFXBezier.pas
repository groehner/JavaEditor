unit UFXBezier;

interface

uses Classes, Types, UFXShape;

type

  TFXQuadCurve = class (TFXShape)
  private
    FStartX: double;
    FStartY: double;
    FEndX: double;
    FEndY: double;
    FControlX: double;
    FControlY: double;
    Points: array[0..3] of TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property StartX: double read FStartX write FStartX;
    property StartY: double read FStartY write FStartY;
    property EndX: double read FEndX write FEndX;
    property EndY: double read FEndY write FEndY;
    property ControlX: double read FControlX write FControlX;
    property ControlY: double read FControlY write FControlY;
  end;

  TFXCubicCurve = class (TFXShape)
  private
    FStartX: double;
    FStartY: double;
    FEndX: double;
    FEndY: double;
    FControlX1: double;
    FControlY1: double;
    FControlX2: double;
    FControlY2: double;
    Points: array[0..3] of TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property StartX: double read FStartX write FStartX;
    property StartY: double read FStartY write FStartY;
    property EndX: double read FEndX write FEndX;
    property EndY: double read FEndY write FEndY;
    property ControlX1: double read FControlX1 write FControlX1;
    property ControlY1: double read FControlY1 write FControlY1;
    property ControlX2: double read FControlX2 write FControlX2;
    property ControlY2: double read FControlY2 write FControlY2;
  end;

implementation

uses SysUtils, Windows, Graphics;

{--- TFXQuadCurve -------------------------------------------------------------}

constructor TFXQuadCurve.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +169;
  Width:= 120;
  Height:= 80;
  FStartX:= 0;
  FStartY:= 25;
  FEndX:= 100;
  FEndY:= 50;
  FControlX:= 30;
  FControlY:= -20;
  JavaType:= 'QuadCurve';
end;

procedure TFXQuadCurve.Paint;
  var FControlX1, FControlY1, FControlX2, FControlY2: double;
begin
  DefaultPenBrush;
  FControlX1:= FStartX + 2.0/3.0*(FControlX - FStartX);
  FControlY1:= FStartY + 2.0/3.0*(FControlY - FStartY);
  FControlX2:= FEndX + 2.0/3.0*(FControlX - FEndX);
  FControlY2:= FEndY + 2.0/3.0*(FControlY - FEndY);
  Points[0]:= Point(Round(FStartX), Round(FStartY));
  Points[1]:= Point(Round(FControlX1), Round(FControlY1));
  Points[2]:= Point(Round(FControlX2), Round(FControlY2));
  Points[3]:= Point(Round(FEndX), Round(FEndY));
  BeginPath(Canvas.Handle);
  Canvas.PolyBezier(points);
  EndPath(Canvas.Handle);
  FillPath(Canvas.Handle);
end;

procedure TFXQuadCurve.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private QuadCurve ' + Name + ' = new QuadCurve();');
  MakeAttribut('StartX', IntToStr(Round(FStartX)));
  MakeAttribut('StartY', IntToStr(Round(FStartY)));
  MakeAttribut('EndX', IntToStr(Round(FEndX)));
  MakeAttribut('EndY', IntToStr(Round(FEndY)));
  MakeAttribut('ControlX', IntToStr(Round(FControlX)));
  MakeAttribut('ControlY', IntToStr(Round(FControlY)));
end;

function TFXQuadCurve.getAttributes(ShowAttributes: integer): string;
  const Attributes = '|StartX|StartY|EndX|EndY|ControlX|ControlY|Width|Height';
begin
  Result:= Attributes + inherited getAttributes(ShowAttributes) + '|LayoutX|LayoutY';
end;

procedure TFXQuadCurve.setAttribute(Attr, Value, Typ: string);
begin
  inherited;
  Invalidate;
end;

{--- TFXCubicCurve ------------------------------------------------------------}

constructor TFXCubicCurve.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +170;
  Width:= 120;
  Height:= 80;
  FStartX:= 0;
  FStartY:= 25;
  FEndX:= 100;
  FEndY:= 25;
  FControlX1:= 30;
  FControlY1:= -20;
  FControlX2:= 80;
  FControlY2:= 80;
  JavaType:= 'CubicCurve';
end;

procedure TFXCubicCurve.Paint;
begin
  DefaultPenBrush;
  Points[0]:= Point(Round(FStartX), Round(FStartY));
  Points[1]:= Point(Round(FControlX1), Round(FControlY1));
  Points[2]:= Point(Round(FControlX2), Round(FControlY2));
  Points[3]:= Point(Round(FEndX), Round(FEndY));
  BeginPath(Canvas.Handle);
  Canvas.PolyBezier(points);
  EndPath(Canvas.Handle);
  FillPath(Canvas.Handle);
end;

procedure TFXCubicCurve.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private CubicCurve ' + Name + ' = new CubicCurve();');
  MakeAttribut('StartX', IntToStr(Round(FStartX)));
  MakeAttribut('StartY', IntToStr(Round(FStartY)));
  MakeAttribut('EndX', IntToStr(Round(FEndX)));
  MakeAttribut('EndY', IntToStr(Round(FEndY)));
  MakeAttribut('ControlX1', IntToStr(Round(FControlX1)));
  MakeAttribut('ControlY1', IntToStr(Round(FControlY1)));
  MakeAttribut('ControlX2', IntToStr(Round(FControlX2)));
  MakeAttribut('ControlY2', IntToStr(Round(FControlY2)));
end;

function TFXCubicCurve.getAttributes(ShowAttributes: integer): string;
  const Attributes = '|StartX|StartY|EndX|EndY|ControlX1|ControlY1|ControlX2|ControlY2|Width|Height';
begin
  Result:= Attributes + inherited getAttributes(ShowAttributes) + '|LayoutX|LayoutY';
end;

procedure TFXCubicCurve.setAttribute(Attr, Value, Typ: string);
begin
  inherited;
  Invalidate;
end;

end.
