unit UFXCanvas;

interface

uses
  Classes,
  UFXComponents;

type

  TFXCanvas = class(TFXNode)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetPositionAndSize; override;
    procedure MakeFont; override;
  end;

  TFXSubCanvas = class(TFXCanvas)
  private
    FSubType: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure MakeUniqueName(const FromText: string = ''); override;
  published
    property SubType: string read FSubType write FSubType;
  end;

implementation

uses
  Graphics,
  ULink;

{ --- TFXCanvas ----------------------------------------------------------------- }

constructor TFXCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 120;
  Height := 80;
  Tag := 114;
  Background := clBtnFace;
  JavaType := 'Canvas';
end;

procedure TFXCanvas.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
end;

procedure TFXCanvas.NewControl;
begin
  InsertImport('javafx.scene.canvas.*');
  InsertNewVariable('private Canvas ' + Name + ' = new Canvas();');
  DefaultComponent;
end;

function TFXCanvas.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Width|Height' + inherited GetAttributes(ShowAttributes);
end;

procedure TFXCanvas.SetPositionAndSize;
begin
  ChangeAttributValue(Name + '.setLayoutX(', GetLayoutXCode);
  ChangeAttributValue(Name + '.setLayoutY(', GetLayoutYCode);
  ChangeAttributValue(Name + '.setWidth(', GetWidthCode);
  ChangeAttributValue(Name + '.setHeight(', GetHeightCode);
end;

procedure TFXCanvas.MakeFont;
begin
  // no font
end;

{ --- TFXSubCanvas ----------------------------------------------------------------- }

constructor TFXSubCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 122;
  SubType := ULink.PanelCanvasType;
  JavaType := SubType;
end;

procedure TFXSubCanvas.NewControl;
var
  Typ: string;
begin
  Typ := ULink.PanelCanvasType;
  DefaultComponent;
  InsertNewVariable('private ' + Typ + ' ' + Name + ' = new ' + Typ + '();');
  InsertImport('javafx.scene.canvas.*');
end;

procedure TFXSubCanvas.MakeUniqueName(const FromText: string = '');
begin
  inherited MakeUniqueName(SubType);
end;

end.
