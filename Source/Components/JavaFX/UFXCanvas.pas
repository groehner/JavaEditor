unit UFXCanvas;

interface

uses
  Classes, UFXComponents;

type

  TFXCanvas = class(TFXNode)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure SetPositionAndSize; override;
  end;

  TFXSubCanvas = class(TFXCanvas)
  private
    FSubType: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure MakeUniqueName(FromText: string = ''); override;
  published
    property SubType: string read FSubType write FSubType;
  end;

implementation

uses Graphics, ULink;

{--- TFXCanvas -----------------------------------------------------------------}

constructor TFXCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width:= 120;
  Height:= 80;
  Tag:= 114;
  Background:= clBtnFace;
  JavaType:= 'Canvas';
end;

procedure TFXCanvas.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
end;

procedure TFXCanvas.NewControl;
begin
  InsertImport('javafx.scene.canvas.*');
  InsertNewVariable('private Canvas '  + Name + ' = new Canvas();');
  DefaultComponent;
end;

function TFXCanvas.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Width|Height' + inherited getAttributes(ShowAttributes);
end;

procedure TFXCanvas.SetPositionAndSize;
begin
  ChangeAttributValue(Name + '.setLayoutX(', getLayoutXCode);
  ChangeAttributValue(Name + '.setLayoutY(', getLayoutYCode);
  ChangeAttributValue(Name + '.setWidth(', getWidthCode);
  ChangeAttributValue(Name + '.setHeight(', getHeightCode);
end;

{--- TFXSubCanvas -----------------------------------------------------------------}

constructor TFXSubCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 122;
  SubType:= ULink.PanelCanvasType;
  JavaType:= SubType;
end;

procedure TFXSubCanvas.NewControl;
  var Typ: string;
begin
  Typ:= ULink.PanelCanvasType;
  DefaultComponent;
  InsertNewVariable('private ' + Typ + ' ' + Name + ' = new ' + Typ + '();');
  InsertImport('javafx.scene.canvas.*');
end;

procedure TFXSubCanvas.MakeUniqueName(FromText: string = '');
begin
  inherited MakeUniqueName(SubType);
end;

end.
