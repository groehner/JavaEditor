unit UAPanel;

{ Classes
  TAPanel = class (TAWTComponent)
  TASubPanel
  TACanvas = class(TAWTComponent)
  TASubCanvas
  TATurtle
}

interface

uses
  Classes,
  ExtCtrls,
  UAComponents;

type

  TAPanel = class(TAWTComponent)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(APanel: TPanel);
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property componentAdded;
    property componentRemoved;
    property propertyChange;
  end;

  TASubPanel = class(TAPanel)
  private
    FSubType: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure MakeUniqueName(FromText: string = ''); override;
  published
    property SubType: string read FSubType write FSubType;
  end;

  TACanvas = class(TAWTComponent)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(APanel: TPanel);
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  end;

  TASubCanvas = class(TACanvas)
  private
    FSubType: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure MakeUniqueName(FromText: string = ''); override;
  published
    property SubType: string read FSubType write FSubType;
  end;

  TATurtle = class(TACanvas)
  private
    FOriginX: Integer;
    FOriginY: Integer;
    FDrawDynamic: Boolean;
    FSleepTime: Integer;
    FTurtleX: Integer;
    FTurtleY: Integer;
    FTurtleW: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(APanel: TPanel);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
  published
    property OriginX: Integer read FOriginX write FOriginX;
    property OriginY: Integer read FOriginY write FOriginY;
    property DrawDynamic: Boolean read FDrawDynamic write FDrawDynamic;
    property SleepTime: Integer read FSleepTime write FSleepTime;
    property TurtleX: Integer read FTurtleX write FTurtleX;
    property TurtleY: Integer read FTurtleY write FTurtleY;
    property TurtleW: Integer read FTurtleW write FTurtleW;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Controls,
  UITypes,
  ULink;

{ --- TAPanel ------------------------------------------------------------------ }

constructor TAPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls];
  HelpType := htContext;
  Width := 120;
  Height := 80;
  Tag := -12;
  Background := clBtnFace;
  ShowFont := False;
  JavaType := 'Panel';
end;

constructor TAPanel.CreateFrom(APanel: TPanel);
begin
  Create(APanel.Owner);
  CreateFromA(APanel);
  Background := APanel.Color;
  if Background = clBtnFace then
    Background := clWhite;
end;

function TAPanel.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Background' + inherited;
end;

function TAPanel.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|componentAdded|componentRemoved|propertyChange' + MouseEvents;
  if ShowEvents = 3 then
    Result := Result + ContainerComponentEvents;
  Result := Result + inherited;
end;

procedure TAPanel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Panel ' + Name + ' = new Panel(null);');
end;

procedure TAPanel.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Background;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
end;

{ --- TASubPanel ----------------------------------------------------------------- }

constructor TASubPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -38;
  SubType := ULink.PanelCanvasType;
  JavaType := SubType;
end;

procedure TASubPanel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ' + SubType + ' ' + Name + ' = new ' +
    SubType + '();');
end;

procedure TASubPanel.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private ' + SubType + ' ' + Name + ' = new '
    + SubType);
end;

procedure TASubPanel.MakeUniqueName(FromText: string = '');
begin
  inherited MakeUniqueName(SubType);
end;

{ --- TACanvas ----------------------------------------------------------------- }

constructor TACanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 120;
  Height := 80;
  Tag := -13;
  Background := clBtnFace;
  ShowFont := False;
  JavaType := 'Canvas';
end;

constructor TACanvas.CreateFrom(APanel: TPanel);
begin
  Create(APanel.Owner);
  CreateFromA(APanel);
  Background := APanel.Color;
end;

function TACanvas.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Background' + inherited;
end;

function TACanvas.GetEvents(ShowEvents: Integer): string;
begin
  Result := MouseEvents + inherited;
end;

procedure TACanvas.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Canvas ' + Name + ' = new Canvas();');
end;

procedure TACanvas.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Background;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
end;

{ --- TASubCanvas -------------------------------------------------------------- }

constructor TASubCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -39;
  SubType := ULink.PanelCanvasType;
  JavaType := SubType;
end;

procedure TASubCanvas.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ' + SubType + ' ' + Name + ' = new ' +
    SubType + '();');
end;

procedure TASubCanvas.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private ' + SubType + ' ' + Name + ' = new '
    + SubType);
end;

procedure TASubCanvas.MakeUniqueName(FromText: string = '');
begin
  inherited MakeUniqueName(SubType);
end;

{ --- TATurtle ----------------------------------------------------------------- }

constructor TATurtle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Background := clWhite;
  ShowFont := False;
  Tag := -14;
  JavaType := 'Turtle';
end;

constructor TATurtle.CreateFrom(APanel: TPanel);
begin
  Create(APanel.Owner);
  CreateFromA(APanel);
  Background := APanel.Color;
end;

function TATurtle.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Background|OriginX|OriginY|DrawDynamic|SleepTime' +
    '|TurtleX|TurtleY|TurtleW' + inherited;
end;

procedure TATurtle.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'OriginX') or (Attr = 'OriginY') then
    MakeAttribut('Origin', IntToStr(OriginX) + ', ' + IntToStr(OriginY))
  else
    inherited;
end;

function TATurtle.GetEvents(ShowEvents: Integer): string;
begin
  Result := MouseEvents + inherited;
end;

procedure TATurtle.NewControl;
begin
  FPartner.InsertImport('je.util.Turtle');
  DefaultComponent;
  InsertNewVariable('private Turtle ' + Name + ' = new Turtle();');
  MakeAttribut('Background', 'Color.WHITE');
end;

end.
