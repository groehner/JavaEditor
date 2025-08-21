unit UJPanel;

interface

uses
  Classes,
  ExtCtrls,
  UJComponents;

type

  TJPanel = class(TSwingComponent)
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
    function GetIndex: Integer;
    procedure SetTab;
  published
    property Border;
  end;

  TJSubPanel = class(TJPanel)
  private
    FSubType: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure MakeUniqueName(const FromText: string = ''); override;
  published
    property SubType: string read FSubType write FSubType;
  end;

implementation

uses
  Graphics,
  Controls,
  UJTabbedPane,
  UITypes,
  ULink;

{ --- TJPanel ------------------------------------------------------------------ }

constructor TJPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +12;
  ControlStyle := [csAcceptsControls];
  Background := clBtnFace;
  ShowFont := False;
  Opaque := False;
  JavaType := 'JPanel';
end;

function TJPanel.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Border|Background' + inherited;
end;

function TJPanel.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|componentAdded|componentRemoved|propertyChange' + MouseEvents +
    inherited;
end;

procedure TJPanel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JPanel ' + Name + ' = new JPanel(null, true);');
  MakeAttribut('Opaque', 'false');
end;

procedure TJPanel.Paint;
begin
  CanvasFontAssign;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Canvas.FillRect(ClientRect);
  Border.Show(Self, Canvas);
end;

procedure TJPanel.SetTab;
begin
  if Parent is TJTabbedPane then
    (Parent as TJTabbedPane).SelectedIndex := GetIndex;
end;

function TJPanel.GetIndex: Integer;
var
  Int: Integer;
  ATabPane: TJTabbedPane;
begin
  ATabPane := (Parent as TJTabbedPane);
  Int := 0;
  while (Int < ATabPane.Tabs.Count) and
    (ATabPane.Name + 'TabPanel' + ATabPane.Tabs[Int] <> Self.Name) do
    Inc(Int);
  if Int < ATabPane.Tabs.Count then
    Result := Int
  else
    Result := -1;
end;

{ --- TJSubPanel ----------------------------------------------------------------- }

constructor TJSubPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 38;
  SubType := ULink.PanelCanvasType;
  JavaType := SubType;
end;

procedure TJSubPanel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ' + SubType + ' ' + Name + ' = new ' +
    SubType + '();');
end;

procedure TJSubPanel.MakeUniqueName(const FromText: string = '');
begin
  inherited MakeUniqueName(SubType);
end;

procedure TJSubPanel.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private ' + SubType + ' ' + Name + ' = new '
    + SubType);
end;

end.
