unit UJPanel;

interface

uses
  Classes, ExtCtrls, UJComponents;

type

  TJPanel = class (TSwingComponent)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aPanel: TPanel);
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
    function getIndex: integer;
    procedure setTab;
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
    procedure MakeUniqueName(FromText: string = ''); override;
  published
    property SubType: string read FSubType write FSubType;
  end;

implementation

uses Graphics, Controls, UJTabbedPane, UITypes, ULink;

{--- TJPanel ------------------------------------------------------------------}

constructor TJPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +12;
  ControlStyle:= [csAcceptsControls];
  Background:= clBtnFace;
  ShowFont:= false;
  Opaque:= false;
  JavaType:= 'JPanel';
end;

constructor TJPanel.CreateFrom(aPanel: TPanel);
begin
  Create(aPanel.Owner);
  CreateFromJ(aPanel);
  Background:= aPanel.Color;
end;

function TJPanel.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Border|Background' + inherited;
end;

function TJPanel.getEvents(ShowEvents: integer): string;
begin
  Result:= '|componentAdded|componentRemoved|propertyChange' +
           MouseEvents + inherited;
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
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TWinControl).Brush.Color
    else Canvas.Brush.Color:= Background;
  Canvas.FillRect(ClientRect);
  Border.Show(Self, Canvas);
end;

procedure TJPanel.setTab;
begin
  if Parent is TJTabbedPane then
    (Parent as TJTabbedPane).SelectedIndex:= getIndex;
end;

function TJPanel.getIndex: integer;
  var i: integer; aTabPane: TJTabbedPane;
begin
  aTabPane:= (Parent as TJTabbedPane);
  i:= 0;
  while (i < aTabPane.Tabs.Count) and
        (aTabPane.Name + 'TabPanel' + aTabPane.Tabs[i] <> Self.Name)
  do
    inc(i);
  if i < aTabPane.Tabs.Count
    then Result:= i
    else Result:= -1;
end;

{--- TJSubPanel -----------------------------------------------------------------}

constructor TJSubPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 38;
  SubType:= ULink.PanelCanvasType;
  JavaType:= SubType;
end;

procedure TJSubPanel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ' + SubType + ' ' + Name + ' = new ' + SubType + '();');
end;

procedure TJSubPanel.MakeUniqueName(FromText: string = '');
begin
  inherited MakeUniqueName(SubType);
end;

procedure TJSubPanel.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private ' + SubType + ' ' + Name + ' = new ' + SubType);
end;

end.
