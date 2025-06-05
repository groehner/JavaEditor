unit UJLayeredDesktopPane;

interface

uses
  Classes,
  UJComponents;

type

  TDragMode = (LIVE_DRAG_MODE, OUTLINE_DRAG_MODE);

  TJLayeredPane = class (TSwingComponent)
  public
    constructor Create (AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Border;
  end;

  TJDesktopPane = class (TSwingComponent)
  // not derived from JLayeredPane because of background-behaviour
  private
    FDragMode: TDragMode;
  public
    constructor Create (AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
  published
    property DragMode: TDragMode read FDragMode write FDragMode;
    property Border;
  end;

  TJInternalFrame = class (TSwingComponent)
  public
    constructor Create (AOwner: TComponent); override;
    procedure NewControl; override;
    procedure Paint; override;
  end;

implementation

uses
  Graphics,
  Controls,
  UGUIDesigner;

{--- JLayeredPane -------------------------------------------------------------}

constructor TJLayeredPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 30;
  ControlStyle:= [csAcceptsControls];
  JavaType:= 'JLayeredPane';
end;

function TJLayeredPane.GetAttributes(ShowAttributes: Integer): string;
begin
  Result:= '|Border' + inherited;
end;

procedure TJLayeredPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JLayeredPane ' + Name + ' = new JLayeredPane();');
end;

procedure TJLayeredPane.Paint;
begin
  Background:= DefaultBackground;
  Foreground:= DefaultBackground;
  inherited Paint;
end;

{--- JDesktopPane -------------------------------------------------------------}

constructor TJDesktopPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 31;
  Background:= clWhite;
  ControlStyle:= [csAcceptsControls];
  JavaType:= 'JDesktopPane';
end;

function TJDesktopPane.GetAttributes(ShowAttributes: Integer): string;
begin
  Result:= '|Border|DragMode' + inherited;
end;

procedure TJDesktopPane.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'DragMode' then
    MakeAttribut(Attr, 'JDesktopPane.' + Value)
  else
    inherited;
end;

procedure TJDesktopPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JDesktopPane ' + Name + ' = new JDesktopPane();');
end;

{--- JInternalFrame -----------------------------------------------------------}

constructor TJInternalFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 32;
  Foreground:= DarkShadow;
  Background:= DefaultBackground;
  ControlStyle:= [csAcceptsControls];
  Visible:= True;
  JavaType:= 'JInternalFrame';
end;

procedure TJInternalFrame.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JInternalFrame ' + Name + ' = new JInternalFrame();');
  MakeAttribut('Visible', 'true');
end;

procedure TJInternalFrame.Paint;
begin
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Pen.Width:= 8;
  Canvas.Brush.Color:= DefaultBackground;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));
  Canvas.Pen.Width:= 1;
  Canvas.MoveTo(0, PPIScale(27));
  Canvas.LineTo(Width, PPIScale(27));
  if Background <> DefaultBackground then begin
    Canvas.Brush.Color:= Background;
    Canvas.FillRect(Rect(PPIScale(5), PPIScale(28), Width-PPIScale(6), Height-PPIScale(6)));
  end;
  FGUIDesigner.vilControls1616.Draw(Canvas, PPIScale(10), PPIScale(8), 15);
  var XPos:= PPIScale(36);
  while XPos + PPIScale(8) < Width - 15 do begin
    FGUIDesigner.vilControls1616.Draw(Canvas, XPos, PPIScale(8), 14);
    XPos:= XPos + PPIScale(8);
  end;
end;

end.
