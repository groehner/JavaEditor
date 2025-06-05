unit UFXToolBar;

interface

uses
  Classes,
  UJEComponents,
  UFXComponents;

type

  TFXToolBar = class(TFXControl)
  private
    FOrientation: TOrientation;
    procedure SetOrientation(AValue: TOrientation);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
  published
    property Orientation: TOrientation read FOrientation write SetOrientation;
  end;

implementation

uses
  Windows,
  Controls;

{ --- TFXToolBar -------------------------------------------------------------- }

constructor TFXToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +134;
  PrefWidth := 200;
  PrefHeight := 24;
  ControlStyle := [csAcceptsControls];
  JavaType := 'ToolBar';
end;

procedure TFXToolBar.Paint;
var
  AColor: Integer;
begin
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  AColor := 181;
  if Orientation = HORIZONTAL then
    for var I := Height - 1 downto Height - 20 do
    begin
      Canvas.Pen.Color := RGB(AColor, AColor, AColor);
      Canvas.MoveTo(0, I);
      Canvas.LineTo(Width, I);
      if AColor = 183 then
        AColor := 210;
      AColor := AColor + 2;
    end
  else
    for var I := 0 to 20 do
    begin
      Canvas.Pen.Color := RGB(AColor, AColor, AColor);
      Canvas.MoveTo(I, 0);
      Canvas.LineTo(I, Height);
      if AColor = 183 then
        AColor := 210;
      AColor := AColor + 2;
    end;
end;

procedure TFXToolBar.SetOrientation(AValue: TOrientation);
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

procedure TFXToolBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ToolBar ' + Name + ' = new ToolBar();');
  InsertImport('javafx.geometry.*');
end;

function TFXToolBar.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Orientation' + inherited GetAttributes(ShowAttributes);
end;

end.
