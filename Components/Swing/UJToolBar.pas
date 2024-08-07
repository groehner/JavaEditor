unit UJToolBar;

interface

uses
  Classes, UJComponents;

type

  TJToolBar = class (TSwingComponent)
  private
    FFloatable: boolean;
    FRollover: boolean;
    FBorderPainted: boolean;
    FOrientation: TOrientation;
    FTitle: string;
    procedure setBorderPainted(aValue: boolean);
    procedure setOrientation(aValue: TOrientation);
   procedure MakeTitle(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Floatable: boolean read FFloatable write FFloatable;
    property Rollover: boolean read FRollover write FRollover;
    property BorderPainted: boolean read FBorderPainted write setBorderPainted;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Title: string read FTitle write FTitle;
  end;

implementation

uses Windows, SysUtils, Graphics, Controls, UGUIDesigner;

{--- TJToolBar --------------------------------------------------------------}

constructor TJToolBar.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +23;
  Width:= 200;
  Height:= 24;
  FFloatable:= true;
  FRollover:= false;
  FBorderPainted:= true;
  ControlStyle := [csAcceptsControls];
  JavaType:= 'JToolBar';
end;

function TJToolBar.getAttributes(ShowAttributes: integer): string;
  const
    Show1 = '|Orientation|Title';
    Show2 = '|Floatable|Rollover|BorderPainted';
begin
  if ShowAttributes = 1
    then Result:= Show1
    else Result:= Show1 + Show2;
  Result:= Result + inherited;
end;

procedure TJToolBar.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Title' then
    MakeTitle(Value)
  else
    inherited;
end;

procedure TJToolBar.MakeTitle(const Value: string);
  var key, s: string;
begin
  key:= 'private JToolBar ' + Name;
  s  := Indent1 + key + ' = new JToolBar("' + Value + '");';
  Partner.ReplaceAttribute(key, s);
end;

procedure TJToolBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JToolBar ' + Name + ' = new JToolBar();');
end;

procedure TJToolBar.Paint;
  var x, y: integer;
begin
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  if FBorderPainted then begin
    Canvas.Pen.Color:= clWhite;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width, 0);

    if FOrientation = Horizontal then begin
      y:= 2;
      while y < Height do begin
        FGUIDesigner.vilControls1616.Draw(Canvas, 2, y, 14);
        inc(y, 16);
      end;
    end else begin
      x:= 0;
      while x < width do begin
        FGUIDesigner.vilControls1616.Draw(Canvas, x, 2, 14);
        inc(x, 16);
      end;
    end;
    Canvas.Pen.Color:= RGB(204, 204, 204);
    Canvas.MoveTo(0, Height-1);
    Canvas.LineTo(Width, Height-1);
  end;
end;

procedure TJToolBar.setBorderPainted(aValue: boolean);
begin
  if aValue <> FBorderPainted then begin
    FBorderPainted:= aValue;
    Invalidate;
  end;
end;

procedure TJToolBar.SetOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then  begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
    end;
    Invalidate;
  end;
end;

end.
