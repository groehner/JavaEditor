unit UJToolBar;

interface

uses
  Classes,
  UJComponents;

type

  TJToolBar = class(TSwingComponent)
  private
    FFloatable: Boolean;
    FRollover: Boolean;
    FBorderPainted: Boolean;
    FOrientation: TOrientation;
    FTitle: string;
    procedure SetBorderPainted(AValue: Boolean);
    procedure SetOrientation(AValue: TOrientation);
    procedure MakeTitle(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Floatable: Boolean read FFloatable write FFloatable;
    property Rollover: Boolean read FRollover write FRollover;
    property BorderPainted: Boolean read FBorderPainted write SetBorderPainted;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Title: string read FTitle write FTitle;
  end;

implementation

uses
  Windows,
  Graphics,
  Controls,
  UGUIDesigner;

{ --- TJToolBar -------------------------------------------------------------- }

constructor TJToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +23;
  Width := 200;
  Height := 24;
  FFloatable := True;
  FRollover := False;
  FBorderPainted := True;
  ControlStyle := [csAcceptsControls];
  JavaType := 'JToolBar';
end;

function TJToolBar.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|Orientation|Title';
  Show2 = '|Floatable|Rollover|BorderPainted';
begin
  if ShowAttributes = 1 then
    Result := Show1
  else
    Result := Show1 + Show2;
  Result := Result + inherited;
end;

procedure TJToolBar.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Title' then
    MakeTitle(Value)
  else
    inherited;
end;

procedure TJToolBar.MakeTitle(const Value: string);
var
  Key, Str: string;
begin
  Key := 'private JToolBar ' + Name;
  Str := Indent1 + Key + ' = new JToolBar("' + Value + '");';
  FPartner.ReplaceAttribute(Key, Str);
end;

procedure TJToolBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JToolBar ' + Name + ' = new JToolBar();');
end;

procedure TJToolBar.Paint;
var
  XPos, YPos: Integer;
begin
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  if FBorderPainted then
  begin
    Canvas.Pen.Color := clWhite;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width, 0);

    if FOrientation = HORIZONTAL then
    begin
      YPos := 2;
      while YPos < Height do
      begin
        FGUIDesigner.vilControls1616.Draw(Canvas, 2, YPos, 14);
        Inc(YPos, 16);
      end;
    end
    else
    begin
      XPos := 0;
      while XPos < Width do
      begin
        FGUIDesigner.vilControls1616.Draw(Canvas, XPos, 2, 14);
        Inc(XPos, 16);
      end;
    end;
    Canvas.Pen.Color := RGB(204, 204, 204);
    Canvas.MoveTo(0, Height - 1);
    Canvas.LineTo(Width, Height - 1);
  end;
end;

procedure TJToolBar.SetBorderPainted(AValue: Boolean);
begin
  if AValue <> FBorderPainted then
  begin
    FBorderPainted := AValue;
    Invalidate;
  end;
end;

procedure TJToolBar.SetOrientation(AValue: TOrientation);
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

end.
