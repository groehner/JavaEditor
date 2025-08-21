unit UAComboBox;

interface

uses
  Classes,
  StdCtrls,
  UAComponents;

type

  TAComboBox = class(TAWTComponent)
  private
    FFocusable: Boolean;
    FItems: TStrings;
    FSelectedIndex: Integer;
    procedure SetSelectedIndex(AIndex: Integer);
    procedure SetItems(AItems: TStrings);
    procedure MakeList;
  protected
    FEditable: Boolean;
    procedure SetEditable(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Focusable: Boolean read FFocusable write FFocusable default True;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Items: TStrings read FItems write SetItems;
    property itemStateChanged;
  end;

implementation

uses
  Math,
  Types,
  Controls,
  Graphics,
  SysUtils;

{ --- TAComboBox --------------------------------------------------------------- }

constructor TAComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -9;
  Width := 80;
  Height := 24;
  FItems := TStringList.Create;
  FItems.Text := defaultItems;
  FSelectedIndex := 0;
  FFocusable := True;
  JavaType := 'Choice';
end;

function TAComboBox.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Focusable|SelectedIndex|Items' + inherited GetAttributes
    (ShowAttributes);
end;

procedure TAComboBox.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectedIndex' then
    MakeSelectedIndex(Value)
  else
    inherited;
end;

function TAComboBox.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|itemStateChanged' + inherited GetEvents(ShowEvents);
end;

procedure TAComboBox.MakeList;
var
  Str: string;
begin
  FPartner.DeleteAttributeValues(Name + '.add(');
  Str := '';
  for var I := 0 to Items.Count - 1 do
    Str := Str + surroundFix(Indent1 + Name + '.add(' +
      AsString(Items[I]) + ');');
  FPartner.InsertAttributValue(GetContainerAdd, Str, 0);
  if Items.Count > 0 then
    FSelectedIndex := 0
  else
    FSelectedIndex := -1;
end;

procedure TAComboBox.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Choice ' + Name + ' = new Choice();');
  MakeList;
end;

destructor TAComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TAComboBox.Paint;
var
  XPos, YPos, Dxy, TextH: Integer;
  Str: string;
  Points: array [0 .. 2] of TPoint;
  Rect1: TRect;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := AWTGray;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 18, Height));

  Canvas.Pen.Color := DefaultForeground;
  Canvas.Brush.Color := DefaultForeground;
  Dxy := 3;
  XPos := Width - 10;
  YPos := (Height + Dxy) div 2;
  Points[0] := Point(XPos, YPos);
  Points[1] := Point(XPos - Dxy, YPos - Dxy);
  Points[2] := Point(XPos + Dxy, YPos - Dxy);
  Canvas.Polygon(Points);

  if (FSelectedIndex > -1) and (FSelectedIndex < FItems.Count) then
  begin
    Canvas.Font.Color := Foreground;
    Canvas.Brush.Style := bsClear;
    Str := FItems[FSelectedIndex];
    TextH := Canvas.TextHeight(Str);
    Rect1 := Rect(0, 0, Width - 20, Height);
    Canvas.TextRect(Rect1, 2, (Height - TextH) div 2, Str);
  end;
end;

procedure TAComboBox.SetItems(AItems: TStrings);
begin
  if AItems.Text <> FItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TAComboBox.SetSelectedIndex(AIndex: Integer);
begin
  if AIndex <> FSelectedIndex then
  begin
    FSelectedIndex := AIndex;
    Invalidate;
  end;
end;

procedure TAComboBox.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

end.
