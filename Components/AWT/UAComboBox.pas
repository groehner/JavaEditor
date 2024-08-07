unit UAComboBox;

interface

uses
  Classes, StdCtrls, UAComponents;

type

  TAComboBox = class (TAWTComponent)
  private
    FFocusable: boolean;
    FItems: TStrings;
    FSelectedIndex: integer;
    procedure setSelectedIndex(aIndex: integer);
    procedure setItems(aItems: TStrings);
    procedure MakeList;
  protected
    FEditable: boolean;
    procedure setEditable(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aComboBox: TComboBox);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Focusable: boolean read FFocusable write FFocusable default true;
    property SelectedIndex: integer read FSelectedIndex write setSelectedIndex;
    property Items: TStrings read FItems write setItems;
    property itemStateChanged;
  end;

implementation

uses Math, Types, Controls, Graphics, SysUtils;

{--- TAComboBox ---------------------------------------------------------------}

constructor TAComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= -9;
  Width:= 80;
  Height:= 24;
  FItems:= TStringList.Create;
  FItems.Text:= defaultItems;
  FSelectedIndex:= 0;
  FFocusable:= true;
  JavaType:= 'Choice';
end;

constructor TAComboBox.CreateFrom(aComboBox: TComboBox);
begin
  Create(aComboBox.Owner);
  CreateFromA(aComboBox);
  Font:= aComboBox.Font;
  Background:= aComboBox.Color;
  if Background = clBtnFace then Background:= clWhite;
  Items.AddStrings(aComboBox.Items);
  FSelectedIndex:= Math.min(aComboBox.maxLength, aComboBox.Items.Count-1);
end;

function TAComboBox.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Focusable|SelectedIndex|Items' + inherited getAttributes(ShowAttributes);
end;

procedure TAComboBox.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectedIndex' then
    MakeSelectedIndex(Value)
  else
    inherited;
end;

function TAComboBox.getEvents(ShowEvents: integer): string;
begin
  Result:= '|itemStateChanged' + inherited getEvents(ShowEvents);
end;

procedure TAComboBox.MakeList;
  var i: integer; s: string;
begin
  Partner.DeleteAttributeValues(Name + '.add(');
  s:= '';
  for i:= 0 to Items.Count - 1 do
    s:= s + surroundFix(Indent1 + Name + '.add(' + asString(Items[i]) + ');');
  Partner.InsertAttributValue(getContainerAdd, s, 0);
  if Items.Count > 0
    then FSelectedIndex:= 0
    else FSelectedIndex:= -1;
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
  var x, y, dxy, th: integer; s: string;
      Points: array[0..2] of TPoint; R1: TRect;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= AWTGray;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-18, Height));

  Canvas.Pen.Color:= DefaultForeground;
  Canvas.Brush.Color:= DefaultForeground;
  dxy:= 3;
  x:= Width - 10;
  y:= (Height + dxy) div 2;
  Points[0]:= Point(x, y);
  Points[1]:= Point(x - dxy, y - dxy);
  Points[2]:= Point(x + dxy, y - dxy);
  Canvas.Polygon(Points);

  if (FSelectedIndex > -1) and (FSelectedIndex < FItems.Count) then begin
    Canvas.Font.Color:= Foreground;
    Canvas.Brush.Style:= bsClear;
    s:= FItems.Strings[FSelectedIndex];
    th:= Canvas.TextHeight(s);
    R1:= Rect(0, 0, Width - 20, Height);
    Canvas.TextRect(R1, 2, (Height - th) div 2, s);
  end;
end;

procedure TAComboBox.setItems(aItems: TStrings);
begin
  if aItems.Text <> FItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TAComboBox.setSelectedIndex(aIndex: integer);
begin
  if aIndex <> FSelectedIndex then begin
    FSelectedIndex:= aIndex;
    Invalidate;
  end;
end;

procedure TAComboBox.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

end.
