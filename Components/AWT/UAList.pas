unit UAList;

interface

uses
  Classes, StdCtrls, UAComponents;

type

  TAList = class (TAWTComponent)
  private
    FItems: TStrings;
    FSelectedIndex: integer;
    FMultipleMode: boolean; // non visible property
    procedure setSelectedIndex(aIndex: integer);
    procedure setItems(aItems: TStrings);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aListBox: TListBox);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property MultipleMode: boolean read FMultipleMode write FMultipleMode default false;
    property SelectedIndex: integer read FSelectedIndex write setSelectedIndex;
    property Items: TStrings read FItems write setItems;

    property actionPerformed;
    property itemStateChanged;
  end;

implementation

uses SysUtils, Graphics, Controls;

{--- TAList -------------------------------------------------------------------}

constructor TAList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= -8;
  Width := 120;
  Height:= 80;
  FItems:= TStringList.Create;
  FItems.Text:= defaultItems;
  FSelectedIndex:= -1;
  JavaType:= 'List';
end;

constructor TAList.CreateFrom(aListBox: TListBox);
begin
  Create(aListBox.Owner);
  CreateFromA(aListBox);
  Font:= aListBox.Font;
  Background:= aListBox.Color;
  Items.AddStrings(aListBox.Items);
  FSelectedIndex:= -1;
end;

function TAList.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|MultipleMode|SelectedIndex|Items' + inherited getAttributes(ShowAttributes);
end;

procedure TAList.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectedIndex' then
    MakeSelectedIndex(Value)
  else
    inherited;
end;

function TAList.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed|itemStateChanged' + inherited getEvents(ShowEvents);
end;

procedure TAList.MakeList;
  var i: integer; s: string;
begin
  Partner.DeleteAttributeValues(Name + '.add(');
  s:= '';
  for i:= 0 to Items.Count - 1 do
    s:= s + surroundFix(Indent1 + Name + '.add(' + asString(Items[i]) + ');');
  Partner.InsertAttributValue(GetContainerAdd, s, 0);
end;

procedure TAList.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private List ' + Name + ' = new List();');
  MakeList;
end;

destructor TAList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TAList.Paint;
  var y, th, i: integer; 
begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= AWTDarkGray;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  y:= 2;
  th:= Canvas.TextHeight('Hg');
  for i:= 0 to FItems.Count - 1 do begin
    if i = FSelectedIndex then begin
      Canvas.Brush.Color:= AWTSelectionColor;
      Canvas.Pen.Color:= AWTSelectionColor;
      Canvas.Font.Color:= clWhite;
      Canvas.Rectangle(Rect(0, y, Width, y + th + 0));
      Canvas.TextOut(3, y + 0, FItems.Strings[i]);
      Canvas.Brush.Color:= Background;
      Canvas.Pen.Color:= Background;
      Canvas.Font.Color:= Foreground;
    end else
      Canvas.TextOut(3, y + 0, FItems.Strings[i]);
    inc(y, th);
    if y + th > Height then begin
      ScrollBar(Rect(Width-17, 1, Width-1, Height-1), false, false);
      break;
    end;
  end;
end;

procedure TAList.setItems(aItems: TStrings);
begin
  if aItems.Text <> FItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TAList.setSelectedIndex(aIndex: integer);
begin
  if aIndex <> FSelectedIndex then begin
    FSelectedIndex:= aIndex;
    Invalidate;
  end;
end;

end.
