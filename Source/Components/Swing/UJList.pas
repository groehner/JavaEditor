unit UJList;

interface

uses
  Classes, Graphics, StdCtrls, UJEComponents, UJComponents;

type

  TJListLayoutOrientation = (VERTICAL, HORIZONTAL_WRAP, VERTICAL_WRAP);
  TJListSelectionMode =
    (SINGLE_INTERVAL_SELECTION, MULTIPLE_INTERVAL_SELECTION, SINGLE_SELECTION);

  TJList = class (TSwingComponent)
  private
    FItems: TStrings;
    FSelectedIndex: integer;
    FSelectionMode: TJListSelectionMode;
    FLayoutOrientation: TJListLayoutOrientation;
    FSelectionBackground: TColor;
    FSelectionForeGround: TColor;
    FVisibleRowCount: integer;
    FFixedCellHeight: integer;
    FFixedCellWidth: integer;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    procedure setSelectedIndex(aIndex: integer);
    procedure setItems(aItems: TStrings);
    procedure setFixedCellHeight(aValue: integer);
    procedure setLayoutOrientation(aValue: TJListLayoutOrientation);
    procedure setSelectionBackground(aValue: TColor);
    procedure setSelectionForeground(aValue: TColor);
    procedure setVisibleRowCount(aValue: integer);
    procedure setHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure setVerticalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aListBox: TListBox);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    function GetContainerAdd: string; override;
    procedure SetPositionAndSize; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property SelectionMode: TJListSelectionMode read FSelectionMode write FSelectionMode;
    property FixedCellHeight: integer read FFixedCellHeight write setFixedCellHeight;
    property FixedCellWidth: integer read FFixedCellWidth write FFixedCellWidth;
    property LayoutOrientation: TJListLayoutOrientation read FLayoutOrientation write setLayoutOrientation;
    property SelectionBackground: TColor read FSelectionBackground write setSelectionBackground;
    property SelectionForeground: TColor read FSelectionForeground write setSelectionForeground;
    property VisibleRowCount: integer read FVisibleRowCount write setVisibleRowCount;
    property HorizontalScrollBarPolicy: TScrollBarPolicy read FHorizontalScrollBarPolicy write setHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy read FVerticalScrollBarPolicy write setVerticalScrollBarPolicy;
    property SelectedIndex: integer read FSelectedIndex write setSelectedIndex;
    property Items: TStrings read FItems write setItems;
  end;

implementation

uses Math, Controls, SysUtils, Types;

{--- TJList -------------------------------------------------------------------}

constructor TJList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +8;
  Width := 120;
  Height:= 80;
  Background:= clWhite;
  FItems:= TStringList.Create;
  FItems.Text:= defaultItems;
  FSelectedIndex:= -1;
  FLayoutOrientation:= VERTICAL;
  FSelectionMode:= MULTIPLE_INTERVAL_SELECTION;
  FSelectionBackground:= SelectionColor;
  FSelectionForeground:= DefaultForeground;
  FHorizontalScrollBarPolicy:= AS_NEEDED;
  FVerticalScrollBarPolicy:= AS_NEEDED;
  FVisibleRowCount:= 8;
  FFixedCellHeight:= -1;
  FFixedCellWidth:= -1;
  JavaType:= 'JList';
end;

constructor TJList.CreateFrom(aListBox: TListBox);
begin
  Create(aListBox.Owner);
  CreateFromJ(aListBox);
  Font:= aListBox.Font;
  Background:= aListBox.Color;
  Items.AddStrings(aListBox.Items);
  FSelectedIndex:= -1;
  SelectedIndex:= aListBox.Columns;
end;

function TJList.getAttributes(ShowAttributes: integer): string;
  const
    list1 = '|SelectedIndex|Items';
    list2 = '|SelectionMode|LayoutOrientation|VisibleRowCount' +
            '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
    list3 = '|FixedCellHeight|FixedCellWidth|SelectionBackground|SelectionForeground';
begin
  case ShowAttributes of
    1: Result:= list1;
    2: Result:= list1 + list2;
  else Result:= list1 + list2 + list3;
  end;
  Result:= Result + inherited;
end;

procedure TJList.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else if Attr = 'Items' then
    MakeList
  else if Attr = 'LayoutOrientation' then
    MakeAttribut(Attr, 'JList.' + Value)
  else if Attr = 'SelectionMode' then
    MakeAttribut(Attr, 'ListSelectionModel.' + Value)
  else
    inherited;
end;

function TJList.getEvents(ShowEvents: integer): string;
begin
  Result:= '|valueChanged' + inherited getEvents(ShowEvents);
end;

procedure TJList.NewControl;
begin
  InsertNewVariable('private JList<String> ' + Name + ' = new JList<>();');
  InsertNewVariable('  private DefaultListModel<String> ' + Name + 'Model = new DefaultListModel<>();');
  InsertNewVariable('  private JScrollPane '+ Name + 'ScrollPane = new JScrollPane(' + Name + ');');
  MakeAttribut('Model', Name + 'Model');
  setAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane' + getBounds);
  Partner.InsertComponent(Indent2 + GetContainerAdd);
  MakeList;
  MakeFont;
end;

procedure TJList.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private JList<String> ' + Name);
  Partner.DeleteAttribute('private DefaultListModel<String> ' + Name + 'Model');
  Partner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  Partner.DeleteAttributeValues(Name + 'Model.addElement');
  Partner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJList.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(', Name + 'ScrollPane' + getBounds);
end;

procedure TJList.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'ScrollPane' , NewName + 'ScrollPane', true);
  Partner.ReplaceWord(OldName + 'Model' , NewName + 'Model', true);
end;

function TJList.GetContainerAdd: string;
begin
  Result:= AWTSwingContainer;
  if Result = ''
    then Result:= 'add(' + name + 'ScrollPane);'
    else Result:= Result + '.add(' + name + 'ScrollPane);';
end;

procedure TJList.MakeList;
  var i: integer; s: string;
begin
  Partner.DeleteAttributeValues(Name + 'Model.addElement(');
  s:= '';
  for i:= 0 to Items.Count - 1 do
    s:= s + surroundFix(Indent1 + Name + 'Model.addElement(' + asString(Items[i]) + ');');
  Partner.InsertAttributValue(getContainerAdd , s, 0);
end;

destructor TJList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJList.Paint;
  var x, y, dx, dy, th, tw, wrap, i, lwidth, lheight, rows, columns: integer; s: string;
      R1: TRect; vsb, hsb: boolean;

  procedure CalcWrap;
  begin
    case LayoutOrientation of
      VERTICAL: begin
        wrap:= FItems.Count;
        rows:= wrap;
        columns:= 1;
      end;
      VERTICAL_WRAP: begin
        if VisibleRowCount <= 0
          then wrap:= lheight div dy
          else wrap:= VisibleRowCount;
        rows:= wrap;
        columns:= FItems.count div wrap;
        if (wrap <> 0) and (FItems.count mod wrap > 0) then inc(columns);
      end;
      HORIZONTAL_WRAP: begin
        if VisibleRowCount <= 0
          then wrap:= lwidth div dx
          else wrap:= FItems.Count div VisibleRowCount + 1;
        columns:= wrap;
        rows:= FItems.Count div wrap;
        if (wrap <> 0) and (FItems.count mod wrap > 0) then inc(rows);
      end;
    end;
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));

  th:= Canvas.TextHeight('Hg');
  vsb:= (VerticalScrollBarPolicy = ALWAYS);
  hsb:= (HorizontalScrollBarPolicy = ALWAYS);
  if hsb then lwidth := Width - 19  else lwidth:= Width - 3;
  if vsb then lheight:= Height - 19 else lheight:= Height - 3;
  
  if FFixedCellHeight = -1
    then dy:= th + 4
    else dy:= FFixedCellHeight;

  if FItems.Count > 0
    then tw:= Canvas.TextWidth(FItems[0])
    else tw:= 0;
  for i := 1 to FItems.Count - 1 do begin
    dx:= Canvas.TextWidth(FItems[i]);
    if dx > tw then tw:= dx;
  end;
  if FFixedCellWidth = -1
    then dx:= tw + 3
    else dx:= FFixedCellWidth;

  CalcWrap;
  if not vsb and (VerticalScrollBarPolicy <> NEVER) and (rows * dy > lheight) then begin
    dec(lwidth, 16);
    vsb:= true;
  end;
  if not hsb and (HorizontalScrollBarPolicy <> NEVER )and (columns * dx > lwidth) then begin
    dec(lheight, 16);
    hsb:= true;
  end;
  CalcWrap;

  x:= -dx; y:= -dy;
  for i:= 0 to FItems.Count - 1 do begin
    case LayoutOrientation of
      VERTICAL: begin
        x:= 0;
        inc(y, dy);
      end;
      VERTICAL_WRAP: begin
        if (wrap <> 0) and (i mod wrap = 0) then begin
          y:= 0; inc(x, dx);
        end else
          inc(y, dy);
      end;
      HORIZONTAL_WRAP: begin
        if (wrap <> 0) and (i mod wrap = 0) then begin
          x:= 0; inc(y, dy);
        end else
          inc(x, dx);
      end;
    end;
    if (x < lwidth) and (y < lheight) then begin
      R1:= Rect(x+2, y+2, min(x + dx-2, lwidth), min(y + dy-2, lheight));
      s:= FItems.Strings[i];
      if i = FSelectedIndex then begin
        Canvas.Brush.Color:= SelectionColor;
        Canvas.Pen.Color:= SelectionColor;
        Canvas.Font.Color:= DefaultForeground;
        Canvas.Rectangle(R1);
      end;
      Canvas.TextRect(R1, x+1, y + (dy -th) div 2, s);
      if i = FSelectedIndex then begin
        Canvas.Brush.Color:= Background;
        Canvas.Pen.Color:= Background;
        Canvas.Font.Color:= Foreground;
      end;
    end;
  end;

  // paint scrollbars
  if hsb and vsb then begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.FillRect(Rect(Width-17, Height-17, Width-2, Height-2));
    ScrollBar(Rect(Width-17, 1, Width-2, Height-17), false, true); // vsb
    ScrollBar(Rect(1, Height-17, Width-17, Height-2), true, true); // hsb
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-17);
    Canvas.LineTo(Width-17, Height-17);
    Canvas.LineTo(Width-17, 0);
  end else if hsb then begin
    ScrollBar(Rect(1, Height-17, Width-2, Height-2), true, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-17);
    Canvas.LineTo(Width-1, Height-17);
  end else if vsb then begin
    ScrollBar(Rect(Width-17, 1, Width-2, Height-2), false, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(Width-17, Height-17);
    Canvas.LineTo(Width-17, 0);
  end;
end;

procedure TJList.setFixedCellHeight(aValue: integer);
begin
  if aValue <> FFixedCellHeight then begin
    FFixedCellHeight:= aValue;
    Invalidate;
  end;
end;

procedure TJList.setLayoutOrientation(aValue: TJListLayoutOrientation);
begin
  if aValue <> FLayoutOrientation then begin
    FLayoutOrientation:= aValue;
    Invalidate;
  end;
end;

procedure TJList.setSelectionBackground(aValue: TColor);
begin
  if aValue <> FSelectionBackground then begin
    FSelectionBackground:= aValue;
    Invalidate;
  end;
end;

procedure TJList.setSelectionForeground(aValue: TColor);
begin
  if aValue <> FSelectionForeground then begin
    FSelectionForeground:= aValue;
    Invalidate;
  end;
end;

procedure TJList.setVisibleRowCount(aValue: integer);
begin
  if (aValue <> FVisibleRowCount) and (aValue >= -1) then begin
    FVisibleRowCount:= aValue;
    Invalidate;
  end;
end;

procedure TJList.setItems(aItems: TStrings);
begin
  if aItems.Text <> FItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TJList.setSelectedIndex(aIndex: integer);
begin
  if (csLoading in ComponentState) then
    FSelectedIndex:= aIndex
  else if (aIndex <> FSelectedIndex) and (aIndex < FItems.Count) then begin
    FSelectedIndex:= aIndex;
    Invalidate;
  end;
end;

procedure TJList.setHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FHorizontalScrollBarPolicy then begin
    FHorizontalScrollBarPolicy:= Value;
    Invalidate;
  end;
end;

procedure TJList.setVerticalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FVerticalScrollBarPolicy then begin
    FVerticalScrollBarPolicy:= Value;
    Invalidate;
  end;
end;

end.
