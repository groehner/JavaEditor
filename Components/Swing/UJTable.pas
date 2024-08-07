unit UJTable;

interface

uses
  Classes, Grids, UJEComponents, UJComponents;

type

  TJTable = class (TSwingComponent)
  private
    FColumns: TStrings;
    FColCount: integer;
    FRowCount: integer;
    FRowHeight: integer;
    FShowHorizontalLines: boolean;
    FShowVerticalLines: boolean;
    FShowGrid: boolean;
    FFillsViewportHeight: boolean;
    FAutoCreateRowSorter: boolean;
    FRowSelectionAllowed: boolean;
    FColumnSelectionAllowed: boolean;
    FCellSelectionEnabled: boolean;
    FDragEnabled: boolean;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    function  getStrings: TStrings;
    procedure setStrings(Strings: TStrings);
    procedure setRowHeight(aValue: integer);
    procedure setShowHorizontalLines(aValue: boolean);
    procedure setShowVerticalLines(aValue: boolean);
    procedure setShowGrid(aValue: boolean);
    procedure setColCount(aValue: integer);
    procedure setRowCount(aValue: integer);
    procedure setFillsViewportHeight(aValue: boolean);
    procedure setHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure setVerticalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure MakeTable;
  public
    constructor Create (AOwner: TComponent); override;
    constructor CreateFrom(aStringGrid: TStringGrid);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure SetPositionAndSize; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function GetContainerAdd: string; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Columns: TStrings read getStrings write setStrings;
    property ColCount: integer read FColCount write setColCount;
    property RowCount: integer read FRowCount write setRowCount;
    property HorizontalScrollBarPolicy: TScrollBarPolicy read FHorizontalScrollBarPolicy write setHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy read FVerticalScrollBarPolicy write setVerticalScrollBarPolicy;
    property ShowGrid: boolean read FShowGrid write setShowGrid;

    property RowHeight: integer read FRowHeight write setRowHeight;
    property ShowHorizontalLines: boolean read FShowHorizontalLines write setShowHorizontalLines;
    property ShowVerticalLines: boolean read FShowVerticalLines write setShowVerticalLines;
    property FillsViewportHeight: boolean read FFillsViewportHeight write setFillsViewportHeight;
    property AutoCreateRowSorter: boolean read FAutoCreateRowSorter write FAutoCreateRowSorter;
    property RowSelectionAllowed: boolean read FRowSelectionAllowed write FRowSelectionAllowed;
    property ColumnSelectionAllowed: boolean read FColumnSelectionAllowed write FColumnSelectionAllowed;
    property CellSelectionEnabled: boolean read FCellSelectionEnabled write FCellSelectionEnabled;
    property DragEnabled: boolean read FDragEnabled write FDragEnabled;
  end;

implementation

uses SysUtils, Graphics, Controls, Math;

const paddingH = 11;
      paddingV = 3;

constructor TJTable.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 19;
  Width:= 240;
  Height:= 120;
  Foreground:= DefaultForeground;
  Background:= clWhite;
  FColumns:= TStringList.Create;
  FColumns.Text:= 'Title 1'#13#10'Title 2'#13#10'Title 3'#13#10'Title 4'#13#10'Title 5';
  FColCount:= 5;
  FRowCount:= 5;
  FRowHeight:= 16;
  FShowHorizontalLines:= true;
  FShowVerticalLines:= true;
  FShowGrid:= true;
  FRowSelectionAllowed:= true;
  FColumnSelectionAllowed:= false;
  FCellSelectionEnabled:= false;
  FHorizontalScrollBarPolicy:= AS_NEEDED;
  FVerticalScrollBarPolicy:= AS_NEEDED;    
  Font.Style:= [];
  JavaType:= 'JTable';
end;

constructor TJTable.CreateFrom(aStringGrid: TStringGrid);
begin
  Create(aStringGrid.Owner);
  CreateFromJ(aStringGrid);
  ColCount:= aStringGrid.ColCount;
  RowCount:= aStringGrid.RowCount;
  RowHeight:= 16; // aStringGrid.DefaultRowHeight;
  Background:= aStringGrid.Color;
  ShowGrid:= aStringGrid.Ctl3D;
  ShowHorizontalLines:= aStringGrid.DefaultDrawing;
  ShowVerticalLines:= aStringGrid.ParentShowHint;
end;

destructor TJTable.Destroy;
begin
  FreeAndNil(FColumns);
  inherited;
end;

function TJTable.getAttributes(ShowAttributes: integer): string;
  const
    show1 = '|Columns|ColCount|RowCount|ShowGrid' +
             '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
    show2 = '|RowHeight|ShowHorizontalLines|ShowVerticalLines|FillsViewportHeight' +
             '|AutoCreateRowSorter|RowSelectionAllowed|ColumnSelectionAllowed|'+
             '|CellSelectionEnabled|DragEnabled';
begin
  if ShowAttributes = 1
    then Result:= show1
    else Result:= show1 + show2;
  Result:= Result + inherited;
end;

procedure TJTable.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Columns') or (Attr = 'ColCount') or (Attr = 'RowCount') then
    MakeTable
  else if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else
    inherited;
end;

procedure TJTable.MakeTable;
  var key, s: string; i: integer;
begin
  key:= 'private JTable ' + Name;
  s:= Indent1 + Key + ' = new JTable(' + IntToStr(RowCount) + ', ' + IntToStr(ColCount) + ');';
  Partner.ReplaceAttribute(key, s);

  Partner.DeleteAttributeValues(Name + '.getColumnModel()');
  s:= '';
  for i:= 0 to min(Columns.Count, ColCount) - 1 do
    s:= s + surroundFix(Indent1 + Name + '.getColumnModel().getColumn(' + IntToStr(i) +
            ').setHeaderValue("' + Columns[i] + '");');
  Partner.InsertAttributValue(getContainerAdd , s, 0);
end;

procedure TJTable.NewControl;
  var s: string; i: integer;
begin
  Partner.InsertImport('javax.swing.table.*');
  InsertNewVariable('private JTable ' + Name + ' = new JTable(5, 5);');
  InsertNewVariable('  private DefaultTableModel ' + Name + 'Model = (DefaultTableModel) ' + Name + '.getModel();');
  InsertNewVariable('  private JScrollPane ' + Name + 'ScrollPane = new JScrollPane(' + Name + ');');
  setAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane' + getBounds);
  s:= '';
  for i:= 0 to 4 do
    s:= s + surroundFix('  '+  Name + '.getColumnModel().getColumn(' + IntToStr(i) +
            ').setHeaderValue("Title ' + IntToStr(i+1) + '");');
  s:= s + surroundFix('  ' + GetContainerAdd);
  Partner.InsertComponent(s);
  MakeFont;
end;

procedure TJTable.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private DefaultTableModel ' + Name + 'Model');
  Partner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  Partner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJTable.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(', Name + 'ScrollPane' + getBounds);
end;

procedure TJTable.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'ScrollPane' , NewName + 'ScrollPane', true);
  Partner.ReplaceWord(OldName + 'Model' , NewName + 'Model', true);
end;

function TJTable.GetContainerAdd: string;
begin
  Result:= AWTSwingContainer;
  if Result = ''
    then Result:= 'add(' + name + 'ScrollPane);'
    else Result:= Result + '.add(' + name + 'ScrollPane);';
end;

procedure TJTable.Paint;
  const cHeaderHeight = 16;
  var i, x, y, dx, dy, tw, th, lWidth, HeaderHeight: integer;
      s: string; vsb, hsb: boolean;
begin
  CanvasFontAssign;
  HeaderHeight:= PPIScale(cHeaderHeight);
  Canvas.Font.Color:= DefaultForeground;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= DefaultBackground;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  Canvas.Brush.Color:= Background;
  if FillsViewportHeight
    then Canvas.FillRect(Rect(1, HeaderHeight + 1, Width-1, Height-1))
    else Canvas.FillRect(Rect(1, HeaderHeight + 1, Width-1, FRowHeight*(FRowCount+1)));

  Canvas.Brush.Color:= DefaultBackground;
  Canvas.Pen.Color:= DarkShadow;
  th:= Canvas.TextHeight('Hg');
  vsb:= (VerticalScrollBarPolicy = ALWAYS);
  hsb:= (HorizontalScrollBarPolicy = ALWAYS);
  if (HeaderHeight + RowCount*RowHeight > Height) and
     not (VerticalScrollBarPolicy = NEVER) then
    vsb:= true;
  if vsb then lwidth := Width - PPIScale(19) else lwidth:= Width - PPIScale(3);

  // header row
  x:= 0;
  dx:= lWidth div FColCount;
  dy:= HeaderHeight;
  for i:= 1 to FColCount do begin
    if i <= FColumns.Count
      then s:= FColumns[i-1]
      else s:= '';
    tw:= Canvas.TextWidth(s);
    Canvas.TextRect(Rect(x+1, 1, x + dx-1, HeaderHeight-1), x + (dx-tw) div 2 , (HeaderHeight-th) div 2 +1, s);
    x:= Round(((lWidth*1.0)/FColCount)*i);
    Canvas.MoveTo(x, 0);
    Canvas.LineTo(x, dy);
  end;
  //Canvas.MoveTo(Width-1, 0);
  //Canvas.LineTo(Width-1, dy);

  if FShowVerticalLines then begin
    dy:= FRowHeight*(FRowCount+1);
    for i:= 1 to FColCount do begin
      x:= Round(((lWidth*1.0)/FColCount)*i);
      Canvas.MoveTo(x, HeaderHeight);
      Canvas.LineTo(x, dy);
    end;
  end;

  if FShowHorizontalLines then begin
    y:= HeaderHeight;
    for i:= 1 to FRowCount + 1 do begin
      Canvas.MoveTo(0, y);
      Canvas.LineTo(lWidth, y);
      y:= y + FRowHeight;
    end;
  end;

  // paint scrollbars
  var p17:= PPIScale(17);
  if hsb and vsb then begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.FillRect(Rect(Width-p17, Height-p17, Width-1, Height-1));
    ScrollBar(Rect(Width-p17, 1, Width-1, Height-p17), false, true); // vsb
    ScrollBar(Rect(1, Height-p17, Width-p17, Height-1), true, true); // hsb
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-p17);
    Canvas.LineTo(Width-p17, Height-p17);
    Canvas.LineTo(Width-p17, 0);
    Canvas.MoveTo(1, Height-1);
    Canvas.LineTo(Width-1, Height-1);
    Canvas.LineTo(Width-1, 0);
  end else if hsb then begin
    ScrollBar(Rect(1, Height-p17, Width-1, Height-1), true, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-p17);
    Canvas.LineTo(Width-1, Height-p17);
  end else if vsb then begin
    ScrollBar(Rect(Width-p17, 1, Width-1, Height-1), false, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(Width-p17, Height-p17);
    Canvas.LineTo(Width-p17, 0);
  end;
end;

function TJTable.getStrings: TStrings;
begin
  Result:= FColumns;
end;

procedure TJTable.setStrings(Strings: TStrings);
begin
  FColumns.Assign(Strings);
  FColCount:= max(FColumns.Count, 1);
  Invalidate;
end;

procedure TJTable.SetRowHeight(aValue: integer);
begin
  if aValue <> FRowHeight then begin
    FRowHeight:= aValue;
    Invalidate;
  end;
end;

procedure TJTable.setShowHorizontalLines(aValue: boolean);
begin
  if aValue <> FShowHorizontalLines then begin
    FShowHorizontalLines:= aValue;
    FShowGrid:= FShowVerticalLines and FShowHorizontalLines;
    Invalidate;
  end;
end;

procedure TJTable.setShowVerticalLines(aValue: boolean);
begin
  if aValue <> FShowVerticalLines then begin
    FShowVerticalLines:= aValue;
    FShowGrid:= FShowVerticalLines and FShowHorizontalLines;
    Invalidate;
  end;
end;

procedure TJTable.setShowGrid(aValue: boolean);
begin
  if aValue <> FShowGrid then begin
    FShowGrid:= aValue;
    FShowHorizontalLines:= aValue;
    FShowVerticalLines:= aValue;
    Invalidate;
  end;
end;

procedure TJTable.setColCount(aValue: integer);
begin
  if aValue <> FColCount then begin
    FColCount:= max(aValue, 1);
    Invalidate;
  end;
end;

procedure TJTable.setRowCount(aValue: integer);
begin
  if aValue <> FRowCount then begin
    FRowCount:= aValue;
    Invalidate;
  end;
end;

procedure TJTable.setFillsViewportHeight(aValue: boolean);
begin
  if aValue <> FFillsViewportHeight then begin
    FFillsViewportHeight:= aValue;
    Invalidate;
  end;
end;

procedure TJTable.setHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FHorizontalScrollBarPolicy then begin
    FHorizontalScrollBarPolicy:= Value;
    Invalidate;
  end;
end;

procedure TJTable.setVerticalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FVerticalScrollBarPolicy then begin
    FVerticalScrollBarPolicy:= Value;
    Invalidate;
  end;
end;

end.
