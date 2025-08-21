unit UJTable;

interface

uses
  Classes,
  Grids,
  UJEComponents,
  UJComponents;

type

  TJTable = class(TSwingComponent)
  private
    FColumns: TStrings;
    FColCount: Integer;
    FRowCount: Integer;
    FRowHeight: Integer;
    FShowHorizontalLines: Boolean;
    FShowVerticalLines: Boolean;
    FShowGrid: Boolean;
    FFillsViewportHeight: Boolean;
    FAutoCreateRowSorter: Boolean;
    FRowSelectionAllowed: Boolean;
    FColumnSelectionAllowed: Boolean;
    FCellSelectionEnabled: Boolean;
    FDragEnabled: Boolean;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    function GetColumns: TStrings;
    procedure SetColumns(Strings: TStrings);
    procedure SetRowHeight(AValue: Integer);
    procedure SetShowHorizontalLines(AValue: Boolean);
    procedure SetShowVerticalLines(AValue: Boolean);
    procedure SetShowGrid(AValue: Boolean);
    procedure SetColCount(AValue: Integer);
    procedure SetRowCount(AValue: Integer);
    procedure SetFillsViewportHeight(AValue: Boolean);
    procedure SetHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure SetVerticalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure MakeTable;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure SetPositionAndSize; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function GetContainerAdd: string; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Columns: TStrings read GetColumns write SetColumns;
    property ColCount: Integer read FColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount;
    property HorizontalScrollBarPolicy: TScrollBarPolicy
      read FHorizontalScrollBarPolicy write SetHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy
      read FVerticalScrollBarPolicy write SetVerticalScrollBarPolicy;
    property ShowGrid: Boolean read FShowGrid write SetShowGrid;

    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property ShowHorizontalLines: Boolean read FShowHorizontalLines
      write SetShowHorizontalLines;
    property ShowVerticalLines: Boolean read FShowVerticalLines
      write SetShowVerticalLines;
    property FillsViewportHeight: Boolean read FFillsViewportHeight
      write SetFillsViewportHeight;
    property AutoCreateRowSorter: Boolean read FAutoCreateRowSorter
      write FAutoCreateRowSorter;
    property RowSelectionAllowed: Boolean read FRowSelectionAllowed
      write FRowSelectionAllowed;
    property ColumnSelectionAllowed: Boolean read FColumnSelectionAllowed
      write FColumnSelectionAllowed;
    property CellSelectionEnabled: Boolean read FCellSelectionEnabled
      write FCellSelectionEnabled;
    property DragEnabled: Boolean read FDragEnabled write FDragEnabled;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Controls,
  Math;

const
  CPaddingH = 11;
  CPaddingV = 3;

constructor TJTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 19;
  Width := 240;
  Height := 120;
  Foreground := DefaultForeground;
  Background := clWhite;
  FColumns := TStringList.Create;
  FColumns.Text :=
    'Title 1'#13#10'Title 2'#13#10'Title 3'#13#10'Title 4'#13#10'Title 5';
  FColCount := 5;
  FRowCount := 5;
  FRowHeight := 16;
  FShowHorizontalLines := True;
  FShowVerticalLines := True;
  FShowGrid := True;
  FRowSelectionAllowed := True;
  FColumnSelectionAllowed := False;
  FCellSelectionEnabled := False;
  FHorizontalScrollBarPolicy := AS_NEEDED;
  FVerticalScrollBarPolicy := AS_NEEDED;
  Font.Style := [];
  JavaType := 'JTable';
end;

destructor TJTable.Destroy;
begin
  FreeAndNil(FColumns);
  inherited;
end;

function TJTable.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|Columns|ColCount|RowCount|ShowGrid' +
    '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
  Show2 = '|RowHeight|ShowHorizontalLines|ShowVerticalLines|FillsViewportHeight'
    + '|AutoCreateRowSorter|RowSelectionAllowed|ColumnSelectionAllowed|' +
    '|CellSelectionEnabled|DragEnabled';
begin
  if ShowAttributes = 1 then
    Result := Show1
  else
    Result := Show1 + Show2;
  Result := Result + inherited;
end;

procedure TJTable.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Columns') or (Attr = 'ColCount') or (Attr = 'RowCount') then
    MakeTable
  else if (Attr = 'HorizontalScrollBarPolicy') or
    (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else
    inherited;
end;

procedure TJTable.MakeTable;
var
  Key, Str: string;
begin
  Key := 'private JTable ' + Name;
  Str := Indent1 + Key + ' = new JTable(' + IntToStr(RowCount) + ', ' +
    IntToStr(ColCount) + ');';
  FPartner.ReplaceAttribute(Key, Str);

  FPartner.DeleteAttributeValues(Name + '.getColumnModel()');
  Str := '';
  for var I := 0 to Min(Columns.Count, ColCount) - 1 do
    Str := Str + SurroundFix(Indent1 + Name + '.getColumnModel().getColumn(' +
      IntToStr(I) + ').setHeaderValue("' + Columns[I] + '");');
  FPartner.InsertAttributValue(GetContainerAdd, Str, 0);
end;

procedure TJTable.NewControl;
var
  Str: string;
begin
  FPartner.InsertImport('javax.swing.table.*');
  InsertNewVariable('private JTable ' + Name + ' = new JTable(5, 5);');
  InsertNewVariable('  private DefaultTableModel ' + Name +
    'Model = (DefaultTableModel) ' + Name + '.getModel();');
  InsertNewVariable('  private JScrollPane ' + Name +
    'ScrollPane = new JScrollPane(' + Name + ');');
  SetAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane'
    + GetBounds);
  Str := '';
  for var I := 0 to 4 do
    Str := Str + SurroundFix('  ' + Name + '.getColumnModel().getColumn(' +
      IntToStr(I) + ').setHeaderValue("Title ' + IntToStr(I + 1) + '");');
  Str := Str + SurroundFix('  ' + GetContainerAdd);
  FPartner.InsertComponent(Str);
  MakeFont;
end;

procedure TJTable.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private DefaultTableModel ' + Name + 'Model');
  FPartner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  FPartner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJTable.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(',
    Name + 'ScrollPane' + GetBounds);
end;

procedure TJTable.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'ScrollPane', NewName + 'ScrollPane', True);
  FPartner.ReplaceWord(OldName + 'Model', NewName + 'Model', True);
end;

function TJTable.GetContainerAdd: string;
begin
  Result := AWTSwingContainer;
  if Result = '' then
    Result := 'add(' + Name + 'ScrollPane);'
  else
    Result := Result + '.add(' + Name + 'ScrollPane);';
end;

procedure TJTable.Paint;
const
  CHeaderHeight = 16;
var
  XPos, YPos, DeltaX, DeltaY, TextWidth, TextHeight, LWidth,
    HeaderHeight: Integer;
  Str: string;
  Vsb, Hsb: Boolean;
begin
  CanvasFontAssign;
  HeaderHeight := PPIScale(CHeaderHeight);
  Canvas.Font.Color := DefaultForeground;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := DefaultBackground;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  Canvas.Brush.Color := Background;
  if FillsViewportHeight then
    Canvas.FillRect(Rect(1, HeaderHeight + 1, Width - 1, Height - 1))
  else
    Canvas.FillRect(Rect(1, HeaderHeight + 1, Width - 1,
      FRowHeight * (FRowCount + 1)));

  Canvas.Brush.Color := DefaultBackground;
  Canvas.Pen.Color := DarkShadow;
  TextHeight := Canvas.TextHeight('Hg');
  Vsb := (VerticalScrollBarPolicy = ALWAYS);
  Hsb := (HorizontalScrollBarPolicy = ALWAYS);
  if (HeaderHeight + RowCount * RowHeight > Height) and
    not(VerticalScrollBarPolicy = NEVER) then
    Vsb := True;
  if Vsb then
    LWidth := Width - PPIScale(19)
  else
    LWidth := Width - PPIScale(3);

  // header row
  XPos := 0;
  DeltaX := LWidth div FColCount;
  DeltaY := HeaderHeight;
  for var I := 1 to FColCount do
  begin
    if I <= FColumns.Count then
      Str := FColumns[I - 1]
    else
      Str := '';
    TextWidth := Canvas.TextWidth(Str);
    Canvas.TextRect(Rect(XPos + 1, 1, XPos + DeltaX - 1, HeaderHeight - 1),
      XPos + (DeltaX - TextWidth) div 2, (HeaderHeight - TextHeight) div 2
      + 1, Str);
    XPos := Round(((LWidth * 1.0) / FColCount) * I);
    Canvas.MoveTo(XPos, 0);
    Canvas.LineTo(XPos, DeltaY);
  end;
  if FShowVerticalLines then
  begin
    DeltaY := FRowHeight * (FRowCount + 1);
    for var I := 1 to FColCount do
    begin
      XPos := Round(((LWidth * 1.0) / FColCount) * I);
      Canvas.MoveTo(XPos, HeaderHeight);
      Canvas.LineTo(XPos, DeltaY);
    end;
  end;

  if FShowHorizontalLines then
  begin
    YPos := HeaderHeight;
    for var I := 1 to FRowCount + 1 do
    begin
      Canvas.MoveTo(0, YPos);
      Canvas.LineTo(LWidth, YPos);
      YPos := YPos + FRowHeight;
    end;
  end;

  // paint scrollbars
  var
  P17 := PPIScale(17);
  if Hsb and Vsb then
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.FillRect(Rect(Width - P17, Height - P17, Width - 1, Height - 1));
    Scrollbar(Rect(Width - P17, 1, Width - 1, Height - P17), False, True);
    // Vsb
    Scrollbar(Rect(1, Height - P17, Width - P17, Height - 1), True, True);
    // Hsb
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - P17);
    Canvas.LineTo(Width - P17, Height - P17);
    Canvas.LineTo(Width - P17, 0);
    Canvas.MoveTo(1, Height - 1);
    Canvas.LineTo(Width - 1, Height - 1);
    Canvas.LineTo(Width - 1, 0);
  end
  else if Hsb then
  begin
    Scrollbar(Rect(1, Height - P17, Width - 1, Height - 1), True, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - P17);
    Canvas.LineTo(Width - 1, Height - P17);
  end
  else if Vsb then
  begin
    Scrollbar(Rect(Width - P17, 1, Width - 1, Height - 1), False, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(Width - P17, Height - P17);
    Canvas.LineTo(Width - P17, 0);
  end;
end;

function TJTable.GetColumns: TStrings;
begin
  Result := FColumns;
end;

procedure TJTable.SetColumns(Strings: TStrings);
begin
  FColumns.Assign(Strings);
  FColCount := Max(FColumns.Count, 1);
  Invalidate;
end;

procedure TJTable.SetRowHeight(AValue: Integer);
begin
  if AValue <> FRowHeight then
  begin
    FRowHeight := AValue;
    Invalidate;
  end;
end;

procedure TJTable.SetShowHorizontalLines(AValue: Boolean);
begin
  if AValue <> FShowHorizontalLines then
  begin
    FShowHorizontalLines := AValue;
    FShowGrid := FShowVerticalLines and FShowHorizontalLines;
    Invalidate;
  end;
end;

procedure TJTable.SetShowVerticalLines(AValue: Boolean);
begin
  if AValue <> FShowVerticalLines then
  begin
    FShowVerticalLines := AValue;
    FShowGrid := FShowVerticalLines and FShowHorizontalLines;
    Invalidate;
  end;
end;

procedure TJTable.SetShowGrid(AValue: Boolean);
begin
  if AValue <> FShowGrid then
  begin
    FShowGrid := AValue;
    FShowHorizontalLines := AValue;
    FShowVerticalLines := AValue;
    Invalidate;
  end;
end;

procedure TJTable.SetColCount(AValue: Integer);
begin
  if AValue <> FColCount then
  begin
    FColCount := Max(AValue, 1);
    Invalidate;
  end;
end;

procedure TJTable.SetRowCount(AValue: Integer);
begin
  if AValue <> FRowCount then
  begin
    FRowCount := AValue;
    Invalidate;
  end;
end;

procedure TJTable.SetFillsViewportHeight(AValue: Boolean);
begin
  if AValue <> FFillsViewportHeight then
  begin
    FFillsViewportHeight := AValue;
    Invalidate;
  end;
end;

procedure TJTable.SetHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FHorizontalScrollBarPolicy then
  begin
    FHorizontalScrollBarPolicy := Value;
    Invalidate;
  end;
end;

procedure TJTable.SetVerticalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FVerticalScrollBarPolicy then
  begin
    FVerticalScrollBarPolicy := Value;
    Invalidate;
  end;
end;

end.
