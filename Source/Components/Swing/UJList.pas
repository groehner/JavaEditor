unit UJList;

interface

uses
  Classes,
  Graphics,
  StdCtrls,
  UJEComponents,
  UJComponents;

type

  TJListLayoutOrientation = (VERTICAL, HORIZONTAL_WRAP, VERTICAL_WRAP);
  TJListSelectionMode = (SINGLE_INTERVAL_SELECTION, MULTIPLE_INTERVAL_SELECTION,
    SINGLE_SELECTION);

  TJList = class(TSwingComponent)
  private
    FItems: TStrings;
    FSelectedIndex: Integer;
    FSelectionMode: TJListSelectionMode;
    FLayoutOrientation: TJListLayoutOrientation;
    FSelectionBackground: TColor;
    FSelectionForeground: TColor;
    FVisibleRowCount: Integer;
    FFixedCellHeight: Integer;
    FFixedCellWidth: Integer;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    procedure SetSelectedIndex(AIndex: Integer);
    procedure SetItems(AItems: TStrings);
    procedure SetFixedCellHeight(AValue: Integer);
    procedure SetLayoutOrientation(AValue: TJListLayoutOrientation);
    procedure SetSelectionBackground(AValue: TColor);
    procedure SetSelectionForeground(AValue: TColor);
    procedure SetVisibleRowCount(AValue: Integer);
    procedure SetHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure SetVerticalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AListBox: TListBox);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetContainerAdd: string; override;
    procedure SetPositionAndSize; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property SelectionMode: TJListSelectionMode read FSelectionMode
      write FSelectionMode;
    property FixedCellHeight: Integer read FFixedCellHeight
      write SetFixedCellHeight;
    property FixedCellWidth: Integer read FFixedCellWidth write FFixedCellWidth;
    property LayoutOrientation: TJListLayoutOrientation read FLayoutOrientation
      write SetLayoutOrientation;
    property SelectionBackground: TColor read FSelectionBackground
      write SetSelectionBackground;
    property SelectionForeground: TColor read FSelectionForeground
      write SetSelectionForeground;
    property VisibleRowCount: Integer read FVisibleRowCount
      write SetVisibleRowCount;
    property HorizontalScrollBarPolicy: TScrollBarPolicy
      read FHorizontalScrollBarPolicy write SetHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy
      read FVerticalScrollBarPolicy write SetVerticalScrollBarPolicy;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Items: TStrings read FItems write SetItems;
  end;

implementation

uses
  Math,
  Controls,
  SysUtils,
  Types;

{ --- TJList ------------------------------------------------------------------- }

constructor TJList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +8;
  Width := 120;
  Height := 80;
  Background := clWhite;
  FItems := TStringList.Create;
  FItems.Text := DefaultItems;
  FSelectedIndex := -1;
  FLayoutOrientation := VERTICAL;
  FSelectionMode := MULTIPLE_INTERVAL_SELECTION;
  FSelectionBackground := SelectionColor;
  FSelectionForeground := DefaultForeground;
  FHorizontalScrollBarPolicy := AS_NEEDED;
  FVerticalScrollBarPolicy := AS_NEEDED;
  FVisibleRowCount := 8;
  FFixedCellHeight := -1;
  FFixedCellWidth := -1;
  JavaType := 'JList';
end;

constructor TJList.CreateFrom(AListBox: TListBox);
begin
  Create(AListBox.Owner);
  CreateFromJ(AListBox);
  Font := AListBox.Font;
  Background := AListBox.Color;
  Items.AddStrings(AListBox.Items);
  FSelectedIndex := -1;
  SelectedIndex := AListBox.Columns;
end;

function TJList.GetAttributes(ShowAttributes: Integer): string;
const
  List1 = '|SelectedIndex|Items';
  List2 = '|SelectionMode|LayoutOrientation|VisibleRowCount' +
    '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
  List3 = '|FixedCellHeight|FixedCellWidth|SelectionBackground|SelectionForeground';
begin
  case ShowAttributes of
    1:
      Result := List1;
    2:
      Result := List1 + List2;
  else
    Result := List1 + List2 + List3;
  end;
  Result := Result + inherited;
end;

procedure TJList.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy')
  then
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

function TJList.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|valueChanged' + inherited GetEvents(ShowEvents);
end;

procedure TJList.NewControl;
begin
  InsertNewVariable('private JList<String> ' + Name + ' = new JList<>();');
  InsertNewVariable('  private DefaultListModel<String> ' + Name +
    'Model = new DefaultListModel<>();');
  InsertNewVariable('  private JScrollPane ' + Name +
    'ScrollPane = new JScrollPane(' + Name + ');');
  MakeAttribut('Model', Name + 'Model');
  SetAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane'
    + GetBounds);
  FPartner.InsertComponent(Indent2 + GetContainerAdd);
  MakeList;
  MakeFont;
end;

procedure TJList.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private JList<String> ' + Name);
  FPartner.DeleteAttribute('private DefaultListModel<String> ' + Name +
    'Model');
  FPartner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  FPartner.DeleteAttributeValues(Name + 'Model.addElement');
  FPartner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJList.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(',
    Name + 'ScrollPane' + GetBounds);
end;

procedure TJList.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'ScrollPane', NewName + 'ScrollPane', True);
  FPartner.ReplaceWord(OldName + 'Model', NewName + 'Model', True);
end;

function TJList.GetContainerAdd: string;
begin
  Result := AWTSwingContainer;
  if Result = '' then
    Result := 'add(' + Name + 'ScrollPane);'
  else
    Result := Result + '.add(' + Name + 'ScrollPane);';
end;

procedure TJList.MakeList;
var
  Str: string;
begin
  FPartner.DeleteAttributeValues(Name + 'Model.addElement(');
  Str := '';
  for var I := 0 to Items.Count - 1 do
    Str := Str + SurroundFix(Indent1 + Name + 'Model.addElement(' +
      AsString(Items[I]) + ');');
  FPartner.InsertAttributValue(GetContainerAdd, Str, 0);
end;

destructor TJList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJList.Paint;
var
  XPos, YPos, DeltaX, DeltaY, TextHeight, TextWidth, Wrap, LWidth, LHeight,
    Rows, Columns: Integer;
  Str: string;
  Rect1: TRect;
  Vsb, Hsb: Boolean;

  procedure CalcWrap;
  begin
    case LayoutOrientation of
      VERTICAL:
        begin
          Wrap := FItems.Count;
          Rows := Wrap;
          Columns := 1;
        end;
      VERTICAL_WRAP:
        begin
          if VisibleRowCount <= 0 then
            Wrap := LHeight div DeltaY
          else
            Wrap := VisibleRowCount;
          Rows := Wrap;
          Columns := FItems.Count div Wrap;
          if (Wrap <> 0) and (FItems.Count mod Wrap > 0) then
            Inc(Columns);
        end;
      HORIZONTAL_WRAP:
        begin
          if VisibleRowCount <= 0 then
            Wrap := LWidth div DeltaX
          else
            Wrap := FItems.Count div VisibleRowCount + 1;
          Columns := Wrap;
          Rows := FItems.Count div Wrap;
          if (Wrap <> 0) and (FItems.Count mod Wrap > 0) then
            Inc(Rows);
        end;
    end;
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));

  TextHeight := Canvas.TextHeight('Hg');
  Vsb := (VerticalScrollBarPolicy = ALWAYS);
  Hsb := (HorizontalScrollBarPolicy = ALWAYS);
  if Hsb then
    LWidth := Width - 19
  else
    LWidth := Width - 3;
  if Vsb then
    LHeight := Height - 19
  else
    LHeight := Height - 3;
  if FFixedCellHeight = -1 then
    DeltaY := TextHeight + 4
  else
    DeltaY := FFixedCellHeight;

  if FItems.Count > 0 then
    TextWidth := Canvas.TextWidth(FItems[0])
  else
    TextWidth := 0;
  for var I := 1 to FItems.Count - 1 do
  begin
    DeltaX := Canvas.TextWidth(FItems[I]);
    if DeltaX > TextWidth then
      TextWidth := DeltaX;
  end;
  if FFixedCellWidth = -1 then
    DeltaX := TextWidth + 3
  else
    DeltaX := FFixedCellWidth;

  CalcWrap;
  if not Vsb and (VerticalScrollBarPolicy <> NEVER) and (Rows * DeltaY > LHeight)
  then
  begin
    Dec(LWidth, 16);
    Vsb := True;
  end;
  if not Hsb and (HorizontalScrollBarPolicy <> NEVER) and
    (Columns * DeltaX > LWidth) then
  begin
    Dec(LHeight, 16);
    Hsb := True;
  end;
  CalcWrap;

  XPos := -DeltaX;
  YPos := -DeltaY;
  for var I := 0 to FItems.Count - 1 do
  begin
    case LayoutOrientation of
      VERTICAL:
        begin
          XPos := 0;
          Inc(YPos, DeltaY);
        end;
      VERTICAL_WRAP:
        begin
          if (Wrap <> 0) and (I mod Wrap = 0) then
          begin
            YPos := 0;
            Inc(XPos, DeltaX);
          end
          else
            Inc(YPos, DeltaY);
        end;
      HORIZONTAL_WRAP:
        begin
          if (Wrap <> 0) and (I mod Wrap = 0) then
          begin
            XPos := 0;
            Inc(YPos, DeltaY);
          end
          else
            Inc(XPos, DeltaX);
        end;
    end;
    if (XPos < LWidth) and (YPos < LHeight) then
    begin
      Rect1 := Rect(XPos + 2, YPos + 2, Min(XPos + DeltaX - 2, LWidth),
        Min(YPos + DeltaY - 2, LHeight));
      Str := FItems[I];
      if I = FSelectedIndex then
      begin
        Canvas.Brush.Color := SelectionColor;
        Canvas.Pen.Color := SelectionColor;
        Canvas.Font.Color := DefaultForeground;
        Canvas.Rectangle(Rect1);
      end;
      Canvas.TextRect(Rect1, XPos + 1, YPos + (DeltaY - TextHeight) div 2, Str);
      if I = FSelectedIndex then
      begin
        Canvas.Brush.Color := Background;
        Canvas.Pen.Color := Background;
        Canvas.Font.Color := Foreground;
      end;
    end;
  end;

  // paint scrollbars
  if Hsb and Vsb then
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.FillRect(Rect(Width - 17, Height - 17, Width - 2, Height - 2));
    Scrollbar(Rect(Width - 17, 1, Width - 2, Height - 17), False, True); // Vsb
    Scrollbar(Rect(1, Height - 17, Width - 17, Height - 2), True, True); // Hsb
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - 17);
    Canvas.LineTo(Width - 17, Height - 17);
    Canvas.LineTo(Width - 17, 0);
  end
  else if Hsb then
  begin
    Scrollbar(Rect(1, Height - 17, Width - 2, Height - 2), True, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - 17);
    Canvas.LineTo(Width - 1, Height - 17);
  end
  else if Vsb then
  begin
    Scrollbar(Rect(Width - 17, 1, Width - 2, Height - 2), False, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(Width - 17, Height - 17);
    Canvas.LineTo(Width - 17, 0);
  end;
end;

procedure TJList.SetFixedCellHeight(AValue: Integer);
begin
  if AValue <> FFixedCellHeight then
  begin
    FFixedCellHeight := AValue;
    Invalidate;
  end;
end;

procedure TJList.SetLayoutOrientation(AValue: TJListLayoutOrientation);
begin
  if AValue <> FLayoutOrientation then
  begin
    FLayoutOrientation := AValue;
    Invalidate;
  end;
end;

procedure TJList.SetSelectionBackground(AValue: TColor);
begin
  if AValue <> FSelectionBackground then
  begin
    FSelectionBackground := AValue;
    Invalidate;
  end;
end;

procedure TJList.SetSelectionForeground(AValue: TColor);
begin
  if AValue <> FSelectionForeground then
  begin
    FSelectionForeground := AValue;
    Invalidate;
  end;
end;

procedure TJList.SetVisibleRowCount(AValue: Integer);
begin
  if (AValue <> FVisibleRowCount) and (AValue >= -1) then
  begin
    FVisibleRowCount := AValue;
    Invalidate;
  end;
end;

procedure TJList.SetItems(AItems: TStrings);
begin
  if AItems.Text <> FItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TJList.SetSelectedIndex(AIndex: Integer);
begin
  if (csLoading in ComponentState) then
    FSelectedIndex := AIndex
  else if (AIndex <> FSelectedIndex) and (AIndex < FItems.Count) then
  begin
    FSelectedIndex := AIndex;
    Invalidate;
  end;
end;

procedure TJList.SetHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FHorizontalScrollBarPolicy then
  begin
    FHorizontalScrollBarPolicy := Value;
    Invalidate;
  end;
end;

procedure TJList.SetVerticalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FVerticalScrollBarPolicy then
  begin
    FVerticalScrollBarPolicy := Value;
    Invalidate;
  end;
end;

end.
