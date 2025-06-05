unit UJTree;

interface

uses
  ComCtrls,
  Classes,
  UJEComponents,
  UJComponents;

type

  TJTree = class(TSwingComponent)
  private
    FRowHeight: Integer;
    FRootVisible: Boolean;
    FDragEnabled: Boolean;
    FEditable: Boolean;
    FInvokesStopCellEditing: Boolean;
    FExpandsSelectedPaths: Boolean;
    FScrollsOnExpand: Boolean;
    FSelectionRow: Integer;
    FShowsRootHandles: Boolean;
    FToggleClickCount: Integer;
    FModel: TStrings;
    FtreeCollapsed: string;
    FtreeExpanded: string;
    FtreeValueChanged: string;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;

    procedure SetRowHeight(AValue: Integer);
    procedure SetRootVisible(AValue: Boolean);
    procedure SetShowsRootHandles(AValue: Boolean);
    procedure SetItems(AValue: TStrings);
    procedure SetHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure SetVerticalScrollBarPolicy(Value: TScrollBarPolicy);
    function NodeName(Num: Integer): string;
    procedure MakeTree;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(ATreeView: TTreeView);
    destructor Destroy; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure SetPositionAndSize; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function GetContainerAdd: string; override;
    procedure Paint; override;
  published
    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property RootVisible: Boolean read FRootVisible write SetRootVisible;
    property Model: TStrings read FModel write SetItems;
    property ShowsRootHandles: Boolean read FShowsRootHandles
      write SetShowsRootHandles;
    property HorizontalScrollBarPolicy: TScrollBarPolicy
      read FHorizontalScrollBarPolicy write SetHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy
      read FVerticalScrollBarPolicy write SetVerticalScrollBarPolicy;
    property DragEnabled: Boolean read FDragEnabled write FDragEnabled;
    property Editable: Boolean read FEditable write FEditable;
    property InvokesStopCellEditing: Boolean read FInvokesStopCellEditing
      write FInvokesStopCellEditing;
    property ExpandsSelectedPaths: Boolean read FExpandsSelectedPaths
      write FExpandsSelectedPaths;
    property ScrollsOnExpand: Boolean read FScrollsOnExpand
      write FScrollsOnExpand;
    property SelectionRow: Integer read FSelectionRow write FSelectionRow;
    property ToggleClickCount: Integer read FToggleClickCount
      write FToggleClickCount;

    property treeCollapsed: string read FtreeCollapsed write FtreeCollapsed;
    property treeExpanded: string read FtreeExpanded write FtreeExpanded;
    property treeValueChanged: string read FtreeValueChanged
      write FtreeValueChanged;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Controls,
  UGUIDesigner,
  UUtils;

constructor TJTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 20;
  Width := 240;
  Height := 120;
  Background := clWhite;
  FRowHeight := 18;
  FDragEnabled := False;
  FEditable := False;
  FRootVisible := True;
  FModel := TStringList.Create;
  FModel.Text :=
    'root'#13#10'  node 1'#13#10'    leaf 1'#13#10'    leaf 2'#13#10'    leaf 3'#13#10'  node 2'#13#10'    node 3'#13#10'      leaf 4'#13#10'    leaf 5'#13#10'  node 4'#13#10'    leaf 6'#13#10'    leaf 7';
  FToggleClickCount := 2;
  FHorizontalScrollBarPolicy := AS_NEEDED;
  FVerticalScrollBarPolicy := AS_NEEDED;
  Font.Style := []; // not bold;
  JavaType := 'JTree';
end;

constructor TJTree.CreateFrom(ATreeView: TTreeView);
begin
  Create(ATreeView.Owner);
  CreateFromJ(ATreeView);
  Background := ATreeView.Color;
  RowHeight := ATreeView.Indent;
  RootVisible := ATreeView.ShowRoot;
end;

destructor TJTree.Destroy;
begin
  FreeAndNil(FModel);
  inherited;
end;

function TJTree.GetAttributes(ShowAttributes: Integer): string;
const
  Tree1 = '|RowHeight|RootVisible|Model|ShowsRootHandles' +
    '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
  Tree2 = '|DragEnabled|Editable|InvokesStopCellEditing|ExpandsSelectedPaths' +
    '|ScrollsOnExpand|SelectionRow|ToggleClickCount|';
begin
  if ShowAttributes = 1 then
    Result := Tree1
  else
    Result := Tree1 + Tree2;
  Result := Result + inherited;
end;

procedure TJTree.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Model' then
    MakeTree
  else if (Attr = 'HorizontalScrollBarPolicy') or
    (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else
    inherited;
end;

function TJTree.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|treeCollapsed|treeExpanded|treeValueChanged' + inherited;
end;

procedure TJTree.NewControl;
var
  Node1, Node2, Str: string;
begin
  FPartner.InsertImport('javax.swing.tree.*');
  InsertNewVariable('private DefaultMutableTreeNode ' + Name +
    'Node0 = new DefaultMutableTreeNode("root");');
  InsertNewVariable('private JTree ' + Name + ' = new JTree(' + Name +
    'Node0);');
  InsertNewVariable('  private JScrollPane ' + Name +
    'ScrollPane = new JScrollPane(' + Name + ');');
  SetAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane'
    + GetBounds);

  Str := '';
  Node1 := Name + 'Node1';
  Node2 := Name + 'Node2';
  Str := Str + SurroundFix2('DefaultMutableTreeNode ' + Node1 +
    ' = new DefaultMutableTreeNode("node 1");');
  Str := Str + SurroundFix2
    (Node1 + '.add(new DefaultMutableTreeNode("leaf 1"));');
  Str := Str + SurroundFix2
    (Node1 + '.add(new DefaultMutableTreeNode("leaf 2"));');
  Str := Str + SurroundFix2
    (Node1 + '.add(new DefaultMutableTreeNode("leaf 3"));');
  Str := Str + SurroundFix2(Name + 'Node0.add(' + Node1 + ');');
  Str := Str + SurroundFix2(Node1 + ' = new DefaultMutableTreeNode("node 2");');
  Str := Str + SurroundFix2('DefaultMutableTreeNode ' + Node2 +
    ' = new DefaultMutableTreeNode("node 3");');
  Str := Str + SurroundFix2
    (Node2 + '.add(new DefaultMutableTreeNode("leaf 4"));');
  Str := Str + SurroundFix2(Node1 + '.add(' + Node2 + ');');
  Str := Str + SurroundFix2
    (Node1 + '.add(new DefaultMutableTreeNode("leaf 5"));');
  Str := Str + SurroundFix2(Name + 'Node0.add(' + Node1 + ');');
  Str := Str + SurroundFix2(Node1 + ' = new DefaultMutableTreeNode("node 4");');
  Str := Str + SurroundFix2
    (Node1 + '.add(new DefaultMutableTreeNode("leaf 6"));');
  Str := Str + SurroundFix2
    (Node1 + '.add(new DefaultMutableTreeNode("leaf 7"));');
  Str := Str + SurroundFix2(Name + 'Node0.add(' + Node1 + ');');
  Str := Str + SurroundFix2(Name + '.expandRow(0);');
  Str := Str + SurroundFix2(Name + '.expandRow(1);');
  Str := Str + SurroundFix2(GetContainerAdd);
  FPartner.InsertComponent(Str);
  MakeFont;
end;

procedure TJTree.MakeTree;
var
  Str: string;
  NodesDepth, NodeNr: Integer;

  function GetIndent(const Str: string): Integer;
  var
    Int, Len: Integer;
  begin
    Len := Length(Str);
    Int := 1;
    while (Int <= Len) and (Str[Int] <= ' ') do
      Inc(Int);
    Result := Int - 1;
  end;

  procedure MakeNode(Depth: Integer);
  var
    IndentNr, IndentNr1: Integer;
    Str1, ALabel: string;
  begin
    ALabel := Model[NodeNr];
    IndentNr := GetIndent(ALabel);
    ALabel := Trim(ALabel);
    while NodeNr < Model.Count do
    begin
      if NodeNr + 1 < Model.Count then
        IndentNr1 := GetIndent(Model[NodeNr + 1])
      else
        IndentNr1 := IndentNr;
      if IndentNr1 > IndentNr then
      begin
        Str1 := NodeName(Depth) + ' = new DefaultMutableTreeNode("' +
          ALabel + '");';
        if NodesDepth < Depth then
        begin
          Str := Str + SurroundFix2('DefaultMutableTreeNode ' + Str1);
          Inc(NodesDepth);
        end
        else
          Str := Str + SurroundFix2(Str1);
        Inc(NodeNr);
        MakeNode(Depth + 1);
        if NodeNr < Model.Count then
          IndentNr1 := GetIndent(Model[NodeNr])
        else
          IndentNr1 := 0;
        if IndentNr1 <= IndentNr then
          Str := Str + SurroundFix2(NodeName(Depth - 1) + '.add(' +
            NodeName(Depth) + ');');
      end
      else if IndentNr1 <= IndentNr then
      begin
        Str := Str + SurroundFix2(NodeName(Depth - 1) +
          '.add(new DefaultMutableTreeNode("' + ALabel + '"));');
        Inc(NodeNr);
      end;
      if IndentNr1 < IndentNr then
        Exit
      else if NodeNr < Model.Count then
        ALabel := Trim(Model[NodeNr]);
    end;
  end;

begin
  Str := Indent1 + 'private DefaultMutableTreeNode ' + Name +
    'Node0 = new DefaultMutableTreeNode("' + Trim(Model[0]) + '");';
  FPartner.ReplaceAttribute('private DefaultMutableTreeNode ' + Name, Str);
  FPartner.DeleteAttributeValues(Name + '.expandRow');
  FPartner.DeleteAttributeValues(Name + 'ScrollPane');

  for var I := 0 to Model.Count - 1 do
    FPartner.DeleteAttributeValues(NodeName(I));
  Str := SurroundFix2(Name + 'ScrollPane' + GetBounds);
  NodesDepth := 0;
  NodeNr := 1;
  FormatItems(FModel);
  if Model.Count > 1 then
    MakeNode(1);
  Str := Str + SurroundFix2(Name + '.expandRow(0);');
  Str := Str + SurroundFix2(Name + '.expandRow(1);');
  Str := Str + SurroundFix2(GetContainerAdd);
  FPartner.InsertComponent(Str);
end;

procedure TJTree.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private DefaultMutableTreeNode ' + Name + 'Node0');
  FPartner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  FPartner.DeleteAttributeValues(Name + 'ScrollPane');
  for var I := 0 to Model.Count - 1 do
    FPartner.DeleteAttributeValues(NodeName(I));
end;

function TJTree.NodeName(Num: Integer): string;
begin
  Result := Name + 'Node' + IntToStr(Num);
end;

procedure TJTree.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(',
    Name + 'ScrollPane' + GetBounds);
end;

procedure TJTree.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FtreeCollapsed);
  Rename(FtreeExpanded);
  Rename(FtreeValueChanged);
  FPartner.ReplaceWord(OldName + 'ScrollPane', NewName + 'ScrollPane', True);
  for var I := 0 to Model.Count - 1 do
    FPartner.ReplaceWord(OldName + 'Node' + IntToStr(I),
      NewName + 'Node' + IntToStr(I), True);
end;

function TJTree.GetContainerAdd: string;
begin
  Result := AWTSwingContainer;
  if Result = '' then
    Result := 'add(' + Name + 'ScrollPane);'
  else
    Result := Result + '.add(' + Name + 'ScrollPane);';
end;

procedure TJTree.Paint;
const
  Cdx = 20;
var
  Str: string;
  JSPos, TextHeight, XPos, YPos, IndentS, DeltaX, PicNr, P17: Integer;
  Vsb, Hsb: Boolean;

  function isLeaf(Num: Integer): Boolean;
  var
    Int1, Int2: Integer;
  begin
    if Num + 1 = FModel.Count then
      Result := True
    else
    begin
      Int1 := GetIndent(FModel[Num]);
      Int2 := GetIndent(FModel[Num + 1]);
      Result := (Int2 <= Int1);
    end;
  end;

  function hasBrother(Num: Integer): Boolean;
  var
    Int, Int1, Int2: Integer;
  begin
    Result := False;
    Int1 := GetIndent(FModel[Num]);
    Int := Num + 1;
    while (Int < FModel.Count) and not Result do
    begin
      Int2 := GetIndent(FModel[Int]);
      if Int2 = Int1 then
        Result := True
      else if Int2 < Int1 then
        Break;
      Inc(Int);
    end;
  end;

  function ParentHasBrotherToBitmap(Parent, Num: Integer): Integer;
  var
    Int, Int1: Integer;
  begin
    Result := -1;
    Int := Num + 1;
    while (Int < FModel.Count) and (Result = -1) do
    begin
      Int1 := GetIndent(FModel[Int]);
      if Int1 = Parent then
        Result := 4
      else if Int1 < Parent then
        Break;
      Inc(Int);
    end;
  end;

  function LeafToBitmap(Leaf: Boolean): Integer;
  begin
    if Leaf then
      Result := 1
    else
      Result := 0;
  end;

  function BrotherLeafToBitmap(Brother, Leaf: Boolean): Integer;
  begin
    if Brother then
      if Leaf then
        Result := 6
      else
        Result := 2
    else if Leaf then
      Result := 5
    else
      Result := 3;
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color := DefaultForeground;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  TextHeight := Canvas.TextHeight('Hg');
  Vsb := (VerticalScrollBarPolicy = ALWAYS);
  Hsb := (HorizontalScrollBarPolicy = ALWAYS);
  DeltaX := PPIScale(Cdx);
  YPos := PPIScale(2);
  FRowHeight := PPIScale(18);

  // Show root
  if FRootVisible and ShowsRootHandles then
  begin
    FGUIDesigner.vilControls1618.Draw(Canvas, 0, YPos, 3);
    XPos := DeltaX;
  end
  else
    XPos := 0;

  if FRootVisible and (Model.Count > 0) then
  begin
    FGUIDesigner.vilControls1618.Draw(Canvas, XPos, YPos,
      LeafToBitmap(isLeaf(0)));
    Str := FModel[0];
    Canvas.TextOut(XPos + DeltaX, YPos + (FRowHeight - TextHeight) div 2, Str);
    YPos := YPos + FRowHeight;
  end;

  // show nodes
  for var I := 1 to Model.Count - 1 do
  begin
    Str := FModel[I];
    IndentS := GetIndent(Str);
    if YPos > Height then
      Break;
    if FRootVisible and ShowsRootHandles then
      XPos := DeltaX
    else
      XPos := 2;
    if RootVisible then
      JSPos := 1
    else
      JSPos := 2;
    for var J := JSPos to IndentS - 1 do
    begin
      PicNr := ParentHasBrotherToBitmap(J, I);
      if PicNr > -1 then
      begin
        FGUIDesigner.vilControls1618.Draw(Canvas, XPos, YPos, PicNr);
        Inc(XPos, DeltaX);
      end;
    end;
    if (IndentS > 1) or RootVisible then
    begin
      FGUIDesigner.vilControls1618.Draw(Canvas, XPos, YPos,
        BrotherLeafToBitmap(hasBrother(I), isLeaf(I)));
      Inc(XPos, DeltaX);
    end;
    FGUIDesigner.vilControls1618.Draw(Canvas, XPos, YPos,
      LeafToBitmap(isLeaf(I)));
    Canvas.TextOut(XPos + DeltaX + 2, YPos + (FRowHeight - TextHeight) div 2,
      Trim(Str));
    YPos := YPos + FRowHeight;
    if (XPos + DeltaX + 2 + Canvas.TextWidth(Str) > Width) and
      (HorizontalScrollBarPolicy <> NEVER) then
      Hsb := True;
  end;
  if (YPos > Height) and (VerticalScrollBarPolicy <> NEVER) then
    Vsb := True;

  // paint scrollbars
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

procedure TJTree.SetRowHeight(AValue: Integer);
begin
  if AValue <> FRowHeight then
  begin
    FRowHeight := AValue;
    Invalidate;
  end;
end;

procedure TJTree.SetRootVisible(AValue: Boolean);
begin
  if AValue <> FRootVisible then
  begin
    FRootVisible := AValue;
    Invalidate;
  end;
end;

procedure TJTree.SetShowsRootHandles(AValue: Boolean);
begin
  if AValue <> FShowsRootHandles then
  begin
    FShowsRootHandles := AValue;
    Invalidate;
  end;
end;

procedure TJTree.SetItems(AValue: TStrings);
begin
  if FModel.Text <> AValue.Text then
  begin
    FModel.Assign(AValue);
    Invalidate;
  end;
end;

procedure TJTree.SetHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FHorizontalScrollBarPolicy then
  begin
    FHorizontalScrollBarPolicy := Value;
    Invalidate;
  end;
end;

procedure TJTree.SetVerticalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FVerticalScrollBarPolicy then
  begin
    FVerticalScrollBarPolicy := Value;
    Invalidate;
  end;
end;

end.
