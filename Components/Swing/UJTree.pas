unit UJTree;

interface

uses
  ComCtrls, Classes, UJEComponents, UJComponents;

type

  TJTree = class (TSwingComponent)
  private
    FRowHeight: integer;
    FRootVisible: boolean;
    FDragEnabled: boolean;
    FEditable: boolean;
    FInvokesStopCellEditing: boolean;
    FExpandsSelectedPaths: boolean;
    FScrollsOnExpand: boolean;
    FSelectionRow: integer;
    FShowsRootHandles: boolean;
    FToggleClickCount: integer;
    FModel: TStrings;
    FtreeCollapsed: string;
    FtreeExpanded: string;
    FtreeValueChanged: string;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;

    procedure setRowHeight(aValue: integer);
    procedure setRootVisible(aValue: boolean);
    procedure setShowsRootHandles(aValue: boolean);
    procedure setItems(aValue: TStrings);
    procedure setHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
    procedure setVerticalScrollBarPolicy(Value: TScrollBarPolicy);
    function NodeName(Nr: integer): string;
    procedure MakeTree;
  public
    constructor Create (AOwner: TComponent); override;
    constructor CreateFrom(aTreeView: TTreeView);
    destructor Destroy; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure SetPositionAndSize; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function GetContainerAdd: string; override;
    procedure Paint; override;
  published
    property RowHeight: integer read FRowHeight write setRowHeight;
    property RootVisible: boolean read FRootVisible write setRootVisible;
    property Model: TStrings read FModel write setItems;
    property ShowsRootHandles: boolean read FShowsRootHandles write setShowsRootHandles;
    property HorizontalScrollBarPolicy: TScrollBarPolicy read FHorizontalScrollBarPolicy write setHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy read FVerticalScrollBarPolicy write setVerticalScrollBarPolicy;
    property DragEnabled: boolean read FDragEnabled write FDragEnabled;
    property Editable: boolean read FEditable write FEditable;
    property InvokesStopCellEditing: boolean read FInvokesStopCellEditing write FInvokesStopCellEditing;
    property ExpandsSelectedPaths: boolean read FExpandsSelectedPaths write FExpandsSelectedPaths;
    property ScrollsOnExpand: boolean read FScrollsOnExpand write FScrollsOnExpand;
    property SelectionRow: integer read FSelectionRow write FSelectionRow;
    property ToggleClickCount: integer read FToggleClickCount write FToggleClickCount;

    property treeCollapsed: string read FtreeCollapsed write FtreeCollapsed;
    property treeExpanded: string read FtreeExpanded write FtreeExpanded;
    property treeValueChanged: string read FtreeValueChanged write FtreeValueChanged;
  end;

implementation

uses SysUtils, Graphics, Controls, Forms, UGUIDesigner, UUtils;

constructor TJTree.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 20;
  Width:= 240;
  Height:= 120;
  Background:= clWhite;
  FRowHeight:= 18;
  FDragEnabled:= false;
  FEditable:= false;
  FRootVisible:= true;
  FModel:= TStringList.Create;
  FModel.Text:= 'root'#13#10'  node 1'#13#10'    leaf 1'#13#10'    leaf 2'#13#10'    leaf 3'#13#10'  node 2'#13#10'    node 3'#13#10'      leaf 4'#13#10'    leaf 5'#13#10'  node 4'#13#10'    leaf 6'#13#10'    leaf 7';
  FToggleClickCount:= 2;
  FHorizontalScrollBarPolicy:= AS_NEEDED;
  FVerticalScrollBarPolicy:= AS_NEEDED;  
  Font.Style:= [];  // not bold;
  JavaType:= 'JTree';
end;

constructor TJTree.CreateFrom(aTreeView: TTreeView);
begin
  Create(aTreeView.Owner);
  CreateFromJ(aTreeView);
  Background:= aTreeView.Color;
  RowHeight:= aTreeView.Indent;
  RootVisible:= aTreeView.ShowRoot;
end;

destructor TJTree.Destroy;
begin
  FreeAndNil(FModel);
  inherited;
end;

function TJTree.getAttributes(ShowAttributes: integer): string;
  const
    tree1 = '|RowHeight|RootVisible|Model|ShowsRootHandles' +
            '|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
    tree2 = '|DragEnabled|Editable|InvokesStopCellEditing|ExpandsSelectedPaths' +
            '|ScrollsOnExpand|SelectionRow|ToggleClickCount|';
begin
  if ShowAttributes = 1
    then Result:= tree1
    else Result:= tree1 + tree2;
  Result:= Result + inherited;
end;

procedure TJTree.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Model' then
    MakeTree
  else if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else
    inherited;
end;

function TJTree.getEvents(ShowEvents: integer): string;
begin
  Result:= '|treeCollapsed|treeExpanded|treeValueChanged' + inherited;
end;

procedure TJTree.NewControl;
  var node1, node2, s: string;
begin
  Partner.InsertImport('javax.swing.tree.*');
  InsertNewVariable('private DefaultMutableTreeNode ' + Name + 'Node0 = new DefaultMutableTreeNode("root");');
  InsertNewVariable('private JTree ' + Name + ' = new JTree('+ Name + 'Node0);');
  InsertNewVariable('  private JScrollPane ' + Name + 'ScrollPane = new JScrollPane(' + Name + ');');
  setAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane' + getBounds);

  s:= '';
  node1:= Name + 'Node1';
  node2:= Name + 'Node2';
  s:= s + surroundFix2('DefaultMutableTreeNode ' + node1 + ' = new DefaultMutableTreeNode("node 1");');
  s:= s + surroundFix2(node1 + '.add(new DefaultMutableTreeNode("leaf 1"));');
  s:= s + surroundFix2(node1 + '.add(new DefaultMutableTreeNode("leaf 2"));');
  s:= s + surroundFix2(node1 + '.add(new DefaultMutableTreeNode("leaf 3"));');
  s:= s + surroundFix2(Name + 'Node0.add(' + node1 + ');');
  s:= s + surroundFix2(node1 + ' = new DefaultMutableTreeNode("node 2");');
  s:= s + surroundFix2('DefaultMutableTreeNode ' + node2 + ' = new DefaultMutableTreeNode("node 3");');
  s:= s + surroundFix2(node2 + '.add(new DefaultMutableTreeNode("leaf 4"));');
  s:= s + surroundFix2(node1 + '.add(' + node2 + ');');
  s:= s + surroundFix2(node1 + '.add(new DefaultMutableTreeNode("leaf 5"));');
  s:= s + surroundFix2(Name + 'Node0.add(' + node1 + ');');
  s:= s + surroundFix2(node1 + ' = new DefaultMutableTreeNode("node 4");');
  s:= s + surroundFix2(node1 + '.add(new DefaultMutableTreeNode("leaf 6"));');
  s:= s + surroundFix2(node1 + '.add(new DefaultMutableTreeNode("leaf 7"));');
  s:= s + surroundFix2(Name + 'Node0.add(' + node1 + ');');
  s:= s + surroundFix2(Name + '.expandRow(0);');
  s:= s + surroundFix2(Name + '.expandRow(1);');
  s:= s + surroundFix2(GetContainerAdd);
  Partner.InsertComponent(s);
  MakeFont;
end;

procedure TJTree.MakeTree;
  var s: string; NodesDepth, NodeNr: integer;

  function getIndent(const s: string): integer;
    var i, l: integer;
  begin
    l:= Length(s);
    i:= 1;
    while (i <= l) and (s[i] <= ' ') do
      inc(i);
    Result:= i-1;
  end;

  procedure makeNode(Depth: integer);
    var IndentNr, IndentNr1: integer; s1, aLabel: string;
  begin
    aLabel:= Model[NodeNr];
    IndentNr:= getIndent(aLabel);
    aLabel:= trim(aLabel);
    while NodeNr < Model.Count do begin
      if NodeNr +1 < Model.Count
        then IndentNr1:= getIndent(Model[NodeNr+1])
        else IndentNr1:= IndentNr;
      if IndentNr1 > IndentNr then begin
        s1:= NodeName(Depth) + ' = new DefaultMutableTreeNode("' + aLabel + '");';
        if NodesDepth < Depth then begin
          s:= s + surroundFix2('DefaultMutableTreeNode ' + s1);
          inc(NodesDepth);
        end else
          s:= s + surroundFix2(s1);
        Inc(NodeNr);
        makeNode(Depth + 1);
        if NodeNr < Model.Count
          then IndentNr1:= getIndent(Model[NodeNr])
          else IndentNr1:= 0;
        if IndentNr1 <= IndentNr then
          s:= s + surroundFix2(NodeName(Depth-1) + '.add(' + NodeName(Depth) + ');');
      end else if IndentNr1 <= IndentNr then begin
        s:= s + surroundFix2(NodeName(Depth-1) + '.add(new DefaultMutableTreeNode("' + aLabel + '"));');
        Inc(NodeNr);
      end;
      if IndentNr1 < IndentNr
        then exit
      else if NodeNr < Model.Count
        then aLabel:= trim(Model[NodeNr]);
    end;
  end;

begin
  s:= Indent1 + 'private DefaultMutableTreeNode ' + Name +
                'Node0 = new DefaultMutableTreeNode("' + Trim(Model[0]) + '");';
  Partner.ReplaceAttribute('private DefaultMutableTreeNode ' + Name, s);
  Partner.DeleteAttributeValues(Name + '.expandRow');
  Partner.DeleteAttributeValues(Name + 'ScrollPane');

  for var i:= 0 to Model.Count - 1 do
    Partner.DeleteAttributeValues(NodeName(i));
  s:= surroundFix2(Name + 'ScrollPane' + getBounds);
  NodesDepth:= 0;
  NodeNr:= 1;
  FormatItems(FModel);
  if Model.Count > 1 then
    makeNode(1);
  s:= s + surroundFix2(Name + '.expandRow(0);');
  s:= s + surroundFix2(Name + '.expandRow(1);');
  s:= s + surroundFix2(GetContainerAdd);
  Partner.InsertComponent(s);
end;

procedure TJTree.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private DefaultMutableTreeNode ' + Name + 'Node0');
  Partner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  Partner.DeleteAttributeValues(Name + 'ScrollPane');
  for var i:= 0 to Model.Count - 1 do
    Partner.DeleteAttributeValues(NodeName(i));
end;

function TJTree.NodeName(Nr: integer): string;
begin
  Result:= Name + 'Node' + IntToStr(Nr);
end;

procedure TJTree.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(', Name + 'ScrollPane' + getBounds);
end;

procedure TJTree.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(FtreeCollapsed);
  rename(FtreeExpanded);
  rename(FtreeValueChanged);
  Partner.ReplaceWord(OldName + 'ScrollPane' , NewName + 'ScrollPane', true);
  for var i:= 0 to Model.Count - 1 do
    Partner.ReplaceWord(OldName + 'Node' + IntToStr(i), NewName + 'Node' + IntToStr(i), true);
end;

function TJTree.GetContainerAdd: string;
begin
  Result:= AWTSwingContainer;
  if Result = ''
    then Result:= 'add(' + name + 'ScrollPane);'
    else Result:= Result + '.add(' + name + 'ScrollPane);';
end;

procedure TJTree.Paint;
  const cdx = 20;
  var s: string;
      i, j, js, th, x, y, IndentS, dx, PicNr, p17: integer;
      vsb, hsb: boolean;

  function isLeaf(Nr: integer): boolean;
    var i1, i2: integer;
  begin
    if Nr + 1 = FModel.Count
       then Result:= true
    else begin
      i1:= getIndent(FModel.Strings[Nr]);
      i2:= getIndent(FModel.Strings[Nr+1]);
      Result:= (i2 <= i1);
    end;
  end;

  function hasBrother(Nr: integer): boolean;
    var i, i1, i2: integer;
  begin
    Result:= false;
    i1:= getIndent(FModel.Strings[Nr]);
    i:= Nr + 1;
    while (i < FModel.Count) and not Result do begin
      i2:= getIndent(FModel.Strings[i]);
      if i2 = i1
        then Result:= true
      else if i2 < i1
        then break;
      inc(i);
    end;
  end;

  function ParentHasBrotherToBitmap(Parent, Nr: integer): integer;
    var i, i1: integer;
  begin
    Result:= -1;
    i:= Nr + 1;
    while (i < FModel.Count) and (Result = -1) do begin
      i1:= getIndent(FModel.Strings[i]);
      if i1 = Parent
        then Result:= 4
      else if i1 < Parent
        then break;
      inc(i);
    end;
  end;

  function LeafToBitmap(Leaf:Boolean): integer;
  begin
    if Leaf then Result:= 1 else Result:= 0;
  end;

  function BrotherLeafToBitmap(brother, leaf: boolean): integer;
  begin
    if brother then
      if Leaf then Result:= 6 else Result:= 2
    else
      if Leaf then Result:= 5 else Result:= 3;
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color:= DefaultForeground; 
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  th:= Canvas.TextHeight('Hg');
  vsb:= (VerticalScrollBarPolicy = ALWAYS);
  hsb:= (HorizontalScrollBarPolicy = ALWAYS);
  dx:= PPIScale(cdx);
  y:= PPIScale(2);
  FRowHeight:= PPIScale(18);

  // Show root
  if FRootVisible and ShowsRootHandles then begin
    FGUIDesigner.vilControls1618.Draw(Canvas, 0, y, 3);
    x:= dx;
  end else
    x:= 0;

  if FRootVisible and (Model.Count > 0) then begin
    FGUIDesigner.vilControls1618.Draw(Canvas, x, y, LeafToBitmap(isLeaf(0)));
    s:= FModel.Strings[0];
    Canvas.TextOut(x + dx,  y + (FRowHeight-th) div 2, s);
    y:= y + FRowHeight;
  end;

  // show nodes
  for i:= 1 to Model.Count - 1 do begin
    s:= FModel.Strings[i];
    indentS:= getIndent(s);
    if y > Height then break;
    if FRootVisible and ShowsRootHandles
      then x:= dx
      else x:= 2;
    if RootVisible
      then js:= 1
      else js:= 2;
    for j:= js to indentS-1 do begin
      PicNr:= ParentHasBrotherToBitmap(j, i);
      if PicNr > -1 then begin
        FGUIDesigner.vilControls1618.Draw(Canvas, x, y, PicNr);
        inc(x, dx);
      end;
    end;
    if (IndentS > 1) or RootVisible then begin
      FGUIDesigner.vilControls1618.Draw(Canvas, x, y, BrotherLeafToBitmap(hasBrother(i), isLeaf(i)));
      inc(x, dx);
    end;
    FGUIDesigner.vilControls1618.Draw(Canvas, x, y, LeafToBitmap(isLeaf(i)));
    Canvas.TextOut(x + dx + 2, y + (FRowHeight-th) div 2, trim(s));
    y:= y + FRowHeight;
    if (x + dx + 2 + Canvas.TextWidth(s) > Width) and
      (HorizontalScrollBarPolicy <> NEVER) then
      hsb:= true;
  end;
  if (y > Height) and (VerticalScrollBarPolicy <> NEVER) then
    vsb:= true;

  // paint scrollbars
  p17:= PPIScale(17);
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

procedure TJTree.SetRowHeight(aValue: integer);
begin
  if aValue <> FRowHeight then begin
    FRowHeight:= aValue;
    Invalidate;
  end;
end;

procedure TJTree.setRootVisible(aValue: boolean);
begin
  if aValue <> FRootVisible then begin
    FRootVisible:= aValue;
    Invalidate;
  end;
end;

procedure TJTree.setShowsRootHandles(aValue: boolean);
begin
  if aValue <> FShowsRootHandles then begin
    FShowsRootHandles:= aValue;
    Invalidate;
  end;
end;

procedure TJTree.setItems(aValue: TStrings);
begin
  if FModel.Text <> aValue.Text then begin
    FModel.Assign(aValue);
    Invalidate;
  end;
end;

procedure TJTree.setHorizontalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FHorizontalScrollBarPolicy then begin
    FHorizontalScrollBarPolicy:= Value;
    Invalidate;
  end;
end;

procedure TJTree.setVerticalScrollBarPolicy(Value: TScrollBarPolicy);
begin
  if Value <> FVerticalScrollBarPolicy then begin
    FVerticalScrollBarPolicy:= Value;
    Invalidate;
  end;
end;

end.
