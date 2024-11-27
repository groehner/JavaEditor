unit UFXTreeView;

interface

uses
  Classes, ComCtrls, UFXComponents;

type

  TFXTreeView = class (TFXControl)
  private
    FShowRoot: boolean;
    FEditable: boolean;
    FItems: TStrings;
    FFixedCellSize: double;

    FEditCancel: string;
    FEditCommit: string;
    FEditStart: string;
    //FScrollTo: string;

    procedure setShowRoot(aValue: boolean);
    procedure setItems(aItems: TStrings);
    procedure setFixedCellSize(aValue: double);
    function NodeName(Nr: integer): string;
    procedure MakeTree;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure Paint; override;
  published
    property ShowRoot: boolean read FShowRoot write setShowRoot;
    property Items: TStrings read FItems write setItems;
    property Editable: boolean read FEditable write FEditable;
    property FixedCellSize: double read FFixedCellSize write setFixedCellSize;

    property editCancel: string read FeditCancel write FeditCancel;
    property editCommit: string read FeditCommit write FeditCommit;
    property editStart: string read FeditStart write FeditStart;
    //property scrollTo: string read FscrollTo write FscrollTo;
  end;

implementation

uses SysUtils, Graphics, Controls, UJEComponents, UGUIDesigner, UUtils;

constructor TFXTreeView.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 152;
  Width:= 240;
  Height:= 120;
  Background:= clWhite;
  FEditable:= false;
  FShowRoot:= true;
  FItems:= TStringList.Create;
  FItems.Text:= 'root'#13#10'  node 1'#13#10'    leaf 1'#13#10'    leaf 2'#13#10'    leaf 3'#13#10'  node 2'#13#10'    node 3'#13#10'      leaf 4'#13#10'    leaf 5'#13#10'  node 4'#13#10'    leaf 6'#13#10'    leaf 7';
  Font.Style:= [];  // not bold;
  JavaType:= 'TreeView';
end;

destructor TFXTreeView.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TFXTreeView.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|ShowRoot|Items|Editable|FixedCellSize' + inherited;
end;

procedure TFXTreeView.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeTree
  else
    inherited;
end;

function TFXTreeView.getEvents(ShowEvents: integer): string;
begin
  Result:= '|editCancel|editCommit|editStart' + inherited;
end;

procedure TFXTreeView.NewControl;
  var node1, node2, s: string;
begin
  InsertImport('javafx.scene.control.*');
  InsertNewVariable('private TreeItem<String> ' + Name + 'Node0 = new TreeItem<>("root");');
  InsertNewVariable('private TreeView<String> ' + Name + ' = new TreeView<>('+ Name + 'Node0);');

  s:= '';
  node1:= Name + 'Node1';
  node2:= Name + 'Node2';
  s:= s + surroundFix2('TreeItem<String> ' + node1 + ' = new TreeItem<>("node 1");');
  s:= s + surroundFix2(node1 + '.getChildren().add(new TreeItem<String>("leaf 1"));');
  s:= s + surroundFix2(node1 + '.getChildren().add(new TreeItem<String>("leaf 2"));');
  s:= s + surroundFix2(node1 + '.getChildren().add(new TreeItem<String>("leaf 3"));');
  s:= s + surroundFix2(Name + 'Node0.getChildren().add(' + node1 + ');');
  s:= s + surroundFix2(node1 + ' = new TreeItem<String>("node 2");');
  s:= s + surroundFix2('TreeItem<String> ' + node2 + ' = new TreeItem<String>("node 3");');
  s:= s + surroundFix2(node2 + '.getChildren().add(new TreeItem<String>("leaf 4"));');
  s:= s + surroundFix2(node1 + '.getChildren().add(' + node2 + ');');
  s:= s + surroundFix2(node1 + '.getChildren().add(new TreeItem<String>("leaf 5"));');
  s:= s + surroundFix2(Name + 'Node0.getChildren().add(' + node1 + ');');
  s:= s + surroundFix2(node1 + ' = new TreeItem<String>("node 4");');
  s:= s + surroundFix2(node1 + '.getChildren().add(new TreeItem<String>("leaf 6"));');
  s:= s + surroundFix2(node1 + '.getChildren().add(new TreeItem<String>("leaf 7"));');
  s:= s + surroundFix2(Name + 'Node0.getChildren().add(' + node1 + ');');
  s:= s + surroundFix2(Name + 'Node0.setExpanded(true);');
  s:= s + surroundFix2(GetContainerAdd);
  Partner.InsertComponent(s);
end;

procedure TFXTreeView.MakeTree;
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
    aLabel:= FItems[NodeNr];
    IndentNr:= getIndent(aLabel);
    aLabel:= trim(aLabel);
    while NodeNr < FItems.Count do begin
      if NodeNr +1 < FItems.Count
        then IndentNr1:= getIndent(FItems[NodeNr+1])
        else IndentNr1:= IndentNr;
      if IndentNr1 > IndentNr then begin
        s1:= NodeName(Depth) + ' = new TreeItem<String>("' + aLabel + '");';
        if NodesDepth < Depth then begin
          s:= s + surroundFix2('TreeItem<String> ' + s1);
          inc(NodesDepth);
        end else
          s:= s + surroundFix2(s1);
        Inc(NodeNr);
        makeNode(Depth + 1);
        if NodeNr < FItems.Count
          then IndentNr1:= getIndent(FItems[NodeNr])
          else IndentNr1:= 0;
        if IndentNr1 <= IndentNr then
          s:= s + surroundFix2(NodeName(Depth-1) + '.getChildren().add(' + NodeName(Depth) + ');');
      end else if IndentNr1 <= IndentNr then begin
        s:= s + surroundFix2(NodeName(Depth-1) + '.getChildren().add(new TreeItem<String>("' + aLabel + '"));');
        Inc(NodeNr);
      end;
      if IndentNr1 < IndentNr then
        exit
      else if NodeNr < FItems.Count then
        aLabel:= trim(FItems[NodeNr]);
    end;
  end;

begin
  s:= Indent1 + 'private TreeItem<String> ' + Name +
                'Node0 = new TreeItem<String>("' + Trim(FItems[0]) + '");';
  Partner.ReplaceAttribute('private TreeItem<String> ' + Name, s);
  Partner.DeleteAttributeValues(Name + '.setExpanded');

  for var i:= 0 to FItems.Count - 1 do
    Partner.DeleteAttributeValues(NodeName(i));
  NodesDepth:= 0;
  NodeNr:= 1;
  FormatItems(FItems);
  s:= '';
  if FItems.Count > 1 then
    makeNode(1);
  s:= s + surroundFix2(Name + 'Node0.setExpanded(true);');
  Partner.InsertComponent(s);
end;

procedure TFXTreeView.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private TreeView<String> ' + Name);
  Partner.DeleteAttribute('private TreeItem<String> ' + Name + 'Node0');
  for var i:= 0 to FItems.Count - 1 do
    Partner.DeleteAttributeValues(NodeName(i));
end;

function TFXTreeView.NodeName(Nr: integer): string;
begin
  Result:= Name + 'Node' + IntToStr(Nr);
end;

procedure TFXTreeView.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(FEditCancel);
  rename(FEditCommit);
  rename(FEditStart);
  //rename(FScrollTo);
  for var i:= 0 to FItems.Count - 1 do
    Partner.ReplaceWord(OldName + 'Node' + IntToStr(i), NewName + 'Node' + IntToStr(i), true);
end;

procedure TFXTreeView.Paint;
  const cdx = 16;
  var s: string;
      i, x, y, dx, IndentS, RowHeight: integer;
begin
  if FixedCellSize > 0
    then RowHeight:= round(FixedCellSize)
    else RowHeight:= 24;

  CanvasFontAssign;
  Canvas.Font.Color:= DefaultForeground; 
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  y:= PPIScale(2);
  x:= PPIScale(4);
  dx:= PPIScale(cdx);
  RowHeight:= PPIScale(RowHeight);
  // show root
  if FShowRoot then begin
    FGUIDesigner.vilControls1618.Draw(Canvas, x, y, 7);
    Canvas.TextOut(x + dx, y + 2, FItems[0]);
    x:= x + dx;
    y:= y + RowHeight;
  end;
  // show nodes
  for i:= 1 to FItems.Count - 1 do begin
    s:= FItems[i];
    indentS:= getIndent(s);
    if IndentS = 2 then begin
      FGUIDesigner.vilControls1618.Draw(Canvas, x, y, 8);
      Canvas.TextOut(x + dx, y + 2, FItems[i]);
      y:= y + RowHeight;
      if y > Height then break;
    end;
  end;
end;

procedure TFXTreeView.setShowRoot(aValue: boolean);
begin
  if aValue <> FShowRoot then begin
    FShowRoot:= aValue;
    Invalidate;
  end;
end;

procedure TFXTreeView.setItems(aItems: TStrings);
begin
  if FItems.Text <> aItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TFXTreeView.setFixedCellSize(aValue: double);
begin
  if FFixedCellSize <> aValue then begin
    FFixedCellSize:= aValue;
    Invalidate;
  end;
end;

end.
