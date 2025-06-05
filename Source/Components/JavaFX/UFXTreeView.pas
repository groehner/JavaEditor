unit UFXTreeView;

interface

uses
  Classes,
  UFXComponents;

type

  TFXTreeView = class(TFXControl)
  private
    FShowRoot: Boolean;
    FEditable: Boolean;
    FItems: TStrings;
    FFixedCellSize: Double;
    FEditCancel: string;
    FEditCommit: string;
    FEditStart: string;

    procedure SetShowRoot(AValue: Boolean);
    procedure SetItems(AItems: TStrings);
    procedure SetFixedCellSize(AValue: Double);
    function NodeName(Num: Integer): string;
    procedure MakeTree;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure Paint; override;
  published
    property ShowRoot: Boolean read FShowRoot write SetShowRoot;
    property Items: TStrings read FItems write SetItems;
    property Editable: Boolean read FEditable write FEditable;
    property FixedCellSize: Double read FFixedCellSize write SetFixedCellSize;

    property editCancel: string read FEditCancel write FEditCancel;
    property editCommit: string read FEditCommit write FEditCommit;
    property editStart: string read FEditStart write FEditStart;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Controls,
  UJEComponents,
  UGUIDesigner,
  UUtils;

constructor TFXTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 152;
  Width := 240;
  Height := 120;
  Background := clWhite;
  FEditable := False;
  FShowRoot := True;
  FItems := TStringList.Create;
  FItems.Text :=
    'root'#13#10'  node 1'#13#10'    leaf 1'#13#10'    leaf 2'#13#10'    leaf 3'#13#10'  node 2'#13#10'    node 3'#13#10'      leaf 4'#13#10'    leaf 5'#13#10'  node 4'#13#10'    leaf 6'#13#10'    leaf 7';
  Font.Style := []; // not bold;
  JavaType := 'TreeView';
end;

destructor TFXTreeView.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TFXTreeView.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|ShowRoot|Items|Editable|FixedCellSize' + inherited;
end;

procedure TFXTreeView.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeTree
  else
    inherited;
end;

function TFXTreeView.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|editCancel|editCommit|editStart' + inherited;
end;

procedure TFXTreeView.NewControl;
var
  Node1, Node2, Str: string;
begin
  InsertImport('javafx.scene.control.*');
  InsertNewVariable('private TreeItem<String> ' + Name +
    'Node0 = new TreeItem<>("root");');
  InsertNewVariable('private TreeView<String> ' + Name + ' = new TreeView<>(' +
    Name + 'Node0);');

  Str := '';
  Node1 := Name + 'Node1';
  Node2 := Name + 'Node2';
  Str := Str + SurroundFix2('TreeItem<String> ' + Node1 +
    ' = new TreeItem<>("node 1");');
  Str := Str + SurroundFix2
    (Node1 + '.getChildren().add(new TreeItem<String>("leaf 1"));');
  Str := Str + SurroundFix2
    (Node1 + '.getChildren().add(new TreeItem<String>("leaf 2"));');
  Str := Str + SurroundFix2
    (Node1 + '.getChildren().add(new TreeItem<String>("leaf 3"));');
  Str := Str + SurroundFix2(Name + 'Node0.getChildren().add(' + Node1 + ');');
  Str := Str + SurroundFix2(Node1 + ' = new TreeItem<String>("node 2");');
  Str := Str + SurroundFix2('TreeItem<String> ' + Node2 +
    ' = new TreeItem<String>("node 3");');
  Str := Str + SurroundFix2
    (Node2 + '.getChildren().add(new TreeItem<String>("leaf 4"));');
  Str := Str + SurroundFix2(Node1 + '.getChildren().add(' + Node2 + ');');
  Str := Str + SurroundFix2
    (Node1 + '.getChildren().add(new TreeItem<String>("leaf 5"));');
  Str := Str + SurroundFix2(Name + 'Node0.getChildren().add(' + Node1 + ');');
  Str := Str + SurroundFix2(Node1 + ' = new TreeItem<String>("node 4");');
  Str := Str + SurroundFix2
    (Node1 + '.getChildren().add(new TreeItem<String>("leaf 6"));');
  Str := Str + SurroundFix2
    (Node1 + '.getChildren().add(new TreeItem<String>("leaf 7"));');
  Str := Str + SurroundFix2(Name + 'Node0.getChildren().add(' + Node1 + ');');
  Str := Str + SurroundFix2(Name + 'Node0.setExpanded(true);');
  Str := Str + SurroundFix2(GetContainerAdd);
  FPartner.InsertComponent(Str);
end;

procedure TFXTreeView.MakeTree;
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
    ALabel := FItems[NodeNr];
    IndentNr := GetIndent(ALabel);
    ALabel := Trim(ALabel);
    while NodeNr < FItems.Count do
    begin
      if NodeNr + 1 < FItems.Count then
        IndentNr1 := GetIndent(FItems[NodeNr + 1])
      else
        IndentNr1 := IndentNr;
      if IndentNr1 > IndentNr then
      begin
        Str1 := NodeName(Depth) + ' = new TreeItem<String>("' + ALabel + '");';
        if NodesDepth < Depth then
        begin
          Str := Str + SurroundFix2('TreeItem<String> ' + Str1);
          Inc(NodesDepth);
        end
        else
          Str := Str + SurroundFix2(Str1);
        Inc(NodeNr);
        MakeNode(Depth + 1);
        if NodeNr < FItems.Count then
          IndentNr1 := GetIndent(FItems[NodeNr])
        else
          IndentNr1 := 0;
        if IndentNr1 <= IndentNr then
          Str := Str + SurroundFix2(NodeName(Depth - 1) + '.getChildren().add('
            + NodeName(Depth) + ');');
      end
      else if IndentNr1 <= IndentNr then
      begin
        Str := Str + SurroundFix2(NodeName(Depth - 1) +
          '.getChildren().add(new TreeItem<String>("' + ALabel + '"));');
        Inc(NodeNr);
      end;
      if IndentNr1 < IndentNr then
        Exit
      else if NodeNr < FItems.Count then
        ALabel := Trim(FItems[NodeNr]);
    end;
  end;

begin
  Str := Indent1 + 'private TreeItem<String> ' + Name +
    'Node0 = new TreeItem<String>("' + Trim(FItems[0]) + '");';
  FPartner.ReplaceAttribute('private TreeItem<String> ' + Name, Str);
  FPartner.DeleteAttributeValues(Name + '.setExpanded');

  for var I := 0 to FItems.Count - 1 do
    FPartner.DeleteAttributeValues(NodeName(I));
  NodesDepth := 0;
  NodeNr := 1;
  FormatItems(FItems);
  Str := '';
  if FItems.Count > 1 then
    MakeNode(1);
  Str := Str + SurroundFix2(Name + 'Node0.setExpanded(true);');
  FPartner.InsertComponent(Str);
end;

procedure TFXTreeView.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private TreeView<String> ' + Name);
  FPartner.DeleteAttribute('private TreeItem<String> ' + Name + 'Node0');
  for var I := 0 to FItems.Count - 1 do
    FPartner.DeleteAttributeValues(NodeName(I));
end;

function TFXTreeView.NodeName(Num: Integer): string;
begin
  Result := Name + 'Node' + IntToStr(Num);
end;

procedure TFXTreeView.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FEditCancel);
  Rename(FEditCommit);
  Rename(FEditStart);
  for var I := 0 to FItems.Count - 1 do
    FPartner.ReplaceWord(OldName + 'Node' + IntToStr(I),
      NewName + 'Node' + IntToStr(I), True);
end;

procedure TFXTreeView.Paint;
const
  Cdx = 16;
var
  Str: string;
  XPos, YPos, DeltaX, IndentS, RowHeight: Integer;
begin
  if FixedCellSize > 0 then
    RowHeight := Round(FixedCellSize)
  else
    RowHeight := 24;

  CanvasFontAssign;
  Canvas.Font.Color := DefaultForeground;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  YPos := PPIScale(2);
  XPos := PPIScale(4);
  DeltaX := PPIScale(Cdx);
  RowHeight := PPIScale(RowHeight);
  // show root
  if FShowRoot then
  begin
    FGUIDesigner.vilControls1618.Draw(Canvas, XPos, YPos, 7);
    Canvas.TextOut(XPos + DeltaX, YPos + 2, FItems[0]);
    XPos := XPos + DeltaX;
    YPos := YPos + RowHeight;
  end;
  // show nodes
  for var I := 1 to FItems.Count - 1 do
  begin
    Str := FItems[I];
    IndentS := GetIndent(Str);
    if IndentS = 2 then
    begin
      FGUIDesigner.vilControls1618.Draw(Canvas, XPos, YPos, 8);
      Canvas.TextOut(XPos + DeltaX, YPos + 2, FItems[I]);
      YPos := YPos + RowHeight;
      if YPos > Height then
        Break;
    end;
  end;
end;

procedure TFXTreeView.SetShowRoot(AValue: Boolean);
begin
  if AValue <> FShowRoot then
  begin
    FShowRoot := AValue;
    Invalidate;
  end;
end;

procedure TFXTreeView.SetItems(AItems: TStrings);
begin
  if FItems.Text <> AItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TFXTreeView.SetFixedCellSize(AValue: Double);
begin
  if FFixedCellSize <> AValue then
  begin
    FFixedCellSize := AValue;
    Invalidate;
  end;
end;

end.
