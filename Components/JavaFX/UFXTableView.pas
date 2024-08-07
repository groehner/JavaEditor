unit UFXTableView;

interface

uses
  Classes, Graphics, UFXComponents;

type

  TFXTableView = class (TFXControl)
  private
    FColumns: TStrings;
    FEditable: boolean;
    FGenericType: string;
    FFixedCellSize: double;
    FTableMenuButtonVisible: boolean;

    FSort: string;
    //FscrollTo: string;
    procedure setColumns(aColumns: TStrings);
    procedure MakeTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property Columns: TStrings read FColumns write setColumns;
    property Editable: boolean read FEditable write FEditable;
    property GenericType: string read FGenericType write FGenericType;
    property TableMenuButtonVisible: boolean read FtableMenuButtonVisible write FtableMenuButtonVisible;
    property FixedCellSize: double read FFixedCellSize write FFixedCellSize;

    property sort: string read FSort write FSort;
    //property scrollTo: string read FscrollTo write FscrollTo;
  end;

  TFXTreeTableView = class (TFXTableView)
  private
    procedure MakeTree;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  end;

implementation

uses Windows, SysUtils, UUtils;

{--- TFXTableView -------------------------------------------------------------}

constructor TFXTableView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +149;
  PrefWidth := 120;
  PrefHeight:= 80;
  Background:= clWhite;
  FColumns:= TStringList.Create;
  FColumns.Text:= 'Column 1' + CrLf + 'Column 2';
  FGenericType:= 'String';
  JavaType:= 'TableView';
end;

destructor TFXTableView.Destroy;
begin
  FreeAndNil(FColumns);
  inherited;
end;

procedure TFXTableView.Paint;
  var x,  dx, dy, th, tw, i: integer; s: string;
begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Font.Style:= [fsBold];
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));
  Canvas.Brush.Color:= RGB(228, 228, 228);
  Canvas.FillRect(Rect(1, 1, Width-2, PPIScale(25)));

  Canvas.Brush.Color:= DefaultBackground;
  Canvas.Pen.Color:= DarkShadow;
  th:= Canvas.TextHeight('Hg');

  x:= 0;
  dx:= PPIScale(80);
  dy:= PPIScale(25);
  for i:= 1 to FColumns.Count do begin
    s:= FColumns[i-1];
    tw:= Canvas.TextWidth(s);
    Canvas.TextRect(Rect(x+1, 1, x + dx-1, PPIScale(24)), x + (dx-tw) div 2 , (PPIScale(25)-th) div 2 +1, s);
    x:= PPIScale(80)*i;
    Canvas.MoveTo(x, 0);
    Canvas.LineTo(x, dy);
  end;
  Canvas.MoveTo(0, PPIScale(25));
  Canvas.LineTo(Width, PPIScale(25));
end;

procedure TFXTableView.setColumns(aColumns: TStrings);
begin
  if aColumns.Text <> FColumns.Text then begin
    FColumns.Assign(aColumns);
    Invalidate;
  end;
end;

procedure TFXTableView.NewControl;
  var i: integer;
begin
  InsertImport('javafx.collections.*');
  InsertImport('javafx.scene.control.cell.PropertyValueFactory');
  DefaultComponent;
  InsertNewVariable('private TableView ' + Name + ' = new TableView<' + GenericType + '>();');
  for i:= 1 to FColumns.Count do begin
    InsertNewVariable(Indent1 + 'private TableColumn<' + GenericType + ', String> ' + Name + 'Column' + IntToStr(i) + ' = new TableColumn("' + FColumns.Strings[i-1]+ '");');
    Partner.InsertAttributValue(FXContainer, Indent2 + Name + '.getColumns().add(' + Name + 'Column' + IntToStr(i) + ');', 0);
    Partner.InsertAttributValue(FXContainer, Indent2 + Name + 'Column' + IntTostr(i) + '.setCellValueFactory(new PropertyValueFactory<' + GenericType  + ', String>("' + FColumns.Strings[i-1] + '"));', 0);
  end;
  InsertNewVariable(Indent1 + 'private ObservableList<' + GenericType + '> ' + Name + 'Items = FXCollections.observableArrayList();');
  MakeAttribut('Items', Name + 'Items');
end;

procedure TFXTableView.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Columns') or (Attr = 'GenericType') then
    MakeTable
  else
    inherited
end;

function TFXTableView.getEvents(ShowEvents: integer): string;
//  const Events = '|sort|scrollTo|scrollToColumn';
  const Events = '|sort';
begin
  Result:= Events + inherited getEvents(ShowEvents);
end;

function TFXTableView.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Columns|GenericType';
        Attributes2 = Attributes1 + '|Editable|FixedCellSize|TableMenuButtonVisible';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited
    else Result:= Attributes2 + inherited;
end;

procedure TFXTableView.DeleteComponent;
begin
  inherited;
  for var i:= 1 to FColumns.Count do begin
    Partner.DeleteAttribute(Name + 'Column' + IntToStr(i));
    Partner.DeleteAttributeValues(Name + 'Column' + IntToStr(i));
  end;
  Partner.DeleteAttribute(Name + 'Items');
end;

procedure TFXTableView.MakeTable;
begin
  Partner.Editor.BeginUpdate;
  DeleteComponent;
  NewControl;
  Partner.Editor.EndUpdate;
end;

procedure TFXTableView.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(Fsort);
  Partner.ReplaceWord(OldName + 'Items' , NewName + 'Items', true);
  for var i:= 1 to FColumns.Count do
    Partner.ReplaceWord(OldName + 'Column' + IntToStr(i),
                        NewName + 'Column' + IntToStr(i), true);
end;

{--- TFXTreeTableView ---------------------------------------------------------}

constructor TFXTreeTableView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +151;
  PrefWidth := 120;
  PrefHeight:= 80;
  Background:= clWhite;
  FGenericType:= 'String';
  JavaType:= 'TreeTableView';
end;

procedure TFXTreeTableView.Paint;
  var x, dx, dy, th, tw, i: integer; s: string;
begin
  Canvas.Font.Assign(Font);
  Canvas.Font.Color:= Foreground;
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));
  Canvas.Brush.Color:= RGB(228, 228, 228);
  Canvas.FillRect(Rect(1, 1, Width-2, 25));

  Canvas.Brush.Color:= DefaultBackground;
  Canvas.Pen.Color:= DarkShadow;
  th:= Canvas.TextHeight('Hg');

  x:= 0;
  dx:= 80;
  dy:= 25;
  for i:= 1 to FColumns.Count do begin
    s:= FColumns.Strings[i-1];
    tw:= Canvas.TextWidth(s);
    Canvas.TextRect(Rect(x+1, 1, x + dx-1, 24), x + (dx-tw) div 2 , (25-th) div 2 +1, s);
    x:= 80*i;
    Canvas.MoveTo(x, 0);
    Canvas.LineTo(x, dy);
  end;
  Canvas.MoveTo(0, 25);
  Canvas.LineTo(Width, 25);
end;

procedure TFXTreeTableView.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Columns') or (Attr = 'GenericType') then
    MakeTree
  else
    inherited
end;

procedure TFXTreeTableView.NewControl;
  var i: integer;
begin
  InsertImport('javafx.collections.*');
  InsertImport('javafx.scene.control.cell.*');
  DefaultComponent;
  InsertNewVariable('private TreeTableView<' + GenericType + '> ' + Name + ' = new TreeTableView<' + GenericType + '>();');
  for i:= 1 to FColumns.Count do begin
    InsertNewVariable(Indent1 + 'private TreeTableColumn<' + GenericType + ', String> ' + Name + 'Column' + IntToStr(i) + ' = new TreeTableColumn("' + FColumns.Strings[i-1]+ '");');
    Partner.InsertAttributValue(FXContainer, Indent2 + Name + '.getColumns().add(' + Name + 'Column' + IntToStr(i) + ');', 0);
    Partner.InsertAttributValue(FXContainer, Indent2 + Name + 'Column1.setCellValueFactory(new TreeItemPropertyValueFactory<' + GenericType  + ', String>("' + FColumns.Strings[i-1] + '"));', 0);
  end;
  InsertNewVariable(Indent1 + 'private TreeItem<' + GenericType + '> ' + Name + 'root = new TreeItem<>(new ' + GenericType + '());');
  MakeAttribut('Root', Name + 'root');
end;

procedure TFXTreeTableView.MakeTree;
begin
  Partner.Editor.BeginUpdate;
  DeleteComponent;
  NewControl;
  Partner.Editor.EndUpdate;
end;

procedure TFXTreeTableView.DeleteComponent;
begin
  inherited;
  for var i:= 1 to FColumns.Count do
    Partner.DeleteAttribute(Name + 'Column' + IntToStr(i));
  Partner.DeleteAttribute(Name + 'root');
  Partner.DeleteAttribute(Name + ' = new TreeTableView');
end;

procedure TFXTreeTableView.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'root' , NewName + 'root', true);
  for var i:= 1 to FColumns.Count do
    Partner.ReplaceWord(OldName + 'Column' + IntToStr(i),
                        NewName + 'Column' + IntToStr(i), true);
end;

end.
