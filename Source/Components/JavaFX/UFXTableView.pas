unit UFXTableView;

interface

uses
  Classes,
  UFXComponents;

type

  TFXTableView = class(TFXControl)
  private
    FColumns: TStrings;
    FEditable: Boolean;
    FGenericType: string;
    FFixedCellSize: Double;
    FTableMenuButtonVisible: Boolean;
    FSort: string;
    procedure SetColumns(AColumns: TStrings);
    procedure MakeTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property Columns: TStrings read FColumns write SetColumns;
    property Editable: Boolean read FEditable write FEditable;
    property GenericType: string read FGenericType write FGenericType;
    property TableMenuButtonVisible: Boolean read FTableMenuButtonVisible
      write FTableMenuButtonVisible;
    property FixedCellSize: Double read FFixedCellSize write FFixedCellSize;
    property sort: string read FSort write FSort;
  end;

  TFXTreeTableView = class(TFXTableView)
  private
    procedure MakeTree;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Graphics,
  UUtils;

{ --- TFXTableView ------------------------------------------------------------- }

constructor TFXTableView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +149;
  PrefWidth := 120;
  PrefHeight := 80;
  Background := clWhite;
  FColumns := TStringList.Create;
  FColumns.Text := 'Column 1' + CrLf + 'Column 2';
  FGenericType := 'String';
  JavaType := 'TableView';
end;

destructor TFXTableView.Destroy;
begin
  FreeAndNil(FColumns);
  inherited;
end;

procedure TFXTableView.Paint;
var
  XPos, DeltaX, DeltaY, TextHeight, TextWidth: Integer;
  Str: string;
begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Font.Style := [fsBold];
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));
  Canvas.Brush.Color := RGB(228, 228, 228);
  Canvas.FillRect(Rect(1, 1, Width - 2, PPIScale(25)));

  Canvas.Brush.Color := DefaultBackground;
  Canvas.Pen.Color := DarkShadow;
  TextHeight := Canvas.TextHeight('Hg');

  XPos := 0;
  DeltaX := PPIScale(80);
  DeltaY := PPIScale(25);
  for var I := 1 to FColumns.Count do
  begin
    Str := FColumns[I - 1];
    TextWidth := Canvas.TextWidth(Str);
    Canvas.TextRect(Rect(XPos + 1, 1, XPos + DeltaX - 1, PPIScale(24)),
      XPos + (DeltaX - TextWidth) div 2, (PPIScale(25) - TextHeight) div 2
      + 1, Str);
    XPos := PPIScale(80) * I;
    Canvas.MoveTo(XPos, 0);
    Canvas.LineTo(XPos, DeltaY);
  end;
  Canvas.MoveTo(0, PPIScale(25));
  Canvas.LineTo(Width, PPIScale(25));
end;

procedure TFXTableView.SetColumns(AColumns: TStrings);
begin
  if AColumns.Text <> FColumns.Text then
  begin
    FColumns.Assign(AColumns);
    Invalidate;
  end;
end;

procedure TFXTableView.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertImport('javafx.scene.control.cell.PropertyValueFactory');
  DefaultComponent;
  InsertNewVariable('private TableView ' + Name + ' = new TableView<' +
    GenericType + '>();');
  for var I := 1 to FColumns.Count do
  begin
    InsertNewVariable(Indent1 + 'private TableColumn<' + GenericType +
      ', String> ' + Name + 'Column' + IntToStr(I) + ' = new TableColumn("' +
      FColumns[I - 1] + '");');
    FPartner.InsertAttributValue(FXContainer,
      Indent2 + Name + '.getColumns().add(' + Name + 'Column' + IntToStr(I)
      + ');', 0);
    FPartner.InsertAttributValue(FXContainer, Indent2 + Name + 'Column' +
      IntToStr(I) + '.setCellValueFactory(new PropertyValueFactory<' +
      GenericType + ', String>("' + FColumns[I - 1] + '"));', 0);
  end;
  InsertNewVariable(Indent1 + 'private ObservableList<' + GenericType + '> ' +
    Name + 'Items = FXCollections.observableArrayList();');
  MakeAttribut('Items', Name + 'Items');
end;

procedure TFXTableView.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Columns') or (Attr = 'GenericType') then
    MakeTable
  else
    inherited;
end;

function TFXTableView.GetEvents(ShowEvents: Integer): string;
// const Events = '|sort|scrollTo|scrollToColumn';
const
  Events = '|sort';
begin
  Result := Events + inherited GetEvents(ShowEvents);
end;

function TFXTableView.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Columns|GenericType';
  Attributes2 = Attributes1 + '|Editable|FixedCellSize|TableMenuButtonVisible';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited
  else
    Result := Attributes2 + inherited;
end;

procedure TFXTableView.DeleteComponent;
begin
  inherited;
  for var I := 1 to FColumns.Count do
  begin
    FPartner.DeleteAttribute(Name + 'Column' + IntToStr(I));
    FPartner.DeleteAttributeValues(Name + 'Column' + IntToStr(I));
  end;
  FPartner.DeleteAttribute(Name + 'Items');
end;

procedure TFXTableView.MakeTable;
begin
  FPartner.Editor.BeginUpdate;
  DeleteComponent;
  NewControl;
  FPartner.Editor.EndUpdate;
end;

procedure TFXTableView.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FSort);
  FPartner.ReplaceWord(OldName + 'Items', NewName + 'Items', True);
  for var I := 1 to FColumns.Count do
    FPartner.ReplaceWord(OldName + 'Column' + IntToStr(I),
      NewName + 'Column' + IntToStr(I), True);
end;

{ --- TFXTreeTableView --------------------------------------------------------- }

constructor TFXTreeTableView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +151;
  PrefWidth := 120;
  PrefHeight := 80;
  Background := clWhite;
  FGenericType := 'String';
  JavaType := 'TreeTableView';
end;

procedure TFXTreeTableView.Paint;
var
  XPos, DeltaX, DeltaY, TextHeight, TextWidth: Integer;
  Str: string;
begin
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := Foreground;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));
  Canvas.Brush.Color := RGB(228, 228, 228);
  Canvas.FillRect(Rect(1, 1, Width - 2, 25));

  Canvas.Brush.Color := DefaultBackground;
  Canvas.Pen.Color := DarkShadow;
  TextHeight := Canvas.TextHeight('Hg');

  XPos := 0;
  DeltaX := 80;
  DeltaY := 25;
  for var I := 1 to FColumns.Count do
  begin
    Str := FColumns[I - 1];
    TextWidth := Canvas.TextWidth(Str);
    Canvas.TextRect(Rect(XPos + 1, 1, XPos + DeltaX - 1, 24),
      XPos + (DeltaX - TextWidth) div 2, (25 - TextHeight) div 2 + 1, Str);
    XPos := 80 * I;
    Canvas.MoveTo(XPos, 0);
    Canvas.LineTo(XPos, DeltaY);
  end;
  Canvas.MoveTo(0, 25);
  Canvas.LineTo(Width, 25);
end;

procedure TFXTreeTableView.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Columns') or (Attr = 'GenericType') then
    MakeTree
  else
    inherited;
end;

procedure TFXTreeTableView.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertImport('javafx.scene.control.cell.*');
  DefaultComponent;
  InsertNewVariable('private TreeTableView<' + GenericType + '> ' + Name +
    ' = new TreeTableView<' + GenericType + '>();');
  for var I := 1 to FColumns.Count do
  begin
    InsertNewVariable(Indent1 + 'private TreeTableColumn<' + GenericType +
      ', String> ' + Name + 'Column' + IntToStr(I) +
      ' = new TreeTableColumn("' + FColumns[I - 1] + '");');
    FPartner.InsertAttributValue(FXContainer,
      Indent2 + Name + '.getColumns().add(' + Name + 'Column' + IntToStr(I)
      + ');', 0);
    FPartner.InsertAttributValue(FXContainer,
      Indent2 + Name +
      'Column1.setCellValueFactory(new TreeItemPropertyValueFactory<' +
      GenericType + ', String>("' + FColumns[I - 1] + '"));', 0);
  end;
  InsertNewVariable(Indent1 + 'private TreeItem<' + GenericType + '> ' + Name +
    'root = new TreeItem<>(new ' + GenericType + '());');
  MakeAttribut('Root', Name + 'root');
end;

procedure TFXTreeTableView.MakeTree;
begin
  FPartner.Editor.BeginUpdate;
  DeleteComponent;
  NewControl;
  FPartner.Editor.EndUpdate;
end;

procedure TFXTreeTableView.DeleteComponent;
begin
  inherited;
  for var I := 1 to FColumns.Count do
    FPartner.DeleteAttribute(Name + 'Column' + IntToStr(I));
  FPartner.DeleteAttribute(Name + 'root');
  FPartner.DeleteAttribute(Name + ' = new TreeTableView');
end;

procedure TFXTreeTableView.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'root', NewName + 'root', True);
  for var I := 1 to FColumns.Count do
    FPartner.ReplaceWord(OldName + 'Column' + IntToStr(I),
      NewName + 'Column' + IntToStr(I), True);
end;

end.
