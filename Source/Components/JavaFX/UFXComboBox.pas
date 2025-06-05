unit UFXComboBox;

{ classes
  TFXComboBoxBase = class(TFXControl)
  TFXComboBox = class (TFXComboBoxBase)
  TFXColorPicker = class(TFXComboBoxBase)
  TFXDatePicker = class(TFXComboBoxBase)
}

interface

uses
  Classes, Graphics, UFXComponents;

type

  TFXComboBoxBase = class(TFXControl)
  private
    FEditable: Boolean;
    FPromptText: string;
    FValue: string;

    Faction: string;
    Fhidden: string;
    Fhiding: string;
    Fshowing: string;
    Fshown: string;
  protected
    procedure SetEditable(AValue: Boolean);
  public
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property Editable: Boolean read FEditable write SetEditable;
    property PromptText: string read FPromptText write FPromptText;
    property Value: string read FValue write FValue;

    property action: string read Faction write Faction;
    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

  TFXComboBox = class(TFXComboBoxBase)
  private
    FVisibleRowCount: Integer;
    FItems: TStrings;
    FPromptText: string;
    FValue: string;
    procedure SetItems(AItems: TStrings);
    procedure SetPromptText(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, AValue, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property VisibleRowCount: Integer read FVisibleRowCount
      write FVisibleRowCount default 8;
    property Items: TStrings read FItems write SetItems;
    property PromptText: string read FPromptText write SetPromptText;
    property Value: string read FValue write SetValue;
  end;

  TFXColorPicker = class(TFXComboBoxBase)
  private
    FAValue: TColor;
    procedure SetValue(AColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, AValue, Typ: string); override;
  published
    property Value: TColor read FAValue write SetValue;
  end;

  TFXDatePicker = class(TFXComboBoxBase)
  private
    FShowWeekNumbers: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, AValue, Typ: string); override;
  published
    property ShowWeekNumbers: Boolean read FShowWeekNumbers
      write FShowWeekNumbers;
  end;

implementation

uses SysUtils, Controls, Types, UGUIDesigner, ULink, UUtils;

{ --- TFXComboBoxBase ---------------------------------------------------------- }

procedure TFXComboBoxBase.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

function TFXComboBoxBase.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes = '|Armed|Editable|PromptText|Value';
begin
  Result := Attributes + inherited GetAttributes(ShowAttributes);
end;

function TFXComboBoxBase.GetEvents(ShowEvents: Integer): string;
const
  Events = '|action|hidden|hiding|showing|shown';
begin
  Result := Events + inherited GetEvents(ShowEvents);
end;

procedure TFXComboBoxBase.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(Faction);
  Rename(Fhidden);
  Rename(Fhiding);
  Rename(Fshowing);
  Rename(Fshown);
end;

{ --- TFXComboBox -------------------------------------------------------------- }

constructor TFXComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +110;
  PrefWidth := 80;
  PrefHeight := 24;
  FItems := TStringList.Create;
  FItems.Text := DefaultItems;
  FValue := FItems[0];
  FVisibleRowCount := 10;
  JavaType := 'ComboBox';
end;

destructor TFXComboBox.Destroy;
begin
  inherited;
  FreeAndNil(FItems);
end;

procedure TFXComboBox.Paint;
var
  XPos, YPos, Dxy, TextHeight: Integer;
  Points: array [0 .. 2] of TPoint;
  Str: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);
  if FEditable then
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.MoveTo(Width - PPIScale(25), 0);
    Canvas.LineTo(Width - PPIScale(25), Height);
  end;

  Dxy := PPIScale(5);
  Canvas.Pen.Color := DefaultForeground;
  Canvas.Brush.Color := DefaultForeground;
  XPos := Width - PPIScale(12);
  YPos := Height div 2 + Dxy div 2;
  Points[0] := Point(XPos, YPos);
  Points[1] := Point(XPos - Dxy, YPos - Dxy);
  Points[2] := Point(XPos + Dxy, YPos - Dxy);
  Canvas.Polygon(Points);

  if FValue <> '' then
  begin
    Canvas.Brush.Color := Background;
    Str := FValue;
    while Canvas.TextWidth(Str) > Width - PPIScale(25 + 5) do
      Delete(Str, Length(Str), 1);
    TextHeight := Canvas.TextHeight(Str);
    YPos := (Height - TextHeight) div 2;
    Canvas.TextOut(PPIScale(8), YPos, Str);
  end;
end;

procedure TFXComboBox.SetItems(AItems: TStrings);
begin
  if AItems.Text <> FItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TFXComboBox.SetPromptText(const AValue: string);
begin
  if FPromptText <> AValue then
  begin
    FPromptText := AValue;
    Invalidate;
  end;
end;

procedure TFXComboBox.SetValue(const AValue: string);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    Invalidate;
  end;
end;

procedure TFXComboBox.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertNewVariable('private ComboBox<String> ' + Name + ' = new ComboBox<>();'
    + CrLf + GetIndentation + '  private ObservableList<String> ' + Name +
    'ObservableList = ' + CrLf + GetIndentation +
    '          FXCollections.observableArrayList();');
  DefaultComponent;
  MakeAttribut('Items', Name + 'ObservableList');
  MakeAttribut('Value', AsString(FValue));
  MakeList;
end;

procedure TFXComboBox.SetAttribute(Attr, AValue, Typ: string);
var
  Str, Key: string;
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectionMode' then
  begin
    Key := Name + '.getSelectionModel()';
    Str := Key + '.setSelectionMode(SelectionMode.' + AValue + ');';
    SetAttributValue(Key, Str);
  end
  else if Attr = 'Orientation' then
  begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'Orientation.' + Value);
  end
  else
    inherited;
end;

procedure TFXComboBox.MakeList;
begin
  FPartner.DeleteComponent(Name + 'ObservableList.add(');
  for var I := 0 to Items.Count - 1 do
  begin
    var
    Str := Name + 'ObservableList.add("' + Items[I] + '");';
    SetAttributValue('___XXX___', Str);
  end;
end;

function TFXComboBox.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes = '|Items|SelectedIndex|VisibleRowCount';
begin
  Result := Attributes + inherited GetAttributes(ShowAttributes);
  Delete(Result, Pos('|Background', Result), 11);
end;

procedure TFXComboBox.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceComponentname(OldName + 'ObservableList',
    NewName + 'ObservableList', Events);
end;

procedure TFXComboBox.DeleteComponent;
var
  Int: Integer;
begin
  inherited;
  FPartner.DeleteAttribute('private ComboBox<String> ' + Name);
  Int := FPartner.GetLineNumberWith('private ObservableList<String> ' + Name +
    'ObservableList');
  FPartner.DeleteBlock(Int, Int + 1);
  FPartner.DeleteComponent(Name + 'ObservableList.add');
end;

{ --- TFXColorPicker ----------------------------------------------------------- }

constructor TFXColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +141;
  PrefWidth := 120;
  PrefHeight := 24;
  FAValue := clWhite;
  JavaType := 'ColorPicker';
end;

procedure TFXColorPicker.Paint;
var
  XPos, YPos, TextHeight, Dxy: Integer;
  Points: array [0 .. 2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Brush.Color := FAValue;
  Canvas.Rectangle(Rect(PPIScale(8), Height div 2 - PPIScale(7), PPIScale(21),
    Height div 2 + PPIScale(6)));
  Canvas.Brush.Color := Background;
  TextHeight := Canvas.TextHeight('A');
  Canvas.TextOut(PPIScale(24), (Height - TextHeight) div 2,
    Delphi2JavaColors(ColorToString(FAValue)));

  Dxy := PPIScale(5);
  Canvas.Pen.Color := DefaultForeground;
  Canvas.Brush.Color := DefaultForeground;
  XPos := Width - PPIScale(10);
  YPos := Height div 2 + Dxy div 2;
  Points[0] := Point(XPos, YPos);
  Points[1] := Point(XPos - Dxy, YPos - Dxy);
  Points[2] := Point(XPos + Dxy, YPos - Dxy);
  Canvas.Polygon(Points);
end;

procedure TFXColorPicker.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ColorPicker ' + Name + ' = new ColorPicker();');
  InsertImport('javafx.scene.paint.Color');
end;

function TFXColorPicker.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := inherited GetAttributes(ShowAttributes);
  Delete(Result, Pos('|PromptText', Result), 11);
end;

procedure TFXColorPicker.SetAttribute(Attr, AValue, Typ: string);
begin
  if Attr = 'Value' then
    MakeAttribut(Attr, GetAttrColor(AValue))
  else
    inherited;
end;

procedure TFXColorPicker.SetValue(AColor: TColor);
begin
  if AColor <> FAValue then
  begin
    FAValue := AColor;
    Invalidate;
  end;
end;

{ --- TFXDatePicker ------------------------------------------------------------ }

constructor TFXDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +142;
  PrefWidth := 120;
  PrefHeight := 24;
  FValue := FormatDateTime('yyyy-mm-dd', Now);
  ShowWeekNumbers := True;
  JavaType := 'DatePicker';
end;

procedure TFXDatePicker.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(0, 0, Width, Height);
  var
  TextHeight := Canvas.TextHeight('A');
  Canvas.TextOut(5, (Height - TextHeight) div 2, FValue);
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(Width - PPIScale(25), 0, Width, Height));
  FGUIDesigner.vilControls21616.Draw(Canvas, Width - PPIScale(20),
    (Height - PPIScale(16)) div 2, 5);
end;

procedure TFXDatePicker.NewControl;
begin
  InsertImport('java.time.LocalDate');
  InsertNewVariable('private DatePicker ' + Name + ' = new DatePicker();');
  DefaultComponent;
  MakeAttribut('ShowWeekNumbers', 'true');
  SetAttribute('Value', FValue, '');
end;

function TFXDatePicker.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|ShowWeekNumbers' + inherited GetAttributes(ShowAttributes);
  Delete(Result, Pos('|PromptText', Result), 11);
end;

procedure TFXDatePicker.SetAttribute(Attr, AValue, Typ: string);
var
  Str, Key: string;
begin
  if Attr = 'Value' then
  begin
    Key := Name + '.setValue(';
    Str := Key + 'LocalDate.parse("' + AValue + '"));';
    SetAttributValue(Key, Str);
  end
  else
    inherited;
end;

end.
