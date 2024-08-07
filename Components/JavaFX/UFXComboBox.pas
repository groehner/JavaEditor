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
    FEditable: boolean;
    FPromptText: string;
    FValue: string;

    Faction: string;
    Fhidden: string;
    Fhiding: string;
    Fshowing: string;
    Fshown: string;
  protected
    procedure setEditable(aValue: boolean);
  public
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property Editable: boolean read FEditable write setEditable;
    property PromptText: string read FPromptText write FPromptText;
    property Value: string read FValue write FValue;

    property action: string read Faction write Faction;
    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

  TFXComboBox = class (TFXComboBoxBase)
  private
    FVisibleRowCount: integer;
    FItems: TStrings;
    FPromptText: string;
    FValue: string;
    procedure setItems(aItems: TStrings);
    procedure setPromptText(const aValue: string);
    procedure setValue(const aValue: string);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, aValue, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property VisibleRowCount: integer read FVisibleRowCount write FVisibleRowCount default 8;
    property Items: TStrings read FItems write setItems;
    property PromptText: string read FPromptText write setPromptText;
    property Value: string read FValue write setValue;    
  end;

  TFXColorPicker = class(TFXComboBoxBase)
  private
    FaValue: TColor;
    procedure setValue(aColor: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, aValue, Typ: string); override;
  published
    property Value: TColor read FaValue write setValue;
  end;

  TFXDatePicker = class(TFXComboBoxBase)
  private
    FShowWeekNumbers: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, aValue, Typ: string); override;
  published
    property ShowWeekNumbers: boolean read FShowWeekNumbers write FShowWeekNumbers;
  end;

implementation

uses SysUtils, Controls, Types, UGUIDesigner, ULink, UUtils;

{--- TFXComboBoxBase ----------------------------------------------------------}

procedure TFXComboBoxBase.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

function TFXComboBoxBase.getAttributes(ShowAttributes: integer): string;
  const Attributes = '|Armed|Editable|PromptText|Value';
begin
  Result:= Attributes + inherited getAttributes(ShowAttributes);
end;

function TFXComboBoxBase.getEvents(ShowEvents: integer): string;
  const Events = '|action|hidden|hiding|showing|shown';
begin
  Result:= Events + inherited getEvents(ShowEvents);
end;

procedure TFXComboBoxBase.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(Faction);
  rename(Fhidden);
  rename(Fhiding);
  rename(Fshowing);
  rename(Fshown);
end;

{--- TFXComboBox --------------------------------------------------------------}

constructor TFXComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +110;
  PrefWidth:= 80;
  PrefHeight:= 24;
  FItems:= TStringList.Create;
  FItems.Text:= defaultItems;
  FValue:= FItems[0];
  FVisibleRowCount:= 10;
  JavaType:= 'ComboBox';
end;

destructor TFXComboBox.Destroy;
begin
  inherited;
  FreeAndNil(FItems);
end;

procedure TFXComboBox.Paint;
  var x, y, dxy, th: integer;
      Points: array[0..2] of TPoint;
      s: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);
  if FEditable then begin
    Canvas.Brush.Color:= clWhite;
    Canvas.MoveTo(Width - PPIScale(25), 0);
    Canvas.LineTo(Width - PPIScale(25), Height);
  end;

  dxy:= PPIScale(5);
  Canvas.Pen.Color:= DefaultForeground;
  Canvas.Brush.Color:= DefaultForeground;
  x:= Width - PPIScale(12);
  y:= Height div 2 + dxy div 2;
  Points[0]:= Point(x, y);
  Points[1]:= Point(x - dxy, y - dxy);
  Points[2]:= Point(x + dxy, y - dxy);
  Canvas.Polygon(Points);

  if FValue <> '' then begin
    Canvas.Brush.Color:= Background;
    s:= FValue;
    while Canvas.TextWidth(s) > Width - PPIScale(25 + 5) do
      delete(s, length(s), 1);
    th:= Canvas.TextHeight(s);
    y:= (Height - th) div 2;
    Canvas.TextOut(PPIScale(8), y, s);
  end;
end;

procedure TFXComboBox.setItems(aItems: TStrings);
begin
  if aItems.Text <> FItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TFXComboBox.setPromptText(const aValue: string);
begin
  if FPromptText <> aValue then begin
    FPromptText:= aValue;
    Invalidate;
  end;
end;

procedure TFXComboBox.setValue(const aValue: string);
begin
  if FValue <> aValue then begin
    FValue:= aValue;
    Invalidate;
  end;
end;

procedure TFXComboBox.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertNewVariable('private ComboBox<String> ' + Name + ' = new ComboBox<>();' + CrLf + GetIndentation +
                    '  private ObservableList<String> ' + Name + 'ObservableList = ' + CrLf + GetIndentation +
                    '          FXCollections.observableArrayList();');
  DefaultComponent;
  MakeAttribut('Items', Name + 'ObservableList');
  MakeAttribut('Value', asString(FValue));
  MakeList;
end;

procedure TFXComboBox.SetAttribute(Attr, aValue, Typ: string);
  var s, key: string;
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectionMode' then begin
    key:= Name + '.getSelectionModel()';
    s:= key + '.setSelectionMode(SelectionMode.' + aValue + ');';
    setAttributValue(key, s);
  end else if Attr = 'Orientation' then begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'Orientation.' + Value);
  end else
    inherited
end;

procedure TFXComboBox.MakeList;
begin
  Partner.DeleteComponent(Name + 'ObservableList.add(');
  for var i:= 0 to Items.Count - 1 do begin
    var s:= Name + 'ObservableList.add("' + Items.Strings[i] + '");';
    setAttributValue('___XXX___', s);
  end
end;

function TFXComboBox.getAttributes(ShowAttributes: integer): string;
  const Attributes = '|Items|SelectedIndex|VisibleRowCount';
begin
  Result:= Attributes + inherited getAttributes(ShowAttributes);
  delete(Result, Pos('|Background', Result), 11);
end;

procedure TFXComboBox.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceComponentname(OldName + 'ObservableList' , NewName + 'ObservableList', Events);
end;

procedure TFXComboBox.DeleteComponent;
  var i: integer;
begin
  inherited;
  Partner.DeleteAttribute('private ComboBox<String> ' + Name);
  i:= Partner.getLineNumberWith('private ObservableList<String> ' + Name + 'ObservableList');
  Partner.DeleteBlock(i, i+1);
  Partner.DeleteComponent(Name + 'ObservableList.add');  
end;

{--- TFXColorPicker -----------------------------------------------------------}

constructor TFXColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +141;
  PrefWidth:= 120;
  PrefHeight:= 24;
  FaValue:= clWhite;
  JavaType:= 'ColorPicker';
end;

procedure TFXColorPicker.Paint;
  var x, y, th, dxy: integer;
      Points: array[0..2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Brush.Color:= FaValue;
  Canvas.Rectangle(Rect(PPIScale(8), Height div 2 - PPIScale(7),
                        PPIScale(21), Height div 2 + PPIScale(6)));
  Canvas.Brush.Color:= Background;
  th:= Canvas.TextHeight('A');
  Canvas.TextOut(PPIScale(24), (Height - th) div 2, Delphi2JavaColors(ColorToString(FaValue)));

  dxy:= PPIScale(5);
  Canvas.Pen.Color:= DefaultForeground;
  Canvas.Brush.Color:= DefaultForeground;
  x:= Width - PPIScale(10);
  y:= Height div 2 + dxy div 2;
  Points[0]:= Point(x, y);
  Points[1]:= Point(x - dxy, y - dxy);
  Points[2]:= Point(x + dxy, y - dxy);
  Canvas.Polygon(Points);
end;

procedure TFXColorPicker.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ColorPicker ' + Name + ' = new ColorPicker();');
  InsertImport('javafx.scene.paint.Color');
end;

function TFXColorPicker.getAttributes(ShowAttributes: integer): string;
begin
  Result:= inherited getAttributes(ShowAttributes);
  delete(Result, Pos('|PromptText', Result), 11);
end;

procedure TFXColorPicker.SetAttribute(Attr, aValue, Typ: string);
begin
  if Attr = 'Value' then
    MakeAttribut(Attr, getAttrColor(aValue))
  else
    inherited;
end;

procedure TFXColorPicker.setValue(aColor: TColor);
begin
  if aColor <> FaValue then begin
    FaValue:= aColor;
    Invalidate;
  end;
end;

{--- TFXDatePicker ------------------------------------------------------------}

constructor TFXDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +142;
  PrefWidth:= 120;
  PrefHeight:= 24;
  FValue:= FormatDateTime('yyyy-mm-dd', Now);
  ShowWeekNumbers:= true;
  JavaType:= 'DatePicker';
end;

procedure TFXDatePicker.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(0, 0, Width, Height);
  var th:= Canvas.TextHeight('A');
  Canvas.TextOut(5, (Height - th) div 2, FValue);
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(Width - PPIScale(25), 0, Width, Height));
  FGUIDesigner.vilControls21616.Draw(Canvas, Width - PPIScale(20), (Height - PPIScale(16)) div 2, 5);
end;

procedure TFXDatePicker.NewControl;
begin
  InsertImport('java.time.LocalDate');
  InsertNewVariable('private DatePicker ' + Name + ' = new DatePicker();');
  DefaultComponent;
  MakeAttribut('ShowWeekNumbers', 'true');
  SetAttribute('Value', FValue, '');
end;

function TFXDatePicker.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|ShowWeekNumbers' + inherited getAttributes(ShowAttributes);
  delete(Result, Pos('|PromptText', Result), 11);
end;

procedure TFXDatePicker.SetAttribute(Attr, aValue, Typ: string);
  var s, key: string;
begin
  if Attr = 'Value' then begin
    key:= Name + '.setValue(';
    s:= key + 'LocalDate.parse("' + aValue + '"));';
    setAttributValue(key, s);
  end else
    inherited;
end;


end.
