unit UFXChoiceBox;

interface

uses
  Classes, UFXComponents;

type

  TFXChoiceBox = class (TFXControl)
  private
    FItems: TStrings;
    FValue: string;
    procedure setItems(aItems: TStrings);
    procedure setValue(const aValue: string);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure DeleteComponent; override;
  published
    property Items: TStrings read FItems write setItems;
    property Value: string read FValue write setValue;
  end;

implementation

uses Graphics, Types, SysUtils;

{--- TFXChoiceBox --------------------------------------------------------------}

constructor TFXChoiceBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +138;
  PrefWidth:= 80;
  PrefHeight:= 24;
  FItems:= TStringList.Create;
  FItems.Text:= defaultItems;
  FValue:= FItems[0];
  JavaType:= 'ChoiceBox';
end;

destructor TFXChoiceBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TFXChoiceBox.Paint;
  var x, y, dxy, tw: integer; s: string;
      Points: array[0..2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  if (FValue <> '') and (Pos(FValue, Items.Text) > 0) then begin
    s:= FValue;
    tw:= Canvas.textWidth(s);
    while tw > Width - 20 do begin
      delete(s, length(s), 1);
      tw:= Canvas.textWidth(s);
    end;
    if s <> FValue then s:= s + '...';
    Canvas.TextOut(5, (Height - Canvas.TextHeight('A')) div 2, s);
  end;

  Canvas.Pen.Color:= DefaultForeground;
  Canvas.Brush.Color:= DefaultForeground;
  dxy:= 5;
  x:= Width - 10;
  y:= Height div 2 + dxy div 2;
  Points[0]:= Point(x, y);
  Points[1]:= Point(x - dxy, y - dxy);
  Points[2]:= Point(x + dxy, y - dxy);
  Canvas.Polygon(Points);
end;

procedure TFXChoiceBox.setItems(aItems: TStrings);
begin
  if aItems.Text <> FItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TFXChoiceBox.setValue(const aValue: string);
begin
  if FValue <> aValue then begin
    FValue:= aValue;
    Invalidate;
  end;
end;

procedure TFXChoiceBox.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertNewVariable('private ChoiceBox ' + Name + ' = new ChoiceBox();' + CrLf + GetIndentation +
                    '  private ObservableList<String> ' + Name + 'ObservableList = ' + CrLf + GetIndentation +
                    '          FXCollections.observableArrayList();');
  DefaultComponent;
  SetAttribute('Value', asString(FValue), '');
  MakeAttribut('Items', Name + 'ObservableList');
  MakeList;
end;

procedure TFXChoiceBox.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else if Attr= 'SelectionMode' then begin
    MakeAttribut(Attr, 'SelectionMode.' + Value);
  end else if Attr= 'Orientation' then begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'Orientation.' + Value);
  end else
    inherited
end;

procedure TFXChoiceBox.MakeList;
begin
  Partner.DeleteComponent(Name + 'ObservableList.add(');
  for var i:= 0 to Items.Count - 1 do begin
    var s:= Name + 'ObservableList.add("' + Items.Strings[i] + '");';
    setAttributValue('___XXX___', s);
  end;
end;

function TFXChoiceBox.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Items|Value' + inherited getAttributes(ShowAttributes);
end;

procedure TFXChoiceBox.DeleteComponent;
  var i: integer;
begin
  inherited;
  Partner.DeleteComponent(Name + 'ObservableList.add(');
  i:= Partner.getLineNumberWith('  private ObservableList<String> ' + Name );
  Partner.DeleteBlock(i, i+1);
end;

end.
