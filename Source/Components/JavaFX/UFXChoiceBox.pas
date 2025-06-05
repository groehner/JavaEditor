unit UFXChoiceBox;

interface

uses
  Classes,
  UFXComponents;

type

  TFXChoiceBox = class(TFXControl)
  private
    FItems: TStrings;
    FValue: string;
    procedure SetItems(AItems: TStrings);
    procedure SetValue(const AValue: string);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure DeleteComponent; override;
  published
    property Items: TStrings read FItems write SetItems;
    property Value: string read FValue write SetValue;
  end;

implementation

uses
  Graphics,
  Types,
  SysUtils;

{ --- TFXChoiceBox -------------------------------------------------------------- }

constructor TFXChoiceBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +138;
  PrefWidth := 80;
  PrefHeight := 24;
  FItems := TStringList.Create;
  FItems.Text := DefaultItems;
  FValue := FItems[0];
  JavaType := 'ChoiceBox';
end;

destructor TFXChoiceBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TFXChoiceBox.Paint;
var
  XPos, YPos, Dxy, TextWidth: Integer;
  Str: string;
  Points: array [0 .. 2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  if (FValue <> '') and (Pos(FValue, Items.Text) > 0) then
  begin
    Str := FValue;
    TextWidth := Canvas.TextWidth(Str);
    while TextWidth > Width - 20 do
    begin
      Delete(Str, Length(Str), 1);
      TextWidth := Canvas.TextWidth(Str);
    end;
    if Str <> FValue then
      Str := Str + '...';
    Canvas.TextOut(5, (Height - Canvas.TextHeight('A')) div 2, Str);
  end;

  Canvas.Pen.Color := DefaultForeground;
  Canvas.Brush.Color := DefaultForeground;
  Dxy := 5;
  XPos := Width - 10;
  YPos := Height div 2 + Dxy div 2;
  Points[0] := Point(XPos, YPos);
  Points[1] := Point(XPos - Dxy, YPos - Dxy);
  Points[2] := Point(XPos + Dxy, YPos - Dxy);
  Canvas.Polygon(Points);
end;

procedure TFXChoiceBox.SetItems(AItems: TStrings);
begin
  if AItems.Text <> FItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TFXChoiceBox.SetValue(const AValue: string);
begin
  if FValue <> AValue then
  begin
    FValue := AValue;
    Invalidate;
  end;
end;

procedure TFXChoiceBox.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertNewVariable('private ChoiceBox ' + Name + ' = new ChoiceBox();' + CrLf +
    GetIndentation + '  private ObservableList<String> ' + Name +
    'ObservableList = ' + CrLf + GetIndentation +
    '          FXCollections.observableArrayList();');
  DefaultComponent;
  SetAttribute('Value', AsString(FValue), '');
  MakeAttribut('Items', Name + 'ObservableList');
  MakeList;
end;

procedure TFXChoiceBox.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectionMode' then
  begin
    MakeAttribut(Attr, 'SelectionMode.' + Value);
  end
  else if Attr = 'Orientation' then
  begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'Orientation.' + Value);
  end
  else
    inherited;
end;

procedure TFXChoiceBox.MakeList;
begin
  FPartner.DeleteComponent(Name + 'ObservableList.add(');
  for var I := 0 to Items.Count - 1 do
  begin
    var
    Str := Name + 'ObservableList.add("' + Items[I] + '");';
    SetAttributValue('___XXX___', Str);
  end;
end;

function TFXChoiceBox.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Items|Value' + inherited GetAttributes(ShowAttributes);
end;

procedure TFXChoiceBox.DeleteComponent;
var
  Int: Integer;
begin
  inherited;
  FPartner.DeleteComponent(Name + 'ObservableList.add(');
  Int := FPartner.GetLineNumberWith('  private ObservableList<String> ' + Name);
  FPartner.DeleteBlock(Int, Int + 1);
end;

end.
