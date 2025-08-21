unit UAList;

interface

uses
  Classes,
  StdCtrls,
  UAComponents;

type

  TAList = class(TAWTComponent)
  private
    FItems: TStrings;
    FSelectedIndex: Integer;
    FMultipleMode: Boolean; // non visible property
    procedure SetSelectedIndex(AIndex: Integer);
    procedure SetItems(AItems: TStrings);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property MultipleMode: Boolean read FMultipleMode write FMultipleMode
      default False;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Items: TStrings read FItems write SetItems;

    property actionPerformed;
    property itemStateChanged;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Controls;

{ --- TAList ------------------------------------------------------------------- }

constructor TAList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -8;
  Width := 120;
  Height := 80;
  FItems := TStringList.Create;
  FItems.Text := defaultItems;
  FSelectedIndex := -1;
  JavaType := 'List';
end;

function TAList.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|MultipleMode|SelectedIndex|Items' + inherited GetAttributes
    (ShowAttributes);
end;

procedure TAList.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectedIndex' then
    MakeSelectedIndex(Value)
  else
    inherited;
end;

function TAList.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed|itemStateChanged' + inherited GetEvents
    (ShowEvents);
end;

procedure TAList.MakeList;
var
  Str: string;
begin
  FPartner.DeleteAttributeValues(Name + '.add(');
  Str := '';
  for var I := 0 to Items.Count - 1 do
    Str := Str + surroundFix(Indent1 + Name + '.add(' +
      AsString(Items[I]) + ');');
  FPartner.InsertAttributValue(GetContainerAdd, Str, 0);
end;

procedure TAList.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private List ' + Name + ' = new List();');
  MakeList;
end;

destructor TAList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TAList.Paint;
var
  YPos, TextH: Integer;
begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := AWTDarkGray;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  YPos := 2;
  TextH := Canvas.TextHeight('Hg');
  for var I := 0 to FItems.Count - 1 do
  begin
    if I = FSelectedIndex then
    begin
      Canvas.Brush.Color := AWTSelectionColor;
      Canvas.Pen.Color := AWTSelectionColor;
      Canvas.Font.Color := clWhite;
      Canvas.Rectangle(Rect(0, YPos, Width, YPos + TextH + 0));
      Canvas.TextOut(3, YPos + 0, FItems[I]);
      Canvas.Brush.Color := Background;
      Canvas.Pen.Color := Background;
      Canvas.Font.Color := Foreground;
    end
    else
      Canvas.TextOut(3, YPos + 0, FItems[I]);
    Inc(YPos, TextH);
    if YPos + TextH > Height then
    begin
      Scrollbar(Rect(Width - 17, 1, Width - 1, Height - 1), False, False);
      Break;
    end;
  end;
end;

procedure TAList.SetItems(AItems: TStrings);
begin
  if AItems.Text <> FItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TAList.SetSelectedIndex(AIndex: Integer);
begin
  if AIndex <> FSelectedIndex then
  begin
    FSelectedIndex := AIndex;
    Invalidate;
  end;
end;

end.
