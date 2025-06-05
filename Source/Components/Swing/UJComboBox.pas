unit UJComboBox;

interface

uses
  Classes,
  StdCtrls,
  UJComponents;

type

  TJComboBox = class(TSwingComponent)
  private
    FItems: TStrings;
    FSelectedIndex: Integer;
    FMaximumRowCount: Integer;
    FpopupMenuCanceled: string;
    FpopupMenuWillBecomeInvisible: string;
    FpopupMenuWillBecomeVisible: string;
    procedure SetSelectedIndex(AIndex: Integer);
    procedure SetItems(AItems: TStrings);
    procedure MakeList;
  protected
    FEditable: Boolean;
    procedure SetEditable(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AComboBox: TComboBox);
    destructor Destroy; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Items: TStrings read FItems write SetItems;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property MaximumRowCount: Integer read FMaximumRowCount
      write FMaximumRowCount default 8;
    property Editable: Boolean read FEditable write SetEditable;
    property popupMenuCanceled: string read FpopupMenuCanceled
      write FpopupMenuCanceled;
    property popupMenuWillBecomeInvisible: string
      read FpopupMenuWillBecomeInvisible write FpopupMenuWillBecomeInvisible;
    property popupMenuWillBecomeVisible: string read FpopupMenuWillBecomeVisible
      write FpopupMenuWillBecomeVisible;
  end;

implementation

uses
  SysUtils,
  Types,
  Graphics,
  Controls,
  UUtils;

{ --- TJComboBox --------------------------------------------------------------- }

constructor TJComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +9;
  Width := 80;
  Height := 24;
  FItems := TStringList.Create;
  FItems.Text := DefaultItems;
  FSelectedIndex := 0;
  FMaximumRowCount := 8;
  JavaType := 'JComboBox';
end;

constructor TJComboBox.CreateFrom(AComboBox: TComboBox);
begin
  Create(AComboBox.Owner);
  CreateFromJ(AComboBox);
  Font := AComboBox.Font;
  Background := AComboBox.Color;
  Items.AddStrings(AComboBox.Items);
  FSelectedIndex := AComboBox.MaxLength;
end;

function TJComboBox.GetAttributes(ShowAttributes: Integer): string;
const
  List1 = '|SelectedIndex|Items';
  List2 = '|MaximumRowCount|Editable';
begin
  if ShowAttributes = 1 then
    Result := List1
  else
    Result := List1 + List2;
  Result := Result + inherited;
end;

procedure TJComboBox.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else
    inherited;
end;

function TJComboBox.GetEvents(ShowEvents: Integer): string;
const
  CBox1 = '|actionPerformed|itemStateChanged';
  CBox2 = '|popupMenuCanceled|popupMenuWillBecomeInvisible|popupMenuWillBecomeVisible';
begin
  if ShowEvents = 1 then
    Result := CBox1
  else
    Result := CBox1 + CBox2;
  Result := Result + inherited;
end;

procedure TJComboBox.NewControl;
begin
  InsertNewVariable('private JComboBox<String> ' + Name +
    ' = new JComboBox<>();');
  InsertNewVariable('  private DefaultComboBoxModel<String> ' + Name +
    'Model = new DefaultComboBoxModel<>();');
  MakeAttribut('Model', Name + 'Model');
  FPartner.InsertComponent(Indent2 + GetContainerAdd);
  MakeList;
  MakeFont;
end;

procedure TJComboBox.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private JComboBox<String> ' + Name);
  FPartner.DeleteAttribute('private DefaultComboBoxModel<String> ' + Name
    + 'Model');
  FPartner.DeleteAttributeValues(Name + 'Model.addElement');
end;

procedure TJComboBox.Rename(const OldName, NewName, Events: string);

  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FpopupMenuCanceled);
  Rename(FpopupMenuWillBecomeInvisible);
  Rename(FpopupMenuWillBecomeVisible);
  FPartner.ReplaceWord(OldName + 'Model', NewName + 'Model', True);
end;

procedure TJComboBox.MakeList;
var
  Str: string;
begin
  FPartner.DeleteAttributeValues(Name + 'Model.addElement(');
  Str := '';
  for var I := 0 to Items.Count - 1 do
    Str := Str + SurroundFix(Indent1 + Name + 'Model.addElement("' +
      Items[I] + '");');
  FPartner.InsertAttributValue(GetContainerAdd, Str, 0);
  if Items.Count > 0 then
    FSelectedIndex := 0
  else
    FSelectedIndex := -1;
end;

destructor TJComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJComboBox.Paint;
var
  XPos, YPos, Dxy, TextHeight: Integer;
  Str: string;
  Points: array [0 .. 2] of TPoint;
  Rect1: TRect;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  if FEditable then
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(Rect(0, 0, Width - 19, Height));
  end;

  Canvas.MoveTo(Width - 20, 0);
  Canvas.LineTo(Width - 20, Height);
  Dxy := 5;

  Canvas.Pen.Color := DefaultForeground;
  Canvas.Brush.Color := DefaultForeground;
  XPos := Width - 10;
  YPos := Height div 2 + Dxy div 2;
  Points[0] := Point(XPos, YPos);
  Points[1] := Point(XPos - Dxy, YPos - Dxy);
  Points[2] := Point(XPos + Dxy, YPos - Dxy);
  Canvas.Polygon(Points);

  if (FSelectedIndex > -1) and (FSelectedIndex < FItems.Count) then
  begin
    Canvas.Font.Color := Foreground;
    Canvas.Brush.Style := bsClear;
    Str := FItems[FSelectedIndex];
    TextHeight := Canvas.TextHeight(Str);
    Rect1 := Rect(0, 0, Width - 20, Height);
    Canvas.TextRect(Rect1, 3, (Height - TextHeight) div 2, Str);
  end;
end;

procedure TJComboBox.SetItems(AItems: TStrings);
begin
  if AItems.Text <> FItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TJComboBox.SetSelectedIndex(AIndex: Integer);
begin
  if AIndex <> FSelectedIndex then
  begin
    FSelectedIndex := AIndex;
    Invalidate;
  end;
end;

procedure TJComboBox.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

end.
