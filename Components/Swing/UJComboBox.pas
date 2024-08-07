unit UJComboBox;

interface

uses
  Classes, StdCtrls, UJComponents;

type

  TJComboBox = class (TSwingComponent)
  private
    FItems: TStrings;
    FSelectedIndex: integer;
    FMaximumRowCount: integer;
    FpopupMenuCanceled: string;
    FpopupMenuWillBecomeInvisible: string;
    FpopupMenuWillBecomeVisible: string;
    procedure setSelectedIndex(aIndex: integer);
    procedure setItems(aItems: TStrings);
    procedure MakeList;
  protected
    FEditable: boolean;
    procedure setEditable(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aComboBox: TComboBox);
    destructor Destroy; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Items: TStrings read FItems write setItems;
    property SelectedIndex: integer read FSelectedIndex write setSelectedIndex;
    property MaximumRowCount: integer read FMaximumRowCount write FMaximumRowCount default 8;
    property Editable: boolean read FEditable write setEditable;
    property popupMenuCanceled: string read FpopupMenuCanceled write FpopupMenuCanceled;
    property popupMenuWillBecomeInvisible: string read FpopupMenuWillBecomeInvisible write FpopupMenuWillBecomeInvisible;
    property popupMenuWillBecomeVisible: string read FpopupMenuWillBecomeVisible write FpopupMenuWillBecomeVisible;
  end;

implementation

uses SysUtils, Types, Graphics, Controls, UUtils;

{--- TJComboBox ---------------------------------------------------------------}

constructor TJComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +9;
  Width:= 80;
  Height:= 24;
  FItems:= TStringList.Create;
  FItems.Text:= defaultItems;
  FSelectedIndex:= 0;
  FMaximumRowCount:= 8;
  JavaType:= 'JComboBox';
end;

constructor TJComboBox.CreateFrom(aComboBox: TComboBox);
begin
  Create(aComboBox.Owner);
  CreateFromJ(aComboBox);
  Font:= aComboBox.Font;
  Background:= aComboBox.Color;
  Items.AddStrings(aComboBox.Items);
  FSelectedIndex:= aComboBox.maxLength;
end;

function TJComboBox.getAttributes(ShowAttributes: integer): string;
  const
    list1 = '|SelectedIndex|Items';
    list2 = '|MaximumRowCount|Editable';
begin
  if ShowAttributes = 1
    then Result:= list1
    else Result:= list1 + list2;
  Result:= Result + inherited;
end;

procedure TJComboBox.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Items' then
    MakeList
  else
    inherited;
end;

function TJComboBox.getEvents(ShowEvents: integer): string;
  const
    cbox1 = '|actionPerformed|itemStateChanged';
    cbox2 = '|popupMenuCanceled|popupMenuWillBecomeInvisible|popupMenuWillBecomeVisible';
begin
  if ShowEvents = 1
    then Result:= cbox1
    else Result:= cbox1 + cbox2;
  Result:= Result + inherited;
end;

procedure TJComboBox.NewControl;
begin
  InsertNewVariable('private JComboBox<String> ' + Name + ' = new JComboBox<>();');
  InsertNewVariable('  private DefaultComboBoxModel<String> ' + Name + 'Model = new DefaultComboBoxModel<>();');
  MakeAttribut('Model', Name + 'Model');
  Partner.InsertComponent(Indent2 + GetContainerAdd);
  MakeList;
  MakeFont;
end;

procedure TJComboBox.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private JComboBox<String> ' + Name);
  Partner.DeleteAttribute('private DefaultComboBoxModel<String> ' + Name + 'Model');
  Partner.DeleteAttributeValues(Name + 'Model.addElement');
end;

procedure TJComboBox.Rename(const OldName, NewName, Events: string);

  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(FpopupMenuCanceled);
  rename(FpopupMenuWillBecomeInvisible);
  rename(FpopupMenuWillBecomeVisible);
  Partner.ReplaceWord(OldName + 'Model' , NewName + 'Model', true);
end;

procedure TJComboBox.MakeList;
  var i: integer; s: string;
begin
  Partner.DeleteAttributeValues(Name + 'Model.addElement(');
  s:= '';
  for i:= 0 to Items.Count - 1 do
    s:= s + surroundFix(Indent1 + Name + 'Model.addElement("' + Items[i] + '");');
  Partner.InsertAttributValue(getContainerAdd , s, 0);
  if Items.Count > 0
    then FSelectedIndex:= 0
    else FSelectedIndex:= -1;
end;

destructor TJComboBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJComboBox.Paint;
  var x, y, dxy, th: integer; s: string;
      Points: array[0..2] of TPoint; R1: TRect;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  if FEditable then begin
    Canvas.Brush.Color:= clWhite;
    Canvas.Rectangle(Rect(0, 0, Width-19, Height));
  end;

  Canvas.MoveTo(Width - 20, 0);
  Canvas.LineTo(Width - 20, Height);
  dxy:= 5;

  Canvas.Pen.Color:= DefaultForeground;
  Canvas.Brush.Color:= DefaultForeground;
  x:= Width - 10;
  y:= Height div 2 + dxy div 2;
  Points[0]:= Point(x, y);
  Points[1]:= Point(x - dxy, y - dxy);
  Points[2]:= Point(x + dxy, y - dxy);
  Canvas.Polygon(Points);

  if (FSelectedIndex > -1) and (FSelectedIndex < FItems.Count) then begin
    Canvas.Font.Color:= Foreground;
    Canvas.Brush.Style:= bsClear;
    s:= FItems.Strings[FSelectedIndex];
    th:= Canvas.TextHeight(s);
    R1:= Rect(0, 0, Width - 20, Height);
    Canvas.TextRect(R1, 3, (Height - th) div 2, s);
  end;
end;

procedure TJComboBox.setItems(aItems: TStrings);
begin
  if aItems.Text <> FItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TJComboBox.setSelectedIndex(aIndex: integer);
begin
  if aIndex <> FSelectedIndex then begin
    FSelectedIndex:= aIndex;
    Invalidate;
  end;
end;

procedure TJComboBox.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

end.
