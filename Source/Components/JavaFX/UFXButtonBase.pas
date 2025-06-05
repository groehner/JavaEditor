unit UFXButtonBase;

{ classes
  TFXButtonBase = class (TFXLabeled)
  TFXButton
  TFXToggleButton
  TFXRadioButton   // deprecated
  TFXCheckBox
  TFXMenuButton
  TFXSplitMenuButton
  TFXHyperlink
  TFXToggleGroup = class (TFXNode)
  TFXButtonGroup = class (TFXNode)
}

interface

uses
  Classes,
  UFXLabeled,
  UFXComponents,
  UFXPane;

type

  TFXButtonBase = class(TFXLabeled)
  private
    Faction: string;
  public
    function GetEvents(ShowEvents: Integer): string; override;
    procedure MakeActionEvent;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SizeToText; override;
  published
    property action: string read Faction write Faction;
  end;

  TFXButton = class(TFXButtonBase)
  private
    FCancelButton: Boolean;
    FDefaultButton: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure DeleteComponent; override;
    procedure NameFromText; override;
  published
    property CancelButton: Boolean read FCancelButton write FCancelButton;
    property DefaultButton: Boolean read FDefaultButton write FDefaultButton;
  end;

  TFXToggleButton = class(TFXButtonBase)
  private
    FSelected: Boolean;
    FToggleGroup: string; // deprecated
    procedure SetSelected(AValue: Boolean);
    procedure MakeToggleGroup(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure DeleteComponent; override;
    procedure NameFromText; override;
  published
    property Selected: Boolean read FSelected write SetSelected;
    property ToggleGroup: string read FToggleGroup write FToggleGroup;
  end;

  TFXRadioButton = class(TFXToggleButton)
    procedure SetText(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure NameFromText; override;
  end;

  TFXCheckBox = class(TFXButtonBase)
  private
    FAllowIndeterminate: Boolean;
    FIndeterminate: Boolean;
    FSelected: Boolean;
    procedure SetSelected(AValue: Boolean);
  protected
    procedure SetText(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NameFromText; override;
  published
    property AllowIndeterminate: Boolean read FAllowIndeterminate
      write FAllowIndeterminate;
    property Indeterminate: Boolean read FIndeterminate write FIndeterminate;
    property Selected: Boolean read FSelected write SetSelected;
  end;

  TSide = (BOTTOM, LEFT, RIGHT, TOP);

  TFXMenuButton = class(TFXButtonBase)
  private
    FMenuItems: TStrings;
    FPopupSide: TSide;
    FMenuItemsOld: TStrings;
    procedure SetItems(AItems: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property MenuItems: TStrings read FMenuItems write SetItems;
    property PopupSide: TSide read FPopupSide write FPopupSide;
  end;

  TFXSplitMenuButton = class(TFXMenuButton)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  end;

  TFXHyperlink = class(TFXButtonBase)
  private
    FVisited: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure DeleteComponent; override;
  published
    property Visited: Boolean read FVisited write FVisited;
  end;

  TFXToggleGroup = class(TFXNode)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure DeleteComponent; override;
  end;

  TFXButtonGroup = class(TFXPane)
  private
    FCheckboxes: Boolean;
    FColumns: Integer;
    FFrame: Boolean;
    FTitle: string;
    FItems: TStrings;
    FOldItems: TStrings;
    Faction: string;
    procedure SetColumns(Value: Integer);
    procedure SetTitle(Value: string);
    procedure SetItems(Value: TStrings);
    procedure SetCheckboxes(Value: Boolean);
    procedure SetFrame(Value: Boolean);
    procedure MakeButtongroupItems;
    procedure MakeTitle(Title: string);
    function SurroundIndent(Str: string): string;
    function SurroundFix(Str: string): string;
    function ItemsInColumn(Int: Integer): Integer;
    function RBName(Int: Integer): string;
    function FontChanged: Boolean;
    procedure DeleteComponentAttributes;
    procedure DeleteComponentValues;
    procedure DeleteSelectedLabelMethod;
    procedure MakeActionEvent;
    procedure MakeSelectedLabelMethod;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteComponent; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure DeleteListener(const Event: string); override;
    procedure AddListener(const Event: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function MakeEventProcedure(const Event: string): string; override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure SetPositionAndSize; override;
  published
    property Items: TStrings read FItems write SetItems;
    // must stay before columns or label
    property Columns: Integer read FColumns write SetColumns;
    property Title: string read FTitle write SetTitle;
    property Checkboxes: Boolean read FCheckboxes write SetCheckboxes;
    property Frame: Boolean read FFrame write SetFrame;

    property action: string read Faction write Faction;
  end;

implementation

uses
  Windows,
  Graphics,
  Controls,
  SysUtils,
  UITypes,
  JvGnugettext,
  UJava,
  UGUIDesigner,
  UUtils,
  UStringRessources,
  UObjectInspector,
  UConfiguration;

{ --- TFXButtonBase ------------------------------------------------------------ }

function TFXButtonBase.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|action' + inherited GetEvents(ShowEvents);
end;

procedure TFXButtonBase.MakeActionEvent;
var
  Str1, Str2: string;
begin
  InsertImport('javafx.event.*');
  FPartner.InsertListener(GetContainerAdd, GetListener('action'));
  Str1 := MakeEventProcedure('action');
  Str2 := MakeEventProcedureName('action');
  if not FPartner.HasText('public void ' + Str2) then
    FPartner.InsertProcedure(0, Str1);
  FObjectInspector.ELEventInspector.SetByCaption('action', Name + '_Action');
end;

procedure TFXButtonBase.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(Faction);
end;

procedure TFXButtonBase.SizeToText;
begin
  SizeToText(Text, FLeftSpace + FRightSpace);
end;

{ --- TFXButton ---------------------------------------------------------------- }

constructor TFXButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +105;
  PrefWidth := 80;
  PrefHeight := 24;
  Alignment := CENTER;
  FText := 'Button';
  JavaType := 'Button';
end;

procedure TFXButton.Paint;
begin
  Canvas.Pen.Color := DarkShadow;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);
  inherited;
end;

procedure TFXButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Button ' + Name + ' = new Button();');
  MakeAttribut('Text', AsString(FText));
  MakeActionEvent;
  MakeFont;
end;

procedure TFXButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'Button')
  else
  begin
    if (Attr = 'CancelButton') or (Attr = 'DefaultButton') then
      Typ := 'Identifier';
    inherited;
  end;
end;

function TFXButton.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes = '|CancelButton|DefaultButton';
begin
  Result := Attributes + inherited GetAttributes(ShowAttributes);
  Delete(Result, Pos('|Background', Result), 11);
end;

procedure TFXButton.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

procedure TFXButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('b' + Text);
end;

{ --- TFXToggleButton ---------------------------------------------------------- }

constructor TFXToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +136;
  PrefWidth := 80;
  PrefHeight := 24;
  Alignment := CENTER;
  FText := 'ToggleButton';
  JavaType := 'ToggleButton';
end;

procedure TFXToggleButton.SetSelected(AValue: Boolean);
begin
  if FSelected <> AValue then
  begin
    FSelected := AValue;
    Invalidate;
  end;
end;

procedure TFXToggleButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ToggleButton ' + Name + ' = new ToggleButton();');
  MakeAttribut('Text', AsString(Text));
  MakeFont;
end;

function TFXToggleButton.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Selected' + inherited GetAttributes(ShowAttributes);
  Delete(Result, Pos('|Background', Result), 11);
end;

procedure TFXToggleButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'ToggleButton')
  else if Attr = 'ToggleGroup' then
    MakeToggleGroup(Value)
  else
    inherited;
end;

procedure TFXToggleButton.Paint;
begin
  Canvas.Pen.Color := DarkShadow;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);

  inherited;
end;

procedure TFXToggleButton.MakeToggleGroup(Value: string);
begin
  MakeAttribut('ToggleGroup', Value);
end;

procedure TFXToggleButton.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

procedure TFXToggleButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('b' + Text);
end;

{ --- TFXRadioButton ----------------------------------------------------------- }

constructor TFXRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +107;
  PrefWidth := 80;
  PrefHeight := 24;
  JavaType := 'RadioButton';
end;

procedure TFXRadioButton.Paint;
var
  YPos, PicNr: Integer;
begin
  Canvas.Brush.Color := Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  FLeftSpace := PPIScale(21);
  inherited;

  case Alignment of
    CENTER_LEFT, CENTER, CENTER_RIGHT:
      YPos := (Height - PPIScale(17)) div 2;
    BOTTOM_LEFT, BOTTOM_CENTER, BOTTOM_RIGHT:
      YPos := Height - PPIScale(17);
  else
    YPos := 1;
  end;
  if Selected then
    PicNr := 1
  else
    PicNr := 0;
  FGUIDesigner.vilControls21616.Draw(Canvas, 0, YPos, PicNr);
end;

function TFXRadioButton.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := inherited GetAttributes(ShowAttributes) + '|Background';
end;

procedure TFXRadioButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('rb' + Text);
end;

procedure TFXRadioButton.SetText(const AValue: string);
var
  Width: Integer;
begin
  Width := 17 + Canvas.TextWidth(AValue + '    ');
  if PrefWidth < Width then
    PrefWidth := Width;
  inherited;
end;

procedure TFXRadioButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'RadioButton')
  else
    inherited;
end;

procedure TFXRadioButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private RadioButton ' + Name + ' = new RadioButton();');
  MakeAttribut('Text', AsString(FText));
  MakeFont;
end;

{ --- CheckBox ----------------------------------------------------------------- }

constructor TFXCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +106;
  PrefHeight := 24;
  PrefWidth := 80;
  Text := 'Checkbox';
  JavaType := 'CheckBox';
end;

procedure TFXCheckBox.Paint;
var
  YPos, PicNr: Integer;
begin
  Canvas.Brush.Color := Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  FLeftSpace := PPIScale(21);
  inherited;

  case Alignment of
    CENTER_LEFT, CENTER, CENTER_RIGHT:
      YPos := (Height - PPIScale(17)) div 2;
    BOTTOM_LEFT, BOTTOM_CENTER, BOTTOM_RIGHT:
      YPos := Height - PPIScale(17);
  else
    YPos := 1;
  end;
  if Selected then
    PicNr := 3
  else
    PicNr := 2;
  FGUIDesigner.vilControls21616.Draw(Canvas, 0, YPos, PicNr);
end;

procedure TFXCheckBox.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

procedure TFXCheckBox.SetSelected(AValue: Boolean);
begin
  if FSelected <> AValue then
  begin
    FSelected := AValue;
    Invalidate;
  end;
end;

procedure TFXCheckBox.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private CheckBox ' + Name + ' = new CheckBox();');
  MakeAttribut('Text', AsString(FText));
  MakeFont;
end;

function TFXCheckBox.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Selected';
  Attributes2 = Attributes1 + '|AllowIndeterminate|Indeterminate';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
end;

procedure TFXCheckBox.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'CheckBox')
  else
    inherited;
end;

procedure TFXCheckBox.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('cb' + Text);
end;

{ --- TFXMenuButton ------------------------------------------------------------ }

constructor TFXMenuButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 118;
  PrefWidth := 80;
  PrefHeight := 24;
  FMenuItems := TStringList.Create;
  FMenuItems.Text := 'Copy'#13#10'  Numbered'#13#10' RTF'#13#10'-'#13#10 +
    'Print'#13#10'Save'#13#10;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'MenuButton';
end;

destructor TFXMenuButton.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

procedure TFXMenuButton.Paint;
begin
  Canvas.Brush.Color := Background;
  Canvas.Pen.Color := DefaultBorderColor;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);
  FLeftSpace := PPIScale(8);
  FRightSpace := PPIScale(24);
  FTopSpace := PPIScale(4);
  FBottomspace := PPIScale(4);
  inherited;
  FGUIDesigner.vilControls21616.Draw(Canvas, Width - PPIScale(19),
    (Height - PPIScale(16)) div 2, 4);
end;

procedure TFXMenuButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private MenuButton ' + Name + ' = new MenuButton();');
  MakeMenuItems(FMenuItemsOld, MenuItems);
  MakeFont;
end;

procedure TFXMenuButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems)
  else if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'MenuButton')
  else
    inherited;
end;

function TFXMenuButton.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|MenuItems|Name|Text';
  Show2 = '|PopupSide';
begin
  if ShowAttributes = 1 then
    Result := Show1
  else
    Result := Show1 + Show2;
  Result := Result + inherited;
end;

procedure TFXMenuButton.DeleteComponent;
begin
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  inherited;
  FPartner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

procedure TFXMenuButton.Rename(const OldName, NewName, Events: string);
begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  FPartner.Editor.EndUpdate;
end;

procedure TFXMenuButton.SetItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

{ --- TFXSplitMenuButton ------------------------------------------------------- }

constructor TFXSplitMenuButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 119;
  PrefWidth := 80;
  PrefHeight := 24;
  JavaType := 'SplitMenuButton';
end;

procedure TFXSplitMenuButton.Paint;
begin
  inherited;
  Canvas.Pen.Color := DefaultBorderColor;
  Canvas.MoveTo(Width - PPIScale(24), 0);
  Canvas.LineTo(Width - PPIScale(24), Height);
end;

procedure TFXSplitMenuButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private SplitMenuButton ' + Name +
    ' = new SplitMenuButton();');
  MakeMenuItems(FMenuItemsOld, MenuItems);
  MakeFont;
end;

procedure TFXSplitMenuButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'SplitMenuButton')
  else
    inherited;
end;

{ --- TFXHyperlink ------------------------------------------------------------- }

constructor TFXHyperlink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PrefWidth := 80;
  PrefHeight := 24;
  Text := 'Hyperlink';
  Tag := +139;
  JavaType := 'Hyperlink';
end;

procedure TFXHyperlink.Paint;
var
  TextHeight: Integer;
begin
  Canvas.Brush.Color := Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Font.Color := RGB(0, 134, 191);
  TextHeight := Canvas.TextHeight(Text);
  Canvas.TextOut(5, (Height - TextHeight) div 2, Text);
end;

procedure TFXHyperlink.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Hyperlink ' + Name + ' = new Hyperlink();');
  if Text = 'xxxxx' then
    Text := Name;
  MakeAttribut('Text', AsString(FText));
  MakeActionEvent;
  MakeFont;
end;

function TFXHyperlink.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Visited' + inherited GetAttributes(ShowAttributes);
end;

procedure TFXHyperlink.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'Hyperlink')
  else
    inherited;
end;

procedure TFXHyperlink.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

{ --- ToggleGroup -------------------------------------------------------------- }

constructor TFXToggleGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 108;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'ToggleGroup';
end;

procedure TFXToggleGroup.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 6, 4, 7);
end;

procedure TFXToggleGroup.NewControl;
begin
  InsertNewVariable('private ToggleGroup ' + Name + ' = new ToggleGroup();');
end;

function TFXToggleGroup.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

function TFXToggleGroup.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name|';
end;

procedure TFXToggleGroup.DeleteComponent;
begin
  inherited;
  if Assigned(FPartner) then
  begin
    for var I := 0 to Owner.ComponentCount - 1 do
      if Owner.Components[I] is TFXRadioButton then
        (Owner.Components[I] as TFXRadioButton).ToggleGroup := '';
  end;
end;

{ --- TFXButtonGroup ----------------------------------------------------------- }

constructor TFXButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 123;
  Width := 120;
  Height := 80;
  FColumns := 1;
  FItems := TStringList.Create;
  FItems.Text := _('America') + ', ' + _('selected') + #13#10 + _('Europe') +
    #13#10 + _('Asia');
  FOldItems := TStringList.Create;
  FTitle := _('Continent');
  FCheckboxes := False;
  FFrame := True;
  Faction := '';
  JavaType := 'ButtonGroup';
end;

destructor TFXButtonGroup.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FOldItems);
  inherited;
end;

procedure TFXButtonGroup.DeleteComponent;
begin
  inherited;
  DeleteComponentAttributes;
  DeleteComponentValues;
  FPartner.DeleteAttribute('private Pane ' + Name);
  DeleteSelectedLabelMethod;
  FPartner.DeleteAttribute(Name + 'Title');
  FPartner.DeleteAttribute(Name + 'Polyline');
  FPartner.DeleteAttributeValues(Name + 'Title');
  FPartner.DeleteAttributeValues(Name + 'Polyline');
end;

procedure TFXButtonGroup.DeleteComponentAttributes;
begin
  FPartner.DeleteAttribute(Name + 'TG');
  for var I := 0 to FOldItems.Count - 1 do
    FPartner.DeleteAttribute(RBName(I));
end;

procedure TFXButtonGroup.DeleteComponentValues;
begin
  for var I := 0 to FOldItems.Count - 1 do
    FPartner.DeleteAttributeValues(RBName(I));
end;

function TFXButtonGroup.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Columns|Items|Title|Checkboxes|Frame|Font|Listener';
  Result := Result + inherited GetAttributes(ShowAttributes);
end;

procedure TFXButtonGroup.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Items') or (Attr = 'Checkboxes') then
    MakeButtongroupItems
  else if (Attr = 'Frame') or (Attr = 'Title') or (Attr = 'Columns') or
    (Attr = 'Listener') or IsFontAttribute(Attr) then
    SetPositionAndSize
  else
    inherited;
end;

function TFXButtonGroup.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|action|';
end;

procedure TFXButtonGroup.DeleteListener(const Event: string);
var
  EventMethodName, Listener: string;
begin
  EventMethodName := MakeEventProcedureName(Event);
  FPartner.DeleteMethod(EventMethodName);
  for var I := 0 to FItems.Count - 1 do
  begin
    Listener := RBName(I) + '.setOnAction((event) -> {' + EventMethodName +
      '(event);});';
    FPartner.DeleteLambdaListener(Listener);
  end;
end;

procedure TFXButtonGroup.AddListener(const Event: string);
var
  EventMethodName, Listener: string;
begin
  EventMethodName := MakeEventProcedureName(Event);
  for var I := 0 to FItems.Count - 1 do
  begin
    Listener := Indent2 + RBName(I) + '.setOnAction((event) -> {' +
      EventMethodName + '(event);});';
    FPartner.InsertListener(RBName(I) + '.setBounds', Listener);
  end;
  if not FPartner.HasText('public void ' + EventMethodName) then
    FPartner.InsertProcedure(0, MakeEventProcedure(Event));
end;

function TFXButtonGroup.MakeEventProcedure(const Event: string): string;
begin
  Result := Indent1 + 'public void ' + Name + '_Action(Event evt) {' + CrLf +
    Indent2 + _(LNGTODO) + CrLf + Indent2 +
    'System.out.println(((Labeled)evt.getSource()).getText());' + CrLf +
    Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result := Result + ' // end of ' + Name + '_Action';
  Result := Result + CrLf + CrLf;
end;

function TFXButtonGroup.RBName(Int: Integer): string;
begin
  Result := Name + 'RB' + IntToStr(Int);
end;

function TFXButtonGroup.FontChanged: Boolean;
begin
  Result := (Font.Name <> 'Dialog') or (Font.Size <> 12);
end;

procedure TFXButtonGroup.MakeButtongroupItems;
var
  Posi: Integer;
  Str, Str1, Nam: string;
begin
  FPartner.Editor.BeginUpdate;
  DeleteComponentAttributes;
  DeleteSelectedLabelMethod;
  Str := '';
  if not FCheckboxes then
  begin
    Str := SurroundIndent('  private ToggleGroup ' + Name +
      'TG = new ToggleGroup();');
    MakeSelectedLabelMethod;
  end;
  for var I := 0 to FItems.Count - 1 do
  begin
    Nam := RBName(I);
    Str1 := FItems[I];
    Posi := Pos(', ' + _('selected'), Str1);
    if Posi > 0 then
      Str1 := AsString(Copy(Str1, 1, Posi - 1))
    else
      Str1 := AsString(Str1);
    if FCheckboxes then
      Str := Str + SurroundIndent('  private CheckBox ' + Nam +
        ' = new CheckBox(' + Str1 + ');')
    else
      Str := Str + SurroundIndent('  private RadioButton ' + Nam +
        ' = new RadioButton(' + Str1 + ');');
  end;
  FPartner.InsertAttribute(FXContainer, Str, True);
  SetPositionAndSize;
  FOldItems.Text := FItems.Text;
  FPartner.Editor.EndUpdate;
end;

function TFXButtonGroup.SurroundIndent(Str: string): string;
begin
  Result := GetIndentation + Str + #13#10;
end;

function TFXButtonGroup.SurroundFix(Str: string): string;
begin
  Result := Indent1 + Str + #13#10;
end;

procedure TFXButtonGroup.MakeTitle(Title: string);
var
  XSPos, YSPos, Wid, Hei, Poly, Key: string;
begin
  FTitle := Title;
  FPartner.DeleteAttribute(Name + 'Title');
  FPartner.DeleteAttribute(Name + 'Polyline');
  FPartner.DeleteAttributeValues(Name + 'Title');
  FPartner.DeleteAttributeValues(Name + 'Polyline');
  if FTitle <> '' then
  begin
    XSPos := IntToStr(PPIUnScale(10 + Canvas.TextWidth(FTitle) + 13));
    YSPos := IntToStr(PPIUnScale(Canvas.TextHeight('A') div 2));
    InsertNewVariable('  private Label ' + Name + 'Title = new Label("' +
      FTitle + '");');
    Key := Name + 'Title.resizeRelocate';
    SetAttributValue(Key, Key + '(' + IntToStr(10 + 1) + ', ' + IntToStr(-2) +
      ', ' + XSPos + ', 0);');
    if FontChanged then
    begin
      Key := Name + 'Title.setFont';
      SetAttributValue(Key, Key + '(new Font(' + AsString(Font.Name) + ', ' +
        IntToStr(PPIUnScale(Font.Size)) + '));');
    end;

  end
  else
  begin
    XSPos := IntToStr(0);
    YSPos := IntToStr(0);
  end;
  if FFrame then
  begin
    Wid := IntToStr(PPIUnScale(Width));
    Hei := IntToStr(PPIUnScale(Height));
    Poly := '(' + XSPos + ', ' + YSPos + ', ' + Wid + ', ' + YSPos + ', ' + Wid
      + ', ' + Hei + ', ' + '0, ' + Hei + ', 0, ' + YSPos + ', 7, ' +
      YSPos + ');';
    InsertNewVariable('  private Polyline ' + Name +
      'Polyline = new Polyline' + Poly);
    Key := Name + 'Polyline.setStyle';
    SetAttributValue(Key, Key + '("-fx-stroke: #BABABA");');
  end;
end;

procedure TFXButtonGroup.SetPositionAndSize;
const
  CCircle = 18;
var
  Col, Row, ItemsInCol, Line, XPos, YPos, DeltaX, DeltaY, TextHeight: Integer;
  RadioHeight, RadioWidth, ColWidth, RowHeight, ColWidthRest,
    RowHeightRest: Integer;
  XOld, YOld, ColWidthI, RowHeightI: Integer;
  Str, Str1, Nam: string;
begin
  FPartner.Editor.BeginUpdate;
  inherited;
  CanvasFontAssign;
  DeleteComponentValues;
  MakeTitle(FTitle);
  Str := '';
  TextHeight := Canvas.TextHeight('Hg');
  DeltaX := Canvas.TextWidth('x');
  if FTitle = '' then
  begin
    RadioWidth := Width;
    RadioHeight := Height - 4;
    DeltaY := 2;
  end
  else
  begin
    RadioWidth := Width - 4;
    RadioHeight := Height - TextHeight - 4;
    DeltaY := 2 + TextHeight;
  end;

  if FItems.Count > 0 then
  begin
    ColWidth := RadioWidth div FColumns;
    RowHeight := RadioHeight div ItemsInColumn(1);
    Line := 0;
    XOld := 0;
    ColWidthRest := RadioWidth mod FColumns;
    for Col := 0 to FColumns - 1 do
    begin
      if ColWidthRest > 0 then
        ColWidthI := ColWidth + 1
      else
        ColWidthI := ColWidth;
      if Col = 0 then
        XPos := DeltaX
      else
        XPos := XOld + ColWidthI;
      Dec(ColWidthRest);

      YOld := 0;
      ItemsInCol := ItemsInColumn(Col + 1);
      RowHeightRest := RadioHeight mod ItemsInColumn(1);
      for Row := 0 to ItemsInCol - 1 do
      begin
        if RowHeightRest > 0 then
          RowHeightI := RowHeight + 1
        else
          RowHeightI := RowHeight;
        if Row = 0 then
          YPos := DeltaY + (RowHeightI - TextHeight) div 2
        else
          YPos := YOld + RowHeightI;
        Dec(RowHeightRest);
        Nam := RBName(Line);
        Str := Str + SurroundFix('  ' + Nam + '.resizeRelocate(' +
          IntToStr(PPIUnScale(XPos)) + ', ' + IntToStr(PPIUnScale(YPos)) + ', '
          + IntToStr(PPIUnScale(ColWidthI)) + ', ' +
          IntToStr(PPIUnScale(RowHeightI)) + ');');
        if not FCheckboxes then
          Str := Str + SurroundFix('  ' + Nam + '.setToggleGroup(' + Name
            + 'TG);');
        if Faction <> '' then
          Str := Str + SurroundFix('  ' + Nam + '.setOnAction((event) -> {' +
            Name + '_Action(event);});');
        if Pos(', ' + _('selected'), FItems[Line]) > 0 then
          Str := Str + SurroundFix('  ' + Nam + '.SetSelected(true);');
        if FontChanged then
          Str := Str + SurroundFix('  ' + Nam + '.setFont(new Font(' +
            AsString(Font.Name) + ', ' + IntToStr(PPIUnScale(Font.Size)
            ) + '));');
        Str1 := Str1 + ', ' + Nam;
        Inc(Line);
        YOld := YPos;
      end;
      XOld := XPos;
    end;
  end;

  Delete(Str1, 1, 2);
  if FTitle <> '' then
    Str1 := Str1 + ', ' + Name + 'Title';
  if FFrame then
    Str1 := Str1 + ', ' + Name + 'Polyline';
  Str := Str + SurroundFix('  ' + Name + '.getChildren().addAll(' +
    Str1 + ');');
  FPartner.InsertAttributValue(Name, Str, 1);
  if Faction <> '' then
    MakeActionEvent;
  FPartner.Editor.EndUpdate;
end;

procedure TFXButtonGroup.NewControl;
begin
  InsertImport('javafx.scene.shape.Polyline');
  InsertImport('javafx.scene.control.*');
  InsertImport('javafx.scene.text.Font');
  InsertNewVariable('private Pane ' + Name + ' = new Pane();');
  var
  Str := SurroundFix('  ' + FXContainer + '.getChildren().add(' + Name + ');');
  FPartner.InsertAttributValue(Name, Str, 1);
  MakeButtongroupItems;
  MakeFont;
end;

procedure TFXButtonGroup.SetItems(Value: TStrings);
begin
  if FItems.Text <> Value.Text then
  begin
    FOldItems.Text := FItems.Text;
    FItems.Text := Value.Text;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.SetColumns(Value: Integer);
begin
  if (FColumns <> Value) and (Value > 0) then
  begin
    FColumns := Value;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.SetTitle(Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.SetCheckboxes(Value: Boolean);
begin
  if FCheckboxes <> Value then
  begin
    FCheckboxes := Value;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.SetFrame(Value: Boolean);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    Invalidate;
  end;
end;

function TFXButtonGroup.ItemsInColumn(Int: Integer): Integer;
var
  Quot, Rest: Integer;
begin
  Quot := FItems.Count div FColumns;
  Rest := FItems.Count mod FColumns;
  if Int <= Rest then
    Result := Quot + 1
  else
    Result := Quot;
end;

procedure TFXButtonGroup.Paint;
var
  ColumnWidth, RowWidth, RadioHeight, LabelHeight, Col, Row, YPosC, ItemsInCol,
    Line, XPos, YPos, TextHeight, TextWidth, Posi, Radius: Integer;
  Rect1: TRect;
  Str: string;
begin
  FOldItems.Text := FItems.Text;
  inherited;
  CanvasFontAssign;
  Canvas.Brush.Color := Background;
  Canvas.FillRect(ClientRect);
  TextHeight := Canvas.TextHeight('Hg');
  Radius := TextHeight div 2;
  TextWidth := Canvas.TextWidth('x');
  LabelHeight := 0;
  RadioHeight := Height;
  Rect1 := ClientRect;
  if FFrame then
  begin
    Canvas.Pen.Color := $BABABA;
    Rect1.Top := TextHeight div 2;
    Canvas.Rectangle(Rect1);
  end;
  Rect1 := ClientRect;

  if FTitle <> '' then
  begin
    Rect1.Top := TextHeight div 2;
    LabelHeight := TextHeight;
    RadioHeight := Height - TextHeight;
    Canvas.Rectangle(Rect1);
    Canvas.TextOut(10, 0, FTitle);
  end;

  if FItems.Count > 0 then
  begin
    Canvas.Pen.Color := $333333;
    ColumnWidth := Width div FColumns;
    RowWidth := RadioHeight div ItemsInColumn(1);
    Line := 0;
    for Col := 1 to FColumns do
    begin
      ItemsInCol := ItemsInColumn(Col);
      for Row := 1 to ItemsInCol do
      begin
        Str := FItems[Line];
        Posi := Pos(', ' + _('selected'), Str);
        if Posi > 0 then
          Str := Copy(Str, 1, Posi - 1);
        XPos := TextWidth + (Col - 1) * ColumnWidth;
        YPos := LabelHeight + 2 + (Row - 1) * RowWidth;
        Canvas.Brush.Color := clWhite;
        YPosC := YPos + RowWidth div 2 - TextHeight div 2;
        if FCheckboxes then
        begin
          Rect1 := Rect(XPos, YPosC + 0, XPos + PPIScale(13),
            YPosC + PPIScale(13));
          Canvas.Rectangle(Rect1);
          if Posi > 0 then
          begin
            Canvas.Pen.Width := 2;
            Canvas.MoveTo(XPos + PPIScale(3), YPosC + PPIScale(6));
            Canvas.LineTo(XPos + PPIScale(4), YPosC + PPIScale(9));
            Canvas.LineTo(XPos + PPIScale(9), YPosC + PPIScale(3));
            Canvas.Pen.Width := 1;
          end;
        end
        else
        begin
          YPosC := YPos + RowWidth div 2 - Radius;
          Canvas.Ellipse(XPos, YPosC, XPos + 2 * Radius, YPosC + 2 * Radius);
          if Posi > 0 then
          begin
            Canvas.Brush.Color := clBlack;
            Canvas.Ellipse(XPos + 5, YPosC + 5, XPos + 2 * Radius - 5,
              YPosC + 2 * Radius - 5);
            Canvas.Brush.Color := clWhite;
          end;
        end;
        Canvas.Brush.Color := Background;
        YPosC := YPos + RowWidth div 2 - TextHeight div 2;
        Rect1 := Rect(XPos + 2 * Radius + 4, YPosC, Col * ColumnWidth,
          YPosC + RowWidth);
        Canvas.TextRect(Rect1, Str);
        Inc(Line);
      end;
    end;
  end;
end;

procedure TFXButtonGroup.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(Faction);
end;

procedure TFXButtonGroup.MakeActionEvent;
var
  Str1, Str2: string;
begin
  InsertImport('javafx.event.*');
  Str1 := MakeEventProcedureName('action');
  Str2 := Indent1 + 'public void ' + Str1 + '(';
  Str2 := Str2 + EventToEventtype('action');
  Str2 := Str2 + ' evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf;
  Str2 := Str2 + Indent2 +
    'System.out.println(((Labeled)evt.getSource()).getText());' + CrLf;
  Str2 := Str2 + Indent1 + '}' + CrLf + CrLf;
  if not FPartner.HasText('public void ' + Str1) then
    FPartner.InsertProcedure(0, Str2);
end;

procedure TFXButtonGroup.DeleteSelectedLabelMethod;
begin
  var
  Line := FPartner.GetLineNumberWith('public String ' + Name +
    'TG_getSelectedButtonGroupLabel');
  if Line > -1 then
    FPartner.DeleteBlock(Line, Line + 4);
end;

procedure TFXButtonGroup.MakeSelectedLabelMethod;
begin
  var
  Str := SurroundFix('public String ' + Name +
    'TG_getSelectedButtonGroupLabel() {') +
    SurroundFix('  RadioButton rb = (RadioButton)' + Name +
    'TG.getSelectedToggle();') +
    SurroundFix('  if (rb != null) return rb.getText();') +
    SurroundFix('  return "";') + SurroundFix('}') + CrLf;
  FPartner.InsertProcedure(Str);
end;

end.
