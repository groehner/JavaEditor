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
  Classes, UFXLabeled, UFXComponents, UFXPane;

type

  TFXButtonBase = class (TFXLabeled)
  private
    Faction: string;
  public
    function getEvents(ShowEvents: integer): string; override;
    procedure MakeActionEvent;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SizeToText; override;
  published
    property action: string read Faction write Faction;
  end;

  TFXButton = class (TFXButtonBase)
  private
    FCancelButton: boolean;
    FDefaultButton: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure DeleteComponent; override;
    procedure NameFromText; override;
  published
    property CancelButton: boolean read FCancelButton write FCancelButton;
    property DefaultButton: boolean read FDefaultButton write FDefaultButton;
  end;

  TFXToggleButton = class (TFXButtonBase)
  private
    FSelected: boolean;
    FToggleGroup: string; // deprecated
    procedure setSelected(aValue: Boolean);
    procedure MakeToggleGroup(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure DeleteComponent; override;
    procedure NameFromText; override;
  published
    property Selected: boolean read FSelected write setSelected;
    property ToggleGroup: string read FToggleGroup write FToggleGroup;
  end;

  TFXRadioButton = class (TFXToggleButton)
    procedure setText(const aValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure NameFromText; override;
  end;

  TFXCheckBox = class (TFXButtonBase)
  private
    FAllowIndeterminate: boolean;
    FIndeterminate: boolean;
    FSelected: boolean;
    procedure setSelected(aValue: Boolean);
  protected
    procedure setText(const aValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NameFromText; override;
  published
    property AllowIndeterminate: boolean read FAllowIndeterminate write FAllowIndeterminate;
    property Indeterminate: boolean read FIndeterminate write FIndeterminate;
    property Selected: boolean read FSelected write setSelected;
  end;

  TSide = (BOTTOM, LEFT, RIGHT, TOP);

  TFXMenuButton = class(TFXButtonBase)
  private
    FMenuItems: TStrings;
    FPopupSide: TSide;
    procedure setItems(aItems: TStrings);
  public
    MenuItemsOld: TStrings;
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property MenuItems: TStrings read FMenuItems write setItems;
    property PopupSide: TSide read FPopupSide write FPopupSide;
  end;

  TFXSplitMenuButton = class(TFXMenuButton)
  public
    constructor Create (AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  end;

  TFXHyperlink = class (TFXButtonBase)
  private
    FVisited: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure DeleteComponent; override;
  published
    property Visited: boolean read FVisited write FVisited;
  end;

  TFXToggleGroup = class (TFXNode)
  public
    constructor Create (AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure DeleteComponent; override;
  end;

  TFXButtonGroup = class (TFXPane)
  private
    FCheckboxes: boolean;
    FColumns: integer;
    FFrame: boolean;
    FTitle: string;
    FItems: TStrings;
    FOldItems: TStrings;
    FAction: string;
    procedure setColumns(Value: integer);
    procedure setTitle(Value: string);
    procedure setItems(Value: TStrings);
    procedure setCheckboxes(Value: boolean);
    procedure setFrame(Value: boolean);
    procedure MakeButtongroupItems;
    procedure MakeTitle(Title: string);
    function surroundIndent(s: string): string;
    function surroundFix(s: string): string;
    function ItemsInColumn(i: integer): integer;
    function RBName(i: integer): string;
    function FontChanged: boolean;
    procedure DeleteComponentAttributes;
    procedure DeleteComponentValues;
    procedure DeleteSelectedLabelMethod;
    procedure MakeActionEvent;
    procedure MakeSelectedLabelMethod;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteComponent; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure DeleteListener(const event: string); override;
    procedure AddListener(const event: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function MakeEventProcedure(const Event: string): string; override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure SetPositionAndSize; override;
  published
    property Items: TStrings read fItems write setItems; // must stay before columns or label
    property Columns: integer read FColumns write setColumns;
    property Title: string read FTitle write setTitle;
    property Checkboxes: boolean read FCheckboxes write setCheckboxes;
    property Frame: boolean read FFrame write setFrame;

    property action: string read Faction write Faction;
  end;

implementation

uses Windows, Graphics, Controls, SysUtils, UITypes, Vcl.Imaging.jpeg,
     UJava, UGUIDesigner, UUtils, JvGnugettext, UStringRessources, UObjectInspector,
     UConfiguration;

{--- TFXButtonBase ------------------------------------------------------------}

function TFXButtonBase.getEvents(ShowEvents: integer): string;
begin
  Result:= '|action' + inherited getEvents(ShowEvents);
end;

procedure TFXButtonBase.MakeActionEvent;
  var s1, s2: string;
begin
  InsertImport('javafx.event.*');
  Partner.InsertListener(GetContainerAdd, getListener('action'));
  s1:= MakeEventProcedure('action');
  s2:= MakeEventProcedureName('action');
  if not Partner.hasText('public void ' + s2) then
    Partner.InsertProcedure(0, s1);
  FObjectInspector.ELEventInspector.SetByCaption('action', Name + '_Action');
end;

procedure TFXButtonBase.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(Faction);
end;

procedure TFXButtonBase.SizeToText;
begin
  SizeToText(Text, LeftSpace + RightSpace);
end;

{--- TFXButton ----------------------------------------------------------------}

constructor TFXButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +105;
  PrefWidth:= 80;
  PrefHeight:= 24;
  Alignment:= CENTER;
  FText:= 'Button';
  JavaType:= 'Button';
end;

procedure TFXButton.Paint;
begin
  Canvas.Pen.Color:= DarkShadow;
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TWinControl).Brush.Color
    else Canvas.Brush.Color:= Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);
  inherited;
end;

procedure TFXButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Button ' + Name + ' = new Button();');
  MakeAttribut('Text', asString(FText));
  MakeActionEvent;
  MakeFont;
end;

procedure TFXButton.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'Button')
  else begin
    if (Attr = 'CancelButton') or (Attr = 'DefaultButton') then
      Typ:= 'Identifier';
    inherited;
  end;
end;

function TFXButton.getAttributes(ShowAttributes: integer): string;
  const Attributes = '|CancelButton|DefaultButton';
begin
  Result:= Attributes + inherited getAttributes(ShowAttributes);
  delete(Result, Pos('|Background', Result), 11);
end;

procedure TFXButton.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

procedure TFXButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('b' + Text);
end;

{--- TFXToggleButton ----------------------------------------------------------}

constructor TFXToggleButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +136;
  PrefWidth:= 80;
  PrefHeight:= 24;
  Alignment:= CENTER;
  FText:= 'ToggleButton';
  JavaType:= 'ToggleButton';
end;

procedure TFXToggleButton.setSelected(aValue: Boolean);
begin
  if FSelected <> aValue then begin
    FSelected:= aValue;
    Invalidate;
  end;
end;

procedure TFXToggleButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ToggleButton ' + Name + ' = new ToggleButton();');
  MakeAttribut('Text', asString(Text));
  MakeFont;
end;

function TFXToggleButton.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Selected' + inherited getAttributes(ShowAttributes);
  delete(Result, Pos('|Background', Result), 11);
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
  Canvas.Pen.Color:= DarkShadow;
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TWinControl).Brush.Color
    else Canvas.Brush.Color:= Background;
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
  Partner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

procedure TFXToggleButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('b' + Text);
end;

{--- TFXRadioButton -----------------------------------------------------------}

constructor TFXRadioButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +107;
  PrefWidth:= 80;
  PrefHeight:= 24;
  JavaType:= 'RadioButton';
end;

procedure TFXRadioButton.Paint;
  var y, PicNr: integer;
begin
  Canvas.Brush.Color:= Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  LeftSpace:= PPIScale(21);
  inherited;

  case Alignment of
    CENTER_LEFT, CENTER, CENTER_RIGHT:        y:= (Height - PPIScale(17)) div 2;
    BOTTOM_LEFT, BOTTOM_CENTER, BOTTOM_RIGHT: y:= Height - PPIScale(17);
    else                                      y:= 1;
  end;
  if Selected
    then PicNr:= 1
    else PicNr:= 0;
  FGUIDesigner.vilControls21616.Draw(Canvas, 0, y, PicNr);
end;

function TFXRadioButton.getAttributes(ShowAttributes: integer): string;
begin
  Result:= inherited getAttributes(ShowAttributes) + '|Background';
end;

procedure TFXRadioButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('rb' + Text);
end;

procedure TFXRadioButton.setText(const aValue: string);
  var w: integer;
begin
  w:= 17 + Canvas.TextWidth(aValue + '    ');
  if PrefWidth < w then PrefWidth:= w;
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
  MakeAttribut('Text', asString(FText));
  MakeFont;
end;

{--- CheckBox -----------------------------------------------------------------}

constructor TFXCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +106;
  PrefHeight:= 24;
  PrefWidth:= 80;
  Text:= 'Checkbox';
  JavaType:= 'CheckBox';
end;

procedure TFXCheckBox.Paint;
  var y, PicNr: integer;
begin
  Canvas.Brush.Color:= Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  LeftSpace:= PPIScale(21);
  inherited;

  case Alignment of
    CENTER_LEFT, CENTER, CENTER_RIGHT:        y:= (Height - PPIScale(17)) div 2;
    BOTTOM_LEFT, BOTTOM_CENTER, BOTTOM_RIGHT: y:= Height - PPIScale(17);
    else                                      y:= 1;
  end;
  if Selected
    then PicNr:= 3
    else PicNr:= 2;
  FGUIDesigner.vilControls21616.Draw(Canvas, 0, y, PicNr);
end;

procedure TFXCheckBox.setText(const aValue: string);
begin
  if FText <> aValue then begin
    FText:= aValue;
    Invalidate;
  end;
end;

procedure TFXCheckBox.setSelected(aValue: Boolean);
begin
  if FSelected <> aValue then begin
    FSelected:= aValue;
    Invalidate;
  end;
end;

procedure TFXCheckBox.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private CheckBox ' + Name + ' = new CheckBox();');
  MakeAttribut('Text', asString(FText));
  MakeFont;
end;

function TFXCheckBox.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Selected';
        Attributes2 = Attributes1 + '|AllowIndeterminate|Indeterminate';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes);
end;

procedure TFXCheckBox.setAttribute(Attr, Value, Typ: string);
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

{--- TFXMenuButton ------------------------------------------------------------}

constructor TFXMenuButton.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 118;
  PrefWidth:= 80;
  PrefHeight:= 24;
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= 'Copy'#13#10'  Numbered'#13#10' RTF'#13#10'-'#13#10 +
                    'Print'#13#10'Save'#13#10;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'MenuButton';
end;

destructor TFXMenuButton.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

procedure TFXMenuButton.Paint;
begin
  Canvas.Brush.Color:= Background;
  Canvas.Pen.Color:= DefaultBorderColor;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);
  LeftSpace:= PPIScale(8);
  RightSpace:= PPIScale(24);
  TopSpace:= PPIScale(4);
  Bottomspace:= PPIScale(4);
  inherited;
  FGUIDesigner.vilControls21616.Draw(Canvas, Width - PPIScale(19), (Height - PPIScale(16)) div 2, 4);
end;

procedure TFXMenuButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private MenuButton ' + Name + ' = new MenuButton();');
  MakeMenuItems(MenuItemsOld, MenuItems);
  MakeFont;
end;

procedure TFXMenuButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems)
  else if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'MenuButton')
  else
    inherited;
end;

function TFXMenuButton.getAttributes(ShowAttributes: integer): string;
  const show1 = '|MenuItems|Name|Text';
        show2 = '|PopupSide';
begin
  if ShowAttributes = 1
    then Result:= show1
    else Result:= show1 + show2;
  Result:= Result + inherited;
end;

procedure TFXMenuButton.DeleteComponent;
begin
  DeleteMenuItems(MenuItemsOld, MenuItems);
  inherited;
  Partner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

procedure TFXMenuButton.Rename(const OldName, NewName, Events: string);
begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  Partner.Editor.EndUpdate;
end;

procedure TFXMenuButton.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

{--- TFXSplitMenuButton -------------------------------------------------------}

constructor TFXSplitMenuButton.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 119;
  PrefWidth:= 80;
  PrefHeight:= 24;
  JavaType:= 'SplitMenuButton';
end;

procedure TFXSplitMenuButton.Paint;
begin
  inherited;
  Canvas.Pen.Color:= DefaultBorderColor;
  Canvas.MoveTo(Width - PPIScale(24), 0);
  Canvas.LineTo(Width - PPIScale(24), Height);
end;

procedure TFXSplitMenuButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private SplitMenuButton ' + Name + ' = new SplitMenuButton();');
  MakeMenuItems(MenuItemsOld, MenuItems);
  MakeFont;
end;

procedure TFXSplitMenuButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'SplitMenuButton')
  else
    inherited;
end;

{--- TFXHyperlink -------------------------------------------------------------}

constructor TFXHyperlink.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  PrefWidth:= 80;
  PrefHeight:= 24;
  Text:= 'Hyperlink';
  Tag:= +139;
  JavaType:= 'Hyperlink';
end;

procedure TFXHyperlink.Paint;
   var th: integer;
begin
  Canvas.Brush.Color:= Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Font.Color:= RGB(0, 134, 191);
  th:= Canvas.TextHeight(Text);
  Canvas.TextOut(5, (Height - th) div 2, Text);
end;

procedure TFXHyperlink.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Hyperlink ' + Name + ' = new Hyperlink();');
  if Text = 'xxxxx' then Text:= Name;
  MakeAttribut('Text', asString(FText));
  MakeActionEvent;
  MakeFont;
end;

function TFXHyperlink.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Visited' + inherited getAttributes(ShowAttributes);
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
  Partner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

{--- ToggleGroup --------------------------------------------------------------}

constructor TFXToggleGroup.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 108;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'ToggleGroup';
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

function TFXToggleGroup.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

function TFXToggleGroup.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name|';
end;

procedure TFXToggleGroup.DeleteComponent;
  var i: integer;
begin
  inherited;
  if Assigned(Partner) then begin
    for i:= 0 to Owner.ComponentCount - 1 do
      if Owner.Components[i] is TFXRadioButton then
        (Owner.Components[i] as TFXRadioButton).ToggleGroup:= '';
  end;
end;

{--- TFXButtonGroup -----------------------------------------------------------}

constructor TFXButtonGroup.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= 123;
  Width:= 120;
  Height:= 80;
  FColumns:= 1;
  FItems:= TStringList.Create;
  FItems.Text:=  _('America') + ', ' + _('selected') + #13#10 + _('Europe') + #13#10 + _('Asia');
  FOldItems:= TStringList.Create;
  FTitle:= _('Continent');
  FCheckboxes:= false;
  FFrame:= true;
  FAction:= '';
  JavaType:= 'ButtonGroup';
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
  Partner.DeleteAttribute('private Pane ' + Name);
  DeleteSelectedLabelMethod;
  Partner.DeleteAttribute(Name + 'Title');
  Partner.DeleteAttribute(Name + 'Polyline');
  Partner.DeleteAttributeValues(Name + 'Title');
  Partner.DeleteAttributeValues(Name + 'Polyline');
end;

procedure TFXButtonGroup.DeleteComponentAttributes;
begin
  Partner.DeleteAttribute(Name + 'TG');
  for var i:= 0 to FOldItems.Count - 1 do
    Partner.DeleteAttribute(RBName(i));
end;

procedure TFXButtonGroup.DeleteComponentValues;
begin
  for var i:= 0 to FOldItems.Count - 1 do
    Partner.DeleteAttributeValues(RBName(i));
end;

function TFXButtonGroup.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Columns|Items|Title|Checkboxes|Frame|Font|Listener';
  Result:= Result + inherited getAttributes(ShowAttributes);
end;

procedure TFXButtonGroup.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Items') or (Attr = 'Checkboxes') then
    MakeButtonGroupItems
  else if (Attr = 'Frame') or (Attr = 'Title') or
          (Attr = 'Columns') or (Attr = 'Listener') or isFontAttribute(Attr) then
    setPositionAndSize
  else
    inherited;
end;

function TFXButtonGroup.getEvents(ShowEvents: integer): string;
begin
  Result:= '|action|';
end;

procedure TFXButtonGroup.DeleteListener(const event: string);
  var EventMethodName, Listener: string;
begin
  EventMethodName:= MakeEventProcedureName(Event);
  Partner.DeleteMethod(EventMethodName);
  for var i:= 0 to FItems.Count - 1 do begin
    Listener:= RBName(i) + '.setOnAction((event) -> {' + EventMethodName + '(event);});';
    Partner.DeleteLambdaListener(Listener);
  end;
end;

procedure TFXButtonGroup.AddListener(const event: string);
  var EventMethodName, Listener: string;
begin
  EventMethodName:= MakeEventProcedureName(Event);
  for var i:= 0 to FItems.Count - 1 do begin
    Listener:= Indent2 + RBName(i) + '.setOnAction((event) -> {' + EventMethodName + '(event);});';
    Partner.InsertListener(RBName(i) + '.setBounds', Listener);
  end;
  if not Partner.hasText('public void ' + EventMethodName) then
    Partner.InsertProcedure(0, MakeEventProcedure(Event));
end;

function TFXButtonGroup.MakeEventProcedure(const Event: string): string;
begin
  Result:= Indent1 + 'public void ' + Name + '_Action(Event evt) {' + CrLf +
           Indent2 + _(LNGTODO) + CrLf +
           Indent2 + 'System.out.println(((Labeled)evt.getSource()).getText());' + CrLf +
           Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result:= Result + ' // end of ' + Name + '_Action';
  Result:= Result + CrLf + CrLf;
end;

function TFXButtonGroup.RBName(i: integer): string;
begin
  Result:= Name + 'RB' + IntToStr(i);
end;

function TFXButtonGroup.FontChanged: boolean;
begin
  Result:= (Font.Name <> 'Dialog') or (Font.Size <> 12);
end;

procedure TFXButtonGroup.MakeButtongroupItems;
  var i, p: integer; s, s1, nam: string;
begin
  Partner.Editor.BeginUpdate;
  DeleteComponentAttributes;
  DeleteSelectedLabelMethod;
  s:= '';
  if not FCheckboxes then begin
    s:= surroundIndent('  private ToggleGroup ' + Name + 'TG = new ToggleGroup();');
    MakeSelectedLabelMethod;
  end;
  for i:= 0 to FItems.Count - 1 do begin
    nam:= RBName(i);
    s1:= FItems[i];
    p:= Pos(', selected', s1);
    if p > 0
      then s1:= asString(copy(s1, 1, p-1))
      else s1:= asString(s1);
    if FCheckboxes
      then s:= s + surroundIndent('  private CheckBox ' + nam +
               ' = new CheckBox(' + s1 + ');')
      else s:= s + surroundIndent('  private RadioButton ' + nam +
               ' = new RadioButton(' + s1 + ');');
  end;
  Partner.InsertAttribute(FXContainer, s, true);
  setPositionAndSize;
  FOldItems.Text:= FItems.Text;
  Partner.Editor.EndUpdate;
end;

function TFXButtonGroup.surroundIndent(s: string): string;
begin
  Result:= GetIndentation + s + #13#10;
end;

function TFXButtonGroup.surroundFix(s: string): string;
begin
  Result:= Indent1  + s + #13#10;
end;

procedure TFXButtonGroup.MakeTitle(Title: string);
  var xs, ys, wi, he, pl, key: string;
begin
  FTitle:= Title;
  Partner.DeleteAttribute(Name + 'Title');
  Partner.DeleteAttribute(Name + 'Polyline');
  Partner.DeleteAttributeValues(Name + 'Title');
  Partner.DeleteAttributeValues(Name + 'Polyline');
  if FTitle <> '' then begin
    xs:= IntToStr(PPIUnScale(10 + Canvas.TextWidth(FTitle) + 13));
    ys:= IntToStr(PPIUnScale(Canvas.TextHeight('A') div 2));
    InsertNewVariable('  private Label ' + Name + 'Title = new Label("' + FTitle + '");');
    key:= Name + 'Title.resizeRelocate';
    setAttributValue(key, key + '(' + IntToStr(10 + 1) + ', ' + IntToStr(-2) + ', ' +
                          xs + ', 0);');
    if FontChanged then begin
      key:= Name + 'Title.setFont';
      setAttributValue(key, key + '(new Font(' + asString(Font.Name) + ', ' + IntToStr(PPIUnScale(Font.Size)) + '));');
    end;

  end else begin
    xs:= IntToStr(0);
    ys:= IntToStr(0);
  end;
  if FFrame then begin
    wi:= IntToStr(PPIUnScale(Width));
    he:= IntToStr(PPIUnScale(Height));
    pl:= '(' + xs + ', ' + ys + ', ' + wi + ', ' + ys + ', ' + wi + ', ' + he + ', ' +
         '0, ' + he + ', 0, ' + ys + ', 7, ' + ys + ');';
    InsertNewVariable('  private Polyline ' + Name + 'Polyline = new Polyline' + pl);
    key:= Name + 'Polyline.setStyle';
    setAttributValue(key, key + '("-fx-stroke: #BABABA");');
  end;
end;

procedure TFXButtonGroup.setPositionAndSize;
  const circle = 18;
  var col, row, ItemsInCol, line, x, y, dx, dy, th: integer;
      RadioHeight, RadioWidth, ColWidth, RowHeight, ColWidthRest, RowHeightRest: integer;
      xold, yold,ColWidthI, RowHeightI: integer;
      s, s1, nam: string;
begin
  Partner.Editor.BeginUpdate;
  inherited;
  CanvasFontAssign;
  DeleteComponentValues;
  MakeTitle(FTitle);
  s:= '';
  th:= Canvas.TextHeight('Hg');
  dx:= Canvas.TextWidth('x');
  if FTitle = '' then begin
    RadioWidth:= Width;
    RadioHeight:= Height - 4;
    dy:= 2;
  end else begin
    RadioWidth:= Width - 4;
    RadioHeight:= Height - th - 4;
    dy:= 2 + th;
  end;

  if FItems.Count > 0 then begin
    ColWidth:= RadioWidth div FColumns;
    RowHeight:= RadioHeight div ItemsInColumn(1);
    line:= 0;
    xold:= 0;
    ColWidthRest:= RadioWidth mod FColumns;
    for col:= 0 to FColumns - 1 do begin
      if ColWidthRest > 0
        then ColWidthI:= ColWidth + 1
        else ColWidthI:= ColWidth;
      if col = 0
        then x:= dx
        else x:= xold + ColWidthI;
      dec(ColWidthRest);

      yold:= 0;
      ItemsInCol:= ItemsInColumn(col+1);
      RowHeightRest:= RadioHeight mod ItemsInColumn(1);
      for row:= 0 to ItemsInCol - 1 do begin
        if RowHeightRest > 0
          then RowHeightI:= RowHeight + 1
          else RowHeightI:= RowHeight;
        if row = 0
          then y:= dy + (RowHeightI - th) div 2
          else y:= yold + RowHeightI;
        dec(RowHeightRest);
        nam:= RBName(line);
        s:= s + surroundFix('  ' + nam + '.resizeRelocate(' + IntToStr(PPIUnScale(x)) + ', ' + IntToStr(PPIUnScale(y)) + ', ' +
                         IntToStr(PPIUnScale(ColWidthI)) + ', ' + IntToStr(PPIUnScale(RowHeightI)) + ');');
        if not FCheckboxes then
          s:= s + surroundFix('  ' + nam + '.setToggleGroup(' + Name + 'TG);');
        if FAction <> '' then
          s:= s + surroundFix('  ' + nam + '.setOnAction((event) -> {' + Name + '_Action(event);});');
        if Pos(', selected', FItems[line]) > 0 then
          s:= s + surroundFix('  ' + nam + '.setSelected(true);');
        if FontChanged then
          s:= s + surroundFix('  ' + nam + '.setFont(new Font(' + asString(Font.Name) + ', ' + IntToStr(PPIUnScale(Font.Size)) + '));');
        s1:= s1 + ', ' + nam;
        inc(line);
        yold:= y;
      end;
      xold:= x;
    end;
  end;

  delete(s1, 1, 2);
  if FTitle <> '' then
    s1:= s1 + ', ' + Name + 'Title';
  if FFrame then
    s1:= s1 + ', ' + Name + 'Polyline';
  s:= s + surroundFix('  ' + Name + '.getChildren().addAll(' + s1 + ');');
  Partner.InsertAttributValue(Name, s, 1);
  if FAction <> '' then
    MakeActionEvent;
  Partner.Editor.EndUpdate;
end;

procedure TFXButtonGroup.NewControl;
begin
  InsertImport('javafx.scene.shape.Polyline');
  InsertImport('javafx.scene.control.*');
  InsertImport('javafx.scene.text.Font');
  InsertNewVariable('private Pane ' + Name + ' = new Pane();');
  var s:= surroundFix('  ' + FXContainer + '.getChildren().add(' + Name + ');');
  Partner.InsertAttributValue(Name, s, 1);
  MakeButtongroupItems;
  MakeFont;
end;

procedure TFXButtonGroup.setItems(Value: TStrings);
begin
  if FItems.Text <> Value.Text then begin
    FOldItems.Text:= FItems.Text;
    FItems.Text:= Value.Text;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.setColumns(Value: integer);
begin
  if (FColumns <> Value) and (Value > 0) then begin
    FColumns:= Value;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.setTitle(Value: string);
begin
  if FTitle <> Value then begin
    FTitle:= Value;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.setCheckboxes(Value: boolean);
begin
  if FCheckboxes <> Value then begin
    FCheckboxes:= Value;
    Invalidate;
  end;
end;

procedure TFXButtonGroup.setFrame(Value: boolean);
begin
  if FFrame <> Value then begin
    FFrame:= Value;
    Invalidate;
  end;
end;

function TFXButtonGroup.ItemsInColumn(i: integer): integer;
  var quot, rest: integer;
begin
  quot:= FItems.Count div FColumns;
  rest:= FItems.Count mod FColumns;
  if i <= rest
    then Result:= quot + 1
    else Result:= quot;
end;

procedure TFXButtonGroup.Paint;
  var ColumnWidth, RowWidth, RadioHeight, LabelHeight,
      col, row, yc, ItemsInCol, line, x, y, th, tw, p, Radius: integer;
      R: TRect; s: string;
begin
  FOldItems.Text:= FItems.Text;
  inherited;
  CanvasFontAssign;
  Canvas.Brush.Color:= Background;
  Canvas.FillRect(ClientRect);
  th:= Canvas.TextHeight('Hg');
  Radius:= th div 2;
  tw:= Canvas.TextWidth('x');
  LabelHeight:= 0;
  RadioHeight:= Height;
  R:= ClientRect;
  if FFrame then begin
    Canvas.Pen.Color:= $BABABA;
    R.Top:= th div 2;
    Canvas.Rectangle(R);
  end;
  R:= ClientRect;

  if FTitle <> '' then begin
    R.Top:= th div 2;
    LabelHeight:= th;
    RadioHeight:= Height - th;
    Canvas.Rectangle(R);
    Canvas.Textout(10, 0, FTitle);
  end;

  if FItems.Count > 0 then begin
    Canvas.Pen.Color:= $333333;
    ColumnWidth:= Width div FColumns;
    RowWidth:= RadioHeight div ItemsInColumn(1);
    line:= 0;
    for col:= 1 to FColumns do begin
      ItemsInCol:= ItemsInColumn(col);
      for row:= 1 to ItemsInCol do begin
        s:= FItems[line];
        p:= Pos(', selected', s);
        if p > 0 then
          s:= copy(s, 1, p-1);
        x:= tw + (col - 1)*ColumnWidth;
        y:= LabelHeight + 2 + (row - 1)*RowWidth;
        Canvas.Brush.Color:= clWhite;
        yc:= y + RowWidth div 2 - th div 2;
        if FCheckboxes then begin
          R:= Rect(x, yc + 0, x + PPIScale(13), yc + PPIScale(13));
          Canvas.Rectangle(R);
          if p > 0 then begin
            Canvas.Pen.Width:= 2;
            Canvas.MoveTo(x + PPIScale(3), yc + PPIScale(6));
            Canvas.LineTo(x + PPIScale(4), yc + PPIScale(9));
            Canvas.LineTo(x + PPIScale(9), yc + PPIScale(3));
            Canvas.Pen.Width:= 1;
          end;
        end else begin
          yc:= y + RowWidth div 2 - Radius;
          Canvas.Ellipse(x, yc, x + 2*Radius, yc + 2*Radius);
          if p > 0 then begin
            Canvas.Brush.Color:= clBlack;
            Canvas.Ellipse(x+5, yc+5, x + 2*Radius-5, yc + 2*Radius-5);
            Canvas.Brush.Color:= clWhite;
          end;
        end;
        Canvas.Brush.Color:= Background;
        yc:= y + RowWidth div 2 - th div 2;
        R:= Rect(x + 2*Radius + 4, yc, col*ColumnWidth, yc + RowWidth);
        Canvas.TextRect(R, s);
        inc(line);
      end;
    end;
  end;
end;

procedure TFXButtonGroup.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(Faction);
end;

procedure TFXButtonGroup.MakeActionEvent;
  var s1, s2: string;
begin
  InsertImport('javafx.event.*');
  s1:= MakeEventProcedureName('action');
  s2:= Indent1 + 'public void ' + s1 + '(';
  s2:= s2 + EventToEventtype('action');
  s2:= s2 + ' evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf;
  s2:= s2 + Indent2 + 'System.out.println(((Labeled)evt.getSource()).getText());' + CrLf;
  s2:= s2 + Indent1 + '}' + CrLf + CrLf;
  if not Partner.hasText('public void ' + s1) then
    Partner.InsertProcedure(0, s2);
end;

procedure TFXButtonGroup.DeleteSelectedLabelMethod;
begin
  var line:= Partner.getLineNumberWith('public String ' + Name + 'TG_getSelectedButtonGroupLabel');
  if line > -1 then
    Partner.deleteBlock(line, line + 4);
end;

procedure TFXButtonGroup.MakeSelectedLabelMethod;
begin
  var s:=
    surroundFix('public String ' + Name + 'TG_getSelectedButtonGroupLabel() {') +
    surroundFix('  RadioButton rb = (RadioButton)' + Name + 'TG.getSelectedToggle();') +
    surroundFix('  if (rb != null) return rb.getText();') +
    surroundFix('  return "";') +
    surroundFix('}') + CrLf;
  Partner.InsertProcedure(s);
end;

end.

