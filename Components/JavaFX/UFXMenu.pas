unit UFXMenu;

{ Classes
    TFXMenuBar = class (TFXControl)
    TFXMenuItem = class (TFXNode)
      TFXMenu
    TFXContextMenu = class (TFXNode)
}

interface

uses
  Classes, UFXComponents;

type

  TFXMenuBar = class (TFXControl)
  private
    FUseSystemMenuBar: boolean;
  public
    constructor Create (AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure SetPositionAndSize; override;
  published
    property UseSystemMenuBar: boolean read FUseSystemMenuBar write FUseSystemMenuBar;
  end;

  TFXMenuBarWithMenus = class (TFXControl)
  private
    FMenuItems: TStrings;
  protected
    procedure setItems(aItems: TStrings);
  public
    MenuItemsOld: TStrings;
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  published
    property MenuItems: TStrings read FMenuItems write setItems;
  end;

  TFXMenuItem = class (TFXNode)
  private
    FDisable: boolean;
    FGraphic: string;
    FId: string;
    FMnemonicParsing: boolean;
    FParentMenu: string;
    FParentPopup: string;
    FText: string;
    FVisible: boolean;
    FSeparator: integer;

    FMenuBar: string;
    Faction: string;
    FmenuValidation: string;
  public
    constructor Create (AOwner: TComponent); override;
    procedure InsertMenuItem(const Title, IndentStr: string);
    function getMenuItemName(const SubMenu, s: string): string;
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
  published
    property Disable: boolean read FDisable write FDisable;
    property Text: string read FText write FText;
    property Id: string read FId write FId;
    property ParentMenu: string read FParentMenu write FParentMenu;
    property ParentPopup: string read FParentPopup write FParentPopup;
    property Graphic: string read FGraphic write FGraphic;
    property MnemonicParsing: boolean read FMnemonicParsing write FMnemonicParsing;
    property Visible: boolean read FVisible write FVisible;
    property MenuBar: string read FMenuBar write FMenuBar;
    property Separator: integer read FSeparator write FSeparator;

    property action: string read FAction write FAction;
    property menuValidation: string read FmenuValidation write FmenuValidation;
  end;

  TFXMenu = class(TFXMenuItem)
  private
    FMenuItems: TStrings;
    Fhidden: string;
    Fhiding: string;
    Fshowing: string;
    Fshown: string;
    procedure setItems(aItems: TStrings);
  public
    MenuItemsOld: TStrings;
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property MenuItems: TStrings read FMenuItems write setItems;

    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

  TFXContextMenu = class (TFXNode)
  private
    FMenuItems: TStrings;
    FListener: string;
    FId: string;

    Faction: string;
    FcloseRequest: string;
    Fhidden: string;
    Fhiding: string;
    Fshowing: string;
    Fshown: string;
    procedure setItems(aItems: TStrings);
    procedure MakeListener(Value: string);
    function getContextMenuListener(Value: string): string;
  public
    MenuItemsOld: TStrings;
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure Paint; override;
  published
    property MenuItems: TStrings read FMenuItems write setItems;
    property Listener: string read FListener write FListener;
    property Id: string read FId write FId;

    property action: string read FAction write FAction;
    property closeRequest: string read FcloseRequest write FcloseRequest;
    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

implementation

uses SysUtils, Graphics,
     UGUIForm, UJava, UUtils, UJEComponents, UFrmEditor;

{--- TFXMenuBar ---------------------------------------------------------------}

constructor TFXMenuBar.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 115;
  Height:= 24;
  Width:= 104;
  JavaType:= 'MenuBar';
end;

procedure TFXMenuBar.Paint;
  var Form: TFGUIForm;
      i, x: integer; s: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= DarkShadow;  // DefaultBorderColor;
  if Background = ColorNone
    then Canvas.Brush.Color:= $F0F0F0
    else Canvas.Brush.Color:= Background;
  Canvas.Rectangle(0, 0, Width, Height);
  x:= 8;
  if Parent is TFGUIForm then begin
    Form:= Parent as TFGUIForm;
    for i:= 0 to Form.ComponentCount - 1 do
      if Form.Components[i] is TFXMenu then begin
        s:= (Form.Components[i] as TFXMenu).Text;
        Canvas.TextOut(x + 8, 7, s);
        x:= x + 8 + Canvas.TextWidth(s) + 8;
      end;
  end;
end;

procedure TFXMenuBar.NewControl;
begin
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  Partner.InsertComponent(surroundFix2(AddFXVariable) +
                          surroundFix2(Name + '.setPrefWidth(4000);'));
end;

function TFXMenuBar.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name|UseSystemMenuBar' + inherited;
  var p:= Pos('|LayoutX|LayoutY', Result);
  if p > 0 then
    delete(Result, p, length('|LayoutX|LayoutY'));
  p:= Pos('|PrefHeight|PrefWidth', Result);
  if p > 0 then
    delete(Result, p, length('|PrefHeight|PrefWidth'));
end;

procedure TFXMenuBar.SetPositionAndSize;
begin
  LayoutX:= 0;
  LayoutY:= 0;
  Height:= 24;
  Width:= Parent.Width - 16;
end;

{--- TFXMenuBarWithMenu--------------------------------------------------------}

constructor TFXMenuBarWithMenus.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 124;
  Height:= 23;
  Width:= 80;
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'MenuBar';
end;

destructor TFXMenuBarWithMenus.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

function TFXMenuBarWithMenus.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name|MenuItems|UseSystemMenuBar' + inherited;
  var p:= Pos('|LayoutX|LayoutY', Result);
  if p > 0 then
    delete(Result, p, length('|LayoutX|LayoutY'));
  p:= Pos('|PrefHeight|PrefWidth', Result);
  if p > 0 then
    delete(Result, p, length('|PrefHeight|PrefWidth'));
end;

procedure TFXMenuBarWithMenus.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems, true)
  else
    inherited;
end;

procedure TFXMenuBarWithMenus.NewControl;
begin
  Partner.InsertImport('javafx.scene.input.*');
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  MakeMenuItems(MenuItemsOld, MenuItems, true);
  Partner.InsertComponent(surroundFix2(Name + '.setPrefWidth(4000);'));
end;

procedure TFXMenuBarWithMenus.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private MenuBar ' + Name);
  Partner.DeleteAttributeValue('root.getChildren().add(');
  Partner.DeleteAttributeValues('setAccelerator(KeyCombination.keyCombination');
  Partner.DeleteAttributeValues(Name + '.getMenus().add');
  Partner.DeleteAttributeValues(Name + '.set');
end;

procedure TFXMenuBarWithMenus.Rename(const OldName, NewName, Events: string);
begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  Partner.Editor.EndUpdate;
end;

procedure TFXMenuBarWithMenus.Paint;
  var i, x: integer;
begin
  CanvasFontAssign;
  Canvas.Font.Size:= 9;
  Canvas.Pen.Color:= $F0F0F0;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(0, 0, Width, Height);
  x:= 8;
  for i:= 0 to FMenuItems.Count - 1 do
    if LeftSpaces(FMenuItems[i], 2) = 0 then begin
      Canvas.TextOut(x + 8, 3, FMenuItems[i]);
      x:= x + 8 + Canvas.TextWidth(FMenuItems[i]) + 8;
    end;
end;

procedure TFXMenuBarWithMenus.SetPositionAndSize;
begin
  Left:= 0;
  Top:= 0;
  Height:= 23;
  Width:= Parent.Width - 16;
end;

procedure TFXMenuBarWithMenus.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

{--- TFXMenuItem --------------------------------------------------------------}

constructor TFXMenuItem.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Height:= 28;
  Width:= 32;
  FSeparator:= 1;
  Sizeable:= false;
end;

procedure TFXMenuItem.InsertMenuItem(const Title, IndentStr: string);
  var s1, s2: string; ch: char; p: integer;
begin
  s1:= trim(Title);
  p:= Pos('&', s1);
  if p > 0 then begin
    ch:= s1[p+1];
    Delete(s1, p, 1);
    InsertAttributValue(Indent1 + IndentStr + 'private MenuItem ' + Name + ' = new MenuItem("' + s1 + '", ' + '''' + Ch + ''');' );
    end
  else
    InsertAttributValue(Indent1 + IndentStr + 'private MenuItem ' + Name + ' = new MenuItem("' + s1 + '");');
  s1:= MakeEventProcedure('action');
  s2:= MakeEventProcedureName('action');
  if not Partner.hasText('public void ' + s2) then
    Partner.InsertProcedure(0, s1);
  Partner.InsertListener(GetContainerAdd, getListener('action'));
end;

function TFXMenuItem.getMenuItemName(const SubMenu, s: string): string;
begin
  if Copy(trim(s), 1, 1) = '-' then begin
    Result:= '_Separator' + IntToStr(FSeparator);
    inc(FSeparator);
  end else
    Result:= SubMenu + '_' + OnlyCharsAndDigits(s);
end;

function TFXMenuItem.getEvents(ShowEvents: integer): string;
begin
  Result:= '|action|menuValidation';
end;

function TFXMenuItem.getAttributes(ShowAttributes: integer): string;
  const MenuItemsAttributes1 = '|Text|Name';
        MenuItemsAttributes2 = MenuItemsAttributes1 +
                          '|Disable|Graphic|Id|MnemonicParsing|ParentMenu|ParentPopup|Style|Visible';
begin
  if ShowAttributes = 1
    then Result:= MenuItemsAttributes1
    else Result:= MenuItemsAttributes2;
end;

procedure TFXMenuItem.NewControl;
begin
  // prevent abstract warning
end;

procedure TFXMenuItem.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(Faction);
  rename(FmenuValidation);
end;

{--- TFXMenu ------------------------------------------------------------------}

constructor TFXMenu.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 116;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  FMenuItems:= TStringList.Create;    // new menu items from user
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;  // old menu itens from user
  JavaType:= 'Menu';
end;

destructor TFXMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

procedure TFXMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 5, 2, 16);
end;

procedure TFXMenu.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

procedure TFXMenu.NewControl;
begin
  InsertImport('javafx.scene.control.*');
  InsertImport('javafx.scene.input.KeyCombination');
  InsertNewVariable('private Menu ' + Name + ' = new Menu();');
  MakeMenuItems(MenuItemsOld, MenuItems);
end;

procedure TFXMenu.SetAttribute(Attr, Value, Typ: string);
  var key: string;
begin
  if Attr = 'MenuBar' then begin
    key:= '.getMenus().add(' + Name + ');';
    Partner.DeleteAttributeValue(key);
    if Value <> '' then
      setAttributValue(key, Indent2 + menuBar + '.getMenus().add(' + Name + ');');
  end else if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems)
  else if Attr = 'Text' then
    setAttributValue('private Menu ' + Name, 'private Menu ' + Name + ' = new Menu("' + Value + '");')
  else if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'Menu')
end;

procedure TFXMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private Menu ' + Name);
end;

procedure TFXMenu.Rename(const OldName, NewName, Events: string);

  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  rename(Fhidden);
  rename(Fhiding);
  rename(Fshowing);
  rename(Fshown);
  Partner.Editor.EndUpdate;
end;

function TFXMenu.getEvents(ShowEvents: integer): string;
  const Events1 = '|hidden|hiding|showing|shown';
        Events2 = Events1 + '|menuValidation';
begin
  if ShowEvents = 1
    then Result:= Events1 + inherited getEvents(ShowEvents)
    else Result:= Events2 + inherited getEvents(ShowEvents);
end;

function TFXMenu.getAttributes(ShowAttributes: integer): string;
begin
  Result:='|MenuItems|MenuBar|Name|Text' + inherited;
  var p:= Pos('|ParentMenu|ParentPopup', Result);
  if p > 0 then
    Delete(Result, p, length('|ParentMenu|ParentPopup'));
end;

{--- TFXContextMenu -----------------------------------------------------------}

constructor TFXContextMenu.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 117;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  Listener:= 'scene';
  JavaType:= 'ContextMenu';
end;

destructor TFXContextMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

procedure TFXContextMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 7, 4, 17);
end;

procedure TFXContextMenu.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

procedure TFXContextMenu.NewControl;
begin
  InsertImport('javafx.scene.control.*');
  InsertNewVariable('private ContextMenu ' + Name + ' = new ContextMenu();');
  MakeMenuItems(MenuItemsOld, MenuItems);
  Partner.InsertListener(GetContainerAdd, getContextMenuListener(Listener));
end;

procedure TFXContextMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems)
  else if Attr = 'Listener' then
    MakeListener(Value)
  else
    inherited
end;

function TFXContextMenu.getEvents(ShowEvents: integer): string;
  const show1 = '|action|closeRequest';
        show2 = '|hidden|hiding|showing|shown|';
begin
  if ShowEvents = 1
    then Result:= show1
    else Result:= show1 + show2;
end;

function TFXContextMenu.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|MenuItems|Name|Id|Style|Listener';
end;

procedure TFXContextMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private ContextMenu ' + Name);
  Partner.DeleteListener(getContextMenuListener(Listener));
end;

procedure TFXContextMenu.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  rename(Faction);
  rename(Fhidden);
  rename(Fhiding);
  rename(Fshowing);
  rename(Fshown);
  rename(FcloseRequest);
  Partner.Editor.EndUpdate;
end;

function TFXContextMenu.getContextMenuListener(Value: string): string;
begin
  Result:= surroundFix2(Value + '.setOnMouseReleased(') +
           surroundFix3('(event) -> {') +
           surroundFix3(Indent1 + 'if (event.isPopupTrigger()) {') +
           surroundFix3(Indent2 + 'MouseEvent me = (MouseEvent)event;') +
           surroundFix3(Indent2 + Name + '.show(root, me.getScreenX(), me.getScreenY());') +
           surroundFix3('}') +
           surroundFix2('}') +
           surroundFix(');');
end;

procedure TFXContextMenu.MakeListener(Value: string);
  var i: integer;
begin
  i:= Partner.getLineNumberWith(Name + '.show(root, me.getScreenX()');
  Partner.ReplaceLineWith(i-4, Indent2 + Value + '.setOnMouseReleased(');
end;

end.
