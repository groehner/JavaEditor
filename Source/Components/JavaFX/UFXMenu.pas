unit UFXMenu;

{ Classes
  TFXMenuBar = class (TFXControl)
  TFXMenuItem = class (TFXNode)
  TFXMenu
  TFXContextMenu = class (TFXNode)
}

interface

uses
  Classes,
  UFXComponents;

type

  TFXMenuBar = class(TFXControl)
  private
    FUseSystemMenuBar: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetPositionAndSize; override;
  published
    property UseSystemMenuBar: Boolean read FUseSystemMenuBar
      write FUseSystemMenuBar;
  end;

  TFXMenuBarWithMenus = class(TFXControl)
  private
    FMenuItems: TStrings;
    FMenuItemsOld: TStrings;
  protected
    procedure SetMenuItems(AItems: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  published
    property MenuItems: TStrings read FMenuItems write SetMenuItems;
  end;

  TFXMenuItem = class(TFXNode)
  private
    FDisable: Boolean;
    FGraphic: string;
    FId: string;
    FMnemonicParsing: Boolean;
    FParentMenu: string;
    FParentPopup: string;
    FText: string;
    FVisible: Boolean;
    FSeparator: Integer;

    FMenuBar: string;
    Faction: string;
    FmenuValidation: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InsertMenuItem(const Title, IndentStr: string);
    function GetMenuItemName(const SubMenu, Str: string): string;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
  published
    property Disable: Boolean read FDisable write FDisable;
    property Text: string read FText write FText;
    property Id: string read FId write FId;
    property ParentMenu: string read FParentMenu write FParentMenu;
    property ParentPopup: string read FParentPopup write FParentPopup;
    property Graphic: string read FGraphic write FGraphic;
    property MnemonicParsing: Boolean read FMnemonicParsing
      write FMnemonicParsing;
    property Visible: Boolean read FVisible write FVisible;
    property MenuBar: string read FMenuBar write FMenuBar;
    property Separator: Integer read FSeparator write FSeparator;

    property action: string read Faction write Faction;
    property menuValidation: string read FmenuValidation write FmenuValidation;
  end;

  TFXMenu = class(TFXMenuItem)
  private
    FMenuItems: TStrings;
    Fhidden: string;
    Fhiding: string;
    Fshowing: string;
    Fshown: string;
    FMenuItemsOld: TStrings;
    procedure SetMenuItems(AItems: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property MenuItems: TStrings read FMenuItems write SetMenuItems;

    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

  TFXContextMenu = class(TFXNode)
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
    FMenuItemsOld: TStrings;
    procedure SetMenuItems(AItems: TStrings);
    procedure MakeListener(const Value: string);
    function GetContextMenuListener(const Value: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure Paint; override;
  published
    property MenuItems: TStrings read FMenuItems write SetMenuItems;
    property Listener: string read FListener write FListener;
    property Id: string read FId write FId;

    property action: string read Faction write Faction;
    property closeRequest: string read FcloseRequest write FcloseRequest;
    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

implementation

uses
  SysUtils,
  Graphics,
  UGUIForm,
  UJava,
  UUtils,
  UJEComponents;

{ --- TFXMenuBar --------------------------------------------------------------- }

constructor TFXMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 115;
  Height := 24;
  Width := 104;
  JavaType := 'MenuBar';
end;

procedure TFXMenuBar.Paint;
var
  Form: TFGUIForm;
  XPos: Integer;
  Str: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := DarkShadow; // DefaultBorderColor
  if Background = ColorNone then
    Canvas.Brush.Color := $F0F0F0
  else
    Canvas.Brush.Color := Background;
  Canvas.Rectangle(0, 0, Width, Height);
  XPos := 8;
  if Parent is TFGUIForm then
  begin
    Form := TFGUIForm(Parent);
    for var I := 0 to Form.ComponentCount - 1 do
      if Form.Components[I] is TFXMenu then
      begin
        Str := TFXMenu(Form.Components[I]).Text;
        Canvas.TextOut(XPos + 8, 7, Str);
        XPos := XPos + 8 + Canvas.TextWidth(Str) + 8;
      end;
  end;
end;

procedure TFXMenuBar.NewControl;
begin
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  FPartner.InsertComponent(SurroundFix2(AddFXVariable) +
    SurroundFix2(Name + '.setPrefWidth(4000);'));
end;

function TFXMenuBar.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name|UseSystemMenuBar' + inherited;
  var
  Posi := Pos('|LayoutX|LayoutY', Result);
  if Posi > 0 then
    Delete(Result, Posi, Length('|LayoutX|LayoutY'));
  Posi := Pos('|PrefHeight|PrefWidth', Result);
  if Posi > 0 then
    Delete(Result, Posi, Length('|PrefHeight|PrefWidth'));
end;

procedure TFXMenuBar.SetPositionAndSize;
begin
  LayoutX := 0;
  LayoutY := 0;
  Height := 24;
  Width := Parent.Width - 16;
end;

{ --- TFXMenuBarWithMenu-------------------------------------------------------- }

constructor TFXMenuBarWithMenus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 124;
  Height := 23;
  Width := 80;
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'MenuBar';
end;

destructor TFXMenuBarWithMenus.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

function TFXMenuBarWithMenus.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name|MenuItems|UseSystemMenuBar' + inherited;
  var
  Posi := Pos('|LayoutX|LayoutY', Result);
  if Posi > 0 then
    Delete(Result, Posi, Length('|LayoutX|LayoutY'));
  Posi := Pos('|PrefHeight|PrefWidth', Result);
  if Posi > 0 then
    Delete(Result, Posi, Length('|PrefHeight|PrefWidth'));
end;

procedure TFXMenuBarWithMenus.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems, True)
  else
    inherited;
end;

procedure TFXMenuBarWithMenus.NewControl;
begin
  FPartner.InsertImport('javafx.scene.input.*');
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  MakeMenuItems(FMenuItemsOld, MenuItems, True);
  FPartner.InsertComponent(SurroundFix2(Name + '.setPrefWidth(4000);'));
end;

procedure TFXMenuBarWithMenus.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private MenuBar ' + Name);
  FPartner.DeleteAttributeValue('root.getChildren().add(');
  FPartner.DeleteAttributeValues
    ('setAccelerator(KeyCombination.keyCombination');
  FPartner.DeleteAttributeValues(Name + '.getMenus().add');
  FPartner.DeleteAttributeValues(Name + '.set');
end;

procedure TFXMenuBarWithMenus.Rename(const OldName, NewName, Events: string);
begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  FPartner.Editor.EndUpdate;
end;

procedure TFXMenuBarWithMenus.Paint;
var
  XPos: Integer;
begin
  CanvasFontAssign;
  Canvas.Font.Size := 9;
  Canvas.Pen.Color := $F0F0F0;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(0, 0, Width, Height);
  XPos := 8;
  for var I := 0 to FMenuItems.Count - 1 do
    if LeftSpaces(FMenuItems[I], 2) = 0 then
    begin
      Canvas.TextOut(XPos + 8, 3, FMenuItems[I]);
      XPos := XPos + 8 + Canvas.TextWidth(FMenuItems[I]) + 8;
    end;
end;

procedure TFXMenuBarWithMenus.SetPositionAndSize;
begin
  Left := 0;
  Top := 0;
  Height := 23;
  Width := Parent.Width - 16;
end;

procedure TFXMenuBarWithMenus.SetMenuItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

{ --- TFXMenuItem -------------------------------------------------------------- }

constructor TFXMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 28;
  Width := 32;
  FSeparator := 1;
  Sizeable := False;
end;

procedure TFXMenuItem.InsertMenuItem(const Title, IndentStr: string);
var
  Str1, Str2: string;
  Chr: Char;
  Posi: Integer;
begin
  Str1 := Trim(Title);
  Posi := Pos('&', Str1);
  if Posi > 0 then
  begin
    Chr := Str1[Posi + 1];
    Delete(Str1, Posi, 1);
    InsertAttributValue(Indent1 + IndentStr + 'private MenuItem ' + Name +
      ' = new MenuItem("' + Str1 + '", ' + '''' + Chr + ''');');
  end
  else
    InsertAttributValue(Indent1 + IndentStr + 'private MenuItem ' + Name +
      ' = new MenuItem("' + Str1 + '");');
  Str1 := MakeEventProcedure('action');
  Str2 := MakeEventProcedureName('action');
  if not FPartner.HasText('public void ' + Str2) then
    FPartner.InsertProcedure(0, Str1);
  FPartner.InsertListener(GetContainerAdd, GetListener('action'));
end;

function TFXMenuItem.GetMenuItemName(const SubMenu, Str: string): string;
begin
  if Copy(Trim(Str), 1, 1) = '-' then
  begin
    Result := '_Separator' + IntToStr(FSeparator);
    Inc(FSeparator);
  end
  else
    Result := SubMenu + '_' + OnlyCharsAndDigits(Str);
end;

function TFXMenuItem.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|action|menuValidation';
end;

function TFXMenuItem.GetAttributes(ShowAttributes: Integer): string;
const
  MenuItemsAttributes1 = '|Text|Name';
  MenuItemsAttributes2 = MenuItemsAttributes1 +
    '|Disable|Graphic|Id|MnemonicParsing|ParentMenu|ParentPopup|Style|Visible';
begin
  if ShowAttributes = 1 then
    Result := MenuItemsAttributes1
  else
    Result := MenuItemsAttributes2;
end;

procedure TFXMenuItem.NewControl;
begin
  // prevent abstract warning
end;

procedure TFXMenuItem.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(Faction);
  Rename(FmenuValidation);
end;

{ --- TFXMenu ------------------------------------------------------------------ }

constructor TFXMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 116;
  Height := 28;
  Width := 32;
  Sizeable := False;
  FMenuItems := TStringList.Create; // new menu items from user
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create; // old menu itens from user
  JavaType := 'Menu';
end;

destructor TFXMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

procedure TFXMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 5, 2, 16);
end;

procedure TFXMenu.SetMenuItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

procedure TFXMenu.NewControl;
begin
  InsertImport('javafx.scene.control.*');
  InsertImport('javafx.scene.input.KeyCombination');
  InsertNewVariable('private Menu ' + Name + ' = new Menu();');
  MakeMenuItems(FMenuItemsOld, MenuItems);
end;

procedure TFXMenu.SetAttribute(Attr, Value, Typ: string);
var
  Key: string;
begin
  if Attr = 'MenuBar' then
  begin
    Key := '.getMenus().add(' + Name + ');';
    FPartner.DeleteAttributeValue(Key);
    if Value <> '' then
      SetAttributValue(Key, Indent2 + MenuBar + '.getMenus().add(' +
        Name + ');');
  end
  else if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems)
  else if Attr = 'Text' then
    SetAttributValue('private Menu ' + Name, 'private Menu ' + Name +
      ' = new Menu("' + Value + '");')
  else if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'Menu');
end;

procedure TFXMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private Menu ' + Name);
end;

procedure TFXMenu.Rename(const OldName, NewName, Events: string);

  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  Rename(Fhidden);
  Rename(Fhiding);
  Rename(Fshowing);
  Rename(Fshown);
  FPartner.Editor.EndUpdate;
end;

function TFXMenu.GetEvents(ShowEvents: Integer): string;
const
  Events1 = '|hidden|hiding|showing|shown';
  Events2 = Events1 + '|menuValidation';
begin
  if ShowEvents = 1 then
    Result := Events1 + inherited GetEvents(ShowEvents)
  else
    Result := Events2 + inherited GetEvents(ShowEvents);
end;

function TFXMenu.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|MenuItems|MenuBar|Name|Text' + inherited;
  var
  Posi := Pos('|ParentMenu|ParentPopup', Result);
  if Posi > 0 then
    Delete(Result, Posi, Length('|ParentMenu|ParentPopup'));
end;

{ --- TFXContextMenu ----------------------------------------------------------- }

constructor TFXContextMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 117;
  Height := 28;
  Width := 32;
  Sizeable := False;
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  Listener := 'scene';
  JavaType := 'ContextMenu';
end;

destructor TFXContextMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

procedure TFXContextMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 7, 4, 17);
end;

procedure TFXContextMenu.SetMenuItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

procedure TFXContextMenu.NewControl;
begin
  InsertImport('javafx.scene.control.*');
  InsertNewVariable('private ContextMenu ' + Name + ' = new ContextMenu();');
  MakeMenuItems(FMenuItemsOld, MenuItems);
  FPartner.InsertListener(GetContainerAdd, GetContextMenuListener(Listener));
end;

procedure TFXContextMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems)
  else if Attr = 'Listener' then
    MakeListener(Value)
  else
    inherited;
end;

function TFXContextMenu.GetEvents(ShowEvents: Integer): string;
const
  Show1 = '|action|closeRequest';
  Show2 = '|hidden|hiding|showing|shown|';
begin
  if ShowEvents = 1 then
    Result := Show1
  else
    Result := Show1 + Show2;
end;

function TFXContextMenu.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|MenuItems|Name|Id|Style|Listener';
end;

procedure TFXContextMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private ContextMenu ' + Name);
  FPartner.DeleteListener(GetContextMenuListener(Listener));
end;

procedure TFXContextMenu.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName);
  inherited;
  Rename(Faction);
  Rename(Fhidden);
  Rename(Fhiding);
  Rename(Fshowing);
  Rename(Fshown);
  Rename(FcloseRequest);
  FPartner.Editor.EndUpdate;
end;

function TFXContextMenu.GetContextMenuListener(const Value: string): string;
begin
  Result := SurroundFix2(Value + '.setOnMouseReleased(') +
    SurroundFix3('(event) -> {') +
    SurroundFix3(Indent1 + 'if (event.isPopupTrigger()) {') +
    SurroundFix3(Indent2 + 'MouseEvent me = (MouseEvent)event;') +
    SurroundFix3(Indent2 + Name +
    '.show(root, me.getScreenX(), me.getScreenY());') + SurroundFix3('}') +
    SurroundFix2('}') + SurroundFix(');');
end;

procedure TFXContextMenu.MakeListener(const Value: string);
var
  Int: Integer;
begin
  Int := FPartner.GetLineNumberWith(Name + '.show(root, me.getScreenX()');
  FPartner.ReplaceLineWith(Int - 4, Indent2 + Value + '.setOnMouseReleased(');
end;

end.
