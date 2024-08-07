unit UAMenu;

interface

uses
  Classes, UAComponents;

type

  TAMenuBar = class (TAWTComponent)
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  end;

  TAMenuBarWithMenus = class (TAWTComponent)
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
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  published
    property MenuItems: TStrings read FMenuItems write setItems;
  end;

  TAMenu = class (TAWTComponent)
  private
    FText: string;
    FMenuBar: string;
    FMenuItems: TStrings;
    procedure MakeMenuBar(Value: string);
    procedure setItems(aItems: TStrings);
  public
    MenuItemsOld: TStrings;
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Text: string read FText write FText;
    property MenuBar: string read FMenuBar write FMenuBar;
    property MenuItems: TStrings read FMenuItems write setItems;
  end;

  TAPopupMenu = class (TAWTComponent)
  private
    FText: string; // deprecated
    FListener: string;
    FMenuItems: TStrings;
  protected
    procedure setItems(aItems: TStrings);
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
    property Text: string read FText write FText;
    property Listener: string read FListener write FListener;
    property MenuItems: TStrings read FMenuItems write setItems;
  end;

implementation

uses SysUtils, Graphics, Controls,
     UJEComponents, UGuiForm, UUtils, UJava;

{--- TAMenuBar ----------------------------------------------------------------}

constructor TAMenuBar.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= -42;
  Height:= 20;
  Width:= 80;
  ShowFont:= false;
  JavaType:= 'MenuBar';
end;

function TAMenuBar.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name';
end;

function TAMenuBar.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TAMenuBar.NewControl;
begin
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  Partner.InsertComponent(Indent2 + 'setMenuBar(' + Name + ');' )
end;

procedure TAMenuBar.Paint;
  var Form: TFGUIForm;
      i, x: integer; s: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= $F0F0F0;
  Canvas.Brush.Color:= $F0F0F0;
  Canvas.Rectangle(0, 0, Width, Height);
  x:= 2;
  if Parent is TFGUIForm then begin
    Form:= Parent as TFGUIForm;
    for i:= 0 to Form.ComponentCount - 1 do
      if Form.Components[i] is TAMenu then begin
        s:= (Form.Components[i] as TAMenu).Text;
        Canvas.TextOut(x + 8, 3, s);
        x:= x + 8 + Canvas.TextWidth(s) + 8;
      end;
  end;
end;

procedure TAMenuBar.SetPositionAndSize;
begin
  Left:= 0;
  Top:= 0;
  Height:= 20;
  Width:= Parent.Width - 16;
end;

{--- TAMenuBarWithMenu---------------------------------------------------------}

constructor TAMenuBarWithMenus.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= -52;
  Height:= 23;
  Width:= 80;
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'MenuBar';
end;

destructor TAMenuBarWithMenus.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

function TAMenuBarWithMenus.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name|MenuItems';
end;

procedure TAMenuBarWithMenus.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems, true)
  else
    inherited;
end;

function TAMenuBarWithMenus.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TAMenuBarWithMenus.NewControl;
begin
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  MakeMenuItems(MenuItemsOld, MenuItems, true);
end;

procedure TAMenuBarWithMenus.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private MenuBar ' + Name);
  Partner.DeleteAttributeValue('setMenuBar(' + Name + ')');
  Partner.DeleteAttributeValues('setShortcut(new MenuShortcut');
end;

procedure TAMenuBarWithMenus.Rename(const OldName, NewName, Events: string);
begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, '');
  inherited;
  Partner.Editor.EndUpdate;
end;

procedure TAMenuBarWithMenus.Paint;
  var i, x: integer;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= $F0F0F0;
  Canvas.Brush.Color:= $F0F0F0;
  Canvas.Rectangle(0, 0, Width, Height);
  x:= 2;
  for i:= 0 to FMenuItems.Count - 1 do
    if LeftSpaces(FMenuItems[i], 2) = 0 then begin
      Canvas.TextOut(x + 8, 3, FMenuItems[i]);
      x:= x + 8 + Canvas.TextWidth(FMenuItems[i]) + 8;
    end;
end;

procedure TAMenuBarWithMenus.SetPositionAndSize;
begin
  Left:= 0;
  Top:= 0;
  Height:= 23;
  Width:= Parent.Width - 16;
end;

procedure TAMenuBarWithMenus.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;


{--- TAMenu -------------------------------------------------------------------}

constructor TAMenu.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= -43;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'Menu';
end;

function TAMenu.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Text|MenuBar|MenuItems|Name';
end;

procedure TAMenu.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    Partner.ReplaceAttribute('private Menu ' + Name, Indent1 + 'private Menu ' + Name + ' = new Menu("' + Value + '");')
  else if Attr = 'MenuBar' then
    MakeMenuBar(Value)
  else if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems)
  else
    inherited;
end;

function TAMenu.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TAMenu.Rename(const OldName, NewName, Events: string);
begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, '');
  inherited;
  Partner.Editor.EndUpdate;
end;

procedure TAMenu.MakeMenuBar(Value: string);
  var key, s: string;
begin
  key:= '.add(' + Name + ');';
  s:= Indent2 + Value + '.add(' + Name + ');';
  Partner.DeleteAttributeValue(key);
  setAttributValue(key, s);
end;

procedure TAMenu.NewControl;
begin
  InsertNewVariable('private Menu ' + Name + ' = new Menu("' + Text + '");');
  MakeMenuItems(MenuItemsOld, MenuItems);
end;

procedure TAMenu.DeleteComponent;
begin
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private Menu ' + Name);
end;

destructor TAMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

procedure TAMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 5, 2, 15);
end;

procedure TAMenu.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

{--- TAPopupMenu ---------------------------------------------------------------}

constructor TAPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= -44;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  Listener:= 'cp';
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'PopupMenu';
end;

destructor TAPopupMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

function TAPopupMenu.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|MenuItems|Name|Id|Style|Listener';
end;

procedure TAPopupMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems)
  else if Attr = 'Listener' then
    MakeListener(Value)
  else
    inherited
end;

function TAPopupMenu.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TAPopupMenu.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, '');
  inherited;
  Partner.Editor.EndUpdate;
end;

procedure TAPopupMenu.NewControl;
begin
  InsertNewVariable('private PopupMenu ' + Name + ' = new PopupMenu();');
  MakeMenuItems(MenuItemsOld, MenuItems);
  Partner.InsertListener(GetContainerAdd, getContextMenuListener(Listener));
  Partner.InsertComponent(Indent2 + 'add(' + Name + ');');
end;

procedure TAPopupMenu.DeleteComponent;
begin
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private PopupMenu ' + Name);
  Partner.DeleteListener(getContextMenuListener(Listener));
  Partner.DeleteAttributeValue(Indent2 + 'add(' + Name + ');');
end;

procedure TAPopupMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 7, 4, 16);
end;

procedure TAPopupMenu.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

end.
