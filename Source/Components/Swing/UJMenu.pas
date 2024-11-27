unit UJMenu;

interface

uses
  Classes, UJComponents;

type

  TJMenuBar = class (TSwingComponent)
  private
    FBorderPainted: boolean;
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  published
    property BorderPainted: boolean read FBorderPainted write FBorderPainted;
  end;

  TJMenuBarWithMenus = class (TSwingComponent)
  private
    FBorderPainted: boolean;
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
    property BorderPainted: boolean read FBorderPainted write FBorderPainted;
    property MenuItems: TStrings read FMenuItems write setItems;
  end;

  TJMenu = class (TSwingComponent)
  private
    FText: string;
    FMenuBar: string;
    FMenuItems: TStrings;
    procedure MakeMenuBar(Value: string);
  protected
    procedure setItems(aItems: TStrings);
  public
    MenuItemsOld: TStrings;
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Text: string read FText write FText;
    property MenuBar: string read FMenuBar write FMenuBar;
    property MenuItems: TStrings read FMenuItems write setItems;
  end;

  TJPopupMenu = class (TSwingComponent)
  private
    FText: string;  // deprecated
    FListener: string;
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
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Text: string read FText write FText;
    property Listener: string read FListener write FListener;
    property MenuItems: TStrings read FMenuItems write setItems;
  end;

implementation

uses SysUtils, Graphics, Controls, UJEComponents, UJava, UGuiForm, UUtils;

{--- TJMenuBar ----------------------------------------------------------------}

constructor TJMenuBar.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 42;
  Height:= 23;
  Width:= 80;
  JavaType:= 'JMenuBar';
end;

function TJMenuBar.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name|BorderPainted';
end;

procedure TJMenuBar.NewControl;
begin
  InsertNewVariable('private JMenuBar ' + Name + ' = new JMenuBar();');
  Partner.InsertComponent(Indent2 + 'setJMenuBar(' + Name + ');' )
end;

procedure TJMenuBar.Paint;
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
      if Form.Components[i] is TJMenu then begin
        s:= (Form.Components[i] as TJMenu).Text;
        Canvas.TextOut(x + 8, 3, s);
        x:= x + 8 + Canvas.TextWidth(s) + 8;
      end;
  end;
end;

procedure TJMenuBar.SetPositionAndSize;
begin
  Left:= 0;
  Top:= 0;
  Height:= PPIScale(23);
  Width:= Parent.Width - 16;
end;

{--- TJMenuBarWithMenu---------------------------------------------------------}

constructor TJMenuBarWithMenus.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 52;
  Height:= 23;
  Width:= 80;
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'JMenuBar';
end;

destructor TJMenuBarWithMenus.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

function TJMenuBarWithMenus.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name|MenuItems|BorderPainted';
end;

procedure TJMenuBarWithMenus.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems, true)
  else
    inherited;
end;

procedure TJMenuBarWithMenus.NewControl;
begin
  InsertNewVariable('private JMenuBar ' + Name + ' = new JMenuBar();');
  MakeMenuItems(MenuItemsOld, MenuItems, true);
end;

procedure TJMenuBarWithMenus.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private JMenuBar ' + Name);
  Partner.DeleteAttributeValue('setJMenuBar(' + Name + ')');
  Partner.DeleteAttributeValue(Name + '.setBorderPainted');
  Partner.DeleteAttributeValues('setAccelerator(KeyStroke.getKeyStroke');
end;

procedure TJMenuBarWithMenus.Rename(const OldName, NewName, Events: string);
begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, 'J');
  inherited;
  Partner.Editor.EndUpdate;
end;

procedure TJMenuBarWithMenus.Paint;
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

procedure TJMenuBarWithMenus.SetPositionAndSize;
begin
  Left:= 0;
  Top:= 0;
  Height:= PPIScale(23);
  Width:= Parent.Width - 16;
end;

procedure TJMenuBarWithMenus.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

{--- TJMenu -------------------------------------------------------------------}

constructor TJMenu.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 43;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'JMenu';
end;

function TJMenu.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name|Text|MenuBar|MenuItems';
end;

procedure TJMenu.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    Partner.ReplaceAttribute('private JMenu ' + Name, Indent1 + 'private JMenu ' + Name + ' = new JMenu("' + Value + '");')
  else if Attr = 'MenuBar' then
    MakeMenuBar(Value)
  else if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems)
  else
    inherited;
end;

procedure TJMenu.NewControl;
begin
  InsertNewVariable('private JMenu ' + Name + ' = new JMenu();');
  MakeMenuItems(MenuItemsOld, MenuItems);
end;

procedure TJMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private JMenu ' + Name);
end;

procedure TJMenu.MakeMenuBar(Value: string);
  var key, s: string;
begin
  key:= '.add(' + Name + ');';
  s:= Indent2 + Value + '.add(' + Name + ');';
  Partner.DeleteAttributeValue(key);
  setAttributValue(key, s);
end;

procedure TJMenu.Rename(const OldName, NewName, Events: string);
begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, 'J');
  inherited;
  Partner.Editor.EndUpdate;
end;

destructor TJMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

procedure TJMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 5, 2, 16);
end;

procedure TJMenu.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

{--- TJPopupMenu ---------------------------------------------------------------}

constructor TJPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 44;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  Listener:= 'cp';
  FMenuItems:= TStringList.Create;
  FMenuItems.Text:= DefaultMenu;
  MenuItemsOld:= TStringList.Create;
  JavaType:= 'JPopupMenu';
end;

destructor TJPopupMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(MenuItemsOld);
  inherited;
end;

function TJPopupMenu.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|MenuItems|Name|Listener';
end;

procedure TJPopupMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(MenuItemsOld, MenuItems)
  else if Attr = 'Listener' then
    MakeListener(Value)
  else
    inherited
end;

function TJPopupMenu.getEvents(ShowEvents: integer): string;
begin
  Result:= '' + inherited;
end;

procedure TJPopupMenu.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  Partner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, 'J');
  inherited;
  Partner.Editor.EndUpdate;
end;

procedure TJPopupMenu.NewControl;
begin
  InsertNewVariable('private JPopupMenu ' + Name + ' = new JPopupMenu();');
  MakeMenuItems(MenuItemsOld, MenuItems);
  Partner.InsertListener(GetContainerAdd, getContextMenuListener(Listener));
end;

procedure TJPopupMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(MenuItemsOld, MenuItems);
  Partner.DeleteAttribute('private JPopupMenu ' + Name);
  Partner.DeleteListener(getContextMenuListener(Listener));
end;

procedure TJPopupMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 7, 4, 17);
end;

procedure TJPopupMenu.setItems(aItems: TStrings);
begin
  MenuItemsOld.Text:= FMenuItems.Text;
  if aItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(aItems);
end;

end.
