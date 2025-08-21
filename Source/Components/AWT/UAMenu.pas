unit UAMenu;

interface

uses
  Classes, UAComponents;

type

  TAMenuBar = class(TAWTComponent)
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  end;

  TAMenuBarWithMenus = class(TAWTComponent)
  private
    FMenuItems: TStrings;
    FMenuItemsOld: TStrings;
  protected
    procedure SetItems(AItems: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  published
    property MenuItems: TStrings read FMenuItems write SetItems;
  end;

  TAMenu = class(TAWTComponent)
  private
    FText: string;
    FMenuBar: string;
    FMenuItems: TStrings;
    FMenuItemsOld: TStrings;
    procedure MakeMenuBar(const Value: string);
    procedure SetItems(AItems: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Text: string read FText write FText;
    property MenuBar: string read FMenuBar write FMenuBar;
    property MenuItems: TStrings read FMenuItems write SetItems;
  end;

  TAPopupMenu = class(TAWTComponent)
  private
    FText: string; // deprecated
    FListener: string;
    FMenuItems: TStrings;
    FMenuItemsOld: TStrings;
  protected
    procedure SetItems(AItems: TStrings);
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
    property Text: string read FText write FText;
    property Listener: string read FListener write FListener;
    property MenuItems: TStrings read FMenuItems write SetItems;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Controls,
  UJEComponents,
  UGUIForm,
  UUtils,
  UJava;

{ --- TAMenuBar ---------------------------------------------------------------- }

constructor TAMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -42;
  Height := 20;
  Width := 80;
  ShowFont := False;
  JavaType := 'MenuBar';
end;

function TAMenuBar.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name';
end;

function TAMenuBar.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TAMenuBar.NewControl;
begin
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  FPartner.InsertComponent(Indent2 + 'setMenuBar(' + Name + ');');
end;

procedure TAMenuBar.Paint;
var
  Form: TFGUIForm;
  XPos: Integer;
  Str: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := $F0F0F0;
  Canvas.Brush.Color := $F0F0F0;
  Canvas.Rectangle(0, 0, Width, Height);
  XPos := 2;
  if Parent is TFGUIForm then
  begin
    Form := Parent as TFGUIForm;
    for var I := 0 to Form.ComponentCount - 1 do
      if Form.Components[I] is TAMenu then
      begin
        Str := (Form.Components[I] as TAMenu).Text;
        Canvas.TextOut(XPos + 8, 3, Str);
        XPos := XPos + 8 + Canvas.TextWidth(Str) + 8;
      end;
  end;
end;

procedure TAMenuBar.SetPositionAndSize;
begin
  Left := 0;
  Top := 0;
  Height := 20;
  Width := Parent.Width - 16;
end;

{ --- TAMenuBarWithMenu--------------------------------------------------------- }

constructor TAMenuBarWithMenus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -52;
  Height := 23;
  Width := 80;
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'MenuBar';
end;

destructor TAMenuBarWithMenus.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

function TAMenuBarWithMenus.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name|MenuItems';
end;

procedure TAMenuBarWithMenus.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems, True)
  else
    inherited;
end;

function TAMenuBarWithMenus.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TAMenuBarWithMenus.NewControl;
begin
  InsertNewVariable('private MenuBar ' + Name + ' = new MenuBar();');
  MakeMenuItems(FMenuItemsOld, MenuItems, True);
end;

procedure TAMenuBarWithMenus.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private MenuBar ' + Name);
  FPartner.DeleteAttributeValue('setMenuBar(' + Name + ')');
  FPartner.DeleteAttributeValues('setShortcut(new MenuShortcut');
end;

procedure TAMenuBarWithMenus.Rename(const OldName, NewName, Events: string);
begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, '');
  inherited;
  FPartner.Editor.EndUpdate;
end;

procedure TAMenuBarWithMenus.Paint;
var
  XPos: Integer;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := $F0F0F0;
  Canvas.Brush.Color := $F0F0F0;
  Canvas.Rectangle(0, 0, Width, Height);
  XPos := 2;
  for var I := 0 to FMenuItems.Count - 1 do
    if LeftSpaces(FMenuItems[I], 2) = 0 then
    begin
      Canvas.TextOut(XPos + 8, 3, FMenuItems[I]);
      XPos := XPos + 8 + Canvas.TextWidth(FMenuItems[I]) + 8;
    end;
end;

procedure TAMenuBarWithMenus.SetPositionAndSize;
begin
  Left := 0;
  Top := 0;
  Height := 23;
  Width := Parent.Width - 16;
end;

procedure TAMenuBarWithMenus.SetItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

{ --- TAMenu ------------------------------------------------------------------- }

constructor TAMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -43;
  Height := 28;
  Width := 32;
  Sizeable := False;
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'Menu';
end;

function TAMenu.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Text|MenuBar|MenuItems|Name';
end;

procedure TAMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    FPartner.ReplaceAttribute('private Menu ' + Name, Indent1 + 'private Menu ' +
      Name + ' = new Menu("' + Value + '");')
  else if Attr = 'MenuBar' then
    MakeMenuBar(Value)
  else if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems)
  else
    inherited;
end;

function TAMenu.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TAMenu.Rename(const OldName, NewName, Events: string);
begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, '');
  inherited;
  FPartner.Editor.EndUpdate;
end;

procedure TAMenu.MakeMenuBar(const Value: string);
var
  Key, Str: string;
begin
  Key := '.add(' + Name + ');';
  Str := Indent2 + Value + '.add(' + Name + ');';
  FPartner.DeleteAttributeValue(Key);
  SetAttributValue(Key, Str);
end;

procedure TAMenu.NewControl;
begin
  InsertNewVariable('private Menu ' + Name + ' = new Menu("' + Text + '");');
  MakeMenuItems(FMenuItemsOld, MenuItems);
end;

procedure TAMenu.DeleteComponent;
begin
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private Menu ' + Name);
end;

destructor TAMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

procedure TAMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 5, 2, 15);
end;

procedure TAMenu.SetItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

{ --- TAPopupMenu --------------------------------------------------------------- }

constructor TAPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -44;
  Height := 28;
  Width := 32;
  Sizeable := False;
  Listener := 'cp';
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'PopupMenu';
end;

destructor TAPopupMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

function TAPopupMenu.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|MenuItems|Name|Id|Style|Listener';
end;

procedure TAPopupMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems)
  else if Attr = 'Listener' then
    MakeListener(Value)
  else
    inherited;
end;

function TAPopupMenu.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TAPopupMenu.Rename(const OldName, NewName, Events: string);

  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, '');
  inherited;
  FPartner.Editor.EndUpdate;
end;

procedure TAPopupMenu.NewControl;
begin
  InsertNewVariable('private PopupMenu ' + Name + ' = new PopupMenu();');
  MakeMenuItems(FMenuItemsOld, MenuItems);
  FPartner.InsertListener(GetContainerAdd, getContextMenuListener(Listener));
  FPartner.InsertComponent(Indent2 + 'add(' + Name + ');');
end;

procedure TAPopupMenu.DeleteComponent;
begin
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private PopupMenu ' + Name);
  FPartner.DeleteListener(getContextMenuListener(Listener));
  FPartner.DeleteAttributeValue(Indent2 + 'add(' + Name + ');');
end;

procedure TAPopupMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 7, 4, 16);
end;

procedure TAPopupMenu.SetItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

end.
