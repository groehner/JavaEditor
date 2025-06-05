unit UJMenu;

interface

uses
  Classes,
  UJComponents;

type

  TJMenuBar = class(TSwingComponent)
  private
    FBorderPainted: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure Paint; override;
  published
    property BorderPainted: Boolean read FBorderPainted write FBorderPainted;
  end;

  TJMenuBarWithMenus = class(TSwingComponent)
  private
    FBorderPainted: Boolean;
    FMenuItems: TStrings;
    FMenuItemsOld: TStrings;
  protected
    procedure SetItems(AItems: TStrings);
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
    property BorderPainted: Boolean read FBorderPainted write FBorderPainted;
    property MenuItems: TStrings read FMenuItems write SetItems;
  end;

  TJMenu = class(TSwingComponent)
  private
    FText: string;
    FMenuBar: string;
    FMenuItems: TStrings;
    FMenuItemsOld: TStrings;
    procedure MakeMenuBar(Value: string);
  protected
    procedure SetItems(AItems: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Text: string read FText write FText;
    property MenuBar: string read FMenuBar write FMenuBar;
    property MenuItems: TStrings read FMenuItems write SetItems;
  end;

  TJPopupMenu = class(TSwingComponent)
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
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure DeleteComponent; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
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
  UJava,
  UGUIForm,
  UUtils;

{ --- TJMenuBar ---------------------------------------------------------------- }

constructor TJMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 42;
  Height := 23;
  Width := 80;
  JavaType := 'JMenuBar';
end;

function TJMenuBar.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name|BorderPainted';
end;

procedure TJMenuBar.NewControl;
begin
  InsertNewVariable('private JMenuBar ' + Name + ' = new JMenuBar();');
  FPartner.InsertComponent(Indent2 + 'setJMenuBar(' + Name + ');');
end;

procedure TJMenuBar.Paint;
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
      if Form.Components[I] is TJMenu then
      begin
        Str := (Form.Components[I] as TJMenu).Text;
        Canvas.TextOut(XPos + 8, 3, Str);
        XPos := XPos + 8 + Canvas.TextWidth(Str) + 8;
      end;
  end;
end;

procedure TJMenuBar.SetPositionAndSize;
begin
  Left := 0;
  Top := 0;
  Height := PPIScale(23);
  Width := Parent.Width - 16;
end;

{ --- TJMenuBarWithMenu--------------------------------------------------------- }

constructor TJMenuBarWithMenus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 52;
  Height := 23;
  Width := 80;
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'JMenuBar';
end;

destructor TJMenuBarWithMenus.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

function TJMenuBarWithMenus.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name|MenuItems|BorderPainted';
end;

procedure TJMenuBarWithMenus.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems, True)
  else
    inherited;
end;

procedure TJMenuBarWithMenus.NewControl;
begin
  InsertNewVariable('private JMenuBar ' + Name + ' = new JMenuBar();');
  MakeMenuItems(FMenuItemsOld, MenuItems, True);
end;

procedure TJMenuBarWithMenus.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private JMenuBar ' + Name);
  FPartner.DeleteAttributeValue('setJMenuBar(' + Name + ')');
  FPartner.DeleteAttributeValue(Name + '.setBorderPainted');
  FPartner.DeleteAttributeValues('setAccelerator(KeyStroke.getKeyStroke');
end;

procedure TJMenuBarWithMenus.Rename(const OldName, NewName, Events: string);
begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, 'J');
  inherited;
  FPartner.Editor.EndUpdate;
end;

procedure TJMenuBarWithMenus.Paint;
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

procedure TJMenuBarWithMenus.SetPositionAndSize;
begin
  Left := 0;
  Top := 0;
  Height := PPIScale(23);
  Width := Parent.Width - 16;
end;

procedure TJMenuBarWithMenus.SetItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

{ --- TJMenu ------------------------------------------------------------------- }

constructor TJMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 43;
  Height := 28;
  Width := 32;
  Sizeable := False;
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'JMenu';
end;

function TJMenu.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name|Text|MenuBar|MenuItems';
end;

procedure TJMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    FPartner.ReplaceAttribute('private JMenu ' + Name,
      Indent1 + 'private JMenu ' + Name + ' = new JMenu("' + Value + '");')
  else if Attr = 'MenuBar' then
    MakeMenuBar(Value)
  else if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems)
  else
    inherited;
end;

procedure TJMenu.NewControl;
begin
  InsertNewVariable('private JMenu ' + Name + ' = new JMenu();');
  MakeMenuItems(FMenuItemsOld, MenuItems);
end;

procedure TJMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private JMenu ' + Name);
end;

procedure TJMenu.MakeMenuBar(Value: string);
var
  Key, Str: string;
begin
  Key := '.add(' + Name + ');';
  Str := Indent2 + Value + '.add(' + Name + ');';
  FPartner.DeleteAttributeValue(Key);
  SetAttributValue(Key, Str);
end;

procedure TJMenu.Rename(const OldName, NewName, Events: string);
begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, 'J');
  inherited;
  FPartner.Editor.EndUpdate;
end;

destructor TJMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

procedure TJMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 5, 2, 16);
end;

procedure TJMenu.SetItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

{ --- TJPopupMenu --------------------------------------------------------------- }

constructor TJPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 44;
  Height := 28;
  Width := 32;
  Sizeable := False;
  Listener := 'cp';
  FMenuItems := TStringList.Create;
  FMenuItems.Text := DefaultMenu;
  FMenuItemsOld := TStringList.Create;
  JavaType := 'JPopupMenu';
end;

destructor TJPopupMenu.Destroy;
begin
  FreeAndNil(FMenuItems);
  FreeAndNil(FMenuItemsOld);
  inherited;
end;

function TJPopupMenu.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|MenuItems|Name|Listener';
end;

procedure TJPopupMenu.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MenuItems' then
    MakeMenuItems(FMenuItemsOld, MenuItems)
  else if Attr = 'Listener' then
    MakeListener(Value)
  else
    inherited;
end;

function TJPopupMenu.GetEvents(ShowEvents: Integer): string;
begin
  Result := '' + inherited;
end;

procedure TJPopupMenu.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  FPartner.Editor.BeginUpdate;
  RenameMenu(OldName, NewName, 'J');
  inherited;
  FPartner.Editor.EndUpdate;
end;

procedure TJPopupMenu.NewControl;
begin
  InsertNewVariable('private JPopupMenu ' + Name + ' = new JPopupMenu();');
  MakeMenuItems(FMenuItemsOld, MenuItems);
  FPartner.InsertListener(GetContainerAdd, GetContextMenuListener(Listener));
end;

procedure TJPopupMenu.DeleteComponent;
begin
  DeleteEvents;
  DeleteMenuItems(FMenuItemsOld, MenuItems);
  FPartner.DeleteAttribute('private JPopupMenu ' + Name);
  FPartner.DeleteListener(GetContextMenuListener(Listener));
end;

procedure TJPopupMenu.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 7, 4, 17);
end;

procedure TJPopupMenu.SetItems(AItems: TStrings);
begin
  FMenuItemsOld.Text := FMenuItems.Text;
  if AItems.Text <> FMenuItems.Text then
    FMenuItems.Assign(AItems);
end;

end.
