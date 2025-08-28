unit UFXComponents;

{ Classes
  TFXNode = class (TJEComponent)
  TFXRegion
  TFXControl
}

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  UJEComponents;

const
  CrLf = #13#10;
  CornerRadius = 5;
  ColorNone = clBtnFace;

  DragDropEvents = '|dragDetected|dragDone|dragDropped|dragEntered|dragExited' +
    '|dragOver|mouseDragEntered|mouseDragExited|mouseDragOver|mouseDragReleased';
  RotationEvents = '|rotate|rotationFinished|rotationStarted';
  ScrollEvents = '|scroll|scrollFinished|scrollStarted';
  SwipeEvents = '|swipeDown|swipeLeft|swipeRight|swipeUp';
  ZoomEvents = '|zoom|zoomFinished|zoomStarted';
  GestureEvents = RotationEvents + ScrollEvents + SwipeEvents + ZoomEvents;
  KeyboardEvents = '|inputMethodTextChanged|keyPressed|keyReleased|keyTyped';
  MouseEvents = '|contextMenuRequested|mouseClicked|mouseDragged|mouseEntered' +
    '|mouseExited|mouseMoved|mousePressed|mouseReleased';
  TouchEvents = '|touchMoved|touchPressed|touchReleased|touchStationary';
  NodeEvents = DragDropEvents + GestureEvents + KeyboardEvents + MouseEvents +
    TouchEvents;

type

  // used in TextInput and Labeled
  TAlignment = (BASELINE_CENTER, BASELINE_LEFT, BASELINE_RIGHT, TOP_CENTER,
    TOP_LEFT, TOP_RIGHT, CENTER, CENTER_LEFT, CENTER_RIGHT, BOTTOM_CENTER,
    BOTTOM_LEFT, BOTTOM_RIGHT);

  // used in Text and Labeled
  TTextAlignment = (_TA_CENTER, _TA_JUSTIFY, _TA_LEFT, _TA_RIGHT);

  TFXNode = class(TJEComponent) // Node
  private
    FDarkShadow: TColor;
    FDefaultBackground: TColor;
    FDefaultForeground: TColor;
    FBlueColor: TColor;
    FDefaultBorderColor: TColor;

    FDisable: Boolean;
    FCache: Boolean;
    FStyle: string;

    // events
    FcontextMenuRequested: string;
    FDragDetected: string;
    FdragDone: string;
    FdragDropped: string;
    FdragEntered: string;
    FdragExited: string;
    FdragOver: string;

    FinputMethodTextChanged: string;
    FkeyPressed: string;
    FkeyReleased: string;
    FkeyTyped: string;
    FmouseClicked: string;
    FmouseDragged: string;
    FmouseEntered: string;
    FmouseExited: string;
    FmouseMoved: string;
    FmousePressed: string;
    FmouseReleased: string;

    Frotate: string;
    FrotationFinished: string;
    FrotationStarted: string;
    Fscroll: string;
    FscrollFinished: string;
    FscrollStarted: string;
    FswipeDown: string;
    FswipeLeft: string;
    FswipeRight: string;
    FswipeUp: string;
    Fzoom: string;
    FzoomFinished: string;
    FzoomStarted: string;

    FmouseDragEntered: string;
    FmouseDragExited: string;
    FmouseDragOver: string;
    FmouseDragReleased: string;
    FtouchMoved: string;
    FtouchPressed: string;
    FtouchReleased: string;
    FtouchStationary: string;

    procedure SetLayoutX(AValue: Integer);
    function GetLayoutX: Integer;
    procedure SetLayoutY(AValue: Integer);
    function GetLayoutY: Integer;
    procedure MakeOrientation(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFromJ(Control: TControl);
    function FXContainer: string;
    function GetLayoutXCode: string;
    function GetLayoutYCode: string;
    function GetWidthCode: string;
    function GetHeightCode: string;
    procedure SetPositionAndSize; override;
    procedure InsertNewVariable(const Variable: string);
    procedure InsertAttributValue(Str: string);
    function GetContainerAdd: string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure ChangeAttributValue(const Key: string; Str: string);
    function AddFXVariable: string;
    procedure DefaultComponent;
    procedure SetBackgroundAsString(const AColor: string);
    function IsFontAttribute(const Str: string): Boolean;
    procedure MakeFont; override;
    procedure DoMakeFont;
    procedure DoMakeFontWithStyle;
    procedure MakeCursor(const Value: string);
    procedure MakeColor(const Attr, Value: string);
    function GetAttrColor(const Value: string): string;
    procedure CalculateMenus(MenuItems, Menu, ConstructMenu, Methods: TStrings;
      NewMenuBar: Boolean = False);
    procedure MakeMenuItems(OldItems, NewItems: TStrings;
      NewMenuBar: Boolean = False);
    procedure RenameMenu(const OldName, NewName: string);
    procedure DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);
    procedure Paint; override;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetListener(const Event: string): string; override;
    procedure DeleteListener(const Event: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function SurroundFix(const Str: string): string;
    function SurroundFix2(const Str: string): string;
    function MakeEventProcedure(const Event: string): string; override;
    function EventToEventtype(const Event: string): string;
    procedure InsertImport(const Str: string);
    procedure MakeGraphic(const Attr, Value, Typ: string);

    property DefaultBackground: TColor read FDefaultBackground;
    property DefaultForeground: TColor read FDefaultForeground;
    property DarkShadow: TColor read FDarkShadow;
    property DefaultBorderColor: TColor read FDefaultBorderColor;
    property BlueColor: TColor read FBlueColor;

  published
    property LayoutX: Integer read GetLayoutX write SetLayoutX;
    property LayoutY: Integer read GetLayoutY write SetLayoutY;
    property Disable: Boolean read FDisable write FDisable;
    property Cache: Boolean read FCache write FCache;
    property Style: string read FStyle write FStyle;
    property Visible;

    property contextMenuRequested: string read FcontextMenuRequested
      write FcontextMenuRequested;
    property dragDone: string read FdragDone write FdragDone;
    property dragDropped: string read FdragDropped write FdragDropped;
    property dragEntered: string read FdragEntered write FdragEntered;
    property dragExited: string read FdragExited write FdragExited;
    property dragOver: string read FdragOver write FdragOver;

    property rotate: string read Frotate write Frotate;
    property rotationFinished: string read FrotationFinished
      write FrotationFinished;
    property rotationStarted: string read FrotationStarted
      write FrotationStarted;
    property scroll: string read Fscroll write Fscroll;
    property scrollFinished: string read FscrollFinished write FscrollFinished;
    property scrollStarted: string read FscrollStarted write FscrollStarted;
    property swipeDown: string read FswipeDown write FswipeDown;
    property swipeLeft: string read FswipeLeft write FswipeLeft;
    property swipeRight: string read FswipeRight write FswipeRight;
    property swipeUp: string read FswipeUp write FswipeUp;
    property zoom: string read Fzoom write Fzoom;
    property zoomFinished: string read FzoomFinished write FzoomFinished;
    property zoomStarted: string read FzoomStarted write FzoomStarted;

    property inputMethodTextChanged: string read FinputMethodTextChanged
      write FinputMethodTextChanged;
    property keyPressed: string read FkeyPressed write FkeyPressed;
    property keyReleased: string read FkeyReleased write FkeyReleased;
    property keyTyped: string read FkeyTyped write FkeyTyped;
    property dragDetected: string read FDragDetected write FDragDetected;
    property mouseClicked: string read FmouseClicked write FmouseClicked;
    property mouseDragged: string read FmouseDragged write FmouseDragged;
    property mouseEntered: string read FmouseEntered write FmouseEntered;
    property mouseExited: string read FmouseExited write FmouseExited;
    property mousePressed: string read FmousePressed write FmousePressed;
    property mouseReleased: string read FmouseReleased write FmouseReleased;
    property mouseMoved: string read FmouseMoved write FmouseMoved;

    property mouseDragEntered: string read FmouseDragEntered
      write FmouseDragEntered;
    property mouseDragExited: string read FmouseDragExited
      write FmouseDragExited;
    property mouseDragOver: string read FmouseDragOver write FmouseDragOver;
    property mouseDragReleased: string read FmouseDragReleased
      write FmouseDragReleased;
    property touchMoved: string read FtouchMoved write FtouchMoved;
    property touchPressed: string read FtouchPressed write FtouchPressed;
    property touchReleased: string read FtouchReleased write FtouchReleased;
    property touchStationary: string read FtouchStationary
      write FtouchStationary;
  end;

  TFXRegion = class(TFXNode)
  private
    FPrefHeight: Integer;
    FPrefWidth: Integer;
    FBackground: TColor;
    FSnapToPixel: Boolean;
    procedure SetBackground(AColor: TColor);
    procedure SetPrefWidth(AValue: Integer);
    function GetPrefWidth: Integer;
    procedure SetPrefHeight(AValue: Integer);
    function GetPrefHeight: Integer;
    function GetPrefHeightCode: string;
    function GetPrefWidthCode: string;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetPositionAndSize; override;
  published
    property PrefHeight: Integer read GetPrefHeight write SetPrefHeight;
    property PrefWidth: Integer read GetPrefWidth write SetPrefWidth;
    property Background: TColor read FBackground write SetBackground;
    property SnapToPixel: Boolean read FSnapToPixel write FSnapToPixel;
  end;

  TFXControl = class(TFXRegion)
  private
    FTooltip: string;
    FContextMenu: string;
  public
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property Tooltip: string read FTooltip write FTooltip;
    property ContextMenu: string read FContextMenu write FContextMenu;
  end;

implementation

uses
  SysUtils,
  UEditorForm,
  UUtils,
  JvGnugettext,
  UStringRessources,
  UObjectInspector,
  UGUIForm,
  UConfiguration,
  ULink,
  UITypes;

{ --- TFXNode ------------------------------------------------------------------ }

constructor TFXNode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultBackground := RGB(244, 244, 244); // F4F4F4
  FDefaultForeground := RGB(51, 51, 51); // 333333  FOREGROUND
  FDarkShadow := RGB(122, 138, 153); // border color
  FBlueColor := RGB(99, 130, 191);
  FDefaultBorderColor := $C8C8C8;
  if AOwner is TFGUIForm then
    FPartner := TFEditForm(TFGUIForm(AOwner).Partner);
  Width := 120;
  Height := 80;
  Sizeable := True;
  HelpType := htContext;
  Font.Name := FConfiguration.GUIFontName; // Dialog
  Font.Size := FConfiguration.GUIFontSize;
  Font.Style := [];
end;

procedure TFXNode.CreateFromJ(Control: TControl);
begin
  Tag := Control.Tag;
  Visible := Control.Visible;
  SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
end;

procedure TFXNode.SetLayoutX(AValue: Integer);
begin
  if AValue <> Top then
  begin
    Left := AValue;
    Invalidate;
  end;
end;

function TFXNode.GetLayoutX: Integer;
begin
  Result := Left;
end;

procedure TFXNode.SetLayoutY(AValue: Integer);
begin
  if AValue <> Top then
  begin
    Top := AValue;
    Invalidate;
  end;
end;

function TFXNode.GetLayoutY: Integer;
begin
  Result := Top;
end;

function TFXNode.FXContainer: string;
begin
  if (Parent = nil) or (Parent is TFGUIForm) then
    Result := 'root'
  else
    Result := Parent.Name;
end;

function TFXNode.GetLayoutXCode: string;
begin
  Result := Name + '.setLayoutX(' + IntToStr(PPIUnScale(Left)) + ');';
end;

function TFXNode.GetLayoutYCode: string;
begin
  Result := Name + '.setLayoutY(' + IntToStr(PPIUnScale(Top)) + ');';
end;

function TFXNode.GetWidthCode: string;
begin
  Result := Name + '.setWidth(' + IntToStr(PPIUnScale(Width)) + ');';
end;

function TFXNode.GetHeightCode: string;
begin
  Result := Name + '.setHeight(' + IntToStr(PPIUnScale(Height)) + ');';
end;

procedure TFXNode.SetPositionAndSize;
begin
  if Sizeable then
  begin
    SetAttributValue(Name + '.setLayoutX(', GetLayoutXCode);
    SetAttributValue(Name + '.setLayoutY(', GetLayoutYCode);
  end
  else
  begin
    Width := PPIScale(32);
    Height := PPIScale(28);
  end;
end;

procedure TFXNode.InsertNewVariable(const Variable: string);
begin
  FPartner.InsertAttribute(FXContainer, GetIndentation, Variable, True);
end;

procedure TFXNode.InsertAttributValue(Str: string);
begin
  FPartner.InsertAttribute(FXContainer, GetIndentation, Str, True);
  Exit;

  // ToDo
  if Pos(GetIndentation, Str) = 0 then
    Str := GetIndentation + Str;
  FPartner.InsertAttributValue(Name, Str, 1);
end;

procedure TFXNode.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Background' then
    SetBackgroundAsString(GetAttrColor(Value))
  else if Attr = 'Orientation' then
    MakeOrientation(Value)
  else // ok
    if IsFontAttribute(Attr) then
      MakeFont
    else if Typ = 'string' then
      MakeAttribut(Attr, AsString(Value))
    else if Typ = 'TCursor' then
      MakeCursor(Value)
    else if Typ = 'TColor' then
      MakeColor(Attr, Value)
    else
      MakeAttribut(Attr, Value);
end;

function TFXNode.GetContainerAdd: string;
begin
  var
  Str := FXContainer;
  if Str = '' then
    Result := 'add(' + Name + ');'
  else
    Result := Str + '.getChildren().add(' + Name + ');';
end;

function TFXNode.AddFXVariable: string;
begin
  Result := FXContainer + '.getChildren().add(' + Name + ');';
end;

procedure TFXNode.DefaultComponent;
begin
  SetPositionAndSize;
  FPartner.InsertComponent(Indent2 + AddFXVariable + CrLf);
  FPartner.InsertImport('javafx.scene.control.*');
end;

procedure TFXNode.ChangeAttributValue(const Key: string; Str: string);
begin
  if Pos(Indent2, Str) = 0 then
    Str := Indent2 + Str;
  FPartner.ChangeAttributValue(GetContainerAdd, Key, Str);
end;

procedure TFXNode.SetBackgroundAsString(const AColor: string);
begin
  FPartner.SetFXBackgroundAsString(GetContainerAdd, Name, AColor);
end;

function TFXNode.IsFontAttribute(const Str: string): Boolean;
begin
  Result := Pos(Str, ' Font Name Size Bold Italic') > 0;
end;

procedure TFXNode.MakeFont;
begin
  DoMakeFontWithStyle;
end;

procedure TFXNode.DoMakeFont;
var
  Str, Map: string;
  IntStyle: Integer;
begin
  if (Name = '') or (Tag in [116, 117]) then
    Exit;
  Map := Name + '_map';
  FPartner.DeleteAttributeValues(Map);
  FPartner.DeleteAttributeValue(Name + '.setFont');

  Str := Name + '.setFont(Font.font("' + Font.Name + '"';
  IntStyle := 0;
  if fsBold in Font.Style then
    Inc(IntStyle, 1);
  if fsItalic in Font.Style then
    Inc(IntStyle, 2);
  case IntStyle of
    1:
      Str := Str + ', FontWeight.BOLD';
    2:
      Str := Str + ', FontPosture.ITALIC';
    3:
      Str := Str + ', FontWeight.BOLD, FontPosture.ITALIC';
  end;
  Str := Str + ', ' + IntToStr(Font.Size) + '));';
  SetAttributValue(Name + '.setFont', Str);
  InsertImport('javafx.scene.text.*');
  if fsUnderline in Font.Style then
    MakeAttribut('Underline', 'true')
  else
    MakeAttribut('Underline', '');
  SizeToText;
  InsertImport('javafx.scene.text.Font');
end;

procedure TFXNode.DoMakeFontWithStyle;
begin
  if (Name = '') or (Tag in [115, 116, 117, 124]) then
    Exit;
  FPartner.DeleteAttributeValue(Name + '.setFont');
  var
  Str := Name + '.setStyle("-fx-font-family: ''' + Font.Name + '''; ';
  Str := 'fx-font-family: ''' + Font.Name + '''; ';
  Str := Str + '-fx-font-size: ' + IntToStr(Font.Size) + 'px; ';
  if fsBold in Font.Style then
    Str := Str + '-fx-font-weight: bold; ';
  if fsItalic in Font.Style then
    Str := Str + '-fx-font-style: italic; ';
  if fsUnderline in Font.Style then
    Str := Str + '-fx-text-decoration: underline; ';
  Style := Str;
  Str := Name + '.setStyle("' + Str + '");';
  SetAttributValue(Name + '.setStyle', Str);
  SizeToText;
end;

procedure TFXNode.MakeCursor(const Value: string);
begin
  if Value = 'DEFAULT' then
    MakeAttribut('Cursor', '')
  else
  begin
    InsertImport('javafx.scene.Cursor');
    MakeAttribut('Cursor', 'Cursor.' + Value);
  end;
end;

procedure TFXNode.MakeColor(const Attr, Value: string);
begin
  if Value = '(NONE)' then
    MakeAttribut(Attr, '')
  else
  begin
    InsertImport('javafx.scene.paint.Color');
    MakeAttribut(Attr, GetAttrColor(Value));
  end;
end;

function TFXNode.GetAttrColor(const Value: string): string;
var
  Str, DValue: string;
begin
  DValue := Java2DelphiColors(Value);
  if Copy(DValue, 1, 3) = '$00' then
    Str := 'Color.web("' + TurnRGB('0x' + Copy(DValue, 4, 6)) + '")'
  else if Copy(Value, 1, 2) = '0x' then
    Str := 'Color.web("' + Value + '")'
  else
    Str := 'Color.' + Value;
  if (Value = '(NONE)') or (Value = '(none)') then
    Result := ''
  else
    Result := Str;
end;

procedure TFXNode.CalculateMenus(MenuItems, Menu, ConstructMenu,
  Methods: TStrings; NewMenuBar: Boolean = False);
var
  Indent, Posi, Separator: Integer;
  IndentAsString, TrimS, ShortCut, Str: string;
  MenuName: array [-1 .. 10] of string;

  function hasSubMenu(Items: TStrings; Int: Integer): Boolean;
  begin
    Result := (Int < Items.Count - 1) and
      (LeftSpaces(Items[Int], 2) < LeftSpaces(Items[Int + 1], 2));
  end;

  function getShortCut(Str: string): string;
  begin
    Result := '.setAccelerator(KeyCombination.keyCombination("' + Str + '"));';
  end;

begin
  Separator := 1;
  MenuName[-1] := Name;
  if NewMenuBar then
    ConstructMenu.Add(Indent2 + 'root.getChildren().add(' + Name + ');');
  for var I := 0 to MenuItems.Count - 1 do
  begin
    Str := MenuItems[I];
    Posi := Pos(',', Str);
    if Posi > 0 then
    begin
      ShortCut := getShortCut(Trim(Copy(Str, Posi + 1, Length(Str))));
      Str := Copy(Str, 1, Posi - 1);
    end
    else
      ShortCut := '';
    TrimS := Trim(Str);
    Indent := LeftSpaces(MenuItems[I], 2) div 2;
    IndentAsString := Indent1 + StringOfChar(' ', Indent * 2);
    MenuName[Indent] := MenuName[Indent - 1] + '_' + OnlyCharsAndDigits(TrimS);
    if hasSubMenu(MenuItems, I) then
    begin
      Menu.Add(IndentAsString + 'private Menu ' + MenuName[Indent] +
        ' = new Menu("' + TrimS + '");');
    end
    else
    begin
      if TrimS = '-' then
      begin
        MenuName[Indent] := MenuName[-1] + '_Separator' + IntToStr(Separator);
        Menu.Add(IndentAsString + 'private SeparatorMenuItem ' +
          MenuName[Indent] + ' = new SeparatorMenuItem();');
        Inc(Separator);
      end
      else
      begin
        Menu.Add(IndentAsString + 'private MenuItem ' + MenuName[Indent] +
          ' = new MenuItem("' + TrimS + '");');
        Methods.Add(MenuName[Indent]);
      end;
    end;
    if NewMenuBar and (Indent = 0) then
      Str := '.getMenus().add(' + MenuName[Indent] + ');'
    else
      Str := '.getItems().add(' + MenuName[Indent] + ');';
    ConstructMenu.Add(Indent2 + MenuName[Indent - 1] + Str);
    if ShortCut <> '' then
      ConstructMenu.Add(Indent2 + MenuName[Indent] + ShortCut);
  end;
end;

procedure TFXNode.MakeMenuItems(OldItems, NewItems: TStrings;
  NewMenuBar: Boolean = False);
var
  OldMenu: TStringList;
  OldConstructMenu: TStringList;
  OldMethods: TStringList;
  NewMenu: TStringList;
  NewConstructMenu: TStringList;
  NewMethods: TStringList;

  function getListener(Method: string): string;
  begin
    Result := Indent2 + Method + '.setOnAction(' + CrLf + Indent3 +
      '(event) -> {' + Method + '_Action(event);} ' + CrLf + Indent2 +
      ');' + CrLf;
  end;

begin
  FPartner.Editor.BeginUpdate;
  FPartner.InsertImport('javafx.event.Event');
  FormatItems(NewItems);
  OldMenu := TStringList.Create;
  OldConstructMenu := TStringList.Create;
  OldMethods := TStringList.Create;
  NewMenu := TStringList.Create;
  NewConstructMenu := TStringList.Create;
  NewMethods := TStringList.Create;

  try
    CalculateMenus(OldItems, OldMenu, OldConstructMenu, OldMethods, NewMenuBar);
    CalculateMenus(NewItems, NewMenu, NewConstructMenu, NewMethods, NewMenuBar);
    FPartner.DeleteFXOldAddNewMethods(OldMethods, NewMethods);
    for var I := 0 to OldConstructMenu.Count - 1 do
      FPartner.DeleteAttributeValue(OldConstructMenu[I]);
    for var I := 0 to OldMenu.Count - 1 do
      FPartner.DeleteAttribute(OldMenu[I]);
    for var I := 0 to OldMethods.Count - 1 do
      FPartner.DeleteFXListener(getListener(OldMethods[I]));

    for var I := 0 to NewMenu.Count - 1 do
      InsertAttributValue(NewMenu[I]);
    FPartner.InsertComponent(NewConstructMenu.Text);
    for var I := 0 to NewMethods.Count - 1 do
      FPartner.InsertComponent(getListener(NewMethods[I]));

  finally
    FreeAndNil(OldMenu);
    FreeAndNil(OldConstructMenu);
    FreeAndNil(OldMethods);
    FreeAndNil(NewMenu);
    FreeAndNil(NewConstructMenu);
    FreeAndNil(NewMethods);
    FPartner.Editor.EndUpdate;
  end;
end;

procedure TFXNode.RenameMenu(const OldName, NewName: string);
var
  AttributeS, AttributeE, ComponentS, ComponentE, MethodsS, MethodsE: Integer;
begin
  FPartner.Editor.BeginUpdate;
  AttributeS := FPartner.GetLNGStartAttributes;
  AttributeE := FPartner.GetLNGEndAttributes;

  ComponentS := FPartner.GetLNGStartComponents;
  ComponentE := FPartner.GetLNGEndComponents;

  MethodsS := FPartner.GetLNGStartEventMethods;
  MethodsE := FPartner.GetLNGEndEventMethods;

  // event methods
  FPartner.ReplaceTextWithRegex('public void ' + OldName + '_(.*)\(Event evt\)',
    'public void ' + NewName + '_$1(Event evt)', True, MethodsS, MethodsE);
  FPartner.ReplaceTextWithRegex('// end of ' + OldName + '(.*)',
    '// end of ' + NewName + '$1', True, MethodsS, MethodsE);

  // listener
  FPartner.ReplaceTextWithRegex(OldName + '_(.*).setOnAction\(',
    NewName + '_$1.setOnAction(', True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex(OldName + '_(.*)_Action\(event\)',
    NewName + '_$1_Action(event)', True, ComponentS, ComponentE);

  // declaration of menu variables
  FPartner.ReplaceTextWithRegex('private Menu ' + OldName + '_(.*) = new ',
    'private Menu ' + NewName + '_$1 = new ', True, AttributeS, AttributeE);
  FPartner.ReplaceTextWithRegex('private Image ' + OldName +
    'Graphic = new Image', 'private Image ' + NewName + 'Graphic = new Image',
    True, AttributeS, AttributeE);
  FPartner.ReplaceTextWithRegex('private MenuItem ' + OldName + '_(.*) = new ',
    'private MenuItem ' + NewName + '_$1 = new ', True, AttributeS, AttributeE);
  FPartner.ReplaceTextWithRegex('private SeparatorMenuItem ' + OldName +
    '_(.*) = new ', 'private SeparatorMenuItem ' + NewName + '_$1 = new ', True,
    AttributeS, AttributeE);

  // creation of menu
  FPartner.ReplaceTextWithRegex(OldName + '(.*).getItems',
    NewName + '$1.getItems', True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex('\(' + OldName + '(.*)\);',
    '(' + NewName + '$1);', True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex(OldName + '(.*).setAccelerator',
    NewName + '$1.setAccelerator', True, ComponentS, ComponentE);
end;

procedure TFXNode.DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);
begin
  MenuItemsOld.Text := MenuItems.Text;
  MenuItems.Clear;
  MakeMenuItems(MenuItemsOld, MenuItems);
end;

procedure TFXNode.Paint;
begin
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));
end;

function TFXNode.GetAttributes(ShowAttributes: Integer): string;
begin
  if ShowAttributes = 3 then
    Result := '|Cursor|Disable|Cache|LayoutX|LayoutY|Visible|Style|Name'
  else
    Result := '|Name';
end;

function TFXNode.GetEvents(ShowEvents: Integer): string;
begin
  case ShowEvents of
    1:
      Result := '';
    2:
      Result := KeyboardEvents + MouseEvents;
    3:
      Result := NodeEvents;
  end;
  Result := Result + '|';
end;

function TFXNode.GetListener(const Event: string): string;
begin
  Result := Indent2 + Name + '.setOn' + UpperLower(Event) + '(' + CrLf + Indent3
    + '(event) -> {' + MakeEventProcedureName(Event) + '(event);} ' + CrLf +
    Indent2 + ');' + CrLf;
end;

function TFXNode.EventToEventtype(const Event: string): string;
begin
  if Event = 'action' then
    Result := '' // Action
  else if Event = 'contextMenuRequested' then
    Result := 'ContextMenu'
  else if Event = 'dragDetected' then
    Result := 'Mouse'
  else if Pos('drag', Event) = 1 then
    Result := 'Drag'
  else if Pos('rotat', Event) = 1 then
    Result := 'Rotate'
  else if Pos('scroll', Event) = 1 then
    Result := 'Scroll'
  else if Pos('swipe', Event) = 1 then
    Result := 'Swipe'
  else if Pos('swipe', Event) = 1 then
    Result := 'Swipe'
  else if Pos('zoom', Event) = 1 then
    Result := 'Zoom'
  else if Event = 'inputMethodTextChanged' then
    Result := 'InputMethod'
  else if Pos('key', Event) = 1 then
    Result := 'Key'
  else if (Pos('mouseDrag', Event) = 1) and (Event <> 'mouseDragged') then
    Result := 'MouseDrag'
  else if Pos('mouse', Event) = 1 then
    Result := 'Mouse'
  else if Pos('touch', Event) = 1 then
    Result := 'Touch'
  else if Pos('window', Event) = 1 then
    Result := 'Window';
  Result := Result + 'Event';
end;

function TFXNode.MakeEventProcedure(const Event: string): string;
var
  Str: string;
begin
  // Example:
  // public void button1_Action(Action event) {
  // // TODO add your code here
  // }

  Str := MakeEventProcedureName(Event);
  Result := Indent1 + 'public void ' + Str + '(';
  Result := Result + EventToEventtype(Event);
  Result := Result + ' evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf +
    Indent2 + CrLf;
  Result := Result + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result := Result + ' // end of ' + Str;
  Result := Result + CrLf + CrLf;
end;

procedure TFXNode.DeleteListener(const Event: string);
var
  EventMethod, Listener: string;
begin
  EventMethod := MakeEventProcedureName(Event);
  FPartner.DeleteMethod(EventMethod);
  Listener := GetListener(Event);
  FPartner.DeleteFXListener(Listener);
end;

procedure TFXNode.Rename(const OldName, NewName, Events: string);

  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;

  Rename(FcontextMenuRequested);
  Rename(FDragDetected);
  Rename(FdragDone);
  Rename(FdragDropped);
  Rename(FdragEntered);
  Rename(FdragExited);
  Rename(FdragOver);

  Rename(FinputMethodTextChanged);
  Rename(FkeyPressed);
  Rename(FkeyReleased);
  Rename(FkeyTyped);
  Rename(FmouseClicked);
  Rename(FmouseDragged);
  Rename(FmouseEntered);
  Rename(FmouseExited);
  Rename(FmouseMoved);
  Rename(FmousePressed);
  Rename(FmouseReleased);

  Rename(Frotate);
  Rename(FrotationFinished);
  Rename(FrotationStarted);
  Rename(Fscroll);
  Rename(FscrollFinished);
  Rename(FscrollStarted);
  Rename(FswipeDown);
  Rename(FswipeLeft);
  Rename(FswipeRight);
  Rename(FswipeUp);
  Rename(Fzoom);
  Rename(FzoomFinished);
  Rename(FzoomStarted);

  Rename(FmouseDragEntered);
  Rename(FmouseDragExited);
  Rename(FmouseDragOver);
  Rename(FmouseDragReleased);
  Rename(FtouchMoved);
  Rename(FtouchPressed);
  Rename(FtouchReleased);
  Rename(FtouchStationary);
end;

function TFXNode.SurroundFix(const Str: string): string;
begin
  Result := Indent1 + Str + #13#10;
end;

function TFXNode.SurroundFix2(const Str: string): string;
begin
  Result := Indent2 + Str + #13#10;
end;

procedure TFXNode.MakeOrientation(const Value: string);
begin
  FPartner.InsertImport('javafx.geometry.Orientation');
  MakeAttribut('Orientation', 'Orientation.' + Value);
  SetPositionAndSize;
end;

procedure TFXNode.InsertImport(const Str: string);
begin
  FPartner.InsertImport(Str);
end;

procedure TFXNode.MakeGraphic(const Attr, Value, Typ: string);
var
  Str, Key, AtPos, Dest, Filename, Path: string;
begin
  if (Value = '(Graphic)') or (Value = '(Icon)') then
  begin
    Key := 'private Image ' + Name + Attr;
    FPartner.DeleteAttribute(Key);
    FPartner.DeleteAttributeValue(Name + '.set' + Attr + '(');
  end
  else
  begin
    InsertImport('javafx.scene.image.*');
    Filename := ExtractFileName(Value);
    if Pos('images/', Filename) = 1 then
      System.Delete(Filename, 1, 7);
    Path := ExtractFilePath(FPartner.Pathname);
    Dest := Path + 'images\' + Filename;
    ForceDirectories(Path + 'images\');
    if not FileExists(Dest) then
      CopyFile(PChar(Value), PChar(Dest), True);
    Filename := 'images/' + Filename;
    FObjectInspector.ELPropertyInspector.SetByCaption(Attr, Filename);

    AtPos := 'private ' + Typ + ' ' + Name;
    Key := 'private Image ' + Name + Attr;
    Str := Indent2 + Key + ' = new Image(getClass().getResourceAsStream("' +
      Filename + '"));';
    FPartner.ReplaceAttributAt(AtPos, Key, Str);

    if Attr = 'Graphic' then
      MakeAttribut('Graphic', 'new ImageView(' + Name + 'Graphic)')
    else
      MakeAttribut('Image', Name + 'Image');
  end;
end;

{ --- TFXRegion ---------------------------------------------------------------- }

constructor TFXRegion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackground := FDefaultBackground;
end;

procedure TFXRegion.SetBackground(AColor: TColor);
begin
  if AColor <> FBackground then
  begin
    FBackground := AColor;
    Invalidate;
  end;
end;

function TFXRegion.GetPrefHeightCode: string;
begin
  Result := Name + '.setPrefHeight(' + IntToStr(PPIUnScale(Height)) + ');';
end;

function TFXRegion.GetPrefWidthCode: string;
begin
  Result := Name + '.setPrefWidth(' + IntToStr(PPIUnScale(Width)) + ');';
end;

procedure TFXRegion.SetPrefWidth(AValue: Integer);
begin
  if AValue <> FPrefWidth then
  begin
    FPrefWidth := AValue;
    Width := FPrefWidth;
    Invalidate;
  end;
end;

function TFXRegion.GetPrefWidth: Integer;
begin
  Result := Width;
end;

procedure TFXRegion.SetPrefHeight(AValue: Integer);
begin
  if AValue <> FPrefHeight then
  begin
    FPrefHeight := AValue;
    Height := FPrefHeight;
    Invalidate;
  end;
end;

function TFXRegion.GetPrefHeight: Integer;
begin
  Result := Height;
end;

procedure TFXRegion.SetPositionAndSize;
begin
  SetAttributValue(Name + '.setLayoutX(', GetLayoutXCode);
  SetAttributValue(Name + '.setLayoutY(', GetLayoutYCode);
  SetAttributValue(Name + '.setPrefHeight(', GetPrefHeightCode);
  SetAttributValue(Name + '.setPrefWidth(', GetPrefWidthCode);
end;

function TFXRegion.GetAttributes(ShowAttributes: Integer): string;
begin
  if ShowAttributes = 3 then
    Result := '|Background|PrefHeight|PrefWidth|SnapToPixel' +
      inherited GetAttributes(3)
  else
    Result := '|Name';
end;

{ --- TFXControl --------------------------------------------------------------- }

function TFXControl.GetAttributes(ShowAttributes: Integer): string;
begin
  if ShowAttributes = 3 then
    Result := '|Tooltip|ContextMenu' + inherited GetAttributes(3)
  else
    Result := inherited GetAttributes(ShowAttributes);
end;

procedure TFXControl.SetAttribute(Attr, Value, Typ: string);
begin
  if Typ = 'TAlignment' then
  begin // wieso hier?
    InsertImport('javafx.geometry.*');
    MakeAttribut('Alignment', 'Pos.' + Value);
  end
  else if Attr = 'Tooltip' then
    MakeAttribut(Attr, 'new Tooltip(' + AsString(Value) + ')')
  else if Attr = 'ContextMenu' then
    MakeAttribut(Attr, Value)
  else
    inherited;
end;

end.
