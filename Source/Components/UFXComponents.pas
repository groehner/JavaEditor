unit UFXComponents;

{ Classes
    TFXNode = class (TJEComponent)
      TFXRegion
        TFXControl
}

interface

uses
  Windows, Classes, Graphics, Controls, UJEComponents;

const
  CrLf = #13#10;
  CornerRadius = 5;
  ColorNone = clBtnFace;

  DragDropEvents = '|dragDetected|dragDone|dragDropped|dragEntered|dragExited'+
                   '|dragOver|mouseDragEntered|mouseDragExited|mouseDragOver|mouseDragReleased';
  RotationEvents = '|rotate|rotationFinished|rotationStarted';
  ScrollEvents = '|scroll|scrollFinished|scrollStarted';
  SwipeEvents =  '|swipeDown|swipeLeft|swipeRight|swipeUp';
  ZoomEvents =   '|zoom|zoomFinished|zoomStarted';
  GestureEvents = RotationEvents + ScrollEvents + SwipeEvents + ZoomEvents;
  KeyboardEvents = '|inputMethodTextChanged|keyPressed|keyReleased|keyTyped';
  MouseEvents = '|contextMenuRequested|mouseClicked|mouseDragged|mouseEntered' +
                '|mouseExited|mouseMoved|mousePressed|mouseReleased';
  TouchEvents = '|touchMoved|touchPressed|touchReleased|touchStationary';
  NodeEvents =  DragDropEvents + GestureEvents + KeyboardEvents + MouseEvents + TouchEvents;

type

  // used in TextInput and Labeled
  TAlignment = (BASELINE_CENTER, BASELINE_LEFT, BASELINE_RIGHT,
                TOP_CENTER,      TOP_LEFT,      TOP_RIGHT,
                CENTER,          CENTER_LEFT,   CENTER_RIGHT,
                BOTTOM_CENTER,   BOTTOM_LEFT,   BOTTOM_RIGHT);

  // used in Text and Labeled
  TTextAlignment = (_TA_CENTER, _TA_JUSTIFY, _TA_LEFT, _TA_RIGHT);

  TFXNode = class (TJEComponent)   // Node
  private
    FDarkShadow: TColor;
    FDefaultBackground: TColor;
    FDefaultForeground: TColor;
    FBlueColor: TColor;
    FDefaultBorderColor: TColor;

    FDisable: boolean;
    FCache: boolean;
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

    procedure setX(aValue: integer);
    function getX: integer;
    procedure setY(aValue: integer);
    function getY: integer;
    procedure MakeOrientation(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFromJ(Control: TControl);
    function FXContainer: string;
    function getLayoutXCode: string;
    function getLayoutYCode: string;
    function getWidthCode: string;
    function getHeightCode: string;
    procedure SetPositionAndSize; override;
    procedure InsertNewVariable(const Variable: string);
    procedure InsertAttributValue(s: string);
    function GetContainerAdd: string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure ChangeAttributValue(const key: string; s: string);
    function AddFXVariable: string;
    procedure DefaultComponent;
    procedure SetBackgroundAsString(const aColor: string);
    function isFontAttribute(const s: string): boolean;
    procedure MakeFont; override;
    procedure MakeCursor(const Value: string);
    procedure MakeColor(const Attr, Value: string);
    function getAttrColor(const Value: string): string;
    procedure CalculateMenus(MenuItems, Menu, ConstructMenu, Methods: TStrings; newMenuBar: boolean = false);
    procedure MakeMenuItems(OldItems, NewItems: TStrings; newMenuBar: boolean = false);
    procedure RenameMenu(const OldName, NewName: string);
    procedure DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);
    procedure Paint; override;
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    function getListener(const event: string): string; override;
    procedure DeleteListener(const event: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    function surroundFix(s: string): string;
    function surroundFix2(s: string): string;
    function MakeEventProcedure(const event: string ): string; override;
    function EventToEventtype(const event: string): string;
    procedure InsertImport(const s: string);
    procedure MakeGraphic(const Attr, Value, Typ: string);

    property DefaultBackground: TColor read FDefaultBackground;
    property DefaultForeground: TColor read FDefaultForeground;
    property DarkShadow: TColor read FDarkShadow;
    property DefaultBorderColor: TColor read FDefaultBorderColor;
    property BlueColor: TColor read FBlueColor;

  published
    property LayoutX: integer read getX write setX;
    property LayoutY: integer read getY write setY;
    property Disable: boolean read FDisable write FDisable;
    property Cache: boolean read FCache write FCache;
    property Style: string read FStyle write FStyle;
    property Visible;

    property contextMenuRequested: string read FcontextMenuRequested write FcontextMenuRequested;
    property dragDone: string read FdragDone write FdragDone;
    property dragDropped: string read FdragDropped write FdragDropped;
    property dragEntered: string read FdragEntered write FdragEntered;
    property dragExited: string read FdragExited write FdragExited;
    {$WARNINGS OFF}
    property dragOver: string read FdragOver write FdragOver;
    {$WARNINGS ON}

    property rotate: string read Frotate write Frotate;
    property rotationFinished: string read FrotationFinished write FrotationFinished;
    property rotationStarted: string read FrotationStarted write FrotationStarted;
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

    property inputMethodTextChanged: string read FinputMethodTextChanged write FinputMethodTextChanged;
    property keyPressed: string read FkeyPressed write FkeyPressed;
    property keyReleased: string read FkeyReleased write FkeyReleased;
    property keyTyped: string read FkeyTyped write FkeyTyped;
    property dragDetected: string read FdragDetected write FdragDetected;
    property mouseClicked: string read FmouseClicked write FmouseClicked;
    property mouseDragged: string read FmouseDragged write FmouseDragged;
    property mouseEntered: string read FmouseEntered write FmouseEntered;
    property mouseExited: string read FmouseExited write FmouseExited;
    property mousePressed: string read FmousePressed write FmousePressed;
    property mouseReleased: string read FmouseReleased write FmouseReleased;
    property mouseMoved: string read FmouseMoved write FmouseMoved;

    property mouseDragEntered: string read FmouseDragEntered write FmouseDragEntered;
    property mouseDragExited: string read FmouseDragExited write FmouseDragExited;
    property mouseDragOver: string read FmouseDragOver write FmouseDragOver;
    property mouseDragReleased: string read FmouseDragReleased write FmouseDragReleased;
    property touchMoved: string read FtouchMoved write FtouchMoved;
    property touchPressed: string read FtouchPressed write FtouchPressed;
    property touchReleased: string read FtouchReleased write FtouchReleased;
    property touchStationary: string read FtouchStationary write FtouchStationary;
  end;

  TFXRegion = class(TFXNode)
  private
    FPrefHeight: integer;
    FPrefWidth: integer;
    FBackground: TColor;
    FSnapToPixel: boolean;
    procedure SetBackground(aColor: TColor);
    procedure SetPrefWidth(aValue: integer);
    function getPrefWidth: integer;
    procedure SetPrefHeight(aValue: integer);
    function getPrefHeight: integer;
    function getPrefHeightCode: string;
    function getPrefWidthCode: string;
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure SetPositionAndSize; override;
  published
    property PrefHeight: integer read getPrefHeight write setPrefHeight;
    property PrefWidth: integer read getPrefWidth write setPrefWidth;
    property Background: TColor read FBackground write setBackground;
    property SnapToPixel: boolean read FSnapToPixel write FSnapToPixel;
  end;

  TFXControl = class(TFXRegion)
  private
    FTooltip: string;
    FContextMenu: string;
  public
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property Tooltip: string read FTooltip write FTooltip;
    property ContextMenu: string read FContextMenu write FContextMenu;
  end;

implementation

uses SysUtils, UBaseForm, UEditorForm, UImages, UUtils, TypInfo, JvGnugettext,
     UStringRessources, UJava,
     UObjectInspector, UGuiForm, UConfiguration, ULink, UITypes;

{--- TFXNode ------------------------------------------------------------------}

constructor TFXNode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultBackground:= RGB(244, 244, 244);        // F4F4F4
  FDefaultForeground:= RGB(51, 51, 51);           // 333333  FOREGROUND
  FDarkShadow := RGB(122, 138, 153);              // border color
  FBlueColor:= RGB(99, 130, 191);
  FDefaultBorderColor:= $C8C8C8;
  if (AOwner is TFGuiForm) then
    Partner:= (AOwner as TFGuiForm).Partner as TFEditForm;
  Width:= 120;
  Height:= 80;
  Sizeable:= true;
  HelpType:= htContext;
  Font.Name:= FConfiguration.GUIFontName; // Dialog
  Font.Size:= FConfiguration.GUIFontSize;
  Font.Style:= [];
end;

procedure TFXNode.CreateFromJ(Control: TControl);
begin
  Tag:= Control.Tag;
  Visible:= Control.Visible;
  SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
end;

procedure TFXNode.setX(aValue: integer);
begin
  if aValue <> Top then begin
    Left:= aValue;
    Invalidate;
  end;
end;

function TFXNode.getX: integer;
begin
  Result:= Left;
end;

procedure TFXNode.setY(aValue: integer);
begin
  if aValue <> Top then begin
    Top:= aValue;
    Invalidate;
  end;
end;

function TFXNode.getY: integer;
begin
  Result:= Top;
end;

function TFXNode.FXContainer: string;
begin
  if (Parent = nil) or (Parent is TFGUIForm)
    then Result:= 'root'
    else Result:= Parent.Name;
end;

function TFXNode.getLayoutXCode: string;
begin
  Result:= Name + '.setLayoutX(' + IntToStr(PPIUnScale(Left)) + ');';
end;

function TFXNode.getLayoutYCode: string;
begin
  Result:= Name + '.setLayoutY(' + IntToStr(PPIUnScale(Top)) + ');';
end;

function TFXNode.getWidthCode: string;
begin
  Result:= Name + '.setWidth(' + IntToStr(PPIUnScale(Width)) + ');';
end;

function TFXNode.getHeightCode: string;
begin
  Result:= Name + '.setHeight(' + IntToStr(PPIUnScale(Height)) + ');';
end;

procedure TFXNode.SetPositionAndSize;
begin
  if Sizeable then begin
    setAttributValue(Name + '.setLayoutX(', getLayoutXCode);
    setAttributValue(Name + '.setLayoutY(', getLayoutYCode);
  end else begin
    Width:= PPIScale(32);
    Height:= PPIScale(28);
  end;
end;

procedure TFXNode.InsertNewVariable(const Variable: string);
begin
  Partner.InsertAttribute(FXContainer, GetIndentation, Variable, true);
end;

procedure TFXNode.InsertAttributValue(s: string);
begin
  Partner.InsertAttribute(FXContainer, GetIndentation, s, true);
  exit;

  // ToDo
  if Pos(GetIndentation, s) = 0 then
    s:= GetIndentation + s;
  Partner.insertAttributValue(Name, s, 1);
end;

procedure TFXNode.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Background'   then SetBackgroundAsString(getAttrColor(Value)) else
  if Attr = 'Orientation'  then MakeOrientation(Value) else   // ok
  if isFontAttribute(Attr) then MakeFont else
  if Typ = 'string'  then MakeAttribut(Attr, asString(Value)) else
  if Typ = 'TCursor' then MakeCursor(Value) else
  if Typ = 'TColor'  then MakeColor(Attr, Value) else
    MakeAttribut(Attr, Value);
end;

function TFXNode.GetContainerAdd: string;
begin
  var s:= FXContainer;
  if s = ''
    then Result:= 'add(' + name + ');'
    else Result:= s + '.getChildren().add(' + name + ');';
end;

function TFXNode.AddFXVariable: string;
begin
  Result:= FXContainer + '.getChildren().add(' + Name + ');'
end;

procedure TFXNode.DefaultComponent;
begin
  SetPositionAndSize;
  Partner.InsertComponent(Indent2 + AddFXVariable + CrLf);
  Partner.InsertImport('javafx.scene.control.*');
end;

procedure TFXNode.ChangeAttributValue(const key: string; s: string);
begin
  if Pos(Indent2, s) = 0 then
    s:= Indent2 + s;
  Partner.ChangeAttributValue(GetContainerAdd, key, s);
end;

procedure TFXNode.SetBackgroundAsString(const aColor: string);
begin
  Partner.SetFXBackgroundAsString(GetContainerAdd, Name, aColor);
end;

function TFXNode.isFontAttribute(const s: string): boolean;
begin
  Result:= Pos(s, ' Font Name Size Bold Italic') > 0;
end;

procedure TFXNode.MakeFont;
  var s, map: string; intstyle: integer;
begin
  if Name = '' then exit;

  map:= Name + '_map';
  Partner.DeleteAttributeValues(map);
  Partner.DeleteAttributeValue(Name + '.setFont');

  s:= Name + '.setFont(Font.font("' + Font.Name + '"';
  intstyle:= 0;
  if fsBold   in Font.Style then inc(intstyle, 1);
  if fsItalic in Font.Style then inc(intstyle, 2);
  case intstyle of
    1: s:= s + ', FontWeight.BOLD';
    2: s:= s + ', FontPosture.ITALIC';
    3: s:= s + ', FontWeight.BOLD, FontPosture.ITALIC';
  end;
  s:= s + ', ' + IntToStr(Font.Size) + '));';
  setAttributValue(Name + '.setFont', s);
  InsertImport('javafx.scene.text.*');
  if fsUnderline in Font.Style
    then MakeAttribut('Underline', 'true')
    else MakeAttribut('Underline', '');
  SizeToText;
  InsertImport('javafx.scene.text.Font');
end;

procedure TFXNode.MakeCursor(const Value: string);
begin
  if Value = 'DEFAULT' then
    MakeAttribut('Cursor', '')
  else begin
    InsertImport('javafx.scene.Cursor');
    MakeAttribut('Cursor', 'Cursor.' + Value);
  end;
end;

procedure TFXNode.MakeColor(const Attr, Value: string);
begin
  if Value = '(NONE)' then
    MakeAttribut(Attr, '')
  else begin
    InsertImport('javafx.scene.paint.Color');
    MakeAttribut(Attr, getAttrColor(Value));
  end;
end;

function TFXNode.getAttrColor(const Value: string): string;
  var s, DValue: string;
begin
  DValue:= Java2DelphiColors(Value);
  if copy(DValue, 1, 3) = '$00' then
     s:= 'Color.web("' + turnRGB('0x' + copy(DValue,4, 6)) + '")'
  else if copy(Value, 1, 2) = '0x'
    then s:= 'Color.web("' + Value + '")'
    else s:= 'Color.' + Value;
  if (Value = '(NONE)') or (Value = '(none)')
    then Result:= ''
    else Result:= s;
end;

procedure TFXNode.CalculateMenus(MenuItems, Menu, ConstructMenu, Methods: TStrings;
                                 newMenuBar: boolean = false);
  var i, Indent, p, Separator: integer;
      IndentAsstring, ts, ShortCut, s: string;
      MenuName: array[-1..10] of string;

  function hasSubMenu(Items: TStrings; i: integer): boolean;
  begin
    Result:= (i < Items.count - 1) and
             (LeftSpaces(Items[i], 2) < LeftSpaces(Items[i+1], 2));
  end;

  function getShortCut(s: string): string;
  begin
    Result:= '.setAccelerator(KeyCombination.keyCombination("' + s + '"));';
  end;

begin
  Separator:= 1;
  MenuName[-1]:= Name;
  if newMenuBar then
    ConstructMenu.add(Indent2 + 'root.getChildren().add(' + Name + ');');
  for i:= 0 to MenuItems.Count - 1 do begin
    s:= MenuItems[i];
    p:= Pos(',', s);
    if p > 0 then begin
      ShortCut:= getShortcut(trim(copy(s, p+1, length(s))));
      s:= copy(s, 1, p-1);
    end else
      Shortcut:= '';
    ts:= trim(s);
    Indent:= LeftSpaces(MenuItems[i], 2) div 2;
    IndentAsString:= Indent1 + StringOfChar(' ', Indent*2);
    MenuName[Indent]:= MenuName[Indent-1] + '_' + OnlyCharsAndDigits(ts);
    if hasSubMenu(MenuItems, i) then begin
      Menu.Add(IndentAsString + 'private Menu ' + MenuName[Indent] +
               ' = new Menu("' + ts + '");');
    end else begin
      if ts = '-' then begin
        MenuName[Indent]:= MenuName[-1] + '_Separator' + IntToStr(Separator);
        Menu.Add(IndentAsString + 'private SeparatorMenuItem ' + MenuName[Indent] +
                 ' = new SeparatorMenuItem();');
        inc(Separator);
      end else begin
        Menu.Add(IndentAsString + 'private MenuItem ' + MenuName[Indent] +
                 ' = new MenuItem("' + ts + '");');
        Methods.Add(MenuName[Indent]);
      end;
    end;
    if newMenuBar and (Indent = 0)
      then s:= '.getMenus().add(' + MenuName[Indent] + ');'
      else s:= '.getItems().add(' + MenuName[Indent] + ');';
    ConstructMenu.Add(Indent2 + MenuName[Indent-1] + s);
    if Shortcut <> '' then
      ConstructMenu.Add(Indent2 + MenuName[Indent] + ShortCut);
  end;
end;

procedure TFXNode.MakeMenuItems(OldItems, NewItems: TStrings; newMenuBar: boolean = false);
  var i: integer;
      OldMenu: TStringList;
      OldConstructMenu: TStringList;
      OldMethods: TStringList;
      NewMenu: TStringList;
      NewConstructMenu: TStringList;
      NewMethods: TStringList;

  function getListener(Method: string): string;
  begin
    Result:= Indent2 + Method + '.setOnAction(' + CrLf +
             Indent3 + '(event) -> {' + Method + '_Action(event);} ' + CrLf +
             Indent2 + ');' + CrLf;
  end;

begin
  Partner.Editor.BeginUpdate;
  Partner.insertImport('javafx.event.Event');
  FormatItems(NewItems);
  OldMenu:= TStringList.Create;
  OldConstructMenu:= TStringList.Create;
  OldMethods:= TStringList.Create;
  NewMenu:= TStringList.Create;
  NewConstructMenu:= TStringList.Create;
  NewMethods:= TStringList.Create;

  try
    CalculateMenus(OldItems, OldMenu, OldConstructMenu, OldMethods, newMenuBar);
    CalculateMenus(NewItems, NewMenu, NewConstructMenu, NewMethods, newMenuBar);
    Partner.DeleteFXOldAddNewMethods(OldMethods, NewMethods);
    for i:= 0 to OldConstructMenu.Count - 1 do
      Partner.DeleteAttributeValue(OldConstructMenu[i]);
    for i:= 0 to OldMenu.Count - 1 do
      Partner.DeleteAttribute(OldMenu[i]);
    for i:= 0 to OldMethods.Count - 1 do
      Partner.deleteFXListener(getListener(OldMethods[i]));

    for i:= 0 to NewMenu.Count -1 do
      InsertAttributValue(NewMenu[i]);
    Partner.InsertComponent(NewConstructMenu.Text);
    for i:= 0 to NewMethods.Count - 1 do
      Partner.InsertComponent(getListener(NewMethods[i]));

  finally
    FreeAndNil(OldMenu);
    FreeAndNil(OldConstructMenu);
    FreeAndNil(OldMethods);
    FreeAndNil(NewMenu);
    FreeAndNil(NewConstructMenu);
    FreeAndNil(NewMethods);
    Partner.Editor.EndUpdate;
  end;
end;

procedure TFXNode.RenameMenu(const OldName, NewName: string);
  var AttributeS, AttributeE, ComponentS, ComponentE, MethodsS, MethodsE: integer;
begin
  Partner.Editor.BeginUpdate;
  AttributeS:= Partner.getLNGStartAttributes;
  AttributeE:= Partner.getLNGEndAttributes;

  ComponentS:= Partner.getLNGStartComponents;
  ComponentE:= Partner.getLNGEndComponents;

  MethodsS:= Partner.getLNGStartEventMethods;
  MethodsE:= Partner.getLNGEndEventMethods;

  // event methods
  Partner.ReplaceTextWithRegex('public void ' + OldName + '_(.*)\(Event evt\)',
                               'public void ' + NewName + '_$1(Event evt)',
                               true, MethodsS, MethodsE);
  Partner.ReplaceTextWithRegex('// end of ' + OldName + '(.*)',
                               '// end of ' + NewName + '$1',
                               true, MethodsS, MethodsE);

  // listener
  Partner.ReplaceTextWithRegex(OldName + '_(.*).setOnAction\(',
                               NewName + '_$1.setOnAction(',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex(OldName + '_(.*)_Action\(event\)',
                               NewName + '_$1_Action(event)',
                               true, ComponentS, ComponentE);

  // declaration of menu variables
  Partner.ReplaceTextWithRegex('private Menu ' + OldName + '_(.*) = new ',
                               'private Menu ' + NewName + '_$1 = new ',
                               true, AttributeS, AttributeE);
  Partner.ReplaceTextWithRegex('private Image ' + OldName + 'Graphic = new Image',
                               'private Image ' + NewName + 'Graphic = new Image',
                               true, AttributeS, AttributeE);
  Partner.ReplaceTextWithRegex('private MenuItem ' + OldName + '_(.*) = new ',
                               'private MenuItem ' + NewName + '_$1 = new ',
                               true, AttributeS, AttributeE);
  Partner.ReplaceTextWithRegex('private SeparatorMenuItem ' + OldName + '_(.*) = new ',
                               'private SeparatorMenuItem ' + NewName + '_$1 = new ',
                               true, AttributeS, AttributeE);

  // creation of menu
  Partner.ReplaceTextWithRegex(OldName + '(.*).getItems', NewName + '$1.getItems',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex('\(' + OldName + '(.*)\);', '(' + NewName + '$1);',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex(OldName + '(.*).setAccelerator', NewName + '$1.setAccelerator',
                               true, ComponentS, ComponentE);
end;

procedure TFXNode.DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);
begin
  MenuItemsOld.Text:= MenuItems.Text;
  MenuItems.Clear;
  MakeMenuItems(MenuItemsOld, MenuItems);
end;

procedure TFXNode.Paint;
begin
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));
end;

function TFXNode.getAttributes(ShowAttributes: integer): string;
begin
  if ShowAttributes = 3
    then Result:= '|Cursor|Disable|Cache|LayoutX|LayoutY|Visible|Style|Name'
    else Result:= '|Name';
end;

function TFXNode.getEvents(ShowEvents: integer): string;
begin
  case ShowEvents of
    1: Result:= '';
    2: Result:= KeyboardEvents + MouseEvents;
    3: Result:= NodeEvents;
  end;
  Result:= Result + '|';
end;

function TFXNode.getListener(const event: string): string;
begin
  Result:= Indent2 + Name + '.setOn' + UpperLower(Event) + '(' + CrLf +
           Indent3 + '(event) -> {' + MakeEventProcedureName(Event) + '(event);} ' + CrLf +
           Indent2 + ');' + CrLf;
end;

function TFXNode.EventToEventtype(const event: string): string;
begin
  if      Event = 'action' then  Result:= ''  // Action
  else if Event = 'contextMenuRequested' then     Result:= 'ContextMenu'
  else if Event = 'dragDetected' then Result:=  'Mouse'
  else if Pos('drag', Event) = 1 then Result:=  'Drag'
  else if Pos('rotat', Event) = 1 then Result:=  'Rotate'
  else if Pos('scroll', Event) = 1 then Result:=  'Scroll'
  else if Pos('swipe', Event) = 1 then Result:=  'Swipe'
  else if Pos('swipe', Event) = 1 then Result:=  'Swipe'
  else if Pos('zoom', Event) = 1 then Result:=  'Zoom'
  else if Event = 'inputMethodTextChanged' then Result:=  'InputMethod'
  else if Pos('key', Event) = 1 then Result:=  'Key'
  else if (Pos('mouseDrag', Event) = 1) and (Event <> 'mouseDragged') then Result:=  'MouseDrag'
  else if Pos('mouse', Event) = 1 then Result:=  'Mouse'
  else if Pos('touch', Event) = 1 then Result:=  'Touch'
  else if Pos('window', Event) = 1 then Result:=  'Window';
  Result:= Result + 'Event';
end;

function TFXNode.MakeEventProcedure(const event: string ): string;
  var s: string;
begin
  //  Example:
  //  public void button1_Action(Action event) {
  //    // TODO add your code here
  //  }

  s:= MakeEventProcedureName(Event);
  Result:= Indent1 + 'public void ' + s + '(';
  Result:= Result + EventToEventtype(Event);
  Result:= Result + ' evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf + Indent2 + CrLf;
  Result:= Result + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result:= Result + ' // end of ' + s;
  Result:= Result + CrLf + CrLf;
end;

procedure TFXNode.DeleteListener(const event: string);
  var EventMethod, Listener: string;
begin
  EventMethod:= MakeEventProcedureName(Event);
  Partner.DeleteMethod(EventMethod);
  Listener:= getListener(Event);
  Partner.DeleteFXListener(Listener);
end;

procedure TFXNode.Rename(const OldName, NewName, Events: string);

  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;

  rename(FcontextMenuRequested);
  rename(FdragDetected);
  rename(FdragDone);
  rename(FdragDropped);
  rename(FdragEntered);
  rename(FdragExited);
  rename(FdragOver);

  rename(FinputMethodTextChanged);
  rename(FkeyPressed);
  rename(FkeyReleased);
  rename(FkeyTyped);
  rename(FmouseClicked);
  rename(FmouseDragged);
  rename(FmouseEntered);
  rename(FmouseExited);
  rename(FmouseMoved);
  rename(FmousePressed);
  rename(FmouseReleased);

  rename(Frotate);
  rename(FrotationFinished);
  rename(FrotationStarted);
  rename(Fscroll);
  rename(FscrollFinished);
  rename(FscrollStarted);
  rename(FswipeDown);
  rename(FswipeLeft);
  rename(FswipeRight);
  rename(FswipeUp);
  rename(Fzoom);
  rename(FzoomFinished);
  rename(FzoomStarted);

  rename(FmouseDragEntered);
  rename(FmouseDragExited);
  rename(FmouseDragOver);
  rename(FmouseDragReleased);
  rename(FtouchMoved);
  rename(FtouchPressed);
  rename(FtouchReleased);
  rename(FtouchStationary);
end;

function TFXNode.surroundFix(s: string): string;
begin
  Result:= Indent1 + s + #13#10;
end;

function TFXNode.surroundFix2(s: string): string;
begin
  Result:= Indent2 + s + #13#10;
end;

procedure TFXNode.MakeOrientation(const Value: string);
begin
  Partner.InsertImport('javafx.geometry.Orientation');
  MakeAttribut('Orientation', 'Orientation.' + Value);
  setPositionAndSize;
end;

procedure TFXNode.InsertImport(const s: string);
begin
  Partner.InsertImport(s);
end;

procedure TFXNode.MakeGraphic(const Attr, Value, Typ: string);
  var s, key, at, Dest, filename, Path: string;
begin
  if (Value = '(Graphic)') or (Value = '(Icon)') then begin
    key:= 'private Image ' + Name + Attr;
    Partner.DeleteAttribute(key);
    Partner.DeleteAttributeValue(Name + '.set' + Attr + '(');
  end else begin
    InsertImport('javafx.scene.image.*');
    filename:= ExtractFileName(Value);
    if Pos('images/', filename) = 1 then
      System.delete(filename, 1, 7);
    Path:= ExtractFilePath(Partner.Pathname);
    Dest:= Path + 'images\' + filename;
    ForceDirectories(Path + 'images\');
    if not FileExists(Dest) then
      copyFile(PChar(Value), PChar(Dest), true);
    filename:= 'images/' + filename;
    FObjectInspector.ELPropertyInspector.SetByCaption(Attr, filename);

    at:= 'private ' + Typ + ' ' + Name;
    key:= 'private Image ' + Name + Attr;
    s:= Indent2 + Key + ' = new Image(getClass().getResourceAsStream("' + filename + '"));';
    Partner.ReplaceAttributAt(at, key, s);

    if Attr = 'Graphic'
      then MakeAttribut('Graphic', 'new ImageView(' + Name + 'Graphic)')
      else MakeAttribut('Image', Name + 'Image');
  end;
end;

{--- TFXRegion ----------------------------------------------------------------}

constructor TFXRegion.Create(AOwner: TComponent);
begin
  inherited create(aOwner);
  FBackground:= FDefaultBackground;
end;

procedure TFXRegion.SetBackground(aColor: TColor);
begin
  if aColor <> FBackground then begin
    FBackground:= aColor;
    Invalidate;
  end;
end;

function TFXRegion.getPrefHeightCode: string;
begin
  Result:= Name + '.setPrefHeight(' + IntToStr(PPIUnScale(Height)) + ');';
end;

function TFXRegion.getPrefWidthCode: string;
begin
  Result:= Name + '.setPrefWidth(' + IntToStr(PPIUnScale(Width)) + ');';
end;

procedure TFXRegion.SetPrefWidth(aValue: integer);
begin
  if aValue <> FPrefWidth then begin
    FPrefWidth:= aValue;
    Width:= FPrefWidth;
    Invalidate;
  end;
end;

function TFXRegion.getPrefWidth: integer;
begin
  Result:= Width;
end;

procedure TFXRegion.SetPrefHeight(aValue: integer);
begin
  if aValue <> FPrefHeight then begin
    FPrefHeight:= aValue;
    Height:= FPrefHeight;
    Invalidate;
  end;
end;

function TFXRegion.getPrefHeight: integer;
begin
  Result:= Height;
end;

procedure TFXRegion.SetPositionAndSize;
begin
  setAttributValue(Name + '.setLayoutX(', getLayoutXCode);
  setAttributValue(Name + '.setLayoutY(', getLayoutYCode);
  setAttributValue(Name + '.setPrefHeight(', getPrefHeightCode);
  setAttributValue(Name + '.setPrefWidth(', getPrefWidthCode);
end;

function TFXRegion.getAttributes(ShowAttributes: integer): string;
begin
  if ShowAttributes = 3
    then Result:= '|Background|PrefHeight|PrefWidth|SnapToPixel' + inherited getAttributes(3)
    else Result:= '|Name';
end;

{--- TFXControl ---------------------------------------------------------------}

function TFXControl.getAttributes(ShowAttributes: integer): string;
begin
  if ShowAttributes = 3
    then Result:= '|Tooltip|ContextMenu' + inherited getAttributes(3)
    else Result:= inherited getAttributes(ShowAttributes)
end;

procedure TFXControl.SetAttribute(Attr, Value, Typ: string);
begin
  if Typ = 'TAlignment' then begin  // wieso hier?
    InsertImport('javafx.geometry.*');
    MakeAttribut('Alignment', 'Pos.' + Value);
  end else if Attr = 'Tooltip' then
    MakeAttribut(Attr, 'new Tooltip(' + asString(Value) + ')')
  else if Attr = 'ContextMenu' then
    MakeAttribut(Attr, Value)
  else
    inherited;
end;

end.
