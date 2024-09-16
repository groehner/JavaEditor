unit UAComponents;

interface

uses
  Windows, Classes, Graphics, Controls,
  UJEComponents;

const
  AncestorEvents1 = '|ancestorMoved|ancestorResized';
  AncestorEvents2 = '|ancestorAdded|ancestorRemoved';
  CaretEvents     = '|caretPositionChanged';
  ComponentEvents = '|componentHidden|componentMoved|componentResized|componentShown';
  ContainerComponentEvents = '|componentAdded|componentHidden|componentMoved|componentRemoved|componentResized|componentShown';
  FocusEvents     = '|focusGained|focusLost';
  HierarchyEvents = '|hierarchyChanged';
  ItemEvents      = '|itemStateChanged';
  InputMethodEvents = '|inputMethodTextChanged';
  KeyEvents = '|keyPressed|keyReleased|keyTyped';
  MouseEvents = '|mouseClicked|mouseDragged|mouseEntered|mouseExited|mouseMoved|mousePressed|mouseReleased|mouseWheelMoved';
  PopupMenuEvents = '|popupMenuCanceled|popupMenuWillBecomeInvisible|popupMenuWillBecomeVisible';  // JComboBox
  PropertyEvents = '|propertyChange';
  StateEvents    = '|stateChanged';
  TreeEvents     = '|treeCollapsed|treeExpanded|treeValueChanged';
  VetoableEvents = '|vetoableChange';
  WindowEvents = '|windowActivated|windowClosed|windowClosing|windowDeactivated|windowDeiconified|windowGainedFocus|windowIconified|windowLostFocus|windowOpened|windowStateChanged';
  ColorNone = clBtnFace;

type

  THorzAlignment = (LEFT, CENTER, RIGHT {, LEADING, TRAILING});

  TAWTComponent = class(TJEComponent)
  private
    FShowFont: boolean;

    FAWTSelectionColor: TColor;
    FAWTGray: TColor;
    FAWTDarkGray: TColor;
    FDarkShadow: TColor;

    FDefaultBackground: TColor;
    FDefaultForeground: TColor;
    FFocusable: boolean;

    FactionPerformed: string;
    FancestorMoved: string;
    FancestorResized: string;
    FcaretPositionChanged: string;
    FcomponentHidden: string;
    FcomponentMoved: string;
    FcomponentResized: string;
    FcomponentShown: string;
    FFocusGained: string;
    FFocusLost: string;
    FhierarchyChanged: string;
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
    FmouseWheelMoved: string;
    FpropertyChange: string;
    FtextValueChanged: string;
    FitemStateChanged: string;
    FadjustmentValueChanged: string;
    FcomponentAdded: string;
    FcomponentRemoved: string;

    procedure MakeCursor(const Value: string);
  protected
    function AWTSwingContainer: string;
    procedure RenameMenu(const OldName, NewName, J: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFromA(Control: TControl);
    procedure SetPositionAndSize; override;
    procedure Scrollbar(SBRect: TRect; horizontal, Swing: boolean); overload;
    function getBounds: string; virtual;
    procedure Rename(const OldName, NewName, Events: string); override;
    function isFontAttribute(const s: string): boolean;
    function FontChanged: boolean;
    procedure MakeEchoChar(const Value: string);
    procedure MakeText(SL: TStrings);
    procedure CalculateMenus(MenuItems, Menu, ConstructMenu, Methods: TStrings;
                             J: string; newMenuBar: boolean = false);
    procedure MakeMenuItems(OldItems, NewItems: TStrings; newMenuBar: boolean = false);
    procedure DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);

    procedure DefaultComponent;
    function AddVariable: string;
    function getContainer: string;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure setAttributValueAfter(key, s: string);
    procedure InsertNewVariable(const Variable: string);
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure ChangeAttributValue(const key: string; s: string);
    procedure DeleteListener(const event: string); override;
    procedure InsertImport(const s: string);
    function getListener(const Event: string): string; override;
    procedure MakeListener(Value: string);
    function getContextMenuListener(Value: string): string;
    function GetContainerAdd: string; override;
    function MakeEventProcedure(const Event: string ): string; override;
    procedure MakeSelectedIndex(const value: string);
    procedure MakeColor(const Attr, Value: string);
    procedure MakeFont; override;
    function getAttrColor(const Value: string): string;
    procedure Paint; override;

    property DefaultBackground: TColor read FDefaultBackground write FDefaultBackground;
    property DefaultForeground: TColor read FDefaultForeground write FDefaultForeground;
    property DarkShadow: TColor read FDarkShadow;
    property actionPerformed: string read FActionPerformed write FActionPerformed;     // Button TextField List
    property textValueChanged: string read FtextValueChanged write FtextValueChanged;  // TextField
    property itemStateChanged: string read FItemStateChanged write FItemStateChanged;  // Checkbox List ComboBox
    property adjustmentValueChanged: string read FAdjustmentValueChanged write FAdjustmentValueChanged; // ScrollBar
    property componentAdded: string read FcomponentAdded write FcomponentAdded;        // Panel
    property componentRemoved: string read FcomponentRemoved write FcomponentRemoved;  // Panel
    property ShowFont: boolean read FShowFont write FShowFont;
  published
    property AWTGray: TColor read FAWTGray;
    property AWTDarkGray: TColor read FAWTDarkGray;
    property AWTSelectionColor: TColor read FAWTSelectionColor;
    property Focusable: boolean read FFocusable write FFocusable;
    property Enabled;
    property Visible;

    property ancestorMoved: string read FancestorMoved write FancestorMoved;
    property ancestorResized: string read FancestorResized write FancestorResized;
    property caretPositionChanged: string read FcaretPositionChanged write FcaretPositionChanged;
    property componentHidden: string read FcomponentHidden write FcomponentHidden;
    property componentMoved: string read FcomponentMoved write FcomponentMoved;
    property componentResized: string read FcomponentResized write FcomponentResized;
    property componentShown: string read FcomponentShown write FcomponentShown;
    property focusGained: string read FFocusGained write FFocusGained;
    property focusLost: string read FFocusLost write FFocusLost;
    property hierarchyChanged: string read FhierarchyChanged write FhierarchyChanged;
    property inputMethodTextChanged: string read FinputMethodTextChanged write FinputMethodTextChanged;
    property keyPressed: string read FkeyPressed write FkeyPressed;
    property keyReleased: string read FkeyReleased write FkeyReleased;
    property keyTyped: string read FkeyTyped write FkeyTyped;
    property mouseClicked: string read FmouseClicked write FmouseClicked;
    property mouseEntered: string read FmouseEntered write FmouseEntered;
    property mouseExited: string read FmouseExited write FmouseExited;
    property mousePressed: string read FmousePressed write FmousePressed;
    property mouseReleased: string read FmouseReleased write FmouseReleased;
    property mouseDragged: string read FmouseDragged write FmouseDragged;
    property mouseMoved: string read FmouseMoved write FmouseMoved;
    property mouseWheelMoved: string read FmouseWheelMoved write FmouseWheelMoved;
    property propertyChange: string read FpropertyChange write FpropertyChange;
  end;

implementation

uses SysUtils, UITypes, UEditorForm, UBaseForm,
     UUtils, ULink, UGUIForm, UGUIDesigner, JvGnugettext,
     UStringRessources, UObjectInspector, UConfiguration;

{--- TAWTComponent ------------------------------------------------------------}

constructor TAWTComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TFGUIForm then
    Partner:= (AOwner as TFGuiForm).Partner as TFEditForm;
  FDefaultBackground:= RGB(238, 238, 238);        // EEEEEE  BACKGROUND
  FDefaultForeground:= RGB(51, 51, 51);           // 333333  FOREGROUND
  FAWTSelectionColor:= RGB(51, 153, 255);
  FAWTGray:= RGB(200, 200, 200);
  FAWTDarkGray:= RGB(105, 105, 105);
  FDarkShadow := RGB(122, 138, 153); // border color
  FFocusable:= true;

  Width:= 120;
  Height:= 80;
  Sizeable:= true;
  Background:= clWhite;
  Foreground:= FDefaultForeground;
  HelpType:= htContext;
  Font.Name:= FConfiguration.GuiFontName;
  Font.Size:= FConfiguration.GuiFontSize;
  FShowFont:= true;
end;

procedure TAWTComponent.CreateFromA(Control: TControl);
begin
  Tag:= Control.Tag;
  Enabled:= Control.Enabled;
  Visible:= Control.Visible;
  SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
end;

procedure TAWTComponent.Paint;
begin
  CanvasFontAssign;
end;

procedure TAWTComponent.Scrollbar(SBRect: TRect; horizontal, Swing: boolean);

  function PicNr(Nr: integer): integer;
  begin
    Result:= Nr;
    if Swing then inc(Result, 6);
  end;

begin
  Canvas.Pen.Color:= DefaultBackground;
  Canvas.Brush.Color:= DefaultBackground;
  Canvas.Rectangle(SBRect);
  var p16:= PPIScale(16);
  if horizontal then begin
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Right - p16, SBRect.Top, PicNr(2));
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left, SBRect.Top, PicNr(3));
    // fill between
    Canvas.Rectangle(Rect(SBRect.Left + p16, SBRect.Top, SBRect.Right - p16, SBRect.Bottom));
    // scroller
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left + p16, SBRect.Top, PicNr(6));
  end else begin
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left, SBRect.Top, PicNr(4));
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left, SBRect.Bottom - p16, PicNr(5));
    // fill between
    Canvas.Rectangle(Rect(SBRect.Left, SBRect.Top + p16, SBRect.Right, SBRect.Bottom - p16));
    // scroller
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Right - p16, SBRect.Top + p16, PicNr(7));
  end;
end;

function TAWTComponent.getBounds: string;
begin
  Result:= '.setBounds(' +
           IntToStr(PPIUnScale(Left)) + ', ' + IntToStr(PPIUnScale(Top)) + ', ' +
           IntToStr(PPIUnScale(Width)) + ', ' + IntToStr(PPIUnScale(Height)) + ');';
end;

function TAWTComponent.isFontAttribute(const s: string): boolean;
begin
  Result:= Pos(s, ' Font Name Size Bold Italic') > 0;
end;

function TAWTComponent.FontChanged: boolean;
begin
  Result:= (Font.Name <> 'Dialog') or (Font.Size <> 12);
end;

procedure TAWTComponent.SetAttribute(Attr, Value, Typ: string);
begin
  if isFontAttribute(Attr) then MakeFont else
  if Typ = 'TCursor'       then MakeCursor(Value) else
  if Typ = 'TColor'        then MakeColor(Attr, Value) else
  if Typ = 'string'        then MakeAttribut(Attr, asString(Value)) else
    MakeAttribut(Attr, Value);
end;

procedure TAWTComponent.setAttributValueAfter(key, s: string);
begin
  if Pos(Indent2, s) = 0 then
    s:= Indent2 + s;
  Partner.setAttributValue(GetContainerAdd, key, s, 1);
end;

procedure TAWTComponent.InsertNewVariable(const Variable: string);
begin
  Partner.InsertAttribute(getContainer, GetIndentation + Variable, false);
end;

function TAWTComponent.getAttributes(ShowAttributes: integer): string;
  const Show1 = '|Background|Name|Visible';
        Show2 = '|Foreground|Font';
        show3 = '|Cursor|Enabled|Focusable|Height|Left|Top|Width';

begin
  case ShowAttributes of
    1: Result:= '|Name';
    2: if ShowFont
         then Result:= Show1 + Show2
         else Result:= Show1;
    else if ShowFont
         then Result:= Show1 + Show2 + Show3
         else Result:= Show1 + Show3;
  end;
end;

function TAWTComponent.getEvents(ShowEvents: integer): string;
begin
  case ShowEvents of
    1: Result:= '';
    2: Result:= FocusEvents + KeyEvents + MouseEvents;
    3: Result:= AncestorEvents1 + FocusEvents + KeyEvents +
                MouseEvents + CaretEvents + ComponentEvents +
                HierarchyEvents + InputMethodEvents + PropertyEvents;
  end;
  Result:= Result + '|';
end;

procedure TAWTComponent.SetPositionAndSize;
begin
  if Sizeable then
    ChangeAttributValue(Name + '.setBounds(', Name + getBounds)
  else begin
    Width:= PPIScale(32);
    Height:= PPIScale(28);
  end;
end;

procedure TAWTComponent.ChangeAttributValue(const key: string; s: string);
begin
  if Pos(Indent2, s) = 0 then
    s:= Indent2 + s;
  Partner.ChangeAttributValue(GetContainerAdd, key, s);
end;

procedure TAWTComponent.DeleteListener(const event: string);
  var EventMethod, Listener: string;
begin
  EventMethod:= MakeEventProcedure(Event);
  Partner.DeleteEventMethod(EventMethod);
  Listener:= getListener(Event);
  Partner.DeleteListener(Listener);
end;

procedure TAWTComponent.InsertImport(const s: string);
begin
  Partner.InsertImport(s);
end;

function TAWTComponent.MakeEventProcedure(const Event: string ): string;
  var s: string;
begin

//  Example:
//  public void jButton1_ActionPerformed(ActionEvent evt) {
//    // TODO add your code here
//  }

  if Event = 'propertyChange' then begin
    Result:= Indent1 + 'public void ' + Name + '_PropertyChange(java.beans.PropertyChangeEvent evt) {' + CrLf +
             Indent2 + 'if (evt.getPropertyName().equals("<Propertyname>")) {' + CrLf +
             Indent3 + _(LNGTODO) + CrLf + CrLf +
             Indent2 + '}'+ CrLf;
    s:= Name + '_PropertyChange';
  end else begin
    s:= MakeEventProcedureName(Event);
    Result:= Indent1 + 'public void ' + s + '(';
    if      Event = 'actionPerformed' then  Result:= Result + 'Action'
    else if Event = 'stateChanged' then     Result:= Result + 'Change'
    else if Event = 'itemStateChanged' then Result:= Result + 'Item'
    else if Pos('key', Event) = 1 then      Result:= Result + 'Key'
    else if Event = 'textValueChanged' then Result:= Result + 'Text'
    else if Event = 'valueChanged' then Result:= Result + 'ListSelection'
    else if Event = 'itemStateChanged' then  Result:= Result + 'Item'
    else if Pos('mouse', Event) = 1 then    Result:= Result + 'Mouse'
    else if Event = 'treeValueChanged' then Result:= Result + 'TreeSelection'
    else if Pos('tree', Event) = 1 then     Result:= Result + 'TreeExpansion'
    else if Pos('focus', Event) = 1 then     Result:= Result + 'Focus'
    else if Event = 'adjustmentValueChanged' then Result:= Result + 'Adjustment'
    else if Event = 'caretPositionChanged' then Result:= Result + 'InputMethod'
    else if Event = 'inputMethodTextChanged' then Result:= Result + 'InputMethod'
    else if (Event = 'componentAdded') or (Event = 'componentRemoved') then Result:= Result + 'Container'
    else if Pos('component', Event) = 1 then Result:= Result + 'Component'
    else if (Event = 'ancestorMoved') or (Event = 'ancestorResized') then Result:= Result + 'Hierarchy'
    else if Pos('ancestor', Event) = 1 then Result:= Result + 'Ancestor'
    else if Event = 'hierarchyChanged' then Result:= Result + 'Hierarchy'
    else if Pos('window', Event) = 1 then Result:= Result + 'Window'
    else if Event = 'vetoableChange' then Result:= Result + 'java.beans.PropertyChange'
    else if Pos('popupMenu', Event) = 1 then Result:= Result + 'PopupMenu';
    Result:= Result + 'Event evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf + Indent2 + CrLf;
  end;
  Result:= Result + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result:= Result + ' // end of ' + s;
  Result:= Result + CrLf + CrLf;
end;

function TAWTComponent.getListener(const Event: string): string;
begin
  if Event = 'actionPerformed' then
    Result:= Name + '.addActionListener(new ActionListener() { ' + CrLf +
             Indent3 + 'public void actionPerformed(ActionEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event)+ '(evt);' + CrLf
  else if Event = 'stateChanged' then
    Result:= Name + '.addChangeListener(new ChangeListener() { ' + CrLf +
             Indent3 + 'public void stateChanged(ChangeEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event)+ '(evt);' + CrLf
  else if Event = 'itemStateChanged' then
    Result:= Name + '.addItemListener(new ItemListener() { ' + CrLf +
             Indent3 + 'public void itemStateChanged(ItemEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('key', Event) = 1 then
    Result:= Name + '.addKeyListener(new KeyAdapter() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(KeyEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'textValueChanged' then
    Result:= Name + '.addTextListener(new TextListener() { ' + CrLf +
             Indent3 + 'public void textValueChanged(TextEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) +'(evt);' + CrLf
  else if Event = 'valueChanged' then
    Result:= Name + '.addListSelectionListener(new ListSelectionListener() { ' + CrLf +
             Indent3 + 'public void valueChanged(ListSelectionEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event)+ '(evt);' + CrLf
  else if Event = 'itemStateChanged' then
    Result:= Name + '.addItemListener(new ItemListener() { ' + CrLf +
             Indent3 + 'public void itemStateChanged(ItemEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event)+ '(evt);' + CrLf
  else if (Event = 'mouseMoved') or (Event = 'mouseDragged') then
    Result:= Name + '.addMouseMotionListener(new MouseMotionAdapter() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(MouseEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'mouseWheelMoved' then
    Result:= Name + '.addMouseWheelListener(new MouseWheelListener() { ' + CrLf +
             Indent3 + 'public void mouseWheelMoved(MouseWheelEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('mouse', Event) = 1 then
    Result:= Name + '.addMouseListener(new MouseAdapter() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(MouseEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'treeValueChanged' then
    Result:= Name + '.addTreeSelectionListener(new TreeSelectionListener() { ' + CrLf +
             Indent3 + 'public void valueChanged(TreeSelectionEvent evt) { ' + CrLf +  // no tree before
             Indent3 + Indent1 + MakeEventProcedureName(Event) +'(evt);' + CrLf
  else if Event = 'treeCollapsed' then
    Result:= Name + '.addTreeExpansionListener(new TreeExpansionListener() { ' + CrLf +
             Indent3 + 'public void treeCollapsed(TreeExpansionEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) +'(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void treeExpanded(TreeExpansionEvent evt) { ' + CrLf
  else if Event = 'treeExpanded' then
    Result:= Name + '.addTreeExpansionListener(new TreeExpansionListener() { ' + CrLf +
             Indent3 + 'public void treeExpanded(TreeExpansionEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) +'(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void treeCollapsed(TreeExpansionEvent evt) { ' + CrLf
  else if Pos('focus', Event) = 1 then
    Result:= Name + '.addFocusListener(new FocusAdapter() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(FocusEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'adjustmentValueChanged' then
    Result:= Name + '.addAdjustmentListener(new AdjustmentListener() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(AdjustmentEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'caretPositionChanged' then
    Result:= Name + '.addInputMethodListener(new InputMethodListener() { ' + CrLf +
             Indent3 + 'public void caretPositionChanged(InputMethodEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) +'(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void inputMethodTextChanged(InputMethodEvent evt) { ' + CrLf
  else if Event = 'inputMethodTextChanged' then
    Result:= Name + '.addInputMethodListener(new InputMethodListener() { ' + CrLf +
             Indent3 + 'public void inputMethodTextChanged(InputMethodEvent evt) { ' + CrLf +
             Indent3 + Indent1 + Name + '_InputMethodTextChanged(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void caretPositionChanged(InputMethodEvent evt) { ' + CrLf
  else if Event = 'propertyChange' then
    Result:= Name + '.addPropertyChangeListener(new java.beans.PropertyChangeListener() { ' + CrLf +
             Indent3 + 'public void propertyChange(java.beans.PropertyChangeEvent evt) { ' + CrLf +
             Indent3 + Indent1 + Name + '_PropertyChange(evt);' + CrLf
  else if (Event = 'componentAdded') or (Event = 'componentRemoved') then
    Result:= Name + '.addContainerListener(new ContainerAdapter() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(ContainerEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('component', Event) = 1 then
    Result:= Name + '.addComponentListener(new ComponentAdapter() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(ComponentEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if (Event = 'ancestorMoved') or (Event = 'ancestorResized')  then
    Result:= Name + '.addHierarchyBoundsListener(new HierarchyBoundsAdapter() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(HierarchyEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'hierarchyChanged' then
    Result:= Name + '.addHierarchyListener(new HierarchyListener() { ' + CrLf +
             Indent3 + 'public void hierarchyChanged(HierarchyEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if (Event = 'windowGainedFocus') or (Event = 'windowLostFocus') then
    Result:= 'addWindowFocusListener(new WindowAdapter() { ' + CrLf +     // without Component.
             Indent3 + 'public void ' + Event + '(WindowEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'windowStateChanged'  then
    Result:= 'addWindowStateListener(new WindowAdapter() { ' + CrLf +     // without Component.
             Indent3 + 'public void windowStateChanged(WindowEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('window', Event) = 1 then
    Result:= 'addWindowListener(new WindowAdapter() { ' + CrLf +     // without Component.
             Indent3 + 'public void ' + Event + '(WindowEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'vetoableChange' then
    Result:= Name + '.addVetoableChangeListener(new java.beans.VetoableChangeListener() { ' + CrLf +
             Indent3 + 'public void ' + Event + '(java.beans.PropertyChangeEvent evt) { ' + CrLf +
             Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'ancestorAdded' then
    Result:= Name + '.addAncestorListener(new AncestorListener() { ' + CrLf +
             Indent3 + 'public void ancestorAdded(AncestorEvent evt) { ' + CrLf +
             Indent3 + Indent1 + Name + '_AncestorAdded(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void ancestorMoved(AncestorEvent evt) { }' + CrLf +
             Indent3 + 'public void ancestorRemoved(AncestorEvent evt) { ' + CrLf
  else if Event = 'ancestorRemoved' then
    Result:= Name + '.addAncestorListener(new AncestorListener() { ' + CrLf +
             Indent3 + 'public void ancestorRemoved(AncestorEvent evt) { ' + CrLf +
             Indent3 + Indent1 + Name + '_AncestorRemoved(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void ancestorAdded(AncestorEvent evt) { }' + CrLf +
             Indent3 + 'public void ancestorMoved(AncestorEvent evt) { ' + CrLf
  else if Event = 'popupMenuCanceled' then
    Result:= Name + '.addPopupMenuListener(new PopupMenuListener() { ' + CrLf +
             Indent3 + 'public void popupMenuCanceled(PopupMenuEvent evt) { ' + CrLf +
             Indent3 + Indent1 + Name + '_PopupMenuCanceled(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) { }' + CrLf +
             Indent3 + 'public void popupMenuWillBecomeVisible(PopupMenuEvent evt) { ' + CrLf
  else if Event = 'popupMenuWillBecomeInvisible' then
    Result:= Name + '.addPopupMenuListener(new PopupMenuListener() { ' + CrLf +
             Indent3 + 'public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) { ' + CrLf +
             Indent3 + Indent1 + Name + '_PopupMenuWillBecomeInvisible(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void popupMenuCanceled(PopupMenuEvent evt) { }' + CrLf +
             Indent3 + 'public void popupMenuWillBecomeVisible(PopupMenuEvent evt) { ' + CrLf
  else if Event = 'popupMenuWillBecomeVisible' then
    Result:= Name + '.addPopupMenuListener(new PopupMenuListener() { ' + CrLf +
             Indent3 + 'public void popupMenuWillBecomeVisible(PopupMenuEvent evt) { ' + CrLf +
             Indent3 + Indent1 + Name + '_PopupMenuWillBecomeVisible(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent3 + 'public void popupMenuCanceled(PopupMenuEvent evt) { }' + CrLf +
             Indent3 + 'public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) { ' + CrLf;
  Result:= Indent2 + Result +  Indent3 + '}' + CrLf + Indent2 + '});' + CrLf;
end;

function TAWTComponent.getContextMenuListener(Value: string): string;
begin
  Result:= surroundFix2(Value + '.addMouseListener(new MouseAdapter() {') +
           surroundFix3('public void mouseReleased(MouseEvent evt) {') +
           surroundFix3(Indent1 + 'if (evt.isPopupTrigger())') +
           surroundFix3(Indent2 + Name + '.show(evt.getComponent(), evt.getX(), evt.getY());') +
           surroundFix3('}') +
           surroundFix2('});');
end;

function TAWTComponent.getContainerAdd: string;
begin
  Result:= AWTSwingContainer;
  if Result = ''
    then Result:= 'add(' + name + ');'
    else Result:= Result + '.add(' + name + ');';
end;

function TAWTComponent.AWTSwingContainer: string;
begin
  if (Parent = nil) or (Parent is TFGUIForm)
    then Result:= 'cp'
    else Result:= Parent.Name;
end;

procedure TAWTComponent.Rename(const OldName, NewName, Events: string);

  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(FactionPerformed);
  rename(FancestorMoved);
  rename(FancestorResized);
  rename(FcaretPositionChanged);
  rename(FcomponentHidden);
  rename(FcomponentMoved);
  rename(FcomponentResized);
  rename(FcomponentShown);
  rename(FFocusGained);
  rename(FFocusLost);
  rename(FhierarchyChanged);
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
  rename(FmouseWheelMoved);
  rename(FpropertyChange);
  rename(FtextValueChanged);
  rename(FitemStateChanged);
  rename(FadjustmentValueChanged);
  rename(FcomponentAdded);
  rename(FcomponentRemoved);
end;

procedure TAWTComponent.RenameMenu(const OldName, NewName, J: string);
  var AttributeS, AttributeE, ComponentS, ComponentE, MethodsS, MethodsE: integer;
begin
  AttributeS:= Partner.getLNGStartAttributes;
  AttributeE:= Partner.getLNGEndAttributes;

  ComponentS:= Partner.getLNGStartComponents;
  ComponentE:= Partner.getLNGEndComponents;

  MethodsS:= Partner.getLNGStartEventMethods;
  MethodsE:= Partner.getLNGEndEventMethods;

  // event methods
  Partner.ReplaceTextWithRegex('public void ' + OldName + '_(.*)\(ActionEvent evt\)',
                               'public void ' + NewName + '_$1(ActionEvent evt)',
                               true, MethodsS, MethodsE);
  Partner.ReplaceTextWithRegex('// end of ' + OldName + '(.*)',
                               '// end of ' + NewName + '$1',
                               true, MethodsS, MethodsE);

  // listener
  Partner.ReplaceTextWithRegex(OldName + '_(.*).addActionListener\(',
                               NewName + '_$1.addActionListener(',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex(OldName + '_(.*)_ActionPerformed\(evt\)',
                               NewName + '_$1_ActionPerformed(evt)',
                               true, ComponentS, ComponentE);

  // declaration of menu variables
  Partner.ReplaceTextWithRegex('private ' + J + 'Menu ' + OldName + '_(.*) = new ',
                               'private ' + J + 'Menu ' + NewName + '_$1 = new ',
                               true, AttributeS, AttributeE);
  Partner.ReplaceTextWithRegex('private ' + J + 'MenuItem ' + OldName + '_(.*) = new ',
                               'private ' + J + 'MenuItem ' + NewName + '_$1 = new ',
                               true, AttributeS, AttributeE);

  // creation of menu
  Partner.ReplaceTextWithRegex(OldName + '(.*).add\(', NewName + '$1.add(',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex(OldName + '(.*).addSeparator\(', NewName + '$1.addSeparator(',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex(OldName + '(.*).setAccelerator\(', NewName + '$1.setAccelerator(',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex(OldName + '(.*).setShortcut\(', NewName + '$1.setShortcut(',
                               true, ComponentS, ComponentE);
  Partner.ReplaceTextWithRegex('\(' + OldName + '(.*)\);', '(' + NewName + '$1);',
                               true, ComponentS, ComponentE);
end;


procedure TAWTComponent.DefaultComponent;
begin
  SetPositionAndSize;
  if ShowFont then
    MakeFont;
  Partner.InsertComponent(Indent2 + AddVariable + CrLf);
end;

function TAWTComponent.AddVariable: string;
begin
  Result:= getContainer + '.add(' + Name + ');'
end;

function TAWTComponent.getContainer: string;
begin
  if (Parent = nil) or (Parent is TFGUIForm)
    then Result:= 'cp'
    else Result:= Parent.Name;
end;

procedure TAWTComponent.MakeCursor(const Value: string);
begin
  if Value = 'DEFAULT'
    then MakeAttribut('Cursor', '')
    else MakeAttribut('Cursor', 'Cursor.getPredefinedCursor(Cursor.' + Value + '_CURSOR)');
end;

procedure TAWTComponent.MakeFont;
  var s, map: string; intstyle: integer;
begin
  if Name = '' then exit;

  map:= Name + '_map';
  Partner.DeleteAttributeValues(map);
  Partner.DeleteAttributeValue(Name + '.setFont');

  if ((fsUnderline in Font.Style) or (fsStrikeout in Font.Style)) then begin
    s:= Indent2 + 'Hashtable<TextAttribute, Object> ' + map + ' = new Hashtable<TextAttribute, Object>();' + CrLf +
        Indent2 + map + '.put(TextAttribute.FAMILY, "' + Font.Name+ '");' + CrLf +
        Indent2 + map + '.put(TextAttribute.SIZE, new Integer(' + IntToStr(Font.Size) + '));' + CrLf;
    if fsUnderline in Font.Style then
      s:= s + Indent2 + map + '.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);' + CrLf;
    if fsStrikeOut in Font.Style then
      s:= s + Indent2 + map + '.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);' + CrLf;
    s:= s + Indent2 + Name + '.setFont(new Font(' + map + '));';
    if Tag = 34
      then Partner.setAttributValue(GetContainerAdd, '___XXX___', s, 1)
      else Partner.setAttributValue(GetContainerAdd, '___XXX___', s, 0);

    Partner.InsertImport('java.util.Hashtable');
    Partner.InsertImport('java.awt.font.TextAttribute');
  end else begin
    s:= Indent2 + Name + '.setFont(new Font("' + Font.Name + '", ';
    intstyle:= 0;
    if fsBold   in Font.Style then inc(intstyle, 1);
    if fsItalic in Font.Style then inc(intstyle, 2);
    case intstyle of
      0: s:= s + 'Font.PLAIN, ';
      1: s:= s + 'Font.BOLD, ';
      2: s:= s + 'Font.ITALIC, ';
      3: s:= s + 'Font.BOLD + Font.ITALIC, ';
    end;
    s:= s + IntToStr(PPIUnScale(Font.Size)) + '));';
    if Tag = 34
      then Partner.setAttributValue(GetContainerAdd, Name + '.setFont', s, 1)
      else Partner.setAttributValue(GetContainerAdd, Name + '.setFont', s, 0);
  end;
  if Foreground <> Font.Color then begin
    Foreground:= Font.Color;
    MakeColor('Foreground', TColorToString(Font.Color));
    FObjectInspector.UpdatePropertyInspector;
  end;
  SizeToText;
end;

procedure TAWTComponent.MakeColor(const Attr, Value: string);
begin
  if Value = '(NONE)' then begin
    MakeAttribut(Attr, '');
    if Attr = 'Foreground' then
      Foreground:= DefaultForeground;
  end else
    MakeAttribut(Attr, getAttrColor(Value));
end;

function TAWTComponent.getAttrColor(const Value: string): string;
  var s, DValue: string;
begin
  DValue:= Java2DelphiColors(Value);
  if copy(DValue, 1, 3) = '$00' then
     s:= 'new Color(' + turnRGB('0x' + copy(DValue, 4, 6)) + ')'
  else if copy(Value, 1, 2) = '0x'
    then s:= 'new Color(' + Value + ')'
    else s:= 'Color.' + Value;
  if (Value = '(NONE)') or (Value = '(none)')
    then Result:= ''
    else Result:= s;
end;

procedure TAWTComponent.MakeEchoChar(const Value: string);
  var key: string;
begin
  key:= Name + '.setEchoChar(';
  if Value = ''
    then Partner.DeleteAttributeValue(key)
    else setAttributValue(key, key + '''' + Value + ''');');
end;

procedure TAWTComponent.MakeText(SL: TStrings);
begin
  var s:= '';
  for var i:= 0 to SL.Count - 1 do
    s:= s + SL.Strings[i] + '\n';
  delete(s, length(s)-1, 2);
  MakeAttribut('Text', asString(s));
end;

procedure TAWTComponent.CalculateMenus(MenuItems, Menu, ConstructMenu, Methods: TStrings;
                                       J: string; newMenubar: boolean);
  var i, p, Indent: integer;
      IndentAsString, ts, s, Shortcut: string;
      MenuName: array[-1..10] of string;

  function hasSubMenu(Items: TStrings; i: integer): boolean;
  begin
    Result:= (i < Items.count - 1) and
             (LeftSpaces(Items[i], 2) < LeftSpaces(Items[i+1], 2));
  end;

  function getShortCut(s: string): string;
    var Ctrl, shift: boolean;
  begin
    Ctrl:= (Pos('Ctrl+', s) = 1);
    if Ctrl then delete(s, 1, 5);
    Shift:= (Pos('Shift+', s) = 1);
    if Shift then delete(s, 1, 6);
    if J = '' then begin // awt
      Result:=  '.setShortcut(new MenuShortcut(KeyEvent.VK_' + s + ', ';
      if Shift
        then Result:= Result + 'true));'
        else Result:= Result + 'false));';
    end else begin  // swing
      Result:=  '.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_' + s + ', ';
      if Ctrl then Result:= Result + 'KeyEvent.CTRL_DOWN_MASK';
      if Ctrl and Shift then Result:= Result + ' + ';
      if Shift then Result:= Result + 'InputEvent.SHIFT_DOWN_MASK';
      Result:= Result + '));';
    end;
  end;

begin
  MenuName[-1]:= Name;
  if newMenuBar then
    ConstructMenu.add(Indent2 + 'set' + J + 'MenuBar(' + Name + ');');
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
      Menu.Add(IndentAsString + 'private ' + J + 'Menu ' + MenuName[Indent] +
               ' = new ' + J + 'Menu("' + ts + '");');
    end else begin
      if ts = '-' then begin
        ConstructMenu.Add(Indent2 + MenuName[Indent-1] + '.addSeparator();');
        continue;
      end else begin
        Menu.Add(IndentAsString + 'private ' + J + 'MenuItem ' + MenuName[Indent] +
                 ' = new ' + J + 'MenuItem("' + ts + '");');
        Methods.Add(MenuName[Indent]);
      end;
    end;
    ConstructMenu.Add(Indent2 + MenuName[Indent-1] + '.add(' + MenuName[Indent] + ');');
    if Shortcut <> '' then
      ConstructMenu.Add(Indent2 + MenuName[Indent] + ShortCut);
  end;
end;

procedure TAWTComponent.MakeMenuItems(OldItems, NewItems: TStrings; newMenuBar: boolean = false);
  var i: integer;
      J: string;
      OldMenu: TStringList;
      OldConstructMenu: TStringList;
      OldMethods: TStringList;
      NewMenu: TStringList;
      NewConstructMenu: TStringList;
      NewMethods: TStringList;

  function getListener(Method: string): string;
  begin
    Result:= Indent2 + Method + '.addActionListener(new ActionListener() {' + CrLf +
             Indent3 + 'public void actionPerformed(ActionEvent evt) {' + crLf +
             Indent3 + Indent1 + Method + '_ActionPerformed(evt);' + CrLf +
             Indent3 + '}' + CrLf +
             Indent2 + '});';
  end;

begin
  if Tag < 0
    then J:= ''
    else J:= 'J';

  Partner.Editor.BeginUpdate;
  FormatItems(NewItems);
  OldMenu:= TStringList.Create;
  OldConstructMenu:= TStringList.Create;
  OldMethods:= TStringList.Create;
  NewMenu:= TStringList.Create;
  NewConstructMenu:= TStringList.Create;
  NewMethods:= TStringList.Create;

  try
    CalculateMenus(OldItems, OldMenu, OldConstructMenu, OldMethods, J, newMenuBar);
    CalculateMenus(NewItems, NewMenu, NewConstructMenu, NewMethods, J, newMenuBar);
    Partner.DeleteOldAddNewMethods(OldMethods, NewMethods);
    for i:= 0 to OldConstructMenu.Count - 1 do
      Partner.DeleteAttributeValue(OldConstructMenu[i]);
    for i:= 0 to OldMenu.Count - 1 do
      Partner.DeleteAttribute(OldMenu[i]);
    for i:= 0 to OldMethods.Count - 1 do
      Partner.deleteListener(getListener(OldMethods[i]));
    for i:= 0 to NewMenu.Count -1 do
      Partner.InsertAttribute(0, Indent1 + NewMenu[i] + CrLf);
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

procedure TAWTComponent.DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);
begin
  MenuItemsOld.Text:= MenuItems.Text;
  MenuItems.Clear;
  MakeMenuItems(MenuItemsOld, MenuItems);
end;

procedure TAWTComponent.MakeListener(Value: string);
  var i: integer;
begin
  i:= Partner.getLineNumberWith(Name + '.show(evt.getComponent(), evt.getX()');
  Partner.ReplaceLineWith(i-3, Indent2 + Value + '.addMouseListener(new MouseAdapter() {');
end;

procedure TAWTComponent.MakeSelectedIndex(const value: string);
begin
  setAttributValue(Name + '.select(', Name + '.select(' + value + ');');
end;

end.
