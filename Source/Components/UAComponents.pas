unit UAComponents;

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  UJEComponents;

const
  AncestorEvents1 = '|ancestorMoved|ancestorResized';
  AncestorEvents2 = '|ancestorAdded|ancestorRemoved';
  CaretEvents = '|caretPositionChanged';
  ComponentEvents =
    '|componentHidden|componentMoved|componentResized|componentShown';
  ContainerComponentEvents =
    '|componentAdded|componentHidden|componentMoved|componentRemoved|componentResized|componentShown';
  FocusEvents = '|focusGained|focusLost';
  HierarchyEvents = '|hierarchyChanged';
  ItemEvents = '|itemStateChanged';
  InputMethodEvents = '|inputMethodTextChanged';
  KeyEvents = '|keyPressed|keyReleased|keyTyped';
  MouseEvents =
    '|mouseClicked|mouseDragged|mouseEntered|mouseExited|mouseMoved|mousePressed|mouseReleased|mouseWheelMoved';
  PopupMenuEvents =
    '|popupMenuCanceled|popupMenuWillBecomeInvisible|popupMenuWillBecomeVisible';
  // JComboBox
  PropertyEvents = '|propertyChange';
  StateEvents = '|stateChanged';
  TreeEvents = '|treeCollapsed|treeExpanded|treeValueChanged';
  VetoableEvents = '|vetoableChange';
  WindowEvents =
    '|windowActivated|windowClosed|windowClosing|windowDeactivated|windowDeiconified|windowGainedFocus|windowIconified|windowLostFocus|windowOpened|windowStateChanged';
  ColorNone = clBtnFace;

type

  THorizontalAlignment = (Left, Center, Right { , LEADING, TRAILING } );

  TAWTComponent = class(TJEComponent)
  private
    FShowFont: Boolean;

    FAWTSelectionColor: TColor;
    FAWTGray: TColor;
    FAWTDarkGray: TColor;
    FDarkShadow: TColor;

    FDefaultBackground: TColor;
    FDefaultForeground: TColor;
    FFocusable: Boolean;

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
    procedure Scrollbar(SBRect: TRect; Horizontal, Swing: Boolean); overload;
    function GetBounds: string; virtual;
    procedure Rename(const OldName, NewName, Events: string); override;
    function IsFontAttribute(const Str: string): Boolean;
    function FontChanged: Boolean;
    procedure MakeEchoChar(const Value: string);
    procedure MakeText(StringList: TStrings);
    procedure CalculateMenus(MenuItems, Menu, ConstructMenu, Methods: TStrings;
      const J: string; NewMenuBar: Boolean = False);
    procedure MakeMenuItems(OldItems, NewItems: TStrings;
      NewMenuBar: Boolean = False);
    procedure DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);

    procedure DefaultComponent;
    function AddVariable: string;
    function GetContainer: string;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetAttributValueAfter(const Key: string; Str: string);
    procedure InsertNewVariable(const Variable: string);
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure ChangeAttributValue(const Key: string; Str: string);
    procedure DeleteListener(const Event: string); override;
    procedure InsertImport(const Str: string);
    function GetListener(const Event: string): string; override;
    procedure MakeListener(const Value: string);
    function GetContextMenuListener(const Value: string): string;
    function GetContainerAdd: string; override;
    function MakeEventProcedure(const Event: string): string; override;
    procedure MakeSelectedIndex(const Value: string);
    procedure MakeColor(const Attr, Value: string);
    procedure MakeFont; override;
    function GetAttrColor(const Value: string): string;
    procedure Paint; override;

    property DefaultBackground: TColor read FDefaultBackground
      write FDefaultBackground;
    property DefaultForeground: TColor read FDefaultForeground
      write FDefaultForeground;
    property DarkShadow: TColor read FDarkShadow;
    property actionPerformed: string read FactionPerformed
      write FactionPerformed; // Button TextField List
    property textValueChanged: string read FtextValueChanged
      write FtextValueChanged; // TextField
    property itemStateChanged: string read FitemStateChanged
      write FitemStateChanged; // Checkbox List ComboBox
    property adjustmentValueChanged: string read FadjustmentValueChanged
      write FadjustmentValueChanged; // ScrollBar
    property componentAdded: string read FcomponentAdded write FcomponentAdded;
    // Panel
    property componentRemoved: string read FcomponentRemoved
      write FcomponentRemoved; // Panel
    property ShowFont: Boolean read FShowFont write FShowFont;
  published
    property AWTGray: TColor read FAWTGray;
    property AWTDarkGray: TColor read FAWTDarkGray;
    property AWTSelectionColor: TColor read FAWTSelectionColor;
    property Focusable: Boolean read FFocusable write FFocusable;
    property Enabled;
    property Visible;

    property ancestorMoved: string read FancestorMoved write FancestorMoved;
    property ancestorResized: string read FancestorResized
      write FancestorResized;
    property caretPositionChanged: string read FcaretPositionChanged
      write FcaretPositionChanged;
    property componentHidden: string read FcomponentHidden
      write FcomponentHidden;
    property componentMoved: string read FcomponentMoved write FcomponentMoved;
    property componentResized: string read FcomponentResized
      write FcomponentResized;
    property componentShown: string read FcomponentShown write FcomponentShown;
    property focusGained: string read FFocusGained write FFocusGained;
    property focusLost: string read FFocusLost write FFocusLost;
    property hierarchyChanged: string read FhierarchyChanged
      write FhierarchyChanged;
    property inputMethodTextChanged: string read FinputMethodTextChanged
      write FinputMethodTextChanged;
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
    property mouseWheelMoved: string read FmouseWheelMoved
      write FmouseWheelMoved;
    property propertyChange: string read FpropertyChange write FpropertyChange;
  end;

implementation

uses
  SysUtils,
  UITypes,
  UEditorForm,
  UUtils,
  ULink,
  UGUIForm,
  UGUIDesigner,
  JvGnugettext,
  UStringRessources,
  UObjectInspector,
  UConfiguration;

{ --- TAWTComponent ------------------------------------------------------------ }

constructor TAWTComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TFGUIForm then
    FPartner := (AOwner as TFGUIForm).Partner as TFEditForm;
  FDefaultBackground := RGB(238, 238, 238); // EEEEEE  BACKGROUND
  FDefaultForeground := RGB(51, 51, 51); // 333333  FOREGROUND
  FAWTSelectionColor := RGB(51, 153, 255);
  FAWTGray := RGB(200, 200, 200);
  FAWTDarkGray := RGB(105, 105, 105);
  FDarkShadow := RGB(122, 138, 153); // border color
  FFocusable := True;

  Width := 120;
  Height := 80;
  Sizeable := True;
  Background := clWhite;
  Foreground := FDefaultForeground;
  HelpType := htContext;
  Font.Name := FConfiguration.GUIFontName;
  Font.Size := FConfiguration.GUIFontSize;
  FShowFont := True;
end;

procedure TAWTComponent.CreateFromA(Control: TControl);
begin
  Tag := Control.Tag;
  Enabled := Control.Enabled;
  Visible := Control.Visible;
  SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
end;

procedure TAWTComponent.Paint;
begin
  CanvasFontAssign;
end;

procedure TAWTComponent.Scrollbar(SBRect: TRect; Horizontal, Swing: Boolean);

  function PicNr(Num: Integer): Integer;
  begin
    Result := Num;
    if Swing then
      Inc(Result, 6);
  end;

begin
  Canvas.Pen.Color := DefaultBackground;
  Canvas.Brush.Color := DefaultBackground;
  Canvas.Rectangle(SBRect);
  var
  P16 := PPIScale(16);
  if Horizontal then
  begin
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Right - P16, SBRect.Top,
      PicNr(2));
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left, SBRect.Top,
      PicNr(3));
    // fill between
    Canvas.Rectangle(Rect(SBRect.Left + P16, SBRect.Top, SBRect.Right - P16,
      SBRect.Bottom));
    // scroller
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left + P16, SBRect.Top,
      PicNr(6));
  end
  else
  begin
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left, SBRect.Top,
      PicNr(4));
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Left, SBRect.Bottom - P16,
      PicNr(5));
    // fill between
    Canvas.Rectangle(Rect(SBRect.Left, SBRect.Top + P16, SBRect.Right,
      SBRect.Bottom - P16));
    // scroller
    FGUIDesigner.vilControls1616.Draw(Canvas, SBRect.Right - P16,
      SBRect.Top + P16, PicNr(7));
  end;
end;

function TAWTComponent.GetBounds: string;
begin
  Result := '.setBounds(' + IntToStr(PPIUnScale(Left)) + ', ' +
    IntToStr(PPIUnScale(Top)) + ', ' + IntToStr(PPIUnScale(Width)) + ', ' +
    IntToStr(PPIUnScale(Height)) + ');';
end;

function TAWTComponent.IsFontAttribute(const Str: string): Boolean;
begin
  Result := Pos(Str, ' Font Name Size Bold Italic') > 0;
end;

function TAWTComponent.FontChanged: Boolean;
begin
  Result := (Font.Name <> 'Dialog') or (Font.Size <> 12);
end;

procedure TAWTComponent.SetAttribute(Attr, Value, Typ: string);
begin
  if IsFontAttribute(Attr) then
    MakeFont
  else if Typ = 'TCursor' then
    MakeCursor(Value)
  else if Typ = 'TColor' then
    MakeColor(Attr, Value)
  else if Typ = 'string' then
    MakeAttribut(Attr, AsString(Value))
  else
    MakeAttribut(Attr, Value);
end;

procedure TAWTComponent.SetAttributValueAfter(const Key: string; Str: string);
begin
  if Pos(Indent2, Str) = 0 then
    Str := Indent2 + Str;
  FPartner.SetAttributValue(GetContainerAdd, Key, Str, 1);
end;

procedure TAWTComponent.InsertNewVariable(const Variable: string);
begin
  FPartner.InsertAttribute(GetContainer, GetIndentation + Variable, False);
end;

function TAWTComponent.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|Background|Name|Visible';
  Show2 = '|Foreground|Font';
  Show3 = '|Cursor|Enabled|Focusable|Height|Left|Top|Width';

begin
  case ShowAttributes of
    1:
      Result := '|Name';
    2:
      if ShowFont then
        Result := Show1 + Show2
      else
        Result := Show1;
  else
    if ShowFont then
      Result := Show1 + Show2 + Show3
    else
      Result := Show1 + Show3;
  end;
end;

function TAWTComponent.GetEvents(ShowEvents: Integer): string;
begin
  case ShowEvents of
    1:
      Result := '';
    2:
      Result := FocusEvents + KeyEvents + MouseEvents;
    3:
      Result := AncestorEvents1 + FocusEvents + KeyEvents + MouseEvents +
        CaretEvents + ComponentEvents + HierarchyEvents + InputMethodEvents +
        PropertyEvents;
  end;
  Result := Result + '|';
end;

procedure TAWTComponent.SetPositionAndSize;
begin
  if Sizeable then
    ChangeAttributValue(Name + '.setBounds(', Name + GetBounds)
  else
  begin
    Width := PPIScale(32);
    Height := PPIScale(28);
  end;
end;

procedure TAWTComponent.ChangeAttributValue(const Key: string; Str: string);
begin
  if Pos(Indent2, Str) = 0 then
    Str := Indent2 + Str;
  FPartner.ChangeAttributValue(GetContainerAdd, Key, Str);
end;

procedure TAWTComponent.DeleteListener(const Event: string);
var
  EventMethod, Listener: string;
begin
  EventMethod := MakeEventProcedure(Event);
  FPartner.DeleteEventMethod(EventMethod);
  Listener := getListener(Event);
  FPartner.DeleteListener(Listener);
end;

procedure TAWTComponent.InsertImport(const Str: string);
begin
  FPartner.InsertImport(Str);
end;

function TAWTComponent.MakeEventProcedure(const Event: string): string;
var
  Str: string;
begin

  // Example:
  // public void jButton1_ActionPerformed(ActionEvent evt) {
  // // TODO add your code here
  // }

  if Event = 'propertyChange' then
  begin
    Result := Indent1 + 'public void ' + Name +
      '_PropertyChange(java.beans.PropertyChangeEvent evt) {' + CrLf + Indent2 +
      'if (evt.getPropertyName().equals("<Propertyname>")) {' + CrLf + Indent3 +
      _(LNGTODO) + CrLf + CrLf + Indent2 + '}' + CrLf;
    Str := Name + '_PropertyChange';
  end
  else
  begin
    Str := MakeEventProcedureName(Event);
    Result := Indent1 + 'public void ' + Str + '(';
    if Event = 'actionPerformed' then
      Result := Result + 'Action'
    else if Event = 'stateChanged' then
      Result := Result + 'Change'
    else if Event = 'itemStateChanged' then
      Result := Result + 'Item'
    else if Pos('key', Event) = 1 then
      Result := Result + 'Key'
    else if Event = 'textValueChanged' then
      Result := Result + 'Text'
    else if Event = 'valueChanged' then
      Result := Result + 'ListSelection'
    else if Event = 'itemStateChanged' then
      Result := Result + 'Item'
    else if Pos('mouse', Event) = 1 then
      Result := Result + 'Mouse'
    else if Event = 'treeValueChanged' then
      Result := Result + 'TreeSelection'
    else if Pos('tree', Event) = 1 then
      Result := Result + 'TreeExpansion'
    else if Pos('focus', Event) = 1 then
      Result := Result + 'Focus'
    else if Event = 'adjustmentValueChanged' then
      Result := Result + 'Adjustment'
    else if Event = 'caretPositionChanged' then
      Result := Result + 'InputMethod'
    else if Event = 'inputMethodTextChanged' then
      Result := Result + 'InputMethod'
    else if (Event = 'componentAdded') or (Event = 'componentRemoved') then
      Result := Result + 'Container'
    else if Pos('component', Event) = 1 then
      Result := Result + 'Component'
    else if (Event = 'ancestorMoved') or (Event = 'ancestorResized') then
      Result := Result + 'Hierarchy'
    else if Pos('ancestor', Event) = 1 then
      Result := Result + 'Ancestor'
    else if Event = 'hierarchyChanged' then
      Result := Result + 'Hierarchy'
    else if Pos('window', Event) = 1 then
      Result := Result + 'Window'
    else if Event = 'vetoableChange' then
      Result := Result + 'java.beans.PropertyChange'
    else if Pos('popupMenu', Event) = 1 then
      Result := Result + 'PopupMenu';
    Result := Result + 'Event evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf +
      Indent2 + CrLf;
  end;
  Result := Result + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result := Result + ' // end of ' + Str;
  Result := Result + CrLf + CrLf;
end;

function TAWTComponent.GetListener(const Event: string): string;
begin
  if Event = 'actionPerformed' then
    Result := Name + '.addActionListener(new ActionListener() { ' + CrLf +
      Indent3 + 'public void actionPerformed(ActionEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'stateChanged' then
    Result := Name + '.addChangeListener(new ChangeListener() { ' + CrLf +
      Indent3 + 'public void stateChanged(ChangeEvent evt) { ' + CrLf + Indent3
      + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'itemStateChanged' then
    Result := Name + '.addItemListener(new ItemListener() { ' + CrLf + Indent3 +
      'public void itemStateChanged(ItemEvent evt) { ' + CrLf + Indent3 +
      Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('key', Event) = 1 then
    Result := Name + '.addKeyListener(new KeyAdapter() { ' + CrLf + Indent3 +
      'public void ' + Event + '(KeyEvent evt) { ' + CrLf + Indent3 + Indent1 +
      MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'textValueChanged' then
    Result := Name + '.addTextListener(new TextListener() { ' + CrLf + Indent3 +
      'public void textValueChanged(TextEvent evt) { ' + CrLf + Indent3 +
      Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'valueChanged' then
    Result := Name + '.addListSelectionListener(new ListSelectionListener() { '
      + CrLf + Indent3 + 'public void valueChanged(ListSelectionEvent evt) { ' +
      CrLf + Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'itemStateChanged' then
    Result := Name + '.addItemListener(new ItemListener() { ' + CrLf + Indent3 +
      'public void itemStateChanged(ItemEvent evt) { ' + CrLf + Indent3 +
      Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if (Event = 'mouseMoved') or (Event = 'mouseDragged') then
    Result := Name + '.addMouseMotionListener(new MouseMotionAdapter() { ' +
      CrLf + Indent3 + 'public void ' + Event + '(MouseEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'mouseWheelMoved' then
    Result := Name + '.addMouseWheelListener(new MouseWheelListener() { ' + CrLf
      + Indent3 + 'public void mouseWheelMoved(MouseWheelEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('mouse', Event) = 1 then
    Result := Name + '.addMouseListener(new MouseAdapter() { ' + CrLf + Indent3
      + 'public void ' + Event + '(MouseEvent evt) { ' + CrLf + Indent3 +
      Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'treeValueChanged' then
    Result := Name + '.addTreeSelectionListener(new TreeSelectionListener() { '
      + CrLf + Indent3 + 'public void valueChanged(TreeSelectionEvent evt) { ' +
      CrLf + // no tree before
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'treeCollapsed' then
    Result := Name + '.addTreeExpansionListener(new TreeExpansionListener() { '
      + CrLf + Indent3 + 'public void treeCollapsed(TreeExpansionEvent evt) { '
      + CrLf + Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' +
      CrLf + Indent3 + '}' + CrLf + Indent3 +
      'public void treeExpanded(TreeExpansionEvent evt) { ' + CrLf
  else if Event = 'treeExpanded' then
    Result := Name + '.addTreeExpansionListener(new TreeExpansionListener() { '
      + CrLf + Indent3 + 'public void treeExpanded(TreeExpansionEvent evt) { ' +
      CrLf + Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
      + Indent3 + '}' + CrLf + Indent3 +
      'public void treeCollapsed(TreeExpansionEvent evt) { ' + CrLf
  else if Pos('focus', Event) = 1 then
    Result := Name + '.addFocusListener(new FocusAdapter() { ' + CrLf + Indent3
      + 'public void ' + Event + '(FocusEvent evt) { ' + CrLf + Indent3 +
      Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'adjustmentValueChanged' then
    Result := Name + '.addAdjustmentListener(new AdjustmentListener() { ' + CrLf
      + Indent3 + 'public void ' + Event + '(AdjustmentEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'caretPositionChanged' then
    Result := Name + '.addInputMethodListener(new InputMethodListener() { ' +
      CrLf + Indent3 +
      'public void caretPositionChanged(InputMethodEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf +
      Indent3 + '}' + CrLf + Indent3 +
      'public void inputMethodTextChanged(InputMethodEvent evt) { ' + CrLf
  else if Event = 'inputMethodTextChanged' then
    Result := Name + '.addInputMethodListener(new InputMethodListener() { ' +
      CrLf + Indent3 +
      'public void inputMethodTextChanged(InputMethodEvent evt) { ' + CrLf +
      Indent3 + Indent1 + Name + '_InputMethodTextChanged(evt);' + CrLf +
      Indent3 + '}' + CrLf + Indent3 +
      'public void caretPositionChanged(InputMethodEvent evt) { ' + CrLf
  else if Event = 'propertyChange' then
    Result := Name +
      '.addPropertyChangeListener(new java.beans.PropertyChangeListener() { ' +
      CrLf + Indent3 +
      'public void propertyChange(java.beans.PropertyChangeEvent evt) { ' + CrLf
      + Indent3 + Indent1 + Name + '_PropertyChange(evt);' + CrLf
  else if (Event = 'componentAdded') or (Event = 'componentRemoved') then
    Result := Name + '.addContainerListener(new ContainerAdapter() { ' + CrLf +
      Indent3 + 'public void ' + Event + '(ContainerEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('component', Event) = 1 then
    Result := Name + '.addComponentListener(new ComponentAdapter() { ' + CrLf +
      Indent3 + 'public void ' + Event + '(ComponentEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if (Event = 'ancestorMoved') or (Event = 'ancestorResized') then
    Result := Name +
      '.addHierarchyBoundsListener(new HierarchyBoundsAdapter() { ' + CrLf +
      Indent3 + 'public void ' + Event + '(HierarchyEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'hierarchyChanged' then
    Result := Name + '.addHierarchyListener(new HierarchyListener() { ' + CrLf +
      Indent3 + 'public void hierarchyChanged(HierarchyEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if (Event = 'windowGainedFocus') or (Event = 'windowLostFocus') then
    Result := 'addWindowFocusListener(new WindowAdapter() { ' + CrLf +
    // without Component.
      Indent3 + 'public void ' + Event + '(WindowEvent evt) { ' + CrLf + Indent3
      + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'windowStateChanged' then
    Result := 'addWindowStateListener(new WindowAdapter() { ' + CrLf +
    // without Component.
      Indent3 + 'public void windowStateChanged(WindowEvent evt) { ' + CrLf +
      Indent3 + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Pos('window', Event) = 1 then
    Result := 'addWindowListener(new WindowAdapter() { ' + CrLf +
    // without Component.
      Indent3 + 'public void ' + Event + '(WindowEvent evt) { ' + CrLf + Indent3
      + Indent1 + MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'vetoableChange' then
    Result := Name +
      '.addVetoableChangeListener(new java.beans.VetoableChangeListener() { ' +
      CrLf + Indent3 + 'public void ' + Event +
      '(java.beans.PropertyChangeEvent evt) { ' + CrLf + Indent3 + Indent1 +
      MakeEventProcedureName(Event) + '(evt);' + CrLf
  else if Event = 'ancestorAdded' then
    Result := Name + '.addAncestorListener(new AncestorListener() { ' + CrLf +
      Indent3 + 'public void ancestorAdded(AncestorEvent evt) { ' + CrLf +
      Indent3 + Indent1 + Name + '_AncestorAdded(evt);' + CrLf + Indent3 + '}' +
      CrLf + Indent3 + 'public void ancestorMoved(AncestorEvent evt) { }' + CrLf
      + Indent3 + 'public void ancestorRemoved(AncestorEvent evt) { ' + CrLf
  else if Event = 'ancestorRemoved' then
    Result := Name + '.addAncestorListener(new AncestorListener() { ' + CrLf +
      Indent3 + 'public void ancestorRemoved(AncestorEvent evt) { ' + CrLf +
      Indent3 + Indent1 + Name + '_AncestorRemoved(evt);' + CrLf + Indent3 + '}'
      + CrLf + Indent3 + 'public void ancestorAdded(AncestorEvent evt) { }' +
      CrLf + Indent3 + 'public void ancestorMoved(AncestorEvent evt) { ' + CrLf
  else if Event = 'popupMenuCanceled' then
    Result := Name + '.addPopupMenuListener(new PopupMenuListener() { ' + CrLf +
      Indent3 + 'public void popupMenuCanceled(PopupMenuEvent evt) { ' + CrLf +
      Indent3 + Indent1 + Name + '_PopupMenuCanceled(evt);' + CrLf + Indent3 +
      '}' + CrLf + Indent3 +
      'public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) { }' + CrLf
      + Indent3 +
      'public void popupMenuWillBecomeVisible(PopupMenuEvent evt) { ' + CrLf
  else if Event = 'popupMenuWillBecomeInvisible' then
    Result := Name + '.addPopupMenuListener(new PopupMenuListener() { ' + CrLf +
      Indent3 + 'public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) { '
      + CrLf + Indent3 + Indent1 + Name + '_PopupMenuWillBecomeInvisible(evt);'
      + CrLf + Indent3 + '}' + CrLf + Indent3 +
      'public void popupMenuCanceled(PopupMenuEvent evt) { }' + CrLf + Indent3 +
      'public void popupMenuWillBecomeVisible(PopupMenuEvent evt) { ' + CrLf
  else if Event = 'popupMenuWillBecomeVisible' then
    Result := Name + '.addPopupMenuListener(new PopupMenuListener() { ' + CrLf +
      Indent3 + 'public void popupMenuWillBecomeVisible(PopupMenuEvent evt) { '
      + CrLf + Indent3 + Indent1 + Name + '_PopupMenuWillBecomeVisible(evt);' +
      CrLf + Indent3 + '}' + CrLf + Indent3 +
      'public void popupMenuCanceled(PopupMenuEvent evt) { }' + CrLf + Indent3 +
      'public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) { ' + CrLf;
  Result := Indent2 + Result + Indent3 + '}' + CrLf + Indent2 + '});' + CrLf;
end;

function TAWTComponent.GetContextMenuListener(const Value: string): string;
begin
  Result := SurroundFix2(Value + '.addMouseListener(new MouseAdapter() {') +
    SurroundFix3('public void mouseReleased(MouseEvent evt) {') +
    SurroundFix3(Indent1 + 'if (evt.isPopupTrigger())') +
    SurroundFix3(Indent2 + Name +
    '.show(evt.getComponent(), evt.getX(), evt.getY());') + SurroundFix3('}') +
    SurroundFix2('});');
end;

function TAWTComponent.GetContainerAdd: string;
begin
  Result := AWTSwingContainer;
  if Result = '' then
    Result := 'add(' + Name + ');'
  else
    Result := Result + '.add(' + Name + ');';
end;

function TAWTComponent.AWTSwingContainer: string;
begin
  if (Parent = nil) or (Parent is TFGUIForm) then
    Result := 'cp'
  else
    Result := Parent.Name;
end;

procedure TAWTComponent.Rename(const OldName, NewName, Events: string);

  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FactionPerformed);
  Rename(FancestorMoved);
  Rename(FancestorResized);
  Rename(FcaretPositionChanged);
  Rename(FcomponentHidden);
  Rename(FcomponentMoved);
  Rename(FcomponentResized);
  Rename(FcomponentShown);
  Rename(FFocusGained);
  Rename(FFocusLost);
  Rename(FhierarchyChanged);
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
  Rename(FmouseWheelMoved);
  Rename(FpropertyChange);
  Rename(FtextValueChanged);
  Rename(FitemStateChanged);
  Rename(FadjustmentValueChanged);
  Rename(FcomponentAdded);
  Rename(FcomponentRemoved);
end;

procedure TAWTComponent.RenameMenu(const OldName, NewName, J: string);
var
  AttributeS, AttributeE, ComponentS, ComponentE, MethodsS, MethodsE: Integer;
begin
  AttributeS := FPartner.GetLNGStartAttributes;
  AttributeE := FPartner.GetLNGEndAttributes;

  ComponentS := FPartner.GetLNGStartComponents;
  ComponentE := FPartner.GetLNGEndComponents;

  MethodsS := FPartner.GetLNGStartEventMethods;
  MethodsE := FPartner.GetLNGEndEventMethods;

  // event methods
  FPartner.ReplaceTextWithRegex('public void ' + OldName +
    '_(.*)\(ActionEvent evt\)', 'public void ' + NewName +
    '_$1(ActionEvent evt)', True, MethodsS, MethodsE);
  FPartner.ReplaceTextWithRegex('// end of ' + OldName + '(.*)',
    '// end of ' + NewName + '$1', True, MethodsS, MethodsE);

  // listener
  FPartner.ReplaceTextWithRegex(OldName + '_(.*).addActionListener\(',
    NewName + '_$1.addActionListener(', True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex(OldName + '_(.*)_ActionPerformed\(evt\)',
    NewName + '_$1_ActionPerformed(evt)', True, ComponentS, ComponentE);

  // declaration of menu variables
  FPartner.ReplaceTextWithRegex('private ' + J + 'Menu ' + OldName +
    '_(.*) = new ', 'private ' + J + 'Menu ' + NewName + '_$1 = new ', True,
    AttributeS, AttributeE);
  FPartner.ReplaceTextWithRegex('private ' + J + 'MenuItem ' + OldName +
    '_(.*) = new ', 'private ' + J + 'MenuItem ' + NewName + '_$1 = new ', True,
    AttributeS, AttributeE);

  // creation of menu
  FPartner.ReplaceTextWithRegex(OldName + '(.*).add\(', NewName + '$1.add(',
    True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex(OldName + '(.*).addSeparator\(',
    NewName + '$1.addSeparator(', True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex(OldName + '(.*).setAccelerator\(',
    NewName + '$1.setAccelerator(', True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex(OldName + '(.*).setShortcut\(',
    NewName + '$1.setShortcut(', True, ComponentS, ComponentE);
  FPartner.ReplaceTextWithRegex('\(' + OldName + '(.*)\);',
    '(' + NewName + '$1);', True, ComponentS, ComponentE);
end;

procedure TAWTComponent.DefaultComponent;
begin
  SetPositionAndSize;
  if ShowFont then
    MakeFont;
  FPartner.InsertComponent(Indent2 + AddVariable + CrLf);
end;

function TAWTComponent.AddVariable: string;
begin
  Result := GetContainer + '.add(' + Name + ');';
end;

function TAWTComponent.GetContainer: string;
begin
  if (Parent = nil) or (Parent is TFGUIForm) then
    Result := 'cp'
  else
    Result := Parent.Name;
end;

procedure TAWTComponent.MakeCursor(const Value: string);
begin
  if Value = 'DEFAULT' then
    MakeAttribut('Cursor', '')
  else
    MakeAttribut('Cursor', 'Cursor.getPredefinedCursor(Cursor.' + Value +
      '_CURSOR)');
end;

procedure TAWTComponent.MakeFont;
var
  Str, Map: string;
  IntStyle: Integer;
begin
  if (Name = '') or (Abs(Tag) in [7, 42, 43, 44, 50, 52]) then
    Exit; // ButtonGroup, Menu, ...

  Map := Name + '_map';
  FPartner.DeleteAttributeValues(Map);
  FPartner.DeleteAttributeValue(Name + '.setFont');

  if ((fsUnderline in Font.Style) or (fsStrikeOut in Font.Style)) then
  begin
    Str := Indent2 + 'Hashtable<TextAttribute, Object> ' + Map +
      ' = new Hashtable<TextAttribute, Object>();' + CrLf + Indent2 + Map +
      '.put(TextAttribute.FAMILY, "' + Font.Name + '");' + CrLf + Indent2 + Map
      + '.put(TextAttribute.SIZE, new Integer(' + IntToStr(Font.Size) +
      '));' + CrLf;
    if fsUnderline in Font.Style then
      Str := Str + Indent2 + Map +
        '.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);' + CrLf;
    if fsStrikeOut in Font.Style then
      Str := Str + Indent2 + Map +
        '.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);' +
        CrLf;
    Str := Str + Indent2 + Name + '.setFont(new Font(' + Map + '));';
    if Tag = 34 then
      FPartner.SetAttributValue(GetContainerAdd, '___XXX___', Str, 1)
    else
      FPartner.SetAttributValue(GetContainerAdd, '___XXX___', Str, 0);

    FPartner.InsertImport('java.util.Hashtable');
    FPartner.InsertImport('java.awt.font.TextAttribute');
  end
  else
  begin
    Str := Indent2 + Name + '.setFont(new Font("' + Font.Name + '", ';
    IntStyle := 0;
    if fsBold in Font.Style then
      Inc(IntStyle, 1);
    if fsItalic in Font.Style then
      Inc(IntStyle, 2);
    case IntStyle of
      0:
        Str := Str + 'Font.PLAIN, ';
      1:
        Str := Str + 'Font.BOLD, ';
      2:
        Str := Str + 'Font.ITALIC, ';
      3:
        Str := Str + 'Font.BOLD + Font.ITALIC, ';
    end;
    Str := Str + IntToStr(Font.Size) + '));';
    if Tag = 34 then
      FPartner.SetAttributValue(GetContainerAdd, Name + '.setFont', Str, 1)
    else
      FPartner.SetAttributValue(GetContainerAdd, Name + '.setFont', Str, 0);
  end;
  if Foreground <> Font.Color then
  begin
    Foreground := Font.Color;
    MakeColor('Foreground', TColorToString(Font.Color));
    FObjectInspector.UpdatePropertyInspector;
  end;
  SizeToText;
end;

procedure TAWTComponent.MakeColor(const Attr, Value: string);
begin
  if Value = '(NONE)' then
  begin
    MakeAttribut(Attr, '');
    if Attr = 'Foreground' then
      Foreground := DefaultForeground;
  end
  else
    MakeAttribut(Attr, GetAttrColor(Value));
end;

function TAWTComponent.GetAttrColor(const Value: string): string;
var
  Str, DValue: string;
begin
  DValue := Java2DelphiColors(Value);
  if Copy(DValue, 1, 3) = '$00' then
    Str := 'new Color(' + TurnRGB('0x' + Copy(DValue, 4, 6)) + ')'
  else if Copy(Value, 1, 2) = '0x' then
    Str := 'new Color(' + Value + ')'
  else
    Str := 'Color.' + Value;
  if (Value = '(NONE)') or (Value = '(none)') then
    Result := ''
  else
    Result := Str;
end;

procedure TAWTComponent.MakeEchoChar(const Value: string);
var
  Key: string;
begin
  Key := Name + '.setEchoChar(';
  if Value = '' then
    FPartner.DeleteAttributeValue(Key)
  else
    SetAttributValue(Key, Key + '''' + Value + ''');');
end;

procedure TAWTComponent.MakeText(StringList: TStrings);
begin
  var
  Str := '';
  for var I := 0 to StringList.Count - 1 do
    Str := Str + StringList[I] + '\n';
  Delete(Str, Length(Str) - 1, 2);
  MakeAttribut('Text', AsString(Str));
end;

procedure TAWTComponent.CalculateMenus(MenuItems, Menu, ConstructMenu,
  Methods: TStrings; const J: string; NewMenuBar: Boolean);
var
  Posi, Indent: Integer;
  IndentAsString, TrimS, Str, ShortCut: string;
  MenuName: array [-1 .. 10] of string;

  function hasSubMenu(Items: TStrings; Num: Integer): Boolean;
  begin
    Result := (Num < Items.Count - 1) and
      (LeftSpaces(Items[Num], 2) < LeftSpaces(Items[Num + 1], 2));
  end;

  function getShortCut(Str: string): string;
  var
    Ctrl, Shift: Boolean;
  begin
    Ctrl := (Pos('Ctrl+', Str) = 1);
    if Ctrl then
      Delete(Str, 1, 5);
    Shift := (Pos('Shift+', Str) = 1);
    if Shift then
      Delete(Str, 1, 6);
    if J = '' then
    begin // awt
      Result := '.setShortcut(new MenuShortcut(KeyEvent.VK_' + Str + ', ';
      if Shift then
        Result := Result + 'true));'
      else
        Result := Result + 'false));';
    end
    else
    begin // swing
      Result := '.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_' +
        Str + ', ';
      if Ctrl then
        Result := Result + 'KeyEvent.CTRL_DOWN_MASK';
      if Ctrl and Shift then
        Result := Result + ' + ';
      if Shift then
        Result := Result + 'InputEvent.SHIFT_DOWN_MASK';
      Result := Result + '));';
    end;
  end;

begin
  MenuName[-1] := Name;
  if NewMenuBar then
    ConstructMenu.Add(Indent2 + 'set' + J + 'MenuBar(' + Name + ');');
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
      Menu.Add(IndentAsString + 'private ' + J + 'Menu ' + MenuName[Indent] +
        ' = new ' + J + 'Menu("' + TrimS + '");');
    end
    else
    begin
      if TrimS = '-' then
      begin
        ConstructMenu.Add(Indent2 + MenuName[Indent - 1] + '.addSeparator();');
        Continue;
      end
      else
      begin
        Menu.Add(IndentAsString + 'private ' + J + 'MenuItem ' +
          MenuName[Indent] + ' = new ' + J + 'MenuItem("' + TrimS + '");');
        Methods.Add(MenuName[Indent]);
      end;
    end;
    ConstructMenu.Add(Indent2 + MenuName[Indent - 1] + '.add(' +
      MenuName[Indent] + ');');
    if ShortCut <> '' then
      ConstructMenu.Add(Indent2 + MenuName[Indent] + ShortCut);
  end;
end;

procedure TAWTComponent.MakeMenuItems(OldItems, NewItems: TStrings;
  NewMenuBar: Boolean = False);
var
  J: string;
  OldMenu: TStringList;
  OldConstructMenu: TStringList;
  OldMethods: TStringList;
  NewMenu: TStringList;
  NewConstructMenu: TStringList;
  NewMethods: TStringList;

  function getListener(Method: string): string;
  begin
    Result := Indent2 + Method + '.addActionListener(new ActionListener() {' +
      CrLf + Indent3 + 'public void actionPerformed(ActionEvent evt) {' + CrLf +
      Indent3 + Indent1 + Method + '_ActionPerformed(evt);' + CrLf + Indent3 +
      '}' + CrLf + Indent2 + '});';
  end;

begin
  if Tag < 0 then
    J := ''
  else
    J := 'J';

  FPartner.Editor.BeginUpdate;
  FormatItems(NewItems);
  OldMenu := TStringList.Create;
  OldConstructMenu := TStringList.Create;
  OldMethods := TStringList.Create;
  NewMenu := TStringList.Create;
  NewConstructMenu := TStringList.Create;
  NewMethods := TStringList.Create;

  try
    CalculateMenus(OldItems, OldMenu, OldConstructMenu, OldMethods, J,
      NewMenuBar);
    CalculateMenus(NewItems, NewMenu, NewConstructMenu, NewMethods, J,
      NewMenuBar);
    FPartner.DeleteOldAddNewMethods(OldMethods, NewMethods);
    for var I := 0 to OldConstructMenu.Count - 1 do
      FPartner.DeleteAttributeValue(OldConstructMenu[I]);
    for var I := 0 to OldMenu.Count - 1 do
      FPartner.DeleteAttribute(OldMenu[I]);
    for var I := 0 to OldMethods.Count - 1 do
      FPartner.DeleteListener(getListener(OldMethods[I]));
    for var I := 0 to NewMenu.Count - 1 do
      FPartner.InsertAttribute(0, Indent1 + NewMenu[I] + CrLf);
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

procedure TAWTComponent.DeleteMenuItems(MenuItemsOld, MenuItems: TStrings);
begin
  MenuItemsOld.Text := MenuItems.Text;
  MenuItems.Clear;
  MakeMenuItems(MenuItemsOld, MenuItems);
end;

procedure TAWTComponent.MakeListener(const Value: string);
var
  Int: Integer;
begin
  Int := FPartner.GetLineNumberWith
    (Name + '.show(evt.getComponent(), evt.getX()');
  FPartner.ReplaceLineWith(Int - 3, Indent2 + Value +
    '.addMouseListener(new MouseAdapter() {');
end;

procedure TAWTComponent.MakeSelectedIndex(const Value: string);
begin
  SetAttributValue(Name + '.select(', Name + '.select(' + Value + ');');
end;

end.
