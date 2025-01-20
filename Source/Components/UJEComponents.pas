unit UJEComponents;

interface

uses
  Controls, Classes, Graphics, UEditorForm;

const DefaultMenu =
  'File'#13#10'  New'#13#10'    Java'#13#10'    XML'#13#10'  Load'#13#10'  Save, Ctrl+Shift+S'#13#10 +
  'Edit'#13#10'  Copy, Ctrl+C'#13#10'  Paste, Ctrl+V'#13#10'  -'#13#10'  Delete';

type

  TScrollBarPolicy = (AS_NEEDED, NEVER, ALWAYS);
  TOrientation = (HORIZONTAL, VERTICAL);

  TJEComponent = class(TCustomControl)
  private
    FSizeable: boolean;
    FBackground: TColor;
    FForeground: TColor;
    FJavaType: string;
    procedure SetBackground(aColor: TColor);
    procedure SetForeground(aColor: TColor);
  protected
    Partner: TFEditForm;
    function defaultItems: string;
    procedure DeleteEvents;
    function MakeEventProcedure(const Event: string ): string; virtual; abstract;
    function getListener(const event: string): string; virtual; abstract;
    procedure MakeAttribut(Attr, Value: string);
    procedure setAttributValue(key, s: string); virtual;
    procedure CanvasFontAssign;
    procedure SizeToText(const aText: string; offset: integer = 0); overload;
    procedure FormatItems(Items: TStrings);
  public
    function MakeEventProcedureName(const event: string): string; virtual;
    procedure AddListener(const event: string); virtual;
    procedure DeleteListener(const event: string); virtual; abstract;
    procedure SetPositionAndSize; virtual; abstract;
    procedure SizeToText; overload; virtual;
    procedure NameFromText; virtual;
    function getEvents(ShowEvents: integer): string; virtual; abstract;
    function getAttributes(ShowAttributes: integer): string; virtual; abstract;
    procedure setAttribute(Attr, Value, Typ: string); virtual; abstract;
    procedure DeleteComponent; virtual;
    function asString(s: string): string;
    procedure NewControl; virtual; abstract;
    procedure Rename(const OldName, NewName, Events: string); virtual;
    procedure MakeUniqueName(FromText: string = ''); virtual;
    procedure Zooming(_in: boolean);
    function GetIndentation: string;
    procedure MakeFont; virtual; abstract;
    function hasScrollpane: boolean;
    function GetContainerAdd: string; virtual; abstract;
    function Indent1: string;
    function Indent2: string;
    function Indent3: string;
    function surroundIndent(s: string): string;
    function surroundFix(s: string): string;
    function surroundFix2(s: string): string;
    function surroundFix3(s: string): string;
    function PPIScale(ASize: integer): integer;
    function PPIUnScale(ASize: integer): integer;

    class function isValidComponentName(const s: string): boolean;
    class function Tag2JavaType(Tag: integer): string;
    property Sizeable: boolean read FSizeable write FSizeable;
  published
    property Cursor;
    property Font;
    property Foreground: TColor read FForeground write setForeground;
    property Background: TColor read FBackground write setBackground;
    property JavaType: string read FJavaType write FJavaType;
  end;

implementation

uses SysUtils, TypInfo, Math, Forms, JvGnugettext,
     UObjectInspector, UConfiguration, UUtils;

class function TJEComponent.Tag2JavaType(Tag: integer): string;
begin
  Result:= '';
  case Tag of
    // AWT
     -1: Result:= 'Label';
     -2: Result:= 'TextField';
     -3: Result:= 'TextArea';
     -4: Result:= 'Button';
     -5: Result:= 'Checkbox';
     -6: Result:= 'Checkbox'; // 'RadioButton';
     -7: Result:= 'CheckboxGroup';
     -8: Result:= 'List';
     -9: Result:= 'Choice';
    -10: Result:= 'Scrollbar';
    -11: Result:= 'ScrollPane';
    -12: Result:= 'Panel';
    -13: Result:= 'Canvas';
    -14: Result:= 'Turtle';
    -21: Result:= 'NumberField';
    -38: Result:= 'Panel';
    -39: Result:= 'Canvas';
    -42: Result:= 'MenuBar';     // deprecated
    -43: Result:= 'Menu';
    -44: Result:= 'PopupMenu';
    -50: Result:= 'ButtonGroup'; // new Buttongroup
    -52: Result:= 'MenuBar';     // new MenuBar
    // Swing
      1: Result:= 'JLabel';
      2: Result:= 'JTextField';
      3: Result:= 'JTextArea';      // gets a JScrollPane
      4: Result:= 'JButton';
      5: Result:= 'JCheckBox';
      6: Result:= 'JRadioButton';
      7: Result:= 'ButtonGroup';    // yes, without J
      8: Result:= 'JList';          // gets a JScrollPane
      9: Result:= 'JComboBox';
     10: Result:= 'JScrollBar';
     11: Result:= 'JScrollPane';    // is a ScrollPane
     12: Result:= 'JPanel';
     13: Result:= 'Canvas';
     14: Result:= 'Turtle';
     15: Result:= 'JSlider';
     16: Result:= 'JProgressBar';
     17: Result:= 'JSplitPane';
     18: Result:= 'JTabbedPane';
     19: Result:= 'JTable';         // gets a JScrollPane
     20: Result:= 'JTree';          // gets a JScrollPane
     21: Result:= 'JNumberField';
     22: Result:= 'JSpinner';
     23: Result:= 'JToolBar';
     24: Result:= 'JSeparator';
     25: Result:= 'JToggleButton';
     26: Result:= 'JPasswordField';
     27: Result:= 'JFormattedTextField';
     28: Result:= 'JEditorPane';    // gets a JScrollPane
     29: Result:= 'JTextPane';      // gets a JScrollPane
     30: Result:= 'JLayeredPane';
     31: Result:= 'JDesktopPane';
     32: Result:= 'JInternalFrame';
     33: Result:= 'Playground';
     34: Result:= 'Turtle';
     38: Result:= 'Canvas'; // SubPanelCanvas - SubPanel
     39: Result:= 'Canvas'; // SubPanelCanvas - SubCanvas

     42: Result:= 'JMenuBar'; // deprecated
     43: Result:= 'JMenu';
     44: Result:= 'JPopupMenu';

     45: Result:= 'JFileChooser';
     46: Result:= 'JFileChooser';
     47: Result:= 'JColorChooser';
     48: Result:= 'JOptionPane';
     49: Result:= 'Timer';

     50: Result:= 'JButtonGroup';
     52: Result:= 'JMenuBar'; // new JMenuBar


    // FX
    101: Result:= 'Label';
    102: Result:= 'TextField';
    103: Result:= 'NumberField';
    104: Result:= 'TextArea';
    105: Result:= 'Button';
    106: Result:= 'CheckBox';
    107: Result:= 'RadioButton';
    108: Result:= 'ToggleGroup'; // deprecated but ToggleButton
    109: Result:= 'ListView';
    110: Result:= 'ComboBox';
    111: Result:= 'Spinner';
    112: Result:= 'ScrollBar';
    113: Result:= 'ScrollPane';
    114: Result:= 'Canvas';
    115: Result:= 'MenuBar'; // deprecated
    116: Result:= 'Menu';
    117: Result:= 'ContextMenu';
    118: Result:= 'MenuButton';
    119: Result:= 'SplitMenuButton';
    120: Result:= 'Turtle';
    121: Result:= 'Pane';
    122: Result:= 'Canvas'; // SubCanvas
    123: Result:= 'ButtonGroup';
    124: Result:= 'MenuBar';  // new MenuBar

    131: Result:= 'Slider';
    132: Result:= 'ProgressBar';
    133: Result:= 'ProgressIndicator';
    134: Result:= 'ToolBar';
    135: Result:= 'Separator';
    136: Result:= 'ToggleButton';
    137: Result:= 'PasswordField';
    138: Result:= 'ChoiceBox';
    139: Result:= 'Hyperlink';
    140: Result:= 'HTMLEditor';
    141: Result:= 'ColorPicker';
    142: Result:= 'DatePicker';
    143: Result:= 'Pagination';
    144: Result:= 'FileChooser';
    145: Result:= 'FileChooser';
    146: Result:= 'DirectoryChooser';
    147: Result:= 'ImageView';
    148: Result:= 'WebView';
    149: Result:= 'TableView';
    150: Result:= 'MediaView';
    151: Result:= 'TreeTableView';
    152: Result:= 'TreeView';

    161: Result:= 'Circle';
    162: Result:= 'Rectangle';
    163: Result:= 'Ellipse';
    164: Result:= 'Polygon';
    165: Result:= 'Polyline';
    166: Result:= 'Arc';
    167: Result:= 'Line';
    168: Result:= 'Text';
    169: Result:= 'QuadCurve';
    170: Result:= 'CubicCurve';
    171: Result:= 'SVGPath';
  end;
end;

function TJEComponent.asString(s: string): string;
begin
  Result:= '"' + s + '"';
end;

procedure TJEComponent.Rename(const OldName, NewName, Events: string);
begin
  Partner.ReplaceComponentname(OldName, NewName, getEvents(3));
  FObjectInspector.UpdateEventInspector;
end;

procedure TJEComponent.MakeUniqueName(FromText: string = '');
  var i: integer; Basename, Testname: string;
begin
  if FromText <> '' then
    BaseName:= OnlyCharsAndDigits(changeVowels(FromText));
  if (FromText = '') or (Basename = '') then
    Basename:= JavaType;
  i:= Length(Basename);
  while Pos(Basename[i], '0123456789') > 0 do
    dec(i);
  TestName:= LowerUpper(copy(Basename, 1, i));
  i:= 0;
  while Owner.FindComponent(Testname) <> nil do begin
    inc(i);
    Testname:= BaseName + IntToStr(i);
  end;
  Name:= Testname;
end;

procedure TJEComponent.DeleteEvents;
  var i: integer; SL: TStringList;
begin
  SL:= Split('|', getEvents(3));
  for i:= 0 to SL.Count - 1 do
    if (SL[i] <> '') and (GetStrProp(self, SL[i]) <> '') then
      DeleteListener(SL[i]);
  FreeAndNil(SL);
end;

procedure TJEComponent.DeleteComponent;
begin
  DeleteEvents;
  Partner.DeleteComponentDefault(Self);
end;

function TJEComponent.defaultItems: string;
begin
  Result:= _('America') + #13#10 + _('Europe') + #13#10 + _('Asia');
end;

function TJEComponent.MakeEventProcedureName(const event: string): string;
begin
  Result:= Name + '_' + UpperLower(Event);
end;

procedure TJEComponent.AddListener(const event: string);
  var Listener, EventProcedureName: string;
begin
  Listener:= getListener(Event);
  Partner.InsertListener(GetContainerAdd, Listener);
  EventProcedureName:= MakeEventProcedureName(Event);
  if not Partner.hasText('public void ' + EventProcedureName) then
    Partner.InsertProcedure(0, MakeEventProcedure(Event));

  TThread.ForceQueue(nil, procedure
    begin
      FObjectInspector.ELEventInspector.SetByCaption(event, EventProcedureName);
    end);
end;

procedure TJEComponent.MakeAttribut(Attr, Value: string);
begin
  var s:= Name  + '.set' +  Attr;
  if Value = ''
    then Partner.DeleteAttributeValue(s)
    else setAttributValue(s, s + '(' + Value + ');')
end;

procedure TJEComponent.setAttributValue(key, s: string);
begin
  if Pos(Indent2, s) = 0 then
    s:= Indent2 + s;
  Partner.setAttributValue(GetContainerAdd, key, s, 0);
end;

procedure TJEComponent.SizeToText;
begin
  // default for most components
end;

procedure TJEComponent.NameFromText;
begin
  // default for most components
  // relevant for Labels, Buttons, Checkboxes and Radiobuttons
end;

procedure TJEComponent.CanvasFontAssign;
begin
  var f:= TFont.Create;
  f.Assign(Font);
  f.Size:= f.Size - PPIScale(3);
  Canvas.Font.Assign(f);
  f.Free;
end;

procedure TJEComponent.SizeToText(const aText: string; Offset: integer = 0);
  var d1, d2: integer;
begin
  CanvasFontAssign;
  d1:= Canvas.TextWidth(aText) + Offset + 4 - Width;
  if d1 > 0 then Width:= Width + d1;
  d2:= Canvas.TextHeight(aText)+ 4 - Height;
  if d2 > 0 then Height:= Height + d2;
  if (d1 > 0) or (d2 > 0) then
    SetPositionandSize;
end;

procedure TJEComponent.FormatItems(Items: TStrings);
  var s, ts: string; i, ls, Indent: integer;
begin
  Indent:= 0;
  i:= 0;
  while i < Items.Count do begin
    s:= Items[i];
    ts:= trim(s);
    if ts = '' then begin
      Items.Delete(i);
      Continue;
    end;
    ls:= LeftSpaces(s, 2);
    if ls > Indent then begin
      inc(Indent, 2);
      if ls <> Indent then
        Items[i]:= StringOfChar(' ', Indent) + ts;
    end else if ls < Indent then begin
      if ls mod 2 = 1 then begin
        Items[i]:= StringOfchar(' ', ls + 1) + ts;
        Indent:= ls + 1;
      end else
        Indent:= ls;
    end;
    inc(i);
  end;
end;

procedure TJEComponent.Zooming(_in: boolean);
begin
  if (Name = '') or (Abs(Tag) in [7, 42, 43, 44, 50, 52, 115, 116, 117, 124]) then
    Exit;   // ButtonGroup, Menu, ...
  if _in
    then Font.Size:= Font.Size + FConfiguration.ZoomSteps
    else Font.Size:= max(Font.Size - FConfiguration.ZoomSteps, 6);
  CanvasFontAssign;
  MakeFont;
  //if Sizeable then
  //  SetPositionAndSize;
  Invalidate;
end;

function TJEComponent.hasScrollPane: boolean;
begin
  Result:= Tag in [3, 8, 19, 20, 28, 29];
end;

function TJEComponent.GetIndentation: string;
begin
  Result:= Indent1;
  var aParent:= Self.Parent;
  while assigned(aParent) and (aParent is TJEComponent) do begin
    Result:= Result + Indent1;
    aParent:= aParent.Parent;
  end;
end;

class function TJEComponent.isValidComponentName(const s: string): boolean;
  var i: integer;
begin
  Result:= true;
  for i:= -44 to 171 do
    if s =  Tag2JavaType(i) then
      Result:= false;
end;

procedure TJEComponent.SetForeground(aColor: TColor);
begin
  if aColor <> FForeground then begin
    FForeground:= aColor;
    Invalidate;
  end;
end;

procedure TJEComponent.SetBackground(aColor: TColor);
begin
  if aColor <> FBackground then begin
    FBackground:= aColor;
    Invalidate;
  end;
end;

function TJEComponent.Indent1: string;
begin
  Result:= FConfiguration.Indent1;
end;

function TJEComponent.Indent2: string;
begin
  Result:= FConfiguration.Indent2;
end;

function TJEComponent.Indent3: string;
begin
  Result:= FConfiguration.Indent3;
end;

function TJEComponent.surroundIndent(s: string): string;
begin
  Result:= GetIndentation + s + #13#10;
end;

function TJEComponent.surroundFix(s: string): string;
begin
  Result:= Indent1 + s + #13#10;
end;

function TJEComponent.surroundFix2(s: string): string;
begin
  Result:= Indent2 + s + #13#10;
end;

function TJEComponent.surroundFix3(s: string): string;
begin
  Result:= Indent3 + s + #13#10;
end;

function TJEComponent.PPIScale(ASize: integer): integer;
begin
  Result := myMulDiv(ASize, (Owner as TForm).CurrentPPI, 96);
  // MulDiv needs Windows, but then we get a strange error with BitmapFromRelativePath
end;

function TJEComponent.PPIUnScale(ASize: integer): integer;
begin
  Result := myMulDiv(ASize, 96, (Owner as TForm).CurrentPPI);
end;

end.
