unit UJEComponents;

interface

uses
  Controls,
  Classes,
  Graphics,
  UEditorForm;

const
  DefaultMenu =
    'File'#13#10'  New'#13#10'    Java'#13#10'    XML'#13#10'  Load'#13#10'  Save, Ctrl+Shift+S'#13#10
    + 'Edit'#13#10'  Copy, Ctrl+C'#13#10'  Paste, Ctrl+V'#13#10'  -'#13#10'  Delete';

type

  TScrollBarPolicy = (AS_NEEDED, NEVER, ALWAYS);
  TOrientation = (HORIZONTAL, VERTICAL);

  TJEComponent = class(TCustomControl)
  private
    FSizeable: Boolean;
    FBackground: TColor;
    FForeground: TColor;
    FJavaType: string;
    procedure SetBackground(AColor: TColor);
    procedure SetForeground(AColor: TColor);
  protected
    FPartner: TFEditForm;
    function DefaultItems: string;
    procedure DeleteEvents;
    function MakeEventProcedure(const Event: string): string; virtual; abstract;
    function GetListener(const Event: string): string; virtual; abstract;
    procedure MakeAttribut(const Attr, Value: string);
    procedure SetAttributValue(const Key: string; Str: string); virtual;
    procedure CanvasFontAssign;
    procedure SizeToText(const AText: string; Offset: Integer = 0); overload;
    procedure FormatItems(Items: TStrings);
  public
    function MakeEventProcedureName(const Event: string): string; virtual;
    procedure AddListener(const Event: string); virtual;
    procedure DeleteListener(const Event: string); virtual; abstract;
    procedure SetPositionAndSize; virtual; abstract;
    procedure SizeToText; overload; virtual;
    procedure NameFromText; virtual;
    function GetEvents(ShowEvents: Integer): string; virtual; abstract;
    function GetAttributes(ShowAttributes: Integer): string; virtual; abstract;
    procedure SetAttribute(Attr, Value, Typ: string); virtual; abstract;
    procedure DeleteComponent; virtual;
    function AsString(const Str: string): string;
    procedure NewControl; virtual; abstract;
    procedure Rename(const OldName, NewName, Events: string); virtual;
    procedure MakeUniqueName(const FromText: string = ''); virtual;
    procedure Zooming(InOut: Boolean);
    function GetIndentation: string;
    function IsNumeric(const Str: string): Boolean;
    procedure MakeFont; virtual; abstract;
    function HasScrollpane: Boolean;
    function GetContainerAdd: string; virtual; abstract;
    function Indent1: string;
    function Indent2: string;
    function Indent3: string;
    function SurroundIndent(const Str: string): string;
    function SurroundFix(const Str: string): string;
    function SurroundFix2(const Str: string): string;
    function SurroundFix3(const Str: string): string;
    function PPIScale(ASize: Integer): Integer;
    function PPIUnScale(ASize: Integer): Integer;

    class function IsValidComponentName(const Str: string): Boolean;
    class function Tag2JavaType(Tag: Integer): string;
    property Sizeable: Boolean read FSizeable write FSizeable;
  published
    property Cursor;
    property Font;
    property Foreground: TColor read FForeground write SetForeground;
    property Background: TColor read FBackground write SetBackground;
    property JavaType: string read FJavaType write FJavaType;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Math,
  Forms,
  JvGnugettext,
  UObjectInspector,
  UConfiguration,
  UUtils;

class function TJEComponent.Tag2JavaType(Tag: Integer): string;
begin
  Result := '';
  case Tag of
    // AWT
    - 1:
      Result := 'Label';
    -2:
      Result := 'TextField';
    -3:
      Result := 'TextArea';
    -4:
      Result := 'Button';
    -5:
      Result := 'Checkbox';
    -6:
      Result := 'Checkbox'; // 'RadioButton';
    -7:
      Result := 'CheckboxGroup';
    -8:
      Result := 'List';
    -9:
      Result := 'Choice';
    -10:
      Result := 'Scrollbar';
    -11:
      Result := 'ScrollPane';
    -12:
      Result := 'Panel';
    -13:
      Result := 'Canvas';
    -14:
      Result := 'Turtle';
    -21:
      Result := 'NumberField';
    -38:
      Result := 'Panel';
    -39:
      Result := 'Canvas';
    -42:
      Result := 'MenuBar'; // deprecated
    -43:
      Result := 'Menu';
    -44:
      Result := 'PopupMenu';
    -50:
      Result := 'ButtonGroup'; // new Buttongroup
    -52:
      Result := 'MenuBar'; // new MenuBar
    // Swing
    1:
      Result := 'JLabel';
    2:
      Result := 'JTextField';
    3:
      Result := 'JTextArea'; // gets a JScrollPane
    4:
      Result := 'JButton';
    5:
      Result := 'JCheckBox';
    6:
      Result := 'JRadioButton';
    7:
      Result := 'ButtonGroup'; // yes, without J
    8:
      Result := 'JList'; // gets a JScrollPane
    9:
      Result := 'JComboBox';
    10:
      Result := 'JScrollBar';
    11:
      Result := 'JScrollPane'; // is a ScrollPane
    12:
      Result := 'JPanel';
    13:
      Result := 'Canvas';
    14:
      Result := 'Turtle';
    15:
      Result := 'JSlider';
    16:
      Result := 'JProgressBar';
    17:
      Result := 'JSplitPane';
    18:
      Result := 'JTabbedPane';
    19:
      Result := 'JTable'; // gets a JScrollPane
    20:
      Result := 'JTree'; // gets a JScrollPane
    21:
      Result := 'JNumberField';
    22:
      Result := 'JSpinner';
    23:
      Result := 'JToolBar';
    24:
      Result := 'JSeparator';
    25:
      Result := 'JToggleButton';
    26:
      Result := 'JPasswordField';
    27:
      Result := 'JFormattedTextField';
    28:
      Result := 'JEditorPane'; // gets a JScrollPane
    29:
      Result := 'JTextPane'; // gets a JScrollPane
    30:
      Result := 'JLayeredPane';
    31:
      Result := 'JDesktopPane';
    32:
      Result := 'JInternalFrame';
    33:
      Result := 'Playground';
    34:
      Result := 'Turtle';
    38:
      Result := 'Canvas'; // SubPanelCanvas - SubPanel
    39:
      Result := 'Canvas'; // SubPanelCanvas - SubCanvas

    42:
      Result := 'JMenuBar'; // deprecated
    43:
      Result := 'JMenu';
    44:
      Result := 'JPopupMenu';

    45:
      Result := 'JFileChooser';
    46:
      Result := 'JFileChooser';
    47:
      Result := 'JColorChooser';
    48:
      Result := 'JOptionPane';
    49:
      Result := 'Timer';

    50:
      Result := 'JButtonGroup';
    52:
      Result := 'JMenuBar'; // new JMenuBar

    // FX
    101:
      Result := 'Label';
    102:
      Result := 'TextField';
    103:
      Result := 'NumberField';
    104:
      Result := 'TextArea';
    105:
      Result := 'Button';
    106:
      Result := 'CheckBox';
    107:
      Result := 'RadioButton';
    108:
      Result := 'ToggleGroup'; // deprecated but ToggleButton
    109:
      Result := 'ListView';
    110:
      Result := 'ComboBox';
    111:
      Result := 'Spinner';
    112:
      Result := 'ScrollBar';
    113:
      Result := 'ScrollPane';
    114:
      Result := 'Canvas';
    115:
      Result := 'MenuBar'; // deprecated
    116:
      Result := 'Menu';
    117:
      Result := 'ContextMenu';
    118:
      Result := 'MenuButton';
    119:
      Result := 'SplitMenuButton';
    120:
      Result := 'Turtle';
    121:
      Result := 'Pane';
    122:
      Result := 'Canvas'; // SubCanvas
    123:
      Result := 'ButtonGroup';
    124:
      Result := 'MenuBar'; // new MenuBar

    131:
      Result := 'Slider';
    132:
      Result := 'ProgressBar';
    133:
      Result := 'ProgressIndicator';
    134:
      Result := 'ToolBar';
    135:
      Result := 'Separator';
    136:
      Result := 'ToggleButton';
    137:
      Result := 'PasswordField';
    138:
      Result := 'ChoiceBox';
    139:
      Result := 'Hyperlink';
    140:
      Result := 'HTMLEditor';
    141:
      Result := 'ColorPicker';
    142:
      Result := 'DatePicker';
    143:
      Result := 'Pagination';
    144:
      Result := 'FileChooser';
    145:
      Result := 'FileChooser';
    146:
      Result := 'DirectoryChooser';
    147:
      Result := 'ImageView';
    148:
      Result := 'WebView';
    149:
      Result := 'TableView';
    150:
      Result := 'MediaView';
    151:
      Result := 'TreeTableView';
    152:
      Result := 'TreeView';

    161:
      Result := 'Circle';
    162:
      Result := 'Rectangle';
    163:
      Result := 'Ellipse';
    164:
      Result := 'Polygon';
    165:
      Result := 'Polyline';
    166:
      Result := 'Arc';
    167:
      Result := 'Line';
    168:
      Result := 'Text';
    169:
      Result := 'QuadCurve';
    170:
      Result := 'CubicCurve';
    171:
      Result := 'SVGPath';
  end;
end;

function TJEComponent.AsString(const Str: string): string;
begin
  Result := '"' + Str + '"';
end;

procedure TJEComponent.Rename(const OldName, NewName, Events: string);
begin
  FPartner.ReplaceComponentname(OldName, NewName, GetEvents(3));
  FObjectInspector.UpdateEventInspector;
end;

procedure TJEComponent.MakeUniqueName(const FromText: string = '');
var
  Int: Integer;
  Basename, Testname: string;
begin
  if FromText <> '' then
    Basename := OnlyCharsAndDigits(ChangeVowels(FromText));
  if (FromText = '') or (Basename = '') then
    Basename := JavaType;
  Int := Length(Basename);
  while Pos(Basename[Int], '0123456789') > 0 do
    Dec(Int);
  Testname := LowerUpper(Copy(Basename, 1, Int));
  Int := 0;
  while Owner.FindComponent(Testname) <> nil do
  begin
    Inc(Int);
    Testname := Basename + IntToStr(Int);
  end;
  Name := Testname;
end;

procedure TJEComponent.DeleteEvents;
var
  StringList: TStringList;
begin
  StringList := Split('|', GetEvents(3));
  for var I := 0 to StringList.Count - 1 do
    if (StringList[I] <> '') and (GetStrProp(Self, StringList[I]) <> '')
    then
      DeleteListener(StringList[I]);
  FreeAndNil(StringList);
end;

procedure TJEComponent.DeleteComponent;
begin
  DeleteEvents;
  FPartner.DeleteComponentDefault(Self);
end;

function TJEComponent.DefaultItems: string;
begin
  Result := _('America') + #13#10 + _('Europe') + #13#10 + _('Asia');
end;

function TJEComponent.MakeEventProcedureName(const Event: string): string;
begin
  Result := Name + '_' + UpperLower(Event);
end;

procedure TJEComponent.AddListener(const Event: string);
var
  Listener, EventProcedureName: string;
begin
  Listener := GetListener(Event);
  FPartner.InsertListener(GetContainerAdd, Listener);
  EventProcedureName := MakeEventProcedureName(Event);
  if not FPartner.HasText('public void ' + EventProcedureName) then
    FPartner.InsertProcedure(0, MakeEventProcedure(Event));

  TThread.ForceQueue(nil,
    procedure
    begin
      FObjectInspector.ELEventInspector.SetByCaption(Event, EventProcedureName);
    end);
end;

procedure TJEComponent.MakeAttribut(const Attr, Value: string);
begin
  var
  Str := Name + '.set' + Attr;
  if Value = '' then
    FPartner.DeleteAttributeValue(Str)
  else
    SetAttributValue(Str, Str + '(' + Value + ');');
end;

procedure TJEComponent.SetAttributValue(const Key: string; Str: string);
begin
  if Pos(Indent2, Str) = 0 then
    Str := Indent2 + Str;
  FPartner.SetAttributValue(GetContainerAdd, Key, Str, 0);
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
  var
  AFont := TFont.Create;
  AFont.Assign(Font);
  AFont.Size := AFont.Size - PPIScale(3);
  Canvas.Font.Assign(AFont);
  AFont.Free;
end;

procedure TJEComponent.SizeToText(const AText: string; Offset: Integer = 0);
var
  Delta1, Delta2: Integer;
begin
  CanvasFontAssign;
  Delta1 := Canvas.TextWidth(AText) + Offset + 4 - Width;
  if Delta1 > 0 then
    Width := Width + Delta1;
  Delta2 := Canvas.TextHeight(AText) + 4 - Height;
  if Delta2 > 0 then
    Height := Height + Delta2;
  if (Delta1 > 0) or (Delta2 > 0) then
    SetPositionAndSize;
end;

procedure TJEComponent.FormatItems(Items: TStrings);
var
  Str, TrimS: string;
  Int, LeftNum, Indent: Integer;
begin
  Indent := 0;
  Int := 0;
  while Int < Items.Count do
  begin
    Str := Items[Int];
    TrimS := Trim(Str);
    if TrimS = '' then
    begin
      Items.Delete(Int);
      Continue;
    end;
    LeftNum := LeftSpaces(Str, 2);
    if LeftNum > Indent then
    begin
      Inc(Indent, 2);
      if LeftNum <> Indent then
        Items[Int] := StringOfChar(' ', Indent) + TrimS;
    end
    else if LeftNum < Indent then
    begin
      if LeftNum mod 2 = 1 then
      begin
        Items[Int] := StringOfChar(' ', LeftNum + 1) + TrimS;
        Indent := LeftNum + 1;
      end
      else
        Indent := LeftNum;
    end;
    Inc(Int);
  end;
end;

procedure TJEComponent.Zooming(InOut: Boolean);
begin
  if (Name = '') or (Abs(Tag) in [7, 42, 43, 44, 50, 52, 115, 116, 117, 124])
  then
    Exit; // ButtonGroup, Menu, ...
  if InOut then
    Font.Size := Font.Size + FConfiguration.ZoomSteps
  else
    Font.Size := Max(Font.Size - FConfiguration.ZoomSteps, 6);
  CanvasFontAssign;
  MakeFont;
  // if Sizeable then
  // SetPositionAndSize;
  Invalidate;
end;

function TJEComponent.HasScrollpane: Boolean;
begin
  Result := Tag in [3, 8, 19, 20, 28, 29];
end;

function TJEComponent.GetIndentation: string;
begin
  Result := Indent1;
  var
  AParent := Self.Parent;
  while Assigned(AParent) and (AParent is TJEComponent) do
  begin
    Result := Result + Indent1;
    AParent := AParent.Parent;
  end;
end;

function TJEComponent.IsNumeric(const Str: string): Boolean;
var
  ADouble: Double;
begin
  Result := TryStrToFloat(Str, ADouble);
  if not Result and ((Str = '-') or (Str = '.') or (Str = '')) then
    Result := True;
end;

class function TJEComponent.IsValidComponentName(const Str: string): Boolean;
begin
  Result := True;
  for var I := -44 to 171 do
    if Str = Tag2JavaType(I) then
      Result := False;
end;

procedure TJEComponent.SetForeground(AColor: TColor);
begin
  if AColor <> FForeground then
  begin
    FForeground := AColor;
    Invalidate;
  end;
end;

procedure TJEComponent.SetBackground(AColor: TColor);
begin
  if AColor <> FBackground then
  begin
    FBackground := AColor;
    Invalidate;
  end;
end;

function TJEComponent.Indent1: string;
begin
  Result := FConfiguration.Indent1;
end;

function TJEComponent.Indent2: string;
begin
  Result := FConfiguration.Indent2;
end;

function TJEComponent.Indent3: string;
begin
  Result := FConfiguration.Indent3;
end;

function TJEComponent.SurroundIndent(const Str: string): string;
begin
  Result := GetIndentation + Str + #13#10;
end;

function TJEComponent.SurroundFix(const Str: string): string;
begin
  Result := Indent1 + Str + #13#10;
end;

function TJEComponent.SurroundFix2(const Str: string): string;
begin
  Result := Indent2 + Str + #13#10;
end;

function TJEComponent.SurroundFix3(const Str: string): string;
begin
  Result := Indent3 + Str + #13#10;
end;

function TJEComponent.PPIScale(ASize: Integer): Integer;
begin
  Result := myMulDiv(ASize, (Owner as TForm).CurrentPPI, 96);
  // MulDiv needs Windows, but then we get a strange error with BitmapFromRelativePath
end;

function TJEComponent.PPIUnScale(ASize: Integer): Integer;
begin
  Result := myMulDiv(ASize, 96, (Owner as TForm).CurrentPPI);
end;

end.
