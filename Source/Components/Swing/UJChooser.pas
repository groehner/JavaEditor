unit UJChooser;

{ Classes
  TJFileChooser = class (TSwingComponent)
  TJFileSaveChooser
  TJColorChooser = class (TSwingComponent)
  TJOptionPane = class (TSwingComponent)
}

interface

uses
  Classes,
  Graphics,
  UJComponents;

type

  TOptionType = (OK, INPUT, DEFAULT, YES_NO, OK_CANCEL, YES_NO_CANCEL);
  TMessageType = (ERROR, INFORMATION, WARNING, QUESTION, PLAIN);
  TFileSelectionMode = (FILES_ONLY, DIRECTORIES_ONLY, FILES_AND_DIRECTORIES);

  TJFileChooser = class(TSwingComponent)
  private
    FAcceptAllFileFilterUsed: Boolean;
    FApproveButtonMnemonic: Integer;
    FControlButtonsAreShown: Boolean;
    FApproveButtonText: string;
    FApproveButtonToolTipText: string;
    FDialogTitle: string;
    FDragEnabled: Boolean;
    FFileHidingEnabled: Boolean;
    FFileSelectionMode: TFileSelectionMode;
    FMultiSelectionEnabled: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property AcceptAllFileFilterUsed: Boolean read FAcceptAllFileFilterUsed
      write FAcceptAllFileFilterUsed;
    property ApproveButtonMnemonic: Integer read FApproveButtonMnemonic
      write FApproveButtonMnemonic;
    property ApproveButtonText: string read FApproveButtonText
      write FApproveButtonText;
    property ApproveButtonToolTipText: string read FApproveButtonToolTipText
      write FApproveButtonToolTipText;
    property ControlButtonsAreShown: Boolean read FControlButtonsAreShown
      write FControlButtonsAreShown;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DragEnabled: Boolean read FDragEnabled write FDragEnabled;
    property FileHidingEnabled: Boolean read FFileHidingEnabled
      write FFileHidingEnabled;
    property FileSelectionMode: TFileSelectionMode read FFileSelectionMode
      write FFileSelectionMode;
    property MultiSelectionEnabled: Boolean read FMultiSelectionEnabled
      write FMultiSelectionEnabled;
  end;

  TJFileSaveChooser = class(TJFileChooser)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  end;

  TJColorChooser = class(TSwingComponent)
  private
    FColor: TColor;
    FDialogTitle: string;
    FDragEnabled: Boolean;
    procedure MakeColorChooser;
    function GetColorChooser: string;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property Color: TColor read FColor write FColor;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DragEnabled: Boolean read FDragEnabled write FDragEnabled;
  end;

  TJOptionPane = class(TSwingComponent)
  private
    FComponent: string;
    FDialogTitle: string;
    FMessage: string;
    FMessageType: TMessageType;
    FOptionType: TOptionType;
    procedure MakeOptionPane;
    function GetOptionPane: string;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
    function MessageTypeAsText: string;
    function OptionTypeAsText: string;
  published
    property Component: string read FComponent write FComponent;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property Message: string read FMessage write FMessage;
    property MessageType: TMessageType read FMessageType write FMessageType;
    property OptionType: TOptionType read FOptionType write FOptionType;
  end;

implementation

uses
  Controls,
  UJava,
  ULink;

{ --- FileChooser -------------------------------------------------------------- }

constructor TJFileChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 45;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'JFileChooser';
end;

function TJFileChooser.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|DialogTitle|DragEnabled|FileSelectionMode|MultiSelectionEnabled';
  Show2 = '|AcceptAllFileFilterUsed|ApproveButtonMnemonic|ApproveButtonText' +
    '|ApproveButtonToolTipText|ControlButtonsAreShown|FileHidingEnabled';
begin
  if ShowAttributes = 1 then
    Result := Show1
  else
    Result := Show1 + Show2;
  Result := Result + inherited;
end;

procedure TJFileChooser.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'FileSelectionMode' then
    MakeAttribut(Attr, 'JFileChooser.' + Value)
  else
    inherited;
end;

procedure TJFileChooser.NewControl;
begin
  FPartner.InsertImport('java.io.*');
  InsertNewVariable('private JFileChooser ' + Name + ' = new JFileChooser();');
  var
  Str := SurroundFix('public File ' + Name + '_openFile() {') +
    SurroundFix2('if (' + Name +
    '.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {') +
    SurroundFix2(Indent1 + 'return ' + Name + '.getSelectedFile();') +
    SurroundFix2('} else {') + SurroundFix2(Indent1 + 'return null;') +
    SurroundFix2('}') + SurroundFix('}') + #13#10;
  FPartner.InsertProcedure(Str);
end;

procedure TJFileChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_openFile', NewName + '_openFile', True);
end;

procedure TJFileChooser.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_openFile', False);
end;

procedure TJFileChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 16);
end;

{ --- FileSaveChooser ---------------------------------------------------------- }

constructor TJFileSaveChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 46;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'JFileChooser';
end;

procedure TJFileSaveChooser.NewControl;
begin
  FPartner.InsertImport('java.io.*');
  InsertNewVariable('private JFileChooser ' + Name + ' = new JFileChooser();');
  var
  Str := SurroundFix('public File ' + Name + '_saveFile() {') +
    SurroundFix2('if (' + Name +
    '.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {') +
    SurroundFix2(Indent1 + 'return ' + Name + '.getSelectedFile();') +
    SurroundFix2('} else {') + SurroundFix2(Indent1 + 'return null;') +
    SurroundFix2('}') + SurroundFix('}') + #13#10;
  FPartner.InsertProcedure(Str);
end;

procedure TJFileSaveChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_saveFile', NewName + '_saveFile', True);
end;

procedure TJFileSaveChooser.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_saveFile', False);
end;

procedure TJFileSaveChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 17);
end;

{ --- ColorChooser ------------------------------------------------------------- }

constructor TJColorChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 47;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'JColorChooser';
end;

function TJColorChooser.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Color|DialogTitle|DragEnabled' + inherited;
end;

procedure TJColorChooser.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Color') or (Attr = 'DialogTitle') then
    MakeColorChooser
  else
    inherited;
end;

procedure TJColorChooser.MakeColorChooser;
begin
  FPartner.DeleteMethod(Name + '_getColor', False);
  FPartner.InsertProcedure(GetColorChooser);
end;

function TJColorChooser.GetColorChooser: string;
var
  Col: string;
begin
  Col := ToJavaColor(Color);
  if Col = 'Color.(NONE)' then
    Col := 'Color.WHITE';
  Result := SurroundFix('public Color ' + Name + '_getColor() {') +
    SurroundFix2('return ' + Name + '.showDialog(this, "' + DialogTitle + '", '
    + Col + ');') + SurroundFix('}') + #13#10;
end;

procedure TJColorChooser.NewControl;
begin
  FPartner.InsertImport('java.io.*');
  InsertNewVariable('private JColorChooser ' + Name +
    ' = new JColorChooser();');
  FPartner.InsertProcedure(GetColorChooser);
end;

procedure TJColorChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_getColor', NewName + '_getColor', True);
end;

procedure TJColorChooser.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_getColor', False);
end;

procedure TJColorChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 18);
end;

{ --- OptionPane --------------------------------------------------------------- }

constructor TJOptionPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 48;
  Height := 28;
  Width := 32;
  Sizeable := False;
  FComponent := 'this';
  JavaType := 'JOptionPane';
end;

function TJOptionPane.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Component|DialogTitle|Message|MessageType|OptionType' + inherited;
end;

procedure TJOptionPane.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Component') or (Attr = 'DialogTitle') or (Attr = 'Message') or
    (Attr = 'MessageType') or (Attr = 'OptionType') then
    MakeOptionPane
  else
    inherited;
end;

procedure TJOptionPane.MakeOptionPane;
begin
  FPartner.DeleteMethod(Name + '_ShowDialog', False);
  FPartner.InsertProcedure(GetOptionPane);
end;

function TJOptionPane.GetOptionPane: string;
var
  Str: string;
begin
  if OptionType = OK then
    Str := SurroundFix('public void ' + Name + '_ShowDialog() {') +
      SurroundFix2('JOptionPane.showMessageDialog(' + Component + ', "' +
      Message + '", "' + DialogTitle + '", ' + MessageTypeAsText + ');')
  else if OptionType = INPUT then
    Str := SurroundFix('public String ' + Name + '_ShowDialog() {') +
      SurroundFix2('return JOptionPane.showInputDialog(' + Component + ', "' +
      Message + '", "' + DialogTitle + '", ' + MessageTypeAsText + ');')
  else
    Str := SurroundFix('public int ' + Name + '_ShowDialog() {') +
      SurroundFix2('return JOptionPane.showConfirmDialog(' + Component + ', "' +
      Message + '", "' + DialogTitle + '", ' + OptionTypeAsText + ', ' +
      MessageTypeAsText + ');');
  Result := Str + SurroundFix('}') + #13#10;
end;

procedure TJOptionPane.NewControl;
begin
  FPartner.InsertImport('java.io.*');
  InsertNewVariable('private JOptionPane ' + Name + ' = new JOptionPane();');
  FPartner.InsertProcedure(GetOptionPane);
end;

procedure TJOptionPane.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_ShowDialog', NewName + '_ShowDialog', True);
end;

procedure TJOptionPane.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_ShowDialog', False);
end;

procedure TJOptionPane.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 19);
end;

function TJOptionPane.MessageTypeAsText: string;
begin
  case FMessageType of
    ERROR:
      Result := 'ERROR';
    INFORMATION:
      Result := 'INFORMATION';
    WARNING:
      Result := 'WARNING';
    QUESTION:
      Result := 'QUESTION';
    PLAIN:
      Result := 'PLAIN';
  end;
  Result := 'JOptionPane.' + Result + '_MESSAGE';
end;

function TJOptionPane.OptionTypeAsText: string;
begin
  case FOptionType of
    OK:
      Result := 'OK';
    INPUT:
      Result := 'INPUT';
    DEFAULT:
      Result := 'DEFAULT';
    YES_NO:
      Result := 'YES_NO';
    OK_CANCEL:
      Result := 'OK_CANCEL';
    YES_NO_CANCEL:
      Result := 'YES_NO_CANCEL';
  end;
  Result := 'JOptionPane.' + Result + '_OPTION';
end;

end.
