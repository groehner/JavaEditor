unit UJChooser;

{ Classes
    TJFileChooser = class (TSwingComponent)
      TJFileSaveChooser
    TJColorChooser = class (TSwingComponent)
    TJOptionPane = class (TSwingComponent)
}

interface

uses
  Classes, Graphics, UJComponents;

type

  TOptionType = (OK, INPUT, DEFAULT, YES_NO, OK_CANCEL, YES_NO_CANCEL);
  TMessageType = (ERROR, INFORMATION, WARNING, QUESTION, PLAIN);
  TFileSelectionMode = (FILES_ONLY, DIRECTORIES_ONLY, FILES_AND_DIRECTORIES);

  TJFileChooser = class (TSwingComponent)
  private
    FAcceptAllFileFilterUsed: boolean;
    FApproveButtonMnemonic: integer;
    FControlButtonsAreShown: boolean;
    FApproveButtonText: string;
    FApproveButtonToolTipText: string;
    FDialogTitle: string;
    FDragEnabled: boolean;
    FFileHidingEnabled: boolean;
    FFileSelectionMode: TFileSelectionMode;
    FMultiSelectionEnabled: boolean;
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property AcceptAllFileFilterUsed: boolean read FAcceptAllFileFilterUsed write FAcceptAllFileFilterUsed;
    property ApproveButtonMnemonic: integer read FApproveButtonMnemonic write FApproveButtonMnemonic;
    property ApproveButtonText: string read FApproveButtonText write FApproveButtonText;
    property ApproveButtonToolTipText: string read FApproveButtonToolTipText write FApproveButtonToolTipText;
    property ControlButtonsAreShown: boolean read FControlButtonsAreShown write FControlButtonsAreShown;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DragEnabled: boolean read FDragEnabled write FDragEnabled;
    property FileHidingEnabled: boolean read FFileHidingEnabled write FFileHidingEnabled;
    property FileSelectionMode: TFileSelectionMode read FFileSelectionMode write FFileSelectionMode;
    property MultiSelectionEnabled: boolean read FMultiSelectionEnabled write FMultiSelectionEnabled;
  end;

  TJFileSaveChooser = class(TJFileChooser)
  public
    constructor Create (AOwner: TComponent); override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  end;

  TJColorChooser = class (TSwingComponent)
  private
    FColor: TColor;
    FDialogTitle: string;
    FDragEnabled: boolean;
    procedure MakeColorChooser;
    function getColorChooser: string;
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property Color: TColor read FColor write FColor;
    property DialogTitle: string read FDialogTitle write FDialogTitle;
    property DragEnabled: boolean read FDragEnabled write FDragEnabled;
  end;

  TJOptionPane = class (TSwingComponent)
  private
    FComponent: string;
    FDialogTitle: string;
    FMessage: string;
    FMessageType: TMessageType;
    FOptionType: TOptionType;
    procedure MakeOptionPane;
    function getOptionPane: string;
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
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

uses Controls, UJava, ULink;

{--- FileChooser --------------------------------------------------------------}

constructor TJFileChooser.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 45;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'JFileChooser';
end;

function TJFileChooser.getAttributes(ShowAttributes: integer): string;
  const
    show1 = '|DialogTitle|DragEnabled|FileSelectionMode|MultiSelectionEnabled';
    show2 = '|AcceptAllFileFilterUsed|ApproveButtonMnemonic|ApproveButtonText' +
            '|ApproveButtonToolTipText|ControlButtonsAreShown|FileHidingEnabled';
begin
  if ShowAttributes = 1
    then Result:= show1
    else Result:= show1 + show2;
  Result:= Result + inherited;
end;

procedure TJFileChooser.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'FileSelectionMode' then
    MakeAttribut(Attr, 'JFileChooser.' + Value)
  else
    inherited;
end;

procedure TJFileChooser.NewControl;
begin
  Partner.InsertImport('java.io.*');
  InsertNewVariable('private JFileChooser ' + Name + ' = new JFileChooser();');
  var s:= surroundFix('public File ' + Name + '_openFile() {') +
      surroundFix2('if (' + Name + '.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {') +
      surroundFix2(Indent1 + 'return ' + Name + '.getSelectedFile();') +
      surroundFix2('} else {') +
      surroundFix2(Indent1 + 'return null;') +
      surroundFix2('}') +
      surroundFix('}') + #13#10;
  Partner.InsertProcedure(s);
end;

procedure TJFileChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_openFile' , NewName + '_openFile', true);
end;

procedure TJFileChooser.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_openFile', false);
end;

procedure TJFileChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 16);
end;

{--- FileSaveChooser ----------------------------------------------------------}

constructor TJFileSaveChooser.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 46;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'JFileChooser';
end;

procedure TJFileSaveChooser.NewControl;
begin
  Partner.InsertImport('java.io.*');
  InsertNewVariable('private JFileChooser ' + Name + ' = new JFileChooser();');
  var s:= surroundFix('public File ' + Name + '_saveFile() {') +
      surroundFix2('if (' + Name + '.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {') +
      surroundFix2(Indent1 + 'return ' + Name + '.getSelectedFile();') +
      surroundFix2('} else {') +
      surroundFix2(Indent1 + 'return null;') +
      surroundFix2('}') +
      surroundFix('}') + #13#10;
  Partner.InsertProcedure(s);
end;

procedure TJFileSaveChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_saveFile' , NewName + '_saveFile', true);
end;

procedure TJFileSaveChooser.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_saveFile', false);
end;

procedure TJFileSaveChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 17);
end;

{--- ColorChooser -------------------------------------------------------------}

constructor TJColorChooser.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 47;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'JColorChooser';
end;

function TJColorChooser.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Color|DialogTitle|DragEnabled' + inherited;
end;

procedure TJColorChooser.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Color') or (Attr = 'DialogTitle') then
    MakeColorChooser
  else
    inherited;
end;

procedure TJColorChooser.MakeColorChooser;
begin
  Partner.DeleteMethod(Name+ '_getColor', false);
  Partner.InsertProcedure(getColorChooser);
end;

function TJColorChooser.getColorChooser;
  var col: string;
begin
  col:= ToJavaColor(Color);
  if col = 'Color.(NONE)' then col:= 'Color.WHITE';
  Result:=
    surroundFix('public Color ' + Name + '_getColor() {') +
    surroundFix2('return ' + Name + '.showDialog(this, "' + DialogTitle +
                   '", ' + col + ');') +
    surroundFix('}') + #13#10;
end;

procedure TJColorChooser.NewControl;
begin
  Partner.InsertImport('java.io.*');
  InsertNewVariable('private JColorChooser ' + Name + ' = new JColorChooser();');
  Partner.InsertProcedure(getColorChooser);
end;

procedure TJColorChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_getColor' , NewName + '_getColor', true);
end;

procedure TJColorChooser.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_getColor', false);
end;

procedure TJColorChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 18);
end;

{--- OptionPane ---------------------------------------------------------------}

constructor TJOptionPane.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 48;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  FComponent:= 'this';
  JavaType:= 'JOptionPane';
end;

function TJOptionPane.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Component|DialogTitle|Message|MessageType|OptionType' + inherited;
end;

procedure TJOptionPane.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Component') or (Attr = 'DialogTitle') or (Attr = 'Message') or
     (Attr = 'MessageType') or (Attr = 'OptionType') then
    MakeOptionPane
  else
    inherited;
end;

procedure TJOptionPane.MakeOptionPane;
begin
  Partner.DeleteMethod(Name+ '_ShowDialog', false);
  Partner.InsertProcedure(getOptionPane);
end;

function TJOptionPane.getOptionPane: string;
  var s: string;
begin
  if OptionType = OK then
    s:= surroundFix('public void ' + Name + '_ShowDialog() {') +
        surroundFix2('JOptionPane.showMessageDialog(' + Component + ', "' +
                      Message + '", "' + DialogTitle + '", ' + MessageTypeAsText + ');')
  else if OptionType = INPUT then
    s:= surroundFix('public String ' + Name + '_ShowDialog() {') +
        surroundFix2('return JOptionPane.showInputDialog('+ Component + ', "' +
                      Message + '", "' + DialogTitle + '", ' + MessageTypeAsText + ');')
  else
    s:= surroundFix('public int ' + Name + '_ShowDialog() {') +
        surroundFix2('return JOptionPane.showConfirmDialog('+ Component  + ', "' +
                     Message + '", "' + DialogTitle + '", ' + OptionTypeAsText +
                     ', ' + MessageTypeAsText + ');');
  Result:= s + surroundFix('}') + #13#10;
end;

procedure TJOptionPane.NewControl;
begin
  Partner.InsertImport('java.io.*');
  InsertNewVariable('private JOptionPane ' + Name + ' = new JOptionPane();');
  Partner.InsertProcedure(getOptionPane);
end;

procedure TJOptionPane.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_ShowDialog' , NewName + '_ShowDialog', true);
end;

procedure TJOptionPane.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_ShowDialog', false);
end;

procedure TJOptionPane.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing2.Draw(Canvas, 5, 2, 19);
end;

function TJOptionPane.MessageTypeAsText: string;
begin
  case FMessageType of
    ERROR:       Result:= 'ERROR';
    INFORMATION: Result:= 'INFORMATION';
    WARNING:     Result:= 'WARNING';
    QUESTION:    Result:= 'QUESTION';
    PLAIN:       Result:= 'PLAIN';
  end;
  Result:= 'JOptionPane.' + Result + '_MESSAGE';
end;

function TJOptionPane.OptionTypeAsText: string;
begin
  case FOptionType of
    OK:            Result:= 'OK';
    INPUT:         Result:= 'INPUT';
    DEFAULT:       Result:= 'DEFAULT';
    YES_NO:        Result:= 'YES_NO';
    OK_CANCEL:     Result:= 'OK_CANCEL';
    YES_NO_CANCEL: Result:= 'YES_NO_CANCEL';
  end;
  Result:= 'JOptionPane.' + Result + '_OPTION';
end;

end.
