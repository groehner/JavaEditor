unit UJTextField;

{ Classes
  TJTextField = class (TSwingComponent)
    TJNumberField
    TJPasswordField
    TJFormattedTextField
}

interface

uses
  Classes, StdCtrls, UAComponents, UJComponents;

type

  TJTextField = class (TSwingComponent)
  private
    FText: string;
    FHorizontalAlignment: THorzAlignment;
    FCaretPosition: integer;
    FEditable: boolean;
    FSelectionEnd: integer;
    FSelectionStart: integer;
    procedure setHorizontalAlignment(aValue: THorzAlignment);
    procedure setText(const aValue: string);
    procedure setEditable(aValue: boolean);
    function isNumeric(const s: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aEdit: TEdit);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property HorizontalAlignment: THorzAlignment read FHorizontalAlignment write setHorizontalAlignment;
    property Text: string read FText write setText;
    property CaretPosition: integer read FCaretPosition write FCaretPosition default 0;
    property Editable: boolean read FEditable write setEditable;
    property SelectionEnd: integer read FSelectionEnd write FSelectionEnd default 0;
    property SelectionStart: integer read FSelectionStart write FSelectionStart default 0;
    property actionPerformed;
  end;

  TJNumberField = class(TJTextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TJPasswordField = class (TJTextField)
  private
    FEchoChar: string;
    procedure setEchoChar(aValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property EchoChar: string read FEchoChar write setEchoChar;
  end;

  TJFormattedTextField = class (TJTextField)
  private
    FColumns: integer;
    FMask: string;
    FDateFormat: string;
    FDecimalFormat: string;
    procedure MakeFormattedTextField(Attr: string; const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure NewControl; override;
  published
    property Columns: integer read FColumns write FColumns;
    property Mask: string read FMask write FMask;
    property DateFormat: string read FDateFormat write FDateFormat;
    property DecimalFormat: string read FDecimalFormat write FDecimalFormat;
  end;

implementation

uses SysUtils, Graphics, Controls, UObjectInspector;

{--- TSwingTextField ----------------------------------------------------------}

constructor TJTextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +2;
  Width:= 80;
  Height:= 24;
  Background:= clWhite;
  FEditable:= true;
  Cursor:= crIBeam;
  FHorizontalAlignment:= UAComponents.LEFT;
  Font.Style:= [];
  JavaType:= 'JTextField';
end;

constructor TJTextField.CreateFrom(aEdit: TEdit);
begin
  Create(aEdit.Owner);
  CreateFromJ(aEdit);
  Text:= aEdit.Text;
  Font:= aEdit.Font;
  Foreground:= Font.Color;
  Background:= aEdit.Color;
  if Background = clBtnFace then Background:= clWhite;
  CaretPosition:= aEdit.MaxLength;
end;

function TJTextField.getAttributes(ShowAttributes: integer): string;
  const
    show1 = '|Text';
    show2 = '|CaretPosition|Editable|SelectionEnd|SelectionStart|HorizontalAlignment';
begin
  if ShowAttributes = 1
    then Result:= show1
    else Result:= show1 + show2;
  Result:= Result + inherited;
end;

procedure TJTextField.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'HorizontalAlignment' then
    MakeAttribut(Attr, 'SwingConstants.' + Value)
  else
    inherited;
end;

function TJTextField.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed' + inherited;
end;

procedure TJTextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JTextField ' + Name + ' = new JTextField();');
end;

procedure TJTextField.Paint;
  var x, y, tw: integer; EchoChar, s: string;
begin
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));
  if Editable = false then begin
    Canvas.Pen.Color:= DefaultBackground;
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.Rectangle(Rect(2, 2, Width-2, Height-2));
  end;
  if Background <> clWhite then begin
    Canvas.Pen.Color:= Background;
    Canvas.Brush.Color:= Background;
    Canvas.Rectangle(Rect(2, 2, Width-2, Height-2));
  end;
  if (Self is TJNumberField) and not isNumeric(FText) then begin
    Canvas.Brush.Color:= clRed;
    Canvas.Rectangle(Rect(2, 2, Width-2, Height-2));
  end;

  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  s:= FText;
  if (self is TJPasswordField) then begin
    EchoChar:= (Self as TJPasswordField).EchoChar;
    if EchoChar <> ''
      then s:= StringOfChar(EchoChar[1], Length(FText));
  end;

  y:= (Height - Canvas.TextHeight(s)) div 2 + 1;
  tw:= Canvas.TextWidth(s);
  x:= 0;
  case FHorizontalAlignment of
    UAComponents.CENTER: x:= (Width-4 - tw) div 2;
    UAComponents.LEFT  : x:= 2;
    UAComponents.RIGHT : x:= Width-4 - tw;
  end;
  Canvas.TextRect(Rect(2, 2, Width-2, Height-2), x, y, s);
end;

procedure TJTextField.setHorizontalAlignment(aValue: THorzAlignment);
begin
  if aValue <> FHorizontalAlignment then begin
    FHorizontalAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TJTextField.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

procedure TJTextField.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

{$HINTS OFF}
function TJTextField.isNumeric(const s: string): boolean;
  var v: double; c: integer;
begin
  val(s, v, c);
  Result:= (c = 0);
  if not Result and ((s = '-') or (s = '.') or (s = ''))
    then Result:= true;
end;
{$HINTS ON}

{--- TJNumberField ------------------------------------------------------------}

constructor TJNumberField.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +21;
  JavaType:= 'JNumberField';
end;

procedure TJNumberField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JNumberField ' + Name + ' = new JNumberField();');
end;

{--- TJPasswordField ----------------------------------------------------------}

constructor TJPasswordField.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +26;
  EchoChar:= WideChar($2022);
  JavaType:= 'JPasswordField';
end;

function TJPasswordField.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|EchoChar' + inherited;
end;

procedure TJPasswordField.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'EchoChar' then
    MakeEchoChar(Value)
  else
    inherited;
end;

procedure TJPasswordField.setEchoChar(aValue: string);
begin
  if Length(aValue) > 1 then begin
    aValue:= copy(aValue, 1, 1);
    FEchoChar:= aValue;
    FObjectInspector.UpdatePropertyInspector;
  end;
  if aValue <> FEchoChar then begin
    FEchoChar:= aValue;
    Invalidate;
  end;
end;

procedure TJPasswordField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JPasswordField ' + Name + ' = new JPasswordField();');
end;

{--- TJFormattedTextField -----------------------------------------------------}

constructor TJFormattedTextField.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +27;
  FColumns:= 0;
  JavaType:= 'JFormattedTextField';
end;

function TJFormattedTextField.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Columns|Mask|DateFormat|DecimalFormat' + inherited;
end;

procedure TJFormattedTextField.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'DateFormat') or (Attr = 'DecimalFormat') or (Attr = 'Mask') then
    MakeFormattedTextField(Attr, Value)
  else
    inherited;
end;

procedure TJFormattedTextField.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'MaskFormatter' , NewName + 'MaskFormatter', true);
  Partner.ReplaceWord(OldName + 'SimpleDateFormat' , NewName + 'SimpleDateFormat', true);
  Partner.ReplaceWord(OldName + 'DecimalFormat' , NewName + 'DecimalFormat', true);
end;

procedure TJFormattedTextField.DeleteComponent;
begin
  inherited;
  Partner.DeleteTryCatch(Name + 'MaskFormatter.setMask');
  Partner.DeleteAttribute('private SimpleDateFormat ' + Name + 'SimpleDateFormat');
  Partner.DeleteAttribute('private DecimalFormat ' + Name + 'DecimalFormat');
  Partner.DeleteAttribute('private MaskFormatter ' + Name + 'MaskFormatter');
end;

procedure TJFormattedTextField.MakeFormattedTextField(Attr: string; const Value: string);
  var key, s, ne: string; line: integer;
begin
  Partner.DeleteAttribute('private SimpleDateFormat ' + Name + 'SimpleDateFormat');
  Partner.DeleteAttribute('private DecimalFormat ' + Name + 'DecimalFormat');
  Partner.DeleteAttribute('private MaskFormatter ' + Name + 'MaskFormatter');
  Partner.DeleteTryCatch(Name + 'MaskFormatter.setMask');

  if Attr = 'DateFormat' then Attr:= 'SimpleDateFormat' else
  if Attr = 'Mask'       then Attr:= 'MaskFormatter';
  if Attr = 'MaskFormatter'
    then ne:= ' = new ' + Attr + '();'
    else ne:= ' = new ' + Attr + '("' + Value + '");';

  key:= 'private JFormattedTextField ' + Name;
  s  := Indent1 + '  private ' + Attr + ' ' + Name + Attr + ne;
  Partner.ReplaceLine(key, s);
  key:= s;
  s  := Indent1 + 'private JFormattedTextField ' + Name +
        ' = new JFormattedTextField(' + Name + Attr + ');';
  Partner.InsertAttributAfter(key, s);

  if Attr = 'MaskFormatter' then begin
    s:= Indent2 + 'try {' + CrLf +
        Indent2 + Indent1 + Name + Attr + '.setMask("' + Value + '");' + CrLf +
        Indent2 + '} catch (Throwable t) {' + CrLf +
        Indent2 + Indent1 + 't.printStackTrace();' + CrLf +
        Indent2 + '}' + CrLf;
    line:= Partner.getLineNumberWith(GetContainerAdd);
    Partner.InsertLinesAt(line+1, s);
    Partner.InsertImport('javax.swing.text.*');
  end else
    InsertImport('java.text.*');
end;

procedure TJFormattedTextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JFormattedTextField ' + Name + ' = new JFormattedTextField();');
end;

end.
