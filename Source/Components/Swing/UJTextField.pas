unit UJTextField;

{ Classes
  TJTextField = class (TSwingComponent)
  TJNumberField
  TJPasswordField
  TJFormattedTextField
}

interface

uses
  Classes,
  StdCtrls,
  UAComponents,
  UJComponents;

type

  TJTextField = class(TSwingComponent)
  private
    FText: string;
    FHorizontalAlignment: THorzAlignment;
    FCaretPosition: Integer;
    FEditable: Boolean;
    FSelectionEnd: Integer;
    FSelectionStart: Integer;
    procedure SetHorizontalAlignment(AValue: THorzAlignment);
    procedure SetText(const AValue: string);
    procedure SetEditable(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AEdit: TEdit);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property HorizontalAlignment: THorzAlignment read FHorizontalAlignment
      write SetHorizontalAlignment;
    property Text: string read FText write SetText;
    property CaretPosition: Integer read FCaretPosition write FCaretPosition
      default 0;
    property Editable: Boolean read FEditable write SetEditable;
    property SelectionEnd: Integer read FSelectionEnd write FSelectionEnd
      default 0;
    property SelectionStart: Integer read FSelectionStart write FSelectionStart
      default 0;
    property actionPerformed;
  end;

  TJNumberField = class(TJTextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TJPasswordField = class(TJTextField)
  private
    FEchoChar: string;
    procedure SetEchoChar(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property EchoChar: string read FEchoChar write SetEchoChar;
  end;

  TJFormattedTextField = class(TJTextField)
  private
    FColumns: Integer;
    FMask: string;
    FDateFormat: string;
    FDecimalFormat: string;
    procedure MakeFormattedTextField(Attr: string; const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure NewControl; override;
  published
    property Columns: Integer read FColumns write FColumns;
    property Mask: string read FMask write FMask;
    property DateFormat: string read FDateFormat write FDateFormat;
    property DecimalFormat: string read FDecimalFormat write FDecimalFormat;
  end;

implementation

uses
  Graphics,
  Controls,
  UObjectInspector;

{ --- TSwingTextField ---------------------------------------------------------- }

constructor TJTextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +2;
  Width := 80;
  Height := 24;
  Background := clWhite;
  FEditable := True;
  Cursor := crIBeam;
  FHorizontalAlignment := UAComponents.Left;
  Font.Style := [];
  JavaType := 'JTextField';
end;

constructor TJTextField.CreateFrom(AEdit: TEdit);
begin
  Create(AEdit.Owner);
  CreateFromJ(AEdit);
  Text := AEdit.Text;
  Font := AEdit.Font;
  Foreground := Font.Color;
  Background := AEdit.Color;
  if Background = clBtnFace then
    Background := clWhite;
  CaretPosition := AEdit.MaxLength;
end;

function TJTextField.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|Text';
  Show2 = '|CaretPosition|Editable|SelectionEnd|SelectionStart|HorizontalAlignment';
begin
  if ShowAttributes = 1 then
    Result := Show1
  else
    Result := Show1 + Show2;
  Result := Result + inherited;
end;

procedure TJTextField.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'HorizontalAlignment' then
    MakeAttribut(Attr, 'SwingConstants.' + Value)
  else
    inherited;
end;

function TJTextField.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed' + inherited;
end;

procedure TJTextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JTextField ' + Name + ' = new JTextField();');
end;

procedure TJTextField.Paint;
var
  XPos, YPos, TextWidth: Integer;
  EchoChar, Str: string;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));
  Canvas.Pen.Color := DarkShadow;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));
  if not Editable then
  begin
    Canvas.Pen.Color := DefaultBackground;
    Canvas.Brush.Color := DefaultBackground;
    Canvas.Rectangle(Rect(2, 2, Width - 2, Height - 2));
  end;
  if Background <> clWhite then
  begin
    Canvas.Pen.Color := Background;
    Canvas.Brush.Color := Background;
    Canvas.Rectangle(Rect(2, 2, Width - 2, Height - 2));
  end;
  if (Self is TJNumberField) and not IsNumeric(FText) then
  begin
    Canvas.Brush.Color := clRed;
    Canvas.Rectangle(Rect(2, 2, Width - 2, Height - 2));
  end;

  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Str := FText;
  if (Self is TJPasswordField) then
  begin
    EchoChar := (Self as TJPasswordField).EchoChar;
    if EchoChar <> '' then
      Str := StringOfChar(EchoChar[1], Length(FText));
  end;

  YPos := (Height - Canvas.TextHeight(Str)) div 2 + 1;
  TextWidth := Canvas.TextWidth(Str);
  XPos := 0;
  case FHorizontalAlignment of
    UAComponents.CENTER:
      XPos := (Width - 4 - TextWidth) div 2;
    UAComponents.Left:
      XPos := 2;
    UAComponents.Right:
      XPos := Width - 4 - TextWidth;
  end;
  Canvas.TextRect(Rect(2, 2, Width - 2, Height - 2), XPos, YPos, Str);
end;

procedure TJTextField.SetHorizontalAlignment(AValue: THorzAlignment);
begin
  if AValue <> FHorizontalAlignment then
  begin
    FHorizontalAlignment := AValue;
    Invalidate;
  end;
end;

procedure TJTextField.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

procedure TJTextField.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

{ --- TJNumberField ------------------------------------------------------------ }

constructor TJNumberField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +21;
  JavaType := 'JNumberField';
end;

procedure TJNumberField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JNumberField ' + Name + ' = new JNumberField();');
end;

{ --- TJPasswordField ---------------------------------------------------------- }

constructor TJPasswordField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +26;
  EchoChar := WideChar($2022);
  JavaType := 'JPasswordField';
end;

function TJPasswordField.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|EchoChar' + inherited;
end;

procedure TJPasswordField.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'EchoChar' then
    MakeEchoChar(Value)
  else
    inherited;
end;

procedure TJPasswordField.SetEchoChar(AValue: string);
begin
  if Length(AValue) > 1 then
  begin
    AValue := Copy(AValue, 1, 1);
    FEchoChar := AValue;
    FObjectInspector.UpdatePropertyInspector;
  end;
  if AValue <> FEchoChar then
  begin
    FEchoChar := AValue;
    Invalidate;
  end;
end;

procedure TJPasswordField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JPasswordField ' + Name +
    ' = new JPasswordField();');
end;

{ --- TJFormattedTextField ----------------------------------------------------- }

constructor TJFormattedTextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +27;
  FColumns := 0;
  JavaType := 'JFormattedTextField';
end;

function TJFormattedTextField.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Columns|Mask|DateFormat|DecimalFormat' + inherited;
end;

procedure TJFormattedTextField.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'DateFormat') or (Attr = 'DecimalFormat') or (Attr = 'Mask') then
    MakeFormattedTextField(Attr, Value)
  else
    inherited;
end;

procedure TJFormattedTextField.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'MaskFormatter',
    NewName + 'MaskFormatter', True);
  FPartner.ReplaceWord(OldName + 'SimpleDateFormat',
    NewName + 'SimpleDateFormat', True);
  FPartner.ReplaceWord(OldName + 'DecimalFormat',
    NewName + 'DecimalFormat', True);
end;

procedure TJFormattedTextField.DeleteComponent;
begin
  inherited;
  FPartner.DeleteTryCatch(Name + 'MaskFormatter.setMask');
  FPartner.DeleteAttribute('private SimpleDateFormat ' + Name +
    'SimpleDateFormat');
  FPartner.DeleteAttribute('private DecimalFormat ' + Name + 'DecimalFormat');
  FPartner.DeleteAttribute('private MaskFormatter ' + Name + 'MaskFormatter');
end;

procedure TJFormattedTextField.MakeFormattedTextField(Attr: string;
  const Value: string);
var
  Key, Str, New: string;
  Line: Integer;
begin
  FPartner.DeleteAttribute('private SimpleDateFormat ' + Name +
    'SimpleDateFormat');
  FPartner.DeleteAttribute('private DecimalFormat ' + Name + 'DecimalFormat');
  FPartner.DeleteAttribute('private MaskFormatter ' + Name + 'MaskFormatter');
  FPartner.DeleteTryCatch(Name + 'MaskFormatter.setMask');

  if Attr = 'DateFormat' then
    Attr := 'SimpleDateFormat'
  else if Attr = 'Mask' then
    Attr := 'MaskFormatter';
  if Attr = 'MaskFormatter' then
    New := ' = new ' + Attr + '();'
  else
    New := ' = new ' + Attr + '("' + Value + '");';

  Key := 'private JFormattedTextField ' + Name;
  Str := Indent1 + '  private ' + Attr + ' ' + Name + Attr + New;
  FPartner.ReplaceLine(Key, Str);
  Key := Str;
  Str := Indent1 + 'private JFormattedTextField ' + Name +
    ' = new JFormattedTextField(' + Name + Attr + ');';
  FPartner.InsertAttributAfter(Key, Str);

  if Attr = 'MaskFormatter' then
  begin
    Str := Indent2 + 'try {' + CrLf + Indent2 + Indent1 + Name + Attr +
      '.setMask("' + Value + '");' + CrLf + Indent2 + '} catch (Throwable t) {'
      + CrLf + Indent2 + Indent1 + 't.printStackTrace();' + CrLf + Indent2 +
      '}' + CrLf;
    Line := FPartner.GetLineNumberWith(GetContainerAdd);
    FPartner.InsertLinesAt(Line + 1, Str);
    FPartner.InsertImport('javax.swing.text.*');
  end
  else
    InsertImport('java.text.*');
end;

procedure TJFormattedTextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JFormattedTextField ' + Name +
    ' = new JFormattedTextField();');
end;

end.
