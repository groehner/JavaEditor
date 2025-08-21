unit UATextField;

{ Classes
  TATextField = class (TAWTComponent)
  TANumberField
}

interface

uses
  Classes,
  StdCtrls,
  UAComponents;

type

  TATextField = class(TAWTComponent)
  private
    FText: string;
    FEchoChar: string;
    FCaretPosition: Integer;
    FEditable: Boolean;
    FSelectionEnd: Integer;
    FSelectionStart: Integer;
    procedure SetEchoChar(AValue: string);
    procedure SetText(const AValue: string);
    procedure SetEditable(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property EchoChar: string read FEchoChar write SetEchoChar;
    property Text: string read FText write SetText;
    property CaretPosition: Integer read FCaretPosition write FCaretPosition
      default 0;
    property Editable: Boolean read FEditable write SetEditable;
    property SelectionEnd: Integer read FSelectionEnd write FSelectionEnd
      default 0;
    property SelectionStart: Integer read FSelectionStart write FSelectionStart
      default 0;

    property actionPerformed;
    property textValueChanged;
  end;

  TANumberField = class(TATextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

implementation

uses
  Graphics,
  Controls,
  UObjectInspector;

{ --- TATextField -------------------------------------------------------------- }

constructor TATextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -2;
  Width := 80;
  Height := 24;
  Editable := True;
  Cursor := crIBeam;
  JavaType := 'TextField';
end;

function TATextField.GetAttributes(ShowAttributes: Integer): string;
begin
  if ShowAttributes = 1 then
    Result := '|Text'
  else
    Result := '|Text|CaretPosition|EchoChar|Editable|SelectionEnd|SelectionStart';
  Result := Result + inherited GetAttributes(ShowAttributes);
end;

procedure TATextField.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'EchoChar' then
    MakeEchoChar(Value)
  else
    inherited;
end;

function TATextField.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed|textValueChanged' + inherited;
end;

procedure TATextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private TextField ' + Name + ' = new TextField();');
end;

procedure TATextField.Paint;
begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Pen.Color := AWTGray;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
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
  if (Self is TANumberField) and not IsNumeric(FText) then
  begin
    Canvas.Brush.Color := clRed;
    Canvas.Rectangle(Rect(2, 2, Width - 2, Height - 2));
  end;
  var
  Str := FText;
  if EchoChar <> '' then
    Str := StringOfChar(EchoChar[1], Length(Str));
  Canvas.TextRect(Rect(2, 2, Width - 2, Height - 2), 4, 3, Str);
end;

procedure TATextField.SetEchoChar(AValue: string);
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

procedure TATextField.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

procedure TATextField.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

{ --- TNumberField ------------------------------------------------------------- }

constructor TANumberField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -21;
  JavaType := 'NumberField';
end;

procedure TANumberField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private NumberField ' + Name + ' = new NumberField();');
end;

end.
