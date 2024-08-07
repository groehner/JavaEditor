unit UATextField;

{ Classes
  TATextField = class (TAWTComponent)
    TANumberField
}

interface

uses
  Classes, StdCtrls, UAComponents;

type

  TATextField = class (TAWTComponent)
  private
    FText: string;
    FEchoChar: string;
    FCaretPosition: integer;
    FEditable: boolean;
    FSelectionEnd: integer;
    FSelectionStart: integer;
    procedure setEchoChar(aValue: string);
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
    property EchoChar: string read FEchoChar write setEchoChar;
    property Text: string read FText write setText;
    property CaretPosition: integer read FCaretPosition write FCaretPosition default 0;
    property Editable: boolean read FEditable write setEditable;
    property SelectionEnd: integer read FSelectionEnd write FSelectionEnd default 0;
    property SelectionStart: integer read FSelectionStart write FSelectionStart default 0;

    property actionPerformed;
    property textValueChanged;
  end;

  TANumberField = class(TATextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

implementation

uses SysUtils, Graphics, Controls, UObjectInspector;

{--- TATextField --------------------------------------------------------------}

constructor TATextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= -2;
  Width:= 80;
  Height:= 24;
  Editable:= true;
  Cursor:= crIBeam;
  JavaType:= 'TextField';
end;

constructor TATextField.CreateFrom(aEdit: TEdit);
begin
  Create(aEdit.Owner);
  CreateFromA(aEdit);
  Text:= aEdit.Text;
  Font:= aEdit.Font;
  Foreground:= Font.Color;
  Background:= aEdit.Color;
  if Background = clBtnFace then Background:= clWhite;
  CaretPosition:= aEdit.MaxLength;
end;

function TATextField.getAttributes(ShowAttributes: integer): string;
begin
  if ShowAttributes = 1
    then Result:= '|Text'
    else Result:= '|Text|CaretPosition|EchoChar|Editable|SelectionEnd|SelectionStart';
  Result:= Result + inherited getAttributes(ShowAttributes)
end;

procedure TATextField.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'EchoChar' then
    MakeEchoChar(Value)
  else
    inherited;
end;

function TATextField.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed|textValueChanged' + inherited;
end;

procedure TATextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private TextField ' + Name + ' = new TextField();');
end;

procedure TATextField.Paint;
begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Pen.Color:= AWTGray;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
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
  if (Self is TANumberField) and not isNumeric(FText) then begin
    Canvas.Brush.Color:= clRed;
    Canvas.Rectangle(Rect(2, 2, Width-2, Height-2));
  end;
  var s:= FText;
  if EchoChar <> '' then
    s:= StringOfChar(EchoChar[1], Length(s));
  Canvas.TextRect(Rect(2, 2, Width-2, Height-2), 4, 3, s);
end;

procedure TATextField.setEchoChar(aValue: string);
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

procedure TATextField.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

procedure TATextField.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

{$HINTS OFF}  {$WARNINGS OFF}
function TATextField.isNumeric(const s: string): boolean;
  var v: double; c: integer;
begin
  val(s, v, c);
  Result:= (c = 0);
  if not Result and ((s = '-') or (s = '.') or (s = ''))
    then Result:= true;
end;
{$HINTS ON}  {$WARNINGS ON}

{--- TNumberField -------------------------------------------------------------}

constructor TANumberField.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= -21;
  JavaType:= 'NumberField';
end;

procedure TANumberField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private NumberField ' + Name + ' = new NumberField();');
end;

end.
