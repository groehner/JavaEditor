unit UFXSpinner;

interface

uses
  Classes, UFXComponents;

type

  TFXSpinner = class (TFXControl)
  private
    FMaximum: Integer;
    FMinimum: Integer;
    FInitialValue: Integer;
    FAmountToStepBy: Integer;
    FEditable: boolean;
    procedure setMaximum(aValue: integer);
    procedure setMinimum(aValue: integer);
    procedure setInitialValue(aValue: integer);
    procedure setAmountToStepBy(aValue: integer);
    function getSpinnerCode: string;
  public
    constructor Create (AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure DeleteComponent; override;
  published
    property Maximum: integer read FMaximum write setMaximum;
    property Minimum: integer read FMinimum write setMinimum;
    property InitialValue: integer read FInitialValue write setInitialValue;
    property AmountToStepBy: integer read FAmountToStepBy write setAmountToStepBy;
    property Editable: boolean read FEditable write FEditable;
  end;

implementation

uses SysUtils, Graphics, Types;

constructor TFXSpinner.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 111;
  PrefWidth:= 80;
  PrefHeight:= 24;
  Background:= clWhite;
  FMaximum:= 10;
  FMinimum:= 0;
  FInitialValue:= 0;
  FAmountToStepBy:= 1;
  FEditable:= true;
  JavaType:= 'Spinner';
end;

procedure TFXSpinner.Paint;
  var s: string;
      th, x, y: integer;
      Points: array[0..2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Font.Style:= [];
  Canvas.Font.Color:= Foreground;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);

  // paint number/value
  s:= IntToStr(FInitialValue);
  th:= Canvas.TextHeight(s);
  y:= (Height - th) div 2;
  Canvas.TextOut(8, y, s);

  // paint up/down
  Canvas.MoveTo(Width - 24, 0);
  Canvas.LineTo(Width - 24, Height);
  Canvas.MoveTo(Width - 24, Height div 2);
  Canvas.LineTo(Width, Height div 2);

  Canvas.Brush.Color:= DarkShadow;
  x:= Width - 20;
  y:= Height div 4;
  Points[0]:= Point(x +  4, y + 2);
  Points[1]:= Point(x + 12, y + 2);
  Points[2]:= Point(x +  8, y - 2);
  Canvas.Polygon(Points);
  y:= (Height*3) div 4;
  Points[0]:= Point(x +  4, y - 2);
  Points[1]:= Point(x + 12, y - 2);
  Points[2]:= Point(x +  8, y + 2);
  Canvas.Polygon(Points);
end;

procedure TFXSpinner.SetMaximum(aValue: integer);
begin
  if (aValue <> FMaximum) and (aValue > FMinimum) then begin
    FMaximum:= aValue;
    Invalidate;
  end;
end;

procedure TFXSpinner.SetMinimum(aValue: integer);
begin
  if (aValue <> FMinimum) and (aValue < FMaximum) then begin
    FMinimum:= aValue;
    Invalidate;
  end;
end;

procedure TFXSpinner.setInitialValue(aValue: integer);
begin
  if aValue <> FInitialValue then begin
    FInitialValue:= aValue;
    Invalidate;
  end;
end;

procedure TFXSpinner.setAmountToStepBy(aValue: integer);
begin
  if aValue <> FAmountToStepBy then begin
    FAmountToStepBy:= aValue;
    Invalidate;
  end;
end;

function TFXSpinner.getSpinnerCode: string;
begin
  Result:= 'private Spinner<Integer> '  + Name + ' = new Spinner<>(' +
             IntToStr(FMinimum) + ', ' + IntToStr(FMaximum) + ', ' +
             IntToStr(FInitialValue) + ', ' + IntToStr(FAmountToStepBy) + ');';
end;

procedure TFXSpinner.NewControl;
begin
  DefaultComponent;
  InsertNewVariable(getSpinnerCode);
end;

procedure TFXSpinner.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Minimum') or (Attr = 'Maximum') or (Attr = 'InitialValue') or (Attr = 'AmountToStepBy') then
    Partner.ReplaceLine('private Spinner<Integer> '  + Name, Indent1 + getSpinnercode)
  else
    inherited;
end;

function TFXSpinner.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Minimum|Maximum|InitialValue';
        Attributes2 = Attributes1 + '|AmountToStepBy|Editable';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
  else begin
    Result:= Attributes2 + inherited getAttributes(ShowAttributes);
    delete(Result, Pos('|Background', Result), 11);
  end;
end;

function TFXSpinner.getEvents(ShowEvents: integer): string;
begin
  Result:= '|mouseClicked' + inherited;
end;

procedure TFXSpinner.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private Spinner<Integer> ' + Name);
end;


end.
