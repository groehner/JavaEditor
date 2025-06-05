unit UFXSpinner;

interface

uses
  Classes, UFXComponents;

type

  TFXSpinner = class(TFXControl)
  private
    FMaximum: Integer;
    FMinimum: Integer;
    FInitialValue: Integer;
    FAmountToStepBy: Integer;
    FEditable: Boolean;
    procedure SetMaximum(AValue: Integer);
    procedure SetMinimum(AValue: Integer);
    procedure SetInitialValue(AValue: Integer);
    procedure SetAmountToStepBy(AValue: Integer);
    function GetSpinnerCode: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure DeleteComponent; override;
  published
    property Maximum: Integer read FMaximum write SetMaximum;
    property Minimum: Integer read FMinimum write SetMinimum;
    property InitialValue: Integer read FInitialValue write SetInitialValue;
    property AmountToStepBy: Integer read FAmountToStepBy
      write SetAmountToStepBy;
    property Editable: Boolean read FEditable write FEditable;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Types;

constructor TFXSpinner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 111;
  PrefWidth := 80;
  PrefHeight := 24;
  Background := clWhite;
  FMaximum := 10;
  FMinimum := 0;
  FInitialValue := 0;
  FAmountToStepBy := 1;
  FEditable := True;
  JavaType := 'Spinner';
end;

procedure TFXSpinner.Paint;
var
  Str: string;
  TextHeight, XPos, YPos: Integer;
  Points: array [0 .. 2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Font.Style := [];
  Canvas.Font.Color := Foreground;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);

  // paint number/value
  Str := IntToStr(FInitialValue);
  TextHeight := Canvas.TextHeight(Str);
  YPos := (Height - TextHeight) div 2;
  Canvas.TextOut(8, YPos, Str);

  // paint up/down
  Canvas.MoveTo(Width - 24, 0);
  Canvas.LineTo(Width - 24, Height);
  Canvas.MoveTo(Width - 24, Height div 2);
  Canvas.LineTo(Width, Height div 2);

  Canvas.Brush.Color := DarkShadow;
  XPos := Width - 20;
  YPos := Height div 4;
  Points[0] := Point(XPos + 4, YPos + 2);
  Points[1] := Point(XPos + 12, YPos + 2);
  Points[2] := Point(XPos + 8, YPos - 2);
  Canvas.Polygon(Points);
  YPos := (Height * 3) div 4;
  Points[0] := Point(XPos + 4, YPos - 2);
  Points[1] := Point(XPos + 12, YPos - 2);
  Points[2] := Point(XPos + 8, YPos + 2);
  Canvas.Polygon(Points);
end;

procedure TFXSpinner.SetMaximum(AValue: Integer);
begin
  if (AValue <> FMaximum) and (AValue > FMinimum) then
  begin
    FMaximum := AValue;
    Invalidate;
  end;
end;

procedure TFXSpinner.SetMinimum(AValue: Integer);
begin
  if (AValue <> FMinimum) and (AValue < FMaximum) then
  begin
    FMinimum := AValue;
    Invalidate;
  end;
end;

procedure TFXSpinner.SetInitialValue(AValue: Integer);
begin
  if AValue <> FInitialValue then
  begin
    FInitialValue := AValue;
    Invalidate;
  end;
end;

procedure TFXSpinner.SetAmountToStepBy(AValue: Integer);
begin
  if AValue <> FAmountToStepBy then
  begin
    FAmountToStepBy := AValue;
    Invalidate;
  end;
end;

function TFXSpinner.GetSpinnerCode: string;
begin
  Result := 'private Spinner<Integer> ' + Name + ' = new Spinner<>(' +
    IntToStr(FMinimum) + ', ' + IntToStr(FMaximum) + ', ' +
    IntToStr(FInitialValue) + ', ' + IntToStr(FAmountToStepBy) + ');';
end;

procedure TFXSpinner.NewControl;
begin
  DefaultComponent;
  InsertNewVariable(GetSpinnerCode);
end;

procedure TFXSpinner.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Minimum') or (Attr = 'Maximum') or (Attr = 'InitialValue') or
    (Attr = 'AmountToStepBy') then
    FPartner.ReplaceLine('private Spinner<Integer> ' + Name,
      Indent1 + GetSpinnerCode)
  else
    inherited;
end;

function TFXSpinner.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Minimum|Maximum|InitialValue';
  Attributes2 = Attributes1 + '|AmountToStepBy|Editable';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
  begin
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
    Delete(Result, Pos('|Background', Result), 11);
  end;
end;

function TFXSpinner.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|mouseClicked' + inherited;
end;

procedure TFXSpinner.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private Spinner<Integer> ' + Name);
end;

end.
