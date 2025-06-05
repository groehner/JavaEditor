unit UJSpinner;

interface

uses
  Classes,
  Graphics,
  Spin,
  UJComponents;

type

  TJSpinner = class(TSwingComponent)
  private
    FNeutral: TColor;
    FMaximum: Integer;
    FMinimum: Integer;
    FStepSize: Integer;
    FValue: string;
    FList: TStrings;
    procedure SetMaximum(AValue: Integer);
    procedure SetMinimum(AValue: Integer);
    procedure SetStepSize(AValue: Integer);
    procedure SetValue(const AValue: string);
    procedure SetStrings(TheStrings: TStrings);
    procedure MakeSpinnerColor(const Attr, Value: string);
    procedure MakeSpinner(const Attr: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(ASpinEdit: TSpinEdit);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure DeleteComponent; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Maximum: Integer read FMaximum write SetMaximum;
    property Minimum: Integer read FMinimum write SetMinimum;
    property StepSize: Integer read FStepSize write SetStepSize;
    property Value: string read FValue write SetValue;
    property List: TStrings read FList write SetStrings;
  end;

implementation

uses
  Controls,
  SysUtils,
  Types,
  UUtils;

constructor TJSpinner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 22;
  Width := 80;
  Height := 24;
  JavaType := 'JSpinner';
  Background := clWhite;
  FNeutral := DefaultBackground;
  FMaximum := 10;
  FMinimum := 0;
  FStepSize := 1;
  FValue := '0';
  FList := TStringList.Create;
end;

constructor TJSpinner.CreateFrom(ASpinEdit: TSpinEdit);
begin
  Create(ASpinEdit.Owner);
  CreateFromJ(ASpinEdit);
  Background := ASpinEdit.Color;
  Font.Assign(ASpinEdit.Font);
end;

function TJSpinner.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Maximum|Minimum|StepSize|Value|List' + inherited;
end;

procedure TJSpinner.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Value') or (Attr = 'Minimum') or (Attr = 'Maximum') or
    (Attr = 'StepSize') or (Attr = 'List') then
    MakeSpinner(Attr)
  else if (Attr = 'Background') or (Attr = 'Foreground') then
    MakeSpinnerColor(Attr, Value)
  else
    inherited;
end;

procedure TJSpinner.MakeSpinner(const Attr: string);
var
  Str, Key: string;
begin
  Key := Name + 'Model = new Spinner';
  if (Attr = 'List') and (Trim(FList.Text) <> '') then
  begin
    Str := 'private SpinnerListModel ' + Key + 'ListModel(new String[] {';
    for var I := 0 to List.Count - 1 do
      Str := Str + '"' + List[I] + '", ';
    Str := UUtils.Left(Str, Length(Str) - 2);
    Str := Str + '});';
    FPartner.ReplaceLine(Key, Indent2 + Str);
  end
  else
  begin
    if Trim(FList.Text) = '' then
    begin
      Str := 'private SpinnerNumberModel ' + Key + 'NumberModel(' + Value + ', '
        + IntToStr(Minimum) + ', ' + IntToStr(Maximum) + ', ' +
        IntToStr(StepSize) + ');';
      FPartner.ReplaceLine(Key, Indent2 + Str);
    end;
    if Attr = 'Value' then
    begin
      Key := Name + '.setValue(';
      if Trim(FList.Text) = '' then
        Str := Key + Value + ');'
      else
        Str := Key + '"' + Value + '");';
      SetAttributValue(Key, Indent2 + Str); // toDo check!
    end;
  end;
end;

procedure TJSpinner.MakeSpinnerColor(const Attr, Value: string);
var
  Str, Key: string;
begin
  Key := Name + '.getEditor()).getTextField().set' + Attr;
  Str := '((JSpinner.DefaultEditor)' + Name + '.getEditor()).getTextField().set'
    + Attr + '(' + GetAttrColor(Value) + ');';
  SetAttributValue(Key, Indent2 + Str);
end;

procedure TJSpinner.NewControl;
begin
  InsertNewVariable('private JSpinner ' + Name + ' = new JSpinner();');
  InsertNewVariable(Indent1 + 'private SpinnerNumberModel ' + Name +
    'Model = new SpinnerNumberModel(0, 0, 10, 1);');
  MakeAttribut('Value', '0');
  MakeAttribut('Model', Name + 'Model');
  FPartner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

procedure TJSpinner.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'Model', NewName + 'Model', True);
end;

function TJSpinner.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|stateChanged' + inherited GetEvents(ShowEvents);
end;

procedure TJSpinner.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private SpinnerNumberModel ' + Name + 'Model');
end;

destructor TJSpinner.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TJSpinner.Paint;
var
  Str: string;
  TextHeight, TextWidth, XPos, YPos: Integer;
  Points: array [0 .. 2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Font.Style := [];
  Canvas.Font.Color := Foreground;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  // paint number/value
  if Trim(FList.Text) = '' then
    Str := FValue
  else
    Str := List[0];
  TextWidth := Canvas.TextWidth(Str);
  TextHeight := Canvas.TextHeight(Str);
  if Trim(FList.Text) = '' then
    XPos := Width - 16 - TextWidth - 2
  else
    XPos := 2;
  YPos := (Height - TextHeight) div 2;
  Canvas.TextOut(XPos, YPos, Str);

  // paint up/down
  Canvas.Brush.Color := FNeutral;
  Canvas.Rectangle(Width - 16, 0, Width, Height div 2);
  Canvas.Rectangle(Width - 16, Height div 2, Width, Height);
  Canvas.Brush.Color := DarkShadow;
  XPos := Width - 16;
  YPos := Height div 4;
  Points[0] := Point(XPos + 5, YPos + 2);
  Points[1] := Point(XPos + 11, YPos + 2);
  Points[2] := Point(XPos + 8, YPos - 2);
  Canvas.Polygon(Points);
  YPos := (Height * 3) div 4;
  Points[0] := Point(XPos + 5, YPos - 2);
  Points[1] := Point(XPos + 11, YPos - 2);
  Points[2] := Point(XPos + 8, YPos + 2);
  Canvas.Polygon(Points);
end;

procedure TJSpinner.SetMaximum(AValue: Integer);
begin
  if (AValue <> FMaximum) and (AValue > FMinimum) then
  begin
    FMaximum := AValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetMinimum(AValue: Integer);
begin
  if (AValue <> FMinimum) and (AValue < FMaximum) then
  begin
    FMinimum := AValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetStepSize(AValue: Integer);
begin
  if AValue <> FStepSize then
  begin
    FStepSize := AValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetValue(const AValue: string);
var
  AInt: Integer;
  ADouble: Double;
begin
  if AValue <> FValue then
  begin
    Val(AValue, ADouble, AInt);
    if (AInt = 0) and (FMinimum <= ADouble) and (ADouble <= FMaximum) then
      FValue := AValue
    else if FList.IndexOf(AValue) > -1 then
      FValue := AValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetStrings(TheStrings: TStrings);
begin
  FList.Assign(TheStrings);
  if Trim(FList.Text) = '' then
    FValue := '0'
  else
    FValue := FList[0];
  Invalidate;
end;

end.
