unit UJSpinner;

interface

uses
  Classes, Graphics, Spin, UJComponents;

type

  TJSpinner = class (TSwingComponent)
  private
    FNeutral: TColor;
    FMaximum: Integer;
    FMinimum: Integer;
    FStepSize: Integer;
    FValue: string;
    FList: TStrings;
    procedure setMaximum(aValue: integer);
    procedure setMinimum(aValue: integer);
    procedure setStepSize(aValue: integer);
    procedure setValue(const aValue: string);
    procedure setStrings(theStrings: TStrings);
    procedure MakeSpinnerColor(const Attr, Value: string);
    procedure MakeSpinner(const Attr: string);
  public
    constructor Create (AOwner: TComponent); override;
    constructor CreateFrom(aSpinEdit: TSpinEdit);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure DeleteComponent; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Maximum: integer read FMaximum write setMaximum;
    property Minimum: integer read FMinimum write setMinimum;
    property StepSize: integer read FStepSize write setStepSize;
    property Value: string read FValue write setValue;
    property List: TStrings read FList write setStrings;
  end;

implementation

uses Controls, SysUtils, Types, UUtils;

constructor TJSpinner.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 22;
  Width:= 80;
  Height:= 24;
  JavaType:= 'JSpinner';
  Background:= clWhite;
  FNeutral:= DefaultBackground;
  FMaximum:= 10;
  FMinimum:= 0;
  FStepSize:= 1;
  FValue:= '0';
  FList:= TStringList.Create;
end;

constructor TJSpinner.CreateFrom(aSpinEdit: TSpinEdit);
begin
  Create(aSpinEdit.Owner);
  CreateFromJ(aSpinEdit);
  Background:= aSpinEdit.Color;
  Font.assign(aSpinEdit.Font);
end;

function TJSpinner.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Maximum|Minimum|StepSize|Value|List' + inherited;
end;

procedure TJSpinner.setAttribute(Attr, Value, Typ: string);
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
  var s, key: string; i: integer;
begin
  key:= Name + 'Model = new Spinner';
  if (Attr = 'List') and (trim(FList.Text) <> '') then begin
    s:= 'private SpinnerListModel ' + key + 'ListModel(new String[] {';
    for i:= 0 to List.Count - 1 do
      s:= s + '"' + List[i] + '", ';
    s:= UUtils.Left(s, Length(s) -2);
    s:= s + '});';
    Partner.ReplaceLine(key, Indent2 + s);
  end else begin
    if trim(FList.Text) = '' then begin
      s:= 'private SpinnerNumberModel ' + key + 'NumberModel(' +
          Value + ', ' + IntToStr(Minimum) + ', ' +
          IntToStr(Maximum) + ', ' + IntToStr(StepSize) + ');';
      Partner.ReplaceLine(key, Indent2 + s);
    end;
    if Attr = 'Value' then begin
      key:= Name + '.setValue(';
      if trim(FList.Text) = ''
        then s:= key + Value + ');'
        else s:= key + '"' + Value + '");';
      setAttributValue(key, Indent2 + s);  // toDo check!
    end;
  end;
end;

procedure TJSpinner.MakeSpinnerColor(const Attr, Value: string);
  var s, key: string;
begin
  key:= Name + '.getEditor()).getTextField().set' + Attr;
  s:= '((JSpinner.DefaultEditor)' + Name + '.getEditor()).getTextField().set' + Attr + '(' + getAttrColor(Value) + ');';
  setAttributValue(key, Indent2 + s);
end;

procedure TJSpinner.NewControl;
begin
  InsertNewVariable('private JSpinner ' + Name + ' = new JSpinner();');
  InsertNewVariable(Indent1 + 'private SpinnerNumberModel ' + Name + 'Model = new SpinnerNumberModel(0, 0, 10, 1);');
  MakeAttribut('Value', '0');
  MakeAttribut('Model', Name + 'Model');
  Partner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

procedure TJSpinner.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'Model' , NewName + 'Model', true);
end;

function TJSpinner.getEvents(ShowEvents: integer): string;
begin
  Result:= '|stateChanged' + inherited getEvents(ShowEvents);
end;

procedure TJSpinner.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private SpinnerNumberModel ' + Name + 'Model');
end;

destructor TJSpinner.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TJSpinner.Paint;
  var s: string;
      th, tw, x, y: integer;
      Points: array[0..2] of TPoint;
begin
  CanvasFontAssign;
  Canvas.Font.Style:= [];
  Canvas.Font.Color:= Foreground;
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  // paint number/value
  if trim(FList.Text) = ''
    then s:= FValue
    else s:= List[0];
  tw:= Canvas.TextWidth(s);
  th:= Canvas.TextHeight(s);
  if trim(FList.Text) = ''
    then x:= Width - 16 - tw - 2
    else x:= 2;
  y:= (Height - th) div 2;
  Canvas.TextOut(x, y, s);

  // paint up/down
  Canvas.Brush.Color:= FNeutral;
  Canvas.Rectangle(Width-16, 0, Width, Height div 2);
  Canvas.Rectangle(Width-16, Height div 2, Width, Height);
  Canvas.Brush.Color:= DarkShadow;
  x:= Width - 16;
  y:= Height div 4;
  Points[0]:= Point(x +  5, y + 2);
  Points[1]:= Point(x + 11, y + 2);
  Points[2]:= Point(x +  8, y - 2);
  Canvas.Polygon(Points);
  y:= (Height*3) div 4;
  Points[0]:= Point(x +  5, y - 2);
  Points[1]:= Point(x + 11, y - 2);
  Points[2]:= Point(x +  8, y + 2);
  Canvas.Polygon(Points);
end;

procedure TJSpinner.SetMaximum(aValue: integer);
begin
  if (aValue <> FMaximum) and (aValue > FMinimum) then begin
    FMaximum:= aValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetMinimum(aValue: integer);
begin
  if (aValue <> FMinimum) and (aValue < FMaximum) then begin
    FMinimum:= aValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetStepSize(aValue: integer);
begin
  if aValue <> FStepSize then begin
    FStepSize:= aValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetValue(const aValue: string);
  var e: integer; d: double;
begin
  if aValue <> FValue then begin
    val(aValue, d, e);
    if (e = 0) and (FMinimum <= d) and (d <= FMaximum) then
      FValue:= aValue
    else if FList.IndexOf(aValue) > -1 then
      FValue:= aValue;
    Invalidate;
  end;
end;

procedure TJSpinner.SetStrings(theStrings: TStrings);
begin
  FList.assign(theStrings);
  if trim(FList.Text) = ''
    then FValue:= '0'
    else FValue:= FList.Strings[0];
  Invalidate;
end;

end.
