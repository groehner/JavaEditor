unit UFXSeparator;

interface

uses
  Classes,
  UJEComponents,
  UFXComponents;

type

  THpos = (CENTER, LEFT, RIGHT);
  TVpos = (_TV_BASELINE, _TV_BOTTOM, _TV_CENTER, _TV_TOP);

  TFXSeparator = class(TFXControl)
  private
    FOrientation: TOrientation;
    FHalignment: THpos;
    FValignment: TVpos;
    procedure SetOrientation(AValue: TOrientation);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
  published
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Halignment: THpos read FHalignment write FHalignment;
    property Valignment: TVpos read FValignment write FValignment;
  end;

implementation

uses Graphics;

{ --- TFXSeparator -------------------------------------------------------------- }

constructor TFXSeparator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +135;
  PrefWidth := 80;
  PrefHeight := 2;
  Background := clWhite;
  JavaType := 'Separator';
end;

procedure TFXSeparator.Paint;
begin
  if Orientation = HORIZONTAL then
  begin
    Canvas.Pen.Color := Foreground;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width - 1, 0);
    Canvas.Pen.Color := Background;
    Canvas.MoveTo(0, 1);
    Canvas.LineTo(Width - 1, 1);
  end
  else
  begin
    Canvas.Pen.Color := Foreground;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, Height - 1);
    Canvas.Pen.Color := Background;
    Canvas.MoveTo(1, 0);
    Canvas.LineTo(1, Height - 1);
  end;
end;

procedure TFXSeparator.SetOrientation(AValue: TOrientation);
var
  Tmp: Integer;
begin
  if AValue <> FOrientation then
  begin
    FOrientation := AValue;
    if not(csLoading in ComponentState) then
    begin
      Tmp := Width;
      Width := Height;
      Height := Tmp;
    end;
    Invalidate;
  end;
end;

procedure TFXSeparator.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Separator ' + Name + ' = new Separator();');
  InsertImport('javafx.geometry.*');
end;

procedure TFXSeparator.SetAttribute(Attr, Value, Typ: string);
var
  Str1, Str2: string;
begin
  if Attr = 'Halignment' then
  begin
    Str1 := Name + '.setHalignment';
    Str2 := Str1 + '(HPos.' + Value + ');';
    SetAttributValue(Str1, Str2);
  end
  else if Attr = 'Valignment' then
  begin
    Str1 := Name + '.setValignment';
    Str2 := Str1 + '(VPos.' + Value + ');';
    SetAttributValue(Str1, Str2);
  end
  else
    inherited;
end;

function TFXSeparator.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Orientation|Halignment|Valignment' + inherited GetAttributes
    (ShowAttributes);
end;

end.
