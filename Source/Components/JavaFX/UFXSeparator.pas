unit UFXSeparator;

interface

uses
  Classes, UJEComponents, UFXComponents;

type

  THpos = (CENTER, LEFT, RIGHT);
  TVpos = (_TV_BASELINE, _TV_BOTTOM, _TV_CENTER, _TV_TOP);

  TFXSeparator = class (TFXControl)
  private
    FOrientation: TOrientation;
    FHalignment: THpos;
    FValignment: TVPos;
    procedure setOrientation(aValue: TOrientation);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
  published
    property Orientation: TOrientation read FOrientation write setOrientation;
    property Halignment: THpos read FHalignment write FHalignment;
    property Valignment: TVpos read FValignment write FValignment;
  end;

implementation

uses Graphics;

{--- TFXSeparator --------------------------------------------------------------}

constructor TFXSeparator.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +135;
  PrefWidth:= 80;
  PrefHeight:= 2;
  Background:= clWhite;
  JavaType:= 'Separator';
end;

procedure TFXSeparator.Paint;
begin
  if Orientation = HORIZONTAL then begin
    Canvas.Pen.Color:= Foreground;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width-1, 0);
    Canvas.Pen.Color:= Background;
    Canvas.MoveTo(0, 1);
    Canvas.LineTo(Width-1, 1);
  end else begin
    Canvas.Pen.Color:= Foreground;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, Height-1);
    Canvas.Pen.Color:= Background;
    Canvas.MoveTo(1, 0);
    Canvas.LineTo(1, Height-1);
  end;
end;

procedure TFXSeparator.setOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
    end;
    Invalidate;
  end;
end;

procedure TFXSeparator.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Separator '  + Name + ' = new Separator();');
  InsertImport('javafx.geometry.*');
end;

procedure TFXSeparator.SetAttribute(Attr, Value, Typ: string);
  var s1, s2: string;
begin
  if Attr = 'Halignment' then begin
    s1:= Name + '.setHalignment';
    s2:= s1 + '(HPos.' + Value + ');';
    setAttributValue(s1, s2);
  end else if Attr = 'Valignment' then begin
    s1:= Name + '.setValignment';
    s2:= s1 + '(VPos.' + Value + ');';
    setAttributValue(s1, s2);
  end else
    inherited;
end;

function TFXSeparator.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Orientation|Halignment|Valignment' + inherited getAttributes(ShowAttributes);
end;

end.
