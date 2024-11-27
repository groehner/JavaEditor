unit UALabel;

interface

uses
  Classes, StdCtrls, UAComponents;

type

  TALabel = class(TAWTComponent)
  private
    FAlignment: THorzAlignment;
    FText: string;
    procedure setHorzAlignment(aValue: THorzAlignment);
    procedure setText(const aValue: string);
  public
    constructor Create(aOwner: TComponent); override;
    constructor CreateFrom(aLabel: TLabel);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure SizeToText; override;
    procedure NameFromText; override;
    procedure Paint; override;
  published
    property Alignment: THorzAlignment read FAlignment write setHorzAlignment;
    property Text: string read FText write setText;
  end;

implementation

uses Graphics, Controls, UITypes;

{--- TALabel ------------------------------------------------------------------}

constructor TALabel.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= -1;
  Width:= 80;
  Height:= 24;
  FText:= 'Label';
  Alignment:= UAComponents.LEFT;
  Background:= clBtnFace;
  JavaType:= 'Label';
end;

constructor TALabel.createFrom(aLabel: TLabel);
begin
  Create(aLabel.Owner);
  CreateFromA(aLabel);
  Text:= aLabel.Caption;
  Font:= aLabel.Font;
  Foreground:= Font.Color;
  Background:= aLabel.Color;
  if Background = clBtnFace then
    Background:= clWhite;
  case aLabel.Alignment of
    taLeftJustify : FAlignment:= UAComponents.LEFT;
    taRightJustify: FAlignment:= UAComponents.RIGHT;
    taCenter      : FAlignment:= UAComponents.CENTER;
  end;
end;

function TALabel.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Alignment|Text|Font' + inherited getAttributes(ShowAttributes);
end;

procedure TALabel.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Alignment' then
    MakeAttribut('Alignment', 'Label.' + Value)
  else
    inherited;
end;

procedure TALabel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Label ' + Name + ' = new Label();');
  MakeAttribut('Text', asString(Text));
end;

procedure TALabel.SizeToText;
begin
  SizeToText(Text);
end;

procedure TALabel.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('l' + Text);
end;

procedure TALabel.Paint;
  var x, y, tw: integer; s: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= Background;
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TWinControl).Brush.Color
    else Canvas.Brush.Color:= Background;
  s:= FText;
  Canvas.Font.Color:= Foreground;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  y:= (Height - Canvas.TextHeight(s)) div 2;
  tw:= Canvas.TextWidth(s);
  case FAlignment of
    UAComponents.CENTER: x:= (Width-4 - tw) div 2;
    UAComponents.LEFT  : x:= 2;
    else { RIGHT }       x:= Width-4 - tw;
  end;
  Canvas.TextRect(Rect(0, 0, Width, Height), x, y, s);
end;

procedure TALabel.setHorzAlignment(aValue: THorzAlignment);
begin
  if aValue <> FAlignment then begin
    FAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TALabel.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

end.
