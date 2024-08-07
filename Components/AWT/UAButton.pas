unit UAButton;

interface

uses
  Classes, StdCtrls, UAComponents;

type

  TAButton = class (TAWTComponent)
  private
    FText: string;
    procedure setText(const aValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aButton: TButton);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure SizeToText; override;
    procedure NameFromText; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Text: string read FText write setText;
    property actionPerformed;
  end;

implementation

uses Windows, Graphics, Controls;

{--- TAButton -----------------------------------------------------------------}

constructor TAButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= -4;
  Width:= 80;
  Height:= 24;
  Foreground:= clBlack;
  Background:= DefaultBackground;
  FText:= 'Button';
  JavaType:= 'Button';
end;

constructor TAButton.CreateFrom(aButton: TButton);
begin
  Create(aButton.Owner);
  CreateFromA(aButton);
  Text:= aButton.Caption;
  Font:= aButton.Font;
  Foreground:= Font.Color;
end;

function TAButton.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Text' + inherited getAttributes(ShowAttributes);
end;

procedure TAButton.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    inherited setAttribute('Label', Value, Typ)
  else
    inherited;
  if (Attr = 'Foreground') and (Value = '(NONE)') then
    Foreground:= clBlack;
end;

function TAButton.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed' + inherited getEvents(ShowEvents);
end;

procedure TAButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Button ' + Name + ' = new Button();');
  MakeAttribut('Label', asString(FText));
  AddListener('actionPerformed');
end;

procedure TAButton.SizeToText;
begin
  SizeToText(Text);
end;

procedure TAButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('b' + Text);
end;

procedure TAButton.Paint;
   var x, y: integer; s: string;
begin
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(0, 0, Width-1, Height-1);
  // left-top outside
  Canvas.MoveTo(0, Height-2);
  Canvas.LineTo(0, 0);
  Canvas.LineTo(Width-1, 0);
  // right-bottom  outside
  Canvas.Pen.Color:= RGB(105, 105, 105);
  Canvas.LineTo(Width-1, Height-1);
  Canvas.LineTo(-1, Height-1);
  // left-to inside
  Canvas.Pen.Color:= RGB(227, 227, 227);
  Canvas.MoveTo(1, Height-2);
  Canvas.LineTo(1, 1);
  Canvas.LineTo(Width-2, 1);
  // right-bottom inside
  Canvas.Pen.Color:= RGB(160, 160, 160);
  Canvas.LineTo(Width-2, Height-2);
  Canvas.LineTo(0, Height-2);
  // text
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Brush.Style:= bsClear;
  s:= FText;
  x:= (Width -  Canvas.TextWidth(s)) div 2;
  y:= (Height - Canvas.TextHeight(s)) div 2;
  Canvas.TextOut(x, y, s);
end;

procedure TAButton.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

end.

