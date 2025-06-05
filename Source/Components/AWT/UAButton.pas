unit UAButton;

interface

uses
  Classes,
  StdCtrls,
  UAComponents;

type

  TAButton = class(TAWTComponent)
  private
    FText: string;
    procedure SetText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AButton: TButton);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure SizeToText; override;
    procedure NameFromText; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Text: string read FText write SetText;
    property actionPerformed;
  end;

implementation

uses
  Windows,
  Graphics,
  Controls;

{ --- TAButton ----------------------------------------------------------------- }

constructor TAButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -4;
  Width := 80;
  Height := 24;
  Foreground := clBlack;
  Background := DefaultBackground;
  FText := 'Button';
  JavaType := 'Button';
end;

constructor TAButton.CreateFrom(AButton: TButton);
begin
  Create(AButton.Owner);
  CreateFromA(AButton);
  Text := AButton.Caption;
  Font := AButton.Font;
  Foreground := Font.Color;
end;

function TAButton.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Text' + inherited GetAttributes(ShowAttributes);
end;

procedure TAButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    inherited SetAttribute('Label', Value, Typ)
  else
    inherited;
  if (Attr = 'Foreground') and (Value = '(NONE)') then
    Foreground := clBlack;
end;

function TAButton.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed' + inherited GetEvents(ShowEvents);
end;

procedure TAButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Button ' + Name + ' = new Button();');
  MakeAttribut('Label', AsString(FText));
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
var
  XPos, YPos: Integer;
  Str: string;
begin
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(0, 0, Width - 1, Height - 1);
  // left-top outside
  Canvas.MoveTo(0, Height - 2);
  Canvas.LineTo(0, 0);
  Canvas.LineTo(Width - 1, 0);
  // right-bottom  outside
  Canvas.Pen.Color := RGB(105, 105, 105);
  Canvas.LineTo(Width - 1, Height - 1);
  Canvas.LineTo(-1, Height - 1);
  // left-to inside
  Canvas.Pen.Color := RGB(227, 227, 227);
  Canvas.MoveTo(1, Height - 2);
  Canvas.LineTo(1, 1);
  Canvas.LineTo(Width - 2, 1);
  // right-bottom inside
  Canvas.Pen.Color := RGB(160, 160, 160);
  Canvas.LineTo(Width - 2, Height - 2);
  Canvas.LineTo(0, Height - 2);
  // text
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Brush.Style := bsClear;
  Str := FText;
  XPos := (Width - Canvas.TextWidth(Str)) div 2;
  YPos := (Height - Canvas.TextHeight(Str)) div 2;
  Canvas.TextOut(XPos, YPos, Str);
end;

procedure TAButton.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

end.
