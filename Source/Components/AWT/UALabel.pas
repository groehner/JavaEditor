unit UALabel;

interface

uses
  Classes,
  StdCtrls,
  UAComponents;

type

  TALabel = class(TAWTComponent)
  private
    FAlignment: THorizontalAlignment;
    FText: string;
    procedure SetHorizontalAlignment(AValue: THorizontalAlignment);
    procedure SetText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NewControl; override;
    procedure SizeToText; override;
    procedure NameFromText; override;
    procedure Paint; override;
  published
    property Alignment: THorizontalAlignment read FAlignment write SetHorizontalAlignment;
    property Text: string read FText write SetText;
  end;

implementation

uses
  Graphics,
  Controls,
  UITypes;

{ --- TALabel ------------------------------------------------------------------ }

constructor TALabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -1;
  Width := 80;
  Height := 24;
  FText := 'Label';
  Alignment := UAComponents.LEFT;
  Background := clBtnFace;
  JavaType := 'Label';
end;

function TALabel.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Alignment|Text|Font' + inherited GetAttributes(ShowAttributes);
end;

procedure TALabel.SetAttribute(Attr, Value, Typ: string);
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
  MakeAttribut('Text', AsString(Text));
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
var
  XPos, YPos, TextW: Integer;
  Str: string;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Background;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Str := FText;
  Canvas.Font.Color := Foreground;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  YPos := (Height - Canvas.TextHeight(Str)) div 2;
  TextW := Canvas.TextWidth(Str);
  case FAlignment of
    UAComponents.CENTER:
      XPos := (Width - 4 - TextW) div 2;
    UAComponents.LEFT:
      XPos := 2;
  else { RIGHT }
    XPos := Width - 4 - TextW;
  end;
  Canvas.TextRect(Rect(0, 0, Width, Height), XPos, YPos, Str);
end;

procedure TALabel.SetHorizontalAlignment(AValue: THorizontalAlignment);
begin
  if AValue <> FAlignment then
  begin
    FAlignment := AValue;
    Invalidate;
  end;
end;

procedure TALabel.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

end.
