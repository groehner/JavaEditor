unit UFXLabel;

interface

uses
  Classes,
  UFXLabeled;

type
  TFXLabel = class (TFXLabeled)
  private
    FLabelFor: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SizeToText; override;
    procedure NameFromText; override;
    procedure Paint; override;
  published
    property LabelFor: string read FLabelFor write FLabelFor;
  end;

implementation

{--- TFXLabel ------------------------------------------------------------------}

constructor TFXLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +101;
  PrefWidth:= 80;
  PrefHeight:= 24;
  Text:= 'Label';
  JavaType:= 'Label';
end;

procedure TFXLabel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Label ' + Name + ' = new Label();');
  MakeAttribut('Text', AsString(Text));
  MakeFont;
end;

procedure TFXLabel.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Graphic' then
    MakeGraphic(Attr, Value, 'Label')
  else if Attr = 'LabelFor' then
    MakeAttribut('LabelFor', Value)
  else
    inherited;
end;

function TFXLabel.GetAttributes(ShowAttributes: Integer): string;
begin
  Result:= '|LabelFor' + inherited GetAttributes(ShowAttributes);
end;

procedure TFXLabel.SizeToText;
begin
  SizeToText(Text);
end;

procedure TFXLabel.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('l' + Text);
end;

procedure TFXLabel.Paint;
begin
  Canvas.Brush.Color:= Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  inherited;
end;

end.
