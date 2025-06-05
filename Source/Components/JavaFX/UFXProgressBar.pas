unit UFXProgressBar;

interface

uses
  Classes,
  UFXComponents;

type

  TFXProgressIndicator = class(TFXControl)
  private
    FProgress: Double;
    procedure SetProgress(Progress: Double);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
  published
    property Progress: Double read FProgress write SetProgress;
  end;

  TFXProgressBar = class(TFXProgressIndicator)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Graphics,
  Math;

constructor TFXProgressIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 133;
  PrefWidth := 40;
  PrefHeight := 40;
  FProgress := 0.3;
  Foreground := RGB(0, 142, 190);
  Background := DefaultBackground;
  JavaType := 'ProgressIndicator';
end;

procedure TFXProgressIndicator.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ProgressIndicator ' + Name +
    ' = new ProgressIndicator();');
  MakeAttribut('Progress', '0.3');
end;

function TFXProgressIndicator.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Progress' + inherited GetAttributes(ShowAttributes);
end;

procedure TFXProgressIndicator.Paint;
var
  Pos, TextWidth, TextHeight, ALeft, Diameter, XPos, YPos: Integer;
  Str: string;
  Alpha: Double;
begin
  CanvasFontAssign;
  Canvas.Brush.Color := Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Pen.Color := Foreground;
  Pos := Round(FProgress * 100);
  Str := IntToStr(Pos) + ' %';
  TextWidth := Canvas.TextWidth(Str);
  TextHeight := Canvas.TextHeight(Str);

  Diameter := Math.Min(Width, Height - TextHeight);
  ALeft := (Width - Diameter) div 2;
  Canvas.TextOut((Width - TextWidth) div 2, Diameter, Str);

  Canvas.Brush.Color := clWhite;
  Canvas.Pen.Color := RGB(168, 168, 168);
  Canvas.Ellipse(ALeft, 0, ALeft + Diameter, Diameter);

  Canvas.Brush.Color := Foreground;
  Alpha := 2 * Pi * FProgress;
  XPos := Round(Sin(Alpha) * Diameter);
  YPos := Round(Cos(Alpha) * Diameter);
  if FProgress > 0 then
    Canvas.Pie(ALeft, 0, ALeft + Diameter, Diameter, ALeft + Diameter div 2 +
      XPos, Diameter div 2 - YPos, ALeft + Diameter div 2, 0);
end;

procedure TFXProgressIndicator.SetProgress(Progress: Double);
begin
  if Progress <> FProgress then
  begin
    FProgress := Progress;
    Invalidate;
  end;
end;

{ --- TFXProgressBar ----------------------------------------------------------- }

constructor TFXProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 132;
  PrefWidth := 120;
  PrefHeight := 24;
  JavaType := 'ProgressBar';
end;

procedure TFXProgressBar.Paint;
var
  Pos: Integer;
begin
  Canvas.Brush.Color := Background;
  Canvas.Pen.Color := RGB(168, 168, 168);
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Pos := Round(Width * FProgress);
  Canvas.Brush.Color := Foreground;
  Canvas.FillRect(Rect(2, 2, Pos, Height - 2));
end;

procedure TFXProgressBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ProgressBar ' + Name + ' = new ProgressBar();');
  MakeAttribut('Progress', '0.3');
end;

end.
