unit UFXProgressBar;

interface

uses
  Classes, UFXComponents;

type

  TFXProgressIndicator = class(TFXControl)
  private
    FProgress: double;
    procedure setProgress(Progress: double);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
  published
    property Progress: double read FProgress write setProgress;
  end;

  TFXProgressBar = class(TFXProgressIndicator)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
  end;

implementation

uses Windows, SysUtils, Graphics, Math;

constructor TFXProgressIndicator.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= 133;
  PrefWidth:= 40;
  PrefHeight:= 40;
  FProgress:= 0.3;
  Foreground:= RGB(0, 142, 190);
  Background:= DefaultBackground;
  JavaType:= 'ProgressIndicator';
end;

procedure TFXProgressIndicator.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ProgressIndicator '  + Name + ' = new ProgressIndicator();');
  MakeAttribut('Progress', '0.3');
end;

function TFXProgressIndicator.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Progress' + inherited getAttributes(ShowAttributes);
end;

procedure TFXProgressIndicator.Paint;
  var Pos, tw, th, _left, diameter, x, y: Integer; s: string;
      alpha: double;
begin
  CanvasFontAssign;
  Canvas.Brush.Color:= Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Pen.Color:= Foreground;
  Pos:= Round(FProgress*100);
  s:= IntToStr(Pos) + ' %';
  tw:= Canvas.TextWidth(s);
  th:= Canvas.TextHeight(s);

  diameter:= Math.min(Width, Height - th);
  _left:= (width - diameter) div 2;
  Canvas.TextOut((width - tw) div 2, diameter, s);

  Canvas.Brush.Color:= clWhite;
  Canvas.Pen.Color:= RGB(168, 168, 168);
  Canvas.Ellipse(_left, 0, _left + diameter, diameter);

  Canvas.Brush.Color:= Foreground;
  alpha:= 2*PI*FProgress;
  x:= round(sin(alpha)*diameter);
  y:= round(cos(alpha)*diameter);
  if FProgress > 0 then
    Canvas.Pie(_left, 0, _left + diameter, diameter,
               _left + diameter div 2 + x, diameter div 2 - y, _left + diameter div 2, 0);
end;

procedure TFXProgressIndicator.setProgress(Progress: double);
begin
  if Progress <> FProgress then begin
    FProgress:= Progress;
    Invalidate;
  end;
end;

{--- TFXProgressBar -----------------------------------------------------------}

constructor TFXProgressBar.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= 132;
  PrefWidth:= 120;
  PrefHeight:= 24;
  JavaType:= 'ProgressBar';
end;

procedure TFXProgressBar.Paint;
  var Pos: Integer;
begin
  Canvas.Brush.Color:= Background;
  Canvas.Pen.Color:= RGB(168, 168, 168);
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Pos:= Round(Width * FProgress);
  Canvas.Brush.Color:= Foreground;
  Canvas.FillRect(Rect(2, 2, Pos, Height-2));
end;

procedure TFXProgressBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ProgressBar '  + Name + ' = new ProgressBar();');
  MakeAttribut('Progress', '0.3');
end;

end.
