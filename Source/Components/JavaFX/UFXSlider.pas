unit UFXSlider;

interface

uses
  Windows, Classes, Graphics,
  UJEComponents, UFXComponents;

type

  TFXSlider = class(TFXControl)
  private
    FMin,
    FMax,
    FValue: double;
    FBlockIncrement: double;
    FOrientation: TOrientation;
    FShowTickMarks: Boolean;
    FShowTickLabels: Boolean;
    FMinorTickCount: integer;
    FMajorTickUnit: double;
    FSnapToTicks: boolean;

    TrackColor: TColor;
    DefaultColor: TColor;
    ThumbSize: Integer;
    Offset: integer;
    TickSize: Integer;
    TrackRect: TRect;
    ThumbHalfSize: Integer;

    procedure SetMax(Value: double);
    procedure SetMin(Value: double);
    procedure SetValue(Value: double);
    procedure SetShowTickLabels(Value: Boolean);
    procedure SetShowTickMarks(Value: Boolean);
    procedure SetOrientation(aValue: TOrientation);
    procedure SetMinorTickCount(Value: integer);
    procedure SetMajorTickUnit(Value: double);
    function getSliderCode: string;
  protected
    procedure UpdateTrack;
    procedure UpdateTicks;
    procedure UpdateThumbData;
    procedure UpdateThumb;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
  published
    property Max: double read FMax write setMax;
    property Min: double read FMin write setMin;
    property Value: double read FValue write setValue;
    property ShowTickLabels: boolean read FShowTickLabels write setShowTickLabels;
    property ShowTickMarks: boolean read FShowTickMarks write setShowTickMarks;
    property Orientation: TOrientation read FOrientation write setOrientation;
    property MinorTickCount: integer read FMinorTickCount write setMinorTickCount;
    property MajorTickUnit: double read FMajorTickUnit write setMajorTickUnit;
    property BlockIncrement: double read FBlockIncrement write FBlockIncrement;
    property SnapToTicks: boolean read FSnapToTicks write FSnapToTicks;
  end;

implementation

uses SysUtils, UGUIDesigner;

constructor TFXSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 131;
  PrefWidth := 120;
  PrefHeight := 56;

  FMax := 100;
  FMin := 0;
  FValue := 0;
  FBlockIncrement:= 10;
  FMinorTickCount := 3;
  FMajorTickUnit := 25;
  FOrientation := HORIZONTAL;
  FShowTickMarks:= true;
  FShowTickLabels:= true;
  FSnapToTicks:= false;

  ThumbSize := 20;
  TickSize := 6;
  Offset:= 0;
  Font.Size:= 10;
  Font.Color:= RGB(130,130,130);

  Background:= DefaultBackground;   // BackgroundColor
  TrackColor:= DefaultBackground;
  DefaultColor:=  RGB(186, 186, 186);
  JavaType:= 'Slider';
end;

procedure TFXSlider.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  UpdateThumbData;
  UpdateTrack;   // the track
  UpdateTicks;   // the ticks
  UpdateThumb;   // the circle showing the position
end;

procedure TFXSlider.UpdateTrack;
  const Tracksize = 6;
begin
  if FOrientation = HORIZONTAL then begin
    TrackRect.Left := 5;
    TrackRect.Right := Width - 5;
    TrackRect.Top := Height div 2 - TrackSize div 2;
    if FShowTickMarks then TrackRect.Top:= TrackRect.Top - TickSize div 2;
    if FShowTickLabels then TrackRect.Top:= TrackRect.Top - Canvas.TextHeight('A') div 2;
    TrackRect.Bottom := TrackRect.Top + TrackSize;
  end else begin
    TrackRect.Top := 5;
    TrackRect.Bottom := Height - 5;
    TrackRect.Left := Width div 2 - TrackSize div 2;
    if FShowTickMarks then TrackRect.Left:= TrackRect.Left - TickSize div 2;
    if FShowTickLabels then Trackrect.Left:= TrackRect.Left - Canvas.TextHeight('A') div 2;
    TrackRect.Right := TrackRect.Left + TrackSize;
  end;
  Canvas.Pen.Color := DefaultColor;
  Canvas.Brush.Color := TrackColor;
  Canvas.RoundRect(TrackRect.Left, TrackRect.Top, TrackRect.Right, TrackRect.Bottom, 3, 3);
end;

function Double2Str(d: double): string;
  var s: string; p, i: integer;
begin
  s:= Format('%.3f', [d]);
  p:= Pos(',', s) + Pos('.', s);
  if p > 0 then begin
    s[p]:= '.';
    i:= length(s);
    while (i > p) and (s[i] = '0') do
      dec(i);
    if s[i] = '.' then
      dec(i);
    s:= copy(s, 1, i);
  end;
  Result:= s;
end;

procedure TFXSlider.UpdateTicks;
  var X, X2, Y2, Xi, Y, Yi, i: integer; dx, dy, d: double; s: string;
begin
  Canvas.Pen.Color := DefaultColor;
  Canvas.Brush.Style:= bsClear;
  if FOrientation = HORIZONTAL then begin
    if ShowTickMarks then begin
      d:= FMin;
      while d <= FMax do begin
        X:= ThumbHalfSize + Round(d / (FMax-FMin) * (Width - ThumbSize));
        Y:= TrackRect.Bottom + 4;
        Canvas.MoveTo(X, Y);
        Canvas.LineTo(X, Y+TickSize+2);
        d:= d + FMajorTickUnit;
        X2:= ThumbHalfSize + Round(d / (FMax-FMin) * (Width - ThumbSize));
        dx:= round((X2 -X)/(FMinorTickCount+1));
        for i:= 1 to minorTickCount do begin
          Xi:= X + Round(i*dx);
          Canvas.MoveTo(Xi, Y);
          Canvas.LineTo(Xi, Y+TickSize-1);
        end;
      end;
      X:= Width - ThumbHalfSize;
      Y:= TrackRect.Bottom + 4;
      Canvas.MoveTo(X, Y);
      Canvas.LineTo(X, Y+TickSize+2);
    end;
    if FShowTickLabels then begin
      d:= FMin;
      while d <= FMax do begin
        X:= ThumbHalfSize + Round(d / (FMax-FMin) * (Width - ThumbSize));
        Y:= TrackRect.Bottom + 4;
        if ShowTickMarks then y:= y + 2;
        s:= Double2Str(d);
        Canvas.TextOut(X-Canvas.TextWidth(s)div 2, Y+TickSize, s);
        d:= d + FMajorTickUnit;
      end;
      d:= FMax;
      X:= Width - ThumbHalfSize;
      Y:= TrackRect.Bottom + 4;
      if ShowTickMarks then y:= y + 2;
      s:= Double2Str(d);
      Canvas.TextOut(X-Canvas.TextWidth(s)div 2, Y+TickSize, s);
    end;
  end else begin
    if ShowTickMarks then begin
      d := FMin;
      while d <= FMax do  begin
        Y := ThumbHalfSize + Round(d / (FMax-FMin) * (Height - ThumbSize));
        X := TrackRect.Right + 4;
        Canvas.MoveTo(X,Y);
        Canvas.LineTo(X+TickSize+2,Y);
        d:= d + FMajorTickUnit;
        Y2:= ThumbHalfSize + Round(d / (FMax-FMin) * (Height - ThumbSize));
        dy:= round((Y2 - Y)/(FMinorTickCount+1));
        for i:= 1 to minorTickCount do begin
          Yi:= Y + Round(i*dy);
          Canvas.MoveTo(X,Yi);
          Canvas.LineTo(X+TickSize-1,Yi);
        end;
      end;
      Y:= Height - ThumbHalfSize;
      X := TrackRect.Right + 4;
      Canvas.MoveTo(X,Y);
      Canvas.LineTo(X+TickSize+2,Y);
    end;
    if FShowTickLabels then begin
      d:= FMin;
      while d <= FMax do begin
        Y := ThumbHalfSize + Round(d / (FMax-FMin) * (Height - ThumbSize));
        X := TrackRect.Right + 4;
        if ShowTickMarks then inc(X, 2);
        s:= Double2Str(FMax-(d-FMin));
        Canvas.TextOut(X+TickSize, Y-Canvas.TextHeight(s) div 2, s);
        d:= d + FMajorTickUnit;
      end;
      Y := Height - ThumbHalfSize;
      X := TrackRect.Right + 4;
      if ShowTickMarks then inc(X, 2);
      s:= Double2Str(FMin);
      Canvas.TextOut(X+TickSize, Y-Canvas.TextHeight(s) div 2, s);
    end;
  end;
end;

procedure TFXSlider.UpdateThumbData;
begin
  ThumbSize:= FGUIDesigner.vilControls21616.Width;
  ThumbHalfSize:= ThumbSize div 2;
end;

procedure TFXSlider.UpdateThumb;
  var X, Y: integer;
begin
  if FOrientation = HORIZONTAL then begin
    X := ThumbHalfSize + Round(FValue / (FMax-FMin) * (Width - ThumbSize));
    Y := (TrackRect.Top + TrackRect.Bottom) div 2;
  end else begin
    X := (TrackRect.Left + TrackRect.Right) div 2;
    Y := ThumbHalfSize + Round((FMax-(FValue-FMin)) / (FMax-FMin) * (Height - ThumbSize));
  end;
  FGUIDesigner.vilControls21616.Draw(Canvas, X - ThumbHalfSize, Y - ThumbHalfSize, 0);
end;

procedure TFXSlider.setMax(Value: double);
begin
  if Value <> FMax then begin
    FMax := Value;
    if FValue > FMax then FValue := FMax;
    Invalidate;
  end;
end;

procedure TFXSlider.setMin(Value: double);
begin
  if Value <> FMin then begin
    FMin := Value;
    if FValue < FMin then FValue := FMin;
    Invalidate;
  end;
end;

procedure TFXSlider.setValue(Value: double);
begin
  if Value <> FValue then begin
    FValue := Value;
    if FValue > FMax then FValue := FMax
    else if FValue < FMin then FValue := FMin;
    if csDesigning in ComponentState
      then Invalidate
      else UpdateThumb;
  end;
end;

procedure TFXSlider.setOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
      PrefWidth:= Width;
      PrefHeight:= Height;
    end;
    Invalidate;
  end;
end;

procedure TFXSlider.SetMinorTickCount(Value: integer);
begin
  if Value <> FMinorTickCount then begin
    FMinorTickCount := Value;
    Invalidate;
  end;
end;

procedure TFXSlider.SetMajorTickUnit(Value: double);
begin
  if (Value <> FMajorTickUnit) and (Value > 0) then begin
    FMajorTickUnit := Value;
    Invalidate;
  end;
end;

procedure TFXSlider.SetShowTickLabels(Value: Boolean);
begin
  if Value <> FShowTickLabels then begin
    FShowTickLabels := Value;
    Invalidate;
  end;
end;

procedure TFXSlider.SetShowTickMarks(Value: Boolean);
begin
  if Value <> FShowTickMarks then begin
    FShowTickMarks := Value;
    Invalidate;
  end;
end;

function TFXSlider.getSliderCode: string;
begin
  Result:= 'private Slider '  + Name + ' = new Slider(' +
            Double2Str(FMin) + ', ' + Double2Str(FMax) + ', ' + Double2Str(FValue) + ');';
end;

procedure TFXSlider.NewControl;
begin
  DefaultComponent;
  InsertNewVariable(getSliderCode);
  MakeAttribut('ShowTickLabels', 'true');
  MakeAttribut('ShowTickMarks', 'true');
end;

procedure TFXSlider.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Min') or (Attr = 'Max') or (Attr = 'Value') then
    Partner.ReplaceLine('private Slider '  + Name, Indent1 +  getSliderCode)
  else
    inherited
end;

function TFXSlider.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Name|Min|Max|Value|ShowTickLabels|ShowTickMarks';
        Attributes2 = '|BlockIncrement|MajorTickUnit|MinorTickCount|Orientation|SnapToTicks';
begin
  if ShowAttributes = 1
    then Result:= Attributes1
    else Result:= Attributes1 + Attributes2;
  Result:= Result + inherited;
end;

end.
