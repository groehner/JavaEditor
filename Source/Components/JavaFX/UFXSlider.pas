unit UFXSlider;

interface

uses
  Windows, Classes, Graphics,
  UJEComponents, UFXComponents;

type

  TFXSlider = class(TFXControl)
  private
    FMin, FMax, FValue: Double;
    FBlockIncrement: Double;
    FOrientation: TOrientation;
    FShowTickMarks: Boolean;
    FShowTickLabels: Boolean;
    FMinorTickCount: Integer;
    FMajorTickUnit: Double;
    FSnapToTicks: Boolean;
    FTrackColor: TColor;
    FDefaultColor: TColor;
    FThumbSize: Integer;
    FOffset: Integer;
    FTickSize: Integer;
    FTrackRect: TRect;
    FThumbHalfSize: Integer;
    procedure SetMax(Value: Double);
    procedure SetMin(Value: Double);
    procedure SetValue(Value: Double);
    procedure SetShowTickLabels(Value: Boolean);
    procedure SetShowTickMarks(Value: Boolean);
    procedure SetOrientation(AValue: TOrientation);
    procedure SetMinorTickCount(Value: Integer);
    procedure SetMajorTickUnit(Value: Double);
    function GetSliderCode: string;
  protected
    procedure UpdateTrack;
    procedure UpdateTicks;
    procedure UpdateThumbData;
    procedure UpdateThumb;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
  published
    property Max: Double read FMax write SetMax;
    property Min: Double read FMin write SetMin;
    property Value: Double read FValue write SetValue;
    property ShowTickLabels: Boolean read FShowTickLabels
      write SetShowTickLabels;
    property ShowTickMarks: Boolean read FShowTickMarks write SetShowTickMarks;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property MinorTickCount: Integer read FMinorTickCount
      write SetMinorTickCount;
    property MajorTickUnit: Double read FMajorTickUnit write SetMajorTickUnit;
    property BlockIncrement: Double read FBlockIncrement write FBlockIncrement;
    property SnapToTicks: Boolean read FSnapToTicks write FSnapToTicks;
  end;

implementation

uses
  SysUtils,
  UGUIDesigner;

constructor TFXSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 131;
  PrefWidth := 120;
  PrefHeight := 56;

  FMax := 100;
  FMin := 0;
  FValue := 0;
  FBlockIncrement := 10;
  FMinorTickCount := 3;
  FMajorTickUnit := 25;
  FOrientation := HORIZONTAL;
  FShowTickMarks := True;
  FShowTickLabels := True;
  FSnapToTicks := False;

  FThumbSize := 20;
  FTickSize := 6;
  FOffset := 0;
  Font.Size := 10;
  Font.Color := RGB(130, 130, 130);

  Background := DefaultBackground; // BackgroundColor
  FTrackColor := DefaultBackground;
  FDefaultColor := RGB(186, 186, 186);
  JavaType := 'Slider';
end;

procedure TFXSlider.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  UpdateThumbData;
  UpdateTrack; // the track
  UpdateTicks; // the ticks
  UpdateThumb; // the circle showing the position
end;

procedure TFXSlider.UpdateTrack;
const
  TrackSize = 6;
begin
  if FOrientation = HORIZONTAL then
  begin
    FTrackRect.Left := 5;
    FTrackRect.Right := Width - 5;
    FTrackRect.Top := Height div 2 - TrackSize div 2;
    if FShowTickMarks then
      FTrackRect.Top := FTrackRect.Top - FTickSize div 2;
    if FShowTickLabels then
      FTrackRect.Top := FTrackRect.Top - Canvas.TextHeight('A') div 2;
    FTrackRect.Bottom := FTrackRect.Top + TrackSize;
  end
  else
  begin
    FTrackRect.Top := 5;
    FTrackRect.Bottom := Height - 5;
    FTrackRect.Left := Width div 2 - TrackSize div 2;
    if FShowTickMarks then
      FTrackRect.Left := FTrackRect.Left - FTickSize div 2;
    if FShowTickLabels then
      FTrackRect.Left := FTrackRect.Left - Canvas.TextHeight('A') div 2;
    FTrackRect.Right := FTrackRect.Left + TrackSize;
  end;
  Canvas.Pen.Color := FDefaultColor;
  Canvas.Brush.Color := FTrackColor;
  Canvas.RoundRect(FTrackRect.Left, FTrackRect.Top, FTrackRect.Right,
    FTrackRect.Bottom, 3, 3);
end;

function Double2Str(ADouble: Double): string;
var
  Str: string;
  Posi, Int: Integer;
begin
  Str := Format('%.3f', [ADouble]);
  Posi := Pos(',', Str) + Pos('.', Str);
  if Posi > 0 then
  begin
    Str[Posi] := '.';
    Int := Length(Str);
    while (Int > Posi) and (Str[Int] = '0') do
      Dec(Int);
    if Str[Int] = '.' then
      Dec(Int);
    Str := Copy(Str, 1, Int);
  end;
  Result := Str;
end;

procedure TFXSlider.UpdateTicks;
var
  XPos, X2Pos, XIPos, YPos, Y2Pos, YIPos: Integer;
  DeltaX, DeltaY, ADouble: Double;
  Str: string;
begin
  Canvas.Pen.Color := FDefaultColor;
  Canvas.Brush.Style := bsClear;
  if FOrientation = HORIZONTAL then
  begin
    if ShowTickMarks then
    begin
      ADouble := FMin;
      while ADouble <= FMax do
      begin
        XPos := FThumbHalfSize + Round(ADouble / (FMax - FMin) *
          (Width - FThumbSize));
        YPos := FTrackRect.Bottom + 4;
        Canvas.MoveTo(XPos, YPos);
        Canvas.LineTo(XPos, YPos + FTickSize + 2);
        ADouble := ADouble + FMajorTickUnit;
        X2Pos := FThumbHalfSize +
          Round(ADouble / (FMax - FMin) * (Width - FThumbSize));
        DeltaX := Round((X2Pos - XPos) / (FMinorTickCount + 1));
        for var I := 1 to MinorTickCount do
        begin
          XIPos := XPos + Round(I * DeltaX);
          Canvas.MoveTo(XIPos, YPos);
          Canvas.LineTo(XIPos, YPos + FTickSize - 1);
        end;
      end;
      XPos := Width - FThumbHalfSize;
      YPos := FTrackRect.Bottom + 4;
      Canvas.MoveTo(XPos, YPos);
      Canvas.LineTo(XPos, YPos + FTickSize + 2);
    end;
    if FShowTickLabels then
    begin
      ADouble := FMin;
      while ADouble <= FMax do
      begin
        XPos := FThumbHalfSize + Round(ADouble / (FMax - FMin) *
          (Width - FThumbSize));
        YPos := FTrackRect.Bottom + 4;
        if ShowTickMarks then
          YPos := YPos + 2;
        Str := Double2Str(ADouble);
        Canvas.TextOut(XPos - Canvas.TextWidth(Str) div 2,
          YPos + FTickSize, Str);
        ADouble := ADouble + FMajorTickUnit;
      end;
      ADouble := FMax;
      XPos := Width - FThumbHalfSize;
      YPos := FTrackRect.Bottom + 4;
      if ShowTickMarks then
        YPos := YPos + 2;
      Str := Double2Str(ADouble);
      Canvas.TextOut(XPos - Canvas.TextWidth(Str) div 2, YPos + FTickSize, Str);
    end;
  end
  else
  begin
    if ShowTickMarks then
    begin
      ADouble := FMin;
      while ADouble <= FMax do
      begin
        YPos := FThumbHalfSize + Round(ADouble / (FMax - FMin) *
          (Height - FThumbSize));
        XPos := FTrackRect.Right + 4;
        Canvas.MoveTo(XPos, YPos);
        Canvas.LineTo(XPos + FTickSize + 2, YPos);
        ADouble := ADouble + FMajorTickUnit;
        Y2Pos := FThumbHalfSize +
          Round(ADouble / (FMax - FMin) * (Height - FThumbSize));
        DeltaY := Round((Y2Pos - YPos) / (FMinorTickCount + 1));
        for var I := 1 to MinorTickCount do
        begin
          YIPos := YPos + Round(I * DeltaY);
          Canvas.MoveTo(XPos, YIPos);
          Canvas.LineTo(XPos + FTickSize - 1, YIPos);
        end;
      end;
      YPos := Height - FThumbHalfSize;
      XPos := FTrackRect.Right + 4;
      Canvas.MoveTo(XPos, YPos);
      Canvas.LineTo(XPos + FTickSize + 2, YPos);
    end;
    if FShowTickLabels then
    begin
      ADouble := FMin;
      while ADouble <= FMax do
      begin
        YPos := FThumbHalfSize + Round(ADouble / (FMax - FMin) *
          (Height - FThumbSize));
        XPos := FTrackRect.Right + 4;
        if ShowTickMarks then
          Inc(XPos, 2);
        Str := Double2Str(FMax - (ADouble - FMin));
        Canvas.TextOut(XPos + FTickSize, YPos - Canvas.TextHeight(Str)
          div 2, Str);
        ADouble := ADouble + FMajorTickUnit;
      end;
      YPos := Height - FThumbHalfSize;
      XPos := FTrackRect.Right + 4;
      if ShowTickMarks then
        Inc(XPos, 2);
      Str := Double2Str(FMin);
      Canvas.TextOut(XPos + FTickSize, YPos - Canvas.TextHeight(Str)
        div 2, Str);
    end;
  end;
end;

procedure TFXSlider.UpdateThumbData;
begin
  FThumbSize := FGUIDesigner.vilControls21616.Width;
  FThumbHalfSize := FThumbSize div 2;
end;

procedure TFXSlider.UpdateThumb;
var
  XPos, YPos: Integer;
begin
  if FOrientation = HORIZONTAL then
  begin
    XPos := FThumbHalfSize + Round(FValue / (FMax - FMin) * (Width - FThumbSize));
    YPos := (FTrackRect.Top + FTrackRect.Bottom) div 2;
  end
  else
  begin
    XPos := (FTrackRect.Left + FTrackRect.Right) div 2;
    YPos := FThumbHalfSize + Round((FMax - (FValue - FMin)) / (FMax - FMin) *
      (Height - FThumbSize));
  end;
  FGUIDesigner.vilControls21616.Draw(Canvas, XPos - FThumbHalfSize,
    YPos - FThumbHalfSize, 0);
end;

procedure TFXSlider.SetMax(Value: Double);
begin
  if Value <> FMax then
  begin
    FMax := Value;
    if FValue > FMax then
      FValue := FMax;
    Invalidate;
  end;
end;

procedure TFXSlider.SetMin(Value: Double);
begin
  if Value <> FMin then
  begin
    FMin := Value;
    if FValue < FMin then
      FValue := FMin;
    Invalidate;
  end;
end;

procedure TFXSlider.SetValue(Value: Double);
begin
  if Value <> FValue then
  begin
    FValue := Value;
    if FValue > FMax then
      FValue := FMax
    else if FValue < FMin then
      FValue := FMin;
    if csDesigning in ComponentState then
      Invalidate
    else
      UpdateThumb;
  end;
end;

procedure TFXSlider.SetOrientation(AValue: TOrientation);
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
      PrefWidth := Width;
      PrefHeight := Height;
    end;
    Invalidate;
  end;
end;

procedure TFXSlider.SetMinorTickCount(Value: Integer);
begin
  if Value <> FMinorTickCount then
  begin
    FMinorTickCount := Value;
    Invalidate;
  end;
end;

procedure TFXSlider.SetMajorTickUnit(Value: Double);
begin
  if (Value <> FMajorTickUnit) and (Value > 0) then
  begin
    FMajorTickUnit := Value;
    Invalidate;
  end;
end;

procedure TFXSlider.SetShowTickLabels(Value: Boolean);
begin
  if Value <> FShowTickLabels then
  begin
    FShowTickLabels := Value;
    Invalidate;
  end;
end;

procedure TFXSlider.SetShowTickMarks(Value: Boolean);
begin
  if Value <> FShowTickMarks then
  begin
    FShowTickMarks := Value;
    Invalidate;
  end;
end;

function TFXSlider.GetSliderCode: string;
begin
  Result := 'private Slider ' + Name + ' = new Slider(' + Double2Str(FMin) +
    ', ' + Double2Str(FMax) + ', ' + Double2Str(FValue) + ');';
end;

procedure TFXSlider.NewControl;
begin
  DefaultComponent;
  InsertNewVariable(GetSliderCode);
  MakeAttribut('ShowTickLabels', 'true');
  MakeAttribut('ShowTickMarks', 'true');
end;

procedure TFXSlider.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Min') or (Attr = 'Max') or (Attr = 'Value') then
    FPartner.ReplaceLine('private Slider ' + Name, Indent1 + GetSliderCode)
  else
    inherited;
end;

function TFXSlider.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Name|Min|Max|Value|ShowTickLabels|ShowTickMarks';
  Attributes2 =
    '|BlockIncrement|MajorTickUnit|MinorTickCount|Orientation|SnapToTicks';
begin
  if ShowAttributes = 1 then
    Result := Attributes1
  else
    Result := Attributes1 + Attributes2;
  Result := Result + inherited;
end;

end.
