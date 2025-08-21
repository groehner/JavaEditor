unit UJSlider;

interface

uses
  Windows,
  Classes,
  Graphics,
  ComCtrls,
  UJComponents;

type

  TJSlider = class(TSwingComponent)
  private
    FMaximum, FMinimum, FValue: Integer;
    FOrientation: TOrientation;
    FTrackSize: Integer;
    FTrackColor: TColor;
    FThumbSize: Integer;
    FOffset: Integer;
    FTickSize: Integer;
    FMinorTickSpacing: Integer;
    FMajorTickSpacing: Integer;
    FPaintLabels: Boolean;
    FPaintTicks: Boolean;
    FPaintTrack: Boolean;
    FInverted: Boolean;
    FSnapToTicks: Boolean;

    FTrackRect: TRect;
    FUsablePixels: Integer;
    FCentering: Integer;
    FThumbHalfSize: Integer;
    FThumbRect: TRect;
    FTickColor: TColor;

    procedure SetMaximum(Value: Integer);
    procedure SetMinimum(Value: Integer);
    procedure SetValue(Value: Integer);
    procedure SetOrientation(AValue: TOrientation);
    procedure SetMinorTickSpacing(Value: Integer);
    procedure SetMajorTickSpacing(Value: Integer);
    procedure SetPaintLabels(Value: Boolean);
    procedure SetPaintTrack(Value: Boolean);
    procedure SetPaintTicks(Value: Boolean);
    procedure SetInverted(Value: Boolean);

  protected
    procedure UpdateTrackData;
    procedure UpdateThumbData;
    procedure UpdateTrack;
    procedure UpdateTicks;
    procedure UpdateThumb;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Maximum: Integer read FMaximum write SetMaximum default 100;
    property Minimum: Integer read FMinimum write SetMinimum default 0;
    property Value: Integer read FValue write SetValue default 50;
    property MinorTickSpacing: Integer read FMinorTickSpacing
      write SetMinorTickSpacing default 10;
    property MajorTickSpacing: Integer read FMajorTickSpacing
      write SetMajorTickSpacing default 50;
    property PaintLabels: Boolean read FPaintLabels write SetPaintLabels;
    property PaintTicks: Boolean read FPaintTicks write SetPaintTicks;
    property PaintTrack: Boolean read FPaintTrack write SetPaintTrack;
    property Inverted: Boolean read FInverted write SetInverted;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property SnapToTicks: Boolean read FSnapToTicks write FSnapToTicks;
  end;

implementation

uses
  Controls,
  SysUtils,
  UITypes,
  UGUIDesigner;

constructor TJSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 15;
  Width := 120;
  Height := 80;
  JavaType := 'JSlider';
  FMaximum := 100;
  FMinimum := 0;
  FValue := 50;
  FOrientation := HORIZONTAL;
  FTrackSize := 6;
  FTrackColor := clWhite;
  FThumbSize := 20;
  FTickSize := 6;
  FMinorTickSpacing := 10;
  FMajorTickSpacing := 50;
  PaintLabels := True;
  FOffset := 6;

  FTickColor := SelectionBackground;
  Foreground := FTickColor;
  Background := DefaultBackground; // BackgroundColor
  FTrackColor := DefaultBackground; // also BackgroundColor
  PaintTicks := True;
  PaintTrack := True;
  PaintLabels := True;

  UpdateThumbData;
  UpdateTrackData;
end;

function TJSlider.GetAttributes(ShowAttributes: Integer): string;
const
  Label1 = '|Maximum|Minimum|Value|MinorTickSpacing|MajorTickSpacing|Orientation';
  Label2 = '|PaintLabels|PaintTicks|PaintTrack|Inverted|SnapToTicks';
begin
  if ShowAttributes = 1 then
    Result := Label1
  else
    Result := Label1 + Label2;
  Result := Result + inherited;
end;

function TJSlider.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|stateChanged' + inherited GetEvents(ShowEvents);
end;

procedure TJSlider.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JSlider ' + Name + ' = new JSlider();');
  MakeAttribut('MinorTickSpacing', '10');
  MakeAttribut('MajorTickSpacing', '50');
  MakeAttribut('PaintTicks', 'true');
  MakeAttribut('PaintLabels', 'true');
end;

procedure TJSlider.Paint;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  UpdateTrackData;
  UpdateTrack;
  UpdateTicks;
  UpdateThumb;
end;

procedure TJSlider.UpdateTrackData;
begin
  if FOrientation = HORIZONTAL then
  begin
    FUsablePixels := Width - (FThumbSize + 2 * FOffset);
    FTrackRect.Left := (FUsablePixels * FMinimum) div (FMaximum - FMinimum) +
      (FThumbHalfSize + FOffset);
    FTrackRect.Right := (FUsablePixels * FMaximum) div (FMaximum - FMinimum) +
      (FThumbHalfSize + FOffset);
    FCentering := (Width - (FTrackRect.Right - FTrackRect.Left)) div 2;
    FCentering := FCentering - FTrackRect.Left;
    FTrackRect.Left := FTrackRect.Left + FCentering;
    FTrackRect.Right := FTrackRect.Right + FCentering;
    FTrackRect.Top := Height div 2 - PPIScale(12 + FTrackSize div 2);
    FTrackRect.Bottom := FTrackRect.Top + FTrackSize;
  end
  else
  begin
    FUsablePixels := Height - (FThumbSize + 2 * FOffset);
    FTrackRect.Top := (FUsablePixels * FMinimum) div (FMaximum - FMinimum) +
      (FThumbHalfSize + FOffset);
    FTrackRect.Bottom := (FUsablePixels * FMaximum) div (FMaximum - FMinimum) +
      (FThumbHalfSize + FOffset);
    FCentering := (Height - (FTrackRect.Bottom - FTrackRect.Top)) div 2;
    FCentering := FCentering - FTrackRect.Top;
    FTrackRect.Top := FTrackRect.Top + FCentering;
    FTrackRect.Bottom := FTrackRect.Bottom + FCentering;
    FTrackRect.Left := Width div 2 - PPIScale(12 + FTrackSize div 2);
    FTrackRect.Right := FTrackRect.Left + FTrackSize;
  end;
end;

procedure TJSlider.UpdateThumbData;
begin
  FThumbHalfSize := PPIScale(8);
end;

procedure TJSlider.UpdateTrack;
begin
  if not FPaintTrack then
    Exit;

  Canvas.Pen.Color := BlueColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FTrackColor;

  Canvas.FillRect(FTrackRect);
  with FTrackRect do
  begin
    Canvas.MoveTo(Left, Bottom);
    Canvas.LineTo(Left, Top);
    Canvas.LineTo(Right, Top);
    Canvas.LineTo(Right, Bottom);
    Canvas.LineTo(Left, Bottom);
  end;
end;

procedure TJSlider.UpdateTicks;
var
  XPos, YPos, Int: Integer;
  Str: string;
begin
  if Foreground = FTickColor then
    Canvas.Pen.Color := FTickColor
  else
  begin
    Canvas.Pen.Color := Foreground;
    Canvas.Font.Color := Foreground;
  end;
  Canvas.Brush.Style := bsClear;
  if FOrientation = HORIZONTAL then
  begin
    Int := FMinimum;
    if FPaintTicks then
      while Int <= FMaximum do
      begin
        XPos := (FUsablePixels * Int) div (FMaximum - FMinimum) +
          (FThumbHalfSize + FOffset) + FCentering;
        YPos := Height div 2 + 6 - FTickSize;
        Canvas.MoveTo(XPos, YPos);
        if (MajorTickSpacing <> 0) and (Int mod MajorTickSpacing = 0) then
          Canvas.LineTo(XPos, YPos + FTickSize + 2)
        else
          Canvas.LineTo(XPos, YPos + FTickSize - 1);
        Int := Int + FMinorTickSpacing;
      end;
    Int := FMinimum;
    if FPaintLabels then
      while Int <= FMaximum do
      begin
        XPos := (FUsablePixels * Int) div (FMaximum - FMinimum) +
          (FThumbHalfSize + FOffset) + FCentering;
        YPos := Height div 2 - 6 - FTickSize;
        if FPaintTicks then
          Inc(YPos, 12);
        if FInverted then
          Str := IntToStr(FMaximum - (Int - FMinimum))
        else
          Str := IntToStr(Int);
        Canvas.TextOut(XPos - Canvas.TextWidth(Str) div 2,
          YPos + FTickSize + 6, Str);
        Int := Int + FMajorTickSpacing;
      end;
  end
  else
  begin
    Int := FMinimum;
    if FPaintTicks then
      while Int <= FMaximum do
      begin
        YPos := (FUsablePixels * Int) div (FMaximum - FMinimum) +
          (FThumbHalfSize + FOffset) + FCentering;
        XPos := Width div 2 + 6 - FTickSize;
        Canvas.MoveTo(XPos, YPos);
        if (MajorTickSpacing <> 0) and (Int mod FMajorTickSpacing = 0) then
          Canvas.LineTo(XPos + FTickSize + 2, YPos)
        else
          Canvas.LineTo(XPos + FTickSize - 1, YPos);
        Int := Int + FMinorTickSpacing;
      end;
    Int := FMinimum;
    if FPaintLabels then
      while Int <= FMaximum do
      begin
        YPos := (FUsablePixels * Int) div (FMaximum - FMinimum) +
          (FThumbHalfSize + FOffset) + FCentering;
        XPos := Width div 2 - 6 - FTickSize;
        if FPaintTicks then
          Inc(XPos, 12);
        if FInverted then
          Str := IntToStr(Int)
        else
          Str := IntToStr(FMaximum - (Int - FMinimum));
        Canvas.TextOut(XPos + FTickSize + 4, YPos - Canvas.TextHeight(Str)
          div 2, Str);
        Int := Int + FMajorTickSpacing;
      end;
  end;
end;

procedure TJSlider.UpdateThumb;
const
  WH1616 = 16;
var
  XPos, YPos, Num: Integer;
begin
  if FOrientation = HORIZONTAL then
  begin
    if FInverted then
      XPos := (FUsablePixels * (FMaximum - (FValue - FMinimum)))
        div (FMaximum - FMinimum) + FCentering + FOffset + 1
    else
      XPos := (FUsablePixels * FValue) div (FMaximum - FMinimum) + FCentering +
        FOffset + 1;
    YPos := (Height div 2) - PPIScale(12) - PPIScale(WH1616 div 2);
  end
  else
  begin
    XPos := (Width div 2) - PPIScale(12) - PPIScale(WH1616 div 2);
    if FInverted then
      YPos := (FUsablePixels * FValue) div (FMaximum - FMinimum) + FCentering +
        FOffset + 1
    else
      YPos := (FUsablePixels * (FMaximum - (FValue - FMinimum)))
        div (FMaximum - FMinimum) + FCentering + FOffset + 1;
  end;
  if FOrientation = HORIZONTAL then
    Num := 0
  else
    Num := 1;
  FGUIDesigner.vilControls1616.Draw(Canvas, XPos, YPos, Num);
  FThumbRect.Left := XPos;
  FThumbRect.Top := YPos;
  FThumbRect.Right := XPos + WH1616;
  FThumbRect.Bottom := YPos + WH1616;
end;

procedure TJSlider.SetMaximum(Value: Integer);
begin
  if Value <> FMaximum then
  begin
    FMaximum := Value;
    if FValue > FMaximum then
      FValue := FMaximum;
    Invalidate;
  end;
end;

procedure TJSlider.SetMinimum(Value: Integer);
begin
  if Value <> FMinimum then
  begin
    FMinimum := Value;
    if FValue < FMinimum then
      FValue := FMinimum;
    Invalidate;
  end;
end;

procedure TJSlider.SetValue(Value: Integer);
begin
  if Value <> FValue then
  begin
    FValue := Value;
    if FValue > FMaximum then
      FValue := FMaximum
    else if FValue < FMinimum then
      FValue := FMinimum;
    if csDesigning in ComponentState then
      Invalidate
    else
      UpdateThumb;
  end;
end;

procedure TJSlider.SetOrientation(AValue: TOrientation);
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
    end;
    UpdateThumbData;
    UpdateTrackData;
    Invalidate;
  end;
end;

procedure TJSlider.SetMinorTickSpacing(Value: Integer);
begin
  if Value <> FMinorTickSpacing then
  begin
    FMinorTickSpacing := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetMajorTickSpacing(Value: Integer);
begin
  if (Value <> FMajorTickSpacing) and (Value > 0) then
  begin
    FMajorTickSpacing := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetPaintLabels(Value: Boolean);
begin
  if Value <> FPaintLabels then
  begin
    FPaintLabels := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetPaintTrack(Value: Boolean);
begin
  if Value <> FPaintTrack then
  begin
    FPaintTrack := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetInverted(Value: Boolean);
begin
  if Value <> FInverted then
  begin
    FInverted := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetPaintTicks(Value: Boolean);
begin
  if Value <> FPaintTicks then
  begin
    FPaintTicks := Value;
    Invalidate;
  end;
end;

end.
