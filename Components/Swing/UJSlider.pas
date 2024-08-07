unit UJSlider;

interface

uses
  Windows, Classes, Graphics, ComCtrls, UJComponents;

type

  TJSlider = class(TSwingComponent)
  private
    FMaximum,
    FMinimum,
    FValue:Integer;
    FOrientation: TOrientation;
    FTrackSize:Integer;
    FTrackColor:TColor;
    FThumbSize:Integer;
    FOffset: integer;
    FTickSize:Integer;
    FMinorTickSpacing:Integer;
    FMajorTickSpacing:Integer;
    FPaintLabels: Boolean;
    FPaintTicks: Boolean;
    FPaintTrack: Boolean;
    FInverted: Boolean;
    FSnapToTicks: boolean;

    FTrackRect:TRect;
    FUsablePixels:Integer;
    FCentering:Integer;
    FThumbHalfSize:Integer;
    FThumbRect:TRect;
    FTickColor: TColor;

    procedure SetMaximum(Value:Integer);
    procedure SetMinimum(Value:Integer);
    procedure SetValue(Value:Integer);
    procedure SetOrientation(aValue: TOrientation);
    procedure SetMinorTickSpacing(Value:Integer);
    procedure SetMajorTickSpacing(Value:Integer);
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
    constructor CreateFrom(aTrackBar: TTrackBar);
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Maximum:Integer read FMaximum write setMaximum default 100;
    property Minimum:Integer read FMinimum write SetMinimum default 0;
    property Value:Integer read FValue write setValue default 50;
    property MinorTickSpacing:Integer read FMinorTickSpacing write SetMinorTickSpacing default 10;
    property MajorTickSpacing:Integer read FMajorTickSpacing write SetMajorTickSpacing default 50;
    property PaintLabels: boolean read FPaintLabels write setPaintLabels;
    property PaintTicks: boolean read FPaintTicks write setPaintTicks;
    property PaintTrack: boolean read FPaintTrack write setPaintTrack;
    property Inverted: boolean read FInverted write setInverted;
    property Orientation: TOrientation read FOrientation write setOrientation;
    property SnapToTicks: boolean read FSnapToTicks write FSnapToTicks;
  end;

implementation

uses Controls, SysUtils, UITypes, UGUIDesigner;

constructor TJSlider.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Tag:= 15;
  Width := 120;
  Height := 80;
  JavaType:= 'JSlider';
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
  PaintLabels:= true;
  FOffset:= 6;

  FTickColor:= SelectionBackground;
  Foreground:= FTickColor;
  Background:= DefaultBackground;  // BackgroundColor
  FTrackColor:= DefaultBackground; // also BackgroundColor
  PaintTicks:= true;
  PaintTrack:= true;
  PaintLabels:= true;

  UpdateThumbData;
  UpdateTrackData;
end;

constructor TJSlider.CreateFrom(aTrackBar: TTrackBar);
begin
  Create(aTrackBar.Owner);
  CreateFromJ(aTrackBar);
  Color:= aTrackBar.Brush.Color;
  Minimum:= aTrackBar.Min;
  Maximum:= aTrackBar.Max;
  Value:= aTrackBar.Position;
  MajorTickSpacing:= aTrackBar.LineSize;
  MinorTickSpacing:= aTrackBar.Frequency;
  if aTrackBar.Orientation = trHorizontal
    then Orientation:= HORIZONTAL
    else Orientation:= VERTICAL;
  PaintTicks:= aTrackBar.ShowHint;
  PaintLabels:= aTrackBar.Ctl3D;
end;

function TJSlider.getAttributes(ShowAttributes: integer): string;
  const
    Label1 = '|Maximum|Minimum|Value|MinorTickSpacing|MajorTickSpacing|Orientation';
    Label2 = '|PaintLabels|PaintTicks|PaintTrack|Inverted|SnapToTicks';
begin
  if ShowAttributes = 1
    then Result:= Label1
    else Result:= Label1 + Label2;
  Result:= Result + inherited;
end;

function TJSlider.getEvents(ShowEvents: integer): string;
begin
  Result:= '|stateChanged' + inherited getEvents(ShowEvents);
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
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  UpdateTrackData;  
  UpdateTrack;
  UpdateTicks;
  UpdateThumb;
end;

procedure TJSlider.UpdateTrackData;
begin
  if FOrientation = HORIZONTAL then begin
    FUsablePixels := Width-(FThumbSize+2*FOffset);
    FTrackRect.Left := (FUsablePixels*FMinimum) div (FMaximum-FMinimum)+(FThumbHalfSize+FOffset);
    FTrackRect.Right := (FUsablePixels*FMaximum) div (FMaximum-FMinimum)+(FThumbHalfSize+FOffset);
    FCentering := (Width-(FTrackRect.Right-FTrackRect.Left)) div 2;
    FCentering:= FCentering - FTrackRect.Left;
    FTrackRect.Left := FTrackRect.Left+FCentering;
    FTrackRect.Right := FTrackRect.Right+FCentering;
    FTrackRect.Top := Height div 2 - PPIScale(12 + FTrackSize div 2);
    FTrackRect.Bottom := FTrackRect.Top + FTrackSize;
  end else begin
    FUsablePixels := Height-(FThumbSize+2*FOffset);
    FTrackRect.Top := (FUsablePixels*FMinimum) div (FMaximum-FMinimum)+(FThumbHalfSize+FOffset);
    FTrackRect.Bottom := (FUsablePixels*FMaximum) div (FMaximum-FMinimum)+(FThumbHalfSize+FOffset);
    FCentering := (Height-(FTrackRect.Bottom-FTrackRect.Top)) div 2;
    FCentering := FCentering - FTrackRect.Top;
    FTrackRect.Top := FTrackRect.Top+FCentering;
    FTrackRect.Bottom := FTrackRect.Bottom+FCentering;
    FTrackRect.Left := Width div 2 - PPIScale(12 + FTrackSize div 2);
    FTrackRect.Right := FTrackRect.Left+FTrackSize;
  end;
end;

procedure TJSlider.UpdateThumbData;
begin
  FThumbHalfSize := PPIScale(8);
end;

procedure TJSlider.UpdateTrack;
begin
  if not FPaintTrack then exit;

  Canvas.Pen.Color := BlueColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FTrackColor;

  Canvas.FillRect(FTrackRect);
  with FTrackRect do begin
    Canvas.MoveTo(Left,Bottom);
    Canvas.LineTo(Left,Top);
    Canvas.LineTo(Right,Top);
    Canvas.LineTo(Right,Bottom);
    Canvas.LineTo(Left,Bottom);
  end;
end;

procedure TJSlider.UpdateTicks;
  var X,Y,I:Integer; s: string;
begin
  if Foreground = FTickColor then
    Canvas.Pen.Color := FTickColor
  else begin
    Canvas.Pen.Color := Foreground;
    Canvas.Font.Color:= Foreground;
  end;
  Canvas.Brush.Style:= bsClear;  
  if FOrientation = HORIZONTAL then
  begin
    I := FMinimum;
    if FPaintTicks then
      while I<=FMaximum do begin
        X := (FUsablePixels*I) div (FMaximum-FMinimum) + (FThumbHalfSize+FOffset) + FCentering;
        Y := Height div 2 + 6 - FTickSize;
        Canvas.MoveTo(X,Y);
        if (MajorTickSpacing <> 0) and (i mod MajorTickSpacing = 0)
          then Canvas.LineTo(X,Y+FTickSize+2)
          else Canvas.LineTo(X,Y+FTickSize-1);
        I := I+FMinorTickSpacing;
      end;
    I:= FMinimum;
    if FPaintLabels then
      while i <= FMaximum do begin
        X := (FUsablePixels*I) div (FMaximum-FMinimum) + (FThumbHalfSize+FOffset) + FCentering;
        Y:= Height div 2 - 6 - FTickSize;
        if FPaintTicks then inc(y, 12);
        if FInverted
          then s:= IntToStr(FMaximum-(i-FMinimum))
          else s:= IntTostr(i);
        Canvas.TextOut(X-Canvas.TextWidth(s)div 2, Y+FTickSize+6, s);
        i:= i + FMajorTickSpacing;
      end;
  end
  else
  begin
    I := FMinimum;
    if FPaintTicks then
      while I<=FMaximum do  begin
        Y := (FUsablePixels*(I)) div (FMaximum-FMinimum) + (FThumbHalfSize+FOffset) + FCentering;
        X := Width div 2 + 6 -FTickSize;
        Canvas.MoveTo(X,Y);
        if (MajorTickSpacing <> 0) and (i mod FMajorTickSpacing = 0)
          then Canvas.LineTo(X+FTickSize+2,Y)
          else Canvas.LineTo(X+FTickSize-1,Y);
        I := I+FMinorTickSpacing;
      end;
    i:= FMinimum;
    if FPaintLabels then
      while i <= FMaximum do begin
        Y := (FUsablePixels*(I)) div (FMaximum-FMinimum) + (FThumbHalfSize+FOffset) + FCentering;
        X := Width div 2 - 6 - FTickSize;
        if FPaintTicks then inc(X, 12);
        if FInverted
          then s:= IntTostr(i)
          else s:= IntToStr(FMaximum-(i-FMinimum));
        Canvas.TextOut(X+FTickSize+4, Y-Canvas.TextHeight(s) div 2, s);
        i:= i + FMajorTickSpacing;
      end;
  end;
end;

procedure TJSlider.UpdateThumb;
  const WH1616 = 16;
  var X, Y, Nr: integer;
begin
  if FOrientation = HORIZONTAL then begin
    if FInverted
      then X := (FUsablePixels*(FMaximum-(FValue-FMinimum))) div (FMaximum-FMinimum)+FCentering+FOffset+1
      else X := (FUsablePixels*FValue) div (FMaximum-FMinimum)+FCentering+FOffset+1;
    Y := (Height div 2) - PPIScale(12) - PPIScale(WH1616 div 2);
  end else begin
    X := (Width div 2) - PPIScale(12) - PPIScale(WH1616 div 2);
    if FInverted
      then Y := (FUsablePixels*FValue) div (FMaximum-FMinimum)+FCentering+FOffset +1
      else Y := (FUsablePixels*(FMaximum-(FValue-FMinimum))) div (FMaximum-FMinimum)+FCentering+FOffset+1;
  end;
  if FOrientation = HORIZONTAL
    then Nr:= 0
    else Nr:= 1;
  FGUIDesigner.vilControls1616.Draw(Canvas, X, Y, Nr);
  FThumbRect.Left := X;
  FThumbRect.Top := Y;
  FThumbRect.Right := X +  WH1616;
  FThumbRect.Bottom := Y + WH1616;
end;

procedure TJSlider.setMaximum(Value:Integer);
begin
  if Value <> FMaximum then begin
    FMaximum := Value;
    if FValue > FMaximum then FValue := FMaximum;
    Invalidate;
  end;
end;

procedure TJSlider.setMinimum(Value:Integer);
begin
  if Value <> FMinimum then begin
    FMinimum := Value;
    if FValue < FMinimum then FValue := FMinimum;
    Invalidate;
  end;
end;

procedure TJSlider.setValue(Value:Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    if FValue > FMaximum then FValue := FMaximum
    else if FValue < FMinimum then FValue := FMinimum;
    if csDesigning in ComponentState
      then Invalidate
      else UpdateThumb;
  end;
end;

procedure TJSlider.setOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
    end;
    UpdateThumbData;
    UpdateTrackData;
    Invalidate;
  end;
end;

procedure TJSlider.SetMinorTickSpacing(Value:Integer);
begin
  if Value <> FMinorTickSpacing then begin
    FMinorTickSpacing := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetMajorTickSpacing(Value:Integer);
begin
  if (Value <> FMajorTickSpacing) and (Value > 0) then begin
    FMajorTickSpacing := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetPaintLabels(Value: Boolean);
begin
  if Value <> FPaintLabels then begin
    FPaintLabels := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetPaintTrack(Value: Boolean);
begin
  if Value <> FPaintTrack then begin
    FPaintTrack := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetInverted(Value: Boolean);
begin
  if Value <> FInverted then begin
    FInverted := Value;
    Invalidate;
  end;
end;

procedure TJSlider.SetPaintTicks(Value: Boolean);
begin
  if Value <> FPaintTicks then begin
    FPaintTicks := Value;
    Invalidate;
  end;
end;

end.
