unit UJProgressBar;

interface

uses
  Windows,
  Classes,
  Graphics,
  ComCtrls,
  UJComponents;

type

  TJProgressBar = class(TSwingComponent)
  private
    FOrientation: TOrientation;
    FValue: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FBorderColor: TColor;
    FTextColorNormal: TColor;
    FStringPainted: Boolean;
    FString: string;
    FBorderPainted: Boolean;
    FIndeterminate: Boolean;
    FRotatedFont: Integer;
    procedure SetValue(Value: Integer);
    procedure SetMinimum(Value: Integer);
    procedure SetMaximum(Value: Integer);
    procedure SetOrientation(AValue: TOrientation);
    procedure SetStringPainted(AValue: Boolean);
    procedure SetString(const AValue: string);
    procedure SetBorderPainted(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AProgressBar: TProgressBar);
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure TextRectRotated(ARect: TRect; XPos, YPos: Integer; const Str: string);
    function GetRotatedFont(Angle: Integer): Integer;
  published
    property Maximum: Integer read FMaximum write SetMaximum;
    property Minimum: Integer read FMinimum write SetMinimum;
    property Value: Integer read FValue write SetValue;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property ProgressString: string read FString write SetString;
    property StringPainted: Boolean read FStringPainted write SetStringPainted;
    property BorderPainted: Boolean read FBorderPainted write SetBorderPainted;
    property Indeterminate: Boolean read FIndeterminate write FIndeterminate;
  end;

implementation

uses
  SysUtils,
  Controls,
  UITypes;

constructor TJProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 16;
  Width := 80;
  Height := 24;
  FMinimum := 0;
  FMaximum := 100;
  FValue := 30;
  FOrientation := HORIZONTAL;
  FBorderColor := DarkShadow;
  FTextColorNormal := BlueColor;
  Foreground := SelectionBackground;
  Background := DefaultBackground;
  FStringPainted := False;
  FBorderPainted := True;
  FString := '0 %';
  JavaType := 'JProgressBar';
end;

constructor TJProgressBar.CreateFrom(AProgressBar: TProgressBar);
begin
  Create(AProgressBar.Owner);
  CreateFromJ(AProgressBar);
  Background := AProgressBar.Brush.Color;
  Maximum := AProgressBar.Max;
  Minimum := AProgressBar.Min;
  Value := AProgressBar.Position;
  if AProgressBar.Orientation = pbVertical then
    Orientation := VERTICAL
  else
    Orientation := HORIZONTAL;
end;

function TJProgressBar.GetAttributes(ShowAttributes: Integer): string;
const
  Bar1 = '|Maximum|Minimum|Value|Orientation';
  Bar2 = '|ProgressString|StringPainted|BorderPainted|Indeterminate';
begin
  if ShowAttributes = 1 then
    Result := Bar1
  else
    Result := Bar1 + Bar2;
  Result := Result + inherited;
end;

function TJProgressBar.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|stateChanged' + inherited GetEvents(ShowEvents);
end;

procedure TJProgressBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JProgressBar ' + Name + ' = new JProgressBar();');
  MakeAttribut('Value', '30');
end;

procedure TJProgressBar.Paint;
var
  Pos, TextWidth, TextHeight, XPos, YPos: Integer;
  Rect1, Rect2, Rect12: TRect;
  HFontOld: Integer;
  Str: string;
begin
  Str := IntToStr(Round((FValue - FMinimum) / (FMaximum - FMinimum) *
    100)) + '%';
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Pen.Color := FBorderColor;
  Canvas.Brush.Color := FTextColorNormal;
  // total rect
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  if FOrientation = HORIZONTAL then
  begin
    // left and right part
    if FBorderPainted then
    begin
      Pos := Round((Width - 3) * (FValue - FMinimum) / (FMaximum - FMinimum));
      Rect1 := Rect(2, 2, Pos + 2, Height - 1);
      Rect2 := Rect(Pos + 2, 2, Width - 1, Height - 1);
    end
    else
    begin
      Pos := Round(Width * (FValue - FMinimum) / (FMaximum - FMinimum));
      Rect1 := Rect(0, 0, Pos, Height);
      Rect2 := Rect(Pos, 0, Width, Height);
    end;
    Canvas.Brush.Color := Foreground;
    Canvas.FillRect(Rect1);
    Canvas.Brush.Color := Background;
    Canvas.FillRect(Rect2);

    // text
    if FStringPainted then
    begin
      TextWidth := Canvas.TextWidth(Str);
      TextHeight := Canvas.TextHeight(Str);
      if FBorderPainted then
      begin
        XPos := (Width - 3 - TextWidth) div 2 + 2;
        YPos := (Height - 2 - TextHeight) div 2 + 1;
        Rect12 := Rect(2, 2, Width - 1, Height - 1);
      end
      else
      begin
        XPos := (Width - TextWidth) div 2;
        YPos := (Height - TextHeight) div 2;
        Rect12 := Rect(0, 0, Width, Height);
      end;

      Canvas.Brush.Style := bsClear; // use transparent mode
      Canvas.Font.Color := FTextColorNormal;
      Canvas.TextRect(Rect12, XPos, YPos, Str); // show text total
      Canvas.Font.Color := Background;
      Canvas.TextRect(Rect1, XPos, YPos, Str); // show left part
    end;
  end
  else
  begin // orientation VERTICAL
    // upper and lower part
    if FBorderPainted then
    begin
      Pos := Round((Height - 3) * (1 - (FValue - FMinimum) /
        (FMaximum - FMinimum)));
      Rect1 := Rect(2, 2, Width - 1, Pos + 2);
      Rect2 := Rect(2, Pos + 2, Width - 1, Height - 1);
    end
    else
    begin
      Pos := Round(Height * (1 - (FValue - FMinimum) / (FMaximum - FMinimum)));
      Rect1 := Rect(0, 0, Width, Pos);
      Rect2 := Rect(0, Pos, Width, Height);
    end;
    Canvas.Brush.Color := Background;
    Canvas.FillRect(Rect1);
    Canvas.Brush.Color := Foreground;
    Canvas.FillRect(Rect2);

    // text
    if FStringPainted then
    begin
      FRotatedFont := GetRotatedFont(-90);
      HFontOld := SelectObject(Canvas.Handle, FRotatedFont);
      TextWidth := Canvas.TextWidth(Str);
      TextHeight := Canvas.TextHeight(Str);
      SelectObject(Canvas.Handle, HFontOld);
      if FBorderPainted then
      begin
        XPos := (Width - 3 - TextHeight) div 2 + 2;
        YPos := (Height - 2 - TextWidth) div 2 + 1;
        Rect12 := Rect(2, 2, Width - 1, Height - 1);
      end
      else
      begin
        XPos := (Width - TextHeight) div 2;
        YPos := (Height - TextWidth) div 2;
        Rect12 := Rect(0, 0, Width, Height);
      end;

      Canvas.Brush.Style := bsClear; // use transparent mode
      Canvas.Font.Color := Background;
      TextRectRotated(Rect12, XPos + TextHeight, YPos, Str);
      Canvas.Font.Color := FTextColorNormal;
      TextRectRotated(Rect1, XPos + TextHeight, YPos, Str);
      DeleteObject(FRotatedFont);
    end;
  end;
end;

procedure TJProgressBar.TextRectRotated(ARect: TRect; XPos, YPos: Integer;
  const Str: string);
var
  DC1: HDC;
  HFontOld: Integer;
  Options: LongInt;
begin
  if Length(Str) > 0 then
  begin
    DC1 := Canvas.Handle;
    Options := ETO_CLIPPED or Canvas.TextFlags;
    if Canvas.Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    SetBkMode(DC1, TRANSPARENT);
    HFontOld := SelectObject(DC1, FRotatedFont);
    ExtTextOut(DC1, XPos, YPos, Options, @ARect, PChar(Str), Length(Str), nil);
    SelectObject(DC1, HFontOld);
  end;
end;

function TJProgressBar.GetRotatedFont(Angle: Integer): Integer;
var
  Fontweight: Integer;
begin
  if fsBold in Canvas.Font.Style then
    Fontweight := FW_BOLD
  else
    Fontweight := FW_NORMAL;
  Result := CreateFont(Canvas.Font.Height, // height
    0, // width
    Angle * 10, // escapement
    0, // orientation
    Fontweight, Ord(fsItalic in Canvas.Font.Style),
    Ord(fsUnderline in Canvas.Font.Style),
    Ord(fsStrikeOut in Canvas.Font.Style), 1, // char set
    4, // output precision
    $10, // clip precision
    2, // quality
    4, // pitch and family
    PChar(Canvas.Font.Name));
end;

procedure TJProgressBar.SetValue(Value: Integer);
begin
  if Value <> FValue then
  begin
    FValue := Value;
    Paint;
  end;
end;

procedure TJProgressBar.SetMinimum(Value: Integer);
begin
  if Value > FMaximum then
    Exit;
  if FMinimum <> Value then
  begin
    FMinimum := Value;
    Paint;
  end;
end;

procedure TJProgressBar.SetMaximum(Value: Integer);
begin
  if Value < FMinimum then
    Exit;
  if FMaximum <> Value then
  begin
    FMaximum := Value;
    Paint;
  end;
end;

procedure TJProgressBar.SetOrientation(AValue: TOrientation);
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
    Invalidate;
  end;
end;

procedure TJProgressBar.SetStringPainted(AValue: Boolean);
begin
  if AValue <> FStringPainted then
  begin
    FStringPainted := AValue;
    Invalidate;
  end;
end;

procedure TJProgressBar.SetString(const AValue: string);
begin
  if AValue <> FString then
  begin
    FString := AValue;
    Invalidate;
  end;
end;

procedure TJProgressBar.SetBorderPainted(AValue: Boolean);
begin
  if AValue <> FBorderPainted then
  begin
    FBorderPainted := AValue;
    Invalidate;
  end;
end;

end.
