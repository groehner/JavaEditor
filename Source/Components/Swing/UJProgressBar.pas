unit UJProgressBar;

interface

uses
  Windows, Classes, Graphics, ComCtrls, UJComponents;

type

  TJProgressBar = class(TSwingComponent)
  private
    FOrientation: TOrientation;
    FValue: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FBorderColor: TColor;
    FTextColorNormal: TColor;
    FStringPainted: boolean;
    FString: string;
    FBorderPainted: boolean;
    FIndeterminate: boolean;
    RotatedFont: integer;
    procedure SetValue(Value: Integer);
    procedure SetMinimum(Value: Integer);
    procedure SetMaximum(Value: Integer);
    procedure SetOrientation(aValue: TOrientation);
    procedure SetStringPainted(aValue: boolean);
    procedure SetString(const aValue: string);
    procedure setBorderPainted(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aProgressBar: TProgressBar);
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure TextRectRotated(R: TRect; x, y: integer; const s: string);
    function getRotatedFont(Angle: integer): integer;
  published
    property Maximum: Integer read FMaximum write SetMaximum;
    property Minimum: Integer read FMinimum write SetMinimum;
    property Value: Integer read FValue write setValue;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property ProgressString: string read FString write setString;
    property StringPainted: boolean read FStringPainted write setStringPainted;
    property BorderPainted: boolean read FBorderPainted write setBorderPainted;
    property Indeterminate: boolean read FIndeterminate write FIndeterminate;
  end;

implementation

uses SysUtils, Controls, UITypes;

constructor TJProgressBar.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= 16;
  Width:= 80;
  Height:= 24;
  FMinimum:= 0;
  FMaximum:= 100;
  FValue:= 30;
  FOrientation:= HORIZONTAL;
  FBorderColor:= DarkShadow;
  FTextColorNormal:= BlueColor;
  Foreground:= SelectionBackground;
  Background:= DefaultBackground;
  FStringPainted:= false;
  FBorderPainted:= true;
  FString:= '0 %';
  JavaType:= 'JProgressBar';
end;

constructor TJProgressBar.CreateFrom(aProgressBar: TProgressBar);
begin
  Create(aProgressBar.Owner);
  CreateFromJ(aProgressBar);
  Background:= aProgressBar.Brush.Color;
  Maximum:= aProgressBar.Max;
  Minimum:= aProgressBar.Min;
  Value:= aProgressBar.Position;
  if aProgressBar.Orientation = pbVertical
    then Orientation:= VERTICAL
    else Orientation:= HORIZONTAL;
end;

function TJProgressBar.getAttributes(ShowAttributes: integer): string;
  const
    bar1 = '|Maximum|Minimum|Value|Orientation';
    bar2 = '|ProgressString|StringPainted|BorderPainted|Indeterminate';
begin
  if ShowAttributes = 1
    then Result:= bar1
    else Result:= bar1 + bar2;
  Result:= Result + inherited;
end;

function TJProgressBar.getEvents(ShowEvents: integer): string;
begin
  Result:= '|stateChanged' + inherited getEvents(ShowEvents);
end;

procedure TJProgressBar.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JProgressBar ' + Name + ' = new JProgressBar();');
  MakeAttribut('Value', '30');
end;

procedure TJProgressBar.Paint;
  var Pos, tw, th, x, y: Integer;
      R1, R2, R12: TRect;
      HFontOld: integer;
      s: string;
begin
  s:= IntToStr(Round((FValue-FMinimum) / (FMaximum-FMinimum)*100)) + '%';
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Pen.Color:= FBorderColor;
  Canvas.Brush.Color:= FTextColorNormal;
  // total rect
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  if FOrientation = HORIZONTAL then begin
    // left and right part
    if FBorderPainted then begin
      Pos:= Round((Width - 3) * (FValue-FMinimum) / (FMaximum-FMinimum));
      R1:= Rect(2, 2, Pos+2, Height-1);
      R2:= Rect(Pos+2, 2, Width-1, Height-1);
    end else begin
      Pos:= Round(Width * (FValue-FMinimum) / (FMaximum-FMinimum));
      R1:= Rect(0, 0, Pos, Height);
      R2:= Rect(Pos, 0, Width, Height);
    end;
    Canvas.Brush.Color:= Foreground;
    Canvas.FillRect(R1);
    Canvas.Brush.Color:= Background;
    Canvas.FillRect(R2);

    // text
    if FStringPainted then begin
      tw:= Canvas.TextWidth(s);
      th:= Canvas.TextHeight(s);
      if FBorderPainted then begin
        x:= (Width-3-tw) div 2 + 2;
        y:= (Height-2-th) div 2 + 1;
        R12:= Rect(2, 2, Width-1, Height-1);
      end else begin
        x:= (Width-tw) div 2;
        y:= (Height-th) div 2;
        R12:= Rect(0, 0, Width, Height);
      end;

      Canvas.Brush.Style:= bsClear; // use transparent mode
      Canvas.Font.Color:= FTextColorNormal;
      Canvas.TextRect(R12, x, y, s);    // show text total
      Canvas.Font.Color := Background;
      Canvas.TextRect(R1, x, y, s);    // show left part
      end
    end
  else begin // orientation VERTICAL
    // upper and lower part
    if FBorderPainted then begin
      Pos:= Round((Height - 3) *(1 - (FValue-FMinimum) / (FMaximum-FMinimum)));
      R1:= Rect(2, 2, Width-1, Pos+2);
      R2:= Rect(2, Pos+2, Width-1, Height-1);
    end else begin
      Pos:= Round(Height * ( 1 - (FValue-FMinimum) / (FMaximum-FMinimum)));
      R1:= Rect(0, 0, Width, Pos);
      R2:= Rect(0, Pos, Width, Height);
    end;
    Canvas.Brush.Color:= Background;
    Canvas.FillRect(R1);
    Canvas.Brush.Color:= Foreground;
    Canvas.FillRect(R2);

    // text
    if FStringPainted then begin
      RotatedFont:= getRotatedFont(-90);
      HFontOld := SelectObject(Canvas.Handle, RotatedFont);
      tw:= Canvas.TextWidth(s);
      th:= Canvas.TextHeight(s);
      SelectObject(Canvas.Handle, HFontOld);
      if FBorderPainted then begin
        x:= (Width-3-th) div 2 + 2;
        y:= (Height-2-tw) div 2 + 1;
        R12:= Rect(2, 2, Width-1, Height-1);
      end else begin
        x:= (Width-th) div 2;
        y:= (Height-tw) div 2;
        R12:= Rect(0, 0, Width, Height);
      end;

      Canvas.Brush.Style:= bsClear; // use transparent mode
      Canvas.Font.Color:= Background;
      TextRectRotated(R12, x+th, y, s);
      Canvas.Font.Color := FTextColorNormal;
      TextRectRotated(R1, x+th, y, s);
      DeleteObject(RotatedFont);
      end
    end
end;

procedure TJProgressBar.TextRectRotated(R: TRect; x, y: integer; const s: string);
  var DC: HDC; HFontOld: integer; Options: Longint;
begin
  if Length(s) > 0 then begin
    DC:= Canvas.Handle;
    Options := ETO_CLIPPED or Canvas.TextFlags;
    if Canvas.Brush.Style <> bsClear then Options := Options or ETO_OPAQUE;
    SetBkMode(DC, Transparent);
    HFontOld := SelectObject(DC, RotatedFont);
    ExtTextOut(DC, x, y, Options, @R, PChar(s), Length(s), nil);
    SelectObject(DC, HFontOld);
  end;
end;

function TJProgressBar.getRotatedFont(Angle: integer): integer;
  var Fontweight: integer;
begin
  if fsBold in Canvas.Font.Style
    then FontWeight:= FW_BOLD
    else FontWeight:= FW_NORMAL;
  Result:= CreateFont(
    Canvas.Font.Height,  // height
    0,          // width
    Angle*10,   // escapement
    0,          // orientation
    FontWeight,
    Ord(fsItalic    in Canvas.Font.Style),
    Ord(fsUnderline in Canvas.Font.Style),
    Ord(fsStrikeout in Canvas.Font.Style),
    1,          // char set
    4,          // output precision
    $10,        // clip precision
    2,          // quality
    4,          // pitch and family
    PChar(Canvas.Font.Name));
end;

procedure TJProgressBar.setValue(Value: Integer);
begin
  if Value <> FValue then begin
    FValue:= Value;
    Paint;
  end;
end;

procedure TJProgressBar.SetMinimum(Value: Integer);
begin
  if Value > FMaximum then Exit;
  if FMinimum <> Value then begin
    FMinimum:= Value;
    Paint;
  end;
end;

procedure TJProgressBar.SetMaximum(Value: Integer);
begin
  if Value < FMinimum then Exit;
  if FMaximum <> Value then begin
    FMaximum:= Value;
    Paint;
  end;
end;

procedure TJProgressBar.SetOrientation(aValue: TOrientation);
  var h: integer;
begin
  if aValue <> FOrientation then  begin
    FOrientation:= aValue;
    if not (csLoading in ComponentState) then begin
      h:= Width; Width:= Height; Height:= h;
    end;
    Invalidate;
  end;
end;

procedure TJProgressBar.SetStringPainted(aValue: boolean);
begin
  if aValue <> FStringPainted then begin
    FStringPainted:= aValue;
    Invalidate;
  end;
end;

procedure TJProgressBar.SetString(const aValue: string);
begin
  if aValue <> FString then begin
    FString:= aValue;
    Invalidate;
  end;
end;

procedure TJProgressBar.SetBorderPainted(aValue: boolean);
begin
  if aValue <> FBorderPainted then begin
    FBorderPainted:= aValue;
    Invalidate;
  end;
end;

end.
