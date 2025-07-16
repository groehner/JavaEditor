unit USynEditEx;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  SynEditHighlighter,
  SynEditTypes,
  SynEdit,
  VCLStyleSynEdit;

type
  TOnMouseOverToken = procedure(Sender: TObject; const Token: string;
    TokenType: Integer; Caret, Posi: TPoint; Attri: TSynHighlighterAttributes;
    var Highlight: Boolean) of object;
  TOnTokenClick = procedure(Sender: TObject; Point: TPoint; const Token: string;
    TokenType: Integer; Attri: TSynHighlighterAttributes) of object;

  THighlightTokenInfo = class
  private
    FToken: string;
    FStart: Integer;
    FActive: Boolean;
    FXY: TPoint;
    FAttri: TSynHighlighterAttributes;
    FTokenType: Integer;
    procedure SetAttributes(Value: TSynHighlighterAttributes);
  public
    constructor Create;
    destructor Destroy; override;
    property Attributes: TSynHighlighterAttributes read FAttri
      write SetAttributes;
    property Token: string read FToken write FToken;
    property TokenType: Integer read FTokenType write FTokenType;
    property Start: Integer read FStart write FStart;
    property Active: Boolean read FActive write FActive;
    property XY: TPoint read FXY write FXY;
  end;

  (*
    This could be optimized by making the structure a record instead of a
    full fledged class. I opted for the class because I was thinking I may
    add some additional features later on that would be more maintainable
    with the structure as a class element. Some optimization of class versus
    record was achieved by pooling the structure classes instead of creating
    new objects each time.

    TextRect is used to hold the start and end text positions where the structure
    line should be drawn. GraphicRect is where the position should be drawn in
    graphical coordinates. TextRect is only updated if a change is made to the text
    and is updated in BuildStructureEx. GraphicRect is updated on each paint operation
    since we always need correct graphical coordinates to determine how lines
    should be invalidated. For example, when InvalidateRect is called the invalidation
    rectangle may need to be increased so the entire structure line can be repainted.
  *)

  TErrors = class
  private
    FArray: array of TPoint;
    FCount: Integer;
    function GetError(Index: Integer): TPoint;
    procedure SetError(Index: Integer; Posi: TPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Value: TPoint);
    procedure Delete(Index: Integer);
    procedure Clear;
    property Count: Integer read FCount;
    property Error[Index: Integer]: TPoint read GetError
      write SetError; default;
  end;

  TErrorPlugin = class(TSynEditPlugin)
  protected
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  end;

  TSynEditEx = class(TSynEdit)
  private
    FErrors: TErrors;
    FErrorPlugin: TErrorPlugin;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FOnMouseOverToken: TOnMouseOverToken;
    FOnTokenClick: TOnTokenClick;
    FTokenInfo: THighlightTokenInfo;
    FTokenIsHighlighted: Boolean;
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoTokenCheck(Shift: TShiftState; X, Y: Integer); virtual;
    procedure HighlightToken(XYPoint: TPoint; const Str: string;
      Start, TokenType: Integer; Attributes: TSynHighlighterAttributes);
    procedure Click; override;
    procedure Paint; override;
    procedure InvalidateToken;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSearchText(const Str: string): string;
    function NeedsWordWrap: Boolean;
    procedure ReplaceTabs(TabWidth: Integer);
    procedure InitShowCompileErrors;
    procedure SetCompileError(Posi: TPoint);
    procedure UnderlineCompileErrors;
    procedure LinesInserted(FirstLine, Count: Integer);
    procedure LinesDeleted(FirstLine, Count: Integer);
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnMouseOverToken: TOnMouseOverToken read FOnMouseOverToken
      write FOnMouseOverToken;
    property OnTokenClick: TOnTokenClick read FOnTokenClick write FOnTokenClick;
    property Errors: TErrors read FErrors;
  end;

implementation

uses
  Types,
  UITypes,
  Math,
  SysUtils,
  Graphics,
  SynEditMiscProcs;

function RectIntersects(Rect1, Rect2: TRect): Boolean;
var
  Rect: TRect;
begin
  Result := IntersectRect(Rect, Rect1, Rect2) or UnionRect(Rect, Rect1, Rect2);
end;

function DisplayToPoint(DisplayCoord: TDisplayCoord): TPoint;
begin
  Result.X := DisplayCoord.Column;
  Result.Y := DisplayCoord.Row;
end;

function PointToBuffer(Posi: TPoint): TBufferCoord;
begin
  Result.Char := Posi.X;
  Result.Line := Posi.Y;
end;

function PointToDisplay(Posi: TPoint): TDisplayCoord;
begin
  Result.Column := Posi.X;
  Result.Row := Posi.Y;
end;

{ Error Plugin }

procedure TErrorPlugin.LinesInserted(FirstLine, Count: Integer);
begin
  (Editor as TSynEditEx).LinesInserted(FirstLine, Count);
end;

procedure TErrorPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
  (Editor as TSynEditEx).LinesDeleted(FirstLine, Count);
end;

{ TSynEditEx }
constructor TSynEditEx.Create(AOwner: TComponent);
begin
  inherited;
  FTokenInfo := THighlightTokenInfo.Create;
  FErrors := TErrors.Create;
  FErrorPlugin := TErrorPlugin.Create(Self);
end;

destructor TSynEditEx.Destroy;
begin
  FreeAndNil(FErrors);
  FreeAndNil(FErrorPlugin);
  inherited;
  FreeAndNil(FTokenInfo);
end;

procedure TSynEditEx.CMMouseEnter(var Msg: TMessage);
begin
  InvalidateToken;
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TSynEditEx.CMMouseLeave(var Msg: TMessage);
begin
  InvalidateToken;
  if Assigned(OnMouseExit) then
    OnMouseExit(Self);
end;

procedure TSynEditEx.DoTokenCheck(Shift: TShiftState; X, Y: Integer);
var
  Str: string;
  Start, TokenType: Integer;
  Attri: TSynHighlighterAttributes;
  Posi1, Posi2: TPoint;
  DoHighlight: Boolean;
begin
  DoHighlight := False;
  Start := 0;
  if { (not (ssCtrl in Shift)) or } not Assigned(FTokenInfo) or
    not Assigned(OnMouseOverToken) then
  begin
    InvalidateToken;
    Exit;
  end;
  Posi1 := DisplayToPoint(PixelsToRowColumn(X, Y));
  Posi1.Y := RowToLine(Posi1.Y);
  GetHighlighterAttriAtRowColEx(PointToBuffer(Posi1), Str, TokenType,
    Start, Attri);
  Posi2 := RowColumnToPixels(DisplayCoord(Start, LineToRow(Posi1.Y + 1)));
  OnMouseOverToken(Self, Str, TokenType, Posi1, ClientToScreen(Posi2), Attri,
    DoHighlight); // new position before next if
  if (Str <> '') and Assigned(Attri) then
  begin
    if DoHighlight and not FTokenIsHighlighted then
    begin
      HighlightToken(Posi1, Str, Start, TokenType, Attri);
      FTokenIsHighlighted := True;
    end
    else if not DoHighlight then
      FTokenIsHighlighted := False;
  end
  else
    FTokenIsHighlighted := False;
  if not DoHighlight then
    InvalidateToken;
end;

procedure TSynEditEx.HighlightToken(XYPoint: TPoint; const Str: string;
  Start, TokenType: Integer; Attributes: TSynHighlighterAttributes);
{var
  Point: TPoint;
  Rect: TRect;
  Int: Integer;}
begin
  // convert from vcl to Winapi.D2D1
  { if FTokenInfo.Active and ((FTokenInfo.Token<>Str) or (FTokenInfo.Start<>Start) or
    (FTokenInfo.XYPoint.Y<>XYPoint.Y)) then
    InvalidateToken;
    Canvas.Font:= Font;
    Canvas.Font.Color:= clBlue;
    Canvas.Font.Style:= Attributes.Style;
    if Attributes.Background = clNone
    then Canvas.Brush.Color:= Color
    else Canvas.Brush.Color:= Attributes.Background;
    Canvas.Font.Style:= Canvas.Font.Style+[fsUnderLine];
    Point.X:= Start;
    Point.Y:= XYPoint.Y;

    fTextDrawer:= TheTextDrawer.Create(Canvas.Font.Style, Canvas.Font);
    try
    fTextDrawer.BeginDrawing(Canvas.Handle);
    fTextDrawer.SetForeColor(Canvas.Font.Color);
    fTextDrawer.SetBackColor(Canvas.Brush.Color);

    //Painting one character at a time because if you paint the whole token
    //it doesn't seem to match up for certain fonts like Lucida. This could
    //probably be optimized by someone who understands SynEdit painting better
    //then I do
    for Int:= 1 to Length(Str) do begin
    Point:= RowColumnToPixels(DisplayCoord(Start+(Int-1),XYPoint.Y));
    Rect.TopLeft:=Point;
    Rect.Right  := Rect.Left + fTextDrawer.CharWidth;
    Rect.Bottom := Rect.Top + fTextDrawer.CharHeight;
    if (Rect.Left>Gutter.RealGutterWidth(CharWidth)) then
    fTextDrawer.ExtTextOut(Point.X, Point.Y, [tooOpaque] , Rect, @Str[Int], 1);
    end;
    fTextDrawer.EndDrawing;
    finally
    FreeAndNil(fTextDrawer);
    end;
    Canvas.Brush.Style:= bsSolid;
    FTokenInfo.XYPoint:= XYPoint;
    FTokenInfo.Token:= Str;
    FTokenInfo.Start:= Start;
    FTokenInfo.Attributes:= Attributes;
    FTokenInfo.TokenType:= TokenType;
    FTokenInfo.Active:= True;
    Cursor:= crHandPoint; }
end;

procedure TSynEditEx.InvalidateToken;
begin
  if FTokenInfo.Active then
  begin
    Cursor := crIBeam;
    FTokenInfo.Active := False;
    InvalidateLine(FTokenInfo.XY.Y);
  end;
end;

procedure TSynEditEx.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FTokenIsHighlighted := False;
  DoTokenCheck(Shift, X, Y);
end;

procedure TSynEditEx.KeyDown(var Key: Word; Shift: TShiftState);
var
  Posi: TPoint;
begin
  if (ssCtrl in Shift) then
  begin
    GetCursorPos(Posi);
    Posi := ScreenToClient(Posi);
    if (Posi.X > 0) and (Posi.Y > 0) and (Posi.X < Width) and (Posi.Y < Height)
    then
      DoTokenCheck(Shift, Posi.X, Posi.Y);
  end;
  inherited;
end;

procedure TSynEditEx.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not(ssCtrl in Shift) then
  begin
    FTokenIsHighlighted := False;
    InvalidateToken;
  end;
  inherited;
end;

procedure TSynEditEx.Click;
begin
  if FTokenInfo.Active and Assigned(OnTokenClick) then
  begin
    OnTokenClick(Self, FTokenInfo.XY, FTokenInfo.Token, FTokenInfo.TokenType,
      FTokenInfo.Attributes);
    InvalidateToken;
  end
  else
    inherited;
end;

procedure TSynEditEx.Paint;
begin
  inherited;
  UnderlineCompileErrors;
end;

function TSynEditEx.GetSearchText(const Str: string): string;
begin
  if SelAvail and (BlockBegin.Line = BlockEnd.Line) then
    Result := SelText
  else if WordAtCursor <> '' then
    Result := WordAtCursor
  else
    Result := Str;
end;

procedure TSynEditEx.ReplaceTabs(TabWidth: Integer);
begin
  if Pos(#9, Text) = 0 then
    Exit;
  Lines.BeginUpdate;
  for var I := 0 to Lines.Count - 1 do
    if Pos(#9, Lines[I]) > 0 then
      Lines[I] := ExpandTabs(Lines[I], TabWidth);
  Lines.EndUpdate;
end;

function TSynEditEx.NeedsWordWrap: Boolean;
var
  Int: Integer;
begin
  Result := False;
  for Int := 0 to Lines.Count - 1 do
    if Length(Lines[Int]) > 500 then
    begin
      Result := True;
      Break;
    end;
end;

procedure TSynEditEx.InitShowCompileErrors;
begin
  FErrors.Clear;
end;

procedure TSynEditEx.SetCompileError(Posi: TPoint);
begin
  FErrors.Add(Posi);
end;

procedure TSynEditEx.UnderlineCompileErrors;
var
  Rect: TRect;
  Int, XPos, XEnd, YPos: Integer;
  Posi1, Posi2: TPoint;
  Str: string;
begin
  if FErrors.Count = 0 then
    Exit;

  Canvas.Pen.Color := clRed;
  for Int := 0 to FErrors.Count - 1 do
  begin
    Posi1 := FErrors[Int];
    Str := Lines[Posi1.Y - 2];
    if Posi1.X = -1 then
    begin // unknown column, only line
      Posi1.X := 1;
      while (Posi1.X < Length(Str)) and IsWordBreakChar(Str[Posi1.X]) do
        Inc(Posi1.X);
    end
    else
    begin
      Posi1.X := Math.Max(Posi1.X - 3, 1);
      while (Posi1.X > 0) and (Posi1.X <= Length(Str)) and
        not IsWordBreakChar(Str[Posi1.X]) do
        Dec(Posi1.X);
      Inc(Posi1.X);
      while (Posi1.X < Length(Str)) and (Str[Posi1.X] = ' ') do
        Inc(Posi1.X);
    end;

    Posi2 := FErrors[Int];
    if Posi2.X = -1 then
    begin // unknown column, only line
      Posi2.X := Length(Str);
      while (Posi2.X > 0) and (Posi2.X <= Length(Str)) and
        IsWordBreakChar(Str[Posi2.X]) do
        Dec(Posi2.X);
      Inc(Posi2.X);
    end
    else
    begin
      Posi2.X := Math.Min(Posi2.X + 3, Length(Str));
      while (Posi2.X < Length(Str)) and not IsWordBreakChar(Str[Posi2.X]) do
        Inc(Posi2.X);
    end;

    Rect.BottomRight := RowColumnToPixels(PointToDisplay(Posi1));
    Rect.TopLeft := RowColumnToPixels(PointToDisplay(Posi2));
    // if Rect.Left <= Gutter.RealGutterWidth(CharWidth) then Continue;
    if RectIntersects(ClientRect, Rect) then
    begin
      XPos := Rect.BottomRight.X;
      YPos := Rect.BottomRight.Y - 1;
      XEnd := Rect.TopLeft.X;
      Canvas.MoveTo(XPos, YPos);
      while XPos < XEnd do
      begin
        Canvas.LineTo(XPos + 1, YPos + 1);
        XPos := XPos + 2;
        if XPos < XEnd then
          Canvas.LineTo(XPos + 1, YPos - 1);
        XPos := XPos + 2;
      end;
    end;
  end;
end;

procedure TSynEditEx.LinesInserted(FirstLine, Count: Integer);
var
  Posi: TPoint;
begin
  for var I := 0 to FErrors.Count - 1 do
  begin
    Posi := FErrors[I];
    if Posi.Y - 2 >= FirstLine - 1 then
    begin
      Posi.Y := Posi.Y + Count;
      FErrors[I] := Posi;
    end;
  end;
end;

procedure TSynEditEx.LinesDeleted(FirstLine, Count: Integer);
var
  Posi: TPoint;
begin
  for var I := 0 to FErrors.Count - 1 do
  begin
    Posi := FErrors[I];
    if Posi.Y - 1 > FirstLine + Count then
    begin
      Posi.Y := Posi.Y - Count;
      FErrors[I] := Posi;
    end
    else if Posi.Y - 1 > FirstLine then
      FErrors.Delete(I);
  end;
end;

{ THighlightTokenInfo }

constructor THighlightTokenInfo.Create;
begin
  FAttri := TSynHighlighterAttributes.Create('Holder', 'Holder');
end;

destructor THighlightTokenInfo.Destroy;
begin
  FreeAndNil(FAttri);
  inherited;
end;

procedure THighlightTokenInfo.SetAttributes(Value: TSynHighlighterAttributes);
begin
  FAttri.Assign(Value);
end;

{ TErrors }
constructor TErrors.Create;
begin
  FCount := 0;
  SetLength(FArray, 10);
end;

destructor TErrors.Destroy;
begin
  SetLength(FArray, 0);
  inherited;
end;

procedure TErrors.Add(Value: TPoint);
begin
  if Length(FArray) = FCount then
    SetLength(FArray, FCount + 10);
  FArray[FCount] := Value;
  Inc(FCount);
end;

procedure TErrors.Delete(Index: Integer);
var
  Int: Integer;
begin
  for Int := Index + 1 to FCount - 1 do
    FArray[Int - 1] := FArray[Int];
  Dec(FCount);
end;

function TErrors.GetError(Index: Integer): TPoint;
begin
  Result := FArray[Index];
end;

procedure TErrors.SetError(Index: Integer; Posi: TPoint);
begin
  FArray[Index] := Posi;
end;

procedure TErrors.Clear;
begin
  FCount := 0;
  SetLength(FArray, 10);
end;

end.
