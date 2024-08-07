unit USynEditEx;

interface

uses Classes, Windows, Messages, Controls, Graphics,
     SynEdit, SynEditHighlighter, VCLStyleSynEdit;

type
  TOnMouseOverToken = procedure(Sender: TObject; const Token: string;
                                TokenType: Integer; Caret, P: TPoint; Attri: TSynHighlighterAttributes;
                                var Highlight: Boolean) of object;
  TOnTokenClick = procedure(Sender: TObject; XY:TPoint; const Token: string; TokenType: Integer; Attri: TSynHighlighterAttributes) of object;

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
    property Attributes: TSynHighlighterAttributes read FAttri write SetAttributes;
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
    FCount: integer;
    function getError(Index: integer): TPoint;
    procedure setError(Index: integer; p: TPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Value: TPoint);
    procedure Delete(Index: integer);
    procedure Clear;
    property Count: Integer read FCount;
    property Error[Index: Integer]: TPoint read getError write setError; default;
end;

TErrorPlugin = class(TSynEditPlugin)
  protected
    procedure AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer); override;
    procedure LinesInserted(FirstLine, Count: Integer); override;
    procedure LinesDeleted(FirstLine, Count: Integer); override;
  public
    constructor Create(AOwner: TCustomSynEdit);
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
    procedure InvalidateToken;
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoTokenCheck(Shift: TShiftState; X,Y: Integer); virtual;
    procedure HighlightToken(XY: TPoint; const s: string; Start,TokenType: Integer; Attributes: TSynHighlighterAttributes);
    procedure Click; override;
    procedure Paint; override;
    procedure PaintTextLines(AClip: TRect; const aFirstRow, aLastRow, FirstCol, LastCol: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSearchText(const s: string): string;
    function NeedsWordWrap: boolean;
    procedure ReplaceTabs(Width: integer);
    procedure InitShowCompileErrors;
    procedure setCompileError(p: TPoint);
    procedure ShowCompileErrors;
    procedure LinesInserted(FirstLine, Count: Integer);
    procedure LinesDeleted(FirstLine, Count: Integer);
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnMouseOverToken: TOnMouseOverToken read FOnMouseOverToken write FOnMouseOverToken;
    property OnTokenClick: TOnTokenClick read FOnTokenClick write FOnTokenClick;
    property Errors: TErrors read FErrors;
end;


implementation

uses Types, UITypes, Math, SysUtils,
     SynEditTypes, SynTextDrawer, SynEditTextBuffer;

function RectIntersects(R1, R2: TRect): boolean;
  var R: TRect;
begin
  Result:= IntersectRect(R, R1, R2) or UnionRect(R, R1, R2);
end;

{ Röhner, changed because of USynEditEx }
function DisplayToPoint(D: TDisplayCoord): TPoint;
begin
  Result.X:= D.Column;
  Result.Y:= D.Row;
end;

function PointToBuffer(P: TPoint): TBufferCoord;
begin
  Result.Char:= P.X;
  Result.Line:= P.Y;
end;

function PointToDisplay(P: TPoint): TDisplayCoord;
begin
  Result.Column:= P.x;
  Result.Row:= P.Y;
end;

{ Error Plugin }

procedure TErrorPlugin.AfterPaint(ACanvas: TCanvas; const AClip: TRect;
      FirstLine, LastLine: Integer);
begin
end;

procedure TErrorPlugin.LinesInserted(FirstLine, Count: Integer);
begin
  (Editor as TSynEditEx).LinesInserted(FirstLine, Count);
end;

procedure TErrorPlugin.LinesDeleted(FirstLine, Count: Integer);
begin
  (Editor as TSynEditEx).LinesDeleted(FirstLine, Count);
end;

constructor TErrorPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited create(AOwner);
end;

{ TSynEditEx }
constructor TSynEditEx.Create(AOwner: TComponent);
begin
  inherited;
  FTokenInfo := THighlightTokenInfo.Create;
  FErrors:= TErrors.Create;
  FErrorPlugin:= TErrorPlugin.Create(Self);
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
  if Assigned(OnMouseEnter) then OnMouseEnter(Self);
end;

procedure TSynEditEx.CMMouseLeave(var Msg: TMessage);
begin
  InvalidateToken;
  if Assigned(OnMouseExit) then OnMouseExit(Self);
end;

procedure TSynEditEx.DoTokenCheck(Shift: TShiftState; X, Y: Integer);
var S: string;
    Start, TokenType: Integer;
    Attri: TSynHighlighterAttributes;
    P1, P2: TPoint;
    DoHighlight: Boolean;
begin
  DoHighlight:= false;
  Start:= 0;
  if {(not (ssCtrl in Shift)) or }(FTokenInfo=nil) or (not Assigned(OnMouseOverToken)) then
  begin
    InvalidateToken;
    exit;
  end;
  P1:= DisplayToPoint(PixelsToRowColumn(X,Y));
  GetHighlighterAttriAtRowColEx(PointToBuffer(P1), S, TokenType, Start, Attri);
  P2:= RowColumnToPixels(DisplayCoord(Start, P1.Y+1));
  OnMouseOverToken(Self, S, TokenType, P1, ClientToScreen(P2), Attri, DoHighlight); // new position before next if
  if (S <> '') and (Attri <> nil) then begin
    if DoHighlight and not fTokenIsHighlighted then begin
      HighlightToken(P1, S, Start, TokenType, Attri);
      fTokenIsHighlighted:= True;
      end
    else if not DoHighlight then
      fTokenIsHighlighted:= False;
    end
  else
    fTokenIsHighlighted:= False;
  if not DoHighlight then InvalidateToken;
end;

procedure TSynEditEx.HighlightToken(XY: TPoint; const s: string; Start,TokenType: Integer; Attributes: TSynHighlighterAttributes);
  var P: TPoint;
      fTextDrawer: TheTextDrawer;
      Rect: TRect;
      i: Integer;
begin
  if FTokenInfo.Active and ((FTokenInfo.Token<>S) or (FTokenInfo.Start<>Start) or (FTokenInfo.XY.Y<>XY.Y)) then
    InvalidateToken;
  Canvas.Font:= Font;
  Canvas.Font.Color:= clBlue;
  Canvas.Font.Style:= Attributes.Style;
  if Attributes.Background = clNone
    then Canvas.Brush.Color:= Color
    else Canvas.Brush.Color:= Attributes.Background;
  Canvas.Font.Style:= Canvas.Font.Style+[fsUnderLine];
  P.X:= Start;
  P.Y:= XY.Y;

  fTextDrawer:= TheTextDrawer.Create(Canvas.Font.Style, Canvas.Font);
  try
    fTextDrawer.BeginDrawing(Canvas.Handle);
    fTextDrawer.SetForeColor(Canvas.Font.Color);
    fTextDrawer.SetBackColor(Canvas.Brush.Color);

    //Painting one character at a time because if you paint the whole token
    //it doesn't seem to match up for certain fonts like Lucida. This could
    //probably be optimized by someone who understands SynEdit painting better
    //then I do
    for i:= 1 to length(S) do begin
      P:= RowColumnToPixels(DisplayCoord(Start+(i-1),XY.Y));
      Rect.TopLeft:=P;
      Rect.Right  := Rect.Left + fTextDrawer.CharWidth;
      Rect.Bottom := Rect.Top + fTextDrawer.CharHeight;
      if (Rect.Left>Gutter.RealGutterWidth(CharWidth)) then
        fTextDrawer.ExtTextOut(P.X, P.Y, [tooOpaque] {ETO_OPAQUE}, Rect, @S[i], 1);
    end;
    fTextDrawer.EndDrawing;
  finally
    FreeAndNil(fTextDrawer);
  end;
  Canvas.Brush.Style:= bsSolid;
  FTokenInfo.XY:= XY;
  FTokenInfo.Token:= S;
  FTokenInfo.Start:= Start;
  FTokenInfo.Attributes:= Attributes;
  FTokenInfo.TokenType:= TokenType;
  FTokenInfo.Active:= true;
  Cursor:= crHandPoint;
end;

procedure TSynEditEx.InvalidateToken;
begin
  if FTokenInfo.Active then begin
    Cursor:= crIBeam;
    FTokenInfo.Active:= false;
    InvalidateLine(FTokenInfo.XY.Y);
  end;
end;

procedure TSynEditEx.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fTokenIsHighlighted:= False;
  DoTokenCheck(Shift,X,Y);
end;

procedure TSynEditEx.KeyDown(var Key: Word; Shift: TShiftState);
  var P: TPoint;
begin
  if (ssCtrl in Shift) then begin
    GetCursorPos(P);
    P:= ScreenToClient(P);
    if (P.X > 0) and (P.Y > 0) and (P.X < Width) and (P.Y < Height) then
      DoTokenCheck(Shift,P.X,P.Y);
  end;
  inherited;
end;

procedure TSynEditEx.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not (ssCtrl in Shift) then begin
    fTokenIsHighlighted := False;
    InvalidateToken;
  end;
  inherited;
end;

procedure TSynEditEx.Click;
begin
  if FTokenInfo.Active and Assigned(OnTokenClick) then begin
    OnTokenClick(Self, FTokenInfo.XY, FTokenInfo.Token, FTokenInfo.TokenType, FTokenInfo.Attributes);
    InvalidateToken;
  end
  else inherited;
end;

procedure TSynEditEx.Paint;
begin
  inherited;
  ShowCompileErrors;
end;

procedure TSynEditEx.PaintTextLines(AClip: TRect; const aFirstRow, aLastRow,
      FirstCol, LastCol: integer);
begin
  inherited;
  ShowCompileErrors;
end;

function TSynEditEx.GetSearchText(const s: string): string;
begin
  if SelAvail and (BlockBegin.Line = BlockEnd.Line)
    then Result:= SelText
  else
    if WordAtCursor <> ''
      then Result:= WordAtCursor
      else Result:= s;
end;

procedure TSynEditEx.ReplaceTabs(Width: integer);
  var i, OldTabWidth: Integer; s: string;
begin
  if Pos(#9, Text) > 0 then begin
    OldTabWidth:= TabWidth;
    TabWidth:= Width;    // wegen ExpandedStrings
    Lines.BeginUpdate;
    for i:= 0 to Lines.Count - 1 do
      if Pos(#9, Lines[i]) > 0 then begin
        s:= TSynEditStringList(Lines).ExpandedStrings[i];
        Lines[i]:= StringReplace(s, #9, ' ', [rfReplaceAll]);
      end;
    TabWidth:= OldTabWidth;
    Lines.EndUpdate;
  end;
end;

function TSynEditEx.NeedsWordWrap: boolean;
  var i: Integer;
begin
  Result:= false;
  for i:= 0 to Lines.Count - 1 do
    if Length(Lines[i]) > 500 then begin
      Result:= true;
      break;
    end;
end;

procedure TSynEditEx.InitShowCompileErrors;
begin
  FErrors.Clear;
end;

procedure TSynEditEx.setCompileError(p: TPoint);
begin
  FErrors.Add(p);
end;

procedure TSynEditEx.ShowCompileErrors;
  var GP: TRect;
      i, x, xe, y: Integer;
      p1, p2: TPoint;
      s: string;
begin
  Canvas.Pen.Color:= clRed;
  for i:= 0 to fErrors.Count-1 do begin
    p1:= FErrors[i];
    s:= Lines[p1.y-2];
    if p1.X = -1 then begin  // unknown column, only line
      p1.X:= 1;
      while (p1.X < length(s)) and IsWordBreakChar(s[p1.X]) do
        inc(p1.X);
    end else begin
      p1.X:= Math.max(p1.X - 3, 1);
      while (p1.X > 0) and (p1.x <= length(s)) and not IsWordBreakChar(s[p1.X]) do
        dec(p1.X);
      inc(p1.x);
      while (p1.X < length(s)) and (s[p1.X] = ' ') do
        inc(p1.X);
    end;

    p2:= FErrors[i];
    if p2.x = -1 then begin // unknown column, only line
      p2.x:= length(s);
      while (p2.x > 0) and (p2.x <= length(s)) and IsWordBreakChar(s[p2.X]) do
        dec(p2.x);
      inc(p2.x);
    end else begin
      p2.x:= Math.min(p2.X + 3, length(s));
      while (p2.X < length(s)) and not IsWordBreakChar(s[p2.X]) do
        inc(p2.X);
    end;

    GP.BottomRight:= RowColumnToPixels(PointToDisplay(p1));
    GP.TopLeft:= RowColumnToPixels(PointToDisplay(p2));
    if GP.Left <= Gutter.RealGutterWidth(CharWidth) then Continue;
    if RectIntersects(ClientRect, GP) then begin
      x:= GP.BottomRight.X;
      y:= GP.BottomRight.Y-1;
      xe:= GP.TopLeft.X;
      Canvas.MoveTo(x, y);
      while x < xe do begin
        Canvas.LineTo(x+1, y+1);
        x:= x + 2;
        if x < xe then
          Canvas.LineTo(x+1, y-1);
        x:= x + 2;
      end;
    end;
  end;
end;

procedure TSynEditEx.LinesInserted(FirstLine, Count: Integer);
  var i: Integer; p: TPoint;
begin
  for i:= 0 to FErrors.Count - 1 do begin
    p:= FErrors[i];
    if p.y - 2 >= FirstLine - 1 then begin
      p.y := p.y + Count;
      FErrors[i]:= p;
    end;
  end;
end;

procedure TSynEditEx.LinesDeleted(FirstLine, Count: Integer);
  var i: Integer; p: TPoint;
begin
  for i:= 0 to FErrors.Count - 1 do begin
    p:= FErrors[i];
    if p.y - 1 > FirstLine + Count then begin
      p.y := p.y - Count;
      FErrors[i]:= p;
    end
    else if p.y - 1 > FirstLine then
      FErrors.delete(i);
  end;
end;

{ THighlightTokenInfo }

constructor THighlightTokenInfo.Create;
begin
  FAttri:= TSynHighlighterAttributes.Create('Holder', 'Holder');
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
  FCount:= 0;
  setLength(FArray, 10);
end;

destructor TErrors.destroy;
begin
  setLength(FArray, 0);
end;

procedure TErrors.Add(Value: TPoint);
begin
  if Length(FArray) = FCount then
    setLength(FArray, FCount + 10);
  FArray[FCount]:= Value;
  inc(FCount);
end;

procedure TErrors.Delete(Index: integer);
  var i: integer;
begin
  for i:= Index + 1 to FCount-1 do
    FArray[i-1]:= FArray[i];
  dec(FCount);
end;

function TErrors.getError(Index: Integer): TPoint;
begin
  Result:= FArray[Index];
end;

procedure TErrors.setError(Index: integer; p: TPoint);
begin
  FArray[Index]:= p;
end;

procedure TErrors.Clear;
begin
  FCount:= 0;
  setLength(FArray, 10);
end;

end.
