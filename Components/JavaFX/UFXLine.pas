unit UFXLine;

interface

uses Classes, UFXShape;

type

  TFXLine = class (TFXShape)
  private
    FEndX: double;
    FEndY: double;
    FStartX: double;
    FStartY: double;
    OldTop: integer;
    OldLeft: integer;
    OldBottom: integer;
    OldRight: integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
    procedure UpdateSource;
    procedure SavePosition;
    function Startposition: integer;
  published
    property EndX: double read FEndX write FEndX;
    property EndY: double read FEndY write FEndY;
    property StartX: double read FStartX write FStartX;
    property StartY: double read FStartY write FStartY;
  end;

implementation

uses Math, Graphics, SysUtils, UObjectInspector;

{--- TFXLine ------------------------------------------------------------------}

constructor TFXLine.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +167;
  FStartX:= 0;
  FStartY:= 0;
  Stroke:= 0; // clBlack;
  Fill:= -16777201;
  FEndX:= Width;
  FEndY:= Height;
  JavaType:= 'Line';
  SavePosition;
end;

procedure TFXLine.Paint;
  var w, h: integer;
begin
  DefaultPenBrush;
  if Height <= 1 then h:= 0 else h:= Height;
  if Width <= 1 then  w:= 0 else w:= Width;
  if Startposition in [2, 4] then begin
    Canvas.MoveTo(0, h);
    Canvas.LineTo(w, 0);
  end else begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(w, h);
  end;
end;

procedure TFXLine.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private Line ' + Name + ' = new Line();');
end;

function TFXLine.getAttributes(ShowAttributes: integer): string;
  const Attributes = '|StartX|StartY|EndX|EndY';
begin
  Result:= Attributes + inherited getAttributes(ShowAttributes);
end;

procedure TFXLine.UpdateSource;
  var sStartX, sStartY, sEndX, sEndY: string;
begin
  sStartX:= Format('%g',[StartX]);
  sStartY:= Format('%g',[StartY]);
  sEndX:= Format('%g',[EndX]);
  sEndY:= Format('%g',[EndY]);
  ChangeAttributValue(Name + '.setStartX(', Name + '.setStartX(' + sStartX + ');');
  ChangeAttributValue(Name + '.setStartY(', Name + '.setStartY(' + sStartY + ');');
  ChangeAttributValue(Name + '.setEndX(',   Name + '.setEndX(' + sEndX + ');');
  ChangeAttributValue(Name + '.setEndY(',   Name + '.setEndY(' + sEndY + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXLine.SavePosition;
begin
  OldTop:= Top;
  OldLeft:= Left;
  OldBottom:= Top + Height;
  OldRight:= Left + Width;
end;

function TFXLine.Startposition: integer;
  //   1 4
  //   2 3
begin
  if StartX <= EndX then
    if StartY <= EndY
      then Result:= 1
      else Result:= 2
  else
    if StartY <= EndY
      then Result:= 4
      else Result:= 3;
end;

procedure TFXLine.SetPositionAndSize;
begin
  if OldBottom = Top then
    case Startposition of
      1, 4: StartY:= Top + Height;
      2, 3: EndY:= Top + Height;
    end
  else if OldTop = Top + Height then
    case Startposition of
      1, 4: EndY:= Top;
      2, 3: StartY:= Top;
    end
  else if OldRight = Left then
    case Startposition of
      1, 2: StartX:= Left + Width;
      3, 4: EndX:= Left + Width;
    end
  else if OldLeft = Left + Width then
    case Startposition of
      1, 2: EndX:= Left;
      3, 4: StartX:= Left;
    end
  else
  case Startposition of
    1: begin
         StartX:= Left;
         StartY:= Top;
         EndX:= Left + Width;
         EndY:= Top + Height;
       end;
    2: begin
         StartX:= Left;
         StartY:= Top + Height;
         EndX:= Left + Width;
         EndY:= Top;
       end;
    3: begin
         StartX:= Left + Width;
         StartY:= Top + Height;
         EndX:= Left;
         EndY:= Top;
       end;
    4: begin
         StartX:= Left + Width;
         StartY:= Top;
         EndX:= Left;
         EndY:= Top + Height;
       end;
  end;
  UpdateSource;
  SavePosition;
  if Height = 0 then Height:= 1;
  if Width = 0  then Width:= 1;
  Invalidate;
end;

procedure TFXLine.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'StartX') or (Attr = 'StartY') or (Attr = 'EndX') or (Attr = 'EndY') then begin
    Width:= abs(Round(EndX-StartX));
    Height:= abs(Round(EndY-StartY));
    Left:= Math.min(Round(StartX), Round(EndX));
    Top:= Math.min(Round(StartY), Round(EndY));
    UpdateSource;
  end else
    inherited;
end;

end.
