unit UFXLine;

interface

uses
  Classes,
  UFXShape;

type

  TFXLine = class(TFXShape)
  private
    FEndX: Double;
    FEndY: Double;
    FStartX: Double;
    FStartY: Double;
    FOldTop: Integer;
    FOldLeft: Integer;
    FOldBottom: Integer;
    FOldRight: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure SetPositionAndSize; override;
    procedure UpdateSource;
    procedure SavePosition;
    function Startposition: Integer;
  published
    property EndX: Double read FEndX write FEndX;
    property EndY: Double read FEndY write FEndY;
    property StartX: Double read FStartX write FStartX;
    property StartY: Double read FStartY write FStartY;
  end;

implementation

uses
  Math,
  SysUtils,
  UObjectInspector;

{ --- TFXLine ------------------------------------------------------------------ }

constructor TFXLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +167;
  FStartX := 0;
  FStartY := 0;
  Stroke := 0; // clBlack
  Fill := -16777201;
  FEndX := Width;
  FEndY := Height;
  JavaType := 'Line';
  SavePosition;
end;

procedure TFXLine.Paint;
var
  Wid, Hei: Integer;
begin
  DefaultPenBrush;
  if Height <= 1 then
    Hei := 0
  else
    Hei := Height;
  if Width <= 1 then
    Wid := 0
  else
    Wid := Width;
  if Startposition in [2, 4] then
  begin
    Canvas.MoveTo(0, Hei);
    Canvas.LineTo(Wid, 0);
  end
  else
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Wid, Hei);
  end;
end;

procedure TFXLine.NewControl;
begin
  inherited;
  DefaultComponent;
  InsertNewVariable('private Line ' + Name + ' = new Line();');
end;

function TFXLine.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes = '|StartX|StartY|EndX|EndY';
begin
  Result := Attributes + inherited GetAttributes(ShowAttributes);
end;

procedure TFXLine.UpdateSource;
var
  SStartX, SStartY, SEndX, SEndY: string;
begin
  SStartX := Format('%g', [StartX]);
  SStartY := Format('%g', [StartY]);
  SEndX := Format('%g', [EndX]);
  SEndY := Format('%g', [EndY]);
  ChangeAttributValue(Name + '.setStartX(', Name + '.setStartX(' +
    SStartX + ');');
  ChangeAttributValue(Name + '.setStartY(', Name + '.setStartY(' +
    SStartY + ');');
  ChangeAttributValue(Name + '.setEndX(', Name + '.setEndX(' + SEndX + ');');
  ChangeAttributValue(Name + '.setEndY(', Name + '.setEndY(' + SEndY + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXLine.SavePosition;
begin
  FOldTop := Top;
  FOldLeft := Left;
  FOldBottom := Top + Height;
  FOldRight := Left + Width;
end;

function TFXLine.Startposition: Integer;
// 1 4
// 2 3
begin
  if StartX <= EndX then
    if StartY <= EndY then
      Result := 1
    else
      Result := 2
  else if StartY <= EndY then
    Result := 4
  else
    Result := 3;
end;

procedure TFXLine.SetPositionAndSize;
begin
  if FOldBottom = Top then
    case Startposition of
      1, 4:
        StartY := Top + Height;
      2, 3:
        EndY := Top + Height;
    end
  else if FOldTop = Top + Height then
    case Startposition of
      1, 4:
        EndY := Top;
      2, 3:
        StartY := Top;
    end
  else if FOldRight = Left then
    case Startposition of
      1, 2:
        StartX := Left + Width;
      3, 4:
        EndX := Left + Width;
    end
  else if FOldLeft = Left + Width then
    case Startposition of
      1, 2:
        EndX := Left;
      3, 4:
        StartX := Left;
    end
  else
    case Startposition of
      1:
        begin
          StartX := Left;
          StartY := Top;
          EndX := Left + Width;
          EndY := Top + Height;
        end;
      2:
        begin
          StartX := Left;
          StartY := Top + Height;
          EndX := Left + Width;
          EndY := Top;
        end;
      3:
        begin
          StartX := Left + Width;
          StartY := Top + Height;
          EndX := Left;
          EndY := Top;
        end;
      4:
        begin
          StartX := Left + Width;
          StartY := Top;
          EndX := Left;
          EndY := Top + Height;
        end;
    end;
  UpdateSource;
  SavePosition;
  if Height = 0 then
    Height := 1;
  if Width = 0 then
    Width := 1;
  Invalidate;
end;

procedure TFXLine.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'StartX') or (Attr = 'StartY') or (Attr = 'EndX') or (Attr = 'EndY')
  then
  begin
    Width := Abs(Round(EndX - StartX));
    Height := Abs(Round(EndY - StartY));
    Left := Math.Min(Round(StartX), Round(EndX));
    Top := Math.Min(Round(StartY), Round(EndY));
    UpdateSource;
  end
  else
    inherited;
end;

end.
