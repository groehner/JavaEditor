unit UJUtilities;

{ Classes
  TJPlayground = class(TSwingComponent)
  TJTurtle = class (TSwingComponent)
  TTimer = class (TSwingComponent)
}

interface

uses
  Classes,
  Graphics,
  UJComponents;

type

  TJPlayground = class(TSwingComponent)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TJTurtle = class(TSwingComponent)
  private
    FAngle: Integer;
    FClip: Boolean;
    FPenUp: Boolean;
    FSpeed: Integer;
    FPenColor: TColor;
    procedure MakeAttribute(const Attr, Value, Typ: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure SetPositionAndSize; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Angle: Integer read FAngle write FAngle;
    property Clip: Boolean read FClip write FClip;
    property PenUp: Boolean read FPenUp write FPenUp;
    property Speed: Integer read FSpeed write FSpeed;
    property PenColor: TColor read FPenColor write FPenColor;
  end;

  TTimer = class(TSwingComponent)
  private
    FCoalesce: Boolean;
    FDelay: Integer;
    FInitialDelay: Integer;
    FLogTimers: Boolean;
    FRepeats: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Coalesce: Boolean read FCoalesce write FCoalesce;
    property Delay: Integer read FDelay write FDelay;
    property InitialDelay: Integer read FInitialDelay write FInitialDelay;
    property LogTimers: Boolean read FLogTimers write FLogTimers;
    property Repeats: Boolean read FRepeats write FRepeats;
  end;

implementation

uses
  Controls,
  UJava,
  UGUIDesigner;

{ --- TJPlayground -------------------------------------------------------------- }

constructor TJPlayground.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +33;
  ControlStyle := [csAcceptsControls];
  Background := clWhite;
  JavaType := 'Playground';
end;

procedure TJPlayground.NewControl;
begin
  FPartner.InsertImport('ch.aplu.turtle.*');
  InsertNewVariable('private Playground ' + Name + ' = new Playground();');
  DefaultComponent;
  MakeAttribut('Background', 'Color.WHITE');
end;

{ --- TJTurtle ----------------------------------------------------------------- }

constructor TJTurtle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +34;
  Height := 26;
  Width := 20;
  Speed := 200;
  Clip := True;
  PenUp := False;
  PenColor := clBlue;
  JavaType := 'Turtle';
end;

const
  Show1 = '|Angle|Clip|PenUp|Speed|PenColor';

function TJTurtle.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := Show1 + inherited;
end;

procedure TJTurtle.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'PenColor' then
    MakeColor(Attr, Value)
  else if Pos(Attr, Show1) > 0 then
    MakeAttribute(Attr, Value, Typ)
  else
    inherited;
end;

function TJTurtle.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TJTurtle.MakeAttribute(const Attr, Value, Typ: string);
begin
  var
  Str := SurroundFix2(Name + '.set' + Attr + '(' + Value + ');');
  SetAttributValueAfter(Attr, Str);
end;

procedure TJTurtle.SetPositionAndSize;
begin
  Width := 20;
  Height := 26;
  ChangeAttributValue(Name + '.setBounds(', Name + GetBounds);
end;

procedure TJTurtle.NewControl;
begin
  InsertNewVariable('private Turtle ' + Name + ' = new Turtle();');
  SetAttributValueAfter('___XXX___', SurroundFix2(GetContainerAdd));
  SetAttributValueAfter('___XXX___', SurroundFix2(Name + GetBounds));
end;

procedure TJTurtle.Paint;
const
  Colors: array [0 .. 10] of TColor = (clAqua, clRed, clLime, clBlue, clYellow,
    $00C0C0C0, clFuchsia, $0000C8FF, $00AFAFFF, clBlack, clGray);
var
  AColor: Integer;
begin
  if Foreground = $00333333 then
  begin
    AColor := Parent.ControlCount - 1;
    if AColor in [0..10] then
      Foreground := Colors[AColor];
  end
  else
  begin
    AColor := 0;
    while AColor < 11 do
    begin
      if Colors[AColor] = Foreground then
        Break;
      Inc(AColor);
    end;
    if AColor = 11 then
      AColor := 0;
  end;
  FGUIDesigner.vilTurtles.Draw(Canvas, 0, 0, AColor);
end;

{ --- TTimer ------------------------------------------------------------------- }

constructor TTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 49;
  Height := 28;
  Width := 32;
  Sizeable := False;
  Delay := 1000;
  InitialDelay := Delay;
  Repeats := True;
  JavaType := 'Timer';
  ShowFont := False;
end;

function TTimer.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Coalesce|Delay|InitialDelay|LogTimers|Repeats' + inherited;
end;

function TTimer.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed';
end;

procedure TTimer.NewControl;
begin
  InsertNewVariable('private Timer ' + Name + ' = new Timer(1000, null);');
  AddListener('actionPerformed');
end;

procedure TTimer.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilSwing1Light.Draw(Canvas, 5, 4, 18);
end;

end.
