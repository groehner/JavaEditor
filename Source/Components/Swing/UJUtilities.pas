unit UJUtilities;

{ Classes
    TJPlayground = class(TSwingComponent)
    TJTurtle = class (TSwingComponent)
    TTimer = class (TSwingComponent)
 }

interface

uses
  Classes, Graphics, UJComponents;

type

  TJPlayground = class(TSwingComponent)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TJTurtle = class (TSwingComponent)
  private
    FAngle: integer;
    FClip: boolean;
    FPenUp: boolean;
    FSpeed: integer;
    FPenColor: TColor;
    procedure MakeAttribute(const Attr, Value, Typ: string);
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure SetPositionAndSize; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Angle: integer read FAngle write FAngle;
    property Clip: boolean read FClip write FClip;
    property PenUp: boolean read FPenUp write FPenUp;
    property Speed: integer read FSpeed write FSpeed;
    property PenColor: TColor read FPenColor write FPenColor;
  end;

  TTimer = class (TSwingComponent)
  private
    FCoalesce: boolean;
    FDelay: integer;
    FInitialDelay: integer;
    FLogTimers: boolean;
    FRepeats: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Coalesce: boolean read FCoalesce write FCoalesce;
    property Delay: integer read FDelay write FDelay;
    property InitialDelay: integer read FInitialDelay write FInitialDelay;
    property LogTimers: boolean read FLogTimers write FLogTimers;
    property Repeats: boolean read FRepeats write FRepeats;
  end;

implementation

uses Controls, UJava, UGUIDesigner;

{--- TJPlayground --------------------------------------------------------------}

constructor TJPlayground.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +33;
  ControlStyle:= [csAcceptsControls];
  Background:= clWhite;
  JavaType:= 'Playground';
end;

procedure TJPlayground.NewControl;
begin
  Partner.InsertImport('ch.aplu.turtle.*');
  InsertNewVariable('private Playground ' + Name + ' = new Playground();');
  DefaultComponent;
  MakeAttribut('Background', 'Color.WHITE');
end;

{--- TJTurtle -----------------------------------------------------------------}

constructor TJTurtle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +34;
  Height:= 26;
  Width:= 20;
  Speed:= 200;
  Clip:= true;
  PenUp:= false;
  PenColor:= clBlue;
  JavaType:= 'Turtle';
end;

const
  Show1 = '|Angle|Clip|PenUp|Speed|PenColor';

function TJTurtle.getAttributes(ShowAttributes: integer): string;
begin
  Result:= show1 + inherited;
end;

procedure TJTurtle.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'PenColor' then
    MakeColor(Attr, Value)
  else if Pos(Attr, show1) > 0 then
    MakeAttribute(Attr, Value, Typ)
  else
    inherited;
end;

function TJTurtle.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TJTurtle.MakeAttribute(const Attr, Value, Typ: string);
begin
 var s:= surroundFix2(Name + '.set' + Attr + '(' + Value + ');');
  setAttributValueAfter(Attr, s);
end;

procedure TJTurtle.SetPositionAndSize;
begin
  Width:= 20;
  Height:= 26;
  ChangeAttributValue(Name + '.setBounds(', Name + getBounds);
end;

procedure TJTurtle.NewControl;
begin
  InsertNewVariable('private Turtle ' + Name + ' = new Turtle();');
  setAttributValueAfter('___XXX___', surroundFix2(getContainerAdd));
  setAttributValueAfter('___XXX___', surroundFix2(Name + getBounds));
end;

procedure TJTurtle.Paint;
  const Colors:array[0..10] of TColor = (clAqua, clRed, clLime, clBlue, clYellow,
                                         $00C0C0C0, clFuchsia, $0000C8FF, $00AFAFFF, clBlack, clGray);
  var aColor: integer;
begin
  if Foreground = $00333333 then begin
    aColor:= Parent.ControlCount - 1;
    try
      Foreground:= Colors[aColor];
    except
    end;
  end else begin
    aColor:= 0;
    while aColor < 11 do begin
      if Colors[aColor] = Foreground then
        break;
      inc(aColor);
    end;
    if aColor = 11 then
      aColor:= 0;
  end;
  FGUIDesigner.vilTurtles.Draw(Canvas, 0, 0, aColor);
end;

{--- TTimer -------------------------------------------------------------------}

constructor TTimer.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 49;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  Delay:= 1000;
  InitialDelay:= Delay;
  Repeats:= true;
  JavaType:= 'Timer';
  ShowFont:= false;
end;

function TTimer.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Coalesce|Delay|InitialDelay|LogTimers|Repeats' + inherited;
end;

function TTimer.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed';
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
