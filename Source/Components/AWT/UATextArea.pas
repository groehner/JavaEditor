unit UATextArea;

interface

uses
  Classes,
  StdCtrls,
  UAComponents;

type

  TAWTScrollBars = (BOTH, HORIZONTAL_ONLY, NONE, VERTICAL_ONLY);

  TATextArea = class(TAWTComponent)
  private
    FScrollbars: TAWTScrollBars;
    FText: TStrings;
    FCaretPosition: Integer;
    FEditable: Boolean;
    FSelectionEnd: Integer;
    FSelectionStart: Integer;
    procedure SetText(Strings: TStrings);
    procedure SetScrollBars(AValue: TAWTScrollBars);
    procedure SetEditable(AValue: Boolean);
    procedure MakeScrollbars(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Scrollbars: TAWTScrollBars read FScrollbars write SetScrollBars;
    property Text: TStrings read FText write SetText;
    property CaretPosition: Integer read FCaretPosition write FCaretPosition
      default 0;
    property Editable: Boolean read FEditable write SetEditable;
    property SelectionEnd: Integer read FSelectionEnd write FSelectionEnd
      default 0;
    property SelectionStart: Integer read FSelectionStart write FSelectionStart
      default 0;
    property textValueChanged;
  end;

implementation

uses
  Windows,
  SysUtils,
  Math,
  Graphics,
  Controls;

{ --- TATextArea --------------------------------------------------------------- }

constructor TATextArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -3;
  Width := 120;
  Height := 80;
  Cursor := crIBeam;
  FEditable := True;
  FScrollbars := BOTH;
  FText := TStringList.Create;
  JavaType := 'TextArea';
end;

function TATextArea.GetAttributes(ShowAttributes: Integer): string;
begin
  if ShowAttributes = 1 then
    Result := '|Text|Scrollbars'
  else
    Result := '|Text|Scrollbars|CaretPosition|Editable|SelectionEnd|SelectionStart';
  Result := Result + inherited GetAttributes(ShowAttributes);
end;

procedure TATextArea.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Scrollbars' then
    MakeScrollbars(Value)
  else if Attr = 'Text' then
    MakeText(Text)
  else
    inherited;
end;

procedure TATextArea.MakeScrollbars(const Value: string);
var
  Str1, Str2: string;
begin
  Str1 := 'private TextArea ' + Name;
  Str2 := ' = new TextArea("", 1, 1, TextArea.SCROLLBARS_' + Value + ');';
  FPartner.ReplaceAttribute(Str1, Indent1 + Str1 + Str2);
end;

function TATextArea.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|textValueChanged' + inherited;
end;

procedure TATextArea.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private TextArea ' + Name +
    ' = new TextArea("", 1, 1, TextArea.SCROLLBARS_BOTH);');
end;

destructor TATextArea.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

procedure TATextArea.Paint;
var
  TextH, YPos: Integer;
  Rect1: TRect;
begin
  // paint surrounding Rectangle
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Pen.Color := RGB(160, 160, 160);
  Canvas.MoveTo(0, Height - 2);
  Canvas.LineTo(0, 0);
  Canvas.LineTo(Width - 1, 0);
  Canvas.Pen.Color := AWTDarkGray;
  Canvas.MoveTo(1, Height - 2);
  Canvas.LineTo(1, 1);
  Canvas.LineTo(Width - 2, 1);
  Canvas.Pen.Color := RGB(227, 227, 227);
  Canvas.LineTo(Width - 2, Height - 2);
  Canvas.LineTo(0, Height - 2);

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  TextH := Canvas.TextHeight('Hg');
  YPos := 3;
  for var I := 0 to FText.Count - 1 do
  begin
    Rect1 := Rect(2, YPos, Width - 2, Min(YPos + TextH, Height - 2));
    Canvas.TextRect(Rect1, 3, YPos, FText[I]);
    YPos := YPos + TextH;
    if YPos > Height then
      Break;
  end;

  // paint scrollbars
  var
  P18 := PPIScale(18);
  case Scrollbars of
    BOTH:
      begin
        Scrollbar(Rect(Width - P18, 2, Width - 2, Height - P18), False, False);
        Canvas.Brush.Color := DefaultBackground;
        Canvas.FillRect(Rect(Width - P18, Height - P18, Width - 2, Height - 2));
        Scrollbar(Rect(2, Height - P18, Width - P18, Height - 2), True, False);
      end;
    HORIZONTAL_ONLY:
      Scrollbar(Rect(2, Height - P18, Width - 2, Height - 2), True, False);
    NONE:
      ;
    VERTICAL_ONLY:
      Scrollbar(Rect(Width - P18, 2, Width - 2, Height - 2), False, False);
  end;
end;

procedure TATextArea.SetScrollBars(AValue: TAWTScrollBars);
begin
  if AValue <> FScrollbars then
  begin
    FScrollbars := AValue;
    Invalidate;
  end;
end;

procedure TATextArea.SetText(Strings: TStrings);
begin
  if Strings.Text <> FText.Text then
  begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TATextArea.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

end.
