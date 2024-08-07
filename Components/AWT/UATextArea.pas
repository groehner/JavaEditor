unit UATextArea;

interface

uses
  Classes, StdCtrls, UAComponents;

type

  TAWTScrollBars = (BOTH, HORIZONTAL_ONLY, NONE, VERTICAL_ONLY);

  TATextArea = class (TAWTComponent)
  private
    FScrollbars: TAWTScrollBars;
    FText: TStrings;
    FCaretPosition: integer;
    FEditable: boolean;
    FSelectionEnd: integer;
    FSelectionStart: integer;
    procedure setStrings(Strings: TStrings);
    procedure setScrollBars(aValue: TAWTScrollBars);
    procedure setEditable(aValue: boolean);
    procedure MakeScrollbars(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aMemo: TMemo);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Scrollbars: TAWTScrollBars read FScrollbars write setScrollBars;
    property Text: TStrings read FText write setStrings;
    property CaretPosition: integer read FCaretPosition write FCaretPosition default 0;
    property Editable: boolean read FEditable write setEditable;
    property SelectionEnd: integer read FSelectionEnd write FSelectionEnd default 0;
    property SelectionStart: integer read FSelectionStart write FSelectionStart default 0;
    property textValueChanged;
  end;

implementation

uses Windows, SysUtils, Math, Graphics, Controls;

{--- TATextArea ---------------------------------------------------------------}

constructor TATextArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= -3;
  Width:= 120;
  Height:= 80;
  Cursor:= crIBeam;
  FEditable:= true;
  FScrollbars:= BOTH;
  FText:= TStringList.Create;
  JavaType:= 'TextArea';
end;

constructor TATextArea.CreateFrom(aMemo: TMemo);
begin
  Create(aMemo.Owner);
  CreateFromA(aMemo);
  Font:= aMemo.Font;
  Foreground:= Font.Color;
  Background:= aMemo.Color;
  if Background = clBtnFace then Background:= clWhite;   
  CaretPosition:= aMemo.MaxLength;
  Text.text:= aMemo.Lines.text;
  Editable:= not aMemo.ReadOnly;
  case aMemo.Scrollbars of
    ssNone      : Scrollbars:= NONE;
    ssHorizontal: Scrollbars:= HORIZONTAL_ONLY;
    ssVertical  : Scrollbars:= VERTICAL_ONLY;
    ssBoth      : Scrollbars:= BOTH;
  end;
end;

function TATextArea.getAttributes(ShowAttributes: integer): string;
begin
  if ShowAttributes = 1
    then Result:= '|Text|Scrollbars'
    else Result:= '|Text|Scrollbars|CaretPosition|Editable|SelectionEnd|SelectionStart';
  Result:= Result + inherited getAttributes(ShowAttributes)
end;

procedure TATextArea.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Scrollbars' then
    MakeScrollbars(Value)
  else if Attr = 'Text' then
    MakeText(Text)
  else
    inherited;
end;

procedure TATextArea.MakeScrollbars(const Value: string);
  var s1, s2: string;
begin
  s1:= 'private TextArea ' + Name;
  s2:= ' = new TextArea("", 1, 1, TextArea.SCROLLBARS_' + Value + ');';
  Partner.ReplaceAttribute(s1, Indent1 + s1 + s2);
end;

function TATextArea.getEvents(ShowEvents: integer): string;
begin
  Result:= '|textValueChanged' + inherited;
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
  var th, y, i: integer; R1: TRect;
begin
  // paint surrounding Rectangle
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Pen.Color:= RGB(160, 160, 160);
  Canvas.MoveTo(0, Height-2);
  Canvas.LineTo(0, 0);
  Canvas.LineTo(Width-1, 0);
  Canvas.Pen.Color:= AWTDarkGray;
  Canvas.MoveTo(1, Height-2);
  Canvas.LineTo(1, 1);
  Canvas.LineTo(Width-2, 1);
  Canvas.Pen.Color:= RGB(227, 227, 227);
  Canvas.LineTo(Width-2, Height-2);
  Canvas.LineTo(0, Height-2);

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  th:= Canvas.TextHeight('Hg');
  y:= 3;
  for i:= 0 to FText.Count - 1 do begin
    R1:= Rect(2, y, Width-2, min(y+th, Height-2));
    Canvas.TextRect(R1, 3, y, FText.Strings[i]);
    y:= y + th;
    if y > Height then break;
  end;

  // paint scrollbars
  var p18:= PPIScale(18);
  case Scrollbars of
    BOTH: begin
      ScrollBar(Rect(Width-p18, 2, Width-2, Height-p18), false, false);
      Canvas.Brush.Color:= DefaultBackground;
      Canvas.FillRect(Rect(Width-p18, Height-p18, Width-2, Height-2));
      ScrollBar(Rect(2, Height-p18, Width-p18, Height-2), true, false);
    end;
    HORIZONTAL_ONLY:
      ScrollBar(Rect(2, Height-p18, Width-2, Height-2), true, false);
    NONE: ;
    VERTICAL_ONLY:
      ScrollBar(Rect(Width-p18, 2, Width-2, Height-2), false, false);
  end;
end;

procedure TATextArea.setScrollBars(aValue: TAWTScrollBars);
begin
  if aValue <> FScrollbars then begin
    FScrollBars:= aValue;
    Invalidate;
  end;
end;

procedure TATextArea.setStrings(Strings: TStrings);
begin
  if Strings.Text <> FText.Text then begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TATextArea.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

end.
