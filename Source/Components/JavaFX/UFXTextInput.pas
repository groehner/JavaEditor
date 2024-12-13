unit UFXTextInput;

{ classes
    TFXTextInputControl = class(TFXControl)
      TFXTextField = class (TFXTextInputControl)
        TFXNumberField = class(TFXTextField)
        TFXPasswordField = class (TFXTextField)
      TFXTextArea = class (TFXTextInputControl)
}

interface

uses
  Classes, UFXComponents;

type

  TFXTextInputControl = class(TFXControl)
  private
    FText: string;
    FEditable: boolean;
    FPromptText: string;
    procedure setEditable(aValue: boolean);
    procedure setPromptText(const aValue: string);
    procedure setText(const aValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure MakeFont; override;
  published
    property Text: string read FText write setText;
    property Editable: boolean read FEditable write setEditable;
    property PromptText: string read FPromptText write setPromptText;
  end;

  TFXTextField = class (TFXTextInputControl)
  private
    FAlignment: TAlignment;
    FPrefColumnCount: integer;
    Faction: string;
    procedure setAlignment(aValue: TAlignment);
    function isNumeric(const s: string): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property Alignment: TAlignment read FAlignment write setAlignment;
    property PrefColumnCount: integer read FPrefColumnCount write FPrefColumnCount;
    property action: string read Faction write Faction;
  end;

  TFXNumberField = class (TFXTextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TFXPasswordField = class (TFXTextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TFXTextArea = class (TFXTextInputControl)
  private
    FScrollLeft: double;
    FScrollTop: double;
    FText: TStrings;
    FWrapText: boolean;

    procedure setStrings(Strings: TStrings);
    procedure setWrapText(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property ScrollLeft: double read FScrollLeft write FScrollLeft;
    property ScrollTop: double read FScrollTop write FScrollTop;
    property Text: TStrings read FText write setStrings;
    property WrapText: boolean read FWrapText write setWrapText;
  end;

implementation

uses Windows, Graphics, Controls, Math, SysUtils, UUtils;

{--- TFXTextInputControl ------------------------------------------------------}

constructor TFXTextInputControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditable:= true;
end;

procedure TFXTextInputControl.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

procedure TFXTextInputControl.setPromptText(const aValue: string);
begin
  if aValue <> FPromptText then begin
    FPromptText:= aValue;
    Invalidate;
  end;
end;

procedure TFXTextInputControl.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

function TFXTextInputControl.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|PromptText|Text|Font';
        Attributes2 = Attributes1 + '|Editable';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes)
end;

procedure TFXTextInputControl.MakeFont;
begin
  DoMakeFont;
end;

{--- TFXTextField ----------------------------------------------------------}

constructor TFXTextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= 102;
  PrefWidth:= 80;
  PrefHeight:= 24;
  Background:= clWhite;
  Cursor:= crIBeam;
  FAlignment:= CENTER_LEFT;
  FPrefColumnCount:= 12;
  JavaType:= 'TextField';
end;

procedure TFXTextField.Paint;
  var x, y, tw, th: integer; s: string;
begin
  Canvas.Pen.Color:= DefaultBorderColor;
  Canvas.Brush.Style:= bsClear;
  Canvas.Brush.Color:= Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);

  if (Self is TFXNumberField) and not isNumeric(Text) then begin
    Canvas.Brush.Color:= clRed;
    Canvas.RoundRect(2, 2, Width-2, Height-2, CornerRadius, CornerRadius);
  end;
  s:= Text;
  if s = '' then
    s:= PromptText
  else if (self is TFXPasswordField) then
    s:= StringOfChar(WideChar($2022), Length(s));
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  tw:= Canvas.TextWidth(s);
  th:= Canvas.TextHeight('Hg');
  x:= 0;
  y:= 0;
  case FAlignment of
    TOP_LEFT     : begin x:= 8; y:= 8; end;
    TOP_CENTER   : begin x:= (Width - tw) div 2; y:= 8; end;
    TOP_RIGHT    : begin x:= Width - tw - 8; y:= 8; end;
    CENTER_LEFT  : begin x:= 8; y:= (Height - th) div 2; end;
    CENTER       : begin x:= (Width - tw) div 2; y:= (Height - th) div 2; end;
    CENTER_RIGHT : begin x:= Width - tw - 8; y:= (Height - th) div 2; end;
    BASELINE_LEFT,
    BOTTOM_LEFT  : begin x:= 8; y:= Height - th - 8; end;
    BASELINE_CENTER,
    BOTTOM_CENTER: begin x:= (Width - tw) div 2;y:= Height - th - 8; end;
    BASELINE_RIGHT,
    BOTTOM_RIGHT : begin x:= Width - tw - 8; y:= Height - th - 8; end;
  end;
  if Text = '' then
    Canvas.Font.Color:= clLtGray;
  Canvas.TextOut(x, y, s);
end;

procedure TFXTextField.setAlignment(aValue: TAlignment);
begin
  if aValue <> FAlignment then begin
    FAlignment:= aValue;
    Invalidate;
  end;
end;

{$HINTS OFF}
function TFXTextField.isNumeric(const s: string): boolean;
  var v: double; c: integer;
begin
  val(s, v, c);
  Result:= (c = 0);
  if not Result and ((s = '-') or (s = '.') or (s = ''))
    then Result:= true;
end;
{$HINTS ON}

procedure TFXTextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private TextField ' + Name + ' = new TextField();');
  MakeFont;
end;

function TFXTextField.getEvents(ShowEvents: integer): string;
begin
  Result:= '|action' + inherited getEvents(ShowEvents);
end;

function TFXTextField.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Alignment';
        Attributes2 = Attributes1 + '|PrefColumnCount';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes);
end;

procedure TFXTextField.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(Faction);
end;

{--- TFXNumberField -----------------------------------------------------------}

constructor TFXNumberField.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  PrefWidth:= 80;
  Tag:= 103;
  JavaType:= 'NumberField';
end;

procedure TFXNumberField.NewControl;
begin
  DefaultComponent;
  InsertImport('je.NumberField');
  InsertNewVariable('private NumberField ' + Name + ' = new NumberField();');
  MakeFont;
end;

{--- TFXPasswordField ----------------------------------------------------------}

constructor TFXPasswordField.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +137;
  JavaType:= 'PasswordField';
end;

procedure TFXPasswordField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private PasswordField ' + Name + ' = new PasswordField();');
  MakeFont;
end;

{--- TFXTextArea ---------------------------------------------------------------}

constructor TFXTextArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +104;
  Width:= 120;
  Height:= 80;
  Background:= clWhite;
  Cursor:= crIBeam;
  FText:= TStringList.Create;
  JavaType:= 'TextArea';
end;

destructor TFXTextArea.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

procedure TFXTextArea.Paint;
  var th, y, i, taw, tah: integer; R1: TRect;
      SL: TStringList;

  procedure split(const s: string; var s1, s2: string);
    var len, n: integer;
  begin
    len:= Canvas.TextWidth('abcdefghijklmnopqrstuvwxyz');
    n:= round(taw / (len / 26.0)) + 1;
    s1:= copy(s, 1, n);
    s2:= copy(s, n+1, length(s));

    while Canvas.Textwidth(s1) < taw do begin
      s1:= s1 + s2[1];
      delete(s2, 1, 1);
    end;
    while (Canvas.TextWidth(s1) > taw) and (length(s1) > 0) do begin
      s2:= s1[length(s1)] + s2;
      delete(s1, length(s1), 1);
    end;
  end;

  procedure makeSL;
    var s, s1, s2: string; i: integer;
  begin
    SL.Clear;
    for i:= 0 to FText.Count - 1 do begin
      s:= FText.Strings[i]; s1:= ''; s2:= '';
      while Canvas.TextWidth(s) > taw do begin
        split(s, s1, s2);
        SL.Add(s1);
        s:= s2;
      end;
      SL.Add(s);
    end;
  end;

begin
  // paint surrounding Rectangle
  Canvas.Pen.Color:= DefaultBorderColor;
  Canvas.Brush.Style:= bsClear;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  if FText.Text <> #$0D#$0A then begin
    SL:= TStringList.Create;
    SL.Text:= FText.Text;

    th:= Canvas.TextHeight('Hg') + 1;
    taw:= Width - 7;
    tah:= Height - 3;

    if FWrapText then begin
      makeSL;
      if SL.Count * th > tah then begin
        dec(taw, 16);
        makeSL;
      end;
    end else begin
      for i:= 0 to SL.Count - 1 do
        if Canvas.TextWidth(SL.Strings[i]) > taw then
          dec(tah, 16);
      if SL.Count * th > tah then begin
        dec(taw, 16);
      end;
    end;

    y:= 1;
    for i:= 0 to SL.Count - 1 do begin
      R1:= Rect(1, y, taw, min(y+th, tah));
      Canvas.TextRect(R1, 5, y, SL.Strings[i]);
      y:= y + th;
      if y > tah + th then break;
    end;
    FreeAndNil(SL);
  end else if PromptText <> '' then begin
    Canvas.Font.Color:= clLtGray;
    Canvas.TextOut(9, 1, PromptText);
  end;

end;

procedure TFXTextArea.setStrings(Strings: TStrings);
begin
  if Strings.Text <> FText.Text then begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TFXTextArea.setWrapText(aValue: boolean);
begin
  if aValue <> FWrapText then begin
    FWrapText:= aValue;
    Invalidate;
  end;
end;

procedure TFXTextArea.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private TextArea ' + Name + ' = new TextArea();');
  MakeFont;
end;

procedure TFXTextArea.SetAttribute(Attr, Value, Typ: string);
  var s: string; i: integer;
begin
  if Attr = 'Text' then begin
    s:= '';
    for i:= 0 to Text.Count - 1 do
      s:= s + Text.Strings[i] + '\n';
    delete(s, length(s)-1, 2);
    MakeAttribut('Text', asString(s));
  end else
    inherited;
end;

function TFXTextArea.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|PromptText|Text|Font|WrapText';
        Attributes2 = Attributes1 + '|Editable|ScrollLeft|ScrollTop';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes)
end;

end.
