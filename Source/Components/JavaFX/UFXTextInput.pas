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
  Classes,
  UFXComponents;

type

  TFXTextInputControl = class(TFXControl)
  private
    FText: string;
    FEditable: Boolean;
    FPromptText: string;
    procedure SetEditable(AValue: Boolean);
    procedure SetPromptText(const AValue: string);
    procedure SetText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure MakeFont; override;
  published
    property Text: string read FText write SetText;
    property Editable: Boolean read FEditable write SetEditable;
    property PromptText: string read FPromptText write SetPromptText;
  end;

  TFXTextField = class(TFXTextInputControl)
  private
    FAlignment: TAlignment;
    FPrefColumnCount: Integer;
    FAction: string;
    procedure SetAlignment(AValue: TAlignment);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property PrefColumnCount: Integer read FPrefColumnCount
      write FPrefColumnCount;
    property action: string read FAction write FAction;
  end;

  TFXNumberField = class(TFXTextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TFXPasswordField = class(TFXTextField)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

  TFXTextArea = class(TFXTextInputControl)
  private
    FScrollLeft: Double;
    FScrollTop: Double;
    FText: TStrings;
    FWrapText: Boolean;

    procedure SetStrings(Strings: TStrings);
    procedure SetWrapText(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property ScrollLeft: Double read FScrollLeft write FScrollLeft;
    property ScrollTop: Double read FScrollTop write FScrollTop;
    property Text: TStrings read FText write SetStrings;
    property WrapText: Boolean read FWrapText write SetWrapText;
  end;

implementation

uses
  Windows,
  Graphics,
  Controls,
  Math,
  SysUtils,
  UUtils;

{ --- TFXTextInputControl ------------------------------------------------------ }

constructor TFXTextInputControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditable := True;
end;

procedure TFXTextInputControl.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

procedure TFXTextInputControl.SetPromptText(const AValue: string);
begin
  if AValue <> FPromptText then
  begin
    FPromptText := AValue;
    Invalidate;
  end;
end;

procedure TFXTextInputControl.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

function TFXTextInputControl.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|PromptText|Text|Font';
  Attributes2 = Attributes1 + '|Editable';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
end;

procedure TFXTextInputControl.MakeFont;
begin
  DoMakeFont;
end;

{ --- TFXTextField ---------------------------------------------------------- }

constructor TFXTextField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 102;
  PrefWidth := 80;
  PrefHeight := 24;
  Background := clWhite;
  Cursor := crIBeam;
  FAlignment := CENTER_LEFT;
  FPrefColumnCount := 12;
  JavaType := 'TextField';
end;

procedure TFXTextField.Paint;
var
  XPos, YPos, TextWidth, TextHeight: Integer;
  Str: string;
begin
  Canvas.Pen.Color := DefaultBorderColor;
  Canvas.Brush.Style := bsClear;
  Canvas.Brush.Color := Background;
  Canvas.RoundRect(0, 0, Width, Height, CornerRadius, CornerRadius);

  if (Self is TFXNumberField) and not IsNumeric(Text) then
  begin
    Canvas.Brush.Color := clRed;
    Canvas.RoundRect(2, 2, Width - 2, Height - 2, CornerRadius, CornerRadius);
  end;
  Str := Text;
  if Str = '' then
    Str := PromptText
  else if (Self is TFXPasswordField) then
    Str := StringOfChar(WideChar($2022), Length(Str));
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  TextWidth := Canvas.TextWidth(Str);
  TextHeight := Canvas.TextHeight('Hg');
  XPos := 0;
  YPos := 0;
  case FAlignment of
    TOP_LEFT:
      begin
        XPos := 8;
        YPos := 8;
      end;
    TOP_CENTER:
      begin
        XPos := (Width - TextWidth) div 2;
        YPos := 8;
      end;
    TOP_RIGHT:
      begin
        XPos := Width - TextWidth - 8;
        YPos := 8;
      end;
    CENTER_LEFT:
      begin
        XPos := 8;
        YPos := (Height - TextHeight) div 2;
      end;
    CENTER:
      begin
        XPos := (Width - TextWidth) div 2;
        YPos := (Height - TextHeight) div 2;
      end;
    CENTER_RIGHT:
      begin
        XPos := Width - TextWidth - 8;
        YPos := (Height - TextHeight) div 2;
      end;
    BASELINE_LEFT, BOTTOM_LEFT:
      begin
        XPos := 8;
        YPos := Height - TextHeight - 8;
      end;
    BASELINE_CENTER, BOTTOM_CENTER:
      begin
        XPos := (Width - TextWidth) div 2;
        YPos := Height - TextHeight - 8;
      end;
    BASELINE_RIGHT, BOTTOM_RIGHT:
      begin
        XPos := Width - TextWidth - 8;
        YPos := Height - TextHeight - 8;
      end;
  end;
  if Text = '' then
    Canvas.Font.Color := clLtGray;
  Canvas.TextOut(XPos, YPos, Str);
end;

procedure TFXTextField.SetAlignment(AValue: TAlignment);
begin
  if AValue <> FAlignment then
  begin
    FAlignment := AValue;
    Invalidate;
  end;
end;

procedure TFXTextField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private TextField ' + Name + ' = new TextField();');
  MakeFont;
end;

function TFXTextField.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|action' + inherited GetEvents(ShowEvents);
end;

function TFXTextField.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Alignment';
  Attributes2 = Attributes1 + '|PrefColumnCount';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
end;

procedure TFXTextField.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FAction);
end;

{ --- TFXNumberField ----------------------------------------------------------- }

constructor TFXNumberField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PrefWidth := 80;
  Tag := 103;
  JavaType := 'NumberField';
end;

procedure TFXNumberField.NewControl;
begin
  DefaultComponent;
  InsertImport('je.NumberField');
  InsertNewVariable('private NumberField ' + Name + ' = new NumberField();');
  MakeFont;
end;

{ --- TFXPasswordField ---------------------------------------------------------- }

constructor TFXPasswordField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +137;
  JavaType := 'PasswordField';
end;

procedure TFXPasswordField.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private PasswordField ' + Name +
    ' = new PasswordField();');
  MakeFont;
end;

{ --- TFXTextArea --------------------------------------------------------------- }

constructor TFXTextArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +104;
  Width := 120;
  Height := 80;
  Background := clWhite;
  Cursor := crIBeam;
  FText := TStringList.Create;
  JavaType := 'TextArea';
end;

destructor TFXTextArea.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

procedure TFXTextArea.Paint;
var
  TextH, YPos, Taw, Tah: Integer;
  Rect1: TRect;
  StringList: TStringList;

  procedure Split(const Str: string; var Str1, Str2: string);
  var
    Len, Num: Integer;
  begin
    Len := Canvas.TextWidth('abcdefghijklmnopqrstuvwxyz');
    Num := Round(Taw / (Len / 26.0)) + 1;
    Str1 := Copy(Str, 1, Num);
    Str2 := Copy(Str, Num + 1, Length(Str));

    while Canvas.TextWidth(Str1) < Taw do
    begin
      Str1 := Str1 + Str2[1];
      Delete(Str2, 1, 1);
    end;
    while (Canvas.TextWidth(Str1) > Taw) and (Length(Str1) > 0) do
    begin
      Str2 := Str1[Length(Str1)] + Str2;
      Delete(Str1, Length(Str1), 1);
    end;
  end;

  procedure makeSL;
  var
    Str, Str1, Str2: string;
  begin
    StringList.Clear;
    for var I := 0 to FText.Count - 1 do
    begin
      Str := FText[I];
      Str1 := '';
      Str2 := '';
      while Canvas.TextWidth(Str) > Taw do
      begin
        Split(Str, Str1, Str2);
        StringList.Add(Str1);
        Str := Str2;
      end;
      StringList.Add(Str);
    end;
  end;

begin
  // paint surrounding Rectangle
  Canvas.Pen.Color := DefaultBorderColor;
  Canvas.Brush.Style := bsClear;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  if FText.Text <> #$0D#$0A then
  begin
    StringList := TStringList.Create;
    StringList.Text := FText.Text;

    TextH := Canvas.TextHeight('Hg') + 1;
    Taw := Width - 7;
    Tah := Height - 3;

    if FWrapText then
    begin
      makeSL;
      if StringList.Count * TextH > Tah then
      begin
        Dec(Taw, 16);
        makeSL;
      end;
    end
    else
    begin
      for var I := 0 to StringList.Count - 1 do
        if Canvas.TextWidth(StringList[I]) > Taw then
          Dec(Tah, 16);
      if StringList.Count * TextH > Tah then
      begin
        Dec(Taw, 16);
      end;
    end;

    YPos := 1;
    for var I := 0 to StringList.Count - 1 do
    begin
      Rect1 := Rect(1, YPos, Taw, Min(YPos + TextH, Tah));
      Canvas.TextRect(Rect1, 5, YPos, StringList[I]);
      YPos := YPos + TextH;
      if YPos > Tah + TextH then
        Break;
    end;
    FreeAndNil(StringList);
  end
  else if PromptText <> '' then
  begin
    Canvas.Font.Color := clLtGray;
    Canvas.TextOut(9, 1, PromptText);
  end;

end;

procedure TFXTextArea.SetStrings(Strings: TStrings);
begin
  if Strings.Text <> FText.Text then
  begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TFXTextArea.SetWrapText(AValue: Boolean);
begin
  if AValue <> FWrapText then
  begin
    FWrapText := AValue;
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
var
  Str: string;
begin
  if Attr = 'Text' then
  begin
    Str := '';
    for var I := 0 to Text.Count - 1 do
      Str := Str + Text[I] + '\n';
    Delete(Str, Length(Str) - 1, 2);
    MakeAttribut('Text', AsString(Str));
  end
  else
    inherited;
end;

function TFXTextArea.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|PromptText|Text|Font|WrapText';
  Attributes2 = Attributes1 + '|Editable|ScrollLeft|ScrollTop';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
end;

end.
