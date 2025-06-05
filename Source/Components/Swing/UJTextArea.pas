unit UJTextArea;

{ Classes
  TJTextArea = class (TSwingComponent)
  TJEditorPane = class (TSwingComponent)
  TJTextPane
}

interface

uses
  Classes,
  StdCtrls,
  UJEComponents,
  UJComponents;

type

  TContentTyp = (html, plain, rtf);

  TJTextArea = class(TSwingComponent)
  private
    FLineWrap: Boolean;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    FWrapStyleWord: Boolean;
    FTabSize: Integer;
    FCaretPosition: Integer;
    FEditable: Boolean;
    FSelectionEnd: Integer;
    FSelectionStart: Integer;
    FText: TStrings;
    procedure SetStrings(Strings: TStrings);
    procedure SetLineWrap(AValue: Boolean);
    procedure SetHorizontalScrollBarPolicy(AValue: TScrollBarPolicy);
    procedure SetVerticalScrollBarPolicy(AValue: TScrollBarPolicy);
    procedure SetWrapStyleWord(AValue: Boolean);
    procedure SetEditable(AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(AMemo: TMemo);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetContainerAdd: string; override;
    procedure SetPositionAndSize; override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property LineWrap: Boolean read FLineWrap write SetLineWrap;
    property HorizontalScrollBarPolicy: TScrollBarPolicy
      read FHorizontalScrollBarPolicy write SetHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy
      read FVerticalScrollBarPolicy write SetVerticalScrollBarPolicy;
    property WrapStyleWord: Boolean read FWrapStyleWord write SetWrapStyleWord;
    property TabSize: Integer read FTabSize write FTabSize;
    property Text: TStrings read FText write SetStrings;
    property CaretPosition: Integer read FCaretPosition write FCaretPosition
      default 0;
    property Editable: Boolean read FEditable write SetEditable;
    property SelectionEnd: Integer read FSelectionEnd write FSelectionEnd
      default 0;
    property SelectionStart: Integer read FSelectionStart write FSelectionStart
      default 0;
  end;

  TJEditorPane = class(TSwingComponent)
  private
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    FContentType: TContentTyp;
    FPage: string;
    FText: TStrings;
    procedure SetText(Strings: TStrings);
    procedure SetPage(Value: string);
    procedure SetHorizontalScrollBarPolicy(AValue: TScrollBarPolicy);
    procedure SetVerticalScrollBarPolicy(AValue: TScrollBarPolicy);
    procedure MakePage(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetContainerAdd: string; override;
    procedure SetPositionAndSize; override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property Text: TStrings read FText write SetText;
    property Page: string read FPage write SetPage;
    property ContentType: TContentTyp read FContentType write FContentType;
    property HorizontalScrollBarPolicy: TScrollBarPolicy
      read FHorizontalScrollBarPolicy write SetHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy
      read FVerticalScrollBarPolicy write SetVerticalScrollBarPolicy;
  end;

  TJTextPane = class(TJEditorPane)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Math,
  Graphics,
  Controls,
  UUtils;

{ --- TJTextArea --------------------------------------------------------------- }

constructor TJTextArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +3;
  Width := 120;
  Height := 80;
  Background := clWhite;
  FTabSize := 8;
  FEditable := True;
  Cursor := crIBeam;
  FHorizontalScrollBarPolicy := AS_NEEDED;
  FVerticalScrollBarPolicy := AS_NEEDED;
  FText := TStringList.Create;
  JavaType := 'JTextArea';
  Font.Style := [];
end;

constructor TJTextArea.CreateFrom(AMemo: TMemo);
begin
  Create(AMemo.Owner);
  CreateFromJ(AMemo);
  Font := AMemo.Font;
  Foreground := Font.Color;
  Background := AMemo.Color;
  if Background = clBtnFace then
    Background := clWhite;
  CaretPosition := AMemo.MaxLength;
  Text.Text := AMemo.Lines.Text;
  Editable := not AMemo.ReadOnly;
  LineWrap := AMemo.WordWrap;
end;

function TJTextArea.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|Text|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
begin
  if ShowAttributes = 1 then
    Result := Show1
  else
    Result := Show1 + '|CaretPosition|Editable|SelectionEnd|SelectionStart' +
      '|LineWrap|WrapStyleWord|TabSize';
  Result := Result + inherited GetAttributes(ShowAttributes);
end;

procedure TJTextArea.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy')
  then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else if Attr = 'Text' then
    MakeText(Text)
  else
    inherited;
end;

procedure TJTextArea.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(',
    Name + 'ScrollPane' + GetBounds);
end;

function TJTextArea.GetContainerAdd: string;
begin
  Result := AWTSwingContainer;
  if Result = '' then
    Result := 'add(' + Name + 'ScrollPane);'
  else
    Result := Result + '.add(' + Name + 'ScrollPane);';
end;

procedure TJTextArea.NewControl;
begin
  InsertNewVariable('private JTextArea ' + Name + ' = new JTextArea();');
  InsertNewVariable('  private JScrollPane ' + Name +
    'ScrollPane = new JScrollPane(' + Name + ');');
  SetAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane'
    + GetBounds);
  FPartner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

procedure TJTextArea.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  FPartner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJTextArea.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'ScrollPane', NewName + 'ScrollPane', True);
end;

destructor TJTextArea.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

procedure TJTextArea.Paint;
var
  TextHeight, YPos, Taw, Tah: Integer;
  Rect1: TRect;
  Hsb, Vsb: Boolean;
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
    if FWrapStyleWord and (Pos(' ', Str1) > 0) then
      while (Length(Str1) > 0) and (Str1[Length(Str1)] <> ' ') do
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
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  StringList := TStringList.Create;
  StringList.Text := FText.Text;

  TextHeight := Canvas.TextHeight('Hg') + 1;
  Vsb := (VerticalScrollBarPolicy = ALWAYS);
  Hsb := (HorizontalScrollBarPolicy = ALWAYS);
  if Vsb then
    Taw := Width - PPIScale(19)
  else
    Taw := Width - PPIScale(3);
  if Hsb then
    Tah := Height - PPIScale(19)
  else
    Tah := Height - PPIScale(3);

  if FLineWrap then
  begin
    makeSL;
    if not Vsb and (VerticalScrollBarPolicy <> NEVER) and
      (StringList.Count * TextHeight > Tah) then
    begin
      Dec(Taw, PPIScale(16));
      makeSL;
      Vsb := True;
    end;
  end
  else
  begin
    if not Hsb and (HorizontalScrollBarPolicy <> NEVER) then
      for var I := 0 to StringList.Count - 1 do
        if Canvas.TextWidth(StringList[I]) > Taw then
        begin
          Dec(Tah, PPIScale(16));
          Hsb := True;
          Break;
        end;
    if not Vsb and (VerticalScrollBarPolicy <> NEVER) and
      (StringList.Count * TextHeight > Tah) then
    begin
      Dec(Taw, PPIScale(16));
      Vsb := True;
    end;
  end;
  YPos := 1;
  for var I := 0 to StringList.Count - 1 do
  begin
    Rect1 := Rect(1, YPos, Taw, Min(YPos + TextHeight, Tah));
    Canvas.TextRect(Rect1, 1, YPos, StringList[I]);
    YPos := YPos + TextHeight;
    if YPos > Tah + TextHeight then
      Break;
  end;
  FreeAndNil(StringList);

  // paint scrollbars
  var
  P17 := PPIScale(17);
  if Hsb and Vsb then
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.FillRect(Rect(Width - P17, Height - P17, Width - 2, Height - 2));
    Scrollbar(Rect(Width - P17, 1, Width - 2, Height - P17), False, True);
    // Vsb
    Scrollbar(Rect(1, Height - P17, Width - P17, Height - 2), True, True);
    // Hsb
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - P17);
    Canvas.LineTo(Width - P17, Height - P17);
    Canvas.LineTo(Width - P17, 0);
  end
  else if Hsb then
  begin
    Scrollbar(Rect(1, Height - P17, Width - 2, Height - 2), True, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - P17);
    Canvas.LineTo(Width - 1, Height - P17);
  end
  else if Vsb then
  begin
    Scrollbar(Rect(Width - P17, 1, Width - 2, Height - 2), False, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(Width - P17, Height - P17);
    Canvas.LineTo(Width - P17, 0);
  end;
end;

procedure TJTextArea.SetLineWrap(AValue: Boolean);
begin
  if AValue <> FLineWrap then
  begin
    FLineWrap := AValue;
    Invalidate;
  end;
end;

procedure TJTextArea.SetHorizontalScrollBarPolicy(AValue: TScrollBarPolicy);
begin
  if AValue <> FHorizontalScrollBarPolicy then
  begin
    FHorizontalScrollBarPolicy := AValue;
    Invalidate;
  end;
end;

procedure TJTextArea.SetVerticalScrollBarPolicy(AValue: TScrollBarPolicy);
begin
  if AValue <> FVerticalScrollBarPolicy then
  begin
    FVerticalScrollBarPolicy := AValue;
    Invalidate;
  end;
end;

procedure TJTextArea.SetWrapStyleWord(AValue: Boolean);
begin
  if AValue <> FWrapStyleWord then
  begin
    FWrapStyleWord := AValue;
    Invalidate;
  end;
end;

procedure TJTextArea.SetStrings(Strings: TStrings);
begin
  if Strings.Text <> FText.Text then
  begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TJTextArea.SetEditable(AValue: Boolean);
begin
  if AValue <> FEditable then
  begin
    FEditable := AValue;
    Invalidate;
  end;
end;

{ --- TJEditorPane --------------------------------------------------------- }

constructor TJEditorPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +28;
  Width := 120;
  Height := 80;
  Background := clWhite;
  FText := TStringList.Create;
  FHorizontalScrollBarPolicy := AS_NEEDED;
  FVerticalScrollBarPolicy := AS_NEEDED;
  FContentType := plain;
  JavaType := 'JEditorPane';
end;

destructor TJEditorPane.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

function TJEditorPane.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|Text|Page|ContentType|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
begin
  Result := Show1 + inherited;
end;

procedure TJEditorPane.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy')
  then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else if Attr = 'Text' then
    MakeText(Text)
  else if Attr = 'ContentType' then
    MakeAttribut('ContentType', AsString('text/' + Value))
  else if Attr = 'Page' then
    MakePage(Value)
  else
    inherited;
end;

procedure TJEditorPane.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(',
    Name + 'ScrollPane' + GetBounds);
end;

function TJEditorPane.GetContainerAdd: string;
begin
  Result := AWTSwingContainer;
  if Result = '' then
    Result := 'add(' + Name + 'ScrollPane);'
  else
    Result := Result + '.add(' + Name + 'ScrollPane);';
end;

procedure TJEditorPane.NewControl;
begin
  InsertNewVariable('private JEditorPane ' + Name + ' = new JEditorPane();');
  InsertNewVariable('  private JScrollPane ' + Name +
    'ScrollPane = new JScrollPane(' + Name + ');');
  SetAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane'
    + GetBounds);
  FPartner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

procedure TJEditorPane.DeleteComponent;
begin
  FPartner.DeleteTryCatch(Name + '.SetPage');
  inherited;
  FPartner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  FPartner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJEditorPane.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'ScrollPane', NewName + 'ScrollPane', True);
end;

procedure TJEditorPane.MakePage(const Value: string);
var
  Key, Str: string;
begin
  Key := Name + '.SetPage("';
  if FPartner.HasText(Key) then
    Str := Indent2 + Indent1 + Key + Value + '");'
  else
    Str := SurroundFix2('try {') + SurroundFix2(Indent1 + Key + Value + '");') +
      SurroundFix2('} catch (Throwable t) {') +
      SurroundFix2(Indent1 + 't.printStackTrace();') + SurroundFix2('}');
  SetAttributValue(Key, Str);
end;

procedure TJEditorPane.Paint;
var
  TextHeight, YPos, Taw, Tah: Integer;
  Rect1: TRect;
  Path: string;
  Hsb, Vsb: Boolean;
  StringList: TStringList;
begin
  // paint surrounding Rectangle
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  StringList := TStringList.Create;
  Path := ToWindows(Copy(FPage, 7, Length(FPage)));
  if (FPage <> '') and FileExists(Path) then
    StringList.LoadFromFile(Path)
  else
    StringList.Text := FText.Text;

  TextHeight := Canvas.TextHeight('Hg') + 1;
  Vsb := (FVerticalScrollBarPolicy = ALWAYS);
  Hsb := (FHorizontalScrollBarPolicy = ALWAYS);
  if Vsb then
    Taw := Width - 19
  else
    Taw := Width - 3;
  if Hsb then
    Tah := Height - 19
  else
    Tah := Height - 3;

  if not Hsb and (FHorizontalScrollBarPolicy <> NEVER) then
    for var I := 0 to StringList.Count - 1 do
      if Canvas.TextWidth(StringList[I]) > Taw then
      begin
        Dec(Tah, 16);
        Hsb := True;
        Break;
      end;
  if not Vsb and (FVerticalScrollBarPolicy <> NEVER) and
    (StringList.Count * TextHeight > Tah) then
  begin
    Dec(Taw, 16);
    Vsb := True;
  end;
  YPos := 1;
  for var I := 0 to StringList.Count - 1 do
  begin
    Rect1 := Rect(1, YPos, Taw, Min(YPos + TextHeight, Tah));
    Canvas.TextRect(Rect1, 1, YPos, StringList[I]);
    YPos := YPos + TextHeight;
    if YPos > Tah + TextHeight then
      Break;
  end;
  FreeAndNil(StringList);

  // paint scrollbars
  if Hsb and Vsb then
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.FillRect(Rect(Width - 17, Height - 17, Width - 2, Height - 2));
    Scrollbar(Rect(Width - 17, 1, Width - 2, Height - 17), False, True); // Vsb
    Scrollbar(Rect(1, Height - 17, Width - 17, Height - 2), True, True); // Hsb
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - 17);
    Canvas.LineTo(Width - 17, Height - 17);
    Canvas.LineTo(Width - 17, 0);
  end
  else if Hsb then
  begin
    Scrollbar(Rect(1, Height - 17, Width - 2, Height - 2), True, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(1, Height - 17);
    Canvas.LineTo(Width - 1, Height - 17);
  end
  else if Vsb then
  begin
    Scrollbar(Rect(Width - 17, 1, Width - 2, Height - 2), False, True);
    Canvas.Pen.Color := DarkShadow;
    Canvas.MoveTo(Width - 17, Height - 17);
    Canvas.LineTo(Width - 17, 0);
  end;
end;

procedure TJEditorPane.SetHorizontalScrollBarPolicy(AValue: TScrollBarPolicy);
begin
  if AValue <> FHorizontalScrollBarPolicy then
  begin
    FHorizontalScrollBarPolicy := AValue;
    Invalidate;
  end;
end;

procedure TJEditorPane.SetVerticalScrollBarPolicy(AValue: TScrollBarPolicy);
begin
  if AValue <> FVerticalScrollBarPolicy then
  begin
    FVerticalScrollBarPolicy := AValue;
    Invalidate;
  end;
end;

procedure TJEditorPane.SetText(Strings: TStrings);
begin
  if FText.Text <> Strings.Text then
  begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TJEditorPane.SetPage(Value: string);
begin
  if FPage <> Value then
  begin
    FPage := Value;
    Invalidate;
  end;
end;

{ --- TJTextPane --------------------------------------------------------------- }

constructor TJTextPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +29;
  JavaType := 'JTextPane';
end;

procedure TJTextPane.NewControl;
begin
  InsertNewVariable('private JTextPane ' + Name + ' = new JTextPane();');
  InsertNewVariable('  private JScrollPane ' + Name +
    'ScrollPane = new JScrollPane(' + Name + ');');
  SetAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane'
    + GetBounds);
  FPartner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

end.
