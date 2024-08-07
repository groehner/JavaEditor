unit UJTextArea;

{ Classes
  TJTextArea = class (TSwingComponent)
  TJEditorPane = class (TSwingComponent)
    TJTextPane
}

interface

uses
  Classes, StdCtrls, UJEComponents, UJComponents;

type

  TContentTyp = (html, plain, rtf);

  TJTextArea = class (TSwingComponent)
  private
    FLineWrap: boolean;
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    FWrapStyleWord: boolean;
    FTabSize: integer;
    FCaretPosition: integer;
    FEditable: boolean;
    FSelectionEnd: integer;
    FSelectionStart: integer;
    FText: TStrings;
    procedure setStrings(Strings: TStrings);
    procedure setLineWrap(aValue: boolean);
    procedure setHorizontalScrollBarPolicy(aValue: TScrollBarPolicy);
    procedure setVerticalScrollBarPolicy(aValue: TScrollBarPolicy);
    procedure setWrapStyleWord(aValue: boolean);
    procedure setEditable(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aMemo: TMemo);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function GetContainerAdd: string; override;
    procedure SetPositionAndSize; override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property LineWrap: boolean read FLineWrap write setLineWrap;
    property HorizontalScrollBarPolicy: TScrollBarPolicy read FHorizontalScrollBarPolicy write setHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy read FVerticalScrollBarPolicy write setVerticalScrollBarPolicy;
    property WrapStyleWord: boolean read FWrapStyleWord write setWrapStyleWord;
    property TabSize: integer read FTabSize write FTabSize;
    property Text: TStrings read FText write setStrings;
    property CaretPosition: integer read FCaretPosition write FCaretPosition default 0;
    property Editable: boolean read FEditable write setEditable;
    property SelectionEnd: integer read FSelectionEnd write FSelectionEnd default 0;
    property SelectionStart: integer read FSelectionStart write FSelectionStart default 0;
  end;

  TJEditorPane = class(TSwingComponent)
  private
    FHorizontalScrollBarPolicy: TScrollBarPolicy;
    FVerticalScrollBarPolicy: TScrollBarPolicy;
    FContentType: TContentTyp;
    FPage: string;
    FText: TStrings;
    procedure setText(Strings: TStrings);
    procedure setPage(Value: string);
    procedure setHorizontalScrollBarPolicy(aValue: TScrollBarPolicy);
    procedure setVerticalScrollBarPolicy(aValue: TScrollBarPolicy);
    procedure MakePage(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function GetContainerAdd: string; override;
    procedure SetPositionAndSize; override;
    procedure NewControl; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property Text: TStrings read FText write setText;
    property Page: string read FPage write setPage;
    property ContentType: TContentTyp read FContentType write FContentType;
    property HorizontalScrollBarPolicy: TScrollBarPolicy read FHorizontalScrollBarPolicy write setHorizontalScrollBarPolicy;
    property VerticalScrollBarPolicy: TScrollBarPolicy read FVerticalScrollBarPolicy write setVerticalScrollBarPolicy;
  end;

  TJTextPane = class(TJEditorPane)
  public
    constructor Create(AOwner: TComponent); override;
    procedure NewControl; override;
  end;

implementation

uses Windows, SysUtils, Math, Graphics, Controls, UUtils;

{--- TJTextArea ---------------------------------------------------------------}

constructor TJTextArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +3;
  Width:= 120;
  Height:= 80;
  Background:= clWhite;
  FTabSize:= 8;
  FEditable:= true;
  Cursor:= crIBeam;
  FHorizontalScrollBarPolicy:= AS_NEEDED;
  FVerticalScrollBarPolicy:= AS_NEEDED;
  FText:= TStringList.Create;
  JavaType:= 'JTextArea';
  Font.Style:= [];
end;

constructor TJTextArea.CreateFrom(aMemo: TMemo);
begin
  Create(aMemo.Owner);
  CreateFromJ(aMemo);
  Font:= aMemo.Font;
  Foreground:= Font.Color;
  Background:= aMemo.Color;
  if Background = clBtnFace then Background:= clWhite;   
  CaretPosition:= aMemo.MaxLength;
  Text.text:= aMemo.Lines.text;
  Editable:= not aMemo.ReadOnly;
  LineWrap:= aMemo.WordWrap;
end;

function TJTextArea.getAttributes(ShowAttributes: integer): string;
  const
    Show1 = '|Text|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
begin
  if ShowAttributes = 1
    then Result:= Show1
    else Result:= Show1 + '|CaretPosition|Editable|SelectionEnd|SelectionStart' +
                          '|LineWrap|WrapStyleWord|TabSize';
  Result:= Result + inherited getAttributes(ShowAttributes)
end;

procedure TJTextArea.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else if Attr = 'Text' then
    MakeText(Text)
  else
    inherited;
end;

procedure TJTextArea.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(', Name + 'ScrollPane' + getBounds);
end;

function TJTextArea.GetContainerAdd: string;
begin
  Result:= AWTSwingContainer;
  if Result = ''
    then Result:= 'add(' + name + 'ScrollPane);'
    else Result:= Result + '.add(' + name + 'ScrollPane);';
end;

procedure TJTextArea.NewControl;
begin
  InsertNewVariable('private JTextArea ' + Name + ' = new JTextArea();');
  InsertNewVariable('  private JScrollPane ' + Name + 'ScrollPane = new JScrollPane(' + Name + ');');
  setAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane' + getBounds);
  Partner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

procedure TJTextArea.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  Partner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJTextArea.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'ScrollPane' , NewName + 'ScrollPane', true);
end;

destructor TJTextArea.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

procedure TJTextArea.Paint;
  var th, y, i, taw, tah: integer; R1: TRect;
      hsb, vsb: boolean; SL: TStringList;

  procedure split(const s: string; var s1, s2: string);
    var len, n: integer;
  begin
    len:= Canvas.TextWidth('abcdefghijklmnopqrstuvwxyz');
    n:= round(taw / (len / 26.0)) + 1;
    s1:= copy(s, 1, n);
    s2:= copy(s, n+1, length(s));

    while Canvas.TextWidth(s1) < taw do begin
      s1:= s1 + s2[1];
      delete(s2, 1, 1);
    end;
    while (Canvas.TextWidth(s1) > taw) and (length(s1) > 0) do begin
      s2:= s1[length(s1)] + s2;
      delete(s1, length(s1), 1);
    end;
    if FWrapStyleWord and (Pos(' ', s1) > 0) then
      while (length(s1) > 0) and (s1[length(s1)] <> ' ') do begin
        s2:= s1[length(s1)] + s2;
        delete(s1, length(s1), 1);
      end;
  end;

  procedure makeSL;
    var s, s1, s2: string; i: integer;
  begin
    SL.Clear;
    for i:= 0 to FText.Count - 1 do begin
      s:= FText[i];
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
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  SL:= TStringList.Create;
  SL.Text:= FText.Text;

  th:= Canvas.TextHeight('Hg') + 1;
  vsb:= (VerticalScrollBarPolicy = ALWAYS);
  hsb:= (HorizontalScrollBarPolicy = ALWAYS);
  if vsb then taw:= Width - PPIScale(19)  else taw:= Width - PPIScale(3);
  if hsb then tah:= Height - PPIScale(19) else tah:= Height - PPIScale(3);

  if FLineWrap then begin
    makeSL;
    if not vsb and (VerticalScrollBarPolicy <> NEVER) and (SL.Count * th > tah) then begin
      dec(taw, PPIScale(16));
      makeSL;
      vsb:= true;
    end;
  end else begin
    if not hsb and (HorizontalScrollBarPolicy <> NEVER)then
      for i:= 0 to SL.Count - 1 do
        if Canvas.TextWidth(SL.Strings[i]) > taw then begin
          dec(tah, PPIScale(16));
          hsb:= true;
          break;
        end;
    if not vsb and (VerticalScrollBarPolicy <> NEVER) and (SL.Count * th > tah) then begin
      dec(taw, PPIScale(16));
      vsb:= true;
    end;
  end;
  y:= 1;
  for i:= 0 to SL.Count - 1 do begin
    R1:= Rect(1, y, taw, min(y+th, tah));
    Canvas.TextRect(R1, 1, y, SL.Strings[i]);
    y:= y + th;
    if y > tah + th then break;
  end;
  FreeAndNil(SL);

  // paint scrollbars
  var p17:= PPIScale(17);
  if hsb and vsb then begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.FillRect(Rect(Width-p17, Height-p17, Width-2, Height-2));
    ScrollBar(Rect(Width-p17, 1, Width-2, Height-p17), false, true); // vsb
    ScrollBar(Rect(1, Height-p17, Width-p17, Height-2), true, true); // hsb
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-p17);
    Canvas.LineTo(Width-p17, Height-p17);
    Canvas.LineTo(Width-p17, 0);
  end else if hsb then begin
    ScrollBar(Rect(1, Height-p17, Width-2, Height-2), true, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-p17);
    Canvas.LineTo(Width-1, Height-p17);
  end else if vsb then begin
    ScrollBar(Rect(Width-p17, 1, Width-2, Height-2), false, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(Width-p17, Height-p17);
    Canvas.LineTo(Width-p17, 0);
  end;
end;

procedure TJTextArea.setLineWrap(aValue: boolean);
begin
  if aValue <> FLineWrap then begin
    FLineWrap:= aValue;
    Invalidate;
  end;
end;

procedure TJTextArea.setHorizontalScrollBarPolicy(aValue: TScrollBarPolicy);
begin
  if aValue <> FHorizontalScrollBarPolicy then begin
    FHorizontalScrollBarPolicy:= aValue;
    Invalidate;
  end;
end;

procedure TJTextArea.setVerticalScrollBarPolicy(aValue: TScrollBarPolicy);
begin
  if aValue <> FVerticalScrollBarPolicy then begin
    FVerticalScrollBarPolicy:= aValue;
    Invalidate;
  end;
end;

procedure TJTextArea.setWrapStyleWord(aValue: boolean);
begin
  if aValue <> FWrapStyleWord then begin
    FWrapStyleWord:= aValue;
    Invalidate;
  end;
end;

procedure TJTextArea.setStrings(Strings: TStrings);
begin
  if Strings.Text <> FText.Text then begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TJTextArea.setEditable(aValue: boolean);
begin
  if aValue <> FEditable then begin
    FEditable:= aValue;
    Invalidate;
  end;
end;

{--- TJEditorPane ---------------------------------------------------------}

constructor TJEditorPane.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +28;
  Width:= 120;
  Height:= 80;
  Background:= clWhite;
  FText:= TStringList.Create;
  FHorizontalScrollBarPolicy:= AS_NEEDED;
  FVerticalScrollBarPolicy:= AS_NEEDED;
  FContentType:= plain;
  JavaType:= 'JEditorPane';
end;

destructor TJEditorPane.Destroy;
begin
  FreeAndNil(FText);
  inherited;
end;

function TJEditorPane.getAttributes(ShowAttributes: integer): string;
  const
    Show1 = '|Text|Page|ContentType|HorizontalScrollBarPolicy|VerticalScrollBarPolicy';
begin
  Result:= show1 + inherited;
end;

procedure TJEditorPane.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalScrollBarPolicy') or (Attr = 'VerticalScrollBarPolicy') then
    MakeScrollbarDisplayPolicy(Attr, Value)
  else if Attr = 'Text' then
    MakeText(Text)
  else if Attr = 'ContentType' then
    MakeAttribut('ContentType', asString('text/' + Value))
  else if Attr = 'Page' then
    MakePage(Value)
  else
    inherited;
end;

procedure TJEditorPane.SetPositionAndSize;
begin
  ChangeAttributValue(Name + 'ScrollPane.setBounds(', Name + 'ScrollPane' + getBounds);
end;

function TJEditorPane.GetContainerAdd: string;
begin
  Result:= AWTSwingContainer;
  if Result = ''
    then Result:= 'add(' + name + 'ScrollPane);'
    else Result:= Result + '.add(' + name + 'ScrollPane);';
end;

procedure TJEditorPane.NewControl;
begin
  InsertNewVariable('private JEditorPane ' + Name + ' = new JEditorPane();');
  InsertNewVariable('  private JScrollPane ' + Name + 'ScrollPane = new JScrollPane(' + Name + ');');
  setAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane' + getBounds);
  Partner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

procedure TJEditorPane.DeleteComponent;
begin
  Partner.DeleteTryCatch(Name + '.setPage');
  inherited;
  Partner.DeleteAttribute('private JScrollPane ' + Name + 'ScrollPane');
  Partner.DeleteAttributeValues(Name + 'ScrollPane');
end;

procedure TJEditorPane.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'ScrollPane' , NewName + 'ScrollPane', true);
end;

procedure TJEditorPane.MakePage(const Value: string);
  var key, s: string;
begin
  key:= Name + '.setPage("';
  if Partner.hasText(key) then
    s:= Indent2 + Indent1 +  key + Value + '");'
  else
    s:= surroundFix2('try {') +
        surroundFix2(Indent1 +  key + Value + '");') +
        surroundFix2('} catch (Throwable t) {') +
        surroundFix2(Indent1 + 't.printStackTrace();') +
        surroundFix2('}');
  setAttributValue(key, s);
end;

procedure TJEditorPane.Paint;
  var th, y, i, taw, tah: integer; R1: TRect; path: string;
      hsb, vsb: boolean; SL: TStringList;
begin
  // paint surrounding Rectangle
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clWhite;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(1, 1, Width, Height));

  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));

  // paint text
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  SL:= TStringList.Create;
  path:= toWindows(copy(FPage, 7, length(FPage)));
  if (FPage <> '') and FileExists(path)
    then SL.LoadFromFile(path)
    else SL.Text:= FText.Text;

  th:= Canvas.TextHeight('Hg') + 1;
  vsb:= (FVerticalScrollBarPolicy = ALWAYS);
  hsb:= (FHorizontalScrollBarPolicy = ALWAYS);
  if vsb then taw:= Width - 19  else taw:= Width - 3;
  if hsb then tah:= Height - 19 else tah:= Height - 3;

  if not hsb and (FHorizontalScrollBarPolicy <> NEVER)then
    for i:= 0 to SL.Count - 1 do
      if Canvas.TextWidth(SL[i]) > taw then begin
        dec(tah, 16);
        hsb:= true;
        break;
      end;
  if not vsb and (FVerticalScrollBarPolicy <> NEVER) and (SL.Count * th > tah) then begin
    dec(taw, 16);
    vsb:= true;
  end;
  y:= 1;
  for i:= 0 to SL.Count - 1 do begin
    R1:= Rect(1, y, taw, min(y+th, tah));
    Canvas.TextRect(R1, 1, y, SL[i]);
    y:= y + th;
    if y > tah + th then break;
  end;
  FreeAndNil(SL);

  // paint scrollbars
  if hsb and vsb then begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.FillRect(Rect(Width-17, Height-17, Width-2, Height-2));
    ScrollBar(Rect(Width-17, 1, Width-2, Height-17), false, true); // vsb
    ScrollBar(Rect(1, Height-17, Width-17, Height-2), true, true); // hsb
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-17);
    Canvas.LineTo(Width-17, Height-17);
    Canvas.LineTo(Width-17, 0);
  end else if hsb then begin
    ScrollBar(Rect(1, Height-17, Width-2, Height-2), true, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(1, Height-17);
    Canvas.LineTo(Width-1, Height-17);
  end else if vsb then begin
    ScrollBar(Rect(Width-17, 1, Width-2, Height-2), false, true);
    Canvas.Pen.Color:= DarkShadow;
    Canvas.MoveTo(Width-17, Height-17);
    Canvas.LineTo(Width-17, 0);
  end;
end;

procedure TJEditorPane.setHorizontalScrollBarPolicy(aValue: TScrollBarPolicy);
begin
  if aValue <> FHorizontalScrollBarPolicy then begin
    FHorizontalScrollBarPolicy:= aValue;
    Invalidate;
  end;
end;

procedure TJEditorPane.setVerticalScrollBarPolicy(aValue: TScrollBarPolicy);
begin
  if aValue <> FVerticalScrollBarPolicy then begin
    FVerticalScrollBarPolicy:= aValue;
    Invalidate;
  end;
end;

procedure TJEditorPane.setText(Strings: TStrings);
begin
  if  FText.Text <> Strings.Text then begin
    FText.Assign(Strings);
    Invalidate;
  end;
end;

procedure TJEditorPane.setPage(Value: string);
begin
  if FPage <> Value then begin
    FPage:= Value;
    Invalidate;
  end;
end;

{--- TJTextPane ---------------------------------------------------------------}

constructor TJTextPane.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +29;
  JavaType:= 'JTextPane';
end;

procedure TJTextPane.NewControl;
begin
  InsertNewVariable('private JTextPane ' + Name + ' = new JTextPane();');
  InsertNewVariable('  private JScrollPane ' + Name + 'ScrollPane = new JScrollPane(' + Name + ');');
  setAttributValue(Name + 'ScrollPane.setBounds(', Indent2 + Name + 'ScrollPane' + getBounds);
  Partner.InsertComponent(Indent2 + GetContainerAdd);
  MakeFont;
end;

end.
