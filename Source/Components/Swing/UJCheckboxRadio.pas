unit UJCheckboxRadio;

{ Classes
  TJCheckBox = class (TSwingComponent)
    TJRadioButton
  TJButtonGroup = class (TSwingComponent)
  TJ2ButtonGroup = class (TSwingComponent)
}

interface

uses
  Classes, StdCtrls, UJComponents;

type

  TJCheckBox = class (TSwingComponent)
  private
    FText: string;
    FSelected: Boolean;
    procedure setSelected(aValue: boolean);
    procedure setText(const aValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aCheckbox: TCheckbox);
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Selected: boolean read FSelected write setSelected;
    property Text: string read FText write setText;
  end;

  // deprecated
  TJRadioButton = class (TJCheckBox)
  private
    FButtonGroup: string;
    procedure MakeButtonGroup(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aRadioButton: TRadioButton); overload;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property ButtonGroup: string read FButtonGroup write FButtonGroup;
  end;

  // deprecated
  TJButtonGroup = class (TSwingComponent)
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  end;

  TJ2ButtonGroup = class (TSwingComponent)
  private
    FCheckboxes: boolean;
    FColumns: integer;
    FFrame: boolean;
    FTitle: string;
    FItems: TStrings;
    FOldItems: TStrings;
    procedure setColumns(Value: integer);
    procedure setTitle(Value: string);
    procedure setItems(Value: TStrings);
    procedure setCheckboxes(Value: boolean);
    procedure setFrame(Value: boolean);
    procedure MakeButtongroupItems;
    procedure MakeTitle;
    function ItemsInColumn(i: integer): integer;
    function RBName(i: integer): string;
    procedure DeleteComponentAttributes;
    procedure DeleteComponentValues;
    procedure MakeSelectedLabelMethod;
    procedure DeleteSelectedLabelMethod;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteComponent; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure DeleteListener(const event: string); override;
    procedure AddListener(const event: string); override;
    function MakeEventProcedure(const Event: string): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure SetPositionAndSize; override;
    procedure MakeFont; override;
  published
    property Items: TStrings read fItems write setItems; // must stay before columns or label
    property Columns: integer read FColumns write setColumns;
    property Checkboxes: boolean read FCheckboxes write setCheckboxes;
    property Title: string read FTitle write setTitle;
    property Frame: boolean read FFrame write setFrame;
  end;

implementation

uses Windows, SysUtils, Graphics, Controls, UITypes, UJava,
     JvGnugettext, UStringRessources, UConfiguration, UGUIDesigner;

{--- TJCheckBox ---------------------------------------------------------------}

constructor TJCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +5;
  Width:= 120;
  Height:= 24;
  FSelected:= False;
  Background:= clBtnFace;
  Opaque:= false;
  Text:= 'Checkbox';
  JavaType:= 'JCheckBox';
end;

constructor TJCheckBox.CreateFrom(aCheckbox: TCheckbox);
begin
  Create(aCheckbox.Owner);
  CreateFromJ(aCheckbox);
  Text:= aCheckbox.Caption; // Label
  Font:= aCheckbox.Font;
  Foreground:= Font.Color;
  Background:= aCheckbox.Color;
  Selected:= aCheckbox.Checked;
end;

function TJCheckBox.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Text|Selected' + inherited getAttributes(ShowAttributes);
end;

function TJCheckBox.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed|itemStateChanged|stateChanged' + inherited;
end;

procedure TJCheckBox.NewControl;
begin
  FText:= 'CheckBox';
  DefaultComponent;
  InsertNewVariable('private JCheckBox ' + Name + ' = new JCheckBox();');
  MakeAttribut('Text', asString(Text));
//  setAttributValue(Name + '.setText(', Name + '.setText("' + Text + '");');
  MakeAttribut('Opaque', 'false');
end;

procedure TJCheckBox.Paint;
  const BoxW = 13; // Width of rectangular checkbox
        BoxH = 15;

  var th, x, y: Integer;

  function TagToPicNr: integer;
  begin
    case Tag of
      -5: Result:= 0;
      +5: Result:= 2;
      -6: Result:= 4;
      +6: Result:= 6;
    else  Result:= 0;
    end;
    if FSelected then inc(Result);
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  th:= Canvas.TextHeight('Q');

  // write the text
  Canvas.Pen.Color:= Font.Color;
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TwinControl).Brush.Color
    else Canvas.Brush.Color:= Background;

  Canvas.FillRect(Self.GetClientRect);
  if Tag > 0 then x:= BoxW + 8 else x:= BoxW + 4;
  x:= PPIScale(x);
  y:= (Height - th) div 2;
  Canvas.TextOut(x, y, FText);

  if Tag > 0 then x:= PPIScale(4) else x:= 0;
  y:= (Height - PPIScale(BoxH)) div 2;
  FGUIDesigner.vilControls1315.Draw(Canvas, x, y, TagToPicNr);
end;

procedure TJCheckBox.setSelected(aValue: Boolean);
begin
  if FSelected <> aValue then begin
    FSelected := aValue;
    Invalidate;
  end;
end;

procedure TJCheckBox.setText(const aValue: string);
begin
  if FText <> aValue then begin
    FText:= aValue;
    Invalidate;
  end;
end;

{--- TJRadioButton ------------------------------------------------------------}

constructor TJRadioButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +6;
  Width:= 80;
  JavaType:= 'JRadioButton';
end;

constructor TJRadioButton.CreateFrom(aRadioButton: TRadioButton);
begin
  Create(aRadioButton.Owner);
  CreateFromJ(aRadioButton);
  Text:= aRadioButton.Caption;
  Font:= aRadioButton.Font;
  Foreground:= Font.Color;
  Background:= aRadioButton.Color;
  Selected:= aRadioButton.Checked;
  ButtonGroup:= aRadioButton.HelpKeyword;
end;

function TJRadioButton.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|ButtonGroup' + inherited getAttributes(ShowAttributes);
end;

procedure TJRadioButton.MakeButtonGroup(const Value: string);
  var key, s: string;
      from, till, i: integer;
begin
  key:= 'private ButtonGroup';
  from:= Partner.GetLineNumberWith(key);
  till:= Partner.GetLNGEndComponents;
  for i:= from to till do begin
    s:= Partner.Editor.Lines[i];
    if Pos(key, s) > 0 then begin
      delete(s, Pos(' =', s), length(s));
      delete(s, 1, Pos(key, s) + length(key));
      // s is name of a ButtonGroup
      Partner.DeleteAttributeValue(s + '.add(' + Name);
    end
  end;
  if Value <> '' then
    setAttributValue('___XXX___', Value + '.add(' + Name + ');');
end;

procedure TJRadioButton.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'ButtonGroup' then
    MakeButtonGroup(Value)
  else
    inherited;
end;

{--- ButtonGroup deprecated ---------------------------------------------------}

constructor TJButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 7;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'ButtonGroup';
end;

function TJButtonGroup.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name';
end;

function TJButtonGroup.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TJButtonGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_getSelectedButtonGroupLabel',
                      NewName + '_getSelectedButtonGroupLabel', false);
end;

procedure TJButtonGroup.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_getSelectedRadioButtonLabel', false);
end;

procedure TJButtonGroup.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 6, 4, 7);
end;

{--- TJ2ButtonGroup -----------------------------------------------------------}

constructor TJ2ButtonGroup.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= 50;
  Width:= 120;
  Height:= 80;
  FColumns:= 1;
  FItems:= TStringList.Create;
  FItems.Text:= _('America') + ', ' + _('selected') + #13#10 + _('Europe') + #13#10 + _('Asia');
  FOldItems:= TStringList.Create;
  FTitle:= _('Continent');
  FCheckboxes:= false;
  FFrame:= true;
  JavaType:= 'JButtonGroup';
end;

destructor TJ2ButtonGroup.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FOldItems);
  inherited;
end;

procedure TJ2ButtonGroup.DeleteComponent;
begin
  inherited;
  DeleteComponentAttributes;
  DeleteComponentValues;
  Partner.DeleteAttribute('private JPanel ' + Name);
  Partner.DeleteAttributeValue(GetContainer + '.add(' + Name + ')');
  Partner.DeleteAttributeValue(Name + '.setLayout');
  DeleteSelectedLabelMethod;
end;

procedure TJ2ButtonGroup.DeleteComponentAttributes;
begin
  Partner.DeleteAttribute('ButtonGroup ' + Name + 'BG');
  Partner.DeleteAttribute('TitledBorder ' + Name + 'TB');
  for var i:= 0 to FOldItems.Count - 1 do
    Partner.DeleteAttribute(RBName(i));
end;

procedure TJ2ButtonGroup.DeleteComponentValues;
begin
  Partner.DeleteAttributeValue(Name + 'TB.setTitleFont');
  Partner.DeleteAttributeValues(Name + '.setBorder');
  for var i:= 0 to FOldItems.Count - 1 do
    Partner.DeleteAttributeValues(RBName(i));
end;

procedure TJ2ButtonGroup.DeleteSelectedLabelMethod;
begin
  Partner.DeleteMethod(Name + 'BG_getSelectedButtonGroupLabel', false);
end;

function TJ2ButtonGroup.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Columns|Items|Title|Checkboxes|Frame|Font';
  Result:= Result + inherited getAttributes(ShowAttributes);
end;

procedure TJ2ButtonGroup.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Items') or (Attr = 'Checkboxes') or (Attr = 'Frame') then
    MakeButtonGroupItems
  else if (Attr = 'Title') or (Attr = 'Columns') or isFontAttribute(Attr) then
    setPositionAndSize
  else
    inherited;
end;

function TJ2ButtonGroup.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed|';
end;

procedure TJ2ButtonGroup.DeleteListener(const event: string);
  var EventMethodName, Listener: string;
begin
  EventMethodName:= MakeEventProcedureName(Event);
  Partner.DeleteMethod(EventMethodName);
  for var i:= 0 to FItems.Count - 1 do begin
    Listener:= RBName(i) + '.addActionListener((event) -> {' + EventMethodName + '(event);});';
    Partner.DeleteLambdaListener(Listener);
  end;
end;

procedure TJ2ButtonGroup.AddListener(const event: string);
  var EventMethodName, Listener: string; i: integer;
begin
  EventMethodName:= MakeEventProcedureName(Event);
  for i:= 0 to FItems.Count - 1 do begin
    Listener:= Indent2 + RBName(i) + '.addActionListener((event) -> {' + EventMethodName + '(event);});';
    Partner.InsertListener(RBName(i) + '.setBounds', Listener);
  end;
  if not Partner.hasText('public void ' + EventMethodName) then
    Partner.InsertProcedure(0, MakeEventProcedure(Event));
end;

function TJ2ButtonGroup.MakeEventProcedure(const Event: string): string;
begin
  Result:= Indent1 + 'public void ' + Name + '_ActionPerformed(ActionEvent evt) {' + CrLf +
           Indent2 + _(LNGTODO) + CrLf +
           Indent2 + 'System.out.println(((AbstractButton)evt.getSource()).getText());' + CrLf +
           Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result:= Result + ' // end of ' + Name + '_Action';
  Result:= Result + CrLf + CrLf;
end;

function TJ2ButtonGroup.RBName(i: integer): string;
begin
  Result:= Name + 'RB' + IntToStr(i);
end;

procedure TJ2ButtonGroup.MakeButtongroupItems;
  var i, p: integer; s, s1: string;
begin
  Partner.Editor.BeginUpdate;
  DeleteComponentAttributes;
  DeleteSelectedLabelMethod;
  s1:= '';
  if not FCheckboxes then begin
    s1:= s1 + surroundIndent('  private ButtonGroup ' + Name + 'BG = new ButtonGroup();');
    MakeSelectedLabelMethod;
  end;
  if FFrame then
    s1:= s1 + surroundIndent('  private TitledBorder ' + Name + 'TB = new TitledBorder(' + asString(FTitle) + ');');
  for i:= 0 to FItems.Count - 1 do begin
    s:= FItems[i];
    p:= Pos(', ' + _('selected'), s);
    if p > 0
      then s:= asString(copy(s, 1, p-1))
      else s:= asString(s);
    if FCheckboxes
      then s1:= s1 + surroundIndent('  private JCheckBox ' + RBName(i) +
                ' = new JCheckBox(' + s + ');')
      else s1:= s1 + surroundIndent('  private JRadioButton ' + RBName(i) +
                ' = new JRadioButton(' + s + ');');
  end;
  Partner.InsertAttribute(GetContainer, s1, false);
  setPositionAndSize;
  FOldItems.Text:= FItems.Text;
  Partner.Editor.EndUpdate;
end;

procedure TJ2ButtonGroup.MakeTitle;
  var s, key: string;
begin
  if FFrame then begin
    key:= Name + '.setBorder';
    setAttributValue(key, key + '(' + Name + 'TB);');
    if FontChanged and (FTitle <> '') then begin
      key:= Name + 'TB.setTitleFont';
      setAttributValue(key, key + '(new Font(' + asString(Font.Name) +
                       ', Font.BOLD, ' + IntToStr(PPIUnScale(Font.Size)) + '));');
    end;
    key:= 'private TitledBorder ' + Name + 'TB';
    s  := Indent2 + key + ' = new TitledBorder(' + asString(Title) + ');';
    Partner.ReplaceLine(key, s);
  end else
    Partner.DeleteAttributeValue(Name + '.setBorder(');
end;

procedure TJ2ButtonGroup.setPositionAndSize;
  const circle = 18;
  var col, row, ItemsInCol, line, x, y, dx, th: integer;
      RadioHeight, RadioWidth, ColWidth, RowHeight, ColWidthRest,
      RowHeightRest, LabelHeight: integer;
      xold, yold,ColWidthI, RowHeightI: integer;
      s, nam: string;
begin
  Partner.Editor.BeginUpdate;
  inherited;
  CanvasFontAssign;
  DeleteComponentValues;
  s:= '';
  th:= Canvas.TextHeight('Hg');
  dx:= Canvas.TextWidth('x');
  LabelHeight:= 0;
  if FFrame then begin
    RadioWidth:= Width - 2*dx;
    RadioHeight:= Height - th - dx;
    LabelHeight:= th;
  end else begin
    RadioWidth:= Width - 2*dx;
    RadioHeight:= Height - dx;
  end;

  if FItems.Count > 0 then begin
    ColWidth:= RadioWidth div FColumns;
    RowHeight:= RadioHeight div ItemsInColumn(1);
    line:= 0;
    xold:= 0;
    ColWidthRest:= RadioWidth mod FColumns;
    for col:= 0 to FColumns - 1 do begin
      if ColWidthRest > 0
        then ColWidthI:= ColWidth + 1
        else ColWidthI:= ColWidth;
      if col = 0
        then x:= dx
        else x:= xold + ColWidthI;
      dec(ColWidthRest);

      yold:= 0;
      ItemsInCol:= ItemsInColumn(col+1);
      RowHeightRest:= RadioHeight mod ItemsInColumn(1);
      for row:= 0 to ItemsInCol - 1 do begin
        if RowHeightRest > 0
          then RowHeightI:= RowHeight + 1
          else RowHeightI:= RowHeight;
        if row = 0
          then y:= LabelHeight + dx div 2
          else y:= yold + RowHeightI;
        dec(RowHeightRest);
        nam:= RBName(line);
        s:= s + surroundFix('  ' + nam + '.setBounds' + '(' + IntToStr(PPIUnScale(x)) + ', ' +
                  IntToStr(PPIUnScale(y)) + ', ' + IntToStr(PPIUnScale(ColWidthI)) + ', ' + IntToStr(PPIUnScale(RowHeightI)) + ');');
        if Pos(', ' + _('selected'), FItems[line]) > 0 then
          s:= s + surroundFix('  ' + nam + '.setSelected(true);');
        if ActionPerformed <> '' then
          s:= s + surroundFix('  ' + nam + '.addActionListener((event) -> {' + Name + '_ActionPerformed(event);});');
        if FontChanged then
          s:= s + surroundFix('  ' + nam + '.setFont(new Font(' + asString(Font.Name) +
               ', Font.BOLD, ' + IntToStr(PPIUnScale(Font.Size)) + '));');
        if not FCheckboxes then
          s:= s + surroundFix('  ' + Name + 'BG.add(' + nam + ');');
        s:= s + surroundFix('  ' + Name + '.add(' + nam + ');');
        inc(line);
        yold:= y;
      end;
      xold:= x;
    end;
  end;
  Partner.InsertAttributValue(Name, s, 1);
  MakeTitle;
  if ActionPerformed <> '' then begin
    if not Partner.hasText('public void ' + MakeEventProcedureName('actionPerformed')) then
      Partner.InsertProcedure(0, MakeEventProcedure('actionPerformed'));
  end;
  Partner.Editor.EndUpdate;
end;

procedure TJ2ButtonGroup.MakeFont;
begin
  setPositionAndSize;
end;

procedure TJ2ButtonGroup.Paint;
  const cRadius = 6;
  var ColumnWidth, RowHeight, RadioHeight, LabelHeight,
      col, row, yc, ItemsInCol, line, x, y, th, tw, p, Radius: integer;
      R: TRect; s: string;
begin
  FOldItems.Text:= FItems.Text;
  inherited;
  CanvasFontAssign;
  Canvas.Brush.Color:= clBtnFace;
  Canvas.FillRect(ClientRect);
  th:= Canvas.TextHeight('Hg');
  tw:= Canvas.TextWidth('x');
  LabelHeight:= 0;
  RadioHeight:= Height;
  R:= ClientRect;
  if FFrame then begin
    Canvas.Pen.Color:= $BABABA;
    R.Top:= th div 2;
    Canvas.Rectangle(R);
  end;
  R:= ClientRect;
  if (FTitle <> '') and FFrame then begin
    R.Top:= th div 2;
    LabelHeight:= th;
    RadioHeight:= Height - th;
    Canvas.Rectangle(R);
    Canvas.Textout(PPIScale(10), 0, FTitle);
  end;

  if FItems.Count > 0 then begin
    Radius:= PPIScale(cRadius);
    Canvas.Pen.Color:= $333333;
    ColumnWidth:= Width div FColumns;
    RowHeight:= RadioHeight div ItemsInColumn(1);
    line:= 0;
    for col:= 1 to FColumns do begin
      ItemsInCol:= ItemsInColumn(col);
      for row:= 1 to ItemsInCol do begin
        s:= FItems[line];
        p:= Pos(', ' + _('selected'), s);
        if p > 0 then
          s:= copy(s, 1, p-1);
        x:= tw + (col - 1)*ColumnWidth;
        y:= LabelHeight + 2 + (row - 1)*RowHeight;
        Canvas.Brush.Color:= clWhite;
        yc:= y + RowHeight div 2 - th div 2;
        if FCheckboxes then begin
          R:= Rect(x, yc + 0, x + 13, yc + 13);
          Canvas.Rectangle(R);
          if p > 0 then begin
            Canvas.Pen.Width:= 2;
            Canvas.MoveTo(x + 3, yc + 6);
            Canvas.LineTo(x + 4, yc + 9);
            Canvas.LineTo(x + 9, yc + 3);
            Canvas.Pen.Width:= 1;
          end;
        end else begin
          yc:= y + RowHeight div 2 - Radius;
          Canvas.Ellipse(x, yc, x + 2*Radius, yc + 2*Radius);
          if p > 0 then begin
            Canvas.Brush.Color:= clBlack;
            Canvas.Ellipse(x+3, yc+3, x + 2*Radius-3, yc + 2*Radius-3);
            Canvas.Brush.Color:= clWhite;
          end;
        end;
        Canvas.Brush.Color:= clBtnFace;
        yc:= y + RowHeight div 2 - th div 2;
        R:= Rect(x + PPIScale(19), yc, col*ColumnWidth, yc + RowHeight);
        Canvas.TextRect(R, s);
        inc(line);
      end;
    end;
  end;
end;

procedure TJ2ButtonGroup.NewControl;
begin
  InsertImport('javax.swing.border.TitledBorder');
  InsertNewVariable('private JPanel ' + Name + ' = new JPanel();');
  var s:= surroundFix('  ' + Name + '.setLayout(null);') +
          surroundFix('  ' + GetContainer + '.add(' + Name + ');');
  Partner.InsertAttributValue(Name, s, 1);
  MakeButtongroupItems;
end;

procedure TJ2ButtonGroup.setItems(Value: TStrings);
begin
  if FItems.Text <> Value.Text then begin
    FOldItems.Text:= FItems.Text;
    FItems.Text:= Value.Text;
    Invalidate;
  end;
end;

function TJ2ButtonGroup.ItemsInColumn(i: integer): integer;
  var quot, rest: integer;
begin
  quot:= FItems.Count div FColumns;
  rest:= FItems.Count mod FColumns;
  if i <= rest
    then Result:= quot + 1
    else Result:= quot;
end;

procedure TJ2ButtonGroup.setColumns(Value: integer);
begin
  if (FColumns <> Value) and (Value > 0) then begin
    FColumns:= Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.setTitle(Value: string);
begin
  if FTitle <> Value then begin
    FTitle:= Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.setCheckboxes(Value: boolean);
begin
  if FCheckboxes <> Value then begin
    FCheckboxes:= Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.setFrame(Value: boolean);
begin
  if FFrame <> Value then begin
    FFrame:= Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.MakeSelectedLabelMethod;
  var s: string;
begin
  s:= 'public String ' + Name + 'BG_getSelectedButtonGroupLabel)';
  if Partner.getLineNumberWith(s) = -1 then begin
    s:= surroundFix('public String ' + Name + 'BG_getSelectedButtonGroupLabel() {') +
        surroundFix('  for (java.util.Enumeration<AbstractButton> e = ' + Name + 'BG.getElements(); e.hasMoreElements();) {') +
        surroundFix('    AbstractButton b = e.nextElement();')+
        surroundFix('    if (b.isSelected()) return b.getText();') +
        surroundFix('  }') +
        surroundFix('  return "";') +
        surroundFix('}') + CrLf;
    Partner.InsertProcedure(s);
  end;
end;

procedure TJ2ButtonGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'BG_getSelectedButtonGroupLabel',
                      NewName + 'BG_getSelectedButtonGroupLabel', false);
end;

end.
