unit UACheckboxRadio;

{ Classes
  TACheckBox = class (TAWTComponent)
    TARadioButton
  TACheckboxGroup = class (TAWTComponent)   deprecated
  TA2ButtonGroup = class(TAWTComponent)
}

interface

uses
  Classes, StdCtrls, UAComponents;

type

  TACheckBox = class (TAWTComponent)
  private
    FText: string;
    FState: Boolean;
    procedure setState(aValue: boolean);
    procedure setText(const aValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aCheckbox: TCheckbox);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property State: boolean read FState write setState;
    property Text: string read FText write setText;
    property itemStateChanged;
  end;

  // deprecated
  TARadioButton = class (TACheckBox)
  private
    FCheckboxGroup: string;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aRadioButton: TRadioButton); overload;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property CheckboxGroup: string read FCheckboxGroup write FCheckboxGroup;
  end;

  //deprecated
  TACheckboxGroup = class (TAWTComponent)
  public
    constructor Create (AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  end;

  TA2ButtonGroup = class (TAWTComponent)
  private
    FCheckboxes: boolean;
    FColumns: integer;
    FItems: TStrings;
    FOldItems: TStrings;
    procedure setItems(Value: TStrings);
    procedure setColumns(Value: integer);
    procedure setCheckboxes(Value: boolean);
    procedure MakeButtongroupItems;
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
    function MakeEventProcedure(const Event: string ): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure SetPositionAndSize; override;
  published
    property Items: TStrings read fItems write setItems; // must stay before columns or label
    property Columns: integer read FColumns write setColumns;
    property Checkboxes: boolean read FCheckboxes write setCheckboxes;
    property itemStateChanged;
  end;

implementation

uses Windows, SysUtils, Graphics, Controls, UITypes, JvGnugettext,
     UStringRessources, UConfiguration, UJava, UGUIDesigner;

{--- TACheckBox ---------------------------------------------------------------}

constructor TACheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= -5;
  Height:= 24;
  Width:= 80;
  FState := False;
  Background:= clBtnFace;
  Text:= 'Checkbox';
  JavaType:= 'Checkbox';
end;

constructor TACheckBox.CreateFrom(aCheckbox: TCheckbox);
begin
  Create(aCheckbox.Owner);
  CreateFromA(aCheckBox);
  Text:= aCheckbox.Caption; // Label
  Font:= aCheckbox.Font;
  Foreground:= Font.Color;
  Background:= aCheckbox.Color;
  if Background = clBtnFace then Background:= clWhite;
  State:= aCheckbox.Checked;
end;

function TACheckBox.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Text|State' + inherited getAttributes(ShowAttributes);
end;

procedure TACheckBox.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    inherited setAttribute('Label', Value, Typ)
  else
    inherited;
end;

function TACheckBox.getEvents(ShowEvents: integer): string;
begin
  Result:= '|itemStateChanged' + inherited getEvents(ShowEvents);
end;

procedure TACheckBox.NewControl;
begin
  FText:= 'Checkbox';
  DefaultComponent;
  InsertNewVariable('private Checkbox ' + Name + ' = new Checkbox();');
  MakeAttribut('Label', asString(Text));
end;

procedure TACheckBox.Paint;
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
     if FState then inc(Result);
   end;

begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  th:= Canvas.TextHeight('Q');

  // write the text
  Canvas.Pen.Color:= Font.Color;
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TWinControl).Brush.Color
    else Canvas.Brush.Color:= Background;
  Canvas.FillRect(Self.GetClientRect);
  if Tag > 0 then x:= BoxW + 8 else x:= BoxW + 4;
  x:= PPIScale(x);
  y:= (Height - th) div 2;
  Canvas.TextOut(x, y, FText);

  // paint the picture
  if Tag > 0 then x:= PPIScale(4) else x:= 0;
  y:= (Height - PPIScale(BoxH)) div 2;
  FGUIDesigner.vilControls1315.Draw(Canvas, x, y, TagToPicNr);
end;

procedure TACheckBox.setState(aValue: Boolean);
begin
  if FState <> aValue then begin
    FState := aValue;
    Invalidate;
  end;
end;

procedure TACheckBox.setText(const aValue: string);
begin
  if FText <> aValue then begin
    FText:= aValue;
    Invalidate;
  end;
end;

{--- TARadioButton ------------------------------------------------------------}

constructor TARadioButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= -6;
  Width:= 80;
  JavaType:= 'Checkbox';
end;

constructor TARadioButton.CreateFrom(aRadioButton: TRadioButton);
begin
  Create(aRadioButton.Owner);
  CreateFromA(aRadioButton);
  Text:= aRadioButton.Caption;
  Font:= aRadioButton.Font;
  Foreground:= Font.Color;
  Background:= aRadioButton.Color;
  if Background = clBtnFace then Background:= clWhite;
  CheckboxGroup:= aRadioButton.HelpKeyWord;
  State:= aRadioButton.Checked;
end;

function TARadioButton.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|CheckboxGroup' + inherited getAttributes(ShowAttributes);
end;

procedure TARadioButton.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'CheckboxGroup' then
    MakeAttribut(Attr, Value)
  else
    inherited;
end;

{--- CheckboxGroup ------------------------------------------------------------}

constructor TACheckboxGroup.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= -7;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'CheckboxGroup';
end;

function TACheckboxGroup.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Name';
end;

function TACheckboxGroup.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TACheckboxGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_getSelectedButtonGroupLabel',
                      NewName + '_getSelectedButtonGroupLabel', false);
end;

procedure TACheckboxGroup.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_getSelectedRadioButtonLabel', false);
end;

procedure TACheckboxGroup.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 6, 4, 7);
end;

{--- TA2ButtonGroup -----------------------------------------------------------}

constructor TA2ButtonGroup.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= -50;
  Width:= 120;
  Height:= 80;
  FColumns:= 1;
  FItems:= TStringList.Create;
  FItems.Text:= _('America') + ', ' + _('selected') + #13#10 + _('Europe') + #13#10 + _('Asia');
  FOldItems:= TStringList.Create;
  FCheckboxes:= false;
  ItemStateChanged:= '';
  JavaType:= 'ButtonGroup';
end;

destructor TA2ButtonGroup.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FOldItems);
  inherited;
end;

procedure TA2ButtonGroup.DeleteComponent;
begin
  inherited;
  DeleteComponentAttributes;
  DeleteComponentValues;
  Partner.DeleteAttribute('private Panel ' + Name);
  Partner.DeleteAttributeValue(Name + '.setLayout');
  Partner.DeleteAttributeValue(GetContainer + '.add(' + Name + ')');
  DeleteSelectedLabelMethod;
end;

procedure TA2ButtonGroup.DeleteComponentAttributes;
begin
  Partner.DeleteAttribute(Name + 'BG');
  for var i:= 0 to FOldItems.Count - 1 do
    Partner.DeleteAttribute(RBName(i));
end;

procedure TA2ButtonGroup.DeleteComponentValues;
begin
  for var i:= 0 to FOldItems.Count - 1 do
    Partner.DeleteAttributeValues(RBName(i));
  Partner.DeleteAttributeValues(Name + '.setBorder');
end;

function TA2ButtonGroup.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Columns|Items|Checkboxes|Font';
  Result:= Result + inherited getAttributes(ShowAttributes);
end;

procedure TA2ButtonGroup.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Items') or (Attr = 'Checkboxes') then
    MakeButtonGroupItems
  else if (Attr = 'Columns') or isFontAttribute(Attr) then
    setPositionAndSize
  else
    inherited;
end;

function TA2ButtonGroup.getEvents(ShowEvents: integer): string;
begin
  Result:= '|itemStateChanged|';
end;

procedure TA2ButtonGroup.DeleteListener(const event: string);
  var EventMethodName, Listener: string;
begin
  EventMethodName:= MakeEventProcedureName(Event);
  Partner.DeleteMethod(EventMethodName);
  for var i:= 0 to FItems.Count - 1 do begin
    Listener:= RBName(i) + '.addItemListener((event) -> {' + EventMethodName + '(event);});';
    Partner.DeleteLambdaListener(Listener);
  end;
end;

procedure TA2ButtonGroup.AddListener(const event: string);
  var EventMethodName, Listener: string; i: integer;
begin
  EventMethodName:= MakeEventProcedureName(Event);
  for i:= 0 to FItems.Count - 1 do begin
    Listener:= Indent2 + RBName(i) + '.addItemListener((event) -> {' + EventMethodName + '(event);});';
    Partner.InsertListener(RBName(i) + '.setBounds', Listener);
  end;
  if not Partner.hasText('public void ' + EventMethodName) then
    Partner.InsertProcedure(0, MakeEventProcedure(Event));
end;

function TA2ButtonGroup.MakeEventProcedure(const Event: string): string;
begin
  Result:= Indent1 + 'public void ' + Name + '_ItemStateChanged(ItemEvent evt) {' + CrLf +
           Indent2 + _(LNGToDo) + CrLf +
           Indent2 + 'System.out.println(evt.getItem());' + CrLf +
           Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result:= Result + ' // end of ' + Name + '_ItemStateChanged';
  Result:= Result + CrLf + CrLf;
end;

function TA2ButtonGroup.RBName(i: integer): string;
begin
  Result:= Name + 'RB' + IntToStr(i);
end;

procedure TA2ButtonGroup.MakeButtongroupItems;
  var i, p: integer; s, s1: string;
begin
  Partner.Editor.BeginUpdate;
  DeleteComponentAttributes;
  DeleteSelectedLabelMethod;
  s1:= '';
  if not FCheckboxes then begin
    s1:= s1 + surroundIndent('  private CheckboxGroup ' + Name +
         'BG = new CheckboxGroup();');
    MakeSelectedLabelMethod;
  end;
  for i:= 0 to FItems.Count - 1 do begin
    s:= FItems[i];
    p:= Pos(', selected', s);
    if p > 0
      then s:= asString(copy(s, 1, p-1))
      else s:= asString(s);
    s1:= s1 + surroundIndent('  private Checkbox ' + RBName(i) +
         ' = new Checkbox(' + s + ');');
  end;
  Partner.InsertAttribute(GetContainer, s1, false);
  setPositionAndSize;
  FOldItems.Text:= FItems.Text;
  Partner.Editor.EndUpdate;
end;

procedure TA2ButtonGroup.setPositionAndSize;
  const circle = 18;
  var col, row, ItemsInCol, line, x, y, dx, dy: integer;
      RadioHeight, RadioWidth, ColWidth, RowHeight, ColWidthRest, RowHeightRest: integer;
      xold, yold,ColWidthI, RowHeightI: integer;
      s, nam: string;
begin
  Partner.Editor.BeginUpdate;
  inherited;
  CanvasFontAssign;
  DeleteComponentValues;
  s:= '';
  dx:= Canvas.TextWidth('x');
  RadioWidth:= Width - 2*dx;
  RadioHeight:= Height - 2*dx;
  dy:= dx;

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
          then y:= dy
          else y:= yold + RowHeightI;
        dec(RowHeightRest);
        nam:= RBName(line);
        s:= s + surroundFix('  ' + nam + '.setBounds' + '(' + IntToStr(PPIUnScale(x)) + ', ' +
                  IntToStr(PPIUnScale(y)) + ', ' + IntToStr(PPIUnScale(ColWidthI)) + ', ' + IntToStr(PPIUnScale(RowHeightI)) + ');');
        if Pos(', selected', FItems[line]) > 0 then
          s:= s + surroundFix('  ' + nam + '.setState(true);');
        if ItemStateChanged <> '' then
          s:= s + surroundFix('  ' + nam + '.addItemListener((event) -> {' + Name + '_ItemStateChanged(event);});');
        if FontChanged then
          s:= s + surroundFix('  ' + nam + '.setFont(new Font(' + asString(Font.Name) +
                    ', Font.PLAIN, ' + IntToStr(PPIUnScale(Font.Size)) + '));');
        if not FCheckboxes then
          s:= s + surroundFix('  ' + nam + '.setCheckboxGroup(' + Name + 'BG);');
        s:= s + surroundFix('  ' + Name + '.add(' + nam + ');');
        inc(line);
        yold:= y;
      end;
      xold:= x;
    end;
  end;
  Partner.InsertAttributValue(Name, s, 1);
  if ItemStateChanged <> '' then begin
    if not Partner.hasText('public void ' + MakeEventProcedureName('itemStateChanged')) then
      Partner.InsertProcedure(0, MakeEventProcedure('itemStateChanged'));
  end;
  Partner.Editor.EndUpdate;
end;

procedure TA2ButtonGroup.NewControl;
begin
  InsertNewVariable('private Panel ' + Name + ' = new Panel();');
  var s:= surroundFix('  ' + Name + '.setLayout(null);') +
          surroundFix('  ' + GetContainer + '.add(' + Name + ');');
  Partner.InsertAttributValue(Name, s, 1);
  MakeButtongroupItems;
end;

procedure TA2ButtonGroup.setItems(Value: TStrings);
begin
  if FItems.Text <> Value.Text then begin
    FOldItems.Text:= FItems.Text;
    FItems.Text:= Value.Text;
    Invalidate;
  end;
end;

procedure TA2ButtonGroup.setColumns(Value: integer);
begin
  if (FColumns <> Value) and (Value > 0) then begin
    FColumns:= Value;
    Invalidate;
  end;
end;

procedure TA2ButtonGroup.setCheckboxes(Value: boolean);
begin
  if FCheckboxes <> Value then begin
    FCheckboxes:= Value;
    Invalidate;
  end;
end;

function TA2ButtonGroup.ItemsInColumn(i: integer): integer;
  var quot, rest: integer;
begin
  quot:= FItems.Count div FColumns;
  rest:= FItems.Count mod FColumns;
  if i <= rest
    then Result:= quot + 1
    else Result:= quot;
end;

procedure TA2ButtonGroup.Paint;
  const cRadius = 6;
  var ColumnWidth, RowWidth, RadioHeight, LabelHeight,
      col, row, yc, ItemsInCol, line, x, y, th, tw, p, Radius: integer;
      R: TRect; s: string;
begin
  FOldItems.Text:= FItems.Text;
  inherited;
  CanvasFontAssign;
  Canvas.Brush.Color:= clWhite;
  Canvas.FillRect(ClientRect);
  th:= Canvas.TextHeight('Hg');
  tw:= Canvas.TextWidth('x');
  LabelHeight:= 0;
  RadioHeight:= Height;

  if FItems.Count > 0 then begin
    Radius:= PPIScale(cRadius);
    Canvas.Pen.Color:= $333333;
    ColumnWidth:= Width div FColumns;
    RowWidth:= RadioHeight div ItemsInColumn(1);
    line:= 0;
    for col:= 1 to FColumns do begin
      ItemsInCol:= ItemsInColumn(col);
      for row:= 1 to ItemsInCol do begin
        s:= FItems[line];
        p:= Pos(', selected', s);
        if p > 0 then
          s:= copy(s, 1, p-1);
        x:= tw + (col - 1)*ColumnWidth;
        y:= LabelHeight + 2 + (row - 1)*RowWidth;
        Canvas.Brush.Color:= clWhite;
        yc:= y + RowWidth div 2 - th div 2;
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
          yc:= y + RowWidth div 2 - Radius;
          Canvas.Ellipse(x, yc, x + 2*Radius, yc + 2*Radius);
          if p > 0 then begin
            Canvas.Brush.Color:= clBlack;
            Canvas.Ellipse(x+3, yc+3, x + 2*Radius-3, yc + 2*Radius-3);
            Canvas.Brush.Color:= clWhite;
          end;
        end;
        yc:= y + RowWidth div 2 - th div 2;
        R:= Rect(x + PPIScale(19), yc, col*ColumnWidth, yc + RowWidth);
        Canvas.TextRect(R, s);
        inc(line);
      end;
    end;
  end;
end;

procedure TA2ButtonGroup.MakeSelectedLabelMethod;
  var s: string;
begin
  s:= 'public String ' + Name + 'BG_getSelectedButtonGroupLabel)';
  if Partner.getLineNumberWith(s) = -1 then begin
    s:= surroundFix('public String ' + Name + 'BG_getSelectedButtonGroupLabel() {') +
        surroundFix('  Checkbox cb = ' + Name + 'BG.getSelectedCheckbox();') +
        surroundFix('  if (cb != null) return cb.getLabel();')+
        surroundFix('  return "";') +
        surroundFix('}') + CrLf;
    Partner.InsertProcedure(s);
  end;
end;

procedure TA2ButtonGroup.DeleteSelectedLabelMethod;
begin
  Partner.DeleteMethod(Name + 'BG_getSelectedButtonGroupLabel', false);
end;

procedure TA2ButtonGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'BG_getSelectedButtonGroupLabel',
                      NewName + 'BG_getSelectedButtonGroupLabel', false);
end;

end.
