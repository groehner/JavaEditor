unit UACheckboxRadio;

{ Classes
  TACheckBox = class (TAWTComponent)
  TARadioButton
  TACheckboxGroup = class (TAWTComponent)   deprecated
  TA2ButtonGroup = class(TAWTComponent)
}

interface

uses
  Classes,
  StdCtrls,
  UAComponents;

type

  TACheckBox = class(TAWTComponent)
  private
    FText: string;
    FState: Boolean;
    procedure SetState(AValue: Boolean);
    procedure SetText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property State: Boolean read FState write SetState;
    property Text: string read FText write SetText;
    property itemStateChanged;
  end;

  // deprecated
  TARadioButton = class(TACheckBox)
  private
    FCheckboxGroup: string;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property CheckboxGroup: string read FCheckboxGroup write FCheckboxGroup;
  end;

  // deprecated
  TACheckboxGroup = class(TAWTComponent)
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  end;

  TA2ButtonGroup = class(TAWTComponent)
  private
    FCheckboxes: Boolean;
    FColumns: Integer;
    FItems: TStrings;
    FOldItems: TStrings;
    procedure SetItems(Value: TStrings);
    procedure SetColumns(Value: Integer);
    procedure SetCheckboxes(Value: Boolean);
    procedure MakeButtongroupItems;
    function ItemsInColumn(Int: Integer): Integer;
    function RBName(Int: Integer): string;
    procedure DeleteComponentAttributes;
    procedure DeleteComponentValues;
    procedure MakeSelectedLabelMethod;
    procedure DeleteSelectedLabelMethod;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteComponent; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure DeleteListener(const Event: string); override;
    procedure AddListener(const Event: string); override;
    function MakeEventProcedure(const Event: string): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure SetPositionAndSize; override;
  published
    property Items: TStrings read FItems write SetItems;
    // must stay before columns or label
    property Columns: Integer read FColumns write SetColumns;
    property Checkboxes: Boolean read FCheckboxes write SetCheckboxes;
    property itemStateChanged;
  end;

implementation

uses
  Windows,
  SysUtils,
  Graphics,
  Controls,
  UITypes,
  JvGnugettext,
  UStringRessources,
  UConfiguration,
  UJava,
  UGUIDesigner;

{ --- TACheckBox --------------------------------------------------------------- }

constructor TACheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -5;
  Height := 24;
  Width := 80;
  FState := False;
  Background := clBtnFace;
  Text := 'Checkbox';
  JavaType := 'Checkbox';
end;

function TACheckBox.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Text|State' + inherited GetAttributes(ShowAttributes);
end;

procedure TACheckBox.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Text' then
    inherited SetAttribute('Label', Value, Typ)
  else
    inherited;
end;

function TACheckBox.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|itemStateChanged' + inherited GetEvents(ShowEvents);
end;

procedure TACheckBox.NewControl;
begin
  FText := 'Checkbox';
  DefaultComponent;
  InsertNewVariable('private Checkbox ' + Name + ' = new Checkbox();');
  MakeAttribut('Label', AsString(Text));
end;

procedure TACheckBox.Paint;
const
  BoxW = 13; // Width of rectangular checkbox
  BoxH = 15;

var
  TextH, XPos, YPos: Integer;

  function TagToPicNr: Integer;
  begin
    case Tag of
      - 5:
        Result := 0;
      +5:
        Result := 2;
      -6:
        Result := 4;
      +6:
        Result := 6;
    else
      Result := 0;
    end;
    if FState then
      Inc(Result);
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  TextH := Canvas.TextHeight('Q');

  // write the text
  Canvas.Pen.Color := Font.Color;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Canvas.FillRect(Self.GetClientRect);
  if Tag > 0 then
    XPos := BoxW + 8
  else
    XPos := BoxW + 4;
  XPos := PPIScale(XPos);
  YPos := (Height - TextH) div 2;
  Canvas.TextOut(XPos, YPos, FText);

  // paint the picture
  if Tag > 0 then
    XPos := PPIScale(4)
  else
    XPos := 0;
  YPos := (Height - PPIScale(BoxH)) div 2;
  FGUIDesigner.vilControls1315.Draw(Canvas, XPos, YPos, TagToPicNr);
end;

procedure TACheckBox.SetState(AValue: Boolean);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    Invalidate;
  end;
end;

procedure TACheckBox.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

{ --- TARadioButton ------------------------------------------------------------ }

constructor TARadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -6;
  Width := 80;
  JavaType := 'Checkbox';
end;

function TARadioButton.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|CheckboxGroup' + inherited GetAttributes(ShowAttributes);
end;

procedure TARadioButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'CheckboxGroup' then
    MakeAttribut(Attr, Value)
  else
    inherited;
end;

{ --- CheckboxGroup ------------------------------------------------------------ }

constructor TACheckboxGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -7;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'CheckboxGroup';
end;

function TACheckboxGroup.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name';
end;

function TACheckboxGroup.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TACheckboxGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_getSelectedButtonGroupLabel',
    NewName + '_getSelectedButtonGroupLabel', False);
end;

procedure TACheckboxGroup.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_getSelectedRadioButtonLabel', False);
end;

procedure TACheckboxGroup.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 6, 4, 7);
end;

{ --- TA2ButtonGroup ----------------------------------------------------------- }

constructor TA2ButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := -50;
  Width := 120;
  Height := 80;
  FColumns := 1;
  FItems := TStringList.Create;
  FItems.Text := _('America') + ', ' + _('selected') + #13#10 + _('Europe') +
    #13#10 + _('Asia');
  FOldItems := TStringList.Create;
  FCheckboxes := False;
  itemStateChanged := '';
  JavaType := 'ButtonGroup';
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
  FPartner.DeleteAttribute('private Panel ' + Name);
  FPartner.DeleteAttributeValue(Name + '.setLayout');
  FPartner.DeleteAttributeValue(GetContainer + '.add(' + Name + ')');
  DeleteSelectedLabelMethod;
end;

procedure TA2ButtonGroup.DeleteComponentAttributes;
begin
  FPartner.DeleteAttribute(Name + 'BG');
  for var I := 0 to FOldItems.Count - 1 do
    FPartner.DeleteAttribute(RBName(I));
end;

procedure TA2ButtonGroup.DeleteComponentValues;
begin
  for var I := 0 to FOldItems.Count - 1 do
    FPartner.DeleteAttributeValues(RBName(I));
  FPartner.DeleteAttributeValues(Name + '.setBorder');
end;

function TA2ButtonGroup.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Columns|Items|Checkboxes|Font';
  Result := Result + inherited GetAttributes(ShowAttributes);
end;

procedure TA2ButtonGroup.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Items') or (Attr = 'Checkboxes') then
    MakeButtongroupItems
  else if (Attr = 'Columns') or isFontAttribute(Attr) then
    SetPositionAndSize
  else
    inherited;
end;

function TA2ButtonGroup.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|itemStateChanged|';
end;

procedure TA2ButtonGroup.DeleteListener(const Event: string);
var
  EventMethodName, Listener: string;
begin
  EventMethodName := MakeEventProcedureName(Event);
  FPartner.DeleteMethod(EventMethodName);
  for var I := 0 to FItems.Count - 1 do
  begin
    Listener := RBName(I) + '.addItemListener((event) -> {' + EventMethodName
      + '(event);});';
    FPartner.DeleteLambdaListener(Listener);
  end;
end;

procedure TA2ButtonGroup.AddListener(const Event: string);
var
  EventMethodName, Listener: string;
begin
  EventMethodName := MakeEventProcedureName(Event);
  for var I := 0 to FItems.Count - 1 do
  begin
    Listener := Indent2 + RBName(I) + '.addItemListener((event) -> {' +
      EventMethodName + '(event);});';
    FPartner.InsertListener(RBName(I) + '.setBounds', Listener);
  end;
  if not FPartner.HasText('public void ' + EventMethodName) then
    FPartner.InsertProcedure(0, MakeEventProcedure(Event));
end;

function TA2ButtonGroup.MakeEventProcedure(const Event: string): string;
begin
  Result := Indent1 + 'public void ' + Name +
    '_ItemStateChanged(ItemEvent evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf +
    Indent2 + 'System.out.println(evt.getItem());' + CrLf + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result := Result + ' // end of ' + Name + '_ItemStateChanged';
  Result := Result + CrLf + CrLf;
end;

function TA2ButtonGroup.RBName(Int: Integer): string;
begin
  Result := Name + 'RB' + IntToStr(Int);
end;

procedure TA2ButtonGroup.MakeButtongroupItems;
var
  Posi: Integer;
  Str, Str1: string;
begin
  FPartner.Editor.BeginUpdate;
  DeleteComponentAttributes;
  DeleteSelectedLabelMethod;
  Str1 := '';
  if not FCheckboxes then
  begin
    Str1 := Str1 + surroundIndent('  private CheckboxGroup ' + Name +
      'BG = new CheckboxGroup();');
    MakeSelectedLabelMethod;
  end;
  for var I := 0 to FItems.Count - 1 do
  begin
    Str := FItems[I];
    Posi := Pos(', ' + _('selected'), Str);
    if Posi > 0 then
      Str := AsString(Copy(Str, 1, Posi - 1))
    else
      Str := AsString(Str);
    Str1 := Str1 + surroundIndent('  private Checkbox ' + RBName(I) +
      ' = new Checkbox(' + Str + ');');
  end;
  FPartner.InsertAttribute(GetContainer, Str1, False);
  SetPositionAndSize;
  FOldItems.Text := FItems.Text;
  FPartner.Editor.EndUpdate;
end;

procedure TA2ButtonGroup.SetPositionAndSize;
const
  Circle = 18;
var
  Col, Row, ItemsInCol, Line, XPos, YPos, DeltaX, DeltaY: Integer;
  RadioHeight, RadioWidth, ColWidth, RowHeight, ColWidthRest,
    RowHeightRest: Integer;
  XOld, YOld, ColWidthI, RowHeightI: Integer;
  Str, Nam: string;
begin
  FPartner.Editor.BeginUpdate;
  inherited;
  CanvasFontAssign;
  DeleteComponentValues;
  Str := '';
  DeltaX := Canvas.TextWidth('XPos');
  RadioWidth := Width - 2 * DeltaX;
  RadioHeight := Height - 2 * DeltaX;
  DeltaY := DeltaX;

  if FItems.Count > 0 then
  begin
    ColWidth := RadioWidth div FColumns;
    RowHeight := RadioHeight div ItemsInColumn(1);
    Line := 0;
    XOld := 0;
    ColWidthRest := RadioWidth mod FColumns;
    for Col := 0 to FColumns - 1 do
    begin
      if ColWidthRest > 0 then
        ColWidthI := ColWidth + 1
      else
        ColWidthI := ColWidth;
      if Col = 0 then
        XPos := DeltaX
      else
        XPos := XOld + ColWidthI;
      Dec(ColWidthRest);

      YOld := 0;
      ItemsInCol := ItemsInColumn(Col + 1);
      RowHeightRest := RadioHeight mod ItemsInColumn(1);
      for Row := 0 to ItemsInCol - 1 do
      begin
        if RowHeightRest > 0 then
          RowHeightI := RowHeight + 1
        else
          RowHeightI := RowHeight;
        if Row = 0 then
          YPos := DeltaY
        else
          YPos := YOld + RowHeightI;
        Dec(RowHeightRest);
        Nam := RBName(Line);
        Str := Str + surroundFix('  ' + Nam + '.setBounds' + '(' +
          IntToStr(PPIUnScale(XPos)) + ', ' + IntToStr(PPIUnScale(YPos)) + ', '
          + IntToStr(PPIUnScale(ColWidthI)) + ', ' +
          IntToStr(PPIUnScale(RowHeightI)) + ');');
        if Pos(', ' + _('selected'), FItems[Line]) > 0 then
          Str := Str + surroundFix('  ' + Nam + '.setState(true);');
        if itemStateChanged <> '' then
          Str := Str + surroundFix('  ' + Nam + '.addItemListener((event) -> {'
            + Name + '_ItemStateChanged(event);});');
        if FontChanged then
          Str := Str + surroundFix('  ' + Nam + '.setFont(new Font(' +
            AsString(Font.Name) + ', Font.PLAIN, ' +
            IntToStr(PPIUnScale(Font.Size)) + '));');
        if not FCheckboxes then
          Str := Str + surroundFix('  ' + Nam + '.setCheckboxGroup(' + Name
            + 'BG);');
        Str := Str + surroundFix('  ' + Name + '.add(' + Nam + ');');
        Inc(Line);
        YOld := YPos;
      end;
      XOld := XPos;
    end;
  end;
  FPartner.InsertAttributValue(Name, Str, 1);
  if itemStateChanged <> '' then
  begin
    if not FPartner.HasText('public void ' + MakeEventProcedureName
      ('itemStateChanged')) then
      FPartner.InsertProcedure(0, MakeEventProcedure('itemStateChanged'));
  end;
  FPartner.Editor.EndUpdate;
end;

procedure TA2ButtonGroup.NewControl;
begin
  InsertNewVariable('private Panel ' + Name + ' = new Panel();');
  var
  Str := surroundFix('  ' + Name + '.setLayout(null);') +
    surroundFix('  ' + GetContainer + '.add(' + Name + ');');
  FPartner.InsertAttributValue(Name, Str, 1);
  MakeButtongroupItems;
end;

procedure TA2ButtonGroup.SetItems(Value: TStrings);
begin
  if FItems.Text <> Value.Text then
  begin
    FOldItems.Text := FItems.Text;
    FItems.Text := Value.Text;
    Invalidate;
  end;
end;

procedure TA2ButtonGroup.SetColumns(Value: Integer);
begin
  if (FColumns <> Value) and (Value > 0) then
  begin
    FColumns := Value;
    Invalidate;
  end;
end;

procedure TA2ButtonGroup.SetCheckboxes(Value: Boolean);
begin
  if FCheckboxes <> Value then
  begin
    FCheckboxes := Value;
    Invalidate;
  end;
end;

function TA2ButtonGroup.ItemsInColumn(Int: Integer): Integer;
var
  Quot, Rest: Integer;
begin
  Quot := FItems.Count div FColumns;
  Rest := FItems.Count mod FColumns;
  if Int <= Rest then
    Result := Quot + 1
  else
    Result := Quot;
end;

procedure TA2ButtonGroup.Paint;
const
  CRadius = 6;
var
  ColumnWidth, RowWidth, RadioHeight, LabelHeight, Col, Row, YCenter,
    ItemsInCol, Line, XPos, YPos, TextH, TextW, Posi, Radius: Integer;
  ARect: TRect;
  Str: string;
begin
  FOldItems.Text := FItems.Text;
  inherited;
  CanvasFontAssign;
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ClientRect);
  TextH := Canvas.TextHeight('Hg');
  TextW := Canvas.TextWidth('XPos');
  LabelHeight := 0;
  RadioHeight := Height;

  if FItems.Count > 0 then
  begin
    Radius := PPIScale(CRadius);
    Canvas.Pen.Color := $333333;
    ColumnWidth := Width div FColumns;
    RowWidth := RadioHeight div ItemsInColumn(1);
    Line := 0;
    for Col := 1 to FColumns do
    begin
      ItemsInCol := ItemsInColumn(Col);
      for Row := 1 to ItemsInCol do
      begin
        Str := FItems[Line];
        Posi := Pos(', ' + _('selected'), Str);
        if Posi > 0 then
          Str := Copy(Str, 1, Posi - 1);
        XPos := TextW + (Col - 1) * ColumnWidth;
        YPos := LabelHeight + 2 + (Row - 1) * RowWidth;
        Canvas.Brush.Color := clWhite;
        YCenter := YPos + RowWidth div 2 - TextH div 2;
        if FCheckboxes then
        begin
          ARect := Rect(XPos, YCenter + 0, XPos + 13, YCenter + 13);
          Canvas.Rectangle(ARect);
          if Posi > 0 then
          begin
            Canvas.Pen.Width := 2;
            Canvas.MoveTo(XPos + 3, YCenter + 6);
            Canvas.LineTo(XPos + 4, YCenter + 9);
            Canvas.LineTo(XPos + 9, YCenter + 3);
            Canvas.Pen.Width := 1;
          end;
        end
        else
        begin
          YCenter := YPos + RowWidth div 2 - Radius;
          Canvas.Ellipse(XPos, YCenter, XPos + 2 * Radius,
            YCenter + 2 * Radius);
          if Posi > 0 then
          begin
            Canvas.Brush.Color := clBlack;
            Canvas.Ellipse(XPos + 3, YCenter + 3, XPos + 2 * Radius - 3,
              YCenter + 2 * Radius - 3);
            Canvas.Brush.Color := clWhite;
          end;
        end;
        YCenter := YPos + RowWidth div 2 - TextH div 2;
        ARect := Rect(XPos + PPIScale(19), YCenter, Col * ColumnWidth,
          YCenter + RowWidth);
        Canvas.TextRect(ARect, Str);
        Inc(Line);
      end;
    end;
  end;
end;

procedure TA2ButtonGroup.MakeSelectedLabelMethod;
var
  Str: string;
begin
  Str := 'public String ' + Name + 'BG_getSelectedButtonGroupLabel)';
  if FPartner.GetLineNumberWith(Str) = -1 then
  begin
    Str := surroundFix('public String ' + Name +
      'BG_getSelectedButtonGroupLabel() {') +
      surroundFix('  Checkbox cb = ' + Name + 'BG.getSelectedCheckbox();') +
      surroundFix('  if (cb != null) return cb.getLabel();') +
      surroundFix('  return "";') + surroundFix('}') + CrLf;
    FPartner.InsertProcedure(Str);
  end;
end;

procedure TA2ButtonGroup.DeleteSelectedLabelMethod;
begin
  FPartner.DeleteMethod(Name + 'BG_getSelectedButtonGroupLabel', False);
end;

procedure TA2ButtonGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'BG_getSelectedButtonGroupLabel',
    NewName + 'BG_getSelectedButtonGroupLabel', False);
end;

end.
