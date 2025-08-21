unit UJCheckboxRadio;

{ Classes
  TJCheckBox = class (TSwingComponent)
  TJRadioButton
  TJButtonGroup = class (TSwingComponent)
  TJ2ButtonGroup = class (TSwingComponent)
}

interface

uses
  Classes,
  StdCtrls,
  UJComponents;

type

  TJCheckBox = class(TSwingComponent)
  private
    FText: string;
    FSelected: Boolean;
    procedure SetSelected(AValue: Boolean);
    procedure SetText(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure NewControl; override;
    procedure Paint; override;
  published
    property Selected: Boolean read FSelected write SetSelected;
    property Text: string read FText write SetText;
  end;

  // deprecated
  TJRadioButton = class(TJCheckBox)
  private
    FButtonGroup: string;
    procedure MakeButtonGroup(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property ButtonGroup: string read FButtonGroup write FButtonGroup;
  end;

  // deprecated
  TJButtonGroup = class(TSwingComponent)
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  end;

  TJ2ButtonGroup = class(TSwingComponent)
  private
    FCheckboxes: Boolean;
    FColumns: Integer;
    FFrame: Boolean;
    FTitle: string;
    FItems: TStrings;
    FOldItems: TStrings;
    procedure SetColumns(Value: Integer);
    procedure SetTitle(const Value: string);
    procedure SetItems(Value: TStrings);
    procedure SetCheckboxes(Value: Boolean);
    procedure SetFrame(Value: Boolean);
    procedure MakeButtongroupItems;
    procedure MakeTitle;
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
    procedure MakeFont; override;
  published
    property Items: TStrings read FItems write SetItems;
    // must stay before columns or label
    property Columns: Integer read FColumns write SetColumns;
    property Checkboxes: Boolean read FCheckboxes write SetCheckboxes;
    property Title: string read FTitle write SetTitle;
    property Frame: Boolean read FFrame write SetFrame;
  end;

implementation

uses
  Windows,
  SysUtils,
  Graphics,
  Controls,
  UITypes,
  JvGnugettext,
  UJava,
  UStringRessources,
  UConfiguration,
  UGUIDesigner;

{ --- TJCheckBox --------------------------------------------------------------- }

constructor TJCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +5;
  Width := 120;
  Height := 24;
  FSelected := False;
  Background := clBtnFace;
  Opaque := False;
  Text := 'Checkbox';
  JavaType := 'JCheckBox';
end;

function TJCheckBox.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Text|Selected' + inherited GetAttributes(ShowAttributes);
end;

function TJCheckBox.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed|itemStateChanged|stateChanged' + inherited;
end;

procedure TJCheckBox.NewControl;
begin
  FText := 'CheckBox';
  DefaultComponent;
  InsertNewVariable('private JCheckBox ' + Name + ' = new JCheckBox();');
  MakeAttribut('Text', AsString(Text));
  MakeAttribut('Opaque', 'false');
end;

procedure TJCheckBox.Paint;
const
  BoxW = 13; // Width of rectangular checkbox
  BoxH = 15;

var
  TextHeight, XPos, YPos: Integer;

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
    if FSelected then
      Inc(Result);
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  TextHeight := Canvas.TextHeight('Q');

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
  YPos := (Height - TextHeight) div 2;
  Canvas.TextOut(XPos, YPos, FText);

  if Tag > 0 then
    XPos := PPIScale(4)
  else
    XPos := 0;
  YPos := (Height - PPIScale(BoxH)) div 2;
  FGUIDesigner.vilControls1315.Draw(Canvas, XPos, YPos, TagToPicNr);
end;

procedure TJCheckBox.SetSelected(AValue: Boolean);
begin
  if FSelected <> AValue then
  begin
    FSelected := AValue;
    Invalidate;
  end;
end;

procedure TJCheckBox.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

{ --- TJRadioButton ------------------------------------------------------------ }

constructor TJRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +6;
  Width := 80;
  JavaType := 'JRadioButton';
end;

function TJRadioButton.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|ButtonGroup' + inherited GetAttributes(ShowAttributes);
end;

procedure TJRadioButton.MakeButtonGroup(const Value: string);
var
  Key, Str: string;
  From, Till: Integer;
begin
  Key := 'private ButtonGroup';
  From := FPartner.GetLineNumberWith(Key);
  Till := FPartner.GetLNGEndComponents;
  for var I := From to Till do
  begin
    Str := FPartner.Editor.Lines[I];
    if Pos(Key, Str) > 0 then
    begin
      Delete(Str, Pos(' =', Str), Length(Str));
      Delete(Str, 1, Pos(Key, Str) + Length(Key));
      // s is name of a ButtonGroup
      FPartner.DeleteAttributeValue(Str + '.add(' + Name);
    end;
  end;
  if Value <> '' then
    SetAttributValue('___XXX___', Value + '.add(' + Name + ');');
end;

procedure TJRadioButton.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'ButtonGroup' then
    MakeButtonGroup(Value)
  else
    inherited;
end;

{ --- ButtonGroup deprecated --------------------------------------------------- }

constructor TJButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 7;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'ButtonGroup';
end;

function TJButtonGroup.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Name';
end;

function TJButtonGroup.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TJButtonGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_getSelectedButtonGroupLabel',
    NewName + '_getSelectedButtonGroupLabel', False);
end;

procedure TJButtonGroup.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_getSelectedRadioButtonLabel', False);
end;

procedure TJButtonGroup.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilAWTLight.Draw(Canvas, 6, 4, 7);
end;

{ --- TJ2ButtonGroup ----------------------------------------------------------- }

constructor TJ2ButtonGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 50;
  Width := 120;
  Height := 80;
  FColumns := 1;
  FItems := TStringList.Create;
  FItems.Text := _('America') + ', ' + _('selected') + #13#10 + _('Europe') +
    #13#10 + _('Asia');
  FOldItems := TStringList.Create;
  FTitle := _('Continent');
  FCheckboxes := False;
  FFrame := True;
  JavaType := 'JButtonGroup';
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
  FPartner.DeleteAttribute('private JPanel ' + Name);
  FPartner.DeleteAttributeValue(GetContainer + '.add(' + Name + ')');
  FPartner.DeleteAttributeValue(Name + '.setLayout');
  DeleteSelectedLabelMethod;
end;

procedure TJ2ButtonGroup.DeleteComponentAttributes;
begin
  FPartner.DeleteAttribute('ButtonGroup ' + Name + 'BG');
  FPartner.DeleteAttribute('TitledBorder ' + Name + 'TB');
  for var I := 0 to FOldItems.Count - 1 do
    FPartner.DeleteAttribute(RBName(I));
end;

procedure TJ2ButtonGroup.DeleteComponentValues;
begin
  FPartner.DeleteAttributeValue(Name + 'TB.setTitleFont');
  FPartner.DeleteAttributeValues(Name + '.setBorder');
  for var I := 0 to FOldItems.Count - 1 do
    FPartner.DeleteAttributeValues(RBName(I));
end;

procedure TJ2ButtonGroup.DeleteSelectedLabelMethod;
begin
  FPartner.DeleteMethod(Name + 'BG_getSelectedButtonGroupLabel', False);
end;

function TJ2ButtonGroup.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|Columns|Items|Title|Checkboxes|Frame|Font';
  Result := Result + inherited GetAttributes(ShowAttributes);
end;

procedure TJ2ButtonGroup.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'Items') or (Attr = 'Checkboxes') or (Attr = 'Frame') then
    MakeButtongroupItems
  else if (Attr = 'Title') or (Attr = 'Columns') or IsFontAttribute(Attr) then
    SetPositionAndSize
  else
    inherited;
end;

function TJ2ButtonGroup.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed|';
end;

procedure TJ2ButtonGroup.DeleteListener(const Event: string);
var
  EventMethodName, Listener: string;
begin
  EventMethodName := MakeEventProcedureName(Event);
  FPartner.DeleteMethod(EventMethodName);
  for var I := 0 to FItems.Count - 1 do
  begin
    Listener := RBName(I) + '.addActionListener((event) -> {' +
      EventMethodName + '(event);});';
    FPartner.DeleteLambdaListener(Listener);
  end;
end;

procedure TJ2ButtonGroup.AddListener(const Event: string);
var
  EventMethodName, Listener: string;
begin
  EventMethodName := MakeEventProcedureName(Event);
  for var I := 0 to FItems.Count - 1 do
  begin
    Listener := Indent2 + RBName(I) + '.addActionListener((event) -> {' +
      EventMethodName + '(event);});';
    FPartner.InsertListener(RBName(I) + '.setBounds', Listener);
  end;
  if not FPartner.HasText('public void ' + EventMethodName) then
    FPartner.InsertProcedure(0, MakeEventProcedure(Event));
end;

function TJ2ButtonGroup.MakeEventProcedure(const Event: string): string;
begin
  Result := Indent1 + 'public void ' + Name +
    '_ActionPerformed(ActionEvent evt) {' + CrLf + Indent2 + _(LNGTODO) + CrLf +
    Indent2 + 'System.out.println(((AbstractButton)evt.getSource()).getText());'
    + CrLf + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Result := Result + ' // end of ' + Name + '_Action';
  Result := Result + CrLf + CrLf;
end;

function TJ2ButtonGroup.RBName(Int: Integer): string;
begin
  Result := Name + 'RB' + IntToStr(Int);
end;

procedure TJ2ButtonGroup.MakeButtongroupItems;
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
    Str1 := Str1 + SurroundIndent('  private ButtonGroup ' + Name +
      'BG = new ButtonGroup();');
    MakeSelectedLabelMethod;
  end;
  if FFrame then
    Str1 := Str1 + SurroundIndent('  private TitledBorder ' + Name +
      'TB = new TitledBorder(' + AsString(FTitle) + ');');
  for var I := 0 to FItems.Count - 1 do
  begin
    Str := FItems[I];
    Posi := Pos(', ' + _('selected'), Str);
    if Posi > 0 then
      Str := AsString(Copy(Str, 1, Posi - 1))
    else
      Str := AsString(Str);
    if FCheckboxes then
      Str1 := Str1 + SurroundIndent('  private JCheckBox ' + RBName(I) +
        ' = new JCheckBox(' + Str + ');')
    else
      Str1 := Str1 + SurroundIndent('  private JRadioButton ' + RBName(I) +
        ' = new JRadioButton(' + Str + ');');
  end;
  FPartner.InsertAttribute(GetContainer, Str1, False);
  SetPositionAndSize;
  FOldItems.Text := FItems.Text;
  FPartner.Editor.EndUpdate;
end;

procedure TJ2ButtonGroup.MakeTitle;
var
  Str, Key: string;
begin
  if FFrame then
  begin
    Key := Name + '.setBorder';
    SetAttributValue(Key, Key + '(' + Name + 'TB);');
    if FontChanged and (FTitle <> '') then
    begin
      Key := Name + 'TB.setTitleFont';
      SetAttributValue(Key, Key + '(new Font(' + AsString(Font.Name) +
        ', Font.BOLD, ' + IntToStr(PPIUnScale(Font.Size)) + '));');
    end;
    Key := 'private TitledBorder ' + Name + 'TB';
    Str := Indent2 + Key + ' = new TitledBorder(' + AsString(Title) + ');';
    FPartner.ReplaceLine(Key, Str);
  end
  else
    FPartner.DeleteAttributeValue(Name + '.setBorder(');
end;

procedure TJ2ButtonGroup.SetPositionAndSize;
const
  Circle = 18;
var
  Col, Row, ItemsInCol, Line, XPos, YPos, DeltaX, TextHeight: Integer;
  RadioHeight, RadioWidth, ColWidth, RowHeight, ColWidthRest, RowHeightRest,
    LabelHeight: Integer;
  XOld, YOld, ColWidthI, RowHeightI: Integer;
  Str, Nam: string;
begin
  FPartner.Editor.BeginUpdate;
  inherited;
  CanvasFontAssign;
  DeleteComponentValues;
  Str := '';
  TextHeight := Canvas.TextHeight('Hg');
  DeltaX := Canvas.TextWidth('x');
  LabelHeight := 0;
  if FFrame then
  begin
    RadioWidth := Width - 2 * DeltaX;
    RadioHeight := Height - TextHeight - DeltaX;
    LabelHeight := TextHeight;
  end
  else
  begin
    RadioWidth := Width - 2 * DeltaX;
    RadioHeight := Height - DeltaX;
  end;

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
          YPos := LabelHeight + DeltaX div 2
        else
          YPos := YOld + RowHeightI;
        Dec(RowHeightRest);
        Nam := RBName(Line);
        Str := Str + SurroundFix('  ' + Nam + '.setBounds' + '(' +
          IntToStr(PPIUnScale(XPos)) + ', ' + IntToStr(PPIUnScale(YPos)) + ', '
          + IntToStr(PPIUnScale(ColWidthI)) + ', ' +
          IntToStr(PPIUnScale(RowHeightI)) + ');');
        if Pos(', ' + _('selected'), FItems[Line]) > 0 then
          Str := Str + SurroundFix('  ' + Nam + '.setSelected(true);');
        if actionPerformed <> '' then
          Str := Str + SurroundFix
            ('  ' + Nam + '.addActionListener((event) -> {' + Name +
            '_ActionPerformed(event);});');
        if FontChanged then
          Str := Str + SurroundFix('  ' + Nam + '.setFont(new Font(' +
            AsString(Font.Name) + ', Font.BOLD, ' +
            IntToStr(PPIUnScale(Font.Size)) + '));');
        if not FCheckboxes then
          Str := Str + SurroundFix('  ' + Name + 'BG.add(' + Nam + ');');
        Str := Str + SurroundFix('  ' + Name + '.add(' + Nam + ');');
        Inc(Line);
        YOld := YPos;
      end;
      XOld := XPos;
    end;
  end;
  FPartner.InsertAttributValue(Name, Str, 1);
  MakeTitle;
  if actionPerformed <> '' then
  begin
    if not FPartner.HasText('public void ' + MakeEventProcedureName
      ('actionPerformed')) then
      FPartner.InsertProcedure(0, MakeEventProcedure('actionPerformed'));
  end;
  FPartner.Editor.EndUpdate;
end;

procedure TJ2ButtonGroup.MakeFont;
begin
  SetPositionAndSize;
end;

procedure TJ2ButtonGroup.Paint;
const
  CRadius = 6;
var
  ColumnWidth, RowHeight, RadioHeight, LabelHeight, Col, Row, YCen, ItemsInCol,
    Line, XPos, YPos, TextHeight, TextWidth, Posi, Radius: Integer;
  ARect: TRect;
  Str: string;
begin
  FOldItems.Text := FItems.Text;
  inherited;
  CanvasFontAssign;
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(ClientRect);
  TextHeight := Canvas.TextHeight('Hg');
  TextWidth := Canvas.TextWidth('x');
  LabelHeight := 0;
  RadioHeight := Height;
  ARect := ClientRect;
  if FFrame then
  begin
    Canvas.Pen.Color := $BABABA;
    ARect.Top := TextHeight div 2;
    Canvas.Rectangle(ARect);
  end;
  ARect := ClientRect;
  if (FTitle <> '') and FFrame then
  begin
    ARect.Top := TextHeight div 2;
    LabelHeight := TextHeight;
    RadioHeight := Height - TextHeight;
    Canvas.Rectangle(ARect);
    Canvas.TextOut(PPIScale(10), 0, FTitle);
  end;

  if FItems.Count > 0 then
  begin
    Radius := PPIScale(CRadius);
    Canvas.Pen.Color := $333333;
    ColumnWidth := Width div FColumns;
    RowHeight := RadioHeight div ItemsInColumn(1);
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
        XPos := TextWidth + (Col - 1) * ColumnWidth;
        YPos := LabelHeight + 2 + (Row - 1) * RowHeight;
        Canvas.Brush.Color := clWhite;
        YCen := YPos + RowHeight div 2 - TextHeight div 2;
        if FCheckboxes then
        begin
          ARect := Rect(XPos, YCen + 0, XPos + 13, YCen + 13);
          Canvas.Rectangle(ARect);
          if Posi > 0 then
          begin
            Canvas.Pen.Width := 2;
            Canvas.MoveTo(XPos + 3, YCen + 6);
            Canvas.LineTo(XPos + 4, YCen + 9);
            Canvas.LineTo(XPos + 9, YCen + 3);
            Canvas.Pen.Width := 1;
          end;
        end
        else
        begin
          YCen := YPos + RowHeight div 2 - Radius;
          Canvas.Ellipse(XPos, YCen, XPos + 2 * Radius, YCen + 2 * Radius);
          if Posi > 0 then
          begin
            Canvas.Brush.Color := clBlack;
            Canvas.Ellipse(XPos + 3, YCen + 3, XPos + 2 * Radius - 3,
              YCen + 2 * Radius - 3);
            Canvas.Brush.Color := clWhite;
          end;
        end;
        Canvas.Brush.Color := clBtnFace;
        YCen := YPos + RowHeight div 2 - TextHeight div 2;
        ARect := Rect(XPos + PPIScale(19), YCen, Col * ColumnWidth,
          YCen + RowHeight);
        Canvas.TextRect(ARect, Str);
        Inc(Line);
      end;
    end;
  end;
end;

procedure TJ2ButtonGroup.NewControl;
begin
  InsertImport('javax.swing.border.TitledBorder');
  InsertNewVariable('private JPanel ' + Name + ' = new JPanel();');
  var
  Str := SurroundFix('  ' + Name + '.setLayout(null);') +
    SurroundFix('  ' + GetContainer + '.add(' + Name + ');');
  FPartner.InsertAttributValue(Name, Str, 1);
  MakeButtongroupItems;
end;

procedure TJ2ButtonGroup.SetItems(Value: TStrings);
begin
  if FItems.Text <> Value.Text then
  begin
    FOldItems.Text := FItems.Text;
    FItems.Text := Value.Text;
    Invalidate;
  end;
end;

function TJ2ButtonGroup.ItemsInColumn(Int: Integer): Integer;
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

procedure TJ2ButtonGroup.SetColumns(Value: Integer);
begin
  if (FColumns <> Value) and (Value > 0) then
  begin
    FColumns := Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    FTitle := Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.SetCheckboxes(Value: Boolean);
begin
  if FCheckboxes <> Value then
  begin
    FCheckboxes := Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.SetFrame(Value: Boolean);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    Invalidate;
  end;
end;

procedure TJ2ButtonGroup.MakeSelectedLabelMethod;
var
  Str: string;
begin
  Str := 'public String ' + Name + 'BG_getSelectedButtonGroupLabel)';
  if FPartner.GetLineNumberWith(Str) = -1 then
  begin
    Str := SurroundFix('public String ' + Name +
      'BG_getSelectedButtonGroupLabel() {') +
      SurroundFix('  for (java.util.Enumeration<AbstractButton> e = ' + Name +
      'BG.getElements(); e.hasMoreElements();) {') +
      SurroundFix('    AbstractButton b = e.nextElement();') +
      SurroundFix('    if (b.isSelected()) return b.getText();') +
      SurroundFix('  }') + SurroundFix('  return "";') +
      SurroundFix('}') + CrLf;
    FPartner.InsertProcedure(Str);
  end;
end;

procedure TJ2ButtonGroup.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'BG_getSelectedButtonGroupLabel',
    NewName + 'BG_getSelectedButtonGroupLabel', False);
end;

end.
