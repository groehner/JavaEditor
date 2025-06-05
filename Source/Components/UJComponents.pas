unit UJComponents;

interface

uses
  Classes, Graphics, Controls, Types, UAComponents;

const
  AncestorEvents1 = '|ancestorMoved|ancestorResized';
  AncestorEvents2 = '|ancestorAdded|ancestorRemoved';
  CaretEvents = '|caretPositionChanged';
  ComponentEvents =
    '|componentHidden|componentMoved|componentResized|componentShown';
  ContainerComponentEvents =
    '|componentAdded|componentHidden|componentMoved|componentRemoved|componentResized|componentShown';
  FocusEvents = '|focusGained|focusLost';
  HierarchyEvents = '|hierarchyChanged';
  ItemEvents = '|itemStateChanged';
  InputMethodEvents = '|inputMethodTextChanged';
  KeyEvents = '|keyPressed|keyReleased|keyTyped';
  MouseEvents =
    '|mouseClicked|mouseDragged|mouseEntered|mouseExited|mouseMoved|mousePressed|mouseReleased|mouseWheelMoved';
  PopupMenuEvents =
    '|popupMenuCanceled|popupMenuWillBecomeInvisible|popupMenuWillBecomeVisible';
  // JComboBox
  PropertyEvents = '|propertyChange';
  StateEvents = '|stateChanged';
  TreeEvents = '|treeCollapsed|treeExpanded|treeValueChanged';
  VetoableEvents = '|vetoableChange';
  WindowEvents =
    '|windowActivated|windowClosed|windowClosing|windowDeactivated|windowDeiconified|windowGainedFocus|windowIconified|windowLostFocus|windowOpened|windowStateChanged';

  ColorNone = clBtnFace;
  CrLf = #13#10;

type
  TBorderType = (NoBorder, LineBorder, EtchedBorder, BevelBorder, TitledBorder,
    MatteBorder);
  TOrientation = (HORIZONTAL, VERTICAL);

  TSwingComponent = class;

  TBorder = class(TPersistent)
  private
    FOwnerControl: TSwingComponent;
    FBorderType: TBorderType;
    FLineColor: TColor;
    FLineThickness: Integer;
    FLineRounded: Boolean;
    FEtchHighlightColor: TColor;
    FEtchShadowColor: TColor;
    FEtchtype: Integer;
    FBevelHighlightColor: TColor;
    FBevelShadowColor: TColor;
    FBeveltype: Integer;
    FTitle: string;
    FFont: TFont;
    FMatteColor: TColor;
    FMatteLeft: Integer;
    FMatteTop: Integer;
    FMatteRight: Integer;
    FMatteBottom: Integer;
    FBox1, FBoy1, FBox2, FBoy2: Integer;
  public
    constructor Create(AOwner: TSwingComponent);
    procedure Show(Control: TSwingComponent; Canvas: TCanvas);
    function GetClientRect: TRect;
    function GetLineRect: TRect;
  published
    property BorderType: TBorderType read FBorderType write FBorderType;
    property LineColor: TColor read FLineColor write FLineColor;
    property LineThickness: Integer read FLineThickness write FLineThickness;
    property LineRounded: Boolean read FLineRounded write FLineRounded;
    property EtchHighlightColor: TColor read FEtchHighlightColor
      write FEtchHighlightColor;
    property EtchShadowColor: TColor read FEtchShadowColor
      write FEtchShadowColor;
    property Etchtype: Integer read FEtchtype write FEtchtype;
    property BevelHighlightColor: TColor read FBevelHighlightColor
      write FBevelHighlightColor;
    property BevelShadowColor: TColor read FBevelShadowColor
      write FBevelShadowColor;
    property Beveltype: Integer read FBeveltype write FBeveltype;
    property Title: string read FTitle write FTitle;
    property Font: TFont read FFont write FFont;
    property MatteColor: TColor read FMatteColor write FMatteColor;
    property MatteTop: Integer read FMatteTop write FMatteTop;
    property MatteLeft: Integer read FMatteLeft write FMatteLeft;
    property MatteBottom: Integer read FMatteBottom write FMatteBottom;
    property MatteRight: Integer read FMatteRight write FMatteRight;
  end;

  TSwingComponent = class(TAWTComponent)
  private
    FSwingSelectionColor: TColor;
    FSwingSelectionBackground: TColor;
    FBlueColor: TColor;
    FBorder: TBorder;
    FToolTipText: string;
    FOpaque: Boolean;

    FAncestorAdded: string;
    FAncestorRemoved: string;
    FStateChanged: string;
    FVetoableChange: string;
    FValueChanged: string;
    procedure SetBorder(AValue: TBorder);
  protected
    procedure MakeIcon(const Attr, Value, Typ: string);
    procedure MakeScrollbarDisplayPolicy(const Attr, Value: string);
    procedure MakeKeyEvent(const Attr, Value: string);
    procedure MakeBorder(Border: TBorder);
    procedure MakeOrientation(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFromJ(Control: TControl);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    destructor Destroy; override;
    procedure Paint; override;

    property BlueColor: TColor read FBlueColor;
    property SelectionColor: TColor read FSwingSelectionColor;
    property SelectionBackground: TColor read FSwingSelectionBackground;
    property Border: TBorder read FBorder write SetBorder;
  published
    property ancestorAdded: string read FAncestorAdded write FAncestorAdded;
    property ancestorRemoved: string read FAncestorRemoved
      write FAncestorRemoved;
    property stateChanged: string read FStateChanged write FStateChanged;
    property valueChanged: string read FValueChanged write FValueChanged;
    property vetoableChange: string read FVetoableChange write FVetoableChange;
    property actionPerformed;
    property textValueChanged;
    property itemStateChanged;
    property adjustmentValueChanged;
    property componentAdded;
    property componentRemoved;

    property Opaque: Boolean read FOpaque write FOpaque;
    property ToolTipText: string read FToolTipText write FToolTipText;
  end;

implementation

uses
  Windows,
  SysUtils,
  UITypes,
  UUtils,
  ULink,
  UObjectInspector;

{ --- TBorder ------------------------------------------------------------------ }

constructor TBorder.Create(AOwner: TSwingComponent);
begin
  FOwnerControl := AOwner;
  FBorderType := NoBorder;
end;

function TBorder.GetLineRect: TRect;
var
  DeltaL: Integer;
begin
  Result := FOwnerControl.ClientRect;
  case BorderType of
    LineBorder:
      begin
        DeltaL := LineThickness div 2;
        Result.Left := DeltaL;
        Result.Top := DeltaL;
        Dec(Result.Right, DeltaL);
        Dec(Result.Bottom, DeltaL);
      end;
    TitledBorder:
      begin
        Result.Left := 3;
        Result.Top := 9;
        Dec(Result.Right, 3);
        Dec(Result.Bottom, 3);
      end;
    MatteBorder:
      begin
        Result.Left := MatteLeft div 2;
        Result.Top := MatteTop div 2;
        Dec(Result.Right, MatteRight div 2);
        Dec(Result.Bottom, MatteBottom div 2);
      end;
  end;
end;

function TBorder.GetClientRect: TRect;
begin
  case BorderType of
    LineBorder:
      begin
        FBox1 := LineThickness;
        FBoy1 := LineThickness;
        FBox2 := LineThickness;
        FBoy2 := LineThickness;
      end;
    EtchedBorder, BevelBorder:
      begin
        FBox1 := 2;
        FBoy1 := 2;
        FBox2 := 2;
        FBoy2 := 2;
      end;
    TitledBorder:
      begin
        FBox1 := 5;
        FBoy1 := 5;
        FBox2 := 5;
        FBoy1 := 17;
      end;
    MatteBorder:
      begin
        FBox1 := MatteLeft;
        FBoy1 := MatteTop;
        FBox2 := MatteRight;
        FBoy2 := MatteBottom;
      end;
  end;
  Result := FOwnerControl.ClientRect;
  Result.Left := FBox1;
  Result.Top := FBoy1;
  Result.Right := Result.Right - FBox2;
  Result.Bottom := Result.Bottom - FBoy2;
end;

procedure TBorder.Show(Control: TSwingComponent; Canvas: TCanvas);
var
  ARect: TRect;
begin
  case BorderType of
    LineBorder:
      begin
        Canvas.Pen.Color := LineColor;
        Canvas.Pen.Width := LineThickness;
        Canvas.Brush.Color := Control.Background;
        ARect := GetLineRect;
        if LineRounded then
          Canvas.RoundRect(ARect.Left, ARect.Top, ARect.Right,
            ARect.Bottom, 4, 4)
        else
          Canvas.Rectangle(ARect);
        Canvas.Pen.Width := 1;
      end;
    EtchedBorder:
      begin
        if Etchtype = 1 then
          Canvas.Brush.Color := FEtchHighlightColor
        else
          Canvas.Brush.Color := FEtchShadowColor;
        Canvas.FrameRect(Rect(0, 0, Control.Width, Control.Height));
        Canvas.FrameRect(Rect(1, 1, Control.Width - 1, Control.Height - 1));
        if Etchtype = 0 then
          Canvas.Brush.Color := FEtchHighlightColor
        else
          Canvas.Brush.Color := FEtchShadowColor;
        Canvas.FrameRect(Rect(0, 0, Control.Width - 1, Control.Height - 1));
      end;
    BevelBorder:
      begin
        if Beveltype = 0 then
          Canvas.Pen.Color := FBevelHighlightColor
        else
          Canvas.Pen.Color := FBevelShadowColor;
        Canvas.MoveTo(0, Control.Height);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(Control.Width, 0);
        Canvas.MoveTo(1, Control.Height - 1);
        Canvas.LineTo(1, 1);
        Canvas.LineTo(Control.Width, 1);
        if Beveltype = 1 then
          Canvas.Pen.Color := FBevelHighlightColor
        else
          Canvas.Pen.Color := FBevelShadowColor;
        Canvas.MoveTo(0, Control.Height - 1);
        Canvas.LineTo(Control.Width - 1, Control.Height - 1);
        Canvas.LineTo(Control.Width - 1, -1);
        Canvas.MoveTo(1, Control.Height - 2);
        Canvas.LineTo(Control.Width - 2, Control.Height - 2);
        Canvas.LineTo(Control.Width - 2, 0);
      end;
    TitledBorder:
      begin
        Canvas.Font.Name := 'Dialog';
        Canvas.Font.Size := 11;
        Canvas.Font.Style := [fsBold];
        Canvas.Pen.Color := Control.SelectionColor;
        Canvas.Brush.Color := Control.Background;
        Canvas.Rectangle(GetLineRect);
        Canvas.TextOut(9, 4, FTitle);
      end;
    MatteBorder:
      begin
        ARect := GetLineRect;
        Canvas.Pen.Color := MatteColor;
        Canvas.Pen.Width := MatteLeft;
        Canvas.MoveTo(ARect.Left, Control.Height);
        Canvas.LineTo(ARect.Left, 0);
        Canvas.Pen.Width := MatteTop;
        Canvas.MoveTo(0, ARect.Top);
        Canvas.LineTo(Control.Width, ARect.Top);
        Canvas.Pen.Width := MatteRight;
        Canvas.MoveTo(ARect.Right, 0);
        Canvas.LineTo(ARect.Right, Control.Height);
        Canvas.Pen.Width := MatteBottom;
        Canvas.MoveTo(Control.Width, ARect.Bottom);
        Canvas.LineTo(0, ARect.Bottom);
      end;
  end;
end;

{ --- TSwingComponent ---------------------------------------------------------- }

constructor TSwingComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlueColor := RGB(99, 130, 191);
  DefaultBackground := RGB(238, 238, 238); // EEEEEE
  DefaultForeground := RGB(51, 51, 51); // 333333
  FSwingSelectionColor := RGB(184, 207, 229); // B8CFE5
  FSwingSelectionBackground := RGB(163, 184, 204); // A3B8CC
  FOpaque := True;

  Foreground := DefaultForeground;
  Background := DefaultBackground;
  Font.Style := [fsBold];
  FBorder := TBorder.Create(Self);
end;

procedure TSwingComponent.CreateFromJ(Control: TControl);
begin
  CreateFromA(Control);
  ToolTipText := Control.Hint;
end;

function TSwingComponent.GetAttributes(ShowAttributes: Integer): string;
begin
  case ShowAttributes of
    1:
      Result := '';
    2:
      Result := '|ToolTipText';
  else
    Result := '|ToolTipText|Opaque';
  end;
  Result := Result + inherited;
end;

procedure TSwingComponent.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'DisplayedMnemonic' then
    MakeKeyEvent(Attr, Value)
  else if Attr = 'Border' then
    MakeBorder(Border)
  else if Attr = 'Orientation' then
    MakeOrientation(Value)
  else
    inherited;
end;

function TSwingComponent.GetEvents(ShowEvents: Integer): string;
begin
  if ShowEvents < 3 then
    Result := ''
  else
    Result := '|ancestorAdded|ancestorRemoved|vetoableChange';
  Result := Result + inherited GetEvents(ShowEvents);
end;

destructor TSwingComponent.Destroy;
begin
  FreeAndNil(FBorder);
  inherited;
end;

procedure TSwingComponent.Paint;
begin
  inherited;
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width - 1, Height - 1));
  if Assigned(FBorder) then
  begin
    case FBorder.BorderType of
      EtchedBorder:
        begin
          Canvas.Brush.Color := ChangeColor(Background, 1.428);
          Canvas.FrameRect(Rect(1, 1, Width, Height));
          Canvas.Brush.Color := ChangeColor(Background, 0.7);
          Canvas.FrameRect(Rect(0, 0, Width - 1, Height - 1));
        end;
      BevelBorder:
        begin // lowered
          Canvas.MoveTo(0, Height - 1);
          Canvas.Pen.Color := ChangeColor(Background, 2);
          Canvas.LineTo(Width - 1, Height - 1);
          Canvas.LineTo(Width - 1, 0);
          Canvas.Pen.Color := ChangeColor(Background, 0.7);
          Canvas.LineTo(0, 0);
          Canvas.LineTo(0, Height);

          Canvas.MoveTo(1, Height - 2);
          Canvas.Pen.Color := ChangeColor(Background, 1.428);
          Canvas.LineTo(Width - 2, Height - 2);
          Canvas.LineTo(Width - 2, 1);
          Canvas.Pen.Color := ChangeColor(Background, 0.4857);
          Canvas.LineTo(1, 1);
          Canvas.LineTo(1, Height - 2);
        end;
      { BevelBorder: begin  //raised
        Canvas.MoveTo(0, Height-1);
        Canvas.Pen.Color:= ChangeColor(Background, 0.4857);
        Canvas.LineTo(Width-1, Height-1);
        Canvas.LineTo(Width-1, 0);
        Canvas.Pen.Color:= ChangeColor(Background, 2);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(0, Height);

        Canvas.MoveTo(1, Height-2);
        Canvas.Pen.Color:= ChangeColor(Background, 0.7);
        Canvas.LineTo(Width-2, Height-2);
        Canvas.LineTo(Width-2, 1);
        Canvas.Pen.Color:= ChangeColor(Background, 1.428);
        Canvas.LineTo(1, 1);
        Canvas.LineTo(1, Height-2);
        end; }
      TitledBorder:
        begin
          Canvas.Brush.Color := FSwingSelectionColor;
          Canvas.FrameRect(Rect(2, 8, Width - 3, Height - 3));
          Canvas.Font := Font;
          Canvas.Font.Color := DefaultForeground;
          Canvas.Brush.Color := Background;
          Canvas.TextOut(8, 2, FBorder.Title);
        end;
    end;
  end;
end;

procedure TSwingComponent.SetBorder(AValue: TBorder);
begin
  FBorder := AValue;
  Invalidate;
end;

procedure TSwingComponent.MakeIcon(const Attr, Value, Typ: string);
var
  Str, Key, AtPos, Dest, Filename, Path: string;
begin
  if Value = '(Icon)' then
  begin
    Key := 'private ImageIcon ' + Name + Attr;
    FPartner.DeleteAttribute(Key);
    FPartner.DeleteAttributeValue(Name + '.set' + Attr + '(');
  end
  else
  begin
    Filename := ExtractFileName(Value);
    if Pos('images/', Filename) = 1 then
      System.Delete(Filename, 1, 7);
    Path := ExtractFilePath(FPartner.Pathname);
    Dest := Path + 'images\' + Filename;
    ForceDirectories(Path + 'images\');
    if not FileExists(Dest) then
      CopyFile(PChar(Value), PChar(Dest), True);
    Filename := 'images/' + Filename;
    FObjectInspector.ELPropertyInspector.SetByCaption(Attr, Filename);

    AtPos := 'private ' + Typ + ' ' + Name;
    Key := 'private ImageIcon ' + Name + Attr;
    Str := Indent2 + Key + ' = new ImageIcon(getClass().getResource("' +
      Filename + '"));';
    FPartner.ReplaceAttributAt(AtPos, Key, Str);
    MakeAttribut(Attr, Name + Attr);
  end;
end;

procedure TSwingComponent.MakeKeyEvent(const Attr, Value: string);
var
  Key, Str: string;
begin
  Key := Name + '.set' + Attr;
  if Value = '(None)' then
    Str := ''
  else
    Str := Indent2 + Key + '(KeyEvent.VK_' + Value + ');';
  SetAttributValue(Key, Str);
end;

procedure TSwingComponent.MakeScrollbarDisplayPolicy(const Attr, Value: string);
var
  Str, Key: string;
begin
  Key := Name + 'ScrollPane.set' + Attr;
  if Attr = 'HorizontalScrollBarPolicy' then
    Str := Indent2 + Key + '(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_' +
      Value + ');'
  else
    Str := Indent2 + Key + '(ScrollPaneConstants.VERTICAL_SCROLLBAR_' +
      Value + ');';
  SetAttributValue(Key, Str);
end;

procedure TSwingComponent.MakeBorder(Border: TBorder);
var
  Key, Str: string;
begin
  Key := Name + '.setBorder(';
  case Border.BorderType of
    NoBorder:
      Str := '';
    LineBorder:
      begin
        Str := 'new javax.swing.border.LineBorder(' +
          ToJavaColor(Border.LineColor);
        if Border.LineThickness <> 1 then
          Str := Str + ', ' + IntToStr(Border.LineThickness);
        if Border.LineRounded then
          Str := Str + ', true';
        Str := Str + '));';
      end;
    EtchedBorder:
      Str := 'BorderFactory.createEtchedBorder(' + IntToStr(Border.Etchtype) +
        ', ' + ToJavaColor(Border.EtchHighlightColor) + ', ' +
        ToJavaColor(Border.EtchShadowColor) + '));';
    BevelBorder:
      Str := 'BorderFactory.createBevelBorder(' + IntToStr(Border.Beveltype) +
        ', ' + ToJavaColor(Border.BevelHighlightColor) + ', ' +
        ToJavaColor(Border.BevelShadowColor) + '));';
    TitledBorder:
      Str := 'BorderFactory.createTitledBorder("' + Border.Title + '"));';
    MatteBorder:
      Str := 'BorderFactory.createMatteBorder(' + IntToStr(Border.MatteTop) +
        ', ' + IntToStr(Border.MatteLeft) + ', ' + IntToStr(Border.MatteBottom)
        + ', ' + IntToStr(Border.MatteRight) + ', ' +
        ToJavaColor(Border.MatteColor) + '));';
  end;
  if Str <> '' then
    Str := Indent2 + Key + Str;
  SetAttributValue(Key, Str);
end;

procedure TSwingComponent.MakeOrientation(const Value: string);
begin
  MakeAttribut('Orientation', 'SwingConstants.' + Value);
  SetPositionAndSize;
end;

procedure TSwingComponent.Rename(const OldName, NewName, Events: string);

  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FAncestorAdded);
  Rename(FAncestorRemoved);
  Rename(FStateChanged);
  Rename(FVetoableChange);
  Rename(FValueChanged);
end;

end.
