unit UJComponents;

interface

uses
  Classes, Graphics, Controls, Types, UAComponents;

const
  AncestorEvents1 = '|ancestorMoved|ancestorResized';
  AncestorEvents2 = '|ancestorAdded|ancestorRemoved';
  CaretEvents     = '|caretPositionChanged';
  ComponentEvents = '|componentHidden|componentMoved|componentResized|componentShown';
  ContainerComponentEvents = '|componentAdded|componentHidden|componentMoved|componentRemoved|componentResized|componentShown';
  FocusEvents    = '|focusGained|focusLost';
  HierarchyEvents = '|hierarchyChanged';
  ItemEvents = '|itemStateChanged';
  InputMethodEvents = '|inputMethodTextChanged';
  KeyEvents = '|keyPressed|keyReleased|keyTyped';
  MouseEvents = '|mouseClicked|mouseDragged|mouseEntered|mouseExited|mouseMoved|mousePressed|mouseReleased|mouseWheelMoved';
  PopupMenuEvents = '|popupMenuCanceled|popupMenuWillBecomeInvisible|popupMenuWillBecomeVisible';  // JComboBox
  PropertyEvents = '|propertyChange';
  StateEvents    = '|stateChanged';
  TreeEvents     = '|treeCollapsed|treeExpanded|treeValueChanged';
  VetoableEvents = '|vetoableChange';
  WindowEvents = '|windowActivated|windowClosed|windowClosing|windowDeactivated|windowDeiconified|windowGainedFocus|windowIconified|windowLostFocus|windowOpened|windowStateChanged';

  ColorNone = clBtnFace;
  CrLf = #13#10;

type
  TBorderType = (NoBorder, LineBorder, EtchedBorder, BevelBorder, TitledBorder, MatteBorder);
  TOrientation = (HORIZONTAL, VERTICAL);

  TSwingComponent = class;

  TBorder = class (TPersistent)
  private
    FOwnerControl: TSwingComponent;
    FBorderType: TBorderType;
    FLineColor: TColor;
    FLineThickness: integer;
    FLineRounded: boolean;
    FEtchHighlightColor: TColor;
    FEtchShadowColor: TColor;
    FEtchtype: integer;
    FBevelHighlightColor: TColor;
    FBevelShadowColor: TColor;
    FBeveltype: integer;
    FTitle: string;
    FFont: TFont;    
    FMatteColor: TColor;
    FMatteLeft: integer;
    FMatteTop: integer;
    FMatteRight: integer;
    FMatteBottom: integer;
    box1, boy1, box2, boy2: integer;
  public
    constructor create(AOwner: TSwingComponent);
    procedure Show(Control: TSwingComponent; Canvas: TCanvas);
    function getClientRect: TRect;
    function getLineRect: TRect;
  published
    property BorderType: TBorderType read FBorderType write FBordertype;
    property LineColor: TColor read FLineColor write FLineColor;
    property LineThickness: integer read FLineThickness write FLineThickness;
    property LineRounded: boolean read FLineRounded write FLineRounded;
    property EtchHighlightColor: TColor read FEtchHighlightColor write FEtchHighlightColor;
    property EtchShadowColor: TColor read FEtchShadowColor write FEtchShadowColor;
    property Etchtype: integer read FEtchtype write FEtchtype;
    property BevelHighlightColor: TColor read FBevelHighlightColor write FBevelHighlightColor;
    property BevelShadowColor: TColor read FBevelShadowColor write FBevelShadowColor;
    property Beveltype: integer read FBeveltype write FBeveltype;
    property Title: string read FTitle write FTitle;
    property Font: TFont read FFont write FFont;    
    property MatteColor: TColor read FMatteColor write FMatteColor;
    property MatteTop: integer read FMatteTop write FMatteTop;
    property MatteLeft: integer read FMatteLeft write FMatteLeft;
    property MatteBottom: integer read FMatteBottom write FMatteBottom;
    property MatteRight: integer read FMatteRight write FMatteRight;
  end;

  TSwingComponent = class (TAWTComponent)
  private
    FSwingSelectionColor: TColor;
    FSwingSelectionBackground: TColor;
    FBlueColor: TColor;
    FBorder: TBorder;
    FToolTipText: string;
    FOpaque: boolean;

    FancestorAdded: string;
    FancestorRemoved: string;
    FStateChanged: string;
    FVetoableChange: string;
    FValueChanged: string;
    procedure setBorder(aValue: TBorder);
  protected
    procedure MakeIcon(const Attr, Value, Typ: string);
    procedure MakeScrollbarDisplayPolicy(const Attr, Value: string);
    procedure MakeKeyEvent(const Attr, Value: string);
    procedure MakeBorder(Border: TBorder);
    procedure MakeOrientation(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateFromJ(Control: TControl);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    destructor Destroy; override;
    procedure Paint; override;

    property BlueColor: TColor read FBlueColor;
    property SelectionColor: TColor read FSwingSelectionColor;
    property SelectionBackground: TColor read FSwingSelectionBackground;
    property Border: TBorder read FBorder write setBorder;
  published
    property ancestorAdded: string read FancestorAdded write FancestorAdded;
    property ancestorRemoved: string read FancestorRemoved write FancestorRemoved;
    property stateChanged: string read FStateChanged write FStateChanged;
    property valueChanged: string read FValueChanged write FValueChanged;
    property vetoableChange: string read FvetoableChange write FvetoableChange;
    property actionPerformed;
    property textValueChanged;
    property itemStateChanged;
    property adjustmentValueChanged;
    property componentAdded;
    property componentRemoved;

    property Opaque: boolean read FOpaque write FOpaque;
    property ToolTipText: string read FToolTipText write FToolTipText;
  end;


implementation

uses Windows, SysUtils, UITypes, TypInfo,
     UUtils, ULink, UObjectInspector, UFrmEditor, UFrmBaseform;

{--- TBorder ------------------------------------------------------------------}

constructor TBorder.create(AOwner: TSwingComponent);
begin
  FOwnerControl:= AOwner;
  FBorderType:= NoBorder;
end;

function TBorder.getLineRect: TRect;
  var dl: integer;
begin
  Result:= FOwnerControl.ClientRect;
  case Bordertype of
    LineBorder: begin
      dl:= LineThickness div 2;
      Result.Left:= dl;
      Result.Top:= dl;
      Dec(Result.Right, dl);
      Dec(Result.Bottom, dl);
    end;
    TitledBorder: begin
      Result.Left:= 3;
      Result.Top:= 9;
      Dec(Result.Right, 3);
      Dec(Result.Bottom, 3);
    end;
    MatteBorder: begin
      Result.Left:= MatteLeft div 2;
      Result.Top:= MatteTop div 2;
      Dec(Result.Right, MatteRight div 2);
      Dec(Result.Bottom, MatteBottom div 2);
    end;
  end;
end;

function TBorder.getClientRect: TRect;
begin
  case Bordertype of
    LineBorder: begin
      box1:= LineThickness;
      boy1:= LineThickness;
      box2:= LineThickness;
      boy2:= LineThickness
    end;
    EtchedBorder,
    BevelBorder: begin
      box1:= 2; boy1:= 2;
      box2:= 2; boy2:= 2;
    end;
    TitledBorder: begin
      box1:= 5; boy1:= 5;
      box2:= 5; boy1:= 17;
    end;
    MatteBorder: begin
      box1:= MatteLeft;
      boy1:= MatteTop;
      box2:= MatteRight;
      boy2:= MatteBottom;
    end;
  end;
  Result:= FOwnerControl.ClientRect;
  Result.Left:= box1;
  Result.Top:= boy1;
  Result.Right:= Result.Right - box2;
  Result.Bottom:= Result.Bottom - boy2;
end;

procedure TBorder.Show(Control: TSwingComponent; Canvas: TCanvas);
  var aRect: TRect;
begin
  case Bordertype of
    LineBorder: begin
      Canvas.Pen.Color:= LineColor;
      Canvas.Pen.Width:= LineThickness;
      Canvas.Brush.Color:= Control.Background;
      aRect:= getLineRect;
      if LineRounded
        then Canvas.RoundRect(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom, 4, 4)
        else Canvas.Rectangle(aRect);
      Canvas.Pen.Width:= 1;
    end;
    EtchedBorder: begin
      if Etchtype = 1
        then Canvas.Brush.Color:= FEtchHighlightColor
        else Canvas.Brush.Color:= FEtchShadowColor;
      Canvas.FrameRect(Rect(0, 0, Control.Width, Control.Height));
      Canvas.FrameRect(Rect(1, 1, Control.Width-1, Control.Height-1));
      if Etchtype = 0
        then Canvas.Brush.Color:= FEtchHighlightColor
        else Canvas.Brush.Color:= FEtchShadowColor;
      Canvas.FrameRect(Rect(0, 0, Control.Width-1, Control.Height-1));
    end;
    BevelBorder: begin
      if Beveltype = 0
        then Canvas.Pen.Color:= FBevelHighlightColor
        else Canvas.Pen.Color:= FBevelShadowColor;
      Canvas.MoveTo(0, Control.height);
      Canvas.LineTo(0, 0);
      Canvas.LineTo(Control.Width, 0);
      Canvas.MoveTo(1, Control.Height-1);
      Canvas.LineTo(1, 1);
      Canvas.LineTo(Control.Width, 1);
      if Beveltype = 1
        then Canvas.Pen.Color:= FBevelHighlightColor
        else Canvas.Pen.Color:= FBevelShadowColor;
      Canvas.MoveTo(0, Control.Height-1);
      Canvas.LineTo(Control.Width-1, Control.Height-1);
      Canvas.LineTo(Control.Width-1, -1);
      Canvas.MoveTo(1, Control.Height-2);
      Canvas.LineTo(Control.Width-2, Control.Height-2);
      Canvas.LineTo(Control.Width-2, 0);
    end;
    TitledBorder: begin
      Canvas.Font.name:= 'Dialog';
      Canvas.Font.Size:= 11;
      Canvas.Font.Style:= [fsBold];
      Canvas.Pen.Color:= Control.SelectionColor;
      Canvas.Brush.Color:= Control.Background;
      Canvas.Rectangle(getLineRect);
      Canvas.TextOut(9, 4, FTitle);
    end;
    MatteBorder: begin
      aRect:= getLineRect;
      Canvas.Pen.Color:= MatteColor;
      Canvas.Pen.Width:= MatteLeft;
      Canvas.MoveTo(aRect.Left, Control.Height);
      Canvas.LineTo(aRect.Left, 0);
      Canvas.Pen.Width:= MatteTop;
      Canvas.MoveTo(0, aRect.Top);
      Canvas.LineTo(Control.Width, aRect.Top);
      Canvas.Pen.Width:= MatteRight;
      Canvas.MoveTo(aRect.Right, 0);
      Canvas.LineTo(aRect.Right, Control.Height);
      Canvas.Pen.Width:= MatteBottom;
      Canvas.MoveTo(Control.Width, aRect.Bottom);
      Canvas.LineTo(0, aRect.Bottom);
    end;
  end;
end;

{--- TSwingComponent ----------------------------------------------------------}

constructor TSwingComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlueColor:= RGB(99, 130, 191);
  DefaultBackground:= RGB(238, 238, 238);        // EEEEEE
  DefaultForeground:= RGB(51, 51, 51);           // 333333
  FSwingSelectionColor:= RGB(184, 207, 229);      // B8CFE5
  FSwingSelectionBackground:= RGB(163, 184, 204); // A3B8CC
  FOpaque:= true;

  Foreground:= DefaultForeground;
  Background:= DefaultBackground;
  Font.Style:= [fsBold];
  FBorder:= TBorder.Create(Self);
end;

procedure TSwingComponent.CreateFromJ(Control: TControl);
begin
  CreateFromA(Control);
  ToolTipText:= Control.Hint;
end;

function TSwingComponent.getAttributes(ShowAttributes: integer): string;
begin
  case ShowAttributes of
    1: Result:= '';
    2: Result:= '|ToolTipText';
  else Result:= '|ToolTipText|Opaque';
  end;
  Result:= Result + inherited;
end;

procedure TSwingComponent.setAttribute(Attr, Value, Typ: string);
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

function TSwingComponent.getEvents(ShowEvents: integer): string;
begin
  if ShowEvents < 3
    then Result:= ''
    else Result:= '|ancestorAdded|ancestorRemoved|vetoableChange';
  Result:= Result + inherited getEvents(ShowEvents);
end;

destructor TSwingComponent.Destroy;
begin
  FreeAndNil(FBorder);
  inherited;
end;

procedure TSwingComponent.Paint;
begin
  inherited;
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width-1, Height-1));
  if assigned(FBorder) then begin
    case FBorder.Bordertype of
      EtchedBorder: begin
        Canvas.Brush.Color:= ChangeColor(Background, 1.428);
        Canvas.FrameRect(Rect(1, 1, Width, Height));
        Canvas.Brush.Color:= ChangeColor(Background, 0.7);
        Canvas.FrameRect(Rect(0, 0, Width-1, Height-1));
      end;
      BevelBorder: begin // lowered
        Canvas.MoveTo(0, Height-1);
        Canvas.Pen.Color:= ChangeColor(Background, 2);
        Canvas.LineTo(Width-1, Height-1);
        Canvas.LineTo(Width-1, 0);
        Canvas.Pen.Color:= ChangeColor(Background, 0.7);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(0, Height);

        Canvas.MoveTo(1, Height-2);
        Canvas.Pen.Color:= ChangeColor(Background, 1.428);
        Canvas.LineTo(Width-2, Height-2);
        Canvas.LineTo(Width-2, 1);
        Canvas.Pen.Color:= ChangeColor(Background, 0.4857);
        Canvas.LineTo(1, 1);
        Canvas.LineTo(1, Height-2);
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
      TitledBorder: begin
        Canvas.Brush.Color:= FSwingSelectionColor;
        Canvas.FrameRect(Rect(2, 8, Width-3, Height-3));
        Canvas.Font:= Font;
        Canvas.Font.Color:= DefaultForeground;
        Canvas.Brush.Color:= Background;
        Canvas.TextOut(8, 2, FBorder.Title);
      end;
    end;
  end;
end;

procedure TSwingComponent.setBorder(aValue: TBorder);
begin
  FBorder:= aValue;
  Invalidate;
end;

procedure TSwingComponent.MakeIcon(const Attr, Value, Typ: string);
  var s, key, at, Dest, filename, Path: string;
begin
  if Value = '(Icon)' then begin
    key:= 'private ImageIcon ' + Name + Attr;
    Partner.DeleteAttribute(key);
    Partner.DeleteAttributeValue(Name + '.set' + Attr + '(');
  end else begin
    filename:= ExtractFileName(Value);
    if Pos('images/', filename) = 1 then
      System.delete(filename, 1, 7);
    Path:= ExtractFilePath(Partner.Pathname);
    Dest:= Path + 'images\' + filename;
    ForceDirectories(Path + 'images\');
    if not FileExists(Dest) then
      copyFile(PChar(Value), PChar(Dest), true);
    filename:= 'images/' + filename;
    FObjectInspector.ELPropertyInspector.SetByCaption(Attr, filename);

    at:= 'private ' + Typ + ' ' + Name;
    key:= 'private ImageIcon ' + Name + Attr;
    s:= Indent2 + Key + ' = new ImageIcon(getClass().getResource("' + filename + '"));';
    Partner.ReplaceAttributAt(at, key, s);
    MakeAttribut(Attr, Name + Attr);
  end;
end;

procedure TSwingComponent.MakeKeyEvent(const Attr, Value: string);
  var key, s: string;
begin
  key:= Name + '.set' + Attr;
  if Value = '(None)'
    then s:= ''
    else s:= Indent2 + key + '(KeyEvent.VK_' + Value+ ');';
  setAttributValue(key, s);
end;

procedure TSwingComponent.MakeScrollbarDisplayPolicy(const Attr, Value: string);
  var s, key: string;
begin
  key:= Name + 'ScrollPane.set' + Attr;
  if Attr = 'HorizontalScrollBarPolicy'
   then s:= Indent2 + key + '(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_' + Value + ');'
   else s:= Indent2 + key + '(ScrollPaneConstants.VERTICAL_SCROLLBAR_' + Value + ');';
  setAttributValue(key, s);
end;

procedure TSwingComponent.MakeBorder(Border: TBorder);
  var key, s: string;
begin
  key:= Name + '.setBorder(';
  case Border.BorderType of
    NoBorder:     s:= '';
    LineBorder:   begin
      s:= 'new javax.swing.border.LineBorder(' + toJavaColor(Border.LineColor);
      if Border.LineThickness <> 1 then
        s:= s + ', ' + IntToStr(Border.LineThickness);
      if Border.LineRounded then
        s:= s + ', true';
      s:= s + '));';
    end;
    EtchedBorder:
      s:= 'BorderFactory.createEtchedBorder(' + IntToStr(Border.Etchtype) + ', ' +
          toJavaColor(Border.EtchHighlightColor) + ', ' +
          toJavaColor(Border.EtchShadowColor) + '));';
    BevelBorder:
      s:= 'BorderFactory.createBevelBorder(' + IntToStr(Border.Beveltype) + ', ' +
          toJavaColor(Border.BevelHighlightColor) + ', ' +
          toJavaColor(Border.BevelShadowColor) + '));';
    TitledBorder: s:= 'BorderFactory.createTitledBorder("' + Border.Title + '"));';
    MatteBorder:
      s:= 'BorderFactory.createMatteBorder(' + IntToStr(Border.MatteTop) + ', ' +
           IntToStr(Border.MatteLeft) + ', ' + IntToStr(Border.MatteBottom) + ', ' +
           IntToStr(Border.MatteRight) + ', ' + toJavaColor(Border.MatteColor) + '));';
  end;
  if s <> '' then
    s:= Indent2 + key + s;
  setAttributValue(key, s);
end;

procedure TSwingComponent.MakeOrientation(const Value: string);
begin
  MakeAttribut('Orientation', 'SwingConstants.' + Value);
  SetPositionAndSize;
end;

procedure TSwingComponent.Rename(const OldName, NewName, Events: string);

  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(FancestorAdded);
  rename(FancestorRemoved);
  rename(FStateChanged);
  rename(FVetoableChange);
  rename(FValueChanged);
end;

end.
