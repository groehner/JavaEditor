unit UGUIForm;

interface

uses Messages, Windows, Classes, Graphics, Forms, Controls, UBaseForm,
  Vcl.StdCtrls, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TFGUIForm = class (TFForm)
    procedure FormClose(Sender: TObject; var aAction: TCloseAction); override;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
  private
    FResizable: boolean;
    FUndecorated: boolean;
    FTitle: string;
    FFontSize: integer;
    FancestorMoved: string;
    FancestorResized: string;
    FcaretPositionChanged: string;
    FcomponentAdded: string;
    FcomponentRemoved: string;
    FcomponentHidden: string;
    FcomponentMoved: string;
    FcomponentResized: string;
    FcomponentShown: string;
    FFocusGained: string;
    FFocusLost: string;
    FhierarchyChanged: string;
    FinputMethodTextChanged: string;
    FkeyPressed: string;
    FkeyReleased: string;
    FkeyTyped: string;
    FmouseClicked: string;
    FmouseDragged: string;
    FmouseEntered: string;
    FmouseExited: string;
    FmouseMoved: string;
    FmousePressed: string;
    FmouseReleased: string;
    FmouseWheelMoved: string;
    FpropertyChange: string;
    FwindowActivated: string;
    FwindowClosed: string;
    FwindowClosing: string;
    FwindowDeactivated: string;
    FwindowDeiconified: string;
    FwindowGainedFocus: string;
    FwindowIconified: string;
    FwindowLostFocus: string;
    FwindowOpened: string;
    FwindowStateChanged: string;

    function getBackground: TColor;
    procedure setBackground(aValue: TColor);
    function toJavaColor(col: string): string;
    procedure SetGridOptions;
    procedure getFontSize;
  public
    ReadOnly: boolean;
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string;
    function getEvents(ShowEvents: integer): string;
    procedure setAttribute(Attr, Value, Typ: string); virtual;
    procedure Open(const Filename: string; State: string); virtual;
    procedure Enter(Sender: TObject); override;
    procedure Save(MitBackup: boolean); override;
    procedure SaveIn(const Dir: string); override;
    procedure Change(const NewFilename: string);
    function GetSaveAsName: string; override;
    procedure SaveAs(const Filename: string);
    function GetFormType: string; override;
    procedure Print; override;
    procedure UpdateState; override;
    procedure Zooming(_in: boolean);
    procedure DeleteGUIComponent(const aName: string);
    procedure EnsureOnDesktop;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure Paint; override;
    procedure SetBoundsForFormular;
    procedure SetOptions; override;
    procedure DPIChanged; override;
    procedure Scale(NewPPI, OldPPI: integer);
    procedure EndOfResizeMoveDetected(var Msg: Tmessage); message WM_EXITSIZEMOVE;
    function GetFrameType: Integer;
  published
    property Resizable: boolean read FResizable write FResizable;
    property Undecorated: boolean read FUndecorated write FUndecorated;
    property Background: TColor read getBackground write setBackground;
    property Title: string read FTitle write FTitle;
    property FontSize: integer read FFontSize write FFontSize;

    property ancestorMoved: string read FancestorMoved write FancestorMoved;
    property ancestorResized: string read FancestorResized write FancestorResized;
    property caretPositionChanged: string read FcaretPositionChanged write FcaretPositionChanged;
    property componentAdded: string read FcomponentAdded write FcomponentAdded;
    property componentRemoved: string read FcomponentRemoved write FcomponentRemoved;
    property componentHidden: string read FcomponentHidden write FcomponentHidden;
    property componentMoved: string read FcomponentMoved write FcomponentMoved;
    property componentResized: string read FcomponentResized write FcomponentResized;
    property componentShown: string read FcomponentShown write FcomponentShown;
    property focusGained: string read FFocusGained write FFocusGained;
    property focusLost: string read FFocusLost write FFocusLost;
    property hierarchyChanged: string read FhierarchyChanged write FhierarchyChanged;
    property inputMethodTextChanged: string read FinputMethodTextChanged write FinputMethodTextChanged;
    property keyPressed: string read FkeyPressed write FkeyPressed;
    property keyReleased: string read FkeyReleased write FkeyReleased;
    property keyTyped: string read FkeyTyped write FkeyTyped;
    property mouseClicked: string read FmouseClicked write FmouseClicked;
    property mouseEntered: string read FmouseEntered write FmouseEntered;
    property mouseExited: string read FmouseExited write FmouseExited;
    property mousePressed: string read FmousePressed write FmousePressed;
    property mouseReleased: string read FmouseReleased write FmouseReleased;
    property mouseDragged: string read FmouseDragged write FmouseDragged;
    property mouseMoved: string read FmouseMoved write FmouseMoved;
    property mouseWheelMoved: string read FmouseWheelMoved write FmouseWheelMoved;
    property propertyChange: string read FpropertyChange write FpropertyChange;
    property windowActivated: string read FwindowActivated write FwindowActivated;
    property windowClosed: string read FwindowClosed write FwindowClosed;
    property windowClosing: string read FwindowClosing write FwindowClosing;
    property windowDeactivated: string read FwindowDeactivated write FwindowDeactivated;
    property windowDeiconified: string read FwindowDeiconified write FwindowDeiconified;
    property windowGainedFocus: string read FwindowGainedFocus write FwindowGainedFocus;
    property windowIconified: string read FwindowIconified write FwindowIconified;
    property windowLostFocus: string read FwindowLostFocus write FwindowLostFocus;
    property windowOpened: string read FwindowOpened write FwindowOpened;
    property windowStateChanged: string read FwindowStateChanged write FwindowStateChanged;
  end;

implementation

uses SysUtils, ComCtrls, Clipbrd, Dialogs, Math, UXTheme, System.Generics.Collections,
     JvGnugettext, UStringRessources,
     SpTBXTabs, UJava, UGUIDesigner, UObjectInspector, UEditorForm, UMessages,
     UUtils, UObjectGenerator, UAComponents, UJEComponents, UConfiguration;

{$R *.DFM}

constructor TFGUIForm.Create(AOwner: TComponent);
begin
  inherited;
  FormTag:= 3;
  Resizable:= true;
  // don't theme this window
  SetWindowTheme(Handle, nil, nil);
  SetGridOptions;
end;

procedure TFGUIForm.Open(const Filename: String; State: string);
begin
  var Animation:= GetAnimation;
  if Animation then
    SetAnimation(false);
  // inherited Create(AOwner);
  Pathname:= Filename;
  Caption:= ChangeFileExt(ExtractFilename(Filename), '');
  Title:= Caption;
  Modified:= false;
  OnMouseActivate:= FormMouseActivate;

  FJava.ConnectGUIAndJavaWindow(Self);
  if (Partner as TFEditForm).isAWT then
    Background:= clWhite;
  SetState(State);
  Enter(Self); // must stay!
  if Animation then
    SetAnimation(true);
  ReadOnly:= IsWriteProtected(Filename);
  if GetFrametype in [4, 7] then
    Caption:= '';
  if FontSize = 0 then
    GetFontSize;
end;

procedure TFGUIForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FGUIDesigner.ELDesigner.Active:= false;
  FGUIDesigner.ELDesigner.DesignControl:= nil;
  FGUIDesigner.DesignForm:= nil;
  FObjectInspector.RefreshCBObjects;
  Save(true);
  CanClose:= true;
  FJava.ActiveTool:= -1;
end;

procedure TFGUIForm.FormCreate(Sender: TObject);
begin
  inherited;
  TranslateComponent(Self);
end;

procedure TFGUIForm.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  inherited;
  FJava.ActiveTool:= 17;
end;

procedure TFGUIForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if FGUIDesigner.ELDesigner.Active then
    FGUIDesigner.ELDesigner.Active:= false;
end;

procedure TFGUIForm.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  FJava.TDIFormsList.Remove(Self);
  if Assigned(Partner) then
    Partner.Partner:= nil;
  for var i:= 1 to maxTab do  // 0 is tab Program
    FJava.TabsControl.Items[i].Visible:= FConfiguration.vistabs[i];
  aAction:= caFree;
end;

function TFGUIForm.getBackground: TColor;
begin
  Result:= Color;
end;

procedure TFGUIForm.setBackground(aValue: TColor);
begin
  Color:= aValue;
end;

{$WARNINGS OFF}
procedure TFGUIForm.Save(MitBackup: boolean);
  var BackupName: string;
begin
  if ReadOnly then exit;
  if MitBackup then begin
    BackupName:= Pathname;
    BackupName:= ChangeFileExt(BackupName, '.~fm');
    if FileExists(BackupName) then
      SysUtils.DeleteFile(BackupName);
    if FileExists(Pathname) then
      RenameFile(Pathname, BackupName);
  end;
  FGUIDesigner.Save(Pathname, Self);
  FMessages.StatusMessage(Pathname + ' ' + _(LNGSaved));
  Modified:= false;
end;
{$WARNINGS ON}

procedure TFGUIForm.SaveAs(const Filename: string);
begin
  FObjectInspector.RefreshCBObjects;
  Pathname:= Filename;
  ReadOnly:= false;
  Caption:= ChangeFileExt(ExtractFilename(Filename), '');
  Title:= Caption;
  Save(WithoutBackup);
end;

procedure TFGUIForm.SaveIn(const Dir: string);
begin
  SaveAs(Dir + ExtractFilename(Pathname));
end;

procedure TFGUIForm.Change(const NewFilename: string);
begin
  var old:= Pathname;
  SaveAs(NewFilename);
  DeleteFile(PCHar(old));
end;

function TFGUIForm.GetSaveAsName: string;
begin
  Result:= Pathname;
end;

procedure TFGUIForm.Enter(Sender: TObject);
begin
  FJava.DisableUpdateMenuItems;
  if assigned(Partner) then begin
    var Form:= FJava.getTDIWindow(Partner.Pathname);
    if assigned(Form) then
      FJava.SetSelectedTabAndWindow(Form.Number);
    FJava.myTabBarClick(Self);
  end;
  if not FObjectInspector.Visible then
    FObjectInspector.ShowIt;
  inherited;
  if (FGUIDesigner.ELDesigner.DesignControl <> Self) or not FGUIDesigner.ELDesigner.Active then
    FGUIDesigner.ChangeTo(Self);
  FJava.ShowAWTSwingOrFX(GetFrameType);
  FJava.EnableUpdateMenuItems;
end;

procedure TFGUIForm.SetGridOptions;
begin
  FGUIDesigner.ELDesigner.SnapToGrid:= FConfiguration.SnapToGrid;
  FGUIDesigner.ELDesigner.Grid.XStep:= PPIScale(FConfiguration.GridSize);
  FGUIDesigner.ELDesigner.Grid.YStep:= PPIScale(FConfiguration.GridSize);
end;

procedure TFGUIForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  FGUIDesigner.ScaleImages;
  Invalidate;
  SetGridOptions;
  OnResize:= FormResize;
end;

procedure TFGUIForm.FormBeforeMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  OnResize:= nil;
end;

procedure TFGUIForm.FormResize(Sender: TObject);
begin
  FObjectInspector.ELPropertyInspector.Modified;
  if Assigned(Partner) and not ReadOnly then begin
    FObjectGenerator.Partner:= TFEditForm(Partner);
    SetBoundsForFormular;
  end;
  Modified:= true;
  UpdateState;
end;

procedure TFGUIForm.SetBoundsForFormular;
  var s1, s2: string; EditForm, HTMLPartner: TFEditForm;
begin
  EditForm:= Partner as TFEditForm;
  if GetFrameType in [2, 3, 5, 6] then begin
    s1:= 'int frameWidth';
    s2:= FConfiguration.Indent2 + 'int frameWidth = ' + IntToStr(PPIUnScale(Width)) + '; ';
    EditForm.ReplaceLine(s1, s2);
    s1:= 'int frameHeight';
    s2:= FConfiguration.Indent2 + 'int frameHeight = ' + IntToStr(PPIUnScale(Height)) + ';';
    EditForm.ReplaceLine(s1, s2);
  end else if GetFrameType in [4, 7] then begin // Applet, JApplet
    s1:= ' cp.setBounds(';
    s2:= FConfiguration.Indent2 + 'cp.setBounds(0, 0, ' + IntToStr(PPIUnScale(Width)) + ', ' +
         IntToStr(PPIUnScale(Height)) + ');';
    EditForm.ReplaceLine(s1, s2);
    s1:= ChangeFileExt(EditForm.Pathname, '.html');
    HTMLPartner:= TFEditForm(FJava.getTDIWindowType(s1, '%E%'));
    if Assigned(HTMLPartner) then
      HTMLPartner.ReplaceWidthHeight(Width, Height);
  end else if GetFrameType = 8 then begin // JavaFX
    s1:= 'Scene scene = new Scene(root';
    s2:= FConfiguration.Indent2 + 'Scene scene = new Scene(root, ' +
           IntToStr(PPIUnScale(Width-16)) + ', ' + IntToStr(PPIUnScale(Height-38)) + ');';
    EditForm.ReplaceLine(s1, s2);
  end;
end;

procedure TFGUIForm.SetOptions;
begin
  SetGridOptions;
end;

function TFGUIForm.GetFormType: string;
begin
  Result:= '%G%'
end;

procedure TFGUIForm.Print;
begin
  inherited Print;
end;

procedure TFGUIForm.DeleteGUIComponent(const aName: string);
begin
  for var i:= ComponentCount - 1 downto 0 do begin
    var Temp:= Components[I];
    if Temp.Name = aName then begin
      FreeAndNil(Temp);
      exit;
    end;
  end;
end;

procedure TFGUIForm.CutToClipboard;
begin
  FGUIDesigner.CutClick(Self);
end;

procedure TFGUIForm.CopyToClipboard;
begin
  if FGUIDesigner.ELDesigner.CanCopy then
    FGUIDesigner.CopyClick(Self)
  else begin
    var BM:= GetFormImage();
    Clipboard.Assign(BM);
    FreeAndNil(BM);
  end;
end;

procedure TFGUIForm.PasteFromClipboard;
begin
  if not ReadOnly then
    FGUIDesigner.PasteClick(Self);
end;

procedure TFGUIForm.UpdateState;
begin
  inherited;
  FGuiDesigner.UpdateState(false);
end;

procedure TFGuiForm.Zooming(_in: boolean);
begin
  (Partner as TFEditForm).Editor.BeginUpdate;
  for var i:= 0 to ComponentCount - 1 do
    if Components[i] is TJEComponent then
      (Components[i] as TJEComponent).Zooming(_in);
  if _in
    then FontSize:= FontSize + FConfiguration.ZoomSteps
    else FontSize:= max(FontSize - FConfiguration.ZoomSteps, 6);
  (Partner as TFEditForm).Editor.EndUpdate;
end;

procedure TFGUIForm.EnsureOnDesktop;
  var l, t, w, h: integer;
begin
  w:= width;
  h:= Height;
  l:= Left;
  if l < 0 then l:= 0;
  if l + w > Screen.DesktopWidth then
    l:= Screen.DesktopWidth - w;
  t:= Top;
  if t < 0 then t:= 0;
  if t + h > Screen.DesktopHeight then
    t:= Screen.DesktopHeight - h;
  SetBounds(l, t, w, h);
end;

procedure TFGUIForm.Paint;
begin
  inherited;
  Canvas.FillRect(ClientRect);
end;

function TFGuiForm.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|Background|Height|Resizable|Title|Undecorated|Width|';
end;

function TFGuiForm.getEvents(ShowEvents: integer): string;
begin
  case ShowEvents of
    1: Result:= '';
    2: Result:= FocusEvents + KeyEvents + MouseEvents;
    3: Result:= FocusEvents + KeyEvents + MouseEvents +
                HierarchyEvents + InputMethodEvents + PropertyEvents +
                AncestorEvents1 + CaretEvents + ComponentEvents;
  end;
  if not GetFrameType in [4, 7] then
    Result:= Result + WindowEvents;
  Result:= Result + '|';
end;

procedure TFGuiForm.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Title' then
    Caption:= Value;
  var s1:= 'set' + Attr;
  var s2:= '';
  if (Typ = 'Integer') or (Typ = 'Boolean') then
    s2:= FConfiguration.Indent2 + s1 + '(' + Value + ');'
  else if Typ = 'string' then
    s2:= FConfiguration.Indent2 + s1 + '("' + Value + '");'
  else if Attr = 'Background' then begin
    if Value = '(NONE)' then
      (Partner as TFEditForm).DeleteAttributeValue('cp.setBackground')
    else begin
      s1:= 'cp.setBackground';
      s2:= FConfiguration.Indent2 + s1 + '(' + toJavaColor(Value) + ');';
    end;
  end;
  if s2 <> '' then
    (Partner as TFEditForm).setAttributValue(_(LNGStartComponents), s1, s2, 0);
end;

function TFGuiForm.toJavaColor(col: string): string;
begin
  if copy(col, 1, 2) = '0x'
    then Result:= 'new Color(' + col + ')'
    else Result:= 'Color.' + col;
end;

procedure TFGuiForm.DPIChanged;
begin
  // do nothing
end;

type
  TControlClass = class(TControl);

procedure TFGuiForm.Scale(NewPPI, OldPPI: integer);
begin
  if NewPPI = OldPPI then
    exit;
  FIScaling := True;
  try
    ScaleScrollBars(NewPPI, OldPPI);
    ScaleConstraints(NewPPI, OldPPI);
    ClientWidth := MulDiv(ClientWidth, NewPPI, OldPPI);
    ClientHeight := MulDiv(ClientHeight, NewPPI, OldPPI);
    for var I := 0 to ComponentCount - 1 do
      if Components[I] is TControl then
        TControlClass(Components[I]).ChangeScale(NewPPI, OldPPI, True);
    if not ParentFont then
      Font.Height := MulDiv(Font.Height, NewPPI, OldPPI);
    Realign;
  finally
    FIScaling := False;
  end;
end;

function TFGuiForm.getFrameType: Integer;
begin
  if assigned(Partner)
    then Result:= (Partner as TFEditForm).FrameType
    else Result:= 0;
end;

procedure TFGUIForm.EndOfResizeMoveDetected(var Msg: Tmessage);
begin
  FGUIDesigner.ELDesigner.Active:= true;
end;

procedure TFGUIForm.getFontSize;
  var CompFontSize, Value, Key, MaxFontCount, MaxFontKey: integer;
      FontSizeDictionary: TDictionary<Integer, Integer>;
begin
  FontSizeDictionary := TDictionary<Integer, Integer>.Create(20);
  for var i:= 0 to ComponentCount - 1 do
    if Components[i] is TJEComponent then begin
      CompFontSize:= (Components[i] as TJEComponent).Font.Size;
      if FontSizeDictionary.TryGetValue(CompFontSize, Value) then
        FontSizeDictionary.AddOrSetValue(CompFontSize, Value +1)
      else
        FontSizeDictionary.AddOrSetValue(CompFontSize, 1);
    end;
  MaxFontCount:= 0;
  MaxFontKey:= 0;
  for Key in FontSizeDictionary.Keys do
    if FontSizeDictionary.Items[Key] > MaxFontCount then begin
      MaxFontCount:= FontSizeDictionary.Items[Key];
      MaxFontKey:= Key;
    end;
  if MaxFontKey > 0
    then FontSize:= MaxFontKey
    else FontSize:= FConfiguration.GUIFontSize;
  FontSizeDictionary.Free;
end;

end.

