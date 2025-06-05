unit UGUIForm;

interface

uses
  Messages,
  Classes,
  Graphics,
  Forms,
  Controls,
  UBaseForm;

type
  TFGUIForm = class (TFForm)
    procedure FormClose(Sender: TObject; var AAction: TCloseAction); override;
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
    FResizable: Boolean;
    FUndecorated: Boolean;
    FTitle: string;
    FFontSize: Integer;
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
    FReadOnly: Boolean;
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

    function GetBackground: TColor;
    procedure SetBackground(AValue: TColor);
    function ToJavaColor(Col: string): string;
    procedure SetGridOptions;
    procedure GetFontSize;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string;
    function GetEvents(ShowEvents: Integer): string;
    procedure SetAttribute(Attr, Value, Typ: string); virtual;
    procedure Open(const FileName: string; State: string); virtual;
    procedure Enter(Sender: TObject); override;
    procedure Save(WithBackup: Boolean); override;
    procedure SaveIn(const Dir: string); override;
    procedure Change(const NewFilename: string);
    function GetSaveAsName: string; override;
    procedure SaveAs(const FileName: string);
    function GetFormType: string; override;
    procedure UpdateState; override;
    procedure Zooming(InOut: Boolean);
    procedure DeleteGUIComponent(const AName: string);
    procedure EnsureOnDesktop;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure Paint; override;
    procedure SetBoundsForFormular;
    procedure SetOptions; override;
    procedure DPIChanged; override;
    procedure Scale(NewPPI, OldPPI: Integer);
    procedure EndOfResizeMoveDetected(var Msg: TMessage); message WM_EXITSIZEMOVE;
    function GetFrameType: Integer;
    property ReadOnly: Boolean read FReadOnly;// write FReadOnly;
  published
    property Resizable: Boolean read FResizable write FResizable;
    property Undecorated: Boolean read FUndecorated write FUndecorated;
    property Background: TColor read GetBackground write SetBackground;
    property Title: string read FTitle write FTitle;
    property FontSize: Integer read FFontSize write FFontSize;

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

uses
  Windows,
  SysUtils,
  Clipbrd,
  Math,
  Winapi.UxTheme,
  System.Generics.Collections,
  JvGnugettext,
  UUtils,
  UJava,
  UGUIDesigner,
  UObjectInspector,
  UEditorForm,
  UMessages,
  UStringRessources,
  UObjectGenerator,
  UAComponents,
  UJEComponents,
  UConfiguration;

{$R *.DFM}

constructor TFGUIForm.Create(AOwner: TComponent);
begin
  inherited;
  FormTag:= 3;
  Resizable:= True;
  // don't theme this window
  SetWindowTheme(Handle, nil, nil);
  SetGridOptions;
end;

procedure TFGUIForm.Open(const FileName: string; State: string);
begin
  var Animation:= GetAnimation;
  if Animation then
    SetAnimation(False);
  Pathname:= FileName;
  Caption:= ChangeFileExt(ExtractFileName(FileName), '');
  Title:= Caption;
  Modified:= False;
  OnMouseActivate:= FormMouseActivate;

  FJava.ConnectGUIAndJavaWindow(Self);
  if (Partner as TFEditForm).IsAWT then
    Background:= clWhite;
  SetState(State);
  Enter(Self); // must stay!
  if Animation then
    SetAnimation(True);
  FReadOnly:= IsWriteProtected(FileName);
  if GetFrameType in [4, 7] then
    Caption:= '';
  if FontSize = 0 then
    GetFontSize;
end;

procedure TFGUIForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FGUIDesigner.ELDesigner.Active:= False;
  FGUIDesigner.ELDesigner.DesignControl:= nil;
  FGUIDesigner.DesignForm:= nil;
  FObjectInspector.RefreshCBObjects;
  Save(True);
  CanClose:= True;
  FJava.ActiveTool:= -1;
end;

procedure TFGUIForm.FormCreate(Sender: TObject);
begin
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
    FGUIDesigner.ELDesigner.Active:= False;
end;

procedure TFGUIForm.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  FJava.TDIFormsList.Remove(Self);
  if Assigned(Partner) then
    Partner.Partner:= nil;
  for var I:= 1 to MaxTab do  // 0 is tab Program
    FJava.TabsControl.Items[I].Visible:= FConfiguration.VisTabs[I];
  AAction:= caFree;
end;

function TFGUIForm.GetBackground: TColor;
begin
  Result:= Color;
end;

procedure TFGUIForm.SetBackground(AValue: TColor);
begin
  Color:= AValue;
end;

procedure TFGUIForm.Save(WithBackup: Boolean);
  var BackupName: string;
begin
  if ReadOnly then Exit;
  if WithBackup then begin
    BackupName:= Pathname;
    BackupName:= ChangeFileExt(BackupName, '.~fm');
    if FileExists(BackupName) then
      SysUtils.DeleteFile(BackupName);
    if FileExists(Pathname) then
      RenameFile(Pathname, BackupName);
  end;
  FGUIDesigner.Save(Pathname, Self);
  FMessages.StatusMessage(Pathname + ' ' + _(LNGSaved));
  Modified:= False;
end;

procedure TFGUIForm.SaveAs(const FileName: string);
begin
  FObjectInspector.RefreshCBObjects;
  Pathname:= FileName;
  FReadOnly:= False;
  Caption:= ChangeFileExt(ExtractFileName(FileName), '');
  Title:= Caption;
  Save(WithoutBackup);
end;

procedure TFGUIForm.SaveIn(const Dir: string);
begin
  SaveAs(Dir + ExtractFileName(Pathname));
end;

procedure TFGUIForm.Change(const NewFilename: string);
begin
  var Old:= Pathname;
  SaveAs(NewFilename);
  DeleteFile(PChar(Old));
end;

function TFGUIForm.GetSaveAsName: string;
begin
  Result:= Pathname;
end;

procedure TFGUIForm.Enter(Sender: TObject);
begin
  FJava.DisableUpdateMenuItems;
  if Assigned(Partner) then begin
    var Form:= FJava.GetTDIWindow(Partner.Pathname);
    if Assigned(Form) then
      FJava.SetSelectedTabAndWindow(Form.Number);
    FJava.MyTabBarClick(Self);
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
  Modified:= True;
  UpdateState;
end;

procedure TFGUIForm.SetBoundsForFormular;
  var Str1, Str2: string; EditForm, HTMLPartner: TFEditForm;
begin
  EditForm:= Partner as TFEditForm;
  if GetFrameType in [2, 3, 5, 6] then begin
    Str1:= 'int frameWidth';
    Str2:= FConfiguration.Indent2 + 'int frameWidth = ' + IntToStr(PPIUnScale(Width)) + '; ';
    EditForm.ReplaceLine(Str1, Str2);
    Str1:= 'int frameHeight';
    Str2:= FConfiguration.Indent2 + 'int frameHeight = ' + IntToStr(PPIUnScale(Height)) + ';';
    EditForm.ReplaceLine(Str1, Str2);
  end else if GetFrameType in [4, 7] then begin // Applet, JApplet
    Str1:= ' cp.setBounds(';
    Str2:= FConfiguration.Indent2 + 'cp.setBounds(0, 0, ' + IntToStr(PPIUnScale(Width)) + ', ' +
         IntToStr(PPIUnScale(Height)) + ');';
    EditForm.ReplaceLine(Str1, Str2);
    Str1:= ChangeFileExt(EditForm.Pathname, '.html');
    HTMLPartner:= TFEditForm(FJava.GetTDIWindowType(Str1, '%E%'));
    if Assigned(HTMLPartner) then
      HTMLPartner.ReplaceWidthHeight(Width, Height);
  end else if GetFrameType = 8 then begin // JavaFX
    Str1:= 'Scene scene = new Scene(root';
    Str2:= FConfiguration.Indent2 + 'Scene scene = new Scene(root, ' +
           IntToStr(PPIUnScale(Width-16)) + ', ' + IntToStr(PPIUnScale(Height-38)) + ');';
    EditForm.ReplaceLine(Str1, Str2);
  end;
end;

procedure TFGUIForm.SetOptions;
begin
  SetGridOptions;
end;

function TFGUIForm.GetFormType: string;
begin
  Result:= '%G%';
end;

procedure TFGUIForm.DeleteGUIComponent(const AName: string);
begin
  for var I:= ComponentCount - 1 downto 0 do begin
    var Temp:= Components[I];
    if Temp.Name = AName then begin
      FreeAndNil(Temp);
      Exit;
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
    var Bitmap:= GetFormImage;
    Clipboard.Assign(Bitmap);
    FreeAndNil(Bitmap);
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
  FGUIDesigner.UpdateState(False);
end;

procedure TFGUIForm.Zooming(InOut: Boolean);
begin
  (Partner as TFEditForm).Editor.BeginUpdate;
  for var I:= 0 to ComponentCount - 1 do
    if Components[I] is TJEComponent then
      (Components[I] as TJEComponent).Zooming(InOut);
  if InOut
    then FontSize:= FontSize + FConfiguration.ZoomSteps
    else FontSize:= Max(FontSize - FConfiguration.ZoomSteps, 6);
  (Partner as TFEditForm).Editor.EndUpdate;
end;

procedure TFGUIForm.EnsureOnDesktop;
  var Left1, Top1: Integer;
begin
  Left1:= Left;
  if Left1 < 0 then
    Left1:= 0;
  if Left1 + Width > Screen.DesktopWidth then
    Left1:= Screen.DesktopWidth - Width;
  Top1:= Top;
  if Top1 < 0 then Top1:= 0;
  if Top1 + Height > Screen.DesktopHeight then
    Top1:= Screen.DesktopHeight - Height;
  SetBounds(Left1, Top1, Width, Height);
end;

procedure TFGUIForm.Paint;
begin
  inherited;
  Canvas.FillRect(ClientRect);
end;

function TFGUIForm.GetAttributes(ShowAttributes: Integer): string;
begin
  Result:= '|Background|Height|Resizable|Title|Undecorated|Width|';
end;

function TFGUIForm.GetEvents(ShowEvents: Integer): string;
begin
  case ShowEvents of
    1: Result:= '';
    2: Result:= FocusEvents + KeyEvents + MouseEvents;
    3: Result:= FocusEvents + KeyEvents + MouseEvents +
                HierarchyEvents + InputMethodEvents + PropertyEvents +
                AncestorEvents1 + CaretEvents + ComponentEvents;
  end;
  if not (GetFrameType in [4, 7]) then
    Result:= Result + WindowEvents;
  Result:= Result + '|';
end;

procedure TFGUIForm.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Title' then
    Caption:= Value;
  var Str1:= 'set' + Attr;
  var Str2:= '';
  if (Typ = 'Integer') or (Typ = 'Boolean') then
    Str2:= FConfiguration.Indent2 + Str1 + '(' + Value + ');'
  else if Typ = 'string' then
    Str2:= FConfiguration.Indent2 + Str1 + '("' + Value + '");'
  else if Attr = 'Background' then begin
    if Value = '(NONE)' then
      (Partner as TFEditForm).DeleteAttributeValue('cp.setBackground')
    else begin
      Str1:= 'cp.setBackground';
      Str2:= FConfiguration.Indent2 + Str1 + '(' + ToJavaColor(Value) + ');';
    end;
  end;
  if Str2 <> '' then
    (Partner as TFEditForm).SetAttributValue(_(LNGStartComponents), Str1, Str2, 0);
end;

function TFGUIForm.toJavaColor(Col: string): string;
begin
  if Copy(Col, 1, 2) = '0x'
    then Result:= 'new Color(' + Col + ')'
    else Result:= 'Color.' + Col;
end;

procedure TFGUIForm.DPIChanged;
begin
  // do nothing
end;

type
  TControlClass = class(TControl);

procedure TFGUIForm.Scale(NewPPI, OldPPI: Integer);
begin
  if NewPPI = OldPPI then
    Exit;
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

function TFGUIForm.GetFrameType: Integer;
begin
  if Assigned(Partner)
    then Result:= (Partner as TFEditForm).FrameType
    else Result:= 0;
end;

procedure TFGUIForm.EndOfResizeMoveDetected(var Msg: TMessage);
begin
  FGUIDesigner.ELDesigner.Active:= True;
end;

procedure TFGUIForm.GetFontSize;
  var CompFontSize, Value, Key, MaxFontCount, MaxFontKey: Integer;
      FontSizeDictionary: TDictionary<Integer, Integer>;
begin
  FontSizeDictionary := TDictionary<Integer, Integer>.Create(20);
  for var I:= 0 to ComponentCount - 1 do
    if Components[I] is TJEComponent then begin
      CompFontSize:= (Components[I] as TJEComponent).Font.Size;
      if FontSizeDictionary.TryGetValue(CompFontSize, Value) then
        FontSizeDictionary.AddOrSetValue(CompFontSize, Value +1)
      else
        FontSizeDictionary.AddOrSetValue(CompFontSize, 1);
    end;
  MaxFontCount:= 0;
  MaxFontKey:= 0;
  for Key in FontSizeDictionary.Keys do
    if FontSizeDictionary[Key] > MaxFontCount then begin
      MaxFontCount:= FontSizeDictionary[Key];
      MaxFontKey:= Key;
    end;
  if MaxFontKey > 0
    then FontSize:= MaxFontKey
    else FontSize:= FConfiguration.GUIFontSize;
  FontSizeDictionary.Free;
end;

end.

