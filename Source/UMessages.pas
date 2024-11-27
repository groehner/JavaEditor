unit UMessages;

// 0: MInterpreter
// 1: LBCompiler

// 2: TVAttributes
// 3: TVLocalVariables
// 4: TVWatchedExpressions
// 5: LBStack

// 6: LBSearch
// 7: LBMessages
// 8: Interactive Editor
// 9: Interactive Variables

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, Menus, ComCtrls, Grids, Contnrs,
  USynEditEx, SynEdit, UExecution, UComJava1,
  Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.BaseImageCollection,
  SVGIconImageCollection, Vcl.VirtualImageList, TB2Dock, TB2Toolbar, SpTBXItem,
  TB2Item;

type
  TInteractiveEdit = class(TSynEditEx)
    constructor Create(AOwner: TComponent); override;
    procedure SystemOutPrintln;
    procedure UpdateState;
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure aKeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Enter(Sender: TObject);
  end;

  TInteractive = class
    UMLForm: TForm;
    InteractiveEditor: TInteractiveEdit;
    SGVariables: TStringGrid;
    Executer: TInteractiveExecuter;
    ComJava: TComJava1;
    constructor create(U: TForm; IE: TInteractiveEdit; SG: TStringGrid; E: TInteractiveExecuter; C: TComJava1);
  end;

  { TFMessages }

  TFMessages = class(TForm)
    PMain: TPanel;
    LBCompiler: TListBox;
    PDebugger: TPanel;
    PDebuggerLeft: TPanel;
    PAttribute: TPanel;
    SplitterLeft: TSplitter;
    PDebuggerCenterLeft: TPanel;
    PLocalVariables: TPanel;
    SplitterCenter: TSplitter;
    PDebuggerCenterRight: TPanel;
    PWatches: TPanel;
    SplitterRight: TSplitter;
    PDebuggerRight: TPanel;
    PStack: TPanel;
    LBStack: TListBox;
    PMMessages: TSpTBXPopupMenu;
    MICopy: TSpTBXItem;
    MIDelete: TSpTBXItem;
    MIDeleteAll: TSpTBXItem;
    LBMessages: TListBox;
    TabControlMessages: TTabControl;
    TVLocalVariables: TTreeView;
    TVAttributes: TTreeView;
    TVWatchedExpressions: TTreeView;
    TVSearch: TTreeView;
    MIFont: TSpTBXItem;
    MICopyAll: TSpTBXItem;
    MIClose: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    MIExpand: TSpTBXItem;
    MICollapse: TSpTBXItem;
    PInterpreter: TPanel;
    SplitterInteractiveLeft: TSplitter;
    PInteractiveLeft: TPanel;
    SplitterInteractiveRight: TSplitter;
    PInteractiveRight: TPanel;
    MInterpreter: TMemo;
    PInteractiveMiddle: TPanel;
    MIPaste: TSpTBXItem;
    TBInteractiveToolbar: TToolBar;
    TBExecute: TToolButton;
    TBShowUML: TToolButton;
    TBJavaReset: TToolButton;
    TBDelete: TToolButton;
    N4: TSpTBXSeparatorItem;
    MIGotoError: TSpTBXItem;
    MIDock: TSpTBXItem;
    MIUndock: TSpTBXItem;
    StatusBar: TStatusBar;
    DebuggerToolbar: TSpTBXToolbar;
    TBStep: TSpTBXItem;
    TBNext: TSpTBXItem;
    TBStepUp: TSpTBXItem;
    TBRunToCursor: TSpTBXItem;
    TBShowExecutionPoint: TSpTBXItem;
    TBExpression: TSpTBXItem;
    TBWatches: TSpTBXItem;
    TBDetails: TSpTBXItem;
    icInteractive: TSVGIconImageCollection;
    vilInteractiveLight: TVirtualImageList;
    vilInteractiveDark: TVirtualImageList;
    vilDebuggerToolbarLight: TVirtualImageList;
    vilDebuggerToolbarDark: TVirtualImageList;
    ToolbarDock: TSpTBXDock;
    icDebuggerToolbar: TSVGIconImageCollection;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    MISameWidth: TSpTBXItem;
    icPMMessages: TSVGIconImageCollection;
    vilPMMessagesDark: TVirtualImageList;
    vilPMMessagesLight: TVirtualImageList;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure LBCompilerDblClick(Sender: TObject);
    procedure TVWatchedExpressionsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TVWatchedExpressionsDblClick(Sender: TObject);
    procedure LBStackDblClick(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure MIDeleteAllClick(Sender: TObject);
    procedure LBMessagesDblClick(Sender: TObject);
    procedure TabControlMessagesChange(Sender: TObject);
    procedure TVLocalVariablesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TVLocalVariablesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TVAttributesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TVAttributesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TVWatchedExpressionsExpanding(Sender: TObject;
      Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TVWatchedExpressionsCollapsing(Sender: TObject;
      Node: TTreeNode; var AllowCollapse: Boolean);
    procedure TVSearchDblClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure MInterpreterKeyPress(Sender: TObject; var Key: Char);
    procedure MICopyAllClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure PMMessagesPopup(Sender: TObject);
    procedure MIExpandClick(Sender: TObject);
    procedure MICollapseClick(Sender: TObject);
    procedure TBExecuteClick(Sender: TObject);
    procedure OnMove(var Msg: TWMMove); message WM_MOVE;
    procedure TVSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LBInteractiveMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnSplitterMoved(Sender: TObject);
    procedure SplitterInteractiveRightMoved(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure TBShowUMLClick(Sender: TObject);
    procedure MInterpreterEnter(Sender: TObject);
    procedure MIDeleteClick(Sender: TObject);
    procedure TBDeleteClick(Sender: TObject);
    procedure TBJavaResetMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MInterpreterDblClick(Sender: TObject);
    procedure MIGotoErrorClick(Sender: TObject);
    procedure TVAttributesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TVLocalVariablesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LBStackKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MIDockClick(Sender: TObject);
    procedure MIUndockClick(Sender: TObject);
    procedure SBStepClick(Sender: TObject);
    procedure SBNextClick(Sender: TObject);
    procedure SBStepUpClick(Sender: TObject);
    procedure SBRunToCursorClick(Sender: TObject);
    procedure SBShowExecutionPointClick(Sender: TObject);
    procedure SBExpressionClick(Sender: TObject);
    procedure SBWatchesClick(Sender: TObject);
    procedure SBDetailsClick(Sender: TObject);
    procedure PMainExit(Sender: TObject);
    procedure LBCompilerEnter(Sender: TObject);
    procedure PDebuggerLeftEnter(Sender: TObject);
    procedure PDebuggerCenterLeftEnter(Sender: TObject);
    procedure PDebuggerCenterRightEnter(Sender: TObject);
    procedure PDebuggerRightEnter(Sender: TObject);
    procedure TVSearchEnter(Sender: TObject);
    procedure LBMessagesEnter(Sender: TObject);
    procedure PInteractiveMiddleEnter(Sender: TObject);
    procedure PInteractiveRightEnter(Sender: TObject);
    procedure StatusBarDrawPanel(aStatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure StringGrid1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure StringGrid1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MISameWidthClick(Sender: TObject);
  private
    initialized: Boolean;
    aPosition: TRect;
    BGColor: TColor;
    FGColor: TColor;

    InteractiveEditors: TStringList;
    InteractiveUMLForms: TStringList;
    InteractiveVariables: TStringList;
    InteractiveExecuters: TObjectList;
    InteractiveComJavas: TObjectList;

    toJavaConsole: string;
    InteractiveWidth1: Integer;
    InteractiveWidth2: Integer;
    fActiveSubTool: integer;
    procedure ShowMessagesOrInterpreter(const aFile: string; OutputNr: integer);
    procedure setActiveSubTool(value: integer);
    procedure SetStatusBarAndTabs;
    procedure ShowInteractiveSplitter;
    procedure HideInteractiveSplitter;
    procedure Expand(Node: TTreeNode; chapter: integer);
    procedure Collapse(Node: TTreeNode; chapter: integer);
    procedure HideIt;
    function GetSelectedLines(ListBox: TListBox): string;
    procedure ScrollEnd(i: Integer);
    function CopyTreeView(TreeView: TTreeView; all: boolean): string;
    function CopyTreeViewSelected(TreeView: TTreeView): string;
    function CopyTreeViewAll(TreeView: TTreeView): string;
    function GetFileWithPath(LB: TListBox; var i: integer): string;
    procedure TreeViewDelete(TreeView: TTreeView; all: boolean);
    procedure TreeViewDeleteSelected(TreeView: TTreeView);
    procedure TreeViewDeleteAll(TreeView: TTreeView);
    procedure TabControlChange(NewTab: integer);
    function GetInteractive(i: integer): TInteractiveEdit;
    procedure SetModifiedUMLForm(i: integer);
    procedure AdjustVariablesWidths(Tab: integer);
  public
    ActiveInteractive: TInteractiveEdit;
    Undocking: boolean;
    Expanded: array[1..3] of TStringList;
    DumpActive: boolean;
    SearchGoalLine: Integer;
    SearchGoalPath: string;
    procedure ShowIt;
    procedure UpdateState;
    procedure ShowTab(i: Integer); overload;
    procedure ShowTab(const path: string); overload;
    procedure ChangeTab(i: Integer);
    procedure DeleteTab(i: Integer);
    procedure ShowMessages(const aFile: string);
    procedure ShowInterpreter(const aFile: string);
    procedure OutputTo(i: Integer; Lines: TStrings);
    procedure OutputToTerminal(s: string);
    procedure OutputLineTo(i: Integer; const s: string);
    function myIsVisible: Boolean;
    procedure ChangeHideShow;
    function GetFont: TFont;
    procedure SetFont(aFont: TFont);
    procedure SetFontSize(Delta: integer);
    procedure InitAndShow;
    procedure MyDock;
    procedure Undock;
    procedure SaveWindow;
    procedure ShowWatchedExpressions;
    function AddInteractive(UMLForm: TForm; const path: string): TInteractive;
    function GetCurrentInteractive: TInteractiveEdit;
    function GetCurrentStringGrid: TStringGrid;
    procedure DelInteractive(const path: string);
    procedure Run(const Classpath, Programm, Callparameter: string);
    procedure RenameInteractive(const FromPath, ToPath: string);
    procedure SetMinHeight(min: integer);
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure CopyToClipboard;
    procedure Undo;
    procedure Redo;
    procedure SystemOutPrintln;
    procedure Execute(const s: string);
    function NeedsSemicolon(const s: string): boolean;
    function getCompileError(const Path: string): string;
    function InteractiveEditActive: boolean;
    procedure StatusMessage(const s: string; Status: integer = 0);
    procedure ClearStack;
    procedure ChangeStyle;
    procedure DeleteDebuggingTreeViews;
    procedure DPIChanged;
    property ActiveSubTool: integer read fActiveSubTool write setActiveSubTool;
  end;

var
  FMessages: TFMessages = nil;

implementation

uses Math, SysUtils, Dialogs, Clipbrd, Types, Themes, System.IOUtils,
  JvGnugettext, UStringRessources, UJava, UEditorForm, UJavaCommands,
  UGrepResults, UUtils, UConfiguration, UWatches, UBaseForm,
  UUMLForm, URtfdDiagram, UDebugger;

{$R *.DFM}

constructor TInteractive.create(U: TForm; IE: TInteractiveEdit; SG: TStringGrid; E: TInteractiveExecuter; C: TComJava1);
begin
  UMLForm:= U;
  InteractiveEditor:= IE;
  SGVariables:= SG;
  Executer:= E;
  ComJava:= C;
end;

{-- TInteractiveEdit ----------------------------------------------------------}

constructor TInteractiveEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Parent:= FMessages.PInteractiveMiddle;
  PopupMenu:= FMessages.PMMessages;
  OnMouseDown:=  FMessages.LBInteractiveMouseDown;
  OnStatusChange:=  EditorStatusChange;
  OnEnter:=  Enter;
  AddKeyUpHandler(aKeyUpHandler);
  Lines.Add('');
  Gutter.Visible:= false;
  //Font.Assign(FMessages.MInterpreter.Font);  ToDO
  var O:= Options; Exclude(O, eoScrollPastEol); Options:= O;
  Align:= alClient;
  RightEdge:= 0;
  Tag:= 8;
  if FConfiguration.NoSyntaxHighlighting
    then Highlighter:= nil
    else Highlighter:= FConfiguration.GetHighlighter('.java');
end;

procedure TInteractiveEdit.SystemOutPrintln;
  var x: Integer;
begin
  if SelText = ''
    then x:= CaretX
    else x:= BlockBegin.Char;
  SelText:= 'System.out.println();';
  CaretX:= x + 19;
  EnsureCursorPosVisible;
end;

procedure TInteractiveEdit.UpdateState;
begin
  with FJava do begin
    SetEnabledMI(MICut, SelAvail);
    SetEnabledMI(MICopy, SelAvail);
    SetEnabledMI(MICopyNormal, SelAvail);
    SetEnabledMI(MIPaste, CanPaste);
    SetEnabledMI(MIUndo, CanUndo);
    SetEnabledMI(MIRedo, CanRedo);
    SetEnabledTB(TBUndo, CanUndo);
    SetEnabledTB(TBRedo, CanRedo);
  end;
end;

procedure TInteractiveEdit.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scModified in Changes then  // SynEditTypes
    FMessages.SetModifiedUMLForm(FMessages.TabControlMessages.TabIndex);
  UpdateState;
end;

procedure TInteractiveEdit.Enter(Sender: TObject);
begin
  SetEnabledMI(FJava.MISystemOutPrintln, Enabled);
  FJava.UpdateMenuItems(nil);
  FJava.scpSetEditor(Self);
end;

var sKeyUp: string;

procedure TInteractiveEdit.aKeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Return then begin
    var i:= FMessages.TabControlMessages.TabIndex - 5;
    if i >= 0 then begin
      if sKeyUp <> ''
        then sKeyUp:= sKeyUp + #13#10;
      sKeyUp:= sKeyUp + Lines[CaretY-2];
      if Shift = [] then begin
        var aExecuter:= TInteractiveExecuter(FMessages.InteractiveExecuters.Items[i]);
        aExecuter.execute(sKeyUp);
        sKeyUp:= '';
        EnsureCursorPosVisible;
        if (i = FMessages.TabControlMessages.TabIndex - 5) and canFocus then
          setFocus;
      end;
      Key:= 0;
    end;
  end else if (Key = VK_Down) and (CaretY >= Lines.Count) then
    Lines.Add('')
  else if (Key = Ord('U')) and (ssCtrl in Shift) then
    SystemOutPrintln;
end;

{--- TFMessages ---------------------------------------------------------------}

procedure TFMessages.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ActiveSubTool:= -1;
  Undocking:= false;
  InteractiveEditors  := TStringList.Create;
  InteractiveUMLForms := TStringList.Create;
  InteractiveVariables:= TStringList.Create;
  InteractiveExecuters:= TObjectList.Create;
  InteractiveComJavas := TObjectList.Create;
  for var i:= 1 to 3 do begin
    Expanded[i]:= TStringList.Create;
    Expanded[i].Duplicates:= dupIgnore;
  end;
  ChangeStyle;
  ShowWatchedExpressions;
  DumpActive:= false;
  toJavaConsole:= '';
  ActiveInteractive:= nil;
  TBStep.Hint:= FJava.MIStep.Caption;
  TBNext.Hint:= FJava.MINext.Caption;
  TBStepUp.Hint:= FJava.MIStepUp.Caption;
  TBRunToCursor.Hint:= FJava.MIRunToCursor.Caption;
  TBShowExecutionPoint.Hint:= FJava.MIShowExecutionPoint.Caption;
  TBExpression.Hint:= FJava.MIExpression.Caption;
  TBWatches.Hint:= FJava.MIWatches.Caption;
end;

procedure TFMessages.FormDestroy(Sender: TObject);
  var i: integer; aObject: TObject;
begin
  if TVSearch.Items.Count > 0 then
    myGrepResults.DeleteSearchResults;
  FreeAndNil(myGrepResults);
  for i:= 0 to InteractiveEditors.Count - 1 do begin
    aObject:= InteractiveEditors.Objects[i];
    FreeAndNil(aObject);
  end;
  FreeAndNil(InteractiveEditors);
  FreeAndNil(InteractiveUMLForms);
  for i:= 0 to InteractiveVariables.Count - 1 do begin
    aObject:= InteractiveVariables.Objects[i];
    FreeAndNil(aObject);
  end;
  FreeAndNil(InteractiveVariables);
  FreeAndNil(InteractiveExecuters);
  FreeAndNil(InteractiveComJavas);
  for i:= 1 to 3 do
    FreeAndNil(Expanded[i]);
  inherited;
end;

procedure TFMessages.FormShow(Sender: TObject);
begin
  FJava.ActiveTool:= 7;
  if canFocus then
    setFocus;
end;

procedure TFMessages.SetStatusBarAndTabs;
begin
  StatusBar.Font.Size:= FConfiguration.Fontsize;
  StatusBar.Canvas.Font.Size:= FConfiguration.Fontsize;
  var h:= StatusBar.Canvas.TextHeight('Ag') + 4;
  StatusBar.Height:= h;
  PAttribute.Height:= h;
  PLocalVariables.Height:= h;
  PWatches.Height:= h;
  PStack.Height:= h;
  TabControlMessages.Font.Size:= FConfiguration.Fontsize;
  TabControlMessages.Canvas.Font.Size:= FConfiguration.Fontsize;
  TabControlMessages.Height:= h;
  TabControlMessages.TabHeight:= h;
end;

procedure TFMessages.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  aAction:= caHide;
  FJava.ShowDockPanel(FJava.BottomDockPanel, False, nil);
  FJava.MIMessages.Checked:= false;
  FJava.ActiveTool:= -1;
end;

procedure TFMessages.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool:= 7;
  FJava.UpdateMenuItems(Self);
end;

procedure TFMessages.InitAndShow;
  var myVisible, Docked: boolean;
      L, T, W, H: integer;
begin
  LockFormUpdate(Self);
  myVisible:= FConfiguration.ReadBoolU('Messages', 'Visible', true);
  Docked:= FConfiguration.ReadBoolU('Messages', 'Docked', true);
  W:= PPIScale(FConfiguration.ReadIntegerU('Messages', 'Width', 200));
  if W < 50 then W:= 50;
  if W > Screen.Width - 50 then W:= Screen.Width - 50;
  H:= PPIScale(FConfiguration.ReadIntegerU('Messages', 'Height', 200));
  if H < 50 then H:= 50;
  if H > Screen.Height - 50 then H:= Screen.Height - 50;
  L:= PPIScale(FConfiguration.ReadIntegerU('Messages', 'Left', 100));
  if L + W < 50 then L:= 50 - W;
  if Screen.Width - L < 50 then L:= Screen.Width - 50;
  T:= PPIScale(FConfiguration.ReadIntegerU('Messages', 'Top', 500));
  if T + W < 50 then T:= 50 - H;
  if Screen.Height - T < 50 then T:= Screen.Height - 50;
  aPosition:= Rect(L, T, W, H);
  SetBounds(L, T, W, H);
  if Docked then
    MyDock;
  ShowIt;
  if not myVisible then
    HideIt;

  var aFont:= TFont.Create;
  aFont.Name:= FConfiguration.ReadStringU('Messages', 'FontName', 'Consolas');
  aFont.Size:= PPIScale(FConfiguration.ReadIntegerU('Messages', 'FontSize', 10));
  SetFont(aFont);
  FreeAndNil(aFont);
  SetStatusBarAndTabs;
  initialized:= false;
  InteractiveWidth1:= PPIScale(FConfiguration.ReadIntegerU('Messages', 'InteractiveWidth1', 300));
  if InteractiveWidth1 > W - 200 then
    InteractiveWidth1:= W - 200;
  if InteractiveWidth1 < 100 then
    InteractiveWidth1:= 100;
  InteractiveWidth2:= PPIScale(FConfiguration.ReadIntegerU('Messages', 'InteractiveWidth2', 200));
  if Interactivewidth2 > W - 200 then
    InteractiveWidth2:= W - 200;
  if InteractiveWidth2 < 100 then
    InteractiveWidth2:= 100;

  PInteractiveLeft.Width:= InteractiveWidth1;
  PInteractiveRight.Width:= InteractiveWidth2;
  FConfiguration.RemoveShortcutsFrom(PMMessages);
  UnLockFormUpdate(Self);
end;

procedure TFMessages.SaveWindow;
begin
  // Don't use with, otherwise PPIUnScale is calculated for FConfiguration!
  FConfiguration.WriteBoolU('Messages', 'Visible', myIsVisible);
  FConfiguration.WriteBoolU('Messages', 'Docked', not FMessages.Floating);
  FConfiguration.WriteStringU('Messages', 'FontName', MInterpreter.Font.Name);
  FConfiguration.WriteIntegerU('Messages', 'FontSize', PPIUnScale(MInterpreter.Font.Size));
  if SplitterInteractiveLeft.Visible then begin
    FConfiguration.WriteIntegerU('Messages', 'InteractiveWidth1', PPIUnScale(PInteractiveLeft.Width));
    FConfiguration.WriteIntegerU('Messages', 'InteractiveWidth2', PPIUnScale(PInteractiveRight.Width));
  end else begin
    FConfiguration.WriteIntegerU('Messages', 'InteractiveWidth1', PPIUnScale(InteractiveWidth1));
    FConfiguration.WriteIntegerU('Messages', 'InteractiveWidth2', PPIUnScale(InteractiveWidth2));
  end;
  FConfiguration.WriteIntegerU('Messages', 'TabIndex', TabControlMessages.TabIndex);
  if FMessages.Floating then begin
    FConfiguration.WriteIntegerU('Messages', 'Left', PPIUnScale(Self.Left));
    FConfiguration.WriteIntegerU('Messages', 'Top',  PPIUnScale(Self.Top));
    FConfiguration.WriteIntegerU('Messages', 'Width', PPIUnScale(Self.Width));
    FConfiguration.WriteIntegerU('Messages', 'Height', PPIUnScale(Self.Height));
  end else begin
    FConfiguration.WriteIntegerU('Messages', 'Left', PPIUnScale(Self.aPosition.Left));
    FConfiguration.WriteIntegerU('Messages', 'Top',  PPIUnScale(Self.aPosition.Top));
    FConfiguration.WriteIntegerU('Messages', 'Width', PPIUnScale(Self.aPosition.Right));
    FConfiguration.WriteIntegerU('Messages', 'Height', PPIUnScale(Self.aPosition.Bottom));
    FConfiguration.WriteIntegerU('Messages', 'Splitter', PPIUnScale(FJava.BottomDockPanel.Height));
  end;
end;

procedure TFMessages.TabControlChange(NewTab: integer);
  var i: integer; IE: TInteractiveEdit;
begin
  if not initialized then begin
    i:= ClientWidth - 3*4; // Splitter
    i:= i div 4;
    PDebuggerLeft.Width:= i;
    PDebuggerCenterLeft.Width:= i;
    PDebuggerCenterRight.Width:= i;
    initialized:= true;
  end;
  HideInteractiveSplitter;
  PInterpreter.Visible:= false;
  LBCompiler.Visible:= false;
  PDebugger.Visible:= false;
  TVSearch.Visible:= false;
  LBMessages.Visible:= false;
  case NewTab of
    0: begin PInterpreter.Visible:= true; if MInterpreter.canFocus then MInterpreter.SetFocus; end;
    1: begin LBCompiler.Visible:= true; if LBCompiler.canFocus then LBCompiler.SetFocus; end;
    2: begin PDebugger.Visible:= true; if TVAttributes.canFocus then TVAttributes.SetFocus end;
    3: begin TVSearch.Visible:= true; if TVSearch.canFocus then TVSearch.SetFocus end;
    4: begin LBMessages.Visible:= true; if LBMessages.canFocus then LBMessages.SetFocus; end;
  else begin
    PInterpreter.Visible:= true;
    for i:= 0 to InteractiveEditors.Count - 1 do begin
      TInteractiveEdit(InteractiveEditors.Objects[i]).Visible:= false;
      TStringGrid(InteractiveVariables.Objects[i]).Visible:= false;
    end;
    IE:= GetInteractive(NewTab);
    if assigned(IE) then begin
      ShowInteractiveSplitter;
      ActiveInteractive:= IE;
      IE.Visible:= true;
      if IE.CanFocus then // responsible for inactive window exception
        IE.SetFocus;  //this gave an unexpected exception
      IE.UpdateState;
      TStringGrid(InteractiveVariables.Objects[NewTab-5]).Show;
      AdjustVariablesWidths(NewTab);
      TComJava1(InteractiveComJavas.Items[NewTab-5]).setActiveComJava(TComJava1(InteractiveComJavas.Items[NewTab-5]));
      IE.Enter(Self);
     end;
    end;
  end;
  TabControlMessages.TabIndex:= NewTab;
  FJava.UpdateMenuItems(nil);
end;

procedure TFMessages.TabControlMessagesChange(Sender: TObject);
begin
  // works only in undocked state
  TabControlChange(TabControlMessages.TabIndex);
end;

procedure TFMessages.TBDeleteClick(Sender: TObject);
begin
  var IE:= GetInteractive(TabControlMessages.TabIndex);
  if IE.SelAvail
    then IE.ClearSelection
    else IE.Clear;
end;

procedure TFMessages.ShowTab(i: Integer);
begin
  if not Visible then
    ShowIt;
  if TabControlMessages.TabIndex <> i then begin
    ChangeTab(i);
    Update;
  end;
end;

procedure TFMessages.ShowTab(const path: string);
begin
  var i:= 0;
  while i < InteractiveEditors.Count do begin
    if path = InteractiveEditors.Strings[i] then begin
      ChangeTab(i+5);
      if ExtractFileExt(path) = '.uml' then
        ShowInteractiveSplitter;
      break;
    end;
    inc(i);
  end;
  Update;
end;

procedure TFMessages.ChangeTab(i: Integer);
begin
  if myIsVisible and (TabControlMessages.TabIndex <> i) then begin
    TabControlMessages.TabIndex:= i;
    TabControlMessagesChange(Self);
    AdjustVariablesWidths(i);
  end;
end;

procedure TFMessages.DeleteTab(i: Integer);
begin
  case i of
    0: MInterpreter.Text:= '';
    1: LBCompiler.Clear;
    2: ;
    3: ;
    4: LBMessages.Clear;
  else begin
    GetInteractive(i).Clear;
    SetModifiedUMLForm(i);
    end;
  end;
end;

procedure TFMessages.ShowMessagesOrInterpreter(const aFile: string; OutputNr: integer);
begin
  var Messages:= TStringlist.Create;
  try
    Messages.LoadfromFile(aFile);
    OutputTo(OutputNr, Messages);
  except
    on e: exception do
      ErrorMsg(e.Message);
  end;
  ChangeTab(OutputNr);
  FreeAndNil(Messages);
end;

procedure TFMessages.ShowMessages(const aFile: string);
begin
  ShowMessagesOrInterpreter(aFile, K_Messages);
end;

procedure TFMessages.ShowInterpreter(const aFile: string);
begin
  ShowMessagesOrInterpreter(aFile, K_Interpreter);
end;

procedure TFMessages.OutputTo(i: integer; Lines: TStrings);
begin
  case i of
    0: MInterpreter.Lines.AddStrings(Lines);
    1: LBCompiler.Items.AddStrings(Lines);
    2: ;
    3: ;
    4: LBMessages.Items.AddStrings(Lines);
  else begin
    GetInteractive(i).Lines.Add(Lines.Text);
    SetModifiedUMLForm(i);
    end;
  end;
  ScrollEnd(i);
end;

procedure TFMessages.OutputToTerminal(s: string);
begin
  if endsWith(s, CrLf) then
    delete(s, length(s)-1, 2);
  MInterpreter.Lines.Add(s);
  ScrollEnd(0);
end;

procedure TFMessages.OutputLineTo(i: Integer; const s: string);
begin
  case i of
    0: MInterpreter.Lines.Add(s);
    1: LBCompiler.Items.Add(s);
    2: ;
    3: ;
    4: LBMessages.Items.Add(s);
  else begin
    GetInteractive(i).Lines.Add(s);
    SetModifiedUMLForm(i);
    end;
  end;
  ScrollEnd(i);
end;

function TFMessages.GetFileWithPath(LB: TListBox; var i: integer): string;
  var s, pathname, filename, path: string; j, p: integer;
begin
  Result:= '';
  while i >= 0 do begin
    s:= LB.Items[i];
    p:= Pos('.java:', s);
    if p > 0 then begin
      delete(s, p + 5, length(s));
      if copy(s, 1, 2) = '.\' then  // .\filename.java
        delete(s, 1, 2);
      if TPath.DriveExists(s) or IsUNC(s) then begin
        Result:= s;
        exit;
      end;
      for j:= 0 to FJava.TDIEditFormCount - 1 do begin
        pathname:= FJava.TDIEditFormGet(j).Pathname;
        filename:= ExtractFilename(pathname);
        if (filename = s) and FileExists(pathname) then begin
          Result:= Pathname;
          exit;
        end;
        path:= ExtractFilePath(pathname);
        if FileExists(path + s) then begin
          Result:= path + s;
          exit;
        end;
      end;
    end;
    dec(i);
  end;
end;

procedure TFMessages.LBCompilerDblClick(Sender: TObject);
  var i, d1, d2, Line, Column : Integer;
      s, s1, s2, FileWithPath: string;
begin
  // activate double-clicked errorline in editor-window
  i:= LBCompiler.ItemIndex;
  d2:= i;
  FileWithPath:= GetFileWithPath(LBCompiler, d2);
  repeat
    s1:= LBCompiler.Items[i];
    d1:= Pos('.java:', s1);   // VisibleStack.java:12:
    if d1 > 0 then begin
      s2:= copy(s1, d1 + 6, 255);
      d2:= Pos(':', s2);                  // compile error
      if d2 > 0 then begin                // errorline found
        LBCompiler.ItemIndex:= i;
        if not TryStrToInt(Copy(s2, 1, d2-1), Line) then
          continue;
        delete(s2, 1, d2);
        d2:= Pos(':', s2);
        if not TryStrToInt(Copy(s2, 1, d2-1), Column) then
          Column:= 1;
        // position cursor in editor
        if Copy(Trim(s1), 1, 2) = '.\' then // relativ path
          FileWithPath:= ExtractFilePath(FileWithPath) + ExtractFileName(Copy(s1, 1, d1+4));
        FJava.ChangeWindowWithPositioning(ToWindows(FileWithPath), Column, Line, true);
        break;
      end;
    end;
    i:= i - 1;
  until i = -1;
  if i = -1 then begin
    i:= LBCompiler.ItemIndex;
    s:= LBCompiler.Items[i];
    if (Pos('-deprecation', s) > 0) and FileExists(FileWithPath) then begin
      FConfiguration.JavaCompilerParameter:= '-deprecation ' + FConfiguration.JavaCompilerParameter;
      myJavaCommands.Compile(FileWithPath, '');
      delete(FConfiguration.JavaCompilerParameter, 1, 13);
    end else
    if (Pos('-Xlint:unchecked ', s) > 0) and FileExists(FileWithPath) then begin
      FConfiguration.JavaCompilerParameter:= '-Xlint:unchecked ' + FConfiguration.JavaCompilerParameter;
      myJavaCommands.Compile(FileWithPath, '');
      delete(FConfiguration.JavaCompilerParameter, 1, 17);
    end;
  end;
end;

procedure TFMessages.MInterpreterDblClick(Sender: TObject);
  var i, p, d0, d1, d2, Line: Integer;
      s, s2, FileWithPath: string;
begin
  // activate double-clicked errorline in editor-window
  i:= MInterpreter.CaretPos.y + 1;
  p:= 0;
  while (i > 0) and (p = 0) do begin
    dec(i);
    s:= trim(MInterpreter.Lines[i]);
    p:= Pos('Exception', s);  // Exception at
  end;

  if p > 0 then begin
    s:= MInterpreter.Lines[i+1];
    d0:= Pos('(', s);
    d1:= Pos('.java:', s);   // VisibleStack.java:12:
    if d1 > 0 then begin
      s2:= copy(s, d1 + 6, 255);
      d2:= Pos(')', s2);                  // compile error
      if d2 > 0 then begin                // errorline found
        MInterpreter.CaretPos:= Point(1, i+1);
        if not TryStrToInt(Copy(s2, 1, d2-1), Line) then exit;
        FileWithPath:=  FJava.GetPathnameForClass(copy(s, d0+1, d1-d0+4));
        if FileWithPath <> '' then
          FJava.ChangeWindowWithPositioning(FileWithPath, -1, Line, true);
      end;
    end;
  end;
end;

procedure TFMessages.MIGotoErrorClick(Sender: TObject);
begin
  case TabControlMessages.TabIndex of
    0: begin
         MInterpreter.CaretPos:= Point(1, MInterpreter.Lines.Count);
         MInterpreterDblClick(Self);
       end;
    1: begin
         var i:= 0;
         while (i < LBCompiler.Items.Count) and (Pos(': error: ', LBCompiler.Items[i]) = 0) do
           inc(i);
         if i < LBCompiler.Items.Count then begin
           LBCompiler.ItemIndex:= i;
           LBCompilerDblClick(self);
         end;
       end;
  end;
end;

procedure TFMessages.LBMessagesDblClick(Sender: TObject);
  var i, d1, d2, Line, Column: Integer;
      s, FileWithPath: string;
begin
  // Activate the double-clicked error line in the editor window
  i:= LBMessages.ItemIndex;
  d1:= 0;
  FileWithPath:= GetFileWithPath(LBMessages, d1);
  s:= LBMessages.Items[i];
  d1:= Pos('.java:', s);   // VisibleStack.java:12:
  if d1 > 0 then begin
    s:= copy(s, d1 + 6, 255);
    d2:= Pos(':', s);      // Style check failed
    if d2 > 0 then begin   // error line found
      LBMessages.ItemIndex:= i;
      if TryStrToInt(Copy(s, 1, d2-1), Line) then begin
        s:= Copy(s, d2+1, 255);
        d2:= Pos(':', s);
        if d2 = 0
          then Column:= -1
          else if not TryStrToInt(Copy(s, 1, d2-1), Column) then exit;
        FJava.ChangeWindowWithPositioning(ToWindows(FileWithPath), Column, Line, true);
      end;
    end;
  end;
end;

procedure TFMessages.ScrollEnd(i: Integer);
  var IE: TInteractiveEdit;

  procedure ScrollEndMemo(M: TMemo);
  begin
    M.Perform(EM_LineScroll, 0 , M.Lines.Count-1);
    M.Update;
    M.SelStart:= Length(M.Text)-2;
  end;

begin
  case i of
    0: ScrollEndMemo(MInterpreter);
    1: ScrollEndListBox(LBCompiler);
    2: ;
    3: ;
    4: ScrollEndListBox(LBMessages);
  else begin
    IE:= GetInteractive(i);
    IE.TopLine:= IE.Lines.Count - 2;
   end;
  end;
end;

procedure TFMessages.SetFont(aFont: TFont);
begin
  MInterpreter.Font.Assign(aFont);
  LBCompiler.Font.Assign(aFont);
  TVAttributes.Font.Assign(aFont);
  TVLocalVariables.Font.Assign(aFont);
  TVWatchedExpressions.Font.Assign(aFont);
  LBStack.Font.Assign(aFont);
  TVSearch.Font.Assign(aFont);
  LBMessages.Font.Assign(aFont);
  for var i:= 0 to InteractiveEditors.Count - 1 do begin
    GetInteractive(i+5).Font.Assign(aFont);
    TStringGrid(InteractiveVariables.Objects[i]).Font.Assign(aFont);
    TStringGrid(InteractiveVariables.Objects[i]).DefaultRowHeight:= Round(aFont.Size*1.8);
  end;
end;

function TFMessages.GetFont: TFont;
begin
  Result:= MInterpreter.Font;
end;

procedure TFMessages.SetFontSize(Delta: integer);
begin
  var Size:= MInterpreter.Font.Size + Delta;
  if Size < 6 then Size:= 6;
  MInterpreter.Font.Size:= Size;
  LBCompiler.Font.Size:= Size;
  TVAttributes.Font.Size:= Size;
  TVLocalVariables.Font.Size:= Size;
  TVWatchedExpressions.Font.Size:= Size;
  LBStack.Font.Size:= Size;
  TVSearch.Font.Size:= Size;
  LBMessages.Font.Size:= Size;
  StatusBar.Font.Size:= Size;
  for var i:= 0 to InteractiveEditors.Count - 1 do begin
    GetInteractive(i+5).Font.Size:= Size;
    SetModifiedUMLForm(i+5);
    TStringGrid(InteractiveVariables.Objects[i]).Font.Size:= Size;
    TStringGrid(InteractiveVariables.Objects[i]).DefaultRowHeight:= Round(Size*1.8);
  end;
  Show;
end;

function TFMessages.myIsVisible: Boolean;
begin
  if Floating
    then Result:= FMessages.Visible
    else Result:= FMessages.Visible and (FJava.BottomDockPanel.Height > 1);
end;

procedure TFMessages.ShowIt;
begin
  if not myIsVisible then
    if Floating
      then Show
      else FJava.ShowDockPanel(FJava.BottomDockPanel, true, FMessages);
end;

procedure TFMessages.HideIt;
begin
  if myIsVisible then
    if Floating
      then Hide
      else FJava.ShowDockPanel(FJava.BottomDockPanel, false, nil);
end;

procedure TFMessages.ChangeHideShow;
begin
  if myIsVisible
    then HideIt
    else ShowIt;
end;

procedure TFMessages.MyDock;
begin
  if Floating then
    ManualDock(FJava.BottomDockPanel, nil, alTop);
  TabControlMessages.Top:= ClientHeight;
end;

procedure TFMessages.Undock;
begin
  if not Floating then begin
    var Pos:= aPosition;
    Pos.Right:= aPosition.Left + aPosition.Right;
    Pos.Bottom:= aPosition.Top + aPosition.Bottom;
    ManualFloat(Pos);
  end;
end;

procedure TFMessages.MIDockClick(Sender: TObject);
begin
  if Floating
    then MyDock
end;

procedure TFMessages.MIUndockClick(Sender: TObject);
begin
  if not FMessages.Floating
     then FMessages.Undock;
end;

procedure TFMessages.TVWatchedExpressionsKeyUp(Sender: TObject;
                       var Key: Word; Shift: TShiftState);
  var p: integer; s: string; Node: TTreeNode;
begin
 if Key = VK_Insert then
    FJava.MIWatchesClick(Self)
  else if Key = VK_Delete then
    with TVWatchedExpressions do begin
      Node:= Selected;
      if Assigned(Node) and not Assigned(Node.Parent) then begin
        s:= Node.Text;
        p:= Pos(' = ', s);
        if p > 0 then
          delete(s, p, length(s));
        Items.delete(Node);
        FWatches.Delete(s);
    end;
  end;
end;

procedure TFMessages.LBStackDblClick(Sender: TObject);
  var s, aClass, Path: string;
      Line, p, p1, p2: Integer;
begin
  if (0 <= LBStack.ItemIndex) and (LBStack.ItemIndex < LBStack.Items.Count) then begin
    s:= LBStack.Items[LBStack.ItemIndex];
    p1:= Pos('(', s);
    p2:= Pos(')', s);
    s:= Copy(s, p1 + 1 , p2 - p1 - 1);
    p:= Pos(':', s);
    aClass:= Copy(s, 1, p-1);
    delete(s, 1, p);
    try
      if not TryStrToInt(s, Line) then exit;
      p:= Pos('.', aClass);
      Delete(aClass, p, Length(aClass));
      Path:= FJava.GetPathnameForClass(aClass);
      if Path <> '' then begin
        SearchGoalLine:= Line;
        SearchGoalPath:= Path;
        FJava.ChangeWindowWithPositioning(Path, 1, Line, false);
      end;
    except
    end;
  end;
end;

procedure TFMessages.LBStackKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Delete then
    LBCompiler.DeleteSelected
end;

procedure TFMessages.TVWatchedExpressionsDblClick(Sender: TObject);
begin
  FJava.MIWatchesClick(Self);
end;

procedure TFMessages.MIDeleteClick(Sender: TObject);
begin
  case ActiveSubTool of
    0: if MInterpreter.SelLength > 0
         then MInterpreter.ClearSelection
         else MInterpreter.Clear;
    1: LBCompiler.DeleteSelected;
    2: TreeViewDeleteSelected(TVAttributes);
    3: TreeViewDeleteSelected(TVLocalVariables);
    4: TreeViewDeleteSelected(TVWatchedExpressions);
    5: LBStack.DeleteSelected;
    6: TreeViewDeleteSelected(TVSearch);
    7: LBMessages.DeleteSelected;
    8: begin
         var IE:= GetInteractive(TabControlMessages.TabIndex);
         IE.ClearSelection;
         SetModifiedUMLForm(TabControlMessages.TabIndex);
       end;
  end;
end;

procedure TFMessages.MIDeleteAllClick(Sender: TObject);
begin
  case ActiveSubTool of
    0: MInterpreter.Clear;
    1: LBCompiler.Clear;
    2: TreeViewDeleteAll(TVAttributes);
    3: TreeViewDeleteAll(TVLocalVariables);
    4: TreeViewDeleteAll(TVWatchedExpressions);
    5: LBStack.Clear;
    6: TreeViewDeleteAll(TVSearch);
    7: LBMessages.Clear;
    8: begin
         var IE:= GetInteractive(TabControlMessages.TabIndex);
         IE.Clear;
         SetModifiedUMLForm(TabControlMessages.TabIndex);
       end;
  end;
end;

procedure TFMessages.TBExecuteClick(Sender: TObject);
  var aForm: TFEditForm; IE: TSynEdit; aExecuter: TInteractiveExecuter;
      i: integer; s: string;
begin
  TBExecute.Enabled:= false;
  Screen.Cursor:= crHourglass;
  try
    i:= TabControlMessages.TabIndex - 5;
    if i >= 0 then begin
      if assigned(InteractiveUMLForms.Objects[i]) then begin
        aForm:= FJava.getActiveEditor;
        if TSynEdit(InteractiveEditors.Objects[i]).SelAvail then
          IE:= TSynEdit(InteractiveEditors.Objects[i])
        else if assigned(aForm) and aForm.Editor.SelAvail then
          IE:= aForm.Editor
        else
          IE:=  TSynEdit(InteractiveEditors.Objects[i]);
        if IE.SelAvail
          then s:= IE.GetLinesWithSelection
          else s:= IE.LineText;
        IE.SelStart:= IE.SelEnd;
        aExecuter:= TInteractiveExecuter(InteractiveExecuters.Items[i]);
        aExecuter.execute(s);
      end;
    end;
  finally
    Screen.Cursor:= crDefault;
    TBExecute.Enabled:= true;
  end;
end;

procedure TFMessages.MIExpandClick(Sender: TObject);
begin
  case ActiveSubTool of
    2: TVAttributes.FullExpand;
    3: TVLocalVariables.FullExpand;
    4: TVWatchedExpressions.FullExpand;
    6: TVSearch.FullExpand;
  end;
end;

procedure TFMessages.MICollapseClick(Sender: TObject);
begin
  case ActiveSubTool of
    2: TVAttributes.FullCollapse;
    3: TVLocalVariables.FullCollapse;
    4: TVWatchedExpressions.FullCollapse;
    6: TVSearch.FullCollapse;
  end;
end;

function TFMessages.GetSelectedLines(ListBox: TListBox): string;
  var i: Integer; var s: string;
begin
  s:= '';
  for i:= 0 to ListBox.Count - 1 do
    if ListBox.Selected[i] then s:= s + ListBox.Items[i] + #13#10;
  Result:= s;
end;

function TFMessages.CopyTreeView(TreeView: TTreeView; all: boolean): string;
begin
  var s:= '';
  for var i:= 0 to TreeView.Items.Count - 1 do
    if all or TreeView.Items[i].Selected then
      if assigned(TreeView.Items[i].Parent) then
        if TreeView.Items[i].Text <> 'dummy'
          then s:= s + '  ' + TreeView.Items[i].Text + #13#10
          else
        else s:= s + TreeView.Items[i].Text + #13#10;
  Result:= s;
end;

function TFMessages.CopyTreeViewAll(TreeView: TTreeView): string;
begin
  Result:= CopyTreeView(TReeView, true);
end;

function TFMessages.CopyTreeViewSelected(TreeView: TTreeView): string;
begin
  Result:= CopyTreeView(TReeView, false);
end;

procedure TFMessages.MICopyClick(Sender: TObject);
begin
  case ActiveSubTool of
    0: MInterpreter.CopyToClipboard;
    1: Clipboard.AsText:= GetSelectedLines(LBCompiler);
    2: Clipboard.AsText:= CopyTreeViewSelected(TVAttributes);
    3: Clipboard.AsText:= CopyTreeViewSelected(TVLocalVariables);
    4: Clipboard.AsText:= CopyTreeViewSelected(TVWatchedExpressions);
    5: Clipboard.AsText:= GetSelectedLines(LBStack);
    6: Clipboard.AsText:= CopyTreeViewSelected(TVSearch);
    7: Clipboard.AsText:= GetSelectedLines(LBMessages);
    8: Clipboard.AsText:= GetInteractive(TabControlMessages.TabIndex).SelText;
  end;
end;

procedure TFMessages.MICopyAllClick(Sender: TObject);
begin
  case ActiveSubTool of
    0: Clipboard.AsText:= MInterpreter.Lines.Text;
    1: Clipboard.AsText:= LBCompiler.Items.Text;
    2: Clipboard.AsText:= CopyTreeViewAll(TVAttributes);
    3: Clipboard.AsText:= CopyTreeViewAll(TVLocalVariables);
    4: Clipboard.AsText:= CopyTreeViewAll(TVWatchedExpressions);
    5: Clipboard.AsText:= LBStack.Items.Text;
    6: Clipboard.AsText:= CopyTreeViewAll(TVSearch);
    7: Clipboard.AsText:= LBMessages.Items.Text;
    8: Clipboard.AsText:= GetInteractive(TabControlMessages.TabIndex).Text;
  end;
end;

procedure TFMessages.MICloseClick(Sender: TObject);
begin
  Close;
end;

  function ExtractVariable(const s: string): string;
  begin
    var p2:= Pos(' = instance of', s);
    if p2 = 0 then p2:= Pos(': instance of', s);
    if p2 = 0 then p2:= Pos(' = {', s);
    if p2 = 0
      then Result:= s
      else Result:= Copy(s, 1, p2-1);
  end;

  function ExtractArraySize(const s: string): integer;
  begin
    // instance of java.lang.Object[10]
    Result:= -1;
    var p1:= Pos('[', s);
    var p2:= Pos(']', s);
    if p1 > 0 then
      TryStrToInt(copy(s, p1 + 1, p2 - p1 - 1), Result);
  end;

  function getFullName(Node: TTreeNode): string;
    var AktNode: TTreeNode;
        s, variable: string;
  begin
    AktNode:= Node;
    s:= ExtractVariable(Node.Text);
    while assigned(AktNode.Parent) do begin
      AktNode:= AktNode.Parent;
      variable:= ExtractVariable(AktNode.Text);
      if Pos(variable, s) = 1
        then s:= s
        else s:= variable + '.' + s;
    end;
    Result:= s;
  end;

procedure TFMessages.Expand(Node: TTreeNode; chapter: integer);
begin
  var TotalVariable:= getFullName(Node);
  Expanded[chapter].Add(TotalVariable);
  myDebugger.NewCommand(chapter + 6, 'dump ' + TotalVariable);
  Node.DeleteChildren;
end;

procedure TFMessages.Collapse(Node: TTreeNode; chapter: integer);
begin
  var TotalVariable:= getFullname(Node);
  for var i:= Expanded[chapter].Count - 1 downto 0 do
    if Pos(TotalVariable, Expanded[chapter].Strings[i]) = 1
      then Expanded[chapter].Delete(i);
end;

procedure TFMessages.TVAttributesExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if DumpActive then exit;
  AllowExpansion:= false;
  Expand(Node, 1);
end;

procedure TFMessages.TVAttributesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Delete then
    TreeViewDelete(TVAttributes, false);
end;

procedure TFMessages.TVLocalVariablesExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if DumpActive then exit;
  AllowExpansion:= false;
  Expand(Node, 2);
end;

procedure TFMessages.TVLocalVariablesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Delete then
    TreeViewDelete(TVLocalVariables, false);
end;

procedure TFMessages.TVWatchedExpressionsExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if DumpActive then exit;
  AllowExpansion:= false;
  Expand(Node, 3);
end;

procedure TFMessages.TVAttributesCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Collapse(Node, 1);
end;

procedure TFMessages.TVLocalVariablesCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Collapse(Node, 2);
end;

procedure TFMessages.TVWatchedExpressionsCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Collapse(Node, 3);
end;

procedure TFMessages.ShowWatchedExpressions;
  var i: integer; s: string; Node: TTreeNode;
begin
  TVWatchedExpressions.Items.Clear;
  for i:= 0 to FWatches.LBWatches.Items.Count - 1 do begin
    s:= FWatches.LBWatches.Items[i];
    Node:= TVWatchedExpressions.Items.Add(nil, s);
    if Pos('instance of', s) > 0 then
      TVWatchedExpressions.Items.AddChild(Node, s);
  end;
end;

procedure TFMessages.SplitterInteractiveRightMoved(Sender: TObject);
begin
  AdjustVariablesWidths(TabControlMessages.Tabindex);
end;

procedure TFMessages.SBStepClick(Sender: TObject);
begin
  FJava.MIStepClick(Self);
end;

procedure TFMessages.SBNextClick(Sender: TObject);
begin
  FJava.MINextClick(Self);
end;

procedure TFMessages.SBStepUpClick(Sender: TObject);
begin
  FJava.MIStepUpClick(Self);
end;

procedure TFMessages.SBRunToCursorClick(Sender: TObject);
begin
  FJava.MIRunToCursorClick(Self);
end;

procedure TFMessages.SBShowExecutionPointClick(Sender: TObject);
begin
  FJava.MIShowExecutionPointClick(Self);
end;

procedure TFMessages.SBExpressionClick(Sender: TObject);
begin
  FJava.MIExpressionClick(Self);
end;

procedure TFMessages.SBWatchesClick(Sender: TObject);
begin
  FJava.MIWatchesClick(Self);
end;

procedure TFMessages.SBDetailsClick(Sender: TObject);
begin
  myDebugger.SwitchDetails;
end;

procedure TFMessages.TVSearchDblClick(Sender: TObject);
  var Node: TTreeNode;
begin
  Node:= TVSearch.Selected;
  if Assigned(Node) then
    myGrepResults.OpenSource(Node);
end;

procedure TFMessages.TreeViewDeleteSelected(TreeView: TTreeView);
  var Node: TTreeNode; s: string; p: integer;
begin
  with TreeView do begin
    Node:= Selected;
    if Assigned(Node) and not Assigned(Node.Parent) then begin
      s:= Node.Text;
      p:= Pos(' = ', s);
      if p > 0 then
        delete(s, p, length(s));
      Items.delete(Node);
      FWatches.Delete(s);
    end;
  end;
  TreeViewDelete(TreeView, false);
end;

procedure TFMessages.TreeViewDeleteAll(TreeView: TTreeView);
begin
  TreeViewDelete(TreeView, true);
  FWatches.DeleteAll;
end;

procedure TFMessages.TreeViewDelete(TreeView: TTreeView; all: boolean);
  var Node, LastNode, PrevNode: TTreeNode; Results: TSearchResults;
begin
  LastNode:= nil;
  TreeView.Items.BeginUpdate;
  Node:= TreeView.Items.GetFirstNode;
  while assigned(Node) do begin
    if all or Node.Selected then Node.Text:= 'DELETE';
    if Node.GetNext = nil then LastNode:= Node;
    Node:= Node.GetNext;
  end;
  Node:= LastNode;
  while assigned(Node) do begin
    if Node.Text = 'DELETE' then begin
      if (Node.Parent = nil) and (TreeView = TVSearch) then begin
        Results:= TSearchResults(Node.Data);
        FreeAndNil(Results);
      end;
      PrevNode:= Node.GetPrev;
      Node.Delete;
      Node:= PrevNode;
    end else
      Node:= Node.GetPrev;
  end;
  TreeView.Items.EndUpdate;
end;

procedure TFMessages.TVSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Delete then
    TreeViewDelete(TVSearch, false);
end;

procedure TFMessages.FormPaint(Sender: TObject);
  var Msg: TWMMove;
begin
  if Undocking then begin
    Undocking:= false;
    ShowTab(K_Interpreter);
  end;
  OnMove(Msg);
end;

procedure TFMessages.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(GetFont);
  FJava.FDFont.Options:= [];
  if FJava.FDFont.Execute then
    SetFont(FJava.FDFont.Font);
end;

procedure TFMessages.MIPasteClick(Sender: TObject);
begin
  if TabControlMessages.TabIndex = 2
    then FJava.MIWatchesClick(Self)
    else PasteFromClipboard;
end;

procedure TFMessages.MISameWidthClick(Sender: TObject);
begin
  var w:= PMain.Width div 4;
  PDebuggerLeft.Width:= w;
  PDebuggerCenterLeft.Width:= w;
  PDebuggerCenterRight.Width:= w;
  PDebuggerRight.Width:= W;
  w:= ClientWidth div 3;
  PInteractiveLeft.Width:= w;
  PInteractiveMiddle.Width:= w;
  PInteractiveRight.Width:= w;
  for var i:= 0 to InteractiveVariables.Count - 1 do begin
    TStringGrid(InteractiveVariables.Objects[i]).ColWidths[0]:= w div 3;
    TStringGrid(InteractiveVariables.Objects[i]).ColWidths[1]:= w div 3;
    TStringGrid(InteractiveVariables.Objects[i]).ColWidths[2]:= w div 3;
  end;
end;

procedure TFMessages.TBJavaResetMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft
    then getComJava.JavaReset
    else FJava.Restart;
end;

procedure TFMessages.MInterpreterEnter(Sender: TObject);
begin
  ActiveSubTool:= 0;
end;

procedure TFMessages.MInterpreterKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13: begin
           if myDebugger.Running
             then myDebugger.ToUserProgram(toJavaConsole)
             else getComJava.WriteToJava(toJavaConsole + #13#10);
           if not myDebugger.Running and (toJavaConsole <> '') then
             Key:= #0;
           toJavaConsole:= '';
         end;
    #08: toJavaConsole:= copy(toJavaConsole, 1, Length(toJavaConsole)-1);
    else toJavaConsole:= toJavaConsole + Key;
  end;
end;

procedure TFMessages.TBShowUMLClick(Sender: TObject);
  var i: integer; U: TFUMLForm;
begin
  i:= TabControlMessages.TabIndex;
  if i > 5 then begin
    U:= TFUMLForm(InteractiveUMLForms.Objects[i-5]);
    if (i = 5) and (not U.Visible)
      then U.Show
      else FJava.SwitchToWindow(U);
  end;
end;

procedure TFMessages.PMMessagesPopup(Sender: TObject);
  var Selected, error: boolean; aWinControl: TWinControl;
begin
  if not (Sender is TSpTBXPopupMenu) then
    exit;
  aWinControl:= FindVCLWindow((Sender as TSpTBXPopupMenu).PopupPoint);
  if assigned(aWinControl)
    then ActiveSubTool:= aWinControl.Tag
    else ActiveSubTool:= -1;

  SetVisibleMI(MICollapse, TabControlMessages.TabIndex in [2, 3]);
  SetVisibleMI(MIExpand, TabControlMessages.TabIndex in [2, 3]);
  SetVisibleMI(MIPaste, ActiveSubTool in [0, 8]);

  SetVisibleMI(MIClose, TabControlMessages.TabIndex <> 2);
  SetVisibleMI(MIDeleteAll, true);
  SetVisibleMI(MICopyAll, true);
  error:= false;
  selected:= false;
  case ActiveSubTool of
    0: begin
         Selected:= MInterpreter.SelLength > 0;
         error:= (Pos('Exception', MInterpreter.Lines.Text) > 0);
         setVisibleMI(MIPaste, false);
       end;
    1: begin
         Selected:= LBCompiler.SelCount > 0;
         error:= (Pos(': error: ', LBCompiler.Items.Text) > 0);
       end;
    2: Selected:= true;
    3: Selected:= true;
    4: Selected:= true;
    5: Selected:= true;
    6: Selected:= true;
    7: Selected:= LBMessages.SelCount > 0;
    8: begin
         Selected:= ActiveInteractive.SelAvail;
         setVisibleMI(MIPaste, true);
       end;
    9: begin
         setVisibleMI(MIPaste, false);
         setVisibleMI(MIDeleteAll, false);
         setVisibleMI(MICopyAll, false);
       end;
  end;
  SetVisibleMI(MICopy, Selected);
  SetVisibleMI(MIDelete, Selected);
  SetVisibleMI(MIGotoError, error);
  SetVisibleMI(MIDock, Floating);
  SetVisibleMI(MIUndock, not Floating);
end;

procedure TFMessages.ShowInteractiveSplitter;
begin
  if not SplitterInteractiveLeft.Visible then begin
    PInteractiveLeft.Align:= alLeft;
    PInteractiveLeft.Width:= Math.min(InteractiveWidth1, round(FJava.Width*0.8));
    SplitterInteractiveLeft.Left:= PInteractiveLeft.Width + 1;
    SplitterInteractiveLeft.Visible:= true;

    PInteractiveRight.Align:= alRight;
    PInteractiveRight.Left:= ClientWidth - InteractiveWidth2;
    PInteractiveRight.Visible:= true;
    SplitterInteractiveRight.Left:= PInteractiveRight.Width - 1;
    SplitterInteractiveRight.Visible:= true;

    PInteractiveMiddle.Align:= alClient;
    PInteractiveMiddle.Visible:= true;
  end;
end;

procedure TFMessages.HideInteractiveSplitter;
begin
  if SplitterInteractiveLeft.Visible then begin
    InteractiveWidth1:= PInteractiveLeft.Width;
    InteractiveWidth2:= PInteractiveRight.Width;
    PInteractiveMiddle.Align:= alNone;
    PInteractiveMiddle.Visible:= false;
    PInteractiveRight.Align:= alNone;
    PInteractiveRight.Visible:= false;
    SplitterInteractiveRight.Visible:= false;
    SplitterInteractiveLeft.Visible:= false;
    PInteractiveLeft.Align:= alClient;
  end;
end;

function TFMessages.AddInteractive(UMLForm: TForm; const path: string): TInteractive;
  var IE: TInteractiveEdit; aExecuter: TInteractiveExecuter;
      SGVariables: TStringGrid; GO: TGridOptions; ComJava: TComJava1;
begin
  IE:= TInteractiveEdit.Create(Self);
  IE.ReadOnly:= false;     // <---

  SGVariables:= TStringGrid.Create(Self);
  SGVariables.Parent:= PInteractiveRight;
  SGVariables.Font.Assign(Font);
  SGVariables.Font.Size:= FConfiguration.Fontsize;
  SGVariables.Align:= alClient;
  SGVariables.ColCount:= 3;
  SGVariables.RowCount:= 2;
  SGVariables.FixedCols:= 0;
  SGVariables.FixedRows:= 1;
  SGVariables.Cells[0,0]:= _('Name');
  SGVariables.Cells[1,0]:= _('Type');
  SGVariables.Cells[2,0]:= _(LNGValue);
  GO:= SGVariables.Options;
  Include(GO, goColSizing);
  SGVariables.Options:= GO;
  SGVariables.DefaultRowHeight:= Round(MInterpreter.Font.Size*1.8);
  SGVariables.Selection:= TGridRect(Rect(-1, -1, -1, -1));
  SGVariables.PopupMenu:= PMMessages;
  SGVariables.Tag:= 9;
  SGVariables.OnMouseWheelDown:= StringGrid1MouseWheelDown;
  SGVariables.OnMouseWheelUp:= StringGrid1MouseWheelUp;
  ComJava:= TComJava1.Create(UMLForm, InteractiveEditors.Count);
  aExecuter:= TInteractiveExecuter.create(UMLForm as TFUMLForm, IE, SGVariables, ComJava);

  InteractiveEditors.AddObject(path, IE);
  InteractiveUMLForms.AddObject(path, UMLForm);
  InteractiveVariables.AddObject(path, SGVariables);
  InteractiveExecuters.Add(aExecuter);
  InteractiveComJavas.Add(ComJava);

  TabControlMessages.Tabs.Add(ExtractFilenameEx(path));
  ChangeTab(TabControlMessages.Tabs.Count-1);
  AdjustVariablesWidths(TabControlMessages.Tabs.Count-1);
  Result:= TInteractive.create(UMLForm, IE, SGVariables, aExecuter, ComJava);
end;

procedure TFMessages.AdjustVariablesWidths(Tab: integer);
  var b: integer; SG: TStringGrid;
begin
  if (0 <= Tab-5) and (Tab-5 < InteractiveVariables.Count) then begin
    SG:= TStringGrid(InteractiveVariables.Objects[Tab-5]);
    b:= SG.Width div 3;
    SG.ColWidths[0]:= b-1;
    SG.ColWidths[1]:= b-1;
    SG.ColWidths[2]:= b;
  end;
end;

procedure TFMessages.LBInteractiveMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  for var i:= 0 to InteractiveEditors.Count - 1 do
    if GetInteractive(i+5) = (Sender as TInteractiveEdit) then ChangeTab(i+5);
  if canFocus then SetFocus;
end;

function TFMessages.GetInteractive(i: integer): TInteractiveEdit;
begin
  if (InteractiveEditors.Count > i-5) and (i-5 >= 0)
    then Result:= TInteractiveEdit(InteractiveEditors.Objects[i-5])
    else Result:= nil;
end;

function TFMessages.GetCurrentInteractive: TInteractiveEdit;
begin
  Result:= nil;
  if ActiveSubTool = 8 then begin
    var i:= TabControlMessages.TabIndex;
    if i >= 5 then
      Result:= TInteractiveEdit(InteractiveEditors.Objects[i-5]);
  end;
end;

function TFMessages.InteractiveEditActive: boolean;
begin
  Result:= (ActiveSubTool = 8);
end;

function TFMessages.GetCurrentStringGrid: TStringGrid;
begin
  Result:= nil;
  if ActiveSubTool = 8 then begin
    var i:= TabControlMessages.TabIndex;
    if i >= 5 then
      Result:= TStringGrid(InteractiveVariables.Objects[i-5]);
  end;
end;

procedure TFMessages.Run(const Classpath, Programm, CallParameter: string);
  var UMLForm: TFUMLForm;
begin
  Visible:= true;
  if canFocus then SetFocus;
  if assigned(FJava.ActiveTDIChild ) and (FJava.ActiveTDIChild.FormTag = 2) then
    UMLForm:= FJava.ActiveTDIChild as TFUMLForm
  else
    UMLForm:= FJava.InteractiveUMLForm;
  (UMLForm.MainModul.Diagram as TRtfdDiagram).CallMain(UnHideBlanks(Classpath), Programm, CallParameter);
end;

procedure TFMessages.SetModifiedUMLForm(i: integer);
begin
  if i >= 6 then
    TFUMLForm(InteractiveUMLForms.Objects[i-5]).Modified:= true;
end;

procedure TFMessages.DelInteractive(const path: string);
begin
  var i:= InteractiveEditors.IndexOf(path);
  if i > -1 then begin
    var aObject:= InteractiveEditors.Objects[i];
    // FreeAndNil(aObject);  ToDo check!
    InteractiveEditors.Delete(i);
    //InteractiveUMLForms.Objects[i] . Free;  UMLForm is Part of the GUI and destroyed by the system
    InteractiveUMLForms.Delete(i);
    aObject:= InteractiveVariables.Objects[i];
    FreeAndNil(aObject);
    InteractiveVariables.Delete(i);
    InteractiveExecuters.Delete(i);
    InteractiveComJavas.Delete(i);
    if InteractiveEditors.Count > 0
      then activeInteractive:= TInteractiveEdit(InteractiveEditors.Objects[0])
      else activeInteractive:= nil;
    if TabControlMessages.TabIndex = i+5 then ChangeTab(0);
    if TabControlMessages.Tabs.Count > i+5 then
      TabControlMessages.Tabs.Delete(i+5);
  end;
end;

procedure TFMessages.RenameInteractive(const FromPath, ToPath: string);
begin
  var i:= InteractiveEditors.IndexOf(FromPath);
  if i > -1 then begin
    try
      InteractiveEditors.Strings[i]:= ToPath;
      InteractiveUMLForms.Strings[i]:= ToPath;
      TabControlMessages.Tabs[i+5]:= ExtractFileNameEx(ToPath);
    except on e: exception do
      ErrorMsg(e.message);
    end;
  end;
end;

procedure TFMessages.OnMove(var Msg: TWMMove);
begin
  inherited;
  if Floating then
    aPosition:= Rect(Left, Top, Width, Height);
end;

procedure TFMessages.setMinHeight(min: integer);
begin
  if not Floating and (FJava.BottomDockPanel.Height < min) then begin
    FJava.BottomDockPanel.Height:= min;
    FJava.MoveHSplitter(min)
  end;
end;

procedure TFMessages.CutToClipboard;
begin
  case ActiveSubTool of
    0: MInterpreter.CutToClipboard;
    8: ActiveInteractive.CutToClipboard;
  end;
end;

procedure TFMessages.PasteFromClipboard;
begin
  case ActiveSubTool of
    0: MInterpreter.PasteFromClipboard;
    8: ActiveInteractive.PasteFromClipboard;
  end;
end;

procedure TFMessages.CopyToClipboard;
begin
  case ActiveSubTool of
    0: MInterpreter.CopyToClipboard;
    8: ActiveInteractive.CopyToClipboard;
  end;
end;

procedure TFMessages.Undo;
begin
  if ActiveSubTool = 8 then
    ActiveInteractive.Undo;
end;

procedure TFMessages.Redo;
begin
  if ActiveSubTool = 8 then
    ActiveInteractive.Redo;
end;

procedure TFMessages.LBCompilerEnter(Sender: TObject);
begin
  ActiveSubTool:= 1;
end;

procedure TFMessages.PDebuggerLeftEnter(Sender: TObject);
begin
  ActiveSubTool:= 2;
end;

procedure TFMessages.PDebuggerCenterLeftEnter(Sender: TObject);
begin
  ActiveSubTool:= 3;
end;

procedure TFMessages.PDebuggerCenterRightEnter(Sender: TObject);
begin
  ActiveSubTool:= 4;
end;

procedure TFMessages.PDebuggerRightEnter(Sender: TObject);
begin
  ActiveSubTool:= 5;
end;

procedure TFMessages.TVSearchEnter(Sender: TObject);
begin
  ActiveSubTool:= 6;
end;

procedure TFMessages.LBMessagesEnter(Sender: TObject);
begin
  ActiveSubTool:= 7;
end;

procedure TFMessages.PInteractiveMiddleEnter(Sender: TObject);
begin
  ActiveSubTool:= 8;
end;

procedure TFMessages.PInteractiveRightEnter(Sender: TObject);
begin
  ActiveSubTool:= 9;
end;

procedure TFMessages.PMainExit(Sender: TObject);
begin
  ActiveSubTool:= -1;
end;

procedure TFMessages.OnSplitterMoved(Sender: TObject);
begin
  if SplitterInteractiveLeft.Left > FJava.Width - 50 then
    SplitterInteractiveLeft.Left:= FJava.Width - 50;
end;

procedure TFMessages.SystemOutPrintln;
begin
  if ActiveSubTool = 8 then
    ActiveInteractive.SystemOutPrintln;
end;

procedure TFMessages.Execute(const s: string);
  var i: integer; aExecuter: TInteractiveExecuter;
begin
  if TabControlMessages.TabIndex < 5 then ShowTab(5);
  i:= TabControlMessages.TabIndex - 5;
  aExecuter:= TInteractiveExecuter(FMessages.InteractiveExecuters.Items[i]);
  aExecuter.execute(s);
end;

function TFMessages.NeedsSemicolon(const s: string): boolean;
  var aExecuter: TInteractiveExecuter;
begin
  if InteractiveExecuters.Count > 0 then begin
    aExecuter:= TInteractiveExecuter(InteractiveExecuters.Items[0]);
    Result:= aExecuter.NeedsSemicolon(s);
  end else
    Result:= false;
end;

function TFMessages.getCompileError(const Path: string): string;
  var i, j, k: Integer;
      FileWithPath: string;
begin
  Result:= '';
  i:= LBCompiler.Items.Count-1;
  while i > -1 do begin
    j:= i;
    FileWithPath:= GetFileWithPath(LBCompiler, i);
    if FileWithPath = Path then begin
      for k:= i to j do
        Result:= Result + '#13#10' + LBCompiler.Items[k];
      exit;
    end else
      i:= i - 1;
  end;
end;

procedure TFMessages.UpdateState;
begin                  {
  SetEnabledTB(TBStep, FJava.MIStep.Enabled);
  SetEnabledTB(TBNext, FJava.MINext.Enabled);
  SetEnabledTB(TBStepUp, FJava.MIStepUp.Enabled);
  SetEnabledTB(TBRunToCursor, FJava.MIRunToCursor.Enabled);
  SetEnabledTB(TBExecute, not myJavaCommands.ProcessRunning and not myDebugger.Running);
  }
end;

procedure TFMessages.StatusMessage(const s: string; Status: integer = 0);
begin
  StatusBar.Tag:= Status;
  StatusBar.Panels[0].Text:= '  ' + s;
end;

procedure TFMessages.StringGrid1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with Sender as Tstringgrid do
    begin
      //top row + displayed rows must not be larger than the total rows
      if TopRow+Visiblerowcount<rowcount then toprow:=toprow+1
    end;
  handled:=true;
end;

procedure TFMessages.StringGrid1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with Sender as Tstringgrid do
    begin
      if TopRow>fixedrows then toprow:=toprow-1
    end;
  handled:=true;
end;

procedure TFMessages.StatusBarDrawPanel(aStatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  case aStatusBar.Tag of
    0: aStatusBar.Canvas.Brush.Color:= StyleServices.GetSystemColor(clBtnFace);
    1: begin
         aStatusBar.Canvas.Font.Color:= clBlack;
         aStatusBar.Canvas.Brush.Color:= clLime;
       end;
    2: begin
         aStatusBar.Canvas.Font.Color:= clWhite;
         aStatusBar.Canvas.Brush.Color:= clRed;
       end;
  end;
  var aRect:= Rect;
  aRect.Width:= ClientWidth;
  aRect.Height:= StatusBar.Height;
  aStatusBar.Canvas.FillRect(aRect);
  aStatusBar.Canvas.Font.Assign(aStatusBar.Font);
  var h:= aStatusBar.Canvas.TextHeight('A');
  aStatusBar.Canvas.TextOut(Rect.Left, (StatusBar.Height - h) div 2 + 1, Panel.Text);
end;

procedure TFMessages.ClearStack;
begin
  LBStack.Clear;
end;

procedure TFMessages.ChangeStyle;
  var Details: TThemedElementDetails; i: integer;
begin
  if FConfiguration.isDark then begin
    DebuggerToolbar.Images:= vilDebuggerToolbarDark;
    TBInteractiveToolbar.Images:= vilInteractiveDark;
    PMMessages.Images:= vilPMMessagesDark;
  end else begin
    DebuggerToolbar.Images:= vilDebuggerToolbarLight;
    TBInteractiveToolbar.Images:= vilInteractiveLight;
    PMMessages.Images:= vilPMMessagesLight;
  end;
  if StyleServices.IsSystemStyle then begin
    BGColor:= clWhite;
    FGColor:= clBlack;
  end else begin
    Details:= StyleServices.GetElementDetails(tbsBackground);
    StyleServices.GetElementColor(Details, ecFillColor, BGColor);
    FGColor:= StyleServices.getStyleFontColor(sfTabTextInactiveNormal);
  end;
  MInterpreter.Color:= BGColor;
  MInterpreter.Font.Color:= FGColor;
  for i:= 0 to InteractiveEditors.Count - 1 do
    TInteractiveEdit(InteractiveEditors.Objects[i]).Loaded;
end;

procedure TFMessages.setActiveSubTool(value: integer);
begin
  fActiveSubTool:= value;
end;

procedure TFMessages.DeleteDebuggingTreeViews;
begin
  TreeViewDelete(TVAttributes, true);
  TreeViewDelete(TVLocalVariables, true);
  LBStack.Clear;
end;

procedure TFMessages.DPIChanged;
begin
  Hide;
  Show;
  SetStatusBarAndTabs;
  MISameWidthClick(Self);
end;

end.
