unit UMessages;

// ActiveSubTool
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
// 10: Interactive Memo

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.Contnrs,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.ToolWin,
  Vcl.ImgList,
  Vcl.BaseImageCollection,
  Vcl.VirtualImageList,
  SVGIconImageCollection,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  TB2Item,
  SynEdit,
  USynEditEx,
  UExecution,
  UComJava1;

type

  TArray3Stringlist = array [1 .. 3] of TStringList;

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
    Memo: TMemo;
    SGVariables: TStringGrid;
    Executer: TInteractiveExecuter;
    ComJava: TComJava1;
    constructor Create(UML: TForm; IEdit: TInteractiveEdit; AMemo: TMemo;
      SGrid: TStringGrid; AExecuter: TInteractiveExecuter; CJava: TComJava1);
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
    N01: TSpTBXSeparatorItem;
    MIExpand: TSpTBXItem;
    MICollapse: TSpTBXItem;
    PInteractive: TPanel;
    SplitterInteractiveLeft: TSplitter;
    PInteractiveLeft: TPanel;
    SplitterInteractiveRight: TSplitter;
    PInteractiveRight: TPanel;
    PInteractiveMiddle: TPanel;
    MIPaste: TSpTBXItem;
    TBInteractiveToolbar: TToolBar;
    TBExecute: TToolButton;
    TBShowUML: TToolButton;
    TBJavaReset: TToolButton;
    TBDelete: TToolButton;
    N04: TSpTBXSeparatorItem;
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
    PInterpreter: TPanel;
    MInterpreter: TMemo;
    PCompiler: TPanel;
    PMessages: TPanel;
    PSearch: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
    procedure TVWatchedExpressionsExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TVWatchedExpressionsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TVSearchDblClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure MInteractiveKeyPress(Sender: TObject; var Key: Char);
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
    procedure SplitterInteractiveRightMoved(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure TBShowUMLClick(Sender: TObject);
    procedure MInteractiveEnter(Sender: TObject);
    procedure MIDeleteClick(Sender: TObject);
    procedure TBDeleteClick(Sender: TObject);
    procedure TBJavaResetMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure StatusBarDrawPanel(PStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure StringGrid1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure StringGrid1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MISameWidthClick(Sender: TObject);
    procedure MInterpreterDblClick(Sender: TObject);
    procedure PInteractiveLeftEnter(Sender: TObject);
  private
    FPosition: TRect;
    FToJavaConsole: string;
    FBGColor: TColor;
    FFGColor: TColor;

    FInteractiveEditors: TStringList;
    FInteractiveMemos: TStringList;
    FInteractiveUMLForms: TStringList;
    FInteractiveVariables: TStringList;
    FInteractiveExecuters: TObjectList;
    FInteractiveComJavas: TObjectList;

    FActiveSubTool: Integer;
    FActiveInteractive: TInteractiveEdit;
    FUndocking: Boolean;
    FExpanded: TArray3Stringlist;
    FDumpActive: Boolean;
    FSearchGoalLine: Integer;
    FSearchGoalPath: string;
    procedure ShowMessagesOrInterpreter(const AFile: string; OutputNr: Integer);
    procedure SetActiveSubTool(Value: Integer);
    procedure SetStatusBarAndTabs;
    procedure Expand(Node: TTreeNode; Chapter: Integer);
    procedure Collapse(Node: TTreeNode; Chapter: Integer);
    procedure HideIt;
    function GetSelectedLines(ListBox: TListBox): string;
    procedure ScrollEnd(Tab: Integer);
    function CopyTreeView(TreeView: TTreeView; All: Boolean): string;
    function CopyTreeViewSelected(TreeView: TTreeView): string;
    function CopyTreeViewAll(TreeView: TTreeView): string;
    function GetFileWithPath(LBox: TListBox; var LineNo: Integer): string;
    procedure TreeViewDelete(TreeView: TTreeView; All: Boolean);
    procedure TreeViewDeleteSelected(TreeView: TTreeView);
    procedure TreeViewDeleteAll(TreeView: TTreeView);
    procedure TabControlChange(NewTab: Integer);
    function GetMemo(Tab: Integer): TMemo;
    function GetInteractive(Tab: Integer): TInteractiveEdit;
    procedure SetModifiedUMLForm(Tab: Integer);
    procedure AdjustVariablesWidths(Tab: Integer);
  public
    procedure ShowIt;
    procedure ShowTab(Tab: Integer); overload;
    procedure ShowTab(const Path: string); overload;
    procedure ChangeTab(Tab: Integer);
    procedure DeleteTab(Tab: Integer);
    procedure ShowMessages(const AFile: string);
    procedure ShowInterpreter(const AFile: string);
    procedure OutputTo(Tab: Integer; Lines: TStrings);
    procedure OutputToTerminal(Message: string);
    procedure OutputLineTo(Tab: Integer; const Message: string);
    function MyIsVisible: Boolean;
    procedure ChangeHideShow;
    function GetFont: TFont;
    procedure SetFont(Font: TFont);
    procedure SetFontSize(Delta: Integer);
    procedure InitAndShow;
    procedure MyDock;
    procedure Undock;
    procedure SaveWindow;
    procedure ShowWatchedExpressions;
    function AddInteractive(UMLForm: TForm; const Path: string): TInteractive;
    function GetCurrentInteractive: TInteractiveEdit;
    function GetCurrentStringGrid: TStringGrid;
    procedure DelInteractive(const Path: string);
    procedure Run(const Classpath, Programm, Callparameter: string);
    procedure RenameInteractive(const FromPath, ToPath: string);
    procedure SetMinHeight(Min: Integer);
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure CopyToClipboard;
    procedure Undo;
    procedure Redo;
    procedure SystemOutPrintln;
    procedure Execute(const Command: string);
    function NeedsSemicolon(const Command: string): Boolean;
    function GetCompileError(const Path: string): string;
    function InteractiveEditActive: Boolean;
    procedure StatusMessage(const Message: string; Status: Integer = 0);
    procedure ClearStack;
    procedure ChangeStyle;
    procedure DeleteDebuggingTreeViews;
    procedure DPIChanged;
    procedure SetDumpActive(Value: Boolean);

    property ActiveSubTool: Integer read FActiveSubTool write SetActiveSubTool;
    property ActiveInteractive: TInteractiveEdit read FActiveInteractive;
    property Undocking: Boolean read FUndocking write FUndocking;
    property Expanded: TArray3Stringlist read FExpanded;
    property DumpActive: Boolean read FDumpActive;
    property SearchGoalLine: Integer read FSearchGoalLine;
    property SearchGoalPath: string read FSearchGoalPath;
end;

var
  FMessages: TFMessages = nil;

implementation

uses
  System.Math,
  System.SysUtils,
  System.IOUtils,
  System.Types,
  Vcl.Clipbrd,
  Vcl.Themes,
  JvGnugettext,
  UStringRessources,
  UUtils,
  UConfiguration,
  UEditorForm,
  UJavaCommands,
  UGrepResults,
  UWatches,
  UUMLForm,
  URtfdDiagram,
  UJava,
  UDebugger;

{$R *.DFM}
{ -- TInteractive ------------------------------------------------------------ }

constructor TInteractive.Create(UML: TForm; IEdit: TInteractiveEdit;
  AMemo: TMemo; SGrid: TStringGrid; AExecuter: TInteractiveExecuter;
  CJava: TComJava1);
begin
  UMLForm := UML;
  InteractiveEditor := IEdit;
  Memo := AMemo;
  SGVariables := SGrid;
  Executer := AExecuter;
  ComJava := CJava;
end;

{ -- TInteractiveEdit -------------------------------------------------------- }

constructor TInteractiveEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := FMessages.PInteractiveMiddle;
  PopupMenu := FMessages.PMMessages;
  OnMouseDown := FMessages.LBInteractiveMouseDown;
  OnStatusChange := EditorStatusChange;
  OnEnter := Enter;
  AddKeyUpHandler(aKeyUpHandler);
  Lines.Add('');
  Gutter.Visible := False;
  // Font.Assign(FMessages.MInterpreter.Font);  ToDO
  var
  Opt := Options;
  Exclude(Opt, eoScrollPastEol);
  Options := Opt;
  Align := alClient;
  RightEdge := 0;
  Tag := 8;
  if FConfiguration.NoSyntaxHighlighting then
    Highlighter := nil
  else
    Highlighter := FConfiguration.GetHighlighter('.java');
end;

procedure TInteractiveEdit.SystemOutPrintln;
var
  XPos: Integer;
begin
  if SelText = '' then
    XPos := CaretX
  else
    XPos := BlockBegin.Char;
  SelText := 'System.out.println();';
  CaretX := XPos + 19;
  EnsureCursorPosVisible;
end;

procedure TInteractiveEdit.UpdateState;
begin
  with FJava do
  begin
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

procedure TInteractiveEdit.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if scModified in Changes then
    FMessages.SetModifiedUMLForm(FMessages.TabControlMessages.TabIndex);
  UpdateState;
end;

procedure TInteractiveEdit.Enter(Sender: TObject);
begin
  SetEnabledMI(FJava.MISystemOutPrintln, Enabled);
  FJava.UpdateMenuItems(nil);
  FJava.scpSetEditor(Self);
end;

var
  SKeyUp: string;

procedure TInteractiveEdit.aKeyUpHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    var
    Tab := FMessages.TabControlMessages.TabIndex - 5;
    if Tab >= 0 then
    begin
      if SKeyUp <> '' then
        SKeyUp := SKeyUp + #13#10;
      SKeyUp := SKeyUp + Lines[CaretY - 2];
      if Shift = [] then
      begin
        var
        Executer := TInteractiveExecuter(FMessages.FInteractiveExecuters[Tab]);
        Executer.Execute(SKeyUp);
        SKeyUp := '';
        EnsureCursorPosVisible;
        if (Tab = FMessages.TabControlMessages.TabIndex - 5) and CanFocus then
          SetFocus;
      end;
      Key := 0;
    end;
  end
  else if (Key = VK_DOWN) and (CaretY >= Lines.Count) then
    Lines.Add('')
  else if (Key = Ord('U')) and (ssCtrl in Shift) then
    SystemOutPrintln;
end;

{ --- TFMessages ------------------------------------------------------------- }

procedure TFMessages.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ActiveSubTool := -1;
  FUndocking := False;
  FInteractiveEditors := TStringList.Create;
  FInteractiveMemos := TStringList.Create;
  FInteractiveUMLForms := TStringList.Create;
  FInteractiveVariables := TStringList.Create;
  FInteractiveExecuters := TObjectList.Create;
  FInteractiveComJavas := TObjectList.Create;
  for var I := 1 to 3 do
  begin
    FExpanded[I] := TStringList.Create;
    FExpanded[I].Sorted := True;
    FExpanded[I].Duplicates := dupIgnore;
  end;
  ChangeStyle;
  ShowWatchedExpressions;
  FDumpActive := False;
  FToJavaConsole := '';
  FActiveInteractive := nil;
end;

procedure TFMessages.FormDestroy(Sender: TObject);
var
  I: Integer;
  AObject: TObject;
begin
  if TVSearch.Items.Count > 0 then
    myGrepResults.DeleteSearchResults;
  FreeAndNil(myGrepResults);
  for I := 0 to FInteractiveEditors.Count - 1 do
  begin
    AObject := FInteractiveEditors.Objects[I];
    FreeAndNil(AObject);
  end;
  FreeAndNil(FInteractiveEditors);
  for I := 0 to FInteractiveMemos.Count - 1 do
  begin
    AObject := FInteractiveMemos.Objects[I];
    FreeAndNil(AObject);
  end;
  FreeAndNil(FInteractiveMemos);
  FreeAndNil(FInteractiveUMLForms);
  for I := 0 to FInteractiveVariables.Count - 1 do
  begin
    AObject := FInteractiveVariables.Objects[I];
    FreeAndNil(AObject);
  end;
  FreeAndNil(FInteractiveVariables);
  FreeAndNil(FInteractiveExecuters);
  FreeAndNil(FInteractiveComJavas);
  for I := 1 to 3 do
    FreeAndNil(FExpanded[I]);
end;

procedure TFMessages.FormShow(Sender: TObject);
begin
  TBStep.Hint := FJava.MIStep.Caption;
  TBNext.Hint := FJava.MINext.Caption;
  TBStepUp.Hint := FJava.MIStepUp.Caption;
  TBRunToCursor.Hint := FJava.MIRunToCursor.Caption;
  TBShowExecutionPoint.Hint := FJava.MIShowExecutionPoint.Caption;
  TBExpression.Hint := FJava.MIExpression.Caption;
  TBWatches.Hint := FJava.MIWatches.Caption;
  FJava.ActiveTool := 7;
  if CanFocus then
    SetFocus;
end;

procedure TFMessages.SetStatusBarAndTabs;
begin
  StatusBar.Font.Size := FConfiguration.Fontsize;
  StatusBar.Canvas.Font.Size := FConfiguration.Fontsize;
  var
  Height := StatusBar.Canvas.TextHeight('Ag') + 4;
  StatusBar.Height := Height;
  PAttribute.Height := Height;
  PLocalVariables.Height := Height;
  PWatches.Height := Height;
  PStack.Height := Height;
  TabControlMessages.Font.Size := FConfiguration.Fontsize;
  TabControlMessages.Canvas.Font.Size := FConfiguration.Fontsize;
  TabControlMessages.Height := Height;
  TabControlMessages.TabHeight := Height;
end;

procedure TFMessages.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
  FJava.ShowDockPanel(FJava.BottomDockPanel, False, nil);
  FJava.MIMessages.Checked := False;
  FJava.ActiveTool := -1;
end;

procedure TFMessages.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool := 7;
  FJava.UpdateMenuItems(Self);
end;

procedure TFMessages.InitAndShow;
var
  MyVisible, Docked: Boolean;
  AFont: TFont;
  Left, Top, Width, Height: Integer;
begin
  LockFormUpdate(Self);
  MyVisible := FConfiguration.ReadBoolU('Messages', 'Visible', True);
  Docked := FConfiguration.ReadBoolU('Messages', 'Docked', True);
  Width := PPIScale(FConfiguration.ReadIntegerU('Messages', 'Width', 200));
  if Width < 50 then
    Width := 50;
  if Width > Screen.Width - 50 then
    Width := Screen.Width - 50;
  Height := PPIScale(FConfiguration.ReadIntegerU('Messages', 'Height', 200));
  if Height < 50 then
    Height := 50;
  if Height > Screen.Height - 50 then
    Height := Screen.Height - 50;
  Left := PPIScale(FConfiguration.ReadIntegerU('Messages', 'Left', 100));
  if Left + Width < 50 then
    Left := 50 - Width;
  if Screen.Width - Left < 50 then
    Left := Screen.Width - 50;
  Top := PPIScale(FConfiguration.ReadIntegerU('Messages', 'Top', 500));
  if Top + Width < 50 then
    Top := 50 - Height;
  if Screen.Height - Top < 50 then
    Top := Screen.Height - 50;
  FPosition := Rect(Left, Top, Width, Height);
  SetBounds(Left, Top, Width, Height);
  if Docked then
    MyDock;
  ShowIt;
  if not MyVisible then
    HideIt;

  AFont := TFont.Create;
  AFont.Name := FConfiguration.ReadStringU('Messages', 'FontName', 'Consolas');
  AFont.Size := PPIScale(FConfiguration.ReadIntegerU('Messages',
    'FontSize', 10));
  SetFont(AFont);
  FreeAndNil(AFont);
  SetStatusBarAndTabs;

  PInteractiveLeft.Width := PPIScale(FConfiguration.ReadIntegerU('Messages',
    'InteractiveLeft', ClientWidth div 3));
  PInteractiveMiddle.Width := PPIScale(FConfiguration.ReadIntegerU('Messages',
    'InteractiveMiddle', ClientWidth div 3));

  PDebuggerLeft.Width := PPIScale(FConfiguration.ReadIntegerU('Messages',
    'DebuggerLeft', ClientWidth div 4));
  PDebuggerCenterLeft.Width := PPIScale(FConfiguration.ReadIntegerU('Messages',
    'DebuggerCenterLeft', ClientWidth div 4));
  PDebuggerCenterRight.Width := PPIScale(FConfiguration.ReadIntegerU('Messages',
    'DebuggerCenterRight', ClientWidth div 4));

  TabControlChange(FConfiguration.ReadIntegerU('Messages', 'TabIndex', 1));
  FConfiguration.RemoveShortcutsFrom(PMMessages);
  UnlockFormUpdate(Self);
end;

procedure TFMessages.SaveWindow;
begin
  // Don't use with, otherwise PPIUnScale is calculated for FConfiguration!
  FConfiguration.WriteBoolU('Messages', 'Visible', MyIsVisible);
  FConfiguration.WriteBoolU('Messages', 'Docked', not FMessages.Floating);
  FConfiguration.WriteStringU('Messages', 'FontName', MInterpreter.Font.Name);
  FConfiguration.WriteIntegerU('Messages', 'FontSize',
    PPIUnScale(MInterpreter.Font.Size));
  FConfiguration.WriteIntegerU('Messages', 'InteractiveLeft',
    PPIUnScale(PInteractiveLeft.Width));
  FConfiguration.WriteIntegerU('Messages', 'InteractiveMiddle',
    PPIUnScale(PInteractiveMiddle.Width));

  FConfiguration.WriteIntegerU('Messages', 'DebuggerLeft',
    PPIUnScale(PDebuggerLeft.Width));
  FConfiguration.WriteIntegerU('Messages', 'DebuggerCenterLeft',
    PPIUnScale(PDebuggerCenterLeft.Width));
  FConfiguration.WriteIntegerU('Messages', 'DebuggerCenterRight',
    PPIUnScale(PDebuggerCenterRight.Width));

  FConfiguration.WriteIntegerU('Messages', 'TabIndex',
    TabControlMessages.TabIndex);
  if FMessages.Floating then
  begin
    FConfiguration.WriteIntegerU('Messages', 'Left', PPIUnScale(Self.Left));
    FConfiguration.WriteIntegerU('Messages', 'Top', PPIUnScale(Self.Top));
    FConfiguration.WriteIntegerU('Messages', 'Width', PPIUnScale(Self.Width));
    FConfiguration.WriteIntegerU('Messages', 'Height', PPIUnScale(Self.Height));
  end
  else
  begin
    FConfiguration.WriteIntegerU('Messages', 'Left',
      PPIUnScale(Self.FPosition.Left));
    FConfiguration.WriteIntegerU('Messages', 'Top',
      PPIUnScale(Self.FPosition.Top));
    FConfiguration.WriteIntegerU('Messages', 'Width',
      PPIUnScale(Self.FPosition.Right));
    FConfiguration.WriteIntegerU('Messages', 'Height',
      PPIUnScale(Self.FPosition.Bottom));
    FConfiguration.WriteIntegerU('Messages', 'Splitter',
      PPIUnScale(FJava.BottomDockPanel.Height));
  end;
  FConfiguration.WriteIntegerU('Messages', 'TabIndex',
    Min(TabControlMessages.TabIndex, 5));
end;

procedure TFMessages.TabControlChange(NewTab: Integer);
var
  IEdit: TInteractiveEdit;
begin
  PInterpreter.Visible := False;
  PCompiler.Visible := False;
  PDebugger.Visible := False;
  PSearch.Visible := False;
  PMessages.Visible := False;
  PInteractive.Visible := False;
  case NewTab of
    0:
      begin
        PInterpreter.Visible := True;
        //if MInterpreter.CanFocus then MInterpreter.SetFocus;    a lot of exceptions
      end;
    1:
      begin
        PCompiler.Visible := True;
        // if LBCompiler.CanFocus then LBCompiler.SetFocus;
      end;
    2:
      begin
        PDebugger.Visible := True;
        // if TVAttributes.CanFocus then TVAttributes.SetFocus;
      end;
    3:
      begin
        PSearch.Visible := True;
        // if TVSearch.CanFocus then TVSearch.SetFocus;
      end;
    4:
      begin
        PMessages.Visible := True;
        // if LBMessages.CanFocus then LBMessages.SetFocus;
      end;
  else
    begin
      PInteractive.Visible := True;
      for var I := 0 to FInteractiveEditors.Count - 1 do
      begin
        TInteractiveEdit(FInteractiveEditors.Objects[I]).Visible := False;
        TMemo(FInteractiveMemos.Objects[I]).Visible := False;
        TStringGrid(FInteractiveVariables.Objects[I]).Visible := False;
      end;
      IEdit := GetInteractive(NewTab);
      if Assigned(IEdit) then
      begin
        FActiveInteractive := IEdit;
        IEdit.Visible := True;
        if IEdit.CanFocus then // responsible for inactive window exception
          IEdit.SetFocus; // this gave an unexpected exception
        IEdit.UpdateState;
        TStringGrid(FInteractiveVariables.Objects[NewTab - 5]).Show;
        AdjustVariablesWidths(NewTab);
        TComJava1(FInteractiveComJavas[NewTab - 5])
          .setActiveComJava(TComJava1(FInteractiveComJavas[NewTab - 5]));
        IEdit.Enter(Self);
      end;
      GetMemo(NewTab).Visible := True;
    end;
  end;
  TabControlMessages.TabIndex := NewTab;
  FJava.UpdateMenuItems(nil);
end;

procedure TFMessages.TabControlMessagesChange(Sender: TObject);
begin
  // works only in undocked state
  TabControlChange(TabControlMessages.TabIndex);
end;

procedure TFMessages.TBDeleteClick(Sender: TObject);
begin
  var
  IEdit := GetInteractive(TabControlMessages.TabIndex);
  if IEdit.SelAvail then
    IEdit.ClearSelection
  else
    IEdit.Clear;
end;

procedure TFMessages.ShowTab(Tab: Integer);
begin
  if not Visible then
    ShowIt;
  if TabControlMessages.TabIndex <> Tab then
  begin
    ChangeTab(Tab);
    Update;
  end;
end;

procedure TFMessages.ShowTab(const Path: string);
begin
  var
  I := 0;
  while I < FInteractiveEditors.Count do
  begin
    if Path = FInteractiveEditors[I] then
    begin
      ChangeTab(I + 5);
      Break;
    end;
    Inc(I);
  end;
  Update;
end;

procedure TFMessages.ChangeTab(Tab: Integer);
begin
  if MyIsVisible and (TabControlMessages.TabIndex <> Tab) then
  begin
    TabControlMessages.TabIndex := Tab;
    TabControlMessagesChange(Self);
    AdjustVariablesWidths(Tab);
  end;
end;

procedure TFMessages.DeleteTab(Tab: Integer);
begin
  case Tab of
    0:
      MInterpreter.Text := '';
    1:
      LBCompiler.Clear;
    2:
      ;
    3:
      ;
    4:
      LBMessages.Clear;
  else
    begin
      GetMemo(Tab).Clear;
      GetInteractive(Tab).Clear;
      SetModifiedUMLForm(Tab);
    end;
  end;
end;

procedure TFMessages.ShowMessagesOrInterpreter(const AFile: string;
  OutputNr: Integer);
begin
  var
  Messages := TStringList.Create;
  try
    Messages.LoadFromFile(AFile);
    OutputTo(OutputNr, Messages);
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
  ChangeTab(OutputNr);
  FreeAndNil(Messages);
end;

procedure TFMessages.ShowMessages(const AFile: string);
begin
  ShowMessagesOrInterpreter(AFile, K_Messages);
end;

procedure TFMessages.ShowInterpreter(const AFile: string);
begin
  ShowMessagesOrInterpreter(AFile, K_Interpreter);
end;

procedure TFMessages.OutputTo(Tab: Integer; Lines: TStrings);
begin
  case Tab of
    0:
      MInterpreter.Lines.AddStrings(Lines);
    1:
      LBCompiler.Items.AddStrings(Lines);
    2:
      ;
    3:
      ;
    4:
      LBMessages.Items.AddStrings(Lines);
  else
    begin
      GetInteractive(Tab).Lines.Add(Lines.Text);
      SetModifiedUMLForm(Tab);
    end;
  end;
  ScrollEnd(Tab);
end;

procedure TFMessages.OutputToTerminal(Message: string);
begin
  if EndsWith(Message, CrLf) then
    Delete(Message, Length(Message) - 1, 2);
  var
  Tab := TabControlMessages.TabIndex;
  if Tab > 4 then
  begin
    GetMemo(Tab).Lines.Add(Message);
    ScrollEnd(Tab);
  end
  else
  begin
    MInterpreter.Lines.Add(Message);
    ScrollEnd(0);
  end;
end;

procedure TFMessages.OutputLineTo(Tab: Integer; const Message: string);
begin
  case Tab of
    0:
      MInterpreter.Lines.Add(Message);
    1:
      LBCompiler.Items.Add(Message);
    2:
      ;
    3:
      ;
    4:
      LBMessages.Items.Add(Message);
  else
    begin
      GetInteractive(Tab).Lines.Add(Message);
      SetModifiedUMLForm(Tab);
    end;
  end;
  ScrollEnd(Tab);
end;

function TFMessages.GetFileWithPath(LBox: TListBox;
  var LineNo: Integer): string;
var
  Str, Pathname, Filename, Path: string;
begin
  Result := '';
  while LineNo >= 0 do
  begin
    Str := LBox.Items[LineNo];
    var Pos := Pos('.java:', Str);
    if Pos > 0 then
    begin
      Delete(Str, Pos + 5, Length(Str));
      if Copy(Str, 1, 2) = '.\' then // .\filename.java
        Delete(Str, 1, 2);
      if TPath.DriveExists(Str) or IsUNC(Str) then
        Exit(Str);
      for var J := 0 to FJava.TDIEditFormCount - 1 do
      begin
        Pathname := FJava.TDIEditFormGet(J).Pathname;
        Filename := ExtractFileName(Pathname);
        if (Filename = Str) and FileExists(Pathname) then
          Exit(Pathname);
        Path := ExtractFilePath(Pathname);
        if FileExists(Path + Str) then
          Exit(Path + Str);
      end;
    end;
    Dec(LineNo);
  end;
end;

procedure TFMessages.LBCompilerDblClick(Sender: TObject);
var
  I, Pos1, Pos2, Line, Column: Integer;
  Str, Str1, Str2, FileWithPath: string;
begin
  // activate double-clicked errorline in editor-window
  I := LBCompiler.ItemIndex;
  Pos2 := I;
  FileWithPath := GetFileWithPath(LBCompiler, Pos2);
  repeat
    Str1 := LBCompiler.Items[I];
    Pos1 := Pos('.java:', Str1); // VisibleStack.java:12:
    if Pos1 > 0 then
    begin
      Str2 := Copy(Str1, Pos1 + 6, 255);
      Pos2 := Pos(':', Str2); // compile error
      if Pos2 > 0 then
      begin // errorline found
        LBCompiler.ItemIndex := I;
        if not TryStrToInt(Copy(Str2, 1, Pos2 - 1), Line) then
          Continue;
        Delete(Str2, 1, Pos2);
        Pos2 := Pos(':', Str2);
        if not TryStrToInt(Copy(Str2, 1, Pos2 - 1), Column) then
          Column := 1;
        // position cursor in editor
        if Copy(Trim(Str1), 1, 2) = '.\' then // relativ path
          FileWithPath := ExtractFilePath(FileWithPath) +
            ExtractFileName(Copy(Str1, 1, Pos1 + 4));
        FJava.ChangeWindowWithPositioning(ToWindows(FileWithPath), Column,
          Line, True);
        Break;
      end;
    end;
    I := I - 1;
  until I = -1;
  if I = -1 then
  begin
    I := LBCompiler.ItemIndex;
    Str := LBCompiler.Items[I];
    if (Pos('-deprecation', Str) > 0) and FileExists(FileWithPath) then
    begin
      FConfiguration.JavaCompilerParameter := '-deprecation ' +
        FConfiguration.JavaCompilerParameter;
      myJavaCommands.Compile(FileWithPath, '');
      Delete(FConfiguration.JavaCompilerParameter, 1, 13);
    end
    else if (Pos('-Xlint:unchecked ', Str) > 0) and FileExists(FileWithPath) then
    begin
      FConfiguration.JavaCompilerParameter := '-Xlint:unchecked ' +
        FConfiguration.JavaCompilerParameter;
      myJavaCommands.Compile(FileWithPath, '');
      Delete(FConfiguration.JavaCompilerParameter, 1, 17);
    end;
  end;
end;

procedure TFMessages.MInterpreterDblClick(Sender: TObject);
var
  I, Pos0, Pos1, Pos2, Pos3, Line: Integer;
  Str1, Str2, FileWithPath: string;
begin
  // activate double-clicked errorline in editor-window
  I := MInterpreter.CaretPos.Y + 1;
  Pos3 := 0;
  while (I > 0) and (Pos3 = 0) do
  begin
    Dec(I);
    Str1 := Trim(MInterpreter.Lines[I]);
    Pos3 := Pos('Exception', Str1); // Exception at
  end;

  if Pos3 > 0 then
  begin
    Str1 := MInterpreter.Lines[I + 1];
    Pos0 := Pos('(', Str1);
    Pos1 := Pos('.java:', Str1); // VisibleStack.java:12:
    if Pos1 > 0 then
    begin
      Str2 := Copy(Str1, Pos1 + 6, 255);
      Pos2 := Pos(')', Str2); // compile error
      if Pos2 > 0 then
      begin // errorline found
        MInterpreter.CaretPos := Point(1, I + 1);
        if not TryStrToInt(Copy(Str2, 1, Pos2 - 1), Line) then
          Exit;
        FileWithPath := FJava.GetPathnameForClass(Copy(Str1, Pos0 + 1, Pos1 - Pos0 + 4));
        if FileWithPath <> '' then
          FJava.ChangeWindowWithPositioning(FileWithPath, -1, Line, True);
      end;
    end;
  end;
end;

procedure TFMessages.MIGotoErrorClick(Sender: TObject);
begin
  case TabControlMessages.TabIndex of
    0:
      begin
        MInterpreter.CaretPos := Point(1, MInterpreter.Lines.Count);
        MInterpreterDblClick(Self);
      end;
    1:
      begin
        var
        I := 0;
        while (I < LBCompiler.Items.Count) and
          (Pos(': error: ', LBCompiler.Items[I]) = 0) do
          Inc(I);
        if I < LBCompiler.Items.Count then
        begin
          LBCompiler.ItemIndex := I;
          LBCompilerDblClick(Self);
        end;
      end;
  end;
end;

procedure TFMessages.LBMessagesDblClick(Sender: TObject);
var
  I, Pos1, Pos2, Line, Column: Integer;
  Str, FileWithPath: string;
begin
  // Activate the double-clicked error line in the editor window
  I := LBMessages.ItemIndex;
  Pos1 := 0;
  FileWithPath := GetFileWithPath(LBMessages, Pos1);
  Str := LBMessages.Items[I];
  Pos1 := Pos('.java:', Str); // VisibleStack.java:12:
  if Pos1 > 0 then
  begin
    Str := Copy(Str, Pos1 + 6, 255);
    Pos2 := Pos(':', Str); // Style check failed
    if Pos2 > 0 then
    begin // error line found
      LBMessages.ItemIndex := I;
      if TryStrToInt(Copy(Str, 1, Pos2 - 1), Line) then
      begin
        Str := Copy(Str, Pos2 + 1, 255);
        Pos2 := Pos(':', Str);
        if Pos2 = 0 then
          Column := -1
        else if not TryStrToInt(Copy(Str, 1, Pos2 - 1), Column) then
          Exit;
        FJava.ChangeWindowWithPositioning(ToWindows(FileWithPath), Column,
          Line, True);
      end;
    end;
  end;
end;

procedure TFMessages.ScrollEnd(Tab: Integer);
var
  IEdit: TInteractiveEdit;

  procedure ScrollEndMemo(Memo: TMemo);
  begin
    Memo.Perform(EM_LINESCROLL, 0, Memo.Lines.Count - 1);
    Memo.Update;
    Memo.SelStart := Length(Memo.Text) - 2;
  end;

begin
  case Tab of
    0:
      ScrollEndMemo(MInterpreter);
    1:
      ScrollEndListBox(LBCompiler);
    2:
      ;
    3:
      ;
    4:
      ScrollEndListBox(LBMessages);
  else
    begin
      ScrollEndMemo(GetMemo(Tab));
      IEdit := GetInteractive(Tab);
      IEdit.TopLine := IEdit.Lines.Count - 2;
    end;
  end;
end;

procedure TFMessages.SetFont(Font: TFont);
begin
  MInterpreter.Font.Assign(Font);
  LBCompiler.Font.Assign(Font);
  TVAttributes.Font.Assign(Font);
  TVLocalVariables.Font.Assign(Font);
  TVWatchedExpressions.Font.Assign(Font);
  LBStack.Font.Assign(Font);
  TVSearch.Font.Assign(Font);
  LBMessages.Font.Assign(Font);
  for var I := 0 to FInteractiveEditors.Count - 1 do
  begin
    GetMemo(I + 5).Font.Assign(Font);
    GetInteractive(I + 5).Font.Assign(Font);
    TStringGrid(FInteractiveVariables.Objects[I]).Font.Assign(Font);
    TStringGrid(FInteractiveVariables.Objects[I]).DefaultRowHeight :=
      Round(Font.Size * 1.8);
  end;
end;

function TFMessages.GetFont: TFont;
begin
  Result := MInterpreter.Font;
end;

procedure TFMessages.SetFontSize(Delta: Integer);
begin
  var
  Size := MInterpreter.Font.Size + Delta;
  if Size < 6 then
    Size := 6;
  MInterpreter.Font.Size := Size;
  LBCompiler.Font.Size := Size;
  TVAttributes.Font.Size := Size;
  TVLocalVariables.Font.Size := Size;
  TVWatchedExpressions.Font.Size := Size;
  LBStack.Font.Size := Size;
  TVSearch.Font.Size := Size;
  LBMessages.Font.Size := Size;
  StatusBar.Font.Size := Size;
  for var I := 0 to FInteractiveEditors.Count - 1 do
  begin
    GetMemo(I + 5).Font.Size := Size;
    GetInteractive(I + 5).Font.Size := Size;
    SetModifiedUMLForm(I + 5);
    TStringGrid(FInteractiveVariables.Objects[I]).Font.Size := Size;
    TStringGrid(FInteractiveVariables.Objects[I]).DefaultRowHeight :=
      Round(Size * 1.8);
  end;
  Show;
end;

function TFMessages.MyIsVisible: Boolean;
begin
  if Floating then
    Result := FMessages.Visible
  else
    Result := FMessages.Visible and (FJava.BottomDockPanel.Height > 1);
end;

procedure TFMessages.ShowIt;
begin
  if not MyIsVisible then
    if Floating then
      Show
    else
      FJava.ShowDockPanel(FJava.BottomDockPanel, True, FMessages);
end;

procedure TFMessages.HideIt;
begin
  if MyIsVisible then
    if Floating then
      Hide
    else
      FJava.ShowDockPanel(FJava.BottomDockPanel, False, nil);
end;

procedure TFMessages.ChangeHideShow;
begin
  if MyIsVisible then
    HideIt
  else
    ShowIt;
end;

procedure TFMessages.MyDock;
begin
  if Floating then
    ManualDock(FJava.BottomDockPanel, nil, alTop);
  TabControlMessages.Top := ClientHeight;
end;

procedure TFMessages.Undock;
begin
  if not Floating then
  begin
    var
    Pos := FPosition;
    Pos.Right := FPosition.Left + FPosition.Right;
    Pos.Bottom := FPosition.Top + FPosition.Bottom;
    ManualFloat(Pos);
  end;
end;

procedure TFMessages.MIDockClick(Sender: TObject);
begin
  if Floating then
    MyDock;
end;

procedure TFMessages.MIUndockClick(Sender: TObject);
begin
  if not FMessages.Floating then
    FMessages.Undock;
end;

procedure TFMessages.TVWatchedExpressionsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Node: TTreeNode;
begin
  if Key = VK_INSERT then
    FJava.MIWatchesClick(Self)
  else if Key = VK_DELETE then
    with TVWatchedExpressions do
    begin
      Node := Selected;
      if Assigned(Node) and not Assigned(Node.Parent) then
      begin
        var Str := Node.Text;
        var Posi := Pos(' = ', Str);
        if Posi > 0 then
          Delete(Str, Posi, Length(STr));
        Items.Delete(Node);
        FWatches.Delete(Str);
      end;
    end;
end;

procedure TFMessages.LBStackDblClick(Sender: TObject);
var
  Str, AClass, Path: string;
  Line, Pos1, Pos2, Pos3: Integer;
begin
  if (0 <= LBStack.ItemIndex) and (LBStack.ItemIndex < LBStack.Items.Count) then
  begin
    Str := LBStack.Items[LBStack.ItemIndex];
    Pos1 := Pos('(', Str);
    Pos2 := Pos(')', Str);
    Str := Copy(Str, Pos1 + 1, Pos2 - Pos1 - 1);
    Pos3 := Pos(':', Str);
    AClass := Copy(Str, 1, Pos3 - 1);
    Delete(Str, 1, Pos3);
    try
      if not TryStrToInt(Str, Line) then
        Exit;
      Pos3 := Pos('.', AClass);
      Delete(AClass, Pos3, Length(AClass));
      Path := FJava.GetPathnameForClass(AClass);
      if Path <> '' then
      begin
        FSearchGoalLine := Line;
        FSearchGoalPath := Path;
        FJava.ChangeWindowWithPositioning(Path, 1, Line, False);
      end;
    except
    end;
  end;
end;

procedure TFMessages.LBStackKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    LBCompiler.DeleteSelected;
end;

procedure TFMessages.TVWatchedExpressionsDblClick(Sender: TObject);
begin
  FJava.MIWatchesClick(Self);
end;

procedure TFMessages.MIDeleteClick(Sender: TObject);
begin
  case ActiveSubTool of
    0:
      if MInterpreter.SelLength > 0 then
        MInterpreter.ClearSelection
      else
        MInterpreter.Clear;
    1:
      LBCompiler.DeleteSelected;
    2:
      TreeViewDeleteSelected(TVAttributes);
    3:
      TreeViewDeleteSelected(TVLocalVariables);
    4:
      TreeViewDeleteSelected(TVWatchedExpressions);
    5:
      LBStack.DeleteSelected;
    6:
      TreeViewDeleteSelected(TVSearch);
    7:
      LBMessages.DeleteSelected;
    8:
      begin
        GetInteractive(TabControlMessages.TabIndex).ClearSelection;
        SetModifiedUMLForm(TabControlMessages.TabIndex);
      end;
    10:
      GetMemo(TabControlMessages.TabIndex).ClearSelection;
  end;
end;

procedure TFMessages.MIDeleteAllClick(Sender: TObject);
begin
  case ActiveSubTool of
    0:
      MInterpreter.Clear;
    1:
      LBCompiler.Clear;
    2:
      TreeViewDeleteAll(TVAttributes);
    3:
      TreeViewDeleteAll(TVLocalVariables);
    4:
      TreeViewDeleteAll(TVWatchedExpressions);
    5:
      LBStack.Clear;
    6:
      TreeViewDeleteAll(TVSearch);
    7:
      LBMessages.Clear;
    8:
      begin
        GetInteractive(TabControlMessages.TabIndex).Clear;
        SetModifiedUMLForm(TabControlMessages.TabIndex);
      end;
    10:
      GetMemo(TabControlMessages.TabIndex).Clear;
  end;
end;

procedure TFMessages.TBExecuteClick(Sender: TObject);
var
  Form: TFEditForm;
  IEdit: TSynEdit;
  Executer: TInteractiveExecuter;
  Str: string;
begin
  TBExecute.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    var I := TabControlMessages.TabIndex - 5;
    if I >= 0 then
    begin
      if Assigned(FInteractiveUMLForms.Objects[I]) then
      begin
        Form := FJava.getActiveEditor;
        if TSynEdit(FInteractiveEditors.Objects[I]).SelAvail then
          IEdit := TSynEdit(FInteractiveEditors.Objects[I])
        else if Assigned(Form) and Form.Editor.SelAvail then
          IEdit := Form.Editor
        else
          IEdit := TSynEdit(FInteractiveEditors.Objects[I]);
        if IEdit.SelAvail then
          Str := IEdit.GetLinesWithSelection
        else
          Str := IEdit.LineText;
        IEdit.SelStart := IEdit.SelEnd;
        Executer := TInteractiveExecuter(FInteractiveExecuters[I]);
        Executer.Execute(Str);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    TBExecute.Enabled := True;
  end;
end;

procedure TFMessages.MIExpandClick(Sender: TObject);
begin
  case ActiveSubTool of
    2:
      TVAttributes.FullExpand;
    3:
      TVLocalVariables.FullExpand;
    4:
      TVWatchedExpressions.FullExpand;
    6:
      TVSearch.FullExpand;
  end;
end;

procedure TFMessages.MICollapseClick(Sender: TObject);
begin
  case ActiveSubTool of
    2:
      TVAttributes.FullCollapse;
    3:
      TVLocalVariables.FullCollapse;
    4:
      TVWatchedExpressions.FullCollapse;
    6:
      TVSearch.FullCollapse;
  end;
end;

function TFMessages.GetSelectedLines(ListBox: TListBox): string;
begin
  var
  Str := '';
  for var I := 0 to ListBox.Count - 1 do
    if ListBox.Selected[I] then
      Str := Str + ListBox.Items[I] + #13#10;
  Result := Str;
end;

function TFMessages.CopyTreeView(TreeView: TTreeView; All: Boolean): string;
begin
  Result := '';
  for var I := 0 to TreeView.Items.Count - 1 do
    if All or TreeView.Items[I].Selected then
      if Assigned(TreeView.Items[I].Parent) then
        if TreeView.Items[I].Text <> 'dummy' then
          Result := Result + '  ' + TreeView.Items[I].Text + #13#10
        else
      else
        Result := Result + TreeView.Items[I].Text + #13#10;
end;

function TFMessages.CopyTreeViewAll(TreeView: TTreeView): string;
begin
  Result := CopyTreeView(TreeView, True);
end;

function TFMessages.CopyTreeViewSelected(TreeView: TTreeView): string;
begin
  Result := CopyTreeView(TreeView, False);
end;

procedure TFMessages.MICopyClick(Sender: TObject);
begin
  case ActiveSubTool of
    0:
      MInterpreter.CopyToClipboard;
    1:
      Clipboard.AsText := GetSelectedLines(LBCompiler);
    2:
      Clipboard.AsText := CopyTreeViewSelected(TVAttributes);
    3:
      Clipboard.AsText := CopyTreeViewSelected(TVLocalVariables);
    4:
      Clipboard.AsText := CopyTreeViewSelected(TVWatchedExpressions);
    5:
      Clipboard.AsText := GetSelectedLines(LBStack);
    6:
      Clipboard.AsText := CopyTreeViewSelected(TVSearch);
    7:
      Clipboard.AsText := GetSelectedLines(LBMessages);
    8:
      Clipboard.AsText := GetInteractive(TabControlMessages.TabIndex).SelText;
    10:
      Clipboard.AsText := GetMemo(TabControlMessages.TabIndex).SelText;
  end;
end;

procedure TFMessages.MICopyAllClick(Sender: TObject);
begin
  case ActiveSubTool of
    0:
      Clipboard.AsText := MInterpreter.Lines.Text;
    1:
      Clipboard.AsText := LBCompiler.Items.Text;
    2:
      Clipboard.AsText := CopyTreeViewAll(TVAttributes);
    3:
      Clipboard.AsText := CopyTreeViewAll(TVLocalVariables);
    4:
      Clipboard.AsText := CopyTreeViewAll(TVWatchedExpressions);
    5:
      Clipboard.AsText := LBStack.Items.Text;
    6:
      Clipboard.AsText := CopyTreeViewAll(TVSearch);
    7:
      Clipboard.AsText := LBMessages.Items.Text;
    8:
      Clipboard.AsText := GetInteractive(TabControlMessages.TabIndex).Text;
    10:
      Clipboard.AsText := GetMemo(TabControlMessages.TabIndex).Text;
  end;
end;

procedure TFMessages.MICloseClick(Sender: TObject);
begin
  Close;
end;

function getFullName(Node: TTreeNode): string;
var
  AktNode: TTreeNode;
  Str, Variable: string;

  function ExtractVariable(const Str: string): string;
  begin
    var
    Posi := Pos(' = instance of', Str);
    if Posi = 0 then
      Posi := Pos(': instance of', Str);
    if Posi = 0 then
      Posi := Pos(' = {', Str);
    if Posi = 0 then
      Result := Str
    else
      Result := Copy(Str, 1, Posi - 1);
  end;

begin
  AktNode := Node;
  Str := ExtractVariable(Node.Text);
  while Assigned(AktNode.Parent) do
  begin
    AktNode := AktNode.Parent;
    Variable := ExtractVariable(AktNode.Text);
    if Pos(Variable, Str) <> 1 then
      Str := Variable + '.' + Str;
  end;
  Result := Str;
end;

procedure TFMessages.Expand(Node: TTreeNode; Chapter: Integer);
begin
  var
  TotalVariable := getFullName(Node);
  FExpanded[Chapter].Add(TotalVariable);
  MyDebugger.NewCommand(Chapter + 6, 'dump ' + TotalVariable);
  Node.DeleteChildren;
end;

procedure TFMessages.Collapse(Node: TTreeNode; Chapter: Integer);
begin
  var
  TotalVariable := getFullName(Node);
  for var I := FExpanded[Chapter].Count - 1 downto 0 do
    if Pos(TotalVariable, FExpanded[Chapter].Strings[I]) = 1 then
      FExpanded[Chapter].Delete(I);
end;

procedure TFMessages.TVAttributesExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if FDumpActive then
    Exit;
  AllowExpansion := False;
  Expand(Node, 1);
end;

procedure TFMessages.TVAttributesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    TreeViewDelete(TVAttributes, False);
end;

procedure TFMessages.TVLocalVariablesExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if FDumpActive then
    Exit;
  AllowExpansion := False;
  Expand(Node, 2);
end;

procedure TFMessages.TVLocalVariablesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    TreeViewDelete(TVLocalVariables, False);
end;

procedure TFMessages.TVWatchedExpressionsExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if FDumpActive then
    Exit;
  AllowExpansion := False;
  Expand(Node, 3);
end;

procedure TFMessages.TVAttributesCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
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
begin
  TVWatchedExpressions.Items.Clear;
  for var I := 0 to FWatches.LBWatches.Items.Count - 1 do
  begin
    var Str := FWatches.LBWatches.Items[I];
    var Node := TVWatchedExpressions.Items.Add(nil, Str);
    if Pos('instance of', Str) > 0 then
      TVWatchedExpressions.Items.AddChild(Node, Str);
  end;
end;

procedure TFMessages.SplitterInteractiveRightMoved(Sender: TObject);
begin
  AdjustVariablesWidths(TabControlMessages.TabIndex);
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
  MyDebugger.SwitchDetails;
end;

procedure TFMessages.TVSearchDblClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TVSearch.Selected;
  if Assigned(Node) then
    myGrepResults.OpenSource(Node);
end;

procedure TFMessages.TreeViewDeleteSelected(TreeView: TTreeView);
var
  Node: TTreeNode;
begin
  with TreeView do
  begin
    Node := Selected;
    if Assigned(Node) and not Assigned(Node.Parent) then
    begin
      var
      Str := Node.Text;
      var
      Posi := Pos(' = ', Str);
      if Posi > 0 then
        Delete(Str, Posi, Length(Str));
      Items.Delete(Node);
      FWatches.Delete(Str);
    end;
  end;
  TreeViewDelete(TreeView, False);
end;

procedure TFMessages.TreeViewDeleteAll(TreeView: TTreeView);
begin
  TreeViewDelete(TreeView, True);
  FWatches.DeleteAll;
end;

procedure TFMessages.TreeViewDelete(TreeView: TTreeView; All: Boolean);
var
  Node, LastNode, PrevNode: TTreeNode;
  Results: TSearchResults;
begin
  LastNode := nil;
  TreeView.Items.BeginUpdate;
  Node := TreeView.Items.GetFirstNode;
  while Assigned(Node) do
  begin
    if All or Node.Selected then
      Node.Text := 'Delete';
    if Node.GetNext = nil then
      LastNode := Node;
    Node := Node.GetNext;
  end;
  Node := LastNode;
  while Assigned(Node) do
  begin
    if Node.Text = 'Delete' then
    begin
      if (Node.Parent = nil) and (TreeView = TVSearch) then
      begin
        Results := TSearchResults(Node.Data);
        FreeAndNil(Results);
      end;
      PrevNode := Node.GetPrev;
      Node.Delete;
      Node := PrevNode;
    end
    else
      Node := Node.GetPrev;
  end;
  TreeView.Items.EndUpdate;
end;

procedure TFMessages.TVSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    TreeViewDelete(TVSearch, False);
end;

procedure TFMessages.FormPaint(Sender: TObject);
var
  Msg: TWMMove;
begin
  if FUndocking then
  begin
    FUndocking := False;
    ShowTab(K_Interpreter);
  end;
  try
    // OnMove(Msg);  // exception reported when no valid java interpreter
  except
  end;
end;

procedure TFMessages.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(GetFont);
  FJava.FDFont.Options := [];
  if FJava.FDFont.Execute then
    SetFont(FJava.FDFont.Font);
end;

procedure TFMessages.MIPasteClick(Sender: TObject);
begin
  if TabControlMessages.TabIndex = 2 then
    FJava.MIWatchesClick(Self)
  else
    PasteFromClipboard;
end;

procedure TFMessages.MISameWidthClick(Sender: TObject);
begin
  var
  Width := PMain.Width div 4;
  PDebuggerLeft.Width := Width;
  PDebuggerCenterLeft.Width := Width;
  PDebuggerCenterRight.Width := Width;
  PDebuggerRight.Width := Width;
  Width := ClientWidth div 3;
  PInteractiveLeft.Width := Width;
  PInteractiveMiddle.Width := Width;
  PInteractiveRight.Width := Width;
  for var I := 0 to FInteractiveVariables.Count - 1 do
  begin
    TStringGrid(FInteractiveVariables.Objects[I]).ColWidths[0] := Width div 3;
    TStringGrid(FInteractiveVariables.Objects[I]).ColWidths[1] := Width div 3;
    TStringGrid(FInteractiveVariables.Objects[I]).ColWidths[2] := Width div 3;
  end;
end;

procedure TFMessages.TBJavaResetMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    getComJava.JavaReset
  else
    FJava.Restart;
end;

procedure TFMessages.MInteractiveEnter(Sender: TObject);
begin
  ActiveSubTool := 0;
end;

procedure TFMessages.MInteractiveKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13:
      begin
        if MyDebugger.Running then
          MyDebugger.ToUserProgram(FToJavaConsole)
        else
          getComJava.WriteToJava(FToJavaConsole + #13#10);
        if not MyDebugger.Running and (FToJavaConsole <> '') then
          Key := #0;
        FToJavaConsole := '';
      end;
    #08:
      FToJavaConsole := Copy(FToJavaConsole, 1, Length(FToJavaConsole) - 1);
  else
    FToJavaConsole := FToJavaConsole + Key;
  end;
end;

procedure TFMessages.TBShowUMLClick(Sender: TObject);
var
  UMLForm: TFUMLForm;
begin
  var
  I := TabControlMessages.TabIndex;
  if I > 5 then
  begin
    UMLForm := TFUMLForm(FInteractiveUMLForms.Objects[I - 5]);
    if (I = 5) and not UMLForm.Visible then
      UMLForm.Show
    else
      FJava.SwitchToWindow(UMLForm);
  end;
end;

procedure TFMessages.PMMessagesPopup(Sender: TObject);
var
  Selected, Error: Boolean;
  WinControl: TWinControl;
begin
  if not(Sender is TSpTBXPopupMenu) then
    Exit;
  WinControl := FindVCLWindow((Sender as TSpTBXPopupMenu).PopupPoint);
  if Assigned(WinControl) then
    ActiveSubTool := WinControl.Tag
  else
    ActiveSubTool := -1;

  SetVisibleMI(MICollapse, TabControlMessages.TabIndex in [2, 3]);
  SetVisibleMI(MIExpand, TabControlMessages.TabIndex in [2, 3]);
  SetVisibleMI(MIPaste, ActiveSubTool in [0, 8, 10]);

  SetVisibleMI(MIClose, TabControlMessages.TabIndex <> 2);
  SetVisibleMI(MIDeleteAll, True);
  SetVisibleMI(MICopyAll, True);
  Error := False;
  Selected := False;
  case ActiveSubTool of
    0:
      begin
        Selected := MInterpreter.SelLength > 0;
        Error := (Pos('Exception', MInterpreter.Lines.Text) > 0);
        SetVisibleMI(MIPaste, False);
      end;
    1:
      begin
        Selected := LBCompiler.SelCount > 0;
        Error := (Pos(': error: ', LBCompiler.Items.Text) > 0);
      end;
    2:
      Selected := True;
    3:
      Selected := True;
    4:
      Selected := True;
    5:
      Selected := True;
    6:
      Selected := True;
    7:
      Selected := LBMessages.SelCount > 0;
    8:
      begin
        Selected := FActiveInteractive.SelAvail;
        SetVisibleMI(MIPaste, True);
      end;
    9:
      begin
        SetVisibleMI(MIPaste, False);
        SetVisibleMI(MIDeleteAll, False);
        SetVisibleMI(MICopyAll, False);
      end;
    10:
      begin
        Selected := (GetMemo(TabControlMessages.TabIndex).SelLength > 0);
        SetVisibleMI(MIPaste, True);
      end;
  end;
  SetVisibleMI(MICopy, Selected);
  SetVisibleMI(MIDelete, Selected);
  SetVisibleMI(MIGotoError, Error);
  SetVisibleMI(MIDock, Floating);
  SetVisibleMI(MIUndock, not Floating);
end;

function TFMessages.AddInteractive(UMLForm: TForm; const Path: string)
  : TInteractive;
var
  IEdit: TInteractiveEdit;
  Executer: TInteractiveExecuter;
  Memo: TMemo;
  SGVariables: TStringGrid;
  Options: TGridOptions;
  ComJava: TComJava1;
begin
  IEdit := TInteractiveEdit.Create(Self);
  IEdit.ReadOnly := False; // <---

  SGVariables := TStringGrid.Create(Self);
  SGVariables.Parent := PInteractiveRight;
  SGVariables.Font.Assign(Font);
  SGVariables.Font.Size := FConfiguration.Fontsize;
  SGVariables.Align := alClient;
  SGVariables.ColCount := 3;
  SGVariables.RowCount := 2;
  SGVariables.FixedCols := 0;
  SGVariables.FixedRows := 1;
  SGVariables.Cells[0, 0] := _('Name');
  SGVariables.Cells[1, 0] := _('Type');
  SGVariables.Cells[2, 0] := _(LNGValue);
  Options := SGVariables.Options;
  Include(Options, goColSizing);
  SGVariables.Options := Options;
  SGVariables.DefaultRowHeight := Round(MInterpreter.Font.Size * 1.8);
  SGVariables.Selection := TGridRect(Rect(-1, -1, -1, -1));
  SGVariables.PopupMenu := PMMessages;
  SGVariables.Tag := 9;
  SGVariables.OnMouseWheelDown := StringGrid1MouseWheelDown;
  SGVariables.OnMouseWheelUp := StringGrid1MouseWheelUp;
  ComJava := TComJava1.Create(UMLForm, FInteractiveEditors.Count);
  Executer := TInteractiveExecuter.Create(UMLForm as TFUMLForm, IEdit,
    SGVariables, ComJava);
  Memo := TMemo.Create(Self);
  Memo.Parent := PInteractiveLeft;
  Memo.Align := alClient;
  Memo.PopupMenu := PMMessages;
  Memo.Tag := 10;

  FInteractiveEditors.AddObject(Path, IEdit);
  FInteractiveMemos.AddObject(Path, Memo);
  FInteractiveUMLForms.AddObject(Path, UMLForm);
  FInteractiveVariables.AddObject(Path, SGVariables);
  FInteractiveExecuters.Add(Executer);
  FInteractiveComJavas.Add(ComJava);

  TabControlMessages.Tabs.Add(ExtractFileNameEx(Path));
  ChangeTab(TabControlMessages.Tabs.Count - 1);
  AdjustVariablesWidths(TabControlMessages.Tabs.Count - 1);
  Result := TInteractive.Create(UMLForm, IEdit, Memo, SGVariables,
    Executer, ComJava);
end;

procedure TFMessages.AdjustVariablesWidths(Tab: Integer);
var
  Width: Integer;
  SGrid: TStringGrid;
begin
  if (0 <= Tab - 5) and (Tab - 5 < FInteractiveVariables.Count) then
  begin
    SGrid := TStringGrid(FInteractiveVariables.Objects[Tab - 5]);
    Width := SGrid.Width div 3;
    SGrid.ColWidths[0] := Width - 1;
    SGrid.ColWidths[1] := Width - 1;
    SGrid.ColWidths[2] := Width;
  end;
end;

procedure TFMessages.LBInteractiveMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  for var I := 0 to FInteractiveEditors.Count - 1 do
    if GetInteractive(I + 5) = (Sender as TInteractiveEdit) then
      ChangeTab(I + 5);
  if CanFocus then
    SetFocus;
end;

function TFMessages.GetInteractive(Tab: Integer): TInteractiveEdit;
begin
  if (FInteractiveEditors.Count > Tab - 5) and (Tab - 5 >= 0) then
    Result := TInteractiveEdit(FInteractiveEditors.Objects[Tab - 5])
  else
    Result := nil;
end;

function TFMessages.GetMemo(Tab: Integer): TMemo;
begin
  if (FInteractiveMemos.Count > Tab - 5) and (Tab - 5 >= 0) then
    Result := TMemo(FInteractiveMemos.Objects[Tab - 5])
  else
    Result := nil;
end;

function TFMessages.GetCurrentInteractive: TInteractiveEdit;
begin
  Result := nil;
  if ActiveSubTool = 8 then
  begin
    var
    I := TabControlMessages.TabIndex;
    if I >= 5 then
      Result := TInteractiveEdit(FInteractiveEditors.Objects[I - 5]);
  end;
end;

function TFMessages.InteractiveEditActive: Boolean;
begin
  Result := (ActiveSubTool = 8);
end;

function TFMessages.GetCurrentStringGrid: TStringGrid;
begin
  Result := nil;
  if ActiveSubTool = 8 then
  begin
    var
    I := TabControlMessages.TabIndex;
    if I >= 5 then
      Result := TStringGrid(FInteractiveVariables.Objects[I - 5]);
  end;
end;

procedure TFMessages.Run(const Classpath, Programm, Callparameter: string);
var
  UMLForm: TFUMLForm;
begin
  Visible := True;
  if CanFocus then
    SetFocus;
  if Assigned(FJava.ActiveTDIChild) and (FJava.ActiveTDIChild.FormTag = 2) then
    UMLForm := FJava.ActiveTDIChild as TFUMLForm
  else
    UMLForm := FJava.InteractiveUMLForm;
  (UMLForm.MainModul.Diagram as TRtfdDiagram).CallMain(UnHideBlanks(Classpath),
    Programm, Callparameter);
end;

procedure TFMessages.SetModifiedUMLForm(Tab: Integer);
begin
  if Tab >= 6 then
    TFUMLForm(FInteractiveUMLForms.Objects[Tab - 5]).Modified := True;
end;

procedure TFMessages.DelInteractive(const Path: string);
begin
  var
  I := FInteractiveEditors.IndexOf(Path);
  if I > -1 then
  begin
    var
    AObject := FInteractiveEditors.Objects[I];
    // FreeAndNil(aObject);  ToDo check!
    FInteractiveEditors.Delete(I);
    FInteractiveMemos.Delete(I);
    // FInteractiveUMLForms.Objects[i] . Free;  UMLForm is Part of the GUI and destroyed by the system
    FInteractiveUMLForms.Delete(I);
    AObject := FInteractiveVariables.Objects[I];
    FreeAndNil(AObject);
    FInteractiveVariables.Delete(I);
    FInteractiveExecuters.Delete(I);
    FInteractiveComJavas.Delete(I);
    if FInteractiveEditors.Count > 0 then
      FActiveInteractive := TInteractiveEdit(FInteractiveEditors.Objects[0])
    else
      FActiveInteractive := nil;
    if TabControlMessages.TabIndex = I + 5 then
      ChangeTab(0);
    if TabControlMessages.Tabs.Count > I + 5 then
      TabControlMessages.Tabs.Delete(I + 5);
  end;
end;

procedure TFMessages.RenameInteractive(const FromPath, ToPath: string);
begin
  var
  I := FInteractiveEditors.IndexOf(FromPath);
  if I > -1 then
  begin
    try
      FInteractiveEditors[I] := ToPath;
      FInteractiveMemos[I] := ToPath;
      FInteractiveUMLForms[I] := ToPath;
      TabControlMessages.Tabs[I + 5] := ExtractFileNameEx(ToPath);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  end;
end;

procedure TFMessages.OnMove(var Msg: TWMMove);
begin
  inherited;
  if Floating then
    FPosition := Rect(Left, Top, Width, Height);
end;

procedure TFMessages.SetMinHeight(Min: Integer);
begin
  if not Floating and (FJava.BottomDockPanel.Height < Min) then
  begin
    FJava.BottomDockPanel.Height := Min;
    FJava.MoveHSplitter(Min);
  end;
end;

procedure TFMessages.CutToClipboard;
begin
  case ActiveSubTool of
    0:
      MInterpreter.CutToClipboard;
    8:
      FActiveInteractive.CutToClipboard;
    10:
      GetMemo(TabControlMessages.TabIndex).CutToClipboard;
  end;
end;

procedure TFMessages.PasteFromClipboard;
begin
  case ActiveSubTool of
    0:
      MInterpreter.PasteFromClipboard;
    8:
      FActiveInteractive.PasteFromClipboard;
    10:
      GetMemo(TabControlMessages.TabIndex).PasteFromClipboard;
  end;
end;

procedure TFMessages.CopyToClipboard;
begin
  case ActiveSubTool of
    0:
      MInterpreter.CopyToClipboard;
    8:
      FActiveInteractive.CopyToClipboard;
    10:
      GetMemo(TabControlMessages.TabIndex).CopyToClipboard;
  end;
end;

procedure TFMessages.Undo;
begin
  if ActiveSubTool = 8 then
    FActiveInteractive.Undo;
end;

procedure TFMessages.Redo;
begin
  if ActiveSubTool = 8 then
    FActiveInteractive.Redo;
end;

procedure TFMessages.LBCompilerEnter(Sender: TObject);
begin
  ActiveSubTool := 1;
end;

procedure TFMessages.PDebuggerLeftEnter(Sender: TObject);
begin
  ActiveSubTool := 2;
end;

procedure TFMessages.PDebuggerCenterLeftEnter(Sender: TObject);
begin
  ActiveSubTool := 3;
end;

procedure TFMessages.PDebuggerCenterRightEnter(Sender: TObject);
begin
  ActiveSubTool := 4;
end;

procedure TFMessages.PDebuggerRightEnter(Sender: TObject);
begin
  ActiveSubTool := 5;
end;

procedure TFMessages.TVSearchEnter(Sender: TObject);
begin
  ActiveSubTool := 6;
end;

procedure TFMessages.LBMessagesEnter(Sender: TObject);
begin
  ActiveSubTool := 7;
end;

procedure TFMessages.PInteractiveMiddleEnter(Sender: TObject);
begin
  ActiveSubTool := 8;
end;

procedure TFMessages.PInteractiveRightEnter(Sender: TObject);
begin
  ActiveSubTool := 9;
end;

procedure TFMessages.PInteractiveLeftEnter(Sender: TObject);
begin
  ActiveSubTool := 10;
end;

procedure TFMessages.PMainExit(Sender: TObject);
begin
  ActiveSubTool := -1;
end;

procedure TFMessages.SystemOutPrintln;
begin
  if ActiveSubTool = 8 then
    FActiveInteractive.SystemOutPrintln;
end;

procedure TFMessages.Execute(const Command: string);
var
  Executer: TInteractiveExecuter;
begin
  if TabControlMessages.TabIndex < 5 then
    ShowTab(5);
  Executer := TInteractiveExecuter(FMessages.FInteractiveExecuters
    [TabControlMessages.TabIndex - 5]);
  Executer.Execute(Command);
end;

function TFMessages.NeedsSemicolon(const Command: string): Boolean;
var
  Executer: TInteractiveExecuter;
begin
  if FInteractiveExecuters.Count > 0 then
  begin
    Executer := TInteractiveExecuter(FInteractiveExecuters[0]);
    Result := Executer.NeedsSemicolon(Command);
  end
  else
    Result := False;
end;

function TFMessages.GetCompileError(const Path: string): string;
var
  I, J, K: Integer;
  FileWithPath: string;
begin
  Result := '';
  I := LBCompiler.Items.Count - 1;
  while I > -1 do
  begin
    J := I;
    FileWithPath := GetFileWithPath(LBCompiler, I);
    if FileWithPath = Path then
    begin
      for K := I to J do
        Result := Result + '#13#10' + LBCompiler.Items[K];
      Exit;
    end
    else
      I := I - 1;
  end;
end;

procedure TFMessages.StatusMessage(const Message: string; Status: Integer = 0);
begin
  StatusBar.Tag := Status;
  StatusBar.Panels[0].Text := '  ' + Message;
end;

procedure TFMessages.StringGrid1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with Sender as TStringGrid do
  begin
    // top row + displayed rows must not be larger than the total rows
    if TopRow + VisibleRowCount < RowCount then
      TopRow := TopRow + 1;
  end;
  Handled := True;
end;

procedure TFMessages.StringGrid1MouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  with Sender as TStringGrid do
    if TopRow > FixedRows then
      TopRow := TopRow - 1;
  Handled := True;
end;

procedure TFMessages.StatusBarDrawPanel(PStatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  case PStatusBar.Tag of
    0:
      PStatusBar.Canvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace);
    1:
      begin
        PStatusBar.Canvas.Font.Color := clBlack;
        PStatusBar.Canvas.Brush.Color := clLime;
      end;
    2:
      begin
        PStatusBar.Canvas.Font.Color := clWhite;
        PStatusBar.Canvas.Brush.Color := clRed;
      end;
  end;
  var
  ARect := Rect;
  ARect.Width := ClientWidth;
  ARect.Height := StatusBar.Height;
  PStatusBar.Canvas.FillRect(ARect);
  PStatusBar.Canvas.Font.Assign(PStatusBar.Font);
  var
  Height := PStatusBar.Canvas.TextHeight('A');
  PStatusBar.Canvas.TextOut(Rect.Left, (StatusBar.Height - Height) div 2 + 1,
    Panel.Text);
end;

procedure TFMessages.ClearStack;
begin
  LBStack.Clear;
end;

procedure TFMessages.ChangeStyle;
var
  Details: TThemedElementDetails;
begin
  if FConfiguration.isDark then
  begin
    DebuggerToolbar.Images := vilDebuggerToolbarDark;
    TBInteractiveToolbar.Images := vilInteractiveDark;
    PMMessages.Images := vilPMMessagesDark;
  end
  else
  begin
    DebuggerToolbar.Images := vilDebuggerToolbarLight;
    TBInteractiveToolbar.Images := vilInteractiveLight;
    PMMessages.Images := vilPMMessagesLight;
  end;
  if StyleServices.IsSystemStyle then
  begin
    FBGColor := clWhite;
    FFGColor := clBlack;
  end
  else
  begin
    Details := StyleServices.GetElementDetails(tbsBackground);
    StyleServices.GetElementColor(Details, ecFillColor, FBGColor);
    FFGColor := StyleServices.GetStyleFontColor(sfTabTextInactiveNormal);
  end;
  MInterpreter.Color := FBGColor;
  MInterpreter.Font.Color := FFGColor;
  for var I := 0 to FInteractiveEditors.Count - 1 do
    TInteractiveEdit(FInteractiveEditors.Objects[I]).Loaded;
end;

procedure TFMessages.SetActiveSubTool(Value: Integer);
begin
  FActiveSubTool := Value;
end;

procedure TFMessages.DeleteDebuggingTreeViews;
begin
  TreeViewDelete(TVAttributes, True);
  TreeViewDelete(TVLocalVariables, True);
  LBStack.Clear;
end;

procedure TFMessages.DPIChanged;
begin
  Hide;
  Show;
  SetStatusBarAndTabs;
  MISameWidthClick(Self);
end;

procedure TFMessages.SetDumpActive(Value: Boolean);
begin
  FDumpActive:= Value;
end;


end.
