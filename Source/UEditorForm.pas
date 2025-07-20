unit UEditorForm;

interface

uses
  Windows,
  Winapi.Messages,
  Winapi.D2D1,
  SynEditPrint,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Menus,
  ComCtrls,
  ExtCtrls,
  System.ImageList,
  Vcl.ToolWin,
  Vcl.ImgList,
  Vcl.BaseImageCollection,
  Vcl.VirtualImageList,
  Vcl.WinXCtrls,
  SVGIconImageCollection,
  TB2Item,
  SpTBXItem,
  SynEdit,
  SynEditExport,
  SynEditHighlighter,
  SynEditMiscClasses,
  USynEditEx,
  UModel,
  UBaseForm,
  UJavaParser,
  UParseThread;

// Application FFrameType:= 8
// JApplet     FFrameType:= 7
// JDialog     FFrameType:= 6
// JFrame      FFrameType:= 5
// Applet      FFrameType:= 4
// Dialog      FFrameType:= 3
// Frame       FFrameType:= 2
// Console     FFrameType:= 1

const
  CInsertBlink = 500;

  EcWordLeft = 5; // Move cursor left one word
  EcWordRight = 6; // Move cursor right one word

type
  TLineInfo = (dlCurrentDebuggerLine, dlBreakpointLine, dlExecutableLine,
    dlSearchLine);
  TLineInfos = set of TLineInfo;

  { TFEditForm }

  TFEditForm = class(TFForm)
    PMain: TPanel;
    TVFileStructure: TTreeView;
    StatusBar: TStatusBar;
    BottomPanel: TPanel;
    DesignButton: TButton;
    EditformToolbar: TToolBar;
    TBClose: TToolButton;
    TBExplorer: TToolButton;
    TBBrowser: TToolButton;
    TBDesignform: TToolButton;
    TBStructure: TToolButton;
    TBClassOpen: TToolButton;
    TBMatchBracket: TToolButton;
    TBSystemOutPrintln: TToolButton;
    TBStructureIndent: TToolButton;
    TBIfStatement: TToolButton;
    TBIfElseStatement: TToolButton;
    TBWhileStatement: TToolButton;
    TBForStatement: TToolButton;
    TBDoWhileStatement: TToolButton;
    TBSwitchStatement: TToolButton;
    TBTryStatement: TToolButton;
    TBBlockStatement: TToolButton;
    TBComment: TToolButton;
    TBIndent: TToolButton;
    TBUnindent: TToolButton;
    TBWordWrap: TToolButton;
    TBBreakpoint: TToolButton;
    TBBreakpointsClear: TToolButton;
    TBBookmark: TToolButton;
    TBGotoBookmark: TToolButton;
    TBParagraph: TToolButton;
    TBNumbers: TToolButton;
    TBZoomIn: TToolButton;
    TBZoomOut: TToolButton;
    TBValidate: TToolButton;
    icEditor: TSVGIconImageCollection;
    vilEditorToolbarLight: TVirtualImageList;
    vilEditorToolbarDark: TVirtualImageList;
    icContextMenu: TSVGIconImageCollection;
    vilContextMenuLight: TVirtualImageList;
    vilContextMenuDark: TVirtualImageList;
    PopUpEditor: TSpTBXPopupMenu;
    MISearchDeclaration: TSpTBXItem;
    MIAPIHelp: TSpTBXItem;
    MIClassOpen: TSpTBXItem;
    MIClassEditor: TSpTBXItem;
    MICreateStructogram: TSpTBXItem;
    MILine0: TSpTBXSeparatorItem;
    MIExecute: TSpTBXItem;
    MIExecuteWithoutConsole: TSpTBXItem;
    MIExecuteWithConsole: TSpTBXItem;
    MILine5: TSpTBXSeparatorItem;
    MIGit: TSpTBXSubmenuItem;
    MIGitStatus: TSpTBXItem;
    MIGitAdd: TSpTBXItem;
    MIGitCommit: TSpTBXItem;
    MIGitLog: TSpTBXItem;
    MIGitLine1: TSpTBXSeparatorItem;
    MIGitReset: TSpTBXItem;
    MIGitCheckout: TSpTBXItem;
    MIGitRemove: TSpTBXItem;
    MIGitLine2: TSpTBXSeparatorItem;
    MIGitRemote: TSpTBXItem;
    MIGitFetch: TSpTBXItem;
    MIGitPush: TSpTBXItem;
    MIGitLine3: TSpTBXSeparatorItem;
    MIGitGui: TSpTBXItem;
    MIGitViewer: TSpTBXItem;
    MIGitConsole: TSpTBXItem;
    MIRenewImports: TSpTBXItem;
    MICopyPath: TSpTBXItem;
    MILine1: TSpTBXSeparatorItem;
    MIUndo: TSpTBXItem;
    MIRedo: TSpTBXItem;
    MILine2: TSpTBXSeparatorItem;
    MICut: TSpTBXItem;
    MICopy: TSpTBXItem;
    MIInsert: TSpTBXItem;
    MILine3: TSpTBXSeparatorItem;
    MIUnindent: TSpTBXItem;
    MIIndent: TSpTBXItem;
    MILine4: TSpTBXSeparatorItem;
    MIFont: TSpTBXItem;
    MIConfiguration: TSpTBXItem;
    MIClose: TSpTBXItem;
    vilBookmarksLight: TVirtualImageList;
    vilBookmarksDark: TVirtualImageList;
    MIAssistant: TSpTBXSubmenuItem;
    mnAssistantCancel: TSpTBXItem;
    mnAssistantOptimize: TSpTBXItem;
    mnAssistantFixBugs: TSpTBXItem;
    mnAssistantExplain: TSpTBXItem;
    mnAssistanSuggest: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    ActivityIndicator: TActivityIndicator;
    icBookmarks: TSVGIconImageCollection;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction); override;
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure Enter(Sender: TObject); override;
    procedure UpdateState; override;
    procedure BreakpointGutterClick(Sender: TObject; Button: TMouseButton;
      X, Y, Row, Line: Integer);
    procedure MICutClick(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure MIInsertClick(Sender: TObject);
    procedure MIUnindentClick(Sender: TObject);
    procedure MIIndentClick(Sender: TObject);
    procedure MIUndoClick(Sender: TObject);
    procedure MIRedoClick(Sender: TObject);
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MICloseClick(Sender: TObject);
    procedure MIAPIHelpClick(Sender: TObject);
    procedure EditorPaintTransient(Sender: TObject; ACanvas: TCanvas;
      TransientType: TTransientType);
    procedure SBParagraphClick(Sender: TObject);
    procedure SBNumbersClick(Sender: TObject);
    procedure SBMatchBracketClick(Sender: TObject);
    procedure SBBookmarkClick(Sender: TObject);
    procedure SBGotoBookmarkClick(Sender: TObject);
    procedure SBStatementClick(Sender: TObject);
    procedure SBIndentClick(Sender: TObject);
    procedure SBUnindentClick(Sender: TObject);
    procedure SBBreakpointClick(Sender: TObject);
    procedure SBBreakpointsClearClick(Sender: TObject);
    procedure SBCommentClick(Sender: TObject);
    procedure SBDesignformClick(Sender: TObject);
    procedure SBClassEditClick(Sender: TObject);
    procedure SBClassOpenClick(Sender: TObject);
    procedure SBBrowserClick(Sender: TObject);
    procedure SBExplorerClick(Sender: TObject);
    procedure SBCloseClick(Sender: TObject);
    procedure SBStructureIndentClick(Sender: TObject);
    procedure SBWordWrapClick(Sender: TObject);
    procedure SBValidateClick(Sender: TObject);
    procedure SBSystemOutPrintlnClick(Sender: TObject);
    procedure SBZoomOutClick(Sender: TObject);
    procedure SBZoomInClick(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure WMNCButtonDBLClick(var Msg: TMessage); message WM_NCLBUTTONDBLCLK;
    procedure MIExecuteClick(Sender: TObject);
    procedure PopUpEditorPopup(Sender: TObject);
    procedure MIExecuteWithoutConsoleClick(Sender: TObject);
    procedure MIExecuteWithConsoleClick(Sender: TObject);
    procedure MIRenewImportsClick(Sender: TObject);
    procedure MICreateStructogramClick(Sender: TObject);
    procedure MICopyPathClick(Sender: TObject);
    procedure DoOnMouseOverToken(Sender: TObject; const Token: string;
      TokenType: Integer; Caret, Posi: TPoint; Attri: TSynHighlighterAttributes;
      var Highlight: Boolean);
    procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoOnBuildStructure(Sender: TObject);
    procedure TVFileStructureChange(Sender: TObject; Node: TTreeNode);
    procedure MIClassOpenClick(Sender: TObject);
    procedure MIClassEditorClick(Sender: TObject);
    procedure MISearchDeclarationClick(Sender: TObject);
    procedure MIGitStatusClick(Sender: TObject);
    procedure MIGitAddClick(Sender: TObject);
    procedure MIGitCommitClick(Sender: TObject);
    procedure MIGitGuiClick(Sender: TObject);
    procedure MIGitRemoveClick(Sender: TObject);
    procedure MIGitLogClick(Sender: TObject);
    procedure MIGitResetClick(Sender: TObject);
    procedure MIGitCheckoutClick(Sender: TObject);
    procedure MIGitRemoteClick(Sender: TObject);
    procedure MIGitFetchClick(Sender: TObject);
    procedure MIGitPushClick(Sender: TObject);
    procedure MIGitConsoleClick(Sender: TObject);
    procedure MIGitViewerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIConfigurationClick(Sender: TObject);
    procedure DesignButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnAssistanSuggestClick(Sender: TObject);
    procedure mnAssistantExplainClick(Sender: TObject);
    procedure mnAssistantFixBugsClick(Sender: TObject);
    procedure mnAssistantOptimizeClick(Sender: TObject);
    procedure mnAssistantCancelClick(Sender: TObject);
  private
    FBookmark: Integer;
    FBreakPointCount: Integer;
    FMouseIsInBorderOfStructure: Boolean;
    FMouseBorderOfStructure: Integer;
    FMousePosition: TBufferCoord;
    FModifiedStrs: array [Boolean] of string;
    FInsertModeStrs: array [Boolean] of string;
    FToolButtons: array [0 .. 29] of TToolButton;
    FFrameType: Integer; // 1..8, look in UEditorForm

    FCheckAgeEnabled: Boolean;
    FDebuglineMark: TSynEditMark;
    FEditor: TSynEditEx;
    FEditorAge: TDateTime;
    FEncoding: string;
    FFileExtension: string;
    FNeedsParsing: Boolean;
    FNeedToSyncFileStructure: Boolean;
    FIsJUnitTestClass: Boolean;
    FLastToken: string;
    FLineBreak: string;
    FModel: TObjectModel;
    FParameter: string;
    FParser: TJavaParser;
    FParseThread: TParseThread;
    FStartOption: Integer;
    FSynEditPrint: TSynEditPrint;
    FHidden: Boolean;

    procedure Translate;
    procedure Statusline(Num: Integer; const Str: string);
    procedure CalculateStatusline;
    procedure SetNeedsParsing(Value: Boolean);
    function GetJavaCodeAt(Caret: TPoint): string;
    procedure CreateTooltip(Caret, Posi: TPoint; const Token: string);
    function GetFrameType: Integer;
    procedure SynEditDebugInfoPaintLines(RenderTarget: ID2D1RenderTarget;
      ClipR: TRect; const FirstRow, LastRow: Integer;
      var DoDefaultPainting: Boolean);
    procedure SynEditGutterDebugInfoMouseCursor(Sender: TObject;
      X, Y, Row, Line: Integer; var Cursor: TCursor);
    procedure SynEditBookmarkPaintLines(RenderTarget: ID2D1RenderTarget;
      ClipR: TRect; const FirstRow, LastRow: Integer;
      var DoDefaultPainting: Boolean);
    procedure BookmarkGutterClick(Sender: TObject; Button: TMouseButton;
      X, Y, Row, Line: Integer);
    procedure SynEditChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure New(const FileName: string);
    procedure Open(const FileName: string; State: string;
      Hidden: Boolean = False);
    procedure Save(WithBackup: Boolean); override;
    procedure SaveIn(const Dir: string); override;
    procedure SaveAs(const FileName: string);
    function GetSaveAsName: string; override;
    procedure SetHighlighter;
    procedure SetToolButtons;
    procedure PreparePrint;
    procedure Print; override;
    procedure PrintAll(AllPages: Boolean);
    procedure Show;
    procedure Hide;
    procedure SetFont(AFont: TFont); override;
    function GetFont: TFont; override;
    procedure SetFontSize(Delta: Integer); override;
    procedure SetOptions; override;
    procedure SetDeleteBookmark(Xpos, YPos: Integer);
    procedure Unindent;
    procedure Indent;
    procedure Search; override;
    procedure SearchAgain; override;
    procedure Replace; override;
    procedure SystemOutPrintln;
    procedure Matchbracket;
    function GetIndent: string;
    procedure PutText(Str: string; WithCursor: Boolean = True);

    function CBSearchClassOrMethod(Stop: Boolean; Line: Integer): string;
    function SourceContainsClass(const AClassname: string): Boolean;
    procedure GotoLine(Line: Integer);
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure SetModified(AModified: Boolean); override;
    function GetModified: Boolean; override;
    procedure DoExport; override;

    // Test-Menü
    procedure ClearBreakpoints;
    procedure SetBreakpoints;
    function HasBreakpoints: Boolean;
    function HasBreakpoint(Line: Integer): Boolean;
    function IsExecutableLine(Line: Integer): Boolean;
    procedure SetDebuglineMark(Line: Integer);
    procedure DeleteDebuglineMark;
    procedure DeleteBreakpoint(Str: string);
    function GetBreakpointMark(Line: Integer): TSynEditMark;

    function GetMarksBreakpoints: string;
    procedure SetMarksBreakpoints(MarkBreakpoint: string);
    procedure InsertBreakpointMark(Line: Integer);
    procedure DeleteBreakpointAtLine(Line: Integer);
    procedure InsertGotoCursorBreakpoint;
    procedure InsertBreakpoint;
    procedure HTMLforApplet(const AWidth, AHeight, CharSet, Path,
      AClass: string; WithJEApplets, Debug: Boolean);
    function CurrentCol: Integer;
    function CurrentRow: Integer;
    procedure ExportToFile(const FileName: string;
      Exporter: TSynCustomExporter);
    procedure ExportToClipboard(AsHtml: Boolean; AsText: Boolean);
    procedure ExportWithNumbers;
    procedure ExportRTFNumbered;
    procedure SynEditorReplaceText(Sender: TObject;
      const ASearch, AReplace: string; Line, Column: Integer;
      var AAction: TSynReplaceAction);
    procedure SynEditorSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var Foreground, Background: TColor);
    procedure SynEditGutterGetText(Sender: TObject; ALine: Integer;
      var AText: string);
    function GetLineInfos(ALine: Integer): TLineInfos;
    procedure ParseSourcecodeWithThread(HasChanged: Boolean);
    procedure ParseSourceCode(HasChanged: Boolean);
    procedure CreateTVFileStructure;
    procedure RunTests;

    function GetWidthAndHeight: TPoint;
    procedure ChangeWidthAndHeight(Width, Height: Integer);
    procedure ReplaceWidthHeight(Width, Height: Integer);

    function GetLNGStartAttributes: Integer;
    function GetLNGEndAttributes: Integer;
    function GetLNGStartComponents: Integer;
    function GetLNGEndComponents: Integer;
    function GetLNGStartEventMethods: Integer;
    function GetLNGEndEventMethods: Integer;
    function GetLNG(Num, ClassNumber: Integer): string;
    procedure EnsureStartEnd(CorI: Integer = 1);
    procedure InsertStartEnd;
    function HasStartAndEnd(CorI: Integer): Boolean;

    function GetLineNumberWith(const Str: string): Integer;
    function GetLineNumberWithFrom(From: Integer; const Str: string): Integer;
    function GetLineNumberWithFromTill(From, Till: Integer;
      const Str: string): Integer;
    function GetLineNumberWithWord(const Str: string): Integer;
    function GetLineNumberWithWordFrom(From: Integer;
      const Str: string): Integer;
    function GetLineNumberWithStartsWordFrom(From: Integer;
      const Str: string): Integer;
    function GetSource(Lines, LineE: Integer): string;
    function GetLine(Line: Integer): string;
    function ContainsWord(const Key: string; Line: Integer): Boolean;

    procedure ReplaceLine(const Str1, Str2: string);
    procedure ReplaceLineWith(Line: Integer; const Str: string);
    procedure ReplaceLineInLine(Line: Integer; const Old, ANew: string);
    procedure ReplaceText(const Str1, Str2: string; All: Boolean);
    procedure ReplaceTextWithRegex(const Reg, Str: string; All: Boolean;
      From: Integer = -1; Till: Integer = -1);
    procedure ReplaceWord(const Str1, Str2: string; All: Boolean);
    procedure ReplaceComponentname(const Str1, Str2: string; Events: string);
    procedure ReplaceAttributAt(const AtLine, Key, Str: string);
    procedure ReplaceAttribute(const Key, Str: string);
    procedure ReplaceMethod(var Method: TOperation; const New: string);
    procedure SetAttributValue(const Container, Key, Str: string;
      After: Integer);
    procedure ChangeAttributValue(const Key, Str: string); overload; // Swing
    procedure ChangeAttributValue(const Container, Key, Str: string); overload;
    // IsFX

    function HasComponent(const Key: string; Line: Integer): Boolean;
    procedure InsertAttributValue(const Destination, Str: string;
      After: Integer);
    procedure InsertLinesAt(Line: Integer; Str: string); overload;
    procedure InsertLinesAt(const AtLine, Str: string); overload;
    procedure InsertAttributAfter(const AtLine, Attribute: string);
    procedure InsertAttribute(ClassNumber: Integer; const Str: string);
      overload;
    procedure InsertAttribute(const Container, AIndent, Variable: string;
      IsFX: Boolean); overload;
    procedure InsertAttribute(const Container, Variable: string;
      IsFX: Boolean); overload;

    procedure InsertComponent(const Str: string);
    procedure InsertProcedure(const AProcedure: string); overload;
    procedure InsertProcedure(ClassNumber: Integer;
      const AProcedure: string); overload;
    procedure InsertListener(const Component, Listener: string);
    procedure InsertConstructor(ClassNumber: Integer; const AProcedure: string);
    procedure InsertImport(const Package: string);

    procedure DeleteAttribute(const Str: string);
    function DeleteAttributeValue(const Str: string): Boolean;
    procedure DeleteComponentValue(Str: string);
    procedure DeleteAttributeValues(const Str: string);
    procedure DeleteEmptyLines(Line: Integer);
    procedure DeleteLine(Line: Integer);
    procedure DeleteBlock(StartLine, EndLine: Integer);
    procedure DeleteComponent(const Component: string);
    procedure DeleteComponentDefault(Control: TControl);
    procedure DeleteComponentTotal(ClassNumber: Integer;
      const Component, Typ: string);
    procedure DeleteMethod(const Method: string;
      SourcecodeCheck: Boolean = True); overload;
    procedure DeleteMethod(Method: TOperation); overload;
    procedure DeleteEventMethod(Method: string);
    procedure DeleteListener(Listener: string);
    procedure DeleteFXListener(Listener: string);
    procedure DeleteLambdaListener(Listener: string);
    procedure DeleteOldAddNewMethods(OldMethods, NewMethods: TStringList);
    procedure DeleteFXOldAddNewMethods(OldMethods, NewMethods: TStringList);
    procedure DeleteTryCatch(const Key: string);

    procedure MoveBlock(From, Till, Dest, DestTill: Integer;
      const Blanklines: string);
    function GetBlock(From, Lines: Integer): string;
    procedure ToForeground(Control: TControl);
    procedure ToBackground(Control: TControl);

    procedure Go_To(const Str: string);
    function GetClassAttribut(const AClass, AAttribute: string): string;

    function HasText(const Str: string): Boolean;
    function HasWord(const Str: string): Boolean;
    function HasEventProcedureInModel(const AMethodname: string): Boolean;
    function HasMainInModel: Boolean;

    function IsJava: Boolean;
    function IsPascal: Boolean;
    function IsHTML: Boolean;
    function IsCSS: Boolean;
    function IsHTMLApplet: Boolean;

    function GetFormType: string; override;
    function GetState: string; override;
    procedure SetState(var Str: string); override;
    function EncodingAsString(const AEncoding: string): string;
    function LinebreakAsString: string;
    function LinebreakAsCtrls(const Str: string): string;
    function GetEncodingAsType: TEncoding;
    procedure SetEncoding(AEncoding: string);
    procedure AutomatedCompleteImports;
    procedure SetNewActionEventFormat;
    procedure CollectClasses(StringList: TStringList); override;
    function GetPackage: string;
    procedure CheckAge;
    procedure AddShortcutsToHints;
    function GetAllPathnames: TStringList; override;
    function GetAllClassnames: TStringList; override;
    function ClassnameDifferentFromAncestors(const AClassname: string): Boolean;
    procedure InitShowCompileErrors;
    procedure SetErrorMark(Line, Column: Integer; const Error: string);
    procedure UnderlineCompileErrors;
    procedure ClearCompilerErrorMarks;
    procedure ClearMarks;
    procedure TerminateThread(Sender: TObject);
    procedure ChangeStyle; override;
    procedure RemoveShortCutFromEditor(ShortCut: Integer);
    procedure ReplaceShortCutFromEditor(ShortCut, ShortCut2: Integer);
    procedure EditShortCuts;
    procedure CollapseGUICreation;
    procedure DoOnIdle;
    procedure SyncFileStructure;
    function MakeUpperEvents(Events: string): string;
    procedure SetFXBackgroundAsString(const Container, AName, AColor: string);
    procedure DPIChanged; override;
    function CountClassOrInterface: Integer;
    procedure SetActivityIndicator(TurnOn: Boolean; Hint: string = '';
      OnClick: TNotifyEvent = nil);
    procedure ShowAssistantError(Msg: string);
    function IsApplet: Boolean;
    function IsAWT: Boolean;
    function FrameTypToString: string;

    property CheckAgeEnabled: Boolean read FCheckAgeEnabled
      write FCheckAgeEnabled;
    property DebuglineMark: TSynEditMark read FDebuglineMark;
    property Editor: TSynEditEx read FEditor;
    property EditorAge: TDateTime read FEditorAge write FEditorAge;
    property Encoding: string read FEncoding;
    property FileExtension: string read FFileExtension;
    property IsJUnitTestClass: Boolean read FIsJUnitTestClass;
    property NeedsParsing: Boolean read FNeedsParsing write SetNeedsParsing;
    property FrameType: Integer read GetFrameType write FFrameType;
    property LastToken: string read FLastToken write FLastToken;
    property LineBreak: string read FLineBreak;
    property Model: TObjectModel read FModel;
    property Parameter: string read FParameter write FParameter;
    property Parser: TJavaParser read FParser write FParser;
    property ParseThread: TParseThread read FParseThread;
    property SynEditPrint: TSynEditPrint read FSynEditPrint;
  end;

implementation

{$R *.dfm}

uses
  Printers,
  Dialogs,
  IOUtils,
  Types,
  UITypes,
  Clipbrd,
  Math,
  StrUtils,
  RegularExpressions,
  JvGnugettext,
  SynExportRTF,
  SynExportHTML,
  SynEditPrintTypes,
  SynEditTypes,
  SynHighlighterJava,
  SynDWrite,
  UJava,
  UGUIDesigner,
  UJavaCommands,
  UUtils,
  UConfiguration,
  UTree,
  UModelEntity,
  UFileProvider,
  UMessages,
  UGUIForm,
  UCodeCompletion,
  UDlgConfirmReplace,
  UUMLForm,
  UStringRessources,
  UDebugger,
  UFileStructure,
  UFXComponents,
  UGit,
  UJUnitTest,
  USequenceForm,
  UJavaScanner,
  UTooltip,
  UObjectInspector,
  UJEComponents,
  UTemplates,
  ULLMSupport;

const
  EcMatchBracket = 250; // Go to matching bracket
  EcBlockIndent = 610; // Indent selection
  EcBlockUnindent = 611; // Unindent selection
  NoBreakpointImageIndex = 14;
  BreakpointImageIndex = 13;
  ErrorMarkIndex = 10;
  StopInAt = True;

var
  LNGs: array [1 .. 6] of string;

  { --- TFEditForm --------------------------------------------------------------- }

  { Gutterparts
    MarksPart
    LineNumberPart
    ChangesPart
    SeparatorPart
    CodeFoldPart
    LineOverviewPart

    Gutter.LeftOffset
  }

constructor TFEditForm.Create(AOwner: TComponent);
begin
  inherited;
  FormTag := 1;
end;

procedure TFEditForm.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FEditor := TSynEditEx.Create(Self);
  with FEditor do
  begin
    MaxUndo := 300;
    TabWidth := 2;
    WantTabs := True;
    PopupMenu := PopUpEditor;
    BookMarkOptions.BookmarkImages := vilBookmarksLight;
    Font.Assign(FConfiguration.EditFont);
    Font.Quality:= fqClearTypeNatural;
    if not FConfiguration.ShowControlFlowSymbols then
      DisplayFlowControl.Enabled := False;

    Options := [eoAutoIndent,  eoSmartTabs, eoTabIndent,
      eoTabsToSpaces, eoSmartTabDelete, eoGroupUndo, eoDropFiles, eoKeepCaretX,
      eoBracketsHighlight, eoAccessibility, eoCompleteBrackets,
      eoCompleteQuotes, eoEnhanceHomeKey];
    if FConfiguration.ShowLigatures then
      Options:= Options + [eoShowLigatures];
    ScrollOptions := [eoDisableScrollArrows, eoScrollPastEol, eoShowScrollHint];

    Gutter.AutoSize := True;
    Gutter.BorderStyle := gbsNone;
    Gutter.DigitCount := 1;
    Gutter.Font.Assign(FConfiguration.Font);
    Gutter.Font.Height := FEditor.Font.Height + 2;
    Gutter.Font.Quality := fqClearTypeNatural;
    Gutter.BorderStyle := gbsNone;
    //Gutter.Gradient := True;         // ToDo as option
    //Gutter.GradientSteps := 30;
    Gutter.ShowLineNumbers := FConfiguration.LineNumbering;
    Gutter.TrackChanges.Width := 2;
    Gutter.TrackChanges.Visible := True;
    FJava.ThemeEditorGutter(Gutter);

    var
    Band := TSynGutterBand(Gutter.Bands.Insert(1));
    Band.Kind := gbkCustom;
    Band.Width := 17;
    Band.OnPaintLines := SynEditDebugInfoPaintLines;
    Band.OnClick := BreakpointGutterClick;
    Band.OnMouseCursor := SynEditGutterDebugInfoMouseCursor;

    Band := Gutter.Bands[0];
    Band.Width := 16;
    Band.OnPaintLines := SynEditBookmarkPaintLines;
    Band.OnClick := BookmarkGutterClick;
    Band.OnMouseCursor := SynEditGutterDebugInfoMouseCursor;

    IndentGuides.Style := igsDotted;
    SelectedColor.Background := clSkyBlue;
    UseCodeFolding := True;
    WantTabs := True;
    WordWrapGlyph.Visible := True;
    SearchEngine := FJava.SynEditSearch;
    Indent := FConfiguration.Indent;
    StructureColorIntensity := FConfiguration.StructureColorIntensity;
    SetCaretBlinkTime(CInsertBlink);
    ScrollbarAnnotations.SetDefaultAnnotations;

    OnBuildStructure := DoOnBuildStructure;
    OnGutterGetText := SynEditGutterGetText;
    OnSpecialLineColors := SynEditorSpecialLineColors;
    OnKeyUp := EditorKeyUp;
    OnKeyPress := EditorKeyPress;
    OnChange := SynEditChange;
    OnStatusChange := EditorStatusChange;
    OnReplaceText := SynEditorReplaceText;
    OnMouseOverToken := DoOnMouseOverToken;
    OnMouseDown := DoOnMouseDown;
    if FConfiguration.EightyColumnLine then
      RightEdge := 80
    else
      RightEdge := 0;
  end;

  FEditor.Parent := PMain;
  FEditor.Align := alClient;
  ActivityIndicator.Parent := FEditor;

  EditformToolbar.Visible := FConfiguration.VisToolbars[2];
  FEncoding := FConfiguration.GetEncoding;
  FCheckAgeEnabled := FConfiguration.CheckAge;
  FNeedsParsing := False;
  FBookmark := 0;
  FBreakPointCount := 0;
  FEditorAge := 0;
  FModel := TObjectModel.Create;
  Partner := nil;
  FParser := nil;
  FLineBreak := #13#10;
  FFrameType := 0;
  FHidden := False;
  CalculateStatusline;
  EditorStatusChange(Sender, [scAll]);
  OnMouseActivate := FormMouseActivate;
  SetOptions;
  ToMainPanel;
  FToolButtons[0] := TBClose;
  FToolButtons[1] := TBExplorer;
  FToolButtons[2] := TBBrowser;
  FToolButtons[3] := TBDesignform;
  FToolButtons[4] := TBStructure;
  FToolButtons[5] := TBClassOpen;
  FToolButtons[6] := TBMatchBracket;
  FToolButtons[7] := TBSystemOutPrintln;
  FToolButtons[8] := TBStructureIndent;
  FToolButtons[9] := TBIfStatement;
  FToolButtons[10] := TBIfElseStatement;
  FToolButtons[11] := TBWhileStatement;
  FToolButtons[12] := TBForStatement;
  FToolButtons[13] := TBDoWhileStatement;
  FToolButtons[14] := TBSwitchStatement;
  FToolButtons[15] := TBTryStatement;
  FToolButtons[16] := TBBlockStatement;

  FToolButtons[17] := TBComment;
  FToolButtons[18] := TBIndent;
  FToolButtons[19] := TBUnindent;
  FToolButtons[20] := TBWordWrap;
  FToolButtons[21] := TBBreakpoint;
  FToolButtons[22] := TBBreakpointsClear;
  FToolButtons[23] := TBBookmark;
  FToolButtons[24] := TBGotoBookmark;
  FToolButtons[25] := TBParagraph;
  FToolButtons[26] := TBNumbers;
  FToolButtons[27] := TBZoomOut;
  FToolButtons[28] := TBZoomIn;
  FToolButtons[29] := TBValidate;
  Translate;
end;

procedure TFEditForm.SynEditGutterGetText(Sender: TObject; ALine: Integer;
  var AText: string);
begin
  if ALine = TSynEdit(Sender).CaretY then
    Exit;

  if ALine mod 10 <> 0 then
    if ALine mod 5 <> 0 then
      AText := '·'
    else
      AText := '-';
end;

procedure TFEditForm.AddShortcutsToHints;
begin
  var
  Str := ShortCutToText(FJava.MIIndent.ShortCut);
  if Pos(Str, TBIndent.Hint) = 0 then
    TBIndent.Hint := TBIndent.Hint + ' - ' + Str;
  Str := ShortCutToText(FJava.MIUnindent.ShortCut);
  if Pos(Str, TBUnindent.Hint) = 0 then
    TBUnindent.Hint := TBUnindent.Hint + ' - ' + Str;
  Str := ShortCutToText(FJava.MIStructuredIndent.ShortCut);
  if Pos(Str, TBStructureIndent.Hint) = 0 then
    TBStructureIndent.Hint := TBStructureIndent.Hint + ' - ' + Str;
  Str := ShortCutToText(FJava.MICommentOnOff.ShortCut);
  if Pos(Str, TBComment.Hint) = 0 then
    TBComment.Hint := TBComment.Hint + ' - ' + Str;
  Str := ShortCutToText(FJava.MISystemOutPrintln.ShortCut);
  if Pos(Str, TBSystemOutPrintln.Hint) = 0 then
    TBSystemOutPrintln.Hint := TBSystemOutPrintln.Hint + ' - ' + Str;
  if Pos(Str, TBBookmark.Hint) = 0 then
    TBBookmark.Hint := TBBookmark.Hint + ' - ' + Str;
  if Pos(Str, TBGotoBookmark.Hint) = 0 then
    TBGotoBookmark.Hint := TBGotoBookmark.Hint + ' - ' + Str;
end;

procedure TFEditForm.Translate;
begin
  TBIfStatement.Hint := 'if ' + _(LNGStatement);
  TBIfElseStatement.Hint := 'if-else ' + _(LNGStatement);
  TBWhileStatement.Hint := 'while ' + _(LNGStatement);
  TBForStatement.Hint := 'for ' + _(LNGStatement);
  TBDoWhileStatement.Hint := 'do-while ' + _(LNGStatement);
  TBSwitchStatement.Hint := 'switch ' + _(LNGStatement);
  TBTryStatement.Hint := 'try ' + _(LNGStatement);
  TBBlockStatement.Hint := 'block ' + _(LNGStatement);
  LNGs[1] := _(LNGStartGUIVariables);
  LNGs[2] := _(LNGEndGUIVariables);
  LNGs[3] := _(LNGStartComponents);
  LNGs[4] := _(LNGEndComponents);
  LNGs[5] := _(LNGStartEventMethods);
  LNGs[6] := _(LNGEndEventMethods);

  FModifiedStrs[False] := '';
  FModifiedStrs[True] := _(LNGModified);
  FInsertModeStrs[False] := _(LNGModusOverwrite);
  FInsertModeStrs[True] := _(LNGModusInsert);
  CalculateStatusline;
  if UUtils.Left(MIExecuteWithoutConsole.Caption, 4) <> '  ' then
  begin
    MIExecuteWithoutConsole.Caption := '  ' + MIExecuteWithoutConsole.Caption;
    MIExecuteWithConsole.Caption := '  ' + MIExecuteWithConsole.Caption;
  end;
end;

procedure TFEditForm.New(const FileName: string);
begin
  Caption := FileName;
  Pathname := FileName;
  FFileExtension := LowerCase(ExtractFileExt(FileName));
  DesignButton.Visible := FileExists(ChangeFileExt(FileName, '.jfm'));
  SetHighlighter;
  SetToolButtons;
  if not FHidden then
  begin
    FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
    FJava.TabModified(Number, Modified);
    Enter(Self); // must stay!
    if Visible and FEditor.CanFocus then
      FEditor.SetFocus;
  end;
end;

procedure TFEditForm.Open(const FileName: string; State: string;
  Hidden: Boolean = False);
begin
  FHidden := Hidden;
  try
    FEditor.LockUndo;
    try
      FEditor.Lines.LoadFromFile(FileName);
      // set UTF8 as default FEncoding
      if (FEditor.Lines.Encoding <> TEncoding.UTF8) and
        not IsWriteProtected(FileName) then
      begin
        var
        WriteTime := TFile.GetLastWriteTime(FileName);
        FEditor.Lines.SaveToFile(FileName, TEncoding.UTF8);
        TFile.SetLastWriteTime(FileName, WriteTime);
        FEditor.Lines.LoadFromFile(FileName, TEncoding.UTF8);
      end;
      FEncoding := EncodingAsString(FEditor.Lines.Encoding.EncodingName);
      FLineBreak := FEditor.Lines.LineBreak;
      FEditor.ReplaceTabs(FConfiguration.TabWidth);
    finally
      FEditor.UnlockUndo;
    end;
    if FEditor.NeedsWordWrap then
      SBWordWrapClick(Self);
    FileAge(FileName, FEditorAge);
    New(FileName);
    FEditor.ReadOnly := (Pos(FConfiguration.JavaCache, FileName) = 1) or
      IsWriteProtected(FileName);
    if FEditor.ReadOnly then
      Caption := Caption + ' (' + _(LNGWriteProtected) + ')';
    CalculateStatusline;
    CollapseGUICreation;
    if IsJava then
      ParseSourceCode(True);
    SetState(State);
    // ensure vertical scrollbar is visible
    FEditor.UpdateScrollBars;
  except
    on e: Exception do
    begin
      ErrorMsg(e.Message);
      FConfiguration.Log('TFEditForm.Open: ' + FileName, e);
    end;
  end;
end;

procedure TFEditForm.CollapseGUICreation;
var
  Line: Integer;
  Str, Name: string;
begin
  if FConfiguration.GUICodeFolding and (GetFrameType > 1) then
  begin
    Name := ChangeFileExt(ExtractFileName(Pathname), '');
    case FFrameType of
      8:
        Str := 'public void start(Stage';
      7, 4:
        Str := 'public void init()';
      6, 5, 3, 2:
        Str := 'public ' + Name + '(';
    end;
    for var I := 0 to Min(FEditor.AllFoldRanges.Count - 1, 2) do
    begin
      Line := FEditor.AllFoldRanges[I].FromLine;
      if Pos(Str, FEditor.Lines[Line - 1]) > 0 then
        FEditor.Collapse(I);
    end;
  end;
end;

procedure TFEditForm.DoOnIdle;
begin
  if not FNeedsParsing then
    SyncFileStructure;
end;

procedure TFEditForm.SyncFileStructure;
begin
  if FNeedToSyncFileStructure and IsJava then
  begin
    FFileStructure.ShowEditorCodeElement;
    FNeedToSyncFileStructure := False;
  end;
end;

procedure TFEditForm.EnsureStartEnd(CorI: Integer = 1);
begin
  if Assigned(FEditor) and not FEditor.ReadOnly and not HasStartAndEnd(CorI)
  then
    InsertStartEnd;
end;

function PointToDisplay(Posi: TPoint): TDisplayCoord;
begin
  Result.Column := Posi.X;
  Result.Row := Posi.Y;
end;

procedure TFEditForm.SynEditorReplaceText(Sender: TObject;
  const ASearch, AReplace: string; Line, Column: Integer;
  var AAction: TSynReplaceAction);
var
  APos: TDisplayCoord;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    AAction := raSkip
  else
  begin
    APos := DisplayCoord(Column, Line);
    APos := PointToDisplay
      (FEditor.ClientToScreen(FEditor.RowColumnToPixels(APos)));
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    with TFConfirmReplace.Create(FJava) do
    begin
      PrepareShow(EditRect, APos.Column, APos.Row,
        APos.Row + FEditor.LineHeight, ASearch);
      case ShowModal of
        mrYes:
          AAction := raReplace;
        mrYesToAll:
          AAction := raReplaceAll;
        mrNo:
          AAction := raSkip;
      else
        AAction := raCancel;
      end;
      Free;
    end;
  end;
end;

procedure TFEditForm.SetHighlighter;
begin
  if FConfiguration.NoSyntaxHighlighting then
    FEditor.Highlighter := nil
  else
    FEditor.Highlighter := FConfiguration.GetHighlighter(FFileExtension);

  if FEditor.Highlighter = nil then
    FEditor.StructureColoring := False
  else
  begin
    FEditor.StructureColoring := FConfiguration.StructureColoring;
    FEditor.UseCodeFolding := True;
  end;
  if FEditor.StructureColoring and IsJava then
    FNeedsParsing := True;
end;

procedure TFEditForm.SetToolButtons;
begin
  for var I := 0 to 29 do
    FToolButtons[I].Visible := True;
  if not IsJava then
  begin
    TBDesignform.Visible := False;
    TBStructure.Visible := False;
    TBClassOpen.Visible := False;
    TBSystemOutPrintln.Visible := False;
    TBBreakpoint.Visible := False;
    TBBreakpointsClear.Visible := False;
    TBStructureIndent.Visible := Pos('{', FEditor.Text) > 0;
    TBIfStatement.Visible := False;
    TBIfElseStatement.Visible := False;
    TBWhileStatement.Visible := False;
    TBForStatement.Visible := False;
    TBDoWhileStatement.Visible := False;
    TBSwitchStatement.Visible := False;
    TBTryStatement.Visible := False;
    TBBlockStatement.Visible := False;
  end;
  if IsPascal then
  begin
    TBClassOpen.Visible := True;
    TBStructureIndent.Visible := False;
  end;
  TBBrowser.Visible := IsHTML;
  TBValidate.Visible := IsHTML or IsCSS;
end;

procedure TFEditForm.Print;
begin
  if HasDefaultPrinter then
    PrintAll(False)
  else
    ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFEditForm.PrintAll(AllPages: Boolean);
begin
  if HasDefaultPrinter then
  begin
    if FEditor.Font.Size >= 13 then
      if MessageDlg(Format(_('Font size is %3d pt. Print anyhow?'),
        [FEditor.Font.Size]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
    try
      PreparePrint;
      FJava.PrintDialog.MaxPage := FSynEditPrint.PageCount;
      // invalid Printer exception
      FJava.PrintDialog.ToPage := FSynEditPrint.PageCount;
      var
      Options := FJava.PrintDialog.Options;
      if FEditor.SelAvail then
        Include(Options, poSelection)
      else
        Exclude(Options, poSelection);
      FJava.PrintDialog.Options := Options;
      if AllPages then
        FSynEditPrint.Print
      else if FJava.PrintDialog.Execute then
      begin
        FSynEditPrint.Colors := FConfiguration.PrintColored;
        case FJava.PrintDialog.PrintRange of
          prAllPages:
            FSynEditPrint.Print;
          prPageNums:
            FSynEditPrint.PrintRange(FJava.PrintDialog.FromPage,
              FJava.PrintDialog.ToPage);
          prSelection:
            begin
              FSynEditPrint.SelectedOnly := True;
              FSynEditPrint.Print;
            end;
        end;
        SetPrinterIndex(Printer.PrinterIndex);
        FConfiguration.WriteStringU('Printer', 'Printer',
          Printer.Printers[Printer.PrinterIndex]);
      end;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  end
  else
    ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFEditForm.PreparePrint;
var
  Str, Str1: string;
  Posi: Integer;
  AFont: TFont;

  procedure EditMacro(var Str: string);
  var
    Posi: Integer;
    Macro: string;
  begin
    Macro := '%FILE%';
    Posi := Pos(Macro, UpperCase(Str));
    if Posi > 0 then
    begin
      Delete(Str, Posi, Length(Macro));
      Insert(ExtractFileName(Pathname), Str, Posi);
    end;
    Macro := '%PATH%';
    Posi := Pos(Macro, UpperCase(Str));
    if Posi > 0 then
    begin
      Delete(Str, Posi, Length(Macro));
      Insert(Pathname, Str, Posi);
    end;
    Macro := '%DATE%';
    Posi := Pos(Macro, UpperCase(Str));
    if Posi > 0 then
    begin
      Delete(Str, Posi, Length(Macro));
      Insert('$DATE$', Str, Posi);
    end;
    Macro := '%TIME%';
    Posi := Pos(Macro, UpperCase(Str));
    if Posi > 0 then
    begin
      Delete(Str, Posi, Length(Macro));
      Insert('$TIME$', Str, Posi);
    end;
    Macro := '%PAGENUM%';
    Posi := Pos(Macro, UpperCase(Str));
    if Posi > 0 then
    begin
      Delete(Str, Posi, Length(Macro));
      Insert('$PAGENUM$', Str, Posi);
    end;
    Macro := '%PAGECOUNT%';
    Posi := Pos(Macro, UpperCase(Str));
    if Posi > 0 then
    begin
      Delete(Str, Posi, Length(Macro));
      Insert('$PAGECOUNT$', Str, Posi);
    end;
  end;

begin
  AFont := TFont.Create;
  if not Assigned(FSynEditPrint) then
    FSynEditPrint := TSynEditPrint.Create(Self);
  with FSynEditPrint do
  begin
    AFont.Assign(Header.DefaultFont);
    AFont.Size := Header.DefaultFont.Size - 2;

    DocTitle := _('File from Java-Editor');
    SelectedOnly := (FJava.PrintDialog.PrintRange = prSelection);
    Highlight := Assigned(FEditor.Highlighter);
    Highlighter := FEditor.Highlighter;
    Colors := False;
    LineNumbers := FConfiguration.WithLinenumbers;
    LineNumbersInMargin := FConfiguration.LineNumbersInMargin;

    Margins.UnitSystem := usMM;
    Margins.Left := FConfiguration.BorderLeft;
    Margins.Top := FConfiguration.BorderTop;
    Margins.Right := FConfiguration.BorderRight;
    Margins.Bottom := FConfiguration.BorderBottom;

    Header.Clear;
    Str := FConfiguration.Header;
    EditMacro(Str);
    Posi := Pos('#', Str);
    Str1 := Copy(Str, 1, Posi - 1);
    if Str1 <> '' then
      Header.Add(Str1, AFont, taLeftJustify, 1);
    if Posi > 0 then
      Delete(Str, 1, Posi);
    Posi := Pos('#', Str);
    Str1 := Copy(Str, 1, Posi - 1);
    if Str1 <> '' then
      Header.Add(Str1, AFont, taCenter, 1);
    if Posi > 0 then
      Delete(Str, 1, Posi);
    if Str <> '' then
      Header.Add(Str, AFont, taRightJustify, 1);

    Footer.Clear;
    Str := FConfiguration.Footer;
    EditMacro(Str);
    Posi := Pos('#', Str);
    Str1 := Copy(Str, 1, Posi - 1);
    if Str1 <> '' then
      Footer.Add(Str1, AFont, taLeftJustify, 1);
    if Posi > 0 then
      Delete(Str, 1, Posi);
    Posi := Pos('#', Str);
    Str1 := Copy(Str, 1, Posi - 1);
    if Str1 <> '' then
      Footer.Add(Str1, AFont, taCenter, 1);
    if Posi > 0 then
      Delete(Str, 1, Posi);
    if Str <> '' then
      Footer.Add(Str, AFont, taRightJustify, 1);

    FSynEditPrint.SynEdit := FEditor;
  end;
  FreeAndNil(AFont);
end;

procedure TFEditForm.SetFont(AFont: TFont);
begin
  FEditor.Font.Assign(AFont);
  FEditor.Gutter.Font.Assign(AFont);
  FEditor.Gutter.Font.Height := AFont.Height + 2;
  FConfiguration.EditFont.Assign(AFont);
end;

function TFEditForm.GetFont: TFont;
begin
  Result := FEditor.Font;
end;

procedure TFEditForm.SetFontSize(Delta: Integer);
begin
  FEditor.Font.Size := FEditor.Font.Size + Delta;
  if FEditor.Font.Size < 6 then
    FEditor.Font.Size := 6;
  FEditor.Gutter.Font.Size := FEditor.Font.Size;
  FConfiguration.EditFont.Assign(FEditor.Font);
  FConfiguration.EditFont.Size := PPIUnScale(FConfiguration.EditFont.Size);
end;

procedure TFEditForm.SetOptions;
begin
  FEditor.TabWidth := FConfiguration.TabWidth;
  FEditor.Indent := FConfiguration.Indent;
  var
  Options := FEditor.Options;
  if FConfiguration.AutomaticIndent then
    Include(Options, eoAutoIndent)
  else
    Exclude(Options, eoAutoIndent);
  if FConfiguration.IndentHelp then
    Include(Options, eoSmartTabs)
  else
    Exclude(Options, eoSmartTabs);
  if FConfiguration.ShowLigatures then
    Include(Options, eoShowLigatures)
  else
    Exclude(Options, eoShowLigatures);
  var
  ScrollOptions := FEditor.ScrollOptions;
  if FConfiguration.CursorBehindLine then
    Include(ScrollOptions, eoScrollPastEol)
  else
    Exclude(ScrollOptions, eoScrollPastEol);
  FEditor.ScrollOptions := ScrollOptions;
  if FConfiguration.ShowBracketPair then
    FEditor.OnPaintTransient := EditorPaintTransient
  else
    FEditor.OnPaintTransient := nil;
  if FConfiguration.EightyColumnLine then
    FEditor.RightEdge := 80
  else
    FEditor.RightEdge := 0;
  if FConfiguration.CompactLineNumbers then
    FEditor.OnGutterGetText := SynEditGutterGetText
  else
    FEditor.OnGutterGetText := nil;

  FEditor.DisplayFlowControl.Enabled := FConfiguration.ShowControlFlowSymbols;
  FEditor.ActiveLineColor := FConfiguration.ActiveLineColor;
  FEditor.StructureColorIntensity := FConfiguration.StructureColorIntensity;
  FEditor.PaintStructurePlane := FConfiguration.StructureColoringPlane;
  FEditor.Options := Options;
  EditformToolbar.Visible := FConfiguration.VisToolbars[2];
  FEditor.Gutter.ShowLineNumbers := FConfiguration.LineNumbering;
  FEditor.Gutter.Font.Assign(FConfiguration.Font);
  FEditor.Gutter.Font.Height := FEditor.Font.Height + 2;
  FCheckAgeEnabled := FConfiguration.CheckAge;
  SetHighlighter;
  MICreateStructogram.Visible := not FConfiguration.LockedStructogram;
  EditShortCuts;
  FConfiguration.RemoveShortcutsFrom(PopUpEditor);
end;

procedure TFEditForm.Save(WithBackup: Boolean);
var
  BackupName, Ext: string;
  AEncoding: TEncoding;
  Form: TFForm;
begin
  if FEditor.ReadOnly and FEditor.Modified then begin
    ErrorMsg(Pathname + ' ' + _(LNGWriteProtected));
    Exit;
  end;
  if ExtractFilePath(Pathname) = '' then
    Pathname := FConfiguration.Sourcepath + Pathname;
  if UpperCase(ExtractFileExt(Pathname)) = '.XML' then // due to Android Mode
    WithBackup := False;
  if WithBackup then
  begin
    BackupName := Pathname;
    Ext := ExtractFileExt(Pathname);
    if Length(Ext) >= 2 then
      Ext[2] := '~'
    else
      Ext := '.~';
    BackupName := ChangeFileExt(BackupName, Ext);
    if FileExists(BackupName) then
      DeleteFile(PChar(BackupName));
    if FileExists(Pathname) then
      RenameFile(Pathname, BackupName);
  end;
  try
    FEditor.Lines.LineBreak := FLineBreak;
    AEncoding := GetEncodingAsType;
    if AEncoding = TEncoding.UTF8 then
      FEditor.Lines.WriteBOM := False
    else if AEncoding = TEncoding.Unicode then
      FEditor.Lines.WriteBOM := True;
    FEditor.Lines.SaveToFile(Pathname, AEncoding);
    FEditor.MarkSaved;

    FileAge(Pathname, FEditorAge);
    if Assigned(Partner) then
      Partner.Save(WithBackup);
    Form := FJava.GetTDIWindowType(Pathname, '%Q%');
    if Assigned(Form) then
      (Form as TFSequenceForm).RefreshFromFile;
  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
  SetModified(False);
  Statusline(1, '');
  FMessages.StatusMessage(Pathname + ' ' + _(LNGSaved));
  EditorStatusChange(Self, []);
end;

procedure TFEditForm.SetModified(AModified: Boolean);
begin
  FEditor.Modified := AModified;
  inherited SetModified(AModified);
end;

function TFEditForm.GetModified: Boolean;
begin
  Result := inherited GetModified or Assigned(FEditor) and FEditor.Modified;
end;

procedure TFEditForm.SaveIn(const Dir: string);
begin
  SaveAs(Dir + ExtractFileName(Pathname));
end;

procedure TFEditForm.SaveAs(const FileName: string);
begin
  if (Pos(FConfiguration.JavaCache, FileName) = 1) and FEditor.ReadOnly then
    Exit;
  var
  OldName := ChangeFileExt(ExtractFileName(Pathname), '');
  var
  NewName := ChangeFileExt(ExtractFileName(FileName), '');
  if FConfiguration.RenameWhenSave and IsJava then
    with FEditor do
    begin
      ReplaceWord('public class ' + OldName, 'public class ' + NewName, False);
      ReplaceWord('public interface ' + OldName, 'public interface ' +
        NewName, False);
      ReplaceWord('public ' + OldName, 'public ' + NewName, True);
      ReplaceText('new ' + OldName + '();', 'new ' + NewName + '();', True);
      ReplaceText('new ' + OldName + '("' + OldName + '");',
        'new ' + NewName + '("' + NewName + '");', True); // Old files
      if FFrameType = 8 then
        ReplaceText('primaryStage.setTitle("' + OldName + '");',
          'primaryStage.setTitle("' + NewName + '");', False)
      else
        ReplaceText('setTitle("' + OldName + '");', 'setTitle("' + NewName +
          '");', False);
      ReplaceWord('// end of class ' + OldName, '// end of class ' +
        NewName, False);
    end;
  if OldName <> '' then // new file?
    FJava.RenameTabAndWindow(Number, FileName);
  if FileExists(FileName) then
    DeleteFile(PChar(FileName));
  Pathname := FileName;
  FEditor.ReadOnly := False;
  Save(WithoutBackup);
  FEditor.Lines.LoadFromFile(FileName);
  // due to possible change of FEncoding or FLineBreak
  try
    Caption := FileName;
  except
    on e: Exception do
      FConfiguration.Log('TFEditForm.Caption: ' + FileName, e);
  end;
  FFileExtension := LowerCase(ExtractFileExt(FileName));
  SetHighlighter;
  SetToolButtons;
end;

procedure TFEditForm.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  var
  Str := _('Line') + ': %4d  ' + _('Column') + ': %3d';
  Statusline(0, Format(Str, [FEditor.CaretY, FEditor.CaretX]));
  if FEditor.ReadOnly then
    Statusline(1, ' ' + _(LNGWriteProtected) + ' ')
  else
    Statusline(1, FModifiedStrs[Modified]);
  Statusline(2, FInsertModeStrs[FEditor.InsertMode]);
  Statusline(3, ' ' + EncodingAsString(FEncoding) + '/' +
    LinebreakAsString + ' ');

  if (Changes * [scModified] <> []) then
  begin
    FNeedsParsing := IsJava;
    FJava.TabModified(Number, Modified);
  end;

  if Changes * [scSelection] <> [] then
    UpdateState;

  if scCaretY in Changes then
    FNeedToSyncFileStructure := True;

  if Assigned(FEditor) and Assigned(FEditor.Lines) then
  begin
    var
    Digits := Length(IntToStr(FEditor.Lines.Count));
    if Digits <> FEditor.Gutter.DigitCount then
      FEditor.Gutter.DigitCount := Digits;
  end;

  if Assigned(FFileStructure) then
    FFileStructure.ShowSelected;
end;

procedure TFEditForm.PutText(Str: string; WithCursor: Boolean = True);
var
  Posi, OffX, OffY, X, Y: Integer;
  Str1: string;
begin
  Posi := Pos('|', Str);
  if Posi = 0 then
    WithCursor := False;
  if WithCursor then
  begin
    OffY := 0;
    Str1 := Copy(Str, 1, Posi - 1);
    Delete(Str, Posi, 1);
    Posi := Pos(#13#10, Str1);
    while Posi > 0 do
    begin
      Inc(OffY);
      Delete(Str1, 1, Posi + 1);
      Posi := Pos(#13#10, Str1);
    end;
    OffX := Length(Str1) + 1;
    with FEditor do
    begin
      if SelText = '' then
      begin
        X := CaretX;
        Y := CaretY;
      end
      else
      begin
        X := BlockBegin.Char;
        Y := BlockBegin.Line;
      end;
      SelText := Str;
      CaretY := Y + OffY;
      if OffY = 0 then
        CaretX := X + OffX - 1
      else
        CaretX := OffX;
    end;
  end
  else
    FEditor.SelText := Str;
  FEditor.EnsureCursorPosVisible;
  FNeedsParsing := True;
end;

procedure TFEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsJava then
    FFileStructure.Clear;
  ClearCompilerErrorMarks;
  if not AlreadySavedAs and Modified then
  begin
    FJava.DoSave(Self, True);
    AlreadySavedAs := True;
  end;
  CanClose := True;
end;

procedure TFEditForm.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  try
    LockEnter := True;
    if Assigned(Partner) then
    begin
      Partner.Close;
      Partner := nil;
      FObjectInspector.SetSelectedObject(nil);
    end;
    if Assigned(FJava.EditorForm) and (FJava.EditorForm.Pathname = Pathname)
    then
      FJava.EditorForm := nil;
    ClearMarks;
    for var I := TVFileStructure.Items.Count - 1 downto 0 do
    begin
      var
      AInteger := TInteger(TVFileStructure.Items[I].Data);
      FreeAndNil(AInteger);
    end;
    FJava.ShowAWTSwingOrFX(0);
  finally
    inherited;
    if FJava.TDIFormsList.Count = 0 then
      TThread.ForceQueue(nil,
        procedure
        begin
          FJava.UpdateMenuItems(Self);
        end);
  end;
end;

procedure TFEditForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FParseThread) then
  begin
    if (0 < FParseThread.State) and (FParseThread.State < 3) then
    begin
      FParseThread.Abort := True;
      FParseThread.WaitFor;
    end;
    FreeAndNil(FParseThread);
  end;
  FreeAndNil(FParser);
  FreeAndNil(TVFileStructure);
  FreeAndNil(FModel);
  FreeAndNil(FEditor);
end;

procedure TFEditForm.ClearBreakpoints;
var
  Str: string;
  Mark: TSynEditMark;
begin
  for var I := Editor.Marks.Count - 1 downto 0 do
  begin
    Mark := FEditor.Marks[I];
    if (Mark.ImageIndex = BreakpointImageIndex) or
      (Mark.ImageIndex = NoBreakpointImageIndex) then
    begin
      if MyDebugger.Running then
      begin
        Str := CBSearchClassOrMethod(not StopInAt, Mark.Line);
        if Str <> '' then
          MyDebugger.NewCommand(2, Str);
      end;
      FEditor.InvalidateLine(Mark.Line);
      FEditor.Marks.Remove(Mark);
    end;
  end;
  FBreakPointCount := 0;
end;

procedure TFEditForm.ClearCompilerErrorMarks;
begin
  for var I := FEditor.Marks.Count - 1 downto 0 do
  begin
    var
    Mark := FEditor.Marks[I];
    if Mark.ImageIndex = ErrorMarkIndex then
    begin
      FEditor.InvalidateLine(Mark.Line);
      FEditor.Marks.Remove(Mark);
    end;
  end;
end;

procedure TFEditForm.ClearMarks;
begin
  for var I := FEditor.Marks.Count - 1 downto 0 do
  begin
    var
    Mark := FEditor.Marks[I];
    FEditor.InvalidateLine(Mark.Line);
    FEditor.Marks.Remove(Mark);
  end;
end;

procedure TFEditForm.SetBreakpoints;
begin
  with FEditor do
  begin
    for var I := 0 to Marks.Count - 1 do
    begin
      var
      Mark := Marks[I];
      if Mark.ImageIndex = BreakpointImageIndex then
      begin
        ParseSourceCode(False);
        var
        Str := CBSearchClassOrMethod(StopInAt, Mark.Line);
        if Str = '' then
          Mark.ImageIndex := NoBreakpointImageIndex
        else
          MyDebugger.NewCommand(1, Str);
      end;
    end;
  end;
end;

function TFEditForm.HasBreakpoints: Boolean;
begin
  Result := False;
  if Assigned(FEditor) and Assigned(FEditor.Marks) then
    for var I := 0 to FEditor.Marks.Count - 1 do
    begin
      var
      Mark := FEditor.Marks[I];
      if Mark.ImageIndex = BreakpointImageIndex then
        Exit(True);
    end;
end;

procedure TFEditForm.SetDebuglineMark(Line: Integer);
const
  DebugImageIndex = 12;
var
  LineMarks: TSynEditMarks;
begin
  DeleteDebuglineMark;
  FDebuglineMark := TSynEditMark.Create(FEditor);
  FEditor.Marks.GetMarksForLine(Line, LineMarks);
  for var I := 1 to MAX_MARKS do
    if not Assigned(LineMarks[I]) then
      Break;
  FDebuglineMark.Line := Line;
  with FDebuglineMark do
  begin
    ImageIndex := DebugImageIndex;
    Visible := True;
  end;
  FEditor.Marks.Add(FDebuglineMark);
  FEditor.InvalidateLine(Line);
  FEditor.CaretY := Line;
  FEditor.EnsureCursorPosVisibleEx(True);
end;

procedure TFEditForm.DeleteDebuglineMark;
var
  Line: Integer;
  LineMarks: TSynEditMarks;
begin
  if not Assigned(FDebuglineMark) then
    Exit;
  if Assigned(FEditor) and Assigned(FEditor.Marks) then
  begin
    Line := FDebuglineMark.Line;
    FEditor.Marks.Remove(FDebuglineMark);
    FDebuglineMark := nil;
    FEditor.Marks.GetMarksForLine(Line, LineMarks);
    for var I := 1 to MAX_MARKS do
      if not Assigned(LineMarks[I]) then
        Break
      else
        LineMarks[I].Visible := True;
    FEditor.InvalidateLine(Line);
  end;
end;

procedure TFEditForm.DeleteBreakpoint(Str: string);
var
  Mark: TSynEditMark;
  Posi: Integer;
begin
  // Format:
  // Unable to set breakpoint BinBaum:23 : No code at Line 23 in BinBaum
  // Unable to set deferred breakpoint BinBaum:23 : No code at Line 23 in BinBaum
  Posi := Pos(':', Str);
  Delete(Str, 1, Posi);
  Posi := Pos(' :', Str);
  Delete(Str, Posi, Length(Str));
  if TryStrToInt(Str, Posi) then
    for var I := 0 to FEditor.Marks.Count - 1 do
    begin
      Mark := FEditor.Marks[I];
      if (Mark.ImageIndex = BreakpointImageIndex) and (Mark.Line = Posi) then
        FEditor.Marks[I].ImageIndex := NoBreakpointImageIndex;
    end;
end;

procedure TFEditForm.HTMLforApplet(const AWidth, AHeight, CharSet, Path,
  AClass: string; WithJEApplets, Debug: Boolean);
begin
  var
  Archive := FConfiguration.GetAppletArchiv;
  // http://java.sun.com/docs/books/tutorial/deployment/applet/deployindex.html
  with FEditor.Lines do
  begin
    Add('<!DOCTYPE html>');
    Add('<html>');
    Add('<head>');
    Add('<title>' + AClass + '-Applet</title>');
    Add('<meta http-equiv="Content-Type" content="text/html;charset=' +
      CharSet + '">');
    Add('</head>');
    Add('<body>');
    Add('<h1>' + AClass + '-Applet</h1>');
    Add('<hr>');
    Add('<applet code="' + ReplaceStr(AClass, '.', '/') + '.class" ' + Archive +
      ' width="' + AWidth + '" height="' + AHeight + '">');
    Add('</applet>');
    Add('<hr>');
    Add('</body>');
    Add('</html>');
  end;
  Modified := True;
end;

function TFEditForm.IsHTMLApplet: Boolean;
begin
  Result := IsHTML and (HasWord('Applet') or HasWord('JApplet'));
end;

function TFEditForm.GetSaveAsName: string;
begin
  Result := Pathname;
  if IsJava then
  begin
    ParseSourceCode(False);
    var
    ClassIt := FModel.ModelRoot.GetAllClassifiers;
    while ClassIt.HasNext do
    begin
      var
      Cent := TClassifier(ClassIt.Next);
      if ((Cent is TClass) or (Cent is TInterface)) and
        (Cent.Visibility = viPublic) and (Cent.Pathname = Pathname) then
        Exit(ExtractFilePath(Pathname) + WithoutGeneric(Cent.ShortName)
          + '.java');
    end;
  end;
end;

procedure TFEditForm.Enter(Sender: TObject);
begin
  if LockEnter then
    Exit;
  FJava.scpSetEditForm(Self);
  FJava.EditorForm := Self; // must stay in before inherited!
  inherited;
  EditorStatusChange(Sender, []);
  if Assigned(Partner) then
    FGUIDesigner.ChangeTo(TFGUIForm(Partner));
  if Assigned(TVFileStructure) and (FFileStructure.myForm <> Self) and
    Assigned(TVFileStructure.Items) then
    FFileStructure.InitWithItems(TVFileStructure.Items, Self);
  FJava.ShowAWTSwingOrFX(FFrameType);
end;

procedure TFEditForm.Statusline(Num: Integer; const Str: string);
begin
  StatusBar.Panels[Num].Text := Str;
end;

procedure TFEditForm.CalculateStatusline;
var
  Str: string;
begin
  StatusBar.Constraints.MinHeight := Canvas.TextHeight('Ag') + 4;
  var
  AWidth := StatusBar.Canvas.TextWidth('_' + _('Line') + ':_9999_' + _('Column')
    + ':_999_');
  StatusBar.Panels[0].Width := Max(AWidth + 10, StatusBar.Panels[1].Width);
  if FEditor.ReadOnly then
    AWidth := StatusBar.Canvas.TextWidth('_' + _(LNGWriteProtected) + '_')
  else
    AWidth := StatusBar.Canvas.TextWidth('_' + _(LNGModified) + '_');
  StatusBar.Panels[1].Width := Max(AWidth + 10, StatusBar.Panels[1].Width);
  if Length(_(LNGModusOverwrite)) > Length(_(LNGModusInsert)) then
    Str := _(LNGModusOverwrite)
  else
    Str := _(LNGModusInsert);
  AWidth := StatusBar.Canvas.TextWidth('_' + Str + '_');
  StatusBar.Panels[2].Width := Max(AWidth + 10, StatusBar.Panels[2].Width);
  StatusBar.Panels[3].Width := ClientWidth;
end;

procedure TFEditForm.UpdateState;
begin
  var
  SelAvail := FEditor.SelAvail;
  var
  MouseInBorder := FMouseIsInBorderOfStructure;
  with FJava do
  begin
    SetEnabledMI(MIUndo, FEditor.CanUndo);
    SetEnabledTB(TBUndo, MIUndo.Enabled);
    SetEnabledMI(MIRedo, FEditor.CanRedo);
    SetEnabledTB(TBRedo, MIRedo.Enabled);

    SetEnabledMI(MICut, SelAvail or MouseInBorder);
    SetEnabledMI(MICopy, SelAvail or MouseInBorder);
    SetEnabledMI(MICopyNormal, SelAvail);
    SetEnabledMI(MICopyRTF, SelAvail);
    SetEnabledMI(MICopyHTML, SelAvail);
    SetEnabledMI(MICopyHTMLAsText, SelAvail);
    SetEnabledMI(MICopyNumbered, SelAvail);
    SetEnabledMI(MICopyRtfNumbered, SelAvail);
    SetEnabledMI(MIPaste, FEditor.CanPaste);
  end;
  SetEnabledMI(MIUndo, FEditor.CanUndo);
  SetEnabledMI(MIRedo, FEditor.CanRedo);
  SetEnabledMI(MICut, SelAvail or MouseInBorder);
  SetEnabledMI(MICopy, SelAvail or MouseInBorder);
  SetEnabledMI(MIInsert, FEditor.CanPaste);
end;

function TFEditForm.CurrentCol: Integer;
begin
  Result := FEditor.CaretX;
end;

function TFEditForm.CurrentRow: Integer;
begin
  Result := FEditor.CaretY;
end;

function TFEditForm.HasBreakpoint(Line: Integer): Boolean;
begin
  Result := Assigned(GetBreakpointMark(Line));
end;

function TFEditForm.GetBreakpointMark(Line: Integer): TSynEditMark;
begin
  Result := nil;
  for var I := 0 to FEditor.Marks.Count - 1 do
  begin
    var
    Mark := FEditor.Marks[I];
    if ((Mark.ImageIndex = BreakpointImageIndex) or
      (Mark.ImageIndex = NoBreakpointImageIndex)) and (Mark.Line = Line) then
      Exit(Mark);
  end;
end;

function TFEditForm.IsExecutableLine(Line: Integer): Boolean;
begin
  Result := True;
  for var I := 0 to FEditor.Marks.Count - 1 do
  begin
    var
    Mark := FEditor.Marks[I];
    if (Mark.ImageIndex = NoBreakpointImageIndex) and (Mark.Line = Line) then
      Exit(False);
  end;
end;

procedure TFEditForm.InsertBreakpointMark(Line: Integer);
var
  Str: string;
  Int: Integer;
  Mark: TSynEditMark;
  Marks: TSynEditMarks;
begin
  FEditor.Marks.GetMarksForLine(Line, Marks);
  Int := 1;
  while Assigned(Marks[Int]) and (Int <= MAX_MARKS) do
    if Marks[Int].ImageIndex in [BreakpointImageIndex, NoBreakpointImageIndex]
    then
      Exit
    else
      Inc(Int);

  Mark := TSynEditMark.Create(FEditor);
  Mark.Line := Line;
  Mark.Char := 1;
  Mark.ImageIndex := BreakpointImageIndex;
  Mark.Visible := True;
  Mark.InternalImage := False;

  ParseSourceCode(False);
  Str := CBSearchClassOrMethod(StopInAt, Line);
  if Str = '' then
    Mark.ImageIndex := NoBreakpointImageIndex
  else if MyDebugger.Running then
    MyDebugger.NewCommand(1, Str);
  FEditor.Marks.Add(Mark);
  Inc(FBreakPointCount);
  FEditor.InvalidateLine(Line);
end;

procedure TFEditForm.DeleteBreakpointAtLine(Line: Integer);
begin
  var
  Mark := GetBreakpointMark(Line);
  if Assigned(Mark) then
  begin
    var
    Str := CBSearchClassOrMethod(not StopInAt, Line);
    if MyDebugger.Running and (Str <> '') then
      MyDebugger.NewCommand(2, Str);
    FEditor.Marks.Remove(Mark);
    Dec(FBreakPointCount);
    FEditor.InvalidateLine(Line);
  end;
end;

procedure TFEditForm.InsertGotoCursorBreakpoint;
var
  Str: string;
  Line: Integer;
begin
  Line := FEditor.CaretY;
  if not HasBreakpoint(Line) then
  begin
    Str := CBSearchClassOrMethod(StopInAt, Line);
    MyDebugger.RunToCursorBreakpoint(Str);
  end;
end;

procedure TFEditForm.InsertBreakpoint;
begin
  BreakpointGutterClick(Self, mbLeft, FEditor.BookMarkOptions.Xoffset, 0,
    FEditor.CaretY, 0);
end;

procedure TFEditForm.BreakpointGutterClick(Sender: TObject;
Button: TMouseButton; X, Y, Row, Line: Integer);
begin
  if Line > FEditor.Lines.Count then
    Exit;
  if not FJava.MIBreakpoint.Enabled or FEditor.SelAvail then
    Exit;
  if HasBreakpoint(Line) then
    DeleteBreakpointAtLine(Line)
  else
    InsertBreakpointMark(Line);
end;

procedure TFEditForm.BookmarkGutterClick(Sender: TObject; Button: TMouseButton;
X, Y, Row, Line: Integer);
begin
  if (Line > FEditor.Lines.Count) or FEditor.SelAvail then
    Exit;
  SetDeleteBookmark(FEditor.CaretX, FEditor.CaretY);
end;

function TFEditForm.GetLineInfos(ALine: Integer): TLineInfos;
begin
  Result := [];
  if ALine > 0 then
  begin
    if Assigned(MyJavaCommands) and MyDebugger.Running and
      Assigned(FDebuglineMark) and (ALine = FDebuglineMark.Line) then
      Include(Result, dlCurrentDebuggerLine);
    if Assigned(MyJavaCommands) and MyDebugger.Running and Assigned(FMessages)
      and (ALine = FMessages.SearchGoalLine) and
      (FMessages.SearchGoalPath = Pathname) then
      Include(Result, dlSearchLine);
    if IsExecutableLine(ALine) then
      Include(Result, dlExecutableLine);
    if HasBreakpoint(ALine) then
      Include(Result, dlBreakpointLine);
  end;
end;

procedure TFEditForm.SynEditorSpecialLineColors(Sender: TObject; Line: Integer;
var Special: Boolean; var Foreground, Background: TColor);
begin
  Special := True;
  var
  LineInfos := GetLineInfos(Line);
  if dlCurrentDebuggerLine in LineInfos then
  begin
    Foreground := clWhite;
    Background := clBlue;
    Exit;
  end;
  if dlSearchLine in LineInfos then
  begin
    Foreground := clWhite;
    Background := clMaroon;
    Exit;
  end;
  if dlBreakpointLine in LineInfos then
  begin
    Foreground := clWhite;
    if dlExecutableLine in LineInfos then
      Background := clRed
    else
      Background := clGray;
    Exit;
  end;
  Special := False;
end;

procedure TFEditForm.SetDeleteBookmark(Xpos, YPos: Integer);
var
  Int, X, Y: Integer;
begin
  with FEditor do
  begin
    Int := 0;
    while (Int < 10) do
    begin
      if GetBookMark(Int, X, Y) and (Y = YPos) then
      begin
        ClearBookMark(Int);
        Exit;
      end;
      Inc(Int);
    end;
    Int := 0;
    while (Int < 10) and IsBookmark(Int) do
      Inc(Int);
    if Int < 10 then
      SetBookMark(Int, Xpos, YPos)
    else
      Windows.Beep(600, 200);
  end;
end;

procedure TFEditForm.Unindent;
begin
  with FEditor do
  begin
    TabWidth := FConfiguration.Indent;
    if SelAvail then
      CommandProcessor(EcBlockUnindent, #0, nil)
    else
    begin
      var
      X := CaretX;
      if FMouseIsInBorderOfStructure then
        SelStructure(FMouseBorderOfStructure)
      else
        SelEnd := SelStart + 1;
      CommandProcessor(EcBlockUnindent, #0, nil);
      SelEnd := SelStart;
      CaretX := X - TabWidth;
    end;
    TabWidth := FConfiguration.TabWidth;
  end;
end;

procedure TFEditForm.Indent;
begin
  with FEditor do
  begin
    TabWidth := FConfiguration.Indent;
    if SelAvail then
      CommandProcessor(EcBlockIndent, #0, nil)
    else
    begin
      var
      X := CaretX;
      if FMouseIsInBorderOfStructure then
        SelStructure(FMouseBorderOfStructure)
      else
        SelEnd := SelStart + 1;
      CommandProcessor(EcBlockIndent, #0, nil);
      SelEnd := SelStart;
      CaretX := X + TabWidth;
    end;
    TabWidth := FConfiguration.TabWidth;
  end;
end;

procedure TFEditForm.SystemOutPrintln;
begin
  PutText('System.out.println(|);');
end;

procedure TFEditForm.TVFileStructureChange(Sender: TObject; Node: TTreeNode);
begin
  var
  Line := TInteger(Node.Data).Int;
  with FEditor do
  begin
    CaretY := Line;
    CaretX := Length(Lines[Line - 1]) + 1;
    EnsureCursorPosVisible;
    if CanFocus then
      SetFocus;
  end;
end;

procedure TFEditForm.Matchbracket;
begin
  FEditor.CommandProcessor(EcMatchBracket, #0, nil);
end;

function TFEditForm.GetIndent: string;
var
  Ind: Integer;
begin
  if FEditor.SelText = '' then
    Ind := FEditor.CaretX - 1
  else
    Ind := FEditor.BlockBegin.Char;
  Result := StringOfChar(' ', Ind);
end;

procedure TFEditForm.GotoLine(Line: Integer);
begin
  if Line = 0 then
    Exit;
  FEditor.TopLine := Line;
  FEditor.CaretX := 1;
  FEditor.CaretY := Line;
  FEditor.EnsureCursorPosVisible;
end;

procedure TFEditForm.Undo;
begin
  FEditor.Undo;
  if FEditor.CanFocus then
    FEditor.SetFocus;
end;

procedure TFEditForm.MIUndoClick(Sender: TObject);
begin
  Undo;
end;

procedure TFEditForm.Redo;
begin
  FEditor.Redo;
  if FEditor.CanFocus then
    FEditor.SetFocus;
end;

procedure TFEditForm.MIRedoClick(Sender: TObject);
begin
  Redo;
end;

procedure TFEditForm.MICutClick(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TFEditForm.CutToClipboard;
begin
  if (FJava.ActiveTool = 17) and Assigned(Partner) then
    Partner.CutToClipboard
  else
  begin
    with FEditor do
      if not SelAvail and FMouseIsInBorderOfStructure then
        SelStructure(FMouseBorderOfStructure);
    FEditor.CutToClipboard;
  end;
end;

procedure TFEditForm.MIGitCommitClick(Sender: TObject);
var
  AMessage: string;
begin
  if InputQuery('Commit', 'Message', AMessage) then
    FGit.GitCall('commit -m "' + AMessage + '"', ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIConfigurationClick(Sender: TObject);
begin
  FConfiguration.OpenAndShowPage('FEditor');
end;

procedure TFEditForm.MICopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TFEditForm.MICopyPathClick(Sender: TObject);
begin
  Clipboard.AsText := Pathname;
end;

procedure TFEditForm.MICreateStructogramClick(Sender: TObject);
var
  SelText, FileName: string;
  Scanner: TJavaScanner;
begin
  if FEditor.SelAvail then
    SelText := FEditor.SelText
  else if FMouseIsInBorderOfStructure then
  begin
    FEditor.SelStructure(FMouseBorderOfStructure);
    SelText := FEditor.SelText;
    FEditor.SelEnd := FEditor.SelStart;
  end;
  if SelText <> '' then
  begin
    Scanner := TJavaScanner.Create;
    Scanner.Init(SelText);
    FileName := Scanner.GetFilename;
    FreeAndNil(Scanner);
    if FileName <> '' then
      FileName := ExtractFilePath(Pathname) + FileName + '.jsg'
    else
      FileName := ChangeFileExt(Pathname, '.jsg');
    FJava.StructogramFromText(SelText, FileName);
  end;
end;

procedure TFEditForm.CopyToClipboard;
begin
  if (FJava.ActiveTool = 17) and Assigned(Partner) then
    Partner.CopyToClipboard
  else
  begin
    with FEditor do
      if not SelAvail and FMouseIsInBorderOfStructure then
        SelStructure(FMouseBorderOfStructure);
    try
      FEditor.CopyToClipboard;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  end;
end;

procedure TFEditForm.MIInsertClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TFEditForm.PasteFromClipboard;
begin
  if (FJava.ActiveTool = 17) and Assigned(Partner) then
    Partner.PasteFromClipboard
  else
  begin
    try
      FEditor.PasteFromClipboard;
      Modified := True;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
    ParseSourceCode(True);
  end;
end;

procedure TFEditForm.PopUpEditorPopup(Sender: TObject);
begin
  FMouseBorderOfStructure := 0;
  FMouseIsInBorderOfStructure := FEditor.MouseInBorderOfStructure
    (FMouseBorderOfStructure);
  if not FEditor.GetPositionOfMouse(FMousePosition) then
    FMousePosition.Char := -1;
  UpdateState;
  MISearchDeclaration.Visible := IsJava;
  MIAPIHelp.Visible := IsJava;
  MIClassOpen.Visible := IsJava;
  MIClassEditor.Visible := IsJava;
  MICreateStructogram.Visible := IsJava;
  MIExecute.Visible := IsJava;
  MIExecuteWithoutConsole.Visible := IsJava;
  MIExecuteWithConsole.Visible := IsJava;
  MIRenewImports.Visible := IsJava;
  MIGit.Visible := FConfiguration.GitOK;

  // Assistant actions
  var
  EditForm := FJava.GetActiveEditor;
  var
  HasJavaFile := Assigned(EditForm) and EditForm.IsJava;
  mnAssistanSuggest.Enabled := HasJavaFile and not FEditor.SelAvail and
    not LLMAssistant.IsBusy;
  mnAssistantOptimize.Enabled := HasJavaFile and FEditor.SelAvail and
    not LLMAssistant.IsBusy;
  mnAssistantFixBugs.Enabled := HasJavaFile and FEditor.SelAvail and
    not LLMAssistant.IsBusy;
  mnAssistantExplain.Enabled := HasJavaFile and FEditor.SelAvail and
    not LLMAssistant.IsBusy;
  mnAssistantCancel.Enabled := LLMAssistant.IsBusy;
end;

procedure TFEditForm.MIUnindentClick(Sender: TObject);
begin
  Unindent;
end;

procedure TFEditForm.mnAssistanSuggestClick(Sender: TObject);
begin
  var
  EditForm := FJava.GetActiveEditor;
  if Assigned(EditForm) then
  begin
    FJava.ScpJava.CancelCompletion;
    FJava.ScpParams.CancelCompletion;
    LLMAssistant.Suggest;
  end;
end;

procedure TFEditForm.mnAssistantCancelClick(Sender: TObject);
begin
  if LLMAssistant.IsBusy then
    LLMAssistant.CancelRequest;
end;

procedure TFEditForm.mnAssistantExplainClick(Sender: TObject);
begin
  LLMAssistant.Explain;
end;

procedure TFEditForm.mnAssistantFixBugsClick(Sender: TObject);
begin
  LLMAssistant.FixBugs;
end;

procedure TFEditForm.mnAssistantOptimizeClick(Sender: TObject);
begin
  LLMAssistant.Optimize;
end;

procedure TFEditForm.MIIndentClick(Sender: TObject);
begin
  Indent;
end;

procedure TFEditForm.MIExecuteClick(Sender: TObject);
begin
  var
  Str := FEditor.SelText;
  if (Str = '') or not IsJava then
    FJava.TBRunClick(Self)
  else
    FMessages.Execute(Str);
end;

procedure TFEditForm.MIExecuteWithoutConsoleClick(Sender: TObject);
begin
  MyJavaCommands.ConsoleMode := 1;
  FJava.TBRunClick(Self);
end;

procedure TFEditForm.MIExecuteWithConsoleClick(Sender: TObject);
begin
  MyJavaCommands.ConsoleMode := 2;
  FJava.TBRunClick(Self);
end;

procedure TFEditForm.EditorKeyPress(Sender: TObject; var Key: Char);
var
  Str: string;
  Int: Integer;
  Empty: Boolean;
begin
  if Key = '{' then
  begin
    Str := FEditor.LineText;
    Empty := (Trim(Str) = '');
    Int := 0;
    while (Int < Length(Str)) and (Str[Int + 1] = ' ') do
      Inc(Int);
    if FConfiguration.IndentAfterBracket and not FConfiguration.AddClosingBracket
    then
      Str := '{' + #13#10 + StringOfChar(' ', Int + FConfiguration.Indent) + '|'
    else if FConfiguration.AddClosingBracket then
    begin
      Str := '{' + #13#10 + StringOfChar(' ', Int + FConfiguration.Indent) +
        '|'#13#10 + StringOfChar(' ', Int) + '}';
      if not Empty then
        Str := Str + #13#10;
    end
    else
      Str := '{|';
    PutText(Str);
    Key := #0;
  end;
  if Key = #27 then
  begin
    FTooltip.Hide;
    FJava.ScpJava.Form.Hide;
    FJava.ScpParams.Form.Hide;
  end;
  if Assigned(FParseThread) and (FParseThread.State > 0) and
    (FParseThread.State < 3) then
    FParseThread.Abort := True;
  // debugging: FJava.Memo1.Lines.Add('EditorKeypress');
end;

function BufferToPoint(Buff: TBufferCoord): TPoint;
begin
  Result.X := Buff.Char;
  Result.Y := Buff.Line;
end;

function PointToBuffer(Posi: TPoint): TBufferCoord;
begin
  Result.Char := Posi.X;
  Result.Line := Posi.Y;
end;

procedure TFEditForm.EditorKeyUp(Sender: TObject; var Key: Word;
Shift: TShiftState);
const
  ControlStruc: array [1 .. 8] of string = ('if', 'while', 'for', 'do',
    'switch', 'try', 'else', '} else');
var
  Kuerzel: TNode;
  Str, Str1, Hex: string;
  Posi, Posi1, Int, Tokentyp, Start: Integer;
  Posi2: TPoint;
  Attri: TSynHighlighterAttributes;
begin
  if ((ssCtrl in Shift) and (Key = Ord('V'))) or
    ((ssShift in Shift) and (Key = VK_INSERT)) then
    FEditor.ReplaceTabs(FConfiguration.TabWidth);

  // Shortcuts
  if ((VK_F1 <= Key) and (Key <= VK_F12)) or (ssCtrl in Shift) or
    (ssShift in Shift) or (ssAlt in Shift) then
  begin
    Kuerzel := FConfiguration.KeyboardShortcutsTree.GetNode
      (ShortCut(Key, Shift));
    if Assigned(Kuerzel) then
      PutText(Kuerzel.Data);
  end;

  // show tooltip
  if (Key = VK_F2) and FConfiguration.TooltipWithKey and IsJava then
  begin
    FEditor.GetHighlighterAttriAtRowColEx(FEditor.CaretXY, Str, Tokentyp,
      Start, Attri);
    if Tokentyp = Ord(SynHighlighterJava.tkIdentifier) then
    begin
      Posi2 := FEditor.RowColumnToPixels
        (DisplayCoord(Start, FEditor.LineToRow(FEditor.CaretY + 1)));
      CreateTooltip(BufferToPoint(FEditor.CaretXY),
        FEditor.ClientToScreen(Posi2), '# VK_F2 #');
    end;
  end;

  // Unicode
  if ((ssCtrl in Shift) and (ssAlt in Shift) and (Key = Ord('U'))) then
  begin
    Str := FEditor.LineText;
    Posi := FEditor.CaretX;
    if Posi < 5 then
      Hex := Copy(Str, 1, Posi - 1)
    else
      Hex := Copy(Str, Posi - 4, 4);
    Posi1 := Pos(' ', Hex);
    while (0 < Posi1) and (Posi1 < Length(Hex)) do
    begin
      Delete(Hex, 1, Posi1);
      Posi1 := Pos(' ', Hex);
    end;
    Posi1 := Length(Hex);
    try
      Insert(Char(StrToInt('$' + Hex)), Str, Posi);
      Delete(Str, Posi - Posi1, Posi1);
      FEditor.LineText := Str;
      FEditor.CaretX := FEditor.CaretX - Posi1 + 1;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
    Key := 0;
  end;

  // control structures
  if FConfiguration.InsertControlStructures and IsJava and
    ((Key = Ord(' ')) or ((ssShift in Shift) and (Key = Ord('8')))) then
  begin
    Str := Trim(FEditor.LineText);
    case Key of
      Ord(' '):
        Str1 := '';
      Ord('8'):
        Str1 := '(';
    end;
    for var I := 1 to 8 do
      if Str = ControlStruc[I] + Str1 then
      begin
        Posi := Pos(Str, FEditor.LineText);
        if I > 1 then
        begin
          FEditor.LineText := '';
          FEditor.CaretX := Posi;
        end;
        FTemplates.SBControlStructures(Self, I, True);
        Key := 0;
        Break;
      end;
  end;

  // semicolons
  if FConfiguration.InsertSemicolons and IsJava and (Key = VK_RETURN) then
  begin
    Int := FEditor.CaretY - 2;
    Str := FEditor.Lines[Int];
    if (Str <> '') and (Pos(';', Str) = 0) and FMessages.NeedsSemicolon(Str)
    then
      FEditor.Lines[Int] := Str + ';';
  end;

end;

// compare to procedure TFUMLForm.CreateTVFileStructure;
procedure TFEditForm.CreateTVFileStructure;
var
  ClassIt, Ite: IModelIterator;
  Cent: TClassifier;
  Attribute: TAttribute;
  Method: TOperation;
  ImageNr, Indented, IndentedOld: Integer;
  CName: string;
  Node: TTreeNode;
  ClassNode: TTreeNode;
  AInteger: TInteger;

  function CalculateIndented(const AClassname: string): Integer;
  begin
    Result := 0;
    for var I := 1 to Length(AClassname) do
      if CharInSet(AClassname[I], ['$', '.']) then
        Inc(Result);
  end;

begin
  Indented := 0;
  ClassNode := nil;
  if not IsJava then
    Exit;
  TVFileStructure.Items.BeginUpdate;
  try
    for var I := TVFileStructure.Items.Count - 1 downto 0 do
    begin
      AInteger := TInteger(TVFileStructure.Items[I].Data);
      FreeAndNil(AInteger);
    end;
    TVFileStructure.Items.Clear;
    ClassIt := FModel.ModelRoot.GetAllClassifiers;
    while ClassIt.HasNext do
    begin
      Cent := TClassifier(ClassIt.Next);
      if Cent.Pathname <> Pathname then
        Continue;
      if EndsWith(Cent.Name, '[]') then
        Continue;

      if (Cent is TClass) then
        FIsJUnitTestClass := (Cent as TClass).IsJUnitTestClass
      else
        FIsJUnitTestClass := False;

      CName := Cent.ShortName;
      IndentedOld := Indented;
      Indented := CalculateIndented(CName);
      while Pos('$', CName) + Pos('.', CName) > 0 do
      begin
        Delete(CName, 1, Pos('$', CName));
        Delete(CName, 1, Pos('.', CName));
      end;

      if (Cent is TClass) then
        ImageNr := 1
      else
        ImageNr := 11;

      if Indented = 0 then
        ClassNode := TVFileStructure.Items.AddObject(nil, CName,
          TInteger.Create(Cent.Lines))
      else if Indented > IndentedOld then
        ClassNode := TVFileStructure.Items.AddChildObject(ClassNode, CName,
          TInteger.Create(Cent.Lines))
      else
      begin
        while Indented <= IndentedOld do
        begin
          Dec(IndentedOld);
          ClassNode := ClassNode.Parent;
        end;
        ClassNode := TVFileStructure.Items.AddChildObject(ClassNode, CName,
          TInteger.Create(Cent.Lines));
      end;

      ClassNode.ImageIndex := ImageNr;
      ClassNode.SelectedIndex := ImageNr;
      ClassNode.HasChildren := True;

      Ite := Cent.GetAttributes;
      while Ite.HasNext do
      begin
        Attribute := Ite.Next as TAttribute;
        ImageNr := Integer(Attribute.Visibility) + 2;
        Node := TVFileStructure.Items.AddChildObject(ClassNode,
          Attribute.ToTypeName, TInteger.Create(Attribute.Lines));
        Node.ImageIndex := ImageNr;
        Node.SelectedIndex := ImageNr;
        Node.HasChildren := False;
      end;
      Ite := Cent.GetOperations;
      while Ite.HasNext do
      begin
        Method := Ite.Next as TOperation;
        if Method.OperationType = otConstructor then
          ImageNr := 6
        else
          ImageNr := Integer(Method.Visibility) + 7;
        Node := TVFileStructure.Items.AddChildObject(ClassNode,
          Method.ToTypeName, TInteger.Create(Method.Lines));
        Node.ImageIndex := ImageNr;
        Node.SelectedIndex := ImageNr;
        Node.HasChildren := False;
      end;
    end;
  finally
    TVFileStructure.Items.EndUpdate;
    FFileStructure.InitWithItems(TVFileStructure.Items, Self);
  end;
end;

procedure TFEditForm.RunTests;
begin
  if not Assigned(FJUnitTests) then
    FJUnitTests := TFJUnitTests.Create(FJava);
  FJUnitTests.Pathname := Pathname;
  var
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  if ClassIt.HasNext then
  begin
    var
    Cent := TClassifier(ClassIt.Next);
    if (Cent is TClass) and (Cent as TClass).IsJUnitTestClass then
      MyJavaCommands.RunTests(Cent as TClass, 'Class');
  end;
end;

function TFEditForm.GetLNG(Num, ClassNumber: Integer): string;
begin
  if ClassNumber = 0 then
    Result := LNGs[Num]
  else
    Result := LNGs[Num] + IntToStr(ClassNumber);
end;

procedure TFEditForm.InsertStartEnd;
var
  ClassIt, Ite: IModelIterator;
  Cent: TClassifier;
  Attribute: TAttribute;
  Method: TOperation;
  Lines: array [1 .. 6] of Integer;
  LNG: array [1 .. 6] of string;
  CaretXY: TBufferCoord;
  Ind, Str1, Str2: string;
  ClassNumber, CentLineE, CentLineS, Line: Integer;
  StringList: TStringList;

  procedure CheckConstructorAndComponentsPosition;
  var
    Line: Integer;
  begin
    if (Method.OperationType = otConstructor) or
      (IsApplet and (Method.Name = 'init')) or
      ((FFrameType = 8) and (Method.Name = 'start')) then
    begin
      Lines[3] := Method.LineE;
      Lines[4] := Method.LineE;
      case FFrameType of
        8:
          begin
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'new Scene');
            if (-1 < Line) and (Line < Method.LineE) then
            begin
              Lines[3] := Line + 1;
              Lines[4] := Line + 1;
            end;
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'primaryStage.setOnCloseRequest');
            if (-1 < Line) and (Line < Method.LineE) then
              Lines[4] := Line;
          end;
        7:
          begin
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'cp.setBounds');
            if (-1 < Line) and (Line < Method.LineE) then
              Lines[3] := Line + 1;
            Lines[4] := Method.LineE - 1;
          end;
        6:
          begin
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'cp.setLayout(null)');
            if (-1 < Line) and (Line < Method.LineE) then
            begin
              Lines[3] := Line + 1;
              Lines[4] := Line + 1;
            end;
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'setResizable(False)');
            if (-1 < Line) and (Line < Method.LineE) then
              Lines[4] := Line;
          end;
        5:
          begin
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'cp.setLayout(null)');
            if (-1 < Line) and (Line < Method.LineE) then
            begin
              Lines[3] := Line + 1;
              Lines[4] := Line + 1;
            end;
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'setVisible(true)');
            if (-1 < Line) and (Line < Method.LineE) then
              Lines[4] := Line;
          end;
        4:
          begin
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'add(cp)');
            if (-1 < Line) and (Line < Method.LineE) then
              Lines[3] := Line + 1;
            Lines[4] := Method.LineE - 1;
          end;
        3:
          begin
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'add(cp)');
            if (-1 < Line) and (Line < Method.LineE) then
            begin
              Lines[3] := Line + 1;
              Lines[4] := Line + 1;
            end;
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'setResizable(False)');
            if (-1 < Line) and (Line < Method.LineE) then
              Lines[4] := Line;
          end;
        2:
          begin
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'add(cp)');
            if (-1 < Line) and (Line < Method.LineE) then
            begin
              Lines[3] := Line + 1;
              Lines[4] := Line + 1;
            end;
            Line := GetLineNumberWithFromTill(Method.Lines, Method.LineE,
              'setVisible(true)');
            if (-1 < Line) and (Line < Method.LineE) then
              Lines[4] := Line;
          end;
      end;
    end
    else
      Lines[5] := Min(Lines[5], Method.Lines - 1);
  end;

  procedure Add(Cha: Char; Line: Integer; const Str: string);
  begin
    StringList.Add(RightStr('000000' + IntToStr(Line), 6) + Cha + Str);
  end;

begin
  if not IsJava then
    Exit;
  FEditor.LockUndo;
  FEditor.BeginUpdate;
  for var I := 1 to 6 do
  begin
    Line := GetLineNumberWith(GetLNG(I, 0));
    if Line > -1 then
      DeleteLine(Line);
  end;

  ParseSourceCode(False);
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  CaretXY := FEditor.CaretXY;
  ClassNumber := -1;
  StringList := TStringList.Create;
  StringList.Sorted := True;
  while ClassIt.HasNext do
  begin
    Cent := TClassifier(ClassIt.Next);
    if (Cent.Pathname <> Pathname) or Cent.Anonym then
      Continue;
    Inc(ClassNumber);
    CentLineS := Cent.Lines;
    CentLineE := Max(CentLineS, Cent.LineE - 1);
    Ind := StringTimesN(FConfiguration.Indent1, Cent.ScopeDepth);
    Lines[1] := CentLineS;
    LNG[1] := GetLNG(1, ClassNumber);
    Lines[2] := CentLineS;
    LNG[2] := GetLNG(2, ClassNumber);
    Lines[3] := CentLineE;
    LNG[3] := GetLNG(3, ClassNumber);
    Lines[4] := CentLineE;
    LNG[4] := GetLNG(4, ClassNumber);
    Lines[5] := CentLineE;
    LNG[5] := GetLNG(5, ClassNumber);
    Lines[6] := CentLineE;
    LNG[6] := GetLNG(6, ClassNumber);

    Ite := Cent.GetOperations;
    if Ite.HasNext then
    begin
      Method := Ite.Next as TOperation;
      CheckConstructorAndComponentsPosition;
      while Ite.HasNext do
      begin
        Method := Ite.Next as TOperation;
        CheckConstructorAndComponentsPosition;
      end;
    end;

    Ite := Cent.GetAttributes;
    if Ite.HasNext then
    begin
      Attribute := Ite.Next as TAttribute;
      Lines[1] := Attribute.Lines - 1;
      Lines[2] := Attribute.LineE;
      while Ite.HasNext do
      begin
        Attribute := Ite.Next as TAttribute;
        Lines[2] := Attribute.LineE;
      end;
    end;

    if Lines[2] <= Lines[1] then
      Lines[2] := Lines[1];
    if Lines[4] <= Lines[3] then
      Lines[4] := Lines[3];
    if Lines[6] <= Lines[5] then
      Lines[6] := Lines[5];

    if GetLineNumberWithWord(LNG[6]) = -1 then
      Add('F', Lines[6], Ind + LNG[6] + CrLf);
    if GetLineNumberWithWord(LNG[5]) = -1 then
      Add('E', Lines[5], Ind + LNG[5] + CrLf);
    if FFrameType > 1 then
    begin
      if GetLineNumberWithWord(LNG[4]) = -1 then
        Add('D', Lines[4], Ind + Ind + LNG[4] + CrLf);
      if GetLineNumberWithWord(LNG[3]) = -1 then
        Add('C', Lines[3], Ind + Ind + LNG[3] + CrLf);
    end;
    if GetLineNumberWithWord(LNG[2]) = -1 then
      Add('B', Lines[2], Ind + LNG[2] + CrLf);
    if GetLineNumberWithWord(LNG[1]) = -1 then
      Add('A', Lines[1], Ind + LNG[1] + CrLf);
  end;

  for var I := StringList.Count - 1 downto 0 do
  begin
    Str2 := StringList[I];
    Str1 := Copy(StringList[I], 1, 6);
    Delete(Str2, 1, 7);
    InsertLinesAt(StrToInt(Str1), Str2);
  end;
  StringList.Clear;
  FreeAndNil(StringList);
  FEditor.CaretXY := CaretXY;
  FEditor.UnlockUndo;
  FEditor.EndUpdate;
end;

function TFEditForm.HasMainInModel: Boolean;
var
  ClassIt, Ite: IModelIterator;
  Cent: TModelEntity;
begin
  Result := True;
  if IsJava then
  begin
    if IsApplet then // Applets have "public void init()"
      Exit;
    ParseSourceCode(False);
    ClassIt := FModel.ModelRoot.GetAllClassifiers;
    while ClassIt.HasNext do
    begin
      Cent := ClassIt.Next;
      if (Cent is TClass) then
      begin
        Ite := (Cent as TClass).GetOperations;
        while Ite.HasNext do
          if (Ite.Next as TOperation).HasMain then
            Exit;
      end;
    end;
  end;
  Result := False;
end;

procedure TFEditForm.CollectClasses(StringList: TStringList);
begin
  if IsJava then
  begin
    ParseSourceCode(False);
    var
    ClassIt := FModel.ModelRoot.GetAllClassifiers;
    while ClassIt.HasNext do
    begin
      var
      Cent := ClassIt.Next;
      if (Cent is TClass) or (Cent is TInterface) then
        StringList.Add(WithoutArray(Cent.Name));
    end;
  end;
end;

function TFEditForm.CBSearchClassOrMethod(Stop: Boolean; Line: Integer): string;
var
  AClassname, AMethodname: string;
  ClassIt, Ite: IModelIterator;
  Cent: TClassifier;
  Method: TOperation;
  Found: Boolean;
  ClassInLine: Integer;
begin
  // ParseSourcecode;  to be done by caller
  Result := '';
  if Trim(FEditor.Lines[Line - 1]) = '' then
    Exit('');

  Found := False;
  ClassInLine := -1;
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  while ClassIt.HasNext and not Found do
  begin
    Cent := TClassifier(ClassIt.Next);
    if Cent.Pathname = Pathname then
    begin
      if Cent.Lines > Line then
        Break;
      if (Cent.Lines <= Line) and (Line <= Cent.LineE) then
      begin
        AClassname := Cent.Name;
        ClassInLine := Cent.Lines;
        AMethodname := '';
        Ite := Cent.GetOperations;
        while Ite.HasNext and not Found do
        begin
          Method := Ite.Next as TOperation;
          if Method.Lines = Line then
          begin
            AMethodname := Method.Name;
            Found := True;
          end;
        end;
      end;
    end;
  end;
  if AClassname = '' then
    Exit;

  if Stop then
  begin
    if Found then
      Result := 'stop in ' + AClassname + '.' + AMethodname
    else if Line > ClassInLine then
      Result := 'stop at ' + AClassname + ':' + IntToStr(Line);
  end
  else // clear
    if Found then
      Result := 'clear ' + AClassname + '.' + AMethodname
    else if Line > ClassInLine then
      Result := 'clear ' + AClassname + ':' + IntToStr(Line);
end;

function TFEditForm.SourceContainsClass(const AClassname: string): Boolean;
begin
  Result := False;
  if not Assigned(FModel) then
    ParseSourceCode(True);

  var
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  while ClassIt.HasNext and not Result do
  begin
    var
    Cent := TClassifier(ClassIt.Next);
    if (Cent is TClass) and ((Cent.ShortName = AClassname) or
      (Cent.Name = AClassname)) and (Cent.Pathname = Pathname) then
      Result := True;
  end;
end;

procedure TFEditForm.MIClassEditorClick(Sender: TObject);
begin
  SBClassEditClick(Self);
end;

procedure TFEditForm.MIClassOpenClick(Sender: TObject);
begin
  SBClassOpenClick(Self);
end;

procedure TFEditForm.MICloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFEditForm.MIAPIHelpClick(Sender: TObject);
begin
  FJava.SearchInIndex;
end;

procedure TFEditForm.ExportToFile(const FileName: string;
Exporter: TSynCustomExporter);
begin
  with Exporter do
  begin
    Highlighter := FEditor.Highlighter;
    ExportAsText := True;
    ExportAll(FEditor.Lines);
    try
      SaveToFile(FileName);
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  end;
end;

procedure TFEditForm.ExportToClipboard(AsHtml: Boolean; AsText: Boolean);
var
  Exporter: TSynCustomExporter;
begin
  if AsHtml then
  begin
    Exporter := TSynExporterHTML.Create(Self);
    if not AsText then
      TSynExporterHTML(Exporter).CreateHTMLFragment := True;
  end
  else
    Exporter := TSynExporterRTF.Create(Self);
  var
  Lines := TStringList.Create;
  Lines.Text := FEditor.SelText;
  with Exporter do
  begin
    Highlighter := FEditor.Highlighter;
    ExportAsText := AsText;
    ExportAll(Lines);
    Exporter.CopyToClipboard;
  end;
  FreeAndNil(Lines);
  FreeAndNil(Exporter);
end;

function getFormatted(Num, Arity: Integer): string;
begin
  var
  Str := IntToStr(Num);
  while Length(Str) < Arity do
    Str := '0' + Str;
  Result := Str + ' ';
end;

procedure TFEditForm.ExportRTFNumbered;
begin
  var
  Exporter := TSynExporterRTF.Create(Self);
  var
  Lines := TStringList.Create;
  Lines.Text := FEditor.SelText;

  var
  Arity := 3;
  if Lines.Count < 100 then
    Arity := 2;
  if Lines.Count < 10 then
    Arity := 1;
  for var I := 0 to Lines.Count - 1 do
    Lines[I] := getFormatted(I + 1, Arity) + Lines[I];
  with Exporter do
  begin
    Highlighter := FEditor.Highlighter;
    Font.Assign(FEditor.Font);
    ExportAsText := False;
    ExportAll(Lines);
    Exporter.CopyToClipboard;
  end;
  FreeAndNil(Lines);
  FreeAndNil(Exporter);
end;

procedure TFEditForm.ExportWithNumbers;
begin
  var
  Lines := TStringList.Create;
  Lines.Text := FEditor.SelText;
  var
  Arity := 3;
  if Lines.Count < 100 then
    Arity := 2;
  if Lines.Count < 10 then
    Arity := 1;
  for var I := 0 to Lines.Count - 1 do
    Lines[I] := getFormatted(I + 1, Arity) + Lines[I];
  Clipboard.AsText := Lines.Text;
  FreeAndNil(Lines);
end;

procedure TFEditForm.DoExport;
var
  Folder: string;
  Exporter: TSynCustomExporter;
begin
  with FJava.SDSaveAs do
  begin
    Title := _(LNGExportTo);
    Filter := 'RTF (*.rtf)|*.rtf|HTML (*.html)|*.html;*.htm';
    FileName := GetSaveAsName;
    Folder := ExtractFilePath(FileName);
    FileName := ChangeFileExt(FileName, '');
    if Folder <> '' then
      InitialDir := Folder
    else
      InitialDir := FConfiguration.Sourcepath;
    if Execute then
    begin
      if ExtractFileExt(FileName) = '' then
        case FilterIndex of
          1:
            FileName := FileName + '.rtf';
          2:
            FileName := FileName + '.html';
        end;
      if not FileExists(FileName) or FileExists(FileName) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
        mbYesNoCancel, 0) = mrYes) then
      begin
        if LowerCase(ExtractFileExt(FileName)) = '.rtf' then
          Exporter := TSynExporterRTF.Create(Self)
        else
          Exporter := TSynExporterHTML.Create(Self);
        Exporter.Font.Assign(FEditor.Font);
        ExportToFile(FileName, Exporter);
        FreeAndNil(Exporter);
        if CanFocus then
          SetFocus;
        FConfiguration.Sourcepath := ExtractFilePath(FileName);
      end;
    end;
  end;
end;

procedure TFEditForm.EditorPaintTransient(Sender: TObject; ACanvas: TCanvas;
TransientType: TTransientType);
const
  BracketSet = ['{', '[', '(', '}', ']', ')'];
  OpenChars: array [0 .. 2] of Char = ('{', '[', '(');
  CloseChars: array [0 .. 2] of Char = ('}', ']', ')');

var
  Posi, Posi1, Posi2, Pix: TBufferCoord;
  Str: string;
  VStructure: Integer;
  Attri: TSynHighlighterAttributes;

  function CharToPixels(Posi: TBufferCoord): TBufferCoord;
  begin
    Result := PointToBuffer(FEditor.RowColumnToPixels(DisplayCoord(Posi.Char,
      Posi.Line)));
    Result.Line := Result.Line - 1;
  end;

begin
  if not Assigned(FEditor) or not Assigned(FEditor.Highlighter) then
    Exit;
  Posi := FEditor.CaretXY;
  FEditor.GetHighlighterAttriAtRowCol(Posi, Str, Attri);

  // If you want to be able to highlight on either side of the bracket, uncomment this block of text
  // Check to see if we need to go back a char;
  if (Str = '') or ((Length(Str) > 0) and not CharInSet(Str[1], BracketSet))
  then
  begin
    Posi.Char := Posi.Char - 1;
    if Posi.Char <= 0 then
      Exit;
    FEditor.GetHighlighterAttriAtRowCol(Posi, Str, Attri);
  end;

  if (FEditor.CaretX <= Length(FEditor.LineText) + 1) and
    (FEditor.Highlighter.SymbolAttribute = Attri) then
  begin
    for var I := 0 to 2 do
      if (Str = OpenChars[I]) or (Str = CloseChars[I]) then
      begin
        Posi2 := Posi;
        Posi2.Line := FEditor.LineToRow(Posi.Line); // regard wordwrap
        Posi1 := FEditor.GetMatchingBracketEx(Posi);
        Posi1.Line := FEditor.LineToRow(Posi1.Line);
        Pix := CharToPixels(Posi2);
        FEditor.Canvas.Brush.Style := bsClear;
        FEditor.Canvas.Font.Assign(FEditor.Font);

        if (TransientType = ttAfter) then
        begin
          FEditor.Canvas.Font.Color := FConfiguration.AttrBrackets.Foreground;
          FEditor.Canvas.Font.Style := FConfiguration.AttrBrackets.Style;
          FEditor.Canvas.Brush.Color := FConfiguration.AttrBrackets.Background;
        end
        else
        begin
          if Attri.Foreground <> clNone then
            FEditor.Canvas.Font.Color := Attri.Foreground
          else
            FEditor.Canvas.Font.Color := FEditor.Font.Color;
          VStructure := FEditor.GetStructureIndex(FEditor.CaretY);
          if VStructure = -1 then
            if Attri.Background <> clNone then
              FEditor.Canvas.Brush.Color := Attri.Background
            else
              FEditor.Canvas.Brush.Color := FEditor.Color
          else
            FEditor.Canvas.Brush.Color := FEditor.GetStructureLineColor
              (Posi.Line, VStructure);
        end;

        FEditor.Canvas.TextOut(Pix.Char, Pix.Line + 1, Str);
        if (Posi1.Char > 0) and (Posi1.Line > 0) then
        begin
          Pix := CharToPixels(Posi1);
          if Str = OpenChars[I] then
            FEditor.Canvas.TextOut(Pix.Char, Pix.Line + 1, CloseChars[I])
          else
            FEditor.Canvas.TextOut(Pix.Char, Pix.Line + 1, OpenChars[I]);
        end;
      end;
    FEditor.Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TFEditForm.SBParagraphClick(Sender: TObject);
begin
  if FEditor.VisibleSpecialChars = [] then
  begin
    FEditor.VisibleSpecialChars := [scWhitespace, scControlChars, scEOL];
    TBParagraph.Down := True;
  end
  else
  begin
    FEditor.VisibleSpecialChars := [];
    TBParagraph.Down := False;
  end;
end;

procedure TFEditForm.SBNumbersClick(Sender: TObject);
begin
  FEditor.Gutter.ShowLineNumbers := not FEditor.Gutter.ShowLineNumbers;
end;

procedure TFEditForm.SBSystemOutPrintlnClick(Sender: TObject);
begin
  SystemOutPrintln;
end;

procedure TFEditForm.SBMatchBracketClick(Sender: TObject);
begin
  Matchbracket;
end;

procedure TFEditForm.SBBookmarkClick(Sender: TObject);
begin
  with FEditor do
    SetDeleteBookmark(CaretX, CaretY);
end;

procedure TFEditForm.SBGotoBookmarkClick(Sender: TObject);
begin
  var
  OldNumber := FBookmark;
  repeat
    if FEditor.IsBookmark(FBookmark) then
    begin
      FEditor.GotoBookMark(FBookmark);
      FBookmark := (FBookmark + 1) mod 10;
      Exit;
    end;
    FBookmark := (FBookmark + 1) mod 10;
  until OldNumber = FBookmark;
end;

procedure TFEditForm.SBStatementClick(Sender: TObject);
begin
  FTemplates.SBControlStructures(Self, (Sender as TToolButton).Tag);
end;

procedure TFEditForm.SBIndentClick(Sender: TObject);
begin
  Indent;
end;

procedure TFEditForm.SBUnindentClick(Sender: TObject);
begin
  Unindent;
end;

procedure TFEditForm.SBWordWrapClick(Sender: TObject);
begin
  if FEditor.WordWrap then
  begin
    FEditor.WordWrap := False;
    FEditor.UseCodeFolding := True;
  end
  else
  begin
    FEditor.UseCodeFolding := False;
    FEditor.WordWrap := True;
  end;
  TBParagraph.Down := FEditor.WordWrap;
end;

procedure TFEditForm.SBZoomInClick(Sender: TObject);
begin
  SetFontSize(+1);
end;

procedure TFEditForm.SBZoomOutClick(Sender: TObject);
begin
  SetFontSize(-1);
end;

procedure TFEditForm.SBBreakpointClick(Sender: TObject);
begin
  if IsJava then
    InsertBreakpoint;
end;

procedure TFEditForm.SBBreakpointsClearClick(Sender: TObject);
begin
  if IsJava then
    ClearBreakpoints;
end;

procedure TFEditForm.SBCommentClick(Sender: TObject);
var
  Posi, From, Till: Integer;
  Str: string;
  Posi1, Posi2: TBufferCoord;
begin
  with FEditor do
  begin
    BeginUpdate;
    if SelAvail then
    begin
      From := BlockBegin.Line;
      Till := BlockEnd.Line;
      Posi1 := BlockBegin;
      Posi2 := BlockEnd;
    end
    else
    begin
      From := CaretY;
      Posi1.Char := CaretX;
      Posi1.Line := CaretY;
      Till := CaretY;
      Posi2.Char := 0;
    end;
    for var I := From to Till do
    begin
      Str := Lines[I - 1];
      Posi := Pos('//', Str);
      if Posi = 1 then
        Delete(Str, Posi, 2)
      else
        Str := '//' + Str;
      Lines[I - 1] := Str;
    end;
    if Posi2.Char <> 0 then
    begin
      BlockBegin := Posi1;
      BlockEnd := Posi2;
    end;
    EndUpdate;
  end;
  Modified := True;
  FEditor.InvalidateLines(From, Till);
end;

procedure TFEditForm.SBStructureIndentClick(Sender: TObject);
var
  ClassIt, Ite: IModelIterator;
  Cent: TClassifier;
  Method: TOperation;
  Attribute: TAttribute;
  AChanged: Boolean;

  procedure IndentLine(Num: Integer);
  var
    Int, Len, Posi: Integer;
    Str: string;
  begin
    if (Num <= 0) or (Num > FEditor.Lines.Count) then
      Exit;
    Int := FEditor.GetStructureIndent(Num);
    if Int >= 0 then
    begin
      Posi := 1;
      Str := FEditor.Lines[Num - 1];
      Len := Length(Str);
      while (Posi <= Len) and (Str[Posi] <= ' ') do
        Inc(Posi);
      Dec(Posi);
      if Posi < Int then
      begin
        FEditor.Lines[Num - 1] := StringOfChar(' ', Int - Posi) + Str;
        AChanged := True;
      end
      else if Posi > Int then
      begin
        FEditor.Lines[Num - 1] := Copy(Str, Posi - Int + 1, Len);
        AChanged := True;
      end;
    end;
  end;

begin
  Screen.Cursor := crHourGlass;
  AChanged := False;
  with FEditor do
  begin
    BeginUpdate;
    LockBuildStructure := True;
    ClassIt := FModel.ModelRoot.GetAllClassifiers;
    while ClassIt.HasNext do
    begin
      Cent := TClassifier(ClassIt.Next);
      for var I := Cent.Lines to Cent.LineSE do
        IndentLine(I);
      IndentLine(Cent.LineE);
      Ite := Cent.GetAttributes;
      while Ite.HasNext do
      begin
        Attribute := Ite.Next as TAttribute;
        IndentLine(Attribute.Lines);
      end;
      Ite := Cent.GetOperations;
      while Ite.HasNext do
      begin
        Method := Ite.Next as TOperation;
        for var I := Method.Lines to Method.LineE do
          IndentLine(I);
      end;
    end;
    LockBuildStructure := False;
    EndUpdate;
    Invalidate;
  end;
  Modified := Modified or AChanged;
  Screen.Cursor := crDefault;
end;

function TFEditForm.GetMarksBreakpoints: string;
begin
  var
  Str := '';
  with FEditor do
    for var I := 0 to Marks.Count - 1 do
    begin
      var
      Mark := Marks[I];
      if Mark.Visible then
        if Mark.IsBookmark then
          Str := Str + 'M' + IntToStr(Mark.Line)
        else if Mark.ImageIndex = BreakpointImageIndex then
          Str := Str + 'B' + IntToStr(Mark.Line);
    end;
  Result := Str;
end;

procedure TFEditForm.SetMarksBreakpoints(MarkBreakpoint: string);
var
  Line, Posi: Integer;
  IstBookmark: Boolean;
begin
  if MarkBreakpoint = '' then
    Exit;
  while MarkBreakpoint <> ')' do
  begin
    IstBookmark := MarkBreakpoint[1] = 'M';
    Posi := 2;
    while ('0' <= MarkBreakpoint[Posi]) and (MarkBreakpoint[Posi] <= '9') do
      Inc(Posi);
    Line := StrToInt(Copy(MarkBreakpoint, 2, Posi - 2));
    Delete(MarkBreakpoint, 1, Posi - 1);
    if IstBookmark then
      SetDeleteBookmark(1, Line)
    else
      InsertBreakpointMark(Line);
  end;
end;

function TFEditForm.GetFormType: string;
begin
  Result := '%E%';
end;

function TFEditForm.GetState: string;
begin
  var
  Str := inherited GetState;
  Str := Str + 'P' + Trim(FParameter) + '%P%' + 'O' + IntToStr(FStartOption) +
    'T' + IntToStr(FEditor.TopLine) + ')' + 'X' + IntToStr(FEditor.CaretX) + ')'
    + 'Y' + IntToStr(FEditor.CaretY) + ')' + GetMarksBreakpoints + ')';
  if FEditor.VisibleSpecialChars <> [] then
    Str := Str + 'P';
  if FEditor.Gutter.ShowLineNumbers then
    Str := Str + '#';
  if Assigned(Partner) then
    Str := Str + 'G';
  Result := Str + ')';
end;

procedure TFEditForm.SetState(var Str: string);
var
  Posi: Integer;
begin
  FEditor.BeginUpdate;
  inherited SetState(Str);
  if Copy(Str, 1, 1) = 'P' then
  begin
    Posi := Pos('%P%', Str);
    FParameter := Copy(Str, 2, Posi - 2);
    Delete(Str, 1, Posi + 2);
  end;
  if Copy(Str, 1, 1) = 'O' then
  begin
    FStartOption := StrToInt(Copy(Str, 2, 1));
    Delete(Str, 1, 2);
  end
  else
    FStartOption := 0;
  if Copy(Str, 1, 1) = 'T' then
  begin
    Posi := Pos(')', Str);
    FEditor.TopLine := StrToInt(Copy(Str, 2, Posi - 2));
    Delete(Str, 1, Posi);

    Posi := Pos(')', Str);
    FEditor.CaretX := StrToInt(Copy(Str, 2, Posi - 2));
    Delete(Str, 1, Posi);

    Posi := Pos(')', Str);
    FEditor.CaretY := StrToInt(Copy(Str, 2, Posi - 2));
    Delete(Str, 1, Posi);

    if (Length(Str) > 0) and (Str[1] = 'W') then
    begin
      Posi := Pos(')', Str);
      Delete(Str, 1, Posi);
    end;
  end;

  if (Copy(Str, 1, 1) = 'M') or (Copy(Str, 1, 1) = 'B') then
  begin
    Posi := Pos(')', Str);
    SetMarksBreakpoints(Copy(Str, 1, Posi));
    Delete(Str, 1, Posi);
  end
  else
    Delete(Str, 1, 1);

  if Copy(Str, 1, 1) = 'P' then
  begin
    SBParagraphClick(Self);
    Delete(Str, 1, 1);
  end;

  if Copy(Str, 1, 1) = '#' then
  begin
    FEditor.Gutter.ShowLineNumbers := True;
    Delete(Str, 1, 1);
  end;

  if Copy(Str, 1, 1) = 'G' then // GUI
    SBDesignformClick(Self);

  FEditor.EnsureCursorPosVisible;
  FEditor.EndUpdate;
end;

function TFEditForm.GetWidthAndHeight: TPoint;
var
  Posi, Line: Integer;
  Str: string;
begin
  Result.X := 300;
  Result.Y := 300;
  if IsApplet then
  begin
    Line := GetLineNumberWith('cp.setBounds(');
    if Line >= 0 then
    begin
      Str := FEditor.Lines[Line];
      Posi := Pos('cp.setBounds(0, 0, ', Str);
      if Posi > 0 then
      begin
        Delete(Str, 1, Posi + Length('cp.setBounds(0, 0, ') - 1);
        Posi := Pos(',', Str);
        TryStrToInt(Copy(Str, 1, Posi - 1), Result.X);
        Delete(Str, 1, Posi);
        Posi := Pos(')', Str);
        TryStrToInt(Copy(Str, 1, Posi - 1), Result.Y);
      end;
    end;
  end
  else if FFrameType = 8 then
  begin
    Line := GetLineNumberWith('new Scene(root');
    if Line >= 0 then
    begin
      Str := FEditor.Lines[Line];
      Posi := Pos('new Scene(root, ', Str);
      Delete(Str, 1, Posi + 15);
      Posi := Pos(',', Str);
      TryStrToInt(Trim(Copy(Str, 1, Posi - 1)), Result.X);
      Delete(Str, 1, Posi);
      Posi := Pos(')', Str);
      TryStrToInt(Trim(Copy(Str, 1, Posi - 1)), Result.Y);
    end;
  end
  else
  begin
    Line := GetLineNumberWith('frameWidth');
    if Line >= 0 then
    begin
      Str := FEditor.Lines[Line];
      Posi := Pos('=', Str);
      Delete(Str, 1, Posi);
      Posi := Pos(';', Str);
      Delete(Str, Posi, 255);
      TryStrToInt(Trim(Str), Result.X);
    end;
    Line := GetLineNumberWith('frameHeight');
    if Line >= 0 then
    begin
      Str := FEditor.Lines[Line];
      Posi := Pos('=', Str);
      Delete(Str, 1, Posi);
      Posi := Pos(';', Str);
      Delete(Str, Posi, 255);
      TryStrToInt(Trim(Str), Result.Y);
    end;
  end;
end;

procedure TFEditForm.ChangeWidthAndHeight(Width, Height: Integer);
begin
  ReplaceLine('int frameWidth', FConfiguration.Indent2 + 'int frameWidth = ' +
    IntToStr(Width) + ';');
  ReplaceLine('int frameHeight', FConfiguration.Indent2 + 'int frameHeight = ' +
    IntToStr(Height) + ';');
end;

procedure TFEditForm.ReplaceLine(const Str1, Str2: string);
begin
  var
  Line := GetLineNumberWith(Str1);
  if (0 <= Line) and (Line < FEditor.Lines.Count) then
  begin
    FEditor.Lines[Line] := Str2;
    Modified := True;
  end;
end;

procedure TFEditForm.ReplaceLineWith(Line: Integer; const Str: string);
begin
  if (0 <= Line) and (Line < FEditor.Lines.Count) then
  begin
    FEditor.Lines[Line] := Str;
    Modified := True;
  end;
end;

procedure TFEditForm.ReplaceLineInLine(Line: Integer; const Old, ANew: string);
begin
  if (0 <= Line) and (Line < FEditor.Lines.Count) then
  begin
    var
    Str := FEditor.Lines[Line];
    var
    Posi := Pos(Old, Str);
    if Posi = 0 then
      Str := ''
    else
      Str := Copy(Str, Posi + Length(Old), Length(Str)); // preserve comments
    FEditor.Lines[Line] := ANew + Str;
    Modified := True;
  end;
end;

procedure TFEditForm.ReplaceText(const Str1, Str2: string; All: Boolean);
var
  Line: Integer;
begin
  if Str1 = Str2 then
    Exit;
  FEditor.BeginUpdate;
  Line := GetLineNumberWith(Str1);
  if Line >= 0 then
  begin
    FEditor.Lines[Line] := ReplaceStr(FEditor.Lines[Line], Str1, Str2);
    if All then
      for var I := Line + 1 to FEditor.Lines.Count - 1 do
        if Pos(Str1, FEditor.Lines[I]) > 0 then
          FEditor.Lines[I] := ReplaceStr(FEditor.Lines[I], Str1, Str2);
    Modified := True;
  end;
  FEditor.EndUpdate;
end;

procedure TFEditForm.ReplaceTextWithRegex(const Reg, Str: string; All: Boolean;
From: Integer = -1; Till: Integer = -1);
var
  Line, Ends: Integer;
  WS1: string;
  RegEx: TRegEx;
begin
  FEditor.BeginUpdate;
  RegEx := CompiledRegEx(Reg);
  if From = -1 then
    Line := GetLNGStartAttributes
  else
    Line := From;
  if Till = -1 then
    Ends := GetLineNumberWith(_(LNGEndEventMethods))
  else
    Ends := Till;
  while Line <= Ends do
  begin
    WS1 := FEditor.Lines[Line];
    if RegEx.Matches(WS1).Count > 0 then
    begin
      FEditor.Lines[Line] := RegEx.Replace(WS1, Reg, Str);
      Modified := True;
    end;
    if not All then
      Break;
    Inc(Line);
  end;
  FEditor.EndUpdate;
end;

procedure TFEditForm.ReplaceWord(const Str1, Str2: string; All: Boolean);
var
  Line: Integer;
  WS1, RegExExpr: string;
  RegEx: TRegEx;
  Matches: TMatchCollection;
  Group: TGroup;
begin
  if (Str1 = Str2) or (Str1 = '') then
    Exit;
  FEditor.BeginUpdate;
  // '(' + s1 + ')' is Groups[1]
  RegExExpr := '\b(' + TRegEx.Escape(Str1) +
    ')(BG|TG|TB|Title|Polyline|RB\d+|Tab\d+)?';
  if not IsWordBreakChar(Str1[Length(Str1)]) then
    RegExExpr := RegExExpr + '\b';
  RegEx := CompiledRegEx(RegExExpr);
  Line := GetLineNumberWithFrom(0, Str1);
  while Line >= 0 do
  begin // for every Line
    WS1 := FEditor.Lines[Line];
    Matches := RegEx.Matches(WS1);
    for var I := Matches.Count - 1 downto 0 do
    begin
      Group := Matches[I].Groups[1];
      Delete(WS1, Group.Index, Group.Length);
      Insert(Str2, WS1, Group.Index);
    end;
    FEditor.Lines[Line] := WS1;
    Line := GetLineNumberWithFrom(Line + 1, Str1);
    Modified := True;
    if not All then
      Break;
  end;
  FEditor.EndUpdate;
end;

procedure TFEditForm.ReplaceComponentname(const Str1, Str2: string;
Events: string);
var
  Line: Integer;
  WS1, RegExExpr: string;
  RegEx: TRegEx;
  Matches: TMatchCollection;
  Group: TGroup;
begin
  if Str1 = Str2 then
    Exit;
  FEditor.BeginUpdate;
  ReplaceWord(Str1, Str2, True);
  while Pos('|', Events) = 1 do
    Delete(Events, 1, 1);
  while (Length(Events) > 0) and (Events[Length(Events)] = '|') do
    Delete(Events, Length(Events), 1);
  Events := Events + '|Action';
  Events := MakeUpperEvents(Events);

  // '(' + s1 + ')' is Groups[1]
  RegExExpr := '\b(' + TRegEx.Escape(Str1) + ')_(' + Events + ')\b';
  RegEx := CompiledRegEx(RegExExpr);
  Line := GetLineNumberWithFrom(0, Str1);
  while Line >= 0 do
  begin // for every Line
    WS1 := FEditor.Lines[Line];
    Matches := RegEx.Matches(WS1);
    for var I := Matches.Count - 1 downto 0 do
    begin
      Group := Matches[I].Groups[1];
      Delete(WS1, Group.Index, Group.Length);
      Insert(Str2, WS1, Group.Index);
    end;
    FEditor.Lines[Line] := WS1;
    Line := GetLineNumberWithFrom(Line + 1, Str1);
    Modified := True;
  end;
  FEditor.EndUpdate;
end;

procedure TFEditForm.ReplaceAttributAt(const AtLine, Key, Str: string);
begin
  with FEditor do
  begin
    BeginUpdate;
    var
    Line := GetLineNumberWith(Key);
    if Line >= 0 then
      DeleteLine(Line);
    InsertAttributAfter(AtLine, Str);
    EndUpdate;
  end;
end;

procedure TFEditForm.ReplaceAttribute(const Key, Str: string);
begin
  var
  Line := GetLineNumberWith(Key);
  if Line >= 0 then
  begin
    FEditor.Lines[Line] := Str;
    Modified := True;
  end
  else
    InsertAttribute(0, Str);
end;

procedure TFEditForm.ReplaceMethod(var Method: TOperation; const New: string);
begin
  FEditor.BeginUpdate;
  DeleteBlock(Method.Lines - 1, Method.LineE - 1);
  DeleteEmptyLines(Method.Lines - 1);
  InsertLinesAt(Method.Lines - 1, New);
  FEditor.EndUpdate;
end;

procedure TFEditForm.SetAttributValue(const Container, Key, Str: string;
After: Integer);
var
  Line, Till: Integer;
begin
  Line := GetLineNumberWith(' ' + Key);
  if FFrameType = 8 then
    Till := GetLineNumberWith('primaryStage.show()')
  else
    Till := GetLineNumberWith(_(LNGEndComponents));
  if (Line >= 0) and (Line < Till) then
  begin
    if Trim(Str) = '' then
      DeleteLine(Line)
    else
      FEditor.Lines[Line] := Str;
    Modified := True;
  end
  else
  begin
    Line := GetLineNumberWith(Container);
    if Line = -1 then
      Line := GetLineNumberWith(_(LNGEndComponents))
    else if After = 1 then
      Inc(Line);
    if Line >= 0 then
    begin
      InsertLinesAt(Line, Str);
      Modified := True;
    end;
  end;
end;

procedure TFEditForm.InsertAttributValue(const Destination, Str: string;
After: Integer);
begin
  var
  Till := GetLNGEndComponents;
  var
  Line := GetLineNumberWithWordFrom(GetLNGStartComponents, Destination);
  if (Line = -1) or (Line >= Till) then
    Line := Till
  else if After = 1 then
  begin
    Inc(Line);
    while (Line < Till) and HasComponent(Destination, Line) do
      Inc(Line);
  end;
  if Line >= 0 then
  begin
    InsertLinesAt(Line, Str);
    Modified := True;
  end;
end;

function TFEditForm.HasComponent(const Key: string; Line: Integer): Boolean;
var
  RegEx: string;
begin
  if Pos(FConfiguration.Indent2, Key) = 1 then
    RegEx := '^' + Key + '\b'
  else
    RegEx := '^[ \t]*' + Key + '\b';
  Result := TRegEx.IsMatch(FEditor.Lines[Line], RegEx);
end;

function TFEditForm.ContainsWord(const Key: string; Line: Integer): Boolean;
begin
  Result := TRegEx.IsMatch(FEditor.Lines[Line], '\b' + Key + '\b');
end;

procedure TFEditForm.ChangeAttributValue(const Key, Str: string);
begin
  var
  Line := GetLineNumberWith(' ' + Key);
  if Line >= 0 then
  begin
    FEditor.Lines[Line] := Str;
    Modified := True;
  end
  else
    SetAttributValue('', Key, Str, 0);
end;

procedure TFEditForm.ChangeAttributValue(const Container, Key, Str: string);
begin
  var
  Line := GetLineNumberWith(' ' + Key);
  if Line >= 0 then
  begin
    FEditor.Lines[Line] := Str;
    Modified := True;
  end
  else
    SetAttributValue(Container, Key, Str, 0);
end;

procedure TFEditForm.InsertAttributAfter(const AtLine, Attribute: string);
begin
  var
  Line := GetLineNumberWith(AtLine);
  if Line >= 0 then
  begin
    InsertLinesAt(Line + 1, Attribute);
    Modified := True;
  end;
end;

procedure TFEditForm.SynEditDebugInfoPaintLines(RenderTarget: ID2D1RenderTarget;
ClipR: TRect; const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean);
var
  LineHeight, YPos, Row, Line: Integer;
begin
  DoDefaultPainting := False;
  if not(FEditor.Highlighter = FConfiguration.JavaHighlighter) or
    not FEditor.Gutter.Visible then
    Exit;

  LineHeight := FEditor.LineHeight;
  for Row := FirstRow to LastRow do
  begin
    Line := FEditor.RowToLine(Row);
    if Row <> FEditor.LineToRow(Line) then
      Continue; // Wrapped Line
    for var I := 0 to FEditor.Marks.Count - 1 do
    begin
      var
      Mark := FEditor.Marks[I];
      if Mark.Line <> Line then
        Continue;
      if Mark.ImageIndex in [BreakpointImageIndex, NoBreakpointImageIndex,
        ErrorMarkIndex] then
      begin
        YPos := (LineHeight - vilBookmarksLight.Height) div 2 + LineHeight *
          (Row - FEditor.TopLine);
        ImageListDraw(RenderTarget, vilBookmarksLight,
          ClipR.Left + MulDiv(TSynGutterBand.MarginX, FCurrentPPI, 96), YPos,
          Mark.ImageIndex);
      end;
    end;
  end;
end;

procedure TFEditForm.SynEditBookmarkPaintLines(RenderTarget: ID2D1RenderTarget;
ClipR: TRect; const FirstRow, LastRow: Integer; var DoDefaultPainting: Boolean);
var
  YPos, Row, Line: Integer;
begin
  DoDefaultPainting := False;
  if not FEditor.Gutter.Visible then
    Exit;
  for Row := FirstRow to LastRow do
  begin
    Line := FEditor.RowToLine(Row);
    if Row <> FEditor.LineToRow(Line) then
      Continue; // Wrapped Line
    for var I := 0 to FEditor.Marks.Count - 1 do
    begin
      var
      Mark := FEditor.Marks[I];
      if Mark.Line <> Line then
        Continue;
      if Mark.ImageIndex < 10 then
      begin
        YPos := (FEditor.LineHeight - vilBookmarksLight.Height) div 2 +
          FEditor.LineHeight * (Row - FEditor.TopLine);
        ImageListDraw(RenderTarget, vilBookmarksLight,
          ClipR.Left + MulDiv(TSynGutterBand.MarginX, FCurrentPPI, 96), YPos,
          Mark.ImageIndex);
      end;
    end;
  end;
end;

procedure TFEditForm.SynEditGutterDebugInfoMouseCursor(Sender: TObject;
X, Y, Row, Line: Integer; var Cursor: TCursor);
begin
  Cursor := crHandPoint;
end;

procedure TFEditForm.InsertLinesAt(Line: Integer; Str: string);
var
  CarX, CarY, TopL, Num: Integer;
  Collapsed: Boolean;
begin
  if not EndsWith(Str, CrLf) then
    Str := Str + CrLf;
  with FEditor do
  begin
    BeginUpdate;
    Collapsed := (AllFoldRanges.Count > 1) and AllFoldRanges.Ranges[1]
      .Collapsed;
    CarX := CaretX;
    CarY := CaretY;
    TopL := TopLine;
    CaretY := Line + 1;
    CaretX := 1;
    PutText(Str, False);
    if CarY > Line + 1 then
    begin
      Num := CountChar(#13, Str);
      if Num = 0 then
        Num := 1;
      CarY := CarY + Num;
      TopL := TopL + Num;
    end;
    TopLine := TopL;
    CaretX := CarX;
    CaretY := CarY;
    EnsureCursorPosVisible;
    if Collapsed then
      Collapse(1);
    Modified := True;
    EndUpdate;
  end;
  Modified := True;
end;

procedure TFEditForm.InsertLinesAt(const AtLine, Str: string);
begin
  with FEditor do
  begin
    var
    Line := GetLineNumberWithWord(AtLine);
    if Line = -1 then
    begin
      EnsureStartEnd;
      Line := GetLineNumberWithWord(AtLine);
    end;
    if Line = -1 then
      ErrorMsg(_(LNGStartGUIVariables) + CrLf + _(LNGStartComponents) + CrLf +
        _(LNGStartEventMethods) + CrLf + _(LNGNotFound))
    else
      InsertLinesAt(Line, Str);
  end;
end;

procedure TFEditForm.InsertAttribute(ClassNumber: Integer; const Str: string);
begin
  InsertLinesAt(GetLNG(2, ClassNumber), Str);
end;

procedure TFEditForm.InsertAttribute(const Container, AIndent, Variable: string;
IsFX: Boolean);
var
  Str, AIn: string;
  Line: Integer;
begin
  if IsFX and (Container = 'root') or not IsFX and (Container = 'cp') then
    InsertAttribute(0, AIndent + Variable)
  else
    with FEditor do
    begin
      AIn := AIndent;
      Line := GetLineNumberWith(' ' + Container + ' ');
      if Line >= 0 then
      begin
        Inc(Line);
        Str := Lines[Line];
        while Copy(Str, 1, Length(AIn)) = AIn do
        begin
          Inc(Line);
          Str := Lines[Line];
        end;
        InsertLinesAt(Line, AIndent + Variable);
        Modified := True;
      end;
    end;
end;

procedure TFEditForm.InsertAttribute(const Container, Variable: string;
IsFX: Boolean);
var
  Str: string;
  AIn, Line, Till: Integer;
begin
  if IsFX and (Container = 'root') or not IsFX and (Container = 'cp') then
    InsertAttribute(0, Variable)
  else
    with FEditor do
    begin
      Till := GetLNGEndAttributes;
      Line := GetLineNumberWith(' ' + Container + ' ');
      if Line >= 0 then
      begin
        AIn := UUtils.GetIndent(FEditor.Lines[Line]);
        Inc(Line);
        Str := Lines[Line];
        while (UUtils.GetIndent(Str) >= AIn) and (Line < Till) do
        begin
          Inc(Line);
          Str := Lines[Line];
        end;
        InsertLinesAt(Line, Variable);
        Modified := True;
      end;
    end;
end;

procedure TFEditForm.InsertProcedure(const AProcedure: string);
begin
  InsertProcedure(0, AProcedure);
end;

procedure TFEditForm.InsertProcedure(ClassNumber: Integer;
const AProcedure: string);
begin
  InsertLinesAt(GetLNG(6, ClassNumber), AProcedure);
end;

procedure TFEditForm.InsertConstructor(ClassNumber: Integer;
const AProcedure: string);
begin
  InsertLinesAt(GetLNG(5, ClassNumber), AProcedure);
end;

procedure TFEditForm.InsertComponent(const Str: string);
begin
  InsertLinesAt(_(LNGEndComponents), Str);
end;

procedure TFEditForm.InsertListener(const Component, Listener: string);
begin
  if HasText(Listener) then
    Exit;
  with FEditor do
  begin
    BeginUpdate;
    var
    Line := GetLineNumberWith(Component);
    if Line = -1 then
    begin
      Line := GetLineNumberWith(_(LNGEndComponents));
      if Line = -1 then
      begin
        EnsureStartEnd;
        Line := GetLineNumberWith(_(LNGEndComponents));
      end;
    end;
    if Line = -1 then
      ErrorMsg(_(LNGEndComponents) + ' ' + _(LNGNotFound))
    else
      InsertLinesAt(Line, Listener);
    EndUpdate;
  end;
end;

procedure TFEditForm.InsertImport(const Package: string);
begin
  if StartsWith(Package, 'java.lang') or StartsWith(Package, 'InOut') or
    (GetPackage = Package) then
    Exit;
  with FEditor do
  begin
    var
    Line := GetLineNumberWithWord('import');
    if Line = -1 then
    begin
      Line := GetLineNumberWithWord('Package');
      if Line > -1 then
        Inc(Line)
      else
        Line := -1;
    end
    else
    begin
      while (Line < Lines.Count) and (Pos('import', Lines[Line]) > 0) do
      begin
        if Pos(Package, Lines[Line]) > 0 then
          Exit;
        Inc(Line);
      end;
      Dec(Line);
    end;
    InsertLinesAt(Line + 1, 'import ' + Package + ';' + CrLf);
    ParseSourceCode(True);
  end;
end;

procedure TFEditForm.DeleteAttribute(const Str: string);
var
  From, Till, Line: Integer;
begin
  From := GetLNGStartAttributes;
  Till := GetLNGEndAttributes;
  Line := GetLineNumberWithWordFrom(From, Str);
  if (0 <= Line) and (Line < Till) then
  begin
    DeleteLine(Line);
    Modified := True;
  end;
end;

procedure TFEditForm.DeleteAttributeValues(const Str: string);
var
  Stop, Line: Integer;
begin
  FEditor.BeginUpdate;
  Line := GetLNGStartComponents + 1;
  Stop := GetLNGEndComponents;
  while Line < Stop do
  begin
    if Pos(Str, FEditor.Lines[Line]) > 0 then
    begin
      DeleteLine(Line);
      Modified := True;
      Dec(Stop);
    end
    else
      Inc(Line);
  end;
  FEditor.EndUpdate;
end;

function TFEditForm.DeleteAttributeValue(const Str: string): Boolean;
begin
  Result := False;
  var
  AEnd := GetLNGEndComponents;
  var
  Search := GetLineNumberWith(Str);
  if (0 <= Search) and (Search < AEnd) then
  begin
    DeleteLine(Search);
    Result := True;
    Modified := True;
  end;
end;

procedure TFEditForm.DeleteComponentValue(Str: string);
var
  From, Till, Line: Integer;
begin
  From := GetLNGStartComponents;
  Till := GetLineNumberWithFrom(From, _(LNGEndComponents));
  Line := GetLineNumberWithFromTill(From, Till, Str);
  if Line > -1 then
  begin
    DeleteLine(Line);
    Modified := True;
  end;
end;

procedure TFEditForm.DeleteEmptyLines(Line: Integer);
begin
  FEditor.BeginUpdate;
  while (Line < FEditor.Lines.Count) and (Trim(FEditor.Lines[Line]) = '') do
  begin
    DeleteLine(Line);
    Modified := True;
  end;
  FEditor.EndUpdate;
end;

function TFEditForm.GetLNGStartAttributes: Integer;
begin
  Result := GetLineNumberWith(_(LNGStartGUIVariables));
end;

function TFEditForm.GetLNGEndAttributes: Integer;
begin
  Result := GetLineNumberWith(_(LNGEndGUIVariables));
end;

function TFEditForm.GetLNGStartComponents: Integer;
begin
  Result := GetLineNumberWith(_(LNGStartComponents));
end;

function TFEditForm.GetLNGEndComponents: Integer;
begin
  Result := GetLineNumberWith(_(LNGEndComponents));
end;

function TFEditForm.GetLNGStartEventMethods: Integer;
begin
  Result := GetLineNumberWith(_(LNGStartEventMethods));
end;

function TFEditForm.GetLNGEndEventMethods: Integer;
begin
  Result := GetLineNumberWith(_(LNGEndEventMethods));
end;

function TFEditForm.GetLineNumberWith(const Str: string): Integer;
begin
  Result := GetLineNumberWithFrom(0, Str);
end;

function TFEditForm.GetLineNumberWithFrom(From: Integer;
const Str: string): Integer;
begin
  Result := -1;
  var
  Int := From;
  repeat
    if (Int < FEditor.Lines.Count) and (Pos(Str, FEditor.Lines[Int]) > 0) then
    begin
      Result := Int;
      Break;
    end;
    Inc(Int);
  until Int >= FEditor.Lines.Count;
end;

function TFEditForm.GetLineNumberWithFromTill(From, Till: Integer;
const Str: string): Integer;
begin
  Result := -1;
  var
  Int := From;
  repeat
    if (Int <= Till) and (Pos(Str, FEditor.Lines[Int]) > 0) then
    begin
      Result := Int;
      Break;
    end;
    Inc(Int);
  until Int > Till;
end;

function TFEditForm.GetLineNumberWithWord(const Str: string): Integer;
begin
  Result := GetLineNumberWithWordFrom(0, Str);
end;

function TFEditForm.GetLineNumberWithWordFrom(From: Integer;
const Str: string): Integer;
var
  Int, Posi: Integer;
  IsOk: Boolean;
  Line: string;
begin
  Result := -1;
  Int := From;
  repeat
    Int := GetLineNumberWithFrom(Int, Str);
    if Int >= 0 then
    begin
      Line := FEditor.Lines[Int];
      Posi := Pos(Str, Line);
      IsOk := True;
      if Posi > 1 then
        IsOk := IsWordBreakChar(Line[Posi - 1]);
      Posi := Posi + Length(Str);
      if Posi <= Length(Line) then
        IsOk := IsOk and IsWordBreakChar(Line[Posi]);
      if IsOk then
      begin
        Result := Int;
        Exit;
      end;
    end
    else
      Exit;
    Inc(Int);
  until Int >= FEditor.Lines.Count;
end;

function TFEditForm.GetLineNumberWithStartsWordFrom(From: Integer;
const Str: string): Integer;
var
  Int, Posi: Integer;
  IsOk: Boolean;
  Line: string;
begin
  Result := -1;
  Int := From;
  repeat
    Int := GetLineNumberWithFrom(Int, Str);
    if Int >= 0 then
    begin
      Line := FEditor.Lines[Int];
      Posi := Pos(Str, Line);
      IsOk := True;
      if Posi > 1 then
        IsOk := IsWordBreakChar(Line[Posi - 1]);
      if IsOk then
      begin
        Result := Int;
        Exit;
      end;
    end
    else
      Exit;
    Inc(Int);
  until Int >= FEditor.Lines.Count;
end;

function TFEditForm.GetSource(Lines, LineE: Integer): string;
begin
  Result := '';
  for var I := Lines to LineE do
    Result := Result + FEditor.Lines[I] + #13#10;
end;

function TFEditForm.GetLine(Line: Integer): string;
begin
  Result := FEditor.Lines[Line];
end;

procedure TFEditForm.DeleteBlock(StartLine, EndLine: Integer);
begin
  FEditor.BeginUpdate;
  for var I := EndLine downto StartLine do
    if I < FEditor.Lines.Count then
      DeleteLine(I);
  Modified := True;
  FNeedsParsing := True;
  FEditor.EndUpdate;
end;

procedure TFEditForm.DeleteLine(Line: Integer);
begin
  var
  Int := 0;
  while Int < FEditor.Marks.Count do
  begin
    if FEditor.Marks[Int].Line = Line then
      DeleteBreakpointAtLine(Line);
    Inc(Int);
  end;
  var
  Collapsed := (FEditor.AllFoldRanges.Count > 1) and
    FEditor.AllFoldRanges.Ranges[1].Collapsed;
  FEditor.CaretY := Line + 1;
  FEditor.CommandProcessor(CecDeleteLine, #0, nil);
  if Collapsed then
    FEditor.Collapse(1);
end;

procedure TFEditForm.MoveBlock(From, Till, Dest, DestTill: Integer;
const Blanklines: string);

  procedure DeleteBlanklines(Int: Integer);
  var
    Str: string;
  begin
    if Int > 0 then
    begin
      Str := Trim(FEditor.Lines[Int - 1]) + Trim(FEditor.Lines[Int]);
      if Str = '' then
        DeleteLine(Int);
      Str := Trim(FEditor.Lines[Int - 1]) + Trim(FEditor.Lines[Int]);
      if Str = '' then
        DeleteLine(Int);
    end
    else if (Int < FEditor.Lines.Count - 1) then
    begin
      Str := Trim(FEditor.Lines[Int]) + Trim(FEditor.Lines[Int + 1]);
      if Str = '' then
        DeleteLine(Int);
    end;
  end;

begin
  FEditor.BeginUpdate;
  var
  Str := '';
  for var I := From to Till do
    Str := Str + FEditor.Lines[I] + #13#10;
  Str := Str + Blanklines;
  if Dest < From then
  begin
    for var I := From to Till do
      DeleteLine(From);
    DeleteBlanklines(From);
    InsertLinesAt(Dest, Str);
  end
  else
  begin // Dest > from
    if DestTill > 0 then
      InsertLinesAt(DestTill + 1, Str)
    else
      InsertLinesAt(Dest, Str);
    for var I := From to Till do
      DeleteLine(From);
    DeleteBlanklines(From);
  end;
  FEditor.EndUpdate;
end;

function TFEditForm.GetBlock(From, Lines: Integer): string;
begin
  var
  Str := '';
  for var I := From to From + Lines - 1 do
    Str := Str + FEditor.Lines[I] + #13#10;
  Result := Str;
end;

procedure TFEditForm.ToBackground(Control: TControl);
var
  Start, From, Till: Integer;
  Container: string;
begin
  if Control is TJEComponent then
  begin
    Container := (Control as TJEComponent).GetContainerAdd;
    Start := GetLNGStartComponents;
    From := GetLineNumberWithStartsWordFrom(Start, Control.Name);
    Till := GetLineNumberWithFrom(Start, Container);
    if Till > -1 then
      if Control is TFXNode then
        MoveBlock(From, Till, Start + 1, 0, '')
      else
        MoveBlock(From, Till, GetLNGEndComponents, 0, '');
  end;
end;

procedure TFEditForm.ToForeground(Control: TControl);
var
  Start, From, Till: Integer;
  Container: string;
begin
  if Control is TJEComponent then
  begin
    Container := (Control as TJEComponent).GetContainerAdd;
    Start := GetLineNumberWith(_(LNGStartComponents));
    From := GetLineNumberWithStartsWordFrom(Start, Control.Name);
    Till := GetLineNumberWithFrom(Start, Container);
    if Till > -1 then
      if Control is TFXNode then
        MoveBlock(From, Till, GetLNGEndComponents, 0, '')
      else
        MoveBlock(From, Till, Start + 1, 0, '');
  end;
end;

procedure TFEditForm.Go_To(const Str: string);
var
  Posi: Integer;
  Line: Integer;
begin
  with FEditor do
  begin
    Line := GetLineNumberWithWord(Str);
    if (0 <= Line) and (Line <= Lines.Count - 1) then
      if Pos('public void ', Lines[Line]) > 0 then
      begin
        CaretY := Line + 2;
        if Line + 1 < Lines.Count then
          Posi := Pos('}', Lines[Line + 1])
        else
          Posi := 0;
        if (0 < Posi) and (Posi < 5) then
          CaretX := Posi
        else
          CaretX := 5;
      end
      else
      begin
        CaretX := Pos(Str, Lines[Line]);
        CaretY := Line + 1;
      end;
  end;
end;

function TFEditForm.GetClassAttribut(const AClass, AAttribute: string): string;
begin
  Result := '';
  with FEditor do
  begin
    var
    Line := GetLineNumberWith(AClass);
    if Line >= 0 then
    begin
      repeat
        if Pos(AAttribute, Lines[Line]) > 0 then
        begin
          Result := Lines[Line];
          Break;
        end;
        Inc(Line);
      until Line >= Lines.Count;
    end;
  end;
end;

function TFEditForm.HasText(const Str: string): Boolean;
begin
  Result := (Pos(Str, FEditor.Text) > 0);
end;

function TFEditForm.HasWord(const Str: string): Boolean;
var
  SText: string;
  Line, Posi: Integer;
begin
  Result := False;
  with FEditor do
  begin
    Line := 0;
    repeat
      Line := GetLineNumberWithFrom(Line, Str);
      if Line >= 0 then
      begin
        SText := Lines[Line];
        Posi := Pos(Str, SText);
        Result := True;
        if Posi > 1 then
          Result := IsWordBreakChar(SText[Posi - 1]);
        if Posi + Length(Str) <= Length(SText) then
          Result := Result and IsWordBreakChar(SText[Posi + Length(Str)]);
        if Result then
          Exit;
      end;
      Inc(Line);
    until Line = 0;
  end;
end;

procedure TFEditForm.SBDesignformClick(Sender: TObject);
begin
  if IsJava and (Partner = nil) then
  begin
    var
    Str := ChangeFileExt(Pathname, '.jfm');
    if FJava.Open(Str) then
      FJava.RearrangeFileHistory(Str);
  end;
end;

function TFEditForm.GetFrameType: Integer;
begin
  if (FFrameType = 0) and ((FFileExtension = '.java') or
    (FFileExtension = '.~ava')) then
  begin
    var
    JavaScanner := TJavaScanner.Create;
    JavaScanner.Init(FEditor.Text);
    FFrameType := JavaScanner.GetFrameType;
    JavaScanner.Destroy;
  end;
  Result := FFrameType;
end;

procedure TFEditForm.DeleteTryCatch(const Key: string);
begin
  var
  Int := GetLineNumberWith(Key);
  if (Int > -1) and (Pos('try {', FEditor.Lines[Int - 1]) > 0) then
    DeleteBlock(Int - 1, Int + 3);
end;

procedure TFEditForm.DesignButtonClick(Sender: TObject);
begin
  var
  Str := ChangeFileExt(Pathname, '.jfm');
  if Partner = nil then
  begin
    if FJava.Open(Str) then
      FJava.RearrangeFileHistory(Str);
  end
  else
    Partner.Close;
end;

procedure TFEditForm.DeleteComponent(const Component: string);
var
  Line, Stop: Integer;
begin
  FEditor.BeginUpdate;
  Line := GetLineNumberWith(GetLNG(3, 0));
  if Line >= 0 then
  begin
    Stop := GetLineNumberWith(GetLNG(4, 0));
    if Stop = -1 then
      Stop := FEditor.Lines.Count - 1;
    for var I := Stop downto Line + 1 do
      if Pos(Component, FEditor.Lines[I]) > 0 then
        DeleteLine(I);
  end;
  Modified := True;
  FEditor.EndUpdate;
end;

procedure TFEditForm.DeleteComponentDefault(Control: TControl);
var
  Line, Stop: Integer;
  Typ: string;
begin
  FEditor.BeginUpdate;
  Typ := (Control as TJEComponent).JavaType;
  DeleteAttribute('private ' + Typ + ' ' + Control.Name);

  Stop := GetLNGEndComponents;
  if Stop = -1 then
    Stop := FEditor.Lines.Count - 1;
  Line := GetLNGStartComponents;
  if Line >= 0 then
    for var I := Stop downto Line + 1 do
      if ContainsWord(Control.Name, I) then
        DeleteLine(I);
  Modified := True;
  FEditor.EndUpdate;
end;

procedure TFEditForm.DeleteComponentTotal(ClassNumber: Integer;
const Component, Typ: string);
var
  Line, Stop: Integer;
  Str: string;
begin
  FEditor.BeginUpdate;
  Line := GetLineNumberWith(GetLNG(1, ClassNumber));
  if Line >= 0 then
  begin
    Stop := GetLineNumberWith(GetLNG(2, ClassNumber));
    if Stop = -1 then
      Stop := FEditor.Lines.Count - 1;
    for var I := Stop downto Line + 1 do
    begin
      Str := FEditor.Lines[I];
      if ((Pos(' ' + Component + ' ', Str) + Pos(' ' + Component + ';', Str) >
        0) and (Pos(Typ, Str) > 0)) then
        DeleteLine(I);
    end;
  end;
  Line := GetLineNumberWith(GetLNG(3, ClassNumber));
  if Line >= 0 then
  begin
    Stop := GetLineNumberWith(GetLNG(4, ClassNumber));
    if Stop = -1 then
      Stop := FEditor.Lines.Count - 1;
    for var I := Stop downto Line + 1 do
    begin
      Str := FEditor.Lines[I];
      if (Pos(Component + '.', Str) > 0) or (Pos(Component + ')', Str) > 0) then
        DeleteLine(I);
    end;
  end;
  Modified := True;
  FEditor.EndUpdate;
end;

function TFEditForm.HasEventProcedureInModel(const AMethodname: string)
  : Boolean;
var
  ClassIt, Ite: IModelIterator;
  Cent: TModelEntity;
  Method: TOperation;
begin
  Result := False;
  // ParseSourcecode; caller has to do
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  while ClassIt.HasNext and not Result do
  begin
    Cent := ClassIt.Next;
    if (Cent is TClass) then
    begin
      Ite := (Cent as TClass).GetOperations;
      while Ite.HasNext and not Result do
      begin
        Method := Ite.Next as TOperation;
        if Method.Name = AMethodname then
          Result := True;
      end;
    end;
  end;
end;

procedure TFEditForm.SetNewActionEventFormat;
var
  Int, Posi, AEnd: Integer;
  Str: string;
begin
  with FEditor do
  begin
    BeginUpdate;
    Int := 1;
    AEnd := GetLNGEndComponents;
    if AEnd > -1 then
    begin
      repeat
        Str := Lines[Int];
        Posi := Pos('ActionPerformed(evt);', Str);
        if (Posi > 1) and (Str[Posi - 1] <> '_') then
        begin
          Insert('_', Str, Posi);
          Lines[Int] := Str;
        end;
        Inc(Int);
      until Int = AEnd;
      repeat
        Str := Lines[Int];
        Posi := Pos('ActionPerformed(ActionEvent evt)', Str);
        if (Posi > 1) and (Str[Posi - 1] <> '_') then
        begin
          Insert('_', Str, Posi);
          Lines[Int] := Str;
        end;
        Inc(Int);
      until Int = Lines.Count - 1;
      EndUpdate;
    end;
  end;
end;

procedure TFEditForm.DeleteMethod(const Method: string;
SourcecodeCheck: Boolean = True);
var
  From, Till: Integer;
  Found: Boolean;
  ClassIt, Ite: IModelIterator;
  Cent: TModelEntity;
  Operation: TOperation;
begin
  From := 0;
  Till := 0;
  Found := False;
  Operation := nil;
  ParseSourceCode(True);
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  while ClassIt.HasNext and not Found do
  begin
    Cent := ClassIt.Next;
    if Cent is TClass then
    begin
      Ite := (Cent as TClass).GetOperations;
      while Ite.HasNext and not Found do
      begin
        Operation := Ite.Next as TOperation;
        if Operation.Name = Method then
        begin
          From := Operation.Lines - 1;
          Till := Operation.LineE - 1;
          Found := True;
        end;
      end;
    end;
  end;

  if Found and (SourcecodeCheck and not Operation.HasSourceCode or
    not SourcecodeCheck) then
  begin
    DeleteBlock(From, Till);
    while (From < FEditor.Lines.Count) and (Trim(FEditor.Lines[From]) = '') and
      (FEditor.CaretY < FEditor.Lines.Count) do
      DeleteLine(From);
    FNeedsParsing := True;
  end;
end;

procedure TFEditForm.DeleteMethod(Method: TOperation);
begin
  FEditor.BeginUpdate;
  DeleteBlock(Method.Lines - 1, Method.LineE - 1);
  var
  Int := Method.Lines - 1;
  if Method.HasComment then
  begin
    DeleteBlock(Method.Documentation.Lines - 1, Method.Documentation.LineE - 1);
    Int := Method.Documentation.Lines - 1;
  end;
  DeleteEmptyLines(Int - 1);
  FEditor.EndUpdate;
end;

procedure TFEditForm.DeleteEventMethod(Method: string);
var
  From, Till, Line: Integer;
  StringList: TStringList;

  function WithoutEndComment(Str: string): string;
  begin
    var
    StringList := TStringList.Create;
    StringList.Text := Str;
    var
    CPos := StringList.Count - 2;
    var
    Posi := Pos('// end of', StringList[CPos]);
    if Posi > 0 then
      StringList[CPos] := Copy(StringList[CPos], 1, Posi - 1);
    Str := StringList.Text;
    FreeAndNil(StringList);
    Result := Str;
  end;

begin
  Method := WithoutEndComment(Method);
  StringList := TStringList.Create;
  StringList.Text := Method;
  From := GetLineNumberWith(_(LNGStartEventMethods));
  Till := GetLineNumberWithFrom(From, _(LNGEndEventMethods));
  Line := GetLineNumberWithFromTill(From, Till, StringList[0]);
  while (Line >= 0) and (Line < Till) do
  begin
    if Method = WithoutEndComment(GetBlock(Line, StringList.Count)) then
    begin
      DeleteBlock(Line, Line + StringList.Count - 1);
      if Trim(FEditor.Lines[Line]) = '' then
        DeleteLine(Line);
      Line := -1;
    end
    else
      Line := GetLineNumberWithFromTill(Line + 1, Till, StringList[0]);
  end;
  FreeAndNil(StringList);
end;

procedure TFEditForm.DeleteListener(Listener: string);
var
  From, Till, Line: Integer;
  StringList: TStringList;
  Block: string;
begin
  StringList := TStringList.Create;
  StringList.Text := Listener;
  From := GetLNGStartComponents;
  Till := GetLineNumberWithFrom(From, _(LNGEndComponents));
  Line := GetLineNumberWithFromTill(From, Till, StringList[0]);
  while (Line >= 0) and (Line < Till) do
  begin
    Block := GetBlock(Line, StringList.Count);
    // due to caretPositonChanged <-> inputMethodTextChanged problem
    if (Listener = Block) or (Listener + #13#10 = Block) then
    begin
      DeleteBlock(Line, Line + StringList.Count - 1);
      if Trim(FEditor.Lines[Line]) = '' then
        DeleteLine(Line);
      Line := -1;
    end
    else
      Line := GetLineNumberWithFromTill(Line + 1, Till, StringList[0]);
  end;
  FreeAndNil(StringList);
end;

procedure TFEditForm.DeleteFXListener(Listener: string);
var
  Str: string;
  Posi, Line: Integer;
begin
  Posi := Pos(#13#10, Listener);
  Delete(Listener, 1, Posi + 1);
  Posi := Pos(#13#10, Listener);
  Str := Trim(Copy(Listener, 1, Posi - 1));
  Line := GetLineNumberWith(Str);
  if Line > -1 then
  begin
    Dec(Line, 1);
    DeleteBlock(Line, Line + 2);
    if Trim(FEditor.Lines[Line]) = '' then
      DeleteLine(Line);
    Modified := True;
  end;
end;

procedure TFEditForm.DeleteLambdaListener(Listener: string);
begin
  var
  Posi := Pos(#13#10, Listener);
  if Posi > 0 then
    Listener := Trim(Copy(Listener, 1, Posi - 1));
  DeleteComponentValue(Listener);
end;

type
  TMethodSource = record
    Name: string;
    From: Integer;
    Till: Integer;
  end;

procedure TFEditForm.DeleteOldAddNewMethods(OldMethods,
  NewMethods: TStringList);
var
  Posi, From, Till, Index: Integer;
  ClassIt, Ite: IModelIterator;
  Cent: TModelEntity;
  Method: TOperation;
  MethodArray: array of TMethodSource;
  StringList: TStringList;
  Str1, Str2, Func: string;

  function inMethodArray(MethodName: string): Integer;
  begin
    Result := -1;
    for var I := 0 to Index do
      if MethodArray[I].Name = MethodName then
      begin
        Result := I;
        Break;
      end;
  end;

  function getActionMethod(Name: string): string;
  begin
    if EndsWith(Name, '_ActionPerformed') then
      Delete(Name, Pos('_ActionPerformed', Name), 16);
    Result := FConfiguration.Indent1 + 'public void ' + Name +
      '_ActionPerformed(ActionEvent evt) {' + CrLf + FConfiguration.Indent2 +
      _(LNGTODO) + CrLf + CrLf + FConfiguration.Indent1 + '}';
    if FConfiguration.CommentClosingBrackets then
      Result := Result + ' // end of ' + Name;
  end;

begin
  // 1. add new methods at the end
  // 2. delete Old methods in the middle
  ParseSourceCode(True);
  StringList := TStringList.Create;
  SetLength(MethodArray, 20);

  // get existing methods
  Index := -1;
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  while ClassIt.HasNext do
  begin
    Cent := ClassIt.Next;
    if Cent is TClass then
    begin
      Ite := (Cent as TClass).GetOperations;
      while Ite.HasNext do
      begin
        Inc(Index);
        if Length(MethodArray) = Index then
          SetLength(MethodArray, Length(MethodArray) + 20);
        Method := Ite.Next as TOperation;
        MethodArray[Index].Name := Method.Name;
        MethodArray[Index].From := Method.Lines - 1;
        MethodArray[Index].Till := Method.LineE - 1;
      end;
    end;
  end;

  // add missing new methods
  for var I := 0 to NewMethods.Count - 1 do
    if inMethodArray(NewMethods[I] + '_ActionPerformed') = -1 then
    begin
      Func := CrLf + getActionMethod(NewMethods[I]);
      StringList.Add(Func);
    end;
  InsertProcedure(StringList.Text);

  // delete unnecessary default methods
  // determine methods to delete
  StringList.Clear;
  for var I := 0 to OldMethods.Count - 1 do // keeps method Lines valid
    if NewMethods.IndexOf(OldMethods[I]) = -1 then
    begin
      Posi := inMethodArray(OldMethods[I] + '_ActionPerformed');
      if Posi > -1 then
      begin
        // delete if default
        From := MethodArray[Posi].From;
        Till := MethodArray[Posi].Till;
        Str1 := GetSource(From, Till);
        Str2 := getActionMethod(MethodArray[Posi].Name) + #13#10;
        if Str1 = Str2 then
          StringList.Add(OldMethods[I] + '_ActionPerformed');
      end;
    end;

  // delete beginning at the end of the sourcecode
  for var I := Index downto 0 do
    if StringList.IndexOf(MethodArray[I].Name) > -1 then
    begin
      From := MethodArray[I].From;
      Till := MethodArray[I].Till;
      DeleteBlock(From, Till);
      while (From < FEditor.Lines.Count - 1) and
        (Trim(FEditor.Lines[From]) = '') do
        DeleteLine(From);
      InsertLinesAt(From, '');
    end;
  FreeAndNil(StringList);
  SetLength(MethodArray, 0);
end;

procedure TFEditForm.DeleteFXOldAddNewMethods(OldMethods,
  NewMethods: TStringList);
var
  Posi, From, Till, Index: Integer;
  ClassIt, Ite: IModelIterator;
  Cent: TModelEntity;
  Method: TOperation;
  MethodArray: array of TMethodSource;
  StringList: TStringList;
  Str1, Str2, Func: string;

  function inMethodArray(MethodName: string): Integer;
  begin
    Result := -1;
    for var I := 0 to Index do
      if MethodArray[I].Name = MethodName then
      begin
        Result := I;
        Break;
      end;
  end;

  function getActionMethod(Name: string): string;
  begin
    Result := FConfiguration.Indent1 + 'public void ' + Name + '(Event evt) {' +
      CrLf + FConfiguration.Indent2 + _(LNGTODO) + CrLf + CrLf +
      FConfiguration.Indent1 + '}';
    if FConfiguration.CommentClosingBrackets then
      Result := Result + ' // end of ' + Name;
  end;

begin
  // 1. add new methods at the end
  // 2. delete Old methods in the middle
  ParseSourceCode(True);
  StringList := TStringList.Create;
  SetLength(MethodArray, 20);

  // get existing methods
  Index := -1;
  ClassIt := FModel.ModelRoot.GetAllClassifiers;
  while ClassIt.HasNext do
  begin
    Cent := ClassIt.Next;
    if Cent is TClass then
    begin
      Ite := (Cent as TClass).GetOperations;
      while Ite.HasNext do
      begin
        Inc(Index);
        if Length(MethodArray) = Index then
          SetLength(MethodArray, Length(MethodArray) + 20);
        Method := Ite.Next as TOperation;
        MethodArray[Index].Name := Method.Name;
        MethodArray[Index].From := Method.Lines - 1;
        MethodArray[Index].Till := Method.LineE - 1;
      end;
    end;
  end;

  // add missing new methods
  for var I := 0 to NewMethods.Count - 1 do
    if inMethodArray(NewMethods[I] + '_Action') = -1 then
    begin
      Func := CrLf + getActionMethod(NewMethods[I] + '_Action');
      StringList.Add(Func);
    end;
  InsertProcedure(StringList.Text);

  // delete unnecessary default methods
  // determine methods to delete
  StringList.Clear;
  for var I := 0 to OldMethods.Count - 1 do // keeps method Lines valid
    if NewMethods.IndexOf(OldMethods[I]) = -1 then
    begin
      Posi := inMethodArray(OldMethods[I] + '_Action');
      if Posi > -1 then
      begin
        // delete if default
        From := MethodArray[Posi].From;
        Till := MethodArray[Posi].Till;
        Str1 := GetSource(From, Till);
        Str2 := getActionMethod(MethodArray[Posi].Name) + #13#10;
        if Str1 = Str2 then
          StringList.Add(OldMethods[I] + '_Action');
      end;
    end;

  // delete beginning at the end of the sourcecode
  for var I := Index downto 0 do
    if StringList.IndexOf(MethodArray[I].Name) > -1 then
    begin
      From := MethodArray[I].From;
      Till := MethodArray[I].Till;
      DeleteBlock(From, Till);
      while (From < FEditor.Lines.Count - 1) and
        (Trim(FEditor.Lines[From]) = '') do
        DeleteLine(From);
      InsertLinesAt(From, '');
    end;
  FreeAndNil(StringList);
  SetLength(MethodArray, 0);
end;

procedure TFEditForm.SBClassEditClick(Sender: TObject);
begin
  if IsJava then
  begin
    if Modified then
      FJava.DoSave(Self, False);
    if not MyJavaCommands.HasValidClass(Pathname) then
      MyJavaCommands.CompileForm(Self);
    FJava.PrepareClassEdit(Pathname, 'Edit', nil);
  end;
end;

function TFEditForm.HasStartAndEnd(CorI: Integer): Boolean;
begin
  Result := True;
  var
  I := 0;
  while Result and (I < CorI) do
  begin
    Result := Result and HasText(GetLNG(1, I)) and HasText(GetLNG(2, I)) and
      HasText(GetLNG(5, I)) and HasText(GetLNG(6, I));
    Inc(I);
  end;
  if Result and (FFrameType > 1) then
    Result := Result and HasText(_(LNGStartComponents)) and
      HasText(_(LNGEndComponents));
end;

procedure TFEditForm.WMNCButtonDBLClick(var Msg: TMessage);
begin
  FJava.MIMaximizedClick(Self);
end;

procedure TFEditForm.SBClassOpenClick(Sender: TObject);
var
  UMLForm: TFUMLForm;
  Str, AClassname: string;
begin
  if IsJava or IsPascal then
  begin
    if Modified then
      FJava.DoSave(Self, False);
    LockWindow(FJava.Handle);
    var
    StringList := TStringList.Create;
    try
      Str := ChangeFileExt(Pathname, '.uml');
      AClassname := ChangeFileExt(ExtractFileName(Pathname), '');
      if FileExists(Str) then
      begin
        FJava.OpenUMLWindow(Str, '');
        StringList.LoadFromFile(Str);
        if Pos('[Box:  - ' + AClassname, StringList.Text) = 0 then
          FJava.DoOpenInUMLWindow(Pathname);
      end
      else
      begin
        UMLForm := FJava.MakeNewUMLWindow(Str, '');
        FConfiguration.ShowAlways := False;
        UMLForm.MainModul.AddToProject(Pathname);
        UMLForm.CreateTVFileStructure;
        UMLForm.MainModul.DoLayout;
        FConfiguration.ShowAlways := True;
        FJava.DoSave(UMLForm, False);
      end;
      FJava.RearrangeFileHistory(Str);
    finally
      UnlockWindow;
      StringList.Destroy;
    end;
  end;
end;

procedure TFEditForm.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFEditForm.SBBrowserClick(Sender: TObject);
begin
  if Modified then
    Save(False);
  FJava.CallApplet(Pathname);
end;

procedure TFEditForm.SBValidateClick(Sender: TObject);
begin
  var
  Browser := FJava.NewBrowser('', '');
  if IsCSS then
    Browser.UploadFilesHttpPost('https://jigsaw.w3.org/css-validator/validator',
      [], [], ['file'], [Pathname])
  else
    Browser.UploadFilesHttpPost('https://validator.w3.org/check', [], [],
      ['uploaded_file'], [Pathname]);
end;

procedure TFEditForm.SBExplorerClick(Sender: TObject);
begin
  var
  Str := ExtractFilePath(Pathname);
  FJava.NewExplorer(Str, '');
end;

procedure TFEditForm.MIRenewImportsClick(Sender: TObject);
begin
  FConfiguration.FixImports := True;
  FNeedsParsing := True;
  ParseSourceCode(True);
end;

procedure TFEditForm.MIFontClick(Sender: TObject);
begin
  FJava.MIFontClick(Self);
end;

procedure TFEditForm.MIGitAddClick(Sender: TObject);
begin
  if Modified then
    Save(False);
  FGit.GitCall('add ' + ExtractFileName(Pathname), ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitCheckoutClick(Sender: TObject);
begin
  FGit.GitCall('checkout -- ' + ExtractFileName(Pathname),
    ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitGuiClick(Sender: TObject);
begin
  FGit.ShowGUI(ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitViewerClick(Sender: TObject);
begin
  FGit.ShowViewer(ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitConsoleClick(Sender: TObject);
begin
  FGit.ShowConsole(ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitFetchClick(Sender: TObject);
begin
  FGit.GitCall('fetch ' + FConfiguration.GitRemoteRepository,
    ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitLogClick(Sender: TObject);
begin
  FGit.GitCall('log --stat', ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitPushClick(Sender: TObject);
begin
  FGit.GitCall('push origin master', ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitResetClick(Sender: TObject);
begin
  FGit.GitCall('reset HEAD ' + ExtractFileName(Pathname),
    ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitRemoteClick(Sender: TObject);
begin
  FGit.GitCall('remote -v', ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitRemoveClick(Sender: TObject);
begin
  FGit.GitCall('rm ' + ExtractFileName(Pathname), ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitStatusClick(Sender: TObject);
begin
  FGit.GitCall('status', ExtractFilePath(Pathname));
end;

function TFEditForm.EncodingAsString(const AEncoding: string): string;
begin
  Result := UpperCase(AEncoding);
  if Pos('ANSI', Result) > 0 then
    Result := 'ANSI'
  else if Pos('ASCII', Result) > 0 then
    Result := 'ASCII'
  else if Pos('UTF-8', Result) > 0 then
    Result := 'UTF-8'
  else if Pos('UTF-16', Result) > 0 then
    Result := 'UTF-16'
  else if Pos('UNICODE', Result) > 0 then
    Result := 'UTF-16'
  else if Pos('CP1252', Result) > 0 then
    Result := 'ANSI';
end;

function TFEditForm.LinebreakAsString: string;
begin
  if FLineBreak = #13#10 then
    Result := 'Windows'
  else if FLineBreak = #10 then
    Result := 'Unix'
  else
    Result := 'Mac';
end;

function TFEditForm.LinebreakAsCtrls(const Str: string): string;
begin
  if Str = 'Windows' then
    Result := #13#10
  else if Str = 'Unix' then
    Result := #10
  else
    Result := #13;
end;

procedure TFEditForm.SetEncoding(AEncoding: string);
begin
  Self.FEncoding := EncodingAsString(AEncoding);
  var
  Posi := Pos('/', AEncoding);
  Delete(AEncoding, 1, Posi);
  FLineBreak := LinebreakAsCtrls(AEncoding);
  FEditor.Lines.DefaultEncoding := GetEncodingAsType;
end;

function TFEditForm.GetEncodingAsType: TEncoding;
begin
  if FEncoding = 'ANSI' then
    Result := TEncoding.ANSI
  else if FEncoding = 'UTF-8' then
    Result := TEncoding.UTF8
  else
    Result := TEncoding.Unicode;
end;

procedure TFEditForm.CheckAge;
var
  FDT: TDateTime;
begin
  if not FConfiguration.CheckAge then
    Exit;
  FileAge(Pathname, FDT);
  if Visible and (FEditorAge <> 0) and FCheckAgeEnabled and FileExists(Pathname)
    and (FEditorAge <> FDT) then
  begin
    if MessageDlg(Format(_('File %s externally modified. Open new?'), [Pathname]
      ), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      with FEditor do
        try
          Lines.LoadFromFile(Pathname);
          FEditor.ReplaceTabs(FConfiguration.TabWidth);
        except
          on e: Exception do
            FConfiguration.Log('TFEditForm.CheckAge: ' + Pathname, e);
        end;
    FileAge(Pathname, FEditorAge);
  end;
end;

function TFEditForm.IsJava: Boolean;
begin
  Result := (FFileExtension = '.java');
end;

function TFEditForm.IsPascal: Boolean;
begin
  Result := (FFileExtension = '.pas');
end;

function TFEditForm.IsHTML: Boolean;
begin
  Result := (FFileExtension = '.html') or (FFileExtension = '.htm');
end;

function TFEditForm.IsCSS: Boolean;
begin
  Result := (FFileExtension = '.css');
end;

procedure TFEditForm.Search;
begin
  FJava.ShowSearchReplaceDialog(FEditor, False);
end;

procedure TFEditForm.SearchAgain;
begin
  FJava.DoSearchReplaceText(FEditor, False);
end;

procedure TFEditForm.Replace;
begin
  FJava.ShowSearchReplaceDialog(FEditor, True);
end;

procedure TFEditForm.Show;
begin
  Visible := True;
  FHidden := False;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
end;

procedure TFEditForm.Hide;
begin
  Visible := False;
  FHidden := True;
  FJava.DeleteTabAndWindow(Number);
end;

procedure TFEditForm.ReplaceWidthHeight(Width, Height: Integer);

  function AReplace(const LineText, WidthHeight: string; I: Integer): string;
  var
    Str: string;
    Posi: Integer;
  begin
    Str := LineText;
    Posi := Pos(WidthHeight, Str) + Pos(UpperCase(WidthHeight), Str) +
      Length(WidthHeight);
    while Str[Posi] <> '"' do
      Inc(Posi);
    Inc(Posi);
    while Str[Posi] <> '"' do
      Delete(Str, Posi, 1);
    Insert(IntToStr(I), Str, Posi);
    Result := Str;
  end;

begin
  var
  Line := GetLineNumberWith(' width=');
  if Line >= 0 then
  begin
    FEditor.Lines[Line] := AReplace(FEditor.Lines[Line], 'width', Width);
    Line := GetLineNumberWithFrom(Line + 1, ' width=');
    if Line >= 0 then // PlugIn
      FEditor.Lines[Line] := AReplace(FEditor.Lines[Line], 'width', Width);
    Modified := True;
  end;
  Line := GetLineNumberWith(' height=');
  if Line >= 0 then
  begin
    FEditor.Lines[Line] := AReplace(FEditor.Lines[Line], 'height', Height);
    Line := GetLineNumberWithFrom(Line + 1, ' height=');
    if Line > 0 then
      FEditor.Lines[Line] := AReplace(FEditor.Lines[Line], 'height', Height);
    Modified := True;
  end;
end;

procedure TFEditForm.AutomatedCompleteImports;
var
  AClass, Package, Shorttype, ClassImp, FullImp, ALine: string;
  ClassIt: IModelIterator;
  Cent: TClassifier;
  FUnit: TUnitPackage;
  ClassImports, FullImports, AllImports: TStringList;
  Posi: Integer;
begin
  FUnit := FModel.ModelRoot.FindUnitPackage('Default');
  if not Assigned(FUnit) then
    Exit;

  with FEditor do
  begin
    BeginUpdate;
    FullImports := FUnit.FullImports; // import java.awt.*;
    if FFrameType in [2 .. 4] then
    begin
      FullImports.Add('java.awt.');
      FullImports.Add('java.awt.event.');
    end
    else if FFrameType in [5 .. 7] then
    begin
      FullImports.Add('java.awt.');
      FullImports.Add('java.awt.event.');
      FullImports.Add('javax.swing.');
      FullImports.Add('javax.swing.event.');
    end;
    ClassImports := FUnit.ClassImports; // import java.io.FileInputStream;
    if FFrameType = 8 then
    begin
      ClassImports.Add('Application=javafx.application');
      ClassImports.Add('Scene=javafx.scene');
      ClassImports.Add('Pane=javafx.scene.layout');
      ClassImports.Add('Stage=javafx.stage');
    end;
    ClassIt := FModel.UnknownPackage.GetClassifiers;
    while ClassIt.HasNext do
    begin
      Cent := TClassifier(ClassIt.Next);
      AClass := WithoutGeneric(Cent.Name);
      Shorttype := GetShortType(AClass);
      Package := ExtractPackageName(AClass);
      if (Pos('java.lang.', AClass) = 0) and (Pos('.', AClass) > 0) and
        (FullImports.IndexOf(Package + '.') = -1) and
        (ClassImports.IndexOfName(Shorttype) = -1) then
        ClassImports.Add(Shorttype + '=' + Package);
    end;
    AllImports := TStringList.Create;
    AllImports.Sorted := True;
    AllImports.Duplicates := dupIgnore;
    for var I := 0 to FullImports.Count - 1 do
      if FullImports[I] <> 'java.lang.' then
        AllImports.Add('import ' + FullImports[I] + '*;');
    for var I := 0 to ClassImports.Count - 1 do
      AllImports.Add('import ' + ClassImports.ValueFromIndex[I] + '.' +
        ClassImports.Names[I] + ';');

    for var I := 0 to FConfiguration.ImportCache.Count - 1 do
    begin
      ClassImp := FConfiguration.ImportCache[I];
      Delete(ClassImp, 1, Pos('=', ClassImp));
      if (Pos('.', ClassImp) > 0) and (Pos('<', ClassImp) = 0) and
        (Pos('[', ClassImp) = 0) and (Pos('java.lang.', ClassImp) = 0) then
      begin
        FullImp := 'import ' + Copy(ClassImp, 1,
          LastDelimiter('.', ClassImp)) + '*;';
        if AllImports.IndexOf(FullImp) = -1 then
          AllImports.Add('import ' + ClassImp + ';');
      end;
    end;
    // handle static imports
    for var I := FUnit.ImportStartline - 1 to FUnit.ImportEndline - 1 do
    begin
      ALine := FEditor.Lines[I];
      Posi := Pos(' static ', ALine);
      if Posi > 0 then
      begin
        Delete(ALine, 1, Posi + Length(' static'));
        ALine := 'import ' + Trim(ALine);
        Posi := AllImports.IndexOf(ALine);
        if Posi > -1 then
        begin
          ALine := AllImports[Posi];
          Insert(' static', ALine, Length('import') + 1);
          AllImports.Delete(Posi);
          AllImports.Add(ALine);
        end;
      end;
    end;
    if FUnit.ImportEndline >= FUnit.ImportStartline then
      DeleteBlock(FUnit.ImportStartline - 1, FUnit.ImportEndline - 1);
    if AllImports.Text <> '' then
      InsertLinesAt(FUnit.ImportStartline - 1, AllImports.Text);
    AllImports.Destroy;
    EndUpdate;
  end;
end;

function TFEditForm.GetPackage: string;
begin
  var
  JavaScanner := TJavaScanner.Create;
  try
    if Assigned(FEditor) and (FEditor.Text <> '') then
      Result := JavaScanner.GetPackage(FEditor.Text)
      // >>> TFEditForm.getPackage exe=0
    else
      Result := '';
    JavaScanner.Destroy;
  except
    on e: Exception do
      FConfiguration.Log('TFEditForm.getPackage', e);
  end;
end;

function TFEditForm.GetAllPathnames: TStringList;
begin
  Result := TStringList.Create;
  if FEditor.Text <> '' then
    Result.Add(Pathname);
end;

function TFEditForm.GetAllClassnames: TStringList;
begin
  Result := TStringList.Create;
  if IsJava then
  begin
    ParseSourceCode(False);
    var
    ClassIt := FModel.ModelRoot.GetAllClassifiers;
    while ClassIt.HasNext do
    begin
      var
      Cent := TClassifier(ClassIt.Next);
      if (Cent is TClass) and (Cent.Pathname = Pathname) then
        Result.Add(ExtractFilePath(Pathname) + WithoutGeneric(Cent.Name) +
          '.class');
    end;
  end;
end;

procedure TFEditForm.DoOnMouseOverToken(Sender: TObject; const Token: string;
TokenType: Integer; Caret, Posi: TPoint; Attri: TSynHighlighterAttributes;
var Highlight: Boolean);
begin
  if FConfiguration.TooltipAutomatic and IsJava then
  begin
    if TokenType = Ord(SynHighlighterJava.tkIdentifier) then
    begin
      if (Token <> FLastToken) and (Trim(Token) <> '') then
        CreateTooltip(Caret, Posi, Token);
    end
    else if FTooltip.Visible and not FTooltip.CloseManually then
      FTooltip.Hide;
  end;
  FLastToken := Trim(Token);
end;

procedure TFEditForm.DoOnMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
begin
  if FJava.ScpJava.Form.Visible then
    FJava.ScpJava.CancelCompletion;
end;

procedure TFEditForm.CreateTooltip(Caret, Posi: TPoint; const Token: string);
var
  StringList: TStringList;
  Typ, Code: string;
  Freq, StartTime, EndTime: Int64;
  Duration: Integer;
  TokenRect: TRect;
  Width, Height: Integer;
begin
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(StartTime);
  Width := FEditor.Canvas.TextWidth(Token);
  Height := FEditor.Canvas.TextHeight(Token);
  TokenRect := Rect(Posi.X, Posi.Y - Height, Posi.X + Width, Posi.Y);

  Code := GetJavaCodeAt(Caret);
  Typ := MyCodeCompletion.SearchDocumentation(Code, Caret.Y);
  if Typ <> '' then
  begin
    FTooltip.Init(Self, Posi, TokenRect, Token);
    StringList := TStringList.Create;
    try
      StringList.Add(FTooltip.GetHead);
      StringList.Add(Typ);
      StringList.Add('</body></html>');
      try
        StringList.SaveToFile(FConfiguration.TempDir + 'Tooltip.html');
      except
        on e: Exception do
          ErrorMsg(e.Message);
      end;
    finally
      FreeAndNil(StringList);
    end;
    QueryPerformanceCounter(EndTime);
    Duration := (EndTime - StartTime) * 1000 div Freq; // ms
    Duration := FConfiguration.TooltipDelay - Duration;
    if (Duration < 10) or (Token = '# VK_F2 #') then
      Duration := 10;
    FTooltip.OpenTooltipTimer.Interval := Duration;
    FTooltip.OpenTooltipTimer.Enabled := True;
  end;
end;

procedure TFEditForm.MISearchDeclarationClick(Sender: TObject);
var
  Code: string;
  Classifier: TClassifier;
  Attribute: TAttribute;
  Operation: TOperation;
  AParameter: TParameter;
begin
  if FMousePosition.Char > -1 then
  begin
    Code := GetJavaCodeAt(Point(FMousePosition.Char, FMousePosition.Line));
    if Code = '' then
      Exit;

    MyCodeCompletion.GetTypeOfCode(Code, FMousePosition.Line, 0, False);
    Classifier := MyCodeCompletion.CCClassifier;
    Attribute := MyCodeCompletion.CCAttribute;
    Operation := MyCodeCompletion.CCOperation;
    AParameter := MyCodeCompletion.CCParameter;

    if Assigned(Classifier) then
    begin
      if Assigned(Attribute) then
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname),
          Attribute.Spalte, Attribute.Lines, False)
      else if Assigned(Operation) then
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname),
          Operation.Spalte, Operation.Lines, False)
      else if Assigned(AParameter) then
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname),
          AParameter.Spalte, AParameter.Lines, False)
      else
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname),
          Classifier.Spalte, Classifier.Lines, False);
    end;
  end;
end;

procedure TFEditForm.SetNeedsParsing(Value: Boolean);
begin
  if Value <> FNeedsParsing then
  begin
    if Value and Assigned(FEditor) and not FEditor.LockBuildStructure then
      FNeedsParsing := True
    else
      FNeedsParsing := False;
  end;
end;

function TFEditForm.GetJavaCodeAt(Caret: TPoint): string;
var
  Str: string;
  Posi, Brackets, Line: Integer;
begin
  Line := Caret.Y - 1;
  Str := FEditor.Lines[Line];
  Posi := Max(Caret.X, 1);
  // expand to right
  while (Posi <= Length(Str)) and (IsCharAlphaNumeric(Str[Posi]) or
    (Str[Posi] = '_')) do
    Inc(Posi);
  // detect method call
  if (Posi <= Length(Str)) and (Str[Posi] = '(') then
  begin
    // get Parameters
    Brackets := 1;
    while Brackets > 0 do
    begin
      if Posi = Length(Str) then
      begin // FParameter on next Line
        Inc(Line);
        if Line < FEditor.Lines.Count then
          Str := Str + ' ' + FEditor.Lines[Line]
        else
          Break;
      end;
      Inc(Posi);
      if Str[Posi] = ')' then
        Dec(Brackets)
      else if Str[Posi] = '(' then
        Inc(Brackets);
    end;
    Str := Copy(Str, 1, Posi);
  end
  else
    Str := Copy(Str, 1, Posi - 1);

  // expand to left
  Posi := Caret.X;
  while (Posi > 0) and (Posi <= Length(Str)) and
    (IsCharAlphaNumeric(Str[Posi]) or CharInSet(Str[Posi],
    ['.', '_', ')', ']'])) do
  begin
    if Str[Posi] = ')' then
    begin
      Brackets := 1;
      while (Posi > 0) and (Brackets > 0) do
      begin
        Dec(Posi);
        if Str[Posi] = ')' then
          Inc(Brackets)
        else if Str[Posi] = '(' then
          Dec(Brackets);
      end;
    end;
    if Str[Posi] = ']' then
    begin
      Brackets := 1;
      while (Posi > 0) and (Brackets > 0) do
      begin
        Dec(Posi);
        if Str[Posi] = ']' then
          Inc(Brackets)
        else if Str[Posi] = '[' then
          Dec(Brackets);
      end;
    end;
    Dec(Posi);
  end;
  Delete(Str, 1, Posi);
  Result := Str;
end;

procedure TFEditForm.DoOnBuildStructure(Sender: TObject);
begin
  FNeedsParsing := True;
end;

// var Count: integer = 0;

procedure TFEditForm.ParseSourcecodeWithThread(HasChanged: Boolean);
begin
  FNeedsParsing := FNeedsParsing or HasChanged;
  if Assigned(FParseThread) then
  begin
    if (FParseThread.State > 0) and (FParseThread.State < 3) then
    begin
      FParseThread.Abort := True; // finish the job
      FParseThread.WaitFor;
    end;
    FreeAndNil(FParseThread);
  end;
  if FNeedsParsing then
    FParseThread := TParseThread.Create(Self, True);
end;

procedure TFEditForm.ParseSourceCode(HasChanged: Boolean);
begin
  FNeedsParsing := FNeedsParsing or HasChanged;
  if IsJava and FNeedsParsing then
  begin
    if Assigned(FParseThread) then
    begin
      FParseThread.Abort := True;
      FParseThread.WaitFor;
      FreeAndNil(FParseThread);
    end;

    if Assigned(FModel) then
      FModel.Clear;
    FConfiguration.ImportCache.Clear;
    var
    Importer := TJavaImporter.Create(FModel, TFileProvider.Create);
    try
      Importer.AddClasspath
        (UnHideBlanks(FConfiguration.GetClassPathJarExpanded(Pathname,
        GetPackage)), Pathname);
      var
      Str := Importer.CodeProvider.LoadStream(Pathname, Self);
      if Assigned(Str) then
      begin
        FreeAndNil(FParser);
        FParser := TJavaParser.Create(True);
        FParser.NeedPackage := Importer.NeedPackageHandler;
        FParser.ParseStream(Str, FModel.ModelRoot, FModel, Pathname,
          False, False);
        FEditor.Structures := FParser.Structures.Clone;
      end;
    finally
      FreeAndNil(Importer);
    end;
    // FreeAndNil(Str); handeld by Scanner
    if FConfiguration.FixImports then
    begin
      AutomatedCompleteImports;
      FNeedsParsing := True;
    end
    else
    begin
      FNeedsParsing := False;
      if not FHidden then
        CreateTVFileStructure;
    end;
  end;
  FConfiguration.FixImports := False;
end;

function TFEditForm.ClassnameDifferentFromAncestors(const AClassname
  : string): Boolean;
begin
  var
  MClassifier := MyCodeCompletion.GetMClassifier(AClassname, Self);
  Result := True;
  while Result and Assigned(MClassifier) do
  begin
    var
    AAncestor := ExtractClassName(MClassifier.GetAncestorName);
    if AAncestor = AClassname then
      Result := False
    else
      MClassifier := MyCodeCompletion.GetMClassifier(AAncestor, Self);
  end;
end;

procedure TFEditForm.InitShowCompileErrors;
begin
  if FEditor.Errors.Count > 0 then
  begin
    FEditor.InitShowCompileErrors;
    FEditor.InvalidateGutter;
    ClearCompilerErrorMarks;
    Application.ProcessMessages;
  end;
end;

procedure TFEditForm.SetErrorMark(Line, Column: Integer; const Error: string);
begin
  FEditor.SetCompileError(Point(Column, Line + 1));
  for var I := FEditor.Marks.Count - 1 downto 0 do
    if (FEditor.Marks[I].ImageIndex = ErrorMarkIndex) and
      (FEditor.Marks[I].Line = Line) then
      Exit;

  var
  Mark := TSynEditMark.Create(FEditor);
  Mark.Line := Line;
  Mark.Char := Column;
  Mark.ImageIndex := ErrorMarkIndex;
  Mark.Visible := True;
  FEditor.Marks.Add(Mark);
end;

procedure TFEditForm.UnderlineCompileErrors;
begin
  FEditor.UnderlineCompileErrors;
end;

procedure TFEditForm.TerminateThread(Sender: TObject);
begin
  // if not FNeedsParsing then
  // CreateTVFileStructure;
end;

procedure TFEditForm.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    EditformToolbar.Images := vilEditorToolbarDark;
    PopUpEditor.Images := vilContextMenuDark;
    FEditor.BookMarkOptions.BookmarkImages := vilBookmarksDark;
  end
  else
  begin
    EditformToolbar.Images := vilEditorToolbarLight;
    PopUpEditor.Images := vilContextMenuLight;
    FEditor.BookMarkOptions.BookmarkImages := vilBookmarksLight;
  end;
end;

procedure TFEditForm.RemoveShortCutFromEditor(ShortCut: Integer);
begin
  var
  I := FEditor.Keystrokes.FindShortcut(ShortCut);
  if I >= 0 then
    FEditor.Keystrokes.Delete(I);
end;

procedure TFEditForm.ReplaceShortCutFromEditor(ShortCut, ShortCut2: Integer);
begin
  var
  I := FEditor.Keystrokes.FindShortcut(ShortCut);
  if I >= 0 then
    try
      FEditor.Keystrokes[I].ShortCut := ShortCut2;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
end;

procedure TFEditForm.EditShortCuts;
var
  Posi, Line, Key, Key2: Integer;
  Keys: TStringList;
  Str: string;

  function GetNextLine: string;
  begin
    Inc(Line);
    if Line < Keys.Count then
      Result := Keys[Line]
    else
      Result := '';
  end;

begin
  if FileExists(FConfiguration.KeyboardFile) then
  begin
    Keys := TStringList.Create;
    Keys.LoadFromFile(FConfiguration.KeyboardFile);
    Line := -1;
    Str := GetNextLine;
    repeat
      Posi := Pos('shortcut:', Str);
      if Pos('shortcut:end', Str) > 0 then
        Posi := 0;
      if Posi = 1 then
      begin
        Key := FConfiguration.StringToShortcut(Str);
        RemoveShortCutFromEditor(Key);
        repeat
          Str := GetNextLine;
          Posi := Pos('shortcut:end', Str);
        until (Posi = 1) or (Line >= Keys.Count - 1);
      end
      else
      begin
        Posi := Pos('disableEditor:', Str);
        if Posi = 1 then
        begin
          Key := FConfiguration.StringToShortcut(Str);
          RemoveShortCutFromEditor(Key);
        end
        else
        begin
          Posi := Pos('replaceEditor:', Str);
          if Posi = 1 then
          begin
            Key := FConfiguration.StringToShortcut(Str);
            Str := GetNextLine;
            Posi := Pos('with:', Str);
            if Posi = 1 then
            begin
              Key2 := FConfiguration.StringToShortcut(Str);
              ReplaceShortCutFromEditor(Key, Key2);
            end;
          end;
        end;
      end;
      Str := GetNextLine;
    until Line >= Keys.Count - 1;
    FreeAndNil(Keys);
  end;
end;

function TFEditForm.MakeUpperEvents(Events: string): string;
begin
  if Events <> '' then
  begin
    Events[1] := UpCase(Events[1]);
    for var I := 1 to Length(Events) do
      if (Events[I] = '|') and (I < Length(Events)) then
        Events[I + 1] := UpCase(Events[I + 1]);
  end;
  Result := Events;
end;

procedure TFEditForm.SetFXBackgroundAsString(const Container, AName,
  AColor: string);
begin
  if AColor = '' then
    DeleteAttributeValue(AName + '.setBackground(')
  else
  begin
    var
    Str1 := AName + '.setBackground(';
    var
    Str2 := 'new Background(new BackgroundFill(' + AColor +
      ', CornerRadii.Empty, Insets.Empty)));';
    Str2 := FConfiguration.Indent2 + Str1 + Str2;
    if Container = 'root' then
      SetAttributValue(Container, Str1, Str2, 1)
    else
      SetAttributValue(Container, Str1, Str2, 0);
    InsertImport('javafx.scene.paint.Color');
    InsertImport('javafx.geometry.Insets');
    InsertImport('javafx.scene.layout.*');
  end;
end;

procedure TFEditForm.DPIChanged;
begin
  SetFontSize(0);
  Hide;
  Show;
end;

function TFEditForm.CountClassOrInterface: Integer;
begin
  if not Assigned(FParser) then
    ParseSourceCode(True);
  if Assigned(FParser) then
    Result := FParser.CountClasses
  else
    Result := 0;
end;

type
  TCrackActivityIndicator = class(TActivityIndicator);

procedure TFEditForm.FormResize(Sender: TObject);
begin
  FEditor.Invalidate;
end;

procedure TFEditForm.SetActivityIndicator(TurnOn: Boolean; Hint: string;
OnClick: TNotifyEvent);
begin
  ActivityIndicator.Left := Width div 2;
  ActivityIndicator.Top := Height div 2;
  ActivityIndicator.Visible := TurnOn;
  ActivityIndicator.Hint := Hint;
  ActivityIndicator.Animate := TurnOn;
  TCrackActivityIndicator(ActivityIndicator).OnClick := OnClick;
end;

procedure TFEditForm.ShowAssistantError(Msg: string);
begin
  ShowMessage(Msg);
end;

function TFEditForm.IsApplet: Boolean;
begin
  Result := (FFrameType in [4, 7]);
end;

function TFEditForm.IsAWT: Boolean;
begin
  Result := FFrameType in [2, 3, 4];
end;

function TFEditForm.FrameTypToString: string;
begin
  case FFrameType of
    8:
      Result := 'Application';
    7:
      Result := 'JApplet';
    6:
      Result := 'JDialog';
    5:
      Result := 'JFrame';
    4:
      Result := 'Applet';
    3:
      Result := 'Dialog';
    2:
      Result := 'Frame';
  else
    Result := '';
  end;
end;

procedure TFEditForm.SynEditChange(Sender: TObject);
begin
  FNeedsParsing:= True; // at every change of the source code
end;

end.
