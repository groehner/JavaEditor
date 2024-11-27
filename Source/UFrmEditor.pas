unit UFrmEditor;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SynEditPrint, SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, Menus, ComCtrls, ExtCtrls,
  SynEdit, USynEditEx, SynEditExport, SynEditHighlighter,
  UModel, UFrmBaseform, UJavaParser, UParseThread, Vcl.ToolWin,
  System.ImageList, Vcl.ImgList, Vcl.BaseImageCollection, SVGIconImageCollection,
  Vcl.VirtualImageList, TB2Item, SpTBXItem, SpTBXTabs;

  //  Application FrameType:= 8
  //  JApplet     FrameType:= 7
  //  JDialog     FrameType:= 6
  //  JFrame      FrameType:= 5
  //  Applet      FrameType:= 4
  //  Dialog      FrameType:= 3
  //  Frame       FrameType:= 2
  //  Console     FrameType:= 1

const
  cInsertBlink = 500;
  cOverBlink   = 200;

  ecWordLeft        = 5;    // Move cursor left one word
  ecWordRight       = 6;    // Move cursor right one word

type
  TLineInfo = (dlCurrentDebuggerLine, dlBreakpointLine, dlExecutableLine, dlSearchLine);
  TLineInfos = set of TLineInfo;

  { TFEditForm }

  TFEditForm = class(TFForm)
    PHaupt: TPanel;
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
    N1: TSpTBXSeparatorItem;
    MIExecute: TSpTBXItem;
    MIExecuteWithoutConsole: TSpTBXItem;
    MIExecuteWithConsole: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
    MIGit: TSpTBXSubmenuItem;
    MIGitStatus: TSpTBXItem;
    MIGitAdd: TSpTBXItem;
    MICommit: TSpTBXItem;
    MIGitLog: TSpTBXItem;
    N3: TSpTBXSeparatorItem;
    MGitReset: TSpTBXItem;
    MIGitCheckout: TSpTBXItem;
    MIGitRemove: TSpTBXItem;
    N4: TSpTBXSeparatorItem;
    MIGitRemote: TSpTBXItem;
    MIGitFetch: TSpTBXItem;
    MIGitPush: TSpTBXItem;
    N5: TSpTBXSeparatorItem;
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
    MIReleaseWindow: TSpTBXItem;
    MIFont: TSpTBXItem;
    MIConfiguration: TSpTBXItem;
    MIClose: TSpTBXItem;
    icBookmarks: TSVGIconImageCollection;
    vilBookmarksLight: TVirtualImageList;
    vilBookmarksDark: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction); override;
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure Enter(Sender: TObject); override;
    procedure UpdateState; override;
    procedure EditorGutterClick(Sender: TObject; Button: TMouseButton;
              X, Y, Line: Integer; mark: TSynEditMark);
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
    procedure EditorPaintTransient(Sender: TObject; aCanvas: TCanvas;
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
    procedure WMNCButtonDBLClick(var msg: TMessage); message WM_NCLBUTTONDBLCLK;
    procedure MIExecuteClick(Sender: TObject);
    procedure PopUpEditorPopup(Sender: TObject);
    procedure MIExecuteWithoutConsoleClick(Sender: TObject);
    procedure MIExecuteWithConsoleClick(Sender: TObject);
    procedure MIRenewImportsClick(Sender: TObject);
    procedure MICreateStructogramClick(Sender: TObject);
    procedure MICopyPathClick(Sender: TObject);
    procedure DoOnMouseOverToken(Sender: TObject; const Token: string; TokenType: Integer; Caret, P: TPoint; Attri: TSynHighlighterAttributes; var Highlight: Boolean);
    procedure DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnBuildStructure(Sender: TObject);
    procedure CreateTooltip(Caret, P: TPoint; const Token: string);
    procedure TVFileStructureChange(Sender: TObject; Node: TTreeNode);
    procedure MIClassOpenClick(Sender: TObject);
    procedure MIClassEditorClick(Sender: TObject);
    procedure MISearchDeclarationClick(Sender: TObject);
    procedure MIGitStatusClick(Sender: TObject);
    procedure MIGitAddClick(Sender: TObject);
    procedure MICommitClick(Sender: TObject);
    procedure MIGitGuiClick(Sender: TObject);
    procedure MIGitRemoveClick(Sender: TObject);
    procedure MIGitLogClick(Sender: TObject);
    procedure MGitResetClick(Sender: TObject);
    procedure MIGitCheckoutClick(Sender: TObject);
    procedure MIGitRemoteClick(Sender: TObject);
    procedure MIGitFetchClick(Sender: TObject);
    procedure MIGitPushClick(Sender: TObject);
    procedure MIGitConsoleClick(Sender: TObject);
    procedure MIGitViewerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIConfigurationClick(Sender: TObject);
    procedure ReleaseWindow(aFree: boolean); override;
    procedure MIReleaseWindowClick(Sender: TObject); override;
    procedure DesignButtonClick(Sender: TObject);
  private
    Bookmark: Integer;
    BreakPointCount: integer;
    MouseIsInBorderOfStructure: boolean;
    MouseBorderOfStructure: integer;
    MousePosition: TBufferCoord;
    ModifiedStrs: array[boolean] of string;
    InsertModeStrs: array[boolean] of string;
    ToolButtons: array[0..29] of TToolButton;
    FFrameType: integer;
    procedure Translate;
    procedure Statusline(i: Integer; const s: string);
    procedure CalculateStatusline;
    procedure setNeedsParsing(value: boolean);
    procedure setFileExtension(value: string);
    function getFrameType: Integer;
  public
    Editor: TSynEditEx;
    SynEditPrint: TSynEditPrint;
    DebuglineMark: TSynEditMark;
    Model: TObjectModel;
    Encoding: string;   // ANSI, UTF-8, UTF-16
    LineBreak: string;  // Windows, Unix, Mac
    Parameter: string;
    fFileExtension: string;
    StartOption: integer;
    EditorAge: TDateTime;
    CheckAgeEnabled: boolean;
    fNeedsParsing: boolean;
    fNeedToSyncFileStructure: boolean;
    ParseThread: TParseThread;
    Parser: TJavaParser;
    LastToken: string;
    isJUnitTestClass: boolean;
    constructor Create(AOwner: TComponent); override;
    procedure New(const Filename: string);
    procedure Open(const FileName: string; Zustand: string);
    procedure Save(withBackup: boolean); override;
    procedure SaveIn(const Dir: string); override;
    procedure SaveAs(const Filename: string); override;
    function GetSaveAsName: string; override;
    procedure SetHighlighter;
    procedure SetToolButtons;
    procedure PreparePrint;
    procedure Print; override;
    procedure PrintAll(AllPages: boolean);
    procedure Show;
    procedure Hide;
    procedure SetFont(aFont: TFont); override;
    function  GetFont: TFont; override;
    procedure SetFontSize(Delta: integer); override;
    procedure SetOptions; override;
    procedure SetDeleteBookmark(Xpos, YPos: Integer);
    procedure Unindent;
    procedure Indent;
    procedure Search; override;
    procedure SearchAgain; override;
    procedure Replace; override;
    procedure SystemOutPrintln;
    procedure Matchbracket;
    function  getIndent: string;
    procedure PutText(s: string);

    function  CBSearchClassOrMethod(Stop: Boolean; line: Integer): string;
    function  SourceContainsClass(const aClassname: string): boolean;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure GotoLine(i: integer); override;
    procedure SetModified(aModified: boolean); override;
    function GetModified: boolean; override;
    procedure DoExport; override;

    // Test-Menü
    procedure ClearBreakpoints;
    procedure SetBreakpoints;
    function HasBreakpoints: Boolean;
    function HasBreakpoint(ALine: integer; var Mark: TSynEditMark): Boolean;
    function IsExecutableLine(ALine: Integer): Boolean;
    procedure SetDebuglineMark(Line: Integer);
    procedure DeleteDebuglineMark;
    procedure DeleteBreakpoint(s: string);
    procedure ResetGutterOffset;

    procedure HTMLforApplet(const aWidth, aHeight, CharSet, Path, aClass: string; withJEApplets, Debug: boolean);
    function CurrentCol: Integer;
    function CurrentRow: Integer;
    procedure ExportToFile(const Filename: string; Exporter: TSynCustomExporter);
    procedure ExportToClipboard(asHtml: boolean; asText: boolean);
    procedure ExportWithNumbers;
    procedure ExportRTFNumbered;
    procedure SynEditorReplaceText(Sender: TObject;
      const ASearch, AReplace: string; Line, Column: Integer;
      var aAction: TSynReplaceAction);
    procedure SynEditorSpecialLineColors(Sender: TObject;
                Line: Integer; var Special: Boolean; var FG, BG: TColor);
    function GetLineInfos(ALine: integer): TLineInfos;
    function getMarksBreakpoints: string;
    procedure SetMarksBreakpoints(MarkBreakpoint: string);
    procedure InsertBreakpointMark(line: Integer);
    procedure DeleteBreakpointMark(Mark: TSynEditMark);
    procedure InsertGotoCursorBreakpoint;
    procedure InsertBreakpoint;
    procedure ParseSourcecodeWithThread(hasChanged: boolean);
    procedure ParseSourceCode(hasChanged: boolean);
    procedure CreateTVFileStructure;
    procedure RunTests;

    function getWidthAndHeight: TPoint;
    procedure ChangeWidthAndHeight(w, h: integer);
    procedure ReplaceWidthHeight(w, h: integer);

    function getLNGStartAttributes: integer;
    function getLNGEndAttributes: integer;
    function getLNGStartComponents: integer;
    function getLNGEndComponents: integer;
    function getLNGStartEventMethods: integer;
    function getLNGEndEventMethods: integer;
    function getLNG(Nr, ClassNumber: integer): string;
    procedure EnsureStartEnd; overload;
    procedure EnsureStartEnd(CorI: integer); overload;
    procedure InsertStartEnd;
    function hasStartAndEnd(CorI: integer): boolean;

    function getLineNumberWith(const s: string): integer;
    function getLineNumberWithFrom(From: integer; const s: string): integer;
    function getLineNumberWithFromTill(From, till: integer; const s: string): integer;
    function getLineNumberWithWord(const s: string): integer;
    function getLineNumberWithWordFrom(From: integer; const s: string): integer;
    function getLineNumberWithStartsWordFrom(From: integer; const s: string): integer;
    function getSource(LineS, LineE: integer): string;
    function getLine(Line: integer): string;
    function containsWord(const key: string; line: integer): boolean;

    procedure ReplaceLine(const s1, s2: string);
    procedure ReplaceLineWith(line: integer; const s: string);
    procedure ReplaceLineInLine(line: integer; const old, aNew: string);
    procedure ReplaceText(const s1, s2: string; all: boolean);
    procedure ReplaceTextWithRegex(const reg, s: string; all: boolean;
                                   from: integer = -1; till: integer = -1);
    procedure ReplaceWord(const s1, s2: string; all: boolean);
    procedure ReplaceComponentname(const s1, s2: string; Events: string);
    procedure ReplaceAttributAt(const At, key, s: string);
    procedure ReplaceAttribute(const key, s: string);
    procedure setAttributValue(const Container, key, s: string; after: integer);
    procedure ChangeAttributValue(const key, s: string); overload;  // Swing
    procedure ChangeAttributValue(const container, key, s: string); overload;  // FX

    function hasComponent(const key: string; line: integer): boolean;
    procedure InsertAttributValue(const destination, s: string; after: integer);
    procedure InsertLinesAt(line: integer; s: string); overload;
    procedure InsertLinesAt(const At, s: string); overload;
    procedure InsertAttributAfter(const At, Attribut: string);
    procedure InsertAttribute(ClassNumber: integer; const s: string); overload;
    procedure InsertAttribute(const Container, aIndent, Variable: string; fx: boolean); overload;
    procedure InsertAttribute(const Container, Variable: string; fx: boolean); overload;

    procedure InsertComponent(const s: string);
    procedure InsertProcedure(const aProcedure: string); overload;
    procedure InsertProcedure(ClassNumber: integer; const aProcedure: string); overload;
    procedure InsertListener(const Component, Listener: string);
    procedure InsertConstructor(ClassNumber: integer; const aProcedure: string);
    procedure InsertImport(const Package: string);

    procedure DeleteAttribute(const s: string);
    function DeleteAttributeValue(const s: string): boolean;
    procedure DeleteComponentValue(s: string);
    procedure DeleteAttributeValues(const s: string);
    procedure DeleteEmptyLines(line: integer);
    procedure DeleteLine(line: integer);
    procedure DeleteBlock(StartLine, EndLine: integer);
    procedure DeleteComponent(const Component: string);
    procedure DeleteComponentDefault(Control: TControl);
    procedure DeleteComponentTotal(ClassNumber: integer; const Component, Typ: string);
    procedure DeleteMethod(const Methode: string; SourcecodeCheck: boolean = true);
    procedure DeleteEventMethod(Method: string);
    procedure DeleteListener(Listener: string);
    procedure DeleteFXListener(Listener: string);
    procedure DeleteLambdaListener(Listener: string);
    procedure DeleteOldAddNewMethods(OldMethods, NewMethods: TStringList);
    procedure DeleteFXOldAddNewMethods(OldMethods, NewMethods: TStringList);
    procedure DeleteTryCatch(const key: string);

    procedure MoveBlock(from, till, dest, desttill: integer; const blanklines: string);
    function getBlock(from, lines: integer): string;
    procedure toForeground(Control: TControl);
    procedure toBackground(Control: TControl);

    procedure Go_To(const s: string);
    function getClassAttribut(const aClass, aAttribute: string): string;

    function hasText(const s: string): boolean;
    function hasWord(const s: string): boolean;
    function hasEventProcedureInModel(const aMethodname: string): boolean;
    function hasMainInModel: boolean;

    function isJava: boolean;
    function isPascal: boolean;
    function isHTML: boolean;
    function isCSS: boolean;
    function IsHTMLApplet: Boolean;

    function getFormType: string; override;
    function getState: string; override;
    procedure setState(var s: string); override;
    function EncodingAsString(const aEncoding: string): string;
    function LinebreakAsString: string;
    function LinebreakAsCtrls(const s: string): string;
    function getEncodingAsType: TEncoding;
    procedure SetEncoding(aEncoding: string);
    procedure AutomatedCompleteImports;
    procedure SetNewActionEventFormat;
    procedure CollectClasses(SL: TStringList); override;
    function getPackage: string;
    procedure CheckAge;
    procedure AddShortcutsToHints;
    function getAllPathnames: TStringList; override;
    function getAllClassnames: TStringList; override;
    function getJavaCodeAt(Caret: TPoint): string;
    function ClassnameDifferentFromAncestors(const aClassname: string): boolean;
    procedure InitShowCompileErrors;
    procedure setErrorMark(line, column: integer; const error: string);
    procedure ShowCompileErrors;
    procedure ClearCompilerErrorMarks;
    procedure ClearMarks;
    procedure TerminateThread(Sender: TObject);
    procedure ChangeStyle; override;
    procedure RemoveShortCutFromEditor(ShortCut: integer);
    procedure ReplaceShortCutFromEditor(ShortCut, ShortCut2: integer);
    procedure EditShortCuts;
    procedure CollapseGUICreation;
    procedure DoOnIdle;
    procedure SyncFileStructure;
    function MakeUpperEvents(Events: string): string;
    procedure SetFXBackgroundAsString(const Container, aName, aColor: string);
    procedure DPIChanged; override;

    property NeedsParsing: boolean read FNeedsParsing write setNeedsParsing;
    property FileExtension: string read fFileExtension write setFileExtension;
    property FrameType: integer read getFrameType write FFrameType;
  end;

  TInteger = class
  public
    i: integer;
    constructor create(aI: Integer);
  end;


implementation

{$R *.dfm}

uses
  Contnrs, Printers, Dialogs, SynHighlighterJava,
  Types, UITypes, Clipbrd, Math, StrUtils, DateUtils, RichEdit, ShellAPI,
  RegularExpressions, SynExportRTF, SynExportHTML,
  SynEditPrintTypes, UGUIDesigner, SynEditCodeFolding,
  SynEditTypes, SynEditMiscClasses,
  UJava, UJavaCommands, UUtils, UConfiguration, UTree,
  UModelEntity, UFileProvider, UMessages, UGUIForm, UCodeCompletion,
  UDlgConfirmReplace, UFrmUMLdiagram, UUMLModule, JvGnugettext, UStringRessources,
  UDebugger, UFileStructure, UFXComponents, UGit, UJUnitTest,
  UFrmSequencediagram, UJavaScanner, UTooltip, UObjectInspector, UJEComponents,
  UTemplates;

const
  ecMatchBracket    = 250;  // Go to matching bracket
  ecBlockIndent     = 610;  // Indent selection
  ecBlockUnindent   = 611;  // Unindent selection
  NoBreakpointImageIndex = 14;
  BreakpointImageIndex = 13;
  ErrorMarkIndex = 10;
  StopInAt = True;

var
  LNGs: array[1..6] of string;

constructor TInteger.create(ai: Integer);
begin
  inherited create;
  self.i:= aI;
end;

{--- TFEditForm ---------------------------------------------------------------}

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
  FormTag:= 1;
end;

procedure TFEditForm.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  Editor:= TSynEditEx.Create(Self);
  with Editor do begin
    MaxUndo:= 300;
    TabWidth:= 2;
    WantTabs:= True;
    PopupMenu:= PopUpEditor;
    BookMarkOptions.BookmarkImages:= vilBookmarksLight;
    Font.Assign(FConfiguration.EditFont);
    Options:= [eoAutoIndent, eoDragDropEditing, eoScrollPastEol, eoShowScrollHint,
               eoSmartTabs, eoTabIndent, eoTabsToSpaces, {eoTrimTrailingSpaces,}
               eoSmartTabDelete, eoGroupUndo, eoKeepCaretX, eoEnhanceHomeKey];
    Gutter.DigitCount:= 1;
    Gutter.LeftOffset:= 15;
    Gutter.Gradient:= true;
    Gutter.AutoSize:= true;
    UseCodeFolding:= true;
    Codefolding.IndentGuides:= true;
    SearchEngine:= FJava.SynEditSearch;
    Indent:= FConfiguration.Indent;
    StructureColorIntensity:= FConfiguration.StructureColorIntensity;
    OnBuildStructure:= DoOnBuildStructure;
    SetCaretBlinkTime(cInsertBlink);
    Gutter.Font.Assign(FConfiguration.Font);
    Gutter.Font.Height:= Editor.Font.Height + 2;
    Gutter.ShowLineNumbers:= FConfiguration.LineNumbering;
    OnGutterClick:= EditorGutterClick;
    OnSpecialLineColors:= SynEditorSpecialLineColors;
    OnKeyUp:= EditorKeyUp;
    OnKeyPress:= EditorKeyPress;
    OnStatusChange:= EditorStatusChange;
    OnReplaceText:= SynEditorReplaceText;
    OnMouseOverToken:= DoOnMouseOverToken;
    OnMouseDown:= DoOnMouseDown;
    if FConfiguration.EightyColumnLine
      then RightEdge:= 80
      else RightEdge:= 0;
  end;

  Editor.Parent:= PHaupt;
  Editor.Align:= alClient;
  EditformToolbar.Visible:= FConfiguration.vistoolbars[2];
  Encoding:= FConfiguration.getEncoding;
  CheckAgeEnabled:= FConfiguration.CheckAge;
  NeedsParsing:= false;
  Bookmark:= 0;
  BreakPointCount:= 0;
  EditorAge:= 0;
  Model:= TObjectModel.Create;
  Partner:= nil;
  Parser:= nil;
  Linebreak:= #13#10;
  FrameType:= 0;
  CalculateStatusline;
  EditorStatusChange(Sender, [scAll]);
  OnMouseActivate:= FormMouseActivate;
  SetOptions;
  ToMainPanel;
  ToolButtons[ 0]:= TBClose;
  ToolButtons[ 1]:= TBExplorer;
  ToolButtons[ 2]:= TBBrowser;
  ToolButtons[ 3]:= TBDesignform;
  ToolButtons[ 4]:= TBStructure;
  ToolButtons[ 5]:= TBClassOpen;
  ToolButtons[ 6]:= TBMatchBracket;
  ToolButtons[ 7]:= TBSystemOutPrintln;
  ToolButtons[ 8]:= TBStructureIndent;
  ToolButtons[ 9]:= TBIfStatement;
  ToolButtons[10]:= TBIfElseStatement;
  ToolButtons[11]:= TBWhileStatement;
  ToolButtons[12]:= TBForStatement;
  ToolButtons[13]:= TBDoWhileStatement;
  ToolButtons[14]:= TBSwitchStatement;
  ToolButtons[15]:= TBTryStatement;
  ToolButtons[16]:= TBBlockStatement;

  ToolButtons[17]:= TBComment;
  ToolButtons[18]:= TBIndent;
  ToolButtons[19]:= TBUnindent;
  ToolButtons[20]:= TBWordWrap;
  ToolButtons[21]:= TBBreakpoint;
  ToolButtons[22]:= TBBreakpointsClear;
  ToolButtons[23]:= TBBookmark;
  ToolButtons[24]:= TBGotoBookmark;
  ToolButtons[25]:= TBParagraph;
  ToolButtons[26]:= TBNumbers;
  ToolButtons[27]:= TBZoomOut;
  ToolButtons[28]:= TBZoomIn;
  ToolButtons[29]:= TBValidate;
  Translate;
end;

procedure TFEditForm.AddShortcutsToHints;
begin
  var s:= ShortCutToText(FJava.MIIndent.ShortCut);
  if Pos(s, TBIndent.Hint) = 0 then
    TBIndent.Hint:= TBIndent.Hint + ' - ' + s;
  s:= ShortCutToText(FJava.MIUnIndent.ShortCut);
  if Pos(s, TBUnIndent.Hint) = 0 then
    TBUnIndent.Hint:= TBUnIndent.Hint + ' - ' + s;
  s:= ShortCutToText(FJava.MIStructuredIndent.ShortCut);
  if Pos(s, TBStructureIndent.Hint) = 0 then
    TBStructureIndent.Hint:= TBStructureIndent.Hint + ' - ' + s;
  s:= ShortCutToText(FJava.MICommentOnOff.ShortCut);
  if Pos(s, TBComment.Hint) = 0 then
    TBComment.Hint:= TBComment.Hint + ' - ' + s;
  s:= ShortCutTotext(FJava.MISystemOutPrintln.ShortCut);
  if Pos(s, TBSystemOutPrintln.Hint) = 0 then
    TBSystemOutPrintln.Hint:= TBSystemOutPrintln.Hint + ' - ' + s;
  //s:=  _SmkcShift + _SmkcCtrl + '<i>';
  if Pos(s, TBBookmark.Hint) = 0 then
    TBBookmark.Hint:= TBBookmark.Hint + ' - ' + s;
  //s:= _SmkcCtrl + '<i>';
  if Pos(s, TBGotoBookmark.Hint) = 0 then
    TBGotoBookmark.Hint:= TBGotoBookmark.Hint + ' - ' + s;
end;

procedure TFEditForm.Translate;
begin
  TBIfStatement.Hint:= 'if ' + _(LNGStatement);
  TBIfElseStatement.Hint:= 'if-else ' + _(LNGStatement);
  TBWhileStatement.Hint:= 'while ' + _(LNGStatement);
  TBForStatement.Hint:= 'for ' + _(LNGStatement);
  TBDoWhileStatement.Hint:= 'do-while ' + _(LNGStatement);
  TBSwitchStatement.Hint:= 'switch ' + _(LNGStatement);
  TBTryStatement.Hint:= 'try ' + _(LNGStatement);
  TBBlockStatement.Hint:= 'block ' + _(LNGStatement);
  LNGs[1]:= _(LNGStartGUIVariables);
  LNGs[2]:= _(LNGEndGUIVariables);
  LNGs[3]:= _(LNGStartComponents);
  LNGs[4]:= _(LNGEndComponents);
  LNGs[5]:= _(LNGStartEventMethods);
  LNGs[6]:= _(LNGEndEventMethods);

  ModifiedStrs[false]:= '';
  ModifiedStrs[true]:= _(LNGModified);
  InsertModeStrs[false]:= _(LNGModusOverwrite);
  InsertModeStrs[true]:= _(LNGModusInsert);
  CalculateStatusline;
  if UUtils.Left(MIExecuteWithoutConsole.Caption, 4) <> '  ' then begin
    MIExecuteWithoutConsole.Caption:= '  ' + MIExecuteWithoutConsole.Caption;
    MIExecuteWithConsole.Caption:= '  ' +  MIExecuteWithConsole.Caption;
  end;
end;

procedure TFEditForm.New(const Filename: string);
begin
  Caption:= Filename;
  Pathname:= Filename;
  FileExtension:= LowerCase(ExtractFileExt(Filename));
  DesignButton.Visible:= FileExists(ChangeFileExt(Filename, '.jfm'));
  SetHighlighter;
  SetToolButtons;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  FJava.TabModified(Number, Modified);
  Enter(Self); // must stay!
  if Visible and Editor.CanFocus then Editor.SetFocus;
end;

procedure TFEditForm.Open(const FileName: string; Zustand: string);
begin
  try
    SetModified(false);
    Editor.Lines.LoadFromFile(Filename);
    // set UTF8 as default encoding
    if Editor.Lines.Encoding <> TEncoding.UTF8 then begin
      var Stream:= TMemoryStream.Create;
      Editor.Lines.SaveToStream(Stream, TEncoding.UTF8);
      Stream.Position:= 0;
      Editor.Lines.LoadFromStream(Stream, TEncoding.UTF8);
      FreeAndNil(Stream);
    end;
    Encoding:= EncodingAsString(Editor.Lines.Encoding.EncodingName);
    Linebreak:= Editor.Lines.LineBreak;
    Editor.ReplaceTabs(FConfiguration.TabWidth);
    if Editor.NeedsWordWrap then
      SBWordWrapClick(self);
    FileAge(Filename, EditorAge);
    New(Filename);
    Editor.ReadOnly:= (Pos(FConfiguration.JavaCache, Filename) = 1) or IsWriteProtected(FileName);
    if Editor.ReadOnly then
      Caption:= Caption + ' (' + _(LNGWriteProtected) + ')';
    CalculateStatusline;
    CollapseGUICreation;
    if IsJava then
      ParseSourcecode(true);
    SetState(Zustand);
    // ensure vertical scrollbar is visible
    Editor.ScrollBars:= ssNone;
    Editor.ScrollBars:= ssBoth;
  except
    on e: Exception do begin
      ErrorMsg(e.Message);
      FConfiguration.Log('TFEditForm.Open: ' + Filename, e);
    end;
  end;
end;

procedure TFEditForm.CollapseGUICreation;
  var i, Line: integer; s, Name: string;
begin
  if FConfiguration.GUICodeFolding and (FrameType > 1) then begin
    Name:= ChangeFileExt(ExtractFilename(Pathname), '');
    case FrameType of
      8: s:= 'public void start(Stage';
      7, 4: s:= 'public void init()';
      6, 5, 3, 2: s:= 'public ' + Name + '(';
    end;
    for i:= 0 to min(Editor.AllFoldRanges.Count - 1, 2) do begin
      Line:= Editor.AllFoldRanges[i].FromLine;
      if Pos(s, Editor.Lines[Line-1]) > 0 then
        Editor.Collapse(i)
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
  if fNeedToSyncFileStructure and IsJava then begin
    FFileStructure.ShowEditorCodeElement;
    fNeedToSyncFileStructure := False;
  end;
end;

procedure TFEditForm.EnsureStartEnd;
begin
  EnsureStartEnd(1);
end;

procedure TFEditForm.EnsureStartEnd(CorI: integer);
begin
 if assigned(Editor) and not Editor.ReadOnly and not hasStartAndEnd(CorI) then
   InsertStartEnd;
end;

function PointToDisplay(P: TPoint): TDisplayCoord;
begin
  Result.Column:= P.x;
  Result.Row:= P.Y;
end;

procedure TFEditForm.SynEditorReplaceText(Sender: TObject;
  const ASearch, AReplace: string; Line, Column: Integer;
  var aAction: TSynReplaceAction);
var
  APos: TDisplayCoord;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    aAction := raSkip
  else begin
    APos := DisplayCoord(Column, Line);
    APos := PointToDisplay(Editor.ClientToScreen(Editor.RowColumnToPixels(APos)));
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    with TFConfirmReplace.Create(FJava) do begin
      PrepareShow(EditRect, APos.Column, APos.Row, APos.Row + Editor.LineHeight, ASearch);
      case ShowModal of
        mrYes:      aAction:= raReplace;
        mrYesToAll: aAction:= raReplaceAll;
        mrNo:       aAction:= raSkip;
        else        aAction:= raCancel;
      end;
      Free;
    end;
  end;
end;

procedure TFEditForm.SetHighlighter;
begin
  if FConfiguration.NoSyntaxHighlighting
    then Editor.Highlighter:= nil
    else Editor.Highlighter:= FConfiguration.GetHighlighter(FileExtension);

  if Editor.Highlighter = nil then
    Editor.StructureColoring:= false
  else begin
    Editor.StructureColoring:= FConfiguration.StructureColoring;
    Editor.UseCodeFolding:= true;
  end;
  if Editor.StructureColoring and IsJava then
    NeedsParsing:= true;
end;

procedure TFEditForm.SetToolButtons;
begin
  for var i:= 0 to 29 do
    ToolButtons[i].Visible:= true;
  if not isJava then begin
    TBDesignform.Visible:= false;
    TBStructure.Visible:= false;
    TBClassOpen.Visible:= false;
    TBSystemOutPrintln.visible:= false;
    TBBreakpoint.Visible:= false;
    TBBreakpointsClear.Visible:= false;
    TBStructureIndent.Visible:= Pos('{', Editor.Text) > 0;
    TBIfStatement.Visible:= false;
    TBIfElseStatement.Visible:= false;
    TBWhileStatement.Visible:= false;
    TBForStatement.Visible:= false;
    TBDoWhileStatement.Visible:= false;
    TBSwitchStatement.Visible:= false;
    TBTryStatement.Visible:= false;
    TBBlockStatement.Visible:= false;
  end;
  if isPascal then begin
    TBClassOpen.Visible:= true;
    TBStructureIndent.Visible:= false;
  end;
  TBBrowser.Visible:= isHTML;
  TBValidate.Visible:= isHTML or isCSS;
end;

procedure TFEditForm.Print;
begin
  if hasDefaultPrinter
    then PrintAll(false)
    else ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFEditForm.PrintAll(AllPages: boolean);
begin
  if hasDefaultPrinter then begin
    if Editor.Font.Size >= 13 then
      if MessageDlg(Format(_('Font size is %3d pt. Print anyhow?'), [Editor.Font.Size]), mtConfirmation,[mbYes, mbNo], 0) = mrNo
        then exit;
    try
      PreparePrint;
      FJava.PrintDialog.MaxPage:= SynEditPrint.PageCount;   // invalid Printer exception
      FJava.PrintDialog.ToPage := SynEditPrint.PageCount;
      var Options:= FJava.PrintDialog.Options;
      if Editor.SelAvail
        then include(Options, poSelection)
        else exclude(Options, poSelection);
      FJava.PrintDialog.Options:= Options;
      if AllPages then
        SynEditPrint.Print
      else if FJava.PrintDialog.Execute then begin
        SynEditPrint.Colors:= FConfiguration.PrintColored;
        case FJava.PrintDialog.PrintRange of
          prAllPages : SynEditPrint.Print;
          prPageNums : SynEditPrint.PrintRange(FJava.PrintDialog.FromPage,
                                               FJava.PrintDialog.ToPage);
          prSelection: begin SynEditPrint.SelectedOnly:= true; SynEditPrint.Print; end;
        end;
        SetPrinterIndex(Printer.PrinterIndex);
        FConfiguration.WriteStringU('Printer', 'Printer', Printer.Printers[Printer.PrinterIndex]);
      end;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  end else
    ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFEditForm.PreparePrint;
var
  s, s1: string;
  p: Integer;
  AFont: TFont;

  procedure EditMacro(var s: string);
    var p: Integer; Macro: string;
  begin
    Macro:= '%FILE%';
    p:= Pos(Macro, Uppercase(s));
    if p > 0 then begin
      Delete(s, p, Length(Macro));
      Insert(ExtractFilename(Pathname), s, p);
    end;
    Macro:= '%PATH%';
    p:= Pos(Macro, UpperCase(s));
    if p > 0 then begin
      Delete(s, p, Length(Macro));
      Insert(Pathname, s, p);
    end;
    Macro:= '%DATE%';
    p:= pos(Macro, Uppercase(s));
    if p > 0 then begin
      Delete(s, p, Length(Macro));
      Insert('$DATE$', s, p);
    end;
    Macro:= '%TIME%';
    p:= pos(Macro, Uppercase(s));
    if p > 0 then begin
      Delete(s, p, Length(Macro));
      Insert('$TIME$', s, p);
    end;
    Macro:= '%PAGENUM%';
    p:= pos(Macro, Uppercase(s));
    if p > 0 then begin
      Delete(s, p, Length(Macro));
      Insert('$PAGENUM$', s, p);
    end;
    Macro:= '%PAGECOUNT%';
    p:= Pos(Macro, Uppercase(s));
    if p > 0 then begin
      Delete(s, p, Length(Macro));
      Insert('$PAGECOUNT$', s, p);
    end;
  end;

begin
  AFont := TFont.Create;
  if not assigned(SynEditPrint) then SynEditPrint:= TSynEditPrint.Create(Self);
  with SynEditPrint do begin
    AFont.Assign(Header.DefaultFont);
    AFont.Size:= Header.DefaultFont.Size-2;

    DocTitle    := _('File from Java-Editor');
    SelectedOnly:= (FJava.PrintDialog.PrintRange = prSelection);
    Highlight   := assigned(Editor.HighLighter);
    HighLighter := Editor.Highlighter;
    Colors      := False;
    LineNumbers := FConfiguration.WithLinenumbers;
    LineNumbersInMargin:= FConfiguration.LinenumbersInMargin;

    Margins.UnitSystem:= usMM;
    Margins.Left  := FConfiguration.BorderLeft;
    Margins.Top   := FConfiguration.BorderTop;
    Margins.Right := FConfiguration.BorderRight;
    Margins.Bottom:= FConfiguration.BorderBottom;

    Header.Clear;
    s:= FConfiguration.Header;
    EditMacro(s);
    p:= Pos('#', s);
    s1:= Copy(s, 1, p-1);
    if s1 <> '' then Header.Add(s1, AFont, taLeftJustify, 1);
    if p > 0 then delete(s, 1, p);
    p:= Pos('#', s);
    s1:= Copy(s, 1, p-1);
    if s1 <> '' then Header.Add(s1, AFont, taCenter, 1);
    if p > 0 then delete(s, 1, p);
    if s <> '' then Header.Add(s, AFont, taRightJustify, 1);

    Footer.Clear;
    s:= FConfiguration.Footer;
    EditMacro(s);
    p:= Pos('#', s);
    s1:= Copy(s, 1, p-1);
    if s1 <> '' then Footer.Add(s1, AFont, taLeftJustify, 1);
    if p > 0 then delete(s, 1, p);
    p:= Pos('#', s);
    s1:= Copy(s, 1, p-1);
    if s1 <> '' then Footer.Add(s1, AFont, taCenter, 1);
    if p > 0 then delete(s, 1, p);
    if s <> '' then Footer.Add(s, AFont, taRightJustify, 1);

    SynEditPrint.SynEdit:= Editor;
  end;
  FreeAndNil(AFont);
end;

procedure TFEditForm.SetFont(aFont: TFont);
begin
  Editor.Font.Assign(aFont);
  Editor.Gutter.Font.Assign(aFont);
  Editor.Gutter.Font.Height:= aFont.Height + 2;
  FConfiguration.EditFont.Assign(aFont);
end;

function TFEditForm.GetFont: TFont;
begin
  Result:= Editor.Font;
end;

procedure TFEditForm.SetFontSize(Delta: integer);
begin
  Editor.Font.Size:= Editor.Font.Size + Delta;
  if Editor.Font.Size < 6 then Editor.Font.Size:= 6;
  Editor.Gutter.Font.Size:= Editor.Font.Size;
  FConfiguration.EditFont.Assign(Editor.Font);
  FConfiguration.EditFont.Size:= PPIUnscale(FConfiguration.EditFont.Size);
end;

procedure TFEditForm.SetOptions;
begin
  Editor.TabWidth:= FConfiguration.TabWidth;
  Editor.Indent:= FConfiguration.Indent;
  var Options:= Editor.Options;
  if FConfiguration.AutomaticIndent
    then Include(Options, eoAutoIndent)
    else Exclude(Options, eoAutoIndent);
  if FConfiguration.IndentHelp
    then Include(Options, eoSmartTabs)
    else Exclude(Options, eoSmartTabs);
  if FConfiguration.CursorBehindLine
    then Include(Options, eoScrollPastEol)
    else Exclude(Options, eoScrollPastEol);
  Include(Options, eoAltSetsColumnMode);
  if FConfiguration.ShowBracketPair
    then Editor.OnPaintTransient:= EditorPaintTransient
    else Editor.OnPaintTransient:= nil;
  if FConfiguration.EightyColumnLine
    then Editor.RightEdge:= 80
    else Editor.RightEdge:= 0;

  Editor.ActiveLineColor:= FConfiguration.ActiveLineColor;
  Editor.StructureColorIntensity:= FConfiguration.StructureColorIntensity;
  Editor.PaintStructurePlane:= FConfiguration.StructureColoringPlane;
  Editor.Options:= Options;
  EditFormToolbar.Visible:= FConfiguration.vistoolbars[2];
  Editor.Gutter.ShowLineNumbers:= FConfiguration.LineNumbering;
  Editor.Gutter.Font.Assign(FConfiguration.Font);
  Editor.Gutter.Font.Height:= Editor.Font.Height + 2;
  CheckAgeEnabled:= FConfiguration.CheckAge;
  SetHighlighter;
  MICreateStructogram.Visible:= not FConfiguration.LockedStructogram;
  EditShortCuts;
  FConfiguration.RemoveShortcutsFrom(PopupEditor);
end;

procedure TFEditForm.Save(withBackup: boolean);
  var BackupName, Ext: string; aEncoding: TEncoding;
      Form: TFForm;
begin
  if Editor.ReadOnly then exit;
  if ExtractFilePath(Pathname) = ''
    then Pathname:= FConfiguration.Sourcepath + Pathname;
  if Uppercase(ExtractFileExt(Pathname)) = '.XML' then  // due to Android Mode
    withBackup:= false;
  if withBackup then begin
    BackupName:= Pathname;
    Ext:= ExtractFileExt(Pathname);
    if length(ext) >= 2
      then Ext[2]:= '~'
      else Ext:= '.~';
    BackupName:= ChangeFileExt(BackupName, Ext);
    if FileExists(BackupName) then
      DeleteFile(PChar(BackupName));
    if FileExists(Pathname) then
      RenameFile(Pathname, BackupName);
  end;
  try
    Editor.Lines.LineBreak:= Linebreak;
    aEncoding:= getEncodingAsType;
    if aEncoding = TEncoding.UTF8 then
      Editor.Lines.WriteBOM:= false
    else if aEncoding = TEncoding.Unicode then
      Editor.Lines.WriteBOM:= true;
    Editor.Lines.SaveToFile(Pathname, aEncoding);

    FileAge(Pathname, EditorAge);
    if Assigned(Partner) then
      Partner.Save(withBackup);
    Form:= FJava.getTDIWindowType(Pathname, '%Q%');
    if assigned(Form) then (Form as TFSequenceForm).RefreshFromEditor;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
  SetModified(false);
  Statusline(1, '');
  FMessages.StatusMessage(Pathname + ' ' + _(LNGSaved));
  EditorStatusChange(Self, []);
end;

procedure TFEditForm.SetModified(aModified: boolean);
begin
  Editor.Modified:= aModified;
  inherited SetModified(aModified);
end;

function TFEditForm.GetModified: boolean;
begin
  Result:= inherited GetModified or assigned(Editor) and Editor.Modified;
end;

procedure TFEditForm.SaveIn(const Dir: string);
begin
  SaveAs(Dir + ExtractFilename(Pathname));
end;

procedure TFEditForm.SaveAs(const Filename: string);
begin
  if (Pos(FConfiguration.JavaCache, Filename) = 1) and Editor.ReadOnly
    then exit;
  var OldName:= ChangeFileExt(ExtractFileName(Pathname), '');
  var NewName:= ChangeFileExt(ExtractFileName(Filename), '');
  if FConfiguration.RenameWhenSave and IsJava then
    with Editor do begin
      ReplaceWord('public class ' + OldName, 'public class ' + NewName, false);
      ReplaceWord('public interface ' + OldName, 'public interface ' + NewName, false);
      ReplaceWord('public ' + OldName, 'public ' + NewName, true);
      ReplaceText('new ' + OldName + '();', 'new ' + NewName + '();', true);
      ReplaceText('new ' + OldName + '("' + OldName + '");', 'new ' + NewName + '("' + NewName + '");', true);  // old files
      if FrameType = 8 then
        ReplaceText('primaryStage.setTitle("' + OldName + '");',
                    'primaryStage.setTitle("' + NewName + '");', false)
      else
        ReplaceText('setTitle("' + OldName + '");',
                    'setTitle("' + NewName + '");', false);
      ReplaceWord('// end of class ' + OldName, '// end of class ' + NewName, false);
    end;
  if OldName <> '' then  // new file?
    FJava.RenameTabAndWindow(Number, Filename);
  if FileExists(Filename) then DeleteFile(PChar(Filename));
  Pathname:= Filename;
  Editor.ReadOnly:= false;
  Save(WithoutBackup);
  Editor.Lines.LoadFromFile(Filename);  // due to possible change of encoding or linebreak
  try
    Caption:= Filename;
  except on e: Exception do
    FConfiguration.Log('TFEditForm.Caption: ' + Filename, e);
  end;
  FileExtension:= LowerCase(ExtractFileExt(Filename));
  SetHighlighter;
  SetToolButtons;
end;

procedure TFEditForm.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  var s:= _('Line') + ': %4d  ' + _('Column') + ': %3d';
  Statusline(0, Format(s, [Editor.CaretY, Editor.CaretX]));
  if Editor.ReadOnly
    then Statusline(1, ' ' + _(LNGWriteProtected) + ' ')
    else Statusline(1, ModifiedStrs[Modified]);
  Statusline(2, InsertModeStrs[Editor.InsertMode]);
  Statusline(3, ' ' + EncodingAsString(Encoding) + '/' + LinebreakAsString + ' ');

  if (Changes * [scModified] <> []) then begin
    NeedsParsing:= isJava;
    FJava.TabModified(Number, Modified);
  end;

  if Changes * [scSelection] <> [] then
    UpdateState;

  if scCaretY in Changes then
    fNeedToSyncFileStructure:= true;

  if assigned(Editor) and assigned(Editor.Lines) then begin
    var Digits:= Length(IntToStr(Editor.Lines.Count));
    if Digits <> Editor.Gutter.DigitCount then
      Editor.Gutter.DigitCount:= Digits;
  end;

  FFileStructure.ShowSelected;
end;

procedure TFEditForm.PutText(s: string);
  var p, OffX, OffY, x, y: Integer; s1: string;
begin
  try
    OffY:= 0;
    p:= Pos('|', s);
    s1:= copy(s, 1, p-1);
    delete(s, p, 1);
    p:= Pos(#13#10, s1);
    while p > 0 do begin
      inc(OffY);
      delete(s1, 1, p+1);
      p:= Pos(#13#10, s1);
    end;
    OffX:= Length(s1) + 1;
    with Editor do begin
      if SelText = '' then begin
        x:= CaretX;
        y:= CaretY;
        end
      else begin
        x:= BlockBegin.Char;
        y:= BlockBegin.Line;
      end;
      SelText:= s;
      CaretY:= y + OffY;
      if OffY = 0
        then CaretX:= x + OffX - 1
        else CaretX:= OffX;
      EnsureCursorPosVisible;
    end;
  except
  end;
  NeedsParsing:= true;
end;

procedure TFEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsJava then
    FFileStructure.Clear;
  ClearCompilerErrorMarks;
  if not AlreadySavedAs and Modified then begin
    FJava.DoSave(Self, true);
    AlreadySavedAs:= true;
  end;
  CanClose:= True;
end;

procedure TFEditForm.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  try
    LockEnter:= true;
    if Assigned(Partner) then begin
      Partner.Close;
      Partner:= nil;
      FObjectInspector.SetSelectedObject(nil);
    end;
    if assigned(FJava.Editorform) and (FJava.Editorform.pathname = pathname) then
      FJava.EditorForm:= nil;
    ClearMarks;
    for var i:= TVFileStructure.Items.Count - 1 downto 0 do begin
      var aInteger:= TInteger(TVFileStructure.Items[i].Data);
      FreeAndNil(aInteger);
    end;
  finally
    inherited;
    if FJava.TDIFormsList.Count = 0 then
      TThread.ForceQueue(nil, procedure
      begin
        FJava.UpdateMenuItems(Self);
      end);
  end;
end;

procedure TFEditForm.FormDestroy(Sender: TObject);
begin
  if assigned(ParseThread) then begin
    if (0 < ParseThread.State) and (ParseThread.State < 3) then begin
      ParseThread.abort:= true;
      ParseThread.WaitFor;
    end;
    FreeAndNil(ParseThread);
  end;
  if assigned(FTooltip) then begin
    FTooltip.Hide;
    FreeAndNil(FTooltip);
  end;
  FreeAndNil(Parser);
  FreeAndNil(TVFileStructure);
  FreeAndNil(Model);
  FreeAndNil(Editor);
end;

procedure TFEditForm.ClearBreakpoints;
  var i: Integer; s: string;
      Mark: TSynEditMark;
begin
  with Editor do
    for i:= Marks.Count-1 downto 0 do begin
      Mark:= Marks.Items[i];
      if (Mark.ImageIndex = BreakpointImageIndex) or
         (Mark.ImageIndex = NoBreakpointImageIndex)
        then begin
          if myDebugger.Running then begin
            s:= CBSearchClassOrMethod(not StopInAt, Mark.Line);
            if s <> '' then
              myDebugger.NewCommand(2, s);
          end;
          InvalidateLine(Mark.Line);
          Marks.Remove(Mark);
        end;
    end;
  BreakPointCount:= 0;
  ResetGutterOffset;
end;

procedure TFEditForm.ClearCompilerErrorMarks;
begin
  with Editor do
    for var i:= Marks.Count-1 downto 0 do begin
      var Mark:= Marks.Items[i];
      if Mark.ImageIndex = ErrorMarkIndex then begin
        InvalidateLine(Mark.Line);
        Marks.Remove(Mark);
      end;
    end;
  ResetGutterOffset;
end;

procedure TFEditForm.ClearMarks;
begin
  with Editor do
    for var i:= Marks.Count-1 downto 0 do begin
      var Mark:= Marks.Items[i];
      InvalidateLine(Mark.Line);
      Marks.Remove(Mark);
    end;
  ResetGutterOffset;
end;

procedure TFEditForm.SetBreakpoints;
begin
  with Editor do begin
    for var i:= 0 to Marks.Count-1 do begin
      var Mark:= Marks.Items[i];
      if Mark.ImageIndex = BreakpointImageIndex then begin
        ParseSourcecode(false);
        var s:= CBSearchClassOrMethod(StopInAt, Mark.Line);
        if s = ''
          then Mark.ImageIndex:= NoBreakpointImageIndex
          else myDebugger.NewCommand(1, s);
      end;
    end;
  end;
end;

function TFEditForm.HasBreakpoints: Boolean;
begin
  Result:= false;
  if assigned(Editor) and assigned(Editor.Marks) then
    for var i:= 0 to Editor.Marks.Count - 1 do
      if Editor.Marks.Items[i].ImageIndex = BreakpointImageIndex then
        Exit(true)
end;

procedure TFEditForm.SetDebuglineMark(Line: Integer);
  const DebugImageIndex = 12;
  var i: Integer;
      LineMarks: TSynEditMarks;
begin
  DeleteDebuglineMark;
  DebugLineMark:= TSynEditMark.Create(Editor);
  Editor.Marks.GetMarksForLine(Line, LineMarks);
  for i:= 1 to MAX_MARKS do
    if not assigned(LineMarks[i]) then break;
  DebugLineMark.Line:= Line;
  with DebuglineMark do begin
    ImageIndex:= DebugImageIndex;
    Visible := true;
  end;
  Editor.Marks.Add(DebuglineMark);
  Editor.InvalidateLine(Line);
  Editor.CaretY:= Line;
  Editor.EnsureCursorPosVisibleEx(true);
  Editor.Gutter.LeftOffset:= 30;
end;

procedure TFEditForm.DeleteDebuglineMark;
  var i, Line: Integer;
      LineMarks: TSynEditMarks;
begin
  if DebugLineMark = nil then exit;
  if assigned(Editor) and assigned(Editor.Marks) then begin
    Line:= DebugLineMark.Line;
    Editor.Marks.Remove(DebuglineMark);
    DebuglineMark:= nil;
    Editor.Marks.GetMarksForLine(Line, LineMarks);
    for i:= 1 to MAX_MARKS do
      if not assigned(LineMarks[i])
        then break
        else LineMarks[i].Visible:= true;
    Editor.InvalidateLine(Line);
    ResetGutterOffset;
  end;
end;

procedure TFEditForm.DeleteBreakpoint(s: string);
  var Mark: TSynEditMark;
      i, p: Integer;
begin
  // Format:
  // Unable to set breakpoint BinBaum:23 : No code at line 23 in BinBaum
  // Unable to set deferred breakpoint BinBaum:23 : No code at line 23 in BinBaum
  p:= Pos(':', s);
  delete(s, 1, p);
  p:= Pos(' :', s);
  delete(s, p, length(s));
  if TryStrToInt(s, p) then
    for i:= 0 to Editor.Marks.Count-1 do begin
      Mark:= Editor.Marks.Items[i];
      if (Mark.ImageIndex = BreakpointImageIndex) and (Mark.Line = p)
        then Editor.Marks.Items[i].ImageIndex:= NoBreakpointImageIndex;
    end;
end;

procedure TFEditForm.ResetGutterOffset;
begin
  if (BreakPointCount = 0) and assigned(Editor.Gutter) and
     assigned(MyDebugger) and not myDebugger.Running then
    if Editor.Gutter.ShowLineNumbers then
      Editor.Gutter.LeftOffset:= 15;
end;

procedure TFEditForm.HTMLforApplet(const aWidth, aHeight, CharSet, Path, aClass: string; withJEApplets, Debug: boolean);
begin
  var archive:= FConfiguration.getAppletArchiv;
  // http://java.sun.com/docs/books/tutorial/deployment/applet/deployindex.html
  with Editor.Lines do begin
    Add('<!DOCTYPE html>');
    Add('<html>');
    Add('<head>');
    Add('<title>' + aClass + '-Applet</title>');
    Add('<meta http-equiv="Content-Type" content="text/html;charset=' + CharSet + '">');
    Add('</head>');
    Add('<body>');
    Add('<h1>' + aClass + '-Applet</h1>');
    Add('<hr>');
    Add('<applet code="' + ReplaceStr(aClass, '.', '/') + '.class" ' + archive + ' width="' + aWidth + '" height="' + aHeight + '">');
    Add('</applet>');
    Add('<hr>');
    Add('</body>');
    Add('</html>');
  end;
  Modified:= True;
end;

function TFEditForm.IsHTMLApplet: Boolean;
begin
  Result:= isHTML and (hasWord('Applet') or hasWord('JApplet'));
end;

function TFEditForm.GetSaveAsName: string;
begin
  Result:= Pathname;
  if IsJava then begin
    ParseSourcecode(false);
    var Ci:= Model.ModelRoot.GetAllClassifiers;
    while Ci.HasNext do begin
      var cent:= TClassifier(Ci.Next);
      if ((cent is TClass) or (cent is TInterface)) and (cent.Visibility = viPublic) and (cent.Pathname = Pathname) then
        exit(ExtractFilepath(Pathname) + WithoutGeneric(cent.ShortName) + '.java');
    end;
  end;
end;

procedure TFEditForm.Enter(Sender: TObject);
  var i, api: integer; TC: TSpTBXTabControl;
begin
  if LockEnter then exit;
  FJava.scpSetEditForm(Self);
  FJava.EditorForm:= Self; // must stay in before inherited!
  inherited;
  EditorStatusChange(Sender, []);
  if Assigned(Partner) then
    FGUIDesigner.ChangeTo(TFGUIForm(Partner));
  if assigned(TVFileStructure) and (FFileStructure.myForm <> self) and
    assigned(TVFileStructure.Items) then
    FFileStructure.init(TVFileStructure.Items, Self);
  TC:= FJava.TabsControl;
  api:= TC.ActiveTabIndex;
  for i:= 1 to maxTab do  // 0 is tab Program
    if FConfiguration.vistabs[i] then
      case i of
        1:       TC.Items[i].Visible:= (FrameType in [0, 1, 2, 3, 4]);  // Frame, Dialog, Applet
        2, 3:    TC.Items[i].Visible:= (FrameType in [0, 1, 5, 6, 7]);  // JFrame, JDialog, JApplet
        4, 5:    TC.Items[i].Visible:= (FrameType < 8);                 // Layout, Utilities
        6, 7, 8: TC.Items[i].Visible:= (FrameType in [0, 1, 8]);        // FX Base, FX Controls, FX Shapes
      end;
  case FrameType of
    2..4: TC.ActiveTabIndex:= 1;
    5..7: if (api < 2) or (api > 3) then TC.ActiveTabIndex:= 2;
    8:    if api < 6 then TC.ActiveTabIndex:= 6;
    else  TC.ActiveTabIndex:= 0;
  end;
end;

procedure TFEditForm.Statusline(i: Integer; const s: string);
begin
  StatusBar.Panels[i].Text:= s;
end;

procedure TFEditForm.CalculateStatusline;
  var s: string;
begin
  StatusBar.Constraints.MinHeight:= Canvas.TextHeight('Ag') + 4;
  var aWidth:= StatusBar.Canvas.TextWidth('_' + _('Line') + ':_9999_' + _('Column') + ':_999_');
  StatusBar.Panels[0].Width:= max(aWidth + 10, StatusBar.Panels[1].Width);
  if Editor.ReadOnly
    then aWidth:= StatusBar.Canvas.TextWidth('_' + _(LNGWriteProtected) + '_')
    else aWidth:= StatusBar.Canvas.TextWidth('_' + _(LNGModified) + '_');
  StatusBar.Panels[1].Width:= max(aWidth + 10, StatusBar.Panels[1].Width);
  if Length(_(LNGModusOverwrite)) > Length(_(LNGModusInsert))
    then s:= _(LNGModusOverwrite)
    else s:= _(LNGModusInsert);
  aWidth:= StatusBar.Canvas.TextWidth('_' + s + '_');
  StatusBar.Panels[2].Width:= max(aWidth + 10, StatusBar.Panels[2].Width );
  StatusBar.Panels[3].Width:= ClientWidth;
end;

procedure TFEditForm.UpdateState;
begin
  var sa:= Editor.SelAvail;
  var sb:= MouseIsInBorderOfStructure;
  with FJava do begin
    SetEnabledMI(MIUndo, Editor.CanUndo);
    SetEnabledTB(TBUndo, MIUndo.Enabled);
    SetEnabledMI(MIRedo, Editor.CanRedo);
    SetEnabledTB(TBRedo, MIRedo.Enabled);

    SetEnabledMI(MICut, sa or sb);
    SetEnabledMI(MICopy, sa or sb);
    SetEnabledMI(MICopyNormal, sa);
    SetEnabledMI(MICopyRTF, sa);
    SetEnabledMI(MICopyHTML, sa);
    SetEnabledMI(MICopyHTMLAsText, sa);
    SetEnabledMI(MICopyNumbered, sa);
    SetEnabledMI(MICopyRtfNumbered, sa);
    SetEnabledMI(MIPaste, Editor.CanPaste);
  end;
  SetEnabledMI(MIUndo, Editor.CanUndo);
  SetEnabledMI(MIRedo, Editor.CanRedo);
  SetEnabledMI(MICut, sa or sb);
  SetEnabledMI(MICopy, sa or sb);
  SetEnabledMI(MIInsert, Editor.CanPaste);
end;

function TFEditForm.CurrentCol: Integer;
begin
  Result:= Editor.CaretX;
end;

function TFEditForm.CurrentRow: Integer;
begin
  Result:= Editor.CaretY;
end;

function TFEditForm.HasBreakpoint(ALine: integer; var Mark: TSynEditMark): Boolean;
begin
  Result:= False;
  for var i:= 0 to Editor.Marks.Count-1 do begin
    Mark:= Editor.Marks.Items[i];
    if ((Mark.ImageIndex = BreakpointImageIndex) or (Mark.ImageIndex = NoBreakpointImageIndex))
        and (Mark.Line = ALine) then
      exit(True);
  end;
end;

function TFEditForm.IsExecutableLine(ALine: Integer): Boolean;
begin
  Result:= true;
  for var i:= 0 to Editor.Marks.Count-1 do begin
    var Mark:= Editor.Marks.Items[i];
    if (Mark.ImageIndex = NoBreakpointImageIndex) and (Mark.Line = ALine) then
      exit(false);
  end;
end;

procedure TFEditForm.InsertBreakpointMark(line: Integer);
  var s: string; i: integer;
      Mark: TSynEditMark;
      Marks: TSynEditMarks;
begin
  Editor.Marks.GetMarksForLine(line, Marks);
  i:= 1;
  while assigned(Marks[i]) and (i <= Max_Marks) do
    if (Marks[i].ImageIndex in [BreakPointImageIndex, NoBreakPointImageIndex])
      then exit
      else inc(i);

  Mark:= TSynEditMark.Create(Editor);
  Mark.Line := line;
  Mark.Char := 1;
  Mark.ImageIndex:= BreakPointImageIndex;
  Mark.Visible := True;
  Mark.InternalImage:= false;

  ParseSourcecode(false);
  s:= CBSearchClassOrMethod(StopInAt, line);
  if s = '' then
    Mark.ImageIndex:= NoBreakpointImageIndex
  else if myDebugger.Running then
    myDebugger.NewCommand(1, s);
  Editor.Marks.Add(Mark);
  inc(BreakPointCount);
  Editor.Gutter.LeftOffset:= 24;
  Editor.InvalidateLine(line);
end;

procedure TFEditForm.DeleteBreakpointMark(Mark: TSynEditMark);
begin
  var s:= CBSearchClassOrMethod(not StopInAt, Mark.Line);
  if myDebugger.Running and (s <> '') then
    myDebugger.NewCommand(2, s);
  Editor.InvalidateLine(Mark.Line);
  Editor.Marks.Remove(Mark);
  Dec(BreakPointCount);
  ResetGutterOffset;
end;

procedure TFEditForm.InsertGotoCursorBreakpoint;
  var s: string; line: integer;
      Mark: TSynEditMark;
begin
  line:= Editor.CaretY;
  if not HasBreakpoint(line, Mark) then begin
    s:= CBSearchClassOrMethod(StopInAt, line);
    myDebugger.RunToCursorBreakpoint(s);
  end;
end;

procedure TFEditForm.InsertBreakpoint;
begin
  EditorGutterClick(Self, mbLeft, Editor.BookMarkOptions.XOffset, 0, Editor.CaretY, nil);
end;

procedure TFEditForm.EditorGutterClick(Sender: TObject; Button: TMouseButton;
            X, Y, Line: Integer; Mark: TSynEditMark);
begin
  if Line > Editor.Lines.Count then Exit;
  if x > Editor.Gutter.RealGutterWidth(Editor.Gutter.Font.Size) - Editor.CodeFolding.GutterShapeSize - 2 * Editor.Gutter.RightMargin then exit;
  if X < Editor.BookMarkOptions.XOffset then  // Bookmark
    SetDeleteBookmark(1, Line)
  else begin
    if not FJava.MIBreakpoint.Enabled or Editor.SelAvail then Exit;
    with Editor do
      if HasBreakPoint(Line, Mark) then
        DeleteBreakpointMark(Mark)
      else begin
        Editor.CaretY:= Line;
        InsertBreakpointMark(Line);
      end;
  end;
end;

function TFEditForm.GetLineInfos(ALine: integer): TLineInfos;
  var Mark: TSynEditMark;
begin
  Result:= [];
  if ALine > 0 then begin
    if assigned(myJavaCommands) and myDebugger.Running and
       assigned(DebugLineMark) and (ALine = DebugLineMark.Line)
    then
      Include(Result, dlCurrentDebuggerLine);
    if assigned(myJavaCommands) and myDebugger.Running and
       assigned(FMessages) and (ALine = FMessages.SearchGoalLine) and
      (FMessages.SearchGoalPath = Pathname)
    then
      Include(Result, dlSearchLine);
    if IsExecutableLine(ALine) then
      Include(Result, dlExecutableLine);
    if HasBreakpoint(ALine, Mark) then
      Include(Result, dlBreakpointLine);
  end;
end;

procedure TFEditForm.SynEditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  Special:= true;
  var LI := GetLineInfos(Line);
  if dlCurrentDebuggerLine in LI then begin
    FG := clWhite;
    BG := clBlue;
    exit;
  end;
  if dlSearchLine in LI then begin
    FG:= clWhite;
    BG:= clMaroon;
    exit;
  end;
  if dlBreakpointLine in LI then begin
    FG := clWhite;
    if dlExecutableLine in LI
      then BG := clRed
      else BG := clGray;
    exit;
  end;
  Special:= false;
end;

procedure TFEditForm.SetDeleteBookmark(XPos, YPos: Integer);
  var i, x, y: Integer;
begin
  with Editor do begin
    i:= 0;
    while (i < 10) do begin
      if GetBookmark(i, x, y) and (y = YPos) then begin
        ClearBookmark(i);
        Exit;
      end;
      inc(i);
    end;
    i:= 0;
    while (i < 10) and IsBookMark(i) do inc(i);
    if i < 10 then begin
      SetBookMark(i, XPos, YPos);
      end
    else
      Windows.Beep(600, 200);
  end;
end;

procedure TFEditForm.Unindent;
begin
  with Editor do begin
    TabWidth:= FConfiguration.Indent;
    if SelAvail
      then CommandProcessor(ecBlockUnindent, #0, nil)
    else begin
      var x:= CaretX;
      if MouseIsInBorderOfStructure
        then SelStructure(MouseBorderOfStructure)
        else SelEnd:= SelStart + 1;
      CommandProcessor(ecBlockUnindent, #0, nil);
      SelEnd:= SelStart;
      CaretX:= x - TabWidth;
    end;
    TabWidth:= FConfiguration.TabWidth;
  end;
end;

procedure TFEditForm.Indent;
begin
  with Editor do begin
    TabWidth:= FConfiguration.Indent;
    if SelAvail
      then CommandProcessor(ecBlockIndent, #0, nil)
    else begin
      var x:= CaretX;
      if MouseIsInBorderOfStructure
        then SelStructure(MouseBorderOfStructure)
        else SelEnd:= SelStart + 1;
      CommandProcessor(ecBlockIndent, #0, nil);
      SelEnd:= SelStart;
      CaretX:= x + TabWidth;
    end;
    TabWidth:= FConfiguration.TabWidth;
  end;
end;

procedure TFEditForm.SystemOutPrintln;
begin
  PutText('System.out.println(|);');
end;

procedure TFEditForm.TVFileStructureChange(Sender: TObject; Node: TTreeNode);
begin
  var line:= TInteger(Node.Data).i;
  with Editor do begin
    CaretY:= line;
    CaretX:= Length(Lines[line-1]) + 1;
    EnsureCursorPosVisible;
    //Topline:= line;
    if CanFocus then SetFocus;
  end;
end;

procedure TFEditForm.Matchbracket;
begin
  Editor.CommandProcessor(ecMatchBracket, #0, nil);
end;

function TFEditForm.getIndent: string;
  var Ind: integer;
begin
  if Editor.SelText = ''
    then Ind:= Editor.CaretX - 1
    else Ind:= Editor.BlockBegin.Char;
  Result:= StringOfChar(' ', Ind);
end;

procedure TFEditForm.GotoLine(i: Integer);
begin
  if i = 0 then exit;
  Editor.Topline:= i;
  Editor.CaretX := 1;
  Editor.CaretY := i;
  Editor.EnsureCursorPosVisible;
end;

procedure TFEditForm.Undo;
begin
  Editor.Undo;
  if Editor.CanFocus then Editor.SetFocus;
end;

procedure TFEditForm.MIUndoClick(Sender: TObject);
begin
  Undo;
end;

procedure TFEditForm.Redo;
begin
  Editor.Redo;
  if Editor.CanFocus then Editor.SetFocus;
end;

procedure TFEditForm.MIRedoClick(Sender: TObject);
begin
  Redo;
end;

procedure TFEditForm.ReleaseWindow(aFree: boolean);
begin
  if aFree then begin
    Parent:= nil;
    Align:= alNone;
    BorderStyle:= bsSizeable;
    BorderIcons:= [biSystemMenu, biMaximize];
    MIReleaseWindow.Caption:= 'Fenster reinholen';
    SetBounds(Random(200), Random(200), FJava.MainPanel.Width, FJava.MainPanel.Height);
    FJava.DeleteTab(Number);
  end else begin
    Parent:= FJava.MainPanel;
    Align:= alClient;
    BorderStyle:= bsNone;
    BorderIcons:= [];
    MIReleaseWindow.Caption:= 'Fenster freigeben';
    FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  end;
end;

procedure TFEditForm.MIReleaseWindowClick(Sender: TObject);
begin
  if Parent = FJava.MainPanel
    then ReleaseWindow(true)
    else ReleaseWindow(false);
end;

procedure TFEditForm.MICutClick(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TFEditForm.CutToClipboard;
begin
  if (FJava.ActiveTool = 17) and assigned(Partner)
    then Partner.CutToClipboard
  else begin
    with Editor do
      if not SelAvail and MouseIsInBorderOfStructure then
        SelStructure(MouseBorderOfStructure);
    Editor.CutToClipboard;
  end;
end;

procedure TFEditForm.MICommitClick(Sender: TObject);
  var m: string;
begin
  if InputQuery('Commit', 'Message', m) then
    FGit.GitCall('commit -m "' + m + '"', ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIConfigurationClick(Sender: TObject);
begin
  FConfiguration.OpenAndShowPage('Editor');
end;

procedure TFEditForm.MICopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TFEditForm.MICopyPathClick(Sender: TObject);
begin
  Clipboard.AsText:= Pathname;
end;

procedure TFEditForm.MICreateStructogramClick(Sender: TObject);
  var s, aText, Filename: string; Scanner: TJavaScanner;
begin
  try
    if Editor.SelAvail then
      aText:= Editor.SelText
    else if MouseIsInBorderOfStructure then begin
      s:= Clipboard.AsText;
      Editor.SelStructure(MouseBorderOfStructure);
      Editor.CopyToClipboard;
      aText:= Clipboard.AsText;
      Clipboard.AsText:= s;
      Editor.SelEnd:= Editor.SelStart;
    end;
    if aText <> '' then begin
      Scanner:= TJavaScanner.create;
      Scanner.Init(aText);
      Filename:= Scanner.getFilename;
      FreeAndNil(Scanner);
      if Filename <> ''
        then Filename:= extractFilePath(Pathname) + Filename + '.jsg'
        else Filename:= ChangeFileExt(Pathname, '.jsg');
      FJava.StructogramFromText(aText, Filename);
    end;
  except on e: exception do begin
    FConfiguration.Log('TFEditForm.MICreateStructogramClick', e);
    ErrorMsg(e.Message);
    end;
  end;
end;

procedure TFEditForm.CopyToClipboard;
begin
  if (FJava.ActiveTool = 17) and assigned(Partner) then
    Partner.CopyToClipboard
  else begin
    with Editor do
      if not SelAvail and MouseIsInBorderOfStructure then
        SelStructure(MouseBorderOfStructure);
    try
      Editor.CopyToClipboard;
    except
    end;
  end;
end;

procedure TFEditForm.MIInsertClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TFEditForm.PasteFromClipboard;
begin
  if (FJava.ActiveTool = 17) and assigned(Partner) then
    Partner.PasteFromClipboard
  else begin
    try
      Editor.PasteFromClipboard;
      Modified:= true;
    except on e: Exception do
      //ErrorMsg(e.Message);
    end;
    ParseSourcecode(true);
  end;
end;

procedure TFEditForm.PopUpEditorPopup(Sender: TObject);
begin
  MouseBorderOfStructure:= 0;
  MouseIsInBorderOfStructure:= Editor.MouseInBorderOfStructure(MouseBorderOfStructure);
  if not Editor.getPositionOfMouse(MousePosition) then
    MousePosition.Char:= -1;
  UpdateState;
  MISearchDeclaration.Visible:= isJava;
  MIAPIHelp.Visible:= isJava;
  MIClassOpen.Visible:= isJava;
  MIClassEditor.Visible:= isJava;
  MICreateStructogram.Visible:= isJava;
  MIExecute.Visible:= isJava;
  MIExecuteWithoutConsole.Visible:= isJava;
  MIExecuteWithConsole.Visible:= isJava;
  MIRenewImports.Visible:= isJava;
  MIGit.Visible:= FConfiguration.GitOK;
end;

procedure TFEditForm.MIUnindentClick(Sender: TObject);
begin
  Unindent;
end;

procedure TFEditForm.MIIndentClick(Sender: TObject);
begin
  Indent;
end;

procedure TFEditForm.MIExecuteClick(Sender: TObject);
begin
  var s:= Editor.SelText;
  if (s = '') or not IsJava
    then FJava.TBRunClick(Self)
    else FMessages.Execute(s);
end;

procedure TFEditForm.MIExecuteWithoutConsoleClick(Sender: TObject);
begin
  myJavaCommands.ConsoleMode:= 1;
  FJava.TBRunClick(self);
end;

procedure TFEditForm.MIExecuteWithConsoleClick(Sender: TObject);
begin
  myJavaCommands.ConsoleMode:= 2;
  FJava.TBRunClick(self);
end;

procedure TFEditForm.EditorKeyPress(Sender: TObject; var Key: Char);
  var s: string; i: integer; empty: boolean;
begin
  if Key = '{' then begin
    s:= Editor.LineText;
    empty:= (trim(s) = '');
    i:= 0;
    while (i < Length(s)) and (s[i+1] = ' ') do
      inc(i);
    if FConfiguration.IndentAfterBracket and not FConfiguration.AddClosingbracket then
      s:= '{' + #13#10 + StringOfChar(' ', i + FConfiguration.Indent) + '|'
    else if FConfiguration.AddClosingBracket then begin
      s:= '{' + #13#10 + StringOfChar(' ', i + FConfiguration.Indent) + '|'#13#10 + StringOfChar(' ', i) + '}';
      if not empty then s:= s + #13#10;
      end
    else s:= '{|';
    PutText(s);
    Key:= #0;
  end;
  if Key = #27 then begin
    if assigned(FTooltip) then
      FTooltip.Hide;
    FJava.scpJava.Form.Hide;
    FJava.scpParams.Form.Hide;
  end;
  if assigned(ParseThread) and (ParseThread.State > 0) and (ParseThread.State < 3) then
    ParseThread.Abort:= true;
  // debugging: FJava.Memo1.lines.Add('EditorKeypress');
end;

function BufferToPoint(B: TBufferCoord): TPoint;
begin
  Result.X:= B.Char;
  Result.Y:= B.Line;
end;

function PointToBuffer(P: TPoint): TBufferCoord;
begin
  Result.Char:= P.X;
  Result.Line:= P.Y;
end;

procedure TFEditForm.EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  const
    controlstruc : array[1..8] of string = ('if', 'while', 'for', 'do', 'switch',
                                            'try', 'else', '} else');
  var Kuerzel: TNode;
      s, s1, hex: string;
      p, q, hexint, i, Tokentyp, Start: integer;
      P2: TPoint;
      Attri: TSynHighlighterAttributes;
begin
  if ((ssCtrl in Shift) and (Key = Ord('V'))) or
     ((ssShift in Shift) and (Key = VK_Insert)) then
    Editor.ReplaceTabs(FConfiguration.TabWidth);

  // Shortcuts
  if ((VK_F1 <= Key) and (Key <= VK_F12)) or
     (ssCtrl in Shift) or (ssShift in Shift) or (ssAlt in Shift) then begin
    Kuerzel:= FConfiguration.KeyboardShortcutsTree.getNode(ShortCut(Key, Shift));
    if Assigned(Kuerzel) then
      PutText(Kuerzel.Data);
  end;

  // show tooltip
  if (Key = VK_F2) and FConfiguration.TooltipWithKey and isJava then begin
    Editor.GetHighlighterAttriAtRowColEx(Editor.CaretXY, S, TokenTyp, Start, Attri);
    if Tokentyp = Ord(SynHighlighterJava.tkIdentifier) then begin
      P2:= Editor.RowColumnToPixels(DisplayCoord(Start, Editor.CaretY +1));
      CreateTooltip(BufferToPoint(Editor.CaretXY), Editor.ClientToScreen(P2), '# VK_F2 #');
    end;
  end;

   // Unicode
  if ((ssCtrl in Shift) and (ssAlt in Shift) and (Key = Ord('U'))) then begin
    s:= Editor.LineText;
    p:= Editor.CaretX;
    if p < 5
      then hex:= copy(s, 1, p-1)
      else hex:= copy(s, p-4, 4);
    q:= Pos(' ', hex);
    while (0 < q) and (q < length(hex)) do begin
      delete(hex, 1, q);
      q:= Pos(' ', hex);
    end;
    q:= length(hex);
    try
      hexint:= StrToInt('$' + hex);
      insert(Char(hexint), s, p);
      delete(s, p-q, q);
      Editor.LineText:= s;
      Editor.CaretX:= Editor.CaretX - q + 1;
    except
    end;
    Key:= 0;
  end;

  // control structures
  if FConfiguration.InsertControlStructures and IsJava and
    ((Key = Ord(' ')) or ((ssShift in Shift) and (Key = Ord('8'))))
  then begin
    s:= Trim(Editor.LineText);
    case Key of
      Ord(' '): s1:= '';
      Ord('8'): s1:= '(';
    end;
    for i:= 1 to 8 do
      if s = controlstruc[i] + s1 then begin
        p:= Pos(s, Editor.LineText);
        if i > 1 then begin
          Editor.LineText:= '';
          Editor.CaretX:= p;
        end;
        FTemplates.SBControlStructures(Self, i, true);
        Key:= 0;
        break;
      end;
  end;

  // semicolons
  if FConfiguration.InsertSemicolons and IsJava and (Key = VK_Return) then begin
    i:= Editor.CaretY - 2;
    s:= Editor.Lines[i];
    if (s <> '') and (Pos(';', s) = 0) and FMessages.NeedsSemicolon(s) then
      Editor.Lines[i]:= s + ';';
  end;

end;

// compare to procedure TFUMLForm.CreateTVFileStructure;
procedure TFEditForm.CreateTVFileStructure;
  var
    Ci, it: IModelIterator;
    cent: TClassifier;
    Attribute: TAttribute;
    Method: TOperation;
    ImageNr, i, indented, indentedOld: Integer;
    CName: string;
    Node: TTreeNode;
    ClassNode: TTreeNode;
    aInteger: TInteger;

  function CalculateIndented(const aClassname: string): integer;
    var i: integer;
  begin
    Result:= 0;
    for i:= 1 to length(aClassname) do
      if CharInSet(aClassname[i], ['$', '.']) then inc(Result);
  end;

begin
  indented:= 0;
  Classnode:= nil;
  if not isJava then exit;
  TVFileStructure.Items.BeginUpdate;
  try
    for i:= TVFileStructure.Items.Count - 1 downto 0 do begin
      aInteger:= TInteger(TVFileStructure.Items[i].Data);
      FreeAndNil(aInteger);
    end;
    TVFilestructure.Items.Clear;
    Ci:= Model.ModelRoot.GetAllClassifiers;
    while Ci.HasNext do begin
      cent := TClassifier(Ci.Next);
      if Cent.Pathname <> Pathname then continue;
      if endsWith(Cent.Name, '[]') then continue;

      if (Cent is TClass)
        then isJUnitTestClass:= (Cent as TClass).isJUnitTestclass
        else isJunitTestClass:= false;

      CName:= cent.ShortName;
      indentedOld:= indented;
      indented:= CalculateIndented(CName);
      while Pos('$', CName) + Pos('.', CName) > 0 do begin
        delete(CName, 1, Pos('$', CName));
        delete(CName, 1, Pos('.', CName));
      end;

      if (cent is TClass)
        then ImageNr:= 1
        else ImageNr:= 11;

      if indented = 0 then
        ClassNode:= TVFileStructure.Items.AddObject(nil, CName, TInteger.create(cent.LineS))
      else if indented > indentedOld then
        ClassNode:= TVFileStructure.Items.AddChildObject(ClassNode, CName, TInteger.create(cent.LineS))
      else begin
        while indented <= indentedOld do begin
          dec(indentedOld);
          ClassNode:= ClassNode.Parent;
        end;
        ClassNode:= TVFileStructure.Items.AddChildObject(ClassNode, CName, TInteger.create(cent.LineS));
      end;

      ClassNode.ImageIndex:= ImageNr;
      ClassNode.SelectedIndex:= ImageNr;
      ClassNode.HasChildren:= true;

      it:= cent.GetAttributes;
      while It.HasNext do begin
        Attribute:= It.Next as TAttribute;
        ImageNr:= Integer(Attribute.Visibility) + 2;
        Node:= TVFileStructure.Items.AddChildObject(ClassNode,
          Attribute.toShortString, TInteger.create(Attribute.LineS));
        Node.ImageIndex:= ImageNr;
        Node.SelectedIndex:= ImageNr;
        Node.HasChildren:= false;
      end;
      It:= cent.GetOperations;
      while It.HasNext do begin
        Method:= It.Next as TOperation;
        if Method.OperationType = otConstructor
          then ImageNr:= 6
          else ImageNr:= Integer(Method.Visibility) + 7;
        Node:= TVFileStructure.Items.AddChildObject(ClassNode,
          Method.toShortString, TInteger.create(Method.LineS));
        Node.ImageIndex:= ImageNr;
        Node.SelectedIndex:= ImageNr;
        Node.HasChildren:= false;
      end;
    end;
  finally
    TVFileStructure.Items.EndUpdate;
    FFileStructure.init(TVFileStructure.Items, Self);
  end;
  //FJava.Memo1.lines.AddStrings(Model.ModelRoot.Debug);
end;

procedure TFEditForm.RunTests;
begin
  if FJUnitTests = nil then
    FJUnitTests:= TFJUnitTests.Create(FJava);
  FJunitTests.Pathname:= Pathname;
  var Ci:= Model.ModelRoot.GetAllClassifiers;
  if Ci.HasNext then begin
    var cent := TClassifier(Ci.Next);
    if (Cent is TClass) and (Cent as TClass).isJUnitTestclass then
      myJavaCommands.RunTests(Cent as TClass, 'Class');
  end;
end;

function TFEditForm.GetLNG(Nr, ClassNumber: integer): string;
begin
  if ClassNumber = 0
    then Result:= LNGs[Nr]
    else Result:= LNGs[Nr] + IntToStr(ClassNumber);
end;

procedure TFEditForm.InsertStartEnd;
  var
    Ci, it: IModelIterator;
    cent: TClassifier;
    Attribute: TAttribute;
    Method: TOperation;
    Lines: array[1..6] of integer;
    LNG: array[1..6] of string;
    XY: TBufferCoord;
    Ind, s1, s2: string;
    ClassNumber, i, ce, cs, line: integer;
    SL: TStringList;

  procedure CheckConstructorAndComponentsPosition;
    var Line: integer;
  begin
    if (Method.OperationType = otConstructor) or
       (isApplet and (Method.Name = 'init')) or
       ((FrameType = 8) and (Method.Name = 'start'))
    then begin
      Lines[3]:= Method.LineE;
      Lines[4]:= Method.LineE;
      case FrameType of
      8: begin
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'new Scene');
        if (-1 < Line) and (Line < Method.LineE) then begin
          Lines[3]:= Line + 1;
          Lines[4]:= Line + 1;
        end;
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'primaryStage.setOnCloseRequest');
        if (-1 < Line) and (Line < Method.LineE) then
          Lines[4]:= Line;
      end;
      7: begin
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'cp.setBounds');
        if (-1 < Line) and (Line < Method.LineE) then
          Lines[3]:= Line + 1;
        Lines[4]:= Method.LineE - 1;
      end;
      6: begin
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'cp.setLayout(null)');
        if (-1 < Line) and (Line < Method.LineE) then begin
          Lines[3]:= Line + 1;
          Lines[4]:= Line + 1;
        end;
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'setResizable(false)');
        if (-1 < Line) and (Line < Method.LineE) then
          Lines[4]:= Line;
      end;
      5: begin
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'cp.setLayout(null)');
        if (-1 < Line) and (Line < Method.LineE) then begin
          Lines[3]:= Line + 1;
          Lines[4]:= Line + 1;
        end;
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'setVisible(true)');
        if (-1 < Line) and (Line < Method.LineE) then
          Lines[4]:= Line;
      end;
      4: begin
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'add(cp)');
        if (-1 < Line) and (Line < Method.LineE) then
          Lines[3]:= Line + 1;
        Lines[4]:= Method.LineE - 1;
      end;
      3: begin
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'add(cp)');
        if (-1 < Line) and (Line < Method.LineE) then begin
          Lines[3]:= Line + 1;
          Lines[4]:= Line + 1;
        end;
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'setResizable(false)');
        if (-1 < Line) and (Line < Method.LineE) then
          Lines[4]:= Line;
      end;
      2: begin
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'add(cp)');
        if (-1 < Line) and (Line < Method.LineE) then begin
          Lines[3]:= Line + 1;
          Lines[4]:= Line + 1;
        end;
        Line:= getLineNumberWithFromTill(Method.LineS, Method.LineE, 'setVisible(true)');
        if (-1 < Line) and (Line < Method.LineE) then
          Lines[4]:= Line;
      end;
     end;
    end else
      Lines[5]:= min(Lines[5], Method.LineS - 1);
  end;

  procedure Add(c: char; Line: integer; const s: string);
  begin
    SL.Add(RightStr('000000' + IntToStr(Line), 6) + c + s);
  end;

begin
  if not IsJava then exit;
  Editor.LockUndo;
  Editor.BeginUpdate;
  for i:= 1 to 6 do begin
    line:= getLineNumberWith(GetLNG(i, 0));
    if line > -1 then DeleteLine(line);
  end;

  ParseSourcecode(false);
  Ci := Model.ModelRoot.GetAllClassifiers;
  Ind:= FConfiguration.Indent1;
  XY:= Editor.CaretXY;
  ClassNumber:= -1;
  SL:= TStringList.Create;
  SL.Sorted:= true;
  while Ci.HasNext do begin
    cent:= TClassifier(Ci.Next);
    if (cent.Pathname <> Pathname) or cent.Anonym then continue;
    inc(ClassNumber);
    cs:= cent.LineS;
    ce:= max(cs, cent.LineE-1);

    Lines[1]:= cs; LNG[1]:= GetLNG(1, ClassNumber);
    Lines[2]:= cs; LNG[2]:= GetLNG(2, ClassNumber);
    Lines[3]:= ce; LNG[3]:= GetLNG(3, ClassNumber);
    Lines[4]:= ce; LNG[4]:= GetLNG(4, ClassNumber);
    Lines[5]:= ce; LNG[5]:= GetLNG(5, ClassNumber);
    Lines[6]:= ce; LNG[6]:= GetLNG(6, ClassNumber);

    it:= (cent as TClassifier).GetOperations;
    if it.HasNext then begin
      Method:= It.Next as TOperation;
      CheckConstructorAndComponentsPosition;
      while it.HasNext do begin
        Method:= It.Next as TOperation;
        CheckConstructorAndComponentsPosition;
      end;
    end;

    it:= (cent as TClassifier).GetAttributes;
    if it.HasNext then begin
      Attribute:= It.Next as TAttribute;
      Lines[1]:= Attribute.LineS - 1;
      Lines[2]:= Attribute.LineE;
      while it.HasNext do begin
        Attribute:= It.Next as TAttribute;
        Lines[2]:= Attribute.LineE;
      end;
    end;

    if Lines[2] <= Lines[1] then Lines[2]:= Lines[1];
    if Lines[4] <= Lines[3] then Lines[4]:= Lines[3];
    if Lines[6] <= Lines[5] then Lines[6]:= Lines[5];

    if getLineNumberWithWord(LNG[6]) = -1 then
      Add('F', Lines[6], Ind + LNG[6] + CrLf);
    if getLineNumberWithWord(LNG[5]) = -1 then
      Add('E', Lines[5], Ind + LNG[5] + CrLf);
    if FrameType > 1 then begin
      if getLineNumberWithWord(LNG[4]) = -1 then
        Add('D', Lines[4], Ind + Ind + LNG[4] + CrLf);
      if getLineNumberWithword(LNG[3]) = -1 then
        Add('C', Lines[3], Ind + Ind + LNG[3] + CrLf);
    end;
    if getLineNumberWithWord(LNG[2]) = -1 then
      Add('B', Lines[2], Ind + LNG[2] + CrLf);
    if getLineNumberWithWord(LNG[1]) = -1 then
      Add('A', Lines[1], Ind + LNG[1] + CrLf);
  end;

  for i:= SL.Count - 1 downto 0 do begin
    s2:= SL.Strings[i];
    s1:= copy(SL.Strings[i], 1, 6);
    delete(s2, 1, 7);
    InsertLinesAt(StrToInt(s1), s2);
  end;
  SL.Clear;
  FreeAndNil(SL);
  Editor.CaretXY:= XY;
  Editor.UnlockUndo;
  Editor.EndUpdate;
end;

function TFEditForm.hasMainInModel: boolean;
  var Ci, it: IModelIterator;
      cent: TModelEntity;
begin
  Result:= true;
  if IsJava then begin
    if isApplet then  // Applets have "public void init()"
      exit;
    ParseSourceCode(false);
    Ci:= Model.ModelRoot.GetAllClassifiers;
    while Ci.HasNext do begin
      cent:= Ci.Next;
      if (cent is TClass) then begin
        It:= (cent as TClass).GetOperations;
        while It.HasNext do
          if (It.Next as TOperation).hasMain then exit;
      end;
    end;
  end;
  Result:= false;
end;

procedure TFEditForm.CollectClasses(SL: TStringList);
begin
  if IsJava then begin
    ParseSourcecode(false);
    var Ci:= Model.ModelRoot.GetAllClassifiers;
    while Ci.HasNext do begin
      var cent:= Ci.Next;
      if (cent is TClass) or (cent is TInterface) then
        SL.Add(WithoutArray(cent.Name));
    end;
  end;
end;

function TFEditForm.CBSearchClassOrMethod(Stop: Boolean; line: Integer): string;
  var aClassname, aMethodname: string;
      Ci, it: IModelIterator;
      cent: TClassifier;
      Method: TOperation;
      found: boolean;
      ClassInLine: Integer;
begin
  // ParseSourcecode;  to be done by caller
  Result:= '';
  if trim(Editor.Lines[line-1]) = '' then exit('');

  found:= false;
  ClassInLine:= -1;
  Ci:= Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext and not found do begin
    cent:= TClassifier(Ci.Next);
    if cent.Pathname = Pathname then begin
      if cent.LineS > line then break;
      if (cent.LineS <= Line) and (Line <= cent.LineE) then begin
        aClassname:= cent.Name;
        ClassInLine:= cent.LineS;
        aMethodname:= '';
        It:= cent.GetOperations;
        while It.HasNext and not found do begin
          Method:= It.Next as TOperation;
          if Method.LineS = line then begin
            aMethodname:= Method.Name;
            found:= true;
          end;
        end;
      end;
    end;
  end;
  if aClassname = '' then exit;

  if Stop then begin
    if found
      then Result:= 'stop in ' + aClassname + '.' + aMethodname
      else if line > ClassInLine then
        Result:= 'stop at ' + aClassname + ':' + IntToStr(line)
    end
  else // clear
    if found
      then Result:= 'clear ' + aClassname + '.' + aMethodname
      else if line > ClassInLine then
         Result:= 'clear ' + aClassname + ':' + IntToStr(line)
end;

function TFEditForm.SourceContainsClass(const aClassname: string): boolean;
begin
  Result:= false;
  if Model = nil then
    ParseSourcecode(true);

  var Ci:= Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext and not Result do begin
    var cent:= TClassifier(Ci.Next);
    if (cent is TClass) and
       ((cent.ShortName = aClassname) or (cent.Name = aClassname)) and
       (cent.Pathname = Pathname) then
      Result:= true;
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

procedure TFEditForm.ExportToFile(const Filename: string; Exporter: TSynCustomExporter);
begin
  with Exporter do begin
    Highlighter := Editor.Highlighter;
    ExportAsText := true;
    ExportAll(Editor.Lines);
    try
      SaveToFile(Filename);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  end;
end;

procedure TFEditForm.ExportToClipboard(asHtml: boolean; asText: boolean);
  var Exporter: TSynCustomExporter;
begin
  if asHtml then begin
    Exporter:= TSynExporterHTML.Create(Self);
    if not asText then
      TSynExporterHTML(Exporter).CreateHTMLFragment:= true;
  end else
    Exporter:= TSynExporterRTF.Create(Self);
  var Lines:= TStringList.Create;
  Lines.Text:= Editor.SelText;
  with Exporter do begin
    Highlighter:= Editor.Highlighter;
    ExportAsText:= asText;
    ExportAll(Lines);
    Exporter.CopyToClipboard;
  end;
  FreeAndNil(Lines);
  FreeandNil(Exporter);
end;

  function getFormatted(i, arity: integer): string;
  begin
    var s:= intToStr(i);
    while length(s) < arity do
      s:= '0' + s;
    result:= s + ' ';
  end;

procedure TFEditForm.ExportRTFNumbered;
begin
  var Exporter:= TSynExporterRTF.Create(Self);
  var Lines:= TStringList.Create;
  Lines.Text:= Editor.SelText;

  var arity:= 3;
  if Lines.Count < 100 then arity:= 2;
  if Lines.Count < 10  then arity:= 1;
  for var i:= 0 to Lines.Count-1 do
    Lines[i]:= getFormatted(i+1, arity) + Lines[i];
  with Exporter do begin
    Highlighter:= Editor.Highlighter;
    Font.Assign(Editor.Font);
    ExportAsText:= false;
    ExportAll(Lines);
    Exporter.CopyToClipboard;
  end;
  FreeAndNil(Lines);
  FreeAndNil(Exporter);
end;

procedure TFEditForm.ExportWithNumbers;
begin
  var Lines:= TStringList.Create;
  Lines.Text:= Editor.SelText;
  var arity:= 3;
  if Lines.Count < 100 then arity:= 2;
  if Lines.Count < 10  then arity:= 1;
  for var i:= 0 to Lines.Count-1 do
    Lines[i]:= getFormatted(i+1, arity) + Lines[i];
  Clipboard.AsText:= Lines.Text;
  FreeAndNil(Lines);
end;

procedure TFEditForm.DoExport;
  var folder: string;
      Exporter: TSynCustomExporter;
begin
  with FJava.SDSaveAs do begin
    Title:= _(LNGExportTo);
    Filter:= 'RTF (*.rtf)|*.rtf|HTML (*.html)|*.html;*.htm';
    Filename:= GetSaveAsName;
    folder:= ExtractFilePath(Filename);
    Filename:= ChangeFileExt(Filename, '');
    if folder <> ''
      then InitialDir:= folder
      else InitialDir:= FConfiguration.Sourcepath;
    if Execute then begin
      if ExtractFileExt(Filename) = '' then
        case FilterIndex of
          1: Filename:= Filename + '.rtf';
          2: Filename:= Filename + '.html';
        end;
      if not FileExists(Filename) or
         FileExists(Filename) and
         (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                       mtConfirmation, mbYesNoCancel,0) = mrYes)
      then begin
        if LowerCase(ExtractFileExt(Filename)) = '.rtf'
          then Exporter:= TSynExporterRTF.Create(Self)
          else Exporter:= TSynExporterHTML.Create(Self);
        Exporter.Font.assign(Editor.Font);
        ExportToFile(FileName, Exporter);
        FreeAndNil(Exporter);
        if CanFocus then SetFocus;
        FConfiguration.Sourcepath:= ExtractFilePath(Filename);
      end
    end;
  end;
end;

procedure TFEditForm.EditorPaintTransient(Sender: TObject; aCanvas: TCanvas;
  TransientType: TTransientType);
const
  BracketSet = ['{', '[', '(', '}', ']', ')'];
  OpenChars : array[0..2] of Char = ('{', '[', '(');
  CloseChars: array[0..2] of Char = ('}', ']', ')');

var P, P1, P2, Pix: TBufferCoord;
    S: string; I, vStructure: Integer;
    Attri: TSynHighlighterAttributes;

  function CharToPixels(P: TBufferCoord): TBufferCoord;
  begin
    Result:= PointToBuffer(Editor.RowColumnToPixels(DisplayCoord(P.Char, P.Line)));
    Result.Line:= Result.Line-1; // Editor.RowToLine(Result.Line-1);
  end;

begin
  if (Editor = nil) or (Editor.Highlighter = nil) then exit;
  P:= Editor.CaretXY;
  Editor.GetHighlighterAttriAtRowCol(P, S, Attri);

  //If you want to be able to highlight on either side of the bracket, uncomment this block of text
  //Check to see if we need to go back a char;
  if (s = '') or ((length(s) > 0) and not CharInSet(S[1], BracketSet)) then begin
    P.Char:= P.Char - 1;
    if P.Char <= 0 then exit;
    Editor.GetHighlighterAttriAtRowCol(P, S, Attri);
  end;

  if (Editor.CaretX <= length(Editor.LineText) + 1) and
     (Editor.Highlighter.SymbolAttribute = Attri) then
  begin
    for i := 0 to 2 do
      if (S = OpenChars[i]) or (S = CloseChars[i]) then begin
        P2:= P;
        P2.Line:= Editor.LineToRow(P.Line);   // regard wordwrap
        P1:= Editor.GetMatchingBracketEx(P);
        P1.Line:= Editor.LineToRow(P1.Line);
        Pix := CharToPixels(P2);
        Editor.Canvas.Brush.Style := bsClear;
        Editor.Canvas.Font.Assign(Editor.Font);

        if (TransientType = ttAfter) then begin
          Editor.Canvas.Font.Color:= FConfiguration.AttrBrackets.Foreground;; // Editor.Font.Color;
          Editor.Canvas.Font.Style := FConfiguration.AttrBrackets.Style;
          Editor.Canvas.Brush.Color:= FConfiguration.AttrBrackets.Background; // Editor.Color;
        end else begin
          if Attri.Foreground <> clNone
            then Editor.Canvas.Font.Color:= Attri.Foreground
            else Editor.Canvas.Font.Color:= Editor.Font.Color;
          vStructure:= Editor.GetStructureIndex(Editor.CaretY);
          if vStructure = -1 then
            if Attri.Background <> clNone
              then Editor.Canvas.Brush.Color:= Attri.Background
              else Editor.Canvas.Brush.Color:= Editor.Color
          else
            Editor.Canvas.Brush.Color:= Editor.GetStructureLineColor(P.Line, vStructure);
        end;

        Editor.Canvas.TextOut(Pix.Char, Pix.Line + 1, S);
        if (P1.Char > 0) and (P1.Line > 0) then begin
          Pix := CharToPixels(P1);
          if S = OpenChars[i]
            then Editor.Canvas.TextOut(Pix.Char, Pix.Line+1, CloseChars[i])
            else Editor.Canvas.TextOut(Pix.Char, Pix.Line+1, OpenChars[i]);
        end;
      end;
    Editor.Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TFEditForm.SBParagraphClick(Sender: TObject);
  // only works in comments in Lazarus
begin
  var Options:= Editor.Options;
  if eoShowSpecialChars in Options
    then Exclude(Options, eoShowSpecialChars)
    else Include(Options, eoShowSpecialChars);
  Editor.Options:= Options;
  TBParagraph.Down:= (eoShowSpecialChars in Options);
end;

procedure TFEditForm.SBNumbersClick(Sender: TObject);
begin
  Editor.Gutter.ShowLineNumbers:= not Editor.Gutter.ShowLineNumbers;
end;

procedure TFEditForm.SBSystemOutPrintlnClick(Sender: TObject);
begin
  SystemOutPrintln;
end;

procedure TFEditForm.SBMatchBracketClick(Sender: TObject);
begin
  MatchBracket;
end;

procedure TFEditForm.SBBookmarkClick(Sender: TObject);
begin
  with Editor do
    SetDeleteBookmark(CaretX, CaretY);
end;

procedure TFEditForm.SBGotoBookmarkClick(Sender: TObject);
begin
  var OldNumber:= Bookmark;
  repeat
    if Editor.IsBookmark(Bookmark) then begin
      Editor.GotoBookmark(Bookmark);
      Bookmark:= (Bookmark+1) mod 10;
      Exit;
    end;
    Bookmark:= (Bookmark+1) mod 10;
  until OldNumber = Bookmark;
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
  if Editor.WordWrap then begin
    Editor.WordWrap:= false;
    Editor.UseCodeFolding:= true;
  end else begin
    Editor.UseCodeFolding:= false;
    Editor.WordWrap:= true;
  end;
  TBParagraph.Down:= Editor.WordWrap;
end;

procedure TFEditForm.SBZoomInClick(Sender: TObject);
begin
  SetFontSize(+1)
end;

procedure TFEditForm.SBZoomOutClick(Sender: TObject);
begin
  SetFontSize(-1)
end;

procedure TFEditForm.SBBreakpointClick(Sender: TObject);
begin
  if isJava then
    InsertBreakpoint;
end;

procedure TFEditForm.SBBreakpointsClearClick(Sender: TObject);
begin
  if IsJava then
    ClearBreakpoints;
end;

procedure TFEditForm.SBCommentClick(Sender: TObject);
  var i, p, from, _to: Integer; s: string;
      p1, p2: TBufferCoord;
begin
  with Editor do begin
    BeginUpdate;
    if SelAvail then begin
      from:= BlockBegin.Line;
      _to:= BlockEnd.Line;
      p1:= BlockBegin;
      p2:= BlockEnd;
      end
    else begin
      from:= CaretY; p1.Char:= CaretX; p1.Line:= CaretY;
      _to:= CaretY; p2.Char:= 0;
    end;
    for i:= from to _to do begin
      s:= Lines[i-1];
      p:= Pos('//', s);
      if p = 1
        then delete(s, p, 2)
        else s:= '//' + s;
      Lines[i-1]:= s;
    end;
    if p2.Char <> 0 then begin
      BlockBegin:= p1;
      BlockEnd:= p2;
    end;
    EndUpdate;
  end;
  Modified:= true;
  Editor.InvalidateLines(from, _to);
end;

procedure TFEditForm.SBStructureIndentClick(Sender: TObject);
  var Ci, it: IModelIterator;
      cent: TClassifier;
      Method: TOperation;
      Attribute: TAttribute;
      i: integer; aChanged: boolean;

  procedure IndentLine(j: integer);
    var i, l, p: integer; s: string;
  begin
    if (j <= 0) or (j > Editor.Lines.Count) then exit;
    i:= Editor.GetStructureIndent(j);
    if i >= 0 then begin
      p:= 1;
      s:= Editor.Lines[j-1];
      l:= Length(s);
      while (p <= l) and (s[p] <= ' ') do
        inc(p);
      dec(p);
      if p < i then begin
        Editor.Lines[j-1]:= StringOfChar(' ', i-p) + s;
        aChanged:= true;
      end else if p > i then begin
        Editor.Lines[j-1]:= copy(s, p-i+1, l);
        aChanged:= true;
      end;
    end;
  end;

begin
  Screen.Cursor:= crHourglass;
  aChanged:= false;
  with Editor do begin
    BeginUpdate;
    LockBuildStructure:= true;
    Ci:= Model.ModelRoot.GetAllClassifiers;
    while Ci.HasNext do begin
      cent:= TClassifier(Ci.Next);
      for i:= Cent.LineS to Cent.LineSE do
        IndentLine(i);
      IndentLine(Cent.LineE);
      it:= (Cent as TClassifier).GetAttributes;
      while it.HasNext do begin
        Attribute:= it.Next as TAttribute;
        IndentLine(Attribute.LineS);
      end;
      it:= (cent as TClassifier).GetOperations;
      while it.HasNext do begin
        Method:= it.Next as TOperation;
        for i:= Method.LineS to Method.LineE do
          IndentLine(i);
      end;
    end;
    LockBuildStructure:= false;
    EndUpdate;
    Invalidate;
  end;
  Modified:= Modified or aChanged;
  Screen.Cursor:= crDefault;
end;

function TFEditForm.getMarksBreakpoints: string;
begin
  var s:= '';
  with Editor do
    for var i:= 0 to Marks.Count-1 do
      if Marks.Items[i].Visible then
        if Marks.Items[i].IsBookmark then
          s:= s + 'M' + IntToStr(Marks.Items[i].Line)
        else if Marks.Items[i].ImageIndex = BreakPointImageIndex then
          s:= s + 'B' + IntToStr(Marks.Items[i].Line);
  Result:= s;
end;

procedure TFEditForm.SetMarksBreakpoints(MarkBreakpoint: string);
  var line, p:integer;
      IstBookmark: boolean;
begin
  if MarkBreakpoint = '' then exit;
  while MarkBreakpoint <> ')' do begin
    IstBookmark:= MarkBreakpoint[1] = 'M';
    p:= 2;
    while ('0' <= MarkBreakpoint[p]) and (MarkBreakpoint[p] <= '9') do
      inc(p);
    line:= StrToInt(Copy(MarkBreakpoint, 2, p-2));
    Delete(MarkBreakpoint, 1, p-1);
    if IstBookmark
      then SetDeleteBookmark(1, line)
      else InsertBreakpointMark(line);
  end;
end;

function TFEditForm.GetFormType: string;
begin
  Result:= '%E%';
end;

function TFEditForm.GetState: string;
begin
  var s:= inherited GetState;
  s:= s + 'P' + trim(Parameter) + '%P%' +
          'O' + IntToStr(StartOption) +
          'T' + IntToStr(Editor.TopLine) + ')' +
          'X' + IntToStr(Editor.CaretX)  + ')' +
          'Y' + IntToStr(Editor.CaretY)  + ')' +
          //'W' + IntToStr(PLeft.Width)    + ')'+
                 getMarksBreakpoints     + ')';
  if eoShowSpecialChars in Editor.Options then
    s:= s + 'P';
  if Editor.Gutter.ShowLineNumbers then
    s:= s + '#';
  if assigned(Partner) then
    s:= s + 'G';
  Result:= s + ')';
end;

procedure TFEditForm.SetState(var s: string);
  var p: integer;
begin
  Editor.BeginUpdate;
  inherited SetState(s);
  if copy(s, 1, 1) = 'P' then begin
    p:= Pos('%P%', s);
    Parameter:= Copy(s, 2, p-2);
    delete(s, 1, p+2);
  end;
  if copy(s, 1, 1) = 'O' then begin
    StartOption:= StrToInt(Copy(s, 2, 1));
    delete(s, 1, 2);
  end else
    StartOption:= 0;
  if copy(s, 1, 1) = 'T' then begin
    p:= Pos(')', s);
    Editor.TopLine:= StrToInt(Copy(s, 2, p-2));
    Delete(s, 1, p);

    p:= Pos(')', s);
    Editor.CaretX:= StrToInt(Copy(s, 2, p-2));
    Delete(s, 1, p);

    p:= Pos(')', s);
    Editor.CaretY:= StrToInt(Copy(s, 2, p-2));
    Delete(s, 1, p);

    if (length(s) > 0) and (s[1] = 'W') then begin
      p:= Pos(')', s);
      Delete(s, 1, p);
    end;
  end;

  if (copy(s, 1, 1) = 'M') or (copy(s, 1, 1) = 'B') then begin
    p:= Pos(')', s);
    SetMarksBreakpoints(Copy(s, 1, p));
    Delete(s, 1, p);
  end else
    Delete(s, 1, 1);

  if copy(s, 1, 1) = 'P' then begin
    SBParagraphClick(Self);
    Delete(s, 1, 1);
  end;

  if copy(s, 1, 1) = '#' then begin
    Editor.Gutter.ShowLineNumbers:= true;
    Delete(s, 1, 1);
  end;

  if copy(s, 1, 1) = 'G' then // GUI
    SBDesignformClick(Self);

  Editor.EnsureCursorPosVisible;
  Editor.EndUpdate;
end;

function TFEditForm.getWidthAndHeight: TPoint;
  var p, line: integer; s: string;
begin
  Result.X:= 300;
  Result.Y:= 300;
  if isApplet then begin
    line:= getLineNumberWith('cp.setBounds(');
    if line >= 0 then begin
      s:= Editor.Lines[line];
      p:= Pos('cp.setBounds(0, 0, ', s);
      if p > 0 then begin
        delete(s, 1, p + length('cp.setBounds(0, 0, ')-1);
        p:= Pos(',', s);
        TryStrToInt(copy(s, 1, p-1), Result.X);
        delete(s, 1, p);
        p:= Pos(')', s);
        TryStrToInt(copy(s, 1, p-1), Result.Y);
      end;
    end;
  end
  else if FrameType = 8 then begin
    line:= getLineNumberWith('new Scene(root');
    if line >= 0 then begin
      s:= Editor.Lines[line];
      p:= Pos('new Scene(root, ', s);
      delete(s, 1, p + 15);
      p:= Pos(',', s);
      TryStrToInt(trim(copy(s, 1, p-1)), Result.X);
      delete(s, 1, p);
      p:= Pos(')', s);
      TryStrToInt(trim(copy(s, 1, p-1)), Result.Y);
    end
  end else begin
    line:= getLineNumberWith('frameWidth');
    if line >= 0 then begin
      s:= Editor.Lines[line];
      p:= Pos('=', s); delete(s, 1, p);
      p:= Pos(';', s); delete(s, p, 255);
      TryStrToInt(Trim(s), Result.X);
    end;
    line:= getLineNumberWith('frameHeight');
    if line >= 0 then begin
      s:= Editor.Lines[line];
      p:= Pos('=', s); delete(s, 1, p);
      p:= Pos(';', s); delete(s, p, 255);
      TryStrToInt(Trim(s), Result.Y);
    end;
  end;
end;

procedure TFEditForm.ChangeWidthAndHeight(w, h: integer);
begin
  ReplaceLine('int frameWidth', FConfiguration.Indent2 + 'int frameWidth = ' + IntToStr(w) + ';');
  ReplaceLine('int frameHeight', FConfiguration.Indent2 + 'int frameHeight = ' + IntToStr(h) + ';');
end;

procedure TFEditForm.ReplaceLine(const s1, s2: string);
begin
  var line:= getLineNumberWith(s1);
  if (0 <= line) and (line < Editor.Lines.Count) then begin
    Editor.Lines[line]:= s2;
    Modified:= true;
  end;
end;

procedure TFEditForm.ReplaceLineWith(line: integer; const s: string);
begin
  if (0 <= line) and (line < Editor.Lines.Count) then begin
    Editor.Lines[line]:= s;
    Modified:= true;
  end;
end;

procedure TFEditForm.ReplaceLineInLine(line: integer; const old, aNew: string);
begin
  if (0 <= line) and (line < Editor.Lines.Count) then begin
    var s:= Editor.Lines[line];
    var p:= pos(old, s);
    if p = 0
      then s:= ''
      else s:= copy(s, p + length(old), length(s)); // preserve comments
    Editor.Lines[line]:= aNew + s;
    Modified:= true;
  end;
end;

procedure TFEditForm.ReplaceText(const s1, s2: string; all: boolean);
  var line, i: integer;
begin
  if s1 = s2 then exit;
  Editor.BeginUpdate;
  line:= getLineNumberWith(s1);
  if line >= 0 then begin
    Editor.Lines[line]:= ReplaceStr(Editor.Lines[line], s1, s2);
    if all then
      for i:= line + 1 to Editor.Lines.Count - 1 do
        if Pos(s1, Editor.Lines[i]) > 0 then
          Editor.Lines[i]:= ReplaceStr(Editor.Lines[i], s1, s2);
    Modified:= true;
  end;
  Editor.EndUpdate;
end;

procedure TFEditForm.ReplaceTextWithRegex(const reg, s: string; all: boolean;
                                          from: integer = -1; till: integer = -1);
  var line, ends: integer; ws1: string;
      RegEx: TRegEx;
begin
  Editor.BeginUpdate;
  RegEx := CompiledRegEx(reg);
  if from = -1
    then line:= getLNGStartAttributes
    else line:= from;
  if till = -1
    then ends:= getLineNumberWith(_(LNGEndEventMethods))
    else ends:= till;
  while line <= ends do begin
    ws1:= Editor.Lines[line];
    if RegEx.Matches(ws1).Count > 0 then begin
      Editor.Lines[line]:= RegEx.Replace(ws1, reg, s);
      Modified:= true;
    end;
    if not all then
      break;
    inc(line);
  end;
  Editor.EndUpdate;
end;

procedure TFEditForm.ReplaceWord(const s1, s2: string; all: boolean);
  var line: integer; ws1, RegExExpr: string;
      RegEx: TRegEx; Matches: TMatchCollection; Group: TGroup;
begin
  if (s1 = s2) or (s1 = '') then exit;
  Editor.BeginUpdate;
  // '(' + s1 + ')' is Groups[1]
  RegExExpr:= '\b(' + TRegEx.Escape(s1) +
                ')(BG|TG|TB|Title|Polyline|RB\d+|Tab\d+)?';
  if not isWordBreakChar(s1[length(s1)]) then
    RegExExpr:= RegExExpr + '\b';
  RegEx := CompiledRegEx(RegExExpr);
  line:= getLineNumberWithFrom(0, s1);
  while line >= 0 do begin // for every line
    ws1:= Editor.Lines[line];
    Matches:= RegEx.Matches(ws1);
    for var i:= Matches.count - 1 downto 0 do begin
      Group:= Matches.Item[i].Groups[1];
      delete(ws1, Group.Index, Group.Length);
      insert(s2, ws1, Group.Index);
    end;
    Editor.Lines[line]:= ws1;
    line:= getLineNumberWithFrom(line+1, s1);
    Modified:= true;
    if not all then
      break;
  end;
  Editor.EndUpdate;
end;

procedure TFEditForm.ReplaceComponentname(const s1, s2: string; Events: string);
  var line: integer; ws1, RegExExpr: string;
      RegEx: TRegEx; Matches: TMatchCollection; Group: TGroup;
begin
  if s1 = s2 then exit;
  Editor.BeginUpdate;
  ReplaceWord(s1, s2, true);
  while Pos('|', events) = 1 do
    delete(events, 1, 1);
  while (length(events) > 0) and (events[length(events)] = '|') do
    delete(events, length(events), 1);
  events:= events + '|Action';   // dubious todo
  events:= MakeUpperEvents(events);

  // '(' + s1 + ')' is Groups[1]
  RegExExpr:= '\b(' + TRegEx.Escape(s1) + ')_(' + events + ')\b';
  RegEx := CompiledRegEx(RegExExpr);
  line:= getLineNumberWithFrom(0, s1);
  while line >= 0 do begin // for every line
    ws1:= Editor.Lines[line];
    Matches:= RegEx.Matches(ws1);
    for var i:= Matches.count - 1 downto 0 do begin
      Group:= Matches.Item[i].Groups[1];
      delete(ws1, Group.Index, Group.Length);
      insert(s2, ws1, Group.Index);
    end;
    Editor.Lines[line]:= ws1;
    line:= getLineNumberWithFrom(line+1, s1);
    Modified:= true;
  end;
  Editor.EndUpdate;
end;

procedure TFEditForm.ReplaceAttributAt(const At, key, s: string);
begin
  with Editor do begin
    BeginUpdate;
    var line:= getLineNumberWith(key);
    if line >= 0 then
      DeleteLine(line);
    InsertAttributAfter(At, s);
    EndUpdate;
  end;
end;

procedure TFEditForm.ReplaceAttribute(const key, s: string);
begin
  var line:= getLineNumberWith(key);
  if line >= 0 then begin
    Editor.Lines[line]:= s;
    Modified:= true;
  end else
    InsertAttribute(0, s);
end;

procedure TFEditForm.setAttributValue(const Container, key, s: string; after: integer);
  var line, till: integer;
begin
  line:= getLineNumberWith(' ' + key);
  if FrameType = 8
    then till:= getLineNumberWith('primaryStage.show()')
    else till:= getLineNumberWith(_(LNGEndComponents));
  if (line >= 0) and (line < till) then begin
    if trim(s) = ''
      then DeleteLine(line)
      else Editor.Lines[line]:= s;
    Modified:= true;
  end else begin
    line:= getLineNumberWith(Container);
    if line = -1 then
      line:= getLineNumberWith(_(LNGEndComponents))
    else if after = 1 then
      inc(line);
    if line >= 0 then begin
      InsertLinesAt(line, s);
      Modified:= true;
    end;
  end;
end;

procedure TFEditForm.insertAttributValue(const destination, s: string; after: integer);
begin
  var till:= getLNGEndComponents;
  var line:= getLineNumberWithWordFrom(getLNGStartComponents, destination);
  if (line = -1) or (line >= till) then
    line:= till
  else if after = 1 then begin
    inc(line);
    while (line < till) and hasComponent(destination, line) do
      inc(line);
  end;
  if line >= 0 then begin
    InsertLinesAt(line, s);
    Modified:= true;
  end;
end;

function TFEditForm.hasComponent(const key: string; line: integer): boolean;
  var RegEx: string;
begin
  if Pos(FConfiguration.Indent2, key) = 1
    then RegEx := '^' + key + '\b'
    else RegEx := '^[ \t]*' + key + '\b';
  Result:= TRegEx.IsMatch(Editor.Lines[line], RegEx);
end;

function TFEditForm.containsWord(const key: string; line: integer): boolean;
begin
  Result:= TRegEx.IsMatch(Editor.Lines[line], '\b' + key + '\b');
end;

procedure TFEditForm.ChangeAttributValue(const key, s: string);
begin
  var line:= getLineNumberWith(' ' + key);
  if line >= 0 then begin
    Editor.Lines[line]:= s;
    Modified:= true;
  end else
    setAttributValue('', key, s,0);
end;

procedure TFEditForm.ChangeAttributValue(const container, key, s: string);
begin
  var line:= getLineNumberWith(' ' + key);
  if line >= 0 then begin
    Editor.Lines[line]:= s;
    Modified:= true;
  end else
    setAttributValue(container, key, s, 0);
end;

procedure TFEditForm.InsertAttributAfter(const At, Attribut: string);
begin
  var line:= getLineNumberWith(At);
  if line >= 0 then begin
    InsertLinesAt(line + 1, Attribut);
    Modified:= true;
  end;
end;

procedure TFEditForm.InsertLinesAt(line: integer; s: string);
  var cx, cy, tl, cl: integer; collapsed: boolean;
begin
  if not endsWith(s, CrLf) then
    s:= s + CrLf;
  with Editor do begin
    BeginUpdate;
    collapsed:= (AllFoldRanges.Count > 1) and AllFoldRanges.Ranges[1].Collapsed;
    cx:= CaretX;
    cy:= CaretY;
    tl:= TopLine;
    CaretY:= line+1;
    CaretX:= 1;
    PutText(s);
    if cy > line + 1 then begin
      cl:= CountChar(#13, s);
      if cl = 0 then cl:= 1;
      cy:= cy + cl;
      tl:= tl + cl;
    end;
    TopLine:= tl;
    CaretX:= cx;
    CaretY:= cy;
    EnsureCursorPosVisible;
    if collapsed then Collapse(1);
    Modified:= true;
    EndUpdate;
  end;
  Modified:= true;
end;

procedure TFEditForm.InsertLinesAt(const At, s: string);
begin
  with Editor do begin
    var line:= getLineNumberWith(At);
    if line = -1 then begin
      EnsureStartEnd;
      line:= getLineNumberWith(At);
    end;
    if line = -1 then
      ErrorMsg(_(LNGStartGUIVariables) + CrLf +
               _(LNGStartComponents) + CrLf +
               _(LNGStartEventMethods) + CrLf +
               _(LNGNotFound))
    else
      InsertLinesAt(line, s);
  end;
end;

procedure TFEditForm.InsertAttribute(ClassNumber: integer; const s: string);
begin
  InsertLinesAt(GetLNG(2, ClassNumber), s);
end;

procedure TFEditForm.InsertAttribute(const Container, aIndent, Variable: string; fx: boolean);
  var s, aIn: string; line: integer;
begin
  if fx and (Container = 'root') or not fx and (Container = 'cp') then
    InsertAttribute(0, aIndent + Variable)
  else with Editor do begin
    aIn:= aIndent;
    line:= getLineNumberWith(' ' + Container + ' ');
    if line >= 0 then begin
      inc(line);
      s:= Lines[line];
      while Copy(s, 1, length(aIn)) = aIn do begin
        inc(line);
        s:= Lines[line];
      end;
      InsertLinesAt(line, aIndent + Variable);
      Modified:= true;
    end;
  end;
end;

procedure TFEditForm.InsertAttribute(const Container, Variable: string; fx: boolean);
  var s: string; aIn, line, till: integer;
begin
  if fx and (Container = 'root') or not fx and (Container = 'cp') then
    InsertAttribute(0, Variable)
  else with Editor do begin
    till:= getLNGEndAttributes;
    line:= getLineNumberWith(' ' + Container + ' ');
    if line >= 0 then begin
      aIn:= UUtils.getIndent(Editor.Lines[line]);
      inc(line);
      s:= Lines[line];
      while (UUtils.getIndent(s) >= aIn) and (line < till) do begin
        inc(line);
        s:= Lines[line];
      end;
      InsertLinesAt(line, Variable);
      Modified:= true;
    end;
  end;
end;

procedure TFEditForm.InsertProcedure(const aProcedure: string);
begin
  InsertProcedure(0, aProcedure);
end;

procedure TFEditForm.InsertProcedure(ClassNumber: integer; const aProcedure: string);
begin
  InsertLinesAt(GetLNG(6, ClassNumber), aProcedure);
end;

procedure TFEditForm.InsertConstructor(ClassNumber: integer; const aProcedure: string);
begin
  InsertLinesAt(GetLNG(5, ClassNumber), aProcedure);
end;

procedure TFEditForm.InsertComponent(const s: string);
begin
  InsertLinesAt(_(LNGEndComponents), s);
end;

procedure TFEditForm.InsertListener(const Component, Listener: string);
begin
  if hasText(Listener) then exit;
  with Editor do begin
    BeginUpdate;
    var line:= getLineNumberWith(Component);
    if line = -1 then begin
      line:= getLineNumberWith(_(LNGEndComponents));
      if line = -1 then begin
        EnsureStartEnd;
        line:= getLineNumberWith(_(LNGEndComponents));
      end;
    end;
    if line = -1 then
      ErrorMsg(_(LNGEndComponents) + ' ' + _(LNGNotFound))
    else
      InsertLinesAt(line, Listener);
    EndUpdate;
  end;
end;

procedure TFEditForm.InsertImport(const Package: string);
begin
  if StartsWith(Package, 'java.lang') or StartsWith(Package, 'InOut') or
    (getPackage = Package) then exit;
  with Editor do begin
    var line:= getLineNumberWithWord('import');
    if line = -1 then begin
      line:= getLineNumberWithWord('package');
      if line > -1
        then inc(line)
        else line:= -1;
    end else begin
      while (line < Lines.Count) and (Pos('import', Lines[line]) > 0) do begin
        if Pos(Package, Lines[line]) > 0 then exit;
        inc(line);
      end;
      dec(line);
    end;
    InsertLinesAt(line + 1, 'import ' + Package + ';' + CrLf);
    ParseSourceCode(true);
  end;
end;

procedure TFEditForm.DeleteAttribute(const s: string);
  var from, till, line: integer;
begin
  from:= getLNGStartAttributes;
  till:= getLNGEndAttributes;
  line:= getLineNumberWithWordFrom(from, s);
  if (0 <= line) and (line < till) then begin
    DeleteLine(line);
    Modified:= true;
  end;
end;

procedure TFEditForm.DeleteAttributeValues(const s: string);
  var stop, line: integer;
begin
  Editor.BeginUpdate;
  line:= GetLNGStartComponents + 1;
  stop:= GetLNGEndComponents;
  while line < stop do begin
    if Pos(s, Editor.Lines[line]) > 0 then begin
      DeleteLine(line);
      Modified:= true;
      dec(stop);
    end else
      inc(line);
  end;
  Editor.EndUpdate;
end;

function TFEditForm.DeleteAttributeValue(const s: string): boolean;
begin
  Result:= false;
  var aEnd:= GetLNGEndComponents;
  var Search:= getLineNumberWith(s);
  if (0 <= Search) and (Search < aEnd) then begin
    DeleteLine(Search);
    Result:= true;
    Modified:= true;
  end;
end;

procedure TFEditForm.DeleteComponentValue(s: string);
  var from, till, line: integer;
begin
  from:= getLNGStartComponents;
  till:= getLineNumberWithFrom(from, _(LNGEndComponents));
  line:= getLineNumberWithFromTill(from, till, s);
  if line > -1 then begin
    DeleteLine(line);
    Modified:= true;
  end;
end;

procedure TFEditForm.DeleteEmptyLines(line: integer);
begin
  Editor.BeginUpdate;
  while (line < Editor.Lines.Count) and (trim(Editor.Lines[line]) = '') do begin
    deleteLine(line);
    Modified:= true;
  end;
  Editor.EndUpdate;
end;

function TFEditForm.getLNGStartAttributes: integer;
begin
  Result:= getLineNumberWith(_(LNGStartGUIVariables));
end;

function TFEditForm.getLNGEndAttributes: integer;
begin
  Result:= getLineNumberWith(_(LNGEndGUIVariables));
end;

function TFEditForm.getLNGStartComponents: integer;
begin
  Result:= getLineNumberWith(_(LNGStartComponents));
end;

function TFEditForm.getLNGEndComponents: integer;
begin
  Result:= getLineNumberWith(_(LNGEndComponents));
end;

function TFEditForm.getLNGStartEventMethods: integer;
begin
  Result:= getLineNumberWith(_(LNGStartEventMethods));
end;

function TFEditForm.getLNGEndeventMethods: integer;
begin
  Result:= getLineNumberWith(_(LNGEndEventMethods));
end;

function TFEditForm.getLineNumberWith(const s: string): integer;
begin
  Result:= getLineNumberWithFrom(0, s);
end;

function TFEditForm.getLineNumberWithFrom(From: integer; const s: string): integer;
begin
  Result:= -1;
  var i:= From;
  repeat
    if (i < Editor.Lines.Count) and (Pos(s, Editor.Lines[i]) > 0) then begin
      Result:= i;
      break;
    end;
    inc(i);
  until i >= Editor.Lines.Count;
end;

function TFEditForm.getLineNumberWithFromTill(From, till: integer; const s: string): integer;
begin
  Result:= -1;
  var i:= From;
  repeat
    if (i <= till) and (Pos(s, Editor.Lines[i]) > 0) then begin
      Result:= i;
      break;
    end;
    inc(i);
  until i > till;
end;

function TFEditForm.getLineNumberWithWord(const s: string): integer;
begin
  Result:= getLineNumberWithWordFrom(0, s);
end;

function TFEditForm.getLineNumberWithWordFrom(From: integer; const s: string): integer;
  var i, p: integer; ok: boolean; ws: string;
begin
  Result:= -1;
  i:= From;
  repeat
    i:= getLineNumberWithFrom(i, s);
    if i >= 0 then begin
      ws:= Editor.Lines[i];
      p:= Pos(s, ws);
      ok:= true;
      if p > 1 then ok:= IsWordBreakChar(ws[p-1]);
      p:= p + length(s);
      if p <= length(ws) then
        ok:= ok and IsWordBreakChar(ws[p]);
      if ok then begin
        Result:= i; exit;
      end;
    end else
      exit;
    inc(i);
  until i >= Editor.Lines.Count;
end;

function TFEditForm.getLineNumberWithStartsWordFrom(From: integer; const s: string): integer;
  var i, p: integer; ok: boolean; ws: string;
begin
  Result:= -1;
  i:= From;
  repeat
    i:= getLineNumberWithFrom(i, s);
    if i >= 0 then begin
      ws:= Editor.Lines[i];
      p:= Pos(s, ws);
      ok:= true;
      if p > 1 then ok:= IsWordBreakChar(ws[p-1]);
      if ok then begin
        Result:= i;
        exit;
      end;
    end else
      exit;
    inc(i);
  until i >= Editor.Lines.Count;
end;

function TFEditForm.getSource(LineS, LineE: integer): string;
begin
  Result:= '';
  for var i:= LineS to LineE do
    Result:= Result + Editor.Lines[i] + #13#10;
end;

function TFEditForm.getLine(Line: integer): string;
begin
  Result:= Editor.Lines[Line];
end;

procedure TFEditForm.DeleteBlock(StartLine, EndLine: integer);
begin
  Editor.BeginUpdate;
  for var i:= EndLine downto StartLine do
    if i < Editor.Lines.Count then
      DeleteLine(i);
  Modified:= true;
  NeedsParsing:= true;
  Editor.EndUpdate;
end;

procedure TFEditForm.DeleteLine(line: Integer);
begin
  var i:= 0;
  while i < Editor.Marks.Count do begin
    if Editor.Marks[i].line = line then
      DeleteBreakpointMark(Editor.Marks[i]);
    inc(i);
  end;
  var collapsed:= (Editor.AllFoldRanges.Count > 1) and Editor.AllFoldRanges.Ranges[1].Collapsed;
  Editor.CaretY:= Line+1;
  Editor.CommandProcessor(ecDeleteLine, #0, nil);
  if collapsed then Editor.Collapse(1);
end;

procedure TFEditForm.MoveBlock(from, till, dest, desttill: integer; const blanklines: string);
  var i: integer;

  procedure DeleteBlanklines(i: integer);
    var s: string;
  begin
    if i > 0 then begin
      s:= trim(Editor.Lines[i-1]) + trim(Editor.Lines[i]);
      if s = '' then DeleteLine(i);
      s:= trim(Editor.Lines[i-1]) + trim(Editor.Lines[i]);
      if s = '' then DeleteLine(i);
    end else if (i < Editor.Lines.Count-1) then begin
      s:= trim(Editor.Lines[i]) + trim(Editor.Lines[i+1]);
      if s = '' then DeleteLine(i);
    end;
  end;

begin
  Editor.BeginUpdate;
  var s:= '';
  for i:= from to till do
    s:= s + Editor.Lines[i] + #13#10;
  s:= s + blanklines;
  if dest < from then begin
    for i:= from to till do
      DeleteLine(from);
    DeleteBlanklines(from);
    insertLinesAt(dest, s);
  end else begin // dest > from
    if desttill > 0
      then insertLinesAt(desttill + 1, s)
      else insertLinesAt(dest, s);
    for i:= from to till do
      DeleteLine(from);
    DeleteBlanklines(from);
  end;
  Editor.EndUpdate;
end;

function TFEditForm.getBlock(from, lines: integer): string;
begin
  var s:= '';
  for var i:= from to from + lines - 1 do
    s:= s + Editor.Lines[i] + #13#10;
  Result:= s;
end;

procedure TFEditForm.toBackground(Control: TControl);
  var start, from, till: integer;
      Container: string;
begin
  Container:= (Control as TJEComponent).getContainerAdd;
  start:= getLNGStartComponents;
  from:= getLineNumberWithStartsWordFrom(start, Control.Name);
  till:= getLineNumberWithFrom(start, Container);
  if till > -1 then
    if Control is TFXNode
      then MoveBlock(from, till, start + 1, 0, '')
      else MoveBlock(from, till, getLNGEndComponents, 0, '');
end;

procedure TFEditForm.toForeground(Control: TControl);
  var start, from, till: integer;
      Container: string;
begin
  Container:= (Control as TJEComponent).getContainerAdd;
  start:= getLineNumberWith(_(LNGStartComponents));
  from:= getLineNumberWithStartsWordFrom(start, Control.Name);
  till:= getLineNumberWithFrom(start, Container);
  if till > -1 then
    if Control is TFXNode
      then MoveBlock(from, till, getLNGEndComponents, 0, '')
      else MoveBlock(from, till, start + 1, 0, '');
end;

procedure TFEditForm.Go_To(const s: string);
  var p: integer; line: integer;
begin
  with Editor do begin
    line:= getLineNumberWithWord(s);
    if (0 <= line) and (line <= Lines.Count - 1) then
      if Pos('public void ', Lines[line]) > 0 then begin
        CaretY:= line + 1;
        if line + 1 < Lines.Count
          then p:= Pos('}', Lines[line+1])
          else p:= 0;
        if (0 < p) and (p < 5)
          then CaretX:= p
          else CaretX:= 5;
        EnsureCursorPosVisible;
        CommandProcessor(ecDown, #0, nil);
        CommandProcessor(ecDown, #0, nil);
      end else begin
        CaretX:= Pos(s, Lines[line]);
        CaretY:= line + 1;
        EnsureCursorPosVisible;
      end;
  end;
end;

function TFEditForm.getClassAttribut(const aClass, aAttribute: string): string;
begin
  Result:= '';
  with Editor do begin
    var line:= getLineNumberWith(aClass);
    if line >= 0 then begin
      repeat
        if Pos(aAttribute, Lines[line]) > 0 then begin
          Result:= Lines[line];
          break;
        end;
        inc(line)
      until line >= Lines.Count;
    end;
  end;
end;

function TFEditForm.hasText(const s: string): boolean;
begin
  Result:= (Pos(s, Editor.Text) > 0);
end;

function TFEditForm.hasWord(const s: string): boolean;
  var stext: string; line, p: integer;
begin
  Result:= false;
  with Editor do begin
    line:= 0;
    repeat
      line:= getLineNumberWithFrom(line, s);
      if line >= 0 then begin
        stext:= Lines[line];
        p:= Pos(s, stext);
        Result:= true;
        if p > 1 then Result:= IsWordBreakChar(stext[p-1]);
        if p + length(s) <= length(stext) then Result:= Result and IsWordBreakChar(stext[p+length(s)]);
        if Result then exit;
      end;
      inc(line);
    until line = 0;
  end;
end;

procedure TFEditForm.SBDesignformClick(Sender: TObject);
begin
  if IsJava and (Partner = nil) then begin
    var s:= ChangeFileExt(Pathname, '.jfm');
    if FJava.Open(s) then
      FJava.RearrangeFileHistory(s);
  end;
end;

function TFEditForm.getFrameType: Integer;
begin
  if (FFrameType = 0) and ((FileExtension = '.java') or (FileExtension = '.~ava')) then begin
    var JavaScanner:= TJavaScanner.create;
    JavaScanner.Init(Editor.Text);
    FFrameType:= JavaScanner.GetFrameType;
    JavaScanner.Destroy;
  end;
  Result:= FFrameType;
end;

procedure TFEditForm.DeleteTryCatch(const key: string);
begin
  var i:= getLineNumberWith(key);
  if (i > -1) and (Pos('try {', Editor.Lines[i-1]) > 0) then
    DeleteBlock(i-1, i+3);
end;

procedure TFEditForm.DesignButtonClick(Sender: TObject);
begin
  var s:= ChangeFileExt(Pathname, '.jfm');
  if Partner = nil then begin
    if FJava.Open(s) then
      FJava.RearrangeFileHistory(s);
  end else
    Partner.Close;
end;

procedure TFEditForm.DeleteComponent(const Component: string);
  var line, stop, i: integer;
begin
  Editor.BeginUpdate;
  line:= getLineNumberWith(getLNG(3, 0));
  if line >= 0 then begin
    stop:= getLineNumberWith(getLNG(4, 0));
    if stop = -1 then stop:= Editor.Lines.Count - 1;
    for i:= stop downto line + 1 do
      if Pos(Component, Editor.Lines[i]) > 0
        then DeleteLine(i);
  end;
  Modified:= true;
  Editor.EndUpdate;
end;

procedure TFEditForm.DeleteComponentDefault(Control: TControl);
  var line, stop, i: integer; Typ: string;
begin
  Editor.BeginUpdate;
  Typ:= (Control as TJEComponent).JavaType;
  DeleteAttribute('private ' + Typ + ' ' + Control.Name);

  stop:= getLNGEndComponents;
  if stop = -1 then
    stop:= Editor.Lines.Count - 1;
  line:= getLNGStartComponents;
  if line >= 0 then
    for i:= stop downto line + 1 do
      if containsWord(Control.Name, i) then
        DeleteLine(i);
  Modified:= true;
  Editor.EndUpdate;
end;

procedure TFEditForm.DeleteComponentTotal(ClassNumber: integer; const Component, Typ: string);
  var line, stop, i: integer; s: string;
begin
  Editor.BeginUpdate;
  line:= getLineNumberWith(GetLNG(1, ClassNumber));
  if line >= 0 then begin
    stop:= getLineNumberWith(GetLNG(2, ClassNumber));
    if stop = -1 then stop:= Editor.Lines.Count - 1;
    for i:= stop downto line + 1 do begin
      s:= Editor.Lines[i];
      if ((Pos(' ' + Component + ' ', s) + Pos(' ' + Component + ';', s) > 0)
          and (Pos(Typ, s) > 0))
      then DeleteLine(i);
    end;
  end;
  line:= getLineNumberWith(GetLNG(3, ClassNumber));
  if line >= 0 then begin
    stop:= getLineNumberWith(GetLNG(4, ClassNumber));
    if stop = -1 then stop:= Editor.Lines.Count - 1;
    for i:= stop downto line + 1 do begin
      s:= Editor.Lines[i];
      if (Pos(Component + '.', s) > 0) or (Pos(Component + ')', s) > 0)
      then
        DeleteLine(i);
    end;
  end;
  Modified:= true;
  Editor.EndUpdate;
end;

{$WARNINGS OFF}

function TFEditForm.hasEventProcedureInModel(const aMethodname: string): boolean;
  var Ci, it: IModelIterator;
      cent: TModelEntity;
      Method: TOperation;
begin
  Result:= false;
  // ParseSourcecode; caller has to do
  Ci:= Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext and not Result do begin
    cent:= Ci.Next;
    if (cent is TClass) then begin
      It:= (cent as TClass).GetOperations;
      while It.HasNext and not Result do begin
        Method:= It.Next as TOperation;
        if Method.Name = aMethodname then
          Result:= true;
      end;
    end;
  end;
end;

procedure TFEditForm.SetNewActionEventFormat;
  var i, p, aEnd: Integer;
      s: string;
begin
  with Editor do begin
    BeginUpdate;
    i:= 1;
    aEnd:= getLNGEndComponents;
    if aEnd > -1 then begin
      repeat
        s:= Lines[i];
        p:= Pos('ActionPerformed(evt);', s);
        if (p > 1) and (s[p-1] <> '_') then begin
          insert('_', s, p);
          Lines[i]:= s;
        end;
        inc(i);
      until i = aEnd;
      repeat
        s:= Lines[i];
        p:= Pos('ActionPerformed(ActionEvent evt)', s);
        if (p > 1) and (s[p-1] <> '_') then begin
          insert('_', s, p);
          Lines[i]:= s;
        end;
        inc(i);
      until i = Lines.Count -1;
      EndUpdate;
    end;
  end;
end;

{$WARNINGS ON}

procedure TFEditForm.DeleteMethod(const Methode: string; SourcecodeCheck: boolean = true);
  var from, _to: integer;
      found: boolean;
      Ci, it: IModelIterator;
      cent: TModelEntity;
      Method: TOperation;
begin
  from:= 0;
  _to:= 0;
  Method:= nil;
  ParseSourcecode(true);
  Found:= false;
  Ci:= Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext and not found do begin
    cent:= Ci.Next;
    if cent is TClass then begin
      It:= (cent as TClass).GetOperations;
      while It.HasNext and not Found do begin
        Method:= It.Next as TOperation;
        if Method.Name = Methode then begin
          from:= Method.LineS - 1;
          _to:= Method.LineE - 1;
          Found:= true;
        end;
      end;
    end;
  end;

  if Found and (SourcecodeCheck and not Method.hasSourceCode or not SourcecodeCheck) then begin
    DeleteBlock(from, _to);
    while (from < Editor.Lines.Count) and (trim(Editor.Lines[from]) = '') and (Editor.CaretY < Editor.Lines.Count) do
      DeleteLine(from);
    NeedsParsing:= true;
  end;
end;

procedure TFEditForm.DeleteEventMethod(Method: string);
  var from, till, line: integer; SL: TStringList;

  function WithoutEndComment(s: string): string;
  begin
    var SL:= TStringList.Create;
    SL.Text:= s;
    var cpos:= SL.Count - 2;
    var p:= Pos('// end of', SL[cpos]);
    if p > 0 then
      SL[cpos]:= copy(SL[cpos], 1, p-1);
    s:= SL.Text;
    FreeAndNil(SL);
    Result:= s;
  end;

begin
  Method:= WithoutEndComment(Method);
  SL:= TStringList.Create;
  SL.Text:= Method;
  from:= getLineNumberWith(_(LNGStartEventMethods));
  till:= getLineNumberWithFrom(from, _(LNGEndEventMethods));
  line:= getLineNumberWithFromTill(from, till, SL[0]);
  while (line >= 0) and (line < till) do begin
    if Method = WithoutEndComment(getBlock(line, SL.Count)) then begin
      deleteBlock(line, line + SL.Count - 1);
      if trim(Editor.Lines[line]) = '' then
        DeleteLine(line);
      line:= -1;
    end else
     line:= getLineNumberWithFromTill(line + 1, till, SL[0]);
  end;
  FreeAndNil(SL);
end;

procedure TFEditForm.DeleteListener(Listener: string);
  var from, till, line: integer; SL: TStringList; block: string;
begin
  SL:= TStringList.Create;
  SL.Text:= Listener;
  from:= getLNGStartComponents;
  till:= getLineNumberWithFrom(from, _(LNGEndComponents));
  line:= getLineNumberWithFromTill(from, till, SL[0]);
  while (line >= 0) and (line < till) do begin
    block:= getBlock(line, SL.Count);
    // due to caretPositonChanged <-> inputMethodTextChanged problem
    if (Listener = block) or (Listener + #13#10 = block) then begin
      deleteBlock(line, line + SL.Count - 1);
      if trim(Editor.Lines[line]) = '' then
        DeleteLine(line);
      line:= -1;
    end else
     line:= getLineNumberWithFromTill(line + 1, till, SL[0]);
  end;
  FreeAndNil(SL);
end;

procedure TFEditForm.DeleteFXListener(Listener: string);
  var s1, s2: string; p, line: integer;
begin
  p:= Pos(#13#10, Listener);
  s1:= trim(copy(Listener, 1, p-1));
  delete(Listener, 1, p+1);
  p:= Pos(#13#10, Listener);
  s2:= trim(copy(Listener, 1, p-1));

  line:= getLineNumberWith(s2);
  if line > -1 then begin
    dec(line, 1);
    deleteBlock(line, line + 2);
    if trim(Editor.Lines[line]) = '' then
      DeleteLine(line);
    Modified:= true;
  end;
end;

procedure TFEditForm.DeleteLambdaListener(Listener: string);
begin
  var p:= Pos(#13#10, Listener);
  if p > 0 then
    Listener:= trim(copy(Listener, 1, p-1));
  DeleteComponentValue(Listener);
end;

type
  TMethodSource = record
     Name: string;
     from: integer;
     till: integer;
  end;

procedure TFEditForm.DeleteOldAddNewMethods(OldMethods, NewMethods: TStringList);
  var i, p, from, till, Index: integer;
      Ci, it: IModelIterator;
      cent: TModelEntity;
      Method: TOperation;
      MethodArray: array of TMethodSource;
      SL: TStringList;
      s1, s2, func: string;

  function inMethodArray(MethodName: string): integer;
  begin
    Result:= -1;
    for var i:= 0 to Index do
      if MethodArray[i].Name = MethodName then begin
        Result:= i;
        break;
      end;
  end;

  function getActionMethod(Name: string): string;
  begin
    if EndsWith(Name, '_ActionPerformed') then
      Delete(Name, Pos('_ActionPerformed', Name), 16);
    Result:= FConfiguration.Indent1 + 'public void ' + Name + '_ActionPerformed(ActionEvent evt) {' + CrLf +
             FConfiguration.Indent2 + _(LNGTODO) + CrLf + CrLf +
             FConfiguration.Indent1  + '}';
    if FConfiguration.CommentClosingBrackets then
      Result:= Result + ' // end of ' + Name;
  end;

begin
  // 1. add new methods at the end
  // 2. delete old methods in the middle
  ParseSourceCode(true);
  SL:= TStringList.Create;
  SetLength(MethodArray, 20);

  // get existing methods
  Index:= -1;
  Ci:= Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext do begin
    cent:= Ci.Next;
    if cent is TClass then begin
      It:= (cent as TClass).GetOperations;
      while It.HasNext do begin
        inc(Index);
        if Length(MethodArray) = Index then
          SetLength(MethodArray, Length(MethodArray) + 20);
        Method:= It.Next as TOperation;
        MethodArray[Index].Name:= Method.Name;
        MethodArray[Index].from:= Method.LineS - 1;
        MethodArray[Index].till:= Method.LineE - 1;
      end;
    end;
  end;

  // add missing new methods
  for i:= 0 to NewMethods.Count - 1 do
    if inMethodArray(NewMethods[i] + '_ActionPerformed') = -1 then begin
      func:= CrLf + getActionMethod(NewMethods[i]);
      SL.Add(func);
    end;
  InsertProcedure(SL.Text);

  // delete unnecessary default methods
  // determine methods to delete
  SL.Clear;
  for i:= 0 to OldMethods.Count - 1 do   // keeps method lines valid
    if NewMethods.IndexOf(OldMethods[i]) = -1 then begin
      p:= inMethodArray(OldMethods[i] + '_ActionPerformed');
      if p > -1 then begin
        // delete if default
        from:= MethodArray[p].from;
        till:= MethodArray[p].till;
        s1:= getSource(from, till);
        s2:= getActionMethod(MethodArray[p].Name) + #13#10;
        if s1 = s2 then
          SL.Add(OldMethods[i] + '_ActionPerformed');
      end;
    end;

  // delete beginning at the end of the sourcecode
  for i:= Index downto 0 do
    if SL.indexOf(MethodArray[i].Name) > -1 then begin
      from:= MethodArray[i].from;
      till:= MethodArray[i].till;
      DeleteBlock(from, till);
      while (from < Editor.Lines.Count - 1) and (trim(Editor.Lines[from]) = '') do
        DeleteLine(from);
      InsertLinesAt(from, '');
    end;
  FreeAndNil(SL);
  setLength(MethodArray, 0);
end;

procedure TFEditForm.DeleteFXOldAddNewMethods(OldMethods, NewMethods: TStringList);
  var i, p, from, till, Index: integer;
      Ci, it: IModelIterator;
      cent: TModelEntity;
      Method: TOperation;
      MethodArray: array of TMethodSource;
      SL: TStringList;
      s1, s2, func: string;

  function inMethodArray(MethodName: string): integer;
    var i: integer;
  begin
    Result:= -1;
    for i:= 0 to Index do
      if MethodArray[i].Name = MethodName then begin
        Result:= i;
        break;
      end;
  end;

  function getActionMethod(Name: string): string;
  begin
    Result:= FConfiguration.Indent1 + 'public void ' + Name + '(Event evt) {' + CrLf +
             FConfiguration.Indent2 + _(LNGTODO) + CrLf + CrLf +
             FConfiguration.Indent1  + '}';
    if FConfiguration.CommentClosingBrackets then
      Result:= Result + ' // end of ' + Name;
  end;

begin
  // 1. add new methods at the end
  // 2. delete old methods in the middle
  ParseSourceCode(true);
  SL:= TStringList.Create;
  SetLength(MethodArray, 20);

  // get existing methods
  Index:= -1;
  Ci:= Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext do begin
    cent:= Ci.Next;
    if cent is TClass then begin
      It:= (cent as TClass).GetOperations;
      while It.HasNext do begin
        inc(Index);
        if Length(MethodArray) = Index then
          SetLength(MethodArray, Length(MethodArray) + 20);
        Method:= It.Next as TOperation;
        MethodArray[Index].Name:= Method.Name;
        MethodArray[Index].from:= Method.LineS - 1;
        MethodArray[Index].till:= Method.LineE - 1;
      end;
    end;
  end;

  //add missing new methods
  for i:= 0 to NewMethods.Count - 1 do
    if inMethodArray(NewMethods[i] + '_Action') = -1 then begin
      func:= CrLf + getActionMethod(NewMethods[i] + '_Action');
      SL.Add(func);
    end;
  InsertProcedure(SL.Text);

  // delete unnecessary default methods
  // determine methods to delete
  SL.Clear;
  for i:= 0 to OldMethods.Count - 1 do   // keeps method lines valid
    if NewMethods.IndexOf(OldMethods[i]) = -1 then begin
      p:= inMethodArray(OldMethods[i] + '_Action');
      if p > -1 then begin
        // delete if default
        from:= MethodArray[p].from;
        till:= MethodArray[p].till;
        s1:= getSource(from, till);
        s2:= getActionMethod(MethodArray[p].Name) + #13#10;
        if s1 = s2 then
          SL.Add(OldMethods[i] + '_Action');
      end;
    end;

  // delete beginning at the end of the sourcecode
  for i:= Index downto 0 do
    if SL.indexOf(MethodArray[i].Name) > -1 then begin
      from:= MethodArray[i].from;
      till:= MethodArray[i].till;
      DeleteBlock(from, till);
      while (from < Editor.Lines.Count - 1) and (trim(Editor.Lines[from]) = '') do
        DeleteLine(from);
      InsertLinesAt(from, '');
    end;
  FreeAndNil(SL);
  setLength(MethodArray, 0);
end;

procedure TFEditForm.SBClassEditClick(Sender: TObject);
begin
  if IsJava then begin
    if Modified then
      FJava.DoSave(Self, false);
    if not myJavaCommands.HasValidClass(Pathname) then
      myJavaCommands.CompileForm(Self);
    FJava.PrepareClassEdit(Pathname, 'Edit', nil);
  end;
end;

function TFEditForm.hasStartAndEnd(CorI: integer): boolean;
begin
  Result:= true;
  var i:= 0;
  while Result and (i < CorI) do begin
    Result:= Result and
             hasText(GetLNG(1, i)) and hasText(GetLNG(2, i)) and
             hasText(GetLNG(5, i)) and hasText(GetLNG(6, i));
    inc(i);
  end;
  if Result and (FrameType > 1) then
    Result:= Result and hasText(_(LNGStartComponents)) and hasText(_(LNGEndComponents));
end;

procedure TFEditForm.WMNCButtonDBLClick(var msg: TMessage);
begin
  FJava.MIMaximizedClick(Self);
end;

procedure TFEditForm.SBClassOpenClick(Sender: TObject);
  var UMLForm: TFUMLForm;
      s, fname, aClassname: string;
begin
  if IsJava or IsPascal then begin
    if Modified then FJava.DoSave(Self, false);
    LockWindow(FJava.Handle);
    var SL:= TStringList.Create;
    try
      s:= ChangeFileExt(Pathname, '.uml');
      fname:= copy(s, 1, length(s)-4);
      aClassname:= ChangeFileExt(ExtractFilename(Pathname), '');
      if FileExists(s) then begin
        FJava.OpenUMLWindow(s, '');
        SL.LoadFromFile(s);
        if Pos('[Box:  - ' + aClassname, SL.Text) = 0 then
          FJava.DoOpenInUMLWindow(Pathname);
      end else begin
        UMLForm:= FJava.MakeNewUMLWindow(s, '');
        FConfiguration.ShowAlways:= false;
        UMLForm.MainModul.AddToProject(Pathname);
        UMLForm.CreateTVFileStructure;
        UMLForm.MainModul.DoLayout;
        FConfiguration.ShowAlways:= true;
        FJava.DoSave(UMLForm, false);
      end;
      FJava.RearrangeFileHistory(s);
    finally
      UnlockWindow;
      SL.Destroy;
    end;
  end;
end;

procedure TFEditForm.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFEditForm.SBBrowserClick(Sender: TObject);
begin
  if Modified then Save(false);
  FJava.CallApplet(Pathname);
end;

procedure TFEditForm.SBValidateClick(Sender: TObject);
begin
  var Browser:= FJava.NewBrowser('', '');
  if isCSS
    then Browser.UploadFilesHttpPost('https://jigsaw.w3.org/css-validator/validator', [], [], ['file'], [Pathname])
    else Browser.UploadFilesHttpPost('https://validator.w3.org/check', [], [], ['uploaded_file'], [Pathname]);
end;

procedure TFEditForm.SBExplorerClick(Sender: TObject);
begin
  var s:= ExtractFilePath(Pathname);
  FJava.NewExplorer(s, '');
end;

procedure TFEditForm.MIRenewImportsClick(Sender: TObject);
begin
  FConfiguration.FixImports:= true;
  NeedsParsing:= true;
  ParseSourcecode(true);
end;

procedure TFEditForm.MIFontClick(Sender: TObject);
begin
  FJava.MIFontClick(Self);
end;

procedure TFEditForm.MIGitAddClick(Sender: TObject);
begin
  if Modified then Save(false);
  FGit.GitCall('add ' + ExtractFileName(Pathname), ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitCheckoutClick(Sender: TObject);
begin
  FGit.GitCall('checkout -- ' + ExtractFilename(Pathname), ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitGuiClick(Sender: TObject);
begin
  FGit.ShowGui(ExtractFilePath(Pathname));
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
  FGit.GitCall('fetch ' + FConfiguration.GitRemoteRepository, ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitLogClick(Sender: TObject);
begin
  FGit.GitCall('log --stat', ExtractFilePath(Pathname));
end;

procedure TFEditForm.MIGitPushClick(Sender: TObject);
begin
  FGit.GitCall('push origin master', ExtractFilePath(Pathname));
end;

procedure TFEditForm.MGitResetClick(Sender: TObject);
begin
  FGit.GitCall('reset HEAD ' + ExtractFileName(Pathname), ExtractFilePath(Pathname));
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

function TFEditForm.EncodingAsString(const aEncoding: string): string;
begin
  Result:= Uppercase(aEncoding);
  if Pos('ANSI', Result) > 0 then Result:= 'ANSI' else
  if Pos('ASCII', Result) > 0 then Result:= 'ASCII' else
  if Pos('UTF-8', Result) > 0 then Result:= 'UTF-8' else
  if Pos('UTF-16', Result) > 0 then Result:= 'UTF-16' else
  if Pos('UNICODE', Result) > 0 then Result:= 'UTF-16' else
  if Pos('CP1252', Result) > 0 then Result:= 'ANSI';
end;

function TFEditForm.LinebreakAsString: string;
begin
  if LineBreak = #13#10 then Result:= 'Windows' else
  if LineBreak = #10 then Result:= 'Unix'
  else Result:= 'Mac';
end;

function TFEditForm.LinebreakAsCtrls(const s: string): string;
begin
  if s = 'Windows' then Result:= #13#10 else
  if s = 'Unix' then Result:= #10
  else Result:= #13;
end;

procedure TFEditForm.SetEncoding(aEncoding: string);
begin
  Self.Encoding:= EncodingAsString(aEncoding);
  var p:= Pos('/', aEncoding);
  delete(aEncoding, 1, p);
  Linebreak:= LinebreakAsCtrls(aEncoding);
  Editor.Lines.DefaultEncoding:= getEncodingAsType;
end;

function TFEditForm.getEncodingAsType: TEncoding;
begin
  if Encoding = 'ANSI' then Result:= TEncoding.ANSI else
  if Encoding = 'UTF-8' then Result:= TEncoding.UTF8 else
  Result:= TEncoding.Unicode;
end;

procedure TFEditForm.CheckAge;
  var FDT: TDateTime;
begin
  if not FConfiguration.CheckAge then exit;
  FileAge(Pathname, FDT);
  if Visible and (EditorAge <> 0) and CheckAgeEnabled and FileExists(Pathname) and (EditorAge <> FDT) then begin
    if MessageDlg(Format(_('File %s externally modified. Open new?'),
                    [Pathname]), mtConfirmation,[mbYes, mbNo], 0) = mrYes then
     with Editor do
       try
          Lines.LoadFromFile(Pathname);
          Editor.ReplaceTabs(FConfiguration.TabWidth);
        except on e: Exception do
          FConfiguration.Log('TFEditForm.CheckAge: ' + Pathname, e);
        end;
    FileAge(Pathname, EditorAge);
  end;
end;

function TFEditForm.IsJava: boolean;
begin
  Result:= (FileExtension = '.java');
end;

function TFEditForm.IsPascal: boolean;
begin
  Result:= (FileExtension = '.pas');
end;

function TFEditForm.IsHTML: boolean;
begin
  Result:= (FileExtension = '.html') or (FileExtension = '.htm');
end;

function TFEditForm.IsCSS: boolean;
begin
  Result:= (FileExtension = '.css');
end;

procedure TFEditForm.Search;
begin
  FJava.ShowSearchReplaceDialog(Editor, false);
end;

procedure TFEditForm.SearchAgain;
begin
  FJava.DoSearchReplaceText(Editor, false);
end;

procedure TFEditForm.Replace;
begin
  FJava.ShowSearchReplaceDialog(Editor, true);
end;

procedure TFEditForm.Show;
begin
  Visible:= true;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
end;

procedure TFEditForm.Hide;
begin
  Visible:= false;
  FJava.DeleteTabAndWindow(Number);
end;

procedure TFEditForm.ReplaceWidthHeight(W, H: integer);

  function aReplace(const LineText, wh: string; i: integer): string;
    var s: string; p: integer;
  begin
    s:= LineText;
    p:= Pos(wh, s) + Pos(UpperCase(wh), s) + length(wh);
    while s[p] <> '"' do inc(p);
    inc(p);
    while s[p] <> '"' do delete(s, p, 1);
    insert(IntToStr(i), s, p);
    Result:= s;
  end;

begin
  var line:= getLineNumberWith(' width=');
  if line >= 0 then begin
    Editor.Lines[line]:= aReplace(Editor.Lines[line], 'width', W);
    line:= getLineNumberwithFrom(line+1, ' width=');
    if line >= 0 then // PlugIn
      Editor.Lines[line]:= aReplace(Editor.Lines[line], 'width', W);
    Modified:= true;
  end;
  line:= getLineNumberWith(' height=');
  if line >= 0 then begin
    Editor.Lines[line]:= aReplace(Editor.Lines[line], 'height', H);
    line:= getLineNumberwithFrom(line+1, ' height=');
    if line > 0 then
      Editor.Lines[line]:= aReplace(Editor.Lines[line], 'height', H);
    Modified:= true;
  end;
end;

procedure TFEditForm.AutomatedCompleteImports;
  var aClass, package, Shorttype, classimp, fullimp, aLine: string;
      Ci: IModelIterator; cent: TClassifier;
      FUnit: TUnitPackage; ClassImports, FullImports, AllImports: TStringList;
      i, p: integer;
begin
  FUnit := (Model.ModelRoot as TLogicPackage).FindUnitPackage('Default');
  if not assigned(FUnit) then exit;

  with Editor do begin
    BeginUpdate;
    FullImports:= FUnit.FullImports;   // import java.awt.*;
    if FrameType in [2..4] then begin
      FullImports.Add('java.awt.');
      FullImports.Add('java.awt.event.');
    end else if FrameType in [5..7] then begin
      FullImports.Add('java.awt.');
      FullImports.Add('java.awt.event.');
      FullImports.Add('javax.swing.');
      FullImports.Add('javax.swing.event.');
    end;
    ClassImports:= FUnit.ClassImports; // import java.io.FileInputStream;
    if FrameType = 8 then begin
      ClassImports.Add('Application=javafx.application');
      ClassImports.Add('Scene=javafx.scene');
      ClassImports.Add('Pane=javafx.scene.layout');
      ClassImports.Add('Stage=javafx.stage');
    end;
    Ci:= Model.UnknownPackage.GetClassifiers;
    while Ci.hasNext do begin
      cent:= TClassifier(Ci.Next);
      aclass:= WithoutGeneric(cent.Name);
      Shorttype:= GetShortType(aClass);
      package:= ExtractPackageName(aclass);
      if (Pos('java.lang.', aclass) = 0) and (Pos('.', aclass) > 0) and
         (FullImports.IndexOf(package + '.') = -1) and
         (ClassImports.IndexOfName(Shorttype) = -1) then
        ClassImports.Add(Shorttype + '=' + package)
    end;
    AllImports:= TStringList.Create;
    AllImports.Sorted:= true;
    AllImports.Duplicates:= dupIgnore;
    for i:= 0 to FullImports.Count - 1 do
      if FullImports.Strings[i] <> 'java.lang.' then
        AllImports.Add('import ' + FullImports.Strings[i] + '*;');
    for i:= 0 to ClassImports.Count - 1 do
      AllImports.add('import ' + ClassImports.ValueFromIndex[I] + '.' + ClassImports.Names[i] + ';');

    for i:= 0 to FConfiguration.ImportCache.Count - 1 do begin
      classimp:= FConfiguration.ImportCache.Strings[i];
      delete(classimp, 1, Pos('=', classimp));
      if (Pos('.', classimp) > 0) and (Pos('<', classimp) = 0) and
         (Pos('[', classimp) = 0) and (Pos('java.lang.', classimp) = 0)then begin
        fullimp:= 'import ' + copy(classimp, 1, Lastdelimiter('.', classimp)) + '*;';
        if AllImports.IndexOf(fullimp) = -1 then
          AllImports.add('import ' + classimp + ';');
      end;
    end;
    // handle static imports
    for i:= FUnit.ImportStartline-1 to FUnit.ImportEndline-1 do begin
      aLine:= Editor.Lines[i];
      p:= Pos(' static ', aLine);
      if p > 0 then begin
        delete(aLine, 1, p + length(' static'));
        aLine:= 'import ' + trim(aLine);
        p:= AllImports.IndexOf(aLine);
        if p > -1 then begin
          aLine:= AllImports.Strings[p];
          insert(' static', aLine, length('import') + 1);
          AllImports.delete(p);
          AllImports.add(aLine);
        end;
      end;
    end;
    if FUnit.ImportEndline >= FUnit.ImportStartline then
      DeleteBlock(FUnit.ImportStartline-1, FUnit.ImportEndline-1);
    if AllImports.Text <> '' then
      InsertLinesAt(FUnit.ImportStartline - 1, AllImports.Text);
    AllImports.Destroy;
    EndUpdate;
  end;
end;

function TFEditForm.getPackage: string;
begin
  var JavaScanner:= TJavaScanner.Create;
  try
    if assigned(Editor) and (Editor.Text <> '')
       then Result:= JavaScanner.getPackage(Editor.Text)   // >>> TFEditForm.getPackage exe=0
       else Result:= '';
    JavaScanner.Destroy;
  except on e: Exception do
    FConfiguration.Log('TFEditForm.getPackage', e);
  end;
end;

function TFEditForm.getAllPathnames: TStringList;
begin
  Result:= TStringList.Create;
  if Editor.Text <> ''
    then Result.Add(Pathname);
end;

function TFEditForm.getAllClassnames: TStringList;
begin
  Result:= TStringList.Create;
  if IsJava then begin
    ParseSourcecode(false);
    var Ci:= Model.ModelRoot.GetAllClassifiers;
    while Ci.HasNext do begin
      var cent:= TClassifier(Ci.Next);
      if (cent is TClass) and (cent.Pathname = Pathname) then
        Result.Add(ExtractFilePath(Pathname) + withoutGeneric(cent.name) + '.class');
    end;
  end;
end;

procedure TFEditForm.DoOnMouseOverToken(Sender: TObject; const Token: string;
            TokenType: Integer; Caret, P: TPoint; Attri: TSynHighlighterAttributes; var Highlight: Boolean);
begin
  if FConfiguration.TooltipAutomatic and isJava then begin
    if Tokentype = Ord(SynHighlighterJava.tkIdentifier) then begin
      if (Token <> LastToken) and (Trim(Token) <> '') then
        CreateTooltip(Caret, P, Token);
    end else
      if assigned(FTooltip) and FTooltip.Visible and not FTooltip.closemanually then
        FTooltip.Hide;
  end;
  LastToken:= Trim(Token);
end;

procedure TFEditForm.DoOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FJava.scpJava.Form.Visible then
    FJava.scpJava.CancelCompletion;
end;

procedure TFEditForm.CreateTooltip(Caret, P: TPoint; const Token: string);
  var SL: TStringList; Typ, Code: string;
      freq, startTime, endTime: Int64;
      duration: integer; TokenRect: TRect;
      w, h: integer;
begin
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(startTime);
  w:= Editor.Canvas.TextWidth(Token);
  h:= editor.Canvas.TextHeight(Token);
  TokenRect:= Rect(P.x, P.y-h, P.x+w, P.y);

  Code:= getJavaCodeAt(Caret);
  Typ:= MyCodeCompletion.SearchDocumentation(Code, Caret.Y);
  if Typ <> '' then begin

    // new Tooltip due to browser.history (forward/backward)
    if assigned(FTooltip) then begin
      FTooltip.Hide;
      FreeAndNil(FTooltip);
    end;
    FTooltip:= TFTooltip.Create(Self, P, TokenRect, Token);
    SL:= TStringList.Create;
    try
      SL.Add(FTooltip.getHead);
      SL.Add(Typ);
      SL.Add('</body></html>');
      try
        SL.SaveToFile(FConfiguration.TempDir + 'Tooltip.html');
      except
      end;
    finally
      FreeAndNil(SL);
    end;
    QueryPerformanceCounter(endTime);
    duration:= (endTime - startTime) * 1000 div freq; // ms
    duration:= FConfiguration.TooltipDelay - Duration;
    if (Duration < 10) or (Token = '# VK_F2 #') then Duration:= 10;
    FTooltip.OpenTooltipTimer.Interval:= Duration;
    FTooltip.OpenTooltipTimer.Enabled:= true;
  end;
end;

procedure TFEditForm.MISearchDeclarationClick(Sender: TObject);
  var Code: string;
    Classifier: TClassifier;
    Attribute: TAttribute;
    Operation: TOperation;
    aParameter: TParameter;
begin
  if MousePosition.Char > -1 then begin
    Code:= getJavaCodeAt(Point(MousePosition.Char, MousePosition.Line));
    if Code = '' then exit;

    myCodeCompletion.getTypeOfCode(Code, MousePosition.Line, 0, false);
    Classifier:= myCodeCompletion.CCClassifier;
    Attribute:= myCodeCompletion.CCAttribute;
    Operation:= myCodeCompletion.CCOperation;
    aParameter:= myCodeCompletion.CCParameter;

    if assigned(Classifier) then begin
      if assigned(Attribute) then
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname), Attribute.Spalte, Attribute.LineS, false)
      else if assigned(Operation) then
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname), Operation.Spalte, Operation.LineS, false)
      else if assigned(aParameter) then
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname), aParameter.Spalte, aParameter.LineS, false)
      else
        FJava.ChangeWindowWithPositioning(ToWindows(Classifier.Pathname), Classifier.Spalte, Classifier.LineS, false);
     end;
  end;
end;

procedure TFEditForm.setNeedsParsing(value: boolean);
begin
  if value <> FNeedsParsing then begin
    if value and assigned(Editor) and not Editor.LockBuildStructure
      then FNeedsParsing:= true
      else FNeedsParsing:= false;
  end;
end;

procedure TFEditForm.setFileExtension(value: string);
begin
  if value <> fFileExtension then
    fFileExtension:= value;
end;

function TFEditForm.getJavaCodeAt(Caret: TPoint): string;
  var s: string; p, brackets, line: Integer;
begin
  line:= Caret.Y - 1;
  s:= Editor.Lines[line];
  p:= max(Caret.X, 1);
  // expand to right
  while (p <= Length(s)) and (IsCharAlphaNumeric(s[p]) or (s[p] = '_')) do
    inc(p);
  // detect method call
  if (p <= length(s)) and (s[p] = '(') then begin
    // get Parameters
    brackets:= 1;
    while brackets > 0 do begin
      if p = length(s) then begin  // parameter on next line
        inc(line);
        if line < Editor.Lines.Count
          then s:= s + ' ' + Editor.Lines[line]
          else break;
      end;
      inc(p);
      if s[p] = ')' then dec(brackets) else
      if s[p] = '(' then inc(brackets);
    end;
    s:= copy(s, 1, p);
  end { else if s[p] = '[' then begin
    s1:= copy(s, 1, p-1);
    while p <= length(s) do begin
      if s[p] = ']' then
        s1:= s1 + '[]';
      inc(p);
    end;
    s:= s1
  end  }
  else
    s:= copy(s, 1, p-1);

  // expand to left
  p:= Caret.X;
  while (p > 0) and (p <= length(s)) and (IsCharAlphaNumeric(s[p]) or CharInSet(s[p], ['.', '_', ')', ']'])) do begin
    if s[p] = ')' then begin
      brackets:= 1;
      while (p > 0) and (brackets > 0) do begin
        dec(p);
        if s[p] = ')' then inc(brackets) else
        if s[p] = '(' then dec(brackets);
      end;
    end;
    if s[p] = ']' then begin
      brackets:= 1;
      while (p > 0) and (brackets > 0) do begin
        dec(p);
        if s[p] = ']' then inc(brackets) else
        if s[p] = '[' then dec(brackets);
      end;
    end;
    dec(p);
  end;
  delete(s, 1, p);
  Result:= s;
end;

procedure TFEditForm.DoOnBuildStructure(Sender: TObject);
begin
  NeedsParsing:= true;
end;

// var Count: integer = 0;

procedure TFEditForm.ParseSourcecodeWithThread(hasChanged: boolean);
begin
  NeedsParsing:= NeedsParsing or hasChanged;

  { // debugging
    inc(Count);
    FMessages.OutputToTerminal('Thread Count: ' + IntTostr(Count));
  }

  if assigned(ParseThread) then begin
    if (ParseThread.State > 0) and  (ParseThread.State < 3) then begin
      ParseThread.abort:= true; // finish the job
      ParseThread.WaitFor;
    end;
    FreeAndNil(ParseThread);
  end;

  if NeedsParsing then begin
    ParseThread:= TParseThread.Create(Self, true);
  end;
end;

procedure TFEditForm.ParseSourceCode(hasChanged: boolean);
begin
  NeedsParsing:= NeedsParsing or hasChanged;
  if IsJava and NeedsParsing then begin
    if assigned(ParseThread) then begin
      ParseThread.abort:= true;
      ParseThread.WaitFor;
      FreeAndNil(ParseThread);
    end;

    if assigned(Model) then
      Model.Clear;
    FConfiguration.ImportCache.Clear;
    var Importer:= TJavaImporter.Create(Model, TFileProvider.Create);
    try
      Importer.AddClasspath(UnHideBlanks(FConfiguration.getClassPathJarExpanded(Pathname, getPackage)), Pathname);
      var Str:= Importer.CodeProvider.LoadStream(Pathname, Self);
      if assigned(Str) then begin
        FreeAndNil(Parser);
        Parser:= TJavaParser.Create(true);
        Parser.NeedPackage:= Importer.NeedPackageHandler;
        Parser.ParseStream(Str, Model.ModelRoot, Model, Pathname, false, false);
        Editor.Structures:= Parser.Structures.clone;
      end;
    finally
      FreeAndNil(Importer);
    end;
    //FreeAndNil(Str); handeld by Scanner
    if FConfiguration.FixImports then begin
      AutomatedCompleteImports;
      NeedsParsing:= true;
    end else begin
      NeedsParsing:= false;
      CreateTVFileStructure;
    end;
  end;
  FConfiguration.FixImports:= false;
end;

function TFEditForm.ClassnameDifferentFromAncestors(const aClassname: string): boolean;
begin
  var MClassifier:= MyCodeCompletion.getMClassifier(aClassname, Self);
  Result:= true;
  while Result and assigned(MClassifier) do begin
    var aAncestor:= ExtractClassName(MClassifier.getAncestorName);
    if aAncestor = aClassname
      then Result:= false
      else MClassifier:= MyCodeCompletion.getMClassifier(aAncestor, Self);
  end;
end;

procedure TFEditForm.InitShowCompileErrors;
begin
  if Editor.Errors.Count > 0 then begin
    Editor.InitShowCompileErrors;
    Editor.InvalidateGutter;
    ClearCompilerErrorMarks;
    Application.ProcessMessages;
  end;
end;

procedure TFEditForm.setErrorMark(line, column: integer; const error: string);
  var Mark: TSynEditMark; i, j: integer; myhint: string;
begin
  Editor.setCompileError(Point(column, line+1));
  j:= -1;
  for i:= Editor.Marks.Count-1 downto 0 do
    if (Editor.Marks.Items[i].ImageIndex = ErrorMarkIndex) and
       (Editor.Marks.Items[i].Line = line) then begin
      j:= i;
      break;
    end;

  if j > -1 then begin
    myhint:= Editor.Marks.Items[i].Hint;
    if myhint = '' then
      Editor.Marks.Items[i].Hint:= error;
  end else
    with Editor do begin
      Mark:= TSynEditMark.Create(Editor);
      Mark.Line:= line;
      Mark.Char:= column;
      Mark.ImageIndex:= ErrorMarkIndex;
      Mark.Visible:= true;
      Mark.Hint:= error;
      Marks.Add(Mark);
    end;
end;

procedure TFEditForm.ShowCompileErrors;
begin
  Editor.ShowCompileErrors;
end;

procedure TFEditForm.TerminateThread(Sender: TObject);
begin
  if not NeedsParsing then
    CreateTVFileStructure;
end;

procedure TFEditForm.ChangeStyle;
begin
  if FConfiguration.isDark then begin
    EditFormToolbar.Images:= vilEditorToolbarDark;
    PopupEditor.Images:= vilContextMenuDark;
    Editor.BookMarkOptions.BookmarkImages:= vilBookmarksDark;
  end else begin
    EditFormToolbar.Images:= vilEditorToolbarLight;
    PopupEditor.Images:= vilContextMenuLight;
    Editor.BookMarkOptions.BookmarkImages:= vilBookmarksLight;
  end;
end;

procedure TFEditForm.RemoveShortCutFromEditor(ShortCut: integer);
begin
  var i:= Editor.Keystrokes.FindShortcut(ShortCut);
  if i >= 0 then
    Editor.Keystrokes.Delete(i);
end;

procedure TFEditForm.ReplaceShortCutFromEditor(ShortCut, ShortCut2: integer);
begin
  var i:= Editor.Keystrokes.FindShortcut(ShortCut);
  if i >= 0 then
    try
      Editor.Keystrokes.Items[i].ShortCut:= ShortCut2;
    except
    end;
end;

procedure TFEditForm.EditShortCuts;
  var p, Line, Key, Key2: integer;
      Keys: TStringList;
      s: string;

  function GetNextLine: string;
  begin
    inc(Line);
    if Line < Keys.Count
      then result:= Keys[Line]
      else result:= '';
  end;

begin
  if FileExists(FConfiguration.KeyboardFile) then begin
    Keys:= TStringList.Create;
    Keys.LoadFromFile(FConfiguration.KeyboardFile);
    Line:= -1;
    s:= getNextLine;
    repeat
      p:= Pos('shortcut:', s);
      if Pos('shortcut:end', s) > 0 then p:= 0;
      if p = 1 then begin
        Key:= FConfiguration.StringToShortCut(s);
        RemoveShortCutFromEditor(Key);
        repeat
          s:= getNextLine;
          p:= pos('shortcut:end', s);
        until (p = 1) or (Line >= Keys.Count-1);
      end else begin
        p:= Pos('disableEditor:', s);
        if p = 1 then begin
          Key:= FConfiguration.StringToShortCut(s);
          RemoveShortCutFromEditor(Key);
        end else begin
          p:= Pos('replaceEditor:', s);
          if p = 1 then begin
            Key:= FConfiguration.StringToShortCut(s);
            s:= getNextLine;
            p:= Pos('with:', s);
            if p = 1 then begin
              Key2:= FConfiguration.StringToShortCut(s);
              ReplaceShortCutFromEditor(Key, Key2);
            end;
          end;
        end;
      end;
      s:= getNextLine;
    until Line >= Keys.Count-1;
    FreeAndNil(Keys);
  end;
end;

function TFEditForm.MakeUpperEvents(Events: string): string;
begin
  if Events <> '' then begin
    Events[1]:= UpCase(Events[1]);
    for var i:= 1 to length(Events) do
      if (Events[i] = '|') and (i < length(Events)) then
        Events[i+1]:= UpCase(Events[i+1]);
  end;
  Result:= Events;
end;

procedure TFEditForm.SetFXBackgroundAsString(const Container, aName, aColor: string);
begin
  if aColor = '' then
    DeleteAttributeValue(aName + '.setBackground(')
  else begin
    var s1:= aName + '.setBackground(';
    var s2:= 'new Background(new BackgroundFill(' + aColor + ', CornerRadii.EMPTY, Insets.EMPTY)));';
    s2:= FConfiguration.Indent2 + s1 + s2;
    if Container = 'root'
      then setAttributValue(Container, s1, s2, 1)
      else setAttributValue(Container, s1, s2, 0);
    InsertImport('javafx.scene.paint.Color');
    InsertImport('javafx.geometry.Insets');
    InsertImport('javafx.scene.layout.*');
  end;
end;

procedure TFEditForm.DPIChanged;
begin
  setFontSize(0);
  Hide;
  Show;
end;

end.
