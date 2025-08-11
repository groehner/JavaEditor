{ -------------------------------------------------------------------------------
  Unit:     UJava
  Author:   Gerhard Röhner
  Date:     August 2001
  Purpose:  The main form of the Java-Editor
  ------------------------------------------------------------------------------- }

unit UJava;

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Forms,
  Controls,
  Dialogs,
  Buttons,
  ExtCtrls,
  ComCtrls,
  SyncObjs,
  Generics.Collections,
  JvTabBar,
  VirtualTrees,
  SynEdit,
  SynEditSearch,
  SynCompletionProposal,
  Vcl.ToolWin,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  TB2Item,
  SpTBXItem,
  TB2Dock,
  TB2Toolbar,
  SpTBXTabs,
  SpTBXDkPanels,
  SpTBXControls,
  SynEditMiscClasses,
  USynEditEx,
  UDockForm,
  UEditorForm,
  UBaseForm,
  UUMLForm,
  UStructogramform,
  USequenceForm,
  UBrowserForm,
  UGUIForm,
  Vcl.DdeMan;

const
  WindowOffset = 16;
  WithoutBackup = False;

type

  TFormsList = TObjectList<TFForm>;

  TFJava = class(TForm)
    BottomDockPanel: TPanel;
    RightDockPanel: TPanel;
    HSplitter: TSplitter;
    VSplitter: TSplitter;
    EditorAgeTimer: TTimer;
    ToolBarUtilities: TToolBar;
    TBPlayground: TToolButton;
    TBTurtle: TToolButton;
    TBTimer: TToolButton;
    ToolBarFXShapes: TToolBar;
    TBFXCircle: TToolButton;
    TBFXRectangle: TToolButton;
    TBFXEllipse: TToolButton;
    TBFXPolygon: TToolButton;
    TBFXPolyline: TToolButton;
    TBFXArc: TToolButton;
    TBFXLine: TToolButton;
    TBFXCubicCurve: TToolButton;
    TBFXText: TToolButton;
    TBFXQuadCurve: TToolButton;
    TBFXSVGPath: TToolButton;
    ActiveWindowTimer: TTimer;
    MainPanel: TPanel;
    CloseTimer: TTimer;
    FDFont: TFontDialog;
    ODOpen: TOpenDialog;
    SDSaveAs: TSaveDialog;
    StyleTimer: TTimer;
    vilSwing2: TVirtualImageList;
    vilFXBaseDark: TVirtualImageList;
    vilFXBaseLight: TVirtualImageList;
    vilSwing1Dark: TVirtualImageList;
    vilSwing1Light: TVirtualImageList;
    vilAWTDark: TVirtualImageList;
    vilAWTLight: TVirtualImageList;
    vilFXControls: TVirtualImageList;
    vilFXShapesDark: TVirtualImageList;
    vilFXShapesLight: TVirtualImageList;
    vilProgramLight: TVirtualImageList;
    vilProgramDark: TVirtualImageList;
    vilUtilities: TVirtualImageList;
    vilLayoutLight: TVirtualImageList;
    vilToolbarLight: TVirtualImageList;
    vilToolbarDark: TVirtualImageList;
    vilToolbarDisabledLight: TVirtualImageList;
    vilToolbarDisabledDark: TVirtualImageList;
    vilMenuLight: TVirtualImageList;
    vilMenuDark: TVirtualImageList;
    TBXDockTop: TSpTBXDockablePanel;
    MainMenu: TSpTBXToolbar;
    MIFile: TSpTBXSubmenuItem;
    MINew: TSpTBXSubmenuItem;
    MINewJava: TSpTBXItem;
    MINewText: TSpTBXItem;
    MINewHtml: TSpTBXItem;
    N19Sep: TSpTBXSeparatorItem;
    MIFileNewClass: TSpTBXItem;
    MINewStructogram: TSpTBXItem;
    MINewSequencediagram: TSpTBXItem;
    N10Sep: TSpTBXSeparatorItem;
    MINewConsole: TSpTBXItem;
    MINewFrame: TSpTBXItem;
    MINewDialog: TSpTBXItem;
    MINewApplet: TSpTBXItem;
    MINewJFrame: TSpTBXItem;
    MINewJDialog: TSpTBXItem;
    MINewJApplet: TSpTBXItem;
    MIApplication: TSpTBXItem;
    MIOpen: TSpTBXItem;
    MIReopen: TSpTBXSubmenuItem;
    N15Sep: TSpTBXSeparatorItem;
    MISave: TSpTBXItem;
    MISaveAs: TSpTBXItem;
    MISaveAll: TSpTBXItem;
    MISaveAllIn: TSpTBXItem;
    MISaveAsProject: TSpTBXItem;
    MICloseProject: TSpTBXItem;
    MIClose: TSpTBXItem;
    MICloseAllFiles: TSpTBXItem;
    MIExport: TSpTBXItem;
    N1Sep: TSpTBXSeparatorItem;
    MIPrint: TSpTBXItem;
    MIPrintAll: TSpTBXItem;
    MIPrintSetup: TSpTBXItem;
    N9Sep: TSpTBXSeparatorItem;
    MIExit: TSpTBXItem;
    MIEdit: TSpTBXSubmenuItem;
    MIUndo: TSpTBXItem;
    MIRedo: TSpTBXItem;
    N3Sep: TSpTBXSeparatorItem;
    MICut: TSpTBXItem;
    MICopy: TSpTBXSubmenuItem;
    MICopyNormal: TSpTBXItem;
    MICopyRTF: TSpTBXItem;
    MICopyRtfNumbered: TSpTBXItem;
    MICopyHTML: TSpTBXItem;
    MICopyHTMLAsText: TSpTBXItem;
    MICopyNumbered: TSpTBXItem;
    MIPaste: TSpTBXItem;
    N4Sep: TSpTBXSeparatorItem;
    MISearch: TSpTBXItem;
    MISearchAgain: TSpTBXItem;
    MIReplace: TSpTBXItem;
    MISearchInFiles: TSpTBXItem;
    N8Sep: TSpTBXSeparatorItem;
    MIUnindent: TSpTBXItem;
    MIIndent: TSpTBXItem;
    MIStructuredIndent: TSpTBXItem;
    MICommentOnOff: TSpTBXItem;
    MIStrich10: TSpTBXSeparatorItem;
    MIGotoLine: TSpTBXItem;
    MISystemOutPrintln: TSpTBXItem;
    MIUnicode: TSpTBXItem;
    MIStart: TSpTBXSubmenuItem;
    MICompile: TSpTBXItem;
    MIMindstorms: TSpTBXItem;
    MICompileAll: TSpTBXItem;
    MIRun: TSpTBXItem;
    MIProgramReset: TSpTBXItem;
    MIJavaReset: TSpTBXItem;
    MIParameter: TSpTBXItem;
    MI30Sep: TSpTBXSeparatorItem;
    MIHTMLforApplet: TSpTBXItem;
    MIHTMLforJavaPlugIn: TSpTBXItem;
    MIAppletviewer: TSpTBXItem;
    N6Sep: TSpTBXSeparatorItem;
    MIDebugger: TSpTBXItem;
    MIDissasembler: TSpTBXItem;
    MIJavaDoc: TSpTBXItem;
    MIJar: TSpTBXSubmenuItem;
    MIJarCreate: TSpTBXItem;
    MIJarPack: TSpTBXItem;
    MIJarShow: TSpTBXItem;
    MIJarUnpack: TSpTBXItem;
    MIJarOpen: TSpTBXItem;
    MITest: TSpTBXSubmenuItem;
    MIStep: TSpTBXItem;
    MINext: TSpTBXItem;
    MIStepUp: TSpTBXItem;
    MIRunToCursor: TSpTBXItem;
    MIShowExecutionPoint: TSpTBXItem;
    N12Sep: TSpTBXSeparatorItem;
    MIBreakpoint: TSpTBXItem;
    MIBreakpointsClear: TSpTBXItem;
    N13Sep: TSpTBXSeparatorItem;
    MIExpression: TSpTBXItem;
    MIWatches: TSpTBXItem;
    MITestCreateSequencediagram: TSpTBXItem;
    MIUML: TSpTBXSubmenuItem;
    MINewUML: TSpTBXItem;
    MINewClass: TSpTBXItem;
    MIClassOpen: TSpTBXItem;
    MIClassEditor: TSpTBXItem;
    MINewComment: TSpTBXItem;
    MINewLayout: TSpTBXItem;
    MIRefresh: TSpTBXItem;
    MIDiagramFromOpenFiles: TSpTBXItem;
    MIOpenFolder: TSpTBXItem;
    MISaveAsPicture: TSpTBXItem;
    MIUMLCreateSequencediagram: TSpTBXItem;
    MITools: TSpTBXSubmenuItem;
    MICheckStyle: TSpTBXItem;
    MIJalopy: TSpTBXItem;
    MICompare: TSpTBXItem;
    MISubversion: TSpTBXSubmenuItem;
    MISVNCommit: TSpTBXItem;
    MISVNAdd: TSpTBXItem;
    MISVNLog: TSpTBXItem;
    MISVNCompare: TSpTBXItem;
    N16Sep: TSpTBXSeparatorItem;
    MISVNStatus: TSpTBXItem;
    MISVNTree: TSpTBXItem;
    MISVNUpdate: TSpTBXItem;
    MIGit: TSpTBXSubmenuItem;
    MIGitStatus: TSpTBXItem;
    MIGitAdd: TSpTBXItem;
    MIGitCommit: TSpTBXItem;
    MIGitLog: TSpTBXItem;
    N20Sep: TSpTBXSeparatorItem;
    MIGitReset: TSpTBXItem;
    MIGitCheckout: TSpTBXItem;
    MIGitRemove: TSpTBXItem;
    N21Sep: TSpTBXSeparatorItem;
    MIGitRemote: TSpTBXItem;
    MIGitFetch: TSpTBXItem;
    MIGitPush: TSpTBXItem;
    N22Sep: TSpTBXSeparatorItem;
    MIGitGUI: TSpTBXItem;
    MIGitViewer: TSpTBXItem;
    MIGitConsole: TSpTBXItem;
    MIJUnit: TSpTBXSubmenuItem;
    MIJUnitRunAllTests: TSpTBXItem;
    MIJUnitCreateTestclass: TSpTBXItem;
    N17Sep: TSpTBXSeparatorItem;
    MIConfigureTools: TSpTBXItem;
    MIComponents: TSpTBXSubmenuItem;
    MIProgramm: TSpTBXSubmenuItem;
    MIConsole: TSpTBXItem;
    MIFrame: TSpTBXItem;
    MIDialog: TSpTBXItem;
    MIApplet: TSpTBXItem;
    MIJFrame: TSpTBXItem;
    MIJDialog: TSpTBXItem;
    MIJApplet: TSpTBXItem;
    N5Sep: TSpTBXSeparatorItem;
    MIJavaDoc1: TSpTBXItem;
    MIclass: TSpTBXItem;
    MIKontrollstrukturen: TSpTBXSubmenuItem;
    MIIf: TSpTBXItem;
    MIifelse: TSpTBXItem;
    MIwhile: TSpTBXItem;
    MIdowhile: TSpTBXItem;
    MIfor: TSpTBXItem;
    MISwitch: TSpTBXItem;
    MITry: TSpTBXItem;
    MIDatentypen: TSpTBXSubmenuItem;
    MIByte: TSpTBXItem;
    MIShort: TSpTBXItem;
    MIInt: TSpTBXItem;
    MILong: TSpTBXItem;
    MIFloat: TSpTBXItem;
    MIDouble: TSpTBXItem;
    MIChar: TSpTBXItem;
    MIBoolean: TSpTBXItem;
    MIString: TSpTBXItem;
    MIAWT: TSpTBXSubmenuItem;
    MILabel: TSpTBXItem;
    MITextField: TSpTBXItem;
    MINumberField: TSpTBXItem;
    MITextArea: TSpTBXItem;
    MIButton: TSpTBXItem;
    MICheckbox: TSpTBXItem;
    MIRadiobutton: TSpTBXItem;
    MICheckBoxGroup: TSpTBXItem;
    MIList: TSpTBXItem;
    MIChoice: TSpTBXItem;
    MIScrollbar: TSpTBXItem;
    MIScrollPane: TSpTBXItem;
    MIPanel: TSpTBXItem;
    MIMenu: TSpTBXItem;
    MICanvas: TSpTBXItem;
    MITurtle: TSpTBXItem;
    MIMenuBar: TSpTBXItem;
    MITimer: TSpTBXItem;
    MIPopUpMenu: TSpTBXItem;
    MISwing1: TSpTBXSubmenuItem;
    MIJLabel: TSpTBXItem;
    MIJTextField: TSpTBXItem;
    MIJNumberField: TSpTBXItem;
    MIJTextArea: TSpTBXItem;
    MIJButton: TSpTBXItem;
    MIJCheckbox: TSpTBXItem;
    MIJRadiobutton: TSpTBXItem;
    MIJCheckBoxGroup: TSpTBXItem;
    MIJList: TSpTBXItem;
    MIJComboBox: TSpTBXItem;
    MIJSpinner: TSpTBXItem;
    MIJScrollbar: TSpTBXItem;
    MIJScrollPane: TSpTBXItem;
    MIJPanel: TSpTBXItem;
    MIJCanvas: TSpTBXItem;
    MIJTurtle: TSpTBXItem;
    MIJMenuBar: TSpTBXItem;
    MIJMenu: TSpTBXItem;
    MIJPopUpMenu: TSpTBXItem;
    MIJTimer: TSpTBXItem;
    MISwing2: TSpTBXSubmenuItem;
    MIJSlider: TSpTBXItem;
    MIJProgressBar: TSpTBXItem;
    MIJSplitPane: TSpTBXItem;
    MIJTabbedPane: TSpTBXItem;
    MIJTable: TSpTBXItem;
    MIJTree: TSpTBXItem;
    MIJToolbar: TSpTBXItem;
    MIJSeparator: TSpTBXItem;
    MIJToggleButton: TSpTBXItem;
    MIJPasswordField: TSpTBXItem;
    MIJFormattedTextField: TSpTBXItem;
    MIJEditorPane: TSpTBXItem;
    MIJTextPane: TSpTBXItem;
    MIJLayeredPane: TSpTBXItem;
    MIJDesktopPane: TSpTBXItem;
    MIJInternalFrame: TSpTBXItem;
    MIJFileChooserOpen: TSpTBXItem;
    MIJFileChooserSave: TSpTBXItem;
    MIJColorChooser: TSpTBXItem;
    MIJOptionPane: TSpTBXItem;
    MILayout: TSpTBXSubmenuItem;
    MIBorderLayout: TSpTBXItem;
    MIFlowLayout: TSpTBXItem;
    MIGridLayout: TSpTBXItem;
    MICardLayout: TSpTBXItem;
    MIGridBagLayout: TSpTBXItem;
    MIAbsoluteLayout: TSpTBXItem;
    MIWindow: TSpTBXSubmenuItem;
    MIObjectInspector: TSpTBXItem;
    MIToolbar: TSpTBXItem;
    MIMessages: TSpTBXItem;
    MIMessagesDocked: TSpTBXItem;
    MIFileStructure: TSpTBXItem;
    MIDefaultLayout: TSpTBXItem;
    MIMsDos: TSpTBXItem;
    MIExplorer: TSpTBXItem;
    MIBrowser: TSpTBXItem;
    MIStrich: TSpTBXSeparatorItem;
    MIFont: TSpTBXItem;
    MIConfiguration: TSpTBXItem;
    N14Sep: TSpTBXSeparatorItem;
    MIMaximized: TSpTBXItem;
    MICascade: TSpTBXItem;
    MITileVertical: TSpTBXItem;
    MITileHorizontal: TSpTBXItem;
    N11Sep: TSpTBXSeparatorItem;
    MIHelp: TSpTBXSubmenuItem;
    MIHelpHelp: TSpTBXItem;
    MIAPI: TSpTBXItem;
    MIJavaFx: TSpTBXItem;
    MIJunitManual: TSpTBXItem;
    N7Sep: TSpTBXSeparatorItem;
    MIDemos: TSpTBXItem;
    MITutorial: TSpTBXItem;
    MIJavabook: TSpTBXItem;
    MIMindstormsHelp: TSpTBXItem;
    N2Sep: TSpTBXSeparatorItem;
    MIWebsite: TSpTBXItem;
    MIUpdate: TSpTBXItem;
    MIAbout: TSpTBXItem;
    MIDebug: TSpTBXItem;
    MainToolBar: TSpTBXToolbar;
    TBOpen: TSpTBXItem;
    TBSave: TSpTBXItem;
    TBSaveAll: TSpTBXItem;
    TBDefaultLayout: TSpTBXItem;
    TBPropertyInspector: TSpTBXItem;
    TBDiagramFromOpenFiles: TSpTBXItem;
    TBMessages: TSpTBXItem;
    DebugToolbar: TSpTBXToolbar;
    TBRedo: TSpTBXItem;
    TBUndo: TSpTBXItem;
    TBStep: TSpTBXItem;
    TBNext: TSpTBXItem;
    TBCompileJava: TSpTBXItem;
    TBCompileAll: TSpTBXItem;
    TBRun: TSpTBXItem;
    TabsControl: TSpTBXTabControl;
    TabUtilities: TSpTBXTabItem;
    TSUtilities: TSpTBXTabSheet;
    TabSwing1: TSpTBXTabItem;
    TSSwing1: TSpTBXTabSheet;
    TabAWT: TSpTBXTabItem;
    TSAWT: TSpTBXTabSheet;
    TabProgram: TSpTBXTabItem;
    TSProgram: TSpTBXTabSheet;
    TabSwing2: TSpTBXTabItem;
    TSSwing2: TSpTBXTabSheet;
    TabLayout: TSpTBXTabItem;
    TSLayout: TSpTBXTabSheet;
    TabFXBase: TSpTBXTabItem;
    TSFXBase: TSpTBXTabSheet;
    TabFXControls: TSpTBXTabItem;
    TSFXControls: TSpTBXTabSheet;
    TabFXShapes: TSpTBXTabItem;
    TSFXShapes: TSpTBXTabSheet;
    PBorder: TPanel;
    SBNorth: TSpeedButton;
    SBSouth: TSpeedButton;
    SBWest: TSpeedButton;
    SBEast: TSpeedButton;
    SBCenter: TSpeedButton;
    ToolbarAWT: TToolBar;
    TBLabel: TToolButton;
    TBTextField: TToolButton;
    TBNumberField: TToolButton;
    TBTextArea: TToolButton;
    TBButton: TToolButton;
    TBCheckbox: TToolButton;
    TBCheckboxGroup: TToolButton;
    TBList: TToolButton;
    TBChoice: TToolButton;
    TBScrollbar: TToolButton;
    TBScrollPane: TToolButton;
    TBPanel: TToolButton;
    TBCanvas: TToolButton;
    TBATurtle: TToolButton;
    TBMenuBar: TToolButton;
    TBMenu: TToolButton;
    TBPopupMenu: TToolButton;
    ToolBarFXBase: TToolBar;
    TBFXLabel: TToolButton;
    TBFXTextField: TToolButton;
    TBFXNumberField: TToolButton;
    TBFXTextArea: TToolButton;
    TBFXButton: TToolButton;
    TBFXCheckBox: TToolButton;
    TBFXButtonGroup: TToolButton;
    TBFXListView: TToolButton;
    TBFXComboBox: TToolButton;
    TBFXSpinner: TToolButton;
    TBFXScrollPane: TToolButton;
    TBFXPane: TToolButton;
    TBFXCanvas: TToolButton;
    TBFXTurtle: TToolButton;
    TBFXMenuBar: TToolButton;
    TBFXMenu: TToolButton;
    TBFXContextMenu: TToolButton;
    TBFXMenuButton: TToolButton;
    TBFXSplitMenuButton: TToolButton;
    ToolBarFXControls: TToolBar;
    TBFXSlider: TToolButton;
    TBFXProgressBar: TToolButton;
    TBFXProgressIndicator: TToolButton;
    TBFXToolbar: TToolButton;
    TBFXSeparator: TToolButton;
    TBFXToggleButton: TToolButton;
    TBFXPasswordField: TToolButton;
    TBFXChoiceBox: TToolButton;
    TBFXHyperlink: TToolButton;
    TBFXHTMLEditor: TToolButton;
    TBFXWebView: TToolButton;
    TBFXColorPicker: TToolButton;
    TBFXDatePicker: TToolButton;
    TBFXPagination: TToolButton;
    TBFXFileOpenChooser: TToolButton;
    TBFXFileSaveChooser: TToolButton;
    TBFXDirectoryChooser: TToolButton;
    TBFXImageView: TToolButton;
    TBFXMediaView: TToolButton;
    TBFXTableView: TToolButton;
    TBFXTreeView: TToolButton;
    ToolBarSwing1: TToolBar;
    TBJLabel: TToolButton;
    TBJTextField: TToolButton;
    TBJNumberField: TToolButton;
    TBJTextArea: TToolButton;
    TBJButton: TToolButton;
    TBJCheckbox: TToolButton;
    TBJButtonGroup: TToolButton;
    TBJList: TToolButton;
    TBJComboBox: TToolButton;
    TBJSpinner: TToolButton;
    TBJScrollBar: TToolButton;
    TBJScrollPane: TToolButton;
    TBJPanel: TToolButton;
    TBJCanvas: TToolButton;
    TBJTurtle: TToolButton;
    TBJMenuBar: TToolButton;
    TBJMenu: TToolButton;
    TBJPopupMenu: TToolButton;
    ToolbarSwing2: TToolBar;
    TBJSlider: TToolButton;
    TBJProgressBar: TToolButton;
    TBJSplitPane: TToolButton;
    TBJTabbedPane: TToolButton;
    TBJTable: TToolButton;
    TBJTree: TToolButton;
    TBJToolBar: TToolButton;
    TJSeparator: TToolButton;
    TJToggleButton: TToolButton;
    TBJPasswordField: TToolButton;
    TBJFormattedTextField: TToolButton;
    TBJEditorPane: TToolButton;
    TBJTextPane: TToolButton;
    TBJLayeredPane: TToolButton;
    TBJDesktopPane: TToolButton;
    TJInternalFrame: TToolButton;
    TBJFileOpen: TToolButton;
    TBJFileSave: TToolButton;
    TBJColorChooser: TToolButton;
    TBJOptionPane: TToolButton;
    ToolBarLayout: TToolBar;
    TBBorderLayout: TToolButton;
    TBFlowLayout: TToolButton;
    TBGridLayout: TToolButton;
    TBCardLayout: TToolButton;
    TBGridBagLayout: TToolButton;
    TBAbsoluteLayout: TToolButton;
    ToolbarProgram: TToolBar;
    TBNew: TToolButton;
    TBClass: TToolButton;
    TBStructogram: TToolButton;
    TBSequence: TToolButton;
    TBConsole: TToolButton;
    TBFrame: TToolButton;
    TBDialog: TToolButton;
    TBApplet: TToolButton;
    TBJFrame: TToolButton;
    TBJDialog: TToolButton;
    TBJApplet: TToolButton;
    TBApplication: TToolButton;
    ControlBar: TSpTBXPanel;
    vilLayoutDark: TVirtualImageList;
    MIRecognizeAssociations: TSpTBXItem;
    MIChat: TSpTBXItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MINewClick(Sender: TObject); // File menu
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure MIPrintClick(Sender: TObject);
    procedure MIPrintSetupClick(Sender: TObject);
    procedure MIMenuOpenClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIUndoClick(Sender: TObject); // Edit menu
    procedure MICutClick(Sender: TObject);
    procedure MICopyRTFClick(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure MISearchClick(Sender: TObject);
    procedure MIReplaceClick(Sender: TObject);
    procedure MIGotoLineClick(Sender: TObject);
    procedure MIUnindentClick(Sender: TObject);
    procedure MIIndentClick(Sender: TObject);

    procedure MICompileClick(Sender: TObject); // Start menu
    procedure MIRunClick(Sender: TObject);
    procedure MIProgramResetClick(Sender: TObject);
    procedure MIHTMLforAppletClick(Sender: TObject);
    procedure MIAppletviewerClick(Sender: TObject);
    procedure MIDissasemblerClick(Sender: TObject);
    procedure MIJavaDocClick(Sender: TObject);
    procedure MIDefaultLayoutClick(Sender: TObject);
    procedure MICascadeClick(Sender: TObject);
    procedure Tile(Horizontal: Boolean);
    procedure MITileVerticalClick(Sender: TObject);
    procedure MITileHorizontalClick(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure MIConfigurationClick(Sender: TObject);

    // help menu
    procedure MIHelpHelpClick(Sender: TObject);
    procedure MIAPIClick(Sender: TObject);
    procedure MIJavaFxClick(Sender: TObject);
    procedure MIDemosClick(Sender: TObject);
    procedure MITutorialClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);

    { --- control structures }
    procedure SBKontrollstrukturenClick(Sender: TObject);

    { --- Programm }
    procedure SBProgramClick(Sender: TObject);
    procedure SBGUIFrameClick(Sender: TObject);
    procedure MISwingClick(Sender: TObject);
    procedure TBSwingClick(Sender: TObject);
    procedure TBLayoutClick(Sender: TObject);
    procedure MIRedoClick(Sender: TObject);
    procedure MIExportClick(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure SystemExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure MIMSDosClick(Sender: TObject);
    procedure MICopyHTMLClick(Sender: TObject);
    procedure MIBrowserClick(Sender: TObject);
    procedure MIMaximizedClick(Sender: TObject);
    procedure TBRunClick(Sender: TObject);
    procedure MIJavabookClick(Sender: TObject);
    procedure MIParameterClick(Sender: TObject);
    procedure MISearchAgainClick(Sender: TObject);
    procedure MISystemOutPrintlnClick(Sender: TObject);
    procedure MIStepClick(Sender: TObject);
    procedure MINextClick(Sender: TObject);
    procedure MIBreakpointClick(Sender: TObject);
    procedure MIBreakpointsClearClick(Sender: TObject);
    procedure MIStepUpClick(Sender: TObject);
    procedure MIExpressionClick(Sender: TObject);
    procedure ShowDockPanel(APanel: TPanel; MakeVisible: Boolean;
      Client: TControl);
    procedure DockPanelDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure BottomDockPanelDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure BottomDockPanelUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure RightDockPanelUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure RightDockPanelDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure BottomDockPanelStartDock(Sender: TObject;
      var DragObject: TDragDockObject);
    procedure RightDockPanelGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure BottomDockPanelGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure ShowDockableForm(DockWindow: TDockableForm);
    procedure VSplitterMoved(Sender: TObject);
    procedure HSplitterMoved(Sender: TObject);
    procedure MIMessagesClick(Sender: TObject);
    procedure MIDebuggerClick(Sender: TObject);
    procedure MICompileAllClick(Sender: TObject);
    procedure TBCompileAllClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MICloseAllFilesClick(Sender: TObject);
    procedure MICommentOnOffClick(Sender: TObject);
    procedure MISaveAllClick(Sender: TObject);
    procedure MIWebsiteClick(Sender: TObject);
    procedure MIMindstormsHelpClick(Sender: TObject);
    procedure MISearchInFilesClick(Sender: TObject);
    procedure MICheckstyleClick(Sender: TObject);
    procedure MIJalopyClick(Sender: TObject);
    procedure TBMsDosClick(Sender: TObject);
    procedure MISaveAsPictureClick(Sender: TObject);
    procedure MIOpenFolderClick(Sender: TObject);
    procedure MINewLayoutClick(Sender: TObject);
    procedure MIWatchesClick(Sender: TObject);
    procedure MIToolbarClick(Sender: TObject);
    procedure MIUpdateClick(Sender: TObject);
    procedure MISaveAsProjectClick(Sender: TObject);
    procedure MIExplorerClick(Sender: TObject);
    procedure MIObjectInspectorClick(Sender: TObject);
    procedure MIClassEditorClick(Sender: TObject);
    procedure MIDiagramFromOpenFilesClick(Sender: TObject);
    procedure MINewClassClick(Sender: TObject);
    procedure MIJarCreateClick(Sender: TObject);
    procedure MIJarPackClick(Sender: TObject);
    procedure MIJarClick(Sender: TObject);
    procedure MIMessagesDockedClick(Sender: TObject);
    procedure MICopyNumberedClick(Sender: TObject);
    procedure MIRefreshClick(Sender: TObject);
    procedure MIClassOpenClick(Sender: TObject);
    procedure MICopyHTMLAsTextClick(Sender: TObject);
    procedure MIShowExecutionPointClick(Sender: TObject);
    procedure MIRunToCursorClick(Sender: TObject);
    procedure ControlBarDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure PrepareClassEdit(const Pathname, Status: string; UML: TFUMLForm);
    procedure MINewUMLClick(Sender: TObject);
    procedure MICompareClick(Sender: TObject);
    procedure MIUnicodeClick(Sender: TObject);

    procedure MISVNAddClick(Sender: TObject);
    procedure MISVNCommitClick(Sender: TObject);
    procedure MISVNTreeClick(Sender: TObject);
    procedure MISVNStatusClick(Sender: TObject);
    procedure MISVNCompareClick(Sender: TObject);
    procedure MISVNLogClick(Sender: TObject);
    procedure MISVNUpdateClick(Sender: TObject);

    // Code-Completion
    procedure scpJavaExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var XPos, YPos: Integer;
      var CanExecute: Boolean);
    procedure scpParamsExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var XPos, YPos: Integer;
      var CanExecute: Boolean);
    procedure scpJavaOnCodeCompletion(Sender: TObject; var Value: string;
      Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure scpJavaAfterCodeCompletion(Sender: TObject; const Value: string;
      Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure scpJavaChange(Sender: TObject; AIndex: Integer);
    procedure scpJavaClose(Sender: TObject);
    procedure scpJavaShow(Sender: TObject);
    procedure scpHandleMethods(Editor: TSynEdit; Index: Integer;
      EndToken: Char);

    procedure scpParamsPaintItem(Sender: TObject; Index: Integer;
      TargetCanvas: TCanvas; ItemRect: TRect; var CustomDraw: Boolean);
    procedure scpSetEditForm(Editform: TFEditForm);
    procedure scpSetEditor(Editor: TSynEdit);
    procedure EditorAgeTimerTimer(Sender: TObject);
    procedure MISaveAllInClick(Sender: TObject);
    procedure MIConfigureToolsClick(Sender: TObject);
    procedure TBPanelCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MICopyNormalClick(Sender: TObject);
    procedure MIPrintAllClick(Sender: TObject);
    procedure MICopyRtfNumberedClick(Sender: TObject);
    procedure MIJavaResetClick(Sender: TObject);
    procedure TBFontMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBNewClick(Sender: TObject);
    procedure SBClassClick(Sender: TObject);
    procedure MICloseProjectClick(Sender: TObject);
    procedure SBStructogramClick(Sender: TObject);
    procedure MINewCommentClick(Sender: TObject);
    procedure MIDebugClick(Sender: TObject);
    procedure TBJavaFXClick(Sender: TObject);
    procedure SBStructureIndentClick(Sender: TObject);
    procedure MIFileStructureClick(Sender: TObject);
    procedure TBClassOpenClick(Sender: TObject);
    procedure ToolbuttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolbuttonStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure MIJUnitCreateTestclassClick(Sender: TObject);
    procedure MIJUnitRunAllTestsClick(Sender: TObject);
    procedure MIJunitManualClick(Sender: TObject);
    procedure SBSequenceClick(Sender: TObject);
    procedure MINewSequencediagramClick(Sender: TObject);
    procedure MITestCreateSequencediagramClick(Sender: TObject);
    procedure MIUMLCreateSequencediagramClick(Sender: TObject);
    procedure ActiveWindowTimerTimer(Sender: TObject);
    procedure AppOnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure ActiveControlChanged(Sender: TObject);
    procedure MIGitClick(Sender: TObject);
    procedure CloseTimerTimer(Sender: TObject);
    procedure StyleTimerTimer(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject;
      OldDPI, NewDPI: Integer);
    procedure TBCompileJavaClick(Sender: TObject);
    procedure MIRecognizeAssociationsClick(Sender: TObject);
    procedure MIChatClick(Sender: TObject);
  private
    FActiveTDIChild: TFForm;
    FActiveTool: Integer;
    FAfterCodeCompletion: Boolean;
    FAUMLForm: TFUMLForm;
    FClosing: Boolean;
    FFixedWindows: array of TFForm;
    FEditClass: string;
    FEditClassStatus: string;
    FEditorForm: TFEditForm;
    FIdle: Integer;
    FIdle2: Integer;
    FInteractiveUMLForm: TFUMLForm;
    FLastDoubleClickTime: TDateTime;
    FLock: TCriticalSection;
    FMakeUpdate: Boolean;
    FMyTabBar: TJvTabBar;
    FPrintDialog: TPrintDialog;
    FPrinterSetup: TPrinterSetupDialog;
    FProjectFilename: string;
    FScpJava: TSynCompletionProposal;
    FScpJavaIsMethod: Boolean;
    FScpParamIndex: Integer;
    FScpParams: TSynCompletionProposal;
    FScpState: Integer;
    FSearch: Boolean;
    FSearched: string;
    FSynEditSearch: TSynEditSearch;
    FTDIFormsList: TFormsList;
    FTimerStatus: Integer;
    FUpdateMenuItemsCount: Integer;
    procedure PrepareStep;
    procedure StepNextUp(const Str: string);
    procedure SaveBeforeCompile;
    procedure Compile;
    procedure MyTabBarDblClick(Sender: TObject);
    procedure MyTabBarClosed(Sender: TObject; Item: TJvTabBarItem);
    procedure ClassEdit;
    procedure EditorSaveAs(AForm: TFForm; Hidden: Boolean);
    procedure StructogramSaveAs(Structogram: TFStructogram);
    procedure SequenceSaveAs(SequenceDiagram: TFSequenceForm);
    function GetTDIType(const Typ: string): TFForm;
    function GetTab(Num: Integer): Integer;
    function GetWindowMenuNr(Num: Integer): Integer;
    procedure NewGUIForm(const AFile: string; State: string);
    procedure NewUMLWindow(const AFile, State: string);
    function NewEditform(Hidden: Boolean): TFEditForm;
    procedure FixWindows(WithHidden: Boolean);
    procedure JarShowUnpackOpen(Num: Integer; const FileName: string);
    procedure JarCall(const Call, Dir: string);
    procedure JarOpen(const FileName: string);
    function GetUMLWindow: TFUMLForm;
    procedure LoadBounds;
    procedure OpenFileStructureAndObjectInspector;
    function GetEditFormWithMain: TFEditForm;
    procedure WMCOPYDATA(var Msg: TWMCopyData); message WM_COPYDATA;
    function HasJavaFiles: Boolean;
    function HasPascalFiles: Boolean;
    function GetActiveUML: TFUMLForm;
    function GetActiveForm: TFForm;
    function GetEditorWithMain: TFEditForm;
    function GetTabForm(Num: Integer): TFForm;
    function GetSelectedTabForm: TFForm;
    procedure SetMenuKeyCaps;
    procedure HideGuiForms;
    procedure ShowGuiForms;
    procedure SaveDocking;
    procedure LoadDocking;
    procedure ShowActive;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    function GetActiveFormTag: Integer;
    procedure DoHTMLForApplet(Editform: TFEditForm;
      ForceCreate, PlugIn, AShow, Debug: Boolean; var Applet: string);
    procedure DoJarCreate(FileName, Package: string);
    procedure CompileList(StringList: TStringList);
    procedure RefreshUMLWindows;
    procedure DoDisassemble(const FileName: string);
    function GuiDesignerOpen: Boolean;
    function OpenStructogram(const FileName, State: string): Boolean;
    function OpenSequenceDiagram(const FileName, State: string): Boolean;
    procedure UMLSaveAs(UMLForm: TFUMLForm);
    procedure ReadHistory;
    procedure OpenFileWithState(const Str: string);
    procedure OpenProject(const FileName: string);
    procedure DoSaveAsProject(const FileName: string);
    procedure NewTextDiff(Edit1, Edit2: TFEditForm);
    function NewSequenceDiagram(const FileName: string; State: string): Boolean;
    function NewStructogram(const FileName: string; State: string): Boolean;
    procedure AddToTabBar(Num: Integer; Form: TFForm);
    procedure RunAndroid;
    procedure DoRun(Editform: TFEditForm; const Pathname: string);
    procedure DoRunApplet(const JavaProgramm: string; Editform: TFEditForm);
    procedure DoRunProgram(const JavaProgram, AParameter: string;
      Editform: TFEditForm);
    procedure DoRunHTML(const Pathname: string);
  public
    function HasEditforms: Boolean;
    function IsJavaApplet(Editform: TFEditForm): Boolean;
    procedure CloseBrowser;
    procedure CallHelp(const Address: string); overload;
    procedure CallHelp(Applet: Boolean; Address: string); overload;
    procedure CallApplet(const Address: string);
    function WindowOpened(const Pathname: string; var AForm: TFForm): Boolean;
    function GetTDIWindow(const Pathname: string): TFForm; overload;
    function GetTDIWindow(Number: Integer): TFForm; overload;
    function GetTDIWindowType(const Pathname, Typ: string): TFForm;
    function GetTDIWindowTypeFilename(const FileName, Typ: string): TFForm;
    function GetEditForm(Pathname: string): TFEditForm;
    function GetGuiForm(Pathname: string): TFGUIForm;
    procedure SwitchToWindow(const Path: string); overload;
    procedure SwitchToWindow(Num: Integer); overload;
    procedure SwitchToWindow(Form: TFForm); overload;
    procedure SwitchWindowWithSearch(const Pathname: string);
    procedure ChangeWindowWithPositioning(const Pathname: string;
      XPos, YPos: Integer; Error: Boolean);
    function GetPathnameForClass(const AClassname: string): string;
    function OpenWindowWithClass(const Directory, AClass: string): Boolean;
    procedure SetBreakpoints;
    function PreCompile(AForm: TFEditForm; Pathname: string): Boolean;
    procedure RunButtonToStop(Stop: Boolean);
    procedure CompileButtonToStop(OnOff: Boolean);
    function Open(const FileName: string): Boolean;
    function OpenEditForm(const Pathname: string; Hidden: Boolean): TFEditForm;
    procedure CloseFile(const FileName: string);
    procedure UpdateMenuItems(Sender: TObject); // MI = MenuItem
    procedure DoSave(AForm: TFForm; WithBackup: Boolean);
    procedure AddToWindowMenuAndTabBar(Num: Integer; NotifyEvent: TNotifyEvent;
      Form: TFForm);
    procedure SetSelectedTabAndWindow(Num: Integer); overload;
    procedure SetSelectedTabAndWindow(const Pathname: string); overload;
    procedure RenameTabAndWindow(Num: Integer; const New: string);
    procedure DeleteTabAndWindow(Num: Integer);
    procedure DeleteTab(Num: Integer);
    procedure TabModified(Num: Integer; Modified: Boolean);
    procedure MyTabBarColorize;
    procedure MyTabBarClick(Sender: TObject);
    function NewBrowser(Address: string; const State: string): TFBrowser;
    procedure NewEditor(const Path: string; State: string);
    procedure NewExplorer(const Dir: string; State: string);
    function MakeNewUMLWindow(const FileName, State: string): TFUMLForm;
    procedure SearchInIndex;
    function OpenUMLWindow(const FileName, State: string): TFUMLForm;
    procedure ConnectGUIAndJavaWindow(GUIForm: TFGUIForm);
    procedure MoveHSplitter(AHeight: Integer);
    procedure MoveVSplitter(AWidth: Integer);
    procedure SaveBounds;
    function GetFilename(const Extension: string; Path: string = ''): string;
    procedure RearrangeFileHistory(const NewFile: string);
    procedure OpenFiles;
    procedure ShowSearchReplaceDialog(Editor: TSynEdit; AReplace: Boolean);
    procedure DoSearchReplaceText(Editor: TSynEdit; AReplace: Boolean);
    procedure NotFound(Editor: TSynEdit; Backwards: Boolean); // Ex
    function IsAValidClass(const Pathname: string): Boolean;
    procedure CompileOneWith(const Pathname: string);
    procedure DoOpenInUMLWindow(const Pathname: string);
    procedure DoJarCreateEV3(const FileName: string);
    function PreparePrint(Sender: TObject): Boolean;
    function GetActiveTDIFormPath: string;
    function ExtentClasspath(Classpath: string): string;
    function GetAllPathnames: TStringList;
    function GetAllClassnames: TStringList;
    procedure Restart;
    procedure Run(const Pathname: string);
    function HasBreakpoints: Boolean;
    procedure StructogramFromText(Sourcecode, Pathname: string);
    procedure ShowCompileErrors;
    procedure ResetToolbars;
    procedure UpdateLayoutRightDockPanel(SetWidthHeight: Boolean = False);
    procedure ImperativeUpdateMenuItems;
    procedure DisableUpdateMenuItems;
    procedure EnableUpdateMenuItems;
    function GetActiveEditor: TFEditForm;
    function TDIEditFormCount: Integer;
    function TDIEditFormGet(Index: Integer): TFEditForm;
    procedure ChangeStyle(Style: string);
    procedure SetStyle(StyleName: string);
    function FormFactory(FormKind: TFormKind): TFForm;
    procedure DoExport(Pathname: string; Bitmap: TBitmap);
    procedure ChangeLanguage(LangCode: string);
    procedure SetDockTopPanel;
    procedure ShowAWTSwingOrFX(FrameType: Integer);
    procedure SetOptions;
    procedure ThemeEditorGutter(Gutter: TSynGutter);

    property ActiveTDIChild: TFForm read FActiveTDIChild write FActiveTDIChild;
    property ActiveTool: Integer read FActiveTool write FActiveTool;
    property AfterCodeCompletion: Boolean read FAfterCodeCompletion;
    property AUMLForm: TFUMLForm read FAUMLForm write FAUMLForm;
    property EditorForm: TFEditForm read FEditorForm write FEditorForm;
    property InteractiveUMLForm: TFUMLForm read FInteractiveUMLForm;
    property LastDoubleClickTime: TDateTime read FLastDoubleClickTime;
    property Lock: TCriticalSection read FLock;
    property MakeUpdate: Boolean read FMakeUpdate write FMakeUpdate;
    property MyTabBar: TJvTabBar read FMyTabBar;
    property PrintDialog: TPrintDialog read FPrintDialog;
    property ProjectFilename: string read FProjectFilename;
    property ScpJava: TSynCompletionProposal read FScpJava;
    property ScpParamIndex: Integer read FScpParamIndex;
    property ScpParams: TSynCompletionProposal read FScpParams;
    property Search: Boolean read FSearch write FSearch;
    property Searched: string read FSearched;
    property SynEditSearch: TSynEditSearch read FSynEditSearch;
    property TDIFormsList: TFormsList read FTDIFormsList;
  end;

var
  FJava: TFJava;

implementation

{$R *.DFM}

uses
  SysUtils,
  ShellAPI,
  Themes,
  StrUtils,
  Vcl.Imaging.pngimage,
  Math,
  Types,
  Printers,
  Menus,
  IOUtils,
  UITypes,
  MadExcept,
  SynEditTypes,
  UDlgAbout,
  UConfiguration,
  UDlgGotoLine,
  UObjectGenerator,
  UDlgParameter,
  UNTProcess,
  USaveDialog,
  UDlgHelp,
  UDlgGrepSearch,
  UDlgEvaluate,
  UWatches,
  UMessages,
  UUtils,
  UModel,
  UCodeCompletion,
  UScpHint,
  UWindow,
  UDlgUpdate,
  UObjectInspector,
  UGUIDesigner,
  UDlgClassEditor,
  UJavaCommands,
  URtfdDiagram,
  UDlgSearch,
  UDlgReplace,
  UClassInsert,
  UTextDiffForm,
  USubversion,
  UDlgUnicode,
  URegExSearch,
  USearchOptions,
  UComJava1,
  UDlgConfigureTools,
  UDlgMindstorms,
  UDlgMindstormsEV3,
  UConjoinHost,
  UTabHost,
  UDebugger,
  UTooltip,
  UFXGUIForm,
  UFileStructure,
  UDlgMessage,
  UTabObject,
  UGit,
  UJUnitTest,
  UExplorerForm,
  UTemplates,
  JvGnugettext,
  UStringRessources,
  ULLMChatForm;

{ --- TFJava ------------------------------------------------------------------- }

procedure TFJava.FormCreate(Sender: TObject);
var
  AColor: TColor;
  Details: TThemedElementDetails;
begin
  Application.ProcessMessages; // to show proper gui style
  FConfiguration := TFConfiguration.Create(Self);
  FGUIDesigner := TFGUIDesigner.Create(Self);
  FObjectGenerator := TFObjectGenerator.Create(Self);
  FWatches := TFWatches.Create(Self);
  FMessages := TFMessages.Create(Self);
  FWindow := TFWindow.Create(Self);
  FScpHint := TFScpHint.Create(Self);
  FEvaluate := TFEvaluate.Create(Self);
  FTooltip := TFTooltip.Create(Self);
  FLLMChatForm := TLLMChatForm.Create(Self);
  MyJavaCommands := TJavaCommands.Create;
  MyDebugger := TDebugger.Create;
  MySearchOptions := TSearchOptions.Create;
  TabDockHost := TTabDockHost.Create(Self);
  ConjoinDockHost := TConjoinDockHost.Create(Self);

  SetMenuKeyCaps;
  TranslateComponent(Self);
  FIdle := 0;
  FIdle2 := 0;
  FUpdateMenuItemsCount := 0;
  FActiveTDIChild := nil;
  FSynEditSearch := TSynEditSearch.Create(Self);
  FScpJava := TSynCompletionProposal.Create(Self);
  with FScpJava do
  begin
    DefaultType := ctCode;
    Options := [scoLimitToMatchedText, scoTitleIsCentered, scoUseInsertList,
      scoUsePrettyText, scoUseBuiltInTimer, scoCompleteWithEnter,
      scoConsiderWordBreakChars];
    TriggerChars := '.';
    EndOfTokenChr := '()[]. ';
    ShortCut := Menus.ShortCut(Word(' '), [ssCtrl]);
    TimerInterval := 0;
    OnChange := scpJavaChange;
    OnClose := scpJavaClose;
    OnExecute := scpJavaExecute;
    OnShow := scpJavaShow;
    OnCodeCompletion := scpJavaOnCodeCompletion;
    OnAfterCodeCompletion := scpJavaAfterCodeCompletion;
    // Images:= DMImages.ILFileStructure;      // make an exception
  end;
  FScpParams := TSynCompletionProposal.Create(Self);
  with FScpParams do
  begin
    DefaultType := ctParams;
    Options := [scoLimitToMatchedText, scoUseBuiltInTimer, scoEndCharCompletion,
      scoCompleteWithTab, scoCompleteWithEnter];
    TriggerChars := '(';
    ShortCut := Menus.ShortCut(Word(' '), [ssShift, ssCtrl]);
    TimerInterval := 1;
    OnExecute := scpParamsExecute;
    OnPaintItem := scpParamsPaintItem;
  end;
  FMyTabBar := TJvTabBar.Create(Self);
  FMyTabBar.Parent := Self;
  FMyTabBar.Top := 200;
  FMyTabBar.Width := 1600;
  FMyTabBar.AllowTabMoving := True;
  FMyTabBar.HotTracking := True;
  FMyTabBar.SelectBeforeClose := False;
  FMyTabBar.OnClick := MyTabBarClick;
  FMyTabBar.OnTabClosed := MyTabBarClosed;
  FMyTabBar.OnDblClick := MyTabBarDblClick;

  FMyTabBar.Painter := TJvModernTabBarPainter.Create(Self);
  TJvModernTabBarPainter(FMyTabBar.Painter).MoveDividerColor := clRed;
  TJvModernTabBarPainter(FMyTabBar.Painter).SelectedFont.Color := clBlack;
  if StyleServices.IsSystemStyle then
    TJvModernTabBarPainter(FMyTabBar.Painter).Color := clBtnFace
    // sets backgound of tabs
  else
  begin
    Details := StyleServices.GetElementDetails(tbsBackground);
    StyleServices.GetElementColor(Details, ecFillColor, AColor);
    TJvModernTabBarPainter(FMyTabBar.Painter).Color := AColor;
  end;
  MIRedo.ShortCut := ShortCut(Word('Z'), [ssShift, ssCtrl]);
  MIUnindent.ShortCut := ShortCut(Word('U'), [ssShift, ssCtrl]);
  MIIndent.ShortCut := ShortCut(Word('I'), [ssShift, ssCtrl]);
  MIProgramReset.Enabled := False;
  FTDIFormsList := TFormsList.Create(False);
  TabsControl.ActivePage := TSProgram;
  FTimerStatus := -1;
  Randomize;
  FLastDoubleClickTime := Time;
  Application.OnIdle := AppOnIdle;
  Screen.HintFont.Size := 10;
  FLock := TCriticalSection.Create;
  DragAcceptFiles(Self.Handle, True);
  UseLatestCommonDialogs := True;
  FInteractiveUMLForm := nil;
  ReadHistory;
  Screen.MenuFont.Name := 'Segoe UI';
  Screen.MenuFont.Size := 9;
  FMakeUpdate := False;
  FConfiguration.Init;
  LoadBounds;
  TThread.ForceQueue(nil, OpenFiles);
end;

procedure TFJava.OpenFileStructureAndObjectInspector;
begin
  FFileStructure := TFFileStructure.Create(Self);
  FFileStructure.Visible := FConfiguration.ReadBoolU('FileStructure',
    'Visible', True);
  FFileStructure.Height := 300;
  FFileStructure.Width := 200;
  FObjectInspector := TFObjectInspector.Create(Self);
  FObjectInspector.Visible := FConfiguration.ReadBoolU('ObjectInspector',
    'Visible', True);
  MyCodeCompletion := TCodeCompletion.Create;
  if FFileStructure.Visible or FObjectInspector.Visible or Assigned(FJUnitTests)
    and FJUnitTests.Visible then
    LoadDocking
  else
  begin
    RightDockPanel.Width := 0;
    VSplitter.Visible := False;
  end;
end;

procedure TFJava.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MySearchOptions);
  FreeAndNil(MyRegExSearch);
  FreeAndNil(MyCodeCompletion);
  FreeAndNil(MyDebugger);
  FreeAndNil(MyJavaCommands);
  FreeAndNil(FClassInsert);
  FreeAndNil(FSynEditSearch);
  FreeAndNil(FScpJava);
  FreeAndNil(FScpParams);
  // FreeAndNil(FScpHint); runtime error
  FreeAndNil(FMyTabBar);
  FreeAndNil(FLock);
  FreeAndNil(FTDIFormsList);
  FreeAndNil(FJUnitTests);
  AllClassesPackageClose;
end;

procedure TFJava.MINewClick(Sender: TObject);
begin
  var
  Str := '';
  var
  Editform := TFEditForm(FormFactory(fkEditor));
  if (Sender is TToolButton) or (Sender is TSpeedButton) then
    Str := '.java'
  else
    case (Sender as TSpTBXItem).Tag of
      1:
        Str := '.java';
      2:
        Str := '.txt';
      3:
        Str := '.html';
    end;
  Editform.New(GetFilename(Str));
end;

procedure TFJava.MINewCommentClick(Sender: TObject);
begin
  if FActiveTDIChild.FormTag = 2 then
    (FActiveTDIChild as TFUMLForm).TBCommentClick(Self);
end;

procedure TFJava.SBNewClick(Sender: TObject);
begin
  MINewClick(Sender);
end;

procedure TFJava.SBClassClick(Sender: TObject); // new Class
begin
  DisableUpdateMenuItems;
  var
  Editform := NewEditform(True);
  if Assigned(Editform) then
  begin
    LockWindow(FJava.Handle);
    FTemplates.SBNewClass(Editform);
    var
    UMLForm := GetActiveUML;
    if (FMyTabBar.Tabs.Count = 0) or not Assigned(UMLForm) then
      FAUMLForm := MakeNewUMLWindow(ChangeFileExt(Editform.Pathname,
        '.uml'), '')
    else
    begin
      FAUMLForm := UMLForm;
      FAUMLForm.MainModul.UnSelectAllElements;
    end;
    FAUMLForm.ConfigureWindow(Self);
    PrepareClassEdit(Editform.Pathname, 'New', FAUMLForm);
    UnlockWindow;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIOpenClick(Sender: TObject);
var
  Path: string;

  procedure InitDir;
  begin
    with ODOpen do
    begin
      InitialDir := FConfiguration.Sourcepath;
      if not SysUtils.DirectoryExists(InitialDir) then
        InitialDir := GetDocumentsPath;
    end;
  end;

begin
  // http://www.installationexcellence.com/articles/VistaWithDelphi/Original/Index.html
  DisableUpdateMenuItems;
  with ODOpen do
  begin
    if Assigned(FActiveTDIChild) then
    begin
      Path := FActiveTDIChild.Pathname;
      InitialDir := ExtractFilePath(Path);
      if InitialDir + '\' = FConfiguration.TempDir then
        InitDir;
    end
    else
      InitDir;
    FileName := '';
    FilterIndex := 1;
    Title := _(LNGOpenFile);
    if Execute then
    begin
      FConfiguration.Sourcepath := ExtractFilePath(FileName);
      if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 4) then
        if Files.Count >= 2 then
          (FActiveTDIChild as TFTextDiff).Open(Files[0], Files[1])
        else
          (FActiveTDIChild as TFTextDiff).Open(FileName)
        else
          try
            Screen.Cursor := crHourGlass;
            LockWindow(FJava.Handle);
            for var I := 0 to Files.Count - 1 do
              if Open(Files[I]) then
                RearrangeFileHistory(Files[I]);
          finally
            UnlockWindow;
            Screen.Cursor := crDefault;
          end;
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIMenuOpenClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var
  Str := (Sender as TSpTBXItem).Caption;
  try
    Screen.Cursor := crHourGlass;
    LockWindow(FJava.Handle);
    Str := Right(Str, Pos(' ', Str) + 1);
    Str := ReplaceStr(Str, '&', ''); // temporarily
    if Open(Str) then
      RearrangeFileHistory(Str);
    FConfiguration.Sourcepath := ExtractFilePath(Str);
  finally
    UnlockWindow;
    Screen.Cursor := crDefault;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MICloseClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if Assigned(FActiveTDIChild) then
    FActiveTDIChild.Close;
  EnableUpdateMenuItems;
end;

procedure TFJava.MICloseProjectClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  MICloseAllFilesClick(Self);
  EnableUpdateMenuItems;
end;

procedure TFJava.MICloseAllFilesClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  try
    Screen.Cursor := crHourGlass;
    LockFormUpdate(Self);
    if FProjectFilename <> '' then
    begin
      DoSaveAsProject(FProjectFilename);
      RearrangeFileHistory(FProjectFilename);
      FConfiguration.RestoreConfigurationAfterProject;
    end;
    for var I := FTDIFormsList.Count - 1 downto 0 do
    begin
      Application.ProcessMessages;
      FTDIFormsList[I].Close;
    end;
    Application.ProcessMessages;
    FProjectFilename := '';
    FMessages.StatusMessage('');
  finally
    UnlockFormUpdate(Self);
    Screen.Cursor := crDefault;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MISaveClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
    DoSave(FActiveTDIChild, FConfiguration.CreateBAKFiles);
end;

procedure TFJava.FixWindows(WithHidden: Boolean);
begin
  if WithHidden then
  begin
    SetLength(FFixedWindows, FTDIFormsList.Count);
    for var I := 0 to FTDIFormsList.Count - 1 do
      FFixedWindows[I] := FTDIFormsList[I];
  end
  else
  begin
    SetLength(FFixedWindows, FMyTabBar.Tabs.Count);
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
      FFixedWindows[I] := GetTabForm(I);
  end;
end;

procedure TFJava.MISaveAllInClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var
  Dir := FConfiguration.Sourcepath;
  if not SysUtils.DirectoryExists(Dir) then
    if Assigned(FEditorForm) then
      Dir := ExtractFilePath(FEditorForm.Pathname)
    else
      Dir := '';
  FConfiguration.FolderDialog.DefaultFolder := Dir;
  if FConfiguration.FolderDialog.Execute then
  begin
    FixWindows(False);
    FConfiguration.Sourcepath := FConfiguration.FolderDialog.FileName;
    for var I := 0 to Length(FFixedWindows) - 1 do
      FFixedWindows[I].SaveIn(WithTrailingSlash(FConfiguration.Sourcepath));
  end;
  if CanFocus then
    SetFocus;
  EnableUpdateMenuItems;
end;

procedure TFJava.MISaveAllClick(Sender: TObject);
begin
  FixWindows(True);
  for var I := 0 to Length(FFixedWindows) - 1 do
    if FFixedWindows[I].Modified then
      DoSave(FFixedWindows[I], FConfiguration.CreateBAKFiles);
end;

procedure TFJava.DoSave(AForm: TFForm; WithBackup: Boolean);
begin
  if not Assigned(AForm) then
    Exit;
  if (AForm.FormTag = 1) and (AForm.IsDefaultFilename or
    (AForm.Pathname <> AForm.GetSaveAsName)) then
    EditorSaveAs(AForm, False)
  else if (AForm.FormTag = 2) and AForm.IsDefaultFilename then
    UMLSaveAs(AForm as TFUMLForm)
  else if (AForm.FormTag = 11) and AForm.IsDefaultFilename then
    StructogramSaveAs(AForm as TFStructogram)
  else if (AForm.FormTag = 14) and AForm.IsDefaultFilename then
    SequenceSaveAs(AForm as TFSequenceForm)
  else
    AForm.Save(WithBackup);
end;

procedure TFJava.MISaveAsClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if Assigned(FActiveTDIChild) then
    case FActiveTDIChild.FormTag of
      1:
        EditorSaveAs(FActiveTDIChild, False);
      2:
        UMLSaveAs(FActiveTDIChild as TFUMLForm);
      11:
        StructogramSaveAs(FActiveTDIChild as TFStructogram);
      14:
        SequenceSaveAs(FActiveTDIChild as TFSequenceForm);
    end;
  EnableUpdateMenuItems;
end;

procedure TFJava.EditorSaveAs(AForm: TFForm; Hidden: Boolean);
var
  Dir, OldPartner, NewPartner, Ext, SaveAsName, FPath, FName: string;
  Form2: TFForm;
  Editform: TFEditForm;
  GUIForm: TFGUIForm;
  SaveDialog: TExtSaveDialog;
  OpenGuiForm: Boolean;
begin
  Editform := AForm as TFEditForm;
  if Editform.AlreadySavedAs then
    Exit;
  if Hidden then
    Editform.Hide
  else
    SwitchToWindow(Editform.Number);
  SaveDialog := TExtSaveDialog.CreateWith(Self, Editform.Encoding + '/' +
    Editform.LinebreakAsString, _('Encoding') + ':');
  try
    with SaveDialog do
    begin
      Title := _(LNGSaveAs);
      Filter := 'Java (*.java)|*.java|HTML (*.html)|*.html;*.htm|Text (*.txt)|*.txt|'
        + _(LNGAll) + ' (*.*)|*.*';
      Ext := '.java';
      SaveAsName := Editform.GetSaveAsName;
      FileName := SaveAsName;
      FilterIndex := GetFilterIndex(Filter, FileName);
      Dir := ExtractFilePath(FileName);
      if Dir <> '' then
        InitialDir := Dir
      else
        InitialDir := FConfiguration.Sourcepath;
      if Execute then
      begin
        FPath := ExtractFilePath(FileName);
        FName := ExtractFileName(FileName);
        FName := WithoutSpaces(FName);
        FileName := FPath + FName;
        FileName := AddFileExt(FileName, Filter, Ext, FilterIndex);
        if SysUtils.SameText(FileName, SaveAsName) then // sensitive
          FileName := SaveAsName;

        Form2 := GetTDIWindowType(FileName, '%E%');
        if Assigned(Form2) and (Form2 <> Editform) and not Form2.Visible then
        begin
          Form2.Close;
          Form2 := nil;
        end;
        if (Length(FName) > 0) and
          (IsWordBreakChar(FName[1]) or IsDigit(FName[1])) then
          ErrorMsg(_('Invalid class name') + ': ' + ChangeFileExt(FName, ''))
        else if not Editform.ClassnameDifferentFromAncestors
          (ChangeFileExt(FName, '')) then
          ErrorMsg(_('Invalid class name') + ': ' + ChangeFileExt(FName, ''))
        else if Assigned(Form2) and (Form2 <> Editform) then
          InformationMsg(Format(_(LNGFileAlreadyOpen), [FileName]))
        else if IsWriteProtected(FileName) then
          ErrorMsg(FileName + ' ' + _(LNGWriteProtected))
        else if IsWriteProtected(ChangeFileExt(FileName, '.jfm')) then
          ErrorMsg(ChangeFileExt(FileName, '.jfm') + ' ' + _(LNGWriteProtected))
        else if not FileExists(FileName) or (Editform.Pathname = FileName) or
          FileExists(FileName) and
          (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]),
          mtConfirmation, mbYesNoCancel, 0) = mrYes) then
        begin
          Editform.SetEncoding(Encoding);
          OldPartner := ChangeFileExt(Editform.Pathname, '.jfm');
          NewPartner := ChangeFileExt(FileName, '.jfm');
          GUIForm := GetGuiForm(OldPartner);
          OpenGuiForm := Assigned(GUIForm);
          if Assigned(GUIForm) then
          begin
            GUIForm.SaveAs(NewPartner);
            GUIForm.Close;
            FreeAndNil(GUIForm);
          end
          else if FileExists(OldPartner) and
            (ExtractFileExt(OldPartner) = '.jfm') then
          begin
            var
            StringList := TStringList.Create;
            try
              try
                StringList.LoadFromFile(OldPartner);
                StringList.SaveToFile(NewPartner);
              except
                on E: Exception do
                  ErrorMsg(Format(_(LNGCanNotCreateFile),
                    [NewPartner, E.Message]));
              end;
            finally
              StringList.Free;
            end;
          end;
          Editform.SaveAs(FileName);
          if OpenGuiForm then
            Open(NewPartner);
          FConfiguration.Sourcepath := ExtractFilePath(FileName);
          RearrangeFileHistory(FileName);
        end;
      end;
    end;
  finally
    FreeAndNil(SaveDialog);
  end;
end;

procedure TFJava.UMLSaveAs(UMLForm: TFUMLForm);
begin
  with SDSaveAs do
  begin
    Title := _(LNGSaveAs);
    Filter := 'UML (*.uml)|*.uml|' + _(LNGAll) + ' (*.*)|*.*';
    FileName := UMLForm.GetSaveAsName;
    // if Filename = '' then exit; // not save on cancel
    FilterIndex := 1;
    var
    Path := ExtractFilePath(FileName);
    if Path <> '' then
      InitialDir := Path
    else
      InitialDir := FConfiguration.Sourcepath;
    if Execute then
    begin
      if ExtractFileExt(FileName) = '' then
        FileName := FileName + '.uml';
      if Assigned(GetTDIWindowType(FileName, '%U%')) then
        InformationMsg(Format(_(LNGFileAlreadyOpen), [FileName]))
      else if not FileExists(FileName) or FileExists(FileName) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
        mbYesNoCancel, 0) = mrYes) then
      begin
        FConfiguration.Sourcepath := ExtractFilePath(FileName);
        FMessages.RenameInteractive(UMLForm.Pathname, FileName);
        UMLForm.Pathname := FileName;
        UMLForm.Save(True);
        RenameTabAndWindow(UMLForm.Number, FileName);
        RearrangeFileHistory(FileName);
        UMLForm.Caption := FileName;
      end;
    end;
  end;
end;

procedure TFJava.StructogramSaveAs(Structogram: TFStructogram);
begin
  with SDSaveAs do
  begin
    Title := _(LNGSaveAs);
    Filter := _(LNGStructogram) + ' (*.jsg)|*.jsg|' + _(LNGAll) + ' (*.*)|*.*';
    FileName := Structogram.GetSaveAsName;
    FilterIndex := 1;
    var
    Path := ExtractFilePath(FileName);
    if Path <> '' then
      InitialDir := Path
    else
      InitialDir := FConfiguration.Sourcepath;
    if Execute then
    begin
      if ExtractFileExt(FileName) = '' then
        FileName := FileName + '.jsg';
      if Assigned(GetTDIWindowType(FileName, '%S%')) then
        InformationMsg(Format(_(LNGFileAlreadyOpen), [FileName]))
      else if not FileExists(FileName) or FileExists(FileName) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
        mbYesNoCancel, 0) = mrYes) then
      begin
        FConfiguration.Sourcepath := ExtractFilePath(FileName);
        FMessages.RenameInteractive(Structogram.Pathname, FileName);
        Structogram.Pathname := FileName;
        Structogram.Save(True);
        RenameTabAndWindow(Structogram.Number, FileName);
        RearrangeFileHistory(FileName);
        Structogram.Caption := FileName;
      end;
    end;
  end;
end;

procedure TFJava.SequenceSaveAs(SequenceDiagram: TFSequenceForm);
begin
  with SDSaveAs do
  begin
    Title := _(LNGSaveAs);
    Filter := _(LNGSequenceDiagram) + ' (*.jsd)|*.jsd|' + _(LNGAll) +
      ' (*.*)|*.*';
    if SequenceDiagram.Pathname <> '' then
      FileName := SequenceDiagram.Pathname
    else
      FileName := SequenceDiagram.GetSaveAsName;
    FilterIndex := 1;
    var
    Path := ExtractFilePath(FileName);
    if Path <> '' then
      InitialDir := Path
    else
      InitialDir := FConfiguration.Sourcepath;
    if Execute then
    begin
      if ExtractFileExt(FileName) = '' then
        FileName := FileName + '.jsd';
      if Assigned(GetTDIWindowType(FileName, '%Q%')) then
        InformationMsg(Format(_(LNGFileAlreadyOpen), [FileName]))
      else if not FileExists(FileName) or FileExists(FileName) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
        mbYesNoCancel, 0) = mrYes) then
      begin
        FConfiguration.Sourcepath := ExtractFilePath(FileName);
        FMessages.RenameInteractive(SequenceDiagram.Pathname, FileName);
        SequenceDiagram.Pathname := FileName;
        SequenceDiagram.Save(True);
        RenameTabAndWindow(SequenceDiagram.Number, FileName);
        RearrangeFileHistory(FileName);
        SequenceDiagram.Caption := FileName;
      end;
    end;
  end;
end;

procedure TFJava.MIExportClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if Assigned(FActiveTDIChild) then
    FActiveTDIChild.DoExport;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIExitClick(Sender: TObject);
begin
  DefaultInstance.DebugLogToFile('C:\temp\log.txt');
  Close;
end;

var
  Counter: Integer = 0;

procedure TFJava.FormAfterMonitorDpiChanged(Sender: TObject;
  OldDPI, NewDPI: Integer);
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      Invalidate;
      for var I := FTDIFormsList.Count - 1 downto 0 do
        FTDIFormsList[I].DPIChanged;
      FMessages.DPIChanged;
      TJvModernTabBarPainter(FMyTabBar.Painter).Font.Size := Font.Size;
      TJvModernTabBarPainter(FMyTabBar.Painter).SelectedFont.Size := Font.Size;
      FMyTabBar.Invalidate;
    end);
end;

procedure TFJava.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// var freq, startTime, endTime: Int64;
begin
  Inc(Counter);
  DisableUpdateMenuItems;
  if (MyDebugger.Running or MyJavaCommands.ProcessRunning) and (Counter < 2)
  then
  begin
    MIProgramResetClick(Self);
    CanClose := False;
    CloseTimer.Enabled := True;
    Exit;
  end;

  DisableUpdateMenuItems;
  CanClose := True;
  FClosing := True;
  Screen.OnActiveFormChange := nil;
  try
    for var I := FTDIFormsList.Count - 1 downto 0 do
      if FTDIFormsList[I].FormTag = 4 then
        FTDIFormsList[I].Close; // exception
    if Assigned(FInteractiveUMLForm) then
      FInteractiveUMLForm.Close;
  except
    on E: Exception do
      OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' + E.Message));
  end;

  Application.ProcessMessages;
  FConfiguration.SaveWindow;
  if FProjectFilename <> '' then
  begin
    if FMyTabBar.Tabs.Count > 0 then
      DoSaveAsProject(FProjectFilename);
    FProjectFilename := '';
    FMessages.StatusMessage('');
    FConfiguration.RestoreConfigurationAfterProject;
  end;

  MISaveAllClick(Self);
  for var I := FTDIFormsList.Count - 1 downto 0 do
    FTDIFormsList[I].Close; // exception

  FMessages.SaveWindow;
  FWatches.SaveWindow;
  FFileStructure.SaveWindow;
  FObjectInspector.SaveWindow;
  SaveDocking;
  FObjectGenerator.SaveWindow;

  if Assigned(FClassInsert) then
    FClassInsert.SaveWindow;
  if Assigned(FJUnitTests) then
    FJUnitTests.Close;

  // QueryPerformanceFrequency(freq);
  // QueryPerformanceCounter(startTime);
  if not FMakeUpdate then
    FConfiguration.ModelToRegistry; // needs about 1,5 seconds under windows 32
  // QueryPerformanceCounter(endTime);
  // ErrorMsg(IntToStr((endTime - startTime) * 1000 div freq) + 'ms');
end;

procedure TFJava.MIUndoClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.Undo
  else if Assigned(FActiveTDIChild) then
    FActiveTDIChild.Undo;
end;

procedure TFJava.MIRecognizeAssociationsClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) and (FActiveTDIChild is TFUMLForm) then
    (FActiveTDIChild as TFUMLForm).TBRecognizeAssociationsClick(Self);
end;

procedure TFJava.MIRedoClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.Redo
  else if Assigned(FActiveTDIChild) then
    FActiveTDIChild.Redo;
end;

procedure TFJava.MIPasteClick(Sender: TObject);
begin
  if FObjectInspector.Active then
    FObjectInspector.PasteFromClipboard
  else if GetActiveFormTag = 7 then
    FMessages.PasteFromClipboard
  else if Assigned(FActiveTDIChild) then
    FActiveTDIChild.PasteFromClipboard;
end;

procedure TFJava.MICopyNormalClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.CopyToClipboard
  else if Assigned(FActiveTDIChild) then
    FActiveTDIChild.CopyToClipboard;
end;

procedure TFJava.MICutClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.CutToClipboard
  else if Assigned(FActiveTDIChild) then
    FActiveTDIChild.CutToClipboard;
end;

procedure TFJava.MICopyRTFClick(Sender: TObject);
begin
  FEditorForm.ExportToClipboard(False, False);
end;

procedure TFJava.MICopyRtfNumberedClick(Sender: TObject);
begin
  FEditorForm.ExportRTFNumbered;
end;

procedure TFJava.MICopyHTMLClick(Sender: TObject);
begin
  FEditorForm.ExportToClipboard(True, False);
end;

procedure TFJava.MICopyHTMLAsTextClick(Sender: TObject);
begin
  FEditorForm.ExportToClipboard(True, True);
end;

procedure TFJava.MICopyNumberedClick(Sender: TObject);
begin
  FEditorForm.ExportWithNumbers;
end;

procedure TFJava.MIGitClick(Sender: TObject);
begin
  FGit.Execute(TSpTBXItem(Sender).Tag, GetActiveEditor);
end;

procedure TFJava.MIGotoLineClick(Sender: TObject);
begin
  with TFGotoLineDialog.Create(Self) do
  begin
    if (ShowModal = mrOk) and (Line > 0) and Assigned(FEditorForm) then
      FEditorForm.GotoLine(Line);
    Free;
  end;
end;

procedure TFJava.MIUnicodeClick(Sender: TObject);
begin
  with TFUnicodeDialog.Create(Self) do
  begin
    CBUnicode.Text := '';
    if ShowModal = mrOk then
      FEditorForm.PutText(UnicodeChar + '|');
    Free;
  end;
end;

procedure TFJava.MIUnindentClick(Sender: TObject);
begin
  FEditorForm.Unindent;
end;

procedure TFJava.MIIndentClick(Sender: TObject);
begin
  FEditorForm.Indent;
end;

procedure TFJava.MISystemOutPrintlnClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.SystemOutPrintln
  else
    FEditorForm.SystemOutPrintln;
end;

procedure TFJava.MISearchClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
    FActiveTDIChild.Search;
end;

procedure TFJava.MISearchAgainClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
    FActiveTDIChild.SearchAgain;
end;

procedure TFJava.MIReplaceClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
    FActiveTDIChild.Replace;
end;

// FSearch and replace is needed for Notepad and TextDiff

procedure TFJava.ShowSearchReplaceDialog(Editor: TSynEdit; AReplace: Boolean);
begin
  if AReplace then
    with TFReplace.Create(Self) do
    begin
      ShowReplaceDialog(Editor);
      Free;
    end
  else
    with TFSearch.Create(Self) do
    begin
      ShowSearchDialog(Editor);
      Free;
    end;
end;

procedure TFJava.MISearchInFilesClick(Sender: TObject);
begin
  with TFGrepSearch.Create(FJava) do
  begin
    ShowSearchDialog(FEditorForm);
    Free;
  end;
end;

procedure TFJava.DoSearchReplaceText(Editor: TSynEdit; AReplace: Boolean);
var
  Options: TSynSearchOptions;
begin
  with MySearchOptions do
    if RegEx then
      try
        MyRegExSearch.DoRegSearchReplace(AReplace);
      except
        on E: Exception do
          ErrorMsg(E.Message);
      end
    else
    begin
      if AReplace then
        Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
      else
        Options := [];
      if CaseSensitive then
        Include(Options, ssoMatchCase);
      if WholeWords then
        Include(Options, ssoWholeWord);
      if not SystemFromCursor then
        Include(Options, ssoEntireScope);
      if SelectionOnly then
        Include(Options, ssoSelectedOnly);
      if Backwards then
        Include(Options, ssoBackwards);
      if Editor.SearchReplace(SearchText, ReplaceText, Options) = 0 then
        NotFound(Editor, ssoBackwards in Options);
    end;
end;

procedure TFJava.NotFound(Editor: TSynEdit; Backwards: Boolean); // Ex
begin
  if Backwards then
    Editor.BlockEnd := Editor.BlockBegin
  else
    Editor.BlockBegin := Editor.BlockEnd;
  Editor.CaretXY := Editor.BlockBegin;
  InformationMsg(Format(_(LNGSearchTextNotFound),
    [MySearchOptions.SearchText]));
end;

procedure TFJava.MIMaximizedClick(Sender: TObject);
begin
  FConfiguration.WindowStateMaximized := True;
  for var I := 0 to FMyTabBar.Tabs.Count - 1 do
  begin
    var
    AForm := GetTabForm(I);
    if Assigned(AForm) then
    begin
      AForm.Align := alClient;
      AForm.BorderStyle := bsNone;
      AForm.BorderIcons := [];
      AForm.WindowState := wsMaximized;
    end;
  end;
end;

procedure TFJava.MICascadeClick(Sender: TObject);
var
  Dist, Border, Titlebar, AWidth, AHeight: Integer;
  AForm: TFForm;
begin
  FConfiguration.WindowStateMaximized := False;
  Dist := 0;
  Titlebar := 0;
  AWidth := MainPanel.Width * 3 div 4;
  AHeight := MainPanel.Height * 2 div 3;
  for var I := 0 to FMyTabBar.Tabs.Count - 1 do
  begin
    AForm := GetTabForm(I);
    if Assigned(AForm) then
    begin
      AForm.Align := alNone;
      AForm.WindowState := wsNormal;
      AForm.BorderStyle := bsSizeable;
      AForm.BorderIcons := [biSystemMenu, biMaximize];
    end;
    if Titlebar = 0 then
    begin
      Border := Max((AForm.Width - AForm.ClientWidth) div 2, 8);
      Titlebar := Max(AForm.Height - AForm.ClientHeight - Border, 30);
    end;
    AForm.SetBounds(Dist, Dist, AWidth, AHeight);
    Dist := Dist + Titlebar;
  end;
end;

procedure TFJava.Tile(Horizontal: Boolean);
var
  Count, Num, AWidth, AHeight, PartX, PartY: Integer;
  Rectangles: array of TRect;
  AForm: TFForm;
begin
  FConfiguration.WindowStateMaximized := False;
  Count := 0;
  if FMyTabBar.Tabs.Count > 0 then
  begin
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
    begin
      AForm := GetTabForm(I);
      if Assigned(AForm) then
      begin
        AForm.Align := alNone;
        AForm.WindowState := wsNormal;
        AForm.BorderStyle := bsSizeable;
        AForm.BorderIcons := [biSystemMenu, biMaximize];
        Inc(Count);
      end;
    end;

    AWidth := MainPanel.Width;
    AHeight := MainPanel.Height;
    if Count > 9 then
      Count := 9;
    SetLength(Rectangles, Count);
    case Count of
      1:
        Rectangles[0] := Rect(0, 0, AWidth, AHeight);
      2:
        if Horizontal then
        begin
          Rectangles[0] := Rect(0, 0, AWidth, AHeight div 2);
          Rectangles[1] := Rect(0, AHeight div 2, AWidth, AHeight);
        end
        else
        begin
          Rectangles[0] := Rect(0, 0, AWidth div 2, AHeight);
          Rectangles[1] := Rect(AWidth div 2, 0, AWidth, AHeight);
        end;
      3:
        if Horizontal then
        begin
          PartY := AHeight div 3;
          Rectangles[0] := Rect(0, 0, AWidth, PartY);
          Rectangles[1] := Rect(0, PartY, AWidth, 2 * PartY);
          Rectangles[2] := Rect(0, 2 * PartY, AWidth, AHeight);
        end
        else
        begin
          PartX := AWidth div 3;
          Rectangles[0] := Rect(0, 0, PartX, AHeight);
          Rectangles[1] := Rect(PartX, 0, 2 * PartX, AHeight);
          Rectangles[2] := Rect(2 * PartX, 0, AWidth, AHeight);
        end;
      4:
        begin
          PartX := AWidth div 2;
          PartY := AHeight div 2;
          Rectangles[0] := Rect(0, 0, PartX, PartY);
          Rectangles[1] := Rect(PartX, 0, AWidth, PartY);
          Rectangles[2] := Rect(0, PartY, PartX, AHeight);
          Rectangles[3] := Rect(PartX, PartY, AWidth, AHeight);
        end;
      5:
        begin
          PartX := AWidth div 2;
          PartY := AHeight div 2;
          Rectangles[0] := Rect(0, 0, PartX, PartY);
          Rectangles[1] := Rect(0, PartY, PartX, AHeight);
          PartY := AHeight div 3;
          Rectangles[2] := Rect(PartX, 0, AWidth, PartY);
          Rectangles[3] := Rect(PartX, PartY, AWidth, 2 * PartY);
          Rectangles[4] := Rect(PartX, 2 * PartY, AWidth, AHeight);
        end;
      6:
        begin
          PartX := AWidth div 2;
          PartY := AHeight div 3;
          Rectangles[0] := Rect(0, 0, PartX, PartY);
          Rectangles[1] := Rect(0, PartY, PartX, 2 * PartY);
          Rectangles[2] := Rect(0, 2 * PartY, PartX, AHeight);

          Rectangles[3] := Rect(PartX, 0, AWidth, PartY);
          Rectangles[4] := Rect(PartX, PartY, AWidth, 2 * PartY);
          Rectangles[5] := Rect(PartX, 2 * PartY, AWidth, AHeight);
        end;
      7:
        begin
          PartX := AWidth div 3;
          PartY := AHeight div 2;
          Rectangles[0] := Rect(0, 0, PartX, PartY);
          Rectangles[1] := Rect(0, PartY, PartX, AHeight);
          Rectangles[2] := Rect(PartX, 0, 2 * PartX, PartY);
          Rectangles[3] := Rect(PartX, PartY, 2 * PartX, AHeight);
          PartY := AHeight div 3;
          Rectangles[4] := Rect(2 * PartX, 0, AWidth, PartY);
          Rectangles[5] := Rect(2 * PartX, PartY, AWidth, 2 * PartY);
          Rectangles[6] := Rect(2 * PartX, 2 * PartY, AWidth, AHeight);
        end;
      8:
        begin
          PartX := AWidth div 4;
          PartY := AHeight div 2;
          Rectangles[0] := Rect(0, 0, PartX, PartY);
          Rectangles[1] := Rect(PartX, 0, 2 * PartX, PartY);
          Rectangles[2] := Rect(2 * PartX, 0, 3 * PartX, PartY);
          Rectangles[3] := Rect(3 * PartX, 0, AWidth, PartY);

          Rectangles[4] := Rect(0, PartY, PartX, AHeight);
          Rectangles[5] := Rect(PartX, PartY, 2 * PartX, AHeight);
          Rectangles[6] := Rect(2 * PartX, PartY, 3 * PartX, AHeight);
          Rectangles[7] := Rect(3 * PartX, PartY, AWidth, AHeight);
        end;
      9:
        begin
          PartX := AWidth div 3;
          PartY := AHeight div 3;
          Rectangles[0] := Rect(0, 0, PartX, PartY);
          Rectangles[1] := Rect(0, PartY, PartX, 2 * PartY);
          Rectangles[2] := Rect(0, 2 * PartY, PartX, AHeight);
          Rectangles[3] := Rect(PartX, 0, 2 * PartX, PartY);
          Rectangles[4] := Rect(PartX, PartY, 2 * PartX, 2 * PartY);
          Rectangles[5] := Rect(PartX, 2 * PartY, 2 * PartX, AHeight);
          Rectangles[6] := Rect(2 * PartX, 0, AWidth, PartY);
          Rectangles[7] := Rect(2 * PartX, PartY, AWidth, 2 * PartY);
          Rectangles[8] := Rect(2 * PartX, 2 * PartY, AWidth, AHeight);
        end;
    end;
  end;
  Num := -1;
  for var I := 0 to Count - 1 do
  begin
    AForm := GetTabForm(I);
    if Assigned(AForm) then
    begin
      Inc(Num);
      AForm.SetBounds(Rectangles[Num].Left, Rectangles[Num].Top,
        Rectangles[Num].Width, Rectangles[Num].Height);
      if Num = Count then
        Break;
    end;
  end;
end;

procedure TFJava.MITileVerticalClick(Sender: TObject);
begin
  Tile(False);
end;

procedure TFJava.MITileHorizontalClick(Sender: TObject);
begin
  Tile(True);
end;

procedure TFJava.MITestCreateSequencediagramClick(Sender: TObject);
var
  SequenceForm: TFSequenceForm;
  Editform: TFEditForm;
  FileName: string;
begin
  DisableUpdateMenuItems;
  Editform := GetActiveEditor;
  if Assigned(Editform) then
  begin
    FileName := ChangeFileExt(Editform.Pathname, '.jsd');
    SequenceForm := GetTDIWindowType(FileName, '%Q%') as TFSequenceForm;
    if Assigned(SequenceForm) then
      MyDebugger.SequenceForm := SequenceForm
    else if not FileExists(FileName) or FileExists(FileName) and
      (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
      mbYesNoCancel, 0) = mrYes) then
    begin
      SequenceForm := TFSequenceForm(FormFactory(fkSequence));
      SequenceForm.New(FileName);
      MyDebugger.SequenceForm := SequenceForm;
      SequenceForm.OnCloseNotify := MyDebugger.CloseNotify;
      Editform.OpenWindow(Self);
      Editform.Enter(Self);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIUMLCreateSequencediagramClick(Sender: TObject);
var
  SequenceForm: TFSequenceForm;
  UMLForm: TFUMLForm;
  FileName: string;
  ARtfdDiagram: TRtfdDiagram;
begin
  DisableUpdateMenuItems;
  UMLForm := GetActiveUML;
  if Assigned(UMLForm) then
  begin
    FileName := ChangeFileExt(UMLForm.Pathname, '.jsd');
    SequenceForm := GetTDIWindowType(FileName, '%Q%') as TFSequenceForm;
    if not Assigned(SequenceForm) and
      (not FileExists(FileName) or FileExists(FileName) and
      (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
      mbYesNoCancel, 0) = mrYes)) then
    begin
      SequenceForm := TFSequenceForm(FormFactory(fkSequence));
      SequenceForm.New(FileName);
      UMLForm.OpenWindow(Self);
      UMLForm.Enter(Self);
    end;
    SequenceForm.AddLifeline('Actor');
    ARtfdDiagram := (UMLForm.MainModul.Diagram as TRtfdDiagram);
    ARtfdDiagram.SequenceForm := SequenceForm;
    SequenceForm.OnCloseNotify := ARtfdDiagram.CloseNotify;
  end;
  EnableUpdateMenuItems;
end;

// receives synchronously sent messages from je2 java
procedure TFJava.WMCOPYDATA(var Msg: TWMCopyData);
var
  Str: string;
begin
  Str := PChar(Msg.CopyDataStruct.lpData);
  Str := Copy(Str, 1, Msg.CopyDataStruct.cbData div 2);
  GetComJava.AnswerFromJe2Java := Str;
end;

function TFJava.HasEditforms: Boolean;
begin
  Result := False;
  for var I := 0 to TDIEditFormCount - 1 do
    if TDIEditFormGet(I).FormTag in [1 .. 3, 11, 14] then
      Exit(True);
end;

function TFJava.HasJavaFiles: Boolean;
begin
  Result := False;
  for var I := 0 to TDIEditFormCount - 1 do
    if TDIEditFormGet(I).IsJava then
      Exit(True);
end;

function TFJava.HasPascalFiles: Boolean;
begin
  Result := False;
  for var I := 0 to TDIEditFormCount - 1 do
    if TDIEditFormGet(I).IsPascal then
      Exit(True);
end;

procedure TFJava.ImperativeUpdateMenuItems;
begin
  var
  Tmp := FUpdateMenuItemsCount;
  FUpdateMenuItemsCount := 0;
  UpdateMenuItems(Self);
  FUpdateMenuItemsCount := Tmp;
end;

procedure TFJava.UpdateMenuItems(Sender: TObject);
var
  EditorIsJava, IsEditorForm, IsBrowserForm, IsUMLForm, IsGUIForm, IsTextDiff,
    IsStructogram, IsSequenceDiagram, IsMessagesAndCompilable,
    IsMessagesAndRunnable, AndroidMode, NoProcessRunning: Boolean;
  UMLForm: TFUMLForm;
  FormTag, VisibleTDIs: Integer;

  function IsApplet: Boolean;
  begin
    Result := (EditorIsJava or IsGUIForm and Assigned(FEditorForm)) and
      FEditorForm.IsApplet;
  end;

  function hasSomethingToCompile: Boolean;
  var
    Form: TFForm;
  begin
    Result := False;
    Form := GetActiveForm;
    if Assigned(Form) then
    begin
      case Form.FormTag of
        1:
          if (Form as TFEditForm).IsJava then
          begin
            FEditorForm := TFEditForm(Form);
            Result := True;
          end;
        2:
          Result := True;
      end;
    end;
  end;

  function canRun: Boolean;
  var
    Editform: TFEditForm;
    UMLForm: TFUMLForm;
  begin
    { not: and FEditorForm.HasMainInModel, due to possible errors in sourcefile }
    if not NoProcessRunning then
      Exit(False);
    Result := True;
    if MyDebugger.Running then
      Exit(MyDebugger.ReadyForInput);
    if FConfiguration.AndroidMode then
      Exit;
    Editform := GetActiveEditor;
    if Assigned(Editform) and Editform.IsHTML or HasJavaFiles then
      Exit;
    UMLForm := GetActiveUML;
    if Assigned(UMLForm) and UMLForm.HasClassWithMain then
      Exit;
    if (FConfiguration.JavaStartClass <> '') or IsMessagesAndRunnable then
      Exit;
    Result := False;
  end;

  function canCompile: Boolean;
  var
    Form: TFForm;
  begin
    if AndroidMode or not FConfiguration.JavaCompilerOK or not NoProcessRunning
    then
      Exit(False);
    Form := GetActiveUML;
    if Assigned(Form) then
      Exit(True);
    Form := GetActiveForm;
    if Assigned(Form) and (Form.FormTag in [1 .. 3]) or MyDebugger.Running or
      HasJavaFiles or IsMessagesAndCompilable then
      Exit(True);
    Result := False;
  end;

begin
  if FUpdateMenuItemsCount > 0 then
    Exit;
  IsEditorForm := False;
  IsBrowserForm := False;
  IsUMLForm := False;
  IsGUIForm := False;
  IsTextDiff := False;
  IsStructogram := False;
  IsSequenceDiagram := False;
  IsMessagesAndCompilable := False;
  IsMessagesAndRunnable := False;
  EditorIsJava := False;
  FEditorForm := nil;
  UMLForm := nil;
  if Assigned(MyJavaCommands) then
    NoProcessRunning := not MyJavaCommands.ProcessRunning
  else
    NoProcessRunning := True;
  FActiveTDIChild := GetActiveForm;
  AndroidMode := FConfiguration.AndroidMode;

  FormTag := GetActiveFormTag;
  case FormTag of
    1:
      begin
        FEditorForm := GetActiveEditor;
        if Assigned(FEditorForm) and Assigned(FEditorForm.Editor) then
        begin
          IsEditorForm := True;
          EditorIsJava := FEditorForm.IsJava;
          if Assigned(FEditorForm.TBBreakpoint) then // nil during FClosing
            if EditorIsJava then
            begin
              FEditorForm.TBBreakpoint.Visible := True;
              FEditorForm.TBBreakpointsClear.Visible := True;
            end
            else
            begin
              FEditorForm.TBBreakpoint.Visible := False;
              FEditorForm.TBBreakpointsClear.Visible := False;
            end
          else
            Exit;
        end;
      end;
    2:
      begin
        IsUMLForm := True;
        UMLForm := TFUMLForm(FActiveTDIChild);
      end;
    3:
      begin
        IsGUIForm := True;
        FEditorForm := TFEditForm((FActiveTDIChild as TFGUIForm).Partner);
      end;
    4:
      IsTextDiff := True;
    5:
      IsBrowserForm := True;
    6:
      ; // Explorer
    7:
      begin // Messages
        IsMessagesAndCompilable := hasSomethingToCompile;
        IsMessagesAndRunnable := Assigned(FEditorForm) and FEditorForm.IsJava;
      end;
    8:
      begin // ObjectInspector
        FObjectInspector.UpdateState;
        if Assigned(FGUIDesigner.ELDesigner.DesignControl) then
        begin
          IsGUIForm := True;
          FEditorForm :=
            TFEditForm(TFForm(FGUIDesigner.ELDesigner.DesignControl).Partner);
        end;
      end;
    9:
      ; // ObjectGenerator
    10:
      ; // Hint
    11:
      IsStructogram := True;
    12:
      ; // Tooltip
    13:
      begin // TSynBaseCompletionProposalFormEx
        FEditorForm := MyCodeCompletion.Editform;
        if Assigned(FEditorForm) and Assigned(FEditorForm.Editor) then
          IsEditorForm := True;
      end;
    14:
      IsSequenceDiagram := True;
  end;
  if Assigned(FEditorForm) then
    EditorIsJava := FEditorForm.IsJava
  else
    EditorIsJava := False;

  VisibleTDIs := FMyTabBar.Tabs.Count;
  if (FormTag in [0, 9, 10, 12]) or (VisibleTDIs = 0) then
  begin
    SetEnabledMI(MIRedo, False);
    SetEnabledMI(MIUndo, False);
    SetEnabledMI(MICut, False);
    SetEnabledMI(MICopy, False);
    SetEnabledMI(MIPaste, False);
    SetEnabledTB(TBRedo, False);
    SetEnabledTB(TBUndo, False);
  end;

  // File-Menu
  // SetEnabledMI(MISave, (FormTag in [1..4, 10..14]) or FActiveTDIChild.Modified);
  // SetEnabledMI(MISaveAs, (FormTag in [1..3, 10..14]) or FActiveTDIChild.Modified);

  // SetEnabledMI(MISaveAll, HasSomethingToSave);
  // SetEnabledMI(MISaveAllIn, MISaveAll.enabled);
  SetEnabledMI(MISaveAsProject, VisibleTDIs > 0);
  SetEnabledMI(MICloseProject, FProjectFilename <> '');
  SetEnabledMI(MIClose, VisibleTDIs > 0);
  SetEnabledMI(MICloseAllFiles, (VisibleTDIs > 0) and (FProjectFilename = ''));
  SetEnabledMI(MIExport, IsEditorForm and
    Assigned(FEditorForm.Editor.Highlighter) or IsStructogram or
    IsSequenceDiagram);
  SetEnabledMI(MIPrint, VisibleTDIs > 0);
  SetEnabledMI(MIPrintAll, VisibleTDIs > 0);
  // Edit menu
  SetEnabledMI(MISearch, IsEditorForm or IsBrowserForm or IsTextDiff);
  SetEnabledMI(MISearchAgain, IsEditorForm or IsTextDiff);
  SetEnabledMI(MIReplace, IsEditorForm or IsTextDiff);
  SetEnabledMI(MIUnindent, IsEditorForm);
  SetEnabledMI(MIIndent, IsEditorForm);
  SetEnabledMI(MIStructuredIndent, IsEditorForm);
  SetEnabledMI(MICommentOnOff, IsEditorForm);
  SetEnabledMI(MIGotoLine, IsEditorForm or IsTextDiff);
  SetEnabledMI(MISystemOutPrintln, IsEditorForm);
  SetEnabledMI(MIUnicode, IsEditorForm and (FEditorForm.Encoding <> 'ANSI'));
  if MIUnicode.Visible <> MIUnicode.Enabled then
    MIUnicode.Visible := MIUnicode.Enabled;

  // Start menu
  // in debug-mode TBCompileJava has abort-function
  SetEnabledMI(MICompile, canCompile);
  SetEnabledTB(TBCompileJava, MICompile.Enabled);
  SetEnabledTB(TBCompileAll, MICompile.Enabled);
  SetEnabledMI(MICompileAll, MICompile.Enabled);
  SetEnabledMI(MIRun, canRun);
  if MyDebugger.Running and not MyDebugger.ReadyForInput then
    SetEnabledMI(MIRun, False);

  SetEnabledMI(MIJavaReset, IsEditorForm and FEditorForm.IsJava and
    NoProcessRunning);
  // SetEnabledTB(TBSaveAll, MISaveAll.Enabled);

  SetEnabledTB(TBRun, MIRun.Enabled or MIProgramReset.Enabled);
  SetEnabledMI(MIHTMLforApplet, IsApplet and NoProcessRunning);
  SetEnabledMI(MIHTMLforJavaPlugIn, MIHTMLforApplet.Enabled);
  SetEnabledMI(MIAppletviewer, FConfiguration.JavaAppletviewerOK and
    NoProcessRunning and (IsApplet or IsEditorForm and
    FEditorForm.IsHTMLApplet));
  if MIRun.Enabled then
    if MIAppletviewer.Enabled then
      MIRun.Caption := _('Start Applet')
    else if AndroidMode then
      MIRun.Caption := _(LNGTransferToAndroid)
    else
      MIRun.Caption := _(LNGRunApplication)
  else
    MIRun.Caption := _('Run');
  SetEnabledMI(MIDebugger, MICompile.Enabled and
    FConfiguration.JavaDebuggerOK and not MyDebugger.Running and
    not MIProgramReset.Enabled and not AndroidMode and NoProcessRunning);
  SetEnabledMI(MIDissasembler, NoProcessRunning);
  SetEnabledMI(MIJavaDoc, FConfiguration.JavaDocOK and EditorIsJava and
    not MyDebugger.Running and NoProcessRunning);
  SetEnabledMI(MIJar, FConfiguration.JavaJarOK and NoProcessRunning);
  SetEnabledMI(MIJarCreate, MICompile.Enabled);
  SetEnabledMI(MIJarPack, Assigned(FActiveTDIChild));

  // Test menu
  SetEnabledMI(MIStep, (EditorIsJava or MyDebugger.ReadyForInput) and
    not AndroidMode and NoProcessRunning);
  SetEnabledMI(MINext, MIStep.Enabled);
  SetEnabledMI(MIStepUp, MIStep.Enabled);
  SetEnabledMI(MIRunToCursor, EditorIsJava and MIStep.Enabled);
  SetEnabledMI(MIShowExecutionPoint, MyDebugger.Running);
  SetEnabledMI(MIBreakpoint, EditorIsJava);
  SetEnabledMI(MIBreakpointsClear, HasJavaFiles);
  SetEnabledMI(MIExpression,
    MyDebugger.Running { and myDebugger.ReadyForInput } );
  SetEnabledMI(MITestCreateSequencediagram, EditorIsJava and NoProcessRunning);
  // GetEditFormWithMain <> nil { EditorIsJava});
  SetEnabledTB(TBStep, MIStep.Enabled);
  SetEnabledTB(TBNext, MINext.Enabled);

  // UML menu
  SetEnabledMI(MINewUML, NoProcessRunning);
  SetEnabledMI(MINewClass, NoProcessRunning);
  SetEnabledMI(MIClassEditor, (EditorIsJava or IsUMLForm and
    UMLForm.HasEditableClass) and NoProcessRunning);
  SetEnabledMI(MINewComment, IsUMLForm);
  SetEnabledMI(MINewLayout, IsUMLForm);
  SetEnabledMI(MIRefresh, IsUMLForm);
  SetEnabledMI(MIDiagramFromOpenFiles, HasJavaFiles or HasPascalFiles);
  SetEnabledMI(MISaveAsPicture, IsUMLForm);
  SetEnabledTB(TBDiagramFromOpenFiles, MIDiagramFromOpenFiles.Enabled);
  SetEnabledMI(MIUMLCreateSequencediagram, IsUMLForm and NoProcessRunning);

  // Tools menu
  MICheckStyle.Visible := FConfiguration.CheckstyleOK;
  MIJalopy.Visible := FConfiguration.JalopyOK;
  MISubversion.Visible := FConfiguration.SubversionOK;
  MIGit.Visible := FConfiguration.GitOK;
  MIJUnit.Visible := FConfiguration.JUnitOk;
  SetEnabledMI(MICheckStyle, MICompile.Enabled and
    FConfiguration.CheckstyleOK and NoProcessRunning);
  SetEnabledMI(MIJalopy, MICompile.Enabled and FConfiguration.JalopyOK and
    NoProcessRunning);
  SetEnabledMI(MISubversion, IsEditorForm and NoProcessRunning);
  SetEnabledMI(MIGit, NoProcessRunning);
  SetEnabledMI(MIJUnit, NoProcessRunning);
  SetEnabledMI(MIJUnitCreateTestclass, MIJUnit.Visible and EditorIsJava and
    not FEditorForm.IsJUnitTestClass);
  SetEnabledMI(MIJUnitRunAllTests, MIJUnit.Visible and EditorIsJava and
    FEditorForm.IsJUnitTestClass);
  SetEnabledMI(MIConfigureTools, NoProcessRunning);

  // Window menu
  SetEnabledMI(MIMsDos, not FConfiguration.DOSWindowLocked);
  SetEnabledMI(MIWebsite, not FConfiguration.BlockedInternet);
  SetEnabledMI(MIUpdate, not FConfiguration.BlockedInternet and
    NoProcessRunning);
  SetEnabledMI(MIMaximized, VisibleTDIs > 0);
  SetEnabledMI(MICascade, VisibleTDIs > 0);
  SetEnabledMI(MITileVertical, VisibleTDIs > 0);
  SetEnabledMI(MITileHorizontal, VisibleTDIs > 0);
  SetEnabledMI(MIFont, FormTag in [1 .. 4, 6 .. 7, 11, 14, 16]);
  SetEnabledTB(TBSave, MISave.Enabled);

  // Help menu
  MIJunitManual.Visible := FConfiguration.JUnitOk;
  MIDemos.Visible := (FConfiguration.JavaDemos <> '');
end;

function TFJava.PreparePrint(Sender: TObject): Boolean;
begin
  Result := False;
  if HasDefaultPrinter then
  begin
    Result := True;
    if not Assigned(FPrintDialog) then
    begin
      FPrintDialog := TPrintDialog.Create(Self);
      try
        with FPrintDialog do
        begin
          Copies := 1;
          FromPage := 1;
          MinPage := 1;
          Options := [poPrintToFile, poPageNums, poSelection,
            poWarning, poHelp];
        end;
      except
        on E: Exception do
          OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' +
            E.Message));
      end;
    end;
  end
  else
    ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFJava.MIPrintClick(Sender: TObject);
begin
  if PreparePrint(Sender) and Assigned(FActiveTDIChild) then
    FActiveTDIChild.Print;
end;

procedure TFJava.MIPrintAllClick(Sender: TObject);
begin
  if PreparePrint(Sender) then
  begin
    FixWindows(False);
    for var I := 0 to Length(FFixedWindows) - 1 do
    begin
      Application.ProcessMessages;
      if FFixedWindows[I].FormTag = 1 then
        (FFixedWindows[I] as TFEditForm).PrintAll(True)
      else
        FFixedWindows[I].Print;
    end;
  end;
end;

procedure TFJava.MIPrintSetupClick(Sender: TObject);
begin
  if HasDefaultPrinter then
  begin
    if not Assigned(FPrinterSetup) then
      FPrinterSetup := TPrinterSetupDialog.Create(Self);
    FPrinterSetup.Execute;
    FConfiguration.WriteStringU('Printer', 'Printer',
      Printer.Printers[Printer.PrinterIndex]);
  end
  else
    ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFJava.MIFileStructureClick(Sender: TObject);
begin
  FFileStructure.ChangeHideShow;
  if FFileStructure.Visible then
    UpdateLayoutRightDockPanel(True);
end;

procedure TFJava.MIFontClick(Sender: TObject);
var
  FormTag: Integer;
  AFont: TFont;
begin
  FormTag := GetActiveFormTag;
  case FormTag of
    3, 5:
      Exit;
    7:
      AFont := FMessages.Font;
    8:
      AFont := FObjectInspector.Font;
    9:
      AFont := FObjectGenerator.Font;
    10:
      AFont := FScpHint.Font;
    12:
      Exit;
    16:
      AFont := FFileStructure.Font;
  else
    if not Assigned(FActiveTDIChild) then
      Exit
    else
      AFont := FActiveTDIChild.GetFont;
  end;
  FDFont.Font.Assign(AFont);

  if FormTag in [1, 4] then
    FDFont.Options := [fdFixedPitchOnly]
  else
    FDFont.Options := [];

  if FDFont.Execute then
  begin
    AFont.Assign(FDFont.Font);
    if FormTag in [1, 2, 4, 6, 11, 14] then
      for var I := 0 to FTDIFormsList.Count - 1 do
        if FTDIFormsList[I].FormTag = FormTag then
          FTDIFormsList[I].SetFont(AFont);
    case FormTag of
      7:
        FMessages.SetFont(AFont);
      8:
        FObjectInspector.SetFont(AFont);
      9:
        FObjectGenerator.SetFont(AFont);
      10:
        FScpHint.SetFont(AFont);
      16:
        FFileStructure.SetFont(AFont);
    end;
  end;
end;

function TFJava.WindowOpened(const Pathname: string; var AForm: TFForm)
  : Boolean;
begin
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    AForm := FTDIFormsList[I];
    if CompareText(AForm.Pathname, Pathname) = 0 then
      Exit(True);
  end;
  AForm := nil;
  Result := False;
end;

function TFJava.GetTDIWindowType(const Pathname, Typ: string): TFForm;
begin
  Result := nil;
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    var
    AForm := FTDIFormsList[I]; // AccessViolation removed
    if (AForm.GetFormType = Typ) and (CompareText(AForm.Pathname, Pathname) = 0)
    then
      Exit(AForm);
  end;
end;

function TFJava.GetTDIWindowTypeFilename(const FileName, Typ: string): TFForm;
begin
  Result := nil;
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    var
    AForm := FTDIFormsList[I]; // AccessViolation removed
    if (AForm.GetFormType = Typ) and AForm.Pathname.EndsWith('\' + FileName)
    then
      Exit(AForm);
  end;
end;

function TFJava.GetEditForm(Pathname: string): TFEditForm;
begin
  Result := nil;
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    var
    AForm := FTDIFormsList[I];
    if (AForm.FormTag = 1) and (AForm.Pathname = Pathname) then
      Exit(TFEditForm(AForm));
  end;
end;

function TFJava.GetGuiForm(Pathname: string): TFGUIForm;
begin
  Result := nil;
  for var I := 0 to TDIEditFormCount - 1 do
  begin
    var
    AForm := TDIEditFormGet(I).Partner;
    if Assigned(AForm) and (CompareText(AForm.Pathname, Pathname) = 0) then
      Exit(TFGUIForm(AForm));
  end;
end;

function TFJava.GetTDIWindow(const Pathname: string): TFForm;
begin
  Result := nil;
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    var
    AForm := FTDIFormsList[I]; // AccessViolation
    if CompareText(AForm.Pathname, Pathname) = 0 then
      Exit(AForm);
  end;
end;

function TFJava.GetTDIWindow(Number: Integer): TFForm;
begin
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    Result := FTDIFormsList[I];
    if Result.Number = Number then
      Exit;
  end;
  Result := nil;
end;

function TFJava.GetTDIType(const Typ: string): TFForm;
begin
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    Result := FTDIFormsList[I];
    if Result.GetFormType = Typ then
      Exit;
  end;
  Result := nil;
end;

procedure TFJava.SwitchToWindow(const Path: string);
begin
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    var
    AForm := FTDIFormsList[I];
    if CompareText(AForm.Pathname, Path) = 0 then
    begin
      AForm.OpenWindow(Self);
      Break;
    end;
  end;
end;

procedure TFJava.SwitchToWindow(Num: Integer);
begin
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    var
    AForm := FTDIFormsList[I];
    if AForm.Number = Num then
    begin
      AForm.OpenWindow(Self);
      Break;
    end;
  end;
end;

procedure TFJava.SwitchToWindow(Form: TFForm);
begin
  if Assigned(Form) then
    Form.OpenWindow(Self);
end;

procedure TFJava.SwitchWindowWithSearch(const Pathname: string);
begin
  SwitchToWindow(Pathname);
  if not Assigned(FEditorForm) or (CompareText(FEditorForm.Pathname, Pathname)
    <> 0) then
    if Open(Pathname) then
      RearrangeFileHistory(Pathname);
end;

function TFJava.GetPathnameForClass(const AClassname: string): string;
begin
  Result := '';
  for var I := 0 to TDIEditFormCount - 1 do
  begin
    var
    AForm := TDIEditFormGet(I);
    AForm.ParseSourceCode(False);
    if AForm.SourceContainsClass(AClassname) then
    begin
      Result := AForm.Pathname;
      Break;
    end;
  end;
end;

procedure TFJava.ChangeWindowWithPositioning(const Pathname: string;
XPos, YPos: Integer; Error: Boolean);
begin
  SwitchWindowWithSearch(Pathname);
  if Assigned(FEditorForm) then
  begin
    if FEditorForm.CanFocus then
    begin
      FEditorForm.SetFocus;
      FocusControl(FEditorForm.Editor);
    end;
    with FEditorForm.Editor do
    begin
      CaretX := XPos;
      CaretY := YPos;
      EnsureCursorPosVisible;
      Gutter.ShowLineNumbers := True;
    end;
    if Error then
      FEditorForm.SetErrorMark(YPos, XPos, 'ERROR');
  end;
end;

function TFJava.OpenEditForm(const Pathname: string; Hidden: Boolean)
  : TFEditForm;
var
  CheckAge: Boolean;
  Str: string;
  AActive: TFForm;
begin
  AActive := FActiveTDIChild;
  CheckAge := EditorAgeTimer.Enabled;
  try
    if CheckAge then
      EditorAgeTimer.Enabled := False;
    Result := nil;
    try
      if FileExists(Pathname) then
      begin
        Result := GetTDIWindowType(Pathname, '%E%') as TFEditForm;
        if Assigned(Result) then
        begin
          if not Hidden then
            Result.Show;
        end
        else
        begin
          LockFormUpdate(Self); // displaces CompletionProposalForm
          Result := TFEditForm(FormFactory(fkEditor));
          Result.Open(Pathname, '', Hidden);
          if Hidden then
          begin
            Result.Hide;
            if AActive.FormTag = 1 then
              scpSetEditor((AActive as TFEditForm).Editor);
          end;
          UnlockFormUpdate(Self);
        end;
      end;
      if not Hidden then
      begin
        Str := ChangeFileExt(Pathname, '.jfm');
        if FileExists(Str) then
          Open(Str);
      end;
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    if CheckAge then
      EditorAgeTimer.Enabled := True;
  end;
end;

function TFJava.Open(const FileName: string): Boolean;
var
  Str, Ext: string;
  AForm: TFForm;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    Ext := UpperCase(ExtractFileExt(FileName));
    AForm := GetTDIWindow(FileName);
    if Assigned(AForm) then
      AForm.OpenWindow(Self)
    else if (Ext = '.UML') then
    begin
      OpenUMLWindow(FileName, '');
      Result := True;
    end
    else if (Ext = '.JEP') then
    begin
      Result := False;
      MICloseAllFilesClick(Self);
      FProjectFilename := FileName;
      FMessages.StatusMessage(FProjectFilename);
      TThread.ForceQueue(nil,
        procedure
        begin
          OpenProject(FProjectFilename);
          UpdateMenuItems(Self);
        end);
    end
    else if (Ext = '.JFM') then
    begin
      Str := ChangeFileExt(FileName, '.java');
      if FileExists(Str) then
      begin
        if not WindowOpened(Str, AForm) then
        begin
          Open(Str); // opens Filename too
          AForm := GetTDIWindow(FileName);
        end
        else
          AForm := FGUIDesigner.Open(FileName, Str);
        Result := Assigned(AForm);
      end
      else
        ErrorMsg(Format(_(LNGAssociatedJavaFileNotFound), [Str]));
    end
    else if (Ext = '.JAR') then
      JarShowUnpackOpen(3, FileName)
    else if (Ext = '.CLASS') then
      DoOpenInUMLWindow(FileName)
    else if ((Ext = '.HTM') or (Ext = '.HTML')) and Assigned(FActiveTDIChild)
      and (FActiveTDIChild.FormTag = 5) then
      (FActiveTDIChild as TFBrowser).WebBrowser.Navigate(FileName)
    else if (Ext = '.JSG') or (Ext = '.NSD') then
      Result := OpenStructogram(FileName, '')
    else if (Ext = '.JSD') then
      Result := OpenSequenceDiagram(FileName, '')
    else
    begin { if ((Ext = '.JAVA') or (Ext = '.INI') or (Ext = '.TXT') or (Ext = '.~AVA') or (Ext = '.BAT')) then }
      FEditorForm := OpenEditForm(FileName, False);
      Result := Assigned(FEditorForm);
    end;
  end;
end;

procedure TFJava.OpenFileWithState(const Str: string);
var
  State, Typ, Address: string;
  Posi: Integer;
begin
  Posi := Pos('%X%', Str) + Pos('%B%', Str) + Pos('%E%', Str) + Pos('%U%', Str)
    + Pos('%G%', Str) + Pos('%T%', Str) + Pos('%S%', Str) + Pos('%Q%', Str);
  if Posi = 0 then
    Exit;
  State := Copy(Str, 1, Posi - 1);
  Typ := Copy(Str, Posi, 3);
  Address := FConfiguration.AddPortableDrive(Copy(Str, Posi + 3, Length(Str)));
  if (Typ = '%X%') and SysUtils.DirectoryExists(Address) then
    NewExplorer(Address, State)
  else if Typ = '%B%' then
    NewBrowser(Address, State)
  else if FileExists(Address) then
    if Typ = '%E%' then
      NewEditor(Address, State)
    else if Typ = '%U%' then
      NewUMLWindow(Address, State)
    else if Typ = '%G%' then
      NewGUIForm(Address, State)
    else if Typ = '%T%' then
      OpenProject(Address)
    else if Typ = '%S%' then
      OpenStructogram(Address, State)
    else if Typ = '%Q%' then
      OpenSequenceDiagram(Address, State);
end;

procedure TFJava.CloseFile(const FileName: string);
var
  AForm: TFForm;
begin
  if WindowOpened(FileName, AForm) then
  begin
    AForm.OpenWindow(Self);
    if Assigned(FActiveTDIChild) then
      FActiveTDIChild.Close;
  end;
end;

function TFJava.OpenWindowWithClass(const Directory, AClass: string): Boolean;
begin
  var
  Str := GetPathnameForClass(AClass);
  if Str <> '' then
  begin
    SwitchToWindow(Str);
    Exit(True);
  end;
  Result := Open(Directory + ReplaceStr(AClass, '.', '\') + '.java');
end;

procedure TFJava.SetBreakpoints;
begin
  for var I := 0 to TDIEditFormCount - 1 do
    TDIEditFormGet(I).SetBreakpoints;
end;

procedure TFJava.RearrangeFileHistory(const NewFile: string);
var
  FileCount: Integer;
  NewItem: TSpTBXItem;
  AMenuItem: TTBCustomItem;
  Str: string;
begin
  // already in history?
  FileCount := MIReopen.Count;
  for var I := FileCount - 1 downto 0 do
    if Pos(NewFile, MIReopen[I].Caption) > 0 then
      MIReopen.Delete(I);
  if not FileExists(NewFile) then
    Exit;

  // new history entry
  NewItem := TSpTBXItem.Create(Self);
  NewItem.Caption := '&1 ' + NewFile;
  NewItem.OnClick := MIMenuOpenClick;
  MIReopen.Insert(0, NewItem);
  FileCount := MIReopen.Count;
  if Assigned(FConfiguration) then
  begin
    if FileCount > FConfiguration.MaxFileHistory then
    begin
      AMenuItem := MIReopen[FileCount - 1];
      FreeAndNil(AMenuItem);
      Dec(FileCount);
    end;

    // number history entries
    for var I := 0 to FileCount - 1 do
    begin
      Str := MIReopen[I].Caption;
      Str := Right(Str, Pos(' ', Str) + 1);
      FConfiguration.WriteStringU('History', 'File' + IntToStr(I + 1),
        FConfiguration.RemovePortableDrive(Str));
      MIReopen[I].Caption := '&' + IntToStr(I + 1) + ' ' + Str;
    end;
    FConfiguration.WriteIntegerU('History', 'Files', FileCount);
  end;
end;

procedure TFJava.SetDockTopPanel;
var
  ATopFindToolbar, AHeight: Integer;
begin
  var
  HasMenuBar := False;
  for var I := 0 to High(FConfiguration.VisMenus) do
    HasMenuBar := HasMenuBar or FConfiguration.VisMenus[I];
  if HasMenuBar then
    AHeight := 23
  else
    AHeight := 0;

  var
  AllTabsClosed := True;
  for var I := 0 to High(FConfiguration.VisTabs) do
    if FConfiguration.VisTabs[I] then
      AllTabsClosed := False;
  TabsControl.Visible := not AllTabsClosed;

  if not AllTabsClosed then
    AHeight := AHeight + 57
  else if FConfiguration.VisToolbars[0] or FConfiguration.VisToolbars[1] then
    AHeight := AHeight + 28;
  // if in the future the find toolbar is activated
  ATopFindToolbar := AHeight;

  // if FindToolbar.Visible then
  // AHeight:= AHeight + 26;

  DebugToolbar.Left := MainToolBar.Left;
  MainToolBar.Top := 2;
  if MainToolBar.Visible then
  begin
    if TabsControl.Visible then
    begin
      DebugToolbar.Top := 2 + MainToolBar.Height + 2;
    end
    else
    begin
      DebugToolbar.Top := 2;
      DebugToolbar.Left := MainToolBar.Left + MainToolBar.Width + 3;
    end;
  end
  else
    DebugToolbar.Top := 2;
  PBorder.Left := TabsControl.Left + TabsControl.Width + 5;
  // FindToolbar.Top:= PPIScale(aTopFindToolbar);
  TBXDockTop.Height := PPIScale(AHeight);
end;

procedure TFJava.ReadHistory;
var
  FileCount, Number: Integer;
  Str: string;
  NewItem: TSpTBXItem;
begin
  FileCount := FConfiguration.ReadIntegerU('History', 'Files', 0);
  Number := 0;
  for var I := 1 to FileCount do
  begin
    Str := FConfiguration.AddPortableDrive(FConfiguration.ReadStringU('History',
      'File' + I.ToString, ''));
    if (Str <> '') and FileExists(Str) then
    begin
      Inc(Number);
      try
        NewItem := TSpTBXItem.Create(Self);
        NewItem.Caption := '&' + Number.ToString + ' ' + Str;
        NewItem.OnClick := MIMenuOpenClick;
        MIReopen.Add(NewItem);
        FConfiguration.WriteStringU('History', 'File' + Number.ToString,
          FConfiguration.RemovePortableDrive(Str));
      except
        on E: Exception do
          OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' +
            E.Message));
      end;
    end;
  end;
  FConfiguration.WriteIntegerU('History', 'Files', Number);
end;

procedure TFJava.OpenFiles;
var
  Str, Cur: string;
  WinCount, X, Y: Integer;
  Mouse, Posi: TPoint;
  Rect: TRect;
  Editform: TFEditForm;
begin
  FMyTabBar.Invalidate;
  Screen.OnActiveFormChange := UpdateMenuItems;
  try
    LockFormUpdate(Self);
    Screen.Cursor := crHourGlass;
    DisableUpdateMenuItems;
    FInteractiveUMLForm := MakeNewUMLWindow(_(LNGInteractive), '');
    OpenFileStructureAndObjectInspector;
    FMessages.InitAndShow;
    if FConfiguration.LoadFiles then
    begin
      WinCount := FConfiguration.ReadIntegerU('Window', 'Wins', 0);
      for var I := 1 to WinCount do
      begin
        Str := FConfiguration.ReadStringU('Window', 'Win' + IntToStr(I), '');
        OpenFileWithState(Str);
      end;
      Cur := FConfiguration.ReadStringU('Window', 'Current', '');
    end;
    for var I := 1 to ParamCount do
      if UpperCase(ExtractFileExt(ParamStr(I))) <> '.INI' then
      begin
        if Open(ParamStr(I)) then
          RearrangeFileHistory(ParamStr(I));
        Cur := ParamStr(I);
      end;
  finally
    OpenFileWithState(Cur);
    HSplitterMoved(Self);
    VSplitterMoved(Self);
    UnlockFormUpdate(Self);
    Screen.Cursor := crDefault;
  end;

  // if one webbrowser is opened, then their is no blinking cursor
  // in the active EditForm! According to Screen ActiveForm a
  // WebBrowser never gets the focus
  // workaround: simulate a click on the EditForm
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 1) then
  begin
    Editform := FActiveTDIChild as TFEditForm;
    GetCursorPos(Mouse);
    Posi := Editform.ClientToScreen(Point(0, 0));
    Rect := Editform.ClientRect;
    X := Posi.X + Rect.Right div 2;
    Y := Posi.Y + Rect.Bottom div 2;
    SetCursorPos(X, Y);
    if GetSystemMetrics(SM_SWAPBUTTON) = 0 then
    begin
      mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
      mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    end
    else
    begin
      mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
      mouse_event(MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
    end;
    Application.ProcessMessages;
    SetCursorPos(Mouse.X, Mouse.Y);
  end;
  EnableUpdateMenuItems;
  if FConfiguration.FirstStartAfterInstallation then
    MIDefaultLayoutClick(Self);
  with TFUpdateDialog.Create(Self) do
  begin
    CheckAutomatically;
    Free;
  end;
  EditorAgeTimer.Enabled := FConfiguration.CheckAge;
  ChangeStyle(FConfiguration.GUIStyle);

  if not FileExists(FConfiguration.JavaInterpreter) then
    TThread.ForceQueue(nil,
      procedure
      begin
        FConfiguration.ShowPage(0);
        MIConfigurationClick(Self);
      end);
end;

procedure TFJava.ConnectGUIAndJavaWindow(GUIForm: TFGUIForm);
begin
  var
  Str := ChangeFileExt(GUIForm.Pathname, '.java');
  FEditorForm := TFEditForm(GetTDIWindow(Str));
  if Assigned(FEditorForm) then
  begin
    FEditorForm.Partner := GUIForm;
    GUIForm.Partner := FEditorForm;
  end
  else
    ErrorMsg(Format(_(LNGAssociatedJavaFileNotFound), [Str]));
  FObjectInspector.RefreshCBObjects;
end;

{ --- Start-Menü --------------------------------------------------- }

procedure TFJava.SaveBeforeCompile;
begin
  FixWindows(True);
  for var I := 0 to Length(FFixedWindows) - 1 do
    if FFixedWindows[I].FormTag = 1 then
    begin
      var
      Editform := (FFixedWindows[I] as TFEditForm);
      if Editform.Modified or not FileExists(Editform.Pathname) then
        DoSave(Editform, False);
    end;
end;

procedure TFJava.Compile;
begin
  if Assigned(FEditorForm) and FEditorForm.IsJava then
  begin
    if Pos(FConfiguration.JavaCache, FEditorForm.Pathname) = 1 then
      ErrorMsg(_('Files in the cache folder are not compiled.') + ': ' +
        FEditorForm.Pathname)
    else
    begin
      MyJavaCommands.SetManyCompiling(False);
      MyJavaCommands.CompileForm(FEditorForm);
      try
        if Assigned(FEditorForm.Editor) and Assigned(FEditorForm.Editor.Gutter)
          and not MyJavaCommands.SuccessfullCompiled and
          not FEditorForm.Editor.Gutter.ShowLineNumbers then
          FEditorForm.SBNumbersClick(Self);
        if not MyJavaCommands.SuccessfullCompiled then
          ShowCompileErrors
        else
          FEditorForm.InitShowCompileErrors;
      except
        on E: Exception do
          FConfiguration.Log('TFJava.Compile', E);
      end;
    end;
  end
  else
    MICompileAllClick(Self);
end;

procedure TFJava.TBCompileJavaClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if TBCompileJava.Hint = _(LNGResetProgram) then
    MIProgramResetClick(Self)
  else
    MICompileClick(Self);
  EnableUpdateMenuItems;
end;

procedure TFJava.MICompileClick(Sender: TObject);
begin
  SaveBeforeCompile;
  var
  UMLForm := GetActiveUML;
  if Assigned(UMLForm) then
  begin
    var
    StringList := UMLForm.GetFilesAndPackages(True);
    CompileList(StringList);
    FreeAndNil(StringList);
  end
  else
  begin
    FEditorForm := GetActiveEditor;
    Compile;
  end;
  RefreshUMLWindows;
end;

procedure TFJava.CompileList(StringList: TStringList);
var
  Pathname, Package: string;
  Posi: Integer;
begin
  FMessages.DeleteTab(K_Compiler);
  FMessages.StatusMessage('');
  MyJavaCommands.SetManyCompiling(True);
  for var I := 0 to StringList.Count - 1 do
  begin
    Pathname := StringList[I];
    Posi := Pos('|', Pathname);
    if Posi > 0 then
    begin
      Package := Copy(Pathname, Posi + 1, Length(Pathname));
      Delete(Pathname, Posi, Length(Pathname));
    end
    else
      Package := '';
    if FileExists(Pathname) then
      MyJavaCommands.Compile(Pathname, Package)
    else
      FMessages.OutputLineTo(K_Compiler, Format(_(LNGFileNotFound),
        [Pathname]));
  end;
  if MyJavaCommands.ManyCompilingErrorOccured then
    ShowCompileErrors;
  MyJavaCommands.SetManyCompiling(False);
end;

procedure TFJava.CompileOneWith(const Pathname: string);
begin
  var
  StringList := TStringList.Create;
  StringList.Add(Pathname);
  SaveBeforeCompile;
  CompileList(StringList);
  FreeAndNil(StringList);
  RefreshUMLWindows;
end;

procedure TFJava.TBClassOpenClick(Sender: TObject);
begin
  MIClassOpenClick(Self);
end;

procedure TFJava.TBCompileAllClick(Sender: TObject);
begin
  if MIProgramReset.Enabled then
    MIProgramResetClick(Self)
  else if FConfiguration.MindstormsMode then
    case FConfiguration.MindstormsVersion of
      0:
        MyJavaCommands.Upload('Lejos-Firmware');
      1:
        with TFMindstormsDialog.Create(Self) do
          ShowModal;
      2:
        with TFMindstormsEV3Dialog.Create(Self) do
          ShowModal;
    end
  else
    MICompileAllClick(Self);
end;

procedure TFJava.MICompileAllClick(Sender: TObject);
begin
  SaveBeforeCompile;
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
  begin
    var
    StringList := (FActiveTDIChild as TFUMLForm).GetFilesAndPackages(False);
    CompileList(StringList);
    FreeAndNil(StringList);
  end
  else
    MyJavaCommands.CompileAll;
  RefreshUMLWindows;
end;

var
  Nesting: Boolean;

procedure TFJava.TBFontMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
var
  Delta, FormTag: Integer;
begin
  if Nesting then
    Exit;
  Nesting := True;
  if Button = mbLeft then
    Delta := +1
  else
    Delta := -1;
  FormTag := GetActiveFormTag;
  if FormTag in [1, 4, 5, 6, 11, 14] then
    for var I := 0 to FTDIFormsList.Count - 1 do
      FTDIFormsList[I].SetFontSize(Delta);

  case FormTag of
    2:
      if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
        FActiveTDIChild.SetFontSize(Delta);
    3:
      for var I := 0 to FTDIFormsList.Count - 1 do
        if FTDIFormsList[I].FormTag = 3 then
          FTDIFormsList[I].SetFontSize(Delta);
    7:
      FMessages.SetFontSize(Delta);
    8:
      FObjectInspector.SetFontSize(Delta);
    9:
      ; // ObjectGenerator has a modal window
    10:
      FScpHint.SetFontSize(Delta);
    12:
      ; // FTooltip
    13:
      ; // TSynBaseCompletionProposalFormEx
    17:
      FGUIDesigner.Zooming(Delta);
  end;
  Nesting := False;
end;

procedure TFJava.TBRunClick(Sender: TObject);
begin
  if TBRun.Hint = _(LNGResetProgram) then
    MIProgramResetClick(Self)
  else if MIRun.Enabled or MyDebugger.Running then
    MIRunClick(Self);
end;

function TFJava.IsJavaApplet(Editform: TFEditForm): Boolean;
begin
  Result := Assigned(Editform) and Editform.IsApplet or
    FConfiguration.JavaStartClassIsApplet;
end;

function TFJava.HasBreakpoints: Boolean;
begin
  Result := MyDebugger.HasBreakpoints;
  var
  I := 0;
  var
  Count := TDIEditFormCount;
  while not Result and (I < Count) do
  begin
    if TDIEditFormGet(I).HasBreakpoints then
    begin
      Result := True;
      Break;
    end;
    Inc(I);
  end;
end;

function TFJava.PreCompile(AForm: TFEditForm; Pathname: string): Boolean;
var
  Saved, Okay: Boolean;
begin
  Saved := False;
  if not Assigned(AForm) then
    AForm := TFEditForm(FJava.GetTDIWindowType(Pathname, '%E%'));
  if Assigned(AForm) and AForm.IsHTML then
  begin
    if AForm.Modified or AForm.IsDefaultFilename then
      DoSave(AForm, WithoutBackup);
    Pathname := ChangeFileExt(AForm.Pathname, '.java');
    AForm := TFEditForm(FJava.GetTDIWindowType(Pathname, '%E%'));
  end;

  if Assigned(AForm) and Assigned(AForm.Editor) then
  begin
    if AForm.Modified or AForm.IsDefaultFilename then
    begin
      DoSave(AForm, WithoutBackup);
      Saved := not AForm.Modified;
    end;
    Pathname := AForm.Pathname;
  end;

  if HasJavaExtension(Pathname) then
  begin
    if not FConfiguration.HasAgeClass(Pathname) or Saved then
    begin // // no class file or freshly saved
      if Assigned(AForm) then
        MyJavaCommands.CompileForm(AForm)
      else
        MyJavaCommands.Compile(Pathname, '');
      RefreshUMLWindows;
      Okay := MyJavaCommands.SuccessfullCompiled;
    end
    else
      Okay := True;
    if Assigned(AForm) and not Okay and not AForm.Editor.Gutter.ShowLineNumbers
    then
      AForm.SBNumbersClick(Self);
  end
  else
    Okay := True;
  Result := Okay;
end;

procedure TFJava.DoRunApplet(const JavaProgramm: string; Editform: TFEditForm);
var
  Applet: string;
begin
  if FConfiguration.HasClass(JavaProgramm) then
  begin
    var
    Debugging := HasBreakpoints;
    if Debugging and not FConfiguration.JavaAppletviewerOK then
      ErrorMsg('no Appletviewer for debugging') // ToDo
    else
    begin
      DoHTMLForApplet(Editform, False, False, False, Debugging, Applet);
      if Debugging then
        MyDebugger.DebugApplet(JavaProgramm)
      else if (FConfiguration.AppletStart in [0, 1]) and FConfiguration.JavaAppletviewerOK
      then
        MyJavaCommands.AppletViewer(Applet)
      else
      begin
        CloseBrowser;
        CallApplet(Applet);
      end;
    end;
  end;
end;

procedure TFJava.DoRunProgram(const JavaProgram, AParameter: string;
Editform: TFEditForm);
var
  FrameType: Integer;
  Gui, JavaFX: Boolean;
  Package: string;
begin
  FrameType := FConfiguration.GetFrameType(JavaProgram);
  Gui := (FrameType > 1);
  JavaFX := (FrameType = 8);
  if HasJavaExtension(JavaProgram) then
  begin
    Package := FConfiguration.GetPackage(JavaProgram, True);
    if FConfiguration.HasClass(JavaProgram) then
    begin
      if HasBreakpoints then
        if FConfiguration.MindstormsMode then
        begin
          ErrorMsg(_(LNGMindstormsPrograms));
          if Assigned(Editform) then
            Editform.ClearBreakpoints;
        end
        else
          MyDebugger.DebugProgram(JavaProgram, AParameter, Package, Gui, JavaFX)
      else if FConfiguration.MindstormsMode then
        MyJavaCommands.Upload(JavaProgram)
      else
        MyJavaCommands.Run(AParameter, JavaProgram, Package, Gui, JavaFX);
    end;
  end;
end;

procedure TFJava.DoRunHTML(const Pathname: string);
begin
  if (FConfiguration.AppletStart in [0, 2]) or not FConfiguration.JavaAppletviewerOK
  then
  begin
    CloseBrowser;
    Application.ProcessMessages;
    Sleep(1000);
    CallApplet(Pathname);
  end
  else
    MIAppletviewerClick(nil);
end;

function TFJava.GetEditFormWithMain: TFEditForm;
begin
  Result := nil;
  var
  I := 0;
  while I < TDIEditFormCount do
  begin
    var
    AForm := TDIEditFormGet(I);
    if AForm.HasMainInModel then
    begin
      Result := AForm;
      Break;
    end;
    Inc(I);
  end;
end;

procedure TFJava.DoRun(Editform: TFEditForm; const Pathname: string);
var
  JavaProgramm, AParameter: string;
  Main: TFEditForm;
  IsApplet: Boolean;
begin
  AParameter := '';
  IsApplet := False;

  if Assigned(Editform) then
  begin
    if Editform.IsJUnitTestClass then
    begin
      Editform.RunTests;
      Exit;
    end;

    JavaProgramm := Editform.Pathname;
    AParameter := Editform.Parameter;
    IsApplet := Editform.IsApplet or Editform.IsHTMLApplet;
    if not(IsApplet or Editform.HasMainInModel) then
    begin
      Main := GetEditFormWithMain;
      if Assigned(Main) then
      begin
        JavaProgramm := Main.Pathname;
        AParameter := Main.Parameter;
        IsApplet := Main.IsApplet;
        Editform := Main;
      end;
    end;
  end
  else
  begin
    JavaProgramm := Pathname;
    AParameter := FConfiguration.JavaStartClass;
    Editform := OpenEditForm(Pathname, True);
    if Assigned(Editform) then
      IsApplet := Editform.IsApplet;
  end;

  if IsApplet then
    DoRunApplet(JavaProgramm, Editform)
  else
    DoRunProgram(JavaProgramm, AParameter, Editform);
end;

procedure TFJava.RunAndroid;
var
  GradleWfound: Boolean;
  Applicationname, CommandLine, ShortenedPath: string;
begin
  // The following will check if the Android SDK Path is configured correctly
  if FConfiguration.AndroidMode and not MIProgramReset.Enabled then
  begin
    if Assigned(FActiveTDIChild) then
      DoSave(FActiveTDIChild, FConfiguration.CreateBAKFiles)
    else
      Exit;
    FConfiguration.CheckAllFilesAndFolders;
    if (FConfiguration.EAndroidSDKFolder.Color = clRed) or
      (FConfiguration.AndroidSDKFolder = '') then
    begin
      ErrorMsg('Android SDK Path is not properly set.');
      Exit;
    end;

    // Find gradlew.bat in the project Folder
    GradleWfound := False;

    ShortenedPath := FActiveTDIChild.Pathname;
    while ((Pos('\', ShortenedPath) <> 0) and not GradleWfound) do
    begin
      ShortenedPath := Copy(ShortenedPath, 1,
        LastDelimiter('\', ShortenedPath) - 1);
      if FileExists(DissolveUsername(ShortenedPath + '\gradlew.bat')) then
        GradleWfound := True;
    end;

    if GradleWfound then
    begin
      Applicationname := FConfiguration.EditorFolder + 'assemble.bat';
      CommandLine := HideBlanks(ShortenedPath) + ' ' +
        HideBlanks(FConfiguration.AndroidSDKFolder) + ' ' +
        HideBlanks(FConfiguration.JDKFolder);
      MyJavaCommands.ExecWithPipe(Applicationname, CommandLine, '.');
    end
    else
      ErrorMsg('gradlew.bat not found');
  end;
end;

procedure TFJava.MIRunClick(Sender: TObject);
var
  JavaProgramm, AFile: string;
  Okay: Boolean;
  Editform: TFEditForm;
  UMLForm: TFUMLForm;
  StringList: TStringList;
begin
  // due to
  // [006A3E2E]{javaeditor.exe} UJava.TFJava.MIRunClick (Line 3065, "UJava.pas" + 1) + $7

  if FConfiguration.AndroidMode then
  begin
    RunAndroid;
    Exit;
  end;

  if not Assigned(MyJavaCommands) then
    Exit;

  if MyDebugger.Running then
  begin
    PrepareStep;
    MyDebugger.NewCommand(3, 'cont');
    Exit;
  end;
  // if myDebugger.Running and MIRun.Enabled then exit; // prevent doubled execution

  for var I := FTDIFormsList.Count - 1 downto 0 do
    if (FTDIFormsList[I] is TFEditForm) and FTDIFormsList[I].Modified then
      PreCompile(FTDIFormsList[I] as TFEditForm, '');

  Editform := GetActiveEditor;
  if Assigned(Editform) and Editform.IsHTML then
  begin
    DoRunHTML(Editform.Pathname);
    Exit;
  end;

  FMessages.DeleteTab(K_Compiler);
  if Assigned(Editform) then
    if not PreCompile(Editform, '') then
      Exit;

  JavaProgramm := '';
  UMLForm := GetActiveUML;
  if Assigned(UMLForm) then
  begin
    MyJavaCommands.SetManyCompiling(True);
    StringList := UMLForm.GetAllPathnames;
    Okay := True;
    for var I := 0 to StringList.Count - 1 do
    begin
      AFile := StringList[I];
      Okay := PreCompile(nil, AFile) and Okay;
    end;
    MyJavaCommands.SetManyCompiling(False);
    JavaProgramm := UMLForm.GetFileWithMain;
    Editform := GetTDIWindowType(JavaProgramm, '%E%') as TFEditForm;
    FreeAndNil(StringList);
    if not Okay then
      Exit;
  end;

  if FConfiguration.JavaStartClass <> '' then
  begin
    if not FileExists(FConfiguration.JavaStartClass) then
    begin
      ErrorMsg(Format(_(LNGFileNotFound), [FConfiguration.JavaStartClass]));
      Exit;
    end;
    if PreCompile(nil, FConfiguration.JavaStartClass) then
    begin
      JavaProgramm := FConfiguration.JavaStartClass;
      if Assigned(Editform) and (Editform.Pathname <> JavaProgramm) then
        Editform := nil;
    end
    else
      Exit;
  end;

  if not Assigned(Editform) then
    Editform := GetEditorWithMain;
  if Assigned(Editform) and not Editform.Modified or (JavaProgramm <> '') then
    DoRun(Editform, JavaProgramm);

  if Assigned(Editform) and Assigned(Editform.Partner) then
    try
      Editform.Partner.Repaint;
    except
      on E: Exception do
        FConfiguration.Log('TFJava.MIRunClick repaint', E);
    end;
end;

procedure TFJava.Run(const Pathname: string);
var
  AFile: string;
  Okay: Boolean;
  StringList: TStringList;
begin
  if MyDebugger.Running and MIRun.Enabled then
    Exit; // prevents doubled execution
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
  begin
    StringList := (FActiveTDIChild as TFUMLForm).GetAllPathnames;
    Okay := True;
    for var I := 0 to StringList.Count - 1 do
    begin
      AFile := StringList[I];
      Okay := PreCompile(nil, AFile) and Okay;
    end;
    FreeAndNil(StringList);
    if Okay then
      DoRun(nil, Pathname);
  end;
end;

procedure TFJava.MIDebugClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
    (FActiveTDIChild as TFUMLForm).DebugJE2Java;
end;

procedure TFJava.MIDebuggerClick(Sender: TObject);
begin
  if FConfiguration.MindstormsMode then
    ShowMessage(_(LNGMindstormsPrograms))
  else
  begin
    MyDebugger.BreakpointAtMain := True;
    MIRunClick(nil);
  end;
end;

procedure TFJava.MIDefaultLayoutClick(Sender: TObject);
begin
  LockFormUpdate(Self);
  FMessages.ShowIt;
  BottomDockPanel.Height := ClientHeight div 5;
  FMessages.MyDock;
  if Assigned(FFileStructure) then
    FFileStructure.ShowIt;
  if Assigned(FObjectInspector) then
    FObjectInspector.ShowIt;
  VSplitter.Visible := True;
  RightDockPanel.Width := ClientWidth div 4;
  if Assigned(FFileStructure) then
    FFileStructure.ManualDock(FJava.RightDockPanel, nil, alTop);
  if Assigned(FJUnitTests) then
  begin
    FJUnitTests.ShowIt;
    FJUnitTests.ManualDock(FJava.RightDockPanel, nil, alTop);
  end;
  if Assigned(FObjectInspector) then
    FObjectInspector.ManualDock(FJava.RightDockPanel, nil, alBottom);
  VSplitterMoved(Self);
  HSplitterMoved(Self);
  UnlockFormUpdate(Self);
end;

procedure TFJava.UpdateLayoutRightDockPanel(SetWidthHeight: Boolean = False);
begin
  if RightDockPanel.Width > 0 then
  begin
    RightDockPanel.Width := ClientWidth div 4;
    if Assigned(FFileStructure) then
      FFileStructure.ManualDock(FJava.RightDockPanel, nil, alTop);
    if Assigned(FJUnitTests) then
    begin
      FJUnitTests.ShowIt;
      FJUnitTests.ManualDock(FJava.RightDockPanel, nil, alTop);
    end;
    if Assigned(FObjectInspector) then
      FObjectInspector.ManualDock(FJava.RightDockPanel, nil, alBottom);
  end;
end;

procedure TFJava.MIProgramResetClick(Sender: TObject);
begin
  MyDebugger.Terminate;
  MyJavaCommands.Terminate;
end;

procedure TFJava.MIParameterClick(Sender: TObject);
begin
  with TFParameterDialog.Create(Self) do
  begin
    EStartClass.Text := FConfiguration.JavaStartClass;
    if Assigned(FEditorForm) then
    begin
      LParameter.Caption := LParameter.Caption + ' ' +
        ExtractFileName(FEditorForm.Pathname);
      EParameter.Text := FEditorForm.Parameter;
      EParameter.Enabled := True;
      EParameter.Color := clWindow;
      Pathname := FEditorForm.Pathname;
    end
    else
    begin
      EParameter.Text := '';
      EParameter.Enabled := False;
      EParameter.Color := clInactiveCaption;
      Pathname := '';
    end;
    if ShowModal = mrOk then
    begin
      FConfiguration.JavaStartClass := EStartClass.Text;
      FConfiguration.WriteStringU('Program', 'StartClass',
        FConfiguration.JavaStartClass);
      if Assigned(FEditorForm) then
        FEditorForm.Parameter := Trim(EParameter.Text);
    end;
    Free;
  end;
  UpdateMenuItems(Self);
end;

procedure TFJava.MIHTMLforAppletClick(Sender: TObject);
var
  Editform: TFEditForm;
  Applet: string;
  AForm: TFForm;
begin
  DisableUpdateMenuItems;
  Editform := FEditorForm;
  if Assigned(Editform) and Editform.IsHTMLApplet then
  begin
    SwitchToWindow(ChangeFileExt(Editform.Pathname, '.java'));
    Editform := FEditorForm;
  end;

  with Editform do
  begin
    if not IsJava then
      Exit;
    if Modified then
      DoSave(Editform, WithoutBackup);
    Applet := ChangeFileExt(Pathname, '.html');
    if WindowOpened(Applet, AForm) then
    begin
      AForm.Close;
      Application.ProcessMessages;
    end;
    DoHTMLForApplet(Editform, True, Sender = MIHTMLforJavaPlugIn, True,
      False, Applet);
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.DoHTMLForApplet(Editform: TFEditForm;
ForceCreate, PlugIn, AShow, Debug: Boolean; var Applet: string);
var
  Child: TFEditForm;
  AForm: TFForm;
  Call, HTMLConverter, ErrFile, Package, AClass, Dir, AppletArchive, AppletCode,
    Width, Height: string;
  WithJEApplets: Boolean;
  Point: TPoint;
  Num: Integer;
begin
  with Editform do
  begin
    Package := GetPackage;
    AClass := ChangeFileExt(ExtractFileName(Pathname), '');
    Dir := ExtractFilePath(Pathname);
    if Package <> '' then
    begin
      Num := CountChar('.', Package) + 1;
      while Num > 0 do
      begin
        Dir := Copy(Dir, 1, Length(Dir) - 1);
        Dir := Copy(Dir, 1, LastDelimiter('\', Dir));
        Dec(Num);
      end;
      AClass := Package + '.' + AClass;
    end;
    Applet := Dir + AClass + '.html';
    Point := GetWidthAndHeight;
    WithJEApplets := HasWord('NumberField') or HasWord('JNumberField') or
      HasWord('Turtle');
  end;
  Width := IntToStr(Point.X - 8);
  Height := IntToStr(Point.Y - 34);

  if ForceCreate or not FileExists(Applet) then
  begin
    LockWindow(FJava.Handle);

    Child := TFEditForm(FormFactory(fkEditor));
    Child.New(Applet);
    Child.HTMLforApplet(Width, Height, FConfiguration.GetCharset, Dir, AClass,
      WithJEApplets, Debug);
    Child.CheckAgeEnabled := False;
    Child.Save(WithoutBackup);
    ErrFile := FConfiguration.TempDir + 'error.txt';
    if PlugIn then
    begin
      HTMLConverter := FConfiguration.JDKFolder + '\lib\htmlconverter.jar';
      Call := '-jar ' + HTMLConverter + ' -f ' + ExtractFileName(Applet);
      if MyJavaCommands.ExecAndWait(FConfiguration.JavaInterpreter, Call,
        ExtractFilePath(Applet), ErrFile, SW_HIDE) then
        Child.Editor.Lines.LoadFromFile(Applet);
      if TFile.GetSize(ErrFile) > 0 then
      begin
        FMessages.ShowTab(K_Messages);
        FMessages.ShowMessages(ErrFile);
      end;
    end;
    Child.Save(WithoutBackup);
    if FConfiguration.ShowHTMLforApplet or AShow then
      RearrangeFileHistory(Applet)
    else
      Child.Close;
    UpdateMenuItems(Self);
    UnlockWindow;
  end
  else
  begin
    AppletArchive := FConfiguration.GetAppletArchiv;
    AppletCode := '<applet code="' + ReplaceStr(AClass, '.', '/') + '.class" ' +
      AppletArchive + ' width="' + Width + '" height="' + Height + '">';

    if WindowOpened(Applet, AForm) then
    begin
      SwitchToWindow(Applet);
      // MantisBT
      if Assigned(FEditorForm) and (Pos(AppletCode, FEditorForm.Editor.Text) = 0)
      then
        FEditorForm.ReplaceLine('<applet code=', AppletCode);
    end
    else
    begin
      Child := OpenEditForm(Applet, True); // open Hidden
      if Assigned(Child) then
      begin
        if Assigned(Child.Editor) and (Pos(AppletCode, Child.Editor.Text) = 0)
        then
          Child.ReplaceLine('<applet code=', AppletCode);
        Child.Close;
      end;
    end;
  end;
end;

procedure TFJava.MIAppletviewerClick(Sender: TObject);
var
  Applet: string;
begin
  if Assigned(FEditorForm) then
    if not PreCompile(FEditorForm, '') then
      Exit;

  if FEditorForm.IsHTMLApplet then
    Applet := FEditorForm.Pathname
  else
    DoHTMLForApplet(FEditorForm, False, False, False, False, Applet);
  MyJavaCommands.AppletViewer(Applet);
end;

procedure TFJava.DoDisassemble(const FileName: string);
var
  Child: TFEditForm;
  Disassembler, Call, AFile, Str, ErrFile: string;
begin
  AFile := ChangeFileExt(ExtractFileName(FileName), '');
  ErrFile := FConfiguration.TempDir + 'error.txt';
  Screen.Cursor := crHourGlass;
  try
    Disassembler := FConfiguration.JavaDisassembler;
    if Pos('javap.exe', Disassembler) > 0 then
      Call := '-classpath ' + FConfiguration.GetClassPath + ' '
    else
      Call := '';
    Call := Call + FConfiguration.JavaDisassemblerParameter + ' ' + AFile;
    if MyJavaCommands.ExecAndWait(Disassembler, Call, ExtractFilePath(FileName),
      ErrFile, SW_HIDE) then
    begin
      Child := TFEditForm(FormFactory(fkEditor));
      if Pos('jad.exe', Disassembler) > 0 then
      begin
        FMessages.ShowMessages(ErrFile);
        Str := ChangeFileExt(FileName, '.jad');
        Child.New(Str);
        Child.Editor.Lines.LoadFromFile(Str);
      end
      else
      begin
        Str := ChangeFileExt(FileName, '.txt');
        Child.New(Str);
        Child.Editor.Lines.LoadFromFile(ErrFile);
      end;
      Child.Editor.Lines.SaveToFile(Str);
      FMessages.StatusMessage(Str + ' ' + _('disassembled'));
    end
    else
    begin
      Screen.Cursor := crDefault;
      ErrorMsg(Format(_('Error during disassembling of %s'), [Str]));
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFJava.MIDissasemblerClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  with ODOpen do
  begin
    if Assigned(FActiveTDIChild) then
      InitialDir := ExtractFilePath(FActiveTDIChild.Pathname)
    else
      InitialDir := FConfiguration.Sourcepath;
    FileName := '*.class';
    if Execute then
    begin
      FConfiguration.Sourcepath := ExtractFilePath(FileName);
      DoDisassemble(FileName);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJavaDocClick(Sender: TObject);
var
  Dir, AFile, Files, Parameter, Package: string;
  Editform: TFEditForm;
begin
  if not Assigned(FEditorForm) then
    Exit;
  DisableUpdateMenuItems;
  Files := '';
  for var I := 0 to TDIEditFormCount - 1 do
  begin
    Editform := TDIEditFormGet(I);
    if Editform.IsJava then
    begin
      if Editform.Modified then
        Editform.Save(WithoutBackup);
      Files := Files + ' ' + HideBlanks(Editform.Pathname);
    end;
  end;

  AFile := FEditorForm.Pathname;
  Package := FEditorForm.GetPackage;
  Parameter := ' -classpath ' + FConfiguration.GetClassPath(AFile, Package);
  Parameter := Parameter + ' ' + FConfiguration.JavaDocParameter;
  if (FEditorForm.Encoding <> 'ANSI') and (Pos('encoding', Parameter) = 0) then
    if FEditorForm.Encoding = 'UTF-8' then
      Parameter := Parameter + ' -encoding UTF-8'
    else
      Parameter := Parameter + ' -encoding UTF-16';
  Dir := ExtractFilePath(AFile) + 'Doc\';
  SysUtils.ForceDirectories(Dir);

  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  FMessages.OutputLineTo(K_Messages, FConfiguration.JavaDoc + ' ' + Parameter +
    ' ' + Files);
  Screen.Cursor := crHourGlass;
  if MyJavaCommands.ExecAndWait(FConfiguration.JavaDoc, Parameter + ' ' + Files,
    Dir, FConfiguration.TempDir + 'error.txt', SW_HIDE) then
  begin
    FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
    if FileExists(Dir + 'Index.html') then
      CallHelp(Dir + 'Index.html')
    else
      FMessages.StatusMessage(_('Error during creation of documentation.'));
  end;
  Screen.Cursor := crDefault;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJavaResetClick(Sender: TObject);
begin
  GetComJava.JavaReset;
end;

procedure TFJava.MIJUnitCreateTestclassClick(Sender: TObject);
var
  AClassname, FileName: string;
  StringList: TStringList;
begin
  DisableUpdateMenuItems;
  if Assigned(FEditorForm) then
  begin
    AClassname := ChangeFileExt(ExtractFileName(FEditorForm.Pathname),
      '') + 'Test';
    FileName := ExtractFilePath(FEditorForm.Pathname) + AClassname + '.java';
    if FileExists(FileName) and
      (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
      mbYesNoCancel, 0) = mrYes) or not FileExists(FileName) then
    begin
      StringList := TStringList.Create;
      StringList.Text := FTemplates.GetTemplate(AClassname, 12);
      if StringList.Text = '' then
        StringList.Text := FTemplates.GetTestClassCode(AClassname);
      StringList.SaveToFile(FileName);
      Open(FileName);
      FEditorForm.ParseSourceCode(True);
      UpdateMenuItems(Self);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJunitManualClick(Sender: TObject);
begin
  if FConfiguration.JUnitOk then
    if FConfiguration.JUnitManual = '' then
      InformationMsg(_('The JUnit manual is not installed.'))
    else
      CallHelp(FConfiguration.JUnitManual + '?overview-summary.html');
end;

procedure TFJava.MIJUnitRunAllTestsClick(Sender: TObject);
begin
  if Assigned(FEditorForm) then
    FEditorForm.RunTests;
end;

procedure TFJava.JarCall(const Call, Dir: string);
begin
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  FMessages.OutputLineTo(K_Interpreter, _('Jar call') + ': ' +
    HideBlanks(FConfiguration.JavaJar) + ' ' + Call);
  Screen.Cursor := crHourGlass;
  if MyJavaCommands.ExecAndWait(FConfiguration.JavaJar, Call, Dir,
    FConfiguration.TempDir + 'error.txt', SW_HIDE) then
    FMessages.ShowInterpreter(FConfiguration.TempDir + 'error.txt');
  Screen.Cursor := crDefault;
end;

procedure TFJava.JarOpen(const FileName: string);
var
  StringList: TStrings;
  Posi, Count: Integer;
  Str, Ext, Dir: string;
begin
  DisableUpdateMenuItems;
  Dir := ExtractFilePath(FileName);
  JarCall('xvf ' + HideBlanks(FileName), Dir);
  Screen.Cursor := crHourGlass;
  StringList := FMessages.MInterpreter.Lines;
  Count := 0;
  for var I := 1 to StringList.Count - 1 do
  begin
    Str := StringList[I];
    // extracted:
    Posi := Pos(': ', Str);
    if Posi > 0 then
    begin
      Delete(Str, 1, Posi + 1);
      Ext := ExtractFileExt(Str);
      if Pos(Ext, '.java;.class;.jfm;.uml;.html;.htm;.jsp;.txt') > 0 then
      begin
        Open(ExpandFileName(Dir + ToWindows(Str)));
        Inc(Count);
        if Count = 20 then
        begin
          ShowMessage(_('Too much files to open.'));
          Break;
        end;
      end;
    end;
  end;
  Screen.Cursor := crDefault;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJarCreateClick(Sender: TObject);
var
  FileName, Package: string;
  Editform: TFEditForm;
begin
  FileName := '';
  Editform := GetEditFormWithMain;
  if Assigned(Editform) then
  begin
    FileName := Editform.Pathname;
    Package := Editform.GetPackage;
  end
  else if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
  begin
    FileName := (FActiveTDIChild as TFUMLForm).GetFileWithMain;
    Package := '';
  end;
  if FileName <> '' then
    DoJarCreate(FileName, Package)
  else
    ErrorMsg('no main method found');
end;

procedure TFJava.DoJarCreate(FileName, Package: string);
var
  Dir, Call, JarName, MyJavaJarManifest, JavaJarParameter, cp, MainClass, FName,
    Images, JarCreate: string;
  ParM, ParF: Integer;
  FStream: TFileStream;
begin
  DisableUpdateMenuItems;
  Dir := ExtractFilePath(FileName);
  FName := ExtractFileName(FileName);
  Package := ReplaceStr(Package, '.', '\');
  if (Package <> '') and EndsWith(Dir, Package + '\') then
  begin
    Delete(Dir, Length(Dir) - Length(Package), Length(Package) + 1);
    FileName := Dir + FName;
  end;

  cp := FConfiguration.JarClassPath;
  if (Pos('JEClasses.jar', cp) > 0) and not FileExists(Dir + 'JEClasses.jar')
  then
    CopyFile(PChar(FConfiguration.EditorFolder + 'JEClasses.jar'),
      PChar(Dir + 'JEClasses.jar'), False);

  JarName := ExtractFileName(ChangeFileExt(FileName, '.jar'));

  MyJavaJarManifest := Trim(FConfiguration.JavaJarManifest);
  if MyJavaJarManifest = '' then
  begin
    MainClass := ChangeFileExt(ExtractFileName(FileName), '');
    if Package <> '' then
      MainClass := Package + '.' + MainClass;

    MyJavaJarManifest := FConfiguration.TempDir + 'MANIFEST.MF';
    DeleteFile(MyJavaJarManifest);
    FStream := TFileStream.Create(MyJavaJarManifest,
      fmCreate or fmShareExclusive);
    StreamWriteln(FStream, 'Class-Path: ' + cp);
    StreamWriteln(FStream, 'Main-Class: ' + MainClass);
    FreeAndNil(FStream);
  end;

  JavaJarParameter := FConfiguration.JavaJarParameter;
  ParM := Pos('m', JavaJarParameter);
  if (ParM = 0) and (MyJavaJarManifest <> '') then
  begin
    JavaJarParameter := JavaJarParameter + 'm';
    ParM := Pos('m', JavaJarParameter);
  end;

  MyJavaJarManifest := HideBlanks(MyJavaJarManifest);
  ParF := Pos('f', JavaJarParameter);
  if ParM > 0 then
    if ParM > ParF then
      Call := JavaJarParameter + ' ' + JarName + ' ' + MyJavaJarManifest
    else
      Call := JavaJarParameter + ' ' + MyJavaJarManifest + ' ' + JarName
  else
    Call := JavaJarParameter + ' ' + JarName;

  JarCreate := FConfiguration.JarCreateCurrent;
  if Pos('Images', JarCreate) = 0 then
  begin
    Images := ExtractFilePath(FileName) + 'Images';
    if SysUtils.DirectoryExists(Images) then
      JarCreate := JarCreate + ' Images';
  end;

  if FileExists(Dir + 'JEClasses.jar') and (Pos('JEClasses.jar', JarCreate) = 0)
  then
    JarCreate := JarCreate + ' JEClasses.jar';

  Call := Call + ' ' + JarCreate;
  JarCall(Call, Dir);
  EnableUpdateMenuItems;
end;

procedure TFJava.DoJarCreateEV3(const FileName: string);
var
  Dir, Call, JarName, MyJavaJarManifest, JavaJarParameter, AClassname: string;
  FStream: TFileStream;
begin
  Dir := ExtractFilePath(FileName);
  AClassname := ChangeFileExt(ExtractFileName(FileName), '');
  JarName := AClassname + '.jar';
  MyJavaJarManifest := FConfiguration.TempDir + 'MANIFEST.MF';
  DeleteFile(MyJavaJarManifest);
  FStream := TFileStream.Create(MyJavaJarManifest,
    fmCreate or fmShareExclusive);
  StreamWriteln(FStream, 'Main-Class: ' + AClassname);
  FreeAndNil(FStream);

  JavaJarParameter := '-cfm';
  MyJavaJarManifest := HideBlanks(MyJavaJarManifest);
  Call := JavaJarParameter + ' ' + JarName + ' ' + MyJavaJarManifest;
  Call := Call + ' ' + FConfiguration.JarCreateCurrent;
  JarCall(Call, Dir);
end;

procedure TFJava.MIJarPackClick(Sender: TObject);
var
  Dir, AFile, Files, Call, JarName, Str: string;
  Form: TFForm;
begin
  DisableUpdateMenuItems;
  if Assigned(FActiveTDIChild) then
    AFile := FActiveTDIChild.Pathname
  else
    Exit;
  Dir := ExtractFilePathEx(AFile);
  JarName := HideBlanks(ChangeFileExt(ExtractFileName(AFile), '.jar'));
  if FConfiguration.JarPackFiles = FConfiguration.CBJarPack.Items[0] then
  begin
    Files := '';
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
    begin
      Form := GetTabForm(I);
      if Form.FormTag in [1, 2, 3] then
      begin
        Str := Form.Pathname;
        Str := ExtractRelativePath(Dir, Str);
        Files := Files + ' ' + HideBlanks(Str);
      end;
    end;
  end
  else
    Files := FConfiguration.JarPackFiles;
  Call := 'cvMf ' + JarName + ' ' + Files;
  JarCall(Call, Dir);
  EnableUpdateMenuItems;
end;

procedure TFJava.JarShowUnpackOpen(Num: Integer; const FileName: string);

  function OverwriteOK: Boolean;
  begin
    Result := True;
    for var I := 1 to FMessages.LBMessages.Items.Count - 1 do
    begin
      var
      Str := FMessages.LBMessages.Items[I];
      Delete(Str, 1, Pos(':', Str));
      Delete(Str, 1, Pos(':', Str));
      Delete(Str, 1, Pos(' ', Str));
      Delete(Str, 1, Pos(' ', Str));
      Delete(Str, 1, Pos(' ', Str));
      Str := ExpandFileName(ToWindows(Str));
      if FileExists(Str) then
      begin
        Result := MessageDlg(_('Overwrite with jar files?'), mtConfirmation,
          mbYesNoCancel, 0) = mrYes;
        Exit;
      end;
    end;
  end;

begin
  try
    var
    Path := ExtractFilePath(FileName);
    JarCall('tvf ' + HideBlanks(FileName), Path);
    if (Num > 1) and OverwriteOK then
      case Num of
        2:
          JarCall('xvf ' + HideBlanks(FileName), Path);
        3:
          JarOpen(FileName);
      end;
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
end;

procedure TFJava.MIJarClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var
  I := (Sender as TSpTBXItem).Tag;
  with ODOpen do
  begin
    if Assigned(ActiveTDIChild) then
      InitialDir := ExtractFilePath(ActiveTDIChild.Pathname)
    else
      InitialDir := FConfiguration.Sourcepath;
    case I of
      1:
        Title := _('Show content of jar file');
      2:
        Title := _('Unpack jar file');
      3:
        Title := _('Open jar file');
    end;
    var
    Tmp := FilterIndex;
    FilterIndex := 9;
    FileName := '*.jar';
    if Execute then
    begin
      FConfiguration.Sourcepath := ExtractFilePath(FileName);
      JarShowUnpackOpen(I, FileName);
    end;
    FilterIndex := Tmp;
  end;
  EnableUpdateMenuItems;
end;

{ --- Test menu ---------------------------------------------------------------- }

procedure TFJava.PrepareStep;
begin
  if Assigned(MyJavaCommands) and Assigned(MyJavaCommands.Editform) then
    MyJavaCommands.Editform.DeleteDebuglineMark;
end;

procedure TFJava.StepNextUp(const Str: string);
begin
  if FConfiguration.MindstormsMode then
    ShowMessage(_(LNGMindstormsPrograms))
  else if MyDebugger.Running then
  begin
    PrepareStep;
    MyDebugger.NewCommand(3, Str);
  end
  else
  begin
    if Assigned(FEditorForm) and not FEditorForm.HasBreakpoints then
      MyDebugger.BreakpointAtMain := True;
    MIRunClick(Self);
  end;
end;

procedure TFJava.MIStepClick(Sender: TObject);
begin
  StepNextUp('step');
end;

procedure TFJava.MINextClick(Sender: TObject);
begin
  StepNextUp('next');
end;

procedure TFJava.MIStepUpClick(Sender: TObject);
begin
  StepNextUp('step up');
end;

procedure TFJava.MIBreakpointClick(Sender: TObject);
begin
  if Assigned(FEditorForm) then
    FEditorForm.InsertBreakpoint;
end;

procedure TFJava.MIBreakpointsClearClick(Sender: TObject);
begin
  for var I := 0 to TDIEditFormCount - 1 do
    TDIEditFormGet(I).ClearBreakpoints;
end;

{ --- Window menu ------------------------------------------------------------- }

procedure TFJava.MIConfigurationClick(Sender: TObject);
begin
  // due to visible/modal-exception
  if not FConfiguration.Visible then
  begin
    FConfiguration.PrepareShow;
    FConfiguration.ShowModal;
    StyleTimer.Enabled := True;
  end;
end;

procedure TFJava.StyleTimerTimer(Sender: TObject);
begin
  // timer using because of unclear exception
  StyleTimer.Enabled := False;
  ChangeStyle(FConfiguration.GUIStyle);
end;

procedure TFJava.MIConfigureToolsClick(Sender: TObject);
begin
  with TFConfigureTools.Create(Self) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TFJava.MIAboutClick(Sender: TObject);
begin
  with TFAbout.Create(Self) do
  begin
    ShowModal;
    Free;
  end;
end;

function TFJava.NewBrowser(Address: string; const State: string): TFBrowser;
begin
  if IsHTTP(Address) and FConfiguration.BlockedInternet then
    Address := 'about:' + _(LNGBlockedInternet);
  if IsHTTP(Address) then
    Address := HttpToWeb(Address);
  if FConfiguration.OnlyOneBrowserWindow then
  begin
    Result := (GetTDIType('%B%') as TFBrowser);
    if Assigned(Result) then
    begin
      Result.WebBrowser.Navigate(Address);
      SwitchToWindow(Result.Number);
    end
    else
      Result := TFBrowser(FormFactory(fkBrowser));
    Result.Open(Address, State);
  end
  else
  begin
    Result := TFBrowser(GetTDIWindowType(Address, '%B%'));
    if Assigned(Result) then
      Result.OpenWindow(Self)
    else
    begin
      Result := TFBrowser(FormFactory(fkBrowser));
      Result.Open(Address, State);
    end;
  end;
end;

procedure TFJava.NewEditor(const Path: string; State: string);
begin
  var
  Editor := TFEditForm(GetTDIWindowType(Path, '%E%'));
  if Assigned(Editor) then
  begin
    SetSelectedTabAndWindow(Editor.Pathname);
    MyTabBarClick(Self);
    Editor.SetState(State);
  end
  else
  begin
    Editor := TFEditForm(FormFactory(fkEditor));
    Editor.Open(Path, State);
  end;
end;

procedure TFJava.NewExplorer(const Dir: string; State: string);
begin
  var
  Explorer := TFExplorer(GetTDIWindowType(Dir, '%X%'));
  if Assigned(Explorer) then
  begin
    SetSelectedTabAndWindow(Explorer.Pathname);
    MyTabBarClick(Self);
    Explorer.SetState(State);
  end
  else
  begin
    Explorer := TFExplorer(FormFactory(fkExplorer));
    Explorer.New(ExcludeTrailingPathDelimiter(Dir), State);
  end;
end;

procedure TFJava.NewTextDiff(Edit1, Edit2: TFEditForm);
begin
  var
  TextDiff := TFTextDiff(FormFactory(fkTextDiff));
  TextDiff.New(Edit1, Edit2);
  SetSelectedTabAndWindow(TextDiff.Number);
end;

procedure TFJava.NewGUIForm(const AFile: string; State: string);
var
  AForm: TFForm;
begin
  if WindowOpened(AFile, AForm) then
    AForm.OpenWindow(Self)
  else
  begin
    var
    Str := ChangeFileExt(AFile, '.java');
    if FileExists(Str) then
    begin
      if not WindowOpened(Str, AForm) then
        Open(Str);
      var
      GUIForm := FGUIDesigner.Open(AFile, Str);
      if Assigned(GUIForm) then
        GUIForm.SetState(State);
    end
    else
      ErrorMsg(Format(_(LNGAssociatedJavaFileNotFound), [AFile]));
  end;
end;

procedure TFJava.NewUMLWindow(const AFile, State: string);
var
  AForm: TFForm;
begin
  if WindowOpened(AFile, AForm) then
    AForm.OpenWindow(Self)
  else if FileExists(AFile) then
    OpenUMLWindow(AFile, State);
end;

function TFJava.MakeNewUMLWindow(const FileName, State: string): TFUMLForm;
begin
  DisableUpdateMenuItems;
  var
  UMLForm := TFUMLForm(FormFactory(fkUML));
  UMLForm.Open(FileName, State);
  if FileName = _(LNGInteractive) then
    FTDIFormsList.Remove(UMLForm)
  else
    UMLForm.Visible := True;
  Result := UMLForm;
  EnableUpdateMenuItems;
end;

function TFJava.OpenStructogram(const FileName, State: string): Boolean;
var
  AForm: TFForm;
begin
  Result := False;
  if WindowOpened(FileName, AForm) then
  begin
    AForm.OpenWindow(Self);
    Result := True;
  end
  else if FileExists(FileName) then
    Result := NewStructogram(FileName, State);
end;

function TFJava.OpenSequenceDiagram(const FileName, State: string): Boolean;
var
  AForm: TFForm;
begin
  Result := False;
  if WindowOpened(FileName, AForm) then
  begin
    AForm.OpenWindow(Self);
    Result := True;
  end
  else if FileExists(FileName) then
    Result := NewSequenceDiagram(FileName, State);
end;

procedure TFJava.CallHelp(const Address: string);
begin
  CallHelp(False, Address);
end;

procedure TFJava.CallApplet(const Address: string);
begin
  CallHelp(True, Address);
end;

procedure TFJava.CallHelp(Applet: Boolean; Address: string);
begin
  if IsHTTP(Address) and FConfiguration.BlockedInternet then
    Address := 'about:' + _(LNGBlockedInternet);
  if IsCHM(Address) then
    Address := FConfiguration.GetCHMJavaManual(Address);
  if not Applet and FConfiguration.UseIEinternForDocuments then
  begin
    NewBrowser(Address, '');
    Exit;
  end;
  Address := ToWeb(FConfiguration.BrowserProgram, Address);
  var
  Window := FWindow.GetWindowHandle(FConfiguration.BrowserTitle);
  if Window = 0 then
    if FileExists(FConfiguration.BrowserProgram) then
      MyJavaCommands.ExecWithoutWait(FConfiguration.BrowserProgram, Address, '',
        SW_SHOWNORMAL)
    else if FConfiguration.BrowserProgram = '' then
      ErrorMsg(_('No browser configured'))
    else
      ErrorMsg(FConfiguration.BrowserProgram + ' ' + _(LNGDoesNotExist))
  else
  begin
    if IsIconic(Window) then
      ShowWindow(Window, SW_SHOWNORMAL)
    else
      SetForegroundWindow(Window);
    Sleep(100);
    SendShortCutStringGlobal(FConfiguration.BrowserOpenKeys);
    Sleep(100);
    SendKeysGlobal(Address + #13);
  end;
end;

{ --- Help menu ---------------------------------------------------------------- }

procedure TFJava.MIHelpHelpClick(Sender: TObject);
begin
  if IsCHM(FConfiguration.JavaManual) then
    if FConfiguration.CHMRootOk then
      MyJavaCommands.ShellExecuteFile(FConfiguration.JavaManual, '', '',
        SW_SHOWNORMAL)
    else
      ErrorMsg('cannot read: ' + FConfiguration.JavaManual)
  else
  begin
    var
    Manual := FConfiguration.JavaManual;
    if FConfiguration.GlobalFileExists(Manual) then
    begin
      FConfiguration.JavaManual := Manual;
      with TFHelpDialog.Create(Self) do
        Show;
    end
    else
      ErrorMsg(Format(_(LNGFileNotFound), [FConfiguration.JavaManual]));
  end;
end;

procedure TFJava.MIAPIClick(Sender: TObject);
begin
  if IsCHM(FConfiguration.JavaManual) and not FConfiguration.CHMRootOk then
    ErrorMsg('cannot read: ' + FConfiguration.JavaManual)
  else
  begin
    var
    Manual := FConfiguration.JavaManual;
    if FConfiguration.GlobalFileExists(Manual) then
    begin
      FConfiguration.JavaManual := Manual;
      if Assigned(FEditorForm) then
        SearchInIndex
      else
        CallHelp(FConfiguration.JavaManual);
    end
    else
      ErrorMsg(Format(_(LNGFileNotFound), [FConfiguration.JavaManual]));
  end;
end;

procedure TFJava.MIJavaFxClick(Sender: TObject);
begin
  CallHelp(FConfiguration.JavaManualFX);
end;

procedure TFJava.MIDemosClick(Sender: TObject);
begin
  CallHelp(FConfiguration.JavaDemos);
end;

procedure TFJava.MITutorialClick(Sender: TObject);
begin
  var
  Tutorial := FConfiguration.JavaTutorial;
  if FConfiguration.GlobalFileExists(Tutorial) then
  begin
    FConfiguration.JavaTutorial := Tutorial;
    if IsCHM(FConfiguration.JavaTutorial) then
      MyJavaCommands.ShellExecuteFile(FConfiguration.JavaTutorial, '', '',
        SW_SHOWNORMAL)
    else
      CallHelp(FConfiguration.JavaTutorial);
  end
  else
    InformationMsg(Format(_('The tutorial %s is not installed.'),
      [FConfiguration.JavaTutorial]));
end;

procedure TFJava.MIJavabookClick(Sender: TObject);
begin
  var
  AFile := FConfiguration.Javabook;
  if FileExists(AFile) or IsHTTP(AFile) then
    CallHelp(AFile)
  else
    InformationMsg(_('The Java book from www.javabuch.de is not installed.'));
end;

procedure TFJava.MIMindstormsHelpClick(Sender: TObject);
begin
  var
  AFile := FConfiguration.MindstormsManual;
  if FileExists(AFile) or IsHTTP(AFile) then
    CallHelp(FConfiguration.MindstormsManual)
  else
    InformationMsg
      (_('The mindstorms manual from https://lejos.sourceforge.net/ is not installed.')
      );
end;

{ --- control structures ------------------------------------------------------- }

procedure TFJava.SBKontrollstrukturenClick(Sender: TObject);
begin
  if Assigned(FEditorForm) then
    FTemplates.SBControlStructures(FEditorForm, TSpTBXItem(Sender).Tag);
end;

{ --- Program ------------------------------------------------------------------ }

function TFJava.NewEditform(Hidden: Boolean): TFEditForm;
begin
  var
  Child := TFEditForm(FormFactory(fkEditor));
  EditorSaveAs(Child, Hidden);
  if Child.Pathname = '' then
  begin // save as aborted?
    Child.Close;
    Result := nil;
  end
  else
  begin
    Result := Child;
    Child.New(Child.Pathname);
  end;
end;

procedure TFJava.SBProgramClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag in [1, 2]) then
    FConfiguration.Sourcepath := ExtractFilePath(FActiveTDIChild.Pathname);
  var
  JavaFormular := NewEditform(False);
  if Assigned(JavaFormular) then
    FTemplates.SBProgram(JavaFormular);
  EnableUpdateMenuItems;
end;

procedure TFJava.SBGUIFrameClick(Sender: TObject);
var
  JavaFormular: TFEditForm;
  GUIForm: TFGUIForm;
  UMLFenster: TFUMLForm;
  FileName: string;
  Point: TPoint;
begin
  DisableUpdateMenuItems;
  if (TComponent(Sender).Tag = 8) and (FConfiguration.GetJavaVersion < 8) then
  begin
    ErrorMsg(_('JavaFX needs Java JDK 8 or higher'));
    Exit;
  end;

  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag in [1, 2]) then
    FConfiguration.Sourcepath := ExtractFilePath(FActiveTDIChild.Pathname);
  JavaFormular := NewEditform(False);
  if Assigned(JavaFormular) then
  begin
    FTemplates.FrameDialogApplet(JavaFormular, TComponent(Sender).Tag);
    Point := JavaFormular.GetWidthAndHeight;
    UMLFenster := GetUMLWindow;
    if Assigned(UMLFenster) then
      UMLFenster.MainModul.AddToProject(JavaFormular.Pathname);
    FileName := ChangeFileExt(JavaFormular.Pathname, '.jfm');
    if TComponent(Sender).Tag = 8 then
      GUIForm := TFXGUIForm(FormFactory(fkFXGUI))
    else
      GUIForm := TFGUIForm(FormFactory(fkGUI));
    with GUIForm do
      SetBounds(Left, Top, PPIScale(Point.X), PPIScale(Point.Y));
    GUIForm.Open(FileName, '');
    DoSave(JavaFormular, WithoutBackup);
  end;
  EnableUpdateMenuItems;
end;

function TFJava.GuiDesignerOpen: Boolean;
begin
  Result := True;
  FActiveTDIChild := GetActiveForm;
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 1) and
    Assigned((FActiveTDIChild as TFEditForm).Partner) then
    Exit;

  FEditorForm := GetActiveEditor;
  if not Assigned(FEditorForm) and Assigned(FGUIDesigner.DesignForm) then
    FEditorForm := TFEditForm(FGUIDesigner.DesignForm.Partner);
  if Assigned(FEditorForm) and FEditorForm.IsJava then
  begin
    if not Assigned(FEditorForm.Partner) then
    begin
      FEditorForm.SBDesignformClick(nil);
      if not Assigned(FEditorForm.Partner) then
      begin
        ErrorMsg(Format(_(LNGFileNotFound), [ChangeFileExt(FEditorForm.Pathname,
          '.jfm')]));
        Result := False;
      end;
    end;
  end
  else
    Result := False;
end;

{ --- AWT-Swing-Toolbar -------------------------------------------------------- }

procedure TFJava.MISwingClick(Sender: TObject);
begin
  if GuiDesignerOpen then
    FGUIDesigner.MIAWTSwing(TSpTBXItem(Sender).Tag);
end;

procedure TFJava.TBSwingClick(Sender: TObject);
begin
  if GuiDesignerOpen then
    FGUIDesigner.TBAWTSwing(TToolButton(Sender).Tag);
end;

procedure TFJava.TBJavaFXClick(Sender: TObject);
begin
  if GuiDesignerOpen then
  begin
    FGUIDesigner.TBJavaFX(TToolButton(Sender).Tag);
    if not FObjectInspector.Visible then
      FObjectInspector.ShowIt;
  end;
end;

procedure TFJava.ToolbuttonMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FGUIDesigner.DesignForm) and (Button = mbLeft) then
    (Sender as TToolButton).BeginDrag(False, 10);
end;

type
  TControlEx = class(TControl)
  protected
    FFont: TFont;
  end;

procedure TFJava.ToolbuttonStartDrag(Sender: TObject;
var DragObject: TDragObject);
begin
  var
  ControlClass := FGUIDesigner.Tag2Class((Sender as TToolButton).Tag);
  var
  DragRectangle := ControlClass.Create(FGUIDesigner.DesignForm);
  DragRectangle.Parent := FGUIDesigner;
  DragRectangle.ScaleForPPI(FGUIDesigner.DesignForm.PixelsPerInch);
  TControlEx(DragRectangle).Font.Size := FGUIDesigner.DesignForm.FontSize;
  DragObject := TMyDragObject.Create(DragRectangle);
end;

procedure TFJava.ResetToolbars;
begin
  for var I := 0 to ToolbarAWT.ButtonCount - 1 do
    ToolbarAWT.Buttons[I].Down := False;
  for var I := 0 to ToolBarSwing1.ButtonCount - 1 do
    ToolBarSwing1.Buttons[I].Down := False;
  for var I := 0 to ToolbarSwing2.ButtonCount - 1 do
    ToolbarSwing2.Buttons[I].Down := False;
  for var I := 0 to ToolBarLayout.ButtonCount - 1 do
    ToolBarLayout.Buttons[I].Down := False;
  for var I := 0 to ToolBarUtilities.ButtonCount - 1 do
    ToolBarUtilities.Buttons[I].Down := False;
  for var I := 0 to ToolBarFXBase.ButtonCount - 1 do
    ToolBarFXBase.Buttons[I].Down := False;
  for var I := 0 to ToolBarFXControls.ButtonCount - 1 do
    ToolBarFXControls.Buttons[I].Down := False;
  for var I := 0 to ToolBarFXShapes.ButtonCount - 1 do
    ToolBarFXShapes.Buttons[I].Down := False;
end;

procedure TFJava.TBPanelCanvasMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    var
    TagNr := TToolButton(Sender).Tag;
    with ODOpen do
    begin
      if Assigned(FActiveTDIChild) then
        InitialDir := ExtractFilePath(FActiveTDIChild.Pathname)
      else
        InitialDir := FConfiguration.Sourcepath;
      if Abs(TagNr) = 12 then
        Title := _(LNGOpenSubClass) + ' Panel'
      else
        Title := _(LNGOpenSubClass) + ' Canvas';
      FileName := '*.java;*.class';
      FilterIndex := 1;
      case TagNr of
        - 12:
          TagNr := -38;
        12:
          TagNr := 38;
        -13:
          TagNr := -39;
        114:
          TagNr := 122;
      end;
      if Execute then
        FGUIDesigner.DoPanelCanvas(TagNr, ExtractFileName(FileName));
    end;
  end;
end;

{ --- Layout bar ------------------------------------------------------------ }

procedure TFJava.TBLayoutClick(Sender: TObject);

  procedure TBLayout(ATag: Integer);
  begin
    var
    Str := '';
    case ATag of
      0:
        Str := 'Border';
      1:
        Str := 'Flow';
      2:
        Str := 'Grid';
      3:
        Str := 'Card';
      4:
        Str := 'GridBag';
      5:
        Str := 'null';
    end;
    if ATag < 5 then
      Str := 'setLayout(new ' + Str + 'Layout());'
    else
      Str := 'setLayout(null);';
    with FEditorForm do
    begin
      Str := Str + CrLf + GetIndent + '|';
      PutText(Str);
    end;
  end;

begin
  if Assigned(FEditorForm) then
    if (Sender is TSpeedButton) then
      TBLayout(TSpeedButton(Sender).Tag)
    else
      TBLayout(TSpTBXItem(Sender).Tag);
end;

procedure TFJava.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;
  DroppedFileCount: Integer;
  FileNameLength: Integer;
  FileName: string;
begin
  inherited;
  LockFormUpdate(Self);
  // Store drop handle from the message
  DropH := Msg.Drop;
  try
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    for var I := 0 to Pred(DroppedFileCount) do
    begin
      FileNameLength := DragQueryFile(DropH, I, nil, 0);
      SetLength(FileName, FileNameLength);
      DragQueryFile(DropH, I, PChar(FileName), FileNameLength + 1);
      if Open(FileName) then
      begin
        RearrangeFileHistory(FileName);
        FConfiguration.Sourcepath := ExtractFilePath(FileName);
      end;
    end;
  finally
    DragFinish(DropH);
  end;
  Msg.Result := 0;
  UnlockFormUpdate(Self);
end;

procedure TFJava.SystemExecuteMacro(Sender: TObject; Msg: TStrings);
var
  FileName: string;
begin
  // loads selected files from windows explorer
  // onExecuteMacro of System: TDdeServerConv component
  LockFormUpdate(Self);
  for var I := 0 to Msg.Count - 1 do
  begin
    FileName := Msg[I];
    if LeftStr(FileName, 11) = '[FileOpen("' then
    begin
      FileName := Copy(FileName, 12, Length(FileName) - 14);
      if Open(FileName) then
      begin
        RearrangeFileHistory(FileName);
        FConfiguration.Sourcepath := ExtractFilePath(FileName);
      end;
    end;
  end;
  UnlockFormUpdate(Self);
end;

procedure TFJava.MIBrowserClick(Sender: TObject);
begin
  CallHelp('about:blank');
end;

procedure TFJava.MyTabBarClick(Sender: TObject);
begin
  SwitchToWindow(GetSelectedTabForm);
end;

procedure TFJava.MyTabBarDblClick(Sender: TObject);
begin
  var
  Form := GetSelectedTabForm;
  if Assigned(Form) then
  begin
    if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag <> 3) then
      FConfiguration.WindowStateMaximized :=
        not FConfiguration.WindowStateMaximized;
    SwitchToWindow(Form);
  end;
end;

procedure TFJava.MyTabBarClosed(Sender: TObject; Item: TJvTabBarItem);
begin
  DisableUpdateMenuItems;
  TTabObject(Item.Data).Form.Close;
  MyTabBarClick(Self);
  EnableUpdateMenuItems;
end;

procedure TFJava.MIExpressionClick(Sender: TObject);
begin
  FEvaluate := TFEvaluate.Create(Self);
  FEvaluate.Show;
end;

procedure TFJava.RunButtonToStop(Stop: Boolean);
begin
  if Stop then
  begin
    SetEnabledMI(MIRun, False);
    SetEnabledMI(MIProgramReset, True);
    TBRun.Hint := _(LNGResetProgram);
    TBRun.ImageIndex := 14;
  end
  else
  begin
    SetEnabledMI(MIRun, True);
    SetEnabledMI(MIProgramReset, False);
    if Assigned(FConfiguration) and FConfiguration.AndroidMode then
    begin
      MIRun.ImageIndex := 87;
      MIRun.Caption := _(LNGTransferToAndroid);
      TBRun.Hint := _(LNGTransferToAndroid);
      TBRun.ImageIndex := 15;
    end
    else
    begin
      MIRun.ImageIndex := 22;
      MIRun.Caption := _(LNGRunApplication);
      TBRun.Hint := _('Run');
      TBRun.ImageIndex := 13;
    end;
  end;
  UpdateMenuItems(nil);
end;

procedure TFJava.CompileButtonToStop(OnOff: Boolean);
begin
  if OnOff then
  begin
    MICompile.Enabled := False;
    MIProgramReset.Enabled := True;
    TBCompileJava.Hint := _(LNGResetProgram);
    TBCompileJava.ImageIndex := 14;
  end
  else
  begin
    MICompile.Enabled := True;
    MIProgramReset.Enabled := False;
    if FConfiguration.MindstormsMode then
      TBCompileJava.Hint := _(LNGCompileForMindstorms)
    else
      TBCompileJava.Hint := _(LNGCompileWithJava);
    TBCompileJava.ImageIndex := 11;
  end;
  UpdateMenuItems(nil);
end;

{ ---Docking ------------------------------------------------------------------- }

procedure TFJava.RightDockPanelUnDock(Sender: TObject; Client: TControl;
NewTarget: TWinControl; var Allow: Boolean);
begin
  if Assigned(FConfiguration) then
  begin
    FConfiguration.RightDockPanelWidth := RightDockPanel.Width;
    // OnUnDock gets called BEFORE the client is undocked, in order to optionally
    // disallow the undock. DockClientCount is never 0 when called from this event.
    if (Sender as TPanel).DockClientCount = 1 then
      ShowDockPanel(Sender as TPanel, False, nil);
  end;
end;

procedure TFJava.BottomDockPanelUnDock(Sender: TObject; Client: TControl;
NewTarget: TWinControl; var Allow: Boolean);
begin
  // When undocking in ShellFolder view, the window
  // is incorrectly positioned
  if FMessages.TabControlMessages.TabIndex = 0 then
  begin
    FMessages.Undocking := True;
    FMessages.ShowTab(K_Compiler);
  end;

  FConfiguration.BottomDockPanelHeight := BottomDockPanel.Height;
  // OnUnDock gets called BEFORE the client is undocked, in order to optionally
  // disallow the undock. DockClientCount is never 0 when called from this event.
  if (Sender as TPanel).DockClientCount = 1 then
    ShowDockPanel(Sender as TPanel, False, nil);
end;

{ --- preview DockPanel dock area ---------------------------------------------- }

procedure TFJava.BottomDockPanelDockOver(Sender: TObject;
Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := Source.Control is TFMessages;
  if Accept then
  begin
    // Modify the DockRect to preview dock area.
    // ARect.TopLeft := StatusBar.ClientToScreen(Point(0, -FConfiguration.BottomDockPanelHeight));
    // ARect.BottomRight := StatusBar.ClientToScreen(Point(StatusBar.Width, 0));
    // TODO without StatusBAR????????
    ARect.TopLeft := ClientToScreen
      (Point(0, -FConfiguration.BottomDockPanelHeight));
    ARect.BottomRight := ClientToScreen(Point(HSplitter.Width, 0));

    // ARect.TopLeft := BottomDockPanel.ClientToScreen( Point(0, -Self.ClientHeight div 3));
    // ARect.BottomRight := BottomDockPanel.ClientToScreen( Point(BottomDockPanel.Width, BottomDockPanel.Height));
    Source.DockRect := ARect;
  end;
end;

procedure TFJava.BottomDockPanelStartDock(Sender: TObject;
var DragObject: TDragDockObject);
begin
  DragObject := nil;
end;

procedure TFJava.RightDockPanelDockOver(Sender: TObject;
Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
  MyHeight: Integer;
begin
  Accept := (Source.Control is TFFileStructure) or
    (Source.Control is TFObjectInspector);
  if Accept then
  begin
    // Modify the DockRect to preview dock area.
    ARect.TopLeft := ClientToScreen
      (Point(ClientWidth - FConfiguration.RightDockPanelWidth,
      ControlBar.Height + FMyTabBar.Height));
    MyHeight := ClientHeight;
    if FMessages.MyIsVisible and not FMessages.Floating then
      MyHeight := MyHeight - BottomDockPanel.Height;
    ARect.BottomRight := ClientToScreen(Point(ClientWidth, MyHeight));
    Source.DockRect := ARect;
  end;
end;

procedure TFJava.RightDockPanelGetSiteInfo(Sender: TObject;
DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
var CanDock: Boolean);
begin
  // if CanDock is true, the panel will not automatically draw the preview rect.
  CanDock := DockClient is TDockableForm;
end;

procedure TFJava.BottomDockPanelGetSiteInfo(Sender: TObject;
DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
var CanDock: Boolean);
begin
  // if CanDock is true, the panel will not automatically draw the preview rect.
  CanDock := DockClient is TFMessages;
end;

{ --- Drop --------------------------------------------------------------------- }

procedure TFJava.DockPanelDockDrop(Sender: TObject; Source: TDragDockObject;
X, Y: Integer);
begin
  // OnDockDrop gets called AFTER the client has actually docked,
  // so we check for DockClientCount = 1 before making the dock panel visible.
  if (Sender as TPanel).DockClientCount = 1 then
  begin
    ShowDockPanel(Sender as TPanel, True, nil);
    // Make DockManager repaints it's clients.
    (Sender as TPanel).DockManager.ResetBounds(True);
  end;
end;

procedure TFJava.ShowDockPanel(APanel: TPanel; MakeVisible: Boolean;
Client: TControl);
begin
  // Client - the docked client to show if we are re-showing the panel.
  // Client is ignored if hiding the panel.

  // Since docking to a non-visible docksite isn't allowed, instead of setting
  // Visible for the panels we set the width to zero. The default InfluenceRect
  // for a control extends a few pixels beyond it's boundaries, so it is possible
  // to dock to zero width controls.

  // Don't try to hide a panel which has visible dock clients.
  if not MakeVisible and (APanel.VisibleDockClientCount > 1) or
    not Assigned(FConfiguration) then
    Exit;

  if APanel = RightDockPanel then
    VSplitter.Visible := MakeVisible
  else
    HSplitter.Visible := MakeVisible;

  if MakeVisible then
    if APanel = RightDockPanel then
    begin
      APanel.Visible := True;
      APanel.Width := FConfiguration.RightDockPanelWidth;
      APanel.Constraints.MinWidth := 50;
      APanel.Align := alRight;
      VSplitter.Left := 100;
      APanel.Left := 0;
    end
    else
    begin // BottomDockPanel
      APanel.Visible := True;
      APanel.Height := FConfiguration.BottomDockPanelHeight;
      APanel.Constraints.MinHeight := 100;
      APanel.Align := alBottom;
      APanel.Top := 100;
      HSplitter.Top := 0;
    end
  else if APanel = RightDockPanel then
  begin
    APanel.Constraints.MinWidth := 0;
    APanel.Width := 0;
  end
  else
  begin
    APanel.Constraints.MinHeight := 0;
    APanel.Height := 0;
  end;
  if MakeVisible and Assigned(Client) then
    Client.Show;
end;

procedure TFJava.ShowDockableForm(DockWindow: TDockableForm);
begin
  // if the docked window is TabDocked, it is docked to the PageControl
  // (owned by TTabDockHost) so show the host form.
  if DockWindow.HostDockSite is TPageControl then
    TTabDockHost(DockWindow.HostDockSite.Owner).Show
  else
    // If window is conjoin-docked, host and/or form may not be visible
    // so show both.
    if (DockWindow.HostDockSite is TConjoinDockHost) and
      not DockWindow.HostDockSite.Visible then
    begin
      DockWindow.HostDockSite.Show;
      TConjoinDockHost(DockWindow.HostDockSite).UpdateCaption(nil);
      DockWindow.Show;
    end
    else
      // If form is docked to one of the "Hidden" docking panels, resize the
      // panel and re-show the docked form.
      if (DockWindow.HostDockSite is TPanel) and
        ((DockWindow.HostDockSite.Height = 0) or
        (DockWindow.HostDockSite.Width = 0)) then
        ShowDockPanel(DockWindow.HostDockSite as TPanel, True, DockWindow)
      else
        // if the window isn't docked at all, simply show it.
        DockWindow.Show;
  VSplitterMoved(Self);
end;

procedure TFJava.MIMessagesClick(Sender: TObject);
begin
  FMessages.ChangeHideShow;
end;

procedure TFJava.AddToTabBar(Num: Integer; Form: TFForm);
begin
  var
  FileName := ExtractFileNameEx(Form.Pathname);
  if FileExists(Form.Pathname) and IsWriteProtected(Form.Pathname) then
    FileName := FileName + ' (' + _(LNGWriteProtected) + ')';
  var
  ATabBarItem := FMyTabBar.AddTab(FileName);
  ATabBarItem.Data := TTabObject.Create(Form.Pathname, Num, Form);
  ATabBarItem.Selected := True;
end;

procedure TFJava.AddToWindowMenuAndTabBar(Num: Integer;
NotifyEvent: TNotifyEvent; Form: TFForm);
begin
  var
  NewItem := TSpTBXItem.Create(Self);
  NewItem.Caption := '&' + IntToStr(FMyTabBar.Tabs.Count + 1) + ' ' +
    Form.Pathname;
  NewItem.OnClick := NotifyEvent;
  NewItem.Checked := True;
  NewItem.Tag := Num;
  MIWindow.Add(NewItem);
  AddToTabBar(Num, Form);
  MyTabBarColorize;
end;

procedure TFJava.DeleteTab(Num: Integer);
begin
  var
  I := GetTab(Num);
  if I > -1 then
  begin
    var
    ATabObject := TTabObject(FMyTabBar.Tabs[I].Data);
    FreeAndNil(ATabObject);
    FMyTabBar.Tabs.Delete(I);
  end;
end;

procedure TFJava.DeleteTabAndWindow(Num: Integer);
var
  Number: Integer;
  Str: string;
begin
  Number := GetWindowMenuNr(Num);
  if Number > -1 then
  begin
    for var J := Number + 1 to MIWindow.Count - 1 do
    begin
      Str := MIWindow[J].Caption;
      Str := '&' + IntToStr(J - WindowOffset) + Copy(Str, Pos(' ', Str),
        Length(Str));
      MIWindow[J].Caption := Str;
    end;
    MIWindow.Delete(Number);
  end;
  DeleteTab(Num);
  MyTabBarColorize;
end;

function TFJava.GetWindowMenuNr(Num: Integer): Integer;
begin
  var
  I := WindowOffset - 1;
  while (I < MIWindow.Count - 1) and (MIWindow[I].Tag <> Num) do
    Inc(I);
  if (I < MIWindow.Count) and (MIWindow[I].Tag = Num) then
    Result := I
  else
    Result := -1;
end;

procedure TFJava.RenameTabAndWindow(Num: Integer; const New: string);
begin
  var
  I := GetWindowMenuNr(Num);
  if I > -1 then
    MIWindow[I].Caption := '&' + IntToStr(I - WindowOffset - 1) + ' ' + New;
  I := GetTab(Num);
  if I > -1 then
  begin
    TTabObject(FMyTabBar.Tabs[I].Data).Path := New;
    FMyTabBar.Tabs[I].Caption := ExtractFileNameEx(New);
    FMyTabBar.Update;
  end;
end;

procedure TFJava.FormResize(Sender: TObject);
var
  AForm: TFForm;
begin
  if Assigned(FMyTabBar) then
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
    begin
      AForm := GetTabForm(I);
      if not AForm.Visible then
        Continue;
      if (AForm.FormTag = 2) and Assigned((AForm as TFUMLForm).MainModul) then
        (AForm as TFUMLForm).MainModul.Diagram.RecalcPanelSize;
    end;
  UpdateLayoutRightDockPanel(False);
end;

procedure TFJava.MICommentOnOffClick(Sender: TObject);
begin
  FEditorForm.SBCommentClick(Self);
end;

procedure TFJava.MIWebsiteClick(Sender: TObject);
var
  Str: string;
begin
  if FConfiguration.IsGerman then
    Str := Homepage + '/doku.php?id=de:java-editor'
  else
    Str := Homepage + '/doku.php?id=start';
  CallHelp(False, Str);
end;

procedure TFJava.MIChatClick(Sender: TObject);
begin
  FLLMChatForm.Show;
end;

procedure TFJava.MICheckstyleClick(Sender: TObject);
var
  Dir, AFile, Call, ErrFile: string;
begin
  AFile := ExtractFileName(FEditorForm.Pathname);
  Dir := ExtractFilePath(FEditorForm.Pathname);
  FMessages.ShowIt;
  FMessages.DeleteTab(K_Messages);
  FMessages.ShowTab(K_Messages);
  try
    Screen.Cursor := crHourGlass;
    ErrFile := FConfiguration.TempDir + 'error.txt';
    if FEditorForm.Modified then
      DoSave(FEditorForm, FConfiguration.CreateBAKFiles);
    Call := ' -cp ' + HideBlanks(FConfiguration.Checkstyle) + ' ' +
      FConfiguration.JavaInterpreterParameter +
      ' com.puppycrawl.tools.checkstyle.Main' + ' -c ' +
      HideBlanks(FConfiguration.CheckConfiguration) + ' ' +
      FConfiguration.CheckParameter + ' ' + HideBlanks(AFile);
    FMessages.OutputLineTo(K_Messages, 'Checkstyle ' + _('for') + ' ' +
      FEditorForm.Pathname);
    if MyJavaCommands.ExecAndWait(FConfiguration.JavaInterpreter, Call, Dir,
      ErrFile, SW_HIDE) then
      if TFile.GetSize(ErrFile) = 0 then
        FMessages.OutputLineTo(K_Messages,
          'Checkstyle ' + _(LNGSuccessfullyTerminated))
      else
        FMessages.ShowMessages(ErrFile);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFJava.MIJalopyClick(Sender: TObject);
var
  Dir, AFile, Call, Parameter: string;
  I: Integer;
  AgeEnabled: Boolean;

  function ExpandJalopyJar(const Str: string): string;
  begin
    var
    Filenames := TDirectory.GetFiles(ExtractFilePath(Str), '*.jar');
    Result := String.Join(';', Filenames);
  end;

begin
  if not Assigned(FEditorForm) then
    Exit;
  AFile := ExtractFileName(FEditorForm.Pathname);
  Dir := ExtractFilePath(FEditorForm.Pathname);
  FMessages.ShowIt;
  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  AgeEnabled := FEditorForm.CheckAgeEnabled;
  try
    Screen.Cursor := crHourGlass;
    DoSave(FEditorForm, FConfiguration.CreateBAKFiles);
    Call := ' -cp ' + HideBlanks(ExpandJalopyJar(FConfiguration.Jalopy)) + ' ' +
      FConfiguration.JavaInterpreterParameter +
      ' de.hunsicker.jalopy.plugin.console.ConsolePlugin';
    if Trim(FConfiguration.JalopyConfiguration) <> '' then
      Call := Call + ' -c ' + HideBlanks(FConfiguration.JalopyConfiguration);
    Parameter := FConfiguration.JalopyParameter;
    if (Pos('Unicode', FEditorForm.Encoding) > 0) and (Pos('-e', Parameter) = 0)
    then
      Parameter := Parameter + ' -e unicode';
    Call := Call + ' ' + Parameter + ' ' + HideBlanks(AFile);

    FMessages.OutputLineTo(K_Messages, 'Jalopy ' + _('for') + ' ' +
      FEditorForm.Pathname);
    FEditorForm.CheckAgeEnabled := False;
    if MyJavaCommands.ExecAndWait(FConfiguration.JavaInterpreter, Call, Dir,
      FConfiguration.TempDir + 'error.txt', SW_HIDE) then
    begin
      ProcessTerminate('JAVA.EXE');
      FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
      I := 0;
      while I < FMessages.LBMessages.Items.Count do
      begin
        if Pos('[ERROR]', FMessages.LBMessages.Items[I]) = 1 then
          Break;
        Inc(I);
      end;
      if I = FMessages.LBMessages.Items.Count then
      begin
        FMessages.OutputLineTo(K_Messages,
          'Jalopy ' + _(LNGSuccessfullyTerminated));
        FEditorForm.Editor.Lines.LoadFromFile(FEditorForm.Pathname);
        var
          Age: TDateTime;
        FileAge(FEditorForm.Pathname, Age);
        FEditorForm.EditorAge := Age;
      end
      else
      begin
        FEditorForm.Save(False);
        FMessages.OutputLineTo(K_Messages, FMessages.LBMessages.Items[I]);
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    FEditorForm.CheckAgeEnabled := AgeEnabled;
  end;
end;

procedure TFJava.MIMSDosClick(Sender: TObject);
var
  Path: string;
begin
  if Assigned(FActiveTDIChild) then
    Path := ExtractFilePath(FActiveTDIChild.Pathname)
  else
    Path := FConfiguration.Sourcepath;
  if Win32Platform >= VER_PLATFORM_WIN32_NT then
    MyJavaCommands.ExecWithoutWait('cmd.exe', '', Path, SW_SHOWNORMAL)
  else
    MyJavaCommands.ExecWithoutWait('command.com', '', Path, SW_SHOWNORMAL);
end;

procedure TFJava.MIExplorerClick(Sender: TObject);
begin
  var
  Dir := '';
  if Assigned(FActiveTDIChild) then
  begin
    var
    AFile := FActiveTDIChild.Pathname;
    if ExtractFileExt(AFile) = '' then
      Dir := AFile
    else
      Dir := ExtractFilePath(AFile);
  end;
  if (Dir = '') or not SysUtils.DirectoryExists(Dir) then
    Dir := FConfiguration.Sourcepath;
  if not SysUtils.DirectoryExists(Dir) then
    Dir := GetDocumentsPath;
  NewExplorer(Dir, '');
end;

procedure TFJava.TBMsDosClick(Sender: TObject);
begin
  MIMSDosClick(Sender);
end;

function TFJava.OpenUMLWindow(const FileName, State: string): TFUMLForm;
var
  UMLForm: TFUMLForm;
  AForm: TFForm;
begin
  AForm := GetTDIWindow(FileName);
  if Assigned(AForm) then
  begin
    AForm.OpenWindow(Self);
    UMLForm := (AForm as TFUMLForm);
  end
  else
  begin
    UMLForm := MakeNewUMLWindow(FileName, State);
    if FileExists(FileName) then
    begin
      UMLForm.MainModul.LoadUML(FileName);
      UMLForm.CreateTVFileStructure;
    end
    else
      UMLForm.ConfigureWindow(Self);
  end;
  FActiveTDIChild := UMLForm;
  if not FMessages.Floating then
  begin
    MoveHSplitter(BottomDockPanel.Height + 1);
    MoveHSplitter(BottomDockPanel.Height - 1);
  end;
  UpdateMenuItems(Self);
  Result := UMLForm;
end;

function TFJava.NewStructogram(const FileName: string; State: string): Boolean;
begin
  var
  Structogram := TFStructogram(FormFactory(fkStructogram));
  Result := Structogram.Open(FileName);
  if Result then
    Structogram.SetState(State)
  else
    Structogram.Close;
end;

function TFJava.NewSequenceDiagram(const FileName: string;
State: string): Boolean;
begin
  var
  SequenceDiagram := TFSequenceForm(FormFactory(fkSequence));
  Result := SequenceDiagram.Open(FileName);
  if Result then
    SequenceDiagram.SetState(State)
  else
    SequenceDiagram.Close;
end;

function TFJava.GetFilename(const Extension: string; Path: string = ''): string;
var
  Str: string;
  AForm: TFForm;
begin
  if Path = '' then
  begin
    for var I := 0 to FTDIFormsList.Count - 1 do
      if FTDIFormsList[I].FormTag in [1, 2, 3, 11] then
      begin
        Path := ExtractFilePath(FTDIFormsList[I].Pathname);
        Break;
      end;
    if Path = '' then
      Path := FConfiguration.Sourcepath;
  end;
  Path := WithTrailingSlash(Path);
  var
  Int := 1;
  Str := Path + _(LNGFile) + IntToStr(Int) + Extension;
  while WindowOpened(Str, AForm) or FileExists(Str) do
  begin
    Inc(Int);
    Str := Path + _(LNGFile) + IntToStr(Int) + Extension;
  end;
  Result := Str;
end;

// Check: Should only be active in the menu if the UML window is open.
procedure TFJava.MISaveAsPictureClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
    (FActiveTDIChild as TFUMLForm).MainModul.SaveDiagramActionExecute(Self);
end;

procedure TFJava.MIOpenFolderClick(Sender: TObject);
begin
  MakeNewUMLWindow(GetFilename('.uml'), '').OpenFolder;
end;

procedure TFJava.MINewLayoutClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
    (FActiveTDIChild as TFUMLForm).MainModul.DoLayout;
end;

procedure TFJava.MINewSequencediagramClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var
  Child := TFSequenceForm(FormFactory(fkSequence));
  MISaveAsClick(Self);
  if Assigned(Child) and (Child.Pathname = '') then // save as aborted?
    Child.Close
  else
    Child.New(Child.Pathname);
  EnableUpdateMenuItems;
end;

procedure TFJava.MIDiagramFromOpenFilesClick(Sender: TObject);
var
  MessageRes: Integer;
  FileName, Path: string;
  DlgMessage: TDlgMessage;
  StringList: TStringList;
begin
  if not(HasJavaFiles or HasPascalFiles) then
    Exit;
  DisableUpdateMenuItems;
  StringList := TStringList.Create;
  try
    StringList.Sorted := True;
    Path := '';
    for var I := 0 to TDIEditFormCount - 1 do
    begin
      FileName := TDIEditFormGet(I).Pathname;
      if HasJavaExtension(FileName) or HasPascalExtension(FileName) then
      begin
        if Path = '' then
          Path := ExtractFilePath(FileName);
        StringList.Add(ChangeFileExt(ExtractFileName(FileName), ''));
      end;
    end;
    FileName := Path;
    for var I := 0 to StringList.Count - 1 do
      FileName := FileName + StringList[I];
    FileName := FileName + '.uml';

    MessageRes := mrNone;
    if FileExists(FileName) then
    begin
      DlgMessage := TDlgMessage.Create(Self);
      try
        DlgMessage.SetMessage(FileName);
        MessageRes := DlgMessage.ShowModal;
        if MessageRes = mrNo then
          Open(FileName);
      finally
        FreeAndNil(DlgMessage);
      end;
    end;
    if MessageRes = mrYes then
    begin
      CloseFile(FileName);
      Application.ProcessMessages;
    end;
    if MessageRes in [mrYes, mrNone] then
      MakeNewUMLWindow(FileName, '').OpenFiles;
  finally
    FreeAndNil(StringList);
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIWatchesClick(Sender: TObject);
begin
  FWatches.ActiveControl := FWatches.EWatch;
  FWatches.Show;
end;

procedure TFJava.SearchInIndex;
var
  Sought, HTMLfile: string;
  Str, Objekt, Typ, Link, Path, MethodAttribute: string;
  X, Line, Posi: Integer;
  StringList: TStringList;
  Editform: TFEditForm;

  function SearchString(Sought: string; var HTMLfile: string): Boolean;
  var
    Int1, Int2, Posi: Integer;
    Str: string;
  begin
    Sought := '"><B>' + Sought; // <A HREF="..."><B>Sought   </A>
    Str := StringList.Text;
    Posi := Pos(Sought, Str);
    if Posi > 0 then
    begin
      Int1 := Posi + Length(Sought);
      if IsAlpha(Str[Int1]) then
        Posi := 0
      else
      begin
        Int2 := Posi - 1;
        Int1 := Posi - 2;
        while (Int1 > 0) and (Str[Int1] <> '"') do
          Dec(Int1);
        HTMLfile := FConfiguration.GetJavaManual + '\Api' +
          Copy(Str, Int1 + 3, Int2 - Int1 - 2);
      end;
    end;
    Result := (Posi > 0);
  end;

  procedure SearchIndex(const Sought: string);
  var
    IndexFile, Str: string;
  begin
    if Sought = '' then
      Exit;
    var
    I := Ord(Sought[1]) and $1F; // map upper-lowercase to 1 to 26
    if I = 31 then
      I := 27; // '_' = 27
    IndexFile := FConfiguration.GetJavaManual + '\Api\Index-files\Index-' +
      IntToStr(I) + '.html';
    if FConfiguration.GlobalFileExists(IndexFile) then
    begin
      StringList := TStringList.Create;
      StringList.Text := MyCodeCompletion.LoadTextFromFile(IndexFile);
      if SearchString(Sought, HTMLfile) then
        if not SearchString(Sought, Str) then
          CallHelp(HTMLfile)
        else
        begin
          FSearch := True;
          CallHelp(IndexFile);
        end
      else
        CallHelp(FConfiguration.JavaManual);
      FreeAndNil(StringList);
    end
    else
      ErrorMsg(Format(_(LNGFileNotFound), [IndexFile]));
  end;

begin // SearchInIndex
  // FEditorForm <> nil;
  MyCodeCompletion.ParseSourceCodes;

  with FEditorForm do
  begin
    Sought := Editor.WordAtCursor;
    X := Editor.CaretX;
    Editor.CommandProcessor(EcWordRight, #0, nil);
    Editor.CommandProcessor(EcWordLeft, #0, nil);
    Str := Copy(Editor.LineText, 1, Editor.CaretX + Length(Sought) - 1);
    Editor.CaretX := X;
    Objekt := MyCodeCompletion.GetObjectFrom(Str);
    Posi := FConfiguration.ImportCache.IndexOfName(Objekt);
    if Posi > -1 then
      Typ := FConfiguration.ImportCache.ValueFromIndex[Posi]
    else
      Typ := MyCodeCompletion.GetToTypeOfObject(Objekt, MethodAttribute,
        Editor.CaretY);
    if Typ = '' then
      Typ := MyCodeCompletion.GetToTypeOfObject('this.' + Objekt,
        MethodAttribute, Editor.CaretY);
    if MyCodeCompletion.IsJavaAPIClass(Typ) or MyCodeCompletion.IsJavaAPIClass
      (Objekt, Typ) then
    begin
      CallHelp(MyCodeCompletion.HTMLFileofJavaClass);
      Exit;
    end;
  end;

  // work is needed here
  if (Typ = '') or IsSimpleType(Typ) or IsSimpleType(WithoutArray(Typ)) or
    (Typ = 'void') then
    if FEditorForm.FrameType = 8 then
      CallHelp(FConfiguration.JavaManualFX)
    else
      CallHelp(FConfiguration.JavaManual)
  else if MyCodeCompletion.IsSelfDefinedClassOrInterface(Typ, Path, Editform)
  then
  begin
    Line := MyCodeCompletion.GetLine(Typ, MethodAttribute);
    if Line = -1 then
    begin // inherited method
      if MyCodeCompletion.IsJavaAPIClass(Typ) then
      begin
        Link := MyCodeCompletion.GetAPIReference(MethodAttribute);
        CallHelp(Link);
      end;
    end
    else
    begin
      if Path = '' then
        SwitchToWindow(GetPathnameForClass(Typ))
      else if Path = FEditorForm.Pathname then
        CallHelp(FConfiguration.JavaManual)
      else
      begin
        SwitchWindowWithSearch(Path);
        FEditorForm.Editor.CaretY := Line;
        FEditorForm.Editor.EnsureCursorPosVisible;
      end;
    end;
  end
  else if MyCodeCompletion.IsJavaAPIClass(Typ) then
  begin
    Link := MyCodeCompletion.GetAPIReference(MethodAttribute);
    CallHelp(Link);
  end
  else
    SearchIndex(Sought);
end;

procedure TFJava.MIToolbarClick(Sender: TObject);
begin
  ControlBar.Visible := not ControlBar.Visible;
end;

procedure TFJava.MIUpdateClick(Sender: TObject);
begin
  with TFUpdateDialog.Create(Self) do
  begin
    EOldVersion.Text := UDlgAbout.Version + ', ' + TFAbout.GetDate;
    ENewVersion.Text := '';
    Memo.Lines.Clear;
    ProgressBar.Position := 0;
    ShowModal;
    Free;
  end;
end;

procedure TFJava.MISaveAsProjectClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  with SDSaveAs do
  begin
    Title := _(LNGSaveAs);
    InitialDir := FConfiguration.Sourcepath;
    Filter := 'Java-Editor-' + _(LNGProject) + ' (*.jep)|*.jep';
    FileName := '';
    if Execute then
    begin
      if ExtractFileExt(FileName) = '' then
        FileName := FileName + '.jep';
      if not FileExists(FileName) or FileExists(FileName) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
        mbYesNoCancel, 0) = mrYes) then
      begin
        DoSaveAsProject(FileName);
        UpdateMenuItems(Self);
      end;
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.DoSaveAsProject(const FileName: string);
var
  StringList: TStringList;
  Str1, Str2, Current: string;
  AForm: TFForm;
begin
  StringList := TStringList.Create;
  StringList.Add('ClasspathUser=' + FConfiguration.JavaClasspathUser);
  StringList.Add('InterpreterParameter=' +
    FConfiguration.JavaInterpreterParameter);
  StringList.Add('CompilerParameter=' + FConfiguration.JavaCompilerParameter);
  StringList.Add('JavaDoc=' + FConfiguration.JavaJavaDocs);
  StringList.Add('JavaStartClass=' + FConfiguration.JavaStartClass);
  StringList.Add('JavaStartClassIsApplet=' +
    BoolToStr(FConfiguration.JavaStartClassIsApplet));

  for var I := 1 to FMyTabBar.Tabs.Count do
  begin
    AForm := GetTabForm(I - 1);
    if Assigned(AForm) then
    begin
      Str1 := AForm.GetState + AForm.GetFormType;
      Str2 := AForm.Pathname;
      if HasUMLExtension(Str2) and AForm.IsDefaultFilename then
        StringList.Add(Str1 + FConfiguration.RemovePortableDrive
          (FConfiguration.TempDir) + ExtractFileName(Str2))
      else
        StringList.Add(Str1 + FConfiguration.RemovePortableDrive(Str2));
    end;
    if I - 1 = FMyTabBar.Tabs.IndexOf(FMyTabBar.SelectedTab) then
      Current := Str1 + FConfiguration.RemovePortableDrive(Str2);
  end;
  StringList.Add(Current);
  FProjectFilename := FileName;
  FMessages.StatusMessage(FProjectFilename);
  StringList.SaveToFile(FileName);
  FreeAndNil(StringList);

  FConfiguration.WriteStringU('Window', 'Win1', '%T%' + FileName);
  FConfiguration.WriteIntegerU('Window', 'Wins', 1);
  FConfiguration.WriteStringU('Window', 'Current', '');
end;

procedure TFJava.OpenProject(const FileName: string);
var
  StringList: TStringList;
  WinName, Current: string;
begin
  // Must not open the explorer window when double-clicking from the explorer
  // must be started by timer in this case
  StringList := TStringList.Create;
  try
    try
      StringList.LoadFromFile(FileName);
      try
        Screen.Cursor := crHourGlass;
        LockWindow(FJava.Handle);
        FConfiguration.SaveConfigurationForProject;
        for var I := 0 to StringList.Count - 2 do
        begin
          if Pos('ClasspathUser=', StringList[I]) = 1 then
            FConfiguration.JavaClasspathUser :=
              Copy(StringList[I], 15, Length(StringList[I]))
          else if Pos('InterpreterParameter=', StringList[I]) = 1 then
            FConfiguration.JavaInterpreterParameter :=
              Copy(StringList[I], 22, Length(StringList[I]))
          else if Pos('CompilerParameter=', StringList[I]) = 1 then
            FConfiguration.JavaCompilerParameter :=
              Copy(StringList[I], 19, Length(StringList[I]))
          else if Pos('JavaJavaDocs=', StringList[I]) = 1 then
            FConfiguration.JavaJavaDocs :=
              Copy(StringList[I], 14, Length(StringList[I]))
          else if Pos('JavaStartClass=', StringList[I]) = 1 then
            FConfiguration.JavaStartClass :=
              Copy(StringList[I], 16, Length(StringList[I]))
          else if Pos('JavaStartClassIsApplet=', StringList[I]) = 1 then
            FConfiguration.JavaStartClassIsApplet :=
              StrToBool(Copy(StringList[I], 24, Length(StringList[I])))
          else
          begin
            WinName := FConfiguration.AddPortableDrive(StringList[I]);
            OpenFileWithState(WinName);
          end;
        end;
        var
        Idx := StringList.Count - 1;
        if Idx >= 0 then
        begin
          Current := FConfiguration.AddPortableDrive(StringList[Idx]);
          OpenFileWithState(Current);
        end;
        FProjectFilename := FileName;
        FMessages.StatusMessage(FProjectFilename);
        RearrangeFileHistory(FileName);
      finally
        UnlockWindow;
        Screen.Cursor := crDefault;
      end;
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TFJava.MIObjectInspectorClick(Sender: TObject);
begin
  FObjectInspector.ChangeHideShow;
end;

procedure TFJava.MIClassEditorClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if Assigned(FActiveTDIChild) then
  begin
    if (FActiveTDIChild.FormTag = 1) and (FActiveTDIChild as TFEditForm).IsJava
    then
      PrepareClassEdit(FActiveTDIChild.Pathname, 'Edit', nil)
    else if (FActiveTDIChild.FormTag = 2) then
    begin
      FAUMLForm := FActiveTDIChild as TFUMLForm;
      FAUMLForm.TBClassEditorClick(Self);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIMessagesDockedClick(Sender: TObject);
begin
  if FMessages.MyIsVisible then
    if FMessages.Floating then
      FMessages.MyDock
    else
      FMessages.Undock;
end;

procedure TFJava.MIRefreshClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
    (FActiveTDIChild as TFUMLForm).TBRefreshClick(Self);
end;

procedure TFJava.MINewClassClick(Sender: TObject);
var
  Editform: TFEditForm;
  Hidden: Boolean;
begin
  DisableUpdateMenuItems;
  FAUMLForm := GetActiveUML;
  if Assigned(FAUMLForm) then
  begin
    FConfiguration.Sourcepath := ExtractFilePath(FAUMLForm.Pathname);
    FAUMLForm.MainModul.UnSelectAllElements;
    LockWindow(FJava.Handle);
    Hidden := True;
  end
  else
  begin
    var
    AEditor := GetActiveEditor;
    if Assigned(AEditor) then
      FConfiguration.Sourcepath := ExtractFilePath(AEditor.Pathname);
    Hidden := False;
  end;
  Editform := NewEditform(Hidden);
  if Assigned(Editform) then
  begin
    FTemplates.SBNewClass(Editform);
    PrepareClassEdit(Editform.Pathname, 'New', FAUMLForm);
  end;
  if Assigned(FAUMLForm) then
  begin
    FAUMLForm.ConfigureWindow(Self);
    UnlockWindow;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIClassOpenClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  LockWindow(FJava.Handle);
  with ODOpen do
  begin
    if Assigned(FActiveTDIChild) then
      InitialDir := ExtractFilePath(FActiveTDIChild.Pathname)
    else
      InitialDir := FConfiguration.Sourcepath;
    FileName := '*.java;*.class';
    FilterIndex := 1;
    if Execute then
    begin
      for var I := 0 to Files.Count - 1 do
        DoOpenInUMLWindow(Files[I]);
      FConfiguration.Sourcepath := ExtractFilePath(Files[0]);
    end;
  end;
  UnlockWindow;
  EnableUpdateMenuItems;
end;

procedure TFJava.DoOpenInUMLWindow(const Pathname: string);
var
  UMLWindow: TFUMLForm;
  Str: string;
  AForm: TFForm;
begin
  if not(HasJavaExtension(Pathname) or HasClassExtension(Pathname)) then
    Exit;
  FConfiguration.ShowAlways := False;
  UMLWindow := nil;
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 2) then
  begin
    UMLWindow := FActiveTDIChild as TFUMLForm;
    UMLWindow.MainModul.UnSelectAllElements;
  end;
  if Assigned(UMLWindow) then
  begin
    UMLWindow.MainModul.AddToProject(Pathname);
    UMLWindow.Modified := True;
    if UMLWindow.MainModul.Diagram.ShowObjectDiagram then
    begin
      UMLWindow.MainModul.Diagram.ShowObjectDiagram := False;
      UMLWindow.TBObjectDiagram.Down := False;
    end;
  end
  else
  begin
    Str := ChangeFileExt(Pathname, '.uml');
    if FileExists(Str) then
    begin
      Open(Str);
      AForm := GetTDIWindowType(Str, '%U%');
    end
    else
      AForm := nil;
    if Assigned(AForm) then
    begin
      UMLWindow := (AForm as TFUMLForm);
      UMLWindow.MainModul.AddToProject(Pathname);
    end
    else
    begin
      UMLWindow := MakeNewUMLWindow(Str, '');
      UMLWindow.MainModul.AddToProject(Pathname);
      DoSave(UMLWindow, WithoutBackup);
      RearrangeFileHistory(Str);
    end;
  end;
  UMLWindow.OpenWindow(Self);
  UMLWindow.SetActiveControl(UMLWindow.MainModul.Diagram.GetPanel);
  UMLWindow.CreateTVFileStructure;
  FConfiguration.ShowAlways := True;
end;

procedure TFJava.ClassEdit;
var
  Str: string;
  Editform: TFEditForm;
  X, Y: Integer;
begin
  Editform := TFEditForm(GetTDIWindowType(FEditClass, '%E%'));
  if not Assigned(Editform) and FileExists(FEditClass) then
    Editform := OpenEditForm(FEditClass, True)
  else if (FEditClassStatus = 'New') and Assigned(FAUMLForm) and
    Assigned(Editform) then
    Editform.Hide;

  if Assigned(Editform) then
  begin
    with TFClassEditor.Create(Self) do
    begin
      CreateTreeView(Editform, FAUMLForm);
      Editform.Editor.LockUndo;
      if Assigned(FAUMLForm) then
      begin
        Application.ProcessMessages;
        FAUMLForm.MainModul.Diagram.Lock(True);
      end;
      ShowModal;
      SaveWindow;
      Free;
    end;

    Editform.AutomatedCompleteImports;
    if Editform.Modified then
    begin
      Editform.Save(False); // Here
      MyJavaCommands.CompileForm(Editform);
    end;

    Editform.Editor.UnlockUndo;

    if Assigned(FAUMLForm) then
    begin
      FAUMLForm.MainModul.AddToProject(Editform.Pathname);
      FAUMLForm.Modified := True;
      FAUMLForm.MainModul.Diagram.Lock(False);
    end;

    if Editform.Visible and (FEditClassStatus = 'Edit') then
    begin
      // TODO isn't that easier?
      X := Editform.Editor.CaretX;
      Y := Editform.Editor.CaretY;
      Editform.Editor.Perform(WM_LBUTTONDOWN, 300, 300);
      Sleep(10);
      Editform.Editor.Perform(WM_LBUTTONUP, 300, 300);
      Editform.Editor.CaretX := X;
      Editform.Editor.CaretY := Y;
    end;
  end
  else
  begin
    Str := ChangeFileExt(ExtractFileName(FEditorForm.Pathname), '');
    ErrorMsg(Format(_(LNGClassNotFound), [Str]));
  end;
  if Assigned(FAUMLForm) then
  begin
    FAUMLForm.OpenWindow(Self);
    FAUMLForm.SetActiveControl(FAUMLForm.MainModul.Diagram.GetPanel);
  end;
end;

procedure TFJava.PrepareClassEdit(const Pathname, Status: string;
UML: TFUMLForm);
begin
  if ((Status = 'Edit') or (Status = 'Refresh')) and
    not(MyJavaCommands.HasValidClass(Pathname) or MyJavaCommands.AcceptableError
    (Pathname) or (MessageDlg(_('Sourcecode has errors. Open anyway?'),
    mtConfirmation, mbYesNo, -1) = idYes)) then
    Exit;
  FEditClass := Pathname;
  FEditClassStatus := Status;
  FAUMLForm := UML;
  TThread.ForceQueue(nil,
    procedure
    begin
      ClassEdit;
    end);
end;

procedure TFJava.MIShowExecutionPointClick(Sender: TObject);
begin
  FMessages.LBStack.ItemIndex := 0;
  FMessages.LBStackDblClick(Self);
end;

procedure TFJava.MIRunToCursorClick(Sender: TObject);
begin
  if Assigned(FEditorForm) then
    FEditorForm.InsertGotoCursorBreakpoint;
  MIRunClick(Self);
end;

procedure TFJava.ControlBarDockOver(Sender: TObject; Source: TDragDockObject;
X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source.Control is TFMessages) then
    Accept := False;
end;

// TODO
procedure TFJava.MoveHSplitter(AHeight: Integer);
begin
  HSplitter.Align := alTop;
  BottomDockPanel.Align := alTop;
  BottomDockPanel.Height := AHeight;
  BottomDockPanel.Align := alBottom;
  HSplitter.Align := alBottom;
end;

procedure TFJava.MoveVSplitter(AWidth: Integer);
begin
  VSplitter.Align := alLeft;
  RightDockPanel.Align := alLeft;
  RightDockPanel.Width := AWidth;
  RightDockPanel.Align := alRight;
  VSplitter.Align := alRight;
end;

procedure TFJava.SBSequenceClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var
  Child := TFSequenceForm(FormFactory(fkSequence));
  MISaveAsClick(Self);
  if Assigned(Child) and (Child.Pathname = '') then // aborting save
    Child.Close
  else
  begin
    Child.New(Child.Pathname);
    Child.Save(False);
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.SBStructogramClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var
  Structogram := TFStructogram(FormFactory(fkStructogram));
  StructogramSaveAs(Structogram);
  if Structogram.Pathname = '' then // aborting save
    Structogram.Close
  else
  begin
    Structogram.New;
    Structogram.Save(False);
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.SBStructureIndentClick(Sender: TObject);
begin
  if Assigned(FEditorForm) then
    FEditorForm.SBStructureIndentClick(Self);
end;

procedure TFJava.StructogramFromText(Sourcecode, Pathname: string);
begin
  var
  Structogram := TFStructogram(GetTDIWindowType(Pathname, '%S%'));
  if Assigned(Structogram) then
  begin
    Structogram.RenewFromText(Sourcecode);
    Open(Pathname);
  end
  else
  begin
    if FileExists(Pathname) then
    begin
      var
      MessageRes := MessageDlg(Format(_(LNGFileAlreadyExists), [Pathname]),
        mtConfirmation, mbYesNoCancel, 0);
      case MessageRes of
        mrYes:
          IOUtils.TFile.Delete(Pathname);
        mrNo:
          Pathname := GetFilename('.jsg');
      else
        Exit;
      end;
    end;
    Structogram := TFStructogram(FormFactory(fkStructogram));
    Structogram.Pathname := Pathname;
    Structogram.GenerateFromText(Sourcecode);
  end;
  UpdateMenuItems(Self);
end;

procedure TFJava.HSplitterMoved(Sender: TObject);
begin
  FConfiguration.BottomDockPanelHeight := BottomDockPanel.Height;
  if Assigned(FFileStructure) and FFileStructure.Visible or
    Assigned(FObjectInspector) and FObjectInspector.Visible then
    UpdateLayoutRightDockPanel;
  HSplitter.Top := 0;
end;

procedure TFJava.VSplitterMoved(Sender: TObject);
begin
  if Assigned(FConfiguration) then
    FConfiguration.RightDockPanelWidth := RightDockPanel.Width;
  VSplitter.Left := 0;
  RightDockPanel.Left := High(SmallInt);
end;

procedure TFJava.LoadBounds;
var
  Left, Top, Width, Height: Integer;
begin
  if FConfiguration.FirstStartAfterInstallation then
    WindowState := wsMaximized
  else
    WindowState := StrToWindowState(FConfiguration.ReadStringU('Program',
      'WindowState', '2'));
  if WindowState = wsNormal then
  begin // wsNormal
    Left := FConfiguration.ReadIntegerU('Program', 'Left', 0);
    Top := FConfiguration.ReadIntegerU('Program', 'Top', 0);
    Width := FConfiguration.ReadIntegerU('Program', 'Width', 800);
    Height := FConfiguration.ReadIntegerU('Program', 'Height', 600);
    Top := Min(Top, Screen.Width - 100);
    SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TFJava.SaveBounds;
begin
  FConfiguration.WriteIntegerU('Program', 'Left', PPIUnScale(Left));
  FConfiguration.WriteIntegerU('Program', 'Top', PPIUnScale(Top));
  FConfiguration.WriteIntegerU('Program', 'Width', PPIUnScale(Width));
  FConfiguration.WriteIntegerU('Program', 'Height', PPIUnScale(Height));
  FConfiguration.WriteStringU('Program', 'WindowState',
    WindowStateToStr(WindowState));
end;

procedure TFJava.MINewUMLClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  MakeNewUMLWindow(GetFilename('.uml'), '');
  EnableUpdateMenuItems;
end;

function TFJava.GetUMLWindow: TFUMLForm;
begin
  Result := nil;
  for var I := 0 to FMyTabBar.Tabs.Count - 1 do
  begin
    var
    AForm := GetTabForm(I);
    if AForm.FormTag = 2 then
    begin
      Result := (AForm as TFUMLForm);
      Break;
    end;
  end;
end;

procedure TFJava.MICompareClick(Sender: TObject);
var
  Forms: array [0 .. 1] of TFEditForm;
begin
  DisableUpdateMenuItems;
  EditorAgeTimer.Enabled := False;
  Forms[0] := nil;
  Forms[1] := nil;
  for var I := 0 to Min(2, TDIEditFormCount) - 1 do
  begin
    Forms[I] := TDIEditFormGet(I);
    if Forms[I].Modified then
      DoSave(Forms[I], WithoutBackup);
  end;
  NewTextDiff(Forms[0], Forms[1]);
  EnableUpdateMenuItems;
end;

procedure TFJava.MISVNCommitClick(Sender: TObject);
var
  Path, ParM: string;
begin
  if Assigned(FActiveTDIChild) then
  begin
    MISaveAllClick(Self);
    Path := ExtractFilePath(FActiveTDIChild.Pathname);
    if InputQuery('Commit', _(LNGMessage), ParM) then
      FSubversion.CallSVN('\bin\svn', 'commit -ParM "' + ParM + '"', Path);
  end;
end;

procedure TFJava.MISVNAddClick(Sender: TObject);
var
  Str, Posi, Repos, TempFile1, TempFile2: string;
  AText: TStringList;
  FStream: TFileStream;
begin
  LockWindow(FMessages.Handle);
  if Assigned(FActiveTDIChild) then
  begin
    if (FActiveTDIChild.FormTag = 1) and (FActiveTDIChild as TFEditForm).Modified
    then
      DoSave(FActiveTDIChild, WithoutBackup);

    Str := FActiveTDIChild.Pathname;
    Posi := ExtractFilePath(Str);
    FSubversion.CallSVN('\bin\svn', 'add ' + HideBlanks(Str), Posi);

    AText := TStringList.Create;
    Repos := FSubversion.GetRepositoryURL(Str);
    TempFile1 := HideBlanks(FConfiguration.TempDir + 'RunJava.bat');
    TempFile2 := HideBlanks(Repos + '/RunJava.bat');
    FStream := TFileStream.Create(TempFile1, fmCreate or fmShareExclusive);
    FreeAndNil(FStream);
    FSubversion.CallSVN('\bin\svn', 'import ' + TempFile1 + ' ' + TempFile2 +
      ' -m ""', Posi);
    FSubversion.CallSVN('\bin\svn', 'delete ' + TempFile2 + ' -m ""', Posi);
    FSubversion.CallSVN('\bin\svn', 'checkout ' + Repos + ' ' +
      HideBlanks(Posi), Posi);
    FSubversion.CallSVN('\bin\svn', 'add ' + HideBlanks(Str), Posi);
    AText.AddStrings(FMessages.LBMessages.Items);
    FMessages.LBMessages.Items := AText;
    FreeAndNil(AText);
  end;
  UnlockWindow;
end;

procedure TFJava.MISVNTreeClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
    FSubversion.CallSVN('\bin\svnlook',
      'tree ' + HideBlanks(FConfiguration.SVNRepository), '');
end;

procedure TFJava.MISVNStatusClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
  begin
    var
    Str := FActiveTDIChild.Pathname;
    var
    Posi := ExtractFilePath(Str);
    FSubversion.CallSVN('\bin\svn', 'status ' + HideBlanks(Posi) +
      ' -u -v', Posi);
  end;
end;

procedure TFJava.MISVNUpdateClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
  begin
    var
    Str := FActiveTDIChild.Pathname;
    var
    Posi := ExtractFilePath(Str);
    FSubversion.CallSVN('\bin\svn', 'update ' + HideBlanks(Posi), Posi);
  end;
end;

procedure TFJava.MISVNCompareClick(Sender: TObject);
var
  Str, Str1, Str2, Posi, Rev: string;
  AActive, AForm: TFEditForm;
begin
  LockWindow(FMessages.Handle);
  if Assigned(FActiveTDIChild) then
  begin
    AActive := FActiveTDIChild as TFEditForm;
    Str := AActive.Pathname;
    Posi := ExtractFilePath(Str);
    FSubversion.ForFile(Str);
    if (FSubversion.ShowModal = mrOk) and
      (FSubversion.LVRevisions.ItemIndex > -1) then
    begin
      Rev := FSubversion.GetRevision;
      FSubversion.CallSVN('\bin\svn', 'cat -r ' + Rev + ' ' + Str, Posi);
      Str1 := FMessages.LBMessages.Items[0];
      FMessages.LBMessages.Items.Delete(0);
      Str2 := ExtractFileName(Str);
      Insert('[R' + Rev + ']', Str2, Pos('.', Str2));
      Str2 := FConfiguration.TempDir + Str2;
      AForm := TFEditForm(FormFactory(fkEditor));
      if Assigned(AForm) then
      begin
        AForm.New(Str2);
        AForm.PutText(FMessages.LBMessages.Items.Text, False);
        AForm.Save(False);
        NewTextDiff(AActive, AForm);
      end;
      FMessages.LBMessages.Items.Text := Str1;
    end
    else
      FMessages.LBMessages.Clear;
  end;
  UnlockWindow;
end;

procedure TFJava.MISVNLogClick(Sender: TObject);
begin
  if Assigned(FActiveTDIChild) then
  begin
    var
    Str := FActiveTDIChild.Pathname;
    var
    Posi := ExtractFilePath(Str);
    FSubversion.CallSVN('\bin\svn', 'log ' + HideBlanks(Str), Posi);
  end;
end;

procedure TFJava.scpJavaExecute(Kind: SynCompletionType; Sender: TObject;
var CurrentInput: string; var XPos, YPos: Integer; var CanExecute: Boolean);
begin
  if CurrentInput <> '' then
    FScpState := 0
  else
    FScpState := 1;
  if FMessages.InteractiveEditActive or Assigned(FEditorForm) and FEditorForm.IsJava
  then
    CanExecute := MyCodeCompletion.DoScpJavaExecute(FScpState, '', CurrentInput,
      FEditorForm)
  else
    CanExecute := False;
end;

procedure TFJava.scpParamsExecute(Kind: SynCompletionType; Sender: TObject;
var CurrentInput: string; var XPos, YPos: Integer; var CanExecute: Boolean);
begin
  CanExecute := MyCodeCompletion.DoScpParamsExecute(2);
  if not CanExecute then
    FAfterCodeCompletion := False;
end;

procedure TFJava.scpJavaChange(Sender: TObject; AIndex: Integer);
begin
  FScpHint.MHint.Clear;
  if (-1 < AIndex) and (AIndex < MyCodeCompletion.DocuList.Count) then
    FScpHint.MHint.Lines.Add(MyCodeCompletion.DocuList[AIndex]);
  FScpHint.MHint.SelStart := 0;
  FScpHint.MHint.SelLength := 0;
end;

procedure TFJava.scpJavaShow(Sender: TObject);
begin
  if (FScpState > 0) and (MyCodeCompletion.DocuList.Count > 0) then
  begin
    FScpHint.MHint.Lines.Add(MyCodeCompletion.DocuList[0]);
    FScpHint.SetBounds(FScpJava.Form.Left, FScpJava.Form.Top +
      FScpJava.Form.Height - 7, FScpJava.Form.Width, FScpHint.Height);
    FScpHint.Visible := True;
  end;
end;

procedure TFJava.scpJavaClose(Sender: TObject);
begin
  FScpHint.Close;
  if Assigned(FScpHint.MHint) then
    FScpHint.MHint.Clear;
end;

procedure TFJava.scpHandleMethods(Editor: TSynEdit; Index: Integer;
EndToken: Char);
const
  EcChar = 511; // insert char
  EcDeleteLastChar = 501; // delete last char (i.e. backspace key)
begin
  var
  Str := FScpJava.ItemList[Index];
  FScpJavaIsMethod := (Pos('\image{9}', Str) + Pos('\image{10}', Str) > 0) and
    (Pos(': void', Str) > 0);
  FScpParamIndex := Index;
  var
  Posi1 := Pos('(', Str);
  var
  Posi2 := Pos(')', Str);
  Str := Copy(Str, Posi1 + 1, Posi2 - Posi1 - 1);
  Editor.CommandProcessor(EcChar, ')', nil);
  if FScpJavaIsMethod then
    Editor.CommandProcessor(EcChar, ';', nil);
  if Str <> '' then
  begin // with parameters
    if FScpJavaIsMethod then
      Editor.CommandProcessor(CecLeft, #0, nil);
    Editor.CommandProcessor(CecLeft, #0, nil);
    FScpParams.ItemList.Clear;
    FScpParams.ItemList.Add(Str);
    FAfterCodeCompletion := True;
    FScpParams.ActivateCompletion;
    if EndToken = '(' then
      Editor.CommandProcessor(EcDeleteLastChar, #0, nil);
  end;
  if Assigned(FEditorForm) then
    FEditorForm.Enter(Self);
end;

procedure TFJava.scpJavaOnCodeCompletion(Sender: TObject; var Value: string;
Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  var
  Ext := ExtractFileExt(Value);
  if (FScpState = 0) and ((Ext = '.java') or (Ext = '.html')) then
    Value := ChangeFileExt(ExtractFileName(Value), '');
end;

procedure TFJava.scpJavaAfterCodeCompletion(Sender: TObject;
const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);

var
  Str, Package: string;
  Int, Posi: Integer;
  DoImport: Boolean;
  Editor: TSynEdit;
  StringList: TStringList;
begin
  DoImport := True;
  if FMessages.InteractiveEditActive then
    Editor := FMessages.ActiveInteractive
  else if Assigned(FEditorForm) then
    Editor := FEditorForm.Editor
  else
    Exit;

  if Copy(Value, Length(Value), 1) = '(' then
    scpHandleMethods(Editor, Index, EndToken)
  else if Copy(Value, Length(Value) - 4, 5) = '.html' then
    with Editor do
    begin
      Str := Lines[CaretY - 1];
      Str := ReplaceStr(Str, Value, '');
      Lines[CaretY - 1] := Str;
      CaretX := CaretX - Length(Value);
      CallHelp(Value);
      DoImport := False;
    end
  else if Copy(Value, Length(Value) - 4, 5) = '.java' then
    with Editor do
    begin
      Str := Lines[CaretY - 1];
      Str := ReplaceStr(Str, Value, '');
      Lines[CaretY - 1] := Str;
      CaretX := CaretX - Length(Value);
      SwitchToWindow(Value);
      DoImport := False;
    end;

  if Pos('|', Value) > 0 then
  begin // if/while/for/do/else/....
    StringList := TStringList.Create;
    StringList.Text := Value;
    Int := 0;
    Posi := Pos('|', StringList[Int]);
    while (Int < StringList.Count - 1) and (Posi = 0) do
    begin
      Inc(Int);
      Posi := Pos('|', StringList[Int]);
    end;
    Editor.CaretY := Editor.CaretY - (StringList.Count - 1) + Int;
    Str := Editor.Lines[Editor.CaretY - 1];
    Posi := Pos('((|', Str); // if with direct following (
    if Posi > 0 then
    begin
      Delete(Str, Posi + 1, 2);
      Inc(Posi);
    end
    else
    begin
      Posi := Pos('|', Str);
      Delete(Str, Posi, 1);
    end;
    Editor.Lines[Editor.CaretY - 1] := Str;
    Editor.CaretX := Posi;
    FreeAndNil(StringList);
    DoImport := False;
  end;

  if Pos('if () {', Value) = 1 then
  begin // handle "if"
    if Pos(' else ', Value) > 0 then
      Editor.CaretY := Editor.CaretY - 4
    else
      Editor.CaretY := Editor.CaretY - 2;
    Str := Editor.Lines[Editor.CaretY - 1];
    Posi := Pos(' () ', Str);
    Editor.CaretX := Posi + 2;
    DoImport := False;
  end;

  if (-1 < Index) and (Index < FScpJava.ItemList.Count) then
  begin
    Str := FScpJava.ItemList[Index];
    Posi := Pos(' - \color{clgray}', Str);
    if (Posi > 0) and Assigned(FEditorForm) and DoImport and (EndToken = #0)
    then
    begin
      Package := Str;
      Delete(Package, 1, Posi + Length(' - \color{clgray}') - 1);
      if Value <> '' then
        Package := Package + '.' + Value;
      FEditorForm.InsertImport(Package);
    end;
  end;
end;

procedure TFJava.scpParamsPaintItem(Sender: TObject; Index: Integer;
TargetCanvas: TCanvas; ItemRect: TRect; var CustomDraw: Boolean);
begin
  var
  I := FScpParams.Form.CurrentIndex;
  var
  Str := FScpParams.ItemList[0];
  Str := MyCodeCompletion.FormatParamList(Str, I);
  FormattedTextOut(TargetCanvas, ItemRect, CurrentPPI, Str, False, nil, nil);
  CustomDraw := True;
end;

procedure TFJava.scpSetEditForm(Editform: TFEditForm);
begin
  if Editform.IsJava then
    scpSetEditor(Editform.Editor)
  else
    scpSetEditor(nil);
end;

procedure TFJava.scpSetEditor(Editor: TSynEdit);
begin
  FScpJava.Editor := Editor;
  FScpParams.Editor := Editor;
  if FConfiguration.CodeCompletionAlways then
    FScpJava.TriggerChars :=
      '.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  if FConfiguration.CodeCompletionCtrlSpace then
    FScpJava.TriggerChars := '.';
  FScpJava.ShortCut := Menus.ShortCut(Word(' '), [ssCtrl]);
  if FConfiguration.ParameterHints then
    FScpParams.TriggerChars := '('
  else
    FScpParams.TriggerChars := '';
end;

function TFJava.GetTab(Num: Integer): Integer;
begin
  Result := -1;
  if Assigned(FMyTabBar) then
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
      if Assigned(FMyTabBar.Tabs[I]) and Assigned(FMyTabBar.Tabs[I].Data) and
        (TTabObject(FMyTabBar.Tabs[I].Data).Num = Num) then
        Exit(I);
end;

procedure TFJava.TabModified(Num: Integer; Modified: Boolean);
begin
  var
  I := GetTab(Num);
  if I > -1 then
    FMyTabBar.Tabs[I].Modified := Modified;
end;

procedure TFJava.SetSelectedTabAndWindow(Num: Integer);
var
  MenuItem: TTBCustomItem;
begin
  if Assigned(FMyTabBar) then
  begin
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
      if (TTabObject(FMyTabBar.Tabs[I].Data).Num = Num) and
        not FMyTabBar.Tabs[I].Selected then
        FMyTabBar.Tabs[I].Selected := True;
    for var I := FMyTabBar.Tabs.Count downto 1 do
    begin
      MenuItem := MIWindow[MIWindow.Count - I];
      if (MenuItem.Tag = Num) and not MenuItem.Checked then
        MenuItem.Checked := True
      else if (MenuItem.Tag <> Num) and MenuItem.Checked then
        MenuItem.Checked := False;
    end;
  end;
end;

procedure TFJava.SetSelectedTabAndWindow(const Pathname: string);
var
  Number: Integer;
begin
  if Assigned(FMyTabBar) then
  begin
    Number := -1;
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
      if GetTabForm(I).Pathname = Pathname then
      begin
        FMyTabBar.Tabs[I].Selected := True;
        Number := I;
        Break;
      end;
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
      MIWindow[I].Checked := False;
    if Number > -1 then
      MIWindow[WindowOffset + Number].Checked := True;
  end;
end;

procedure TFJava.EditorAgeTimerTimer(Sender: TObject);
begin
  EditorAgeTimer.Enabled := False;
  for var I := 0 to TDIEditFormCount - 1 do
    TDIEditFormGet(I).CheckAge;
  EditorAgeTimer.Enabled := True;
end;

procedure TFJava.Restart;
begin
  var
  Params := '';
  for var I := 1 to ParamCount do
    Params := Params + ' ' + ParamStr(I);
  Close;
  Application.ProcessMessages;
  Sleep(20);
  MyJavaCommands.ExecWithoutWait(ParamStr(0), Params, '', SW_SHOWNORMAL);
end;

function TFJava.IsAValidClass(const Pathname: string): Boolean;
begin
  var
  Editform := GetTDIWindowType(Pathname, '%E%');
  var
  Modified := Assigned(Editform) and (Editform as TFEditForm).Modified and
    Editform.Visible;
  Result := (Pathname = '') or (Pos(FConfiguration.JavaCache, Pathname) = 1) or
    (FConfiguration.HasAgeClass(Pathname) and not Modified);
end;

procedure TFJava.RefreshUMLWindows;
var
  Form, AForm: TFForm;
begin
  LockWindow(FJava.Handle);
  Form := FActiveTDIChild;
  for var I := 0 to FMyTabBar.Tabs.Count - 1 do
  begin
    AForm := GetTabForm(I);
    if AForm.FormTag = 2 then
      (AForm as TFUMLForm).Refresh;
  end;
  if Assigned(Form) and Form.CanFocus then
    Form.SetFocus;
  LockWindow(0);
  UpdateMenuItems(Self);
end;

function TFJava.GetActiveTDIFormPath: string;
begin
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag in [1, 2]) then
    Result := ExtractFilePath(FActiveTDIChild.Pathname)
  else
    Result := FConfiguration.EditorFolder;
end;

function TFJava.ExtentClasspath(Classpath: string): string;
var
  Str: string;
  AForm: TFForm;
  UMLForm: TFUMLForm;
  Editform: TFEditForm;
begin
  Editform := GetActiveEditor;
  if Assigned(Editform) then
  begin
    Str := ExtractFilePath(FActiveTDIChild.Pathname);
    if Pos(Str, Classpath) = 0 then
      Classpath := Str + ';' + Classpath;
  end;
  UMLForm := GetActiveUML;
  if Assigned(UMLForm) then
  begin
    Str := UMLForm.MainModul.Diagram.GetSourcePath;
    if Pos(Str, Classpath) = 0 then
      Classpath := Str + ';' + Classpath;
  end;

  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    AForm := FTDIFormsList[I];
    if AForm.FormTag = 1 then
    begin
      Str := ExtractFilePath(AForm.Pathname);
      if Pos(Str, Classpath) = 0 then
        Classpath := Str + ';' + Classpath;
    end
    else if AForm.FormTag = 2 then
    begin
      Str := (AForm as TFUMLForm).MainModul.Diagram.GetSourcePath;
      if Pos(Str, Classpath) = 0 then
        Classpath := Str + ';' + Classpath;
    end;
  end;
  Result := Classpath;
end;

function TFJava.GetAllPathnames: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    var
    StringList := FTDIFormsList[I].GetAllPathnames;
    if StringList.Count > 0 then
      Result.AddStrings(StringList);
    FreeAndNil(StringList);
  end;
end;

function TFJava.GetAllClassnames: TStringList;
var
  StringList: TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  for var I := 0 to FTDIFormsList.Count - 1 do
  begin
    StringList := FTDIFormsList[I].GetAllClassnames;
    if StringList.Count > 0 then
      Result.AddStrings(StringList);
    FreeAndNil(StringList);
  end;
end;

function TFJava.GetActiveFormTag: Integer;
begin
  Result := -1;
  if FActiveTool > -1 then
    Result := FActiveTool
  else if Assigned(FActiveTDIChild) then
    Result := FActiveTDIChild.FormTag;
end;

procedure TFJava.SaveDocking;
begin
  var
  Stream := TMemoryStream.Create;
  try
    RightDockPanel.DockManager.SaveToStream(Stream);
    Stream.Position := 0;
    FConfiguration.WriteBinaryStreamU('Docking', 'RightDockPanel', Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TFJava.LoadDocking;
begin
  var
  Stream := TMemoryStream.Create;
  try
    var
    Size := FConfiguration.ReadBinaryStreamU('Docking',
      'RightDockPanel', Stream);
    if Size > 0 then
    begin
      Stream.Position := 0;
      RightDockPanel.DockManager.LoadFromStream(Stream);
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TFJava.ShowCompileErrors;
var
  StringList: TStrings;
  Delta1, Delta2, Delta3, Posi, LineNum, Column: Integer;
  ACompile, Path, FileName, Line, Str2, Pre, Post, Mid1, Mid2, Err: string;
  Edit1, Edit2: TFEditForm;
  StringList1: TStringList;

  procedure DeterminePrePost(Idx: Integer);
  var
    Jidx: Integer;
  begin
    Pre := '';
    Post := '';
    Mid1 := '';
    Mid2 := '';
    if Str2 = 'cannot find symbol' then
    begin
      Post := StringList[Idx + 3];
      Posi := Pos('symbol: ', Post);
      Delete(Post, 1, Posi + 8);
    end
    else if Pos('might not have been initialized', Str2) > 0 then
    begin
      Pre := ReplaceStr(Str2, 'might not have been initialized', '');
      Str2 := ReplaceStr(Str2, Pre, '');
    end
    else if Pos('is already defined in', Str2) > 0 then
    begin
      Posi := Pos('is already defined in', Str2);
      Pre := Copy(Str2, 1, Posi - 1);
      Delete(Str2, 1, Posi - 1);
      Post := Copy(Str2, Length('is already defined in') + 1, Length(Str2));
      Str2 := ReplaceStr(Str2, Post, '');
    end
    else if Pos('array required, but', Str2) > 0 then
    begin
      StringList1 := Split(' ', Str2);
      Mid1 := StringList1[3];
      Str2 := 'array required, but found';
      FreeAndNil(StringList1);
    end
    else if Str2 = 'incompatible types: missing return value' then
    else if Str2 = 'incompatible types: unexpected return value' then
    else if Pos('incompatible types: possible lossy conversion from', Str2) = 1
    then
    begin
      StringList1 := Split(' ', Str2);
      if StringList1.Count >= 9 then
      begin
        Mid1 := StringList1[6];
        Mid2 := StringList1[8];
      end;
      Str2 := 'incompatible types: possible lossy';
      FreeAndNil(StringList1);
    end
    else if Pos('incompatible types: ', Str2) = 1 then
    begin
      StringList1 := Split(' ', Str2);
      if StringList1.Count >= 8 then
      begin
        Mid1 := StringList1[2];
        Mid2 := StringList1[7];
      end;
      Str2 := 'incompatible types';
      FreeAndNil(StringList1);
    end
    else if (Pos('non-static', Str2) = 1) and
      (Pos('cannot be referenced from a static context', Str2) > 0) then
    begin
      StringList1 := Split(' ', Str2);
      Jidx := 1;
      while (Jidx < StringList1.Count) and (StringList1[Jidx] <> 'cannot') do
      begin
        Mid1 := Mid1 + StringList1[Jidx] + ' ';
        Inc(Jidx);
      end;
      Mid1 := Trim(Mid1);
      Str2 := 'non-static cannot be referenced';
      FreeAndNil(StringList1);
    end
    else if Pos('bad operand types for binary operator', Str2) = 1 then
    begin
      Post := Copy(Str2, Length('bad operand types for binary operator') + 1,
        Length(Str2));
      Str2 := 'bad operand types for binary operator';
    end;
  end;

  procedure applyMiddle;
  begin
    if Mid2 <> '' then
      Err := Format(Err, [Mid1, Mid2])
    else if Mid1 <> '' then
      Err := Format(Err, [Mid1]);
  end;

begin
  Edit1 := nil;
  Edit2 := nil;
  StringList := FMessages.LBCompiler.Items;
  ACompile := Copy(_(LNGCompileWith), 1, Pos('%', _(LNGCompileWith)) - 2);

  for var I := 0 to StringList.Count - 1 do
  begin
    Line := StringList[I];
    if Pos(ACompile, Line) = 1 then
    begin
      Delete(Line, 1, Length(ACompile) + 1);
      Posi := Pos('.java', Line);
      Delete(Line, Posi + 5, Length(Line));
      FileName := Line;
      Edit1 := TFEditForm(GetTDIWindowTypeFilename(FileName, '%E%'));
      if Assigned(Edit1) then
      begin
        Edit1.InitShowCompileErrors;
        Break;
      end;
    end;
  end;

  for var I := StringList.Count - 1 downto 1 do
  begin
    Line := StringList[I];
    Delta1 := Pos('.java:', Line);
    if Delta1 > 0 then
    begin
      Path := Copy(Line, 1, Delta1 + 4);
      Str2 := Copy(Line, Delta1 + 6, 255);
      Delta3 := Delta1 + 5;
      Delta2 := Pos(':', Str2); // compile error
      if Delta2 > 0 then
      begin // errorline found
        if not TryStrToInt(Copy(Str2, 1, Delta2 - 1), LineNum) then
          Continue;
        Delete(Str2, 1, Delta2);
        Delta3 := Delta3 + Delta2;
        Delta2 := Pos(':', Str2);
        Delta3 := Delta3 + Delta2 + 1;
        Line := Copy(Line, 1, Delta3);
        if not TryStrToInt(Copy(Str2, 1, Delta2 - 1), Column) then
          Column := 1;
        Delete(Str2, 1, Delta2 + 1);
        if Pos('error: ', Str2) = 1 then
          Delete(Str2, 1, 7);
        if FConfiguration.TranslateCompilerErrors then
        begin
          DeterminePrePost(I);
          Err := FConfiguration.TranslateCompilerError(Str2);
          applyMiddle;
        end
        else
        begin
          Pre := '';
          Err := Str2;
          Post := '';
        end;
        Line := Line + Pre + Err + Post;
        FMessages.LBCompiler.Items[I] := Line;
        if Assigned(Edit1) and (FileName = Path) then
          Edit1.SetErrorMark(LineNum, Column, Pre + Err + Post)
        else if Assigned(Edit2) and (Edit2.Pathname = Path) then
          Edit2.SetErrorMark(LineNum, Column, Pre + Err + Post)
        else
        begin
          Edit2 := TFEditForm(GetTDIWindowType(Path, '%E%'));
          if Assigned(Edit2) then
          begin
            Edit2.InitShowCompileErrors;
            Edit2.SetErrorMark(LineNum, Column, Pre + Err + Post);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFJava.DisableUpdateMenuItems;
begin
  Inc(FUpdateMenuItemsCount);
end;

procedure TFJava.EnableUpdateMenuItems;
begin
  Dec(FUpdateMenuItemsCount);
  if FUpdateMenuItemsCount = 0 then
    UpdateMenuItems(nil);
end;

function TFJava.GetActiveEditor: TFEditForm;
begin
  Result := nil;
  var
  Form := GetActiveForm;
  if Assigned(Form) then
    case Form.FormTag of
      1:
        Result := Form as TFEditForm;
      2:
        if Assigned(Form.Partner) then
          Result := (Form.Partner as TFEditForm);
    end;
end;

function TFJava.GetActiveUML: TFUMLForm;
begin
  Result := nil;
  var
  Form := GetActiveForm;
  if Assigned(Form) and (Form.FormTag = 2) then
    Result := Form as TFUMLForm;
end;

function TFJava.GetActiveForm: TFForm;
begin
  Result := GetSelectedTabForm;
end;

function TFJava.GetEditorWithMain: TFEditForm;
begin
  for var I := 0 to FTDIFormsList.Count - 1 do
    if (FTDIFormsList[I].FormTag = 1) and TFEditForm(FTDIFormsList[I]).HasMainInModel
    then
      Exit(TFEditForm(FTDIFormsList[I]));
  Result := nil;
end;

function TFJava.TDIEditFormCount: Integer;
begin
  Result := 0;
  for var I := 0 to FTDIFormsList.Count - 1 do
    if FTDIFormsList[I].FormTag = 1 then
      Inc(Result);
end;

function TFJava.TDIEditFormGet(Index: Integer): TFEditForm;
begin
  var
  I := -1;
  while (I < FTDIFormsList.Count) and (Index >= 0) do
  begin
    if FTDIFormsList[I + 1].FormTag = 1 then
      Dec(Index);
    Inc(I);
  end;
  if (-1 < I) and (I < FTDIFormsList.Count) then
    Result := TFEditForm(FTDIFormsList[I])
  else
    Result := nil;
end;

function TFJava.GetTabForm(Num: Integer): TFForm;
begin
  if Assigned(FMyTabBar.Tabs[Num]) and Assigned(FMyTabBar.Tabs[Num].Data) then
    Result := TTabObject(FMyTabBar.Tabs[Num].Data).Form
  else
    Result := nil;
end;

function TFJava.GetSelectedTabForm: TFForm;
begin
  if Assigned(FMyTabBar) and Assigned(FMyTabBar.SelectedTab) and
    Assigned(FMyTabBar.SelectedTab.Data) then
    Result := TTabObject(FMyTabBar.SelectedTab.Data).Form
  else
    Result := nil;
end;

procedure TFJava.HideGuiForms;
// during ChangeStyle GuiForm.Resize causes an exception
begin
  for var I := 0 to FTDIFormsList.Count - 1 do
    if FTDIFormsList[I] is TFGUIForm then
      (FTDIFormsList[I] as TFGUIForm).Hide;
end;

procedure TFJava.ShowGuiForms;
// unhiding doesn't work, so close and reopen
begin
  var
  StringList := TStringList.Create;
  for var I := 0 to FTDIFormsList.Count - 1 do
    if FTDIFormsList[I] is TFGUIForm then
    begin
      StringList.Add(FTDIFormsList[I].Pathname);
      (FTDIFormsList[I] as TFGUIForm).Close;
    end;
  for var Path in StringList do
    FJava.Open(Path);
  FreeAndNil(StringList);
end;

procedure TFJava.SetStyle(StyleName: string);
begin
  if StyleName <> TStyleManager.ActiveStyle.Name then
  begin
    HideGuiForms;
    TStyleManager.SetStyle(StyleName);
    FConfiguration.GUIStyle := StyleName;
    Application.ProcessMessages;
    ShowGuiForms;
  end;
end;

procedure TFJava.ChangeStyle(Style: string);
var
  AColor: TColor;
  Details: TThemedElementDetails;
begin
  if Style = '' then
    Exit;
  DragAcceptFiles(Handle, False);

  SetStyle(Style);

  for var I := 0 to FTDIFormsList.Count - 1 do
    FTDIFormsList[I].ChangeStyle;

  FMessages.ChangeStyle;
  FGUIDesigner.ChangeStyle;
  FTooltip.ChangeStyle;
  FLLMChatForm.ChangeStyle;
  if Assigned(FObjectInspector) then
    FObjectInspector.ChangeStyle;
  if Assigned(FJUnitTests) then
    FJUnitTests.ChangeStyle;
  if Assigned(FFileStructure) then
    FFileStructure.ChangeStyle;

  if TFConfiguration.IsDark then
  begin
    ToolbarProgram.Images := vilProgramDark;
    ToolbarAWT.Images := vilAWTDark;
    ToolBarSwing1.Images := vilSwing1Dark;
    ToolBarFXBase.Images := vilFXBaseDark;
    ToolBarFXShapes.Images := vilFXShapesDark;
    ToolBarLayout.Images := vilLayoutDark;
    MainToolBar.Images := vilToolbarDark;
    DebugToolbar.Images := vilToolbarDark;
    MainMenu.Images := vilMenuDark;
  end
  else
  begin
    ToolbarProgram.Images := vilProgramLight;
    ToolbarAWT.Images := vilAWTLight;
    ToolBarSwing1.Images := vilSwing1Light;
    ToolBarFXBase.Images := vilFXBaseLight;
    ToolBarFXShapes.Images := vilFXShapesLight;
    ToolBarLayout.Images := vilLayoutLight;
    MainToolBar.Images := vilToolbarLight;
    DebugToolbar.Images := vilToolbarLight;
    MainMenu.Images := vilMenuLight;
  end;

  if Assigned(FMyTabBar) then
  begin
    if StyleServices.IsSystemStyle then
    begin
      TJvModernTabBarPainter(FMyTabBar.Painter).Color := clBtnFace;
      // sets backgound of tabs
      TJvModernTabBarPainter(FMyTabBar.Painter).Font.Color := clBlack;
    end
    else
    begin
      Details := StyleServices.GetElementDetails(tbsBackground);
      StyleServices.GetElementColor(Details, ecFillColor, AColor);
      TJvModernTabBarPainter(FMyTabBar.Painter).Color := AColor;
      TJvModernTabBarPainter(FMyTabBar.Painter).Font.Color :=
        StyleServices.GetStyleFontColor(sfTabTextInactiveNormal);
    end;
  end;
  MyTabBarColorize;
  DragAcceptFiles(Handle, True);
end;

{
  TThemedTabSet = (
  tbsDontCare,
  tbsRoot,
  tbsBackground,
  tbsTabNormal, tbsTabSelected
  ); }

procedure TFJava.MyTabBarColorize;
var
  Path1, Path2: string;
  Form: TFForm;
  Details: TThemedElementDetails;
  SamepathColor: TColor;
begin
  Form := GetSelectedTabForm;
  if not Assigned(Form) and (FTDIFormsList.Count > 0) then
    Form := FTDIFormsList[0];
  if Assigned(Form) then
  begin
    if StyleServices.IsSystemStyle then
      SamepathColor := clWindow
    else
    begin
      Details := StyleServices.GetElementDetails(tbsTabNormal);
      StyleServices.GetElementColor(Details, ecFillColor, SamepathColor);
    end;
    Path1 := ExtractFilePath(Form.Pathname);
    for var I := 0 to FMyTabBar.Tabs.Count - 1 do
    begin
      Path2 := ExtractFilePath(TTabObject(FMyTabBar.Tabs[I].Data).Path);
      if (UpperCase(Path2) = UpperCase(Path1)) or (Form.FormTag in [4, 5, 6]) or
        (TTabObject(FMyTabBar.Tabs[I].Data).Form.FormTag in [4, 5, 6]) then
        FMyTabBar.Tabs[I].Color := SamepathColor
      else
        FMyTabBar.Tabs[I].Color := $CFFFFF;
    end;
    if Assigned(FMyTabBar.SelectedTab) then
      FMyTabBar.SelectedTab.Color := clActiveCaption;
  end;
end;

// TFileKind = (fkEditor, fkUML, fkGUI, fkTextDiff, fkBrowser, fkExplorer,
// fkStructogram, fkSequence);

function TFJava.FormFactory(FormKind: TFormKind): TFForm;
var
  AForm: TFForm;
begin
  AForm := nil;
  case FormKind of
    fkEditor:
      AForm := TFEditForm.Create(Self);
    fkUML:
      AForm := TFUMLForm.Create(Self);
    fkGUI:
      AForm := TFGUIForm.Create(Self);
    fkFXGUI:
      AForm := TFXGUIForm.Create(Self);
    fkTextDiff:
      AForm := TFTextDiff.Create(Self);
    fkBrowser:
      AForm := TFBrowser.Create(Self);
    fkExplorer:
      AForm := TFExplorer.Create(Self);
    fkStructogram:
      AForm := TFStructogram.Create(Self);
    fkSequence:
      AForm := TFSequenceForm.Create(Self);
  end;
  FTDIFormsList.Add(AForm);
  FActiveTDIChild := AForm;
  Result := AForm;
end;

procedure TFJava.DoExport(Pathname: string; Bitmap: TBitmap);
var
  Folder, Ext: string;

  procedure InToPng(const FileName: string);
  begin
    var
    Png := TPngImage.Create;
    Png.Assign(Bitmap);
    try
      Png.SaveToFile(FileName);
    finally
      FreeAndNil(Png);
    end;
  end;

  procedure InToBmp(const FileName: string);
  begin
    Bitmap.SaveToFile(FileName);
  end;

  procedure InToWMF(const FileName: string);
  var
    MetafileCanvas: TMetafileCanvas;
    DeviceContext: HDC;
    ScreenLogPixels: Integer;
  begin
    var
    Metafile := TMetafile.Create;
    try
      DeviceContext := GetDC(0);
      ScreenLogPixels := GetDeviceCaps(DeviceContext, LOGPIXELSY);
      Metafile.Inch := ScreenLogPixels;
      Metafile.Width := Bitmap.Width + 20;
      Metafile.Height := Bitmap.Height + 20;
      MetafileCanvas := TMetafileCanvas.Create(Metafile, DeviceContext);
      ReleaseDC(0, DeviceContext);
      try
        MetafileCanvas.Draw(0, 0, Bitmap);
      finally
        FreeAndNil(MetafileCanvas);
      end;
      Metafile.Enhanced := False;
      Metafile.SaveToFile(FileName);
    finally
      Metafile.Destroy;
    end;
  end;

begin
  with SDSaveAs do
  begin
    Title := _(LNGExportTo);
    Filter := 'PNG (*.png)|*.png|BMP (*.bmp)|*.bmp|WMF (*.wmf)|*.wmf|';
    Folder := ExtractFilePath(Pathname);
    if Folder <> '' then
      InitialDir := Folder
    else
      InitialDir := FConfiguration.Sourcepath;
    FileName := ChangeFileExt(Pathname, '');
    if Execute then
    begin
      if ExtractFileExt(FileName) = '' then
        case FilterIndex of
          1:
            FileName := FileName + '.png';
          2:
            FileName := FileName + '.bmp';
          3:
            FileName := FileName + '.wmf';
        end;
      if not FileExists(FileName) or FileExists(FileName) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
        mbYesNoCancel, 0) = mrYes) then
      begin
        Ext := LowerCase(ExtractFileExt(FileName));
        if Ext = '.png' then
          InToPng(FileName)
        else if Ext = '.bmp' then
          InToBmp(FileName)
        else if Ext = '.wmf' then
          InToWMF(FileName);
      end;
      FConfiguration.Sourcepath := ExtractFilePath(FileName);
    end;
  end;
end;

procedure TFJava.CloseBrowser;
begin
  if Assigned(FActiveTDIChild) and (FActiveTDIChild.FormTag = 1) then
    if TFEditForm(FActiveTDIChild).Editor.CanFocus then
      TFEditForm(FActiveTDIChild).Editor.SetFocus;
  UpdateMenuItems(Self);
end;

procedure TFJava.CloseTimerTimer(Sender: TObject);
begin
  CloseTimer.Enabled := False;
  DisableUpdateMenuItems;
  Close;
end;

procedure TFJava.ChangeLanguage(LangCode: string);
begin
  if CompareText(GetCurrentLanguage, LangCode) <> 0 then
  begin
    UseLanguage(LangCode);
    SetMenuKeyCaps;
    // otherwise the width of the main menu items will be miscalculated
    RetranslateComponent(Self);
    for var I := 0 to FTDIFormsList.Count - 1 do
      if FTDIFormsList[I] is TFUMLForm then
        (FTDIFormsList[I] as TFUMLForm).Retranslate
      else
        RetranslateComponent(FTDIFormsList[I]);
    RetranslateComponent(FConfiguration);
    RetranslateComponent(FGUIDesigner);
    RetranslateComponent(FObjectGenerator);
    RetranslateComponent(FWatches);
    RetranslateComponent(FMessages);
    RetranslateComponent(FTooltip);

    if Assigned(FFileStructure) then
      RetranslateComponent(FFileStructure);
    if FConfiguration.GitOK and Assigned(FGit) then
      RetranslateComponent(FGit);
    if Assigned(FJUnitTests) then
      RetranslateComponent(FJUnitTests);
    if Assigned(FObjectInspector) then
      RetranslateComponent(FObjectInspector);
    if FConfiguration.SubversionOK and Assigned(FSubversion) then
      RetranslateComponent(FSubversion);
  end;
  FConfiguration.ShowDefaultMindstormsAndroidConfiguration;
  FConfiguration.InitTreeView;
  // Workaround: "Brackets" is always shown as "Klammern"
  FConfiguration.RGColors.Items.Text := 'Java'#13#10'HTML'#13#10 +
    _('Brackets') + #13#10;
end;

procedure TFJava.SetMenuKeyCaps;
begin
  MenuKeyCaps[mkcBkSp] := _('BkSp');
  MenuKeyCaps[mkcTab] := _('Tab');
  MenuKeyCaps[mkcEsc] := _('Esc');
  MenuKeyCaps[mkcEnter] := _('Enter');
  MenuKeyCaps[mkcSpace] := _('Space');
  MenuKeyCaps[mkcPgUp] := _('PgUp');
  MenuKeyCaps[mkcPgDn] := _('PgDn');
  MenuKeyCaps[mkcEnd] := _('End');
  MenuKeyCaps[mkcHome] := _('Home');
  MenuKeyCaps[mkcLeft] := _('Left');
  MenuKeyCaps[mkcUp] := _('Up');
  MenuKeyCaps[mkcRight] := _('Right');
  MenuKeyCaps[mkcDown] := _('Down');
  MenuKeyCaps[mkcIns] := _('Ins');
  MenuKeyCaps[mkcDel] := _('Del');
  MenuKeyCaps[mkcBkSp] := _('BkSp');
  MenuKeyCaps[mkcShift] := _('Shift+');
  MenuKeyCaps[mkcCtrl] := _('Ctrl+');
  MenuKeyCaps[mkcAlt] := _('Alt+');
end;

procedure TFJava.ShowAWTSwingOrFX(FrameType: Integer);
var
  Api: Integer;
  TabControl: TSpTBXTabControl;
begin
  TabControl := FJava.TabsControl;
  Api := TabControl.ActiveTabIndex;
  for var I := 1 to MaxTab do // 0 is tab Program
    if FConfiguration.VisTabs[I] then
      case I of
        1:
          TabControl.Items[I].Visible := (FrameType in [0, 1, 2, 3, 4]);
        // Frame, Dialog, Applet
        2, 3:
          TabControl.Items[I].Visible := (FrameType in [0, 1, 5, 6, 7]);
        // JFrame, JDialog, JApplet
        4, 5:
          TabControl.Items[I].Visible := (FrameType < 8); // Layout, Utilities
        6, 7, 8:
          TabControl.Items[I].Visible := (FrameType in [0, 1, 8]);
        // FX Base, FX Controls, FX Shapes
      end;
  case FrameType of
    2 .. 4:
      TabControl.ActiveTabIndex := 1;
    5 .. 7:
      if (Api < 2) or (Api > 3) then
        TabControl.ActiveTabIndex := 2;
    8:
      if Api < 6 then
        TabControl.ActiveTabIndex := 6;
  else
    TabControl.ActiveTabIndex := 0;
  end;
end;

procedure TFJava.SetOptions;
begin
  for var I := TDIFormsList.Count - 1 downto 0 do
    TDIFormsList[I].SetOptions;
  ChangeLanguage(FConfiguration.LanguageCode);
  ODOpen.Filter := FConfiguration.GetFileFilters;
  FMessages.SetOptions;
end;

procedure TFJava.ThemeEditorGutter(Gutter: TSynGutter);
var
  GradColor: TColor;
begin
  // Delphi Styles
  if not StyleServices.GetElementColor
    (StyleServices.GetElementDetails(ttTabItemNormal), ecFillColor, GradColor)
    or (GradColor = clNone) then
    GradColor := StyleServices.GetSystemColor(clBtnFace);
  Gutter.Font.Color := StyleServices.GetSystemColor(clGrayText);

  with Gutter do
  begin
    BorderStyle := gbsNone;
    GradientStartColor := LightenColor(GradColor, 40);
    GradientEndColor := DarkenColor(GradColor, 20);
    Color := DarkenColor(GradColor, 4);
  end;
end;

{ --- Debugging ---------------------------------------------------------------- }

// var aIdle: integer;

procedure TFJava.AppOnIdle(Sender: TObject; var Done: Boolean);
// var freq, startTime, endTime: Int64;
var
  Str: string;
begin
  if FConfiguration.FixImports then
    Str := 'FixImports';
  if Assigned(FActiveTDIChild) then
  begin
    Str := 'Java-Editor - ' + FActiveTDIChild.Pathname;
    if FActiveTDIChild.Modified then
      Str := Str + '*';
  end
  else
    Str := 'Java-Editor';
  if Caption <> Str then
    Caption := Str;

  if not Assigned(FEditorForm) then
    FEditorForm := GetActiveEditor;

  if Assigned(FEditorForm) then
    FEditorForm.DoOnIdle;

  if Assigned(FEditorForm) and FEditorForm.IsJava and
    FEditorForm.NeedsParsing and ((FEditorForm.ParseThread = nil) or
    (FEditorForm.ParseThread.State <> 2)) then
  begin
    FEditorForm.ParseSourcecodeWithThread(False);
  end;

  if Assigned(FEditorForm) and FEditorForm.IsJava and
    not FEditorForm.NeedsParsing and Assigned(FEditorForm.ParseThread) and
    (FEditorForm.ParseThread.State = 3) then
  begin
    FEditorForm.CreateTVFileStructure;
    if Assigned(FEditorForm.ParseThread) then
      FEditorForm.ParseThread.State := 0;
  end;
  Done := True;
end;

procedure TFJava.AppOnMessage(var Msg: TMsg; var Handled: Boolean);
var
  AControl: TWinControl;
begin
  AControl := Screen.ActiveControl;
  // or Application.ActiveControl (sorry, don't remember)
  while Assigned(AControl) and not(AControl is TForm) do
    AControl := AControl.Parent;
end;

procedure TFJava.ActiveControlChanged(Sender: TObject);
begin
  FMessages.OutputToTerminal(Screen.ActiveCustomForm.Name + ' - ' +
    Screen.ActiveControl.Name);
end;

procedure TFJava.ActiveWindowTimerTimer(Sender: TObject);
var
  Str, Str1: string;
  AForm: TFForm;

begin
  Str := 'ActiveForm: ';
  AForm := GetActiveForm;
  if Assigned(AForm) then
    Str := Str + AForm.Pathname
  else
    Str := Str + 'nil';

  Str := Str + ' ActiveEditor: ';
  AForm := GetActiveEditor;
  if Assigned(AForm) then
    Str := Str + AForm.Pathname
  else
    Str := Str + 'nil';

  { n:= 0;
    for var i:= 0 to FTDIFormsList.count - 1 do begin
    AForm:= FTDIFormsList[i];
    if not (AForm is TFForm) then
    ErrorMsg('FTDIFormsList ist kein Formular!');
    if assigned(AForm) and (AForm is TFGUIForm) then
    inc(n);
    end;
  }

  {
    if assigned(FMessages) then
    Label1.Caption:=IntTostr(FMessages.ActiveSubTool);
  }

  Str := Str + ' ActiveFormtag: ' + IntToStr(GetActiveFormTag) + Str1;

  if Assigned(Screen.ActiveForm) then
    Str := 'ActiveCustomForm: ' + Screen.ActiveCustomForm.Name
  else
    Str := 'no ActiveForm';

  if Assigned(Screen.ActiveControl) and Assigned(Screen.ActiveControl.Parent)
  then
    Str := Str + ' - # ' + Screen.ActiveControl.Parent.Name
  else
    Str := Str + ' - ' + 'no parent';

  if Assigned(FActiveTDIChild) and Assigned(FActiveTDIChild.ActiveControl) then
    Str := Str + ' ActiveControl ' + FActiveTDIChild.ActiveControl.Name;
  { if assigned(Application.Mainform.ActiveControl) then
    s:= s + ' ActiveChild: ' + Application.Mainform.ActiveControl.ClassName
    else
    s:= 'FActiveTDIChild: ' + 'nil';
    Caption:= s; }

  {
    if assigned(FEditorForm)
    then s:= s + ' ' + ExtractFilename(FEditorForm.Pathname)
    else s:= s + ' nil';
    Label1.Caption:= s;
  }

  {
    if assigned(Screen.ActiveForm)
    then Label2.Caption:= 'ActiveTool: ' + IntTostr(ActiveTool);
  }

  {
    if assigned(FActiveTDIChild)
    then Label3.caption:= 'FActiveTDIChild.Caption: ' + FActiveTDIChild.Caption
    else Label3.Caption:= 'FActiveTDIChild.Caption: ' + 'nil';
    Label1.Caption:= 'ActiveTool: ' + IntTostr(ActiveTool);
  }
  {

    if FActiveTDIChild.Active
    then s:= 'Activ'
    else s:= 'not activ';
    if FActiveTDIChild.CanFocus
    then s:= s + '  canFocus '
    else s:= s + '  not canFocus ';

    if (FActiveTDIChild.FormTag = 2) then
    with (FActiveTDIChild as TFUMLForm) do begin
    // s:= '';
    if MainModul.Diagram.GetPanel.FisMoving then s:= s + ' isMoving';
    if MainModul.Diagram.GetPanel.FIsRectSelecting then s:= s + ' IsRectSelecting';
    // if MainModul.Diagram.GetPanel.FIsConnecting then s:= s + ' IsConnecting';
    if MainModul.Diagram.GetPanel.FSelectedOnly then s:= s + ' SelectedOnly';
    if MainModul.Diagram.GetPanel.MouseDownOK then s:= s + ' MouseDownOK';
    end;

    Label4.Caption:= 'ActiveForm.Status: ' + s;

    s:= intTostr(GetActiveFormTag);
    if assigned(FActiveTDIChild) then s:= s + ' - ' + IntToStr(FActiveTDIChild.Formtag);
    Label5.Caption:= 'ActiveTDIFormTag - .Formtag: ' + s;
    ShowActive;
  }

end;

// for debugging purpose
procedure TFJava.ShowActive;
var
  Str: string;
  AHWND: HWND;
  Control: TControl;
  CharArr: array [0 .. MAX_PATH] of Char;
begin
  if Assigned(Screen.ActiveCustomForm) then
    Str := 'CustomForm: ' + Screen.ActiveCustomForm.ClassName + '.' +
      Screen.ActiveCustomForm.Name + ' ';
  if Assigned(Screen.ActiveControl) then
    Str := Str + ' - ActiveControl: ' + Screen.ActiveControl.ClassName + '.' +
      Screen.ActiveControl.Name + ' ';
  AHWND := GetActiveWindow;
  Control := FindControl(AHWND);
  if not Assigned(Control) then
  begin
    if AHWND <> 0 then
    begin
      GetClassName(AHWND, @CharArr, MAX_PATH);
      Str := Str + ' - ActiveWindow (' + CharArr + ' ';
    end
    else
      Str := Str + ' - ActiveWindow ()';
  end
  else
    Str := Str + '(' + Control.ClassName + '.' + Control.Name + ' ';
  AHWND := GetFocus;
  Control := FindControl(AHWND);
  if not Assigned(Control) then
  begin
    if AHWND <> 0 then
    begin
      GetClassName(AHWND, @CharArr, MAX_PATH);
      Str := Str + CharArr + ') ';
    end
    else
      Str := Str + ' - Focus: ()';
  end
  else
    Str := Str + ' - Focus: ' + Control.ClassName + '.' + Control.Name + ') ';

  Application.Title := Str;
  for var I := 0 to Screen.CustomFormCount - 1 do
    Screen.CustomForms[I].Caption := Str;
end;

end.
