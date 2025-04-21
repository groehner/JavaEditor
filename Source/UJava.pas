{-------------------------------------------------------------------------------
 Unit:     UJava
 Author:   Gerhard Röhner
 Date:     August 2001
 Purpose:  The main form of the Java-Editor
-------------------------------------------------------------------------------}

unit UJava;

interface

uses
  Windows, Messages, Classes, Graphics, Forms, Controls, Dialogs,
  Buttons, ExtCtrls, Menus, ComCtrls, SyncObjs,
  Generics.Collections, JvTabbar, VirtualTrees,
  SynEdit, SynEditSearch, SynCompletionProposal, USynEditEx, UDockForm,
  UEditorForm, UBaseForm, UUMLForm, UStructogramform,
  USequenceForm, UBrowserForm, UGUIForm, Vcl.DdeMan, Vcl.ToolWin,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, TB2Item, SpTBXItem,
  TB2Dock, TB2Toolbar, SpTBXTabs, SpTBXDkPanels, SpTBXControls, Vcl.WinXCtrls;

const
  WindowOffset = 16;
  WithoutBackup = false;

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
    N19: TSpTBXSeparatorItem;
    MIFileNewClass: TSpTBXItem;
    MINewStructogram: TSpTBXItem;
    MINewSequencediagram: TSpTBXItem;
    N10: TSpTBXSeparatorItem;
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
    N15: TSpTBXSeparatorItem;
    MISave: TSpTBXItem;
    MISaveAs: TSpTBXItem;
    MISaveAll: TSpTBXItem;
    MISaveAllIn: TSpTBXItem;
    MISaveAsProject: TSpTBXItem;
    MICloseProject: TSpTBXItem;
    MIClose: TSpTBXItem;
    MICloseAllFiles: TSpTBXItem;
    MIExport: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    MIPrint: TSpTBXItem;
    MIPrintAll: TSpTBXItem;
    MIPrintSetup: TSpTBXItem;
    N9: TSpTBXSeparatorItem;
    MIExit: TSpTBXItem;
    MIEdit: TSpTBXSubmenuItem;
    MIUndo: TSpTBXItem;
    MIRedo: TSpTBXItem;
    N3: TSpTBXSeparatorItem;
    MICut: TSpTBXItem;
    MICopy: TSpTBXSubmenuItem;
    MICopyNormal: TSpTBXItem;
    MICopyRTF: TSpTBXItem;
    MICopyRtfNumbered: TSpTBXItem;
    MICopyHTML: TSpTBXItem;
    MICopyHTMLAsText: TSpTBXItem;
    MICopyNumbered: TSpTBXItem;
    MIPaste: TSpTBXItem;
    N4: TSpTBXSeparatorItem;
    MISearch: TSpTBXItem;
    MISearchAgain: TSpTBXItem;
    MIReplace: TSpTBXItem;
    MISearchInFiles: TSpTBXItem;
    N8: TSpTBXSeparatorItem;
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
    MIStrich7: TSpTBXSeparatorItem;
    MIHTMLforApplet: TSpTBXItem;
    MIHTMLforJavaPlugIn: TSpTBXItem;
    MIAppletviewer: TSpTBXItem;
    N6: TSpTBXSeparatorItem;
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
    N12: TSpTBXSeparatorItem;
    MIBreakpoint: TSpTBXItem;
    MIBreakpointsClear: TSpTBXItem;
    N13: TSpTBXSeparatorItem;
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
    N16: TSpTBXSeparatorItem;
    MISVNStatus: TSpTBXItem;
    MISVNTree: TSpTBXItem;
    MISVNUpdate: TSpTBXItem;
    MIGit: TSpTBXSubmenuItem;
    MIGitStatus: TSpTBXItem;
    MIGitAdd: TSpTBXItem;
    MIGitCommit: TSpTBXItem;
    MIGitLog: TSpTBXItem;
    N20: TSpTBXSeparatorItem;
    MIGitReset: TSpTBXItem;
    MIGitCheckout: TSpTBXItem;
    MIGitRemove: TSpTBXItem;
    N21: TSpTBXSeparatorItem;
    MIGitRemote: TSpTBXItem;
    MIGitFetch: TSpTBXItem;
    MIGitPush: TSpTBXItem;
    N22: TSpTBXSeparatorItem;
    MIGitGUI: TSpTBXItem;
    MIGitViewer: TSpTBXItem;
    MIGitConsole: TSpTBXItem;
    MIJUnit: TSpTBXSubmenuItem;
    MIJUnitRunAllTests: TSpTBXItem;
    MIJUnitCreateTestclass: TSpTBXItem;
    N17: TSpTBXSeparatorItem;
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
    N5: TSpTBXSeparatorItem;
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
    N14: TSpTBXSeparatorItem;
    MIMaximized: TSpTBXItem;
    MICascade: TSpTBXItem;
    MITileVertical: TSpTBXItem;
    MITileHorizontal: TSpTBXItem;
    N11: TSpTBXSeparatorItem;
    MIHelp: TSpTBXSubmenuItem;
    MIHelpHelp: TSpTBXItem;
    MIAPI: TSpTBXItem;
    MIJavaFx: TSpTBXItem;
    MIJunitManual: TSpTBXItem;
    N7: TSpTBXSeparatorItem;
    MIDemos: TSpTBXItem;
    MITutorial: TSpTBXItem;
    MIJavabook: TSpTBXItem;
    MIMindstormsHelp: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
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
    procedure MINewClick(Sender: TObject);           // File menu
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure MIPrintClick(Sender: TObject);
    procedure MIPrintSetupClick(Sender: TObject);
    procedure MIMenuOpenClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIUndoClick(Sender: TObject);          // Edit menu
    procedure MICutClick(Sender: TObject);
    procedure MICopyRTFClick(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure MISearchClick(Sender: TObject);
    procedure MIReplaceClick(Sender: TObject);
    procedure MIGotoLineClick(Sender: TObject);
    procedure MIUnindentClick(Sender: TObject);
    procedure MIIndentClick(Sender: TObject);

    procedure MICompileClick(Sender: TObject);       // Start menu
    procedure MIRunClick(Sender: TObject);
    procedure MIProgramResetClick(Sender: TObject);
    procedure MIHTMLforAppletClick(Sender: TObject);
    procedure MIAppletviewerClick(Sender: TObject);
    procedure MIDissasemblerClick(Sender: TObject);
    procedure MIJavaDocClick(Sender: TObject);

    procedure MIDefaultLayoutClick(Sender: TObject);
    procedure MICascadeClick(Sender: TObject);
    procedure Tile(horizontal: boolean);
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

    {--- Kontrollstrukturen }
    procedure SBKontrollstrukturenClick(Sender: TObject);

    {--- Programm }
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
    procedure ShowDockPanel(APanel: TPanel; MakeVisible: Boolean; Client: TControl);
    procedure DockPanelDockDrop(Sender: TObject;
              Source: TDragDockObject; X, Y: Integer);
    procedure BottomDockPanelDockOver(Sender: TObject;
              Source: TDragDockObject; X, Y: Integer; state: TDragState;
              var Accept: Boolean);
    procedure BottomDockPanelUnDock(Sender: TObject; Client: TControl;
              NewTarget: TWinControl; var Allow: Boolean);
    procedure RightDockPanelUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure RightDockPanelDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; state: TDragState; var Accept: Boolean);
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
      X, Y: Integer; state: TDragState; var Accept: Boolean);
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
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure scpParamsExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure scpJavaOnCodeCompletion(Sender: TObject; var Value: string;
      Shift: TShiftState; Index: Integer; EndToken: Char);
    procedure scpJavaAfterCodeCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer;
      EndToken: Char);
    procedure scpJavaChange(Sender: TObject; AIndex: Integer);
    procedure scpJavaClose(Sender: TObject);
    procedure scpJavaShow(Sender: TObject);
    procedure scpHandleMethods(Editor: TSynEdit; Index: integer; EndToken: Char);

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
    procedure setActiveTool(value: integer);

    procedure AppOnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure ActiveControlChanged(Sender: TObject);
    procedure MIGitClick(Sender: TObject);
    procedure CloseTimerTimer(Sender: TObject);
    procedure StyleTimerTimer(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
    procedure TBCompileJavaClick(Sender: TObject);
    procedure MIRecognizeAssociationsClick(Sender: TObject);
    procedure MIChatClick(Sender: TObject);
  private
    FixedWindows: array of TFForm;
    EditClass: string;
    EditClassStatus: string;
    procedure PrepareStep;
    procedure StepNextUp(const s: string);
    procedure SaveBeforeCompile;
    procedure Compile;
    procedure myTabBarDblClick(Sender: TObject);
    procedure myTabBarClosed(Sender: TObject; Item: TJvTabBarItem);
    procedure ClassEdit;
    procedure EditorSaveAs(aForm: TFForm; Hidden: boolean);
    procedure StructogramSaveAs(Structogram: TFStructogram);
    procedure SequenceSaveAs(SequenceDiagram: TFSequenceForm);
    function getTDIType(const Typ: string): TFForm;
    function getTab(Nr: integer): integer;
    function getWindowMenuNr(Nr: integer): integer;
    procedure NewGUIForm(const aFile: string; state: string);
    procedure NewUMLWindow(const aFile, state: string);
    function  NewEditform(Hidden: boolean): TFEditForm;
    procedure FixWindows(WithHidden: boolean);
    procedure JarShowUnpackOpen(i: integer; const Filename: string);
    procedure JarCall(const call, dir: string);
    procedure JarOpen(const Filename: string);
    function getUMLWindow: TFUMLForm;
    procedure LoadBounds;
    procedure OpenFileStructureAndObjectInspector;
  public
    scpJava: TSynCompletionProposal;
    scpParams: TSynCompletionProposal;
    PrintDialog: TPrintDialog;
    PrinterSetup: TPrinterSetupDialog;
    SynEditSearch: TSynEditSearch;
    scpParamIndex: integer;
    scpJavaIsMethod: boolean;
    AfterCodeCompletion: boolean;
    Search: Boolean;
    Searched: string;
    aUMLForm: TFUMLForm;
    ProjectFilename: string;
    LastDoubleClickTime: TDateTime;
    TimerStatus: integer;
    myTabBar: TJvTabBar;
    Closing: boolean;
    ScpState: integer;
    idle, idle2: integer;
    Lock: TCriticalSection;
    UpdateMenuItemsCount: integer;
    EditorForm: TFEditForm;
    ActiveTDIChild: TFForm;
    FActiveTool: integer;
    TDIFormsList: TFormsList;
    InteractiveUMLForm: TFUMLForm;
    MakeUpdate: boolean;

    procedure WMCOPYDATA(var msg: TWMCopyData); message WM_COPYDATA;
    function hasEditforms: boolean;
    function hasJavaFiles: boolean;
    function hasPascalFiles: boolean;
    function IsJavaApplet(Editform: TFEditForm): Boolean;
    procedure CloseBrowser;
    procedure CallHelp(const address: string); overload;
    procedure CallHelp(Applet: Boolean; address: string); overload;
    procedure CallApplet(const address: string);
    function  WindowOpened(const Pathname: string; var aForm: TFForm): boolean;
    function  getTDIWindow(const Pathname: string): TFForm; overload;
    function  getTDIWindow(Number: integer): TFForm; overload;
    function  getTDIWindowType(const Pathname, Typ: string): TFForm;
    function getGuiForm(Pathname: string): TFGUIForm;
    procedure SwitchToWindow(const path: string); overload;
    procedure SwitchToWindow(Nr: integer); overload;
    procedure SwitchToWindow(Form: TFForm); overload;
    procedure SwitchWindowWithSearch(const Pathname: string);
    procedure ChangeWindowWithPositioning(const Pathname: string; XPos, YPos: Integer; error: boolean);
    function GetPathnameForClass(const Classname: string): string;
    function  OpenWindowWithClass(const Directory, aClass: string): boolean;
    procedure SetBreakpoints;
    function  PreCompile(aForm: TFEditForm; Pathname: string): boolean;
    procedure DoRunApplet(const JavaProgramm: string; E: TFEditForm);
    procedure DoRunProgram(const JavaProgram, AParameter: string; E: TFEditForm);
    procedure DoRunHTML(const Pathname: string);
    function getEditFormWithMain: TFEditForm;
    procedure DoRun(EditForm: TFEditForm; const Pathname: string);
    procedure RunButtonToStop(Stop: Boolean);
    procedure CompileButtonToStop(OnOff: Boolean);
    function Open(const Filename: string): Boolean;
    function OpenEditForm(const Pathname: string; hidden: boolean): TFEditForm;
    procedure CloseFile(const Filename: string);
    procedure RunAndroid;
    procedure UpdateMenuItems(Sender: TObject);      // MI = MenuItem
    procedure DoSave(aForm: TFForm; WithBackup: boolean);
    procedure AddToWindowMenuAndTabBar(Nr: integer;
                NotifyEvent: TNotifyEvent; Form: TFForm);
    procedure AddToTabBar(Nr: integer; Form: TFForm);
    procedure SetSelectedTabAndWindow(Nr: integer); overload;
    procedure SetSelectedTabAndWindow(const Pathname: string); overload;
    procedure RenameTabAndWindow(Nr: integer; const Neu: string);
    procedure DeleteTabAndWindow(Nr: integer);
    procedure DeleteTab(Nr: integer);
    procedure TabModified(Nr: integer; Modified: boolean);
    procedure myTabBarColorize;
    procedure myTabBarClick(Sender: TObject);

    function NewBrowser(Address: string; const state: string): TFBrowser;
    procedure NewEditor(const Path: string; state: string);
    procedure NewExplorer(const dir: string; state: string);
    function NewStructogram(const Filename: string; state: string): boolean;
    function NewSequenceDiagram(const Filename: string; state: string): boolean;
    function MakeNewUMLWindow(const Filename, state: string): TFUMLForm;
    procedure NewTextDiff(F1, F2: TFEditForm);
    procedure SearchInIndex;
    procedure OpenFileWithState(const s: string);
    procedure OpenProject(const Filename: string);
    procedure DoSaveAsProject(const Filename: string);
    procedure ReadHistory;

    procedure UMLSaveAs(UMLForm: TFUMLForm);
    function OpenUMLWindow(const Filename, state: string): TFUMLForm;
    function OpenStructogram(const Filename, state: string): boolean;
    function OpenSequenceDiagram(const Filename, state: string): boolean;
    procedure ConnectGUIAndJavaWindow(GUIForm: TFGUIForm);
    function GuiDesignerOpen: boolean;
    procedure MoveHSplitter(aHeight: integer);
    procedure MoveVSplitter(aWidth: integer);
    procedure SaveBounds;
    function getFilename(const Extension: string; path: string = ''): string;
    procedure DoDisassemble(const Filename: string);
    procedure RearrangeFileHistory(const NewFile: string);
    procedure OpenFiles;
    procedure ShowSearchReplaceDialog(Editor: TSynEditEx; AReplace: boolean);
    procedure DoSearchReplaceText(Editor: TSynEditEx; AReplace: boolean);
    procedure NotFound(Editor: TSynEdit; Backwards: boolean);  // Ex
    function IsAValidClass(const Pathname: string): boolean;
    procedure RefreshUMLWindows;
    procedure CompileList(SL: TStringList);
    procedure CompileOneWith(const Pathname: string);
    procedure DoOpenInUMLWindow(const Pathname: string);
    procedure DoJarCreate(Filename, Package: string);
    procedure DoJarCreateEV3(const Filename: string);
    procedure DoHTMLForApplet(EditForm: TFEditForm; ForceCreate, PlugIn, aShow, Debug: boolean; var Applet: string);
    function PreparePrint(Sender: TObject): boolean;
    function getActiveTDIFormPath: string;
    function ExtentClasspath(cp: string): string;
    function getAllPathnames: TStringList;
    function getAllClassnames: TStringList;
    function GetActiveFormTag: integer;
    procedure Restart;
    procedure Run(const Pathname: string);
    function  hasBreakpoints: Boolean;
    procedure setEditformWithBreakpoints;
    procedure AppOnIdle(Sender: TObject; var Done: boolean);
    procedure StructogramFromText(sourcecode, pathname: string);
    procedure ShowActive;
    procedure SaveDocking;
    procedure LoadDocking;
    procedure ShowCompileErrors;
    procedure ResetToolbars;
    procedure NewTestClass(const Pathname: string);
    procedure UpdateLayoutRightDockPanel(SetWidthHeight: boolean = false);
    function hasSomethingToSave: boolean;
    procedure ImperativeUpdateMenuItems;
    procedure DisableUpdateMenuItems;
    procedure EnableUpdateMenuItems;

    function getActiveEditor: TFEditForm;
    function getActiveUML: TFUMLForm;
    function getActiveForm: TFForm;
    function getEditorWithMain: TFEditForm;
    function TDIEditFormCount: integer;
    function TDIEditFormGet(index: integer): TFEditForm;
    function getTabForm(i: integer): TFForm;
    function getSelectedTabForm: TFForm;
    procedure HideGuiForms;
    procedure ShowGuiForms;
    procedure ChangeStyle(Style: string);
    procedure SetStyle(StyleName: string);
    function FormFactory(FormKind: TFormKind): TFForm;
    procedure DoExport(Pathname: string; Bitmap: TBitmap);
    procedure ChangeLanguage(LangCode: string);
    procedure SetMenuKeyCaps;
    procedure SetDockTopPanel;
    procedure ShowAWTSwingOrFX(FrameType: integer);
    property ActiveTool: integer read FActiveTool write setActiveTool;
  end;

var FJava: TFJava;

implementation

{$R *.DFM}

uses SysUtils, ShellAPI, StdCtrls, Themes, StrUtils, IOUtils,
  Vcl.Imaging.pngimage, Math, CommCtrl, GraphUtil, Types, Printers, UITypes,
  SHDocVw, MadExcept, SynEditTypes,
  UDlgAbout, UConfiguration, UDlgGotoLine, UObjectGenerator, UDlgParameter,
  UNTProcess, USaveDialog, UDlgHelp, UDlgGrepSearch,
  UUMLModule, UDlgEvaluate, UWatches, UMessages, UUtils,
  UModel, uCodeCompletion, UScpHint, UWindow, UDlgUpdate,
  UObjectInspector, UGUIDesigner, UDlgClassEditor, UJavaCommands,
  URtfdDiagram, UDlgSearch, UDlgReplace, UClassInsert,
  UTextDiffForm, USubversion, UDlgUnicode, URegExSearch, USearchOptions,
  UComJava1, UDlgConfigureTools, UDlgMindstorms, UDlgMindstormsEV3,
  UConjoinHost, UTabHost, UDebugger, UTooltip, UFXGuiForm, UFileStructure,
  UDlgMessage, UTabObject, UGit, UJUnitTest, UExplorerForm, UImages,
  UTemplates, JvGnugettext, UStringRessources, UJEComponents, ULLMChatForm;

{--- TFJava -------------------------------------------------------------------}

procedure TFJava.FormCreate(Sender: TObject);
  var aColor: TColor; Details: TThemedElementDetails;
begin
  Application.ProcessMessages;  // to show proper gui style
  FConfiguration:= TFConfiguration.Create(Self);
  FGUIDesigner:= TFGUIDesigner.Create(Self);
  FObjectGenerator:= TFObjectGenerator.Create(Self);
  FWatches:= TFWatches.Create(Self);
  FMessages:= TFMessages.Create(Self);
  FWindow:= TFWindow.Create(Self);
  FScpHint:= TFScpHint.Create(Self);
  FEvaluate:= TFEvaluate.Create(Self);
  FTooltip:= TFTooltip.Create(Self);
  FLLMChatForm:= TLLMChatForm.Create(Self);
  myJavaCommands:= TJavaCommands.Create;
  myDebugger:= TDebugger.Create;
  mySearchOptions:= TSearchOptions.Create;
  TabDockHost:= TTabDockHost.Create(Self);
  ConjoinDockHost:= TConjoinDockHost.Create(Self);

  SetMenuKeyCaps;
  TranslateComponent(Self);
  idle:= 0;
  idle2:= 0;
  UpdateMenuItemsCount:= 0;
  ActiveTDIChild:= nil;
  SynEditSearch:= TSynEditSearch.Create(Self);
  scpJava:= TSynCompletionProposal.Create(Self);
  with scpJava do begin
    DefaultType:= ctCode;
    Options:= [scoLimitToMatchedText, scoTitleIsCentered, scoUseInsertList,
               scoUsePrettyText, scoUseBuiltInTimer, scoCompleteWithEnter,
               scoConsiderWordBreakChars];
    TriggerChars:= '.';
    EndOfTokenChr:= '()[]. ';
    ShortCut:= Menus.ShortCut(Word(' '), [ssCtrl]);
    TimerInterval:= 0;
    OnChange:= scpJavaChange;
    OnClose:= scpJavaClose;
    OnExecute:= scpJavaExecute;
    OnShow:= scpJavaShow;
    OnCodeCompletion:= scpJavaOnCodeCompletion;
    OnAfterCodeCompletion:= scpJavaAfterCodeCompletion;
    //Images:= DMImages.ILFileStructure;      // make an exception
  end;
  scpParams:= TSynCompletionProposal.Create(Self);
  with scpParams do begin
    DefaultType:= ctParams;
    Options:= [scoLimitToMatchedText, scoUseBuiltInTimer, scoEndCharCompletion,
               scoCompleteWithTab, scoCompleteWithEnter];
    TriggerChars:= '(';
    ShortCut:= Menus.ShortCut(Word(' '), [ssShift, ssCtrl]);
    TimerInterval:= 1;
    OnExecute:= scpParamsExecute;
    OnPaintItem:= scpParamsPaintItem;
  end;
  myTabBar:= TJvTabBar.create(self);
  myTabBar.Parent:= Self;
  myTabBar.Top:= 200;
  myTabBar.Width:= 1600;
  myTabBar.AllowTabMoving:= true;
  myTabBar.HotTracking:= true;
  myTabBar.SelectBeforeClose:= false;
  myTabBar.OnClick    := myTabBarClick;
  myTabBar.OnTabClosed:= myTabBarClosed;
  myTabBar.OnDblClick := myTabBarDblClick;

  myTabBar.Painter:= TJvModernTabBarPainter.create(self);
  TJvModernTabBarPainter(myTabBar.Painter).MoveDividerColor:= clRed;
  TJvModernTabBarPainter(myTabBar.Painter).SelectedFont.Color:= clBlack;
  if StyleServices.IsSystemStyle then
    TJvModernTabBarPainter(myTabBar.Painter).Color:= clBtnFace  // sets backgound of tabs
  else begin
    Details:= StyleServices.GetElementDetails(tbsBackground);
    StyleServices.GetElementColor(Details, ecFillColor, aColor);
    TJvModernTabBarPainter(myTabBar.Painter).Color:= aColor;
  end;
  MIRedo.Shortcut:= ShortCut(Word('Z'), [ssShift, ssCtrl]);
  MIUnindent.Shortcut:= ShortCut(Word('U'), [ssShift, ssCtrl]);
  MIIndent.Shortcut:= ShortCut(Word('I'), [ssShift, ssCtrl]);
  MIProgramReset.Enabled:= False;
  TDIFormsList:= TFormsList.Create(false);
  TabsControl.ActivePage:= TSProgram;
  TimerStatus:= -1;
  Randomize;
  LastDoubleClickTime:= Time;
  Application.OnIdle:= AppOnIdle;
  // Screen.OnActiveControlChange:= ActiveFormChange;
  Screen.HintFont.Size:= 10;
  // Create critical section and threads
  Lock:= TCriticalSection.Create;
  DragAcceptFiles(Self.Handle, True);
  UseLatestCommonDialogs:= true;
  InteractiveUMLForm:= nil;
  ReadHistory;
  Screen.MenuFont.Name:= 'Segoe UI';
  Screen.MenuFont.Size:= 9;
  MakeUpdate:= false;
  FConfiguration.Init;
  LoadBounds;
  TThread.ForceQueue(nil, OpenFiles);
end;

procedure TFJava.OpenFileStructureAndObjectInspector;
begin
  FFileStructure:= TFFileStructure.create(Self);
  FFileStructure.Visible:= FConfiguration.ReadBoolU('FileStructure', 'Visible', true);
  FFileStructure.Height:= 300;
  FFileStructure.Width:= 200;
  FObjectInspector:= TFObjectInspector.create(Self);
  FObjectInspector.Visible:= FConfiguration.ReadBoolU('ObjectInspector', 'Visible', true);
  myCodeCompletion:= TCodeCompletion.Create;
  if FFileStructure.Visible or FObjectInspector.Visible or assigned(FJunitTests) and FJunitTests.Visible
    then LoadDocking
    else begin
      RightDockPanel.Width:= 0;
      VSplitter.Visible:= false;
    end;
end;

procedure TFJava.FormDestroy(Sender: TObject);
begin
  FreeAndNil(mySearchOptions);
  FreeAndNil(myRegExSearch);
  FreeandNil(myCodeCompletion);
  FreeAndNil(myDebugger);
  FreeAndNil(myJavaCommands);
  FreeAndNil(FClassInsert);
  FreeAndNil(SynEditSearch);
  FreeAndNil(scpJava);
  FreeAndNil(scpParams);
  // FreeAndNil(FScpHint); runtime error
  FreeAndNil(myTabBar);
  FreeAndNil(Lock);
  FreeAndNil(TDIFormsList);
  FreeAndNil(FJunitTests);
  AllClassesPackageClose;
end;

procedure TFJava.MINewClick(Sender: TObject);
begin
  var s:= '';
  var EditForm:= TFEditForm(FormFactory(fkEditor));
  if (Sender is TToolButton) or (Sender is TSpeedButton) then
    s:= '.java'
  else
    case (Sender as TSpTBXItem).Tag of
      1: s:= '.java';
      2: s:= '.txt';
      3: s:= '.html';
    end;
  EditForm.New(getFilename(s));
end;

procedure TFJava.MINewCommentClick(Sender: TObject);
begin
  if ActiveTDIChild.FormTag = 2 then
    (ActiveTDIChild as TFUMLForm).TBCommentClick(Self);
end;

procedure TFJava.SBNewClick(Sender: TObject);
begin
  MINewClick(Sender);
end;

procedure TFJava.SBClassClick(Sender: TObject);  // new Class
begin
  DisableUpdateMenuItems;
  var EditForm:= NewEditform(true);
  if Assigned(EditForm) then begin
    LockWindow(FJava.Handle);
    FTemplates.SBNewClass(EditForm);
    var UMLForm:= getActiveUML;
    if (myTabBar.Tabs.Count = 0) or not assigned(UMLForm) then
      aUMLForm:= MakeNewUMLWindow(ChangeFileExt(EditForm.Pathname, '.uml'), '')
    else begin
      aUMLForm:= UMLForm;
      aUMLForm.MainModul.UnSelectAllElements;
    end;
    aUMLForm.ConfigureWindow(Self);
    PrepareClassEdit(EditForm.Pathname, 'New', aUMLForm);
    UnlockWindow;
  end;
  EnableUpdateMenuItems
end;

procedure TFJava.MIOpenClick(Sender: TObject);
  var i: Integer; s, path: string; 

  procedure InitDir;
  begin
    with ODOpen do begin
      InitialDir:= FConfiguration.Sourcepath;
      if not SysUtils.DirectoryExists(InitialDir) then
        InitialDir:= GetDocumentsPath;
    end;
  end;

begin
  // http://www.installationexcellence.com/articles/VistaWithDelphi/Original/Index.html
  DisableUpdateMenuItems;
  // setRegPos(80);
  with ODOpen do begin
    if assigned(ActiveTDIChild) then begin
      path:= (ActiveTDIChild as TFForm).Pathname;
      InitialDir:= ExtractFilePath(Path);
      if InitialDir + '\' = FConfiguration.TempDir then
        InitDir
    end else
      InitDir;
    Filename:= '';
    FilterIndex:= 1;
    Title:= _(LNGOpenFile);
    if Execute then begin
      FConfiguration.Sourcepath:= ExtractFilePath(Filename);
      if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 4) then
        if Files.Count >= 2
          then (ActiveTDIChild as TFTextDiff).Open(Files.Strings[0], Files.Strings[1])
          else (ActiveTDIChild as TFTextDiff).Open(Filename)
      else
        try
          Screen.Cursor:= crHourglass;
          LockWindow(FJava.Handle);
          for i:= 0 to Files.Count - 1 do begin
            s:= Files.Strings[i];
            if Open(s) then RearrangeFileHistory(s);
          end;
        finally
          UnlockWindow;
          Screen.Cursor:= crDefault;
        end;
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIMenuOpenClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var s:= (Sender as TSpTBXItem).Caption;
  try
    Screen.Cursor:= crHourglass;
    LockWindow(FJava.Handle);
    s:= Right(s, Pos(' ', s) + 1);
    s:= ReplaceStr(s, '&', ''); // temporarily
    if Open(s) then RearrangeFileHistory(s);
    FConfiguration.Sourcepath:= ExtractFilePath(s);
  finally
    UnlockWindow;
    Screen.Cursor:= crDefault;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MICloseClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if assigned(ActiveTDIChild) then
    ActiveTDIChild.Close;
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
    Screen.Cursor:= crHourglass;
    LockFormUpdate(Self);
    if ProjectFilename <> '' then begin
      DoSaveAsProject(Projectfilename);
      RearrangeFileHistory(ProjectFilename);
      FConfiguration.RestoreConfigurationAfterProject;
    end;
    for var i:= TDIFormsList.Count - 1 downto 0 do begin
      Application.ProcessMessages;
      TDIFormsList[i].Close;
    end;
    Application.ProcessMessages;
    ProjectFilename:= '';
    FMessages.StatusMessage('');
  finally
    UnlockFormUpdate(Self);
    Screen.Cursor:= crDefault;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MISaveClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) then
    DoSave(ActiveTDIChild, FConfiguration.CreateBAKFiles);
end;

procedure TFJava.FixWindows(WithHidden: boolean);
  var i: integer;
begin
  if WithHidden then begin
    SetLength(FixedWindows, TDIFormsList.Count);
    for i:= 0 to TDIFormsList.Count - 1 do
      FixedWindows[i]:= TDIFormsList[i];
  end else begin
    SetLength(FixedWindows, myTabBar.Tabs.Count);
    for i:= 0 to myTabBar.Tabs.Count - 1 do
      FixedWindows[i]:= getTabForm(i);
  end;
end;

procedure TFJava.MISaveAllInClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var dir:= FConfiguration.Sourcepath;
  if not SysUtils.DirectoryExists(dir) then
    if assigned(EditorForm)
      then dir:= ExtractFilePath(EditorForm.Pathname)
      else dir:= '';
  {$WARNINGS OFF}
  FConfiguration.FolderDialog.DefaultFolder:= dir;
  if FConfiguration.FolderDialog.Execute then begin
    FixWindows(false);
    FConfiguration.Sourcepath:= FConfiguration.FolderDialog.Filename;
    for var i:= 0 to Length(FixedWindows) - 1 do
      FixedWindows[i].SaveIn(withTrailingSlash(FConfiguration.Sourcepath));
  end;
  {$WARNINGS ON}
  if CanFocus then SetFocus;
  EnableUpdateMenuItems
end;

procedure TFJava.MISaveAllClick(Sender: TObject);
begin
  FixWindows(true);
  for var i:= 0 to Length(FixedWindows) - 1 do
    if FixedWindows[i].Modified then
      DoSave(FixedWindows[i], FConfiguration.CreateBAKFiles);
end;

procedure TFJava.DoSave(aForm: TFForm; WithBackup: boolean);
begin
  if aForm = nil then exit;
  if (aForm.FormTag = 1) and (aForm.DefaultFilename or
     (aForm.Pathname <> aForm.GetSaveAsName))
    then EditorSaveAs(aForm, false)
  else if (aForm.FormTag = 2) and aForm.DefaultFilename
    then UMLSaveAs(aForm as TFUMLForm)
  else if (aForm.FormTag = 11) and aForm.DefaultFilename
    then StructogramSaveAs(aForm as TFStructogram)
  else if (aForm.FormTag = 14) and aForm.DefaultFilename
    then SequenceSaveAs(aForm as TFSequenceForm)
  else aForm.Save(WithBackup);
end;

procedure TFJava.MISaveAsClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if assigned(ActiveTDIChild) then
    case ActiveTDIChild.FormTag of
      1: EditorSaveAs(ActiveTDIChild, false);
      2: UMLSaveAs(ActiveTDIChild as TFUMLForm);
     11: StructogramSaveAs(ActiveTDIChild as TFStructogram);
     14: SequenceSaveAs(ActiveTDIChild as TFSequenceForm);
    end;
  EnableUpdateMenuItems
end;

procedure TFJava.EditorSaveAs(aForm: TFForm; Hidden: boolean);
  var dir, OldPartner, NewPartner, Ext, SaveAsName, fPath, fName: string;
      Form2: TFForm; Editform: TFEditForm; GuiForm: TFGuiForm;
      SaveDialog: TExtSaveDialog;
      OpenGuiForm: boolean;
begin
  EditForm:= aForm as TFEditForm;
  if EditForm.AlreadySavedAs then exit;
  if Hidden
    then EditForm.Hide
    else SwitchToWindow(EditForm.Number);
  SaveDialog:= TExtSaveDialog.CreateWith(Self,
      EditForm.Encoding + '/' +
      EditForm.LinebreakAsString, _('Encoding') + ':');
  try
    with SaveDialog do begin
      Title:= _(LNGSaveAs);
      Filter:= 'Java (*.java)|*.java|HTML (*.html)|*.html;*.htm|Text (*.txt)|*.txt|' + _(LNGAll) + ' (*.*)|*.*';
      Ext:= '.java';
      SaveAsName:= EditForm.GetSaveAsName;
      Filename:= SaveAsName;
      FilterIndex:= getFilterIndex(Filter, Filename);
      dir:= ExtractFilePath(Filename);
      if dir <> ''
        then InitialDir:= dir
        else InitialDir:= FConfiguration.Sourcepath;
      if Execute then begin
        fPath:= ExtractFilepath(Filename);
        fName:= ExtractFilename(Filename);
        fName:= WithoutSpaces(fName);
        Filename:= fPath + fName;
        Filename:= AddFileExt(Filename, filter, Ext, filterindex);
        if SysUtils.SameText(Filename, SaveAsName) then  // sensitive
          Filename:= SaveAsName;

        Form2:= getTDIWindowType(Filename, '%E%');
        if assigned(Form2) and (Form2 <> EditForm) and not Form2.visible then begin
          Form2.Close;
          Form2:= nil;
        end;
        if (length(fname) > 0) and (isWordBreakChar(fName[1]) or isDigit(fName[1])) then
           ErrorMsg(_('Invalid class name') + ': ' + ChangeFileExt(fName, ''))
        else if not EditForm.ClassnameDifferentFromAncestors(ChangeFileExt(fName, '')) then
           ErrorMsg(_('Invalid class name') + ': ' + ChangeFileExt(fName, ''))
        else if assigned(Form2) and (Form2 <> EditForm) then
          InformationMsg(Format(_(LNGFileAlreadyOpen), [Filename]))
        else if IsWriteProtected(Filename) then
          ErrorMsg(Filename + ' ' + _(LNGWriteprotected))
        else if IsWriteProtected(ChangeFileExt(Filename, '.jfm')) then
          ErrorMsg(ChangeFileExt(Filename, '.jfm') + ' ' + _(LNGWriteprotected))
        else if not FileExists(Filename) or (EditForm.Pathname = Filename) or FileExists(Filename) and
            (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                         mtConfirmation, mbYesNoCancel, 0) = mrYes)
        then begin
          EditForm.SetEncoding(Encoding);
          OldPartner:= ChangeFileExt(EditForm.Pathname, '.jfm');
          NewPartner:= ChangeFileExt(Filename, '.jfm');
          GuiForm:= getGuiForm(OldPartner);
          OpenGuiForm:= assigned(GuiForm);
          if Assigned(GuiForm) then begin
            GuiForm.SaveAs(NewPartner);
            GuiForm.Close;
            FreeAndNil(GuiForm);
          end else if FileExists(OldPartner) and (ExtractFileExt(OldPartner) = '.jfm') then
            CopyFile(PChar(OldPartner), PChar(NewPartner), false);
          EditForm.SaveAs(Filename);
          if OpenGuiForm then
            Open(NewPartner);
          FConfiguration.Sourcepath:= ExtractFilePath(Filename);
          RearrangeFileHistory(Filename);
        end;
      end;
    end;
  finally
    FreeAndNil(SaveDialog);
  end;
end;

procedure TFJava.UMLSaveAs(UMLForm: TFUMLForm);
begin
  with SDSaveAs do begin
    Title:= _(LNGSaveAs);
    Filter:= 'UML (*.uml)|*.uml|' + _(LNGAll) + ' (*.*)|*.*';
    Filename:= UMLForm.GetSaveAsName;
    //if Filename = '' then exit; // not save on cancel
    FilterIndex:= 1;
    var Path:= ExtractFilePath(Filename);
    if Path <> ''
      then InitialDir:= Path
      else InitialDir:= FConfiguration.Sourcepath;
    if Execute then begin
      if ExtractFileExt(Filename) = '' then
        Filename:= Filename + '.uml';
      if assigned(getTDIWindowType(Filename, '%U%')) then
        InformationMsg(Format(_(LNGFileAlreadyOpen), [Filename]))
      else if not FileExists(Filename) or FileExists(Filename) and
         (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                       mtConfirmation, mbYesNoCancel,0) = mrYes)
      then begin
        FConfiguration.Sourcepath:= ExtractFilePath(Filename);
        FMessages.RenameInteractive(UMLForm.Pathname, Filename);
        UMLForm.Pathname:= Filename;
        UMLForm.Save(true);
        RenameTabAndWindow(UMLForm.Number, Filename);
        RearrangeFileHistory(Filename);
        UMLForm.Caption:= Filename;
      end;
    end;
  end;
end;

procedure TFJava.StructogramSaveAs(Structogram: TFStructogram);
begin
  with SDSaveAs do begin
    Title:= _(LNGSaveAs);
    Filter:= _(LNGStructogram) + ' (*.jsg)|*.jsg|' + _(LNGAll) + ' (*.*)|*.*';
    Filename:= Structogram.GetSaveAsName;
    FilterIndex:= 1;
    var Path:= ExtractFilePath(Filename);
    if Path <> ''
      then InitialDir:= Path
      else InitialDir:= FConfiguration.Sourcepath;
    if Execute then begin
      if ExtractFileExt(Filename) = '' then
        Filename:= Filename + '.jsg';
      if assigned(getTDIWindowType(Filename, '%S%')) then
        InformationMsg(Format(_(LNGFileAlreadyOpen), [Filename]))
      else if not FileExists(Filename) or FileExists(Filename) and
         (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                       mtConfirmation, mbYesNoCancel,0) = mrYes)
      then begin
        FConfiguration.Sourcepath:= ExtractFilePath(Filename);
        FMessages.RenameInteractive(Structogram.Pathname, Filename);
        Structogram.Pathname:= Filename;
        Structogram.Save(true);
        RenameTabAndWindow(Structogram.Number, Filename);
        RearrangeFileHistory(Filename);
        Structogram.Caption:= Filename;
      end;
    end;
  end;
end;

procedure TFJava.SequenceSaveAs(SequenceDiagram: TFSequenceForm);
begin
  with SDSaveAs do begin
    Title:= _(LNGSaveAs);
    Filter:= _(LNGSequenceDiagram) + ' (*.jsd)|*.jsd|' + _(LNGAll) + ' (*.*)|*.*';
    if SequenceDiagram.Pathname <> ''
      then Filename:= SequenceDiagram.Pathname
      else Filename:= SequenceDiagram.GetSaveAsName;
    FilterIndex:= 1;
    var Path:= ExtractFilePath(Filename);
    if Path <> ''
      then InitialDir:= Path
      else InitialDir:= FConfiguration.Sourcepath;
    if Execute then begin
      if ExtractFileExt(Filename) = '' then
        Filename:= Filename + '.jsd';
      if assigned(getTDIWindowType(Filename, '%Q%')) then
        InformationMsg(Format(_(LNGFileAlreadyOpen), [Filename]))
      else if not FileExists(Filename) or FileExists(Filename) and
         (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                       mtConfirmation, mbYesNoCancel,0) = mrYes)
      then begin
        FConfiguration.Sourcepath:= ExtractFilePath(Filename);
        FMessages.RenameInteractive(SequenceDiagram.Pathname, Filename);
        SequenceDiagram.Pathname:= Filename;
        SequenceDiagram.Save(true);
        RenameTabAndWindow(SequenceDiagram.Number, Filename);
        RearrangeFileHistory(Filename);
        SequenceDiagram.Caption:= Filename;
      end;
    end;
  end;
end;

procedure TFJava.MIExportClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if assigned(ActiveTDIChild) then
    ActiveTDIChild.DoExport;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIExitClick(Sender: TObject);
begin
  DefaultInstance.DebugLogToFile ('C:\temp\log.txt');
  Close;
end;

var Counter: integer = 0;

procedure TFJava.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  TThread.ForceQueue(nil, procedure
  begin
    Invalidate;
    for var i:= TDIFormsList.count - 1 downto 0 do
      TDIFormsList[i].DPIChanged;
    FMessages.DPIChanged;
    TJvModernTabBarPainter(myTabBar.Painter).Font.Size:= Font.Size;
    TJvModernTabBarPainter(myTabBar.Painter).SelectedFont.Size:= Font.Size;
    myTabBar.Invalidate;
  end);
end;

procedure TFJava.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  var i: integer;
  // var freq, startTime, endTime: Int64;
begin
  inc(Counter);
  DisableUpdateMenuItems;
  if (MyDebugger.Running or myJavaCommands.ProcessRunning) and (Counter < 2) then begin
    MIProgramResetClick(Self);
    CanClose:= false;
    CloseTimer.Enabled:= true;
    exit;
  end;

  DisableUpdateMenuItems;
  CanClose:= True;
  closing:= true;
  Screen.OnActiveFormChange:= nil;
  try
    for i:= TDIFormsList.count - 1 downto 0 do
      if TDIFormsList[i].FormTag = 4 then
        TDIFormsList[i].Close;  // exception
    if assigned(InteractiveUMLForm) then
      InteractiveUMLForm.Close;
  except
  end;

  Application.ProcessMessages;
  FConfiguration.SaveWindow;
  if Projectfilename <> '' then begin
    if myTabBar.Tabs.Count > 0 then
      DoSaveAsProject(Projectfilename);
    ProjectFilename:= '';
    FMessages.StatusMessage('');
    FConfiguration.RestoreConfigurationAfterProject;
  end;

  MISaveAllClick(Self);
  for i:= TDIFormsList.count - 1 downto 0 do
    TDIFormsList[i].Close;   // exception

  FMessages.SaveWindow;
  FWatches.SaveWindow;
  FFileStructure.SaveWindow;
  FObjectInspector.SaveWindow;
  SaveDocking;
  FObjectGenerator.SaveWindow;

  if Assigned(FClassInsert) then FClassInsert.SaveWindow;
  //if Assigned(mySearchOptions) then mySearchOptions.SaveWindow;
  if Assigned(FJunitTests) then FJunitTests.Close;

  //QueryPerformanceFrequency(freq);
  //QueryPerformanceCounter(startTime);
  if not MakeUpdate then
    FConfiguration.ModelToRegistry;   // needs about 1,5 seconds under windows 32
  //QueryPerformanceCounter(endTime);
  //ErrorMsg(IntToStr((endTime - startTime) * 1000 div freq) + 'ms');
end;

procedure TFJava.MIUndoClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.Undo
  else if Assigned(ActiveTDIChild) then
    ActiveTDIChild.Undo;
end;

procedure TFJava.MIRecognizeAssociationsClick(Sender: TObject);
begin
  if Assigned(ActiveTDIChild) and (ActiveTDIChild is TFUMLForm) then
    (ActiveTDIChild as TFUMLForm).TBRecognizeAssociationsClick(self);
end;

procedure TFJava.MIRedoClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.Redo
  else if Assigned(ActiveTDIChild) then
    ActiveTDIChild.Redo;
end;

procedure TFJava.MIPasteClick(Sender: TObject);
begin
  if FObjectInspector.Active then
    FObjectInspector.PasteFromClipboard
  else if GetActiveFormTag = 7 then
    FMessages.PasteFromClipboard
  else if Assigned(ActiveTDIChild) then
    ActiveTDIChild.PasteFromClipboard;
end;

procedure TFJava.MICopyNormalClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.CopyToClipboard
  else if Assigned(ActiveTDIChild) then
    ActiveTDIChild.CopyToClipboard;
end;

procedure TFJava.MICutClick(Sender: TObject);
begin
  if GetActiveFormTag = 7 then
    FMessages.CutToClipboard
  else if Assigned(ActiveTDIChild) then
    ActiveTDIChild.CutToClipboard;
end;

procedure TFJava.MICopyRTFClick(Sender: TObject);
begin
  EditorForm.ExportToClipboard(false, false);
end;

procedure TFJava.MICopyRtfNumberedClick(Sender: TObject);
begin
  EditorForm.ExportRTFNumbered;
end;

procedure TFJava.MICopyHTMLClick(Sender: TObject);
begin
  EditorForm.ExportToClipboard(true, false);
end;

procedure TFJava.MICopyHTMLAsTextClick(Sender: TObject);
begin
  EditorForm.ExportToClipboard(true, true);
end;

procedure TFJava.MICopyNumberedClick(Sender: TObject);
begin
  EditorForm.ExportWithNumbers;
end;

procedure TFJava.MIGitClick(Sender: TObject);
begin
  FGit.Execute(TSpTBXItem(Sender).Tag, GetActiveEditor);
end;

procedure TFJava.MIGotoLineClick(Sender: TObject);
begin
  with TFGotoLineDialog.Create(Self) do begin
    if (Showmodal = mrOK) and (Line > 0) and Assigned(EditorForm) then
      EditorForm.GotoLine(Line);
    Free;
  end;
end;

procedure TFJava.MIUnicodeClick(Sender: TObject);
begin
  with TFUnicodeDialog.Create(Self) do begin
    CBUnicode.Text:= '';
    if ShowModal = mrOK then
      EditorForm.PutText(UnicodeChar + '|');
    Free;
  end;
end;

procedure TFJava.MIUnindentClick(Sender: TObject);
begin
  EditorForm.Unindent;
end;

procedure TFJava.MIIndentClick(Sender: TObject);
begin
  EditorForm.Indent;
end;

procedure TFJava.MISystemOutPrintlnClick(Sender: TObject);
begin
  if GetActiveFormTag = 7
    then FMessages.SystemOutPrintln
    else EditorForm.SystemOutPrintln;
end;

procedure TFJava.MISearchClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) then
    ActiveTDIChild.Search;
end;

procedure TFJava.MISearchAgainClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) then
    ActiveTDIChild.SearchAgain;
end;

procedure TFJava.MIReplaceClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) then
    ActiveTDIChild.Replace;
end;

// Search and replace is needed for Notepad and TextDiff

procedure TFJava.ShowSearchReplaceDialog(Editor: TSynEditEx; AReplace: boolean);
begin
  if AReplace then
    with TFReplace.Create(Self) do begin
      ShowReplaceDialog(Editor);
      Free;
    end
  else
    with TFSearch.Create(Self) do begin
      ShowSearchDialog(Editor);
      Free;
    end;
end;

procedure TFJava.MISearchInFilesClick(Sender: TObject);
begin
  with TFGrepSearch.Create(FJava) do begin
    ShowSearchDialog(EditorForm);
    Free;
  end;
end;

procedure TFJava.DoSearchReplaceText(Editor: TSynEditEx; AReplace: boolean);
  var Options: TSynSearchOptions;
begin
  with MySearchOptions do
    if RegEx then
      try
        MyRegExSearch.DoRegSearchReplace(AReplace);
      except
        on e: exception do
          ErrorMsg(e.Message);
      end
    else begin
      if AReplace
        then Options:= [ssoPrompt, ssoReplace, ssoReplaceAll]
        else Options:= [];
      if CaseSensitive   then Include(Options, ssoMatchCase);
      if WholeWords      then Include(Options, ssoWholeWord);
      if not fFromCursor then Include(Options, ssoEntireScope);
      if SelectionOnly   then Include(Options, ssoSelectedOnly);
      if Backwards       then Include(Options, ssoBackwards);
      if Editor.SearchReplace(SearchText, ReplaceText, Options) = 0 then
        NotFound(Editor, ssoBackwards in Options);
    end;
end;

procedure TFJava.NotFound(Editor: TSynEdit; Backwards: boolean);  // Ex
begin
  if Backwards
    then Editor.BlockEnd  := Editor.BlockBegin
    else Editor.BlockBegin:= Editor.BlockEnd;
  Editor.CaretXY:= Editor.BlockBegin;
  InformationMsg(Format(_(LNGSearchTextNotFound), [MySearchOptions.SearchText]));
end;

procedure TFJava.MIMaximizedClick(Sender: TObject);
begin
  FConfiguration.WindowStateMaximized:= true;
  for var i:= 0 to myTabBar.Tabs.count - 1 do begin
    var aForm:= getTabForm(i);
    if assigned(aForm) then begin
      aForm.Align:= alClient;
      aForm.BorderStyle:= bsNone;
      aForm.BorderIcons:= [];
      aForm.WindowState:= wsMaximized;
    end;
  end;
end;

procedure TFJava.MICascadeClick(Sender: TObject);
  var i, dist, border, titlebar, aWidth, aHeight: integer; aForm: TFForm;
begin
  FConfiguration.WindowStateMaximized:= false;
  dist:= 0;
  titlebar:= 0;
  aWidth:= MainPanel.width * 3 div 4;
  aHeight:= MainPanel.Height * 2 div 3;
  for i:= 0 to myTabBar.Tabs.Count - 1 do begin
    aForm:= getTabForm(i);
    if assigned(aForm) then begin
      aForm.Align:= alNone;
      aForm.WindowState:= wsNormal;
      aForm.BorderStyle:= bsSizeable;
      aForm.BorderIcons:= [biSystemMenu, biMaximize];
    end;
    if titlebar = 0 then begin
      border:= max((aForm.Width - aForm.ClientWidth) div 2, 8);
      titlebar:= max(aForm.Height - aForm.ClientHeight - border, 30);
    end;
    aForm.setBounds(dist, dist, aWidth, aHeight);
    dist:= dist + titlebar;
  end;
end;

procedure TFJava.Tile(horizontal: boolean);
  var i, count, nr, aWidth, aHeight, PartX, PartY: integer;
      rectangles: array of TRect; aForm: TFForm;
begin
  FConfiguration.WindowStateMaximized:= false;
  count:= 0;
  if myTabBar.Tabs.Count > 0 then begin
    for i:= 0 to myTabBar.Tabs.Count - 1 do begin
      aForm:= getTabForm(i);
      if assigned(aForm) then begin
        aForm.Align:= alNone;
        aForm.WindowState:= wsNormal;
        aForm.BorderStyle:= bsSizeable;
        aForm.BorderIcons:= [biSystemMenu, biMaximize];
        inc(count);
      end;
    end;

    aWidth:= MainPanel.Width;
    aHeight:= MainPanel.Height;
    if count > 9 then count:= 9;
    setLength(Rectangles, count);
    case count of
      1: Rectangles[0]:= Rect(0, 0, aWidth, aHeight);
      2: if horizontal then begin
           Rectangles[0]:= Rect(0, 0, aWidth, aHeight div 2);
           Rectangles[1]:= Rect(0, aHeight div 2, aWidth, aHeight);
         end else begin
           Rectangles[0]:= Rect(0, 0, aWidth div 2, aHeight);
           Rectangles[1]:= Rect(aWidth div 2, 0, aWidth, aHeight);
         end;
      3: if horizontal then begin
           PartY:= aHeight div 3;
           Rectangles[0]:= Rect(0, 0, aWidth, PartY);
           Rectangles[1]:= Rect(0, PartY, aWidth, 2*PartY );
           Rectangles[2]:= Rect(0, 2*PartY, aWidth, aHeight);
         end else begin
           PartX:= aWidth div 3;
           Rectangles[0]:= Rect(0, 0, PartX, aHeight);
           Rectangles[1]:= Rect(PartX, 0, 2*PartX, aHeight);
           Rectangles[2]:= Rect(2*PartX, 0, aWidth, aHeight);
         end;
      4: begin
           PartX:= aWidth div 2;
           PartY:= aHeight div 2;
           Rectangles[0]:= Rect(0, 0, PartX, PartY);
           Rectangles[1]:= Rect(PartX, 0, aWidth, PartY);
           Rectangles[2]:= Rect(0, PartY, PartX, aHeight);
           Rectangles[3]:= Rect(PartX, PartY, aWidth, aHeight);
         end;
      5: begin
           PartX:= aWidth div 2;
           PartY:= aHeight div 2;
           Rectangles[0]:= Rect(0, 0, PartX, PartY);
           Rectangles[1]:= Rect(0, PartY, PartX, aHeight);
           PartY:= aHeight div 3;
           Rectangles[2]:= Rect(PartX, 0, aWidth, PartY);
           Rectangles[3]:= Rect(PartX, PartY, aWidth, 2*PartY);
           Rectangles[4]:= Rect(PartX, 2*PartY, aWidth, aHeight);
         end;
      6: begin
           PartX:= aWidth div 2;
           PartY:= aHeight div 3;
           Rectangles[0]:= Rect(0, 0, PartX, PartY);
           Rectangles[1]:= Rect(0, PartY, PartX, 2*PartY);
           Rectangles[2]:= Rect(0, 2*PartY, PartX, aHeight);

           Rectangles[3]:= Rect(PartX, 0, aWidth, PartY);
           Rectangles[4]:= Rect(PartX, PartY, aWidth, 2*PartY);
           Rectangles[5]:= Rect(PartX, 2*PartY, aWidth, aHeight);
         end;
      7: begin
           PartX:= aWidth div 3;
           PartY:= aHeight div 2;
           Rectangles[0]:= Rect(0, 0, PartX, PartY);
           Rectangles[1]:= Rect(0, PartY, PartX, aHeight);
           Rectangles[2]:= Rect(PartX, 0, 2*PartX, PartY);
           Rectangles[3]:= Rect(PartX, PartY, 2*PartX, aHeight);
           PartY:= aHeight div 3;
           Rectangles[4]:= Rect(2*PartX, 0, aWidth, PartY);
           Rectangles[5]:= Rect(2*PartX, PartY, aWidth, 2*PartY);
           Rectangles[6]:= Rect(2*PartX, 2*PartY, aWidth, aHeight);
         end;
      8: begin
           PartX:= aWidth div 4;
           PartY:= aHeight div 2;
           Rectangles[0]:= Rect(0, 0, PartX, PartY);
           Rectangles[1]:= Rect(PartX, 0, 2*PartX, PartY);
           Rectangles[2]:= Rect(2*PartX, 0, 3*PartX, PartY);
           Rectangles[3]:= Rect(3*PartX, 0, aWidth, PartY);

           Rectangles[4]:= Rect(0, PartY, PartX, aHeight);
           Rectangles[5]:= Rect(PartX, PartY, 2*PartX, aHeight);
           Rectangles[6]:= Rect(2*PartX, PartY, 3*PartX, aHeight);
           Rectangles[7]:= Rect(3*PartX, PartY, aWidth, aHeight);
         end;
      9: begin
           PartX:= aWidth div 3;
           PartY:= aHeight div 3;
           Rectangles[0]:= Rect(0, 0, PartX, PartY);
           Rectangles[1]:= Rect(0, PartY, PartX, 2*PartY);
           Rectangles[2]:= Rect(0, 2*PartY, PartX, aHeight);
           Rectangles[3]:= Rect(PartX, 0, 2*PartX, PartY);
           Rectangles[4]:= Rect(PartX, PartY, 2*PartX, 2*PartY);
           Rectangles[5]:= Rect(PartX, 2*PartY, 2*PartX, aHeight);
           Rectangles[6]:= Rect(2*PartX, 0, aWidth, PartY);
           Rectangles[7]:= Rect(2*PartX, PartY, aWidth, 2*PartY);
           Rectangles[8]:= Rect(2*PartX, 2*PartY, aWidth, aHeight);
        end;
      end;
    end;
    Nr:= -1;
    for i:= 0 to Count - 1 do begin
      aForm:= getTabForm(i);
      if assigned(aForm) then begin
        inc(Nr);
        aForm.setBounds(Rectangles[Nr].left, Rectangles[Nr].Top, Rectangles[Nr].width, Rectangles[Nr].Height);
        if Nr = count then break;
      end;
    end;
end;

procedure TFJava.MITileVerticalClick(Sender: TObject);
begin
  Tile(false);
end;

procedure TFJava.MITileHorizontalClick(Sender: TObject);
begin
  Tile(true);
end;

procedure TFJava.MITestCreateSequencediagramClick(Sender: TObject);
  var SequenceForm: TFSequenceForm; EditForm: TFEditForm; Filename: string;
begin
  DisableUpdateMenuItems;
  EditForm:= getActiveEditor;
  if assigned(EditForm) then begin
    Filename:= ChangeFileExt(EditForm.Pathname, '.jsd');
    SequenceForm:= getTDIWindowType(Filename, '%Q%') AS TFSequenceForm;
    if assigned(SequenceForm) then
      MyDebugger.SequenceForm:= SequenceForm
    else if not FileExists(Filename) or FileExists(Filename) and
       (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                   mtConfirmation, mbYesNoCancel,0) = mrYes)
    then begin
      SequenceForm:= TFSequenceForm(FormFactory(fkSequence));
      SequenceForm.New(Filename);
      MyDebugger.SequenceForm:= SequenceForm;
      SequenceForm.OnCloseNotify:= MyDebugger.CloseNotify;
      EditForm.OpenWindow(Self);
      EditForm.Enter(Self);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIUMLCreateSequencediagramClick(Sender: TObject);
  var Sequenceform: TFSequenceForm; UMLForm: TFUMLForm; Filename: string;
    aRtfdDiagram: TRtfdDiagram;
begin
  DisableUpdateMenuItems;
  UMLForm:= getActiveUML;
  if assigned(UMLForm) then begin
    Filename:= ChangeFileExt(UMLForm.Pathname, '.jsd');
    Sequenceform:= getTDIWindowType(Filename, '%Q%') AS TFSequenceForm;
    if (Sequenceform = nil) and (not FileExists(Filename) or FileExists(Filename) and
       (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                   mtConfirmation, mbYesNoCancel,0) = mrYes))
    then begin
      Sequenceform:= TFSequenceForm(FormFactory(fkSequence));
      Sequenceform.New(Filename);
      UMLForm.OpenWindow(Self);
      UMLForm.Enter(Self);
    end;
    Sequenceform.AddLifeLine('Actor');
    aRtfdDiagram:= (UMLForm.MainModul.Diagram as TRtfdDiagram);
    aRtfdDiagram.SequenceForm:= Sequenceform;
    Sequenceform.OnCloseNotify:= aRtfdDiagram.CloseNotify;
  end;
  EnableUpdateMenuItems;
end;

// receives synchronously sent messages from je2 java
procedure TFJava.WMCOPYDATA(var msg: TWMCopyData);
  var s: string;
begin
  s:= PChar(msg.CopyDataStruct.lpData);
  s:= copy(s, 1, msg.CopyDataStruct.cbData div 2);
  getComJava.AnswerFromJe2Java:= s;
end;

function TFJava.hasEditforms: boolean;
begin
  Result:= false;
  for var i:= 0 to TDIEditFormCount - 1 do
    if TDIEditFormGet(i).FormTag in [1..3, 11, 14] then
      exit(true);
end;

function TFJava.hasJavaFiles: boolean;
begin
  Result:= false;
  for var i:= 0 to TDIEditFormCount - 1 do
    if TDIEditFormGet(i).isJava then
      exit(true);
end;

function TFJava.hasPascalFiles: boolean;
begin
  Result:= false;
  for var i:= 0 to TDIEditFormCount - 1 do
    if TDIEditFormGet(i).isPascal then
      exit(true);
end;

procedure TFJava.ImperativeUpdateMenuItems;
begin
  var n:= UpdateMenuItemsCount;
  UpdateMenuItemsCount:= 0;
  UpdateMenuItems(Self);
  UpdateMenuItemsCount:= n;
end;

procedure TFJava.UpdateMenuItems(Sender: TObject);
  var EditorIsJava, IsEditorForm, IsBrowserForm, IsUMLForm, IsGUIForm,
      IsTextDiff, IsStructogram, IsMessagesAndCompilable, IsMessagesAndRunnable,
      AndroidMode, NoProcessRunning: Boolean;
      UMLForm: TFUMLForm;
      FormTag, visibleTDIs: integer;

  function isApplet: boolean;
  begin
    Result:= (EditorIsJava or IsGUIForm and assigned(EditorForm)) and EditorForm.IsApplet;
  end;

  function hasSomethingToCompile: boolean;
    var Form: TFForm; 
  begin
    Result:= false;
    Form:= getActiveForm;
    if assigned(Form) then begin
      case Form.FormTag of
        1: if (Form as TFEditForm).isJava then begin
             EditorForm:= TFEditForm(Form);
             Result:= true;
           end;
        2: Result:= true;
      end;
    end;
  end;

  function canRun: boolean;
    var EditForm: TFEditForm; UmlForm: TFUMLForm;
  begin
    {not: and EditorForm.hasMainInModel, due to possible errors in sourcefile}
    if not NoProcessRunning then
      exit(false);
    Result:= true;
    if myDebugger.Running then
      exit(myDebugger.ReadyForInput);
    if FConfiguration.AndroidMode then exit;
    EditForm:= getActiveEditor;
    if assigned(EditForm) and EditForm.isHTML or hasJavaFiles then
      exit;
    UmlForm:= getActiveUML;
    if assigned(UmlForm) and UmlForm.hasClassWithMain then exit;
    if (FConfiguration.JavaStartClass <> '') or IsMessagesAndRunnable then exit;
    Result:= false;
  end;

  function canCompile: boolean;
    var Form: TFForm;
  begin
    if AndroidMode or not FConfiguration.JavaCompilerOK or not NoProcessRunning then
      exit(false);
    Form:= getActiveUML;
    if assigned(Form) then exit(true);
    Form:= GetActiveForm;
    if assigned(Form) and (Form.Formtag in [1..3]) or myDebugger.Running or hasJavaFiles or IsMessagesAndCompilable then
      exit(true);
    Result:= false;
  end;

begin
  if UpdateMenuItemsCount > 0 then exit;
  IsEditorForm:= false;
  IsBrowserForm:= false;
  IsUMLForm:= false;
  IsGUIForm:= false;
  IsTextDiff:= false;
  IsStructogram:= false;
  IsMessagesAndCompilable:= false;
  IsMessagesAndRunnable:= false;
  EditorIsJava:= false;
  EditorForm:= nil;
  UMLForm:= nil;
  if assigned(myJavaCommands)
    then NoProcessRunning:= not myJavaCommands.ProcessRunning
    else NoProcessRunning:= true;
  ActiveTDIChild:= getActiveForm;
  AndroidMode:= FConfiguration.AndroidMode;

  FormTag:= GetActiveFormTag;
  case FormTag of
    1: begin
         EditorForm:= getActiveEditor;
         if assigned(EditorForm) and assigned(EditorForm.Editor) then begin
           IsEditorForm:= true;
           EditorIsJava:= EditorForm.IsJava;
           if assigned(EditorForm.TBBreakpoint) then // nil during closing
             if EditorIsJava then begin
               EditorForm.TBBreakpoint.Visible:= true;
               EditorForm.TBBreakpointsClear.Visible:= true;
               end
             else begin
               EditorForm.TBBreakpoint.Visible:= false;
               EditorForm.TBBreakpointsClear.Visible:= false;
             end
           else
             exit;
         end;
       end;
    2: begin
         IsUMLForm:= true;
         UMLForm:= TFUMLForm(ActiveTDIChild);
       end;
    3: begin
         IsGUIForm:= true;
         EditorForm:= TFEditForm((ActiveTDIChild as TFGUIForm).Partner);
       end;
    4: IsTextDiff:= true;
    5: IsBrowserForm:= true;
    6: ; // Explorer
    7: begin // Messages
         IsMessagesAndCompilable:= hasSomethingToCompile;
         IsMessagesAndRunnable:= assigned(EditorForm) and EditorForm.isJava;
       end;
    8: begin // ObjectInspector
         FObjectInspector.UpdateState;
         if assigned(FGUIDesigner.ELDesigner.DesignControl) then begin
           isGuiForm:= true;
           EditorForm:= TFEditForm(TFForm(FGUIDesigner.ELDesigner.DesignControl).Partner);
         end;
       end;
    9: ; // ObjectGenerator
   10: ; // Hint
   11: IsStructogram:= true;
   12: ; // Tooltip
   13: begin // TSynBaseCompletionProposalFormEx
         EditorForm:= myCodeCompletion.EditForm;
         if assigned(EditorForm) and assigned(EditorForm.Editor) then
           IsEditorForm:= true;
       end;
   14: ; // SequenceForm
  end;
  if assigned(EditorForm)
    then EditorIsJava:= EditorForm.IsJava
    else EditorIsJava:= false;

  visibleTDIs:= myTabBar.Tabs.Count;
  if (FormTag in [0, 9, 10, 12]) or (visibleTDIs = 0) then begin
    SetEnabledMI(MIRedo, false);
    SetEnabledMI(MIUndo, false);
    SetEnabledMI(MICut, false);
    SetEnabledMI(MICopy, false);
    SetEnabledMI(MIPaste, false);
    SetEnabledTB(TBRedo, false);
    SetEnabledTB(TBUndo, false);
  end;

  // File-Menu
  //SetEnabledMI(MISave, (FormTag in [1..4, 10..14]) or ActiveTDIChild.Modified);
  //SetEnabledMI(MISaveAs, (FormTag in [1..3, 10..14]) or ActiveTDIChild.Modified);

  //SetEnabledMI(MISaveAll, hasSomethingToSave);
  //SetEnabledMI(MISaveAllIn, MISaveAll.enabled);
  SetEnabledMI(MISaveAsProject, visibleTDIs > 0);
  SetEnabledMI(MICloseProject, ProjectFilename <> '');
  SetEnabledMI(MIClose, visibleTDIs > 0);
  SetEnabledMI(MICloseAllFiles, (visibleTDIs > 0) and (Projectfilename = ''));
  SetEnabledMI(MIExport,isEditorForm and assigned(EditorForm.Editor.Highlighter) or IsStructogram);
  SetEnabledMI(MIPrint, visibleTDIs > 0);
  SetEnabledMI(MIPrintAll, visibleTDIs > 0);
  // Edit menu
  SetEnabledMI(MISearch, IsEditorForm or IsBrowserForm or IsTextDiff);
  SetEnabledMI(MISearchAgain, IsEditorForm or IsTextDiff);
  SetEnabledMI(MIReplace, IsEditorForm or IsTextDiff);
  SetEnabledMI(MIUnindent, IsEditorForm);
  SetEnabledMI(MIIndent, IsEditorForm);
  SetEnabledMI(MIStructuredIndent, isEditorForm);
  SetEnabledMI(MICommentOnOff, IsEditorForm);
  SetEnabledMI(MIGotoLine, IsEditorForm or IsTextDiff);
  SetEnabledMI(MISystemOutPrintln, IsEditorForm);
  SetEnabledMI(MIUnicode, IsEditorForm and (EditorForm.Encoding <> 'ANSI'));
  if MIUnicode.Visible <> MIUnicode.Enabled then MIUnicode.Visible:= MIUnicode.Enabled;

  // Start menu
  // in debug-mode TBCompileJava has abort-function
  SetEnabledMI(MICompile, canCompile);
  SetEnabledTB(TBCompileJava, MICompile.Enabled);
  SetEnabledTB(TBCompileAll, MICompile.Enabled);
  SetEnabledMI(MICompileAll, MICompile.Enabled);
  SetEnabledMI(MIRun, canRun);
  if myDebugger.Running and not myDebugger.ReadyForInput then
    SetEnabledMI(MIRun, false);

  SetEnabledMI(MIJavaReset, IsEditorForm and EditorForm.isJava and NoProcessRunning);
  //SetEnabledTB(TBSaveAll, MISaveAll.Enabled);

  SetEnabledTB(TBRun, MIRun.Enabled or MIProgramReset.Enabled);
  SetEnabledMI(MIHTMLforApplet, isApplet and NoProcessrunning);
  SetEnabledMI(MIHTMLforJavaPlugIn, MIHTMLforApplet.Enabled);
  SetEnabledMI(MIAppletviewer, FConfiguration.JavaAppletViewerOK and NoProcessRunning
               and (isApplet or isEditorForm and EditorForm.IsHTMLApplet));
  if MIRun.Enabled then
    if MIAppletViewer.Enabled
      then MIRun.Caption:= _('Start Applet')
    else if AndroidMode
      then MIRun.Caption:= _(LNGTransferToAndroid)
      else MIRun.Caption:= _(LNGRunApplication)
  else
    MIRun.Caption:= _('Run');
  SetEnabledMI(MIDebugger, MICompile.Enabled and FConfiguration.JavaDebuggerOK and
                          not myDebugger.Running and not MIProgramReset.Enabled and
                          not AndroidMode and NoProcessRunning);
  SetEnabledMI(MIDissasembler, NoProcessRunning);
  SetEnabledMI(MIJavaDoc, FConfiguration.JavaDocOK and EditorIsJava
               and not myDebugger.Running and NoProcessRunning);
  SetEnabledMI(MIJar, FConfiguration.JavaJarOk and NoProcessRunning);
  SetEnabledMI(MIJarCreate, MICompile.Enabled);
  SetEnabledMI(MIJarPack, Assigned(ActiveTDIChild));

  // Test menu
  SetEnabledMI(MIStep, (EditorIsJava or myDebugger.ReadyForInput) and not AndroidMode and NoProcessRunning);
  SetEnabledMI(MINext, MIStep.Enabled);
  SetEnabledMI(MIStepUp, MIStep.Enabled);
  SetEnabledMI(MIRunToCursor, EditorIsJava and MIStep.Enabled);
  SetEnabledMI(MIShowExecutionPoint, myDebugger.Running);
  SetEnabledMI(MIBreakpoint, EditorIsJava);
  SetEnabledMI(MIBreakpointsClear, hasJavaFiles);
  SetEnabledMI(MIExpression, myDebugger.Running {and myDebugger.ReadyForInput});
  SetEnabledMI(MITestCreateSequencediagram, EditorIsJava and NoProcessRunning); // getEditFormWithMain <> nil { EditorIsJava});
  SetEnabledTB(TBStep, MIStep.Enabled);
  SetEnabledTB(TBNext, MINext.Enabled);

  // UML menu
  SetEnabledMI(MINewUML, NoProcessRunning);
  SetEnabledMI(MINewClass, NoProcessRunning);
  SetEnabledMI(MIClassEditor, (EditorIsJava or IsUMLForm and UMLForm.hasEditableClass) and NoProcessRunning);
  SetEnabledMI(MINewComment, isUMLForm);
  SetEnabledMI(MINewLayout, IsUMLForm);
  SetEnabledMI(MIRefresh, IsUMLForm);
  SetEnabledMI(MIDiagramFromOpenFiles, hasJavaFiles or hasPascalFiles);
  SetEnabledMI(MISaveAsPicture, isUMLForm);
  SetEnabledTB(TBDiagramFromOpenFiles, MIDiagramFromOpenFiles.Enabled);
  SetEnabledMI(MIUMLCreateSequencediagram, isUMLForm and NoProcessRunning);

  // Tools menu
  MICheckStyle.Visible:= FConfiguration.CheckstyleOK;
  MIJalopy.Visible:= FConfiguration.JalopyOK;
  MISubVersion.Visible:= FConfiguration.SubversionOK;
  MIGit.Visible:= FConfiguration.GitOK;
  MIJUnit.Visible:= FConfiguration.JUnitOK;
  SetEnabledMI(MICheckstyle, MICompile.Enabled and FConfiguration.CheckstyleOK and NoProcessRunning);
  SetEnabledMI(MIJalopy, MICompile.Enabled and FConfiguration.JalopyOK and NoProcessRunning);
  SetEnabledMI(MISubversion, IsEditorForm and NoProcessRunning);
  SetEnabledMI(MIGit, NoProcessRunning);
  SetEnabledMI(MIJunit, NoProcessRunning);
  SetEnabledMI(MIJUnitCreateTestclass, MIJUnit.Visible and EditorIsJava and not EditorForm.isJUnitTestClass);
  SetEnabledMI(MIJUnitRunAllTests, MIJUnit.Visible and EditorIsJava and EditorForm.isJUnitTestClass);
  SetEnabledMI(MIConfigureTools, NoProcessRunning);

  // Window menu
  SetEnabledMI(MIMsDos, not FConfiguration.DOSWindowLocked);
  SetEnabledMI(MIWebsite, not FConfiguration.BlockedInternet);
  SetEnabledMI(MIUpdate, not FConfiguration.BlockedInternet and NoProcessRunning);
  SetEnabledMI(MIMaximized, visibleTDIs > 0);
  SetEnabledMI(MICascade, visibleTDIs > 0);
  SetEnabledMI(MITileVertical, visibleTDIs > 0);
  SetEnabledMI(MITileHorizontal, visibleTDIs > 0);
  SetEnabledMI(MIFont, FormTag in [1..4, 6..7, 11, 14, 16]);
  SetEnabledTB(TBSave, MISave.Enabled);

  // Help menu
  MIJunitManual.Visible:= FConfiguration.JUnitOK;
  MIDemos.Visible:= (FConfiguration.JavaDemos <> '');
end;

function TFJava.PreparePrint(Sender: TObject): boolean;
begin
  Result:= false;
  if hasDefaultPrinter then begin
    Result:= true;
    if PrintDialog = nil then begin
      PrintDialog:= TPrintDialog.Create(Self);
      try
        with PrintDialog do begin
          Copies:= 1;
          FromPage:= 1;
          MinPage:= 1;
          Options:= [poPrintToFile, poPageNums, poSelection, poWarning, poHelp];
        end
      except
      end;
    end;
  end else
    ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFJava.MIPrintClick(Sender: TObject);
begin
  if PreparePrint(Sender) and assigned(ActiveTDIChild) then
    ActiveTDIChild.Print;
end;

procedure TFJava.MIPrintAllClick(Sender: TObject);
begin
  if PreparePrint(Sender) then begin
    FixWindows(false);
    for var i:= 0 to Length(FixedWindows) - 1 do begin
      Application.ProcessMessages;
      if FixedWindows[i].FormTag = 1
        then (FixedWindows[i] as TFEditForm).PrintAll(true)
        else FixedWindows[i].Print
    end;
  end;
end;

procedure TFJava.MIPrintSetupClick(Sender: TObject);
begin
  if HasDefaultPrinter then begin
    if PrinterSetup = nil then
      PrinterSetup:= TPrinterSetupDialog.Create(Self);
    PrinterSetup.Execute;
    FConfiguration.WriteStringU('Printer', 'Printer', Printer.Printers[Printer.PrinterIndex]);
  end else
    ErrorMsg(_(LNGNoDefaultPrinter));
end;

procedure TFJava.MIFileStructureClick(Sender: TObject);
begin
  FFileStructure.ChangeHideShow;
  if FFileStructure.Visible then
    UpdateLayoutRightDockPanel(true);
end;

procedure TFJava.MIFontClick(Sender: TObject);
  var i, FormTag: Integer; aFont: TFont;
begin
  FormTag:= GetActiveFormTag;
  case FormTag of
     3,
     5: exit;
     7: aFont:= FMessages.Font;
     8: aFont:= FObjectInspector.Font;
     9: aFont:= FObjectGenerator.Font;
    10: aFont:= FScpHint.Font;
    12: exit;
    16: aFont:= FFileStructure.Font;
    else
      if ActiveTDIChild = nil
        then exit
        else aFont:= ActiveTDIChild.GetFont;
  end;
  FDFont.Font.Assign(aFont);

  if FormTag in [1, 4]
    then FDFont.Options:= [fdFixedPitchOnly]
    else FDFont.Options:= [];

  if FDFont.Execute then begin
    aFont.assign(FDFont.Font);
    if FormTag in [1, 2, 4, 6, 11, 14] then
      for i:= 0 to TDIFormsList.Count - 1 do
        if TDIFormsList[i].FormTag = FormTag then
          TDIFormsList[i].SetFont(aFont);
    case FormTag of
       7: FMessages.SetFont(aFont);
       8: FObjectInspector.SetFont(aFont);
       9: FObjectGenerator.SetFont(aFont);
      10: FScpHint.SetFont(aFont);
      16: FFileStructure.SetFont(aFont);
    end;
  end;
end;

function TFJava.WindowOpened(const Pathname: string; var aForm: TFForm): boolean;
begin
  for var i:= 0 to TDIFormsList.Count - 1 do begin
    aForm:= TDIFormsList[i];
    if CompareText(aForm.Pathname, Pathname) = 0 then
      exit(true);
  end;
  aForm:= nil;
  Result:= false;
end;

function TFJava.getTDIWindowType(const Pathname, Typ: string): TFForm;
begin
  Result:= nil;
  for var i:= 0 to TDIFormsList.Count - 1 do begin
    var aForm:= TDIFormsList[i];  // AccessViolation removed
    if (aForm.GetFormType = Typ) and (CompareText(aForm.Pathname, Pathname) = 0)
    then begin
      Result:= aForm;
      break;
    end;
  end;
end;

function TFJava.getGuiForm(Pathname: string): TFGUIForm;
begin
  Result:= nil;
  //Pathname:= ChangeFileExt(Pathname, '.java');
  for var i:= 0 to TDIEditFormCount - 1 do begin
    var aForm:= TDIEditFormGet(i).Partner;
    if assigned(aForm) and (CompareText(aForm.Pathname, Pathname) = 0) then begin
      Result:= TFGuiForm(aForm);
      break;
    end;
  end;
end;

function TFJava.getTDIWindow(const Pathname: string): TFForm;
begin
  Result:= nil;
  for var i:= 0 to TDIFormsList.Count - 1 do begin
    var aForm:= TDIFormsList[i];  // AccessViolation
    if CompareText(aForm.Pathname, Pathname) = 0 then begin
      Result:= aForm;
      break;
    end;
  end;
end;

function TFJava.getTDIWindow(Number: integer): TFForm;
begin
  for var i:= 0 to TDIFormsList.Count - 1 do begin
    Result:= TDIFormsList[i];
    if Result.Number = Number then
      exit;
  end;
  Result:= nil;
end;

function TFJava.getTDIType(const Typ: string): TFForm;
begin
  for var i:= 0 to TDIFormsList.Count - 1 do begin
    Result:= TDIFormsList[i];
    if Result.GetFormType = Typ then
      exit;
  end;
  Result:= nil;
end;

procedure TFJava.SwitchToWindow(const path: string);
begin
  for var i:= 0 to TDIFormsList.Count - 1 do begin
    var aForm:= TDIFormsList[i];
    if CompareText(aForm.Pathname, path) = 0 then begin
      aForm.OpenWindow(Self);
      break;
    end;
  end;
end;

procedure TFJava.SwitchToWindow(Nr: integer);
begin
  for var i:= 0 to TDIFormsList.Count - 1 do begin
    var aForm:= TDIFormsList[i];
    if aForm.Number = Nr then begin
      aForm.OpenWindow(Self);
      break;
    end;
  end;
end;

procedure TFJava.SwitchToWindow(Form: TFForm);
begin
  if assigned(Form) then
    Form.OpenWindow(Self);
end;

procedure TFJava.SwitchWindowWithSearch(const Pathname: string);
begin
  SwitchToWindow(Pathname);
  if (EditorForm = nil) or (CompareText(EditorForm.Pathname, Pathname) <> 0) then
    if Open(Pathname) then
      RearrangeFileHistory(Pathname);
end;

function TFJava.GetPathnameForClass(const classname: string): string;
begin
  Result:= '';
  for var i:= 0 to TDIEditFormCount - 1 do begin
    var aForm:= TDIEditFormGet(i);
    aForm.ParseSourcecode(false);
    if aForm.SourceContainsClass(classname) then begin
      Result:= aForm.Pathname;
      break;
    end;
  end;
end;

procedure TFJava.ChangeWindowWithPositioning(const Pathname: string; XPos, YPos: Integer; error: boolean);
begin
  SwitchWindowWithSearch(Pathname);
  if assigned(EditorForm) then begin
    if EditorForm.CanFocus then begin
      EditorForm.SetFocus;
      FocusControl(EditorForm.Editor);
    end;
    with EditorForm.Editor do begin
      CaretX:= XPos;
      CaretY:= YPos;
      EnsureCursorPosVisible;
      Gutter.ShowLineNumbers:= true;
    end;
    if error then
      EditorForm.SetErrorMark(YPos, XPos, 'ERROR');
  end;
end;

function TFJava.OpenEditForm(const Pathname: string; hidden: boolean): TFEditForm;
  var Checkage: boolean; s: string; aActive: TFForm;
begin
  aActive:= ActiveTDIChild;
  CheckAge:= EditorAgeTimer.Enabled;
  try
    if CheckAge then EditorAgeTimer.Enabled:= false;
    Result:= nil;
    try
      if FileExists(Pathname) then begin
        Result:= getTDIWindowType(Pathname, '%E%') as TFEditForm;
        if assigned(Result) then begin
          if not hidden then Result.Show;
        end else begin
          LockFormUpdate(Self);  // displaces CompletionProposalForm
          Result:= TFEditForm(FormFactory(fkEditor));
          Result.Open(Pathname, '');
          if hidden then begin
            Result.Hide;
            if aActive.FormTag = 1 then
              scpJava.Form.CurrentEditor:= (aActive as TFEditForm).Editor;
          end;
          UnlockFormUpdate(Self);
        end;
      end;
      if not hidden then begin
        s:= ChangeFileExt(Pathname, '.jfm');
        if FileExists(s) then
          Open(s);
      end;
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    if CheckAge then EditorAgeTimer.Enabled:= true;
  end;
end;

function TFJava.Open(const Filename: string): Boolean;
  var s, Ext: string; aForm: TFForm;
begin
  Result:= False;
  if FileExists(Filename) then begin
    Ext:= Uppercase(ExtractFileExt(Filename));
    aForm:= getTDIWindow(Filename);
    if assigned(aForm) then
      aForm.OpenWindow(Self)
    else if (Ext = '.UML') then begin
      OpenUMLWindow(Filename, '');
      Result:= true;
    end else if (Ext = '.JEP') then begin
      Result:= False;
      MICloseAllFilesClick(self);
      ProjectFilename:= Filename;
      FMessages.StatusMessage(ProjectFilename);
      TThread.ForceQueue(nil, procedure
        begin
          OpenProject(ProjectFilename);
          UpdateMenuItems(Self);
        end);
    end else if (Ext = '.JFM') then begin
      s:= ChangeFileExt(Filename, '.java');
      if FileExists(s) then begin
        if not WindowOpened(s, aForm) then begin
          Open(s); // opens Filename too
          aForm:= getTDIWindow(Filename);
        end else
          aForm:= FGUIDesigner.Open(Filename, s);
        Result:= (aForm <> nil);
      end else
        ErrorMsg(Format(_(LNGAssociatedJavaFileNotFound), [s]))
    end else if (Ext = '.JAR') then
      JarShowUnpackOpen(3, Filename)
    else if (Ext = '.CLASS') then
      DoOpenInUMLWindow(Filename)
    else if ((Ext = '.HTM') or (Ext = '.HTML')) and
           assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 5)
      then (ActiveTDIChild as TFBrowser).WebBrowser.Navigate(Filename)
    else if (Ext = '.JSG') or (Ext = '.NSD') then
       Result:= OpenStructogram(Filename, '')
    else if (Ext = '.JSD') then
       Result:= OpenSequenceDiagram(Filename, '')
    else begin {if ((Ext = '.JAVA') or (Ext = '.INI') or (Ext = '.TXT') or (Ext = '.~AVA') or (Ext = '.BAT')) then }
      EditorForm:= OpenEditForm(Filename, false);
      Result:= Assigned(EditorForm);
    end;
  end;
end;

procedure TFJava.OpenFileWithState(const s: string);
  var state, Typ, Address: string;
      p: integer;
begin
  p:= Pos('%X%', s) + Pos('%B%', s) + Pos('%E%', s) + Pos('%U%', s) + Pos('%G%', s) + Pos('%T%', s) + Pos('%S%', s) + Pos('%Q%', s);
  if p = 0 then exit;
  state:= copy(s, 1, p-1);
  Typ    := copy(s, p, 3);
  Address:= FConfiguration.AddPortableDrive(copy(s, p+3, length(s)));
  if (Typ = '%X%') and SysUtils.DirectoryExists(Address) then
    NewExplorer(Address, state)
  else if Typ = '%B%' then
    NewBrowser(Address, state)
  else if FileExists(Address) then
    if Typ = '%E%' then
      NewEditor(Address, state)
    else if Typ = '%U%' then
      NewUMLWindow(Address, state)
    else if Typ = '%G%' then
      NewGUIForm(Address, state)
    else if Typ = '%T%' then
      OpenProject(Address)
    else if Typ = '%S%' then
      OpenStructogram(Address, state)
    else if Typ = '%Q%' then
      OpenSequenceDiagram(Address, state)
end;

procedure TFJava.CloseFile(const Filename: string);
  var aForm: TFForm;
begin
  if WindowOpened(Filename, aForm) then begin
    aForm.OpenWindow(Self);
    if assigned(ActiveTDIChild) then
      ActiveTDIChild.Close;
  end;
end;

function TFJava.OpenWindowWithClass(const Directory, aClass: string): boolean;
begin
  var s:= GetPathnameForClass(aClass);
  if s <> '' then begin
    SwitchToWindow(s);
    Exit(true);
  end;
  Result:= Open(Directory + ReplaceStr(aClass, '.', '\') + '.java');
end;

procedure TFJava.SetBreakpoints;
begin
  for var i:= 0 to TDIEditFormCount - 1 do
    TDIEditFormGet(i).SetBreakpoints;
end;

procedure TFJava.RearrangeFileHistory(const NewFile: string);
  var FileCount, i: Integer;
      NewItem: TSpTBXItem;
      aMenuItem: TTBCustomItem;
      s: string;
begin
  // already in history?
  FileCount:= MIReopen.Count;
  for i:= FileCount - 1 downto 0 do
    if Pos(NewFile, MIReopen.Items[i].Caption) > 0 then
      MIReopen.Delete(i);
  if not FileExists(NewFile) then exit;

  // new history entry
  NewItem:= TSpTBXItem.Create(Self);
  NewItem.Caption:= '&1 ' + NewFile;
  NewItem.OnClick:= MIMenuOpenClick;
  MIReopen.Insert(0, NewItem);
  FileCount:= MIReopen.Count;
  if assigned(FConfiguration) then begin
    if FileCount > FConfiguration.MaxFileHistory then begin
      aMenuItem:= MIReopen.Items[FileCount-1];
      FreeAndNil(aMenuItem);
      Dec(FileCount);
    end;

    // number history entries
    for i:= 0 to FileCount - 1 do begin
      s:= MIReopen.Items[i].Caption;
      s:= Right(s, Pos(' ', s) + 1);
      FConfiguration.WriteStringU('History', 'File' + IntToStr(i + 1), FConfiguration.RemovePortableDrive(s));
      MIReopen.Items[i].Caption:= '&' + IntToStr(i+1) + ' ' + s;
    end;
    FConfiguration.WriteIntegerU('History', 'Files', FileCount);
  end;
end;

procedure TFJava.SetDockTopPanel;
  var aTopFindToolbar, aHeight: integer;
begin
  var HasMenuBar:= false;
  for var i:= 0 to high(FConfiguration.visMenus) do
    HasMenuBar:= HasMenuBar or FConfiguration.VisMenus[i];
  if HasMenuBar
    then aHeight:= 23
    else aHeight:= 0;

  var allTabsClosed:= true;
  for var i:= 0 to High(FConfiguration.VisTabs) do
    if FConfiguration.VisTabs[i] then allTabsClosed:= false;
  TabsControl.Visible:= not allTabsClosed;

  if not allTabsClosed then
    aHeight:= aHeight + 57
  else if FConfiguration.VisToolbars[0] or FConfiguration.VisToolbars[1] then
    aHeight:= aHeight + 28;
  aTopFindToolbar:= aHeight;

  //if FindToolbar.Visible then
  //  aHeight:= aHeight + 26;

  DebugToolbar.Left:= MainToolbar.Left;
  MainToolBar.Top:= 2;
  if MainToolBar.Visible then begin
    if TabsControl.Visible then begin
      DebugToolbar.Top:= 2 + MainToolBar.Height + 2;
    end else begin
      DebugToolbar.Top:= 2;
      DebugToolbar.Left:= MainToolbar.Left + MainToolbar.Width + 3;
    end;
  end else
    DebugToolbar.Top:= 2;
  PBorder.Left:= TabsControl.Left + TabsControl.Width + 5;
  //FindToolbar.Top:= PPIScale(aTopFindToolbar);
  TBXDockTop.Height:= PPIScale(aHeight);
end;

procedure TFJava.ReadHistory;
  var FileCount, i, j: integer;
      s: string;
      NewItem: TSpTBXItem;
begin
  FileCount:= FConfiguration.ReadIntegerU('History', 'Files', 0);
  j:= 0;
  for i:= 1 to FileCount do begin
    s:= FConfiguration.AddPortableDrive(FConfiguration.ReadStringU('History', 'File' + IntToStr(i), ''));
    if (s <> '') and FileExists(s) then begin
      Inc(j);
      try
        NewItem:= TSpTBXItem.Create(Self);
        NewItem.Caption:= '&' + IntToStr(j) + ' ' + s;
        NewItem.OnClick:= MIMenuOpenClick;
        MIReopen.Add(NewItem);
        FConfiguration.WriteStringU('History', 'File' + IntToStr(j), FConfiguration.RemovePortableDrive(s));
      except
      end;
    end
  end;
  FConfiguration.WriteIntegerU('History', 'Files', j);
end;

procedure TFJava.OpenFiles;
  var s, cur: string;
      i, WinCount, x, y: Integer;
      Mouse, P: TPoint; Rect: TRect;
      E: TFEditForm;
begin
  Screen.OnActiveFormChange:= UpdateMenuItems;
  try
    LockFormUpdate(Self);
    Screen.Cursor:= crHourGlass;
    DisableUpdateMenuItems;
    InteractiveUMLForm:= MakeNewUMLWindow(_(LNGInteractive), '');
    OpenFileStructureAndObjectInspector;
    FMessages.InitAndShow;
    if FConfiguration.LoadFiles then begin
      WinCount:= FConfiguration.ReadIntegerU('Window', 'Wins', 0);
      for i:= 1 to WinCount do begin
        s:= FConfiguration.ReadStringU('Window', 'Win' + IntToStr(i), '');
        OpenFileWithState(s);
      end;
      cur:= FConfiguration.ReadStringU('Window', 'Current', '');
    end;
    for i:= 1 to ParamCount do
      if UpperCase(ExtractFileExt(ParamStr(i))) <> '.INI' then begin
        if open(ParamStr(i)) then
          RearrangeFileHistory(ParamStr(i));
        cur:= Paramstr(i);
      end;

  finally
    OpenFileWithState(cur);
    HSplitterMoved(Self);
    VSplitterMoved(Self);
    UnlockFormUpdate(Self);
    Screen.Cursor:= crDefault;
  end;

  // if one webbrowser is opened, then their is no blinking cursor
  // in the active EditForm! According to Screen ActiveForm a
  // WebBrowser never gets the focus
  // workaround: simulate a click on the EditForm
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 1) then begin
    E:= ActiveTDIChild as TFEditForm;
    GetCursorPos(Mouse);
    P:= E.ClientToScreen(Point(0,0));
    Rect:= E.clientRect;
    x:= P.X + Rect.Right div 2;
    y:= p.y + Rect.Bottom div 2;
    SetCursorPos(x, y);
    if GetSystemMetrics(SM_SWAPBUTTON) = 0 then begin
      mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
      mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    end else begin
      mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
      mouse_event(MOUSEEVENTF_RIGHTUP, 0, 0, 0, 0);
    end;
    Application.ProcessMessages;
    SetCursorPos(Mouse.x, Mouse.y);
  end;
  EnableUpdateMenuItems;
  if FConfiguration.FirstStartAfterInstallation then
    MIDefaultLayoutClick(Self);
  with TFUpdateDialog.Create(Self) do begin
    CheckAutomatically;
    Free;
  end;
  EditorAgeTimer.Enabled:= FConfiguration.CheckAge;
  ChangeStyle(FConfiguration.GUIStyle);

  if not FileExists(FConfiguration.JavaInterpreter) then
    TThread.ForceQueue(nil, procedure
      begin
        FConfiguration.ShowPage(0);
        MIConfigurationClick(Self);
      end);
end;

procedure TFJava.ConnectGUIAndJavaWindow(GUIForm: TFGUIForm);
begin
  var s:= ChangeFileExt(GUIForm.Pathname, '.java');
  EditorForm:= TFEditForm(getTDIWindow(s));
  if assigned(EditorForm) then begin
    EditorForm.Partner:= GUIForm;
    GUIForm.Partner:= EditorForm;
    //GUIForm.FrameType:= EditorForm.FrameType;
  end else
    ErrorMsg(Format(_(LNGAssociatedJavaFileNotFound), [s]));
  FObjectInspector.RefreshCBObjects;
end;

{--- Start-Menü ---------------------------------------------------}

procedure TFJava.SaveBeforeCompile;
begin
  FixWindows(true);
  for var i:= 0 to Length(FixedWindows) - 1 do
    if FixedWindows[i].FormTag = 1 then begin
      var E:= (FixedWindows[i] as TFEditForm);
      if E.Modified or not FileExists(E.Pathname) then
        DoSave(E, false);
    end;
end;

procedure TFJava.Compile;
begin
  if assigned(EditorForm) and EditorForm.isJava then begin
    if Pos(FConfiguration.JavaCache, EditorForm.Pathname) = 1 then
      ErrorMsg(_('Files in the cache folder are not compiled.') + ': ' + EditorForm.Pathname)
    else begin
      myJavaCommands.setManyCompiling(false);
      myJavaCommands.CompileForm(EditorForm);
      try
        if assigned(EditorForm.Editor) and assigned(Editorform.Editor.Gutter) and
          not myJavaCommands.SuccessfullCompiled and
          not EditorForm.Editor.Gutter.ShowLineNumbers
        then EditorForm.SBNumbersClick(Self);
        if not myJavaCommands.SuccessfullCompiled
          then ShowCompileErrors
          else EditorForm.InitShowCompileErrors;
      except on e: exception do
        FConfiguration.Log('TFJava.Compile', E);
      end;
    end
  end else
    MICompileAllClick(Self);
end;

procedure TFJava.TBCompileJavaClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if TBCompileJava.Hint = _(LNGResetProgram)
    then MIProgramResetClick(Self)
    else MICompileClick(Self);
  EnableUpdateMenuItems;
end;

procedure TFJava.MICompileClick(Sender: TObject);
begin
  SaveBeforeCompile;
  var UMLForm:= getActiveUML;
  if assigned(UMLForm) then begin
    var SL:= UMLForm.getFilesAndPackages(true);
    CompileList(SL);
    FreeAndNil(SL);
  end else begin
    Editorform:= getActiveEditor;
    Compile;
  end;
  RefreshUMLWindows;
end;

procedure TFJava.CompileList(SL: TStringList);
  var Pathname, Package: string; i, p: integer;
begin
  FMessages.DeleteTab(K_Compiler);
  FMessages.StatusMessage('');
  myJavaCommands.setManyCompiling(true);
  for i:= 0 to SL.Count - 1 do begin
    Pathname:= SL[i];
    p:= Pos('|', Pathname);
    if p > 0 then begin
      Package:= copy(Pathname, p+1, length(Pathname));
      delete(Pathname, p, length(Pathname));
    end else
      Package:= '';
    if FileExists(Pathname)
      then myJavaCommands.Compile(Pathname, Package)
      else FMessages.OutputLineTo(K_Compiler, Format(_(LNGFileNotFound), [Pathname]));
  end;
  if myJavaCommands.ManyCompilingErrorOccured then
    ShowCompileErrors;
  myJavaCommands.setManyCompiling(false);
end;

procedure TFJava.CompileOneWith(const Pathname: string);
begin
  var SL:= TStringList.Create;
  SL.Add(Pathname);
  SaveBeforeCompile;
  CompileList(SL);
  FreeAndNil(SL);
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
      0: myJavaCommands.Upload('Lejos-Firmware');
      1: with TFMindstormsDialog.create(Self) do
           ShowModal;
      2: with TFMindstormsEV3Dialog.create(Self) do
          ShowModal;
    end
  else MICompileAllClick(Self);
end;

procedure TFJava.MICompileAllClick(Sender: TObject);
begin
  SaveBeforeCompile;
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then begin
    var SL:= (ActiveTDIChild as TFUMLForm).getFilesAndPackages(false);
    CompileList(SL);
    FreeAndNil(SL);
  end else
    myJavaCommands.CompileAll;
  RefreshUMLWindows;
end;

var Nesting: boolean;

procedure TFJava.TBFontMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var i, delta, FormTag: integer;
begin
  if Nesting then
    exit;
  Nesting:= true;
  if Button = mbLeft
    then delta:= +1
    else delta:= -1;
  FormTag:= GetActiveFormTag;
  if FormTag in [1, 4, 5, 6, 11, 14] then
    for i:= 0 to TDIFormsList.Count - 1 do
      TDIFormsList[i].SetFontSize(delta);

  case FormTag of
    2: if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then
         ActiveTDIChild.SetFontSize(delta);
    3: for i:= 0 to TDIFormsList.Count  - 1 do
         if TDIFormsList[i].FormTag =  3 then
           TDIFormsList[i].SetFontSize(delta);
    7: FMessages.SetFontSize(delta);
    8: FObjectInspector.SetFontSize(delta);
    9: ; // ObjectGenerator has a modal window
   10: FScpHint.SetFontSize(delta);
   12: ; // FTooltip
   13: ; // TSynBaseCompletionProposalFormEx
   17: FGuiDesigner.Zooming(delta);
  end;
  Nesting:= false;
end;

procedure TFJava.TBRunClick(Sender: TObject);
begin
  if TBRun.Hint = _(LNGResetProgram) then
    MIProgramResetClick(Self)
  else if MIRun.Enabled or myDebugger.Running then
    MIRunClick(self)
end;

function TFJava.IsJavaApplet(Editform: TFEditForm): Boolean;
begin
  Result:= Assigned(Editform) and Editform.isApplet or
           FConfiguration.JavaStartKlasseIsApplet;
end;

function TFJava.hasBreakpoints: Boolean;
begin
  Result:= myDebugger.hasBreakpoints;
  var i:= 0;
  var count:= TDIEditFormCount;
  while not Result and (i < count) do begin
    if TDIEditFormGet(i).hasBreakpoints then begin
      Result:= true;
      break;
    end;
    inc(i);
  end;
end;

procedure TFJava.setEditFormWithBreakpoints;
begin
  for var i:= 0 to TDIEditFormCount - 1 do begin
    var aForm:= TDIEditFormGet(i);
    if aForm.hasBreakpoints then begin
      EditorForm:= aForm;
      break;
    end;
  end;
end;

function TFJava.PreCompile(aForm: TFEditForm; Pathname: string): boolean;
  var Saved, ok: boolean;
begin
  Saved:= false;
  if aForm = nil then
    aForm:= TFEditForm(FJava.getTDIWindowType(Pathname, '%E%'));
  if Assigned(aForm) and aForm.isHTML then begin
    if aForm.Modified or aForm.DefaultFilename then
      DoSave(aForm, WithoutBackup);
    Pathname:= ChangeFileExt(aForm.Pathname, '.java');
    aForm:= TFEditForm(FJava.getTDIWindowType(Pathname, '%E%'));
  end;

  if Assigned(aForm) and Assigned(aForm.Editor) then begin
    if aForm.Modified or aForm.DefaultFilename then begin
      DoSave(aForm, WithoutBackup);
      Saved:= not aForm.Modified;
    end;
    Pathname:= aForm.Pathname;
  end;

  if hasJavaExtension(Pathname) then begin
    if not FConfiguration.HasAgeClass(Pathname) or Saved then begin  // // no class file or freshly saved
      if assigned(aForm)
        then myJavaCommands.CompileForm(aForm)
        else myJavaCommands.Compile(Pathname, '');
      RefreshUMLWindows;
      ok:= myJavaCommands.SuccessfullCompiled;
    end else
      ok:= true;
    if Assigned(aForm) and not ok and not aForm.Editor.Gutter.ShowLineNumbers
      then aForm.SBNumbersClick(Self);
  end else
    ok:= true;
  Result:= ok;
end;

procedure TFJava.DoRunApplet(const JavaProgramm: string; E: TFEditForm);
  var Applet: string;
begin
  if FConfiguration.HasClass(JavaProgramm) then begin
    var Debugging:= hasBreakpoints;
    if Debugging and not FConfiguration.JavaAppletviewerOK then
      ErrorMsg('no Appletviewer for debugging')     // ToDo
    else begin
      DoHTMLforApplet(E, false, false, false, Debugging, Applet);
      if Debugging
        then myDebugger.DebugApplet(JavaProgramm)
        else if (FConfiguration.AppletStart in [0, 1]) and FConfiguration.JavaAppletviewerOK then
          myJavaCommands.AppletViewer(Applet)
        else begin
          CloseBrowser;
          CallApplet(Applet);
        end;
    end;
  end;
end;

procedure TFJava.DoRunProgram(const JavaProgram, AParameter: string; E: TFEditForm);
  var FrameType: integer; Gui, JavaFX: boolean;
      Package: string;
begin
  FrameType:= FConfiguration.GetFrameType(JavaProgram);
  Gui:= (FrameType > 1);
  JavaFX:= (FrameType = 8);
  if hasJavaExtension(JavaProgram) then begin
    Package:= FConfiguration.getPackage(JavaProgram, true);
    if FConfiguration.HasClass(JavaProgram) then begin
      if hasBreakpoints then
        if FConfiguration.MindStormsMode then begin
          ErrorMsg(_(LNGMindstormsPrograms));
          if assigned(E) then
            E.ClearBreakpoints;
        end else
          myDebugger.DebugProgram(JavaProgram, AParameter, Package, Gui, JavaFX)
      else if FConfiguration.MindstormsMode
        then myJavaCommands.Upload(JavaProgram)
      else
        myJavaCommands.Run(AParameter, JavaProgram, Package, Gui, JavaFX);
    end
  end
end;

procedure TFJava.doRunHTML(const Pathname: string);
begin
  if (FConfiguration.AppletStart in [0, 2]) or not FConfiguration.JavaAppletviewerOK then begin
    CloseBrowser;
    Application.ProcessMessages;
    Sleep(1000);
    CallApplet(Pathname);
  end else
    MIAppletviewerClick(nil);
end;

function TFJava.getEditFormWithMain: TFEditForm;
begin
  Result:= nil;
  var i:= 0;
  while i < TDIEditFormCount do begin
    var aForm:= TDIEditFormGet(i);
    if aForm.hasMainInModel then begin
      Result:= aForm;
      break;
    end;
    inc(i);
  end;
end;

procedure TFJava.DoRun(EditForm: TFEditForm; const Pathname: string);
  var JavaProgramm, AParameter: string; Main: TFEditForm;
      isApplet: boolean;
begin
  AParameter:= '';
  isApplet:= false;

  if assigned(EditForm) then begin
    if EditForm.isJUnitTestClass then begin
      EditForm.RunTests;
      exit;
    end;

    JavaProgramm:= EditForm.Pathname;
    AParameter:= EditForm.Parameter;
    isApplet:= EditForm.isApplet or EditForm.IsHTMLApplet;
    if not (isApplet or EditForm.hasMainInModel) then begin
      Main:= getEditFormWithMain;
      if assigned(Main) then begin
        JavaProgramm:= Main.Pathname;
        AParameter:= Main.Parameter;
        isApplet:= Main.isApplet;
        EditForm:= Main;
      end;
    end;
  end else begin
    JavaProgramm:= Pathname;
    AParameter:= FConfiguration.JavaStartClass;
    EditForm:= OpenEditForm(Pathname, true);
    if assigned(EditForm) then
      isApplet:= EditForm.isApplet;
  end;

  if isApplet
    then DoRunApplet(JavaProgramm, EditForm)
    else DoRunProgram(JavaProgramm, AParameter, EditForm);
end;

procedure TFJava.RunAndroid;
  var gradleWfound: boolean;
      Applicationname, CommandLine, shortenedPath: string;
begin
  // The following will check if the Android SDK path is configured correctly
  if FConfiguration.AndroidMode and not MIProgramReset.Enabled then begin
    if Assigned(ActiveTDIChild)
      then DoSave(ActiveTDIChild, FConfiguration.CreateBAKFiles)
      else exit;
    FConfiguration.CheckAllFilesAndFolders;
    if (FConfiguration.EAndroidSDKFolder.Color = clRed) or (FConfiguration.AndroidSDKFolder = '') then begin
      ErrorMsg('Android SDK path is not properly set.');
      exit;
    end;

    // Find gradlew.bat in the project folder
    gradleWfound:= false;

    shortenedPath:= ActiveTDIChild.Pathname;
    while ((Pos('\',shortenedPath)<>0) and not gradleWfound) do begin
      shortenedPath:= copy(shortenedPath, 1, LastDelimiter('\',shortenedPath) - 1);
      if FileExists(dissolveUsername(shortenedPath + '\gradlew.bat')) then
        gradleWfound:=true;
    end;

    if gradleWfound then begin
      ApplicationName:= FConfiguration.EditorFolder + 'assemble.bat';
      CommandLine:= HideBlanks(shortenedPath) + ' ' +
                    HideBlanks(FConfiguration.AndroidSDKFolder) + ' ' +
                    HideBlanks(FConfiguration.JDKFolder);
      myJavaCommands.ExecWithPipe(ApplicationName, CommandLine, '.')
    end else
      ErrorMsg('gradlew.bat not found');
    end
end;

procedure TFJava.MIRunClick(Sender: TObject);
  var JavaProgramm, aFile: string; i: integer; ok: boolean;
      EditForm: TFEditForm; UmlForm: TFUMLForm; SL: TStringList;
begin
  // due to
  // [006A3E2E]{javaeditor.exe} UJava.TFJava.MIRunClick (Line 3065, "UJava.pas" + 1) + $7

  if FConfiguration.AndroidMode then begin
    RunAndroid;
    exit;
  end;

  if myJavaCommands = nil then exit;

  if myDebugger.Running then begin
    PrepareStep;
    myDebugger.NewCommand(3, 'cont');
    exit;
  end;
  // if myDebugger.Running and MIRun.Enabled then exit; // prevent doubled execution

  for i:= TDIFormsList.Count - 1 downto 0 do
    if (TDIFormsList[i] is TFEditForm) and TDIFormsList[i].Modified then
      PreCompile(TDIFormsList[i] as TFEditForm, '');

  EditForm:= getActiveEditor;
  if assigned(EditForm) and EditForm.isHTML then begin
    DoRunHTML(EditForm.Pathname);
    exit;
  end;

  FMessages.DeleteTab(K_Compiler);
  if assigned(EditForm) then
    if not PreCompile(EditForm, '') then exit;

  JavaProgramm:= '';
  UmlForm:= getActiveUML;
  if assigned(UmlForm) then begin
    myJavaCommands.setManyCompiling(true);
    SL:= UMLForm.getAllPathnames;
    ok:= true;
    for i:= 0 to SL.Count - 1 do begin
      aFile:= SL.Strings[i];
      ok:= PreCompile(nil, aFile) and ok;
    end;
    myJavaCommands.setManyCompiling(false);
    JavaProgramm:= UmlForm.getFileWithMain;
    EditForm:= getTDIWindowType(JavaProgramm, '%E%') as TFEditForm;
    FreeAndNil(SL);
    if not ok then exit;
  end;

  if FConfiguration.JavaStartClass <> '' then begin
    if not FileExists(FConfiguration.JavaStartClass) then begin
      ErrorMsg(Format(_(LNGFileNotFound), [FConfiguration.JavaStartClass]));
      exit;
    end;
    if PreCompile(nil, FConfiguration.JavaStartClass) then begin
      JavaProgramm:= FConfiguration.JavaStartClass;
      if assigned(EditForm) and (EditForm.Pathname <> JavaProgramm) then
        EditForm:= nil;
    end else
      exit;
  end;

  if EditForm = nil then
    EditForm:= getEditorWithMain;
  if assigned(EditForm) and not EditForm.Modified or (JavaProgramm <> '') then
    DoRun(EditForm, JavaProgramm);

  if Assigned(EditForm) and Assigned(EditForm.Partner) then
    try
      EditForm.Partner.Repaint;
    except on e: exception do
      FConfiguration.Log('TFJava.MIRunClick repaint', e);
    end;
end;

procedure TFJava.Run(const Pathname: string);
  var aFile: string; i: integer; ok: boolean; SL: TStringList;
begin
  if myDebugger.Running and MIRun.Enabled then exit; // prevents doubled execution
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then begin
    SL:= (ActiveTDIChild as TFUMLForm).getAllPathnames;
    ok:= true;
    for i:= 0 to SL.Count - 1 do begin
      aFile:= SL.Strings[i];
      ok:= PreCompile(nil, aFile) and ok;
    end;
    FreeAndNil(SL);
    if ok then
      DoRun(nil, Pathname);
  end;
end;

procedure TFJava.MIDebugClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then
    (ActiveTDIChild as TFUMLForm).DebugJE2Java;
end;

procedure TFJava.MIDebuggerClick(Sender: TObject);
begin
  if FConfiguration.MindstormsMode then
    ShowMessage(_(LNGMindstormsPrograms))
  else begin
    myDebugger.BreakPointAtMain:= true;
    MIRunClick(nil);
  end;
end;

procedure TFJava.MIDefaultLayoutClick(Sender: TObject);
begin
  LockFormUpdate(Self);
  FMessages.ShowIt;
  BottomDockPanel.Height:= ClientHeight div 5;
  FMessages.MyDock;
  if assigned(FFileStructure) then
    FFileStructure.ShowIt;
  if assigned(FObjectInspector) then
    FObjectInspector.ShowIt;
  VSplitter.Visible:= true;
  RightDockPanel.Width:= ClientWidth div 4;
  if assigned(FFileStructure) then
    FFileStructure.ManualDock(FJava.RightDockPanel, nil, alTop);
  if assigned(FJUnitTests) then begin
    FJUnitTests.ShowIt;
    FJUnitTests.ManualDock(FJava.RightDockPanel, nil, alTop);
  end;
  if assigned(FObjectInspector) then
    FObjectInspector.ManualDock(FJava.RightDockPanel, nil, alBottom);
  VSplitterMoved(Self);
  HSplitterMoved(Self);
  UnlockFormUpdate(Self);
end;

procedure TFJava.UpdateLayoutRightDockPanel(SetWidthHeight: boolean = false);
begin
  if RightDockPanel.Width > 0 then begin
    //LockFormUpdate(Self); this prevented minification
    RightDockPanel.Width:= ClientWidth div 4;
    if assigned(FFileStructure) then
      FFileStructure.ManualDock(FJava.RightDockPanel, nil, alTop);
    if assigned(FJUnitTests) then begin
      FJUnitTests.ShowIt;
      FJUnitTests.ManualDock(FJava.RightDockPanel, nil, alTop);
    end;
    if assigned(FObjectInspector) then
      FObjectInspector.ManualDock(FJava.RightDockPanel, nil, alBottom);
    //UnlockFormUpdate(Self);
  end;
end;

procedure TFJava.MIProgramResetClick(Sender: TObject);
begin
  myDebugger.Terminate;
  myJavaCommands.Terminate;
end;

procedure TFJava.MIParameterClick(Sender: TObject);
begin
  with TFParameterDialog.Create(Self) do begin
    EStartClass.Text:= FConfiguration.JavaStartClass;
    if Assigned(EditorForm) then begin
      LParameter.Caption:= LParameter.Caption + ' ' + ExtractFilename(EditorForm.PathName);
      EParameter.Text:= EditorForm.Parameter;
      EParameter.Enabled:= true;
      EParameter.Color:= clWindow;
      Pathname:= Editorform.Pathname;
    end else begin
      EParameter.Text:= '';
      EParameter.Enabled:= false;
      EParameter.Color:= clInactiveCaption;
      Pathname:= '';
    end;
    if ShowModal = mrOK then begin
      FConfiguration.JavaStartClass:= EStartClass.Text;
      FConfiguration.WriteStringU('Program', 'StartClass', FConfiguration.JavaStartClass);
      if Assigned(EditorForm) then
        EditorForm.Parameter:= Trim(EParameter.Text);
    end;
    Free;
  end;
  UpdateMenuItems(Self);
end;

procedure TFJava.MIHTMLforAppletClick(Sender: TObject);
  var EditForm: TFEditForm; Applet: string; aForm: TFForm;
begin
  DisableUpdateMenuItems;
  EditForm:= EditorForm;
  if assigned(EditForm) and EditForm.IsHTMLApplet then begin
    SwitchToWindow(ChangeFileExt(EditForm.Pathname, '.java'));
    EditForm:= EditorForm;
  end;

  with EditForm do begin
    if not IsJava then exit;
    if Modified then DoSave(EditForm, WithoutBackup);
    Applet:= ChangeFileExt(Pathname, '.html');
    if WindowOpened(Applet, aForm) then begin
      aForm.Close;
      Application.ProcessMessages;
    end;
    DoHTMLForApplet(EditForm, true, Sender = MIHTMLforJavaPlugIn, true, false, Applet);
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.DoHTMLForApplet(EditForm: TFEditForm; ForceCreate, PlugIn, aShow, Debug: boolean; var Applet: string);
  var Child: TFEditForm; aForm: TFForm;
      call, HTMLConverter, ErrFile, Package, aClass, dir, AppletArchive, AppletCode, W, H: string;
      withJEApplets: boolean; WH: TPoint; n: integer;
begin
  with EditForm do begin
    Package:= getPackage;
    aClass:= ChangeFileExt(ExtractFilename(Pathname), '');
    dir := ExtractFilePath(Pathname);
    if Package <> '' then begin
      n:= CountChar('.', Package) + 1;
      while n > 0 do begin
        dir:= copy(dir, 1, Length(dir)-1);
        dir:= copy(dir, 1, LastDelimiter('\', dir));
        dec(n);
      end;
      aClass:= Package + '.' + aClass;
    end;
    Applet:= dir + aClass + '.html';
    WH:= getWidthAndHeight;
    W:= IntToStr(WH.X-8);
    H:= IntToStr(WH.Y-34);
    withJEApplets:= hasWord('NumberField') or hasWord('JNumberField') or hasWord('Turtle');
  end;

  if ForceCreate or not FileExists(Applet) then begin
    LockWindow(FJava.Handle);

    Child:= TFEditForm(FormFactory(fkEditor));
    Child.New(Applet);
    Child.HTMLforApplet(W, H, FConfiguration.getCharset,
                        dir, aClass, withJEApplets, Debug);
    Child.CheckAgeEnabled:= false;
    Child.Save(WithoutBackup);
    ErrFile:= FConfiguration.TempDir + 'error.txt';
    if PlugIn then begin
      HTMLConverter:= FConfiguration.JDKFolder + '\lib\htmlconverter.jar';
      call:= '-jar ' + HTMLConverter + ' -f ' + ExtractFileName(Applet);
      if myJavaCommands.ExecAndWait(FConfiguration.JavaInterpreter, call, ExtractFilePath(Applet), ErrFile, SW_HIDE) then
        Child.Editor.Lines.LoadFromFile(Applet);
      if GetFileSize(ErrFile) > 0 then begin
        FMessages.ShowTab(K_Messages);
        FMessages.ShowMessages(ErrFile);
      end;
    end;
    Child.Save(WithoutBackup);
    if FConfiguration.ShowHTMLforApplet or aShow
      then RearrangeFileHistory(Applet)
      else Child.Close;
    UpdateMenuItems(Self);
    UnlockWindow;
  end else begin
    AppletArchive:= FConfiguration.getAppletArchiv;
    AppletCode:= '<applet code="' + ReplaceStr(aClass, '.', '/') + '.class" ' + AppletArchive + ' width="' + W + '" height="' + H + '">';

    if WindowOpened(Applet, aForm) then begin
      SwitchToWindow(Applet);
      // MantisBT
      if assigned(EditorForm) and (Pos(AppletCode, EditorForm.Editor.Text) = 0) then
        EditorForm.ReplaceLine('<applet code=', AppletCode);
    end else begin
      Child:= OpenEditForm(Applet, true); // open hidden
      if assigned(Child) then begin
        if assigned(Child.Editor) and (Pos(AppletCode, Child.Editor.Text) = 0) then
          Child.ReplaceLine('<applet code=', AppletCode);
        Child.close;
      end;
    end;
  end;
end;

procedure TFJava.MIAppletviewerClick(Sender: TObject);
  var Applet: string;
begin
  if EditorForm <> nil then
    if not PreCompile(EditorForm, '') then exit;

  if EditorForm.IsHTMLApplet
    then Applet:= EditorForm.Pathname
    else DoHTMLForApplet(EditorForm, false, false, false, false, Applet);
  myJavaCommands.AppletViewer(Applet);
end;

procedure TFJava.DoDisassemble(const Filename: string);
  var Child: TFEditForm;
      Disassembler, Call, aFile, s, ErrFile: string;
begin
  aFile:= ChangeFileExt(ExtractFilename(Filename), '');
  ErrFile:= FConfiguration.TempDir + 'error.txt';
  Screen.Cursor:= crHourGlass;
  try
    Disassembler:= FConfiguration.JavaDisassembler;
    if Pos('javap.exe', Disassembler) > 0
      then Call:= '-classpath ' + FConfiguration.getClassPath + ' '
      else Call:= '';
    Call:= Call + FConfiguration.JavaDisassemblerParameter + ' ' + aFile;
    if myJavaCommands.ExecAndWait(Disassembler, Call, ExtractFilePath(Filename), ErrFile, SW_HIDE) then begin
    //if ExecWithoutWait(Disassembler, Call, ExtractFilePath(Filename), SW_HIDE) then begin
      Child:= TFEditForm(FormFactory(fkEditor));
      if Pos('jad.exe', Disassembler) > 0 then begin
        FMessages.ShowMessages(ErrFile);
        s:= ChangeFileExt(FileName, '.jad');
        Child.New(s);
        Child.Editor.Lines.LoadFromFile(s);
      end else begin
        s:= ChangeFileExt(FileName, '.txt');
        Child.New(s);
        Child.Editor.Lines.LoadFromFile(ErrFile);
      end;
      Child.Editor.Lines.SaveToFile(s);
      FMessages.StatusMessage(s + ' ' + _('disassembled'));
    end else begin
      Screen.Cursor:= crDefault;
      ErrorMsg(Format(_('Error during disassembling of %s'), [s]));
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFJava.MIDissasemblerClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  with ODOpen do begin
    if assigned(ActiveTDIChild)
      then InitialDir:= ExtractFilePath(ActiveTDIChild.Pathname)
      else InitialDir:= FConfiguration.Sourcepath;
    Filename:= '*.class';
    if Execute then begin
      FConfiguration.Sourcepath:= ExtractFilePath(Filename);
      DoDisassemble(Filename);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJavaDocClick(Sender: TObject);
  var dir, aFile, Files, Parameter, Package: string; i: Integer;
      EditForm: TFEditForm;
begin
  if EditorForm = nil then exit;
  DisableUpdateMenuItems;
  Files:= '';
  for i:= 0 to TDIEditFormCount -1 do begin
    EditForm:= TDIEditFormGet(i);
    if EditForm.IsJava then begin
      if EditForm.Modified then EditForm.Save(WithoutBackup);
      Files:= Files + ' ' + HideBlanks(EditForm.Pathname);
    end;
  end;

  aFile:= EditorForm.Pathname;
  Package:= EditorForm.getPackage;
  Parameter:= ' -classpath ' + FConfiguration.getClassPath(aFile, Package);
  Parameter:= Parameter + ' ' + FConfiguration.JavaDocParameter;
  if (EditorForm.Encoding <> 'ANSI') and (Pos('encoding', Parameter) = 0) then
    if EditorForm.Encoding = 'UTF-8'
      then Parameter:= Parameter + ' -encoding UTF-8'
      else Parameter:= Parameter + ' -encoding UTF-16';
  dir:= ExtractFilePath(aFile) + 'Doc\';
  SysUtils.ForceDirectories(dir);

  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  FMessages.OutputLineTo(K_Messages, FConfiguration.JavaDoc + ' '+ Parameter + ' ' + Files);
  Screen.Cursor:= crHourGlass;
  if myJavaCommands.ExecAndWait(FConfiguration.JavaDoc, Parameter + ' ' + Files, dir,
                 FConfiguration.TempDir + 'error.txt', SW_Hide) then begin
    FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
    if FileExists(dir + 'index.html')
      then CallHelp(dir + 'index.html')
      else FMessages.StatusMessage(_('Error during creation of documentation.'))
  end;
  Screen.Cursor:= crDefault;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJavaResetClick(Sender: TObject);
begin
  getComJava.JavaReset;
end;

procedure TFJava.MIJUnitCreateTestclassClick(Sender: TObject);
  var aClassname, Filename: string; SL: TStringList;
begin
  DisableUpdateMenuItems;
  if assigned(EditorForm) then begin
    aClassname:= ChangeFileExt(ExtractFilename(EditorForm.Pathname), '') + 'Test';
    Filename:= ExtractFilepath(EditorForm.Pathname) + aClassname + '.java';
    if FileExists(Filename) and
               (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                          mtConfirmation, mbYesNoCancel, 0) = mrYes) or
               not FileExists(Filename)
    then begin
      SL:= TStringList.Create;
      SL.Text:= FTemplates.GetTemplate(aClassname, 12);
      if SL.Text = '' then SL.Text:= FTemplates.GetTestClassCode(aClassname);
      SL.SaveToFile(Filename);
      Open(Filename);
      EditorForm.ParseSourcecode(true);
      UpdateMenuItems(Self);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJunitManualClick(Sender: TObject);
begin
  if FConfiguration.JUnitOK then
    if FConfiguration.JUnitManual = ''
      then InformationMsg(_('The JUnit manual is not installed.'))
      else CallHelp(FConfiguration.JUnitManual + '?overview-summary.html');
end;

procedure TFJava.MIJUnitRunAllTestsClick(Sender: TObject);
begin
  if assigned(EditorForm) then
    EditorForm.RunTests;
end;

procedure TFJava.JarCall(const call, dir: string);
begin
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  FMessages.OutputLineTo(K_Interpreter, _('Jar call') + ': ' +
                         HideBlanks(FConfiguration.JavaJar) + ' ' + call);
  Screen.Cursor:= crHourGlass;
  if myJavaCommands.ExecAndWait(FConfiguration.JavaJar, call, dir,
    FConfiguration.TempDir + 'error.txt', SW_Hide) then
    FMessages.ShowInterpreter(FConfiguration.TempDir + 'error.txt');
  Screen.Cursor:= crDefault;
end;

procedure TFJava.JarOpen(const filename: string);
  var SL: TStrings; i, p, count: integer; s, Ext, dir: string;
begin
  DisableUpdateMenuItems;
  dir:= ExtractFilePath(filename);
  JarCall('xvf ' + HideBlanks(filename), dir);
  Screen.Cursor:= crHourGlass;
  SL:= FMessages.MInterpreter.Lines;
  count:= 0;
  for i:= 1 to SL.Count - 1 do begin
    s:= SL[i];
    // extracted:
    p:= Pos(': ', s);
    if p > 0 then begin
      delete(s, 1, p + 1);
      Ext:= ExtractFileExt(s);
      if Pos(Ext, '.java;.class;.jfm;.uml;.html;.htm;.jsp;.txt') > 0 then begin
        open(ExpandFileName(dir + toWindows(s)));
        inc(count);
        if count = 20 then begin
          ShowMessage(_('Too much files to open.'));
          break;
        end;
      end;
    end;
  end;
  Screen.Cursor:= crDefault;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIJarCreateClick(Sender: TObject);
  var Filename, package: string; EditForm: TFEditForm;
begin
  Filename:= '';
  EditForm:= getEditFormWithMain;
  if assigned(EditForm) then begin
    Filename:= EditForm.Pathname;
    package:= EditForm.getPackage;
  end else if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then begin
    Filename:= (ActiveTDIChild as TFUMLForm).getFileWithMain;
    Package:= '';
  end;
  if Filename <> ''
    then DoJarCreate(Filename, Package)
    else ErrorMsg('no main method found');
end;

procedure TFJava.DoJarCreate(Filename, Package: string);
  var dir, call, JarName, myJavaJarManifest, JavaJarParameter,
      cp, MainClass, fName, images, jarCreate: string;
      pm, pf: Integer;
      FStream: TFileStream;
begin
  DisableUpdateMenuItems;
  dir:= ExtractFilePath(Filename);
  fName:= ExtractFilename(Filename);
  package:= ReplaceStr(Package, '.', '\');
  if (package <> '') and endsWith(dir, package + '\') then begin
    delete(dir, length(dir) - length(Package), length(package) + 1);
    Filename:= dir + fname;
  end;

  cp:= FConfiguration.JarClassPath;
  if (Pos('JEClasses.jar', cp) > 0) and not FileExists(dir + 'JEClasses.jar') then
    CopyFile(PChar(FConfiguration.EditorFolder + 'JEClasses.jar'), PChar(dir + 'JEClasses.jar'), false);

  JarName:= ExtractFileName(ChangeFileExt(Filename, '.jar'));

  myJavaJarManifest:= Trim(FConfiguration.JavaJarManifest);
  if myJavaJarManifest = '' then begin
    MainClass:= ChangeFileExt(ExtractFilename(Filename), '');
    if Package <> '' then MainClass:= Package + '.' + MainClass;

    myJavaJarManifest:= FConfiguration.TempDir + 'MANIFEST.MF';
    DeleteFile(myJavaJarManifest);
    FStream:= TFileStream.Create(myJavaJarManifest, fmCreate or fmShareExclusive);
    StreamWriteln(FStream, 'Class-Path: ' + cp);
    StreamWriteln(FStream, 'Main-Class: ' + MainClass);
    FreeAndNil(FStream);
  end;

  JavaJarParameter:= FConfiguration.JavaJarParameter;
  pm:= Pos('m', JavaJarParameter);
  if (pm = 0) and (myJavaJarManifest <> '') then begin
    JavaJarParameter:= JavaJarParameter + 'm';
    pm:= Pos('m', JavaJarParameter);
  end;

  myJavaJarManifest:= HideBlanks(myJavaJarManifest);
  pf:= Pos('f', JavaJarParameter);
  if pm > 0 then
    if pm > pf
      then call:= JavaJarParameter + ' ' + JarName + ' ' + myJavaJarManifest
      else call:= JavaJarParameter + ' ' + myJavaJarManifest + ' ' + JarName
  else
    call:= JavaJarParameter + ' ' + JarName;

  jarCreate:= FConfiguration.JarCreateCurrent;
  if Pos('images', jarCreate) = 0 then begin
    images:= ExtractFilePath(Filename) + 'images';
    if SysUtils.DirectoryExists(images)
      then jarCreate:= jarCreate + ' images';
  end;

  if FileExists(dir + 'JEClasses.jar') and (Pos('JEClasses.jar', jarCreate) = 0) then
    jarCreate:= jarCreate + ' JEClasses.jar';

  call:= call + ' ' + jarCreate;
  JarCall(call, dir);
  EnableUpdateMenuItems;
end;

procedure TFJava.DoJarCreateEV3(const Filename: string);
  var dir, call, JarName, myJavaJarManifest, JavaJarParameter, aClassname: string;
      FStream: TFileStream;
begin
  dir:= ExtractFilePath(Filename);
  aClassname:= ChangeFileExt(ExtractFilename(Filename), '');
  JarName:= aClassname + '.jar';
  myJavaJarManifest:= FConfiguration.TempDir + 'MANIFEST.MF';
  DeleteFile(myJavaJarManifest);
  FStream:= TFileStream.Create(myJavaJarManifest, fmCreate or fmShareExclusive);
  StreamWriteln(FStream, 'Main-Class: ' + aClassname);
  FreeAndNil(FStream);

  JavaJarParameter:= '-cfm';
  myJavaJarManifest:= HideBlanks(myJavaJarManifest);
  call:= JavaJarParameter + ' ' + JarName + ' ' + myJavaJarManifest;
  call:= call + ' ' + FConfiguration.JarCreateCurrent;
  JarCall(call, dir);
end;

procedure TFJava.MIJarPackClick(Sender: TObject);
  var dir, aFile, files, call, JarName, s: string;
      i: integer; Form: TFForm;
begin
  DisableUpdateMenuItems;
  if assigned(ActiveTDIChild)
    then aFile:= ActiveTDIChild.Pathname
    else exit;
  dir:= ExtractFilePathEx(aFile);
  JarName:= HideBlanks(ChangeFileExt(ExtractFilename(aFile), '.jar'));
  if FConfiguration.JarPackFiles = FConfiguration.CBJarPack.Items[0] then begin
    files:= '';
    for i:= 0 to myTabBar.Tabs.Count - 1 do begin
      Form:= getTabForm(i);
      if Form.FormTag in [1, 2, 3] then begin
        s:= Form.Pathname;
        s:= ExtractRelativePath(dir, s);
        files:= files + ' ' + HideBlanks(s);
      end;
    end;
  end else
    files:= FConfiguration.JarPackFiles;
  call:= 'cvMf ' + JarName + ' ' + files;
  JarCall(call, dir);
  EnableUpdateMenuItems;
end;

procedure TFJava.JarShowUnpackOpen(i: integer; const Filename: string);

  function OverwriteOK: boolean;
  begin
    Result:= true;
    for var i:= 1 to FMessages.LBMessages.Items.Count - 1 do begin
      var s:= FMessages.LBMessages.Items[i];
      delete(s, 1, Pos(':', s));
      delete(s, 1, Pos(':', s));
      delete(s, 1, Pos(' ', s));
      delete(s, 1, Pos(' ', s));
      delete(s, 1, Pos(' ', s));
      s:= ExpandFileName(ToWindows(s));
      if FileExists(s) then begin
        Result:= MessageDlg(_('Overwrite with jar files?'), mtConfirmation, mbYesNoCancel,0) = mrYes;
        exit;
      end;
    end;
  end;

begin
  try
    var path:= ExtractFilePath(Filename);
    JarCall('tvf ' + HideBlanks(Filename), path);
    if (i > 1) and OverwriteOK then
      case i of
        2: JarCall('xvf ' + HideBlanks(Filename), path);
        3: JarOpen(Filename);
      end;
  except on e: exception do
    ErrorMsg(e.Message);
  end;
end;

procedure TFJava.MIJarClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var i:= (Sender as TSpTBXItem).Tag;
  with ODOpen do begin
    if assigned(ActiveTDIChild)
      then InitialDir:= ExtractFilePath(ActiveTDIChild.Pathname)
      else InitialDir:= FConfiguration.Sourcepath;
    case i of
      1: Title:= _('Show content of jar file');
      2: Title:= _('Unpack jar file');
      3: Title:= _('Open jar file');
    end;
    var fi:= FilterIndex;
    FilterIndex:= 9;
    Filename:= '*.jar';
    if Execute then begin
      FConfiguration.Sourcepath:= ExtractFilePath(FileName);
      JarShowUnpackOpen(i, Filename);
    end;
    FilterIndex:= fi;
  end;
  EnableUpdateMenuItems;
end;

{--- Test menu ----------------------------------------------------------------}

procedure TFJava.PrepareStep;
begin
  if assigned(myJavaCommands) and assigned(myJavaCommands.EditForm) then
    myJavaCommands.EditForm.DeleteDebuglineMark;
end;

procedure TFJava.StepNextUp(const s: string);
begin
  if FConfiguration.MindstormsMode then
    ShowMessage(_(LNGMindstormsPrograms))
  else if myDebugger.Running then begin
    PrepareStep;
    MyDebugger.NewCommand(3, s);
  end else begin
    if assigned(Editorform) and not EditorForm.hasBreakpoints then
      myDebugger.BreakPointAtMain:= True;
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
  StepNextUp('step up')
end;

procedure TFJava.MIBreakpointClick(Sender: TObject);
begin
  if assigned(EditorForm) then
    EditorForm.InsertBreakpoint;
end;

procedure TFJava.MIBreakpointsClearClick(Sender: TObject);
begin
  for var i:= 0 to TDIEditFormCount -1 do
    TDIEditFormGet(i).ClearBreakpoints;
end;

{--- Window menu -------------------------------------------------------------}

procedure TFJava.MIConfigurationClick(Sender: TObject);
begin
  // due to visible/modal-exception
  if not FConfiguration.Visible then begin
    FConfiguration.PrepareShow;
    FConfiguration.ShowModal;
    StyleTimer.Enabled:= true;
  end;
end;

procedure TFJava.StyleTimerTimer(Sender: TObject);
begin
  // timer using because of unclear exception
  StyleTimer.Enabled:= false;
  ChangeStyle(FConfiguration.GUIStyle);
end;

procedure TFJava.MIConfigureToolsClick(Sender: TObject);
begin
  with TFConfigureTools.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TFJava.MIAboutClick(Sender: TObject);
begin
  with TFAbout.Create(self) do begin
    ShowModal;
    Free;
  end;
end;

function TFJava.NewBrowser(Address: string; const state: string): TFBrowser;
begin
  if isHttp(Address) and FConfiguration.BlockedInternet then
    Address:= 'about:' + _(LNGBlockedInternet);
  if IsHttp(Address) then
    Address:= HttpToWeb(Address);
  if FConfiguration.OnlyOneBrowserWindow then begin
    Result:= (getTDIType('%B%') as TFBrowser);
    if Assigned(Result) then begin
      Result.WebBrowser.Navigate(Address);
      SwitchToWindow(Result.Number);
    end else
      Result:= TFBrowser(FormFactory(fkBrowser));
    Result.Open(Address, state);
  end else begin
    Result:= TFBrowser(getTDIWindowType(Address, '%B%'));
    if Assigned(Result) then
      Result.OpenWindow(Self)
    else begin
      Result:= TFBrowser(FormFactory(fkBrowser));
      Result.Open(Address, state);
    end;
  end;
end;

procedure TFJava.NewEditor(const path: string; state: string);
begin
  var Editor:= TFEditForm(getTDIWindowType(path, '%E%'));
  if assigned(Editor) then begin
    SetSelectedTabAndWindow(Editor.Pathname);
    myTabBarClick(Self);
    Editor.SetState(state);
  end else begin
    Editor:= TFEditForm(FormFactory(fkEditor));
    Editor.Open(path, state);
  end;
end;

procedure TFJava.NewExplorer(const dir: string; state: string);
begin
  var Explorer:= TFExplorer(getTDIWindowType(dir, '%X%'));
  if assigned(Explorer) then begin
    SetSelectedTabAndWindow(Explorer.Pathname);
    myTabBarClick(Self);
    Explorer.SetState(state);
  end else begin
    Explorer:= TFExplorer(FormFactory(fkExplorer));
    Explorer.New(withoutTrailingSlash(dir), state);
  end;
end;

procedure TFJava.NewTextDiff(F1, F2: TFEditForm);
begin
  var TextDiff:= TFTextDiff(FormFactory(fkTextDiff));
  TextDiff.New(F1, F2);
  SetSelectedTabAndWindow(TextDiff.Number);
end;

procedure TFJava.NewGUIForm(const aFile: string; state: string);
  var aForm: TFForm;
begin
  if WindowOpened(aFile, aForm) then
    aForm.OpenWindow(Self)
  else begin
    var s:= ChangeFileExt(aFile, '.java');
    if FileExists(s) then begin
      if not WindowOpened(s, aForm) then
        open(s);
      var GUIForm:= FGUIDesigner.Open(aFile, s);
      if assigned(GUIForm) then
        GUIForm.SetState(state);
    end else
      ErrorMsg(Format(_(LNGAssociatedJavaFileNotFound), [aFile]))
  end;
end;

procedure TFJava.NewUMLWindow(const aFile, state: string);
  var aForm: TFForm;
begin
  if WindowOpened(aFile, aForm) then
    aForm.OpenWindow(Self)
  else if FileExists(aFile) then
    OpenUMLWindow(aFile, state);
end;

function TFJava.MakeNewUMLWindow(const Filename, state: string): TFUMLForm;
begin
  DisableUpdateMenuItems;
  var UMLForm:= TFUMLForm(FormFactory(fkUML));
  UMLForm.Open(Filename, state);
  if Filename = _(LNGInteractive)
    then TDIFormsList.Remove(UMLForm)
    else UMLForm.Visible:= true;
  Result:= UMLForm;
  EnableUpdateMenuItems;
end;

function TFJava.OpenStructogram(const filename, state: string): boolean;
  var aForm: TFForm;
begin
  Result:= false;
  if WindowOpened(filename, aForm) then begin
    aForm.OpenWindow(Self);
    Result:= true;
  end else if FileExists(filename) then
    Result:= NewStructogram(filename, state);
end;

function TFJava.OpenSequenceDiagram(const Filename, state: string): boolean;
  var aForm: TFForm;
begin
  Result:= false;
  if WindowOpened(filename, aForm) then begin
    aForm.OpenWindow(Self);
    Result:= true;
  end else if FileExists(filename) then
    Result:= NewSequenceDiagram(filename, state);
end;

procedure TFJava.CallHelp(const address: string);
begin
  CallHelp(false, address);
end;

procedure TFJava.CallApplet(const address: string);
begin
  CallHelp(true, address);
end;

procedure TFJava.CallHelp(Applet: Boolean; address: string);
begin
  if isHttp(address) and FConfiguration.BlockedInternet then
    address:= 'about:' + _(LNGBlockedInternet);
  if IsCHM(address) then
    address:= FConfiguration.getCHMJavaManual(address);
    if not Applet and FConfiguration.UseIEinternForDocuments then begin
      NewBrowser(address, '');
      Exit;
    end;
  address:= toWeb(FConfiguration.BrowserProgram, address);
  var Window:= FWindow.GetWindowHandle(FConfiguration.BrowserTitle);
  if Window = 0 then
    if FileExists(FConfiguration.BrowserProgram)
      then myJavaCommands.ExecWithoutWait(FConfiguration.BrowserProgram, address, '', SW_ShowNormal)
    else if FConfiguration.BrowserProgram = ''
      then ErrorMsg(_('No browser configured'))
      else ErrorMsg(FConfiguration.BrowserProgram + ' ' + _(LNGDoesNotExist))
  else begin
    if IsIconic(Window)
      then ShowWindow(Window, SW_SHOWNORMAL)
      else SetForegroundWindow(Window);
    Sleep(100);
    SendShortCutStringGlobal(FConfiguration.BrowserOpenKeys);
    Sleep(100);
    SendKeysGlobal(address + #13);
  end;
end;

{--- Help menu ----------------------------------------------------------------}

procedure TFJava.MIHelpHelpClick(Sender: TObject);
begin
  if IsCHM(FConfiguration.JavaManual) then
    if FConfiguration.CHMRootOk
      then myJavaCommands.ShellExecuteFile(FConfiguration.JavaManual, '', '', SW_SHOWNORMAL)
      else ErrorMsg('cannot read: ' + FConfiguration.JavaManual)
  else if FConfiguration.GlobalFileExists(FConfiguration.JavaManual) then
    with TFHelpDialog.Create(Self) do
      Show
  else
    ErrorMsg(Format(_(LNGFileNotFound), [FConfiguration.JavaManual]));
end;

procedure TFJava.MIAPIClick(Sender: TObject);
begin
  if IsCHM(FConfiguration.JavaManual) and not FConfiguration.CHMRootOk then
    ErrorMsg('cannot read: ' + FConfiguration.JavaManual)
  else if FConfiguration.GlobalFileExists(FConfiguration.JavaManual) then
    if Assigned(EditorForm)
      then SearchInIndex
      else CallHelp(FConfiguration.JavaManual)
  else
    ErrorMsg(Format(_(LNGFileNotFound), [FConfiguration.JavaManual]));
end;

procedure TFJava.MIJavaFxClick(Sender: TObject);
begin
  CallHelp(FConfiguration.JavaManualFX)
end;

procedure TFJava.MIDemosClick(Sender: TObject);
begin
  CallHelp(FConfiguration.JavaDemos);
end;

procedure TFJava.MITutorialClick(Sender: TObject);
begin
  if FConfiguration.GlobalFileExists(FConfiguration.JavaTutorial) then
    if IsCHM(FConfiguration.JavaTutorial)
      then myJavaCommands.ShellExecuteFile(FConfiguration.JavaTutorial, '', '', SW_SHOWNORMAL)
      else CallHelp(FConfiguration.JavaTutorial)
  else
    InformationMsg(Format(_('The tutorial %s is not installed.'), [FConfiguration.JavaTutorial]));
end;

procedure TFJava.MIJavabookClick(Sender: TObject);
begin
  var aFile:= FConfiguration.Javabook;
  if FileExists(aFile) or IsHTTP(aFile)
    then CallHelp(aFile)
    else InformationMsg(_('The Java book from www.javabuch.de is not installed.'));
end;

procedure TFJava.MIMindstormsHelpClick(Sender: TObject);
begin
  var aFile:= FConfiguration.MindstormsManual;
  if FileExists(aFile) or IsHTTP(aFile)
    then CallHelp(FConfiguration.MindstormsManual)
    else InformationMsg(_('The mindstorms manual from https://lejos.sourceforge.net/ is not installed.'));
end;

{--- control structures -------------------------------------------------------}

procedure TFJava.SBKontrollstrukturenClick(Sender: TObject);
begin
  if EditorForm <> nil then
    FTemplates.SBControlStructures(EditorForm, TSpTBXItem(Sender).Tag)
end;

{--- Program ------------------------------------------------------------------}

function TFJava.NewEditform(Hidden: boolean): TFEditForm;
begin
  var Child:= TFEditForm(FormFactory(fkEditor));
  EditorSaveAs(Child, Hidden);
  if Child.Pathname = '' then begin // save as aborted?
    Child.Close;
    Result:= nil;
  end else begin
    Result:= Child;
    Child.New(Child.Pathname);
  end;
end;

procedure TFJava.SBProgramClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag in [1, 2])
    then FConfiguration.Sourcepath:= ExtractFilePath(ActiveTDIChild.Pathname);
  var JavaFormular:= NewEditform(false);
  if Assigned(JavaFormular) then
    FTemplates.SBProgram(JavaFormular);
  EnableUpdateMenuItems
end;

procedure TFJava.SBGUIFrameClick(Sender: TObject);
  var JavaFormular: TFEditForm;
      GUIForm: TFGUIForm;
      UMLFenster: TFUMLForm;
      Filename: string;
      WH: TPoint;
begin
  DisableUpdateMenuItems;
  if (TComponent(Sender).Tag = 8) and (FConfiguration.getJavaVersion < 8) then begin
    ErrorMsg(_('JavaFX needs Java JDK 8 or higher'));
    exit;
  end;

  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag in [1, 2]) then
    FConfiguration.Sourcepath:= ExtractFilePath(ActiveTDIChild.Pathname);
  JavaFormular:= NewEditform(false);
  if Assigned(JavaFormular) then begin
    FTemplates.FrameDialogApplet(JavaFormular, TComponent(Sender).Tag);
    WH:= JavaFormular.getWidthAndHeight;
    UMLFenster:= getUMLWindow;
    if Assigned(UMLFenster) then
      UMLFenster.MainModul.AddToProject(JavaFormular.Pathname);
    Filename:= ChangeFileExt(JavaFormular.Pathname, '.jfm');
    if TComponent(Sender).Tag = 8
      then GUIForm:= TFXGUIForm(FormFactory(fkFXGUI))
      else GUIForm:= TFGUIForm(FormFactory(fkGUI));
    with GUIForm do
      setBounds(Left, Top, PPIScale(WH.X), PPIScale(WH.Y));
    GUIForm.Open(Filename, '');
    DoSave(JavaFormular, WithoutBackup);
  end;
  EnableUpdateMenuItems;
end;

function TFJava.GuiDesignerOpen: boolean;
begin
  Result:= true;
  ActiveTDIChild:= getActiveForm;
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 1) and
     assigned((ActiveTDIChild as TFEditForm).Partner) then exit;

  EditorForm:= getActiveEditor;
  if (EditorForm = nil) and assigned(FGUIDesigner.DesignForm) then
    EditorForm:= TFEditForm(FGUIDesigner.DesignForm.Partner);
  if assigned(EditorForm) and EditorForm.isJava then begin
    if not assigned(EditorForm.Partner) then begin
      EditorForm.SBDesignformClick(nil);
      if not assigned(EditorForm.Partner) then begin
        ErrorMsg(Format(_(LNGFileNotFound),
                [ChangeFileExt(EditorForm.Pathname, '.jfm')]));
        Result:= false;
      end
    end
  end else
    Result:= false;
end;

{--- AWT-Swing-Toolbar --------------------------------------------------------}

procedure TFJava.MISwingClick(Sender: TObject);
begin
  if GuiDesignerOpen then
    FGUIDesigner.MIAWTSwing(TSpTBXItem(Sender).Tag)
end;

procedure TFJava.TBSwingClick(Sender: TObject);
begin
  if GuiDesignerOpen then
    FGUIDesigner.TBAWTSwing(TToolButton(Sender).Tag);
end;

procedure TFJava.TBJavaFXClick(Sender: TObject);
begin
  if GuiDesignerOpen then begin
    FGUIDesigner.TBJavaFX(TToolButton(Sender).Tag);
    if not FObjectInspector.Visible then
      FObjectInspector.ShowIt;
  end;
end;

procedure TFJava.ToolbuttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if assigned(FGuiDesigner.DesignForm) and (Button = mbLeft) then
    (Sender as TToolButton).BeginDrag(false, 10);
end;

type
  TControlEx = class(TControl)
  protected
    FFont: TFont;
  end;

procedure TFJava.ToolbuttonStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  var ControlClass:= FGUIDesigner.Tag2Class((Sender as TToolButton).Tag);
  var DragRectangle:= ControlClass.Create(FGUIDesigner.DesignForm);
  DragRectangle.Parent:= FGuiDesigner;
  DragRectangle.ScaleForPPI(FGUIDesigner.DesignForm.PixelsPerInch);
  TControlEx(DragRectangle).Font.Size:= FGUIDesigner.DesignForm.FontSize;
  DragObject:= TMyDragObject.Create(DragRectangle);
end;

procedure TFJava.ResetToolbars;
  var i: integer;
begin
  for i:= 0 to ToolbarAWT.ButtonCount - 1 do
    ToolbarAWT.Buttons[i].Down:= false;
  for i:= 0 to ToolbarSwing1.ButtonCount - 1 do
    ToolbarSwing1.Buttons[i].Down:= false;
  for i:= 0 to ToolbarSwing2.ButtonCount - 1 do
    ToolbarSwing2.Buttons[i].Down:= false;
  for i:= 0 to ToolbarLayout.ButtonCount - 1 do
    ToolbarLayout.Buttons[i].Down:= false;
  for i:= 0 to ToolbarUtilities.ButtonCount - 1 do
    ToolbarUtilities.Buttons[i].Down:= false;
  for i:= 0 to ToolbarFXBase.ButtonCount - 1 do
    ToolbarFXBase.Buttons[i].Down:= false;
  for i:= 0 to ToolbarFXControls.ButtonCount - 1 do
    ToolbarFXControls.Buttons[i].Down:= false;
  for i:= 0 to ToolbarFXShapes.ButtonCount - 1 do
    ToolbarFXShapes.Buttons[i].Down:= false;
end;

procedure TFJava.TBPanelCanvasMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    var TagNr:= TToolButton(Sender).Tag;
    with ODOpen do begin
      if assigned(ActiveTDIChild)
        then InitialDir:= ExtractFilePath(ActiveTDIChild.Pathname)
        else InitialDir:= FConfiguration.Sourcepath;
      if Abs(TagNr) = 12
        then Title:= _(LNGOpenSubClass) + ' Panel'
        else Title:= _(LNGOpenSubClass) + ' Canvas';
      filename:= '*.java;*.class';
      FilterIndex:= 1;
      case TagNr of
        -12: TagNr:= -38;
         12: TagNr:=  38;
        -13: TagNr:= -39;
        114: TagNr:= 122;
      end;
      if Execute then
        FGUIDesigner.DoPanelCanvas(TagNr, ExtractFileName(filename));
    end;
  end;
end;

{--- Layout bar ------------------------------------------------------------}

procedure TFJava.TBLayoutClick(Sender: TObject);

  procedure TBLayout(aTag: integer);
  begin
    var s:= '';
    case aTag of
      0: s:= 'Border';
      1: s:= 'Flow';
      2: s:= 'Grid';
      3: s:= 'Card';
      4: s:= 'GridBag';
      5: s:= 'null';
    end;
    if aTag < 5
      then s:= 'setLayout(new ' + s + 'Layout());'
      else s:= 'setLayout(null);';
    with EditorForm do begin
      s:= s + CrLf + getIndent + '|';
      PutText(s);
    end;
  end;

begin
  if Assigned(EditorForm) then
    if (Sender is TSpeedButton)
      then TBLayout(TSpeedButton(Sender).Tag)
      else TBLayout(TSpTBXItem(Sender).Tag)
end;

procedure TFJava.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;
  DroppedFileCount: Integer;
  FileNameLength: Integer;
  filename: string;
  I: Integer;
begin
  inherited;
  LockFormUpdate(Self);
  // Store drop handle from the message
  DropH:= Msg.Drop;
  try
    DroppedFileCount:= DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    for i:= 0 to Pred(DroppedFileCount) do begin
      FileNameLength:= DragQueryFile(DropH, I, nil, 0);
      SetLength(filename, FileNameLength);
      DragQueryFile(DropH, I, PChar(filename), FileNameLength + 1);
      if Open(filename) then begin
        RearrangeFileHistory(filename);
        FConfiguration.Sourcepath:= ExtractFilepath(filename);
      end;
    end;
  finally
    DragFinish(DropH);
  end;
  Msg.Result:= 0;
  UnLockFormUpdate(Self);
end;

procedure TFJava.SystemExecuteMacro(Sender: TObject; Msg: TStrings);
  var filename: string;
begin
  // loads selected files from windows explorer
  // onExecuteMacro of System: TDdeServerConv component
  LockFormUpdate(Self);
  for var i:= 0 to Msg.Count - 1 do begin
    filename:= Msg.Strings[i];
    if LeftStr(filename, 11) = '[FileOpen("' then begin
      filename:= copy(filename, 12, length(filename) - 14);
      if Open(filename) then begin
        RearrangeFileHistory(filename);
        FConfiguration.Sourcepath:= ExtractFilepath(filename)
      end;
    end;
  end;
  UnLockFormUpdate(Self);
end;

procedure TFJava.MIBrowserClick(Sender: TObject);
begin
  CallHelp('about:blank');
end;

procedure TFJava.myTabBarClick(Sender: TObject);
begin
  SwitchToWindow(getSelectedTabForm);
end;

procedure TFJava.myTabBarDblClick(Sender: TObject);
begin
  var Form:= getSelectedTabForm;
  if assigned(Form) then begin
    if Assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag <> 3) then
      FConfiguration.WindowStateMaximized:= not FConfiguration.WindowStateMaximized;
    SwitchToWindow(Form);
  end;
end;

procedure TFJava.myTabBarClosed(Sender: TObject; Item: TJvTabBarItem);
begin
  DisableUpdateMenuItems;
  TTabObject(Item.Data).Form.Close;
  myTabBarClick(Self);
  EnableUpdateMenuItems;
end;

procedure TFJava.MIExpressionClick(Sender: TObject);
begin
  FEvaluate:= TFEvaluate.Create(Self);
  FEvaluate.Show;
end;

procedure TFJava.RunButtonToStop(Stop: Boolean);
begin
  if Stop then begin
    SetEnabledMI(MIRun, false);
    SetEnabledMI(MIProgramReset, true);
    TBRun.Hint:= _(LNGResetProgram);
    TBRun.ImageIndex:= 14;
  end else begin
    SetEnabledMI(MIRun, true);
    SetEnabledMI(MIProgramReset, false);
    if assigned(FConfiguration) and FConfiguration.AndroidMode then begin
      MIRun.ImageIndex:= 87;
      MIRun.Caption:= _(LNGTransferToAndroid);
      TBRun.Hint:= _(LNGTransferToAndroid);
      TBRun.ImageIndex:= 15;
    end else begin
      MIRun.ImageIndex:= 22;
      MIRun.Caption:= _(LNGrunApplication);
      TBRun.Hint:= _('Run');
      TBRun.ImageIndex:= 13;
    end;
  end;
  UpdateMenuItems(nil);
end;

procedure TFJava.CompileButtonToStop(OnOff: Boolean);
begin
  if OnOff then begin
    MICompile.Enabled:= False;
    MIProgramReset.Enabled:= True;
    TBCompileJava.Hint:= _(LNGResetProgram);
    TBCompileJava.ImageIndex:= 14;
  end else begin
    MICompile.Enabled:= True;
    MIProgramReset.Enabled:= False;
    if FConfiguration.MindstormsMode
      then TBCompileJava.Hint:= _(LNGCompileForMindstorms)
      else TBCompileJava.Hint:= _(LNGCompileWithJava);
    TBCompileJava.ImageIndex:= 11;
  end;
  UpdateMenuItems(nil);
end;

{---Docking -------------------------------------------------------------------}

procedure TFJava.RightDockPanelUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  if assigned(FConfiguration) then begin
    FConfiguration.RightDockPanelWidth:= RightDockPanel.Width;
    //OnUnDock gets called BEFORE the client is undocked, in order to optionally
    //disallow the undock. DockClientCount is never 0 when called from this event.
    if (Sender as TPanel).DockClientCount = 1 then
      ShowDockPanel(Sender as TPanel, false, nil);
  end;
end;

procedure TFJava.BottomDockPanelUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  // When undocking in ShellFolder view, the window
  // is incorrectly positioned
  if FMessages.TabControlMessages.TabIndex = 0 then begin
    FMessages.Undocking:= true;
    FMessages.ShowTab(K_Compiler);
  end;

  FConfiguration.BottomDockPanelHeight:= BottomDockPanel.Height;
  //OnUnDock gets called BEFORE the client is undocked, in order to optionally
  //disallow the undock. DockClientCount is never 0 when called from this event.
  if (Sender as TPanel).DockClientCount = 1 then
    ShowDockPanel(Sender as TPanel, false, nil);
end;

{--- preview DockPanel dock area ----------------------------------------------}

procedure TFJava.BottomDockPanelDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; state: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept:= Source.Control is TFMessages;
  if Accept then begin
    // Modify the DockRect to preview dock area.
    //ARect.TopLeft := StatusBar.ClientToScreen(Point(0, -FConfiguration.BottomDockPanelHeight));
    //ARect.BottomRight := StatusBar.ClientToScreen(Point(StatusBar.Width, 0));
// TODO without StatusBAR????????
    ARect.TopLeft := ClientToScreen(Point(0, -FConfiguration.BottomDockPanelHeight));
    ARect.BottomRight := ClientToScreen(Point(HSplitter.Width, 0));

    //ARect.TopLeft := BottomDockPanel.ClientToScreen( Point(0, -Self.ClientHeight div 3));
    //ARect.BottomRight := BottomDockPanel.ClientToScreen( Point(BottomDockPanel.Width, BottomDockPanel.Height));
    Source.DockRect := ARect;
  end;
end;

procedure TFJava.BottomDockPanelStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  DragObject:= nil;
end;

procedure TFJava.RightDockPanelDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; state: TDragState; var Accept: Boolean);
var
  ARect: TRect; myHeight: integer;
begin
  Accept:= (Source.Control is TFFileStructure) or (Source.Control is TFObjectInspector);
  if Accept then begin
    // Modify the DockRect to preview dock area.
    ARect.TopLeft := ClientToScreen(Point(ClientWidth - FConfiguration.RightDockPanelWidth, ControlBar.Height + myTabBar.Height));
    myHeight:= ClientHeight;
    if FMessages.myIsVisible and not FMessages.Floating then
      myHeight:= myHeight - BottomDockPanel.Height;
    ARect.BottomRight := ClientToScreen(Point(ClientWidth, myHeight));
    Source.DockRect := ARect;
  end;
end;

procedure TFJava.RightDockPanelGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  //if CanDock is true, the panel will not automatically draw the preview rect.
  CanDock := DockClient is TDockableForm;
end;

procedure TFJava.BottomDockPanelGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  //if CanDock is true, the panel will not automatically draw the preview rect.
  CanDock := DockClient is TFMessages;
end;

{--- Drop ---------------------------------------------------------------------}

procedure TFJava.DockPanelDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  //OnDockDrop gets called AFTER the client has actually docked,
  //so we check for DockClientCount = 1 before making the dock panel visible.
  if (Sender as TPanel).DockClientCount = 1 then begin
    ShowDockPanel(Sender as TPanel, true, nil);
    //Make DockManager repaints it's clients.
    (Sender as TPanel).DockManager.ResetBounds(True);
  end;
end;

procedure TFJava.ShowDockPanel(APanel: TPanel; MakeVisible: Boolean; Client: TControl);
begin
  //Client - the docked client to show if we are re-showing the panel.
  //Client is ignored if hiding the panel.

  //Since docking to a non-visible docksite isn't allowed, instead of setting
  //Visible for the panels we set the width to zero. The default InfluenceRect
  //for a control extends a few pixels beyond it's boundaries, so it is possible
  //to dock to zero width controls.

  //Don't try to hide a panel which has visible dock clients.
  if not MakeVisible and (APanel.VisibleDockClientCount > 1) or (FConfiguration = nil) then
    Exit;

  if APanel = RightDockPanel
    then VSplitter.Visible:= MakeVisible
    else HSplitter.Visible:= MakeVisible;

  if MakeVisible then
    if APanel = RightDockPanel then begin
      APanel.Visible:= true;
      APanel.Width := FConfiguration.RightDockPanelWidth;
      APanel.Constraints.MinWidth:= 50;
      APanel.Align:= alRight;
      VSplitter.Left:= 100;
      APanel.Left:= 0;
    end else begin // BottomDockPanel
      APanel.Visible:= true;
      APanel.Height := FConfiguration.BottomDockPanelHeight;
      APanel.Constraints.MinHeight:= 100;
      APanel.Align:= alBottom;
      APanel.Top:= 100;
      HSplitter.Top:= 0;
    end
    else
    if APanel = RightDockPanel then begin
      APanel.Constraints.MinWidth:= 0;
      APanel.Width:= 0
    end
    else begin
      APanel.Constraints.MinHeight:= 0;
      APanel.Height:= 0;
    end;
  if MakeVisible and (Client <> nil) then
    Client.Show;
end;

procedure TFJava.ShowDockableForm(DockWindow: TDockableForm);
begin
  //if the docked window is TabDocked, it is docked to the PageControl
  //(owned by TTabDockHost) so show the host form.
  if DockWindow.HostDockSite is TPageControl then
    TTabDockHost(DockWindow.HostDockSite.Owner).Show
  else
  //If window is conjoin-docked, host and/or form may not be visible
  //so show both.
  if (DockWindow.HostDockSite is TConjoinDockHost) and not
    DockWindow.HostDockSite.Visible then
  begin
    DockWindow.HostDockSite.Show;
    TConjoinDockHost(DockWindow.HostDockSite).UpdateCaption(nil);
    DockWindow.Show;
  end
  else
  //If form is docked to one of the "hidden" docking panels, resize the
  //panel and re-show the docked form.
  if (DockWindow.HostDockSite is TPanel) and
    ((DockWindow.HostDockSite.Height = 0) or (DockWindow.HostDockSite.Width = 0)) then
    ShowDockPanel(DockWindow.HostDockSite as TPanel, True, DockWindow)
  else
    //if the window isn't docked at all, simply show it.
    DockWindow.Show;
  VSplitterMoved(Self);
end;

procedure TFJava.MIMessagesClick(Sender: TObject);
begin
  FMessages.ChangeHideShow;
end;

procedure TFJava.AddToTabBar(Nr: integer; Form: TFForm);
begin
  var s:= ExtractFilenameEx(Form.Pathname);
  if IsWriteProtected(Form.Pathname) then
    s:= s + ' (' + _(LNGWriteProtected) + ')';
  var aTabBarItem:= myTabBar.AddTab(s);
  aTabBarItem.Data:= TTabObject.create(Form.Pathname, Nr, Form);
  aTabBarItem.Selected:= true;
end;

procedure TFJava.AddToWindowMenuAndTabBar(Nr: integer;
                 NotifyEvent: TNotifyEvent; Form: TFForm);
begin
  var NewItem:= TSpTBXItem.Create(Self);
  NewItem.Caption:= '&' + IntToStr(myTabBar.Tabs.Count + 1) + ' ' + Form.Pathname;
  NewItem.OnClick:= NotifyEvent;
  NewItem.Checked:= true;
  NewItem.Tag:= Nr;
  MIWindow.Add(NewItem);
  AddToTabBar(Nr, Form);
  myTabBarColorize;
end;

procedure TFJava.DeleteTab(Nr: integer);
begin
  var i:= getTab(Nr);
  if i > -1 then begin
    var aTabObject:= TTabObject(myTabBar.Tabs[i].Data);
    FreeAndNil(aTabObject);
    myTabBar.Tabs.Delete(i);
  end;
end;

procedure TFJava.DeleteTabAndWindow(Nr: integer);
  var i, j: integer; s: string;
begin
  i:= getWindowMenuNr(Nr);
  if i > -1 then begin
    for j:= i + 1 to MIWindow.Count - 1 do begin
      s:= MIWindow.Items[j].Caption;
      s:= '&' + IntToStr(j-WindowOffset) + Copy(s, Pos(' ', s), Length(s));
      MIWindow.Items[j].Caption:= s;
    end;
    MIWindow.Delete(i);
  end;
  DeleteTab(Nr);
  myTabBarColorize;
end;

function TFJava.getWindowMenuNr(Nr: integer): integer;
begin
  var i:= WindowOffset - 1;
  while (i < MIWindow.Count-1) and (MIWindow.Items[i].Tag <> Nr) do
    inc(i);
  if (i < MIWindow.Count) and (MIWindow.Items[i].Tag = Nr)
    then Result:= i
    else Result:= -1;
end;

procedure TFJava.RenameTabAndWindow(Nr: integer; const Neu: string);
begin
  var i:= getWindowMenuNr(Nr);
  if i > -1 then
    MIWindow.Items[i].Caption:= '&' + IntToStr(i-WindowOffset-1) + ' ' + Neu;
  i:= getTab(Nr);
  if i > -1 then begin
    TTabObject(myTabBar.Tabs[i].Data).Path:= Neu;
    myTabBar.Tabs[i].Caption:= ExtractFileNameEx(Neu);
    myTabBar.Update;
  end;
end;

procedure TFJava.FormResize(Sender: TObject);
  var i: Integer; aForm: TFForm;
begin
  if assigned(myTabBar) then
    for i:= 0 to myTabBar.Tabs.Count - 1 do begin
      aForm:= getTabForm(i);
      if not aForm.Visible then continue;
      if (aForm.FormTag = 2) and Assigned((aForm as TFUMLForm).MainModul) then
        (aForm as TFUMLForm).MainModul.Diagram.RecalcPanelSize;
    end;
  UpdateLayoutRightDockPanel(false);
end;

procedure TFJava.MICommentOnOffClick(Sender: TObject);
begin
  EditorForm.SBCommentClick(Self);
end;

procedure TFJava.MIWebsiteClick(Sender: TObject);
  var s: string;
begin
  if FConfiguration.isGerman
    then s:= Homepage + '/doku.php?id=de:java-editor'
    else s:= Homepage + '/doku.php?id=start';
  CallHelp(false, s);
end;

procedure TFJava.MIChatClick(Sender: TObject);
begin
  FLLMChatForm.Show;
end;

procedure TFJava.MICheckstyleClick(Sender: TObject);
  var dir, aFile, call, ErrFile: string;
begin
  aFile:= ExtractFilename(EditorForm.Pathname);
  dir:= ExtractFilePath(EditorForm.Pathname);
  FMessages.ShowIt;
  FMessages.DeleteTab(K_Messages);
  FMessages.ShowTab(K_Messages);
  try
    Screen.Cursor:= crHourGlass;
    ErrFile:= FConfiguration.TempDir + 'error.txt';
    if EditorForm.Modified then DoSave(EditorForm, FConfiguration.CreateBAKFiles);
    call:= ' -cp ' + HideBlanks(FConfiguration.Checkstyle) + ' ' + FConfiguration.JavaInterpreterParameter +
             ' com.puppycrawl.tools.checkstyle.Main' +
             ' -c ' + HideBlanks(FConfiguration.CheckKonfiguration) + ' ' +
             FConfiguration.CheckParameter + ' ' + HideBlanks(aFile);
    FMessages.OutputLineTo(K_Messages, 'Checkstyle ' + _('for') + ' ' + EditorForm.Pathname);
    if myJavaCommands.ExecAndWait(FConfiguration.JavaInterpreter, call, dir, ErrFile, SW_Hide) then
      if GetFileSize(ErrFile) = 0
        then FMessages.OutputLineTo(K_Messages, 'Checkstyle ' + _(LNGSuccessfullyTerminated))
        else FMessages.ShowMessages(ErrFile);
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFJava.MIJalopyClick(Sender: TObject);
  var dir, aFile, call, Parameter: string;
      i: integer; Age: boolean;

  function ExpandJalopyJar(const s: string): string;
    var s1, s2: string; sr: TSearchRec;
  begin
    s2:= '';
    s1:= ExtractFilePath(s);
    FindFirst(s1 + '*.jar', 0, sr);
    repeat
      s2:= s2 + ';' + s1 + sr.Name;
    until FindNext(sr) <> 0;
    FindClose(sr);
    Result:= copy(s2, 2, length(s2));
  end;

begin
  if EditorForm = nil then exit;
  aFile:= ExtractFilename(EditorForm.Pathname);
  dir:= ExtractFilePath(EditorForm.Pathname);
  FMessages.ShowIt;
  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  Age:= EditorForm.CheckAgeEnabled;
  try
    Screen.Cursor:= crHourGlass;
    DoSave(EditorForm, FConfiguration.CreateBAKFiles);
    call:= ' -cp ' + HideBlanks(ExpandJalopyJar(FConfiguration.Jalopy)) + ' ' +
             FConfiguration.JavaInterpreterParameter +
             ' de.hunsicker.jalopy.plugin.console.ConsolePlugin';
    if trim(FConfiguration.JalopyConfiguration) <> '' then
      call := call + ' -c ' + HideBlanks(FConfiguration.JalopyConfiguration);
    Parameter:= FConfiguration.JalopyParameter;
    if (Pos('Unicode', EditorForm.Encoding) > 0) and (Pos('-e', Parameter) = 0) then
      Parameter:= Parameter + ' -e unicode';
    call:= call + ' ' + Parameter + ' ' + HideBlanks(aFile);

    FMessages.OutputLineTo(K_Messages, 'Jalopy ' + _('for') + ' ' + EditorForm.Pathname);
    EditorForm.CheckAgeEnabled:= false;
    if myJavaCommands.ExecAndWait(FConfiguration.JavaInterpreter, call, dir, FConfiguration.TempDir + 'error.txt', SW_Hide) then begin
      ProcessTerminate('JAVA.EXE');
      FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
      i:= 0;
      while i < FMessages.LBMessages.Items.Count do begin
        if Pos('[ERROR]', FMessages.LBMessages.Items[i]) = 1 then
          break;
        inc(i);
      end;
      if i = FMessages.LBMessages.Items.Count then begin
        FMessages.OutputLineTo(K_Messages, 'Jalopy ' + _(LNGSuccessfullyTerminated));
        EditorForm.Editor.Lines.LoadFromFile(EditorForm.PathName);
        FileAge(EditorForm.PathName, EditorForm.EditorAge);
        end
      else begin
        EditorForm.Save(false);
        FMessages.OutputLineTo(K_Messages, FMessages.LBMessages.Items[i]);
      end;
     end;
  finally
    Screen.Cursor:= crDefault;
    EditorForm.CheckAgeEnabled:= Age;
  end;
end;

procedure TFJava.MIMSDosClick(Sender: TObject);
  var path: string;
begin
  if Assigned(ActiveTDIChild)
    then path:= ExtractFilePath(ActiveTDIChild.Pathname)
    else path:= FConfiguration.Sourcepath;
  if Win32Platform >= VER_PLATFORM_WIN32_NT
    then myJavaCommands.ExecWithoutWait('cmd.exe', '',  path, SW_ShowNormal)
    else myJavaCommands.ExecWithoutWait('command.com', '', path, SW_ShowNormal);
end;

procedure TFJava.MIExplorerClick(Sender: TObject);
begin
  var dir:= '';
  if Assigned(ActiveTDIChild) then begin
    var aFile := ActiveTDIChild.Pathname;
    if ExtractFileExt(aFile) = ''
      then dir:= aFile
      else dir:= ExtractFilePath(aFile);
  end;
  if (dir = '') or not SysUtils.DirectoryExists(dir) then
    dir:= FConfiguration.Sourcepath;
  if not SysUtils.DirectoryExists(dir) then dir:= GetDocumentsPath;
  NewExplorer(dir, '');
end;

procedure TFJava.TBMsDosClick(Sender: TObject);
begin
  MIMSDosClick(Sender);
end;

function TFJava.OpenUMLWindow(const filename, state: string): TFUMLForm;
  var UMLForm: TFUMLForm; aForm: TFForm;
begin
  aForm:= getTDIWindow(filename);
  if assigned(aForm) then begin
    aForm.OpenWindow(Self);
    UMLForm:= (aForm as TFUMLForm);
  end else begin
    UMLForm:= MakeNewUMLWindow(filename, state);
    if FileExists(filename) then begin
      UMLForm.MainModul.LoadUML(filename);
      UMLForm.CreateTVFileStructure;
    end else
      UMLForm.ConfigureWindow(Self);
  end;
  ActiveTDIChild:= UMLForm;
//  UMLForm.TBShowConnections.Down:= (UMLForm.MainModul.Diagram.ShowConnections <> 0);
  if not FMessages.Floating then begin
    MoveHSplitter(BottomDockPanel.Height+1);
    MoveHSplitter(BottomDockPanel.Height-1);
  end;
  UpdateMenuItems(Self);
  Result:= UMLForm;
end;

function TFJava.NewStructogram(const filename: string; state: string): boolean;
begin
  var Structogram:= TFStructogram(FormFactory(fkStructogram));
  Result:= Structogram.open(filename);
  if Result
    then Structogram.setState(state)
    else Structogram.Close;
end;

function TFJava.NewSequenceDiagram(const filename: string;  state: string): boolean;
begin
  var SequenceDiagram:= TFSequenceForm(FormFactory(fkSequence));
  Result:= SequenceDiagram.open(filename);
  if Result
    then SequenceDiagram.setState(state)
    else SequenceDiagram.Close;
end;

function TFJava.getFilename(const Extension: string; path: string = ''): string;
  var s: string; i: integer; aForm: TFForm;
begin
  if path = '' then begin
    for i:= 0 to TDIFormsList.Count - 1 do
      if TDIFormsList[i].FormTag in [1, 2, 3, 11] then begin
        path:= ExtractFilepath(TDIFormsList[i].Pathname);
        break;
      end;
    if path = '' then
      path:= FConfiguration.Sourcepath;
  end;
  path:= withTrailingSlash(path);
  i:= 1;
  s:= path + _(LNGFile) + IntToStr(i) + Extension;
  while WindowOpened(s, aForm) or FileExists(s) do begin
    inc(i);
    s:= path + _(LNGFile) + IntToStr(i) + Extension;
  end;
  Result:= s;
end;

// Check: Should only be active in the menu if the UML window is open.
procedure TFJava.MISaveAsPictureClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then
    (ActiveTDIChild as TFUMLForm).MainModul.SaveDiagramActionExecute(Self);
end;

procedure TFJava.MIOpenFolderClick(Sender: TObject);
begin
  MakeNewUMLWindow(getFilename('.uml'), '').OpenFolder;
end;

procedure TFJava.MINewLayoutClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then
    (ActiveTDIChild as TFUMLForm).MainModul.DoLayout;
end;

procedure TFJava.MINewSequencediagramClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var Child:= TFSequenceForm(FormFactory(fkSequence));
  MISaveAsClick(Self);
  if Assigned(Child) and (Child.Pathname = '') then   // save as aborted?
    Child.Close
  else
    Child.New(Child.Pathname);
  EnableUpdateMenuItems
end;

procedure TFJava.MIDiagramFromOpenFilesClick(Sender: TObject);
  var i, mr: integer; filename, path: string; DlgMessage: TDlgMessage;
      SL: TStringList;
begin
  if not (hasJavaFiles or hasPascalFiles) then exit;
  DisableUpdateMenuItems;
  SL:= TStringList.Create;
  try
    SL.Sorted:= true;
    path:= '';
    for i:= 0 to TDIEditFormCount - 1 do begin
      filename:= TDIEditFormGet(i).Pathname;
      if hasJavaExtension(filename) or hasPascalExtension(filename) then begin
        if path = '' then path:= ExtractFilePath(filename);
        SL.Add(ChangeFileExt(ExtractFilename(filename), ''));
      end;
    end;
    filename:= path;
    for i:= 0 to SL.Count - 1 do
      filename:= filename + SL.Strings[i];
    filename:= filename + '.uml';

    mr:= mrNone;
    if FileExists(filename) then begin
      DlgMessage:= TDlgMessage.create(Self);
      try
        DlgMessage.setMessage(filename);
        mr:= DlgMessage.ShowModal;
        if mr = mrNo then
          Open(filename);
      finally
        FreeAndNil(DlgMessage);
      end;
    end;
    if mr = mrYes then begin
      CloseFile(filename);
      Application.ProcessMessages;
    end;
    if mr in [mrYes, mrNone] then
      MakeNewUMLWindow(filename, '').OpenFiles;
  finally
    FreeAndNil(SL);
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIWatchesClick(Sender: TObject);
begin
  FWatches.ActiveControl:= FWatches.EWatch;
  FWatches.Show;
end;

procedure TFJava.SearchInIndex;
  var sought, HTMLfile: string;
      s, Objekt, Typ, link, path, MethodAttribute: string;
      x, line, p: integer;
      SL: TStringList;
      EditForm: TFEditForm;

  function SearchString(sought: string; var HTMLfile: string): boolean;
    var i1, i2, p: Integer; s: string;
  begin
    sought:= '"><B>' + sought;  // <A HREF="..."><B>Sought   </A>
    s:= SL.Text;
    p:= Pos(sought, s);
    if p > 0 then begin
      i1:= p + Length(sought);
      if IsAlpha(s[i1]) then
        p:= 0
      else begin
        i2:= p - 1;
        i1:= p - 2;
        while (i1 > 0) and (s[i1] <> '"') do
          dec(i1);
        HTMLfile:= FConfiguration.getJavaManual + '\api' + copy(s, i1+3, i2-i1-2);
      end;
    end;
    Result:= (p > 0);
  end;

  procedure SearchIndex(const sought: string);
    var IndexFile, s: string;
  begin
    if sought = '' then exit;
    var i:= Ord(sought[1]) and $1F; // map upper-lowercase to 1 to 26
    if i = 31 then i:= 27; // '_' = 27
    IndexFile:= FConfiguration.getJavaManual + '\api\index-files\index-' +
                  IntToStr(i) + '.html';
    if FConfiguration.GlobalFileExists(IndexFile) then begin
      SL:= TStringList.Create;
      SL.Text:= MyCodeCompletion.LoadTextFromFile(IndexFile);
      if SearchString(sought, HTMLfile) then
        if not SearchString(sought, s) then
          CallHelp(HTMLfile)
        else begin
          Search:= True;
          CallHelp(IndexFile);
        end
      else
        CallHelp(FConfiguration.JavaManual);
      FreeAndNil(SL);
    end else
      ErrorMsg(Format(_(LNGFileNotFound), [IndexFile]));
  end;

begin  // SearchInIndex
  // EditorForm <> nil;
  MyCodeCompletion.ParseSourceCodes;

  with EditorForm do begin
    sought:= Editor.WordAtCursor;
    x:= Editor.CaretX;
    Editor.CommandProcessor(ecWordRight, #0, nil);
    Editor.CommandProcessor(ecWordLeft, #0, nil);
    s:= Copy(Editor.LineText, 1, Editor.CaretX + Length(sought) - 1);
    Editor.CaretX:= x;
    Objekt:= MyCodeCompletion.getObjectFrom(s);
    p:= FConfiguration.ImportCache.IndexOfName(Objekt);
    if p > -1
      then Typ:= FConfiguration.ImportCache.ValueFromIndex[p]
      else Typ:= MyCodeCompletion.getToTypeOfObject(Objekt, MethodAttribute, Editor.CaretY);
    if Typ = '' then
      Typ:= MyCodeCompletion.getToTypeOfObject('this.' + Objekt, MethodAttribute, Editor.CaretY);
    if MyCodeCompletion.isJavaAPIClass(Typ) or
       MyCodeCompletion.isJavaAPIClass(Objekt, Typ)
    then begin
      CallHelp(MyCodeCompletion.HTMLFileOfJavaClass);
      exit;
    end;
  end;

  // work is needed here
  if  (Typ = '') or IsSimpleType(Typ) or isSimpleType(WithoutArray(Typ))or (Typ = 'void') then
    if EditorForm.Frametype = 8
      then CallHelp(FConfiguration.JavaManualFX)
      else CallHelp(FConfiguration.JavaManual)
  else if MyCodeCompletion.IsSelfDefinedClassOrInterface(Typ, path, EditForm) then begin
    line:= MyCodeCompletion.getLine(Typ, MethodAttribute);
    if line = - 1 then begin  // inherited method
      if MyCodeCompletion.isJavaAPIClass(Typ) then begin
        link:= MyCodeCompletion.getAPIReference(MethodAttribute);
        CallHelp(link);
      end;
    end
    else begin
      if path = '' then
        SwitchToWindow(GetPathnameForClass(Typ))
      else if path = EditorForm.Pathname then
        CallHelp(FConfiguration.JavaManual)
      else begin
        SwitchWindowWithSearch(path);
        EditorForm.Editor.CaretY:= line;
        EditorForm.Editor.EnsureCursorPosVisible;
      end;
    end;
  end else if MyCodeCompletion.isJavaAPIClass(Typ) then begin
    link:= MyCodeCompletion.getAPIReference(MethodAttribute);
    CallHelp(link);
  end else
    SearchIndex(sought);
end;

procedure TFJava.MIToolbarClick(Sender: TObject);
begin
  ControlBar.Visible:= not ControlBar.Visible
end;

procedure TFJava.MIUpdateClick(Sender: TObject);
begin
  with TFUpdateDialog.Create(Self) do begin
    EOldVersion.Text:= UDlgAbout.Version + ', ' + TFAbout.GetDate;
    ENewVersion.Text:= '';
    Memo.Lines.Clear;
    ProgressBar.Position:= 0;
    ShowModal;
    Free;
  end;
end;

procedure TFJava.MISaveAsProjectClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  with SDSaveAs do begin
    Title:= _(LNGSaveAs);
    InitialDir:= FConfiguration.Sourcepath;
    Filter:= 'Java-Editor-' + _(LNGProject) + ' (*.jep)|*.jep';
    filename:= '';
    if Execute then begin
      if ExtractFileExt(filename) = '' then
        filename:= filename + '.jep';
      if not FileExists (filename) or
         FileExists(filename) and
         (MessageDlg(Format(_(LNGFileAlreadyExists), [filename]),
                       mtConfirmation, mbYesNoCancel, 0) = mrYes)
      then begin
        DoSaveAsProject(filename);
        UpdateMenuItems(Self);
      end;
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.DoSaveAsProject(const filename: string);
  var SL: TStringList;
      i: integer;
      s1, s2, current: string;
      aForm: TFForm;
begin
  SL:= TStringList.create;
  SL.Add('ClasspathUser=' + FConfiguration.JavaClasspathUser);
  SL.Add('InterpreterParameter=' + FConfiguration.JavaInterpreterParameter);
  SL.Add('CompilerParameter=' + FConfiguration.JavaCompilerParameter);
  SL.Add('JavaDoc=' + FConfiguration.JavaJavaDocs);
  SL.Add('JavaStartClass=' + FConfiguration.JavaStartClass);
  SL.Add('JavaStartClassIsApplet=' + BoolToStr(FConfiguration.JavaStartKlasseIsApplet));

  for i:= 1 to myTabBar.Tabs.Count do begin
    aForm:= getTabForm(i-1);
    if assigned(aForm) then begin
      s1:= aForm.GetState + aForm.GetFormType;
      s2:= aForm.Pathname;
      if hasUMLExtension(s2) and aForm.DefaultFilename
        then SL.Add(s1 + FConfiguration.RemovePortableDrive(FConfiguration.TempDir) + ExtractFilename(s2))
        else SL.Add(s1 + FConfiguration.RemovePortableDrive(s2));
    end;
    if i - 1 = myTabBar.Tabs.IndexOf(myTabBar.SelectedTab) then
      Current:= s1 + FConfiguration.RemovePortableDrive(s2);
  end;
  SL.Add(Current);
  ProjectFilename:= filename;
  FMessages.StatusMessage(ProjectFilename);
  SL.SaveToFile(filename);
  FreeAndNil(SL);

  FConfiguration.WriteStringU('Window', 'Win1', '%T%' + filename);
  FConfiguration.WriteIntegerU('Window', 'Wins', 1);
  FConfiguration.WriteStringU('Window', 'Current', '');
end;

procedure TFJava.OpenProject(const filename: string);
  var SL: TStringList; WinName, Current: string; i: integer;
begin
  // Must not open the explorer window when double-clicking from the explorer
  // must be started by timer in this case
  SL:= TStringList.Create;
  try
    try
      SL.LoadFromFile(filename);
      try
        Screen.Cursor:= crHourGlass;
        LockWindow(FJava.Handle);
        FConfiguration.SaveConfigurationForProject;
        for i:= 0 to SL.Count - 2 do begin
          if Pos('ClasspathUser=', SL[i]) = 1 then
            FConfiguration.JavaClasspathUser:= Copy(SL[i], 15, length(SL[i]))
          else if Pos('InterpreterParameter=', SL[i]) = 1 then
            FConfiguration.JavaInterpreterParameter:= Copy(SL[i], 22, length(SL[i]))
          else if Pos('CompilerParameter=', SL[i]) = 1 then
            FConfiguration.JavaCompilerParameter:= Copy(SL[i], 19, length(SL[i]))
          else if Pos('JavaJavaDocs=', SL[i]) = 1 then
            FConfiguration.JavaJavaDocs:= Copy(SL[i], 14, length(SL[i]))
          else if Pos('JavaStartClass=', SL[i]) = 1 then
            FConfiguration.JavaStartClass:= Copy(SL[i], 16, length(SL[i]))
          else if Pos('JavaStartClassIsApplet=', SL[i]) = 1 then
            FConfiguration.JavaStartKlasseIsApplet:= StrToBool(Copy(SL[i], 24, length(SL[i])))
          else begin
            WinName:= FConfiguration.AddPortableDrive(SL[i]);
            OpenFileWithState(WinName);
          end;
        end;
        i:= SL.Count - 1;
        if i >= 0 then begin
          Current:= FConfiguration.AddPortableDrive(SL[i]);
          OpenFileWithState(Current);
        end;
        ProjectFilename:= filename;
        FMessages.StatusMessage(ProjectFilename);
        RearrangeFileHistory(filename);
      finally
        UnlockWindow;
        Screen.Cursor:= crDefault;
      end;
    except
      on e: exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TFJava.MIObjectInspectorClick(Sender: TObject);
begin
  FObjectInspector.ChangeHideShow;
end;

procedure TFJava.MIClassEditorClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  if assigned(ActiveTDIChild) then begin
    if (ActiveTDIChild.FormTag = 1) and (ActiveTDIChild as TFEditForm).isJava then
      PrepareClassEdit(ActiveTDIChild.Pathname, 'Edit', nil)
    else if (ActiveTDIChild.FormTag = 2) then begin
      aUMLForm:= ActiveTDIChild as TFUMLForm;
      aUMLForm.TBClassEditorClick(Self);
    end;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.MIMessagesDockedClick(Sender: TObject);
begin
  if FMessages.myIsVisible then
    if FMessages.Floating
      then FMessages.MyDock
      else FMessages.Undock;
end;

procedure TFJava.MIRefreshClick(Sender: TObject);
begin
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then
    (ActiveTDIChild as TFUMLForm).TBRefreshClick(Self);
end;

procedure TFJava.MINewClassClick(Sender: TObject);
  var EditForm: TFEditForm; hidden: boolean;
begin
  DisableUpdateMenuItems;
  aUMLForm:= getActiveUML;
  if assigned(aUMLForm) then begin
    FConfiguration.Sourcepath:= ExtractFilePath(aUMLForm.Pathname);
    aUMLForm.MainModul.UnSelectAllElements;
    LockWindow(FJava.Handle);
    hidden:= true;
  end else begin
    var aEditor:= getActiveEditor;
    if assigned(aEditor) then
      FConfiguration.Sourcepath:= ExtractFilePath(aEditor.Pathname);
    hidden:= false;
  end;
  EditForm:= NewEditform(hidden);
  if Assigned(EditForm) then begin
    FTemplates.SBNewClass(EditForm);
    PrepareClassEdit(EditForm.Pathname, 'New', aUMLForm);
  end;
  if Assigned(aUMLForm) then begin
    aUMLForm.ConfigureWindow(Self);
    UnlockWindow;
  end;
  EnableUpdateMenuItems;
end;

procedure TFJava.NewTestClass(const Pathname: string);
begin
  aUMLForm:= nil;
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then begin
    aUMLForm:= ActiveTDIChild as TFUMLForm;
    aUMLForm.MainModul.UnSelectAllElements;
    LockWindow(FJava.Handle);
  end;
  var JavaFormular:= NewEditform(true);
  if Assigned(JavaFormular) then begin
    FTemplates.SBNewClass(JavaFormular);
    PrepareClassEdit(JavaFormular.Pathname, 'New', aUMLForm);
  end;
  if Assigned(aUMLForm) then begin
    aUMLForm.ConfigureWindow(Self);
    UnlockWindow;
  end;
end;

procedure TFJava.MIClassOpenClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  LockWindow(FJava.Handle);
  with ODOpen do begin
    if assigned(ActiveTDIChild)
      then InitialDir:= ExtractFilePath(ActiveTDIChild.Pathname)
      else InitialDir:= FConfiguration.Sourcepath;
    filename:= '*.java;*.class';
    FilterIndex:= 1;
    if Execute then begin
      for var i:= 0 to Files.Count - 1 do
        DoOpenInUMLWindow(Files.Strings[i]);
      FConfiguration.Sourcepath:= ExtractFilePath(Files.Strings[0]);
    end;
  end;
  UnlockWindow;
  EnableUpdateMenuItems;
end;

procedure TFJava.DoOpenInUMLWindow(const Pathname: string);
  var UMLWindow: TFUMLForm; s: string; aForm: TFForm;
begin
  if not (hasJavaExtension(Pathname) or hasClassExtension(Pathname)) then
    exit;
  FConfiguration.ShowAlways:= false;
  UMLWindow:= nil;
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 2) then begin
    UMLWindow:= ActiveTDIChild as TFUMLForm;
    UMLWindow.MainModul.UnSelectAllElements;
  end;
  if Assigned(UMLWindow) then begin
    UMLWindow.MainModul.AddToProject(Pathname);
    UMLWindow.Modified:= true;
    if UMLWindow.Mainmodul.Diagram.ShowObjectDiagram then begin
      UMLWindow.Mainmodul.Diagram.ShowObjectDiagram:= false;
      UMLWindow.TBObjectDiagram.Down:= false;
    end;
  end else begin
    s:= ChangeFileExt(Pathname, '.uml');
    if FileExists(s) then begin
      Open(s);
      aForm:= getTDIWindowType(s, '%U%');
    end else
      aForm:= nil;
    if assigned(aForm) then begin
      UMLWindow:= (aForm as TFUMLForm);
      UMLWindow.MainModul.AddToProject(Pathname);
    end else begin
      UMLWindow:= MakeNewUMLWindow(s, '');
      UMLWindow.MainModul.AddToProject(Pathname);
      DoSave(UMLWindow, WithoutBackup);
      RearrangeFileHistory(s);
    end;
  end;
  UMLWindow.OpenWindow(Self);
  UMLWindow.setActiveControl(UMLWindow.MainModul.Diagram.GetPanel);
  UMLWindow.CreateTVFileStructure;
  FConfiguration.ShowAlways:= true;
end;

procedure TFJava.ClassEdit;
  var s: string; Editform: TFEditForm;
      x, y: integer;
begin
  Editform:= TFEditForm(getTDIWindowType(EditClass, '%E%'));
  if not Assigned(Editform) and FileExists(EditClass) then
    Editform:= OpenEditForm(EditClass, true)
  else if (EditClassStatus = 'New') and assigned(aUMLForm) and assigned(Editform) then
    Editform.Hide;

  if Assigned(Editform) then begin
    with TFClassEditor.Create(Self) do begin
      CreateTreeView(Editform, aUMLForm);
      Editform.Editor.LockUndo;
      if Assigned(aUMLForm) then begin
        Application.ProcessMessages;
        aUMLForm.MainModul.Diagram.Lock(true);
      end;
      ShowModal;
      SaveWindow;
      Free;
    end;

    Editform.AutomatedCompleteImports;
    if Editform.Modified then begin
      Editform.Save(false); // Here
      myJavaCommands.CompileForm(EditForm);
    end;

    Editform.Editor.UnlockUndo;

    if Assigned(aUMLForm) then begin
      aUMLForm.MainModul.AddToProject(Editform.Pathname);
      aUMLForm.Modified:= true;
      aUMLForm.MainModul.Diagram.Lock(false);
    end;

    if Editform.Visible and (EditClassStatus = 'Edit') then begin
      // TODO isn't that easier?
      x:= Editform.Editor.CaretX;
      y:= Editform.Editor.CaretY;
      Editform.Editor.Perform(WM_LBUTTONDOWN, 300, 300);
      sleep(10);
      Editform.Editor.Perform(WM_LBUTTONUP, 300, 300);
      Editform.Editor.CaretX:= x;
      Editform.Editor.CaretY:= y;
    end;
  end else begin
    s:= ChangeFileExt(ExtractFilename(EditorForm.Pathname), '');
    ErrorMsg(Format(_(LNGClassNotFound), [s]));
  end;
  if Assigned(aUMLForm) then begin
    aUMLForm.OpenWindow(Self);
    aUMLForm.setActiveControl(aUMLForm.MainModul.Diagram.GetPanel);
  end;
end;

procedure TFJava.PrepareClassEdit(const Pathname, Status: string; UML: TFUMLForm);
begin
  if ((Status = 'Edit') or (Status = 'Refresh')) and
      not (myJavaCommands.HasValidClass(Pathname) or
           myJavaCommands.AcceptableError(Pathname) or
           (MessageDlg(_('Sourcecode has errors. Open anyway?'), mtConfirmation, mbYesNo, -1) = idYes))
     then exit;
  EditClass:= Pathname;
  EditClassStatus:= Status;
  aUMLForm:= UML;
  TThread.ForceQueue(nil, procedure
  begin
    ClassEdit;
  end);
end;

procedure TFJava.MIShowExecutionPointClick(Sender: TObject);
begin
  FMessages.LBStack.ItemIndex:= 0;
  FMessages.LBStackDblClick(Self);
end;

procedure TFJava.MIRunToCursorClick(Sender: TObject);
begin
  if assigned(EditorForm) then
    EditorForm.InsertGotoCursorBreakpoint;
  MIRunClick(Self);
end;

procedure TFJava.ControlBarDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; state: TDragState;
  var Accept: Boolean);
begin
  if (Source.Control is TFMessages) then
    Accept:= false;
end;

// TODO
procedure TFJava.MoveHSplitter(aHeight: integer);
begin
  HSplitter.Align:= alTop;
  BottomDockPanel.Align:= alTop;
  BottomDockPanel.Height:= aHeight;
  BottomDockPanel.Align:= alBottom;
  HSplitter.Align:= alBottom;
end;

procedure TFJava.MoveVSplitter(aWidth: integer);
begin
  VSplitter.Align:= alLeft;
  RightDockPanel.Align:= alLeft;
  RightDockPanel.Width:= aWidth;
  RightDockPanel.Align:= alRight;
  VSplitter.Align:= alRight;
end;

procedure TFJava.SBSequenceClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var Child:= TFSequenceForm(FormFactory(fkSequence));
  MISaveAsClick(Self);
  if Assigned(Child) and (Child.Pathname = '') then   // aborting save
    Child.Close
  else begin
    Child.New(Child.Pathname);
    Child.Save(false);
  end;
  EnableUpdateMenuItems
end;

procedure TFJava.SBStructogramClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  var Child:= TFStructogram(FormFactory(fkStructogram));
  MISaveAsClick(Self);
  if Assigned(Child) and (Child.Pathname = '') then   // aborting save
    Child.Close
  else begin
    Child.New;
    Child.Save(false);
  end;
  EnableUpdateMenuItems
end;

procedure TFJava.SBStructureIndentClick(Sender: TObject);
begin
  if assigned(EditorForm) then
    EditorForm.SBStructureIndentClick(Self);
end;

procedure TFJava.StructogramFromText(sourcecode, pathname: string);
begin
  var Child:= TFStructogram(getTDIWindowType(Pathname, '%S%'));
  if assigned(Child) then begin
    Child.RenewFromText(sourcecode);
    Open(Pathname);
  end else begin
    var s:= Format(_(LNGFileAlreadyExists), [Pathname]);
    if FileExists(Pathname) then begin
      var mr:= MessageDlg(s, mtConfirmation, mbYesNoCancel, 0);
      case mr of
         mrYes: System.IOUtils.TFile.Delete(Pathname);
         mrNo : Pathname:= getFileName('.jsg');
         else exit;
       end;
    end;
    Child:= TFStructogram(FormFactory(fkStructogram));
    Child.Pathname:= Pathname;
    Child.FromText(sourcecode);
  end;
  UpdateMenuItems(self);
end;

procedure TFJava.HSplitterMoved(Sender: TObject);
begin
  FConfiguration.BottomDockPanelHeight:= BottomDockPanel.Height;
  if assigned(FFilestructure) and FFileStructure.Visible or
     assigned(FObjectInspector) and FObjectInspector.Visible then
    UpdateLayoutRightDockPanel;
  HSplitter.Top:= 0;
end;

procedure TFJava.VSplitterMoved(Sender: TObject);
begin
  if Assigned(FConfiguration) then
    FConfiguration.RightDockPanelWidth:= RightDockPanel.Width;
  VSplitter.Left:= 0;
  RightDockPanel.Left:= High(SmallInt);
end;

procedure TFJava.LoadBounds;
  var L, T, W, H: integer;
begin
  if FConfiguration.FirstStartAfterInstallation
    then WindowState:= wsMaximized
    else WindowState:= StrToWindowState(FConfiguration.ReadStringU('Program', 'WindowState', '2'));
  if WindowState = wsNormal then begin // wsNormal
    L:= FConfiguration.ReadIntegerU('Program', 'Left', 0);
    T:= FConfiguration.ReadIntegerU('Program', 'Top', 0);
    W:= FConfiguration.ReadIntegerU('Program', 'Width', 800);
    H:= FConfiguration.ReadIntegerU('Program', 'Height', 600);
    T:= min(T, Screen.Width - 100);
    SetBounds(L, T, W, H);
  end;
end;

procedure TFJava.SaveBounds;
begin
  FConfiguration.WriteIntegerU('Program', 'Left', PPIUnScale(Left));
  FConfiguration.WriteIntegerU('Program', 'Top', PPIUnScale(Top));
  FConfiguration.WriteIntegerU('Program', 'Width', PPIUnScale(Width));
  FConfiguration.WriteIntegerU('Program', 'Height', PPIUnScale(Height));
  FConfiguration.WriteStringU('Program', 'WindowState', WindowStateToStr(WindowState));
end;

procedure TFJava.MINewUMLClick(Sender: TObject);
begin
  DisableUpdateMenuItems;
  MakeNewUMLWindow(getFilename('.uml'), '');
  EnableUpdateMenuItems;
end;

function TFJava.getUMLWindow: TFUMLForm;
begin
  Result:= nil;
  for var i:= 0 to myTabBar.Tabs.Count - 1 do begin
    var aForm:= getTabForm(i);
    if aForm.FormTag = 2 then begin
      Result:= (aForm as TFUMLForm);
      break;
    end;
  end;
end;

procedure TFJava.MICompareClick(Sender: TObject);
  var Forms: array[0..1] of TFEditForm; i: integer;
begin
  DisableUpdateMenuItems;
  EditorAgeTimer.Enabled:= false;
  Forms[0]:= nil;
  Forms[1]:= nil;
  for i:= 0 to min(2, TDIEditFormCount) - 1 do begin
    Forms[i]:= TDIEditFormGet(i);
    if Forms[i].Modified then
      DoSave(Forms[i], WithoutBackup);
  end;
  NewTextDiff(Forms[0], Forms[1]);
  EnableUpdateMenuItems;
end;

procedure TFJava.MISVNCommitClick(Sender: TObject);
  var p, m: string;
begin
  if Assigned(ActiveTDIChild) then begin
    MISaveAllClick(Self);
    p:= ExtractFilePath(ActiveTDIChild.Pathname);
    if InputQuery('Commit', _(LNGMessage), m) then
      FSubversion.CallSVN('\bin\svn', 'commit -m "' + m + '"', p);
  end;
end;

procedure TFJava.MISVNAddClick(Sender: TObject);
  var s, p, repos, TempFile1, TempFile2: string;
      aText: TStringList;
      FStream: TFileStream;
begin
  LockWindow(FMessages.Handle);
  if Assigned(ActiveTDIChild) then begin
    if (ActiveTDIChild.FormTag = 1) and
       (ActiveTDIChild as TFEditForm).Modified
      then DoSave(ActiveTDIChild as TFForm, WithoutBackup);

    s:= ActiveTDIChild.Pathname;
    p:= ExtractFilePath(s);
    FSubversion.CallSVN('\bin\svn', 'add ' + HideBlanks(s), p);

    aText:= TStringList.create;
    Repos:= FSubversion.getRepositoryURL(s);
    TempFile1:= HideBlanks(FConfiguration.TempDir + 'RunJava.bat');
    TempFile2:= HideBlanks(Repos + '/RunJava.bat');
    FStream:= TFileStream.Create(TempFile1, fmCreate or fmShareExclusive);
    FreeAndNil(FStream);
    FSubversion.CallSVN('\bin\svn', 'import ' + TempFile1 + ' ' + TempFile2 + ' -m ""', p);
    FSubversion.CallSVN('\bin\svn', 'delete ' + TempFile2+ ' -m ""', p);
    FSubversion.CallSVN('\bin\svn', 'checkout ' + Repos + ' ' + HideBlanks(p), p);
    FSubversion.CallSVN('\bin\svn', 'add ' + HideBlanks(s), p);
    aText.AddStrings(FMessages.LBMessages.Items);
    FMessages.LBMessages.Items:= aText;
    FreeAndNil(aText);
  end;
  UnlockWindow;
end;

procedure TFJava.MISVNTreeClick(Sender: TObject);
begin
  if Assigned(ActiveTDIChild) then
    FSubversion.CallSVN('\bin\svnlook', 'tree ' + HideBlanks(FConfiguration.SVNRepository), '');
end;

procedure TFJava.MISVNStatusClick(Sender: TObject);
begin
  if Assigned(ActiveTDIChild) then begin
    var s:= ActiveTDIChild.Pathname;
    var p:= ExtractFilePath(s);
    FSubversion.CallSVN('\bin\svn', 'status ' + HideBlanks(p) + ' -u -v', p);
  end;
end;

procedure TFJava.MISVNUpdateClick(Sender: TObject);
begin
  if Assigned(ActiveTDIChild) then begin
    var s:= ActiveTDIChild.Pathname;
    var p:= ExtractFilePath(s);
    FSubversion.CallSVN('\bin\svn', 'update ' + HideBlanks(p), p);
  end;
end;

procedure TFJava.MISVNCompareClick(Sender: TObject);
  var s, s1, s2, p, rev: string;
      aActive, aForm: TFEditForm;
begin
  LockWindow(FMessages.Handle);
  if Assigned(ActiveTDIChild) then begin
    aActive:= ActiveTDIChild as TFEditForm;
    s:= aActive.Pathname;
    p:= ExtractFilePath(s);
    FSubversion.ForFile(s);
    if (FSubversion.ShowModal = mrOK) and (FSubversion.LVRevisions.ItemIndex > -1) then begin
      rev:= FSubversion.getRevision;
      FSubversion.CallSVN('\bin\svn', 'cat -r ' + rev + ' ' + s, p);
      s1:= FMessages.LBMessages.Items[0];
      FMessages.LBMessages.Items.Delete(0);
      s2:= ExtractFileName(s);
      insert('[R' + rev + ']', s2, Pos('.', s2));
      s2:= FConfiguration.TempDir + s2;
      aForm:= TFEditForm(FormFactory(fkEditor));
      if Assigned(aForm) then begin
        aForm.New(s2);
        aForm.PutText(FMessages.LBMessages.Items.Text, false);
        aForm.Save(false);
        NewTextDiff(aActive, aForm);
      end;
      FMessages.LBMessages.Items.Text:= s1;
    end
    else
      FMessages.LBMessages.Clear;
  end;
  UnlockWindow;
end;

procedure TFJava.MISVNLogClick(Sender: TObject);
begin
  if Assigned(ActiveTDIChild) then begin
    var s:= ActiveTDIChild.Pathname;
    var p:= ExtractFilePath(s);
    FSubversion.CallSVN('\bin\svn', 'log ' + HideBlanks(s), p);
  end;
end;

procedure TFJava.scpJavaExecute(Kind: SynCompletionType; Sender: TObject;
    var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
begin
  if CurrentInput <> ''
    then ScpState:= 0
    else ScpState:= 1;
  if FMessages.InteractiveEditActive or assigned(EditorForm) and EditorForm.IsJava
    then CanExecute:= myCodeCompletion.DoScpJavaExecute(ScpState, '', CurrentInput, EditorForm)
    else CanExecute:= false;
end;

procedure TFJava.scpParamsExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
begin
  CanExecute:= myCodeCompletion.DoScpParamsExecute(2);
  if not CanExecute then
    AfterCodeCompletion:= false;
end;

procedure TFJava.scpJavaChange(Sender: TObject; AIndex: Integer);
begin
  FScpHint.MHint.Clear;
  if (-1 < AIndex) and (AIndex < myCodeCompletion.DocuList.Count) then
    FScpHint.MHint.Lines.Add(MyCodeCompletion.DocuList[AIndex]);
  FScpHint.MHint.SelStart:= 0;
  FScpHint.MHint.SelLength:= 0;
end;

procedure TFJava.scpJavaShow(Sender: TObject);
begin
  if (ScpState > 0) and (MyCodeCompletion.DocuList.Count > 0) then begin
    FScpHint.MHint.Lines.Add(MyCodeCompletion.DocuList[0]);
    FScpHint.SetBounds(scpJava.Form.Left,
                       scpJava.Form.Top + scpJava.Form.Height - 7,
                       scpJava.Form.Width, FScpHint.Height);
    FScpHint.Visible:= true;
  end;
end;

procedure TFJava.scpJavaClose(Sender: TObject);
begin
  FScpHint.Close;
  if assigned(FScpHint.MHint) then
    FScpHint.MHint.Clear;
end;

procedure TFJava.scpHandleMethods(Editor: TSynEdit; Index: integer; EndToken: Char);
  const ecChar = 511;           // insert char
        ecDeleteLastChar = 501; // delete last char (i.e. backspace key)
begin
  var s:= scpJava.ItemList[Index];
  scpJavaIsMethod:= (Pos('\image{9}', s) + Pos('\image{10}', s) > 0) and (pos(': void', s) > 0);
  ScpParamIndex:= Index;
  var p1:= Pos('(', s);
  var p2:= Pos(')', s);
  s:= Copy(s, p1+1, p2-p1-1);
  Editor.CommandProcessor(ecChar, ')', nil);
  if scpJavaIsMethod then
    Editor.CommandProcessor(ecChar, ';', nil);
  if s <> '' then begin // with parameters
    if scpJavaIsMethod then
      Editor.CommandProcessor(ecLeft, #0, nil);
    Editor.CommandProcessor(ecLeft, #0, nil);
    scpParams.ItemList.Clear;
    scpParams.ItemList.Add(s);
    AfterCodeCompletion:= true;
    scpParams.ActivateCompletion;
    if EndToken = '(' then
      Editor.CommandProcessor(ecDeleteLastChar, #0, nil)
  end;
  if assigned(EditorForm) then
    EditorForm.Enter(Self);
end;

procedure TFJava.scpJavaOnCodeCompletion(Sender: TObject; var Value: string;
      Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  var ext:= ExtractFileExt(Value);
  if (ScpState = 0) and ((ext = '.java') or (ext = '.html')) then
    Value:= ChangeFileExt(ExtractFilename(Value), '');
end;

procedure TFJava.scpJavaAfterCodeCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);

  const ecLeft           = 1;    // Move cursor left one char
        ecUndo           = 601;  // Perform undo if available
        ecKey            = 511;
        ecChar           = 511;  // insert char
        ecDeleteLastChar = 501;  // Delete last char (i.e. backspace key)
        ecString         = 630;

  var s, package: string; i, p: integer; doImport: boolean;
      Editor: TSynEdit; SL: TStringList;
begin
  doImport:= true;
  if FMessages.InteractiveEditActive
    then Editor:= FMessages.ActiveInteractive
  else if assigned(EditorForm)
    then Editor:= EditorForm.Editor
    else exit;

  if Copy(Value, Length(Value), 1) = '(' then
    scpHandleMethods(Editor, Index, EndToken)
  else if Copy(Value, Length(Value)-4, 5) = '.html' then
    with Editor do begin
      s:= Lines[CaretY-1];
      s:= ReplaceStr(s, Value, '');
      Lines[CaretY-1]:= s;
      CaretX:= CaretX - Length(Value);
      CallHelp(Value);
      doImport:= false;
    end
  else if Copy(Value, Length(Value)-4, 5) = '.java' then
    with Editor do begin
      s:= Lines[CaretY-1];
      s:= ReplaceStr(s, Value, '');
      Lines[CaretY-1]:= s;
      CaretX:= CaretX - Length(Value);
      SwitchToWindow(Value);
      doImport:= false;
    end;

  if Pos('|', Value) > 0 then begin   // if/while/for/do/else/....
    SL:= TStringList.Create;
    SL.Text:= Value;
    i:= 0;
    p:= Pos('|', SL.Strings[i]);
    while (i < SL.Count - 1) and (p = 0) do begin
      inc(i);
      p:= Pos('|', SL.Strings[i]);
    end;
    Editor.CaretY:= Editor.CaretY - (SL.Count - 1) + i;
    s:= Editor.Lines[Editor.CaretY - 1];
    p:= Pos('((|', s);  //  if with direct following (
    if p > 0 then begin
      delete(s, p+1, 2);
      inc(p);
    end else begin
      p:= Pos('|', s);
      delete(s, p, 1);
    end;
    Editor.Lines[editor.CaretY - 1]:= s;
    Editor.CaretX:= p;
    FreeAndNil(SL);
    doImport:= false;
  end;

  if Pos('if () {', Value) = 1 then begin // handle "if"
    if Pos(' else ', Value) > 0
      then Editor.CaretY:= Editor.CaretY - 4
      else Editor.CaretY:= Editor.CaretY - 2;
    s:= Editor.Lines[Editor.CaretY-1];
    p:= Pos(' () ', s);
    Editor.CaretX:= p + 2;
    doImport:= false;
  end;

  if (-1 < Index) and (Index < scpJava.ItemList.Count) then begin
    s:= scpJava.ItemList[Index];
    p:= Pos(' - \color{clgray}', s);
    if (p > 0) and assigned(EditorForm) and doImport and (endToken = #0) then begin
      package:= s;
      delete(package, 1, p + length(' - \color{clgray}') - 1);
      if value <> '' then package:= package + '.' + value;    // value = class
      EditorForm.InsertImport(package);
    end;
  end;
end;

procedure TFJava.scpParamsPaintItem(Sender: TObject; Index: Integer;
  TargetCanvas: TCanvas; ItemRect: TRect; var CustomDraw: Boolean);
begin
  var i:= scpParams.Form.CurrentIndex;
  var s:= scpParams.ItemList[0];
  s:= myCodeCompletion.FormatParamList(s, i);
  FormattedTextOut(TargetCanvas, ItemRect, CurrentPPI, s, False, nil, nil);
  CustomDraw:= true;
end;

procedure TFJava.scpSetEditForm(Editform: TFEditForm);
begin
  if Editform.isJava
    then scpSetEditor(EditForm.Editor)
    else scpSetEditor(nil);
end;

procedure TFJava.scpSetEditor(Editor: TSynEdit);
begin
  scpJava.Editor:= Editor;
  scpParams.Editor:= Editor;
  if FConfiguration.CodeCompletionAlways then
    scpJava.TriggerChars:= '.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  if FConfiguration.CodeCompletionCtrlSpace then
    scpJava.TriggerChars:= '.';
  scpJava.ShortCut:= Menus.ShortCut(Word(' '), [ssCtrl]);
  if FConfiguration.ParameterHints
    then FJava.scpParams.TriggerChars:= '('
    else FJava.scpParams.TriggerChars:= '';
end;

function TFJava.getTab(Nr: integer): integer;
begin
  Result:= -1;
  if assigned(myTabBar) then
    for var i:= 0 to myTabBar.Tabs.Count - 1 do
      if assigned(myTabBar.Tabs[i]) and assigned(myTabBar.Tabs[i].Data) and
         (TTabObject(myTabBar.Tabs[i].Data).Nr = Nr) then
        exit(i);
end;

procedure TFJava.TabModified(Nr: integer; Modified: boolean);
begin
  var i:= getTab(Nr);
  if i > -1 then
    myTabBar.Tabs[i].Modified:= Modified;
end;

procedure TFJava.SetSelectedTabAndWindow(Nr: integer);
  var i: integer; MI: TTBCustomItem;
begin
  if assigned(myTabBar) then begin
    for i:= 0 to myTabBar.Tabs.Count - 1 do
      if (TTabObject(myTabBar.Tabs[i].Data).Nr = Nr) and not myTabBar.Tabs[i].Selected then
        myTabBar.Tabs[i].Selected:= true;
    for i:= myTabBar.Tabs.Count downto 1 do begin
      MI:= MIWindow[MIWindow.Count - i];
      if (MI.Tag = Nr) and not MI.Checked then MI.Checked:= true
      else if (MI.Tag <> Nr) and MI.Checked then MI.Checked:= false;
    end;
  end;
end;

procedure TFJava.SetSelectedTabAndWindow(const Pathname: string);
  var i, j, n: integer;
begin
  if assigned(myTabBar) then begin
    n:= -1;
    for i:= 0 to myTabBar.Tabs.Count - 1 do
      if getTabForm(i).Pathname = Pathname then begin
        myTabBar.Tabs[i].Selected:= true;
        n:= i;
        break;
      end;
    for j:= 0 to myTabBar.Tabs.Count -1 do
      MIWindow.Items[j].checked:= false;
    if n > -1 then
      MIWindow.Items[WindowOffset + n].checked:= true;
  end;
end;

procedure TFJava.EditorAgeTimerTimer(Sender: TObject);
begin
  EditorAgeTimer.Enabled:= false;
  for var i:= 0 to TDIEditFormCount - 1 do
    TDIEditFormGet(i).CheckAge;
  EditorAgeTimer.Enabled:= true;
end;

procedure TFJava.Restart;
begin
  var s:= '';
  for var i:= 1 to ParamCount do
    s:= s + ' ' + ParamStr(i);
  Close;
  Application.ProcessMessages;
  Sleep(20);
  myJavaCommands.ExecWithoutWait(ParamStr(0), s, '', SW_ShowNormal);
end;

function TFJava.IsAValidClass(const Pathname: string): boolean;
begin
  var F:= getTDIWindowType(Pathname, '%E%');
  var Modified:= assigned(F) and (F as TFEditForm).Modified and F.Visible;
  Result:= (Pathname = '') or (Pos(FConfiguration.JavaCache, Pathname) = 1) or
           (FConfiguration.HasAgeClass(Pathname) and not Modified);
end;

procedure TFJava.RefreshUMLWindows;
  var i: integer; Form, aForm: TFForm;
begin
  LockWindow(FJava.Handle);
  Form:= ActiveTDIChild;
  for i:= 0 to myTabBar.Tabs.Count - 1 do begin
    aForm:= getTabForm(i);
    if aForm.FormTag = 2 then
      (aForm as TFUMLForm).Refresh;
  end;
  if assigned(Form) and Form.CanFocus then Form.setFocus;
  LockWindow(0);
  UpdateMenuItems(Self);
end;

function TFJava.getActiveTDIFormPath: string;
begin
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag in [1, 2])
    then Result:= ExtractFilepath(ActiveTDIChild.Pathname)
    else Result:= FConfiguration.EditorFolder;
end;

function TFJava.ExtentClasspath(cp: string): string;
  var s: string; i: integer;
      aForm: TFForm; UMLForm: TFUMLForm; EditForm: TFEditForm;
begin
  EditForm:= getActiveEditor;
  if assigned(EditForm) then begin
    s:= ExtractFilePath(ActiveTDIChild.Pathname);
    if Pos(s, cp) = 0 then
      cp:= s + ';' + cp;
  end;
  UMLForm:= getActiveUML;
  if assigned(UMLForm) then begin
    s:= UMLForm.MainModul.Diagram.getSourcePath;
    if Pos(s, cp) = 0 then
      cp:= s + ';' + cp;
  end;

  for i:= 0 to TDIFormsList.Count - 1 do begin
    aForm:= TDIFormsList[i];
    if aForm.FormTag = 1 then begin
      s:= ExtractFilePath(aForm.Pathname);
      if Pos(s, cp) = 0 then
        cp:= s + ';' + cp;
    end else if aForm.FormTag = 2 then begin
      s:= (aForm as TFUMLForm).MainModul.Diagram.getSourcePath;
      if Pos(s, cp) = 0 then
        cp:= s + ';' + cp;
    end;
  end;
  Result:= cp;
end;

function TFJava.getAllPathnames: TStringList;
begin
  Result:= TStringList.Create;
  Result.Sorted:= true;
  for var i:= 0 to TDIFormsList.count - 1 do begin
    var SL:= TDIFormsList[i].getAllPathnames;
    if SL.Count > 0 then
      Result.AddStrings(SL);
    FreeAndNil(SL);
  end;
end;

function TFJava.getAllClassnames: TStringList;
  var SL: TStringList;
begin
  Result:= TStringList.Create;
  Result.Sorted:= true;
  for var i:= 0 to TDIFormsList.count - 1 do begin
    SL:= TDIFormsList[i].getAllClassnames;
    if SL.Count > 0 then
      Result.AddStrings(SL);
    FreeAndNil(SL);
  end;
end;

function TFJava.GetActiveFormTag: integer;
begin
  Result:= -1;
  if ActiveTool > -1 then
    Result:= ActiveTool
  else if assigned(ActiveTDIChild) then
    Result:= ActiveTDIChild.FormTag;
end;

procedure TFJava.SaveDocking;
begin
  var Stream:= TMemoryStream.Create;
  try
    RightDockPanel.DockManager.SaveToStream(Stream);
    Stream.Position:= 0;
    FConfiguration.WriteBinaryStreamU('Docking', 'RightDockPanel', Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TFJava.LoadDocking;
begin
  var Stream:= TMemoryStream.Create;
  try
    var Size:= FConfiguration.ReadBinaryStreamU('Docking', 'RightDockPanel', Stream);
    if Size > 0 then begin
      Stream.Position:= 0;
      RightDockPanel.DockManager.LoadFromStream(Stream);
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TFJava.ShowCompileErrors;
  var SL: TStrings; i, d1, d2, d3, p, lineNr, column: integer;
      aCompile, path, compilepath, line, s2, pre, post, mid1, mid2, err: string;
      E, E1: TFEditForm; SL1: TStringList;

  procedure determinePrePost;
    var j: integer;
  begin
    pre:= '';
    post:= '';
    mid1:= '';
    mid2:= '';
    if s2 = 'cannot find symbol' then begin
      post:= SL.Strings[i+3];
      p:= Pos('symbol: ', post);
      delete(post, 1, p + 8);
    end else if Pos('might not have been initialized', s2) > 0 then begin
      pre:= ReplaceStr(s2, 'might not have been initialized', '');
      s2:= ReplaceStr(s2, pre, '');
    end else if Pos('is already defined in', s2) > 0 then begin
      p:= Pos('is already defined in', s2);
      pre:= copy(s2, 1, p-1);
      delete(s2, 1, p-1);
      post:= copy(s2, length('is already defined in') + 1, length(s2));
      s2:= ReplaceStr(s2, post, '');
    end else if Pos('array required, but', s2) > 0 then begin
      SL1:= Split(' ', s2);
      mid1:= SL1.Strings[3];
      s2:= 'array required, but found';
      FreeAndNil(SL1);
    end else if s2 = 'incompatible types: missing return value' then
    else if s2 = 'incompatible types: unexpected return value' then
    else if Pos('incompatible types: possible lossy conversion from', s2) = 1 then begin
      SL1:= Split(' ', s2);
      if SL1.Count >= 9 then begin
        mid1:= SL1.Strings[6];
        mid2:= SL1.Strings[8];
      end;
      s2:= 'incompatible types: possible lossy';
      FreeAndNil(SL1);
    end else if Pos('incompatible types: ', s2) = 1 then begin
      SL1:= Split(' ', s2);
      if SL1.Count >= 8 then begin
        mid1:= SL1.Strings[2];
        mid2:= SL1.Strings[7];
      end;
      s2:= 'incompatible types';
      FreeAndNil(SL1);
    end else if (Pos('non-static', s2) = 1) and (Pos('cannot be referenced from a static context', s2) > 0) then begin
      SL1:= Split(' ', s2);
      j:= 1;
      while (j < SL1.Count) and (SL1.Strings[j] <> 'cannot') do begin
        mid1:= mid1 + SL1.Strings[j] + ' ';
        inc(j);
      end;
      mid1:= trim(mid1);
      s2:= 'non-static cannot be referenced';
      FreeAndNil(SL1);
    end else if Pos('bad operand types for binary operator', s2) = 1 then begin
      post:= Copy(s2, length('bad operand types for binary operator') + 1, length(s2));
      s2:= 'bad operand types for binary operator';
    end;
  end;

  procedure applyMiddle;
  begin
    if mid2 <> '' then
      err:= Format(err, [mid1, mid2])
    else if mid1 <> '' then
      err:= Format(err, [mid1]);
  end;

begin
  E:= nil;
  E1:= nil;
  SL:= FMessages.LBCompiler.Items;
  aCompile:= Copy(_(LNGCompileWith), 1, Pos('%', _(LNGCompileWith)) - 2);

  for i:= 0 to SL.Count - 1 do begin
    line:= SL.Strings[i];
    if Pos(aCompile, line) = 1 then begin
      if assigned(E) then E.ShowCompileErrors;
      delete(line, 1, length(aCompile) + 1);
      p:= Pos('.java', line);
      delete(line, p + 5, length(line));
      compilepath:= line;
      E:= TFEditForm(getTDIWindowType(compilepath, '%E%'));
      if assigned(E) then E.InitShowCompileErrors;
      break;
    end;
  end;

  for i:= SL.Count - 1 downto 1 do begin
    line:= SL.Strings[i];
    d1:= Pos('.java:', line);
    if d1 > 0 then begin
      path:= copy(line, 1, d1 + 4);
      s2:= copy(line, d1 + 6, 255);
      d3:= d1 + 5;
      d2:= Pos(':', s2);                  // compile error
      if d2 > 0 then begin                // errorline found
        if not TryStrToInt(Copy(s2, 1, d2-1), LineNr) then continue;
        delete(s2, 1, d2);
        d3:= d3 + d2;
        d2:= Pos(':', s2);
        d3:= d3 + d2 + 1;
        line:= copy(line, 1, d3);
        if not TryStrToInt(Copy(s2, 1, d2-1), Column) then
          Column:= 1;
        delete(s2, 1, d2 + 1);
        if Pos('error: ', s2) = 1 then
          delete(s2, 1, 7);
        if FConfiguration.TranslateCompilerErrors then begin
          determinePrePost;
          err:= FConfiguration.TranslateCompilerError(s2);
          applyMiddle;
        end else begin
          pre:= '';
          err:= s2;
          post:= '';
        end;
        line:= line + pre + err + post;
        FMessages.LBCompiler.Items[i]:= line;
        if assigned(E) and (ExtractFilename(compilepath) = path) then
          E.SetErrorMark(lineNr, column, pre + err + post)
        else if Assigned(E1) and (E1.Pathname = path) then
          E1.SetErrorMark(lineNr, column, pre + err + post)
        else begin
          E1:= TFEditForm(getTDIWindowType(path, '%E%'));
          if assigned(E1) then begin
            E1.InitShowCompileErrors;
            E1.SetErrorMark(lineNr, column, pre + err + post)
          end;
        end;
      end;
    end;
  end;
  if assigned(E) then E.ShowCompileErrors;
end;

procedure TFJava.DisableUpdateMenuItems;
begin
  inc(UpdateMenuItemsCount);
end;

procedure TFJava.EnableUpdateMenuItems;
begin
  dec(UpdateMenuItemsCount);
  if UpdateMenuItemsCount = 0 then
    UpdateMenuItems(nil);
end;

procedure TFJava.setActiveTool(value: integer);
begin
  FActiveTool:= value;
end;

function TFJava.hasSomethingToSave: boolean;
begin
  Result:= false;
  for var i:= 0 to TDIFormsList.Count - 1 do
    if TDIFormsList[i].Visible and TDIFormsList[i].Modified then
      Exit(true);
end;

function TFJava.getActiveEditor: TFEditForm;
begin
  Result:= nil;
  var Form:= getActiveForm;
  if assigned(Form) then
    case Form.FormTag of
      1: Result:= Form as TFEditForm;
      2: if assigned(Form.Partner) then
           Result:= (Form.Partner as TFEditForm);
    end;
end;

function TFJava.getActiveUML: TFUMLForm;
begin
  Result:= nil;
  var Form:= getActiveForm;
  if assigned(Form) and (Form.FormTag = 2) then
    Result:= Form as TFUMLForm;
end;

function TFJava.getActiveForm: TFForm;
begin
  Result:= getSelectedTabForm;
end;

function TFJava.getEditorWithMain: TFEditForm;
begin
  for var i:= 0 to TDIFormsList.Count - 1 do
    if (TDIFormsList[i].FormTag = 1) and TFEditForm(TDIFormsList[i]).hasMainInModel then
      Exit(TFEditForm(TDIFormsList[i]));
  Result:= nil;
end;

function TFJava.TDIEditFormCount: integer;
begin
  Result:= 0;
  for var i:= 0 to TDIFormsList.count - 1 do
    if TDIFormsList[i].FormTag = 1 then
      inc(Result);
end;

function TFJava.TDIEditFormGet(index: integer): TFEditForm;
begin
  var i:= -1;
  while (i < TDIFormsList.Count) and (index >= 0) do begin
    if TDIFormsList[i+1].FormTag = 1 then
      dec(index);
    inc(i);
  end;
  if (-1 < i) and (i < TDIFormsList.Count)
    then Result:= TFEditForm(TDIFormsList[i])
    else Result:= nil;
end;

function TFJava.getTabForm(i: integer): TFForm;
begin
  if assigned(myTabBar.Tabs[i]) and assigned(myTabBar.Tabs[i].Data)
    then Result:= TTabObject(myTabBar.Tabs[i].Data).Form
    else Result:= nil;
end;

function TFJava.getSelectedTabForm: TFForm;
begin
  if assigned(myTabBar) and assigned(myTabBar.SelectedTab) and assigned(myTabBar.SelectedTab.Data)
    then Result:= TTabObject(myTabBar.SelectedTab.Data).Form
    else Result:= nil;
end;

procedure TFJava.HideGuiForms;
  // during ChangeStyle GuiForm.Resize causes an exception
begin
  for var i:= 0 to TDIFormsList.count - 1 do
    if TDIFormsList[i] is TFGuiForm then
      (TDIFormsList[i] as TFGuiForm).Hide;
end;

procedure TFJava.ShowGuiForms;
  // unhiding doesn't work, so close and reopen
begin
  var SL:= TStringList.Create;
  for var i:= 0 to TDIFormsList.count - 1 do
    if TDIFormsList[i] is TFGuiForm then begin
      SL.Add(TDIFormsList[i].Pathname);
      (TDIFormsList[i] as TFGuiForm).Close;
    end;
  for var Path in SL do
    FJava.Open(Path);
  FreeAndNil(SL);
end;

procedure TFJava.SetStyle(StyleName: string);
begin
  if StyleName <> TStyleManager.ActiveStyle.Name then begin
    HideGuiForms;
    TStyleManager.SetStyle(StyleName);
    FConfiguration.GUIStyle:= StyleName;
    Application.ProcessMessages;
    ShowGuiForms;
  end;
end;

procedure TFJava.ChangeStyle(Style: string);
  var aColor: TColor;
      Details: TThemedElementDetails;
begin
  if Style = '' then exit;
  DragAcceptFiles(Handle, False);

  SetStyle(Style);

  for var i:= 0 to TDIFormsList.count - 1 do
    TDIFormsList[i].ChangeStyle;

  FMessages.ChangeStyle;
  FGUIDesigner.ChangeStyle;
  FTooltip.ChangeStyle;
  FLLMChatForm.ChangeStyle;
  if assigned(FObjectInspector) then
    FObjectInspector.ChangeStyle;
  if assigned(FJUnitTests) then
    FJUnitTests.ChangeStyle;

  if TFConfiguration.isDark then begin
    ToolbarProgram.Images:= vilProgramDark;
    ToolbarAWT.Images:= vilAWTDark;
    ToolbarSwing1.Images:= vilSwing1Dark;
    ToolBarFXBase.Images:= vilFXBaseDark;
    ToolBarFXShapes.Images:= vilFXShapesDark;
    ToolBarLayout.Images:= vilLayoutDark;
    MainToolbar.Images:= vilToolbarDark;
    DebugToolbar.Images:= vilToolbarDark;
    //MainToolbar.DisabledImages:= vilToolbarDisabledDark;
    MainMenu.Images:= vilMenuDark;
  end else begin
    ToolbarProgram.images:= vilProgramLight;
    ToolbarAWT.Images:= vilAWTLight;
    ToolbarSwing1.Images:= vilSwing1Light;
    ToolBarFXBase.Images:= vilFXBaseLight;
    ToolBarFXShapes.Images:= vilFXShapesLight;
    ToolBarLayout.Images:= vilLayoutLight;
    MainToolbar.Images:= vilToolbarLight;
    DebugToolbar.Images:= vilToolbarLight;
    //MainToolbar.DisabledImages:= vilToolbarDisabledLight;
    MainMenu.Images:= vilMenuLight;
  end;

  if assigned(myTabBar) then begin
    if StyleServices.IsSystemStyle then begin
      TJvModernTabBarPainter(myTabBar.Painter).Color:= clBtnFace;  // sets backgound of tabs
      TJvModernTabBarPainter(myTabBar.Painter).Font.Color:= clBlack;
    end else begin
      Details:= StyleServices.GetElementDetails(tbsBackground);
      StyleServices.GetElementColor(Details, ecFillColor, aColor);
      TJvModernTabBarPainter(myTabBar.Painter).Color:= aColor;
      TJvModernTabBarPainter(myTabBar.Painter).Font.Color:= StyleServices.getStyleFontColor(sfTabTextInactiveNormal);
    end;
  end;
  myTabBarColorize;
  DragAcceptFiles(Handle, True);
end;

 {
  TThemedTabSet = (
    tbsDontCare,
    tbsRoot,
    tbsBackground,
    tbsTabNormal, tbsTabSelected
  );   }

procedure TFJava.myTabBarColorize;
  var i: integer; path1, path2: string; Form: TFForm;
      Details: TThemedElementDetails;
      SamepathColor: TColor;
begin
  Form:= getSelectedTabForm;
  if (Form = nil) and (TDIFormsList.Count > 0) then
    Form:= TDIFormsList[0];
  if assigned(Form) then begin
    if StyleServices.IsSystemStyle then
      SamepathColor:= clWindow
    else begin
      Details:= StyleServices.GetElementDetails(tbsTabNormal);
      StyleServices.GetElementColor(Details, ecFillColor, SamepathColor);
    end;
    path1:= ExtractFilePath(Form.pathname);
    for i:= 0 to myTabBar.Tabs.Count - 1 do begin
      path2:= ExtractFilePath(TTabObject(myTabBar.Tabs[i].Data).Path);
      if (Uppercase(path2) = Uppercase(path1)) or (Form.FormTag in [4, 5, 6]) or
        (TTabObject(myTabBar.Tabs[i].Data).Form.FormTag in [4, 5, 6])
        then myTabBar.Tabs[i].Color:= SamepathColor
        else myTabBar.Tabs[i].Color:= $CFFFFF;
    end;
    if assigned(myTabBar.SelectedTab) then
      myTabBar.SelectedTab.Color:= clActiveCaption;
  end;
end;

//  TFileKind = (fkEditor, fkUML, fkGUI, fkTextDiff, fkBrowser, fkExplorer,
//               fkStructogram, fkSequence);

function TFJava.FormFactory(FormKind: TFormKind): TFForm;
  var aForm: TFForm;
begin
  aForm:= nil;
  case FormKind of
    fkEditor:      aForm:= TFEditForm.Create(Self);
    fkUML:         aForm:= TFUMLForm.Create(Self);
    fkGUI:         aForm:= TFGuiForm.Create(Self);
    fkFXGUI:       aForm:= TFXGUIForm.Create(Self);
    fkTextDiff:    aForm:= TFTextDiff.Create(Self);
    fkBrowser:     aForm:= TFBrowser.Create(Self);
    fkExplorer:    aForm:= TFExplorer.Create(Self);
    fkStructogram: aForm:= TFStructogram.Create(Self);
    fkSequence:    aForm:= TFSequenceform.Create(Self);
  end;
  TDIFormslist.Add(aForm);
  ActiveTDIChild:= aForm;
  Result:= aForm;
end;

procedure TFJava.DoExport(Pathname: string; Bitmap: TBitmap);
  var folder, ext: string;

  procedure InToPng(const filename: string);
  begin
    var Png:= TPngImage.Create;
    Png.Assign(Bitmap);
    try
      Png.SaveToFile(filename);
    finally
      FreeAndNil(Png);
    end;
  end;

  procedure InToBmp(const filename: string);
  begin
    Bitmap.SaveToFile(filename);
  end;

  procedure InToWMF(const filename: string);
    var
      MetafileCanvas: TMetafileCanvas;
      DC: HDC;
      ScreenLogPixels: Integer;
  begin
    var Metafile:= TMetafile.Create;
    try
      DC:= GetDC(0);
      ScreenLogPixels:= GetDeviceCaps(DC, LOGPIXELSY);
      Metafile.Inch:= ScreenLogPixels;
      Metafile.Width:= Bitmap.Width + 20;
      Metafile.Height:= Bitmap.Height + 20;
      MetafileCanvas:= TMetafileCanvas.Create(Metafile, DC);
      ReleaseDC(0, DC);
      try
        MetafileCanvas.Draw(0, 0, Bitmap);
      finally
        FreeAndNil(MetafileCanvas);
      end;
      Metafile.Enhanced:= false;
      Metafile.SaveToFile(filename);
    finally
      Metafile.Destroy;
    end;
  end;

begin
  with SDSaveAs do begin
    Title:= _(LNGExportTo);
    Filter:= 'PNG (*.png)|*.png|BMP (*.bmp)|*.bmp|WMF (*.wmf)|*.wmf|';
    folder:= ExtractFilePath(Pathname);
    if folder <> ''
      then InitialDir:= folder
      else InitialDir:= FConfiguration.Sourcepath;
    filename:= ChangeFileExt(Pathname, '');
    if Execute then begin
      if ExtractFileExt(filename) = '' then
        case FilterIndex of
          1: filename:= filename + '.png';
          2: filename:= filename + '.bmp';
          3: filename:= filename + '.wmf';
        end;
      if not FileExists(filename) or FileExists(filename) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [filename]),
                       mtConfirmation, mbYesNoCancel,0) = mrYes)
      then begin
        ext:= LowerCase(ExtractFileExt(filename));
        if ext = '.png' then InToPng(filename) else
        if ext = '.bmp' then InToBmp(filename) else
        if ext = '.wmf' then inToWmf(filename);
      end;
      FConfiguration.Sourcepath:= ExtractFilePath(filename);
    end;
  end;
end;

procedure TFJava.CloseBrowser;
begin
  if assigned(ActiveTDIChild) and (ActiveTDIChild.FormTag = 1) then
    if TFEditForm(ActiveTDIChild).Editor.CanFocus then
      TFEditForm(ActiveTDIChild).Editor.SetFocus;
  UpdateMenuItems(Self);
end;

procedure TFJava.CloseTimerTimer(Sender: TObject);
begin
  CloseTimer.Enabled:= false;
  DisableUpdateMenuItems;
  Close;
end;

procedure TFJava.ChangeLanguage(LangCode: string);
begin
  if CompareText(GetCurrentLanguage, LangCode) <> 0 then begin
    UseLanguage(LangCode);
    SetMenuKeyCaps;
    // otherwise the width of the main menu items will be miscalculated
    //MainMenu.Images:= nil;
    RetranslateComponent(Self);
    //MainMenu.Images:= vilMenuLight;
    for var i:= 0 to TDIFormsList.Count - 1 do
      if TDIFormsList[i] is TFUMLForm
        then (TDIFormsList[i] as TFUMLForm).Retranslate
        else RetranslateComponent(TDIFormsList[i]);
    RetranslateComponent(FConfiguration);
    RetranslateComponent(FGUIDesigner);
    RetranslateComponent(FObjectGenerator);
    RetranslateComponent(FWatches);
    RetranslateComponent(FMessages);
    RetranslateComponent(FTooltip);

    if assigned(FFileStructure) then
      RetranslateComponent(FFileStructure);
    if FConfiguration.GitOK and assigned(FGit) then
      RetranslateComponent(FGit);
    if assigned(FJUnitTests) then
      RetranslateComponent(FJUnitTests);
    if assigned(FObjectInspector) then
      RetranslateComponent(FObjectInspector);
    if FConfiguration.SubversionOK and assigned(FSubversion) then
      RetranslateComponent(FSubversion);
  end;
  FConfiguration.ShowDefaultMindstormsAndroidConfiguration;
  FConfiguration.InitTreeView;
  // Workaround: "Brackets" is always shown as "Klammern"
  FConfiguration.rgColors.Items.Text:= 'Java'#13#10'HTML'#13#10 + _('Brackets') + #13#10;
end;

procedure TFJava.SetMenuKeyCaps;
begin
  MenuKeyCaps[mkcBkSp]:= _('BkSp');
  MenuKeyCaps[mkcTab]:= _('Tab');
  MenuKeyCaps[mkcEsc]:= _('Esc');
  MenuKeyCaps[mkcEnter]:= _('Enter');
  MenuKeyCaps[mkcSpace]:= _('Space');
  MenuKeyCaps[mkcPgUp]:= _('PgUp');
  MenuKeyCaps[mkcPgDn]:= _('PgDn');
  MenuKeyCaps[mkcEnd]:= _('End');
  MenuKeyCaps[mkcHome]:= _('Home');
  MenuKeyCaps[mkcLeft]:= _('Left');
  MenuKeyCaps[mkcUp]:= _('Up');
  MenuKeyCaps[mkcRight]:= _('Right');
  MenuKeyCaps[mkcDown]:= _('Down');
  MenuKeyCaps[mkcIns]:= _('Ins');
  MenuKeyCaps[mkcDel]:= _('Del');
  MenuKeyCaps[mkcBkSp]:= _('BkSp');
  MenuKeyCaps[mkcShift]:= _('Shift+');
  MenuKeyCaps[mkcCtrl]:= _('Ctrl+');
  MenuKeyCaps[mkcAlt]:= _('Alt+');
end;

procedure TFJava.ShowAWTSwingOrFX(FrameType: integer);
  var i, api: integer; TC: TSpTBXTabControl;
begin
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

{--- Debugging ----------------------------------------------------------------}

//var aIdle: integer;

procedure TFJava.AppOnIdle(Sender: TObject; var Done: boolean);
//   var freq, startTime, endTime: Int64;
   var s: string;
begin
  if FConfiguration.FixImports then
    s:= 'FixImpors';
  if assigned(ActiveTDIChild) then begin
    s:= 'Java-Editor - ' + ActiveTDIChild.Pathname;
    if ActiveTDIChild.Modified then
      s:= s + '*';
  end else
    s:= 'Java-Editor';
  if Caption <> s then
    Caption:= s;

  if not assigned(EditorForm) then
    EditorForm:= getActiveEditor;

  if Assigned(EditorForm) then
    EditorForm.DoOnIdle;

  if assigned(EditorForm) and Editorform.isJava and EditorForm.NeedsParsing and
     ((Editorform.ParseThread = nil) or (EditorForm.ParseThread.state <> 2)) then
  begin
    EditorForm.ParseSourceCodeWithThread(false);
  end;

  if assigned(EditorForm) and Editorform.isJava and not EditorForm.NeedsParsing and
     assigned(EditorForm.ParseThread) and (EditorForm.ParseThread.state = 3) then
  begin
    EditorForm.CreateTVFileStructure;
    if assigned(EditorForm.ParseThread) then
      EditorForm.ParseThread.state:= 0;
  end;
  Done:= true;
end;

procedure TFJava.AppOnMessage(var Msg: TMsg; var Handled: Boolean);
  var s: string; aControl : TWinControl;
begin
  s:=  Screen.ActiveCustomForm.Name;
  aControl := Screen.ActiveControl; // or Application.ActiveControl (sorry, don't remember)
  while Assigned(aControl) and not (aControl is TForm) do aControl := aControl.Parent;
end;

procedure TFJava.ActiveControlChanged(Sender: TObject);
begin
  FMessages.OutputToTerminal(Screen.ActiveCustomForm.Name + ' - ' + Screen.ActiveControl.Name);
end;

procedure TFJava.ActiveWindowTimerTimer(Sender: TObject);
  var s, s1: string; aForm: TFForm;

begin
  s:= 'ActiveForm: ';
  aForm:= getActiveForm;
  if assigned(aForm)
    then s:= s + aForm.Pathname
    else s:= s + 'nil';

  s:= s +  ' ActiveEditor: ';
  aForm:= getActiveEditor;
  if assigned(aForm)
    then s:= s + aForm.Pathname
    else s:= s + 'nil';

{  n:= 0;
  for i:= 0 to TDIFormsList.count - 1 do begin
    aForm:= TDIFormsList[i];
    if not (aForm is TFForm) then
      ErrorMsg('TDIFormsList ist kein Formular!');
    if assigned(aForm) and (aForm is TFGUIForm) then
      inc(n);
  end;
 }

  {
    if assigned(FMessages) then
    Label1.Caption:=IntTostr(FMessages.ActiveSubTool);
  }

  s:= s + ' ActiveFormtag: '  + IntToStr(GetActiveFormTag) +  s1;

  if assigned(Screen.ActiveForm)
    then s:= 'ActiveCustomForm: ' + Screen.ActiveCustomForm.Name
    else s:= 'no ActiveForm';
  //Label1.Caption:= s;

  if assigned(Screen.ActiveControl) and assigned(Screen.ActiveControl.Parent)
    then s:= s +' - # ' +Screen.ActiveControl.Parent.Name
    else s:= s + ' - ' + 'kein Parent';
  //Caption:= s;

   if assigned(ActiveTDIChild) and assigned(ActiveTDIChild.ActiveControl) then
       s:= s + ' ActiveControl ' + ActiveTDIChild.ActiveControl.Name;
  // caption:= s;
   {if assigned(Application.Mainform.ActiveControl) then
     s:= s + ' ActiveChild: ' + Application.Mainform.ActiveControl.ClassName
   else
     s:= 'ActiveTDIChild: ' + 'nil';
   Caption:= s; }


   {
   if assigned(EditorForm)
     then s:= s + ' ' + ExtractFilename(Editorform.pathname)
     else s:= s + ' nil';
   Label1.Caption:= s;
    }


  {
   if assigned(Screen.ActiveForm)
     then Label2.Caption:= 'ActiveTool: ' + IntTostr(ActiveTool);
  }

  {
   if assigned(ActiveTDIChild)
     then Label3.caption:= 'ActiveTDIChild.Caption: ' + ActiveTDIChild.Caption
     else Label3.Caption:= 'ActiveTDIChild.Caption: ' + 'nil';
  Label1.Caption:= 'ActiveTool: ' + IntTostr(ActiveTool);
   }
 {

  if ActiveTDIChild.Active
    then s:= 'Activ'
    else s:= 'not activ';
  if ActiveTDIChild.CanFocus
    then s:= s + '  canFocus '
    else s:= s + '  not canFocus ';

  if (ActiveTDIChild.FormTag = 2) then
    with (ActiveTDIChild as TFUMLForm) do begin
      // s:= '';
      if MainModul.Diagram.GetPanel.FisMoving then s:= s + ' isMoving';
      if MainModul.Diagram.GetPanel.FIsRectSelecting then s:= s + ' IsRectSelecting';
     // if MainModul.Diagram.GetPanel.FIsConnecting then s:= s + ' IsConnecting';
      if MainModul.Diagram.GetPanel.FSelectedOnly then s:= s + ' SelectedOnly';
      if MainModul.Diagram.GetPanel.MouseDownOK then s:= s + ' MouseDownOK';
     end;

  Label4.Caption:= 'ActiveForm.Status: ' + s;

  s:= intTostr(GetActiveFormTag);
  if assigned(ActiveTDIChild) then s:= s + ' - ' + IntToStr(ActiveTDIChild.Formtag);
  Label5.Caption:= 'ActiveTDIFormTag - .Formtag: ' + s;
  ShowActive;
  }

end;

procedure TFJava.ShowActive;
var
  S: string;
  H: HWND;
  C: TControl;
  B: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  if Assigned(Screen.ActiveCustomForm) then
    S := 'CustomForm: '+ Screen.ActiveCustomForm.ClassName + '.' + Screen.ActiveCustomForm.Name + ' ';
  if Assigned(Screen.ActiveControl) then
    S := S + ' - ActiveControl: ' + Screen.ActiveControl.ClassName + '.' + Screen.ActiveControl.Name + ' ';
  H := GetActiveWindow;
  C := FindControl(H);
  if not Assigned(C) then begin
    if H <> 0 then begin
      GetClassName(H, @B, MAX_PATH);
      S := S + ' - ActiveWindow (' + B + ' ';
    end else
      S:= S + ' - ActiveWindow ()';
  end else
    S := S + '(' + C.ClassName + '.' + C.Name + ' ';
  H := GetFocus;
  C := FindControl(H);
  if not Assigned(C) then begin
    if h <> 0 then begin
      GetClassName(H, @B, MAX_PATH);
      S := S + B + ') ';
    end else
      S:= S + ' - Focus: ()';
  end else
    S := S + ' - Focus: ' + C.ClassName + '.' + C.Name + ') ';

  Application.Title := S;
  for i := 0 to Screen.CustomFormCount - 1 do
    Screen.CustomForms[i].Caption := S;
end;


end.

