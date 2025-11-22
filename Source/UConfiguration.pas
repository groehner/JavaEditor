unit UConfiguration;
{

  We have configuration-Values
  a) in the registry or in ini-files (JEMachine.INI, JEUser.INI)
  b) in variables (Model)
  c) in form (View)

  #           RegistryToModel               ModelToView
  registry  ----------------->  model      -------------> view

  INI-files <-----------------  variables  <------------- gui-elements
  #            ModelToRegistry               ViewToModel

}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ComCtrls,
  Registry,
  ExtCtrls,
  IniFiles,
  System.Generics.Collections,
  SynEditHighlighter,
  SynHighlighterHtml,
  SynHighlighterJava,
  SynHighlighterMulti,
  SynHighlighterPHP,
  SynHighlighterCSS,
  SynHighlighterPas,
  SynHighlighterGeneral,
  UTree,
  UStyles,
  SpTBXItem,
  ULLMSupport;

const
  CrLf = #13#10;
  Homepage = 'https://www.javaeditor.org';
  MaxTab = 8;
  MaxTabItem = 22;

type
  TBoolArray = array of Boolean;

  TStringListArray = array [1 .. 21] of TStringList;
  TStringArray = array [1 .. 12] of string;
  TVisibilityArray = array [0 .. MaxTab, 0 .. MaxTabItem] of Boolean;

  TEditStyleHookColor = class(TEditStyleHook) // TEditStyleHook in StdCtrls
  private
    procedure UpdateColors;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TWinControlH = class(TWinControl);

  TFConfiguration = class(TForm)
    PMain: TPanel;
    PButtons: TPanel;
    BSave: TButton;
    BCancel: TButton;
    BDump: TButton;
    BCheck: TButton;
    PInterpreter: TTabSheet;
    BHelp: TButton;
    LJavaInterpreter: TLabel;
    LInterpreterParameter: TLabel;
    LClasspathUser: TLabel;
    LClasspathAdmin: TLabel;
    EInterpreterParameter: TEdit;
    EInterpreter: TEdit;
    EClasspathUser: TEdit;
    EClasspathAdmin: TEdit;
    BJDKInstall: TButton;
    BInterpreterParameter: TButton;
    BInterpreter: TButton;
    BClasspathUser: TButton;
    BClasspathAdmin: TButton;
    PCompiler: TTabSheet;
    EJavaCompilerParameter: TEdit;
    BJavaParameter: TButton;
    BJavaCompiler: TButton;
    EJavaCompiler: TEdit;
    LJavaCompilerParameter: TLabel;
    LJavaCompiler: TLabel;
    PPrograms: TTabSheet;
    BJavaDocParameter: TButton;
    EDocParameter: TEdit;
    BJavaDoc: TButton;
    EJavaDoc: TEdit;
    BDebugger: TButton;
    EDebugger: TEdit;
    LJavaDocParameter: TLabel;
    LJavaDoc: TLabel;
    LDebugger: TLabel;
    PDisassembler: TTabSheet;
    BDisassemblerParameter: TButton;
    BDisassemblerInstall: TButton;
    EDisassemblerParameter: TEdit;
    LDissasemblerParameter: TLabel;
    LDisassembler: TLabel;
    PJar: TTabSheet;
    BJarFiles: TButton;
    EJarCreate: TEdit;
    BManifest: TButton;
    EJarManifest: TEdit;
    EJarParameter: TEdit;
    BJarParameter: TButton;
    BJar: TButton;
    EJar: TEdit;
    LJarCreate: TLabel;
    LJarManifest: TLabel;
    LJarParameter: TLabel;
    LJar: TLabel;
    PCheckStyle: TTabSheet;
    BCheckstyleConfiguration: TButton;
    ECheckstyleConfiguration: TEdit;
    ECheckstyleParameter: TEdit;
    BCheckstyleInstall: TButton;
    ECheckstyle: TEdit;
    LCheckstyleConfiguration: TLabel;
    LCheckstyleParameter: TLabel;
    LCheckstyle: TLabel;
    PTemplates: TTabSheet;
    BTemplateJApplet: TButton;
    ETemplateJApplet: TEdit;
    BTemplateControlstructure: TButton;
    ETemplateControlstructure: TEdit;
    BTemplateApplet: TButton;
    ETemplateApplet: TEdit;
    BTemplateJFrame: TButton;
    ETemplateJFrame: TEdit;
    BTemplateFrame: TButton;
    ETemplateFrame: TEdit;
    BTemplateConsole: TButton;
    ETemplateConsole: TEdit;
    LTemplateJApplet: TLabel;
    LTemplateControlstructure: TLabel;
    LTemplateHint2: TLabel;
    LTemplateHint1: TLabel;
    LTemplateHint: TLabel;
    LTemplateApplet: TLabel;
    LTemplateJFrame: TLabel;
    LTemplateFrame: TLabel;
    LTemplateConsole: TLabel;
    PDocumentation: TTabSheet;
    CBManual: TComboBox;
    BCache: TButton;
    ECache: TEdit;
    BJavabookInstall: TButton;
    EJavabook: TEdit;
    BTutorialInstall: TButton;
    ETutorial: TEdit;
    BManualInstall: TButton;
    LCache: TLabel;
    LSearchAgainFiles: TLabel;
    LSearchAgain: TLabel;
    LJavabook: TLabel;
    LTutorial: TLabel;
    LManual: TLabel;
    PEditorOptions: TTabSheet;
    CBCursorBehindLine: TCheckBox;
    CBShowBracketpair: TCheckBox;
    CBCompleteBracket: TCheckBox;
    CBLineNumbering: TCheckBox;
    CBIndentHelp: TCheckBox;
    CBAutomaticIndent: TCheckBox;
    LEditorIndent: TLabel;
    LEditorTabWidth: TLabel;
    PCode: TTabSheet;
    PColors: TTabSheet;
    BDefaultColors: TButton;
    GBTextattribute: TGroupBox;
    CBBold: TCheckBox;
    CBItalic: TCheckBox;
    CBUnderline: TCheckBox;
    LBColorElements: TListBox;
    LTextattribute: TLabel;
    LColor: TLabel;
    LColorElement: TLabel;
    PBrowser: TTabSheet;
    CBOpenBrowserShortcut: TComboBox;
    CBOnlyOneBrowserWindow: TCheckBox;
    BSelectBrowser: TButton;
    EBrowserProgram: TEdit;
    CBUseIEinternForDocuments: TCheckBox;
    BBrowserTitle: TButton;
    EBrowserTitle: TEdit;
    LAltKeysBrowser: TLabel;
    LBrowser: TLabel;
    LBrowserTitle: TLabel;
    PPrinter: TTabSheet;
    CBLinenumbersInBorder: TCheckBox;
    CBLinenumbers: TCheckBox;
    CBPrintColored: TCheckBox;
    CBFooter: TComboBox;
    CBHeader: TComboBox;
    RGAdjustment: TRadioGroup;
    EHeader: TEdit;
    EFooter: TEdit;
    EBorderBottom: TEdit;
    EBorderTop: TEdit;
    EBorderRight: TEdit;
    EBorderLeft: TEdit;
    LFooterMacro: TLabel;
    LHeaderMacro: TLabel;
    LFooter: TLabel;
    LHeader: TLabel;
    LBorderBottom: TLabel;
    LBorderTop: TLabel;
    LBorderRight: TLabel;
    LBorderLeft: TLabel;
    LBorder: TLabel;
    PComment: TTabSheet;
    EAuthor: TEdit;
    RGComment: TRadioGroup;
    MComment: TMemo;
    LAuthor: TLabel;
    LComment: TLabel;
    PKeyboard: TTabSheet;
    BKeyboardFile: TButton;
    EKeyboardFile: TEdit;
    MKeyboard: TMemo;
    LKeyboardFromFile: TLabel;
    PGeneralOptions: TTabSheet;
    CBDebuggerProtocol: TCheckBox;
    CBRenameWhenSave: TCheckBox;
    CBUseInterpreterWindowAsConsole: TCheckBox;
    CBLoadFiles: TCheckBox;
    CBBAKFiles: TCheckBox;
    LFontsize: TLabel;
    PMindstorms: TTabSheet;
    RGMindstormsVersion: TRadioGroup;
    BMindstormsParameter: TButton;
    EMindstormsParameter: TEdit;
    BMindstormsTemplate: TButton;
    EMindstormsTemplate: TEdit;
    BMindstormsManual: TButton;
    EMindstormsManual: TEdit;
    CBMindstormsModus: TCheckBox;
    CBMindstormsPort: TComboBox;
    BLejosInstall: TButton;
    ELejosFolder: TEdit;
    LMindstormsParameter: TLabel;
    LMindstormsTemplate: TLabel;
    LLejosManual: TLabel;
    LMindstormsPort: TLabel;
    LLejos: TLabel;
    PJalopy: TTabSheet;
    EJalopyParameter: TEdit;
    BJalopyConfiguration: TButton;
    EJalopyConfiguration: TEdit;
    BJalopyInstall: TButton;
    EJalopy: TEdit;
    LJalopyParameter: TLabel;
    LJalopyConfiguration: TLabel;
    LJalopy: TLabel;
    PSubversion: TTabSheet;
    CBRepository: TComboBox;
    BRepository: TButton;
    BSVN: TButton;
    ESVNFolder: TEdit;
    LRepository: TLabel;
    LSVN: TLabel;
    PTitle: TPanel;
    LTitle: TLabel;
    LTempFolder: TLabel;
    ETempFolder: TEdit;
    BTempFolder: TButton;
    PApplets: TTabSheet;
    LAppletviewer: TLabel;
    EAppletviewer: TEdit;
    BAppletviewer: TButton;
    RGApplet: TRadioGroup;
    GBProxy: TGroupBox;
    CBUseProxy: TCheckBox;
    LProxyIP: TLabel;
    EProxyIP: TEdit;
    LProxyPort: TLabel;
    EProxyPort: TEdit;
    RGColors: TRadioGroup;
    PLanguage: TTabSheet;
    CBDisassembler: TComboBox;
    ETabWidth: TEdit;
    EIndent: TEdit;
    FolderDialog: TFileOpenDialog;
    UDIndent: TUpDown;
    UDTabWidth: TUpDown;
    UDFontSize: TUpDown;
    EFontSize: TEdit;
    EMaxSearch: TEdit;
    UDMaxSearch: TUpDown;
    ETemplateDialog: TEdit;
    LTemplateDialog: TLabel;
    LTemplateJDialog: TLabel;
    ETemplateJDialog: TEdit;
    BTemplateDialog: TButton;
    BTemplateJDialog: TButton;
    PUML: TTabSheet;
    LObjectDesign: TLabel;
    RGObjectHead: TRadioGroup;
    CBObjectColor: TColorBox;
    RGObjectCaption: TRadioGroup;
    CBObjectUnderline: TCheckBox;
    LClassDesign: TLabel;
    RGClassHead: TRadioGroup;
    LValidClassColor: TLabel;
    CBValidClassColor: TColorBox;
    SBLejosSelect: TSpeedButton;
    SBMindstormsManual: TSpeedButton;
    SBMindstormsTemplate: TSpeedButton;
    SBTempSelect: TSpeedButton;
    SBManualSelect: TSpeedButton;
    SBTutorialSelect: TSpeedButton;
    SBJavabookSelect: TSpeedButton;
    SBCacheSelect: TSpeedButton;
    SBCheckStyleSelect: TSpeedButton;
    SBJalopySelect: TSpeedButton;
    LTemplateClass: TLabel;
    ETemplateClass: TEdit;
    BTemplateClass: TButton;
    LJarPack: TLabel;
    CBJarPack: TComboBox;
    EShadowWidth: TEdit;
    UDShadowWidth: TUpDown;
    LShadowWidth: TLabel;
    UDShadowIntensity: TUpDown;
    EShadowIntensity: TEdit;
    LShadowIntensity: TLabel;
    LInvalidClassColor: TLabel;
    CBInvalidClassColor: TColorBox;
    LShadow: TLabel;
    LObjectColor: TLabel;
    EFileFilter: TEdit;
    LFileFilter: TLabel;
    RGKeyboard: TRadioGroup;
    SBKeyboardfile: TSpeedButton;
    CBKeyboard: TComboBox;
    LCollision: TLabel;
    LKeyboard: TLabel;
    PAssociations: TTabSheet;
    LAssociations: TLabel;
    CBAssociationJava: TCheckBox;
    CBAssociationJfm: TCheckBox;
    CBAssociationHtml: TCheckBox;
    CBAssociationJep: TCheckBox;
    CBAssociationUml: TCheckBox;
    CBAssociationTxt: TCheckBox;
    CBAssociationJsp: TCheckBox;
    CBAssociationPhp: TCheckBox;
    CBAssociationCss: TCheckBox;
    CBAssociationJsg: TCheckBox;
    LAdditionalAssociations: TLabel;
    EAdditionalAssociations: TEdit;
    CBAssociationInc: TCheckBox;
    LAssociationsExample: TLabel;
    LFileFilterExample: TLabel;
    CBJDKFolder: TComboBox;
    SBJDKFolderSelect: TSpeedButton;
    CBShowHTMLforApplet: TCheckBox;
    SBDisassemblerSelect: TSpeedButton;
    BFileExtensions: TButton;
    RGObjectFooter: TRadioGroup;
    CBCheckAge: TCheckBox;
    SBLejosUploader: TSpeedButton;
    ELejosUploader: TEdit;
    LUploader: TLabel;
    LFlasher: TLabel;
    ELejosFlasher: TEdit;
    SBLejosFlasher: TSpeedButton;
    PVisibility: TTabSheet;
    LvisTabs: TLabel;
    LVVisibilityTabs: TListView;
    LVVisibilityElements: TListView;
    LVisItems: TLabel;
    LvisToolbars: TLabel;
    LVVisibilityToolbars: TListView;
    LvisMenus: TLabel;
    LVVisibilityMenus: TListView;
    BVisDefault: TButton;
    EFileHistory: TEdit;
    UDFileHistory: TUpDown;
    LFileHistory: TLabel;
    CBUseJavaCompilerInternally: TCheckBox;
    CBStrictJavaMode: TCheckBox;
    LJEAssociation: TLabel;
    EJEAssociation: TEdit;
    BJEAssociation: TButton;
    CBCommentClosingBrackets: TCheckBox;
    CBStructureColoring: TCheckBox;
    CBStructureColoringPlane: TCheckBox;
    CBInsertControlStructures: TCheckBox;
    CBInsertSemicolons: TCheckBox;
    PLogfiles: TTabSheet;
    LLogfilenames: TLabel;
    LLogfileCompiler: TLabel;
    ELogfileCompiler: TEdit;
    LLogfileExceptions: TLabel;
    ELogfileExceptions: TEdit;
    BLogfileCompiler: TButton;
    BLogfileExceptions: TButton;
    LIntensity: TLabel;
    EIntensity: TEdit;
    UDIntensity: TUpDown;
    BLogfileInteractive: TButton;
    ELogfileInteractive: TEdit;
    LLogfileInteractive: TLabel;
    LLejosCompiler: TLabel;
    ELejosCompiler: TEdit;
    SBLejosCompiler: TSpeedButton;
    CBShowCompilerCall: TCheckBox;
    PStructogram: TTabSheet;
    LInput: TLabel;
    EInput: TEdit;
    LOutput: TLabel;
    EOutput: TEdit;
    LAlgorithm: TLabel;
    EAlgorithm: TEdit;
    LWhile: TLabel;
    EWhile: TEdit;
    EDoWhile: TEdit;
    LDoWhile: TLabel;
    LFor: TLabel;
    EFor: TEdit;
    LYes: TLabel;
    EYes: TEdit;
    LNo: TLabel;
    ENo: TEdit;
    LCaseCount: TLabel;
    ECaseCount: TEdit;
    LOther: TLabel;
    EOther: TEdit;
    RGGenerateJavacode: TRadioGroup;
    LDatatype: TLabel;
    CBDataType: TComboBox;
    CBAcceptdefaultname: TCheckBox;
    LCommentDesign: TLabel;
    CBCommentColor: TColorBox;
    CBSwitchWithCaseLine: TCheckBox;
    UpDowncaseCount: TUpDown;
    PRestrictions: TTabSheet;
    CBDosWindow: TCheckBox;
    CBBlockedInternet: TCheckBox;
    CBLockedPaths: TCheckBox;
    LRestrictions: TLabel;
    LStructogramShadow: TLabel;
    EStructogramShadowWidth: TEdit;
    UDStructogramShadowWidth: TUpDown;
    LStructogramShadowWidth: TLabel;
    EStructogramShadowIntensity: TEdit;
    UDStructogramShadowIntensity: TUpDown;
    LStructogramShadowIntensity: TLabel;
    CBIndentafterBracket: TCheckBox;
    LJavaDocs: TLabel;
    EJavaDocs: TEdit;
    BJavaDocFolder: TButton;
    CBShowInterpreterCall: TCheckBox;
    LMindstormsIP: TLabel;
    EMindstormsIP: TEdit;
    LJarClasspath: TLabel;
    EJarClasspath: TEdit;
    BJarClasspath: TButton;
    GBCodeCompletion: TGroupBox;
    CBParameterHints: TCheckBox;
    LCompletionDelay: TLabel;
    TBDelay: TTrackBar;
    BFont: TButton;
    LCompletionHint1: TLabel;
    LCompletionHint2: TLabel;
    LCompletionMin: TLabel;
    LCompletionMax: TLabel;
    GBTooltips: TGroupBox;
    LTooltipsDelay: TLabel;
    TBTooltipDelay: TTrackBar;
    LToolTipMin: TLabel;
    LToolTipMax: TLabel;
    CBTooltipWithKey: TCheckBox;
    CBTooltipAutomatic: TCheckBox;
    LMethodComment: TLabel;
    MMethodComment: TMemo;
    CBShowClassObject: TCheckBox;
    CBTextColorBox: TColorBox;
    LTextColor: TLabel;
    LBackgroundColor: TLabel;
    CBBackgroundColorBox: TColorBox;
    CBNoBackgroundColor: TCheckBox;
    LActiveLineColor: TLabel;
    CBActiveLineColor: TColorBox;
    CBNoActiveLineColor: TCheckBox;
    LManualFX: TLabel;
    CBManualFX: TComboBox;
    SBManualFXSelect: TSpeedButton;
    BManualFXInstall: TButton;
    LTemplateapplication: TLabel;
    ETemplateApplication: TEdit;
    BTemplateApplication: TButton;
    PAndroid: TTabSheet;
    LAndroidSDK: TLabel;
    EAndroidSDKFolder: TEdit;
    SBAndroidSDKFolder: TSpeedButton;
    BAndroidSDKInstall: TButton;
    CBAndroidMode: TCheckBox;
    BAndroidAssemble: TButton;
    Memo1: TMemo;
    CBNoSyntaxHighlighting: TCheckBox;
    CBTranslateCompilerErrors: TCheckBox;
    PUMLOptions: TTabSheet;
    LAttributesAndMethods: TLabel;
    RGAttributsMethodsDisplay: TRadioGroup;
    RGSequenceAttributsMethods: TRadioGroup;
    RGParameterDisplay: TRadioGroup;
    RGVisibilityDisplay: TRadioGroup;
    LFor2: TLabel;
    GBClassPresentation: TGroupBox;
    CBShowEmptyRects: TCheckBox;
    CBUseVoid: TCheckBox;
    CBIntegerInsteadofInt: TCheckBox;
    CBStartWithDatatype: TCheckBox;
    CBShowClassparameterSeparately: TCheckBox;
    CBRoleHidesAttribute: TCheckBox;
    CBConstructorWithVisibility: TCheckBox;
    GBObjectPresentation: TGroupBox;
    CBShowObjectsWithMethods: TCheckBox;
    CBShowObjectsWithInheritedPrivateAttributes: TCheckBox;
    CBShowAllNewObjects: TCheckBox;
    CBLowerCaseLetter: TCheckBox;
    GBClassEditing: TGroupBox;
    CBOpenPublicClasses: TCheckBox;
    CBDefaultModifiers: TCheckBox;
    CBSetterWithoutThis: TCheckBox;
    GBObjectEditing: TGroupBox;
    CBUMLEdit: TCheckBox;
    CBShowFunctionvalues: TCheckBox;
    PGit: TTabSheet;
    LGitFolder: TLabel;
    EGitFolder: TEdit;
    LLocalRepository: TLabel;
    CBLocalRepository: TComboBox;
    BGitFolder: TButton;
    BGitRepository: TButton;
    LUserName: TLabel;
    LRemoteRepository: TLabel;
    CBRemoteRepository: TComboBox;
    EUserName: TEdit;
    LUserEMail: TLabel;
    EUserEMail: TEdit;
    BGitClone: TButton;
    RBCodeCompletionAlways: TRadioButton;
    RBCodeCompletionCtrlSpace: TRadioButton;
    LTemplateJUnit: TLabel;
    ETemplateJUnitTest: TEdit;
    BTemplateJUnitTest: TButton;
    PageList: TPageControl;
    PJUnit: TTabSheet;
    PJava: TTabSheet;
    PEditor: TTabSheet;
    PTools: TTabSheet;
    LJUnit: TLabel;
    EJUnitJarFile: TEdit;
    SBJUnit: TSpeedButton;
    BJUnitInstall: TButton;
    EJunitParameter: TEdit;
    LJUnitParameter: TLabel;
    CBJUnitBeforeEach: TCheckBox;
    CBJUnitAfterEach: TCheckBox;
    LJUnitManual: TLabel;
    EJUnitManual: TEdit;
    SBJUnitManual: TSpeedButton;
    LHeightInLines: TLabel;
    LHeigthInLinesMin: TLabel;
    LHeigthInLinesMax: TLabel;
    ESelectionSizeMin: TEdit;
    ESelectionSizeMax: TEdit;
    UpDownSelectionSizeMin: TUpDown;
    UpDownSelectionSizeMax: TUpDown;
    CBObjectsWithoutVisibility: TCheckBox;
    EJavaFXFolder: TEdit;
    BJavaFXFolder: TButton;
    LJavaFXFolder: TLabel;
    BJavaFXParameter: TButton;
    EJavaFXParameter: TEdit;
    LJavaFXParameter: TLabel;
    CBAssociationJSD: TCheckBox;
    PSequencediagram: TTabSheet;
    LSDObject: TLabel;
    ESDObject: TEdit;
    LSDNew: TLabel;
    ESDNew: TEdit;
    LSDClose: TLabel;
    ESDClose: TEdit;
    LSDFilling: TLabel;
    CBSDFillingColor: TColorBox;
    CBSDShowMainCall: TCheckBox;
    CBSDShowParameter: TCheckBox;
    CBSDShowReturn: TCheckBox;
    CBGUICodeFolding: TCheckBox;
    TVConfiguration: TTreeView;
    PPanelRight: TPanel;
    BRunJava: TButton;
    LSDKFolder: TLabel;
    CBRelationshipAttributesBold: TCheckBox;
    CB80Columnline: TCheckBox;
    CBLockedStructogram: TCheckBox;
    CBSDNoFilling: TCheckBox;
    LEditorStyle: TLabel;
    CBEditorStyles: TComboBox;
    BEditorStyleDefault: TButton;
    CBAttributesAParametersP: TCheckBox;
    CBUseAbstractForClass: TCheckBox;
    PStyles: TTabSheet;
    LNewStyle: TLabel;
    LBStyleNames: TListBox;
    StylesPreviewPanel: TPanel;
    LPreview: TLabel;
    LCurrentStyle: TLabel;
    ECurrentStyle: TEdit;
    LCompilerEncoding: TLabel;
    CBCompilerEncoding: TComboBox;
    GBConsoleOptions: TGroupBox;
    LFileEncoding: TLabel;
    LCodepage: TLabel;
    CBFileEncoding: TComboBox;
    CBCodepage: TComboBox;
    RGLanguages: TRadioGroup;
    CBArrayListAsIntegratedList: TCheckBox;
    PGUIDesigner: TTabSheet;
    CBNameFromText: TCheckBox;
    EGridSize: TEdit;
    UDGridSize: TUpDown;
    LGridSize: TLabel;
    CBGuiDesignerHints: TCheckBox;
    CBSnapToGrid: TCheckBox;
    EAppletwidth: TEdit;
    EAppletheight: TEdit;
    LAppletheight: TLabel;
    LAppletwidth: TLabel;
    EFrameheight: TEdit;
    LFrameheight: TLabel;
    EFrameWidth: TEdit;
    LFrameWidth: TLabel;
    BGuiFont: TButton;
    BEditorFont: TButton;
    BGuiFontDefault: TButton;
    LDialog12pt: TLabel;
    LDoesntShow: TLabel;
    LZoomsteps: TLabel;
    UDZoomSteps: TUpDown;
    EZoomSteps: TEdit;
    PLLMAssistant: TTabSheet;
    PLLMChat: TTabSheet;
    LProvider: TLabel;
    CBProvider: TComboBox;
    LEndpoint: TLabel;
    EEndPoint: TEdit;
    LModel: TLabel;
    EModel: TEdit;
    LAPIKey: TLabel;
    EAPIKey: TEdit;
    LSystemPrompt: TLabel;
    ESystemPrompt: TEdit;
    LMaxTokens: TLabel;
    EMaxTokens: TEdit;
    LLLMTimeout: TLabel;
    ELLMTimeout: TEdit;
    LChatProvider: TLabel;
    CBChatProvider: TComboBox;
    LChatEndPoint: TLabel;
    EChatEndPoint: TEdit;
    LChatModel: TLabel;
    EChatModel: TEdit;
    LChatApiKey: TLabel;
    EChatApiKey: TEdit;
    LChatSystemPrompt: TLabel;
    EChatSystemPrompt: TEdit;
    LChatMaxTokens: TLabel;
    EChatMaxTokens: TEdit;
    LChatTimeout: TLabel;
    EChatTimeout: TEdit;
    EChatTemperature: TEdit;
    LChatTemperature: TLabel;
    LLLMTemperature: TLabel;
    ELLMTemperature: TEdit;
    BLLMChatDefault: TButton;
    BLLMAssistantDefault: TButton;
    CBShowControlFlowSymbols: TCheckBox;
    CBShowLigatures: TCheckBox;
    CBCompactLineNumbers: TCheckBox;
    CBUseAbstractForMethods: TCheckBox;
    CBRunsUnderWine: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BJDKFolderSelectClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BBrowserTitleClick(Sender: TObject);
    procedure BJavaParameterClick(Sender: TObject);
    procedure BJavaCompilerClick(Sender: TObject);
    procedure BInterpreterClick(Sender: TObject);
    procedure BAppletviewerClick(Sender: TObject);
    procedure BManualInstallClick(Sender: TObject);
    procedure BDebuggerClick(Sender: TObject);
    procedure BDisassemblerInstallClick(Sender: TObject);
    procedure BJavaDocClick(Sender: TObject);
    procedure BTutorialInstallClick(Sender: TObject);
    procedure BJavabookInstallClick(Sender: TObject);
    procedure BInterpreterParameterClick(Sender: TObject);
    procedure BClasspathAdminClick(Sender: TObject);
    procedure LBColorElementsClick(Sender: TObject);
    procedure CBUseIEinternForDocumentsClick(Sender: TObject);
    procedure BSelectBrowserClick(Sender: TObject);
    procedure CBHeaderChange(Sender: TObject);
    procedure CBFooterChange(Sender: TObject);
    procedure RGCommentClick(Sender: TObject);
    procedure BTempFolderClick(Sender: TObject);
    procedure BJavaDocParameterClick(Sender: TObject);
    procedure CBKeyboardChange(Sender: TObject);
    procedure BKeyboardFileClick(Sender: TObject);
    procedure BClasspathUserClick(Sender: TObject);
    procedure BVorlageClick(Sender: TObject);
    procedure BDisassemblerParameterClick(Sender: TObject);
    procedure BJarClick(Sender: TObject);
    procedure BJarParameterClick(Sender: TObject);
    procedure BManifestClick(Sender: TObject);
    procedure BJarFilesClick(Sender: TObject);
    procedure BHelpClick(Sender: TObject);
    procedure BLejosInstallClick(Sender: TObject);
    procedure BMindstormsManualClick(Sender: TObject);
    procedure BCheckClick(Sender: TObject);
    procedure BDumpClick(Sender: TObject);
    procedure LMouseEnter(Sender: TObject);
    procedure LMouseLeave(Sender: TObject);
    procedure BCheckstyleInstallClick(Sender: TObject);
    procedure BCheckstyleConfigurationClick(Sender: TObject);
    procedure LJavaCompilerClick(Sender: TObject);
    procedure LJavaCompilerParameterClick(Sender: TObject);
    procedure LJavaInterpreterClick(Sender: TObject);
    procedure LInterpreterParameterClick(Sender: TObject);
    procedure LAppletviewerClick(Sender: TObject);
    procedure LDebuggerClick(Sender: TObject);
    procedure LDisassemblerClick(Sender: TObject);
    procedure LJavaDocClick(Sender: TObject);
    procedure LCheckstyleClick(Sender: TObject);
    procedure LJarClick(Sender: TObject);
    procedure LLejosClick(Sender: TObject);
    procedure BMindstormsParameterClick(Sender: TObject);
    procedure BJavaDocFolderClick(Sender: TObject);
    procedure LTutorialClick(Sender: TObject);
    procedure LJavabookClick(Sender: TObject);
    procedure BCacheClick(Sender: TObject);
    procedure BJalopyInstallClick(Sender: TObject);
    procedure BJalopyConfigurationClick(Sender: TObject);
    procedure LJalopyClick(Sender: TObject);
    procedure RGMindstormsVersionClick(Sender: TObject);
    procedure BSVNClick(Sender: TObject);
    procedure BRepositoryClick(Sender: TObject);
    procedure LSVNClick(Sender: TObject);
    procedure CBManualSelect(Sender: TObject);
    procedure BDefaultColorsClick(Sender: TObject);
    procedure BFontClick(Sender: TObject);
    procedure RGColorsClick(Sender: TObject);
    procedure CBDisassemblerSelect(Sender: TObject);
    procedure SBOpenClick(Sender: TObject);
    procedure RGKeyboardClick(Sender: TObject);
    procedure BJDKInstallClick(Sender: TObject);
    procedure CBJDKFolderSelect(Sender: TObject);
    procedure SBManualSelectClick(Sender: TObject);
    procedure SBTutorialSelectClick(Sender: TObject);
    procedure SBJavabookSelectClick(Sender: TObject);
    procedure SBLejosSelectClick(Sender: TObject);
    procedure SBJalopySelectClick(Sender: TObject);
    procedure SBCheckStyleSelectClick(Sender: TObject);
    procedure SBCacheSelectClick(Sender: TObject);
    procedure SBTempSelectClick(Sender: TObject);
    procedure SBDisassemblerSelectClick(Sender: TObject);
    procedure BFileExtensionsClick(Sender: TObject);
    procedure SBLejosUploaderClick(Sender: TObject);
    procedure SBLejosFlasherClick(Sender: TObject);
    procedure BVisDefaultClick(Sender: TObject);
    procedure BJEAssociationClick(Sender: TObject);
    procedure BLogfileCompilerClick(Sender: TObject);
    procedure BLogfileExceptionsClick(Sender: TObject);
    procedure BLogfileInteractiveClick(Sender: TObject);
    procedure SBLejosCompilerClick(Sender: TObject);
    procedure BJarClasspathClick(Sender: TObject);
    procedure CBColorBoxChange(Sender: TObject);
    procedure CBNoActiveLineColorClick(Sender: TObject);
    procedure CBManualFXSelect(Sender: TObject);
    procedure SBManualFXSelectClick(Sender: TObject);
    procedure BManualFXInstallClick(Sender: TObject);
    procedure BAndroidAssembleClick(Sender: TObject);
    procedure SBAndroidSDKFolderClick(Sender: TObject);
    procedure BAndroidSDKInstallClick(Sender: TObject);
    procedure LGitFolderClick(Sender: TObject);
    procedure BGitFolderClick(Sender: TObject);
    procedure BGitRepositoryClick(Sender: TObject);
    procedure BGitCloneClick(Sender: TObject);
    procedure LJUnitClick(Sender: TObject);
    procedure SBJUnitClick(Sender: TObject);
    procedure BJUnitInstallClick(Sender: TObject);
    procedure SBJUnitManualClick(Sender: TObject);
    procedure BJavaFXFolderClick(Sender: TObject);
    procedure BJavaFXParameterClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TVConfigurationChange(Sender: TObject; Node: TTreeNode);
    procedure BRunJavaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
    procedure BEditorStyleDefaultClick(Sender: TObject);
    procedure CBEditorStylesChange(Sender: TObject);
    procedure LBStyleNamesClick(Sender: TObject);
    procedure LVVisibilityTabsClick(Sender: TObject);
    procedure LVVisibilityElementsItemChecked(Sender: TObject; Item: TListItem);
    procedure BGuiFontClick(Sender: TObject);
    procedure BEditorFontClick(Sender: TObject);
    procedure BGuiFontDefaultClick(Sender: TObject);
    procedure CBProviderDropDown(Sender: TObject);
    procedure CBProviderSelect(Sender: TObject);
    procedure CBChatProviderDropDown(Sender: TObject);
    procedure CBChatProviderSelect(Sender: TObject);
    procedure BLLMChatDefaultClick(Sender: TObject);
    procedure BLLMAssistantDefaultClick(Sender: TObject);
  private
    FAllClasses: TStringList;
    FAllClasspathClasses: TStringList;
    FAllInterfaces: TStringList;
    FAllKeys: TStringList;
    FAllPackages: TStringList;
    FAttributesAParametersP: Boolean;
    FBottomDockPanelHeight: Integer;
    FDefaultModifiers: Boolean;
    FDiShowIcons: Integer;
    FDiShowParameter: Integer;
    FDiSortOrder: Integer;
    FDiVisibilityFilter: Integer;
    FDocumentationVersion: Integer;
    FDumpIniFileAllUsers: TMemIniFile;
    FDumpIniFileHKCU: TMemIniFile;
    FDumpIniFileHKLM: TMemIniFile;
    FDumpMode: Boolean;
    FEditFont: TFont;
    FEditorAndMenuShortcuts: TTree;
    FEditorFolder: string;
    FEditorKeys: TStringList;
    FMyRegistry: TRegistry;
    FPreview: TVclStylesPreview;
    FExternalStyleFilesDict: TDictionary<string, string>;
    FFirstStartAfterInstallation: Boolean;
    FFixImports: Boolean;
    FHomeDir: string;
    FTempDir: string;
    FTempDirWithUsername: string;
    FImportCache: TStringList;
    FJavaVersion: Integer;
    FKeyboardShortcutsTree: TTree;
    FLoadedStylesDict: TDictionary<string, string>;
    FLanguagesList: TStringList;
    FLNGTVItems: TStringList;
    FMachineIniFile: TMemIniFile;
    FMenuKeys: TStringList;
    FODSelect: TOpenDialog;
    FPortableApplication: Boolean;
    FPortAppDrive: string;
    FRightDockPanelWidth: Integer;
    FSavedClasspathUser: string;
    FSavedCompilerParameter: string;
    FSavedInterpreterParameter: string;
    FSavedJavaStartClass: string;
    FSequenceFont: TFont;
    FShowAlways: Boolean;
    FShowObjectsWithInheritedPrivateAttributes: Boolean;
    FShowObjectsWithMethods: Boolean;
    FShowPublicOnly: Boolean;
    FSourcepath: string;
    FStructogramFont: TFont;
    FVisSelectedTab: Integer;
    FTempProviders: TLLMProviders;
    FTempChatProviders: TLLMProviders;
    FUMLFont: TFont;
    FUseRegistry: Boolean;
    FUserIniFile: TIniFile;
    FWindowStateMaximized: Boolean;
    FWriteProtection: Boolean;
    FIndent1: string;
    FIndent2: string;
    FIndent3: string;

    // tab Interpreter
    FJDKFolder: string;
    FJDKFolderItems: string;
    FJavaFXFolder: string;
    FJavaFXParameter: string;
    FJavaInterpreter: string;
    FJavaInterpreterParameter: string;
    FCodepage: string;
    FFileEncoding: string;
    FJavaClasspathAdmin: string;
    FJavaClasspathAll: string;
    FJavaClasspathUser: string;
    FJavaStartClass: string;
    FJavaStartClassIsApplet: Boolean;
    FShowInterpreterCall: Boolean;

    // tab compiler
    FJavaCompiler: string;
    FJavaCompilerOK: Boolean;
    FJavaCompilerParameter: string;
    FShowCompilerCall: Boolean;
    FCompileInternally: Boolean;
    FCompilerEncoding: string;

    // tab programs
    FJarClassPath: string;
    FJarCreateAll: string;
    FJarCreateCurrent: string;
    FJarPackFiles: string;
    FJavaAppletviewerOK: Boolean;
    FJavaDebugger: string;
    FJavaDebuggerOK: Boolean;
    FJavaDisassembler: string;
    FJavaDisassemblerItems: string;
    FJavaDisassemblerParameter: string;
    FJavaDoc: string;
    FJavaDocOK: Boolean;
    FJavaDocParameter: string;
    FJavaJar: string;
    FJavaJarManifest: string;
    FJavaJarOK: Boolean;
    FJavaJarParameter: string;

    // tab applets
    FAppletStart: Integer;
    FJavaAppletviewer: string;
    FShowHTMLforApplet: Boolean;

    // tab Mindstorms
    FEV3Folder: string;
    FEV3Manual: string;
    FLejosCompiler: string;
    FLejosFlasher: string;
    FLejosUploader: string;
    FLejosVerzeichnis: string;
    FMindstormsDebug: Boolean;
    FMindstormsIP: string;
    FMindstormsManual: string;
    FMindstormsMode: Boolean;
    FMindstormsParameter: string;
    FMindstormsPort: Integer;
    FMindstormsRun: Boolean;
    FMindstormsVerbose: Boolean;
    FMindstormsVersion: Integer;
    FNXTCompiler: string;
    FNXTFlasher: string;
    FNXTFolder: string;
    FNXTManual: string;
    FNXTUploader: string;
    FRCXCompiler: string;
    FRCXFlasher: string;
    FRCXFolder: string;
    FRCXManual: string;
    FRCXUploader: string;

    // tab Android
    FAndroidMode: Boolean;
    FAndroidSDKFolder: string;

    // tab templates
    FControlStructureTemplates: TStringListArray;
    FTemplates: TStringArray;

    // tab keyboard
    FKeyboardFile: string;

    // tab documentation
    FJavaManual: string;
    FJavaManualFX: string;
    FJavaManualFXItems: string;
    FJavaManualItems: string;
    FCHMRootOk: Boolean;
    FJavabook: string;
    FJavaCHMRoot: string;
    FJavaJavaDocs: string;
    FJavaJavaDocsAll: string;
    FJavaTutorial: string;
    FJavaCache: string;
    FJavaCacheWithUsername: string;
    FJavaDemos: string;
    FJavaTools: string;
    FAllDocumentations: TStringList;
    FInformed: Boolean;
    FMaxSearch: Integer;

    // tab GUI Designer
    FNameFromText: Boolean;
    FSnapToGrid: Boolean;
    FGridSize: Integer;
    FGuiDesignerHints: Boolean;
    FAppletHeight: Integer;
    FAppletWidth: Integer;
    FFrameHeight: Integer;
    FFrameWidth: Integer;
    FGUIFontName: string;
    FGUIFontSize: Integer;
    FZoomSteps: Integer;

    // tab options
    FAcceptDefaultname: Boolean;
    FBorderLayout: Boolean;
    FCheckAge: Boolean;
    FComponentsToolbar: Boolean;
    FDebuggerProtocol: Boolean;
    FEditorStyle: string;
    FFileFilter: string;
    FFontSize: Integer;
    FMaxFileHistory: Integer;
    FRenameWhenSave: Boolean;
    FStrictJavaMode: Boolean;
    FTranslateCompilerErrors: Boolean;
    FUseInterpreterWindowAsConsole: Boolean;

    // tab restrictions
    FBlockedInternet: Boolean;
    FDOSWindowLocked: Boolean;
    FLockedPaths: Boolean;
    FLockedStructogram: Boolean;

    // tab Associations
    FAdditionalAssociations: string;

    // tab UML
    FClassHead: Integer;
    FCommentColor: Integer;
    FInvalidClassColor: Integer;
    FObjectCaption: Integer;
    FObjectColor: Integer;
    FObjectFooter: Integer;
    FObjectHead: Integer;
    FObjectUnderline: Boolean;
    FValidClassColor: Integer;

    // tab uml options
    FArrayListAsIntegratedList: Boolean;
    FConstructorWithVisibility: Boolean;
    FIntegerInsteadofInt: Boolean;
    FObjectLowerCaseLetter: Boolean;
    FObjectsWithoutVisibility: Boolean;
    FPrivateAttributEditable: Boolean;
    FRelationshipAttributesBold: Boolean;
    FRoleHidesAttribute: Boolean;
    FSetterWithoutThis: Boolean;
    FShadowIntensity: Integer;
    FShadowWidth: Integer;
    FShowAllNewObjects: Boolean;
    FShowClassparameterSeparately: Boolean;
    FShowEmptyRects: Boolean;
    FShowFunctionValues: Boolean;
    FStartWithDatatype: Boolean;
    FUseAbstractForClass: Boolean;
    FUseAbstractForMethods: Boolean;
    FUseVoid: Boolean;

    // tab editor
    FAddClosingBracket: Boolean;
    FAutomaticIndent: Boolean;
    FCommentClosingBrackets: Boolean;
    FCreateBAKFiles: Boolean;
    FRunsUnderWine: Boolean;
    FCursorBehindLine: Boolean;
    FEightyColumnLine: Boolean;
    FGUICodeFolding: Boolean;
    FIndent: Integer;
    FIndentAfterBracket: Boolean;
    FIndentHelp: Boolean;
    FInsertControlStructures: Boolean;
    FInsertSemicolons: Boolean;
    FLineNumbering: Boolean;
    FLoadFiles: Boolean;
    FShowBracketPair: Boolean;
    FStructureColoring: Boolean;
    FStructureColoringPlane: Boolean;
    FStructureColorIntensity: Integer;
    FTabWidth: Integer;
    FShowControlFlowSymbols: Boolean;
    FShowLigatures: Boolean;
    FCompactLineNumbers: Boolean;

    // tab code
    FCodeCompletionAlways: Boolean;
    FCodeCompletionCtrlSpace: Boolean;
    FCodeDelay: Integer;
    FParameterHints: Boolean;
    FSelectionSizeMax: Integer;
    FSelectionSizeMin: Integer;
    FShowClassObject: Boolean;
    FTooltipAutomatic: Boolean;
    FTooltipDelay: Integer;
    FTooltipFontSize: Integer;
    FTooltipHeight: Integer;
    FTooltipWidth: Integer;
    FTooltipWithKey: Boolean;

    // tab colors
    FActiveLineColor: TColor;
    FNoActiveLineColor: Boolean;
    FNoSyntaxHighlighting: Boolean;

    // tab checkstyle
    FCheckConfiguration: string;
    FCheckParameter: string;
    FCheckstyle: string;
    FCheckstyleOK: Boolean;

    // tab jalopy
    FJalopy: string;
    FJalopyConfiguration: string;
    FJalopyOK: Boolean;
    FJalopyParameter: string;

    // tab comment
    FCommentKind: Integer;
    FFreeComment: string;
    FJavaAuthor: string;
    FMethodComment: string;

    // tab printer
    FBorderBottom: Integer;
    FBorderLeft: Integer;
    FBorderRight: Integer;
    FBorderTop: Integer;
    FFooter: string;
    FHeader: string;
    FLinenumbersInMargin: Boolean;
    FPrintColored: Boolean;
    FWithLinenumbers: Boolean;

    // tab browser
    FBrowserOpenKeys: string;
    FBrowserProgram: string;
    FBrowserTitle: string;
    FOnlyOneBrowserWindow: Boolean;
    FProxyIP: string;
    FProxyPort: Integer;
    FUseIEinternForDocuments: Boolean;
    FWithProxy: Boolean;

    // tab colors
    FAttrBrackets: TSynHighlighterAttributes;

    // tab SVN
    FSubversionOK: Boolean;
    FSVNFolder: string;
    FSVNRepository: string;

    // tab Git
    FGitFolder: string;
    FGitLocalRepository: string;
    FGitOK: Boolean;
    FGitRemoteRepository: string;
    FGitUserEMail: string;
    FGitUserName: string;

    // tab junit
    FJUnitAfterEach: Boolean;
    FJUnitBeforeEach: Boolean;
    FJUnitJarFile: string;
    FJUnitManual: string;
    FJUnitOk: Boolean;
    FJUnitParameter: string;

    // tab Logfiles
    FLogfileCompiler: string;
    FLogfileCompilerOK: Boolean;
    FLogfileCompilerWithUserName: string;
    FLogfileExceptions: string;
    FLogfileExceptionsOK: Boolean;
    FLogfileExceptionsWithUsername: string;
    FLogfileInteractive: string;
    FLogfileInteractiveOK: Boolean;
    FLogfileInteractiveWithUsername: string;

    // tab sequence diagram
    FSDClose: string;
    FSDFillingcolor: TColor;
    FSDNew: string;
    FSDNoFilling: Boolean;
    FSDObject: string;
    FSDShowMainCall: Boolean;
    FSDShowParameter: Boolean;
    FSDShowReturn: Boolean;
    FSequencediagramS: string;

    // tab Structogram
    FAlgorithm: string;
    FCaseCount: Integer;
    FDoWhile: string;
    FGenerateJavaAsProgram: Integer;
    FInput: string;
    FNo: string;
    FOther: string;
    FOutput: string;
    FStructoDatatype: string;
    FStructogramS: string;
    FStructogramShadowIntensity: Integer;
    FStructogramShadowWidth: Integer;
    FSwitchWithCaseLine: Boolean;
    FYes: string;
    FFor: string;
    FWhile: string;

    // tab Language
    FLanguageCode: string;

    // highligthers
    FCSSHighlighter: TSynCssSyn;
    FEditorTextColor: TColor;
    FGeneralHighlighter: TSynGeneralSyn;
    FHTMLHighlighter: TSynHTMLSyn;
    FJavaHighlighter: TSynJavaSyn;
    FMultiSynHighlighter: TSynMultiSyn;
    FPascalHighlighter: TSynPasSyn;
    FPHPHighlighter: TSynMultiSyn;
    FPHPInternHighlighter: TSynPHPSyn;

    // tab Visibility
    FVis1: TVisibilityArray;
    FVis2: TVisibilityArray;
    FVisMenus: TBoolArray;
    FVisTabs: TBoolArray;
    FVisToolbars: TBoolArray;

    function GetWriteProtection: Boolean;
    function GetCheckColor(Str: string; EmptyAllowed: Boolean): TColor;
    function DirectoryFilesExists(Str: string): Boolean;
    procedure CheckFile(WinControl: TWinControl; EmptyAllowed: Boolean);
    procedure CheckFileWithoutShortenpath(WinControl: TWinControl;
      EmptyAllowed: Boolean);
    procedure CheckFolder(Edit: TEdit; EmptyAllowed: Boolean);
    procedure CheckFolders(Edit: TEdit);
    procedure CheckFolderCB(ComboBox: TComboBox);
    procedure CheckUserFolder(Edit: TEdit);
    procedure CheckUserFolders(Edit: TEdit);
    procedure CheckCBManual;
    procedure CheckCBManualFX;
    procedure SetJDKFolder(Dir: string);
    function JavaDevelopmentKit: string;
    procedure SetStartDir(const Dir, AFile, Filter: string);
    procedure RegisterJavaeditor;
    procedure RegistryForMachine;
    procedure RegistryForUser;
    function BrowserProgToName(const Str: string): string;
    procedure MenuAndEditorShortcuts;
    procedure CreateBrowserShortCuts;
    function LNGColornameToAttributIndex(const Str: string): Integer;
    procedure SetDefaultBracketColor;
    procedure StyleSelectorShow;
    procedure FillVclStylesList;
    procedure DecideProxy;
    function JavaDocComment(const Indent: string = ''): string;
    function ShortComment(const Indent: string = ''): string;
    function GetFileInCHM(Str: string): string;
    function GetJavaManualFX: string;
    procedure UpdateHeaderFooter;
    procedure ReadProviders(const Name: string; var Providers: TLLMProviders);
    procedure WriteProviders(const Name: string; Providers: TLLMProviders);
    procedure CopyProviders(From: TLLMProviders; var Toward: TLLMProviders);
    procedure LLMAssistantModelToView(Settings: TLLMSettings);
    procedure LLMChatModelToView(Settings: TLLMSettings);
    procedure LLMAssistantViewToModel;
    procedure LLMChatViewToModel;
    function LLMAssistantSettings: TLLMSettings;
    function LLMChatSettings: TLLMSettings;
  public
    class var GUIStyle: string;
    procedure Init;
    procedure RegistryToModel;
    procedure ModelToRegistry;
    procedure ShowDefaultMindstormsAndroidConfiguration;
    procedure MakeJavaCache(Str: string);
    procedure SaveInis;

    procedure SaveStrings(const Key, AName: string; Values: TStrings);
    procedure ReadStrings(const Key, AName: string; Values: TStrings);
    procedure SaveFavorites(Favorites: TStringList);
    procedure ReadFavorites(var Favorites: TStringList);
    procedure SaveUserColors;
    procedure ReadUserColors;
    function GetRegPath: string;

    procedure ModelToView;
    procedure ViewToModel;
    procedure SaveWindow;
    function StringToShortcut(Str: string): Integer;
    procedure RemoveShortcutFromMainMenu(ShortCut: Integer);
    procedure ReplaceShortcutFromMainMenu(ShortCut, ShortCut2: Integer);
    procedure ApplyKeyboardShortcuts;
    procedure RemoveShortcutsFrom(PopupMenu: TSpTBXPopupMenu);
    procedure CheckAllFilesAndFolders;
    procedure LockButtons;
    procedure DoHelp(const AFile: string);

    procedure WriteString(Dest: Integer; const Key, AName, Value: string);
    procedure WriteStringM(const Key, AName, Value: string);
    procedure WriteStringF(const Key, AName, Value: string);
    procedure WriteStringU(const Key, AName, Value: string);
    procedure WriteStringFile(const Key, AName, Value: string);
    procedure WriteStringDirectory(const Key, AName, Value: string);

    function ReadString(Dest: Integer;
      const Key, AName, Default: string): string;
    function ReadStringM(const Key, AName, Default: string): string;
    function ReadStringF(const Key, AName, Default: string): string;
    function ReadStringU(const Key, AName, Default: string): string;
    function ReadStringFile(const Key, AName, Default: string): string;
    function ReadStringDirectory(const Key, AName: string): string;

    procedure WriteIntegerU(const Key, AName: string; Value: Integer);
    function ReadIntegerU(const Key, AName: string; Default: Integer): Integer;

    procedure WriteBool(Machine: Boolean; const Key, AName: string;
      Value: Boolean);
    procedure WriteBoolM(const Key, AName: string; Value: Boolean);
    procedure WriteBoolU(const Key, AName: string; Value: Boolean);

    function ReadBool(Machine: Boolean; const Key, AName: string;
      Default: Boolean): Boolean;
    function ReadBoolM(const Key, AName: string; Default: Boolean): Boolean;
    function ReadBoolU(const Key, AName: string; Default: Boolean): Boolean;

    procedure WriteBinaryStreamU(const Key, AName: string; Value: TStream);
    procedure WriteBinaryStream(Machine: Boolean; const Key, AName: string;
      Value: TStream);
    function ReadBinaryStreamU(const Key, AName: string;
      Value: TStream): Integer;
    function ReadBinaryStream(Machine: Boolean; const Key, AName: string;
      Value: TStream): Integer;

    procedure MakeAssociations;
    function HeadText(const Indent: string = ''): string;
    function RemovePortableDrive(const Str: string;
      const Folder: string = ''): string;
    function AddPortableDrive(const Str: string; const Folder: string = ''): string;
    function AddPortableDrives(Str: string): string;
    function RemovePortableDrives(Str: string): string;
    function GetCHMJavaManual(const Str: string): string;
    function GetJavaManual: string;
    function SetRCXNXTEV3(const Str: string): string;
    function GetCharset: string;
    function GetJavaVersion: Integer;
    function GetDocumentationVersion: Integer;
    function GetEncoding: string; overload;
    function GetEncoding(const Pathname: string): TEncoding; overload;
    procedure ShowColorElements;
    procedure ReplaceClNone;
    procedure LoadUserColors;
    procedure LoadDefaultColors(Typ: Integer);
    function GetTVConfigurationItem(const Text: string): TTreeNode;
    procedure ShowPage(Page: Integer);
    procedure PrepareShow;
    function GetTargetDir(const Target: string): string;
    procedure FileSelect(Edit: TEdit; const Filter, Filename, Dir: string);
    procedure FileSelectWithoutShortenPath(Edit: TEdit;
      const Filter, Filename, Dir: string);
    procedure FileSelect2(Edit: TEdit; const Filter, Filename, Dir: string);
    function FolderSelect(Edit: TEdit; const Foldername: string): string;
    function GetFileFilters: string;
    function IsGerman: Boolean;
    function GetConfigurationAddress(const Str: string): string;
    function GetJavaCompilerParameter(const Pathname: string): string;
    function GetJavaInterpreterParameter(IsFX: Boolean): string;
    function GetClassPath: string; overload;
    function GetClassPath(const Pathname, Package: string): string; overload;
    function GetClassPathJarExpanded(const Pathname, Package: string): string;
    function GetPackageDirectorySecure(const Pathname: string;
      Package: string): string;
    function GetPackageDirectoryRelativ(const Pathname,
      Package: string): string;
    function GetPascalHighlighter: TSynPasSyn;
    function GetPhpHighlighter: TSynMultiSyn;
    function GetPHPInternHighlighter: TSynPHPSyn;
    function GetCSSHighlighter: TSynCssSyn;
    function GetMultiSynHighlighter: TSynMultiSyn;
    function GetGeneralHighlighter: TSynGeneralSyn;
    function GetHighlighter(const Pathname: string): TSynCustomHighlighter;
    function GetClasspathFromSourcepath(const AClassname,
      Sourcepath: string): string;
    function IsInClasspath(const AClassname: string; ActPath: string): Boolean;
    procedure MakeClassAndPackageList(const Classfile, Packagefile: string);
    procedure MakeClassAndPackageListFromDocumentation(const Classfile,
      InterfaceFile, Packagefile: string);
    procedure MakeSystemClasses;
    procedure MakeClasspathClasses;
    function ToStringListClass(const AClassname: string): string;
    function IsAPIInterface(var AClassname: string): Boolean;
    function IsAPIClass(var AClassname: string): Boolean;
    function IsAPIPackage(Packagename: string): Boolean;
    function IsAPIClassOrInterface(AClassname: string): Boolean;
    procedure CallUpdater(const Target, Source1: string;
      Source2: string); overload;
    procedure SetElevationRequiredState(AControl: TWinControl);

    procedure LoadVisibility;
    procedure SaveVisibility;
    procedure SetVisibility;
    procedure VisibilityModelToView;
    procedure VisibilityViewToModel;
    procedure PrepareVisibilityPage;

    function GetJarsFromClasspath: string;
    function UpdatePossible(const Source, Target: string): Boolean;
    function RunAsAdmin(Wnd: HWND; const AFile, Parameters: string): Boolean;
    procedure SetMindstormsVersion;
    procedure CheckMindstorms;
    function GetFrameType(JavaProgram: string;
      Startclass: Boolean = False): Integer;
    function GetPackage(JavaProgram: string;
      Startclass: Boolean = False): string;
    function GetCompileParameter(const Pathname, Package,
      Encoding: string): string;
    procedure SetPrinter(const Str: string);
    function GetDumpText: string;
    function PathForSystemClass(const Path: string): Boolean;
    function PathForUserClass(const Path: string): Boolean;
    function GetAppletArchiv: string;
    function HasAgeClass(const Pathname: string): Boolean;
    function HasClass(const Pathname: string): Boolean;
    procedure SaveConfigurationForProject;
    procedure RestoreConfigurationAfterProject;
    procedure MakeControlStructureTemplates;
    procedure ShortenPath(WinControl: TWinControl; const Str: string);
    function ExtendPath(WinControl: TWinControl): string;
    procedure ComboBoxAddEx(ComboBox: TComboBox);
    function SearchClassInClasspath(const AClassname, Sourcepath,
      Package: string): string;
    function SearchClassInDirectory(const AClassname, Sourcepath,
      Package: string): string;
    function IsInterface(const Pathname: string): Boolean;
    function GetCompleteClassname(FullImports, Classimports, UserImportClasses
      : TStringList; const AClassname: string): string;
    function TranslateCompilerError(const Err: string): string;
    procedure CollectDocumentations;
    function GetJavaTools(const Tool: string): string;
    procedure ReadEditorStyleNames;
    function ExplorerTest: Boolean;
    procedure OpenAndShowPage(const Page: string);
    procedure Log(const Str: string; E: Exception = nil);
    procedure SetupLanguages;
    procedure InitTreeView;
    function GlobalFileExists(var Filename: string;
      WithoutChange: Boolean = True): Boolean;
    function ExtractZipToDir(const Filename, Dir: string): Boolean;

    class procedure SetGUIStyle;
    class procedure LoadGUIStyle(const Style: string);
    class function IsDark: Boolean;

    property Sourcepath: string read FSourcepath write FSourcepath;
    property EditorFolder: string read FEditorFolder;
    property HomeDir: string read FHomeDir;
    property TempDir: string read FTempDir;
    property ODSelect: TOpenDialog read FODSelect;
    property AllPackages: TStringList read FAllPackages;
    property AllClasses: TStringList read FAllClasses;
    property AllClasspathClasses: TStringList read FAllClasspathClasses;
    property AllInterfaces: TStringList read FAllInterfaces;
    property AttributesAParametersP: Boolean read FAttributesAParametersP;
    property DiShowIcons: Integer read FDiShowIcons write FDiShowIcons;
    property DiShowParameter: Integer read FDiShowParameter
      write FDiShowParameter;
    property DiSortOrder: Integer read FDiSortOrder;
    property DiVisibilityFilter: Integer read FDiVisibilityFilter
      write FDiVisibilityFilter;
    property KeyboardShortcutsTree: TTree read FKeyboardShortcutsTree;
    property FirstStartAfterInstallation: Boolean
      read FFirstStartAfterInstallation;
    property EditFont: TFont read FEditFont;
    property SequenceFont: TFont read FSequenceFont;
    property StructogramFont: TFont read FStructogramFont;
    property UMLFont: TFont read FUMLFont;
    property DefaultModifiers: Boolean read FDefaultModifiers;
    property FixImports: Boolean read FFixImports write FFixImports;
    property ShowPublicOnly: Boolean read FShowPublicOnly write FShowPublicOnly;
    property ShowObjectsWithInheritedPrivateAttributes: Boolean
      read FShowObjectsWithInheritedPrivateAttributes;
    property ShowObjectsWithMethods: Boolean read FShowObjectsWithMethods;
    property ShowAlways: Boolean read FShowAlways write FShowAlways;
    property ImportCache: TStringList read FImportCache;
    property WindowStateMaximized: Boolean read FWindowStateMaximized
      write FWindowStateMaximized;
    property BottomDockPanelHeight: Integer read FBottomDockPanelHeight
      write FBottomDockPanelHeight;
    property RightDockPanelWidth: Integer read FRightDockPanelWidth
      write FRightDockPanelWidth;
    property Indent1: string read FIndent1;
    property Indent2: string read FIndent2;
    property Indent3: string read FIndent3;

    // highlighters
    property JavaHighlighter: TSynJavaSyn read FJavaHighlighter;
    property HTMLHighlighter: TSynHTMLSyn read FHTMLHighlighter;

    // tab interpreter
    property JDKFolder: string read FJDKFolder;
    property JavaInterpreter: string read FJavaInterpreter
      write FJavaInterpreter;
    property JavaInterpreterParameter: string read FJavaInterpreterParameter
      write FJavaInterpreterParameter;
    property JavaClasspathUser: string read FJavaClasspathUser
      write FJavaClasspathUser;
    property JavaClasspathAdmin: string read FJavaClasspathAdmin;
    property JavaStartClass: string read FJavaStartClass write FJavaStartClass;
    property JavaStartClassIsApplet: Boolean read FJavaStartClassIsApplet
      write FJavaStartClassIsApplet;
    property Codepage: string read FCodepage;
    property ShowInterpreterCall: Boolean read FShowInterpreterCall;

    // tab compiler
    property JavaCompiler: string read FJavaCompiler;
    property JavaCompilerOK: Boolean read FJavaCompilerOK;
    property JavaCompilerParameter: string read FJavaCompilerParameter
      write FJavaCompilerParameter;
    property CompileInternally: Boolean read FCompileInternally;
    property ShowCompilerCall: Boolean read FShowCompilerCall;

    // tab programs
    property JarClassPath: string read FJarClassPath;
    property JarCreateAll: string read FJarCreateAll;
    property JarCreateCurrent: string read FJarCreateCurrent;
    property JarPackFiles: string read FJarPackFiles;
    property JavaDisassembler: string read FJavaDisassembler;
    property JavaDisassemblerParameter: string read FJavaDisassemblerParameter;
    property JavaDocParameter: string read FJavaDocParameter;
    property JavaJar: string read FJavaJar;
    property JavaJarManifest: string read FJavaJarManifest;
    property JavaJarParameter: string read FJavaJarParameter;
    property JavaAppletviewerOK: Boolean read FJavaAppletviewerOK;
    property JavaDebugger: string read FJavaDebugger;
    property JavaDoc: string read FJavaDoc;
    property JavaDebuggerOK: Boolean read FJavaDebuggerOK;
    property JavaDocOK: Boolean read FJavaDocOK;
    property JavaJarOK: Boolean read FJavaJarOK;

    // tab applets
    property AppletStart: Integer read FAppletStart;
    property JavaAppletviewer: string read FJavaAppletviewer;
    property ShowHTMLforApplet: Boolean read FShowHTMLforApplet
      write FShowHTMLforApplet;

    // tab mindstorms
    property LejosVerzeichnis: string read FLejosVerzeichnis;
    property LejosCompiler: string read FLejosCompiler;
    property LejosUploader: string read FLejosUploader;
    property NXTUploader: string read FNXTUploader;
    property RCXFlasher: string read FRCXFlasher;
    property RCXUploader: string read FRCXUploader;
    property MindstormsManual: string read FMindstormsManual;
    property MindstormsDebug: Boolean read FMindstormsDebug
      write FMindstormsDebug;
    property MindstormsIP: string read FMindstormsIP;
    property MindstormsMode: Boolean read FMindstormsMode;
    property MindstormsParameter: string read FMindstormsParameter;
    property MindstormsRun: Boolean read FMindstormsRun write FMindstormsRun;
    property MindstormsVerbose: Boolean read FMindstormsVerbose
      write FMindstormsVerbose;
    property MindstormsVersion: Integer read FMindstormsVersion;

    // tab android
    property AndroidMode: Boolean read FAndroidMode;
    property AndroidSDKFolder: string read FAndroidSDKFolder;

    // tab templates
    property ControlStructureTemplates: TStringListArray
      read FControlStructureTemplates;
    property Templates: TStringArray read FTemplates;

    // tab keyboard
    property KeyboardFile: string read FKeyboardFile;

    // tab documentation
    property JavaManual: string read FJavaManual write FJavaManual;
    property JavaManualFX: string read FJavaManualFX;
    property CHMRootOk: Boolean read FCHMRootOk;
    property Javabook: string read FJavabook;
    property JavaJavaDocs: string read FJavaJavaDocs write FJavaJavaDocs;
    property JavaTutorial: string read FJavaTutorial write FJavaTutorial;
    property JavaCache: string read FJavaCache;
    property JavaDemos: string read FJavaDemos;
    property AllDocumentations: TStringList read FAllDocumentations;
    property Informed: Boolean read FInformed;
    property MaxSearch: Integer read FMaxSearch;

    // tab GUI designer
    property GridSize: Integer read FGridSize;
    property GuiDesignerHints: Boolean read FGuiDesignerHints;
    property NameFromText: Boolean read FNameFromText;
    property SnapToGrid: Boolean read FSnapToGrid;
    property FrameHeight: Integer read FFrameHeight;
    property FrameWidth: Integer read FFrameWidth;
    property GUIFontName: string read FGUIFontName;
    property GUIFontSize: Integer read FGUIFontSize;
    property ZoomSteps: Integer read FZoomSteps;

    // tab options
    property AcceptDefaultname: Boolean read FAcceptDefaultname;
    property CheckAge: Boolean read FCheckAge;
    property DebuggerProtocol: Boolean read FDebuggerProtocol;
    property FileFilter: string read FFileFilter;
    property FontSize: Integer read FFontSize;
    property MaxFileHistory: Integer read FMaxFileHistory;
    property RenameWhenSave: Boolean read FRenameWhenSave;
    property StrictJavaMode: Boolean read FStrictJavaMode;
    property TranslateCompilerErrors: Boolean read FTranslateCompilerErrors;
    property UseInterpreterWindowAsConsole: Boolean
      read FUseInterpreterWindowAsConsole;

    // tab restrictions
    property BlockedInternet: Boolean read FBlockedInternet;
    property DOSWindowLocked: Boolean read FDOSWindowLocked;
    property LockedStructogram: Boolean read FLockedStructogram;

    // tab uml
    property ClassHead: Integer read FClassHead;
    property CommentColor: Integer read FCommentColor;
    property InvalidClassColor: Integer read FInvalidClassColor;
    property ObjectCaption: Integer read FObjectCaption;
    property ObjectColor: Integer read FObjectColor;
    property ObjectFooter: Integer read FObjectFooter;
    property ObjectHead: Integer read FObjectHead;
    property ObjectUnderline: Boolean read FObjectUnderline;
    property ValidClassColor: Integer read FValidClassColor;

    // tab uml otions
    property ArrayListAsIntegratedList: Boolean read FArrayListAsIntegratedList;
    property ConstructorWithVisibility: Boolean read FConstructorWithVisibility;
    property IntegerInsteadofInt: Boolean read FIntegerInsteadofInt;
    property ObjectLowerCaseLetter: Boolean read FObjectLowerCaseLetter;
    property ObjectsWithoutVisibility: Boolean read FObjectsWithoutVisibility;
    property PrivateAttributEditable: Boolean read FPrivateAttributEditable;
    property RelationshipAttributesBold: Boolean
      read FRelationshipAttributesBold;
    property RoleHidesAttribute: Boolean read FRoleHidesAttribute;
    property SetterWithoutThis: Boolean read FSetterWithoutThis;
    property ShadowIntensity: Integer read FShadowIntensity;
    property ShadowWidth: Integer read FShadowWidth;
    property ShowAllNewObjects: Boolean read FShowAllNewObjects;
    property ShowClassparameterSeparately: Boolean
      read FShowClassparameterSeparately;
    property ShowEmptyRects: Boolean read FShowEmptyRects;
    property ShowFunctionValues: Boolean read FShowFunctionValues;
    property StartWithDatatype: Boolean read FStartWithDatatype;
    property UseAbstractForClass: Boolean read FUseAbstractForClass;
    property UseAbstractForMethods: Boolean read FUseAbstractForMethods;
    property UseVoid: Boolean read FUseVoid;

    // tab editor
    property AddClosingBracket: Boolean read FAddClosingBracket;
    property AutomaticIndent: Boolean read FAutomaticIndent;
    property CommentClosingBrackets: Boolean read FCommentClosingBrackets;
    property CreateBAKFiles: Boolean read FCreateBAKFiles;
    property RunsUnderWine: Boolean read FRunsUnderWine;
    property CursorBehindLine: Boolean read FCursorBehindLine;
    property EightyColumnLine: Boolean read FEightyColumnLine;
    property GUICodeFolding: Boolean read FGUICodeFolding;
    property Indent: Integer read FIndent;
    property IndentAfterBracket: Boolean read FIndentAfterBracket;
    property IndentHelp: Boolean read FIndentHelp;
    property InsertControlStructures: Boolean read FInsertControlStructures;
    property InsertSemicolons: Boolean read FInsertSemicolons;
    property LineNumbering: Boolean read FLineNumbering;
    property LoadFiles: Boolean read FLoadFiles;
    property ShowBracketPair: Boolean read FShowBracketPair;
    property StructureColoring: Boolean read FStructureColoring;
    property StructureColoringPlane: Boolean read FStructureColoringPlane;
    property StructureColorIntensity: Integer read FStructureColorIntensity;
    property TabWidth: Integer read FTabWidth;
    property ShowControlFlowSymbols: Boolean read FShowControlFlowSymbols;
    property ShowLigatures: Boolean read FShowLigatures;
    property CompactLineNumbers: Boolean read FCompactLineNumbers;

    // tab code
    property CodeCompletionAlways: Boolean read FCodeCompletionAlways;
    property CodeCompletionCtrlSpace: Boolean read FCodeCompletionCtrlSpace;
    property ParameterHints: Boolean read FParameterHints;
    property ShowClassObject: Boolean read FShowClassObject;
    property TooltipAutomatic: Boolean read FTooltipAutomatic;
    property TooltipDelay: Integer read FTooltipDelay;
    property TooltipFontSize: Integer read FTooltipFontSize
      write FTooltipFontSize;
    property TooltipHeight: Integer read FTooltipHeight write FTooltipHeight;
    property TooltipWidth: Integer read FTooltipWidth write FTooltipWidth;
    property TooltipWithKey: Boolean read FTooltipWithKey;

    // tab colors
    property ActiveLineColor: TColor read FActiveLineColor;
    property NoSyntaxHighlighting: Boolean read FNoSyntaxHighlighting;

    // tab checkstyle
    property CheckConfiguration: string read FCheckConfiguration;
    property CheckParameter: string read FCheckParameter;
    property Checkstyle: string read FCheckstyle;
    property CheckstyleOK: Boolean read FCheckstyleOK;

    // tab jalopy
    property Jalopy: string read FJalopy;
    property JalopyConfiguration: string read FJalopyConfiguration;
    property JalopyOK: Boolean read FJalopyOK;
    property JalopyParameter: string read FJalopyParameter;

    // tab comment
    property JavaAuthor: string read FJavaAuthor;
    property MethodComment: string read FMethodComment;

    // tab printer
    property BorderBottom: Integer read FBorderBottom;
    property BorderLeft: Integer read FBorderLeft;
    property BorderRight: Integer read FBorderRight;
    property BorderTop: Integer read FBorderTop;
    property Footer: string read FFooter;
    property Header: string read FHeader;
    property LinenumbersInMargin: Boolean read FLinenumbersInMargin;
    property PrintColored: Boolean read FPrintColored;
    property WithLinenumbers: Boolean read FWithLinenumbers;

    // tab browser
    property BrowserOpenKeys: string read FBrowserOpenKeys;
    property BrowserProgram: string read FBrowserProgram;
    property BrowserTitle: string read FBrowserTitle;
    property OnlyOneBrowserWindow: Boolean read FOnlyOneBrowserWindow;
    property UseIEinternForDocuments: Boolean read FUseIEinternForDocuments;

    // tab colors
    property AttrBrackets: TSynHighlighterAttributes read FAttrBrackets;

    // tab subversion
    property SubversionOK: Boolean read FSubversionOK;
    property SVNFolder: string read FSVNFolder;
    property SVNRepository: string read FSVNRepository;

    // tab git
    property GitFolder: string read FGitFolder;
    property GitLocalRepository: string read FGitLocalRepository;
    property GitOK: Boolean read FGitOK;
    property GitRemoteRepository: string read FGitRemoteRepository;

    // tab junit
    property JUnitAfterEach: Boolean read FJUnitAfterEach;
    property JUnitBeforeEach: Boolean read FJUnitBeforeEach;
    property JUnitJarFile: string read FJUnitJarFile;
    property JUnitManual: string read FJUnitManual;
    property JUnitOk: Boolean read FJUnitOk;
    property JUnitParameter: string read FJUnitParameter;

    // tab logfiles
    property LogfileCompiler: string read FLogfileCompiler;
    property LogfileCompilerOK: Boolean read FLogfileCompilerOK;
    property LogfileExceptions: string read FLogfileExceptions;
    property LogfileExceptionsOK: Boolean read FLogfileExceptionsOK;
    property LogfileInteractive: string read FLogfileInteractive;
    property LogfileInteractiveOK: Boolean read FLogfileInteractiveOK;

    // tab sequence diagram
    property SDClose: string read FSDClose;
    property SDFillingcolor: TColor read FSDFillingcolor;
    property SDNew: string read FSDNew;
    property SDNoFilling: Boolean read FSDNoFilling;
    property SDObject: string read FSDObject;
    property SDShowMainCall: Boolean read FSDShowMainCall;
    property SDShowParameter: Boolean read FSDShowParameter;
    property SDShowReturn: Boolean read FSDShowReturn;
    property SequencediagramS: string read FSequencediagramS;

    // tab structogram
    property Algorithm: string read FAlgorithm;
    property CaseCount: Integer read FCaseCount;
    property DoWhile: string read FDoWhile;
    property GenerateJavaAsProgram: Integer read FGenerateJavaAsProgram
      write FGenerateJavaAsProgram;
    property Input: string read FInput;
    property No_: string read FNo;
    property Other: string read FOther;
    property Output: string read FOutput;
    property StructoDatatype: string read FStructoDatatype
      write FStructoDatatype;
    property StructogramS: string read FStructogramS;
    property StructogramShadowIntensity: Integer
      read FStructogramShadowIntensity;
    property StructogramShadowWidth: Integer read FStructogramShadowWidth;
    property SwitchWithCaseLine: Boolean read FSwitchWithCaseLine;
    property Yes: string read FYes;
    property _For: string read FFor;
    property _While: string read FWhile;

    // tab language
    property LanguageCode: string read FLanguageCode;

    // tab visibility
    property VisMenus: TBoolArray read FVisMenus;
    property VisTabs: TBoolArray read FVisTabs;
    property VisToolbars: TBoolArray read FVisToolbars;
  end;

var
  FConfiguration: TFConfiguration = nil;

implementation

{$R *.DFM}

uses
  System.IOUtils,
  System.StrUtils,
  Zip,
  Printers,
  ShlObj,
  Menus,
  Math,
  Themes,
  SHDocVw,
  ShellAPI,
  UITypes,
  JvGnugettext,
  SynUnicode,
  SynEditKeyCmds,
  TB2Item,
  SpTBXTabs,
  UJava,
  UEditorForm,
  UBaseForm,
  UUtils,
  UHTMLHelp,
  UDlgClasspath,
  UDlgAbout,
  UDlgDownload,
  UDlgJarCreate,
  USubversion,
  UScpHint,
  UJavaScanner,
  UGit,
  UCodeCompletion,
  UStringRessources,
  UJavaCommands,
  ULLMChatForm;

const
  MaxPages = 36;
  Machine = 0;
  AllUsers = 1;
  User = 2;

  { --- TEditStyleHook ----------------------------------------------------------- }
  { --- https://theroadtodelphi.com/2012/02/06/changing-the-color-of-edit-controls-with-vcl-styles-enabled/ }

constructor TEditStyleHookColor.Create(AControl: TWinControl);
begin
  inherited;
  UpdateColors;
end;

// Here you set the colors of the style hook
procedure TEditStyleHookColor.UpdateColors;
begin
  if Control.Enabled then
  begin
    if TWinControlH(Control).Color = clRed then
    begin
      Brush.Color := clRed;
      FontColor := StyleServices.GetStyleFontColor(sfEditBoxTextDisabled);
      // use the Control font color
    end
    else
      Brush.Color := StyleServices.GetStyleColor(scEdit);
    // use the Control color
  end
  else
  begin
    Brush.Color := StyleServices.GetStyleColor(scEditDisabled);
    // scEditDisabled
    FontColor := StyleServices.GetStyleFontColor(sfEditBoxTextDisabled);
  end;
end;

// handle the messages
procedure TEditStyleHookColor.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX .. CN_CTLCOLORSTATIC:
      begin
        // get the colors
        UpdateColors;
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
        Handled := True;
      end;
    CM_COLORCHANGED, CM_ENABLEDCHANGED:
      begin
        // get the colors
        UpdateColors;
        Handled := False;
      end;
  else
    inherited WndProc(Message);
  end;
end;

function TFConfiguration.GetCheckColor(Str: string;
  EmptyAllowed: Boolean): TColor;
begin
  if (Str = '') and EmptyAllowed or FileExists(DissolveUsername(Str)) or
    IsHTTP(Str) and GlobalFileExists(Str) then
    Result := StyleServices.GetSystemColor(clWindow)
  else
    Result := clRed;
end;

function TFConfiguration.DirectoryFilesExists(Str: string): Boolean;
var
  Posi: Integer;
  Dir: string;
begin
  Result := True;
  if Str <> '' then
  begin
    Str := Str + ';';
    Posi := Pos(';', Str);
    while Posi > 0 do
    begin
      Dir := Trim(Copy(Str, 1, Posi - 1));
      Delete(Str, 1, Posi);
      if EndsWith(Dir, '*') then
        Delete(Dir, Length(Dir), 1);
      if ((Copy(Dir, 2, 1) = ':') or (Copy(Dir, 1, 2) = '\\')) and
        not(DirectoryExists(Dir) or FileExists(Dir)) then
        Result := False;
      Posi := Pos(';', Str);
    end;
  end;
end;

procedure TFConfiguration.CheckFolder(Edit: TEdit; EmptyAllowed: Boolean);
begin
  var
  Str := ExtendPath(Edit);
  ShortenPath(Edit, Str);
  if DirectoryExists(Edit.Hint) or (Str = '') and EmptyAllowed then
    Edit.Color := clWindow
  else
    Edit.Color := clRed;
  Edit.Enabled := not FLockedPaths;
end;

procedure TFConfiguration.CheckFolders(Edit: TEdit);
begin
  if DirectoryFilesExists(Edit.Text) then
    Edit.Color := clWindow
  else
    Edit.Color := clRed;
  Edit.Enabled := not FLockedPaths;
end;

procedure TFConfiguration.CheckFolderCB(ComboBox: TComboBox);
begin
  var
  Str := ExtendPath(ComboBox);
  ShortenPath(ComboBox, Str);
  if DirectoryFilesExists(Str) then
    ComboBox.Color := clWindow
  else
    ComboBox.Color := clRed;
  ComboBox.Enabled := not FLockedPaths;
end;

procedure TFConfiguration.CheckUserFolder(Edit: TEdit);
begin
  var
  Str := ExtendPath(Edit);
  ShortenPath(Edit, Str);
  if DirectoryFilesExists(DissolveUsername(Edit.Hint)) then
    Edit.Color := clWindow
  else
    Edit.Color := clRed;
end;

procedure TFConfiguration.CheckUserFolders(Edit: TEdit);
begin
  if DirectoryFilesExists(DissolveUsername(Edit.Text)) then
    Edit.Color := clWindow
  else
    Edit.Color := clRed;
end;

procedure TFConfiguration.CheckCBManual;
begin
  CheckFile(CBManual, True);
  if CBManual.Text <> '' then
    ComboBoxAdd(CBManual);
end;

procedure TFConfiguration.CheckCBManualFX;
begin
  CheckFile(CBManualFX, True);
  if CBManualFX.Text <> '' then
    ComboBoxAdd(CBManualFX);
end;

procedure TFConfiguration.CheckAllFilesAndFolders;
begin
  CheckFolderCB(CBJDKFolder);
  if (CBJDKFolder.Hint = '') or
    not FileExists(WithTrailingSlash(CBJDKFolder.Hint) + 'bin\java.exe') then
    CBJDKFolder.Color := clRed;
  CheckFile(EInterpreter, False);
  CheckFolder(EJavaFXFolder, True);
  CheckFolders(EClasspathAdmin);
  CheckUserFolders(EClasspathUser);
  CheckFile(EJavaCompiler, False);
  CheckFile(EAppletviewer, False);
  CheckFile(EDebugger, False);
  CheckFile(CBDisassembler, False);
  CheckFile(ECheckstyle, True);
  CheckFile(ECheckstyleConfiguration, True);
  CheckFile(EJalopy, True);
  CheckFile(EJalopyConfiguration, True);
  CheckFile(EJavaDoc, False);
  CheckFile(EJar, False);
  CheckFile(ETemplateConsole, True);
  CheckFile(ETemplateFrame, True);
  CheckFile(ETemplateDialog, True);
  CheckFile(ETemplateApplet, True);
  CheckFile(ETemplateJFrame, True);
  CheckFile(ETemplateJDialog, True);
  CheckFile(ETemplateJApplet, True);
  CheckFile(ETemplateApplication, True);
  CheckFile(ETemplateControlstructure, True);
  CheckFile(ETemplateClass, True);
  CheckFile(ETemplateJUnitTest, True);
  CheckMindstorms;

  CheckFolders(EAndroidSDKFolder);
  CheckCBManual;
  CheckCBManualFX;
  CheckUserFolders(EJavaDocs);
  CheckFile(ETutorial, True);
  CheckUserFolder(ECache);
  ECache.Enabled := not FLockedPaths;
  CheckFile(EJavabook, True);
  CheckUserFolder(ETempFolder);
  CheckFile(EBrowserProgram, True);
  CheckFile(EKeyboardFile, True);
  if not CBUseIEinternForDocuments.Checked and (CBOpenBrowserShortcut.Text = '')
  then
    CBOpenBrowserShortcut.Color := clRed
  else
    CBOpenBrowserShortcut.Color := clWindow;
  CheckFolder(ESVNFolder, True);
  CheckFolderCB(CBRepository);
  CheckFolder(ETempFolder, False);
  CheckFile(ELogfileInteractive, True);
  CheckFile(ELogfileCompiler, True);
  CheckFile(ELogfileExceptions, True);
  CheckFolder(EGitFolder, True);
  CheckFolderCB(CBLocalRepository);
  CheckFolderCB(CBRemoteRepository);
  CheckFile(EJUnitJarFile, True);
  CheckFile(EJUnitManual, True);
  LockButtons;
end;

procedure TFConfiguration.LockButtons;
begin
  SBJDKFolderSelect.Enabled := not FLockedPaths;
  BJDKInstall.Enabled := not FLockedPaths;
  BInterpreter.Enabled := not FLockedPaths;
  BJavaFXFolder.Enabled := not FLockedPaths;
  BClasspathAdmin.Enabled := not FLockedPaths;
  BJavaCompiler.Enabled := not FLockedPaths;
  BDebugger.Enabled := not FLockedPaths;
  BJavaDoc.Enabled := not FLockedPaths;
  BAppletviewer.Enabled := not FLockedPaths;
  BDisassemblerInstall.Enabled := not FLockedPaths;
  SBDisassemblerSelect.Enabled := not FLockedPaths;
  BJar.Enabled := not FLockedPaths;
  BSelectBrowser.Enabled := not FLockedPaths;
  BTemplateConsole.Enabled := not FLockedPaths;
  BTemplateFrame.Enabled := not FLockedPaths;
  BTemplateDialog.Enabled := not FLockedPaths;
  BTemplateApplet.Enabled := not FLockedPaths;
  BTemplateJFrame.Enabled := not FLockedPaths;
  BTemplateJDialog.Enabled := not FLockedPaths;
  BTemplateJApplet.Enabled := not FLockedPaths;
  BTemplateApplication.Enabled := not FLockedPaths;
  BTemplateControlstructure.Enabled := not FLockedPaths;
  BTemplateClass.Enabled := not FLockedPaths;
  BTemplateJUnitTest.Enabled := not FLockedPaths;
  BKeyboardFile.Enabled := not FLockedPaths;
  SBKeyboardfile.Enabled := not FLockedPaths;
  SBManualSelect.Enabled := not FLockedPaths;
  SBManualFXSelect.Enabled := not FLockedPaths;
  BManualInstall.Enabled := not FLockedPaths;
  BManualFXInstall.Enabled := not FLockedPaths;
  SBTutorialSelect.Enabled := not FLockedPaths;
  BTutorialInstall.Enabled := not FLockedPaths;
  SBJavabookSelect.Enabled := not FLockedPaths;
  BJavabookInstall.Enabled := not FLockedPaths;
  BJavaDocFolder.Enabled := not FLockedPaths;
  SBCacheSelect.Enabled := not FLockedPaths;
  BCache.Enabled := not FLockedPaths;
  SBLejosSelect.Enabled := not FLockedPaths;
  SBLejosCompiler.Enabled := not FLockedPaths;
  SBLejosUploader.Enabled := not FLockedPaths;
  SBLejosFlasher.Enabled := not FLockedPaths;
  SBMindstormsManual.Enabled := not FLockedPaths;
  BLejosInstall.Enabled := not FLockedPaths;
  BMindstormsManual.Enabled := not FLockedPaths;
  SBMindstormsTemplate.Enabled := not FLockedPaths;
  BMindstormsTemplate.Enabled := not FLockedPaths;
  SBAndroidSDKFolder.Enabled := not FLockedPaths;
  BAndroidSDKInstall.Enabled := not FLockedPaths;
  SBTempSelect.Enabled := not FLockedPaths;
  BTempFolder.Enabled := not FLockedPaths;
  BLogfileCompiler.Enabled := not FLockedPaths;
  BLogfileInteractive.Enabled := not FLockedPaths;
  BLogfileExceptions.Enabled := not FLockedPaths;
  BGitFolder.Enabled := not FLockedPaths;
  BGitRepository.Enabled := not FLockedPaths;
  BGitClone.Enabled := not FLockedPaths;

  BJUnitInstall.Enabled := not FLockedPaths;
  SBJUnit.Enabled := not FLockedPaths;
  SBJUnitManual.Enabled := not FLockedPaths;
  SBCheckStyleSelect.Enabled := not FLockedPaths;
  BCheckstyleInstall.Enabled := not FLockedPaths;
  BCheckstyleConfiguration.Enabled := not FLockedPaths;
  SBJalopySelect.Enabled := not FLockedPaths;
  BJalopyInstall.Enabled := not FLockedPaths;
  BJalopyConfiguration.Enabled := not FLockedPaths;
  BSVN.Enabled := not FLockedPaths;
  BRepository.Enabled := not FLockedPaths;
end;

procedure TFConfiguration.SetJDKFolder(Dir: string);
begin
  ShortenPath(CBJDKFolder, Dir);
  ComboBoxAdd(CBJDKFolder);
  TidyPath(Dir +  '\bin;'); // Exception, if Dir is declared as const!
  ShortenPath(EJavaCompiler, CBJDKFolder.Hint + '\bin\javac.exe');
  ShortenPath(EInterpreter, CBJDKFolder.Hint + '\bin\java.exe');
  ShortenPath(EAppletviewer, CBJDKFolder.Hint + '\bin\appletviewer.exe');
  if (Pos('jdb.exe', EDebugger.Hint) > 0) then
    ShortenPath(EDebugger, CBJDKFolder.Hint + '\bin\jdb.exe');
  if (Pos('javap.exe', CBDisassembler.Hint) > 0) then
    ShortenPath(CBDisassembler, CBJDKFolder.Hint + '\bin\javap.exe');
  ShortenPath(EJavaDoc, CBJDKFolder.Hint + '\bin\JavaDoc.exe');
  ShortenPath(EJar, CBJDKFolder.Hint + '\bin\jar.exe');
  BClasspathAdminClick(Self);
  CheckAllFilesAndFolders;
end;

procedure TFConfiguration.BJDKFolderSelectClick(Sender: TObject);
begin
  var
  Dir := CBJDKFolder.Hint;
  if not DirectoryExists(Dir) then
    Dir := GetEnvironmentVariable('PROGRAMFILES');
  FolderDialog.DefaultFolder := Dir;
  if FolderDialog.Execute then
  begin
    Dir := ExcludeTrailingPathDelimiter(FolderDialog.Filename);
    SetJDKFolder(Dir);
  end;
end;

procedure TFConfiguration.CBJDKFolderSelect(Sender: TObject);
begin
  CheckFolderCB(CBJDKFolder);
  if (CBJDKFolder.Color <> clRed) and (CBJDKFolder.Text <> '') then
    SetJDKFolder(CBJDKFolder.Hint);
end;

function TFConfiguration.GetTargetDir(const Target: string): string;
begin
  if FPortableApplication then
    Result := FEditorFolder + 'App\' + Target
  else
    Result := FEditorFolder + Target;
  SysUtils.ForceDirectories(Result);
  Result := ExcludeTrailingPathDelimiter(Result);
end;

procedure TFConfiguration.FileSelect(Edit: TEdit;
  const Filter, Filename, Dir: string);
begin
  if (Edit.Hint <> '') and FileExists(Edit.Hint) then
    FODSelect.InitialDir := ExtractFilePath(Edit.Hint)
  else
    FODSelect.InitialDir := GetTargetDir(Dir);
  FODSelect.Filter := Filter + '|*.*|*.*';
  FODSelect.Filename := Filename;
  if FODSelect.Execute then
    ShortenPath(Edit, FODSelect.Filename);
end;

procedure TFConfiguration.FileSelectWithoutShortenPath(Edit: TEdit;
  const Filter, Filename, Dir: string);
begin
  if FileExists(Edit.Text) then
    FODSelect.InitialDir := ExtractFilePath(Edit.Text)
  else
    FODSelect.InitialDir := GetTargetDir(Dir);
  FODSelect.Filter := Filter + '|*.*|*.*';
  FODSelect.Filename := Filename;
  if FODSelect.Execute then
    ShortenPath(Edit, FODSelect.Filename);
end;

procedure TFConfiguration.BJavaFXParameterClick(Sender: TObject);
begin
  EJavaFXParameter.Text := 'javafx.controls,javafx.media,javafx.web';
end;

procedure TFConfiguration.FileSelect2(Edit: TEdit;
  const Filter, Filename, Dir: string);
begin
  FODSelect.InitialDir := Dir;
  FODSelect.Filter := Filter + '|*.*|*.*';
  FODSelect.Filename := Filename;
  if FODSelect.Execute then
    ShortenPath(Edit, FODSelect.Filename);
end;

procedure TFConfiguration.BLejosInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := GetTargetDir('');
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        if RGMindstormsVersion.ItemIndex = 2 then
          StringList := GetDownloadFiles('LejosEV3')
        else
          StringList := GetDownloadFiles('Lejos');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'],
            Source + StringList.Values['zip2']);
          ShowModal;
          if DownloadIsOK then
          begin
            if VistaOrBetter then
              CallUpdater(Target, Source + StringList.Values['zip2'], '')
            else
              ExtractZipToDir(Source + StringList.Values['zip2'], Target);
            ShortenPath(ELejosFolder, Target + StringList.Values['file1']);
            ShortenPath(EMindstormsManual, Target + StringList.Values['file2']);
            if RGMindstormsVersion.ItemIndex = 1 then
            begin
              ShortenPath(ELejosCompiler, ELejosFolder.Hint + '\bin\nxjc.bat');
              ShortenPath(ELejosUploader, ELejosFolder.Hint + '\bin\nxj.bat');
              ShortenPath(ELejosFlasher, ELejosFolder.Hint +
                '\bin\nxjflashg.bat');
            end;
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckMindstorms;
end;

procedure TFConfiguration.SBLejosSelectClick(Sender: TObject);
begin
  var
  Str := ELejosFolder.Hint;
  if DirectoryExists(Str) then
    FolderDialog.DefaultFolder := Str;
  if FolderDialog.Execute then
  begin
    Str := ExcludeTrailingPathDelimiter(FolderDialog.Filename);
    ShortenPath(ELejosFolder, Str);
    if Pos(ELejosFolder.Hint, EMindstormsManual.Hint) > 0 then
      ShortenPath(EMindstormsManual, ELejosFolder.Hint +
        '\docs\nxt\index.html');
  end;
  CheckFile(EMindstormsManual, True);
end;

procedure TFConfiguration.SBLejosUploaderClick(Sender: TObject);
begin
  FileSelect2(ELejosUploader, '*.bat|*.bat', 'nxj.bat',
    FLejosVerzeichnis + '\bin');
  CheckFile(ELejosUploader, True);
end;

procedure TFConfiguration.SBLejosCompilerClick(Sender: TObject);
begin
  FileSelect2(ELejosCompiler, '*.bat|*.bat', 'nxjc.bat',
    FLejosVerzeichnis + '\bin');
  CheckFile(ELejosCompiler, True);
end;

procedure TFConfiguration.SBLejosFlasherClick(Sender: TObject);
begin
  FileSelect2(ELejosFlasher, '*.bat|*.bat', 'nxjflashg.bat',
    FLejosVerzeichnis + '\bin');
  CheckFile(ELejosFlasher, True);
end;

procedure TFConfiguration.BMindstormsManualClick(Sender: TObject);
begin
  var
  Str := FLejosVerzeichnis + '\projects\classes\doc';
  if not SysUtils.DirectoryExists(Str) then
    Str := '';
  FileSelect(EMindstormsManual, '*.html|*.html', 'index.html', Str);
  CheckFile(EMindstormsManual, True);
end;

procedure TFConfiguration.BSaveClick(Sender: TObject);
begin
  Close;
  if LBStyleNames.ItemIndex >= 0 then
    GUIStyle := LBStyleNames.Items[LBStyleNames.ItemIndex];
  if not FileExists(EInterpreter.Hint) then
    ErrorMsg(Format(_('Java interpreter %s not found!'), [EInterpreter.Hint]));
  Screen.Cursor := crHourGlass;
  try
    ViewToModel;
    ModelToRegistry;
    RegistryToModel;
    FJava.SetOptions;
  finally
    Screen.Cursor := crDefault;
  end;
  FJava.UpdateMenuItems(Self);
end;

procedure TFConfiguration.BCancelClick(Sender: TObject);
begin
  Close;
  ReadUserColors;
end;

procedure TFConfiguration.BJDKInstallClick(Sender: TObject);
begin
  FJava.CallHelp('https://www.oracle.com/java/technologies/downloads/');
end;

procedure TFConfiguration.BSelectBrowserClick(Sender: TObject);
begin
  FODSelect.InitialDir := GetEnvironmentVariable('PROGRAMFILES');
  FODSelect.Filename := '*.exe';
  FODSelect.Filter := '*.exe|*.exe';
  if not SysUtils.DirectoryExists(FODSelect.InitialDir) then
    FODSelect.InitialDir := 'C:\';
  if FODSelect.Execute then
    ShortenPath(EBrowserProgram, FODSelect.Filename);
  CheckFile(EBrowserProgram, True);
end;

procedure TFConfiguration.BEditorFontClick(Sender: TObject);
begin
  with FJava.FDFont do
  begin
    Options := [fdFixedPitchOnly];
    Font.Assign(FConfiguration.FEditFont);
    if Execute then
    begin
      for var I := 0 to FJava.TDIFormsList.Count - 1 do
        if FJava.TDIFormsList[I].FormTag = 1 then
          FJava.TDIFormsList[I].SetFont(Font);
      FConfiguration.FEditFont.Assign(Font);
    end;
  end;
end;

procedure TFConfiguration.BEditorStyleDefaultClick(Sender: TObject);
begin
  CBEditorStyles.Text := 'Default';
  CBEditorStylesChange(Self);
end;

function TFConfiguration.BrowserProgToName(const Str: string): string;
begin
  var
  Browser := UpperCase(Str);
  if Pos('NETSCAPE', Browser) > 0 then
    Result := 'Netscape'
  else if Pos('IEXPLORE', Browser) > 0 then
    Result := 'Internet Explorer'
  else if Pos('OPERA', Browser) > 0 then
    Result := 'Opera'
  else if Pos('FIREFOX', Browser) > 0 then
    Result := 'Mozilla Firefox'
  else if Pos('MOZILLA', Browser) > 0 then
    Result := 'Mozilla'
  else
    Result := 'unknown';
end;

procedure TFConfiguration.BRunJavaClick(Sender: TObject);
begin
  var
  Str := FTempDir + 'RunJava.bat';
  if FileExists(Str) then
  begin
    FJava.Open(Str);
    Close;
  end
  else
    ErrorMsg(_(LNGNoRunJavaBat));
end;

procedure TFConfiguration.BBrowserTitleClick(Sender: TObject);
begin
  ShortenPath(EBrowserTitle, BrowserProgToName(EBrowserProgram.Hint));
end;

procedure TFConfiguration.BJavaParameterClick(Sender: TObject);
begin
  EJavaCompilerParameter.Text := '-deprecation -g';
end;

procedure TFConfiguration.BInterpreterParameterClick(Sender: TObject);
begin
  EInterpreterParameter.Text := '';
end;

procedure TFConfiguration.BClasspathAdminClick(Sender: TObject);
begin
  EClasspathAdmin.Text := FEditorFolder + 'JEClasses.jar';
  CheckFolders(EClasspathAdmin);
end;

procedure TFConfiguration.BClasspathUserClick(Sender: TObject);
begin
  with TFClasspath.Create(Self) do
  begin
    Caption := _('Edit classpath');
    BNewJarFile.Visible := True;
    CBAllJarFiles.Visible := True;
    BNewAllClasses.Visible := False;
    Initialize(FJavaClasspathAll, False);
    if ShowModal = mrOk then
    begin
      FJavaClasspathAll := ClasspathAll;
      FJavaClasspathUser := ClasspathUser;
      EClasspathUser.Text := FJavaClasspathUser;
      CheckUserFolders(EClasspathUser);
    end;
    Free;
  end;
end;

procedure TFConfiguration.BJavaDocFolderClick(Sender: TObject);
begin
  with TFClasspath.Create(Self) do
  begin
    Caption := _('Edit JavaDoc User');
    BNewJarFile.Visible := False;
    CBAllJarFiles.Visible := False;
    BNewAllClasses.Visible := True;
    BNewAllClasses.Top := 200;
    Initialize(FJavaJavaDocsAll, True);
    if ShowModal = mrOk then
    begin
      FJavaJavaDocsAll := ClasspathAll;
      FJavaJavaDocs := ClasspathUser;
      EJavaDocs.Text := FJavaJavaDocs;
      CheckUserFolders(EJavaDocs);
    end;
    Free;
  end;
end;

procedure TFConfiguration.SetStartDir(const Dir, AFile, Filter: string);
begin
  FODSelect.InitialDir := CBJDKFolder.Hint + Dir;
  FODSelect.Filename := AFile;
  FODSelect.Filter := Filter + '|*.*|*.*';
  if not SysUtils.DirectoryExists(FODSelect.InitialDir) then
    FODSelect.InitialDir := CBJDKFolder.Hint;
  if not SysUtils.DirectoryExists(FODSelect.InitialDir) then
    FODSelect.InitialDir := 'C:\';
end;

procedure TFConfiguration.BJavaCompilerClick(Sender: TObject);
begin
  SetStartDir('\bin', 'javac.exe', '*.exe|*.exe');
  if FODSelect.Execute then
    ShortenPath(EJavaCompiler, FODSelect.Filename);
  CheckFile(EJavaCompiler, False);
end;

function TFConfiguration.FolderSelect(Edit: TEdit;
  const Foldername: string): string;
begin
  Result := Foldername;
  FolderDialog.DefaultFolder := Foldername;
  if FolderDialog.Execute then
  begin
    Result := ExcludeTrailingPathDelimiter(FolderDialog.Filename);
    ShortenPath(Edit, Result);
  end;
end;

procedure TFConfiguration.BInterpreterClick(Sender: TObject);
begin
  SetStartDir('\bin', 'java.exe', '*.exe|*.exe');
  if FODSelect.Execute then
    ShortenPath(EInterpreter, FODSelect.Filename);
  CheckFile(EInterpreter, False);
end;

procedure TFConfiguration.BLogfileCompilerClick(Sender: TObject);
begin
  with FJava.SDSaveAs do
  begin
    Title := _(LNGSaveAs);
    Filter := '*.txt|*.txt|(*.*)|*.*';
    FilterIndex := 1;
    Filename := ELogfileCompiler.Hint;
    if Filename = '' then
      Filename := 'JELogfileCompiler.txt';
    var
    Path := ExtractFilePath(Filename);
    if Path <> '' then
      InitialDir := Path
    else if FPortableApplication then
      InitialDir := FEditorFolder + 'App\Log'
    else
      InitialDir := TPath.GetHomePath + '\JavaEditor\Log';
    SysUtils.ForceDirectories(InitialDir);
    if Execute then
    begin
      CreateMyFile(Filename);
      ShortenPath(ELogfileCompiler, Filename);
      FLogfileCompiler := Filename;
      FLogfileCompilerOK := FileExists(Filename);
    end;
  end;
end;

procedure TFConfiguration.BLogfileInteractiveClick(Sender: TObject);
begin
  with FJava.SDSaveAs do
  begin
    Title := _(LNGSaveAs);
    Filter := '*.txt|*.txt|(*.*)|*.*';
    FilterIndex := 1;
    Filename := ELogfileInteractive.Hint;
    if Filename = '' then
      Filename := 'JELogfileInteractive.txt';
    var
    Path := ExtractFilePath(Filename);
    if Path <> '' then
      InitialDir := Path
    else if FPortableApplication then
      InitialDir := FEditorFolder + 'App\Log'
    else
      InitialDir := TPath.GetHomePath + '\JavaEditor\Log';
    SysUtils.ForceDirectories(InitialDir);
    if Execute then
    begin
      CreateMyFile(Filename);
      ShortenPath(ELogfileInteractive, Filename);
      FLogfileInteractive := Filename;
      FLogfileInteractiveOK := FileExists(Filename);
    end;
  end;
end;

procedure TFConfiguration.BLogfileExceptionsClick(Sender: TObject);
begin
  with FJava.SDSaveAs do
  begin
    Title := _(LNGSaveAs);
    Filter := '*.txt|*.txt|(*.*)|*.*';
    FilterIndex := 1;
    Filename := ELogfileExceptions.Hint;
    if Filename = '' then
      Filename := 'JELogfileExceptions.txt';
    var
    Path := ExtractFilePath(Filename);
    if Path <> '' then
      InitialDir := Path
    else if FPortableApplication then
      InitialDir := FEditorFolder + 'App\Log'
    else
      InitialDir := TPath.GetHomePath + '\JavaEditor\Log';
    SysUtils.ForceDirectories(InitialDir);
    if Execute then
    begin
      CreateMyFile(Filename);
      ShortenPath(ELogfileExceptions, Filename);
      FLogfileExceptions := Filename;
      FLogfileExceptionsOK := FileExists(Filename);
    end;
  end;
end;

procedure TFConfiguration.BAndroidAssembleClick(Sender: TObject);
begin
  var
  Str := FEditorFolder + 'assemble.bat';
  if FileExists(Str) then
  begin
    FJava.Open(Str);
    Close;
  end
  else
    ErrorMsg(_(LNGNoRunJavaBat));
end;

procedure TFConfiguration.BAndroidSDKInstallClick(Sender: TObject);
begin
  FJava.CallHelp('https://developer.android.com/studio/index.html');
end;

procedure TFConfiguration.BAppletviewerClick(Sender: TObject);
begin
  SetStartDir('\bin', 'appletviewer.exe', '*.exe|*.exe');
  if FODSelect.Execute then
    ShortenPath(EAppletviewer, FODSelect.Filename);
  CheckFile(EAppletviewer, False);
end;

// tab Utilities

procedure TFConfiguration.BDebuggerClick(Sender: TObject);
begin
  SetStartDir('\bin', 'jdb.exe', '*.exe|*.exe');
  if FODSelect.Execute then
    ShortenPath(EDebugger, FODSelect.Filename);
  CheckFile(EDebugger, False);
end;

procedure TFConfiguration.SBDisassemblerSelectClick(Sender: TObject);
begin
  var
  Str := CBDisassembler.Hint;
  if FileExists(Str) then
    FODSelect.InitialDir := ExtractFilePath(Str);
  FODSelect.Filter := '*.exe|*.exe;*.*|*.*';
  FODSelect.Filename := 'javap';
  if FODSelect.Execute then
    ShortenPath(CBDisassembler, FODSelect.Filename);
end;

procedure TFConfiguration.BDisassemblerInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := GetTargetDir('jad');
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('Disassembler');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'], Source + '\jad.zip');
          ShowModal;
          if DownloadIsOK then
          begin
            if VistaOrBetter then
              CallUpdater(Target, Source + '\jad.zip', '')
            else
              ExtractZipToDir(Source + '\jad.zip', Target);
            ShortenPath(CBDisassembler, Target + '\jad.exe');
            if FileExists(Target + '\jad.exe') then
              ComboBoxAddEx(CBDisassembler);
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(CBDisassembler, False);
end;

procedure TFConfiguration.CBDisassemblerSelect(Sender: TObject);
begin
  ShortenPath(CBDisassembler, CBDisassembler.Text);
  BDisassemblerParameterClick(Self);
end;

procedure TFConfiguration.CBEditorStylesChange(Sender: TObject);
begin
  FEditorStyle := CBEditorStyles.Text;
  ReadUserColors;
  LBColorElementsClick(Self);
end;

procedure TFConfiguration.BDisassemblerParameterClick(Sender: TObject);
begin
  if Pos('javap.exe', CBDisassembler.Text) > 0 then
    EDisassemblerParameter.Text := '-l -c -verbose'
  else
    EDisassemblerParameter.Text := '-o';
end;

procedure TFConfiguration.BJavaDocClick(Sender: TObject);
begin
  SetStartDir('\bin', 'JavaDoc.exe', '*.exe|*.exe');
  if FODSelect.Execute then
    ShortenPath(EJavaDoc, FODSelect.Filename);
  CheckFile(EJavaDoc, False);
end;

procedure TFConfiguration.BJarClasspathClick(Sender: TObject);
begin
  EJarClasspath.Text := 'JEClasses.jar';
end;

procedure TFConfiguration.BJarClick(Sender: TObject);
begin
  SetStartDir('\bin', 'jar.exe', '*.exe|*.exe');
  if FODSelect.Execute then
    ShortenPath(EJar, FODSelect.Filename);
  CheckFile(EJar, False);
end;

// tab Documentation

procedure TFConfiguration.CBManualFXSelect(Sender: TObject);
begin
  CheckCBManualFX;
  if CBManualFX.Color = clRed then
    SBManualFXSelectClick(Self);
end;

procedure TFConfiguration.CBManualSelect(Sender: TObject);
begin
  CheckCBManual;
  if CBManual.Color = clRed then
    SBManualSelectClick(Self);
end;

procedure TFConfiguration.CBNoActiveLineColorClick(Sender: TObject);
begin
  CBActiveLineColor.Enabled := not CBNoActiveLineColor.Checked;
end;

procedure TFConfiguration.SBManualFXSelectClick(Sender: TObject);
begin
  SetStartDir('docs', '', '*.chm;*.html|*.chm;*.html');
  if FODSelect.Execute then
    ShortenPath(CBManualFX, FODSelect.Filename);
  CheckCBManualFX;
end;

procedure TFConfiguration.SBManualSelectClick(Sender: TObject);
begin
  SetStartDir('docs', '', '*.chm;*.html|*.chm;*.html');
  if FODSelect.Execute then
    ShortenPath(CBManual, FODSelect.Filename);
  CheckCBManual;
end;

procedure TFConfiguration.BManualFXInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := FJDKFolder + '\docsfx';
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('ManualFX');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'],
            Source + StringList.Values['zip2']);
          ShowModal;
          if DownloadIsOK then
          begin
            if VistaOrBetter then
              CallUpdater(Target, Source + StringList.Values['zip2'], '')
            else
              ExtractZipToDir(Source + StringList.Values['zip2'], Target);
            ShortenPath(CBManualFX, Target + StringList.Values['file1']);
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckCBManualFX;
end;

procedure TFConfiguration.BManualInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := FJDKFolder + '\docs';
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('Manual');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'],
            Source + StringList.Values['zip2']);
          ShowModal;
          if DownloadIsOK then
          begin
            if VistaOrBetter then
              CallUpdater(Target, Source + StringList.Values['zip2'], '')
            else
              ExtractZipToDir(Source + StringList.Values['zip2'], Target);
            ShortenPath(CBManual, Target + StringList.Values['file1']);
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckCBManual;
end;

procedure TFConfiguration.BTutorialInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := GetTargetDir('tutorial');
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('Tutorial');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'], Source + '\tutorial.zip');
          ShowModal;
          if DownloadIsOK then
          begin
            if VistaOrBetter then
              CallUpdater(Target, Source + '\tutorial.zip', '')
            else
              ExtractZipToDir(Source + '\tutorial.zip', Target);
            ShortenPath(ETutorial, Target + '\tutorial.chm');
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(ETutorial, True);
end;

procedure TFConfiguration.BGuiFontDefaultClick(Sender: TObject);
begin
  FGUIFontSize := 12;
  FGUIFontName := 'Dialog';
end;

procedure TFConfiguration.BGuiFontClick(Sender: TObject);
begin
  with FJava.FDFont do
  begin
    Options := [];
    Font.Size := FGUIFontSize;
    Font.Name := FGUIFontName;
    if Execute then
    begin
      FGUIFontSize := Max(Font.Size, 4);
      FGUIFontName := Font.Name;
    end;
  end;
end;

procedure TFConfiguration.BFileExtensionsClick(Sender: TObject);
var
  Str, Str1, Str2: string;
  Posi: Integer;
begin
  if VistaOrBetter then
  begin
    Str2 := CBAssociationJava.Caption + ' ' +
      BoolToStr(CBAssociationJava.Checked) + ' ';
    Str2 := Str2 + CBAssociationJfm.Caption + ' ' +
      BoolToStr(CBAssociationJfm.Checked) + ' ';
    Str2 := Str2 + CBAssociationUml.Caption + ' ' +
      BoolToStr(CBAssociationUml.Checked) + ' ';
    Str2 := Str2 + CBAssociationJep.Caption + ' ' +
      BoolToStr(CBAssociationJep.Checked) + ' ';
    Str2 := Str2 + CBAssociationHtml.Caption + ' ' +
      BoolToStr(CBAssociationHtml.Checked) + ' ';
    Str2 := Str2 + CBAssociationTxt.Caption + ' ' +
      BoolToStr(CBAssociationTxt.Checked) + ' ';
    Str2 := Str2 + CBAssociationJsp.Caption + ' ' +
      BoolToStr(CBAssociationJsp.Checked) + ' ';
    Str2 := Str2 + CBAssociationPhp.Caption + ' ' +
      BoolToStr(CBAssociationPhp.Checked) + ' ';
    Str2 := Str2 + CBAssociationCss.Caption + ' ' +
      BoolToStr(CBAssociationCss.Checked) + ' ';
    Str2 := Str2 + CBAssociationInc.Caption + ' ' +
      BoolToStr(CBAssociationInc.Checked) + ' ';
    Str2 := Str2 + CBAssociationJsg.Caption + ' ' +
      BoolToStr(CBAssociationJsg.Checked) + ' ';
    Str2 := Str2 + CBAssociationJSD.Caption + ' ' +
      BoolToStr(CBAssociationJSD.Checked) + ' ';

    Str := EAdditionalAssociations.Text + ';';
    Posi := Pos(';', Str);
    while Posi > 0 do
    begin
      Str1 := Copy(Str, 1, Posi - 1);
      Delete(Str, 1, Posi);
      Posi := Pos('.', Str1);
      if Posi > 0 then
        Delete(Str1, 1, Posi);
      var
      Len := Length(Str1);
      if (Str1 <> '') and (Len in [2, 3, 4, 5]) then
        Str2 := Str2 + '.' + Str1 + ' ' + BoolToStr(True) + ' ';
      Posi := Pos(';', Str);
    end;
    CallUpdater(ParamStr(0), 'registry', Str2);
  end
  else
    MakeAssociations;
end;

procedure TFConfiguration.BJEAssociationClick(Sender: TObject);
begin
  CallUpdater(ParamStr(0), 'jeregistry',
    HideBlanks(EncodeQuotationMark(EJEAssociation.Text)));
end;

procedure TFConfiguration.BJUnitInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := GetTargetDir('junit');
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('JUnit');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['file1'],
            Target + StringList.Values['file2']);
          ShowModal;
          if DownloadIsOK then
          begin
            EJUnitJarFile.Text := EFile.Text;
            EJUnitJarFile.Hint := EFile.Text;
            DownloadFile(StringList.Values['zip1'], Source + '\junitdoc.zip');
            if VistaOrBetter then
              CallUpdater(Target + '\docs', Source + '\junitdoc.zip ', '')
            else
              ExtractZipToDir(Source + '\junitdoc.zip', Target + '\docs');
            EJUnitManual.Text := Target + '\docs\index.html';
            EJUnitManual.Hint := Target + '\docs\index.html';
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFileWithoutShortenpath(EJUnitJarFile, True);
end;

procedure TFConfiguration.SBTutorialSelectClick(Sender: TObject);
begin
  FileSelect(ETutorial, '*.chm;*.html|*.chm;*.html', '', 'tutorial');
  CheckFile(ETutorial, True);
end;

procedure TFConfiguration.BJavabookInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := GetTargetDir('Javabook');
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('Javabook');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'], Source + '\html.zip');
          ShowModal;
          if DownloadIsOK then
          begin
            DownloadFile(StringList.Values['zip2'], Source + '\examples.zip');
            if VistaOrBetter then
              CallUpdater(Target, Source + '\html.zip ',
                Source + '\examples.zip')
            else
            begin
              ExtractZipToDir(Source + '\html.zip', Target);
              ExtractZipToDir(Source + '\examples.zip', Target);
            end;
            ShortenPath(EJavabook, Target + '\html\cover.html');
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(EJavabook, True);
end;

procedure TFConfiguration.SBJavabookSelectClick(Sender: TObject);
begin
  FileSelect(EJavabook, '*.html|*.html', 'cover.html', 'Javabook');
  CheckFile(EJavabook, True);
end;

procedure TFConfiguration.SBJUnitClick(Sender: TObject);
begin
  FileSelectWithoutShortenPath(EJUnitJarFile, 'jar|*.jar',
    'junit-platform-console-standalone*.jar', 'junit');
  CheckFileWithoutShortenpath(EJUnitJarFile, True);
end;

procedure TFConfiguration.SBJUnitManualClick(Sender: TObject);
begin
  SetStartDir('docs', '', 'index.html|index.html');
  if FileExists(FJUnitJarFile) then
    FODSelect.InitialDir := ExtractFilePath(FJUnitJarFile);
  if FODSelect.Execute then
  begin
    EJUnitManual.Text := FODSelect.Filename;
    EJUnitManual.Hint := FODSelect.Filename;
  end;
end;

procedure TFConfiguration.RemoveShortcutFromMainMenu(ShortCut: Integer);
var
  MenuItem, SubMenuItem, SubSubMenuItem: TTBCustomItem;
begin
  for var I := 0 to FJava.MainMenu.Items.Count - 1 do
  begin
    MenuItem := FJava.MainMenu.Items[I];
    if MenuItem.ShortCut = ShortCut then
      MenuItem.ShortCut := 0
    else
      for var J := 0 to MenuItem.Count - 1 do
      begin
        SubMenuItem := MenuItem[J];
        if SubMenuItem.ShortCut = ShortCut then
          SubMenuItem.ShortCut := 0
        else
          for var K := 0 to SubMenuItem.Count - 1 do
          begin
            SubSubMenuItem := SubMenuItem[K];
            if SubSubMenuItem.ShortCut = ShortCut then
              SubSubMenuItem.ShortCut := 0;
          end;
      end;
  end;
end;

procedure TFConfiguration.ReplaceShortcutFromMainMenu(ShortCut,
  ShortCut2: Integer);
var
  MenuItem, SubMenuItem, SubSubMenuItem: TTBCustomItem;
begin
  for var I := 0 to FJava.MainMenu.Items.Count - 1 do
  begin
    MenuItem := FJava.MainMenu.Items[I];
    if MenuItem.ShortCut = ShortCut then
      MenuItem.ShortCut := ShortCut2
    else
      for var J := 0 to MenuItem.Count - 1 do
      begin
        SubMenuItem := MenuItem[J];
        if SubMenuItem.ShortCut = ShortCut then
          SubMenuItem.ShortCut := ShortCut2
        else
          for var K := 0 to SubMenuItem.Count - 1 do
          begin
            SubSubMenuItem := SubMenuItem[K];
            if SubSubMenuItem.ShortCut = ShortCut then
              SubSubMenuItem.ShortCut := ShortCut2;
          end;
      end;
  end;
end;

function TFConfiguration.StringToShortcut(Str: string): Integer;
begin
  Str := Trim(Copy(Str, Pos(':', Str) + 1, Length(Str)));
  Str := ReplaceStr(Str, 'Ctrl+', _('Ctrl+'));
  Str := ReplaceStr(Str, 'Shift+', _('Shift+'));
  Str := ReplaceStr(Str, 'Alt+', _('Alt+'));
  Result := TextToShortCut(Str);
end;

procedure TFConfiguration.ApplyKeyboardShortcuts;
var
  Posi, Key, Key2, Line: Integer;
  OffX, OffY: Integer;
  Str, Def: string;
  Keys: TStringList;

  function getNextLine: string;
  begin
    Inc(Line);
    if Line < Keys.Count then
      Result := Keys[Line]
    else
      Result := '';
  end;

  procedure EditKeyboardFile(const AFile: string);
  var
    Collision: Boolean;
  begin
    Keys := TStringList.Create;
    Keys.LoadFromFile(AFile);
    Line := -1;
    Str := getNextLine;
    repeat
      Posi := Pos('shortcut:', Str);
      if Pos('shortcut:end', Str) > 0 then
        Posi := 0;
      if Posi = 1 then
      begin
        Key := StringToShortcut(Str);
        RemoveShortcutFromMainMenu(Key);
        OffX := 0;
        OffY := 0;
        Def := '';
        Posi := 0;
        repeat
          Str := getNextLine;
          Posi := Pos('shortcut:end', Str);
          if Posi = 0 then
          begin
            var
            Posi1 := Pos('|', Str);
            if Posi1 > 0 then
            begin
              OffX := Posi1;
              OffY := Line;
            end;
            Def := Def + #13#10 + Str;
          end;
        until (Posi = 1) or (Line >= Keys.Count - 1);
        OffY := Line - OffY - 1;
        Def := Copy(Def, 3, Length(Def));
        Collision := Assigned(FEditorAndMenuShortcuts.getNode(Key));
        if FKeyboardShortcutsTree.InsertKey(Key, Def, OffX, OffY, Collision)
        then
          CBKeyboard.Items.Add(ShortCutToText(Key));
      end
      else
      begin
        Posi := Pos('disableMenu:', Str);
        if Posi = 1 then
        begin
          Key := StringToShortcut(Str);
          RemoveShortcutFromMainMenu(Key);
        end
        else
        begin
          Posi := Pos('replaceMenu:', Str);
          if Posi = 1 then
          begin
            Key := StringToShortcut(Str);
            Str := getNextLine;
            Posi := Pos('with:', Str);
            if Posi = 1 then
            begin
              Key2 := StringToShortcut(Str);
              ReplaceShortcutFromMainMenu(Key, Key2);
            end;
          end;
        end;
      end;
      Str := getNextLine;
    until Line >= Keys.Count - 1;
    FreeAndNil(Keys);
  end;

begin
  CBKeyboard.Clear;
  CBKeyboard.ItemIndex := 0;
  FKeyboardShortcutsTree.Delete;
  if FileExists(FKeyboardFile) then
    EditKeyboardFile(FKeyboardFile);
  CheckFile(EKeyboardFile, True);
end;

procedure TFConfiguration.RemoveShortcutsFrom(PopupMenu: TSpTBXPopupMenu);
var
  Posi, Line, Key: Integer;
  Str: string;
  Keys: TStringList;
  MenuItem: TTBCustomItem;

  function getNextLine: string;
  begin
    Inc(Line);
    if Line < Keys.Count then
      Result := Keys[Line]
    else
      Result := '';
  end;

begin
  if FileExists(FKeyboardFile) then
  begin
    Keys := TStringList.Create;
    Keys.LoadFromFile(FKeyboardFile);
    Line := -1;
    Str := getNextLine;
    repeat
      Posi := Pos('disableMenu:', Str);
      if Posi = 1 then
      begin
        Key := StringToShortcut(Str);
        for var I := 0 to PopupMenu.Items.Count - 1 do
        begin
          MenuItem := PopupMenu.Items[I];
          if MenuItem.ShortCut = Key then
            MenuItem.ShortCut := 0;
        end;
      end;
      Str := getNextLine;
    until Line >= Keys.Count - 1;
    FreeAndNil(Keys);
  end;

end;

procedure TFConfiguration.MenuAndEditorShortcuts;

  function FillUp(Str: string; Int: Integer): string;
  begin
    Str := Str + StringOfChar(' ', Int);
    Result := Copy(Str, 1, Int);
  end;

  procedure MenuShortCuts;
  var
    Menue: TSpTBXItem;
    Str: string;
  begin
    FMenuKeys.Clear;
    for var I := 0 to FJava.ComponentCount - 1 do
      if FJava.Components[I] is TSpTBXItem then
      begin
        Menue := TSpTBXItem(FJava.Components[I]);
        if Menue.ShortCut <> 0 then
        begin
          FEditorAndMenuShortcuts.InsertKey(Menue.ShortCut, '', 0, 0, False);
          Str := ShortCutToText(Menue.ShortCut);
          if Str <> '' then
            FMenuKeys.Add(FillUp(Str, 30) + ReplaceStr(Menue.Caption, '&', ''));
        end;
      end;
  end;

  procedure EditorShortCuts;
  var
    Str1, Str2: string;
    Keys: TSynEditKeyStrokes;
    AEditor: TFEditForm;
  begin
    FEditorKeys.Clear;
    AEditor := FJava.GetActiveEditor;
    if Assigned(AEditor) then
      Keys := AEditor.Editor.Keystrokes
    else
    begin
      Keys := TSynEditKeyStrokes.Create(nil);
      Keys.ResetDefaults;
    end;
    try
      for var I := 0 to Keys.Count - 1 do
      begin
        FEditorAndMenuShortcuts.InsertKey(Keys[I].ShortCut, '', 0, 0, False);
        Str1 := ShortCutToText(Keys[I].ShortCut);
        Str2 := ShortCutToText(Keys[I].ShortCut2);
        if Str2 <> '' then
          Str1 := Str1 + '+' + Str2;
        EditorCommandToIdent(Keys[I].Command, Str2);
        FEditorKeys.Add(FillUp(Str1, 30) + Copy(Str2, 3, Length(Str2)));
      end;
    finally
      if not Assigned(AEditor) then
        Keys.Free;
    end;
  end;

  procedure AllShortcuts;
  var
    Max: Integer;
  begin
    Max := 0;
    for var I := 0 to FMenuKeys.Count - 1 do
      if Length(FMenuKeys[I]) > Max then
        Max := Length(FMenuKeys[I]);
    Max := Max + 2;
    FAllKeys.Clear;
    for var I := 0 to FEditorKeys.Count - 1 do
      FAllKeys.Add(FillUp(FEditorKeys[I], Max) + '(' + RGKeyboard.Items
        [0] + ')');
    for var I := 0 to FMenuKeys.Count - 1 do
      FAllKeys.Add(FillUp(FMenuKeys[I], Max) + '(' + RGKeyboard.Items[1] + ')');
    for var I := 0 to CBKeyboard.Items.Count - 1 do
      FAllKeys.Add(FillUp(CBKeyboard.Items[I], Max) + '(Code)');
  end;

begin
  ApplyKeyboardShortcuts;
  MenuShortCuts;
  EditorShortCuts;
  AllShortcuts;
  case RGKeyboard.ItemIndex of
    0:
      MKeyboard.Text := FEditorKeys.Text;
    1:
      MKeyboard.Text := FMenuKeys.Text;
    2:
      MKeyboard.Text := FAllKeys.Text;
  end;
end;

procedure TFConfiguration.FormCreate(Sender: TObject);
begin
  Width := PPIScale(812);
  Height := PPIScale(531);
  FODSelect := TOpenDialog.Create(Self);
  FODSelect.Options := [ofPathMustExist, ofFileMustExist, ofEnableSizing];

  FJavaHighlighter := TSynJavaSyn.Create(Self);
  with FJavaHighlighter do
  begin
    CommentAttri.Foreground := clNavy;
    DocumentAttri.Foreground := clNavy;
    NumberAttri.Foreground := clBlue;
    StringAttri.Foreground := clBlue;
  end;

  FHTMLHighlighter := TSynHTMLSyn.Create(Self);
  with FHTMLHighlighter do
  begin
    AndAttri.Foreground := clGreen;
    CommentAttri.Foreground := clTeal;
    KeyAttri.Foreground := clPurple;
  end;

  CBValidClassColor.Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
    cbPrettyNames];
  CBInvalidClassColor.Style := [cbStandardColors, cbExtendedColors,
    cbCustomColor, cbPrettyNames];
  CBObjectColor.Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
    cbPrettyNames];
  CBCommentColor.Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
    cbPrettyNames];
  CBTextColorBox.Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
    cbPrettyNames];
  CBBackgroundColorBox.Style := [cbStandardColors, cbExtendedColors,
    cbCustomColor, cbPrettyNames];
  CBActiveLineColor.Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
    cbPrettyNames];
  CBSDFillingColor.Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
    cbPrettyNames];

  FPascalHighlighter := nil;
  FPHPHighlighter := nil;
  FPHPInternHighlighter := nil;
  FCSSHighlighter := nil;
  FGeneralHighlighter := nil;
  FMultiSynHighlighter := nil;

  FAllClasses := nil;
  FAllInterfaces := nil;
  FAllPackages := nil;
  FAllClasspathClasses := nil;

  for var I := 1 to 21 do
    FControlStructureTemplates[I] := nil;

  FJavaVersion := 0;
  FDocumentationVersion := 0;
  FEditFont := TFont.Create;
  FUMLFont := TFont.Create;
  FStructogramFont := TFont.Create;
  FSequenceFont := TFont.Create;
  RGColors.ItemIndex := 0;
  FAttrBrackets := TSynHighlighterAttributes.Create('Brackets', 'Brackets');
  SetDefaultBracketColor;

  FMyRegistry := TRegistry.Create;
  RegistryForMachine;
  RegistryForUser;

  FLNGTVItems := TStringList.Create;
  for var I := 0 to TVConfiguration.Items.Count - 1 do
    FLNGTVItems.Add(TVConfiguration.Items[I].Text);

  FKeyboardShortcutsTree := TTree.Create;
  FEditorAndMenuShortcuts := TTree.Create;

  FMenuKeys := TStringList.Create;
  FEditorKeys := TStringList.Create;
  FAllKeys := TStringList.Create;
  FMenuKeys.Sorted := True;
  FEditorKeys.Sorted := True;
  FAllKeys.Sorted := True;
  // JavaParser-support
  FImportCache := TStringList.Create;
  FImportCache.Sorted := True;
  FImportCache.CaseSensitive := True;
  FImportCache.Duplicates := dupIgnore;
  SetLength(FVisTabs, 9);
  SetLength(FVisMenus, 4);
  SetLength(FVisToolbars, 6);

  for var I := 0 to PageList.PageCount - 1 do
    PageList.Pages[I].TabVisible := False;
  TVConfiguration.FullExpand;
  ShowPage(1);
  TVConfiguration.TopItem := TVConfiguration.Items[0];
  if VistaOrBetter then
  begin
    SetElevationRequiredState(BFileExtensions);
    SetElevationRequiredState(BJEAssociation);
    SetElevationRequiredState(BJDKInstall);
    SetElevationRequiredState(BDisassemblerInstall);
    SetElevationRequiredState(BManualInstall);
    SetElevationRequiredState(BManualFXInstall);
    SetElevationRequiredState(BTutorialInstall);
    SetElevationRequiredState(BJavabookInstall);
    SetElevationRequiredState(BLejosInstall);
    SetElevationRequiredState(BCheckstyleInstall);
    SetElevationRequiredState(BJalopyInstall);
    SetElevationRequiredState(BJUnitInstall);
  end;
  FVisSelectedTab := 0;
  FAllDocumentations := TStringList.Create;
  FInformed := False;
  LBStyleNames.Sorted := True;
  FLoadedStylesDict := TDictionary<string, string>.Create;
  FExternalStyleFilesDict := TDictionary<string, string>.Create;
  FPreview := TVclStylesPreview.Create(Self);
  FPreview.Parent := StylesPreviewPanel;
  FPreview.Icon := Application.Icon.Handle;
  FPreview.BoundsRect := StylesPreviewPanel.ClientRect;
  TranslateComponent(Self);
  SetupLanguages;
end; // FormCreate

function TFConfiguration.GetPascalHighlighter: TSynPasSyn;
begin
  if not Assigned(FPascalHighlighter) then
  begin
    FPascalHighlighter := TSynPasSyn.Create(Self);
    var
    Path := FEditorFolder + 'styles' + PathDelim;
    FPascalHighlighter.LoadFromFile(Path + 'DefaultColorsPascal.ini',
      FEditorStyle);
  end;
  Result := FPascalHighlighter;
end;

function TFConfiguration.GetCSSHighlighter: TSynCssSyn;
begin
  if not Assigned(FCSSHighlighter) then
  begin
    FCSSHighlighter := TSynCssSyn.Create(Self);
    var
    Path := FEditorFolder + 'styles' + PathDelim;
    FCSSHighlighter.LoadFromFile(Path + 'DefaultColorsCSS.ini', FEditorStyle);
  end;
  Result := FCSSHighlighter;
end;

function TFConfiguration.GetMultiSynHighlighter: TSynMultiSyn;
begin
  if not Assigned(FMultiSynHighlighter) then
  begin
    FMultiSynHighlighter := TSynMultiSyn.Create(Self);
    with FMultiSynHighlighter do
    begin
      DefaultFilter := '*.jsp';
      DefaultHighlighter := FHTMLHighlighter;
      DefaultLanguageName := 'JSP';
      var
      Scheme := TScheme(Schemes.Add);
      Scheme.StartExpr := '<%';
      Scheme.EndExpr := '%>';
      Scheme.Highlighter := FJavaHighlighter;
      Scheme.SchemeName := 'Java';
    end;
  end;
  Result := FMultiSynHighlighter;
end;

function TFConfiguration.GetPhpHighlighter: TSynMultiSyn;
begin
  if not Assigned(FPHPHighlighter) then
  begin
    FPHPHighlighter := TSynMultiSyn.Create(Self);
    with FPHPHighlighter do
    begin
      DefaultFilter := '*.php';
      DefaultHighlighter := FHTMLHighlighter;
      DefaultLanguageName := 'PHP';
      var
      Scheme := TScheme(Schemes.Add);
      Scheme.StartExpr := '(<\?){1}(php){0,1}';
      Scheme.EndExpr := '\?>';
      Scheme.Highlighter := GetPHPInternHighlighter;
      Scheme.SchemeName := 'PHP';
    end;
  end;
  Result := FPHPHighlighter;
end;

// 2: Set CaseSensitive to False
// 3: Set EndExpr til (\?>)
// 4: Set StartExpr til (<\?){1}(php){0,1}

function TFConfiguration.GetPHPInternHighlighter: TSynPHPSyn;
begin
  if not Assigned(FPHPInternHighlighter) then
  begin
    FPHPInternHighlighter := TSynPHPSyn.Create(Self);
    var
    Path := FEditorFolder + 'styles' + PathDelim;
    FPHPInternHighlighter.LoadFromFile(Path + 'DefaultColorsPhp.ini',
      FEditorStyle);
  end;
  Result := FPHPInternHighlighter;
end;

function TFConfiguration.GetGeneralHighlighter: TSynGeneralSyn;
begin
  if not Assigned(FGeneralHighlighter) then
  begin
    FGeneralHighlighter := TSynGeneralSyn.Create(Self);
    var
    Path := FEditorFolder + 'styles' + PathDelim;
    FGeneralHighlighter.LoadFromFile(Path + 'DefaultColorsGeneral.ini',
      FEditorStyle);
    FGeneralHighlighter.IdentifierChars :=
      '_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  end;
  Result := FGeneralHighlighter;
end;

procedure TFConfiguration.Init;
begin
  StyleSelectorShow;
  RegistryToModel;
  FJava.SetOptions;
  CreateBrowserShortCuts;
  if FFirstStartAfterInstallation then
  begin
    ModelToView;
    ViewToModel;
    ModelToRegistry;
  end;
end;

procedure TFConfiguration.RegistryToModel;
var
  Str, Str1, Str2: string;
  Int, Ver: Integer;
  IniFile: TIniFile;
  Posi: Integer;
  Dt1, Dt2: TDateTime;
  StringList: TStringList;
begin
  // General
  if FPortableApplication then
    FSourcepath := AddPortableDrive(ReadStringU('Program', 'Sourcepath',
      FEditorFolder + 'Java\'))
  else
    FSourcepath := AddPortableDrive(ReadStringU('Program', 'Sourcepath',
      GetDocumentsPath));
  FSourcepath := DissolveUsername(FSourcepath);
  if not DirectoryExists(FSourcepath) then
    FSourcepath := GetDocumentsPath;
  FJavaStartClass := AddPortableDrive(ReadStringU('Program', 'StartClass', ''));
  if (FJavaStartClass <> '') and not FileExists(FJavaStartClass) then
    FJavaStartClass := '';
  FJava.ODOpen.FilterIndex := ReadIntegerU('Program', 'FileFilter', 0);
  FWindowStateMaximized := ReadBoolU('Program', 'WindowStateMaximized', True);

  // Font
  FEditFont.Name := ReadStringU('Font', 'Name', DefaultCodeFontName);
  FEditFont.Size := Max(ReadIntegerU('Font', 'Size', 12), 4);
  FUMLFont.Name := ReadStringU('UML', 'Name', 'Segoe UI');
  FUMLFont.Size := Max(ReadIntegerU('UML', 'Size', 12), 4);
  FStructogramFont.Name := ReadStringU('Structogram', 'Name', 'Segoe UI');
  if FStructogramFont.Name = 'Arial' then
    FStructogramFont.Name := 'Segoe UI';
  FStructogramFont.Size := Max(ReadIntegerU('Structogram', 'Size', 12), 4);
  FSequenceFont.Name := ReadStringU('SequenceDiagram', 'Name', 'Segoe UI');
  FSequenceFont.Size := Max(ReadIntegerU('SequenceDiagram', 'Size', 12), 4);
  FShowAlways := True;

  // tab Interpreter
  FJDKFolder := ReadStringDirectory('Java', 'JDK-Folder');
  if FJDKFolder = '' then
    FJDKFolder := JavaDevelopmentKit;
  FJDKFolder := ExcludeTrailingPathDelimiter(FJDKFolder);
  ExpandPath(FJDKFolder + '\bin;');
  SetEnvironmentVar('JAVA_HOME', FJDKFolder);
  FJavaFXFolder := ReadStringDirectory('Java', 'JavaFX-Folder');
  if FJavaFXFolder <> '' then
    SetEnvironmentVar('PATH_TO_FX', FJavaFXFolder + '\lib');
  FJavaFXParameter := ReadStringU('Program', 'JavaFXParameter',
    'javafx.controls,javafx.media,javafx.web');

  FJDKFolderItems := LoadComboBoxItems(AddPortableDrives(ReadStringU('Java',
    'JDK-FolderItems', '')));
  FJavaInterpreter := ReadStringFile('Java', 'Interpreter',
    FJDKFolder + '\bin\java.exe');
  FJavaInterpreterParameter := ReadStringU('Program',
    'InterpreterParameter', '');

  // change FJavaClasspathAdmin since version 12.60
  Str := FEditorFolder + 'JEClasses.jar';
  FJavaClasspathAdmin := AddPortableDrives
    (ReadStringM('Java', 'Classpath', Str));
  Posi := Pos(FEditorFolder, FJavaClasspathAdmin);
  if (Posi > 0) and (Pos(Str, FJavaClasspathAdmin) = 0) then
    Insert('JEClasses.jar', FJavaClasspathAdmin, Posi + Length(FEditorFolder))
  else
  begin
    Posi := Pos(ExcludeTrailingPathDelimiter(FEditorFolder),
      FJavaClasspathAdmin);
    if (Posi > 0) and (Pos(Str, FJavaClasspathAdmin) = 0) then
      Insert('\JEClasses.jar', FJavaClasspathAdmin,
        Posi + Length(FEditorFolder) - 1);
  end;
  Posi := Pos('.;', FJavaClasspathAdmin);
  if Posi > 0 then
    Delete(FJavaClasspathAdmin, Posi, 2);

  FJavaClasspathUser := AddPortableDrives(ReadStringU('Program',
    'Classpath', ''));
  FJavaClasspathAll := AddPortableDrives(ReadStringU('Program',
    'ClasspathAll', ''));
  if FJavaClasspathAll = '' then
    FJavaClasspathAll := FJavaClasspathUser; // for a transition time
  FFileEncoding := ReadStringU('Program', 'FileEncoding', '');
  FCodepage := ReadStringU('Editor', 'Codepage', '');
  FShowInterpreterCall := ReadBoolU('Program', 'ShowInterpreterCall', False);

  // tab Compiler
  FJavaCompiler := ReadStringFile('Java', 'JavaCompiler',
    FJDKFolder + '\bin\javac.exe');
  FJavaCompilerParameter := ReadStringU('Program', 'JavaCompilerParameter',
    '-deprecation -g');
  FJavaCompilerOK := FileExists(FJavaCompiler);
  FCompileInternally := ReadBoolU('Program', 'CompileInternally', False);
  FShowCompilerCall := ReadBoolU('Program', 'ShowCompilerCall', False);
  FCompilerEncoding := ReadStringU('Program', 'CompilerEncoding', '');

  // tab Applet
  FAppletStart := ReadIntegerU('Program', 'AppletStart', 0);
  FShowHTMLforApplet := ReadBoolU('Program', 'ShowHTMLforApplet', True);
  if FPortableApplication then
    FTempDirWithUsername := ReadStringU('Program', 'TempDir',
      FEditorFolder + 'App\Temp')
  else
    FTempDirWithUsername := ReadStringU('Program', 'TempDir',
      TPath.GetTempPath);
  FTempDirWithUsername := IncludeTrailingPathDelimiter
    (AddPortableDrive(FTempDirWithUsername));
  FTempDir := ExpandFileName(DissolveUsername(FTempDirWithUsername));
  if (FTempDir = '') or not SysUtils.ForceDirectories(FTempDir) then
    FTempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));

  if FPortableApplication then
  begin
    IniFile := TIniFile.Create(FTempDir + 'test.ini');
    try
      try
        IniFile.WriteString('test', 'test', 'test');
      except
        FTempDirWithUsername := IncludeTrailingPathDelimiter
          (AddPortableDrive(ReadStringU('Program', 'TempDir',
          TPath.GetTempPath)));
        FTempDir := FTempDirWithUsername;
      end;
    finally
      IniFile.Free;
    end;
  end;

  // tab checkstyle
  FCheckstyle := ReadStringFile('Checkstyle', 'Checkstyle', '');
  FCheckConfiguration := ReadStringFile('Checkstyle', 'Configurationfile', '');
  FCheckParameter := ReadStringU('Checkstyle', 'CheckParameter', '');
  FCheckstyleOK := FileExists(FCheckstyle);

  // tab jalopy
  FJalopy := ReadStringFile('Jalopy', 'Jalopy', '');
  FJalopyConfiguration := ReadStringFile('Jalopy', 'JalopyConfiguration', '');
  FJalopyParameter := ReadStringU('Jalopy', 'JalopyParameter', '');
  FJalopyOK := FileExists(FJalopy);

  // tab programs
  FJavaDebugger := ReadStringFile('Java', 'Debugger',
    FJDKFolder + '\bin\jdb.exe');
  FJavaDebuggerOK := FileExists(FJavaDebugger);
  FJavaDoc := ReadStringFile('Java', 'JavaDoc',
    FJDKFolder + '\bin\JavaDoc.exe');
  FJavaDocOK := FileExists(FJavaDoc);
  FJavaDocParameter := ReadStringU('Program', 'DocParameter',
    '-author -version');

  // tab applets
  FJavaAppletviewer := ReadStringFile('Java', 'Appletviewer',
    FJDKFolder + '\bin\appletviewer.exe');
  FJavaAppletviewerOK := FileExists(FJavaAppletviewer);

  if not FJavaAppletviewerOK then
    FJava.MIAppletviewer.Visible := False;
  if not FileExists(FJDKFolder + '\lib\htmlconverter.jar') then
    // till java 1.6.x
    FJava.MIHTMLforJavaPlugIn.Visible := False;

  // tab disasssembler
  FJavaDisassembler := ReadStringFile('Java', 'Disassembler',
    FJDKFolder + '\bin\javap.exe');
  FJavaDisassemblerItems := LoadComboBoxItems
    (AddPortableDrives(ReadStringU('Program', 'DisassemblerItems', '')));
  FJavaDisassemblerParameter := ReadStringU('Program', 'DisassemblerParameter',
    '-l -c -verbose');

  // tab jar
  FJavaJar := ReadStringFile('Java', 'Jar', FJDKFolder + '\bin\jar.exe');
  FJavaJarOK := FileExists(FJavaJar);
  FJavaJarParameter := ReadStringU('Jar', 'JarParameter', '-cfv');
  FJavaJarManifest := ReadStringU('Jar', 'JarManifest', '');
  FJarCreateCurrent := ReadStringU('Jar', 'JarCreate', '*.class');
  FJarCreateAll := ReadStringU('Jar', 'JarCreateAll', '1#*.class');
  FJarPackFiles := ReadStringU('Jar', 'JarPack', '*.java *.jfm *.uml');
  FJarClassPath := ReadStringU('Jar', 'JarClassPath', 'JEClasses.jar');

  // tab mindstorms
  FMindstormsParameter := ReadStringU('Mindstorms', 'Parameter', '');
  FMindstormsPort := ReadIntegerU('Mindstorms', 'Port', 0);
  FMindstormsIP := ReadStringU('Mindstorms', 'IP', '10.0.1.1');
  FMindstormsMode := ReadBoolU('Mindstorms', 'Mode', False);
  FMindstormsVersion := ReadIntegerU('Mindstorms', 'Version', 0);
  FMindstormsVerbose := ReadBoolU('Mindstorms', 'Verbose', False);
  FMindstormsDebug := ReadBoolU('Mindstorms', 'Debug', True);
  FMindstormsRun := ReadBoolU('Mindstorms', 'Run', True);
  FMindstormsManual := ReadStringFile('Mindstorms', 'Manual', '');
  FRCXManual := ReadStringFile('Mindstorms', 'RCXManual', '');
  FNXTManual := ReadStringFile('Mindstorms', 'NXTManual', '');
  FEV3Manual := ReadStringFile('Mindstorms', 'EV3Manual', '');

  FLejosVerzeichnis := ReadStringDirectory('Mindstorms', 'LejosFolder');
  FRCXFolder := ReadStringDirectory('Mindstorms', 'RCXFolder');
  FNXTFolder := ReadStringDirectory('Mindstorms', 'NXTFolder');
  FEV3Folder := ReadStringDirectory('Mindstorms', 'EV3Folder');

  FLejosCompiler := ReadStringFile('Mindstorms', 'LejosCompiler', '');
  FLejosUploader := ReadStringFile('Mindstorms', 'LejosUploader', '');
  FLejosFlasher := ReadStringFile('Mindstorms', 'LejosFlasher', '');

  FRCXCompiler := ReadStringDirectory('Mindstorms', 'RCXCompiler');
  FRCXUploader := ReadStringDirectory('Mindstorms', 'RCXUploader');
  FRCXFlasher := ReadStringDirectory('Mindstorms', 'RCXFlasher');

  FNXTCompiler := ReadStringDirectory('Mindstorms', 'NXTCompiler');
  FNXTUploader := ReadStringDirectory('Mindstorms', 'NXTUploader');
  FNXTFlasher := ReadStringDirectory('Mindstorms', 'NXTFlasher');

  if FMindstormsMode then
    case FMindstormsVersion of
      0:
        begin
          SetEnvironmentVar('LEJOS_HOME', ANSI2ASCII(FRCXFolder));
          SetEnvironmentVar('RCXTTY', CBMindstormsPort.Text);
          ExpandPath(ANSI2ASCII(FRCXFolder + '\bin') + ';');
          if (FLejosCompiler = '') and FileExists(FLejosUploader) then
            FLejosCompiler := ExtractFilePath(FLejosUploader) + 'lejosjc.bat';
        end;
      1:
        begin
          SetEnvironmentVar('NXJ_HOME', ANSI2ASCII(FNXTFolder));
          ExpandPath(ANSI2ASCII(FNXTFolder + '\bin') + ';');
          if (FLejosCompiler = '') and FileExists(FLejosUploader) then
            FLejosCompiler := ExtractFilePath(FLejosUploader) + 'nxjc.bat';
        end;
      2:
        begin
          SetEnvironmentVar('EV3_HOME', ANSI2ASCII(FEV3Folder));
          SetEnvironmentVar('LEJOS_EV3_JAVA_HOME', ANSI2ASCII(FJDKFolder));
          ExpandPath(ANSI2ASCII(FEV3Folder + '\bin') + ';');
          FLejosCompiler := '';
        end;
    end;

  // tab android mode
  FAndroidMode := ReadBoolU('Android', 'Mode', False);
  FAndroidSDKFolder := ReadStringDirectory('Android', 'AndroidSDKFolder');
  if FAndroidMode and FMindstormsMode then
  begin
    FAndroidMode := False;
    FMindstormsMode := False;
  end;

  // tab documentation
  FJavaManual := ReadStringFile('Program', 'Manual',
    FJDKFolder + '\docs\index.html');
  if IsCHM(FJavaManual) and FileExists(FJavaManual) then
    FJavaCHMRoot := GetRootCHM(FJavaManual) + '\api'
  else if (FJavaManual <> '') and not IsHTTP(FJavaManual) then
  begin
    Str1 := ExtractFilePathEx(FJavaManual) + '\AllClasses-frame.html';
    Str2 := ExtractFilePathEx(FJavaManual) + '\AllClasses-index.html';
    if not FileExists(Str1) and not FileExists(Str2) then
    begin
      Str1 := ExtractFilePathEx(FJavaManual) + '\api\AllClasses-frame.html';
      Str2 := ExtractFilePathEx(FJavaManual) + '\api\AllClasses-index.html';
      if FileExists(Str1) or FileExists(Str2) then
        FJavaManual := ExtractFilePathEx(FJavaManual) + '\api\index.html';
    end;
  end;

  FCHMRootOk := (FJavaCHMRoot <> '');
  FJavaManualFX := ReadStringFile('Program', 'ManualFX',
    FJDKFolder + '\docsfx\api\index.html');

  Str := 'docs;chm;';
  FJavaManualItems := LoadComboBoxItems(AddPortableDrives(ReadStringU('Program',
    'ManualItems', Str)));
  FJavaManualFXItems := LoadComboBoxItems
    (AddPortableDrives(ReadStringU('Program', 'ManualFXItems', Str)));
  FJavaJavaDocs := AddPortableDrives(ReadStringU('Program', 'JavaDocs', ''));
  FJavaJavaDocsAll := AddPortableDrives(ReadStringU('Program',
    'JavaDocsAll', ''));
  if FJavaJavaDocsAll = '' then
    FJavaJavaDocsAll := FJavaJavaDocs; // for a transition time

  FJavaTutorial := ReadStringFile('Program', 'Tutorial', '');
  FJavabook := ReadStringFile('Program', 'Javabook', '');
  FJavaCache := ReadStringDirectory('Program', 'Cache');
  MakeJavaCache(FJavaCache);
  FMaxSearch := ReadIntegerU('Program', 'MaxSearch', 20);
  FJavaTools := GetJavaManual;
  if EndsWith(FJavaTools, '\api') then
    Delete(FJavaTools, Length(FJavaTools) - 3, 4);
  FJavaDemos := FJavaTools;
  Ver := GetDocumentationVersion;
  case Ver of
    1 .. 3:
      FJavaTools := FJavaTools + '\tooldocs\win32\';
    4 .. 5:
      FJavaTools := FJavaTools + '\tooldocs\windows\';
    6 .. 8:
      FJavaTools := FJavaTools + '\technotes\tools\windows\';
  else
    FJavaTools := 'https://docs.oracle.com/javase/' + IntToStr(Ver) + '/tools/';
  end;
  case Ver of
    1 .. 5:
      FJavaDemos := FJavaDemos + '\relnotes\demos.html';
    6 .. 8:
      FJavaDemos := FJavaDemos + '\technotes\samples\demos.html';
  else
    FJavaDemos := '';
  end;

  // tab editor
  FTabWidth := ReadIntegerU('Editor', 'TabWidth', 2);
  FIndent := ReadIntegerU('Editor', 'Indent', 2);
  FIndent1 := StringOfChar(' ', 1 * FIndent);
  FIndent2 := StringOfChar(' ', 2 * FIndent);
  FIndent3 := StringOfChar(' ', 3 * FIndent);
  FAutomaticIndent := ReadBoolU('Editor', 'AutomaticIndent', True);
  FCursorBehindLine := ReadBoolU('Editor', 'CursorBehindLine', True);
  FIndentHelp := ReadBoolU('Editor', 'IndentHelp', True);
  FShowBracketPair := ReadBoolU('Editor', 'BracketPair', True);
  FCommentClosingBrackets := ReadBoolU('Editor',
    'CommentClosingBrackets', True);
  FStructureColoring := ReadBoolU('Editor', 'StructureColoring', True);
  FStructureColoringPlane := ReadBoolU('Editor',
    'StructureColoringPlane', False);
  FStructureColorIntensity := ReadIntegerU('Editor',
    'StructureColorIntensity', 5);
  FGUICodeFolding := ReadBoolU('Editor', 'GUICodeFolding', True);
  FEightyColumnLine := ReadBoolU('Editor', '80ColumnLine', True);
  FInsertControlStructures := ReadBoolU('Editor',
    'InsertControlStructures', True);
  FInsertSemicolons := ReadBoolU('Editor', 'InsertSemicolons', True);
  FLineNumbering := ReadBoolU('Editor', 'LineNumbering', True);
  FAddClosingBracket := ReadBoolU('Editor', 'CompleteBracket', False);
  FIndentAfterBracket := ReadBoolU('Editor', 'IndentAfterBracket', True);
  FKeyboardFile := ReadStringFile('Editor', 'KeyboardFile', '');
  FShowControlFlowSymbols := ReadBoolU('Editor',
    'ShowControlFlowSymbols', True);
  FShowLigatures := ReadBoolU('Editor', 'ShowLigatures', True);
  FCompactLineNumbers := ReadBoolU('Editor', 'CompactLineNumbers', True);

  // tab templates
  FTemplates[1] := ReadStringFile('Templates', 'Program', '');
  FTemplates[2] := ReadStringFile('Templates', 'Frame', '');
  FTemplates[3] := ReadStringFile('Templates', 'Dialog', '');
  FTemplates[4] := ReadStringFile('Templates', 'Applet', '');
  FTemplates[5] := ReadStringFile('Templates', 'JFrame', '');
  FTemplates[6] := ReadStringFile('Templates', 'JDialog', '');
  FTemplates[7] := ReadStringFile('Templates', 'JApplet', '');
  FTemplates[8] := ReadStringFile('Templates', 'Application', '');
  FTemplates[9] := ReadStringFile('Templates', 'Controlstructures', '');
  FTemplates[10] := ReadStringFile('Templates', 'Class', '');
  FTemplates[11] := ReadStringFile('Templates', 'Mindstorms', '');
  FTemplates[12] := ReadStringFile('Templates', 'TestClass', '');
  MakeControlStructureTemplates;

  // tab code
  FCodeCompletionAlways := ReadBoolU('Code', 'Code-Completion-Always', True);
  FCodeCompletionCtrlSpace := ReadBoolU('Code', 'Code-Completion', True);
  FParameterHints := ReadBoolU('Code', 'Parameter-Hints', True);
  FShowClassObject := ReadBoolU('Code', 'ShowClassObject', False);
  FCodeDelay := ReadIntegerU('Code', 'Delay', 100);
  FSelectionSizeMin := ReadIntegerU('Code', 'SelectionSizeMin', 4);
  FSelectionSizeMax := ReadIntegerU('Code', 'SelectionSizeMax', 10);
  FJava.scpJava.TimerInterval := FCodeDelay;
  FJava.scpParams.TimerInterval := FCodeDelay;
  FTooltipWithKey := ReadBoolU('Code', 'TooltipWithKey', True);
  FTooltipAutomatic := ReadBoolU('Code', 'TooltipAutomatic', False);
  FTooltipDelay := ReadIntegerU('Code', 'TooltipDelay', 700);
  FTooltipWidth := ReadIntegerU('Code', 'TooltipWidth', 200);
  FTooltipHeight := ReadIntegerU('Code', 'TooltipHeight', 150);
  FTooltipFontSize := ReadIntegerU('Code', 'TooltipFontSize', 12);

  // tab browser
  FUseIEinternForDocuments := ReadBoolU('Browser',
    'UseIEinternForDocuments', True);
  FOnlyOneBrowserWindow := ReadBoolU('Browser', 'OnlyOneBrowserWindow', False);
  FBrowserTitle := ReadStringU('Browser', 'Title', 'Internet Explorer');
  FBrowserProgram := ReadStringFile('Browser', 'Browser', '');
  if FBrowserProgram = '' then
  begin
    FBrowserProgram := GetExeForExtension('.html');
    FBrowserTitle := BrowserProgToName(FBrowserProgram);
  end;
  FBrowserOpenKeys := ReadStringU('Browser', 'BrowserOpenKeys', 'XXX');
  if FBrowserOpenKeys = 'XXX' then
    FBrowserOpenKeys := ReadStringU('Browser', 'AltKeysBrowser', '');

  DecideProxy;
  FProxyIP := ReadStringU('Browser', 'ProxyIP', FProxyIP);
  FProxyPort := ReadIntegerU('Browser', 'ProxyPort', FProxyPort);
  FWithProxy := ReadBoolU('Browser', 'ProxyEnabled', FWithProxy);

  // tab printer
  FBorderLeft := ReadIntegerU('Printer', 'Left', 20);
  FBorderTop := ReadIntegerU('Printer', 'Top', 20);
  FBorderRight := ReadIntegerU('Printer', 'Right', 20);
  FBorderBottom := ReadIntegerU('Printer', 'Bottom', 20);
  FHeader := ReadStringU('Printer', 'Header', '#%PATH%#');
  FFooter := ReadStringU('Printer', 'Footer', '##- %PAGENUM% -');
  // since version 20.08
  UpdateHeaderFooter;
  FWithLinenumbers := ReadBoolU('Printer', 'Linenumbers', False);
  FLinenumbersInMargin := ReadBoolU('Printer', 'LinenumbersInMargin', True);
  FPrintColored := ReadBoolU('Printer', 'PrintColored', True);
  if HasDefaultPrinter then
    SetPrinter(ReadStringU('Printer', 'Printer', ''));

  // tab comment
  FFreeComment := ReadStringU('Comment', 'Comment', '');
  FCommentKind := ReadIntegerU('Comment', 'Type', 0);
  FJavaAuthor := ReadStringU('Comment', 'Author', '');
  FMethodComment := LoadComboBoxItems(ReadStringU('Comment', 'Method', ''));

  // tab colors
  GUIStyle := ReadStringU('Colors', 'GUIStyle', 'Windows');
  FEditorStyle := ReadStringU('Colors', 'EditorStyle', 'Default');
  ReadUserColors;
  FNoActiveLineColor := ReadBoolU('Colors', 'NoActiveLineColor', True);
  if FNoActiveLineColor then
    FActiveLineColor := clNone
  else
    FActiveLineColor := ReadIntegerU('Colors', 'ActiveLineColor', clRed);
  FNoSyntaxHighlighting := ReadBoolU('Colors', 'NoSyntaxHighlighting', False);

  // tab GUI designer
  FNameFromText := ReadBoolU('Options', 'NameFromText', True);
  FGuiDesignerHints := ReadBoolU('Options', 'GuiDesignerHints', True);
  FSnapToGrid := ReadBoolU('Options', 'SnapToGrid', True);
  FGridSize := ReadIntegerU('Options', 'GridSize', 8);
  FZoomSteps := ReadIntegerU('Options', 'ZoomSteps', 1);
  FGUIFontSize := Max(ReadIntegerU('Options', 'GUIFontSize', 11), 4);
  FGUIFontName := ReadStringU('Options', 'GUIFontName', 'Dialog');
  FFrameWidth := ReadIntegerU('Editor', 'FrameWidth', 300);
  FFrameHeight := ReadIntegerU('Editor', 'FrameHeight', 300);
  FAppletWidth := ReadIntegerU('Editor', 'AppletWidth', 400);
  FAppletHeight := ReadIntegerU('Editor', 'AppletHeight', 300);

  // tab options
  FCreateBAKFiles := ReadBoolU('Options', 'BAKFiles', True);
  FRunsUnderWine := ReadBoolU('Options', 'RunsUnderWine', False);
  FLoadFiles := ReadBoolU('Options', 'LoadFiles', True);
  FUseInterpreterWindowAsConsole := ReadBoolU('Options',
    'UseInterpreterWindowAsConsole', False);
  FAcceptDefaultname := ReadBoolU('Options', 'AcceptDefaultname', False);
  FDebuggerProtocol := ReadBoolU('Options', 'DebuggerProtocol', False);
  FTranslateCompilerErrors := ReadBoolU('Options',
    'TranslateCompilerErrors', True);
  FBorderLayout := ReadBoolU('Options', 'BorderLayout', False);
  FComponentsToolbar := ReadBoolU('Options', 'ComponentsToolbar', True);
  FStrictJavaMode := ReadBoolU('Options', 'StrictJavaMode', True);
  FCheckAge := ReadBoolU('Options', 'CheckAge', True);
  FFileFilter := ReadStringU('Options', 'FileFilter', '');
  FRenameWhenSave := ReadBoolU('Options', 'Rename', True);
  FFontSize := ReadIntegerU('Options', 'FontSize', 9);
  FJava.Font.Size := PPIScale(FFontSize);
  FMaxFileHistory := ReadIntegerU('Options', 'MaxFileHistory', 10);
  FJava.TabsControl.Visible := FComponentsToolbar;
  FJava.PBorder.Visible := FBorderLayout;

  // tab restrictions
  FDOSWindowLocked := ReadBoolM('Java', 'DOS-Window', False);
  FBlockedInternet := ReadBoolM('Java', 'BlockedInternet', False);
  FLockedPaths := ReadBoolM('Java', 'LockedPaths', False);
  FLockedStructogram := ReadBoolM('Java', 'LockedStructogram', False);

  // tab associations
  FAdditionalAssociations := ReadStringU('Associations', 'Additional', '');

  // tab UML
  FValidClassColor := ReadIntegerU('UML', 'ValidClassColor', clWhite);
  FInvalidClassColor := ReadIntegerU('UML', 'InvalidClassColor', clFuchsia);
  FClassHead := ReadIntegerU('UML', 'ClassHead', 0);
  FShadowWidth := ReadIntegerU('UML', 'ShadowWidth', 3);
  FShadowIntensity := ReadIntegerU('UML', 'ShadowIntensity', 8);

  FObjectColor := ReadIntegerU('UML', 'ObjectColor', clYellow);
  FObjectHead := ReadIntegerU('UML', 'ObjectHead', 1);
  FObjectFooter := ReadIntegerU('UML', 'ObjectFooter', 1);
  FObjectCaption := ReadIntegerU('UML', 'ObjectCaption', 0);
  FObjectUnderline := ReadBoolU('UML', 'ObjectUnderline', True);
  FCommentColor := ReadIntegerU('UML', 'CommentColor', clSkyBlue);

  FDiVisibilityFilter := ReadIntegerU('UML', 'Visibility', 0);
  FDiSortOrder := ReadIntegerU('UML', 'SortOrder', 0);
  FDiShowParameter := ReadIntegerU('UML', 'ShowParameter', 4);
  FDiShowIcons := ReadIntegerU('UML', 'ShowIcons', 1);

  // tab uml options
  FPrivateAttributEditable := ReadBoolU('UML', 'PrivateAttribut', True);
  FShowEmptyRects := ReadBoolU('UML', 'ShowEmptyRects', False);
  FConstructorWithVisibility :=
    ReadBoolU('UML', 'ConstructorWithVisibility', False);
  FShowFunctionValues := ReadBoolU('UML', 'ShowFunctionValues', True);
  FObjectLowerCaseLetter := ReadBoolU('UML', 'ObjectLowerCaseLetter', True);
  FShowPublicOnly := ReadBoolU('UML', 'ShowPublicOnly', False);
  FDefaultModifiers := ReadBoolU('UML', 'DefaultModifiers', True);
  FShowObjectsWithMethods := ReadBoolU('UML', 'ShowObjectsWithMethods', False);
  FShowObjectsWithInheritedPrivateAttributes :=
    ReadBoolU('UML', 'ShowObjectsWithInheritedPrivateAttributes', True);
  FAttributesAParametersP := ReadBoolU('UML', 'AttributesAParametersP', False);
  FUseVoid := ReadBoolU('UML', 'UseVoid', False);
  FIntegerInsteadofInt := ReadBoolU('UML', 'IntegerInsteadofInt', False);
  FShowAllNewObjects := ReadBoolU('UML', 'ShowAllNewObjects', True);
  FObjectsWithoutVisibility :=
    ReadBoolU('UML', 'ObjectsWithoutVisibility', True);
  FArrayListAsIntegratedList :=
    ReadBoolU('UML', 'ArrayListAsIntegratedList', False);
  FRelationshipAttributesBold :=
    ReadBoolU('UML', 'RelationshipAttributesBold', True);
  FStartWithDatatype := ReadBoolU('UML', 'StartWithDatatype', False);
  FSetterWithoutThis := ReadBoolU('UML', 'SetterWithoutThis', True);
  FShowClassparameterSeparately :=
    ReadBoolU('UML', 'ShowClassparameterSeparately', False);
  FRoleHidesAttribute := ReadBoolU('UML', 'RoleHidesAttribute', False);
  FUseAbstractForClass := ReadBoolU('UML', 'UseAbstractForClass', False);
  FUseAbstractForMethods := ReadBoolU('UML', 'UseAbstractForMethods', False);

  // tab SVN
  FSVNFolder := ReadStringDirectory('SVN', 'SVNFolder');
  FSVNRepository := ReadStringDirectory('SVN', 'SVNRepository');
  // TODO: Dir or File
  FSubversionOK := FileExists(FSVNFolder + '\svn.exe');
  if FSubversionOK and not Assigned(FSubversion) then
    FSubversion := TFSubversion.Create(FJava);

  // tab git
  FGitFolder := ReadStringDirectory('Git', 'GitFolder');
  FGitLocalRepository := ReadStringDirectory('Git', 'GitLocalRepository');
  FGitRemoteRepository := ReadStringDirectory('Git', 'GitRemoteRepository');
  FGitUserName := ReadStringU('Git', 'User.name', '');
  FGitUserEMail := ReadStringU('Git', 'User.email', '');
  FGitOK := FileExists(FGitFolder + '\bin\git.exe');
  if FGitOK and not Assigned(FGit) then
    FGit := TFGit.Create(FJava);

  // tab JUnit
  FJUnitJarFile := ReadStringDirectory('JUnit', 'JUnitJarFile');
  FJUnitManual := ReadStringFile('JUnit', 'JUnitManual', '');
  FJUnitParameter := ReadStringU('JUnit', 'JUnitParameter', '');
  FJUnitBeforeEach := ReadBoolU('JUnit', 'JUnitBeforeEach', True);
  FJUnitAfterEach := ReadBoolU('JUnit', 'JUnitAfterEach', True);
  FJUnitOk := (FJUnitJarFile <> '') and FileExists(FJUnitJarFile);

  // tab Visibility
  LoadVisibility;
  SetVisibility;
  MakeSystemClasses;

  // tab log files
  FLogfileCompilerWithUserName := ReadStringFile('Logfiles',
    'LogfileCompiler', '');
  FLogfileCompiler := DissolveUsername(FLogfileCompilerWithUserName);
  FLogfileCompilerOK := FileExists(FLogfileCompiler);
  if not FLogfileCompilerOK then
  begin
    CreateMyFile(FLogfileCompiler);
    FLogfileCompilerOK := FileExists(FLogfileCompiler);
  end;

  FLogfileInteractiveWithUsername := ReadStringFile('Logfiles',
    'LogfileInteractive', '');
  FLogfileInteractive := DissolveUsername(FLogfileInteractiveWithUsername);
  FLogfileInteractiveOK := FileExists(FLogfileInteractive);
  if not FLogfileInteractiveOK then
  begin
    CreateMyFile(FLogfileInteractive);
    FLogfileInteractiveOK := FileExists(FLogfileInteractive);
  end;

  if FPortableApplication then
    Str := FEditorFolder + 'App\Log\JELogfileExceptions.txt'
  else
  begin
    Str := TPath.GetHomePath + '\JavaEditor\Log\JELogfileExceptions.txt';
    StringList := Split('\', Str);
    Int := 1;
    while (Int < StringList.Count) and (StringList[Int] <> 'AppData') do
      Inc(Int);
    if Int < StringList.Count then
      StringList[Int - 1] := '%USERNAME%';
    Str := ExcludeTrailingPathDelimiter(ReplaceStr(StringList.Text,
      #$0D#$0A, '\'));
    FreeAndNil(StringList);
  end;
  FLogfileExceptionsWithUsername := ReadStringFile('Logfiles',
    'LogfileExceptions', Str);
  FLogfileExceptions := DissolveUsername(FLogfileExceptionsWithUsername);
  CreateMyFile(FLogfileExceptions);
  FLogfileExceptionsOK := FileExists(FLogfileExceptions);

  // tab language
  FLanguageCode := ReadStringU('Options', 'Language', 'XXX');
  if FLanguageCode = 'XXX' then
  begin
    if Pos('Deutsch', GetUsersWindowsLanguage) > 0 then
      FLanguageCode := 'de'
    else
      FLanguageCode := GetCurrentLanguage;
  end;
  RGLanguages.ItemIndex := Max(0, FLanguagesList.IndexOf(FLanguageCode));

  FRightDockPanelWidth := ReadIntegerU('Panel', 'RightDockPanelWidth', 150);
  FBottomDockPanelHeight := ReadIntegerU('Panel', 'BottomDockPanelHeight', 150);

  // tab structogramS after reading language
  FStructogramS := 'Structogram.' + FLanguageCode;
  FAlgorithm := ReadStringU(FStructogramS, 'Algorithm', _('Algorithm'));
  FInput := ReadStringU(FStructogramS, 'Input', _('Input:'));
  FOutput := ReadStringU(FStructogramS, 'Output', _('Output:'));
  FWhile := ReadStringU(FStructogramS, 'While', _('repeat while'));
  FDoWhile := ReadStringU(FStructogramS, 'DoWhile', _('repeat while'));
  FFor := ReadStringU(FStructogramS, 'For', _('repeat for [i] = [1] to [n]'));

  FYes := ReadStringU(FStructogramS, 'Yes', _('Yes'));
  FNo := ReadStringU(FStructogramS, 'No', _('No'));
  FOther := ReadStringU(FStructogramS, 'Other', _('Other'));

  FGenerateJavaAsProgram := ReadIntegerU('Structogram',
    'GenerateJavaAsProgram', 1);
  if FGenerateJavaAsProgram = 2 then
    FGenerateJavaAsProgram := 1;
  FStructoDatatype := ReadStringU('Structogram', 'Datatype', 'double');
  FSwitchWithCaseLine := ReadBoolU('Structogram', 'SwitchWithCaseLine', False);
  FCaseCount := ReadIntegerU('Structogram', 'CaseCount', 4);
  FStructogramShadowWidth := ReadIntegerU('Structogram', 'ShadowWidth', 3);
  FStructogramShadowIntensity := ReadIntegerU('Structogram',
    'ShadowIntensity', 8);

  // tab sequence diagram
  FSequencediagramS := 'Sequencediagram.' + FLanguageCode;
  FSDObject := ReadStringU(FSequencediagramS, 'Object', _('Object'));
  FSDNew := ReadStringU(FSequencediagramS, 'New', _('new'));
  FSDClose := ReadStringU(FSequencediagramS, 'Close', _('close'));
  FSDFillingcolor := ReadIntegerU(FSequencediagramS, 'Fillingcolor', clYellow);
  FSDNoFilling := ReadBoolU(FSequencediagramS, 'NoFilling', False);
  FSDShowMainCall := ReadBoolU(FSequencediagramS, 'ShowMainCall', False);
  FSDShowParameter := ReadBoolU(FSequencediagramS, 'ShowParameter', True);
  FSDShowReturn := ReadBoolU(FSequencediagramS, 'ShowReturn', True);

  // tab providers
  ReadProviders('LLMAssistant', LLMAssistant.Providers);
  ReadProviders('LLMChat', FLLMChatForm.LLMChat.Providers);

  FJava.FormResize(Self);
  ApplyKeyboardShortcuts;
  // if JEClasses.jar changes it must be updated
  FileAge(FJavaCache + '\classes\classpathclasses.txt', Dt1);
  FileAge(FEditorFolder + 'JEClasses.jar', Dt2);
  if Dt1 < Dt2 then
    MakeClasspathClasses;
  CollectDocumentations;
  GetJavaVersion;
end; // RegistryToModel

function TFConfiguration.TranslateCompilerError(const Err: string): string;
begin
  if Err = ''';'' expected' then
    Exit(_(''';'' expected'));
  if Err = '''('' expected' then
    Exit(_('''('' expected'));
  if Err = ''')'' expected' then
    Exit(_(''')'' expected'));
  if Err = ''']'' expected' then
    Exit(_(''']'' expected'));
  if Err = '''}'' expected' then
    Exit(_('''}'' expected'));
  if Err = '> expected' then
    Exit(_('> expected'));
  if Err = '<identifier> expected' then
    Exit(_('<identifier> expected'));
  if Err = 'not a statement' then
    Exit(_('not a statement'));
  if Err = 'unclosed string literal' then
    Exit(_('unclosed string literal'));
  if Err = 'illegal start of expression' then
    Exit(_('illegal start of expression'));
  if Err = 'illegal start of type' then
    Exit(_('illegal start of type'));
  if Err = 'cannot find symbol' then
    Exit(_('cannot find symbol'));
  if Err = 'class, interface, or enum expected' then
    Exit(_('class, interface, or enum expected'));
  if Err = 'class expected' then
    Exit(_('class expected'));
  if Err = 'reached end of file while parsing' then
    Exit(_('reached end of file while parsing'));
  if Err = 'might not have been initialized' then
    Exit(_('might not have been initialized'));
  if Err = 'is already defined in' then
    Exit(_('is already defined in'));
  if Err = 'array required, but found' then
    Exit(_('array required, but found'));
  if Err = 'incompatible types' then
    Exit(_('incompatible types'));
  if Err = 'incompatible types: missing return value' then
    Exit(_('incompatible types: missing return value'));
  if Err = 'incompatible types: unexpected return value' then
    Exit(_('incompatible types: unexpected return value'));
  if Err = 'incompatible types: possible lossy' then
    Exit(_('incompatible types: possible lossy'));
  if Err = 'invalid method declaration; return type required' then
    Exit(_('invalid method declaration; return type required'));
  if Err = 'non-static cannot be referenced' then
    Exit(_('non-static cannot be referenced'));
  if Err = 'unreachable statement' then
    Exit(_('unreachable statement'));
  if Err = 'bad operand types for binary operator' then
    Exit(_('bad operand types for binary operator'));
  Result := Err;
end;

procedure TFConfiguration.MakeJavaCache(Str: string);
begin
  if Str = '' then
    if FPortableApplication then
      Str := FEditorFolder + 'App\Cache'
    else
      Str := TPath.GetHomePath + '\JavaEditor\Cache';
  FJavaCacheWithUsername := ExcludeTrailingPathDelimiter(Str);
  FJavaCache := ExcludeTrailingPathDelimiter(DissolveUsername(Str));
  if not SysUtils.ForceDirectories(FJavaCache) then
  begin
    if FPortableApplication then
      Str := FEditorFolder + 'App\Cache'
    else
      Str := TPath.GetHomePath + '\JavaEditor\Cache';
    FJavaCacheWithUsername := ExcludeTrailingPathDelimiter(Str);
    FJavaCache := ExcludeTrailingPathDelimiter(DissolveUsername(Str));
    if not SysUtils.ForceDirectories(Str) then
      ErrorMsg(Format(_(LNGCanNotCreateDirectory), [FJavaCache]));
  end;
end;

procedure TFConfiguration.ShowDefaultMindstormsAndroidConfiguration;
begin
  with FJava do
  begin
    MIMindstormsHelp.Visible := FMindstormsMode;
    MIMindstorms.Visible := FMindstormsMode;
    MIRun.ImageIndex := 22;
    if FMindstormsMode then
    begin
      MICompile.ImageIndex := 50;
      TBCompileJava.ImageIndex := 16;
      TBCompileJava.Hint := _(LNGCompileForMindstorms);
      TBRun.Hint := SetRCXNXTEV3(_(LNGTransferToRCX));
      TBCompileAll.ImageIndex := 17;
      case FMindstormsVersion of
        0:
          begin
            TBCompileAll.Hint := 'Download Lejos firmware';
            MIMindstorms.Caption := TBCompileAll.Hint;
          end;
        1:
          begin
            TBCompileAll.Hint := _('NXT options');
            MIMindstorms.Caption := TBCompileAll.Hint;
          end;
        2:
          begin
            TBCompileAll.Hint := 'EV3 Control Center';
            MIMindstorms.Caption := 'EV3 Control Center';
          end;
      end;
    end
    else if FAndroidMode then
    begin
      MIRun.ImageIndex := 87;
      MIRun.Caption := _(LNGTransferToAndroid);
      TBRun.Hint := _(LNGTransferToAndroid);
      TBRun.ImageIndex := 15;
    end
    else
    begin
      MICompile.ImageIndex := 21;
      TBCompileJava.ImageIndex := 11;
      TBCompileJava.Hint := _(LNGCompileWithJava);
      TBCompileAll.ImageIndex := 12;
      TBCompileAll.Hint := _('Compile all');
      TBRun.Hint := _('Run');
    end;
  end;
end;

procedure TFConfiguration.CreateBrowserShortCuts;
var
  Ctrl, Shift: string;
begin
  Ctrl := SmkcCtrl;
  Shift := SmkcShift;
  CBOpenBrowserShortcut.Items.Clear;
  for var I := 65 to 65 + 25 do
    CBOpenBrowserShortcut.Items.Add('<' + Ctrl + Chr(I) + '>');
  for var I := 65 to 65 + 25 do
    CBOpenBrowserShortcut.Items.Add('<' + Ctrl + Shift + Chr(I) + '>');
end;

procedure TFConfiguration.RegistryForMachine;
begin
  FEditorFolder := ExtractFilePath(ParamStr(0));
  ChDir(FEditorFolder);
  var
  Str := ParamStr(1);
  if (Str = '') or (UpperCase(ExtractFileExt(Str)) <> '.INI') then
    Str := FEditorFolder + 'JEMachine.ini'
  else
    Str := ExpandUNCFileName(Str);
  FPortAppDrive := ExtractFileDrive(Str); // with UNC we get \\Server\Freigabe
  if Pos(':', FPortAppDrive) > 0 then
    FPortAppDrive := Copy(FPortAppDrive, 1, 1);
  FUseRegistry := not FileExists(Str);

  if FUseRegistry then
  begin
    FMachineIniFile := nil;
    FPortableApplication := False;
  end
  else
  begin
    FMachineIniFile := TMemIniFile.Create(Str);
    FPortableApplication := FMachineIniFile.ReadBool('Java',
      'PortableApplication', False);
  end;
  FWriteProtection := GetWriteProtection;
end;

procedure TFConfiguration.RegistryForUser;
var
  Path, Filename: string;
  AFile: TFileStream;
begin
  FFirstStartAfterInstallation := False;
  if FUseRegistry then
  begin
    with FMyRegistry do
    begin
      RootKey := HKEY_CURRENT_USER;
      Access := KEY_READ;
      if not OpenKey('\Software\JavaEditor', False) then
      begin
        FFirstStartAfterInstallation := True;
        FJavaHighlighter.SaveToRegistry(HKEY_CURRENT_USER,
          GetRegPath + '\Java');
        FHTMLHighlighter.SaveToRegistry(HKEY_CURRENT_USER,
          GetRegPath + '\HTML');
      end;
      Path := TPath.GetHomePath + '\JavaEditor\';
      if not SysUtils.DirectoryExists(Path) then
        SysUtils.ForceDirectories(Path);
      FHomeDir := Path;
    end;
    FUserIniFile := nil;
  end
  else
  begin
    Path := FMachineIniFile.ReadString('User', 'HomeDir', '<nix>');
    if Path = '<nix>' then
    begin
      ErrorMsg(Format
        (_('In section [user] of the configuration file "%s" the value'),
        [FMachineIniFile.Filename]) + #13#10 +
        _('of the key "HomeDir" for the home directory of the user is not set.')
        );
      FUserIniFile := nil;
      Exit;
    end;
    Path := WithTrailingSlash(AddPortableDrive(DissolveUsername(Path)));
    if not SysUtils.DirectoryExists(Path) then
      if not SysUtils.ForceDirectories(Path) then begin
        ErrorMsg(Format(_('Could not create folder %s!'), [Path]));
        FUserIniFile := nil;
        Exit;
      end;
    FHomeDir := Path;
    Filename := TPath.Combine(Path, 'JEUser.ini');
    if not FileExists(Filename) then
    begin
      FFirstStartAfterInstallation := True;
      try
        AFile := TFileStream.Create(Filename, fmCreate or fmOpenWrite);
        AFile.Free;
        FJavaHighlighter.SaveToFile(TPath.Combine(Path, 'JEJavaCol.ini'));
        FHTMLHighlighter.SaveToFile(TPath.Combine(Path, 'JEHTMLCol.ini'));
      except
        on E: Exception do begin
          ErrorMsg(Format(_('Could not create path %s!'), [Filename]));
          FUserIniFile := nil;
          Exit;
        end;
      end;
    end;

    try
      FUserIniFile := TIniFile.Create(Filename);
    except
      on E: Exception do
      begin
        ErrorMsg(Format(_('Could not open the preferencesfile %s!'), [Filename]) +
          ' ' + E.Message);
        FUserIniFile := nil;
      end;
    end;
  end;
end;

function TFConfiguration.GetWriteProtection: Boolean;
begin
  Result := False;
  if FUseRegistry then
    Result := not IsAdministrator
  else
    try
      FMachineIniFile.WriteString('Java', 'WriteProtection', 'ok');
      FMachineIniFile.UpdateFile;
    except
      Result := True;
    end;
end;

function TFConfiguration.JavaDevelopmentKit: string;
const
  JDK = '\Software\JavaSoft\Java Development Kit';
var
  StringList: TStringList;
  Home: string;
  Int: Integer;
begin
  Result := '';
  with FMyRegistry do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    Access := KEY_READ;
    try
      if OpenKey(JDK, False) then
      begin
        StringList := TStringList.Create;
        GetKeyNames(StringList);
        Home := '';
        Int := StringList.Count - 1;
        while Int >= 0 do
        begin
          if OpenKey(JDK + '\' + StringList[Int], False) then
          begin
            Home := ReadString('JavaHome');
            if SysUtils.DirectoryExists(Home) then
              Break
            else
              Home := '';
          end;
          Dec(Int);
        end;
        Result := Home;
        FreeAndNil(StringList);
      end;
    finally
      CloseKey;
    end;
  end;
end;

procedure TFConfiguration.ModelToView;
begin
  with FJava do
  begin
    // tab interpreter
    ShortenPath(CBJDKFolder, FJDKFolder);
    CBJDKFolder.Items.Text := FJDKFolderItems;
    ShortenPath(EInterpreter, FJavaInterpreter);
    EInterpreterParameter.Text := FJavaInterpreterParameter;
    EJavaFXFolder.Text := FJavaFXFolder;
    EJavaFXParameter.Text := FJavaFXParameter;
    EClasspathAdmin.Text := FJavaClasspathAdmin;
    EClasspathUser.Text := FJavaClasspathUser;
    EJavaDocs.Text := FJavaJavaDocs;
    CBShowInterpreterCall.Checked := FShowInterpreterCall;
    CBFileEncoding.Text := FFileEncoding;
    CBCodepage.Text := FCodepage;

    // tab compiler
    ShortenPath(EJavaCompiler, FJavaCompiler);
    EJavaCompilerParameter.Text := FJavaCompilerParameter;
    CBUseJavaCompilerInternally.Checked := FCompileInternally;
    CBShowCompilerCall.Checked := FShowCompilerCall;
    CBCompilerEncoding.Text := FCompilerEncoding;

    // tab programs
    ShortenPath(EDebugger, FJavaDebugger);
    ShortenPath(EJavaDoc, FJavaDoc);
    EDocParameter.Text := FJavaDocParameter;

    // tab applets
    ShortenPath(EAppletviewer, FJavaAppletviewer);
    RGApplet.ItemIndex := FAppletStart;
    CBShowHTMLforApplet.Checked := FShowHTMLforApplet;

    // tab disassembler
    ShortenPath(CBDisassembler, FJavaDisassembler);
    CBDisassembler.Items.Text := FJavaDisassemblerItems;
    EDisassemblerParameter.Text := FJavaDisassemblerParameter;

    // tab checkstyle
    ShortenPath(ECheckstyle, FCheckstyle);
    ShortenPath(ECheckstyleConfiguration, FCheckConfiguration);
    ECheckstyleParameter.Text := FCheckParameter;

    // tab jalopy
    ShortenPath(EJalopy, FJalopy);
    ShortenPath(EJalopyConfiguration, FJalopyConfiguration);
    EJalopyParameter.Text := FJalopyParameter;

    // tab jar
    ShortenPath(EJar, FJavaJar);
    EJarParameter.Text := FJavaJarParameter;
    EJarManifest.Text := FJavaJarManifest;
    EJarCreate.Text := FJarCreateCurrent;
    CBJarPack.Text := FJarPackFiles;
    EJarClasspath.Text := FJarClassPath;

    // tab templates
    ShortenPath(ETemplateConsole, FTemplates[1]);
    ShortenPath(ETemplateFrame, FTemplates[2]);
    ShortenPath(ETemplateDialog, FTemplates[3]);
    ShortenPath(ETemplateApplet, FTemplates[4]);
    ShortenPath(ETemplateJFrame, FTemplates[5]);
    ShortenPath(ETemplateJDialog, FTemplates[6]);
    ShortenPath(ETemplateJApplet, FTemplates[7]);
    ShortenPath(ETemplateApplication, FTemplates[8]);
    ShortenPath(ETemplateControlstructure, FTemplates[9]);
    ShortenPath(ETemplateClass, FTemplates[10]);
    ShortenPath(EMindstormsTemplate, FTemplates[11]);
    ShortenPath(ETemplateJUnitTest, FTemplates[12]);

    // tab documentation
    CBManual.Items.Text := FJavaManualItems;
    ShortenPath(CBManual, FJavaManual);
    CBManualFX.Items.Text := FJavaManualFXItems;
    ShortenPath(CBManualFX, FJavaManualFX);
    ShortenPath(ETutorial, FJavaTutorial);
    ShortenPath(EJavabook, FJavabook);
    ShortenPath(ECache, FJavaCacheWithUsername);
    UDMaxSearch.Position := FMaxSearch;

    // tab editor
    UDTabWidth.Position := FTabWidth;
    UDIndent.Position := FIndent;
    CBIndentHelp.Checked := FIndentHelp;
    CBCursorBehindLine.Checked := FCursorBehindLine;
    CBAutomaticIndent.Checked := FAutomaticIndent;

    CBLineNumbering.Checked := FLineNumbering;
    CBCompleteBracket.Checked := FAddClosingBracket;
    CBIndentafterBracket.Checked := FIndentAfterBracket;
    CBShowBracketpair.Checked := FShowBracketPair;
    CBCommentClosingBrackets.Checked := FCommentClosingBrackets;
    CBStructureColoring.Checked := FStructureColoring;
    CBStructureColoringPlane.Checked := FStructureColoringPlane;
    CBGUICodeFolding.Checked := FGUICodeFolding;
    CB80Columnline.Checked := FEightyColumnLine;

    UDIntensity.Position := FStructureColorIntensity;
    CBInsertControlStructures.Checked := FInsertControlStructures;
    CBInsertSemicolons.Checked := FInsertSemicolons;
    CBShowControlFlowSymbols.Checked := FShowControlFlowSymbols;
    CBShowLigatures.Checked := FShowLigatures;
    CBCompactLineNumbers.Checked := FCompactLineNumbers;

    // tab code
    RBCodeCompletionCtrlSpace.Checked := FCodeCompletionCtrlSpace;
    RBCodeCompletionAlways.Checked := FCodeCompletionAlways;
    CBParameterHints.Checked := FParameterHints;
    CBShowClassObject.Checked := FShowClassObject;
    TBDelay.Position := FCodeDelay;
    ESelectionSizeMin.Text := IntToStr(FSelectionSizeMin);
    ESelectionSizeMax.Text := IntToStr(FSelectionSizeMax);
    CBTooltipWithKey.Checked := FTooltipWithKey;
    CBTooltipAutomatic.Checked := FTooltipAutomatic;
    TBTooltipDelay.Position := FTooltipDelay;

    // tab colors
    CBNoActiveLineColor.Checked := FNoActiveLineColor;
    CBActiveLineColor.Selected := FActiveLineColor;
    CBActiveLineColor.Enabled := not FNoActiveLineColor;
    CBNoSyntaxHighlighting.Checked := FNoSyntaxHighlighting;
    ReadEditorStyleNames;
    CBEditorStyles.Text := FEditorStyle;

    // tab browser
    CBUseIEinternForDocuments.Checked := FUseIEinternForDocuments;
    CBOnlyOneBrowserWindow.Checked := FOnlyOneBrowserWindow;
    EBrowserTitle.Text := FBrowserTitle;
    CBOpenBrowserShortcut.Text := FBrowserOpenKeys;
    ShortenPath(EBrowserProgram, FBrowserProgram);
    EProxyIP.Text := FProxyIP;
    EProxyPort.Text := IntToStr(FProxyPort);
    CBUseProxy.Checked := FWithProxy;

    // tab printer
    EBorderLeft.Text := IntToStr(FBorderLeft);
    EBorderTop.Text := IntToStr(FBorderTop);
    EBorderRight.Text := IntToStr(FBorderRight);
    EBorderBottom.Text := IntToStr(FBorderBottom);
    EHeader.Text := FHeader;
    EFooter.Text := FFooter;
    CBLinenumbers.Checked := FWithLinenumbers;
    CBLinenumbersInBorder.Checked := FLinenumbersInMargin;
    CBPrintColored.Checked := FPrintColored;

    // tab comment
    RGComment.ItemIndex := FCommentKind;
    EAuthor.Text := FJavaAuthor;
    RGCommentClick(Self);
    MMethodComment.Text := FMethodComment;

    // tab keyboard
    ShortenPath(EKeyboardFile, FKeyboardFile);

    // tab mindstorms
    case FMindstormsVersion of
      0:
        begin
          FLejosVerzeichnis := FRCXFolder;
          FLejosCompiler := FRCXCompiler;
          FLejosUploader := FRCXUploader;
          FLejosFlasher := FRCXFlasher;
          FMindstormsManual := FRCXManual;
        end;
      1:
        begin
          FLejosVerzeichnis := FNXTFolder;
          FLejosCompiler := FNXTCompiler;
          FLejosUploader := FNXTUploader;
          FLejosFlasher := FNXTFlasher;
          FMindstormsManual := FNXTManual;
        end;
      2:
        begin
          FLejosVerzeichnis := FEV3Folder;
          FMindstormsManual := FEV3Manual;
        end;
    end;
    ShortenPath(ELejosFolder, FLejosVerzeichnis);
    ShortenPath(ELejosCompiler, FLejosCompiler);
    ShortenPath(ELejosUploader, FLejosUploader);
    ShortenPath(ELejosFlasher, FLejosFlasher);
    ShortenPath(EMindstormsManual, FMindstormsManual);

    EMindstormsParameter.Text := FMindstormsParameter;
    CBMindstormsPort.ItemIndex := FMindstormsPort;
    CBMindstormsModus.Checked := FMindstormsMode;
    EMindstormsIP.Text := FMindstormsIP;
    RGMindstormsVersion.ItemIndex := FMindstormsVersion;

    // tab android
    CBAndroidMode.Checked := FAndroidMode;
    EAndroidSDKFolder.Text := FAndroidSDKFolder;

    // tab GUI designer
    CBNameFromText.Checked := FNameFromText;
    CBGuiDesignerHints.Checked := FGuiDesignerHints;
    CBSnapToGrid.Checked := FSnapToGrid;
    UDGridSize.Position := FGridSize;
    UDZoomSteps.Position := FZoomSteps;
    EFrameWidth.Text := IntToStr(FFrameWidth);
    EFrameheight.Text := IntToStr(FFrameHeight);
    EAppletwidth.Text := IntToStr(FAppletWidth);
    EAppletheight.Text := IntToStr(FAppletHeight);

    // tab options
    CBBAKFiles.Checked := FCreateBAKFiles;
    CBRunsUnderWine.Checked := FRunsUnderWine;
    CBLoadFiles.Checked := FLoadFiles;
    CBUseInterpreterWindowAsConsole.Checked := FUseInterpreterWindowAsConsole;
    CBAcceptdefaultname.Checked := FAcceptDefaultname;
    CBRenameWhenSave.Checked := FRenameWhenSave;
    CBDebuggerProtocol.Checked := FDebuggerProtocol;
    CBTranslateCompilerErrors.Checked := FTranslateCompilerErrors;
    CBStrictJavaMode.Checked := FStrictJavaMode;
    UDFileHistory.Position := FMaxFileHistory;
    UDFontSize.Position := FFontSize;
    EFileFilter.Text := FFileFilter;
    CBCheckAge.Checked := FCheckAge;
    ShortenPath(ETempFolder, FTempDirWithUsername);

    // tab restrictions
    CBDosWindow.Checked := FDOSWindowLocked;
    CBBlockedInternet.Checked := FBlockedInternet;
    CBLockedPaths.Checked := FLockedPaths;
    CBLockedStructogram.Checked := FLockedStructogram;
    if not IsAdministrator then
    begin
      CBDosWindow.Enabled := False;
      CBBlockedInternet.Enabled := False;
      CBLockedPaths.Enabled := False;
      CBLockedStructogram.Enabled := False;
    end;

    // tab associations
    CBAssociationJava.Checked := HasAssociationWithJavaeditor('.java');
    CBAssociationJfm.Checked := HasAssociationWithJavaeditor('.jfm');
    CBAssociationUml.Checked := HasAssociationWithJavaeditor('.uml');
    CBAssociationJep.Checked := HasAssociationWithJavaeditor('.jep');
    CBAssociationHtml.Checked := HasAssociationWithJavaeditor('.html');
    CBAssociationTxt.Checked := HasAssociationWithJavaeditor('.txt');
    CBAssociationJsp.Checked := HasAssociationWithJavaeditor('.jsp');
    CBAssociationPhp.Checked := HasAssociationWithJavaeditor('.php');
    CBAssociationCss.Checked := HasAssociationWithJavaeditor('.css');
    CBAssociationInc.Checked := HasAssociationWithJavaeditor('.inc');
    CBAssociationJsg.Checked := HasAssociationWithJavaeditor('.jsg');
    CBAssociationJSD.Checked := HasAssociationWithJavaeditor('.jsd');

    EAdditionalAssociations.Text := FAdditionalAssociations;
    EJEAssociation.Text := GetRegisteredJavaEditor;

    // tab UML
    CBValidClassColor.Selected := FValidClassColor;
    CBInvalidClassColor.Selected := FInvalidClassColor;
    RGClassHead.ItemIndex := FClassHead;
    CBObjectColor.Selected := FObjectColor;
    RGObjectHead.ItemIndex := FObjectHead;
    RGObjectFooter.ItemIndex := FObjectFooter;
    RGObjectCaption.ItemIndex := FObjectCaption;
    CBObjectUnderline.Checked := FObjectUnderline;
    CBCommentColor.Selected := FCommentColor;

    RGAttributsMethodsDisplay.ItemIndex := 4 - FDiVisibilityFilter;
    RGSequenceAttributsMethods.ItemIndex := FDiSortOrder;
    RGParameterDisplay.ItemIndex := FDiShowParameter;
    RGVisibilityDisplay.ItemIndex := 2 - FDiShowIcons;

    // tab uml options
    CBUMLEdit.Checked := FPrivateAttributEditable;
    CBShowEmptyRects.Checked := FShowEmptyRects;
    CBShowFunctionvalues.Checked := FShowFunctionValues;
    CBConstructorWithVisibility.Checked := FConstructorWithVisibility;
    CBLowerCaseLetter.Checked := FObjectLowerCaseLetter;
    CBOpenPublicClasses.Checked := FShowPublicOnly;
    CBDefaultModifiers.Checked := FDefaultModifiers;
    CBShowObjectsWithMethods.Checked := FShowObjectsWithMethods;
    CBShowObjectsWithInheritedPrivateAttributes.Checked :=
      FShowObjectsWithInheritedPrivateAttributes;
    CBAttributesAParametersP.Checked := AttributesAParametersP;
    UDShadowWidth.Position := FShadowWidth;
    UDShadowIntensity.Position := FShadowIntensity;
    CBUseVoid.Checked := FUseVoid;
    CBIntegerInsteadofInt.Checked := FIntegerInsteadofInt;
    CBShowAllNewObjects.Checked := FShowAllNewObjects;
    CBObjectsWithoutVisibility.Checked := FObjectsWithoutVisibility;
    CBArrayListAsIntegratedList.Checked := FArrayListAsIntegratedList;
    CBRelationshipAttributesBold.Checked := FRelationshipAttributesBold;
    CBStartWithDatatype.Checked := FStartWithDatatype;
    CBSetterWithoutThis.Checked := FSetterWithoutThis;
    CBShowClassparameterSeparately.Checked := FShowClassparameterSeparately;
    CBRoleHidesAttribute.Checked := FRoleHidesAttribute;
    CBUseAbstractForClass.Checked := FUseAbstractForClass;
    CBUseAbstractForMethods.Checked := FUseAbstractForMethods;

    // tab visibility
    VisibilityModelToView;

    // tab LLM Assistant
    CopyProviders(LLMAssistant.Providers, FTempProviders);
    CBProvider.ItemIndex := Integer(FTempProviders.Provider);
    LLMAssistantModelToView(LLMAssistantSettings);

    CopyProviders(FLLMChatForm.LLMChat.Providers, FTempChatProviders);
    CBChatProvider.ItemIndex := Integer(FTempChatProviders.Provider);
    LLMChatModelToView(LLMChatSettings);

    // tab SVN
    ShortenPath(ESVNFolder, FSVNFolder);
    ShortenPath(CBRepository, FSVNRepository);

    // tab git
    ShortenPath(EGitFolder, FGitFolder);
    ShortenPath(CBLocalRepository, FGitLocalRepository);
    ShortenPath(CBRemoteRepository, FGitRemoteRepository);
    EUserName.Text := FGitUserName;
    EUserEMail.Text := FGitUserEMail;

    // tab JUnit
    ShortenPath(EJUnitJarFile, FJUnitJarFile);
    ShortenPath(EJUnitManual, FJUnitManual);
    EJunitParameter.Text := FJUnitParameter;
    CBJUnitBeforeEach.Checked := FJUnitBeforeEach;
    CBJUnitAfterEach.Checked := FJUnitAfterEach;

    // tab og files
    ShortenPath(ELogfileCompiler, FLogfileCompilerWithUserName);
    ShortenPath(ELogfileInteractive, FLogfileInteractiveWithUsername);
    ShortenPath(ELogfileExceptions, FLogfileExceptionsWithUsername);

    // tab structogramS
    EAlgorithm.Text := FAlgorithm;
    EInput.Text := FInput;
    EOutput.Text := FOutput;
    EWhile.Text := FWhile;
    EDoWhile.Text := FDoWhile;
    EFor.Text := FFor;
    EYes.Text := FYes;
    ENo.Text := FNo;
    EOther.Text := FOther;
    RGGenerateJavacode.ItemIndex := FGenerateJavaAsProgram;
    CBDataType.Text := FStructoDatatype;
    CBSwitchWithCaseLine.Checked := FSwitchWithCaseLine;
    ECaseCount.Text := IntToStr(FCaseCount);
    UDStructogramShadowWidth.Position := FStructogramShadowWidth;
    UDStructogramShadowIntensity.Position := FStructogramShadowIntensity;

    // tab sequence diagram
    ESDObject.Text := FSDObject;
    ESDNew.Text := FSDNew;
    ESDClose.Text := FSDClose;
    CBSDFillingColor.Selected := FSDFillingcolor;
    CBSDNoFilling.Checked := FSDNoFilling;
    CBSDShowMainCall.Checked := FSDShowMainCall;
    CBSDShowParameter.Checked := FSDShowParameter;
    CBSDShowReturn.Checked := FSDShowReturn;

    // tab styles
    ECurrentStyle.Text := TStyleManager.ActiveStyle.Name;
    ECurrentStyle.Hint := ECurrentStyle.Text;
    StyleSelectorShow;
    CollectDocumentations;
  end;
  RGLanguages.ItemIndex := Max(0, FLanguagesList.IndexOf(FLanguageCode));
end; // ModelToView

procedure TFConfiguration.ModelToRegistry;
var
  Cp1, Cp2: string;
begin
  // Sonstiges
  WriteStringU('Program', 'StartClass', RemovePortableDrive(FJavaStartClass));

  // tab interpreter
  WriteStringDirectory('Java', 'JDK-Folder', FJDKFolder);
  WriteStringU('Java', 'JDK-FolderItems',
    RemovePortableDrives(SaveComboBoxItems(FJDKFolderItems)));

  WriteStringFile('Java', 'Interpreter', FJavaInterpreter);
  WriteStringU('Program', 'InterpreterParameter', FJavaInterpreterParameter);
  Cp1 := AddPortableDrives(ReadStringM('Java', 'Classpath', ''));
  WriteStringDirectory('Java', 'JavaFX-Folder',
    RemovePortableDrive(FJavaFXFolder));
  WriteStringU('Program', 'JavaFXParameter', FJavaFXParameter);

  WriteStringM('Java', 'Classpath', RemovePortableDrives(FJavaClasspathAdmin));
  Cp2 := AddPortableDrives(ReadStringU('Program', 'Classpath', ''));
  WriteStringU('Program', 'Classpath',
    RemovePortableDrives(FJavaClasspathUser));
  if (Cp1 <> FJavaClasspathAdmin) or (Cp2 <> FJavaClasspathUser) then
    MakeClasspathClasses;

  WriteStringU('Program', 'ClasspathAll',
    RemovePortableDrives(FJavaClasspathAll));
  WriteStringU('Program', 'FileEncoding', FFileEncoding);
  WriteStringU('Editor', 'Codepage', FCodepage);
  WriteBoolU('Program', 'ShowInterpreterCall', FShowInterpreterCall);

  // tab compiler
  WriteStringFile('Java', 'JavaCompiler', FJavaCompiler);
  WriteStringU('Program', 'JavaCompilerParameter', FJavaCompilerParameter);
  WriteBoolU('Program', 'CompileInternally', FCompileInternally);
  WriteBoolU('Program', 'ShowCompilerCall', FShowCompilerCall);
  WriteStringU('Program', 'CompilerEncoding', FCompilerEncoding);

  // tab programs
  WriteStringFile('Java', 'Debugger', FJavaDebugger);
  WriteStringFile('Java', 'JavaDoc', FJavaDoc);
  WriteStringU('Program', 'DocParameter', FJavaDocParameter);

  // tab applets
  WriteStringFile('Java', 'Appletviewer', FJavaAppletviewer);
  WriteIntegerU('Program', 'AppletStart', FAppletStart);
  WriteBoolU('Program', 'ShowHTMLforApplet', FShowHTMLforApplet);
  WriteStringU('Program', 'TempDir', RemovePortableDrive(FTempDirWithUsername));

  // tab jar
  WriteStringFile('Java', 'Jar', FJavaJar);
  WriteStringU('Jar', 'JarParameter', FJavaJarParameter);
  WriteStringU('Jar', 'JarManifest', FJavaJarManifest);
  WriteStringU('Jar', 'JarCreate', FJarCreateCurrent);
  WriteStringU('Jar', 'JarCreateAll', FJarCreateAll);
  WriteStringU('Jar', 'JarPack', FJarPackFiles);
  WriteStringU('Jar', 'JarClassPath', FJarClassPath);

  // tab disassembler
  WriteStringFile('Java', 'Disassembler', FJavaDisassembler);
  WriteStringU('Program', 'DisassemblerParameter', FJavaDisassemblerParameter);
  WriteStringU('Program', 'DisassemblerItems',
    RemovePortableDrives(SaveComboBoxItems(CBDisassembler.Items.Text)));

  // tab templates
  WriteStringFile('Templates', 'Program', FTemplates[1]);
  WriteStringFile('Templates', 'Frame', FTemplates[2]);
  WriteStringFile('Templates', 'Dialog', FTemplates[3]);
  WriteStringFile('Templates', 'Applet', FTemplates[4]);
  WriteStringFile('Templates', 'JFrame', FTemplates[5]);
  WriteStringFile('Templates', 'JDialog', FTemplates[6]);
  WriteStringFile('Templates', 'JApplet', FTemplates[7]);
  WriteStringFile('Templates', 'Application', FTemplates[8]);
  WriteStringFile('Templates', 'Controlstructures', FTemplates[9]);
  WriteStringFile('Templates', 'Class', FTemplates[10]);
  WriteStringFile('Templates', 'Mindstorms', FTemplates[11]);
  WriteStringFile('Templates', 'JunitTest', FTemplates[12]);

  // tab documentation
  WriteStringFile('Program', 'Manual', FJavaManual);
  WriteStringU('Program', 'ManualItems',
    RemovePortableDrives(SaveComboBoxItems(FJavaManualItems)));
  WriteStringFile('Program', 'ManualFX', FJavaManualFX);
  WriteStringU('Program', 'ManualFXItems',
    RemovePortableDrives(SaveComboBoxItems(FJavaManualFXItems)));
  WriteStringU('Program', 'JavaDocs', RemovePortableDrives(FJavaJavaDocs));
  WriteStringU('Program', 'JavaDocsAll',
    RemovePortableDrives(FJavaJavaDocsAll));
  WriteStringFile('Program', 'Tutorial', FJavaTutorial);
  WriteStringFile('Program', 'Javabook', FJavabook);
  WriteStringDirectory('Program', 'Cache', FJavaCacheWithUsername);
  WriteIntegerU('Program', 'MaxSearch', FMaxSearch);
  WriteIntegerU('Program', 'Update', ReadIntegerU('Program', 'Update', 0));

  // tab editor
  WriteIntegerU('Editor', 'TabWidth', FTabWidth);
  WriteIntegerU('Editor', 'Indent', FIndent);
  WriteBoolU('Editor', 'IndentHelp', FIndentHelp);
  WriteBoolU('Editor', 'CursorBehindLine', FCursorBehindLine);
  WriteBoolU('Editor', 'AutomaticIndent', FAutomaticIndent);
  WriteBoolU('Editor', 'LineNumbering', FLineNumbering);
  WriteBoolU('Editor', 'CompleteBracket', FAddClosingBracket);
  WriteBoolU('Editor', 'IndentAfterBracket', FIndentAfterBracket);
  WriteBoolU('Editor', 'BracketPair', FShowBracketPair);
  WriteBoolU('Editor', 'CommentClosingBrackets', FCommentClosingBrackets);
  WriteBoolU('Editor', 'StructureColoring', FStructureColoring);
  WriteBoolU('Editor', 'StructureColoringPlane', FStructureColoringPlane);
  WriteIntegerU('Editor', 'StructureColorIntensity', FStructureColorIntensity);
  WriteBoolU('Editor', 'GUICodeFolding', FGUICodeFolding);
  WriteBoolU('Editor', '80ColumnLine', FEightyColumnLine);
  WriteBoolU('Editor', 'InsertControlStructures', FInsertControlStructures);
  WriteBoolU('Editor', 'InsertSemicolons', FInsertSemicolons);
  WriteBoolU('Editor', 'ShowControlFlowSymbols', FShowControlFlowSymbols);
  WriteBoolU('Editor', 'ShowLigatures', FShowLigatures);
  WriteBoolU('Editor', 'CompactLineNumbers', FCompactLineNumbers);

  // tab code
  WriteBoolU('Code', 'Code-Completion-Always', FCodeCompletionAlways);
  WriteBoolU('Code', 'Code-Completion', FCodeCompletionCtrlSpace);
  WriteBoolU('Code', 'Parameter-Hints', FParameterHints);
  WriteBoolU('Code', 'ShowClassObject', FShowClassObject);
  WriteIntegerU('Code', 'Delay', FCodeDelay);
  WriteBoolU('Code', 'TooltipWithKey', FTooltipWithKey);
  WriteBoolU('Code', 'TooltipAutomatic', FTooltipAutomatic);
  WriteIntegerU('Code', 'SelectionSizeMin', FSelectionSizeMin);
  WriteIntegerU('Code', 'SelectionSizeMax', FSelectionSizeMax);
  WriteIntegerU('Code', 'TooltipDelay', FTooltipDelay);
  WriteIntegerU('Code', 'TooltipWidth', FTooltipWidth);
  WriteIntegerU('Code', 'TooltipHeight', FTooltipHeight);
  WriteIntegerU('Code', 'TooltipFontSize', FTooltipFontSize);

  // tab keyboard
  WriteStringFile('Editor', 'KeyboardFile', FKeyboardFile);

  // tab colors
  SaveUserColors;
  WriteBoolU('Colors', 'NoActiveLineColor', FNoActiveLineColor);
  WriteIntegerU('Colors', 'ActiveLineColor', FActiveLineColor);
  WriteBoolU('Colors', 'NoSyntaxHighlighting', FNoSyntaxHighlighting);
  WriteStringU('Colors', 'GUIStyle', GUIStyle);
  WriteStringU('Colors', 'EditorStyle', FEditorStyle);

  // tab browser
  WriteBoolU('Browser', 'UseIEinternForDocuments', FUseIEinternForDocuments);
  WriteBoolU('Browser', 'OnlyOneBrowserWindow', FOnlyOneBrowserWindow);
  WriteStringU('Browser', 'BrowserOpenKeys', FBrowserOpenKeys);
  WriteStringFile('Browser', 'Browser', FBrowserProgram);
  WriteStringU('Browser', 'Title', FBrowserTitle);
  WriteBoolU('Browser', 'ProxyEnabled', FWithProxy);
  WriteStringU('Browser', 'ProxyIP', FProxyIP);
  WriteIntegerU('Browser', 'ProxyPort', FProxyPort);

  // tab printer
  WriteIntegerU('Printer', 'Left', FBorderLeft);
  WriteIntegerU('Printer', 'Top', FBorderTop);
  WriteIntegerU('Printer', 'Right', FBorderRight);
  WriteIntegerU('Printer', 'Bottom', FBorderBottom);
  WriteStringU('Printer', 'Header', FHeader);
  WriteStringU('Printer', 'Footer', FFooter);
  WriteBoolU('Printer', 'Linenumbers', FWithLinenumbers);
  WriteBoolU('Printer', 'LinenumbersInMargin', FLinenumbersInMargin);
  WriteBoolU('Printer', 'PrintColored', FPrintColored);
  if HasDefaultPrinter then
    WriteStringU('Printer', 'Printer', Printer.Printers[Printer.PrinterIndex])
  else
    WriteStringU('Printer', 'Printer', '');

  // tab comment
  if RGComment.ItemIndex = 2 then
    WriteStringU('Comment', 'Comment', FFreeComment);
  WriteIntegerU('Comment', 'Type', FCommentKind);
  WriteStringU('Comment', 'Author', FJavaAuthor);
  WriteStringU('Comment', 'Method', SaveComboBoxItems(FMethodComment));

  // tab mindstorms
  WriteStringDirectory('Mindstorms', 'LejosFolder', FLejosVerzeichnis);
  WriteStringFile('Mindstorms', 'LejosCompiler', FLejosCompiler);
  WriteStringFile('Mindstorms', 'LejosUploader', FLejosUploader);
  WriteStringFile('Mindstorms', 'LejosFlasher', FLejosFlasher);
  WriteStringFile('Mindstorms', 'Manual', FMindstormsManual);
  WriteIntegerU('Mindstorms', 'Port', FMindstormsPort);
  WriteStringU('Mindstorms', 'IP', FMindstormsIP);
  WriteStringU('Mindstorms', 'Parameter', FMindstormsParameter);
  WriteBoolU('Mindstorms', 'Mode', FMindstormsMode);
  WriteIntegerU('Mindstorms', 'Version', FMindstormsVersion);

  WriteStringDirectory('Mindstorms', 'RCXFolder', FRCXFolder);
  WriteStringDirectory('Mindstorms', 'NXTFolder', FNXTFolder);
  WriteStringDirectory('Mindstorms', 'EV3Folder', FEV3Folder);
  WriteStringFile('Mindstorms', 'RCXCompiler', FRCXCompiler);
  WriteStringFile('Mindstorms', 'NXTCompiler', FNXTCompiler);
  WriteStringFile('Mindstorms', 'RCXUploader', FRCXUploader);
  WriteStringFile('Mindstorms', 'NXTUploader', FNXTUploader);
  WriteStringFile('Mindstorms', 'RCXFlasher', FRCXFlasher);
  WriteStringFile('Mindstorms', 'NXTFlasher', FNXTFlasher);
  WriteStringFile('Mindstorms', 'RCXManual', FRCXManual);
  WriteStringFile('Mindstorms', 'NXTManual', FNXTManual);
  WriteStringFile('Mindstorms', 'EV3Manual', FEV3Manual);

  // tab android
  WriteBoolU('Android', 'Mode', FAndroidMode);
  WriteStringDirectory('Android', 'AndroidSDKFolder', FAndroidSDKFolder);

  // tag GUI designer
  WriteBoolU('Options', 'NameFromText', FNameFromText);
  WriteBoolU('Options', 'GuiDesignerHints', FGuiDesignerHints);
  WriteBoolU('Options', 'SnapToGrid', FSnapToGrid);
  WriteIntegerU('Options', 'GridSize', FGridSize);
  WriteIntegerU('Options', 'ZoomSteps', FZoomSteps);
  WriteIntegerU('Options', 'GUIFontSize', FGUIFontSize);
  WriteStringU('Options', 'GUIFontName', FGUIFontName);
  WriteIntegerU('Editor', 'FrameWidth', FFrameWidth);
  WriteIntegerU('Editor', 'FrameHeight', FFrameHeight);
  WriteIntegerU('Editor', 'AppletWidth', FAppletWidth);
  WriteIntegerU('Editor', 'AppletHeight', FAppletHeight);

  // tab options
  WriteBoolU('Options', 'BAKFiles', FCreateBAKFiles);
  WriteBoolU('Options', 'RunsUnderWine', FRunsUnderWine);
  WriteBoolU('Options', 'LoadFiles', FLoadFiles);
  WriteBoolU('Options', 'UseInterpreterWindowAsConsole',
    FUseInterpreterWindowAsConsole);
  WriteBoolU('Options', 'AcceptDefaultname', FAcceptDefaultname);
  WriteBoolU('Options', 'ComponentsToolbar', FComponentsToolbar);
  WriteBoolU('Options', 'BorderLayout', FBorderLayout);
  WriteBoolU('Options', 'Rename', FRenameWhenSave);
  WriteBoolU('Options', 'DebuggerProtocol', FDebuggerProtocol);
  WriteBoolU('Options', 'TranslateCompilerErrors', FTranslateCompilerErrors);
  WriteBoolU('Options', 'StrictJavaMode', FStrictJavaMode);
  WriteBoolU('Options', 'CheckAge', FCheckAge);
  WriteIntegerU('Options', 'FontSize', FFontSize);
  WriteIntegerU('Options', 'MaxFileHistory', UDFileHistory.Position);
  WriteStringU('Options', 'FileFilter', FFileFilter);

  // tab restrictions
  WriteBoolM('Java', 'DOS-Window', FDOSWindowLocked);
  WriteBoolM('Java', 'BlockedInternet', FBlockedInternet);
  WriteBoolM('Java', 'LockedPaths', FLockedPaths);
  WriteBoolM('Java', 'LockedStructogram', FLockedStructogram);

  // tab associations
  WriteStringU('Associations', 'Additional', FAdditionalAssociations);

  // tab UML
  WriteIntegerU('UML', 'ValidClassColor', FValidClassColor);
  WriteIntegerU('UML', 'InvalidClassColor', FInvalidClassColor);
  WriteIntegerU('UML', 'ClassHead', FClassHead);
  WriteIntegerU('UML', 'ShadowWidth', FShadowWidth);
  WriteIntegerU('UML', 'ShadowIntensity', FShadowIntensity);
  WriteIntegerU('UML', 'ObjectColor', FObjectColor);
  WriteIntegerU('UML', 'ObjectHead', FObjectHead);
  WriteIntegerU('UML', 'ObjectFooter', FObjectFooter);
  WriteIntegerU('UML', 'ObjectCaption', FObjectCaption);
  WriteBoolU('UML', 'ObjectUnderline', FObjectUnderline);
  WriteIntegerU('UML', 'CommentColor', FCommentColor);

  WriteIntegerU('UML', 'Visibility', FDiVisibilityFilter);
  WriteIntegerU('UML', 'ShowParameter', FDiShowParameter);
  WriteIntegerU('UML', 'ShowIcons', FDiShowIcons);
  WriteIntegerU('UML', 'SortOrder', FDiSortOrder);

  // tab uml options
  WriteBoolU('UML', 'PrivateAttribut', FPrivateAttributEditable);
  WriteBoolU('UML', 'ShowEmptyRects', FShowEmptyRects);
  WriteBoolU('UML', 'ShowFunctionValues', FShowFunctionValues);
  WriteBoolU('UML', 'ConstructorWithVisibility', FConstructorWithVisibility);
  WriteBoolU('UML', 'ObjectLowerCaseLetter', FObjectLowerCaseLetter);
  WriteBoolU('UML', 'ShowPublicOnly', FShowPublicOnly);
  WriteBoolU('UML', 'DefaultModifiers', FDefaultModifiers);
  WriteBoolU('UML', 'ShowObjectsWithMethods', FShowObjectsWithMethods);
  WriteBoolU('UML', 'ShowObjectsWithInheritedPrivateAttributes',
    FShowObjectsWithInheritedPrivateAttributes);
  WriteBoolU('UML', 'AttributesAParametersP', AttributesAParametersP);
  WriteBoolU('UML', 'UseVoid', FUseVoid);
  WriteBoolU('UML', 'IntegerInsteadofInt', FIntegerInsteadofInt);
  WriteBoolU('UML', 'ShowAllNewObjects', FShowAllNewObjects);
  WriteBoolU('UML', 'ObjectsWithoutVisibility', FObjectsWithoutVisibility);
  WriteBoolU('UML', 'ArrayListAsIntegratedList', FArrayListAsIntegratedList);
  WriteBoolU('UML', 'RelationshipAttributesBold', FRelationshipAttributesBold);
  WriteBoolU('UML', 'StartWithDatatype', FStartWithDatatype);
  WriteBoolU('UML', 'SetterWithoutThis', FSetterWithoutThis);
  WriteBoolU('UML', 'ShowClassparameterSeparately',
    FShowClassparameterSeparately);
  WriteBoolU('UML', 'RoleHidesAttribute', FRoleHidesAttribute);
  WriteBoolU('UML', 'UseAbstractForClass', FUseAbstractForClass);
  WriteBoolU('UML', 'UseAbstractForMethods', FUseAbstractForMethods);

  // tab checkstyle
  WriteStringFile('Checkstyle', 'Checkstyle', FCheckstyle);
  WriteStringFile('Checkstyle', 'Configurationfile', FCheckConfiguration);
  WriteStringU('Checkstyle', 'CheckParameter', FCheckParameter);

  // tab jalopy
  WriteStringFile('Jalopy', 'Jalopy', FJalopy);
  WriteStringFile('Jalopy', 'JalopyConfiguration', FJalopyConfiguration);
  WriteStringU('Jalopy', 'JalopyParameter', FJalopyParameter);

  // tab SVN
  WriteStringDirectory('SVN', 'SVNFolder', FSVNFolder);
  WriteStringDirectory('SVN', 'SVNRepository', FSVNRepository);

  // tab git
  WriteStringDirectory('Git', 'GitFolder', FGitFolder);
  WriteStringDirectory('Git', 'GitLocalRepository', FGitLocalRepository);
  WriteStringDirectory('Git', 'GitRemoteRepository', FGitRemoteRepository);
  WriteStringU('Git', 'User.name', FGitUserName);
  WriteStringU('Git', 'User.email', FGitUserEMail);

  // tab JUnit
  WriteStringFile('JUnit', 'JUnitJarFile', FJUnitJarFile);
  WriteStringFile('JUnit', 'JUnitManual', FJUnitManual);
  WriteStringU('JUnit', 'JUnitParameter', FJUnitParameter);
  WriteBoolU('JUnit', 'JUnitBeforeEach', FJUnitBeforeEach);
  WriteBoolU('JUnit', 'JUnitAfterEach', FJUnitAfterEach);

  // tab log files
  WriteStringFile('Logfiles', 'LogfileCompiler', FLogfileCompilerWithUserName);
  WriteStringFile('Logfiles', 'LogfileInteractive',
    FLogfileInteractiveWithUsername);
  WriteStringFile('Logfiles', 'LogfileExceptions',
    FLogfileExceptionsWithUsername);

  // tab structogramS
  // write for old Language
  FStructogramS := 'Structogram.' + FLanguageCode;
  WriteStringU(FStructogramS, 'Algorithm', FAlgorithm);
  WriteStringU(FStructogramS, 'Input', FInput);
  WriteStringU(FStructogramS, 'Output', FOutput);
  WriteStringU(FStructogramS, 'While', FWhile);
  WriteStringU(FStructogramS, 'DoWhile', FDoWhile);
  WriteStringU(FStructogramS, 'For', FFor);
  WriteStringU(FStructogramS, 'Yes', FYes);
  WriteStringU(FStructogramS, 'No', FNo);
  WriteStringU(FStructogramS, 'Other', FOther);
  WriteIntegerU('Structogram', 'GenerateJavaAsProgram', FGenerateJavaAsProgram);
  WriteStringU('Structogram', 'Datatype', FStructoDatatype);
  WriteBoolU('Structogram', 'SwitchWithCaseLine', FSwitchWithCaseLine);
  WriteIntegerU('Structogram', 'CaseCount', FCaseCount);
  WriteIntegerU('Structogram', 'ShadowWidth', FStructogramShadowWidth);
  WriteIntegerU('Structogram', 'ShadowIntensity', FStructogramShadowIntensity);

  // tab sequence diagram
  FSequencediagramS := 'Sequencediagram.' + FLanguageCode;
  WriteStringU(FSequencediagramS, 'Object', FSDObject);
  WriteStringU(FSequencediagramS, 'New', FSDNew);
  WriteStringU(FSequencediagramS, 'Close', FSDClose);
  WriteIntegerU(FSequencediagramS, 'Fillingcolor', FSDFillingcolor);
  WriteBoolU(FSequencediagramS, 'NoFilling', FSDNoFilling);
  WriteBoolU(FSequencediagramS, 'ShowMainCall', FSDShowMainCall);
  WriteBoolU(FSequencediagramS, 'ShowParameter', FSDShowParameter);
  WriteBoolU(FSequencediagramS, 'ShowReturn', FSDShowReturn);

  // no change language code
  if RGLanguages.ItemIndex = -1 then
    FLanguageCode := 'en_GB'
  else
    FLanguageCode := FLanguagesList[RGLanguages.ItemIndex];
  WriteStringU('Options', 'Language', FLanguageCode);
  WriteIntegerU('Panel', 'RightDockPanelWidth', FRightDockPanelWidth);
  WriteIntegerU('Panel', 'BottomDockPanelHeight', FBottomDockPanelHeight);

  // tab Providers
  WriteProviders('LLMAssistant', LLMAssistant.Providers);
  WriteProviders('LLMChat', FLLMChatForm.LLMChat.Providers);

  FJavaVersion := 0;
  FDocumentationVersion := 0;
  SaveVisibility;
  SaveInis;
end; // ModelToRegistry

procedure TFConfiguration.ViewToModel;
begin
  with FJava do
  begin
    // tab interpreter
    FJDKFolder := ExtendPath(CBJDKFolder);
    FJDKFolderItems := CBJDKFolder.Items.Text;
    FJavaInterpreter := ExtendPath(EInterpreter);
    FJavaInterpreterParameter := EInterpreterParameter.Text;
    FJavaFXFolder := ExcludeTrailingPathDelimiter(EJavaFXFolder.Text);
    FJavaFXParameter := EJavaFXParameter.Text;
    FJavaClasspathAdmin := EClasspathAdmin.Text;
    FJavaClasspathUser := EClasspathUser.Text;
    FFileEncoding := CBFileEncoding.Text;
    FCodepage := CBCodepage.Text;
    FShowInterpreterCall := CBShowInterpreterCall.Checked;

    // tab compiler
    FJavaCompiler := ExtendPath(EJavaCompiler);
    FJavaCompilerParameter := EJavaCompilerParameter.Text;
    FCompileInternally := CBUseJavaCompilerInternally.Checked;
    FShowCompilerCall := CBShowCompilerCall.Checked;
    FCompilerEncoding := CBCompilerEncoding.Text;

    // tab programs
    FJavaAppletviewer := ExtendPath(EAppletviewer);
    FJavaDebugger := ExtendPath(EDebugger);
    FJavaDoc := ExtendPath(EJavaDoc);
    FJavaDocParameter := EDocParameter.Text;

    // tab disassembler
    var Str := ReadStringU('Program', 'DisassemblerItems', '');
    if Str <> '' then
    begin
      Str := AddPortableDrives(LoadComboBoxItems(Str));
      CBDisassembler.Items.Text := Str;
    end;
    FJavaDisassembler := ExtendPath(CBDisassembler);
    FJavaDisassemblerItems := LoadComboBoxItems(Str);
    FJavaDisassemblerParameter := EDisassemblerParameter.Text;

    // tab checkstyle
    FCheckstyle := ExtendPath(ECheckstyle);
    FCheckConfiguration := ExtendPath(ECheckstyleConfiguration);
    FCheckParameter := ECheckstyleParameter.Text;

    // tab jalopy
    FJalopy := ExtendPath(EJalopy);
    FJalopyConfiguration := ExtendPath(EJalopyConfiguration);
    FJalopyParameter := EJalopyParameter.Text;

    // tab jar
    FJavaJar := ExtendPath(EJar);
    FJavaJarParameter := EJarParameter.Text;
    FJavaJarManifest := EJarManifest.Text;
    FJarPackFiles := CBJarPack.Text;
    FJarClassPath := ReplaceStr(EJarClasspath.Text, ';', ' ');

    // tab applet
    FAppletStart := RGApplet.ItemIndex;
    FShowHTMLforApplet := CBShowHTMLforApplet.Checked;

    // tab templates
    FTemplates[1] := ExtendPath(ETemplateConsole);
    FTemplates[2] := ExtendPath(ETemplateFrame);
    FTemplates[3] := ExtendPath(ETemplateDialog);
    FTemplates[4] := ExtendPath(ETemplateApplet);
    FTemplates[5] := ExtendPath(ETemplateJFrame);
    FTemplates[6] := ExtendPath(ETemplateJDialog);
    FTemplates[7] := ExtendPath(ETemplateJApplet);
    FTemplates[8] := ExtendPath(ETemplateApplication);
    FTemplates[9] := ExtendPath(ETemplateControlstructure);
    FTemplates[10] := ExtendPath(ETemplateClass);
    FTemplates[11] := ExtendPath(EMindstormsTemplate);
    FTemplates[12] := ExtendPath(ETemplateJUnitTest);

    // tab documentation
    FJavaManual := ExtendPath(CBManual);
    FJavaManualItems := CBManual.Items.Text;
    FJavaManualFX := ExtendPath(CBManualFX);
    FJavaManualFXItems := CBManualFX.Items.Text;
    FJavaTutorial := ExtendPath(ETutorial);
    FJavabook := ExtendPath(EJavabook);
    FJavaCache := ExtendPath(ECache);
    MakeJavaCache(FJavaCache);
    FMaxSearch := UDMaxSearch.Position;

    // tab editor
    FTabWidth := UDTabWidth.Position;
    FIndent := UDIndent.Position;
    FIndentHelp := CBIndentHelp.Checked;
    FCursorBehindLine := CBCursorBehindLine.Checked;
    FAutomaticIndent := CBAutomaticIndent.Checked;

    FLineNumbering := CBLineNumbering.Checked;
    FAddClosingBracket := CBCompleteBracket.Checked;
    FIndentAfterBracket := CBIndentafterBracket.Checked;
    FShowBracketPair := CBShowBracketpair.Checked;
    FCommentClosingBrackets := CBCommentClosingBrackets.Checked;
    FStructureColoring := CBStructureColoring.Checked;
    FStructureColoringPlane := CBStructureColoringPlane.Checked;
    FGUICodeFolding := CBGUICodeFolding.Checked;
    FEightyColumnLine := CB80Columnline.Checked;

    FStructureColorIntensity := UDIntensity.Position;
    FInsertControlStructures := CBInsertControlStructures.Checked;
    FInsertSemicolons := CBInsertSemicolons.Checked;

    FShowControlFlowSymbols := CBShowControlFlowSymbols.Checked;
    FShowLigatures := CBShowLigatures.Checked;
    FCompactLineNumbers := CBCompactLineNumbers.Checked;

    // tab code
    FCodeCompletionAlways := RBCodeCompletionAlways.Checked;
    FCodeCompletionCtrlSpace := RBCodeCompletionCtrlSpace.Checked;
    if Assigned(FJava.EditorForm) then
      FJava.scpSetEditForm(FJava.EditorForm);
    FParameterHints := CBParameterHints.Checked;
    FShowClassObject := CBShowClassObject.Checked;
    FCodeDelay := TBDelay.Position;
    FSelectionSizeMin := StrToInt(ESelectionSizeMin.Text);
    FSelectionSizeMax := StrToInt(ESelectionSizeMax.Text);
    if FSelectionSizeMax < FSelectionSizeMin then
      FSelectionSizeMax := FSelectionSizeMin;
    FTooltipWithKey := CBTooltipWithKey.Checked;
    FTooltipAutomatic := CBTooltipAutomatic.Checked;
    FTooltipDelay := TBTooltipDelay.Position;

    // tab browser
    FUseIEinternForDocuments := CBUseIEinternForDocuments.Checked;
    FOnlyOneBrowserWindow := CBOnlyOneBrowserWindow.Checked;
    FBrowserTitle := EBrowserTitle.Text;
    FBrowserOpenKeys := CBOpenBrowserShortcut.Text;
    FBrowserProgram := ExtendPath(EBrowserProgram);
    FProxyIP := EProxyIP.Text;
    FProxyPort := StrToInt(EProxyPort.Text);
    FWithProxy := CBUseProxy.Checked;

    // tab printer
    FBorderLeft := StrToInt(EBorderLeft.Text);
    FBorderTop := StrToInt(EBorderTop.Text);
    FBorderRight := StrToInt(EBorderRight.Text);
    FBorderBottom := StrToInt(EBorderBottom.Text);
    FHeader := EHeader.Text;
    FFooter := EFooter.Text;
    FWithLinenumbers := CBLinenumbers.Checked;
    FLinenumbersInMargin := CBLinenumbersInBorder.Checked;
    FPrintColored := CBPrintColored.Checked;

    // tab comment
    FCommentKind := RGComment.ItemIndex;
    FJavaAuthor := EAuthor.Text;
    FMethodComment := MMethodComment.Text;

    // tab keyboard
    FKeyboardFile := ExtendPath(EKeyboardFile);

    // tab mindstorms
    FMindstormsParameter := EMindstormsParameter.Text;
    FMindstormsPort := CBMindstormsPort.ItemIndex;
    FMindstormsIP := EMindstormsIP.Text;
    FMindstormsMode := CBMindstormsModus.Checked;
    FMindstormsVersion := RGMindstormsVersion.ItemIndex;
    case FMindstormsVersion of
      0:
        begin
          FRCXManual := ExtendPath(EMindstormsManual);
          FRCXFolder := ExtendPath(ELejosFolder);
          FRCXCompiler := ExtendPath(ELejosCompiler);
          FRCXUploader := ExtendPath(ELejosUploader);
          FRCXFlasher := ExtendPath(ELejosFlasher);
        end;
      1:
        begin
          FNXTManual := ExtendPath(EMindstormsManual);
          FNXTFolder := ExtendPath(ELejosFolder);
          FNXTCompiler := ExtendPath(ELejosCompiler);
          FNXTUploader := ExtendPath(ELejosUploader);
          FNXTFlasher := ExtendPath(ELejosFlasher);
        end;
      2:
        begin
          FEV3Manual := ExtendPath(EMindstormsManual);
          FEV3Folder := ExtendPath(ELejosFolder);
        end;
    end;

    case FMindstormsVersion of
      0:
        begin
          FLejosVerzeichnis := FRCXFolder;
          FLejosCompiler := FRCXCompiler;
          FLejosUploader := FRCXUploader;
          FLejosFlasher := FRCXFlasher;
          FMindstormsManual := FRCXManual;
        end;
      1:
        begin
          FLejosVerzeichnis := FNXTFolder;
          FLejosCompiler := FNXTCompiler;
          FLejosUploader := FNXTUploader;
          FLejosFlasher := FNXTFlasher;
          FMindstormsManual := FNXTManual;
        end;
      2:
        begin
          FLejosVerzeichnis := FEV3Folder;
          FMindstormsManual := FEV3Manual;
        end;
    end;

    // tab android
    FAndroidMode := CBAndroidMode.Checked;
    FAndroidSDKFolder := EAndroidSDKFolder.Text;

    // tab GUI designer
    FNameFromText := CBNameFromText.Checked;
    FGuiDesignerHints := CBGuiDesignerHints.Checked;
    FSnapToGrid := CBSnapToGrid.Checked;
    FGridSize := UDGridSize.Position;
    FZoomSteps := UDZoomSteps.Position;
    FFrameWidth := StrToInt(EFrameWidth.Text);
    FFrameHeight := StrToInt(EFrameheight.Text);
    FAppletWidth := StrToInt(EAppletwidth.Text);
    FAppletHeight := StrToInt(EAppletheight.Text);

    // tab options
    FCreateBAKFiles := CBBAKFiles.Checked;
    FRunsUnderWine := CBRunsUnderWine.Checked;
    FLoadFiles := CBLoadFiles.Checked;
    FUseInterpreterWindowAsConsole := CBUseInterpreterWindowAsConsole.Checked;
    FAcceptDefaultname := CBAcceptdefaultname.Checked;
    FRenameWhenSave := CBRenameWhenSave.Checked;
    FDebuggerProtocol := CBDebuggerProtocol.Checked;
    FTranslateCompilerErrors := CBTranslateCompilerErrors.Checked;
    FStrictJavaMode := CBStrictJavaMode.Checked;

    FMaxFileHistory := UDFileHistory.Position;
    FFontSize := UDFontSize.Position;

    FFileFilter := EFileFilter.Text;
    FCheckAge := CBCheckAge.Checked;
    FTempDirWithUsername := IncludeTrailingPathDelimiter
      (ExtendPath(ETempFolder));
    FTempDir := DissolveUsername(FTempDirWithUsername);

    // tab restrictions
    FDOSWindowLocked := CBDosWindow.Checked;
    FBlockedInternet := CBBlockedInternet.Checked;
    FLockedPaths := CBLockedPaths.Checked;
    FLockedStructogram := CBLockedStructogram.Checked;

    // tab associations
    FAdditionalAssociations := EAdditionalAssociations.Text;

    // tab UML
    FValidClassColor := CBValidClassColor.Selected;
    FInvalidClassColor := CBInvalidClassColor.Selected;
    FClassHead := RGClassHead.ItemIndex;
    FShadowWidth := UDShadowWidth.Position;
    FShadowIntensity := UDShadowIntensity.Position;
    FObjectColor := CBObjectColor.Selected;
    FObjectHead := RGObjectHead.ItemIndex;
    FObjectFooter := RGObjectFooter.ItemIndex;
    FObjectCaption := RGObjectCaption.ItemIndex;
    FObjectUnderline := CBObjectUnderline.Checked;
    FCommentColor := CBCommentColor.Selected;

    FDiVisibilityFilter := 4 - RGAttributsMethodsDisplay.ItemIndex;
    FDiSortOrder := RGSequenceAttributsMethods.ItemIndex;
    FDiShowParameter := RGParameterDisplay.ItemIndex;
    FDiShowIcons := 2 - RGVisibilityDisplay.ItemIndex;

    // tab uml options
    FPrivateAttributEditable := CBUMLEdit.Checked;
    FShowEmptyRects := CBShowEmptyRects.Checked;
    FShowFunctionValues := CBShowFunctionvalues.Checked;
    FConstructorWithVisibility := CBConstructorWithVisibility.Checked;
    FObjectLowerCaseLetter := CBLowerCaseLetter.Checked;
    FShowPublicOnly := CBOpenPublicClasses.Checked;
    FDefaultModifiers := CBDefaultModifiers.Checked;
    FShowObjectsWithMethods := CBShowObjectsWithMethods.Checked;
    FShowObjectsWithInheritedPrivateAttributes :=
      CBShowObjectsWithInheritedPrivateAttributes.Checked;
    FAttributesAParametersP := CBAttributesAParametersP.Checked;
    FUseVoid := CBUseVoid.Checked;
    FIntegerInsteadofInt := CBIntegerInsteadofInt.Checked;
    FShowAllNewObjects := CBShowAllNewObjects.Checked;
    FObjectsWithoutVisibility := CBObjectsWithoutVisibility.Checked;
    FArrayListAsIntegratedList := CBArrayListAsIntegratedList.Checked;
    FRelationshipAttributesBold := CBRelationshipAttributesBold.Checked;
    FStartWithDatatype := CBStartWithDatatype.Checked;
    FSetterWithoutThis := CBSetterWithoutThis.Checked;
    FShowClassparameterSeparately := CBShowClassparameterSeparately.Checked;
    FRoleHidesAttribute := CBRoleHidesAttribute.Checked;
    FUseAbstractForClass := CBUseAbstractForClass.Checked;
    FUseAbstractForMethods := CBUseAbstractForMethods.Checked;

    // tab color
    FNoActiveLineColor := CBNoActiveLineColor.Checked;
    if FNoActiveLineColor then
      FActiveLineColor := clNone
    else
      FActiveLineColor := CBActiveLineColor.Selected;
    CBActiveLineColor.Enabled := FNoActiveLineColor;
    FNoSyntaxHighlighting := CBNoSyntaxHighlighting.Checked;
    FEditorStyle := CBEditorStyles.Text;

    // tab svn
    FSVNFolder := ExtendPath(ESVNFolder);
    FSVNRepository := ExtendPath(CBRepository);

    // tab git
    FGitFolder := ExtendPath(EGitFolder);
    FGitLocalRepository := ExtendPath(CBLocalRepository);
    FGitRemoteRepository := ExtendPath(CBRemoteRepository);
    if Assigned(FGit) and (EUserName.Text <> FGitUserName) then
    begin
      FGit.GitCall('config --global User.name="' + EUserName.Text + '"', '.');
      FGitUserName := EUserName.Text;
    end;
    if Assigned(FGit) and (EUserEMail.Text <> FGitUserEMail) then
    begin
      FGit.GitCall('config --global User.email="' + EUserEMail.Text + '"', '.');
      FGitUserEMail := EUserEMail.Text;
    end;

    // tab JUnit
    FJUnitJarFile := ExtendPath(EJUnitJarFile);
    FJUnitManual := ExtendPath(EJUnitManual);
    FJUnitParameter := EJunitParameter.Text;
    FJUnitBeforeEach := CBJUnitBeforeEach.Checked;
    FJUnitAfterEach := CBJUnitAfterEach.Checked;

    // tab log files
    FLogfileCompilerWithUserName := ExtendPath(ELogfileCompiler);
    FLogfileCompiler := DissolveUsername(FLogfileCompilerWithUserName);
    FLogfileInteractiveWithUsername := ExtendPath(ELogfileInteractive);
    FLogfileInteractive := DissolveUsername(FLogfileInteractiveWithUsername);
    FLogfileExceptionsWithUsername := ExtendPath(ELogfileExceptions);
    FLogfileExceptions := DissolveUsername(FLogfileExceptionsWithUsername);

    // tab structogramS
    FAlgorithm := EAlgorithm.Text;
    FInput := EInput.Text;
    FOutput := EOutput.Text;
    FWhile := EWhile.Text;
    FDoWhile := EDoWhile.Text;
    FFor := EFor.Text;
    FYes := EYes.Text;
    FNo := ENo.Text;
    FOther := EOther.Text;
    FGenerateJavaAsProgram := RGGenerateJavacode.ItemIndex;
    FStructoDatatype := CBDataType.Text;
    FSwitchWithCaseLine := CBSwitchWithCaseLine.Checked;
    FCaseCount := StrToInt(ECaseCount.Text);
    FStructogramShadowWidth := UDStructogramShadowWidth.Position;
    FStructogramShadowIntensity := UDStructogramShadowIntensity.Position;

    // tab sequence diagram
    FSDObject := ESDObject.Text;
    FSDNew := ESDNew.Text;
    FSDClose := ESDClose.Text;
    FSDFillingcolor := CBSDFillingColor.Selected;
    FSDNoFilling := CBSDNoFilling.Checked;
    FSDShowMainCall := CBSDShowMainCall.Checked;
    FSDShowParameter := CBSDShowParameter.Checked;
    FSDShowReturn := CBSDShowReturn.Checked;

    // tab providers
    LLMAssistantViewToModel;
    CopyProviders(FTempProviders, LLMAssistant.Providers);

    LLMChatViewToModel;
    CopyProviders(FTempChatProviders, FLLMChatForm.LLMChat.Providers);

    VisibilityViewToModel;
  end;
end; // ViewToModel

procedure TFConfiguration.SaveVisibility;

  function ArrVisibilityToString1(Arr: array of Boolean): string;
  begin
    var
    Str := '';
    for var I := 0 to High(Arr) do
      if Arr[I] then
        Str := Str + '1'
      else
        Str := Str + '0';
    Result := Str;
  end;

  function ArrVisibilityToString2(Tab, Num: Integer): string;
  begin
    var
    Str := '';
    for var I := 0 to Num - 1 do
      if FVis1[Tab, I] then
        Str := Str + '1'
      else
        Str := Str + '0';
    Result := Str;
  end;

begin
  WriteStringU('Visibility', 'Tabs', ArrVisibilityToString1(FVisTabs));
  WriteStringU('Visibility', 'Program', ArrVisibilityToString2(0, 12));
  WriteStringU('Visibility', 'AWT', ArrVisibilityToString2(1,
    FJava.ToolbarAWT.ButtonCount));
  WriteStringU('Visibility', 'Swing1', ArrVisibilityToString2(2,
    FJava.ToolBarSwing1.ButtonCount));
  WriteStringU('Visibility', 'Swing2', ArrVisibilityToString2(3,
    FJava.ToolbarSwing2.ButtonCount));
  WriteStringU('Visibility', 'Layout', ArrVisibilityToString2(4,
    FJava.ToolBarLayout.ButtonCount));
  WriteStringU('Visibility', 'Utilities', ArrVisibilityToString2(5,
    FJava.ToolBarUtilities.ButtonCount));
  WriteStringU('Visibility', 'FXBase', ArrVisibilityToString2(6,
    FJava.ToolBarFXBase.ButtonCount));
  WriteStringU('Visibility', 'FXControls', ArrVisibilityToString2(7,
    FJava.ToolBarFXControls.ButtonCount));
  WriteStringU('Visibility', 'FXShapes', ArrVisibilityToString2(8,
    FJava.ToolBarFXShapes.ButtonCount));

  WriteStringU('Visibility', 'Menus', ArrVisibilityToString1(FVisMenus));
  WriteStringU('Visibility', 'Toolbars', ArrVisibilityToString1(FVisToolbars));
end;

procedure TFConfiguration.LoadVisibility;
var
  Num: Integer;
  Str: string;

  procedure StringVisibilityToArr1(Str: string; var Arr: TBoolArray);
  begin
    while Length(Str) < Length(Arr) do
      Str := Str + '1';
    for var I := 0 to High(Arr) do
      Arr[I] := (Str[I + 1] = '1');
  end;

  procedure StringVisibilityToArr2(Str: string; Len, Num: Integer);
  begin
    while Length(Str) < Len do
      Str := Str + '1';
    while Length(Str) > Len do
      Delete(Str, 1, 1);

    for var I := 0 to Len - 1 do
      FVis1[Num, I] := (Str[I + 1] = '1');
  end;

begin
  StringVisibilityToArr1(ReadStringU('Visibility', 'Tabs', '111101111'),
    FVisTabs); // FNo Layout
  StringVisibilityToArr2(ReadStringU('Visibility', 'Program', '111111111111'),
    12, 0); // by Default
  Num := FJava.ToolbarAWT.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'AWT', StringOfChar('1', Num)
    ), Num, 1);
  Num := FJava.ToolBarSwing1.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Swing1',
    StringOfChar('1', Num)), Num, 2);
  Num := FJava.ToolbarSwing2.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Swing2',
    StringOfChar('1', Num)), Num, 3);
  Num := FJava.ToolBarLayout.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Layout',
    StringOfChar('1', Num)), Num, 4);
  Num := FJava.ToolBarUtilities.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Utilities',
    StringOfChar('1', Num)), Num, 5);
  Num := FJava.ToolBarFXBase.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'FXBase',
    StringOfChar('1', Num)), Num, 6);
  Num := FJava.ToolBarFXControls.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'FXControls',
    StringOfChar('1', Num)), Num, 7);
  Num := FJava.ToolBarFXShapes.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'FXShapes',
    StringOfChar('1', Num)), Num, 8);

  Str := ReadStringU('Visibility', 'Menus', '1110');
  StringVisibilityToArr1(Str, FVisMenus);
  Str := ReadStringU('Visibility', 'Toolbars', '111110');
  StringVisibilityToArr1(Str, FVisToolbars);
end;

procedure TFConfiguration.SetVisibility;
var
  WinControl: TWinControl;
  Item: TSpTBXTabItem;
  Page: TSpTBXTabSheet;
begin
  for var I := 0 to MaxTab do
  begin
    FJava.TabsControl.Items[I].Visible := FVisTabs[I];
    Item := TSpTBXTabItem(FJava.TabsControl.Items[I]);
    Page := FJava.TabsControl.GetPage(Item);
    WinControl := TWinControl(Page.Controls[0]);
    for var J := 0 to WinControl.ControlCount - 1 do
      WinControl.Controls[J].Visible := FVis1[I, J];
  end;

  FJava.MITest.Visible := FVisMenus[0];
  FJava.MIUML.Visible := FVisMenus[1];
  FJava.MITools.Visible := FVisMenus[2];
  FJava.MIComponents.Visible := FVisMenus[3];

  FJava.MainToolBar.Visible := FVisToolbars[0];
  FJava.TabsControl.Visible := FVisToolbars[4];
  FJava.PBorder.Visible := FVisToolbars[5];
  FJava.ControlBar.Visible := FVisToolbars[0] or FVisToolbars[4] or
    FVisToolbars[5];
  FJava.SetDockTopPanel;
end;

procedure TFConfiguration.VisibilityViewToModel;
begin
  for var I := 0 to MaxTab do
    FVisTabs[I] := LVVisibilityTabs.Items[I].Checked;

  if Assigned(LVVisibilityMenus.Items[0]) then // due to unknown problem
    for var I := 0 to High(FVisMenus) do
      FVisMenus[I] := LVVisibilityMenus.Items[I].Checked;

  if Assigned(LVVisibilityToolbars.Items[0]) then
    for var I := 0 to High(FVisToolbars) do
      FVisToolbars[I] := LVVisibilityToolbars.Items[I].Checked;

  for var I := 0 to MaxTab do
    for var J := 0 to MaxTabItem do
      FVis1[I, J] := FVis2[I, J];
end;

procedure TFConfiguration.VisibilityModelToView;
begin
  if LVVisibilityTabs.Items.Count = 0 then
    PrepareVisibilityPage;

  for var I := 0 to LVVisibilityTabs.Items.Count - 1 do
    LVVisibilityTabs.Items[I].Checked := FVisTabs[I];

  for var I := 0 to LVVisibilityMenus.Items.Count - 1 do
    LVVisibilityMenus.Items[I].Checked := FVisMenus[I];

  for var I := 0 to LVVisibilityToolbars.Items.Count - 1 do
    LVVisibilityToolbars.Items[I].Checked := FVisToolbars[I];

  // save visibility settings in FVis2 for changing
  for var I := 0 to High(FVis1) do
    for var J := 0 to High(FVis1[I]) do
      FVis2[I, J] := FVis1[I, J];

  LVVisibilityElements.OnItemChecked := nil;
  for var I := 0 to LVVisibilityElements.Items.Count - 1 do
    LVVisibilityElements.Items[I].Checked := FVis2[FVisSelectedTab, I];
  LVVisibilityElements.OnItemChecked := LVVisibilityElementsItemChecked;
end;

procedure TFConfiguration.PrepareVisibilityPage;
var
  TabsControl: TSpTBXTabControl;
  AnItem: TListItem;
begin
  TabsControl := FJava.TabsControl;
  for var I := 0 to TabsControl.Items.Count - 1 do
  begin
    AnItem := LVVisibilityTabs.Items.Add;
    AnItem.Caption := TabsControl.Items[I].Caption;
    AnItem.Checked := FVisTabs[I];
  end;
  LVVisibilityTabs.ItemIndex := 0;
  LVVisibilityTabs.Selected := LVVisibilityTabs.Items[0];
  LVVisibilityTabs.ItemFocused := LVVisibilityTabs.Items[0];

  for var I := 0 to High(FVisMenus) do
  begin
    AnItem := LVVisibilityMenus.Items.Add;
    AnItem.Checked := FVisMenus[I];
    AnItem.Caption := ReplaceStr(FJava.MainMenu.Items[I + 3].Caption, '&', '');
  end;

  LVVisibilityToolbars.Items.BeginUpdate;
  for var I := 0 to High(FVisToolbars) do
  begin
    AnItem := LVVisibilityToolbars.Items.Add;
    AnItem.Checked := FVisToolbars[I];
  end;
  LVVisibilityToolbars.Items[0].Caption :=
    ReplaceStr(FJava.MainMenu.Items[0].Caption, '&', '');
  LVVisibilityToolbars.Items[1].Caption :=
    ReplaceStr(FJava.MainMenu.Items[1].Caption, '&', '');
  LVVisibilityToolbars.Items[2].Caption := 'Editor';
  LVVisibilityToolbars.Items[3].Caption :=
    ReplaceStr(FJava.MainMenu.Items[4].Caption, '&', '');
  LVVisibilityToolbars.Items[4].Caption :=
    ReplaceStr(FJava.MainMenu.Items[6].Caption, '&', '');
  LVVisibilityToolbars.Items[5].Caption := 'BorderLayout';
  LVVisibilityToolbars.Items.EndUpdate;
  LVVisibilityTabsClick(Self);
end;

procedure TFConfiguration.LVVisibilityElementsItemChecked(Sender: TObject;
  Item: TListItem);
begin
  FVis2[FVisSelectedTab, Item.Index] := Item.Checked;
end;

procedure TFConfiguration.LVVisibilityTabsClick(Sender: TObject);
var
  Str: string;
  Posi, Tab: Integer;
  ToolBar: TToolBar;
  AnItem: TListItem;
begin
  Tab := Max(LVVisibilityTabs.ItemIndex, 0);
  LVVisibilityElements.Clear;
  case Tab of
    0:
      begin
        LVVisibilityElements.SmallImages := FJava.vilProgramLight;
        ToolBar := FJava.ToolbarProgram;
      end;
    1:
      begin
        LVVisibilityElements.SmallImages := FJava.vilAWTLight;
        ToolBar := FJava.ToolbarAWT;
      end;
    2:
      begin
        LVVisibilityElements.SmallImages := FJava.vilSwing1Light;
        ToolBar := FJava.ToolBarSwing1;
      end;
    3:
      begin
        LVVisibilityElements.SmallImages := FJava.vilSwing2;
        ToolBar := FJava.ToolbarSwing2;
      end;
    4:
      begin
        LVVisibilityElements.SmallImages := FJava.vilLayoutLight;
        ToolBar := FJava.ToolBarLayout;
      end;
    5:
      begin
        LVVisibilityElements.SmallImages := FJava.vilUtilities;
        ToolBar := FJava.ToolBarUtilities;
      end;
    6:
      begin
        LVVisibilityElements.SmallImages := FJava.vilFXBaseLight;
        ToolBar := FJava.ToolBarFXBase;
      end;
    7:
      begin
        LVVisibilityElements.SmallImages := FJava.vilFXControls;
        ToolBar := FJava.ToolBarFXControls;
      end;
    8:
      begin
        LVVisibilityElements.SmallImages := FJava.vilFXShapesLight;
        ToolBar := FJava.ToolBarFXShapes;
      end;
  else
    Exit;
  end;
  LVVisibilityElements.OnItemChecked := nil;
  if Assigned(ToolBar) then
  begin
    for var I := 0 to ToolBar.ButtonCount - 1 do
    begin
      Str := ToolBar.Buttons[I].Hint;
      Posi := Pos(' ', Str);
      if Posi > 0 then
        Str := Copy(Str, 1, Posi - 1);
      AnItem := LVVisibilityElements.Items.Add;
      AnItem.Caption := Str;
      AnItem.Checked := FVis2[Tab, I];
      AnItem.ImageIndex := I;
    end;
  end;
  FVisSelectedTab := Tab;
  LVVisibilityElements.OnItemChecked := LVVisibilityElementsItemChecked;
end;

procedure TFConfiguration.SaveWindow;
var
  Num: Integer;
  Str1, Str2: string;
  Form: TFForm;
begin
  WriteStringU('Font', 'Name', FEditFont.Name);
  WriteIntegerU('Font', 'Size', FEditFont.Size);
  WriteStringU('UML', 'Name', FUMLFont.Name);
  WriteIntegerU('UML', 'Size', FUMLFont.Size);
  WriteIntegerU('UML', 'ShowParameter', FDiShowParameter);
  WriteIntegerU('UML', 'Visibility', FDiVisibilityFilter);
  WriteIntegerU('UML', 'SortOrder', FDiSortOrder);
  WriteIntegerU('UML', 'ShowIcons', FDiShowIcons);
  WriteStringU('Structogram', 'Name', FStructogramFont.Name);
  WriteIntegerU('Structogram', 'Size', FStructogramFont.Size);
  WriteStringU('SequenceDiagram', 'Name', FSequenceFont.Name);
  WriteIntegerU('SequenceDiagram', 'Size', FSequenceFont.Size);
  WriteStringU('Window', 'Current', '');
  Num := 0;
  for var I := 0 to FJava.TDIFormsList.Count - 1 do
  begin
    Form := FJava.TDIFormsList[I];
    if Assigned(Form) then
    begin
      Inc(Num);
      Str1 := Form.GetState + Form.GetFormType;
      if (Form.FormTag in [1, 2, 11, 14]) and not Form.AlreadySavedAs and Form.IsDefaultFilename
      then
        FJava.DoSave(Form, FCreateBAKFiles);
      Str2 := Form.Pathname;
      if HasUMLExtension(Str2) and Form.IsDefaultFilename then
        WriteStringU('Window', 'Win' + IntToStr(Num),
          Str1 + RemovePortableDrive(FTempDir) + ExtractFileName(Str2))
      else
        WriteStringU('Window', 'Win' + IntToStr(Num),
          Str1 + RemovePortableDrive(Str2));
      if Form = FJava.ActiveTDIChild then
        WriteStringU('Window', 'Current', Str1 + RemovePortableDrive(Str2));
    end;
  end;
  WriteIntegerU('Window', 'Wins', Num);
  WriteStringU('Program', 'Sourcepath', RemovePortableDrive(FSourcepath));
  WriteBoolU('Program', 'WindowStateMaximized', FWindowStateMaximized);
  WriteStringU('Program', 'Version', UDlgAbout.Version);
  WriteIntegerU('Program', 'FileFilter', FJava.ODOpen.FilterIndex);
  FJava.SaveBounds;
end;

procedure TFConfiguration.SaveStrings(const Key, AName: string;
  Values: TStrings);
begin
  var
  Str := '';
  for var I := 0 to Values.Count - 1 do
    Str := Str + Values[I] + ' |';
  WriteStringU(Key, AName, RemovePortableDrive(Str));
end;

procedure TFConfiguration.ReadStrings(const Key, AName: string;
  Values: TStrings);
var
  Str: string;
  Posi: Integer;
begin
  try
    Str := ReadStringU(Key, AName, '');
  except
    Str := '';
  end;
  Values.Clear;
  Posi := Pos(' |', Str);
  while Posi > 1 do
  begin
    Values.Add(AddPortableDrive(Copy(Str, 1, Posi - 1)));
    Delete(Str, 1, Posi + 1);
    Posi := Pos(' |', Str);
  end;
end;

procedure TFConfiguration.SaveFavorites(Favorites: TStringList);
begin
  for var I := 0 to Favorites.Count - 1 do
    WriteStringU('Favoriten', 'Fav' + IntToStr(I + 1), Favorites[I]);
  WriteIntegerU('Favoriten', 'Favs', Favorites.Count);
end;

procedure TFConfiguration.ReadFavorites(var Favorites: TStringList);
var
  Count: Integer;
  Str: string;
begin
  Count := ReadIntegerU('Favoriten', 'Favs', 0);
  for var I := 1 to Count do
  begin
    Str := ReadStringU('Favoriten', 'Fav' + IntToStr(I), '');
    if Str <> '' then
      Favorites.Add(Str);
  end;
end;

procedure TFConfiguration.ReplaceClNone;
begin
  if IsDark then
    FEditorTextColor := StyleServices.GetStyleFontColor(sfTabTextInactiveNormal)
  else
    FEditorTextColor := clBlack;
  with FJavaHighlighter do
  begin
    if CommentAttri.Foreground = clNone then
      CommentAttri.Foreground := FEditorTextColor;
    if DocumentAttri.Foreground = clNone then
      DocumentAttri.Foreground := FEditorTextColor;
    if IdentifierAttri.Foreground = clNone then
      IdentifierAttri.Foreground := FEditorTextColor;
    if InvalidAttri.Foreground = clNone then
      InvalidAttri.Foreground := FEditorTextColor;
    if NumberAttri.Foreground = clNone then
      NumberAttri.Foreground := FEditorTextColor;
    if KeyAttri.Foreground = clNone then
      KeyAttri.Foreground := FEditorTextColor;
    if SpaceAttri.Foreground = clNone then
      SpaceAttri.Foreground := FEditorTextColor;
    if StringAttri.Foreground = clNone then
      StringAttri.Foreground := FEditorTextColor;
    if SymbolAttri.Foreground = clNone then
      SymbolAttri.Foreground := FEditorTextColor;
  end;
  with FHTMLHighlighter do
  begin
    if CommentAttri.Foreground = clNone then
      CommentAttri.Foreground := FEditorTextColor;
    if AndAttri.Foreground = clNone then
      AndAttri.Foreground := FEditorTextColor;
    if IdentifierAttri.Foreground = clNone then
      IdentifierAttri.Foreground := FEditorTextColor;
    if TextAttri.Foreground = clNone then
      TextAttri.Foreground := FEditorTextColor;
    if KeyAttri.Foreground = clNone then
      KeyAttri.Foreground := FEditorTextColor;
    if UndefKeyAttri.Foreground = clNone then
      UndefKeyAttri.Foreground := FEditorTextColor;
    if SpaceAttri.Foreground = clNone then
      SpaceAttri.Foreground := FEditorTextColor;
    if ValueAttri.Foreground = clNone then
      ValueAttri.Foreground := FEditorTextColor;
    if SymbolAttri.Foreground = clNone then
      SymbolAttri.Foreground := FEditorTextColor;
  end;
end;

procedure TFConfiguration.LoadUserColors;
var
  Path: string;
begin
  for var I := 0 to 6 do
    LoadDefaultColors(I);
  if FUseRegistry then
  begin
    FJavaHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, GetRegPath + '\Java');
    FHTMLHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, GetRegPath + '\HTML');
  end
  else if Assigned(FUserIniFile) then
  begin
    Path := ExtractFilePath(FUserIniFile.Filename);
    FJavaHighlighter.LoadFromFile(Path + 'JEJavaCol.ini', FEditorStyle);
    FHTMLHighlighter.LoadFromFile(Path + 'JEHTMLCol.ini', FEditorStyle);
  end;
end;

procedure TFConfiguration.LoadDefaultColors(Typ: Integer);
begin
  var
  Path := FEditorFolder + 'styles' + PathDelim;
  case Typ of
    0:
      FJavaHighlighter.LoadFromFile(Path + 'DefaultColorsJava.ini',
        FEditorStyle);
    1:
      FHTMLHighlighter.LoadFromFile(Path + 'DefaultColorsHTML.ini',
        FEditorStyle);
    2:
      SetDefaultBracketColor;
    3:
      if Assigned(FPascalHighlighter) then
        FPascalHighlighter.LoadFromFile(Path + 'DefaultColorsPascal.ini',
          FEditorStyle);
    4:
      if Assigned(FPHPHighlighter) then
        FPHPHighlighter.LoadFromFile(Path + 'DefaultColorsPHP.ini',
          FEditorStyle);
    5:
      if Assigned(FCSSHighlighter) then
        FCSSHighlighter.LoadFromFile(Path + 'DefaultColorsCSS.ini',
          FEditorStyle);
    6:
      if Assigned(FGeneralHighlighter) then
        FGeneralHighlighter.LoadFromFile(Path + 'DefaultColorsGeneral.ini',
          FEditorStyle);
  end;
end;

function TFConfiguration.GetRegPath: string;
begin
  Result := '\Software\JavaEditor\Colors';
  if FEditorStyle <> 'Default' then
    Result := Result + '\' + FEditorStyle;
end;

// https://www.slant.co/topics/358/~best-color-themes-for-text-editors#5

procedure TFConfiguration.SaveUserColors;

  procedure SaveAttributes(Attr: TSynHighlighterAttributes);
  var
    Key: string;
  begin
    Key := 'Colors\' + Attr.Name;
    WriteIntegerU(Key, 'Background', Attr.Background);
    WriteIntegerU(Key, 'Foreground', Attr.Foreground);
    WriteIntegerU(Key, 'Style', Attr.IntegerStyle);
  end;

begin
  if FUseRegistry then
  begin
    try
      FJavaHighlighter.SaveToRegistry(HKEY_CURRENT_USER, GetRegPath + '\Java');
      FHTMLHighlighter.SaveToRegistry(HKEY_CURRENT_USER, GetRegPath + '\HTML');
    except
      on E: Exception do
        ErrorMsg('Error in SaveUserColors! ' + Format(LNGCanNotWriteRegistry,
          ['HKEY_CURRENT_USER\' + GetRegPath]) + ' Error: ' + E.Message);
    end;
  end
  else if Assigned(FUserIniFile) then
  begin
    try
      FJavaHighlighter.SaveToFile(ExtractFilePath(FUserIniFile.Filename) +
        'JEJavaCol.ini', FEditorStyle);
      FHTMLHighlighter.SaveToFile(ExtractFilePath(FUserIniFile.Filename) +
        'JEHTMLCol.ini', FEditorStyle);
      SaveAttributes(FAttrBrackets);
    except
      on E: Exception do
        ErrorMsg('Error in SaveUserColors! ! ' + Format(LNGCanNotWrite,
          [FUserIniFile.Filename]) + ' Error: ' + E.Message);
    end;
  end;
end;

procedure TFConfiguration.ReadUserColors;

  procedure ReadAttributes(var Attr: TSynHighlighterAttributes);
  begin
    var
    Key := 'Colors\' + Attr.Name;
    Attr.Background := ReadIntegerU(Key, 'Background', Attr.Background);
    Attr.Foreground := ReadIntegerU(Key, 'Foreground', Attr.Foreground);
    Attr.IntegerStyle := ReadIntegerU(Key, 'Style', Attr.IntegerStyle);
  end;

begin
  LoadUserColors;
  ReadAttributes(FAttrBrackets);
  ReplaceClNone;
end;

procedure TFConfiguration.WriteStringM(const Key, AName, Value: string);
begin
  WriteString(Machine, Key, AName, Value);
end;

procedure TFConfiguration.WriteStringF(const Key, AName, Value: string);
begin
  WriteString(AllUsers, Key, AName, Value);
end;

procedure TFConfiguration.WriteStringU(const Key, AName, Value: string);
begin
  WriteString(User, Key, AName, Value);
end;

procedure TFConfiguration.WriteString(Dest: Integer;
  const Key, AName, Value: string);
begin
  if FDumpMode then
  begin
    case Dest of
      0:
        FDumpIniFileHKLM.WriteString(Key, AName, Value);
      1:
        FDumpIniFileAllUsers.WriteString(Key, AName, Value);
      2:
        FDumpIniFileHKCU.WriteString(Key, AName, Value);
    end;
    Exit;
  end;
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        case Dest of
          0:
            RootKey := HKEY_LOCAL_MACHINE;
          1:
            RootKey := HKEY_CURRENT_USER;
          2:
            RootKey := HKEY_CURRENT_USER;
        end;
        Access := KEY_WRITE;
        if OpenKey('\Software\JavaEditor\' + Key, True) then
        begin
          WriteString(AName, Value);
          CloseKey;
        end;
      end
    else if (Dest = 0) and Assigned(FMachineIniFile) then
      FMachineIniFile.WriteString(Key, AName, Value)
    else if Assigned(FUserIniFile) then
      FUserIniFile.WriteString(Key, AName, Value);
  except
    // no logging
  end;
end;

function TFConfiguration.ReadStringFile(const Key, AName,
  Default: string): string;
begin
  if FLockedPaths or not FUseRegistry then
    Result := AddPortableDrive(ReadStringM(Key, AName, Default))
  else
  begin
    Result := AddPortableDrive(ReadStringF(Key, AName, Default));
    if not IsHTTP(Result) and not FileExists(DissolveUsername(Result)) then
      Result := AddPortableDrive(ReadStringU(Key, AName, Default));
    if not IsHTTP(Result) and not FileExists(DissolveUsername(Result)) then
      Result := AddPortableDrive(ReadStringM(Key, AName, Default));
  end;
end;

procedure TFConfiguration.WriteStringFile(const Key, AName, Value: string);
begin
  var
  AValue := RemovePortableDrive(Value);
  if FLockedPaths or not FUseRegistry then
    WriteStringM(Key, AName, AValue)
  else
    WriteStringF(Key, AName, AValue);
end;

function TFConfiguration.ReadStringDirectory(const Key, AName: string): string;
begin
  if FLockedPaths or not FUseRegistry then
    Result := ReadStringM(Key, AName, '')
  else
  begin
    Result := ReadStringF(Key, AName, '');
    if Result = '' then
      Result := ReadStringU(Key, AName, '');
    if Result = '' then
      Result := ReadStringM(Key, AName, '');
  end;
  Result := AddPortableDrive(Result);
end;

procedure TFConfiguration.WriteStringDirectory(const Key, AName, Value: string);
begin
  var
  AValue := RemovePortableDrive(Value);
  if FLockedPaths or not FUseRegistry then
    WriteStringM(Key, AName, AValue)
  else
    WriteStringF(Key, AName, AValue);
end;

function TFConfiguration.ReadStringM(const Key, AName, Default: string): string;
begin
  Result := ReadString(Machine, Key, AName, Default);
end;

function TFConfiguration.ReadStringF(const Key, AName, Default: string): string;
begin
  Result := ReadString(AllUsers, Key, AName, Default);
end;

function TFConfiguration.ReadStringU(const Key, AName, Default: string): string;
begin
  Result := ReadString(User, Key, AName, Default);
end;

function TFConfiguration.ReadString(Dest: Integer;
  const Key, AName, Default: string): string;
begin
  Result := Default;
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        case Dest of
          0:
            RootKey := HKEY_LOCAL_MACHINE;
          1:
            RootKey := HKEY_CURRENT_USER;
          2:
            RootKey := HKEY_CURRENT_USER;
        end;
        Access := KEY_READ;
        if OpenKey('\Software\JavaEditor\' + Key, False) then
        begin
          if ValueExists(AName) then
            Result := ReadString(AName);
          CloseKey;
        end;
      end
    else if (Dest = 0) and Assigned(FMachineIniFile) then
      Result := FMachineIniFile.ReadString(Key, AName, Default)
    else if Assigned(FUserIniFile) and Assigned(FMachineIniFile) then
      if FMachineIniFile.ValueExists(Key, AName) then
        Result := FMachineIniFile.ReadString(Key, AName, Default)
      else
        Result := FUserIniFile.ReadString(Key, AName, Default)
  except
    // no logging
  end;
end;

procedure TFConfiguration.WriteIntegerU(const Key, AName: string;
  Value: Integer);
begin
  if FDumpMode then
  begin
    FDumpIniFileHKCU.WriteInteger(Key, AName, Value);
    Exit;
  end;
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        RootKey := HKEY_CURRENT_USER;
        Access := KEY_WRITE;
        if OpenKey('\Software\JavaEditor\' + Key, True) then
        begin
          WriteInteger(AName, Value);
          CloseKey;
        end;
      end
    else if Assigned(FUserIniFile) then
      FUserIniFile.WriteInteger(Key, AName, Value);
  except
    // no logging
  end;
end;

function TFConfiguration.ReadIntegerU(const Key, AName: string;
  Default: Integer): Integer;
begin
  Result := Default;
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        RootKey := HKEY_CURRENT_USER;
        Access := KEY_READ;
        if OpenKey('\Software\JavaEditor\' + Key, False) then
        begin
          if ValueExists(AName) then
            Result := ReadInteger(AName);
          CloseKey;
        end;
      end
    else if Assigned(FUserIniFile) then
      if Assigned(FMachineIniFile) and FMachineIniFile.ValueExists(Key, AName)
      then
        Result := FMachineIniFile.ReadInteger(Key, AName, Default)
      else
        Result := FUserIniFile.ReadInteger(Key, AName, Default)
  except
    // no logging
  end;
end;

procedure TFConfiguration.WriteBoolM(const Key, AName: string; Value: Boolean);
begin
  WriteBool(True, Key, AName, Value);
end;

procedure TFConfiguration.WriteBoolU(const Key, AName: string; Value: Boolean);
begin
  WriteBool(False, Key, AName, Value);
end;

procedure TFConfiguration.WriteBool(Machine: Boolean; const Key, AName: string;
  Value: Boolean);
begin
  if FDumpMode then
  begin
    if Machine then
      FDumpIniFileHKLM.WriteBool(Key, AName, Value)
    else
      FDumpIniFileHKCU.WriteBool(Key, AName, Value);
    Exit;
  end;
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        if Machine then
          RootKey := HKEY_LOCAL_MACHINE
        else
          RootKey := HKEY_CURRENT_USER;
        Access := KEY_WRITE;
        if OpenKey('\Software\JavaEditor\' + Key, True) then
        begin
          WriteBool(AName, Value);
          CloseKey;
        end;
      end
    else if Machine and Assigned(FMachineIniFile) then
      FMachineIniFile.WriteBool(Key, AName, Value)
    else if Assigned(FUserIniFile) then
      FUserIniFile.WriteBool(Key, AName, Value);
  except
    // no logging
  end;
end;

function TFConfiguration.ReadBoolM(const Key, AName: string;
  Default: Boolean): Boolean;
begin
  Result := ReadBool(True, Key, AName, Default);
end;

function TFConfiguration.ReadBoolU(const Key, AName: string;
  Default: Boolean): Boolean;
begin
  Result := ReadBool(False, Key, AName, Default);
end;

function TFConfiguration.ReadBool(Machine: Boolean; const Key, AName: string;
  Default: Boolean): Boolean;
begin
  Result := Default;
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        if Machine then
          RootKey := HKEY_LOCAL_MACHINE
        else
          RootKey := HKEY_CURRENT_USER;
        Access := KEY_READ;
        if OpenKey('\Software\JavaEditor\' + Key, False) then
        begin
          if ValueExists(AName) then
            Result := ReadBool(AName);
          CloseKey;
        end;
      end
    else if Machine and Assigned(FMachineIniFile) then
      Result := FMachineIniFile.ReadBool(Key, AName, Default)
    else if Assigned(FUserIniFile) then
      if Assigned(FMachineIniFile) and FMachineIniFile.ValueExists(Key, AName)
      then
        Result := FMachineIniFile.ReadBool(Key, AName, Default)
      else
        Result := FUserIniFile.ReadBool(Key, AName, Default);
  except
    // no logging
  end;
end;

procedure TFConfiguration.WriteBinaryStreamU(const Key, AName: string;
  Value: TStream);
begin
  WriteBinaryStream(False, Key, AName, Value);
end;

procedure TFConfiguration.WriteBinaryStream(Machine: Boolean;
  const Key, AName: string; Value: TStream);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream(Value);
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        if Machine then
          RootKey := HKEY_LOCAL_MACHINE
        else
          RootKey := HKEY_CURRENT_USER;
        Access := KEY_WRITE;
        if OpenKey('\Software\JavaEditor\' + Key, True) then
        begin
          WriteBinaryData(AName, Pointer(PByte(Stream.Memory) + Stream.Position)
            ^, Stream.Size - Stream.Position);
          CloseKey;
        end;
      end
    else if Machine and Assigned(FMachineIniFile) then
      FMachineIniFile.WriteBinaryStream(Key, AName, Stream)
    else if Assigned(FUserIniFile) then
      FUserIniFile.WriteBinaryStream(Key, AName, Stream);
  except
    // no logging
  end;
end;

function TFConfiguration.ReadBinaryStreamU(const Key, AName: string;
  Value: TStream): Integer;
begin
  Result := ReadBinaryStream(False, Key, AName, Value);
end;

function TFConfiguration.ReadBinaryStream(Machine: Boolean;
  const Key, AName: string; Value: TStream): Integer;
var
  RegData: TRegDataType;
  Info: TRegDataInfo;
  Stream: TMemoryStream;
begin
  Result := 0;
  try
    if FUseRegistry then
      with FMyRegistry do
      begin
        if Machine then
          RootKey := HKEY_LOCAL_MACHINE
        else
          RootKey := HKEY_CURRENT_USER;
        Access := KEY_READ;
        try
          OpenKey('\Software\JavaEditor\' + Key, False);
          if ValueExists(AName) and GetDataInfo(AName, Info) then
          begin
            Result := Info.DataSize;
            RegData := Info.RegData;
            Stream := TMemoryStream(Value);
            try
              if (RegData = rdBinary) or (RegData = rdUnknown) then
              begin
                Stream.Size := Stream.Position + Info.DataSize;
                Result := ReadBinaryData(AName,
                  Pointer(PByte(Stream.Memory) + Stream.Position)^,
                  Stream.Size);
                if Stream <> Value then
                  Value.CopyFrom(Stream, Stream.Size - Stream.Position);
              end;
            finally
              if Stream <> Value then
                FreeAndNil(Stream);
            end;
          end
          else
            Result := 0;
        finally
          CloseKey;
        end;
      end
    else if Machine and Assigned(FMachineIniFile) then
      Result := FMachineIniFile.ReadBinaryStream(Key, AName, Value)
    else if Assigned(FUserIniFile) then
      if Assigned(FMachineIniFile) and FMachineIniFile.ValueExists(Key, AName)
      then
        Result := FMachineIniFile.ReadBinaryStream(Key, AName, Value)
      else
        Result := FUserIniFile.ReadBinaryStream(Key, AName, Value);
  except
    // no logging
  end;
end;

procedure TFConfiguration.MakeAssociations;
var
  Reg: TRegistry;
  Str, Str1: string;
  Posi: Integer;

  procedure EditAssociation(const Extension: string; DoCreate: Boolean);
  begin
    with Reg do
    begin
      try
        Access := KEY_ALL_ACCESS;
        RootKey := HKEY_LOCAL_MACHINE;
        if DoCreate then
        begin
          if OpenKey('SOFTWARE\Classes\' + Extension, True) then
            WriteString('', 'JavaEditor');
        end
        else
        begin
          if OpenKey('SOFTWARE\Classes\' + Extension, False) then
            if ReadString('') = 'JavaEditor' then
              WriteString('', '');
        end;
        CloseKey;

        // HKEY_CURRENT_USER\Software\Classes
        RootKey := HKEY_CURRENT_USER;
        if DoCreate then
        begin
          if OpenKey('SOFTWARE\Classes\' + Extension, True) then
            WriteString('', 'JavaEditor');
        end
        else
        begin
          if OpenKey('SOFTWARE\Classes\' + Extension, False) then
            if ReadString('') = 'JavaEditor' then
              WriteString('', '');
        end;
        CloseKey;
      except
        on E: Exception do
          ErrorMsg('Error in EditAssociation! ' + Format(LNGCanNotWriteRegistry,
            ['SOFTWARE\Classes\']) + ' Error: ' + E.Message);
      end;
    end;
  end;

begin
  RegisterJavaeditor;
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      EditAssociation(CBAssociationJava.Caption, CBAssociationJava.Checked);
      EditAssociation(CBAssociationJfm.Caption, CBAssociationJfm.Checked);
      EditAssociation(CBAssociationUml.Caption, CBAssociationUml.Checked);
      EditAssociation(CBAssociationJep.Caption, CBAssociationJep.Checked);
      EditAssociation(CBAssociationHtml.Caption, CBAssociationHtml.Checked);
      EditAssociation(CBAssociationTxt.Caption, CBAssociationTxt.Checked);
      EditAssociation(CBAssociationJsp.Caption, CBAssociationJsp.Checked);
      EditAssociation(CBAssociationPhp.Caption, CBAssociationPhp.Checked);
      EditAssociation(CBAssociationCss.Caption, CBAssociationCss.Checked);
      EditAssociation(CBAssociationInc.Caption, CBAssociationInc.Checked);
      EditAssociation(CBAssociationJsg.Caption, CBAssociationJsg.Checked);
      Str := FAdditionalAssociations + ';';
      Posi := Pos(';', Str);
      while Posi > 0 do
      begin
        Str1 := Copy(Str, 1, Posi - 1);
        Delete(Str, 1, Posi);
        Posi := Pos('.', Str1);
        if Posi > 0 then
          Delete(Str1, 1, Posi);
        if (Str1 <> '') and (Pos(' ', Str1) = 0) then
          EditAssociation('.' + Str1, True);
        Posi := Pos(';', Str);
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure TFConfiguration.RegisterJavaeditor;
var
  JavaEditor, Filename: string;
  Reg: TRegistry;

  procedure WriteToRegistry(const Key: string);
  begin
    with Reg do
    begin
      OpenKey(Key, True);
      WriteString('', 'Java-Editor');
      CloseKey;
      OpenKey(Key + '\DefaultIcon', True);
      WriteString('', JavaEditor + ',0');
      CloseKey;
      OpenKey(Key + '\Shell\Open\command', True);
      WriteString('', JavaEditor);
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec', True);
      WriteString('', '[FileOpen("%1")]');
      OpenKey('Application', True);
      Filename := ExtractFileName(ParamStr(0));
      Filename := Copy(Filename, 1, Length(Filename) - 4);
      WriteString('', Filename);
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec\topic', True);
      WriteString('', 'System');
      CloseKey;
    end;
  end;

begin
  JavaEditor := HideBlanks(ParamStr(0));
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      Access := KEY_ALL_ACCESS;
      RootKey := HKEY_LOCAL_MACHINE;
      WriteToRegistry('SOFTWARE\Classes\JavaEditor');
      RootKey := HKEY_CURRENT_USER;
      WriteToRegistry('\SOFTWARE\Classes\JavaEditor');
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TFConfiguration.LBColorElementsClick(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
  Str: string;
  Int: Integer;
begin
  Attr := TSynHighlighterAttributes.Create('', '');
  try
    if LBColorElements.ItemIndex < 0 then
      LBColorElements.ItemIndex := 0;
    Str := LBColorElements.Items[LBColorElements.ItemIndex];
    Int := Max(LNGColornameToAttributIndex(Str), 0);
    case RGColors.ItemIndex of
      0:
        Attr.Assign(FJavaHighlighter.Attribute[Int]);
      1:
        Attr.Assign(FHTMLHighlighter.Attribute[Int]);
      2:
        Attr.Assign(FAttrBrackets);
    end;
    CBTextColorBox.Selected := Attr.Foreground;
    CBBackgroundColorBox.Selected := Attr.Background;
    CBNoBackgroundColor.Checked := (Attr.Background = clNone);
    CBBackgroundColorBox.Enabled := not CBNoBackgroundColor.Checked;
    CBBold.Checked := (fsBold in Attr.Style);
    CBItalic.Checked := (fsItalic in Attr.Style);
    CBUnderline.Checked := (fsUnderline in Attr.Style);
  finally
    FreeAndNil(Attr);
  end;
end;

function TFConfiguration.LNGColornameToAttributIndex(const Str: string)
  : Integer;
// determines from User-defined token child name the
// Index of the responsible highlighter attribute
begin
  case RGColors.ItemIndex of
    0:
      if Str = _(LNGComment) then
        Result := 0
      else if Str = _(LNGDocumentation) then
        Result := 1
      else if Str = _(LNGIdentifier) then
        Result := 2
      else if Str = _(LNGInvalidSymbol) then
        Result := 3
      else if Str = _(LNGNumber) then
        Result := 4
      else if Str = _(LNGReservedWord) then
        Result := 5
      else if Str = _(LNGSpace) then
        Result := 6
      else if Str = _(LNGString) then
        Result := 7
      else if Str = _(LNGSymbol) then
        Result := 8
      else
        Result := -1;
    1:
      if Str = _(LNGComment) then
        Result := 0
      else if Str = _(LNGEscape) then
        Result := 1
      else if Str = _(LNGAttribute) then
        Result := 2
      else if Str = _(LNGTag) then
        Result := 3
      else if Str = _(LNGSpace) then
        Result := 4
      else if Str = _(LNGSymbol) then
        Result := 5
      else if Str = _(LNGText) then
        Result := 6
      else if Str = _(LNGUnknownWord) then
        Result := 7
      else if Str = _(LNGValue) then
        Result := 8
      else
        Result := -1;
    2:
      if Str = _(LNGBrackets) then
        Result := 0
      else
        Result := -1;
  else
    Result := -1;
  end;
end;

procedure TFConfiguration.CBProviderDropDown(Sender: TObject);
begin
  LLMAssistantViewToModel;
end;

procedure TFConfiguration.CBProviderSelect(Sender: TObject);
begin
  LLMAssistantModelToView(LLMAssistantSettings);
end;

procedure TFConfiguration.CBChatProviderDropDown(Sender: TObject);
begin
  LLMChatViewToModel;
end;

procedure TFConfiguration.CBChatProviderSelect(Sender: TObject);
begin
  LLMChatModelToView(LLMChatSettings);
end;

procedure TFConfiguration.CBColorBoxChange(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
  AttrStyle: TFontStyles;
  Int: Integer;
  Str: string;
begin
  CBBackgroundColorBox.Enabled := not CBNoBackgroundColor.Checked;
  // conversion LNGColorName in Highlighter.Attributname
  if LBColorElements.ItemIndex < 0 then
    LBColorElements.ItemIndex := 0;
  Str := LBColorElements.Items[LBColorElements.ItemIndex];
  Int := Max(LNGColornameToAttributIndex(Str), 0);
  case RGColors.ItemIndex of
    0:
      Str := FJavaHighlighter.Attribute[Int].Name;
    1:
      Str := FHTMLHighlighter.Attribute[Int].Name;
    2:
      Str := 'Brackets';
  end;
  // Change the relevant highlighter attribute
  Attr := TSynHighlighterAttributes.Create(Str, Str);
  try
    AttrStyle := [];
    if CBNoBackgroundColor.Checked then
      Attr.Background := clNone
    else
      Attr.Background := CBBackgroundColorBox.Selected;
    Attr.Foreground := CBTextColorBox.Selected;

    if CBBold.Checked then
      Include(AttrStyle, fsBold);
    if CBItalic.Checked then
      Include(AttrStyle, fsItalic);
    if CBUnderline.Checked then
      Include(AttrStyle, fsUnderline);
    Attr.Style := AttrStyle;
    case RGColors.ItemIndex of
      0:
        FJavaHighlighter.Attribute[Int].Assign(Attr);
      1:
        FHTMLHighlighter.Attribute[Int].Assign(Attr);
      2:
        FAttrBrackets.Assign(Attr);
    end;
  finally
    FreeAndNil(Attr);
  end;
end;

procedure TFConfiguration.ShowColorElements;
begin
  with LBColorElements do
  begin
    Clear;
    case RGColors.ItemIndex of
      0:
        begin
          Items.Add(_(LNGIdentifier));
          Items.Add(_(LNGDocumentation));
          Items.Add(_(LNGComment));
          Items.Add(_(LNGSpace));
          Items.Add(_(LNGReservedWord));
          Items.Add(_(LNGString));
          Items.Add(_(LNGSymbol));
          Items.Add(_(LNGInvalidSymbol));
          Items.Add(_(LNGNumber));
        end;
      1:
        begin
          Items.Add(_(LNGAttribute));
          Items.Add(_(LNGEscape));
          Items.Add(_(LNGComment));
          Items.Add(_(LNGSpace));
          Items.Add(_(LNGTag));
          Items.Add(_(LNGSymbol));
          Items.Add(_(LNGText));
          Items.Add(_(LNGUnknownWord));
          Items.Add(_(LNGValue));
        end;
      2:
        Items.Add(_(LNGBrackets));
    end;
    ItemIndex := 0;
    LBColorElementsClick(Self);
  end;
end;

procedure TFConfiguration.BDefaultColorsClick(Sender: TObject);
begin
  LoadDefaultColors(RGColors.ItemIndex);
  LBColorElementsClick(Self);
  for var I := 0 to FJava.TDIEditFormCount - 1 do
    FJava.TDIEditFormGet(I).SetHighlighter;
end;

procedure TFConfiguration.CBUseIEinternForDocumentsClick(Sender: TObject);
begin
  if CBUseIEinternForDocuments.Checked and not ExplorerTest then
  begin
    MessageDlg(_('Internet Explorer is not installed.'), mtWarning, [mbOK], 0);
    CBUseIEinternForDocuments.Checked := False;
  end;
end;

procedure TFConfiguration.CBHeaderChange(Sender: TObject);
var
  Str: string;
  PosL, PosR: Integer;
begin
  Str := EHeader.Text;
  PosL := 1;
  while (PosL <= Length(Str)) and (Str[PosL] <> '#') do
    Inc(PosL);
  if PosL > Length(Str) then
  begin // FNo # found
    Str := '#' + Str + '#';
    PosL := 1;
    PosR := Length(Str);
  end
  else
  begin // search second #
    PosR := PosL + 1;
    while (PosR <= Length(Str)) and (Str[PosR] <> '#') do
      Inc(PosR);
    if PosR > Length(Str) then // FNo second # found
      if PosL = 1 then
      begin
        Str := Str + '#';
        PosR := Length(Str);
      end
      else
      begin
        Str := '#' + Str;
        PosR := PosL + 1;
        PosL := 1;
      end;
  end;
  case RGAdjustment.ItemIndex of
    0:
      Insert(CBHeader.Text, Str, PosL);
    1:
      Insert(CBHeader.Text, Str, PosR);
    2:
      Str := Str + CBHeader.Text;
  end;
  EHeader.Text := Str;
end;

procedure TFConfiguration.CBFooterChange(Sender: TObject);
var
  Str: string;
  PosL, PosR: Integer;
begin
  Str := EFooter.Text;
  PosL := 1;
  while (PosL <= Length(Str)) and (Str[PosL] <> '#') do
    Inc(PosL);
  if PosL > Length(Str) then
  begin // FNo # found
    Str := '#' + Str + '#';
    PosL := 1;
    PosR := Length(Str);
  end
  else
  begin // search second #
    PosR := PosL + 1;
    while (PosR <= Length(Str)) and (Str[PosR] <> '#') do
      Inc(PosR);
    if PosR > Length(Str) then // FNo second # found
      if PosL = 1 then
      begin
        Str := Str + '#';
        PosR := Length(Str);
      end
      else
      begin
        Str := '#' + Str;
        PosR := PosL + 1;
        PosL := 1;
      end;
  end;
  case RGAdjustment.ItemIndex of
    0:
      Insert(CBFooter.Text, Str, PosL);
    1:
      Insert(CBFooter.Text, Str, PosR);
    2:
      Str := Str + CBFooter.Text;
  end;
  EFooter.Text := Str;
end;

procedure TFConfiguration.RGCommentClick(Sender: TObject);
var
  Str: string;
begin
  case RGComment.ItemIndex of
    0:
      begin
        Str := JavaDocComment;
        MComment.ReadOnly := True;
      end;
    1:
      begin
        Str := ShortComment;
        MComment.ReadOnly := True;
      end;
    2:
      begin
        Str := FFreeComment;
        MComment.ReadOnly := False;
      end;
  end;
  MComment.Text := Str;
end;

procedure TFConfiguration.BTempFolderClick(Sender: TObject);
var
  Path: string;
begin
  if FPortableApplication then
    Path := FEditorFolder + 'App\Temp\'
  else
    Path := TPath.GetTempPath;
  ForceDirectories(Path);
  ShortenPath(ETempFolder, Path);
end;

procedure TFConfiguration.SBTempSelectClick(Sender: TObject);
var
  Path: string;
begin
  if FPortableApplication then
    Path := FEditorFolder + 'App\Temp\'
  else
    Path := TPath.GetTempPath;
  Path := FolderSelect(ETempFolder, Path);
  ForceDirectories(Path);
  ShortenPath(ETempFolder, Path);
end;

procedure TFConfiguration.BJavaDocParameterClick(Sender: TObject);
begin
  EDocParameter.Text := '-author -version';
end;

procedure TFConfiguration.BJavaFXFolderClick(Sender: TObject);
begin
  var
  Dir := EJavaFXFolder.Hint;
  if not SysUtils.DirectoryExists(Dir) then
    Dir := GetEnvironmentVariable('PATH_TO_FX');
  if not SysUtils.DirectoryExists(Dir) then
    Dir := FJDKFolder;
  FolderDialog.DefaultFolder := Dir;
  if FolderDialog.Execute then
  begin
    Dir := ExcludeTrailingPathDelimiter(FolderDialog.Filename);
    if EndsWith(Dir, '\lib') then
      Dir := Copy(Dir, 1, Length(Dir) - 4);
    Dir := ExcludeTrailingPathDelimiter(Dir);
    ShortenPath(EJavaFXFolder, Dir);
    CheckAllFilesAndFolders;
  end;
end;

procedure TFConfiguration.BJarParameterClick(Sender: TObject);
begin
  EJarParameter.Text := '-cfv';
end;

procedure TFConfiguration.BManifestClick(Sender: TObject);
begin
  EJarManifest.Text := '';
end;

procedure TFConfiguration.BJarFilesClick(Sender: TObject);
begin
  var
  FJarCreate := TFJarCreateDialog.Create(Self);
  try
    FJarCreate.Caption := _('Jar create files');
    FJarCreate.Init(FJarCreateAll);
    if FJarCreate.ShowModal = mrOk then
    begin
      FJarCreateCurrent := FJarCreate.JarCreateCurrent;
      FJarCreateAll := FJarCreate.JarCreateAll;
      EJarCreate.Text := FJarCreateCurrent;
    end;
  finally
    FreeAndNil(FJarCreate);
  end;
end;

procedure TFConfiguration.CBKeyboardChange(Sender: TObject);
var
  ShortCut: TNode;
  Str: string;
begin
  LCollision.Visible := False;
  Str := CBKeyboard.Items[CBKeyboard.ItemIndex];
  ShortCut := FKeyboardShortcutsTree.getNode(TextToShortCut(Str));
  if Assigned(ShortCut) then
  begin
    MKeyboard.Text := ShortCut.Data;
    LCollision.Visible := ShortCut.Collision;
  end;
end;

procedure TFConfiguration.RGKeyboardClick(Sender: TObject);
begin
  LCollision.Visible := False;
  case RGKeyboard.ItemIndex of
    0:
      MKeyboard.Text := FEditorKeys.Text;
    1:
      MKeyboard.Text := FMenuKeys.Text;
    2:
      MKeyboard.Text := FAllKeys.Text;
  end;
end;

procedure TFConfiguration.BKeyboardFileClick(Sender: TObject);
begin
  FileSelect(EKeyboardFile, '*.txt|*.txt', 'JEKeyboard.txt', 'Templates');
  CheckFile(EKeyboardFile, True);
  if EKeyboardFile.Color = clWindow then
  begin
    FKeyboardFile := EKeyboardFile.Hint;
    ApplyKeyboardShortcuts;
  end;
end;

procedure TFConfiguration.BVisDefaultClick(Sender: TObject);

  procedure DefaultVis(var Arr: array of Boolean);
  begin
    for var I := 0 to High(Arr) do
      Arr[I] := True;
  end;

begin
  DefaultVis(FVisTabs);
  FVisTabs[4] := False; // Tab Layout
  for var I := 0 to MaxTab do // items on tabs
    for var J := 0 to MaxTabItem do
      FVis2[I, J] := True;
  DefaultVis(FVisMenus);
  FVisMenus[3] := False; // Menu Components
  DefaultVis(FVisToolbars);
  FVisToolbars[5] := False; // Toolbar FBorderLayout

  for var I := 0 to High(FVisTabs) do
    LVVisibilityTabs.Items[I].Checked := FVisTabs[I];
  for var I := 0 to LVVisibilityElements.Items.Count - 1 do
    LVVisibilityElements.Items[I].Checked := True;
  for var I := 0 to High(FVisMenus) do
    LVVisibilityMenus.Items[I].Checked := FVisMenus[I];
  for var I := 0 to High(FVisToolbars) do
    LVVisibilityToolbars.Items[I].Checked := FVisToolbars[I];
end;

procedure TFConfiguration.BVorlageClick(Sender: TObject);
begin
  FODSelect.InitialDir := FEditorFolder + 'Templates';
  FODSelect.Filename := '*.java';
  FODSelect.Filter := '*.java|*.java;*.*|*.*';
  if FODSelect.Execute then
    case TButton(Sender).Tag of
      1:
        ShortenPath(ETemplateConsole, FODSelect.Filename);
      2:
        ShortenPath(ETemplateFrame, FODSelect.Filename);
      3:
        ShortenPath(ETemplateDialog, FODSelect.Filename);
      4:
        ShortenPath(ETemplateApplet, FODSelect.Filename);
      5:
        ShortenPath(ETemplateJFrame, FODSelect.Filename);
      6:
        ShortenPath(ETemplateJDialog, FODSelect.Filename);
      7:
        ShortenPath(ETemplateJApplet, FODSelect.Filename);
      8:
        ShortenPath(ETemplateApplication, FODSelect.Filename);
      9:
        ShortenPath(ETemplateControlstructure, FODSelect.Filename);
      10:
        ShortenPath(ETemplateClass, FODSelect.Filename);
      11:
        ShortenPath(EMindstormsTemplate, FODSelect.Filename);
      12:
        ShortenPath(ETemplateJUnitTest, FODSelect.Filename);
    end;
  CheckFile(ETemplateConsole, True);
  CheckFile(ETemplateFrame, True);
  CheckFile(ETemplateDialog, True);
  CheckFile(ETemplateApplet, True);
  CheckFile(ETemplateJFrame, True);
  CheckFile(ETemplateJDialog, True);
  CheckFile(ETemplateJApplet, True);
  CheckFile(ETemplateApplication, True);
  CheckFile(ETemplateControlstructure, True);
  CheckFile(ETemplateClass, True);
  CheckFile(EMindstormsTemplate, True);
  CheckFile(ETemplateJUnitTest, True);
end;

procedure TFConfiguration.SetDefaultBracketColor;
begin
  FAttrBrackets.Foreground := clRed;
  FAttrBrackets.Background := 14737632;
  FAttrBrackets.IntegerStyle := 0;
end;

procedure TFConfiguration.DoHelp(const AFile: string);
begin
  FJava.CallHelp(AFile);
end;

var
  EnArr: array [0 .. MaxPages] of string = (
    'java',
    'interpreter',
    'compiler',
    'programs',
    'applets',
    'disassembler',
    'jar',
    'editor',
    'options',
    'code',
    'colors',
    'comment',
    'templates',
    'keyboard',
    'structogram',
    'sequencediagram',
    'browser',
    'documentation',
    'printer',
    'mindstorms',
    'android',
    'language',
    'options',
    'restrictions',
    'associations',
    'uml',
    'uml2',
    'llm_assistant',
    'llm_chat',
    'visibility',
    'protocols',
    'tools',
    'git',
    'junit',
    'checkstyle',
    'jalopy',
    'subversion'
  );
  DeArr: array [0 .. MaxPages] of string = (
    'java',
    'interpreter',
    'compiler',
    'programme',
    'applets',
    'disassembler',
    'jar',
    'editor',
    'optionen',
    'code',
    'farben',
    'kommentar',
    'vorlagen',
    'tastatur',
    'struktogramm',
    'sequenzdiagramm',
    'browser',
    'dokumentation',
    'drucker',
    'mindstorms',
    'android',
    'sprache',
    'optionen',
    'restriktionen',
    'verknüpfungen',
    'uml',
    'uml2',
    'llm_assistent',
    'llm_chat',
    'sichtbarkeit',
    'protokolle',
    'tools',
    'git',
    'junit',
    'checkstyle',
    'jalopy',
    'subversion'
  );

procedure TFConfiguration.BHelpClick(Sender: TObject);
var
  Count: Integer;
  ANode: TTreeNode;
begin
  with FJava do
  begin
    if PInterpreter.Visible then
      DoHelp(GetJavaTools('java.html'))
    else if PCompiler.Visible then
      DoHelp(GetJavaTools('javac.html'))
    else if PPrograms.Visible then
    begin
      DoHelp(GetJavaTools('java.html'));
      DoHelp(GetJavaTools('jdb.html'));
      DoHelp(GetJavaTools('JavaDoc.html'));
    end
    else if PApplets.Visible then
      DoHelp(GetJavaTools('appletviewer.html'))
    else if PDisassembler.Visible then
      DoHelp(GetJavaTools('javap.html'))
    else if PJar.Visible then
      DoHelp(GetJavaTools('jar.html'))
    else if PDocumentation.Visible then
      DoHelp(GetJavaTools('JavaDoc.html'))
    else if PMindstorms.Visible then
      DoHelp('https://lejos.sourceforge.io/');

    ANode := TVConfiguration.Items.GetFirstNode;
    Count := 0;
    while (Count < TVConfiguration.Items.Count) and
      (ANode <> TVConfiguration.Selected) do
    begin
      ANode := ANode.GetNext;
      Inc(Count);
    end;
    if IsGerman then
      DoHelp(GetConfigurationAddress(DeArr[Count]))
    else
      DoHelp(GetConfigurationAddress(EnArr[Count]));
  end;
end;

procedure TFConfiguration.BCheckClick(Sender: TObject);
begin
  CheckAllFilesAndFolders;
end;

function TFConfiguration.GetDumpText: string;
var
  Pathname1, Pathname2, Pathname3, Str: string;
  StringList, SL1: TStringList;
  Editor: TFEditForm;
begin
  Result := '';
  Pathname1 := FTempDir + 'Configuration.ini';
  Pathname2 := FTempDir + 'Configuration2.ini';
  Pathname3 := FTempDir + 'Configuration3.ini';
  Editor := TFEditForm(FJava.GetTDIWindow(Pathname1));
  if Assigned(Editor) then
  begin
    Editor.Save(False);
    Editor.Close;
    Application.ProcessMessages;
  end;
  try
    if FileExists(Pathname1) then
      DeleteFile(Pathname1);
    if FileExists(Pathname2) then
      DeleteFile(Pathname2);
    if FileExists(Pathname3) then
      DeleteFile(Pathname3);

    FDumpIniFileHKCU := TMemIniFile.Create(Pathname1);
    FDumpIniFileHKLM := TMemIniFile.Create(Pathname2);
    FDumpIniFileAllUsers := TMemIniFile.Create(Pathname3);
    FDumpMode := True;
    ModelToRegistry;
    FDumpMode := False;
    FDumpIniFileHKCU.UpdateFile;
    FDumpIniFileAllUsers.UpdateFile;
    FDumpIniFileHKLM.UpdateFile;

    Str := 'Installation ' + CrLf;
    Str := Str + '  JE-Version: ' + UDlgAbout.Version + ', ' +
      TFAbout.GetDate + CrLf;
    Str := Str + '  Java-Version: ' + IntToStr(GetJavaVersion) + CrLf;
    Str := Str + '  Windows-Version: ' + TOSVersion.ToString + CrLf;
    {$WARN SYMBOL_PLATFORM OFF}
    Str := Str + '  CmdLine: ' + CmdLine + CrLf;
    {$WARN SYMBOL_PLATFORM ON}
    StringList := TStringList.Create;
    SL1 := TStringList.Create;
    if FUseRegistry then
    begin
      Str := Str + CrLf;
      Str := Str + '  regedit: HKEY_LOCAL_MACHINE\SOFTWARE\JavaEditor ' + CrLf;
      Str := Str + '  regedit: HKEY_CURRENT_USER\SOFTWARE\JavaEditor ' + CrLf;
      Str := Str + StringOfChar('-', 80) + CrLf + CrLf;
      Str := Str + '--- HKEY_LOCAL_MACHINE\SOFTWARE\JavaEditor' + CrLf;
      FDumpIniFileHKLM.GetStrings(StringList);
      Str := Str + StringList.Text + CrLf;
      StringList.Clear;
      FDumpIniFileAllUsers.GetStrings(StringList);
      Str := Str + StringList.Text + CrLf;
      StringList.Clear;
      Str := Str + StringOfChar('-', 80) + CrLf + CrLf;
      Str := Str + '--- HKEY_CURRENT_USER\SOFTWARE\JavaEditor' + CrLf;
      FDumpIniFileHKCU.GetStrings(StringList);
      Str := Str + StringList.Text;
    end
    else
    begin
      Str := Str + CrLf;
      Str := Str + '  ' + FMachineIniFile.Filename + CrLf;
      if Assigned(FUserIniFile) then
        Str := Str + '  ' + FUserIniFile.Filename + CrLf;
      Str := Str + CrLf;
      Str := Str + StringOfChar('-', 80) + CrLf;
      Str := Str + '--- ' + FMachineIniFile.Filename + CrLf + CrLf;
      SL1.LoadFromFile(FMachineIniFile.Filename);
      Str := Str + SL1.Text + CrLf;
      Str := Str + StringOfChar('-', 80) + CrLf;
      if Assigned(FUserIniFile) then
      begin
        Str := Str + '--- ' + FUserIniFile.Filename + CrLf + CrLf;
        SL1.Clear;
        SL1.LoadFromFile(FUserIniFile.Filename);
        Str := Str + SL1.Text;
        Str := Str + StringOfChar('-', 80) + CrLf;
      end
      else
        Str := Str + 'missing ' + FUserIniFile.Filename + CrLf;
    end;
    FreeAndNil(StringList);
    FreeAndNil(SL1);
    Result := Str + CrLf;

    FreeAndNil(FDumpIniFileHKCU);
    FreeAndNil(FDumpIniFileAllUsers);
    FreeAndNil(FDumpIniFileHKLM);
  except
    on E: Exception do
      Log('Error in GetDumpText!', E);
  end;
end;

procedure TFConfiguration.BDumpClick(Sender: TObject);
begin
  var
  StringList := TStringList.Create;
  StringList.Text := GetDumpText;
  var
  Pathname := TPath.Combine(FTempDir, 'Configuration.ini');
  StringList.SaveToFile(Pathname);
  FreeAndNil(StringList);
  FJava.Open(Pathname);
end;

procedure TFConfiguration.LMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TFConfiguration.LMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TFConfiguration.BCheckstyleInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := GetTargetDir('Checkstyle');
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('Checkstyle');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'],
            Source + StringList.Values['zip2']);
          ShowModal;
          if DownloadIsOK then
          begin
            DownloadFile(StringList.Values['file2'],
              Source + '\mycheckstyle.xml');
            if VistaOrBetter then
              CallUpdater(Target, Source + StringList.Values['zip2'],
                Source + '\mycheckstyle.xml')
            else
            begin
              ExtractZipToDir(Source + StringList.Values['zip2'], Target);
              CopyFile(PChar(Source + '\mycheckstyle.xml'),
                PChar(Target + '\mycheckstyle.xml'), False);
            end;
          end;
          ShortenPath(ECheckstyle, Target + StringList.Values['file1']);
          ShortenPath(ECheckstyleConfiguration, Target + '\mycheckstyle.xml');
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(ECheckstyle, True);
  CheckFile(ECheckstyleConfiguration, True);
end;

procedure TFConfiguration.SBCheckStyleSelectClick(Sender: TObject);
begin
  FileSelect(ECheckstyle, 'jar|*.jar', 'Checkstyle-all*.jar', 'Checkstyle');
  if ECheckstyleConfiguration.Hint = '' then
  begin
    var
    Str := ExtractFilePath(ECheckstyle.Hint) + 'mycheckstyle.xml';
    ShortenPath(ECheckstyleConfiguration, Str);
  end;
  CheckFile(ECheckstyle, True);
  CheckFile(ECheckstyleConfiguration, True);
end;

procedure TFConfiguration.BCheckstyleConfigurationClick(Sender: TObject);
begin
  FileSelect(ECheckstyleConfiguration, 'xml|*.xml', '*.xml', 'Checkstyle');
  CheckFile(ECheckstyleConfiguration, True);
end;

procedure TFConfiguration.LJavaInterpreterClick(Sender: TObject);
begin
  DoHelp('https://www.oracle.com/java/');
end;

procedure TFConfiguration.LInterpreterParameterClick(Sender: TObject);
begin
  DoHelp(GetJavaTools('java.html'));
end;

procedure TFConfiguration.LJUnitClick(Sender: TObject);
begin
  FJava.CallHelp('https://junit.org/junit5/');
end;

procedure TFConfiguration.LJavaCompilerClick(Sender: TObject);
begin
  DoHelp('https://www.oracle.com/java/');
end;

procedure TFConfiguration.LJavaCompilerParameterClick(Sender: TObject);
begin
  DoHelp(GetJavaTools('javac.html'));
end;

procedure TFConfiguration.LGitFolderClick(Sender: TObject);
begin
  DoHelp('https://git-scm.com/');
end;

procedure TFConfiguration.LAppletviewerClick(Sender: TObject);
begin
  DoHelp(GetJavaTools('appletviewer.html'));
end;

procedure TFConfiguration.LDebuggerClick(Sender: TObject);
begin
  DoHelp(GetJavaTools('jdb.html'));
end;

procedure TFConfiguration.LDisassemblerClick(Sender: TObject);
begin
  DoHelp(GetJavaTools('javap.html'));
  FJava.CallHelp('https://java-decompiler.github.io/');
end;

procedure TFConfiguration.LJavaDocClick(Sender: TObject);
begin
  DoHelp(GetJavaTools('JavaDoc.html'));
end;

procedure TFConfiguration.LCheckstyleClick(Sender: TObject);
begin
  FJava.CallHelp('https://Checkstyle.sourceforge.io/');
end;

procedure TFConfiguration.LJarClick(Sender: TObject);
begin
  DoHelp(GetJavaTools('jar.html'));
end;

procedure TFConfiguration.LLejosClick(Sender: TObject);
begin
  FJava.CallHelp('https://lejos.sourceforge.io/');
end;

procedure TFConfiguration.BMindstormsParameterClick(Sender: TObject);
begin
  if RGMindstormsVersion.ItemIndex = 0 then
    EMindstormsParameter.Text := '-Target 1.1 -source 1.2'
  else
    EMindstormsParameter.Text := '';
end;

procedure TFConfiguration.LJavabookClick(Sender: TObject);
begin
  DoHelp('https://www.javabuch.de/');
end;

procedure TFConfiguration.BCacheClick(Sender: TObject);
begin
  if FPortableApplication then
    ECache.Text := FEditorFolder + 'App\Cache'
  else
    ECache.Text := TPath.GetHomePath + '\JavaEditor\Cache';
  SysUtils.ForceDirectories(ECache.Text);
  ShortenPath(ECache, ECache.Text);
  CheckFolder(ECache, False);
end;

procedure TFConfiguration.SBAndroidSDKFolderClick(Sender: TObject);
begin
  var
  Dir := EAndroidSDKFolder.Hint;
  if not SysUtils.DirectoryExists(Dir) then
    Dir := GetEnvironmentVariable('PROGRAMFILES');
  FolderDialog.DefaultFolder := Dir;
  if FolderDialog.Execute then
  begin
    Dir := ExcludeTrailingPathDelimiter(FolderDialog.Filename);
    EAndroidSDKFolder.Text := Dir;
    CheckAllFilesAndFolders;
  end;
end;

procedure TFConfiguration.SBCacheSelectClick(Sender: TObject);
var
  Path: string;
begin
  if FPortableApplication then
    Path := FEditorFolder + 'App\Cache'
  else
    Path := TPath.GetHomePath + '\JavaEditor\Cache';
  Path := FolderSelect(ECache, Path);
  ForceDirectories(Path);
end;

procedure TFConfiguration.CallUpdater(const Target, Source1: string;
  Source2: string);
begin
  if Source2 = '' then
    Source2 := 'xxx';

  var
  Updater := FEditorFolder + 'setup.exe';
  var
  Str := '-Update ' + HideBlanks(Target) + ' ' + HideBlanks(Source1) + ' ' +
    HideBlanks(Source2);
  if not FUseRegistry then
    Str := Str + ' -INI ' + HideBlanks(FMachineIniFile.Filename);
  if FileExists(Updater) then
    try
      RunAsAdmin(Handle, Updater, Str);
    except
      on E: Exception do
        ErrorMsg(_('Can not execute file ') + Updater + '! Error: ' +
          E.Message);
    end
  else
    ErrorMsg(Format(_(LNGFileNotFound), [Updater]));
end;

function TFConfiguration.GetConfigurationAddress(const Str: string): string;
begin
  if IsGerman then
    Result := Homepage + '/doku.php?id=de:konfiguration#' + Str
  else
    Result := Homepage + '/doku.php?id=en:configuration#' + Str;
end;

procedure TFConfiguration.LTutorialClick(Sender: TObject);
begin
  DoHelp(GetConfigurationAddress('ducumentation'));
end;

procedure TFConfiguration.DecideProxy;
var
  Posi: Integer;
  AEnabled: Boolean;
  Str: string;
begin
  FWithProxy := False;
  FProxyPort := 0;
  FProxyIP := '';
  with FMyRegistry do
  begin
    RootKey := HKEY_CURRENT_USER;
    Access := KEY_READ;
    try
      if OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings',
        False) then
      begin
        AEnabled := ReadBool('ProxyEnable');
        FProxyIP := ReadString('ProxyServer');
        Posi := Pos(':', FProxyIP);
        if Posi > 0 then
        begin
          Str := Copy(FProxyIP, Posi + 1, Length(FProxyIP));
          if Str <> '' then
            FProxyPort := StrToInt(Str)
          else
            FProxyPort := 0;
          Delete(FProxyIP, Posi, Length(FProxyIP));
        end;
        FWithProxy := AEnabled and (FProxyIP <> '') and (FProxyPort <> 0);
      end;
    except
      on E: Exception do
        Log('Registry error!', E);
    end;
  end;
end;

function TFConfiguration.JavaDocComment(const Indent: string = ''): string;
begin
  Result := Indent + '/**' + CrLf + Indent + ' *' + CrLf + Indent + ' * ' +
    _('Description') + CrLf + Indent + ' *' + CrLf + Indent +
    ' * @version 1.0 ' + _('from') + ' %DATE%' + CrLf + Indent +
    ' * @author %AUTHOR%' + CrLf + Indent + ' */' + CrLf;
end;

function TFConfiguration.ShortComment(const Indent: string = ''): string;
begin
  Result := Indent + '// Author: %AUTHOR%' + CrLf + '// Date: %DATE%' + CrLf;
end;

function TFConfiguration.HeadText(const Indent: string = ''): string;
var
  Str: string;
  Posi: Integer;
begin
  case FCommentKind of
    0:
      Str := JavaDocComment(Indent);
    1:
      Str := ShortComment(Indent);
    2:
      Str := FFreeComment;
  end;
  Posi := Pos('%AUTHOR%', UpperCase(Str));
  if Posi > 0 then
    Str := ReplaceStr(Str, '%AUTHOR%', FJavaAuthor);
  Posi := Pos('%DATE%', UpperCase(Str));
  if Posi > 0 then
    Str := ReplaceStr(Str, '%DATE%', DateToStr(Date));
  Result := Str;
end;

procedure TFConfiguration.BJalopyInstallClick(Sender: TObject);
var
  Target, Source: string;
  StringList: TStringList;
begin
  Target := GetTargetDir('Jalopy');
  Source := FTempDir + 'javaeditor';
  if UpdatePossible(Source, Target) then
  begin
    with TFDownload.Create(Self) do
      try
        StringList := GetDownloadFiles('Jalopy');
        if StringList.Text <> '' then
        begin
          SetUrlAndFile(StringList.Values['zip1'], Source + '\Jalopy.zip');
          ShowModal;
          if DownloadIsOK then
          begin
            DownloadFile(StringList.Values['file2'], Source + '\myjalopy.xml');
            if VistaOrBetter then
              CallUpdater(Target, Source + '\Jalopy.zip',
                Source + '\myjalopy.xml')
            else
            begin
              ExtractZipToDir(Source + '\Jalopy.zip', Target);
              CopyFile(PChar(Source + '\myjalopy.xml'),
                PChar(Target + '\myjalopy.xml'), False);
            end;
            ShortenPath(EJalopy, Target + StringList.Values['file1']);
            ShortenPath(EJalopyConfiguration, Target + '\myjalopy.xml');
          end;
        end
        else
          ErrorMsg(_(LNGNoInternetConnection));
      finally
        FreeAndNil(StringList);
        Free;
      end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));

  Sleep(800);
  CheckFile(EJalopy, True);
  CheckFile(EJalopyConfiguration, True);
end;

procedure TFConfiguration.SBJalopySelectClick(Sender: TObject);
begin
  FileSelect(EJalopy, 'jar|*.jar', 'Jalopy-console.jar', 'Jalopy');
  CheckFile(EJalopy, True);
end;

procedure TFConfiguration.BJalopyConfigurationClick(Sender: TObject);
begin
  FileSelect(EJalopyConfiguration, 'xml|*.xml', '*.xml', 'Jalopy');
  CheckFile(EJalopyConfiguration, True);
end;

procedure TFConfiguration.LJalopyClick(Sender: TObject);
begin
  DoHelp('http://Jalopy.sourceforge.net/');
end;

function TFConfiguration.AddPortableDrives(Str: string): string;
begin
  Result := Str;
  // if not FPortableApplication then exit;
  var
  E := '';
  var
  Posi := Pos(';', Str);
  while Posi > 0 do
  begin
    E := E + AddPortableDrive(Copy(Str, 1, Posi));
    Delete(Str, 1, Posi);
    Posi := Pos(';', Str);
  end;
  E := E + AddPortableDrive(Str);
  Result := E;
end;

function TFConfiguration.RemovePortableDrives(Str: string): string;
begin
  Result := Str;
  if not FPortableApplication then
    Exit;
  var
  E := '';
  var
  Posi := Pos(';', Str);
  while Posi > 0 do
  begin
    E := E + RemovePortableDrive(Copy(Str, 1, Posi));
    Delete(Str, 1, Posi);
    Posi := Pos(';', Str);
  end;
  E := E + RemovePortableDrive(Str);
  Result := E;
end;

function TFConfiguration.RemovePortableDrive(const Str: string;
  const Folder: string = ''): string;
// if Folder is set then switch to relative paths, used in Store/FetchDiagram
var
  SL1, SL2: TStringList;
  Int, JNum: Integer;
  Basefolder: string;
begin
  Result := Str;
  if FPortableApplication then
    Basefolder := FEditorFolder
  else
    Basefolder := Folder;
  // same drive
  if TPath.IsDriveRooted(Basefolder) and
    (UpperCase(Copy(Basefolder, 1, 2)) = UpperCase(Copy(Str, 1, 2))) then
  begin
    SL1 := Split('\', ExcludeTrailingPathDelimiter(Basefolder));
    SL2 := Split('\', ExcludeTrailingPathDelimiter(Str));
    Int := 0;
    while (Int < SL1.Count) and (Int < SL2.Count) and
      (UpperCase(SL1[Int]) = UpperCase(SL2[Int])) do
      Inc(Int);
    Result := '';
    JNum := Int;
    while Int < SL1.Count do
    begin
      Result := Result + '..\';
      Inc(Int);
    end;
    while JNum < SL2.Count do
    begin
      Result := Result + SL2[JNum] + '\';
      Inc(JNum);
    end;
    Result := ExcludeTrailingPathDelimiter(Result);
    FreeAndNil(SL1);
    FreeAndNil(SL2);
  end;
end;

function TFConfiguration.AddPortableDrive(const Str: string;
  const Folder: string = ''): string;
// if Folder is set then switch to relative paths, used in Store/FetchDiagram
var
  SL1, SL2: TStringList;
  Int: Integer;
  Basefolder: string;
begin
  Result := Str;
  if Str.StartsWith('its:') or (Str = '') or (Copy(Str, 2, 2) = ':\') or
    (Copy(Str, 1, 7) = 'file://') or // absolute path
    (Pos('\\', Str) = 1) or (Pos('://', Str) > 1) then
    // UNC or https:// or http://
    Exit;

  if FPortableApplication then
    Basefolder := FEditorFolder
  else
    Basefolder := Folder;
  if Copy(Str, 1, 2) = ':\' then // old portable concept
    if Length(FPortAppDrive) = 1 then
      Result := FPortAppDrive + Str
    else
      Result := FPortAppDrive + Copy(Str, 2, Length(Str))
  else if Basefolder <> '' then
  begin // new portaple concept, relative to Basefolder
    SL1 := Split('\', ExcludeTrailingPathDelimiter(Basefolder));
    SL2 := Split('\', ExcludeTrailingPathDelimiter(Str));
    Int := 0;
    while (Int < SL2.Count) and (SL2[Int] = '..') do
      Inc(Int);
    Result := SL1[0];
    for var J := 1 to SL1.Count - 1 - Int do
      Result := Result + '\' + SL1[J];
    for var J := Int to SL2.Count - 1 do
      Result := Result + '\' + SL2[J];
    FreeAndNil(SL1);
    FreeAndNil(SL2);
  end;
end;

function TFConfiguration.GetFileInCHM(Str: string): string;
begin
  var
  Posi := Pos('.CHM', UpperCase(Str));
  Delete(Str, 1, Posi + 3);
  if Copy(Str, 1, 2) = '::' then
    Delete(Str, 1, 2);
  Result := Str;
end;

function TFConfiguration.GetCHMJavaManual(const Str: string): string;
begin
  var
  Str1 := GetFileInCHM(Str);
  if Str1 = '' then
  begin
    Str1 := ExtractFileName(FJavaManual);
    Str1 := ChangeFileExt(Str1, '');
    Str1 := '\' + Str1 + '\index.html';
  end;
  Result := 'its:' + FJavaManual + '::' + Str1;
end;

function TFConfiguration.GetJavaManual: string;
begin
  if IsCHM(FJavaManual) and FileExists(FJavaManual) then
    Result := FJavaManual + FJavaCHMRoot
  else if IsHTTP(FJavaManual) then
    Result := ExcludeTrailingPathDelimiter(ExtractFilePathEx(FJavaManual))
  else
    Result := ExcludeTrailingPathDelimiter(ExtractFilePathEx(FJavaManual));
end;

function TFConfiguration.GetJavaManualFX: string;
begin
  Result := ExtractFilePathEx(FJavaManualFX);
end;

procedure TFConfiguration.RGMindstormsVersionClick(Sender: TObject);
begin
  var
  Str := EMindstormsParameter.Text;
  if (Str = '-Target 1.1 -source 1.2') or (Str = '-Target 1.1 -source 1.3') then
    BMindstormsParameterClick(Self);
  SetMindstormsVersion;
end;

procedure TFConfiguration.SetMindstormsVersion;
begin
  var
  Version := RGMindstormsVersion.ItemIndex;
  LLejosCompiler.Visible := (Version < 2);
  ELejosCompiler.Visible := (Version < 2);
  SBLejosCompiler.Visible := (Version < 2);
  LUploader.Visible := (Version < 2);
  ELejosUploader.Visible := (Version < 2);
  SBLejosUploader.Visible := (Version < 2);
  LFlasher.Visible := (Version < 2);
  ELejosFlasher.Visible := (Version < 2);
  SBLejosFlasher.Visible := (Version < 2);
  BMindstormsParameter.Visible := (Version < 2);

  case Version of
    0:
      begin
        ELejosUploader.Enabled := False;
        LMindstormsPort.Visible := True;
        CBMindstormsPort.Visible := True;
        LMindstormsIP.Visible := False;
        EMindstormsIP.Visible := False;
        FLejosVerzeichnis := FRCXFolder;
        FLejosCompiler := FRCXCompiler;
        FLejosUploader := FRCXUploader;
        FLejosFlasher := FRCXFlasher;
        FMindstormsManual := FRCXManual;
      end;
    1:
      begin
        ELejosCompiler.Enabled := not FLockedPaths;
        ELejosUploader.Enabled := not FLockedPaths;
        ELejosFlasher.Enabled := not FLockedPaths;
        LMindstormsPort.Visible := True;
        CBMindstormsPort.Visible := True;
        LMindstormsIP.Visible := False;
        EMindstormsIP.Visible := False;
        FLejosVerzeichnis := FNXTFolder;
        FLejosCompiler := FNXTCompiler;
        FLejosUploader := FNXTUploader;
        FLejosFlasher := FNXTFlasher;
        FMindstormsManual := FNXTManual;
      end;
    2:
      begin
        LMindstormsPort.Visible := False;
        CBMindstormsPort.Visible := False;
        LMindstormsIP.Visible := True;
        LMindstormsIP.Top := 244;
        EMindstormsIP.Visible := True;
        EMindstormsIP.Top := 240;
        FLejosVerzeichnis := FEV3Folder;
        FMindstormsManual := FEV3Manual;
      end;
  end;
  ShortenPath(ELejosFolder, FLejosVerzeichnis);
  ShortenPath(ELejosCompiler, FLejosCompiler);
  ShortenPath(ELejosUploader, FLejosUploader);
  ShortenPath(ELejosFlasher, FLejosFlasher);
  ShortenPath(EMindstormsManual, FMindstormsManual);
  CheckMindstorms;
end;

procedure TFConfiguration.CheckMindstorms;
begin
  CheckFolder(ELejosFolder, True);
  CheckFile(ELejosCompiler, True);
  CheckFile(ELejosUploader, True);
  CheckFile(ELejosFlasher, True);
  CheckFile(EMindstormsManual, True);
  CheckFile(EMindstormsTemplate, True);
end;

procedure TFConfiguration.SaveInis;
begin
  if not FUseRegistry then
    if not FWriteProtection and Assigned(FMachineIniFile) then
      try
        FMachineIniFile.UpdateFile;
      except
        on E: Exception do
          ErrorMsg(Format(_(LNGCanNotCreateFile), [FMachineIniFile.Filename,
            E.Message]));
      end;
  if Assigned(FUserIniFile) then
    try
      FUserIniFile.UpdateFile;
    except
      on E: Exception do
        ErrorMsg(Format(_(LNGCanNotCreateFile), [FUserIniFile.Filename,
          E.Message]));
    end;
end;

procedure TFConfiguration.BSVNClick(Sender: TObject);
begin
  var
  Dir := ESVNFolder.Hint;
  if not SysUtils.DirectoryExists(Dir) then
    Dir := FSourcepath;
  FolderDialog.DefaultFolder := Dir;
  if FolderDialog.Execute then
    ShortenPath(ESVNFolder, FolderDialog.Filename);
  CheckFolder(ESVNFolder, True);
end;

procedure TFConfiguration.BRepositoryClick(Sender: TObject);
begin
  var
  Dir := CBRepository.Hint;
  if not SysUtils.DirectoryExists(Dir) then
    Dir := FSourcepath;
  FolderDialog.DefaultFolder := Dir;
  if FolderDialog.Execute then
  begin
    Dir := FolderDialog.Filename;
    ShortenPath(CBRepository, Dir);
    SysUtils.ForceDirectories(Dir);
    if not FSubversion.IsRepository(Dir) then
      FSubversion.CallSVN('\svnadmin.exe', 'create ' + HideBlanks(Dir), Dir);
  end;
  CheckFolderCB(CBRepository);
end;

procedure TFConfiguration.LSVNClick(Sender: TObject);
begin
  DoHelp('https://subversion.apache.org/');
end;

procedure TFConfiguration.SBOpenClick(Sender: TObject);
begin
  var
  Str := '';
  case TButton(Sender).Tag of
    4:
      Str := EMindstormsManual.Hint;
    5:
      Str := EMindstormsTemplate.Hint;
    12:
      Str := ECache.Hint;
    17:
      Str := EKeyboardFile.Hint;
  end;
  if FileExists(Str) then
    if Pos('.html', Str) > 0 then
      DoHelp(Str)
    else
      FJava.Open(Str)
  else if SysUtils.DirectoryExists(Str) then
    FJava.NewExplorer(Str, '');
end;

function TFConfiguration.SetRCXNXTEV3(const Str: string): string;
begin
  case FMindstormsVersion of
    0:
      Result := Str;
    1:
      Result := AnsiReplaceStr(Str, 'RCX', 'NXJ');
    2:
      Result := AnsiReplaceStr(Str, 'RCX', 'EV3');
  end;
end;

function TFConfiguration.GetJavaVersion: Integer;
var
  Version: string;
  Lines: TArray<String>;
begin
  if FJavaVersion = 0 then
  begin
    FJavaVersion := 6;
    try
      if FileExists(FJDKFolder + '\release') then
      begin // since 1.7.0
        // Format: JAVA_VERSION="9.0.1"
        Lines:= TFile.ReadAllLines(FJDKFolder + '\release', TEncoding.UTF8);
        for var Line in Lines do
          if Line.StartsWith('JAVA_VERSION=') then begin
            Version := Copy(Line, Length('JAVA_VERSION=') + 1, MaxInt);
            Break;
          end;
      end
      else if MyJavaCommands.ExecAndWait(FJavaInterpreter, '-version', '.',
        FTempDir + 'version.txt', SW_HIDE, False) then
      begin
        // Format: java version "1.5.4-rc"
        Lines:= TFile.ReadAllLines(FTempDir + 'version.txt', TEncoding.UTF8);
        Version := Copy(Lines[0], Length('java version ') + 1,  MaxInt);
      end;
      // Format: "1.5.4-rc", "1.7.0", "1.8.0", "9.0.1", "11.0.1", "21.0.1", "25"
      Version:= Version.Trim(['"']);
      var Posi := Pos('.', Version);
      if (Posi = 2) and (Version[1] = '1') then
        Version := Copy(Version, 3, 1)
      else if Posi > 0 then
        Version := Copy(Version, 1, Posi - 1);
      if not TryStrToInt(Version, FJavaVersion) then
        FJavaVersion := 6;
    except
      on E: Exception do
        Log('Get java version', E);
    end;
  end;
  Result := FJavaVersion;
end;

function TFConfiguration.GetDocumentationVersion: Integer;
var
  IndexFile: string;
begin
  if FDocumentationVersion = 0 then
  begin
    FDocumentationVersion := 7;
    IndexFile := GetJavaManual + '\index.html';
    if GlobalFileExists(IndexFile, False) then
    begin
      var
      StringList := TStringList.Create;
      try
        try
          StringList.Text := MyCodeCompletion.LoadTextFromFile(IndexFile);
          for var I := 9 to 50 do
            if Pos('Java SE ' + IntToStr(I), StringList.Text) > 0 then
            begin
              FDocumentationVersion := I;
              Break;
            end;
          if FDocumentationVersion = 7 then
            if Pos('Standard Edition 8', StringList.Text) > 0 then
              FDocumentationVersion := 8
            else if Pos('Standard Edition 7', StringList.Text) > 0 then
              FDocumentationVersion := 7
            else if Pos('JDK 6 Documentation', StringList.Text) > 0 then
              FDocumentationVersion := 6
            else if Pos('JDK 5 Documentation', StringList.Text) > 0 then
              FDocumentationVersion := 5;
        except
          on E: Exception do
            Log('GetDocumentationVersion', E);
        end;
      finally
        FreeAndNil(StringList);
      end;
    end;
  end;
  Result := FDocumentationVersion;
end;

procedure TFConfiguration.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  FJava.ActiveTool := -1;
end;

procedure TFConfiguration.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  SaveInis;
  CanClose := True;
end;

procedure TFConfiguration.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FolderDialog);
  FreeAndNil(FODSelect);
  FreeAndNil(FJavaHighlighter);
  FreeAndNil(FHTMLHighlighter);

  FreeAndNil(FEditFont);
  FreeAndNil(FUMLFont);
  FreeAndNil(FStructogramFont);
  FreeAndNil(FSequenceFont);
  FreeAndNil(FAttrBrackets);

  FreeAndNil(FMyRegistry);
  FreeAndNil(FMachineIniFile);
  FreeAndNil(FUserIniFile);
  FreeAndNil(FLNGTVItems);
  FreeAndNil(FKeyboardShortcutsTree);
  FreeAndNil(FEditorAndMenuShortcuts);

  FreeAndNil(FAllClasses);
  FreeAndNil(FAllInterfaces);
  FreeAndNil(FAllPackages);
  FreeAndNil(FAllClasspathClasses);
  FreeAndNil(FMenuKeys);
  FreeAndNil(FEditorKeys);
  FreeAndNil(FAllKeys);
  FreeAndNil(FImportCache);
  FreeAndNil(FExternalStyleFilesDict);
  FreeAndNil(FPreview);
  FreeAndNil(FLoadedStylesDict);

  for var I := 1 to 21 do
    FreeAndNil(FControlStructureTemplates[I]);
  FConfiguration := nil;
  FreeAndNil(FAllDocumentations);
  FreeAndNil(FLanguagesList);
end;

procedure TFConfiguration.FormShow(Sender: TObject);
begin
  FJava.ActiveTool := 15;
end;

procedure TFConfiguration.BFontClick(Sender: TObject);
begin
  FScpHint.MIFontClick(Self);
end;

procedure TFConfiguration.BGitFolderClick(Sender: TObject);
begin
  var
  Dir := EGitFolder.Hint;
  if not SysUtils.DirectoryExists(Dir) then
    Dir := FSourcepath;
  FolderDialog.DefaultFolder := Dir;
  if FolderDialog.Execute then
    ShortenPath(EGitFolder, FolderDialog.Filename);
  CheckFolder(EGitFolder, True);
end;

procedure TFConfiguration.BGitCloneClick(Sender: TObject);
var
  Dir, Remote, AName: string;
begin
  Screen.Cursor := crHourGlass;
  try
    Dir := CBLocalRepository.Text;
    Remote := CBRemoteRepository.Text;
    if not SysUtils.DirectoryExists(Dir) then
      SysUtils.ForceDirectories(Dir);
    FGit.GitCall('clone ' + Remote + ' ', Dir);
    AName := ExtractFileNameEx(Remote);
    AName := ChangeFileExt(AName, '');
    if not EndsWith(Dir, '\' + AName) then
      CBLocalRepository.Text := Dir + '\' + AName;
    if CBRemoteRepository.Items.IndexOf(Remote) = -1 then
      CBRemoteRepository.Items.Insert(0, Remote);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFConfiguration.BGitRepositoryClick(Sender: TObject);
begin
  var
  Dir := CBLocalRepository.Text;
  if not SysUtils.DirectoryExists(Dir) then
    Dir := FSourcepath;
  FolderDialog.DefaultFolder := Dir;
  if FolderDialog.Execute then
  begin
    Dir := FolderDialog.Filename;
    SysUtils.ForceDirectories(Dir);
    if not FGit.IsRepository(Dir) then
      FGit.GitCall('init', Dir);
    if CBLocalRepository.Items.IndexOf(Dir) = -1 then
      CBLocalRepository.Items.Insert(0, Dir);
    CBLocalRepository.Text := Dir;
  end;
  CheckFolderCB(CBLocalRepository);
end;

procedure TFConfiguration.TVConfigurationChange(Sender: TObject;
  Node: TTreeNode);
var
  ANode: TTreeNode;
  Count: Integer;
begin
  ANode := TVConfiguration.Items.GetFirstNode;
  Count := 0;
  while (Count < TVConfiguration.Items.Count) and (ANode <> Node) do
  begin
    ANode := ANode.GetNext;
    Inc(Count);
  end;
  if ANode.HasChildren then
    Inc(Count);

  ShowPage(Count);
  CheckAllFilesAndFolders;
  if PCode.Visible then
    if CBManual.Color = clRed then
    begin
      LCompletionHint1.Show;
      LCompletionHint2.Show;
    end
    else
    begin
      LCompletionHint1.Hide;
      LCompletionHint2.Hide;
    end;
  if PMindstorms.Visible then
    SetMindstormsVersion;
end;

function TFConfiguration.GetTVConfigurationItem(const Text: string): TTreeNode;
begin
  for var I := 0 to FLNGTVItems.Count - 1 do
    if FLNGTVItems[I] = Text then
      Exit(TVConfiguration.Items[I]);
  Result := TVConfiguration.Items[0];
end;

procedure TFConfiguration.ShowPage(Page: Integer);
begin
  PageList.ActivePageIndex := Page;
  TVConfiguration.Selected := TVConfiguration.Items[Page];
  LTitle.Caption := TVConfiguration.Items[Page].Text;
end;

procedure TFConfiguration.RGColorsClick(Sender: TObject);
begin
  ShowColorElements;
end;

procedure TFConfiguration.PrepareShow;
begin
  if LVVisibilityTabs.Items.Count = 0 then
    PrepareVisibilityPage;
  ModelToView;
  if Assigned(FJava.EditorForm) then
    if FJava.EditorForm.IsJava then
      RGColors.ItemIndex := 0
    else if FJava.EditorForm.IsHTML then
      RGColors.ItemIndex := 1;
  ShowColorElements;
  CheckAllFilesAndFolders;
  MenuAndEditorShortcuts;
end;

function TFConfiguration.GetCharset: string;
begin
  Result := 'UTF-8';
end;

function TFConfiguration.GetEncoding: string;
begin
  Result := 'UTF-8';
end;

function TFConfiguration.GetEncoding(const Pathname: string): TEncoding;
var
  Stream: TStream;
  EditForm: TFEditForm;
  WithBOM: Boolean;
begin
  Result := TEncoding.ANSI;
  EditForm := TFEditForm(FJava.GetTDIWindowType(Pathname, '%E%'));
  if Assigned(EditForm) then
    Result := EditForm.Editor.Lines.Encoding
  else if FileExists(Pathname) then
    try
      Stream := TFileStream.Create(Pathname, fmOpenRead or fmShareDenyWrite);
      try
        Result := SynUnicode.GetEncoding(Stream, WithBOM);
      finally
        FreeAndNil(Stream);
      end;
    except
      on E: Exception do
        Log('GetEncoding: ' + Pathname + '#', E);
    end;
end;

function TFConfiguration.GetFileFilters: string;
var
  Str: string;
begin
  if FFileFilter <> '' then
    Str := ';' + FFileFilter
  else
    Str := '';
  Result := 'Java-Editor|*.java;*.jfm;*.uml;*.jsg;*.html;*.htm;*.jep;*.jsp;*.jsd;*.txt;*.jar'
    + Str + '|Java (*.java)|*.java|' + _('Form') +
    ' (*.jfm)|*.jfm|UML (*.uml)|*.uml' + '|' + _(LNGStructogram) +
    ' (*.jsg)|*.jsg' + '|' + _(LNGSequenceDiagram) + ' (*.jsd)|*.jsd' +
    '|HTML (*.html)|*.html;*.htm' + '|XML (*.xml)|*.xml|' + _(LNGProject) +
    ' (*.jep)|*.jep|' + LNGText + ' (*.txt)|*.txt' + '|Jar-' + _(LNGFile) +
    ' (*.jar)|*.jar|class(*.class)|*.class|JSP (*.jsp)|*.jsp' + '|' +
    _('Backup') + ' (*.~ava)|*.~ava|' + _('All files') + ' (*.*)|*.*';
end;

function TFConfiguration.IsGerman: Boolean;
begin
  Result := (FLanguageCode = 'de_DE');
end;

function TFConfiguration.GetClassPath: string;
begin
  var
  Str := FJavaClasspathAdmin;
  if FJavaClasspathUser <> '' then
    Str := FJavaClasspathUser + ';' + Str;
  Str := FJava.ExtentClasspath(Str);
  while CharInSet(Str[Length(Str)], ['\', ';']) do
    Delete(Str, Length(Str), 1);
  Result := HideBlanks(Str);
end;

function TFConfiguration.GetClassPath(const Pathname, Package: string): string;
begin
  var
  Str := FJavaClasspathAdmin;
  if FJavaClasspathUser <> '' then
    Str := FJavaClasspathUser + ';' + Str;
  if FJUnitJarFile <> '' then
    Str := FJUnitJarFile + ';' + Str;
  Str := GetPackageDirectorySecure(Pathname, Package) + ';' + Str;
  while CharInSet(Str[Length(Str)], ['\', ';']) do
    Delete(Str, Length(Str), 1);
  Result := HideBlanks(Str);
end;

function TFConfiguration.GetClassPathJarExpanded(const Pathname,
  Package: string): string;
var
  Cp1, Cp2: string;
  Posi: Integer;
begin
  Result := GetClassPath(Pathname, Package);
  if Pos('*', Result) = 0 then
    Exit;
  Cp1 := UnHideBlanks(Result) + ';';
  Result := '';
  Posi := Pos(';', Cp1);
  while Posi > 0 do
  begin
    Cp2 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    if EndsWith(Cp2, '*') then
    begin
      Delete(Cp2, Length(Cp2), 1);
      if DirectoryExists(Cp2) then begin
        var
        Filenames := TDirectory.GetFiles(Cp2, '*.jar');
        for var Filename in Filenames do
          Result := Result + ';' + Filename;
      end;
    end
    else
      Result := Result + ';' + Cp2;
    Posi := Pos(';', Cp1);
  end;
  Delete(Result, 1, 1);
end;

function TFConfiguration.GetPackageDirectorySecure(const Pathname: string;
  Package: string): string;
begin
  var
  Directory := ExtractFilePath(Pathname);
  if Package <> '' then
  begin
    Package := ReplaceStr(Package, '.', '\') + '\';
    if EndsWith(Directory, Package) then
      Delete(Directory, Length(Directory) - Length(Package), Length(Package));
  end;
  Result := Directory;
end;

function TFConfiguration.GetPackageDirectoryRelativ(const Pathname,
  Package: string): string;
begin
  var
  Directory := ExtractFilePath(Pathname);
  if Package <> '' then
  begin
    var
    Num := CountChar('.', Package) + 1;
    while Num > 0 do
    begin
      Directory := Copy(Directory, 1, Length(Directory) - 1);
      Directory := Copy(Directory, 1, LastDelimiter('\', Directory));
      Dec(Num);
    end;
  end;
  Result := Directory;
end;

function TFConfiguration.GetHighlighter(const Pathname: string)
  : TSynCustomHighlighter;
begin
  var
  FileExtension := LowerCase(ExtractFileExt(Pathname));
  if FileExtension = '' then
    Result := nil
  else if (FileExtension = '.java') or (FileExtension = '.~ava') then
    Result := FJavaHighlighter
  else if (FileExtension = '.html') or (FileExtension = '.htm') or
    (FileExtension = '.xml') then
    Result := FHTMLHighlighter
  else if FileExtension = '.pas' then
    Result := GetPascalHighlighter
  else if FileExtension = '.jsp' then
    Result := GetMultiSynHighlighter
  else if (FileExtension = '.php') or (FileExtension = '.inc') then
    Result := GetPhpHighlighter
  else if FileExtension = '.css' then
    Result := GetCSSHighlighter
  else
    Result := GetGeneralHighlighter;
end;

function TFConfiguration.GetClasspathFromSourcepath(const AClassname,
  Sourcepath: string): string;
var
  Cp1, Cp2: string;
  Posi: Integer;
begin
  Result := '';
  if Sourcepath = '' then
    Exit;
  Cp1 := UnHideBlanks(GetClassPath) + ';';
  Cp1 := ReplaceStr(Cp1, '.;', Sourcepath + ';');
  if Pos(Sourcepath, Cp1) = 0 then
    Cp1 := Sourcepath + ';' + Cp1;

  Posi := Pos(';', Cp1);
  while Posi > 0 do
  begin
    Cp2 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    Cp2 := ExcludeTrailingPathDelimiter(Cp2);
    if EndsWith(Cp2, '*') then
      Cp2 := Copy(Cp2, 1, Length(Cp2) - 2);

    if SysUtils.DirectoryExists(Cp2) then
    begin
      Result := Cp2 + '\' + AClassname + '.java';
      if FileExists(Result) then
        Exit;
      Result := Cp2 + '\' + AClassname + '.class';
      if FileExists(Result) then
        Exit;
    end;
    Posi := Pos(';', Cp1);
  end;
  Result := '';
end;

function TFConfiguration.IsInClasspath(const AClassname: string;
  ActPath: string): Boolean;
var
  Cp1, Cp2: string;
  Posi: Integer;

  function InJarFile(const AClassname, JarFilename: string): Boolean;
  var
    JarFile: TZipFile;
    Classname: string;
  begin
    Result := False;
    Classname := ReplaceStr(AClassname, '/', '\') + '.class';
    JarFile := TZipFile.Create;
    try
      try
        JarFile.Open(JarFilename, zmRead);
        Result := (JarFile.IndexOf(Classname) > -1);
      finally
        FreeAndNil(JarFile);
      end;
    except
      on E: Exception do
        Log('InJarFile', E);
    end;
  end;

begin
  Result := False;
  Posi := Pos(';', ActPath);
  if Posi > 0 then
    ActPath := Copy(ActPath, 1, Posi - 1);
  Cp1 := UnHideBlanks(GetClassPath) + ';';
  Cp1 := ReplaceStr(Cp1, '.;', ActPath + ';');
  Posi := Pos(';', Cp1);
  while (Posi > 0) and not Result do
  begin
    Cp2 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    if (ExtractFileExt(Cp2) = '.jar') and (ExtractFileName(Cp2) <> 'rt.jar') and
      FileExists(Cp2) then
      Result := InJarFile(ReplaceStr(AClassname, '.', '/'), Cp2)
    else
    begin
      Cp2 := WithTrailingSlash(Cp2) + ReplaceStr(AClassname, '.', '\');
      if FileExists(Cp2 + '.java') or FileExists(Cp2 + '*.class') then
        Result := True;
    end;
    Posi := Pos(';', Cp1);
  end;
end;

procedure TFConfiguration.MakeClassAndPackageList(const Classfile,
  Packagefile: string);
var
  StringList, PackageList: TStringList;
  Str: string;
  Posi: Integer;
  JarFile: TZipFile;
begin
  if not FileExists(FJDKFolder + '\jre\lib\rt.jar') then
    Exit;

  JarFile := TZipFile.Create;
  try
    try
      JarFile.Open(FJDKFolder + '\jre\lib\rt.jar', zmRead);
      StringList := TStringList.Create;
      StringList.Duplicates := dupIgnore;
      StringList.Sorted := True;
      StringList.Add('[Classes]');
      PackageList := TStringList.Create;
      PackageList.Duplicates := dupIgnore;
      PackageList.Sorted := True;
      PackageList.Add('[Packages]');
      for var I := 0 to JarFile.FileCount - 1 do
      begin
        Str := JarFile.Filename[I];
        if Pos('.class', Str) > 0 then
        begin
          Str := Copy(Str, 1, Length(Str) - 6);
          Str := ReplaceStr(Str, '/', '.');
          Posi := LastDelimiter('.', Str);
          StringList.Add(Copy(Str, Posi + 1, Length(Str)) + '=' + Str);
          PackageList.Add(Copy(Str, 1, Posi - 1));
        end;
      end;
      StringList.SaveToFile(Classfile);
      PackageList.SaveToFile(Packagefile);
    except
      on E: Exception do
        ErrorMsg('Error in MakeClassAndPackageList: ' + E.Message);
    end;
  finally
    FreeAndNil(JarFile);
  end;
end;

procedure TFConfiguration.MakeSystemClasses;
var
  Str: string;

  function FromDocumentation: Boolean;
  begin
    if FileExists(FJavaManual) then
      if IsCHM(FJavaManual) then
        Result := FCHMRootOk
      else
        Result := True
    else
      Result := False;
  end;

begin
  var
  StringList := TStringList.Create;
  FreeAndNil(FAllClasses);
  FAllClasses := TStringList.Create;
  FAllClasses.Sorted := True;
  FAllClasses.CaseSensitive := True;
  FAllClasses.Duplicates := dupIgnore;
  FreeAndNil(FAllInterfaces);
  FAllInterfaces := TStringList.Create;
  FAllInterfaces.Sorted := True;
  FAllInterfaces.CaseSensitive := True;
  FAllInterfaces.Duplicates := dupIgnore;
  FreeAndNil(FAllPackages);
  FAllPackages := TStringList.Create;
  FAllPackages.Sorted := True;
  FAllPackages.CaseSensitive := True;
  FAllPackages.Duplicates := dupIgnore;
  FreeAndNil(FAllClasspathClasses);
  FAllClasspathClasses := TStringList.Create;
  FAllClasspathClasses.Sorted := True;
  FAllClasspathClasses.CaseSensitive := True;
  FAllClasspathClasses.Duplicates := dupIgnore;

  try
    if FromDocumentation then
    begin
      Str := FJavaCache + '\classes\' + IntToStr(GetDocumentationVersion);
      if not SysUtils.ForceDirectories(Str) then
      begin
        ErrorMsg(Format(_(LNGCanNotCreateDirectory), [Str]));
        Exit;
      end;
      if not(FileExists(Str + '\classesd.txt') and
        FileExists(Str + '\interfacesd.txt') and
        FileExists(Str + '\packagesd.txt')) then
        MakeClassAndPackageListFromDocumentation(Str + '\classesd.txt',
          Str + '\interfacesd.txt', Str + '\packagesd.txt');
      FAllClasses.LoadFromFile(Str + '\classesd.txt');
      if FAllClasses.IndexOf('Class=java.lang.Class') = -1 then
        MakeClassAndPackageListFromDocumentation(Str + '\classesd.txt',
          Str + '\interfacesd.txt', Str + '\packagesd.txt');
      if (FileExists(GetJavaManualFX + '\AllClasses-frame.html') or
        FileExists(GetJavaManualFX + '\AllClasses-index.html')) and
        (FAllClasses.IndexOf('Label=javafx.scene.control.Label') = -1) then
      begin
        MakeClassAndPackageListFromDocumentation(Str + '\classesdfx.txt',
          Str + '\interfacesdfx.txt', Str + '\packagesdfx.txt');
        StringList.LoadFromFile(Str + '\classesdfx.txt');
        FAllClasses.AddStrings(StringList);
        FAllClasses.SaveToFile(Str + '\classesd.txt');
        StringList.Clear;
        DeleteFile(Str + '\classesdfx.txt');

        FAllInterfaces.LoadFromFile(Str + '\interfacesd.txt');
        StringList.LoadFromFile(Str + '\interfacesdfx.txt');
        FAllInterfaces.AddStrings(StringList);
        FAllInterfaces.SaveToFile(Str + '\interfacesd.txt');
        StringList.Clear;
        DeleteFile(Str + '\interfacesdfx.txt');

        FAllPackages.LoadFromFile(Str + '\packagesd.txt');
        StringList.LoadFromFile(Str + '\packagesdfx.txt');
        FAllPackages.AddStrings(StringList);
        FAllPackages.SaveToFile(Str + '\packagesd.txt');
        StringList.Clear;
        DeleteFile(Str + '\packagesdfx.txt');
      end
      else
      begin
        FAllInterfaces.LoadFromFile(Str + '\interfacesd.txt');
        FAllPackages.LoadFromFile(Str + '\packagesd.txt');
      end;
    end
    else
    begin
      // from jar-file, not from documentation
      Str := FJavaCache + '\classes\' + IntToStr(GetJavaVersion);
      if not SysUtils.ForceDirectories(Str) then
      begin
        ErrorMsg(Format(_(LNGCanNotCreateDirectory), [Str]));
        Exit;
      end;
      if not(FileExists(Str + '\classes.txt') and
        FileExists(Str + '\packages.txt')) then
        MakeClassAndPackageList(Str + '\classes.txt', Str + '\packages.txt');
      if FileExists(Str + '\classes.txt') then
        FAllClasses.LoadFromFile(Str + '\classes.txt');
      if FileExists(Str + '\packages.txt') then
        FAllPackages.LoadFromFile(Str + '\packages.txt');
    end;
    Str := FJavaCache + '\classes\classpathclasses.txt';
    if not FileExists(Str) then
      MakeClasspathClasses
    else
      FAllClasspathClasses.LoadFromFile(Str);
  except
    on E: Exception do
      ErrorMsg('Error in MakeSystemClasses: ' + E.Message);
  end;
  FreeAndNil(StringList);
end;

procedure TFConfiguration.MakeClasspathClasses;
var
  Cp1, Cp2, Ext, Classname, Fullclassname: string;
  Posi: Integer;

  procedure CollectInJarFile(const JarFilename: string);
  var
    Str: string;
  begin
    var
    JarFile := TZipFile.Create;
    try
      try
        JarFile.Open(JarFilename, zmRead);
        for var I := 0 to JarFile.FileCount - 1 do
        begin
          Str := JarFile.Filenames[I];
          Ext := ExtractFileExt(Str);
          if Ext = '.class' then
          begin
            var
            J := LastDelimiter('\', Str);
            Classname :=
              ChangeFileExt(ExtractFileName(Copy(Str, J + 1, Length(Str))), '');
            if (Pos('$', Classname) = 0) then
            begin
              Fullclassname := ReplaceStr(ChangeFileExt(Str, ''), '\', '.');
              FAllClasspathClasses.Add(Classname + '=' + Fullclassname);
            end;
          end;
        end;
      except
        on E: Exception do
          Log('Error in CollectInJarFile: ', E);
      end;
    finally
      FreeAndNil(JarFile);
    end;
  end;

  procedure CollectInDirectory(const Cp2, Ext: string);
  begin
    if not DirectoryExists(Cp2) then
      Exit;

    var
    Filenames := TDirectory.GetFiles(Cp2, Ext);
    for var Filename in Filenames do
    begin
      var
      AFilename := ChangeFileExt(ExtractFileName(Filename), '');
      if Pos('$', AFilename) = 0 then
        FAllClasspathClasses.Add(AFilename + '=' + AFilename);
    end;
  end;

  procedure CollectJarsInDirectory(const Cp2: string);
  begin
    if not DirectoryExists(Cp2) then
      Exit;

    var
    Filenames := TDirectory.GetFiles(Cp2, '*.jar');
    for var Filepath in Filenames do
      CollectInJarFile(Filepath);
  end;

begin
  Cp1 := FJavaClasspathAdmin + ';';
  if FJavaClasspathUser <> '' then
    Cp1 := FJavaClasspathUser + ';' + Cp1;
  Posi := Pos(';', Cp1);
  while Posi > 0 do
  begin
    Cp2 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    if EndsWith(Cp2, '*') then
    begin
      Delete(Cp2, Length(Cp2), 1);
      CollectJarsInDirectory(Cp2);
    end
    else if (ExtractFileExt(Cp2) = '.jar') and
      (ExtractFileName(Cp2) <> 'rt.jar') and FileExists(Cp2) then
      CollectInJarFile(Cp2)
    else if Cp2 <> '.' then
    begin
      Cp2 := WithTrailingSlash(Cp2);
      CollectInDirectory(Cp2, '*.class');
      CollectInDirectory(Cp2, '*.java');
    end;
    Posi := Pos(';', Cp1);
  end;
  try
    FAllClasspathClasses.SaveToFile
      (FJavaCache + '\classes\classpathclasses.txt');
  except
    on E: Exception do
      ErrorMsg('Error in MakeClasspathClasses: ' + E.Message);
  end;
end;

procedure TFConfiguration.MakeClassAndPackageListFromDocumentation
  (const Classfile, InterfaceFile, Packagefile: string);
var
  StringList, ClassList, InterfaceList, PackageList: TStringList;
  Str, AClassname: string;
  Posi: Integer;
  AIsInterface, AIsClass, IsFX, WithModule: Boolean;
begin
  try
    ClassList := TStringList.Create;
    ClassList.Duplicates := dupIgnore;
    ClassList.Sorted := True;
    InterfaceList := TStringList.Create;
    InterfaceList.Duplicates := dupIgnore;
    InterfaceList.Sorted := True;
    PackageList := TStringList.Create;
    PackageList.Duplicates := dupIgnore;
    PackageList.Sorted := True;
    IsFX := Pos('classesdfx', Classfile) > 0;
    WithModule := False;
    if IsFX then
      Str := GetJavaManualFX + '\AllClasses-frame.html'
    else
      Str := GetJavaManual + '\AllClasses-frame.html';
    if not IsCHM(Str) and not FileExists(Str) then
    begin
      WithModule := True;
      if IsFX then
        Str := GetJavaManualFX + '\AllClasses-index.html'
      else
        Str := GetJavaManual + '\AllClasses-index.html';
    end;

    StringList := TStringList.Create;
    if IsCHM(Str) then
    begin
      if GlobalFileExists(Str) then
        StringList.Text := LoadFromCHM(Str);
    end
    else
      StringList.Text := MyCodeCompletion.LoadTextFromFile(Str);
    for var I := 0 to StringList.Count - 1 do
    begin
      Str := StringList[I];
      Posi := Pos('.html', Str);
      if Posi > 0 then
      begin
        if Pos('enum in ', Str) > 0 then
          Continue;
        AIsInterface := Pos('interface in ', Str) > 0;
        AIsClass := Pos('class in ', Str) > 0;
        if not(AIsClass or AIsInterface) then
          Continue;
        Delete(Str, Posi, Length(Str));
        Posi := Length(Str);
        while Posi > 0 do
        begin
          if Str[Posi] = '/' then
          begin
            AClassname := Copy(Str, Posi + 1, Length(Str));
            Break;
          end
          else
            Dec(Posi);
        end;
        while Posi > 0 do
        begin
          if Str[Posi] = '"' then
            Break
          else
            Dec(Posi);
        end;
        if Posi > 0 then
        begin
          Delete(Str, 1, Posi);
          Posi := Pos('/api', Str);
          if (Posi > 0) and not WithModule then
            Delete(Str, 1, Posi + 4);
          if WithModule then
          begin
            Posi := Pos('/', Str);
            Delete(Str, 1, Posi);
          end;
          Str := ReplaceStr(Str, '/', '.');
          if AIsInterface then
            InterfaceList.Add(AClassname + '=' + Str)
          else
            ClassList.Add(AClassname + '=' + Str);
          Posi := LastDelimiter('.', Str);
          if Posi > 0 then
            Str := Copy(Str, 1, Posi - 1);
          PackageList.Add(Str);
        end;
      end;
    end;
    ClassList.SaveToFile(Classfile);
    InterfaceList.SaveToFile(InterfaceFile);
    PackageList.SaveToFile(Packagefile);
    FreeAndNil(StringList);
    FreeAndNil(ClassList);
    FreeAndNil(PackageList);
    FreeAndNil(InterfaceList);
  except
    on E: Exception do
      ErrorMsg('Error in MakeClassAndPackageListFromDocumentation: ' +
        E.Message);
  end;
end;

function TFConfiguration.ToStringListClass(const AClassname: string): string;
begin
  var
  Posi := LastDelimiter('.', AClassname);
  if Posi > 0 then
    Result := Copy(AClassname, Posi + 1, Length(AClassname)) + '=' + AClassname
  else
    Result := AClassname;
end;

function TFConfiguration.IsAPIInterface(var AClassname: string): Boolean;
begin
  Result := False;
  if Assigned(FAllInterfaces) and (FAllInterfaces.Count > 0) then
  begin
    var
    Str := ToStringListClass(AClassname);
    var
    Int := FAllInterfaces.IndexOf(Str);
    if Int > -1 then
    begin
      AClassname := FAllInterfaces.ValueFromIndex[Int];
      Result := True;
    end
    else
    begin
      Int := FAllInterfaces.IndexOfName(AClassname);
      if Int > -1 then
      begin
        AClassname := FAllInterfaces.ValueFromIndex[Int];
        Result := True;
      end;
    end;
  end;
end;

function TFConfiguration.IsAPIClass(var AClassname: string): Boolean;
var
  Int: Integer;
  Str: string;
begin
  Result := False;
  if Assigned(FAllClasses) and (FAllClasses.Count > 0) then
  begin
    Str := ToStringListClass(AClassname);
    Int := FAllClasses.IndexOf(Str);
    if Int > -1 then
      Result := True
    else
    begin
      Int := FAllClasses.IndexOfName(AClassname);
      if Int > -1 then
      begin
        AClassname := FAllClasses.ValueFromIndex[Int];
        Result := True;
      end
      else
      begin
        Int := FAllClasspathClasses.IndexOfName(AClassname);
        if Int > -1 then
        begin
          AClassname := FAllClasspathClasses.ValueFromIndex[Int];
          Result := True;
        end;
      end;
    end;
  end;
end;

function TFConfiguration.IsAPIPackage(Packagename: string): Boolean;
begin
  Result := False;
  if Length(Packagename) > 0 then
  begin
    if Packagename[Length(Packagename)] = '.' then
      Delete(Packagename, Length(Packagename), 1);
    if Assigned(FAllPackages) and (FAllPackages.Count > 0) then
      Result := (FAllPackages.IndexOf(Packagename) > -1);
  end;
end;

function TFConfiguration.IsAPIClassOrInterface(AClassname: string): Boolean;
begin
  AClassname := WithoutGeneric(AClassname);
  Result := IsAPIClass(AClassname) or IsAPIInterface(AClassname);
end;

procedure TFConfiguration.SetElevationRequiredState(AControl: TWinControl);
const
  BCM_FIRST = $1600;
  BCM_SETSHIELD = BCM_FIRST + $000C;
begin
  SendMessage(AControl.Handle, BCM_SETSHIELD, 0, LPARAM(True));
end;

function TFConfiguration.GetJarsFromClasspath: string;
var
  Posi: Integer;
  Cp1, Str1, Jar: string;

  procedure CollectJarsInDirectory(const Cp2: string);
  begin
    if not DirectoryExists(Cp2) then
      Exit;

    var
    Filenames := TDirectory.GetFiles(Cp2, '*.jar');
    for var Filepath in Filenames do
      Cp1 := Filepath + ';' + Cp1;
  end;

begin
  Jar := '';
  Cp1 := FJavaClasspathUser + ';' + FJavaClasspathAdmin + ';';
  Posi := Pos(';', Cp1);
  while Posi > 0 do
  begin
    Str1 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    if EndsWith(Str1, '*') then
      CollectJarsInDirectory(Str1)
    else if LowerCase(ExtractFileExt(Str1)) = '.Jar' then
      Jar := Jar + ',file:\\\' + Str1;
    Posi := Pos(';', Cp1);
  end;
  Result := Jar;
end;

function TFConfiguration.UpdatePossible(const Source, Target: string): Boolean;
begin
  if not SysUtils.DirectoryExists(Source) then
    SysUtils.ForceDirectories(Source);
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and IsAdministrator or
    VistaOrBetter;
  SysUtils.ForceDirectories(Target);
  if not Result then
    Result := SysUtils.DirectoryExists(Target) and HasWriteAccess(Target);
end;

function TFConfiguration.RunAsAdmin(Wnd: HWND;
  const AFile, Parameters: string): Boolean;
var
  Sei: TShellExecuteInfoA;
begin
  FillChar(Sei, SizeOf(Sei), 0);
  Sei.cbSize := SizeOf(Sei);
  Sei.Wnd := Wnd;
  Sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  // left side is pAnsiChar
  Sei.lpVerb := 'runas';
  Sei.lpFile := PAnsiChar(AnsiString(AFile));
  Sei.lpParameters := PAnsiChar(AnsiString(Parameters));
  Sei.nShow := SW_SHOWNORMAL;
  Result := ShellExecuteExA(@Sei);
end;

function TFConfiguration.GetFrameType(JavaProgram: string;
  Startclass: Boolean = False): Integer;
var
  AForm: TFEditForm;
  JavaScanner: TJavaScanner;
begin
  Result := 1;
  if ExtractFileExt(JavaProgram) <> '.java' then
    Exit;
  var
  Str := '';
  if Startclass and (FJavaStartClass <> '') and FileExists(FJavaStartClass) then
    JavaProgram := FJavaStartClass;

  AForm := TFEditForm(FJava.GetTDIWindowType(JavaProgram, '%E%'));
  if Assigned(AForm) then
    Str := AForm.Editor.Text
  else if FileExists(JavaProgram) then
  begin
    var
    StringList := TStringList.Create;
    try
      try
        StringList.LoadFromFile(JavaProgram);
        Str := StringList.Text;
      except
        on E: Exception do
          ErrorMsg('Error in GetFrameType: ' + Format(LNGCanNotRead,
            [JavaProgram]) + ' ' + E.Message);
      end;
    finally
      FreeAndNil(StringList);
    end;
  end;
  if Str <> '' then
  begin
    JavaScanner := TJavaScanner.Create;
    try
      JavaScanner.Init(Str);
      Result := JavaScanner.GetFrameType;
    finally
      JavaScanner.Destroy;
    end;
  end;
end;

function TFConfiguration.GetPackage(JavaProgram: string;
  Startclass: Boolean = False): string;
var
  Str: string;
  AForm: TFEditForm;
  JavaScanner: TJavaScanner;
begin
  Result := '';
  if ExtractFileExt(JavaProgram) <> '.java' then
    Exit;
  if Startclass and (FJavaStartClass <> '') and FileExists(FJavaStartClass) then
    JavaProgram := FJavaStartClass;
  AForm := TFEditForm(FJava.GetTDIWindowType(JavaProgram, '%E%'));
  if Assigned(AForm) then
    Str := AForm.Editor.Text
  else if FileExists(JavaProgram) then
  begin
    var
    StringList := TStringList.Create;
    try
      try
        StringList.LoadFromFile(JavaProgram);
        Str := StringList.Text;
      except
        on E: Exception do
          ErrorMsg('Error in GetPackage!' + Format(LNGCanNotRead, [JavaProgram])
            + ' Error: ' + E.Message);
      end;
    finally
      FreeAndNil(StringList);
    end;
  end;
  if Str <> '' then
  begin
    JavaScanner := TJavaScanner.Create;
    try
      JavaScanner.Init(Str);
      Result := JavaScanner.GetPackage(Str);
    finally
      JavaScanner.Destroy;
    end;
  end;
end;

function TFConfiguration.GetCompileParameter(const Pathname, Package,
  Encoding: string): string;
var
  Str: string;
begin
  if FMindstormsMode then
    case FMindstormsVersion of
      0:
        Str := '';
      1:
        Str := '';
    else
      Str := FMindstormsParameter + ' -classpath ' +
        HideBlanks(FLejosVerzeichnis + '\lib\ev3\ev3classes.jar;' +
        UnHideBlanks(GetClassPath(Pathname, Package)));
    end
  else
  begin
    Str := ' -classpath ' + GetClassPath(Pathname, Package) + ' ' +
      GetJavaCompilerParameter(Pathname);
    if FCompilerEncoding <> '' then
      Str := Str + ' -encoding ' + FCompilerEncoding
    else if Encoding <> 'ANSI' then
      Str := Str + ' -encoding ' + Encoding;
  end;
  Result := Str;
end;

function TFConfiguration.GetJavaCompilerParameter(const Pathname
  : string): string;
begin
  Result := FJavaCompilerParameter;
  var
  Str := '';
  var
  NeedsJavaFX := False;
  if FileExists(Pathname) then
  begin
    var
    StringList := TStringList.Create;
    try
      try
        StringList.LoadFromFile(Pathname);
        Str := StringList.Text;
      except
        on E: Exception do
          ErrorMsg('Error in GetJavaCompilerParameter! ' + Format(LNGCanNotRead,
            [Pathname]) + ' Error: ' + E.Message);
      end;
    finally
      FreeAndNil(StringList);
    end;
  end;
  if Str <> '' then
  begin
    var
    JavaScanner := TJavaScanner.Create;
    NeedsJavaFX := JavaScanner.NeedsJavaFX(Str);
    JavaScanner.Destroy;
  end;
  if NeedsJavaFX and (FJavaVersion >= 9) and (FJavaFXFolder <> '') then
    Result := '--module-path ' + HideBlanks(FJavaFXFolder) +
      '\lib --add-modules=' + FJavaFXParameter + ' ' + Result;
end;

function TFConfiguration.GetJavaInterpreterParameter(IsFX: Boolean): string;
begin
  Result := FJavaInterpreterParameter;
  if IsFX and (FJavaVersion >= 11) and (FJavaFXFolder <> '') then
    Result := Result + ' --module-path ' + HideBlanks(FJavaFXFolder) +
      '\lib --add-modules=' + FJavaFXParameter;
end;

procedure TFConfiguration.SetPrinter(const Str: string);
begin
  if Str <> '' then
  begin
    var
    Index := Printer.Printers.IndexOf(Str);
    if (-1 < Index) and (Index < Printer.Printers.Count) then
      SetPrinterIndex(Index);
  end;
end;

function TFConfiguration.PathForSystemClass(const Path: string): Boolean;
begin
  Result := (Pos(FJavaCache, Path) = 1) and FileExists(Path);
end;

function TFConfiguration.PathForUserClass(const Path: string): Boolean;
begin
  Result := (Pos(FJavaCache, Path) = 0) and FileExists(Path);
end;

function TFConfiguration.GetAppletArchiv: string;
begin
  var
  Archive := GetJarsFromClasspath;
  if Archive <> '' then
    Archive := 'archive="' + Copy(Archive, 2, Length(Archive)) + '" ';
  Result := Archive;
end;

function TFConfiguration.HasAgeClass(const Pathname: string): Boolean;
var
  FDT1, FDT2: TDateTime;
begin
  var
  ClassDatei := GetClass(GetClassPath, Pathname);
  if (ClassDatei <> '') and FileAge(ClassDatei, FDT1) and FileAge(Pathname, FDT2)
  then
    Result := (FDT1 >= FDT2)
  else
    Result := False;
end;

function TFConfiguration.HasClass(const Pathname: string): Boolean;
begin
  var
  ClassDatei := GetClass(GetClassPath, Pathname);
  Result := (ClassDatei <> '');
end;

procedure TFConfiguration.SaveConfigurationForProject;
begin
  FSavedClasspathUser := FJavaClasspathUser;
  FSavedInterpreterParameter := FJavaInterpreterParameter;
  FSavedCompilerParameter := FJavaCompilerParameter;
  FSavedJavaStartClass := FJavaStartClass;
end;

procedure TFConfiguration.RestoreConfigurationAfterProject;
begin
  FJavaClasspathUser := FSavedClasspathUser;
  FJavaInterpreterParameter := FSavedInterpreterParameter;
  FJavaCompilerParameter := FSavedCompilerParameter;
  FJavaStartClass := FSavedJavaStartClass;
end;

procedure TFConfiguration.MakeControlStructureTemplates;
const
  ControlStructure: array [1 .. 10] of string = ('#if', '#while', '#for', '#do',
    '#switch', '#try', 'else', '} else', '#ifelse', '{ }');
var
  Posi: Integer;
  Str: string;
  StringList: TStringList;
begin
  for var I := 1 to 21 do
  begin
    FreeAndNil(FControlStructureTemplates[I]);
    FControlStructureTemplates[I] := TStringList.Create;
  end;
  FControlStructureTemplates[1].Text := 'if (|) {' + CrLf + FIndent1 +
    CrLf + '}';
  FControlStructureTemplates[2].Text := 'while (|) { ' + CrLf + FIndent1 +
    CrLf + '}';
  FControlStructureTemplates[3].Text := 'for (|; ; ) {' + CrLf + FIndent1 +
    CrLf + '}';
  FControlStructureTemplates[4].Text := 'do {' + CrLf + FIndent1 + '|' + CrLf +
    '} while ();';
  FControlStructureTemplates[5].Text := 'switch (|) {' + CrLf + FIndent1 +
    'case  : ' + CrLf + FIndent2 + CrLf + FIndent2 + 'break;' + CrLf + FIndent1
    + 'case  : ' + CrLf + FIndent2 + CrLf + FIndent2 + 'break;' + CrLf +
    FIndent1 + 'Default: ' + CrLf + FIndent2 + CrLf + '}';
  FControlStructureTemplates[6].Text := 'try {' + CrLf + FIndent1 + '|' + CrLf +
    '} catch(Exception e) {' + CrLf + FIndent1 + CrLf + '} finally {' + CrLf +
    FIndent1 + CrLf + '}';
  FControlStructureTemplates[7].Text := 'else {' + CrLf + FIndent1 + '|' +
    CrLf + '}';
  FControlStructureTemplates[8].Text := '} else {' + CrLf + FIndent1 + '|' +
    CrLf + '}';
  FControlStructureTemplates[9].Text := 'if (|) {' + CrLf + FIndent1 + CrLf +
    '} else {' + CrLf + FIndent1 + CrLf + '}';
  FControlStructureTemplates[10].Text := '{' + CrLf + FIndent1 + '|' +
    CrLf + '}';

  FControlStructureTemplates[11].Text := 'else if (|) {' + CrLf + FIndent1 +
    CrLf + '}';
  FControlStructureTemplates[12].Text := 'for (int i = 0; i < |10; i++) {' +
    CrLf + FIndent1 + CrLf + '}';
  FControlStructureTemplates[13].Text := 'catch (Exception e) {' + CrLf +
    FIndent1 + '|' + CrLf + '}';
  FControlStructureTemplates[14].Text := 'finally {' + CrLf + FIndent1 + '|' +
    CrLf + '}';
  FControlStructureTemplates[15].Text := 'private |void name() {' + CrLf +
    FIndent1 + CrLf + '}';
  FControlStructureTemplates[16].Text := 'public |void name() {' + CrLf +
    FIndent1 + CrLf + '}';
  FControlStructureTemplates[17].Text := 'System.out.println(|); ' + CrLf;
  FControlStructureTemplates[18].Text := '|type name = new type();' + CrLf;
  FControlStructureTemplates[19].Text :=
    'public static void main(String[] args) {' + CrLf + FIndent1 + '|' +
    CrLf + '}';
  FControlStructureTemplates[20].Text := 'private static |void name() {' + CrLf
    + FIndent1 + CrLf + '}';
  FControlStructureTemplates[21].Text := 'public static |void name() {' + CrLf +
    FIndent1 + CrLf + '}';

  if not FileExists(FTemplates[9]) then
    Exit;
  StringList := TStringList.Create;
  try
    try
      StringList.LoadFromFile(FTemplates[9]);
      for var I := 1 to 10 do
      begin
        Posi := StringList.IndexOf(ControlStructure[I]);
        if Posi < 0 then
          Continue;
        FControlStructureTemplates[I].Text := '';
        Inc(Posi);
        while (Posi < StringList.Count) and
          (Copy(StringList[Posi], 1, 1) <> '#') do
        begin
          FControlStructureTemplates[I].Add(StringList[Posi]);
          Inc(Posi);
        end;
      end;

      StringList.Text := FControlStructureTemplates[9].Text;
      while (StringList.Count > 0) and (Pos('else', StringList[0]) = 0) do
        StringList.Delete(0);
      Posi := 0;
      while (StringList.Count > 0) and (Trim(StringList[Posi]) <> '') do
        Inc(Posi);
      if Posi < StringList.Count then
        StringList[Posi] := StringOfChar(' ', FIndent) + '|';
      FControlStructureTemplates[8].Text := StringList.Text;
      Str := StringList.Text;
      Posi := Pos('else', Str);
      if Posi > 0 then
        Delete(Str, 1, Posi - 1);
      FControlStructureTemplates[7].Text := Str;
    except
      on E: Exception do
        ErrorMsg('Error in MakeControlStructureTemplates! ' +
          Format(LNGCanNotRead, [FTemplates[9]]) + ' Error: ' + E.Message);
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TFConfiguration.ShortenPath(WinControl: TWinControl;
  const Str: string);
var
  Str1, Str2, Str3: string;
  Posi, Wid: Integer;
begin
  WinControl.Hint := Str;
  WinControl.ShowHint := True;
  if WinControl is TEdit then
    Wid := WinControl.Width
  else
    Wid := WinControl.Width - 16;
  Str1 := GlobalMinimizeName(Str, Canvas, Wid);
  if WinControl is TEdit then
    TEdit(WinControl).Text := Str1
  else
    TComboBox(WinControl).Text := Str1;
  Posi := Pos('...', Str1);
  if Posi > 0 then
  begin
    Str2 := Str;
    Delete(Str2, 1, Posi - 1);
    Str3 := Copy(Str1, Posi + 3, Length(Str1));
    Posi := Pos(Str3, Str2);
    Delete(Str2, Posi, Length(Str1));
    WinControl.HelpKeyword := Str2;
  end
  else
    WinControl.HelpKeyword := '';
end;

function TFConfiguration.ExtendPath(WinControl: TWinControl): string;
var
  Str: string;
  Posi: Integer;
begin
  if WinControl is TEdit then
    Str := TEdit(WinControl).Text
  else
    Str := TComboBox(WinControl).Text;
  Posi := Pos('...', Str);
  if Posi > 0 then
  begin
    Delete(Str, Posi, 3);
    Insert(WinControl.HelpKeyword, Str, Posi);
  end;
  Result := Str;
end;

procedure TFConfiguration.CheckFile(WinControl: TWinControl;
  EmptyAllowed: Boolean);
var
  Str: string;
begin
  Str := ExtendPath(WinControl);
  ShortenPath(WinControl, Str);
  if WinControl is TEdit then
    TEdit(WinControl).Color := GetCheckColor(Str, EmptyAllowed)
  else
    TComboBox(WinControl).Color := GetCheckColor(Str, EmptyAllowed);
  WinControl.Enabled := not FLockedPaths;
end;

procedure TFConfiguration.CheckFileWithoutShortenpath(WinControl: TWinControl;
  EmptyAllowed: Boolean);
begin
  if WinControl is TEdit then
    TEdit(WinControl).Color := GetCheckColor(TEdit(WinControl).Text,
      EmptyAllowed)
  else
    TComboBox(WinControl).Color :=
      GetCheckColor(TComboBox(WinControl).Text, EmptyAllowed);
  WinControl.Enabled := not FLockedPaths;
end;

procedure TFConfiguration.ComboBoxAddEx(ComboBox: TComboBox);
begin
  ComboBox.Text := ExtendPath(ComboBox);
  ComboBoxAdd(ComboBox);
  ShortenPath(ComboBox, ComboBox.Text);
end;

function TFConfiguration.SearchClassInClasspath(const AClassname, Sourcepath,
  Package: string): string;
var
  Cp1, Cp2: string;
  Posi: Integer;

  function InJarFile(const JarFilename: string; AClassname: string): string;
  var
    JarFile: TZipFile;
    AFilename: string;
  begin
    Result := '';
    AClassname := AClassname + '.class';
    JarFile := TZipFile.Create;
    try
      try
        JarFile.Open(JarFilename, zmRead);
        for var I := 0 to JarFile.FileCount - 1 do
        begin
          AFilename := JarFile.Filenames[I];
          if (AFilename = AClassname) or EndsWith(AFilename, '\' + AClassname)
          then
          begin
            Result := FJavaCache + '\' + AFilename;
            if not FileExists(Result) then
            begin
              SysUtils.ForceDirectories(ExtractFilePath(Result));
              JarFile.Extract(I, Result);
            end;
            Break;
          end;
        end;
      except
        on E: Exception do
          ErrorMsg('Error in InJarFile! ' + Format(LNGCanNotOpen, [JarFilename])
            + ' Error: ' + E.Message);
      end;
    finally
      FreeAndNil(JarFile);
    end;
  end;

  function SearchFile(const Dir, Fname: string): string;
  begin
    Result := '';
    var
    Filepath := TPath.Combine(Dir, Fname);
    if FileExists(Filepath) then
      Exit(Filepath)
    else if DirectoryExists(Dir) then
    begin
      var
      Filenames := TDirectory.GetFiles(Dir, Fname,
        TSearchOption.soAllDirectories);
      if Length(Filenames) > 0 then
        Result := Filenames[0];
    end;
  end;

  function InDirectory(Directory: string; const AClassname: string): string;
  begin
    Result := SearchFile(Directory, AClassname + '.java');
    if Result = '' then
      Result := SearchFile(Directory, AClassname + '.class');
  end;

begin
  Result := '';
  Cp1 := UnHideBlanks(GetClassPathJarExpanded(Sourcepath, Package));
  Posi := Pos(';', Cp1);
  while (Posi > 0) and (Result = '') do
  begin
    Cp2 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    if (ExtractFileExt(Cp2) = '.jar') and (ExtractFileName(Cp2) <> 'rt.jar') and
      FileExists(Cp2) then
      Result := InJarFile(Cp2, AClassname)
    else
    begin
      if Package <> '' then
        Cp2 := Cp2 + ReplaceStr(Package, '.', '\') + '\';
      Result := InDirectory(Cp2, AClassname);
    end;
    Posi := Pos(';', Cp1);
  end;
end;

function TFConfiguration.SearchClassInDirectory(const AClassname, Sourcepath,
  Package: string): string;
begin
  Result := '';
  var
  Cp1 := GetPackageDirectoryRelativ(Sourcepath, Package);
  if Package <> '' then
    Cp1 := Cp1 + ReplaceStr(Package, '.', '\') + '\';
  Cp1 := Cp1 + AClassname + '.java';
  if FileExists(Cp1) then
    Result := Cp1
  else
    Result := '';
end;

function TFConfiguration.IsInterface(const Pathname: string): Boolean;
var
  JavaScanner: TJavaScanner;
begin
  Result := False;
  if not FileExists(Pathname) then
    Exit;
  JavaScanner := TJavaScanner.Create;
  try
    var
    Text := TFile.ReadAllText(Pathname);
    Result := JavaScanner.IsInterface(Text);
  finally
    FreeAndNil(JavaScanner);
  end;
end;

function TFConfiguration.GetCompleteClassname(FullImports, Classimports,
  UserImportClasses: TStringList; const AClassname: string): string;
var
  Num, Int, Posi: Integer;
  Str, Cname: string;
begin
  Result := '';

  Cname := AClassname;
  Posi := Pos('<', Cname);
  if Posi > 0 then
    Cname := Copy(Cname, 1, Posi - 1);
  Posi := Pos('[', Cname);
  if Posi > 0 then
    Cname := Copy(Cname, 1, Posi - 1);

  // 1. look in cache
  try
    // >>> TFConfiguration.getCompleteClassname Classname= System # exe=0
    Posi := FImportCache.IndexOfName(Cname);
    if (Posi > -1) and (Posi < FImportCache.Count) then
    begin
      Result := FImportCache.ValueFromIndex[Posi];
      Exit;
    end;
  except
    on E: Exception do
      Log('TFConfiguration.getCompleteClassname Classname= ' + Cname, E);
  end;

  // 2. look in ClassImports
  Num := Classimports.IndexOfName(Cname);
  if (Num > -1) and (Num < Classimports.Count) then
  begin
    Result := Classimports.ValueFromIndex[Num] + '.' + AClassname;
    Exit;
  end;

  // 3. look in FullImports
  for Num := 0 to FullImports.Count - 1 do
  begin
    Str := FullImports[Num] + Cname;
    if IsAPIPackage(FullImports[Num]) then
    begin
      Int := FAllClasses.IndexOf(Cname + '=' + Str);
      if Int > -1 then
      begin
        Result := FAllClasses.ValueFromIndex[Int];
        Exit;
      end;
      Int := FAllInterfaces.IndexOf(Cname + '=' + Str);
      if Int > -1 then
      begin
        Result := FAllInterfaces.ValueFromIndex[Int];
        Exit;
      end;
    end
    else
    begin
      Int := UserImportClasses.IndexOf(Cname + '=' + Str);
      if Int > -1 then
      begin
        Result := UserImportClasses.ValueFromIndex[Int];
        Exit;
      end;
      Int := FAllClasspathClasses.IndexOf(Cname + '=' + Str);
      if Int > -1 then
      begin
        Result := FAllClasspathClasses.ValueFromIndex[Int];
        Exit;
      end;
    end;
  end;
end;

procedure TFConfiguration.CollectDocumentations;
var
  ADocumentation, DocsFolder: string;
  Posi: Integer;
begin
  FAllDocumentations.Clear;
  ADocumentation := FEditorFolder + 'docs\AllClasses-frame.html';
  if GlobalFileExists(ADocumentation) then
    FAllDocumentations.Add(ADocumentation);
  DocsFolder := FJavaJavaDocs + ';';
  repeat
    Posi := Pos(';', DocsFolder);
    ADocumentation := Copy(DocsFolder, 1, Posi - 1);
    Delete(DocsFolder, 1, Posi);
    if (ADocumentation <> '') and GlobalFileExists(ADocumentation) then
      FAllDocumentations.Add(ADocumentation);
  until DocsFolder = '';
  ADocumentation := GetJavaManual + '\AllClasses-frame.html';
  if GlobalFileExists(ADocumentation) then
    FAllDocumentations.Add(ADocumentation)
  else
  begin
    ADocumentation := GetJavaManual + '\AllClasses-index.html';
    if GlobalFileExists(ADocumentation) then
      FAllDocumentations.Add(ADocumentation);
  end;
  ADocumentation := GetJavaManualFX + '\AllClasses-frame.html';
  if GlobalFileExists(ADocumentation) then
    FAllDocumentations.Add(ADocumentation)
  else
  begin
    ADocumentation := GetJavaManualFX + '\AllClasses-index.html';
    if GlobalFileExists(ADocumentation) then
      FAllDocumentations.Add(ADocumentation);
  end;
  if FMindstormsMode then
  begin
    ADocumentation := ExtractFilePathEx(FMindstormsManual) +
      '\AllClasses-frame.html';
    if GlobalFileExists(ADocumentation) then
      FAllDocumentations.Add(ADocumentation);
  end;
  if FJUnitOk then
  begin
    ADocumentation := ExtractFilePathEx(FJUnitManual) +
      '\AllClasses-frame.html';
    if GlobalFileExists(ADocumentation) then
      FAllDocumentations.Add(ADocumentation);
  end;
end;

function TFConfiguration.GetJavaTools(const Tool: string): string;
begin
  Result := FJavaTools + Tool;
  if GetDocumentationVersion > 8 then
    Delete(Result, Length(Result), 1);
end;

class function TFConfiguration.IsDark: Boolean;
var
  BGColor, FGColor: TColor;
begin
  if StyleServices.IsSystemStyle then
  begin
    BGColor := clWhite;
    FGColor := clBlack;
  end
  else
  begin
    BGColor := StyleServices.GetStyleColor(scPanel);
    FGColor := StyleServices.GetStyleFontColor(sfTabTextInactiveNormal);
  end;
  if GetRValue(BGColor) + GetGValue(BGColor) + GetBValue(BGColor) >
    GetRValue(FGColor) + GetGValue(FGColor) + GetBValue(FGColor) then
    Result := False
  else
    Result := True;
end;

// style
// https://stackoverflow.com/questions/9130945/delphi-xe2-disabling-vcl-style-on-a-component
// https://theroadtodelphi.com/2012/02/06/changing-the-color-of-edit-controls-with-vcl-styles-enabled/

class procedure TFConfiguration.LoadGUIStyle(const Style: string);
var
  StyleInfo: TStyleInfo;
begin
  if not TStyleManager.TrySetStyle(Style, False) then
  begin
    var
    SearchDir := ExtractFilePath(ParamStr(0)) + 'styles';
    if DirectoryExists(SearchDir) then
    begin
      var
      Filenames := TDirectory.GetFiles(SearchDir, '*.vsf');
      for var Filename in Filenames do
        if TStyleManager.IsValidStyle(Filename, StyleInfo) and
          (StyleInfo.Name = Style) then
        begin
          TStyleManager.LoadFromFile(Filename);
          TStyleManager.TrySetStyle(Style, False);
          Break;
        end;
    end;
  end;
end;

class procedure TFConfiguration.SetGUIStyle;
var
  Str, AEditorFolder, APortAppDrive, Style: string;
  MyReg: TRegistry;
  AMachineIniFile: TMemIniFile;
  AUserIniFile: TIniFile;
begin
  Style := 'Windows';
  AEditorFolder := ExtractFilePath(ParamStr(0));
  ChDir(AEditorFolder);
  Str := ParamStr(1);
  if (Str = '') or (UpperCase(ExtractFileExt(Str)) <> '.INI') then
    Str := AEditorFolder + 'JEMachine.ini'
  else
    Str := ExpandUNCFileName(Str);

  APortAppDrive := ExtractFileDrive(Str); // with UNC we get \\Server\Freigabe
  if Pos(':', APortAppDrive) > 0 then
    APortAppDrive := Copy(APortAppDrive, 1, 1);

  if not FileExists(Str) then
  begin
    MyReg := TRegistry.Create;
    with MyReg do
    begin
      RootKey := HKEY_CURRENT_USER;
      Access := KEY_READ;
      try
        OpenKey('\Software\JavaEditor\Colors', False);
        if ValueExists('GUIStyle') then
          Style := ReadString('GUIStyle');
      finally
        CloseKey;
      end;
    end;
    FreeAndNil(MyReg);
  end
  else
  begin
    try
      AMachineIniFile := TMemIniFile.Create(Str);
      try
        Str := AMachineIniFile.ReadString('User', 'HomeDir', '<nix>');
        if Str = '<nix>' then
          Exit;
        Str := DissolveUsername(Str);
        if Copy(Str, 1, 2) = ':\' then
          Str := APortAppDrive + Str;
        Str := WithTrailingSlash(Str);
        Str := Str + 'JEUser.ini';
        try
          AUserIniFile := TIniFile.Create(Str);
          try
            if Assigned(AUserIniFile) then
              if Assigned(AMachineIniFile) and AMachineIniFile.ValueExists
                ('Colors', 'GUIStyle') then
                Style := AMachineIniFile.ReadString('Colors', 'GUIStyle',
                  'Windows')
              else
                Style := AUserIniFile.ReadString('Colors', 'GUIStyle',
                  'Windows');
          finally
            FreeAndNil(AUserIniFile);
          end;
        except
          on E: Exception do
            ErrorMsg('Error in TFConfiguration.SetGUIStyle! ' +
              Format(LNGCanNotRead, [Str]) + ' Error: ' + E.Message);
        end;
      finally
        FreeAndNil(AMachineIniFile);
      end;
    except
      on E: Exception do
        ErrorMsg('Error in TFConfiguration.SetGUIStyle! ' +
          Format(LNGCanNotRead, [Str]) + ' Error: ' + E.Message);
    end;
  end;
  if Style <> 'Windows' then
    LoadGUIStyle(Style);
end;

procedure TFConfiguration.ReadEditorStyleNames;
var
  StringList, EditorStyles: TStringList;
  Path, Str: string;
  IniFile: TMemIniFile;
  Posi: Integer;
begin
  Path := FEditorFolder + 'styles' + PathDelim;
  IniFile := TMemIniFile.Create(Path + 'DefaultColorsJava.ini');
  EditorStyles := TStringList.Create;
  EditorStyles.Sorted := True;
  EditorStyles.Duplicates := TDuplicates.dupIgnore;
  StringList := TStringList.Create;
  try
    IniFile.ReadSections(StringList);
    for var I := 0 to StringList.Count - 1 do
    begin
      Str := StringList[I];
      Posi := Pos('\', Str);
      if Posi > 0 then
      begin
        Delete(Str, Posi, Length(Str));
        EditorStyles.Add(Str);
      end;
    end;
    EditorStyles.Sorted := False;
    EditorStyles.Insert(0, 'Default');
    CBEditorStyles.Items.Assign(EditorStyles);
  finally
    FreeAndNil(IniFile);
    FreeAndNil(StringList);
    FreeAndNil(EditorStyles);
  end;
end;

function TFConfiguration.ExplorerTest: Boolean;
begin
  try
    var
    Web := TWebBrowser.Create(Self);
    try
      Result := Assigned(Web);
    finally
      FreeAndNil(Web);
    end;
  except
    Result := False;
  end;
end;

procedure TFConfiguration.OpenAndShowPage(const Page: string);
begin
  // due to visible/modal-exception
  if not Visible then
  begin
    PrepareShow;
    TThread.ForceQueue(nil,
      procedure
      begin
        TVConfiguration.Selected := GetTVConfigurationItem(_(Page));
      end);
    ShowModal;
  end;
end;

procedure TFConfiguration.StyleSelectorShow;
begin
  if LBStyleNames.Items.Count = 0 then
    FillVclStylesList;
  if LBStyleNames.Items.Count > 0 then
  begin
    var
    Index := LBStyleNames.Items.IndexOf(TStyleManager.ActiveStyle.Name);
    if Index >= 0 then
      LBStyleNames.Selected[Index] := True
    else
      LBStyleNames.Selected[0] := True;
  end;
  LBStyleNamesClick(Self);
end;

procedure TFConfiguration.FillVclStylesList;
var
  Filename: string;
  StyleInfo: TStyleInfo;
begin
  // First add resource styles
  LBStyleNames.Items.AddStrings(TStyleManager.StyleNames);
  try
    for Filename in TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) + 'styles',
      '*.vsf') do
      if TStyleManager.IsValidStyle(Filename, StyleInfo) and
        (LBStyleNames.Items.IndexOf(StyleInfo.Name) < 0) then
      begin
        LBStyleNames.Items.AddObject(StyleInfo.Name, TObject(1));
        FExternalStyleFilesDict.Add(StyleInfo.Name, Filename);
      end;
  except
    on E: Exception do
      Log('FillVclStylesList error!', E);
  end;
end;

type
  TVclStylesPreviewClass = class(TVclStylesPreview);

procedure TFConfiguration.LBStyleNamesClick(Sender: TObject);
var
  LStyle: TCustomStyleServices;
  Filename: string;
  StyleName: string;
begin
  LStyle := nil;
  if LBStyleNames.ItemIndex >= 0 then
  begin
    StyleName := LBStyleNames.Items[LBStyleNames.ItemIndex];
    if Integer(LBStyleNames.Items.Objects[LBStyleNames.ItemIndex]) = 1 then
    begin
      Filename := FExternalStyleFilesDict[StyleName];
      TStyleManager.LoadFromFile(Filename);
      LStyle := TStyleManager.Style[StyleName];
      FLoadedStylesDict.Add(StyleName, Filename);
      LBStyleNames.Items.Objects[LBStyleNames.ItemIndex] := nil;
    end
    else
      LStyle := TStyleManager.Style[StyleName];
  end;

  if Assigned(LStyle) then
  begin
    FPreview.Caption := StyleName;
    FPreview.Style := LStyle;
    TVclStylesPreviewClass(FPreview).Paint;
  end;
end;

procedure TFConfiguration.Log(const Str: string; E: Exception = nil);
var
  Writer: TStreamWriter;
  DateTimeStr: string;
  FS: TFormatSettings;
begin
  if not FConfiguration.FLogfileExceptionsOK then
    Exit;

  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';   // eindeutiges Format
  FS.TimeSeparator := ':';
  FS.DateSeparator := '-';

  DateTimeStr := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now, FS);

  Writer := TStreamWriter.Create(FConfiguration.FLogfileExceptions, True, TEncoding.UTF8);
  try
    Writer.WriteLine(DateTimeStr + ' ' + GetComputerNetName +
      ' Version: ' + UDlgAbout.Version);

    if Assigned(E) then
    begin
      Writer.WriteLine('>>> Log silent Exception: ' + Str);
      Writer.WriteLine('Exception class: ' + E.ClassName);
      Writer.WriteLine('E.Message: ' + E.Message);
    end
    else
      Writer.WriteLine('>>> Log silent: ' + Str);
  finally
    Writer.Free;
  end;
end;


procedure TFConfiguration.SetupLanguages;
var
  English, Language: string;
  HaveLang: Boolean;
begin
  HaveLang := False;
  RGLanguages.Items.Clear;
  FLanguagesList := TStringList.Create;
  FLanguageCode := GetCurrentLanguage;
  DefaultInstance.bindtextdomainToFile('languagecodes',
    ExtractFilePath(Application.ExeName) + 'locale\languagecodes.mo');
  DefaultInstance.GetListOfLanguages('Default', FLanguagesList);
  FLanguagesList.Insert(0, 'en');
  for var I := 0 to FLanguagesList.Count - 1 do
  begin
    // Translate the language code to English language name and then to a localized language name
    English := dgettext('languagecodes', FLanguagesList[I]);
    Language := dgettext('languages', English);
    if (I = 0) and (Pos('Englis', Language) = 0) then
      Language := Language + ' (English)';
    RGLanguages.Items.Add(Language);
    if FLanguagesList[I] = FLanguageCode then
      HaveLang := True;
  end;
  if not HaveLang then
    RGLanguages.ItemIndex := 0;
end;

procedure TFConfiguration.UpdateHeaderFooter;
begin
  FHeader := ReplaceText(FHeader, 'Datei', 'FILE');
  FHeader := ReplaceText(FHeader, 'Pfad', 'PATH');
  FHeader := ReplaceText(FHeader, 'Datum', 'DATE');
  FHeader := ReplaceText(FHeader, 'Seite', 'PAGENUM');
  FHeader := ReplaceText(FHeader, 'Seitenzahl', 'PAGECOUNT');
  FHeader := ReplaceText(FHeader, '%PAGE%', '$PAGENUM%');

  FFooter := ReplaceText(FFooter, 'Datei', 'FILE');
  FFooter := ReplaceText(FFooter, 'Pfad', 'PATH');
  FFooter := ReplaceText(FFooter, 'Datum', 'DATE');
  FFooter := ReplaceText(FFooter, 'Seite', 'PAGENUM');
  FFooter := ReplaceText(FFooter, 'Seitenzahl', 'PAGECOUNT');
  FFooter := ReplaceText(FFooter, '%PAGE%', '$PAGENUM%');
end;

procedure TFConfiguration.InitTreeView;
begin
  FLNGTVItems.Clear;
  FLNGTVItems.Add(_('Java'));
  FLNGTVItems.Add(_('Interpreter'));
  FLNGTVItems.Add(_('Compiler'));
  FLNGTVItems.Add(_('Programs'));
  FLNGTVItems.Add(_('Applets'));
  FLNGTVItems.Add(_('Disassembler'));
  FLNGTVItems.Add(_('Jar'));
  FLNGTVItems.Add(_('Editor'));
  FLNGTVItems.Add(_('Options'));
  FLNGTVItems.Add(_('Code'));
  FLNGTVItems.Add(_('Colors'));
  FLNGTVItems.Add(_('Comment'));
  FLNGTVItems.Add(_('Templates'));
  FLNGTVItems.Add(_('Keyboard'));
  FLNGTVItems.Add(_('GUI designer'));
  FLNGTVItems.Add(_('Structograms'));
  FLNGTVItems.Add(_('Sequencediagrams'));
  FLNGTVItems.Add(_('Browser'));
  FLNGTVItems.Add(_('Documentation'));
  FLNGTVItems.Add(_('Printer'));
  FLNGTVItems.Add(_('Mindstorms'));
  FLNGTVItems.Add(_('Android'));
  FLNGTVItems.Add('Language');
  FLNGTVItems.Add(_('Options'));
  FLNGTVItems.Add(_('Styles'));
  FLNGTVItems.Add(_('Restrictions'));
  FLNGTVItems.Add(_('Associations'));
  FLNGTVItems.Add(_('UML'));
  FLNGTVItems.Add(_('UML options'));
  FLNGTVItems.Add(_('LLM Assistant'));
  FLNGTVItems.Add(_('LLM Chat'));
  FLNGTVItems.Add(_('Visibility'));
  FLNGTVItems.Add(_('Log files'));
  FLNGTVItems.Add(_('Tools'));
  FLNGTVItems.Add(_('Git'));
  FLNGTVItems.Add(_('JUnit'));
  FLNGTVItems.Add(_('Checkstyle'));
  FLNGTVItems.Add(_('Jalopy'));
  FLNGTVItems.Add(_('Subversion'));
  for var I := 0 to TVConfiguration.Items.Count - 1 do
    TVConfiguration.Items[I].Text := FLNGTVItems[I];
end;

function TFConfiguration.GlobalFileExists(var Filename: string;
WithoutChange: Boolean = True): Boolean;
var
  Str, Cachename, FilenameSik: string;
  Posi: Integer;
begin
  if FileExists(Filename) then
    Exit(True);
  if Filename = '' then
    Exit(False);
  FilenameSik := Filename;
  if IsHTTP(Filename) then
  begin
    Filename := HttpToWeb(Filename);
    Str := StripHttpParams(Filename);
    Str := StripHttp(Filename);
    Posi := Pos('/', Str);
    Cachename := Copy(Str, Posi, Length(Str));
    if Cachename = '/' then
      Cachename := Cachename + 'index.html';
    Cachename := ToWindows(FConfiguration.FJavaCache + Cachename);
    ForceDirectories(ExtractFilePath(Cachename));
    with TFDownload.Create(Self) do
    begin
      GetInetFile(Filename, Cachename, ProgressBar); // Cache oder Internet
      Free;
    end;
  end
  else if IsCHM(Filename) then
  begin
    Posi := Pos('.chm\', Filename);
    Cachename := FConfiguration.FJavaCache + '\' + Copy(Filename, Posi + 5,
      Length(Filename));
    if not FileExists(Cachename) then
    begin
      ForceDirectories(ExtractFilePath(Cachename));
      var
      StringList := TStringList.Create;
      StringList.Text := LoadFromCHM(Filename);
      StringList.SaveToFile(Cachename);
      FreeAndNil(StringList);
    end;
  end
  else
    Cachename := Filename;
  Filename := Cachename;
  Result := FileExists(Filename);
  if WithoutChange then
    Filename := FilenameSik;
end;

function TFConfiguration.ExtractZipToDir(const Filename, Dir: string): Boolean;
begin
  Result := False;
  var
  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(Filename, zmRead);
    try
      ZipFile.ExtractAll(Dir);
      Result := True;
    finally
      FreeAndNil(ZipFile);
    end;
  except
    on E: Exception do
      ErrorMsg('Error in ExtractZipToDir! ' + Format(LNGCanNotOpen, [Filename])
        + ' Error: ' + E.Message);
  end;
end;

procedure TFConfiguration.ReadProviders(const Name: string;
var Providers: TLLMProviders);

  procedure ReadProvider(ID: string; var Provider: TLLMSettings);
  begin
    var
    Key := Name + '\' + ID;
    Provider.EndPoint := ReadStringU(Key, 'EndPoint', Provider.EndPoint);
    Provider.ApiKey := Obfuscate(ReadStringU(Key, 'ApiKey', Provider.ApiKey));
    Provider.Model := ReadStringU(Key, 'Model', Provider.Model);
    Provider.SystemPrompt := ReadStringU(Key, 'SystemPrompt',
      Provider.SystemPrompt);
    Provider.TimeOut := ReadIntegerU(Key, 'TimeOut', Provider.TimeOut);
    Provider.MaxTokens := ReadIntegerU(Key, 'MaxTokens', Provider.MaxTokens);
    Provider.Temperature := StrToFloatDef(ReadStringU(Key, 'Temperature',
      FloatToStr(Provider.Temperature)), Provider.Temperature);
  end;

begin
  Providers.Provider := TLLMProvider(ReadIntegerU(Name, 'Provider',
    Integer(Providers.Provider)));
  ReadProvider('OpenAI', Providers.OpenAI);
  ReadProvider('Gemini', Providers.Gemini);
  ReadProvider('Ollama', Providers.Ollama);
  ReadProvider('DeepSeek', Providers.DeepSeek);
  ReadProvider('Grok', Providers.Grok);
end;

procedure TFConfiguration.WriteProviders(const Name: string;
Providers: TLLMProviders);

  procedure WriteProvider(ID: string; Provider: TLLMSettings);
  begin
    var
    Key := Name + '\' + ID;
    WriteStringU(Key, 'EndPoint', Provider.EndPoint);
    WriteStringU(Key, 'ApiKey', Obfuscate(Provider.ApiKey));
    WriteStringU(Key, 'Model', Provider.Model);
    WriteStringU(Key, 'SystemPrompt', Provider.SystemPrompt);
    WriteIntegerU(Key, 'TimeOut', Provider.TimeOut);
    WriteIntegerU(Key, 'MaxTokens', Provider.MaxTokens);
    WriteStringU(Key, 'Temperature', Provider.Temperature.ToString);
  end;

begin
  WriteIntegerU(Name, 'Provider', Integer(Providers.Provider));
  WriteProvider('OpenAI', Providers.OpenAI);
  WriteProvider('Gemini', Providers.Gemini);
  WriteProvider('Ollama', Providers.Ollama);
  WriteProvider('DeepSeek', Providers.DeepSeek);
  WriteProvider('Grok', Providers.Grok);
end;

procedure TFConfiguration.CopyProviders(From: TLLMProviders;
var Toward: TLLMProviders);

  procedure CopyProvider(From: TLLMSettings; var Toward: TLLMSettings);
  begin
    Toward.EndPoint := From.EndPoint;
    Toward.ApiKey := From.ApiKey;
    Toward.Model := From.Model;
    Toward.SystemPrompt := From.SystemPrompt;
    Toward.TimeOut := From.TimeOut;
    Toward.MaxTokens := From.MaxTokens;
    Toward.Temperature := From.Temperature;
  end;

begin
  Toward.Provider := From.Provider;
  CopyProvider(From.OpenAI, Toward.OpenAI);
  CopyProvider(From.Gemini, Toward.Gemini);
  CopyProvider(From.Ollama, Toward.Ollama);
  CopyProvider(From.DeepSeek, Toward.DeepSeek);
  CopyProvider(From.Grok, Toward.Grok);
end;

procedure TFConfiguration.LLMAssistantModelToView(Settings: TLLMSettings);
begin
  EEndPoint.Text := Settings.EndPoint;
  EModel.Text := Settings.Model;
  EAPIKey.Text := Settings.ApiKey;
  ESystemPrompt.Text := Settings.SystemPrompt;
  EMaxTokens.Text := Settings.MaxTokens.ToString;
  ELLMTimeout.Text := (Settings.TimeOut div 1000).ToString;
  ELLMTemperature.Text := LeftStr(Settings.Temperature.ToString, 5);
end;

procedure TFConfiguration.LLMAssistantViewToModel;
var
  Settings: TLLMSettings;
  Value: Integer;
begin
  FTempProviders.Provider := TLLMProvider(CBProvider.ItemIndex);
  Settings.EndPoint := EEndPoint.Text;
  Settings.Model := EModel.Text;
  Settings.ApiKey := EAPIKey.Text;
  Settings.SystemPrompt := ESystemPrompt.Text;
  if not TryStrToInt(EMaxTokens.Text, Value) then
    Value := 1000;
  Settings.MaxTokens := Value;
  if not TryStrToInt(ELLMTimeout.Text, Value) then
    Value := 20;
  Settings.TimeOut := Value * 1000;
  Settings.Temperature := StringToSingle(ELLMTemperature.Text);
  case CBProvider.ItemIndex of
    0:
      FTempProviders.OpenAI := Settings;
    1:
      FTempProviders.Gemini := Settings;
    2:
      FTempProviders.Ollama := Settings;
    3:
      FTempProviders.DeepSeek := Settings;
    4:
      FTempProviders.Grok := Settings;
  end;
end;

procedure TFConfiguration.LLMChatModelToView(Settings: TLLMSettings);
begin
  EChatEndPoint.Text := Settings.EndPoint;
  EChatModel.Text := Settings.Model;
  EChatApiKey.Text := Settings.ApiKey;
  EChatSystemPrompt.Text := Settings.SystemPrompt;
  EChatMaxTokens.Text := Settings.MaxTokens.ToString;
  EChatTimeout.Text := (Settings.TimeOut div 1000).ToString;
  EChatTemperature.Text := Settings.Temperature.ToString;
end;

function TFConfiguration.LLMChatSettings: TLLMSettings;
begin
  case CBChatProvider.ItemIndex of
    0:
      Result := FTempChatProviders.OpenAI;
    1:
      Result := FTempChatProviders.Gemini;
    2:
      Result := FTempChatProviders.Ollama;
    3:
      Result := FTempChatProviders.DeepSeek;
    4:
      Result := FTempChatProviders.Grok;
  end;
end;

function TFConfiguration.LLMAssistantSettings: TLLMSettings;
begin
  case CBProvider.ItemIndex of
    0:
      Result := FTempProviders.OpenAI;
    1:
      Result := FTempProviders.Gemini;
    2:
      Result := FTempProviders.Ollama;
    3:
      Result := FTempProviders.DeepSeek;
    4:
      Result := FTempProviders.Grok;
  end;
end;

procedure TFConfiguration.BLLMAssistantDefaultClick(Sender: TObject);
begin
  var
  Settings := LLMAssistant.DefaultAssistantSettings(CBProvider.ItemIndex);
  Settings.ApiKey := LLMAssistantSettings.ApiKey;
  LLMAssistantModelToView(Settings);
end;

procedure TFConfiguration.BLLMChatDefaultClick(Sender: TObject);
begin
  var
  Settings := FLLMChatForm.LLMChat.DefaultChatSettings
    (CBChatProvider.ItemIndex);
  Settings.ApiKey := LLMChatSettings.ApiKey;
  LLMChatModelToView(Settings);
end;

procedure TFConfiguration.LLMChatViewToModel;
var
  Settings: TLLMSettings;
  Value: Integer;
begin
  FTempChatProviders.Provider := TLLMProvider(CBChatProvider.ItemIndex);
  Settings.EndPoint := EChatEndPoint.Text;
  Settings.Model := EChatModel.Text;
  Settings.ApiKey := EChatApiKey.Text;
  Settings.SystemPrompt := EChatSystemPrompt.Text;
  if not TryStrToInt(EChatMaxTokens.Text, Value) then
    Value := 1000;
  Settings.MaxTokens := Value;
  if not TryStrToInt(EChatTimeout.Text, Value) then
    Value := 20;
  Settings.TimeOut := Value * 1000;
  Settings.Temperature := StringToSingle(EChatTemperature.Text);
  case CBChatProvider.ItemIndex of
    0:
      FTempChatProviders.OpenAI := Settings;
    1:
      FTempChatProviders.Gemini := Settings;
    2:
      FTempChatProviders.Ollama := Settings;
    3:
      FTempChatProviders.DeepSeek := Settings;
    4:
      FTempChatProviders.Grok := Settings;
  end;
end;

initialization

TStyleManager.Engine.RegisterStyleHook(TEdit, TEditStyleHookColor);

finalization

TStyleManager.Engine.UnRegisterStyleHook(TEdit, TEditStyleHookColor);

end.
