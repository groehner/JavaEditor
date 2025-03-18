unit UConfiguration;
{

We have configuration-values
a) in the registry or in ini-files (JEMachine.INI, JEUser.INI)
b) in variables (Model)
c) in form (View)

           RegistryToModel               ModelToView
registry  ----------------->  model    -------------> view
INI-files <----------------- variables <------------- gui-elements
           ModelToRegistry               ViewToModel

}

interface

uses
  Windows, Messages, Menus, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, Registry, ExtCtrls, IniFiles,
  System.Generics.Collections,
  SynEditHighlighter, SynHighlighterHtml, SynHighlighterJava,
  SynHighlighterMulti, SynHighlighterPhp, SynHighlighterCSS,
  SynHighlighterPas, SynHighlighterGeneral,
  UTree, UStyles, SpTBXItem, ULLMSupport, ULLMChatForm;

const
  CrLf = #13#10;
  Homepage = 'https://www.javaeditor.org';
  MaxTab = 8;
  MaxTabItem = 22;

type
  TBoolArray = array of boolean;

  TEditStyleHookColor = class(TEditStyleHook)  // TEditStyleHook in StdCtrls
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
    {$WARNINGS OFF}
    FolderDialog: TFileOpenDialog;
    {$WARNINGS ON}
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
    {$WARNINGS OFF}
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
    CBUseAbstract: TCheckBox;
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
    {$WARNINGS ON}
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
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
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
    MyRegistry: TRegistry;
    FPreview: TVclStylesPreview;
    ExternalStyleFilesDict :  TDictionary<string, string>;
    LoadedStylesDict :  TDictionary<string, string>;
    fLanguagesList: TStringList;
    VisSelectedTab: integer;
    // tab LLM assistant
    TempProviders: TLLMProviders;
    TempChatProviders: TLLMProviders;
    function getWriteProtection: Boolean;
    function getCheckColor(s: string; emptyAllowed: boolean): TColor;
    function DirectoryFilesExists(s: string): boolean;
    procedure CheckFile(WinControl: TWinControl; emptyAllowed: boolean);
    procedure CheckFileWithoutShortenpath(WinControl: TWinControl; emptyAllowed: boolean);
    procedure CheckFolder(Edit: TEdit; emptyAllowed: boolean);
    procedure CheckFolders(Edit: TEdit);
    procedure CheckFolderCB(ComboBox: TComboBox);
    procedure CheckUserFolder(Edit: TEdit);
    procedure CheckUserFolders(Edit: TEdit);
    procedure CheckCBManual;
    procedure CheckCBManualFX;
    procedure SetJDKFolder(Dir: string);
    function JavaDevelopmentKit: string;
    procedure SetStartDir(const dir, afile, _filter: string);
    procedure RegisterJavaeditor;
    procedure RegistryForMachine;
    procedure RegistryForUser;
    function BrowserProgToName(const s: string): string;
    procedure MenuAndEditorShortcuts;
    procedure CreateBrowserShortCuts;
    function LNGColornameToAttributIndex(const s: string): integer;
    procedure SetDefaultBracketColor;
    procedure StyleSelectorShow;
    procedure FillVclStylesList;
    procedure DecideProxy;
    function JavaDocComment(Indent: string = ''): string;
    function ShortComment(Indent: string = ''): string;
    function getFileInCHM(s: string): string;
    function getJavaManualFX: string;
    procedure UpdateHeaderFooter;
    procedure ReadProviders(Name: string; var Providers: TLLMProviders);
    procedure WriteProviders(Name: string; Providers: TLLMProviders);
    procedure CopyProviders(From: TLLMProviders; var Toward: TLLMProviders);
    procedure LLMAssistantModelToView(Settings: TLLMSettings);
    procedure LLMChatModelToView(Settings: TLLMSettings);
    procedure LLMAssistantViewToModel;
    procedure LLMChatViewToModel;
    function LLMAssistantSettings: TLLMSettings;
    function LLMChatSettings: TLLMSettings;
  public
    ODSelect: TOpenDialog;
    LNGTVItems: TStringList;
    HomeDir: string;       // home-directory of the user
    Sourcepath: string;
    EditorFolder: string;
    MachineIniFile: TMemIniFile;
    // with TMemIniFile we get a strange error when opening a second Explorer-Window at:
    // PLeft.Width:= FConfiguration.ReadIntegerU('Explorer', 'PLeft.Width', 200);
    UserIniFile: TIniFile;

    DumpIniFileHKCU: TMemIniFile;
    DumpIniFileAllUsers: TMemIniFile;
    DumpIniFileHKLM: TMemIniFile;

    AllClasses: TStringList;
    AllInterfaces: TStringList;
    AllPackages: TStringList;
    AllClasspathClasses: TStringList;

    DumpMode: boolean;
    UseRegistry: boolean;
    WriteProtection: boolean;
    KeyboardShortcutsTree: TTree;
    EditorAndMenuShortcuts: TTree;
    MenuKeys:  TStringList;
    EditorKeys: TStringList;
    AllKeys: TStringList;
    PortableApplication: boolean;
    PortAppDrive: string;
    EditFont: TFont;
    StructogramFont: TFont;
    UMLFont: TFont;
    SequenceFont: TFont;
    FirstStartAfterInstallation: boolean;
    DiShowParameter: integer;
    DiVisibilityFilter: integer;
    DiSortOrder: integer;
    DiShowIcons: integer;
    JavaVersion: integer;
    DocumentationVersion: integer;
    ShowPublicOnly: boolean;
    DefaultModifiers: boolean;
    ShowObjectsWithMethods: boolean;
    ShowObjectsWithInheritedPrivateAttributes: boolean;
    AttributesAParametersP: boolean;
    ShowAlways: boolean;
    ImportCache: TStringList;     // opening of classes outside of an uml-window
    SavedClasspathUser: string;
    SavedInterpreterParameter: string;
    SavedCompilerParameter: string;
    SavedJavaStartClass: string;
    WindowStateMaximized: boolean;
    FixImports: boolean;
    RightDockPanelWidth: integer;
    BottomDockPanelHeight: integer;
    Indent1, Indent2, Indent3: string;

    // tab Interpreter
    JDKFolder: string;
    JDKFolderItems: string;
    JavaInterpreter: string;
    JavaInterpreterParameter: string;
    JavaFXFolder: string;
    JavaFXParameter: string;
    JavaClasspathAdmin: string;
    JavaClasspathUser: string;
    JavaClasspathAll: string;
    JavaStartClass: string;
    JavaStartKlasseIsApplet: boolean;
    ShowInterpreterCall: Boolean;
    FileEncoding: string;
    Codepage: string;

    // tab Compiler
    JavaCompiler: string;
    JavaCompilerParameter: string;
    JavaCompilerOK: Boolean;
    CompileInternally: Boolean;
    ShowCompilerCall: Boolean;
    CompilerEncoding: string;

    // tab programs
    JavaDocParameter: string;
    JavaDisassemblerParameter: string;
    JavaJarParameter: string;
    JavaJarManifest: string;
    JarCreateCurrent: string;
    JarCreateAll: string;
    JarPackFiles: string;
    JarClassPath: string;
    JavaJar: string;
    JavaDisassembler: string;
    JavaDisassemblerItems: string;
    JavaDebugger: string;
    JavaDoc: string;
    JavaAppletviewerOK: boolean;
    JavaDebuggerOK: boolean;
    JavaDocOK: boolean;
    JavaJarOK: boolean;

    // tab Applet
    JavaAppletviewer: string;
    AppletStart: Integer;
    ShowHTMLforApplet: boolean;

    TempDir: string;
    TempDirWithUsername: string;

    // tab Mindstorms
    LejosVerzeichnis: string;
    RCXFolder: string;
    NXTFolder: string;
    EV3Folder: string;

    LejosCompiler: string;
    RCXCompiler: string;
    NXTCompiler: string;

    LejosUploader: string;
    RCXUploader: string;
    NXTUploader: string;

    LejosFlasher: string;
    RCXFlasher: string;
    NXTFlasher: string;

    MindstormsManual: string;
    RCXManual: string;
    NXTManual: string;
    EV3Manual: string;

    MindstormsParameter: string;
    MindstormsPort: integer;
    MindstormsIP: string;
    MindstormsMode: boolean;
    MindstormsVersion: integer;
    MindstormsVerbose: boolean;
    MindstormsDebug: boolean;
    MindstormsRun: boolean;

    //tab Android
    AndroidMode: boolean;
    AndroidSDKFolder: string;

    // tab Templates
    Templates: array[1..12] of string;
    ControlStructureTemplates: array[1..21] of TStringList;

    // tab keyboard
    KeyboardFile: string;

    // tab documentation
    JavaManual: string;
    JavaManualFX: string;
    JavaManualItems: string;
    JavaManualFXItems: string;
    JavaCHMRoot: string;
    CHMRootOk: boolean;
    JavaJavaDocs: string;     // active JavaDocs
    JavaJavaDocsAll: string;  // active and inactive JavaDocs
    JavaTutorial: string;
    Javabook: string;
    JavaCache: string;
    JavaCacheWithUsername: string;
    JavaTools: string;
    JavaDemos: string;
    MaxSearch: Integer;
    AllDocumentations: TStringList;
    informed: boolean;

    // tab GUI Designer
    NameFromText: boolean;
    GuiDesignerHints: Boolean;
    SnapToGrid: boolean;
    GridSize: Integer;
    ZoomSteps: Integer;
    GUIFontSize: integer;
    GUIFontName: string;
    FrameWidth: Integer;
    FrameHeight: Integer;
    AppletWidth: Integer;
    AppletHeight: Integer;

    // tab options
    UseInterpreterWindowAsConsole: Boolean;
    AcceptDefaultname: Boolean;
    RenameWhenSave: Boolean;
    DebuggerProtocol: Boolean;
    TranslateCompilerErrors: boolean;
    StrictJavaMode: Boolean;
    Fontsize: Integer;
    MaxFileHistory: Integer;
    FileFilter: string;
    CheckAge: boolean;
    ComponentsToolbar: boolean;
    BorderLayout: boolean;
    EditorStyle: string;

    // tab restrictions
    DOSWindowLocked: Boolean;
    BlockedInternet: Boolean;
    LockedPaths: boolean;
    LockedStructogram: boolean;

    // tab Associations
    AdditionalAssociations: string;

    // tab UML
    ValidClassColor: integer;
    InvalidClassColor: integer;
    ClassHead: integer;
    ObjectColor: integer;
    ObjectHead: integer;
    ObjectFooter: integer;
    ObjectCaption: integer;
    ObjectUnderline: boolean;
    CommentColor: integer;

    // tab uml options
    PrivateAttributEditable: Boolean;
    ShowEmptyRects: Boolean;
    ShowFunctionvalues: Boolean;
    ConstructorWithVisibility: Boolean;
    ObjectLowerCaseLetter: Boolean;
    ShadowWidth: integer;
    ShadowIntensity: integer;
    UseVoid: boolean;
    IntegerInsteadofInt: boolean;
    ShowAllNewObjects: boolean;
    ObjectsWithoutVisibility: boolean;
    ArrayListAsIntegratedList: boolean;
    RelationshipAttributesBold: boolean;
    StartWithDatatype: boolean;
    SetterWithoutThis: boolean;
    ShowClassparameterSeparately: boolean;
    RoleHidesAttribute: boolean;
    UseAbstract: boolean;

    // tab Editor
    Indent: Integer;
    TabWidth: Integer;
    CommentClosingBrackets: boolean;
    StructureColoring: boolean;
    StructureColoringPlane: boolean;
    StructureColorIntensity: integer;
    GUICodeFolding: boolean;
    EightyColumnLine: boolean;
    InsertControlStructures: boolean;
    InsertSemicolons: boolean;
    AutomaticIndent: Boolean;
    IndentHelp: Boolean;
    CursorBehindLine: Boolean;
    LineNumbering: Boolean;
    AddClosingBracket: Boolean;
    IndentAfterBracket: Boolean;
    ShowBracketPair: Boolean;
    CreateBAKFiles: Boolean;
    LoadFiles: Boolean;

    // Code
    CodeCompletionAlways: boolean;
    CodeCompletionCtrlSpace: boolean;
    ParameterHints: boolean;
    ShowClassObject: boolean;
    CodeDelay: integer;
    SelectionSizeMin: integer;
    SelectionSizeMax: integer;
    TooltipAutomatic: boolean;
    TooltipWithKey: boolean;
    TooltipDelay: integer;
    TooltipWidth: integer;
    TooltipHeight: integer;
    TooltipFontSize: integer;

    // tab colors
    NoActiveLineColor: boolean;
    ActiveLineColor: TColor;
    NoSyntaxHighlighting: boolean;

    // tab Checkstyle
    Checkstyle: string;
    CheckKonfiguration: string;
    CheckParameter: string;
    CheckstyleOK: boolean;

    // tab Jalopy
    Jalopy: string;
    JalopyConfiguration: string;
    JalopyParameter: string;
    JalopyOK: boolean;

    // tab Comment
    JavaAuthor: string;
    FreeComment: string;
    CommentKind: Integer;
    MethodComment: string;

    // tab Printer
    BorderLeft, BorderTop, BorderRight, BorderBottom: Integer;
    Header, Footer: string;
    WithLinenumbers,
    LinenumbersInMargin,
    PrintColored: Boolean;

    // tab Browser
    UseIEinternForDocuments: Boolean;
    OnlyOneBrowserWindow: Boolean;
    BrowserTitle   : string;
    BrowserOpenKeys : string;
    BrowserProgram: string;
    withProxy: boolean;
    ProxyIP: string;
    ProxyPort: Integer;

    // tab colors
    AttrBrackets: TSynHighlighterAttributes;

    // tab SVN
    SVNFolder: string;
    SVNRepository: string;
    SubversionOK: boolean;

    // tab Git
    GitFolder: string;
    GitLocalRepository: string;
    GitRemoteRepository: string;
    GitUserName: string;
    GitUserEMail: string;
    GitOK: boolean;

    // tab JUnit
    JUnitOk: boolean;
    JUnitJarFile: string;
    JUnitManual: string;
    JUnitParameter: string;
    JUnitBeforeEach: boolean;
    JUnitAfterEach: boolean;

    // tab Visibility
    vis1: array[0..maxTab, 0..maxTabItem] of boolean;
    vis2: array[0..maxTab, 0..maxTabItem] of boolean;
    visTabs:     TBoolArray;
    visMenus:    TBoolArray;
    visToolbars: TBoolArray;

    // tab Logfiles
    LogfileCompiler: string;
    LogfileCompilerWithUserName: string;
    LogfileCompilerOK: boolean;
    LogfileInteractive: string;
    LogfileInteractiveWithUsername: string;
    LogfileInteractiveOK: boolean;
    LogfileExceptions: string;
    LogfileExceptionsWithUsername: string;
    LogfileExceptionsOK: boolean;

    // tab Structogram
    Algorithm: string;
    Input: string;
    Output: string;
    _While: string;
    DoWhile: string;
    _for: string;
    Yes: string;
    No: string;
    Other: string;
    GenerateJavaAsProgram: integer;
    StructoDatatype: string;
    StructogramS: string;
    SwitchWithCaseLine: boolean;
    CaseCount: integer;
    StructogramShadowWidth: integer;
    StructogramShadowIntensity: integer;

    // tab Sequence diagram
    SDObject: string;
    SDNew: string;
    SDClose: string;
    SDFillingcolor: TColor;
    SDNoFilling: boolean;
    SequencediagramS: string;
    SDShowMainCall: boolean;
    SDShowParameter: boolean;
    SDShowReturn: boolean;

    // tab Language
    LanguageCode: string;

    JavaHighlighter: TSynJavaSyn;
    HTMLHighlighter: TSynHTMLSyn;
    PascalHighlighter: TSynPasSyn;
    PHPHighlighter: TSynMultiSyn;
    PHPInternHighlighter: TSynPHPSyn;
    CSSHighlighter: TSynCSSSyn;
    GeneralHighlighter: TSynGeneralSyn;
    MultiSynHighlighter: TSynMultiSyn;
    EditorTextColor: TColor;

    class var GUIStyle: string;
    procedure Init;
    procedure RegistryToModel;
    procedure ModelToRegistry;
    procedure ShowDefaultMindstormsAndroidConfiguration;
    procedure MakeJavaCache(s: string);
    procedure SaveInis;

    procedure SaveStrings(const key, aName: string; values: TStrings);
    procedure ReadStrings(const key, aName: string; values: TStrings);
    procedure SaveFavorites(Favorites: TStringList);
    procedure ReadFavorites(var Favorites: TStringList);
    procedure SaveUserColors;
    procedure ReadUserColors;
    function getRegPath: string;

    procedure ModelToView;
    procedure ViewToModel;
    procedure SaveWindow;
    function StringToShortcut(s: string): integer;
    procedure RemoveShortcutFromMainMenu(ShortCut: integer);
    procedure ReplaceShortcutFromMainMenu(ShortCut, ShortCut2: integer);
    procedure ApplyKeyboardShortcuts;
    procedure RemoveShortcutsFrom(PopupMenu: TSpTBXPopupMenu);
    procedure CheckAllFilesAndFolders;
    procedure LockButtons;
    procedure DoHelp(const afile: string);

    procedure WriteStringM(const key, aName, value: string);
    procedure WriteStringF(const key, aName, value: string);
    procedure WriteStringU(const key, aName, value: string);
    procedure WriteString(dest: integer; const key, aName, value: string);
    procedure WriteStringFile(const key, aName, value: string);
    procedure WriteStringDirectory(const key, aName, value: string);

    function  ReadStringM(const key, aName, default: string): string;
    function  ReadStringF(const key, aName, default: string): string;
    function  ReadStringU(const key, aName, default: string): string;
    function  ReadString(dest: integer; const key, aName, default: string): string;
    function  ReadStringFile(const key, aName, default: string): string;
    function  ReadStringDirectory(const key, aName: string): string;

    procedure WriteIntegerU(const key, aName: string; value: Integer);
    function  ReadIntegerU(const key, aName: string; Default: Integer): Integer;

    procedure WriteBoolM(const key, aName: string; value: Boolean);
    procedure WriteBoolU(const key, aName: string; value: Boolean);
    procedure WriteBool(machine: boolean; const key, aName: string; value: Boolean);
    function  ReadBoolM(const key, aName: string; Default: Boolean): Boolean;
    function  ReadBoolU(const key, aName: string; Default: Boolean): Boolean;
    function  ReadBool(machine: boolean; const key, aName: string; Default: Boolean): Boolean;

    procedure WriteBinaryStreamU(const key, aName: string; value: TStream);
    procedure WriteBinaryStream(machine: boolean; const key, aName: string; value: TStream);
    function ReadBinaryStreamU(const key, aName: string; value: TStream): integer;
    function ReadBinaryStream(machine: boolean; const key, aName: string; value: TStream): integer;

    procedure MakeAssociations;
    function HeadText(Indent: string= ''): string;
    function RemovePortableDrive(const s: string; folder: string = ''): string;
    function AddPortableDrive(const s: string; folder: string = ''): string;
    function AddPortableDrives(s: string): string;
    function RemovePortableDrives(s: string): string;
    function getCHMJavaManual(const s: string): string;
    function getJavaManual: string;
    function setRCXNXTEV3(const s: string): string;
    function getCharset: string;
    function getJavaVersion: integer;
    function getDocumentationVersion: integer;
    function getEncoding: string; overload;
    function getEncoding(const Pathname: string): TEncoding; overload;
    procedure ShowColorElements;
    procedure ReplaceClNone;
    procedure LoadUserColors;
    procedure LoadDefaultColors(Typ: integer);
    function getTVConfigurationItem(text: string): TTreeNode;
    procedure ShowPage(i: integer);
    procedure PrepareShow;
    function getTargetDir(const target: string): string;
    procedure FileSelect(Edit: TEdit; const aFilter, aFilename, aDir: string);
    procedure FileSelectWithoutShortenPath(Edit: TEdit; const aFilter, aFilename, aDir: string);
    procedure FileSelect2(Edit: TEdit; const aFilter, aFilename, aDir: string);
    procedure FolderSelect(Edit: TEdit; const aFoldername: string);
    function GetFileFilters: string;
    function isGerman: boolean;
    function GetConfigurationAddress(const s: string): string;
    function getJavaCompilerParameter(const Pathname: string): string;
    function getJavaInterpreterParameter(FX: boolean): string;
    function getClassPath: string; overload;
    function getClassPath(const pathname, package: string): string; overload;
    function getClassPathJarExpanded(const Pathname, Package: string): string;
    function getPackageDirectorySecure(const Pathname: string; Package: string): string;
    function getPackageDirectoryRelativ(const Pathname, Package: string): string;
    function GetPascalHighlighter: TSynPasSyn;
    function GetPhpHighlighter: TSynMultiSyn;
    function GetPHPInternHighlighter: TSynPhpSyn;
    function GetCSSHighlighter: TSynCssSyn;
    function GetMultiSynHighlighter: TSynMultiSyn;
    function GetGeneralHighlighter: TSynGeneralSyn;
    function GetHighlighter(const Pathname: string): TSynCustomHighlighter;
    function getClasspathFromSourcepath(const aClassname, aSourcepath: string): string;
    function IsInClasspath(const aClassname: string; actPath: string): boolean;
    procedure MakeClassAndPackageList(const classfile, packagefile: string);
    procedure MakeClassAndPackageListFromDocumentation(const classfile, interfacefile, packagefile: string);
    procedure MakeSystemClasses;
    procedure MakeClasspathClasses;
    function ToStringListClass(const aClassname: string): string;
    function IsAPIInterface(var cn: string): boolean;
    function IsAPIClass(var cn: string): boolean;
    function IsAPIPackage(pn: string): boolean;
    function IsAPIClassOrInterface(aClassname: string): boolean;
    procedure CallUpdater(const Target, Source1: string; Source2: string); overload;
    procedure SetElevationRequiredState(aControl: TWinControl);

    procedure LoadVisibility;
    procedure SaveVisibility;
    procedure SetVisibility;
    procedure VisibilityModelToView;
    procedure VisibilityViewToModel;
    procedure PrepareVisibilityPage;

    function GetJarsFromClasspath: string;
    function UpdatePossible(const Source, Target: string): boolean;
    function RunAsAdmin(hWnd: HWND; const aFile, aParameters: string): THandle;
    procedure SetMindstormsVersion;
    procedure CheckMindstorms;
    function GetFrameType(JavaProgramm: string; Startclass: boolean = false): integer;
    function GetPackage(JavaProgram: string; Startclass: boolean = false): string;
    function getCompilerCall(const Pathname, Package, Encoding: string): string;
    procedure SetPrinter(const s: string);
    function getDumpText: string;
    function PathForSystemClass(const Path: string): boolean;
    function PathForUserClass(const Path: string): boolean;
    function getAppletArchiv: string;
    function HasAgeClass(const Pathname: string): Boolean;
    function HasClass(const Pathname: string): Boolean;
    procedure SaveConfigurationForProject;
    procedure RestoreConfigurationAfterProject;
    procedure MakeControlStructureTemplates;
    procedure ShortenPath(WinControl: TWinControl; const s: string);
    function ExtendPath(WinControl: TWinControl): string;
    procedure ComboBoxAddEx(ComboBox: TComboBox);
    function SearchClassInClasspath(const aClassname, aSourcepath, Package: string): string;
    function SearchClassInDirectory(const aClassname, aSourcepath, Package: string): string;
    function IsInterface(const pathname: string): boolean;
    function getCompleteClassname(FullImports, Classimports, UserImportClasses: TStringList; const aClassname: string): string;
    function TranslateCompilerError(const err: string): string;
    procedure CollectDocumentations;
    function getJavaTools(const Tool: string): string;
    procedure ReadEditorStyleNames;
    function ExplorerTest: boolean;
    procedure OpenAndShowPage(Page: string);
    procedure Log(const s: string; E: Exception = nil);
    procedure SetupLanguages;
    procedure InitTreeView;
    function GlobalFileExists(var Filename: string; withoutChange: boolean = true): boolean;
    function ExtractZipToDir(const Filename, Dir: string): boolean;

    class procedure setGUIStyle;
    class procedure LoadGUIStyle(Style: string);
    class function isDark: boolean;
  end;

 var
   FConfiguration: TFConfiguration = nil;

implementation

{$R *.DFM}

uses Zip, StrUtils, Printers, ShlObj, Math, Themes, SHDocVw, ShellAPI, IOUtils,
     UITypes, JvGnugettext,
     SynUnicode, SynEditKeyCmds, TB2Item, SpTBXTabs,
     UJava, UEditorForm, UBaseForm, UUtils, UHtmlHelp,
     UDlgClasspath, UDlgAbout, UDlgDownload, UDlgJarCreate,
     USubversion, UScpHint, UImages, UJavaScanner, UGit, UCodeCompletion,
     UStringRessources, UJavaCommands;

const
  MaxPages = 36;
  machine = 0;
  allusers = 1;
  user = 2;

{--- TEditStyleHook -----------------------------------------------------------}
{--- https://theroadtodelphi.com/2012/02/06/changing-the-color-of-edit-controls-with-vcl-styles-enabled/ }

{ Set colors in TEdit's Color and Font.Color,
  Example in UMessages:
    EAppletviewer.Color:= BGColor;
    EAppletviewer.Font.Color:= FGColor;
}

constructor TEditStyleHookColor.Create(AControl: TWinControl);
begin
  inherited;
  //call the UpdateColors method to use the custom colors
  UpdateColors;
end;

//Here you set the colors of the style hook
procedure TEditStyleHookColor.UpdateColors;
begin
  if Control.Enabled then begin
    if TWinControlH(Control).Color = clRed then begin
      Brush.Color:= clRed;
      FontColor:= StyleServices.GetStyleFontColor(sfEditBoxTextDisabled); // use the Control font color
    end else
      Brush.Color:= StyleServices.GetStyleColor(scEdit); // use the Control color
    //FontColor := StyleServices.GetStyleFontColor(sfEditBoxTextDisabled); // use the Control font color
  end else begin
    // if the control is disabled use the colors of the style
    Brush.Color:= StyleServices.GetStyleColor(scEditDisabled); // scEditDisabled
    FontColor  := StyleServices.GetStyleFontColor(sfEditBoxTextDisabled);
  end;
end;

//handle the messages
procedure TEditStyleHookColor.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        //get the colors
        UpdateColors;
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
        Handled:= true;
      end;
    CM_COLORCHANGED,
    CM_ENABLEDCHANGED:
      begin
        //get the colors
        UpdateColors;
        Handled:= false;
      end
  else
    inherited WndProc(Message);
  end;
end;

function TFConfiguration.getCheckColor(s: string; emptyAllowed: boolean): TColor;
begin
  Result:= clRed;
  if s = '' then
    if emptyAllowed
      then Result:= StyleServices.getSystemColor(clWindow)
      else
  else if FileExists(dissolveUsername(s)) then
    Result:= StyleServices.getSystemColor(clWindow)
  else if IsHttp(s) and GlobalFileExists(s) then
    Result:= StyleServices.getSystemColor(clWindow);
end;

function TFConfiguration.DirectoryFilesExists(s: string): boolean;
  var p: integer; dir: string;
begin
  Result:= true;
  if s <> '' then begin
    s:= s + ';';
    p:= Pos(';', s);
    while p > 0 do begin
      dir:= Trim(Copy(s, 1, p-1));
      delete(s, 1, p);
      if endsWith(dir, '*') then delete(dir, length(dir), 1);
      if ((Copy(dir, 2, 1) = ':') or (Copy(dir, 1, 2) = '\\')) and
         not (SysUtils.DirectoryExists(dir) or FileExists(dir)) then
          Result:= false;
      p:= Pos(';', s);
    end;
  end;
end;

procedure TFConfiguration.CheckFolder(Edit: TEdit; emptyAllowed: boolean);
begin
  var s:= ExtendPath(Edit);
  ShortenPath(Edit, s);
  if Sysutils.DirectoryExists(Edit.Hint) or (s = '') and emptyAllowed
    then Edit.Color:= clWindow
    else Edit.Color:= clRed;
  Edit.Enabled:= not LockedPaths;
end;

procedure TFConfiguration.CheckFolders(Edit: TEdit);
begin
  if DirectoryFilesExists(Edit.Text)
    then Edit.Color:= clWindow
    else Edit.Color:= clRed;
  Edit.Enabled:= not LockedPaths;
end;

procedure TFConfiguration.CheckFolderCB(ComboBox: TComboBox);
begin
  var s:= ExtendPath(ComboBox);
  ShortenPath(ComboBox, s);
  if DirectoryFilesExists(s)
    then ComboBox.Color:= clWindow
    else ComboBox.Color:= clRed;
  ComboBox.Enabled:= not LockedPaths;
end;

procedure TFConfiguration.CheckUserFolder(Edit: TEdit);
begin
  var s:= ExtendPath(Edit);
  ShortenPath(Edit, s);
  if DirectoryFilesExists(dissolveUsername(Edit.Hint))
    then Edit.Color:= clWindow
    else Edit.Color:= clRed;
end;

procedure TFConfiguration.CheckUserFolders(Edit: TEdit);
begin
  if DirectoryFilesExists(dissolveUsername(Edit.Text))
    then Edit.Color:= clWindow
    else Edit.Color:= clRed;
end;

procedure TFConfiguration.CheckCBManual;
begin
  CheckFile(CBManual, true);
  if CBManual.Text <> '' then
    ComboBoxAdd(CBManual);
end;

procedure TFConfiguration.CheckCBManualFX;
begin
  CheckFile(CBManualFX, true);
  if CBManualFX.Text <> '' then
    ComboBoxAdd(CBManualFX);
end;

procedure TFConfiguration.CheckAllFilesAndFolders;
begin
  CheckFolderCB(CBJDKFolder);
  if (CBJDKFolder.Hint = '') or not FileExists(withTrailingSlash(CBJDKFolder.Hint) + 'bin\java.exe') then
    CBJDKFolder.Color:= clRed;
  CheckFile(EInterpreter, false);
  CheckFolder(EJavaFXFolder, true);
  CheckFolders(EClasspathAdmin);
  CheckUserFolders(EClasspathUser);
  CheckFile(EJavaCompiler, false);
  CheckFile(EAppletviewer, false);
  CheckFile(EDebugger, false);
  CheckFile(CBDisassembler, false);
  CheckFile(ECheckstyle, true);
  CheckFile(ECheckstyleConfiguration, true);
  CheckFile(EJalopy, true);
  CheckFile(EJalopyConfiguration, true);
  CheckFile(EJavaDoc, false);
  CheckFile(EJar, false);
  CheckFile(ETemplateConsole, true);
  CheckFile(ETemplateFrame, true);
  CheckFile(ETemplateDialog, true);
  CheckFile(ETemplateApplet, true);
  CheckFile(ETemplateJFrame, true);
  CheckFile(ETemplateJDialog, true);
  CheckFile(ETemplateJApplet, true);
  CheckFile(ETemplateApplication, true);
  CheckFile(ETemplateControlstructure, true);
  CheckFile(ETemplateClass, true);
  CheckFile(ETemplateJUnitTest, true);
  CheckMindstorms;

  CheckFolders(EAndroidSDKFolder);
  CheckCBManual;
  CheckCBManualFX;
  CheckUserFolders(EJavaDocs);
  CheckFile(ETutorial, true);
  CheckUserFolder(ECache);
  ECache.Enabled:= not LockedPaths;
  CheckFile(EJavabook, true);
  CheckUserFolder(ETempFolder);
  CheckFile(EBrowserProgram, true);
  CheckFile(EKeyboardFile, true);
  if not CBUseIEinternForDocuments.Checked and (CBOpenBrowserShortcut.Text = '')
    then CBOpenBrowserShortcut.Color:= clRed
    else CBOpenBrowserShortcut.Color:= clWindow;
  CheckFolder(ESVNFolder, true);
  CheckFolderCB(CBRepository);
  CheckFolder(ETempFolder, false);
  CheckFile(ELogfileInteractive, true);
  CheckFile(ELogfileCompiler, true);
  CheckFile(ELogfileExceptions, true);
  CheckFolder(EGitFolder, true);
  CheckFolderCB(CBLocalRepository);
  CheckFolderCB(CBRemoteRepository);
  CheckFile(EJunitJarFile, true);
  CheckFile(EJunitManual, true);
  LockButtons;
end;

procedure TFConfiguration.LockButtons;
begin
  SBJDKFolderSelect.Enabled:= not LockedPaths;
  BJDKInstall.Enabled:= not LockedPaths;
  BInterpreter.Enabled:= not LockedPaths;
  BJavaFXFolder.Enabled:= not LockedPaths;
  BClasspathAdmin.Enabled:= not LockedPaths;
  BJavaCompiler.Enabled:= not LockedPaths;
  BDebugger.Enabled:= not LockedPaths;
  BJavaDoc.Enabled:= not LockedPaths;
  BAppletViewer.Enabled:= not LockedPaths;
  BDisassemblerInstall.Enabled:= not LockedPaths;
  SBDisassemblerSelect.Enabled:= not LockedPaths;
  BJar.Enabled:= not LockedPaths;
  BSelectBrowser.Enabled:= not Lockedpaths;
  BTemplateConsole.Enabled:= not LockedPaths;
  BTemplateFrame.Enabled:= not LockedPaths;
  BTemplateDialog.Enabled:= not LockedPaths;
  BTemplateApplet.Enabled:= not LockedPaths;
  BTemplateJFrame.Enabled:= not LockedPaths;
  BTemplateJDialog.Enabled:= not LockedPaths;
  BTemplateJApplet.Enabled:= not LockedPaths;
  BTemplateApplication.Enabled:= not LockedPaths;
  BTemplateControlStructure.Enabled:= not LockedPaths;
  BTemplateClass.Enabled:= not LockedPaths;
  BTemplateJunitTest.Enabled:= not LockedPaths;
  BKeyboardFile.Enabled:= not LockedPaths;
  SBKeyboardFile.Enabled:= not LockedPaths;
  SBManualSelect.Enabled:= not LockedPaths;
  SBManualFXSelect.Enabled:= not LockedPaths;
  BManualInstall.Enabled:= not LockedPaths;
  BManualFXInstall.Enabled:= not LockedPaths;
  SBTutorialSelect.Enabled:= not LockedPaths;
  BTutorialInstall.Enabled:= not LockedPaths;
  SBJavabookSelect.Enabled:= not LockedPaths;
  BJavabookInstall.Enabled:= not LockedPaths;
  BJavaDocFolder.Enabled:= not LockedPaths;
  SBCacheSelect.Enabled:= not LockedPaths;
  BCache.Enabled:= not LockedPaths;
  SBLejosSelect.Enabled:= not LockedPaths;;
  SBLejosCompiler.Enabled:= not LockedPaths;
  SBLejosUploader.Enabled:= not LockedPaths;
  SBLejosFlasher.Enabled:= not LockedPaths;
  SBMindstormsManual.Enabled:= not LockedPaths;
  BLejosInstall.Enabled:= not LockedPaths;
  BMindstormsManual.Enabled:= not LockedPaths;
  SBMindstormsTemplate.Enabled:= not LockedPaths;
  BMindstormsTemplate.Enabled:= not LockedPaths;
  SBAndroidSDKFolder.Enabled:= not LockedPaths;
  BAndroidSDKInstall.Enabled:= not LockedPaths;
  SBTempSelect.Enabled:= not LockedPaths;
  BTempFolder.Enabled:= not LockedPaths;
  BLogfileCompiler.Enabled:= not LockedPaths;
  BLogfileInteractive.Enabled:= not LockedPaths;
  BLogfileExceptions.Enabled:= not LockedPaths;
  BGitFolder.Enabled:= not LockedPaths;
  BGitRepository.Enabled:= not LockedPaths;
  BGitClone.Enabled:= not LockedPaths;

  BJUnitInstall.Enabled:= not LockedPaths;
  SBJUnit.Enabled:= not LockedPaths;
  SBJUnitManual.Enabled:= not LockedPaths;
  SBCheckStyleSelect.Enabled:= not LockedPaths;
  BCheckStyleInstall.Enabled:= not LockedPaths;
  BCheckStyleConfiguration.Enabled:= not LockedPaths;
  SBJalopySelect.Enabled:= not LockedPaths;
  BJalopyInstall.Enabled:= not LockedPaths;
  BJalopyConfiguration.Enabled:= not LockedPaths;
  BSVN.Enabled:= not LockedPaths;
  BRepository.Enabled:= not LockedPaths;
end;

procedure TFConfiguration.SetJDKFolder(Dir: string);
begin
  ShortenPath(CBJDKFolder, Dir);
  ComboBoxAdd(CBJDKFolder);
  TidyPath(Dir + '\bin;');
  ShortenPath(EJavaCompiler, CBJDKFolder.Hint + '\bin\javac.exe');
  ShortenPath(EInterpreter, CBJDKFolder.Hint + '\bin\java.exe');
  ShortenPath(EAppletviewer, CBJDKFolder.Hint + '\bin\appletviewer.exe');
  if (Pos('jdb.exe', EDebugger.Hint) > 0) then
    ShortenPath(EDebugger, CBJDKFolder.Hint + '\bin\jdb.exe' );
  if (Pos('javap.exe', CBDisassembler.Hint) > 0) then
    ShortenPath(CBDisassembler, CBJDKFolder.Hint + '\bin\javap.exe');
  ShortenPath(EJavaDoc, CBJDKFolder.Hint + '\bin\javadoc.exe');
  ShortenPath(EJar, CBJDKFolder.Hint + '\bin\jar.exe');
  BClasspathAdminClick(Self);
  CheckAllFilesAndFolders;
end;

procedure TFConfiguration.BJDKFolderSelectClick(Sender: TObject);
begin
  var Dir:= CBJDKFolder.Hint;
  if not Sysutils.DirectoryExists(Dir) then
    Dir:= GetEnvironmentVariable('PROGRAMFILES');
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= Dir;
  if FolderDialog.Execute then begin
    Dir:= withoutTrailingSlash(FolderDialog.Filename);
    SetJDKFolder(Dir);
  end;
  {$WARNINGS ON}
end;

procedure TFConfiguration.CBJDKFolderSelect(Sender: TObject);
begin
  CheckFolderCB(CBJDKFolder);
  if (CBJDKFolder.Color <> clRed) and (CBJDKFolder.Text <> '') then
    SetJDKFolder(CBJDKFolder.Hint);
end;

function TFConfiguration.getTargetDir(const target: string): string;
begin
  if PortableApplication
    then Result:= EditorFolder + 'App\' + target
    else Result:= EditorFolder + target;
  SysUtils.ForceDirectories(Result);
  Result:= withoutTrailingSlash(Result);
end;

procedure TFConfiguration.FileSelect(Edit: TEdit; const aFilter, aFilename, aDir: string);
begin
  with ODSelect do begin
    if (Edit.Hint <> '') and FileExists(Edit.Hint)
      then InitialDir:= ExtractFilePath(Edit.Hint)
      else InitialDir:= getTargetDir(aDir);
    Filter:= aFilter + '|*.*|*.*';
    Filename:= aFilename;
    if ODSelect.Execute then ShortenPath(Edit, Filename);
  end;
end;

procedure TFConfiguration.FileSelectWithoutShortenPath(Edit: TEdit; const aFilter, aFilename, aDir: string);
begin
  with ODSelect do begin
    if FileExists(Edit.Text)
      then InitialDir:= ExtractFilePath(Edit.Text)
      else InitialDir:= getTargetDir(aDir);
    Filter:= aFilter + '|*.*|*.*';
    Filename:= aFilename;
    if ODSelect.Execute then ShortenPath(Edit, Filename);
  end;
end;

procedure TFConfiguration.BJavaFXParameterClick(Sender: TObject);
begin
  EJavaFXParameter.Text:= 'javafx.controls,javafx.media,javafx.web';
end;

procedure TFConfiguration.FileSelect2(Edit: TEdit; const aFilter, aFilename, aDir: string);
begin
  with ODSelect do begin
    InitialDir:= aDir;
    Filter:= aFilter + '|*.*|*.*';
    Filename:= aFilename;
    if ODSelect.Execute then ShortenPath(Edit, Filename);
  end;
end;

procedure TFConfiguration.BLejosInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= getTargetDir('');
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.Create(Self) do
    try
      if RGMindstormsVersion.ItemIndex = 2
        then SL:= GetDownloadFiles('LejosEV3')
        else SL:= GetDownloadFiles('Lejos');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + SL.Values['zip2']);
        ShowModal;
        if DownloadIsOK then begin
          if VistaOrBetter
            then CallUpdater(Target, source + SL.Values['zip2'], '')
            else ExtractZipToDir(source + SL.Values['zip2'], target);
          ShortenPath(ELejosFolder, target + SL.Values['file1']);
          ShortenPath(EMindstormsManual, target + SL.Values['file2']);
          if RGMindstormsVersion.ItemIndex = 1 then begin
            ShortenPath(ELejosCompiler, ELejosfolder.Hint + '\bin\nxjc.bat');
            ShortenPath(ELejosUploader, ELejosfolder.Hint + '\bin\nxj.bat');
            ShortenPath(ELejosFlasher, ELejosfolder.Hint + '\bin\nxjflashg.bat');
          end;
        end
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckMindstorms;
end;

procedure TFConfiguration.SBLejosSelectClick(Sender: TObject);
begin
  var s:= ELejosFolder.Hint;
  {$WARNINGS OFF}
  if Sysutils.DirectoryExists(s) then
    FolderDialog.DefaultFolder:= s;
  if FolderDialog.Execute then begin
    s:= withoutTrailingSlash(FolderDialog.Filename);
    ShortenPath(ELejosFolder, s);
    if Pos(ELejosFolder.Hint, EMindstormsManual.Hint) > 0 then
      ShortenPath(EMindstormsManual, ELejosFolder.Hint + '\docs\nxt\index.html');
  end;
  {$WARNINGS ON}
  CheckFile(EMindstormsManual, true);
end;

procedure TFConfiguration.SBLejosUploaderClick(Sender: TObject);
begin
  FileSelect2(ELejosUploader, '*.bat|*.bat', 'nxj.bat', LejosVerzeichnis + '\bin');
  CheckFile(ELejosUploader, true);
end;

procedure TFConfiguration.SBLejosCompilerClick(Sender: TObject);
begin
  FileSelect2(ELejosCompiler, '*.bat|*.bat', 'nxjc.bat', LejosVerzeichnis + '\bin');
  CheckFile(ELejosCompiler, true);
end;

procedure TFConfiguration.SBLejosFlasherClick(Sender: TObject);
begin
  FileSelect2(ELejosFlasher, '*.bat|*.bat', 'nxjflashg.bat', LejosVerzeichnis + '\bin');
  CheckFile(ELejosFlasher, true);
end;

procedure TFConfiguration.BMindstormsManualClick(Sender: TObject);
begin
  var s:= LejosVerzeichnis + '\projects\classes\doc';
  if not Sysutils.DirectoryExists(s) then s:= '';
  FileSelect(EMindstormsManual, '*.html|*.html', 'index.html', s);
  CheckFile(EMindstormsManual, true);
end;

procedure TFConfiguration.BSaveClick(Sender: TObject);
begin
  Close;
  if LBStyleNames.ItemIndex >= 0 then
    GUIStyle:= LBStyleNames.Items[LBStyleNames.ItemIndex];
  if not FileExists(EInterpreter.Hint) then
    ErrorMsg(Format(_('Java interpreter %s not found!'), [EInterpreter.Hint]));
  Screen.Cursor:= crHourGlass;
  try
    ViewToModel;
    ModelToRegistry;
    RegistryToModel;
  finally
    Screen.Cursor:= crDefault;
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
  with ODSelect do begin
    InitialDir:= GetEnvironmentVariable('PROGRAMFILES');
    Filename  := '*.exe';
    Filter:= '*.exe|*.exe';
    if not Sysutils.DirectoryExists(InitialDir) then InitialDir:= 'C:\';
    if Execute then
      ShortenPath(EBrowserProgram, Filename);
    CheckFile(EBrowserProgram, true);
  end;
end;

procedure TFConfiguration.BEditorFontClick(Sender: TObject);
begin
  with FJava.FDFont do begin
    Options:= [fdFixedPitchOnly];
    Font.Assign(FConfiguration.EditFont);
    if Execute then begin
      for var i:= 0 to FJava.TDIFormsList.Count - 1 do
        if FJava.TDIFormsList[i].FormTag = 1 then
          FJava.TDIFormsList[i].SetFont(Font);
      FConfiguration.EditFont.Assign(Font);
    end;
  end;
end;

procedure TFConfiguration.BEditorStyleDefaultClick(Sender: TObject);
begin
  CBEditorStyles.Text:= 'Default';
  CBEditorStylesChange(Self);
end;

function TFConfiguration.BrowserProgToName(const s: string): string;
begin
  var Browser:= UpperCase(s);
  if Pos('NETSCAPE', Browser) > 0
    then Result:= 'Netscape'
  else if Pos('IEXPLORE', Browser) > 0
    then Result:= 'Internet Explorer'
  else if Pos('OPERA', Browser) > 0
    then Result:= 'Opera'
  else if Pos('FIREFOX', Browser) > 0
    then Result:= 'Mozilla Firefox'
  else if Pos('MOZILLA', Browser) > 0
    then Result:= 'Mozilla'
  else Result:= 'unknown';
end;

procedure TFConfiguration.BRunJavaClick(Sender: TObject);
begin
  var s:= TempDir + 'RunJava.bat';
  if FileExists(s) then begin
    FJava.Open(s);
    Close;
  end else
    ErrorMsg(_(LNGNoRunJavaBat));
end;

procedure TFConfiguration.BBrowserTitleClick(Sender: TObject);
begin
  ShortenPath(EBrowsertitle, BrowserProgToName(EBrowserProgram.Hint));
end;

procedure TFConfiguration.BJavaParameterClick(Sender: TObject);
begin
  EJavaCompilerParameter.Text:= '-deprecation -g';
end;

procedure TFConfiguration.BInterpreterParameterClick(Sender: TObject);
begin
  EInterpreterParameter.Text:= '';
end;

procedure TFConfiguration.BClasspathAdminClick(Sender: TObject);
begin
  EClasspathAdmin.Text:= EditorFolder + 'JEClasses.jar';
  CheckFolders(EClassPathAdmin);
end;

procedure TFConfiguration.BClasspathUserClick(Sender: TObject);
begin
  with TFClasspath.Create(Self) do begin
    Caption:= _('Edit classpath');
    BNewJarFile.Visible:= true;
    CBAllJarFiles.Visible:= true;
    BNewAllClasses.Visible:= false;
    Initialize(JavaClasspathAll, false);
    if ShowModal = mrOK then begin
      JavaClasspathAll:= ClasspathAll;
      JavaClasspathUser:= ClasspathUser;
      EClasspathUser.Text:= JavaClasspathUser;
      CheckUserFolders(EClasspathUser);
    end;
    Free;
  end;
end;

procedure TFConfiguration.BJavaDocFolderClick(Sender: TObject);
begin
  with TFClasspath.Create(Self) do begin
    Caption:= _('Edit JavaDoc User');
    BNewJarFile.Visible:= false;
    CBAllJarFiles.Visible:= false;
    BNewAllClasses.Visible:= true;
    BNewAllClasses.top:= 200;
    Initialize(JavaJavaDocsAll, true);
    if ShowModal = mrOK then begin
      JavaJavaDocsAll:= ClasspathAll;
      JavaJavaDocs:= ClasspathUser;
      EJavaDocs.Text:= JavaJavaDocs;
      CheckUserFolders(EJavaDocs);
    end;
    Free;
  end;
end;

procedure TFConfiguration.SetStartDir(const dir, afile, _filter: string);
begin
  with ODSelect do begin
    InitialDir:= CBJDKFolder.Hint + dir;
    Filename:= afile;
    Filter:= _filter + '|*.*|*.*';
    if not Sysutils.DirectoryExists(InitialDir) then InitialDir:= CBJDKFolder.Hint;
    if not Sysutils.DirectoryExists(InitialDir) then InitialDir:= 'C:\';
  end;
end;

procedure TFConfiguration.BJavaCompilerClick(Sender: TObject);
begin
  SetStartDir('\bin', 'javac.exe', '*.exe|*.exe');
  if ODSelect.Execute then
    ShortenPath(EJavaCompiler, ODSelect.Filename);
  CheckFile(EJavaCompiler, false);
end;

procedure TFConfiguration.FolderSelect(Edit: TEdit; const aFoldername: string);
begin
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= aFoldername;
  if FolderDialog.Execute then
    ShortenPath(Edit, withoutTrailingSlash(FolderDialog.Filename));
  {$WARNINGS ON}
end;

procedure TFConfiguration.BInterpreterClick(Sender: TObject);
begin
  SetStartDir('\bin', 'java.exe', '*.exe|*.exe');
  with ODSelect do
    if Execute then
      ShortenPath(EInterpreter, Filename);
  CheckFile(EInterpreter, false);
end;

procedure TFConfiguration.BLogfileCompilerClick(Sender: TObject);
begin
  with FJava.SDSaveAs do begin
    Title:= _(LNGSaveAs);
    Filter:= '*.txt|*.txt|(*.*)|*.*';
    FilterIndex:= 1;
    Filename:= ELogfileCompiler.Hint;
    if Filename = '' then Filename:=  'JELogfileCompiler.txt';
    var Path:= ExtractFilePath(Filename);
    if Path <> ''
      then InitialDir:= Path
    else if PortableApplication
      then InitialDir:= EditorFolder + 'App\Log'
      else InitialDir:= GetHomePath + '\JavaEditor\Log';
    Sysutils.ForceDirectories(InitialDir);
    if Execute then begin
      CreateMyFile(Filename);
      ShortenPath(ELogfileCompiler, Filename);
      LogfileCompiler:= Filename;
      LogfileCompilerOK:= FileExists(Filename);
    end;
  end;
end;

procedure TFConfiguration.BLogfileInteractiveClick(Sender: TObject);
begin
  with FJava.SDSaveAs do begin
    Title:= _(LNGSaveAs);
    Filter:= '*.txt|*.txt|(*.*)|*.*';
    FilterIndex:= 1;
    Filename:= ELogfileInteractive.Hint;
    if Filename = '' then Filename:=  'JELogfileInteractive.txt';
    var Path:= ExtractFilePath(Filename);
    if Path <> ''
      then InitialDir:= Path
    else if PortableApplication
      then InitialDir:= EditorFolder + 'App\Log'
      else InitialDir:= GetHomePath + '\JavaEditor\Log';
    Sysutils.ForceDirectories(InitialDir);
    if Execute then begin
      CreateMyFile(Filename);
      ShortenPath(ELogfileInteractive, Filename);
      LogfileInteractive:= Filename;
      LogfileInteractiveOK:= FileExists(Filename);
    end;
  end;
end;

procedure TFConfiguration.BLogfileExceptionsClick(Sender: TObject);
begin
  with FJava.SDSaveAs do begin
    Title:= _(LNGSaveAs);
    Filter:= '*.txt|*.txt|(*.*)|*.*';
    FilterIndex:= 1;
    Filename:= ELogfileExceptions.Hint;
    if Filename = '' then Filename:=  'JELogfileExceptions.txt';
    var Path:= ExtractFilePath(Filename);
    if Path <> ''
      then InitialDir:= Path
    else if PortableApplication
      then InitialDir:= EditorFolder + 'App\Log'
      else InitialDir:= GetHomePath + '\JavaEditor\Log';
    Sysutils.ForceDirectories(InitialDir);
    if Execute then begin
      CreateMyFile(Filename);
      ShortenPath(ELogfileExceptions, Filename);
      LogfileExceptions:= Filename;
      LogfileExceptionsOK:= FileExists(Filename);
    end;
  end;
end;

procedure TFConfiguration.BAndroidAssembleClick(Sender: TObject);
begin
  var s:= EditorFolder + 'assemble.bat';
  if FileExists(s) then begin
    FJava.Open(s);
    Close;
  end else
    ErrorMsg(_(LNGNoRunJavaBat));
end;

procedure TFConfiguration.BAndroidSDKInstallClick(Sender: TObject);
begin
  FJava.CallHelp('https://developer.android.com/studio/index.html');
end;

procedure TFConfiguration.BAppletviewerClick(Sender: TObject);
begin
  SetStartDir('\bin', 'appletviewer.exe', '*.exe|*.exe');
  with ODSelect do
    if Execute then
      ShortenPath(EAppletviewer, Filename);
  CheckFile(EAppletViewer, false);
end;

// tab Utilities

procedure TFConfiguration.BDebuggerClick(Sender: TObject);
begin
  SetStartDir('\bin', 'jdb.exe', '*.exe|*.exe');
  if ODSelect.Execute then
    ShortenPath(EDebugger, ODSelect.Filename);
  CheckFile(EDebugger, false);
end;

procedure TFConfiguration.SBDisassemblerSelectClick(Sender: TObject);
begin
  var s:= CBDisassembler.Hint;
  with ODSelect do begin
    if FileExists(s) then InitialDir:= ExtractFilePath(s);
    Filter:= '*.exe|*.exe;*.*|*.*';
    Filename:= 'javap';
    if Execute then
      ShortenPath(CBDisassembler, Filename);
  end;
end;

procedure TFConfiguration.BDisassemblerInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= getTargetDir('jad');
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('Disassembler');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + '\jad.zip');
        ShowModal;
        if DownloadIsOK then begin
          if VistaOrBetter
            then CallUpdater(Target, source + '\jad.zip', '')
            else ExtractZipToDir(source + '\jad.zip', target);
          ShortenPath(CBDisassembler, target + '\jad.exe');
          if FileExists(target + '\jad.exe') then
            ComboBoxAddEx(CBDisassembler);
        end;
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(CBDisassembler, false);
end;

procedure TFConfiguration.CBDisassemblerSelect(Sender: TObject);
begin
  ShortenPath(CBDisassembler, CBDisassembler.Text);
  BDisassemblerParameterClick(Self);
end;

procedure TFConfiguration.CBEditorStylesChange(Sender: TObject);
begin
  EditorStyle:= CBEditorStyles.Text;
  ReadUserColors;
  LBColorElementsClick(Self);
end;

procedure TFConfiguration.BDisassemblerParameterClick(Sender: TObject);
begin
  if Pos('javap.exe', CBDisassembler.Text) > 0
    then EDisassemblerParameter.Text:= '-l -c -verbose'
    else EDisassemblerParameter.Text:= '-o';
end;

procedure TFConfiguration.BJavaDocClick(Sender: TObject);
begin
  SetStartDir('\bin', 'javadoc.exe','*.exe|*.exe');
  if ODSelect.Execute then
    ShortenPath(EJavaDoc, ODSelect.Filename);
  CheckFile(EJavaDoc, false);
end;

procedure TFConfiguration.BJarClasspathClick(Sender: TObject);
begin
  EJarClasspath.Text:= 'JEClasses.jar';
end;

procedure TFConfiguration.BJarClick(Sender: TObject);
begin
  SetStartDir('\bin', 'jar.exe','*.exe|*.exe');
  if ODSelect.Execute then
    ShortenPath(EJar, ODSelect.Filename);
  CheckFile(EJar, false);
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
    SBManualSelectClick(self);
end;

procedure TFConfiguration.CBNoActiveLineColorClick(Sender: TObject);
begin
  CBActiveLineColor.Enabled:= not CBNoActiveLineColor.Checked;
end;

procedure TFConfiguration.SBManualFXSelectClick(Sender: TObject);
begin
  SetStartDir('docs', '', '*.chm;*.html|*.chm;*.html');
  if ODSelect.Execute then
    ShortenPath(CBManualFX, ODSelect.Filename);
  CheckCBManualFX;
end;

procedure TFConfiguration.SBManualSelectClick(Sender: TObject);
begin
  SetStartDir('docs', '', '*.chm;*.html|*.chm;*.html');
  if ODSelect.Execute then
    ShortenPath(CBManual, ODSelect.Filename);
  CheckCBManual;
end;

procedure TFConfiguration.BManualFXInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= JDKFolder + '\docsfx';
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('ManualFX');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + SL.Values['zip2']);
        ShowModal;
        if DownloadIsOK then begin
          if VistaOrBetter
            then CallUpdater(Target, source + SL.Values['zip2'], '')
            else ExtractZipToDir(source + SL.Values['zip2'], target);
          ShortenPath(CBManualFX, target + SL.Values['file1']);
        end;
       end
      else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckCBManualFX;
end;

procedure TFConfiguration.BManualInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= JDKFolder + '\docs';
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('Manual');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + SL.Values['zip2']);
        ShowModal;
        if DownloadIsOK then begin
          if VistaOrBetter
            then CallUpdater(Target, source + SL.Values['zip2'], '')
            else ExtractZipToDir(source + SL.Values['zip2'], target);
          ShortenPath(CBManual, target + SL.Values['file1']);
        end;
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckCBManual;
end;

procedure TFConfiguration.BTutorialInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= getTargetDir('tutorial');
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('Tutorial');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + '\tutorial.zip');
        ShowModal;
        if DownloadIsOK then begin
          if VistaOrBetter
            then CallUpdater(Target, Source + '\tutorial.zip', '')
            else ExtractZipToDir(source + '\tutorial.zip', target);
          ShortenPath(ETutorial, target + '\tutorial.chm');
        end
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(ETutorial, true);
end;

procedure TFConfiguration.BGuiFontDefaultClick(Sender: TObject);
begin
  GUIFontSize:= 12;
  GUIFontName:= 'Dialog';
end;

procedure TFConfiguration.BGuiFontClick(Sender: TObject);
begin
  with FJava.FDFont do begin
    Options:= [];
    Font.Size:= GUIFontSize;
    Font.Name:= GUIFontName;
    if Execute then begin
      GUIFontSize:= max(Font.Size, 4);
      GUIFontName:= Font.Name;
    end;
  end;
end;

procedure TFConfiguration.BFileExtensionsClick(Sender: TObject);
  var s, s1, s2: string; p: integer; b: byte;
begin
  if VistaOrBetter then begin
    s2:=      CBAssociationJava.Caption + ' ' + BoolToStr(CBAssociationJava.Checked) + ' ';
    s2:= s2 + CBAssociationJfm.Caption  + ' ' + BoolToStr(CBAssociationJfm.Checked) + ' ';
    s2:= s2 + CBAssociationUML.Caption  + ' ' + BoolToStr(CBAssociationUML.Checked) + ' ';
    s2:= s2 + CBAssociationJep.Caption  + ' ' + BoolToStr(CBAssociationJep.Checked) + ' ';
    s2:= s2 + CBAssociationHtml.Caption + ' ' + BoolToStr(CBAssociationHtml.Checked) + ' ';
    s2:= s2 + CBAssociationTxt.Caption  + ' ' + BoolToStr(CBAssociationTxt.Checked) + ' ';
    s2:= s2 + CBAssociationJsp.Caption  + ' ' + BoolToStr(CBAssociationJsp.Checked) + ' ';
    s2:= s2 + CBAssociationPhp.Caption  + ' ' + BoolToStr(CBAssociationPhp.Checked) + ' ';
    s2:= s2 + CBAssociationCss.Caption  + ' ' + BoolToStr(CBAssociationCss.Checked) + ' ';
    s2:= s2 + CBAssociationInc.Caption  + ' ' + BoolToStr(CBAssociationInc.Checked) + ' ';
    s2:= s2 + CBAssociationJsg.Caption  + ' ' + BoolToStr(CBAssociationJsg.Checked) + ' ';
    s2:= s2 + CBAssociationJsd.Caption  + ' ' + BoolToStr(CBAssociationJsd.Checked) + ' ';

    s:= EAdditionalAssociations.Text + ';';
    p:= Pos(';', s);
    while p > 0 do begin
      s1:= copy(s, 1, p-1);
      delete(s, 1, p);
      p:= Pos('.', s1);
      if p > 0 then delete(s1, 1, p);
      b:= length(s1);
      if (s1 <> '') and (b in [2, 3, 4, 5]) then s2:= s2 + '.' + s1 + ' ' + BoolToStr(true)+ ' ';
      p:= Pos(';', s);
    end;
    CallUpdater(ParamStr(0), 'registry', s2);
  end else
    MakeAssociations;
end;

procedure TFConfiguration.BJEAssociationClick(Sender: TObject);
begin
  CallUpdater(ParamStr(0), 'jeregistry',
              HideBlanks(encodeQuotationMark(EJEAssociation.Text)))
end;

procedure TFConfiguration.BJUnitInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= getTargetDir('junit');
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('JUnit');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['file1'], target + SL.Values['file2']);
        ShowModal;
        if DownloadIsOK then begin
          EJUnitJarFile.Text:= EFile.Text;
          EJUnitJarFile.Hint:= EFile.Text;
          DownloadFile(SL.Values['zip1'], source + '\junitdoc.zip');
          if VistaOrBetter
            then CallUpdater(Target + '\docs', Source + '\junitdoc.zip ', '')
            else ExtractZipToDir(source + '\junitdoc.zip', target + '\docs');
          EJUnitManual.Text:= target + '\docs\index.html';
          EJUnitManual.Hint:= target + '\docs\index.html';
        end;
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFileWithoutShortenPath(EJUnitJarFile, true);
end;

procedure TFConfiguration.SBTutorialSelectClick(Sender: TObject);
begin
  FileSelect(ETutorial, '*.chm;*.html|*.chm;*.html', '', 'tutorial');
  CheckFile(ETutorial, true);
end;

procedure TFConfiguration.BJavabookInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= getTargetDir('javabook');
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('Javabook');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + '\html.zip');
        ShowModal;
        if DownloadIsOK then begin
          DownloadFile(SL.Values['zip2'], source + '\examples.zip');
          if VistaOrBetter
            then CallUpdater(Target, Source + '\html.zip ', Source + '\examples.zip')
          else begin
            ExtractZipToDir(source + '\html.zip', target);
            ExtractZipToDir(source + '\examples.zip', target);
          end;
        ShortenPath(EJavabook, target + '\html\cover.html');
        end;
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(EJavabook, true);
end;

procedure TFConfiguration.SBJavabookSelectClick(Sender: TObject);
begin
  FileSelect(EJavabook, '*.html|*.html', 'cover.html', 'javabook');
  CheckFile(EJavabook, true);
end;

procedure TFConfiguration.SBJUnitClick(Sender: TObject);
begin
  FileSelectWithoutShortenPath(EJUnitJarFile, 'jar|*.jar', 'junit-platform-console-standalone*.jar', 'junit');
  CheckFileWithoutShortenPath(EJUnitJarFile, true);
end;

procedure TFConfiguration.SBJUnitManualClick(Sender: TObject);
begin
  SetStartDir('docs', '', 'index.html|index.html');
  if FileExists(JUnitJarFile) then
    ODSelect.InitialDir:= ExtractFilePath(JUnitJarFile);
  if ODSelect.Execute then begin
    EJUnitManual.Text:= ODSelect.Filename;
    EJUnitManual.Hint:= ODSelect.FileName;
  end;
end;

procedure TFConfiguration.RemoveShortcutFromMainMenu(ShortCut: integer);
  var i, j, k: integer;
      MenuItem, SubMenuItem, SubSubMenuItem: TTBCustomItem;
begin
  for i:= 0 to FJava.mainMenu.Items.Count - 1 do begin
    MenuItem:= FJava.mainMenu.Items[i];
    if MenuItem.ShortCut = ShortCut then
      MenuItem.ShortCut:= 0
    else for j:= 0 to MenuItem.Count - 1 do begin
      SubMenuItem:= MenuItem.Items[j];
      if SubMenuItem.ShortCut = ShortCut then
        SubMenuItem.ShortCut:= 0
      else for k:= 0 to SubMenuItem.Count -1 do begin
        SubSubMenuItem:= SubMenuItem.Items[k];
        if SubSubMenuItem.ShortCut = ShortCut then
          SubSubMenuItem.ShortCut:= 0
      end;
    end;
  end;
end;

procedure TFConfiguration.ReplaceShortcutFromMainMenu(ShortCut, ShortCut2: integer);
  var i, j, k: integer;
      MenuItem, SubMenuItem, SubSubMenuItem: TTBCustomItem;
begin
  for i:= 0 to FJava.mainMenu.Items.Count - 1 do begin
    MenuItem:= FJava.mainMenu.Items[i];
    if MenuItem.ShortCut = ShortCut then
      MenuItem.ShortCut:= ShortCut2
    else for j:= 0 to MenuItem.Count - 1 do begin
      SubMenuItem:= MenuItem.Items[j];
      if SubMenuItem.ShortCut = ShortCut then
        SubMenuItem.ShortCut:= ShortCut2
      else for k:= 0 to SubMenuItem.Count -1 do begin
        SubSubMenuItem:= SubMenuItem.Items[k];
        if SubSubMenuItem.ShortCut = ShortCut then
          SubSubMenuItem.ShortCut:= ShortCut2
      end;
    end;
  end;
end;

function TFConfiguration.StringToShortCut(s: string): integer;
begin
  s:= trim(copy(s, Pos(':', s) + 1, length(s)));
  s:= ReplaceStr(s, 'Ctrl+', _('Ctrl+'));
  s:= ReplaceStr(s, 'Shift+', _('Shift+'));
  s:= ReplaceStr(s, 'Alt+', _('Alt+'));
  Result:= TextToShortCut(s);
end;

procedure TFConfiguration.ApplyKeyboardShortcuts;
  var p, q, Key, Key2, Line: Integer;
      OffX, OffY: Integer;
      s, Def: string;
      Keys: TStringList;

  function getNextLine: string;
  begin
    inc(Line);
    if Line < Keys.Count
      then result:= Keys[Line]
      else result:= '';
  end;

  procedure EditKeyboardFile(const afile: string);
    var Collision: boolean;
  begin
    Keys:= TStringList.Create;
    Keys.LoadFromFile(afile);
    Line:= -1;
    s:= getNextLine;
    repeat
      p:= Pos('shortcut:', s);
      if Pos('shortcut:end', s) > 0 then p:= 0;
      if p = 1 then begin
        Key:= StringToShortCut(s);
        RemoveShortCutFromMainMenu(Key);
        OffX:= 0; OffY:= 0;
        Def:= '';
        p:= 0;
        repeat
          s:= getNextLine;
          p:= pos('shortcut:end', s);
          if p = 0 then begin
            q:= Pos('|', s);
            if q > 0 then begin
              OffX:= q;
              OffY:= Line;
            end;
            Def:= Def + #13#10 + s;
          end
        until (p = 1) or (Line >= Keys.Count-1);
        OffY:= Line - OffY - 1;
        Def:= Copy(Def, 3, Length(Def));
        Collision:= Assigned(EditorAndMenuShortcuts.getNode(Key));
        if KeyboardShortcutsTree.InsertKey(Key, Def, OffX, OffY, Collision) then
          CBKeyboard.Items.Add(ShortCutToText(Key));
      end else begin
        p:= Pos('disableMenu:', s);
        if p = 1 then begin
          Key:= StringToShortCut(s);
          RemoveShortcutFromMainMenu(Key);
        end else begin
          p:= Pos('replaceMenu:', s);
          if p = 1 then begin
            Key:= StringToShortCut(s);
            s:= getNextLine;
            p:= Pos('with:', s);
            if p = 1 then begin
              Key2:= StringToShortCut(s);
              ReplaceShortcutFromMainMenu(Key, Key2);
            end;
          end;
        end;
      end;
      s:= getNextLine;
    until Line >= Keys.Count-1;
    FreeAndNil(Keys);
  end;

begin
  CBKeyboard.Clear;
  CBKeyboard.ItemIndex:= 0;
  KeyboardShortcutsTree.Delete;
  if FileExists(KeyboardFile) then
    EditKeyboardFile(KeyboardFile);
  CheckFile(EKeyboardFile, true);
end;

procedure TFConfiguration.RemoveShortcutsFrom(PopupMenu: TSpTBXPopupMenu);
  var p, Line, i, Key: Integer; s: string;
      Keys: TStringList;
      MenuItem: TTBCustomItem;

  function getNextLine: string;
  begin
    inc(Line);
    if Line < Keys.Count
      then result:= Keys[Line]
      else result:= '';
  end;

begin
  if FileExists(KeyboardFile) then begin
    Keys:= TStringList.Create;
    Keys.LoadFromFile(KeyboardFile);
    Line:= -1;
    s:= getNextLine;
    repeat
      p:= Pos('disableMenu:', s);
      if p = 1 then begin
        Key:= StringToShortCut(s);
        for i:= 0 to PopupMenu.Items.Count - 1 do begin
          MenuItem:= PopupMenu.Items[i];
          if MenuItem.ShortCut = Key then
            MenuItem.ShortCut:= 0
        end;
      end;
      s:= getNextLine;
    until Line >= Keys.Count - 1;
    FreeAndNil(Keys);
  end;

end;

procedure TFConfiguration.MenuAndEditorShortcuts;

  function FillUp(s: string; i: integer): string;
  begin
    s:= s + stringOfChar(' ', i);
    Result:= Copy(s, 1, i);
  end;

  procedure MenuShortCuts;
    var i: Integer; Menue: TSpTBXItem; s: string;
  begin
    MenuKeys.Clear;
    for i:= 0 to FJava.ComponentCount - 1 do
      if FJava.Components[i] is TSpTBXItem then begin
        Menue:= (FJava.Components[i] as TSpTBXItem);
        if Menue.ShortCut <> 0 then begin
          EditorAndMenuShortcuts.InsertKey(Menue.ShortCut, '', 0, 0, false);
          s:= ShortCutToText(Menue.ShortCut);
          if s <> '' then
            MenuKeys.Add(FillUp(s, 30) +  ReplaceStr(Menue.Caption, '&', ''));
        end;
      end;         
  end;

  procedure EditorShortCuts;
    var i: Integer; s1, s2: string;
        Keys: TSynEditKeyStrokes;
        aEditor: TFEditForm;
  begin
    EditorKeys.Clear;
    aEditor:= FJava.getActiveEditor;
    if assigned(aEditor) then
      Keys:= aEditor.Editor.Keystrokes
    else begin
      Keys:= TSynEditKeyStrokes.Create(nil);
      Keys.ResetDefaults;
    end;
    try
      for i:= 0 to Keys.Count-1 do begin
        EditorAndMenuShortcuts.InsertKey(Keys.Items[i].ShortCut, '', 0, 0, false);
        s1:= ShortCutToText(Keys.Items[i].ShortCut);
        s2:= ShortCutToText(Keys.Items[i].ShortCut2);
        if s2 <> ''then
           s1:= s1 + '+' + s2;
        EditorCommandToIdent(Keys.Items[i].Command, s2);
        EditorKeys.Add(FillUp(s1, 30) + Copy(s2, 3, Length(s2)));
      end;
    finally
      if aEditor = nil then
        FreeAndNil(Keys);
    end;
  end;

  procedure AllShortcuts;
    var max, i: integer;
  begin
    max:= 0;
    for i:= 0 to MenuKeys.Count - 1 do
      if Length(MenuKeys[i]) > max then
        max:= Length(MenuKeys[i]);
    max:= max + 2;
    AllKeys.Clear;
    for i:= 0 to EditorKeys.Count - 1 do
      AllKeys.Add(FillUp(EditorKeys[i], max) + '(' + RGKeyboard.Items[0]+ ')');
    for i:= 0 to MenuKeys.Count - 1 do
      AllKeys.Add(FillUp(MenuKeys[i], max) + '(' + RGKeyboard.Items[1]+ ')');
    for i:= 0 to CBKeyboard.Items.Count - 1 do
      AllKeys.Add(FillUp(CBKeyboard.Items[i], max) + '(Code)');
  end;

begin
  ApplyKeyboardShortcuts;
  MenuShortcuts;
  EditorShortcuts;
  AllShortcuts;
  case RGKeyboard.ItemIndex of
    0: MKeyboard.Text:= EditorKeys.Text;
    1: MKeyboard.Text:= MenuKeys.Text;
    2: MKeyboard.Text:= AllKeys.Text;
  end;
end;

procedure TFConfiguration.FormCreate(Sender: TObject);
  var i: integer;
begin
  Width:= PPIScale(812);
  Height:= PPIScale(531);
  ODSelect:= TOpenDialog.Create(Self);
  ODSelect.Options:= [ofPathMustExist, ofFileMustExist, ofEnableSizing];

  JavaHighlighter:= TSynJavaSyn.Create(Self);
  with JavaHighlighter do begin
    CommentAttri.Foreground:= clNavy;
    DocumentAttri.Foreground:= clNavy;
    NumberAttri.Foreground:= clBlue;
    StringAttri.Foreground:= clBlue;
  end;

  HTMLHighlighter:= TSynHTMLSyn.Create(Self);
  with HTMLHighlighter do begin
    AndAttri.Foreground:= clGreen;
    CommentAttri.Foreground:= clTeal;
    KeyAttri.Foreground:= clPurple;
  end;

  CBValidClassColor.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];
  CBInvalidClassColor.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];
  CBObjectColor.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];
  CBCommentColor.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];
  CBTextColorBox.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];
  CBBackgroundColorBox.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];
  CBActiveLineColor.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];
  CBSDFillingColor.Style:= [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames];

  PascalHighlighter:= nil;
  PhpHighlighter:= nil;
  PhpInternHighlighter:= nil;
  CSSHighlighter:= nil;
  GeneralHighlighter:= nil;
  MultiSynHighlighter:= nil;

  AllClasses:= nil;
  AllInterfaces:= nil;
  AllPackages:= nil;
  AllClasspathClasses:= nil;

  for i:= 1 to 21 do
    ControlStructureTemplates[i]:= nil;

  JavaVersion:= 0;
  DocumentationVersion:= 0;
  EditFont:= TFont.Create;
  UMLFont:= TFont.Create;
  StructogramFont:= TFont.Create;
  SequenceFont:= TFont.Create;
  RGColors.ItemIndex:= 0;
  AttrBrackets:= TSynHighlighterAttributes.Create('Brackets', 'Brackets');
  SetDefaultBracketColor;

  MyRegistry:= TRegistry.Create;
  RegistryForMachine;
  RegistryForUser;

  LNGTVItems:= TStringList.Create;
  for i:= 0 to TVConfiguration.Items.Count - 1 do
    LNGTVItems.Add(TVConfiguration.Items[i].Text);

  KeyboardShortcutsTree:= TTree.Create;
  EditorAndMenuShortcuts:= TTree.Create;

  MenuKeys:= TStringList.Create;
  EditorKeys:= TStringList.Create;
  AllKeys:= TStringList.Create;
  MenuKeys.Sorted:= true;
  EditorKeys.Sorted:= true;
  AllKeys.Sorted:= true;
  // JavaParser-support
  ImportCache := TStringList.Create;
  ImportCache.Sorted := true;
  ImportCache.CaseSensitive:= true;
  ImportCache.Duplicates := dupIgnore;
  setLength(visTabs, 9);
  setLength(visMenus, 4);
  setLength(visToolbars, 6);

  for i:= 0 to PageList.PageCount - 1 do
    PageList.Pages[i].TabVisible := false;
  TVConfiguration.FullExpand;
  ShowPage(1);
  TVConfiguration.TopItem:= TVConfiguration.Items[0];
  if VistaOrBetter then begin
    SetElevationRequiredState(BFileExtensions);
    SetElevationRequiredState(BJEAssociation);
    SetElevationRequiredState(BJDKInstall);
    SetElevationRequiredState(BDisassemblerInstall);
    SetElevationRequiredState(BManualInstall);
    SetElevationRequiredState(BManualFXInstall);
    SetElevationRequiredState(BTutorialInstall);
    SetElevationRequiredState(BJavaBookInstall);
    SetElevationRequiredState(BLejosInstall);
    SetElevationRequiredState(BCheckstyleInstall);
    SetElevationRequiredState(BJalopyInstall);
    SetElevationRequiredState(BJUnitInstall);
  end;
  VisSelectedTab:= 0;
  AllDocumentations:= TStringList.Create;
  informed:= false;
  LBStyleNames.Sorted := True;
  LoadedStylesDict := TDictionary<string, string>.Create;
  ExternalStyleFilesDict := TDictionary<string, string>.Create;
//  FStylesPath := TPyScripterSettings.StylesFilesDir;
  FPreview:= TVclStylesPreview.Create(Self);
  FPreview.Parent:= StylesPreviewPanel;
  FPreview.Icon := Application.Icon.Handle;
  FPreview.BoundsRect := StylesPreviewPanel.ClientRect;
  TranslateComponent(Self);
  SetupLanguages;
end; // FormCreate

function TFConfiguration.GetPascalHighlighter: TSynPasSyn;
begin
  if not Assigned(PascalHighlighter) then begin
    PascalHighlighter:= TSynPasSyn.Create(Self);
    var Path:= EditorFolder + 'styles' + PathDelim;
    PascalHighlighter.LoadFromFile(Path + 'DefaultColorsPascal.ini', EditorStyle);
  end;
  Result:= PascalHighlighter;
end;

function TFConfiguration.GetCSSHighlighter: TSynCssSyn;
begin
  if not Assigned(CSSHighlighter) then begin
    CSSHighlighter:= TSynCSSSyn.Create(Self);
    var Path:= EditorFolder + 'styles' + PathDelim;
    CSSHighlighter.LoadFromFile(Path + 'DefaultColorsCSS.ini', EditorStyle);
  end;
  Result:= CSSHighlighter;
end;

function TFConfiguration.GetMultiSynHighlighter: TSynMultiSyn;
begin
  if not Assigned(MultiSynHighlighter) then begin
    MultiSynHighlighter:= TSynMultiSyn.Create(Self);
    with MultiSynHighlighter do begin
      DefaultFilter:= '*.jsp';
      DefaultHighlighter:= HTMLHighlighter;
      DefaultLanguageName:= 'JSP';
      var Scheme:= TScheme(Schemes.Add);
      with Scheme do begin
        StartExpr:= '<%';
        EndExpr:= '%>';
        Highlighter:= JavaHighlighter;
        SchemeName:= 'Java';
      end;
    end;
  end;
  Result:= MultiSynHighlighter;
end;

function TFConfiguration.GetPhpHighlighter: TSynMultiSyn;
begin
  if not Assigned(PhpHighlighter) then begin
    PhpHighlighter:= TSynMultiSyn.Create(Self);
    with PhpHighlighter do begin
      DefaultFilter:= '*.php';
      DefaultHighlighter:= HTMLHighlighter;
      DefaultLanguageName:= 'PHP';
      var Scheme:= TScheme(Schemes.Add);
      with Scheme do begin
        StartExpr:= '(<\?){1}(php){0,1}';
        EndExpr:= '\?>';
        Highlighter:= GetPHPInternHighlighter;
        SchemeName:= 'PHP';
      end;
    end;
  end;
  Result:= PhpHighlighter;
end;

//2: Set CaseSensitive to False
//3: Set EndExpr til (\?>)
//4: Set StartExpr til (<\?){1}(php){0,1}

function TFConfiguration.GetPHPInternHighlighter: TSynPhpSyn;
begin
  if not Assigned(PhpInternHighlighter) then begin
    PhpInternHighlighter:= TSynPhpSyn.Create(Self);
    var Path:= EditorFolder + 'styles' + PathDelim;
    PhpInternHighlighter.LoadFromFile(Path + 'DefaultColorsPhp.ini', EditorStyle);
  end;
  Result:= PhpInternHighlighter;
end;

function TFConfiguration.GetGeneralHighlighter: TSynGeneralSyn;
begin
  if not Assigned(GeneralHighlighter) then begin
    GeneralHighlighter:= TSynGeneralSyn.Create(Self);
    var Path:= EditorFolder + 'styles' + PathDelim;
    GeneralHighlighter.LoadFromFile(Path + 'DefaultColorsGeneral.ini', EditorStyle);
    GeneralHighlighter.IdentifierChars:= '_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  end;
  Result:= GeneralHighlighter;
end;

procedure TFConfiguration.Init;
begin
  StyleSelectorShow;
  RegistryToModel;
  CreateBrowserShortCuts;
  if FirstStartAfterInstallation then begin
    ModelToView;
    ViewToModel;
    ModelToRegistry;
  end;
end;

procedure TFConfiguration.RegistryToModel;
  var s, s1, s2: string; i, ver: Integer; IniFile: TIniFile; p: integer;
      dt1, dt2: TDateTime;
      SL: TStringList;
begin
  // General
  if PortableApplication
    then SourcePath:= AddPortableDrive(ReadStringU('Program', 'Sourcepath', EditorFolder + 'Java\'))
    else Sourcepath:= AddPortableDrive(ReadStringU('Program', 'Sourcepath', GetDocumentsPath));
  Sourcepath:= dissolveUsername(Sourcepath);
  if not DirectoryExists(Sourcepath) then
    Sourcepath:= GetDocumentsPath;
  JavaStartClass:= AddPortableDrive(ReadStringU('Program', 'StartClass', ''));
  if (JavaStartClass <> '') and not FileExists(JavaStartClass)
    then JavaStartClass:= '';
  FJava.ODOpen.FilterIndex:= ReadIntegerU('Program', 'FileFilter', 0);
  WindowStateMaximized:= ReadBoolU('Program', 'WindowStateMaximized', true);

  // Font
  EditFont.Name:= ReadStringU('Font', 'Name', 'Consolas');
  EditFont.Size:= max(ReadIntegerU('Font', 'Size', 12), 4);
  UMLFont.Name:= ReadStringU('UML', 'Name', 'Segoe UI');
  UMLFont.Size:= max(ReadIntegerU('UML', 'Size', 12), 4);
  StructogramFont.Name:= ReadStringU('Structogram', 'Name', 'Segoe UI');
  if StructogramFont.Name = 'Arial' then StructogramFont.Name:= 'Segoe UI';
  StructogramFont.Size:= max(ReadIntegerU('Structogram', 'Size', 12), 4);
  SequenceFont.Name:= ReadStringU('SequenceDiagram', 'Name', 'Segoe UI');
  SequenceFont.Size:= max(ReadIntegerU('SequenceDiagram', 'Size', 12), 4);
  ShowAlways:= true;

  // tab Interpreter
  JDKFolder:= ReadStringDirectory('Java', 'JDK-Folder');
  if JDKFolder = '' then JDKFolder:= JavaDevelopmentKit;
  JDKFolder:= withoutTrailingSlash(JDKFolder);
  ExpandPath(JDKFolder + '\bin;');
  SetEnvironmentVar('JAVA_HOME', JDKFolder);
  JavaFXFolder:= ReadStringDirectory('Java', 'JavaFX-Folder');
  if JavaFXFolder <> '' then
    SetEnvironmentVar('PATH_TO_FX', JavaFXFolder + '\lib');
  JavaFxParameter:= ReadStringU('Program', 'JavaFXParameter', 'javafx.controls,javafx.media,javafx.web');

  JDKFolderItems          := LoadComboBoxItems(AddPortableDrives(ReadStringU('Java', 'JDK-FolderItems', '')));
  JavaInterpreter         := ReadStringFile('Java', 'Interpreter', JDKFolder + '\bin\java.exe');
  JavaInterpreterParameter:= ReadStringU('Program', 'InterpreterParameter', '');

  // change JavaClasspathAdmin since version 12.60
  s:= EditorFolder + 'JEClasses.jar';
  JavaClasspathAdmin:= AddPortableDrives(ReadStringM('Java', 'Classpath', s));
  p:= Pos(EditorFolder, JavaClasspathAdmin);
  if (p > 0) and (Pos(s, JavaClasspathAdmin) = 0) then
    insert('JEClasses.jar', JavaClasspathAdmin, p + length(EditorFolder))
  else begin
    {$WARN SYMBOL_PLATFORM OFF}
    p:= Pos(ExcludeTrailingBackslash(EditorFolder), JavaClasspathAdmin);
    {$WARN SYMBOL_PLATFORM ON}
    if (p > 0) and (Pos(s, JavaClasspathAdmin) = 0) then
      insert('\JEClasses.jar', JavaClasspathAdmin, p + length(EditorFolder) -1)
  end;
  p:= Pos('.;', JavaClasspathAdmin);
  if p > 0 then
    Delete(JavaClasspathAdmin, p, 2);

  JavaClasspathUser    := AddPortableDrives(ReadStringU('Program', 'Classpath', ''));
  JavaClasspathAll     := AddPortableDrives(ReadStringU('Program', 'ClasspathAll', ''));
  if JavaClasspathAll = '' then JavaClasspathall:= JavaClasspathUser; // for a transition time
  FileEncoding         := ReadStringU('Program', 'FileEncoding', '');
  Codepage             := ReadStringU('Editor', 'Codepage', '');
  ShowInterpreterCall  := ReadBoolU('Program', 'ShowInterpreterCall', false);

  // tab Compiler
  JavaCompiler         := ReadStringFile('Java', 'JavaCompiler', JDKFolder + '\bin\javac.exe');
  JavaCompilerParameter:= ReadStringU('Program', 'JavaCompilerParameter', '-deprecation -g');
  JavaCompilerOK       := FileExists(JavaCompiler);
  CompileInternally    := ReadBoolU('Program', 'CompileInternally', false);
  ShowCompilerCall     := ReadBoolU('Program', 'ShowCompilerCall', false);
  CompilerEncoding         := ReadStringU('Program', 'CompilerEncoding', '');

  // tab Applet
  AppletStart:= ReadIntegerU('Program', 'AppletStart', 0);
  ShowHTMLforApplet:= ReadBoolU('Program', 'ShowHTMLforApplet', true);
  if PortableApplication
    then TempDirWithUsername:= ReadStringU('Program', 'TempDir', EditorFolder + 'App\Temp')
    else TempDirWithUsername:= ReadStringU('Program', 'TempDir', GetTempDir);
  TempDirWithUserName:= IncludeTrailingPathDelimiter(AddPortableDrive(TempDirWithUsername));
  { $WARN SYMBOL_PLATFORM OFF}
  TempDir:= ExpandFileName(dissolveUsername(TempDirWithUsername));
  try
    Sysutils.ForceDirectories(TempDir);
  except on e: Exception do
    ErrorMsg(E.Message);
  end;
  if not Sysutils.DirectoryExists(TempDir) then
    TempDir:= IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
  { $WARN SYMBOL_PLATFORM ON}

  if PortableApplication then begin
    IniFile:= TIniFile.Create(TempDir + 'test.ini');
    try
      try
        IniFile.WriteString('test', 'test', 'test');
      except
        TempDirWithUsername:= IncludeTrailingPathDelimiter( AddPortableDrive(ReadStringU('Program', 'TempDir', GetTempDir)));
        TempDir:= TempDirWithUsername;
      end;
    finally
      FreeAndNil(IniFile);
    end;
  end;

  // tab Checkstyle
  Checkstyle        := ReadStringFile('Checkstyle', 'Checkstyle', '');
  CheckKonfiguration:= ReadStringFile('Checkstyle', 'Configurationfile', '');
  CheckParameter    := ReadStringU('Checkstyle', 'CheckParameter', '');
  CheckstyleOK      := FileExists(Checkstyle);

  // tab Jalopy
  Jalopy             := ReadStringFile('Jalopy', 'Jalopy', '');
  JalopyConfiguration:= ReadStringFile('Jalopy', 'JalopyConfiguration', '');
  JalopyParameter    := ReadStringU('Jalopy', 'JalopyParameter', '');
  JalopyOK           := FileExists(Jalopy);

  // tab Programs
  JavaDebugger      := ReadStringFile('Java', 'Debugger', JDKFolder + '\bin\jdb.exe');
  JavaDebuggerOK    := FileExists(JavaDebugger);
  JavaDoc           := ReadStringFile('Java', 'JavaDoc', JDKFolder + '\bin\javadoc.exe');
  JavaDocOK         := FileExists(JavaDoc);
  JavaDocParameter  := ReadStringU('Program', 'DocParameter', '-author -version');

  // tab Applets
  JavaAppletviewer  := ReadStringFile('Java', 'Appletviewer', JDKFolder + '\bin\appletviewer.exe');
  JavaAppletviewerOK:= FileExists(JavaAppletViewer);

  if not JavaAppletviewerOK then
    FJava.MIAppletviewer.Visible:= false;
  if not FileExists(JDKFolder + '\lib\htmlconverter.jar') then // till java 1.6.x
    FJava.MIHTMLforJavaPlugIn.Visible:= false;

  // tab Disasssembler
  JavaDisassembler:= ReadStringFile('Java', 'Disassembler', JDKFolder + '\bin\javap.exe');
  JavaDisassemblerItems:= LoadComboBoxItems(AddPortableDrives(ReadStringU('Program', 'DisassemblerItems', '')));
  JavaDisassemblerParameter:= ReadStringU('Program', 'DisassemblerParameter', '-l -c -verbose');

  // tab Jar
  JavaJar:= ReadStringFile('Java', 'Jar', JDKFolder + '\bin\jar.exe');
  JavaJarOK:= FileExists(JavaJar);
  JavaJarParameter:= ReadStringU('Jar', 'JarParameter', '-cfv');
  JavaJarManifest := ReadStringU('Jar', 'JarManifest', '');
  JarCreateCurrent:= ReadStringU('Jar', 'JarCreate', '*.class');
  JarCreateAll    := ReadStringU('Jar', 'JarCreateAll', '1#*.class');
  JarPackFiles    := ReadStringU('Jar', 'JarPack', '*.java *.jfm *.uml');
  JarClassPath    := ReadStringU('Jar', 'JarClassPath', 'JEClasses.jar');

  // tab Mindstorms
  MindstormsParameter:= ReadStringU('Mindstorms', 'Parameter', '');
  MindstormsPort     := ReadIntegerU('Mindstorms', 'Port', 0);
  MindstormsIP       := ReadStringU('Mindstorms', 'IP', '10.0.1.1');
  MindstormsMode     := ReadBoolU('Mindstorms', 'Mode', false);
  MindstormsVersion  := ReadIntegerU('Mindstorms', 'Version', 0);
  MindstormsVerbose  := ReadBoolU('Mindstorms', 'Verbose', false);
  MindstormsDebug    := ReadBoolU('Mindstorms', 'Debug', true);
  MindstormsRun      := ReadBoolU('Mindstorms', 'Run', true);
  MindstormsManual   := ReadStringFile('Mindstorms', 'Manual', '');
  RCXManual:= ReadStringFile('Mindstorms', 'RCXManual', '');
  NXTManual:= ReadStringFile('Mindstorms', 'NXTManual', '');
  EV3Manual:= ReadStringFile('Mindstorms', 'EV3Manual', '');

  LejosVerzeichnis:= ReadStringDirectory('Mindstorms', 'LejosFolder');
  RCXFolder       := ReadStringDirectory('Mindstorms', 'RCXFolder');
  NXTFolder       := ReadStringDirectory('Mindstorms', 'NXTFolder');
  EV3Folder       := ReadStringDirectory('Mindstorms', 'EV3Folder');

  LejosCompiler:= ReadStringFile('Mindstorms', 'LejosCompiler', '');
  LejosUploader:= ReadStringFile('Mindstorms', 'LejosUploader', '');
  LejosFlasher := ReadStringFile('Mindstorms', 'LejosFlasher', '');

  RCXCompiler  := ReadStringDirectory('Mindstorms', 'RCXCompiler');
  RCXUploader  := ReadStringDirectory('Mindstorms', 'RCXUploader');
  RCXFlasher   := ReadStringDirectory('Mindstorms', 'RCXFlasher');

  NXTCompiler  := ReadStringDirectory('Mindstorms', 'NXTCompiler');
  NXTUploader  := ReadStringDirectory('Mindstorms', 'NXTUploader');
  NXTFlasher   := ReadStringDirectory('Mindstorms', 'NXTFlasher');

  if MindstormsMode then
    case MindstormsVersion of
      0: begin
        SetEnvironmentVar('LEJOS_HOME', ANSI2ASCII(RCXFolder));
        SetEnvironmentVar('RCXTTY', CBMindstormsPort.Text);
        ExpandPath(ANSI2ASCII(RCXFolder + '\bin') + ';');
        if (LejosCompiler = '') and (FileExists(LejosUploader)) then
          LejosCompiler:= ExtractFilePath(LejosUploader) + 'lejosjc.bat';
      end;
      1: begin
        SetEnvironmentVar('NXJ_HOME', ANSI2ASCII(NXTFolder));
        ExpandPath(ANSI2ASCII(NXTFolder + '\bin') + ';');
        if (LejosCompiler = '') and (FileExists(LejosUploader)) then
          LejosCompiler:= ExtractFilePath(LejosUploader) + 'nxjc.bat';
      end;
      2: begin
        SetEnvironmentVar('EV3_HOME', ANSI2ASCII(EV3Folder));
        SetEnvironmentVar('LEJOS_EV3_JAVA_HOME', ANSI2ASCII(JDKFolder));
        ExpandPath(ANSI2ASCII(EV3Folder + '\bin') + ';');
        LejosCompiler:= '';
      end;
    end;

  //tab Android-Mode
  AndroidMode:= ReadBoolU('Android', 'Mode', false);
  AndroidSDKFolder:= ReadStringDirectory('Android', 'AndroidSDKFolder');
  if AndroidMode and MindstormsMode then begin
    AndroidMode:= false;
    MindstormsMode:= false;
  end;

  // tab documentation
  JavaManual:= ReadStringFile('Program', 'Manual', JDKFolder + '\docs\index.html');
  if IsCHM(JavaManual) and FileExists(JavaManual) then
    JavaCHMRoot:= GetRootCHM(JavaManual) + '\api'
  else if (JavaManual <> '') and not isHttp(JavaManual) then begin
    s1:= ExtractFilePathEx(JavaManual) + '\allclasses-frame.html';
    s2:= ExtractFilePathEx(JavaManual) + '\allclasses-index.html';
    if not FileExists(s1) and not FileExists(s2) then begin
      s1:= ExtractFilePathEx(JavaManual) + '\api\allclasses-frame.html';
      s2:= ExtractFilePathEx(JavaManual) + '\api\allclasses-index.html';
      if FileExists(s1) or FileExists(s2) then
        JavaManual:= ExtractFilePathEx(JavaManual) + '\api\index.html';
    end;
  end;

  CHMRootOk:= (JavaCHMRoot <> '');
  JavaManualFX:= ReadStringFile('Program', 'ManualFX', JDKFolder + '\docsfx\api\index.html');

  s:= 'docs;chm;';
  JavaManualItems  := LoadComboBoxItems(AddPortableDrives(ReadStringU('Program', 'ManualItems', s)));
  JavaManualFXItems:= LoadComboBoxItems(AddPortableDrives(ReadStringU('Program', 'ManualFXItems', s)));
  JavaJavaDocs   := AddPortableDrives(ReadStringU('Program', 'JavaDocs', ''));
  JavaJavaDocsAll:= AddPortableDrives(ReadStringU('Program', 'JavaDocsAll', ''));
  if JavaJavaDocsAll = '' then JavaJavaDocsAll:= JavaJavaDocs; // for a transition time
    
  JavaTutorial:= ReadStringFile('Program', 'Tutorial', '');
  JavaBook    := ReadStringFile('Program', 'Javabook', '');
  JavaCache   := ReadStringDirectory('Program', 'Cache');
  MakeJavaCache(JavaCache);
  MaxSearch:= ReadIntegerU('Program', 'MaxSearch', 20);
  JavaTools:= getJavaManual;
  if EndsWith(JavaTools, '\api') then
    delete(JavaTools, length(JavaTools) - 3, 4);
  JavaDemos:= JavaTools;
  ver:= getDocumentationVersion;
  case ver of
    1..3: JavaTools:= JavaTools + '\tooldocs\win32\';
    4..5: JavaTools:= JavaTools + '\tooldocs\windows\';
    6..8: JavaTools:= JavaTools + '\technotes\tools\windows\';
    else  JavaTools:= 'https://docs.oracle.com/javase/' + IntToStr(ver) + '/tools/';
  end;
  case ver of
    1..5: JavaDemos:= JavaDemos + '\relnotes\demos.html';
    6..8: JavaDemos:= JavaDemos + '\technotes\samples\demos.html';
    else  JavaDemos:= '';
   end;

  // tab Editor
  TabWidth:= ReadIntegerU('Editor', 'TabWidth', 2);
  Indent  := ReadIntegerU('Editor', 'Indent', 2);
  Indent1 := StringOfChar(' ', 1*Indent);
  Indent2 := StringOfChar(' ', 2*Indent);
  Indent3 := StringOfChar(' ', 3*Indent);
  AutomaticIndent  := ReadBoolU('Editor', 'AutomaticIndent', true);
  CursorBehindLine:= ReadBoolU('Editor', 'CursorBehindLine', true);
  IndentHelp    := ReadBoolU('Editor', 'IndentHelp', true);
  ShowBracketPair  := ReadBoolU('Editor', 'BracketPair', true);
  CommentClosingBrackets := ReadBoolU('Editor', 'CommentClosingBrackets', true);
  StructureColoring      := ReadBoolU('Editor', 'StructureColoring', true);
  StructureColoringPlane := ReadBoolU('Editor', 'StructureColoringPlane', false);
  StructureColorIntensity:= ReadIntegerU('Editor', 'StructureColorIntensity', 5);
  GUICodeFolding := ReadBoolU('Editor', 'GUICodeFolding', true);
  EightyColumnLine:= ReadBoolU('Editor', '80ColumnLine', true);
  InsertControlStructures:= ReadBoolU('Editor', 'InsertControlStructures', true);
  InsertSemicolons:= ReadBoolU('Editor', 'InsertSemicolons', true);
  LineNumbering:= ReadBoolU('Editor', 'Linenumbering', true);
  AddClosingBracket := ReadBoolU('Editor', 'CompleteBracket', false);
  IndentAfterBracket := ReadBoolU('Editor', 'IndentAfterBracket', true);
  KeyboardFile:= ReadStringFile('Editor', 'Keyboardfile', '');

  // tab templates
  Templates[ 1]:= ReadStringFile('Templates', 'Program', '');
  Templates[ 2]:= ReadStringFile('Templates', 'Frame', '');
  Templates[ 3]:= ReadStringFile('Templates', 'Dialog', '');
  Templates[ 4]:= ReadStringFile('Templates', 'Applet', '');
  Templates[ 5]:= ReadStringFile('Templates', 'JFrame', '');
  Templates[ 6]:= ReadStringFile('Templates', 'JDialog', '');
  Templates[ 7]:= ReadStringFile('Templates', 'JApplet', '');
  Templates[ 8]:= ReadStringFile('Templates', 'Application', '');
  Templates[ 9]:= ReadStringFile('Templates', 'Controlstructures', '');
  Templates[10]:= ReadStringFile('Templates', 'Class', '');
  Templates[11]:= ReadStringFile('Templates', 'Mindstorms', '');
  Templates[12]:= ReadStringFile('Templates', 'TestClass', '');
  MakeControlStructureTemplates;

  // tab Code
  CodeCompletionAlways:= ReadBoolU('Code', 'Code-Completion-Always', true);
  CodeCompletionCtrlSpace:= ReadBoolU('Code', 'Code-Completion', true);
  ParameterHints:= ReadBoolU('Code', 'Parameter-Hints', true);
  ShowClassObject:= ReadBoolU('Code', 'ShowClassObject', false);
  CodeDelay:= ReadIntegerU('Code', 'Delay', 100);
  SelectionSizeMin:= ReadIntegerU('Code', 'SelectionSizeMin', 4);
  SelectionSizeMax:= ReadIntegerU('Code', 'SelectionSizeMax', 10);
  FJava.scpJava.TimerInterval:= CodeDelay;
  FJava.scpParams.TimerInterval:= CodeDelay;
  TooltipWithKey:= ReadBoolU('Code', 'TooltipWithKey', true);
  TooltipAutomatic:= ReadBoolU('Code', 'TooltipAutomatic', false);
  TooltipDelay:= ReadIntegerU('Code', 'TooltipDelay', 700);
  TooltipWidth:= ReadIntegerU('Code', 'TooltipWidth', 200);
  TooltipHeight:= ReadIntegerU('Code', 'TooltipHeight', 150);
  TooltipFontSize:= ReadIntegerU('Code', 'TooltipFontSize', 12);

  // tab Browser
  UseIEinternForDocuments:= ReadBoolU('Browser', 'UseIEinternForDocuments', true);
  OnlyOneBrowserWindow:= ReadBoolU('Browser', 'OnlyOneBrowserWindow', false);
  BrowserTitle:= ReadStringU('Browser', 'Title', 'Internet Explorer');
  BrowserProgram:= ReadStringFile('Browser', 'Browser', '');
  if BrowserProgram = '' then begin
    BrowserProgram:= GetExeForExtension('.html');
    BrowserTitle  := BrowserProgToName(BrowserProgram);
  end;
  BrowserOpenKeys := ReadStringU('Browser', 'BrowserOpenKeys', 'XXX');
  if BrowserOpenKeys = 'XXX' then
    BrowserOpenKeys := ReadStringU('Browser', 'AltKeysBrowser', '');

  DecideProxy;
  ProxyIP  := ReadStringU('Browser', 'ProxyIP', ProxyIP);
  ProxyPort:= ReadIntegerU('Browser', 'ProxyPort', ProxyPort);
  withProxy := ReadBoolU('Browser', 'ProxyEnabled', withProxy);

  // tab printer
  BorderLeft  := ReadIntegerU('Printer', 'Left', 20);
  BorderTop   := ReadIntegerU('Printer', 'Top', 20);
  BorderRight := ReadIntegerU('Printer', 'Right', 20);
  BorderBottom:= ReadIntegerU('Printer', 'Bottom', 20);
  Header:= ReadStringU('Printer', 'Header', '#%PATH%#');
  Footer:= ReadStringU('Printer', 'Footer', '##- %PAGENUM% -');
  // since version 20.08
  UpdateHeaderFooter;
  WithLinenumbers    := ReadBoolU('Printer', 'Linenumbers', False);
  LinenumbersInMargin:= ReadBoolU('Printer', 'LinenumbersInMargin', True);
  PrintColored       := ReadBoolU('Printer', 'PrintColored', True);
  if hasDefaultPrinter then
    setPrinter(ReadStringU('Printer', 'Printer', ''));

  // tab comment
  FreeComment:= ReadStringU('Comment', 'Comment', '');
  CommentKind:= ReadIntegerU('Comment', 'Type', 0);
  JavaAuthor:= ReadStringU('Comment', 'Author', '');
  MethodComment:= LoadComboBoxItems(ReadStringU('Comment', 'Method', ''));

  // tab colors
  GUIStyle:= ReadStringU('Colors', 'GUIStyle', 'Windows');
  EditorStyle:= ReadStringU('Colors', 'EditorStyle', 'Default');
  ReadUserColors;
  NoActiveLineColor:= ReadBoolU('Colors', 'NoActiveLineColor', true);
  if NoActiveLineColor
    then ActiveLineColor:= clNone
    else ActiveLineColor:= ReadIntegerU('Colors', 'ActiveLineColor', clRed);
  NoSyntaxHighlighting:= ReadBoolU('Colors', 'NoSyntaxHighlighting', false);

  // tab GUI designer
  NameFromText:= ReadBoolU('Options', 'NameFromText', true);
  GuiDesignerHints:= ReadBoolU('Options', 'GuiDesignerHints', true);
  SnapToGrid:= ReadBoolU('Options', 'SnapToGrid', true);
  GridSize:= ReadIntegerU('Options', 'GridSize', 8);
  ZoomSteps:= ReadIntegerU('Options', 'ZoomSteps', 1);
  GUIFontSize:= max(ReadIntegerU('Options', 'GUIFontSize', 11), 4);
  GUIFontName:= ReadStringU('Options', 'GUIFontName', 'Dialog');
  FrameWidth  := ReadIntegerU('Editor', 'FrameWidth', 300);
  FrameHeight := ReadIntegerU('Editor', 'FrameHeight', 300);
  AppletWidth := ReadIntegerU('Editor', 'AppletWidth', 400);
  AppletHeight:= ReadIntegerU('Editor', 'AppletHeight', 300);

  // tab options
  CreateBAKFiles:= ReadBoolU('Options', 'BAKFiles', true);
  LoadFiles:= ReadBoolU('Options', 'LoadFiles', true);
  UseInterpreterWindowAsConsole:= ReadBoolU('Options', 'UseInterpreterWindowAsConsole', false);
  AcceptDefaultname:= ReadBoolU('Options', 'AcceptDefaultname', false);
  DebuggerProtocol:= ReadBoolU('Options', 'DebuggerProtocol', false);
  TranslateCompilerErrors:= ReadBoolU('Options', 'TranslateCompilerErrors', true);
  BorderLayout:= ReadBoolU('Options', 'BorderLayout', false);
  ComponentsToolbar:= ReadBoolU('Options', 'ComponentsToolbar', true);
  StrictJavaMode:= ReadBoolU('Options', 'StrictJavaMode', true);
  CheckAge:= ReadBoolU('Options', 'CheckAge', true);
  FileFilter:= ReadStringU('Options', 'FileFilter', '');
  RenameWhenSave:= ReadBoolU('Options', 'Rename', true);
  Fontsize:= ReadIntegerU('Options', 'Fontsize', 9);
  FJava.Font.Size:= PPIScale(Fontsize);
  MaxFileHistory:= ReadIntegerU('Options', 'MaxFileHistory', 10);
  FJava.TabsControl.Visible:= ComponentsToolbar;
  FJava.PBorder.Visible:= BorderLayout;

  // tab restrictions
  DOSWindowLocked:= ReadBoolM('Java', 'DOS-Window', false);
  BlockedInternet:= ReadBoolM('Java', 'BlockedInternet', false);
  LockedPaths:= ReadBoolM('Java', 'LockedPaths', false);
  LockedStructogram:= ReadBoolM('Java', 'LockedStructogram', false);

  // tab Associations
  AdditionalAssociations:= ReadStringU('Associations', 'Additional', '');

  // tab UML
  ValidClassColor:= ReadIntegerU('UML', 'ValidClassColor', clWhite);
  InvalidClassColor:= ReadIntegerU('UML', 'InvalidClassColor', clFuchsia);
  ClassHead:= ReadIntegerU('UML', 'ClassHead', 0);
  ShadowWidth:= ReadIntegerU('UML', 'ShadowWidth', 3);
  ShadowIntensity:= ReadIntegerU('UML', 'ShadowIntensity', 8);

  ObjectColor:= ReadIntegerU('UML', 'ObjectColor', clYellow);
  ObjectHead:= ReadIntegerU('UML', 'ObjectHead', 1);
  ObjectFooter:= ReadIntegerU('UML', 'ObjectFooter', 1);
  ObjectCaption:= ReadIntegerU('UML', 'ObjectCaption', 0);
  ObjectUnderline:= ReadBoolU('UML', 'ObjectUnderline', true);
  CommentColor:= ReadIntegerU('UML', 'CommentColor', clSkyBlue);

  DiVisibilityFilter:= ReadIntegerU('UML', 'Visibility', 0);
  DiSortOrder:= ReadIntegerU('UML', 'SortOrder', 0);
  DiShowParameter:= ReadIntegerU('UML', 'ShowParameter', 4);
  DiShowIcons:= ReadIntegerU('UML', 'ShowIcons', 1);

  // tab uml options
  PrivateAttributEditable:= ReadBoolU('UML', 'PrivateAttribut', true);
  ShowEmptyRects:= ReadBoolU('UML', 'ShowEmptyRects', false);
  ConstructorWithVisibility:= ReadBoolU('UML', 'ConstructorWithVisibility', false);
  ShowFunctionvalues:= ReadBoolU('UML', 'ShowFunctionvalues', true);
  ObjectLowerCaseLetter:= ReadBoolU('UML', 'ObjectLowerCaseLetter', true);
  ShowPublicOnly:= ReadBoolU('UML', 'ShowPublicOnly', false);
  DefaultModifiers:= ReadBoolU('UML', 'DefaultModifiers', true);
  ShowObjectsWithMethods:= ReadBoolU('UML', 'ShowObjectsWithMethods', false);
  ShowObjectsWithInheritedPrivateAttributes:= ReadBoolU('UML', 'ShowObjectsWithInheritedPrivateAttributes', true);
  AttributesAParametersP:= ReadBoolU('UML', 'AttributesAParametersP', false);
  UseVoid:= ReadBoolU('UML', 'UseVoid', false);
  IntegerInsteadofInt:= ReadBoolU('UML', 'IntegerInsteadofInt', false);
  ShowAllNewObjects:= ReadBoolU('UML', 'ShowAllNewObjects', true);
  ObjectsWithoutVisibility:= ReadBoolU('UML', 'ObjectsWithoutVisibility', true);
  ArrayListAsIntegratedList:= ReadBoolU('UML', 'ArrayListAsIntegratedList', false);
  RelationshipAttributesBold:= ReadBoolU('UML', 'RelationshipAttributesBold', true);
  StartWithDatatype:= ReadBoolU('UML', 'StartWithDatatype', false);
  SetterWithoutThis:= ReadBoolU('UML', 'SetterWithoutThis', true);
  ShowClassparameterSeparately:= ReadBoolU('UML', 'ShowClassparameterSeparately', false);
  RoleHidesAttribute:= ReadBoolU('UML', 'RoleHidesAttribute', false);
  UseAbstract:= ReadBoolU('UML', 'UseAbstract', false);

  // tab SVN
  SVNFolder    := ReadStringDirectory('SVN', 'SVNFolder');
  SVNRepository:= ReadStringDirectory('SVN', 'SVNRepository'); // TODO: Dir or File
  SubversionOK := FileExists(SVNFolder + '\svn.exe');
  if SubversionOK and (FSubversion = nil) then
    FSubversion:= TFSubversion.Create(FJava);

  // tab Git
  GitFolder:= ReadStringDirectory('Git', 'GitFolder');
  GitLocalRepository:= ReadStringDirectory('Git', 'GitLocalRepository');
  GitRemoteRepository:= ReadStringDirectory('Git', 'GitRemoteRepository');
  GitUserName:= ReadStringU('Git', 'user.name', '');
  GitUserEMail:= ReadStringU('Git', 'user.email', '');
  GitOK := FileExists(GitFolder + '\bin\git.exe');
  if GitOK and (FGit = nil) then
    FGit:= TFGit.Create(FJava);

  // tab JUnit
  JUnitJarFile:= ReadStringDirectory('JUnit', 'JUnitJarFile');
  JUnitManual:= ReadStringFile('JUnit', 'JUnitManual', '');
  JUnitParameter:= ReadStringU('JUnit', 'JUnitParameter', '');
  JUnitBeforeEach:= ReadBoolU('JUnit', 'JUnitBeforeEach', true);
  JUnitAfterEach:= ReadBoolU('JUnit', 'JUnitAfterEach', true);
  JUnitOK := (JUnitJarFile <> '') and FileExists(JUnitJarFile);

  // tab Visibility  Reg2Mod
  LoadVisibility;
  SetVisibility;
  MakeSystemClasses;

  // tab Logfiles
  LogfileCompilerWithUsername:= ReadStringFile('Logfiles', 'LogfileCompiler', '');
  LogfileCompiler:= dissolveUsername(LogfileCompilerWithUsername);
  LogfileCompilerOK:= FileExists(LogfileCompiler);
  if not LogfileCompilerOK then begin
    CreateMyFile(LogfileCompiler);
    LogfileCompilerOK:= FileExists(LogfileCompiler);
  end;

  RightDockPanelWidth:= ReadIntegerU('Panel', 'RightDockPanelWidth', 150);
  BottomDockPanelHeight:= ReadIntegerU('Panel', 'BottomDockPanelHeight', 150);

  LogfileInteractiveWithUsername:= ReadStringFile('Logfiles', 'LogfileInteractive', '');
  LogfileInteractive:= dissolveUsername(LogfileInteractiveWithUsername);
  LogfileInteractiveOK:= FileExists(LogfileInteractive);
  if not LogfileInteractiveOK then begin
    CreateMyFile(LogfileInteractive);
    LogfileInteractiveOK:= FileExists(LogfileInteractive);
  end;

  if PortableApplication then
    s:= EditorFolder + 'App\Log\JELogfileExceptions.txt'
  else begin
    s:= GetHomePath + '\JavaEditor\Log\JELogfileExceptions.txt';
    SL:= Split('\', s);
    i:= 1;
    while (i < SL.Count) and (SL.Strings[i] <> 'AppData') do
      inc(i);
    if i < SL.Count then
      SL.Strings[i-1]:= '%USERNAME%';
    s:= WithoutTrailingSlash(ReplaceStr(SL.Text, #$0D#$0A, '\'));
    FreeAndNil(SL);
  end;
  LogfileExceptionsWithUsername:= ReadStringFile('Logfiles', 'LogfileExceptions', s);
  LogfileExceptions:= dissolveUsername(LogfileExceptionsWithUsername);
  CreateMyFile(LogfileExceptions);
  LogfileExceptionsOK:= FileExists(LogfileExceptions);

  try
    for i:= FJava.TDIFormsList.Count - 1 downto 0 do
      FJava.TDIFormsList[i].SetOptions;
  except
  end;

  // tab Language
  LanguageCode:= ReadStringU('Options', 'Language', 'XXX');
  if LanguageCode = 'XXX' then begin
    if Pos('Deutsch', GetUsersWindowsLanguage) > 0
      then LanguageCode:= 'de'
      else LanguageCode:= GetCurrentLanguage;
  end;
  FJava.ChangeLanguage(LanguageCode);
  RGLanguages.ItemIndex:= max(0, fLanguagesList.IndexOf(LanguageCode));

  // tab Structograms after reading language
  StructogramS:= 'Structogram.' + LanguageCode;

  Algorithm:= ReadStringU(StructogramS, 'Algorithm', _('Algorithm'));
  Input:= ReadStringU(StructogramS, 'Input', _('Input:'));
  Output:= ReadStringU(StructogramS, 'Output', _('Output:'));
  _While:= ReadStringU(StructogramS, 'While', _('repeat while'));
  DoWhile:= ReadStringU(StructogramS, 'DoWhile', _('repeat while'));
  _For:= ReadStringU(StructogramS, 'For', _('repeat for [i] = [1] to [n]'));
  //if Pos('[', _For) = 0 then _For:= LNGFor;

  Yes:= ReadStringU(StructogramS, 'Yes', _('yes'));
  No:= ReadStringU(StructogramS, 'No', _('no'));
  Other:= ReadStringU(StructogramS, 'Other', _('other'));

  GenerateJavaAsProgram:= ReadIntegerU('Structogram', 'GenerateJavaAsProgram', 1);
  if GenerateJavaAsProgram = 2 then GenerateJavaAsProgram:= 1;
  StructoDatatype:= ReadStringU('Structogram', 'Datatype', 'double');
  SwitchWithCaseLine:= ReadBoolU('Structogram', 'SwitchWithCaseLine', false);
  CaseCount:= ReadIntegerU('Structogram', 'CaseCount', 4);
  StructogramShadowWidth:= ReadIntegerU('Structogram', 'ShadowWidth', 3);
  StructogramShadowIntensity:= ReadIntegerU('Structogram', 'ShadowIntensity', 8);

  // tab Sequence diagram
  SequenceDiagramS:= 'Sequencediagram.' + LanguageCode;
  SDObject:= ReadStringU(SequenceDiagramS, 'Object', _('Object'));
  SDNew:= ReadStringU(SequenceDiagramS, 'New', _('new'));
  SDClose:= ReadStringU(SequenceDiagramS, 'Close', _('close'));
  SDFillingcolor:= ReadIntegerU(SequenceDiagramS, 'Fillingcolor', clYellow);
  SDNoFilling:= ReadBoolU(SequenceDiagramS, 'NoFilling', false);
  SDShowMainCall:= ReadBoolU(SequenceDiagramS, 'ShowMainCall', false);
  SDShowParameter:= ReadBoolU(SequenceDiagramS, 'ShowParameter', true);
  SDShowReturn:= ReadBoolU(SequenceDiagramS, 'ShowReturn', true);

  // tab providers
  ReadProviders('LLMAssistant', LLMAssistant.Providers);
  ReadProviders('LLMChat', FLLMChatForm.LLMChat.Providers);

  FJava.FormResize(Self);
  ApplyKeyboardShortcuts;
  FJava.ODOpen.Filter:= GetFileFilters;

  // if JEClasses.jar changes it must be updated
  FileAge(JavaCache + '\classes\classpathclasses.txt', dt1);
  FileAge(EditorFolder + 'JEClasses.jar', dt2);
  if dt1 < dt2 then
    MakeClasspathClasses;
  CollectDocumentations;
  getJavaVersion;
end; // RegistryToModel

function TFConfiguration.TranslateCompilerError(const err: string): string;
begin
  if err = ''';'' expected' then exit(_(''';'' expected'));
  if err = '''('' expected' then exit(_('''('' expected'));
  if err = ''')'' expected' then exit(_(''')'' expected'));
  if err = ''']'' expected' then exit(_(''']'' expected'));
  if err = '''}'' expected' then exit(_('''}'' expected'));
  if err = '> expected' then exit(_('> expected'));
  if err = '<identifier> expected' then exit(_('<identifier> expected'));
  if err = 'not a statement' then exit(_('not a statement'));
  if err = 'unclosed string literal' then exit(_('unclosed string literal'));
  if err = 'illegal start of expression' then exit(_('illegal start of expression'));
  if err = 'illegal start of type' then exit(_('illegal start of type'));
  if err = 'cannot find symbol' then exit(_('cannot find symbol'));
  if err = 'class, interface, or enum expected' then exit(_('class, interface, or enum expected'));
  if err = 'class expected' then exit(_('class expected'));
  if err = 'reached end of file while parsing' then exit(_('reached end of file while parsing'));
  if err = 'might not have been initialized' then exit(_('might not have been initialized'));
  if err = 'is already defined in' then exit(_('is already defined in'));
  if err = 'array required, but found' then exit(_('array required, but found'));
  if err = 'incompatible types' then exit(_('incompatible types'));
  if err = 'incompatible types: missing return value' then exit(_('incompatible types: missing return value'));
  if err = 'incompatible types: unexpected return value' then exit(_('incompatible types: unexpected return value'));
  if err = 'incompatible types: possible lossy' then exit(_('incompatible types: possible lossy'));
  if err = 'invalid method declaration; return type required' then exit(_('invalid method declaration; return type required'));
  if err = 'non-static cannot be referenced' then exit(_('non-static cannot be referenced'));
  if err = 'unreachable statement' then exit(_('unreachable statement'));
  if err = 'bad operand types for binary operator' then exit(_('bad operand types for binary operator'));
  Result:= err;
end;

procedure TFConfiguration.MakeJavaCache(s: string);
begin
  if s = '' then
    if PortableApplication
      then s:= EditorFolder + 'App\Cache'
      else s:= GetHomePath + '\JavaEditor\Cache';
  JavaCacheWithUsername:= withoutTrailingSlash(s);
  JavaCache:= withoutTrailingSlash(dissolveUsername(s));
  if not Sysutils.ForceDirectories(JavaCache) then begin
    if PortableApplication
      then s:= EditorFolder + 'App\Cache'
      else s:= GetHomePath + '\JavaEditor\Cache';
    JavaCacheWithUsername:= withoutTrailingSlash(s);
    JavaCache:= withoutTrailingSlash(dissolveUsername(s));
    if not Sysutils.ForceDirectories(s) then
      ErrorMsg(Format(_(LNGCanNotCreateDirectory), [JavaCache]));
  end;
end;

procedure TFConfiguration.ShowDefaultMindstormsAndroidConfiguration;
begin
  with FJava do begin
    MIMindstormsHelp.Visible:= MindstormsMode;
    MIMindstorms.Visible:= MindstormsMode;
    MIRun.ImageIndex:= 22;
    if MindstormsMode then begin
      MICompile.ImageIndex:= 50;
      TBCompileJava.ImageIndex:= 16;
      TBCompileJava.Hint:= _(LNGCompileForMindstorms);
      TBRun.Hint:= SetRCXNXTEV3(_(LNGTransferToRCX));
      TBCompileAll.ImageIndex:= 17;
      case MindstormsVersion of
        0: begin
             TBCompileAll.Hint:= 'Download Lejos firmware';
             MIMindstorms.Caption:= TBCompileAll.Hint;
        end;
        1: begin
             TBCompileAll.Hint:= _('NXT options');
             MIMindstorms.Caption:= TBCompileAll.Hint;
        end;
        2: begin
             TBCompileAll.Hint:= 'EV3 Control Center';
             MIMindstorms.Caption:= 'EV3 Control Center';
        end;
      end;
      end
    else if AndroidMode then begin
      MIRun.ImageIndex:= 87;
      MIRun.Caption:= _(LNGTransferToAndroid);
      TBRun.Hint:= _(LNGTransferToAndroid);
      TBRun.ImageIndex:= 15;
    end else begin
      MICompile.ImageIndex:= 21;
      TBCompileJava.ImageIndex:= 11;
      TBCompileJava.Hint:= _(LNGCompileWithJava);
      TBCompileAll.ImageIndex:= 12;
      TBCompileAll.Hint:= _('Compile all');
      TBRun.Hint:= _('Run');
    end;
  end;
end;

procedure TFConfiguration.CreateBrowserShortCuts;
  var i: integer; ct, sh: string;
begin
  //ct:= _SmkcCtrl;
  //sh:= _SmkcShift;
  CBOpenBrowserShortcut.Items.Clear;
  for i:= 65 to 65 + 25 do
    CBOpenBrowserShortcut.Items.Add('<' + ct + Chr(i) + '>');
  for i:= 65 to 65 + 25 do
    CBOpenBrowserShortcut.Items.Add('<' + ct + sh + Chr(i) + '>');
end;

procedure TFConfiguration.RegistryForMachine;
begin
  EditorFolder:= ExtractFilePath(ParamStr(0));
  ChDir(EditorFolder);
  var s:= ParamStr(1);
  if (s = '') or (UpperCase(ExtractFileExt(s)) <> '.INI')
    then s:= EditorFolder + 'JEMachine.ini'
    else s:= ExpandUNCFileName(s);
  PortAppDrive:= ExtractFileDrive(s);   // with UNC we get \\Server\Freigabe
  if Pos(':', PortAppDrive) > 0 then
    PortAppDrive:= copy(PortAppDrive, 1, 1);
  UseRegistry:= not FileExists(s);

  if UseRegistry then begin
    MachineIniFile:= nil;
    PortableApplication:= false;
  end else begin
    MachineIniFile:= TMemIniFile.Create(s);
    PortableApplication:= MachineIniFile.ReadBool('Java', 'PortableApplication', false);
  end;
  WriteProtection:= getWriteProtection;
end;

procedure TFConfiguration.RegistryForUser;
  var s: string;
      aFile: TFileStream;
begin
  FirstStartAfterInstallation:= false;
  if UseRegistry then begin
    with MyRegistry do begin
      RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_READ;
      if not OpenKey('\Software\JavaEditor', false) then begin
        FirstStartAfterInstallation:= true;
        JavaHighlighter.SaveToRegistry(HKEY_CURRENT_USER, getRegPath + '\Java');
        HTMLHighlighter.SaveToRegistry(HKEY_CURRENT_USER, getRegPath + '\HTML');
      end;
      s:= GetHomePath + '\JavaEditor\';
      if not Sysutils.DirectoryExists(s) then Sysutils.ForceDirectories(s);
      HomeDir:= s;
    end;
    UserIniFile:= nil;
  end else begin
    s:= MachineIniFile.ReadString('User', 'HomeDir', '<nix>');
    if s = '<nix>' then begin
      ErrorMsg(Format(_('In section [user] of the configuration file "%s" the value'), [MachineIniFile.Filename]) + #13#10 +
                      _('of the key "HomeDir" for the home directory of the user is not set.'));
      UserIniFile:= nil;
      exit;
    end;
    s:= withTrailingSlash(AddPortableDrive(dissolveUsername(s)));
    if not Sysutils.DirectoryExists(s) then Sysutils.ForceDirectories(s);
    HomeDir:= s;
    s:= s + 'JEUser.ini';
    if not FileExists(s) then begin
      FirstStartAfterInstallation:= true;
      try
        aFile:= TFileStream.Create(s, fmCreate or fmOpenWrite);
        FreeAndNil(aFile);
      except
        ErrorMsg(Format(_('Could not create the preferencesfile %s!'), [s]));
        s:= AddPortableDrive(GetTempDir + 'JEUser.ini');
      end;
      try
        JavaHighlighter.SaveToFile(ExtractFilePath(s) + '\JEJavaCol.ini');
        HTMLHighlighter.SaveToFile(ExtractFilePath(s) + '\JEHTMLCol.ini');
      except
      end;
    end;
    try
      UserIniFile:= TIniFile.Create(s);
    except
      on e: exception do begin
        ErrorMsg(Format(_('Could not create the preferencesfile %s!'), [s]) + ' ' + e.Message);
        UserIniFile:= nil;
      end;
    end;
  end;
end;

function TFConfiguration.getWriteProtection: Boolean;
begin
  Result:= false;
  if UseRegistry then
    Result:= not IsAdministrator
  else
    try
      MachineIniFile.WriteString('Java', 'WriteProtection', 'ok');
      MachineIniFile.UpdateFile;
    except
      Result:= true;
    end;
end;

function TFConfiguration.JavaDevelopmentKit: string;
  const JDK = '\Software\JavaSoft\Java Development Kit';
  var SL: TStringList; Home: string; i: Integer;
begin
  Result:= '';
  with MyRegistry do begin
    RootKey:= HKEY_LOCAL_MACHINE;
    Access:= KEY_READ;
    try
      if OpenKey(JDK, false) then begin
        SL:= TStringList.Create;
        GetKeyNames(SL);
        Home:= '';
        i:= SL.Count - 1;
        while i >= 0 do begin
          if OpenKey(JDK + '\' + SL[i], false) then begin
            Home:= ReadString('JavaHome');
            if Sysutils.DirectoryExists(Home) then break else Home:= '';
          end;
          dec(i);
        end;
        Result:= Home;
        FreeAndNil(SL);
      end;
    finally
      CloseKey;
    end;
  end;
end;

procedure TFConfiguration.ModelToView;
begin
  with FJava do begin
    // tab Interpreter
    ShortenPath(CBJDKFolder, JDKFolder);
    CBJDKFolder.Items.Text:= JDKFolderItems;
    ShortenPath(EInterpreter, JavaInterpreter);
    EInterpreterParameter.Text:= JavaInterpreterParameter;
    EJavaFXFolder.Text:= JavaFXFolder;
    EJavaFxParameter.Text:= JavaFXParameter;
    EClasspathAdmin.Text:= JavaClasspathAdmin;
    EClasspathUser.Text:= JavaClasspathUser;
    EJavaDocs.Text:= JavaJavaDocs;
    CBShowInterpreterCall.Checked:= ShowInterpreterCall;
    CBFileEncoding.Text:= FileEncoding;
    CBCodepage.Text:= CodePage;

    // tab Compiler
    ShortenPath(EJavaCompiler, JavaCompiler);
    EJavaCompilerParameter.Text:= JavaCompilerParameter;
    CBUseJavaCompilerInternally.Checked:= CompileInternally;
    CBShowCompilerCall.Checked:= ShowCompilerCall;
    CBCompilerEncoding.Text:= CompilerEncoding;

    // tab Programme
    ShortenPath(EDebugger, JavaDebugger);
    ShortenPath(EJavaDoc, JavaDoc);
    EDocParameter.Text:= JavaDocParameter;

    // tab Applets
    ShortenPath(EAppletviewer, JavaAppletviewer);
    RGApplet.ItemIndex:= AppletStart;
    CBShowHTMLforApplet.checked:= ShowHTMLForApplet;

    // tab Disassembler
    ShortenPath(CBDisassembler, JavaDisassembler);
    CBDisassembler.Items.Text:= JavaDisassemblerItems;
    EDisassemblerParameter.Text:= JavaDisassemblerParameter;

    // tab Checkstyle
    ShortenPath(ECheckstyle, Checkstyle);
    ShortenPath(ECheckstyleConfiguration, CheckKonfiguration);
    ECheckstyleParameter.Text:= CheckParameter;

    // tab Jalopy
    ShortenPath(EJalopy, Jalopy);
    ShortenPath(EJalopyConfiguration, JalopyConfiguration);
    EJalopyParameter.Text:= JalopyParameter;

    // tab Jar
    ShortenPath(EJar, JavaJar);
    EJarParameter.Text:= JavaJarParameter;
    EJarManifest.Text:= JavaJarManifest;
    EJarCreate.Text:= JarCreateCurrent;
    CBJarPack.Text:= JarPackFiles;
    EJarClassPath.Text:= JarClassPath;

    // tab templates
    ShortenPath(ETemplateConsole, Templates[1]);
    ShortenPath(ETemplateFrame, Templates[2]);
    ShortenPath(ETemplateDialog, Templates[3]);
    ShortenPath(ETemplateApplet, Templates[4]);
    ShortenPath(ETemplateJFrame, Templates[5]);
    ShortenPath(ETemplateJDialog, Templates[6]);
    ShortenPath(ETemplateJApplet, Templates[7]);
    ShortenPath(ETemplateApplication, Templates[8]);
    ShortenPath(ETemplateControlstructure, Templates[9]);
    ShortenPath(ETemplateClass, Templates[10]);
    ShortenPath(EMindstormsTemplate, Templates[11]);
    ShortenPath(ETemplateJUnitTest, Templates[12]);

    // tab documentation
    CBManual.Items.Text:= JavaManualItems;
    ShortenPath(CBManual, JavaManual);
    CBManualFX.Items.Text:= JavaManualFXItems;
    ShortenPath(CBManualFX, JavaManualFX);
    ShortenPath(ETutorial, JavaTutorial);
    ShortenPath(EJavabook, Javabook);
    ShortenPath(ECache, JavaCacheWithUsername);
    UDMaxSearch.Position:= MaxSearch;

    // tab Editor
    UDTabWidth.Position:= TabWidth;
    UDIndent.Position:= Indent;
    CBIndentHelp.Checked:= IndentHelp;
    CBCursorBehindLine.Checked:= CursorBehindLine;
    CBAutomaticIndent.Checked:= AutomaticIndent;

    CBLineNumbering.Checked:= LineNumbering;
    CBCompleteBracket.Checked:= AddClosingBracket;
    CBIndentAfterBracket.Checked:= IndentAfterBracket;
    CBShowBracketpair.Checked:= ShowBracketPair;
    CBCommentClosingBrackets.Checked:= CommentClosingBrackets;
    CBStructureColoring.Checked:= StructureColoring;
    CBStructureColoringPlane.Checked:= StructureColoringPlane;
    CBGUICodeFolding.Checked:= GUICodeFolding;
    CB80ColumnLine.Checked:= EightyColumnLine;

    UDIntensity.Position:= StructureColorIntensity;
    CBInsertControlStructures.Checked:= InsertControlStructures;
    CBInsertSemicolons.Checked:= InsertSemicolons;

    // tab Code
    RBCodeCompletionCtrlSpace.Checked:= CodeCompletionCtrlSpace;
    RBCodeCompletionAlways.Checked:= CodeCompletionAlways;
    CBParameterHints.Checked:= ParameterHints;
    CBShowClassObject.Checked:= ShowClassObject;
    TBDelay.Position:= CodeDelay;
    ESelectionSizeMin.Text:= IntToStr(SelectionSizeMin);
    ESelectionSizeMax.Text:= IntToStr(SelectionSizeMax);
    CBTooltipWithKey.Checked:= TooltipWithKey;
    CBTooltipAutomatic.Checked:= TooltipAutomatic;
    TBTooltipDelay.Position:= TooltipDelay;

    // tab colors
    CBNoActiveLineColor.Checked:= NoActiveLineColor;
    CBActiveLineColor.Selected:= ActiveLineColor;
    CBActiveLineColor.Enabled:= not NoActiveLineColor;
    CBNoSyntaxHighlighting.Checked:= NoSyntaxHighlighting;
    ReadEditorStyleNames;
    CBEditorStyles.Text:= EditorStyle;

    // tab Browser
    CBUseIEinternForDocuments.Checked:= UseIEinternForDocuments;
    CBOnlyOneBrowserWindow.Checked:= OnlyOneBrowserWindow;
    EBrowsertitle.text   := BrowserTitle;
    CBOpenBrowserShortcut.Text := BrowserOpenKeys;
    ShortenPath(EBrowserProgram, BrowserProgram);
    EProxyIP.text     := ProxyIP;
    EProxyPort.text   := IntToStr(ProxyPort);
    CBUseProxy.Checked:= withProxy;

    // tab printer
    EBorderLeft.Text  := IntTostr(BorderLeft);
    EBorderTop.Text   := IntTostr(BorderTop);
    EBorderRight.Text := IntTostr(BorderRight);
    EBorderBottom.Text:= IntTostr(BorderBottom);
    EHeader.Text:= Header;
    EFooter.Text:= Footer;
    CBLinenumbers.Checked:= WithLinenumbers;
    CBLinenumbersInBorder.Checked:= LinenumbersInMargin;
    CBPrintColored.Checked:= PrintColored;

    // tab comment
    RGComment.ItemIndex:= CommentKind;
    EAuthor.Text:= JavaAuthor;
    RGCommentClick(Self);
    MMethodComment.Text:= MethodComment;

    // tab keyboard
    ShortenPath(EKeyboardFile, KeyboardFile);

    // tab Mindstorms
    case MindstormsVersion of
      0: begin
         LejosVerzeichnis:= RCXFolder;
         LejosCompiler:= RCXCompiler;
         LejosUploader:= RCXUploader;
         LejosFlasher:= RCXFlasher;
         MindstormsManual:= RCXManual;
      end;
      1: begin
         LejosVerzeichnis:= NXTFolder;
         LejosCompiler:= NXTCompiler;
         LejosUploader:= NXTUploader;
         LejosFlasher:= NXTFlasher;
         MindstormsManual:= NXTManual;
      end;
      2: begin
         LejosVerzeichnis:= EV3Folder;
         MindstormsManual:= EV3Manual;
      end;
    end;
    ShortenPath(ELejosFolder, LejosVerzeichnis);
    ShortenPath(ELejosCompiler, LejosCompiler);
    ShortenPath(ELejosUploader, LejosUploader);
    ShortenPath(ELejosFlasher, LejosFlasher);
    ShortenPath(EMindstormsManual, MindstormsManual);

    EMindstormsParameter.Text:= MindstormsParameter;
    CBMindstormsPort.ItemIndex:= MindstormsPort;
    CBMindstormsModus.Checked:= MindstormsMode;
    EMindstormsIP.Text:= MindstormsIP;
    RGMindStormsVersion.ItemIndex:= MindstormsVersion;

    //tab Android
    CBAndroidMode.Checked:= AndroidMode;
    EAndroidSDKFolder.Text:= AndroidSDKFolder;

    // tab GUI designer
    CBNameFromText.Checked:= NameFromText;
    CBGuiDesignerHints.Checked:= GuiDesignerHints;
    CBSnapToGrid.Checked:= SnapToGrid;
    UDGridSize.Position:= GridSize;
    UDZoomSteps.Position:= ZoomSteps;
    EFrameWidth.Text:= IntToStr(FrameWidth);
    EFrameHeight.Text:= IntToStr(FrameHeight);
    EAppletWidth.Text:= IntToStr(AppletWidth);
    EAppletHeight.Text:= IntToStr(AppletHeight);

    // tab options
    CBBakFiles.Checked:= CreateBAKFiles;
    CBLoadFiles.Checked:= LoadFiles;
    CBUseInterpreterWindowAsConsole.Checked:= UseInterpreterWindowAsConsole;
    CBAcceptDefaultname.Checked:= AcceptDefaultname;
    CBRenameWhenSave.Checked:= RenameWhenSave;
    CBDebuggerProtocol.Checked:= DebuggerProtocol;
    CBTranslateCompilerErrors.Checked:= TranslateCompilerErrors;
    CBStrictJavaMode.Checked:= StrictJavaMode;
    UDFileHistory.Position:= MaxFileHistory;
    UDFontSize.Position:= Fontsize;
    EFileFilter.Text:= FileFilter;
    CBCheckAge.Checked:= CheckAge;
    ShortenPath(ETempFolder, TempDirWithUsername);

    // tab restrictions
    CBDosWindow.Checked:= DOSWindowLocked;
    CBBlockedInternet.Checked:= BlockedInternet;
    CBLockedPaths.Checked:= LockedPaths;
    CBLockedStructogram.Checked:= LockedStructogram;
    if not IsAdministrator then begin
      CBDosWindow.Enabled:= false;
      CBBlockedInternet.Enabled:= false;
      CBLockedPaths.Enabled:= false;
      CBLockedStructogram.Enabled:= false;
    end;

    // tab Associations
    CBAssociationJava.Checked:= HasAssociationWithJavaeditor('.java');
    CBAssociationJfm.Checked := HasAssociationWithJavaeditor('.jfm');
    CBAssociationUml.Checked := HasAssociationWithJavaeditor('.uml');
    CBAssociationJep.Checked := HasAssociationWithJavaeditor('.jep');
    CBAssociationHtml.Checked:= HasAssociationWithJavaeditor('.html');
    CBAssociationTxt.Checked := HasAssociationWithJavaeditor('.txt');
    CBAssociationJsp.Checked := HasAssociationWithJavaeditor('.jsp');
    CBAssociationPhp.Checked := HasAssociationWithJavaeditor('.php');
    CBAssociationCss.Checked := HasAssociationWithJavaeditor('.css');
    CBAssociationInc.Checked := HasAssociationWithJavaeditor('.inc');
    CBAssociationJsg.Checked := HasAssociationWithJavaeditor('.jsg');
    CBAssociationJsd.Checked := HasAssociationWithJavaeditor('.jsd');

    EAdditionalAssociations.Text:= AdditionalAssociations;
    EJEAssociation.Text:= getRegisteredJavaEditor;

    // tab UML
    CBValidClassColor.Selected:= ValidClassColor;
    CBInvalidClassColor.Selected:= InvalidClassColor;
    RGClassHead.ItemIndex:= ClassHead;
    CBObjectColor.Selected:= ObjectColor;
    RGObjectHead.ItemIndex:= ObjectHead;
    RGObjectFooter.ItemIndex:= ObjectFooter;
    RGObjectCaption.ItemIndex:= ObjectCaption;
    CBObjectUnderline.Checked:= ObjectUnderline;
    CBCommentColor.Selected:= CommentColor;

    RGAttributsMethodsDisplay.ItemIndex:= 4 - DiVisibilityFilter;
    RGSequenceAttributsMethods.ItemIndex:= DISortOrder;
    RGParameterDisplay.ItemIndex:= DIShowParameter;
    RGVisibilityDisplay.ItemIndex:= 2 - DIShowIcons;

    // tab uml options
    CBUMLEdit.Checked:= PrivateAttributEditable;
    CBShowEmptyRects.Checked:= ShowEmptyRects;
    CBShowFunctionvalues.Checked:= ShowFunctionvalues;
    CBConstructorWithVisibility.Checked:= ConstructorWithVisibility;
    CBLowerCaseLetter.Checked:= ObjectLowerCaseLetter;
    CBOpenPublicClasses.Checked:= ShowPublicOnly;
    CBDefaultModifiers.Checked:= DefaultModifiers;
    CBShowObjectsWithMethods.Checked:= ShowObjectsWithMethods;
    CBShowObjectsWithInheritedPrivateAttributes.Checked:= ShowObjectsWithInheritedPrivateAttributes;
    CBAttributesAParametersP.Checked:= AttributesAParametersP;
    UDShadowWidth.Position:= ShadowWidth;
    UDShadowIntensity.Position:= ShadowIntensity;
    CBUseVoid.Checked:= UseVoid;
    CBIntegerInsteadofInt.Checked:= IntegerInsteadofInt;
    CBShowAllNewObjects.Checked:= ShowAllNewObjects;
    CBObjectsWithoutVisibility.Checked:= ObjectsWithoutVisibility;
    CBArrayListAsIntegratedList.Checked:= ArrayListAsIntegratedList;
    CBRelationshipAttributesBold.Checked:= RelationshipAttributesBold;
    CBStartWithDatatype.Checked:= StartWithDataType;
    CBSetterWithoutThis.Checked:= SetterWithoutThis;
    CBShowClassparameterSeparately.Checked:= ShowClassparameterSeparately;
    CBRoleHidesAttribute.Checked:= RoleHidesAttribute;
    CBUseAbstract.Checked:= UseAbstract;

    // tab Visibility
    VisibilityModelToView;

    // tab LLM Assistant
    CopyProviders(LLMAssistant.Providers, TempProviders);
    CBProvider.ItemIndex:= Integer(TempProviders.Provider);
    LLMAssistantModelToView(LLMAssistantSettings);

    CopyProviders(FLLMChatForm.LLMChat.Providers, TempChatProviders);
    CBChatProvider.ItemIndex:= Integer(TempChatProviders.Provider);
    LLMChatModelToView(LLMChatSettings);

    // tab SVN
    ShortenPath(ESVNFolder, SVNFolder);
    ShortenPath(CBRepository, SVNRepository);

    // tab Git
    ShortenPath(EGitFolder, GitFolder);
    ShortenPath(CBLocalRepository, GitLocalRepository);
    ShortenPath(CBRemoteRepository, GitRemoteRepository);
    EUserName.Text:= GitUserName;
    EUserEMail.Text:= GitUserEMail;

    // tab JUnit
    ShortenPath(EJUnitJarFile, JUnitJarFile);
    ShortenPath(EJUnitManual, JUnitManual);
    EJUnitParameter.Text:= JUnitParameter;
    CBJUnitBeforeEach.Checked:= JUnitBeforeEach;
    CBJUnitAfterEach.Checked:= JUnitAfterEach;

    // tab Logfiles
    ShortenPath(ELogfileCompiler, LogfileCompilerWithUsername);
    ShortenPath(ELogfileInteractive, LogfileInteractiveWithUsername);
    ShortenPath(ELogfileExceptions, LogfileExceptionsWithUsername);

    // tab Structograms
    EAlgorithm.Text:= Algorithm;
    EInput.Text:= Input;
    EOutput.Text:= Output;
    EWhile.Text:= _While;
    EDoWhile.Text:= DoWhile;
    EFor.Text:= _For;
    EYes.Text:= Yes;
    ENo.Text:= No;
    EOther.Text:= Other;
    RGGenerateJavacode.ItemIndex:= GenerateJavaAsProgram;
    CBDatatype.text:= StructoDatatype;
    CBSwitchWithCaseLine.Checked:= SwitchWithCaseLine;
    ECaseCount.text:= IntTostr(CaseCount);
    UDStructogramShadowWidth.Position:= StructogramShadowWidth;
    UDStructogramShadowIntensity.Position:= StructogramShadowIntensity;

    // tab Sequence diagram
    ESDObject.Text:= SDObject;
    ESDNew.Text:= SDNew;
    ESDClose.Text:= SDClose;
    CBSDFillingColor.Selected:= SDFillingcolor;
    CBSDNoFilling.Checked:= SDNoFilling;
    CBSDShowMainCall.Checked:= SDShowMainCall;
    CBSDShowParameter.Checked:= SDShowParameter;
    CBSDShowReturn.Checked:= SDShowReturn;

    // tab Styles
    ECurrentStyle.Text:= TStyleManager.ActiveStyle.Name;
    ECurrentStyle.Hint:= ECurrentStyle.Text;
    StyleSelectorShow;
    CollectDocumentations;
  end;
  RGLanguages.ItemIndex:= max(0, fLanguagesList.IndexOf(LanguageCode));
end;  // ModelToView

procedure TFConfiguration.ModelToRegistry;
  var cp1, cp2: string;
begin
  // Sonstiges
  WriteStringU('Program', 'StartClass', RemovePortableDrive(JavaStartClass));

  // tab Interpreter
  WriteStringDirectory('Java', 'JDK-Folder', JDKFolder);
  WriteStringU('Java', 'JDK-FolderItems', RemovePortableDrives(SaveComboBoxItems(JDKFolderItems)));

  WriteStringFile('Java', 'Interpreter', JavaInterpreter);
  WriteStringU('Program', 'InterpreterParameter', JavaInterpreterParameter);
  cp1:= AddPortableDrives(ReadStringM('Java', 'Classpath', ''));
  WriteStringDirectory('Java', 'JavaFX-Folder', RemovePortableDrive(JavaFXFolder));
  WriteStringU('Program', 'JavaFXParameter', JavaFXParameter);

  WriteStringM('Java', 'Classpath', RemovePortableDrives(JavaClasspathAdmin));
  cp2:= AddPortableDrives(ReadStringU('Program', 'Classpath', ''));
  WriteStringU('Program', 'Classpath', RemovePortableDrives(JavaClasspathUser));
  if (cp1 <> JavaClasspathAdmin) or (cp2 <> JavaClasspathUser) then
    MakeClasspathClasses;

  WriteStringU('Program', 'ClasspathAll', RemovePortableDrives(JavaClasspathAll));
  WriteStringU('Program', 'FileEncoding', FileEncoding);
  WriteStringU('Editor', 'Codepage', Codepage);
  WriteBoolU('Program', 'ShowInterpreterCall', ShowInterpreterCall);

  // tab Compiler
  WriteStringFile('Java', 'JavaCompiler', JavaCompiler);
  WriteStringU('Program', 'JavaCompilerParameter', JavaCompilerParameter);
  WriteBoolU('Program', 'CompileInternally', CompileInternally);
  WriteBoolU('Program', 'ShowCompilerCall', ShowCompilerCall);
  WriteStringU('Program', 'CompilerEncoding', CompilerEncoding);

  // tab Programme
  WriteStringFile('Java', 'Debugger', JavaDebugger);
  WriteStringFile('Java', 'JavaDoc', JavaDoc);
  WriteStringU('Program', 'DocParameter', JavaDocParameter);

  // tab Applets
  WriteStringFile('Java', 'Appletviewer', JavaAppletviewer);
  WriteIntegerU('Program', 'AppletStart', AppletStart);
  WriteBoolU('Program', 'ShowHTMLforApplet', ShowHTMLforApplet);
  WriteStringU('Program', 'TempDir', RemovePortableDrive(TempDirWithUsername));

  // tab Jar
  WriteStringFile('Java', 'Jar', JavaJar);
  WriteStringU('Jar', 'JarParameter', JavaJarParameter);
  WriteStringU('Jar', 'JarManifest', JavaJarManifest);
  WriteStringU('Jar', 'JarCreate', JarCreateCurrent);
  WriteStringU('Jar', 'JarCreateAll', JarCreateAll);
  WriteStringU('Jar', 'JarPack', JarPackFiles);
  WriteStringU('Jar', 'JarClassPath', JarClassPath);

  // tab Disassembler
  WriteStringFile('Java', 'Disassembler', JavaDisassembler);
  WriteStringU('Program', 'DisassemblerParameter', JavaDisassemblerParameter);
  WriteStringU('Program', 'DisassemblerItems', RemovePortableDrives(SaveComboBoxItems(CBDisassembler.Items.Text)));

  // tab templates
  WriteStringFile('Templates', 'Program', Templates[1]);
  WriteStringFile('Templates', 'Frame',   Templates[2]);
  WriteStringFile('Templates', 'Dialog',  Templates[3]);
  WriteStringFile('Templates', 'Applet',  Templates[4]);
  WriteStringFile('Templates', 'JFrame',  Templates[5]);
  WriteStringFile('Templates', 'JDialog', Templates[6]);
  WriteStringFile('Templates', 'JApplet', Templates[7]);
  WriteStringFile('Templates', 'Application', Templates[8]);
  WriteStringFile('Templates', 'Controlstructures', Templates[9]);
  WriteStringFile('Templates', 'Class', Templates[10]);
  WriteStringFile('Templates', 'Mindstorms', Templates[11]);
  WriteStringFile('Templates', 'JunitTest', Templates[12]);

  // tab documentation
  WriteStringFile('Program', 'Manual', JavaManual);
  WriteStringU('Program', 'ManualItems', RemovePortableDrives(SaveComboBoxItems(JavaManualItems)));
  WriteStringFile('Program', 'ManualFX', JavaManualFX);
  WriteStringU('Program', 'ManualFXItems', RemovePortableDrives(SaveComboBoxItems(JavaManualFXItems)));
  WriteStringU('Program', 'JavaDocs', RemovePortableDrives(JavaJavaDocs));
  WriteStringU('Program', 'JavaDocsAll', RemovePortableDrives(JavaJavaDocsAll));
  WriteStringFile('Program', 'Tutorial', JavaTutorial);
  WriteStringFile('Program', 'Javabook', JavaBook);
  WriteStringDirectory('Program', 'Cache', JavaCacheWithUsername);
  WriteIntegerU('Program', 'MaxSearch', MaxSearch);
  WriteIntegerU('Program', 'Update', ReadIntegerU('Program', 'Update', 0));

  // tab Editor
  WriteIntegerU('Editor', 'TabWidth', TabWidth);
  WriteIntegerU('Editor', 'Indent', Indent);
  WriteBoolU('Editor', 'IndentHelp', IndentHelp);
  WriteBoolU('Editor', 'CursorBehindLine', CursorBehindLine);
  WriteBoolU('Editor', 'AutomaticIndent', AutomaticIndent);
  WriteBoolU('Editor', 'Linenumbering', LineNumbering);
  WriteBoolU('Editor', 'CompleteBracket', AddClosingBracket);
  WriteBoolU('Editor', 'IndentAfterBracket', IndentAfterBracket);
  WriteBoolU('Editor', 'BracketPair', ShowBracketPair);
  WriteBoolU('Editor', 'CommentClosingBrackets', CommentClosingBrackets);
  WriteBoolU('Editor', 'StructureColoring', StructureColoring);
  WriteBoolU('Editor', 'StructureColoringPlane', StructureColoringPlane);
  WriteIntegerU('Editor', 'StructureColorIntensity', StructureColorIntensity);
  WriteBoolU('Editor', 'GUICodeFolding', GUICodeFolding);
  WriteBoolU('Editor', '80ColumnLine', EightyColumnLine);
  WriteBoolU('Editor', 'InsertControlStructures', InsertControlStructures);
  WriteBoolU('Editor', 'InsertSemicolons', InsertSemicolons);

  // tab Code
  WriteBoolU('Code', 'Code-Completion-Always', CodeCompletionAlways);
  WriteBoolU('Code', 'Code-Completion', CodeCompletionCtrlSpace);
  WriteBoolU('Code', 'Parameter-Hints', ParameterHints);
  WriteBoolU('Code', 'ShowClassObject', ShowClassObject);
  WriteIntegerU('Code', 'Delay', CodeDelay);
  WriteBoolU('Code', 'TooltipWithKey', TooltipWithKey);
  WriteBoolU('Code', 'TooltipAutomatic', TooltipAutomatic);
  WriteIntegerU('Code', 'SelectionSizeMin', SelectionSizeMin);
  WriteIntegerU('Code', 'SelectionSizeMax', SelectionSizeMax);
  WriteIntegerU('Code', 'TooltipDelay', TooltipDelay);
  WriteIntegerU('Code', 'TooltipWidth', TooltipWidth);
  WriteIntegerU('Code', 'TooltipHeight', TooltipHeight);
  WriteIntegerU('Code', 'TooltipFontSize', TooltipFontSize);

  // tab keyboard
  WriteStringFile('Editor', 'Keyboardfile', KeyboardFile);

  // tab colors
  SaveUserColors;
  WriteBoolU('Colors', 'NoActiveLineColor', NoActiveLineColor);
  WriteIntegerU('Colors', 'ActiveLineColor', ActiveLineColor);
  WriteBoolU('Colors', 'NoSyntaxHighlighting', NoSyntaxHighlighting);
  WriteStringU('Colors', 'GUIStyle', GUIStyle);
  WriteStringU('Colors', 'EditorStyle', EditorStyle);

  // tab Browser
  WriteBoolU('Browser', 'UseIEinternForDocuments', UseIEinternForDocuments);
  WriteBoolU('Browser', 'OnlyOneBrowserWindow', OnlyOneBrowserWindow);
  WriteStringU('Browser', 'BrowserOpenKeys', BrowserOpenKeys);
  WriteStringFile('Browser', 'Browser', BrowserProgram);
  WriteStringU('Browser', 'Title', BrowserTitle);
  WriteBoolU('Browser', 'ProxyEnabled', withProxy);
  WriteStringU('Browser', 'ProxyIP', ProxyIP);
  WriteIntegerU('Browser', 'ProxyPort', ProxyPort);

  // tab printer
  WriteIntegerU('Printer', 'Left',   BorderLeft);
  WriteIntegerU('Printer', 'Top',    BorderTop);
  WriteIntegerU('Printer', 'Right',  BorderRight);
  WriteIntegerU('Printer', 'Bottom', BorderBottom);
  WriteStringU('Printer', 'Header', Header);
  WriteStringU('Printer', 'Footer', Footer);
  WriteBoolU('Printer', 'Linenumbers', WithLinenumbers);
  WriteBoolU('Printer', 'LinenumbersInMargin', LinenumbersInMargin);
  WriteBoolU('Printer', 'PrintColored', PrintColored);
  try
    if hasDefaultPrinter
      then WriteStringU('Printer', 'Printer', Printer.Printers[Printer.PrinterIndex])
      else WriteStringU('Printer', 'Printer', '');
  except
  end;
  
  // tab comment
  if RGComment.ItemIndex = 2 then
    WriteStringU('Comment', 'Comment', FreeComment);
  WriteIntegerU('Comment', 'Type', CommentKind);
  WriteStringU('Comment', 'Author', JavaAuthor);
  WriteStringU('Comment', 'Method', SaveComboBoxItems(MethodComment));

  // tab Mindstorms
  WriteStringDirectory('Mindstorms', 'LejosFolder', LejosVerzeichnis);
  WriteStringFile('Mindstorms', 'LejosCompiler', LejosCompiler);
  WriteStringFile('Mindstorms', 'LejosUploader', LejosUploader);
  WriteStringFile('Mindstorms', 'LejosFlasher', LejosFlasher);
  WriteStringFile('Mindstorms', 'Manual', MindstormsManual);
  WriteIntegerU('Mindstorms', 'Port', MindstormsPort);
  WriteStringU('Mindstorms', 'IP', MindstormsIP);
  WriteStringU('Mindstorms', 'Parameter', MindstormsParameter);
  WriteBoolU('Mindstorms', 'Mode', MindstormsMode);
  WriteIntegerU('Mindstorms', 'Version', MindstormsVersion);

  WriteStringDirectory('Mindstorms', 'RCXFolder', RCXFolder);
  WriteStringDirectory('Mindstorms', 'NXTFolder', NXTFolder);
  WriteStringDirectory('Mindstorms', 'EV3Folder', EV3Folder);
  WriteStringFile('Mindstorms', 'RCXCompiler', RCXCompiler);
  WriteStringFile('Mindstorms', 'NXTCompiler', NXTCompiler);
  WriteStringFile('Mindstorms', 'RCXUploader', RCXUploader);
  WriteStringFile('Mindstorms', 'NXTUploader', NXTUploader);
  WriteStringFile('Mindstorms', 'RCXFlasher', RCXFlasher);
  WriteStringFile('Mindstorms', 'NXTFlasher', NXTFlasher);
  WriteStringFile('Mindstorms', 'RCXManual', RCXManual);
  WriteStringFile('Mindstorms', 'NXTManual', NXTManual);
  WriteStringFile('Mindstorms', 'EV3Manual', EV3Manual);

  // tab android
  WriteBoolU('Android', 'Mode', AndroidMode);
  WriteStringDirectory('Android', 'AndroidSDKFolder', AndroidSDKFolder);

  // tag GUI designer
  WriteBoolU('Options', 'NameFromText', NameFromText);
  WriteBoolU('Options', 'GuiDesignerHints', GuiDesignerHints);
  WriteBoolU('Options', 'SnapToGrid', SnapToGrid);
  WriteIntegerU('Options', 'GridSize', GridSize);
  WriteIntegerU('Options', 'ZoomSteps',ZoomSteps);
  WriteIntegerU('Options', 'GUIFontSize', GUIFontSize);
  WriteStringU('Options', 'GUIFontName', GUIFontName);
  WriteIntegerU('Editor', 'FrameWidth', FrameWidth);
  WriteIntegerU('Editor', 'FrameHeight', FrameHeight);
  WriteIntegerU('Editor', 'AppletWidth', AppletWidth);
  WriteIntegerU('Editor', 'AppletHeight', AppletHeight);

  // tab options
  WriteBoolU('Options', 'BAKFiles', CreateBAKFiles);
  WriteBoolU('Options', 'LoadFiles', LoadFiles);
  WriteBoolU('Options', 'UseInterpreterWindowAsConsole', UseInterpreterWindowAsConsole);
  WriteBoolU('Options', 'AcceptDefaultname', AcceptDefaultname);
  WriteBoolU('Options', 'ComponentsToolbar', ComponentsToolbar);
  WriteBoolU('Options', 'BorderLayout', BorderLayout);
  WriteBoolU('Options', 'Rename', RenameWhenSave);
  WriteBoolU('Options', 'DebuggerProtocol', DebuggerProtocol);
  WriteBoolU('Options', 'TranslateCompilerErrors', TranslateCompilerErrors);
  WriteBoolU('Options', 'StrictJavaMode', StrictJavaMode);
  WriteBoolU('Options', 'CheckAge', CheckAge);
  WriteIntegerU('Options', 'Fontsize', Fontsize);
  WriteIntegerU('Options', 'MaxFileHistory', UDFileHistory.Position);
  WriteStringU('Options', 'FileFilter', FileFilter);

  // tab restrictions
  WriteBoolM('Java', 'DOS-Window', DOSWindowLocked);
  WriteBoolM('Java', 'BlockedInternet', BlockedInternet);
  WriteBoolM('Java', 'LockedPaths', LockedPaths);
  WriteBoolM('Java', 'LockedStructogram', LockedStructogram);

  // tab Associations
  WriteStringU('Associations', 'Additional', AdditionalAssociations);

  // tab UML
  WriteIntegerU('UML', 'ValidClassColor', ValidClassColor);
  WriteIntegerU('UML', 'InvalidClassColor', InvalidClassColor);
  WriteIntegerU('UML', 'ClassHead', ClassHead);
  WriteIntegerU('UML', 'ShadowWidth', ShadowWidth);
  WriteIntegerU('UML', 'ShadowIntensity', ShadowIntensity);
  WriteIntegerU('UML', 'ObjectColor', ObjectColor);
  WriteIntegerU('UML', 'ObjectHead', ObjectHead);
  WriteIntegerU('UML', 'ObjectFooter', ObjectFooter);
  WriteIntegerU('UML', 'ObjectCaption', ObjectCaption);
  WriteBoolU('UML', 'ObjectUnderline', ObjectUnderline);
  WriteIntegerU('UML', 'CommentColor', CommentColor);

  WriteIntegerU('UML', 'Visibility', DiVisibilityFilter);
  WriteIntegerU('UML', 'ShowParameter', DiShowParameter);
  WriteIntegerU('UML', 'ShowIcons',  DiShowIcons);
  WriteIntegerU('UML', 'SortOrder', DiSortOrder);

  // tab uml options
  WriteBoolU('UML', 'PrivateAttribut', PrivateAttributEditable);
  WriteBoolU('UML', 'ShowEmptyRects', ShowEmptyRects);
  WriteBoolU('UML', 'ShowFunctionvalues', ShowFunctionvalues);
  WriteBoolU('UML', 'ConstructorWithVisibility', ConstructorWithVisibility);
  WriteBoolU('UML', 'ObjectLowerCaseLetter', ObjectLowerCaseLetter);
  WriteBoolU('UML', 'ShowPublicOnly', ShowPublicOnly);
  WriteBoolU('UML', 'DefaultModifiers', DefaultModifiers);
  WriteBoolU('UML', 'ShowObjectsWithMethods', ShowObjectsWithMethods);
  WriteBoolU('UML', 'ShowObjectsWithInheritedPrivateAttributes', ShowObjectsWithInheritedPrivateAttributes);
  WriteBoolU('UML', 'AttributesAParametersP', AttributesAParametersP);
  WriteBoolU('UML', 'UseVoid', UseVoid);
  WriteBoolU('UML', 'IntegerInsteadofInt', IntegerInsteadofInt);
  WriteBoolU('UML', 'ShowAllNewObjects', ShowAllNewObjects);
  WriteBoolU('UML', 'ObjectswithoutVisibility', ObjectswithoutVisibility);
  WriteBoolU('UML', 'ArrayListAsIntegratedList', ArrayListAsIntegratedList);
  WriteBoolU('UML', 'RelationshipAttributesBold', RelationshipAttributesBold);
  WriteBoolU('UML', 'StartWithDatatype', StartWithDatatype);
  WriteBoolU('UML', 'SetterWithoutThis', SetterWithoutThis);
  WriteBoolU('UML', 'ShowClassparameterSeparately', ShowClassparameterSeparately);
  WriteBoolU('UML', 'RoleHidesAttribute', RoleHidesAttribute);
  WriteBoolU('UML', 'UseAbstract', UseAbstract);

  // tab Checkstyle
  WriteStringFile('Checkstyle', 'Checkstyle', Checkstyle);
  WriteStringFile('Checkstyle', 'Configurationfile', CheckKonfiguration);
  WriteStringU('Checkstyle', 'CheckParameter', CheckParameter);

  // tab Jalopy
  WriteStringFile('Jalopy', 'Jalopy', Jalopy);
  WriteStringFile('Jalopy', 'JalopyConfiguration', JalopyConfiguration);
  WriteStringU('Jalopy', 'JalopyParameter', JalopyParameter);

  // tab SVN
  WriteStringDirectory('SVN', 'SVNFolder', SVNFolder);
  WriteStringDirectory('SVN', 'SVNRepository', SVNRepository);

  // tab Git
  WriteStringDirectory('Git', 'GitFolder', GitFolder);
  WriteStringDirectory('Git', 'GitLocalRepository', GitLocalRepository);
  WriteStringDirectory('Git', 'GitRemoteRepository', GitRemoteRepository);
  WriteStringU('Git', 'user.name', GitUserName);
  WriteStringU('Git', 'user.email', GitUserEMail);

  // tab JUnit
  WriteStringFile('JUnit', 'JUnitJarFile', JUnitJarFile);
  WriteStringFile('JUnit', 'JUnitManual', JUnitManual);
  WriteStringU('JUnit', 'JUnitParameter', JUnitParameter);
  WriteBoolU('JUnit', 'JUnitBeforeeach', JUnitBeforeEach);
  WriteBoolU('JUnit', 'JUnitafterEach', JUNitAfterEach);

  //tab Logfiles
  WriteStringFile('Logfiles', 'LogfileCompiler', LogfileCompilerWithUsername);
  WriteStringFile('Logfiles', 'LogfileInteractive', LogfileInteractiveWithUsername);
  WriteStringFile('Logfiles', 'LogfileExceptions', LogfileExceptionsWithUsername);

  // tab Structograms
  // write for old Language
  StructogramS:= 'Structogram.' + LanguageCode;
  WriteStringU(StructogramS, 'Algorithm', Algorithm);
  WriteStringU(StructogramS, 'Input', Input);
  WriteStringU(StructogramS, 'Output', Output);
  WriteStringU(StructogramS, 'While', _While);
  WriteStringU(StructogramS, 'DoWhile', DoWhile);
  WriteStringU(StructogramS, 'For', _For);
  WriteStringU(StructogramS, 'Yes', Yes);
  WriteStringU(StructogramS, 'No', No);
  WriteStringU(StructogramS, 'Other', Other);
  WriteIntegerU('Structogram','GenerateJavaAsProgram', GenerateJavaAsProgram);
  WriteStringU('Structogram', 'Datatype', StructoDatatype);
  WriteBoolU('Structogram', 'SwitchWithCaseLine', SwitchWithCaseLine);
  WriteIntegerU('Structogram', 'CaseCount', CaseCount);
  WriteIntegerU('Structogram', 'ShadowWidth', StructogramShadowWidth);
  WriteIntegerU('Structogram', 'ShadowIntensity', StructogramShadowIntensity);

  // tab Sequence diagram
  SequencediagramS:= 'Sequencediagram.' + LanguageCode;
  WriteStringU(SequencediagramS, 'Object', SDObject);
  WriteStringU(SequencediagramS, 'New', SDNew);
  WriteStringU(SequencediagramS, 'Close', SDClose);
  WriteIntegerU(SequencediagramS, 'Fillingcolor', SDFillingcolor);
  writeBoolU(SequencediagramS, 'NoFilling', SDNoFilling);
  WriteBoolU(SequencediagramS, 'ShowMainCall', SDShowMainCall);
  WriteBoolU(SequencediagramS, 'ShowParameter', SDShowParameter);
  WriteBoolU(SequencediagramS, 'ShowReturn', SDShowReturn);

  // no change Languagecode
  if RGLanguages.ItemIndex = -1
    then LanguageCode:= 'en_GB'
    else LanguageCode:= fLanguagesList[RGLanguages.ItemIndex];
  WriteStringU('Options', 'Language', LanguageCode);
  WriteIntegerU('Panel', 'RightDockPanelWidth', RightDockPanelWidth);
  WriteIntegerU('Panel', 'BottomDockPanelHeight', BottomDockPanelHeight);

  // tab Providers
  WriteProviders('LLMAssistant', LLMAssistant.Providers);
  WriteProviders('LLMChat', FLLMChatForm.LLMChat.Providers);

  JavaVersion:= 0;
  DocumentationVersion:= 0;
  SaveVisibility;

  if not UseRegistry then begin
    if not WriteProtection and assigned(MachineIniFile) then
      try
        MachineIniFile.UpdateFile;
      except
        on e: exception do
          ErrorMsg(e.Message);      
      end;
    if assigned(UserIniFile) then
      try
        UserIniFile.UpdateFile;
      except
        on e: exception do
          ErrorMsg(e.Message);
      end;
  end;
end; // ModelToRegistry

procedure TFConfiguration.ViewToModel;
  var s: string;
begin
  with FJava do begin
    // tab Interpreter
    JDKFolder:= ExtendPath(CBJDKFolder);
    JDKFolderItems:= CBJDKFolder.Items.Text;
    JavaInterpreter:= ExtendPath(EInterpreter);
    JavaInterpreterParameter:= EInterpreterParameter.Text;
    {$WARN SYMBOL_PLATFORM OFF}
    JavaFXFolder:= ExcludeTrailingBackslash(EJavaFXFolder.Text);
    {$WARN SYMBOL_PLATFORM ON}
    JavaFXParameter:= EJavaFXParameter.Text;
    JavaClasspathAdmin:= EClasspathAdmin.Text;
    JavaClasspathUser:= EClasspathUser.Text;
    FileEncoding:= CBFileEncoding.Text;
    Codepage:= CBCodepage.Text;
    ShowInterpreterCall:= CBShowInterpreterCall.Checked;

    // tab Compiler
    JavaCompiler:= ExtendPath(EJavaCompiler);
    JavaCompilerParameter:= EJavaCompilerParameter.Text;
    CompileInternally:= CBUseJavaCompilerInternally.Checked;
    ShowCompilerCall:= CBShowCompilerCall.Checked;
    CompilerEncoding:= CBCompilerEncoding.Text;

    // tab Programme
    JavaAppletviewer:= ExtendPath(EAppletviewer);
    JavaDebugger:= ExtendPath(EDebugger);
    JavaDoc:= ExtendPath(EJavaDoc);
    JavaDocParameter:= EDocParameter.Text;

    // tab Disassembler
    s:= ReadStringU('Program', 'DisassemblerItems', '');
    if s <> '' then begin
      s:= AddPortableDrives(LoadComboBoxItems(s));
      CBDisassembler.Items.Text:= s;
    end;
    JavaDisassembler:= ExtendPath(CBDisassembler);
    JavaDisassemblerItems:= LoadComboBoxItems(s);
    JavaDisassemblerParameter:= EDisassemblerParameter.Text;

    // tab Checkstyle
    Checkstyle:= ExtendPath(ECheckstyle);
    CheckKonfiguration:= ExtendPath(ECheckstyleConfiguration);
    CheckParameter:= ECheckstyleParameter.Text;

    // tab Jalopy
    Jalopy:= ExtendPath(EJalopy);
    JalopyConfiguration:= ExtendPath(EJalopyConfiguration);
    JalopyParameter:= EJalopyParameter.Text;

    // tab Jar
    JavaJar:= ExtendPath(EJar);
    JavaJarParameter:= EJarParameter.Text;
    JavaJarManifest:= EJarManifest.Text;
    JarPackFiles:= CBJarPack.Text;
    JarClassPath:= ReplaceStr(EJarClasspath.Text, ';', ' ');

    // tab Applet
    AppletStart:= RGApplet.ItemIndex;
    ShowHTMLForApplet:= CBShowHTMLforApplet.checked;

    // tab templates
    Templates[ 1]:= ExtendPath(ETemplateConsole);
    Templates[ 2]:= ExtendPath(ETemplateFrame);
    Templates[ 3]:= ExtendPath(ETemplateDialog);
    Templates[ 4]:= ExtendPath(ETemplateApplet);
    Templates[ 5]:= ExtendPath(ETemplateJFrame);
    Templates[ 6]:= ExtendPath(ETemplateJDialog);
    Templates[ 7]:= ExtendPath(ETemplateJApplet);
    Templates[ 8]:= ExtendPath(ETemplateApplication);
    Templates[ 9]:= ExtendPath(ETemplateControlstructure);
    Templates[10]:= ExtendPath(ETemplateClass);
    Templates[11]:= ExtendPath(EMindstormsTemplate);
    Templates[12]:= ExtendPath(ETemplateJUnitTest);

    // tab documentation
    JavaManual:= ExtendPath(CBManual);
    JavaManualItems:= CBManual.Items.Text;
    JavaManualFX:= ExtendPath(CBManualFX);
    JavaManualFXItems:= CBManualFX.Items.Text;
    JavaTutorial:= ExtendPath(ETutorial);
    Javabook:= ExtendPath(EJavabook);
    JavaCache:= ExtendPath(ECache);
    MakeJavaCache(JavaCache);
    MaxSearch:= UDMaxSearch.Position;

    // tab Editor
    TabWidth:= UDTabWidth.Position;
    Indent:= UDIndent.Position;
    IndentHelp:= CBIndentHelp.Checked;
    CursorBehindLine:= CBCursorBehindLine.Checked;
    AutomaticIndent:= CBAutomaticIndent.Checked;

    LineNumbering:= CBLineNumbering.Checked;
    AddClosingBracket:= CBCompleteBracket.Checked;
    IndentAfterBracket:= CBIndentAfterBracket.Checked;
    ShowBracketPair:= CBShowBracketpair.Checked;
    CommentClosingBrackets:= CBCommentClosingBrackets.Checked;
    StructureColoring:= CBStructureColoring.Checked;
    StructureColoringPlane:= CBStructureColoringPlane.Checked;
    GUICodeFolding:= CBGUICodeFolding.Checked;
    EightyColumnLine:= CB80ColumnLine.Checked;

    StructureColorIntensity:= UDIntensity.Position;
    InsertControlStructures:= CBInsertControlStructures.Checked;
    InsertSemicolons:= CBInsertSemicolons.Checked;

    // tab Code
    CodeCompletionAlways:= RBCodeCompletionAlways.Checked;
    CodeCompletionCtrlSpace:= RBCodeCompletionCtrlSpace.Checked;
    if assigned(FJava.EditorForm) then
      FJava.scpSetEditForm(FJava.Editorform);
    ParameterHints:= CBParameterHints.Checked;
    ShowClassObject:= CBShowClassObject.Checked;
    CodeDelay:= TBDelay.Position;
    SelectionSizeMin:= StrToInt(ESelectionSizeMin.Text);
    SelectionSizeMax:= StrToInt(ESelectionSizeMax.Text);
    if SelectionSizeMax < SelectionSizeMin then
      SelectionSizeMax:= SelectionSizeMin;
    TooltipWithKey:= CBTooltipWithKey.Checked;
    TooltipAutomatic:= CBTooltipAutomatic.Checked;
    TooltipDelay:= TBTooltipDelay.Position;

    // tab Browser
    UseIEinternForDocuments:= CBUseIEinternForDocuments.Checked;
    OnlyOneBrowserWindow:= CBOnlyOneBrowserWindow.Checked;
    BrowserTitle:= EBrowsertitle.Text;
    BrowserOpenKeys:= CBOpenBrowserShortcut.Text;
    BrowserProgram:= ExtendPath(EBrowserProgram);
    ProxyIP:= EProxyIP.Text;
    ProxyPort:= StrToInt(EProxyPort.Text);
    withProxy:= CBUseProxy.checked;

    // tab printer
    BorderLeft:= StrToInt(EBorderLeft.Text);
    BorderTop:= StrToInt(EBorderTop.Text);
    BorderRight:= StrToInt(EBorderRight.Text);
    BorderBottom:= StrToInt(EBorderBottom.Text);
    Header:= EHeader.Text;
    Footer:= EFooter.Text;
    WithLinenumbers:= CBLinenumbers.Checked;
    LinenumbersInMargin:= CBLinenumbersInBorder.Checked;
    PrintColored:= CBPrintColored.Checked;

    // tab comment
    CommentKind:= RGComment.ItemIndex;
    JavaAuthor:= EAuthor.Text;
    MethodComment:= MMethodComment.Text;

    // tab keyboard
    KeyboardFile:= ExtendPath(EKeyboardFile);

    // tab Mindstorms
    MindstormsParameter:= EMindstormsParameter.Text;
    MindstormsPort:= CBMindstormsPort.ItemIndex;
    MindstormsIP:= EMindstormsIP.Text;
    MindstormsMode:= CBMindstormsModus.Checked;
    MindstormsVersion:= RGMindStormsVersion.ItemIndex;
    case MindstormsVersion of
      0: begin
        RCXManual:= ExtendPath(EMindstormsManual);
        RCXFolder:= ExtendPath(ELejosFolder);
        RCXCompiler:= ExtendPath(ELejosCompiler);
        RCXUploader:= ExtendPath(ELejosUploader);
        RCXFlasher:= ExtendPath(ELejosFlasher);
      end;
      1: begin
        NXTManual:= ExtendPath(EMindstormsManual);
        NXTFolder:= ExtendPath(ELejosFolder);
        NXTCompiler:= ExtendPath(ELejosCompiler);
        NXTUploader:= ExtendPath(ELejosUploader);
        NXTFlasher:= ExtendPath(ELejosFlasher);
      end;
      2: begin
        EV3Manual:= ExtendPath(EMindstormsManual);
        EV3Folder:= ExtendPath(ELejosFolder);
      end;
    end;

    case MindstormsVersion of
      0: begin
         LejosVerzeichnis:= RCXFolder;
         LejosCompiler:= RCXCompiler;
         LejosUploader:= RCXUploader;
         LejosFlasher:= RCXFlasher;
         MindstormsManual:= RCXManual;
      end;
      1: begin
         LejosVerzeichnis:= NXTFolder;
         LejosCompiler:= NXTCompiler;
         LejosUploader:= NXTUploader;
         LejosFlasher:= NXTFlasher;
         MindstormsManual:= NXTManual;
      end;
      2: begin
         LejosVerzeichnis:= EV3Folder;
         MindstormsManual:= EV3Manual;
      end;
    end;

    //tab Android
    AndroidMode:= CBAndroidMode.Checked;
    AndroidSDKFolder:= EAndroidSDKFolder.Text;

    // tab GUI designer
    NameFromText:= CBNameFromText.Checked;
    GuiDesignerHints:= CBGuiDesignerHints.Checked;
    SnapToGrid:= CBSnapToGrid.Checked;
    GridSize:= UDGridSize.Position;
    ZoomSteps:= UDZoomSteps.Position;
    FrameWidth:= StrToInt(EFrameWidth.Text);
    FrameHeight:= strToInt(EFrameHeight.Text);
    AppletWidth:= StrToInt(EAppletWidth.Text);
    AppletHeight:= StrToInt(EAppletHeight.Text);

    // tab options
    CreateBAKFiles:= CBBakFiles.Checked;
    LoadFiles:= CBLoadFiles.Checked;
    UseInterpreterWindowAsConsole:= CBUseInterpreterWindowAsConsole.Checked;
    AcceptDefaultname:= CBAcceptDefaultname.Checked;
    RenameWhenSave:= CBRenameWhenSave.Checked;
    DebuggerProtocol:= CBDebuggerProtocol.Checked;
    TranslateCompilerErrors:= CBTranslateCompilerErrors.Checked;
    StrictJavaMode:= CBStrictJavaMode.Checked;

    MaxFileHistory:= UDFileHistory.Position;
    Fontsize:= UDFontSize.Position;

    FileFilter:= EFileFilter.Text;
    CheckAge:= CBCheckAge.Checked;
    TempDirWithUsername:= IncludeTrailingPathDelimiter(ExtendPath(ETempFolder));
    TempDir:= dissolveUsername(TempDirWithUsername);

    // tab restrictions
    DOSWindowLocked:= CBDosWindow.Checked;
    BlockedInternet:= CBBlockedInternet.Checked;
    LockedPaths:= CBLockedPaths.Checked;
    LockedStructogram:= CBLockedStructogram.Checked;

    // tab Associations
    AdditionalAssociations:= EAdditionalAssociations.Text;

    // tab UML
    ValidClassColor:= CBValidClassColor.Selected;
    InvalidClassColor:= CBInvalidClassColor.Selected;
    ClassHead:= RGClassHead.ItemIndex;
    ShadowWidth:= UDShadowWidth.Position;
    ShadowIntensity:= UDShadowIntensity.Position;
    ObjectColor:= CBObjectColor.Selected;
    ObjectHead:= RGObjectHead.ItemIndex;
    ObjectFooter:= RGObjectFooter.ItemIndex;
    ObjectCaption:= RGObjectCaption.ItemIndex;
    ObjectUnderline:= CBObjectUnderline.Checked;
    CommentColor:= CBCommentColor.Selected;

    DIVisibilityFilter:= 4 - RGAttributsMethodsDisplay.ItemIndex;
    DISortOrder:= RGSequenceAttributsMethods.ItemIndex;
    DIShowParameter:= RGParameterDisplay.ItemIndex;
    DIShowIcons:= 2 - RGVisibilityDisplay.ItemIndex;

    // tab uml options
    PrivateAttributEditable:= CBUMLEdit.Checked;
    ShowEmptyRects:= CBShowEmptyRects.Checked;
    ShowFunctionvalues:= CBShowFunctionvalues.Checked;
    ConstructorWithVisibility:= CBConstructorWithVisibility.Checked;
    ObjectLowerCaseLetter:= CBLowerCaseLetter.Checked;
    ShowPublicOnly:= CBOpenPublicClasses.Checked;
    DefaultModifiers:= CBDefaultModifiers.Checked;
    ShowObjectsWithMethods:= CBShowObjectsWithMethods.Checked;
    ShowObjectsWithInheritedPrivateAttributes:= CBShowObjectsWithInheritedPrivateAttributes.Checked;
    AttributesAParametersP:= CBAttributesAParametersP.Checked;
    UseVoid:= CBUseVoid.Checked;
    IntegerInsteadofInt:= CBIntegerInsteadofInt.Checked;
    ShowAllNewObjects:= CBShowAllNewObjects.Checked;
    ObjectsWithoutVisibility:= CBObjectswithoutVisibility.Checked;
    ArrayListAsIntegratedList:= CBArrayListAsIntegratedList.Checked;
    RelationshipAttributesBold:= CBRelationshipAttributesBold.Checked;
    StartWithDatatype:= CBStartWithDatatype.Checked;
    SetterWithoutThis:= CBSetterWithoutThis.Checked;
    ShowClassparameterSeparately:= CBShowClassparameterSeparately.Checked;
    RoleHidesAttribute:= CBRoleHidesAttribute.Checked;
    UseAbstract:= CBUseAbstract.Checked;

    // tab color
    NoActiveLineColor:= CBNoActiveLineColor.Checked;
    if NoActiveLineColor
      then ActiveLineColor:= clNone
      else ActiveLineColor:= CBActiveLineColor.Selected;
    CBActiveLineColor.Enabled:= NoActiveLineColor;
    NoSyntaxHighlighting:= CBNoSyntaxHighlighting.Checked;
    EditorStyle:= CBEditorStyles.Text;
    //ReadUserColors;

    // tab SVN
    SVNFolder:= ExtendPath(ESVNFolder);
    SVNRepository:= ExtendPath(CBRepository);

    // tab Git
    GitFolder:= ExtendPath(EGitFolder);
    GitLocalRepository:= ExtendPath(CBLocalRepository);
    GitRemoteRepository:= ExtendPath(CBRemoteRepository);
    if assigned(FGit) and (EUserName.Text <> GitUserName) then begin
      FGit.GitCall('config --global user.name="' + EUserName.Text + '"', '.');
      GitUserName:= EUserName.Text;
    end;
    if assigned(FGit) and (EUserEMail.Text <> GitUserEMail) then begin
      FGit.GitCall('config --global user.email="' + EUserEMail.Text + '"', '.');
      GitUserEMail:= EUserEMail.Text;
    end;

    // tab JUnit
    JUnitJarFile:= ExtendPath(EJUnitJarFile);
    JUnitManual:= extendpath(EJUnitManual);
    JUnitParameter:= EJUnitParameter.Text;
    JUnitBeforeEach:= CBJUnitBeforeEach.Checked;
    JUNitAfterEach:= CBJunitAfterEach.Checked;

    // tab Logfiles
    LogfileCompilerWithUsername:= ExtendPath(ELogfileCompiler);
    LogfileCompiler:= dissolveUsername(LogfileCompilerWithUsername);
    LogfileInteractiveWithUsername:= ExtendPath(ELogfileInteractive);
    LogfileInteractive:= dissolveUsername(LogfileInteractiveWithUsername);
    LogfileExceptionsWithUsername:= ExtendPath(ELogfileExceptions);
    LogfileExceptions:= dissolveUsername(LogfileExceptionsWithUsername);

    // tab Structograms
    Algorithm:= EAlgorithm.Text;
    Input:= EInput.Text;
    Output:= EOutput.Text;
    _While:= EWhile.Text;
    DoWhile:= EDoWhile.Text;
    _For:= EFor.Text;
    Yes:= EYes.Text;
    No:= ENo.Text;
    Other:= EOther.Text;
    GenerateJavaAsProgram:= RGGenerateJavacode.ItemIndex;
    StructoDatatype:= CBDatatype.Text;
    SwitchWithCaseLine:= CBSwitchWithCaseLine.Checked;
    CaseCount:= StrToInt(ECaseCount.text);
    StructogramShadowWidth:= UDStructogramShadowWidth.Position;
    StructogramShadowIntensity:= UDStructogramShadowIntensity.Position;

    // tab Sequence diagram
    SDObject:= ESDObject.Text;
    SDNew:= ESDNew.Text;
    SDClose:= ESDClose.Text;
    SDFillingcolor:= CBSDFillingcolor.Selected;
    SDNoFilling:= CBSDNoFilling.Checked;
    SDShowMainCall:= CBSDShowMainCall.Checked;
    SDShowParameter:= CBSDShowParameter.Checked;
    SDShowReturn:= CBSDShowReturn.Checked;

    // tab providers
    LLMAssistantViewToModel;
    CopyProviders(TempProviders, LLMAssistant.Providers);

    LLMChatViewToModel;
    CopyProviders(TempChatProviders, FLLMChatForm.LLMChat.Providers);

    VisibilityViewToModel;
  end;
end; // ViewToModel

procedure TFConfiguration.SaveVisibility;

  function ArrVisibilityToString1(arr: array of boolean): string;
    var i: integer; s: string;
  begin
    s:= '';
    for i:= 0 to High(arr) do
      if arr[i]
        then s:= s + '1'
        else s:= s + '0';
    Result:= s;
  end;

  function ArrVisibilityToString2(Tab, n: integer): string;
    var i: integer; s: string;
  begin
    s:= '';
    for i:= 0 to n-1 do
      if vis1[Tab, i]
        then s:= s + '1'
        else s:= s + '0';
    Result:= s;
  end;

begin
  WriteStringU('Visibility', 'Tabs', ArrVisibilityToString1(visTabs));
    WriteStringU('Visibility', 'Program',    ArrVisibilityToString2(0, 12));
    WriteStringU('Visibility', 'AWT',        ArrVisibilityToString2(1, FJava.ToolbarAWT.ButtonCount));
    WriteStringU('Visibility', 'Swing1',     ArrVisibilityToString2(2, FJava.ToolBarSwing1.ButtonCount));
    WriteStringU('Visibility', 'Swing2',     ArrVisibilityToString2(3, FJava.ToolbarSwing2.ButtonCount));
    WriteStringU('Visibility', 'Layout',     ArrVisibilityToString2(4, FJava.ToolBarLayout.ButtonCount));
    WriteStringU('Visibility', 'Utilities',  ArrVisibilityToString2(5, FJava.ToolBarUtilities.ButtonCount));
    WriteStringU('Visibility', 'FXBase',     ArrVisibilityToString2(6, FJava.ToolBarFXBase.ButtonCount));
    WriteStringU('Visibility', 'FXControls', ArrVisibilityToString2(7, FJava.ToolBarFXControls.ButtonCount));
    WriteStringU('Visibility', 'FXShapes',   ArrVisibilityToString2(8, FJava.ToolBarFXShapes.ButtonCount));

  WriteStringU('Visibility', 'Menus', ArrVisibilityToString1(visMenus));
  WriteStringU('Visibility', 'Toolbars', ArrVisibilityToString1(visToolbars));
end;

procedure TFConfiguration.LoadVisibility;
  var n: integer; s: string;

  procedure StringVisibilityToArr1(s: string; var arr: TBoolArray);
    var i: integer;
  begin
    while length(s) < length(arr) do s:= s + '1';
    for i:= 0 to high(arr) do
      arr[i]:= (s[i+1] = '1');
  end;

  procedure StringVisibilityToArr2(s: string; l, n: integer);
    var i: integer;
  begin
    while length(s) < l do s:= s + '1';
    while length(s) > l do delete(s, 1, 1);

    for i:= 0 to l - 1 do
      vis1[n, i]:= (s[i+1] = '1');
  end;

begin
  StringVisibilityToArr1(ReadStringU('Visibility', 'Tabs', '111101111'), visTabs);      // no Layout
  StringVisibilityToArr2(ReadStringU('Visibility', 'Program', '111111111111'), 12, 0);  // by default
  n:= FJava.ToolbarAWT.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'AWT',       StringOfChar('1', n)), n, 1);
  n:= FJava.ToolBarSwing1.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Swing1',    StringOfChar('1', n)), n, 2);
  n:= FJava.ToolbarSwing2.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Swing2',    StringOfChar('1', n)), n, 3);
  n:= FJava.ToolBarLayout.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Layout',    StringOfChar('1', n)), n, 4);
  n:= FJava.ToolBarUtilities.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'Utilities', StringOfChar('1', n)), n, 5);
  n:= FJava.ToolBarFXBase.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'FXBase',    StringOfChar('1', n)), n, 6);
  n:= FJava.ToolBarFXControls.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'FXControls',StringOfChar('1', n)), n, 7);
  n:= FJava.ToolBarFXShapes.ButtonCount;
  StringVisibilityToArr2(ReadStringU('Visibility', 'FXShapes',  StringOfChar('1', n)), n, 8);

  s:=ReadStringU('Visibility', 'Menus', '1110');
  StringVisibilityToArr1(s, visMenus);
  s:= ReadStringU('Visibility', 'Toolbars', '111110');
  StringVisibilityToArr1(s, visToolbars);
end;

procedure TFConfiguration.SetVisibility;
  var i, j: integer; winControl: TWinControl;
      Item: TSpTBXTabItem; Page: TSpTBXTabSheet;
begin
  for i:= 0 to maxTab do begin
    FJava.TabsControl.Items[i].Visible:= visTabs[i];
    Item:= TSpTBXTabItem(FJava.TabsControl.Items[i]);
    Page:= FJava.TabsControl.GetPage(item);
    WinControl:= TWinControl(Page.Controls[0]);
    for j:= 0 to WinControl.ControlCount-1 do
      WinControl.Controls[j].Visible:= vis1[i, j];
  end;

  FJava.MITest.Visible:= visMenus[0];
  FJava.MIUML.Visible:= visMenus[1];
  FJava.MITools.Visible:= visMenus[2];
  FJava.MIComponents.Visible:= visMenus[3];

  FJava.MainToolbar.Visible:= visToolbars[0];
  // FJava.Toolbar2.Visible:= visToolbars[1];
  // editor window toolbar = visToolbars[2]
  // uml window toolbar    = visToolbars[3]
  FJava.TabsControl.Visible:= visToolbars[4];
  FJava.PBorder.Visible:= visToolbars[5];
  FJava.ControlBar.Visible:= visToolbars[0] or visToolbars[4] or visToolbars[5];
  FJava.SetDockTopPanel;
end;

procedure TFConfiguration.VisibilityViewToModel;
  var i, j: integer;
begin
  for i:= 0 to maxTab do
    visTabs[i]:= LVVisibilityTabs.Items[i].Checked;

  if assigned(LVVisibilityMenus.Items[0]) then // due to unknown problem
    for i:= 0 to high(visMenus) do
      visMenus[i]:= LVVisibilityMenus.Items[i].Checked;

  if assigned(LVVisibilityToolbars.Items[0]) then
    for i:= 0 to high(visToolbars) do
      visToolbars[i]:= LVVisibilityToolbars.Items[i].Checked;

  for i:= 0 to maxTab do
    for j:= 0 to maxTabItem do
      vis1[i, j]:= vis2[i, j];
end;

procedure TFConfiguration.VisibilityModelToView;
  var i, j: integer;
begin
  if LVVisibilityTabs.Items.Count = 0 then
    PrepareVisibilityPage;

  for i:= 0 to LVVisibilityTabs.Items.Count - 1 do
    LVVisibilityTabs.Items[i].Checked:= visTabs[i];

  for i:= 0 to LVVisibilityMenus.Items.Count - 1 do
    LVVisibilityMenus.Items[i].Checked:= visMenus[i];

  for i:= 0 to LVVisibilityToolbars.Items.Count - 1 do
    LVVisibilityToolbars.Items[i].Checked:= visToolbars[i];

  // save visibility settings in vis2 for changing
  for i:= 0 to high(vis1) do
    for j:= 0 to high(vis1[i]) do
      vis2[i, j]:= vis1[i, j];

  LVVisibilityElements.onItemChecked:= nil;
  for i:= 0 to LVVisibilityElements.Items.Count - 1 do
    LVVisibilityElements.Items[i].Checked:= vis2[VisSelectedTab, i];
  LVVisibilityElements.onItemChecked:= LVVisibilityElementsItemChecked;
end;

procedure TFConfiguration.PrepareVisibilityPage;
  var i: integer;
      PC: TSpTBXTabControl;
      anItem: TListItem;
begin
  PC:= FJava.TabsControl;
  for i:= 0 to PC.Items.Count - 1 do begin
    anItem:= LVVisibilityTabs.Items.Add;
    anItem.Caption:= PC.Items[i].Caption;
    anItem.Checked:= visTabs[i];
  end;
  LVVisibilityTabs.ItemIndex:= 0;
  LVVisibilityTabs.Selected:= LVVisibilityTabs.Items[0];
  LVVisibilityTabs.ItemFocused:= LVVisibilityTabs.Items[0];

  for i:= 0 to High(visMenus) do begin
    anItem:= LVVisibilityMenus.Items.Add;
    anItem.Checked:= visMenus[i];
    anItem.Caption:= ReplaceStr(FJava.MainMenu.Items[i+3].Caption, '&', '');
  end;

  LVVisibilityToolbars.Items.BeginUpdate;
  for i:= 0 to high(visToolbars) do begin
    anItem:= LVVisibilityToolbars.Items.Add;
    anItem.Checked:= visToolbars[i];
  end;
  LVVisibilityToolbars.Items[0].Caption:= ReplaceStr(FJava.MainMenu.Items[0].Caption, '&', '');
  LVVisibilityToolbars.Items[1].Caption:= ReplaceStr(FJava.MainMenu.Items[1].Caption, '&', '');
  LVVisibilityToolbars.Items[2].Caption:= 'Editor';
  LVVisibilityToolbars.Items[3].Caption:= ReplaceStr(FJava.MainMenu.Items[4].Caption, '&', '');
  LVVisibilityToolbars.Items[4].Caption:= ReplaceStr(FJava.MainMenu.Items[6].Caption, '&', '');
  LVVisibilityToolbars.Items[5].Caption:= 'BorderLayout';
  LVVisibilityToolbars.Items.EndUpdate;
  LVVisibilityTabsClick(Self);
end;

procedure TFConfiguration.LVVisibilityElementsItemChecked(Sender: TObject;
  Item: TListItem);
begin
  vis2[VisSelectedTab, Item.Index]:= Item.Checked;
end;

procedure TFConfiguration.LVVisibilityTabsClick(Sender: TObject);
  var s: string; i, p, Tab: integer;
      TB: TToolBar;
      anItem: TListItem;
begin
  Tab:= max(LVVisibilityTabs.ItemIndex, 0);
  LVVisibilityElements.Clear;
  case Tab of
    0: begin
         LVVisibilityElements.SmallImages:= FJava.vilProgramLight;
         TB:= FJava.ToolbarProgram;
    end;
    1: begin
         LVVisibilityElements.SmallImages:= FJava.vilAWTLight;
         TB:= FJava.ToolbarAWT;
    end;
    2: begin
         LVVisibilityElements.SmallImages:= FJava.vilSwing1Light;
         TB:= FJava.ToolbarSwing1;
    end;
    3: begin
         LVVisibilityElements.SmallImages:= FJava.vilSwing2;
         TB:= FJava.ToolbarSwing2;
    end;
    4: begin
         LVVisibilityElements.SmallImages:= FJava.vilLayoutLight;
         TB:= FJava.ToolbarLayout;
    end;
    5: begin
         LVVisibilityElements.SmallImages:= FJava.vilUtilities;
         TB:= FJava.ToolbarUtilities;
    end;
    6: begin
         LVVisibilityElements.SmallImages:= FJava.vilFXBaseLight;
         TB:= FJava.ToolbarFXBase;
    end;
    7: begin
         LVVisibilityElements.SmallImages:= FJava.vilFXControls;
         TB:= FJava.ToolbarFXControls;
    end;
    8: begin
         LVVisibilityElements.SmallImages:= FJava.vilFXShapesLight;
         TB:= FJava.ToolbarFXShapes;
    end;
    else
      exit;
  end;
  LVVisibilityElements.onItemChecked:= nil;
  if assigned(TB) then begin
    for i:= 0 to TB.ButtonCount - 1 do begin
      s:= TB.Buttons[i].Hint;
      p:= Pos(' ', s);
      if p > 0 then s:= copy(s, 1, p-1);
      anItem:= LVVisibilityElements.Items.Add;
      anItem.Caption:= s;
      anItem.Checked:= vis2[Tab, i];
      anItem.ImageIndex:= i;
    end;
  end;
  VisSelectedTab:= Tab;
  LVVisibilityElements.onItemChecked:= LVVisibilityElementsItemChecked;
end;

procedure TFConfiguration.SaveWindow;
  var i, n: Integer; s1, s2: string; aForm: TFForm;
begin
  WriteStringU('Font', 'Name', EditFont.Name);
  WriteIntegerU('Font', 'Size', EditFont.Size);
  WriteStringU('UML',  'Name', UMLFont.Name);
  WriteIntegerU('UML',  'Size', UMLFont.Size);
  WriteIntegerU('UML', 'ShowParameter', DiShowParameter);
  WriteIntegerU('UML', 'Visibility', DiVisibilityFilter);
  WriteIntegerU('UML', 'SortOrder', DiSortOrder);
  WriteIntegerU('UML', 'ShowIcons', DiShowIcons);
  WriteStringU('Structogram', 'Name', StructogramFont.Name);
  WriteIntegerU('Structogram', 'Size', StructogramFont.Size);
  WriteStringU('SequenceDiagram', 'Name', SequenceFont.Name);
  WriteIntegerU('SequenceDiagram', 'Size', SequenceFont.Size);
  WriteStringU('Window', 'Current', '');
  n:= 0;
  for i:= 0 to FJava.TDIFormsList.Count - 1 do begin
    aForm:= FJava.TDIFormsList[i];
    if assigned(aForm) then begin
      inc(n);
      s1:= aForm.GetState + aForm.GetFormType;
      if (aForm.FormTag in [1, 2, 11, 14]) and
         not aForm.AlreadySavedAs and aForm.DefaultFilename then
        FJava.DoSave(aForm, CreateBAKFiles);
      s2:= aForm.Pathname;
      if hasUMLExtension(s2) and aForm.DefaultFilename
        then WriteStringU('Window', 'Win' + IntToStr(n), s1 + RemovePortableDrive(TempDir) + ExtractFileName(s2))
        else WriteStringU('Window', 'Win' + IntToStr(n), s1 + RemovePortableDrive(s2));
      if aForm = FJava.ActiveTDIChild
        then WriteStringU('Window', 'Current', s1 + RemovePortableDrive(s2));
    end;
  end;
  WriteIntegerU('Window', 'Wins', n);
  WriteStringU('Program', 'Sourcepath', RemovePortableDrive(Sourcepath));
  WriteBoolU('Program', 'WindowStateMaximized', WindowStateMaximized);
  WriteStringU('Program', 'Version', UDlgAbout.Version);
  WriteIntegerU('Program', 'FileFilter', FJava.ODOpen.FilterIndex);
  FJava.SaveBounds;
end;

procedure TFConfiguration.SaveStrings(const key, aName: string; values: TStrings);
begin
  var s:= '';
  for var i:= 0 to values.count - 1 do
    s:= s + values.Strings[i] + ' |';
  WriteStringU(key, aName, RemovePortableDrive(s));
end;

procedure TFConfiguration.ReadStrings(const key, aName: string; values: TStrings);
  var s: string; p: integer;
begin
  try
    s:= ReadStringU(key, aName, '');
  except
    s:= '';
  end;
  values.Clear;
  p:= Pos(' |', s);
  while p > 1 do begin
    values.add(AddPortableDrive(copy(s, 1, p-1)));
    delete(s, 1, p + 1);
    p:= Pos(' |', s);
  end;
end;

procedure TFConfiguration.SaveFavorites(Favorites: TStringList);
begin
  for var i:= 0 to Favorites.Count - 1 do
    WriteStringU('Favoriten', 'Fav' + IntToStr(i+1), Favorites.Strings[i]);
  WriteIntegerU('Favoriten', 'Favs', Favorites.Count);
end;

procedure TFConfiguration.ReadFavorites(var Favorites: TStringList);
  var count, i: Integer; s: string;
begin
  count:= ReadIntegerU('Favoriten', 'Favs', 0);
  for i:= 1 to count do begin
    s:= ReadStringU('Favoriten', 'Fav'+IntTostr(i), '');
    if s <> '' then Favorites.Add(s);
  end;
end;

procedure TFConfiguration.ReplaceClNone;
begin
  if isDark
    then EditorTextColor:= StyleServices.GetStyleFontColor(sfTabTextInactiveNormal)
    else EditorTextColor:= clBlack;
  with JavaHighlighter do begin
    if CommentAttri.Foreground = clNone then CommentAttri.Foreground:= EditorTextColor;
    if DocumentAttri.Foreground = clNone then DocumentAttri.Foreground:= EditorTextColor;
    if IdentifierAttri.Foreground = clNone then IdentifierAttri.Foreground:= EditorTextColor;
    if InvalidAttri.Foreground = clNone then InvalidAttri.Foreground:= EditorTextColor;
    if NumberAttri.Foreground = clNone then NumberAttri.Foreground:= EditorTextColor;
    if KeyAttri.Foreground = clNone then KeyAttri.Foreground:= EditorTextColor;
    if SpaceAttri.Foreground = clNone then SpaceAttri.Foreground:= EditorTextColor;
    if StringAttri.Foreground = clNone then StringAttri.Foreground:= EditorTextColor;
    if SymbolAttri.Foreground = clNone then SymbolAttri.Foreground:= EditorTextColor;
  end;
  with HTMLHighlighter do begin
    if CommentAttri.Foreground = clNone then CommentAttri.Foreground:= EditorTextColor;
    if AndAttri.Foreground = clNone then AndAttri.Foreground:= EditorTextColor;
    if IdentifierAttri.Foreground = clNone then IdentifierAttri.Foreground:= EditorTextColor;
    if TextAttri.Foreground = clNone then TextAttri.Foreground:= EditorTextColor;
    if KeyAttri.Foreground = clNone then KeyAttri.Foreground:= EditorTextColor;
    if UndefKeyAttri.Foreground = clNone then UndefKeyAttri.Foreground:= EditorTextColor;
    if SpaceAttri.Foreground = clNone then SpaceAttri.Foreground:= EditorTextColor;
    if ValueAttri.Foreground = clNone then ValueAttri.Foreground:= EditorTextColor;
    if SymbolAttri.Foreground = clNone then SymbolAttri.Foreground:= EditorTextColor
  end;
end;

procedure TFConfiguration.LoadUserColors;
  var path: string; i: integer;
begin
  for i:= 0 to 6 do
    LoadDefaultColors(i);
  if UseRegistry then begin
    JavaHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, getRegPath + '\Java');
    HTMLHighlighter.LoadFromRegistry(HKEY_CURRENT_USER, getRegPath + '\HTML');
  end else if assigned(UserIniFile) then begin
    path:= ExtractFilepath(UserIniFile.Filename);
    JavaHighlighter.LoadFromFile(path + 'JEJavaCol.ini', EditorStyle);
    HTMLHighlighter.LoadFromFile(path + 'JEHTMLCol.ini', EditorStyle);
  end;
end;

procedure TFConfiguration.LoadDefaultColors(Typ: integer);
begin
  var Path:= EditorFolder + 'styles' + PathDelim;
  case Typ of
    0: JavaHighlighter.LoadFromFile(Path + 'DefaultColorsJava.ini', EditorStyle);
    1: HTMLHighlighter.LoadFromFile(Path + 'DefaultColorsHTML.ini', EditorStyle);
    2: SetDefaultBracketColor;
    3: if assigned(PascalHighlighter) then
         PascalHighlighter.LoadFromFile(Path + 'DefaultColorsPascal.ini', EditorStyle);
    4: if assigned(PhpHighlighter) then
         PhpHighlighter.LoadFromFile(Path + 'DefaultColorsPHP.ini', EditorStyle);
    5: if assigned(CSSHighlighter) then
         CSSHighlighter.LoadFromFile(Path + 'DefaultColorsCSS.ini', EditorStyle);
    6: if assigned(GeneralHighlighter) then
         GeneralHighlighter.LoadFromFile(Path + 'DefaultColorsGeneral.ini', EditorStyle);
  end;
end;

function TFConfiguration.getRegPath: string;
begin
  Result:= '\Software\JavaEditor\Colors';
  if EditorStyle <> 'Default' then
    Result:= Result + '\' + EditorStyle;
end;

// https://www.slant.co/topics/358/~best-color-themes-for-text-editors#5

procedure TFConfiguration.SaveUserColors;

  procedure SaveAttributes(Attr: TSynHighlighterAttributes);
    var key: string;
  begin
    key:= 'Colors\' + Attr.Name;
    WriteIntegerU(key, 'Background', Attr.Background);
    WriteIntegerU(key, 'Foreground', Attr.Foreground);
    WriteIntegerU(key, 'Style', Attr.IntegerStyle);
  end;

begin
  try
    if UseRegistry then begin
      JavaHighlighter.SaveToRegistry(HKEY_CURRENT_USER, getRegPath + '\Java');
      HTMLHighlighter.SaveToRegistry(HKEY_CURRENT_USER, getRegPath + '\HTML');
      end
    else if Assigned(UserIniFile) then begin
      JavaHighlighter.SaveToFile(ExtractFilePath(UserIniFile.Filename) + 'JEJavaCol.ini', EditorStyle);
      HTMLHighlighter.SaveToFile(ExtractFilePath(UserIniFile.Filename) + 'JEHTMLCol.ini', EditorStyle);
    end;
    SaveAttributes(AttrBrackets);
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
end;

procedure TFConfiguration.ReadUserColors;

  procedure ReadAttributes(var Attr: TSynHighlighterAttributes);
  begin
    var key:= 'Colors\' + Attr.Name;
    Attr.Background:= ReadIntegerU(key, 'Background', Attr.Background);
    Attr.Foreground:= ReadIntegerU(key, 'Foreground', Attr.Foreground);
    Attr.IntegerStyle:= ReadIntegerU(key, 'Style', Attr.IntegerStyle);
  end;

begin
  LoadUserColors;
  ReadAttributes(AttrBrackets);
  ReplaceClNone;
end;

procedure TFConfiguration.WriteStringM(const key, aName, value: string);
begin
  WriteString(machine, key, aName, value);
end;

procedure TFConfiguration.WriteStringF(const key, aName, value: string);
begin
  WriteString(allusers, key, aName, value);
end;

procedure TFConfiguration.WriteStringU(const key, aName, value: string);
begin
  WriteString(user, key, aName, value);
end;

procedure TFConfiguration.WriteString(dest: integer; const key, aName, value: string);
begin
  if DumpMode then begin
    case dest of
      0: DumpIniFileHKLM.WriteString(key, aName, value);
      1: DumpIniFileAllUsers.WriteString(key, aName, value);
      2: DumpIniFileHKCU.WriteString(key, aName, value);
    end;
    exit;
  end;
  if UseRegistry then
    with MyRegistry do begin
      case dest of
        0: RootKey:= HKEY_LOCAL_MACHINE;
        1: RootKey:= HKEY_CURRENT_USER;
        2: RootKey:= HKEY_CURRENT_USER;
      end;
      Access:= KEY_WRITE;
      try
        try
          if OpenKey('\Software\JavaEditor\' + key, true)
            then WriteString(aName, value);
        except
        end;
      finally
        CloseKey;
      end;
    end
  else
    try
      if (dest = 0) and assigned(MachineIniFile)
        then MachineIniFile.WriteString(key, aName, value)
      else if Assigned(UserIniFile)
        then UserIniFile.WriteString(key, aName, value);
    except
    end;
end;

function TFConfiguration.ReadStringFile(const key, aName, default: string): string;
begin
  if LockedPaths or not UseRegistry then
    Result:= AddPortableDrive(ReadStringM(key, aName, default))
  else begin
    Result:= AddPortableDrive(ReadStringF(key, aName, default));
    if not isHTTP(Result) and not FileExists(dissolveUsername(Result)) then
      Result:= AddPortableDrive(ReadStringU(key, aName, default));
    if not isHTTP(Result) and not FileExists(dissolveUsername(Result)) then
      Result:= AddPortableDrive(ReadStringM(key, aName, default));
  end;
end;

procedure TFConfiguration.WriteStringFile(const key, aName, value: string);
begin
  var aValue:= RemovePortableDrive(value);
  if LockedPaths or not UseRegistry
    then WriteStringM(key, aName, aValue)
    else WriteStringF(key, aName, aValue);
end;

function TFConfiguration.ReadStringDirectory(const key, aName: string): string;
begin
  if LockedPaths or not UseRegistry
    then Result:= ReadStringM(key, aName, '')
  else begin
    Result:= ReadStringF(key, aName, '');
    if Result = '' then
      Result:= ReadStringU(key, aName, '');
    if Result = '' then
      Result:= ReadStringM(key, aName, '');
  end;
  Result:= AddPortableDrive(Result);
end;

procedure TFConfiguration.WriteStringDirectory(const key, aName, value: string);
begin
  var aValue:= RemovePortableDrive(value);
  if LockedPaths or not UseRegistry
    then WriteStringM(key, aName, aValue)
    else WriteStringF(key, aName, aValue);
end;

function TFConfiguration.ReadStringM(const key, aName, default: string): string;
begin
  Result:= ReadString(machine, key, aName, default);
end;

function TFConfiguration.ReadStringF(const key, aName, default: string): string;
begin
  Result:= ReadString(allusers, key, aName, default);
end;

function TFConfiguration.ReadStringU(const key, aName, default: string): string;
begin
  Result:= ReadString(user, key, aName, default);
end;

function TFConfiguration.ReadString(dest: integer; const key, aName, default: string): string;
begin
  if UseRegistry then
    with MyRegistry do begin
      case dest of
        0: RootKey:= HKEY_LOCAL_MACHINE;
        1: RootKey:= HKEY_CURRENT_USER;
        2: RootKey:= HKEY_CURRENT_USER;
      end;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\' + key, false);
        if ValueExists(aName)
          then Result:= ReadString(aName)
          else Result:= Default;
      finally
        CloseKey;
      end;
    end
  else
    try
      if (dest = 0) and assigned(MachineIniFile)
        then Result:= MachineIniFile.ReadString(key, aName, default)
      else if Assigned(UserIniFile) and Assigned(MachineIniFile)
        then if MachineIniFile.ValueExists(key, aName)
          then Result:= MachineIniFile.ReadString(key, aName, default)
          else Result:= UserIniFile.ReadString(key, aName, default)
        else Result:= Default;
    except
      Result:= Default;
    end;
end;

procedure TFConfiguration.WriteIntegerU(const key, aName: string; value: integer);
begin
  if DumpMode then begin
    DumpIniFileHKCU.WriteInteger(key, aName, value);
    exit;
  end;
  if UseRegistry then
    with MyRegistry do begin
      RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_WRITE;
      try
        if OpenKey('\Software\JavaEditor\' + key, true)
          then WriteInteger(aName, value);
      finally
        CloseKey;
      end;
    end
  else
    try
      if Assigned(UserIniFile)
        then UserIniFile.WriteInteger(key, aName, value);
    except
    end;
end;

function TFConfiguration.ReadIntegerU(const key, aName: string; default: integer): Integer;
begin
  if UseRegistry then
    with MyRegistry do begin
      RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\' + key, false);
        if ValueExists(aName)
          then Result:= ReadInteger(aName)
          else Result:= default;
      finally
        CloseKey;
      end;
    end
  else
    if Assigned(UserIniFile)
      then if assigned(MachineIniFile) and MachineIniFile.ValueExists(key, aName)
        then Result:= MachineIniFile.ReadInteger(key, aName, default)
        else Result:= UserIniFile.ReadInteger(key, aName, default)
      else Result:= Default;
end;

procedure TFConfiguration.WriteBoolM(const key, aName: string; value: boolean);
begin
  WriteBool(true, key, aName, value);
end;

procedure TFConfiguration.WriteBoolU(const key, aName: string; value: boolean);
begin
  WriteBool(false, key, aName, value);
end;

procedure TFConfiguration.WriteBool(machine: boolean; const key, aName: string; value: Boolean);
begin
  if DumpMode then begin
    if machine
      then DumpIniFileHKLM.WriteBool(key, aName, value)
      else DumpIniFileHKCU.WriteBool(key, aName, value);
    exit;
  end;
  if UseRegistry then
    with MyRegistry do begin
      if Machine
        then RootKey:= HKEY_LOCAL_MACHINE
        else RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_WRITE;
      try
        if OpenKey('\Software\JavaEditor\' + key, true)
          then WriteBool(aName, value);
      finally
        CloseKey;
      end;
    end
  else
    try
      if Machine and assigned(MachineIniFile)
        then MachineIniFile.WriteBool(key, aName, value)
      else if Assigned(UserIniFile)
        then UserIniFile.WriteBool(key, aName, value);
    except
    end;
end;

function TFConfiguration.ReadBoolM(const key, aName: string; default: boolean): boolean;
begin
  Result:= ReadBool(true, key, aName, default);
end;

function TFConfiguration.ReadBoolU(const key, aName: string; default: boolean): boolean;
begin
  Result:= ReadBool(false, key, aName, default);
end;

function TFConfiguration.ReadBool(machine: boolean; const key, aName: string; default: Boolean): Boolean;
begin
  if UseRegistry then
    with MyRegistry do begin
      if machine
        then RootKey:= HKEY_LOCAL_MACHINE
        else RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\' + key, false);
        if ValueExists(aName)
          then Result:= ReadBool(aName)
          else Result:= default
      finally
        CloseKey;
      end;
    end
  else
    try
      if Machine and Assigned(MachineIniFile)
        then Result:= MachineIniFile.ReadBool(key, aName, default)
      else if Assigned(UserIniFile)
        then if Assigned(MachineIniFile) and MachineIniFile.ValueExists(key, aName)
          then Result:= MachineIniFile.ReadBool(key, aName, default)
          else Result:= UserIniFile.ReadBool(key, aName, default)
        else Result:= Default;
    except
      Result:= Default;
    end;
end;

procedure TFConfiguration.WriteBinaryStreamU(const key, aName: string; value: TStream);
begin
  WriteBinaryStream(false, key, aName, value);
end;

procedure TFConfiguration.WriteBinaryStream(machine: boolean; const key, aName: string; value: TStream);
  var Stream: TMemoryStream;
begin
  Stream:= TMemoryStream(value);
  if UseRegistry then
    with MyRegistry do begin
      if Machine
        then RootKey:= HKEY_LOCAL_MACHINE
        else RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_WRITE;
      try
        if OpenKey('\Software\JavaEditor\' + key, true)
          then WriteBinaryData(aName, Pointer(PByte(Stream.Memory) + Stream.Position)^,
                               Stream.Size - Stream.Position);
      finally
        CloseKey;
      end;
    end
  else
    try
      if Machine and assigned(MachineIniFile)
        then MachineIniFile.WriteBinaryStream(key, aName, Stream)
      else if Assigned(UserIniFile)
        then UserIniFile.WriteBinaryStream(key, aName, Stream);
    except
    end;
end;

function TFConfiguration.ReadBinaryStreamU(const key, aName: string; value: TStream): integer;
begin
  Result:= ReadBinaryStream(false, key, aName, value);
end;

function TFConfiguration.ReadBinaryStream(machine: boolean; const key, aName: string; value: TStream): integer;
var
  RegData: TRegDataType;
  Info: TRegDataInfo;
  Stream: TMemoryStream;
begin
  if UseRegistry then
    with MyRegistry do begin
      if machine
        then RootKey:= HKEY_LOCAL_MACHINE
        else RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\' + key, false);
        if ValueExists(aName) and GetDataInfo(aName, Info) then begin
          Result := Info.DataSize;
          RegData := Info.RegData;
          Stream := TMemoryStream(Value);
          try
            if (RegData = rdBinary) or (RegData = rdUnknown) then
            begin
              Stream.Size := Stream.Position + Info.DataSize;
              Result := ReadBinaryData(aName,
                Pointer(PByte(Stream.Memory) + Stream.Position)^, Stream.Size);
              if Stream <> Value then Value.CopyFrom(Stream, Stream.Size - Stream.Position);
            end;
          finally
            if Stream <> Value then
              FreeAndNil(Stream);
          end;
        end else
          Result:= 0;
      finally
        CloseKey;
      end;
    end
  else
    if Machine and assigned(MachineIniFile)
      then Result:= MachineIniFile.ReadBinaryStream(key, aName, value)
    else if Assigned(UserIniFile)
      then if assigned(MachineIniFile) and MachineIniFile.ValueExists(key, aName)
        then Result:= MachineIniFile.ReadBinaryStream(key, aName, value)
        else Result:= UserIniFile.ReadBinaryStream(key, aName, value)
      else Result:= 0;
end;

procedure TFConfiguration.MakeAssociations;
  var Reg: TRegistry; s, s1: string; p: integer;

  procedure EditAssociation(const Extension: string; docreate: boolean);
  begin
    with Reg do begin
      try
        Access:= KEY_ALL_ACCESS;
        // HKEY_LOCAL_MACHINE\Software\Classes
        RootKey:= HKEY_LOCAL_MACHINE;
        if docreate then begin
          if OpenKey('SOFTWARE\Classes\' + Extension, true) then
            WriteString('', 'JavaEditor')
        end else begin
          if OpenKey('SOFTWARE\Classes\' + Extension, false) then
            if ReadString('') = 'JavaEditor' then
              WriteString('', '');
        end;
        CloseKey;

        // HKEY_CURRENT_USER\Software\Classes
        RootKey:= HKEY_CURRENT_USER;
        if docreate then begin
          if OpenKey('SOFTWARE\Classes\' + Extension, true) then
            WriteString('', 'JavaEditor');
        end else begin
          if OpenKey('SOFTWARE\Classes\' + Extension, false) then
            if ReadString('') = 'JavaEditor' then
              WriteString('', '');
        end;
        CloseKey;
      except
        on E: Exception do
          ErrorMsg(E.Message);
      end
    end;
  end;

begin
  RegisterJavaEditor;
  Reg:= TRegistry.Create;
  try
    with Reg do begin
      EditAssociation(CBAssociationJava.Caption, CBAssociationJava.Checked);
      EditAssociation(CBAssociationJfm.Caption,  CBAssociationJfm.Checked);
      EditAssociation(CBAssociationUml.Caption,  CBAssociationUml.Checked);
      EditAssociation(CBAssociationJep.Caption,  CBAssociationJep.Checked);
      EditAssociation(CBAssociationHtml.Caption, CBAssociationHtml.Checked);
      EditAssociation(CBAssociationTxt.Caption,  CBAssociationTxt.Checked);
      EditAssociation(CBAssociationJsp.Caption,  CBAssociationJsp.Checked);
      EditAssociation(CBAssociationPhp.Caption,  CBAssociationPhp.Checked);
      EditAssociation(CBAssociationCss.Caption,  CBAssociationCss.Checked);
      EditAssociation(CBAssociationInc.Caption,  CBAssociationInc.Checked);
      EditAssociation(CBAssociationJsg.Caption,  CBAssociationJsg.Checked);
      s:= AdditionalAssociations + ';';
      p:= Pos(';', s);
      while p > 0 do begin
        s1:= copy(s, 1, p-1);
        delete(s, 1, p);
        p:= Pos('.', s1);
        if p > 0 then delete(s1, 1, p);
        if (s1 <> '') and (Pos(' ', s1) = 0) then EditAssociation('.' + s1, true);
        p:= Pos(';', s);
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil) ;
end;

procedure TFConfiguration.RegisterJavaEditor;
  var JavaEditor, Filename: string; Reg: TRegistry;

  procedure WriteToRegistry(const Key: string);
  begin
    with Reg do begin
      OpenKey(Key, true);
      WriteString('', 'Java-Editor');
      CloseKey;
      OpenKey(Key + '\DefaultIcon', true);
      WriteString('', JavaEditor + ',0');
      CloseKey;
      OpenKey(Key + '\Shell\Open\command', true);
      WriteString('', JavaEditor);
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec', true);
      WriteString('', '[FileOpen("%1")]');
      OpenKey('Application', TRUE);
      FileName := ExtractFileName(ParamStr(0));
      FileName := Copy(FileName, 1, Length(FileName)-4);
      WriteString('', Filename);
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec\topic', true);
      WriteString('', 'System');
      CloseKey;
    end;
  end;

begin
  JavaEditor:= HideBlanks(ParamStr(0));
  Reg:= TRegistry.Create;
  try
    with Reg do begin
      Access:= KEY_ALL_ACCESS;
      RootKey:= HKEY_LOCAL_MACHINE;
      WriteToRegistry('SOFTWARE\Classes\JavaEditor');
      RootKey:= HKEY_CURRENT_USER;
      WriteToRegistry('\SOFTWARE\Classes\JavaEditor');
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TFConfiguration.LBColorElementsClick(Sender: TObject);
  var Attr: TSynHighlighterAttributes;
      s: string; i: integer;
begin
  Attr:= TSynHighlighterAttributes.Create('', '');
  try
    if LBColorElements.ItemIndex < 0 then
      LBColorElements.ItemIndex:= 0;
    s:= LBColorElements.Items[LBColorElements.ItemIndex];
    i:= max(LNGColornameToAttributIndex(s), 0);
    case RGColors.ItemIndex of
      0: Attr.Assign(JavaHighlighter.Attribute[i]);
      1: Attr.Assign(HTMLHighlighter.Attribute[i]);
      2: Attr.Assign(AttrBrackets);
    end;
    CBTextColorBox.Selected:= Attr.Foreground;
    CBBackgroundColorBox.Selected:= Attr.Background;
    CBNoBackgroundColor.Checked:= (Attr.Background = clNone);
    CBBackgroundColorBox.enabled:= not CBNoBackgroundColor.Checked;
    cbBold.Checked     := (fsBold in Attr.Style);
    cbItalic.Checked   := (fsItalic in Attr.Style);
    cbUnderline.Checked:= (fsUnderline in Attr.Style);
  finally
    FreeAndNil(Attr);
  end;
end;

function TFConfiguration.LNGColornameToAttributIndex(const s: string): integer;
// determines from user-defined token child name the
// Index of the responsible highlighter attribute
begin
  case RGColors.ItemIndex of
    0: if      s = _(LNGComment)       then Result:= 0
       else if s = _(LNGDocumentation) then Result:= 1
       else if s = _(LNGIdentifier)    then Result:= 2
       else if s = _(LNGInvalidSymbol) then Result:= 3
       else if s = _(LNGNumber)        then Result:= 4
       else if s = _(LNGReservedWord)  then Result:= 5
       else if s = _(LNGSpace)         then Result:= 6
       else if s = _(LNGString)        then Result:= 7
       else if s = _(LNGSymbol)        then Result:= 8
       else Result:= -1;
    1: if      s = _(LNGComment)       then Result:= 0
       else if s = _(LNGEscape)        then Result:= 1
       else if s = _(LNGAttribute)     then Result:= 2
       else if s = _(LNGTag)           then Result:= 3
       else if s = _(LNGSpace)         then Result:= 4
       else if s = _(LNGSymbol)        then Result:= 5
       else if s = _(LNGText)          then Result:= 6
       else if s = _(LNGUnknownWord)   then Result:= 7
       else if s = _(LNGValue)         then Result:= 8
       else Result:= -1;
    2: if s = _(LNGBrackets)           then Result:= 0
       else Result:= -1;
    else Result:= -1;
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
  i: integer; s: string;
begin
  CBBackgroundColorBox.enabled:= not CBNoBackgroundColor.Checked;
  // conversion LNGColorName in Highlighter.Attributname
  if LBColorElements.ItemIndex < 0 then
    LBColorElements.ItemIndex:= 0;
  s:= LBColorElements.Items[LBColorElements.ItemIndex];
  i:= max(LNGColornameToAttributIndex(s), 0);
  case RGColors.ItemIndex of
    0: s:= JavaHighlighter.Attribute[i].Name;
    1: s:= HTMLHighlighter.Attribute[i].Name;
    2: s:= 'Brackets';
  end;
  // Change the relevant highlighter attribute
  Attr:= TSynHighlighterAttributes.Create(s, s);
  try
    AttrStyle:= [];
    if CBNoBackgroundColor.Checked
      then Attr.Background:= clNone
      else Attr.Background:= CBBackgroundColorBox.Selected;
    Attr.Foreground:= CBTextColorBox.Selected;

    if cbBold.Checked then      Include(AttrStyle, fsBold);
    if cbItalic.Checked then    Include(AttrStyle, fsItalic);
    if cbUnderline.Checked then Include(AttrStyle, fsUnderline);
    Attr.Style:= AttrStyle;
    case RGColors.ItemIndex of
      0: JavaHighLighter.Attribute[i].Assign(Attr);
      1: HTMLHighLighter.Attribute[i].Assign(Attr);
      2: AttrBrackets.Assign(Attr);
    end;
  finally
    FreeAndNil(Attr);
  end;
end;

procedure TFConfiguration.ShowColorElements;
begin
  with LBColorElements do begin
    Clear;
    case RGColors.ItemIndex of
      0: begin
           Items.Add(_(LNGIdentifier));
           Items.Add(_(LNGDocumentation));
           Items.Add(_(LNGComment));
           Items.Add(_(LNGSpace));
           Items.Add(_(LNGReservedWord));
           Items.Add(_(LNGstring));
           Items.Add(_(LNGSymbol));
           Items.Add(_(LNGInvalidSymbol));
           Items.Add(_(LNGNumber));
         end;
      1: begin
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
      2: Items.Add(_(LNGBrackets));
    end;
    ItemIndex:= 0;
    LBColorElementsClick(Self);
  end;
end;

procedure TFConfiguration.BDefaultColorsClick(Sender: TObject);
begin
  LoadDefaultColors(RGColors.ItemIndex);
  LBColorElementsClick(Self);
  for var i:= 0 to FJava.TDIEditFormCount - 1 do
    FJava.TDIEditFormGet(i).SetHighlighter;
end;

procedure TFConfiguration.CBUseIEinternForDocumentsClick(Sender: TObject);
begin
  if CBUseIEinternForDocuments.Checked and not ExplorerTest then begin
    MessageDlg(_('Internet Explorer is not installed.'), mtWarning, [mbOK], 0);
    CBUseIEinternForDocuments.Checked:= false;
  end
end;

procedure TFConfiguration.CBHeaderChange(Sender: TObject);
  var s: string; pL, pR: Integer;
begin
  s:= EHeader.Text;
  pL:= 1;
  while (pL <= Length(s)) and (s[pL] <> '#') do inc(pL);
  if pL > Length(s) then begin // no # found
    s:= '#' + s + '#';
    pL:= 1;
    pR:= Length(s);
  end else begin // search second #
    pR:= pL + 1;
    while (pR <= Length(s)) and (s[pR] <> '#') do inc(pR);
    if pR > Length(s) then  // no second # found
      if pL = 1 then begin
        s:= s + '#';
        pR := Length(s);
      end else begin
        s:= '#' + s;
        pR:= pL + 1;
        pL:= 1;
      end;
  end;
  case RGAdjustment.ItemIndex of
    0: insert(CBHeader.Text, s, pL);
    1: insert(CBHeader.Text, s, pR);
    2: s:= s + CBHeader.Text;
  end;
  EHeader.Text:= s;
end;

procedure TFConfiguration.CBFooterChange(Sender: TObject);
  var s: string; pL, pR: Integer;
begin
  s:= EFooter.Text;
  pL:= 1;
  while (pL <= Length(s)) and (s[pL] <> '#') do inc(pL);
  if pL > Length(s) then begin // no # found
    s:= '#' + s + '#';
    pL:= 1;
    pR:= Length(s);
  end else begin // search second #
    pR:= pL + 1;
    while (pR <= Length(s)) and (s[pR] <> '#') do inc(pR);
    if pR > Length(s) then  // no second # found
      if pL = 1 then begin
        s:= s + '#';
        pR := Length(s);
      end else begin
        s:= '#' + s;
        pR:= pL + 1;
        pL:= 1;
      end;
  end;
  case RGAdjustment.ItemIndex of
    0: insert(CBFooter.Text, s, pL);
    1: insert(CBFooter.Text, s, pR);
    2: s:= s + CBFooter.Text;
  end;
  EFooter.Text:= s;
end;

procedure TFConfiguration.RGCommentClick(Sender: TObject);
  var s: string;
begin
  case RGComment.ItemIndex of
    0: begin s:= JavaDocComment; MComment.ReadOnly:= true; end;
    1: begin s:= ShortComment;   MComment.ReadOnly:= true; end;
    2: begin s:= FreeComment;    MComment.ReadOnly:= false end;
  end;
  MComment.Text:= s;
end;

procedure TFConfiguration.BTempFolderClick(Sender: TObject);
  var s: string;
begin
  if PortableApplication
    then s:= EditorFolder + 'App\Temp'
    else s:= GetTempDir;
  { $WARN SYMBOL_PLATFORM OFF}
  s:= IncludeTrailingPathDelimiter(GetLongPathName(s));
  { $WARN SYMBOL_PLATFORM ON}
  Sysutils.ForceDirectories(s);
  ShortenPath(ETempFolder, s);
end;

procedure TFConfiguration.SBTempSelectClick(Sender: TObject);
  var s: string;
begin
  if PortableApplication
    then s:= EditorFolder + 'App\Temp'
    else s:= GetTempDir;
  { $WARN SYMBOL_PLATFORM OFF}
  s:= IncludeTrailingPathDelimiter(GetLongPathName(s));
  { $WARN SYMBOL_PLATFORM ON}
  FolderSelect(ETempFolder, s);
  Sysutils.ForceDirectories(s);
  ShortenPath(ETempFolder, s);
end;

procedure TFConfiguration.BJavaDocParameterClick(Sender: TObject);
begin
  EDocParameter.Text:= '-author -version';
end;

procedure TFConfiguration.BJavaFXFolderClick(Sender: TObject);
begin
  var Dir:= EJavaFXFolder.Hint;
  if not Sysutils.DirectoryExists(Dir) then
    Dir:= GetEnvironmentVariable('PATH_TO_FX');
  if not Sysutils.DirectoryExists(Dir) then
    Dir:= JDKFolder;
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= Dir;
  if FolderDialog.Execute then begin
    Dir:= withoutTrailingSlash(FolderDialog.Filename);
    if EndsWith(Dir, '\lib') then
      Dir:= copy(Dir, 1, length(Dir)-4);
    Dir:= ExcludeTrailingBackslash(Dir);
    ShortenPath(EJavaFXFolder, Dir);
    CheckAllFilesAndFolders;
  end;
  {$WARNINGS ON}
end;

procedure TFConfiguration.BJarParameterClick(Sender: TObject);
begin
  EJarParameter.Text:= '-cfv';
end;

procedure TFConfiguration.BManifestClick(Sender: TObject);
begin
  EJarManifest.Text:= '';
end;

procedure TFConfiguration.BJarFilesClick(Sender: TObject);
begin
  var FJarCreate:= TFJarCreateDialog.Create(Self);
  try
    FJarCreate.Caption:= _('Jar create files');
    FJarCreate.Init(JarCreateAll);
    if FJarCreate.ShowModal = mrOK then begin
      JarCreateCurrent:= FJarCreate.JarCreateCurrent;
      JarCreateAll:= FJarCreate.JarCreateAll;
      EJarCreate.Text:= JarCreateCurrent;
    end;
  finally
    FreeAndNil(FJarCreate);
  end;
end;

procedure TFConfiguration.CBKeyboardChange(Sender: TObject);
  var Shortcut: TNode; s: string;
begin
  LCollision.Visible:= false;
  s:= CBKeyboard.Items[CBKeyboard.ItemIndex];
  Shortcut:= KeyboardShortcutsTree.getNode(TextToShortCut(s));
  if Assigned(Shortcut) then begin
    MKeyboard.Text:= Shortcut.Data;
    LCollision.Visible:= Shortcut.Collision;
  end;
end;

procedure TFConfiguration.RGKeyboardClick(Sender: TObject);
begin
  LCollision.Visible:= false;
  case RGKeyboard.ItemIndex of
    0: MKeyboard.Text:= EditorKeys.Text;
    1: MKeyboard.Text:= MenuKeys.Text;
    2: MKeyboard.Text:= AllKeys.Text;
  end;
end;

procedure TFConfiguration.BKeyboardFileClick(Sender: TObject);
begin
  FileSelect(EKeyboardFile, '*.txt|*.txt', 'JEKeyboard.txt', 'templates');
  CheckFile(EKeyboardFile, true);
  if EKeyboardFile.Color = clWindow then begin
    KeyboardFile:= EKEyboardFile.Hint;
    ApplyKeyboardShortcuts;
  end;
end;

procedure TFConfiguration.BVisDefaultClick(Sender: TObject);
  var i, j: integer;

  procedure DefaultVis(var arr: array of boolean);
  begin
    for var i:= 0 to High(arr) do
      arr[i]:= true;
  end;

begin
  DefaultVis(visTabs);
  visTabs[4]:= false;      // Tab Layout
  for i:= 0 to maxTab do   // items on tabs
    for j:= 0 to maxTabItem do
      vis2[i, j]:= true;
  DefaultVis(visMenus);
  visMenus[3]:= false;     // Menu Components
  DefaultVis(visToolbars);
  visToolbars[5]:= false;  // Toolbar BorderLayout

  for i:= 0 to high(visTabs) do
    LVVisibilityTabs.Items[i].Checked:= visTabs[i];
  for i:= 0 to LVVisibilityElements.Items.Count - 1 do
    LVVisibilityElements.Items[i].Checked:= true;
  for i:= 0 to high(visMenus) do
    LVVisibilityMenus.Items[i].Checked:= visMenus[i];
  for i:= 0 to high(visToolbars) do
    LVVisibilityToolbars.Items[i].Checked:= visToolbars[i];
end;

procedure TFConfiguration.BVorlageClick(Sender: TObject);
begin
  with ODSelect do begin
    InitialDir:= EditorFolder + 'templates';
    Filename:= '*.java';
    Filter:= '*.java|*.java;*.*|*.*';
    if Execute then
      case TButton(Sender).Tag of
        1: ShortenPath(ETemplateConsole, Filename);
        2: ShortenPath(ETemplateFrame, Filename);
        3: ShortenPath(ETemplateDialog, Filename);
        4: ShortenPath(ETemplateApplet, Filename);
        5: ShortenPath(ETemplateJFrame, Filename);
        6: ShortenPath(ETemplateJDialog, Filename);
        7: ShortenPath(ETemplateJApplet, Filename);
        8: ShortenPath(ETemplateApplication, Filename);
        9: ShortenPath(ETemplateControlstructure, Filename);
       10: ShortenPath(ETemplateClass, Filename);
       11: ShortenPath(EMindstormsTemplate, Filename);
       12: ShortenPath(ETemplateJunitTest, Filename);
      end;
    CheckFile(ETemplateConsole, true);
    CheckFile(ETemplateFrame, true);
    CheckFile(ETemplateDialog, true);
    CheckFile(ETemplateApplet, true);
    CheckFile(ETemplateJFrame, true);
    CheckFile(ETemplateJDialog, true);
    CheckFile(ETemplateJApplet, true);
    CheckFile(ETemplateApplication, true);
    CheckFile(ETemplateControlstructure, true);
    CheckFile(ETemplateClass, true);
    CheckFile(EMindstormsTemplate, true);
    CheckFile(ETemplateJUnitTest, true);
  end;
end;

procedure TFConfiguration.SetDefaultBracketColor;
begin
  AttrBrackets.Foreground:= clRed;
  AttrBrackets.Background:= 14737632; //clSilver;
  AttrBrackets.IntegerStyle:= 0;
end;

procedure TFConfiguration.DoHelp(const afile: string);
begin
  FJava.CallHelp(afile);
end;

var
  en: array[0..MaxPages] of string =
      ('java', 'interpreter', 'compiler', 'programs', 'applets', 'disassembler', 'jar',
       'editor', 'options', 'code', 'colors', 'comment', 'templates', 'keyboard',
       'structogram', 'sequencediagram', 'browser', 'documentation', 'printer', 'mindstorms', 'android',
       'language', 'options', 'restrictions', 'associations', 'uml', 'uml2', 'llm_assistant', 'llm_chat',
       'visibility', 'protocols', 'tools', 'git', 'junit', 'checkstyle', 'jalopy', 'subversion');
  de: array[0..MaxPages] of string =
      ('java', 'interpreter', 'compiler', 'programme', 'applets', 'disassembler', 'jar',
       'editor', 'optionen', 'code', 'farben', 'kommentar', 'vorlagen', 'tastatur',
       'struktogramm', 'sequenzdiagramm', 'browser', 'dokumentation', 'drucker', 'mindstorms', 'android',
       'sprache', 'optionen', 'restriktionen', 'verknüpfungen', 'uml', 'uml2', 'llm_assistent', 'llm_chat',
       'sichtbarkeit', 'protokolle', 'tools', 'git', 'junit', 'checkstyle', 'jalopy', 'subversion');

procedure TFConfiguration.BHelpClick(Sender: TObject);
  var count: integer; aNode: TTreeNode;
begin
  with FJava do begin
    if PInterpreter.Visible then
      DoHelp(getJavaTools('java.html'))
    else if PCompiler.Visible
      then DoHelp(getJavaTools('javac.html'))
    else if PPrograms.Visible then begin
      DoHelp(getJavaTools('java.html'));
      DoHelp(getJavaTools('jdb.html'));
      DoHelp(getJavaTools('javadoc.html'));
    end else if PApplets.Visible then
      DoHelp(getJavaTools('appletviewer.html'))
    else if PDisassembler.Visible then
      DoHelp(getJavaTools('javap.html'))
    else if PJar.Visible
      then DoHelp(getJavaTools('jar.html'))
    else if PDocumentation.Visible then
      DoHelp(getJavaTools('javadoc.html'))
    else if PMindstorms.Visible
      then DoHelp('https://lejos.sourceforge.io/');

    aNode:= TVConfiguration.Items.GetFirstNode;
    count:= 0;
    while (count < TVConfiguration.Items.Count) and (aNode <> TVConfiguration.Selected) do begin
      aNode:= aNode.GetNext;
      inc(count);
    end;
    if isGerman
      then DoHelp(GetConfigurationAddress(de[count]))
      else DoHelp(GetConfigurationAddress(en[count]));
  end;
end;

procedure TFConfiguration.BCheckClick(Sender: TObject);
begin
  CheckAllFilesAndFolders;
end;

{$WARNINGS OFF}
function TFConfiguration.getDumpText: string;
  var Pathname1, Pathname2, Pathname3, s: string;
      SL, SL1: TStringList; Editor: TFEditForm;
begin
  Result:= '';
  Pathname1:= TempDir + 'Configuration.ini';
  Pathname2:= TempDir + 'Configuration2.ini';
  Pathname3:= TempDir + 'Configuration3.ini';
  Editor:= TFEditForm(FJava.getTDIWindow(Pathname1));
  if Assigned(Editor) then begin
    Editor.Save(false);
    Editor.Close;
    Application.ProcessMessages;
  end;
  try
    if FileExists(Pathname1) then DeleteFile(Pathname1);
    if FileExists(Pathname2) then DeleteFile(Pathname2);
    if FileExists(Pathname3) then DeleteFile(Pathname3);

    DumpIniFileHKCU:= TMemIniFile.Create(Pathname1);
    DumpIniFileHKLM:= TMemIniFile.Create(Pathname2);
    DumpIniFileAllUsers:= TMemIniFile.create(Pathname3);
    DumpMode:= true;
    ModelToRegistry;
    DumpMode:= false;
    DumpIniFileHKCU.UpdateFile;
    DumpIniFileAllUsers.UpdateFile;
    DumpIniFileHKLM.UpdateFile;

    s:= 'Installation ' + CrLf;
    s:= s + '  JE-Version: ' + UDlgAbout.Version + ', ' + TFAbout.GetDate + CrLf;
    s:= s + '  Java-Version: ' + IntToStr(GetJavaVersion) + CrLf;
    s:= s + '  Windows-Version: ' + TOSVersion.ToString + CrLF;
    s:= s + '  CmdLine: ' + CmdLine + CrLf;
    SL:= TStringList.Create;
    SL1:= TStringList.Create;
    if UseRegistry then begin
      s:= s + CrLf;
      s:= s + '  regedit: HKEY_LOCAL_MACHINE\SOFTWARE\JavaEditor ' + CrLf;
      s:= s + '  regedit: HKEY_CURRENT_USER\SOFTWARE\JavaEditor ' + CrLf;
      s:= s + StringOfChar('-', 80) + CrLf + CrLf;
      s:= s + '--- HKEY_LOCAL_MACHINE\SOFTWARE\JavaEditor' + CrLf;
      DumpIniFileHKLM.GetStrings(SL);
      s:= s + SL.Text + CrLf;
      SL.Clear;
      DumpIniFileAllUsers.GetStrings(SL);
      s:= s + SL.Text + CrLf;
      SL.Clear;
      s:= s + StringOfChar('-', 80) + CrLf + CrLf;
      s:= s + '--- HKEY_CURRENT_USER\SOFTWARE\JavaEditor' + CrLf;
      DumpIniFileHKCU.GetStrings(SL);
      s:= s + SL.Text;
    end
    else begin
      s:= s + CrLf;
      s:= s + '  ' + MachineIniFile.Filename + CrLf;
      if assigned(UserIniFile) then
        s:= s + '  ' + UserIniFile.Filename + CrLf;
      s:= s + CrLf;
      s:= s + StringOfChar('-', 80) + CrLf;
      s:= s + '--- ' + MachineIniFile.Filename + CrLf + CrLf;
      SL1.LoadFromFile(MachineIniFile.Filename);
      s:= s + SL1.Text + CrLf;
      s:= s + StringOfChar('-', 80) + CrLf;
      if Assigned(UserIniFile) then begin
        s:= s + '--- ' + UserIniFile.FileName + CrLf + CrLf;
        SL1.Clear;
        SL1.LoadFromFile(UserIniFile.Filename);
        s:= s + SL1.Text;
        s:= s + StringOfChar('-', 80) + CrLf;
      end else
        s:= s + 'missing ' + UserIniFile.Filename + CrLf;
    end;
    FreeAndNil(SL);
    FreeAndNil(SL1);
    Result:= s + CrLf;

    FreeAndNil(DumpIniFileHKCU);
    FreeAndNil(DumpIniFileAllUsers);
    FreeAndNil(DumpIniFileHKLM);
  except
    on e: exception do
      ErrorMsg(e.Message + ' ' + _('Could not create preferences.'));
  end;
end;
{$WARNINGS ON}

procedure TFConfiguration.BDumpClick(Sender: TObject);
begin
  var SL:= TStringList.Create;
  SL.Text:= getDumptext;
  var Pathname:= TPath.Combine(TempDir, 'Configuration.ini');
  SL.SaveToFile(Pathname);
  FreeAndNil(SL);
  FJava.Open(Pathname);
end;

procedure TFConfiguration.LMouseEnter(Sender: TObject);
begin
  Screen.Cursor:= crHandpoint;
end;

procedure TFConfiguration.LMouseLeave(Sender: TObject);
begin
  Screen.Cursor:= crDefault;
end;

procedure TFConfiguration.BCheckstyleInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= getTargetDir('checkstyle');
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('Checkstyle');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + SL.Values['zip2']);
        ShowModal;
        if DownloadIsOK then begin
          DownloadFile(SL.Values['file2'], source + '\mycheckstyle.xml');
          if VistaOrBetter
            then CallUpdater(Target, Source + SL.Values['zip2'], Source + '\mycheckstyle.xml')
          else begin
            ExtractZipToDir(source + SL.Values['zip2'], target);
            CopyFile(PChar(source + '\mycheckstyle.xml'), PChar(target + '\mycheckstyle.xml'), false);
          end;
        end;
        ShortenPath(ECheckstyle, target + SL.Values['file1']);
        ShortenPath(ECheckstyleConfiguration, target + '\mycheckstyle.xml');
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end
  else
    ErrorMsg(_(LNGMissingAdminRights));
  Sleep(800);
  CheckFile(ECheckstyle, true);
  CheckFile(ECheckstyleConfiguration, true);
end;

procedure TFConfiguration.SBCheckStyleSelectClick(Sender: TObject);
begin
  FileSelect(ECheckstyle, 'jar|*.jar', 'checkstyle-all*.jar', 'checkstyle');
  if ECheckstyleConfiguration.Hint = '' then begin
    var s:= ExtractFilePath(ECheckstyle.Hint) + 'mycheckstyle.xml';
    ShortenPath(ECheckstyleConfiguration, s);
  end;
  CheckFile(ECheckstyle, true);
  CheckFile(ECheckstyleConfiguration, true);
end;

procedure TFConfiguration.BCheckstyleConfigurationClick(Sender: TObject);
begin
  FileSelect(ECheckstyleConfiguration, 'xml|*.xml', '*.xml', 'checkstyle');
  CheckFile(ECheckstyleConfiguration, true);
end;

procedure TFConfiguration.LJavaInterpreterClick(Sender: TObject);
begin
  DoHelp('https://www.oracle.com/java/');
end;

procedure TFConfiguration.LInterpreterParameterClick(Sender: TObject);
begin
  DoHelp(getJavaTools('java.html'));
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
  DoHelp(getJavaTools('javac.html'));
end;

procedure TFConfiguration.LGitFolderClick(Sender: TObject);
begin
  DoHelp('https://git-scm.com/');
end;

procedure TFConfiguration.LAppletviewerClick(Sender: TObject);
begin
  DoHelp(getJavaTools('appletviewer.html'));
end;

procedure TFConfiguration.LDebuggerClick(Sender: TObject);
begin
  DoHelp(getJavaTools('jdb.html'));
end;

procedure TFConfiguration.LDisassemblerClick(Sender: TObject);
begin
  DoHelp(getJavaTools('javap.html'));
  FJava.CallHelp('https://java-decompiler.github.io/');
end;

procedure TFConfiguration.LJavaDocClick(Sender: TObject);
begin
  DoHelp(getJavaTools('javadoc.html'));
end;

procedure TFConfiguration.LCheckstyleClick(Sender: TObject);
begin
  FJava.CallHelp('https://checkstyle.sourceforge.io/');
end;

procedure TFConfiguration.LJarClick(Sender: TObject);
begin
  DoHelp(getJavaTools('jar.html'));
end;

procedure TFConfiguration.LLejosClick(Sender: TObject);
begin
  FJava.CallHelp('https://lejos.sourceforge.io/');
end;

procedure TFConfiguration.BMindstormsParameterClick(Sender: TObject);
begin
  if RGMindStormsVersion.ItemIndex = 0
    then EMindstormsParameter.Text:= '-target 1.1 -source 1.2'
    else EMindstormsParameter.Text:= '';
end;

procedure TFConfiguration.LJavabookClick(Sender: TObject);
begin
  DoHelp('https://www.javabuch.de/');
end;

procedure TFConfiguration.BCacheClick(Sender: TObject);
begin
  if PortableApplication
    then ECache.Text:= EditorFolder + 'App\Cache'
    else ECache.Text:= GetHomePath + '\JavaEditor\Cache';
  Sysutils.ForceDirectories(ECache.Text);
  ShortenPath(ECache, ECache.Text);
  CheckFolder(ECache, false);
end;

procedure TFConfiguration.SBAndroidSDKFolderClick(Sender: TObject);
begin
  var Dir:= EAndroidSDKFolder.Hint;
  if not Sysutils.DirectoryExists(Dir) then
    Dir:= GetEnvironmentVariable('PROGRAMFILES');
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= Dir;
  if FolderDialog.Execute then begin
    Dir:= withoutTrailingSlash(FolderDialog.Filename);
    EAndroidSDKFolder.Text:= Dir;
    CheckAllFilesAndFolders;
  end;
  {$WARNINGS ON}
end;

procedure TFConfiguration.SBCacheSelectClick(Sender: TObject);
  var dir: string;
begin
  if PortableApplication
    then Dir:= EditorFolder + 'App\Cache'
    else Dir:= GetHomePath + '\JavaEditor\Cache';
  Sysutils.ForceDirectories(Dir);
  FolderSelect(ECache, Dir);
end;

procedure TFConfiguration.CallUpdater(const Target, Source1: string; Source2: string);
begin
  if Source2 = '' then Source2:= 'xxx';

  var Updater:= EditorFolder + 'setup.exe';
  var s:= '-Update ' + HideBlanks(Target) + ' ' + HideBlanks(Source1) + ' ' + HideBlanks(Source2);
  if not UseRegistry then
    s:= s + ' -INI ' + HideBlanks(MachineIniFile.FileName);
  if FileExists(Updater) then
    try
      RunAsAdmin(Handle, Updater, s);
    except
      on e: exception do
        ErrorMsg(e.Message);
    end
  else
    ErrorMsg(Format(_(LNGFileNotFound), [Updater]));
end;

function TFConfiguration.GetConfigurationAddress(const s: string): string;
begin
  if isGerman
    then Result:= Homepage + '/doku.php?id=de:konfiguration#' + s
    else Result:= Homepage + '/doku.php?id=en:configuration#' + s;
end;

procedure TFConfiguration.LTutorialClick(Sender: TObject);
begin
  DoHelp(GetConfigurationAddress('ducumentation'));
end;

procedure TFConfiguration.DecideProxy;
  var p: integer; aEnabled: boolean; s: string;
begin
  withProxy:= false;
  ProxyPort:= 0;
  ProxyIP:= '';
  with MyRegistry do begin
    RootKey:= HKEY_CURRENT_USER;
    Access:= KEY_READ;
    if OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings', false) then begin
      try
        aEnabled:= ReadBool('ProxyEnable');
        ProxyIP:= ReadString('ProxyServer');
        p:= Pos(':', ProxyIP);
        if p > 0 then begin
          s:= Copy(ProxyIP, p+1, length(ProxyIP));
          if s <> '' then ProxyPort:= StrToInt(s) else ProxyPort:= 0;
          delete(ProxyIP, p, length(ProxyIP));
        end;
        withProxy:= aEnabled and (ProxyIP <> '') and (ProxyPort <> 0);
      except
      end;
    end;
  end;
end;

function TFConfiguration.JavaDocComment(Indent: string= ''): string;
begin
  Result:=
    Indent + '/**' + CrLf +
    Indent + ' *' + CrLf +
    Indent + ' * ' + _('Description') + CrLf +
    Indent + ' *' + CrLf +
    Indent + ' * @version 1.0 ' + _('from') + ' %DATE%' + CrLf +
    Indent + ' * @author %AUTHOR%' + CrLf +
    Indent + ' */' + CrLf;
end;

function TFConfiguration.ShortComment(Indent: string = ''): string;
begin
  Result:= Indent + '// Author: %AUTHOR%' + CrLf + '// Date: %DATE%' + CrLf;
end;

function TFConfiguration.HeadText(Indent: string = ''): string;
  var s: string; p: Integer;
begin
  case CommentKind of
    0: s:= JavaDocComment(Indent);
    1: s:= ShortComment(Indent);
    2: s:= FreeComment;
  end;
  p:= Pos('%AUTHOR%', UpperCase(s));
  if p > 0 then s:= ReplaceStr(s, '%AUTHOR%', JavaAuthor);
  p:= Pos('%DATE%', Uppercase(s));
  if p > 0 then s:= ReplaceStr(s, '%DATE%', DateToStr(Date));
  Result:= s;
end;

procedure TFConfiguration.BJalopyInstallClick(Sender: TObject);
  var target, source: string; SL: TStringList;
begin
  target:= getTargetDir('jalopy');
  source:= TempDir + 'javaeditor';
  if UpdatePossible(source, target) then begin
    with TFDownload.create(Self) do
    try
      SL:= GetDownloadFiles('Jalopy');
      if SL.Text <> '' then begin
        SetUrlAndFile(SL.Values['zip1'], source + '\jalopy.zip');
        ShowModal;
        if DownloadIsOK then begin
          DownloadFile(SL.Values['file2'], source + '\myjalopy.xml');
          if VistaOrBetter
            then CallUpdater(Target, Source + '\jalopy.zip', Source + '\myjalopy.xml')
          else begin
            ExtractZipToDir(source + '\jalopy.zip', target);
            CopyFile(PChar(source + '\myjalopy.xml'), PChar(target + '\myjalopy.xml'), false);
          end;
          ShortenPath(EJalopy, target + SL.Values['file1']);
          ShortenPath(EJalopyConfiguration, target + '\myjalopy.xml');
        end;
      end else
        ErrorMsg(_(LNGNoInternetConnection));
    finally
      FreeAndNil(SL);
      Free;
    end;
  end else
    ErrorMsg(_(LNGMissingAdminRights));

  Sleep(800);
  CheckFile(EJalopy, true);
  CheckFile(EJalopyConfiguration, true);
end;

procedure TFConfiguration.SBJalopySelectClick(Sender: TObject);
begin
  FileSelect(EJalopy, 'jar|*.jar', 'jalopy-console.jar', 'jalopy');
  CheckFile(EJalopy, true);
end;

procedure TFConfiguration.BJalopyConfigurationClick(Sender: TObject);
begin
  FileSelect(EJalopyConfiguration, 'xml|*.xml', '*.xml', 'jalopy');
  CheckFile(EJalopyConfiguration, true);
end;

procedure TFConfiguration.LJalopyClick(Sender: TObject);
begin
  DoHelp('http://jalopy.sourceforge.net/');
end;

function TFConfiguration.AddPortableDrives(s: string): string;
begin
  Result:= s;
  //if not PortableApplication then exit;
  var e:= '';
  var p:= Pos(';', s);
  while p > 0 do begin
    e:= e + AddPortableDrive(copy(s, 1, p));
    delete(s, 1, p);
    p:= Pos(';', s);
  end;
  e:= e + AddPortableDrive(s);
  Result:= e;
end;

function TFConfiguration.RemovePortableDrives(s: string): string;
begin
  Result:= s;
  if not PortableApplication then exit;
  var e:= '';
  var p:= Pos(';', s);
  while p > 0 do begin
    e:= e + RemovePortableDrive(copy(s, 1, p));
    delete(s, 1, p);
    p:= Pos(';', s);
  end;
  e:= e + RemovePortableDrive(s);
  Result:= e;
end;

function TFConfiguration.RemovePortableDrive(const s: string; folder: string = ''): string;
  // if folder is set then switch to relative paths, used in Store/FetchDiagram
  var SL1, SL2: TStringList;
      i, j: integer; Basefolder: string;
begin
  Result:= s;
  if PortableApplication
    then Basefolder:= Editorfolder
    else Basefolder:= folder;
  // same drive
  if TPath.isDriveRooted(Basefolder) and
     (Uppercase(copy(Basefolder, 1, 2)) = Uppercase(copy(s, 1, 2)))
  then begin
    SL1:= Split('\', withoutTrailingSlash(Basefolder));
    SL2:= Split('\', withoutTrailingSlash(s));
    i:= 0;
    while (i < SL1.Count) and (i < SL2.Count) and
          (Uppercase(SL1.Strings[i]) = Uppercase(SL2.Strings[i])) do
      inc(i);
    Result:= '';
    j:= i;
    while i < SL1.Count do begin
      Result:= Result + '..\';
      inc(i);
    end;
    while j < SL2.Count do begin
      Result:= Result + SL2.Strings[j] + '\';
      inc(j);
    end;
    Result:= withoutTrailingSlash(Result);
    FreeAndNil(SL1);
    FreeAndNil(SL2);
  end;
end;

function TFConfiguration.AddPortableDrive(const s: string; folder: string = ''): string;
  // if folder is set then switch to relative paths, used in Store/FetchDiagram
  var SL1, SL2: TStringList;
      i, j: integer; Basefolder: string;
begin
  Result:= s;
  if s.startsWith('its:') or (s = '') or
     (copy(s, 2, 2) = ':\') or (copy(s, 1, 7) = 'file://') or  // absolute path
     (Pos('\\', s) = 1) or (Pos('://', s) > 1) then   // UNC or https:// or http://
    exit;

  if PortableApplication
    then Basefolder:= Editorfolder
    else Basefolder:= folder;
  if copy(s, 1, 2) = ':\' then // old portable concept
    if length(PortAppDrive) = 1
      then Result:= PortAppDrive + s
      else Result:= PortAppDrive + copy(s, 2, length(s))
  else if Basefolder <> '' then begin // new portaple concept, relative to Basefolder
    SL1:= Split('\', withoutTrailingSlash(Basefolder));
    SL2:= Split('\', withoutTrailingSlash(s));
    i:= 0;
    while (i < SL2.Count) and (SL2.Strings[i] = '..') do
      inc(i);
    Result:= SL1.Strings[0];
    for j:= 1 to SL1.Count - 1 - i do
      Result:= Result + '\' + SL1.Strings[j];
    for j:= i to SL2.Count - 1 do
      Result:= Result + '\' + SL2.Strings[j];
    FreeAndNil(SL1);
    FreeAndNil(SL2);
  end;
end;

function TFConfiguration.getFileInCHM(s: string): string;
begin
  var p:= Pos('.CHM', Uppercase(s));
  delete(s, 1, p + 3);
  if copy(s, 1, 2) = '::' then
    delete(s, 1, 2);
  result:= s;
end;

function TFConfiguration.getCHMJavaManual(const s: string): string;
begin
  var s1:= getFileInCHM(s);
  if s1 = '' then begin
    s1:= ExtractFileName(JavaManual);
    s1:= ChangeFileExt(s1, '');
    s1:= '\' + s1 + '\index.html';
  end;
  Result:= 'its:' + JavaManual + '::' + s1;
end;

function TFConfiguration.getJavaManual: string;
begin
  if IsCHM(JavaManual) and FileExists(JavaManual)
    then Result:= JavaManual + JavaCHMRoot
  else if IsHttp(JavaManual)
    then Result:= withoutTrailingSlash(ExtractFilePathEx(JavaManual))
  else Result:= withoutTrailingSlash(ExtractFilePathEx(JavaManual));
end;

function TFConfiguration.getJavaManualFX: string;
begin
  Result:= ExtractFilePathEx(JavaManualFX);
end;

procedure TFConfiguration.RGMindstormsVersionClick(Sender: TObject);
begin
  var s:= EMindstormsParameter.text;
  if (s = '-target 1.1 -source 1.2') or (s = '-target 1.1 -source 1.3') then
    BMindstormsParameterClick(Self);
  SetMindstormsVersion;
end;

procedure TFConfiguration.SetMindstormsVersion;
begin
  var version:= RGMindStormsVersion.ItemIndex;
  LLejosCompiler.Visible:= (version < 2);
  ELejosCompiler.Visible:= (version < 2);
  SBLejosCompiler.Visible:= (version < 2);
  LUploader.Visible:= (version < 2);
  ELejosUploader.Visible:= (version < 2);
  SBLejosUploader.Visible:= (version < 2);
  LFlasher.Visible:= (version < 2);
  ELejosFlasher.Visible:= (version < 2);
  SBLejosFlasher.Visible:= (version < 2);
  BMindstormsParameter.Visible:= (version < 2);

  case version of
    0: begin
         ELejosUploader.Enabled:= false;
         LMindstormsPort.Visible:= true;
         CBMindstormsPort.Visible:= true;
         LMindstormsIP.Visible:= false;
         EMindstormsIP.Visible:= false;
         LejosVerzeichnis:= RCXFolder;
         LejosCompiler:= RCXCompiler;
         LejosUploader:= RCXUploader;
         LejosFlasher:= RCXFlasher;
         MindstormsManual:= RCXManual;
       end;
    1: begin
         ELejosCompiler.Enabled:= not LockedPaths;
         ELejosUploader.Enabled:= not LockedPaths;
         ELejosFlasher.Enabled:= not LockedPaths;
         LMindstormsPort.Visible:= true;
         CBMindstormsPort.Visible:= true;
         LMindstormsIP.Visible:= false;
         EMindstormsIP.Visible:= false;
         LejosVerzeichnis:= NXTFolder;
         LejosCompiler:= NXTCompiler;
         LejosUploader:= NXTUploader;
         LejosFlasher:= NXTFlasher;
         MindstormsManual:= NXTManual
       end;
    2: begin
         LMindstormsPort.Visible:= false;
         CBMindstormsPort.Visible:= false;
         LMindstormsIP.Visible:= true;
         LMindstormsIP.Top:= 244;
         EMindstormsIP.Visible:= true;
         EMindstormsIP.Top:= 240;
         LejosVerzeichnis:= EV3Folder;
         MindstormsManual:= EV3Manual;
       end;
  end;
  ShortenPath(ELejosFolder, LejosVerzeichnis);
  ShortenPath(ELejosCompiler, LejosCompiler);
  ShortenPath(ELejosUploader, LejosUploader);
  ShortenPath(ELejosFlasher, LejosFlasher);
  ShortenPath(EMindstormsManual, MindstormsManual);
  CheckMindstorms;
end;

procedure TFConfiguration.CheckMindstorms;
begin
  CheckFolder(ELejosFolder, true);
  CheckFile(ELejosCompiler, true);
  CheckFile(ELejosUploader, true);
  CheckFile(ELejosFlasher, true);
  CheckFile(EMindstormsManual, true);
  CheckFile(EMindstormsTemplate, true);
end;

procedure TFConfiguration.SaveInis;
begin
  if not UseRegistry then
    try
      if not WriteProtection and assigned(MachineIniFile) then
        MachineIniFile.UpdateFile;
      if Assigned(UserIniFile) then
        UserIniFile.UpdateFile;
    except
    end;
end;

procedure TFConfiguration.BSVNClick(Sender: TObject);
begin
  var Dir:= ESVNFolder.Hint;
  if not Sysutils.DirectoryExists(Dir) then Dir:= SourcePath;
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= Dir;
  if FolderDialog.Execute then
    ShortenPath(ESVNFolder, FolderDialog.Filename);
  {$WARNINGS ON}
  CheckFolder(ESVNFolder, true);
end;

procedure TFConfiguration.BRepositoryClick(Sender: TObject);
begin
  var Dir:= CBRepository.Hint;
  if not Sysutils.DirectoryExists(Dir) then Dir:= SourcePath;
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= Dir;
  if FolderDialog.Execute then begin
    Dir:= FolderDialog.Filename;
    ShortenPath(CBRepository, Dir);
    Sysutils.ForceDirectories(Dir);
    if not FSubversion.IsRepository(Dir) then
      FSubversion.CallSVN('\svnadmin.exe', 'create ' + HideBlanks(Dir), Dir);
  end;
  {$WARNINGS ON}
  CheckFolderCB(CBRepository);
end;

procedure TFConfiguration.LSVNClick(Sender: TObject);
begin
  DoHelp('https://subversion.apache.org/');
end;

procedure TFConfiguration.SBOpenClick(Sender: TObject);
begin
  var s:= '';
  case (Sender as TButton).Tag of
    4: s:= EMindstormsManual.Hint;
    5: s:= EMindstormsTemplate.Hint;
   12: s:= ECache.Hint;
   17: s:= EKeyBoardFile.Hint;
  end;
  if FileExists(s) then
    if Pos('.html', s) > 0
      then DoHelp(s)
      else FJava.Open(s)
  else if Sysutils.DirectoryExists(s) then
    FJava.NewExplorer(s, '');
end;

function TFConfiguration.SetRCXNXTEV3(const s: string): string;
begin
  case MindstormsVersion of
    0: Result:= s;
    1: Result:= AnsiReplaceStr(s, 'RCX', 'NXJ');
    2: Result:= AnsiReplaceStr(s, 'RCX', 'EV3');
  end;
end;

function TFConfiguration.getJavaVersion: integer;
  var s: string; i, p: integer;
begin
  if JavaVersion = 0 then begin
    JavaVersion:= 6;
    var SL:= TStringList.Create;
    try
      try
        if FileExists(JDKFolder + '\release') then begin  // since 1.7.0
          SL.LoadFromFile(JDKFolder + '\release');
          i:= 0;
          p:= Pos('JAVA_VERSION', SL[i]);
          while (p = 0) and (i < SL.Count) do begin
            inc(i);
            p:= Pos('JAVA_VERSION', SL[i]);
          end;
          s:= SL[i]; // JAVA_VERSION="1.7.0"
        end else if myJavaCommands.ExecAndWait(JavaInterpreter, '-version', '.', TempDir + 'version.txt', SW_HIDE, false) then begin
          SL.LoadFromFile(TempDir + 'version.txt');
          s:= SL[0];   // java version "1.5.4-rc"
        end;
        delete(s, 1, length('JAVA_VERSION="'));
        p:= Pos('"', s);
        delete(s, p, 1);
        p:= Pos('.', s);
        if (p = 2) and (s[1] = '1') then begin
          delete(s, 1, Pos('.', s));
          delete(s, 2, length(s));
        end else if p > 1 then
          s:= copy(s, 1, p-1);
        if not TryStrToInt(s, JavaVersion) then
          JavaVersion:= 6;
      except
      end;
    finally
      FreeAndNil(SL);
    end;
  end;
  Result:= JavaVersion;
end;

function TFConfiguration.getDocumentationVersion: integer;
  var IndexFile: string; i: integer;
begin
  if DocumentationVersion = 0 then begin
    DocumentationVersion:= 7;
    IndexFile:= getJavaManual + '\index.html';
    if GlobalFileExists(IndexFile, false) then begin
      var SL:= TStringList.Create;
      try
        try
          SL.Text:= myCodeCompletion.LoadTextFromFile(IndexFile);
          for i:= 9 to 50 do
            if Pos('Java SE ' + IntToStr(i), SL.Text) > 0 then begin
              DocumentationVersion:= i;
              break;
            end;
          if DocumentationVersion = 7 then
            if Pos('Standard Edition 8',  SL.Text) > 0 then DocumentationVersion:= 8 else
            if Pos('Standard Edition 7',  SL.Text) > 0 then DocumentationVersion:= 7 else
            if Pos('JDK 6 Documentation', SL.Text) > 0 then DocumentationVersion:= 6 else
            if Pos('JDK 5 Documentation', SL.Text) > 0 then DocumentationVersion:= 5;
        except
        end;
      finally
        FreeAndNil(SL);
      end;
    end;
  end;
  Result:= DocumentationVersion;
end;

procedure TFConfiguration.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  FJava.ActiveTool:= -1;
end;

procedure TFConfiguration.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveInis;
  CanClose:= true;
end;

procedure TFConfiguration.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FolderDialog);
  FreeAndNil(ODSelect);
  FreeAndNil(JavaHighlighter);
  FreeAndNil(HTMLHighlighter);

  FreeAndNil(EditFont);
  FreeAndNil(UMLFont);
  FreeAndNil(StructogramFont);
  FreeAndNil(SequenceFont);
  FreeAndNil(AttrBrackets);

  FreeAndNil(MyRegistry);
  FreeAndNil(MachineIniFile);
  FreeAndNil(UserIniFile);
  FreeAndNil(LNGTVItems);
  FreeAndNil(KeyboardShortcutsTree);
  FreeAndNil(EditorAndMenuShortcuts);

  FreeAndNil(AllClasses);
  FreeAndNil(AllInterfaces);
  FreeAndNil(AllPackages);
  FreeAndNil(AllClasspathClasses);
  FreeAndNil(MenuKeys);
  FreeAndNil(EditorKeys);
  FreeAndNil(AllKeys);
  FreeAndNil(ImportCache);
  FreeAndNil(ExternalStyleFilesDict);
  FreeAndNil(FPreview);
  FreeAndNil(LoadedStylesDict);

  for var i:= 1 to 21 do
    FreeAndNil(ControlStructureTemplates[i]);
  FConfiguration:= nil;
  FreeAndNil(AllDocumentations);
  FreeAndNil(fLanguagesList);
end;

procedure TFConfiguration.FormShow(Sender: TObject);
begin
  FJava.ActiveTool:= 15;
end;

procedure TFConfiguration.BFontClick(Sender: TObject);
begin
  FScpHint.MIFontClick(Self);
end;

procedure TFConfiguration.BGitFolderClick(Sender: TObject);
begin
  var Dir:= EGitFolder.Hint;
  if not Sysutils.DirectoryExists(Dir) then Dir:= SourcePath;
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= Dir;
  if FolderDialog.Execute then
    ShortenPath(EGitFolder, FolderDialog.Filename);
  {$WARNINGS ON}
  CheckFolder(EGitFolder, true);
end;

procedure TFConfiguration.BGitCloneClick(Sender: TObject);
  var Dir, Remote, aName: string;
begin
  Screen.Cursor:= crHourGlass;
  try
    Dir:= CBLocalRepository.Text;
    Remote:= CBRemoteRepository.Text;
    if not Sysutils.DirectoryExists(Dir) then SysUtils.ForceDirectories(Dir);
    FGit.GitCall('clone ' + Remote + ' ', Dir);
    aName:= ExtractFileNameEx(Remote);
    aName:= ChangeFileExt(aName, '');
    if not EndsWith(Dir, '\' + aName) then
      CBLocalRepository.Text:= Dir + '\' + aName;
    if CBRemoteRepository.Items.IndexOf(Remote) = -1 then
      CBRemoteRepository.Items.Insert(0, Remote);
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFConfiguration.BGitRepositoryClick(Sender: TObject);
begin
  var Dir:= CBLocalRepository.Text;
  if not Sysutils.DirectoryExists(Dir) then Dir:= SourcePath;
  {$WARNINGS OFF}
  FolderDialog.DefaultFolder:= Dir;
  if FolderDialog.Execute then begin
    Dir:= FolderDialog.Filename;
    SysUtils.ForceDirectories(Dir);
    if not FGit.IsRepository(Dir) then
      FGit.GitCall('init', Dir);
    if CBLocalRepository.Items.IndexOf(Dir) = -1 then
      CBLocalRepository.Items.Insert(0, Dir);
    CBLocalRepository.Text:= Dir;
  end;
  {$WARNINGS ON}
  CheckFolderCB(CBLocalRepository);
end;

procedure TFConfiguration.TVConfigurationChange(Sender: TObject; Node: TTreeNode);
  var aNode: TTreeNode; count: integer;
begin
  aNode:= TVConfiguration.Items.GetFirstNode;
  count:= 0;
  while (count < TVConfiguration.Items.Count) and (aNode <> Node) do begin
    aNode:= aNode.GetNext;
    inc(count);
  end;
  if aNode.HasChildren then inc(count);
  
  ShowPage(count);
  CheckAllFilesAndFolders;
  if PCode.Visible then
    if CBManual.Color = clRed
      then begin LCompletionHint1.Show; LCompletionHint2.Show end
      else begin LCompletionHint1.Hide; LCompletionHint2.Hide end;
  if PMindstorms.Visible then SetMindstormsVersion;
end;

function TFConfiguration.getTVConfigurationItem(text: string): TTreeNode;
begin
  for var i:= 0 to LNGTVItems.Count - 1 do
    if LNGTVItems[i] = text then
      exit(TVConfiguration.Items[i]);
  Result:= TVConfiguration.Items[0];
end;

procedure TFConfiguration.ShowPage(i: integer);
begin
  PageList.ActivePageIndex:= i;
  TVConfiguration.Selected:= TVConfiguration.Items[i];
  LTitle.Caption:= TVConfiguration.Items[i].Text;
end;

procedure TFConfiguration.rgcolorsClick(Sender: TObject);
begin
  ShowColorElements;
end;

procedure TFConfiguration.PrepareShow;
begin
  if LVVisibilityTabs.Items.Count = 0 then
    PrepareVisibilityPage;
  ModelToView;
  if FJava.EditorForm <> nil then
    if FJava.EditorForm.IsJava then
      RGColors.ItemIndex:= 0
    else if FJava.EditorForm.IsHTML then
      RGColors.ItemIndex:= 1;
  ShowColorElements;
  CheckAllFilesAndFolders;
  MenuAndEditorShortcuts;
end;

function TFConfiguration.getCharset: string;
begin
  Result:= 'UTF-8';
end;

function TFConfiguration.GetEncoding: string;
begin
  Result:= 'UTF-8';
end;

function TFConfiguration.GetEncoding(const Pathname: string): TEncoding;
  var Stream: TStream;
      EditForm: TFEditForm;
      withBOM: boolean;
begin
  Result:= TEncoding.ANSI;
  EditForm:= TFEditForm(FJava.getTDIWindowType(Pathname, '%E%'));
  if assigned(EditForm) then
    Result:= EditForm.Editor.Lines.Encoding
  else if FileExists(Pathname) then
    try
      Stream:= TFileStream.Create(Pathname, fmOpenRead or fmShareDenyWrite);
      try
        Result:= SynUnicode.GetEncoding(Stream, withBOM);
      finally
        FreeAndNil(Stream);
      end;
    except
      on e: exception do
        Log('GetEncoding: ' + Pathname + '#', e);
    end;
end;

function TFConfiguration.GetFileFilters: string;
  var s: string;
begin
  if FileFilter <> ''
    then s:= ';' + FileFilter
    else s:= '';
  Result:=
    'Java-Editor|*.java;*.jfm;*.uml;*.jsg;*.html;*.htm;*.jep;*.jsp;*.jsd;*.txt;*.jar' + s +
    '|Java (*.java)|*.java|' + _('Form') + ' (*.jfm)|*.jfm|UML (*.uml)|*.uml' +
    '|' + _(LNGStructogram) + ' (*.jsg)|*.jsg' +
    '|' + _(LNGSequenceDiagram) + ' (*.jsd)|*.jsd' +
    '|HTML (*.html)|*.html;*.htm' +
    '|XML (*.xml)|*.xml|' +
    _(LNGProject) + ' (*.jep)|*.jep|' + LNGText + ' (*.txt)|*.txt' +
    '|Jar-' + _(LNGFile) + ' (*.jar)|*.jar|class(*.class)|*.class|JSP (*.jsp)|*.jsp' +
    '|' + _('Backup') + ' (*.~ava)|*.~ava|' + _('All files') + ' (*.*)|*.*';
end;

function TFConfiguration.isGerman: boolean;
begin
  Result:= (LanguageCode = 'de_DE');
end;

function TFConfiguration.getClassPath: string;
begin
  var s:= JavaClasspathAdmin;
  if JavaClasspathUser <> '' then
    s:= JavaClasspathUser + ';' + s;
  s:= FJava.ExtentClasspath(s);
  while CharInset(s[length(s)], ['\', ';']) do
    delete(s, length(s), 1);
  Result:= HideBlanks(s);
end;

function TFConfiguration.getClassPath(const Pathname, Package: string): string;
begin
  var s:= JavaClasspathAdmin;
  if JavaClasspathUser <> '' then
    s:= JavaClasspathUser + ';' + s;
  if JUnitJarFile <> '' then
    s:= JUnitJarFile + ';' + s;
  s:= getPackageDirectorySecure(Pathname, Package) + ';' + s;
  while CharInset(s[length(s)], ['\', ';']) do
    delete(s, length(s), 1);
  Result:= HideBlanks(s);
end;

function TFConfiguration.getClassPathJarExpanded(const Pathname, Package: string): string;
  var cp, cp1: string; p: integer;
      SR: TSearchRec;
begin
  Result:= getClassPath(Pathname, Package);
  if Pos('*', Result) = 0 then
    exit;
  cp:= UnhideBlanks(Result) + ';';
  Result:= '';
  p:= Pos(';', cp);
  while p > 0 do begin
    cp1:= copy(cp, 1, p-1);
    delete(cp, 1, p);
    if endsWith(cp1, '*') then begin
      delete(cp1, length(cp1), 1);
      if FindFirst(cp1 + '*.jar', 0, SR) = 0 then begin
        repeat
          Result:= Result + ';' + cp1 + SR.Name;
        until FindNext(SR) <> 0;
      end;
      FindClose(SR);
    end else
      Result:= Result + ';' + cp1;
    p:= Pos(';', cp);
  end;
  Delete(Result, 1, 1);
end;

function TFConfiguration.getPackageDirectorySecure(const Pathname: string; Package: string): string;
begin
  var Directory:= ExtractFilePath(Pathname);
  if Package <> '' then begin
    Package:= ReplaceStr(Package, '.', '\') + '\';
    if endsWith(Directory, Package)
      then delete(Directory, Length(Directory) - Length(Package), Length(Package))
  end;// else
//    Directory:= '.';
  Result:= Directory;
end;

function TFConfiguration.getPackageDirectoryRelativ(const Pathname, Package: string): string;
begin
  var Directory:= ExtractFilePath(Pathname);
  if Package <> '' then begin
    var n:= CountChar('.', Package) + 1;
    while n > 0 do begin
      Directory:= copy(Directory, 1, Length(Directory)-1);
      Directory:= copy(Directory, 1, LastDelimiter('\', Directory));
      dec(n);
    end;
  end;
  Result:= Directory;
end;

function TFConfiguration.GetHighlighter(const Pathname: string): TSynCustomHighlighter;
begin
  var FileExtension:= Lowercase(ExtractFileExt(Pathname));
  if FileExtension = '' then
    Result:= nil
  else if (FileExtension = '.java') or (FileExtension = '.~ava') then
    Result:= JavaHighlighter
  else if (FileExtension = '.html') or (FileExtension = '.htm') or (FileExtension = '.xml') then
    Result:= HTMLHighlighter
  else if FileExtension = '.pas' then
    Result:= GetPascalHighlighter
  else if FileExtension = '.jsp' then
    Result:= GetMultiSynHighlighter
  else if (FileExtension = '.php') or (FileExtension = '.inc') then
    Result:= GetPhpHighlighter
  else if FileExtension = '.css' then
    Result:= GetCSSHighlighter
  else
    Result:= GetGeneralHighlighter;
end;

function TFConfiguration.getClasspathFromSourcepath(const aClassname, aSourcepath: string): string;
  var cp, cp1: string; p: integer;
begin
  Result:= '';
  if aSourcepath = '' then exit;
  cp:= UnHideBlanks(getClassPath) + ';';
  cp:= ReplaceStr(cp, '.;', aSourcepath + ';');
  if Pos(aSourcepath, cp ) = 0 then cp:= aSourcepath + ';' + cp; 
  
  p:= Pos(';', cp);
  while p > 0 do begin
    cp1:= copy(cp, 1, p-1);
    delete(cp, 1, p);
    cp1:= withoutTrailingSlash(cp1);
    if endsWith(cp1, '*') then cp1:= copy(cp1, 1, length(cp1)-2);
    
    if Sysutils.DirectoryExists(cp1) then begin
      Result:= cp1 + '\' + aClassname + '.java';
      if FileExists(Result) then exit;
      Result:= cp1 + '\' + aClassname + '.class';
      if FileExists(Result) then exit;
    end;
    p:= Pos(';', cp);
  end;
  Result:= '';
end;

function TFConfiguration.IsInClasspath(const aClassname: string; actPath: string): boolean;
  var cp, cp1: string; p: integer;

  function InJarFile(const aClassname, JarFilename: string): boolean;
    var JarFile: TZipFile; cn: string;
  begin
    Result:= false;
    cn:= ReplaceStr(aClassname, '/', '\') + '.class';
    JarFile:= TZipFile.Create;
    try
      try
        JarFile.Open(JarFileName, zmRead);
        Result:= (JarFile.IndexOf(cn) > -1);
      finally
        FreeAndNil(JarFile);
      end;
    except
    end;
  end;

begin
  Result:= false;
  p:= Pos(';', actPath);
  if p > 0 then actPath:= copy(actPath, 1, p-1);
  cp:= UnHideBlanks(getClassPath) + ';';
  cp:= ReplaceStr(cp, '.;', actPath + ';');
  p:= Pos(';', cp);
  while (p > 0) and not Result do begin
    cp1:= copy(cp, 1, p-1);
    delete(cp, 1, p);
    if (ExtractFileExt(cp1) = '.jar') and (ExtractFilename(cp1) <> 'rt.jar') and FileExists(cp1)
      then Result:= InJarFile(ReplaceStr(aClassname, '.', '/'), cp1)
    else begin
      cp1:= withTrailingSlash(cp1) + ReplaceStr(aClassname, '.', '\');
      if FileExists(cp1 + '.java') or Fileexists(cp1 + '*.class')
        then Result:= true;
    end;
    p:= Pos(';', cp);
  end;
end;

procedure TFConfiguration.MakeClassAndPackageList(const classfile, packagefile: string);
  var SL, PL: TStringList; s: string; i, p: integer;
      JarFile: TZipFile;
begin
  if not FileExists(JDKFolder + '\jre\lib\rt.jar') then exit;

  JarFile:= TZipFile.Create;
  try
    try
      JarFile.Open(JDKFolder + '\jre\lib\rt.jar', zmRead);
      SL:= TStringList.Create;
      SL.Duplicates:= dupIgnore;
      SL.Sorted:= true;
      SL.Add('[Classes]');
      PL:= TStringList.Create;
      PL.Duplicates:= dupIgnore;
      PL.Sorted:= true;
      PL.Add('[Packages]');
      for i:= 0 to JarFile.FileCount - 1 do begin
        s:= JarFile.FileName[i];
        if Pos('.class', s) > 0 then begin
          s:= copy(s, 1, length(s) - 6);
          s:= ReplaceStr(s, '/', '.');
          p:= LastDelimiter('.', s);
          SL.Add(copy(s, p+1, length(s)) + '=' + s);
          PL.Add(copy(s, 1, p-1));
        end;
      end;
      SL.SaveToFile(classfile);
      PL.SaveToFile(packagefile);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    FreeAndNil(JarFile);
  end;
end;

procedure TFConfiguration.MakeSystemClasses;
  var s: string;

  function FromDocumentation: boolean;
  begin
    if FileExists(JavaManual) then
      if IsCHM(JavaManual)
        then Result:= CHMRootOk
        else Result:= true
    else
      Result:= false;
  end;

begin
  var SL:= TStringList.Create;
  FreeAndNil(AllClasses);
  AllClasses:= TStringList.Create;
  AllClasses.Sorted:= true;
  AllClasses.CaseSensitive:= true;
  AllClasses.Duplicates:= dupIgnore;
  FreeAndNil(AllInterfaces);
  AllInterfaces:= TStringList.create;
  AllInterfaces.Sorted:= true;
  AllInterfaces.CaseSensitive:= true;
  AllInterfaces.Duplicates:= dupIgnore;
  FreeAndNil(AllPackages);
  AllPackages:= TStringList.create;
  AllPackages.Sorted:= true;
  AllPackages.CaseSensitive:= true;
  AllPackages.Duplicates:= dupIgnore;
  FreeAndNil(AllClasspathClasses);
  AllClasspathClasses:= TStringList.Create;
  AllClasspathClasses.Sorted:= true;
  AllClasspathClasses.CaseSensitive:= true;
  AllClasspathClasses.Duplicates:= dupIgnore;

  try
    if FromDocumentation then begin
      s:= JavaCache + '\classes\' + IntToStr(getDocumentationVersion);
      if not Sysutils.ForceDirectories(s) then begin
        ErrorMsg(Format(_(LNGCanNotCreateDirectory), [s]));
        exit;
      end;
      if not (FileExists(s + '\classesd.txt') and FileExists(s + '\interfacesd.txt') and FileExists(s + '\packagesd.txt')) then
        MakeClassAndPackageListFromDocumentation(s + '\classesd.txt', s + '\interfacesd.txt', s + '\packagesd.txt');
      AllClasses.LoadFromFile(s + '\classesd.txt');
      if AllClasses.IndexOf('Class=java.lang.Class') = -1 then
        MakeClassAndPackageListFromDocumentation(s + '\classesd.txt', s + '\interfacesd.txt', s + '\packagesd.txt');
      if (FileExists(getJavaManualFX + '\allclasses-frame.html') or FileExists(getJavaManualFX + '\allclasses-index.html')) and
         (AllClasses.IndexOf('Label=javafx.scene.control.Label') = -1) then begin
        MakeClassAndPackageListFromDocumentation(s + '\classesdfx.txt', s + '\interfacesdfx.txt', s + '\packagesdfx.txt');
        SL.LoadFromFile(s + '\classesdfx.txt');
        AllClasses.AddStrings(SL);
        AllClasses.SaveToFile(s + '\classesd.txt');
        SL.Clear;
        DeleteFile(s + '\classesdfx.txt');

        AllInterfaces.LoadFromFile(s + '\interfacesd.txt');
        SL.LoadFromFile(s + '\interfacesdfx.txt');
        AllInterfaces.AddStrings(SL);
        AllInterfaces.SaveToFile(s + '\interfacesd.txt');
        SL.Clear;
        DeleteFile(s + '\interfacesdfx.txt');

        AllPackages.LoadFromFile(s + '\packagesd.txt');
        SL.LoadFromFile(s + '\packagesdfx.txt');
        AllPackages.AddStrings(SL);
        AllPackages.SaveToFile(s + '\packagesd.txt');
        SL.Clear;
        DeleteFile(s + '\packagesdfx.txt');
      end else begin
        AllInterfaces.LoadFromFile(s + '\interfacesd.txt');
        AllPackages.LoadFromFile(s + '\packagesd.txt');
      end;
    end else begin
      // from jar-file, not from d-ocumentation
      s:= JavaCache + '\classes\' + IntToStr(getJavaVersion);
      if not Sysutils.ForceDirectories(s) then begin
        ErrorMsg(Format(_(LNGCanNotCreateDirectory), [s]));
        exit;
      end;
      if not (FileExists(s + '\classes.txt') and FileExists(s + '\packages.txt')) then
        MakeClassAndPackageList(s + '\classes.txt', s + '\packages.txt');
      if FileExists(s + '\classes.txt') then
        AllClasses.LoadFromFile(s + '\classes.txt');
      if FileExists(s + '\packages.txt') then
        AllPackages.LoadFromFile(s + '\packages.txt');
    end;
    s:= JavaCache + '\classes\classpathclasses.txt';
    if not FileExists(s)
      then MakeClasspathClasses
      else AllClasspathClasses.LoadFromFile(s);
  except
    on E: Exception do
      ErrorMsg('Exception: ' + E.Message);
  end;
  FreeAndNil(SL);
end;

procedure TFConfiguration.MakeClasspathClasses;
  var cp, cp1, ext, classnam, fullclassname: string; p: integer;

  procedure CollectInJarFile(const JarFilename: string);
    var s: string; i, j: integer;
  begin
    var JarFile:= TZipFile.Create;
    try
      try
        JarFile.Open(JarFileName, zmRead);
        for i:= 0 to JarFile.FileCount - 1 do begin
          s:= JarFile.FileNames[i];
          Ext:= ExtractFileExt(s);
          if Ext = '.class' then begin
            j:= LastDelimiter('\', s);
            classnam:= ChangeFileExt(ExtractFilename(copy(s, j+1, Length(s))), '');
            if (Pos('$', classnam) = 0) then begin
              fullclassname:= ReplaceStr(ChangeFileExt(s, ''), '\', '.');
              AllClasspathClasses.add(classnam + '=' + fullclassname);
            end;
          end;
        end;
      except
      end;
    finally
      FreeAndNil(JarFile);
    end;
  end;

  procedure CollectInDirectory(const cp1, Ext: string);
    var fn, path, srn: string; SR: TSearchRec;
  begin
    path:= cp1;
    fn:= path + Ext;
    if FindFirst(fn, 0, SR) = 0 then begin
      repeat
        srn:= ChangeFileExt(SR.Name, '');
        if Pos('$', srn) = 0 then       // TODO s not initialized
          AllClasspathClasses.add(srn + '=' + srn);
      until FindNext(SR) <> 0;
    end;
    FindClose(SR);
  end;

  procedure CollectJarsInDirectory(const cp1: string);
    var SR: TSearchRec;
  begin
    if FindFirst(cp1 + '*.jar', 0, SR) = 0 then begin
      repeat
        CollectInJarFile(cp1 + SR.Name);
      until FindNext(SR) <> 0;
    end;
    FindClose(SR);
  end;

begin
  cp:= JavaClasspathAdmin + ';';
  if JavaClasspathUser <> ''
    then cp:= JavaClasspathUser + ';' + cp;
  p:= Pos(';', cp);
  while p > 0 do begin
    cp1:= copy(cp, 1, p-1);
    delete(cp, 1, p);
    if endsWith(cp1, '*') then begin
      delete(cp1, length(cp1), 1);
      CollectJarsInDirectory(cp1);
    end
    else if (ExtractFileExt(cp1) = '.jar') and (ExtractFilename(cp1) <> 'rt.jar') and FileExists(cp1)
      then CollectInJarFile(cp1)
    else if cp1 <> '.' then begin
      cp1:= withTrailingSlash(cp1);
      CollectInDirectory(cp1, '*.class');
      CollectInDirectory(cp1, '*.java');
    end;
    p:= Pos(';', cp);
  end;
  try
    AllClasspathClasses.SaveToFile(JavaCache + '\classes\classpathclasses.txt');
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
end;

procedure TFConfiguration.MakeClassAndPackageListFromDocumentation(const classfile, interfacefile, packagefile: string);
  var SL, CL, IL, PL: TStringList;
      s, aClassname: string; i, p: integer;
      aIsInterface, aIsClass, fx, withModule: boolean;
begin
  try
    CL:= TStringList.Create;
    CL.Duplicates:= dupIgnore;
    CL.Sorted:= true;
    IL:= TStringList.Create;
    IL.Duplicates:= dupIgnore;
    IL.Sorted:= true;
    PL:= TStringList.Create;
    PL.Duplicates:= dupIgnore;
    PL.Sorted:= true;
    fx:= Pos('classesdfx', classfile) > 0;
    withModule:= false;
    if fx
      then s:= getJavaManualFX + '\allclasses-frame.html'
      else s:= getJavaManual + '\allclasses-frame.html';
    if not IsChm(s) and not FileExists(s) then begin
      withModule:= true;
      if fx
        then s:= getJavaManualFX + '\allclasses-index.html'
        else s:= getJavaManual + '\allclasses-index.html';
    end;

    SL:= TStringList.Create;
    if IsCHM(s) then begin
      if GlobalFileExists(s) then
        SL.Text:= LoadFromCHM(s);
    end else
      SL.Text:= myCodeCompletion.LoadTextFromFile(s);
    for i:= 0 to SL.Count - 1 do begin
      s:= SL.Strings[i];
      p:= Pos('.html', s);
      if p > 0 then begin
        if Pos('enum in ', s) > 0 then continue;
        aIsInterface:= Pos('interface in ', s) > 0;
        aIsClass:= Pos('class in ', s) > 0;
        if not (aIsClass or aIsInterface) then
          continue;
        delete(s, p, length(s));
        p:= length(s);
        while p > 0 do begin
          if s[p] = '/' then begin
            aClassname:= copy(s, p+1, length(s));
            break;
          end
          else dec(p);
        end;
        while p > 0 do begin
          if s[p] = '"'
            then break
            else dec(p);
        end;
        if p > 0 then begin
          delete(s, 1, p);
          p:= Pos('/api', s);
          if (p > 0) and not withModule then
            delete(s, 1, p + 4);
          if withModule then begin
            p:= Pos('/', s);
            delete(s, 1, p);
          end;
          s:= ReplaceStr(s, '/', '.');
          if aIsInterface
            then IL.Add(aClassname + '=' + s)
            else CL.Add(aClassname + '=' + s);
          p:= LastDelimiter('.', s);
          if p > 0 then
            s:= copy(s, 1, p-1);
          PL.Add(s)
        end;
      end;
    end;
    CL.SaveToFile(classfile);
    IL.SaveToFile(interfacefile);
    PL.SaveToFile(packagefile);
    FreeAndNil(SL);
    FreeAndNil(CL);
    FreeAndNil(PL);
    FreeAndNil(IL);
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
end;

function TFConfiguration.ToStringListClass(const aClassname: string): string;
begin
  var p:= LastDelimiter('.', aClassname);
  if p > 0
    then Result:= copy(aClassname, p+1, length(aClassname)) + '=' + aClassname
    else Result:= aClassname;
end;

function TFConfiguration.IsAPIInterface(var cn: string): boolean;
begin
  Result:= false;
  if assigned(AllInterfaces) and (AllInterfaces.Count > 0) then begin
    var s:= ToStringListClass(cn);
    var i:= AllInterfaces.IndexOf(s);
    if i > -1 then begin
      cn:= AllInterfaces.ValueFromIndex[i];
      Result:= true;
    end else begin
      i:= AllInterfaces.IndexOfName(cn);
      if i > -1 then begin
        cn:= AllInterfaces.ValueFromIndex[i];
        Result:= true;
      end;
    end;
  end;
end;

function TFConfiguration.IsAPIClass(var cn: string): boolean;
  var i: integer; s: string;
begin
  Result:= false;
  if assigned(AllClasses) and (AllClasses.Count > 0) then begin
    s:= ToStringListClass(cn);
    i:= AllClasses.IndexOf(s);
    if i > -1 then
      Result:= true
    else begin
      i:= AllClasses.IndexOfName(cn);
      if i > -1 then begin
        cn:= AllClasses.ValueFromIndex[i];
        Result:= true;
      end else begin
        i:= AllClasspathClasses.IndexOfName(cn);
        if i > -1 then begin
          cn:= AllClasspathClasses.ValueFromIndex[i];
          Result:= true;
        end;
      end;
    end;
  end;
end;

function TFConfiguration.IsAPIPackage(pn: string): boolean;
begin
  Result:= false;
  if length(pn) > 0 then begin
    if pn[length(pn)] = '.' then delete(pn, length(pn), 1);
    if assigned(AllPackages) and (AllPackages.Count > 0) then
      Result:= (AllPackages.IndexOf(pn) > -1);
  end;
end;

function TFConfiguration.IsAPIClassOrInterface(aClassname: string): boolean;
begin
  aClassname:= WithoutGeneric(aClassname);
  Result:= IsAPIClass(aClassname) or IsAPIInterface(aClassname);
end;

procedure TFConfiguration.SetElevationRequiredState(aControl: TWinControl);
  const BCM_FIRST = $1600;
        BCM_SETSHIELD = BCM_FIRST + $000C;
begin
  SendMessage(aControl.Handle, BCM_SETSHIELD, 0, LPARAM(true));
end;

function TFConfiguration.GetJarsFromClasspath: string;
  var p: integer; cp, s1, jar: string;

  procedure CollectJarsInDirectory(const cp1: string);
    var SR: TSearchRec;
  begin
    if FindFirst(cp1 + '*.jar', 0, SR) = 0 then begin
      repeat
        cp:= cp1 + SR.Name + ';' + cp;
      until FindNext(SR) <> 0;
    end;
    FindClose(SR);
  end;

begin
  jar:= '';
  cp:= JavaClassPathUser + ';' + JavaClasspathAdmin + ';';
  p:= Pos(';', cp);
  while p > 0 do begin
    s1:= copy(cp, 1, p-1);
    delete(cp, 1, p);
    if endsWith(s1, '*') then
      CollectJarsInDirectory(s1)
    else if LowerCase(ExtractFileExt(s1)) = '.jar' then
      jar:= jar + ',file:\\\' + s1;
    p:= Pos(';', cp);
  end;
  Result:= jar;
end;

function TFConfiguration.UpdatePossible(const Source, Target: string): boolean;
begin
  if not SysUtils.DirectoryExists(Source) then SysUtils.ForceDirectories(Source);
  Result:= (Win32Platform = VER_PLATFORM_WIN32_NT) and IsAdministrator or
            VistaOrBetter;
  SysUtils.ForceDirectories(Target);
  if not Result then
    Result:= Sysutils.DirectoryExists(Target) and HasWriteAccess(Target);
end;

function TFConfiguration.RunAsAdmin(hWnd: HWND; const aFile, aParameters: string): THandle;
  var sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize:= SizeOf(sei);
  sei.Wnd:= hWnd;
  sei.fMask:= SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  // left side is pAnsiChar
  sei.lpVerb:= 'runas';
  sei.lpFile := pAnsiChar(AnsiString(aFile));
  sei.lpParameters := pAnsiChar(AnsiString(aParameters));
  sei.nShow:= SW_ShowNormal;
  if ShellExecuteExA(@sei) then
    Result:= 33
  else begin
    ErrorMsg(SysErrorMessage(GetLastError));
    Result:= 0;
  end;
end;

function TFConfiguration.GetFrameType(JavaProgramm: string; Startclass: boolean = false): integer;
  var aForm: TFEditForm; JavaScanner: TJavaScanner;
begin
  Result:= 1;
  if ExtractFileExt(JavaProgramm) <> '.java' then Exit;
  var s:= '';
  if Startclass and (JavaStartClass <> '') and FileExists(JavaStartClass) then
    JavaProgramm:= JavaStartClass;

  aForm:= TFEditForm(FJava.getTDIWindowType(JavaProgramm, '%E%'));
  if assigned(aForm) then
    s:= aForm.Editor.Text
  else if FileExists(JavaProgramm) then begin
    var SL:= TStringList.Create;
    try
      try
        SL.LoadFromFile(JavaProgramm);
        s:= SL.text;
      except
      end;
    finally
      FreeAndNil(SL);
    end;
  end;
  if s <> '' then begin
    JavaScanner:= TJavaScanner.create;
    try
      JavaScanner.Init(s);
      Result:= JavaScanner.GetFrameType;
    finally
      JavaScanner.Destroy;
    end;
  end;
end;

function TFConfiguration.getPackage(JavaProgram: string; Startclass: boolean = false): string;
  var s: string;
      aForm: TFEditForm; JavaScanner: TJavaScanner;
begin
  Result:= '';
  if ExtractFileExt(JavaProgram) <> '.java' then exit;
  if Startclass and (JavaStartClass <> '') and
    FileExists(JavaStartClass) then
    JavaProgram:= JavaStartClass;
  aForm:= TFEditForm(FJava.getTDIWindowType(JavaProgram, '%E%'));
  if assigned(aForm) then
    s:= aForm.Editor.Text
  else if FileExists(JavaProgram) then begin
    var SL:= TStringList.Create;
    try
      try
        SL.LoadFromFile(JavaProgram);
        s:= SL.text;
      except
      end;
    finally
      FreeAndNil(SL);
    end;
  end;
  if s <> '' then begin
    JavaScanner:= TJavaScanner.create;
    try
      JavaScanner.Init(s);
      Result:= JavaScanner.GetPackage(s);
    finally
      JavaScanner.Destroy;
    end;
  end;
end;

function TFConfiguration.getCompilerCall(const Pathname, Package, Encoding: string): string;
  var s: string;
begin
  if MindstormsMode then
    case MindstormsVersion of
      0: s:= '';
      1: s:= '';
    else
      s:= MindstormsParameter + ' -classpath ' +
          HideBlanks(LejosVerzeichnis + '\lib\ev3\ev3classes.jar;' +
                     UnHideBlanks(getClassPath(Pathname, Package)))
    end
  else begin
    s:= ' -classpath ' + getClassPath(Pathname, Package) + ' ' + getJavaCompilerParameter(Pathname);
    if CompilerEncoding <> '' then
      s:= s + ' -encoding ' + CompilerEncoding
    else if Encoding <> 'ANSI' then
      s:= s + ' -encoding ' + Encoding;
  end;
  Result:= s;
end;

function TFConfiguration.getJavaCompilerParameter(const Pathname: string): string;
begin
  Result:= JavaCompilerParameter;
  var s:= '';
  var NeedsJavaFX:= false;
  if FileExists(Pathname) then begin
    var SL:= TStringList.Create;
    try
      try
        SL.LoadFromFile(Pathname);
        s:= SL.text;
      except
      end;
    finally
      FreeAndNil(SL);
    end;
  end;
  if s <> '' then begin
    var JavaScanner:= TJavaScanner.create;
    NeedsJavaFX:= JavaScanner.NeedsJavaFX(s);
    JavaScanner.Destroy;
  end;
  if NeedsJavaFX and (JavaVersion >= 9) and (JavaFXFolder <> '') then
    Result:= '--module-path ' + HideBlanks(JavaFXFolder) +
             '\lib --add-modules=' + JavaFXParameter + ' ' + Result;
end;

function TFConfiguration.getJavaInterpreterParameter(FX: boolean): string;
begin
  Result:= JavaInterpreterParameter;
  if FX and (JavaVersion >= 11) and (JavaFXFolder <> '') then
    Result:= Result + ' --module-path ' + HideBlanks(JavaFXFolder) +
                      '\lib --add-modules=' + JavaFXParameter;
end;

procedure TFConfiguration.SetPrinter(const s: string);
begin
  if s <> '' then begin
    var i:= Printer.Printers.IndexOf(s);
    if (-1 < i) and (i < Printer.Printers.Count) then
      SetPrinterIndex(i)
  end;
end;


function TFConfiguration.PathForSystemClass(const Path: string): boolean;
begin
  Result:= (Pos(JavaCache, Path) = 1) and FileExists(Path);
end;

function TFConfiguration.PathForUserClass(const Path: string): boolean;
begin
  Result:= (Pos(JavaCache, Path) = 0) and FileExists(Path);
end;

function TFConfiguration.getAppletArchiv: string;
begin
  var archive:= GetJarsFromClasspath;
  if archive <> '' then
    archive:= 'archive="' + copy(archive, 2, length(archive)) + '" ';
  Result:= archive;
end;

function TFConfiguration.HasAgeClass(const Pathname: string): Boolean;
  var FDT1, FDT2: TDateTime;
begin
  var ClassDatei:= getClass(getClasspath, JavaCompilerParameter, Pathname);
  if (ClassDatei <> '') and FileAge(ClassDatei, FDT1) and FileAge(Pathname, FDT2)
    then Result:= (FDT1 >= FDT2)
    else Result:= false;
end;

function TFConfiguration.HasClass(const Pathname: string): Boolean;
begin
  var ClassDatei:= getClass(getClasspath, JavaCompilerParameter, Pathname);
  Result:= (ClassDatei <> '')
end;

procedure TFConfiguration.SaveConfigurationForProject;
begin
  SavedClasspathUser:= JavaClasspathUser;
  SavedInterpreterParameter:= JavaInterpreterParameter;
  SavedCompilerParameter:= JavaCompilerParameter;
  SavedJavaStartClass:= JavaStartClass;
end;

procedure TFConfiguration.RestoreConfigurationAfterProject;
begin
  JavaClasspathUser:= SavedClasspathUser;
  JavaInterpreterParameter:= SavedInterpreterParameter;
  JavaCompilerParameter:= SavedCompilerParameter;
  JavaStartClass:= SavedJavaStartClass;
end;

procedure TFConfiguration.MakeControlStructureTemplates;
  const
    ControlStructure: array[1..10] of string =
      ('#if', '#while', '#for', '#do', '#switch', '#try', 'else', '} else', '#ifelse', '{ }');
  var i, p: integer; s: string; SL: TStringList;
begin
  for i:= 1 to 21 do begin
    FreeAndNil(ControlStructureTemplates[i]);
    ControlStructureTemplates[i]:= TStringList.Create;
  end;
  ControlStructureTemplates[1].Text:= 'if (|) {' + CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[2].Text:= 'while (|) { ' +  CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[3].Text:= 'for (|; ; ) {' +  CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[4].Text:= 'do {' + CrLf + Indent1 + '|' +  CrLf + '} while ();';
  ControlStructureTemplates[5].Text:=
         'switch (|) {' + CrLf +
            Indent1 + 'case  : ' + CrLf + Indent2 + CrLf + Indent2 + 'break;' + CrLf +
            Indent1 + 'case  : ' + CrLf + Indent2 + CrLf + Indent2 + 'break;' + CrLf +
            Indent1 + 'default: ' + CrLf + Indent2 + CrLf +
            '}';
  ControlStructureTemplates[6].Text:=
         'try {' + CrLf +
            Indent1 + '|' + CrLf +
         '} catch(Exception e) {' + CrLf  +
            Indent1 + CrLf +
         '} finally {' + CrLf +
            Indent1 + CrLf +
         '}';
  ControlStructureTemplates[7].Text:=  'else {' + CrLf + Indent1 + '|' + CrLf + '}';
  ControlStructureTemplates[8].Text:=  '} else {' + CrLf + Indent1 + '|' + CrLf + '}';
  ControlStructureTemplates[9].Text:=  'if (|) {' + CrLf + Indent1 + CrLf + '} else {' + CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[10].Text:= '{' + CrLf + Indent1 + '|' + CrLf + '}';

  ControlStructureTemplates[11].Text:= 'else if (|) {' + CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[12].Text:= 'for (int i = 0; i < |10; i++) {' +  CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[13].Text:= 'catch (Exception e) {' +  CrLf + Indent1 + '|' + CrLf + '}';
  ControlStructureTemplates[14].Text:= 'finally {' +  CrLf + Indent1 + '|' + CrLf + '}';
  ControlStructureTemplates[15].Text:= 'private |void name() {' + CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[16].Text:= 'public |void name() {' + CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[17].Text:= 'System.out.println(|); ' + CrLf;
  ControlStructureTemplates[18].Text:= '|type name = new type();' + CrLf;
  ControlStructureTemplates[19].Text:= 'public static void main(String[] args) {' + CrLf + Indent1 + '|' + CrLf + '}';
  ControlStructureTemplates[20].Text:= 'private static |void name() {' + CrLf + Indent1 + CrLf + '}';
  ControlStructureTemplates[21].Text:= 'public static |void name() {' + CrLf + Indent1 + CrLf + '}';

  if not FileExists(Templates[9]) then Exit;
  SL:= TStringList.Create;
  try
    try
      SL.LoadFromFile(Templates[9]);
      for i:= 1 to 10 do begin
        p:= SL.IndexOf(ControlStructure[i]);
        if p < 0 then Continue;
        ControlStructureTemplates[i].Text:= '';
        inc(p);
        while (p < SL.Count) and (Copy(SL.Strings[p], 1, 1) <> '#') do begin
          ControlStructureTemplates[i].Add(SL.Strings[p]);
          inc(p);
        end;
      end;

      SL.Text:= ControlStructureTemplates[9].Text;
      while (SL.Count > 0) and (Pos('else', SL.Strings[0]) = 0) do
        SL.Delete(0);
      p:= 0;
      while (SL.Count > 0) and (trim(SL.Strings[p]) <> '') do
        inc(p);
      if p < SL.Count then
         SL.Strings[p]:= StringOfChar(' ', Indent) + '|';
      ControlStructureTemplates[8].Text:= SL.Text;
      s:= SL.Text;
      p:= Pos('else', s);
      if p > 0 then Delete(s, 1, p-1);
      ControlStructureTemplates[7].Text:= s;
    except
      on e: exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TFConfiguration.ShortenPath(WinControl: TWinControl; const s: string);
  var s1, s2, s3: string; p, w: integer;
begin
  WinControl.Hint:= s;
  WinControl.ShowHint:= true;
  if WinControl is TEdit
    then w:= WinControl.Width
    else w:= WinControl.Width - 16;
  s1:= GlobalMinimizeName(s, Canvas, w);
  if WinControl is TEdit
    then (WinControl as TEdit).Text:= s1
    else (WinControl as TComboBox).Text:= s1;
  p:= Pos('...', s1);
  if p > 0 then begin
    s2:= s;
    delete(s2, 1, p-1);
    s3:= copy(s1, p+3, length(s1));
    p:= Pos(s3, s2);
    delete(s2, p, length(s1));
    WinControl.HelpKeyword:= s2;
  end else
    WinControl.HelpKeyword:= '';
end;

function TFConfiguration.ExtendPath(WinControl: TWinControl): string;
  var s: string; p: integer;
begin
  if WinControl is TEdit
    then s:= (WinControl as TEdit).Text
    else s:= (WinControl as TComboBox).Text;
  p:= Pos('...', s);
  if p > 0 then begin
    delete(s, p, 3);
    insert(WinControl.HelpKeyword, s, p);
  end;
  Result:= s;
end;

procedure TFConfiguration.CheckFile(WinControl: TWinControl; emptyAllowed: boolean);
  var s: string; E: TEdit;
begin
  s:= ExtendPath(WinControl);
  ShortenPath(WinControl, s);
  if WinControl is TEdit then begin
    E:= (WinControl as TEdit);
    E.Color:= getCheckColor(s, emptyAllowed);
  end else
    (WinControl as TComboBox).Color:= getCheckColor(s, emptyAllowed);
  WinControl.Enabled:= not LockedPaths;
end;

procedure TFConfiguration.CheckFileWithoutShortenpath(WinControl: TWinControl; emptyAllowed: boolean);
begin
  if WinControl is TEdit
    then (WinControl as TEdit).Color:= getCheckColor((WinControl as TEdit).text, emptyAllowed)
    else (WinControl as TComboBox).Color:= getCheckColor((WinControl as TComboBox).text, emptyAllowed);
  WinControl.Enabled:= not LockedPaths;
end;

procedure TFConfiguration.ComboBoxAddEx(ComboBox: TComboBox);
begin
  ComboBox.Text:= ExtendPath(ComboBox);
  ComboBoxAdd(ComboBox);
  ShortenPath(ComboBox, ComboBox.Text);
end;

{$WARN SYMBOL_PLATFORM OFF}
function TFConfiguration.SearchClassInClasspath(const aClassname, aSourcepath, Package: string): string;
  var cp, cp1: string; p: integer;

  function InJarFile(const JarFilename, aClassname: string): string;
    var JarFile: TZipFile;
        cn, aFilename: string; i: integer;
  begin
    Result:= '';
    cn:= aClassname + '.class';
    JarFile:= TZipFile.Create;
    try
      try
        JarFile.Open(JarFilename, zmRead);
        for i:= 0 to JarFile.FileCount - 1 do begin
          aFilename:= JarFile.FileNames[i];
          if (aFilename = cn) or endswith(aFilename, '\' + cn) then begin
            Result:= JavaCache + '\' + aFilename;
            if not FileExists(Result) then begin
              Sysutils.ForceDirectories(ExtractFilePath(Result));
              JarFile.Extract(i, Result);
            end;
            break;
          end;
        end;
      except
      end;
    finally
      FreeAndNil(JarFile);
    end;
  end;

  function SearchFile(const Dir, Fname: string): string;
    var SR: TSearchRec;
  begin
    Result:= '';
    if FileExists(Dir + Fname) then
      Result:= Dir + Fname
    else begin
      if FindFirst(Dir + '*' + Fname, faDirectory, SR) = 0 then
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Attr AND faDirectory = faDirectory) then
            Result:= SearchFile(Dir + SR.Name + '\', Fname);
        until (FindNext(SR) <> 0) or (Result <> '');
      FindClose(SR);
    end;
  end;

  function InDirectory(Directory: string; const aClassname: string): string;
  begin
    Directory:= IncludeTrailingPathDelimiter(Directory);
    Result:= SearchFile(Directory, aClassname + '.java');
    if Result = '' then
      Result:= SearchFile(Directory, aClassname + '.class');
  end;

begin
  Result:= '';
  cp:= UnHideBlanks(getClassPathJarExpanded(aSourcepath, Package));
  p:= Pos(';', cp);
  while (p > 0) and (Result = '') do begin
    cp1:= copy(cp, 1, p-1);
    delete(cp, 1, p);
    if (ExtractFileExt(cp1) = '.jar') and (ExtractFilename(cp1) <> 'rt.jar') and FileExists(cp1) then
      Result:= InJarFile(cp1, aClassname)
    else begin
      if Package <> '' then
        cp1:= cp1 + ReplaceStr(Package, '.', '\') + '\';
      Result:= InDirectory(cp1, aClassname);
    end;
    p:= Pos(';', cp);
  end;
end;

function TFConfiguration.SearchClassInDirectory(const aClassname, aSourcepath, Package: string): string;
begin
  Result:= '';
  var cp:= getPackageDirectoryRelativ(aSourcepath, Package);
  if Package <> '' then
    cp:= cp + ReplaceStr(Package, '.', '\') + '\';
  cp:= cp + aClassname + '.java';
  if FileExists(cp)
    then Result:= cp
    else Result:= '';
end;

{$WARN SYMBOL_PLATFORM OFF}

function TFConfiguration.IsInterface(const pathname: string): boolean;
  var JavaScanner: TJavaScanner;
begin
  Result:= false;
  if not FileExists(pathname) then exit;
  JavaScanner:= TJavaScanner.Create;
  var SL:= TStringList.Create;
  try
    SL.LoadFromFile(pathname);
    Result:= JavaScanner.isInterface(SL.Text);
  finally
    FreeAndNil(JavaScanner);
    FreeAndNil(SL);
  end;
end;

function TFConfiguration.getCompleteClassname(FullImports, Classimports, UserImportClasses: TStringList;
          const aClassname: string): string;
  var k, i, p: integer; s, cname: string;
begin
  Result:= '';

  Cname:= aClassname;
  p:= Pos('<', cname);
  if p > 0 then
    cname:= Copy(cname, 1, p-1);
  p:= Pos('[', cname);
  if p > 0 then
    cname:= Copy(cname, 1, p-1);

  // 1. look in cache
  try
    // >>> TFConfiguration.getCompleteClassname Classname= System # exe=0
    p:= ImportCache.IndexOfName(cname);
    if (p > -1) and (p < ImportCache.Count) then begin
      Result:= ImportCache.ValueFromIndex[p];
      exit;
    end;
  except on e : Exception do
     Log('TFConfiguration.getCompleteClassname Classname= ' + cname, e);
  end;

  // 2. look in ClassImports
  k:= ClassImports.IndexOfName(cname);
  if (k > -1) and (k < ClassImports.Count) then begin
    Result:= ClassImports.ValueFromIndex[k] + '.' + aClassname;
    //ImportCache.Add(cname + '=' + Result);
    exit;
  end;

  // 3. look in FullImports
  for k:= 0 to FullImports.Count - 1 do begin
    s:= FullImports.Strings[k] + cname;
    if IsAPIPackage(FullImports.Strings[k]) then begin
      i:= AllClasses.IndexOf(cname + '=' + s);
      if i > -1 then begin
        Result:= AllClasses.ValueFromIndex[i];
        //ImportCache.Add(cname + '=' + Result);
        exit;
      end;
      i:= AllInterfaces.IndexOf(cname + '=' + s);
      if i > -1 then begin
        Result:= AllInterfaces.ValueFromIndex[i];
        //ImportCache.Add(cname + '=' +Result);
        exit;
      end;
    end else begin
      i:= UserImportClasses.IndexOf(cname + '=' + s);
      if i > -1 then begin
        Result:= UserImportClasses.ValueFromIndex[i];
        //ImportCache.Add(cname + '=' + Result);
        exit;
      end;
      i:= AllClasspathClasses.IndexOf(cname + '=' + s);
      if i > -1 then begin
        Result:= AllClasspathClasses.ValueFromIndex[i];
        //ImportCache.Add(cname + '=' + Result);
        exit;
      end;
    end;
  end;
end;

procedure TFConfiguration.CollectDocumentations;
  var aDocumentation, DocsFolder: string; p: integer;
begin
  AllDocumentations.Clear;
  aDocumentation:= EditorFolder + 'docs\allclasses-frame.html';
  if GlobalFileExists(aDocumentation) then
    AllDocumentations.Add(aDocumentation);
  DocsFolder:= JavaJavaDocs + ';';
  repeat
    p:= Pos(';', DocsFolder);
    aDocumentation:= Copy(DocsFolder, 1, p-1);
    Delete(DocsFolder, 1, p);
    if (aDocumentation <> '') and GlobalFileExists(aDocumentation) then
      AllDocumentations.Add(aDocumentation);
  until DocsFolder = '';
  aDocumentation:= getJavaManual + '\allclasses-frame.html';
  if GlobalFileExists(aDocumentation) then
    AllDocumentations.Add(aDocumentation)
  else begin
    aDocumentation:= getJavaManual + '\allclasses-index.html';
    if GlobalFileExists(aDocumentation) then
      AllDocumentations.Add(aDocumentation)
  end;
  aDocumentation:= getJavaManualFX + '\allclasses-frame.html';
  if GlobalFileExists(aDocumentation) then
    AllDocumentations.Add(aDocumentation)
  else begin
    aDocumentation:= getJavaManualFX + '\allclasses-index.html';
    if GlobalFileExists(aDocumentation) then
      AllDocumentations.Add(aDocumentation)
  end;
  if MindstormsMode then begin
    aDocumentation:= ExtractFilePathEx(MindstormsManual) + '\allclasses-frame.html';
    if GlobalFileExists(aDocumentation) then
      AllDocumentations.Add(aDocumentation);
  end;
  if JUnitOk then begin
    aDocumentation:= ExtractFilePathEx(JUnitManual) + '\allclasses-frame.html';
    if GlobalFileExists(aDocumentation) then
      AllDocumentations.Add(aDocumentation);
  end;
end;

function TFConfiguration.getJavaTools(const Tool: string): string;
begin
  Result:= JavaTools + Tool;
  if getDocumentationVersion > 8 then
    Delete(Result, length(Result), 1);
end;

class function TFConfiguration.isDark: boolean;
  var BGColor, FGColor: TColor;
begin
  if StyleServices.IsSystemStyle then begin
    BGColor:= clWhite;
    FGColor:= clBlack;
  end else begin
    BGColor:= StyleServices.GetStyleColor(scPanel);
    FGColor:= StyleServices.GetStyleFontColor(sfTabTextInactiveNormal);
  end;
  if GetRValue(BGColor) + GetGValue(BGColor) + GetBValue(BGColor) >
     GetRValue(FGColor) + GetGValue(FGColor) + GetBValue(FGColor)
    then Result:= false
    else Result:= true;
end;

// style
// https://stackoverflow.com/questions/9130945/delphi-xe2-disabling-vcl-style-on-a-component
// https://theroadtodelphi.com/2012/02/06/changing-the-color-of-edit-controls-with-vcl-styles-enabled/

class procedure TFConfiguration.LoadGUIStyle(Style: string);
var
  searchResults : TSearchRec;
  SearchDir, Filename: string;
  StyleInfo: TStyleInfo;
begin
  if not TStyleManager.TrySetStyle(Style, false) then begin
    SearchDir := ExtractFilePath(ParamStr(0)) + 'styles' + PathDelim;
    if DirectoryExists(SearchDir) then begin
      if FindFirst(SearchDir + '*.*', faAnyFile - faDirectory, searchResults) = 0 then
        repeat
          Filename:= SearchDir + searchResults.Name;
          if TStyleManager.IsValidStyle(Filename, StyleInfo) and (StyleInfo.Name = Style) then begin
            TStyleManager.LoadFromFile(Filename);
            TStyleManager.TrySetStyle(Style, false);
            break;
          end;
        until (FindNext(searchResults) <> 0);
      FindClose(searchResults);
    end;
  end;
end;

class procedure TFConfiguration.setGUIStyle;
  var s, EditorFolder, PortAppDrive, Style: string;
      myReg: TRegistry;
      MachineIniFile: TMemIniFile;
      UserIniFile: TIniFile;
begin
  Style:= 'Windows';
  EditorFolder:= ExtractFilePath(ParamStr(0));
  ChDir(EditorFolder);
  s:= ParamStr(1);
  if (s = '') or (UpperCase(ExtractFileExt(s)) <> '.INI')
    then s:= EditorFolder + 'JEMachine.ini'
    else s:= ExpandUNCFileName(s);

  PortAppDrive:= ExtractFileDrive(s);   // with UNC we get \\Server\Freigabe
  if Pos(':', PortAppDrive) > 0 then
    PortAppDrive:= copy(PortAppDrive, 1, 1);

  if not FileExists(s) then begin
    MyReg:= TRegistry.Create;
    with MyReg do begin
      RootKey:= HKEY_CURRENT_USER;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\Colors', false);
        if ValueExists('GUIStyle')
          then Style:= ReadString('GUIStyle')
      finally
        CloseKey;
      end;
    end;
    FreeAndNil(MyReg);
  end else begin
    try
      MachineIniFile:= TMemIniFile.Create(s);
      s:= MachineIniFile.ReadString('User', 'HomeDir', '<nix>');
      if s = '<nix>' then exit;
      s:= dissolveUsername(s);
      if copy(s, 1, 2) = ':\' then s:= PortAppDrive + s;
      s:= withTrailingSlash(s);
      s:= s + 'JEUser.ini';
      UserIniFile:= TIniFile.Create(s);
      if Assigned(UserIniFile) then
        if Assigned(MachineIniFile) and MachineIniFile.ValueExists('Colors', 'GUIStyle')
          then Style:= MachineIniFile.ReadString('Colors', 'GUIStyle', 'Windows')
          else Style:= UserIniFile.ReadString('Colors', 'GUIStyle', 'Windows');
      FreeAndNil(MachineIniFile);
      FreeAndNil(UserIniFile);
    except
    end;
  end;
  if Style <> 'Windows' then
    LoadGUIStyle(Style);
end;

procedure TFConfiguration.ReadEditorStyleNames;
  var SL, EditorStyles: TStringList;
      Path, s: string;
      IniFile: TMemIniFile; i, p: integer;
begin
  Path:= EditorFolder + 'styles' + PathDelim;
  IniFile:= TMemIniFile.Create(Path + 'DefaultColorsJava.ini');
  EditorStyles:= TStringList.Create;
  EditorStyles.Sorted:= true;
  EditorStyles.Duplicates:= TDuplicates.dupIgnore;
  SL:= TStringList.Create;
  try
    IniFile.ReadSections(SL);
    for i:= 0 to SL.Count - 1 do begin
      s:= SL.Strings[i];
      p:= Pos('\', s);
      if p > 0 then begin
        delete(s, p, length(s));
        EditorStyles.Add(s)
      end;
    end;
    EditorStyles.Sorted:= false;
    EditorStyles.Insert(0, 'Default');
    CBEditorStyles.Items.Assign(EditorStyles);
  finally
    FreeAndNil(IniFile);
    FreeandNil(SL);
    FreeAndNil(EditorStyles);
  end;
end;

function TFConfiguration.ExplorerTest: boolean;
begin
  try
    var Web:= TWebBrowser.Create(Self);
    try
      Result:= Assigned(Web);
    finally
      FreeAndNil(Web);
    end;
  except
    Result:= false;
  end;
end;

procedure TFConfiguration.OpenAndShowPage(Page: string);
begin
  // due to visible/modal-exception
  if not Visible then begin
    PrepareShow;
    TThread.ForceQueue(nil, procedure
      begin
        TVConfiguration.Selected:= getTVConfigurationItem(_(Page));
      end);
    ShowModal;
  end;
end;

procedure TFConfiguration.StyleSelectorShow;
begin
  if LBStyleNames.Items.Count = 0 then
    FillVclStylesList;
  if LBStyleNames.Items.Count> 0 then begin
     var Index := LBStyleNames.Items.IndexOf(TStyleManager.ActiveStyle.Name);
     if Index >= 0 then
       LBStyleNames.Selected[Index] :=  True
     else
       LBStyleNames.Selected[0] :=  True;
   end;
   LBStyleNamesClick(Self);
end;

procedure TFConfiguration.FillVclStylesList;
var
  FileName : string;
  StyleInfo:  TStyleInfo;
begin
  // First add resource styles
  LBStyleNames.Items.AddStrings(TStyleManager.StyleNames);
  try
    for FileName in TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) + 'styles', '*.vsf') do
      if TStyleManager.IsValidStyle(FileName, StyleInfo) and
         (LBStyleNames.Items.IndexOf(StyleInfo.Name) < 0)
      then begin
        LBStyleNames.Items.AddObject(StyleInfo.Name, TObject(1));
        ExternalStyleFilesDict.Add(StyleInfo.Name, FileName);
      end;
  except
  end;
end;

type
 TVclStylesPreviewClass = class(TVclStylesPreview);

procedure TFConfiguration.LBStyleNamesClick(Sender: TObject);
var
  LStyle : TCustomStyleServices;
  FileName : string;
  StyleName : string;
begin
  LStyle:=nil;
  if LBStyleNames.ItemIndex >= 0 then begin
    StyleName := LBStyleNames.Items[LBStyleNames.ItemIndex];
    if Integer(LBStyleNames.Items.Objects[LbStylenames.ItemIndex]) = 1 then begin
      FileName := ExternalStyleFilesDict.Items[StyleName];
      TStyleManager.LoadFromFile(FileName);
      LStyle := TStyleManager.Style[StyleName];
      LoadedStylesDict.Add(StyleName, FileName);
      //  The Style is now loaded and registerd
      LBStyleNames.Items.Objects[LbStylenames.ItemIndex] := nil;
    end else
      // Resource style
      LStyle := TStyleManager.Style[StyleName];
  end;

  if Assigned(LStyle) then begin
    FPreview.Caption:= StyleName;
    FPreview.Style:= LStyle;
    TVclStylesPreviewClass(FPreview).Paint;
  end;
end;

procedure TFConfiguration.Log(const s: string; E: Exception = nil);
  var F: TextFile; datetime: string;
begin
  if FConfiguration.LogfileExceptionsOK then begin
    AssignFile(F, FConfiguration.LogfileExceptions);
    try
      Append(F);
      DateTimeTostring(datetime, 'ddddd h:n:s.zzz', Now());
      Writeln(F, datetime + ' ' + GetComputerNetName + ' Version: ' + UDlgAbout.Version);
      if assigned(E) then begin
        Writeln(F, '>>> Log silent Exception: ' + s);
        Writeln(F, 'E.Message: ' + E.Message);
      end else
        Writeln(F, '>>> Log silent: ' + s);
      CloseFile(F);
    except
      on e: exception do
        ErrorMsg(e.Message);
    end;
  end;
end;

procedure TFConfiguration.SetupLanguages;
Var
  i: integer;
  English, Language: string;
  HaveLang: boolean;
begin
  HaveLang := False;
  RGLanguages.Items.Clear;
  fLanguagesList:= TStringList.Create;
  LanguageCode := GetCurrentLanguage;
  DefaultInstance.BindtextdomainToFile('languagecodes',
    ExtractFilePath(Application.ExeName) + 'locale\languagecodes.mo');
  DefaultInstance.GetListOfLanguages('default', fLanguagesList);
  fLanguagesList.Insert(0, 'en');
  for i := 0 to fLanguagesList.Count - 1 do begin
    // Translate the language code to English language name and then to a localized language name
    English:= dgettext('languagecodes', fLanguagesList[i]);
    Language := dgettext('languages', English);
    if (i = 0) and (pos('Englis', Language) = 0) then
      Language:= Language + ' (English)';
    RGLanguages.Items.Add(Language);
    if fLanguagesList[i] = LanguageCode then
      HaveLang := True;
  end;
  if not HaveLang then
    RGLanguages.ItemIndex:= 0;
end;

procedure TFConfiguration.UpdateHeaderFooter;
begin
  Header:= ReplaceText(Header, 'Datei', 'FILE');
  Header:= ReplaceText(Header, 'Pfad', 'PATH');
  Header:= ReplaceText(Header, 'Datum', 'DATE');
  Header:= ReplaceText(Header, 'Seite', 'PAGENUM');
  Header:= ReplaceText(Header, 'Seitenzahl', 'PAGECOUNT');
  Header:= ReplaceText(Header, '%PAGE%', '$PAGENUM%');

  Footer:= ReplaceText(Footer, 'Datei', 'FILE');
  Footer:= ReplaceText(Footer, 'Pfad', 'PATH');
  Footer:= ReplaceText(Footer, 'Datum', 'DATE');
  Footer:= ReplaceText(Footer, 'Seite', 'PAGENUM');
  Footer:= ReplaceText(Footer, 'Seitenzahl', 'PAGECOUNT');
  Footer:= ReplaceText(Footer, '%PAGE%', '$PAGENUM%');
end;

procedure TFConfiguration.InitTreeView;
begin
  LNGTVItems.Clear;
  LNGTVItems.Add(_('Java'));
  LNGTVItems.Add(_('Interpreter'));
  LNGTVItems.Add(_('Compiler'));
  LNGTVItems.Add(_('Programs'));
  LNGTVItems.Add(_('Applets'));
  LNGTVItems.Add(_('Disassembler'));
  LNGTVItems.Add(_('Jar'));
  LNGTVItems.Add(_('Editor'));
  LNGTVItems.Add(_('Options'));
  LNGTVItems.Add(_('Code'));
  LNGTVItems.Add(_('Colors'));
  LNGTVItems.Add(_('Comment'));
  LNGTVItems.Add(_('Templates'));
  LNGTVItems.Add(_('Keyboard'));
  LNGTVItems.Add(_('GUI designer'));
  LNGTVItems.Add(_('Structograms'));
  LNGTVItems.Add(_('Sequencediagrams'));
  LNGTVItems.Add(_('Browser'));
  LNGTVItems.Add(_('Documentation'));
  LNGTVItems.Add(_('Printer'));
  LNGTVItems.Add(_('Mindstorms'));
  LNGTVItems.Add(_('Android'));
  LNGTVItems.Add(  'Language');
  LNGTVItems.Add(_('Options'));
  LNGTVItems.Add(_('Styles'));
  LNGTVItems.Add(_('Restrictions'));
  LNGTVItems.Add(_('Associations'));
  LNGTVItems.Add(_('UML'));
  LNGTVItems.Add(_('UML options'));
  LNGTVItems.Add(_('LLM Assistant'));
  LNGTVItems.Add(_('LLM Chat'));
  LNGTVItems.Add(_('Visibility'));
  LNGTVItems.Add(_('Log files'));
  LNGTVItems.Add(_('Tools'));
  LNGTVItems.Add(_('Git'));
  LNGTVItems.Add(_('JUnit'));
  LNGTVItems.Add(_('Checkstyle'));
  LNGTVItems.Add(_('Jalopy'));
  LNGTVItems.Add(_('Subversion'));
  for var i:= 0 to TVConfiguration.Items.Count - 1 do
    TVConfiguration.Items[i].Text:= LNGTVItems[i];
end;

function TFConfiguration.GlobalFileExists(var Filename: string; withoutChange: boolean = true): boolean;
  var s, Cachename, FilenameSik: string; p: integer;
begin
  if FileExists(Filename) then Exit(true);
  if Filename = '' then Exit(false);
  FilenameSik:= Filename;
  if IsHttp(Filename) then begin
    Filename:= HttpToWeb(Filename);
    s:= StripHttpParams(Filename);
    s:= StripHttp(Filename);
    p:= Pos('/', s);
    Cachename:= copy(s, p, length(s));
    if Cachename = '/' then Cachename:= Cachename + 'index.html';
    Cachename:= ToWindows(FConfiguration.JavaCache + Cachename);
    ForceDirectories(ExtractFilePath(Cachename));
    with TFDownload.Create(Self) do begin
      GetInetFile(Filename, Cachename, ProgressBar);  // Cache oder Internet
      Free;
    end;
  end else if IsCHM(Filename) then begin
    p:= Pos('.chm\', Filename);
    Cachename:= FConfiguration.Javacache + '\' + copy(Filename, p+5, length(Filename));
    if not FileExists(Cachename) then begin
      ForceDirectories(ExtractFilePath(Cachename));
      var SL:= TStringList.Create;
      SL.Text:= LoadFromCHM(Filename);
      SL.SaveToFile(Cachename);
      FreeAndNil(SL);
    end;
  end else
    Cachename:= Filename;
  Filename:= Cachename;
  Result:= FileExists(Filename);
  if withoutChange then
    Filename:= FilenameSik;
end;

function TFConfiguration.ExtractZipToDir(const Filename, Dir: string): boolean;
begin
  Result:= false;
  var ZipFile:= TZipFile.Create;
  try
    try
      ZipFile.Open(Filename, zmRead);
      ZipFile.ExtractAll(Dir);
      Result:= true;
    except on E: Exception do
      ErrorMsg(E.Message + ' ' + SysErrorMessage(GetLastError));
    end;
  finally
    FreeAndNil(ZipFile);
  end;
end;

procedure TFConfiguration.ReadProviders(Name: string; var Providers: TLLMProviders);

  procedure ReadProvider(ID: string; var Provider: TLLMSettings);
  begin
    var key:= Name + '\' + ID;
    Provider.EndPoint:= ReadStringU(key, 'EndPoint', Provider.EndPoint);
    Provider.ApiKey:= Obfuscate(ReadStringU(key, 'ApiKey', Provider.ApiKey));
    Provider.Model:= ReadStringU(key, 'Model', Provider.Model);
    Provider.SystemPrompt:= ReadStringU(key, 'SystemPrompt', Provider.SystemPrompt);
    Provider.TimeOut:= ReadIntegerU(key, 'TimeOut', Provider.TimeOut);
    Provider.MaxTokens:= ReadIntegerU(key, 'MaxTokens', Provider.MaxTokens);
    Provider.Temperature:= StrToFloatDef(ReadStringU(key, 'Temperature', FloatToStr(Provider.Temperature)), Provider.Temperature);
  end;

begin
  Providers.Provider:= TLLMProvider(ReadIntegerU(Name, 'Provider', Integer(Providers.Provider)));
  ReadProvider('OpenAI', Providers.OpenAI);
  ReadProvider('Gemini', Providers.Gemini);
  ReadProvider('Ollama', Providers.Ollama);
  ReadProvider('DeepSeek', Providers.DeepSeek);
  ReadProvider('Grok', Providers.Grok);
end;

procedure TFConfiguration.WriteProviders(Name: string; Providers: TLLMProviders);

  procedure WriteProvider(ID: string; Provider: TLLMSettings);
  begin
    var key:= Name + '\' + ID;
    WriteStringU(key, 'EndPoint', Provider.EndPoint);
    WriteStringU(key, 'ApiKey', Obfuscate(Provider.ApiKey));
    WriteStringU(key, 'Model', Provider.Model);
    WriteStringU(key, 'SystemPrompt', Provider.SystemPrompt);
    WriteIntegerU(key, 'TimeOut', Provider.TimeOut);
    WriteIntegerU(key, 'MaxTokens', Provider.MaxTokens);
    WriteStringU(key, 'Temperature', Provider.Temperature.ToString);
  end;

begin
  WriteIntegerU(Name, 'Provider', Integer(Providers.Provider));
  WriteProvider('OpenAI', Providers.OpenAI);
  WriteProvider('Gemini', Providers.Gemini);
  WriteProvider('Ollama', Providers.Ollama);
  WriteProvider('DeepSeek', Providers.DeepSeek);
  WriteProvider('Grok', Providers.Grok);
end;

procedure TFConfiguration.CopyProviders(From: TLLMProviders; var Toward: TLLMProviders);

  procedure CopyProvider(From: TLLMSettings; var Toward: TLLMSettings);
  begin
    Toward.EndPoint:= From.EndPoint;
    Toward.ApiKey:= From.ApiKey;
    Toward.Model:= From.Model;
    Toward.SystemPrompt:= From.SystemPrompt;
    Toward.Timeout:= From.Timeout;
    Toward.MaxTokens:= From.MaxTokens;
    Toward.Temperature:= From.Temperature;
  end;

begin
  Toward.Provider:= From.Provider;
  CopyProvider(From.OpenAI, Toward.OpenAI);
  CopyProvider(From.Gemini, Toward.Gemini);
  CopyProvider(From.Ollama, Toward.Ollama);
  CopyProvider(From.DeepSeek, Toward.DeepSeek);
  CopyProvider(From.Grok, Toward.Grok);
end;

procedure TFConfiguration.LLMAssistantModelToView(Settings: TLLMSettings);
begin
  EEndPoint.text:= Settings.EndPoint;
  EModel.text:= Settings.Model;
  EAPIKey.text:= Settings.ApiKey;
  ESystemPrompt.text:= Settings.SystemPrompt;
  EMaxTokens.text:= Settings.MaxTokens.toString;
  ELLMTimeout.text:= (Settings.TimeOut div 1000).toString;
  ELLMTemperature.text:= StrUtils.LeftStr((Settings.Temperature).toString, 5);
end;

procedure TFConfiguration.LLMAssistantViewToModel;
  var Settings: TLLMSettings; value: integer;
begin
  TempProviders.Provider:= TLLMProvider(CBProvider.ItemIndex);
  Settings.EndPoint:= EEndPoint.text;
  Settings.Model:= EModel.text;
  Settings.ApiKey:= EAPIKey.text;
  Settings.SystemPrompt:= ESystemPrompt.text;
  if not TryStrToInt(EMaxTokens.text, value) then
    value:= 1000;
  Settings.MaxTokens:= value;
  if not TryStrToInt(ELLMTimeout.text, value) then
    value:= 20;
  Settings.Timeout:= value*1000;
  Settings.Temperature:= StringZuSingle(ELLMTemperature.text);
  case CBProvider.ItemIndex of
    0: TempProviders.OpenAI:= Settings;
    1: TempProviders.Gemini:= Settings;
    2: TempProviders.Ollama:= Settings;
    3: TempProviders.DeepSeek:= Settings;
    4: TempProviders.Grok:= Settings;
  end;
end;

procedure TFConfiguration.LLMChatModelToView(Settings: TLLMSettings);
begin
  EChatEndPoint.text:= Settings.EndPoint;
  EChatModel.text:= Settings.Model;
  EChatAPIKey.text:= Settings.ApiKey;
  EChatSystemPrompt.text:= Settings.SystemPrompt;
  EChatMaxTokens.text:= Settings.MaxTokens.toString;
  EChatTimeout.text:= (Settings.TimeOut div 1000).toString;
  EChatTemperature.text:= Settings.Temperature.toString;
end;

function TFConfiguration.LLMChatSettings: TLLMSettings;
begin
  case CBChatProvider.ItemIndex of
    0: Result := TempChatProviders.OpenAI;
    1: Result := TempChatProviders.Gemini;
    2: Result := TempChatProviders.Ollama;
    3: Result := TempChatProviders.DeepSeek;
    4: Result := TempChatProviders.Grok;
  end;
end;

function TFConfiguration.LLMAssistantSettings: TLLMSettings;
begin
  case CBProvider.ItemIndex of
    0: Result := TempProviders.OpenAI;
    1: Result := TempProviders.Gemini;
    2: Result := TempProviders.Ollama;
    3: Result := TempProviders.DeepSeek;
    4: Result := TempProviders.Grok;
  end;
end;

procedure TFConfiguration.BLLMAssistantDefaultClick(Sender: TObject);
begin
  var Settings:= LLMAssistant.DefaultAssistantSettings(CBProvider.ItemIndex);
  Settings.ApiKey:= LLMAssistantSettings.ApiKey;
  LLMAssistantModelToView(Settings);
end;

procedure TFConfiguration.BLLMChatDefaultClick(Sender: TObject);
begin
  var Settings:= FLLMChatForm.LLMChat.DefaultChatSettings(CBChatProvider.ItemIndex);
  Settings.ApiKey:= LLMChatSettings.ApiKey;
  LLMChatModelToView(Settings);
end;

procedure TFConfiguration.LLMChatViewToModel;
  var Settings: TLLMSettings; value: integer;
begin
  TempChatProviders.Provider:= TLLMProvider(CBChatProvider.ItemIndex);
  Settings.EndPoint:= EChatEndPoint.text;
  Settings.Model:= EChatModel.text;
  Settings.ApiKey:= EChatAPIKey.text;
  Settings.SystemPrompt:= EChatSystemPrompt.text;
  if not TryStrToInt(EChatMaxTokens.text, value) then
    value:= 1000;
  Settings.MaxTokens:= value;
  if not TryStrToInt(EChatTimeout.text, value) then
    value:= 20;
  Settings.Timeout:= value*1000;
  Settings.Temperature:= StringZuSingle(EChatTemperature.text);
  case CBChatProvider.ItemIndex of
    0: TempChatProviders.OpenAI:= Settings;
    1: TempChatProviders.Gemini:= Settings;
    2: TempChatProviders.Ollama:= Settings;
    3: TempChatProviders.DeepSeek:= Settings;
    4: TempChatProviders.Grok:= Settings;
  end;
end;

initialization
  TStyleManager.Engine.RegisterStyleHook(TEdit, TEditStyleHookColor);

finalization
  TStyleManager.Engine.UnRegisterStyleHook(TEdit, TEditStyleHookColor);

end.
