unit UGUIDesigner;

interface

uses
  Classes,
  Controls,
  Forms,
  Menus,
  ExtCtrls,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  TB2Item,
  SpTBXItem,
  ELDsgnr,
  UGUIForm,
  UEditorForm;

type
  TFGUIDesigner = class(TForm)
    PopupMenu: TSpTBXPopupMenu;
    MIClose: TSpTBXItem;
    MIToSource: TSpTBXItem;
    MIForeground: TSpTBXItem;
    MIBackground: TSpTBXItem;
    N1Sep: TSpTBXSeparatorItem;
    MISnapToGrid: TSpTBXItem;
    N2Sep: TSpTBXSeparatorItem;
    MIDelete: TSpTBXItem;
    MICopy: TSpTBXItem;
    MICut: TSpTBXItem;
    MIPaste: TSpTBXItem;
    GUIDesignerTimer: TTimer;
    MIAlign: TSpTBXSubmenuItem;
    MIAlignLeft: TSpTBXItem;
    MIAlignCentered: TSpTBXItem;
    MIAlignRight: TSpTBXItem;
    MIAlignLine: TSpTBXSeparatorItem;
    MIAlignTop: TSpTBXItem;
    MIAlignMiddle: TSpTBXItem;
    MIAlignBottom: TSpTBXItem;
    MIAlignCenteredInWindowHorz: TSpTBXItem;
    MISameDistanceHorz: TSpTBXItem;
    MIAlignCenteredInWindowVert: TSpTBXItem;
    MISameDistanceVert: TSpTBXItem;
    N4Sep: TSpTBXSeparatorItem;
    MIZoomIn: TSpTBXItem;
    MIZoomOut: TSpTBXItem;
    scGuiDesigner: TSVGIconImageCollection;
    icJavaControls1315: TSVGIconImageCollection;
    icJavaControls1616: TSVGIconImageCollection;
    icJavaControls1618: TSVGIconImageCollection;
    icJavaControls21616: TSVGIconImageCollection;
    icPagination: TSVGIconImageCollection;
    icTurtles: TSVGIconImageCollection;
    icFXTurtle: TSVGIconImageCollection;
    vilGuiDesignerLight: TVirtualImageList;
    vilControls1315: TVirtualImageList;
    vilControls1616: TVirtualImageList;
    vilControls1618: TVirtualImageList;
    vilControls21616: TVirtualImageList;
    vilPagination: TVirtualImageList;
    vilTurtles: TVirtualImageList;
    vilFXTurtle: TVirtualImageList;
    vilGUIDesignerDark: TVirtualImageList;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    MIConfiguration: TSpTBXItem;
    procedure ELDesignerControlInserting(Sender: TObject;
      var AControlClass: TControlClass);
    procedure ELDesignerControlInserted(Sender: TObject);
    procedure ELDesignerChangeSelection(Sender: TObject);
    procedure ELDesignerModified(Sender: TObject);
    procedure ELDesignerDesignFormClose(Sender: TObject;
      var Action: TCloseAction);
    procedure ELDesignerControlDeleting(Sender: TObject;
      SelectedControls: TELDesignerSelectedControls);
    procedure ELDesignerDblClick(Sender: TObject);
    procedure ELDesignerGetUniqueName(Sender: TObject; const ABaseName: string;
      var AUniqueName: string);
    procedure ELDragDrop(Sender, ASource, ATarget: TObject;
      XPos, YPos: Integer);
    procedure ELDragOver(Sender, ASource, ATarget: TObject; XPos, YPos: Integer;
      AState: TDragState; var Accept: Boolean);
    procedure MIDeleteClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure MIToSourceClick(Sender: TObject);
    procedure MISnapToGridClick(Sender: TObject);
    procedure GUIDesignerTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure CutClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure MIForegroundClick(Sender: TObject);
    procedure MIBackgroundClick(Sender: TObject);
    procedure MIAlignClick(Sender: TObject);
    procedure MIZoomInClick(Sender: TObject);
    procedure MIZoomOutClick(Sender: TObject);
    procedure MIConfigurationClick(Sender: TObject);
  private
    FComponentToInsert: TControlClass;
    FDesignForm: TFGUIForm;
    FELDesigner: TELDesigner;
    function GetParent(ATarget: TObject; XPos, YPos: Integer): TWinControl;
    procedure ComponentToBackground(APartner: TFEditForm; Control: TControl);
    procedure ComponentToForeground(APartner: TFEditForm; Control: TControl);
    function GetPixelsPerInchOfFile(const FileName: string): Integer;
    procedure RemovePixelsPerInch0(const FileName: string);
    procedure RemoveMDIChild(const FileName: string);
    procedure RemoveFrameType(const FileName: string);
  public
    procedure Save(const FileName: string; AForm: TFGUIForm);
    function Open(const FileName, JavaFilename: string): TFGUIForm;
    procedure FindMethod(Reader: TReader; const MethodName: string;
      var Address: Pointer; var Error: Boolean);
    procedure ErrorMethod(Reader: TReader; const Message: string;
      var Handled: Boolean);
    procedure CreateComponent(Reader: TReader; ComponentClass: TComponentClass;
      var Component: TComponent);
    procedure MIAWTSwing(Tag: Integer);
    procedure TBAWTSwing(Tag: Integer);
    procedure TBJavaFX(Tag: Integer);
    procedure ChangeTo(AForm: TFGUIForm);
    function GetEditForm: TFEditForm;
    procedure UpdateState(Modified: Boolean);
    procedure DoPanelCanvas(TagNr: Integer; const FileName: string);
    function GetPath: string;
    function Tag2Class(Tag: Integer): TControlClass;
    procedure Zooming(Delta: Integer);
    procedure SetComponentValues(DesignForm: TFGUIForm; Control: TControl);
    procedure SetAttributForComponent(const Attr, Value, Typ: string;
      Control: TControl);
    procedure ScaleImages;
    procedure ChangeStyle;
    procedure InsertComponent(Control: TControl);
    procedure MoveComponent(Control: TControl);
    procedure DeleteComponent(Control: TControl);
    property DesignForm: TFGUIForm read FDesignForm write FDesignForm;
    property ELDesigner: TELDesigner read FELDesigner write FELDesigner;
  end;

  TMyDragObject = class(TDragControlObjectEx)
  private
    FDragImages: TDragImageList;
  protected
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

var
  FGUIDesigner: TFGUIDesigner = nil;

implementation

uses
  Windows,
  Messages,
  SysUtils,
  Graphics,
  ActnList,
  Mask,
  Clipbrd,
  ComCtrls,
  Dialogs,
  Grids,
  Buttons,
  Tabs,
  DdeMan,
  MPlayer,
  OleCtnrs,
  TabNotBk,
  FileCtrl,
  Outline,
  Spin,
  System.Types,
  UITypes,
  TypInfo,
  StdCtrls,
  ELPropInsp,
  JvGnugettext,
  UJava,
  UBaseForm,
  UObjectGenerator,
  ULink,
  UConfiguration,
  UUtils,
  UJComponents,
  UJEComponents,
  UJUtilities,
  UObjectInspector,

  UAButton, UACheckboxRadio, UAComboBox, UALabel, UAList, UAMenu,
  UAPanel, UAScrollBar, UAScrollPane, UATextArea, UATextField,

  UJButton, UJCheckboxRadio, UJComboBox, UJLabel, UJList, UJMenu,
  UJPanel, UJScrollBar, UJScrollPane, UJTextArea, UJTextField,
  UJSlider, UJProgressBar, UJSplitPane, UJTabbedPane, UJTable,
  UJTree, UJSpinner, UJToolBar, UJSeparator, UJLayeredDesktopPane,
  UJChooser,

  UFXLabel, UFXTextInput, UFXButtonBase, UFXListView, UFXPane,
  UFXComboBox, UFXSpinner, UFXScrollBar, UFXScrollPane, UFXCanvas,
  UFXMenu, UFXSlider, UFXProgressBar, UFXToolBar, UFXSeparator, UFXChoiceBox,
  UFXHTMLEditor, UFXPagination, UFXFileChooser, UFXImageView, UFXWebView,
  UFXTableView, UFXMediaView, UFXTreeView, UFXCircle, UFXRectangle,
  UFXPolygon, UFXLine, UFXText, UFXBezier, UFXSVGPath, UFXTurtle, UFXGUIForm;

{$R *.dfm}

type
  TClassArray = array [1 .. 226] of TPersistentClass;

const
  Modified = True;

  ClassArray: TClassArray = (TBitmap, TGraphic, TOutlineNode, TGraphicsObject,
    TBrush, THeaderSection, TCanvas, THeaderSections, TPen, TIcon, TPicture,
    TIconOptions, TCollection, TCollectionItem, TStatusPanel, TStatusPanels,
    TClipboard, TControlScrollBar, TListColumn, TStringList, TListItem,
    TStrings, TListItems, TMetafile, TMetafileCanvas, TTreeNode, TFont,
    TParaAttributes, TTreeNodes, TApplication, TDdeServerItem, TPanel,
    TDirectoryListBox, TPopupMenu, TDrawGrid, TPrintDialog, TDriveComboBox,
    TPrinterSetupDialog, TBevel, TEdit, TProgressBar, TBitBtn, TFileListBox,
    TRadioButton, TFilterComboBox, TRadioGroup, TButton, TFindDialog,
    TReplaceDialog, TCheckBox, TFontDialog, TRichEdit, TColorDialog, TForm,
    TSaveDialog, TComboBox, TScreen, TGroupBox, TScrollBar, THeader, TScrollBox,
    THeaderControl, THotKey, TShape, TStaticText, TImage, TImageList,
    TSpeedButton, TStatusBar, TLabel, TListBox, TListView, TStringGrid,
    TMainMenu, TTabbedNotebook, TMaskEdit, TTabControl, TMediaPlayer, TMemo,
    TTabSet, TTabSheet, TToolBar, TCoolBar, TToolButton, TMenuItem, TNotebook,
    TOleContainer, TTimer, TOpenDialog, TTrackBar, TOutline, TTreeView,
    TDdeClientConv, TOutline, TDdeClientItem, TPageControl, TUpDown,
    TDdeServerConv, TPaintBox, TSpinEdit, TSplitter, TActionList, TAction,
    TFGUIForm,

    TALabel, TJLabel, TATextField, TJTextField, TANumberField, TJNumberField,
    TATextArea, TJTextArea, TAButton, TJButton, TACheckBox, TJCheckBox,
    TARadioButton, TJRadioButton, TAList, TJList, TAComboBox, TJComboBox,
    TAScrollBar, TJScrollBar, TAScrollPane, TJScrollPane, TAPanel, TASubPanel,
    TJPanel, TJSubPanel, TACanvas, TASubCanvas, TATurtle, TJSlider,
    TJProgressBar, TJSplitPane, TJTabbedPane, TJTable, TJTree, TJSpinner,
    TJToolBar, TJSeparator, TJToggleButton, TJPasswordField,
    TJFormattedTextField, TJEditorPane, TJTextPane, TJLayeredPane,
    TJDesktopPane, TJInternalFrame, TJFileChooser, TJFileSaveChooser,
    TJColorChooser, TJOptionPane, TACheckboxGroup, TJButtonGroup, TAMenuBar,
    TAMenuBarWithMenus, TJMenuBar, TJMenuBarWithMenus, TAMenu, TJMenu,
    TAPopupMenu, TJPopupMenu, TTimer, TJPlayground, TJTurtle, TJ2ButtonGroup,
    TA2ButtonGroup, TFXLabel, TFXTextField, TFXNumberField, TFXTextArea,
    TFXButton, TFXCheckBox, TFXRadioButton, TFXToggleGroup, TFXListView,
    TFXComboBox, TFXSpinner, TFXScrollBar, TFXScrollPane, TFXCanvas, TFXMenuBar,
    TFXMenuBarWithMenus, TFXMenu, TFXContextMenu, TFXSlider, TFXProgressBar,
    TFXProgressIndicator, TFXToolBar, TFXSeparator, TFXToggleButton,
    TFXPasswordField, TFXChoiceBox, TFXHyperlink, TFXButtonGroup, TFXHTMLEditor,
    TFXColorPicker, TFXDatePicker, TFXPagination, TFXFileOpenChooser,
    TFXFileSaveChooser, TFXDirectoryChooser, TFXImageView, TFXWebView,
    TFXTableView, TFXMediaView, TFXMenuButton, TFXSplitMenuButton,
    TFXTreeTableView, TFXTreeView, TFXCircle, TFXRectangle, TFXEllipse,
    TFXPolygon, TFXPolyline, TFXArc, TFXLine, TFXText, TFXQuadCurve,
    TFXCubicCurve, TFXSVGPath, TFXTurtle, TFXPane, TFXSubCanvas);

procedure TFGUIDesigner.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ELDesigner := TELDesigner.Create(Self);
  ELDesigner.PopupMenu := PopupMenu;
  with ELDesigner do
  begin
    ShowingHints := [htControl, TELDesignerHintType.htSize, htMove, htInsert];
    ClipboardFormat := 'Extension Library designer components';
    OnModified := ELDesignerModified;
    OnGetUniqueName := ELDesignerGetUniqueName;
    OnControlInserting := ELDesignerControlInserting;
    OnDragDrop := ELDragDrop;
    OnDragOver := ELDragOver;
    OnControlInserted := ELDesignerControlInserted;
    OnControlDeleting := ELDesignerControlDeleting;
    OnChangeSelection := ELDesignerChangeSelection;
    OnDesignFormClose := ELDesignerDesignFormClose;
    OnDblClick := ELDesignerDblClick;
  end;
end;

procedure TFGUIDesigner.ChangeTo(AForm: TFGUIForm);
begin
  if Assigned(ELDesigner) then
  begin
    if (ELDesigner.DesignControl <> AForm) or not ELDesigner.Active then
    begin
      ELDesigner.Active := False;
      ELDesigner.DesignControl := AForm;
      DesignForm := AForm;
      if Assigned(AForm) then
        ELDesigner.Active := True;
      if FObjectInspector.Visible then
        FObjectInspector.RefreshCBObjects;
    end;
  end;
end;

procedure TFGUIDesigner.ELDesignerChangeSelection(Sender: TObject);
begin
  FObjectInspector.ChangeSelection(ELDesigner.SelectedControls);
  UpdateState(not Modified);
end;

procedure TFGUIDesigner.ELDesignerModified(Sender: TObject);
begin
  FObjectInspector.ELPropertyInspector.Modified;
  for var I := 0 to ELDesigner.SelectedControls.Count - 1 do
    MoveComponent(ELDesigner.SelectedControls[I]);
  UpdateState(Modified);
end;

procedure TFGUIDesigner.UpdateState(Modified: Boolean);
begin
  with FJava do
  begin
    SetEnabledMI(MICut, ELDesigner.CanCut);
    SetEnabledMI(MICopy, True);
    SetEnabledMI(MICopyNormal, True);
    SetEnabledMI(MIPaste, ELDesigner.CanPaste);
  end;
  if Modified and Assigned(ELDesigner.DesignControl) then
    TFForm(ELDesigner.DesignControl).Modified := True;
end;

procedure TFGUIDesigner.PopupMenuPopup(Sender: TObject);
begin
  PopupMenu.Images.SetSize(DesignForm.PPIScale(16), DesignForm.PPIScale(16));
  SetEnabledMI(MICut, ELDesigner.CanCut);
  SetEnabledMI(MICopy, ELDesigner.CanCopy);
  SetEnabledMI(MIPaste, ELDesigner.CanPaste);
  SetEnabledMI(MIAlign, ELDesigner.SelectedControls.Count >= 2);
  MISnapToGrid.Checked := ELDesigner.SnapToGrid;
  var
  Enable := (ELDesigner.SelectedControls.Count > 1) or
    ((ELDesigner.SelectedControls.Count = 1) and
    (ELDesigner.SelectedControls[0].ClassName <> 'TFGUIForm') and
    (ELDesigner.SelectedControls[0].ClassName <> 'TFXGuiForm'));
  SetEnabledMI(MIDelete, Enable);
end;

procedure TFGUIDesigner.MIDeleteClick(Sender: TObject);
begin
  if not DesignForm.ReadOnly then
  begin
    ELDesigner.DeleteSelectedControls;
    UpdateState(Modified);
  end;
end;

procedure TFGUIDesigner.MIForegroundClick(Sender: TObject);
begin
  for var I := 0 to ELDesigner.SelectedControls.Count - 1 do
    ComponentToForeground(GetEditForm, ELDesigner.SelectedControls[I]);
end;

procedure TFGUIDesigner.MIBackgroundClick(Sender: TObject);
begin
  for var I := 0 to ELDesigner.SelectedControls.Count - 1 do
    ComponentToBackground(GetEditForm, ELDesigner.SelectedControls[I]);
end;

procedure TFGUIDesigner.ComponentToBackground(APartner: TFEditForm;
  Control: TControl);
begin
  if Assigned(Control) then
  begin
    Control.SendToBack;
    APartner.ToBackground(Control);
  end;
end;

procedure TFGUIDesigner.ComponentToForeground(APartner: TFEditForm;
  Control: TControl);
begin
  if Assigned(Control) then
  begin
    Control.BringToFront;
    APartner.ToForeground(Control);
  end;
end;

procedure TFGUIDesigner.MIAlignClick(Sender: TObject);
var
  AHorzAlign, AVertAlign: TELDesignerAlignType;
  Tag: NativeInt;
begin
  Tag := (Sender as TSpTBXItem).Tag;
  AHorzAlign := atNoChanges;
  AVertAlign := atNoChanges;
  case Tag of
    1:
      AHorzAlign := atLeftTop;
    2:
      AHorzAlign := atCenter;
    3:
      AHorzAlign := atRightBottom;
    4:
      AHorzAlign := atCenterInWindow;
    5:
      AHorzAlign := atSpaceEqually;
    6:
      AVertAlign := atLeftTop;
    7:
      AVertAlign := atCenter;
    8:
      AVertAlign := atRightBottom;
    9:
      AVertAlign := atCenterInWindow;
    10:
      AVertAlign := atSpaceEqually;
  end;
  ELDesigner.SelectedControls.Align(AHorzAlign, AVertAlign);
end;

procedure TFGUIDesigner.ELDesignerControlInserting(Sender: TObject;
  var AControlClass: TControlClass);
begin
  if not DesignForm.ReadOnly then
    AControlClass := FComponentToInsert;
end;

function TFGUIDesigner.GetParent(ATarget: TObject; XPos, YPos: Integer)
  : TWinControl;
var
  Wid, Hei: Integer;
  WinControl: TWinControl;
begin
  WinControl := nil;
  if ATarget is TForm then
    WinControl := ATarget as TForm
  else if ATarget is TJPanel then
    WinControl := ATarget as TJPanel
  else if ATarget is TAPanel then
    WinControl := ATarget as TAPanel
  else if ATarget is TFXPane then
    WinControl := ATarget as TFXPane
  else if ATarget is TJPlayground then
    WinControl := ATarget as TJPlayground;
  if Assigned(WinControl) then
  begin
    Wid := WinControl.ClientWidth;
    Hei := WinControl.ClientHeight;
  end
  else
  begin
    Wid := -1;
    Hei := -1;
  end;
  if GComponentNrToInsert = 34 then
    if Assigned(WinControl) and not(WinControl is TJPlayground) then
      Exit(nil);
  if (XPos >= 0) and (YPos >= 0) and (XPos <= Wid) and (YPos <= Hei) then
    Result := WinControl
  else
    Result := nil;
end;

type
  TControlEx = class(TControl)
  protected
    FFont: TFont;
  end;

procedure TFGUIDesigner.ELDragDrop(Sender, ASource, ATarget: TObject;
  XPos, YPos: Integer);
var
  LInsertingControl: TControl;
  LName: string;
begin
  if csAcceptsControls in (ATarget as TControl).ControlStyle then
  begin
    LInsertingControl := ASource as TControl;
    ELDesigner.GetUniqueName((LInsertingControl as TJEComponent)
      .Tag2JavaType(Integer(LInsertingControl.Tag)), LName);
    LInsertingControl.Name := LName;
    LInsertingControl.Parent := ATarget as TWinControl;
    LInsertingControl.SetBounds(XPos - LInsertingControl.Width,
      YPos - LInsertingControl.Height, LInsertingControl.Width,
      LInsertingControl.Height);
    TControlEx(LInsertingControl).Font.Size := DesignForm.FontSize;
    ELDesigner.SelectedControls.ClearExcept(LInsertingControl);
    ELDesignerControlInserted(nil);
    UpdateState(Modified);
  end;
end;

procedure TFGUIDesigner.ELDragOver(Sender, ASource, ATarget: TObject;
  XPos, YPos: Integer; AState: TDragState; var Accept: Boolean);
begin
  Accept := Assigned(GetParent(ATarget, XPos, YPos));
end;

function TFGUIDesigner.Tag2Class(Tag: Integer): TControlClass;
begin
  case Tag of
    - 1:
      Result := TALabel;
    -2:
      Result := TATextField;
    -3:
      Result := TATextArea;
    -4:
      Result := TAButton;
    -5:
      Result := TACheckBox;
    -6:
      Result := TARadioButton;
    -7:
      Result := TACheckboxGroup; // deprecated since 19.33
    -8:
      Result := TAList;
    -9:
      Result := TAComboBox;
    -10:
      Result := TAScrollBar;
    -11:
      Result := TAScrollPane;
    -12:
      Result := TAPanel;
    -13:
      Result := TACanvas;
    -14:
      Result := TATurtle;
    -21:
      Result := TANumberField;
    -38:
      Result := TASubPanel;
    -39:
      Result := TASubCanvas;
    -42:
      Result := TAMenuBar;
    -43:
      Result := TAMenu;
    -44:
      Result := TAPopupMenu;
    -50:
      Result := TA2ButtonGroup;
    -52:
      Result := TAMenuBarWithMenus;

    1:
      Result := TJLabel;
    2:
      Result := TJTextField;
    3:
      Result := TJTextArea;
    4:
      Result := TJButton;
    5:
      Result := TJCheckBox;
    6:
      Result := TJRadioButton;
    7:
      Result := TJButtonGroup; // deprecated since 19.33
    8:
      Result := TJList;
    9:
      Result := TJComboBox;
    10:
      Result := TJScrollBar;
    11:
      Result := TJScrollPane;
    12:
      Result := TJPanel;
    15:
      Result := TJSlider;
    16:
      Result := TJProgressBar;
    17:
      Result := TJSplitPane;
    18:
      Result := TJTabbedPane;
    19:
      Result := TJTable;
    20:
      Result := TJTree;
    21:
      Result := TJNumberField;
    22:
      Result := TJSpinner;
    23:
      Result := TJToolBar;
    24:
      Result := TJSeparator;
    25:
      Result := TJToggleButton;
    26:
      Result := TJPasswordField;
    27:
      Result := TJFormattedTextField;
    28:
      Result := TJEditorPane;
    29:
      Result := TJTextPane;
    30:
      Result := TJLayeredPane;
    31:
      Result := TJDesktopPane;
    32:
      Result := TJInternalFrame;
    33:
      Result := TJPlayground;
    34:
      Result := TJTurtle;
    38:
      Result := TJSubPanel;
    42:
      Result := TJMenuBar;
    43:
      Result := TJMenu;
    44:
      Result := TJPopupMenu;
    45:
      Result := TJFileChooser;
    46:
      Result := TJFileSaveChooser;
    47:
      Result := TJColorChooser;
    48:
      Result := TJOptionPane;
    49:
      Result := TTimer;
    50:
      Result := TJ2ButtonGroup;
    52:
      Result := TJMenuBarWithMenus;

    // FX
    101:
      Result := TFXLabel;
    102:
      Result := TFXTextField;
    103:
      Result := TFXNumberField;
    104:
      Result := TFXTextArea;
    105:
      Result := TFXButton;
    106:
      Result := TFXCheckBox;
    107:
      Result := TFXRadioButton;
    108:
      Result := TFXToggleGroup;
    109:
      Result := TFXListView;
    110:
      Result := TFXComboBox;
    111:
      Result := TFXSpinner;
    112:
      Result := TFXScrollBar;
    113:
      Result := TFXScrollPane;
    114:
      Result := TFXCanvas;
    115:
      Result := TFXMenuBar;
    116:
      Result := TFXMenu;
    117:
      Result := TFXContextMenu;
    118:
      Result := TFXMenuButton;
    119:
      Result := TFXSplitMenuButton;
    120:
      Result := TFXTurtle;
    121:
      Result := TFXPane;
    122:
      Result := TFXSubCanvas;
    123:
      Result := TFXButtonGroup;
    124:
      Result := TFXMenuBarWithMenus;

    131:
      Result := TFXSlider;
    132:
      Result := TFXProgressBar;
    133:
      Result := TFXProgressIndicator;
    134:
      Result := TFXToolBar;
    135:
      Result := TFXSeparator;
    136:
      Result := TFXToggleButton;
    137:
      Result := TFXPasswordField;
    138:
      Result := TFXChoiceBox;
    139:
      Result := TFXHyperlink;
    140:
      Result := TFXHTMLEditor;
    141:
      Result := TFXColorPicker;
    142:
      Result := TFXDatePicker;
    143:
      Result := TFXPagination;
    144:
      Result := TFXFileOpenChooser;
    145:
      Result := TFXFileSaveChooser;
    146:
      Result := TFXDirectoryChooser;
    147:
      Result := TFXImageView;
    148:
      Result := TFXWebView;
    149:
      Result := TFXTableView;
    150:
      Result := TFXMediaView;
    151:
      Result := TFXTreeTableView;
    152:
      Result := TFXTreeView;

    // Shapes
    161:
      Result := TFXCircle;
    162:
      Result := TFXRectangle;
    163:
      Result := TFXEllipse;
    164:
      Result := TFXPolygon;
    165:
      Result := TFXPolyline;
    166:
      Result := TFXArc;
    167:
      Result := TFXLine;
    168:
      Result := TFXText;
    169:
      Result := TFXQuadCurve;
    170:
      Result := TFXCubicCurve;
    171:
      Result := TFXSVGPath;
  else
    Result := nil;
  end;
end;

procedure TFGUIDesigner.MIAWTSwing(Tag: Integer);
var
  XPos, YPos, DeltaX, DeltaY: Integer;
  Ctrl: TWinControl;
  MyMouse: TPoint;
begin
  TBAWTSwing(Tag); // set Component to insert
  if Assigned(FComponentToInsert) then
  begin
    GetCursorPos(MyMouse);
    if ELDesigner.SnapToGrid then
    begin
      DeltaX := Random(5) * ELDesigner.Grid.XStep;
      DeltaY := Random(5) * ELDesigner.Grid.YStep;
    end
    else
    begin
      DeltaX := Random(40);
      DeltaY := Random(40);
    end;
    XPos := FJava.Left + ELDesigner.DesignControl.Left + 18 + DeltaX;
    YPos := FJava.Top + ELDesigner.DesignControl.Top + 170 + DeltaY;
    SetCursorPos(XPos, YPos);
    Ctrl := FindVCLWindow(Mouse.CursorPos);
    if Assigned(Ctrl) then
    begin
      SendMessage(Ctrl.Handle, WM_LBUTTONDOWN, MK_LBUTTON, 0);
      SendMessage(Ctrl.Handle, WM_LBUTTONUP, 0, 0);
    end;
    SetCursorPos(MyMouse.X, MyMouse.Y);
  end;
end;

procedure TFGUIDesigner.TBAWTSwing(Tag: Integer);
begin
  FComponentToInsert := nil;
  var
  Form := GetEditForm;
  if Assigned(Form) and (Form.FrameType in [2 .. 8]) then
  begin
    GComponentNrToInsert := Tag;
    FComponentToInsert := Tag2Class(Tag);
    ScaleImages;
  end;
end;

procedure TFGUIDesigner.TBJavaFX(Tag: Integer);
begin
  FComponentToInsert := nil;
  var
  Form := GetEditForm;
  if Assigned(Form) and (Form.FrameType = 8) then
  begin
    GComponentNrToInsert := Tag;
    FComponentToInsert := Tag2Class(Tag);
    ScaleImages;
  end;
end;

procedure TFGUIDesigner.ELDesignerControlInserted(Sender: TObject);
begin
  var
  Control := ELDesigner.SelectedControls[0];
  if (Control is TJTurtle) and not(Control.Parent is TJPlayground) then
  begin
    FreeAndNil(Control);
    Exit;
  end;

  Control.Tag := GComponentNrToInsert;
  Control.HelpKeyword := PanelCanvasType;
  Control.HelpType := htContext;
  Control.Hint := Control.Name;
  Control.ShowHint := True;

  FObjectInspector.RefreshCBObjects;
  FObjectInspector.SetSelectedObject(Control);
  FComponentToInsert := nil;
  InsertComponent(Control);
  UpdateState(Modified);
  FJava.ResetToolbars;
end;

procedure TFGUIDesigner.Save(const FileName: string; AForm: TFGUIForm);
begin
  var
  BinStream := TMemoryStream.Create;
  try
    try
      var
      FilStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
      try
        BinStream.WriteComponent(AForm);
        BinStream.Seek(0, soFromBeginning);
        ObjectBinaryToText(BinStream, FilStream);
      finally
        FreeAndNil(FilStream);
      end;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(BinStream);
  end;
  if Assigned(ELDesigner.DesignControl) then
    TFForm(ELDesigner.DesignControl).Modified := False;
end;

procedure TFGUIDesigner.RemovePixelsPerInch0(const FileName: string);
begin
  var
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    for var I := 0 to StringList.Count - 1 do
    begin
      var
      Str := StringList[I];
      var
      Posi := Pos('PixelsPerInch', Str);
      if Posi > 0 then
      begin
        Posi := Pos('=', Str);
        Delete(Str, 1, Posi);
        Posi := StrToInt(Trim(Str));
        if Posi = 0 then
        begin
          StringList.Delete(I);
          var WriteProtected:= IsWriteProtected(FileName);
          if WriteProtected then
            RemoveReadOnlyAttribute(FileName);
          try
            StringList.SaveToFile(FileName);
            if WriteProtected then
              SetReadOnlyAttribute(FileName);
          except
            ErrorMsg(Format(_('File %s contains invalid "PixelsPerInch = 0", ' +
                    'but cannot delete due to write protection.'), [FileName]));
          end;
          Break;
        end;
      end
      else if Pos('  object', Str) > 0 then
        Break;
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TFGUIDesigner.RemoveMDIChild(const FileName: string);
begin
  var
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    for var I := 0 to StringList.Count - 1 do
    begin
      var
      Str := StringList[I];
      var
      Posi := Pos('FormStyle = fsMDIChild', Str);
      if Posi > 0 then
      begin
        StringList.Delete(I);
        var WriteProtected:= IsWriteProtected(FileName);
        if WriteProtected then
          RemoveReadOnlyAttribute(FileName);
        try
          StringList.SaveToFile(FileName);
          if WriteProtected then
            SetReadOnlyAttribute(FileName);
        except
          ErrorMsg(Format(_('File %s contains invalid "FormStyle = fsMDIChild", ' +
                  'but cannot delete due to write protection.'), [FileName]));
        end;
        Break;
      end
      else if Pos('  object', Str) > 0 then
        Break;
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TFGUIDesigner.RemoveFrameType(const FileName: string);
begin
  var
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    for var I := 0 to StringList.Count - 1 do
    begin
      var
      Str := StringList[I];
      var
      Posi := Pos('FrameType =', Str);
      if Posi > 0 then
      begin
        StringList.Delete(I);
        var WriteProtected:= IsWriteProtected(FileName);
        if WriteProtected then
          RemoveReadOnlyAttribute(FileName);
        try
          StringList.SaveToFile(FileName);
          if WriteProtected then
            SetReadOnlyAttribute(FileName);
        except
          ErrorMsg(Format(_('File %s contains invalid "FrameType =", ' +
                  'but cannot delete due to write protection.'), [FileName]));
        end;
        Break;
      end
      else if Pos('  object', Str) > 0 then
        Break;
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

function TFGUIDesigner.GetPixelsPerInchOfFile(const FileName: string): Integer;
begin
  Result := 96;
  var
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    for var I := 0 to StringList.Count - 1 do
    begin
      var
      Str := StringList[I];
      var
      Posi := Pos('PixelsPerInch', Str);
      if Posi > 0 then
      begin
        Posi := Pos('=', Str);
        Delete(Str, 1, Posi);
        Exit(StrToInt(Trim(Str)));
      end
      else if Pos('  object', Str) > 0 then
        Break;
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

function TFGUIDesigner.Open(const FileName, JavaFilename: string): TFGUIForm;
var
  FilStream: TFileStream;
  BinStream: TMemoryStream;
  JavaForm: TFEditForm;
  Reader: TReader;
  PPI: Integer;
  NewName: string;

  function GetName: string;
  var
    StringList: TStringList;
    Index, Num: Integer;
    Str: string;
  begin
    Result := '';
    StringList := TStringList.Create;
    try
      StringList.Sorted := True;
      for var I := 0 to Screen.FormCount - 1 do
        if StartsWith(Screen.Forms[I].Name, 'FGUIForm') then
          StringList.Add(Screen.Forms[I].Name);
      if not StringList.Find('FGUIForm', Index) then
        Exit('FGUIForm');
      Num := 1;
      repeat
        Str := 'FGUIForm_' + IntToStr(Num);
        if not StringList.Find(Str, Index) then
          Exit(Str);
        Inc(Num);
      until False;
    finally
      FreeAndNil(StringList);
    end;
  end;

begin
  Result := nil;
  if not FileExists(FileName) then
    Exit;
  PPI := GetPixelsPerInchOfFile(FileName);
  if PPI = 0 then
  begin
    RemovePixelsPerInch0(FileName);
    PPI := 96;
  end;
  RemoveMDIChild(FileName);
  RemoveFrameType(FileName);
  JavaForm := TFEditForm(FJava.GetTDIWindow(JavaFilename));
  FObjectGenerator.Partner := JavaForm;
  FilStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  BinStream := TMemoryStream.Create;
  Reader := TReader.Create(BinStream, 4096);
  Reader.OnError := ErrorMethod;
  Reader.OnFindMethod := FindMethod;
  Reader.OnCreateComponent := CreateComponent;
  try
    try
      NewName := getName;
      ObjectTextToResource(FilStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      BinStream.ReadResHeader;
      if JavaForm.FrameType = 8 then
        DesignForm := TFXGUIForm(FJava.FormFactory(fkFXGUI))
      else
        DesignForm := TFGUIForm(FJava.FormFactory(fkGUI));
      DesignForm.Partner := JavaForm;
      Reader.ReadRootComponent(DesignForm);
      if JavaForm.FrameType = 8 then
        (DesignForm as TFXGUIForm).Open(FileName, '')
      else
        DesignForm.Open(FileName, '');
      DesignForm.Name := NewName;
      if DesignForm.Monitor.PixelsPerInch > PPI then
        DesignForm.Scale(DesignForm.Monitor.PixelsPerInch, PPI);
      ScaleImages;
      DesignForm.Invalidate; // to paint correct image sizes
      DesignForm.EnsureOnDesktop;
      ChangeTo(DesignForm);
      if not JavaForm.Editor.Modified and not DesignForm.Modified and
        TFEditForm(DesignForm.Partner).Modified then
        TFEditForm(DesignForm.Partner).Modified := False;
      Result := DesignForm;
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(Reader);
    FreeAndNil(FilStream);
    FreeAndNil(BinStream);
    FObjectInspector.RefreshCBObjects;
  end;
  if Assigned(DesignForm) and DesignForm.ReadOnly then
    ELDesigner.LockAll([lmNoMove, lmNoResize, lmNoDelete, lmNoInsertIn,
      lmNoCopy]);
end;

procedure TFGUIDesigner.FindMethod(Reader: TReader; const MethodName: string;
  var Address: Pointer; var Error: Boolean);
begin
  Address := nil;
  Error := False;
end;

procedure TFGUIDesigner.ErrorMethod(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := (Pos('ShowName', Message) + Pos('Number', Message) > 0);
  Handled := True; // read form anyway
end;

procedure TFGUIDesigner.CreateComponent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
  if ComponentClass = TSpinEdit then
    Component := TSpinEdit.Create(Reader.Owner);
end;

procedure TFGUIDesigner.MICloseClick(Sender: TObject);
begin
  TForm(ELDesigner.DesignControl).Close;
end;

procedure TFGUIDesigner.MIConfigurationClick(Sender: TObject);
begin
  FConfiguration.OpenAndShowPage('GUI designer');
end;

procedure TFGUIDesigner.ELDesignerDesignFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  if Assigned(ELDesigner.DesignControl) then
    TForm(ELDesigner.DesignControl).Close;
end;

procedure TFGUIDesigner.ELDesignerControlDeleting(Sender: TObject;
  SelectedControls: TELDesignerSelectedControls);
begin
  FObjectGenerator.Partner := GetEditForm;
  var Idx := SelectedControls.Count - 1;
  while Idx > -1 do
  begin
    // don't delete Panels of a JTabbedPane
    if not(SelectedControls[Idx].Parent is TJTabbedPane) then
      DeleteComponent(SelectedControls[Idx]);
    Dec(Idx);
  end;
  FObjectInspector.SetSelectedObject(nil);
  UpdateState(Modified);
end;

procedure TFGUIDesigner.MIToSourceClick(Sender: TObject);
begin
  TFEditForm(ELDesigner.DesignControl).Partner.BringToFront;
  ELDesignerDblClick(Self);
end;

procedure TFGUIDesigner.MIZoomInClick(Sender: TObject);
begin
  DesignForm.Zooming(True);
end;

procedure TFGUIDesigner.MIZoomOutClick(Sender: TObject);
begin
  DesignForm.Zooming(False);
end;

procedure TFGUIDesigner.Zooming(Delta: Integer);
begin
  if Delta = 1 then
    DesignForm.Zooming(True)
  else
    DesignForm.Zooming(False);
end;

procedure TFGUIDesigner.SetComponentValues(DesignForm: TFGUIForm;
  Control: TControl);
var
  NewComp, Comp2: TControl;
  Str, Attr, AllowedEvents, AllowedAttributes, ForbiddenAttributes: string;
  Font1, Font2: TFont;
  Border1, Border2: TBorder;
  PropInfos1, PropInfos2: PPropList;
  Count: Integer;
  PropEditor1: TELPropEditor;
  PropEditor2: TELPropEditor;
  AEditorClass: TELPropEditorClass;
  APropertyInspector: TELCustomPropertyInspector;
  Partner: TFEditForm;
begin
  Partner := DesignForm.Partner as TFEditForm;
  Partner.EnsureStartEnd;
  Partner.Editor.BeginUpdate;
  NewComp := Control;
  Str := NewComp.ClassName;
  Comp2 := TControlClass(Classes.GetClass(Str)).Create(DesignForm);
  if NewComp is TJEComponent then
  begin
    AllowedEvents := (NewComp as TJEComponent).GetEvents(3);
    AllowedAttributes := (NewComp as TJEComponent).GetAttributes(3);
    ForbiddenAttributes := ULink.DelphiBounds;
  end;

  Count := GetPropList(NewComp.ClassInfo, tkAny, nil);
  GetMem(PropInfos1, Count * SizeOf(PPropInfo));
  GetMem(PropInfos2, Count * SizeOf(PPropInfo));
  GetPropList(NewComp.ClassInfo, tkAny, PropInfos1);
  GetPropList(Comp2.ClassInfo, tkAny, PropInfos2);
  APropertyInspector := TELCustomPropertyInspector.Create(nil);
  try
    for var I := 0 to Count - 1 do
    begin
      Attr := string(PropInfos1[I].Name);
      if IsLower(Attr[1]) and (Attr <> NewComp.Name) then // the events
        SetStrProp(NewComp, Attr, '')
      else if (Pos(' ' + Attr + ' ', ForbiddenAttributes) = 0) and
        (Pos('|' + Attr + '|', AllowedAttributes) > 0) then
      begin
        AEditorClass := APropertyInspector.GetEditorClass(NewComp,
          PropInfos1[I]);
        if Assigned(AEditorClass) then
        begin
          PropEditor1 := AEditorClass.Create2(NewComp, PropInfos1[I]);
          PropEditor2 := AEditorClass.Create2(Comp2, PropInfos2[I]);
          try
            if PropEditor2.Value <> PropEditor1.Value then
              SetAttributForComponent(Attr, PropEditor1.Value,
                string(PropEditor1.PropTypeInfo.Name), NewComp)
            else if (PropEditor1.PropTypeInfo.Name = 'TFont') then
            begin
              Font1 := (PropEditor1 as TELFontPropEditor).getFont;
              Font2 := (PropEditor2 as TELFontPropEditor).getFont;
              if (Font1.Name <> Font2.Name) or (Font1.Size <> Font2.Size) or
                (Font1.Style <> Font2.Style) then
                SetAttributForComponent(Attr, PropEditor1.Value,
                  string(PropEditor1.PropTypeInfo.Name), NewComp);
            end
            else if PropEditor1.PropTypeInfo.Name = 'TStrings' then
            begin
              if (PropEditor1 as TELStringsPropEditor).getText <>
                (PropEditor2 as TELStringsPropEditor).getText then
                SetAttributForComponent(Attr, PropEditor1.Value,
                  string(PropEditor1.PropTypeInfo.Name), NewComp);
            end
            else if PropEditor1.PropTypeInfo.Name = 'TBorder' then
            begin
              Border1 := (PropEditor1 as TELBorderPropEditor).getBorder;
              Border2 := (PropEditor2 as TELBorderPropEditor).getBorder;
              if (Border1.BorderType <> Border2.BorderType) or
                (Border1.LineColor <> Border2.LineColor) or
                (Border1.LineThickness <> Border2.LineThickness) or
                (Border1.LineRounded <> Border2.LineRounded) or
                (Border1.EtchHighlightColor <> Border2.EtchHighlightColor) or
                (Border1.EtchShadowColor <> Border2.EtchShadowColor) or
                (Border1.Etchtype <> Border2.Etchtype) or
                (Border1.BevelHighlightColor <> Border2.BevelHighlightColor) or
                (Border1.BevelHighlightColor <> Border2.BevelHighlightColor) or
                (Border1.Beveltype <> Border2.Beveltype) or
                (Border1.Title <> Border2.Title) or
                (Border1.MatteColor <> Border2.MatteColor) or
                Assigned(Border1.Font) and Assigned(Border2.Font) and
                ((Border1.Font.Name <> Border2.Font.Name) or
                (Border1.Font.Size <> Border2.Font.Size) or
                (Border1.Font.Style <> Border2.Font.Style)) or
                (Border1.MatteColor <> Border2.MatteColor) or
                (Border1.MatteLeft <> Border2.MatteLeft) or
                (Border1.MatteBottom <> Border2.MatteBottom) or
                (Border1.MatteBottom <> Border2.MatteBottom) then
                SetAttributForComponent(Attr, PropEditor1.Value,
                  string(PropEditor1.PropTypeInfo.Name), NewComp);
            end;
          finally
            FreeAndNil(PropEditor1);
            FreeAndNil(PropEditor2);
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropInfos1, Count * SizeOf(PPropInfo));
    FreeMem(PropInfos2, Count * SizeOf(PPropInfo));
    FreeAndNil(APropertyInspector);
    FreeAndNil(Comp2);
  end;
  Partner.Editor.EndUpdate;
end;

procedure TFGUIDesigner.SetAttributForComponent(const Attr, Value, Typ: string;
  Control: TControl);
begin
  var AValue := Delphi2JavaValues(Value);
  if Control is TFGUIForm then
    (Control as TFGUIForm).SetAttribute(Attr, AValue, Typ)
  else if Control is TFXGUIForm then
    (Control as TFXGUIForm).SetAttribute(Attr, AValue, Typ)
  else if Control is TJEComponent then
    (Control as TJEComponent).SetAttribute(Attr, AValue, Typ);
end;

procedure TFGUIDesigner.MISnapToGridClick(Sender: TObject);
begin
  ELDesigner.SelectedControls.AlignToGrid;
  UpdateState(Modified);
  MISnapToGrid.Checked := not MISnapToGrid.Checked;
  ELDesigner.SnapToGrid := MISnapToGrid.Checked;
end;

procedure TFGUIDesigner.ELDesignerDblClick(Sender: TObject);
var
  Str: string;
begin
  if ELDesigner.SelectedControls.Count = 1 then
  begin
    if Abs(ELDesigner.SelectedControls[0].Tag) = NativeInt(4) then
      Str := 'public void ' + ELDesigner.SelectedControls[0].Name +
        '_ActionPerformed'
    else if ELDesigner.SelectedControls[0].Tag = NativeInt(105) then
      Str := 'public void ' + ELDesigner.SelectedControls[0].Name + '_Action'
    else
      Str := ELDesigner.SelectedControls[0].Name;
    GetEditForm.Go_To(Str);
    GUIDesignerTimer.Enabled := True;
  end;
  UpdateState(not Modified);
end;

procedure TFGUIDesigner.GUIDesignerTimerTimer(Sender: TObject);
begin
  GUIDesignerTimer.Enabled := False;
  var
  EditForm := GetEditForm;
  if Assigned(EditForm) then
  begin
    EditForm.BringToFront;
    EditForm.Enter(Self);
    if EditForm.Editor.CanFocus then
      EditForm.Editor.SetFocus;
  end;
end;

procedure TFGUIDesigner.CutClick(Sender: TObject);
begin
  ELDesigner.Cut;
  UpdateState(Modified);
end;

procedure TFGUIDesigner.CopyClick(Sender: TObject);
begin
  ELDesigner.Copy;
  UpdateState(not Modified);
end;

procedure TFGUIDesigner.PasteClick(Sender: TObject);
var
  PasteInContainer: Boolean;
  Container, Control: TControl;
  WinControl: TWinControl;
  EditForm: TFEditForm;
begin
  Container := nil;
  PasteInContainer := False;
  if ELDesigner.SelectedControls.Count > 0 then
  begin
    Container := ELDesigner.SelectedControls[0];
    if Abs(Integer(Container.Tag)) in [12, 121] then
      PasteInContainer := True;
  end;
  ELDesigner.Paste;
  EditForm := GetEditForm;
  EditForm.Editor.BeginUpdate;

  for var I := 0 to ELDesigner.SelectedControls.Count - 1 do
  begin
    Control := ELDesigner.SelectedControls[I];
    if PasteInContainer then
    begin
      if Control.Left + Control.Width > Container.Width then
        Control.Left := Container.Width - Control.Width;
      if Control.Top + Control.Height > Container.Height then
        Control.Top := Container.Height - Control.Height;
    end;
    FObjectInspector.SetSelectedObject(Control);
    GComponentNrToInsert := Integer(ELDesigner.SelectedControls[I].Tag);
    InsertComponent(Control);
    SetComponentValues(DesignForm, Control);
    WinControl := TWinControl(Control);
    for var J := 0 to WinControl.ControlCount - 1 do
    begin
      FObjectInspector.SetSelectedObject(WinControl.Controls[J]);
      GComponentNrToInsert := Integer(WinControl.Controls[J].Tag);
      InsertComponent(WinControl.Controls[J]);
      SetComponentValues(DesignForm, WinControl.Controls[J]);
    end;
  end;
  FObjectInspector.RefreshCBObjects;
  UpdateState(Modified);
  EditForm.Editor.EndUpdate;

  // otherwise the source code isn't shown
  EditForm.Editor.ExecuteCommand(cecRight, #0, nil);
end;

procedure TFGUIDesigner.InsertComponent(Control: TControl);
begin
  var
  Partner := GetEditForm;
  Partner.Editor.BeginUpdate;
  Partner.Editor.LockBuildStructure := True;  // ToDo
  Partner.EnsureStartEnd;
  if Control is TJEComponent then
    (Control as TJEComponent).NewControl;
  if ELDesigner.SnapToGrid then
    ELDesigner.SelectedControls.AlignToGrid;
  Partner.Editor.LockBuildStructure := False;
  Partner.NeedsParsing := True;
  Partner.Editor.EndUpdate;
  FObjectInspector.UpdateItems;
end;

procedure TFGUIDesigner.MoveComponent(Control: TControl);
begin
  if Control.Parent is TJTabbedPane then
    Exit;
  if Control is TJEComponent then
    (Control as TJEComponent).SetPositionAndSize;
end;

procedure TFGUIDesigner.DeleteComponent(Control: TControl);

  procedure DeleteAComponent(Control: TControl);
  begin
    if Control is TJEComponent then
    begin
      (Control as TJEComponent).DeleteComponent;
      var
      Typ := (Control as TJEComponent).JavaType;
      var
      Idx := FObjectInspector.CBObjects.Items.IndexOf
        (Control.Name + ': ' + Typ);
      FObjectInspector.CBObjects.Items.Delete(Idx);
    end;
  end;

  procedure DeleteAllComponents(Control: TControl);
  begin
    if Control is TWinControl then
      for var I := 0 to (Control as TWinControl).ControlCount - 1 do
        DeleteAllComponents((Control as TWinControl).Controls[I]);
    DeleteAComponent(Control);
  end;

begin
  var
  Partner := GetEditForm;
  Partner.Editor.BeginUpdate;
  Partner.EnsureStartEnd;
  DeleteAllComponents(Control);
  Partner.ParseSourceCode(True);
  Partner.Editor.EndUpdate;
end;

procedure TFGUIDesigner.ELDesignerGetUniqueName(Sender: TObject;
  const ABaseName: string; var AUniqueName: string);
var
  Num: Integer;
  Str, Base: string;
begin
  Base := LowerUpper(ABaseName);
  Str := UUtils.Right(Base, -1);
  while (Pos(Str, '0123456789') > 0) and (Length(Base) > 2) do
  begin
    Base := UUtils.Left(Base, Length(Base) - 1);
    Str := UUtils.Right(Base, -1);
  end;
  Num := 1;
  repeat
    AUniqueName := Base + IntToStr(Num);
    Inc(Num);
  until ELDesigner.IsUniqueName(AUniqueName);
end;

function TFGUIDesigner.GetEditForm: TFEditForm;
begin
  if Assigned(ELDesigner) and Assigned(ELDesigner.DesignControl) then
    Result := TFEditForm(TFForm(ELDesigner.DesignControl).Partner)
  else
    Result := nil;
end;

procedure TFGUIDesigner.DoPanelCanvas(TagNr: Integer; const FileName: string);
begin
  TBAWTSwing(TagNr);
  ULink.PanelCanvasType := ChangeFileExt(FileName, '');
end;

function TFGUIDesigner.GetPath: string;
begin
  if Assigned(TFGUIForm(ELDesigner.DesignControl)) then
    Result := ExtractFilePath(TFGUIForm(ELDesigner.DesignControl).Pathname)
  else
    Result := '';
end;

procedure TFGUIDesigner.ScaleImages;
begin
  vilControls1315.SetSize(DesignForm.PPIScale(13), DesignForm.PPIScale(15));
  vilControls1616.SetSize(DesignForm.PPIScale(16), DesignForm.PPIScale(16));
  vilControls1618.SetSize(DesignForm.PPIScale(16), DesignForm.PPIScale(18));
  vilControls21616.SetSize(DesignForm.PPIScale(16), DesignForm.PPIScale(16));
  vilPagination.SetSize(DesignForm.PPIScale(125), DesignForm.PPIScale(45));
  vilTurtles.SetSize(DesignForm.PPIScale(21), DesignForm.PPIScale(26));
  vilFXTurtle.SetSize(DesignForm.PPIScale(26), DesignForm.PPIScale(21));
end;

procedure TFGUIDesigner.ChangeStyle;
begin
  if FConfiguration.IsDark then
    PopupMenu.Images := vilGUIDesignerDark
  else
    PopupMenu.Images := vilGuiDesignerLight;
end;

{ TMyDragObject }

function TMyDragObject.GetDragImages: TDragImageList;
begin
  Result := FDragImages;
end;

constructor TMyDragObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FDragImages := TDragImageList.Create(AControl);
  AlwaysShowDragImages := True;

  var
  Bitmap := TBitmap.Create;
  Bitmap.Width := AControl.Width;
  Bitmap.Height := AControl.Height;
  if AControl is TWinControl then
    (AControl as TWinControl).PaintTo(Bitmap.Canvas, 0, 0);
  FDragImages.Width := Bitmap.Width;
  FDragImages.Height := Bitmap.Height;
  FDragImages.Add(Bitmap, nil);
  FDragImages.DragHotspot := Point(Bitmap.Width, Bitmap.Height);
  FreeAndNil(Bitmap);
end;

destructor TMyDragObject.Destroy;
begin
  FreeAndNil(FDragImages);
  inherited Destroy;
end;

initialization

RegisterClasses(ClassArray);

end.
