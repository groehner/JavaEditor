unit UGUIDesigner;

interface

uses
  Windows, Classes, Controls, Forms, Menus, Messages, ExtCtrls, ElDsgnr,
  System.ImageList, Vcl.ImgList, UGUIForm, UEditorForm, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, SVGIconImageCollection, TB2Item, SpTBXItem,
  SVGIconImage;

type
  TFGUIDesigner = class(TForm)
    PopupMenu: TSpTBXPopupMenu;
    MIClose: TSpTBXItem;
    MIToSource: TSpTBXItem;
    MIForeground: TSpTBXItem;
    MIBackground: TSpTBXItem;
    N1: TSpTBXSeparatorItem;
    MISnapToGrid: TSpTBXItem;
    N2: TSpTBXSeparatorItem;
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
    N4: TSpTBXSeparatorItem;
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
    procedure ELDragDrop(Sender, ASource, ATarget: TObject; AX, AY: Integer);
    procedure ELDragOver(Sender, ASource, ATarget: TObject; AX, AY: Integer;
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
    ComponentToInsert: TControlClass;
    function getParent(ATarget: TObject; AX, AY: Integer): TWinControl;
    procedure ComponentToBackground(aPartner: TFEditForm; Control: TControl);
    procedure ComponentToForeground(aPartner: TFEditForm; Control: TControl);
    function GetPixelsPerInchOfFile(Filename: string): integer;
    procedure RemovePixelsPerInch0(Filename: string);
    procedure RemoveMDIChild(Filename: string);
    procedure RemoveFrameType(Filename: string);
  public
    ELDesigner: TELDesigner;
    DesignForm: TFGuiForm;
    procedure Save(const Filename: string; aForm: TFGuiForm);
    function Open(const Filename, JavaFilename: string): TFGuiForm;
    procedure FindMethod(Reader: TReader; const MethodName: string;
      var Address: Pointer; var Error: Boolean);
    procedure ErrorMethod(Reader: TReader; const Message: string;
      var Handled: Boolean);
    procedure CreateComponent(Reader: TReader; ComponentClass: TComponentClass;
      var Component: TComponent);
    procedure MIAWTSwing(Tag: Integer);
    procedure TBAWTSwing(Tag: Integer);
    procedure TBJavaFX(Tag: Integer);
    procedure ChangeTo(aForm: TFGuiForm);
    function GetEditForm: TFEditForm;
    procedure UpdateState(Modified: Boolean);
    procedure DoPanelCanvas(TagNr: Integer; const Filename: string);
    function getPath: string;
    function Tag2Class(Tag: Integer): TControlClass;
    procedure Zooming(delta: Integer);
    procedure SetComponentValues(DesignForm: TFGuiForm; Control: TControl);
    procedure SetAttributForComponent(Attr, Value, Typ: string;
      Control: TControl);
    procedure ScaleImages;
    procedure ChangeStyle;
    procedure InsertComponent(Control: TControl);
    procedure MoveComponent(Control: TControl);
    procedure DeleteComponent(Control: TControl);
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
  SysUtils,Graphics, ActnList, Mask, Clipbrd,
  Comctrls, Dialogs, Grids, Buttons, Tabs, Ddeman, Mplayer, Olectnrs, Tabnotbk,
  {$WARNINGS OFF} FileCtrl, Outline, {$WARNINGS ON}  Spin, UJComponents,
  System.Types, UITypes, TypInfo, StdCtrls, ELPropInsp,
  JvGnugettext,
  UJava, UBaseForm, UObjectGenerator, ULink, UConfiguration, UUtils,
  UJEComponents, UJUtilities, UObjectInspector,

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
  UFXPolygon, UFXLine, UFXText, UFXBezier, UFXSVGPath, UFXTurtle, UFXGuiForm;

{$R *.dfm}

type
  TClassArray = array [1 .. 226] of TPersistentClass;

const
  Modified = true;

  ClassArray: TClassArray = (TBitmap, TGraphic, TOutlineNode, TGraphicsObject,
    TBrush, THeaderSection, TCanvas, THeaderSections, TPen, TIcon, TPicture,
    TIconOptions, TCollection, TCollectionItem, TStatusPanel, TStatusPanels,
    TClipboard, TControlScrollBar, TListColumn, TStringList, TListItem,
    TStrings, TListItems, TMetafile, TMetafileCanvas, TTreeNode, TFont,
    TParaAttributes, TTreeNodes, TApplication, TDDEServerItem, TPanel,
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
    TDDEClientConv, TOutline, TDDEClientItem, TPageControl, TUpDown,
    TDDEServerConv, TPaintBox, TSpinEdit, TSplitter, TActionList, TAction,
    TFGuiForm,

    TALabel, TJLabel, TATextField, TJTextField, TANumberField, TJNumberField,
    TATextArea, TJTextArea, TAButton, TJButton, TACheckbox, TJCheckbox,
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
    TFXPasswordField, TFXChoiceBox, TFXHyperlink, TFXButtongroup, TFXHTMLEditor,
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
  with ELDesigner do begin
    ShowingHints := [htControl, htSize, htMove, htInsert];
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

procedure TFGUIDesigner.ChangeTo(aForm: TFGuiForm);
begin
  if assigned(ELDesigner) then begin
    if (ELDesigner.DesignControl <> aForm) or not ELDesigner.Active then begin
      ELDesigner.Active:= false;
      ELDesigner.DesignControl:= aForm;
      DesignForm:= aForm;
      if assigned(aForm) then
        ELDesigner.Active:= true;
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
  for var i := 0 to ELDesigner.SelectedControls.Count - 1 do
    MoveComponent(ELDesigner.SelectedControls.Items[i]);
  UpdateState(Modified);
end;

procedure TFGUIDesigner.UpdateState(Modified: Boolean);
begin
  with FJava do begin
    SetEnabledMI(MICut, ELDesigner.CanCut);
    SetEnabledMI(MICopy, true);
    SetEnabledMI(MICopyNormal, true);
    SetEnabledMI(MIPaste, ELDesigner.CanPaste);
  end;
  if Modified and assigned(ELDesigner.DesignControl) then
    TFForm(ELDesigner.DesignControl).Modified := true;
end;

procedure TFGUIDesigner.PopupMenuPopup(Sender: TObject);
begin
  PopupMenu.Images.SetSize(DesignForm.PPIScale(16), DesignForm.PPIScale(16));
  SetEnabledMI(MICut, ELDesigner.CanCut);
  SetEnabledMI(MICopy, ELDesigner.CanCopy);
  SetEnabledMI(MIPaste, ELDesigner.CanPaste);
  SetEnabledMI(MIAlign, ELDesigner.SelectedControls.Count >= 2);
  MISnapToGrid.Checked := ELDesigner.SnapToGrid;
  var en := (ELDesigner.SelectedControls.Count > 1) or
    ((ELDesigner.SelectedControls.Count = 1) and
    (ELDesigner.SelectedControls[0].ClassName <> 'TFGUIForm') and
    (ELDesigner.SelectedControls[0].ClassName <> 'TFXGuiForm'));
  SetEnabledMI(MIDelete, en);
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
  for var i := 0 to ELDesigner.SelectedControls.Count - 1 do
    ComponentToForeground(GetEditForm, ELDesigner.SelectedControls.Items[i]);
end;

procedure TFGUIDesigner.MIBackgroundClick(Sender: TObject);
begin
  for var i := 0 to ELDesigner.SelectedControls.Count - 1 do
    ComponentToBackground(GetEditForm, ELDesigner.SelectedControls.Items[i]);
end;

procedure TFGUIDesigner.ComponentToBackground(aPartner: TFEditForm;
  Control: TControl);
begin
  if assigned(Control) then
  begin
    Control.SendToBack;
    aPartner.ToBackground(Control);
  end;
end;

procedure TFGUIDesigner.ComponentToForeground(aPartner: TFEditForm;
  Control: TControl);
begin
  if assigned(Control) then
  begin
    Control.BringToFront;
    aPartner.ToForeground(Control);
  end;
end;

procedure TFGUIDesigner.MIAlignClick(Sender: TObject);
var
  AHorzAlign, AVertAlign: TELDesignerAlignType;
  Tag: Integer;
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
    AControlClass := ComponentToInsert;
end;

function TFGUIDesigner.getParent(ATarget: TObject; AX, AY: Integer)
  : TWinControl;
var
  w, h: Integer;
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
  if assigned(WinControl) then
  begin
    w := WinControl.ClientWidth;
    h := WinControl.ClientHeight;
  end
  else
  begin
    w := -1;
    h := -1;
  end;
  if ComponentNrToInsert = 34 then
    if assigned(WinControl) and not(WinControl is TJPlayground) then
      Exit(nil);
  if (AX >= 0) and (AY >= 0) and (AX <= w) and (AY <= h) then
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
  AX, AY: Integer);
  var LInsertingControl: TControl;
      LName: string;
begin
  if csAcceptsControls in (ATarget as TControl).ControlStyle then begin
    LInsertingControl := ASource as TControl;
    ELDesigner.getUniqueName((LInsertingControl as TJEComponent).Tag2JavaType(LInsertingControl.Tag), LName);
    LInsertingControl.Name:= LName;
    LInsertingControl.Parent := ATarget as TWinControl;
    LInsertingControl.SetBounds(AX-LInsertingControl.Width, AY-LInsertingControl.Height,
                                   LInsertingControl.Width, LInsertingControl.Height);
    TControlEx(LInsertingControl).Font.Size:= DesignForm.FontSize;
    ELDesigner.SelectedControls.ClearExcept(LInsertingControl);
    ELDesignerControlInserted(nil);
    UpdateState(Modified);
  end;
end;

procedure TFGUIDesigner.ELDragOver(Sender, ASource, ATarget: TObject;
  AX, AY: Integer; AState: TDragState; var Accept: Boolean);
begin
  Accept := assigned(getParent(ATarget, AX, AY));
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
      Result := TACheckbox;
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
      Result := TJCheckbox;
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
      Result := TFXButtongroup;
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
  x, y, dx, dy: Integer;
  ctrl: TWinControl;
  myMouse: TPoint;
begin
  TBAWTSwing(Tag); // set Component to insert
  if assigned(ComponentToInsert) then
  begin
    GetCursorPos(myMouse);
    if ELDesigner.SnapToGrid then
    begin
      dx := random(5) * ELDesigner.Grid.XStep;
      dy := random(5) * ELDesigner.Grid.YStep;
    end
    else
    begin
      dx := random(40);
      dy := random(40);
    end;
    x := FJava.Left + ELDesigner.DesignControl.Left + 18 + dx;
    y := FJava.Top + ELDesigner.DesignControl.Top + 170 + dy;
    SetCursorPos(x, y);
    ctrl := FindVCLWindow(Mouse.CursorPos);
    if ctrl <> nil then
    begin
      SendMessage(ctrl.Handle, WM_LBUTTONDOWN, MK_LBUTTON, 0);
      SendMessage(ctrl.Handle, WM_LBUTTONUP, 0, 0);
    end;
    SetCursorPos(myMouse.x, myMouse.y);
  end;
end;

procedure TFGUIDesigner.TBAWTSwing(Tag: Integer);
begin
  ComponentToInsert := nil;
  var Form := GetEditForm;
  if assigned(Form) and (Form.FrameType in [2..8]) then begin
    ULink.ComponentNrToInsert := Tag;
    ComponentToInsert := Tag2Class(Tag);
    ScaleImages;
  end;
end;

procedure TFGUIDesigner.TBJavaFX(Tag: Integer);
begin
  ComponentToInsert := nil;
  var Form := GetEditForm;
  if assigned(Form) and (Form.FrameType = 8) then begin
    ULink.ComponentNrToInsert := Tag;
    ComponentToInsert := Tag2Class(Tag);
    ScaleImages;
  end;
end;

procedure TFGUIDesigner.ELDesignerControlInserted(Sender: TObject);
begin
  var Control:= ELDesigner.SelectedControls[0];
  if (Control is TJTurtle) and not (Control.Parent is TJPlayground) then begin
    FreeAndNil(Control);
    Exit;
  end;

  Control.Tag := ComponentNrToInsert;
  Control.HelpKeyword := PanelCanvasType;
  Control.HelpType := htContext;
  Control.Hint := Control.Name;
  Control.ShowHint := true;

  FObjectInspector.RefreshCBObjects;
  FObjectInspector.SetSelectedObject(Control);
  ComponentToInsert := nil;
  InsertComponent(Control);
  UpdateState(Modified);
  FJava.ResetToolbars;
end;

procedure TFGUIDesigner.Save(const Filename: string; aForm: TFGuiForm);
begin
  var BinStream := TMemoryStream.Create;
  try
    try
      var FilStream := TFileStream.Create(Filename, fmCreate or fmShareExclusive);
      try
        BinStream.WriteComponent(aForm);
        BinStream.Seek(0, soFromBeginning);
        ObjectBinaryToText(BinStream, FilStream);
      finally
        FreeAndNil(FilStream);
      end;
    except
      on e: exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(BinStream)
  end;
  if assigned(ELDesigner.DesignControl) then
    TFForm(ELDesigner.DesignControl).Modified := false;
end;

procedure TFGUIDesigner.RemovePixelsPerInch0(Filename: string);
begin
  var SL:= TStringList.Create;
  try
    SL.LoadFromFile(Filename);
    for var i:= 0 to SL.Count -1 do begin
      var s:= SL[i];
      var p:= Pos('PixelsPerInch', s);
      if p > 0 then begin
        p:= Pos('=', s);
        delete(s, 1, p);
        p:= StrToInt(trim(s));
        if p = 0 then begin
          SL.Delete(i);
          SL.SaveToFile(Filename);
          break;
        end
      end else if Pos('  object', s) > 0 then
        break;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TFGUIDesigner.RemoveMDIChild(Filename: string);
begin
  var SL:= TStringList.Create;
  try
    SL.LoadFromFile(Filename);
    for var i:= 0 to SL.Count -1 do begin
      var s:= SL[i];
      var p:= Pos('FormStyle = fsMDIChild', s);
      if p > 0 then begin
        SL.Delete(i);
        SL.SaveToFile(Filename);
        Break;
      end else if Pos('  object', s) > 0 then
        Break;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TFGUIDesigner.RemoveFrameType(Filename: string);
begin
  var SL:= TStringList.Create;
  try
    SL.LoadFromFile(Filename);
    for var i:= 0 to SL.Count -1 do begin
      var s:= SL[i];
      var p:= Pos('FrameType =', s);
      if p > 0 then begin
        SL.Delete(i);
        SL.SaveToFile(Filename);
        Break;
      end else if Pos('  object', s) > 0 then
        Break;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

function TFGUIDesigner.GetPixelsPerInchOfFile(Filename: string): integer;
begin
  Result:= 96;
  var SL:= TStringList.Create;
  try
    SL.LoadFromFile(Filename);
    for var i:= 0 to SL.Count -1 do begin
      var s:= SL[i];
      var p:= Pos('PixelsPerInch', s);
      if p > 0 then begin
        p:= Pos('=', s);
        delete(s, 1, p);
        Exit(StrToInt(trim(s)));
      end else if Pos('  object', s) > 0 then
        break;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

function TFGUIDesigner.Open(const Filename, JavaFilename: string): TFGuiForm;
var
  FilStream: TFileStream;
  BinStream: TMemoryStream;
  JavaForm: TFEditForm;
  Reader: TReader;
  PPI: integer;
  NewName: string;

  function getName: string;
    var SL: TStringList; i, index, Nr: integer; s: string;
  begin
    SL:= TStringList.Create;
    try
      SL.Sorted:= true;
      for i:= 0 to screen.FormCount -1 do
         if startsWith(Screen.Forms[I].Name, 'FGUIForm') then
           SL.Add(Screen.Forms[i].Name);
      if not SL.Find('FGUIForm', index) then exit('FGUIForm');
      Nr:= 1;
      repeat
        s:= 'FGUIForm_' + IntToStr(Nr);
        if not SL.Find(s, index) then exit(s);
        Inc(Nr);
      until false;
    finally
      FreeAndNil(SL);
    end;
  end;

begin
  Result := nil;
  if not FileExists(Filename) then
    Exit;
  PPI:= GetPixelsPerInchOfFile(Filename);
  if PPI = 0 then begin
    RemovePixelsPerInch0(Filename);
    PPI:= 96;
  end;
  RemoveMDIChild(Filename);
  RemoveFrameType(Filename);
  JavaForm := TFEditForm(FJava.getTDIWindow(JavaFilename));
  FObjectGenerator.Partner := JavaForm;
  FilStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  BinStream := TMemoryStream.Create;
  Reader := TReader.Create(BinStream, 4096);
  Reader.OnError := ErrorMethod;
  Reader.OnFindMethod := FindMethod;
  Reader.OnCreateComponent := CreateComponent;
  try
    try
      NewName:= getName;
      ObjectTextToResource(FilStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      BinStream.ReadResHeader;
      if JavaForm.FrameType = 8
        then DesignForm := TFXGUIForm(FJava.FormFactory(fkFXGUI))
        else DesignForm := TFGUIForm(FJava.FormFactory(fkGUI));
      DesignForm.Partner := JavaForm;
      Reader.ReadRootComponent(DesignForm);
      if JavaForm.FrameType = 8
        then (DesignForm as TFXGUIForm).Open(Filename, '')
        else DesignForm.Open(Filename, '');
      DesignForm.Name:= NewName;
      if DesignForm.Monitor.PixelsPerInch > PPI then
        DesignForm.Scale(DesignForm.Monitor.PixelsPerInch, PPI);
      ScaleImages;
      DesignForm.Invalidate; // to paint correct image sizes
      DesignForm.EnsureOnDesktop;
      ChangeTo(DesignForm);
      if not JavaForm.Editor.Modified and not DesignForm.Modified and
        TFEditForm(DesignForm.Partner).Modified then
        TFEditForm(DesignForm.Partner).Modified := false;
      Result:= DesignForm;
    except
      on e: exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(Reader);
    FreeAndNil(FilStream);
    FreeAndNil(BinStream);
    FObjectInspector.RefreshCBObjects;
  end;
  if assigned(DesignForm) and DesignForm.ReadOnly then
    ELDesigner.LockAll([lmNoMove, lmNoResize, lmNoDelete, lmNoInsertIn, lmNoCopy]);
end;

procedure TFGUIDesigner.FindMethod(Reader: TReader; const MethodName: string;
  var Address: Pointer; var Error: Boolean);
begin
  Address := nil;
  Error := false;
end;

procedure TFGUIDesigner.ErrorMethod(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := (Pos('ShowName', Message) + Pos('Number', Message) > 0);
  Handled := true; // read form anyway
end;

procedure TFGUIDesigner.CreateComponent(Reader: TReader;
  ComponentClass: TComponentClass; var Component: TComponent);
begin
  if ComponentClass = TSpinEdit then
    Component := TSpinEdit.Create(Reader.Owner)
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
  if assigned(ELDesigner.DesignControl) then
    TForm(ELDesigner.DesignControl).Close;
end;

procedure TFGUIDesigner.ELDesignerControlDeleting(Sender: TObject;
  SelectedControls: TELDesignerSelectedControls);
var
  i: Integer;
begin
  FObjectGenerator.Partner := GetEditForm;
  i := SelectedControls.Count - 1;
  while i > -1 do
  begin
    // don't delete Panels of a JTabbedPane
    if not(SelectedControls.Items[i].Parent is TJTabbedPane) then
      DeleteComponent(SelectedControls.Items[i]);
    dec(i);
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
  DesignForm.Zooming(true);
end;

procedure TFGUIDesigner.MIZoomOutClick(Sender: TObject);
begin
  DesignForm.Zooming(false);
end;

procedure TFGUIDesigner.Zooming(delta: Integer);
begin
  if delta = 1 then
    DesignForm.Zooming(true)
  else
    DesignForm.Zooming(false);
end;

procedure TFGUIDesigner.SetComponentValues(DesignForm: TFGuiForm;
  Control: TControl);
var
  NewComp, Comp2: TControl;
  S, Attr, AllowedEvents, AllowedAttributes, ForbiddenAttributes: string;
  f1, f2: TFont;
  b1, b2: TBorder;
  PropInfos1, PropInfos2: PPropList;
  Count, i: Integer;
  PropEditor1: TELPropEditor;
  PropEditor2: TELPropEditor;
  aEditorClass: TELPropEditorClass;
  aPropertyInspector: TELCustomPropertyInspector;
  Partner: TFEditForm;
begin
  Partner := DesignForm.Partner as TFEditForm;
  Partner.EnsureStartEnd;
  Partner.Editor.BeginUpdate;
  NewComp := Control;
  S := NewComp.ClassName;
  Comp2 := TControlClass(Classes.GetClass(S)).Create(DesignForm);
  if NewComp is TJEComponent then
  begin
    AllowedEvents := (NewComp as TJEComponent).getEvents(3);
    AllowedAttributes := (NewComp as TJEComponent).getAttributes(3);
    ForbiddenAttributes := ULink.DelphiBounds;
  end;

  Count := GetPropList(NewComp.ClassInfo, tkAny, nil);
  GetMem(PropInfos1, Count * SizeOf(PPropInfo));
  GetMem(PropInfos2, Count * SizeOf(PPropInfo));
  try
    GetPropList(NewComp.ClassInfo, tkAny, PropInfos1);
    GetPropList(Comp2.ClassInfo, tkAny, PropInfos2);
    aPropertyInspector := TELCustomPropertyInspector.Create(nil);
    for i := 0 to Count - 1 do
    begin
      Attr := string(PropInfos1[i].Name);
      if IsLower(Attr[1]) and (Attr <> NewComp.Name) then // the events
        SetStrProp(NewComp, Attr, '')
      else if (Pos(' ' + Attr + ' ', ForbiddenAttributes) = 0) and
        (Pos('|' + Attr + '|', AllowedAttributes) > 0) then
      begin
        aEditorClass := aPropertyInspector.GetEditorClass(NewComp,
          PropInfos1[i]);
        if assigned(aEditorClass) then
        begin
          PropEditor1 := aEditorClass.Create2(NewComp, PropInfos1[i]);
          PropEditor2 := aEditorClass.Create2(Comp2, PropInfos2[i]);
          try
            if PropEditor2.Value <> PropEditor1.Value then
              SetAttributForComponent(Attr, PropEditor1.Value,
                string(PropEditor1.PropTypeInfo.Name), NewComp)
            else if (PropEditor1.PropTypeInfo.Name = 'TFont') then
            begin
              f1 := (PropEditor1 as TELFontPropEditor).getFont;
              f2 := (PropEditor2 as TELFontPropEditor).getFont;
              if (f1.Name <> f2.Name) or (f1.Size <> f2.Size) or
                (f1.Style <> f2.Style) then
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
              b1 := (PropEditor1 as TELBorderPropEditor).getBorder;
              b2 := (PropEditor2 as TELBorderPropEditor).getBorder;
              if (b1.BorderType <> b2.BorderType) or
                (b1.LineColor <> b2.LineColor) or
                (b1.LineThickness <> b2.LineThickness) or
                (b1.LineRounded <> b2.LineRounded) or
                (b1.EtchHighlightColor <> b2.EtchHighlightColor) or
                (b1.EtchShadowColor <> b2.EtchShadowColor) or
                (b1.Etchtype <> b2.Etchtype) or
                (b1.BevelHighlightColor <> b2.BevelHighlightColor) or
                (b1.BevelHighlightColor <> b2.BevelHighlightColor) or
                (b1.Beveltype <> b2.Beveltype) or (b1.Title <> b2.Title) or
                (b1.MatteColor <> b2.MatteColor) or assigned(b1.Font) and
                assigned(b2.Font) and
                ((b1.Font.Name <> b2.Font.Name) or
                (b1.Font.Size <> b2.Font.Size) or
                (b1.Font.Style <> b2.Font.Style)) or
                (b1.MatteColor <> b2.MatteColor) or
                (b1.MatteLeft <> b2.MatteLeft) or
                (b1.MatteBottom <> b2.MatteBottom) or
                (b1.MatteBottom <> b2.MatteBottom) then
                SetAttributForComponent(Attr, PropEditor1.Value,
                  string(PropEditor1.PropTypeInfo.Name), NewComp);
            end;
          finally
            FreeAndNil(PropEditor1);
            FreeAndNil(PropEditor2);
          end;
        end
      end;
    end
  finally
    FreeMem(PropInfos1, Count * SizeOf(PPropInfo));
    FreeMem(PropInfos2, Count * SizeOf(PPropInfo));
    FreeAndNil(aPropertyInspector);
    FreeAndNil(Comp2);
  end;
  Partner.Editor.EndUpdate;
end;

procedure TFGUIDesigner.SetAttributForComponent(Attr, Value, Typ: string;
  Control: TControl);
begin
  Value := Delphi2JavaValues(Value);
  if Control is TFGuiForm then
    (Control as TFGuiForm).setAttribute(Attr, Value, Typ)
  else if Control is TFXGUIForm then
    (Control as TFXGUIForm).setAttribute(Attr, Value, Typ)
  else if Control is TJEComponent then
    (Control as TJEComponent).setAttribute(Attr, Value, Typ)
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
  S: string;
begin
  if ELDesigner.SelectedControls.Count = 1 then
  begin
    if Abs(ELDesigner.SelectedControls[0].Tag) = 4 then
      S := 'public void ' + ELDesigner.SelectedControls[0].Name +
        '_ActionPerformed'
    else if ELDesigner.SelectedControls[0].Tag = 105 then
      S := 'public void ' + ELDesigner.SelectedControls[0].Name + '_Action'
    else
      S := ELDesigner.SelectedControls[0].Name;
    GetEditForm.Go_To(S);
    GUIDesignerTimer.Enabled := true;
  end;
  UpdateState(not Modified);
end;

procedure TFGUIDesigner.GUIDesignerTimerTimer(Sender: TObject);
begin
  GUIDesignerTimer.Enabled := false;
  var EditForm := GetEditForm;
  if assigned(EditForm) then begin
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
  i, j: Integer;
  PasteInContainer: Boolean;
  Container, Control: TControl;
  WinControl: TWinControl;
  EditForm: TFEditForm;
begin
  Container := nil;
  PasteInContainer := false;
  if ELDesigner.SelectedControls.Count > 0 then
  begin
    Container := ELDesigner.SelectedControls[0];
    if Abs(Container.Tag) in [12, 121] then
      PasteInContainer := true;
  end;
  ELDesigner.Paste;
  EditForm := GetEditForm;
  EditForm.Editor.BeginUpdate;

  for i := 0 to ELDesigner.SelectedControls.Count - 1 do
  begin
    Control := ELDesigner.SelectedControls[i];
    if PasteInContainer then
    begin
      if Control.Left + Control.Width > Container.Width then
        Control.Left := Container.Width - Control.Width;
      if Control.Top + Control.Height > Container.Height then
        Control.Top := Container.Height - Control.Height;
    end;
    FObjectInspector.SetSelectedObject(Control);
    ComponentNrToInsert := ELDesigner.SelectedControls.Items[i].Tag;
    InsertComponent(Control);
    SetComponentValues(DesignForm, Control);
    WinControl := TWinControl(Control);
    for j := 0 to WinControl.ControlCount - 1 do
    begin
      FObjectInspector.SetSelectedObject(WinControl.Controls[j]);
      ComponentNrToInsert := WinControl.Controls[j].Tag;
      InsertComponent(WinControl.Controls[j]);
      SetComponentValues(DesignForm, WinControl.Controls[j]);
    end;
  end;
  FObjectInspector.RefreshCBObjects;
  UpdateState(Modified);
  EditForm.Editor.EndUpdate;

  // otherwise the source code isn't shown
  EditForm.Editor.ExecuteCommand(ecRight, #0, nil);
end;

procedure TFGUIDesigner.InsertComponent(Control: TControl);
begin
  var
  Partner:= GetEditForm;
  Partner.Editor.BeginUpdate;
  Partner.Editor.LockBuildStructure := true;
  Partner.EnsureStartEnd;
  if Control is TJEComponent then
    (Control as TJEComponent).NewControl;
  if ELDesigner.SnapToGrid then
    ELDesigner.SelectedControls.AlignToGrid;
  Partner.Editor.LockBuildStructure := false;
  Partner.NeedsParsing := true;
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
    if Control is TJEComponent then begin
      (Control as TJEComponent).DeleteComponent;
      var Typ := (Control as TJEComponent).JavaType;
      var i := FObjectInspector.CBObjects.Items.IndexOf(Control.Name + ': ' + Typ);
      FObjectInspector.CBObjects.Items.Delete(i);
    end;
  end;

  procedure DeleteAllComponents(Control: TControl);
  begin
    if Control is TWinControl then
      for var i := 0 to (Control as TWinControl).ControlCount - 1 do
        DeleteAllComponents((Control as TWinControl).Controls[i]);
    DeleteAComponent(Control);
  end;

begin
  var Partner := GetEditForm;
  Partner.Editor.BeginUpdate;
  Partner.EnsureStartEnd;
  DeleteAllComponents(Control);
  Partner.ParseSourcecode(true);
  Partner.Editor.EndUpdate;
end;

procedure TFGUIDesigner.ELDesignerGetUniqueName(Sender: TObject;
  const ABaseName: string; var AUniqueName: string);
var
  i: Integer;
  S, b: string;
begin
  b := LowerUpper(ABaseName);
  S := UUtils.Right(b, -1);
  while (Pos(S, '0123456789') > 0) and (Length(b) > 2) do
  begin
    b := UUtils.Left(b, Length(b) - 1);
    S := UUtils.Right(b, -1);
  end;
  i := 1;
  repeat
    AUniqueName := b + IntToStr(i);
    inc(i);
  until ELDesigner.IsUniqueName(AUniqueName);
end;

function TFGUIDesigner.GetEditForm: TFEditForm;
begin
  if assigned(ELDesigner) and assigned(ELDesigner.DesignControl) then
    Result := TFEditForm(TFForm(ELDesigner.DesignControl).Partner)
  else
    Result := nil;
end;

procedure TFGUIDesigner.DoPanelCanvas(TagNr: Integer; const Filename: string);
begin
  TBAWTSwing(TagNr);
  ULink.PanelCanvasType := ChangeFileExt(Filename, '');
end;

function TFGUIDesigner.getPath: string;
begin
  if assigned(TFGuiForm(ELDesigner.DesignControl)) then
    Result := ExtractFilepath(TFGuiForm(ELDesigner.DesignControl).Pathname)
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
  if FConfiguration.isDark
    then PopupMenu.Images:= vilGuiDesignerDark
    else PopupMenu.Images:= vilGuiDesignerLight;
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
  AlwaysShowDragImages := true;

  var Bitmap := TBitmap.Create;
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
