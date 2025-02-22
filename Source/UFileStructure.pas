unit UFileStructure;

interface

uses
  Classes, Graphics, Controls, Forms, ComCtrls, Menus, UBaseForm, UDockForm,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  SVGIconImageCollection, TB2Item, SpTBXItem;

type
  TInteger = class
  public
    i: integer;
    constructor create(aI: Integer);
  end;

  TFFileStructure = class(TDockableForm)
    TVFileStructure: TTreeView;
    PMFileStructure: TSpTBXPopupMenu;
    MIFont: TSpTBXItem;
    MIDefaulLayout: TSpTBXItem;
    MIClose: TSpTBXItem;
    icFileStructure: TSVGIconImageCollection;
    vilFileStructureLight: TVirtualImageList;
    vilFileStructureDark: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure MIDefaulLayoutClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure TVFileStructureClick(Sender: TObject);
    procedure TVFileStructureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure TVFileStructureKeyPress(Sender: TObject; var Key: Char);
  private
    locked: boolean;
    LockShowSelected: boolean;
    WindowOpened: boolean;
    procedure OpenWindow;
    function DifferentItems(Items: TTreeNodes): boolean;
  public
    myForm: TFForm;
    procedure Init(Items: TTreeNodes; Form: TFForm);
    procedure SaveWindow;
    procedure Clear;
    procedure ShowIt;
    procedure HideIt;
    procedure ChangeHideShow;
    procedure SetFont(aFont: TFont);
    procedure ChangeStyle;
    procedure ShowEditorCodeElement;
    procedure NavigateToNodeElement(Node: TTreeNode;
                ForceToMiddle : Boolean = True; Activate : Boolean = True);
    procedure ShowSelected;
  end;

var
  FFileStructure: TFFileStructure;

implementation

{$R *.dfm}

uses Windows, SysUtils, Messages, Math, Dialogs, SynEditTypes, JvGnugettext,
     UJava, UEditorForm, UUMLForm, UConfiguration, UUtils, UGuiDesigner;

constructor TInteger.create(aI: Integer);
begin
  inherited create;
  self.i:= aI;
end;

{--- TFFileStructure ----------------------------------------------------------}

{ If TVFiletructure has ParentFont true and the default font with size 9
  then during dpi change the font doesn't change, remains small. But if it has
  another font size, then during dpi change it's size is scaled.

  But the dependency does not exist if ParentFont is false.
}

procedure TFFileStructure.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  visible:= false;
  locked:= false;
  LockShowSelected:= false;
  myForm:= nil;
  WindowOpened:= false;
end;

procedure TFFileStructure.FormShow(Sender: TObject);
begin
  inherited;
  if not WindowOpened then begin  // form is now scaled
    OpenWindow;
    WindowOpened:= true;
  end;
  FJava.ActiveTool:= 16;
  if canFocus then
    SetFocus;
end;

procedure TFFileStructure.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action:= caHide;
  FJava.ActiveTool:= -1;
end;

procedure TFFileStructure.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TFFileStructure.FormMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool:= 16;
  FJava.UpdateMenuItems(Self);
end;

procedure TFFileStructure.init(Items: TTreeNodes; Form: TFForm);
begin
  myForm:= Form;
  if DifferentItems(Items) then begin
    FJava.Lock.Acquire;
    ChangeStyle;
    TVFileStructure.Items.BeginUpdate;
    for var i:= 0 to TVFileStructure.Items.Count - 1 do
      FreeAndNil(TVFileStructure.Items[i].Data);
    TVFileStructure.Items.Clear;
    TVFileStructure.Items.Assign(Items);
    for var i:= 0 to TVFileStructure.Items.Count - 1 do
      TVFileStructure.Items[i].Data:= TInteger.create(TInteger(Items[i].Data).i);
    TVFileStructure.FullExpand;
    TVFileStructure.HideSelection:= false;
    TVFileStructure.Items.EndUpdate;
    // if Form.CanFocus then Form.SetFocus; sets DesignButton
    FJava.Lock.Release;
  end;
end;

function TFFileStructure.DifferentItems(Items: TTreeNodes): boolean;
  var i: integer;
begin
  if TVFileStructure.Items.Count <> Items.Count then
    Exit(true);
  for i:= 0 to TVFileStructure.Items.Count - 1 do
    if TVFileStructure.Items[i].Text <> Items[i].Text then
      Exit(true);
  for i:= 0 to TVFileStructure.Items.Count - 1 do
    TInteger(TVFileStructure.Items[i].Data).i:=
      TInteger(Items[i].Data).i;
  Result:= false;
end;

procedure TFFileStructure.Clear;
begin
  if assigned(TVFileStructure) then begin
    TVFileStructure.Items.BeginUpdate;
    for var i:= TVFileStructure.Items.Count - 1 downto 0 do
      FreeAndNil(TVFileStructure.Items[i].Data);
    TVFileStructure.Items.Clear;
    TVFileStructure.Items.EndUpdate;
  end;
end;

procedure TFFileStructure.SaveWindow;
begin
  FConfiguration.WriteBoolU('FileStructure', 'Visible', Visible);
  FConfiguration.WriteBoolU('FileStructure', 'Floating', Floating);
  if Floating then begin
    FConfiguration.WriteIntegerU('FileStructure', 'UndockLeft', PPIUnScale(Left));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockTop',  PPIUnScale(Top));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockWidth', PPIUnscale(Width));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockHeight', PPIUnScale(Height));
  end else begin
    // defined in Dockableform
    FConfiguration.WriteIntegerU('FileStructure', 'UndockLeft', PPIUnScale(UndockLeft));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockTop',  PPIUnScale(UndockTop));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockWidth', PPIUnScale(UndockWidth));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockHeight', PPIUnScale(UndockHeight));
  end;
  FConfiguration.WriteStringU('FileStructure', 'Fontname', Font.Name);
  FConfiguration.WriteIntegerU('FileStructure', 'Fontsize', PPIUnscale(Font.Size));
end;

procedure TFFileStructure.OpenWindow;
begin
  UndockWidth:= PPIScale(FConfiguration.ReadIntegerU('FileStructure', 'UndockWidth', 200));
  UndockHeight:= PPIScale(FConfiguration.ReadIntegerU('FileStructure', 'UndockHeight', 200));
  UndockLeft:= PPIScale(FConfiguration.ReadIntegerU('FileStructure', 'UndockLeft', 400));
  UndockTop:= PPIScale(FConfiguration.ReadIntegerU('FileStructure', 'UndockTop', 100));
  UndockLeft:= min(UndockLeft, Screen.DesktopWidth - 50);
  UndockTop:= min(UndockTop, Screen.DesktopHeight - 50);
  ManualFloat(Rect(UnDockLeft, UnDockTop, UnDockLeft + UnDockWidth, UnDockTop + UnDockHeight));
  Font.Name:= FConfiguration.ReadStringU('FileStructure', 'Fontname', 'Segoe UI');
  Font.Size:= PPIScale(FConfiguration.ReadIntegerU('FileStructure', 'Fontsize', 10));
end;

procedure TFFileStructure.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options:= [];
  if FJava.FDFont.Execute then
    Font.Assign(FJava.FDFont.Font);
end;

procedure TFFileStructure.MIDefaulLayoutClick(Sender: TObject);
begin
  FJava.MIDefaultLayoutClick(Self);
end;

procedure TFFileStructure.MICloseClick(Sender: TObject);
begin
  HideIt;
end;

procedure TFFileStructure.TVFileStructureClick(Sender: TObject);
  var Node: TTreeNode;
begin
  if locked then begin
    locked:= false;
    exit;
  end;
  with TVFileStructure.ScreenToClient(Mouse.CursorPos) do
    Node:= TVFileStructure.GetNodeAt(X, Y);
  if not Assigned(Node) then
    Exit;
  NavigateToNodeElement(TVFileStructure.Selected);
  var attri:= Node.Text;
  Delete(attri, 1, Pos(' ', attri));
  if (Pos('(', attri) = 0) and assigned(FGuiDesigner.ELDesigner) then // methods can't be selected
    FGuiDesigner.ELDesigner.SelectControl(attri);
end;

procedure TFFileStructure.TVFileStructureKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_Return) then
    NavigateToNodeElement(TVFileStructure.Selected);
end;

procedure TFFileStructure.ShowEditorCodeElement;
  var Line, i, Nr: integer; aInteger: TInteger;
begin
  if Assigned(myForm) and (myForm is TFEditForm) and assigned((myForm as TFEditForm).Editor) then begin
    Line:= (myForm as TFEditForm).Editor.CaretY;
    Nr:= -1;
    for i:= 0 to TVFileStructure.Items.Count - 1 do begin
      aInteger:= TInteger(TVFileStructure.Items[i].Data);
      if aInteger.i = Line then
        Nr:= i
      else if (aInteger.i > Line) and (i > 0) then
        Nr:= i - 1;
      if Nr > - 1 then
        break;
    end;
    if Nr = -1 then
      Nr:= TVFileStructure.Items.Count - 1;
    if (Nr > -1) and not TVFileStructure.Items[Nr].Selected then
       TVFileStructure.Items[Nr].Selected:= true;
    ShowSelected;
  end;
end;

procedure TFFileStructure.NavigateToNodeElement(Node: TTreeNode;
            ForceToMiddle : Boolean = True; Activate : Boolean = True);
var
  i, aNodeLine: integer;
  Line, aClassname, aNodeText: string;
  EditForm: TFEditForm;
  aForm: TFForm;
  isWrapping: boolean;
  Files: TStringList;
  cNode: TTreeNode;
begin
  EditForm:= nil;
  if Assigned(Node) then begin
    aNodeLine:= TInteger(Node.Data).i;
    aNodeText:= Node.Text;
  end else
    exit;

  if myForm.FormTag = 1 then
    EditForm:= myForm as TFEditForm
  else if myForm.FormTag = 2 then begin // UML window
    locked:= true;
    Files:= (myForm as TFUMLForm).MainModul.Model.ModelRoot.Files;
    cNode:= Node;
    while cNode.Parent <> nil do
      cNode:= cNode.Parent;
    aClassname:= WithoutGeneric(cNode.Text);
    delete(aClassname, 1, LastDelimiter('.', aClassname));
    aClassname:= '\' + aClassname + '.java';
    i:= 0;
    while i < Files.Count do begin
      if Pos(aClassname, Files[i]) > 0 then begin
        FJava.SwitchWindowWithSearch(Files[i]);
        if FJava.WindowOpened(Files[i], aForm) then begin
          EditForm:= aForm as TFEditForm;
          break;
        end;
      end;
      inc(i);
    end;
    if EditForm = nil then
      exit;
  end;

  isWrapping:= EditForm.Editor.WordWrap;
  if isWrapping then
    EditForm.SBWordWrapClick(nil);
  with EditForm.Editor do begin
    Line:= Lines[aNodeLine - 1];
    LockShowSelected:= true;
    Topline:= aNodeLine;
    // EnsureCursorPosVisibleEx(ForceToMiddle);
    LockShowSelected:= false;
    CaretXY:= BufferCoord(max(1, Pos(aNodeText, Line)), aNodeLine);
  end;
  if Activate and CanActuallyFocus(EditForm.Editor) then
    EditForm.Editor.SetFocus;
  if isWrapping then
    EditForm.SBWordWrapClick(nil);
end;

procedure TFFileStructure.TVFileStructureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbRight then
    PMFileStructure.Popup(X+(Sender as TTreeView).ClientOrigin.X-40, Y+(Sender as TTreeView).ClientOrigin.Y-5);
end;

procedure TFFileStructure.ShowIt;
begin
  FJava.ShowDockableForm(Self);
end;

procedure TFFileStructure.HideIt;
begin
  Close;
end;

procedure TFFileStructure.ChangeHideShow;
begin
  if Visible
    then HideIt
    else ShowIt;
end;

procedure TFFileStructure.SetFont(aFont: TFont);
begin
  Font.Assign(aFont);
end;

procedure TFFileStructure.ChangeStyle;
begin
  if FConfiguration.isDark then begin
    TVFileStructure.Images:= vilFileStructureDark;
    PMFileStructure.Images:= vilFileStructureDark;
  end else begin
    TVFileStructure.Images:= vilFileStructureLight;
    PMFileStructure.Images:= vilFileStructureLight;
  end;
end;

procedure TFFileStructure.ShowSelected;
begin
  if assigned(TVFileStructure) and assigned(TVFileStructure.Selected) and
    not TVFileStructure.Selected.isVisible and not LockShowSelected
  then begin
    LockFormUpdate(Self);
    TVFileStructure.Selected.MakeVisible;
    SendMessage(TVFileStructure.Handle, WM_HSCROLL, SB_PAGELEFT, 0);
    UnlockFormUpdate(Self);
  end;
end;

Initialization
  FFileStructure:= nil;

end.
