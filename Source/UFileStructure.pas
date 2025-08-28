unit UFileStructure;

interface

uses
  Classes,
  Graphics,
  Controls,
  Forms,
  ComCtrls,
  Menus,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  TB2Item,
  SpTBXItem,
  UBaseForm,
  UDockForm;

type

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
    FLocked: Boolean;
    FLockShowSelected: Boolean;
    FMyForm: TFForm;
    FWindowOpened: Boolean;
    procedure OpenWindow;
    function DifferentItems(Items: TTreeNodes): Boolean;
    procedure NavigateToNodeElement(Node: TTreeNode);
  public
    procedure InitWithItems(Items: TTreeNodes; Form: TFForm);
    procedure SaveWindow;
    procedure Clear;
    procedure ShowIt;
    procedure HideIt;
    procedure ChangeHideShow;
    procedure SetFont(AFont: TFont);
    procedure ChangeStyle;
    procedure ShowEditorCodeElement;
    procedure ShowSelected;
    property MyForm: TFForm read FMyForm write FMyForm;
  end;

var
  FFileStructure: TFFileStructure;

implementation

{$R *.dfm}

uses
  Windows,
  SysUtils,
  Messages,
  Math,
  SynEditTypes,
  JvGnugettext,
  UJava,
  UEditorForm,
  UUMLForm,
  UConfiguration,
  UUtils,
  UGUIDesigner;

{ --- TFFileStructure ---------------------------------------------------------- }

{ If TVFiletructure has ParentFont true and the default font with size 9
  then during dpi change the font doesn't change, remains small. But if it has
  another font size, then during dpi change it's size is scaled.

  But the dependency does not exist if ParentFont is false.
}

procedure TFFileStructure.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  Visible := False;
  FLocked := False;
  FLockShowSelected := False;
  MyForm := nil;
  FWindowOpened := False;
end;

procedure TFFileStructure.FormShow(Sender: TObject);
begin
  inherited;
  if not FWindowOpened then
  begin
    OpenWindow;
    FWindowOpened := True;
  end;
  FJava.ActiveTool := 16;
  if CanFocus then
    SetFocus;
end;

procedure TFFileStructure.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Action := caHide;
  FJava.ActiveTool := -1;
end;

procedure TFFileStructure.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TFFileStructure.FormMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool := 16;
  FJava.UpdateMenuItems(Self);
end;

procedure TFFileStructure.InitWithItems(Items: TTreeNodes; Form: TFForm);
begin
  MyForm := Form;
  if DifferentItems(Items) then
  begin
    FJava.Lock.Acquire;
    TVFileStructure.Items.BeginUpdate;
    for var I := 0 to TVFileStructure.Items.Count - 1 do
      FreeAndNil(TInteger(TVFileStructure.Items[I].Data));
    TVFileStructure.Items.Clear;
    TVFileStructure.Items.Assign(Items);
    for var I := 0 to TVFileStructure.Items.Count - 1 do
      TVFileStructure.Items[I].Data :=
        TInteger.Create(TInteger(Items[I].Data).Int);
    TVFileStructure.FullExpand;
    TVFileStructure.HideSelection := False;
    TVFileStructure.Items.EndUpdate;
    // if Form.CanFocus then Form.SetFocus; sets DesignButton
    FJava.Lock.Release;
  end;
end;

function TFFileStructure.DifferentItems(Items: TTreeNodes): Boolean;
begin
  if TVFileStructure.Items.Count <> Items.Count then
    Exit(True);
  for var I := 0 to TVFileStructure.Items.Count - 1 do
    if TVFileStructure.Items[I].Text <> Items[I].Text then
      Exit(True);
  for var I := 0 to TVFileStructure.Items.Count - 1 do
    TInteger(TVFileStructure.Items[I].Data).Int :=
      TInteger(Items[I].Data).Int;
  Result := False;
end;

procedure TFFileStructure.Clear;
begin
  if Assigned(TVFileStructure) then
  begin
    TVFileStructure.Items.BeginUpdate;
    for var I := TVFileStructure.Items.Count - 1 downto 0 do
      FreeAndNil(TInteger(TVFileStructure.Items[I].Data));
    TVFileStructure.Items.Clear;
    TVFileStructure.Items.EndUpdate;
  end;
end;

procedure TFFileStructure.SaveWindow;
begin
  FConfiguration.WriteBoolU('FileStructure', 'Visible', Visible);
  FConfiguration.WriteBoolU('FileStructure', 'Floating', Floating);
  if Floating then
  begin
    FConfiguration.WriteIntegerU('FileStructure', 'UndockLeft',
      PPIUnScale(Left));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockTop', PPIUnScale(Top));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockWidth',
      PPIUnScale(Width));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockHeight',
      PPIUnScale(Height));
  end
  else
  begin
    // defined in Dockableform
    FConfiguration.WriteIntegerU('FileStructure', 'UndockLeft',
      PPIUnScale(UndockLeft));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockTop',
      PPIUnScale(UndockTop));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockWidth',
      PPIUnScale(UndockWidth));
    FConfiguration.WriteIntegerU('FileStructure', 'UndockHeight',
      PPIUnScale(UndockHeight));
  end;
  FConfiguration.WriteStringU('FileStructure', 'Fontname', Font.Name);
  FConfiguration.WriteIntegerU('FileStructure', 'Fontsize',
    PPIUnScale(Font.Size));
end;

procedure TFFileStructure.OpenWindow;
begin
  UndockWidth := PPIScale(FConfiguration.ReadIntegerU('FileStructure',
    'UndockWidth', 200));
  UndockHeight := PPIScale(FConfiguration.ReadIntegerU('FileStructure',
    'UndockHeight', 200));
  UndockLeft := PPIScale(FConfiguration.ReadIntegerU('FileStructure',
    'UndockLeft', 400));
  UndockTop := PPIScale(FConfiguration.ReadIntegerU('FileStructure',
    'UndockTop', 100));
  UndockLeft := Min(UndockLeft, Screen.DesktopWidth - 50);
  UndockTop := Min(UndockTop, Screen.DesktopHeight - 50);
  ManualFloat(Rect(UndockLeft, UndockTop, UndockLeft + UndockWidth,
    UndockTop + UndockHeight));
  Font.Name := FConfiguration.ReadStringU('FileStructure', 'Fontname',
    'Segoe UI');
  Font.Size := PPIScale(FConfiguration.ReadIntegerU('FileStructure',
    'Fontsize', 10));
end;

procedure TFFileStructure.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options := [];
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
var
  Node: TTreeNode;
begin
  if FLocked then
  begin
    FLocked := False;
    Exit;
  end;
  with TVFileStructure.ScreenToClient(Mouse.CursorPos) do
    Node := TVFileStructure.GetNodeAt(X, Y);
  if not Assigned(Node) then
    Exit;
  NavigateToNodeElement(TVFileStructure.Selected);
  var
  Attri := Node.Text;
  Delete(Attri, 1, Pos(' ', Attri));
  if (Pos('(', Attri) = 0) and Assigned(FGUIDesigner.ELDesigner) then
  // methods can't be selected
    FGUIDesigner.ELDesigner.SelectControl(Attri);
end;

procedure TFFileStructure.TVFileStructureKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = Char(VK_RETURN) then
    NavigateToNodeElement(TVFileStructure.Selected);
end;

procedure TFFileStructure.ShowEditorCodeElement;
var
  Line, Num: Integer;

  function GetNum(Line: Integer): Integer;
  begin
    for var I := 0 to TVFileStructure.Items.Count - 1 do
    begin
      var AInteger := TInteger(TVFileStructure.Items[I].Data);
      if AInteger.Int = Line then
        Exit(I);
      if (AInteger.Int > Line) and (I > 0) then
        Exit(I - 1);
    end;
    Result := TVFileStructure.Items.Count - 1;
  end;

begin
  if not Assigned(MyForm) then
    Exit;

  if (MyForm.FormTag = 1) and Assigned(TFEditForm(MyForm).Editor) then
  begin
    Line := TFEditForm(MyForm).Editor.CaretY;
    Num := GetNum(Line);
    if (Num > -1) and not TVFileStructure.Items[Num].Selected then
      TVFileStructure.Items[Num].Selected := True;
    ShowSelected;
  end;
end;

procedure TFFileStructure.NavigateToNodeElement(Node: TTreeNode);
var
  ANodeLine: Integer;
  Line, AClassname, ANodeText: string;
  EditForm: TFEditForm;
  AForm: TFForm;
  IsWrapping: Boolean;
  Files: TStringList;
  CNode: TTreeNode;

  function GetEditForm(AClassname: string): TFEditForm;
  begin
    for var I := 0 to Files.Count - 1 do
      if Pos(AClassname, Files[I]) > 0 then
      begin
        FJava.SwitchWindowWithSearch(Files[I]);
        if FJava.WindowOpened(Files[I], AForm) then
          Exit(TFEditForm(AForm));
      end;
    Result := nil;
  end;

begin
  if not Assigned(Node) then
    Exit;

  ANodeLine := TInteger(Node.Data).Int;
  ANodeText := Node.Text;
  if MyForm.FormTag = 1 then
    EditForm := TFEditForm(MyForm)
  else
    EditForm := nil;

  if MyForm.FormTag = 2 then  // UML window
  begin
    FLocked := True;
    Files := TFUMLForm(MyForm).MainModul.Model.ModelRoot.Files;
    CNode := Node;
    while CNode.Parent <> nil do
      CNode := CNode.Parent;
    AClassname := WithoutGeneric(CNode.Text);
    Delete(AClassname, 1, LastDelimiter('.', AClassname));
    AClassname := '\' + AClassname + '.java';
    EditForm := GetEditForm(AClassname);
    if not Assigned(EditForm) then
      Exit;
  end;

  IsWrapping := EditForm.Editor.WordWrap;
  if IsWrapping then
    EditForm.SBWordWrapClick(nil);
  Line := EditForm.Editor.Lines[ANodeLine - 1];
  FLockShowSelected := True;
  EditForm.Editor.TopLine := ANodeLine;
  FLockShowSelected := False;
  EditForm.Editor.CaretXY := BufferCoord(Max(1, Pos(ANodeText, Line)), ANodeLine);
  if CanActuallyFocus(EditForm.Editor) then
    EditForm.Editor.SetFocus;
  if IsWrapping then
    EditForm.SBWordWrapClick(nil);
end;

procedure TFFileStructure.TVFileStructureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    PMFileStructure.Popup(X + (TTreeView(Sender)).ClientOrigin.X - 40,
      Y + TTreeView(Sender).ClientOrigin.Y - 5);
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
  if Visible then
    HideIt
  else
    ShowIt;
end;

procedure TFFileStructure.SetFont(AFont: TFont);
begin
  Font.Assign(AFont);
end;

procedure TFFileStructure.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    TVFileStructure.Images := vilFileStructureDark;
    PMFileStructure.Images := vilFileStructureDark;
  end
  else
  begin
    TVFileStructure.Images := vilFileStructureLight;
    PMFileStructure.Images := vilFileStructureLight;
  end;
end;

procedure TFFileStructure.ShowSelected;
begin
  if Assigned(TVFileStructure) and Assigned(TVFileStructure.Selected) and
    not TVFileStructure.Selected.IsVisible and not FLockShowSelected then
  begin
    LockFormUpdate(Self);
    TVFileStructure.Selected.MakeVisible;
    SendMessage(TVFileStructure.Handle, WM_HSCROLL, SB_PAGELEFT, 0);
    UnlockFormUpdate(Self);
  end;
end;


initialization

FFileStructure := nil;

end.
