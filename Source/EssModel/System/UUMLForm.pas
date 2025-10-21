{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit UUMLForm;

// UMLForm -> FMainModul -> Diagram -> RtfdDiagram -> TessConnectPanel
// -> Model                  -> TAFrameDiagram
// contextmenus in UDiagramFrame

interface

uses
  Classes,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  ComCtrls,
  Vcl.ToolWin,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  UUMLModule,
  UBaseForm;

type
  TFUMLForm = class(TFForm)
    PDiagramPanel: TPanel;
    Panel: TPanel;
    IcSVGIconImageCollection: TSVGIconImageCollection;
    VilToolbarDark: TVirtualImageList;
    PUMLPanel: TPanel;
    TVFileStructure: TTreeView;
    TBUMLToolbar: TToolBar;
    TBClose: TToolButton;
    TBClassNew: TToolButton;
    TBClassOpen: TToolButton;
    TBClassInsert: TToolButton;
    TBShowConnections: TToolButton;
    TBObjectDiagram: TToolButton;
    TBView: TToolButton;
    TBZoomOut: TToolButton;
    TBZoomIn: TToolButton;
    TBComment: TToolButton;
    TBNewLayout: TToolButton;
    TBRefresh: TToolButton;
    TBJavaReset: TToolButton;
    TBRecognizeAssociations: TToolButton;
    VilToolbarLight: TVirtualImageList;
    procedure SBCloseClick(Sender: TObject);
    procedure TBShowConnectionsClick(Sender: TObject);
    procedure TBNewLayoutClick(Sender: TObject);
    procedure TBDiagramFromOpenWindowsClick(Sender: TObject);
    procedure TBClassNewClick(Sender: TObject);
    procedure TBRefreshClick(Sender: TObject);
    procedure TBClassEditorClick(Sender: TObject);
    procedure TBClassOpenClick(Sender: TObject);
    procedure TBClassInsertClick(Sender: TObject);
    procedure PDiagramPanelResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction); override;
    procedure FormDestroy(Sender: TObject);
    procedure TBObjectDiagramClick(Sender: TObject);
    procedure TBJavaResetMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TBCommentClick(Sender: TObject);
    procedure TBZoomOutClick(Sender: TObject);
    procedure TBZoomInClick(Sender: TObject);
    procedure TBViewClick(Sender: TObject);
    procedure TBRecognizeAssociationsClick(Sender: TObject);
  private
    FMainModul: TDMUMLModule;
    procedure SaveAndReload;
    procedure OnPanelModified(Value: Boolean);
    procedure OnSelectionChanged(Sender: TObject);
    procedure OnInteractiveModified(Sender: TObject);
  protected
    procedure UpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(const FileName: string; State: string);
    procedure ConfigureWindow(Sender: TObject);
    procedure Print; override;
    procedure Exchange(File1, File2: string);
    function GetFilesAndPackages(Selected: Boolean): TStringList;
    function GetFileWithMain: string;
    function HasClassWithMain: Boolean;
    function HasClass(const AClassname: string): Boolean;
    function HasEditableClass: Boolean;
    procedure AddClassToProject(const CName: string);
    procedure Hide;
    procedure Show;
    procedure DebugJE2Java;
    procedure CreateTVFileStructure;
    procedure Retranslate;
    procedure CollectClasses(StringList: TStringList); override;
    function GetAllPathnames: TStringList; override;
    function GetAllClassnames: TStringList; override;
    function GetFont: TFont; override;
    procedure ChangeStyle; override;
    function GetFormType: string; override;
    procedure SetOptions; override;
    procedure Save(WithBackup: Boolean); override;
    procedure SaveIn(const Dir: string); override;
    function GetSaveAsName: string; override;
    procedure CopyToClipboard; override;
    procedure SetFont(Font: TFont); override;
    procedure SetFontSize(Delta: Integer); override;
    procedure Enter(Sender: TObject); override;
    procedure DPIChanged; override;
    procedure OpenFiles;
    procedure OpenFolder;
    property MainModul: TDMUMLModule read FMainModul;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  UITypes,
  JvGnugettext,
  UStringRessources,
  UConfiguration,
  UJava,
  UUtils,
  UModelEntity,
  UModel,
  UClassInsert,
  URtfdDiagram,
  UMessages,
  UJavaCommands,
  UFileStructure;

{$R *.DFM}

constructor TFUMLForm.Create(AOwner: TComponent);
begin
  inherited;
  FormTag := 2;
end;

procedure TFUMLForm.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FMainModul := TDMUMLModule.Create(Self, PDiagramPanel);
  SetFont(FConfiguration.UMLFont);
  Modified := False;
  Enter(Self);
  PUMLPanel.Visible := FConfiguration.VisToolbars[3];
  FMainModul.Diagram.SetOnModified(OnPanelModified);
  FMainModul.Diagram.SetOnSelectionChanged(OnSelectionChanged);
  LockEnter := False;
end;

procedure TFUMLForm.Retranslate;
begin
  RetranslateComponent(Self);
  FMainModul.Diagram.Retranslate;
  if Assigned(FClassInsert) then
    RetranslateComponent(FClassInsert);
end;

procedure TFUMLForm.Open(const FileName: string; State: string);
begin
  FMainModul.Diagram.SetInteractive(Self, FileName, OnInteractiveModified);
  if FileName = _(LNGInteractive) then
    Exit;
  ToMainPanel;
  Pathname := FileName;
  if ExtractFilePath(Pathname) = FConfiguration.TempDir then
    Caption := ExtractFileName(Pathname)
  else
    Caption := FileName;
  UpdateState;
  SetState(State);
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
end;

procedure TFUMLForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const CInteractive = 1;
begin
  if (Number <> CInteractive) and not AlreadySavedAs and Modified then
  begin
    FJava.DoSave(Self, True);
    AlreadySavedAs := True;
  end;
  if Assigned(FMainModul) then
    TRtfdDiagram(FMainModul.Diagram).JavaReset;
  FFileStructure.Clear;
  CanClose := True;
end;

procedure TFUMLForm.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  LockEnter := True;
  try
    FMessages.DelInteractive(Pathname);
    if Assigned(TVFileStructure) then
      for var I := TVFileStructure.Items.Count - 1 downto 0 do
        Dispose(TVFileStructure.Items[I].Data);
  finally
    inherited;
  end;
end;

procedure TFUMLForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMainModul);
end;

procedure TFUMLForm.Enter(Sender: TObject);
begin
  if LockEnter then
    Exit;
  LockEnter := True;
  inherited;
  if Visible then
  begin // due to bug, else ActiveForm doesn't change
    if PUMLPanel.Visible and PUMLPanel.CanFocus then
      PUMLPanel.SetFocus;
    if Assigned(FMainModul) and Assigned(FMainModul.Diagram) then
    begin
      var
      APanel := FMainModul.Diagram.GetPanel;
      if Assigned(APanel) and Panel.CanFocus then
        APanel.SetFocus;
      var
      ComJava := TRtfdDiagram(FMainModul.Diagram).GetComJava;
      ComJava.SetActiveComJava(ComJava);
    end;
  end;
  UpdateState;
  SaveAndReload;
  if not FMainModul.Diagram.HasObjects and FMainModul.Diagram.HasAInvalidClass
  then
    FJava.MICompileAllClick(Self);
  LockEnter := False;
end;

procedure TFUMLForm.ConfigureWindow(Sender: TObject);
begin
  Align := alClient;
  FMainModul.Diagram.ShowIcons := FConfiguration.DiShowIcons;
  FMainModul.Diagram.VisibilityFilter := TVisibility(0);
  FMainModul.Diagram.ShowParameter := FConfiguration.DiShowParameter;
  FMainModul.Diagram.SortOrder := FConfiguration.DiSortOrder;
end;

procedure TFUMLForm.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFUMLForm.TBShowConnectionsClick(Sender: TObject);
begin
  if Assigned(FMainModul) and Assigned(FMainModul.Diagram) then
  begin
    FMainModul.Diagram.ShowConnections :=
      (FMainModul.Diagram.ShowConnections + 1) mod 3;
    Modified := True;
    FMainModul.Diagram.GetPanel.Invalidate;
  end;
end;

procedure TFUMLForm.TBViewClick(Sender: TObject);
begin
  if Assigned(FMainModul) and Assigned(FMainModul.Diagram) then
  begin
    FMainModul.Diagram.ShowView := (FMainModul.Diagram.ShowView + 1) mod 3;
    Modified := True;
    FMainModul.Diagram.GetPanel.Invalidate;
  end;
end;

procedure TFUMLForm.TBZoomInClick(Sender: TObject);
begin
  SetFontSize(+1);
end;

procedure TFUMLForm.TBZoomOutClick(Sender: TObject);
begin
  SetFontSize(-1);
end;

procedure TFUMLForm.TBJavaResetMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    TRtfdDiagram(FMainModul.Diagram).JavaReset
  else
    FJava.Restart;
end;

procedure TFUMLForm.TBNewLayoutClick(Sender: TObject);
begin
  FMainModul.DoLayout;
  Modified := True;
end;

procedure TFUMLForm.TBDiagramFromOpenWindowsClick(Sender: TObject);
begin
  FMainModul.ShowAllOpenedFiles;
  Modified := True;
end;

procedure TFUMLForm.SaveAndReload;
begin
  if Pathname <> '' then
  begin
    LockWindow(Self.Handle);
    Save(True);
    FMainModul.LoadUML(Pathname);
    CreateTVFileStructure;
    UnlockWindow;
  end;
end;

procedure TFUMLForm.TBRecognizeAssociationsClick(Sender: TObject);
begin
  FMainModul.Diagram.ResolveAssociations;
end;

procedure TFUMLForm.TBRefreshClick(Sender: TObject);
begin
  SaveAndReload;
end;

procedure TFUMLForm.TBClassNewClick(Sender: TObject);
begin
  FJava.MINewClassClick(Self);
  Modified := True;
end;

procedure TFUMLForm.Save(WithBackup: Boolean);
begin
  FMainModul.SaveUML(Pathname);
  FMainModul.Diagram.InteractiveSetModified(False);
  Modified := False;
end;

procedure TFUMLForm.SaveIn(const Dir: string);
begin
  Pathname := Dir + ExtractFileName(Pathname);
  Save(False);
  FJava.RenameTabAndWindow(Number, Pathname);
  Caption := Pathname;
end;

procedure TFUMLForm.Exchange(File1, File2: string);
var Str, Path, Class1, Class2: string; Posi: Integer;
begin
  Pathname := FConfiguration.HomeDir + 'Default.uml';
  var
  StringList := TStringList.Create;
  try
    try
      Save(True);
      StringList.LoadFromFile(Pathname);
      Path := ExtractFilePath(Pathname);
      Str := StringList.Text;
      File1 := ExtractRelativePath(Path, File1);
      File2 := ExtractRelativePath(Path, File2);
      Posi := Pos(File1, StringList.Text);
      if Posi > 0 then
      begin
        Delete(Str, Posi, Length(File1));
        insert(File2, Str, Posi);
      end;
      Class1 := 'Box:  - ' + ChangeFileExt(ExtractFileName(File1), '');
      Class2 := 'Box:  - ' + ChangeFileExt(ExtractFileName(File2), '');
      Str := ReplaceStr(Str, Class1, Class2);
      StringList.Text := Str;
      StringList.SaveToFile(Pathname);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    StringList.Free;
  end;
  FMainModul.LoadUML(Pathname);
  Modified := True;
end;

function TFUMLForm.GetFormType: string;
begin
  Result := '%U%';
end;

procedure TFUMLForm.Print;
begin
  FMainModul.Print;
end;

procedure TFUMLForm.TBObjectDiagramClick(Sender: TObject);
begin
  if Assigned(FMainModul) and Assigned(FMainModul.Diagram) then
  begin
    var
    AShow := not FMainModul.Diagram.ShowObjectDiagram;
    FMainModul.Diagram.ShowObjectDiagram := AShow;
    Modified := True;
    TBObjectDiagram.Down := AShow;
  end;
end;

procedure TFUMLForm.TBClassEditorClick(Sender: TObject);
begin
  FJava.AUMLForm := Self;
  FMainModul.EditSelectedElement;
  Modified := True;
end;

procedure TFUMLForm.TBClassOpenClick(Sender: TObject);
begin
  if FMainModul.Diagram.ShowObjectDiagram then
  begin
    FMainModul.Diagram.ShowObjectDiagram := False;
    TBObjectDiagram.Down := False;
  end;
  FJava.MIClassOpenClick(Self);
  Modified := True;
end;

procedure TFUMLForm.TBCommentClick(Sender: TObject);
begin
  if Assigned(FMainModul) and Assigned(FMainModul.Diagram) then
    FMainModul.Diagram.AddCommentBoxTo(nil);
end;

procedure TFUMLForm.TBClassInsertClick(Sender: TObject);
begin
  if not Assigned(FClassInsert) then
    FClassInsert := TFClassInsert.Create(FJava);
  FClassInsert.ActivateListView;
  FConfiguration.ShowAlways := True;
  var
  ShowPublicOnly := FConfiguration.ShowPublicOnly;
  if FClassInsert.ShowModal = mrOk then
  begin
    case FClassInsert.RGOptions.ItemIndex of
      0:
        FConfiguration.ShowPublicOnly := True;
      1:
        FConfiguration.ShowPublicOnly := False;
    end;
    var
    StringList := TStringList.Create;
    FClassInsert.GetSelectedFiles(StringList);
    FMainModul.AddToProject(StringList);
    Modified := True;
    FreeAndNil(StringList);
  end;
  FClassInsert.ZipClose;
  FConfiguration.ShowPublicOnly := ShowPublicOnly;
end;

procedure TFUMLForm.PDiagramPanelResize(Sender: TObject);
begin
  if Assigned(FMainModul) then
    FMainModul.Diagram.RecalcPanelSize;
end;

function TFUMLForm.GetSaveAsName: string;
begin
  Result := GetFileWithMain;
  if Result = '' then
    Result := FMainModul.GetUMLFilename
  else
    Result := ChangeFileExt(Result, '.uml');
end;

procedure TFUMLForm.CopyToClipboard;
begin
  FMainModul.Diagram.CopyDiagramToClipboard;
end;

procedure TFUMLForm.SetFont(Font: TFont);
begin
  Self.Font.Assign(Font);
  FMainModul.Diagram.SetFont(Font);
  FMainModul.RefreshDiagram;
end;

function TFUMLForm.GetFont: TFont;
begin
  Result := FMainModul.Diagram.Font;
end;

procedure TFUMLForm.SetFontSize(Delta: Integer);
begin
  var
  AFont := GetFont;
  AFont.Size := AFont.Size + Delta;
  if AFont.Size < 6 then
    AFont.Size := 6;
  SetFont(AFont);
  Font.Size := AFont.Size;
end;

procedure TFUMLForm.UpdateState;
begin
  inherited;
  with FJava do
  begin
    SetEnabledMI(MICopy, True);
    SetEnabledMI(MICopyNormal, True);
    SetEnabledMI(MIPaste, False);
    SetEnabledMI(MICut, False);
  end;
  var
  Editable := HasEditableClass;
  SetEnabledMI(FJava.MIClassEditor, Editable and
    not MyJavaCommands.ProcessRunning);

  Editable := HasClassWithMain;
  SetEnabledTB(FJava.TBRun, Editable);
  SetEnabledMI(FJava.MIRun, Editable and not MyJavaCommands.ProcessRunning);
end;

procedure TFUMLForm.SetOptions;
begin
  SaveAndReload;
  PUMLPanel.Visible := FConfiguration.VisToolbars[3];
end;

function TFUMLForm.GetFilesAndPackages(Selected: Boolean): TStringList;
begin
  Result := TRtfdDiagram(FMainModul.Diagram).GetFilesAndPackages(Selected);
end;

function TFUMLForm.GetAllPathnames: TStringList;
begin
  Result := TRtfdDiagram(FMainModul.Diagram).GetAllPathnames;
end;

function TFUMLForm.GetAllClassnames: TStringList;
begin
  Result := TRtfdDiagram(FMainModul.Diagram).GetAllClassnames;
end;

function TFUMLForm.GetFileWithMain: string;
begin
  if Assigned(FMainModul) and Assigned(FMainModul.Diagram) and
    (FMainModul.Diagram is TRtfdDiagram) then
    Result := TRtfdDiagram(FMainModul.Diagram).GetFileWithMain
  else
    Result := '';
end;

function TFUMLForm.HasClassWithMain: Boolean;
begin
  Result := (GetFileWithMain <> '');
end;

function TFUMLForm.HasClass(const AClassname: string): Boolean;
begin
  if Assigned(FMainModul) then
    Result := TRtfdDiagram(FMainModul.Diagram).HasClass(AClassname)
  else
    Result := False;
end;

procedure TFUMLForm.OnPanelModified(Value: Boolean);
begin
  SetModified(Value);
end;

procedure TFUMLForm.OnSelectionChanged(Sender: TObject);
begin
  UpdateState;
end;

function TFUMLForm.HasEditableClass: Boolean;
begin
  if Assigned(FMainModul) then
    Result := FMainModul.HasEditableClass
  else
    Result := False;
end;

procedure TFUMLForm.AddClassToProject(const CName: string);
begin
  if not Assigned(FClassInsert) then
    FClassInsert := TFClassInsert.Create(FJava);
  var
  Str := FClassInsert.OpenOrExtractClass
    (FMainModul.Model.ModelRoot.Files, CName);
  if Str = '' then
    Str := FConfiguration.SearchClassInClasspath(ExtractClassName(CName),
      Pathname, ExtractPackageName(CName));
  if Str <> '' then
  begin
    FConfiguration.ShowAlways := False;
    FMainModul.AddToProject(Str);
    FConfiguration.ShowAlways := True;
  end;
end;

procedure TFUMLForm.OnInteractiveModified(Sender: TObject);
begin
  Modified := True;
end;

procedure TFUMLForm.CollectClasses(StringList: TStringList);
begin
  var
  CIte := FMainModul.Model.ModelRoot.GetAllClassifiers;
  while CIte.HasNext do
  begin
    var
    Cent := CIte.Next;
    if (Cent is TClass) or (Cent is TInterface) then
      StringList.Add(WithoutArray(Cent.Name));
  end;
end;

procedure TFUMLForm.Hide;
begin
  Visible := False;
  FJava.DeleteTabAndWindow(Number);
end;

procedure TFUMLForm.Show;
begin
  Visible := True;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
end;

procedure TFUMLForm.DebugJE2Java;
begin
  if Assigned(FMainModul) and Assigned(FMainModul.Diagram) then
    TRtfdDiagram(FMainModul.Diagram).DebugJE2Java;
end;

procedure TFUMLForm.CreateTVFileStructure;
var ClassIte, Ite: IModelIterator; Cent: TClassifier; Attribute: TAttribute;
  Method: TOperation; PictureNr, Indented, IndentedOld: Integer; CName: string;
  Node, ClassNode: TTreeNode; AInteger: TInteger; Classes: TStringList;

  function CalculateIndentation(const Classname: string): Integer;
  begin
    Result := 0;
    for var I := 1 to Length(Classname) do
      if CharInSet(Classname[I], ['$', '.']) then
        Inc(Result);
  end;

begin
  Indented := 0;
  ClassNode := nil;
  TVFileStructure.Items.BeginUpdate;
  for var I := TVFileStructure.Items.Count - 1 downto 0 do
  begin
    AInteger := TInteger(TVFileStructure.Items[I].Data);
    FreeAndNil(AInteger);
  end;
  TVFileStructure.Items.Clear;
  Classes := FMainModul.GetClasses;
  ClassIte := FMainModul.Model.ModelRoot.GetAllClassifiers;
  while ClassIte.HasNext do
  begin
    Cent := TClassifier(ClassIte.Next);
    if Classes.IndexOf(Cent.Name) = -1 then
      Continue;
    CName := Cent.ShortName;
    IndentedOld := Indented;
    Indented := CalculateIndentation(CName);
    while Pos('$', CName) + Pos('.', CName) > 0 do
    begin
      Delete(CName, 1, Pos('$', CName));
      Delete(CName, 1, Pos('.', CName));
    end;

    if Cent is TClass then
      PictureNr := 1
    else
      PictureNr := 11;

    if Indented = 0 then
      ClassNode := TVFileStructure.Items.AddObject(nil, CName,
        TInteger.Create(Cent.LineS))
    else if Indented > IndentedOld then
      ClassNode := TVFileStructure.Items.AddChildObject(ClassNode, CName,
        TInteger.Create(Cent.LineS))
    else
    begin
      while Indented < IndentedOld do
      begin
        Dec(IndentedOld);
        ClassNode := ClassNode.Parent;
      end;
      ClassNode := TVFileStructure.Items.AddChildObject(ClassNode, CName,
        TInteger.Create(Cent.LineS));
    end;

    ClassNode.ImageIndex := PictureNr;
    ClassNode.SelectedIndex := PictureNr;
    ClassNode.HasChildren := True;

    Ite := Cent.GetAttributes;
    while Ite.HasNext do
    begin
      Attribute := TAttribute(Ite.Next);
      PictureNr := Integer(Attribute.Visibility) + 2;
      Node := TVFileStructure.Items.AddChildObject(ClassNode,
        Attribute.ToTypeName, TInteger.Create(Attribute.LineS));
      Node.ImageIndex := PictureNr;
      Node.SelectedIndex := PictureNr;
      Node.HasChildren := False;
    end;
    Ite := Cent.GetOperations;
    while Ite.HasNext do
    begin
      Method := TOperation(Ite.Next);
      if Method.OperationType = otConstructor then
        PictureNr := 6
      else
        PictureNr := Integer(Method.Visibility) + 7;
      Node := TVFileStructure.Items.AddChildObject(ClassNode, Method.ToTypeName,
        TInteger.Create(Method.LineS));
      Node.ImageIndex := PictureNr;
      Node.SelectedIndex := PictureNr;
      Node.HasChildren := False;
    end;
  end;
  FreeAndNil(Classes);
  TVFileStructure.Items.EndUpdate;
  FFileStructure.InitWithItems(TVFileStructure.Items, Self);
end;

procedure TFUMLForm.ChangeStyle;
begin
  if FConfiguration.IsDark then
    TBUMLToolbar.Images := VilToolbarDark
  else
    TBUMLToolbar.Images := VilToolbarLight;
  FMainModul.Diagram.ChangeStyle;
end;

procedure TFUMLForm.DPIChanged;
begin
  SetFontSize(Font.Size - GetFont.Size);
  Refresh;
end;

procedure TFUMLForm.OpenFiles;
begin
  FConfiguration.ShowAlways := False;
  FMainModul.ShowAllOpenedFiles;
  FMainModul.Diagram.ShowParameter := 4;
  FConfiguration.ShowAlways := True;
  SaveAndReload;
  FMainModul.Diagram.ResolveAssociations;
  FMainModul.DoLayout;
  CreateTVFileStructure;
end;

procedure TFUMLForm.OpenFolder;
begin
  if FMainModul.OpenFolderActionExecute(Self) then
  begin
    Pathname := FJava.GetFilename('.uml', FMainModul.OpendFolder);
    // FOpenFolderForm.PathTreeView.Path
    SaveAndReload;
    FMainModul.Diagram.ResolveAssociations;
    FMainModul.DoLayout;
    CreateTVFileStructure;
  end
  else
    Close;
end;

end.
