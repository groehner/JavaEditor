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

// UMLForm -> MainModul -> Diagram -> RtfdDiagram -> TessConnectPanel
//                      -> Model                  -> TAFrameDiagram
// contextmenus in UDiagramFrame

interface

uses
  Classes, Graphics, Controls, Forms, ExtCtrls, ComCtrls,
  UUMLModule, UBaseForm, Vcl.ToolWin,  System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, SVGIconImageCollection,
  Vcl.StdCtrls, SVGIconImageListBase, SVGIconVirtualImageList,
  VirtualShellToolBar, TB2Item, SpTBXItem, TB2Dock, TB2Toolbar;

type
  TFUMLForm = class(TFForm)
    PDiagramPanel: TPanel;
    Panel: TPanel;
    icSVGIconImageCollection: TSVGIconImageCollection;
    vilToolbarDark: TVirtualImageList;
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
    vilToolbarLight: TVirtualImageList;
    TBRecognizeAssociations: TToolButton;
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
    procedure FormClose(Sender: TObject; var aAction: TCloseAction); override;
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
    procedure OnPanelModified(aValue: Boolean);
    procedure OnSelectionChanged(Sender: TObject);
    procedure OnInteractiveModified(Sender: TObject);
  protected
    procedure UpdateState; override;
  public
    MainModul: TDMUMLModule;
    constructor Create(AOwner: TComponent); override;
    procedure Open(const Filename: string; State: string);
    procedure ConfigureWindow(Sender: TObject);
    procedure Print; override;
    procedure Exchange(File1, File2: string);
    function getFilesAndPackages(Selected: boolean): TStringList;
    function getFileWithMain: string;
    function hasClassWithMain: boolean;
    function hasClass(const aClassname: string): boolean;
    function hasEditableClass: boolean;
    procedure AddClassToProject(const CName: string);
    procedure SaveAndReload;
    procedure Hide;
    procedure Show;
    procedure DebugJE2Java;
    procedure CreateTVFileStructure;
    procedure Retranslate;

    procedure CollectClasses(SL: TStringList); override;
    function getAllPathnames: TStringList; override;
    function getAllClassnames: TStringList; override;
    function  GetFont: TFont; override;
    procedure ChangeStyle; override;
    function getFormType: string; override;
    procedure SetOptions; override;
    procedure Save(MitBackup: boolean); override;
    procedure SaveIn(const Dir: string); override;
    function GetSaveAsName: string; override;
    procedure CopyToClipboard; override;
    procedure SetFont(aFont: TFont); override;
    procedure SetFontSize(Delta: integer); override;
    procedure Enter(Sender: TObject); override;
    procedure DPIChanged; override;
  end;

implementation

uses SysUtils, IniFiles, StrUtils, UITypes, JvGnugettext, UStringRessources,
     UConfiguration, UJava, UUtils, UModelEntity, UModel, UClassInsert,
     URtfdDiagram, UViewIntegrator, UMessages, UComJava1, UJavaCommands,
     UFileStructure, Themes;

{$R *.DFM}

constructor TFUMLForm.Create(AOwner: TComponent);
begin
  inherited;
  FormTag:= 2;
end;

procedure TFUMLForm.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  MainModul:= TDMUMLModule.Create(Self, PDiagramPanel);
  SetFont(FConfiguration.UMLFont);
  Modified:= false;
  Enter(Self);
  PUMLPanel.Visible:= FConfiguration.vistoolbars[3];
  MainModul.Diagram.SetOnModified(OnPanelModified);
  MainModul.Diagram.SetOnSelectionChanged(OnSelectionChanged);
  LockEnter:= false;
end;

procedure TFUMLForm.Retranslate;
begin
  RetranslateComponent(Self);
  MainModul.Diagram.Retranslate;
  if assigned(FClassInsert) then
    RetranslateComponent(FClassInsert);
end;

procedure TFUMLForm.Open(const Filename: string; State: string);
begin
  MainModul.Diagram.SetInteractive(Self, Filename, OnInteractiveModified);
  if Filename = _(LNGInteractive) then exit;
  ToMainPanel;
  Pathname:= Filename;
  if ExtractFilePath(Pathname) = FConfiguration.TempDir
    then Caption:= ExtractFilename(Pathname)
    else Caption:= Filename;
  UpdateState;
  SetState(State);
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);

  //Enter(Self);
end;

procedure TFUMLForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  const Interactive = 1;
begin
  if (Number <> Interactive) and not AlreadySavedAs and Modified then begin
    FJava.DoSave(Self, true);
    AlreadySavedAs:= true;
  end;
  if assigned(MainModul) then begin
    var ComJava:= (MainModul.Diagram as TRtfDDiagram).getComJava;
    if assigned(ComJava) then ComJava.ExecuteCommand('term');
  end;
  FFileStructure.Clear;
  CanClose:= true;
end;

procedure TFUMLForm.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  LockEnter:= true;
  try
    FMessages.DelInteractive(pathname);
    if assigned(TVFileStructure) then
      for var i:= TVFileStructure.Items.Count - 1 downto 0 do
        FreeAndNil(TVFileStructure.Items[i].Data);
  finally
    inherited;
  end;
end;

procedure TFUMLForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MainModul);  // added
  inherited;
end;

procedure TFUMLForm.Enter(Sender: TObject);
begin
  if LockEnter then exit;
  LockEnter:= true;
  inherited;
  if Visible then begin  // due to bug, else ActiveForm doesn't change
    if PUMLPanel.Visible and PUMLPanel.CanFocus then PUMLPanel.SetFocus;
    if assigned(MainModul) and assigned(MainModul.Diagram) then begin
      var aPanel:= MainModul.Diagram.GetPanel;
      if assigned(aPanel) and aPanel.CanFocus then
        aPanel.SetFocus;
      var ComJava:= (MainModul.Diagram as TRtfDDiagram).getComJava;
      ComJava.setActiveComJava(ComJava);
    end;
  end;
  UpdateState;
  SaveAndReload;
  if not MainModul.Diagram.hasObjects and MainModul.Diagram.HasAInvalidClass then
    FJava.MICompileAllClick(Self);
  LockEnter:= false;
end;

procedure TFUMLForm.ConfigureWindow(Sender: TObject);
begin
  Align:= alClient;
  MainModul.Diagram.ShowIcons:= FConfiguration.DiShowIcons;
  MainModul.Diagram.VisibilityFilter:= TVisibility(0);
  MainModul.Diagram.ShowParameter:= FConfiguration.DiShowParameter;
  MainModul.Diagram.SortOrder:= FConfiguration.DiSortOrder;
end;

procedure TFUMLForm.SBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFUMLForm.TBShowConnectionsClick(Sender: TObject);
begin
  if assigned(MainModul) and assigned(MainModul.Diagram) then begin
    var sa:= (Mainmodul.Diagram.ShowConnections + 1) mod 3;
    MainModul.Diagram.ShowConnections:= sa;
    Modified:= true;
    MainModul.Diagram.GetPanel.Invalidate;
  end;
end;

procedure TFUMLForm.TBViewClick(Sender: TObject);
begin
  if assigned(MainModul) and assigned(MainModul.Diagram) then begin
    var sv:= (Mainmodul.Diagram.ShowView + 1) mod 3;
    MainModul.Diagram.ShowView:= sv;
    Modified:= true;
    MainModul.Diagram.GetPanel.Invalidate;
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
  if Button = mbLeft
    then (MainModul.Diagram as TRtfdDiagram).JavaReset
    else FJava.Restart;
end;

procedure TFUMLForm.TBNewLayoutClick(Sender: TObject);
begin
  MainModul.DoLayout;
  Modified:= true;
end;

procedure TFUMLForm.TBDiagramFromOpenWindowsClick(Sender: TObject);
begin
  MainModul.ShowAllOpenedFiles;
  Modified:= true;
end;

procedure TFUMLForm.SaveAndReload;
begin
  if Pathname <> '' then begin
    LockWindow(Self.Handle);
    Save(true);
    MainModul.LoadUML(Pathname);
    CreateTVFileStructure;
    UnlockWindow;
  end;
end;

procedure TFUMLForm.TBRecognizeAssociationsClick(Sender: TObject);
begin
  MainModul.Diagram.ResolveAssociations;
end;

procedure TFUMLForm.TBRefreshClick(Sender: TObject);
begin
  SaveAndReload;
end;

procedure TFUMLForm.TBClassNewClick(Sender: TObject);
begin
  FJava.MINewClassClick(Self);
  Modified:= true;
end;

procedure TFUMLForm.Save(MitBackup: boolean);
begin
  MainModul.SaveUML(Pathname);
  MainModul.Diagram.InteractiveSetModified(false);
  Modified:= false;
end;

procedure TFUMLForm.SaveIn(const Dir: string);
begin
  Pathname:= Dir + ExtractFilename(Pathname);
  Save(false);
  FJava.RenameTabAndWindow(Number, Pathname);
  Caption:= Pathname;
end;

procedure TFUMLForm.Exchange(File1, File2: string);
  var s, Path, class1, class2: string; p: integer;
begin
  Pathname:= FConfiguration.HomeDir + 'Default.uml';
  var SL:= TStringList.Create;
  try
    try
      Save(true);
      SL.LoadFromFile(Pathname);
      Path:= ExtractFilePath(Pathname);
      s:= SL.Text;
      File1:= ExtractRelativePath(Path, File1);
      File2:= ExtractRelativePath(Path, File2);
      p:= Pos(File1, SL.Text);
      if p > 0 then begin
        delete(s, p, length(File1));
        insert(File2, s, p);
      end;
      class1:= 'Box:  - ' + ChangeFileExt(ExtractFilename(File1), '');
      class2:= 'Box:  - ' + ChangeFileExt(ExtractFilename(File2), '');
      s:= ReplaceStr(s, class1, class2);
      SL.Text:= s;
      SL.SaveToFile(Pathname);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    FreeAndNil(SL);
  end;
  MainModul.LoadUML(Pathname);
  Modified:= true;
end;

function TFUMLForm.getFormType: string;
begin
  Result:= '%U%';
end;

procedure TFUMLForm.Print;
begin
  MainModul.Print;
end;

procedure TFUMLForm.TBObjectDiagramClick(Sender: TObject);
begin
  if assigned(MainModul) and assigned(MainModul.Diagram) then begin
    var b:= not Mainmodul.Diagram.ShowObjectDiagram;
    Mainmodul.Diagram.ShowObjectDiagram:= b;
    Modified:= true;
    TBObjectDiagram.Down:= b;
  end;
end;

procedure TFUMLForm.TBClassEditorClick(Sender: TObject);
begin
  FJava.aUMLForm:= Self;
  MainModul.EditSelectedElement;
  Modified:= true;
end;

procedure TFUMLForm.TBClassOpenClick(Sender: TObject);
begin
  if Mainmodul.Diagram.ShowObjectDiagram then begin
    Mainmodul.Diagram.ShowObjectDiagram:= false;
    TBObjectDiagram.Down:= false;
  end;
  FJava.MIClassOpenClick(Self);
  Modified:= true;
end;

procedure TFUMLForm.TBCommentClick(Sender: TObject);
begin
  if assigned(MainModul) and assigned(MainModul.Diagram) then
    Mainmodul.Diagram.addCommentBoxTo(nil);
end;

procedure TFUMLForm.TBClassInsertClick(Sender: TObject);
begin
  if FClassInsert = nil then
    FClassInsert:= TFClassInsert.Create(FJava);
  FClassInsert.ActivateListView;
  FConfiguration.ShowAlways:= true;
  var ShowPublicOnly:= FConfiguration.ShowPublicOnly;
  if FClassInsert.ShowModal = mrOK then begin
    case FClassInsert.RGOptions.ItemIndex of
      0: FConfiguration.ShowPublicOnly:= true;
      1: FConfiguration.ShowPublicOnly:= false;
    end;
    var SL:= TStringList.Create;
    FClassInsert.GetSelectedFiles(SL);
    MainModul.AddToProject(SL);
    Modified:= true;
    FreeAndNil(SL);
  end;
  FClassInsert.ZipClose;
  FConfiguration.ShowPublicOnly:= ShowPublicOnly;
end;

procedure TFUMLForm.PDiagramPanelResize(Sender: TObject);
begin
  inherited;
  if Assigned(MainModul) then
    MainModul.Diagram.RecalcPanelSize;
end;

function TFUMLForm.GetSaveAsName: string;
begin
  Result:= getFileWithMain;
  if Result = ''
    then Result:= MainModul.getUMLFilename
    else Result:= ChangeFileExt(Result, '.uml');
end;

procedure TFUMLForm.CopyToClipboard;
begin
  MainModul.Diagram.CopyDiagramToClipboard;
end;

procedure TFUMLForm.SetFont(aFont: TFont);
begin
  Font.Assign(aFont);
  MainModul.Diagram.SetFont(aFont);
  MainModul.RefreshDiagram;
end;

function TFUMLForm.GetFont: TFont;
begin
  Result:= MainModul.Diagram.Font;
end;

procedure TFUMLForm.SetFontSize(Delta: integer);
begin
  var aFont:= GetFont;
  aFont.Size:= aFont.Size + Delta;
  if aFont.Size < 6 then aFont.Size:= 6;
  SetFont(aFont);
  Font.Size:= aFont.Size;
end;

procedure TFUMLForm.UpdateState;
begin
  inherited;
  with FJava do begin
    SetEnabledMI(MICopy, true);
    SetEnabledMI(MICopyNormal, true);
    SetEnabledMI(MIPaste, false);
    SetEnabledMI(MICut, false);
  end;
  var b:= hasEditableClass();
  SetEnabledMI(FJava.MIClassEditor, b and not myJavaCommands.ProcessRunning);

  b:= hasClassWithMain;
  SetEnabledTB(FJava.TBRun, b);
  SetEnabledMI(FJava.MIRun, b and not myJavaCommands.ProcessRunning);
end;

procedure TFUMLForm.SetOptions;
begin
  Refresh;
  PUMLPanel.Visible:= FConfiguration.vistoolbars[3];
end;

function TFUMLForm.getFilesAndPackages(Selected: boolean): TStringList;
begin
  Result:= (MainModul.Diagram as TRtfdDiagram).getFilesAndPackages(Selected);
end;

function TFUMLForm.getAllPathnames: TStringList;
begin
  Result:= (MainModul.Diagram as TRtfdDiagram).getAllPathnames;
end;

function TFUMLForm.getAllClassnames: TStringList;
begin
  Result:= (MainModul.Diagram as TRtfdDiagram).getAllClassnames;
end;

function TFUMLForm.getFileWithMain: string;
begin
  if assigned(MainModul) and assigned(MainModul.Diagram) and (MainModul.Diagram is TRtfdDiagram)
    then Result:= (MainModul.Diagram as TRtfdDiagram).getFileWithMain
    else Result:= '';
end;

function TFUMLForm.hasClassWithMain: boolean;
begin
  Result:= (getFileWithMain <> '');
end;

function TFUMLForm.hasClass(const aClassname: string): boolean;
begin
  if assigned(Mainmodul)
    then Result:= (MainModul.Diagram as TRtfdDiagram).hasClass(aClassname)
    else Result:= false;
end;

procedure TFUMLForm.OnPanelModified(aValue: Boolean);
begin
  setModified(aValue);
end;

procedure TFUMLForm.OnSelectionChanged(Sender: TObject);
begin
  UpdateState;
end;

function TFUMLForm.hasEditableClass: boolean;
begin
  if assigned(MainModul)
    then Result:= MainModul.hasEditableClass
    else Result:= false;
end;

procedure TFUMLForm.AddClassToProject(const CName: string);
begin
  if FClassInsert = nil then
    FClassInsert:= TFClassInsert.Create(FJava);
  var s:= FClassInsert.OpenOrExtractClass(MainModul.Model.ModelRoot.Files, CName);
  if s = '' then
    s:= FConfiguration.SearchClassInClasspath(ExtractClassName(CName), Pathname, ExtractPackageName(CName));
  if s <> '' then begin
    FConfiguration.ShowAlways:= false;
    MainModul.AddToProject(s);
    FConfiguration.ShowAlways:= true;
  end;
end;

procedure TFUMLForm.OnInteractiveModified(Sender: TObject);
begin
  Modified:= true;
end;

procedure TFUMLForm.CollectClasses(SL: TStringList);
begin
  var Ci:= MainModul.Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext do begin
    var cent:= Ci.Next;
    if (cent is TClass) or (cent is TInterface) then
      SL.Add(WithoutArray(cent.Name));
  end;
end;

procedure TFUMLForm.Hide;
begin
  Visible:= false;
  FJava.DeleteTabAndWindow(Number);
end;

procedure TFUMLForm.Show;
begin
  Visible:= true;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
end;

procedure TFUMLForm.DebugJE2Java;
begin
  if assigned(Mainmodul) and assigned(MainModul.Diagram) then
    (MainModul.Diagram as TRtfdDiagram).DebugJE2Java;
end;

procedure TFUMLForm.CreateTVFileStructure;
  var
    Ci, it: IModelIterator;
    cent: TClassifier;
    Attribute: TAttribute;
    Method: TOperation;
    PictureNr, i, Indented, IndentedOld: Integer;
    CName: string;
    Node, ClassNode: TTreeNode;
    aInteger: TInteger;
    Classes: TStringList;

  function CalculateIndentation(const classname: string): integer;
  begin
    Result:= 0;
    for var i:= 1 to length(classname) do
      if CharInSet(classname[i], ['$', '.']) then inc(Result);
  end;

begin
  Indented:= 0;
  Classnode:= nil;
  TVFileStructure.Items.BeginUpdate;
  for i:= TVFileStructure.Items.Count - 1 downto 0 do begin
    aInteger:= TInteger(TVFileStructure.Items[i].Data);
    FreeAndNil(aInteger);
  end;
  TVFilestructure.Items.Clear;
  Classes:= MainModul.getClasses;
  Ci:= MainModul.Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext do begin
    cent := TClassifier(Ci.Next);
    if Classes.IndexOf(Cent.Name) = -1 then
      continue;
    CName:= cent.ShortName;
    IndentedOld:= Indented;
    Indented:= CalculateIndentation(CName);
    while Pos('$', CName) + Pos('.', CName) > 0 do begin
      delete(CName, 1, Pos('$', CName));
      delete(CName, 1, Pos('.', CName));
    end;

    if cent is TClass
      then PictureNr:= 1
      else PictureNr:= 11;

    if Indented = 0 then
      ClassNode:= TVFileStructure.Items.AddObject(nil, CName, TInteger.create(cent.LineS))
    else if Indented > IndentedOld then
      ClassNode:= TVFileStructure.Items.AddChildObject(ClassNode, CName, TInteger.create(cent.LineS))
    else begin
      while Indented < IndentedOld do begin
        dec(IndentedOld);
        ClassNode:= ClassNode.Parent;
      end;
      ClassNode:= TVFileStructure.Items.AddChildObject(ClassNode, CName, TInteger.create(cent.LineS));
    end;

    ClassNode.ImageIndex:= PictureNr;
    ClassNode.SelectedIndex:= PictureNr;
    ClassNode.HasChildren:= true;

    it:= cent.GetAttributes;
    while It.HasNext do begin
      Attribute:= It.Next as TAttribute;
      PictureNr:= Integer(Attribute.Visibility) + 2;
      Node:= TVFileStructure.Items.AddChildObject(ClassNode, Attribute.toShortString, TInteger.create(Attribute.LineS));
      Node.ImageIndex:= PictureNr;
      Node.SelectedIndex:= PictureNr;
      Node.HasChildren:= false;
    end;
    It:= cent.GetOperations;
    while It.HasNext do begin
      Method:= It.Next as TOperation;
      if Method.OperationType = otConstructor
        then PictureNr:= 6
        else PictureNr:= Integer(Method.Visibility) + 7;
      Node:= TVFileStructure.Items.AddChildObject(ClassNode, Method.toShortString, TInteger.create(Method.LineS));
      Node.ImageIndex:= PictureNr;
      Node.SelectedIndex:= PictureNr;
      Node.HasChildren:= false;
    end;
  end;
  FreeAndNil(Classes);
  TVFileStructure.Items.EndUpdate;
  FFileStructure.init(TVFileStructure.Items, Self);
  // FJava.Memo1.lines.AddStrings(MainModul.Model.ModelRoot.Debug);
end;

procedure TFUMLForm.ChangeStyle;
begin
  if FConfiguration.isDark
    then TBUMLToolbar.Images:= vilToolbarDark
    else TBUMLToolbar.Images:= vilToolbarLight;
  MainModul.Diagram.ChangeStyle;
end;

procedure TFUMLForm.DPIChanged;
begin
  setFontSize(Font.Size - GetFont.Size);
  Refresh;
end;

end.

