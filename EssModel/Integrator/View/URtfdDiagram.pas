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

{$ASSERTIONS ON}

unit URtfdDiagram;

interface

uses Controls, Types, Graphics, Classes, Forms, ExtCtrls,
  uDiagramFrame, uRtfdComponents, uListeners, UJniWrapper1,
  UComJava1, UMessages, uViewIntegrator, UEssConnectPanel, uModelEntity,
  uModel, UUtils, UUMLForm, USequenceForm, UConnection;

type
  TRtfdDiagram = class(TDiagramIntegrator,
      IBeforeObjectModelListener,
      IAfterObjectModelListener,
      IAfterUnitPackageListener)
  private
    // Model: TObjectModel; is inherited
    IsAllClasses : boolean;
    InteractivePath: string;
    OnModified: TNotifyEvent;

    BoxNames: TStringList;
    FullParameters: TStringList;
    Interactive: TInteractive;
    ComJava: TComJava1;
    UMLForm: TFUMLForm;

    procedure AddBox(E: TModelEntity);
    function GetBox(typ: string) : TRtfdBox;
    //Model listeners
    procedure ModelBeforeChange(Sender: TModelEntity);
    procedure ModelAfterChange(Sender: TModelEntity);
    procedure IBeforeObjectModelListener.Change = ModelBeforeChange;
    procedure IAfterObjectModelListener.Change = ModelAfterChange;
    //Unitpackage listeners
    procedure UnitPackageAfterChange(Sender: TModelEntity);
    procedure UnitPackageAfterAddChild(Sender: TModelEntity; NewChild: TModelEntity);
    procedure UnitPackageAfterRemove(Sender: TModelEntity);
    procedure UnitPackageAfterEntityChange(Sender: TModelEntity);
    procedure IAfterUnitPackageListener.Change = UnitPackageAfterChange;
    procedure IAfterUnitPackageListener.AddChild = UnitPackageAfterAddChild;
    procedure IAfterUnitPackageListener.Remove = UnitPackageAfterRemove;
    procedure IAfterUnitPackageListener.EntityChange = UnitPackageAfterEntityChange;
  private
    function PPIScale(ASize: integer): integer;
    function PPIUnScale(ASize: integer): integer;
    function PanelIsLocked: boolean;
  protected
    procedure SetVisibilityFilter(const Value: TVisibility); override;
    procedure SetShowParameter(const Value: integer); override;
    procedure SetShowView(Value: integer); override;
    procedure SetSortOrder(const Value: integer); override;
    procedure SetShowIcons(const Value: integer); override;
    procedure SetShowObjectDiagram(const Value: boolean); override;
    procedure CurrentEntityChanged; override;
  public
    Panel: TessConnectPanel;
    Frame: TAFrameDiagram;
    SequenceForm: TFSequenceForm;
    constructor Create(om: TObjectModel; Parent: TWinControl); override;
    destructor Destroy; override;
    procedure ClearDiagram; override;
    procedure ResolveAssociations; override;
    procedure ResolveObjectAssociations; override;
    procedure InitFromModel; override;
    procedure PaintTo(Canvas: TCanvas; X, Y: integer; SelectedOnly : boolean); override;
    procedure GetDiagramSize(var W, H : integer); override;
    procedure SetPackage(const Value: TAbstractPackage); override;
    procedure DoLayout; override;
    function GetClickAreas : TStringList; override;
    procedure ClassEditSelectedDiagramElements; overload; override;
    procedure ClassEditSelectedDiagramElements(Sender: TObject); overload; override;
    procedure SourceEditSelectedDiagramElementsControl(C: TControl); override;

    procedure UnSelectAllElements; override;
    function GetSelectedRect: TRect; override;
    procedure ScreenCenterEntity(E : TModelEntity); override;
    procedure SetFont(const aFont: TFont); override;
    function GetFont: TFont; override;

    function JavaInsteadClass(SL: TStringList): TStringList;
    procedure StoreDiagram(filename: string); override;
    procedure FetchDiagram(filename: string); override;

    procedure RefreshDiagram; override;
    procedure RecalcPanelSize; override;
    procedure SetConnections(const Value: integer); override;

    procedure SelectAssociation; override;
    procedure DeleteSelectedControls(Sender: TObject);
    procedure DeleteSelectedControlsAndRefresh; override;
    procedure DeleteObjects; override;
    procedure DeleteObject(const objectname: string);
    function hasObjects: boolean; override;
    function hasEditableClass: boolean; override;
    function hasSelectedControl: boolean; override;
    function hasSelectedConnection: boolean; override;
    function GetFrame: TAFrameDiagram;
    function GetPanel: TCustomPanel; override;
    procedure SetInteractive(aUMLForm: TForm; path: string; OnInteractiveModified: TNotifyEvent); override;
    procedure SetFormMouseDown(OnFormMouseDown: TNotifyEvent); override;
    function getComJava: TComJava1;
    function hasInteractiveCode: boolean; override;
    procedure AddToInteractive(const s: string);
    procedure InteractiveSetModified(Modified: boolean); override;
    function getSourcePath: string; override;

    function getFilesAndPackages(Selected: boolean): TStringList;
    function getFilesAndPackagesFromList(L: TList): TStringList;
    procedure CompileOneWith(C: TControl; Compiler: string); override;
    function getAllPathnames: TStringList;
    function getAllClassnames: TStringList;
    function getFileWithMain: string;
    procedure Run(C: TControl); override;
    procedure ShowInheritedMethodsFromSystemClasses(C: TControl; ShowOrHide: boolean); override;

    procedure OpenClassOrInterface(Sender: TObject);
    procedure OpenClassWithDialog; override;
    procedure NewClass; override;
    procedure ClassInsert; override;
    procedure ShowUnnamedObject(Sender: TObject); override;
    procedure ShowAllNewObjects(Sender: TObject); override;
    procedure ShowAllNewObjectsString(From: string = '');

    procedure ShowObject(const objname: string);
    procedure ConnectBoxes(Sender: TObject);
    procedure DoConnection(Item: integer); override;
    procedure DoAlign(Item: integer); override;

    procedure PopMenuClassPopup(Sender: TObject); override;
    procedure PopMenuObjectPopup(Sender: TOBject); override;
    procedure PopMenuConnectionPopup(Sender: TObject); override;

    procedure CreateObjectForSelectedClass(Sender: TObject);
    function CreateModelClass(const Typ: string): TClass;
    function FindClassifier(const CName: string): TClassifier;
    procedure ShowNewObject(aJavaObject: TComJavaObject);
    procedure CallMethod(C: TControl; Sender: TObject);
    procedure CallMethodForObject(Sender: TObject);
    procedure CallMethodForClass(Sender: TObject);
    procedure CallMain(const Classpath, aClassname: string; CallParameter: string);
    function CollectClasses: boolean;
    function MakeParams(Parameter: TStringList; var theParams: TComJavaParams; var asString: string): boolean;
    procedure DeleteParams(Parameter: TStringList);

    procedure EditObject(C: TControl); override;
    procedure OpenClass(C: TControl); override;
    function ClassHasObjects(aBox: TRtfdBox): boolean;
    function HasAttributes(aModelClass: TClass): boolean;
    procedure UpdateAllObjects;
    procedure ShowAttributes(aJavaObject: TComJavaObject; aModelObject: TObjekt);
    procedure GetAllAttributeValues(aClass: TClass; aJavaObject: TComJavaObject;
                                 aAttribut: TComJavaAttribute; Attributes: TStringList);
    procedure SetAttributeValues(aClass: TClass; aJavaObject: TComJavaObject;
                                 aAttribut: TComJavaAttribute; Attributes: TStringList);
    function EditClass(const Caption, Title, ObjectNameOld: string; var ObjectNameNew: string; Attributes: TStringList): boolean;
    function EditObjectOrParams(const Caption, Title: string; Attributes: TStringList): boolean;
    procedure SetRecursiv(P: TPoint; pos: integer); override;
    function getModelClass(const s: string): TClass;
    function StringToArrowStyle(s: string): TessConnectionArrowStyle;
    function ArrowStyleToString(ArrowStyle: TessConnectionArrowStyle): string;
    procedure JavaReset;
    function getCommentBoxName: string;
    procedure AddCommentBoxTo(aControl: TControl); override;
    function insertParameterNames(s: string): string;
    function hasClass(aClassname: string): boolean;
    procedure DebugJE2Java;
    procedure DoShowParameter(aControl: TControl; Mode: integer); override;
    procedure DoShowVisibility(aControl: TControl; Mode: integer); override;
    procedure DoShowVisibilityFilter(aControl: TControl; Mode: integer); override;
    procedure CreateTestClass(aControl: TControl); override;
    procedure Lock(b: boolean); override;
    procedure RunTests(aControl: TControl; const Method: string); override;
    procedure OnRunJunitTestMethod(Sender: TObject);
    procedure ShowMethodEntered(const aMethodname, From, _To, Parameter: string);
    procedure ShowMethodExited(const aMethodname, From, _To, _Result: string);
    procedure ShowObjectDeleted(const From, _To: string);
    procedure CloseNotify(Sender: TObject);
    procedure ClearSelection; override;
    procedure CopyDiagramToClipboard; override;
    procedure ClearMarkerAndConnections(Control: TControl); override;
    procedure DrawMarkers(r: TRect; show: boolean); override;
    procedure EditBox(Control: TControl); override;
    procedure SetModified(const Value: boolean); override;
    procedure SetOnModified(OnBoolEvent: TBoolEvent); override;
    procedure SetOnSelectionChanged(Sender: TNotifyEvent); override;
    procedure ChangeStyle; override;
    procedure DeleteComment; override;
    function getDebug: TStringList;
    function getSVG: string; override;
    function getClasses: TStringList; override;
    procedure Retranslate; override;
    procedure SetUMLFont; override;
    function HasAInvalidClass: boolean; override;
  end;

implementation

uses windows, Math, Menus, SysUtils, StrUtils, IniFiles, Dialogs, Contnrs,
  Clipbrd, StdCtrls, UJUnitTest, JvGnugettext, UStringRessources,
  uIterators, USugiyamaLayout, uDlgMethodCall, uRtfdDiagramFrame,
  uIntegrator, UJava, UConfiguration, UUMLModule,
  UObjectgenerator, UCodeCompletion, UITypes, UEditorForm,
  UBaseForm, UJavaCommands, SynEdit, UDlgAbout, JNI, UExecution,
  UTemplates, SpTBXItem;

{ TRtfdDiagram }

constructor TRtfdDiagram.Create(om: TObjectModel; Parent: TWinControl);
begin
  inherited Create(Om, Parent);
  Frame:= TAFrameRtfdDiagram.Create(Parent, Self);
  Frame.Parent:= Parent;  // assigment to the gui
  UMLForm:= (Parent.Parent.Parent as TFUMLForm);

  // Panel is ActiveControl in MainForm
  Panel:= TessConnectPanel.Create(UMLForm);
  Panel.PopupMenuConnection:= Frame.PopMenuConnection;
  Panel.PopupMenuAlign:= Frame.PopupMenuAlign;
  Panel.PopupMenuWindow:= Frame.PopupMenuWindow;
  Panel.Parent:= Frame.ScrollBox;
  Panel.OnDeleteSelectedControls:= DeleteSelectedControls;
  Panel.OnClassEditSelectedDiagramElements:= ClassEditSelectedDiagramElements;
  OnModified:= nil;
  Sequenceform:= nil;

  BoxNames:= TStringList.Create;
  BoxNames.CaseSensitive:= True;
  BoxNames.Sorted:= True;
  BoxNames.Duplicates:= dupIgnore;
  FullParameters:= TStringList.Create;

  Model.AddListener(IBeforeObjectModelListener(Self));
  ClearDiagram;
end;

destructor TRtfdDiagram.Destroy;
begin
  // Force listeners to release, and diagram to persist.
  // Package:= nil;
  // Model.RemoveListener(IBeforeObjectModelListener(Self));
  Model.ClearListeners;

  FreeAndNil(FullParameters);
  for var i:= 0 to BoxNames.Count-1 do begin
    var Box:= BoxNames.Objects[i] as TRtfdBox;
    FreeAndNil(Box);
  end;
  FreeAndNil(BoxNames);
  FreeAndNil(Interactive);
  // dont do that: FreeAndNil(Frame);
  // Frame is part of the gui and therefore is destroyed as GUI-component
  // Panel is part of Frame so it is also destroyed by system as GUI-component
  inherited;   // Model is inherited
end;

procedure TRtfdDiagram.InitFromModel;

  var Mi: IModelIterator;

  procedure InAddUnit(Up: TUnitPackage);
  begin
    var Mi:= Up.GetClassifiers;
    while Mi.HasNext do begin
      var Cl:= MI.Next as TClassifier;
      if CL.IsVisible then
        AddBox(Cl);
    end;
  end;

begin
  IsAllClasses:= (Package = AllClassesPackage);
  IsAllClasses:= true;  // otherwise no Delpi-class is shown
  Panel.Hide;
  if not Assigned(FPackage) then begin
    Package := Model.ModelRoot;
    //If there is only one package (except unknown) then show it.
    //Assign with Package-property to trigger listeners
    Mi := (FPackage as TLogicPackage).GetPackages;
    if Mi.Count = 2 then begin
      Mi.Next;
      Package := Mi.Next as TAbstractPackage;
    end;
  end;

  //Clean old
  ClearDiagram;

  //Create boxes
  if FPackage is TUnitPackage then
    InAddUnit(FPackage as TUnitPackage)
  else  begin
    //Logic package
    //Exclude unknown-package, otherwise all temp-classes will be included on showallclasses.
    //Also, unkown-package will be shown on package-overview (including docgen)
    if IsAllClasses then begin
      //These lines show all members of a package on one diagram
      Mi := TModelIterator.Create( (Model.ModelRoot as TLogicPackage).GetPackages, TEntitySkipFilter.Create(Model.UnknownPackage) );
      while Mi.HasNext do
        InAddUnit(Mi.Next as TUnitPackage)
    end else begin
      Mi := TModelIterator.Create( (FPackage as TLogicPackage).GetPackages, TEntitySkipFilter.Create(Model.UnknownPackage) );
      while Mi.HasNext do
        AddBox(Mi.Next);
    end;
  end;

  //Create arrow between boxes
  //This must be done after fetchdiagram because connection-setting might be stored
  DoLayout;
  Panel.RecalcSize;
  Panel.IsModified := False;
  Panel.Show;
  if Panel.CanFocus then
    Panel.SetFocus;
end;

procedure TRtfdDiagram.ModelBeforeChange(Sender: TModelEntity);
begin
  Package := nil;
  IsAllClasses := False;
  ClearDiagram;
end;

procedure TRtfdDiagram.ModelAfterChange(Sender: TModelEntity);
begin
  InitFromModel;
end;

procedure TRtfdDiagram.PaintTo(Canvas: TCanvas; X, Y: integer; SelectedOnly : boolean);
begin
  var OldBit := Panel.BackBitmap;
  Panel.BackBitmap := nil;
  if SelectedOnly then begin
    if Panel.GetFirstSelected <> nil then
      Panel.SelectedOnly := True;
  end else
    // selection-markers should not be visible in the saved picture
    Panel.ClearSelection;
  Canvas.Lock;
  try
    Panel.PaintTo(Canvas.Handle, X, Y);
    Panel.TextTo(Canvas);
  finally
    Canvas.Unlock;
    Panel.SelectedOnly := False;
    Panel.BackBitmap := OldBit;
  end;
end;

function TRtfdDiagram.getSVG: string;
  var s, sw, si, ga: string; i, w, h: integer;
begin
  Panel.GetDiagramSize(w, h);
  s:= '<?xml version="1.0" encoding="UTF-8" ?>'#13#10;
  s:= s + '<svg width="' + IntToStr(w) + '"' + ' height="' + IntToStr(h) + '"' +
          ' font-family="' + Font.Name + '"' +
          ' font-size="' + IntToStr(round(Font.Size*1.3)) + '">'#13#10;
  if FConfiguration.Shadowwidth > 0 then begin
    sw:= FloatToVal(FConfiguration.ShadowWidth / 2.0);
    si:= FloatToVal(min(2*FConfiguration.ShadowIntensity/10.0, 1));
    ga:= FloatToVal(min(FConfiguration.ShadowWidth, 10)*0.4);
    s:= s +
      '  <defs>'#13#10 +
      '    <filter style="color-interpolation-filters:sRGB;" id="Shadow">'#13#10 +
      '      <feFlood flood-opacity=' + si + ' flood-color="rgb(0,0,0)" result="flood" />'#13#10 +
      '      <feComposite in="flood" in2="SourceGraphic" operator="in" result="composite1"/>'#13#10 +
      '      <feGaussianBlur in="composite1" stdDeviation=' + ga + ' result="blur" />'#13#10 +
      '      <feOffset dx=' + sw + ' dy=' + sw + ' result="offset" />'#13#10 +
      '      <feComposite in="SourceGraphic" in2="offset" operator="over" result="composite2" />'#13#10 +
      '    </filter>'#13#10 +
      '  </defs>'#13#10;
  end;
  s:= s + Panel.getSVGConnections;
  for i:= 0 to BoxNames.Count-1 do
    s:= s + (BoxNames.Objects[i] as TRtfdBox).getSVG;
  Result:= s + '</svg>'#13#10;
end;

procedure TRtfdDiagram.ClearDiagram;
begin
  if not (csDestroying in Panel.ComponentState) then begin
    Panel.ClearManagedObjects;
    Panel.DestroyComponents;
  end;
  BoxNames.Clear;
end;

//Add a 'Box' to the diagram (class/interface/package/objekt/comment).
procedure TRtfdDiagram.AddBox(E: TModelEntity);
var
  Mi : IModelIterator;
  Int : TInterface;
  C : TClass;
  A : TAttribute;

  function InCreateBox(E: TModelEntity; BoxT: TRtfdBoxClass): TRtfdBox;
    var vis: TVisibility; aClass: TClass;
  begin
    if E is TClass
      then aClass:= E as TClass
      else aClass:= nil;
    if assigned(aClass) and (Pos(FConfiguration.JavaCache, aClass.Pathname) > 0)
      then vis:= viPublished
      else vis:= viPrivate;
    Result:= BoxT.Create(Panel, E, Frame, vis);
    Result.Font.Assign(Font);
    if FConfiguration.ArrayListAsIntegratedList and
      (E is TObjekt) and ((E as TObjekt).getTyp.Name = 'java.util.ArrayList') then
      Result.Visible:= false;
    BoxNames.AddObject(E.Name, Result);
  end;

begin
  if E is TUnitPackage then
    Panel.AddManagedObject(InCreateBox(E, TRtfdUnitPackage))
  else if E is TClass then begin
    //Insert related boxes from other packages
    //This should not be done if IsAllClasses, because then all boxes are inserted anyway
    IsAllClasses:= true; // testweise
    if not IsAllClasses then begin
      //Ancestor that is in another package and that is not already inserted
      //is added to the diagram.
      C := (E as TClass);
      if Assigned(C.Ancestor) and (C.Ancestor.Owner <> E.Owner) and
        (GetBox(C.Ancestor.FullName) = nil) then begin
          Panel.AddManagedObject(InCreateBox(C.Ancestor, TRtfdClass));
        end;
      //Implementing interface that is in another package and is not already inserted
      //is added to the diagram.
      Mi := C.GetImplements;
      while Mi.HasNext do begin
        Int := Mi.Next as TInterface;
        if (Int.Owner<>E.Owner) and
          ( GetBox( Int.FullName )=nil ) then
          Panel.AddManagedObject(InCreateBox(Int,TRtfdInterface) );
      end;
      //Attribute associations that are in other packages are added
      Mi := C.GetAttributes;
      while Mi.HasNext do begin
        A := TAttribute(Mi.Next);
        if Assigned(A.TypeClassifier) and (GetBox(A.TypeClassifier.FullName) = nil) and
          (A.TypeClassifier <> C) and (A.TypeClassifier <> C.Ancestor) and
          (A.TypeClassifier.Owner <> Model.UnknownPackage) then //Avoid getting temp-types from unknown (java 'int' for example)
        begin
          if A.TypeClassifier is TClass then
            Panel.AddManagedObject( InCreateBox(A.TypeClassifier,TRtfdClass) );
          if A.TypeClassifier is TInterface then
            Panel.AddManagedObject(InCreateBox(A.TypeClassifier, TRtfdInterface) );
        end;
      end;
    end;
    if E.IsVisible and (GetBox(E.getFullnameWithoutOuter) = nil) then
      Panel.AddManagedObject(InCreateBox(E, TRtfdClass));
  end else if E is TInterface then begin
    //Interface
    //Ancestor that is in another package and that is not already inserted
    //is added to the diagram.
    IsAllClasses:= true; // for testing
    if (not IsAllClasses) and  Assigned((E as TInterface).Ancestor) and
      (TInterface(E).Ancestor.Owner<>E.Owner) and
      (GetBox(TInterface(E).Ancestor.FullName) = nil) then
        Panel.AddManagedObject(InCreateBox((E as TInterface).Ancestor, TRtfdInterface));
    if GetBox(E.FullName) = nil then
      Panel.AddManagedObject(InCreateBox(E, TRtfdInterface));
  end else if E is TObjekt then
    if GetBox(E.FullName) = nil then
      Panel.AddManagedObject(InCreateBox(E, TRtfdObject))
end;

//Make arrows between boxes
procedure TRtfdDiagram.ResolveAssociations;
var
  i, p : integer;
  CBox: TRtfdClass;
  aClass: TClass;
  IBox : TRtfdInterface;
  A : TAttribute;
  OBox: TRtfdObject;

  UBox : TRtfdUnitPackage;
  U : TUnitPackage;
  Dep : TUnitDependency;

  Mi : IModelIterator;
  DestBox: TRtfdBox;
  aJavaObject: TComJavaObject;
  s, Agg, Ass, Generic, Boxname: string;
  AttributeConnected: boolean;

begin
  Panel.DeleteNotEditedConnections;
  Panel.DeleteObjectConnections;
  for i:= 0 to BoxNames.Count - 1 do
    if (BoxNames.Objects[I] is TRtfdClass) then begin //Class
      CBox:= (BoxNames.Objects[I] as TRtfdClass);
      //Ancestor
      if Assigned((CBox.Entity as TClass).Ancestor) then begin
        aClass:= CBox.Entity as TClass;
        if assigned(aClass) then begin
          DestBox := GetBox( aClass.Ancestor.FullName );
          if Assigned(DestBox) then
            Panel.ConnectObjects(CBox, DestBox, asInheritends);
        end;
      end;
      //Implements
      Mi := (CBox.Entity as TClass).GetImplements;
      while Mi.HasNext do begin
        s:= Mi.Next.FullName;
        DestBox := GetBox(s);
        if Assigned(DestBox) then
          Panel.ConnectObjects(CBox, DestBox, asImplements);
      end;
      p:= Pos('$', CBox.Entity.Name);
      if p > 0 then begin
        DestBox:= GetBox(copy(CBox.Entity.Name, 1, p-1));
        if Assigned(DestBox) then
          Panel.ConnectObjects(DestBox, CBox, asAssociation2);
      end;

      //Attributes associations
      AttributeConnected:= false;
      Mi := (CBox.Entity as TClass).GetAttributes;
      while Mi.HasNext do begin
        AttributeConnected:= false;
        A := TAttribute(Mi.Next);
        //Avoid arrows that points to themselves, also associations to ancestor (double arrows)
        if Assigned(A.TypeClassifier) then
          if (A.TypeClassifier = (CBox.Entity as TClass).Ancestor) and
              assigned(getBox(A.TypeClassifier.Name)) then
            A.Connected:= true
          else begin
            s:= A.TypeClassifier.Fullname;
            if IsSimpleTypeOrString(s) then continue;
            Generic:= GenericOf(s);
            if Generic <> '' then begin // Vector<E>, Stack<E>, ArrayList<E>,...
              DestBox:= GetBox(Generic);
              if Assigned(DestBox) and (Panel.HaveConnection(CBox, DestBox) = -1) and (DestBox.Entity.Name = Generic) then
                Panel.ConnectObjects(CBox, DestBox, asAggregation1);
            end else if Pos('[]', s) > 0 then begin // Typ[]
              Agg:= WithoutArray(s);
              DestBox:= GetBox(Agg);
              if not assigned(DestBox) and (Pos('.', Agg) = 0) and (CBox.Entity.Package <> '') then begin
                Ass:= CBox.Entity.Package + '.' + Agg;
                DestBox:= GetBox(Ass);
              end;
              if Assigned(DestBox) and (Panel.HaveConnection(CBox, DestBox) = -1) then
                Panel.ConnectObjects(CBox, DestBox, asAggregation1)
            end else begin
              Ass:= s;
              DestBox:= GetBox(Ass);
              if not assigned(DestBox) and (Pos('.', Ass) = 0) and (CBox.Entity.Package <> '') then begin
                Ass:= CBox.Entity.Package + '.' + Ass;
                DestBox:= GetBox(Ass);
              end;
              if Assigned(DestBox) then
                if Panel.HaveConnection(CBox, DestBox) = -1  then
                  Panel.ConnectObjects(CBox, DestBox, asAssociation2)
                else begin
                  p:= Panel.HaveConnection(DestBox, CBox, asAssociation2);
                  if p > -1 then
                    Panel.SetConnection(p, asAssociation3)
                end;
            end;
            if assigned(DestBox) and (Panel.HaveConnection(CBox, DestBox) > -1) and DestBox.Entity.IsVisible then begin
              A.Connected:= true;
              AttributeConnected:= true;
            end;
          end;
      end;
      if AttributeConnected then
        CBox.RefreshEntities;
    end else if (BoxNames.Objects[I] is TRtfdInterface) then begin
      //Interface
      IBox := (BoxNames.Objects[I] as TRtfdInterface);
      //Ancestor
      if Assigned((IBox.Entity as TInterface).Ancestor) then begin
        DestBox := GetBox( (IBox.Entity as TInterface).Ancestor.FullName);
        if Assigned(DestBox) then
          Panel.ConnectObjects(IBox, DestBox, asInheritends);
      end;
    end else if (BoxNames.Objects[I] is TRtfdUnitPackage) then begin
      //Unit
      UBox := (BoxNames.Objects[I] as TRtfdUnitPackage);
      U := UBox.Entity as TUnitPackage;
      Mi := U.GetUnitDependencies;
      while Mi.HasNext do begin
        Dep := Mi.Next as TUnitDependency;
        if Dep.Visibility = viPublic then begin
          DestBox:= GetBox( Dep.myPackage.FullName );
          if Assigned(DestBox) then
            Panel.ConnectObjects(UBox, DestBox, asAssociation2);
        end;
      end;
    end else if BoxNames.Objects[i] is TRtfdObject then begin
      // connect Objects to Classes
      OBox:= (BoxNames.Objects[I] as TRtfdObject);
      if Assigned(OBox.Entity) then begin
        aJavaObject:= ComJava.GetObject(OBox.Entity.Name);
        if assigned(aJavaObject) then begin
          boxname:= aJavaObject.ClassRef.getGenericTyp;
          DestBox:= GetBox(boxname);
          if DestBox = nil then
            DestBox:= GetBox(aJavaObject.ClassRef.Name);
          if Assigned(DestBox) then
            Panel.ConnectObjects(OBox, DestBox, asInstanceOf);
        end;
      end;
    end;
  Panel.ShowAll;
end;

// make arrows between Objects
procedure TRtfdDiagram.ResolveObjectAssociations;
  var
    i, j, k: integer;
    s1, s2: string;
    SL1, SL_V: TStringList;
    OBox: TRtfdObject;
    DestBox: TRtfdBox;
    aModelObject: TObjekt;
    aJavaObject: TComJavaObject;
begin
  try
    Panel.DeleteObjectConnections;
    for i:= 0 to BoxNames.Count - 1 do begin
      if assigned(BoxNames.Objects[i]) and (BoxNames.Objects[i] is TRtfdObject) then begin  // reconnect Objects
        OBox:= BoxNames.Objects[i] as TRtfdObject;
        if Assigned(OBox.Entity) and (OBox.Entity is TObjekt) then begin
          aModelObject:= OBox.Entity as TObjekt;
          aJavaObject:= ComJava.GetObject(OBox.Entity.Name);
          if assigned(aJavaObject) then begin
            SL_V:= aJavaObject.getObjectAttributeValues(false);
            for j:= 0 to SL_V.Count - 1 do begin
              s1:= SL_V[j];
              if Pos('{', s1) + Pos ('[', s1) = 1 then   // array or ArrayList
                s1:= copy(s1, 2, Length(s1)-2);
              SL1:= split(',', s1);
              for k:= 0 to SL1.Count - 1 do begin
                s2:= trim(SL1[k]);
                if s2 <> 'null' then begin
                  DestBox:= GetBox(s2);
                  if Assigned(DestBox) then
                    Panel.ConnectObjects(OBox, DestBox, asAssociation2);
                end;
              end;
              FreeAndNil(SL1);
            end;
            aModelObject.RefreshEntities;
            FreeAndNil(SL_V);
          end else
            FConfiguration.Log('TRtfdDiagram.ResolveObjectAssociations A: aJavaObject not assigned');
        end else
          FConfiguration.Log('TRtfdDiagram.ResolveObjectAssociations B: not assigned OBox.Entity');
      end;
    end;
  except
    on e: exception do
      FConfiguration.Log('TRtfdDiagram.ResolveObjectAssociations C: ', e);
  end;
  Panel.ShowAll;
end;

procedure TRtfdDiagram.SetPackage(const Value: TAbstractPackage);
begin
  if Assigned(FPackage) and (FPackage is TUnitPackage) then
    FPackage.RemoveListener(IAfterUnitPackageListener(Self));
  inherited SetPackage(Value);
  if Assigned(FPackage) then begin
    if (FPackage is TUnitPackage)
      then FPackage.AddListener(IAfterUnitPackageListener(Self));
    if (FPackage is TLogicPackage)
      then FPackage.AddListener(IAfterUnitPackageListener(Self));
  end;
  if Assigned(Frame.ScrollBox) and assigned(FConfiguration) then begin
    Frame.ScrollBox.HorzScrollBar.Position := 0;
    Frame.ScrollBox.VertScrollBar.Position := 0;
  end;
end;

procedure TRtfdDiagram.UnitPackageAfterAddChild(Sender, NewChild: TModelEntity);
begin
  if (NewChild is TClass) or (NewChild is TInterface) or (NewChild is TObjekt) then
    AddBox(NewChild);
end;

procedure TRtfdDiagram.UnitPackageAfterChange(Sender: TModelEntity);
begin
end;

procedure TRtfdDiagram.UnitPackageAfterEntityChange(Sender: TModelEntity);
begin
end;

procedure TRtfdDiagram.UnitPackageAfterRemove(Sender: TModelEntity);
begin
end;

function TRtfdDiagram.JavaInsteadClass(SL: TStringList): TStringList;
  var i: Integer; aFile, Ext: string;
begin
  i:= 0;
  while i < SL.Count do begin
    Ext:= ExtractFileExt(SL[i]);
    if Ext = '.class' then begin
      aFile:= ChangeFileExt(SL[i], '.java');
      if SL.IndexOf(aFile) > 0 then
        SL.Delete(i)
      else if FileExists(aFile) then
        SL.Strings[i]:= aFile
      else
        inc(i);
    end else
      inc(i);
  end;
  Result:= SL;
end;

procedure TRtfdDiagram.StoreDiagram(Filename: string);
var
  Ini : TMemIniFile;
  I, i1, p, comments: integer;
  Box: TRtfdBox;
  S, C, fname, s1, path : string;
  Connections: TList;
  Conn: TConnection;
  Values: TStrings;
  aJavaObject: TComJavaObject;
  Files: TStringList;

begin
  Values:= TStringList.Create;
  if FileExists(Filename) then begin
    Ini:= TMemIniFile.Create(Filename);
    Ini.ReadSectionValues('Window', Values);
    FreeAndNil(Ini);
  end;
  DeleteFile(Filename);

  Ini:= TMemIniFile.Create(Filename, TEncoding.UTF8);
  Files:= TStringList.Create;
  Files.Duplicates:= dupIgnore;
  Files.Sorted:= true;
  try
    try
      // Files
      path:= ExtractFilePath(Filename);
      if isUNC(Path) then
        Path:= '';
      i1:= 0;
      Files.AddStrings(Frame.Diagram.Model.Modelroot.Files);
      Files:= JavaInsteadClass(Files);
      for i:= 0 to Files.Count-1 do begin
        fname:= FConfiguration.RemovePortableDrive(Files[i], path);
        Ini.WriteString('Files', 'File' + IntToStr(i1), fname);
        inc(i1);
      end;

      //Boxes
      comments:= 0;
      for i:= 0 to BoxNames.Count - 1 do
        if BoxNames.Objects[i] is TRtfdObject then begin
          // Objects
          Box:= BoxNames.Objects[i] as TRtfdObject;
          S:= 'Object: ' + Package.FullName + ' - ' + Box.Entity.FullName;
          Ini.WriteInteger(S, 'X', PPIUnScale(Box.Left));
          Ini.WriteInteger(S, 'Y', PPIUnScale(Box.Top));
          //Ini.WriteInteger(S, 'MinVis', Integer(Box.MinVisibility));
          aJavaObject:= ComJava.GetObject(Box.Entity.Name);
          Ini.WriteString(S, 'Name', Box.Entity.Name);
          if assigned(aJavaObject)
            then Ini.WriteString(S, 'Typ', aJavaObject.ClassRef.ImportTyp)
            else Ini.WriteString(S, 'Typ','unknown')
        end else if BoxNames.Objects[i] is TRtfdCommentBox then begin
          Box:= BoxNames.Objects[i] as TRtfdBox;
          S:= Box.Entity.FullName;
          Ini.WriteInteger(S, 'X', PPIUnScale(Box.Left));
          Ini.WriteInteger(S, 'Y', PPIUnScale(Box.Top));
          Ini.WriteInteger(S, 'W', PPIUnScale(Box.Width));
          Ini.WriteInteger(S, 'H', PPIUnScale(Box.Height));
          s1:= (Box as TRtfdCommentBox).TrMemo.Text;
          Ini.WriteString(S, 'Comment', '_;_' + ReplaceStr(s1, #13#10, '_;_'));
        end else begin
          // Class, Stereotype, Interface
          Box:= BoxNames.Objects[i] as TRtfdBox;
          S:= 'Box: ' + Package.FullName + ' - ' + Box.Entity.FullName;
          Ini.WriteInteger(S, 'X', PPIUnScale(Box.Left));
          Ini.WriteInteger(S, 'Y', PPIUnScale(Box.Top));
          Ini.WriteInteger(S, 'MinVis', Integer(Box.MinVisibility));
          Ini.WriteInteger(S, 'ShowParameter', Box.ShowParameter);
          Ini.WriteInteger(S, 'SortOrder', Box.SortOrder);
          Ini.WriteInteger(S, 'ShowIcons', Box.ShowIcons);
          if Box.TypeBinding <> '' then
            Ini.WriteString(S, 'TypeBinding', Box.TypeBinding);
        end;

      //Diagram stuff
      s:= 'Diagram';
      Ini.WriteInteger(S, 'comments', comments);
      Ini.WriteInteger(S, 'OffsetX', Frame.ScrollBox.VertScrollBar.Position);
      Ini.WriteInteger(S, 'OffsetY', Frame.ScrollBox.HorzScrollBar.Position);

      Ini.WriteInteger(S, 'Visibility', Integer(VisibilityFilter));
      Ini.WriteInteger(S, 'ShowParameter', Showparameter);
      Ini.WriteInteger(S, 'SortOrder', SortOrder);
      Ini.WriteInteger(S, 'ShowIcons', ShowIcons);
      Ini.WriteInteger(S, 'ShowConnections', ShowConnections);
      Ini.WriteString (S, 'Fontname', Font.Name);
      Ini.WriteInteger(S, 'Fontsize', PPIUnScale(Font.Size));
      Ini.WriteBool(S, 'ShowObjectDiagram', ShowObjectDiagram);

      // Connections
      s:= 'Connections';
      Connections:= Panel.GetConnections;
      for i:= 0 to Connections.Count-1 do begin
        Conn:= TConnection(Connections[i]);
        with conn do
          c:= (FFrom as TRtfdBox).Entity.FullName + '#' +
              (FTo as TRtfdBox).Entity.FullName + '#' +
              ArrowStyleToString(ArrowStyle) + '#' +
              HideCrLf(MultiplicityA) + '#' + Relation + '#' + HideCrLf(MultiplicityB) + '#' +
              IntToStr(RecursivCorner) + '#' + BoolToStr(isTurned) + '#' +
              BoolToStr(isEdited) + '#' +
              HideCrLf(RoleA) + '#' + HideCrLf(RoleB) + '#' +
              BoolToStr(ReadingOrderA) + '#' + BoolToStr(ReadingOrderB);
         Ini.WriteString(S, 'V' + IntToStr(i), c);
      end;
      FreeAndNil(Connections);

      // Interactive
      s:= 'Interactive';
      for i:= 0 to Interactive.InteractiveEditor.Lines.Count - 1 do
        Ini.WriteString(s, 'I' + IntToStr(i), Interactive.InteractiveEditor.Lines[i]);

      // Window
      for i:= 0 to Values.Count-1 do begin
        s:= Values[i];
        p:= Pos('=', s);
        Ini.WriteString('Window', copy(s, 1, p-1), copy(s, p+1, length(s)));
      end;
      Ini.UpdateFile;
    except
      on e: Exception do begin
        ErrorMsg(e.Message);
      end;
    end;
  finally
    FreeAndNil(Ini);
    FreeAndNil(Files);
  end;

  FreeAndNil(Values);
  Panel.IsModified:= false;
end;

procedure TRtfdDiagram.FetchDiagram(Filename: string);
var
  Ini: TMemIniFile;
  i, j, p, CountObjects: integer;
  Box, Box1, Box2: TRtfdBox;
  s, c, aFile, b1, b2, path: string;
  FilesPre, FilesPost, Sections, SL: TStringList;
  AlleBoxen: TList;

  UnitPackage: TUnitPackage;
  theClassname: string;
  theObjectname: string;
  aModelObject: TObjekt;

  myObject: TComJavaObject;
  Attributes: TConnectionAttributes;
  aClass: TClass;
  aClassifier: TClassifier;

  BoxShowParameter, BoxSortorder, BoxShowIcons: integer;
  BoxTypeBinding: string;
  CommentBox: TRtfdCommentBox;
  VisibilityFilterAsInteger: integer;
  ShadowWidth: integer;
begin
  Filename:= ExpandFileName(Filename);
  Ini:= TMemIniFile.Create(Filename);
  FilesPre:= TStringList.Create;
  FilesPost:= TStringList.Create;
  Attributes:= TConnectionAttributes.Create;
  try
    s:= 'Diagram';
    if not Ini.SectionExists(s) and assigned(Package) then
      s:= 'Diagram: ' + Package.FullName; // old format
    Frame.ScrollBox.VertScrollBar.Position:= Ini.ReadInteger(S, 'OffsetX', Frame.ScrollBox.VertScrollBar.Position);
    Frame.ScrollBox.HorzScrollBar.Position:= Ini.ReadInteger(S, 'OffsetY', Frame.ScrollBox.HorzScrollBar.Position);

    ShowConnections:= Ini.ReadInteger(S, 'ShowConnections', 0);
    VisibilityFilterAsInteger:= Ini.ReadInteger(S, 'Visibility', 0);
    VisibilityFilter:= TVisibility(VisibilityFilterAsInteger);
    ShowParameter:= Ini.ReadInteger(S, 'ShowParameter', ShowParameter);
    SortOrder:= Ini.ReadInteger(S, 'SortOrder', SortOrder);
    ShowIcons:= Ini.ReadInteger(S, 'ShowIcons', ShowIcons);
    ShowObjectDiagram:= Ini.ReadBool(S, 'ShowObjectDiagram', false);
    Font.Name:= Ini.ReadString(S, 'Fontname', 'Segoe UI');
    if Font.Name = 'MS Sans Serif' then Font.Name:= 'Segoe UI';
    Font.Size:= PPIScale(Ini.ReadInteger(S, 'Fontsize', 11));
    setFont(Font);

    // read files
    path:= ExtractFilePath(Filename);
    if isUNC(path)
      then path:= ''
      else SetCurrentDir(path); // due to relativ paths
    i:= 0;
    aFile:= Ini.ReadString('Files', 'File' + IntToStr(i), '');
    while aFile <> '' do begin
      aFile:= ExpandFileName(FConfiguration.AddPortableDrive(aFile, path));
      if not FileExistsCaseSensitive(aFile) then
        aFile:= ExtractFilePath(Filename) + extractFilename(aFile);
      if FileExistsCaseSensitive(aFile) and (FilesPre.IndexOf(aFile) = -1) then
        FilesPre.Add(aFile);
      inc(i);
      aFile:= Ini.ReadString('Files', 'File' + IntToStr(i), '');
    end;
    FilesPre:= JavaInsteadClass(FilesPre);
    UMLForm.MainModul.LoadProject(FilesPre);

    // read classes
    for i:= BoxNames.Count - 1 downto 0 do begin
      Box:= BoxNames.Objects[i] as TRtfdBox;
      if (BoxNames.Objects[i] is TRtfdClass) or (BoxNames.Objects[i] is TRtfdInterface) then begin
        S:= 'Box: ' + Package.FullName + ' - ' + Box.Entity.FullName;
        if not Ini.SectionExists(s) then
          S:= 'Box: ' + ' - ' + Box.Entity.FullName;
        if Ini.SectionExists(S) then begin
          Box.Left:= PPIScale(Ini.ReadInteger(S, 'X', Box.Left));
          Box.Top := PPIScale(Ini.ReadInteger(S, 'Y', Box.Top));
          Box.MinVisibility:= TVisibility(Ini.ReadInteger(S, 'MinVis', VisibilityFilterAsInteger));
          BoxShowParameter:= Ini.ReadInteger(S, 'ShowParameter', ShowParameter);
          BoxSortOrder:= Ini.ReadInteger(S, 'SortOrder', SortOrder);
          BoxShowIcons:= Ini.ReadInteger(S, 'ShowIcons', ShowIcons);
          BoxTypeBinding:= Ini.ReadString(S, 'TypeBinding', '');
          ShadowWidth:= FConfiguration.ShadowWidth;
          Box.SetParameters(BoxShowParameter, BoxSortOrder, BoxShowIcons,
                            ShadowWidth, Font, BoxTypeBinding);
          // reduce to necessary files
          j:= 0;
          while j < FilesPre.Count do begin
            path:= ReplaceStr(withoutGeneric(Box.Entity.Fullname), '.', '\');
            if EndsWith(FilesPre.Strings[j], path + '.java') or
               EndsWith(FilesPre.Strings[j], path + '.class') then
            begin
              FilesPost.Add(FilesPre.Strings[j]);
              FilesPre.Delete(j);
              j:= FilesPre.Count;
            end else
              inc(j);
          end;
        end else
        //if not ShowAll then
          TManagedObject(Panel.FindManagedControl(Box)).Selected:= true;
      end;
    end;
    DeleteSelectedControls(nil);
    UMLForm.MainModul.Model.ModelRoot.Files.Assign(FilesPost);

    // read objects
    CountObjects:= 0;
    Sections:= TStringList.Create;
    Ini.ReadSections(Sections);
    for i:= 0 to Sections.Count - 1 do
      if Pos('Object', Sections[i]) > 0 then begin
        S:= Sections[i];
        UnitPackage:= Model.ModelRoot.FindUnitPackage('Default'); // TODO nicht bei package
        if not assigned(UnitPackage) then
          UnitPackage:= Model.ModelRoot.AddUnit('Default');
        ComJava.Transfer(UnitPackage.ClassImports, UnitPackage.FullImports, getSourcepath);
        theClassname := Ini.ReadString(S, 'Typ', '');
        theObjectname:= Ini.ReadString(S, 'Name', '');
        myObject:= ComJava.GetObject(theObjectname);

        aClass:= nil;
        if assigned(myObject) then begin
          j:= BoxNames.IndexOf(theClassname);
          if j >= 0 then
            aClass:= ((BoxNames.Objects[j] as TRtfdClass).Entity) as TClass
        end;
        if not assigned(aClass) then begin
          aClassifier:= UnitPackage.FindClassifier(theClassname, TClass, true);
          if assigned(aClassifier) then
            aClass:= aClassifier as TClass
          else begin
            aClass:= TClass.Create(nil);
            aClass.Name:= theClassname;
            UnitPackage.AddClass(aClass);
          end;
        end;

        if assigned(myObject) then begin
          aModelObject:= UnitPackage.AddObject(theObjectname, aClass);
          ShowAttributes(myObject, aModelObject);

          j:= BoxNames.IndexOf(theObjectname);
          if j = -1 then begin
            AddBox(aModelObject);
            j:= BoxNames.IndexOf(theObjectname);
          end;

          if j > -1 then begin
            Box:= BoxNames.Objects[j] as TRtfdBox;
            Box.Left:= PPIScale(Ini.ReadInteger(S, 'X', Box.Left));
            Box.Top := PPIScale(Ini.ReadInteger(S, 'Y', Box.Top));
            Box.Font.Assign(Font);
            p:= BoxNames.IndexOf(theClassname);
            if p > 0 then begin
              Box1:= BoxNames.Objects[p] as TRtfdBox;
              Box.Font.Size:= Box1.Font.Size;
            end else
              Box.Font.Size:= PPIScale(Font.Size);
          end;
          inc(CountObjects);
        end;
      end;
    Panel.DeleteConnections;

    // read comments
    for i:= 0 to Sections.Count - 1 do begin
      if Pos('Comment', Sections[i]) = 1 then begin
        S:= Sections[i];
        j:= BoxNames.IndexOf(S);
        if j = -1 then begin
          CommentBox:= TRtfdCommentBox.Create(Panel, S, Frame, viPublic, HANDLESIZE);
          BoxNames.AddObject(S, CommentBox);
          Panel.AddManagedObject(CommentBox);
          j:= BoxNames.IndexOf(S);
        end;

        if j > -1 then begin
          Box:= BoxNames.Objects[j] as TRtfdBox;
          Box.Left:= PPIScale(Ini.ReadInteger(S, 'X', Box.Left));
          Box.Top := PPIScale(Ini.ReadInteger(S, 'Y', Box.Top));
          Box.Width:= PPIScale(Ini.ReadInteger(S, 'W', Box.Width));
          Box.Height:= PPIScale(Ini.ReadInteger(S, 'H', Box.Height));
          Box.Font.Assign(Font);
          s:= Ini.ReadString(S, 'Comment', '');
          if copy(s, 1, 3) = '_;_' then
            delete(s, 1, 3);
          (Box as TRtfdCommentBox).TrMemo.Text:= ReplaceStr(s, '_;_', #13#10);
        end;
      end;
    end;
    FreeAndNil(Sections);
    Panel.DeleteConnections;
    AlleBoxen:= Panel.GetManagedObjects;

    S:= 'Connections';
    i:= 0;
    repeat
      c:= Ini.ReadString(S, 'V' + IntToStr(i), '');
      if c <> '' then begin
        Box1:= nil;
        Box2:= nil;
        SL:= Split('#', c);
        b1:= trim(SL[0]);
        b2:= trim(SL[1]);
        Attributes.ArrowStyle:= StringToArrowStyle(SL[2]);
        if SL.Count > 7 then begin // new format
          Attributes.MultiplicityA:= UnhideCrLf(SL[3]);
          Attributes.Relation:= SL[4];
          Attributes.MultiplicityB:= UnhideCrLf(SL[5]);
          Attributes.RecursivCorner:= StrToInt(SL[6]);
          Attributes.isTurned:= StrToBool(SL[7]);
        end;
        if (SL.Count > 8) and (SL[8] <> '')
          then Attributes.isEdited:= StrToBool(SL[8])
          else Attributes.isEdited:= false;
        if (SL.Count > 12) and (SL[12] <> '') then begin
          Attributes.RoleA:= UnhideCrLf(SL[9]);
          Attributes.RoleB:= UnhideCrLf(SL[10]);
          Attributes.ReadingOrderA:= StrToBool(SL[11]);
          Attributes.ReadingOrderB:= StrToBool(SL[12]);
        end;

        for j:= 0 to AlleBoxen.Count-1 do begin
          Box:= TRtfdBox(AlleBoxen[j]);
          if Box.Entity.FullName = b1 then Box1:= Box;
          if Box.Entity.FullName = b2 then Box2:= Box;
        end;
        Panel.ConnectObjects(Box1, Box2, Attributes);
        FreeAndNil(SL);
      end;
      inc(i);
    until c = '';
    FreeAndNil(AlleBoxen);
    Panel.SetConnections(ShowConnections);
    if CountObjects = 0
      then SetShowObjectDiagram(false)
      else SetShowObjectDiagram(ShowObjectDiagram);
    UpdateAllObjects;

    s:= 'Interactive';
    if assigned(Interactive) and assigned(Interactive.InteractiveEditor) and
       assigned(Interactive.InteractiveEditor.Lines) then begin
      Interactive.InteractiveEditor.BeginUpdate;
      Interactive.InteractiveEditor.Lines.Clear;
      SL:= TStringList.Create;
      Ini.ReadSectionValues(s, SL);
      for i:= 0 to SL.Count - 1 do begin
        c:= SL.Strings[i];
        p:= Pos('=', c);
        delete(c, 1, p);
        p:= Pos('<#>', c);  // old format
        if p > 0 then c:= copy(c, 1, p-1);
        Interactive.InteractiveEditor.Lines.Add(c);
      end;
      FreeAndNil(SL);
      Interactive.InteractiveEditor.EndUpdate;
    end;
  finally
    FreeAndNil(Ini);
    FreeAndNil(FilesPre);
    FreeAndNil(FilesPost);
    FreeAndNil(Attributes);
  end;
  Panel.RecalcSize;
  Panel.IsModified := False;
  Panel.ShowAll;
  if Panel.CanFocus then
    Panel.SetFocus;
end;

procedure TRtfdDiagram.DoLayout;
begin
  if BoxNames.Count > 0 then begin
    Panel.Hide;
    var Layout:= TSugiyamaLayout.Create(Panel.GetManagedObjects, Panel.GetConnections);
    try
      Layout.Execute;
    finally
      Panel.Show;
      FreeAndNil(Layout);
    end;
    Panel.IsModified:= true;
    Panel.RecalcSize;
    Panel.ShowAll;
  end;
end;

function TRtfdDiagram.GetBox(typ: string): TRtfdBox;
begin
  typ:= withoutGeneric(typ);
  var i:= 0;
  while i < BoxNames.Count do begin
    var s:= withoutGeneric( BoxNames.Strings[i]);
    if typ = s then
      break;
    inc(i);
  end;
  if i = BoxNames.Count
    then Result:= nil
    else Result:= BoxNames.Objects[i] as TRtfdBox;
end;

procedure TRtfdDiagram.SetVisibilityFilter(const Value: TVisibility);
  var B: TRtfdBox; L: TList; i: integer;
begin
  if Panel.CountSelectedControls > 0
    then L:= Panel.GetSelectedControls
    else L:= Panel.GetManagedObjects;
  Panel.Hide;
  try
    for i:= 0 to L.Count-1 do
      if TObject(L[i]) is TRtfdBox then begin
        B:= TObject(L[i]) as TRtfdBox;
        if B.MinVisibility <> Value then begin
          B.MinVisibility:= Value;
          Panel.isModified:= true;
        end;
      end;
  finally
    FreeAndNil(L);
  end;
  Panel.RecalcSize;
  Panel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetShowView(Value: integer);
  var i, objs: integer; L: TList; B: TRtfdBox;
begin
  L:= Panel.GetManagedObjects;
  Panel.Hide;
  try
    objs:= 0;
    for i:= 0 to L.Count-1 do
      if TObject(L[i]) is TRtfdObject then
        inc(objs);
    if (objs = 0) and (Value = 1) then
      Value:= 2;
    for i:= 0 to L.Count-1 do begin
      B:= TObject(L[i]) as TRtfdBox;
      case value of
        0: B.MinVisibility:= TVisibility(0);
        1: if B is TRtfdClass
             then B.MinVisibility:= TVisibility(4)
             else B.MinVisibility:= TVisibility(0);
        2: B.MinVisibility:= TVisibility(4);
      end;
    end;
  finally
    FreeAndNil(L);
  end;
  Panel.isModified:= true;
  Panel.RecalcSize;
  Panel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetShowParameter(const Value: integer);
  var i: integer; L: TList; B: TRtfdBox;
begin
  if Panel.CountSelectedControls > 0
    then L:= Panel.GetSelectedControls
    else L:= Panel.GetManagedObjects;
  Panel.Hide;
  try
    for i:= 0 to L.Count-1 do begin
      B:= TObject(L[i]) as TRtfdBox;
      if B.ShowParameter <> Value then begin
        B.ShowParameter:= Value;
        Panel.isModified:= true;
      end;
    end;
  finally
    FreeAndNil(L);
  end;
  Panel.RecalcSize;
  Panel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetSortOrder(const Value: integer);
  var i: integer; L: TList; B: TRtfdBox;
begin
  if Panel.CountSelectedControls > 0
    then L:= Panel.GetSelectedControls
    else L:= Panel.GetManagedObjects;
  Panel.Hide;
  try
    for i:= 0 to L.Count-1 do begin
      B:= TObject(L[i]) as TRtfdBox;
      if B.SortOrder <> Value then begin
        B.SortOrder:= Value;
        Panel.IsModified:= true;
      end;
    end;
  finally
    FreeAndNil(L);
  end;
  Panel.RecalcSize;
  Panel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetShowIcons(const Value: integer);
  var i: integer; L: TList; B: TRtfdBox;
begin
  if Panel.CountSelectedControls > 0
    then L:= Panel.GetSelectedControls
    else L:= Panel.GetManagedObjects;
  Panel.Hide;
  try
    for i:= 0 to L.Count-1 do begin
      B:= TObject(L[i]) as TRtfdBox;
      if (B is TRtfdObject) and FConfiguration.ObjectsWithoutVisibility then
        continue;
      if B.ShowIcons <> Value then begin
        B.ShowIcons:= Value;
        Panel.IsModified:= true;
      end;
    end;
  finally
    FreeAndNil(L);
  end;
  Panel.RecalcSize;
  Panel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetFont(const aFont: TFont);
  var i: integer; L: TList; B: TRtfdBox;
begin
  inherited;
  L:= Panel.GetManagedObjects;
  try
    for i:= 0 to L.Count - 1 do begin
      B:= TObject(L[i]) as TRtfdBox;
      if assigned(B) and assigned(B.Font) then
        B.Font.Assign(aFont);
    end;
  finally
    FreeAndNil(L);
  end;
  Panel.Font.Assign(aFont);
end;

function TRtfdDiagram.GetFont: TFont;
  var Control: TControl;
begin
  if Panel.hasSelectedControls
    then Control:= Panel.GetFirstSelected
    else Control:= nil;
  if assigned(Control)
    then Result:= (Control as TRtfdBox).Font
    else Result:= inherited getFont;
end;

procedure TRtfdDiagram.GetDiagramSize(var W, H: integer);
begin
  Panel.GetDiagramSize(W, H);
end;

//Returns list with str = 'x1,y1,x2,y2', obj = modelentity
function TRtfdDiagram.GetClickAreas: TStringList;
var
  I : integer;
  Box : TRtfdBox;
  S : string;
begin
  Result := TStringList.Create;
  for I := 0 to BoxNames.Count-1 do begin
    Box := BoxNames.Objects[I] as TRtfdBox;
    S := IntToStr(Box.Left) + ',' + IntToStr(Box.Top) + ',' +
      IntToStr(Box.Left + Box.Width) + ',' + IntToStr(Box.Top + Box.Height);
    Result.AddObject(S,Box.Entity);
  end;
end;

procedure TRtfdDiagram.ClassEditSelectedDiagramElements(Sender: TObject);
  var Pathname, aPackage: string; Form: TFEditForm; aBox: TRtfdBox;
      C: TControl;
begin
  C:= (Sender as TControl);
  aBox:= (C as TRtfdBox);
  if ((C is TRtfdClass) or (C is TRtfdInterface)) and Assigned(GetBox(aBox.Entity.FullName)) then begin
    Pathname:= ChangeFileExt(aBox.GetPathname, '.java');
    if (Length(Pathname) > 0) and (Pos(FConfiguration.TempDir, Pathname) = 0) and FileExists(Pathname) then begin
      Form:= (FJava.getTDIWindowType(Pathname, '%E%') as TFEditForm);
      if assigned(Form) and Form.Modified then
        FJava.DoSave(Form, false);
      // Mr. Ehrlich: Problem implementing a class derived from an abstract class
      if not myJavaCommands.HasValidClass(Pathname) then begin
        aPackage:= aBox.Entity.Package;
        if assigned(Form)
          then myJavaCommands.CompileForm(Form)
          else myJavaCommands.Compile(Pathname, aPackage);
        UMLForm.Refresh;
      end;
      if assigned(FJava.ActiveTDIChild) and (FJava.ActiveTDIChild.FormTag = 2) then
        FJava.PrepareClassEdit(Pathname, 'Refresh', FJava.ActiveTDIChild as TFUMLForm);
    end;
  end;
  Panel.ClearSelection;
end;

procedure TRtfdDiagram.ClassEditSelectedDiagramElements;
begin
  var C:= Panel.GetFirstSelected;
  if not assigned(C) then
    C:= Panel.GetFirstManaged;
  if Assigned(C) then
    ClassEditSelectedDiagramElements(C);
end;

procedure TRtfdDiagram.SourceEditSelectedDiagramElementsControl(C: TControl);
begin
  Panel.ClearSelection;
  if assigned(C) and (C is TRtfdBox) then begin
    var FileName:= ChangeFileExt((C as TRtfdBox).GetPathname, '.java');
    if FileExists(FileName) then
      FJava.SwitchWindowWithSearch(FileName);
  end;
end;

procedure TRtfdDiagram.DeleteSelectedControls(Sender: TObject);
var
  C: TControl;
  L: TObjectList;
  Box: TRtfdBox;
  U : TUnitPackage;
  i, k: integer;
  key, classname: string;
  aObject: TObject;

  function CountClassesWith(const classname: string): integer;
    var
      C: TControl;
      L: TList;
      Box: TRtfdBox;
      s: string;
      i: integer;
  begin
    Result:= 0;
    L:= Panel.GetManagedObjects;
    for i:= 0 to L.Count-1 do begin
      C:= TControl(L[i]);
      if C is TRtfdBox then begin
        Box:= C as TRtfdBox;
        if (C is TRtfdClass) or (C is TRtfdInterface) then begin
          s:= Box.Entity.Name;
          delete(s, 1, LastDelimiter('.', s));
          //if Pos(classname, s) = 1 then inc(Result);
          if classname = s then inc(Result);
        end;
      end;
    end;
    FreeAndNil(L);
  end;

begin
  L:= Panel.GetSelectedControls;
  try
    for i:= 0 to L.Count-1 do begin
      C:= L[i] as TControl;
      if C is TRtfdBox then begin
        Box:= C as TRtfdBox;
        if (C is TRtfdClass) or (C is TRtfdInterface) then begin
          if (Box.Entity as TClassifier).Visibility = viPublic then begin
            key:= Box.GetPathname;
            k:= Frame.Diagram.Model.ModelRoot.Files.IndexOf(key);
            classname:= ChangeFileExt(ExtractFilename(key), '');
            if (k > -1) and (CountClassesWith(classname) <= 1) then
              Frame.Diagram.Model.ModelRoot.Files.Delete(k);
          end;

          key:= Box.Entity.FullName;
          k:= BoxNames.IndexOf(key);
          if k > -1 then begin
            if assigned(BoxNames.Objects[k]) then begin
              aObject:= BoxNames.Objects[k];
              FreeAndNil(aObject);
            end;
            BoxNames.Delete(k);
          end
         end
        else if (C is TRtfdObject) then begin
          key:= (C as TRtfdObject).Entity.Name;
          ShowObjectDeleted('Actor', key);
          k:= BoxNames.IndexOf(key);
          if k > -1 then begin
            if assigned(BoxNames.Objects[k]) then begin
              aObject:= BoxNames.Objects[k];
              FreeAndNil(aObject);
            end;
            BoxNames.Delete(k);
          end;
          Interactive.Executer.DelVariable(key);
          try
            ComJava.DeleteObject(key);                       // java-level
          except on e: exception do
            FConfiguration.Log('TRtfdDiagram.DeleteSelectedControls.', e);
          end;
          U:= Model.ModelRoot.FindUnitPackage('Default');
          U.DeleteObject(key);                               // diagram-level
        end else if (C is TRtfdCommentBox) then begin
          key:= (C as TRtfdCommentBox).Entity.Name;
          k:= BoxNames.IndexOf(key);
          if k > -1 then begin
            if assigned(BoxNames.Objects[k]) then begin
              aObject:= BoxNames.Objects[k];
              FreeAndNil(aObject);
            end;
            BoxNames.Delete(k);
          end;
        end;
      end;
    end;
  finally
    Panel.DeleteSelectedControls;
    FreeAndNil(L);
  end;
  ResolveObjectAssociations; // nötig?
end;

procedure TRtfdDiagram.DeleteSelectedControlsAndRefresh;
begin
  LockFormUpdate(UMLForm);
  DeleteSelectedControls(nil);
  UMLForm.Refresh;
  UnLockFormUpdate(UMLForm);
end;

procedure TRtfdDiagram.UnSelectAllElements;
begin
  if assigned(Panel) then
    Panel.ClearSelection;
end;

procedure TRtfdDiagram.CurrentEntityChanged;
begin
  inherited;
  var P := CurrentEntity;
  while Assigned(P) and (not (P is TAbstractPackage)) do
    P := P.Owner;
  if Assigned(P) and (P<>Package) then
    Package := P as TAbstractPackage;
  if (CurrentEntity is TClass) or (CurrentEntity is TInterface) then
    ScreenCenterEntity(CurrentEntity);
end;

function TRtfdDiagram.GetSelectedRect: TRect;
begin
  Result:= Panel.GetSelectedRect;
end;

procedure TRtfdDiagram.ScreenCenterEntity(E: TModelEntity);
begin
  for var i:= 0 to BoxNames.Count-1 do
    if TRtfdBox(BoxNames.Objects[i]).Entity = E then begin
      var Box:= TRtfdBox(BoxNames.Objects[i]);
      Frame.ScrollBox.ScrollInView(Box);
      Break;
    end;
end;

procedure TRtfdDiagram.SetShowObjectDiagram(const Value: boolean);
begin
  UMLForm.TBObjectDiagram.Down:= Value;
  inherited SetShowObjectDiagram(Value);
  Panel.SetShowObjectDiagram(Value);
end;

procedure TRtfdDiagram.RefreshDiagram;
begin
  if not PanelIsLocked then
    for var i:= 0 to BoxNames.Count - 1 do
      (BoxNames.Objects[I] as TRtfdBox).RefreshEntities;
  Panel.RecalcSize;
  Panel.ShowAll;
  Panel.IsModified := true;
end;

function TRtfdDiagram.HasAInvalidClass: boolean;
begin
  for var i:= 0 to BoxNames.Count - 1 do
    if (BoxNames.Objects[i] is TRtfdClass) then begin
      var path:= (BoxNames.Objects[i] as TRtfdClass).getPathname;
      if not FJava.IsAValidClass(path) then exit(true);
    end;
   Result:= false;
end;

procedure TRtfdDiagram.RecalcPanelSize;
begin
  Panel.RecalcSize;
end;

procedure TRtfdDiagram.SetConnections(const Value: integer);
begin
  if Value <> ShowConnections then begin
    Panel.IsModified := true;
    inherited SetConnections(Value);
    Panel.SetConnections(Value);
  end;
  inherited;
end;

procedure TRtfdDiagram.SelectAssociation;
begin
  Panel.SelectAssociation;
end;

function TRtfdDiagram.GetFrame: TAFrameDiagram;
begin
  Result:= Frame;
end;

function TRtfdDiagram.GetPanel: TCustomPanel;
begin
  Result:= Panel;
end;

procedure TRtfdDiagram.CompileOneWith(C: TControl; Compiler: string);
begin
  var aBox:= GetBox((C as TRtfdBox).Entity.FullName);
  if Assigned(aBox) then begin
    var s:= ChangeFileExt(aBox.GetPathname, '.java');
    FJava.CompileOneWith(s + '|' + aBox.Entity.Package);
  end;
end;

function TRtfdDiagram.getFilesAndPackagesFromList(L: TList): TStringList;
begin
  Result:= TStringList.Create;
  Result.Sorted:= true;
  for var i:= 0 to L.Count-1 do begin
    var aBox:= TRtfdBox(L[I]);
    if ((aBox is TRtfdClass) or (aBox is TRtfdInterface)) and
       Assigned(GetBox(aBox.Entity.FullName))
    then begin
      var s:= aBox.GetPathname;
      if Pos(FConfiguration.JavaCache, s) = 0 then
        Result.Add(ChangeFileExt(s, '.java') + '|' + aBox.Entity.package);
    end;
  end;
end;

procedure TRtfdDiagram.Run(C: TControl);
begin
  if Assigned(GetBox((C as TRtfdBox).Entity.FullName)) then begin
    var Pathname:= ChangeFileExt((C as TRtfdBox).GetPathname, '.java');
    Panel.ClearSelection;
    FJava.Run(Pathname);
  end;
end;

function TRtfdDiagram.getFileWithMain: string;
  var i: integer; aRtfdClass: TRtfdClass; aModelClass: TClass;
      it1: IModelIterator;
      Operation: UModel.TOperation;
begin
  Result:= '';
  for i:= 0 to BoxNames.Count - 1 do begin
    if BoxNames.Objects[i] is TRtfdClass then begin
      aRtfdClass:= (BoxNames.Objects[i] as TRtfdClass);
      if not (aRtfdClass.Entity is TClass) then continue;
      aModelClass:= aRtfdClass.Entity as TClass;
      if not aModelClass.IsAbstract then begin
        it1:= aModelClass.GetOperations;
        while it1.HasNext do begin
          Operation:= it1.Next as UModel.TOperation;
          if (Operation.OperationType = otProcedure) and (Operation.name = 'main') then begin
            Result:= aModelClass.Pathname;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TRtfdDiagram.getAllPathnames: TStringList;
  var L: TList; s: string; i: integer; aBox: TRtfdBox;
begin
  try
    L:= Panel.GetManagedObjects;
    Result:= TStringList.Create;
    for i:= 0 to L.Count-1 do begin
      aBox:= TRtfdBox(L[I]);
      if ((aBox is TRtfdClass) or (aBox is TRtfdInterface)) and
           Assigned(GetBox(aBox.Entity.FullName))
      then begin
        s:= aBox.GetPathname;
        if Pos(FConfiguration.JavaCache, s) = 0 then
          Result.Add(ChangeFileExt(s, '.java'));
      end;
    end;
  finally
    FreeAndNil(L);
  end;
end;

function TRtfdDiagram.getDebug: TStringList;
  var L: TList; i: integer; aBox: TRtfdBox;
begin
  try
    L:= Panel.GetManagedObjects;
    Result:= TStringList.Create;
    for i:= 0 to L.Count-1 do begin
      aBox:= TRtfdBox(L[I]);
      Result.Add(aBox.Entity.Fullname);
    end;
    Result.Add('');
 finally
    FreeAndNil(L);
  end;
end;

function TRtfdDiagram.getAllClassnames: TStringList;
  var L: TList;
begin
  try
    L:= Panel.GetManagedObjects;
    Result:= TStringList.Create;
    for var i:= 0 to L.Count - 1 do begin
      var aBox:= TRtfdBox(L[I]);
      if (aBox is TRtfdClass) and Assigned(GetBox(aBox.Entity.FullName)) then
        Result.Add(ExtractFilePath(aBox.GetPathname) +  withoutGeneric(aBox.Entity.Fullname) + '.class')
    end;
  finally
    FreeAndNil(L);
  end;
end;

function TRtfdDiagram.getFilesAndPackages(Selected: boolean): TStringList;
  var L: TList;
begin
  if Selected and (Panel.CountSelectedControls > 0)
    then L:= Panel.GetSelectedControls
    else L:= Panel.GetManagedObjects;
  try
    Result:= getFilesAndPackagesFromList(L);
  finally
    FreeAndNil(L);
  end;
end;

procedure TRtfdDiagram.ShowInheritedMethodsFromSystemClasses(C: TControl; ShowOrHide: boolean);
begin
  if C is TRtfdBox then
    (C as TRtfdBox).ShowInherited:= ShowOrHide;
  Panel.ClearSelection;
end;

function TRtfdDiagram.MakeParams(Parameter: TStringList;
              var theParams: TComJavaParams; var asString: string): boolean;
begin
  Result:= true;
  theParams:= TComJavaParams.Create;
  asString:= '';
  for var i:= 0 to Parameter.Count - 1 do begin
    var aJavaValue:= TComJavaValue(Parameter.objects[i]);
    asString:= asString + aJavaValue.AsString + ', ';
    theParams.addToParams(aJavaValue.AsString, aJavaValue.Sig);
  end;
  Delete(asString, length(asString)-1, 2);
end;

procedure TRtfdDiagram.DeleteParams(Parameter: TStringList);
begin
  for var i:= Parameter.Count - 1 downto 0 do begin
    var aJavaValue:= TComJavaValue(Parameter.objects[i]);
    FreeAndNil(aJavaValue);
  end;
  FreeAndNil(Parameter);
end;

function TRtfdDiagram.CollectClasses: boolean;
  var i: integer; ok, AllowCompiling: boolean;
      CorIName, Boxname: string;
begin
  // Compiling not allowed, wenn ObjectList.Count > 0;
  Result:= true;
  AllowCompiling:= (ComJava.ObjectList.Count = 0);
  for i:= BoxNames.Count - 1 downto 0 do begin
    Boxname:= (BoxNames.Objects[i] as TRtfdBox).Entity.Name;
    if (BoxNames.Objects[i] is TRtfdClass) then
      CorIName:= ((BoxNames.Objects[i] as TRtfdBox).Entity as TClass).Pathname
    else if (BoxNames.Objects[i] is TRtfdInterface) then
      CorIName:= ((BoxNames.Objects[i] as TRtfdInterface).Entity as TInterface).Pathname
    else
      continue;  // object
    if CorIName = '' then
      continue
    else if FConfiguration.PathForSystemClass(CorIName) then
      Result:= Result and ComJava.NewAPIClass(Boxname)
    else begin
      if AllowCompiling
        then ok:= FJava.PreCompile(nil, CorIName)
        else ok:= true;
      if ok
        then Result:= Result and ComJava.NewClass(Boxname, CorIName)
        else exit(false);
    end;
  end;
end;

procedure TRtfdDiagram.CallMain(const Classpath, aClassname: string; CallParameter: string);
  var i: integer;
      inApostroph: boolean;
      aJavaClass: TComJavaClass;
      aJavaMethod: TComJavaMethod;
      aJavaValue: TComJavaValue;
      theParams: TComJavaParams;
begin
  ComJava.Sourcepath:= Classpath;
  aJavaClass:= ComJava.GetClass(aClassname);
  if aJavaClass = nil then begin
    ErrorMsg(_(LNGUnknownClass) + ' "' + aClassname + '"');
    exit;
  end;

  if myJavaCommands.ProcessRunning then begin
    myJavaCommands.TerminateProcess:= true;
    exit;
  end;

  FJava.RunButtonToStop(true);
  myJavaCommands.ProcessRunning:= true;
  myJavaCommands.ProcessRunningComJava:= ComJava;
  inApostroph:= false;
  i:= 1;
  while i <= Length(Callparameter) do begin
    case CallParameter[i] of
      '"': inApostroph:= not inApostroph;
      ' ': if not inApostroph then CallParameter[i]:= ',';
    end;
    inc(i);
  end;
  theParams:= TComJavaParams.Create;
  try
    theParams.addToParams('{' + CallParameter + '}', '[Ljava/lang/String;');
    aJavaMethod:= TComJavaMethod.Create(aJavaClass, 'main', static, 'void', theParams, ComJava);
    try
      aJavaValue:= aJavaMethod.call(nil);
      if aJavaMethod.IsValid then begin
        ShowMethodEntered('main', 'Actor', 'Actor', '{' + CallParameter + '}');
        FreeAndNil(aJavaValue);
      end else
        ErrorMsg(aJavaMethod.Error);
    finally
      FreeAndNil(aJavaMethod);
    end;
  finally
    FreeAndNil(theParams);
    myJavaCommands.ProcessRunning:= false;
    FJava.RunButtonToStop(false);
  end;
end;

procedure TRtfdDiagram.ShowNewObject(aJavaObject: TComJavaObject);
  var U: TUnitPackage;
      aModelClass: TClassifier;
      aModelObject: TObjekt;
      B1, B2: TRtfdBox;
      aComJavaClass: TComJavaClass;
      s: string;
begin
  aModelObject:= nil;
  aComJavaClass:= nil;
  try
    B1:= nil;
    B2:= nil;
    U:= Model.ModelRoot.FindUnitPackage('Default');
    if not Assigned(U) then
      U:= Model.ModelRoot.AddUnit('Default');
    ComJava.Transfer(U.ClassImports, U.FullImports, getSourcePath);
    aComJavaClass:= aJavaObject.ClassRef;
    if aComJavaClass = nil then begin
      FConfiguration.Log('TRtfdDiagram.ShowNewObject aClass = nil, aJavaObject = ' + aJavaObject.Name);
      exit;
    end;
    aModelClass:= U.FindClassifier(aComJavaClass.GenericName);
    if not assigned(aModelClass) then begin
      aModelClass:= TClass.Create(nil);
      aModelClass.Name:= aComJavaClass.Name;
      U.AddClass(aModelClass as TClass);
    end;
    if not (aModelClass is TClass) then
      exit;

    aModelClass.GenericName:= aComJavaClass.GenericName;
    // model-level -> show object
    aModelObject:= U.AddObject(aJavaObject.Name, aModelClass as TClass);
    if Assigned(aModelClass)  then B1:= GetBox(aModelClass.GenericName);
    if Assigned(aModelObject) then B2:= GetBox(aModelObject.FullName);
    if Assigned(B1) and Assigned(B2) then begin
      B2.Top := B1.Top + B1.Height + 50 + random(30)-30;
      B2.Left:= max(B1.Left + (B1.Width - B2.Width) div 2 + random(200) - 100, 0);
      Panel.ConnectObjects(B2, B1, asInstanceOf);
    end else if Assigned(B2) then begin
      B2.Top := B2.Top  + random(30) - 30;
      B2.Left:= B2.Left + random(30) - 30;
    end;
    B2.Font.Assign(Font);
    Panel.RecalcSize;
    ShowAttributes(aJavaObject, aModelObject);
    // exceptions have arisen in here
    // should be fixed by initializing type classifier in constructor
    if (B2 = nil) and assigned(aModelObject) then
      AddBox(aModelObject);
  except
    on e: Exception do begin
      s:= 'TRtfdDiagram.ShowNewObject: ';
      if assigned(Panel)
        then s:= s + 'Panel assigned'
        else s:= s + 'Panel = nil';
      if assigned(aJavaObject)
        then s:= s + ' aJavaObject assigned'
        else s:= s + ' aJavaObject = nil';
      if assigned(aComJavaClass)
        then s:= s + ' aComJavaClass assigned'
        else s:= s + ' aComJavaClass = nil';
      if assigned(aModelObject)
        then s:= s + ' aModelObject assigned'
        else s:= s + ' aModelObject = nil';
      FConfiguration.Log(s, e);
    end;
  end;
end;

procedure TRtfdDiagram.CreateObjectForSelectedClass(Sender: TObject);
  var Caption, Title, Pathname, ParamName, ParamTyp, s, s1: string;
      p: integer;
      C: TControl;
      theClassname: string;
      theFullClassname: string;
      theObjectname: string;
      theObjectnameNew: string;
      Generic: string;
      ParameterAsString: string;

      aClass: TComJavaClass;
      aJavaObject: TComJavaObject;
      theParams: TComJavaParams;
      aJavaValue: TComJavaValue;
      Parameter: TStringList;
      MenuItem: TSpTBXItem;

  procedure ShowOnInteractive;
    var s: string; i: integer;
  begin
    FMessages.ShowTab(InteractivePath);
    s:= '';
    for i:= 0 to Parameter.Count - 1 do
      s:= s + TComJavaValue(Parameter.objects[i]).AsFormattedString + ', ';
    delete(s, length(s)-1, 2);
    AddToInteractive(theClassName + ' ' + theObjectname + ' = new ' + theClassName + '(' + s + ');');
    Interactive.Executer.AddVariable(theObjectname, theClassname, aJavaObject.CreateValue);
  end;

begin
  C:= FindVCLWindow(Frame.PopMenuClass.PopupPoint);
  FJava.DisableUpdateMenuItems;
  try
    if Assigned(C) and (C is TRtfdClass) then begin
      Panel.ClearSelection;
      Pathname:= (C as TRtfdClass).GetPathname;
      theFullClassname:= (C as TRtfdClass).Entity.Name;
      theClassname:= theFullClassname;
      ParameterAsString:= '';

      p:= Pos('$', theClassname); // inner class
      if p > 0 then delete(theClassname, 1, p);

      p:= Pos('<', theClassname);
      if p > 0 then begin
        Generic:= copy(theClassname, p+1, length(theClassname)-p-1);
        theClassname:= copy(theClassname, 1, p-1);
      end;
    
      if FConfiguration.PathForUserClass(Pathname) and
         hasJavaExtension(Pathname) and
         not FJava.PreCompile(nil, Pathname)
      then exit;
      if not CollectClasses then
        exit;   // Init und LoadClass
      if ComJava.GetClass(theFullClassname) = nil then begin
        ErrorMsg(_(LNGUnknownClass) + ' "' + theClassname + '"');
        exit;
      end;

      // get parameters for constructor
      Parameter:= TStringList.Create;
      MenuItem:= (Sender as TSpTBXItem);
      s:= MenuItem.Caption;
      if assigned(MenuItem.Parent) then begin
        s1:= MenuItem.Parent.caption;
        p:= Pos('Inherited from ', s1);
        if p > 0 then
          theClassname:= copy(s1, 16, length(s1));
      end;

      p:= FullParameters.IndexOfName(s);
      s:= FullParameters.ValueFromIndex[p];
      p:= Pos('(', s);
      delete(s, 1, p);
      if s <> ')' then begin
        s:= ReplaceStr(s, ')', ', )');
        while s <> ')' do begin
          p:= Pos(' ', s); ParamTyp := Copy(s, 1, p - 1); delete(s, 1, p);
          if ParamTyp = Generic then ParamTyp:= 'java.lang.Object';

          p:= Pos(',', s); ParamName:= Copy(s, 1, p - 1); delete(s, 1, p + 1);
          aJavaValue:= TComJavaValue.create(ParamTyp, ComJava);
          Parameter.AddObject(GetShortType(ParamTyp) + ' ' + ParamName, aJavaValue);
        end;
      end;

      if Parameter.Count = 0 then begin
        Title  := _('Attribute') + #13#10 + _(LNGValue);
        Caption:= _(LNGNameOfObject)
      end else begin
        Title  := _('Parameter') + #13#10 + _(LNGValue);
        Caption:= _('Parameter for constructor') + ' ' + theClassname + '(...)';
      end;
      theObjectname:= ComJava.GetUniqueObjectName(theClassname);  // getNewObjectName
      theObjectnameNew:= '';
      theParams:= nil;

      if (Parameter.Count = 0) or EditClass(Caption, Title, theObjectName, theObjectnameNew, Parameter) and
         MakeParams(Parameter, theParams, ParameterAsString)
      then begin
        if (theObjectnameNew <> '') and not assigned(ComJava.GetObject(theObjectnameNew)) then
          theObjectname:= theObjectnameNew;
        aClass:= ComJava.GetClass(theFullClassName);
        if assigned(aClass) then begin
          aClass.Generic:= Generic;
          aClass.Genericname:= theFullClassname;
          aJavaObject:= ComJava.NewObject(theObjectname, aClass, theParams); // java-level
          if aJavaObject.isValid then begin
            ShowMethodEntered('<init>', 'Actor', theObjectname, ParameterAsString);
            ShowOnInteractive;
            ShowNewObject(aJavaObject);
            if FConfiguration.ShowAllNewObjects then
              ShowAllNewObjectsString(theObjectname);
            UpdateAllObjects;
            ResolveObjectAssociations;
            Panel.RecalcSize;
          end else begin
            if FConfiguration.logfileInteractiveOK then
              ComJava.ExecuteCommand('logMemo#' + FConfiguration.LogfileInteractive + '#' + UdlgAbout.Version);
            ErrorMsg(aJavaObject.Error);
            FreeAndNil(aJavaObject);
          end;
        end;
        FreeAndNil(theParams);
      end;
      DeleteParams(Parameter);
    end;
  finally
    FJava.EnableUpdateMenuItems;
  end;
end;

procedure TRtfdDiagram.ShowAttributes(aJavaObject: TComJavaObject; aModelObject: TObjekt);
  var aModelAttribut: TAttribute;
      SL, SL1: TStringList;
      i: integer;
      error: string;
begin
  SL:= nil;
  SL1:= nil;
  i:= 0;
  try
    if aJavaObject = nil then begin
       FConfiguration.Log('ShowAttributes: aJavObject = nil');
       exit;
    end;
    if aModelObject = nil then begin
       FConfiguration.Log('ShowAttributes: aModelObject = nil');
       exit;
    end;
      // just create attributes, they will be shown by UpdateAllObjects
    SL:= aJavaObject.getObjectAttributes;
    // for example s =
    //   protected|0|0||int|modCount
    //   private|-1|-1||long|serialVersionUID
    //   private|0|0||java.lang.Object[]|elementData
    //   private|0|0||int|size
    //   private|-1|-1||int|MAX_ARRAY_SIZE
    aModelObject.setCapacity(SL.Count);  // nevertheless very slow!

    for i:= 0 to SL.Count - 1 do begin
      SL1:= Split('|', SL.Strings[i]);
      if SL1.Count >= 6 then begin // object has an attribute
        aModelAttribut:= aModelObject.AddAttribute(SL1[5]);  // AddAttribut can raise an exception
        aModelAttribut.Visibility:= String2Visibility(SL1[0]);
        aModelAttribut.Static:= (SL1[1] = '-1');
        aModelAttribut.IsFinal:= (SL1[2] = '-1');
        if aModelAttribut.IsFinal then
          aModelObject.hasFinal:= true;
        aModelAttribut.TypeClassifier:= FindClassifier(SL1[4]);
        if aModelAttribut.TypeClassifier = nil then begin
          aModelAttribut.TypeClassifier:= TClassifier.Create(nil); // GetClass(s1);
          aModelAttribut.TypeClassifier.Name:= SL1[4];
        end;
        aModelAttribut.TypeClassifier.aGeneric:= SL1[3];
        // Found: TypeClassifier was not set to nil in the constructor of ModelAttribute
      end;
      FreeAndNil(SL1);
    end;
    i:= 0;
  except
    on e: Exception do begin
      error:= 'TRtfdDiagram.ShowAttributes TypeClassifier ' + #13#10+
              SL.Text + '#' +
              SL1.Text + '#' + #13#10;
      if SL1.count >= 5 then
        error:= error + '>' + SL1[3] + '<' +  '>' + SL1[4] + '<' ;
      error:= error + 'i = ' + IntToStr(i) + #13#10;
      FConfiguration.Log(error, e);
    end;
  end;

end;

procedure TRtfdDiagram.GetAllAttributeValues(aClass: TClass; aJavaObject: TComJavaObject;
                                          aAttribut: TComJavaAttribute; Attributes: TStringList);
  var It: IModelIterator;
      aItAttribut: TAttribute;
      aJavaValue: TComJavaValue;
begin
  if Assigned(aClass.Ancestor) then
    GetAllAttributeValues(aClass.Ancestor, aJavaObject, aAttribut, Attributes);
  It:= aClass.GetAttributes;
  while It.HasNext do begin
    aItAttribut:= It.Next as TAttribute;
    if not aItAttribut.isFinal and (FConfiguration.PrivateAttributEditable or (aItAttribut.Visibility <> viPrivate)) then begin
      aJavaValue:= TComJavaValue.create(aItAttribut.TypeClassifier.Name, ComJava);
      aJavaValue.Value:= aAttribut.GetAttributeValue(aJavaObject, aItAttribut.Name, aItAttribut.TypeClassifier.Name, aItAttribut.static);
      Attributes.AddObject(aItAttribut.TypeClassifier.GetShortType + ' ' + aItAttribut.Name, aJavaValue);
    end;
  end;
end;

procedure TRtfdDiagram.SetAttributeValues(aClass: TClass; aJavaObject: TComJavaObject;
                                          aAttribut: TComJavaAttribute; Attributes: TStringList);
  var It: IModelIterator;
      aItAttribut: TAttribute;
      aJavaValue: TComJavaValue;
      k: integer;
begin
  if Assigned(aClass.Ancestor) then
    SetAttributeValues(aClass.Ancestor, aJavaObject, aAttribut, Attributes);
  It:= aClass.GetAttributes;
  while It.HasNext do begin
    aItAttribut:= It.Next as TAttribute;
    if FConfiguration.PrivateAttributEditable or (aItAttribut.Visibility <> viPrivate) then begin
      k:= Attributes.IndexOf(aItAttribut.TypeClassifier.GetShortType + ' ' + aItAttribut.Name);
      if k > -1 then begin
        aJavaValue:= TComJavaValue(Attributes.Objects[k]);
        aAttribut.SetAttributeValue(aJavaObject, aItAttribut.Name, aItAttribut.TypeClassifier.Name, aJavaValue.AsString, AItAttribut.Static);
      end;
    end;
  end;
end;

procedure TRtfdDiagram.CallMethodForObject(Sender: TObject);
begin
  var C:= FindVCLWindow(Frame.PopMenuObject.PopupPoint);
  CallMethod(C, Sender);
end;

procedure TRtfdDiagram.CallMethodForClass(Sender: TObject);
begin
  var C:= FindVCLWindow(Frame.PopMenuClass.PopupPoint);
  CallMethod(C, Sender);
end;

procedure TRtfdDiagram.CallMethod(C: TControl; Sender: TObject);
  var Caption, Title, ParamName, ParamTyp, ParamTypName, s, Sig, LongType: string;
      i, p, InheritedLevel: integer;
      theObjectname: string;
      theMethodname: string;
      theReturntype: string;
      ParamsAsString: string;

      aJavaObject: TComJavaObject;
      aJavaMethod: TComJavaMethod;
      aJavaValue: TComJavaValue;
      aJavaClass: TComJavaClass;
      aViewClass: TRtfdClass;
      aModelClass: TClass;
      theParams: TComJavaParams;
      MethodType: TMethodAttribute;
      Parameter: TStringList;
      Values: string;
      s1, s2, From: string;
begin
  try
    if Assigned(C) and ((C is TRtfdObject) or (C is TRtfdClass)) then begin
      if not CollectClasses then exit;
      LockWindow(UMLForm.Handle);
      aModelClass:= nil;
      ParamsAsString:= '';
      try
        Panel.ClearSelection;
        if C is TRtfdObject
          then theObjectName:= (C as TRtfdObject).Entity.Name
          else theObjectName:= '';
        InheritedLevel:= (Sender as TSpTBXItem).Tag;
        // get parameters for method-call
        Parameter:= TStringList.Create;
        s:= (Sender as TSpTBXItem).Caption;
        p:= FullParameters.IndexOfName(s);
        s:= FullParameters.ValueFromIndex[p];
        if Pos('static', s) = 1
          then begin delete(s, 1, 7); MethodType:= static; end
        else if C is TRtfdClass
          then MethodType:= static
          else MethodType:= nonstatic;
        theReturnType:= getNextPart(s);
        if length(theReturnType) = 1 then theReturnType:= 'java.lang.Object'; // generic

        p:= Pos('(', s);
        theMethodname:= getShortType(copy(s, 1, p-1));
        delete(s, 1, p); s:= trim(s);
        if s <> ')' then begin
          s:= ReplaceStr(s, ')', ',)');
          while s <> ')' do begin
            ParamTypName:= getNextPart(s, ',');
            s:= trim(s);
            p:= LastDelimiter(' ', ParamTypName);
            if p > 0 then begin
              ParamTyp := Copy(ParamTypName, 1, p - 1);
              ParamName:= copy(ParamTypName, p+1, length(ParamTypName));
            end else begin
              ParamTyp:= ParamTypName;
              ParamName:= '';
            end;
            if length(ParamTyp) = 1
              then aJavaValue:= TComJavaValue.create('java.lang.Object', ComJava)
              else aJavaValue:= TComJavaValue.create(ParamTyp, ComJava);
            Parameter.AddObject(GetShortType(ParamTyp) + ' ' + ParamName, aJavaValue);
          end;
        end;
        Caption:= _('Parameter for method call') + ' ' + theMethodName + '(...)';
        Title:= _('Parameter') + #13#10 + _(LNGValue);
        // get parameter-values from user
        if (Parameter.Count = 0) or EditObjectOrParams(Caption, Title, Parameter) then begin
          if theReturnType <> 'void' then begin
            Values:= '(';
            for i:= 0 to Parameter.Count - 1 do begin
              aJavaValue:= TComJavaValue(Parameter.objects[i]);
              Values:= Values + aJavaValue.Value + ', ';
            end;
            Values:= ReplaceStr(Values + ')', ', )', ')');
          end;
          if MakeParams(Parameter, theParams, ParamsAsString) then begin
            // call the method
            if theObjectName = '' then begin // static method of a class
              aJavaObject:= nil;
              aModelClass:= (C as TRtfdClass).Entity as TClass;
              end
            else begin                       // method of an object
              aJavaObject:= ComJava.GetObject(theObjectname);   // java-level
              aJavaClass:= aJavaObject.ClassRef;
              Longtype:= aJavaClass.Name;
              // get model class for object to get the methods
              aViewClass:= GetBox(Longtype) as TRtfdClass;
              if aViewClass = nil then
                aViewClass:= GetBox(getShortType(LongType)) as TRtfdClass;
              if assigned(aViewClass)
                then aModelClass:= (aViewClass.Entity as TClass)  // model class from view class
                else aModelClass:= nil;
              if aModelClass = nil then
                aModelClass:= getModelClass(LongType);            // model class from model
              if (aModelClass = nil) or (aModelClass.Pathname = '') then
                aModelClass:= CreateModelClass(LongType);          // model class from java
            end;
            // if assigned(aModelClass)
            while InheritedLevel > 0 do begin
              aModelClass:= aModelClass.Ancestor;
              dec(InheritedLevel);
              if C is TRtfdObject then
                MethodType:= nonvirtual
            end;
            aJavaClass:= ComJava.GetClass(aModelClass.GetTyp);
            if assigned(aJavaClass) then begin
              aJavaMethod:= TComJavaMethod.Create(aJavaClass, theMethodName, MethodType, theReturnType, theParams, ComJava);
              try
                aJavaValue:= aJavaMethod.Call(aJavaObject);
                if aJavaMethod.IsValid then begin
                  ShowMethodEntered(theMethodName, 'Actor', theObjectname, ParamsAsString);
                  s:= '';
                  for i:= 0 to Parameter.Count - 1 do
                    s:= s + TComJavaValue(Parameter.objects[i]).AsFormattedString + ', ';
                  delete(s, length(s)-1, 2);
                  Sig:= aModelClass.GetTyp + ' ' + theReturnType + ' ' + theParams.Signature;
                  case MethodType of
                    static:     Sig:= 'static ' + Sig;
                    nonvirtual: Sig:= 'nonvirtual ' + Sig;
                  end;
                  From:= theObjectname;
                  if From = '' then From:= aModelClass.Name;
                  AddToInteractive(From + '.' + theMethodname + '(' + s + ');');

                  if FConfiguration.ShowAllNewObjects then
                    ShowAllNewObjectsString(From);
                  UpdateAllObjects;

                  // show the return-value
                  if theReturnType <> 'void' then begin
                    if theObjectname = ''
                      then s1:= aModelClass.Name + '.' + theMethodname + Values
                      else s1:= theObjectname + '.' + theMethodname + Values;
                    s2:= aJavaValue.Value;
                    if (s2 <> 'null') and (aJavaValue.Kind = ntObject) then begin
                      s2:= s2 + '/' + aJavaValue.toString;
                    end;
                    ShowMethodExited(theMethodName, theObjectname, 'Actor', aJavaValue.Value);
                    if FConfiguration.ShowFunctionvalues then
                      FMessages.OutputToTerminal(s1 + ': ' + s2 + #13#10)
                    else
                      with TFMethodCallDialog.create(Self) do begin
                        EMethodCall.Text:= (Sender as TSpTBXItem).Caption;
                        EParameterValues.Text:= s1;
                        EResult.Text:= s2;
                        Prepare;
                        ShowModal;
                        Free;
                      end;
                  end else
                    ShowMethodExited(theMethodName, theObjectname, 'Actor', '');

                  FreeAndNil(aJavaValue);
                end else begin
                  ErrorMsg(aJavaMethod.Error);
                  FMessages.OutputLineTo(0, aJavaMethod.Error);
                end;
              finally
                FreeAndNil(aJavaMethod);
              end;
            end;
            FreeAndNil(theParams);
            end;
          end;
        DeleteParams(Parameter);
      except
        on e: Exception do begin
          s:= 'CallMethod-theObjectname: ';
          if theObjectname = ''
            then s:= s + '<leer> aModelClass: '
            else s:= s + theObjectname + ' aModelClass: ';
          if assigned(aModelClass)
            then s:= s + aModelClass.Name
            else s:= s + '<nil>';
          FConfiguration.Log(s, e);
          ErrorMsg(e.Message);
        end;
      end;
  end;
  finally
    if assigned(UMLForm) then
      UnlockWindow;
  end;
end;

function TRtfdDiagram.CreateModelClass(const Typ: string): TClass;
  var U: TUnitPackage;
      C: TClass;
      Operation: UModel.TOperation;
      Attribute: UModel.TAttribute;
      aJavaClass: TComJavaClass;
      SL, SLParameter, SLParTypesLong, SL1: TStringList;
      i, j, p: integer;
      s, s1, ParTyp, ParName: string;
      CodeCompletion: TCodeCompletion;
      ParNames, ParTypesShort, Superclassname: string;
      TypeClass: TClassifier;
begin
  Result:= nil;
  U:= Model.ModelRoot.FindUnitPackage('Default');
  CodeCompletion:= TCodeCompletion.create; // used for parameter-names
  SLParTypesLong:= TStringList.Create;
  if CodeCompletion.isJavaAPIClass(Typ)
    then SLParameter:= CodeCompletion.getJavaAPIMethodParameters
    else SLParameter:= nil;
  try
    aJavaClass:= ComJava.getClass(Typ);

    if assigned(aJavaClass) and aJavaClass.IsValid then begin
      C:= U.MakeClass(Typ, '');
      if not assigned(C) then exit;

      C.Importname:= Typ;
      U.AddClassWithoutShowing(C);

      // make operations
      SL:= aJavaClass.getMethods(''); // static and not static
      for i:= 0 to SL.Count - 1 do begin
        ParTypesShort:= '';
        SLParTypesLong.Clear;
        s:= SL.Strings[i];
        p:= Pos('throws ', s);
        if p > 0 then s:= trim(copy(s, 1, p-1));
        if s = '' then continue;
        Operation:= C.AddOperationWithoutType('');
        Operation.Static:= false;
        Operation.OperationType:= otFunction;

        s1:= getNextPart(s);
        if IsVisibility(s1) then begin
          Operation.Visibility:= String2Visibility(s1);
          s1:= getNextPart(s);
        end else
          Operation.Visibility:= viPackage;
        while IsModifier(s1) do begin
          if s1 = 'static' then
            Operation.Static:= true;
          s1:= getNextPart(s);
        end;
        Operation.ReturnValue:= FindClassifier(s1);
        if s1 = 'void' then
          Operation.OperationType:= otProcedure;
        p:= Pos('(', s);
        Operation.Name:= GetShortType(copy(s, 1, p-1));
        delete(s, 1, p);
        p:= Pos(')', s);
        s[p]:= ',';
        ParTyp:= GetNextPart(s, ',');
        while ParTyp <> '' do begin
          SLParTypesLong.Add(ParTyp);
          ParTypesShort:= ParTypesShort + GetShortType(ParTyp) + ',';
          ParTyp:= GetNextPart(s, ',');
        end;

        if assigned(SLParameter) then begin
          p:= SLParameter.IndexOfName(Operation.Name + '/' + ParTypesShort);
          if p > -1
            then ParNames:= SLParameter.ValueFromIndex[p]
            else ParNames:= '';
        end else
          ParNames:= '';

        for j:= 0 to SLParTypesLong.Count - 1 do begin
          ParTyp:= SLParTypesLong.Strings[j];
          if ParNames = ''
            then ParName:= '' // 'Par' + IntToStr(j)
            else begin
              p:= Pos(',', ParNames);
              ParName:= copy(ParNames, 1, p-1);
              delete(ParNames, 1, p);
            end;
          Operation.AddParameter(ParName).TypeClassifier:= FindClassifier(ParTyp);   // ToDo ParName = ''
        end;
      end;

      // make attributes
      SL:= aJavaClass.getClassAttributes;
      for i:= 0 to SL.Count - 1 do begin
        SL1:= Split('|', SL.Strings[i]);
        try
          if SL1.Count = 6 then begin
            TypeClass:= FindClassifier(SL1[4]);
            if TypeClass = nil then begin
              TypeClass:= TClassifier.Create(nil);
              TypeClass.Name:= SL1[4];
            end;
            Attribute:= C.AddAttribute('', TypeClass);
            Attribute.Visibility:= String2Visibility(SL1[0]);
            Attribute.Static:= (SL1[1] = '-1');
            Attribute.IsFinal:= (SL1[2] = '-1');
            if Attribute.IsFinal then Attribute.hasFinal:= true;
            Attribute.TypeClassifier.aGeneric:= SL1[3];
            Attribute.Name:= SL1[5];
          end;
        finally
          FreeAndNil(SL1);
        end;
      end;
      // make ancestor
      SuperclassName:= aJavaClass.getSuperclassName;
      if Superclassname <> '' then
        C.Ancestor:= TClass(FindClassifier(SuperclassName));
      Result:= C;
    end;
  finally
    FreeAndNil(CodeCompletion);
    FreeAndNil(SLParameter);
    FreeAndNil(SLParTypesLong);
  end;
end;

function TRtfdDiagram.PPIScale(ASize: integer): integer;
begin
  Result := MulDiv(ASize, UMLForm.CurrentPPI, 96);
end;

function TRtfdDiagram.PPIUnScale(ASize: integer): integer;
begin
  Result := MulDiv(ASize, 96, UMLForm.CurrentPPI);
end;

function TRtfdDiagram.FindClassifier(const CName: string): TClassifier;
var
  PName, ShortName : string;
  CacheI : integer;
  aClass: TClass;
  aInterface: TInterface;
  TheClass: TModelEntityClass;

  function InLookInModel: TClassifier;
  var
    U : TUnitPackage;
  begin
    Result := nil;
    if PName <> '' then begin // search in package
      U := Model.ModelRoot.FindUnitPackage(PName);
      if Assigned(U) then
        Result:= U.FindClassifier(ShortName, TheClass, true);
    end;
    if Result = nil then begin
      U:= Model.ModelRoot.FindUnitPackage('Default');
      Result := U.FindClassifier(ShortName, TheClass, True);
    end;
    if Result = nil then begin
      U:= Model.ModelRoot.FindUnitPackage('Default');
      Result := U.FindClassifier(CName, TheClass, True);
    end;
  end;

  function ExtractPackageName(const CName: string): string;
  begin
    var i := LastDelimiter('.', CName);
    if i = 0 then
      Result := ''
    else
      Result := Copy(CName, 1, I-1);
  end;

  function ExtractClassName(const CName: string): string;
  begin
    var i:= LastDelimiter('.', CName);
    if i = 0 then
      Result := CName
    else
      Result := Copy(CName,I+1,255);
    Result:= withoutArray(Result);
  end;

begin
  TheClass:= nil;
  CacheI:= Model.NameCache.IndexOf(CName);
  if (CacheI > -1) then begin
    Result := TClassifier(Model.NameCache.Objects[CacheI]);
    exit;
  end;
  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);
  Result := InLookInModel;

  if assigned(Result) and (Pos('[]', CName) > 0) and (Result.Name <> CName) then begin
    if (Result is TClass) then begin
      aClass:= TClass.Create(Result.Owner);
      aClass.Pathname:= Result.Pathname;
      aClass.Importname:= Result.Importname;
      aClass.Ancestor:= (Result as TClass).Ancestor;
      aClass.Name:= CName;
      aClass.Visibility:= Result.Visibility;
      aClass.Static:= Result.Static;
      Result:= aClass;
    end else begin
      aInterface:= TInterface.Create(Result.Owner);
      aInterface.Pathname:= Result.Pathname;
      aInterface.Importname:= Result.Importname;
      aInterface.Ancestor:= (Result as TInterface).Ancestor;
      aInterface.Name:= CName;
      aInterface.Visibility:= Result.Visibility;
      aInterface.Static:= Result.Static;
      Result:= aInterface;
    end;
    Model.NameCache.AddObject(CName, Result);
  end;

  if not Assigned(Result) then begin
    Result:= Model.UnknownPackage.FindClassifier(CName, TClass, True);
    if not Assigned(Result) then begin
      Result:= Model.UnknownPackage.MakeClass(CName, '');
      Model.UnknownPackage.AddClass(TClass(Result));
    end;
  end;

 // if Assigned(Result) and (CacheI = -1) then
 //   Model.NameCache.AddObject(CName, Result);
  if Assigned(Result) and (Pos('/', Result.Name) > 0) then
    Result.Name:= ReplaceStr(Result.Name, '/', '.');
end;

procedure TRtfdDiagram.OpenClass(C: TControl);
begin
  if Assigned(C) and (C is TRtfdObject) then begin
    var CName:= (C as TRtfdObject).getClassname;
    (Frame.Parent.Owner as TFUMLForm).AddClassToProject(CName);
  end;
  Panel.ClearSelection;
end;

procedure TRtfdDiagram.EditObject(C: TControl);
  var Caption, Title, ObjectnameOld, aClassType: string;
      i: integer;
      aJavaObject  : TComJavaObject;
      aJavaClass   : TComJavaClass;
      aJavaAttribut: TComJavaAttribute;
      aJavaValue   : TComJavaValue;
      Attributes   : TStringList;
      U: TUnitPackage;
      aModelClass : TClass;
      aObject: TObject;
begin
  if Assigned(C) and (C is TRtfdObject) then begin
    Panel.ClearSelection;
    ObjectnameOld:= (C as TRtfdObject).Entity.Name;
    aJavaObject  := ComJava.GetObject(ObjectnameOld);
    aJavaClass   := aJavaObject.ClassRef;
    aJavaAttribut:= TComJavaAttribute.Create(aJavaClass, ComJava);

    U:= Model.ModelRoot.FindUnitPackage('Default');
    ComJava.Transfer(U.ClassImports, U.FullImports, getSourcePath);
    aClassType:= aJavaClass.Name;
    aModelClass:= U.FindClassifier(aClassType) as TClass;
    if Assigned(aModelClass) then begin
      Attributes:= TStringList.Create;
      GetAllAttributeValues(aModelClass, aJavaObject, aJavaAttribut, Attributes);
      Caption:= _('Edit object') + ' ' + ObjectNameOld;
      Title  := _('Attribute') + #13#10 + _(LNGValue);
      if EditObjectOrParams(Caption, Title, Attributes) then begin
        SetAttributeValues(aModelClass, aJavaObject, aJavaAttribut, Attributes);
        UpdateAllObjects;
      end;
      for i:= Attributes.Count - 1 downto 0 do begin
        aJavaValue:= TComJavaValue(Attributes.Objects[i]);
        if aJavaValue.Kind <> ntObject then begin
          aObject:= Attributes.Objects[i];
          FreeAndNil(aObject);
        end;
      end;
      Attributes.Clear;
    end
    else
      ErrorMsg(Format(_(LNGClassNotFound), [aJavaClass.Signature]));
  end;
end;

procedure TRtfdDiagram.UpdateAllObjects;
  var
    U: TUnitPackage;
    aJavaObject: TComJavaObject;
    aModelClass: TClassifier;
    aModelObject: TObjekt;
    aModelAttribut: TAttribute;
    aModelClassAttribut: TAttribute;
    It, It2, It3: IModelIterator;
    aObjectList, SL_V, SL_N: TStringList;
    i, j: integer;
    value, s: string;

  function Shorten(const s: string): string;
  begin
    Result:= s;
    if Length(s) > 100 then begin
      var p:= 100;
      while (p > 0) and (s[p] <> ',') do
        dec(p);
      if p = 0 then p:= 96;
      Result:= copy(s, 1, p+1) + '...}';
    end;
  end;

begin
  try
    if assigned(Model) and assigned(Model.ModelRoot)
      then U:= Model.ModelRoot.FindUnitPackage('Default')
      else U:= nil;
    if U = nil then exit;
    ComJava.Transfer(U.ClassImports, U.FullImports, getSourcepath);
    aObjectList:= U.GetAllObjects;
    for i:= 0 to aObjectList.Count - 1 do begin
      aJavaObject:= ComJava.getObject(aObjectList.Strings[i]);
      if not assigned(aJavaObject) then
        continue;
      SL_N:= aJavaObject.getAttributeNames;
      SL_V:= aJavaObject.getAttributeValues;  // SL_V kann einen Fehler enthalten
      if Pos('-ERR', SL_V.Text) = 1 then begin
        FConfiguration.Log('TRtfdDiagram.UpdateAllObjects A: ' + SL_V.Text);
        continue;
      end;
      if (SL_N.Count <> SL_V.Count) or (SL_N.Count <= 1) then begin
         SL_N:= aJavaObject.getRefreshedAttributeNames;
         SL_V:= aJavaObject.getAttributeValues;
      end;
      if SL_N.count <> SL_V.count then begin
        FConfiguration.Log('TRtfdDiagram.UpdateAllObjects B: ' + aJavaObject.DebugGetAttributeValues + ' SL_N: ' + IntTostr(SL_N.Count) + ' SL_V: ' + IntToStr(SL_V.Count));
        continue;
      end;
      if assigned(aObjectList) and  assigned(aObjectList.objects[i]) and (aObjectList.Objects[i] is TObjekt)
        then aModelObject:= aObjectList.objects[i] as TObjekt
        else begin
          FConfiguration.Log('TRtfdDiagram.UpdateAllObjects C: ' + aObjectList.Strings[i] + ' | ' + aObjectList.Objects[i].ClassName);
          continue;
        end;
      It:= aModelObject.GetAttributes;
      while It.HasNext do begin
        aModelAttribut:= It.Next as TAttribute;
        j:= SL_N.IndexOf(aModelAttribut.Name);
        if j = -1 then begin
          SL_N:= aJavaObject.getRefreshedAttributeNames;
          SL_V:= aJavaObject.getAttributeValues;
          j:= SL_N.IndexOf(aModelAttribut.Name);
        end;
        if (0 <= j) and (j < SL_V.Count) then
          Value:= Shorten(SL_V[j])
        else if aModelAttribut.Name = '' then
          Value:= ''
        else begin
          It3:= aModelObject.GetAttributes;
          s:= '';
          while It3.HasNext do begin
            aModelAttribut:= It3.Next as TAttribute;
            s:= s + '|' + aModelAttribut.Name;
          end;
          Value:= '<error>';
          FConfiguration.Log('TRtfdDiagram.UpdateAllObjects D: ' + aObjectList.Strings[i] + ' | ' + aObjectList.Objects[i].ClassName);
        end;

        if aModelAttribut.Static then begin
          aModelClass:= aModelObject.getTyp;
          if assigned(aModelClass) then begin
            It2:= aModelClass.GetAttributes;
            while It2.HasNext do begin
              aModelClassAttribut:= It2.Next as TAttribute;
              if aModelClassAttribut.Name = aModelAttribut.Name then begin
                aModelClassAttribut.Value:= Value;
                break;
              end;
            end;
          end;
        end else
          aModelAttribut.Value:= Value;
      end;
      aModelObject.RefreshEntities;
      Interactive.Executer.AddVariable(aJavaObject.Name, aJavaObject.ClassRef.Name, aJavaObject.CreateValue);
    end;
    FreeAndNil(aObjectList);
    RefreshDiagram;
  except on e: exception do
    FConfiguration.Log('UpdateAllObjects: ', e);
  end;
end;

function TRtfdDiagram.insertParameterNames(s: string): string;
  var p: integer; s1, s2: string;
begin
  p:= pos('(', s);
  s1:= copy(s, 1, p);
  delete(s, 1, p);
  s:= ReplaceStr(s, ')', ',)');
  p:= pos(',', s);
  while p > 0 do begin
    s2:= copy(s, 1, p-1);
    delete(s, 1, p);
    s2:= GetShortTypeWith(s2);
    if Pos('<?', s2) > 0 then begin
      s2:= s2;
    end else begin
      p:= Pos(' ', s2);
      if (p = 0) and (s2 <> '') then
        s2:= s2;
    end;
    p:= Pos(',', s);
    if p > 0
      then s1:= s1 + s2 + ', '
      else s1:= s1 + s2 + ')';
  end;
  Result:= s1;
end;


procedure TRtfdDiagram.PopMenuClassPopup(Sender: TObject);
  var s1, s2: string;
      i, p, MenuIndex, InheritedLevel, StartIndex: integer;
      aViewClass: TRtfdClass;
      aViewInterface: TRtfdInterface;
      aModelClass, superClass: TClass;
      aModelInterface, superInterface: TInterface;
      it1, it2: IModelIterator;
      Operation: UModel.TOperation;
      Attribute: UModel.TAttribute;
      aInterface: UModel.TInterface;
      Parameter: TParameter;
      aInheritedMenu: TSpTBXSubmenuItem;
      aMenuItem: TSpTBXItem;
      HasSourcecode: boolean;
      Associations: TStringList;
      Interfaces: TStringList;
      Connections: TStringlist;
      SLSorted: TStringList;
      NoSystemClass: boolean;
      aBox: TControl;
      hasMain: boolean;
      hasInheritedSystemMethods: boolean;
      MethodWithParam, MethodNoParam: string;

  procedure MakeOpenClassMenuItem(const Caption: string; ImageIndex: integer);
  begin
    if BoxNames.IndexOf(Caption) = -1 then begin
      var aMenuItem:= TSpTBXItem.Create(Frame.PopMenuClass);
      aMenuItem.Caption:= WithoutGeneric(Caption);
      aMenuItem.OnClick:= OpenClassOrInterface;
      aMenuItem.ImageIndex:= ImageIndex;
      Frame.MIClassPopupOpenClass.Add(aMenuItem);
    end;
  end;

  procedure MakeConnectClassMenuItems;
    var aMenuItem: TSpTBXItem; c: char;
        i: integer; s: string;
  begin
    for i:= BoxNames.Count - 1 downto 0 do
      if (BoxNames.Objects[i] is TRtfdClass) then
        Connections.Add('C' + (BoxNames.Objects[i] as TRtfdBox).Entity.FullName)
      else if (BoxNames.Objects[i] is TRtfdInterface) then
        Connections.Add('I' + (BoxNames.Objects[i] as TRtfdBox).Entity.FullName)
      else if (BoxNames.Objects[i] is TRtfdCommentBox) then
        Connections.Add('K' + BoxNames.Strings[i]);
    for i:= 0 to Connections.Count - 1 do begin
      s:= Connections.Strings[i];
      c:= s[1];
      s:= copy(s, 2, length(s));
      aMenuItem:= TSpTBXItem.Create(Frame.PopMenuClass);
      aMenuItem.Caption:= WithoutGeneric(s);
      AMenuItem.Caption:= s;
      aMenuItem.OnClick:= ConnectBoxes;
      case c of
        'C': aMenuItem.ImageIndex:= 0;
        'I': aMenuItem.ImageIndex:= 16;
        'K': aMenuItem.ImageIndex:= 23;
      end;
      s:= (aBox as TRtfdBox).Entity.Name;
      aMenuItem.Tag:= BoxNames.IndexOf(s);
      Frame.MIClassPopupConnect.Add(aMenuItem);
    end;
  end;

  procedure AddDatatype(const s: string);
    var s1, s2, path: string; i: integer;
  begin
    if IsSimpleType(s) then exit;
    s1:= WithoutGeneric(WithOutArray(s));
    if IsSimpleType(s1) or (s1 = '') then exit;
    for i:= 0 to BoxNames.Count - 1 do
      if Pos(s1, BoxNames.Strings[i]) = 1 then exit;
    if (Associations.IndexOf(s1) > -1) or
       (Interfaces.IndexOf(s1) > -1) then exit;
    s2:= getShortType(s1);
    if FConfiguration.IsAPIClass(s2) then
      Associations.Add(s2)
    else if FConfiguration.IsAPIInterface(s2) then
      Interfaces.Add(s2)
    else begin
      if TComJavaClass.findClass(s1, ComJava) then
        if TComJavaClass.IsInterface(s1, ComJava)
          then Interfaces.Add(s1)
          else Associations.Add(s1)
      else begin
        path:= FConfiguration.SearchClassInDirectory(ExtractClassName(s1),
                     ExtractFilePath(UMLForm.Pathname), ExtractPackageName(s1));
        if FConfiguration.IsInterface(path)
          then Interfaces.Add(s1)
          else Associations.Add(s1)
      end;
    end;
  end;

  procedure MakeMenuItem(const s1, s2: string; ImageIndex: integer);
  begin
    if s1 = '' then begin
      var aSeparator:= TSpTBXSeparatorItem.Create(Frame.PopMenuClass);
      Frame.PopMenuClass.Items.Insert(MenuIndex, aSeparator);
      Inc(MenuIndex);
    end else begin
      var aMenuItem:= TSpTBXItem.Create(Frame.PopMenuClass);
      aMenuItem.Caption:= s1; // Copy(Caption, 1, 50);
      aMenuItem.Tag:= InheritedLevel;
      if s1 <> '' then begin
        if ImageIndex >= 7
          then aMenuItem.OnClick:= CallMethodForClass
          else aMenuItem.OnClick:= CreateObjectForSelectedClass;
        aMenuItem.ImageIndex:= ImageIndex;
        FullParameters.Add(s1 + '=' + s2);
      end;
      if (InheritedLevel = 0) and (0 <= MenuIndex) and (MenuIndex < Frame.PopMenuClass.Items.Count) then begin
        Frame.PopMenuClass.Items.Insert(MenuIndex, aMenuItem);
        Inc(MenuIndex);
      end else
        aInheritedMenu.Add(aMenuItem);
    end;
  end;

  function MakeTestMenuItem(const s1, s2: string): TSpTBXItem;
    var aMenuItem: TSpTBXItem;
  begin
    aMenuItem:= TSpTBXItem.Create(Frame.PopMenuClass);
    aMenuItem.Caption:= s1; // Copy(Caption, 1, 50);
    aMenuItem.OnClick:= OnRunJunitTestMethod;
    aMenuItem.ImageIndex:= 21;
    FullParameters.Add(s1 + '=' + s2);
    Result:= aMenuItem;
  end;

  procedure MakeSortedMenu(SLSorted: TStringList);
    var i, e, p, img: integer; s, s1, s2: string;
  begin
    for i:= 0 to SLSorted.Count - 1 do begin
      s:= SLSorted.Strings[i];
      p:= Pos('#', s);
      delete(s, 1, p);
      p:= Pos('#', s);
      s1:= copy(s, 1, p - 1);
      delete(s, 1, p);
      p:= Pos('#', s);
      s2:= copy(s, 1, p - 1);
      delete(s, 1, p);
      val(s, img, e);
      MakeMenuItem(s1, s2, img);
    end;
  end;

  procedure MakeSystemInheritedMenus;
    var i, img: integer; s, s1, s2, s3, s4: string;
        SL, SLSorted: TStringList;
        SuperclassName: string;
        aJavaClass: TComJavaClass;
  begin
    // go on with API-classes
    aJavaClass:= ComJava.getClass(aModelClass.ImportName);
    if aJavaClass = nil then exit;
    
    SLSorted:= TStringList.create;
    try
      SLSorted.Sorted:= true;
      repeat
        SL:= aJavaClass.getConstructors;
        for i:= 0 to SL.Count - 1 do begin
          s:= SL.Strings[i];
          s:= withoutThrows(s);
          s1:= getNextPart(s);
          if IsVisibility(s1) then
            s1:= GetNextPart(s);
          while IsModifier(s1) do
            s1:= GetNextPart(s);
          s1:= insertParameterNames(s1);
          s2:= s1;
          s1:= GetShortTypeWith(s1);
          MakeMenuItem(s1, s2, 2);
        end;
        SL:= aJavaClass.getMethods('static');
        for i:= 0 to SL.Count - 1 do begin
          s:= SL.Strings[i];
          s:= withoutThrows(s);
          s1:= getNextPart(s);
          img:= Visibility2ImageNumber(viPackage); // default;
          if s1 <> 'public' then continue;

          if IsVisibility(s1) then begin
            img:= Visibility2ImageNumber(String2Visibility(s1));
            s1:= GetNextPart(s);
          end;
          while IsModifier(s1) do
            s1:= GetNextPart(s);
          s2:= s1 + ' ' + s;
          s4:= getNextPart(s); // <T> T[] getListeners
          if s = ''
            then s:= s4
            else s1:= s1 + ' ' + s4;
          s3:= getShortMethod(s);
          s1:= GetShortType(s1) + ' ' + s3;
          SLSorted.add(s3 + '#' + s1 + '#' + s2 + '#' + IntToStr(img));
        end;
        MakeSortedMenu(SLSorted);
        SLSorted.Text:= '';

        // empty inherited static methods menu
        if assigned(aInheritedMenu) then
          if (InheritedLevel > 0) and (aInheritedMenu.Count = 0) then begin
            FreeAndNil(aInheritedMenu);
            dec(MenuIndex);
          end else begin
            aInheritedMenu.Visible:= aViewClass.ShowInherited;
            hasInheritedSystemMethods:= true;
          end;

        SuperclassName:= aJavaClass.getSuperclassName;
        if SuperclassName = '' then
          break
        else begin
          aJavaClass:= ComJava.getClass(SuperclassName);
          if assigned(aJavaClass) then begin
            aInheritedMenu:= TSpTBXSubmenuItem.Create(Frame.PopMenuObject);
            aInheritedMenu.Caption:= 'Inherited from ' + aJavaClass.Name;
            Frame.PopMenuClass.Items.Insert(MenuIndex, aInheritedMenu);
            inc(MenuIndex);
          end;
        end;
      until false;
    finally
      FreeAndNil(SLSorted);
    end;
  end;

begin // PopMenuClassPopup
  aBox:= FindVCLWindow((Sender as TSpTBXPopupMenu).PopupPoint);
  Panel.ClearSelection;
  if assigned(aBox)
    then TManagedObject(Panel.FindManagedControl(aBox)).Selected:= true
    else exit;

  // delete previous menu
  for i:= Frame.PopMenuClass.Items.Count - 1 downto 0 do
    if Frame.PopMenuClass.Items[i].Tag = 0 then
      FreeAndNil(Frame.PopMenuClass.Items[i]);
  for i:= Frame.MIClassPopupRunOneTest.Count - 1 downto 0 do
    FreeAndNil(Frame.MIClassPopupRunOneTest.Items[i]);
  for i:= Frame.MIClassPopupOpenClass.Count - 1 downto 0 do
    FreeAndNil(Frame.MIClassPopupOpenClass.Items[i]);
  for i:= Frame.MIClassPopupConnect.Count - 1 downto 0 do
    FreeAndNil(Frame.MIClassPopupConnect.Items[i]);

  FullParameters.Clear;
  InheritedLevel:= 0;
  StartIndex:= 12;
  MenuIndex:= StartIndex;
  MakeMenuItem('', '', 0);
  Associations:= TStringList.Create;
  Associations.Sorted:= true;
  Associations.Duplicates:= dupIgnore;
  Interfaces:= TStringList.Create;
  Interfaces.Sorted:= true;
  Interfaces.Duplicates:= dupIgnore;
  Connections:= TStringList.Create;
  Connections.Sorted:= true;
  Connections.Duplicates:= dupIgnore;
  SLSorted:= TStringList.create;
  SLSorted.Sorted:= true;
  hasMain:= false;
  hasInheritedSystemMethods:= false;
  aViewClass:= nil;
  aViewInterface:= nil;

  if (aBox is TRtfdClass) and not (aBox as TRtfdBox).isJUnitTestclass then begin
    aViewClass:= aBox as TRtfdClass;
    aModelClass:= aViewClass.Entity as TClass;

    // get superclass
    superClass:= aModelClass.Ancestor;
    if Assigned(superClass) then
      MakeOpenClassMenuItem(superClass.FullName, 13);

    // get constructors
    if not aModelClass.IsAbstract and not myJavaCommands.ProcessRunning then begin
      it1:= aModelClass.GetOperations;
      while it1.HasNext do begin
        Operation:= it1.Next as UModel.TOperation;
        if Operation.OperationType = otConstructor then begin
          s1:= Operation.Name + '(';
          s2:= s1;
          it2:= Operation.GetParameters;
          while it2.HasNext do begin
            Parameter:= it2.next as TParameter;
            if assigned(Parameter.TypeClassifier) then begin
              s1:= s1 + Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType + ', ';
              s2:= s2 + Parameter.TypeClassifier.Name + ' ' + Parameter.Name +  ', ';
              if InheritedLevel = 0 then
                AddDatatype(Parameter.TypeClassifier.Name);
            end;
          end;
          s1:= ReplaceStr(s1 + ')', ', )', ')');
          s2:= ReplaceStr(s2 + ')', ', )', ')');
          MakeMenuItem(s1, s2, 2);
         end
        else if (Operation.OperationType = otProcedure) and (Operation.name = 'main') then
          hasMain:= true;
      end;
      if MenuIndex = StartIndex + 1 then // default constructor
        MakeMenuItem(aModelClass.GetShortType + '()', aModelClass.GetShortType + '()', 2);
    end;
    // get static methods and Parameterclasses
    repeat
      it1:= aModelClass.GetOperations;
      while it1.HasNext and not myJavaCommands.ProcessRunning do begin
        Operation:= it1.Next as UModel.TOperation;
        if Operation.OperationType in [otFunction, otProcedure] then begin
          s1:= Operation.Name + '(';
          s2:= s1;
          it2:= Operation.GetParameters;
          while it2.HasNext do begin
            Parameter:= it2.next as TParameter;
            if assigned(Parameter.TypeClassifier) then begin
              s1:= s1 + Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType + ', ';
              s2:= s2 + Parameter.TypeClassifier.Name + ' ' + Parameter.Name +  ', ';
              if InheritedLevel = 0 then
                AddDatatype(Parameter.TypeClassifier.Name);
            end;
          end;
          if not Operation.Static then continue;
          s1:= ReplaceStr(s1 + ')', ', )', ')');
          s2:= ReplaceStr(s2 + ')', ', )', ')');
          case Operation.OperationType of
            otFunction : begin
              s1:= s1 + ': ' + Operation.ReturnValue.GetShortType;
              s2:= Operation.ReturnValue.Name + ' ' + s2;
              if InheritedLevel = 0 then
                AddDatatype(Operation.ReturnValue.Name);
              end;
            otProcedure: begin
              {s1:= 'void ' + s1;}
              s2:= 'void ' + s2;
            end;
          end;
          if Operation.Static then begin
            {s1:= 'static ' + s1;}
            s2:= 'static ' + s2;
            MakeMenuItem(s1, s2, Integer(Operation.Visibility) + 7);
          end;
        end;
      end;

      // empty inherited static methods menu
      if (InheritedLevel > 0) and (aInheritedMenu.Count = 0) then begin
        FreeAndNil(aInheritedMenu);
        dec(MenuIndex);
      end;

      // get Association classes
      if InheritedLevel = 0 then begin
        it1:= aModelClass.GetAttributes;
        while it1.HasNext do begin
          Attribute:= it1.Next as UModel.TAttribute;
          if assigned(Attribute.TypeClassifier) then
            AddDatatype(Attribute.TypeClassifier.Importname)
        end;

        it1:= aModelClass.GetImplements;
        while it1.HasNext do begin
          aInterface:= it1.Next as UModel.TInterface;
          AddDatatype(aInterface.FullName);
        end;
      end;

      // get inherited static methods
      aModelClass:= aModelClass.Ancestor;
      if assigned(aModelClass) and not myJavaCommands.ProcessRunning then begin
        //if InheritedLevel = 0 then MakeMenuItem('-', '-', 0);
        inc(InheritedLevel);
        aInheritedMenu:= TSpTBXSubmenuItem.Create(Frame.PopMenuClass);
        aInheritedMenu.Caption:= 'Inherited from ' + aModelClass.Name;
        Frame.PopMenuClass.Items.Insert(MenuIndex, aInheritedMenu);
        inc(MenuIndex);
        if FConfiguration.IsAPIClassOrInterface(aModelClass.Importname) then begin
          MakeSystemInheritedMenus;
          break;
        end;
      end
      else aModelClass:= nil;
    until aModelClass = nil;
  end;

  {--- JUnit-Tests ------------------------------------------------------------}
  if FConfiguration.JUnitOK and (aBox as TRtfdBox).isJUnitTestclass and not myJavaCommands.ProcessRunning then begin
    aViewClass:= aBox as TRtfdClass;
    aModelClass:= aViewClass.Entity as TClass;
    // get test methods
    it1:= aModelClass.GetOperations;
    MenuIndex:= 1;
    while it1.HasNext do begin
      Operation:= it1.Next as UModel.TOperation;
      if (Operation.Annotation <> 'Test') and (Operation.Annotation <> 'ParameterizedTest') then continue;
      if Operation.OperationType in [otFunction, otProcedure] then begin
        s1:= Operation.Name + '(';
        it2:= Operation.GetParameters;
        while it2.HasNext do begin
          Parameter:= it2.next as TParameter;
          if assigned(Parameter.TypeClassifier) then begin
            s1:= s1 + Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType + ', ';
            if InheritedLevel = 0 then
              AddDatatype(Parameter.TypeClassifier.Name);
          end;
        end;
        s1:= ReplaceStr(s1 + ')', ', )', ')');
        SLSorted.Add(s1 + '#' + Operation.Name);
      end;
    end;

    for i:= 0 to SLSorted.Count - 1 do begin
      p:= Pos('#', SLSorted.Strings[i]);
      MethodWithParam:= copy(SLSorted.Strings[i], 1, p-1);
      MethodNoParam:= copy(SLSorted.Strings[i], p + 1, 255);
      aMenuItem:= MakeTestMenuItem(MethodWithParam, MethodNoParam);
      if SLSorted.Count > 3
        then Frame.MIClassPopupRunOneTest.Add(aMenuItem)
      else begin
        Frame.PopMenuClass.Items.Insert(MenuIndex, aMenuItem);
        Inc(MenuIndex);
      end;
    end;
  end;

  // --- Interface -------------------------------------------------
  if aBox is TRtfdInterface then begin
    aViewInterface:= aBox as TRtfdInterface;
    aModelInterface:= aViewInterface.Entity as TInterface;

    // get superinterface
    superInterface:= aModelInterface.Ancestor;
    if Assigned(superInterface) then
      MakeOpenClassMenuItem(superInterface.Name, 13);

    // get Parameterclasses
    it1:= aModelInterface.GetOperations;
    while it1.HasNext do begin
      Operation:= it1.Next as UModel.TOperation;
      if Operation.OperationType in [otFunction, otProcedure] then begin
        it2:= Operation.GetParameters;
        while it2.HasNext do begin
          Parameter:= it2.next as TParameter;
          if assigned(Parameter.TypeClassifier) and (InheritedLevel = 0) then
            AddDatatype(Parameter.TypeClassifier.Name);
        end;
        if (Operation.OperationType = otFunction) and (InheritedLevel = 0)
          then AddDatatype(Operation.ReturnValue.Name);
      end;
    end;

    it1:= aModelInterface.GetImplementingClasses;
    while it1.HasNext do begin
      aModelClass:= it1.Next as UModel.TClass;
      AddDatatype(aModelClass.ImportName)
    end;

    // get association interfaces
    it1:= aModelInterface.GetAttributes;
    while it1.HasNext do begin
      Attribute:= it1.Next as UModel.TAttribute;
      AddDatatype(Attribute.TypeClassifier.Name)
    end;
  end;

  for i:= 0 to Associations.Count - 1 do
    MakeOpenClassMenuItem(Associations.Strings[i], 0);
  for i:= 0 to Interfaces.Count - 1 do
    MakeOpenClassMenuItem(Interfaces.Strings[i], 16);
  MakeConnectClassMenuItems;

  InheritedLevel:= 0;
  inc(MenuIndex, 2);
  MakeMenuItem('', '', 0);

  s1:= ChangeFileExt((aBox as TRtfdBox).GetPathname, '.java');
  NoSystemClass:= (Pos(FConfiguration.JavaCache, s1) = 0);
  HasSourcecode:= FileExists(s1);

  Frame.MIClassPopupCompileJava.Enabled:= HasSourceCode and NoSystemClass and FConfiguration.JavaCompilerOK and not myJavaCommands.ProcessRunning;
  Frame.MIClassPopupRun.Enabled:= hasMain and not myJavaCommands.ProcessRunning;

  Frame.MIClassPopupClassEdit.Enabled    := (aBox is TRtfdClass) and HasSourceCode and NoSystemClass and not myJavaCommands.ProcessRunning;
  Frame.MIClassPopupInterfaceEdit.Visible:= (aBox is TRtfdInterface) and HasSourceCode and NoSystemClass and not myJavaCommands.ProcessRunning;
  Frame.MIClassPopupOpenSource.Visible:= HasSourceCode;  // in Arbeit
  Frame.MIClassPopupOpenclass.Visible:= (Frame.MIClassPopupOpenclass.Count > 0);
  Frame.MIClassPopupConnect.Visible:= (Frame.MIClassPopupConnect.Count > 0);
  if aBox is TRtfdClass
    then Frame.MIClassPopupDelete.Caption:= _('Delete class')
    else Frame.MIClassPopupDelete.Caption:= _('Delete interface');

  Frame.MIClassPopupShowInherited.Visible:= not (aBox as TRtfdBox).ShowInherited and hasInheritedSystemMethods;
  Frame.MIClassPopupHideInherited.Visible:= (aBox as TRtfdBox).ShowInherited and hasInheritedSystemMethods;
  if FConfiguration.JUnitOK and not myJavaCommands.ProcessRunning then begin
    Frame.MIClassPopupRunAllTests.Visible:= (aBox as TRtfdBox).isJUnitTestclass;
    Frame.MIClassPopupRunOneTest.Visible:= (SLSorted.Count > 3);
//    Frame.NEndOfJUnitTest.Visible:= true;
    Frame.MIClassPopupCreateTestClass.Visible:= not (aBox as TRtfdBox).isJUnitTestclass;
  end else begin
    Frame.MIClassPopupRunAllTests.Visible:= false;
    Frame.MIClassPopupRunOneTest.Visible:= false;
    Frame.MIClassPopupCreateTestClass.Visible:= false;
  end;

  for i:= 0 to Frame.MIClassPopupDisplay.Count - 1 do
    Frame.MIClassPopupDisplay.Items[i].Checked:= false;
  if assigned(aViewClass) or assigned(aViewInterface) then begin
    if assigned(aViewClass)
      then i:= 4 - Ord(aViewClass.MinVisibility)
      else i:= 4 - Ord(aViewInterface.MinVisibility);
    Frame.MIClassPopupDisplay.Items[i].Checked:= true;
  end;

  for i:= 0 to Frame.MIClassPopupParameter.Count - 1 do
    Frame.MIClassPopupParameter.Items[i].Checked:= false;
  if assigned(aViewClass) or assigned(aViewInterface) then begin
    if assigned(aViewClass)
      then i:= aViewClass.ShowParameter
      else i:= aViewInterface.ShowParameter;
    Frame.MIClassPopUpParameter.Items[i].Checked:= true;
  end;

  for i:= 0 to Frame.MIClassPopupVisibility.Count - 1 do
    Frame.MIClassPopupVisibility.Items[i].Checked:= false;
  if assigned(aViewClass) or assigned(aViewInterface) then begin
    if assigned(aViewClass)
      then i:= 2 - aViewClass.ShowIcons
      else i:= 2 - aViewInterface.ShowIcons;
    Frame.MIClassPopupVisibility.Items[i].Checked:= true;
  end;

  // Frame.MIClassPopupVisibility.Visible:= true;
  // Frame.MiPopupDebugJE2Java.Visible:= true;

  FreeAndNil(Associations);
  FreeAndNil(Interfaces);
  FreeAndNil(Connections);
  FreeAndNil(SLSorted);
end;  // PopMenuClassPopup

procedure TRtfdDiagram.PopMenuObjectPopup(Sender: TOBject);
  var s1, s2, s3, LongType, ancest, objectname: string; C: TControl;
      i, InheritedLevel, MenuIndex: integer;
      aObjectBox: TRtfdObject;
      aViewClass: TRtfdClass;
      aModelClass: TClass;
      aModelClassRoot: TClass;
      aJavaClass: TComJavaClass;
      aJavaObject: TComJavaObject;
      it1, it2: IModelIterator;
      Operation: UModel.TOperation;
      Parameter: TParameter;
      aInheritedMenu: TSpTBXItem;
      hasInheritedSystemMethods: boolean;
      SLSorted: TStringList;
      aMenuItem: TSpTBXItem;

  procedure MakeMenuItem(const s1, s2: string; ImageIndex: integer);
  begin
    if s1 = '' then begin
      var aSeparator:= TSpTBXSeparatorItem.Create(Frame.PopMenuObject);
      Frame.PopMenuObject.Items.Insert(MenuIndex, aSeparator);
      Inc(MenuIndex);
    end else begin
      var aMenuItem:= TSpTBXItem.Create(Frame.PopMenuObject);
      aMenuItem.Caption:= s1;
      aMenuItem.OnClick:= CallMethodForObject;
      aMenuItem.ImageIndex:= ImageIndex;
      FullParameters.Add(s1 + '=' + s2);
      if InheritedLevel = 0 then begin
        Frame.PopMenuObject.Items.Insert(MenuIndex, aMenuItem);
        Inc(MenuIndex);
      end else begin
        aMenuItem.Tag:= InheritedLevel;
        aInheritedMenu.Add(aMenuItem);
      end;
    end;
  end;

  procedure MakeSortedMenu(SLSorted: TStringList);
    var i, e, p, img: integer; s, s1, s2: string;
  begin
    for i:= 0 to SLSorted.Count - 1 do begin
      s:= SLSorted.Strings[i];
      p:= Pos('#', s);
      delete(s, 1, p);
      p:= Pos('#', s);
      s1:= copy(s, 1, p - 1);
      delete(s, 1, p);
      p:= Pos('#', s);
      s2:= copy(s, 1, p - 1);
      delete(s, 1, p);
      val(s, img, e);
      MakeMenuItem(s1, s2, img);
    end;
  end;

  procedure MakeSystemInheritedMenus(aClassname: string; withInherited: boolean);
    var i, img: integer; SL, SLSorted: TStringList; s, s1, s2, s3, s4, SuperclassName: string;
  begin
    // go on with API-classes
    aJavaClass:= ComJava.getClass(aClassname);
    SLSorted:= TStringList.create;
    SLSorted.Sorted:= true;
    if assigned(aJavaClass) and aJavaClass.IsValid then
    repeat
      SL:= aJavaClass.getMethods('not static');
      for i:= 0 to SL.Count - 1 do begin
        s:= SL.Strings[i];
        // if Pos('addAll', s) > 0 then s:= s;

        s:= withoutThrows(s);
        s1:= getNextPart(s);
        img:= Visibility2ImageNumber(viPackage); // default;
        if IsVisibility(s1) then begin
          if s1 <> 'public' then
            continue;
          img:= Visibility2ImageNumber(String2Visibility(s1));
          s1:= GetNextPart(s);
        end;
        while IsModifier(s1) do
          s1:= GetNextPart(s);
        s2:= s1 + ' ' + s;
        s4:= getNextPart(s); // <T> T[] getListeners
        if s = ''
          then s:= s4
          else s1:= s1 + ' ' + s4;
        s3:= getShortMethod(s);
        if s1 = 'void'
          then s1:= s3
          else s1:= s3 + ': ' + GetShortType(s1); // + ' ' + s3;
        SLSorted.add(s3 + '#' + s1 + '#' + s2 + '#' + IntToStr(img));
      end;
      MakeSortedMenu(SLSorted);
      SLSorted.Text:= '';

      // empty inherited static methods menu
      if assigned(aInheritedMenu) then
        if (InheritedLevel > 0) and (aInheritedMenu.Count = 0) then begin
          FreeAndNil(aInheritedMenu);
          dec(MenuIndex);
        end else begin
          aInheritedMenu.Visible:= aObjectBox.ShowInherited;
          hasInheritedSystemMethods:= true;
        end;
      Superclassname:= aJavaClass.getSuperclassName;
      if withInherited and (Superclassname <> '') then begin
        aJavaClass:= ComJava.getClass(SuperclassName);
        if assigned(aJavaClass) then begin
          aInheritedMenu:= TSpTBXSubmenuItem.Create(Frame.PopMenuObject);
          aInheritedMenu.Caption:= 'Inherited from ' + aJavaClass.ImportTyp;
          Frame.PopMenuObject.Items.Insert(MenuIndex, aInheritedMenu);
          inc(MenuIndex);
        end;
      end else
        break;
    until false;
    FreeAndNil(SLSorted);
  end;

  procedure MakeShowUnnamedMenu;
    var i: integer;
        SL1, SL2: TStringList;
        aMenuItem: TSpTBXItem;

    function NotShown(const s, t: string): boolean;
    begin
      if BoxNames.IndexOf(s) >= 0 then exit(false);
      Result:= (s <> '') and (s <> 'null') and
               (BoxNames.IndexOf(s) = -1) and (SL2.IndexOf(s) = -1) and
               (pos('.awt.', t) = 0) and (pos('.swing.', t) = 0) and
               (pos('.lang.', t) = 0) and (pos('NumberField', t) = 0);
    end;

    procedure PrepareMenu(s: string);
      var attr, typ: string; p, k: integer; SL: TStringList;
    begin
      if Pos('{', s) + Pos('[', s) = 1 then
        s:= copy(s, 2, Length(s)-2);
      SL:= split(',', s);
      for k:= 0 to SL.Count - 1 do begin
        s:= trim(SL[k]);
        p:= Pos('=', s);
        if p > 0 then begin
          typ:= copy(s, p+1, length(s));
          attr:= Copy(s, 1, p-1);
        end else begin
          typ:= '';
          attr:= s;
        end;
        if NotShown(attr, typ) then begin
          SL2.Add(attr);
          Fullparameters.Add(s);
        end;
      end;
      FreeAndNil(SL);
    end;

    function MakeMenuItem(const s: string; Count: integer): TSpTBXItem;
    begin
      var aMenuItem:= TSpTBXItem.Create(Frame.MIObjectPopupShowNewObject);
      if Count > 3
        then aMenuItem.Caption:= s
        else aMenuItem.Caption:= Frame.MIObjectPopupShowNewObject.Caption + ' ' + s;
      aMenuItem.OnClick:= ShowUnnamedObject;
      aMenuItem.ImageIndex:= 19;
      aMenuItem.Tag:= -2;  // only unnamed menuitems
      Result:= aMenuItem;
    end;

  begin
    Frame.MIObjectPopupShowNewObject.Visible:= false;
    Frame.MIObjectPopupShowAllNewObjects.Visible:= false;
    SL1:= aJavaObject.getObjectAttributeValues(true);
    SL2:= TStringList.Create;
    for i:= 0 to SL1.Count - 1 do
      PrepareMenu(SL1[i]);
    for i:= 0 to SL2.Count - 1 do begin
      aMenuItem:= MakeMenuItem(SL2[i], SL2.Count);
      if SL2.Count > 3 then
        Frame.MIObjectPopupShowNewObject.Add(aMenuItem)
      else begin
        Frame.PopMenuObject.Items.Insert(MenuIndex-1, aMenuItem);
        Inc(MenuIndex);
      end;
    end;
    Frame.MIObjectPopupShowNewObject.Visible:= (SL2.Count > 3);
    Frame.MIObjectPopupShowAllNewObjects.Visible:= (SL2.Count > 1);
    FreeAndNil(SL2);
    FreeAndNil(SL1);
  end;

begin // PopMenuObjectPopup
  C:= FindVCLWindow((Sender as TSpTBXPopupMenu).PopupPoint);
  Panel.ClearSelection;
  if assigned(C)
    then TManagedObject(Panel.FindManagedControl(C)).Selected:= true
    else exit;

  // delete previous menu
  for i:= Frame.PopMenuObject.Items.Count - 1 downto 0 do
    if Frame.PopMenuObject.Items[i].Tag <= 0 then begin
      aMenuItem:= TSpTBXItem(Frame.PopMenuObject.Items[i]);
      FreeAndNil(aMenuItem);
    end;
  for i:= Frame.MIObjectPopupShowNewObject.Count - 1 downto 0 do begin
    aMenuItem:= TSpTBXItem(Frame.MIObjectPopupShowNewObject.Items[i]);
    FreeAndNil(aMenuItem);
  end;

  FullParameters.Clear;

  if Assigned(C) and (C is TRtfdObject) then begin
    Objectname:= (C as TRtfdObject).Entity.FullName;
    aJavaObject:= ComJava.getObject(Objectname);
    if aJavaObject = nil then
      exit;
    aJavaClass:= aJavaObject.ClassRef;
    Longtype:= aJavaClass.Name;
    InheritedLevel:= 0;
    MenuIndex:= 4;   // edit, show, hide, show final, hide final, open class
    MakeShowUnnamedMenu;
    MakeMenuItem('', '', 0);

    // get model class for object to get the methods
    aObjectBox:= GetBox(Objectname) as TRtfdObject;
    aViewClass:= GetBox(Longtype) as TRtfdClass;
    if aViewClass = nil then
      aViewClass:= GetBox(getShortType(LongType)) as TRtfdClass;
    if assigned(aViewClass)
      then aModelClass:= (aViewClass.Entity as TClass)  // model class from view class
      else aModelClass:= nil;
    if aModelClass = nil then
      aModelClass:= getModelClass(LongType);             // model class from model
    if (aModelClass = nil) or (aModelClass.Pathname = '') then
      aModelClass:= CreateModelClass(LongType);          // model class from java
    aModelClassRoot:= aModelClass;

    if assigned(aModelClass) then begin // known Class in Model
      SLSorted:= TStringList.Create;
      SLSorted.Sorted:= true;
      repeat
        it1:= aModelClass.GetOperations;
        while it1.HasNext do begin
          Operation:= it1.Next as UModel.TOperation;
          if (InheritedLevel > 0) and (Operation.Visibility = viPrivate) then continue;
          if (Operation.OperationType in [otProcedure, otFunction]) and not Operation.IsAbstract then begin
            s1:= Operation.Name + '(';
            s2:= s1;
            it2:= Operation.GetParameters;
            while it2.HasNext do begin
              Parameter:= it2.next as TParameter;
              try
                if assigned(Parameter.TypeClassifier) then begin
                  s1:= s1 +  Parameter.TypeClassifier.GetShortType;
                  if Parameter.Name <> '' then s1:= s1 + ' ' + Parameter.Name;
                  s1:= s1 + ', ';
                  s2:= s2 + Parameter.TypeClassifier.Name;
                  if Parameter.Name <> '' then s2:= s2 + ' ' + Parameter.Name;
                  s2:= s2 + ', ';
                end;
              except on e: Exception do begin
                if assigned(Parameter.TypeClassifier)
                  then s3:= ' Parameter.TypeClassifier assigned '
                  else s3:= ' Parameter.TypeClassifier nil ';
                s3:= s3 + 'Parameter.Name = ' + Parameter.Name;
                FConfiguration.Log(s3, e);
              end;
             end;
            end;
            s1:= ReplaceStr(s1 + ')', ', )', ')');
            s2:= ReplaceStr(s2 + ')', ', )', ')');
            if Operation.OperationType = otFunction then begin
              if assigned(Operation.ReturnValue) then
                s1:= s1 + ': ' + Operation.ReturnValue.GetShortType //  + ' ' + s1;
              else begin
                s1:= s1 + ': <unknown>'; //  + ' ' +
                FConfiguration.Log('TRtfdDiagram.PopMenuObjectPopup: Operation.ReturnValue = nil');
              end;
              s2:= Operation.ReturnValue.Name + ' ' + s2;
            end else begin
              s1:= {'void ' + }s1;
              s2:= 'void ' + s2;
            end;
            if Operation.Static then begin
              s1:= {'static ' + }s1;
              s2:= 'static ' + s2;
            end;
            // Operation.Name is sort-criteria, s1 is Menu.caption, s2 is
            SLSorted.add(Operation.Name + '#' + s1 + '#' + s2 + '#' + IntToStr(Integer(Operation.Visibility) + 7));
          end;
        end;
        // end of while - all methods handelt

        MakeSortedMenu(SLSorted);
        SLSorted.Text:= '';

        // empty inherited methods menu
        if (InheritedLevel > 0) and (aInheritedMenu.Count = 0) then begin
          FreeAndNil(aInheritedMenu);
          dec(MenuIndex);
        end;

        if assigned(aModelClass.Ancestor) then begin
          ancest:= aModelClass.Ancestor.Name;
          aModelClass:= getModelClass(ancest);
          if aModelClass = nil then
            aModelClass:= CreateModelClass(ancest);
        end else
          aModelClass:= nil;

        if assigned(aModelClass) then begin
          inc(InheritedLevel);
          aInheritedMenu:= TSpTBXSubmenuItem.Create(Frame.PopMenuObject);
          aInheritedMenu.Caption:= 'Inherited from ' + aModelClass.Name;
          Frame.PopMenuObject.Items.Insert(MenuIndex, aInheritedMenu);
          inc(MenuIndex);
          if FConfiguration.IsAPIClassOrInterface(aModelClass.Importname) then begin
            MakeSystemInheritedMenus(aModelClass.ImportName, true);
            break;
          end;
        end else
          break;
      until false;
      FreeAndNil(SLSorted);
    end;

    InheritedLevel:= 0;
    Inc(MenuIndex,1);
    MakeMenuItem('', '', 0);
    if assigned(aModelClassRoot)
      then Frame.MIObjectPopupEdit.Visible:= HasAttributes(aModelClassRoot)
      else Frame.MIObjectPopupEdit.Visible:= false;
    Frame.MIObjectPopupOpenClass.Visible:= (aViewClass = nil);
    if assigned(aObjectBox) then begin
      Frame.MIObjectPopupShowInherited.Visible:= not aObjectBox.ShowInherited and hasInheritedSystemMethods;
      Frame.MIObjectPopupHideInherited.Visible:= aObjectBox.ShowInherited and hasInheritedSystemMethods;
    end;
    for i:= 0 to Frame.MIObjectPopupDisplay.Count - 1 do
      Frame.MIObjectPopupDisplay.Items[i].Checked:= false;
    i:= 4 - Ord(aObjectBox.MinVisibility);
    Frame.MIObjectPopupDisplay.Items[i].Checked:= true;

    for i:= 0 to Frame.MIObjectPopupVisibility.Count - 1 do
      Frame.MIObjectPopupVisibility.Items[i].Checked:= false;
    i:= 2 - aObjectBox.ShowIcons;
    Frame.MIObjectPopupVisibility.Items[i].Checked:= true;
  end;
end; // PopMenuObjectPopup

procedure TRtfdDiagram.PopMenuConnectionPopup(Sender: TObject);
  var Conn: TConnection;
      BothClassOrInterface, AClassAInterface, AClassAObject: boolean;
begin
  Conn:= Panel.GetClickedConnection;
  if Conn = nil then exit;

  BothClassOrInterface:=
    ((Conn.FFrom is TRtfdClass) and (Conn.FTo is TRtfdClass)) or
    ((Conn.FFrom is TRtfdInterface) and (Conn.FTo is TRtfdInterface));
  AClassAInterface:=
    ((Conn.FFrom is TRtfdClass) and (Conn.FTo is TRtfdInterface)) or
    ((Conn.FFrom is TRtfdInterface) and (Conn.FTo is TRtfdClass));
  AClassAObject:=
    ((Conn.FFrom is TRtfdClass) and (Conn.FTo is TRtfdObject)) or
    ((Conn.FFrom is TRtfdObject) and (Conn.FTo is TRtfdClass));
  Frame.MIConnectionAssoziation.Visible:= BothClassOrInterface;
  Frame.MIConnectionAssoziationArrow.Visible:= BothClassOrInterface;
  Frame.MIConnectionAssoziationBidirectional.Visible:= BothClassOrInterface;
  Frame.MIConnectionAggregation.Visible:= BothClassOrInterface;
  Frame.MIConnectionAggregationArrow.Visible:= BothClassOrInterface;
  Frame.MIConnectionComposition.Visible:= BothClassOrInterface;
  Frame.MIConnectionCompositionArrow.Visible:= BothClassOrInterface;
  Frame.MIConnectionInheritance.Visible:= not Conn.isRecursiv and BothClassOrInterface;
  Frame.MIConnectionImplements.Visible:= AClassAInterface;
  Frame.MIConnectionInstanceOf.Visible:= AClassAObject;
  Frame.MIConnectionRecursiv.Visible:= Conn.isRecursiv;
end;

function TRtfdDiagram.ClassHasObjects(aBox: TRtfdBox): boolean;
  var i: integer;
begin
  Result:= true;
  for i:= 0 to BoxNames.Count - 1 do
    if BoxNames.Objects[i] is TRtfdObject then
      if GetShortType((BoxNames.Objects[i] as TRtfdObject).getClassname) = aBox.Entity.Name then
        exit;
  Result:= false;
end;

function TRtfdDiagram.HasAttributes(aModelClass: TClass): boolean;
begin
  var i:= 0;
  while (i = 0) and (AModelClass <> nil) do begin
    var It:= aModelClass.GetAttributes;
    while It.HasNext do begin
      var aItAttribut:= It.Next as TAttribute;
      if FConfiguration.PrivateAttributEditable or (aItAttribut.Visibility <> viPrivate)
        then inc(i);
    end;
    AModelClass:= AModelClass.Ancestor;
  end;
  Result:= (i > 0);
end;

function TRtfdDiagram.hasEditableClass: boolean;
begin
  Result:= false;
  var C:= Panel.GetFirstSelected;
  if not assigned(C) then
    C:= Panel.GetFirstManaged;

  if Assigned(C) and (C is TRtfdClass) and
     Assigned(GetBox((C as TRtfdClass).Entity.FullName))
  then begin
    var s:= ChangeFileExt((C as TRtfdBox).GetPathname, '.java');
    Result:= (Length(s) > 0) and not FConfiguration.PathForSystemClass(s) and
             FileExists(s);
  end;
end;

function TRtfdDiagram.hasSelectedControl: boolean;
  var C: TControl;
begin
  C:= Panel.GetFirstSelected;
  Result:= assigned(C);
end;

function TRtfdDiagram.hasSelectedConnection: boolean;
begin
  Result:= Panel.hasSelectedConnection;
end;

procedure TRtfdDiagram.OpenClassOrInterface(Sender: TObject);
begin
  var CName:= (Sender as TSpTBXItem).Caption;
  try
    Screen.Cursor:= crHourglass;
    (Frame.Parent.Owner as TFUMLForm).AddClassToProject(CName);
    Panel.ClearSelection;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TRtfdDiagram.OpenClassWithDialog;
begin
  FJava.MIClassOpenClick(nil);
end;

procedure TRtfdDiagram.NewClass;
begin
  FJava.MINewClassClick(nil);
end;

procedure TRtfdDiagram.ClassInsert;
begin
  if Assigned(UMLForm) then
    UMLForm.TBClassInsertClick(nil);
end;

procedure TRtfdDiagram.ShowUnnamedObject(Sender: TObject);
  var objname: string; p: integer;
      aJavaObject: TComJavaObject;
begin
  try
    LockWindow(UMLForm.Handle);
    objname:= (Sender as TSpTBXItem).Caption;
    p:= Pos(' ', objname);
    while p > 0 do begin
      delete(objname, 1, p);
      p:= Pos(' ', objname);
    end;
    aJavaObject:= ComJava.NewUnnamedObject(objname);
    if aJavaObject.isValid then begin
      ShowNewObject(aJavaObject);
      ResolveObjectAssociations;
      UpdateAllObjects;
    end else
      FreeAndNil(aJavaObject);
  finally
    UnlockWindow;
  end;
end;

procedure TRtfdDiagram.ShowObject(const objname: string);
begin
  var aJavaObject:= ComJava.GetObject(objname);
  if assigned(aJavaObject) then
    ShowNewObject(aJavaObject);
end;

procedure TRtfdDiagram.ShowAllNewObjects(Sender: TObject);
  var i: integer;
begin
  try
    LockFormUpdate(UMLForm);
    Screen.Cursor:= crHourglass;
    if Frame.MIObjectPopupShowNewObject.Visible then
      for i:= 0 to Frame.MIObjectPopupShowNewObject.Count - 1 do
        ShowUnnamedObject(Frame.MIObjectPopupShowNewObject.Items[i])
    else begin
      i:= 2;
      while i < Frame.PopMenuObject.Items.Count - 1 do begin
        if Frame.PopMenuObject.Items[i].Tag = -2 then
          ShowUnnamedObject(Frame.PopMenuObject.Items[i]);
        inc(i);
      end;
    end;
    ResolveObjectAssociations;
  finally
    UnLockFormUpdate(UMLForm);
    Screen.Cursor:= crDefault;
  end;
end;

procedure TRtfdDiagram.ShowAllNewObjectsString(From: string = '');
  var aJavaObject: TComJavaObject;
      aJavaObject2: TComJavaObject;
      i, j, k, p: integer;
      newobj, newobj2, typ: string;
      SL1, SL2: TStringList;

  function NotShown(s, t: string): boolean;
  begin
    if BoxNames.IndexOf(s) >= 0 then exit(false);
    Result:= true;
    if StartsWith(t, 'Ljava.util.') then exit;
    Result:= (s <> '') and (s <> 'null') and (BoxNames.IndexOf(s) = -1) and
              not StartsWith(t, 'Ljava.') and not Startswith(t, 'Lsun.') and not StartsWith(t, 'Ljavax.') and
             (pos('NumberField', t) = 0);
    if Pos('Ljava.lang.', t) = 1 then begin
      delete(t, 1, 11);
      if (t = 'Boolean;') or (t = 'Byte;') or (t = 'Character;') or (t = 'Double;') or
         (t = 'Float;') or (t = 'Integer;') or (t = 'Long;') or (t = 'Short;') or (t = 'String;') then
        Result:= true;
    end;
  end;

begin
  i:= 0;
  while i < ComJava.ObjectList.Count do begin
    aJavaObject:= TComJavaObject(ComJava.ObjectList.Objects[i]);
    SL1:= aJavaObject.getObjectAttributeValues(true);
    for j:= 0 to SL1.Count - 1 do begin
      newobj:= trim(SL1[j]);
      if newobj = '' then
        continue;
      if Pos('{', newobj) + Pos('[', newobj) = 1 then
        newobj:= copy(newobj, 2, Length(newobj)-2);
      SL2:= split(',', newobj);
      for k:= 0 to SL2.Count - 1 do begin
        newobj2:= trim(SL2[k]);
        typ:= '';
        p:= Pos('=', newobj2);
        if p > 0 then begin
          typ:= copy(newobj2, p+1, length(newobj2));
          newobj2:= copy(newobj2, 1, p-1);
        end;

        if NotShown(newobj2, typ) then begin
          aJavaObject2:= ComJava.NewUnnamedObject(newobj2);
          if aJavaObject2.isValid then begin
            if From = '' then From:= 'Actor';
            ShowMethodEntered('<init>', From, aJavaObject2.Name, '');
            ShowNewObject(aJavaObject2);
            Interactive.Executer.AddVariable(newobj, aJavaObject2.ClassRef.Name, aJavaObject2.CreateValue);
          end else
            FreeAndNil(aJavaObject2);
        end;
      end;
      FreeAndNil(SL2);
    end;
    FreeAndNil(SL1);
    inc(i);
  end;
  ResolveObjectAssociations;
  UpdateAllObjects;
  Panel.RecalcSize;
end;

procedure TRtfdDiagram.ConnectBoxes(Sender: TObject);
  var CName: string; Src: TControl; Dest: TRtfdBox; UMLForm: TFUMLForm;
begin
  Src:= Panel.GetFirstSelected;
  if (Src <> nil) and (Src is TRtfdBox) then begin
    if Src.Owner.Owner is TFUMLForm
      then UMLForm:= Src.Owner.Owner as TFUMLForm
      else UMLForm:= nil;
    LockFormUpdate(UMLForm);
    CName:= (Sender as TSpTBXItem).Caption;
    Dest:= GetBox(CName);
    Panel.FindManagedControl(Dest).Selected:= true;
    Panel.ConnectBoxes(Src, Dest);
    UnLockFormUpdate(UMLForm);
  end;
end;

procedure TRtfdDiagram.DoConnection(Item: integer);
begin
  Panel.DoConnection(Item);
end;

procedure TRtfdDiagram.DoAlign(Item: integer);
begin
  Panel.DoAlign(Item);
end;

procedure TRtfdDiagram.SetInteractive(aUMLForm: TForm; path: string; OnInteractiveModified: TNotifyEvent);
begin
  Interactive:= FMessages.AddInteractive(aUMLForm, path);
  InteractivePath:= path;
  ComJava:= Interactive.ComJava;
  OnModified:= OnInteractiveModified;
end;

procedure TRtfdDiagram.SetFormMouseDown(OnFormMouseDown: TNotifyEvent);
begin
  Panel.OnFormMouseDown:= OnFormMouseDown;
end;

function TRtfdDiagram.getComJava:TComJava1;
begin
  Result:= ComJava;
end;

procedure TRtfdDiagram.AddToInteractive(const s: string);
  var s1: string; line: integer;
begin
  try
    Interactive.InteractiveEditor.Lines.Add(s);
    line:= Interactive.InteractiveEditor.Lines.Count - 1;
    if line < 1 then line:= 1;
    Interactive.InteractiveEditor.TopLine:= line;
    if Assigned(OnModified) then OnModified(nil);
  except
    on e: Exception do begin
      if Assigned(OnModified)
        then s1:= 'AddToInteractive: OnModified assigned '
        else s1:= 'AddToInteractive: OnModified = nil ';
      if assigned(Interactive) then
        if assigned(Interactive.InteractiveEditor)
          then s1:= s1 + 'Interactive.InteractiveEditor assigned '
          else s1:= s1 + 'Interactive.InteractiveEditor = nil '
      else s1:= s1 + 'Interactive = nil ';
      s1:= s1 + ' s = ' + s;
      FConfiguration.Log(s1, e);
    end;
  end;
end;

function TRtfdDiagram.hasInteractiveCode: boolean;
begin
  Result:= assigned(Interactive) and assigned(Interactive.InteractiveEditor) and
          (Interactive.InteractiveEditor.Lines.Count > 0);
end;

procedure TRtfdDiagram.InteractiveSetModified(Modified: boolean);
begin
  try
    Interactive.InteractiveEditor.Modified:= Modified;
  finally
  end;
end;

function TRtfdDiagram.EditClass(const Caption, Title, ObjectNameOld: string;
           var ObjectNameNew: string; Attributes: TStringList): boolean;
  var i: integer; s: string;
begin
  FObjectGenerator.PrepareEditClass(Caption, Title, ObjectNameOld);
  Result:= FObjectGenerator.Edit(Attributes, 2);
  if Result then begin
    ObjectNameNew:= FObjectGenerator.ValueListEditor.Cells[1, 1];
    for i:= 0 to Attributes.Count - 1 do begin
      s:= FObjectGenerator.ValueListEditor.Cells[1, i+2];
      TComJavaValue(Attributes.objects[i]).SetFromString(s);
    end;
  end;
end;

function TRtfdDiagram.EditObjectOrParams(const Caption, Title: string; Attributes: TStringList): boolean;
begin
  FObjectGenerator.PrepareEditObjectOrParams(Caption, Title);
  Result:= FObjectGenerator.Edit(Attributes, 1);
  if Result then
    for var i:= 0 to Attributes.Count - 1 do begin
      var s:= FObjectGenerator.ValueListEditor.Cells[1, i+1];
      TComJavaValue(Attributes.objects[i]).SetFromString(s);
    end;
end;

procedure TRtfdDiagram.DeleteObjects;
  var i: integer; aObject: TRtfdObject; ManagedObject: TManagedObject;
begin
  UnSelectAllElements;
  Application.ProcessMessages;
  for i:= BoxNames.Count - 1 downto 0 do
    if (BoxNames.Objects[i] is TRtfdObject) then begin
      aObject:= BoxNames.Objects[i] as TRtfdObject;
      ManagedObject:= Panel.FindManagedControl(aObject);
      if assigned(ManagedObject) then
        ManagedObject.Selected:= true;
    end;
  try
    DeleteSelectedControls(nil);
    Panel.Invalidate;
    if assigned(Interactive) and assigned(Interactive.Executer) then
      Interactive.Executer.Clear;
  except on e: exception do
    FConfiguration.Log('TRtfdDiagram.DeleteObjects; ', e);
  end;
end;

function TRtfdDiagram.hasObjects: boolean;
begin
  for var i:= 0 to BoxNames.Count - 1 do
    if (BoxNames.Objects[i] is TRtfdObject) then
      exit(true);
  Result:= false;
end;

procedure TRtfdDiagram.DeleteObject(const objectname: string);
  var i: integer; aObject: TRtfdObject; ManagedObject: TManagedObject;
begin
  for i:= BoxNames.Count - 1 downto 0 do
    if (BoxNames.Objects[i] is TRtfdObject) then begin
      aObject:= BoxNames.Objects[i] as TRtfdObject;
      if aObject.Entity.Name = objectname then begin
        ManagedObject:= Panel.FindManagedControl(aObject);
        if assigned(ManagedObject) then
          ManagedObject.Selected:= true;
      end;
    end;
  try
    DeleteSelectedControls(nil);
  except on e: exception do
    FConfiguration.Log('TRtfdDiagram.DeleteObject; ', e);
  end;
//  Interactive.Executer.Clear;
end;

procedure TRtfdDiagram.SetRecursiv(P: TPoint; pos: integer);
begin
  Panel.SetRecursiv(P, pos);
end;

function TRtfdDiagram.getModelClass(const s: string): TClass;
  var Ci: IModelIterator; cent: TClassifier; typ: string;
begin
  Result:= nil;
  Ci:= Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext do begin
    cent:= TClassifier(Ci.Next);
    if cent is TClass then begin
      typ:= (cent as TClass).getTyp;
      if (typ = s) or (typ = GetShortType(s)) then begin
        Result:= (cent as TClass);
        break;
      end;
    end;
  end;
  if assigned(Result) and (Result.Pathname = '') then begin
    while Ci.HasNext do begin
      cent:= TClassifier(Ci.Next);
      if cent is TClass then begin
        typ:= (cent as TClass).getTyp;
        if (typ = s) or (typ = GetShortType(s)) then begin
          Result:= (cent as TClass);
          break;
        end;
      end;
    end;
  end;
end;

function TRtfdDiagram.StringToArrowStyle(s: string): TessConnectionArrowStyle;
  var i: integer; t: TessConnectionArrowStyle;
begin
  Result:= asAssociation1;
  s:= trim(s);
  if TryStrToInt(s, i) then // pre 10.3 uml-file-format
    Result:= TessConnectionArrowStyle(i)
  else
    for t:= asAssociation1 to asComment do
      if ArrowStyleToString(t) = s then begin
        Result:= t;
        exit;
      end;
end;

function TRtfdDiagram.ArrowStyleToString(ArrowStyle: TessConnectionArrowStyle): string;
begin
  case ArrowStyle of
    asAssociation1: Result:= 'Association';
    asAssociation2: Result:= 'AssociationDirected';
    asAssociation3: Result:= 'AssociationBidirectional';
    asAggregation1: Result:= 'Aggregation';
    asAggregation2: Result:= 'AggregationArrow';
    asComposition1: Result:= 'Composition';
    asComposition2: Result:= 'CompositionArrow';
    asInheritends:  Result:= 'Inheritends';
    asImplements:   Result:= 'Implements';
    asInstanceOf:   Result:= 'InstanceOf';
    asComment:      Result:= 'Comment';
  end;
end;

function TRtfdDiagram.getSourcepath: string;
begin
  Result:= '';
  if assigned(Model) and assigned(Model.ModelRoot) then begin
    var SL:= Model.ModelRoot.Files;
    for var i:= 0 to SL.Count - 1 do begin
      var s:= SL.Strings[i];
      s:= ExtractFilePath(s);
      if Pos(s, Result) = 0 then
        Result:= Result + ';' + s;
    end;
    Delete(Result, 1, 1);
  end;
end;

procedure TRtfdDiagram.JavaReset;
begin
  ComJava.JavaReset;
end;

function TRtfdDiagram.getCommentBoxName: string;
  var i, Nr, CommentNr: integer;
      s: string;
begin
  CommentNr:= 0;
  for i:= 0 to BoxNames.Count - 1 do
    if Pos('Comment: ', BoxNames[i]) = 1 then begin
      s:= copy(BoxNames[i], 9, 255);
      if TryStrToInt(s, Nr) then
        CommentNr:= Math.max(CommentNr, Nr);
    end;
  Result:= 'Comment: ' + IntToStr(CommentNr+1);
end;

procedure TRtfdDiagram.AddCommentBoxTo(aControl: TControl);
  var CommentBox: TRtfdCommentBox;
      aClass: TRtfdClass;
      s: string;
begin
  s:= getCommentBoxName;
  CommentBox:= TRtfdCommentBox.Create(Panel, S, Frame, viPublic, HANDLESIZE);
  CommentBox.Top:= 50 + random(50);
  CommentBox.Left:= 50 + random(50);
  CommentBox.Width:= 150;
  CommentBox.Height:= 100;
  CommentBox.Font.Assign(Font);
  BoxNames.AddObject(S, CommentBox);
  Panel.AddManagedObject(CommentBox);

  if aControl = nil then
    aControl:= Panel.GetFirstSelected;
  if assigned(aControl) and (aControl is TRtfdClass) then begin
    aClass:= (aControl as TRtfdClass);
    CommentBox.Top:= aClass.Top + random(50);
    CommentBox.Left:= aClass.Left + aClass.Width + 100 + random(50);
    Panel.ConnectObjects(aClass, CommentBox, asComment);
  end;
  Panel.IsModified:= true;
  Panel.RecalcSize;
  Panel.ShowConnections;
  CommentBox.SendToBack;
end;

function TRtfdDiagram.hasClass(aClassname: string): boolean;
begin
  Result:= false;
  for var i:= 0 to BoxNames.Count - 1 do
    if BoxNames[i] = aClassname then
      exit(true);
end;

procedure TRtfdDiagram.DebugJE2Java;
begin
  ComJava.ExecuteCommand('ShowHide');
end;

procedure TRtfdDiagram.DoShowParameter(aControl: TControl; Mode: integer);
begin
  if aControl is TRtfdBox then begin
    var Box:= (aControl as TRtfdBox);
    if Box.ShowParameter <> Mode then begin
      Box.ShowParameter:= Mode;
      Panel.ShowAll;
      Panel.IsModified:= true;
    end;
  end;
end;

procedure TRtfdDiagram.DoShowVisibility(aControl: TControl; Mode: integer);
begin
  if aControl is TRtfdBox then begin
    var Box:= (aControl as TRtfdBox);
    if Box.ShowIcons <>  Mode then begin
      Box.ShowIcons:= Mode;
      Panel.ShowAll;
      Panel.IsModified:= true;
    end;
  end;
end;

procedure TRtfdDiagram.DoShowVisibilityFilter(aControl: TControl; Mode: integer);
begin
  if aControl is TRtfdBox then begin
    var Box:= (aControl as TRtfdBox);
    if Box.MinVisibility <> TVisibility(Mode) then begin
      Box.MinVisibility:= TVisibility(Mode);
      Panel.ShowAll;
      Panel.IsModified:= true;
    end;
  end;
end;

procedure TRtfdDiagram.CreateTestClass(aControl: TControl);
begin
  if (aControl is TRtfdClass) then begin
    var aClass:= (aControl as TRtfdClass);
    var aClassname:= aClass.Entity.Name + 'Test';
    var Filename:= ExtractFilepath(aClass.getPathname) + WithoutGeneric(aClassname) + '.java';
    if FileExists(Filename) and
                 (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                            mtConfirmation, mbYesNoCancel, 0) = mrYes) or
                 not FileExists(Filename)
    then begin
      var SL:= TStringList.Create;
      SL.Text:= FTemplates.GetTemplate(aClassname, 12);
      if SL.Text = '' then SL.Text:= FTemplates.GetTestClassCode(aClassname);
      SL.SaveToFile(Filename);
      UMLForm.MainModul.AddToProject(Filename);
      UMLForm.Modified:= true;
    end;
  end;
end;

procedure TRtfdDiagram.OnRunJunitTestMethod(Sender: TObject);
  var aMenuItem: TSpTBXItem; s: string; p: integer;
      C: TControl;
begin
  C:= FindVCLWindow(Frame.PopMenuClass.PopupPoint);
  if assigned(C) and (Sender is TSpTBXItem) then begin
    aMenuItem:= Sender as TSpTBXItem;
    p:= FullParameters.IndexOfName(aMenuItem.Caption);
    s:= FullParameters.ValueFromIndex[p];
    RunTests(C, s);
  end;
end;

procedure TRtfdDiagram.RunTests(aControl: TControl; const Method: string);
begin
  if aControl is TRtfdClass then
    if (aControl as TRtfdClass).Entity is TClass then begin
      if FJUnitTests = nil then
        FJUnitTests:= TFJUnitTests.Create(FJava);
      FJUnitTests.Pathname:= ((aControl as TRtfdClass).Entity as TClass).Pathname;
      myJavaCommands.RunTests((aControl as TRtfdClass).Entity as TClass, Method);
    end;
end;

procedure TRtfdDiagram.Lock(b: boolean);
begin
  for var i:= 0 to BoxNames.Count - 1 do
    (BoxNames.Objects[i] as TRtfdBox).Lock(b);
end;

procedure TRtfdDiagram.ShowMethodEntered(const aMethodname, From, _To, Parameter: string);
begin
  if assigned(SequenceForm) then begin
    Sequenceform.MethodEntered(aMethodname);
    if FConfiguration.SDShowParameter then
      Sequenceform.addParameter(Parameter);
    Sequenceform.FromParticipant:= From;
    Sequenceform.ToParticipant:= _To;
    Sequenceform.makeConnection;
  end;
end;

procedure TRtfdDiagram.ShowMethodExited(const aMethodname, From, _To, _Result: string);
begin
  if assigned(SequenceForm) then begin
    Sequenceform.MethodExited(aMethodname);
    Sequenceform.FromParticipant:= From;
    Sequenceform.ToParticipant:= _To;
    Sequenceform.aResult:= _Result;
    Sequenceform.makeConnection;
  end;
end;

procedure TRtfdDiagram.ShowObjectDeleted(const From, _To: string);
begin
  if assigned(SequenceForm) then begin
    Sequenceform.ObjectDelete;
    Sequenceform.FromParticipant:= From;
    Sequenceform.ToParticipant:= _To;
    Sequenceform.makeConnection;
  end;
end;

procedure TRtfdDiagram.CloseNotify(Sender: TObject);
begin
  SequenceForm:= nil;
end;

procedure TRtfdDiagram.ClearSelection;
begin
  Panel.ClearSelection();
end;

procedure TRtfdDiagram.ChangeStyle;
begin
  if FConfiguration.isDark then begin
    Frame.PopMenuClass.Images:= Frame.vilClassObjectDark;
    Frame.PopMenuObject.Images:= Frame.vilClassObjectDark;
    Frame.PopMenuConnection.Images:= Frame.vilAssociationsDark;
    Frame.PopupMenuWindow.Images:= Frame.vilWindowDark;
    Frame.PopupMenuAlign.Images:= Frame.vilAlignDark;
  end else begin
    Frame.PopMenuClass.Images:= Frame.vilClassObjectLight;
    Frame.PopMenuObject.Images:= Frame.vilClassObjectLight;
    Frame.PopMenuConnection.Images:= Frame.vilAssociationsLight;
    Frame.PopupMenuWindow.Images:= Frame.vilWindowLight;
    Frame.PopupMenuAlign.Images:= Frame.vilAlignLight;
  end;
  Panel.ChangeStyle;
end;

procedure TRtfdDiagram.CopyDiagramToClipboard;
var
  Selected: boolean;
  B1, B2:  Graphics.TBitmap;
  W, H : integer;
  SelRect: TRect;
begin
  Panel.ChangeStyle(true);
  SelRect:= GetSelectedRect;
  Selected:= (SelRect.Right > SelRect.Left);
  GetDiagramSize(W, H);
  try
    B1:= Graphics.TBitmap.Create;
    B1.Width := W;
    B1.Height:= H;
    B1.Canvas.Lock;
    PaintTo(B1.Canvas, 0, 0, True);

    B2:= Graphics.TBitmap.Create;
    if Selected then begin
      B2.Width := SelRect.Right - SelRect.Left + 2;
      B2.Height:= SelRect.Bottom - SelRect.Top + 2;
      B2.Canvas.Draw(-SelRect.Left + 1, -SelRect.Top + 1, B1);
      Clipboard.Assign(B2) end
    else
      Clipboard.Assign(B1);
    B1.Canvas.Unlock;
  finally
    FreeAndNil(B1);
    FreeAndNil(B2);
  end;
  ClearSelection;
  Panel.ChangeStyle(false);
end;

procedure TRtfdDiagram.ClearMarkerAndConnections(Control: TControl);
begin
  Panel.ClearMarkerAndConnections(Control);
end;

procedure TRtfdDiagram.DrawMarkers(r: TRect; show: boolean);
begin
  Panel.DrawMarkers(r, show);
end;

procedure TRtfdDiagram.EditBox(Control: TControl);
begin
  Panel.EditBox(Control);
end;

procedure TRtfdDiagram.SetModified(const Value: boolean);
begin
  Panel.SetModified(Value);
end;

procedure TRtfdDiagram.SetOnModified(OnBoolEvent: TBoolEvent);
begin
  Panel.OnModified:= OnBoolEvent;
end;

procedure TRtfdDiagram.SetOnSelectionChanged(Sender: TNotifyEvent);
begin
  Panel.OnSelectionChanged:= Sender;
end;

procedure TRtfdDiagram.DeleteComment;
begin
  var C:= FindVCLWindow(Frame.PopupMenuComment.PopupPoint);
  if Assigned(C) then begin
    var ManagedObject:= Panel.FindManagedControl(C);
    if assigned(ManagedObject) then begin
      UnSelectAllElements;
      ManagedObject.Selected:= true;
      DeleteSelectedControls(nil);
    end;
  end;
end;

function TRtfdDiagram.getClasses: TStringList;
begin
  Result:= TStringList.Create;
  for var i:= 0 to BoxNames.Count - 1 do
    if (BoxNames.Objects[I] is TRtfdClass) or
       (BoxNames.Objects[I] is TRtfdInterface) then
      Result.Add(BoxNames[i]);
end;

procedure TRtfdDiagram.Retranslate;
begin
  Frame.Retranslate;
end;

function TRtfdDiagram.PanelIsLocked: boolean;
begin
  Result:= (Panel.UpdateCounter > 0);
end;

procedure TRtfdDiagram.SetUMLFont;
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options:= [];
  if FJava.FDFont.Execute then begin
    Font.Assign(FJava.FDFont.Font);
    FConfiguration.UMLFont.Assign(Font);
    SetFont(Font);
    RefreshDiagram;
  end;
end;

end.
