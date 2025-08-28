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

uses
  Controls,
  Types,
  Graphics,
  Classes,
  Forms,
  ExtCtrls,
  UDiagramFrame,
  URtfdComponents,
  UListeners,
  UJniWrapper1,
  UComJava1,
  UMessages,
  UViewIntegrator,
  UEssConnectPanel,
  UModelEntity,
  UModel,
  UUtils,
  UUMLForm,
  USequenceForm,
  UConnection;

type
  TRtfdDiagram = class(TDiagramIntegrator, IBeforeObjectModelListener,
    IAfterObjectModelListener, IAfterUnitPackageListener)
  private
    // Model: TObjectModel is inherited
    FIsAllClasses: Boolean;
    FInteractivePath: string;
    FOnModified: TNotifyEvent;

    FBoxNames: TStringList;
    FFullParameters: TStringList;
    FInteractive: TInteractive;
    FComJava: TComJava1;
    FFrame: TAFrameDiagram;
    FPanel: TEssConnectPanel;
    FSequenceForm: TFSequenceForm;
    FUMLForm: TFUMLForm;

    procedure AddBox(ModelEntity: TModelEntity);
    function GetBox(Typ: string): TRtfdBox;
    procedure ModelBeforeChange(Sender: TModelEntity);
    procedure ModelAfterChange(Sender: TModelEntity);
    procedure IBeforeObjectModelListener.Change = ModelBeforeChange;
    procedure IAfterObjectModelListener.Change = ModelAfterChange;
    procedure UnitPackageAfterChange(Sender: TModelEntity);
    procedure UnitPackageAfterAddChild(Sender: TModelEntity;
      NewChild: TModelEntity);
    procedure UnitPackageAfterRemove(Sender: TModelEntity);
    procedure UnitPackageAfterEntityChange(Sender: TModelEntity);
    procedure IAfterUnitPackageListener.Change = UnitPackageAfterChange;
    procedure IAfterUnitPackageListener.AddChild = UnitPackageAfterAddChild;
    procedure IAfterUnitPackageListener.Remove = UnitPackageAfterRemove;
    procedure IAfterUnitPackageListener.EntityChange =
      UnitPackageAfterEntityChange;
    function PPIScale(ASize: Integer): Integer;
    function PPIUnScale(ASize: Integer): Integer;
    function PanelIsLocked: Boolean;
    procedure ShowRelationshipAttributesBold;
  protected
    procedure SetVisibilityFilter(const Value: TVisibility); override;
    procedure SetShowParameter(const Value: Integer); override;
    procedure SetShowView(Value: Integer); override;
    procedure SetSortOrder(const Value: Integer); override;
    procedure SetShowIcons(const Value: Integer); override;
    procedure SetShowObjectDiagram(const Value: Boolean); override;
    procedure CurrentEntityChanged; override;
  public
    constructor Create(ObjectModel: TObjectModel; Parent: TWinControl);
      override;
    destructor Destroy; override;
    procedure ClearDiagram; override;
    procedure ResolveAssociations; override;
    procedure ResolveObjectAssociations; override;
    procedure InitFromModel; override;
    procedure PaintTo(Canvas: TCanvas; X, Y: Integer;
      SelectedOnly: Boolean); override;
    procedure GetDiagramSize(var Width, Height: Integer); override;
    procedure SetPackage(const Value: TAbstractPackage); override;
    procedure DoLayout; override;
    function GetClickAreas: TStringList; override;
    procedure ClassEditSelectedDiagramElements; overload; override;
    procedure ClassEditSelectedDiagramElements(Sender: TObject);
      overload; override;
    procedure SourceEditSelectedDiagramElementsControl
      (Control: TControl); override;

    procedure UnSelectAllElements; override;
    function GetSelectedRect: TRect; override;
    procedure ScreenCenterEntity(E: TModelEntity); override;
    procedure SetFont(const AFont: TFont); override;
    function GetFont: TFont; override;

    function JavaInsteadClass(StringList: TStringList): TStringList;
    procedure StoreDiagram(Filename: string); override;
    procedure FetchDiagram(Filename: string); override;

    procedure RefreshDiagram; override;
    procedure RecalcPanelSize; override;
    procedure SetShowConnections(const Value: Integer); override;

    procedure SelectAssociation; override;
    procedure DeleteSelectedControls(Sender: TObject);
    procedure DeleteSelectedControlsAndRefresh; override;
    procedure DeleteObjects; override;
    procedure DeleteObject(const Objectname: string);
    function HasObjects: Boolean; override;
    function HasEditableClass: Boolean; override;
    function HasSelectedControl: Boolean; override;
    function HasSelectedConnection: Boolean; override;
    function GetFrame: TAFrameDiagram;
    function GetPanel: TCustomPanel; override;
    procedure SetInteractive(AUMLForm: TForm; Path: string;
      OnInteractiveModified: TNotifyEvent); override;
    procedure SetFormMouseDown(OnFormMouseDown: TNotifyEvent); override;
    function GetComJava: TComJava1;
    function HasInteractiveCode: Boolean; override;
    procedure AddToInteractive(const Str: string);
    procedure InteractiveSetModified(Modified: Boolean); override;
    function GetSourcePath: string; override;

    function GetFilesAndPackages(Selected: Boolean): TStringList;
    function GetFilesAndPackagesFromList(List: TList): TStringList;
    procedure CompileOneWith(Control: TControl; Compiler: string); override;
    function GetAllPathnames: TStringList;
    function GetAllClassnames: TStringList;
    function GetFileWithMain: string;
    procedure Run(Control: TControl); override;
    procedure ShowInheritedMethodsFromSystemClasses(Control: TControl;
      ShowOrHide: Boolean); override;

    procedure OpenClassOrInterface(Sender: TObject);
    procedure OpenClassWithDialog; override;
    procedure NewClass; override;
    procedure ClassInsert; override;
    procedure ShowUnnamedObject(Sender: TObject); override;
    procedure ShowAllNewObjects(Sender: TObject); override;
    procedure ShowAllNewObjectsString(From: string = '');

    procedure ShowObject(const ObjName: string);
    procedure ConnectBoxes(Sender: TObject);
    procedure DoConnection(Item: Integer); override;
    procedure DoAlign(Item: Integer); override;

    procedure PopMenuClassPopup(Sender: TObject); override;
    procedure PopMenuObjectPopup(Sender: TObject); override;
    procedure PopMenuConnectionPopup(Sender: TObject); override;

    procedure CreateObjectForSelectedClass(Sender: TObject);
    function CreateModelClass(const Typ: string): TClass;
    function FindClassifier(const CName: string): TClassifier;
    procedure ShowNewObject(AJavaObject: TComJavaObject);
    procedure CallMethod(AControl: TControl; Sender: TObject);
    procedure CallMethodForObject(Sender: TObject);
    procedure CallMethodForClass(Sender: TObject);
    procedure CallMain(const Classpath, AClassname: string;
      CallParameter: string);
    function CollectClasses: Boolean;
    function MakeParams(Parameter: TStringList; var TheParams: TComJavaParams;
      var AsString: string): Boolean;
    procedure DeleteParams(Parameter: TStringList);

    procedure EditObject(Control: TControl); override;
    procedure OpenClass(Control: TControl); override;
    function ClassHasObjects(ABox: TRtfdBox): Boolean;
    function HasAttributes(AModelClass: TClass): Boolean;
    procedure UpdateAllObjects;
    procedure ShowAttributes(AJavaObject: TComJavaObject;
      AModelObject: TObjekt);
    procedure GetAllAttributeValues(AClass: TClass; AJavaObject: TComJavaObject;
      AAttribut: TComJavaAttribute; Attributes: TStringList);
    procedure SetAttributeValues(AClass: TClass; AJavaObject: TComJavaObject;
      AAttribut: TComJavaAttribute; Attributes: TStringList);
    function EditClass(const Caption, Title, ObjectNameOld: string;
      var ObjectNameNew: string; Control: TControl; Attributes: TStringList): Boolean;
    function EditObjectOrParams(const Caption, Title: string; Control: TControl;
      Attributes: TStringList): Boolean;
    procedure SetRecursiv(Posi: TPoint; Pos: Integer); override;
    function GetModelClass(const Str: string): TClass;
    function StringToArrowStyle(Str: string): TEssConnectionArrowStyle;
    function ArrowStyleToString(ArrowStyle: TEssConnectionArrowStyle): string;
    procedure JavaReset;
    function GetCommentBoxName: string;
    procedure AddCommentBoxTo(AControl: TControl); override;
    function InsertParameterNames(Str: string): string;
    function HasClass(AClassname: string): Boolean;
    procedure DebugJE2Java;
    procedure DoShowParameter(Control: TControl; Mode: Integer); override;
    procedure DoShowVisibility(Control: TControl; Mode: Integer); override;
    procedure DoShowVisibilityFilter(Control: TControl; Mode: Integer);
      override;
    procedure CreateTestClass(Control: TControl); override;
    procedure Lock(ALock: Boolean); override;
    procedure RunTests(Control: TControl; const Method: string); override;
    procedure OnRunJunitTestMethod(Sender: TObject);
    procedure ShowMethodEntered(const AMethodname, From, Till,
      Parameter: string);
    procedure ShowMethodExited(const AMethodname, From, Till, AResult: string);
    procedure ShowObjectDeleted(const From, Till: string);
    procedure CloseNotify(Sender: TObject);
    procedure ClearSelection; override;
    procedure CopyDiagramToClipboard; override;
    procedure ClearMarkerAndConnections(Control: TControl); override;
    procedure DrawMarkers(Rect: TRect; Show: Boolean); override;
    procedure EditBox(Control: TControl); override;
    procedure SetModified(const Value: Boolean); override;
    procedure SetOnModified(OnBoolEvent: TBoolEvent); override;
    procedure SetOnSelectionChanged(Sender: TNotifyEvent); override;
    procedure ChangeStyle; override;
    procedure DeleteComment; override;
    function GetDebug: TStringList;
    function GetSVG: string; override;
    function GetClasses: TStringList; override;
    procedure Retranslate; override;
    procedure SetUMLFont; override;
    function HasAInvalidClass: Boolean; override;

    property Frame: TAFrameDiagram read FFrame;
    property Panel: TEssConnectPanel read FPanel;
    property SequenceForm: TFSequenceForm read FSequenceForm write FSequenceForm;
  end;

implementation

uses
  Windows,
  Math,
  SysUtils,
  StrUtils,
  IniFiles,
  Dialogs,
  Contnrs,
  Clipbrd,
  JvGnugettext,
  SpTBXItem,
  jni,
  UJUnitTest,
  UStringRessources,
  UIterators,
  USugiyamaLayout,
  UDlgMethodCall,
  URtfdDiagramFrame,
  UIntegrator,
  UJava,
  UConfiguration,
  UObjectGenerator,
  UCodeCompletion,
  UITypes,
  UEditorForm,
  UJavaCommands,
  UDlgAbout,
  UTemplates;

{ TRtfdDiagram }

constructor TRtfdDiagram.Create(ObjectModel: TObjectModel; Parent: TWinControl);
begin
  inherited Create(ObjectModel, Parent);
  FFrame := TAFrameRtfdDiagram.Create(Parent, Self);
  FFrame.Parent := Parent; // assigment to the gui
  FUMLForm := TFUMLForm(Parent.Parent.Parent);

  // FPanel is ActiveControl in MainForm
  FPanel := TEssConnectPanel.Create(FUMLForm);
  FPanel.PopupMenuConnection := FFrame.PopMenuConnection;
  FPanel.PopupMenuAlign := FFrame.PopupMenuAlign;
  FPanel.PopupMenuWindow := FFrame.PopupMenuWindow;
  FPanel.Parent := FFrame.ScrollBox;
  FPanel.OnDeleteSelectedControls := DeleteSelectedControls;
  FPanel.OnClassEditSelectedDiagramElements := ClassEditSelectedDiagramElements;
  FOnModified := nil;
  FSequenceForm := nil;

  FBoxNames := TStringList.Create;
  FBoxNames.CaseSensitive := True;
  FBoxNames.Sorted := True;
  FBoxNames.Duplicates := dupIgnore;
  FFullParameters := TStringList.Create;

  Model.AddListener(IBeforeObjectModelListener(Self));
  ClearDiagram;
end;

destructor TRtfdDiagram.Destroy;
begin
  // Force listeners to release, and diagram to persist.
  // Package:= nil
  // Model.RemoveListener(IBeforeObjectModelListener(Self))
  Model.ClearListeners;

  FreeAndNil(FFullParameters);
  for var I := 0 to FBoxNames.Count - 1 do
  begin
    var
    Box := TRtfdBox(FBoxNames.Objects[I]);
    FreeAndNil(Box);
  end;
  FreeAndNil(FBoxNames);
  FreeAndNil(FInteractive);
  // dont do that: FreeAndNil(FFrame);
  // FFrame is part of the gui and therefore is destroyed as GUI-component
  // FPanel is part of FFrame so it is also destroyed by system as GUI-component
  inherited; // Model is inherited
end;

procedure TRtfdDiagram.InitFromModel;

var
  MIte: IModelIterator;

  procedure InAddUnit(Package: TUnitPackage);
  begin
    var
    Mite1 := Package.GetClassifiers;
    while Mite1.HasNext do
    begin
      var
      Classifier := TClassifier(Mite1.Next);
      if Classifier.IsVisible then
        AddBox(Classifier);
    end;
  end;

begin
  FIsAllClasses := (Package = AllClassesPackage);
  FIsAllClasses := True; // otherwise no Delpi-class is shown
  FPanel.Hide;
  if not Assigned(FPackage) then
  begin
    Package := Model.ModelRoot;
    // If there is only one package (except unknown) then show it.
    // Assign with Package-property to trigger listeners
    MIte := TLogicPackage(FPackage).GetPackages;
    if MIte.Count = 2 then
    begin
      MIte.Next;
      Package := TAbstractPackage(MIte.Next);
    end;
  end;

  // Clean old
  ClearDiagram;

  // Create boxes
  if FPackage is TUnitPackage then
    InAddUnit(TUnitPackage(FPackage))
  else
  begin
    // Logic package
    // Exclude unknown-package, otherwise all temp-classes will be included on showallclasses.
    // Also, unkown-package will be shown on package-overview (including docgen)
    if FIsAllClasses then
    begin
      // These lines show all members of a package on one diagram
      MIte := TModelIterator.Create(TLogicPackage(Model.ModelRoot)
        .GetPackages, TEntitySkipFilter.Create(Model.UnknownPackage));
      while MIte.HasNext do
        InAddUnit(TUnitPackage(MIte.Next));
    end
    else
    begin
      MIte := TModelIterator.Create(TLogicPackage(FPackage).GetPackages,
        TEntitySkipFilter.Create(Model.UnknownPackage));
      while MIte.HasNext do
        AddBox(MIte.Next);
    end;
  end;

  // Create arrow between boxes
  // This must be done after fetchdiagram because connection-setting might be stored
  DoLayout;
  FPanel.RecalcSize;
  FPanel.IsModified := False;
  FPanel.Show;
  if FPanel.CanFocus then
    FPanel.SetFocus;
end;

procedure TRtfdDiagram.ModelBeforeChange(Sender: TModelEntity);
begin
  Package := nil;
  FIsAllClasses := False;
  ClearDiagram;
end;

procedure TRtfdDiagram.ModelAfterChange(Sender: TModelEntity);
begin
  InitFromModel;
end;

procedure TRtfdDiagram.PaintTo(Canvas: TCanvas; X, Y: Integer;
  SelectedOnly: Boolean);
begin
  var
  OldBit := FPanel.BackBitmap;
  FPanel.BackBitmap := nil;
  if SelectedOnly then
  begin
    if FPanel.GetFirstSelected <> nil then
      FPanel.SelectedOnly := True;
  end
  else
    // selection-markers should not be visible in the saved picture
    FPanel.ClearSelection;
  Canvas.Lock;
  try
    FPanel.PaintTo(Canvas.Handle, X, Y);
    FPanel.TextTo(Canvas);
  finally
    Canvas.Unlock;
    FPanel.SelectedOnly := False;
    FPanel.BackBitmap := OldBit;
  end;
end;

function TRtfdDiagram.GetSVG: string;
var
  SVG, ShadowWidth, ShadowIntensity, ShadowWitdh2: string;
  Width, Height: Integer;
begin
  FPanel.GetDiagramSize(Width, Height);
  SVG := '<?xml version="1.0" encoding="UTF-8" ?>'#13#10;
  SVG := SVG + '<svg width="' + IntToStr(Width) + '"' + ' height="' +
    IntToStr(Height) + '"' + ' font-family="' + Font.Name + '"' + ' font-size="'
    + IntToStr(Round(Font.Size * 1.3)) + '">'#13#10;
  if FConfiguration.ShadowWidth > 0 then
  begin
    ShadowWidth := FloatToVal(FConfiguration.ShadowWidth / 2.0);
    ShadowIntensity :=
      FloatToVal(Min(2 * FConfiguration.ShadowIntensity / 10.0, 1));
    ShadowWitdh2 := FloatToVal(Min(FConfiguration.ShadowWidth, 10) * 0.4);
    SVG := SVG + '  <defs>'#13#10 +
      '    <filter style="color-interpolation-filters:sRGB;" id="Shadow">'#13#10
      + '      <feFlood flood-opacity=' + ShadowIntensity +
      ' flood-color="rgb(0,0,0)" result="flood" />'#13#10 +
      '      <feComposite in="flood" in2="SourceGraphic" operator="in" result="composite1"/>'#13#10
      + '      <feGaussianBlur in="composite1" stdDeviation=' + ShadowWitdh2 +
      ' result="blur" />'#13#10 + '      <feOffset dx=' + ShadowWidth + ' dy=' +
      ShadowWidth + ' result="offset" />'#13#10 +
      '      <feComposite in="SourceGraphic" in2="offset" operator="over" result="composite2" />'#13#10
      + '    </filter>'#13#10 + '  </defs>'#13#10;
  end;
  SVG := SVG + FPanel.GetSVGConnections;
  for var I := 0 to FBoxNames.Count - 1 do
    SVG := SVG + TRtfdBox(FBoxNames.Objects[I]).GetSVG;
  Result := SVG + '</svg>'#13#10;
end;

procedure TRtfdDiagram.ClearDiagram;
begin
  if not (csDestroying in FPanel.ComponentState) then
  begin
    FPanel.ClearManagedObjects;
    FPanel.DestroyComponents;
  end;
  FBoxNames.Clear;
end;

// Add a 'Box' to the diagram (class/interface/package/objekt/comment).
procedure TRtfdDiagram.AddBox(ModelEntity: TModelEntity);
var
  MIte: IModelIterator;
  Intf: TInterface;
  AClass: TClass;
  Attribute: TAttribute;

  function InCreateBox(ModelEntity: TModelEntity; BoxT: TRtfdBoxClass)
    : TRtfdBox;
  var
    Vis: TVisibility;
    AClass: TClass;
  begin
    if ModelEntity is TClass then
      AClass := TClass(ModelEntity)
    else
      AClass := nil;
    if Assigned(AClass) and (Pos(FConfiguration.JavaCache, AClass.Pathname) > 0)
    then
      Vis := viPublished
    else
      Vis := viPrivate;
    Result := BoxT.Create(FPanel, ModelEntity, FFrame, Vis);
    Result.Font.Assign(Font);
    if FConfiguration.ArrayListAsIntegratedList and (ModelEntity is TObjekt) and
      (TObjekt(ModelEntity).GetTyp.Name = 'java.util.ArrayList') then
      Result.Visible := False;
    FBoxNames.AddObject(ModelEntity.Name, Result);
  end;

begin
  if ModelEntity is TUnitPackage then
    FPanel.AddManagedObject(InCreateBox(ModelEntity, TRtfdUnitPackage))
  else if ModelEntity is TClass then
  begin
    // Insert related boxes from other packages
    // This should not be done if FIsAllClasses, because then all boxes are inserted anyway
    FIsAllClasses := True; // testweise
    if not FIsAllClasses then
    begin
      // Ancestor that is in another package and that is not already inserted
      // is added to the diagram.
      AClass := TClass(ModelEntity);
      if Assigned(AClass.Ancestor) and
        (AClass.Ancestor.Owner <> ModelEntity.Owner) and
        not Assigned(GetBox(AClass.Ancestor.Fullname)) then
      begin
        FPanel.AddManagedObject(InCreateBox(AClass.Ancestor, TRtfdClass));
      end;
      // Implementing interface that is in another package and is not already inserted
      // is added to the diagram.
      MIte := AClass.GetImplements;
      while MIte.HasNext do
      begin
        Intf := TInterface(MIte.Next);
        if (Intf.Owner <> ModelEntity.Owner) and not Assigned(GetBox(Intf.Fullname))
        then
          FPanel.AddManagedObject(InCreateBox(Intf, TRtfdInterface));
      end;
      // Attribute associations that are in other packages are added
      MIte := AClass.GetAttributes;
      while MIte.HasNext do
      begin
        Attribute := TAttribute(MIte.Next);
        if Assigned(Attribute.TypeClassifier) and
          not Assigned(GetBox(Attribute.TypeClassifier.Fullname)) and
          (Attribute.TypeClassifier <> AClass) and
          (Attribute.TypeClassifier <> AClass.Ancestor) and
          (Attribute.TypeClassifier.Owner <> Model.UnknownPackage) then
        // Avoid getting temp-types from unknown (java 'I' for example)
        begin
          if Attribute.TypeClassifier is TClass then
            FPanel.AddManagedObject(InCreateBox(Attribute.TypeClassifier,
              TRtfdClass));
          if Attribute.TypeClassifier is TInterface then
            FPanel.AddManagedObject(InCreateBox(Attribute.TypeClassifier,
              TRtfdInterface));
        end;
      end;
    end;
    if ModelEntity.IsVisible and (GetBox(ModelEntity.GetFullnameWithoutOuter)
      = nil) then
      FPanel.AddManagedObject(InCreateBox(ModelEntity, TRtfdClass));
  end
  else if ModelEntity is TInterface then
  begin
    // Interface
    // Ancestor that is in another package and that is not already inserted
    // is added to the diagram.
    FIsAllClasses := True; // for testing
    if (not FIsAllClasses) and Assigned(TInterface(ModelEntity).Ancestor)
      and (TInterface(ModelEntity).Ancestor.Owner <> ModelEntity.Owner) and
      not Assigned(GetBox(TInterface(ModelEntity).Ancestor.Fullname)) then
      FPanel.AddManagedObject(InCreateBox(TInterface(ModelEntity).Ancestor,
        TRtfdInterface));
    if not Assigned(GetBox(ModelEntity.Fullname)) then
      FPanel.AddManagedObject(InCreateBox(ModelEntity, TRtfdInterface));
  end
  else if ModelEntity is TObjekt then
    if not Assigned(GetBox(ModelEntity.Fullname)) then
      FPanel.AddManagedObject(InCreateBox(ModelEntity, TRtfdObject));
end;

// Make arrows between boxes
procedure TRtfdDiagram.ResolveAssociations;
var
  Posi: Integer;
  CBox: TRtfdClass;
  AClass: TClass;
  IBox: TRtfdInterface;
  Attribute: TAttribute;
  OBox: TRtfdObject;
  UBox: TRtfdUnitPackage;
  APackage: TUnitPackage;
  Dep: TUnitDependency;
  MIte: IModelIterator;
  DestBox: TRtfdBox;
  AJavaObject: TComJavaObject;
  Str, Agg, Ass, Generic, Boxname: string;
  AttributeConnected: Boolean;

begin
  FPanel.DeleteNotEditedConnections;
  FPanel.DeleteObjectConnections;
  for var I := 0 to FBoxNames.Count - 1 do
    if (FBoxNames.Objects[I] is TRtfdClass) then
    begin // Class
      CBox := TRtfdClass(FBoxNames.Objects[I]);
      // Ancestor
      if Assigned(TClass(CBox.Entity).Ancestor) then
      begin
        AClass := TClass(CBox.Entity);
        if Assigned(AClass) then
        begin
          DestBox := GetBox(AClass.Ancestor.Fullname);
          if Assigned(DestBox) then
            FPanel.ConnectObjects(CBox, DestBox, asInheritends);
        end;
      end;
      // Implements
      MIte := TClass(CBox.Entity).GetImplements;
      while MIte.HasNext do
      begin
        Str := MIte.Next.Fullname;
        DestBox := GetBox(Str);
        if Assigned(DestBox) then
          FPanel.ConnectObjects(CBox, DestBox, asImplements);
      end;
      Posi := Pos('$', CBox.Entity.Name);
      if Posi > 0 then
      begin
        DestBox := GetBox(Copy(CBox.Entity.Name, 1, Posi - 1));
        if Assigned(DestBox) then
          FPanel.ConnectObjects(DestBox, CBox, asAssociation2);
      end;

      // Attributes associations
      AttributeConnected := False;
      MIte := TClass(CBox.Entity).GetAttributes;
      while MIte.HasNext do
      begin
        AttributeConnected := False;
        Attribute := TAttribute(MIte.Next);
        // Avoid arrows that points to themselves, also associations to ancestor (double arrows)
        if Assigned(Attribute.TypeClassifier) then
          if (Attribute.TypeClassifier = TClass(CBox.Entity).Ancestor) and
            Assigned(GetBox(Attribute.TypeClassifier.Name)) then
            Attribute.Connected := True
          else
          begin
            Str := Attribute.TypeClassifier.Fullname;
            if IsSimpleTypeOrString(Str) then
              Continue;
            Generic := GenericOf(Str);
            if Generic <> '' then
            begin // Vector<E>, Stack<E>, ArrayList<E>,...
              DestBox := GetBox(Generic);
              if Assigned(DestBox) and
                (FPanel.HaveConnection(CBox, DestBox) = -1) and
                (DestBox.Entity.Name = Generic) then
                FPanel.ConnectObjects(CBox, DestBox, asAggregation1);
            end
            else if Pos('[]', Str) > 0 then
            begin // Typ[]
              Agg := WithoutArray(Str);
              DestBox := GetBox(Agg);
              if not Assigned(DestBox) and (Pos('.', Agg) = 0) and
                (CBox.Entity.Package <> '') then
              begin
                Ass := CBox.Entity.Package + '.' + Agg;
                DestBox := GetBox(Ass);
              end;
              if Assigned(DestBox) and
                (FPanel.HaveConnection(CBox, DestBox) = -1) then
                FPanel.ConnectObjects(CBox, DestBox, asAggregation1);
            end
            else
            begin
              Ass := Str;
              DestBox := GetBox(Ass);
              if not Assigned(DestBox) and (Pos('.', Ass) = 0) and
                (CBox.Entity.Package <> '') then
              begin
                Ass := CBox.Entity.Package + '.' + Ass;
                DestBox := GetBox(Ass);
              end;
              if Assigned(DestBox) then
                if FPanel.HaveConnection(CBox, DestBox) = -1 then
                  FPanel.ConnectObjects(CBox, DestBox, asAssociation2)
                else
                begin
                  Posi := FPanel.HaveConnection(DestBox, CBox, asAssociation2);
                  if Posi > -1 then
                    FPanel.SetConnection(Posi, asAssociation3);
                end;
            end;
            if Assigned(DestBox) and (FPanel.HaveConnection(CBox, DestBox) > -1)
              and DestBox.Entity.IsVisible then
            begin
              Attribute.Connected := True;
              AttributeConnected := True;
            end;
          end;
      end;
      if AttributeConnected then
        CBox.RefreshEntities;
    end
    else if (FBoxNames.Objects[I] is TRtfdInterface) then
    begin
      // Interface
      IBox := TRtfdInterface(FBoxNames.Objects[I]);
      // Ancestor
      if Assigned(TInterface(IBox.Entity).Ancestor) then
      begin
        DestBox := GetBox(TInterface(IBox.Entity).Ancestor.Fullname);
        if Assigned(DestBox) then
          FPanel.ConnectObjects(IBox, DestBox, asInheritends);
      end;
    end
    else if (FBoxNames.Objects[I] is TRtfdUnitPackage) then
    begin
      // Unit
      UBox := TRtfdUnitPackage(FBoxNames.Objects[I]);
      APackage := TUnitPackage(UBox.Entity);
      MIte := APackage.GetUnitDependencies;
      while MIte.HasNext do
      begin
        Dep := TUnitDependency(MIte.Next);
        if Dep.Visibility = viPublic then
        begin
          DestBox := GetBox(Dep.MyPackage.Fullname);
          if Assigned(DestBox) then
            FPanel.ConnectObjects(UBox, DestBox, asAssociation2);
        end;
      end;
    end
    else if FBoxNames.Objects[I] is TRtfdObject then
    begin
      // connect Objects to Classes
      OBox := TRtfdObject(FBoxNames.Objects[I]);
      if Assigned(OBox.Entity) then
      begin
        AJavaObject := FComJava.GetObject(OBox.Entity.Name);
        if Assigned(AJavaObject) then
        begin
          Boxname := AJavaObject.ClassRef.GetGenericTyp;
          DestBox := GetBox(Boxname);
          if not Assigned(DestBox) then
            DestBox := GetBox(AJavaObject.ClassRef.Name);
          if Assigned(DestBox) then
            FPanel.ConnectObjects(OBox, DestBox, asInstanceOf);
        end;
      end;
    end;
  FPanel.ShowAll;
end;

procedure TRtfdDiagram.ShowRelationshipAttributesBold;
var
  CBox: TRtfdClass;
  AClass: TClass;
  Attribute: TAttribute;
  MIte: IModelIterator;
  DestBox: TRtfdBox;
  Ass, Agg, Str, Generic: string;
begin
  for var I := 0 to FBoxNames.Count - 1 do
  begin
    if (FBoxNames.Objects[I] is TRtfdClass) then
    begin // Class
      CBox := TRtfdClass(FBoxNames.Objects[I]);
      AClass := TClass(CBox.Entity);
      MIte := AClass.GetAttributes;
      while MIte.HasNext do
      begin
        Attribute := TAttribute(MIte.Next);
        if Assigned(Attribute.TypeClassifier) then
          if (Attribute.TypeClassifier = TClass(CBox.Entity).Ancestor) and
            Assigned(GetBox(Attribute.TypeClassifier.Name)) then
            Attribute.Connected := True
          else
          begin
            Str := Attribute.TypeClassifier.Fullname;
            if IsSimpleTypeOrString(Str) then
              Continue;
            Generic := GenericOf(Str);
            if Generic <> '' then // Vector<E>, Stack<E>, ArrayList<E>,...
              DestBox := GetBox(Generic)
            else if Pos('[]', Str) > 0 then
            begin // Typ[]
              Agg := WithoutArray(Str);
              DestBox := GetBox(Agg);
              if not Assigned(DestBox) and (Pos('.', Agg) = 0) and
                (CBox.Entity.Package <> '') then
              begin
                Ass := CBox.Entity.Package + '.' + Agg;
                DestBox := GetBox(Ass);
              end;
            end
            else
            begin
              Ass := Str;
              DestBox := GetBox(Ass);
              if not Assigned(DestBox) and (Pos('.', Ass) = 0) and
                (CBox.Entity.Package <> '') then
              begin
                Ass := CBox.Entity.Package + '.' + Ass;
                DestBox := GetBox(Ass);
              end;
            end;
            if Assigned(DestBox) and (FPanel.HaveConnection(CBox, DestBox) > -1)
              and DestBox.Entity.IsVisible then
              Attribute.Connected := True;
          end;
      end;
    end;
  end;
end;

// make arrows between Objects
procedure TRtfdDiagram.ResolveObjectAssociations;
var
  Str1, Str2: string;
  SL1, SL_V: TStringList;
  OBox: TRtfdObject;
  DestBox: TRtfdBox;
  AModelObject: TObjekt;
  AJavaObject: TComJavaObject;
begin
  try
    FPanel.DeleteObjectConnections;
    for var I := 0 to FBoxNames.Count - 1 do
    begin
      if Assigned(FBoxNames.Objects[I]) and
        (FBoxNames.Objects[I] is TRtfdObject) then
      begin // reconnect Objects
        OBox := TRtfdObject(FBoxNames.Objects[I]);
        if Assigned(OBox.Entity) and (OBox.Entity is TObjekt) then
        begin
          AModelObject := TObjekt(OBox.Entity);
          AJavaObject := FComJava.GetObject(OBox.Entity.Name);
          if Assigned(AJavaObject) then
          begin
            SL_V := AJavaObject.GetObjectAttributeValues(False);
            for var J := 0 to SL_V.Count - 1 do
            begin
              Str1 := SL_V[J];
              if Pos('{', Str1) + Pos('[', Str1) = 1 then // array or ArrayList
                Str1 := Copy(Str1, 2, Length(Str1) - 2);
              SL1 := Split(',', Str1);
              for var K := 0 to SL1.Count - 1 do
              begin
                Str2 := Trim(SL1[K]);
                if Str2 <> 'null' then
                begin
                  DestBox := GetBox(Str2);
                  if Assigned(DestBox) then
                    FPanel.ConnectObjects(OBox, DestBox, asAssociation2);
                end;
              end;
              FreeAndNil(SL1);
            end;
            AModelObject.RefreshEntities;
            FreeAndNil(SL_V);
          end
          else
            FConfiguration.Log
              ('TRtfdDiagram.ResolveObjectAssociations A: AJavaObject not assigned');
        end
        else
          FConfiguration.Log
            ('TRtfdDiagram.ResolveObjectAssociations B: not assigned OBox.Entity');
      end;
    end;
  except
    on E: Exception do
      FConfiguration.Log('TRtfdDiagram.ResolveObjectAssociations C: ', E);
  end;
  FPanel.ShowAll;
end;

procedure TRtfdDiagram.SetPackage(const Value: TAbstractPackage);
begin
  if Assigned(FPackage) and (FPackage is TUnitPackage) then
    FPackage.RemoveListener(IAfterUnitPackageListener(Self));
  inherited SetPackage(Value);
  if Assigned(FPackage) then
  begin
    if (FPackage is TUnitPackage) then
      FPackage.AddListener(IAfterUnitPackageListener(Self));
    if (FPackage is TLogicPackage) then
      FPackage.AddListener(IAfterUnitPackageListener(Self));
  end;
  if Assigned(FFrame.ScrollBox) and Assigned(FConfiguration) then
  begin
    FFrame.ScrollBox.HorzScrollBar.Position := 0;
    FFrame.ScrollBox.VertScrollBar.Position := 0;
  end;
end;

procedure TRtfdDiagram.UnitPackageAfterAddChild(Sender, NewChild: TModelEntity);
begin
  if (NewChild is TClass) or (NewChild is TInterface) or (NewChild is TObjekt)
  then
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

function TRtfdDiagram.JavaInsteadClass(StringList: TStringList): TStringList;
var
  I: Integer;
  AFile, Ext: string;
begin
  I := 0;
  while I < StringList.Count do
  begin
    Ext := ExtractFileExt(StringList[I]);
    if Ext = '.class' then
    begin
      AFile := ChangeFileExt(StringList[I], '.java');
      if StringList.IndexOf(AFile) > 0 then
        StringList.Delete(I)
      else if FileExists(AFile) then
        StringList[I] := AFile
      else
        Inc(I);
    end
    else
      Inc(I);
  end;
  Result := StringList;
end;

procedure TRtfdDiagram.StoreDiagram(Filename: string);
var
  Ini: TMemIniFile;
  Int1, Posi, Comments: Integer;
  Box: TRtfdBox;
  Section, CName, FName, Str1, Path: string;
  Connections: TList;
  Conn: TConnection;
  Values: TStrings;
  AJavaObject: TComJavaObject;
  Files: TStringList;

begin
  Values := TStringList.Create;
  if FileExists(Filename) then
  begin
    Ini := TMemIniFile.Create(Filename);
    Ini.ReadSectionValues('Window', Values);
    Ini.Free;
  end;
  DeleteFile(Filename);
  Ini := nil;
  Files := nil;
  try
    Ini := TMemIniFile.Create(Filename, TEncoding.UTF8);
    Files := TStringList.Create;
    Files.Duplicates := dupIgnore;
    Files.Sorted := True;
    try
      // Files
      Path := ExtractFilePath(Filename);
      if IsUNC(Path) then
        Path := '';
      Int1 := 0;
      Files.AddStrings(FFrame.Diagram.Model.ModelRoot.Files);
      Files := JavaInsteadClass(Files);
      for var I := 0 to Files.Count - 1 do
      begin
        FName := FConfiguration.RemovePortableDrive(Files[I], Path);
        Ini.WriteString('Files', 'File' + IntToStr(Int1), FName);
        Inc(Int1);
      end;

      // Boxes
      Comments := 0;
      for var I := 0 to FBoxNames.Count - 1 do
        if FBoxNames.Objects[I] is TRtfdObject then
        begin
          // Objects
          Box := TRtfdObject(FBoxNames.Objects[I]);
          Section := 'Object: ' + Package.Fullname + ' - ' + Box.Entity.Fullname;
          Ini.WriteInteger(Section, 'X', PPIUnScale(Box.Left));
          Ini.WriteInteger(Section, 'Y', PPIUnScale(Box.Top));
          AJavaObject := FComJava.GetObject(Box.Entity.Name);
          Ini.WriteString(Section, 'Name', Box.Entity.Name);
          if Assigned(AJavaObject) then
            Ini.WriteString(Section, 'Typ', AJavaObject.ClassRef.ImportTyp)
          else
            Ini.WriteString(Section, 'Typ', 'unknown');
        end
        else if FBoxNames.Objects[I] is TRtfdCommentBox then
        begin
          Box := TRtfdBox(FBoxNames.Objects[I]);
          Section := Box.Entity.Fullname;
          Ini.WriteInteger(Section, 'X', PPIUnScale(Box.Left));
          Ini.WriteInteger(Section, 'Y', PPIUnScale(Box.Top));
          Ini.WriteInteger(Section, 'W', PPIUnScale(Box.Width));
          Ini.WriteInteger(Section, 'H', PPIUnScale(Box.Height));
          Str1 := TRtfdCommentBox(Box).TrMemo.Text;
          Ini.WriteString(Section, 'Comment', '_;_' + ReplaceStr(Str1,
            #13#10, '_;_'));
        end
        else
        begin
          // Class, Stereotype, Interface
          Box := TRtfdBox(FBoxNames.Objects[I]);
          Section := 'Box: ' + Package.Fullname + ' - ' + Box.Entity.Fullname;
          Ini.WriteInteger(Section, 'X', PPIUnScale(Box.Left));
          Ini.WriteInteger(Section, 'Y', PPIUnScale(Box.Top));
          Ini.WriteInteger(Section, 'MinVis', Integer(Box.MinVisibility));
          Ini.WriteInteger(Section, 'ShowParameter', Box.ShowParameter);
          Ini.WriteInteger(Section, 'SortOrder', Box.SortOrder);
          Ini.WriteInteger(Section, 'ShowIcons', Box.ShowIcons);
          if Box.TypeBinding <> '' then
            Ini.WriteString(Section, 'TypeBinding', Box.TypeBinding);
        end;

      // Diagram stuff
      Section := 'Diagram';
      Ini.WriteInteger(Section, 'Comments', Comments);
      Ini.WriteInteger(Section, 'OffsetX', FFrame.ScrollBox.VertScrollBar.Position);
      Ini.WriteInteger(Section, 'OffsetY', FFrame.ScrollBox.HorzScrollBar.Position);

      Ini.WriteInteger(Section, 'Visibility', Integer(VisibilityFilter));
      Ini.WriteInteger(Section, 'ShowParameter', ShowParameter);
      Ini.WriteInteger(Section, 'SortOrder', SortOrder);
      Ini.WriteInteger(Section, 'ShowIcons', ShowIcons);
      Ini.WriteInteger(Section, 'ShowConnections', ShowConnections);
      Ini.WriteString(Section, 'Fontname', Font.Name);
      Ini.WriteInteger(Section, 'Fontsize', PPIUnScale(Font.Size));
      Ini.WriteBool(Section, 'ShowObjectDiagram', ShowObjectDiagram);

      // Connections
      Section := 'Connections';
      Connections := FPanel.GetConnections;
      for var I := 0 to Connections.Count - 1 do
      begin
        Conn := TConnection(Connections[I]);
        with Conn do
          CName := TRtfdBox(FromControl).Entity.Fullname + '#' +
            TRtfdBox(ToControl).Entity.Fullname + '#' +
            ArrowStyleToString(ArrowStyle) + '#' + HideCrLf(MultiplicityA) + '#'
            + Relation + '#' + HideCrLf(MultiplicityB) + '#' +
            IntToStr(RecursivCorner) + '#' + BoolToStr(IsTurned) + '#' +
            BoolToStr(IsEdited) + '#' + HideCrLf(RoleA) + '#' + HideCrLf(RoleB)
            + '#' + BoolToStr(ReadingOrderA) + '#' + BoolToStr(ReadingOrderB);
        Ini.WriteString(Section, 'V' + IntToStr(I), CName);
      end;
      Connections.Free;

      // FInteractive
      Section := 'FInteractive';
      for var I := 0 to FInteractive.InteractiveEditor.Lines.Count - 1 do
        Ini.WriteString(Section, 'I' + IntToStr(I),
          FInteractive.InteractiveEditor.Lines[I]);

      // Window
      for var I := 0 to Values.Count - 1 do
      begin
        Section := Values[I];
        Posi := Pos('=', Section);
        Ini.WriteString('Window', Copy(Section, 1, Posi - 1),
          Copy(Section, Posi + 1, Length(Section)));
      end;
      Ini.UpdateFile;
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    Ini.Free;
    Files.Free;
  end;

  Values.Free;
  FPanel.IsModified := False;
end;

procedure TRtfdDiagram.FetchDiagram(Filename: string);
var
  Ini: TMemIniFile;
  Int, Num, Posi, CountObjects: Integer;
  Box, Box1, Box2: TRtfdBox;
  Section, CName, AFile, B1Name, B2Name, Path: string;
  FilesPre, FilesPost, Sections, StringList: TStringList;
  AlleBoxen: TList;

  UnitPackage: TUnitPackage;
  TheClassname: string;
  TheObjectname: string;
  AModelObject: TObjekt;

  MyObject: TComJavaObject;
  Attributes: TConnectionAttributes;
  AClass: TClass;
  AClassifier: TClassifier;

  BoxShowParameter, BoxSortorder, BoxShowIcons: Integer;
  BoxTypeBinding: string;
  CommentBox: TRtfdCommentBox;
  VisibilityFilterAsInteger: Integer;
  ShadowWidth: Integer;
begin
  Filename := ExpandFileName(Filename);
  Ini := TMemIniFile.Create(Filename);
  FilesPre := TStringList.Create;
  FilesPost := TStringList.Create;
  Attributes := TConnectionAttributes.Create;
  try
    Section := 'Diagram';
    if not Ini.SectionExists(Section) and Assigned(Package) then
      Section := 'Diagram: ' + Package.Fullname; // old format
    FFrame.ScrollBox.VertScrollBar.Position := Ini.ReadInteger(Section, 'OffsetX',
      FFrame.ScrollBox.VertScrollBar.Position);
    FFrame.ScrollBox.HorzScrollBar.Position := Ini.ReadInteger(Section, 'OffsetY',
      FFrame.ScrollBox.HorzScrollBar.Position);

    ShowConnections := Ini.ReadInteger(Section, 'ShowConnections', 0);
    VisibilityFilterAsInteger := Ini.ReadInteger(Section, 'Visibility', 0);
    VisibilityFilter := TVisibility(VisibilityFilterAsInteger);
    ShowParameter := Ini.ReadInteger(Section, 'ShowParameter', ShowParameter);
    SortOrder := Ini.ReadInteger(Section, 'SortOrder', SortOrder);
    ShowIcons := Ini.ReadInteger(Section, 'ShowIcons', ShowIcons);
    ShowObjectDiagram := Ini.ReadBool(Section, 'ShowObjectDiagram', False);
    Font.Name := Ini.ReadString(Section, 'Fontname', 'Segoe UI');
    if Font.Name = 'MS Sans Serif' then
      Font.Name := 'Segoe UI';
    Font.Size := PPIScale(Ini.ReadInteger(Section, 'Fontsize', 11));
    SetFont(Font);

    // read files
    Path := ExtractFilePath(Filename);
    if IsUNC(Path) then
      Path := ''
    else
      SetCurrentDir(Path); // due to relativ paths
    Int := 0;
    AFile := Ini.ReadString('Files', 'File' + IntToStr(Int), '');
    while AFile <> '' do
    begin
      AFile := ExpandFileName(FConfiguration.AddPortableDrive(AFile, Path));
      if not FileExistsCaseSensitive(AFile) then
        AFile := ExtractFilePath(Filename) + ExtractFileName(AFile);
      if FileExistsCaseSensitive(AFile) and (FilesPre.IndexOf(AFile) = -1) then
        FilesPre.Add(AFile);
      Inc(Int);
      AFile := Ini.ReadString('Files', 'File' + IntToStr(Int), '');
    end;
    FilesPre := JavaInsteadClass(FilesPre);
    FUMLForm.MainModul.LoadProject(FilesPre);

    // read classes
    for var I := FBoxNames.Count - 1 downto 0 do
    begin
      Box := TRtfdBox(FBoxNames.Objects[I]);
      if (FBoxNames.Objects[I] is TRtfdClass) or
        (FBoxNames.Objects[I] is TRtfdInterface) then
      begin
        Section := 'Box: ' + Package.Fullname + ' - ' + Box.Entity.Fullname;
        if not Ini.SectionExists(Section) then
          Section := 'Box: ' + ' - ' + Box.Entity.Fullname;
        if Ini.SectionExists(Section) then
        begin
          Box.Left := PPIScale(Ini.ReadInteger(Section, 'X', Box.Left));
          Box.Top := PPIScale(Ini.ReadInteger(Section, 'Y', Box.Top));
          Box.MinVisibility := TVisibility(Ini.ReadInteger(Section, 'MinVis',
            VisibilityFilterAsInteger));
          BoxShowParameter := Ini.ReadInteger(Section, 'ShowParameter',
            ShowParameter);
          BoxSortorder := Ini.ReadInteger(Section, 'SortOrder', SortOrder);
          BoxShowIcons := Ini.ReadInteger(Section, 'ShowIcons', ShowIcons);
          BoxTypeBinding := Ini.ReadString(Section, 'TypeBinding', '');
          ShadowWidth := FConfiguration.ShadowWidth;
          Box.SetParameters(BoxShowParameter, BoxSortorder, BoxShowIcons,
            ShadowWidth, Font, BoxTypeBinding);
          // reduce to necessary files
          Num := 0;
          while Num < FilesPre.Count do
          begin
            Path := ReplaceStr(WithoutGeneric(Box.Entity.Fullname), '.', '\');
            if EndsWith(FilesPre[Num], Path + '.java') or
              EndsWith(FilesPre[Num], Path + '.class') then
            begin
              FilesPost.Add(FilesPre[Num]);
              FilesPre.Delete(Num);
              Num := FilesPre.Count;
            end
            else
              Inc(Num);
          end;
        end
        else
          // if not ShowAll then
          FPanel.FindManagedControl(Box).Selected := True;
      end;
    end;
    DeleteSelectedControls(nil);
    FUMLForm.MainModul.Model.ModelRoot.Files.Assign(FilesPost);

    // read Objects
    CountObjects := 0;
    Sections := TStringList.Create;
    Ini.ReadSections(Sections);
    for var I := 0 to Sections.Count - 1 do
    begin
      if Pos('Object', Sections[I]) > 0 then
      begin
        Section := Sections[I];
        UnitPackage := Model.ModelRoot.FindUnitPackage('Default');
        // TODO nicht bei package
        if not Assigned(UnitPackage) then
          UnitPackage := Model.ModelRoot.AddUnit('Default');
        FComJava.Transfer(UnitPackage.ClassImports, UnitPackage.FullImports,
          GetSourcePath);
        TheClassname := Ini.ReadString(Section, 'Typ', '');
        TheObjectname := Ini.ReadString(Section, 'Name', '');
        MyObject := FComJava.GetObject(TheObjectname);

        AClass := nil;
        if Assigned(MyObject) then
        begin
          Num := FBoxNames.IndexOf(TheClassname);
          if Num >= 0 then
            AClass := TClass(TRtfdClass(FBoxNames.Objects[Num]).Entity);
        end;
        if not Assigned(AClass) then
        begin
          AClassifier := UnitPackage.FindClassifier(TheClassname, TClass, True);
          if Assigned(AClassifier) then
            AClass := TClass(AClassifier)
          else
          begin
            AClass := TClass.Create(nil);
            AClass.Name := TheClassname;
            UnitPackage.AddClass(AClass);
          end;
        end;

        if Assigned(MyObject) then
        begin
          AModelObject := UnitPackage.AddObject(TheObjectname, AClass);
          ShowAttributes(MyObject, AModelObject);

          Num := FBoxNames.IndexOf(TheObjectname);
          if Num = -1 then
          begin
            AddBox(AModelObject);
            Num := FBoxNames.IndexOf(TheObjectname);
          end;

          if Num > -1 then
          begin
            Box := TRtfdBox(FBoxNames.Objects[Num]);
            Box.Left := PPIScale(Ini.ReadInteger(Section, 'X', Box.Left));
            Box.Top := PPIScale(Ini.ReadInteger(Section, 'Y', Box.Top));
            Box.Font.Assign(Font);
          end;
          Inc(CountObjects);
        end;
      end;
    end;
    FPanel.DeleteConnections;

    // read Comments
    for var I := 0 to Sections.Count - 1 do
    begin
      if Pos('Comment', Sections[I]) = 1 then
      begin
        Section := Sections[I];
        Num := FBoxNames.IndexOf(Section);
        if Num = -1 then
        begin
          CommentBox := TRtfdCommentBox.Create(FPanel, Section, FFrame, viPublic,
            HANDLESIZE);
          FBoxNames.AddObject(Section, CommentBox);
          FPanel.AddManagedObject(CommentBox);
          Num := FBoxNames.IndexOf(Section);
        end;

        if Num > -1 then
        begin
          Box := TRtfdBox(FBoxNames.Objects[Num]);
          Box.Left := PPIScale(Ini.ReadInteger(Section, 'X', Box.Left));
          Box.Top := PPIScale(Ini.ReadInteger(Section, 'Y', Box.Top));
          Box.Width := PPIScale(Ini.ReadInteger(Section, 'W', Box.Width));
          Box.Height := PPIScale(Ini.ReadInteger(Section, 'H', Box.Height));
          Box.Font.Assign(Font);
          Section := Ini.ReadString(Section, 'Comment', '');
          if Copy(Section, 1, 3) = '_;_' then
            Delete(Section, 1, 3);
          TRtfdCommentBox(Box).TrMemo.Text :=
            ReplaceStr(Section, '_;_', #13#10);
        end;
      end;
    end;
    FreeAndNil(Sections);
    FPanel.DeleteConnections;
    AlleBoxen := FPanel.GetManagedObjects;

    Section := 'Connections';
    Int := 0;
    repeat
      CName := Ini.ReadString(Section, 'V' + IntToStr(Int), '');
      if CName <> '' then
      begin
        Box1 := nil;
        Box2 := nil;
        StringList := Split('#', CName);
        B1Name := Trim(StringList[0]);
        B2Name := Trim(StringList[1]);
        Attributes.ArrowStyle := StringToArrowStyle(StringList[2]);
        if StringList.Count > 7 then
        begin // new format
          Attributes.MultiplicityA := UnHideCrLf(StringList[3]);
          Attributes.Relation := StringList[4];
          Attributes.MultiplicityB := UnHideCrLf(StringList[5]);
          Attributes.RecursivCorner := StrToInt(StringList[6]);
          Attributes.IsTurned := StrToBool(StringList[7]);
        end;
        if (StringList.Count > 8) and (StringList[8] <> '') then
          Attributes.IsEdited := StrToBool(StringList[8])
        else
          Attributes.IsEdited := False;
        if (StringList.Count > 12) and (StringList[12] <> '') then
        begin
          Attributes.RoleA := UnHideCrLf(StringList[9]);
          Attributes.RoleB := UnHideCrLf(StringList[10]);
          Attributes.ReadingOrderA := StrToBool(StringList[11]);
          Attributes.ReadingOrderB := StrToBool(StringList[12]);
        end;

        for var J := 0 to AlleBoxen.Count - 1 do
        begin
          Box := TRtfdBox(AlleBoxen[J]);
          if Box.Entity.Fullname = B1Name then
            Box1 := Box;
          if Box.Entity.Fullname = B2Name then
            Box2 := Box;
        end;
        FPanel.ConnectObjects(Box1, Box2, Attributes);
        FreeAndNil(StringList);
      end;
      Inc(Int);
    until CName = '';
    FreeAndNil(AlleBoxen);
    if FConfiguration.RelationshipAttributesBold then
      ShowRelationshipAttributesBold;

    FPanel.SetConnections(ShowConnections);
    if CountObjects = 0 then
      SetShowObjectDiagram(False)
    else
      SetShowObjectDiagram(ShowObjectDiagram);
    UpdateAllObjects;

    Section := 'Interactive';
    if Assigned(FInteractive) and Assigned(FInteractive.InteractiveEditor) and
      Assigned(FInteractive.InteractiveEditor.Lines) then
    begin
      FInteractive.InteractiveEditor.BeginUpdate;
      FInteractive.InteractiveEditor.Lines.Clear;
      StringList := TStringList.Create;
      Ini.ReadSectionValues(Section, StringList);
      for var I := 0 to StringList.Count - 1 do
      begin
        CName := StringList[I];
        Posi := Pos('=', CName);
        Delete(CName, 1, Posi);
        Posi := Pos('<#>', CName); // old format
        if Posi > 0 then
          CName := Copy(CName, 1, Posi - 1);
        FInteractive.InteractiveEditor.Lines.Add(CName);
      end;
      FreeAndNil(StringList);
      FInteractive.InteractiveEditor.EndUpdate;
    end;
  finally
    FreeAndNil(Ini);
    FreeAndNil(FilesPre);
    FreeAndNil(FilesPost);
    FreeAndNil(Attributes);
  end;
  FPanel.RecalcSize;
  FPanel.IsModified := False;
  FPanel.ShowAll;
  if FPanel.CanFocus then
    FPanel.SetFocus;
end;

procedure TRtfdDiagram.DoLayout;
begin
  if FBoxNames.Count = 0 then
    Exit;

  FPanel.Hide;
  var
  Layout := TSugiyamaLayout.Create(FPanel.GetManagedObjects,
    FPanel.GetConnections);
  try
    Layout.Execute;
  finally
    FPanel.Show;
    Layout.Free;
  end;
  FPanel.IsModified := True;
  FPanel.RecalcSize;
  FPanel.ShowAll;
end;

function TRtfdDiagram.GetBox(Typ: string): TRtfdBox;
begin
  Typ := WithoutGeneric(Typ);
  var
  Int := 0;
  while Int < FBoxNames.Count do
  begin
    var
    Str := WithoutGeneric(FBoxNames[Int]);
    if Typ = Str then
      Break;
    Inc(Int);
  end;
  if Int = FBoxNames.Count then
    Result := nil
  else
    Result := TRtfdBox(FBoxNames.Objects[Int]);
end;

procedure TRtfdDiagram.SetVisibilityFilter(const Value: TVisibility);
var
  ABox: TRtfdBox;
  List: TList;
begin
  if FPanel.CountSelectedControls > 0 then
    List := FPanel.GetSelectedControls
  else
    List := FPanel.GetManagedObjects;
  FPanel.Hide;
  try
    for var I := 0 to List.Count - 1 do
      if TObject(List[I]) is TRtfdBox then
      begin
        ABox := TRtfdBox(TObject(List[I]));
        if ABox.MinVisibility <> Value then
        begin
          ABox.MinVisibility := Value;
          FPanel.IsModified := True;
        end;
      end;
  finally
    List.Free;
  end;
  FPanel.RecalcSize;
  FPanel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetShowView(Value: Integer);
var
  Objs: Integer;
  List: TList;
  ABox: TRtfdBox;
begin
  FPanel.Hide;
  List := FPanel.GetManagedObjects;
  try
    Objs := 0;
    for var I := 0 to List.Count - 1 do
      if TObject(List[I]) is TRtfdObject then
        Inc(Objs);
    if (Objs = 0) and (Value = 1) then
      Value := 2;
    for var I := 0 to List.Count - 1 do
    begin
      ABox := TRtfdBox(TObject(List[I]));
      case Value of
        0:
          ABox.MinVisibility := TVisibility(0);
        1:
          if ABox is TRtfdClass then
            ABox.MinVisibility := TVisibility(4)
          else
            ABox.MinVisibility := TVisibility(0);
        2:
          ABox.MinVisibility := TVisibility(4);
      end;
    end;
  finally
    List.Free;
  end;
  FPanel.IsModified := True;
  FPanel.RecalcSize;
  FPanel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetShowParameter(const Value: Integer);
var
  List: TList;
  ABox: TRtfdBox;
begin
  if FPanel.CountSelectedControls > 0 then
    List := FPanel.GetSelectedControls
  else
    List := FPanel.GetManagedObjects;
  FPanel.Hide;
  try
    for var I := 0 to List.Count - 1 do
    begin
      ABox := TRtfdBox(TObject(List[I]));
      if ABox.ShowParameter <> Value then
      begin
        ABox.ShowParameter := Value;
        FPanel.IsModified := True;
      end;
    end;
  finally
    List.Free;
  end;
  FPanel.RecalcSize;
  FPanel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetSortOrder(const Value: Integer);
var
  List: TList;
  ABox: TRtfdBox;
begin
  FPanel.Hide;
  if FPanel.CountSelectedControls > 0 then
    List := FPanel.GetSelectedControls
  else
    List := FPanel.GetManagedObjects;
  try
    for var I := 0 to List.Count - 1 do
    begin
      ABox := TRtfdBox(List[I]);
      if ABox.SortOrder <> Value then
      begin
        ABox.SortOrder := Value;
        FPanel.IsModified := True;
      end;
    end;
  finally
    List.Free;
  end;
  FPanel.RecalcSize;
  FPanel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetShowIcons(const Value: Integer);
var
  List: TList;
  ABox: TRtfdBox;
begin
  FPanel.Hide;
  if FPanel.CountSelectedControls > 0 then
    List := FPanel.GetSelectedControls
  else
    List := FPanel.GetManagedObjects;
  try
    for var I := 0 to List.Count - 1 do
    begin
      ABox := TRtfdBox(List[I]);
      if (ABox is TRtfdObject) and FConfiguration.ObjectsWithoutVisibility then
        Continue;
      if ABox.ShowIcons <> Value then
      begin
        ABox.ShowIcons := Value;
        FPanel.IsModified := True;
      end;
    end;
  finally
    List.Free;
  end;
  FPanel.RecalcSize;
  FPanel.Show;
  inherited;
end;

procedure TRtfdDiagram.SetFont(const AFont: TFont);
var
  List: TList;
  ABox: TRtfdBox;
begin
  inherited;
  List := FPanel.GetManagedObjects;
  try
    for var I := 0 to List.Count - 1 do
    begin
      ABox := TRtfdBox(List[I]);
      if Assigned(ABox) and Assigned(ABox.Font) then
        ABox.Font.Assign(AFont);
    end;
  finally
    List.Free;
  end;
  FPanel.Font.Assign(AFont);
end;

function TRtfdDiagram.GetFont: TFont;
var
  Control: TControl;
begin
  if FPanel.HasSelectedControls then
    Control := FPanel.GetFirstSelected
  else
    Control := nil;
  if Assigned(Control) then
    Result := TRtfdBox(Control).Font
  else
    Result := inherited GetFont;
end;

procedure TRtfdDiagram.GetDiagramSize(var Width, Height: Integer);
begin
  FPanel.GetDiagramSize(Width, Height);
end;

// Returns list with str = 'x1,y1,x2,y2', obj = modelentity
function TRtfdDiagram.GetClickAreas: TStringList;
begin
  Result := TStringList.Create;
  for var I := 0 to FBoxNames.Count - 1 do
  begin
    var Box := TRtfdBox(FBoxNames.Objects[I]);
    var Size := IntToStr(Box.Left) + ',' + IntToStr(Box.Top) + ',' +
      IntToStr(Box.Left + Box.Width) + ',' + IntToStr(Box.Top + Box.Height);
    Result.AddObject(Size, Box.Entity);
  end;
end;

procedure TRtfdDiagram.ClassEditSelectedDiagramElements(Sender: TObject);
var
  Pathname, APackage: string;
  Form: TFEditForm;
  ABox: TRtfdBox;
  AControl: TControl;
begin
  AControl := TControl(Sender);
  ABox := TRtfdBox(AControl);
  if ((AControl is TRtfdClass) or (AControl is TRtfdInterface)) and
    Assigned(GetBox(ABox.Entity.Fullname)) then
  begin
    Pathname := ChangeFileExt(ABox.GetPathname, '.java');
    if (Length(Pathname) > 0) and (Pos(FConfiguration.TempDir, Pathname) = 0)
      and FileExists(Pathname) then
    begin
      Form := TFEditForm(FJava.GetTDIWindowType(Pathname, '%E%'));
      if Assigned(Form) and Form.Modified then
        FJava.DoSave(Form, False);
      // Mr. Ehrlich: Problem implementing a class derived from an abstract class
      if not MyJavaCommands.HasValidClass(Pathname) then
      begin
        APackage := ABox.Entity.Package;
        if Assigned(Form) then
          MyJavaCommands.CompileForm(Form)
        else
          MyJavaCommands.Compile(Pathname, APackage);
        FUMLForm.Refresh;
      end;
      if Assigned(FJava.ActiveTDIChild) and (FJava.ActiveTDIChild.FormTag = 2)
      then
        FJava.PrepareClassEdit(Pathname, 'Refresh',
          TFUMLForm(FJava.ActiveTDIChild));
    end;
  end;
  FPanel.ClearSelection;
end;

procedure TRtfdDiagram.ClassEditSelectedDiagramElements;
begin
  var
  Control := FPanel.GetFirstSelected;
  if not Assigned(Control) then
    Control := FPanel.GetFirstManaged;
  if Assigned(Control) then
    ClassEditSelectedDiagramElements(Control);
end;

procedure TRtfdDiagram.SourceEditSelectedDiagramElementsControl
  (Control: TControl);
begin
  FPanel.ClearSelection;
  if Assigned(Control) and (Control is TRtfdBox) then
  begin
    var
    Filename := ChangeFileExt(TRtfdBox(Control).GetPathname, '.java');
    if FileExists(Filename) then
      FJava.SwitchWindowWithSearch(Filename);
  end;
end;

procedure TRtfdDiagram.DeleteSelectedControls(Sender: TObject);
var
  AControl: TControl;
  ObjectList: TObjectList;
  Box: TRtfdBox;
  APackage: TUnitPackage;
  Kidx: Integer;
  Key, AClassname: string;
  AObject: TObject;

  function CountClassesWith(const AClassname: string): Integer;
  var
    AControl: TControl;
    List: TList;
    Box: TRtfdBox;
    Name: string;
  begin
    Result := 0;
    List := FPanel.GetManagedObjects;
    for var I := 0 to List.Count - 1 do
    begin
      AControl := TControl(List[I]);
      if AControl is TRtfdBox then
      begin
        Box := TRtfdBox(AControl);
        if (AControl is TRtfdClass) or (AControl is TRtfdInterface) then
        begin
          Name := Box.Entity.Name;
          Delete(Name, 1, LastDelimiter('.', Name));
          // if Pos(AClassname, s) = 1 then inc(Result)
          if AClassname = Name then
            Inc(Result);
        end;
      end;
    end;
    List.Free;
  end;

begin
  ObjectList := FPanel.GetSelectedControls;
  try
    for var I := 0 to ObjectList.Count - 1 do
    begin
      AControl := TControl(ObjectList[I]);
      if AControl is TRtfdBox then
      begin
        Box := TRtfdBox(AControl);
        if (AControl is TRtfdClass) or (AControl is TRtfdInterface) then
        begin
          if TClassifier(Box.Entity).Visibility = viPublic then
          begin
            Key := Box.GetPathname;
            Kidx := FFrame.Diagram.Model.ModelRoot.Files.IndexOf(Key);
            AClassname := ChangeFileExt(ExtractFileName(Key), '');
            if (Kidx > -1) and (CountClassesWith(AClassname) <= 1) then
              FFrame.Diagram.Model.ModelRoot.Files.Delete(Kidx);
          end;

          Key := Box.Entity.Fullname;
          Kidx := FBoxNames.IndexOf(Key);
          if Kidx > -1 then
          begin
            if Assigned(FBoxNames.Objects[Kidx]) then
            begin
              AObject := FBoxNames.Objects[Kidx];
              FreeAndNil(AObject);
            end;
            FBoxNames.Delete(Kidx);
          end;
        end
        else if (AControl is TRtfdObject) then
        begin
          Key := TRtfdObject(AControl).Entity.Name;
          ShowObjectDeleted('Actor', Key);
          Kidx := FBoxNames.IndexOf(Key);
          if Kidx > -1 then
          begin
            if Assigned(FBoxNames.Objects[Kidx]) then
            begin
              AObject := FBoxNames.Objects[Kidx];
              FreeAndNil(AObject);
            end;
            FBoxNames.Delete(Kidx);
          end;
          FInteractive.Executer.DelVariable(Key);
          try
            FComJava.DeleteObject(Key); // java-level
          except
            on E: Exception do
              FConfiguration.Log('TRtfdDiagram.DeleteSelectedControls.', E);
          end;
          APackage := Model.ModelRoot.FindUnitPackage('Default');
          APackage.DeleteObject(Key); // diagram-level
        end
        else if (AControl is TRtfdCommentBox) then
        begin
          Key := TRtfdCommentBox(AControl).Entity.Name;
          Kidx := FBoxNames.IndexOf(Key);
          if Kidx > -1 then
          begin
            if Assigned(FBoxNames.Objects[Kidx]) then
            begin
              AObject := FBoxNames.Objects[Kidx];
              FreeAndNil(AObject);
            end;
            FBoxNames.Delete(Kidx);
          end;
        end;
      end;
    end;
  finally
    FPanel.DeleteSelectedControls;
    FreeAndNil(ObjectList);
  end;
  ResolveObjectAssociations; // nötig?
end;

procedure TRtfdDiagram.DeleteSelectedControlsAndRefresh;
begin
  LockFormUpdate(FUMLForm);
  DeleteSelectedControls(nil);
  FUMLForm.Refresh;
  UnlockFormUpdate(FUMLForm);
end;

procedure TRtfdDiagram.UnSelectAllElements;
begin
  if Assigned(FPanel) then
    FPanel.ClearSelection;
end;

procedure TRtfdDiagram.CurrentEntityChanged;
begin
  inherited;
  var
  ModelEntity := CurrentEntity;
  while Assigned(ModelEntity) and (not(ModelEntity is TAbstractPackage)) do
    ModelEntity := ModelEntity.Owner;
  if Assigned(ModelEntity) and (ModelEntity <> Package) then
    Package := TAbstractPackage(ModelEntity);
  if (CurrentEntity is TClass) or (CurrentEntity is TInterface) then
    ScreenCenterEntity(CurrentEntity);
end;

function TRtfdDiagram.GetSelectedRect: TRect;
begin
  Result := FPanel.GetSelectedRect;
end;

procedure TRtfdDiagram.ScreenCenterEntity(E: TModelEntity);
begin
  for var I := 0 to FBoxNames.Count - 1 do
    if TRtfdBox(FBoxNames.Objects[I]).Entity = E then
    begin
      var
      Box := TRtfdBox(FBoxNames.Objects[I]);
      FFrame.ScrollBox.ScrollInView(Box);
      Break;
    end;
end;

procedure TRtfdDiagram.SetShowObjectDiagram(const Value: Boolean);
begin
  FUMLForm.TBObjectDiagram.Down := Value;
  inherited SetShowObjectDiagram(Value);
  FPanel.SetShowObjectDiagram(Value);
end;

procedure TRtfdDiagram.RefreshDiagram;
begin
  if not PanelIsLocked then
    for var I := 0 to FBoxNames.Count - 1 do
      TRtfdBox(FBoxNames.Objects[I]).RefreshEntities;
  FPanel.RecalcSize;
  FPanel.ShowAll;
  FPanel.IsModified := True;
end;

function TRtfdDiagram.HasAInvalidClass: Boolean;
begin
  for var I := 0 to FBoxNames.Count - 1 do
    if (FBoxNames.Objects[I] is TRtfdClass) then
    begin
      var
      Path := TRtfdClass(FBoxNames.Objects[I]).GetPathname;
      if not FJava.IsAValidClass(Path) then
        Exit(True);
    end;
  Result := False;
end;

procedure TRtfdDiagram.RecalcPanelSize;
begin
  FPanel.RecalcSize;
end;

procedure TRtfdDiagram.SetShowConnections(const Value: Integer);
begin
  if Value <> ShowConnections then
  begin
    FPanel.IsModified := True;
    inherited SetShowConnections(Value);
    FPanel.SetConnections(Value);
  end;
  inherited;
end;

procedure TRtfdDiagram.SelectAssociation;
begin
  FPanel.SelectAssociation;
end;

function TRtfdDiagram.GetFrame: TAFrameDiagram;
begin
  Result := FFrame;
end;

function TRtfdDiagram.GetPanel: TCustomPanel;
begin
  Result := FPanel;
end;

procedure TRtfdDiagram.CompileOneWith(Control: TControl; Compiler: string);
begin
  var
  ABox := GetBox(TRtfdBox(Control).Entity.Fullname);
  if Assigned(ABox) then
  begin
    var
    Str := ChangeFileExt(ABox.GetPathname, '.java');
    FJava.CompileOneWith(Str + '|' + ABox.Entity.Package);
  end;
end;

function TRtfdDiagram.GetFilesAndPackagesFromList(List: TList): TStringList;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  for var I := 0 to List.Count - 1 do
  begin
    var
    ABox := TRtfdBox(List[I]);
    if ((ABox is TRtfdClass) or (ABox is TRtfdInterface)) and
      Assigned(GetBox(ABox.Entity.Fullname)) then
    begin
      var
      Str := ABox.GetPathname;
      if Pos(FConfiguration.JavaCache, Str) = 0 then
        Result.Add(ChangeFileExt(Str, '.java') + '|' + ABox.Entity.Package);
    end;
  end;
end;

procedure TRtfdDiagram.Run(Control: TControl);
begin
  if Assigned(GetBox(TRtfdBox(Control).Entity.Fullname)) then
  begin
    var
    Pathname := ChangeFileExt(TRtfdBox(Control).GetPathname, '.java');
    FPanel.ClearSelection;
    FJava.Run(Pathname);
  end;
end;

function TRtfdDiagram.GetFileWithMain: string;
var
  ARtfdClass: TRtfdClass;
  AModelClass: TClass;
  It1: IModelIterator;
  Operation: UModel.TOperation;
begin
  Result := '';
  for var I := 0 to FBoxNames.Count - 1 do
  begin
    if FBoxNames.Objects[I] is TRtfdClass then
    begin
      ARtfdClass := TRtfdClass(FBoxNames.Objects[I]);
      if not(ARtfdClass.Entity is TClass) then
        Continue;
      AModelClass := TClass(ARtfdClass.Entity);
      if not AModelClass.IsAbstract then
      begin
        It1 := AModelClass.GetOperations;
        while It1.HasNext do
        begin
          Operation := UModel.TOperation(It1.Next);
          if (Operation.OperationType = otProcedure) and
            (Operation.Name = 'main') then
            Exit(AModelClass.Pathname);
        end;
      end;
    end;
  end;
end;

function TRtfdDiagram.GetAllPathnames: TStringList;
var
  List: TList;
  Str: string;
  ABox: TRtfdBox;
begin
  List := FPanel.GetManagedObjects;
  try
    Result := TStringList.Create;
    for var I := 0 to List.Count - 1 do
    begin
      ABox := TRtfdBox(List[I]);
      if ((ABox is TRtfdClass) or (ABox is TRtfdInterface)) and
        Assigned(GetBox(ABox.Entity.Fullname)) then
      begin
        Str := ABox.GetPathname;
        if Pos(FConfiguration.JavaCache, Str) = 0 then
          Result.Add(ChangeFileExt(Str, '.java'));
      end;
    end;
  finally
    List.Free;
  end;
end;

function TRtfdDiagram.GetDebug: TStringList;
var
  List: TList;
  ABox: TRtfdBox;
begin
  try
    List := FPanel.GetManagedObjects;
    Result := TStringList.Create;
    for var I := 0 to List.Count - 1 do
    begin
      ABox := TRtfdBox(List[I]);
      Result.Add(ABox.Entity.Fullname);
    end;
    Result.Add('');
  finally
    List.Free;
  end;
end;

function TRtfdDiagram.GetAllClassnames: TStringList;
var
  List: TList;
begin
  List := FPanel.GetManagedObjects;
  try
    Result := TStringList.Create;
    for var I := 0 to List.Count - 1 do
    begin
      var
      ABox := TRtfdBox(List[I]);
      if (ABox is TRtfdClass) and Assigned(GetBox(ABox.Entity.Fullname)) then
        Result.Add(ExtractFilePath(ABox.GetPathname) +
          WithoutGeneric(ABox.Entity.Fullname) + '.class');
    end;
  finally
    List.Free;
  end;
end;

function TRtfdDiagram.GetFilesAndPackages(Selected: Boolean): TStringList;
var
  List: TList;
begin
  if Selected and (FPanel.CountSelectedControls > 0) then
    List := FPanel.GetSelectedControls
  else
    List := FPanel.GetManagedObjects;
  try
    Result := GetFilesAndPackagesFromList(List);
  finally
    List.Free;
  end;
end;

procedure TRtfdDiagram.ShowInheritedMethodsFromSystemClasses(Control: TControl;
  ShowOrHide: Boolean);
begin
  if Control is TRtfdBox then
    TRtfdBox(Control).ShowInherited := ShowOrHide;
  FPanel.ClearSelection;
end;

function TRtfdDiagram.MakeParams(Parameter: TStringList;
  var TheParams: TComJavaParams; var AsString: string): Boolean;
begin
  Result := True;
  TheParams := TComJavaParams.Create;
  AsString := '';
  for var I := 0 to Parameter.Count - 1 do
  begin
    var
    AJavaValue := TComJavaValue(Parameter.Objects[I]);
    AsString := AsString + AJavaValue.AsString + ', ';
    TheParams.AddToParams(AJavaValue.AsString, AJavaValue.Sig);
  end;
  Delete(AsString, Length(AsString) - 1, 2);
end;

procedure TRtfdDiagram.DeleteParams(Parameter: TStringList);
begin
  for var I := Parameter.Count - 1 downto 0 do
  begin
    var
    AJavaValue := TComJavaValue(Parameter.Objects[I]);
    AJavaValue.Free;
  end;
  Parameter.Free;
end;

function TRtfdDiagram.CollectClasses: Boolean;
var
  Okay, AllowCompiling: Boolean;
  CorIName, Boxname: string;
begin
  // Compiling not allowed, wenn ObjectList.Count > 0;
  Result := True;
  AllowCompiling := (FComJava.ObjectList.Count = 0);
  for var I := FBoxNames.Count - 1 downto 0 do
  begin
    Boxname := TRtfdBox(FBoxNames.Objects[I]).Entity.Name;
    if (FBoxNames.Objects[I] is TRtfdClass) then
      CorIName := TClass(TRtfdBox(FBoxNames.Objects[I])
        .Entity).Pathname
    else if (FBoxNames.Objects[I] is TRtfdInterface) then
      CorIName := TInterface(TRtfdInterface(FBoxNames.Objects[I])
        .Entity).Pathname
    else
      Continue; // object
    if CorIName = '' then
      Continue
    else if FConfiguration.PathForSystemClass(CorIName) then
      Result := Result and FComJava.NewAPIClass(Boxname)
    else
    begin
      if AllowCompiling then
        Okay := FJava.PreCompile(nil, CorIName)
      else
        Okay := True;
      if Okay then
        Result := Result and FComJava.NewClass(Boxname, CorIName)
      else
        Exit(False);
    end;
  end;
end;

procedure TRtfdDiagram.CallMain(const Classpath, AClassname: string;
  CallParameter: string);
var
  I: Integer;
  InApostroph: Boolean;
  AJavaClass: TComJavaClass;
  AJavaMethod: TComJavaMethod;
  AJavaValue: TComJavaValue;
  TheParams: TComJavaParams;
begin
  FComJava.Sourcepath := Classpath;
  AJavaClass := FComJava.GetClass(AClassname);
  if not Assigned(AJavaClass) then
  begin
    ErrorMsg(_(LNGUnknownClass) + ' "' + AClassname + '"');
    Exit;
  end;

  if MyJavaCommands.ProcessRunning then
  begin
    MyJavaCommands.TerminateProcess := True;
    Exit;
  end;

  FJava.RunButtonToStop(True);
  MyJavaCommands.ProcessRunning := True;
  MyJavaCommands.ProcessRunningComJava := FComJava;
  InApostroph := False;
  I := 1;
  while I <= Length(CallParameter) do
  begin
    case CallParameter[I] of
      '"':
        InApostroph := not InApostroph;
      ' ':
        if not InApostroph then
          CallParameter[I] := ',';
    end;
    Inc(I);
  end;
  TheParams := TComJavaParams.Create;
  try
    TheParams.AddToParams('{' + CallParameter + '}', '[Ljava/lang/String;');
    AJavaMethod := TComJavaMethod.Create(AJavaClass, 'main', static, 'void',
      TheParams, FComJava);
    try
      AJavaValue := AJavaMethod.Call(nil);
      if AJavaMethod.IsValid then
      begin
        ShowMethodEntered('main', 'Actor', 'Actor', '{' + CallParameter + '}');
        AJavaValue.Free;
      end
      else
        ErrorMsg(AJavaMethod.Error);
    finally
      AJavaMethod.Free;
    end;
  finally
    TheParams.Free;
    MyJavaCommands.ProcessRunning := False;
    FJava.RunButtonToStop(False);
  end;
end;

procedure TRtfdDiagram.ShowNewObject(AJavaObject: TComJavaObject);
var
  APackage: TUnitPackage;
  AModelClass: TClassifier;
  AModelObject: TObjekt;
  Box1, Box2: TRtfdBox;
  AComJavaClass: TComJavaClass;
  Str: string;
begin
  AModelObject := nil;
  AComJavaClass := nil;
  try
    Box1 := nil;
    Box2 := nil;
    APackage := Model.ModelRoot.FindUnitPackage('Default');
    if not Assigned(APackage) then
      APackage := Model.ModelRoot.AddUnit('Default');
    FComJava.Transfer(APackage.ClassImports, APackage.FullImports,
      GetSourcePath);
    AComJavaClass := AJavaObject.ClassRef;
    if not Assigned(AComJavaClass) then
    begin
      FConfiguration.Log
        ('TRtfdDiagram.ShowNewObject AClass = nil, AJavaObject = ' +
        AJavaObject.Name);
      Exit;
    end;
    AModelClass := APackage.FindClassifier(AComJavaClass.GenericName);
    if not Assigned(AModelClass) then
    begin
      AModelClass := TClass.Create(nil);
      AModelClass.Name := AComJavaClass.Name;
      APackage.AddClass(TClass(AModelClass));
    end;
    if not(AModelClass is TClass) then
      Exit;

    AModelClass.GenericName := AComJavaClass.GenericName;
    // model-level -> show object
    AModelObject := APackage.AddObject(AJavaObject.Name, TClass(AModelClass));
    if Assigned(AModelClass) then
      Box1 := GetBox(AModelClass.GenericName);
    if Assigned(AModelObject) then
      Box2 := GetBox(AModelObject.Fullname);
    if Assigned(Box1) and Assigned(Box2) then
    begin
      Box2.Top := Box1.Top + Box1.Height + 50 + Random(30) - 30;
      Box2.Left := Max(Box1.Left + (Box1.Width - Box2.Width) div 2 + Random(200)
        - 100, 0);
      FPanel.ConnectObjects(Box2, Box1, asInstanceOf);
    end
    else if Assigned(Box2) then
    begin
      Box2.Top := Box2.Top + Random(30) - 30;
      Box2.Left := Box2.Left + Random(30) - 30;
    end;
    Box2.Font.Assign(Font);
    FPanel.RecalcSize;
    ShowAttributes(AJavaObject, AModelObject);
    // exceptions have arisen in here
    // should be fixed by initializing type classifier in constructor
    if not Assigned(Box2) and Assigned(AModelObject) then
      AddBox(AModelObject);
  except
    on E: Exception do
    begin
      Str := 'TRtfdDiagram.ShowNewObject: ';
      if Assigned(FPanel) then
        Str := Str + 'FPanel assigned'
      else
        Str := Str + 'FPanel = nil';
      if Assigned(AJavaObject) then
        Str := Str + ' AJavaObject assigned'
      else
        Str := Str + ' AJavaObject = nil';
      if Assigned(AComJavaClass) then
        Str := Str + ' AComJavaClass assigned'
      else
        Str := Str + ' AComJavaClass = nil';
      if Assigned(AModelObject) then
        Str := Str + ' AModelObject assigned'
      else
        Str := Str + ' AModelObject = nil';
      FConfiguration.Log(Str, E);
    end;
  end;
end;

procedure TRtfdDiagram.CreateObjectForSelectedClass(Sender: TObject);
var
  Caption, Title, Pathname, ParamName, ParamTyp, Str, Str1: string;
  Posi: Integer;
  AControl: TControl;
  TheClassname: string;
  TheFullClassname: string;
  TheObjectname: string;
  TheObjectnameNew: string;
  Generic: string;
  ParameterAsString: string;
  AClass: TComJavaClass;
  AJavaObject: TComJavaObject;
  TheParams: TComJavaParams;
  AJavaValue: TComJavaValue;
  Parameter: TStringList;
  MenuItem: TSpTBXItem;

  procedure ShowOnInteractive;
  begin
    FMessages.ShowTab(FInteractivePath);
    var Str := '';
    for var I := 0 to Parameter.Count - 1 do
      Str := Str + TComJavaValue(Parameter.Objects[I])
        .AsFormattedString + ', ';
    Delete(Str, Length(Str) - 1, 2);
    AddToInteractive(TheClassname + ' ' + TheObjectname + ' = new ' +
      TheClassname + '(' + Str + ');');
    FInteractive.Executer.AddVariable(TheObjectname, TheClassname,
      AJavaObject.CreateValue);
  end;

begin
  AControl := FindVCLWindow(FFrame.PopMenuClass.PopupPoint);
  FJava.DisableUpdateMenuItems;
  try
    if Assigned(AControl) and (AControl is TRtfdClass) then
    begin
      FPanel.ClearSelection(False);
      Pathname := TRtfdClass(AControl).GetPathname;
      TheFullClassname := TRtfdClass(AControl).Entity.Name;
      TheClassname := TheFullClassname;
      ParameterAsString := '';

      Posi := Pos('$', TheClassname); // inner class
      if Posi > 0 then
        Delete(TheClassname, 1, Posi);

      Posi := Pos('<', TheClassname);
      if Posi > 0 then
      begin
        Generic := Copy(TheClassname, Posi + 1, Length(TheClassname) -
          Posi - 1);
        TheClassname := Copy(TheClassname, 1, Posi - 1);
      end;
      if FConfiguration.PathForUserClass(Pathname) and
        HasJavaExtension(Pathname) and not FJava.PreCompile(nil, Pathname) then
        Exit;
      if not CollectClasses then
        Exit; // Init und LoadClass
      if not Assigned(FComJava.GetClass(TheFullClassname)) then
      begin
        ErrorMsg(_(LNGUnknownClass) + ' "' + TheClassname + '"');
        Exit;
      end;

      // get parameters for constructor
      Parameter := TStringList.Create;
      MenuItem := TSpTBXItem(Sender);
      Str := MenuItem.Caption;
      if Assigned(MenuItem.Parent) then
      begin
        Str1 := MenuItem.Parent.Caption;
        Posi := Pos('Inherited from ', Str1);
        if Posi > 0 then
          TheClassname := Copy(Str1, 16, Length(Str1));
      end;

      Posi := FFullParameters.IndexOfName(Str);
      Str := FFullParameters.ValueFromIndex[Posi];
      Posi := Pos('(', Str);
      Delete(Str, 1, Posi);
      if Str <> ')' then
      begin
        Str := ReplaceStr(Str, ')', ', )');
        while Str <> ')' do
        begin
          Posi := Pos(' ', Str);
          ParamTyp := Copy(Str, 1, Posi - 1);
          Delete(Str, 1, Posi);
          if ParamTyp = Generic then
            ParamTyp := 'java.lang.Object';

          Posi := Pos(',', Str);
          ParamName := Copy(Str, 1, Posi - 1);
          Delete(Str, 1, Posi + 1);
          AJavaValue := TComJavaValue.Create(ParamTyp, FComJava);
          Parameter.AddObject(GetShortType(ParamTyp) + ' ' + ParamName,
            AJavaValue);
        end;
      end;

      if Parameter.Count = 0 then
      begin
        Title := _('Attribute') + #13#10 + _(LNGValue);
        Caption := _(LNGNameOfObject);
      end
      else
      begin
        Title := _('Parameter') + #13#10 + _(LNGValue);
        Caption := _('Parameter for constructor') + ' ' + TheClassname
          + '(...)';
      end;
      TheObjectname := FComJava.GetUniqueObjectName(TheClassname);
      // getNewObjectName
      TheObjectnameNew := '';
      TheParams := nil;

      if (Parameter.Count = 0) or EditClass(Caption, Title, TheObjectname,
        TheObjectnameNew, AControl, Parameter) and MakeParams(Parameter, TheParams,
        ParameterAsString) then
      begin
        if (TheObjectnameNew <> '') and
          not Assigned(FComJava.GetObject(TheObjectnameNew)) then
          TheObjectname := TheObjectnameNew;
        AClass := FComJava.GetClass(TheFullClassname);
        if Assigned(AClass) then
        begin
          AClass.Generic := Generic;
          AClass.GenericName := TheFullClassname;
          AJavaObject := FComJava.NewObject(TheObjectname, AClass, TheParams);
          // java-level
          if AJavaObject.IsValid then
          begin
            ShowMethodEntered('<init>', 'Actor', TheObjectname,
              ParameterAsString);
            ShowOnInteractive;
            ShowNewObject(AJavaObject);
            if FConfiguration.ShowAllNewObjects then
              ShowAllNewObjectsString(TheObjectname);
            UpdateAllObjects;
            ResolveObjectAssociations;
            FPanel.RecalcSize;
          end
          else
          begin
            if FConfiguration.LogfileInteractiveOK then
              FComJava.ExecuteCommand
                ('logMemo#' + FConfiguration.LogfileInteractive + '#' +
                UDlgAbout.Version);
            ErrorMsg(AJavaObject.Error);
            FreeAndNil(AJavaObject);
          end;
        end;
        FreeAndNil(TheParams);
      end;
      DeleteParams(Parameter);
    end;
  finally
    FJava.EnableUpdateMenuItems;
  end;
end;

procedure TRtfdDiagram.ShowAttributes(AJavaObject: TComJavaObject;
  AModelObject: TObjekt);
var
  AModelAttribut: TAttribute;
  StringList, StringList1: TStringList;
  Error: string;
begin
  StringList := nil;
  StringList1 := nil;
  try
    if not Assigned(AJavaObject) then
    begin
      FConfiguration.Log('ShowAttributes: aJavObject = nil');
      Exit;
    end;
    if not Assigned(AModelObject) then
    begin
      FConfiguration.Log('ShowAttributes: AModelObject = nil');
      Exit;
    end;
    // just create attributes, they will be shown by UpdateAllObjects
    StringList := AJavaObject.GetObjectAttributes;
    // for example s =
    // protected|0|0||I|modCount
    // private|-1|-1||long|serialVersionUID
    // private|0|0||java.lang.Object[]|elementData
    // private|0|0||I|size
    // private|-1|-1||I|MAX_ARRAY_SIZE
    AModelObject.SetCapacity(StringList.Count); // nevertheless very slow!

    for var I := 0 to StringList.Count - 1 do
    begin
      StringList1 := Split('|', StringList[I]);
      if StringList1.Count >= 6 then
      begin // object has an attribute
        AModelAttribut := AModelObject.AddAttribute(StringList1[5]);
        // AddAttribut can raise an exception
        AModelAttribut.Visibility := String2Visibility(StringList1[0]);
        AModelAttribut.Static := (StringList1[1] = '-1');
        AModelAttribut.IsFinal := (StringList1[2] = '-1');
        if AModelAttribut.IsFinal then
          AModelObject.HasFinal := True;
        AModelAttribut.TypeClassifier := FindClassifier(StringList1[4]);
        if not Assigned(AModelAttribut.TypeClassifier) then
        begin
          AModelAttribut.TypeClassifier := TClassifier.Create(nil);
          AModelAttribut.TypeClassifier.Name := StringList1[4];
        end;
        AModelAttribut.TypeClassifier.Generic := StringList1[3];
        // Found: TypeClassifier was not set to nil in the constructor of ModelAttribute
      end;
      FreeAndNil(StringList1);
    end;
  except
    on E: Exception do
    begin
      Error := 'TRtfdDiagram.ShowAttributes TypeClassifier ' + #13#10 +
        StringList.Text + '#' + StringList1.Text + '#' + #13#10;
      if StringList1.Count >= 5 then
        Error := Error + '>' + StringList1[3] + '<' + '>' + StringList1[4] + '<';
      FConfiguration.Log(Error, E);
    end;
  end;

end;

procedure TRtfdDiagram.GetAllAttributeValues(AClass: TClass;
  AJavaObject: TComJavaObject; AAttribut: TComJavaAttribute;
  Attributes: TStringList);
var
  Ite: IModelIterator;
  AItAttribut: TAttribute;
  AJavaValue: TComJavaValue;
begin
  if Assigned(AClass.Ancestor) then
    GetAllAttributeValues(AClass.Ancestor, AJavaObject, AAttribut, Attributes);
  Ite := AClass.GetAttributes;
  while Ite.HasNext do
  begin
    AItAttribut := TAttribute(Ite.Next);
    if not AItAttribut.IsFinal and (FConfiguration.PrivateAttributEditable or
      (AItAttribut.Visibility <> viPrivate)) then
    begin
      AJavaValue := TComJavaValue.Create(AItAttribut.TypeClassifier.Name,
        FComJava);
      AJavaValue.Value := AAttribut.GetAttributeValue(AJavaObject,
        AItAttribut.Name, AItAttribut.TypeClassifier.Name, AItAttribut.Static);
      Attributes.AddObject(AItAttribut.TypeClassifier.GetShortType + ' ' +
        AItAttribut.Name, AJavaValue);
    end;
  end;
end;

procedure TRtfdDiagram.SetAttributeValues(AClass: TClass;
  AJavaObject: TComJavaObject; AAttribut: TComJavaAttribute;
  Attributes: TStringList);
var
  Ite: IModelIterator;
  AItAttribut: TAttribute;
  AJavaValue: TComJavaValue;
  Kidx: Integer;
begin
  if Assigned(AClass.Ancestor) then
    SetAttributeValues(AClass.Ancestor, AJavaObject, AAttribut, Attributes);
  Ite := AClass.GetAttributes;
  while Ite.HasNext do
  begin
    AItAttribut := TAttribute(Ite.Next);
    if FConfiguration.PrivateAttributEditable or
      (AItAttribut.Visibility <> viPrivate) then
    begin
      Kidx := Attributes.IndexOf(AItAttribut.TypeClassifier.GetShortType + ' ' +
        AItAttribut.Name);
      if Kidx > -1 then
      begin
        AJavaValue := TComJavaValue(Attributes.Objects[Kidx]);
        AAttribut.SetAttributeValue(AJavaObject, AItAttribut.Name,
          AItAttribut.TypeClassifier.Name, AJavaValue.AsString,
          AItAttribut.Static);
      end;
    end;
  end;
end;

procedure TRtfdDiagram.CallMethodForObject(Sender: TObject);
begin
  var
  Control := FindVCLWindow(FFrame.PopMenuObject.PopupPoint);
  CallMethod(Control, Sender);
end;

procedure TRtfdDiagram.CallMethodForClass(Sender: TObject);
begin
  var
  Control := FindVCLWindow(FFrame.PopMenuClass.PopupPoint);
  CallMethod(Control, Sender);
end;

procedure TRtfdDiagram.CallMethod(AControl: TControl; Sender: TObject);
var
  Caption, Title, ParamName, ParamTyp, ParamTypName, Str, Sig, LongType: string;
  Posi, InheritedLevel: Integer;
  TheObjectname: string;
  TheMethodname: string;
  TheReturntype: string;
  ParamsAsString: string;

  AJavaObject: TComJavaObject;
  AJavaMethod: TComJavaMethod;
  AJavaValue: TComJavaValue;
  AJavaClass: TComJavaClass;
  AViewClass: TRtfdClass;
  AModelClass: TClass;
  TheParams: TComJavaParams;
  MethodType: TMethodAttribute;
  Parameter: TStringList;
  Values: string;
  Str1, Str2, From: string;
begin
  try
    if Assigned(AControl) and ((AControl is TRtfdObject) or
      (AControl is TRtfdClass)) then
    begin
      if not CollectClasses then
        Exit;
      LockWindow(FUMLForm.Handle);
      AModelClass := nil;
      ParamsAsString := '';
      try
        FPanel.ClearSelection;
        if AControl is TRtfdObject then
          TheObjectname := TRtfdObject(AControl).Entity.Name
        else
          TheObjectname := '';
        InheritedLevel := TSpTBXItem(Sender).Tag;
        // get parameters for method-call
        Parameter := TStringList.Create;
        Str := TSpTBXItem(Sender).Caption;
        Posi := FFullParameters.IndexOfName(Str);
        Str := FFullParameters.ValueFromIndex[Posi];
        if Pos('static', Str) = 1 then
        begin
          Delete(Str, 1, 7);
          MethodType := static;
        end
        else if AControl is TRtfdClass then
          MethodType := static
        else
          MethodType := nonstatic;
        TheReturntype := GetNextPart(Str);
        if Length(TheReturntype) = 1 then
          TheReturntype := 'java.lang.Object'; // generic

        Posi := Pos('(', Str);
        TheMethodname := GetShortType(Copy(Str, 1, Posi - 1));
        Delete(Str, 1, Posi);
        Str := Trim(Str);
        if Str <> ')' then
        begin
          Str := ReplaceStr(Str, ')', ',)');
          while Str <> ')' do
          begin
            ParamTypName := GetNextPart(Str, ',');
            Str := Trim(Str);
            Posi := LastDelimiter(' ', ParamTypName);
            if Posi > 0 then
            begin
              ParamTyp := Copy(ParamTypName, 1, Posi - 1);
              ParamName := Copy(ParamTypName, Posi + 1, Length(ParamTypName));
            end
            else
            begin
              ParamTyp := ParamTypName;
              ParamName := '';
            end;
            if Length(ParamTyp) = 1 then
              AJavaValue := TComJavaValue.Create('java.lang.Object', FComJava)
            else
              AJavaValue := TComJavaValue.Create(ParamTyp, FComJava);
            Parameter.AddObject(GetShortType(ParamTyp) + ' ' + ParamName,
              AJavaValue);
          end;
        end;
        Caption := _('Parameter for method call') + ' ' + TheMethodname + '(...)';
        Title := _('Parameter') + #13#10 + _(LNGValue);
        // get parameter-values from user
        if (Parameter.Count = 0) or EditObjectOrParams(Caption, Title, AControl, Parameter)
        then
        begin
          if TheReturntype <> 'void' then
          begin
            Values := '(';
            for var I := 0 to Parameter.Count - 1 do
            begin
              AJavaValue := TComJavaValue(Parameter.Objects[I]);
              Values := Values + AJavaValue.Value + ', ';
            end;
            Values := ReplaceStr(Values + ')', ', )', ')');
          end;
          if MakeParams(Parameter, TheParams, ParamsAsString) then
          begin
            // call the method
            if TheObjectname = '' then
            begin // static method of a class
              AJavaObject := nil;
              AModelClass := TClass(TRtfdClass(AControl).Entity);
            end
            else
            begin // method of an object
              AJavaObject := FComJava.GetObject(TheObjectname); // java-level
              AJavaClass := AJavaObject.ClassRef;
              LongType := AJavaClass.Name;
              // get model class for object to get the methods
              AViewClass := TRtfdClass(GetBox(LongType));
              if not Assigned(AViewClass) then
                AViewClass := TRtfdClass(GetBox(GetShortType(LongType)));
              if Assigned(AViewClass) then
                AModelClass := TClass(AViewClass.Entity)
                // model class from view class
              else
                AModelClass := nil;
              if not Assigned(AModelClass) then
                AModelClass := GetModelClass(LongType);
              // model class from model
              if not Assigned(AModelClass) or (AModelClass.Pathname = '') then
                AModelClass := CreateModelClass(LongType);
              // model class from java
            end;
            // if assigned(AModelClass)
            while InheritedLevel > 0 do
            begin
              AModelClass := AModelClass.Ancestor;
              Dec(InheritedLevel);
              if AControl is TRtfdObject then
                MethodType := nonvirtual;
            end;
            AJavaClass := FComJava.GetClass(AModelClass.GetTyp);
            if Assigned(AJavaClass) then
            begin
              AJavaMethod := TComJavaMethod.Create(AJavaClass, TheMethodname,
                MethodType, TheReturntype, TheParams, FComJava);
              try
                AJavaValue := AJavaMethod.Call(AJavaObject);
                if AJavaMethod.IsValid then
                begin
                  ShowMethodEntered(TheMethodname, 'Actor', TheObjectname,
                    ParamsAsString);
                  Str := '';
                  for var I := 0 to Parameter.Count - 1 do
                    Str := Str + TComJavaValue(Parameter.Objects[I])
                      .AsFormattedString + ', ';
                  Delete(Str, Length(Str) - 1, 2);
                  Sig := AModelClass.GetTyp + ' ' + TheReturntype + ' ' +
                    TheParams.Signature;
                  case MethodType of
                    static:
                      Sig := 'static ' + Sig;
                    nonvirtual:
                      Sig := 'nonvirtual ' + Sig;
                  end;
                  From := TheObjectname;
                  if From = '' then
                    From := AModelClass.Name;
                  AddToInteractive(From + '.' + TheMethodname + '(' +
                    Str + ');');

                  if FConfiguration.ShowAllNewObjects then
                    ShowAllNewObjectsString(From);
                  UpdateAllObjects;

                  // show the return-Value
                  if TheReturntype <> 'void' then
                  begin
                    if TheObjectname = '' then
                      Str1 := AModelClass.Name + '.' + TheMethodname + Values
                    else
                      Str1 := TheObjectname + '.' + TheMethodname + Values;
                    Str2 := AJavaValue.Value;
                    if (Str2 <> 'null') and (AJavaValue.Kind = ntObject) then
                    begin
                      Str2 := Str2 + '/' + AJavaValue.ToString;
                    end;
                    ShowMethodExited(TheMethodname, TheObjectname, 'Actor',
                      AJavaValue.Value);
                    if FConfiguration.ShowFunctionValues then
                      FMessages.OutputToTerminal(Str1 + ': ' + Str2 + #13#10)
                    else
                      with TFMethodCallDialog.Create(Self) do
                      begin
                        EMethodCall.Text := TSpTBXItem(Sender).Caption;
                        EParametervalues.Text := Str1;
                        EResult.Text := Str2;
                        Prepare;
                        ShowModal;
                        Free;
                      end;
                  end
                  else
                    ShowMethodExited(TheMethodname, TheObjectname, 'Actor', '');

                  FreeAndNil(AJavaValue);
                end
                else
                begin
                  ErrorMsg(AJavaMethod.Error);
                  FMessages.OutputToTerminal(AJavaMethod.Error);
                end;
              finally
                FreeAndNil(AJavaMethod);
              end;
            end;
            FreeAndNil(TheParams);
          end;
        end;
        DeleteParams(Parameter);
      except
        on E: Exception do
        begin
          Str := 'CallMethod-TheObjectname: ';
          if TheObjectname = '' then
            Str := Str + '<leer> AModelClass: '
          else
            Str := Str + TheObjectname + ' AModelClass: ';
          if Assigned(AModelClass) then
            Str := Str + AModelClass.Name
          else
            Str := Str + '<nil>';
          FConfiguration.Log(Str, E);
          ErrorMsg(E.Message);
        end;
      end;
    end;
  finally
    if Assigned(FUMLForm) then
      UnlockWindow;
  end;
end;

function TRtfdDiagram.CreateModelClass(const Typ: string): TClass;
var
  APackage: TUnitPackage;
  AClass: TClass;
  Operation: UModel.TOperation;
  Attribute: UModel.TAttribute;
  AJavaClass: TComJavaClass;
  StringList, SLParameter, SLParTypesLong, StringList1: TStringList;
  Posi: Integer;
  Str, Str1, ParTyp, ParName: string;
  CodeCompletion: TCodeCompletion;
  ParNames, ParTypesShort, Superclassname: string;
  TypeClass: TClassifier;
begin
  Result := nil;
  APackage := Model.ModelRoot.FindUnitPackage('Default');
  CodeCompletion := TCodeCompletion.Create; // used for parameter-names
  SLParTypesLong := TStringList.Create;
  if CodeCompletion.isJavaAPIClass(Typ) then
    SLParameter := CodeCompletion.getJavaAPIMethodParameters
  else
    SLParameter := nil;
  try
    AJavaClass := FComJava.GetClass(Typ);

    if Assigned(AJavaClass) and AJavaClass.IsValid then
    begin
      AClass := APackage.MakeClass(Typ, '');
      if not Assigned(AClass) then
        Exit;

      AClass.Importname := Typ;
      APackage.AddClassWithoutShowing(AClass);

      // make operations
      StringList := AJavaClass.GetMethods(''); // static and not static
      for var I := 0 to StringList.Count - 1 do
      begin
        ParTypesShort := '';
        SLParTypesLong.Clear;
        Str := StringList[I];
        Posi := Pos('throws ', Str);
        if Posi > 0 then
          Str := Trim(Copy(Str, 1, Posi - 1));
        if Str = '' then
          Continue;
        Operation := AClass.AddOperationWithoutType('');
        Operation.Static := False;
        Operation.OperationType := otFunction;

        Str1 := GetNextPart(Str);
        if IsVisibility(Str1) then
        begin
          Operation.Visibility := String2Visibility(Str1);
          Str1 := GetNextPart(Str);
        end
        else
          Operation.Visibility := viPackage;
        while IsModifier(Str1) do
        begin
          if Str1 = 'static' then
            Operation.Static := True;
          Str1 := GetNextPart(Str);
        end;
        Operation.ReturnValue := FindClassifier(Str1);
        if Str1 = 'void' then
          Operation.OperationType := otProcedure;
        Posi := Pos('(', Str);
        Operation.Name := GetShortType(Copy(Str, 1, Posi - 1));
        Delete(Str, 1, Posi);
        Posi := Pos(')', Str);
        Str[Posi] := ',';
        ParTyp := GetNextPart(Str, ',');
        while ParTyp <> '' do
        begin
          SLParTypesLong.Add(ParTyp);
          ParTypesShort := ParTypesShort + GetShortType(ParTyp) + ',';
          ParTyp := GetNextPart(Str, ',');
        end;

        if Assigned(SLParameter) then
        begin
          Posi := SLParameter.IndexOfName(Operation.Name + '/' + ParTypesShort);
          if Posi > -1 then
            ParNames := SLParameter.ValueFromIndex[Posi]
          else
            ParNames := '';
        end
        else
          ParNames := '';

        for var J := 0 to SLParTypesLong.Count - 1 do
        begin
          ParTyp := SLParTypesLong[J];
          if ParNames = '' then
            ParName := '' // 'Par' + IntToStr(J)
          else
          begin
            Posi := Pos(',', ParNames);
            ParName := Copy(ParNames, 1, Posi - 1);
            Delete(ParNames, 1, Posi);
          end;
          Operation.AddParameter(ParName).TypeClassifier :=
            FindClassifier(ParTyp); // ToDo ParName = ''
        end;
      end;

      // make attributes
      StringList := AJavaClass.GetClassAttributes;
      for var I := 0 to StringList.Count - 1 do
      begin
        StringList1 := Split('|', StringList[I]);
        try
          if StringList1.Count = 6 then
          begin
            TypeClass := FindClassifier(StringList1[4]);
            if not Assigned(TypeClass) then
            begin
              TypeClass := TClassifier.Create(nil);
              TypeClass.Name := StringList1[4];
            end;
            Attribute := AClass.AddAttribute('', TypeClass);
            Attribute.Visibility := String2Visibility(StringList1[0]);
            Attribute.Static := (StringList1[1] = '-1');
            Attribute.IsFinal := (StringList1[2] = '-1');
            if Attribute.IsFinal then
              Attribute.HasFinal := True;
            Attribute.TypeClassifier.Generic := StringList1[3];
            Attribute.Name := StringList1[5];
          end;
        finally
          FreeAndNil(StringList1);
        end;
      end;
      // make ancestor
      Superclassname := AJavaClass.GetSuperclassName;
      if Superclassname <> '' then
        AClass.Ancestor := TClass(FindClassifier(Superclassname));
      Result := AClass;
    end;
  finally
    FreeAndNil(CodeCompletion);
    FreeAndNil(SLParameter);
    FreeAndNil(SLParTypesLong);
  end;
end;

function TRtfdDiagram.PPIScale(ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, FUMLForm.CurrentPPI, 96);
end;

function TRtfdDiagram.PPIUnScale(ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, 96, FUMLForm.CurrentPPI);
end;

function TRtfdDiagram.FindClassifier(const CName: string): TClassifier;
var
  PName, ShortName: string;
  CacheI: Integer;
  AClass: TClass;
  AInterface: TInterface;
  TheClass: TModelEntityClass;

  function InLookInModel: TClassifier;
  var
    APackage: TUnitPackage;
  begin
    Result := nil;
    if PName <> '' then
    begin // search in package
      APackage := Model.ModelRoot.FindUnitPackage(PName);
      if Assigned(APackage) then
        Result := APackage.FindClassifier(ShortName, TheClass, True);
    end;
    if not Assigned(Result) then
    begin
      APackage := Model.ModelRoot.FindUnitPackage('Default');
      Result := APackage.FindClassifier(ShortName, TheClass, True);
    end;
    if not Assigned(Result) then
    begin
      APackage := Model.ModelRoot.FindUnitPackage('Default');
      Result := APackage.FindClassifier(CName, TheClass, True);
    end;
  end;

  function ExtractPackageName(const CName: string): string;
  begin
    var
    I := LastDelimiter('.', CName);
    if I = 0 then
      Result := ''
    else
      Result := Copy(CName, 1, I - 1);
  end;

  function ExtractClassName(const CName: string): string;
  begin
    var
    I := LastDelimiter('.', CName);
    if I = 0 then
      Result := CName
    else
      Result := Copy(CName, I + 1, 255);
    Result := WithoutArray(Result);
  end;

begin
  TheClass := nil;
  CacheI := Model.NameCache.IndexOf(CName);
  if CacheI > -1 then
    Exit(TClassifier(Model.NameCache.Objects[CacheI]));
  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);
  Result := InLookInModel;

  if Assigned(Result) and (Pos('[]', CName) > 0) and (Result.Name <> CName) then
  begin
    if (Result is TClass) then
    begin
      AClass := TClass.Create(Result.Owner);
      AClass.Pathname := Result.Pathname;
      AClass.Importname := Result.Importname;
      AClass.Ancestor := TClass(Result).Ancestor;
      AClass.Name := CName;
      AClass.Visibility := Result.Visibility;
      AClass.Static := Result.Static;
      Result := AClass;
    end
    else
    begin
      AInterface := TInterface.Create(Result.Owner);
      AInterface.Pathname := Result.Pathname;
      AInterface.Importname := Result.Importname;
      AInterface.Ancestor := TInterface(Result).Ancestor;
      AInterface.Name := CName;
      AInterface.Visibility := Result.Visibility;
      AInterface.Static := Result.Static;
      Result := AInterface;
    end;
    Model.NameCache.AddObject(CName, Result);
  end;

  if not Assigned(Result) then
  begin
    Result := Model.UnknownPackage.FindClassifier(CName, TClass, True);
    if not Assigned(Result) then
    begin
      Result := Model.UnknownPackage.MakeClass(CName, '');
      Model.UnknownPackage.AddClass(TClass(Result));
    end;
  end;
  if Assigned(Result) and (Pos('/', Result.Name) > 0) then
    Result.Name := ReplaceStr(Result.Name, '/', '.');
end;

procedure TRtfdDiagram.OpenClass(Control: TControl);
begin
  if Assigned(Control) and (Control is TRtfdObject) then
  begin
    var
    CName := TRtfdObject(Control).GetClassname;
    TFUMLForm(FFrame.Parent.Owner).AddClassToProject(CName);
  end;
  FPanel.ClearSelection;
end;

procedure TRtfdDiagram.EditObject(Control: TControl);
var
  Caption, Title, ObjectNameOld, AClassType: string;
  AJavaObject: TComJavaObject;
  AJavaClass: TComJavaClass;
  AJavaAttribut: TComJavaAttribute;
  AJavaValue: TComJavaValue;
  Attributes: TStringList;
  APackage: TUnitPackage;
  AModelClass: TClass;
  AObject: TObject;
begin
  if Assigned(Control) and (Control is TRtfdObject) then
  begin
    FPanel.ClearSelection;
    ObjectNameOld := TRtfdObject(Control).Entity.Name;
    AJavaObject := FComJava.GetObject(ObjectNameOld);
    AJavaClass := AJavaObject.ClassRef;
    AJavaAttribut := TComJavaAttribute.Create(AJavaClass, FComJava);

    APackage := Model.ModelRoot.FindUnitPackage('Default');
    FComJava.Transfer(APackage.ClassImports, APackage.FullImports,
      GetSourcePath);
    AClassType := AJavaClass.Name;
    AModelClass := TClass(APackage.FindClassifier(AClassType));
    if Assigned(AModelClass) then
    begin
      Attributes := TStringList.Create;
      GetAllAttributeValues(AModelClass, AJavaObject, AJavaAttribut,
        Attributes);
      Caption := _('Edit object') + ' ' + ObjectNameOld;
      Title := _('Attribute') + #13#10 + _(LNGValue);
      if EditObjectOrParams(Caption, Title, Control, Attributes) then
      begin
        SetAttributeValues(AModelClass, AJavaObject, AJavaAttribut, Attributes);
        UpdateAllObjects;
      end;
      for var I := Attributes.Count - 1 downto 0 do
      begin
        AJavaValue := TComJavaValue(Attributes.Objects[I]);
        if AJavaValue.Kind <> ntObject then
        begin
          AObject := Attributes.Objects[I];
          FreeAndNil(AObject);
        end;
      end;
      Attributes.Clear;
    end
    else
      ErrorMsg(Format(_(LNGClassNotFound), [AJavaClass.Signature]));
  end;
end;

procedure TRtfdDiagram.UpdateAllObjects;
var
  APackage: TUnitPackage;
  AJavaObject: TComJavaObject;
  AModelClass: TClassifier;
  AModelObject: TObjekt;
  AModelAttribut: TAttribute;
  AModelClassAttribut: TAttribute;
  It1, It2, It3: IModelIterator;
  AObjectList, SL_V, SL_N: TStringList;
  Num: Integer;
  Value, Str: string;

  function Shorten(const Str: string): string;
  begin
    Result := Str;
    if Length(Str) > 100 then
    begin
      var
      Posi := 100;
      while (Posi > 0) and (Str[Posi] <> ',') do
        Dec(Posi);
      if Posi = 0 then
        Posi := 96;
      Result := Copy(Str, 1, Posi + 1) + '...}';
    end;
  end;

begin
  try
    if Assigned(Model) and Assigned(Model.ModelRoot) then
      APackage := Model.ModelRoot.FindUnitPackage('Default')
    else
      APackage := nil;
    if not Assigned(APackage) then
      Exit;
    FComJava.Transfer(APackage.ClassImports, APackage.FullImports,
      GetSourcePath);
    AObjectList := APackage.GetAllObjects;
    for var I := 0 to AObjectList.Count - 1 do
    begin
      AJavaObject := FComJava.GetObject(AObjectList[I]);
      if not Assigned(AJavaObject) then
        Continue;
      SL_N := AJavaObject.GetAttributeNames;
      SL_V := AJavaObject.GetAttributeValues;
      // SL_V may contain an error
      if Pos('-ERR', SL_V.Text) = 1 then
      begin
        FConfiguration.Log('TRtfdDiagram.UpdateAllObjects A: ' + SL_V.Text);
        Continue;
      end;
      if (SL_N.Count <> SL_V.Count) or (SL_N.Count <= 1) then
      begin
        SL_N := AJavaObject.GetRefreshedAttributeNames;
        SL_V := AJavaObject.GetAttributeValues;
      end;
      if SL_N.Count <> SL_V.Count then
      begin
        FConfiguration.Log('TRtfdDiagram.UpdateAllObjects B: ' +
          AJavaObject.DebugGetAttributeValues + ' SL_N: ' + IntToStr(SL_N.Count)
          + ' SL_V: ' + IntToStr(SL_V.Count));
        Continue;
      end;
      if Assigned(AObjectList) and Assigned(AObjectList.Objects[I]) and
        (AObjectList.Objects[I] is TObjekt) then
        AModelObject := TObjekt(AObjectList.Objects[I])
      else
      begin
        FConfiguration.Log('TRtfdDiagram.UpdateAllObjects C: ' +
          AObjectList[I] + ' | ' + AObjectList.Objects[I]
          .ClassName);
        Continue;
      end;
      It1 := AModelObject.GetAttributes;
      while It1.HasNext do
      begin
        AModelAttribut := TAttribute(It1.Next);
        Num := SL_N.IndexOf(AModelAttribut.Name);
        if Num = -1 then
        begin
          SL_N := AJavaObject.GetRefreshedAttributeNames;
          SL_V := AJavaObject.GetAttributeValues;
          Num := SL_N.IndexOf(AModelAttribut.Name);
        end;
        if (0 <= Num) and (Num < SL_V.Count) then
          Value := Shorten(SL_V[Num])
        else if AModelAttribut.Name = '' then
          Value := ''
        else
        begin
          It3 := AModelObject.GetAttributes;
          Str := '';
          while It3.HasNext do
          begin
            AModelAttribut := TAttribute(It3.Next);
            Str := Str + '|' + AModelAttribut.Name;
          end;
          Value := '<Error>';
          FConfiguration.Log('TRtfdDiagram.UpdateAllObjects D: ' +
            AObjectList[I] + ' | ' + AObjectList.Objects[I]
            .ClassName);
        end;

        if AModelAttribut.Static then
        begin
          AModelClass := AModelObject.GetTyp;
          if Assigned(AModelClass) then
          begin
            It2 := AModelClass.GetAttributes;
            while It2.HasNext do
            begin
              AModelClassAttribut := TAttribute(It2.Next);
              if AModelClassAttribut.Name = AModelAttribut.Name then
              begin
                AModelClassAttribut.Value := Value;
                Break;
              end;
            end;
          end;
        end
        else
          AModelAttribut.Value := Value;
      end;
      AModelObject.RefreshEntities;
      FInteractive.Executer.AddVariable(AJavaObject.Name,
        AJavaObject.ClassRef.Name, AJavaObject.CreateValue);
    end;
    FreeAndNil(AObjectList);
    RefreshDiagram;
  except
    on E: Exception do
      FConfiguration.Log('UpdateAllObjects: ', E);
  end;
end;

function TRtfdDiagram.InsertParameterNames(Str: string): string;
var
  Posi: Integer;
  Str1, Str2: string;
begin
  Posi := Pos('(', Str);
  Str1 := Copy(Str, 1, Posi);
  Delete(Str, 1, Posi);
  Str := ReplaceStr(Str, ')', ',)');
  Posi := Pos(',', Str);
  while Posi > 0 do
  begin
    Str2 := Copy(Str, 1, Posi - 1);
    Delete(Str, 1, Posi);
    Str2 := GetShortTypeWith(Str2);
    Posi := Pos(',', Str);
    if Posi > 0 then
      Str1 := Str1 + Str2 + ', '
    else
      Str1 := Str1 + Str2 + ')';
  end;
  Result := Str1;
end;

procedure TRtfdDiagram.PopMenuClassPopup(Sender: TObject);
var
  Str1, Str2: string;
  Num, Posi, MenuIndex, InheritedLevel, StartIndex: Integer;
  AViewClass: TRtfdClass;
  AViewInterface: TRtfdInterface;
  AModelClass, SuperClass: TClass;
  AModelInterface, SuperInterface: TInterface;
  It1, It2: IModelIterator;
  Operation: UModel.TOperation;
  Attribute: UModel.TAttribute;
  AInterface: UModel.TInterface;
  Parameter: TParameter;
  AInheritedMenu: TSpTBXSubmenuItem;
  AMenuItem: TSpTBXItem;
  HasSourcecode: Boolean;
  Associations: TStringList;
  Interfaces: TStringList;
  Connections: TStringList;
  SLSorted: TStringList;
  NoSystemClass: Boolean;
  ABox: TControl;
  HasMain: Boolean;
  HasInheritedSystemMethods: Boolean;
  MethodWithParam, MethodNoParam: string;

  procedure MakeOpenClassMenuItem(const Caption: string; ImageIndex: Integer);
  begin
    if FBoxNames.IndexOf(Caption) = -1 then
    begin
      var
      AMenuItem := TSpTBXItem.Create(FFrame.PopMenuClass);
      AMenuItem.Caption := WithoutGeneric(Caption);
      AMenuItem.OnClick := OpenClassOrInterface;
      AMenuItem.ImageIndex := ImageIndex;
      FFrame.MIClassPopupOpenClass.Add(AMenuItem);
    end;
  end;

  procedure MakeConnectClassMenuItems;
  var
    AMenuItem: TSpTBXItem;
    Chr: Char;
    Str: string;
  begin
    for var I := FBoxNames.Count - 1 downto 0 do
      if (FBoxNames.Objects[I] is TRtfdClass) then
        Connections.Add('C' + TRtfdBox(FBoxNames.Objects[I])
          .Entity.Fullname)
      else if (FBoxNames.Objects[I] is TRtfdInterface) then
        Connections.Add('I' + TRtfdBox(FBoxNames.Objects[I])
          .Entity.Fullname)
      else if (FBoxNames.Objects[I] is TRtfdCommentBox) then
        Connections.Add('K' + FBoxNames[I]);
    for var I := 0 to Connections.Count - 1 do
    begin
      Str := Connections[I];
      Chr := Str[1];
      Str := Copy(Str, 2, Length(Str));
      AMenuItem := TSpTBXItem.Create(FFrame.PopMenuClass);
      AMenuItem.Caption := WithoutGeneric(Str);
      AMenuItem.Caption := Str;
      AMenuItem.OnClick := ConnectBoxes;
      case Chr of
        'C':
          AMenuItem.ImageIndex := 0;
        'I':
          AMenuItem.ImageIndex := 16;
        'K':
          AMenuItem.ImageIndex := 23;
      end;
      Str := TRtfdBox(ABox).Entity.Name;
      AMenuItem.Tag := FBoxNames.IndexOf(Str);
      FFrame.MIClassPopupConnect.Add(AMenuItem);
    end;
  end;

  procedure AddDatatype(const Str: string);
  var
    Str1, Str2, Path: string;
  begin
    if IsSimpleType(Str) then
      Exit;
    Str1 := WithoutGeneric(WithoutArray(Str));
    if IsSimpleType(Str1) or (Str1 = '') then
      Exit;
    for var I := 0 to FBoxNames.Count - 1 do
      if Pos(Str1, FBoxNames[I]) = 1 then
        Exit;
    if (Associations.IndexOf(Str1) > -1) or (Interfaces.IndexOf(Str1) > -1) then
      Exit;
    Str2 := GetShortType(Str1);
    if FConfiguration.IsAPIClass(Str2) then
      Associations.Add(Str2)
    else if FConfiguration.IsAPIInterface(Str2) then
      Interfaces.Add(Str2)
    else
    begin
      if TComJavaClass.FindClass(Str1, FComJava) then
        if TComJavaClass.IsInterface(Str1, FComJava) then
          Interfaces.Add(Str1)
        else
          Associations.Add(Str1)
      else
      begin
        Path := FConfiguration.SearchClassInDirectory(ExtractClassName(Str1),
          ExtractFilePath(FUMLForm.Pathname), ExtractPackageName(Str1));
        if FConfiguration.IsInterface(Path) then
          Interfaces.Add(Str1)
        else
          Associations.Add(Str1);
      end;
    end;
  end;

  procedure MakeMenuItem(const Str1, Str2: string; ImageIndex: Integer);
  begin
    if Str1 = '' then
    begin
      var
      ASeparator := TSpTBXSeparatorItem.Create(FFrame.PopMenuClass);
      FFrame.PopMenuClass.Items.Insert(MenuIndex, ASeparator);
      Inc(MenuIndex);
    end
    else
    begin
      var
      AMenuItem := TSpTBXItem.Create(FFrame.PopMenuClass);
      AMenuItem.Caption := Str1;
      AMenuItem.Tag := InheritedLevel;
      if Str1 <> '' then
      begin
        if ImageIndex >= 7 then
          AMenuItem.OnClick := CallMethodForClass
        else
          AMenuItem.OnClick := CreateObjectForSelectedClass;
        AMenuItem.ImageIndex := ImageIndex;
        FFullParameters.Add(Str1 + '=' + Str2);
      end;
      if (InheritedLevel = 0) and (0 <= MenuIndex) and
        (MenuIndex < FFrame.PopMenuClass.Items.Count) then
      begin
        FFrame.PopMenuClass.Items.Insert(MenuIndex, AMenuItem);
        Inc(MenuIndex);
      end
      else
        AInheritedMenu.Add(AMenuItem);
    end;
  end;

  function MakeTestMenuItem(const Str1, Str2: string): TSpTBXItem;
  var
    AMenuItem: TSpTBXItem;
  begin
    AMenuItem := TSpTBXItem.Create(FFrame.PopMenuClass);
    AMenuItem.Caption := Str1;
    AMenuItem.OnClick := OnRunJunitTestMethod;
    AMenuItem.ImageIndex := 21;
    FFullParameters.Add(Str1 + '=' + Str2);
    Result := AMenuItem;
  end;

  procedure MakeSortedMenu(SLSorted: TStringList);
  var
    Error, Posi, Img: Integer;
    Str, Str1, Str2: string;
  begin
    for var I := 0 to SLSorted.Count - 1 do
    begin
      Str := SLSorted[I];
      Posi := Pos('#', Str);
      Delete(Str, 1, Posi);
      Posi := Pos('#', Str);
      Str1 := Copy(Str, 1, Posi - 1);
      Delete(Str, 1, Posi);
      Posi := Pos('#', Str);
      Str2 := Copy(Str, 1, Posi - 1);
      Delete(Str, 1, Posi);
      Val(Str, Img, Error);
      MakeMenuItem(Str1, Str2, Img);
    end;
  end;

  procedure MakeSystemInheritedMenus;
  var
    Img: Integer;
    Str, Str1, Str2, Str3, Str4: string;
    StringList, SLSorted: TStringList;
    Superclassname: string;
    AJavaClass: TComJavaClass;
  begin
    // go on with API-classes
    AJavaClass := FComJava.GetClass(AModelClass.Importname);
    if not Assigned(AJavaClass) then
      Exit;
    SLSorted := TStringList.Create;
    try
      SLSorted.Sorted := True;
      repeat
        StringList := AJavaClass.GetConstructors;
        for var I := 0 to StringList.Count - 1 do
        begin
          Str := StringList[I];
          Str := WithoutThrows(Str);
          Str1 := GetNextPart(Str);
          if IsVisibility(Str1) then
            Str1 := GetNextPart(Str);
          while IsModifier(Str1) do
            Str1 := GetNextPart(Str);
          Str1 := InsertParameterNames(Str1);
          Str2 := Str1;
          Str1 := GetShortTypeWith(Str1);
          MakeMenuItem(Str1, Str2, 2);
        end;
        StringList := AJavaClass.GetMethods('static');
        for var I := 0 to StringList.Count - 1 do
        begin
          Str := StringList[I];
          Str := WithoutThrows(Str);
          Str1 := GetNextPart(Str);
          Img := Visibility2ImageNumber(viPackage);
          if Str1 <> 'public' then
            Continue;

          if IsVisibility(Str1) then
          begin
            Img := Visibility2ImageNumber(String2Visibility(Str1));
            Str1 := GetNextPart(Str);
          end;
          while IsModifier(Str1) do
            Str1 := GetNextPart(Str);
          Str2 := Str1 + ' ' + Str;
          Str4 := GetNextPart(Str); // <T> T[] getListeners
          if Str = '' then
            Str := Str4
          else
            Str1 := Str1 + ' ' + Str4;
          Str3 := GetShortMethod(Str);
          Str1 := GetShortType(Str1) + ' ' + Str3;
          SLSorted.Add(Str3 + '#' + Str1 + '#' + Str2 + '#' + IntToStr(Img));
        end;
        MakeSortedMenu(SLSorted);
        SLSorted.Text := '';

        // empty inherited static methods menu
        if Assigned(AInheritedMenu) then
          if (InheritedLevel > 0) and (AInheritedMenu.Count = 0) then
          begin
            FreeAndNil(AInheritedMenu);
            Dec(MenuIndex);
          end
          else
          begin
            AInheritedMenu.Visible := AViewClass.ShowInherited;
            HasInheritedSystemMethods := True;
          end;

        Superclassname := AJavaClass.GetSuperclassName;
        if Superclassname = '' then
          Break
        else
        begin
          AJavaClass := FComJava.GetClass(Superclassname);
          if Assigned(AJavaClass) then
          begin
            AInheritedMenu := TSpTBXSubmenuItem.Create(FFrame.PopMenuObject);
            AInheritedMenu.Caption := 'Inherited from ' + AJavaClass.Name;
            FFrame.PopMenuClass.Items.Insert(MenuIndex, AInheritedMenu);
            Inc(MenuIndex);
          end;
        end;
      until False;
    finally
      FreeAndNil(SLSorted);
    end;
  end;

begin // PopMenuClassPopup
  ABox := FindVCLWindow(TSpTBXPopupMenu(Sender).PopupPoint);
  FPanel.ClearSelection;
  if Assigned(ABox) then
    FPanel.FindManagedControl(ABox).Selected := True
  else
    Exit;

  // delete previous menu
  for var I := FFrame.PopMenuClass.Items.Count - 1 downto 0 do
    if FFrame.PopMenuClass.Items[I].Tag = 0 then
      FreeAndNil(FFrame.PopMenuClass.Items[I]);
  for var I := FFrame.MIClassPopupRunOneTest.Count - 1 downto 0 do
    FreeAndNil(FFrame.MIClassPopupRunOneTest[I]);
  for var I := FFrame.MIClassPopupOpenClass.Count - 1 downto 0 do
    FreeAndNil(FFrame.MIClassPopupOpenClass[I]);
  for var I := FFrame.MIClassPopupConnect.Count - 1 downto 0 do
    FreeAndNil(FFrame.MIClassPopupConnect[I]);

  FFullParameters.Clear;
  InheritedLevel := 0;
  StartIndex := 12;
  MenuIndex := StartIndex;
  MakeMenuItem('', '', 0);
  Associations := TStringList.Create;
  Associations.Sorted := True;
  Associations.Duplicates := dupIgnore;
  Interfaces := TStringList.Create;
  Interfaces.Sorted := True;
  Interfaces.Duplicates := dupIgnore;
  Connections := TStringList.Create;
  Connections.Sorted := True;
  Connections.Duplicates := dupIgnore;
  SLSorted := TStringList.Create;
  SLSorted.Sorted := True;
  HasMain := False;
  HasInheritedSystemMethods := False;
  AViewClass := nil;
  AViewInterface := nil;

  if (ABox is TRtfdClass) and not TRtfdBox(ABox).IsJUnitTestclass then
  begin
    AViewClass := TRtfdClass(ABox);
    AModelClass := TClass(AViewClass.Entity);

    // get SuperClass
    SuperClass := AModelClass.Ancestor;
    if Assigned(SuperClass) then
      MakeOpenClassMenuItem(SuperClass.Fullname, 13);

    // get constructors
    if not AModelClass.IsAbstract and not MyJavaCommands.ProcessRunning then
    begin
      It1 := AModelClass.GetOperations;
      while It1.HasNext do
      begin
        Operation := UModel.TOperation(It1.Next);
        if Operation.OperationType = otConstructor then
        begin
          Str1 := Operation.Name + '(';
          Str2 := Str1;
          It2 := Operation.GetParameters;
          while It2.HasNext do
          begin
            Parameter := TParameter(It2.Next);
            if Assigned(Parameter.TypeClassifier) then
            begin
              Str1 := Str1 + Parameter.Name + ': ' +
                Parameter.TypeClassifier.GetShortType + ', ';
              Str2 := Str2 + Parameter.TypeClassifier.Name + ' ' +
                Parameter.Name + ', ';
              if InheritedLevel = 0 then
                AddDatatype(Parameter.TypeClassifier.Name);
            end;
          end;
          Str1 := ReplaceStr(Str1 + ')', ', )', ')');
          Str2 := ReplaceStr(Str2 + ')', ', )', ')');
          MakeMenuItem(Str1, Str2, 2);
        end
        else if (Operation.OperationType = otProcedure) and
          (Operation.Name = 'main') then
          HasMain := True;
      end;
      if MenuIndex = StartIndex + 1 then // default constructor
        MakeMenuItem(AModelClass.GetShortType + '()', AModelClass.GetShortType
          + '()', 2);
    end;
    // get static methods and Parameterclasses
    repeat
      It1 := AModelClass.GetOperations;
      while It1.HasNext and not MyJavaCommands.ProcessRunning do
      begin
        Operation := UModel.TOperation(It1.Next);
        if Operation.OperationType in [otFunction, otProcedure] then
        begin
          Str1 := Operation.Name + '(';
          Str2 := Str1;
          It2 := Operation.GetParameters;
          while It2.HasNext do
          begin
            Parameter := TParameter(It2.Next);
            if Assigned(Parameter.TypeClassifier) then
            begin
              Str1 := Str1 + Parameter.Name + ': ' +
                Parameter.TypeClassifier.GetShortType + ', ';
              Str2 := Str2 + Parameter.TypeClassifier.Name + ' ' +
                Parameter.Name + ', ';
              if InheritedLevel = 0 then
                AddDatatype(Parameter.TypeClassifier.Name);
            end;
          end;
          if not Operation.Static then
            Continue;
          Str1 := ReplaceStr(Str1 + ')', ', )', ')');
          Str2 := ReplaceStr(Str2 + ')', ', )', ')');
          case Operation.OperationType of
            otFunction:
              begin
                Str1 := Str1 + ': ' + Operation.ReturnValue.GetShortType;
                Str2 := Operation.ReturnValue.Name + ' ' + Str2;
                if InheritedLevel = 0 then
                  AddDatatype(Operation.ReturnValue.Name);
              end;
            otProcedure:
              begin
                Str2 := 'void ' + Str2;
              end;
          end;
          if Operation.Static then
          begin
            Str2 := 'static ' + Str2;
            MakeMenuItem(Str1, Str2, Integer(Operation.Visibility) + 7);
          end;
        end;
      end;

      // empty inherited static methods menu
      if (InheritedLevel > 0) and (AInheritedMenu.Count = 0) then
      begin
        FreeAndNil(AInheritedMenu);
        Dec(MenuIndex);
      end;

      // get Association classes
      if InheritedLevel = 0 then
      begin
        It1 := AModelClass.GetAttributes;
        while It1.HasNext do
        begin
          Attribute := UModel.TAttribute(It1.Next);
          if Assigned(Attribute.TypeClassifier) then
            AddDatatype(Attribute.TypeClassifier.Importname);
        end;

        It1 := AModelClass.GetImplements;
        while It1.HasNext do
        begin
          AInterface := UModel.TInterface(It1.Next);
          AddDatatype(AInterface.Fullname);
        end;
      end;

      // get inherited static methods
      AModelClass := AModelClass.Ancestor;
      if Assigned(AModelClass) and not MyJavaCommands.ProcessRunning then
      begin
        // if InheritedLevel = 0 then MakeMenuItem('-', '-', 0);
        Inc(InheritedLevel);
        AInheritedMenu := TSpTBXSubmenuItem.Create(FFrame.PopMenuClass);
        AInheritedMenu.Caption := 'Inherited from ' + AModelClass.Name;
        FFrame.PopMenuClass.Items.Insert(MenuIndex, AInheritedMenu);
        Inc(MenuIndex);
        if FConfiguration.IsAPIClassOrInterface(AModelClass.Importname) then
        begin
          MakeSystemInheritedMenus;
          Break;
        end;
      end
      else
        AModelClass := nil;
    until not Assigned(AModelClass);
  end;

  { --- JUnit-Tests ------------------------------------------------------------ }
  if FConfiguration.JUnitOk and TRtfdBox(ABox).IsJUnitTestclass and
    not MyJavaCommands.ProcessRunning then
  begin
    AViewClass := TRtfdClass(ABox);
    AModelClass := TClass(AViewClass.Entity);
    // get test methods
    It1 := AModelClass.GetOperations;
    MenuIndex := 1;
    while It1.HasNext do
    begin
      Operation := UModel.TOperation(It1.Next);
      if (Operation.Annotation <> 'Test') and
        (Operation.Annotation <> 'ParameterizedTest') then
        Continue;
      if Operation.OperationType in [otFunction, otProcedure] then
      begin
        Str1 := Operation.Name + '(';
        It2 := Operation.GetParameters;
        while It2.HasNext do
        begin
          Parameter := TParameter(It2.Next);
          if Assigned(Parameter.TypeClassifier) then
          begin
            Str1 := Str1 + Parameter.Name + ': ' +
              Parameter.TypeClassifier.GetShortType + ', ';
            if InheritedLevel = 0 then
              AddDatatype(Parameter.TypeClassifier.Name);
          end;
        end;
        Str1 := ReplaceStr(Str1 + ')', ', )', ')');
        SLSorted.Add(Str1 + '#' + Operation.Name);
      end;
    end;

    for var I := 0 to SLSorted.Count - 1 do
    begin
      Posi := Pos('#', SLSorted[I]);
      MethodWithParam := Copy(SLSorted[I], 1, Posi - 1);
      MethodNoParam := Copy(SLSorted[I], Posi + 1, 255);
      AMenuItem := MakeTestMenuItem(MethodWithParam, MethodNoParam);
      if SLSorted.Count > 3 then
        FFrame.MIClassPopupRunOneTest.Add(AMenuItem)
      else
      begin
        FFrame.PopMenuClass.Items.Insert(MenuIndex, AMenuItem);
        Inc(MenuIndex);
      end;
    end;
  end;

  // --- Interface -------------------------------------------------
  if ABox is TRtfdInterface then
  begin
    AViewInterface := TRtfdInterface(ABox);
    AModelInterface := TInterface(AViewInterface.Entity);

    // get SuperInterface
    SuperInterface := AModelInterface.Ancestor;
    if Assigned(SuperInterface) then
      MakeOpenClassMenuItem(SuperInterface.Name, 13);

    // get Parameterclasses
    It1 := AModelInterface.GetOperations;
    while It1.HasNext do
    begin
      Operation := UModel.TOperation(It1.Next);
      if Operation.OperationType in [otFunction, otProcedure] then
      begin
        It2 := Operation.GetParameters;
        while It2.HasNext do
        begin
          Parameter := TParameter(It2.Next);
          if Assigned(Parameter.TypeClassifier) and (InheritedLevel = 0) then
            AddDatatype(Parameter.TypeClassifier.Name);
        end;
        if (Operation.OperationType = otFunction) and (InheritedLevel = 0) then
          AddDatatype(Operation.ReturnValue.Name);
      end;
    end;

    It1 := AModelInterface.GetImplementingClasses;
    while It1.HasNext do
    begin
      AModelClass := UModel.TClass(It1.Next);
      AddDatatype(AModelClass.Importname);
    end;

    // get association interfaces
    It1 := AModelInterface.GetAttributes;
    while It1.HasNext do
    begin
      Attribute := UModel.TAttribute(It1.Next);
      AddDatatype(Attribute.TypeClassifier.Name);
    end;
  end;

  for var I := 0 to Associations.Count - 1 do
    MakeOpenClassMenuItem(Associations[I], 0);
  for var I := 0 to Interfaces.Count - 1 do
    MakeOpenClassMenuItem(Interfaces[I], 16);
  MakeConnectClassMenuItems;

  InheritedLevel := 0;
  Inc(MenuIndex, 2);
  MakeMenuItem('', '', 0);

  Str1 := ChangeFileExt(TRtfdBox(ABox).GetPathname, '.java');
  NoSystemClass := (Pos(FConfiguration.JavaCache, Str1) = 0);
  HasSourcecode := FileExists(Str1);

  FFrame.MIClassPopupCompileJava.Enabled := HasSourcecode and NoSystemClass and
    FConfiguration.JavaCompilerOK and not MyJavaCommands.ProcessRunning;
  FFrame.MIClassPopupRun.Enabled := HasMain and
    not MyJavaCommands.ProcessRunning;

  FFrame.MIClassPopupClassEdit.Enabled := (ABox is TRtfdClass) and
    HasSourcecode and NoSystemClass and not MyJavaCommands.ProcessRunning;
  FFrame.MIClassPopupInterfaceEdit.Visible := (ABox is TRtfdInterface) and
    HasSourcecode and NoSystemClass and not MyJavaCommands.ProcessRunning;
  FFrame.MIClassPopupOpenSource.Visible := HasSourcecode; // in Arbeit
  FFrame.MIClassPopupOpenClass.Visible :=
    (FFrame.MIClassPopupOpenClass.Count > 0);
  FFrame.MIClassPopupConnect.Visible := (FFrame.MIClassPopupConnect.Count > 0);
  if ABox is TRtfdClass then
    FFrame.MIClassPopupDelete.Caption := _('Delete class')
  else
    FFrame.MIClassPopupDelete.Caption := _('Delete interface');

  FFrame.MIClassPopupShowInherited.Visible := not TRtfdBox(ABox)
    .ShowInherited and HasInheritedSystemMethods;
  FFrame.MIClassPopupHideInherited.Visible := TRtfdBox(ABox)
    .ShowInherited and HasInheritedSystemMethods;
  if FConfiguration.JUnitOk and not MyJavaCommands.ProcessRunning then
  begin
    FFrame.MIClassPopupRunAllTests.Visible := TRtfdBox(ABox)
      .IsJUnitTestclass;
    FFrame.MIClassPopupRunOneTest.Visible := (SLSorted.Count > 3);
    FFrame.MIClassPopupCreateTestClass.Visible := not TRtfdBox(ABox)
      .IsJUnitTestclass;
  end
  else
  begin
    FFrame.MIClassPopupRunAllTests.Visible := False;
    FFrame.MIClassPopupRunOneTest.Visible := False;
    FFrame.MIClassPopupCreateTestClass.Visible := False;
  end;

  for var I := 0 to FFrame.MIClassPopupDisplay.Count - 1 do
    FFrame.MIClassPopupDisplay[I].Checked := False;
  if Assigned(AViewClass) or Assigned(AViewInterface) then
  begin
    if Assigned(AViewClass) then
      Num := 4 - Ord(AViewClass.MinVisibility)
    else
      Num := 4 - Ord(AViewInterface.MinVisibility);
    FFrame.MIClassPopupDisplay[Num].Checked := True;
  end;

  for var I := 0 to FFrame.MIClassPopupParameter.Count - 1 do
    FFrame.MIClassPopupParameter[I].Checked := False;
  if Assigned(AViewClass) or Assigned(AViewInterface) then
  begin
    if Assigned(AViewClass) then
      Num := AViewClass.ShowParameter
    else
      Num := AViewInterface.ShowParameter;
    FFrame.MIClassPopupParameter[Num].Checked := True;
  end;

  for var I := 0 to FFrame.MIClassPopupVisibility.Count - 1 do
    FFrame.MIClassPopupVisibility[I].Checked := False;
  if Assigned(AViewClass) or Assigned(AViewInterface) then
  begin
    if Assigned(AViewClass) then
      Num := 2 - AViewClass.ShowIcons
    else
      Num := 2 - AViewInterface.ShowIcons;
    FFrame.MIClassPopupVisibility[Num].Checked := True;
  end;
  FreeAndNil(Associations);
  FreeAndNil(Interfaces);
  FreeAndNil(Connections);
  FreeAndNil(SLSorted);
end; // PopMenuClassPopup

procedure TRtfdDiagram.PopMenuObjectPopup(Sender: TObject);
var
  Str1, Str2, Str3, LongType, Ancest, Objectname: string;
  AControl: TControl;
  InheritedLevel, MenuIndex: Integer;
  AObjectBox: TRtfdObject;
  AViewClass: TRtfdClass;
  AModelClass: TClass;
  AModelClassRoot: TClass;
  AJavaClass: TComJavaClass;
  AJavaObject: TComJavaObject;
  It1, It2: IModelIterator;
  Operation: UModel.TOperation;
  Parameter: TParameter;
  AInheritedMenu: TSpTBXItem;
  HasInheritedSystemMethods: Boolean;
  SLSorted: TStringList;
  AMenuItem: TSpTBXItem;

  procedure MakeMenuItem(const Str1, Str2: string; ImageIndex: Integer);
  begin
    if Str1 = '' then
    begin
      var
      ASeparator := TSpTBXSeparatorItem.Create(FFrame.PopMenuObject);
      FFrame.PopMenuObject.Items.Insert(MenuIndex, ASeparator);
      Inc(MenuIndex);
    end
    else
    begin
      var
      AMenuItem := TSpTBXItem.Create(FFrame.PopMenuObject);
      AMenuItem.Caption := Str1;
      AMenuItem.OnClick := CallMethodForObject;
      AMenuItem.ImageIndex := ImageIndex;
      FFullParameters.Add(Str1 + '=' + Str2);
      if InheritedLevel = 0 then
      begin
        FFrame.PopMenuObject.Items.Insert(MenuIndex, AMenuItem);
        Inc(MenuIndex);
      end
      else
      begin
        AMenuItem.Tag := InheritedLevel;
        AInheritedMenu.Add(AMenuItem);
      end;
    end;
  end;

  procedure MakeSortedMenu(SLSorted: TStringList);
  var
    Error, Posi, Img: Integer;
    Str, Str1, Str2: string;
  begin
    for var I := 0 to SLSorted.Count - 1 do
    begin
      Str := SLSorted[I];
      Posi := Pos('#', Str);
      Delete(Str, 1, Posi);
      Posi := Pos('#', Str);
      Str1 := Copy(Str, 1, Posi - 1);
      Delete(Str, 1, Posi);
      Posi := Pos('#', Str);
      Str2 := Copy(Str, 1, Posi - 1);
      Delete(Str, 1, Posi);
      Val(Str, Img, Error);
      MakeMenuItem(Str1, Str2, Img);
    end;
  end;

  procedure MakeSystemInheritedMenus(AClassname: string;
    WithInherited: Boolean);
  var
    Img: Integer;
    StringList, SLSorted: TStringList;
    Str, Str1, Str2, Str3, Str4, Superclassname: string;
  begin
    // go on with API-classes
    AJavaClass := FComJava.GetClass(AClassname);
    SLSorted := TStringList.Create;
    SLSorted.Sorted := True;
    if Assigned(AJavaClass) and AJavaClass.IsValid then
      repeat
        StringList := AJavaClass.GetMethods('not static');
        for var I := 0 to StringList.Count - 1 do
        begin
          Str := StringList[I];
          Str := WithoutThrows(Str);
          Str1 := GetNextPart(Str);
          Img := Visibility2ImageNumber(viPackage);
          if IsVisibility(Str1) then
          begin
            if Str1 <> 'public' then
              Continue;
            Img := Visibility2ImageNumber(String2Visibility(Str1));
            Str1 := GetNextPart(Str);
          end;
          while IsModifier(Str1) do
            Str1 := GetNextPart(Str);
          Str2 := Str1 + ' ' + Str;
          Str4 := GetNextPart(Str); // <T> T[] getListeners
          if Str = '' then
            Str := Str4
          else
            Str1 := Str1 + ' ' + Str4;
          Str3 := GetShortMethod(Str);
          if Str1 = 'void' then
            Str1 := Str3
          else
            Str1 := Str3 + ': ' + GetShortType(Str1); // + ' ' + s3;
          SLSorted.Add(Str3 + '#' + Str1 + '#' + Str2 + '#' + IntToStr(Img));
        end;
        MakeSortedMenu(SLSorted);
        SLSorted.Text := '';

        // empty inherited static methods menu
        if Assigned(AInheritedMenu) then
          if (InheritedLevel > 0) and (AInheritedMenu.Count = 0) then
          begin
            FreeAndNil(AInheritedMenu);
            Dec(MenuIndex);
          end
          else
          begin
            AInheritedMenu.Visible := AObjectBox.ShowInherited;
            HasInheritedSystemMethods := True;
          end;
        Superclassname := AJavaClass.GetSuperclassName;
        if WithInherited and (Superclassname <> '') then
        begin
          AJavaClass := FComJava.GetClass(Superclassname);
          if Assigned(AJavaClass) then
          begin
            AInheritedMenu := TSpTBXSubmenuItem.Create(FFrame.PopMenuObject);
            AInheritedMenu.Caption := 'Inherited from ' + AJavaClass.ImportTyp;
            FFrame.PopMenuObject.Items.Insert(MenuIndex, AInheritedMenu);
            Inc(MenuIndex);
          end;
        end
        else
          Break;
      until False;
    FreeAndNil(SLSorted);
  end;

  procedure MakeShowUnnamedMenu;
  var
    SL1, SL2: TStringList;
    AMenuItem: TSpTBXItem;

    function NotShown(const Str, Text: string): Boolean;
    begin
      if FBoxNames.IndexOf(Str) >= 0 then
        Exit(False);
      Result := (Str <> '') and (Str <> 'null') and
        (FBoxNames.IndexOf(Str) = -1) and (SL2.IndexOf(Str) = -1) and
        (Pos('.awt.', Text) = 0) and (Pos('.swing.', Text) = 0) and
        (Pos('.lang.', Text) = 0) and (Pos('NumberField', Text) = 0);
    end;

    procedure PrepareMenu(Str: string);
    var
      Attr, Typ: string;
      Posi: Integer;
      StringList: TStringList;
    begin
      if Pos('{', Str) + Pos('[', Str) = 1 then
        Str := Copy(Str, 2, Length(Str) - 2);
      StringList := Split(',', Str);
      for var I := 0 to StringList.Count - 1 do
      begin
        Str := Trim(StringList[I]);
        Posi := Pos('=', Str);
        if Posi > 0 then
        begin
          Typ := Copy(Str, Posi + 1, Length(Str));
          Attr := Copy(Str, 1, Posi - 1);
        end
        else
        begin
          Typ := '';
          Attr := Str;
        end;
        if NotShown(Attr, Typ) then
        begin
          SL2.Add(Attr);
          FFullParameters.Add(Str);
        end;
      end;
      FreeAndNil(StringList);
    end;

    function MakeMenuItem(const Str: string; Count: Integer): TSpTBXItem;
    begin
      var
      AMenuItem := TSpTBXItem.Create(FFrame.MIObjectPopUpShowNewObject);
      if Count > 3 then
        AMenuItem.Caption := Str
      else
        AMenuItem.Caption := FFrame.MIObjectPopUpShowNewObject.Caption +
          ' ' + Str;
      AMenuItem.OnClick := ShowUnnamedObject;
      AMenuItem.ImageIndex := 19;
      AMenuItem.Tag := -2; // only unnamed menuitems
      Result := AMenuItem;
    end;

  begin
    FFrame.MIObjectPopUpShowNewObject.Visible := False;
    FFrame.MIObjectPopupShowAllNewObjects.Visible := False;
    SL1 := AJavaObject.GetObjectAttributeValues(True);
    SL2 := TStringList.Create;
    for var I := 0 to SL1.Count - 1 do
      PrepareMenu(SL1[I]);
    for var I := 0 to SL2.Count - 1 do
    begin
      AMenuItem := MakeMenuItem(SL2[I], SL2.Count);
      if SL2.Count > 3 then
        FFrame.MIObjectPopUpShowNewObject.Add(AMenuItem)
      else
      begin
        FFrame.PopMenuObject.Items.Insert(MenuIndex - 1, AMenuItem);
        Inc(MenuIndex);
      end;
    end;
    FFrame.MIObjectPopUpShowNewObject.Visible := (SL2.Count > 3);
    FFrame.MIObjectPopupShowAllNewObjects.Visible := (SL2.Count > 1);
    FreeAndNil(SL2);
    FreeAndNil(SL1);
  end;

begin // PopMenuObjectPopup
  AControl := FindVCLWindow(TSpTBXPopupMenu(Sender).PopupPoint);
  FPanel.ClearSelection;
  if Assigned(AControl) then
    FPanel.FindManagedControl(AControl).Selected := True
  else
    Exit;

  // delete previous menu
  for var I := FFrame.PopMenuObject.Items.Count - 1 downto 0 do
    if FFrame.PopMenuObject.Items[I].Tag <= 0 then
    begin
      AMenuItem := TSpTBXItem(FFrame.PopMenuObject.Items[I]);
      FreeAndNil(AMenuItem);
    end;
  for var I := FFrame.MIObjectPopUpShowNewObject.Count - 1 downto 0 do
  begin
    AMenuItem := TSpTBXItem(FFrame.MIObjectPopUpShowNewObject[I]);
    FreeAndNil(AMenuItem);
  end;

  FFullParameters.Clear;

  if Assigned(AControl) and (AControl is TRtfdObject) then
  begin
    Objectname := TRtfdObject(AControl).Entity.Fullname;
    AJavaObject := FComJava.GetObject(Objectname);
    if not Assigned(AJavaObject) then
      Exit;
    AJavaClass := AJavaObject.ClassRef;
    LongType := AJavaClass.Name;
    InheritedLevel := 0;
    MenuIndex := 4; // edit, show, hide, show final, hide final, open class
    MakeShowUnnamedMenu;
    MakeMenuItem('', '', 0);

    // get model class for object to get the methods
    AObjectBox := TRtfdObject(GetBox(Objectname));
    AViewClass := TRtfdClass(GetBox(LongType));
    if not Assigned(AViewClass) then
      AViewClass := TRtfdClass(GetBox(GetShortType(LongType)));
    if Assigned(AViewClass) then
      AModelClass := TClass(AViewClass.Entity)
      // model class from view class
    else
      AModelClass := nil;
    if not Assigned(AModelClass) then
      AModelClass := GetModelClass(LongType); // model class from model
    if not Assigned(AModelClass) or (AModelClass.Pathname = '') then
      AModelClass := CreateModelClass(LongType); // model class from java
    AModelClassRoot := AModelClass;

    if Assigned(AModelClass) then
    begin // known Class in Model
      SLSorted := TStringList.Create;
      SLSorted.Sorted := True;
      repeat
        It1 := AModelClass.GetOperations;
        while It1.HasNext do
        begin
          Operation := UModel.TOperation(It1.Next);
          if (InheritedLevel > 0) and (Operation.Visibility = viPrivate) then
            Continue;
          if (Operation.OperationType in [otProcedure, otFunction]) and
            not Operation.IsAbstract then
          begin
            Str1 := Operation.Name + '(';
            Str2 := Str1;
            It2 := Operation.GetParameters;
            while It2.HasNext do
            begin
              Parameter := TParameter(It2.Next);
              try
                if Assigned(Parameter.TypeClassifier) then
                begin
                  Str1 := Str1 + Parameter.TypeClassifier.GetShortType;
                  if Parameter.Name <> '' then
                    Str1 := Str1 + ' ' + Parameter.Name;
                  Str1 := Str1 + ', ';
                  Str2 := Str2 + Parameter.TypeClassifier.Name;
                  if Parameter.Name <> '' then
                    Str2 := Str2 + ' ' + Parameter.Name;
                  Str2 := Str2 + ', ';
                end;
              except
                on E: Exception do
                begin
                  if Assigned(Parameter.TypeClassifier) then
                    Str3 := ' Parameter.TypeClassifier assigned '
                  else
                    Str3 := ' Parameter.TypeClassifier nil ';
                  Str3 := Str3 + 'Parameter.Name = ' + Parameter.Name;
                  FConfiguration.Log(Str3, E);
                end;
              end;
            end;
            Str1 := ReplaceStr(Str1 + ')', ', )', ')');
            Str2 := ReplaceStr(Str2 + ')', ', )', ')');
            if Operation.OperationType = otFunction then
            begin
              if Assigned(Operation.ReturnValue) then
                Str1 := Str1 + ': ' + Operation.ReturnValue.GetShortType
                // + ' ' + s1;
              else
              begin
                Str1 := Str1 + ': <unknown>'; // + ' ' +
                FConfiguration.Log
                  ('TRtfdDiagram.PopMenuObjectPopup: Operation.ReturnValue = nil');
              end;
              Str2 := Operation.ReturnValue.Name + ' ' + Str2;
            end
            else
            begin
              Str2 := 'void ' + Str2;
            end;
            if Operation.Static then
            begin
              Str2 := 'static ' + Str2;
            end;
            // Operation.Name is sort-criteria, s1 is Menu.caption, s2 is
            SLSorted.Add(Operation.Name + '#' + Str1 + '#' + Str2 + '#' +
              IntToStr(Integer(Operation.Visibility) + 7));
          end;
        end;
        // end of while - all methods handelt

        MakeSortedMenu(SLSorted);
        SLSorted.Text := '';

        // empty inherited methods menu
        if (InheritedLevel > 0) and (AInheritedMenu.Count = 0) then
        begin
          FreeAndNil(AInheritedMenu);
          Dec(MenuIndex);
        end;

        if Assigned(AModelClass.Ancestor) then
        begin
          Ancest := AModelClass.Ancestor.Name;
          AModelClass := GetModelClass(Ancest);
          if not Assigned(AModelClass) then
            AModelClass := CreateModelClass(Ancest);
        end
        else
          AModelClass := nil;

        if Assigned(AModelClass) then
        begin
          Inc(InheritedLevel);
          AInheritedMenu := TSpTBXSubmenuItem.Create(FFrame.PopMenuObject);
          AInheritedMenu.Caption := 'Inherited from ' + AModelClass.Name;
          FFrame.PopMenuObject.Items.Insert(MenuIndex, AInheritedMenu);
          Inc(MenuIndex);
          if FConfiguration.IsAPIClassOrInterface(AModelClass.Importname) then
          begin
            MakeSystemInheritedMenus(AModelClass.Importname, True);
            Break;
          end;
        end
        else
          Break;
      until False;
      FreeAndNil(SLSorted);
    end;

    InheritedLevel := 0;
    Inc(MenuIndex, 1);
    MakeMenuItem('', '', 0);
    if Assigned(AModelClassRoot) then
      FFrame.MIObjectPopupEdit.Visible := HasAttributes(AModelClassRoot)
    else
      FFrame.MIObjectPopupEdit.Visible := False;
    FFrame.MIObjectPopupOpenClass.Visible := not Assigned(AViewClass);
    if Assigned(AObjectBox) then
    begin
      FFrame.MIObjectPopupShowInherited.Visible :=
        not AObjectBox.ShowInherited and HasInheritedSystemMethods;
      FFrame.MIObjectPopupHideInherited.Visible := AObjectBox.ShowInherited and
        HasInheritedSystemMethods;
    end;
    for var I := 0 to FFrame.MIObjectPopupDisplay.Count - 1 do
      FFrame.MIObjectPopupDisplay[I].Checked := False;
    var Num := 4 - Ord(AObjectBox.MinVisibility);
    FFrame.MIObjectPopupDisplay[Num].Checked := True;

    for var I := 0 to FFrame.MIObjectPopupVisibility.Count - 1 do
      FFrame.MIObjectPopupVisibility[I].Checked := False;
    Num := 2 - AObjectBox.ShowIcons;
    FFrame.MIObjectPopupVisibility[Num].Checked := True;
  end;
end; // PopMenuObjectPopup

procedure TRtfdDiagram.PopMenuConnectionPopup(Sender: TObject);
var
  Conn: TConnection;
  BothClassOrInterface, AClassAInterface, AClassAObject: Boolean;
begin
  Conn := FPanel.GetClickedConnection;
  if not Assigned(Conn) then
    Exit;

  BothClassOrInterface := ((Conn.FromControl is TRtfdClass) and
    (Conn.ToControl is TRtfdClass)) or ((Conn.FromControl is TRtfdInterface) and
    (Conn.ToControl is TRtfdInterface));
  AClassAInterface := ((Conn.FromControl is TRtfdClass) and
    (Conn.ToControl is TRtfdInterface)) or
    ((Conn.FromControl is TRtfdInterface) and (Conn.ToControl is TRtfdClass));
  AClassAObject := ((Conn.FromControl is TRtfdClass) and
    (Conn.ToControl is TRtfdObject)) or ((Conn.FromControl is TRtfdObject) and
    (Conn.ToControl is TRtfdClass));
  FFrame.MIConnectionAssoziation.Visible := BothClassOrInterface;
  FFrame.MIConnectionAssoziationArrow.Visible := BothClassOrInterface;
  FFrame.MIConnectionAssoziationBidirectional.Visible := BothClassOrInterface;
  FFrame.MIConnectionAggregation.Visible := BothClassOrInterface;
  FFrame.MIConnectionAggregationArrow.Visible := BothClassOrInterface;
  FFrame.MIConnectionComposition.Visible := BothClassOrInterface;
  FFrame.MIConnectionCompositionArrow.Visible := BothClassOrInterface;
  FFrame.MIConnectionInheritance.Visible := not Conn.IsRecursiv and
    BothClassOrInterface;
  FFrame.MIConnectionImplements.Visible := AClassAInterface;
  FFrame.MIConnectionInstanceOf.Visible := AClassAObject;
  FFrame.MIConnectionRecursiv.Visible := Conn.IsRecursiv;
end;

function TRtfdDiagram.ClassHasObjects(ABox: TRtfdBox): Boolean;
begin
  Result := True;
  for var I := 0 to FBoxNames.Count - 1 do
    if FBoxNames.Objects[I] is TRtfdObject then
      if GetShortType(TRtfdObject(FBoxNames.Objects[I]).GetClassname)
        = ABox.Entity.Name then
        Exit;
  Result := False;
end;

function TRtfdDiagram.HasAttributes(AModelClass: TClass): Boolean;
begin
  var
  I := 0;
  while (I = 0) and Assigned(AModelClass) do
  begin
    var
    Ite := AModelClass.GetAttributes;
    while Ite.HasNext do
    begin
      var
      AItAttribut := TAttribute(Ite.Next);
      if FConfiguration.PrivateAttributEditable or
        (AItAttribut.Visibility <> viPrivate) then
        Inc(I);
    end;
    AModelClass := AModelClass.Ancestor;
  end;
  Result := (I > 0);
end;

function TRtfdDiagram.HasEditableClass: Boolean;
begin
  Result := False;
  var
  Control := FPanel.GetFirstSelected;
  if not Assigned(Control) then
    Control := FPanel.GetFirstManaged;

  if Assigned(Control) and (Control is TRtfdClass) and
    Assigned(GetBox(TRtfdClass(Control).Entity.Fullname)) then
  begin
    var
    Str := ChangeFileExt(TRtfdBox(Control).GetPathname, '.java');
    Result := (Length(Str) > 0) and not FConfiguration.PathForSystemClass(Str)
      and FileExists(Str);
  end;
end;

function TRtfdDiagram.HasSelectedControl: Boolean;
var
  Control: TControl;
begin
  Control := FPanel.GetFirstSelected;
  Result := Assigned(Control);
end;

function TRtfdDiagram.HasSelectedConnection: Boolean;
begin
  Result := FPanel.HasSelectedConnection;
end;

procedure TRtfdDiagram.OpenClassOrInterface(Sender: TObject);
begin
  var
  CName := TSpTBXItem(Sender).Caption;
  try
    Screen.Cursor := crHourGlass;
    TFUMLForm(FFrame.Parent.Owner).AddClassToProject(CName);
    FPanel.ClearSelection;
  finally
    Screen.Cursor := crDefault;
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
  if Assigned(FUMLForm) then
    FUMLForm.TBClassInsertClick(nil);
end;

procedure TRtfdDiagram.ShowUnnamedObject(Sender: TObject);
var
  ObjName: string;
  Posi: Integer;
  AJavaObject: TComJavaObject;
begin
  try
    LockWindow(FUMLForm.Handle);
    ObjName := TSpTBXItem(Sender).Caption;
    Posi := Pos(' ', ObjName);
    while Posi > 0 do
    begin
      Delete(ObjName, 1, Posi);
      Posi := Pos(' ', ObjName);
    end;
    AJavaObject := FComJava.NewUnnamedObject(ObjName);
    if AJavaObject.IsValid then
    begin
      ShowNewObject(AJavaObject);
      ResolveObjectAssociations;
      UpdateAllObjects;
    end
    else
      FreeAndNil(AJavaObject);
  finally
    UnlockWindow;
  end;
end;

procedure TRtfdDiagram.ShowObject(const ObjName: string);
begin
  var
  AJavaObject := FComJava.GetObject(ObjName);
  if Assigned(AJavaObject) then
    ShowNewObject(AJavaObject);
end;

procedure TRtfdDiagram.ShowAllNewObjects(Sender: TObject);
var
  Int: Integer;
begin
  try
    LockFormUpdate(FUMLForm);
    Screen.Cursor := crHourGlass;
    if FFrame.MIObjectPopUpShowNewObject.Visible then
      for var I := 0 to FFrame.MIObjectPopUpShowNewObject.Count - 1 do
        ShowUnnamedObject(FFrame.MIObjectPopUpShowNewObject[I])
    else
    begin
      Int := 2;
      while Int < FFrame.PopMenuObject.Items.Count - 1 do
      begin
        if FFrame.PopMenuObject.Items[Int].Tag = -2 then
          ShowUnnamedObject(FFrame.PopMenuObject.Items[Int]);
        Inc(Int);
      end;
    end;
    ResolveObjectAssociations;
  finally
    UnlockFormUpdate(FUMLForm);
    Screen.Cursor := crDefault;
  end;
end;

procedure TRtfdDiagram.ShowAllNewObjectsString(From: string = '');
var
  AJavaObject: TComJavaObject;
  AJavaObject2: TComJavaObject;
  Int, Posi: Integer;
  NewObj, NewObj2, Typ: string;
  StringList1, StringList2: TStringList;

  function NotShown(Str, Text: string): Boolean;
  begin
    if FBoxNames.IndexOf(Str) >= 0 then
      Exit(False);
    Result := True;
    if StartsWith(Text, 'Ljava.util.') then
      Exit;
    Result := (Str <> '') and (Str <> 'null') and (FBoxNames.IndexOf(Str) = -1)
      and not StartsWith(Text, 'Ljava.') and not StartsWith(Text, 'Lsun.') and
      not StartsWith(Text, 'Ljavax.') and (Pos('NumberField', Text) = 0);
    if Pos('Ljava.lang.', Text) = 1 then
    begin
      Delete(Text, 1, 11);
      if (Text = 'Boolean;') or (Text = 'Byte;') or (Text = 'Character;') or
        (Text = 'Double;') or (Text = 'Float;') or (Text = 'Integer;') or
        (Text = 'Long;') or (Text = 'Short;') or (Text = 'String;') then
        Result := True;
    end;
  end;

begin
  Int := 0;
  while Int < FComJava.ObjectList.Count do
  begin
    AJavaObject := TComJavaObject(FComJava.ObjectList.Objects[Int]);
    StringList1 := AJavaObject.GetObjectAttributeValues(True);
    for var J := 0 to StringList1.Count - 1 do
    begin
      NewObj := Trim(StringList1[J]);
      if NewObj = '' then
        Continue;
      if Pos('{', NewObj) + Pos('[', NewObj) = 1 then
        NewObj := Copy(NewObj, 2, Length(NewObj) - 2);
      StringList2 := Split(',', NewObj);
      for var K := 0 to StringList2.Count - 1 do
      begin
        NewObj2 := Trim(StringList2[K]);
        Typ := '';
        Posi := Pos('=', NewObj2);
        if Posi > 0 then
        begin
          Typ := Copy(NewObj2, Posi + 1, Length(NewObj2));
          NewObj2 := Copy(NewObj2, 1, Posi - 1);
        end;

        if NotShown(NewObj2, Typ) then
        begin
          AJavaObject2 := FComJava.NewUnnamedObject(NewObj2);
          if AJavaObject2.IsValid then
          begin
            if From = '' then
              From := 'Actor';
            ShowMethodEntered('<init>', From, AJavaObject2.Name, '');
            ShowNewObject(AJavaObject2);
            FInteractive.Executer.AddVariable(NewObj,
              AJavaObject2.ClassRef.Name, AJavaObject2.CreateValue);
          end
          else
            FreeAndNil(AJavaObject2);
        end;
      end;
      FreeAndNil(StringList2);
    end;
    FreeAndNil(StringList1);
    Inc(Int);
  end;
  ResolveObjectAssociations;
  UpdateAllObjects;
  FPanel.RecalcSize;
end;

procedure TRtfdDiagram.ConnectBoxes(Sender: TObject);
var
  CName: string;
  Src: TControl;
  Dest: TRtfdBox;
  UMLForm: TFUMLForm;
begin
  Src := FPanel.GetFirstSelected;
  if Assigned(Src) and (Src is TRtfdBox) then
  begin
    if Src.Owner.Owner is TFUMLForm then
      UMLForm := TFUMLForm(Src.Owner.Owner)
    else
      UMLForm := nil;
    LockFormUpdate(UMLForm);
    CName := TSpTBXItem(Sender).Caption;
    Dest := GetBox(CName);
    FPanel.FindManagedControl(Dest).Selected := True;
    FPanel.ConnectBoxes(Src, Dest);
    UnlockFormUpdate(UMLForm);
  end;
end;

procedure TRtfdDiagram.DoConnection(Item: Integer);
begin
  FPanel.DoConnection(Item);
end;

procedure TRtfdDiagram.DoAlign(Item: Integer);
begin
  FPanel.DoAlign(Item);
end;

procedure TRtfdDiagram.SetInteractive(AUMLForm: TForm; Path: string;
  OnInteractiveModified: TNotifyEvent);
begin
  FInteractive := FMessages.AddInteractive(AUMLForm, Path);
  FInteractivePath := Path;
  FComJava := FInteractive.ComJava;
  FOnModified := OnInteractiveModified;
end;

procedure TRtfdDiagram.SetFormMouseDown(OnFormMouseDown: TNotifyEvent);
begin
  FPanel.OnFormMouseDown := OnFormMouseDown;
end;

function TRtfdDiagram.GetComJava: TComJava1;
begin
  Result := FComJava;
end;

procedure TRtfdDiagram.AddToInteractive(const Str: string);
var
  Str1: string;
  Line: Integer;
begin
  try
    FInteractive.InteractiveEditor.Lines.Add(Str);
    Line := FInteractive.InteractiveEditor.Lines.Count - 1;
    if Line < 1 then
      Line := 1;
    FInteractive.InteractiveEditor.TopLine := Line;
    if Assigned(FOnModified) then
      FOnModified(nil);
  except
    on E: Exception do
    begin
      if Assigned(FOnModified) then
        Str1 := 'AddToInteractive: FOnModified assigned '
      else
        Str1 := 'AddToInteractive: FOnModified = nil ';
      if Assigned(FInteractive) then
        if Assigned(FInteractive.InteractiveEditor) then
          Str1 := Str1 + 'FInteractive.InteractiveEditor assigned '
        else
          Str1 := Str1 + 'FInteractive.InteractiveEditor = nil '
      else
        Str1 := Str1 + 'FInteractive = nil ';
      Str1 := Str1 + ' s = ' + Str;
      FConfiguration.Log(Str1, E);
    end;
  end;
end;

function TRtfdDiagram.HasInteractiveCode: Boolean;
begin
  Result := Assigned(FInteractive) and Assigned(FInteractive.InteractiveEditor)
    and (FInteractive.InteractiveEditor.Lines.Count > 0);
end;

procedure TRtfdDiagram.InteractiveSetModified(Modified: Boolean);
begin
  FInteractive.InteractiveEditor.Modified := Modified;
end;

function TRtfdDiagram.EditClass(const Caption, Title, ObjectNameOld: string;
  var ObjectNameNew: string; Control: TControl; Attributes: TStringList): Boolean;
var
  Str: string;
begin
  FObjectGenerator.PrepareEditClass(Caption, Title, ObjectNameOld);
  Result := FObjectGenerator.Edit(Control, Attributes, 2);
  if Result then
  begin
    ObjectNameNew := FObjectGenerator.ValueListEditor.Cells[1, 1];
    for var I := 0 to Attributes.Count - 1 do
    begin
      Str := FObjectGenerator.ValueListEditor.Cells[1, I + 2];
      TComJavaValue(Attributes.Objects[I]).SetFromString(Str);
    end;
  end;
end;

function TRtfdDiagram.EditObjectOrParams(const Caption, Title: string;
  Control: TControl; Attributes: TStringList): Boolean;
begin
  FObjectGenerator.PrepareEditObjectOrParams(Caption, Title);
  Result := FObjectGenerator.Edit(Control, Attributes, 1);
  if Result then
    for var I := 0 to Attributes.Count - 1 do
    begin
      var
      Str := FObjectGenerator.ValueListEditor.Cells[1, I + 1];
      TComJavaValue(Attributes.Objects[I]).SetFromString(Str);
    end;
end;

procedure TRtfdDiagram.DeleteObjects;
var
  AObject: TRtfdObject;
  ManagedObject: TManagedObject;
begin
  UnSelectAllElements;
  Application.ProcessMessages;
  for var I := FBoxNames.Count - 1 downto 0 do
    if (FBoxNames.Objects[I] is TRtfdObject) then
    begin
      AObject := TRtfdObject(FBoxNames.Objects[I]);
      ManagedObject := FPanel.FindManagedControl(AObject);
      if Assigned(ManagedObject) then
        ManagedObject.Selected := True;
    end;
  try
    DeleteSelectedControls(nil);
    FPanel.Invalidate;
    if Assigned(FInteractive) and Assigned(FInteractive.Executer) then
      FInteractive.Executer.Clear;
  except
    on E: Exception do
      FConfiguration.Log('TRtfdDiagram.DeleteObjects; ', E);
  end;
end;

function TRtfdDiagram.HasObjects: Boolean;
begin
  for var I := 0 to FBoxNames.Count - 1 do
    if (FBoxNames.Objects[I] is TRtfdObject) then
      Exit(True);
  Result := False;
end;

procedure TRtfdDiagram.DeleteObject(const Objectname: string);
var
  AObject: TRtfdObject;
  ManagedObject: TManagedObject;
begin
  for var I := FBoxNames.Count - 1 downto 0 do
    if (FBoxNames.Objects[I] is TRtfdObject) then
    begin
      AObject := TRtfdObject(FBoxNames.Objects[I]);
      if AObject.Entity.Name = Objectname then
      begin
        ManagedObject := FPanel.FindManagedControl(AObject);
        if Assigned(ManagedObject) then
          ManagedObject.Selected := True;
      end;
    end;
  try
    DeleteSelectedControls(nil);
  except
    on E: Exception do
      FConfiguration.Log('TRtfdDiagram.DeleteObject; ', E);
  end;
end;

procedure TRtfdDiagram.SetRecursiv(Posi: TPoint; Pos: Integer);
begin
  FPanel.SetRecursiv(Posi, Pos);
end;

function TRtfdDiagram.GetModelClass(const Str: string): TClass;
var
  CIte: IModelIterator;
  Cent: TClassifier;
  Typ: string;
begin
  Result := nil;
  CIte := Model.ModelRoot.GetAllClassifiers;
  while CIte.HasNext do
  begin
    Cent := TClassifier(CIte.Next);
    if Cent is TClass then
    begin
      Typ := TClass(Cent).GetTyp;
      if (Typ = Str) or (Typ = GetShortType(Str)) then
      begin
        Result := TClass(Cent);
        Break;
      end;
    end;
  end;
  if Assigned(Result) and (Result.Pathname = '') then
  begin
    while CIte.HasNext do
    begin
      Cent := TClassifier(CIte.Next);
      if Cent is TClass then
      begin
        Typ := TClass(Cent).GetTyp;
        if (Typ = Str) or (Typ = GetShortType(Str)) then
        begin
          Result := TClass(Cent);
          Break;
        end;
      end;
    end;
  end;
end;

function TRtfdDiagram.StringToArrowStyle(Str: string): TEssConnectionArrowStyle;
var
  Int: Integer;
  ArrowStyle: TEssConnectionArrowStyle;
begin
  Result := asAssociation1;
  Str := Trim(Str);
  if TryStrToInt(Str, Int) then // pre 10.3 uml-file-format
    Result := TEssConnectionArrowStyle(Int)
  else
    for ArrowStyle := asAssociation1 to asComment do
      if ArrowStyleToString(ArrowStyle) = Str then
      begin
        Result := ArrowStyle;
        Exit;
      end;
end;

function TRtfdDiagram.ArrowStyleToString(ArrowStyle
  : TEssConnectionArrowStyle): string;
begin
  case ArrowStyle of
    asAssociation1:
      Result := 'Association';
    asAssociation2:
      Result := 'AssociationDirected';
    asAssociation3:
      Result := 'AssociationBidirectional';
    asAggregation1:
      Result := 'Aggregation';
    asAggregation2:
      Result := 'AggregationArrow';
    asComposition1:
      Result := 'Composition';
    asComposition2:
      Result := 'CompositionArrow';
    asInheritends:
      Result := 'Inheritends';
    asImplements:
      Result := 'Implements';
    asInstanceOf:
      Result := 'InstanceOf';
    asComment:
      Result := 'Comment';
  end;
end;

function TRtfdDiagram.GetSourcePath: string;
begin
  Result := '';
  if Assigned(Model) and Assigned(Model.ModelRoot) then
  begin
    var
    StringList := Model.ModelRoot.Files;
    for var I := 0 to StringList.Count - 1 do
    begin
      var
      Str := ExtractFilePath(StringList[I]);
      if Pos(Str, Result) = 0 then
        Result := Result + ';' + Str;
    end;
    Delete(Result, 1, 1);
  end;
end;

procedure TRtfdDiagram.JavaReset;
begin
  FComJava.JavaReset;
end;

function TRtfdDiagram.GetCommentBoxName: string;
var
  Num, CommentNr: Integer;
  Str: string;
begin
  CommentNr := 0;
  for var I := 0 to FBoxNames.Count - 1 do
    if Pos('Comment: ', FBoxNames[I]) = 1 then
    begin
      Str := Copy(FBoxNames[I], 9, 255);
      if TryStrToInt(Str, Num) then
        CommentNr := Math.Max(CommentNr, Num);
    end;
  Result := 'Comment: ' + IntToStr(CommentNr + 1);
end;

procedure TRtfdDiagram.AddCommentBoxTo(AControl: TControl);
var
  CommentBox: TRtfdCommentBox;
  AClass: TRtfdClass;
  Str: string;
begin
  Str := GetCommentBoxName;
  CommentBox := TRtfdCommentBox.Create(FPanel, Str, FFrame, viPublic,
    HANDLESIZE);
  CommentBox.Top := 50 + Random(50);
  CommentBox.Left := 50 + Random(50);
  CommentBox.Width := 150;
  CommentBox.Height := 100;
  CommentBox.Font.Assign(Font);
  FBoxNames.AddObject(Str, CommentBox);
  FPanel.AddManagedObject(CommentBox);

  if not Assigned(AControl) then
    AControl := FPanel.GetFirstSelected;
  if Assigned(AControl) and (AControl is TRtfdClass) then
  begin
    AClass := TRtfdClass(AControl);
    CommentBox.Top := AClass.Top + Random(50);
    CommentBox.Left := AClass.Left + AClass.Width + 100 + Random(50);
    FPanel.ConnectObjects(AClass, CommentBox, asComment);
  end;
  FPanel.IsModified := True;
  FPanel.RecalcSize;
  FPanel.ShowConnections;
  CommentBox.SendToBack;
end;

function TRtfdDiagram.HasClass(AClassname: string): Boolean;
begin
  Result := False;
  for var I := 0 to FBoxNames.Count - 1 do
    if FBoxNames[I] = AClassname then
      Exit(True);
end;

procedure TRtfdDiagram.DebugJE2Java;
begin
  FComJava.ExecuteCommand('ShowHide');
end;

procedure TRtfdDiagram.DoShowParameter(Control: TControl; Mode: Integer);
begin
  if Control is TRtfdBox then
  begin
    var
    Box := TRtfdBox(Control);
    if Box.ShowParameter <> Mode then
    begin
      Box.ShowParameter := Mode;
      FPanel.ShowAll;
      FPanel.IsModified := True;
    end;
  end;
end;

procedure TRtfdDiagram.DoShowVisibility(Control: TControl; Mode: Integer);
begin
  if Control is TRtfdBox then
  begin
    var
    Box := TRtfdBox(Control);
    if Box.ShowIcons <> Mode then
    begin
      Box.ShowIcons := Mode;
      FPanel.ShowAll;
      FPanel.IsModified := True;
    end;
  end;
end;

procedure TRtfdDiagram.DoShowVisibilityFilter(Control: TControl; Mode: Integer);
begin
  if Control is TRtfdBox then
  begin
    var
    Box := TRtfdBox(Control);
    if Box.MinVisibility <> TVisibility(Mode) then
    begin
      Box.MinVisibility := TVisibility(Mode);
      FPanel.ShowAll;
      FPanel.IsModified := True;
    end;
  end;
end;

procedure TRtfdDiagram.CreateTestClass(Control: TControl);
begin
  if (Control is TRtfdClass) then
  begin
    var
    AClass := TRtfdClass(Control);
    var
    AClassname := AClass.Entity.Name + 'Test';
    var
    Filename := ExtractFilePath(AClass.GetPathname) + WithoutGeneric(AClassname)
      + '.java';
    if FileExists(Filename) and
      (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]), mtConfirmation,
      mbYesNoCancel, 0) = mrYes) or not FileExists(Filename) then
    begin
      var
      StringList := TStringList.Create;
      StringList.Text := FTemplates.GetTemplate(AClassname, 12);
      if StringList.Text = '' then
        StringList.Text := FTemplates.GetTestClassCode(AClassname);
      StringList.SaveToFile(Filename);
      FUMLForm.MainModul.AddToProject(Filename);
      FUMLForm.Modified := True;
    end;
  end;
end;

procedure TRtfdDiagram.OnRunJunitTestMethod(Sender: TObject);
var
  AMenuItem: TSpTBXItem;
  Str: string;
  Posi: Integer;
  Control: TControl;
begin
  Control := FindVCLWindow(FFrame.PopMenuClass.PopupPoint);
  if Assigned(Control) and (Sender is TSpTBXItem) then
  begin
    AMenuItem := TSpTBXItem(Sender);
    Posi := FFullParameters.IndexOfName(AMenuItem.Caption);
    Str := FFullParameters.ValueFromIndex[Posi];
    RunTests(Control, Str);
  end;
end;

procedure TRtfdDiagram.RunTests(Control: TControl; const Method: string);
begin
  if Control is TRtfdClass then
    if TRtfdClass(Control).Entity is TClass then
    begin
      if not Assigned(FJUnitTests) then
        FJUnitTests := TFJUnitTests.Create(FJava);
      FJUnitTests.Pathname :=
        TClass(TRtfdClass(Control).Entity).Pathname;
      MyJavaCommands.RunTests(TClass(TRtfdClass(Control).Entity), Method);
    end;
end;

procedure TRtfdDiagram.Lock(ALock: Boolean);
begin
  for var I := 0 to FBoxNames.Count - 1 do
    TRtfdBox(FBoxNames.Objects[I]).Lock(ALock);
end;

procedure TRtfdDiagram.ShowMethodEntered(const AMethodname, From, Till,
  Parameter: string);
begin
  if Assigned(FSequenceForm) then
  begin
    FSequenceForm.MethodEntered(AMethodname);
    if FConfiguration.SDShowParameter then
      FSequenceForm.AddParameter(Parameter);
    FSequenceForm.StartParticipant := From;
    FSequenceForm.EndParticipant := Till;
    FSequenceForm.MakeConnection;
  end;
end;

procedure TRtfdDiagram.ShowMethodExited(const AMethodname, From, Till,
  AResult: string);
begin
  if Assigned(FSequenceForm) then
  begin
    FSequenceForm.MethodExited(AMethodname);
    FSequenceForm.StartParticipant := From;
    FSequenceForm.EndParticipant := Till;
    FSequenceForm.AResult := AResult;
    FSequenceForm.MakeConnection;
  end;
end;

procedure TRtfdDiagram.ShowObjectDeleted(const From, Till: string);
begin
  if Assigned(FSequenceForm) then
  begin
    FSequenceForm.ObjectDelete;
    FSequenceForm.StartParticipant := From;
    FSequenceForm.EndParticipant := Till;
    FSequenceForm.MakeConnection;
  end;
end;

procedure TRtfdDiagram.CloseNotify(Sender: TObject);
begin
  FSequenceForm := nil;
end;

procedure TRtfdDiagram.ClearSelection;
begin
  FPanel.ClearSelection;
end;

procedure TRtfdDiagram.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    FFrame.PopMenuClass.Images := FFrame.vilClassObjectDark;
    FFrame.PopMenuObject.Images := FFrame.vilClassObjectDark;
    FFrame.PopMenuConnection.Images := FFrame.vilAssociationsDark;
    FFrame.PopupMenuWindow.Images := FFrame.vilWindowDark;
    FFrame.PopupMenuAlign.Images := FFrame.vilAlignDark;
  end
  else
  begin
    FFrame.PopMenuClass.Images := FFrame.vilClassObjectLight;
    FFrame.PopMenuObject.Images := FFrame.vilClassObjectLight;
    FFrame.PopMenuConnection.Images := FFrame.vilAssociationsLight;
    FFrame.PopupMenuWindow.Images := FFrame.vilWindowLight;
    FFrame.PopupMenuAlign.Images := FFrame.vilAlignLight;
  end;
  FPanel.ChangeStyle;
end;

procedure TRtfdDiagram.CopyDiagramToClipboard;
var
  Selected: Boolean;
  Bmp1, Bmp2: Graphics.TBitmap;
  Width, Height: Integer;
  SelRect: TRect;
begin
  FPanel.ChangeStyle(True);
  SelRect := GetSelectedRect;
  Selected := (SelRect.Right > SelRect.Left);
  GetDiagramSize(Width, Height);
  try
    Bmp1 := Graphics.TBitmap.Create;
    Bmp1.Width := Width;
    Bmp1.Height := Height;
    Bmp1.Canvas.Lock;
    PaintTo(Bmp1.Canvas, 0, 0, True);

    Bmp2 := Graphics.TBitmap.Create;
    if Selected then
    begin
      Bmp2.Width := SelRect.Right - SelRect.Left + 2;
      Bmp2.Height := SelRect.Bottom - SelRect.Top + 2;
      Bmp2.Canvas.Draw(-SelRect.Left + 1, -SelRect.Top + 1, Bmp1);
      Clipboard.Assign(Bmp2);
    end
    else
      Clipboard.Assign(Bmp1);
    Bmp1.Canvas.Unlock;
  finally
    FreeAndNil(Bmp1);
    FreeAndNil(Bmp2);
  end;
  ClearSelection;
  FPanel.ChangeStyle(False);
end;

procedure TRtfdDiagram.ClearMarkerAndConnections(Control: TControl);
begin
  FPanel.ClearMarkerAndConnections(Control);
end;

procedure TRtfdDiagram.DrawMarkers(Rect: TRect; Show: Boolean);
begin
  FPanel.DrawMarkers(Rect, Show);
end;

procedure TRtfdDiagram.EditBox(Control: TControl);
begin
  FPanel.EditBox(Control);
end;

procedure TRtfdDiagram.SetModified(const Value: Boolean);
begin
  FPanel.SetIsModified(Value);
end;

procedure TRtfdDiagram.SetOnModified(OnBoolEvent: TBoolEvent);
begin
  FPanel.OnModified := OnBoolEvent;
end;

procedure TRtfdDiagram.SetOnSelectionChanged(Sender: TNotifyEvent);
begin
  FPanel.OnSelectionChanged := Sender;
end;

procedure TRtfdDiagram.DeleteComment;
begin
  var
  Control := FindVCLWindow(FFrame.PopupMenuComment.PopupPoint);
  if Assigned(Control) then
  begin
    var
    ManagedObject := FPanel.FindManagedControl(Control);
    if Assigned(ManagedObject) then
    begin
      UnSelectAllElements;
      ManagedObject.Selected := True;
      DeleteSelectedControls(nil);
    end;
  end;
end;

function TRtfdDiagram.GetClasses: TStringList;
begin
  Result := TStringList.Create;
  for var I := 0 to FBoxNames.Count - 1 do
    if (FBoxNames.Objects[I] is TRtfdClass) or
      (FBoxNames.Objects[I] is TRtfdInterface) then
      Result.Add(FBoxNames[I]);
end;

procedure TRtfdDiagram.Retranslate;
begin
  FFrame.Retranslate;
end;

function TRtfdDiagram.PanelIsLocked: Boolean;
begin
  Result := (FPanel.UpdateCounter > 0);
end;

procedure TRtfdDiagram.SetUMLFont;
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options := [];
  if FJava.FDFont.Execute then
  begin
    Font.Assign(FJava.FDFont.Font);
    FConfiguration.UMLFont.Assign(Font);
    SetFont(Font);
    RefreshDiagram;
  end;
end;

end.
