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

unit UModel;

{
  Classes to represent the object model.
}

interface

uses
  Contnrs,
  Classes,
  UListeners,
  UModelEntity,
  UUtils;

const
  UNKNOWNPACKAGE_NAME = '<<Unknown>>';
  ConfigFileExt = '.uml';

type
  TLogicPackage = class;
  TUnitPackage = class;

  TOperationType = (otConstructor, otProcedure, otFunction, otDestructor);

  TObjectModel = class
  private
    FListeners: TInterfaceList;
    FModelRoot: TLogicPackage;
    FUnknownPackage: TUnitPackage;
    FLocked: Integer;
    FNameCache: TStringList;
    procedure CreatePackages;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
    procedure AddListener(NewListener: IUnknown);
    procedure RemoveListener(Listener: IUnknown);
    procedure ClearListeners;
    procedure Clear;
    procedure Lock;
    procedure Unlock;
    function Debug: string;
    property ModelRoot: TLogicPackage read FModelRoot;
    property Locked: Integer read FLocked write FLocked;
    property NameCache: TStringList read FNameCache;
    property UnknownPackage: TUnitPackage read FUnknownPackage;
  end;

  TFeature = class(TModelEntity);

  TClassifier = class(TModelEntity)
  private
    FAnonym: Boolean;
    FGeneric: string;
    FFeatures: TObjectList;
    FGenericName: string;
    FImportname: string;
    FInner: Boolean;
    FSourceRead: Boolean;
  public
    Pathname: string;
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function GetFeatures: IModelIterator;
    function GetAttributes: IModelIterator;
    function GetAllAttributes: IModelIterator;
    function GetOperations: IModelIterator;
    function GetShortType: string;
    function GetName: string;
    function IsReference: Boolean;
    function GetAncestorName: string; virtual;
    procedure SetCapacity(Capacity: Integer);
    property Anonym: Boolean read FAnonym write FAnonym;
    property Generic: string read FGeneric write FGeneric;
    property GenericName: string read FGenericName write FGenericName;
    property Importname: string read FImportname write FImportname;
    property Inner: Boolean read FInner write FInner;
    property SourceRead: Boolean read FSourceRead write FSourceRead;
  end;

  TParameter = class(TModelEntity)
  private
    FTypeClassifier: TClassifier;
    FUsedForAttribute: Boolean;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    property TypeClassifier: TClassifier read FTypeClassifier
      write FTypeClassifier;
    property UsedForAttribute: Boolean read FUsedForAttribute
      write FUsedForAttribute;
  end;

  TAttribute = class(TFeature)
  private
    FTypeClassifier: TClassifier; // reference
    FValue: string;
    FConnected: Boolean;
    procedure SetTypeClassifier(const Value: TClassifier);
    procedure SetValue(const Value: string);
    procedure SetConnected(Value: Boolean);
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    function ToTypeName: string;
    function ToNameType: string;
    function ToNameTypeValue: string;
    function ToJava: string;
    property TypeClassifier: TClassifier read FTypeClassifier
      write SetTypeClassifier;
    property Value: string read FValue write SetValue;
    property Connected: Boolean read FConnected write SetConnected;
  end;

  TOperation = class(TFeature)
  private
    FAnnotation: string;
    FOperationType: TOperationType;
    FParameters: TObjectList;
    FReturnValue: TClassifier;
    FAttributes: TObjectList; // local variables
    FHasComment: Boolean;
    FHasSourceCode: Boolean;
    FParentname: string;
    procedure SetOperationType(const Value: TOperationType);
    procedure SetReturnValue(const Value: TClassifier);
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    procedure NewParameters;
    function AddParameter(const NewName: string): TParameter;
    function GetParameters: IModelIterator;
    function AddAttribute(const NewName: string; TypeClass: TClassifier)
      : TAttribute; // local variables
    function GetAttributes: IModelIterator;
    function ToTypeName: string;
    function ToNameParameterTyp: string;
    function ToJava: string;
    function HasMain: Boolean;
    procedure SetAttributeScope(ScopeDepth, LineE: Integer);
    function GetFormattedDescription: string;
    property Annotation: string read FAnnotation write FAnnotation;
    property HasComment: Boolean read FHasComment write FHasComment;
    property HasSourceCode: Boolean read FHasSourceCode write FHasSourceCode;
    property OperationType: TOperationType read FOperationType
      write SetOperationType;
    property Parentname: string read FParentname write FParentname;
    property ReturnValue: TClassifier read FReturnValue write SetReturnValue;
  end;

  TProperty = class(TAttribute)
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  end;

  TDataType = class(TClassifier)
    { From UML-spec: A descriptor of a set of values that lack identity and whose
      operations do not have side effects. Datatypes include
      primitive pre-defined types and user-definable types. Pre-defined
      types include numbers, string and time. User-definable
      types include enumerations. }
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  end;

  TInterface = class(TClassifier)
  private
    FAncestor: TInterface;
    FExtends: TObjectList;
    procedure SetAncestor(const Value: TInterface);
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function MakeOperation(const NewName: string; TypeClass: TClassifier)
      : TOperation;
    procedure AddOperation(Operation: TOperation);
    function AddAttribute(const NewName: string; TypeClass: TClassifier)
      : TAttribute;
    function AddExtends(Int: TInterface): TInterface;
    procedure ViewExtends(Int: TInterface);
    function GetExtends: IModelIterator;
    function GetImplementingClasses: IModelIterator;
    function FindAttribute(Name, AType: string): TAttribute;
    function FindOperation(Operation: TOperation): TOperation;
    function GetAncestorName: string; override;
    function AddOperationWithoutType(const NewName: string): TOperation;
    property Ancestor: TInterface read FAncestor write SetAncestor;
  end;

  TClass = class(TClassifier, IBeforeClassListener)
  private
    FAncestor: TClass;
    FImplements: TObjectList;
    FIsJUnitTestClass: Boolean;
    procedure SetAncestor(const Value: TClass);
    // Ancestorlisteners
    procedure AncestorChange(Sender: TModelEntity);
    procedure AncestorAddChild(Sender: TModelEntity; NewChild: TModelEntity);
    procedure AncestorRemove(Sender: TModelEntity);
    procedure AncestorEntityChange(Sender: TModelEntity);
    function AncestorQueryInterface
      ({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const
      {$ENDIF} IID: TGUID; out Obj): HRESULT; stdcall;
    procedure IBeforeClassListener.Change = AncestorChange;
    procedure IBeforeClassListener.EntityChange = AncestorEntityChange;
    procedure IBeforeClassListener.AddChild = AncestorAddChild;
    procedure IBeforeClassListener.Remove = AncestorRemove;
    function IBeforeClassListener.QueryInterface = AncestorQueryInterface;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function MakeOperation(const NewName: string; TypeClass: TClassifier)
      : TOperation;
    procedure AddOperation(Operation: TOperation);
    function AddOperationWithoutType(const NewName: string): TOperation;
    function AddAttribute(const NewName: string; TypeClass: TClassifier)
      : TAttribute;
    function AddProperty(const NewName: string): TProperty;
    function AddImplements(Int: TInterface): TInterface;
    procedure ViewImplements(Int: TInterface);
    function GetImplements: IModelIterator;
    function GetDescendants: IModelIterator;
    function FindOperation(Operation: TOperation): TOperation;
    function FindAttribute(Name, AType: string): TAttribute;
    function GetTyp: string;
    function GetAncestorName: string; override;
    property Ancestor: TClass read FAncestor write SetAncestor;
    property IsJUnitTestClass: Boolean read FIsJUnitTestClass
      write FIsJUnitTestClass;
  end;

  TJUnitClass = class(TClass, IBeforeClassListener)
  end;

  TObjekt = class(TClassifier)
  private
    FClass: TClass;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    destructor Destroy; override;
    function AddAttribute(const NewName: string): TAttribute;
    function AddOperation(const NewName: string): TOperation;
    procedure RefreshEntities;
    function GetTyp: TClass;
    function GenericName: string;
  end;

  TAbstractPackage = class(TModelEntity)
  private
    FConfigFile: string;
  public
    procedure SetConfigFile(const Value: string);
    function GetConfigFile: string;
  end;

  // Represents the link between one package that uses another
  TUnitDependency = class(TModelEntity)
  private
    FMyPackage: TUnitPackage;
  public
    property MyPackage: TUnitPackage read FMyPackage write FMyPackage;
  end;

  TUnitPackage = class(TAbstractPackage)
  private
    FClassifiers: TObjectList;
    FClassImports: TStringList;
    FFullImports: TStringList;
    FImportEndline: Integer;
    FImportStartline: Integer;
    FUnitDependencies: TObjectList;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    function MakeClass(const NewName, Filename: string): TClass;
    procedure AddClass(const AClass: TClass);
    procedure AddClassWithoutShowing(const AClass: TClass);
    function MakeInterface(const NewName, Filename: string): TInterface;
    procedure AddInterface(const AInterface: TInterface);
    procedure AddInterfaceWithoutShowing(const AInterface: TInterface);
    function AddDatatype(const NewName: string): TDataType;
    function AddObject(const NewName: string; AClass: TClass): TObjekt;
    procedure DeleteObject(AName: string);
    function GetObject(const AName: string): TObjekt;
    function AddUnitDependency(UnitPackage: TUnitPackage; Visibility: TVisibility)
      : TUnitDependency;
    function FindClassifier(const CName: string;
      TheClass: TModelEntityClass = nil; CaseSense: Boolean = False)
      : TClassifier;
    function FindClass(const Pathname: string): TClassifier;
    function GetClassifiers: IModelIterator;
    function GetUnitDependencies: IModelIterator;
    function GetObjects(const Typ: string): TStringList;
    function GetAllObjects: TStringList;
    function GetObjekt(const AName, Typ: string): TObjekt;
    function Debug: string;
    property ClassImports: TStringList read FClassImports;
    property FullImports: TStringList read FFullImports;
    property ImportEndline: Integer read FImportEndline write FImportEndline;
    property ImportStartline: Integer read FImportStartline write FImportStartline;
  end;

  TLogicPackage = class(TAbstractPackage)
  private
    FFiles: TStringList;
    FPackages: TObjectList;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(Owner: TModelEntity); override;
    destructor Destroy; override;
    procedure Clear;
    function AddUnit(const NewUnitName: string): TUnitPackage;
    // might need a AddLogicPackage also
    function FindUnitPackage(const PName: string; CaseSense: Boolean = False)
      : TUnitPackage;
    function GetPackages: IModelIterator;
    function GetAllUnitPackages: IModelIterator;
    function GetAllClassifiers: IModelIterator;
    function Debug: string;
    property Files: TStringList read FFiles;
  end;

function AllClassesPackage: TAbstractPackage;
procedure AllClassesPackageClose;

implementation

uses
  Windows,
  StrUtils,
  SysUtils,
  Types,
  UIterators,
  UConfiguration;

type
  // Used by Class.GetDescendant
  TClassDescendantFilter = class(TIteratorFilter)
  private
    FAncestor: TClass;
  public
    constructor Create(Ancestor: TClass);
    function Accept(Model: TModelEntity): Boolean; override;
  end;

  // Used by Interface.GetImplementingClasses
  TInterfaceImplementsFilter = class(TIteratorFilter)
  private
    FIntf: TInterface;
  public
    constructor Create(Intf: TInterface);
    function Accept(Model: TModelEntity): Boolean; override;
  end;

  TStrCompare = function(const Str1, Str2: string): Integer;

const
  CompareFunc: array [Boolean] of TStrCompare = (CompareText, CompareStr);

  { TObjectModel }

constructor TObjectModel.Create;
begin
  FListeners := TInterfaceList.Create;
  FNameCache := TStringList.Create;
  CreatePackages;
end;

destructor TObjectModel.Destroy;
begin
  FreeAndNil(FModelRoot);
  // FUnknownPackage will be freed by FModelRoot who owns it
  FreeAndNil(FListeners);
  for var I := 0 to FNameCache.Count - 1 do
  begin
    var
    AClassifier := TClassifier(FNameCache.Objects[I]);
    FreeAndNil(AClassifier);
  end;
  FreeAndNil(FNameCache);
  inherited;
end;

procedure TObjectModel.Clear;
begin
  // Model must be locked, otherwise events will be fired back to
  // backend and diagram.
  for var I := 0 to FNameCache.Count - 1 do
    FreeAndNil(FNameCache.Objects[I]);
  FNameCache.Clear;
  FModelRoot.Clear; // destroys UUnknownPackage
  FUnknownPackage := FModelRoot.AddUnit(UNKNOWNPACKAGE_NAME);
end;

procedure TObjectModel.Fire(Method: TListenerMethodType;
  Info: TModelEntity = nil);
var
  Listener, Dum: IUnknown;
begin
  if (FLocked = 0) and Assigned(FListeners) then
    for var I := 0 to FListeners.Count - 1 do
    begin
      Listener := FListeners[I];
      case Method of
        // BeforeChange is triggered when the model will be changed from the root-level.
        mtBeforeChange:
          if Listener.QueryInterface(IBeforeObjectModelListener, Dum) = 0 then
            (Listener as IBeforeObjectModelListener).Change(nil);
          // AfterChange is triggered when the model has been changed from the root-level.
        mtAfterChange:
          if Listener.QueryInterface(IAfterObjectModelListener, Dum) = 0 then
            (Listener as IAfterObjectModelListener).Change(nil);
        end;
    end;
end;

procedure TObjectModel.Lock;
begin
  Fire(mtBeforeChange);
  Inc(FLocked);
  ModelRoot.Locked := True;
end;

procedure TObjectModel.Unlock;
begin
  Dec(FLocked);
  if FLocked = 0 then
  begin
    ModelRoot.Locked := False;
    Fire(mtAfterChange);
  end;
end;

function TObjectModel.Debug: string;
begin
  Result := FModelRoot.Debug + FUnknownPackage.Debug;
end;

procedure TObjectModel.CreatePackages;
begin
  // Creates the default packages that must exist
  FModelRoot := TLogicPackage.Create(nil);
  FUnknownPackage := FModelRoot.AddUnit(UNKNOWNPACKAGE_NAME);
end;

procedure TObjectModel.AddListener(NewListener: IUnknown);
begin
  if FListeners.IndexOf(NewListener) = -1 then
    FListeners.Add(NewListener);
end;

procedure TObjectModel.RemoveListener(Listener: IUnknown);
begin
  if Assigned(FListeners) then
    FListeners.Remove(Listener);
end;

procedure TObjectModel.ClearListeners;
begin
  if Assigned(FListeners) then
    FListeners.Clear;
end;

{ TLogicPackage }

constructor TLogicPackage.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FPackages := TObjectList.Create(True);
  FFiles := TStringList.Create;
  FFiles.Duplicates := dupIgnore;
  FFiles.Sorted := True;
end;

destructor TLogicPackage.Destroy;
begin
  FreeAndNil(FPackages);
  FreeAndNil(FFiles);
  inherited;
end;

procedure TLogicPackage.Clear;
begin
  FFiles.Clear;
  FPackages.Clear;
end;

function TLogicPackage.AddUnit(const NewUnitName: string): TUnitPackage;
begin
  Result := TUnitPackage.Create(Self);
  Result.FName := NewUnitName;
  FPackages.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FPackages.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

class function TLogicPackage.GetAfterListener: TGUID;
begin
  Result := IAfterLogicPackageListener;
end;

class function TLogicPackage.GetBeforeListener: TGUID;
begin
  Result := IBeforeLogicPackageListener;
end;

// Searches in this and dependant logic packages after a unit with name PName.
function TLogicPackage.FindUnitPackage(const PName: string;
  CaseSense: Boolean = False): TUnitPackage;
var
  Posi: TAbstractPackage;
  Compare: TStrCompare;
begin
  Compare := CompareFunc[CaseSense];
  Result := nil;
  for var I := 0 to FPackages.Count - 1 do
  begin
    Posi := FPackages[I] as TAbstractPackage;
    if Posi is TLogicPackage then
    begin
      Result := (Posi as TLogicPackage).FindUnitPackage(PName);
      if Assigned(Result) then
        Exit;
    end
    else if Posi is TUnitPackage then
    begin
      if Compare(Posi.Name, PName) = 0 then
      begin
        Result := Posi as TUnitPackage;
        Exit;
      end;
    end;
  end;
end;

function TLogicPackage.GetPackages: IModelIterator;
begin
  // Got TObjectList expected IModellIterator
  var
  ObjList := FPackages;
  Result := TModelIterator.Create(ObjList);
end;

// Returns all unitpackages in and below this logic package.
// Unknownpackage is excluded.
function TLogicPackage.GetAllUnitPackages: IModelIterator;

var
  List: TObjectList;

  procedure InAddNested(Logic: TLogicPackage);
  begin
    var
    Ite := Logic.GetPackages;
    while Ite.HasNext do
    begin
      var
      Posi := Ite.Next;
      if Posi is TLogicPackage then
        InAddNested(Posi as TLogicPackage)
      else // Not logicpackage, must be unitpackage.
        if (Posi.Name <> UNKNOWNPACKAGE_NAME) then
          List.Add(Posi);
    end;
  end;

begin
  List := TObjectList.Create(False);
  try
    try
      InAddNested(Self);
    except
      on E: Exception do
        OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' + E.Message));
    end;
    Result := TModelIterator.Create(List, True);
  finally
    FreeAndNil(List);
  end;
end;

// Returns all classifiers in and below this logic package.
function TLogicPackage.GetAllClassifiers: IModelIterator;
var
  Pmi, Cmi: IModelIterator;
  List: TObjectList;
begin
  List := TObjectList.Create(False);
  try
    Pmi := GetAllUnitPackages;
    while Pmi.HasNext do
    begin
      Cmi := (Pmi.Next as TUnitPackage).GetClassifiers;
      while Cmi.HasNext do
        List.Add(Cmi.Next);
    end;
    Result := TModelIterator.Create(List, True);
  finally
    FreeAndNil(List);
  end;
end;

function TLogicPackage.Debug: string;
var
  StringList: TStringList;
  Int: Integer;
  CIte, AIte, OIte, PIte: IModelIterator;
  Cent: TClassifier;
  Attribute: TAttribute;
  Operation: TOperation;
begin
  // use it in TFJava.OpenUMLWindow/TFJava.DoOpenInUMLWindow
  Int := 0;
  StringList := TStringList.Create;
  StringList.Add('LogicPackage ' + Name);
  StringList.Add('Packages');
  PIte := GetAllUnitPackages;
  while PIte.HasNext do
  begin
    StringList.Add((PIte.Next as TUnitPackage).Name);
  end;
  StringList.Add('');
  StringList.Add('Classes');
  CIte := GetAllClassifiers;
  while CIte.HasNext do
  begin
    Cent := TClassifier(CIte.Next);
    Inc(Int);
    StringList.Add('#' + IntToStr(Int) + ' ' + Cent.Name + ' - ' + Cent.Importname +
      ' - ' + Cent.Pathname);
    if (Cent is TClass) and Assigned((Cent as TClass).Ancestor) then
      StringList.Add('Ancestor: ' + (Cent as TClass).Ancestor.Name);
    StringList.Add('--- Attributes ---');
    AIte := Cent.GetAllAttributes;
    while AIte.HasNext do
    begin
      Attribute := AIte.Next as TAttribute;
      StringList.Add(Attribute.ToTypeName);
    end;
    StringList.Add('--- Operations ---');
    OIte := Cent.GetOperations;
    while OIte.HasNext do
    begin
      Operation := OIte.Next as TOperation;
      StringList.Add(Operation.ToJava);
    end;
    StringList.Add('-------------------------');
  end;
  StringList.Add('');
  StringList.Add('Files: ');
  StringList.Add(FFiles.Text);
  StringList.Add('-------------------------');

  Result := StringList.Text;
  FreeAndNil(StringList);
end;

{ TUnitPackage }

constructor TUnitPackage.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FClassifiers := TObjectList.Create(True);
  FUnitDependencies := TObjectList.Create(True);
  FFullImports := TStringList.Create;
  FFullImports.Add('java.lang.');
  FClassImports := TStringList.Create;
end;

destructor TUnitPackage.Destroy;
begin
  FreeAndNil(FClassImports);
  FreeAndNil(FFullImports);
  FreeAndNil(FUnitDependencies);
  FreeAndNil(FClassifiers);
  inherited;
end;

function TUnitPackage.MakeClass(const NewName, Filename: string): TClass;
begin
  var
  AClass := FindClassifier(NewName, TClass, True);
  if not Assigned(AClass) then
  begin
    AClass := TClass.Create(Self);
    AClass.Name := NewName; // sets package too
    AddClass(AClass as TClass);
  end;
  AClass.Pathname := Filename;
  Result := (AClass as TClass);
end;

procedure TUnitPackage.AddClass(const AClass: TClass);
begin
  if not Assigned(FindClassifier(AClass.Name, TClass, True)) then
    FClassifiers.Add(AClass);
  Fire(mtBeforeAddChild, AClass);
  Fire(mtAfterAddChild, AClass);
end;

procedure TUnitPackage.AddClassWithoutShowing(const AClass: TClass);
begin
  if Assigned(FindClassifier(AClass.Name, TClass, True)) then
    Exit;
  FClassifiers.Add(AClass);
end;

function TUnitPackage.MakeInterface(const NewName, Filename: string)
  : TInterface;
begin
  Result := TInterface.Create(Self);
  Result.FName := NewName;
  Result.Pathname := Filename;
end;

procedure TUnitPackage.AddInterface(const AInterface: TInterface);
begin
  FClassifiers.Add(AInterface);
  try
    Fire(mtBeforeAddChild, AInterface);
  except
    FClassifiers.Remove(AInterface);
  end;
  Fire(mtAfterAddChild, AInterface);
end;

procedure TUnitPackage.AddInterfaceWithoutShowing(const AInterface
  : TInterface);
begin
  FClassifiers.Add(AInterface);
end;

function TUnitPackage.AddDatatype(const NewName: string): TDataType;
begin
  Result := TDataType.Create(Self);
  Result.FName := NewName;
  FClassifiers.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.AddObject(const NewName: string;
  AClass: TClass): TObjekt;
begin
  try
    Result := GetObjekt(NewName, AClass.Name);
  except
    Result := nil;
    Exit;
  end;
  if not Assigned(Result) then
  begin
    Result := TObjekt.Create(Self);
    Result.FName := NewName;
    Result.IsVisible := True;
    Result.FClass := AClass;
    FClassifiers.Add(Result);
  end;
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.GetObjekt(const AName, Typ: string): TObjekt;
var
  Ite: TModelIterator;
  AClassifier: TClassifier;
  AObjekt: TObjekt;
begin
  Result := nil;
  Ite := TModelIterator.Create(GetClassifiers);
  try
    while Ite.HasNext do
    begin
      AClassifier := Ite.Next as TClassifier;
      if (AClassifier is TObjekt) then
      begin
        AObjekt := (AClassifier as TObjekt);
        if (AClassifier.Name = AName) and
          (GetShortType(AObjekt.FClass.Name) = Typ) then
        begin
          Result := (AClassifier as TObjekt);
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(Ite);
  end;
end;

function TUnitPackage.GetObjects(const Typ: string): TStringList;
begin
  Result := TStringList.Create;
  var
  Ite := TModelIterator.Create(GetClassifiers);
  try
    while Ite.HasNext do
    begin
      var
      AClassifier := Ite.Next as TClassifier;
      if (AClassifier is TObjekt) and
        (GetShortType((AClassifier as TObjekt).FClass.Name) = Typ) then
        Result.AddObject((AClassifier as TObjekt).FName, AClassifier);
    end;
  finally
    FreeAndNil(Ite);
  end;
end;

function TUnitPackage.GetAllObjects: TStringList;
begin
  Result := TStringList.Create;
  var
  Ite := TModelIterator.Create(GetClassifiers);
  try
    try
      while Ite.HasNext do
      begin
        var
        AClassifier := Ite.Next as TClassifier;
        if AClassifier is TObjekt then
          Result.AddObject((AClassifier as TObjekt).FName, AClassifier);
      end;
    finally
      FreeAndNil(Ite);
    end;
  except
    on E: Exception do
      OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' + E.Message));
  end;
end;

procedure TUnitPackage.DeleteObject(AName: string);
begin
  var
  AObject := FindClassifier(AName, nil, True);
  // true because of casesensitive
  if Assigned(AObject) then
    FClassifiers.Remove(AObject);
end;

function TUnitPackage.GetObject(const AName: string): TObjekt;
begin
  Result := TObjekt(FindClassifier(AName, nil, False));
end;

class function TUnitPackage.GetAfterListener: TGUID;
begin
  Result := IAfterUnitPackageListener;
end;

class function TUnitPackage.GetBeforeListener: TGUID;
begin
  Result := IBeforeUnitPackageListener;
end;

{
  Search for classifier in this unit, then looks in UnitDependencies if necessary.
  Used by the parser to find ancestorclass within current scope.
}
function TUnitPackage.FindClassifier(const CName: string;
  TheClass: TModelEntityClass = nil; CaseSense: Boolean = False): TClassifier;
var
  Classi: TClassifier;
  MIte: IModelIterator;
  Posi: TUnitPackage;
  Compare: TStrCompare;

  function InFind(Posi: TUnitPackage): TClassifier;
  var
    MIte: IModelIterator;
    Str: string;
  begin
    Result := nil;
    // Search in this unit
    if Assigned(TheClass) then
      MIte := TModelIterator.Create(Posi.GetClassifiers, TheClass)
    else if Assigned(Posi) then
    begin
      try
        MIte := Posi.GetClassifiers;
      except
        on e: Exception do
        begin
          if Assigned(Posi) then
            Str := Posi.ClassName
          else
            Str := 'p = nil';
          FConfiguration.Log('InFind(P : TUnitPackage): ' + Str, e);
          Exit;
        end;
      end;
    end
    else
      Exit;
    while MIte.HasNext do
    begin
      Classi := MIte.Next as TClassifier;
      if Compare(Classi.Name, CName) = 0 then
      begin
        Result := Classi;
        Break;
      end;
    end;
  end;

begin
  Result := nil;
  try
    Compare := CompareFunc[CaseSense];
    // Search in this unit
    Result := InFind(Self);
    // If nil search in public dependencies
    if not Assigned(Result) then
    begin
      MIte := GetUnitDependencies;
      while MIte.HasNext do
      begin
        Posi := (MIte.Next as TUnitDependency).MyPackage;
        Result := InFind(Posi);
        if Assigned(Result) then
          Break;
      end;
    end;
  except
    on e: Exception do
      FConfiguration.Log(' TUnitPackage.FindClassifier', e);
  end;
end;

function TUnitPackage.FindClass(const Pathname: string): TClassifier;
begin
  Result := nil;
  var
  MIte := GetClassifiers;
  while MIte.HasNext do
  begin
    var
    Classi := MIte.Next as TClassifier;
    if Classi.Pathname = Pathname then
    begin
      Result := Classi;
      Break;
    end;
  end;
end;

function TUnitPackage.GetClassifiers: IModelIterator;
begin
  try
    Result := TModelIterator.Create(FClassifiers);
  except
    Result := nil;
  end;
end;

function TUnitPackage.AddUnitDependency(UnitPackage: TUnitPackage;
  Visibility: TVisibility): TUnitDependency;
begin
  Assert((UnitPackage <> Self) and Assigned(UnitPackage),
    ClassName + 'AddUnitDependency invalid parameter');
  Result := TUnitDependency.Create(Self);
  Result.MyPackage := UnitPackage;
  Result.Visibility := Visibility;
  FUnitDependencies.Add(Result);
end;

function TUnitPackage.GetUnitDependencies: IModelIterator;
begin
  Result := TModelIterator.Create(FUnitDependencies);
end;

// use it in TFJava.OpenUMLWindow/TFJava.DoOpenInUMLWindow
function TUnitPackage.Debug: string;
var
  StringList: TStringList;
  CIte, AIte, OIte: IModelIterator;
  Cent: TClassifier;
  Attribute: TAttribute;
  Operation: TOperation;
begin
  StringList := TStringList.Create;
  StringList.Add('UnitPackage: ' + Name);
  CIte := GetClassifiers;
  while CIte.HasNext do
  begin
    Cent := TClassifier(CIte.Next);
    StringList.Add(Cent.Name + ' - ' + Cent.Importname + ' - ' +
      Cent.Pathname);
    StringList.Add('--- Attributes ---');
    AIte := Cent.GetAllAttributes;
    while AIte.HasNext do
    begin
      Attribute := AIte.Next as TAttribute;
      StringList.Add(Attribute.ToTypeName);
    end;
    StringList.Add('--- Operations ---');
    OIte := Cent.GetOperations;
    while OIte.HasNext do
    begin
      Operation := OIte.Next as TOperation;
      StringList.Add(Operation.ToJava);
    end;
    StringList.Add('-------------------------');
  end;
  Result := StringList.Text;
  FreeAndNil(StringList);
end;

{ TClass }

constructor TClass.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FAncestor := nil;
  FImplements := TObjectList.Create(False); // Only reference
end;

destructor TClass.Destroy;
begin
  // Dont touch FListeners if the model is locked.
  if not Locked then
  begin
    Fire(mtBeforeRemove);
  end;
  FreeAndNil(FImplements);
  inherited;
end;

function TClass.AddAttribute(const NewName: string; TypeClass: TClassifier)
  : TAttribute;
begin
  if Assigned(TypeClass) then
    Result := FindAttribute(NewName, TypeClass.Name)
  else
    Result := nil;
  if not Assigned(Result) then
  begin
    Result := TAttribute.Create(Self);
    Result.FTypeClassifier := TypeClass;
    Result.FName := NewName;
    FFeatures.Add(Result);
    try
      Fire(mtBeforeAddChild, Result);
    except
      FFeatures.Remove(Result);
    end;
    Fire(mtAfterAddChild, Result);
  end;
end;

function TClass.AddProperty(const NewName: string): TProperty;
begin
  Result := TProperty.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
end;

function TClass.AddOperationWithoutType(const NewName: string): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue := nil;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

function TClass.MakeOperation(const NewName: string; TypeClass: TClassifier)
  : TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue := TypeClass;
end;

procedure TClass.AddOperation(Operation: TOperation);
begin
  FFeatures.Add(Operation);
  try
    Fire(mtBeforeAddChild, Operation);
  except
    FFeatures.Remove(Operation);
  end;
  Fire(mtAfterAddChild, Operation);
end;

class function TClass.GetAfterListener: TGUID;
begin
  Result := IAfterClassListener;
end;

class function TClass.GetBeforeListener: TGUID;
begin
  Result := IBeforeClassListener;
end;

function TClass.AddImplements(Int: TInterface): TInterface;
begin
  Result := Int;
  FImplements.Add(Int);
end;

procedure TClass.ViewImplements(Int: TInterface);
begin
  try
    Fire(mtBeforeAddChild, Int);
  except
    FImplements.Remove(Int);
  end;
  Fire(mtAfterAddChild, Int);
end;

procedure TClass.SetAncestor(const Value: TClass);
begin
  if Value = Self then
    Exit;
  if Value <> FAncestor then
  begin
    var
    Old := FAncestor;
    FAncestor := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FAncestor := Old;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

procedure TClass.AncestorAddChild(Sender, NewChild: TModelEntity);
begin
end;

procedure TClass.AncestorChange(Sender: TModelEntity);
begin
end;

procedure TClass.AncestorEntityChange(Sender: TModelEntity);
begin
  Fire(mtBeforeEntityChange);
  Fire(mtAfterEntityChange);
end;

procedure TClass.AncestorRemove(Sender: TModelEntity);
begin
  FAncestor.RemoveListener(IBeforeClassListener(Self));
  Ancestor := nil;
end;

function TClass.AncestorQueryInterface
  ({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const
  {$ENDIF} IID: TGUID; out Obj): HRESULT; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TClass.GetImplements: IModelIterator;
begin
  Result := TModelIterator.Create(FImplements);
end;

// Returns a list of classes that inherits from this class.
function TClass.GetDescendants: IModelIterator;
begin
  Result := TModelIterator.Create((Root as TLogicPackage).GetAllClassifiers,
    TClassDescendantFilter.Create(Self));
end;

{
  Finds an operation with same name and signature as parameter.
  Used by Delphi-parser to find a modelentity for a method implementation.
}
function TClass.FindOperation(Operation: TOperation): TOperation;
var
  MIte, Omi1, Omi2: IModelIterator;
  Operation2: TOperation;
begin
  Result := nil;
  MIte := GetOperations;
  while MIte.HasNext do
  begin
    Operation2 := MIte.Next as TOperation;
    // Compare nr of parameters
    if Operation.FParameters.Count <> Operation2.FParameters.Count then
      Continue;
    // Compare operation name
    if CompareText(Operation.Name, Operation2.Name) <> 0 then
      Continue;
    // Compare parameters
    Omi1 := Operation.GetParameters;
    Omi2 := Operation2.GetParameters;
    var Okay:= True;
    while Okay and Omi1.HasNext do
      if CompareText((Omi1.Next as TParameter).Name, (Omi2.Next as TParameter).Name) <> 0 then
        Okay:= False;
    if not Okay then
      Continue;
    // Ok, match
    Result := Operation2;
    Break;
  end;
end;

{
  Finds an attribute with same name and type.
}
function TClass.FindAttribute(Name, AType: string): TAttribute;
begin
  Result := nil;
  var
  MIte := GetAttributes;
  while MIte.HasNext do
  begin
    var
    Attribute := MIte.Next as TAttribute;
    if (CompareText(Name, Attribute.Name) = 0) and
      (CompareText(AType, Attribute.TypeClassifier.Name) = 0) then
      Exit(Attribute);
  end;
end;

function TClass.GetTyp: string;
begin
  if Pathname <> '' then
    Result := Name
  else
    Result := Importname;
end;

function TClass.GetAncestorName: string;
begin
  if Assigned(Ancestor) then
    Result := Ancestor.Name
  else
    Result := '';
end;

{ TObjekt }

destructor TObjekt.Destroy;
begin
  // Dont touch FListeners if the model is locked.
  if not Locked then
    Fire(mtBeforeRemove);
  inherited;
end;

class function TObjekt.GetAfterListener: TGUID;
begin
  Result := IAfterObjektListener;
end;

class function TObjekt.GetBeforeListener: TGUID;
begin
  Result := IBeforeObjektListener;
end;

function TObjekt.AddAttribute(const NewName: string): TAttribute;
begin
  Result := nil;
  for var I := 0 to FFeatures.Count - 1 do
  begin
    var
    Attribute := FFeatures[I] as TAttribute;
    if Attribute.Name = NewName then
    begin
      Result := Attribute;
      Break;
    end;
  end;
  if not Assigned(Result) then
  begin
    Result := TAttribute.Create(Self);
    Result.FName := NewName;
    FFeatures.Add(Result);
  end;
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

function TObjekt.AddOperation(const NewName: string): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
end;

procedure TObjekt.RefreshEntities;
begin
  Fire(mtAfterAddChild, nil);
end;

function TObjekt.GetTyp: TClass;
begin
  Result := FClass;
end;

function TObjekt.GenericName: string;
begin
  Result := FClass.GenericName;
  if Result = '' then
    Result := FClass.Name;
end;

{ TParameter }

constructor TParameter.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FTypeClassifier := nil;
  FUsedForAttribute := False;
end;

class function TParameter.GetAfterListener: TGUID;
begin
  Result := IAfterParameterListener;
end;

class function TParameter.GetBeforeListener: TGUID;
begin
  Result := IBeforeParameterListener;
end;

{ TOperation }

constructor TOperation.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FParameters := TObjectList.Create(True);
  FAttributes := TObjectList.Create(True);
  FReturnValue := nil;
end;

destructor TOperation.Destroy;
begin
  FreeAndNil(FParameters);
  FreeAndNil(FAttributes);
  // FreeAndNil(FReturnValue); FReturnValues belongs to FUnit.
  inherited;
end;

procedure TOperation.NewParameters;
begin
  FreeAndNil(FParameters);
  FParameters := TObjectList.Create(True);
end;

function TOperation.AddParameter(const NewName: string): TParameter;
begin
  Result := TParameter.Create(Self);
  Result.FName := NewName;
  FParameters.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FParameters.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

class function TOperation.GetAfterListener: TGUID;
begin
  Result := IAfterOperationListener;
end;

class function TOperation.GetBeforeListener: TGUID;
begin
  Result := IBeforeOperationListener;
end;

procedure TOperation.SetOperationType(const Value: TOperationType);
begin
  var
  Old := FOperationType;
  if Old <> Value then
  begin
    FOperationType := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FOperationType := Old;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

procedure TOperation.SetReturnValue(const Value: TClassifier);
begin
  var
  Old := FReturnValue;
  if Old <> Value then
  begin
    FReturnValue := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FReturnValue := Old;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

function TOperation.GetParameters: IModelIterator;
begin
  Result := TModelIterator.Create(FParameters);
end;

function TOperation.AddAttribute(const NewName: string;
  TypeClass: TClassifier): TAttribute;
begin
  Result := nil;
  try
    Result := TAttribute.Create(Self);
    Result.FTypeClassifier := TypeClass;
    Result.FName := NewName;
    FAttributes.Add(Result);
    Fire(mtBeforeAddChild, Result);
    Fire(mtAfterAddChild, Result);
  except
    on e: Exception do
      FConfiguration.Log('TOperation.AddAttribute', e);
  end;
end;

function TOperation.GetAttributes: IModelIterator;
begin
  Result := TModelIterator.Create(FAttributes);
end;

function TOperation.ToTypeName: string;
var
  Str: string;
  It2: IModelIterator;
  Parameter: TParameter;
begin
  Str := '(';
  It2 := GetParameters;
  while It2.HasNext do
  begin
    Parameter := It2.Next as TParameter;
    if Assigned(Parameter.TypeClassifier) then
      Str := Str + Parameter.TypeClassifier.GetShortType + ' ' +
        Parameter.Name + ', ';
  end;
  if Copy(Str, Length(Str) - 1, 2) = ', ' then
    Delete(Str, Length(Str) - 1, 2);
  Str := Str + ')';
  case OperationType of
    otConstructor:
      Result := Name + Str;
    otProcedure:
      Result := 'void ' + Name + Str;
    otFunction:
      Result := ReturnValue.GetShortType + ' ' + Name + Str;
  end;
end;

function TOperation.ToNameParameterTyp: string;
var
  Str: string;
  It2: IModelIterator;
  Parameter: TParameter;
begin
  Str := '(';
  It2 := GetParameters;
  while It2.HasNext do
  begin
    Parameter := It2.Next as TParameter;
    if Assigned(Parameter.TypeClassifier) then
      Str := Str + Parameter.Name + ': ' +
        Parameter.TypeClassifier.GetShortType + ', ';
  end;
  if Copy(Str, Length(Str) - 1, 2) = ', ' then
    Delete(Str, Length(Str) - 1, 2);
  Str := Str + ')';
  case OperationType of
    otConstructor:
      Result := Name + Str;
    otProcedure:
      Result := Name + Str;
    otFunction:
      Result := Name + Str + ': ' + ReturnValue.GetShortType;
  end;
end;

function TOperation.ToJava: string;
var
  Str, Vis: string;
begin
  Str := ToTypeName;
  if OperationType <> otConstructor then
  begin
    if IsAbstract then
      Str := 'abstract ' + Str;
    if Static then
      Str := 'static ' + Str;
  end;
  Vis := VisibilityAsString(Visibility);
  if Vis = '' then
    Result := Str
  else
    Result := Vis + ' ' + Str;
end;

function TOperation.HasMain: Boolean;
var
  Str: string;
  It2: IModelIterator;
  Parameter: TParameter;
begin
  Str := '(';
  It2 := GetParameters;
  while It2.HasNext do
  begin
    Parameter := It2.Next as TParameter;
    if Assigned(Parameter.TypeClassifier) then
      Str := Str + Parameter.TypeClassifier.GetShortType + ', ';
  end;
  if Copy(Str, Length(Str) - 1, 2) = ', ' then
    Delete(Str, Length(Str) - 1, 2); // delete last comma
  Str := Str + ')';
  if OperationType = otProcedure then
  begin
    Str := 'void ' + Name + Str;
    if Static then
      Str := 'static ' + Str;
    Str := VisibilityAsString(Visibility) + ' ' + Str;
  end;
  Result := (Str = 'public static void main(String[])');
end;

procedure TOperation.SetAttributeScope(ScopeDepth, LineE: Integer);
begin
  var
  Ite := GetAttributes;
  while Ite.HasNext do
  begin
    var
    Attribute := Ite.Next as TAttribute;
    if (Attribute.LineE = 0) and (Attribute.ScopeDepth = ScopeDepth) then
      Attribute.LineE := LineE;
  end;
end;

function TOperation.GetFormattedDescription: string;
begin
  Result := ReplaceStr(Documentation.Description, #13#10, '<br>');
end;

{ TAttribute }

constructor TAttribute.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FTypeClassifier := nil;
  FConnected := False;
  FValue := '';
end;

class function TAttribute.GetAfterListener: TGUID;
begin
  Result := IAfterAttributeListener;
end;

class function TAttribute.GetBeforeListener: TGUID;
begin
  Result := IBeforeAttributeListener;
end;

procedure TAttribute.SetTypeClassifier(const Value: TClassifier);
begin
  var
  Old := FTypeClassifier;
  if Old <> Value then
  begin
    FTypeClassifier := Value;
    try
      Fire(mtBeforeEntityChange);
    except
      FTypeClassifier := Old;
    end;
    Fire(mtAfterEntityChange);
  end;
end;

procedure TAttribute.SetValue(const Value: string);
begin
  var
  OldValue := FValue;
  FValue := Value;
  try
    Fire(mtBeforeEntityChange);
  except
    FName := OldValue;
  end;
  Fire(mtAfterEntityChange);
end;

procedure TAttribute.SetConnected(Value: Boolean);
begin
  var
  OldValue := FConnected;
  FConnected := Value;
  try
    Fire(mtBeforeEntityChange);
  except
    FConnected := OldValue;
  end;
  Fire(mtAfterEntityChange);
end;

function TAttribute.ToTypeName: string;
begin
  if Assigned(FTypeClassifier) then
    Result := FTypeClassifier.GetShortType + ' ' + Name
  else
    Result := Name;
end;

function TAttribute.ToNameType: string;
begin
  try
    if Assigned(TypeClassifier) then
      Result := Name + ': ' + TypeClassifier.GetShortType
    else
      Result := Name;
  except
    Result := Name;
  end;
end;

function TAttribute.ToNameTypeValue: string;
begin
  Result := ToNameType;
  if Value <> '' then
    Result := Result + ' = ' + Value;
end;

function TAttribute.ToJava: string;
var
  Str, Vis, Val: string;
begin
  if Assigned(TypeClassifier) then
    Str := TypeClassifier.ShortName + ' ' + Name
  else
    Str := Name;
  if IsFinal then
    Str := 'final ' + Str;
  if Static then
    Str := 'static ' + Str;
  Vis := VisibilityAsString(Visibility);
  if Vis <> '' then
    Str := Vis + ' ' + Str;

  if Value <> '' then
  begin
    Val := Value;
    if Assigned(TypeClassifier) then
    begin
      if TypeClassifier.ShortName = 'char' then
      begin
        Val := ReplaceStr(Val, '''', '');
        if Val = '' then
          Val := ''''''
        else
          Val := '''' + Val[1] + '''';
      end
      else if TypeClassifier.ShortName = 'String' then
      begin
        Val := ReplaceStr(Val, '"', '');
        Val := '"' + Val + '"';
      end;
    end;
    Str := Str + ' = ' + Val;
  end;
  Result := StringTimesN(FConfiguration.Indent1, ScopeDepth) + Str + ';';
end;

{ TProperty }

class function TProperty.GetAfterListener: TGUID;
begin
  Result := IAfterPropertyListener;
end;

class function TProperty.GetBeforeListener: TGUID;
begin
  Result := IBeforePropertyListener;
end;

{ TClassifier }

constructor TClassifier.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FFeatures := TObjectList.Create(True);
  FGenericName := '';
  FInner := False;
  FSourceRead := False;
end;

destructor TClassifier.Destroy;
begin
  FreeAndNil(FFeatures);
  inherited;
end;

function TClassifier.GetFeatures: IModelIterator;
begin
  Result := TModelIterator.Create(FFeatures);
end;

function TClassifier.GetShortType: string;
begin
  try
    Result := ShortName;
  except
    on E: Exception do
      OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' + E.Message));
  end;
end;

function TClassifier.GetAttributes: IModelIterator;
begin
  Result := TModelIterator.Create(GetFeatures, TAttribute);
end;

function TClassifier.GetAllAttributes: IModelIterator;
begin
  Result := TModelIterator.Create(GetFeatures, TAttribute, Low(TVisibility),
    ioNone, True);
end;

function TClassifier.GetOperations: IModelIterator;
begin
  Result := TModelIterator.Create(GetFeatures, TOperation);
end;

function TClassifier.GetName: string;
begin
  Result := Name;
  if (FGenericName <> '') or (Length(Name) = 1) then
    Result := 'java.lang.Object';
end;

function TClassifier.IsReference: Boolean;
begin
  Result := not IsSimpleTypeOrString(Name);
end;

function TClassifier.GetAncestorName: string;
begin
  Result := '';
end;

procedure TClassifier.SetCapacity(Capacity: Integer);
begin
  FFeatures.Capacity := Capacity;
end;

{ TInterface }

constructor TInterface.Create(Owner: TModelEntity);
begin
  inherited Create(Owner);
  FExtends := TObjectList.Create(False); // Only reference
  FAncestor := nil;
end;

destructor TInterface.Destroy;
begin
  inherited;
  FreeAndNil(FExtends);
end;

function TInterface.MakeOperation(const NewName: string;
  TypeClass: TClassifier): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue := TypeClass;
end;

procedure TInterface.AddOperation(Operation: TOperation);
begin
  FFeatures.Add(Operation);
  try
    Fire(mtBeforeAddChild, Operation);
  except
    FFeatures.Remove(Operation);
  end;
  Fire(mtAfterAddChild, Operation);
end;

function TInterface.AddOperationWithoutType(const NewName: string)
  : TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue := nil;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

class function TInterface.GetAfterListener: TGUID;
begin
  Result := IAfterInterfaceListener;
end;

class function TInterface.GetBeforeListener: TGUID;
begin
  Result := IBeforeInterfaceListener;
end;

function TInterface.AddExtends(Int: TInterface): TInterface;
begin
  Result := Int;
  FExtends.Add(Int);
end;

procedure TInterface.ViewExtends(Int: TInterface);
begin
  try
    Fire(mtBeforeAddChild, Int);
  except
    FExtends.Remove(Int);
  end;
  Fire(mtAfterAddChild, Int);
end;

function TInterface.GetExtends: IModelIterator;
begin
  Result := TModelIterator.Create(FExtends);
end;

procedure TInterface.SetAncestor(const Value: TInterface);
begin
  if Value = Self then
    FAncestor := nil
  else
    FAncestor := Value;
end;

// Returns a list of classes that implements this interface.
function TInterface.GetImplementingClasses: IModelIterator;
begin
  Result := TModelIterator.Create((Root as TLogicPackage).GetAllClassifiers,
    TInterfaceImplementsFilter.Create(Self));
end;

{
  Finds an attribute with same name and type.
}
function TInterface.FindAttribute(Name, AType: string): TAttribute;
begin
  Result := nil;
  var
  MIte := GetAttributes;
  while MIte.HasNext do
  begin
    var
    Attribute := MIte.Next as TAttribute;
    if (CompareText(Name, Attribute.Name) = 0) and
      (CompareText(AType, Attribute.TypeClassifier.Name) = 0) then
      Exit(Attribute);
  end;
end;

function TInterface.FindOperation(Operation: TOperation): TOperation;
var
  MIte, Omi1, Omi2: IModelIterator;
  Operation2: TOperation;
begin
  Result := nil;
  MIte := GetOperations;
  while MIte.HasNext do
  begin
    Operation2 := MIte.Next as TOperation;
    // Compare nr of parameters
    if Operation.FParameters.Count <> Operation2.FParameters.Count then
      Continue;
    // Compare operation name
    if CompareText(Operation.Name, Operation2.Name) <> 0 then
      Continue;
    // Compare parameters
    Omi1 := Operation.GetParameters;
    Omi2 := Operation2.GetParameters;
    var Okay:= True;
    while Okay and Omi1.HasNext do
      if CompareText((Omi1.Next as TParameter).Name, (Omi2.Next as TParameter).Name) <> 0 then
        Okay:= False;
    if not Okay then
      Continue;
    // Ok, match
    Result := Operation2;
    Break;
  end;
end;

function TInterface.AddAttribute(const NewName: string;
  TypeClass: TClassifier): TAttribute;
begin
  if Assigned(TypeClass) then
    Result := FindAttribute(NewName, TypeClass.Name)
  else
    Result := nil;
  if not Assigned(Result) then
  begin
    Result := TAttribute.Create(Self);
    Result.FName := NewName;
    Result.FTypeClassifier := TypeClass;
    FFeatures.Add(Result);
    try
      Fire(mtBeforeAddChild, Result);
    except
      FFeatures.Remove(Result);
    end;
    Fire(mtAfterAddChild, Result);
  end;
end;

function TInterface.GetAncestorName: string;
begin
  if Assigned(Ancestor) then
    Result := Ancestor.Name
  else
    Result := '';
end;

{ TDataType }

class function TDataType.GetAfterListener: TGUID;
begin
  Result := IBeforeInterfaceListener;
end;

class function TDataType.GetBeforeListener: TGUID;
begin
  Result := IAfterInterfaceListener;
end;

{ TAbstractPackage }

function TAbstractPackage.GetConfigFile: string;
begin
  Result := FConfigFile;
  if (Result = '') and Assigned(FOwner) then
    Result := (Owner as TAbstractPackage).GetConfigFile;
end;

procedure TAbstractPackage.SetConfigFile(const Value: string);
begin
  if Value <> '' then
    FConfigFile := ChangeFileExt(Value, ConfigFileExt);
end;

{ TClassDescendantFilter }

constructor TClassDescendantFilter.Create(Ancestor: TClass);
begin
  inherited Create;
  Self.FAncestor := Ancestor;
end;

// Returns true if M inherits from ancestor
function TClassDescendantFilter.Accept(Model: TModelEntity): Boolean;
begin
  Result := (Model is TClass) and ((Model as TClass).FAncestor = FAncestor);
end;

{ TInterfaceImplementsFilter }

constructor TInterfaceImplementsFilter.Create(Intf: TInterface);
begin
  inherited Create;
  Self.FIntf := Intf;
end;

// Returns true if M implements interface Int
function TInterfaceImplementsFilter.Accept(Model: TModelEntity): Boolean;
begin
  Result := (Model is TClass) and ((Model as TClass).FImplements.IndexOf(FIntf) <> -1);
end;

var
  GAllClassesPackage: TAbstractPackage = nil;

  // Unique Flag-instance, if Integrator.CurrentEntity=AllClassesPackage then show all classes
function AllClassesPackage: TAbstractPackage;
begin
  if not Assigned(GAllClassesPackage) then
    GAllClassesPackage := TAbstractPackage.Create(nil);
  Result := GAllClassesPackage;
end;

procedure AllClassesPackageClose;
begin
  FreeAndNil(GAllClassesPackage);
end;

end.
