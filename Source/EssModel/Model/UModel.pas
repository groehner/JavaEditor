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

uses Contnrs, Classes, uListeners, uModelEntity, UUtils;

const
  UNKNOWNPACKAGE_NAME = '<<Unknown>>';
  ConfigFileExt = '.uml';

type
  TLogicPackage = class;
  TUnitPackage = class;

  TOperationType = (otConstructor, otProcedure, otFunction, otDestructor);

  TObjectModel = class
  private
    Listeners: TInterfaceList;
    FModelRoot: TLogicPackage;
    FUnknownPackage: TUnitPackage;
    FLocked: integer;
    procedure CreatePackages;
  public
    NameCache: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
    procedure AddListener(NewListener: IUnknown);
    procedure RemoveListener(Listener: IUnknown);
    procedure ClearListeners;
    procedure Clear;
    procedure Lock;
    procedure Unlock;
    function debug: string;  // use it in TFJava.OpenUMLWindow/TFJava.DoOpenInUMLWindow
    property ModelRoot: TLogicPackage read FModelRoot;
    property Locked: integer read FLocked write FLocked;
    property UnknownPackage: TUnitPackage read FUnknownPackage;
  end;

  TFeature = class(TModelEntity);

  TClassifier = class(TModelEntity)
  private
    FFeatures: TObjectList;
  public
    Pathname: string;
    Importname: string;
    aGeneric: string;
    GenericName: string;
    Inner: boolean;
    Anonym: boolean;
    SourceRead: boolean;
    constructor Create(aOwner: TModelEntity); override;
    destructor Destroy; override;
    function GetFeatures : IModelIterator;
    function GetAttributes : IModelIterator;
    function GetAllAttributes: IModelIterator;
    function GetOperations : IModelIterator;
    function GetShortType: string;
    function getName: string;
    function isReference: boolean;
    function getAncestorName: string; virtual;
    procedure setCapacity(capacity: integer);
  end;

  TParameter = class(TModelEntity)
  private
    FTypeClassifier : TClassifier;
    FUsedForAttribute: boolean;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(aOwner: TModelEntity); override;
    property TypeClassifier : TClassifier read FTypeClassifier write FTypeClassifier;
    property UsedForAttribute : Boolean read FUsedForAttribute write FUsedForAttribute;
  end;

  TAttribute = class(TFeature)
  private
    FTypeClassifier: TClassifier;  // reference
    FValue: string;
    FConnected: boolean;
    procedure SetTypeClassifier(const Value: TClassifier);
    procedure SetValue(const Value: string);
    procedure SetConnected(Value: boolean);
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(aOwner: TModelEntity); override;
    function toTypeName: string;
    function toNameType: string;
    function toNameTypeValue: string;
    function toJava: string;
    property TypeClassifier: TClassifier read FTypeClassifier write SetTypeClassifier;
    property Value: string read FValue write SetValue;
    property Connected: boolean read FConnected write SetConnected;
  end;

  TOperation = class(TFeature)
  private
    FOperationType: TOperationType;
    FParameters: TObjectList;
    FReturnValue: TClassifier;
    FAttributes: TObjectList; // local variables
    procedure SetOperationType(const Value: TOperationType);
    procedure SetReturnValue(const Value: TClassifier);
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    hasSourceCode: boolean;
    hasComment: boolean;
    Parentname: string;
    Annotation: string;
    constructor Create(aOwner: TModelEntity); override;
    destructor Destroy; override;
    procedure NewParameters;
    function AddParameter(const NewName: string): TParameter;
    function GetParameters : IModelIterator;
    function AddAttribute(const NewName: string; TypeClass: TClassifier): TAttribute; // local variables
    function GetAttributes : IModelIterator;
    function toTypeName: string;
    function toNameParameterTyp: string;
    function toJava: string;
    function hasMain: boolean;
    procedure setAttributeScope(aScopeDepth, aLineE: integer);
    function getFormattedDescription: string;
    property OperationType: TOperationType read FOperationType write SetOperationType;
    property ReturnValue: TClassifier read FReturnValue write SetReturnValue;
  end;

  TProperty = class(TAttribute)
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  end;

  TDataType = class(TClassifier)
    {From UML-spec: A descriptor of a set of values that lack identity and whose
    operations do not have side effects. Datatypes include
    primitive pre-defined types and user-definable types. Pre-defined
    types include numbers, string and time. User-definable
    types include enumerations.}
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
    constructor Create(aOwner: TModelEntity); override;
    destructor Destroy; override;
    function MakeOperation(const NewName: string; TypeClass: TClassifier): TOperation;
    procedure AddOperation(Operation: TOperation);
    function AddAttribute(const NewName: string; TypeClass: TClassifier): TAttribute;
    function AddExtends(I: TInterface): TInterface;
    procedure ViewExtends(I: TInterface);
    function GetExtends : IModelIterator;
    function GetImplementingClasses : IModelIterator;
    function FindAttribute(Name, aType: string): TAttribute;
    function FindOperation(O: TOperation): TOperation;
    function getAncestorName: string; override;
    function AddOperationWithoutType(const NewName: string): TOperation;
    property Ancestor: TInterface read FAncestor write SetAncestor;
  end;

  TClass = class(TClassifier, IBeforeClassListener)
  private
    FAncestor: TClass;
    FImplements: TObjectList;
    FisJUnitTestClass: boolean;
    procedure SetAncestor(const Value: TClass);
    //Ancestorlisteners
    procedure AncestorChange(Sender: TModelEntity);
    procedure AncestorAddChild(Sender: TModelEntity; NewChild: TModelEntity);
    procedure AncestorRemove(Sender: TModelEntity);
    procedure AncestorEntityChange(Sender: TModelEntity);
    function AncestorQueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; stdcall;
    procedure IBeforeClassListener.Change = AncestorChange;
    procedure IBeforeClassListener.EntityChange = AncestorEntityChange;
    procedure IBeforeClassListener.AddChild = AncestorAddChild;
    procedure IBeforeClassListener.Remove = AncestorRemove;
    function IBeforeClassListener.QueryInterface = AncestorQueryInterface;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(aOwner: TModelEntity); override;
    destructor Destroy; override;
    function MakeOperation(const NewName: string; TypeClass: TClassifier): TOperation;
    procedure AddOperation(Operation: TOperation);
    function AddOperationWithoutType(const NewName: string): TOperation;
    function AddAttribute(const NewName: string; TypeClass: TClassifier): TAttribute;
    function AddProperty(const NewName: string): TProperty;
    function AddImplements(I: TInterface): TInterface;
    procedure ViewImplements(I: TInterface);
    function GetImplements : IModelIterator;
    function GetDescendants : IModelIterator;
    function FindOperation(O : TOperation) : TOperation;
    function FindAttribute(Name, aType: string): TAttribute;
    function GetTyp: string;
    function getAncestorName: string; override;
    property Ancestor: TClass read FAncestor write SetAncestor;
    property isJUnitTestClass: boolean read FisJUnitTestClass write FisJUnitTestClass;
  end;

  TJUnitClass = class(TClass, IBeforeClassListener)
  end;

  TObjekt = class(TClassifier)
  private
    aClass: TClass;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    constructor Create(aOwner: TModelEntity); override;
    destructor Destroy; override;
    function AddAttribute(const NewName: string): TAttribute;
    function AddOperation(const NewName: string): TOperation;
    procedure RefreshEntities;
    function getTyp: TClass;
    function GenericName: string;
  end;

  TAbstractPackage = class(TModelEntity)
  private
    ConfigFile : string;
  public
    procedure SetConfigFile(const Value : string);
    function GetConfigFile : string;
  end;

  //Represents the link between one package that uses another
  TUnitDependency = class(TModelEntity)
  public
    myPackage : TUnitPackage;
  end;

  TUnitPackage = class(TAbstractPackage)
  private
    FClassifiers: TObjectList;
    FUnitDependencies: TObjectList;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    ImportStartline: integer;
    ImportEndline: integer;
    FullImports: TStringList;
    ClassImports: TStringList;
    constructor Create(aOwner: TModelEntity); override;
    destructor Destroy; override;
    function MakeClass(const NewName, Filename: string): TClass;
    procedure AddClass(const aClass: TClass);
    procedure AddClassWithoutShowing(const aClass: TClass);
    function MakeInterface(const NewName, filename: string): TInterface;
    procedure AddInterface(const aInterface: TInterface);
    procedure AddInterfaceWithoutShowing(const aInterface: TInterface);    
    function AddDatatype(const NewName: string): TDataType;
    function AddObject(const NewName: string; aClass: TClass): TObjekt;
    procedure DeleteObject(aName: string);
    function GetObject(const aName: string): TObjekt;
    function AddUnitDependency(U : TUnitPackage; aVisibility : TVisibility): TUnitDependency;
    function FindClassifier(const CName: string; TheClass : TModelEntityClass = nil;
              CaseSense : boolean = False): TClassifier;
    function FindClass(const Pathname: string): TClassifier;
    function GetClassifiers : IModelIterator;
    function GetUnitDependencies : IModelIterator;
    function GetObjects(const Typ: string): TStringList;
    function GetAllObjects: TStringList;
    function GetObjekt(const aName, Typ: string): TObjekt;
    function Debug: string;
  end;

  TLogicPackage = class(TAbstractPackage)
  private
    FPackages: TObjectList;
  protected
    class function GetBeforeListener: TGUID; override;
    class function GetAfterListener: TGUID; override;
  public
    Files: TStringList;
    constructor Create(aOwner: TModelEntity); override;
    destructor Destroy; override;
    procedure Clear;
    function AddUnit(const NewUnitName: string): TUnitPackage;
    // might need a AddLogicPackage also
    function FindUnitPackage(const PName: string; CaseSense : boolean = False): TUnitPackage;
    function GetPackages : IModelIterator;
    function GetAllUnitPackages : IModelIterator;
    function GetAllClassifiers : IModelIterator;
    function Debug: string;
  end;

  function AllClassesPackage : TAbstractPackage;
  procedure AllClassesPackageClose;

implementation

uses StrUtils, uIterators, SysUtils, Types, UConfiguration;

type
  //Used by Class.GetDescendant
  TClassDescendantFilter = class(TIteratorFilter)
  private
    Ancestor : TClass;
  public
    constructor Create(aAncestor : TClass);
    function Accept(M : TModelEntity) : boolean; override;
  end;

  //Used by Interface.GetImplementingClasses
  TInterfaceImplementsFilter = class(TIteratorFilter)
  private
    Int : TInterface;
  public
    constructor Create(I : TInterface);
    function Accept(M : TModelEntity) : boolean; override;
  end;

  TStrCompare = function(const S1, S2: string): Integer;

const
  CompareFunc : array[boolean] of TStrCompare = (CompareText, CompareStr);

{ TObjectModel }

constructor TObjectModel.Create;
begin
  Listeners := TInterfaceList.Create;
  NameCache:= TStringList.Create;
  CreatePackages;
  // CreatePackages:
  //   FModelRoot := TLogicPackage.Create(nil);
  //   FUnknownPackage := FModelRoot.AddUnit(UNKNOWNPACKAGE_NAME);
end;

destructor TObjectModel.Destroy;
begin
  FreeAndNil(FModelRoot);
  // FUnknownPackage will be freed by FModelRoot who owns it
  FreeAndNil(Listeners);
  for var i:= 0 to NameCache.Count - 1 do begin
    var aClassifier:= TClassifier(NameCache.Objects[i]);
    FreeAndNil(aClassifier);
  end;
  FreeAndNil(NameCache);
  inherited;
end;

procedure TObjectModel.Clear;
begin
  //Model must be locked, otherwise events will be fired back to
  //backend and diagram.
  for var i:= 0 to NameCache.Count - 1 do
    FreeAndNil(NameCache.Objects[i]);
  NameCache.Clear;
  FModelRoot.Clear;  // destroys UUnknownPackage
  FUnknownPackage := FModelRoot.AddUnit(UNKNOWNPACKAGE_NAME);

{  if FLocked = 0 then begin
    Lock;
    FreeAndNil(FModelRoot);
    CreatePackages;
    UnLock;
  end else begin
    FreeAndNil(FModelRoot);
    CreatePackages;
  end;}
end;

procedure TObjectModel.Fire(Method: TListenerMethodType; Info: TModelEntity = nil);
var
  I: integer;
  L,Dum: IUnknown;
begin
  if (FLocked = 0) and assigned(Listeners) then
    for I := 0 to Listeners.Count - 1 do begin
      L := Listeners[I];
      case Method of
        //BeforeChange is triggered when the model will be changed from the root-level.
        mtBeforeChange:
           if L.QueryInterface(IBeforeObjectModelListener,Dum) = 0 then
             (L as IBeforeObjectModelListener).Change(nil);
        //AfterChange is triggered when the model has been changed from the root-level.
        mtAfterChange:
           if L.QueryInterface(IAfterObjectModelListener,Dum) = 0 then
             (L as IAfterObjectModelListener).Change(nil);
      end;
    end;
end;

procedure TObjectModel.Lock;
begin
  Fire(mtBeforeChange);
  inc(FLocked);
  ModelRoot.Locked := True;
end;

procedure TObjectModel.Unlock;
begin
  dec(FLocked);
  if FLocked = 0 then begin
    ModelRoot.Locked := False;
    Fire(mtAfterChange);
  end;
end;

function TObjectModel.debug: string;
begin
  Result:= FModelRoot.debug + FUnknownPackage.debug;
end;

procedure TObjectModel.CreatePackages;
begin
  //Creates the default packages that must exist
  FModelRoot := TLogicPackage.Create(nil);
  FUnknownPackage := FModelRoot.AddUnit(UNKNOWNPACKAGE_NAME);
end;

procedure TObjectModel.AddListener(NewListener: IUnknown);
begin
  if Listeners.IndexOf(NewListener) = -1 then
    Listeners.Add(NewListener);
end;

procedure TObjectModel.RemoveListener(Listener: IUnknown);
begin
  if assigned(Listeners) then
    Listeners.Remove(Listener);
end;

procedure TObjectModel.ClearListeners;
begin
  if assigned(Listeners) then
    Listeners.Clear;
end;

{ TLogicPackage }

constructor TLogicPackage.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FPackages := TObjectList.Create(True);
  Files:= TStringList.Create;
  Files.Duplicates:= dupIgnore;
  Files.Sorted:= true;
end;

destructor TLogicPackage.Destroy;
begin
  FreeAndNil(FPackages);
  FreeAndNil(Files);
  inherited;
end;

procedure TLogicPackage.Clear;
begin
  Files.Clear;
  FPackages.Clear;
end;

function TLogicPackage.AddUnit(const NewUnitName: string): TUnitPackage;
begin
  Result := TUnitPackage.Create(Self);
  Result.FName := NewUnitName;
  FPackages.Add(Result);
  try
    Fire(mtBeforeAddChild, Result)
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

//Searches in this and dependant logic packages after a unit with name PName.
function TLogicPackage.FindUnitPackage(const PName: string; CaseSense : boolean = False): TUnitPackage;
var
  I: integer;
  P: TAbstractPackage;
  F : TStrCompare;
begin
  F := CompareFunc[CaseSense];
  Result := nil;
  for I := 0 to FPackages.Count - 1 do begin
    P := FPackages[I] as TAbstractPackage;
    if P is TLogicPackage then begin
      Result := (P as TLogicPackage).FindUnitPackage(PName);
      if Assigned(Result) then
        Exit;
    end
    else if P is TUnitPackage then begin
      if F(P.Name, PName) = 0 then begin
        Result := P as TUnitPackage;
        Exit;
      end;
    end;
  end;
end;

function TLogicPackage.GetPackages: IModelIterator;
begin
  // Got TObjectList expected IModellIterator
  var ObjList:= TObjectList(FPackages);
  Result := TModelIterator.Create(ObjList);
end;

//Returns all unitpackages in and below this logic package.
//Unknownpackage is excluded.
function TLogicPackage.GetAllUnitPackages: IModelIterator;

  var List: TObjectList;

  procedure InAddNested(L : TLogicPackage);
  begin
    var Mi := L.GetPackages;
    while Mi.HasNext do begin
      var P:= Mi.Next;
      if P is TLogicPackage then
        InAddNested(P as TLogicPackage)
      else //Not logicpackage, must be unitpackage.
        if (P.Name <> UNKNOWNPACKAGE_NAME) then List.Add( P );
    end;
  end;

begin
  List := TObjectList.Create(False);
  try
    try
      InAddNested(Self);
    except
    end;
    Result := TModelIterator.Create(List, true);
  finally
    FreeAndNil(List);
  end;
end;

//Returns all classifiers in and below this logic package.
function TLogicPackage.GetAllClassifiers: IModelIterator;
var
  Pmi,Cmi : IModelIterator;
  List : TObjectList;
begin
  List := TObjectList.Create(False);
  try
    Pmi := GetAllUnitPackages;
    while Pmi.HasNext do begin
      Cmi := (Pmi.Next as TUnitPackage).GetClassifiers;
      while Cmi.HasNext do
        List.Add(Cmi.Next);
    end;
    Result := TModelIterator.Create(List, true);
  finally
    FreeAndNil(List);
  end;
end;

function TLogicPackage.Debug: string;
  var SL: TStringList; i: integer;
      Ci, Ai, Oi, Pi: IModelIterator;
      cent: TClassifier;
      Attribute: TAttribute;
      Operation: TOperation;
begin
 // use it in TFJava.OpenUMLWindow/TFJava.DoOpenInUMLWindow
  i:= 0;
  SL:= TStringList.Create;
  SL.Add('LogicPackage ' + Name);
  SL.Add('Packages');
  Pi:= GetAllUnitPackages;
  while Pi.HasNext do begin
    SL.Add((Pi.Next as TUnitPackage).Name);
  end;
  SL.Add('');
  SL.Add('Classes');
  Ci:= GetAllClassifiers;
  while Ci.HasNext do begin
    cent:= TClassifier(Ci.Next);
    inc(i);
    SL.Add('#' + IntTostr(i) + ' ' + Cent.Name + ' - ' + Cent.Importname + ' - ' + Cent.Pathname);
    if (Cent is TClass) and assigned((Cent as TClass).Ancestor) then
      SL.Add('Ancestor: ' + (Cent as TClass).Ancestor.Name);
    SL.Add('--- Attributes ---');
    Ai:= Cent.GetAllAttributes;
    while Ai.HasNext do begin
      Attribute:= ai.Next as TAttribute;
      SL.Add(Attribute.toTypeName);
    end;
    SL.Add('--- Operations ---');
    oi:= cent.GetOperations;
    while oi.HasNext do begin
      Operation:= oi.Next as TOperation;
      SL.Add(Operation.toJava);
    end;
    SL.Add('-------------------------');
  end;
  SL.Add('');
  SL.Add('Files: ');
  SL.Add(Files.Text);
  SL.Add('-------------------------');

  Result:= SL.Text;
  FreeAndNil(SL);
end;

{ TUnitPackage }

constructor TUnitPackage.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FClassifiers := TObjectList.Create(True);
  FUnitDependencies := TObjectList.Create(True);
  FullImports:= TStringList.Create;
  FullImports.Add('java.lang.');
  ClassImports:= TStringList.Create;
end;

destructor TUnitPackage.Destroy;
begin
  FreeAndNil(ClassImports);
  FreeAndNil(FullImports);
  FreeAndNil(FUnitDependencies);
  FreeAndNil(FClassifiers);
  inherited;
end;

function TUnitPackage.MakeClass(const NewName, Filename: string): TClass;
begin
  var aClass:= FindClassifier(NewName, TClass, true);
  if aClass = nil then begin
    aClass:= TClass.Create(Self);
    aClass.Name:= NewName;  // sets package too
    AddClass(aClass as TClass);
  end;
  aClass.Pathname:= Filename;
  Result:= (aClass as TClass);
end;

procedure TUnitPackage.AddClass(const aClass: TClass);
begin
  if not assigned(FindClassifier(aClass.Name, TClass, true)) then
    FClassifiers.Add(aClass);
  Fire(mtBeforeAddChild, aClass);
  Fire(mtAfterAddChild, aClass);
end;

procedure TUnitPackage.AddClassWithoutShowing(const aClass: TClass);
begin
  if assigned(FindClassifier(aClass.Name, TClass, true)) then exit;
  FClassifiers.Add(aClass);
end;

function TUnitPackage.MakeInterface(const NewName, Filename: string): TInterface;
begin
  Result := TInterface.Create(Self);
  Result.FName := NewName;
  Result.Pathname:= Filename;
end;

procedure TUnitPackage.AddInterface(const aInterface: TInterface);
begin
  FClassifiers.Add(aInterface);
  try
    Fire(mtBeforeAddChild, aInterface);
  except
    FClassifiers.Remove(aInterface);
  end;
  Fire(mtAfterAddChild, aInterface);
end;

procedure TUnitPackage.AddInterfaceWithoutShowing(const aInterface: TInterface);
begin
  FClassifiers.Add(aInterface);
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

function TUnitPackage.AddObject(const NewName: string; aClass: TClass): TObjekt;
begin
  try
    Result:= GetObjekt(NewName, aClass.Name);
  except
    Result:= nil;
    exit;
  end;
  if Result = nil then begin
    Result:= TObjekt.Create(Self);
    Result.FName:= NewName;
    Result.IsVisible:= true;
    Result.aClass:= aClass;
    FClassifiers.Add(Result);
  end;
  try
    Fire(mtBeforeAddChild, Result);
  except
    FClassifiers.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

function TUnitPackage.GetObjekt(const aName, Typ: string): TObjekt;
  var it: TModelIterator; aClassifier: TClassifier; aObjekt: TObjekt;
begin
  Result:= nil;
  it:= TModelIterator.Create(GetClassifiers);
  try
    while it.HasNext do begin
      aClassifier:= it.Next as TClassifier;
      if (aClassifier is TObjekt) then begin
        aObjekt:= (aClassifier as TObjekt);
        if (aClassifier.Name = aName) and (GetShortType(aObjekt.aClass.name) = Typ) then begin
          Result:= (aClassifier as TObjekt);
          break;
        end;
      end;
    end;
  finally
    FreeAndNil(it);
  end;
end;

function TUnitPackage.GetObjects(const Typ: string): TStringList;
begin
  Result:= TStringList.Create;
  var it:= TModelIterator.Create(GetClassifiers);
  try
    while it.HasNext do begin
      var aClassifier:= it.Next as TClassifier;
      if (aClassifier is TObjekt) and (GetShortType((aClassifier as TObjekt).aClass.name) = Typ) then
        Result.AddObject((aClassifier as TObjekt).FName, aClassifier);
    end;
  finally
    FreeAndNil(it);
  end;
end;

function TUnitPackage.GetAllObjects: TStringList;
begin
  Result:= TStringList.Create;
  var it:= TModelIterator.Create(GetClassifiers);
  try
    try
      while it.HasNext do begin
        var aClassifier:= it.Next as TClassifier;
        if aClassifier is TObjekt then
          Result.AddObject((aClassifier as TObjekt).FName, aClassifier);
      end;
    finally
      FreeAndNil(it);
    end;
  except
  end;
end;

procedure TUnitPackage.DeleteObject(aName: string);
begin
  var aObject:= FindClassifier(aName, nil, true);   // true because of casesensitive
  if assigned(aObject) then
    FClassifiers.Remove(aObject);
end;

function TUnitPackage.GetObject(const aName: string): TObjekt;
begin
  Result:= TObjekt(FindClassifier(aName, nil, false));
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
  TheClass : TModelEntityClass = nil; CaseSense : boolean = False): TClassifier;
var
  C : TClassifier;
  Mi : IModelIterator;
  P : TUnitPackage;
  F : TStrCompare;

  function InFind(P : TUnitPackage) : TClassifier;
    var Mi: IModelIterator; s: string;
  begin
    Result := nil;
    //Search in this unit
    if assigned(TheClass) then
      Mi := TModelIterator.Create( P.GetClassifiers , TheClass )
    else if assigned(P) then begin
      try
        Mi := P.GetClassifiers;
      except on e: exception do begin
        if assigned(p) then s:= P.classname else s:= 'p = nil';
        FConfiguration.Log('InFind(P : TUnitPackage): ' + s, e);
        exit;
       end;
      end;
    end else
      exit;
    while Mi.HasNext do begin
      C := Mi.Next as TClassifier;
      if F(C.Name, CName) = 0 then begin
        Result := C;
        Break;
      end;
    end;
  end;

begin
  Result:= nil;
  try
    F := CompareFunc[CaseSense];
    //Search in this unit
    Result := InFind(Self);
    //If nil search in public dependencies
    if not Assigned(Result) then begin
      Mi := GetUnitDependencies;
      while Mi.HasNext do begin
        P := (Mi.Next as TUnitDependency).myPackage;
        Result := InFind(P);
        if Assigned(Result) then
          Break;
      end;
    end;
  except on e: exception do
    FConfiguration.Log(' TUnitPackage.FindClassifier', e);
  end;
end;

function TUnitPackage.FindClass(const Pathname: string): TClassifier;
begin
  Result:= nil;
  var Mi:= GetClassifiers;
  while Mi.HasNext do begin
    var C:= Mi.Next as TClassifier;
    if C.Pathname = Pathname then begin
      Result:= C;
      Break;
    end;
  end;
end;

function TUnitPackage.GetClassifiers: IModelIterator;
begin
  try
    Result := TModelIterator.Create( FClassifiers );
  except
    Result:= nil;
  end;
end;

function TUnitPackage.AddUnitDependency(U: TUnitPackage; aVisibility: TVisibility): TUnitDependency;
begin
  Assert( (U<>Self) and (U<>nil), Classname + 'AddUnitDependency invalid parameter');
  Result := TUnitDependency.Create( Self );
  Result.myPackage := U;
  Result.Visibility := aVisibility;
  FUnitDependencies.Add( Result );
end;

function TUnitPackage.GetUnitDependencies: IModelIterator;
begin
  Result := TModelIterator.Create( FUnitDependencies );
end;

 // use it in TFJava.OpenUMLWindow/TFJava.DoOpenInUMLWindow
function TUnitPackage.Debug: string;
   var SL: TStringList;
      Ci, Ai, Oi: IModelIterator;
      cent: TClassifier;
      Attribute: TAttribute;
      Operation: TOperation;
begin
  SL:= TStringList.Create;
  SL.Add('UnitPackage: ' + Name);
  Ci:= GetClassifiers;
  while Ci.HasNext do begin
    cent:= TClassifier(Ci.Next);
    SL.Add(Cent.Name + ' - ' + Cent.Importname + ' - ' + Cent.Pathname);
    SL.Add('--- Attributes ---');
    Ai:= Cent.GetAllAttributes;
    while Ai.HasNext do begin
      Attribute:= ai.Next as TAttribute;
      SL.Add(Attribute.toTypeName);
    end;
    SL.Add('--- Operations ---');
    oi:= cent.GetOperations;
    while oi.HasNext do begin
      Operation:= oi.Next as TOperation;
      SL.Add(Operation.toJava);
    end;
    SL.Add('-------------------------');
  end;
  Result:= SL.Text;
  FreeAndNil(SL);
end;

{ TClass }

constructor TClass.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FAncestor:= nil;
  FImplements:= TObjectList.Create(False); //Only reference
end;

destructor TClass.Destroy;
begin
  //Dont touch listeners if the model is locked.
  if not Locked then begin
    Fire(mtBeforeRemove);
    //    if Assigned(FAncestor) then
    //      FAncestor.RemoveListener(IBeforeClassListener(Self));
  end;
  FreeAndNil(FImplements);
  inherited;
end;

function TClass.AddAttribute(const NewName: string; TypeClass: TClassifier): TAttribute;
begin
  if Assigned(TypeClass) then
    Result:= FindAttribute(NewName, TypeClass.Name)
  else
    Result:= nil;
  if Result = nil then begin
    Result := TAttribute.Create(Self);
    Result.FTypeClassifier:= TypeClass;
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

function TClass.AddOperationWithoutType(Const NewName: string): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue:= nil;
  FFeatures.Add(Result);
  try
    Fire(mtBeforeAddChild, Result);
  except
    FFeatures.Remove(Result);
  end;
  Fire(mtAfterAddChild, Result);
end;

function TClass.MakeOperation(const NewName: string; TypeClass: TClassifier): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue:= TypeClass;
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

function TClass.AddImplements(I: TInterface): TInterface;
begin
  Result := I;
  FImplements.Add(I);
end;

procedure TClass.ViewImplements(I: TInterface);
begin
  try
    Fire(mtBeforeAddChild, I);
  except
    FImplements.Remove(I);
  end;
  Fire(mtAfterAddChild, I);
end;

procedure TClass.SetAncestor(const Value: TClass);
begin
  if Value = Self then exit;
  if Value <> FAncestor then begin
    var Old := FAncestor;
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

function TClass.AncestorQueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then Result := S_OK
  else Result := E_NOINTERFACE
end;

function TClass.GetImplements: IModelIterator;
begin
  Result := TModelIterator.Create( FImplements );
end;

//Returns a list of classes that inherits from this class.
function TClass.GetDescendants: IModelIterator;
begin
  Result := TModelIterator.Create(
    (Root as TLogicPackage).GetAllClassifiers,
    TClassDescendantFilter.Create(Self) );
end;

{
  Finds an operation with same name and signature as parameter.
  Used by Delphi-parser to find a modelentity for a method implementation.
}
function TClass.FindOperation(O: TOperation): TOperation;
var
  Mi,Omi1,Omi2 : IModelIterator;
  O2 : TOperation;
  label Skip;
begin
  Assert(O <> nil, ClassName + '.FindOperation invalid parameter');
  Result := nil;
  Mi := GetOperations;
  while Mi.HasNext do begin
    O2 := Mi.Next as TOperation;
    //Compare nr of parameters
    if O.FParameters.Count<>O2.FParameters.Count then
      Continue;
    //Compare operation name
    if CompareText(O.Name,O2.Name)<>0 then
      Continue;
    //Compare parameters
    Omi1 := O.GetParameters;
    Omi2 := O2.GetParameters;
    while Omi1.HasNext do
      if CompareText((Omi1.Next as TParameter).Name,(Omi2.Next as TParameter).Name)<>0 then
        goto Skip;
    //Ok, match
    Result := O2;
    Break;
  Skip:
  end;
end;

{
  Finds an attribute with same name and type.
}
function TClass.FindAttribute(Name, aType: string): TAttribute;
begin
  Result := nil;
  var Mi := getAttributes;
  while Mi.HasNext do begin
    var A := Mi.Next as TAttribute;
    if (CompareText(Name, A.Name) = 0) and
       (CompareText(aType, A.TypeClassifier.Name) = 0) then
      exit(A);
  end;
end;

function TClass.GetTyp: string;
begin
  if Pathname <> ''
    then Result:= Name
    else Result:= ImportName;
end;

function TClass.getAncestorName: string;
begin
  if assigned(Ancestor)
    then Result:= Ancestor.Name
    else Result:= '';
end;

{ TObjekt }

constructor TObjekt.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
end;

destructor TObjekt.Destroy;
begin
  // Dont touch listeners if the model is locked.
  if not Locked then
    Fire(mtBeforeRemove);
  inherited;
end;

class function TObjekt.GetAfterListener: TGUID;
begin
  Result:= IAfterObjektListener;
end;

class function TObjekt.GetBeforeListener: TGUID;
begin
  Result:= IBeforeObjektListener;
end;

function TObjekt.AddAttribute(const NewName: string): TAttribute;
begin
  Result:= nil;
  for var i:= 0 to FFeatures.Count - 1 do begin
    var aAttribute:= FFeatures.Items[i] as TAttribute;
    if aAttribute.Name = NewName then begin
      Result:= aAttribute;
      break;
    end;
  end;
  if Result = nil then begin
    Result:= TAttribute.Create(Self);
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
  Result:= TOperation.Create(Self);
  Result.FName := NewName;
  FFeatures.Add(Result);
end;

procedure TObjekt.RefreshEntities;
begin
  Fire(mtAfterAddChild, nil);
end;

function TObjekt.getTyp: TClass;
begin
  Result:= aClass;
end;

function TObjekt.GenericName: string;
begin
  Result:= aClass.GenericName;
  if Result = '' then Result:= aClass.Name;
end;

{ TParameter }

constructor TParameter.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FTypeClassifier:= nil;
  FUsedForAttribute:= false;
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

constructor TOperation.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FParameters := TObjectList.Create(True);
  FAttributes := TObjectList.Create(True);
  FReturnValue:= nil;
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
  Result:= TParameter.Create(Self);
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
  var Old := FOperationType;
  if Old <> Value then begin
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
  var Old := FReturnValue;
  if Old <> Value then begin
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
  Result := TModelIterator.Create( FParameters );
end;

function TOperation.AddAttribute(const NewName: string; TypeClass: TClassifier): TAttribute;
begin
  Result:= nil;
  try
    Result := TAttribute.Create(Self);
    Result.FTypeClassifier:= TypeClass;
    Result.FName := NewName;
    FAttributes.Add(Result);
    Fire(mtBeforeAddChild, Result);
    Fire(mtAfterAddChild, Result);
  except on e: exception do begin
    // FAttributes.Remove(Result);
    FConfiguration.Log('TOperation.AddAttribute', e);
    end;
  end;
end;

function TOperation.GetAttributes: IModelIterator;
begin
  Result := TModelIterator.Create( FAttributes);
end;

function TOperation.toTypeName: string;
  var s: string; It2: IModelIterator; Parameter: TParameter;
begin
  s:= '(';
  it2:= GetParameters;
  while it2.HasNext do begin
    Parameter:= it2.next as TParameter;
    if assigned(Parameter.TypeClassifier) then
      s:= s + Parameter.TypeClassifier.GetShortType + ' ' + Parameter.Name + ', ';
  end;
  if Copy(s, length(s) - 1, 2) = ', ' then
    Delete(s, length(s) - 1, 2);
  s:= s + ')';
  case OperationType of
    otConstructor: Result:= Name + s;
    otProcedure:   Result:= 'void ' + Name + s;
    otFunction:    Result:= ReturnValue.GetShortType + ' ' + Name + s;
  end;
end;

function TOperation.toNameParameterTyp: string;
  var s: string; It2: IModelIterator; Parameter: TParameter;
begin
  s:= '(';
  it2:= GetParameters;
  while it2.HasNext do begin
    Parameter:= it2.next as TParameter;
    if assigned(Parameter.TypeClassifier) then
      s:= s + Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType + ', ';
  end;
  if Copy(s, length(s)-1, 2) = ', ' then
    Delete(s, length(s)-1, 2);
  s:= s + ')';
  case OperationType of
    otConstructor: Result:= Name + s;
    otProcedure:   Result:= Name + s;
    otFunction:    Result:= Name + s + ': ' + ReturnValue.GetShortType;
  end;
end;

function TOperation.toJava: string;
  var s, vis: string;
begin
  s:= toTypeName;
  if OperationType <> otConstructor then begin
    if IsAbstract then s:= 'abstract ' + s;
    if Static then s:= 'static ' + s;
  end;
  vis:=  VisibilityAsString(Visibility);
  if vis = ''
    then Result:= s
    else Result:= vis + ' ' + s;
end;

function TOperation.hasMain: boolean;
  var s: string; It2: IModelIterator; Parameter: TParameter;
begin
  s:= '(';
  it2:= GetParameters;
  while it2.HasNext do begin
    Parameter:= it2.next as TParameter;
    if assigned(Parameter.TypeClassifier) then
      s:= s + Parameter.TypeClassifier.GetShortType + ', ';
  end;
  if Copy(s, length(s)-1, 2) = ', ' then
    Delete(s, length(s)-1, 2); // delete last comma
  s:= s + ')';
  if OperationType = otProcedure then begin
    s:= 'void ' + Name + s;
    if Static then s:= 'static ' + s;
    s:= VisibilityAsString(Visibility) + ' ' + s;
  end;
  Result:= (s = 'public static void main(String[])');
end;

procedure TOperation.setAttributeScope(aScopeDepth, aLineE: integer);
begin
  var it:= GetAttributes;
  while it.HasNext do begin
    var Attribute:= it.next as TAttribute;
    if (Attribute.LineE = 0) and (Attribute.ScopeDepth = ScopeDepth) then
      Attribute.LineE:= LineE;
  end;
end;

function TOperation.getFormattedDescription: string;
begin
  Result:= ReplaceStr(Documentation.Description, #13#10, '<br>');
end;

{ TAttribute }

constructor TAttribute.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FTypeClassifier:= nil;
  FConnected:= false;
  FValue:= '';
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
  var Old := FTypeClassifier;
  if Old <> Value then begin
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
  var OldValue := FValue;
  FValue := Value;
  try
    Fire(mtBeforeEntityChange)
  except
    FName := OldValue;
  end;
  Fire(mtAfterEntityChange)
end;

procedure TAttribute.SetConnected(Value: boolean);
begin
  var OldValue := FConnected;
  FConnected := Value;
  try
    Fire(mtBeforeEntityChange)
  except
    FConnected := OldValue;
  end;
  Fire(mtAfterEntityChange)
end;

function TAttribute.toTypeName: string;
begin
  if assigned(FTypeClassifier)
    then Result:= FTypeClassifier.GetShortType + ' ' + Name
    else Result:= Name;
end;

function TAttribute.toNameType: string;
begin
  try
    if assigned(TypeClassifier)
      then result:= Name + ': ' + TypeClassifier.GetShortType
      else result:= Name;
  except
    result:= Name;
  end;
end;

function TAttribute.toNameTypeValue: string;
begin
  result:= toNameType;
  if Value <> '' then
    result:= result + ' = ' + Value;
end;

function TAttribute.toJava: string;
  var s, vis, v: string;
begin
  if assigned(TypeClassifier)
    then s:= TypeClassifier.ShortName + ' ' + Name
    else s:= Name;
  if IsFinal then s:= 'final ' + s;
  if Static then s:= 'static ' + s;
  vis:= VisibilityAsString(Visibility);
  if vis <> '' then
    s:= vis + ' ' + s;

  if Value <> '' then begin
    v:= Value;
    if assigned(TypeClassifier) then begin
      if TypeClassifier.ShortName = 'char' then begin
        v:= ReplaceStr(v, '''', '');
        if v = ''
          then v:= ''''''
          else v:= '''' + v[1] + ''''
      end else if TypeClassifier.ShortName = 'String' then begin
        v:= ReplaceStr(v, '"', '');
        v:= '"' + v + '"';
      end;
    end;
    s:= s + ' = ' + v;
  end;
  Result:= StringTimesN(FConfiguration.Indent1, ScopeDepth) + s + ';';
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

constructor TClassifier.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FFeatures := TObjectList.Create(True);
  GenericName:= '';
  Inner:= false;
  SourceRead:= false;
end;

destructor TClassifier.Destroy;
begin
  FreeAndNil(FFeatures);
  inherited;
end;

function TClassifier.GetFeatures: IModelIterator;
begin
  Result := TModelIterator.Create( FFeatures );
end;

function TClassifier.GetShortType: string;
begin
  try
    Result:= ShortName;
  except
  end;
end;

function TClassifier.GetAttributes: IModelIterator;
begin
  Result := TModelIterator.Create( GetFeatures , TAttribute);
end;

function TClassifier.GetAllAttributes: IModelIterator;
begin
  Result := TModelIterator.Create( GetFeatures , TAttribute, Low(TVisibility), ioNone, true);
end;

function TClassifier.GetOperations: IModelIterator;
begin
  Result := TModelIterator.Create( GetFeatures , TOperation);
end;

function TClassifier.getName: string;
begin
  Result:= Name;
  if (GenericName <> '') or (length(Name) = 1) then
    Result:= 'java.lang.Object';
end;

function TClassifier.isReference: boolean;
begin
  Result:= not IsSimpleTypeOrString(Name);
end;

function TClassifier.getAncestorName: string;
begin
  Result:= '';
end;

procedure TClassifier.setCapacity(capacity: integer);
begin
  FFeatures.Capacity:= capacity;
end;

{ TInterface }

constructor TInterface.Create(aOwner: TModelEntity);
begin
  inherited Create(aOwner);
  FExtends := TObjectList.Create(False); //Only reference
  FAncestor:= nil;
end;

destructor TInterface.Destroy;
begin
  inherited;
  FreeAndNil(FExtends);
end;

function TInterface.MakeOperation(const NewName: string; TypeClass: TClassifier): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue:= TypeClass;
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

function TInterface.AddOperationWithoutType(const NewName: string): TOperation;
begin
  Result := TOperation.Create(Self);
  Result.FName := NewName;
  Result.FReturnValue:= nil;
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

function TInterface.AddExtends(I: TInterface): TInterface;
begin
  Result := I;
  FExtends.Add(I);
end;

procedure TInterface.ViewExtends(I: TInterface);
begin
  try
    Fire(mtBeforeAddChild, I);
  except
    FExtends.Remove(I);
  end;
  Fire(mtAfterAddChild, I);
end;

function TInterface.GetExtends: IModelIterator;
begin
  Result := TModelIterator.Create( FExtends );
end;

procedure TInterface.SetAncestor(const Value: TInterface);
begin
  if Value = Self
    then FAncestor:= nil
    else FAncestor := Value;
end;

//Returns a list of classes that implements this interface.
function TInterface.GetImplementingClasses: IModelIterator;
begin
  Result := TModelIterator.Create(
    (Root as TLogicPackage).GetAllClassifiers,
    TInterfaceImplementsFilter.Create(Self) );
end;

{
  Finds an attribute with same name and type.
}
function TInterface.FindAttribute(Name, aType: string): TAttribute;
begin
  Result:= nil;
  var Mi:= getAttributes;
  while Mi.HasNext do begin
    var A:= Mi.Next as TAttribute;
    if (CompareText(Name, A.Name) = 0) and
       (CompareText(aType, A.TypeClassifier.Name) = 0) then
      exit(A);
  end;
end;

function TInterface.FindOperation(O: TOperation): TOperation;
var
  Mi,Omi1,Omi2 : IModelIterator;
  O2 : TOperation;
  label Skip;
begin
  Assert(O <> nil, ClassName + '.FindOperation invalid parameter');
  Result := nil;
  Mi := GetOperations;
  while Mi.HasNext do
  begin
    O2 := Mi.Next as TOperation;
    //Compare nr of parameters
    if O.FParameters.Count<>O2.FParameters.Count then
      Continue;
    //Compare operation name
    if CompareText(O.Name,O2.Name)<>0 then
      Continue;
    //Compare parameters
    Omi1 := O.GetParameters;
    Omi2 := O2.GetParameters;
    while Omi1.HasNext do
      if CompareText((Omi1.Next as TParameter).Name,(Omi2.Next as TParameter).Name)<>0 then
        goto Skip;
    //Ok, match
    Result := O2;
    Break;
  Skip:
  end;
end;

function TInterface.AddAttribute(const NewName: string; TypeClass: TClassifier): TAttribute;
begin
  if Assigned(TypeClass) then
    Result:= FindAttribute(NewName, TypeClass.Name)
  else
    Result:= nil;
  if Result = nil then begin
    Result := TAttribute.Create(Self);
    Result.FName := NewName;
    Result.FTypeClassifier:= TypeClass;
    FFeatures.Add(Result);
    try
      Fire(mtBeforeAddChild, Result);
    except
      FFeatures.Remove(Result);
    end;
    Fire(mtAfterAddChild, Result);
  end;
end;

function TInterface.getAncestorName: string;
begin
  if assigned(Ancestor)
    then Result:= Ancestor.Name
    else Result:= '';
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
  Result := ConfigFile;
  if (Result='') and Assigned(FOwner) then
    Result := (Owner as TAbstractPackage).GetConfigFile;
end;

procedure TAbstractPackage.SetConfigFile(const Value: string);
begin
  if Value<>'' then
    ConfigFile := ChangeFileExt(Value,ConfigFileExt);
end;


{ TClassDescendantFilter }

constructor TClassDescendantFilter.Create(aAncestor: TClass);
begin
  inherited Create;
  Self.Ancestor := aAncestor;
end;

//Returns true if M inherits from ancestor
function TClassDescendantFilter.Accept(M: TModelEntity): boolean;
begin
  Result := (M is TClass) and ((M as TClass).Ancestor = Ancestor);
end;

{ TInterfaceImplementsFilter }

constructor TInterfaceImplementsFilter.Create(I: TInterface);
begin
  inherited Create;
  Int := I;
end;

//Returns true if M implements interface Int
function TInterfaceImplementsFilter.Accept(M: TModelEntity): boolean;
begin
  Result := (M is TClass) and ((M as TClass).FImplements.IndexOf(Int)<>-1);
end;

var
  _AllClassesPackage : TAbstractPackage = nil;

//Unique Flag-instance, if Integrator.CurrentEntity=AllClassesPackage then show all classes
function AllClassesPackage : TAbstractPackage;
begin
  if _AllClassesPackage = nil then
    _AllClassesPackage := TAbstractPackage.Create(nil);
  Result := _AllClassesPackage;
end;

procedure AllClassesPackageClose;
begin
  FreeAndNil(_AllClassesPackage);
end;

end.
