Unit UJniWrapper2;
{
Copyright (c) 1998-2001 Jonathan Revusky
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. All advertising materials mentioning features or use of this software
   must display the following acknowledgement:
     This product includes software developed by Jonathan Revusky
4. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.
}

// An object-oriented wrapper around the JNI.
// The code here (by contrast with JavaRuntime.pas) should be cross-platform.

interface

uses Windows, Messages, Classes, SysUtils, Contnrs, JNI, UUtils;

type

// encapsulates a JVM instance,
// wraps around the PJavaVM handle
// and provides some static methods

  TJavaVM = class
  private
  public
    pvm: PJavaVM;
    constructor Create(p: PJavaVM);
    destructor Destroy; override;

    // convenience method to call a method's static main
    // uses delphi's native TStrings to pass the
    // array of string args
    class procedure CallMain(const aClassname: String ; strings: TStrings);
    
    // Convenience method. Calls Exit procedure
    class procedure CallExit(exitCode: Integer);
  
    // procedure to explicitly detach a local reference.
    class procedure freeRef(jobj: JObject; isGlobal: Boolean);

    // returns the current JNI environment pointer.
    class function getPEnv: PJNIEnv;
    
    // IMPORTANT: The following method must be called by native methods
    // that receive a PEnv argument if they intend to use this unit.
    class procedure setThreadPEnv(p: PJNIEnv);

    // This method sets whether you will only be using the JNIWrapper
    // methods from a single thread of execution. Basically, this
    // turns off thread-safety in order to obtain better code performance.
    // Only to be used if you really know what you're doing. Even
    // then, it's probably rarely worth it.
    class procedure setSingleThreaded(B: Boolean);
  end; // class TJavaVM

  TJavaClass = class;
  TJavaObject = class;
  TJValueArray = array of JValue;
  TJavaObjectArray = array of TJavaObject;

  TJavaValue = class
  private
    FPEnv: PJNIEnv;
    FValue: JValue;
    FKind: TNumType;
    FString: string;
    FSig: string;
    FValid: boolean;
    FError: string;
    function GetAsString: string;
    function ResolveString(s: string): string;
  public
    constructor create(aValue: JValue; Kind: TNumType; const Sig: string); overload;
    constructor create(Typ: string); overload;
    constructor create(const Value, Sig: string); overload;
    destructor Destroy; override;
    function GetSig: string;

    function GetAsBoolArray: TJBooleanArray;
    function GetAsByteArray: TJByteArray;
    function GetAsCharArray: TJCharArray;
    function GetAsShortArray: TJShortArray;
    function GetAsIntArray: TJIntArray;
    function GetAsLongArray: TJLongArray;
    function GetAsFloatArray: TJFloatArray;
    function GetAsDoubleArray: TJDoubleArray;
    function GetAsTStrings: TStrings;

    procedure SetBoolArrayFromString(const s: string);
    procedure SetByteArrayFromString(const s: string);
    procedure SetCharArrayFromString(const s: string);
    procedure SetShortArrayFromString(const s: string);
    procedure SetIntArrayFromString(const s: string);
    procedure SetLongArrayFromString(const s: string);
    procedure SetFloatArrayFromString(const s: string);
    procedure SetDoubleArrayFromString(const s: string);
    procedure SetStringArrayFromString(const s: string);
    procedure SetObjectArrayFromString(const s: string);

    function StringToJBooleanArray(s: string): TJBooleanArray;
    function StringToJByteArray(s: string): TJByteArray;
    function StringToJCharArray(s: string): TJCharArray;
    function StringToJShortArray(s: string): TJShortArray;
    function StringToJIntArray(s: string): TJIntArray;
    function StringToJLongArray(s: string): TJLongArray;
    function StringToJFloatArray(s: string): TJFloatArray;
    function StringToJDoubleArray(s: string): TJDoubleArray;
    function StringToTStrings(s: string): TStrings;

    function AsFormattedString: string;
    function SetFromString(s: string): boolean;
    property Value: JValue read FValue write FValue;
    property Kind: TNumType read FKind write FKind;
    property AsString: string read GetAsString;
    property Valid: boolean read FValid;
    property Error: string read FError write FError;
  end;

{Delphi class to encapsulate list of params to Java method.}

  TJavaParams = class
  private
    RefList: TList; //a list of references to be freed by the destructor.
    FSig: string;
    FValid: boolean;
    FArgPointer: TJValueArray;
    FError: string;
    procedure addToArgBuffer(val: JValue); //add an element to buffer.
  public
    constructor Create;
    constructor CreateMakeParams(const Signature, Params: string);
    destructor Destroy; override;
    procedure Clear;
    // The following methods add the various types to the parameter list,
    // updating the signature as well.
    procedure addBoolean(value: JValue);
    procedure addByte(value: JValue);
    procedure addChar(value: JValue);
    procedure addShort(value: JValue);
    procedure addInt(value: JValue);
    procedure addLong(value: JValue);
    procedure addFloat(value: JValue);
    procedure addDouble(value: JValue);
    procedure addString(value: JValue); overload;
    procedure addString(const s: string); overload;
    procedure addStringAsObject(const s: string);
    procedure addBooleanArray(arr: array of JBoolean);
    procedure addByteArray(arr: array of JByte);
    procedure addCharArray(arr: array of JChar);
    procedure addShortArray(arr: array of JShort);
    procedure addIntArray(arr: array of JInt);
    procedure addLongArray(arr: array of Jlong);
    procedure addFloatArray(arr: array of JFloat);
    procedure addDoubleArray(arr: array of JDouble);
    procedure addStringArray(strings: TStrings);
    procedure MakeObjectArray(strings: TStrings; aJavaClass: TJavaClass; const Typ: string);

  // In the following two methods, the second parameter
  // indicates the TJavaClass of which the object is an instance
    procedure addObjectSingle(value: TJavaObject; jcl: TJavaClass; aValue: TJavaValue);
    procedure addObject(value: TJavaObject);
    procedure addObjectArray(arr: TJavaObjectArray; jcl: TJavaClass);

  //the java signature of this parameter list.
    property Signature: String read FSig write FSig;
  // a pointer to the buffer that contains the Parameters to be passed.
    property argPointer: TJValueArray read FArgPointer;
    property IsValid: boolean read FValid;
    property Error: String read FError write FError;
  end;

{Delphi class to encapsulate a Java method; }

  TJavaMethod = class
  private
    Fclass: TJavaClass;
    FSig: string;
    FmethodType: TMethodAttribute;
    FmethodID: JMethodID;
    FRetVal: TNumType;
    FError: string;
  public
    // the constructor. The retclass is Nil unless returntype is an object.
    // raises a EJavaMethodNotFound exception if method is not found.
    constructor Create(cls: TJavaClass;
                       const name: string;
                       methodType: TMethodAttribute;
                       returnType: TNumType;
                       params: TJavaParams;
                       const retClassSig: string);
    // a minimal constructor for virtual methods that
    // takes no arguments and return nothing.
    constructor CreateVoid(cls: TJavaClass; const name: string);
    // TJavaMethod needs no destructor
    // because FMethodID needs no ressources
    function Call(params: TJavaParams ; jobj: TJavaObject): JValue;
    function isValid: boolean;
    property Error: string read FError;
  end;

  TJavaAttribute = class
  private
    FClass: TJavaClass;
    FAttributeID: JFieldID;
    FPEnv: PJNIEnv;
  public
    Static_ : boolean;
    Final_  : boolean;
    isObject: boolean;
    vis: TVisibility;
    Name: string;
    Typ: string;
    Sig: string;
    Generic: string;
    constructor Create(cls: TJavaClass);
    function  GetStaticAttributeValue(const Attributename, aSig: string): TJavaValue; overload;
    function  GetAttributeValue(jobj: TJavaObject; const Attributename, aSig: string): TJavaValue; overload;

    function  GetStaticAttributeValue: TJavaValue; overload;
    function  GetAttributeValue(jobj: TJavaObject): TJavaValue; overload;

    procedure SetStaticAttributeValue(const Attributename, aSig: string; Value: JValue);
    procedure SetAttributeValue(jobj: TJavaObject; const Attributename, aSig: string; Value: JValue);
    function ToString: string; override;
  end;

{Delphi class to encapsulate a Java object reference.}

  TJavaObject = class
  private
    FLocalHandle: jobject;
    FGlobalHandle: jobject;
    FClass: TJavaClass;
    FPEnv: PJNIEnv;
    FError: string;
    function getPEnv: PJNIEnv;
    procedure setGlobal(B: Boolean);
    function isGlobal: Boolean;
    function isValid: Boolean;
    function getHandle: jobject;
  public
// instantiates a new object of the type passed as the first param,
// using the constructor with parameters as encapsulated by the params argument.
    constructor Create(jcl: TJavaClass; params: TJavaParams);
// creates a wrapper object around the low-level JNI handle passed as an argument.
// to be used when you already have a JNI local object reference but want a delphi wrapper.
    constructor CreateWithHandle(jcl: TJavaClass; jobj: jobject);
    constructor CreateFromProperties(const Objectname: string);
    destructor Destroy; override;

    function ToString: string; override;
    function Equals(JavaObject: TJavaObject): Boolean; reintroduce;
    function isInstanceOf(JavaClass: TJavaClass): Boolean;
    function getAttributes: string;
    function getAttributeNames: string;
    procedure addToProperties(const Objectname: string);
    procedure delFromProperties(const Objectname: string);

    property Handle: jobject read GetHandle write FLocalHandle;
    property ClassRef: TJavaClass read FClass;
    property Global: Boolean read isGlobal write setGlobal;
    property Valid: Boolean read isValid;
    property Error: string read FError;
  end;

{Delphi class to encapsulate a Java class reference.}

  TJavaClass = class
  private
    FSig: string;
    FName: string;
    FPEnv: PJNIEnv;
    FLocalHandle: jobject;
    FGlobalHandle: jobject;
    FPathname: string;
    FAttributesList: TObjectList;
    FAttributesString: string;
    FAttributesNames: string;
    procedure setGlobal(B: Boolean);
    function isGlobal: Boolean;
    function isValid: Boolean;
    function getHandle: jobject;
    function getTyp: string;
    function getImportTyp: string;
  public
    Error: string;
// the constructor raises a EJavaClassNotFound exception if class is not found.
    constructor Create(const Name: string); overload;
    constructor Create(jc: jclass); overload;
// a constructor that creates a TJavaClass wrapper object when it already has
// a local object ref to the class's JNI handle.
    constructor CreateWithHandle(const Name: string; jc: jclass);
    constructor CreateLoadClass(const aClassname: string; Pathname: string);
    destructor Destroy; override;
    class function FindClass(const aClassname: string): string;
    class function IsInterface(const aClassname: string): string;
    class function getSuperClassFromName(const aClassname: string): string;
    class function cookClass(const Pathname: string): string;
    class function getPackage(const aClassname: string): string;
    procedure detecterror;
    // returns a handle to a new instance of this class.
    function Instantiate(params: TJavaParams): TJavaObject;
    function extends(JavaClass: TJavaClass): Boolean;
    function hasSuperClass: boolean;
    function getSuperClass: TJavaClass;
    function getSuperclassName: string;
    function IsAbstract: boolean;
    function getAttributes: string;
    function getAttributeNames: string;
    function getRefreshedAttributeNames: string;
    function getConstructors: string;
    function getMethods(const _Static: string): string;
    function getOuterClassSig: string;

    property Handle: jobject read GetHandle;
    property LocalHandle: jobject read FLocalHandle;
    property Signature: String read FSig write FSig;
    property Name: String read FName;
    property ImportTyp: string read getImportTyp;
    property Valid: Boolean read isValid;
    property Global: Boolean read isGlobal write setGlobal;
    property Pathname: String read FPathname write FPathname;
    property AttributesList: TObjectList read FAttributesList write FAttributesList;
  end;

  TJavaCompiler = class
    Compiler: TJavaObject;
    CompilerClass: TJavaClass;
    MethodID: JMethodID;
    Params: TJavaParams;
    Args: PJValue;
    SL: TStringList;
  public
    constructor create;
    function Compile(const Classpath, Parameter, Pathname: string): string;
  end;

{Exceptions to be raised when stuff goes wrong with the Java runtime.}

  EJvmException = class(Exception);
  EJavaClassNotFound = class(EJvmException);
  EJavaMethodNotFound = class(EJvmException);
  EJavaObjectInstantiation = class(EJvmException);
  EInvalidJNIHandle = class(EJvmException);

{ Various utility functions for creating java objects from delphi objects.}
  function createJString (const s: string ): jstring;
  function createJStringArray (myStrings: TStrings): jStringArray;
  function createJBooleanArray (arr: array of JBoolean): jBooleanArray;
  function createJByteArray (arr: array of JByte): jByteArray;
  function createJCharArray (arr: array of JChar): jCharArray;
  function createJShortArray (arr: array of JShort): jShortArray;
  function createJIntArray (arr: array of JInt): jIntArray;
  function createJLongArray (arr: array of JLong): jLongArray;
  function createJFloatArray (arr: array of JFloat): jFloatArray;
  function createJDoubleArray (arr: array of JDouble): jDoubleArray;
  function createJObjectArray(ObjectArr: TJavaObjectArray; aClass: jclass): jObjectArray;
  function getStringClass: jclass;
  function createObjectArray(strings: TStrings; aJavaClass: TJavaClass; const Typ: string;
              var Valid: boolean; var AsString: string): TJavaObjectArray;

  {various utility functions for creating Delphi objects from Java objects}
  function JToDString(js: JString): string;
  function JToTStrings(jarr: JobjectArray): TStrings;
//  function JValueToString(Value: JKindValue): string;
//  function StringToJValue(Typ, Value: string): JKindValue;
  function TypToSig(Typ: string): string;
  function sigToShortTyp(const Sig: string): string;
  function JNIPointer: PJNIEnv;


implementation

uses Dialogs, AnsiStrings, StrUtils, UComJava2, JavaRuntime;

threadvar
  PEnvThread: PJNIEnv;

var
  PEnvGlobal: PJNIenv;
  sc: jclass = Nil;
  SingleThreaded: Boolean;

  function TryStrToFloat(var s: string; var f: double): boolean;
    var p: integer;
  begin
    Result:= true;
    try
      p:= Pos('.', s);
      if (p > 0) and (FormatSettings.DecimalSeparator <> '.') then
        s[p]:= FormatSettings.DecimalSeparator;
      p:= Pos(',', s);
      if (p > 0) and (FormatSettings.DecimalSeparator <> ',') then
        s[p]:= FormatSettings.DecimalSeparator;
      f:= StrToFloat(s);
    except
      Result:= false;
    end;
  end;

  function JNIPointer: PJNIEnv;
  begin
    result:= PEnvGlobal;
    if (not SingleThreaded) or (PEnvGlobal = nil) then begin
      result:= PEnvThread;
      if SingleThreaded then PEnvGlobal:= PEnvThread;
    end;
    if result = nil then begin
      result:= PEnvThread;
      if SingleThreaded then
        PEnvGlobal:= PEnvThread;
    end;
    if result = nil then
      raise EJVMException.Create('JVM not loadad. No PEnv pointer is available');
  end;

  { --- TJavaVM ---------------------------------------------------------------}

  constructor TJavaVM.Create(p: PJavaVM);
  begin
    pvm:= p;
  end;
    
  destructor TJavaVM.Destroy;
  begin
    {if pvm <> Nil then
      CallExit(0); }
    inherited Destroy;
  end;

  class function TJavaVM.getPEnv: PJNIEnv;
  begin
    result:= JNIPointer;
  end;
    
  class procedure TJavaVM.setThreadPEnv(p: PJNIEnv);
  begin
    PEnvThread:= p;
    PEnvGlobal:= p;
  end;
        
  class procedure TJavaVM.setSingleThreaded(B: Boolean);
  begin
    if B then
      PEnvGlobal:= PEnvThread;
    SingleThreaded:= B;
  end;
    
  class procedure TJavaVM.freeRef(jobj: JObject; isGlobal: Boolean);
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    if isGlobal then
      PEnv^.DeleteGlobalRef(PEnv, jobj)
    else
      PEnv^.DeleteLocalRef(PEnv, jobj);
  end;


  class procedure TJavaVM.CallMain(const aClassname: String ; strings: TStrings);
  var
    classID: jclass;
    methodID: jmethodID;
    stringArray: jarray;
    PEnv: PJNIEnv;
    uClassname: UTF8String;

    function dotToSlash(const s : UTF8String) : UTF8String;
    var
     I: Integer;
    begin
      Result:= s;
      for I := 1 to length(Result) do
        if Result[I] = '.' then
          Result[I] := '/';
    end;

  begin
    PEnv:= JNIPointer;
    uClassname:= dotToSlash(UTF8Encode(aClassname));
    classID:= PEnv^.FindClass(PEnv, PAnsiChar(uClassname));
    if classID = nil then 
      raise EJavaClassNotFound.Create('Could not find class ' + classname);
    methodID:= PEnv^.GetStaticMethodID(PEnv, classID, 'main', '([Ljava/lang/String;)V');
    if methodID = nil then
      raise EJavaMethodNotFound.create('Could not find main method in class ' + aClassname);
    stringArray:= createJStringArray(strings);
    PEnv^.CallStaticVoidMethodV(PEnv, classID, methodID, @stringArray);
    FreeRef(stringArray, false);
  end;

  class procedure TJavaVM.CallExit(exitCode: Integer);
  var
    classID: jclass;
    methodID: jmethodID;
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    classID:= PEnv^.FindClass(PEnv, 'java/lang/System');
    methodID:= PEnv^.GetStaticMethodID(PEnv, classID, 'exit', '(I)V');
    PEnv^.CallStaticVoidMethodV(PEnv, classID, methodID, @exitCode);
  end;

  { ---------------------------------------------------------------------------}

  constructor TJavaClass.Create(const Name: string);
    var uSig: UTF8String;
  begin
    FPathname:= Name;
    FPEnv:= JNIPointer;
    FName:= Name;
    FSig := TypToSig(Name);
    uSig := UTF8Encode(FSig);
    FLocalHandle:= FPEnv^.FindClass(FPEnv, PAnsiChar(uSig));
    if FPEnv^.ExceptionCheck(FPEnv) then begin
      FPEnv^.ExceptionDescribe(FPEnv);
      Error:= myComJava2.ReadConsole;
      FLocalHandle:= nil;
    end;
    FGlobalHandle:= nil;
    FAttributesList:= nil;
  end;

  constructor TJavaClass.CreateWithHandle(const Name: string; jc: jclass);
  begin
    FPEnv:= JNIPointer;
    FName:= Name;
    FSig:= TypToSig(Name);
    FLocalHandle:= jc;
    FGlobalHandle:= nil;
    FAttributesList:= nil;
  end;

  constructor TJavaClass.Create(jc: jclass);
    var ClassH: jClass; FMethodID: jMethodID; aValue: JValue; s: string;
  begin
    FPEnv:= JNIPointer;
    FLocalHandle:= jc;
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
    FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'getName', '()Ljava/lang/String;');
    aValue.s:= FPEnv^.CallObjectMethod(FPEnv, jc, FMethodID);
    if assigned(aValue.s)
      then s:= JToDString(aValue.s)
      else s:= 'UnknownClass';
    FName:= correctGetName(s);
    FSig:= TypToSig(FName);
    FPEnv^.DeleteLocalRef(FPEnv, ClassH);
    if FPEnv^.ExceptionCheck(FPEnv) then begin
      FPEnv^.ExceptionDescribe(FPEnv);
      Error:= myComJava2.ReadConsole;
      FLocalHandle:= nil;
    end;
    FGlobalHandle:= nil;
    FAttributesList:= nil;
  end;

  constructor TJavaClass.CreateLoadClass(const aClassname: string; Pathname: string);
    var Packagename, Path: string;
        myClass: TJavaClass;
        myParams: TJavaParams;
        myMethod: TJavaMethod;
        aValue: JValue;
  begin
    FPathname:= Pathname;
    Path:= ExtractFilePath(Pathname);
    ChDir(Path);
    FPEnv:= JNIPointer;
    FGlobalHandle:= nil;
    FLocalHandle:= nil;
    Packagename:= copy(aClassname, 1, LastDelimiter('.', aClassname)-1);
    if Length(Packagename) > 0 then
      Path:= copy(Path, 1, Length(Path) - Length(Packagename) - 1);
    FName:= aClassname;
    FSig:= TypToSig(aClassname);
    Pathname:= ChangeFileExt(Pathname, '.class');
    myClass:= myComJava2.getClass('JEClassLoader');
    if assigned(myClass) then begin
      myParams:= TJavaParams.Create;
      try
        myParams.addString(Path);
        myParams.addString(aClassname);
        try
          myMethod:= TJavaMethod.Create(myClass, 'loadClass', static, ntObject, myParams, 'Ljava/lang/Class;');
          try
            if assigned(MyMethod) and (myMethod.FMethodID <> nil) then begin
              aValue:= myMethod.Call(myParams, nil);
              Error:= myMethod.FError;
              FLocalHandle:= aValue.s;
            end else
              Error:= myMethod.FError;
          finally
            FreeAndNil(myMethod);
          end;
        except on e: Exception do begin
          Error:= e.message;
          end;
        end
      finally
        myParams.Free;
      end;
    end else
      Error:= 'Missing JEClassLoader';
    FAttributesList:= nil;
  end;

  destructor TJavaClass.Destroy;
  begin
    if assigned(FPEnv) then
      if assigned(FGlobalHandle)
        then FPEnv^.DeleteGlobalRef(FPEnv, FGlobalHandle)
        else FPEnv^.DeleteLocalRef(FPEnv, FLocalHandle);
    FLocalHandle:= nil;
    FGlobalHandle:= nil;
    FreeAndNil(FAttributesList);
  end;

  class function TJavaClass.FindClass(const aClassname: string): string;
    var PEnv: PJNIEnv; P: JObject; s: UTF8String;
  begin
    PEnv:= JNIPointer;
    s:= UTF8Encode(TypToSig(aClassname));
    P:= PEnv^.FindClass(PEnv, PAnsiChar(s));
    if Assigned(P) then begin
      PEnv^.DeleteLocalRef(PEnv, P);
      Result:= '+OK ';
    end else begin
      PEnv^.ExceptionDescribe(PEnv);
      Result:= '-NO ' + myComJava2.ReadConsole;
    end;
  end;

  class function TJavaClass.IsInterface(const aClassname: string): string;
    var PEnv: PJNIEnv; P: JObject; uClassname: UTF8String;
        ClassH: jclass; FMethodID: jMethodID;
  begin
    PEnv:= JNIPointer;
    uClassname:= UTF8encode(TypToSig(aClassname));
    P:= PEnv^.FindClass(PEnv, PAnsiChar(uClassname));
    if Assigned(P)then begin
      ClassH:= PEnv^.FindClass(PEnv, 'java/lang/Class');
      FMethodID:= PEnv^.getMethodID(PEnv, ClassH, 'isInterface', '()Z');
      if PEnv^.CallBooleanMethod(PEnv, P, FMethodID)
        then Result:= '+OK'
        else Result:= '-NO';
      PEnv^.DeleteLocalRef(PEnv, ClassH);
      PEnv^.DeleteLocalRef(PEnv, P);
      end
    else begin
      PEnv^.ExceptionDescribe(PEnv);
      Result:= '-ERR ' + myComJava2.ReadConsole;
    end;
  end;

  class function TJavaClass.getSuperClassFromName(const aClassname: string): string;
    var ClassObjectH, ClassH: jclass; aValue: JValue;
        FMethodID: jMethodID; uSignature: UTF8String;
        PEnv: PJNIEnv; P: jobject;
  begin
    PEnv:= JNIPointer;
    uSignature:= UTF8Encode(TypToSig(aClassname));
    P:= PEnv^.FindClass(PEnv, PAnsiChar(uSignature));
    if assigned(P) then begin
      ClassObjectH:= PEnv^.GetSuperClass(PEnv, P); // get the Class-object
      if assigned(ClassObjectH) then begin
        ClassH:= PEnv^.FindClass(PEnv, 'java/lang/Class');
        FMethodID:= PEnv^.getMethodID(PEnv, ClassH, 'getName', '()Ljava/lang/String;');
        aValue.s:= PEnv^.CallObjectMethod(PEnv, ClassObjectH, FMethodID);
        Result:= '+OK ' + JToDString(aValue.s);
        PEnv^.DeleteLocalRef(PEnv, ClassH);
        PEnv^.DeleteLocalRef(PEnv, aValue.s);
      end else begin
        PEnv^.ExceptionDescribe(PEnv);
        Result:= '-ERR ' + myComJava2.ReadConsole;
      end
    end else begin
      PEnv^.ExceptionDescribe(PEnv);
      Result:= '-ERR ' + myComJava2.ReadConsole;
    end;
  end;

  class function TJavaClass.cookClass(const Pathname: string): string;
    var PEnv: PJNIEnv;
        aSignature: UTF8String;
        ClassH: jClass;
        MethodID: JMethodID;
        Args: PJValue;
        myParams: TJavaParams;
        myObject: jobject;
  begin
    PEnv:= JNIPointer;
    ClassH:= PEnv^.FindClass(PEnv, 'org/codehaus/janino/SimpleCompiler');
    if assigned(ClassH) then begin
      aSignature:= '(Ljava/lang/String;)V';
      MethodID:= PEnv^.GetMethodID(PEnv, ClassH, '<init>', PAnsiChar(aSignature));
      if MethodID <> nil then begin
        myParams:= TJavaParams.Create;
        myParams.addString(Pathname);
        if myParams <> nil
          then Args:= PJValue(myParams.ArgPointer)
          else Args:= nil;
        myObject:= PEnv^.NewObjectA(PEnv, ClassH, MethodID, Args);
        if myObject = nil then begin
          if PEnv^.ExceptionCheck(PEnv) then
            PEnv^.ExceptionDescribe(PEnv);
          Result:= '-ERR ' + myComJava2.ReadConsole;
        end else
          Result:= '+OK';
      end else
        Result:= '-ERR method not found';
    end else
      Result:= '-ERR org/codehaus/janino/SimpleCompiler not found.';

    //if Result then PEnv^.DeleteLocalRef(PEnv, ClassH);
  end;

  class function TJavaClass.getPackage(const aClassname: string): string;
    var PEnv: PJNIEnv;
        aSignature: UTF8String;
        ClassH: jClass;
        MethodID: JMethodID;
        Args: PJValue;
        myParams: TJavaParams;
        aValue: jValue;
  begin
    Result:= '';
    PEnv:= JNIPointer;
    ClassH:= PEnv^.FindClass(PEnv, 'java/lang/Class');
    if assigned(ClassH) then begin
      aSignature:= '(Ljava/lang/String;)Ljava/lang/String;';
      MethodID:= PEnv^.GetMethodID(PEnv, ClassH, 'forName', PAnsiChar(aSignature));
      if MethodID <> nil then begin
        myParams:= TJavaParams.Create;
        myParams.addString(aClassname);
        Args:= PJValue(myParams.ArgPointer);
        aValue.s:= PEnv^.CallObjectMethodA(PEnv, ClassH, MethodID, Args);
        if assigned(aValue.s) then begin
          Result:= '+OK ' + JToDString(aValue.s);
          PEnv^.DeleteLocalRef(PEnv, aValue.s);
        end else begin
          if PEnv^.ExceptionCheck(PEnv) then begin
            PEnv^.ExceptionDescribe(PEnv);
            Result:= '-ERR ' + myComJava2.ReadConsole;
          end;
          Result:= Result + #$0D#$0A + '-ERR Class ' + aClassname + ' not found.';
        end;
        PEnv^.DeleteLocalRef(PEnv, ClassH);
        myParams.Destroy;
      end;
    end;
  end;

  procedure TJavaClass.DetectError;
  begin
    if FPEnv^.ExceptionCheck(FPEnv) then begin
      FPEnv^.ExceptionDescribe(FPEnv);
      myComJava2.ReadConsole;
    end;
  end;

  function TJavaClass.Instantiate(params: TJavaParams): TJavaObject;
  begin
    Result:= TJavaObject.Create(self, params)
  end;

  function TJavaClass.extends(JavaClass: TJavaClass): Boolean;
  begin
    Result:= FPEnv^.isAssignableFrom(FPEnv, Handle, JavaClass.Handle);
  end;

  function TJavaClass.getHandle: jobject;
  begin
    result:= FGlobalHandle;
    if result = nil then Result:= FLocalHandle;
  end;

  function TJavaClass.getTyp: string;
    var p: integer; s: string;
  begin
    s:= FSig;
    p:= Pos('/', s);
    while p > 0 do begin
      delete(s, 1, p);
      p:= Pos('/', s);
    end;
    Result:= s;
  end;

  function TJavaClass.getImportTyp: string;
  begin
    Result:= ReplaceText(FSig, '/', '.');
  end;

  function TJavaClass.isValid: Boolean;
  begin
    if isGlobal then
      result:= true
    else
      result:= (FLocalHandle <> Nil) and (FPEnv = JNIPointer);
  end;

  procedure TJavaClass.setGlobal(B: Boolean);
  begin
    if B = Global then
      Exit;
    if B then
      FGlobalHandle:= FPEnv^.NewGlobalRef(FPEnv, FLocalHandle)
    else begin
      FPEnv:= JNIPointer;
      FLocalHandle:= FPEnv^.NewLocalRef(FPEnv, FGlobalHandle);
      FPEnv^.DeleteGlobalRef(FPEnv, FGlobalHandle);
      FGlobalHandle:= nil;
    end;
  end;

  function TJavaClass.isGlobal: Boolean;
  begin
    result:= FGlobalHandle <> nil;
  end;

  function TJavaClass.hasSuperClass: boolean;
    var h: jclass;
  begin
    h:= FPEnv^.GetSuperClass(FPEnv, Self.handle);
    Result:= assigned(h);
    FPEnv^.DeleteLocalRef(FPEnv, h);
  end;

  function TJavaClass.getSuperClass: TJavaClass;
    var aClassname: string;
  begin
    aClassname:= getSuperClassname;
    if aClassname = ''
      then Result:= nil
      else Result:= myComJava2.GetClass(aClassname);
  end;

  function TJavaClass.getSuperClassname: string;
    var ClassObjectH, ClassH: jclass; aValue: JValue;
        FMethodID: jMethodID; 
  begin
    ClassObjectH:= FPEnv^.GetSuperClass(FPEnv, Self.handle); // get the Class-object
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
    FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'getName', '()Ljava/lang/String;');
    aValue.s:= FPEnv^.CallObjectMethod(FPEnv, ClassObjectH, FMethodID);
    if assigned(aValue.s) then begin
      Result:= JToDString(aValue.s);
      FPEnv^.DeleteLocalRef(FPEnv, aValue.s);
    end else begin
      if FPEnv^.ExceptionCheck(FPEnv) then begin
        FPEnv^.ExceptionDescribe(FPEnv);
        myComJava2.ReadConsole;
      end;
      Result:= '';
    end;
    FPEnv^.DeleteLocalRef(FPEnv, ClassH);
  end;

  function TJavaClass.IsAbstract: boolean;
    var ClassH: jclass; i: integer; FMethodID: jMethodID;
  begin
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
    FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'getModifiers', '()I');
    i:= FPEnv^.CallIntMethod(FPEnv, Self.Handle, FMethodID);
    Result:= (i and 1024) = 1024;
    FPEnv^.DeleteLocalRef(FPEnv, ClassH);
  end;

  function TJavaClass.GetAttributes: string;
    var JavaLangClass, FieldClass: jclass;
        DeclaredFieldID: jMethodID;

    function toString(o: jobject): string;
      var classH: jclass; FMethodID: jMethodID; objclass: jclass;
    begin
      ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'toString', '()Ljava/lang/String;');
      objClass:= FPEnv^.CallObjectMethod(FPEnv, o, FMethodID);
      if assigned(objClass)               // TK;
        then Result:= JToDString(objClass)
        else Result:= 'UnknownClass';
    end;

    function checkException(obj: jObject): string;
    begin
      if assigned(obj) then
        Result:= '+OK'
      else
        if FPEnv^.ExceptionCheck(FPEnv) then begin
          FPEnv^.ExceptionDescribe(FPEnv);
          Result:= myComJava2.ReadConsole;
        end
        else Result:= 'error?';
    end;

    procedure ProcessAttribute(isInherited: boolean; ss, sgs: string);
      var p: integer; s1, s2, s3, sg1: string;
          vis: TVisibility;
          Attr: TJavaAttribute;

      function GetShortType(s: string): string;
        var p: integer;
      begin
        delete(s, 1, LastDelimiter('.', s));
        p:= Pos('<', s);
        if p > 1 then
          s:= copy(s, 1, p-1);
        Result:= s;
      end;


    begin
      vis:= viPackage;
      s1:= getNextPart(ss);
      sg1:= getNextPart(sgs);
      if IsVisibility(s1) then begin
        vis:= String2Visibility(s1);
        s1:= getNextPart(ss);
        sg1:= getNextPart(sgs)
      end;
      if isInherited and (vis = viPrivate) and
         (myComJava2.ShowInheritedPrivateAttributes =  '0') then exit;

      Attr:= TJavaAttribute.Create(Self);
      Attr.static_:= false;
      Attr.final_ := false;
      Attr.vis    := vis;
      while IsModifier(s1) do begin
        if s1 = 'static' then
          Attr.static_:= true;
        if s1 = 'final' then
          Attr.final_:= true;
        s1:= getNextPart(ss);
        sg1:= getNextPart(sgs);
      end;

      p:= Pos('$', s1);
      // java.util.LinkedList.java.util.LinkedList$Node<E>
      // sgs: private BinaryTree<ContentType>.BTNode<ContentType> BinaryTree.node is without $
      // ss : private BinaryTree$BTNode BinaryTree.node is with $
      if p > 0 then begin
        s2:= copy(s1, 1, p-1);
        p:= length(s2) div 2;
        s3:= System.copy(s2, 1, p);
        s2:= System.copy(s2, p + 2, 255);
        if s2 = s3 then delete(s1, 1, p+1);
      end;
      p:= Pos('.', sg1);
      while p > 0 do begin
        delete(sg1, 1, p);
        p:= Pos('.', sg1);
      end;

      p:= Pos('<', sg1);
      if p > 0 then begin
        Attr.Generic:= copy(sg1, p, length(sg1));
        sg1:= copy(s1, 1, p-1);
      end;
      if (sg1 = 'E') or (sg1 = 'K') or (sg1 = 'V')
        then Attr.Typ:= 'java.lang.Object'
        else Attr.Typ:= s1;
      Attr.Sig:= TypToSig(Attr.Typ);
      Attr.isObject:= (SigToNumType(Attr.Sig) in [ntObject, ntObjectArray]) and not Attr.Static_;
      Attr.Name:= GetShortType(ss);
      FAttributesString:= FAttributesString + #4 + Attr.toString;
      FAttributesNames:= FAttributesNames + #4 + Attr.Name;
      FAttributesList.Add(Attr);
    end;

    procedure GetInheritedAttributes(aJavaClass: TJavaClass; isInherited: boolean);
      var Count, i: integer; aJavaClass2: TJavaClass;
          Arr, aAttribut: jobject;
          aValue1, aValue2: JValue;
          FMethodID1, FMethodID2: jMethodID;
          s1, s2: string;
    begin
      if aJavaClass.hasSuperClass then begin
        aJavaClass2:= aJavaClass.getSuperClass;
        if assigned(aJavaClass2) then
          GetInheritedAttributes(aJavaClass2, true);
      end;
      // get the array of declared attributes
      Arr:= FPEnv^.CallObjectMethod(FPEnv, aJavaClass.Handle, DeclaredFieldID);
      if FPEnv^.ExceptionCheck(FPEnv) then begin
        FPEnv^.ExceptionDescribe(FPEnv);
        FAttributesNames:= 'Exception during getAttributeNames of class ' + Name + ': ' + myComJava2.ReadConsole;
        FLocalHandle:= nil;
        Count:= 0;
      end else
        Count:= FPEnv^.GetArrayLength(FPEnv, Arr);
      // get a description of every declared (public/private) attribute
      FieldClass := FPEnv^.FindClass(FPEnv, 'java/lang/reflect/Field');
      FMethodID1:= FPEnv^.GetMethodID(FPEnv, FieldClass, 'toString', '()Ljava/lang/String;');
      FMethodID2:= FPEnv^.GetMethodID(FPEnv, FieldClass, 'toGenericString', '()Ljava/lang/String;');
      for i:= 0 to Count - 1 do begin
        aAttribut:= FPEnv^.GetObjectArrayElement(FPEnv, Arr, i);
        aValue1.s:= FPEnv^.CallObjectMethod(FPEnv, aAttribut, FMethodID1);
        s1:= JToDString(aValue1.s);
        aValue2.s:= FPEnv^.CallObjectMethod(FPEnv, aAttribut, FMethodID2);
        s2:= JToDString(aValue2.s);
        ProcessAttribute(isInherited, s1, s2);
        FPEnv^.DeleteLocalRef(FPEnv, aValue1.s);
        FPEnv^.DeleteLocalRef(FPEnv, aAttribut);
      end;
      // release ressources
      FPEnv^.DeleteLocalRef(FPEnv, FieldClass);
      FPEnv^.DeleteLocalRef(FPEnv, Arr);
    end;

  begin
    if not assigned(FAttributesList) or (Pos('|', FAttributesString) = 0) then begin
      FAttributesList:= TObjectList.create;
      FAttributesString:= '';
      FAttributesNames:= '';
      JavaLangClass:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      DeclaredFieldID:= FPEnv^.GetMethodID(FPEnv, JavaLangClass, 'getDeclaredFields', '()[Ljava/lang/reflect/Field;');
      GetInheritedAttributes(Self, false);
      FPEnv^.DeleteLocalRef(FPEnv, JavaLangClass);
    end;
    Result:= FAttributesString;
  end; // TJavaClass.GetAttributes: string;

  function TJavaClass.getAttributeNames: string;
  begin
    if not assigned(FAttributesList) then
      GetAttributes;
    Result:= FAttributesNames;
  end;

  function TJavaClass.getRefreshedAttributeNames: string;
  begin
    if assigned(FAttributesList) then
      FreeAndNil(FAttributesList);
    GetAttributes;
    Result:= FAttributesNames;
  end;

  // get public constructors only
  function TJavaClass.getConstructors: string;
    var s: string; Count, i: integer;
        ClassH, ClassC: jclass; Arr, aConstructor: jobject;
        aValue: JValue; FMethodID: jMethodID;
  begin
    s:= '';
    try
      // get the constructors-array
      ClassH   := FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassH, 'getConstructors', '()[Ljava/lang/reflect/Constructor;');
      Arr      := FPEnv^.CallObjectMethod(FPEnv, Self.Handle, FMethodID);
      Count    := FPEnv^.GetArrayLength(FPEnv, Arr);

      // get a description of every constructor
      ClassC  := FPEnv^.FindClass(FPEnv, 'java/lang/reflect/Constructor');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassC, 'toGenericString', '()Ljava/lang/String;');
      for i:= 0 to Count - 1 do begin
        aConstructor:= FPEnv^.GetObjectArrayElement(FPEnv, Arr, i);
        aValue.s:= FPEnv^.CallObjectMethod(FPEnv, aConstructor, FMethodID);
        s:= s + #13#10 + JToDString(aValue.s);
        FPEnv^.DeleteLocalRef(FPEnv, aValue.s);
        FPEnv^.DeleteLocalRef(FPEnv, aConstructor);
      end;
      // release ressources
      FPEnv^.DeleteLocalRef(FPEnv, Arr);
      FPEnv^.DeleteLocalRef(FPEnv, ClassC);
      FPEnv^.DeleteLocalRef(FPEnv, ClassH);
      Result:= '+OK ' + s;
    except
      on e: Exception do
        Result:= '-ERR ' + e.Message;
    end;
  end;

  function TJavaClass.getMethods(const _Static: string): string;
    var Count, i: integer; s, s1: string;
        ClassH, ClassC: jclass; Arr, aMethod: jobject;
        aValue: JValue; FMethodID: jMethodID;
  begin
    s:= '';
    try
      // get the methods-array
      ClassH   := FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassH, 'getDeclaredMethods', '()[Ljava/lang/reflect/Method;');
      Arr      := FPEnv^.CallObjectMethod(FPEnv, Self.Handle, FMethodID);
      Count    := FPEnv^.GetArrayLength(FPEnv, Arr);

      // get a description of every method
      ClassC  := FPEnv^.FindClass(FPEnv, 'java/lang/reflect/Method');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassC, 'toGenericString', '()Ljava/lang/String;');
      for i:= 0 to Count - 1 do begin
        aMethod:= FPEnv^.GetObjectArrayElement(FPEnv, Arr, i);
        aValue.s:= FPEnv^.CallObjectMethod(FPEnv, aMethod, FMethodID);
        s1:= JToDString(aValue.s);
        if s1 <> '' then begin
          if (_Static = 'static') and (Pos('static', s1) > 0) then
            s:= s + #13#10 + s1;
          if (_Static = 'not static') and (Pos('static', s1) = 0) then
            s:= s + #13#10 + s1;
          if _Static = '' then
            s:= s + #13#10 + s1;
        end;
        FPEnv^.DeleteLocalRef(FPEnv, aValue.s);
        FPEnv^.DeleteLocalRef(FPEnv, aMethod);
      end;
      // release ressources
      FPEnv^.DeleteLocalRef(FPEnv, Arr);
      FPEnv^.DeleteLocalRef(FPEnv, ClassC);
      FPEnv^.DeleteLocalRef(FPEnv, ClassH);
      Result:= '+OK ' + s;
    except
      on e: Exception do
        Result:= '-ERR ' + e.Message;
    end;
  end;

  function TJavaClass.getOuterClassSig: string;
    var p: integer;
  begin
    Result:= '';
    p:= Length(FSig);
    while (p > 0) and (FSig[p] <> '$') do dec(p);
    if p > 0 then begin
      Result:= FSig;
      delete(Result, p, length(Result));
      Result:= Result + ';';
    end;
  end;

  {--- TJavaObject ------------------------------------------------------------}

  constructor TJavaObject.Create(jcl: TJavaClass; params: TJavaParams);
  var
    Signature, s: string; uSig: UTF8String;
    MethodID: JMethodID;
    Args: PJValue;
  begin
    s:= '';
    FError:= '';
    Signature:= '';
    Args:= nil;
    FClass:= jcl;
    FPEnv:= JNIPointer;
    if params <> nil then begin
      Signature:= Params.Signature;
      Args:= PJValue(Params.ArgPointer);
    end;
    Signature:= '(' + jcl.getOuterClassSig + Signature + ')V';
    uSig:= UTF8Encode(Signature);
    MethodID:= FPEnv^.GetMethodID(FPEnv, jcl.Handle, '<init>', PAnsiChar(uSig));
    if MethodID <> nil then begin
      try
        FLocalHandle:= FPEnv^.NewObjectA(FPEnv, jcl.Handle, MethodID, Args);
      except on e:Exception do
        FError:= e.Message;
      end;
      //FLocalHandle:= FPEnv^.NewObject(FPEnv, jcl.Handle, MethodID);
      if FLocalHandle = nil then begin
        if FPEnv^.ExceptionCheck(FPEnv) then begin
          FPEnv^.ExceptionDescribe(FPEnv);
          s:= myComJava2.ReadConsole;
        end;
        FError:= 'Could not create new instance of ' + ReplaceText(jcl.signature, '/', '.') + ' ' + s;
      end
    end else begin
      if FPEnv^.ExceptionCheck(FPEnv) then begin
        FPEnv^.ExceptionDescribe(FPEnv);
        s:= myComJava2.ReadConsole;
      end;
      FError:= 'No such constructor ' + Signature + ' - ' + s;
    end;
  end;

  constructor TJavaObject.CreateWithHandle(jcl: TJavaClass; jobj: jobject);
  begin
    FPEnv:= JNIPointer;
    FClass:= jcl;
    FLocalHandle:= jobj;
    FGlobalHandle:= nil;
  end;

  constructor TJavaObject.CreateFromProperties(const Objectname: string);
    var ClassH: jClass; MethodID: jMethodID; Properties, objClass: JObject;
        myParams: TJavaParams; myClass: TJavaClass; myMethod: TJavaMethod;
        aJValue: JValue; aJavaObject: TJavaObject;
  begin
    // Properties p = System.getProperties();
    FPEnv:= JNIPointer;
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/System');
    MethodID:= FPEnv^.GetStaticMethodID(FPEnv, ClassH, 'getProperties', '()Ljava/util/Properties;');
    Properties:= FPEnv^.CallStaticObjectMethod(FPenv, ClassH, MethodID);

    //  p.put("car1", car1);
    myParams:= TJavaParams.Create;
    try
      myParams.addString('_lvo_' + Objectname);
      myParams.Signature:= 'Ljava/lang/Object;';
      myClass:= myComJava2.getClass('java.util.Hashtable');
      aJavaObject:= TJavaObject.CreateWithHandle(myClass, Properties);

      myMethod:= TJavaMethod.Create(myClass, 'get', nonstatic, ntObject, myParams, 'Ljava/lang/Object;');
      try
        if myMethod.FMethodID <> nil then begin
          aJValue:= myMethod.Call(myParams, aJavaObject);
          FLocalHandle:= aJValue.s;
          setGlobal(true);
          if assigned(FLocalHandle) then begin
            ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Object');
            MethodID:= FPEnv^.GetMethodID(FPEnv, ClassH, 'getClass', '()Ljava/lang/Class;');
            objClass:= FPEnv^.CallObjectMethod(FPenv, FLocalHandle, MethodID);
            FClass:= TJavaClass.Create(objClass);
          end else
            if FPEnv^.ExceptionCheck(FPEnv) then begin
              FPEnv^.ExceptionDescribe(FPEnv);
              myComJava2.ReadConsole;
              FLocalHandle:= nil;
            end;
        end;
      finally
        FreeAndNil(myMethod);
      end;
    finally
      FreeAndNil(myParams);
    end;
  end;

  destructor TJavaObject.Destroy;
  begin
    if FLocalHandle <> nil then
      FPEnv^.DeleteLocalRef(FPEnv, FLocalHandle);
    if FGlobalHandle <> nil then
      FPEnv^.DeleteGlobalRef(FPEnv, FGlobalHandle);
    inherited;
  end;

  // adds a named object to the System.Properties
  procedure TJavaObject.addToProperties(const Objectname: string);
    var ClassH: jClass; MethodID: jMethodID; Properties: JObject;
        myParams: TJavaParams; myClass: TJavaClass; myMethod: TJavaMethod;
        aJavaObject: TJavaObject;
  begin
    // Properties p = System.getProperties();
    FPEnv:= JNIPointer;
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/System');
    MethodID:= FPEnv^.GetStaticMethodID(FPEnv, ClassH, 'getProperties', '()Ljava/util/Properties;');
    Properties:= FPEnv^.CallStaticObjectMethod(FPenv, ClassH, MethodID);

    //  p.put("car1", car1);
    myParams:= TJavaParams.Create;
    try
      myParams.addString('_lvo_' + Objectname);
      myParams.addObject(Self);
      myParams.Signature:= 'Ljava/lang/Object;Ljava/lang/Object;';
      myClass:= myComJava2.getClass('java.util.Hashtable');
      aJavaObject:= TJavaObject.CreateWithHandle(myClass, Properties);
      myMethod:= TJavaMethod.Create(myClass, 'put', nonstatic, ntObject, myParams, 'Ljava/lang/Object;');
      try
        if myMethod.FMethodID <> nil then
           myMethod.Call(myParams, aJavaObject);
      finally
        FreeAndNil(myMethod);
        FreeAndNil(aJavaObject);
      end;
    finally
      FreeAndNil(myParams);
    end;
  end;

  // deletes a named object from the System.Properties
  procedure TJavaObject.delFromProperties(const Objectname: string);
    var ClassH: jClass; MethodID: jMethodID; Properties: JObject;
        myParams: TJavaParams; myClass: TJavaClass; myMethod: TJavaMethod;
        aJValue: JValue; aJavaObject: TJavaObject;
  begin
    // Properties p = System.getProperties();
    FPEnv:= JNIPointer;
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/System');
    MethodID:= FPEnv^.GetStaticMethodID(FPEnv, ClassH, 'getProperties', '()Ljava/util/Properties;');
    Properties:= FPEnv^.CallStaticObjectMethod(FPenv, ClassH, MethodID);

    //  p.remove("car1");
    myParams:= TJavaParams.Create;
    try
      myParams.addString('_lvo_' + Objectname);
      myParams.Signature:= 'Ljava/lang/Object;';
      myClass:= myComJava2.getClass('java.util.Hashtable');
      aJavaObject:= TJavaObject.CreateWithHandle(myClass, Properties);
      myMethod:= TJavaMethod.Create(myClass, 'remove', nonstatic, ntObject, myParams, 'Ljava/lang/Object;');
      try
        if myMethod.FMethodID <> nil then begin
          aJValue:= myMethod.Call(myParams, aJavaObject);
          if assigned(aJValue.s) then
            FPEnv^.DeleteGlobalRef(FPEnv, aJValue.s);
        end;
      finally
        FreeAndNil(myMethod);
      end;
    finally
      FreeAndNil(myParams);
    end;
  end;

  function TJavaObject.getPEnv: PJNIEnv;
  begin
    if isGlobal or (FPEnv = Nil) then 
      result:= JNIPointer
    else
      result:= FPEnv;
  end;

  function TJavaObject.Equals(JavaObject: TJavaObject): Boolean;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= getPEnv;
    if (not self.Valid) or (not JavaObject.Valid) then
      raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
    Result:= PEnv^.IsSameObject(PEnv, Handle, JavaObject.Handle);
  end;

  function TJavaObject.isInstanceOf(JavaClass: TJavaClass): Boolean;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= GetPEnv;
    if (not self.Valid) or (not JavaClass.Valid) then 
        raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
    result:= PEnv^.IsInstanceOf(PEnv, Handle, JavaClass.Handle);
  end;

  procedure TJavaObject.setGlobal(B: Boolean);
  begin
    if B = Global then
      Exit;
    if B then
      FGlobalHandle:= FPEnv^.NewGlobalRef(FPEnv, FLocalhandle)
    else begin
      FPEnv:= JNIPointer;
      FLocalHandle:= FPEnv^.NewLocalRef(FPEnv, FGlobalHandle);
      FPEnv^.DeleteGlobalRef(FPEnv, FGlobalHandle);
      FGlobalHandle:= Nil;
    end;
  end;

  function TJavaObject.isGlobal: Boolean;
  begin
    result:= FGlobalHandle <> Nil;
  end;
    
  function TJavaObject.isValid: Boolean;
  begin
    if isGlobal
      then result:= true
      else result:= (FLocalHandle <> Nil) and (FPEnv = JNIPointer);
  end;

  function TJavaObject.getAttributes: string;
  begin
    Result:= FClass.getAttributes;
  end;

  function TJavaObject.getAttributeNames: string;
  begin
    Result:= FClass.getAttributeNames;
  end;

  function TJavaObject.getHandle: jobject;
  begin
    Result:= FGlobalHandle;
    if Result = Nil then
      Result:= FLocalHandle;
  end;

  function TJavaObject.toString: string;
    var
      toStringMethod: jmethodID;
      js: jstring; s: string;
      PEnv: PJNIEnv;
  begin
    PEnv:= getPEnv;
    toStringMethod:= PEnv^.getMethodID(PEnv, classRef.Handle, 'toString', '()Ljava/lang/String;');
    js:= PEnv^.callObjectMethod(PEnv, Handle, toStringMethod);
    if js <> nil
      then s:= JToDString(js)
    else begin
      if PEnv^.ExceptionCheck(PEnv) then
        PEnv^.ExceptionDescribe(PEnv);
      s:= myComJava2.ReadConsole;
    end;
    Result:= s;
  end;

{ --- TJavaParams ------------------------------------------------------------ }

  constructor TJavaParams.Create;
  begin
    RefList:= TList.Create;
  end;
    
  destructor TJavaParams.Destroy;
  var
    I: Integer;
  begin
    for I:= 0 to RefList.Count - 1 do
      TJavaVM.FreeRef(Reflist.Items[i], false);
    RefList.Free;
    if Length(FArgPointer) >0
      then setLength(FArgPointer, 0);
    inherited Destroy;
  end;

  procedure TJavaParams.Clear;
    var I: Integer;
  begin
    for I:= 0 to RefList.Count - 1 do
      TJavaVM.FreeRef(Reflist.Items[i], false);
    RefList.Clear;
    setLength(FArgPointer, 0);
    FArgPointer:= nil;
    FSig:= '';
  end;
  
  procedure TJavaParams.addBoolean(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'Z';
  end;
  
  procedure TJavaParams.addByte(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'B';
  end;

  procedure TJavaParams.addChar(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'C';
  end;

  procedure TJavaParams.addShort(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'S';
  end;

  procedure TJavaParams.addInt(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'I';
  end;

  procedure TJavaParams.addLong(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'J';
  end;

  procedure TJavaParams.addFloat(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'F';
  end;

  procedure TJavaParams.addDouble(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'D';
  end;

  procedure TJavaParams.addString(value: JValue);
  begin
    addToArgBuffer(value);
    FSig:= FSig + 'Ljava/lang/String;';
    RefList.add(value.s);
  end;

  procedure TJavaParams.addString(const s: string);
    var js: JString; val: JValue;
  begin
    js:= createJString(s);
    val.s:= js;
    addToArgBuffer(val);
    FSig:= FSig + 'Ljava/lang/String;';
    RefList.add(js);
  end;

  procedure TJavaParams.addStringAsObject(const s: string);
    var js: JString; val: JValue;
  begin
    js:= createJString(s);
    val.s:= js;
    addToArgBuffer(val);
    FSig:= FSig + 'Ljava/lang/Object;';
    RefList.add(js);
  end;

  procedure TJavaParams.addObjectSingle(value: TJavaObject; jcl: TJavaClass; aValue: TJavaValue);
    var jo: JObject; val: JValue;
  begin
    if Assigned(value)
      then jo:= value.Handle
      else jo:= nil;
    val.s:= jo;
    addToArgBuffer(val);
    if Assigned(jcl)
      then FSig:= FSig + jcl.Signature
      else FSig:= aValue.FSig;
  end;

  procedure TJavaParams.addObject(value: TJavaObject);
    var jo: JObject; val: JValue;
  begin
    if Assigned(value)
      then jo:= value.Handle
      else jo:= nil;
    val.s:= jo;
    addToArgBuffer(val);
    if Assigned(value.ClassRef)
      then FSig:= FSig + value.ClassRef.Signature;
  end;

  procedure TJavaParams.addObjectArray(arr: TJavaObjectArray; jcl: TJavaClass);
    var
      PEnv: PJNIEnv;
      jarr: jobjectarray;
      i: Integer; val: JValue;
  begin
    PEnv:= JNIPointer;
    jarr:= PEnv^.NewObjectArray(PEnv, High(Arr)+1, jcl.Handle, arr[0].Handle);
    for i:= 1 + Low(arr) to High(arr) do
      PEnv^.setObjectArrayElement(PEnv, jarr, I, arr[I].Handle);
    val.s:= jarr;
    addToArgBuffer(val);
    FSig:= FSig + '[' + jcl.Signature;
    RefList.add(jarr)
  end;
    
  procedure TJavaParams.addBooleanArray(arr: array of JBoolean);
    var jbarray: JBooleanArray; val: JValue;
  begin
    jbarray:= createJBooleanArray(arr);
    val.s:= jbarray;
    addToArgBuffer(val);
    FSig:= FSig + '[Z';
    RefList.add(jbarray)
  end;

  procedure TJavaParams.addByteArray(arr: array of JByte);
    var jbarray: JByteArray; val: JValue;
  begin
    jbarray:= createJByteArray(arr);
    val.s:= jbarray;
    addToArgBuffer(val);
    FSig:= FSig + '[B';
    RefList.add(jbarray)
  end;

  procedure TJavaParams.addCharArray(arr: array of JChar);
    var jcarray: JCharArray; val: JValue;
  begin
    jcarray:= createJCharArray(arr);
    val.s:= jcarray;
    addToArgBuffer(val);
    FSig:= FSig + '[C';
    RefList.add(jcarray)
  end;

  procedure TJavaParams.addShortArray(arr: array of JShort);
    var jsarray: JShortArray; val: JValue;
  begin
    jsarray:= createJShortArray(arr);
    val.s:= jsarray;
    addToArgBuffer(val);
    FSig:= FSig + '[S';
    RefList.add(jsarray)
  end;

  procedure TJavaParams.addIntArray(arr: array of JInt);
    var jiarray: JIntArray; val: JValue;
  begin
    jiarray:= createJIntArray(arr);
    val.s:= jiarray;
    addToArgBuffer(val);
    FSig:= FSig + '[I';
    RefList.add(jiarray)
  end;
    
  procedure TJavaParams.addLongArray(arr: array of Jlong);
    var jlarray: JLongArray; val: JValue;
  begin
    jlarray:= createJLongArray(arr);
    val.s:= jlarray;
    addToArgBuffer(val);
    FSig:= FSig + '[J';
    RefList.add(jlarray)
  end;

  procedure TJavaParams.addFloatArray(arr: array of JFloat);
    var  jfarray: JFloatArray; val: JValue;
  begin
    jfarray:= createJFloatArray(arr);
    val.s:= jfarray;
    addToArgBuffer(val);
    FSig:= FSig + '[F';
    RefList.add(jfarray)
  end;
        
  procedure TJavaParams.addDoubleArray(arr: array of JDouble);
    var jdarray: JDoubleArray; val: JValue;
  begin
    jdarray:= createJDoubleArray(arr);
    val.s:= jdarray;
    addToArgBuffer(val);
    FSig:= FSig + '[D';
    RefList.add(jdarray)
  end;
    
  procedure TJavaParams.addStringArray(strings: TStrings);
    var jsarray: JArray; val: JValue;
  begin
    jsarray:= createJStringArray(strings);
    val.s:= jsarray;
    addToArgBuffer(val);
    FSig:= FSig + '[Ljava/lang/String;';
    RefList.add(jsarray)
  end;

  procedure TJavaParams.MakeObjectArray(Strings: TStrings; aJavaClass: TJavaClass; const Typ: string);
    var ObjectArr: TJavaObjectArray; var s: string;
  begin
     ObjectArr:= CreateObjectArray(Strings, aJavaClass, Typ, FValid, s);
     addObjectArray(ObjectArr, aJavaClass);
  end;

  procedure TJavaParams.addToArgBuffer(val: JValue);
    var i: integer;
  begin
    i:= Length(FArgPointer);
    SetLength(FArgPointer, i + 1);
    FArgPointer[i]:= val;
  end;

  constructor TJavaParams.CreateMakeParams(const Signature, Params: string);
    var i: integer;
        aJavaValue: TJavaValue;
        aJavaObject: TJavaObject;
        aJavaClass:  TJavaClass;
        SLT, SLSig, SLPar: TStrings;
        Typ: string;
  begin
    RefList:= TList.Create;
    FValid:= true;
    SLSig:= Split('|', Signature);
    SLPar:= Split('|', Params);

    for i:= 0 to SLSig.Count - 1 do begin
      if SLSig[i] = '' then continue;
      aJavaValue:= TJavaValue.create(SLPar[i], SLSig[i]);
      FValid:= aJavaValue.FValid;
      if not FValid then begin
        Error:= aJavaValue.Error;
        FreeAndNil(aJavaValue);
        break;
      end;

      case aJavaValue.Kind of
        ntBool: addBoolean(aJavaValue.Value);
        ntByte: addByte(aJavaValue.Value);
        ntChar: addChar(aJavaValue.Value);
        ntShort: addShort(aJavaValue.Value);
        ntInt: addInt(aJavaValue.Value);
        ntLong: addLong(aJavaValue.Value);
        ntFloat: addFloat(aJavaValue.Value);
        ntDouble: addDouble(aJavaValue.Value);
        ntString: addString(aJavaValue.Value);
        ntBoolArray: addBooleanArray(aJavaValue.GetAsBoolArray);
        ntByteArray: addByteArray(aJavaValue.GetAsByteArray);
        ntCharArray: addCharArray(aJavaValue.GetAsCharArray);
        ntShortArray: addShortArray(aJavaValue.GetAsShortArray);
        ntIntArray:  addIntArray(aJavaValue.GetAsIntArray);
        ntLongArray: addLongArray(aJavaValue.GetAsLongArray);
        ntFloatArray: addFloatArray(aJavaValue.GetAsFloatArray);
        ntDoubleArray: addDoubleArray(aJavaValue.GetAsDoubleArray);
        ntStringArray: begin
            SLT:= aJavaValue.GetAsTStrings;
            addStringArray(SLT);
            SLT.Free;
          end;
        ntObject : begin
            aJavaObject:= myComJava2.GetObject(aJavaValue.AsString);
            if (aJavaObject = nil) and (aJavaValue.AsString <> 'null') then begin
              FError:= myComJava2.LNGUnknownObject + ' "' + aJavaValue.AsString + '"';
              FValid:= false; break;
             end
            else begin
              Typ:= SigToTyp(aJavaValue.GetSig);
              aJavaClass:= myComJava2.GetClass(Typ);
              if (aJavaClass = nil) and (aJavaObject <> nil) then begin
                FError:= myComJava2.LNGUnknownClass + ' "' + Typ + '"';
                FValid:= false; break;
              end;
              if Assigned(aJavaObject) and not aJavaObject.ClassRef.extends(aJavaClass) then begin
                FError:= format(myComJava2.LNGIncompatibelTypes, [aJavaObject.ClassRef.Name, Typ]);
                FValid:= false;
                break;
              end else
                addObjectSingle(aJavaObject, aJavaClass, aJavaValue);
            end;
          end;
        ntObjectArray: begin
            Typ:= SigToTyp(aJavaValue.GetSig);
            aJavaClass:= myComJava2.GetClass(Typ);
            if aJavaClass = nil then begin
              FError:= myComJava2.LNGUnknownClass + ' "' + Typ + '"';
              FValid:= false;
              end
            else begin
              SLT:= aJavaValue.GetAsTStrings;
              if assigned(SLT) then begin
                MakeObjectArray(SLT, aJavaClass, Typ);
                SLT.Free;
              end else begin
                FError:= 'Invalid object array';
                FValid:= false;
              end;
            end;
          end;
      end;
      FreeAndNil(aJavaValue);
    end;
    FreeAndNil(SLSig);
    FreeAndNil(SLPar);
  end;

{ --- TJavaMethod ------------------------------------------------------------ }

  constructor TJavaMethod.Create(cls: TJavaClass;
                                 const Name: string;
                                 methodType: TMethodAttribute;
                                 returnType: TNumType;
                                 params: TJavaParams;
                                 const retClassSig: string);
  var
    PEnv: PJNIEnv;
    uName, uSig: UTF8String;
  begin
    inherited create;
    FClass:= cls;
    if params = nil
      then FSig:= '()'
      else FSig:= '(' + params.signature + ')';
    FMethodType:= MethodType;
    FRetval:= ReturnType;
    //retClassSig:= ReplaceText(retClassSig, '.', '/');
    FSig:= FSig + NumTypeToSig(FRetVal, retClassSig);
    PEnv:= JNIPointer;
    uName:= UTF8Encode(Name);
    uSig:= UTF8encode(FSig);

    if FMethodType = static
      then FMethodID:= PEnv^.getStaticMethodID(PEnv, Fclass.Handle, PAnsiChar(uName), PAnsiChar(uSig))
      else FMethodID:= PEnv^.getMethodID(PEnv, Fclass.Handle, PAnsiChar(uName), PAnsiChar(uSig));
    if FMethodID = Nil then
      FError:= 'Method ' + String(uName) + ' with signature ' + String(uSig) + ' not found.';
  end;

  constructor TJavaMethod.CreateVoid(cls: TJavaClass; const name: string);
  begin
    Create(cls, name, nonstatic, ntVoid, Nil, '');
  end;

  function TJavaMethod.Call(Params: TJavaParams; jobj: TJavaObject): JValue;
    var
      PEnv: PJNIEnv;
      obj: jobject;
      args: PJValue;
  begin
    PEnv:= JNIPointer;
    args:= nil;
    if params <> nil then args:= PJValue(Params.argpointer);
    if jobj <> nil
      then obj:= jobj.Handle
      else obj:= nil;
    if FMethodType = static then
      case FRetVal of
         ntVoid:   PEnv^.CallStaticVoidMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntBool:   result.z:= PEnv^.CallStaticBooleanMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntByte:   result.b:= PEnv^.CallStaticByteMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntChar:   result.c:= PEnv^.CallStaticCharMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntShort:  result.h:= PEnv^.CallStaticShortMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntInt:    result.i:= PEnv^.CallStaticIntMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntLong:   result.l:= PEnv^.CallStaticLongMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntFloat:  result.f:= PEnv^.CallStaticFloatMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntDouble: result.d:= PEnv^.CallStaticDoubleMethodA(PEnv, FClass.Handle, FmethodID, args);
         else      result.s:= PEnv^.CallStaticObjectMethodA(PEnv, FClass.Handle, FmethodID, args);
      end;

   if FMethodType = nonvirtual then
      case FRetVal of
         ntVoid:   PEnv^.CallNonvirtualVoidMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntBool:   result.z:= PEnv^.CallNonVirtualBooleanMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntByte:   result.b:= PEnv^.CallNonVirtualByteMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntChar:   result.c:= PEnv^.CallNonVirtualCharMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntShort:  result.h:= PEnv^.CallNonVirtualShortMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntInt:    result.i:= PEnv^.CallNonVirtualIntMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntLong:   result.l:= PEnv^.CallNonVirtualLongMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntFloat:  result.f:= PEnv^.CallNonVirtualFloatMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntDouble: result.d:= PEnv^.CallNonVirtualDoubleMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         else      result.s:= PEnv^.CallNonVirtualObjectMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
      end;
   
   if FMethodType = nonstatic then
      case FRetVal of
         ntVoid:   PEnv^.CallVoidMethodA(PEnv, obj, FmethodID, args);
         ntBool:   result.z:= PEnv^.CallBooleanMethodA(PEnv, obj, FmethodID, args);
         ntByte:   result.b:= PEnv^.CallByteMethodA(PEnv, obj, FmethodID, args);
         ntChar:   result.c:= PEnv^.CallCharMethodA(PEnv, obj, FmethodID, args);
         ntShort:  result.h:= PEnv^.CallShortMethodA(PEnv, obj, FmethodID, args);
         ntInt:    result.i:= PEnv^.CallIntMethodA(PEnv, obj, FmethodID, args);
         ntLong:   result.l:= PEnv^.CallLongMethodA(PEnv, obj, FmethodID, args);
         ntFloat:  result.f:= PEnv^.CallFloatMethodA(PEnv, obj, FmethodID, args);
         ntDouble: result.d:= PEnv^.CallDoubleMethodA(PEnv, obj, FmethodID, args);
         else      result.s:= PEnv^.CallObjectMethodA(PEnv, obj, FmethodID, args);
      end;
    if PEnv^.ExceptionCheck(PEnv) then begin
      PEnv^.ExceptionDescribe(PEnv);
      FError:= myComJava2.ReadConsole;
    end;
  end;

  function TJavaMethod.IsValid: boolean;
  begin
    Result:= Assigned(FMethodID);
  end;

{ --- TJavaAttribute ------------------------------------------------------------- }

  constructor TJavaAttribute.Create(cls: TJavaClass);
  begin
    vis     := viPackage;
    Static_ := false;
    Final_  := false;
    isObject:= false;
    Generic := '';
    Sig     := '';
    Name    := '';
    FClass  := cls;
    if assigned(cls)
      then FPEnv:= cls.FPEnv
      else FPEnv:= nil;
  end;

  function TJavaAttribute.toString: string;
  begin
    Result:= VisibilityAsString(vis) + '|' + BoolToStr(Static_) + '|' + BoolToStr(Final_) + '|' + Generic + '|' + Typ + '|' + Name;
  end;

  function TJavaAttribute.GetStaticAttributeValue(const Attributename, aSig: string): TJavaValue;
    var value: JValue; uName, uSig: UTF8String;
  begin
    Sig:= aSig;
    uSig:= UTF8Encode(aSig);
    uName:= UTF8Encode(Attributename);
    FAttributeID:= FPEnv^.GetStaticFieldID(FPEnv, Fclass.Handle, PAnsiChar(uName), PAnsiChar(uSig));
    if FAttributeID <> nil then begin
      FillChar(value, SizeOf(value), #0);
      case Sig[1] of
        'Z': value.Z:= FPEnv^.GetStaticBooleanField(FPEnv, FClass.Handle, FAttributeID);
        'B': value.B:= FPEnv^.GetStaticByteField(FPEnv, FClass.Handle, FAttributeID);
        'C': value.C:= FPEnv^.GetStaticCharField(FPEnv, FClass.Handle, FAttributeID);
        'S': value.H:= FPEnv^.GetStaticShortField(FPEnv, FClass.Handle, FAttributeID);
        'I': value.I:= FPEnv^.GetStaticIntField(FPEnv, FClass.Handle, FAttributeID);
        'J': value.L:= FPEnv^.GetStaticLongField(FPEnv, FClass.Handle, FAttributeID);
        'F': value.F:= FPEnv^.GetStaticFloatField(FPEnv, FClass.Handle, FAttributeID);
        'D': value.D:= FPEnv^.GetStaticDoubleField(FPEnv, FClass.Handle, FAttributeID);
        'L',
        '[': value.S:= FPEnv^.GetStaticObjectField(FPEnv, FClass.Handle, FAttributeID);
      end;
      Result:= TJavaValue.create(value, SigToNumType(Sig), Sig);
    end
    else Result:= nil;
  end;

  function TJavaAttribute.GetAttributeValue(JObj: TJavaObject; const Attributename, aSig: string): TJavaValue;
    var value: JValue; uName, uSig: UTF8String;
  begin
    Sig:= aSig;
    uSig:= UTF8Encode(aSig);
    uName:= UTF8Encode(Attributename);
    FAttributeID:= FPEnv^.GetFieldId(FPEnv, Fclass.Handle, PAnsiChar(uName), PAnsiChar(uSig));
    if (JObj <> nil) and (FAttributeID <> nil) then begin
      FillChar(value, SizeOf(value), #0);
      case Sig[1] of
        'Z': value.Z:= FPEnv^.GetBooleanField(FPEnv, JObj.Handle, FAttributeID);
        'B': value.B:= FPEnv^.GetByteField(FPEnv, JObj.Handle, FAttributeID);
        'C': value.C:= FPEnv^.GetCharField(FPEnv, JObj.Handle, FAttributeID);
        'S': value.H:= FPEnv^.GetShortField(FPEnv, JObj.Handle, FAttributeID);
        'I': value.I:= FPEnv^.GetIntField(FPEnv, JObj.Handle, FAttributeID);
        'J': value.L:= FPEnv^.GetLongField(FPEnv, JObj.Handle, FAttributeID);
        'F': value.F:= FPEnv^.GetFloatField(FPEnv, JObj.Handle, FAttributeID);
        'D': value.D:= FPEnv^.GetDoubleField(FPEnv, JObj.Handle, FAttributeID);
        'L',
        '[': value.S:= FPEnv^.GetObjectField(FPEnv, JObj.Handle, FAttributeID);
      end;
      Result:= TJavaValue.create(value, SigToNumType(Sig), Sig);
    end
    else
      Result:= nil;
  end;

  function TJavaAttribute.GetStaticAttributeValue: TJavaValue;
  begin
    Result:= GetStaticAttributeValue(Name, Sig);
    if Result = nil then
      Result:= GetStaticAttributeValue(Name, 'Ljava/lang/Object;');
  end;

  function TJavaAttribute.GetAttributeValue(jobj: TJavaObject): TJavaValue;
  begin
    Result:= GetAttributeValue(jobj, Name, Sig);   // unsinnig Name/Sig
    // due to
    // public class Node<ContentType> {
    //    private ContentType content = null;
    //    private Node<ContentType> nextNode = null;
    //  Sig LContentType; doesn't exist    
    if Result = nil then
      Result:= GetAttributeValue(jobj, Name, 'Ljava/lang/Object;');
  end;

  procedure TJavaAttribute.SetStaticAttributeValue(const Attributename, aSig: string; Value: JValue);
    var uName, uSig: UTF8String;
  begin
    uSig:= UTF8Encode(aSig);
    uName:= UTF8encode(Attributename);
    FAttributeID:= FPEnv^.GetStaticFieldID(FPEnv, Fclass.Handle, PAnsiChar(uName), PAnsiChar(uSig));
    case uSig[1] of
      'Z': FPEnv^.SetStaticBooleanField(FPEnv, FClass.Handle, FAttributeID, Value.z);
      'B': FPEnv^.SetStaticByteField(FPEnv, FClass.Handle, FAttributeID, Value.b);
      'C': FPEnv^.SetStaticCharField(FPEnv, FClass.Handle, FAttributeID, Value.c);
      'S': FPEnv^.SetStaticShortField(FPEnv, FClass.Handle, FAttributeID, Value.h);
      'I': FPEnv^.SetStaticIntField(FPEnv, FClass.Handle, FAttributeID, Value.i);
      'J': FPEnv^.SetStaticLongField(FPEnv, FClass.Handle, FAttributeID, Value.l);
      'F': FPEnv^.SetStaticFloatField(FPEnv, FClass.Handle, FAttributeID, Value.f);
      'D': FPEnv^.SetStaticDoubleField(FPEnv, FClass.Handle, FAttributeID, Value.d);
      else FPEnv^.SetStaticObjectField(FPEnv, FClass.Handle, FAttributeID, Value.s);
    end;
  end;

  procedure TJavaAttribute.SetAttributeValue(JObj: TJavaObject; const Attributename, aSig: string; Value: JValue);
    var uName, uSig: UTF8String;
  begin
    uSig:= UTF8Encode(aSig);
    uName:= UTF8encode(Attributename);
    FAttributeID:= FPEnv^.GetFieldID(FPEnv, Fclass.Handle, PAnsiChar(uName), PAnsiChar(uSig));
    case uSig[1] of
      'Z': FPEnv^.SetBooleanField(FPEnv, JObj.Handle, FAttributeID, Value.z);
      'B': FPEnv^.SetByteField(FPEnv, JObj.Handle, FAttributeID, Value.b);
      'C': FPEnv^.SetCharField(FPEnv, JObj.Handle, FAttributeID, Value.c);
      'S': FPEnv^.SetShortField(FPEnv, JObj.Handle, FAttributeID, Value.h);
      'I': FPEnv^.SetIntField(FPEnv, JObj.Handle, FAttributeID, Value.i);
      'J': FPEnv^.SetLongField(FPEnv, JObj.Handle, FAttributeID, Value.l);
      'F': FPEnv^.SetFloatField(FPEnv, JObj.Handle, FAttributeID, Value.f);
      'D': FPEnv^.SetDoubleField(FPEnv, JObj.Handle, FAttributeID, Value.d);
      else FPEnv^.SetObjectField(FPEnv, JObj.Handle, FAttributeID, Value.s);
    end;
  end;

{--- TJavaValue ------------------------------------------}

constructor TJavaValue.create(aValue: JValue; Kind: TNumType; const Sig: string);
begin
  FPEnv:= JNIPointer;
  FValue:= aValue;
  FKind:= Kind;
  FString:= '_x_';
  FSig:= Sig;
  FValid:= true;
end;

constructor TJavaValue.create(const Value, Sig: string);
begin
  FPEnv:= JNIPointer;
  FKind:= SigToNumType(Sig);
  FString:= '_x_';
  FSig:= Sig;
  FValid:= SetFromString(Value);
end;

constructor TJavaValue.create(Typ: string);
begin
  // ntBoolArray ntIntArray
  FPEnv:= JNIPointer;
  FString:= '_x_';
  FSig:= TypToSig(Typ);
  delete(Typ, 1, LastDelimiter('.', Typ));
  FillChar(FValue, SizeOf(FValue), #0);
  FKind:= TypToNumType(Typ);
  FValid:= true;
end;

destructor TJavaValue.Destroy;
begin
  {if (Kind in [ntString, ntStringArray, ntObject, ntObjectArray]) and (FValue.s <> nil) then
    FPEnv^.DeleteLocalRef(FPEnv, FValue.s)}
end;

function TJavaValue.GetSig: string;
begin
  Result:= FSig;
end;

function TJavaValue.GetAsBoolArray: TJBooleanArray;
begin
  result:= StringToJBooleanArray(FString);
end;

function TJavaValue.GetAsByteArray: TJByteArray;
begin
  result:= StringToJByteArray(FString);
end;

function TJavaValue.GetAsCharArray: TJCharArray;
begin
  result:= StringToJCharArray(FString);
end;

function TJavaValue.GetAsShortArray: TJShortArray;
begin
  result:= StringToJShortArray(FString);
end;

function TJavaValue.GetAsIntArray: TJIntArray;
begin
  result:= StringToJIntArray(FString);
end;

function TJavaValue.GetAsLongArray: TJLongArray;
begin
  result:= StringToJLongArray(FString);
end;

function TJavaValue.GetAsFloatArray: TJFloatArray;
begin
  result:= StringToJFloatArray(FString);
end;

function TJavaValue.GetAsDoubleArray: TJDoubleArray;
begin
  result:= StringToJDoubleArray(FString);
end;

function TJavaValue.GetAsTStrings: TStrings;
begin
  result:= StringToTStrings(FString);
end;

procedure TJavaValue.SetBoolArrayFromString(const s: string);
  var arr: TJBooleanArray;
begin
  FString:= s;
  arr:= GetAsBoolArray;
  if Assigned(arr)
    then FValue.s:= createJBooleanArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetByteArrayFromString(const s: string);
  var arr: TJByteArray;
begin
  FString:= s;
  arr:= GetAsByteArray;
  if Assigned(arr)
    then FValue.s:= createJByteArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetCharArrayFromString(const s: string);
  var arr: TJCharArray;
begin
  FString:= s;
  arr:= GetAsCharArray;
  if Assigned(arr)
    then FValue.s:= createJCharArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetShortArrayFromString(const s: string);
  var arr: TJShortArray;
begin
  FString:= s;
  arr:= GetAsShortArray;
  if Assigned(arr)
    then FValue.s:= createJShortArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetIntArrayFromString(const s: string);
  var arr: TJIntArray;
begin
  FString:= s;
  arr:= GetAsIntArray;
  if Assigned(arr)
    then FValue.s:= createJIntArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetLongArrayFromString(const s: string);
  var arr: TJLongArray;
begin
  FString:= s;
  arr:= GetAsLongArray;
  if Assigned(arr)
    then FValue.s:= createJLongArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetFloatArrayFromString(const s: string);
  var arr: TJFloatArray;
begin
  FString:= s;
  arr:= GetAsFloatArray;
  if Assigned(arr)
    then FValue.s:= createJFloatArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetDoubleArrayFromString(const s: string);
  var arr: TJDoubleArray;
begin
  FString:= s;
  arr:= GetAsDoubleArray;
  if Assigned(arr)
    then FValue.s:= createJDoubleArray(arr)
    else FValue.s:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetStringArrayFromString(const s: string);
  var arr: TStrings;
begin
  FString:= s;
  arr:= GetAsTStrings;
  if Assigned(arr) then begin
    FValue.s:= createJStringArray(arr);
    arr.Free;
    end
  else FValue.s:= nil;
end;

procedure TJavaValue.SetObjectArrayFromString(const s: string);
  var arr: TStrings; Typ: string; aJavaClass: TJavaClass;
      ObjectArr: TJavaObjectArray; aValid: boolean;
begin
  Typ:= SigToTyp(FSig);
  aJavaClass:= myComJava2.GetClass(Typ);
  arr:= StringToTStrings(s);
  if Assigned(arr) then begin
    ObjectArr:= CreateObjectArray(arr, aJavaClass, Typ, aValid, FString);
    FValue.s:= CreateJObjectArray(ObjectArr, aJavaClass.Handle);
    arr.Free;
    end
  else
    FValue.s:= nil;
end;

function TJavaValue.StringToJBooleanArray(s: string): TJBooleanArray;
  var arr: TJBooleanArray; p, n: integer; b: boolean; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    if not SysUtils.TryStrToBool(s1, b) then b:= false;
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= b;
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToJByteArray(s: string): TJByteArray;
  var arr: TJByteArray; p, n, i: integer; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    if not (TryStrToInt(s1, i) and (-128 <= i) and (i <= 127)) then i:= 0;
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= i;
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToJCharArray(s: string): TJCharArray;
  var arr: TJCharArray; p, n: integer; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1)) + ' ';
    delete(s, 1, p);
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= Ord(s1[1]);
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToJShortArray(s: string): TJShortArray;
  var arr: TJShortArray; p, n, i: integer; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    if not (TryStrToInt(s1, i) and (-32768 <= i) and (i <= 32767)) then i:= 0;
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= i;
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToJIntArray(s: string): TJIntArray;
  var arr: TJIntArray; p, n, i: integer; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    if not TryStrToInt(s1, i) then i:= 0;
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= i;
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToJLongArray(s: string): TJLongArray;
  var arr: TJLongArray; p, n: integer; i: int64; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    if not TryStrToInt64(s1, i) then i:= 0;
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= i;
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToJFloatArray(s: string): TJFloatArray;
  var arr: TJFloatArray; p, n: integer; f: double; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    if not TryStrToFloat(s1, f) then f:= 0.0;
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= f;
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToJDoubleArray(s: string): TJDoubleArray;
  var arr: TJDoubleArray; p, n: integer; f: double; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  n:= 0;
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    if not TryStrToFloat(s1, f) then f:= 0.0;
    inc(n);
    SetLength(arr, n);
    arr[n-1]:= f;
    p:= Pos(',', s);
  end;
  result:= arr;
end;

function TJavaValue.StringToTStrings(s: string): TStrings;
  var p: integer; s1: string;
begin
  result:= nil;
  if Pos('{', s) = 0 then exit;
  result:= TStringList.Create;
  s:= ReplaceText(s, '"', '');
  s:= ReplaceText(ReplaceText(s, '{', ''), '}', '') + ',';
  p:= Pos(',', s);
  while p > 0 do begin
    s1:= trim(copy(s, 1, p-1));
    delete(s, 1, p);
    result.add(ResolveString(s1));
    p:= Pos(',', s);
  end;
end;

function TryStrToInt64(s: string; var l: int64): boolean;
  var i, sign: integer; c: char;
begin
  Result:= false;
  s:= trim(s);
  l:= 0;
  if length(s) > 0 then begin
    c:= s[1];
    sign:= +1;
    if CharInset(c, ['+', '-']) then begin
      delete(s, 1, 1);
      if c = '-' then sign:= -1;
    end;
    if (sign = -1) and (length(s) = 19 ) and (s > '9223372036854775808') then exit;
    if (sign = +1) and (length(s) = 19) and (s > '9223372036854775807') then exit;
    if length(s) > 19 then exit;
    
    for i:= 1 to length(s) do begin
      c:= s[i];
      if ('0' <= c) and (c <= '9')
        then l:= l * 10 + Ord(c) - Ord('0')
        else exit;
    end;
    l:= sign * l;
    Result:= true;
  end;
end;

function TJavaValue.AsFormattedString: string;
  var s: string;
begin
  FString:= '_x_';
  s:= GetAsString;
  Result:= s;
end;

function TJavaValue.GetAsString: string;
  var SL: TStringList;
      aJavaObject: TJavaObject;
      i, anz: integer; zahl: int64; s: string;
      b: boolean; ch: char; by: byte; sh: short; dou: double;

  function IntToOkt(i: integer): string;
    var s: string; r: integer;
  begin
    while i > 0 do begin
      r:= i mod 8;
      s:= IntTostr(r) + s;
      i:= i div 8;
    end;
    Result:= s;
  end;

  function createNewObject(obj: JObject): string;
    var aJavaClass: TJavaClass; aClass: jClass; aJavaObject: TJavaObject;
        s: string;
  begin
    aClass:= FPEnv^.GetObjectClass(FPEnv, obj);
    aJavaClass:= myComJava2.ClassToJavaClass(aClass);
    if assigned(aJavaClass) then begin
      aJavaObject:= TJavaObject.CreateWithHandle(aJavaClass, obj);
      aJavaObject.Global:= true;
      s:= myComJava2.getNewObjectName(WithoutArray(GetShortType(aJavaClass.Name)));
      if myComJava2.ObjectList.IndexOf(s) = -1 then begin
        myComJava2.ObjectList.AddObject(s, aJavaObject);
        aJavaObject.AddToProperties(s);
      end;
      Result:= s;
    end else
      Result:= '';
  end;

  function getArrayAsString(ob: JObject): string;
    var aJavaClass: TJavaClass;
        jc, arr, obj: JObject;
        SLT: TStrings; SL: TStringList;
        i, j: integer;
        sig: char;
        addr: NativeInt;
  begin
    if assigned(ob) then begin
      Result:= '{';
      anz:= FPEnv^.GetArrayLength(FPEnv, ob);
      jc:= FPEnv^.GetObjectClass(FPEnv, ob);
      aJavaClass:= TJavaClass.Create(jc);
      try
        sig:= aJavaClass.FSig[2];
        if sig = '[' then begin  // array of array
          for i:= 0 to anz-1 do begin
            arr:= FPEnv^.GetObjectArrayElement(FPEnv, ob, i);
            Result:= Result + getArrayAsString(arr) + ', ';
            FPEnv^.DeleteLocalRef(FPENv, arr);
          end;
          Result:= copy(Result, 1, length(Result)-2) + '}';
        end else if Pos(sig, 'ZCBSIJFD') > 0 then begin // primitive
          arr:= FPEnv^.GetPrimitiveArrayCritical(FPEnv, ob, nil);
          addr:= NativeInt(arr);
          for i:= 0 to anz - 1 do
            case sig of
              'Z': begin
                  b:= Boolean(Pointer(addr)^);
                  Result:= Result + BoolToStr(b, true) + ', ';
                  inc(addr, sizeof(jBoolean));
                end;
              'B': begin
                  by:= Byte(Pointer(addr)^);
                  Result:= Result + IntToStr(by) + ', ';
                  inc(addr, sizeof(Byte));
                end;
              'C': begin
                  ch:= Char(Pointer(addr)^);
                  if ch = #0
                    then Result:= Result + '\0, '
                    else Result:= Result + ch + ', ';
                  inc(addr, sizeof(JChar));
                end;
              'S': begin
                  sh:= Short(Pointer(addr)^);
                  Result:= Result + IntToStr(sh) + ', ';
                  inc(addr, sizeof(Short));
                end;
              'I': begin
                  zahl:= Int32(Pointer(addr)^);
                  Result:= Result + IntToStr(zahl) + ', ';
                  inc(addr, sizeof(Int32));
                end;
              'J': begin
                  zahl:= Int64(Pointer(addr)^);
                  Result:= Result + IntToStr(zahl) + ', ';
                  inc(addr, sizeof(Int64));
                end;
              'F': begin
                  dou:= Single(Pointer(addr)^);
                  Result:= Result + ReplaceText(FloatToStr(dou), ',', '.') + ', ';
                  inc(addr, sizeof(Single));
                end;
              'D': begin
                  dou:= Double(Pointer(addr)^);
                  Result:= Result + ReplaceText(FloatToStr(dou), ',', '.') + ', ';
                  inc(addr, sizeof(Double));
                end;
            end;
          Result:= copy(Result, 1, length(Result)-2) + '}';
          FPEnv^.ReleasePrimitiveArrayCritical(FPEnv, ob, arr, 0);
        end else if aJavaClass.FSig = '[Ljava/lang/String;' then begin
          SLT:= JToTStrings(ob);
          for i:= 0 to SLT.Count - 1 do
            if SLT.Strings[i] = 'null'
              then Result:= Result + 'null, '
              else Result:= Result + '"' + SLT.Strings[i] + '", ';
          Result:= Copy(Result, 1, length(Result)-2) + '}';
          SLT.Free;
        end else begin
          SL:= myComJava2.ObjectList;
          for i:= 0 to anz - 1 do begin
            s:= '';
            obj:= FPEnv^.GetObjectArrayElement(FPEnv, ob, i);
            if obj = nil then
              s:= ', null'
            else begin
              try
                for j:= 0 to SL.Count - 1 do begin
                  aJavaObject:= SL.Objects[j] as TJavaObject;
                  if FPEnv^.IsSameObject(FPEnv, obj, aJavaObject.Handle) then begin
                    s:= ', ' + SL.Strings[j];
                    break;
                  end;
                end;
              except
                s:= aJavaObject.toString;
                for j:= 0 to SL.Count - 1 do begin
                  aJavaObject:= SL.Objects[j] as TJavaObject;
                  if aJavaObject.toString = s then begin
                    s:= ', ' + SL.Strings[j];
                    break;
                  end;
                end;
                s:= ', ' + aJavaObject.toString;
              end;
              if s = '' then
                s:= ', ' + createNewObject(obj);
            end;
            Result:= Result + s;
          end;
          Result:= '{' + Right(Result, 4) + '}';
        end;
      finally
        FreeAndNil(aJavaClass);
      end;
    end else
      Result:= 'null';
  end;

begin
  if FString = '_x_' then
    case FKind of
      ntVoid : FString:= '';
      ntBool : FString:= LowerCase(BoolToStr(Value.z, true));
      ntByte : FString:= IntToStr(Value.b);
      ntChar : begin
                 if Value.c = Ord('\') then
                   FString:= '\\'
                 else if Value.c = Ord('"') then
                   FString:= '\"'
                 else if Value.c = Ord('''') then
                   FString:= '\'''
                 else if Value.c < 32 then
                   case Value.c of
                     8: FString:= '\b';
                     9: FString:= '\t';
                    10: FString:= '\n';
                    12: FString:= '\f';
                    13: FString:= '\r';
                    else FString:= '\' + IntToStr(Value.c)
                    end
                  else
                    FString:= Chr(Value.c);
                 FString:= '''' + FString + '''';
               end;
      ntShort: FString:= IntToStr(Value.h);
      ntInt  : FString:= IntToStr(Value.i);
      ntLong : FString:= IntToStr(Value.l);
      ntFloat: FString:= ReplaceText(FloatToStr(Value.f), ',', '.');
      ntDouble: FString:= ReplaceText(FloatToStr(Value.d), ',', '.');
      ntString: if Value.s = nil // default value of a single string is "", not null
                  then begin Result:= ''; exit; end
                  else FString:= '"' + JToDString(Value.s) + '"';
      ntObject: begin
          if Value.s = nil
            then s:= 'null'
          else begin
            s:= '';
            SL:= myComJava2.ObjectList;
            for i:= 0 to SL.Count - 1 do begin
              aJavaObject:= SL.Objects[i] as TJavaObject;
              if FPEnv^.IsSameObject(FPEnv, Value.s, aJavaObject.Handle) then begin
                s:= SL.Strings[i];
                break;
              end;
            end;
            if s = '' then
              s:= createNewObject(Value.s);
          end;
          FString:= s;
        end;
      else
        FString:= getArrayAsString(Value.s);
    end;
  Result:= ReplaceText(FString, #$A, '\n');
end;

function TJavaValue.SetFromString(s: string): boolean;
  var i: integer;
      aJavaObject: TJavaObject;
      aJavaClass: TJavaClass;
      Typ: string;
      l: int64; d: double; c: char;
begin
  Result:= true;
  if (FString <> '_x_') and (FString = s) then exit;
  case FKind of
    ntBool : begin
                s:= LowerCase(s);
                if s = 'true'  then FValue.z:= true
                else if s = 'false' then FValue.z:= false
                else s:= FString;
              end;
    ntChar : if (copy(s, 1, 1) = '\') and (length(s) > 1) then begin
                c:= s[2];
                case c of
                  'b': FValue.c:= 8;
                  't': FValue.c:= 9;
                  'n': FValue.c:= 10;
                  'f': FValue.c:= 12;
                  'r': FValue.c:= 13;
                  '"': FValue.c:= Ord('"');
                  '''': FValue.c:= Ord('''');
                  '\': FValue.c:= Ord('\');
                  else begin
                    if TrystrToInt(Right(s, 2), i) and (i < 256)
                      then FValue.c:= i
                      else FValue.c:= 0;
                  end;
                end
              end
              else begin
                s:= copy(s, 1, 1);
                if length(s) = 1 then FValue.c:= Ord(s[1]) else s:= FString;
              end;
    ntByte : if TryStrToInt(s, i) and (-128 <= i) and (i <= 127)
                then FValue.b:= i else s:= FString;
    ntShort: if TryStrToInt(s, i) and (-32768 <= i) and (i <= 32767)
                then FValue.h:= i else s:= FString;
    ntInt  : if TryStrToInt(s, i) // and (-2147483648 <= i) and (i <= 2147483647)
                then FValue.i:= i else s:= FString;
    ntLong : if TryStrToInt64(s, l) then FValue.l:= l else s:= FString;
    ntFloat: if TryStrToFloat(s, d) then FValue.f:= d else s:= FString;
    ntDouble: if TryStrToFloat(s, d) then FValue.d:= d else s:= FString;
    ntString: if s = 'null'
                then FValue.s:= nil
                else FValue.s:= createJString(ResolveString(s));
    ntObject:
      if s = 'null' then
        FValue.s:= nil
      else begin
        aJavaObject:= myComJava2.GetObject(s);
        if aJavaObject = nil then begin
          Error:= myComJava2.LNGUnknownObject + ' "' + s + '"';
          Result:= false;
         end
        else begin
          Typ:= SigToTyp(FSig);
          aJavaClass:= myComJava2.getClass(Typ);
          if aJavaClass = nil then begin  // due to generic problem
            aJavaClass:= myComJava2.getClass('java.lang.Object');
            if assigned(aJavaClass) then
              FSig:= 'Ljava/lang/Object;';
          end;
          
          if aJavaClass = nil then begin
            Error:= myComJava2.LNGUnknownClass + ' "' + Typ + '"';
            Result:= false;
            end
          else
            if aJavaObject.ClassRef.extends(aJavaClass)
              then FValue.s:= aJavaObject.getHandle
              else begin
                Error:= format(myComJava2.LNGIncompatibelTypes, [aJavaObject.ClassRef.Name, SigToTyp(FSig)]);
                Result:= false;
              end;
        end;
      end;
    ntBoolArray..ntStringArray: begin
      if Assigned(FValue.s) then
        FPEnv^.DeleteLocalRef(FPEnv, FValue.s);
      case FKind of
        ntBoolArray:   SetBoolArrayFromString(s);
        ntByteArray:   SetByteArrayFromString(s);
        ntCharArray:   SetCharArrayFromString(s);
        ntShortArray:  SetShortArrayFromString(s);
        ntIntArray:    SetIntArrayFromString(s);
        ntLongArray:   SetLongArrayFromString(s);
        ntFloatArray:  SetFloatArrayFromString(s);
        ntDoubleArray: SetDoubleArrayFromString(s);
        ntStringArray: SetStringArrayFromString(s);
      end;
    end;
    ntObjectArray: SetObjectArrayFromString(s);
  end;
  FString:= s;
end;

function TJavaValue.ResolveString(s: string): string;
  var aJavaObject: TJavaObject;
begin
  aJavaObject:= myComJava2.getObject(s);
  if assigned(aJavaObject) and (aJavaObject.FClass.getTyp = 'String') then
    s:= aJavaObject.toString;
  if IsJavaString(s)
    then Result:= copy(s, 2, length(s)-2)
    else Result:= s;
end;

{--- TJavaCompiler ------------------------------------------------------------}

constructor TJavaCompiler.Create;
  var PEnv: PJNIEnv;
      Signature: UTF8string;
      ClassH: jClass;
      MID: JMethodID;
      CompilerObject: JObject;
begin
  PEnv:= JNIPointer;
  ClassH:= PEnv^.FindClass(PEnv, 'javax/tools/ToolProvider');
  Signature:= '()Ljavax/tools/JavaCompiler;';
  MID:= PEnv^.getStaticMethodID(PEnv, ClassH, 'getSystemJavaCompiler', PAnsiChar(Signature));
  CompilerObject:= PEnv^.CallStaticObjectMethodA(PEnv, ClassH, MID, nil);
  Signature:= '(Ljava/io/InputStream;Ljava/io/OutputStream;Ljava/io/OutputStream;[Ljava/lang/String;)I';
  ClassH:= PEnv^.FindClass(PEnv, 'javax/tools/JavaCompiler');
  MethodID:= PEnv^.getMethodID(PEnv, ClassH, 'run', PAnsiChar(Signature));
  CompilerClass:= TJavaClass.CreateWithHandle('javax.tools.JavaCompiler', ClassH);
  CompilerClass.setGlobal(true);
  Compiler:= TJavaObject.CreateWithHandle(CompilerClass, CompilerObject);
  Compiler.setGlobal(true);
  Params:= TJavaParams.Create;
  SL:= TStringList.Create;
end;

function TJavaCompiler.Compile(const Classpath, Parameter, Pathname: string): string;
  var i, p: integer; PEnv: PJNIEnv;
      Input, Output: TJavaClass;
      l, m, s, s1, s2, Sourcepath: string;
begin
  PEnv:= JNIPointer;
  SL.Clear;
  s:= ExtractFilePath(Pathname);
  if Pos(s, Classpath) = 0
    then Sourcepath:= s + ';' + Classpath
    else Sourcepath:= Classpath;

  s1:= '-classpath ' + HideBlanks(Classpath) + ' ' + Parameter;
  s1:= ReplaceText(s1, ' ', #13#10);
  s2:= '';
  p:= Pos('"', s1);
  while p > 0 do begin
    l:= Left(s1, p-1);
    delete(s1, 1, p);
    p:= Pos('"', s1);
    if p = 0 then begin
      s1:= s1 + '"';
      p:= length(s1);
    end;
    m:= Left(s1, p-1);
    delete(s1, 1, p);
    s2:= s2 + l + ReplaceText(m, #13#10, ' ');
    p:= Pos('"', s1);
  end;
  s2:= s2 + s1;

  SL.Text:= s2;
  SL.Add('-sourcepath'); SL.Add(Sourcepath);
  SL.Add(Pathname);
  Input := myComJava2.getClass('java.io.InputStream');
  Output:= myComJava2.getClass('java.io.OutputStream');
  Params.Clear;
  Params.addObjectSingle(nil, Input, nil);
  Params.addObjectSingle(nil, Output, nil);
  Params.addObjectSingle(nil, Output, nil);
  Params.addStringArray(SL);
  Args:= PJValue(Params.ArgPointer);
  i:= PEnv^.CallIntMethodA(PEnv, Compiler.Handle, MethodID, Args);
  if i <> 0 then begin
    if PEnv^.ExceptionCheck(PEnv) then
      PEnv^.ExceptionDescribe(PEnv);
    Result:= '-ERR ' + myComJava2.ReadConsole;
  end
  else
    Result:= '+OK  ' + myComJava2.ReadConsole;   // -verbose for example
end;

{ --- Sonstiges -------------------------------------------------------------- }

  function createJString (const s: string): jstring;
    var PEnv: PJNIEnv;
        su: UTF8String;
  begin
    su:= UTF8String(s);
    PEnv:= JNIPointer;
    result:= PEnv^.NewStringUTF(PEnv, PAnsiChar(su));
  end;

  function JToDString(js: JString): string;
  var
   PEnv: PJNIEnv;
   len: Integer;
   CharBuf: PAnsiChar;
   IsCopy: JBoolean;
   s: UTF8String;
  begin
    PEnv:= JNIPointer;
    CharBuf:= PEnv^.GetStringUTFChars(PEnv, js, IsCopy);
    len:= PEnv^.GetStringUTFLength(PEnv, js);
    SetLength(s, len);
    if len > 0 then AnsiStrings.StrLCopy(PAnsiChar(s), Charbuf, len);
    if IsCopy then 
      PEnv^.ReleaseStringUTFChars(PEnv, js, CharBuf);
    Result:= UTF8ToString(s);
  end;

  function JToTStrings(jarr: JObjectarray): TStrings;
  var
    PEnv: PJNIEnv;
    jobj: jobject;
    len, i: Integer;
  begin
    PEnv:= JNIPointer;
    result:= TStringList.Create;
    len:= PEnv^.GetArrayLength(PEnv, jarr);
    for i:= 1 to len do begin
      jobj:= PEnv^.GetObjectArrayElement(PEnv, jarr, i-1);
      if jobj = nil
        then result.Add('null')
        else result.add(JToDString(jobj));
    end;
  end;
    
  function getStringClass: jclass;
  var
    PEnv: PJNIEnv;
  begin
    if sc = Nil then
    begin
      PEnv:= JNIPointer;
      sc:= PEnv^.FindClass(JNIPointer, 'java/lang/String');
      sc:= PEnv^.NewGlobalRef(PEnv, sc);
    end;
    result:= sc;
  end;

  function createJStringArray(myStrings: TStrings): jStringArray;
  var
    i, Count: Integer;
    js: jstring;
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Count:= 0;
    if myStrings <> nil then
      Count:= myStrings.Count;
    js:= createJString('');
    result:= PEnv^.NewObjectArray(PEnv, Count, getStringClass, js);
    for i:= 0 to Count-1 do begin
      js:= createJString(myStrings.Strings[i]);
      PEnv^.SetObjectArrayElement(PEnv, result, i, js);
    end;
  end;

  function CreateObjectArray(Strings: TStrings; aJavaClass: TJavaClass; const Typ: string;
              var Valid: boolean; var AsString: string): TJavaObjectArray;
    var ObjectArr: TJavaObjectArray; i, n: integer;
        aJavaObject: TJavaObject; s: string;
  begin
    n:= 0;
    AsString:= '';
    for i:= 0 to Strings.Count - 1 do begin
      s:= '';
      if (Strings[i] = 'null') or (Uppercase(Strings[i]) = 'POINTER') then begin
        aJavaObject:= nil;
        s:= ', null';
      end
      else begin
        aJavaObject:= myComJava2.GetObject(Strings[i]);
        if aJavaObject = nil then begin
          ErrorMsg(myComJava2.LNGUnknownObject + ' "' + Strings[i] + '"');
          Valid:= false; break;
        end;
        if not aJavaObject.ClassRef.extends(aJavaClass) then begin
          ErrorMsg(format(myComJava2.LNGIncompatibelTypes,
            [aJavaObject.ClassRef.Name, Typ]));
          Valid:= false; break;
        end;
        s:= ', ' + Strings[i];
      end;
      inc(n);
      SetLength(ObjectArr, n);
      ObjectArr[n-1]:= aJavaObject;
      AsString:= AsString + s;
    end;
    AsString:= '{' + Right(AsString, 3) + '}';
    Result:= ObjectArr;
  end;

  function createJObjectArray(ObjectArr: TJavaObjectArray; aClass: jclass): jObjectArray;
    var i, Count: Integer; PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Count:= High(ObjectArr);
    result:= PEnv^.NewObjectArray(PEnv, Count + 1, aClass, nil);
    for i:= 0 to Count do
      if assigned(ObjectArr[i]) then
        PEnv^.SetObjectArrayElement(PEnv, result, i, ObjectArr[i].handle);
  end;

  function createJBooleanArray ( arr: array of JBoolean): JBooleanArray;
    var PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newBooleanArray(PEnv, High(arr) + 1);
    PEnv^.setBooleanArrayRegion(PEnv, result, low(arr), High(arr) +1, @arr);
  end;

  function createJByteArray ( arr: array of JByte): JByteArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newByteArray(PEnv, High(arr) + 1);
    PEnv^.setByteArrayRegion(PEnv, result, 0, High(arr) +1, @arr);
  end;
    
  function createJCharArray ( arr: array of JChar): JCharArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newCharArray(PEnv, High(arr) + 1);
    PEnv^.setCharArrayRegion(PEnv, result, low(arr), High(arr) +1, @arr);
  end;

  function createJShortArray ( arr: array of JShort): JShortArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newShortArray(PEnv, High(arr) + 1);
    PEnv^.setShortArrayRegion(PEnv, result, 0, High(arr) +1, @arr);
  end;
    
  function createJIntArray ( arr: array of Jint): JIntArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newIntArray(PEnv, High(arr) + 1);
    PEnv^.setIntArrayRegion(PEnv, result, low(arr), High(arr) +1, @arr);
  end;
  
  function createJLongArray ( arr: array of JLong): JLongArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newLongArray(PEnv, High(arr) + 1);
    PEnv^.setLongArrayRegion(PEnv, result, low(arr), High(arr) +1, @arr);
  end;

  function createJFloatArray ( arr: array of JFloat): JFloatArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newFloatArray(PEnv, High(arr) + 1);
    PEnv^.setFloatArrayRegion(PEnv, result, low(arr), High(arr) + 1, @arr);
  end;

  function createJDoubleArray (arr: array of JDouble): JDoubleArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    result:= PEnv^.newDoubleArray(PEnv, High(arr) + 1);
    PEnv^.setDoubleArrayRegion(PEnv, result, 0, High(arr) +1, @arr);
  end;

  function TypToSig(Typ: string): string;
    var dimension: string; p: integer;
  begin
    Result:= '';
    dimension:= '';
    p:= Pos('[]', Typ);
    while p > 0 do begin
      dimension:= dimension + '[';
      delete(Typ, p, 2);
      p:= Pos('[]', Typ);
    end;
    if Typ = 'String'    then Result:= 'Ljava/lang/String;' else
    if Typ = 'java.lang.String' then Result:= 'Ljava/lang/String;'  else
    if Typ = 'boolean'   then Result:= 'Z' else
    if Typ = 'char'      then Result:= 'C' else
    if Typ = 'byte'      then Result:= 'B' else
    if Typ = 'short'     then Result:= 'S' else
    if Typ = 'int'       then Result:= 'I' else
    if Typ = 'long'      then Result:= 'J' else
    if Typ = 'float'     then Result:= 'F' else
    if Typ = 'double'    then Result:= 'D' else
    if Typ = 'void'      then Result:= 'V'
    else begin
      Typ:= WithoutGeneric(Typ); // z. B. LinkedList<Schueler>
      Result:= 'L' + ReplaceText(Typ, '.', '/') + ';';
    end;
    Result:= dimension + Result;
  end;

  function SigToShortTyp(const Sig: string): string;
  begin
    Result:= GetShortType(SigToTyp(Sig));
  end;

end.
