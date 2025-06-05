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
    constructor Create(Posi: PJavaVM);
    destructor Destroy; override;

    // convenience method to call a method's static main
    // uses delphi's native TStrings to pass the
    // array of string args
    class procedure CallMain(const AClassname: string ; strings: TStrings);
    
    // Convenience method. Calls Exit procedure
    class procedure CallExit(exitCode: Integer);
  
    // procedure to explicitly detach a local reference.
    class procedure freeRef(jobj: JObject; isGlobal: Boolean);

    // returns the current JNI environment pointer.
    class function getPEnv: PJNIEnv;
    
    // IMPORTANT: The following method must be called by native methods
    // that receive a PEnv argument if they intend to use this unit.
    class procedure setThreadPEnv(Posi: PJNIEnv);

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
    FValid: Boolean;
    FError: string;
    function GetAsString: string;
    function ResolveString(Str: string): string;
  public
    constructor Create(aValue: JValue; Kind: TNumType; const Sig: string); overload;
    constructor Create(Typ: string); overload;
    constructor Create(const Value, Sig: string); overload;
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

    procedure SetBoolArrayFromString(const Str: string);
    procedure SetByteArrayFromString(const Str: string);
    procedure SetCharArrayFromString(const Str: string);
    procedure SetShortArrayFromString(const Str: string);
    procedure SetIntArrayFromString(const Str: string);
    procedure SetLongArrayFromString(const Str: string);
    procedure SetFloatArrayFromString(const Str: string);
    procedure SetDoubleArrayFromString(const Str: string);
    procedure SetStringArrayFromString(const Str: string);
    procedure SetObjectArrayFromString(const Str: string);

    function StringToJBooleanArray(Str: string): TJBooleanArray;
    function StringToJByteArray(Str: string): TJByteArray;
    function StringToJCharArray(Str: string): TJCharArray;
    function StringToJShortArray(Str: string): TJShortArray;
    function StringToJIntArray(Str: string): TJIntArray;
    function StringToJLongArray(Str: string): TJLongArray;
    function StringToJFloatArray(Str: string): TJFloatArray;
    function StringToJDoubleArray(Str: string): TJDoubleArray;
    function StringToTStrings(Str: string): TStrings;

    function AsFormattedString: string;
    function SetFromString(Str: string): Boolean;
    property Value: JValue read FValue write FValue;
    property Kind: TNumType read FKind write FKind;
    property AsString: string read GetAsString;
    property Valid: Boolean read FValid;
    property Error: string read FError write FError;
  end;

{Delphi class to encapsulate list of params to Java method.}

  TJavaParams = class
  private
    RefList: TList; //a list of references to be freed by the destructor.
    FSig: string;
    FValid: Boolean;
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
    procedure addString(const Str: string); overload;
    procedure addStringAsObject(const Str: string);
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
    property Signature: string read FSig write FSig;
  // a pointer to the buffer that contains the Parameters to be passed.
    property argPointer: TJValueArray read FArgPointer;
    property IsValid: Boolean read FValid;
    property Error: string read FError write FError;
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
                       const Name: string;
                       methodType: TMethodAttribute;
                       returnType: TNumType;
                       params: TJavaParams;
                       const retClassSig: string);
    // a minimal constructor for virtual methods that
    // takes no arguments and return nothing.
    constructor CreateVoid(cls: TJavaClass; const Name: string);
    // TJavaMethod needs no destructor
    // because FMethodID needs no ressources
    function Call(params: TJavaParams ; jobj: TJavaObject): JValue;
    function isValid: Boolean;
    property Error: string read FError;
  end;

  TJavaAttribute = class
  private
    FClass: TJavaClass;
    FAttributeID: JFieldID;
    FPEnv: PJNIEnv;
  public
    Static_ : Boolean;
    Final_  : Boolean;
    isObject: Boolean;
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
    function GetAttributes: string;
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
    constructor CreateLoadClass(const AClassname: string; Pathname: string);
    destructor Destroy; override;
    class function FindClass(const AClassname: string): string;
    class function IsInterface(const AClassname: string): string;
    class function getSuperClassFromName(const AClassname: string): string;
    class function cookClass(const Pathname: string): string;
    class function getPackage(const AClassname: string): string;
    procedure detecterror;
    // returns a handle to a new instance of this class.
    function Instantiate(params: TJavaParams): TJavaObject;
    function extends(JavaClass: TJavaClass): Boolean;
    function hasSuperClass: Boolean;
    function getSuperClass: TJavaClass;
    function getSuperclassName: string;
    function IsAbstract: Boolean;
    function GetAttributes: string;
    function getAttributeNames: string;
    function getRefreshedAttributeNames: string;
    function getConstructors: string;
    function getMethods(const _Static: string): string;
    function getOuterClassSig: string;

    property Handle: jobject read GetHandle;
    property LocalHandle: jobject read FLocalHandle;
    property Signature: string read FSig write FSig;
    property Name: string read FName;
    property ImportTyp: string read getImportTyp;
    property Valid: Boolean read isValid;
    property Global: Boolean read isGlobal write setGlobal;
    property Pathname: string read FPathname write FPathname;
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
    constructor Create;
    function Compile(const Classpath, Parameter, Pathname: string): string;
  end;

{Exceptions to be raised when stuff goes wrong with the Java runtime.}

  EJvmException = class(Exception);
  EJavaClassNotFound = class(EJvmException);
  EJavaMethodNotFound = class(EJvmException);
  EJavaObjectInstantiation = class(EJvmException);
  EInvalidJNIHandle = class(EJvmException);

{ Various utility functions for creating java objects from delphi objects.}
  function createJString (const Str: string ): jstring;
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
              var Valid: Boolean; var AsString: string): TJavaObjectArray;

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

  function TryStrToFloat(var Str: string; var f: double): Boolean;
    var Posi: Integer;
  begin
    Result:= True;
    try
      Posi:= Pos('.', Str);
      if (Posi > 0) and (FormatSettings.DecimalSeparator <> '.') then
        Str[Posi]:= FormatSettings.DecimalSeparator;
      Posi:= Pos(',', Str);
      if (Posi > 0) and (FormatSettings.DecimalSeparator <> ',') then
        Str[Posi]:= FormatSettings.DecimalSeparator;
      f:= StrToFloat(Str);
    except
      Result:= False;
    end;
  end;

  function JNIPointer: PJNIEnv;
  begin
    Result:= PEnvGlobal;
    if (not SingleThreaded) or (PEnvGlobal = nil) then begin
      Result:= PEnvThread;
      if SingleThreaded then PEnvGlobal:= PEnvThread;
    end;
    if Result = nil then begin
      Result:= PEnvThread;
      if SingleThreaded then
        PEnvGlobal:= PEnvThread;
    end;
    if Result = nil then
      raise EJVMException.Create('JVM not loadad. No PEnv pointer is available');
  end;

  { --- TJavaVM ---------------------------------------------------------------}

  constructor TJavaVM.Create(Posi: PJavaVM);
  begin
    pvm:= Posi;
  end;
    
  destructor TJavaVM.Destroy;
  begin
    {if pvm <> Nil then
      CallExit(0); }
    inherited Destroy;
  end;

  class function TJavaVM.getPEnv: PJNIEnv;
  begin
    Result:= JNIPointer;
  end;
    
  class procedure TJavaVM.setThreadPEnv(Posi: PJNIEnv);
  begin
    PEnvThread:= Posi;
    PEnvGlobal:= Posi;
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


  class procedure TJavaVM.CallMain(const AClassname: string ; strings: TStrings);
  var
    classID: jclass;
    methodID: jmethodID;
    stringArray: jarray;
    PEnv: PJNIEnv;
    uClassname: UTF8String;

    function dotToSlash(const Str : UTF8String) : UTF8String;
    var
     Int: Integer;
    begin
      Result:= Str;
      for Int := 1 to Length(Result) do
        if Result[Int] = '.' then
          Result[Int] := '/';
    end;

  begin
    PEnv:= JNIPointer;
    uClassname:= dotToSlash(UTF8Encode(AClassname));
    classID:= PEnv^.FindClass(PEnv, PAnsiChar(uClassname));
    if classID = nil then 
      raise EJavaClassNotFound.Create('Could not find class ' + classname);
    methodID:= PEnv^.GetStaticMethodID(PEnv, classID, 'main', '([Ljava/lang/String;)V');
    if methodID = nil then
      raise EJavaMethodNotFound.Create('Could not find main method in class ' + AClassname);
    stringArray:= createJStringArray(strings);
    PEnv^.CallStaticVoidMethodV(PEnv, classID, methodID, @stringArray);
    FreeRef(stringArray, False);
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
    var ClassH: jClass; FMethodID: jMethodID; aValue: JValue; Str: string;
  begin
    FPEnv:= JNIPointer;
    FLocalHandle:= jc;
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
    FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'getName', '()Ljava/lang/String;');
    aValue.Str:= FPEnv^.CallObjectMethod(FPEnv, jc, FMethodID);
    if Assigned(aValue.Str)
      then Str:= JToDString(aValue.Str)
      else Str:= 'UnknownClass';
    FName:= correctGetName(Str);
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

  constructor TJavaClass.CreateLoadClass(const AClassname: string; Pathname: string);
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
    Packagename:= Copy(AClassname, 1, LastDelimiter('.', AClassname)-1);
    if Length(Packagename) > 0 then
      Path:= Copy(Path, 1, Length(Path) - Length(Packagename) - 1);
    FName:= AClassname;
    FSig:= TypToSig(AClassname);
    Pathname:= ChangeFileExt(Pathname, '.class');
    myClass:= myComJava2.getClass('JEClassLoader');
    if Assigned(myClass) then begin
      myParams:= TJavaParams.Create;
      try
        myParams.addString(Path);
        myParams.addString(AClassname);
        try
          myMethod:= TJavaMethod.Create(myClass, 'loadClass', static, ntObject, myParams, 'Ljava/lang/Class;');
          try
            if Assigned(MyMethod) and (myMethod.FMethodID <> nil) then begin
              aValue:= myMethod.Call(myParams, nil);
              Error:= myMethod.FError;
              FLocalHandle:= aValue.Str;
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
    if Assigned(FPEnv) then
      if Assigned(FGlobalHandle)
        then FPEnv^.DeleteGlobalRef(FPEnv, FGlobalHandle)
        else FPEnv^.DeleteLocalRef(FPEnv, FLocalHandle);
    FLocalHandle:= nil;
    FGlobalHandle:= nil;
    FreeAndNil(FAttributesList);
  end;

  class function TJavaClass.FindClass(const AClassname: string): string;
    var PEnv: PJNIEnv; Posi: JObject; Str: UTF8String;
  begin
    PEnv:= JNIPointer;
    Str:= UTF8Encode(TypToSig(AClassname));
    Posi:= PEnv^.FindClass(PEnv, PAnsiChar(Str));
    if Assigned(Posi) then begin
      PEnv^.DeleteLocalRef(PEnv, Posi);
      Result:= '+OK ';
    end else begin
      PEnv^.ExceptionDescribe(PEnv);
      Result:= '-NO ' + myComJava2.ReadConsole;
    end;
  end;

  class function TJavaClass.IsInterface(const AClassname: string): string;
    var PEnv: PJNIEnv; Posi: JObject; uClassname: UTF8String;
        ClassH: jclass; FMethodID: jMethodID;
  begin
    PEnv:= JNIPointer;
    uClassname:= UTF8encode(TypToSig(AClassname));
    Posi:= PEnv^.FindClass(PEnv, PAnsiChar(uClassname));
    if Assigned(Posi)then begin
      ClassH:= PEnv^.FindClass(PEnv, 'java/lang/Class');
      FMethodID:= PEnv^.getMethodID(PEnv, ClassH, 'isInterface', '()Z');
      if PEnv^.CallBooleanMethod(PEnv, Posi, FMethodID)
        then Result:= '+OK'
        else Result:= '-NO';
      PEnv^.DeleteLocalRef(PEnv, ClassH);
      PEnv^.DeleteLocalRef(PEnv, Posi);
      end
    else begin
      PEnv^.ExceptionDescribe(PEnv);
      Result:= '-ERR ' + myComJava2.ReadConsole;
    end;
  end;

  class function TJavaClass.getSuperClassFromName(const AClassname: string): string;
    var ClassObjectH, ClassH: jclass; aValue: JValue;
        FMethodID: jMethodID; uSignature: UTF8String;
        PEnv: PJNIEnv; Posi: jobject;
  begin
    PEnv:= JNIPointer;
    uSignature:= UTF8Encode(TypToSig(AClassname));
    Posi:= PEnv^.FindClass(PEnv, PAnsiChar(uSignature));
    if Assigned(Posi) then begin
      ClassObjectH:= PEnv^.GetSuperClass(PEnv, Posi); // get the Class-object
      if Assigned(ClassObjectH) then begin
        ClassH:= PEnv^.FindClass(PEnv, 'java/lang/Class');
        FMethodID:= PEnv^.getMethodID(PEnv, ClassH, 'getName', '()Ljava/lang/String;');
        aValue.Str:= PEnv^.CallObjectMethod(PEnv, ClassObjectH, FMethodID);
        Result:= '+OK ' + JToDString(aValue.Str);
        PEnv^.DeleteLocalRef(PEnv, ClassH);
        PEnv^.DeleteLocalRef(PEnv, aValue.Str);
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
    if Assigned(ClassH) then begin
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

  class function TJavaClass.getPackage(const AClassname: string): string;
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
    if Assigned(ClassH) then begin
      aSignature:= '(Ljava/lang/String;)Ljava/lang/String;';
      MethodID:= PEnv^.GetMethodID(PEnv, ClassH, 'forName', PAnsiChar(aSignature));
      if MethodID <> nil then begin
        myParams:= TJavaParams.Create;
        myParams.addString(AClassname);
        Args:= PJValue(myParams.ArgPointer);
        aValue.Str:= PEnv^.CallObjectMethodA(PEnv, ClassH, MethodID, Args);
        if Assigned(aValue.Str) then begin
          Result:= '+OK ' + JToDString(aValue.Str);
          PEnv^.DeleteLocalRef(PEnv, aValue.Str);
        end else begin
          if PEnv^.ExceptionCheck(PEnv) then begin
            PEnv^.ExceptionDescribe(PEnv);
            Result:= '-ERR ' + myComJava2.ReadConsole;
          end;
          Result:= Result + #$0D#$0A + '-ERR Class ' + AClassname + ' not found.';
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
    Result:= TJavaObject.Create(Self, params)
  end;

  function TJavaClass.extends(JavaClass: TJavaClass): Boolean;
  begin
    Result:= FPEnv^.isAssignableFrom(FPEnv, Handle, JavaClass.Handle);
  end;

  function TJavaClass.getHandle: jobject;
  begin
    Result:= FGlobalHandle;
    if Result = nil then Result:= FLocalHandle;
  end;

  function TJavaClass.getTyp: string;
    var Posi: Integer; Str: string;
  begin
    Str:= FSig;
    Posi:= Pos('/', Str);
    while Posi > 0 do begin
      Delete(Str, 1, Posi);
      Posi:= Pos('/', Str);
    end;
    Result:= Str;
  end;

  function TJavaClass.getImportTyp: string;
  begin
    Result:= ReplaceText(FSig, '/', '.');
  end;

  function TJavaClass.isValid: Boolean;
  begin
    if isGlobal then
      Result:= True
    else
      Result:= (FLocalHandle <> Nil) and (FPEnv = JNIPointer);
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
    Result:= FGlobalHandle <> nil;
  end;

  function TJavaClass.hasSuperClass: Boolean;
    var h: jclass;
  begin
    h:= FPEnv^.GetSuperClass(FPEnv, Self.handle);
    Result:= Assigned(h);
    FPEnv^.DeleteLocalRef(FPEnv, h);
  end;

  function TJavaClass.getSuperClass: TJavaClass;
    var AClassname: string;
  begin
    AClassname:= getSuperClassname;
    if AClassname = ''
      then Result:= nil
      else Result:= myComJava2.GetClass(AClassname);
  end;

  function TJavaClass.getSuperClassname: string;
    var ClassObjectH, ClassH: jclass; aValue: JValue;
        FMethodID: jMethodID; 
  begin
    ClassObjectH:= FPEnv^.GetSuperClass(FPEnv, Self.handle); // get the Class-object
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
    FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'getName', '()Ljava/lang/String;');
    aValue.Str:= FPEnv^.CallObjectMethod(FPEnv, ClassObjectH, FMethodID);
    if Assigned(aValue.Str) then begin
      Result:= JToDString(aValue.Str);
      FPEnv^.DeleteLocalRef(FPEnv, aValue.Str);
    end else begin
      if FPEnv^.ExceptionCheck(FPEnv) then begin
        FPEnv^.ExceptionDescribe(FPEnv);
        myComJava2.ReadConsole;
      end;
      Result:= '';
    end;
    FPEnv^.DeleteLocalRef(FPEnv, ClassH);
  end;

  function TJavaClass.IsAbstract: Boolean;
    var ClassH: jclass; Int: Integer; FMethodID: jMethodID;
  begin
    ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
    FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'getModifiers', '()I');
    Int:= FPEnv^.CallIntMethod(FPEnv, Self.Handle, FMethodID);
    Result:= (Int and 1024) = 1024;
    FPEnv^.DeleteLocalRef(FPEnv, ClassH);
  end;

  function TJavaClass.GetAttributes: string;
    var JavaLangClass, FieldClass: jclass;
        DeclaredFieldID: jMethodID;

    function ToString(o: jobject): string;
      var classH: jclass; FMethodID: jMethodID; objclass: jclass;
    begin
      ClassH:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      FMethodID:= FPEnv^.getMethodID(FPEnv, ClassH, 'toString', '()Ljava/lang/String;');
      objClass:= FPEnv^.CallObjectMethod(FPEnv, o, FMethodID);
      if Assigned(objClass)               // TK;
        then Result:= JToDString(objClass)
        else Result:= 'UnknownClass';
    end;

    function checkException(obj: jObject): string;
    begin
      if Assigned(obj) then
        Result:= '+OK'
      else
        if FPEnv^.ExceptionCheck(FPEnv) then begin
          FPEnv^.ExceptionDescribe(FPEnv);
          Result:= myComJava2.ReadConsole;
        end
        else Result:= 'error?';
    end;

    procedure ProcessAttribute(isInherited: Boolean; ss, sgs: string);
      var Posi: Integer; Str1, Str2, Str3, sg1: string;
          vis: TVisibility;
          Attr: TJavaAttribute;

      function GetShortType(Str: string): string;
        var Posi: Integer;
      begin
        Delete(Str, 1, LastDelimiter('.', Str));
        Posi:= Pos('<', Str);
        if Posi > 1 then
          Str:= Copy(Str, 1, Posi-1);
        Result:= Str;
      end;


    begin
      vis:= viPackage;
      Str1:= getNextPart(ss);
      sg1:= getNextPart(sgs);
      if IsVisibility(Str1) then begin
        vis:= String2Visibility(Str1);
        Str1:= getNextPart(ss);
        sg1:= getNextPart(sgs)
      end;
      if isInherited and (vis = viPrivate) and
         (myComJava2.ShowInheritedPrivateAttributes =  '0') then Exit;

      Attr:= TJavaAttribute.Create(Self);
      Attr.static_:= False;
      Attr.final_ := False;
      Attr.vis    := vis;
      while IsModifier(Str1) do begin
        if Str1 = 'static' then
          Attr.static_:= True;
        if Str1 = 'final' then
          Attr.final_:= True;
        Str1:= getNextPart(ss);
        sg1:= getNextPart(sgs);
      end;

      Posi:= Pos('$', Str1);
      // java.util.LinkedList.java.util.LinkedList$Node<E>
      // sgs: private BinaryTree<ContentType>.BTNode<ContentType> BinaryTree.node is without $
      // ss : private BinaryTree$BTNode BinaryTree.node is with $
      if Posi > 0 then begin
        Str2:= Copy(Str1, 1, Posi-1);
        Posi:= Length(Str2) div 2;
        Str3:= System.Copy(Str2, 1, Posi);
        Str2:= System.Copy(Str2, Posi + 2, 255);
        if Str2 = Str3 then Delete(Str1, 1, Posi+1);
      end;
      Posi:= Pos('.', sg1);
      while Posi > 0 do begin
        Delete(sg1, 1, Posi);
        Posi:= Pos('.', sg1);
      end;

      Posi:= Pos('<', sg1);
      if Posi > 0 then begin
        Attr.Generic:= Copy(sg1, Posi, Length(sg1));
        sg1:= Copy(Str1, 1, Posi-1);
      end;
      if (sg1 = 'E') or (sg1 = 'K') or (sg1 = 'V')
        then Attr.Typ:= 'java.lang.Object'
        else Attr.Typ:= Str1;
      Attr.Sig:= TypToSig(Attr.Typ);
      Attr.isObject:= (SigToNumType(Attr.Sig) in [ntObject, ntObjectArray]) and not Attr.Static_;
      Attr.Name:= GetShortType(ss);
      FAttributesString:= FAttributesString + #4 + Attr.ToString;
      FAttributesNames:= FAttributesNames + #4 + Attr.Name;
      FAttributesList.Add(Attr);
    end;

    procedure GetInheritedAttributes(aJavaClass: TJavaClass; isInherited: Boolean);
      var Count, Int: Integer; aJavaClass2: TJavaClass;
          Arr, aAttribut: jobject;
          aValue1, aValue2: JValue;
          FMethodID1, FMethodID2: jMethodID;
          Str1, Str2: string;
    begin
      if aJavaClass.hasSuperClass then begin
        aJavaClass2:= aJavaClass.getSuperClass;
        if Assigned(aJavaClass2) then
          GetInheritedAttributes(aJavaClass2, True);
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
      for Int:= 0 to Count - 1 do begin
        aAttribut:= FPEnv^.GetObjectArrayElement(FPEnv, Arr, Int);
        aValue1.Str:= FPEnv^.CallObjectMethod(FPEnv, aAttribut, FMethodID1);
        Str1:= JToDString(aValue1.Str);
        aValue2.Str:= FPEnv^.CallObjectMethod(FPEnv, aAttribut, FMethodID2);
        Str2:= JToDString(aValue2.Str);
        ProcessAttribute(isInherited, Str1, Str2);
        FPEnv^.DeleteLocalRef(FPEnv, aValue1.Str);
        FPEnv^.DeleteLocalRef(FPEnv, aAttribut);
      end;
      // release ressources
      FPEnv^.DeleteLocalRef(FPEnv, FieldClass);
      FPEnv^.DeleteLocalRef(FPEnv, Arr);
    end;

  begin
    if not Assigned(FAttributesList) or (Pos('|', FAttributesString) = 0) then begin
      FAttributesList:= TObjectList.Create;
      FAttributesString:= '';
      FAttributesNames:= '';
      JavaLangClass:= FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      DeclaredFieldID:= FPEnv^.GetMethodID(FPEnv, JavaLangClass, 'getDeclaredFields', '()[Ljava/lang/reflect/Field;');
      GetInheritedAttributes(Self, False);
      FPEnv^.DeleteLocalRef(FPEnv, JavaLangClass);
    end;
    Result:= FAttributesString;
  end; // TJavaClass.GetAttributes: string;

  function TJavaClass.getAttributeNames: string;
  begin
    if not Assigned(FAttributesList) then
      GetAttributes;
    Result:= FAttributesNames;
  end;

  function TJavaClass.getRefreshedAttributeNames: string;
  begin
    if Assigned(FAttributesList) then
      FreeAndNil(FAttributesList);
    GetAttributes;
    Result:= FAttributesNames;
  end;

  // get public constructors only
  function TJavaClass.getConstructors: string;
    var Str: string; Count, Int: Integer;
        ClassH, ClassC: jclass; Arr, aConstructor: jobject;
        aValue: JValue; FMethodID: jMethodID;
  begin
    Str:= '';
    try
      // get the constructors-array
      ClassH   := FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassH, 'getConstructors', '()[Ljava/lang/reflect/Constructor;');
      Arr      := FPEnv^.CallObjectMethod(FPEnv, Self.Handle, FMethodID);
      Count    := FPEnv^.GetArrayLength(FPEnv, Arr);

      // get a description of every constructor
      ClassC  := FPEnv^.FindClass(FPEnv, 'java/lang/reflect/Constructor');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassC, 'toGenericString', '()Ljava/lang/String;');
      for Int:= 0 to Count - 1 do begin
        aConstructor:= FPEnv^.GetObjectArrayElement(FPEnv, Arr, Int);
        aValue.Str:= FPEnv^.CallObjectMethod(FPEnv, aConstructor, FMethodID);
        Str:= Str + #13#10 + JToDString(aValue.Str);
        FPEnv^.DeleteLocalRef(FPEnv, aValue.Str);
        FPEnv^.DeleteLocalRef(FPEnv, aConstructor);
      end;
      // release ressources
      FPEnv^.DeleteLocalRef(FPEnv, Arr);
      FPEnv^.DeleteLocalRef(FPEnv, ClassC);
      FPEnv^.DeleteLocalRef(FPEnv, ClassH);
      Result:= '+OK ' + Str;
    except
      on e: Exception do
        Result:= '-ERR ' + e.Message;
    end;
  end;

  function TJavaClass.getMethods(const _Static: string): string;
    var Count, Int: Integer; Str, Str1: string;
        ClassH, ClassC: jclass; Arr, aMethod: jobject;
        aValue: JValue; FMethodID: jMethodID;
  begin
    Str:= '';
    try
      // get the methods-array
      ClassH   := FPEnv^.FindClass(FPEnv, 'java/lang/Class');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassH, 'getDeclaredMethods', '()[Ljava/lang/reflect/Method;');
      Arr      := FPEnv^.CallObjectMethod(FPEnv, Self.Handle, FMethodID);
      Count    := FPEnv^.GetArrayLength(FPEnv, Arr);

      // get a description of every method
      ClassC  := FPEnv^.FindClass(FPEnv, 'java/lang/reflect/Method');
      FMethodID:= FPEnv^.GetMethodID(FPEnv, ClassC, 'toGenericString', '()Ljava/lang/String;');
      for Int:= 0 to Count - 1 do begin
        aMethod:= FPEnv^.GetObjectArrayElement(FPEnv, Arr, Int);
        aValue.Str:= FPEnv^.CallObjectMethod(FPEnv, aMethod, FMethodID);
        Str1:= JToDString(aValue.Str);
        if Str1 <> '' then begin
          if (_Static = 'static') and (Pos('static', Str1) > 0) then
            Str:= Str + #13#10 + Str1;
          if (_Static = 'not static') and (Pos('static', Str1) = 0) then
            Str:= Str + #13#10 + Str1;
          if _Static = '' then
            Str:= Str + #13#10 + Str1;
        end;
        FPEnv^.DeleteLocalRef(FPEnv, aValue.Str);
        FPEnv^.DeleteLocalRef(FPEnv, aMethod);
      end;
      // release ressources
      FPEnv^.DeleteLocalRef(FPEnv, Arr);
      FPEnv^.DeleteLocalRef(FPEnv, ClassC);
      FPEnv^.DeleteLocalRef(FPEnv, ClassH);
      Result:= '+OK ' + Str;
    except
      on e: Exception do
        Result:= '-ERR ' + e.Message;
    end;
  end;

  function TJavaClass.getOuterClassSig: string;
    var Posi: Integer;
  begin
    Result:= '';
    Posi:= Length(FSig);
    while (Posi > 0) and (FSig[Posi] <> '$') do Dec(Posi);
    if Posi > 0 then begin
      Result:= FSig;
      Delete(Result, Posi, Length(Result));
      Result:= Result + ';';
    end;
  end;

  {--- TJavaObject ------------------------------------------------------------}

  constructor TJavaObject.Create(jcl: TJavaClass; params: TJavaParams);
  var
    Signature, Str: string; uSig: UTF8String;
    MethodID: JMethodID;
    Args: PJValue;
  begin
    Str:= '';
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
          Str:= myComJava2.ReadConsole;
        end;
        FError:= 'Could not create new instance of ' + ReplaceText(jcl.signature, '/', '.') + ' ' + Str;
      end
    end else begin
      if FPEnv^.ExceptionCheck(FPEnv) then begin
        FPEnv^.ExceptionDescribe(FPEnv);
        Str:= myComJava2.ReadConsole;
      end;
      FError:= 'No such constructor ' + Signature + ' - ' + Str;
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
          FLocalHandle:= aJValue.Str;
          setGlobal(True);
          if Assigned(FLocalHandle) then begin
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
          if Assigned(aJValue.Str) then
            FPEnv^.DeleteGlobalRef(FPEnv, aJValue.Str);
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
      Result:= JNIPointer
    else
      Result:= FPEnv;
  end;

  function TJavaObject.Equals(JavaObject: TJavaObject): Boolean;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= getPEnv;
    if (not Self.Valid) or (not JavaObject.Valid) then
      raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
    Result:= PEnv^.IsSameObject(PEnv, Handle, JavaObject.Handle);
  end;

  function TJavaObject.isInstanceOf(JavaClass: TJavaClass): Boolean;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= GetPEnv;
    if (not Self.Valid) or (not JavaClass.Valid) then 
        raise EInvalidJNIHandle.Create('Attempt to use JNI local object reference in a different thread.');
    Result:= PEnv^.IsInstanceOf(PEnv, Handle, JavaClass.Handle);
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
    Result:= FGlobalHandle <> Nil;
  end;
    
  function TJavaObject.isValid: Boolean;
  begin
    if isGlobal
      then Result:= True
      else Result:= (FLocalHandle <> Nil) and (FPEnv = JNIPointer);
  end;

  function TJavaObject.GetAttributes: string;
  begin
    Result:= FClass.GetAttributes;
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

  function TJavaObject.ToString: string;
    var
      toStringMethod: jmethodID;
      js: jstring; Str: string;
      PEnv: PJNIEnv;
  begin
    PEnv:= getPEnv;
    toStringMethod:= PEnv^.getMethodID(PEnv, classRef.Handle, 'toString', '()Ljava/lang/String;');
    js:= PEnv^.callObjectMethod(PEnv, Handle, toStringMethod);
    if js <> nil
      then Str:= JToDString(js)
    else begin
      if PEnv^.ExceptionCheck(PEnv) then
        PEnv^.ExceptionDescribe(PEnv);
      Str:= myComJava2.ReadConsole;
    end;
    Result:= Str;
  end;

{ --- TJavaParams ------------------------------------------------------------ }

  constructor TJavaParams.Create;
  begin
    RefList:= TList.Create;
  end;
    
  destructor TJavaParams.Destroy;
  var
    Int: Integer;
  begin
    for Int:= 0 to RefList.Count - 1 do
      TJavaVM.FreeRef(Reflist.Items[Int], False);
    RefList.Free;
    if Length(FArgPointer) >0
      then SetLength(FArgPointer, 0);
    inherited Destroy;
  end;

  procedure TJavaParams.Clear;
    var Int: Integer;
  begin
    for Int:= 0 to RefList.Count - 1 do
      TJavaVM.FreeRef(Reflist.Items[Int], False);
    RefList.Clear;
    SetLength(FArgPointer, 0);
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
    RefList.Add(value.Str);
  end;

  procedure TJavaParams.addString(const Str: string);
    var js: JString; val: JValue;
  begin
    js:= createJString(Str);
    val.Str:= js;
    addToArgBuffer(val);
    FSig:= FSig + 'Ljava/lang/String;';
    RefList.Add(js);
  end;

  procedure TJavaParams.addStringAsObject(const Str: string);
    var js: JString; val: JValue;
  begin
    js:= createJString(Str);
    val.Str:= js;
    addToArgBuffer(val);
    FSig:= FSig + 'Ljava/lang/Object;';
    RefList.Add(js);
  end;

  procedure TJavaParams.addObjectSingle(value: TJavaObject; jcl: TJavaClass; aValue: TJavaValue);
    var jo: JObject; val: JValue;
  begin
    if Assigned(value)
      then jo:= value.Handle
      else jo:= nil;
    val.Str:= jo;
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
    val.Str:= jo;
    addToArgBuffer(val);
    if Assigned(value.ClassRef)
      then FSig:= FSig + value.ClassRef.Signature;
  end;

  procedure TJavaParams.addObjectArray(arr: TJavaObjectArray; jcl: TJavaClass);
    var
      PEnv: PJNIEnv;
      jarr: jobjectarray;
      Int: Integer; val: JValue;
  begin
    PEnv:= JNIPointer;
    jarr:= PEnv^.NewObjectArray(PEnv, High(Arr)+1, jcl.Handle, arr[0].Handle);
    for Int:= 1 + Low(arr) to High(arr) do
      PEnv^.setObjectArrayElement(PEnv, jarr, Int, arr[Int].Handle);
    val.Str:= jarr;
    addToArgBuffer(val);
    FSig:= FSig + '[' + jcl.Signature;
    RefList.Add(jarr)
  end;
    
  procedure TJavaParams.addBooleanArray(arr: array of JBoolean);
    var jbarray: JBooleanArray; val: JValue;
  begin
    jbarray:= createJBooleanArray(arr);
    val.Str:= jbarray;
    addToArgBuffer(val);
    FSig:= FSig + '[Z';
    RefList.Add(jbarray)
  end;

  procedure TJavaParams.addByteArray(arr: array of JByte);
    var jbarray: JByteArray; val: JValue;
  begin
    jbarray:= createJByteArray(arr);
    val.Str:= jbarray;
    addToArgBuffer(val);
    FSig:= FSig + '[B';
    RefList.Add(jbarray)
  end;

  procedure TJavaParams.addCharArray(arr: array of JChar);
    var jcarray: JCharArray; val: JValue;
  begin
    jcarray:= createJCharArray(arr);
    val.Str:= jcarray;
    addToArgBuffer(val);
    FSig:= FSig + '[C';
    RefList.Add(jcarray)
  end;

  procedure TJavaParams.addShortArray(arr: array of JShort);
    var jsarray: JShortArray; val: JValue;
  begin
    jsarray:= createJShortArray(arr);
    val.Str:= jsarray;
    addToArgBuffer(val);
    FSig:= FSig + '[S';
    RefList.Add(jsarray)
  end;

  procedure TJavaParams.addIntArray(arr: array of JInt);
    var jiarray: JIntArray; val: JValue;
  begin
    jiarray:= createJIntArray(arr);
    val.Str:= jiarray;
    addToArgBuffer(val);
    FSig:= FSig + '[I';
    RefList.Add(jiarray)
  end;
    
  procedure TJavaParams.addLongArray(arr: array of Jlong);
    var jlarray: JLongArray; val: JValue;
  begin
    jlarray:= createJLongArray(arr);
    val.Str:= jlarray;
    addToArgBuffer(val);
    FSig:= FSig + '[J';
    RefList.Add(jlarray)
  end;

  procedure TJavaParams.addFloatArray(arr: array of JFloat);
    var  jfarray: JFloatArray; val: JValue;
  begin
    jfarray:= createJFloatArray(arr);
    val.Str:= jfarray;
    addToArgBuffer(val);
    FSig:= FSig + '[F';
    RefList.Add(jfarray)
  end;
        
  procedure TJavaParams.addDoubleArray(arr: array of JDouble);
    var jdarray: JDoubleArray; val: JValue;
  begin
    jdarray:= createJDoubleArray(arr);
    val.Str:= jdarray;
    addToArgBuffer(val);
    FSig:= FSig + '[D';
    RefList.Add(jdarray)
  end;
    
  procedure TJavaParams.addStringArray(strings: TStrings);
    var jsarray: JArray; val: JValue;
  begin
    jsarray:= createJStringArray(strings);
    val.Str:= jsarray;
    addToArgBuffer(val);
    FSig:= FSig + '[Ljava/lang/String;';
    RefList.Add(jsarray)
  end;

  procedure TJavaParams.MakeObjectArray(Strings: TStrings; aJavaClass: TJavaClass; const Typ: string);
    var ObjectArr: TJavaObjectArray; var Str: string;
  begin
     ObjectArr:= CreateObjectArray(Strings, aJavaClass, Typ, FValid, Str);
     addObjectArray(ObjectArr, aJavaClass);
  end;

  procedure TJavaParams.addToArgBuffer(val: JValue);
    var Int: Integer;
  begin
    Int:= Length(FArgPointer);
    SetLength(FArgPointer, Int + 1);
    FArgPointer[Int]:= val;
  end;

  constructor TJavaParams.CreateMakeParams(const Signature, Params: string);
    var Int: Integer;
        aJavaValue: TJavaValue;
        aJavaObject: TJavaObject;
        aJavaClass:  TJavaClass;
        SLT, SLSig, SLPar: TStrings;
        Typ: string;
  begin
    RefList:= TList.Create;
    FValid:= True;
    SLSig:= Split('|', Signature);
    SLPar:= Split('|', Params);

    for Int:= 0 to SLSig.Count - 1 do begin
      if SLSig[Int] = '' then Continue;
      aJavaValue:= TJavaValue.Create(SLPar[Int], SLSig[Int]);
      FValid:= aJavaValue.FValid;
      if not FValid then begin
        Error:= aJavaValue.Error;
        FreeAndNil(aJavaValue);
        Break;
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
              FValid:= False; Break;
             end
            else begin
              Typ:= SigToTyp(aJavaValue.GetSig);
              aJavaClass:= myComJava2.GetClass(Typ);
              if (aJavaClass = nil) and (aJavaObject <> nil) then begin
                FError:= myComJava2.LNGUnknownClass + ' "' + Typ + '"';
                FValid:= False; Break;
              end;
              if Assigned(aJavaObject) and not aJavaObject.ClassRef.extends(aJavaClass) then begin
                FError:= format(myComJava2.LNGIncompatibelTypes, [aJavaObject.ClassRef.Name, Typ]);
                FValid:= False;
                Break;
              end else
                addObjectSingle(aJavaObject, aJavaClass, aJavaValue);
            end;
          end;
        ntObjectArray: begin
            Typ:= SigToTyp(aJavaValue.GetSig);
            aJavaClass:= myComJava2.GetClass(Typ);
            if aJavaClass = nil then begin
              FError:= myComJava2.LNGUnknownClass + ' "' + Typ + '"';
              FValid:= False;
              end
            else begin
              SLT:= aJavaValue.GetAsTStrings;
              if Assigned(SLT) then begin
                MakeObjectArray(SLT, aJavaClass, Typ);
                SLT.Free;
              end else begin
                FError:= 'Invalid object array';
                FValid:= False;
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
    inherited Create;
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
      FError:= 'Method ' + string(uName) + ' with signature ' + string(uSig) + ' not found.';
  end;

  constructor TJavaMethod.CreateVoid(cls: TJavaClass; const Name: string);
  begin
    Create(cls, Name, nonstatic, ntVoid, Nil, '');
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
         ntBool:   Result.z:= PEnv^.CallStaticBooleanMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntByte:   Result.b:= PEnv^.CallStaticByteMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntChar:   Result.c:= PEnv^.CallStaticCharMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntShort:  Result.h:= PEnv^.CallStaticShortMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntInt:    Result.Int:= PEnv^.CallStaticIntMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntLong:   Result.l:= PEnv^.CallStaticLongMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntFloat:  Result.f:= PEnv^.CallStaticFloatMethodA(PEnv, FClass.Handle, FmethodID, args);
         ntDouble: Result.d:= PEnv^.CallStaticDoubleMethodA(PEnv, FClass.Handle, FmethodID, args);
         else      Result.Str:= PEnv^.CallStaticObjectMethodA(PEnv, FClass.Handle, FmethodID, args);
      end;

   if FMethodType = nonvirtual then
      case FRetVal of
         ntVoid:   PEnv^.CallNonvirtualVoidMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntBool:   Result.z:= PEnv^.CallNonVirtualBooleanMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntByte:   Result.b:= PEnv^.CallNonVirtualByteMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntChar:   Result.c:= PEnv^.CallNonVirtualCharMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntShort:  Result.h:= PEnv^.CallNonVirtualShortMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntInt:    Result.Int:= PEnv^.CallNonVirtualIntMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntLong:   Result.l:= PEnv^.CallNonVirtualLongMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntFloat:  Result.f:= PEnv^.CallNonVirtualFloatMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         ntDouble: Result.d:= PEnv^.CallNonVirtualDoubleMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
         else      Result.Str:= PEnv^.CallNonVirtualObjectMethodA(PEnv, obj, FClass.Handle, FmethodID, args);
      end;
   
   if FMethodType = nonstatic then
      case FRetVal of
         ntVoid:   PEnv^.CallVoidMethodA(PEnv, obj, FmethodID, args);
         ntBool:   Result.z:= PEnv^.CallBooleanMethodA(PEnv, obj, FmethodID, args);
         ntByte:   Result.b:= PEnv^.CallByteMethodA(PEnv, obj, FmethodID, args);
         ntChar:   Result.c:= PEnv^.CallCharMethodA(PEnv, obj, FmethodID, args);
         ntShort:  Result.h:= PEnv^.CallShortMethodA(PEnv, obj, FmethodID, args);
         ntInt:    Result.Int:= PEnv^.CallIntMethodA(PEnv, obj, FmethodID, args);
         ntLong:   Result.l:= PEnv^.CallLongMethodA(PEnv, obj, FmethodID, args);
         ntFloat:  Result.f:= PEnv^.CallFloatMethodA(PEnv, obj, FmethodID, args);
         ntDouble: Result.d:= PEnv^.CallDoubleMethodA(PEnv, obj, FmethodID, args);
         else      Result.Str:= PEnv^.CallObjectMethodA(PEnv, obj, FmethodID, args);
      end;
    if PEnv^.ExceptionCheck(PEnv) then begin
      PEnv^.ExceptionDescribe(PEnv);
      FError:= myComJava2.ReadConsole;
    end;
  end;

  function TJavaMethod.IsValid: Boolean;
  begin
    Result:= Assigned(FMethodID);
  end;

{ --- TJavaAttribute ------------------------------------------------------------- }

  constructor TJavaAttribute.Create(cls: TJavaClass);
  begin
    vis     := viPackage;
    Static_ := False;
    Final_  := False;
    isObject:= False;
    Generic := '';
    Sig     := '';
    Name    := '';
    FClass  := cls;
    if Assigned(cls)
      then FPEnv:= cls.FPEnv
      else FPEnv:= nil;
  end;

  function TJavaAttribute.ToString: string;
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
        'I': value.Int:= FPEnv^.GetStaticIntField(FPEnv, FClass.Handle, FAttributeID);
        'J': value.L:= FPEnv^.GetStaticLongField(FPEnv, FClass.Handle, FAttributeID);
        'F': value.F:= FPEnv^.GetStaticFloatField(FPEnv, FClass.Handle, FAttributeID);
        'D': value.D:= FPEnv^.GetStaticDoubleField(FPEnv, FClass.Handle, FAttributeID);
        'L',
        '[': value.Str:= FPEnv^.GetStaticObjectField(FPEnv, FClass.Handle, FAttributeID);
      end;
      Result:= TJavaValue.Create(value, SigToNumType(Sig), Sig);
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
        'I': value.Int:= FPEnv^.GetIntField(FPEnv, JObj.Handle, FAttributeID);
        'J': value.L:= FPEnv^.GetLongField(FPEnv, JObj.Handle, FAttributeID);
        'F': value.F:= FPEnv^.GetFloatField(FPEnv, JObj.Handle, FAttributeID);
        'D': value.D:= FPEnv^.GetDoubleField(FPEnv, JObj.Handle, FAttributeID);
        'L',
        '[': value.Str:= FPEnv^.GetObjectField(FPEnv, JObj.Handle, FAttributeID);
      end;
      Result:= TJavaValue.Create(value, SigToNumType(Sig), Sig);
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
      'I': FPEnv^.SetStaticIntField(FPEnv, FClass.Handle, FAttributeID, Value.Int);
      'J': FPEnv^.SetStaticLongField(FPEnv, FClass.Handle, FAttributeID, Value.l);
      'F': FPEnv^.SetStaticFloatField(FPEnv, FClass.Handle, FAttributeID, Value.f);
      'D': FPEnv^.SetStaticDoubleField(FPEnv, FClass.Handle, FAttributeID, Value.d);
      else FPEnv^.SetStaticObjectField(FPEnv, FClass.Handle, FAttributeID, Value.Str);
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
      'I': FPEnv^.SetIntField(FPEnv, JObj.Handle, FAttributeID, Value.Int);
      'J': FPEnv^.SetLongField(FPEnv, JObj.Handle, FAttributeID, Value.l);
      'F': FPEnv^.SetFloatField(FPEnv, JObj.Handle, FAttributeID, Value.f);
      'D': FPEnv^.SetDoubleField(FPEnv, JObj.Handle, FAttributeID, Value.d);
      else FPEnv^.SetObjectField(FPEnv, JObj.Handle, FAttributeID, Value.Str);
    end;
  end;

{--- TJavaValue ------------------------------------------}

constructor TJavaValue.Create(aValue: JValue; Kind: TNumType; const Sig: string);
begin
  FPEnv:= JNIPointer;
  FValue:= aValue;
  FKind:= Kind;
  FString:= '_x_';
  FSig:= Sig;
  FValid:= True;
end;

constructor TJavaValue.Create(const Value, Sig: string);
begin
  FPEnv:= JNIPointer;
  FKind:= SigToNumType(Sig);
  FString:= '_x_';
  FSig:= Sig;
  FValid:= SetFromString(Value);
end;

constructor TJavaValue.Create(Typ: string);
begin
  // ntBoolArray ntIntArray
  FPEnv:= JNIPointer;
  FString:= '_x_';
  FSig:= TypToSig(Typ);
  Delete(Typ, 1, LastDelimiter('.', Typ));
  FillChar(FValue, SizeOf(FValue), #0);
  FKind:= TypToNumType(Typ);
  FValid:= True;
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
  Result:= StringToJBooleanArray(FString);
end;

function TJavaValue.GetAsByteArray: TJByteArray;
begin
  Result:= StringToJByteArray(FString);
end;

function TJavaValue.GetAsCharArray: TJCharArray;
begin
  Result:= StringToJCharArray(FString);
end;

function TJavaValue.GetAsShortArray: TJShortArray;
begin
  Result:= StringToJShortArray(FString);
end;

function TJavaValue.GetAsIntArray: TJIntArray;
begin
  Result:= StringToJIntArray(FString);
end;

function TJavaValue.GetAsLongArray: TJLongArray;
begin
  Result:= StringToJLongArray(FString);
end;

function TJavaValue.GetAsFloatArray: TJFloatArray;
begin
  Result:= StringToJFloatArray(FString);
end;

function TJavaValue.GetAsDoubleArray: TJDoubleArray;
begin
  Result:= StringToJDoubleArray(FString);
end;

function TJavaValue.GetAsTStrings: TStrings;
begin
  Result:= StringToTStrings(FString);
end;

procedure TJavaValue.SetBoolArrayFromString(const Str: string);
  var arr: TJBooleanArray;
begin
  FString:= Str;
  arr:= GetAsBoolArray;
  if Assigned(arr)
    then FValue.Str:= createJBooleanArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetByteArrayFromString(const Str: string);
  var arr: TJByteArray;
begin
  FString:= Str;
  arr:= GetAsByteArray;
  if Assigned(arr)
    then FValue.Str:= createJByteArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetCharArrayFromString(const Str: string);
  var arr: TJCharArray;
begin
  FString:= Str;
  arr:= GetAsCharArray;
  if Assigned(arr)
    then FValue.Str:= createJCharArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetShortArrayFromString(const Str: string);
  var arr: TJShortArray;
begin
  FString:= Str;
  arr:= GetAsShortArray;
  if Assigned(arr)
    then FValue.Str:= createJShortArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetIntArrayFromString(const Str: string);
  var arr: TJIntArray;
begin
  FString:= Str;
  arr:= GetAsIntArray;
  if Assigned(arr)
    then FValue.Str:= createJIntArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetLongArrayFromString(const Str: string);
  var arr: TJLongArray;
begin
  FString:= Str;
  arr:= GetAsLongArray;
  if Assigned(arr)
    then FValue.Str:= createJLongArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetFloatArrayFromString(const Str: string);
  var arr: TJFloatArray;
begin
  FString:= Str;
  arr:= GetAsFloatArray;
  if Assigned(arr)
    then FValue.Str:= createJFloatArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetDoubleArrayFromString(const Str: string);
  var arr: TJDoubleArray;
begin
  FString:= Str;
  arr:= GetAsDoubleArray;
  if Assigned(arr)
    then FValue.Str:= createJDoubleArray(arr)
    else FValue.Str:= nil;
  SetLength(arr, 0);
end;

procedure TJavaValue.SetStringArrayFromString(const Str: string);
  var arr: TStrings;
begin
  FString:= Str;
  arr:= GetAsTStrings;
  if Assigned(arr) then begin
    FValue.Str:= createJStringArray(arr);
    arr.Free;
    end
  else FValue.Str:= nil;
end;

procedure TJavaValue.SetObjectArrayFromString(const Str: string);
  var arr: TStrings; Typ: string; aJavaClass: TJavaClass;
      ObjectArr: TJavaObjectArray; aValid: Boolean;
begin
  Typ:= SigToTyp(FSig);
  aJavaClass:= myComJava2.GetClass(Typ);
  arr:= StringToTStrings(Str);
  if Assigned(arr) then begin
    ObjectArr:= CreateObjectArray(arr, aJavaClass, Typ, aValid, FString);
    FValue.Str:= CreateJObjectArray(ObjectArr, aJavaClass.Handle);
    arr.Free;
    end
  else
    FValue.Str:= nil;
end;

function TJavaValue.StringToJBooleanArray(Str: string): TJBooleanArray;
  var arr: TJBooleanArray; Posi, n: Integer; b: Boolean; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    if not SysUtils.TryStrToBool(Str1, b) then b:= False;
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= b;
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToJByteArray(Str: string): TJByteArray;
  var arr: TJByteArray; Posi, n, Int: Integer; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    if not (TryStrToInt(Str1, Int) and (-128 <= Int) and (Int <= 127)) then Int:= 0;
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= Int;
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToJCharArray(Str: string): TJCharArray;
  var arr: TJCharArray; Posi, n: Integer; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1)) + ' ';
    Delete(Str, 1, Posi);
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= Ord(Str1[1]);
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToJShortArray(Str: string): TJShortArray;
  var arr: TJShortArray; Posi, n, Int: Integer; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    if not (TryStrToInt(Str1, Int) and (-32768 <= Int) and (Int <= 32767)) then Int:= 0;
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= Int;
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToJIntArray(Str: string): TJIntArray;
  var arr: TJIntArray; Posi, n, Int: Integer; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    if not TryStrToInt(Str1, Int) then Int:= 0;
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= Int;
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToJLongArray(Str: string): TJLongArray;
  var arr: TJLongArray; Posi, n: Integer; Int: int64; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    if not TryStrToInt64(Str1, Int) then Int:= 0;
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= Int;
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToJFloatArray(Str: string): TJFloatArray;
  var arr: TJFloatArray; Posi, n: Integer; f: double; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    if not TryStrToFloat(Str1, f) then f:= 0.0;
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= f;
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToJDoubleArray(Str: string): TJDoubleArray;
  var arr: TJDoubleArray; Posi, n: Integer; f: double; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  n:= 0;
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    if not TryStrToFloat(Str1, f) then f:= 0.0;
    Inc(n);
    SetLength(arr, n);
    arr[n-1]:= f;
    Posi:= Pos(',', Str);
  end;
  Result:= arr;
end;

function TJavaValue.StringToTStrings(Str: string): TStrings;
  var Posi: Integer; Str1: string;
begin
  Result:= nil;
  if Pos('{', Str) = 0 then Exit;
  Result:= TStringList.Create;
  Str:= ReplaceText(Str, '"', '');
  Str:= ReplaceText(ReplaceText(Str, '{', ''), '}', '') + ',';
  Posi:= Pos(',', Str);
  while Posi > 0 do begin
    Str1:= Trim(Copy(Str, 1, Posi-1));
    Delete(Str, 1, Posi);
    Result.Add(ResolveString(Str1));
    Posi:= Pos(',', Str);
  end;
end;

function TryStrToInt64(Str: string; var l: int64): Boolean;
  var Int, sign: Integer; c: Char;
begin
  Result:= False;
  Str:= Trim(Str);
  l:= 0;
  if Length(Str) > 0 then begin
    c:= Str[1];
    sign:= +1;
    if CharInSet(c, ['+', '-']) then begin
      Delete(Str, 1, 1);
      if c = '-' then sign:= -1;
    end;
    if (sign = -1) and (Length(Str) = 19 ) and (Str > '9223372036854775808') then Exit;
    if (sign = +1) and (Length(Str) = 19) and (Str > '9223372036854775807') then Exit;
    if Length(Str) > 19 then Exit;
    
    for Int:= 1 to Length(Str) do begin
      c:= Str[Int];
      if ('0' <= c) and (c <= '9')
        then l:= l * 10 + Ord(c) - Ord('0')
        else Exit;
    end;
    l:= sign * l;
    Result:= True;
  end;
end;

function TJavaValue.AsFormattedString: string;
  var Str: string;
begin
  FString:= '_x_';
  Str:= GetAsString;
  Result:= Str;
end;

function TJavaValue.GetAsString: string;
  var SL: TStringList;
      aJavaObject: TJavaObject;
      Int, anz: Integer; zahl: int64; Str: string;
      b: Boolean; ch: Char; by: byte; sh: short; dou: double;

  function IntToOkt(Int: Integer): string;
    var Str: string; r: Integer;
  begin
    while Int > 0 do begin
      r:= Int mod 8;
      Str:= IntToStr(r) + Str;
      Int:= Int div 8;
    end;
    Result:= Str;
  end;

  function createNewObject(obj: JObject): string;
    var aJavaClass: TJavaClass; aClass: jClass; aJavaObject: TJavaObject;
        Str: string;
  begin
    aClass:= FPEnv^.GetObjectClass(FPEnv, obj);
    aJavaClass:= myComJava2.ClassToJavaClass(aClass);
    if Assigned(aJavaClass) then begin
      aJavaObject:= TJavaObject.CreateWithHandle(aJavaClass, obj);
      aJavaObject.Global:= True;
      Str:= myComJava2.getNewObjectName(WithoutArray(GetShortType(aJavaClass.Name)));
      if myComJava2.ObjectList.IndexOf(Str) = -1 then begin
        myComJava2.ObjectList.AddObject(Str, aJavaObject);
        aJavaObject.AddToProperties(Str);
      end;
      Result:= Str;
    end else
      Result:= '';
  end;

  function getArrayAsString(ob: JObject): string;
    var aJavaClass: TJavaClass;
        jc, arr, obj: JObject;
        SLT: TStrings; SL: TStringList;
        Int, j: Integer;
        sig: Char;
        addr: NativeInt;
  begin
    if Assigned(ob) then begin
      Result:= '{';
      anz:= FPEnv^.GetArrayLength(FPEnv, ob);
      jc:= FPEnv^.GetObjectClass(FPEnv, ob);
      aJavaClass:= TJavaClass.Create(jc);
      try
        sig:= aJavaClass.FSig[2];
        if sig = '[' then begin  // array of array
          for Int:= 0 to anz-1 do begin
            arr:= FPEnv^.GetObjectArrayElement(FPEnv, ob, Int);
            Result:= Result + getArrayAsString(arr) + ', ';
            FPEnv^.DeleteLocalRef(FPENv, arr);
          end;
          Result:= Copy(Result, 1, Length(Result)-2) + '}';
        end else if Pos(sig, 'ZCBSIJFD') > 0 then begin // primitive
          arr:= FPEnv^.GetPrimitiveArrayCritical(FPEnv, ob, nil);
          addr:= NativeInt(arr);
          for Int:= 0 to anz - 1 do
            case sig of
              'Z': begin
                  b:= Boolean(Pointer(addr)^);
                  Result:= Result + BoolToStr(b, True) + ', ';
                  Inc(addr, sizeof(jBoolean));
                end;
              'B': begin
                  by:= Byte(Pointer(addr)^);
                  Result:= Result + IntToStr(by) + ', ';
                  Inc(addr, sizeof(Byte));
                end;
              'C': begin
                  ch:= Char(Pointer(addr)^);
                  if ch = #0
                    then Result:= Result + '\0, '
                    else Result:= Result + ch + ', ';
                  Inc(addr, sizeof(JChar));
                end;
              'S': begin
                  sh:= Short(Pointer(addr)^);
                  Result:= Result + IntToStr(sh) + ', ';
                  Inc(addr, sizeof(Short));
                end;
              'I': begin
                  zahl:= Int32(Pointer(addr)^);
                  Result:= Result + IntToStr(zahl) + ', ';
                  Inc(addr, sizeof(Int32));
                end;
              'J': begin
                  zahl:= Int64(Pointer(addr)^);
                  Result:= Result + IntToStr(zahl) + ', ';
                  Inc(addr, sizeof(Int64));
                end;
              'F': begin
                  dou:= Single(Pointer(addr)^);
                  Result:= Result + ReplaceText(FloatToStr(dou), ',', '.') + ', ';
                  Inc(addr, sizeof(Single));
                end;
              'D': begin
                  dou:= Double(Pointer(addr)^);
                  Result:= Result + ReplaceText(FloatToStr(dou), ',', '.') + ', ';
                  Inc(addr, sizeof(Double));
                end;
            end;
          Result:= Copy(Result, 1, Length(Result)-2) + '}';
          FPEnv^.ReleasePrimitiveArrayCritical(FPEnv, ob, arr, 0);
        end else if aJavaClass.FSig = '[Ljava/lang/String;' then begin
          SLT:= JToTStrings(ob);
          for Int:= 0 to SLT.Count - 1 do
            if SLT.Strings[Int] = 'null'
              then Result:= Result + 'null, '
              else Result:= Result + '"' + SLT.Strings[Int] + '", ';
          Result:= Copy(Result, 1, Length(Result)-2) + '}';
          SLT.Free;
        end else begin
          SL:= myComJava2.ObjectList;
          for Int:= 0 to anz - 1 do begin
            Str:= '';
            obj:= FPEnv^.GetObjectArrayElement(FPEnv, ob, Int);
            if obj = nil then
              Str:= ', null'
            else begin
              try
                for j:= 0 to SL.Count - 1 do begin
                  aJavaObject:= SL.Objects[j] as TJavaObject;
                  if FPEnv^.IsSameObject(FPEnv, obj, aJavaObject.Handle) then begin
                    Str:= ', ' + SL.Strings[j];
                    Break;
                  end;
                end;
              except
                Str:= aJavaObject.ToString;
                for j:= 0 to SL.Count - 1 do begin
                  aJavaObject:= SL.Objects[j] as TJavaObject;
                  if aJavaObject.ToString = Str then begin
                    Str:= ', ' + SL.Strings[j];
                    Break;
                  end;
                end;
                Str:= ', ' + aJavaObject.ToString;
              end;
              if Str = '' then
                Str:= ', ' + createNewObject(obj);
            end;
            Result:= Result + Str;
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
      ntBool : FString:= LowerCase(BoolToStr(Value.z, True));
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
      ntInt  : FString:= IntToStr(Value.Int);
      ntLong : FString:= IntToStr(Value.l);
      ntFloat: FString:= ReplaceText(FloatToStr(Value.f), ',', '.');
      ntDouble: FString:= ReplaceText(FloatToStr(Value.d), ',', '.');
      ntString: if Value.Str = nil // default value of a single string is "", not null
                  then begin Result:= ''; Exit; end
                  else FString:= '"' + JToDString(Value.Str) + '"';
      ntObject: begin
          if Value.Str = nil
            then Str:= 'null'
          else begin
            Str:= '';
            SL:= myComJava2.ObjectList;
            for Int:= 0 to SL.Count - 1 do begin
              aJavaObject:= SL.Objects[Int] as TJavaObject;
              if FPEnv^.IsSameObject(FPEnv, Value.Str, aJavaObject.Handle) then begin
                Str:= SL.Strings[Int];
                Break;
              end;
            end;
            if Str = '' then
              Str:= createNewObject(Value.Str);
          end;
          FString:= Str;
        end;
      else
        FString:= getArrayAsString(Value.Str);
    end;
  Result:= ReplaceText(FString, #$A, '\n');
end;

function TJavaValue.SetFromString(Str: string): Boolean;
  var Int: Integer;
      aJavaObject: TJavaObject;
      aJavaClass: TJavaClass;
      Typ: string;
      l: int64; d: double; c: Char;
begin
  Result:= True;
  if (FString <> '_x_') and (FString = Str) then Exit;
  case FKind of
    ntBool : begin
                Str:= LowerCase(Str);
                if Str = 'true'  then FValue.z:= True
                else if Str = 'false' then FValue.z:= False
                else Str:= FString;
              end;
    ntChar : if (Copy(Str, 1, 1) = '\') and (Length(Str) > 1) then begin
                c:= Str[2];
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
                    if TrystrToInt(Right(Str, 2), Int) and (Int < 256)
                      then FValue.c:= Int
                      else FValue.c:= 0;
                  end;
                end
              end
              else begin
                Str:= Copy(Str, 1, 1);
                if Length(Str) = 1 then FValue.c:= Ord(Str[1]) else Str:= FString;
              end;
    ntByte : if TryStrToInt(Str, Int) and (-128 <= Int) and (Int <= 127)
                then FValue.b:= Int else Str:= FString;
    ntShort: if TryStrToInt(Str, Int) and (-32768 <= Int) and (Int <= 32767)
                then FValue.h:= Int else Str:= FString;
    ntInt  : if TryStrToInt(Str, Int) // and (-2147483648 <= i) and (i <= 2147483647)
                then FValue.Int:= Int else Str:= FString;
    ntLong : if TryStrToInt64(Str, l) then FValue.l:= l else Str:= FString;
    ntFloat: if TryStrToFloat(Str, d) then FValue.f:= d else Str:= FString;
    ntDouble: if TryStrToFloat(Str, d) then FValue.d:= d else Str:= FString;
    ntString: if Str = 'null'
                then FValue.Str:= nil
                else FValue.Str:= createJString(ResolveString(Str));
    ntObject:
      if Str = 'null' then
        FValue.Str:= nil
      else begin
        aJavaObject:= myComJava2.GetObject(Str);
        if aJavaObject = nil then begin
          Error:= myComJava2.LNGUnknownObject + ' "' + Str + '"';
          Result:= False;
         end
        else begin
          Typ:= SigToTyp(FSig);
          aJavaClass:= myComJava2.getClass(Typ);
          if aJavaClass = nil then begin  // due to generic problem
            aJavaClass:= myComJava2.getClass('java.lang.Object');
            if Assigned(aJavaClass) then
              FSig:= 'Ljava/lang/Object;';
          end;
          
          if aJavaClass = nil then begin
            Error:= myComJava2.LNGUnknownClass + ' "' + Typ + '"';
            Result:= False;
            end
          else
            if aJavaObject.ClassRef.extends(aJavaClass)
              then FValue.Str:= aJavaObject.getHandle
              else begin
                Error:= format(myComJava2.LNGIncompatibelTypes, [aJavaObject.ClassRef.Name, SigToTyp(FSig)]);
                Result:= False;
              end;
        end;
      end;
    ntBoolArray..ntStringArray: begin
      if Assigned(FValue.Str) then
        FPEnv^.DeleteLocalRef(FPEnv, FValue.Str);
      case FKind of
        ntBoolArray:   SetBoolArrayFromString(Str);
        ntByteArray:   SetByteArrayFromString(Str);
        ntCharArray:   SetCharArrayFromString(Str);
        ntShortArray:  SetShortArrayFromString(Str);
        ntIntArray:    SetIntArrayFromString(Str);
        ntLongArray:   SetLongArrayFromString(Str);
        ntFloatArray:  SetFloatArrayFromString(Str);
        ntDoubleArray: SetDoubleArrayFromString(Str);
        ntStringArray: SetStringArrayFromString(Str);
      end;
    end;
    ntObjectArray: SetObjectArrayFromString(Str);
  end;
  FString:= Str;
end;

function TJavaValue.ResolveString(Str: string): string;
  var aJavaObject: TJavaObject;
begin
  aJavaObject:= myComJava2.getObject(Str);
  if Assigned(aJavaObject) and (aJavaObject.FClass.getTyp = 'String') then
    Str:= aJavaObject.ToString;
  if IsJavaString(Str)
    then Result:= Copy(Str, 2, Length(Str)-2)
    else Result:= Str;
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
  CompilerClass.setGlobal(True);
  Compiler:= TJavaObject.CreateWithHandle(CompilerClass, CompilerObject);
  Compiler.setGlobal(True);
  Params:= TJavaParams.Create;
  SL:= TStringList.Create;
end;

function TJavaCompiler.Compile(const Classpath, Parameter, Pathname: string): string;
  var Int, Posi: Integer; PEnv: PJNIEnv;
      Input, Output: TJavaClass;
      l, m, Str, Str1, Str2, Sourcepath: string;
begin
  PEnv:= JNIPointer;
  SL.Clear;
  Str:= ExtractFilePath(Pathname);
  if Pos(Str, Classpath) = 0
    then Sourcepath:= Str + ';' + Classpath
    else Sourcepath:= Classpath;

  Str1:= '-classpath ' + HideBlanks(Classpath) + ' ' + Parameter;
  Str1:= ReplaceText(Str1, ' ', #13#10);
  Str2:= '';
  Posi:= Pos('"', Str1);
  while Posi > 0 do begin
    l:= Left(Str1, Posi-1);
    Delete(Str1, 1, Posi);
    Posi:= Pos('"', Str1);
    if Posi = 0 then begin
      Str1:= Str1 + '"';
      Posi:= Length(Str1);
    end;
    m:= Left(Str1, Posi-1);
    Delete(Str1, 1, Posi);
    Str2:= Str2 + l + ReplaceText(m, #13#10, ' ');
    Posi:= Pos('"', Str1);
  end;
  Str2:= Str2 + Str1;

  SL.Text:= Str2;
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
  Int:= PEnv^.CallIntMethodA(PEnv, Compiler.Handle, MethodID, Args);
  if Int <> 0 then begin
    if PEnv^.ExceptionCheck(PEnv) then
      PEnv^.ExceptionDescribe(PEnv);
    Result:= '-ERR ' + myComJava2.ReadConsole;
  end
  else
    Result:= '+OK  ' + myComJava2.ReadConsole;   // -verbose for example
end;

{ --- Sonstiges -------------------------------------------------------------- }

  function createJString (const Str: string): jstring;
    var PEnv: PJNIEnv;
        su: UTF8String;
  begin
    su:= UTF8String(Str);
    PEnv:= JNIPointer;
    Result:= PEnv^.NewStringUTF(PEnv, PAnsiChar(su));
  end;

  function JToDString(js: JString): string;
  var
   PEnv: PJNIEnv;
   len: Integer;
   CharBuf: PAnsiChar;
   IsCopy: JBoolean;
   Str: UTF8String;
  begin
    PEnv:= JNIPointer;
    CharBuf:= PEnv^.GetStringUTFChars(PEnv, js, IsCopy);
    len:= PEnv^.GetStringUTFLength(PEnv, js);
    SetLength(Str, len);
    if len > 0 then AnsiStrings.StrLCopy(PAnsiChar(Str), Charbuf, len);
    if IsCopy then 
      PEnv^.ReleaseStringUTFChars(PEnv, js, CharBuf);
    Result:= UTF8ToString(Str);
  end;

  function JToTStrings(jarr: JObjectarray): TStrings;
  var
    PEnv: PJNIEnv;
    jobj: jobject;
    len, Int: Integer;
  begin
    PEnv:= JNIPointer;
    Result:= TStringList.Create;
    len:= PEnv^.GetArrayLength(PEnv, jarr);
    for Int:= 1 to len do begin
      jobj:= PEnv^.GetObjectArrayElement(PEnv, jarr, Int-1);
      if jobj = nil
        then Result.Add('null')
        else Result.Add(JToDString(jobj));
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
    Result:= sc;
  end;

  function createJStringArray(myStrings: TStrings): jStringArray;
  var
    Int, Count: Integer;
    js: jstring;
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Count:= 0;
    if myStrings <> nil then
      Count:= myStrings.Count;
    js:= createJString('');
    Result:= PEnv^.NewObjectArray(PEnv, Count, getStringClass, js);
    for Int:= 0 to Count-1 do begin
      js:= createJString(myStrings.Strings[Int]);
      PEnv^.SetObjectArrayElement(PEnv, Result, Int, js);
    end;
  end;

  function CreateObjectArray(Strings: TStrings; aJavaClass: TJavaClass; const Typ: string;
              var Valid: Boolean; var AsString: string): TJavaObjectArray;
    var ObjectArr: TJavaObjectArray; Int, n: Integer;
        aJavaObject: TJavaObject; Str: string;
  begin
    n:= 0;
    AsString:= '';
    for Int:= 0 to Strings.Count - 1 do begin
      Str:= '';
      if (Strings[Int] = 'null') or (UpperCase(Strings[Int]) = 'POINTER') then begin
        aJavaObject:= nil;
        Str:= ', null';
      end
      else begin
        aJavaObject:= myComJava2.GetObject(Strings[Int]);
        if aJavaObject = nil then begin
          ErrorMsg(myComJava2.LNGUnknownObject + ' "' + Strings[Int] + '"');
          Valid:= False; Break;
        end;
        if not aJavaObject.ClassRef.extends(aJavaClass) then begin
          ErrorMsg(format(myComJava2.LNGIncompatibelTypes,
            [aJavaObject.ClassRef.Name, Typ]));
          Valid:= False; Break;
        end;
        Str:= ', ' + Strings[Int];
      end;
      Inc(n);
      SetLength(ObjectArr, n);
      ObjectArr[n-1]:= aJavaObject;
      AsString:= AsString + Str;
    end;
    AsString:= '{' + Right(AsString, 3) + '}';
    Result:= ObjectArr;
  end;

  function createJObjectArray(ObjectArr: TJavaObjectArray; aClass: jclass): jObjectArray;
    var Int, Count: Integer; PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Count:= High(ObjectArr);
    Result:= PEnv^.NewObjectArray(PEnv, Count + 1, aClass, nil);
    for Int:= 0 to Count do
      if Assigned(ObjectArr[Int]) then
        PEnv^.SetObjectArrayElement(PEnv, Result, Int, ObjectArr[Int].handle);
  end;

  function createJBooleanArray ( arr: array of JBoolean): JBooleanArray;
    var PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newBooleanArray(PEnv, High(arr) + 1);
    PEnv^.setBooleanArrayRegion(PEnv, Result, Low(arr), High(arr) +1, @arr);
  end;

  function createJByteArray ( arr: array of JByte): JByteArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newByteArray(PEnv, High(arr) + 1);
    PEnv^.setByteArrayRegion(PEnv, Result, 0, High(arr) +1, @arr);
  end;
    
  function createJCharArray ( arr: array of JChar): JCharArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newCharArray(PEnv, High(arr) + 1);
    PEnv^.setCharArrayRegion(PEnv, Result, Low(arr), High(arr) +1, @arr);
  end;

  function createJShortArray ( arr: array of JShort): JShortArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newShortArray(PEnv, High(arr) + 1);
    PEnv^.setShortArrayRegion(PEnv, Result, 0, High(arr) +1, @arr);
  end;
    
  function createJIntArray ( arr: array of Jint): JIntArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newIntArray(PEnv, High(arr) + 1);
    PEnv^.setIntArrayRegion(PEnv, Result, Low(arr), High(arr) +1, @arr);
  end;
  
  function createJLongArray ( arr: array of JLong): JLongArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newLongArray(PEnv, High(arr) + 1);
    PEnv^.setLongArrayRegion(PEnv, Result, Low(arr), High(arr) +1, @arr);
  end;

  function createJFloatArray ( arr: array of JFloat): JFloatArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newFloatArray(PEnv, High(arr) + 1);
    PEnv^.setFloatArrayRegion(PEnv, Result, Low(arr), High(arr) + 1, @arr);
  end;

  function createJDoubleArray (arr: array of JDouble): JDoubleArray;
  var
    PEnv: PJNIEnv;
  begin
    PEnv:= JNIPointer;
    Result:= PEnv^.newDoubleArray(PEnv, High(arr) + 1);
    PEnv^.setDoubleArrayRegion(PEnv, Result, 0, High(arr) +1, @arr);
  end;

  function TypToSig(Typ: string): string;
    var dimension: string; Posi: Integer;
  begin
    Result:= '';
    dimension:= '';
    Posi:= Pos('[]', Typ);
    while Posi > 0 do begin
      dimension:= dimension + '[';
      Delete(Typ, Posi, 2);
      Posi:= Pos('[]', Typ);
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
