{ ********************************************

  Translation of Sun's jni.h file to Borland
  Object Pascal. Most people will prefer to use the
  object wrappers in JNIWrapper.Pas

  Java and Delphi Freelance programming
  jon@revusky.com

  ******************************************** }

unit jni;

interface

{ jboolean constants }
const
  JNI_FALSE = 0;
  JNI_TRUE = 1;

  { possible return values for JNI functions }

  JNI_OK = 0;
  JNI_ERR = -1;
  JNI_EDETACHED = -2;
  JNI_EVERSION = -3;
  JNI_ENOMEM = -4;
  JNI_EEXIST = -5;
  JNI_EINVAL = -6;

  { used in ReleaseScalarArrayElements }

  JNI_COMMIT = 1;
  JNI_ABORT = 2;

type

  jbyte = type ShortInt;
  jint = Integer;
  jlong = Int64;
  jboolean = Boolean;
  jchar = Word;
  jshort = SmallInt;
  jfloat = Single;
  jdouble = Double;
  jsize = jint;

  jobject = type Pointer;
  jclass = type jobject;
  jthrowable = type jobject;
  jstring = type jobject;
  jarray = type jobject;
  jbooleanArray = type jarray;
  jbyteArray = type jarray;
  jcharArray = type jarray;
  jshortArray = type jarray;
  jintArray = type jarray;
  jlongArray = type jarray;
  jfloatArray = type jarray;
  jdoubleArray = type jarray;
  jstringArray = type jarray;
  jobjectArray = type jarray;
  jweak = type jobject;

  JValue = packed record
    case Integer of
      0:
        (z: jboolean);
      1:
        (b: jbyte);
      2:
        (c: jchar);
      3:
        (h: jshort);
      4:
        (Int: jint);
      5:
        (l: jlong);
      6:
        (f: jfloat);
      7:
        (d: jdouble);
      8:
        (Str: jobject);
  end;

  { pointer types }

  PJNIEnv = ^JNIEnv;
  PPJNIEnv = ^PJNIEnv;
  PJBoolean = ^jboolean;
  PJByte = ^jbyte;
  PJValue = ^JValue;
  PJChar = ^jchar;
  PJShort = ^jshort;
  PJInt = ^jint;
  PJLong = ^jlong;
  PJFloat = ^jfloat;
  PJDouble = ^jdouble;
  PJObject = ^jobject;
  va_list = PAnsiChar;

  jfieldID = type Pointer;
  jmethodID = type Pointer;

  JNINativeMethod = packed record
    Name, signature: PAnsiChar;
    fnPtr: Pointer;
  end;

  PJNINativeMethod = ^JNINativeMethod;

  { JNI Native Method Interface. }

  JNIEnv = ^JNINativeInterface_;

  { JNI Invocation Interface. }

  JavaVM = ^JNIInvokeInterface_;

  PJNIInvokeInterface = ^JNIInvokeInterface_;

  JavaVM_ = packed record
    functions: PJNIInvokeInterface;
  end;

  PJavaVM = ^JavaVM;
  PPJavaVM = ^PJavaVM;

  JNINativeInterface_ = packed record
    reserved0, reserved1, reserved2, reserved3: Pointer;
    GetVersion: function(Env: PJNIEnv): jint; stdcall;
    DefineClass: function(Env: PJNIEnv; const Name: PAnsiChar; Loader: jobject;
      const Buf: PJByte; Len: jsize): jclass; stdcall;
    FindClass: function(Env: PJNIEnv; const Name: PAnsiChar): jclass; stdcall;
    FromReflectedMethod: function(Env: PJNIEnv; Method: jobject)
      : jmethodID; stdcall;
    FromReflectedField: function(Env: PJNIEnv; Field: jobject)
      : jfieldID; stdcall;
    ToReflectedMethod: function(Env: PJNIEnv; Cls: jclass; MethodID: jmethodID;
      IsStatic: jboolean): jobject; stdcall;
    GetSuperClass: function(Env: PJNIEnv; Sub { , Sup } : jclass)
      : jclass; stdcall;
    IsAssignableFrom: function(Env: PJNIEnv; Sub, Sup: jclass)
      : jboolean; stdcall;
    ToReflectedField: function(Env: PJNIEnv; Cls: jclass; FieldID: jfieldID;
      IsStatic: jboolean): jobject; stdcall;
    Throw: function(Env: PJNIEnv; Obj: jthrowable): jint; stdcall;
    ThrowNew: function(Env: PJNIEnv; Clazz: jclass; const Msg: PAnsiChar)
      : jint; stdcall;
    ExceptionOccurred: function(Env: PJNIEnv): jthrowable; stdcall;
    ExceptionDescribe, ExceptionClear: procedure(Env: PJNIEnv); stdcall;
    FatalError: procedure(Env: PJNIEnv; const Msg: PAnsiChar); stdcall;
    PushLocalFrame: function(Env: PJNIEnv; Capacity: jint): jint; stdcall;
    PopLocalFrame: function(Env: PJNIEnv; Res: jobject): jobject; stdcall;
    NewGlobalRef: function(Env: PJNIEnv; Obj: jobject): jobject; stdcall;
    DeleteGlobalRef: procedure(Env: PJNIEnv; Obj: jobject); stdcall;
    DeleteLocalRef: procedure(Env: PJNIEnv; Obj: jobject); stdcall;
    IsSameObject: function(Env: PJNIEnv; Obj1, Obj2: jobject)
      : jboolean; stdcall;
    NewLocalRef: function(Env: PJNIEnv; Ref: jobject): jobject; stdcall;
    EnsureLocalCapacity: function(Env: PJNIEnv; Capacity: jint): jint; stdcall;
    AllocObject: function(Env: PJNIEnv; Clazz: jclass): jclass; stdcall;
    NewObject: function(Env: PJNIEnv; Clazz: jclass; MethodID: jmethodID)
      : jobject; stdcall;
    NewObjectV: function(Env: PJNIEnv; Clazz: jclass; MethodID: jmethodID;
      Args: Pointer): jobject; stdcall;
    NewObjectA: function(Env: PJNIEnv; Clazz: jclass; MethodID: jmethodID;
      Args: PJValue): jobject; stdcall;
    GetObjectClass: function(Env: PJNIEnv; Obj: jobject): jclass; stdcall;
    IsInstanceof: function(Env: PJNIEnv; Obj: jobject; Clazz: jclass)
      : jboolean; stdcall;
    GetMethodID: function(Env: PJNIEnv; Clazz: jclass;
      const Name, Sig: PAnsiChar): jmethodID; stdcall;
    CallObjectMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jobject; stdcall;
    CallObjectMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jobject; stdcall;
    CallObjectMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jobject; stdcall;
    CallBooleanMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jboolean; stdcall;
    CallBooleanMethodV: function(Env: PJNIEnv; Obj: jobject;
      MethodID: jmethodID; Args: Pointer): jboolean; stdcall;
    CallBooleanMethodA: function(Env: PJNIEnv; Obj: jobject;
      MethodID: jmethodID; Args: Pointer): jboolean; stdcall;
    CallByteMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jbyte; stdcall;
    CallByteMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jbyte; stdcall;
    CallByteMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jbyte; stdcall;
    CallCharMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jchar; stdcall;
    CallCharMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jchar; stdcall;
    CallCharMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jchar; stdcall;
    CallShortMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jshort; stdcall;
    CallShortMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jshort; stdcall;
    CallShortMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jshort; stdcall;
    CallIntMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jint; stdcall;
    CallIntMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jint; stdcall;
    CallIntMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jint; stdcall;
    CallLongMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jlong; stdcall;
    CallLongMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jlong; stdcall;
    CallLongMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jlong; stdcall;
    CallFloatMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jfloat; stdcall;
    CallFloatMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jfloat; stdcall;
    CallFloatMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jfloat; stdcall;
    CallDoubleMethod: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID)
      : jdouble; stdcall;
    CallDoubleMethodV: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer): jdouble; stdcall;
    CallDoubleMethodA: function(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue): jdouble; stdcall;
    CallVoidMethod: procedure(Env: PJNIEnv; Obj: jobject;
      MethodID: jmethodID); stdcall;
    CallVoidMethodV: procedure(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: Pointer); stdcall;
    CallVoidMethodA: procedure(Env: PJNIEnv; Obj: jobject; MethodID: jmethodID;
      Args: PJValue); stdcall;
    CallNonvirtualObjectMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jobject; stdcall;
    CallNonvirtualObjectMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jobject; stdcall;
    CallNonvirtualObjectMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jobject; stdcall;
    CallNonvirtualBooleanMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jboolean; stdcall;
    CallNonvirtualBooleanMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jboolean; stdcall;
    CallNonvirtualBooleanMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jboolean; stdcall;
    CallNonvirtualByteMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jbyte; stdcall;
    CallNonvirtualByteMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jbyte; stdcall;
    CallNonvirtualByteMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jbyte; stdcall;
    CallNonvirtualCharMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jchar; stdcall;
    CallNonvirtualCharMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jchar; stdcall;
    CallNonvirtualCharMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jchar; stdcall;
    CallNonvirtualShortMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jshort; stdcall;
    CallNonvirtualShortMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jshort; stdcall;
    CallNonvirtualShortMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jshort; stdcall;
    CallNonvirtualIntMethod: function(Env: PJNIEnv; Obj: jobject; Clazz: jclass;
      MethodID: jmethodID): jint; stdcall;
    CallNonvirtualIntMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jint; stdcall;
    CallNonvirtualIntMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jint; stdcall;
    CallNonvirtualLongMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jlong; stdcall;
    CallNonvirtualLongMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jlong; stdcall;
    CallNonvirtualLongMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jlong; stdcall;
    CallNonvirtualFloatMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jfloat; stdcall;
    CallNonvirtualFloatMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jfloat; stdcall;
    CallNonvirtualFloatMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jfloat; stdcall;
    CallNonvirtualDoubleMethod: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID): jdouble; stdcall;
    CallNonvirtualDoubleMethodV: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer): jdouble; stdcall;
    CallNonvirtualDoubleMethodA: function(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue): jdouble; stdcall;
    CallNonvirtualVoidMethod: procedure(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID); stdcall;
    CallNonvirtualVoidMethodV: procedure(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: Pointer); stdcall;
    CallNonvirtualVoidMethodA: procedure(Env: PJNIEnv; Obj: jobject;
      Clazz: jclass; MethodID: jmethodID; Args: PJValue); stdcall;

    GetFieldId: function(Env: PJNIEnv; Clazz: jclass;
      const Name, Sig: PAnsiChar): jfieldID; stdcall;
    GetObjectField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jobject; stdcall;
    GetBooleanField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jboolean; stdcall;
    GetByteField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jbyte; stdcall;
    GetCharField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jchar; stdcall;
    GetShortField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jshort; stdcall;
    GetIntField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jint; stdcall;
    GetLongField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jlong; stdcall;
    GetFloatField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jfloat; stdcall;
    GetDoubleField: function(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID)
      : jdouble; stdcall;

    SetObjectField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jobject); stdcall;
    SetBooleanField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jboolean); stdcall;
    SetByteField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jbyte); stdcall;
    SetCharField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jchar); stdcall;
    SetShortField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jshort); stdcall;
    SetIntField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jint); stdcall;
    SetLongField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jlong); stdcall;
    SetFloatField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jfloat); stdcall;
    SetDoubleField: procedure(Env: PJNIEnv; Obj: jobject; FieldID: jfieldID;
      Val: jdouble); stdcall;

    GetStaticMethodID: function(Env: PJNIEnv; Clazz: jclass;
      const Name, Sig: PAnsiChar): jmethodID; stdcall;
    CallStaticObjectMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jobject; stdcall;
    CallStaticObjectMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jobject; stdcall;
    CallStaticObjectMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jobject; stdcall;
    CallStaticBooleanMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jboolean; stdcall;
    CallStaticBooleanMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jboolean; stdcall;
    CallStaticBooleanMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jboolean; stdcall;
    CallStaticByteMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jbyte; stdcall;
    CallStaticByteMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jbyte; stdcall;
    CallStaticByteMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jbyte; stdcall;
    CallStaticCharMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jchar; stdcall;
    CallStaticCharMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jchar; stdcall;
    CallStaticCharMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jchar; stdcall;
    CallStaticShortMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jshort; stdcall;
    CallStaticShortMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jshort; stdcall;
    CallStaticShortMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jshort; stdcall;
    CallStaticIntMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jint; stdcall;
    CallStaticIntMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jint; stdcall;
    CallStaticIntMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jint; stdcall;
    CallStaticLongMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jlong; stdcall;
    CallStaticLongMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jlong; stdcall;
    CallStaticLongMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jlong; stdcall;
    CallStaticFloatMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jfloat; stdcall;
    CallStaticFloatMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jfloat; stdcall;
    CallStaticFloatMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jfloat; stdcall;
    CallStaticDoubleMethod: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID): jdouble; stdcall;
    CallStaticDoubleMethodV: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer): jdouble; stdcall;
    CallStaticDoubleMethodA: function(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue): jdouble; stdcall;
    CallStaticVoidMethod: procedure(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID); stdcall;
    CallStaticVoidMethodV: procedure(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: Pointer); stdcall;
    CallStaticVoidMethodA: procedure(Env: PJNIEnv; Clazz: jclass;
      MethodID: jmethodID; Args: PJValue); stdcall;

    GetStaticFieldID: function(Env: PJNIEnv; Clazz: jclass;
      const Name, Sig: PAnsiChar): jfieldID; stdcall;
    GetStaticObjectField: function(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID): jobject; stdcall;
    GetStaticBooleanField: function(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID): jboolean; stdcall;
    GetStaticByteField: function(Env: PJNIEnv; Clazz: jclass; FieldID: jfieldID)
      : jbyte; stdcall;
    GetStaticCharField: function(Env: PJNIEnv; Clazz: jclass; FieldID: jfieldID)
      : jchar; stdcall;
    GetStaticShortField: function(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID): jshort; stdcall;
    GetStaticIntField: function(Env: PJNIEnv; Clazz: jclass; FieldID: jfieldID)
      : jint; stdcall;
    GetStaticLongField: function(Env: PJNIEnv; Clazz: jclass; FieldID: jfieldID)
      : jlong; stdcall;
    GetStaticFloatField: function(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID): jfloat; stdcall;
    GetStaticDoubleField: function(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID): jdouble; stdcall;

    SetStaticObjectField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jobject); stdcall;
    SetStaticBooleanField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jboolean); stdcall;
    SetStaticByteField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jbyte); stdcall;
    SetStaticCharField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jchar); stdcall;
    SetStaticShortField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jshort); stdcall;
    SetStaticIntField: procedure(Env: PJNIEnv; Clazz: jclass; FieldID: jfieldID;
      Value: jint); stdcall;
    SetStaticLongField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jlong); stdcall;
    SetStaticFloatField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jfloat); stdcall;
    SetStaticDoubleField: procedure(Env: PJNIEnv; Clazz: jclass;
      FieldID: jfieldID; Value: jdouble); stdcall;

    NewString: function(Env: PJNIEnv; const Unicode: PJChar; Len: jsize)
      : jstring; stdcall;
    GetStringLength: function(Env: PJNIEnv; Str: jstring): jsize; stdcall;
    GetStringChars: function(Env: PJNIEnv; Str: jstring; IsCopy: PJBoolean)
      : PJChar; stdcall;
    ReleaseStringChars: procedure(Env: PJNIEnv; Str: jstring;
      const Chars: PJChar); stdcall;
    NewStringUTF: function(Env: PJNIEnv; const Utf: PAnsiChar)
      : jstring; stdcall;
    GetStringUTFLength: function(Env: PJNIEnv; Str: jstring): jsize; stdcall;
    GetStringUTFChars: function(Env: PJNIEnv; Str: jstring;
      var IsCopy: jboolean): PAnsiChar; stdcall;
    ReleaseStringUTFChars: procedure(Env: PJNIEnv; Str: jstring;
      const Chars: PAnsiChar); stdcall;
    GetArrayLength: function(Env: PJNIEnv; Arr: jarray): jsize; stdcall;
    NewObjectArray: function(Env: PJNIEnv; Len: jsize; Clazz: jclass;
      Init: jobject): jobjectArray; stdcall;
    GetObjectArrayElement: function(Env: PJNIEnv; Arr: jobjectArray;
      Index: jsize): jobject; stdcall;
    SetObjectArrayElement: procedure(Env: PJNIEnv; Arr: jobjectArray;
      Index: jsize; Val: jobject); stdcall;
    NewBooleanArray: function(Env: PJNIEnv; Len: jsize): jbooleanArray; stdcall;
    NewByteArray: function(Env: PJNIEnv; Len: jsize): jbyteArray; stdcall;
    NewCharArray: function(Env: PJNIEnv; Len: jsize): jcharArray; stdcall;
    NewShortArray: function(Env: PJNIEnv; Len: jsize): jshortArray; stdcall;
    NewIntArray: function(Env: PJNIEnv; Len: jsize): jintArray; stdcall;
    NewLongArray: function(Env: PJNIEnv; Len: jsize): jlongArray; stdcall;
    NewFloatArray: function(Env: PJNIEnv; Len: jsize): jfloatArray; stdcall;
    NewDoubleArray: function(Env: PJNIEnv; Len: jsize): jdoubleArray; stdcall;
    GetBooleanArrayElements: function(Env: PJNIEnv; Arr: jbooleanArray;
      IsCopy: PJBoolean): PJBoolean; stdcall;
    GetByteArrayElements: function(Env: PJNIEnv; Arr: jbyteArray;
      IsCopy: PJBoolean): PJByte; stdcall;
    GetCharArrayElements: function(Env: PJNIEnv; Arr: jcharArray;
      IsCopy: PJBoolean): PJChar; stdcall;
    GetShortArrayElements: function(Env: PJNIEnv; Arr: jshortArray;
      IsCopy: PJBoolean): PJShort; stdcall;
    GetIntArrayElements: function(Env: PJNIEnv; Arr: jintArray; IsCopy: PJInt)
      : PJInt; stdcall;
    GetLongArrayElements: function(Env: PJNIEnv; Arr: jlongArray;
      IsCopy: PJLong): PJLong; stdcall;
    GetFloatArrayElements: function(Env: PJNIEnv; Arr: jfloatArray;
      IsCopy: PJBoolean): PJFloat; stdcall;
    GetDoubleArrayElements: function(Env: PJNIEnv; Arr: jdoubleArray;
      IsCopy: PJBoolean): PJDouble; stdcall;
    ReleaseBooleanArrayElements: procedure(Env: PJNIEnv; Arr: jbooleanArray;
      Elems: PJBoolean; Mode: jint); stdcall;
    ReleaseByteArrayElements: procedure(Env: PJNIEnv; Arr: jbyteArray;
      Elems: PJByte; Mode: jint); stdcall;
    ReleaseCharArrayElements: procedure(Env: PJNIEnv; Arr: jcharArray;
      Elems: PJChar; Mode: jint); stdcall;
    ReleaseShortArrayElements: procedure(Env: PJNIEnv; Arr: jshortArray;
      Elems: PJShort; Mode: jint); stdcall;
    ReleaseIntArrayElements: procedure(Env: PJNIEnv; Arr: jintArray;
      Elems: PJInt; Mode: jint); stdcall;
    ReleaseLongArrayElements: procedure(Env: PJNIEnv; Arr: jlongArray;
      Elems: PJLong; Mode: jint); stdcall;
    ReleaseFloatArrayElements: procedure(Env: PJNIEnv; Arr: jfloatArray;
      Elems: PJFloat; Mode: jint); stdcall;
    ReleaseDoubleArrayElements: procedure(Env: PJNIEnv; Arr: jdoubleArray;
      Elems: PJDouble; Mode: jint); stdcall;
    GetBooleanArrayRegion: procedure(Env: PJNIEnv; Arr: jbooleanArray;
      Start, l: jsize; Buf: PJBoolean); stdcall;
    GetByteArrayRegion: procedure(Env: PJNIEnv; Arr: jbyteArray;
      Start, l: jsize; Buf: PJByte); stdcall;
    GetCharArrayRegion: procedure(Env: PJNIEnv; Arr: jcharArray;
      Start, l: jsize; Buf: PJChar); stdcall;
    GetShortArrayRegion: procedure(Env: PJNIEnv; Arr: jshortArray;
      Start, l: jsize; Buf: PJShort); stdcall;
    GetIntArrayRegion: procedure(Env: PJNIEnv; Arr: jintArray; Start, l: jsize;
      Buf: PJInt); stdcall;
    GetLongArrayRegion: procedure(Env: PJNIEnv; Arr: jlongArray;
      Start, l: jsize; Buf: PJLong); stdcall;
    GetFloatArrayRegion: procedure(Env: PJNIEnv; Arr: jfloatArray;
      Start, l: jsize; Buf: PJFloat); stdcall;
    GetDoubleArrayRegion: procedure(Env: PJNIEnv; Arr: jdoubleArray;
      Start, l: jsize; Buf: PJDouble); stdcall;
    SetBooleanArrayRegion: procedure(Env: PJNIEnv; Arr: jbooleanArray;
      Start, Len: jsize; Buf: PJBoolean); stdcall;
    SetByteArrayRegion: procedure(Env: PJNIEnv; Arr: jbyteArray;
      Start, Len: jsize; Buf: PJByte); stdcall;
    SetCharArrayRegion: procedure(Env: PJNIEnv; Arr: jcharArray;
      Start, Len: jsize; Buf: PJChar); stdcall;
    SetShortArrayRegion: procedure(Env: PJNIEnv; Arr: jshortArray;
      Start, Len: jsize; Buf: PJShort); stdcall;
    SetIntArrayRegion: procedure(Env: PJNIEnv; Arr: jintArray;
      Start, Len: jsize; Buf: PJInt); stdcall;
    SetLongArrayRegion: procedure(Env: PJNIEnv; Arr: jlongArray;
      Start, Len: jsize; Buf: PJLong); stdcall;
    SetFloatArrayRegion: procedure(Env: PJNIEnv; Arr: jfloatArray;
      Start, Len: jsize; Buf: PJFloat); stdcall;
    SetDoubleArrayRegion: procedure(Env: PJNIEnv; Arr: jdoubleArray;
      Start, Len: jsize; Buf: PJDouble); stdcall;
    RegisterNatives: function(Env: PJNIEnv; Clazz: jclass;
      const Method: PJNINativeMethod; NMethods: jint): jint; stdcall;
    UnregisterNatives: function(Env: PJNIEnv; Clazz: jclass): jint; stdcall;
    MonitorEnter: function(Env: PJNIEnv; Obj: jobject): jint; stdcall;
    MonitorExit: function(Env: PJNIEnv; Obj: jobject): jint; stdcall;
    GetJavaVM: function(Env: PJNIEnv; vm: PPJavaVM): jint; stdcall;
    { from here, it's only supported by Java 2 }
    GetStringRegion: procedure(Env: PJNIEnv; Str: jstring; Start, Len: jsize;
      Buf: PJChar); stdcall;
    GetStringUTFRegion: procedure(Env: PJNIEnv; Str: jstring; Start, Len: jsize;
      Buf: PAnsiChar); stdcall;
    GetPrimitiveArrayCritical: function(Env: PJNIEnv; Arr: jarray;
      IsCopy: PJBoolean): Pointer; stdcall;
    ReleasePrimitiveArrayCritical: procedure(Env: PJNIEnv; Arr: jarray;
      CAarray: Pointer; Mode: jint); stdcall;
    GetStringCritical: function(Env: PJNIEnv; Str: jstring; IsCopy: PJBoolean)
      : PJChar; stdcall;
    ReleaseStringCritical: procedure(Env: PJNIEnv; Str: jstring;
      const CString: PJChar); stdcall;
    NewWeakGlobalRef: function(Env: PJNIEnv; Obj: jobject): jweak; stdcall;
    DeleteWeakGlobalRef: procedure(Env: PJNIEnv; Ref: jweak); stdcall;
    ExceptionCheck: function(Env: PJNIEnv): jboolean; stdcall;
  end;

  JNIEnv_ = packed record
    functions: ^JNINativeInterface_;
  end;

  JavaVMOption = packed record
    OptionString: PAnsiChar;
    ExtraInfo: Pointer;
  end;

  PJavaVMOption = ^JavaVMOption;

  JavaVMInitArgs = packed record
    Version: jint;
    NOptions: jint;
    options: PJavaVMOption;
    IgnoreUnrecognized: jboolean;
  end;

  JavaVMAttachArgs = packed record
    Version: jint;
    Name: PAnsiChar;
    Group: jobject;
  end;

  { The following structures will be VM-specific }

  JDK1_1InitArgs = packed record
    Version: jint;
    properties: ^PAnsiChar;
    checkSource, nativeStackSize, javaStackSize, minHeapSize, maxHeapSize,
      verifyMode: jint;
    classpath: PAnsiChar;
    vfprintf: function(FilePointer: Pointer; const Format: PAnsiChar;
      Args: va_list): jint; stdcall;
    Exit: procedure(ExitCode: jint); stdcall;
    Abort: procedure; stdcall;
    enableClassGC: jint;
    enableVerboseGC: jint;
    disableAsyncGC: jint;
    verbose: jint;
    debugging: jboolean;
    debugPort: jint;
  end;

  JDK1_1AttachArgs = packed record
    padding: Pointer;
  end;
  { end of VM-specific structures }

  JNIInvokeInterface_ = packed record
    reserved0, reserved1, reserved2: Pointer;
    DestroyJavaVM: function(vm: PJavaVM): jint; stdcall;
    AttachCurrentThread: function(vm: PJavaVM; PEnv: PPJNIEnv; Args: Pointer)
      : jint; stdcall;
    DetachCurrentThread: function(vm: PJavaVM; PEnv: PPJNIEnv; Args: Pointer)
      : jint; stdcall;
    { the following function is only in Java 2 }
    GetEnv: function(vm: PJavaVM; PEnv: PPJNIEnv; Version: jint): jint; stdcall;
  end;

  TNumType = (ntBool, ntByte, ntChar, ntShort, ntInt, ntLong, ntFloat, ntDouble,
    ntString, ntObject,

    ntBoolArray, ntByteArray, ntCharArray, ntShortArray, ntIntArray,
    ntLongArray, ntFloatArray, ntDoubleArray, ntStringArray, ntObjectArray,

    // array of array
    ntObjectBoolArray, ntObjectByteArray, ntObjectCharArray, ntObjectShortArray,
    ntObjectIntArray, ntObjectLongArray, ntObjectFloatArray,
    ntObjectDoubleArray, ntObjectStringArray, ntObjectObjectArray,

    ntVoid, ntNull, ntUnknown);

  TMethodAttribute = (static, nonstatic, nonvirtual);

  TJBooleanArray = array of jboolean;
  TJByteArray = array of jbyte;
  TJCharArray = array of jchar;
  TJShortArray = array of jshort;
  TJIntArray = array of jint;
  TJLongArray = array of jlong;
  TJFloatArray = array of jfloat;
  TJDoubleArray = array of jdouble;
  TJObjectArray = array of jobject;

  // Achtung: einige Funktionen werden nur in Je2Java verwendet
function SigToTyp(Sig: string): string;
function CorrectGetName(Name: string): string;
function SigToNumType(Sig: string): TNumType;
function TypToNumType(Typ: string): TNumType;

function NumTypeToSig(NumType: TNumType; Sig: string): string;
function GetFirstSig(var Sig: string): string;

implementation

uses StrUtils, UUtils;

function GetFirstSig(var Sig: string): string;
begin
  if Pos(Sig[1], 'ZBCSIJFD') > 0 then
  begin
    Result := Sig[1];
    Delete(Sig, 1, 1);
  end
  else if (Sig[1] = '[') and (Pos(Sig[2], 'ZBCSIJFD') > 0) then
  begin
    Result := Copy(Sig, 1, 2);
    Delete(Sig, 1, 2);
  end
  else
  begin
    var Posi := Pos(';', Sig);
    Result := Copy(Sig, 1, Posi);
    Delete(Sig, 1, Posi);
  end;
  if Left(Sig, 1) = '|' then
    Sig := Right(Sig, 2);
end;

function SigToNumType(Sig: string): TNumType;
begin
  Result := ntUnknown;
  var Offset := 0;
  if Sig[1] = '[' then
  begin
    Offset := 10;
    Delete(Sig, 1, 1);
  end;
  if Sig[1] = '[' then
  begin
    Offset := 20;
    Delete(Sig, 1, 1);
  end;

  case Sig[1] of
    'Z':
      Result := ntBool;
    'B':
      Result := ntByte;
    'C':
      Result := ntChar;
    'S':
      Result := ntShort;
    'I':
      Result := ntInt;
    'J':
      Result := ntLong;
    'F':
      Result := ntFloat;
    'D':
      Result := ntDouble;
    'V':
      Result := ntVoid;
    'L':
      if Sig = 'Ljava/lang/String;' then
        Result := ntString
      else
        Result := ntObject;
  end;
  Result := TNumType(Ord(Result) + Offset);
end;

function NumTypeToSig(NumType: TNumType; Sig: string): string;
begin
  var Arr:= '';
  if Ord(NumType) in [10 .. 19] then
  begin
    NumType := TNumType(Ord(NumType) - 10);
    Arr := '[';
  end;
  if Ord(NumType) in [20 .. 29] then
  begin
    NumType := TNumType(Ord(NumType) - 20);
    Arr := '[[';
  end;
  case NumType of
    ntBool:
      Result := 'Z';
    ntByte:
      Result := 'B';
    ntChar:
      Result := 'C';
    ntShort:
      Result := 'S';
    ntInt:
      Result := 'I';
    ntLong:
      Result := 'J';
    ntFloat:
      Result := 'F';
    ntDouble:
      Result := 'D';
    ntString:
      Result := 'Ljava/lang/String;';
    ntObject:
      begin
        Sig := ReplaceStr(Sig, '[]', '');
        Sig := ReplaceStr(Sig, '.', '/');
        if Right(Sig, -1) <> ';' then
          Sig := 'L' + Sig + ';';
        Result := Sig;
      end;
    ntVoid:
      Result := 'V';
  else
    Result := '';
  end;
  Result := Arr + Result;
end;

function SigToTyp(Sig: string): string;
begin
  var Arr := '';
  if Sig[1] = '[' then
  begin
    Arr := '[]';
    Delete(Sig, 1, 1);
  end;
  if Sig[1] = '[' then
  begin
    Arr := '[][]';
    Delete(Sig, 1, 1);
  end;
  case Sig[1] of
    'Z':
      Result := 'boolean';
    'C':
      Result := 'char';
    'B':
      Result := 'byte';
    'S':
      Result := 'short';
    'I':
      Result := 'int';
    'J':
      Result := 'long';
    'F':
      Result := 'float';
    'D':
      Result := 'double';
    'L':
      if (Sig = 'Ljava/lang/String;') or (Sig = 'LString;') then
        Result := 'String'
      else
      begin
        Sig := Copy(Sig, 2, Length(Sig) - 2);
        Result := ReplaceStr(Sig, '/', '.');
      end;
    'V':
      Result := 'void';
  else
    Result := 'typ-error';
  end;
  Result := Result + Arr;
end;

// http://illegalargumentexception.blogspot.de/2008/06/java-class-names.html
function CorrectGetName(Name: string): string;
begin
  var Arr := '';
  while Name[1] = '[' do
  begin
    Arr := Arr + '[]';
    Delete(Name, 1, 1);
  end;
  if Length(Name) = 1 then
    case Name[1] of
      'Z':
        Result := 'boolean';
      'C':
        Result := 'char';
      'B':
        Result := 'byte';
      'S':
        Result := 'short';
      'I':
        Result := 'int';
      'J':
        Result := 'long';
      'F':
        Result := 'float';
      'D':
        Result := 'double';
      'V':
        Result := 'void';
    end
  else if (Name[1] = 'L') and (Name[Length(Name)] = ';') then
    Result := Copy(Name, 2, Length(Name) - 2)
  else
    Result := Name;
  Result := Result + Arr;
end;

function TypToNumType(Typ: string): TNumType;
begin
  var Offset := 0;
  var Posi := Pos('[]', Typ);
  if Posi > 0 then
  begin
    Offset := 10;
    Delete(Typ, Posi, 2);
  end;
  Posi := Pos('[]', Typ);
  if Posi > 0 then
  begin
    Offset := 20;
    Delete(Typ, Posi, 2);
  end;
  if Typ = 'boolean' then
    Result := ntBool
  else if Typ = 'byte' then
    Result := ntByte
  else if Typ = 'char' then
    Result := ntChar
  else if Typ = 'short' then
    Result := ntShort
  else if Typ = 'int' then
    Result := ntInt
  else if Typ = 'long' then
    Result := ntLong
  else if Typ = 'float' then
    Result := ntFloat
  else if Typ = 'double' then
    Result := ntDouble
  else if Typ = 'String' then
    Result := ntString
  else if Typ = 'java.lang.String' then
    Result := ntString
  else if Typ = 'void' then
    Result := ntVoid
  else
    Result := ntObject;
  Result := TNumType(Ord(Result) + Offset);
end;

end.
