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

  jbyte = type Shortint;
  jint = Integer;
  jlong = Int64; // Comp;
  jboolean = Boolean;
  jchar = Word;
  jshort = SmallInt;
  jfloat = single;
  jdouble = double;
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
        (i: jint);
      5:
        (l: jlong);
      6:
        (f: jfloat);
      7:
        (d: jdouble);
      8:
        (s: jobject);
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
    name, signature: PAnsiChar;
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
    GetVersion: function(env: PJNIEnv): jint; stdcall;
    DefineClass: function(env: PJNIEnv; const name: PAnsiChar; loader: jobject;
      const buf: PJByte; len: jsize): jclass; stdcall;
    FindClass: function(env: PJNIEnv; const name: PAnsiChar): jclass; stdcall;
    FromReflectedMethod: function(env: PJNIEnv; method: jobject)
      : jmethodID; stdcall;
    FromReflectedField: function(env: PJNIEnv; field: jobject)
      : jfieldID; stdcall;
    ToReflectedMethod: function(env: PJNIEnv; cls: jclass; methodID: jmethodID;
      isStatic: jboolean): jobject; stdcall;
    GetSuperClass: function(env: PJNIEnv; sub { , sup } : jclass)
      : jclass; stdcall;
    IsAssignableFrom: function(env: PJNIEnv; sub, sup: jclass)
      : jboolean; stdcall;
    ToReflectedField: function(env: PJNIEnv; cls: jclass; fieldID: jfieldID;
      isStatic: jboolean): jobject; stdcall;
    Throw: function(env: PJNIEnv; obj: jthrowable): jint; stdcall;
    ThrowNew: function(env: PJNIEnv; clazz: jclass; const msg: PAnsiChar)
      : jint; stdcall;
    ExceptionOccurred: function(env: PJNIEnv): jthrowable; stdcall;
    ExceptionDescribe, ExceptionClear: procedure(env: PJNIEnv); stdcall;
    FatalError: procedure(env: PJNIEnv; const msg: PAnsiChar); stdcall;
    PushLocalFrame: function(env: PJNIEnv; capacity: jint): jint; stdcall;
    PopLocalFrame: function(env: PJNIEnv; res: jobject): jobject; stdcall;
    NewGlobalRef: function(env: PJNIEnv; obj: jobject): jobject; stdcall;
    DeleteGlobalRef: procedure(env: PJNIEnv; obj: jobject); stdcall;
    DeleteLocalRef: procedure(env: PJNIEnv; obj: jobject); stdcall;
    IsSameObject: function(env: PJNIEnv; obj1, obj2: jobject)
      : jboolean; stdcall;
    NewLocalRef: function(env: PJNIEnv; ref: jobject): jobject; stdcall;
    EnsureLocalCapacity: function(env: PJNIEnv; capacity: jint): jint; stdcall;
    AllocObject: function(env: PJNIEnv; clazz: jclass): jclass; stdcall;
    NewObject: function(env: PJNIEnv; clazz: jclass; methodID: jmethodID)
      : jobject; stdcall;
    NewObjectV: function(env: PJNIEnv; clazz: jclass; methodID: jmethodID;
      args: Pointer): jobject; stdcall;
    NewObjectA: function(env: PJNIEnv; clazz: jclass; methodID: jmethodID;
      args: PJValue): jobject; stdcall;
    GetObjectClass: function(env: PJNIEnv; obj: jobject): jclass; stdcall;
    IsInstanceof: function(env: PJNIEnv; obj: jobject; clazz: jclass)
      : jboolean; stdcall;
    GetMethodID: function(env: PJNIEnv; clazz: jclass;
      const name, sig: PAnsiChar): jmethodID; stdcall;
    CallObjectMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jobject; stdcall;
    CallObjectMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jobject; stdcall;
    CallObjectMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jobject; stdcall;
    CallBooleanMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jboolean; stdcall;
    CallBooleanMethodV: function(env: PJNIEnv; obj: jobject;
      methodID: jmethodID; args: Pointer): jboolean; stdcall;
    CallBooleanMethodA: function(env: PJNIEnv; obj: jobject;
      methodID: jmethodID; args: Pointer): jboolean; stdcall;
    CallByteMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jbyte; stdcall;
    CallByteMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jbyte; stdcall;
    CallByteMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jbyte; stdcall;
    CallCharMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jchar; stdcall;
    CallCharMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jchar; stdcall;
    CallCharMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jchar; stdcall;
    CallShortMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jshort; stdcall;
    CallShortMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jshort; stdcall;
    CallShortMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jshort; stdcall;
    CallIntMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jint; stdcall;
    CallIntMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jint; stdcall;
    CallIntMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jint; stdcall;
    CallLongMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jlong; stdcall;
    CallLongMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jlong; stdcall;
    CallLongMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jlong; stdcall;
    CallFloatMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jfloat; stdcall;
    CallFloatMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jfloat; stdcall;
    CallFloatMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jfloat; stdcall;
    CallDoubleMethod: function(env: PJNIEnv; obj: jobject; methodID: jmethodID)
      : jdouble; stdcall;
    CallDoubleMethodV: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer): jdouble; stdcall;
    CallDoubleMethodA: function(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue): jdouble; stdcall;
    CallVoidMethod: procedure(env: PJNIEnv; obj: jobject;
      methodID: jmethodID); stdcall;
    CallVoidMethodV: procedure(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: Pointer); stdcall;
    CallVoidMethodA: procedure(env: PJNIEnv; obj: jobject; methodID: jmethodID;
      args: PJValue); stdcall;
    CallNonvirtualObjectMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jobject; stdcall;
    CallNonvirtualObjectMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jobject; stdcall;
    CallNonvirtualObjectMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jobject; stdcall;
    CallNonvirtualBooleanMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jboolean; stdcall;
    CallNonvirtualBooleanMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jboolean; stdcall;
    CallNonvirtualBooleanMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jboolean; stdcall;
    CallNonvirtualByteMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jbyte; stdcall;
    CallNonvirtualByteMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jbyte; stdcall;
    CallNonvirtualByteMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jbyte; stdcall;
    CallNonvirtualCharMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jchar; stdcall;
    CallNonvirtualCharMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jchar; stdcall;
    CallNonvirtualCharMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jchar; stdcall;
    CallNonvirtualShortMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jshort; stdcall;
    CallNonvirtualShortMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jshort; stdcall;
    CallNonvirtualShortMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jshort; stdcall;
    CallNonvirtualIntMethod: function(env: PJNIEnv; obj: jobject; clazz: jclass;
      methodID: jmethodID): jint; stdcall;
    CallNonvirtualIntMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jint; stdcall;
    CallNonvirtualIntMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jint; stdcall;
    CallNonvirtualLongMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jlong; stdcall;
    CallNonvirtualLongMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jlong; stdcall;
    CallNonvirtualLongMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jlong; stdcall;
    CallNonvirtualFloatMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jfloat; stdcall;
    CallNonvirtualFloatMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jfloat; stdcall;
    CallNonvirtualFloatMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jfloat; stdcall;
    CallNonvirtualDoubleMethod: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID): jdouble; stdcall;
    CallNonvirtualDoubleMethodV: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer): jdouble; stdcall;
    CallNonvirtualDoubleMethodA: function(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue): jdouble; stdcall;
    CallNonvirtualVoidMethod: procedure(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID); stdcall;
    CallNonvirtualVoidMethodV: procedure(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: Pointer); stdcall;
    CallNonvirtualVoidMethodA: procedure(env: PJNIEnv; obj: jobject;
      clazz: jclass; methodID: jmethodID; args: PJValue); stdcall;

    GetFieldId: function(env: PJNIEnv; clazz: jclass;
      const name, sig: PAnsiChar): jfieldID; stdcall;
    GetObjectField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jobject; stdcall;
    GetBooleanField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jboolean; stdcall;
    GetByteField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jbyte; stdcall;
    GetCharField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jchar; stdcall;
    GetShortField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jshort; stdcall;
    GetIntField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jint; stdcall;
    GetLongField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jlong; stdcall;
    GetFloatField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jfloat; stdcall;
    GetDoubleField: function(env: PJNIEnv; obj: jobject; fieldID: jfieldID)
      : jdouble; stdcall;

    SetObjectField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jobject); stdcall;
    SetBooleanField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jboolean); stdcall;
    SetByteField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jbyte); stdcall;
    SetCharField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jchar); stdcall;
    SetShortField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jshort); stdcall;
    SetIntField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jint); stdcall;
    SetLongField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jlong); stdcall;
    SetFloatField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jfloat); stdcall;
    SetDoubleField: procedure(env: PJNIEnv; obj: jobject; fieldID: jfieldID;
      val: jdouble); stdcall;

    GetStaticMethodID: function(env: PJNIEnv; clazz: jclass;
      const name, sig: PAnsiChar): jmethodID; stdcall;
    CallStaticObjectMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jobject; stdcall;
    CallStaticObjectMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jobject; stdcall;
    CallStaticObjectMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jobject; stdcall;
    CallStaticBooleanMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jboolean; stdcall;
    CallStaticBooleanMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jboolean; stdcall;
    CallStaticBooleanMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jboolean; stdcall;
    CallStaticByteMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jbyte; stdcall;
    CallStaticByteMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jbyte; stdcall;
    CallStaticByteMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jbyte; stdcall;
    CallStaticCharMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jchar; stdcall;
    CallStaticCharMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jchar; stdcall;
    CallStaticCharMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jchar; stdcall;
    CallStaticShortMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jshort; stdcall;
    CallStaticShortMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jshort; stdcall;
    CallStaticShortMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jshort; stdcall;
    CallStaticIntMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jint; stdcall;
    CallStaticIntMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jint; stdcall;
    CallStaticIntMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jint; stdcall;
    CallStaticLongMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jlong; stdcall;
    CallStaticLongMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jlong; stdcall;
    CallStaticLongMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jlong; stdcall;
    CallStaticFloatMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jfloat; stdcall;
    CallStaticFloatMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jfloat; stdcall;
    CallStaticFloatMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jfloat; stdcall;
    CallStaticDoubleMethod: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID): jdouble; stdcall;
    CallStaticDoubleMethodV: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer): jdouble; stdcall;
    CallStaticDoubleMethodA: function(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue): jdouble; stdcall;
    CallStaticVoidMethod: procedure(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID); stdcall;
    CallStaticVoidMethodV: procedure(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: Pointer); stdcall;
    CallStaticVoidMethodA: procedure(env: PJNIEnv; clazz: jclass;
      methodID: jmethodID; args: PJValue); stdcall;

    GetStaticFieldID: function(env: PJNIEnv; clazz: jclass;
      const name, sig: PAnsiChar): jfieldID; stdcall;
    GetStaticObjectField: function(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID): jobject; stdcall;
    GetStaticBooleanField: function(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID): jboolean; stdcall;
    GetStaticByteField: function(env: PJNIEnv; clazz: jclass; fieldID: jfieldID)
      : jbyte; stdcall;
    GetStaticCharField: function(env: PJNIEnv; clazz: jclass; fieldID: jfieldID)
      : jchar; stdcall;
    GetStaticShortField: function(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID): jshort; stdcall;
    GetStaticIntField: function(env: PJNIEnv; clazz: jclass; fieldID: jfieldID)
      : jint; stdcall;
    GetStaticLongField: function(env: PJNIEnv; clazz: jclass; fieldID: jfieldID)
      : jlong; stdcall;
    GetStaticFloatField: function(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID): jfloat; stdcall;
    GetStaticDoubleField: function(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID): jdouble; stdcall;

    SetStaticObjectField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jobject); stdcall;
    SetStaticBooleanField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jboolean); stdcall;
    SetStaticByteField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jbyte); stdcall;
    SetStaticCharField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jchar); stdcall;
    SetStaticShortField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jshort); stdcall;
    SetStaticIntField: procedure(env: PJNIEnv; clazz: jclass; fieldID: jfieldID;
      value: jint); stdcall;
    SetStaticLongField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jlong); stdcall;
    SetStaticFloatField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jfloat); stdcall;
    SetStaticDoubleField: procedure(env: PJNIEnv; clazz: jclass;
      fieldID: jfieldID; value: jdouble); stdcall;

    NewString: function(env: PJNIEnv; const unicode: PJChar; len: jsize)
      : jstring; stdcall;
    GetStringLength: function(env: PJNIEnv; str: jstring): jsize; stdcall;
    GetStringChars: function(env: PJNIEnv; str: jstring; isCopy: PJBoolean)
      : PJChar; stdcall;
    ReleaseStringChars: procedure(env: PJNIEnv; str: jstring;
      const chars: PJChar); stdcall;
    NewStringUTF: function(env: PJNIEnv; const utf: PAnsiChar)
      : jstring; stdcall;
    GetStringUTFLength: function(env: PJNIEnv; str: jstring): jsize; stdcall;
    GetStringUTFChars: function(env: PJNIEnv; str: jstring;
      var isCopy: jboolean): PAnsiChar; stdcall;
    ReleaseStringUTFChars: procedure(env: PJNIEnv; str: jstring;
      const chars: PAnsiChar); stdcall;
    GetArrayLength: function(env: PJNIEnv; arr: jarray): jsize; stdcall;
    NewObjectArray: function(env: PJNIEnv; len: jsize; clazz: jclass;
      init: jobject): jobjectArray; stdcall;
    GetObjectArrayElement: function(env: PJNIEnv; arr: jobjectArray;
      index: jsize): jobject; stdcall;
    SetObjectArrayElement: procedure(env: PJNIEnv; arr: jobjectArray;
      index: jsize; val: jobject); stdcall;
    NewBooleanArray: function(env: PJNIEnv; len: jsize): jbooleanArray; stdcall;
    NewByteArray: function(env: PJNIEnv; len: jsize): jbyteArray; stdcall;
    NewCharArray: function(env: PJNIEnv; len: jsize): jcharArray; stdcall;
    NewShortArray: function(env: PJNIEnv; len: jsize): jshortArray; stdcall;
    NewIntArray: function(env: PJNIEnv; len: jsize): jintArray; stdcall;
    NewLongArray: function(env: PJNIEnv; len: jsize): jlongArray; stdcall;
    NewFloatArray: function(env: PJNIEnv; len: jsize): jfloatArray; stdcall;
    NewDoubleArray: function(env: PJNIEnv; len: jsize): jdoubleArray; stdcall;
    GetBooleanArrayElements: function(env: PJNIEnv; arr: jbooleanArray;
      isCopy: PJBoolean): PJBoolean; stdcall;
    GetByteArrayElements: function(env: PJNIEnv; arr: jbyteArray;
      isCopy: PJBoolean): PJByte; stdcall;
    GetCharArrayElements: function(env: PJNIEnv; arr: jcharArray;
      isCopy: PJBoolean): PJChar; stdcall;
    GetShortArrayElements: function(env: PJNIEnv; arr: jshortArray;
      isCopy: PJBoolean): PJShort; stdcall;
    GetIntArrayElements: function(env: PJNIEnv; arr: jintArray; isCopy: PJInt)
      : PJInt; stdcall;
    GetLongArrayElements: function(env: PJNIEnv; arr: jlongArray;
      isCopy: PJLong): PJLong; stdcall;
    GetFloatArrayElements: function(env: PJNIEnv; arr: jfloatArray;
      isCopy: PJBoolean): PJFloat; stdcall;
    GetDoubleArrayElements: function(env: PJNIEnv; arr: jdoubleArray;
      isCopy: PJBoolean): PJDouble; stdcall;
    ReleaseBooleanArrayElements: procedure(env: PJNIEnv; arr: jbooleanArray;
      elems: PJBoolean; mode: jint); stdcall;
    ReleaseByteArrayElements: procedure(env: PJNIEnv; arr: jbyteArray;
      elems: PJByte; mode: jint); stdcall;
    ReleaseCharArrayElements: procedure(env: PJNIEnv; arr: jcharArray;
      elems: PJChar; mode: jint); stdcall;
    ReleaseShortArrayElements: procedure(env: PJNIEnv; arr: jshortArray;
      elems: PJShort; mode: jint); stdcall;
    ReleaseIntArrayElements: procedure(env: PJNIEnv; arr: jintArray;
      elems: PJInt; mode: jint); stdcall;
    ReleaseLongArrayElements: procedure(env: PJNIEnv; arr: jlongArray;
      elems: PJLong; mode: jint); stdcall;
    ReleaseFloatArrayElements: procedure(env: PJNIEnv; arr: jfloatArray;
      elems: PJFloat; mode: jint); stdcall;
    ReleaseDoubleArrayElements: procedure(env: PJNIEnv; arr: jdoubleArray;
      elems: PJDouble; mode: jint); stdcall;
    GetBooleanArrayRegion: procedure(env: PJNIEnv; arr: jbooleanArray;
      start, l: jsize; buf: PJBoolean); stdcall;
    GetByteArrayRegion: procedure(env: PJNIEnv; arr: jbyteArray;
      start, l: jsize; buf: PJByte); stdcall;
    GetCharArrayRegion: procedure(env: PJNIEnv; arr: jcharArray;
      start, l: jsize; buf: PJChar); stdcall;
    GetShortArrayRegion: procedure(env: PJNIEnv; arr: jshortArray;
      start, l: jsize; buf: PJShort); stdcall;
    GetIntArrayRegion: procedure(env: PJNIEnv; arr: jintArray; start, l: jsize;
      buf: PJInt); stdcall;
    GetLongArrayRegion: procedure(env: PJNIEnv; arr: jlongArray;
      start, l: jsize; buf: PJLong); stdcall;
    GetFloatArrayRegion: procedure(env: PJNIEnv; arr: jfloatArray;
      start, l: jsize; buf: PJFloat); stdcall;
    GetDoubleArrayRegion: procedure(env: PJNIEnv; arr: jdoubleArray;
      start, l: jsize; buf: PJDouble); stdcall;
    SetBooleanArrayRegion: procedure(env: PJNIEnv; arr: jbooleanArray;
      start, len: jsize; buf: PJBoolean); stdcall;
    SetByteArrayRegion: procedure(env: PJNIEnv; arr: jbyteArray;
      start, len: jsize; buf: PJByte); stdcall;
    SetCharArrayRegion: procedure(env: PJNIEnv; arr: jcharArray;
      start, len: jsize; buf: PJChar); stdcall;
    SetShortArrayRegion: procedure(env: PJNIEnv; arr: jshortArray;
      start, len: jsize; buf: PJShort); stdcall;
    SetIntArrayRegion: procedure(env: PJNIEnv; arr: jintArray;
      start, len: jsize; buf: PJInt); stdcall;
    SetLongArrayRegion: procedure(env: PJNIEnv; arr: jlongArray;
      start, len: jsize; buf: PJLong); stdcall;
    SetFloatArrayRegion: procedure(env: PJNIEnv; arr: jfloatArray;
      start, len: jsize; buf: PJFloat); stdcall;
    SetDoubleArrayRegion: procedure(env: PJNIEnv; arr: jdoubleArray;
      start, len: jsize; buf: PJDouble); stdcall;
    RegisterNatives: function(env: PJNIEnv; clazz: jclass;
      const method: PJNINativeMethod; nMethods: jint): jint; stdcall;
    UnregisterNatives: function(env: PJNIEnv; clazz: jclass): jint; stdcall;
    MonitorEnter: function(env: PJNIEnv; obj: jobject): jint; stdcall;
    MonitorExit: function(env: PJNIEnv; obj: jobject): jint; stdcall;
    GetJavaVM: function(env: PJNIEnv; vm: PPJavaVM): jint; stdcall;
    { from here, it's only supported by Java 2 }
    GetStringRegion: procedure(env: PJNIEnv; str: jstring; start, len: jsize;
      buf: PJChar); stdcall;
    GetStringUTFRegion: procedure(env: PJNIEnv; str: jstring; start, len: jsize;
      buf: PAnsiChar); stdcall;
    GetPrimitiveArrayCritical: function(env: PJNIEnv; arr: jarray;
      isCopy: PJBoolean): Pointer; stdcall;
    ReleasePrimitiveArrayCritical: procedure(env: PJNIEnv; arr: jarray;
      carray: Pointer; mode: jint); stdcall;
    GetStringCritical: function(env: PJNIEnv; str: jstring; isCopy: PJBoolean)
      : PJChar; stdcall;
    ReleaseStringCritical: procedure(env: PJNIEnv; str: jstring;
      const cstring: PJChar); stdcall;
    NewWeakGlobalRef: function(env: PJNIEnv; obj: jobject): jweak; stdcall;
    DeleteWeakGlobalRef: procedure(env: PJNIEnv; ref: jweak); stdcall;
    ExceptionCheck: function(env: PJNIEnv): jboolean; stdcall;
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
    name: PAnsiChar;
    Group: jobject;
  end;

  { The following structures will be VM-specific }

  JDK1_1InitArgs = packed record
    Version: jint;
    properties: ^PAnsiChar;
    checkSource, nativeStackSize, javaStackSize, minHeapSize, maxHeapSize,
      verifyMode: jint;
    classpath: PAnsiChar;
    vfprintf: function(filePointer: Pointer; const format: PAnsiChar;
      args: va_list): jint; stdcall;
    exit: procedure(exitCode: jint); stdcall;
    abort: procedure; stdcall;
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
    AttachCurrentThread: function(vm: PJavaVM; penv: PPJNIEnv; args: Pointer)
      : jint; stdcall;
    DetachCurrentThread: function(vm: PJavaVM; penv: PPJNIEnv; args: Pointer)
      : jint; stdcall;
    { the following function is only in Java 2 }
    GetEnv: function(vm: PJavaVM; penv: PPJNIEnv; Version: jint): jint; stdcall;
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
function SigToTyp(sig: string): string;
function correctGetName(name: string): string;
function SigToNumType(sig: string): TNumType;
function TypToNumType(Typ: string): TNumType;

function NumTypeToSig(NumType: TNumType; sig: string): string;
function GetFirstSig(var sig: string): string;

implementation

uses StrUtils, UUtils;

function GetFirstSig(var sig: string): string;
begin
  if Pos(sig[1], 'ZBCSIJFD') > 0 then
  begin
    Result := sig[1];
    delete(sig, 1, 1);
  end
  else if (sig[1] = '[') and (Pos(sig[2], 'ZBCSIJFD') > 0) then
  begin
    Result := copy(sig, 1, 2);
    delete(sig, 1, 2);
  end
  else
  begin
    var p := Pos(';', sig);
    Result := copy(sig, 1, p);
    delete(sig, 1, p);
  end;
  if Left(sig, 1) = '|' then
    sig := Right(sig, 2);
end;

function SigToNumType(sig: string): TNumType;
begin
  Result := ntUnknown;
  var Offset := 0;
  if sig[1] = '[' then
  begin
    Offset := 10;
    delete(sig, 1, 1);
  end;
  if sig[1] = '[' then
  begin
    Offset := 20;
    delete(sig, 1, 1);
  end;

  case sig[1] of
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
      if sig = 'Ljava/lang/String;' then
        Result := ntString
      else
        Result := ntObject;
  end;
  Result := TNumType(Ord(Result) + Offset);
end;

function NumTypeToSig(NumType: TNumType; sig: string): string;
begin
  var arr:= '';
  if Ord(NumType) in [10 .. 19] then
  begin
    NumType := TNumType(Ord(NumType) - 10);
    arr := '[';
  end;
  if Ord(NumType) in [20 .. 29] then
  begin
    NumType := TNumType(Ord(NumType) - 20);
    arr := '[[';
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
        sig := ReplaceStr(sig, '[]', '');
        sig := ReplaceStr(sig, '.', '/');
        if Right(sig, -1) <> ';' then
          sig := 'L' + sig + ';';
        Result := sig;
      end;
    ntVoid:
      Result := 'V';
  else
    Result := '';
  end;
  Result := arr + Result;
end;

function SigToTyp(sig: string): string;
begin
  var arr := '';
  if sig[1] = '[' then
  begin
    arr := '[]';
    delete(sig, 1, 1);
  end;
  if sig[1] = '[' then
  begin
    arr := '[][]';
    delete(sig, 1, 1);
  end;
  case sig[1] of
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
      if (sig = 'Ljava/lang/String;') or (sig = 'LString;') then
        Result := 'String'
      else
      begin
        sig := copy(sig, 2, length(sig) - 2);
        Result := ReplaceStr(sig, '/', '.');
      end;
    'V':
      Result := 'void';
  else
    Result := 'typ-error';
  end;
  Result := Result + arr;
end;

// http://illegalargumentexception.blogspot.de/2008/06/java-class-names.html
function correctGetName(name: string): string;
begin
  var arr := '';
  while Name[1] = '[' do
  begin
    arr := arr + '[]';
    delete(Name, 1, 1);
  end;
  if length(Name) = 1 then
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
  else if (Name[1] = 'L') and (Name[length(Name)] = ';') then
    Result := copy(Name, 2, length(Name) - 2)
  else
    Result := Name;
  Result := Result + arr;
end;

function TypToNumType(Typ: string): TNumType;
begin
  var Offset := 0;
  var p := Pos('[]', Typ);
  if p > 0 then
  begin
    Offset := 10;
    delete(Typ, p, 2);
  end;
  p := Pos('[]', Typ);
  if p > 0 then
  begin
    Offset := 20;
    delete(Typ, p, 2);
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
