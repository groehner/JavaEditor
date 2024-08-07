unit UJniWrapper1;

interface

uses Classes, Jni;

type

  TComJavaClass = class;
  TComJavaObject = class;

  TComJavaValue = class
  private
    FComJava: TObject;
    FValueAsString: string;
    FKind: TNumType;
    FSig: string;
    procedure setSig(const Sig: string);
  public
    constructor create(Typ: string; ComJava: TObject); overload;
    // duplicate dummy due to '%s' with identical parameters; Cannot access from C++ (Delphi)
    constructor createFromString(const s: string; ComJava: TObject; const dummy: boolean = true);
    constructor create(const Value, Sig: string; ComJava: TObject); overload;

    function AsFormattedString: string;
    procedure setFromString(s: string);
    function ToString: string; override;
    function AsString: string;
    function AsStringWithout(c: char): string;
    property Kind: TNumType read FKind write FKind;
    property Sig: string read FSig write setSig;
    property Value: string read FValueAsString write FValueAsString;
  end;

  TComJavaParams = class
  private
    FParams: string;
    FSig: string;
  public
    procedure addToParams(const Value, Sig: string);
    property Signature: string read FSig;
    property Params: string read FParams;
  end;

  TComJavaAttribute = class
  private
    FComJava: TObject;
    FClass: TComJavaClass;
  public
    constructor Create(cls: TComJavaClass; ComJava: TObject);
    function  GetAttributeValue(jobj: TComJavaObject; const Attributename, Typename: string; IsStatic: boolean): string;
    procedure SetAttributeValue(jobj: TComJavaObject; const Attributename, Typename, Value: string; IsStatic: boolean);
  end;

  TComJavaClass = class
  private
    FComJava: TObject;
    FName: string;
    FGenericName: string;
    FSig: string;
    FPathname: string;
    FValid: Boolean;
    FGeneric: string;
    FError: string;
    FConstructors: TStringList;
    FAttributes: TStringList;
    FClassAttributeNames: TStringList;
    FStaticMethods: TStringList;
    FNotStaticMethods: TStringList;
    FAllMethods: TStringList;
    FSuperclass: string;
    FGetSuperclass: boolean;
    function getTyp: string;
    function getImportTyp: string;
    function getGenericName: string;
  public
    constructor CreateClass(const Name: string; ComJava: TObject);
    constructor CreateLoadClass(const Name, Path: string; ComJava: TObject);
    // duplicate dummy due to '%s' with identical parameters; Cannot access from C++ (Delphi)
    constructor CreateWithHandle(const aClassname: string; ComJava: TObject; const dummy: boolean = true);
    class function IsInterface(const Interfacename: string; ComJava: TObject): boolean;
    class function findClass(const aClassname: string; ComJava: TObject): boolean;
    destructor Destroy; override;
    procedure init;
    function getConstructors: TStringList;
    function getMethods(const static: string): TStringList;
    function getClassAttributes: TStringList;
    function getClassAttributeNames: TStringlist;
    function getRefreshedClassAttributeNames: TStringList;
    function getSuperclassName: string;
    property Signature: string read FSig write FSig;
    property Typ: string read getTyp;
    property ImportTyp: string read getImportTyp;
    property IsValid: Boolean read FValid;
    property Pathname: string read FPathname write FPathname;
    property Name: string read FName;
    property Error: string read FError;
    property GenericName: string read getGenericName write FGenericName;
    property Generic: string read FGeneric write FGeneric;
    function getGenericTyp: string;
  end;

  TComJavaObject = class
  private
    FComJava: TObject;
    FClass: TComJavaClass;
    FName: string;
    FValid: Boolean;
    FError: string;
    FCreateValue: string;
    FAttributeValues: TStringList;
  public
    constructor Create(const Objectname: string; jcl: TComJavaClass; ComParams: TComJavaParams; ComJava: TObject);
    constructor CreateByName(const Objectname: string; jcl: TComJavaClass; ComJava: TObject);
    constructor CreateNewUnnamedObject(const Objectname: string; ComJava: TObject);
    destructor Destroy; override;
    procedure Delete;
    function ToString: string; override;
    function getObjectAttributes: TStringList;
    function getAttributeValues: TStringlist;
    function DebugGetAttributeValues: string;
    function getAttributeNames: TStringList;
    function getRefreshedAttributeNames: TStringList;
    function getObjectAttributeValues(withType: boolean): TStringList;
    function getObjectAttributeNames: TStringlist;
    property ClassRef: TComJavaClass read FClass;
    property isValid: Boolean read FValid;
    property Name: string read FName;
    property Error: string read FError;
    property CreateValue: string read FCreateValue;
    property AttributeValues: TStringList read FAttributeValues write FAttributeValues;
  end;

  TComJavaMethod = class
  private
    FComJava: TObject;
    Fclass: TComJavaClass;
    FMethodname: string;
    FMethodType: TMethodAttribute;
    FParams: TComJavaParams;
    FSig: string;
    FRetVal: TNumType;
    FReturnType: string;
    FValid: boolean;
    FError: string;
  public
    constructor Create(cls: TComJavaClass;
                       const aMethodname: string;
                       methodType: TMethodAttribute;
                       const theReturnType: string;
                       params: TComJavaParams;
                       ComJava: TObject);
    function Call(jobj: TComJavaObject): TComJavaValue;
    function getExpressionValue: TComJavaValue;
    function getExpressionType: string;
    property isValid: boolean read FValid;
    property Error: string read FError;
    property aMethodname: string read FMethodname;
  end;

implementation

  uses SysUtils, StrUtils, UUtils, UConfiguration, UComJava1;

  function TryStrToFloat(var s: string; var f: double): boolean;
    var p: integer; error: boolean;

    function StrToFloat(const S: string): Extended;
      var e: extended;
    begin
      if TextToFloat(PChar(S), e, fvExtended) then
        Result:= e
      else begin
        Result:= 0;
        error:= true;
      end;
    end;

  begin
    error:= false;
    p:= Pos('.', s);
    if (p > 0) and (FormatSettings.DecimalSeparator <> '.') then s[p]:= Formatsettings.DecimalSeparator;
    p:= Pos(',', s);
    if (p > 0) and (Formatsettings.DecimalSeparator <> ',') then s[p]:= Formatsettings.DecimalSeparator;
    f:= StrToFloat(s);
    Result:= not error;
  end;

  function TypToSig(Typ: string): string;
    var s, dimension: string; p: integer;
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
    if Typ = 'java.lang.String' then Result:= 'Ljava/lang/String;' else
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
      Typ:= WithoutGeneric(Typ); // z. B. LinkedList<pupil>
      s:= (getComJava as TComJava1).GetSignature(Typ);  // java/util/ArrayList   // TODO
      if s = '' then
        if length(Typ) = 1  // generic
          then Result:= 'Ljava/lang/Object;'
          else Result:= 'L' + ReplaceStr(Typ, '.', '/') + ';'
      else
        Result:= s;
    end;
    Result:= dimension + Result;
  end;

  function ClassTypToSig(Typ: string): string;
  begin
    var dimension:= '';
    var p:= Pos('[]', Typ);
    while p > 0 do begin
      dimension:= dimension + '[';
      delete(Typ, p, 2);
      p:= Pos('[]', Typ);
    end;
    Typ:= WithoutGeneric(Typ); // z. B. LinkedList<pupil>
    Result:= dimension + 'L' + ReplaceStr(Typ, '.', '/') + ';';
  end;

  function SigToClassTyp(Sig: string): string;
  begin
    delete(Sig, length(Sig), 1);
    while Sig[1] = '[' do begin
      delete(Sig, 1, 1);
      Sig:= Sig + '[]';
    end;
    delete(Sig, 1, 1); // L
    Result:= Sig;
  end;

  procedure TComJavaClass.init;
  begin
    FConstructors:= nil;
    FAttributes:= nil;
    FClassAttributeNames:= nil;
    FStaticMethods:= nil;
    FNotStaticMethods:= nil;
    FAllMethods:= nil;
    FGetSuperclass:= false;
  end;

  constructor TComJavaClass.CreateClass(const Name: string; ComJava: TObject);
  begin
    FPathname:= FName;
    FComJava:= ComJava;
    FName:= WithoutGeneric(Name);
    FSig:= ClassTypToSig(FName);
    var s:= (FComJava as TComJava1).ExecuteCommand('createClass'#4 + FName);
    FValid:= (Left(s, 4) <> '-ERR');
    FError:= Right(s, 6);
    Init;
  end;

  constructor TComJavaClass.CreateLoadClass(const Name, Path: string; ComJava: TObject);
  begin
    FPathname:= Path;
    FComJava:= ComJava;
    FName:= WithoutGeneric(Name);
    FSig:= ClassTypToSig(FName);
    var s:= (FComJava as TComJava1).ExecuteCommand('loadClass'#4 + FName + #4 + Path);
    FValid:= (Left(s, 4) <> '-ERR');
    FError:= Right(s, 6);
    Init;
  end;

  constructor TComJavaClass.CreateWithHandle(const aClassname: string; ComJava: TObject; const Dummy: boolean = true);
  begin
    FPathname:= '';
    FComJava:= ComJava;
    FName:= WithoutGeneric(aClassname);
    FSig:= ClassTypToSig(FName);
    FValid:= true;
    Init;
  end;

  destructor TComJavaClass.Destroy;
  begin
    FreeAndNil(FConstructors);
    FreeAndNil(FAttributes);
    FreeAndNil(FClassAttributeNames);
    FreeAndNil(FStaticMethods);
    FreeAndNil(FNotStaticMethods);
    FreeAndNil(FAllMethods);
  end;

  class function TComJavaClass.IsInterface(const Interfacename: string; ComJava: TObject): boolean;
  begin
    var s:= (ComJava as TComJava1).ExecuteCommand('isInterface'#4 + Interfacename);
    Result:= (Left(s, 3) = '+OK');
  end;

  class function TComJavaClass.findClass(const aClassname: string; ComJava: TObject): boolean;
  begin
    if (aClassname = 'T') or (aClassname = 'E') then
      Result:= false
    else begin
      var s:= (ComJava as TComJava1).ExecuteCommandWithoutDebugger('findClass'#4 + aClassname);
      Result:= (Left(s, 3) = '+OK');
    end;
  end;

  function TComJavaClass.getTyp: string;
  begin
    var s:= FSig;
    var p:= Pos('/', s);
    while p > 0 do begin
      delete(s, 1, p);
      p:= Pos('/', s);
    end;
    Result:= s;
  end;

  function TComJavaClass.getGenericTyp: string;
  begin
    Result:= SigToClassTyp(FSig);
    if Generic <> '' then
      Result:= Result + '<' + Generic + '>';
  end;

  function TComJavaClass.getImportTyp: string;
  begin
    Result:= ReplaceStr(FName, '/', '.');
  end;

  function TComJavaClass.getGenericName: string;
  begin
    if FGenericname = ''
      then Result:= FName
      else Result:= FGenericname;
  end;

  function TComJavaClass.getSuperclassName: string;
  begin
    if not FGetSuperclass then begin
      FSuperclass:= (FComJava as TComJava1).ExecuteCommand('getSuperClass'#4 + FName);
      if Pos('+OK ', FSuperclass) = 1
        then Delete(FSuperclass, 1, 4)
        else FSuperclass:= '';
      FGetSuperclass:= true;
    end;
    Result:= FSuperclass;
  end;

  function TComJavaClass.getConstructors: TStringList;
  begin
    if FConstructors = nil then begin
      var s:= (FComJava as TComJava1).ExecuteCommand('getConstructors'#4 + FName);
      if Pos('+OK ', s) = 1
        then delete(s, 1, 6)
        else s:= '';
      FConstructors:= TStringList.Create;
      FConstructors.Text:= s;
    end;
    Result:= FConstructors;
  end;

  function TComJavaClass.getClassAttributes: TStringList;
  begin
    if FAttributes = nil then begin
      var s:= (FComJava as TComJava1).ExecuteCommand('getAttributes'#4 + FName);
      if Pos('+OK', s) = 1
        then delete(s, 1, 5)
        else s:= '';
      FAttributes:= split(#4, s);
    end;
    Result:= FAttributes;
  end;

  function TComJavaClass.getClassAttributeNames: TStringList;
  begin
    if FClassAttributeNames = nil then begin
      var s:= (FComJava as TComJava1).ExecuteCommand('getAttributeNames'#4 + FName);
      if Pos('+OK', s) = 1
        then delete(s, 1, 5)
        else s:= '';
      FClassAttributeNames:= split(#4, s);
      if s = '' then FClassAttributeNames.Text:= '';
    end;
    Result:= FClassAttributeNames;
  end;

  function TComJavaClass.getRefreshedClassAttributeNames: TStringList;
  begin
    if assigned(FClassAttributeNames) then
      FreeAndNil(FClassAttributeNames);
    var s:= (FComJava as TComJava1).ExecuteCommand('getRefreshedAttributeNames'#4 + FName);
    if Pos('+OK', s) = 1
      then delete(s, 1, 5)
      else s:= '';
    FClassAttributeNames:= split(#4, s);
    if s = '' then FClassAttributeNames.Text:= '';
    Result:= FClassAttributeNames;
  end;

  function TComJavaClass.getMethods(const static: string): TStringList;

    function getmethods_(const static: string): string;
    begin
      var s:= (FComJava as TComJava1).ExecuteCommand('getMethods'#4 + FName+ #4 + static);
      if Pos('+OK ', s) = 1
        then delete(s, 1, 6)
        else s:= '';
      Result:= s;
    end;

  begin
    if static = 'static' then begin
      if FStaticMethods = nil then begin
        FStaticMethods:= TStringList.Create;
        FStaticMethods.Text:= getmethods_(static);
      end;
      Result:= FStaticMethods;
    end else if static = 'not static' then begin
      if FNotStaticMethods = nil then begin
        FNotStaticMethods:= TStringList.Create;
        FNotStaticMethods.Text:= getmethods_(static);
      end;
      Result:= FNotStaticMethods;
    end else begin
      if FAllMethods = nil then begin
        FAllMethods:= TStringList.Create;
        FAllMethods.Text:= getmethods_(static);
      end;
      Result:= FAllMethods;
    end;
  end;

  {--- TJavaObject ------------------------------------------------------------}

  constructor TComJavaObject.Create(const Objectname: string; jcl: TComJavaClass;
                                    ComParams: TComJavaParams; ComJava: TObject);
    var Signature, Params, s: string;
  begin
    FComJava:= ComJava;
    FName:= Objectname;
    FAttributeValues:= nil;
    FClass:= jcl;
    FError:= '';
    FCreateValue:= '';
    Signature:= '';
    if ComParams =  nil then begin
      Signature:= '()V';
      Params:= '';
    end else begin
      Signature:= '(' + ComParams.Signature + ')V';
      Params:= ComParams.Params;
    end;
    s:= (FComJava as TComJava1).
          ExecuteCommand('createObject'#4 + jcl.Name + #4 +
            Objectname + #4 + Signature + #4 + Params);
    FValid:= (Left(s, 3) = '+OK');
    if FValid
      then FCreateValue:= Right(s, 5)
      else FError:= Right(s, 6);
  end;

  constructor TComJavaObject.CreateByName(const Objectname: string; jcl: TComJavaClass; ComJava: TObject);
  begin
    FComJava:= ComJava;
    FName:= Objectname;
    FAttributeValues:= nil;
    FClass:= jcl;
    FValid:= true;
  end;

  constructor TComJavaObject.CreateNewUnnamedObject(const Objectname: string; ComJava: TObject);
    var s, aClassname: string; i, p: integer; ClassList: TStringList;
  begin
    FComJava:= ComJava;
    FName:= Objectname;
    FAttributeValues:= nil;
    s:= (FComJava as TComJava1).executeCommand('getClassOf'#4 + Objectname);
    FValid:= (Left(s, 3) = '+OK');
    if FValid then begin
      s:= Right(s, 5);
      p:= Pos('|', s);
      FCreateValue:= copy(s, p+1, length(s));
      aClassname:= copy(s, 1, p-1);
      ClassList:= TComJava1(ComJava).ClassList;
      i:= ClassList.IndexOf(aClassname);
      if i = -1 then begin
        FClass:= TComJavaClass.CreateWithHandle(aClassname, FComJava);
        ClassList.AddObject(aClassname, FClass);
      end else
        FClass:= TComJavaClass(ClassList.Objects[i]);
      FError:= '';
    end else
      FError:= Right(s, 6);
  end;

  destructor TComJavaObject.Destroy;
  begin
    FreeAndNil(FAttributeValues);
    inherited;
  end;

  procedure TComJavaObject.Delete;
  begin
    if not (FComJava as TComJava1).Terminated then
      (FComJava as TComJava1).ExecuteCommand('deleteObject'#4 + FName);
  end;

  function TComJavaObject.toString: string;
  begin
    var s:= (FComJava as TComJava1).ExecuteCommand('toString'#4 + FName);
    Result:= right(s, 5)
  end;

  function TComJavaObject.getAttributeValues: TStringlist;
    var s1, s: string;
  begin
    Result:= nil;
    try
      s1:= 'getAttributeValues'#4 + FName;
      if FConfiguration.ArrayListAsIntegratedList
        then s1:= s1 + #4 + 'ArrayList'
        else s1:= s1 + #4;
      if FComJava is TComJava1 then
        s:= (FComJava as TComJava1).ExecuteCommand(s1);
      if Pos('+OK ', s) = 1 then begin
        s:= right(s, 6);
        if assigned(FAttributeValues) then
          FreeAndNil(FAttributeValues);
        FAttributeValues:= split(#4, s);
        if FAttributeValues.Count > 0 then begin
          FCreateValue:= FAttributeValues[FAttributeValues.Count-1];
          FAttributeValues.Delete(FAttributeValues.Count-1);
        end;
        Result:= FAttributeValues;
      end else begin
        FConfiguration.Log('TComJavaObject.getAttributeValues: ' + s1 + ' - ' + s);
        Result:= TStringList.Create;
        Result.Add(s)
      end;
    except on e: exception do
      FConfiguration.Log('TComJavaObject.getAttributeValues', e);
    end;
  end;

  function TComJavaObject.DebugGetAttributeValues: string;
  begin
    var s:= 'DebugGetAttributeValues'#4 + FName;
    Result:= (FComJava as TComJava1).ExecuteCommand(s);
  end;

  function TComJavaObject.getAttributeNames: TStringList;
  begin
    if assigned(FClass) then
      Result:= FClass.getClassAttributeNames
    else begin
      Result:= TStringList.Create;
      FConfiguration.Log('TComJavaObject.getAttributeNames');
    end;
  end;

  function TComJavaObject.getRefreshedAttributeNames: TStringList;
  begin
    if assigned(FClass) then
      Result:= FClass.getRefreshedClassAttributeNames
    else begin
      Result:= TStringList.Create;
      FConfiguration.Log('TComJavaObject.getRefreshedAttributeNames');
    end;
  end;

  function TComJavaObject.getObjectAttributes: TStringList;
  begin
    if assigned(FClass) then
      Result:= FClass.getClassAttributes
    else begin
      Result:= TStringList.Create;
      FConfiguration.Log('TComJavaObject.getObjectAttributes');
    end;
  end;

  function TComJavaObject.getObjectAttributeValues(WithType: boolean): TStringlist;
  begin
    var s:= 'getObjectAttributeValues'#4 + FName;
    if WithType
      then s:= s + #4'withtype'
      else s:= s + #4;
    if FConfiguration.ArrayListAsIntegratedList
      then s:= s + #4'ArrayList'
      else s:= s + #4;
    s:= Right((FComJava as TComJava1).ExecuteCommand(s), 6);
    Result:= split(#4, s);
  end;

  function TComJavaObject.getObjectAttributeNames: TStringList;
  begin
    var s:= 'getObjectAttributeNames'#4 + FName;
    s:= Right((FComJava as TComJava1).ExecuteCommand(s), 6);
    Result:= split(#4, s);
  end;

  { --- TJavaParams ------------------------------------------------------------ }

  procedure TComJavaParams.addToParams(const Value, Sig: string);
  begin
    FParams:= FParams +  Value + '|';
    FSig   := FSig + Sig + '|';
  end;

{ --- TComJavaValue -----------------------------------------------------------}

  constructor TComJavaValue.create(const Value, Sig: string; ComJava: TObject);
  begin
    FComJava:= ComJava;
    FSig:= Sig;
    FKind:= SigToNumType(Sig);
    FValueAsString:= Value;
  end;

  constructor TComJavaValue.createFromString(const s: string; ComJava: TObject; const dummy: boolean = true);
    var i: integer; l: int64; d: double; s1, Suffix, withoutSuffix: string;
  begin
    // ntByte, ntShort not possible as Literals
    FComJava:= ComJava;
    FKind:= ntUnknown;
    FValueAsString:= s;
    s1:= LowerCase(s);
    Suffix:= Right(s1, -1);
    WithoutSuffix:= Left(s1, length(s1)-1);

    if s = 'null' then FKind:= ntNull
    else if (copy(s, 1, 1) = '"') and (Suffix = '"') then FKind:= ntString
    else if (copy(s, 1, 1) = '''') and (Suffix = '''') and (length(s) = 3) then FKind:= ntChar
    else if Suffix = 'l' then begin
      if TryStrToInt64(withoutSuffix, l) then FKind:= ntLong;
    end
    else if Suffix = 'f' then begin
      if TryStrToFloat(withoutSuffix, d) then FKind:= ntFloat;
    end
    else if Suffix = 'd' then begin
      if TryStrToFloat(withoutSuffix, d) then FKind:= ntDouble;
    end
    else if TryStrToInt(s1, i) then FKind:= ntInt
    else if TryStrToFloat(s1, d) then FKind:= ntDouble
    else if (s1 = 'true') or (s1 = 'false') then FKind:= ntBool;
    if FKind = ntUnknown then FKind:= ntObject;
  end;

  constructor TComJavaValue.create(Typ: string; ComJava: TObject);
  begin
    FComJava:= ComJava;
    FValueAsString:= '_x_';
    FSig:= TypToSig(Typ);
    delete(Typ, 1, LastDelimiter('.', Typ));
    FKind:= TypToNumType(Typ);
  end;

  function TComJavaValue.AsFormattedString: string;
  begin
    var s:= AsString;
    case FKind of
      ntChar:        s:= '''' + s + '''';
      ntStringArray: s:= 'new String[] ' + s;
    end;
    Result:= s;
  end;

  procedure TComJavaValue.setFromString(s: string);
    var r, s1: string; p: integer;
        aJavaObject: TComJavaObject;
  begin
    FValueAsString:= s;
    case FKind of
      ntStringArray: begin
        if s = 'null' then
          FValueAsString:= '{}'
        else begin
          r:= '';
          s:= copy(trim(s), 2, length(s) -2) + ',';
          p:= Pos(',', s);
          while p > 0 do begin
            s1:= trim(copy(s, 1, p-1));
            delete(s, 1, p);
            aJavaObject:= (FComJava as TComJava1).getObject(s1);
            if not assigned(aJavaObject) and not IsJavaString(s1)
              then s1:= '"' + s1 + '"';
            r:= r + ', ' + s1;
            p:= Pos(',', s);
          end;
          FValueAsString:= '{' + Right(r, 3) + '}';
        end;
      end;
      ntString: begin
        aJavaObject:= (FComJava as TComJava1).getObject(s);
        if not assigned(aJavaObject) and not IsJavaString(s)
          then s:= '"' + s + '"';
        FValueAsString:= s;
      end;
      ntChar: begin
        aJavaObject:= (FComJava as TComJava1).getObject(s);
        if not assigned(aJavaObject) and not IsJavaChar(s)
          then s:= '''' + s + '''';
        FValueAsString:= s;
      end;
    end;
  end;

  procedure TComJavaValue.setSig(const Sig: string);
  begin
    FKind:= SigToNumType(Sig);
    FSig:= Sig;
  end;

  function TComJavaValue.toString: string;
  begin
    var s:= (FComJava as TComJava1).ExecuteCommand('toString'#4 + FValueAsString);
    Result:= right(s, 5)
  end;

  function TComJavaValue.AsString: string;
    var s: string; i: integer; l: int64; d: double;
  begin
    if FValueAsString = '_x_' then
      case FKind of
        ntBool : FValueAsString:= 'false';
        ntByte : FValueAsString:= '0';
        ntChar : FValueAsstring:= '';
        ntShort: FValueAsString:= '0';
        ntInt  : FValueAsString:= '0';
        ntLong : FValueAsString:= '0';
        ntFloat: FValueAsString:= '0';
        ntDouble: FValueAsString:= '0';
        ntString: FValueAsString:= '';
        ntObject:    FValueAsString:= 'null';
        ntBoolArray: FValueAsString:= '{true, false}';
        ntByteArray: FValueAsString:= '{127, -127}';
        ntCharArray: FValueAsString:= '{A, B}';
        ntShortArray:FValueAsString:= '{0, 1}';
        ntIntArray:  FValueAsString:= '{0, 1}';
        ntLongArray: FValueAsString:= '{0, 1}';
        ntFloatArray: FValueAsString:= '{0.0, 1.0}';
        ntDoubleArray: FValueAsString:= '{0.0, 1.0}';
        ntStringArray: FValueAsString:= 'null';
        ntObjectArray: FValueAsString:= 'null';
      else
        FValueAsString:= 'null';
      end
    else begin
      s:= FValueAsString;
      case FKind of
        ntBool: begin
          s:= LowerCase(s);
          if (s <> 'true') and (s <> 'false') then s:= 'false';
        end;
        ntChar: begin
          if (Pos('''', s) = 1) and (length(s) > 1)
            then s:= copy(s, 2, 1)
            else s:= copy(s, 1, 1);
          //if Left(s, 1) = '\' then s:= Left(s, 2) else s:= Left(s, 1);
        end;
        ntByte: begin
          while length(s) > 0 do begin
            if TryStrToInt(s, i) and (-128 <= i) and (i <= 127) then break;
            s:= Right(s, -1);
          end;
          if s = '' then s:= '0';
        end;
        ntShort: begin
          while length(s) > 0 do begin
            if TryStrToInt(s, i) and (-32768 <= i) and (i <= 32767) then break;
            s:= Left(s, length(s)-1);
          end;
          if s = '' then s:= '0';
        end;
        ntInt: begin
          while length(s) > 0 do begin
            if TryStrToInt(s, i) then break;
            s:= Left(s, length(s)-1);
          end;
          if s = '' then s:= '0';
        end;
        ntLong: begin
          while length(s) > 0 do begin
            if TryStrToInt64(s, l) then break;
            s:= Left(s, length(s)-1);
          end;
          if s = '' then s:= '0';
        end;
        ntFloat,
        ntDouble: begin
          while length(s) > 0 do begin
            if TryStrToFloat(s, d) then break;
            s:= Left(s, length(s)-1);
          end;
          s:= ReplaceStr(s, ',', '.');
          if s = '' then s:= '0';
        end;
      end;
      FValueAsString:= s;
    end;
    Result:= FValueAsString;
  end;

  function TComJavaValue.AsStringWithout(c: char): string;
    var s: string;
  begin
    s:= AsString;
    if (length(s) > 0) and (s[1] = c)
      then Result:= copy(s, 2, length(s)-2)
      else Result:= s;
  end;

{ --- TJavaField ------------------------------------------------------------- }

  constructor TComJavaAttribute.Create(cls: TComJavaClass; ComJava: TObject);
  begin
    FComJava:= ComJava; 
    FClass:= cls;
  end;

  function TComJavaAttribute.GetAttributeValue(JObj: TComJavaObject; const Attributename, Typename: string; isStatic: boolean): string;
    const _static = 's';
    var s, s1: string;
  begin
    s:= 'getAttributeValue'#4 + FClass.FName+ #4 + JObj.FName + #4 + Attributename + #4 + TypToSig(Typename) + #4;
    if IsStatic then s:= s + _static;
    s1:= (FComJava as TComJava1).ExecuteCommand(s);
    if (Left(s1, 3) = '+OK') then
      Result:= copy(s1, 5, length(s1))
    else begin
      FConfiguration.Log('TComJavaAttribute.GetAttributeValue: ' + s);
      Result:= '<error>';
    end;
  end;

  procedure TComJavaAttribute.SetAttributeValue(JObj: TComJavaObject; const Attributename, Typename, Value: string; IsStatic: boolean);
    const _static = 's';
  begin
    var s:= 'setAttributeValue'#4 + FClass.FName + #4 + JObj.FName + #4 + Attributename + #4 + TypToSig(Typename) + #4 + Value + #4;
    if IsStatic then s:= s + 's';
    s:= (FComJava as TComJava1).ExecuteCommand(s);
  end;

{ --- TJavaMethod ------------------------------------------------------------ }

  constructor TComJavaMethod.Create(
                                 cls: TComJavaClass;
                                 const aMethodname: string;
                                 methodType: TMethodAttribute;
                                 const theReturnType: string;
                                 params: TComJavaParams;
                                 ComJava: TObject);
    var aRetClass: TComJavaClass;
  begin
    FComJava:= ComJava;
    FClass:= cls;
    FMethodname:= aMethodname;
    FMethodType:= MethodType;
    FReturnType:= withoutGeneric(theReturnType);
    FParams:= Params;
    if params = nil
      then FSig:= '()'
      else FSig:= '(' + params.signature + ')';

    FRetVal:= TypToNumType(FReturnType);
    if FRetVal = ntObject  // TODO ntObjectArray?
      then aRetClass:= (FComJava as TComJava1).GetClass(FReturnType)
      else aRetClass:= nil;
    if assigned(aRetClass)
      then FSig:= FSig + NumTypeToSig(FRetVal, aRetClass.Signature)
      else FSig:= FSig + NumTypeToSig(FRetVal, '');
  end;

  function TComJavaMethod.Call(jobj: TComJavaObject): TComJavaValue;
    var s, s1: string; 
  begin
    Result:= nil;
    if assigned(jobj)
      then s:= 'callMethod'#4 + Fclass.Name + #4 + jobj.Name + #4
      else s:= 'callMethod'#4 + Fclass.Name + #4#4;
    s:= s + FMethodname + #4 + FReturnType +#4 + FSig + #4;
    if assigned(FParams) then s:= s + FParams.FParams;
    s:= s + #4 + IntToStr(Ord(FMethodType));
    s:= (FComJava as TComJava1).ExecuteCommand(s);
    FValid:= (Left(s, 3) = '+OK');
    if FValid
      then Result:= TComJavaValue.create(right(s, 5), right(FSig, Pos(')', Fsig) + 1), FComJava)
    else begin
      if assigned(jobj)
        then s1:= jobj.Name + '.'
        else s1:= '';
      if Pos('Java terminated', s) + Pos('Java disconnected', s) = 0  then
        FError:= Right(s, 6);
    end;
  end;

  function TComJavaMethod.getExpressionValue: TComJavaValue;
  begin
    Result:= nil;
    var s:= (FComJava as TComJava1).ExecuteCommand('getExpressionValue'#4 + Fclass.Name);
    FValid:= (Left(s, 3) = '+OK');
    if FValid then begin
      s:= Right(s, 5);
      var p:= Pos('_|_', s);
      s:= Left(s, p-1);
      Result:= TComJavaValue.create(s, right(FSig, Pos(')', FSig) + 1), FComJava);
    end else
      if Pos('Java terminated', s) + Pos('Java disconnected', s) = 0 then
        FError:= Right(s, 6);
  end;

  function TComJavaMethod.getExpressionType: string;
  begin
    Result:= '';
    var s:= (FComJava as TComJava1).ExecuteCommand('getExpressionValue'#4 + Fclass.Name);
    FValid:= (Left(s, 3) = '+OK');
    if FValid then begin
      s:= Right(s, 5);
      var p:= pos('_|_', s);
      s:= Right(s, p + 3);
      Result:= s;
    end else
      if Pos('Java terminated', s) + Pos('Java disconnected', s) = 0 then
        FError:= Right(s, 6);
  end;

end.

