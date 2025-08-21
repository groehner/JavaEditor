unit UJniWrapper1;

interface

uses
  Classes,
  jni;

type

  TComJavaClass = class;
  TComJavaObject = class;

  TComJavaValue = class
  private
    FComJava: TObject;
    FValueAsString: string;
    FKind: TNumType;
    FSig: string;
    procedure SetSig(const Sig: string);
  public
    constructor Create(Typ: string; ComJava: TObject); overload;
    // duplicate dummy due to '%s' with identical parameters; Cannot access from C++ (Delphi)
    constructor CreateFromString(const Str: string; ComJava: TObject);
    constructor Create(const Value, Sig: string; ComJava: TObject); overload;

    function AsFormattedString: string;
    procedure SetFromString(Str: string);
    function ToString: string; override;
    function AsString: string;
    function AsStringWithout(Chr: Char): string;
    property Kind: TNumType read FKind write FKind;
    property Sig: string read FSig write SetSig;
    property Value: string read FValueAsString write FValueAsString;
  end;

  TComJavaParams = class
  private
    FParams: string;
    FSig: string;
  public
    procedure AddToParams(const Value, Sig: string);
    property Signature: string read FSig;
    property Params: string read FParams;
  end;

  TComJavaAttribute = class
  private
    FComJava: TObject;
    FClass: TComJavaClass;
  public
    constructor Create(Cls: TComJavaClass; ComJava: TObject);
    function GetAttributeValue(JObj: TComJavaObject;
      const Attributename, Typename: string; IsStatic: Boolean): string;
    procedure SetAttributeValue(JObj: TComJavaObject;
      const Attributename, Typename, Value: string; IsStatic: Boolean);
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
    FGetSuperclass: Boolean;
    function GetTyp: string;
    function GetImportTyp: string;
    function GetGenericName: string;
  public
    constructor CreateClass(const Name: string; ComJava: TObject);
    constructor CreateLoadClass(const Name, Path: string; ComJava: TObject);
    // duplicate dummy due to '%s' with identical parameters; Cannot access from C++ (Delphi)
    constructor CreateWithHandle(const AClassname: string; ComJava: TObject);
    class function IsInterface(const Interfacename: string;
      ComJava: TObject): Boolean;
    class function FindClass(const AClassname: string;
      ComJava: TObject): Boolean;
    destructor Destroy; override;
    procedure Init;
    function GetConstructors: TStringList;
    function GetMethods(const AStatic: string): TStringList;
    function GetClassAttributes: TStringList;
    function GetClassAttributeNames: TStringList;
    function GetRefreshedClassAttributeNames: TStringList;
    function GetSuperclassName: string;
    function GetGenericTyp: string;
    property Signature: string read FSig write FSig;
    property Typ: string read GetTyp;
    property ImportTyp: string read GetImportTyp;
    property IsValid: Boolean read FValid;
    property Pathname: string read FPathname write FPathname;
    property Name: string read FName;
    property Error: string read FError;
    property GenericName: string read GetGenericName write FGenericName;
    property Generic: string read FGeneric write FGeneric;
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
    constructor Create(const Objectname: string; Jcl: TComJavaClass;
      ComParams: TComJavaParams; ComJava: TObject);
    constructor CreateByName(const Objectname: string; Jcl: TComJavaClass;
      ComJava: TObject);
    constructor CreateNewUnnamedObject(const Objectname: string;
      ComJava: TObject);
    destructor Destroy; override;
    procedure Delete;
    function ToString: string; override;
    function GetObjectAttributes: TStringList;
    function GetAttributeValues: TStringList;
    function DebugGetAttributeValues: string;
    function GetAttributeNames: TStringList;
    function GetRefreshedAttributeNames: TStringList;
    function GetObjectAttributeValues(WithType: Boolean): TStringList;
    function GetObjectAttributeNames: TStringList;
    property ClassRef: TComJavaClass read FClass;
    property IsValid: Boolean read FValid;
    property Name: string read FName;
    property Error: string read FError;
    property CreateValue: string read FCreateValue;
    property AttributeValues: TStringList read FAttributeValues
      write FAttributeValues;
  end;

  TComJavaMethod = class
  private
    FComJava: TObject;
    FClass: TComJavaClass;
    FMethodname: string;
    FMethodType: TMethodAttribute;
    FParams: TComJavaParams;
    FSig: string;
    FRetVal: TNumType;
    FReturnType: string;
    FValid: Boolean;
    FError: string;
  public
    constructor Create(Cls: TComJavaClass; const AMethodname: string;
      MethodType: TMethodAttribute; const AReturnType: string;
      Params: TComJavaParams; ComJava: TObject);
    function Call(JObj: TComJavaObject): TComJavaValue;
    function GetExpressionValue: TComJavaValue;
    function GetExpressionType: string;
    property IsValid: Boolean read FValid;
    property Error: string read FError;
    property AMethodname: string read FMethodname;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  UUtils,
  UConfiguration,
  UComJava1;

function TryStrToFloat(var Str: string; var AFloat: Double): Boolean;
var
  Posi: Integer;
  Error: Boolean;

  function StrToFloat(const Str: string): Extended;
  var
    Extd: Extended;
  begin
    if TextToFloat(PChar(Str), Extd, fvExtended) then
      Result := Extd
    else
    begin
      Result := 0;
      Error := True;
    end;
  end;

begin
  Error := False;
  Posi := Pos('.', Str);
  if (Posi > 0) and (FormatSettings.DecimalSeparator <> '.') then
    Str[Posi] := FormatSettings.DecimalSeparator;
  Posi := Pos(',', Str);
  if (Posi > 0) and (FormatSettings.DecimalSeparator <> ',') then
    Str[Posi] := FormatSettings.DecimalSeparator;
  AFloat := StrToFloat(Str);
  Result := not Error;
end;

function TypToSig(Typ: string): string;
var
  Str, Dimension: string;
  Posi: Integer;
begin
  Result := '';
  Dimension := '';
  Posi := Pos('[]', Typ);
  while Posi > 0 do
  begin
    Dimension := Dimension + '[';
    Delete(Typ, Posi, 2);
    Posi := Pos('[]', Typ);
  end;

  if Typ = 'String' then
    Result := 'Ljava/lang/String;'
  else if Typ = 'java.lang.String' then
    Result := 'Ljava/lang/String;'
  else if Typ = 'boolean' then
    Result := 'Z'
  else if Typ = 'char' then
    Result := 'C'
  else if Typ = 'byte' then
    Result := 'B'
  else if Typ = 'short' then
    Result := 'S'
  else if Typ = 'int' then
    Result := 'I'
  else if Typ = 'long' then
    Result := 'J'
  else if Typ = 'float' then
    Result := 'F'
  else if Typ = 'double' then
    Result := 'D'
  else if Typ = 'void' then
    Result := 'V'
  else
  begin
    Typ := WithoutGeneric(Typ); // z. B. LinkedList<pupil>
    Str := GetComJava.GetSignature(Typ);
    // java/util/ArrayList   // TODO
    if Str = '' then
      if Length(Typ) = 1 // generic
      then
        Result := 'Ljava/lang/Object;'
      else
        Result := 'L' + ReplaceStr(Typ, '.', '/') + ';'
    else
      Result := Str;
  end;
  Result := Dimension + Result;
end;

function ClassTypToSig(Typ: string): string;
begin
  var
  Dimension := '';
  var
  Posi := Pos('[]', Typ);
  while Posi > 0 do
  begin
    Dimension := Dimension + '[';
    Delete(Typ, Posi, 2);
    Posi := Pos('[]', Typ);
  end;
  Typ := WithoutGeneric(Typ); // z. B. LinkedList<pupil>
  Result := Dimension + 'L' + ReplaceStr(Typ, '.', '/') + ';';
end;

function SigToClassTyp(Sig: string): string;
begin
  Delete(Sig, Length(Sig), 1);
  while Sig[1] = '[' do
  begin
    Delete(Sig, 1, 1);
    Sig := Sig + '[]';
  end;
  Delete(Sig, 1, 1); // L
  Result := Sig;
end;

procedure TComJavaClass.Init;
begin
  FConstructors := nil;
  FAttributes := nil;
  FClassAttributeNames := nil;
  FStaticMethods := nil;
  FNotStaticMethods := nil;
  FAllMethods := nil;
  FGetSuperclass := False;
end;

constructor TComJavaClass.CreateClass(const Name: string; ComJava: TObject);
begin
  FPathname := FName;
  FComJava := ComJava;
  FName := WithoutGeneric(Name);
  FSig := ClassTypToSig(FName);
  var
  Str := (FComJava as TComJava1).ExecuteCommand('createClass'#4 + FName);
  FValid := (Left(Str, 4) <> '-ERR');
  FError := Right(Str, 6);
  Init;
end;

constructor TComJavaClass.CreateLoadClass(const Name, Path: string;
  ComJava: TObject);
begin
  FPathname := Path;
  FComJava := ComJava;
  FName := WithoutGeneric(Name);
  FSig := ClassTypToSig(FName);
  var
  Str := (FComJava as TComJava1).ExecuteCommand('loadClass'#4 + FName +
    #4 + Path);
  FValid := (Left(Str, 4) <> '-ERR');
  FError := Right(Str, 6);
  Init;
end;

constructor TComJavaClass.CreateWithHandle(const AClassname: string;
  ComJava: TObject);
begin
  FPathname := '';
  FComJava := ComJava;
  FName := WithoutGeneric(AClassname);
  FSig := ClassTypToSig(FName);
  FValid := True;
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
  inherited;
end;

class function TComJavaClass.IsInterface(const Interfacename: string;
  ComJava: TObject): Boolean;
begin
  var
  Str := (ComJava as TComJava1).ExecuteCommand('isInterface'#4 + Interfacename);
  Result := (Left(Str, 3) = '+OK');
end;

class function TComJavaClass.FindClass(const AClassname: string;
  ComJava: TObject): Boolean;
begin
  if (AClassname = 'T') or (AClassname = 'E') then
    Result := False
  else
  begin
    var
    Str := (ComJava as TComJava1).ExecuteCommandWithoutDebugger
      ('findClass'#4 + AClassname);
    Result := (Left(Str, 3) = '+OK');
  end;
end;

function TComJavaClass.GetTyp: string;
begin
  var
  Str := FSig;
  var
  Posi := Pos('/', Str);
  while Posi > 0 do
  begin
    Delete(Str, 1, Posi);
    Posi := Pos('/', Str);
  end;
  Result := Str;
end;

function TComJavaClass.GetGenericTyp: string;
begin
  Result := SigToClassTyp(FSig);
  if Generic <> '' then
    Result := Result + '<' + Generic + '>';
end;

function TComJavaClass.GetImportTyp: string;
begin
  Result := ReplaceStr(FName, '/', '.');
end;

function TComJavaClass.GetGenericName: string;
begin
  if FGenericName = '' then
    Result := FName
  else
    Result := FGenericName;
end;

function TComJavaClass.GetSuperclassName: string;
begin
  if not FGetSuperclass then
  begin
    FSuperclass := (FComJava as TComJava1)
      .ExecuteCommand('getSuperClass'#4 + FName);
    if Pos('+OK ', FSuperclass) = 1 then
      Delete(FSuperclass, 1, 4)
    else
      FSuperclass := '';
    FGetSuperclass := True;
  end;
  Result := FSuperclass;
end;

function TComJavaClass.GetConstructors: TStringList;
begin
  if not Assigned(FConstructors) then
  begin
    var
    Str := (FComJava as TComJava1).ExecuteCommand('getConstructors'#4 + FName);
    if Pos('+OK ', Str) = 1 then
      Delete(Str, 1, 6)
    else
      Str := '';
    FConstructors := TStringList.Create;
    FConstructors.Text := Str;
  end;
  Result := FConstructors;
end;

function TComJavaClass.GetClassAttributes: TStringList;
begin
  if not Assigned(FAttributes) then
  begin
    var
    Str := (FComJava as TComJava1).ExecuteCommand('getAttributes'#4 + FName);
    if Pos('+OK', Str) = 1 then
      Delete(Str, 1, 5)
    else
      Str := '';
    FAttributes := Split(#4, Str);
  end;
  Result := FAttributes;
end;

function TComJavaClass.GetClassAttributeNames: TStringList;
begin
  if not Assigned(FClassAttributeNames) then
  begin
    var
    Str := (FComJava as TComJava1).ExecuteCommand('getAttributeNames'#4
      + FName);
    if Pos('+OK', Str) = 1 then
      Delete(Str, 1, 5)
    else
      Str := '';
    FClassAttributeNames := Split(#4, Str);
    if Str = '' then
      FClassAttributeNames.Text := '';
  end;
  Result := FClassAttributeNames;
end;

function TComJavaClass.GetRefreshedClassAttributeNames: TStringList;
begin
  FreeAndNil(FClassAttributeNames);
  var
  Str := (FComJava as TComJava1).ExecuteCommand
    ('getRefreshedAttributeNames'#4 + FName);
  if Pos('+OK', Str) = 1 then
    Delete(Str, 1, 5)
  else
    Str := '';
  FClassAttributeNames := Split(#4, Str);
  if Str = '' then
    FClassAttributeNames.Text := '';
  Result := FClassAttributeNames;
end;

function TComJavaClass.GetMethods(const AStatic: string): TStringList;

  function GetMethods_(const AStatic: string): string;
  begin
    var
    Str := (FComJava as TComJava1).ExecuteCommand('getMethods'#4 + FName + #4
      + AStatic);
    if Pos('+OK ', Str) = 1 then
      Delete(Str, 1, 6)
    else
      Str := '';
    Result := Str;
  end;

begin
  if AStatic = 'static' then
  begin
    if not Assigned(FStaticMethods) then
    begin
      FStaticMethods := TStringList.Create;
      FStaticMethods.Text := GetMethods_(AStatic);
    end;
    Result := FStaticMethods;
  end
  else if AStatic = 'not static' then
  begin
    if not Assigned(FNotStaticMethods) then
    begin
      FNotStaticMethods := TStringList.Create;
      FNotStaticMethods.Text := GetMethods_(AStatic);
    end;
    Result := FNotStaticMethods;
  end
  else
  begin
    if not Assigned(FAllMethods) then
    begin
      FAllMethods := TStringList.Create;
      FAllMethods.Text := GetMethods_(AStatic);
    end;
    Result := FAllMethods;
  end;
end;

{ --- TJavaObject ------------------------------------------------------------ }

constructor TComJavaObject.Create(const Objectname: string; Jcl: TComJavaClass;
  ComParams: TComJavaParams; ComJava: TObject);
var
  Signature, Params, Str: string;
begin
  FComJava := ComJava;
  FName := Objectname;
  FAttributeValues := nil;
  FClass := Jcl;
  FError := '';
  FCreateValue := '';
  Signature := '';
  if not Assigned(ComParams) then
  begin
    Signature := '()V';
    Params := '';
  end
  else
  begin
    Signature := '(' + ComParams.Signature + ')V';
    Params := ComParams.Params;
  end;
  Str := (FComJava as TComJava1).ExecuteCommand('createObject'#4 + Jcl.Name + #4
    + Objectname + #4 + Signature + #4 + Params);
  FValid := (Left(Str, 3) = '+OK');
  if FValid then
    FCreateValue := Right(Str, 5)
  else
    FError := Right(Str, 6);
end;

constructor TComJavaObject.CreateByName(const Objectname: string;
  Jcl: TComJavaClass; ComJava: TObject);
begin
  FComJava := ComJava;
  FName := Objectname;
  FAttributeValues := nil;
  FClass := Jcl;
  FValid := True;
end;

constructor TComJavaObject.CreateNewUnnamedObject(const Objectname: string;
  ComJava: TObject);
var
  Str, AClassname: string;
  Int, Posi: Integer;
  ClassList: TStringList;
begin
  FComJava := ComJava;
  FName := Objectname;
  FAttributeValues := nil;
  Str := (FComJava as TComJava1).ExecuteCommand('getClassOf'#4 + Objectname);
  FValid := (Left(Str, 3) = '+OK');
  if FValid then
  begin
    Str := Right(Str, 5);
    Posi := Pos('|', Str);
    FCreateValue := Copy(Str, Posi + 1, Length(Str));
    AClassname := Copy(Str, 1, Posi - 1);
    ClassList := TComJava1(ComJava).ClassList;
    Int := ClassList.IndexOf(AClassname);
    if Int = -1 then
    begin
      FClass := TComJavaClass.CreateWithHandle(AClassname, FComJava);
      ClassList.AddObject(AClassname, FClass);
    end
    else
      FClass := TComJavaClass(ClassList.Objects[Int]);
    FError := '';
  end
  else
    FError := Right(Str, 6);
end;

destructor TComJavaObject.Destroy;
begin
  FreeAndNil(FAttributeValues);
  inherited;
end;

procedure TComJavaObject.Delete;
begin
  if not(FComJava as TComJava1).Terminated then
    (FComJava as TComJava1).ExecuteCommand('deleteObject'#4 + FName);
end;

function TComJavaObject.ToString: string;
begin
  var
  Str := (FComJava as TComJava1).ExecuteCommand('toString'#4 + FName);
  Result := Right(Str, 5);
end;

function TComJavaObject.GetAttributeValues: TStringList;
var
  Str1, Str: string;
begin
  Result := nil;
  try
    Str1 := 'getAttributeValues'#4 + FName;
    if FConfiguration.ArrayListAsIntegratedList then
      Str1 := Str1 + #4 + 'ArrayList'
    else
      Str1 := Str1 + #4;
    if FComJava is TComJava1 then
      Str := (FComJava as TComJava1).ExecuteCommand(Str1);
    if Pos('+OK ', Str) = 1 then
    begin
      Str := Right(Str, 6);
      FreeAndNil(FAttributeValues);
      FAttributeValues := Split(#4, Str);
      if FAttributeValues.Count > 0 then
      begin
        FCreateValue := FAttributeValues[FAttributeValues.Count - 1];
        FAttributeValues.Delete(FAttributeValues.Count - 1);
      end;
      Result := FAttributeValues;
    end
    else
    begin
      FConfiguration.Log('TComJavaObject.getAttributeValues: ' + Str1 +
        ' - ' + Str);
      Result := TStringList.Create;
      Result.Add(Str);
    end;
  except
    on E: Exception do
      FConfiguration.Log('TComJavaObject.getAttributeValues', E);
  end;
end;

function TComJavaObject.DebugGetAttributeValues: string;
begin
  var
  Str := 'DebugGetAttributeValues'#4 + FName;
  Result := (FComJava as TComJava1).ExecuteCommand(Str);
end;

function TComJavaObject.GetAttributeNames: TStringList;
begin
  if Assigned(FClass) then
    Result := FClass.GetClassAttributeNames
  else
  begin
    Result := TStringList.Create;
    FConfiguration.Log('TComJavaObject.getAttributeNames');
  end;
end;

function TComJavaObject.GetRefreshedAttributeNames: TStringList;
begin
  if Assigned(FClass) then
    Result := FClass.GetRefreshedClassAttributeNames
  else
  begin
    Result := TStringList.Create;
    FConfiguration.Log('TComJavaObject.getRefreshedAttributeNames');
  end;
end;

function TComJavaObject.GetObjectAttributes: TStringList;
begin
  if Assigned(FClass) then
    Result := FClass.GetClassAttributes
  else
  begin
    Result := TStringList.Create;
    FConfiguration.Log('TComJavaObject.getObjectAttributes');
  end;
end;

function TComJavaObject.GetObjectAttributeValues(WithType: Boolean)
  : TStringList;
begin
  var
  Str := 'getObjectAttributeValues'#4 + FName;
  if WithType then
    Str := Str + #4'WithType'
  else
    Str := Str + #4;
  if FConfiguration.ArrayListAsIntegratedList then
    Str := Str + #4'ArrayList'
  else
    Str := Str + #4;
  Str := Right((FComJava as TComJava1).ExecuteCommand(Str), 6);
  Result := Split(#4, Str);
end;

function TComJavaObject.GetObjectAttributeNames: TStringList;
begin
  var
  Str := 'getObjectAttributeNames'#4 + FName;
  Str := Right((FComJava as TComJava1).ExecuteCommand(Str), 6);
  Result := Split(#4, Str);
end;

{ --- TJavaParams ------------------------------------------------------------ }

procedure TComJavaParams.AddToParams(const Value, Sig: string);
begin
  FParams := FParams + Value + '|';
  FSig := FSig + Sig + '|';
end;

{ --- TComJavaValue ----------------------------------------------------------- }

constructor TComJavaValue.Create(const Value, Sig: string; ComJava: TObject);
begin
  FComJava := ComJava;
  FSig := Sig;
  FKind := SigToNumType(Sig);
  FValueAsString := Value;
end;

constructor TComJavaValue.CreateFromString(const Str: string; ComJava: TObject);
var
  Int: Integer;
  BigInt: Int64;
  ADouble: Double;
  Str1, Suffix, WithoutSuffix: string;
begin
  // ntByte, ntShort not possible as Literals
  FComJava := ComJava;
  FKind := ntUnknown;
  FValueAsString := Str;
  Str1 := LowerCase(Str);
  Suffix := Right(Str1, -1);
  WithoutSuffix := Left(Str1, Length(Str1) - 1);

  if Str = 'null' then
    FKind := ntNull
  else if (Copy(Str, 1, 1) = '"') and (Suffix = '"') then
    FKind := ntString
  else if (Copy(Str, 1, 1) = '''') and (Suffix = '''') and (Length(Str) = 3)
  then
    FKind := ntChar
  else if Suffix = 'BigInt' then
  begin
    if TryStrToInt64(WithoutSuffix, BigInt) then
      FKind := ntLong;
  end
  else if Suffix = 'f' then
  begin
    if TryStrToFloat(WithoutSuffix, ADouble) then
      FKind := ntFloat;
  end
  else if Suffix = 'd' then
  begin
    if TryStrToFloat(WithoutSuffix, ADouble) then
      FKind := ntDouble;
  end
  else if TryStrToInt(Str1, Int) then
    FKind := ntInt
  else if TryStrToFloat(Str1, ADouble) then
    FKind := ntDouble
  else if (Str1 = 'true') or (Str1 = 'false') then
    FKind := ntBool;
  if FKind = ntUnknown then
    FKind := ntObject;
end;

constructor TComJavaValue.Create(Typ: string; ComJava: TObject);
begin
  FComJava := ComJava;
  FValueAsString := '_x_';
  FSig := TypToSig(Typ);
  Delete(Typ, 1, LastDelimiter('.', Typ));
  FKind := TypToNumType(Typ);
end;

function TComJavaValue.AsFormattedString: string;
begin
  var
  Str := AsString;
  case FKind of
    ntChar:
      Str := '''' + Str + '''';
    ntStringArray:
      Str := 'new String[] ' + Str;
  end;
  Result := Str;
end;

procedure TComJavaValue.SetFromString(Str: string);
var
  StrArr, Str1: string;
  Posi: Integer;
  JavaObject: TComJavaObject;
begin
  FValueAsString := Str;
  case FKind of
    ntStringArray:
      begin
        if Str = 'null' then
          FValueAsString := '{}'
        else
        begin
          StrArr := '';
          Str := Copy(Trim(Str), 2, Length(Str) - 2) + ',';
          Posi := Pos(',', Str);
          while Posi > 0 do
          begin
            Str1 := Trim(Copy(Str, 1, Posi - 1));
            Delete(Str, 1, Posi);
            JavaObject := (FComJava as TComJava1).GetObject(Str1);
            if not Assigned(JavaObject) and not IsJavaString(Str1) then
              Str1 := '"' + Str1 + '"';
            StrArr := StrArr + ', ' + Str1;
            Posi := Pos(',', Str);
          end;
          FValueAsString := '{' + Right(StrArr, 3) + '}';
        end;
      end;
    ntString:
      begin
        JavaObject := (FComJava as TComJava1).GetObject(Str);
        if not Assigned(JavaObject) and not IsJavaString(Str) then
          Str := '"' + Str + '"';
        FValueAsString := Str;
      end;
    ntChar:
      begin
        JavaObject := (FComJava as TComJava1).GetObject(Str);
        if not Assigned(JavaObject) and not IsJavaChar(Str) then
          Str := '''' + Str + '''';
        FValueAsString := Str;
      end;
  end;
end;

procedure TComJavaValue.SetSig(const Sig: string);
begin
  FKind := SigToNumType(Sig);
  FSig := Sig;
end;

function TComJavaValue.ToString: string;
begin
  var
  Str := (FComJava as TComJava1).ExecuteCommand('toString'#4 + FValueAsString);
  Result := Right(Str, 5);
end;

function TComJavaValue.AsString: string;
var
  Str: string;
  Int: Integer;
  BigInt: Int64;
  ADouble: Double;
begin
  if FValueAsString = '_x_' then
    case FKind of
      ntBool:
        FValueAsString := 'false';
      ntByte:
        FValueAsString := '0';
      ntChar:
        FValueAsString := '';
      ntShort:
        FValueAsString := '0';
      ntInt:
        FValueAsString := '0';
      ntLong:
        FValueAsString := '0';
      ntFloat:
        FValueAsString := '0';
      ntDouble:
        FValueAsString := '0';
      ntString:
        FValueAsString := '';
      ntObject:
        FValueAsString := 'null';
      ntBoolArray:
        FValueAsString := '{true, false}';
      ntByteArray:
        FValueAsString := '{127, -127}';
      ntCharArray:
        FValueAsString := '{A, B}';
      ntShortArray:
        FValueAsString := '{0, 1}';
      ntIntArray:
        FValueAsString := '{0, 1}';
      ntLongArray:
        FValueAsString := '{0, 1}';
      ntFloatArray:
        FValueAsString := '{0.0, 1.0}';
      ntDoubleArray:
        FValueAsString := '{0.0, 1.0}';
      ntStringArray:
        FValueAsString := 'null';
      ntObjectArray:
        FValueAsString := 'null';
    else
      FValueAsString := 'null';
    end
  else
  begin
    Str := FValueAsString;
    case FKind of
      ntBool:
        begin
          Str := LowerCase(Str);
          if (Str <> 'true') and (Str <> 'false') then
            Str := 'false';
        end;
      ntChar:
        begin
          if (Pos('''', Str) = 1) and (Length(Str) > 1) then
            Str := Copy(Str, 2, 1)
          else
            Str := Copy(Str, 1, 1);
        end;
      ntByte:
        begin
          while Length(Str) > 0 do
          begin
            if TryStrToInt(Str, Int) and (-128 <= Int) and (Int <= 127) then
              Break;
            Str := Right(Str, -1);
          end;
          if Str = '' then
            Str := '0';
        end;
      ntShort:
        begin
          while Length(Str) > 0 do
          begin
            if TryStrToInt(Str, Int) and (-32768 <= Int) and (Int <= 32767) then
              Break;
            Str := Left(Str, Length(Str) - 1);
          end;
          if Str = '' then
            Str := '0';
        end;
      ntInt:
        begin
          while Length(Str) > 0 do
          begin
            if TryStrToInt(Str, Int) then
              Break;
            Str := Left(Str, Length(Str) - 1);
          end;
          if Str = '' then
            Str := '0';
        end;
      ntLong:
        begin
          while Length(Str) > 0 do
          begin
            if TryStrToInt64(Str, BigInt) then
              Break;
            Str := Left(Str, Length(Str) - 1);
          end;
          if Str = '' then
            Str := '0';
        end;
      ntFloat, ntDouble:
        begin
          while Length(Str) > 0 do
          begin
            if TryStrToFloat(Str, ADouble) then
              Break;
            Str := Left(Str, Length(Str) - 1);
          end;
          Str := ReplaceStr(Str, ',', '.');
          if Str = '' then
            Str := '0';
        end;
    end;
    FValueAsString := Str;
  end;
  Result := FValueAsString;
end;

function TComJavaValue.AsStringWithout(Chr: Char): string;
var
  Str: string;
begin
  Str := AsString;
  if (Length(Str) > 0) and (Str[1] = Chr) then
    Result := Copy(Str, 2, Length(Str) - 2)
  else
    Result := Str;
end;

{ --- TJavaField ------------------------------------------------------------- }

constructor TComJavaAttribute.Create(Cls: TComJavaClass; ComJava: TObject);
begin
  FComJava := ComJava;
  FClass := Cls;
end;

function TComJavaAttribute.GetAttributeValue(JObj: TComJavaObject;
  const Attributename, Typename: string; IsStatic: Boolean): string;
const
  CStatic = 's';
var
  Str, Str1: string;
begin
  Str := 'getAttributeValue'#4 + FClass.FName + #4 + JObj.FName + #4 +
    Attributename + #4 + TypToSig(Typename) + #4;
  if IsStatic then
    Str := Str + CStatic;
  Str1 := (FComJava as TComJava1).ExecuteCommand(Str);
  if (Left(Str1, 3) = '+OK') then
    Result := Copy(Str1, 5, Length(Str1))
  else
  begin
    FConfiguration.Log('TComJavaAttribute.GetAttributeValue: ' + Str);
    Result := '<error>';
  end;
end;

procedure TComJavaAttribute.SetAttributeValue(JObj: TComJavaObject;
  const Attributename, Typename, Value: string; IsStatic: Boolean);
begin
  var
  Str := 'setAttributeValue'#4 + FClass.FName + #4 + JObj.FName + #4 +
    Attributename + #4 + TypToSig(Typename) + #4 + Value + #4;
  if IsStatic then
    Str := Str + 's';
  Str := (FComJava as TComJava1).ExecuteCommand(Str);
end;

{ --- TJavaMethod ------------------------------------------------------------ }

constructor TComJavaMethod.Create(Cls: TComJavaClass; const AMethodname: string;
  MethodType: TMethodAttribute; const AReturnType: string;
  Params: TComJavaParams; ComJava: TObject);
var
  RetClass: TComJavaClass;
begin
  FComJava := ComJava;
  FClass := Cls;
  FMethodname := AMethodname;
  FMethodType := MethodType;
  FReturnType := WithoutGeneric(AReturnType);
  FParams := Params;
  if not Assigned(Params) then
    FSig := '()'
  else
    FSig := '(' + Params.Signature + ')';

  FRetVal := TypToNumType(FReturnType);
  if FRetVal = ntObject // TODO ntObjectArray?
  then
    RetClass := (FComJava as TComJava1).GetClass(FReturnType)
  else
    RetClass := nil;
  if Assigned(RetClass) then
    FSig := FSig + NumTypeToSig(FRetVal, RetClass.Signature)
  else
    FSig := FSig + NumTypeToSig(FRetVal, '');
end;

function TComJavaMethod.Call(JObj: TComJavaObject): TComJavaValue;
var
  Str: string;
begin
  Result := nil;
  if Assigned(JObj) then
    Str := 'callMethod'#4 + FClass.Name + #4 + JObj.Name + #4
  else
    Str := 'callMethod'#4 + FClass.Name + #4#4;
  Str := Str + FMethodname + #4 + FReturnType + #4 + FSig + #4;
  if Assigned(FParams) then
    Str := Str + FParams.FParams;
  Str := Str + #4 + IntToStr(Ord(FMethodType));
  Str := (FComJava as TComJava1).ExecuteCommand(Str);
  FValid := (Left(Str, 3) = '+OK');
  if FValid then
    Result := TComJavaValue.Create(Right(Str, 5),
      Right(FSig, Pos(')', FSig) + 1), FComJava)
  else
    if Pos('Java terminated', Str) + Pos('Java disconnected', Str) = 0 then
      FError := Right(Str, 6);
end;

function TComJavaMethod.GetExpressionValue: TComJavaValue;
begin
  Result := nil;
  var
  Str := (FComJava as TComJava1).ExecuteCommand('getExpressionValue'#4 +
    FClass.Name);
  FValid := (Left(Str, 3) = '+OK');
  if FValid then
  begin
    Str := Right(Str, 5);
    var
    Posi := Pos('_|_', Str);
    Str := Left(Str, Posi - 1);
    Result := TComJavaValue.Create(Str, Right(FSig, Pos(')', FSig) + 1),
      FComJava);
  end
  else if Pos('Java terminated', Str) + Pos('Java disconnected', Str) = 0 then
    FError := Right(Str, 6);
end;

function TComJavaMethod.GetExpressionType: string;
begin
  Result := '';
  var
  Str := (FComJava as TComJava1).ExecuteCommand('getExpressionValue'#4 +
    FClass.Name);
  FValid := (Left(Str, 3) = '+OK');
  if FValid then
  begin
    Str := Right(Str, 5);
    var
    Posi := Pos('_|_', Str);
    Str := Right(Str, Posi + 3);
    Result := Str;
  end
  else if Pos('Java terminated', Str) + Pos('Java disconnected', Str) = 0 then
    FError := Right(Str, 6);
end;

end.
