unit UExecution;

interface

uses
  Contnrs,
  Classes,
  Grids,
  SynEdit,
  UUMLForm,
  UComJava1;

type

  TExecutionType = (None, ObjectCreation, // Type aObject = new Type();
    VariableCreation,    // int  i = 7;
    ObjectDeclaration,   // Type aObject;
    VariableDeclaration, // int i;
    MethodCall, Statement, Assignment,   // i = 7 und aObject = new Type
    Expression,          // 7+3*4;
    ArrayDeclaration);   // int[] p = {1,2,3,4,4+5};

  TExecutionLine = class
  private
    FClassImport: string;
    FKind: TExecutionType;
    FMethod: string;
    FName: string;
    FParams: string;
    FTypename: string;
    FValue: string;
  public
    constructor Create(const Name: string); overload;
    constructor Create(const Name: string; Kind: TExecutionType); overload;
    property ClassImport: string read FClassImport write FClassImport;
    property Kind: TExecutionType read FKind write FKind;
    property Method: string read FMethod write FMethod;
    property Name: string read FName write FName;
    property Params: string read FParams write FParams;
    property Typename: string read FTypename write FTypename;
    property Value: string read FValue write FValue;
  end;

  TExecutionList = class(TObjectList)
    constructor Create;
    function Get(Int: Integer): TExecutionLine;
  end;

  TVariable = class
  private
    FExisting: Boolean;
    FName: string;
    FPrimitive: Boolean;
    FTyp: string;
  public
    constructor Create(const Name, Typ: string);
    function Put: string;
    function Get: string;
    function PrimitiveToObject(const Str: string): string;
    property Existing: Boolean read FExisting write FExisting;
    property Name: string read FName;
    property Primitive: Boolean read FPrimitive;
    property Typ: string read FTyp;
  end;

  TVariableList = class(TObjectList)
  public
    constructor Create;
    function Get(Int: Integer): TVariable; overload;
    function Get(const Name: string): TVariable; overload;
    procedure Delete(const Name: string);
    procedure SetNotExisting;
    function IsKnownType(const Typename: string): Boolean;
  end;

  TInteractiveExecuter = class
  private
    FComJava: TComJava1;
    FUMLForm: TFUMLForm;
    FSynEditor: TSynEdit;
    FSGVariables: TStringGrid;
    FClassNr: Integer;
    FClassname: string;
    FPathname: string;
    FPath: string;
    FStringList: TStringList;
    FVariableList: TVariableList;
    FExecutionList: TExecutionList;
    function GetVariables: string;
    procedure ExistingToDeclared(const Name: string);
    procedure DeclaredToExisting(const Name: string);
    function PutVariables: string;
    procedure DeclareVariable(Execution: TExecutionLine);
    procedure AddVarToGrid(const Name, Typ, Value: string);
    function IsKnownType(const Typename: string): Boolean;
    function UpdateVariables: Boolean;
    procedure DeleteFiles(const Pathname: string);
    procedure MakePathname;
    function StripFile(Str: string): string;
    function StripGetOb(const Str: string): string;
    procedure LogExecute(const Str: string);
    procedure Error(const Compiled: string);
    procedure MakeClass(Execution: TExecutionLine);
    procedure MakeClassExpression(Execution: TExecutionLine);
    function DoCompile: string;
    function Compile(Execution: TExecutionLine; Normal: Boolean): string;
    function Run(Execution: TExecutionLine): Boolean;
    procedure GetExpression(Execution: TExecutionLine);
    procedure AssignWithoutType(Execution: TExecutionLine);
    function CreateVariable(Execution: TExecutionLine): Boolean;
    procedure CreateObject(Execution: TExecutionLine);
    procedure CreateArray(Execution: TExecutionLine);
  public
    constructor Create(UMLForm: TFUMLForm; SynEdit: TSynEdit;
      StringGrid: TStringGrid; ComJava: TComJava1);
    destructor Destroy; override;
    procedure Execute(const Command: string);
    procedure Clear;
    procedure DelVariable(const Name: string);
    procedure AddVariable(const Name, Typ, Value: string);
    function NeedsSemicolon(const Str: string): Boolean;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  System.StrUtils,
  Forms,
  jni, {needed}
  URtfdDiagram,
  UDlgAbout,
  UExecutionParser,
  UConfiguration,
  UMessages,
  UJniWrapper1,
  UJava,
  UUtils;

{ --- TExecutionLine ----------------------------------------------------------- }

constructor TExecutionLine.Create(const Name: string);
begin
  Self.FName := Name;
end;

constructor TExecutionLine.Create(const Name: string; Kind: TExecutionType);
begin
  Self.FName := Name;
  Self.FKind := Kind;
end;

{ --- TExecutionList ----------------------------------------------------------- }

constructor TExecutionList.Create;
begin
  OwnsObjects := True;
end;

function TExecutionList.Get(Int: Integer): TExecutionLine;
begin
  Result := Items[Int] as TExecutionLine;
end;

{ --- TVariable ---------------------------------------------------------------- }

const
  Prim: array [1 .. 9] of string = ('byte', 'short', 'int', 'long',
    'double', 'float', 'boolean', 'char', 'String');

  Obje: array [1 .. 9] of string = ('Byte', 'Short', 'Integer', 'Long',
    'Double', 'Float', 'Boolean', 'Character', 'String');

constructor TVariable.Create(const Name, Typ: string);
begin
  Self.FName := Name;
  Self.FTyp := Typ;
  FExisting := False;
  var
  Int := 1;
  while (Int <= 8) and (Prim[Int] <> Typ) do
    Inc(Int);
  FPrimitive := (Int <= 8);
end;

function TVariable.Put: string;
begin
  if FPrimitive then
    Result := '    p_.put("_lvp_' + Name + '",' + Name + ');'
  else
    Result := '    p_.put("_lvo_' + Name + '",' + Name + ');';
end;

function TVariable.Get: string;
begin
  if FPrimitive then
    Result := '    ' + Typ + ' ' + Name + ' = (' + PrimitiveToObject(Typ) +
      ') p_.get("_lvp_' + Name + '");'
  else
    Result := '    ' + ReplaceStr(Typ, '$', '.') + ' ' + Name + ' = (' +
      ReplaceStr(Typ, '$', '.') + ') p_.get("_lvo_' + Name + '");';
end;

function TVariable.PrimitiveToObject(const Str: string): string;
begin
  var
  Int := 1;
  while (Int < 9) and (Prim[Int] <> Str) do
    Inc(Int);
  if Int <= 9 then
    Result := Obje[Int]
  else
    Result := Str;
end;

{ --- TVariableList ----------------------------------------------------------- }

constructor TVariableList.Create;
begin
  OwnsObjects := True;
end;

function TVariableList.Get(Int: Integer): TVariable;
begin
  Result := Items[Int] as TVariable;
end;

function TVariableList.Get(const Name: string): TVariable;
begin
  Result := nil;
  for var I := 0 to Count - 1 do
  begin
    var
    Variable := TVariable(Get(I));
    if Variable.Name = Name then
    begin
      Result := Variable;
      Break;
    end;
  end;
end;

procedure TVariableList.Delete(const Name: string);
begin
  for var I := 0 to Count - 1 do
  begin
    var
    Variable := TVariable(Get(I));
    if Variable.Name = Name then
    begin
      inherited Delete(I);
      Break;
    end;
  end;
end;

procedure TVariableList.SetNotExisting;
begin
  for var I := 0 to Count - 1 do
  begin
    var
    Variable := TVariable(Get(I));
    Variable.Existing := False;
  end;
end;

function TVariableList.IsKnownType(const Typename: string): Boolean;
begin
  Result := False;
  for var I := 0 to Count - 1 do
  begin
    var
    Variable := TVariable(Get(I));
    Result := Result or (Variable.Typ = Typename);
  end;
end;

{ --- TInteractiveExecuter ---------------------------------------------------------------- }

constructor TInteractiveExecuter.Create(UMLForm: TFUMLForm; SynEdit: TSynEdit;
  StringGrid: TStringGrid; ComJava: TComJava1);
begin
  FStringList := TStringList.Create;
  FVariableList := TVariableList.Create;
  FExecutionList := TExecutionList.Create;
  FUMLForm := UMLForm;
  FSynEditor := SynEdit;
  FSGVariables := StringGrid;
  FComJava := ComJava;
  FClassNr := 0;
  FClassname := '';
end;

destructor TInteractiveExecuter.Destroy;
begin
  FreeAndNil(FStringList);
  FreeAndNil(FVariableList);
  FreeAndNil(FExecutionList);
  inherited;
end;

procedure TInteractiveExecuter.DeleteFiles(const Pathname: string);
begin
  var Filenames := TDirectory.GetFiles(ExtractFilePath(Pathname), 'Class*.*');
  for var Filename in Filenames do
    DeleteFile(Filename);
end;

procedure TInteractiveExecuter.MakePathname;
begin
  FClassname := 'Class' + IntToStr(FClassNr);
  Inc(FClassNr);
  FPathname := FPath + FClassname + '.java';
end;

function TInteractiveExecuter.StripFile(Str: string): string;
var
  Int, JPos, Posi: Integer;
  StringList: TStringList;
begin
  Posi := Pos('.java:', Str); // VisibleStack.java:12:
  if Posi > 0 then
  begin
    Str := Right(Str, Posi + 6);
    Posi := Pos(':', Str);
    Str := Right(Str, Posi + 2);
  end;
  // only first error
  StringList := TStringList.Create;
  StringList.Text := Str;
  Int := -1;
  repeat
    Inc(Int);
    Posi := Pos('.java:', StringList[Int]);
  until (Int = StringList.Count - 1) or (Posi > 0);
  if Posi > 0 then
    for var J := StringList.Count - 1 downto Int do
      StringList.Delete(J)
  else
  begin
    JPos := StringList.Count;
    repeat
      Dec(JPos);
      Posi := Pos('1 error', StringList[JPos]);
    until (JPos = 0) or (Posi > 0);
    if Posi > 0 then
    begin
      StringList.Delete(JPos);
      StringList.Delete(JPos - 1);
    end;
  end;

  for var I := StringList.Count - 1 downto 0 do
    if Pos('location: class', StringList[I]) > 0 then
      StringList.Delete(I);

  Result := StringList.Text;
  FreeAndNil(StringList);
end;

function TInteractiveExecuter.StripGetOb(const Str: string): string;
var
  Int, Posi: Integer;
begin
  var
  StringList := TStringList.Create;
  Posi := Pos('getOb(', Str);
  if Posi > 0 then
  begin
    StringList.Text := Str;
    Int := 0;
    repeat
      Inc(Int);
      Posi := Pos('getOb(', StringList[Int]);
    until (Posi > 0) or (Int = StringList.Count - 1);
    if Posi > 0 then
    begin
      StringList[Int] := Left(StringList[Int], -2);
      StringList[Int] := '  ' + Right(StringList[Int], Posi + 6);
      if Int + 1 < StringList.Count then
        StringList[Int + 1] := '  ' + Right(StringList[Int + 1], Posi + 6);
    end;
    Result := StringList.Text;
  end
  else
    Result := Str;
  FreeAndNil(StringList);
end;

procedure TInteractiveExecuter.LogExecute(const Str: string);
var
  TeFile: TextFile;
begin
  if FConfiguration.LogfileInteractiveOK then
  begin
    AssignFile(TeFile, FConfiguration.LogfileInteractive);
    try
      Append(TeFile);
      Writeln(TeFile, DateTimeToStr(Now()) + ' ' + GetComputerNetName + ' Version: '
        + UDlgAbout.Version);
      Writeln(TeFile, Str);
      CloseFile(TeFile);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  end;
end;

procedure TInteractiveExecuter.Error(const Compiled: string);
begin
  if FStringList.Count > 1 then
    FMessages.OutputToTerminal(StripFile(Compiled))
  else
    ErrorMsg(Right(Compiled, 6));
  LogExecute(Compiled);
end;

procedure TInteractiveExecuter.MakeClass(Execution: TExecutionLine);
begin
  FStringList.Clear;
  if Execution.ClassImport <> '' then
    FStringList.Add('import ' + Execution.ClassImport + ';');
  FStringList.Add('');
  FStringList.Add('public class ' + FClassname + ' {');
  FStringList.Add('  public static void run() {'); // static
  FStringList.Add('    java.util.Properties p_ = System.getProperties();');
  FStringList.Add(GetVariables);
  FStringList.Add('    ' + Execution.Value);
  FStringList.Add(PutVariables); // p_.put("lv_
  FStringList.Add('  }');
  FStringList.Add('}');
end;

procedure TInteractiveExecuter.MakeClassExpression(Execution: TExecutionLine);
begin
  FStringList.Clear;
  if Execution.ClassImport <> '' then
    FStringList.Add('import ' + Execution.ClassImport + ';');
  FStringList.Add('');
  FStringList.Add('public class ' + FClassname + ' extends Eval {');
  FStringList.Add('  public static Object run() {');
  FStringList.Add('    java.util.Properties p_ = System.getProperties();'
    + #13#10);
  FStringList.Add(GetVariables);
  var
  Str := Execution.Value;
  if Copy(Str, Length(Str), 1) = ';' then
    Delete(Str, Length(Str), 1);
  FStringList.Add('    Object aObject = getOb(' + Str + ');');
  FStringList.Add(PutVariables);
  FStringList.Add('    return aObject;');
  FStringList.Add('  }');
  FStringList.Add('}');
end;

function TInteractiveExecuter.DoCompile: string;
var
  Str1, Str2, Compiled: string;
  Int, JPos: Integer;
  SLCompiled, SLSplitSpace: TStringList;
begin
  repeat
    Compiled := FComJava.ExecuteCommand
      ('compile'#4 + FConfiguration.JavaCompilerParameter + #4 + FPathname);
    if (Left(Compiled, 4) = '-ERR') and
      (Pos('might not have been initialized', Compiled) > 0) then
    begin
      try
        FStringList.LoadFromFile(FPathname);
        SLCompiled := TStringList.Create;
        SLCompiled.Text := Compiled;
        JPos := 0;
        while JPos < SLCompiled.Count - 1 do
        begin
          Str1 := SLCompiled[JPos];
          if Pos('might not have been initialized', Str1) > 0 then
          begin
            Inc(JPos);
            Str2 := SLCompiled[JPos];
            var Posi := FStringList.IndexOf(Str2);
            if Posi > -1 then
              FStringList.Delete(Posi);
            SLSplitSpace := Split(' ', Str1);
            Int := 0;
            while SLSplitSpace[Int] <> 'might' do
              Inc(Int);
            if (0 <= Int - 1) and (Int - 1 < SLSplitSpace.Count) then
              ExistingToDeclared(SLSplitSpace[Int - 1]);
            FreeAndNil(SLSplitSpace);
          end;
          Inc(JPos);
        end;
        FreeAndNil(SLCompiled);
        FStringList.SaveToFile(FPathname);
      except
        on E: Exception do
          ErrorMsg(E.Message);
      end;
    end
    else
      Break;
  until False;
  Result := Compiled;
end;

function TInteractiveExecuter.Compile(Execution: TExecutionLine;
  Normal: Boolean): string;
begin
  MakePathname;
  if Normal then
    MakeClass(Execution) // instead of MakeClass / MakeThread
  else
    MakeClassExpression(Execution);
  try
    FStringList.SaveToFile(FPathname);
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
  var
  Compiled := DoCompile;
  if Normal and (Left(Compiled, 4) = '-ERR') then
    Error(Compiled);
  Result := Compiled;
end;

function TInteractiveExecuter.Run(Execution: TExecutionLine): Boolean;
var
  JavaClass: TComJavaClass;
  JavaMethod: TComJavaMethod;
  JavaValue: TComJavaValue;
begin
  Result := False;
  if Left(Compile(Execution, True), 3) = '+OK' then
  begin
    Result := True;
    if FComJava.NewClass(FClassname, FPathname) then
    begin
      JavaClass := FComJava.GetClass(FClassname);
      JavaMethod := TComJavaMethod.Create(JavaClass, 'run', jni.static,
        'void', nil, FComJava);
      try
        JavaValue := JavaMethod.Call(nil);
        FreeAndNil(JavaValue);
        if not JavaMethod.IsValid then
          FMessages.OutputToTerminal(JavaMethod.Error);
      finally
        FreeAndNil(JavaMethod);
      end;
      UpdateVariables;
    end;
  end;
end;

procedure TInteractiveExecuter.GetExpression(Execution: TExecutionLine);
var
  JavaClass: TComJavaClass;
  JavaMethod: TComJavaMethod;
  ComJavaValue: TComJavaValue;
  Compiled, Value: string;
begin
  // try method-call as an expression
  Compiled := Compile(Execution, False);
  if (Left(Compiled, 4) = '-ERR') and
    (Pos('''void''', Compiled) + Pos('"void"', Compiled) > 0) then
    Run(Execution)
  else if Left(Compiled, 3) = '+OK' then
  begin
    if FComJava.NewClass(FClassname, FPathname) then
    begin
      JavaClass := FComJava.GetClass(FClassname);
      JavaMethod := TComJavaMethod.Create(JavaClass, 'run', jni.static,
        'java.lang.Object', nil, FComJava);
      try
        ComJavaValue := JavaMethod.GetExpressionValue;
        Value := Execution.Value;
        if Assigned(ComJavaValue) then
          FMessages.OutputToTerminal(Copy(Value, 1, Length(Value) - 1) + ': ' +
            ComJavaValue.AsString)
        else
          FMessages.OutputToTerminal(JavaMethod.Error);
      finally
        FreeAndNil(JavaMethod);
        FreeAndNil(ComJavaValue); // new
      end;
      UpdateVariables;
    end;
  end
  else
  begin
    Value := StripFile(Compiled);
    Value := StripGetOb(Value);
    FMessages.OutputToTerminal(Value);
  end;
end;

procedure TInteractiveExecuter.AssignWithoutType(Execution: TExecutionLine);
var
  JavaClass: TComJavaClass;
  JavaMethod: TComJavaMethod;
  Compiled, Str, VarName: string;
  Posi: Integer;
  Variable: TVariable;
begin
  // do we know the variable?
  VarName := Execution.Name;
  Posi := Pos('.', VarName);
  if Posi > 0 then
    Delete(VarName, Posi, Length(VarName));
  Variable := FVariableList.Get(VarName);
  if Assigned(Variable) or FConfiguration.StrictJavaMode then
  begin
    Run(Execution);
    Exit;
  end;

  // try method-call as an expression
  Str := Execution.Value;
  Posi := Pos('=', Str);
  Execution.FValue := Copy(Str, Posi + 1, Length(Str));
  Compiled := Compile(Execution, False);

  if Left(Compiled, 3) = '+OK' then
  begin
    if FComJava.NewClass(FClassname, FPathname) then
    begin
      JavaClass := FComJava.GetClass(FClassname);
      JavaMethod := TComJavaMethod.Create(JavaClass, 'run', jni.static,
        'java.lang.Object', nil, FComJava);
      try
        Execution.FTypename := JavaMethod.GetExpressionType;
        if JavaMethod.IsValid then
        begin
          Execution.FValue := Execution.Typename + ' ' + Str;
          if Pos(' new ', Execution.Value) > 0 then
            CreateObject(Execution)
          else
            CreateVariable(Execution);
        end
        else
          FMessages.OutputToTerminal(JavaMethod.Error);
      finally
        FreeAndNil(JavaMethod);
      end;
    end;
  end
  else
  begin
    Str := StripFile(Compiled);
    Str := StripGetOb(Str);
    FMessages.OutputToTerminal(Str);
  end;
end;

function TInteractiveExecuter.IsKnownType(const Typename: string): Boolean;
begin
  Result := IsSimpleType(Typename) or IsSimpleType(WithoutArray(Typename)) or
    FVariableList.IsKnownType(Typename);
end;

procedure TInteractiveExecuter.DeclareVariable(Execution: TExecutionLine);
var
  Compiled: Boolean;
  Variable: TVariable;
begin
  // Type Var;
  if IsKnownType(Execution.Typename) then
    Compiled := True
  else
    Compiled := (Left(Compile(Execution, True), 3) = '+OK');
  if Compiled then
  begin
    Variable := FVariableList.Get(Execution.Name);
    if Assigned(Variable) then
      Variable.FTyp := Execution.Typename
    else
      FVariableList.Add(TVariable.Create(Execution.Name, Execution.Typename));
    AddVarToGrid(Execution.Name, Execution.Typename, '');
  end;
end;

function TInteractiveExecuter.CreateVariable(Execution
  : TExecutionLine): Boolean;
begin
  // create variable:  Type Var = Expression;
  Result := False;
  var
  Variable := FVariableList.Get(Execution.Name);
  if Assigned(Variable) then
  begin
    if Variable.Typ <> Execution.Typename then
    begin
      Variable.FTyp := Execution.Typename;
      Variable.FExisting := False;
    end;
  end
  else
  begin
    Variable := TVariable.Create(Execution.Name, Execution.Typename);
    FVariableList.Add(Variable);
    AddVarToGrid(Execution.Name, Execution.Typename, '');
  end;
  if Run(Execution) then
    Result := True
  else
    DelVariable(Variable.Name);
end;

procedure TInteractiveExecuter.CreateObject(Execution: TExecutionLine);
begin
  // create object  Type obj = new Type(...);
  var
  Diagram := FUMLForm.MainModul.Diagram as TRtfdDiagram;
  var
  Variable := FVariableList.Get(Execution.Name);
  if Assigned(Variable) then
  begin
    Diagram.DeleteObject(Variable.Name);
    DelVariable(Execution.Name);
  end;
  Variable := nil;

  if CreateVariable(Execution) and Assigned(FComJava.GetObject(Execution.Name))
  then
    Diagram.ShowObject(Execution.Name)
  else if not Assigned(Variable) then
    DelVariable(Execution.Name);
end;

procedure TInteractiveExecuter.CreateArray(Execution: TExecutionLine);
var
  Value: string;
  Variable: TVariable;
begin
  // create variable:  Type[] Var = {1, 2, 3}; literal array
  Variable := FVariableList.Get(Execution.Name);
  if Assigned(Variable) then
  begin
    if Variable.Typ <> Execution.Typename then
    begin
      Variable.FTyp := Execution.Typename;
      Variable.FExisting := False;
    end;
  end
  else
  begin
    Variable := TVariable.Create(Execution.Name, Execution.Typename);
    FVariableList.Add(Variable);
    AddVarToGrid(Execution.Name, Execution.Typename, '');
  end;
  Value := Execution.Value;
  var Posi := Pos(' ' + Execution.Name + ' ', Value);
  Insert('_', Value, Posi + 1);
  Insert('_', Value, Posi + 3);
  Value := Value + #13#10 + '    ' + Execution.Name + ' = _' +
    Execution.Name + '_;';
  Execution.FValue := Value;
  if not Run(Execution) then
    DelVariable(Variable.Name);
end;

procedure TInteractiveExecuter.Execute(const Command: string);
var
  AFile: string;
  IsOk, Update: Boolean;
  SLFiles: TStringList;
  ExecutionParser: TExecutionParser;
  Execution: TExecutionLine;
  Diagram: TRtfdDiagram;
begin
  if Command = '' then
    Exit;

  SLFiles := FJava.GetAllPathnames;
  IsOk := True;
  for var I := 0 to SLFiles.Count - 1 do
  begin
    AFile := SLFiles[I];
    if HasJavaExtension(AFile) then
    begin
      if not FJava.PreCompile(nil, AFile) then
      begin
        IsOk := False;
        Break;
      end;
    end;
  end;
  FreeAndNil(SLFiles);
  if not IsOk then
    Exit;

  FStringList.Text := Command;
  if FComJava = FComJava.GetFirstComJava then
    FPath := FConfiguration.TempDir
  else
    FPath := ExtractFilePath(FUMLForm.Pathname);
  FPath := IncludeTrailingPathDelimiter(FPath);

  FMessages.ChangeTab(FMessages.TabControlMessages.TabIndex);
  FMessages.TBExecute.Enabled := False;

  SLFiles := FJava.GetAllClassnames;
  for var I := 0 to SLFiles.Count - 1 do
    FComJava.NewClass(ChangeFileExt(ExtractFileName(SLFiles[I]), ''),
      SLFiles[I]);
  FreeAndNil(SLFiles);

  if Assigned(FUMLForm) then
    LockWindow(FUMLForm.Handle);
  ExecutionParser := TExecutionParser.Create(True);
  Update := True;
  try
    ExecutionParser.PrepareExecute(FStringList.Text, FExecutionList);
    for var I := 0 to FExecutionList.Count - 1 do
    begin
      // i have got exceptions with FExecutionList.Count = 1 and i = 1 !
      try
        Execution := FExecutionList.Get(I);
        if UpdateVariables then
        begin
          case Execution.Kind of
            MethodCall:
              GetExpression(Execution); // java.lang.Math.random()
            Statement:
              Run(Execution); // must be run(Execution)
            // while (i>0) i--;  new Auto() im StrictJavaMode
            Assignment:
              AssignWithoutType(Execution); // i = 5+2*i
            VariableDeclaration, // I i;
            ObjectDeclaration:
              DeclareVariable(Execution); // Car car1
            VariableCreation:
              CreateVariable(Execution); // I i = 6
            ObjectCreation:
              CreateObject(Execution); // Car car = new Car("a car")
            Expression:
              GetExpression(Execution); // i + 4
            ArrayDeclaration:
              CreateArray(Execution); // I[] p = {1,2,3,4,4+5}
          end;
        end
        else
          Update := False;
      except
        on E: Exception do
          Continue;
      end;
    end;
  finally
    FExecutionList.Clear;
    if Assigned(FUMLForm) and (FUMLForm.Pathname <> '') and
      Assigned(FUMLForm.MainModul) and Assigned(FUMLForm.MainModul.Diagram) then
    begin
      Diagram := FUMLForm.MainModul.Diagram as TRtfdDiagram;
      if FConfiguration.ShowAllNewObjects then
        Diagram.ShowAllNewObjects(Self);
      Diagram.UpdateAllObjects;
    end;
    if Update then
      UpdateVariables;
    FMessages.TBExecute.Enabled := True;
    FStringList.Clear;
    if Assigned(FUMLForm) then
      UnlockWindow;
    FreeAndNil(ExecutionParser);
    // debug je2java
    DeleteFiles(FPathname);
 // Application.ProcessMessages;  //TODO RR
  end;
end;

function TInteractiveExecuter.NeedsSemicolon(const Str: string): Boolean;
var
  ExecutionParser: TExecutionParser;
  Kind: TExecutionType;
begin
  FStringList.Text := Str;
  ExecutionParser := TExecutionParser.Create(True);
  try
    ExecutionParser.PrepareExecute(FStringList.Text, FExecutionList);
    if FExecutionList.Count = 1 then
      Kind := FExecutionList.Get(0).Kind
    else
      Kind := None;
    Result := (Kind in [ObjectCreation, VariableCreation, Assignment,
      VariableDeclaration, ObjectDeclaration, MethodCall]);
    // not None, Statement, Expression
  finally
    FreeAndNil(ExecutionParser);
    FStringList.Clear;
  end;
end;

function TInteractiveExecuter.UpdateVariables: Boolean;
var
  Variables: string;
  StringList: TStringList;
  AClass: TComJavaClass;
begin
  FVariableList.SetNotExisting;
  Variables := FComJava.ExecuteCommand('getVariables');
  if Left(Variables, 3) = '+OK' then
  begin
    StringList := Split(#4, Right(Variables, 5));
    for var I := 0 to StringList.Count - 1 do
    begin
      Variables := StringList.Names[I];
      var Posi := Pos('|', Variables);
      if Posi > 0 then
      begin // new object detected
        FClassname := Copy(Variables, 1, Posi - 1);
        Delete(Variables, 1, Posi);
        if FComJava.ObjectList.IndexOf(Variables) = -1 then
        begin
          AClass := FComJava.GetClass(FClassname);
          if not Assigned(AClass) then
          begin
            AClass := TComJavaClass.CreateWithHandle(FClassname, FComJava);
            FComJava.ClassList.AddObject(FClassname, AClass);
          end;
          FComJava.AddObject(Variables, AClass);
        end;
      end;
      DeclaredToExisting(Variables);
      Posi := 1;
      while (Posi < FSGVariables.RowCount) and (FSGVariables.Cells[0, Posi] <> Variables) do
        Inc(Posi);
      if Posi < FSGVariables.RowCount then
        FSGVariables.Cells[2, Posi] := StringList.ValueFromIndex[I];
    end;
    FreeAndNil(StringList);
    Result := True;
  end
  else
    Result := False;
end;

function TInteractiveExecuter.GetVariables: string;
var
  Str: string;
  Variable: TVariable;
begin
  Str := '';
  for var I := 0 to FVariableList.Count - 1 do
  begin
    Variable := FVariableList.Get(I);
    if Variable.FExisting then
      Str := Str + Variable.Get + #13#10
    else
      Str := Str + '    ' + Variable.Typ + ' ' + Variable.Name + ';' + #13#10;
  end;
  Result := Str;
end;

procedure TInteractiveExecuter.ExistingToDeclared(const Name: string);
begin
  var
  Variable := FVariableList.Get(Name);
  if Assigned(Variable) then
    Variable.FExisting := False;
end;

procedure TInteractiveExecuter.DeclaredToExisting(const Name: string);
begin
  var
  Variable := FVariableList.Get(Name);
  if Assigned(Variable) then
    Variable.FExisting := True;
end;

function TInteractiveExecuter.PutVariables: string;
var
  Str: string;
  Variable: TVariable;
begin
  Str := '';
  for var I := 0 to FVariableList.Count - 1 do
  begin
    Variable := FVariableList.Get(I);
    Str := Str + Variable.Put + #13#10;
  end;
  Result := Str;
end;

procedure TInteractiveExecuter.AddVarToGrid(const Name, Typ, Value: string);
begin
  try
    if Assigned(FSGVariables) and (FSGVariables.ColCount >= 3) then
    begin
      var
      Int := FSGVariables.Cols[0].IndexOf(Name);
      if Int = -1 then
      begin // new object
        if FSGVariables.Cells[0, 1] <> '' then
          FSGVariables.RowCount := FSGVariables.RowCount + 1;
        Int := FSGVariables.RowCount - 1;
        FSGVariables.Cells[0, Int] := Name;
      end;
      // update object
      FSGVariables.Cells[1, Int] := Typ;
      FSGVariables.Cells[2, Int] := Value;
    end
    else
      FConfiguration.Log('AddVarToGrid FSGVariables = nil');
    if FSGVariables.ColCount > 3 then
      FConfiguration.Log('AddVarToGrid FSGVariables.ColCount = ' +
        IntToStr(FSGVariables.ColCount));
  except
    on E: Exception do
      FConfiguration.Log('TInteractiveExecuter.AddVarToGrid ', E);
  end;
end;

procedure TInteractiveExecuter.Clear;
begin
  try
    if Assigned(FSGVariables) then
    begin
      FSGVariables.RowCount := 2;
      for var I := 0 to 2 do
        FSGVariables.Cells[I, 1] := '';
      if FVariableList.Count > 0 then
        FVariableList.Clear;
    end;
  except
  end;
end;

procedure TInteractiveExecuter.DelVariable(const Name: string);
begin
  FVariableList.Delete(Name);
  var
  Int := FSGVariables.Cols[0].IndexOf(Name);
  if Int > -1 then
  begin
    for var J := Int to FSGVariables.RowCount - 2 do
      FSGVariables.Rows[J] := FSGVariables.Rows[J + 1];
    if FSGVariables.RowCount = 2 then
    begin
      FSGVariables.Cells[0, 1] := '';
      FSGVariables.Cells[1, 1] := '';
      FSGVariables.Cells[2, 1] := '';
    end
    else
      FSGVariables.RowCount := FSGVariables.RowCount - 1;
  end;
end;

procedure TInteractiveExecuter.AddVariable(const Name, Typ, Value: string);
begin
  var
  Variable := FVariableList.Get(Name);
  if Assigned(Variable) then
    Variable.FTyp := Typ
  else
    FVariableList.Add(TVariable.Create(Name, Typ));
  AddVarToGrid(Name, Typ, Value);
end;

// function TInteractiveExecuter.getShell: TStringList;
// var aSL: TStringList;
// begin
// aSL:= TStringList.Create;
// aSL.Add('class Eval {');
// aSL.Add('public static Wrap getOb(Object o) {return new Wrap(o);}');
// aSL.Add('protected static Object getOb(final String s) {return new Object() {public String result = s;};}');
// aSL.Add('protected static Object getOb(final boolean b) {return new Object() {public boolean result = b;};}');
// aSL.Add('protected static Object getOb(final byte b) {return new Object() {public byte result = b;};}');
// aSL.Add('protected static Object getOb(final char c) {return new Object() {public char result = c;};}');
// aSL.Add('protected static Object getOb(final double d) {return new Object() {public double result = d;};}');
// aSL.Add('protected static Object getOb(final float f) {return new Object() {public float result = f;};}');
// aSL.Add('protected static Object getOb(final int i) {return new Object() {public int result = i;};}');
// aSL.Add('protected static Object getOb(final long l) {return new Object() {public long result = l;};}');
// aSL.Add('protected static Object getOb(final short s) {return new Object() {public short result = s;};}');
// aSL.Add('}');
// aSL.Add('class Wrap { public Object result; Wrap(Object result) { this.result = result; } }');
// Result:= aSL;
// end;

end.
