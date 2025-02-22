unit UExecution;

interface

uses Contnrs, Classes, Grids, SynEdit, UUMLForm, UComJava1;

type

  TExecutionType = (None,
                    ObjectCreation,      // Type aObject = new Type();
                    VariableCreation,    // int  i = 7;
                    ObjectDeclaration,   // Type aObject;
                    VariableDeclaration, // int i;
                    MethodCall,
                    Statement,
                    Assignment,          // i = 7; aObject = new Type;
                    Expression,          // 7+3*4;
                    ArrayDeclaration);   // int[] p = {1,2,3,4,4+5};

  TExecutionLine = class
  public
    Name: string;
    Typename: string;
    Method: string;
    Value: string;
    Kind: TExecutionType;
    ClassImport: string;
    Params: string;
    constructor create(const aName: string); overload;
    constructor create(const aName: string; aKind: TExecutionType); overload;
  end;

  TExecutionList = class(TObjectList)
    constructor create;
    function get(i: integer): TExecutionLine;
  end;

  TVariable = class
  public
    Name: string;
    Typ: string;
    existing: boolean;
    primitive: boolean;
    constructor create(const aName, aTyp: string);
    function put: string;
    function get: string;
    function PrimitiveToObject(const s: string): string;
  end;

  TVariableList = class(TObjectList)
  public
    constructor create;
    function get(i: integer): TVariable; overload;
    function get(const Name: string): TVariable; overload;
    procedure delete(const Name: string);
    procedure setNotExisting;
    function isKnownType(const Typename: string): boolean;
  end;

  TInteractiveExecuter = class
  private
    ComJava: TComJava1;
    UMLForm: TFUMLForm;
    aSynEditor: TSynEdit;
    SGVariables: TStringGrid;
    ClassNr: integer;
    aClassname: string;
    Pathname: string;
    Path: string;
    SL: TStringList;
    VariableList: TVariableList;
    ExecutionList: TExecutionList;
    function getVariables: string;
    procedure existingToDeclared(const Name: string);
    procedure DeclaredToExisting(const Name: string);
    function putVariables: string;
    procedure DeclareVariable(aExecution: TExecutionLine);
    procedure AddVarToGrid(const Name, Typ, Value: string);
    function IsKnownType(const Typename: string): boolean;
    function UpdateVariables: boolean;
    procedure DeleteFiles(const aPathname: string);
    procedure MakePathname;
    function StripFile(s: string): string;
    function StripGetOb(const s: string): string;
    procedure LogExecute(const s: string);
    procedure Error(const Compiled: string);
    procedure MakeClass(aExecution: TExecutionLine);
    procedure MakeClassExpression(aExecution: TExecutionLine);
    function DoCompile: string;
    function Compile(aExecution: TExecutionLine; normal: boolean): string;
    function Run(aExecution: TExecutionLine): boolean;
    procedure getExpression(aExecution: TExecutionLine);
    procedure assignWithoutType(aExecution: TExecutionLine);
    function CreateVariable(aExecution: TExecutionLine): boolean;
    procedure CreateObject(aExecution: TExecutionLine);
    procedure CreateArray(aExecution: TExecutionLine);
  public
    constructor Create(aUMLForm: TFUMLForm; SE: TSynEdit; SG: TStringGrid; C: TComJava1);
    destructor Destroy; override;
    procedure Execute(const s: string);
    procedure Clear;
    procedure DelVariable(const Name: string);
    procedure AddVariable(const Name, Typ, Value: string);
    function NeedsSemicolon(const s: string): boolean;
  end;

implementation

uses Windows, SysUtils, Forms, Controls, UJavaParser, JNI, {needed}
     URtfdDiagram, UDlgAbout, UExecutionParser, UConfiguration, UMessages,
     UJniWrapper1, UJava, StrUtils, UUtils;

{--- TExecutionLine -----------------------------------------------------------}

constructor TExecutionLine.create(const aName: string);
begin
  Self.Name:= aName;
end;

constructor TExecutionLine.create(const aName: string; aKind: TExecutionType);
begin
  Self.Name:= aName;
  Self.Kind:= aKind;
end;

{--- TExecutionList -----------------------------------------------------------}

constructor TExecutionList.create;
begin
  OwnsObjects:= true;
end;

function TExecutionList.get(i: Integer): TExecutionLine;
begin
  Result:= items[i] as TExecutionLine;
end;

{--- TVariable ----------------------------------------------------------------}

const prim : array[1..9] of string = ('byte', 'short', 'int', 'long', 'double', 'float', 'boolean', 'char', 'String');
const obje : array[1..9] of string = ('Byte', 'Short', 'Integer', 'Long', 'Double', 'Float', 'Boolean', 'Character', 'String');

constructor TVariable.create(const aName, aTyp: string);
begin
  Self.Name:= aName;
  self.Typ:= aTyp;
  existing:= false;
  var i:= 1;
  while (i <= 8) and (prim[i] <> Typ) do inc(i);
  primitive:= (i <= 8);
end;

function TVariable.put: string;
begin
  if primitive
    then Result:= '    p_.put("_lvp_' + Name + '",' + Name+ ');'
    else Result:= '    p_.put("_lvo_' + Name + '",' + Name+ ');'
end;

function TVariable.get: string;
begin
  if primitive
    then Result:= '    ' + Typ + ' ' + Name + ' = (' + PrimitiveToObject(Typ) +
                  ') p_.get("_lvp_' + Name + '");'
    else Result:= '    ' + ReplaceStr(Typ, '$', '.') + ' ' + Name + ' = (' + ReplaceStr(Typ, '$', '.') +
                  ') p_.get("_lvo_' + Name + '");'
end;

function TVariable.PrimitiveToObject(const s: string): string;
begin
  var i:= 1;
  while (i < 9) and (prim[i] <> s) do
    inc(i);
  if i <= 9
    then Result:= obje[i]
    else Result:= s;
end;

{--- TVariableList -----------------------------------------------------------}

constructor TVariableList.create;
begin
  OwnsObjects:= true;
end;

function TVariableList.get(i: Integer): TVariable;
begin
  Result:= Items[i] as TVariable;
end;

function TVariableList.get(const Name: string): TVariable;
begin
  Result:= nil;
  for var i:= 0 to Count - 1 do begin
    var Variable:= get(i);
    if TVariable(Variable).Name = Name then begin
      Result:= Variable;
      break;
    end;
  end;
end;

procedure TVariableList.delete(const Name: string);
begin
  for var i:= 0 to Count - 1 do begin
    var Variable:= get(i);
    if TVariable(Variable).Name = Name then begin
      inherited Delete(i);
      break;
    end;
  end;
end;

procedure TVariableList.setNotExisting;
begin
  for var i:= 0 to Count - 1 do begin
    var Variable:= get(i);
    TVariable(Variable).existing:= false;
  end;
end;

function TVariableList.isKnownType(const Typename: string): boolean;
begin
  Result:= false;
  for var i:= 0 to Count - 1 do begin
    var Variable:= get(i);
    Result:= Result or (TVariable(Variable).Typ = Typename);
  end;
end;

{--- TInteractiveExecuter ----------------------------------------------------------------}

constructor TInteractiveExecuter.Create(aUMLForm: TFUMLForm; SE: TSynEdit; SG: TStringGrid; C: TComJava1);
begin
  SL:= TStringList.Create;
  VariableList:= TVariableList.create;
  ExecutionList:= TExecutionList.create;
  UMLForm:= aUMLForm;
  aSynEditor:= SE;
  SGVariables:= SG;
  ComJava:= C;
  ClassNr:= 0;
  aClassname:= '';
end;

destructor TInteractiveExecuter.Destroy;
begin
  FreeAndNil(SL);
  FreeAndNil(VariableList);
  FreeAndNil(ExecutionList);
  // FreeAndNil(SGVariables);
end;

procedure TInteractiveExecuter.DeleteFiles(const aPathname: string);
  var aPath, Mask: string; SearchRec: TSearchRec;
begin
  aPath:= ExtractFilePath(aPathname);
  Mask:= aPath + 'Class*.*';
  if FindFirst(Mask, 0, SearchRec) = 0 then begin
    DeleteFile(aPath + SearchRec.Name);
    while FindNext(SearchRec) = 0 do
      DeleteFile(aPath + SearchRec.Name);
    FindClose(SearchRec);
  end;
end;

procedure TInteractiveExecuter.MakePathname;
begin
  aClassname:= 'Class' + IntToStr(ClassNr);
  inc(ClassNr);
  Pathname:= Path + aClassname + '.java';
end;

function TInteractiveExecuter.StripFile(s: string): string;
  var i, j, p: integer; aSL: TStringList;
begin
  p:= Pos('.java:', s);   // VisibleStack.java:12:
  if p > 0 then begin
    s:= Right(s, p+6);
    p:= Pos(':', s);
    s:= Right(s, p+2);
  end;
  // only first error
  aSL:= TStringList.Create;
  aSL.Text:= s;
  i:= -1;
  repeat
    inc(i);
    p:= Pos('.java:', aSL[i]);
  until (i = aSL.Count-1) or (p > 0);
  if p > 0 then
    for j:= aSL.Count - 1 downto i do
      aSL.Delete(j)
  else begin
    j:= aSL.Count;
    repeat
      dec(j);
      p:= Pos('1 error', aSL[j]);
    until (j = 0) or (p > 0);
    if p > 0 then begin
      aSL.Delete(j);
      aSL.Delete(j-1);
    end;
  end;

  for i:= aSL.Count - 1 downto 0 do
    if Pos('location: class', aSL[i]) > 0 then
      aSL.Delete(i);

  Result:= aSL.Text;
  FreeAndNil(aSL);
end;

function TInteractiveExecuter.StripGetOb(const s: string): string;
  var i, p: integer;
begin
  var aSL:= TStringList.Create;
  p:= Pos('getOb(', s);
  if p > 0 then begin
    aSL.Text:= s;
    i:= 0;
    repeat
      inc(i);
      p:= pos('getOb(', aSL[i]);
    until (p > 0) or (i = aSL.Count-1);
    if p > 0 then begin
      aSL[i]:= Left(aSL[i], -2);
      aSL[i]:= '  ' + Right(aSL[i], p + 6);
      if i + 1 < aSL.Count then
        aSL[i+1]:= '  ' + Right(aSL[i+1], p + 6);
    end;
    Result:= aSL.Text;
  end
  else
    Result:= s;
  FreeAndNil(aSL);
end;

procedure TInteractiveExecuter.LogExecute(const s: string);
  var F: TextFile;
begin
  if FConfiguration.LogfileInteractiveOK then begin
    AssignFile(F, FConfiguration.LogfileInteractive);
    try
      Append(F);
      Writeln(F, DateTimeToStr(Now()) + ' ' + GetComputerNetName +
                 ' Version: ' + UDlgAbout.Version);
      Writeln(F, s);
      CloseFile(F);
    except
      on e: exception do
        ErrorMsg(e.Message);
    end;
  end;
end;

procedure TInteractiveExecuter.Error(const Compiled: string);
begin
  if SL.Count > 1
    then FMessages.OutputToTerminal(StripFile(compiled))
    else ErrorMsg(Right(compiled, 6));
  LogExecute(Compiled);
end;

procedure TInteractiveExecuter.MakeClass(aExecution: TExecutionLine);
begin
  SL.Clear;
  if aExecution.ClassImport <> '' then
    SL.Add('import ' + aExecution.ClassImport + ';');
  SL.Add('');
  SL.Add('public class ' + aClassname + ' {');
  SL.Add('  public static void run() {');    // static
  SL.Add('    java.util.Properties p_ = System.getProperties();');
  SL.Add(getVariables);
  SL.Add('    ' + aExecution.Value);
  SL.Add(putVariables);  // p_.put("lv_
  SL.Add('  }');
  SL.Add('}');
end;

procedure TInteractiveExecuter.MakeClassExpression(aExecution: TExecutionLine);
begin
  SL.Clear;
  if aExecution.ClassImport <> '' then
    SL.Add('import ' + aExecution.ClassImport + ';');
  SL.Add('');
  SL.Add('public class ' + aClassname + ' extends Eval {');
  SL.Add('  public static Object run() {');
  SL.Add('    java.util.Properties p_ = System.getProperties();' + #13#10);
  SL.Add(getVariables);
  var s:= aExecution.Value;
  if Copy(s, length(s), 1) = ';' then
    delete(s, length(s), 1);
  SL.Add('    Object aObject = getOb(' + s + ');');
  SL.Add(putVariables);
  SL.Add('    return aObject;');
  SL.Add('  }');
  SL.Add('}');
end;

function TInteractiveExecuter.DoCompile: string;
  var s1, s2, compiled: string;
      i, j, p: integer; SLCompiled, SLSplitSpace: TStringList;
begin
  repeat
    Compiled:= ComJava.ExecuteCommand('compile'#4 + FConfiguration.JavaCompilerParameter + #4 + Pathname);
    if (Left(Compiled, 4) = '-ERR') and (Pos('might not have been initialized', Compiled) > 0) then begin
    try
      SL.LoadFromFile(Pathname);
      SLCompiled:= TStringList.Create;
      SLCompiled.Text:= Compiled;
      j:= 0;
      while  j < SLCompiled.Count - 1 do begin
        s1:= SLCompiled.Strings[j];
        if Pos('might not have been initialized', s1) > 0 then begin
          inc(j);
          s2:= SLCompiled.Strings[j];
          p:= SL.IndexOf(s2);
          if p > -1 then SL.Delete(p);
          SLSplitSpace:= Split(' ', s1);
          i:= 0;
          while SLSplitSpace.Strings[i] <> 'might' do inc(i);
          if (0 <= i-1) and (i-1 < SLSplitSpace.Count) then
            ExistingToDeclared(SLSplitSpace.Strings[i-1]);
          FreeAndNil(SLSplitSpace);
        end;
        inc(j);
      end;
      FreeAndNil(SLCompiled);
      SL.SaveToFile(Pathname);
    except
      on e: exception do ErrorMsg(e.Message);
    end;
    end else
      break;
  until false;
  Result:= Compiled;
end;

function TInteractiveExecuter.Compile(aExecution: TExecutionLine; normal: boolean): string;
begin
  MakePathname;
  if Normal
    then MakeClass(aExecution)      // instead of MakeClass / MakeThread
    else MakeClassExpression(aExecution);
  try
    SL.SaveToFile(Pathname);
  except
    on e: Exception do
      Errormsg(e.Message);
  end;
  var Compiled:= DoCompile;
  if Normal and (Left(Compiled, 4) = '-ERR') then
    Error(Compiled);
  Result:= Compiled;
end;

function TInteractiveExecuter.Run(aExecution: TExecutionLine): boolean;
  var aJavaClass: TComJavaClass;
      aJavaMethod: TComJavaMethod;
      aJavaValue: TComJavaValue;
begin
  Result:= false;
  if Left(Compile(aExecution, true), 3) = '+OK' then begin
    Result:= true;
    if ComJava.NewClass(aClassname, Pathname) then begin
      aJavaClass:= ComJava.GetClass(aClassname);
      aJavaMethod:= TComJavaMethod.create(aJavaClass, 'run', JNI.static, 'void', nil, ComJava);
      try
        // ShowCallMethod(theMethodName, theObjectname);
        aJavaValue:= aJavaMethod.call(nil);
        FreeAndNil(aJavaValue);
        if not aJavaMethod.IsValid then
          FMessages.OutputToTerminal(aJavaMethod.Error);
      finally
        FreeAndNil(aJavaMethod);
      end;
      UpdateVariables;
    end;
  end;
end;

procedure TInteractiveExecuter.GetExpression(aExecution: TExecutionLine);
  var aJavaClass: TComJavaClass;
      aJavaMethod: TComJavaMethod;
      aComJavaValue: TComJavaValue;
      Compiled, s: string;
begin
  // try method-call as an expression
  Compiled:= Compile(aExecution, false);
  if (Left(Compiled, 4) = '-ERR') and (Pos('''void'' type not allowed here', Compiled) > 0) then
    run(aExecution)
  else if Left(compiled, 3) = '+OK' then begin
    if ComJava.NewClass(aClassname, Pathname) then begin
      aJavaClass:= ComJava.GetClass(aClassname);
      aJavaMethod:= TComJavaMethod.create(aJavaClass, 'run', JNI.static, 'java.lang.Object', nil, ComJava);
      try
        aComJavaValue:= aJavaMethod.getExpressionValue;
        s:= aExecution.Value;
        if assigned(aComJavaValue)
          then FMessages.OutputToTerminal(copy(s, 1, length(s)-1) + ': ' + aComJavaValue.AsString)
          else FMessages.OutputToTerminal(aJavaMethod.Error);
      finally
        FreeAndNil(aJavaMethod);
        FreeAndNil(aComJavaValue);  //new
      end;
      UpdateVariables;
    end;
   end
  else begin
    s:= StripFile(compiled);
    s:= StripGetOb(s);
    FMessages.OutputToTerminal(s);
  end;
end;

procedure TInteractiveExecuter.AssignWithoutType(aExecution: TExecutionLine);
  var aJavaClass: TComJavaClass;
      aJavaMethod: TComJavaMethod;
      Compiled, s, varname: string;
      p: integer;
      aVariable: TVariable;
begin
  // do we know the variable?
  varname:= aExecution.Name;
  p:= Pos('.', varname);
  if p > 0 then delete(varname, p, length(varname));
  aVariable:= VariableList.get(varname);
  if assigned(aVariable) or FConfiguration.StrictJavaMode then begin
    Run(aExecution);
    exit;
  end;

  // try method-call as an expression
  s:= aExecution.Value;
  p:= Pos('=', s);
  aExecution.Value:= copy(s, p+1, length(s));
  Compiled:= Compile(aExecution, false);

  if Left(compiled, 3) = '+OK' then begin
    if ComJava.NewClass(aClassname, Pathname) then begin
      aJavaClass:= ComJava.GetClass(aClassname);
      aJavaMethod:= TComJavaMethod.create(aJavaClass, 'run', JNI.static, 'java.lang.Object', nil, ComJava);
      try
        aExecution.Typename:= aJavaMethod.getExpressionType;
        if aJavaMethod.IsValid then begin
          aExecution.Value:= aExecution.Typename + ' ' + s;
          if Pos(' new ', aExecution.value) > 0
            then CreateObject(aExecution)
            else CreateVariable(aExecution);
        end else
          FMessages.OutputToTerminal(aJavaMethod.Error);
      finally
        FreeAndNil(aJavaMethod);
      end;
    end;
  end else begin
    s:= StripFile(compiled);
    s:= StripGetOb(s);
    FMessages.OutputToTerminal(s);
  end;
end;

function TInteractiveExecuter.IsKnownType(const Typename: string): boolean;
begin
  Result:= isSimpleType(Typename) or isSimpleType(WithoutArray(Typename)) or Variablelist.isKnownType(Typename);
end;

procedure TInteractiveExecuter.DeclareVariable(aExecution: TExecutionLine);
  var Compiled: boolean; Variable: TVariable;
begin
  // Type Var;
  if isKnownType(aExecution.Typename)
    then Compiled:= true
    else Compiled:= (Left(Compile(aExecution, true), 3) = '+OK');
  if Compiled then begin
    Variable:= VariableList.get(aExecution.Name);
    if assigned(Variable)
      then Variable.Typ:= aExecution.Typename
      else VariableList.Add(TVariable.create(aExecution.Name, aExecution.Typename));
    AddVarToGrid(aExecution.Name, aExecution.Typename, '');
  end;
end;

function TInteractiveExecuter.CreateVariable(aExecution: TExecutionLine): boolean;
begin
  // create variable:  Type Var = Expression;
  Result:= false;
  var Variable:= VariableList.get(aExecution.Name);
  if assigned(Variable) then begin
    if Variable.Typ <> aExecution.Typename then begin
      Variable.Typ:= aExecution.Typename;
      Variable.existing:= false;
    end;
  end else begin
    Variable:= TVariable.create(aExecution.Name, aExecution.Typename);
    VariableList.Add(Variable);
    AddVarToGrid(aExecution.Name, aExecution.Typename, '');
  end;
  if Run(aExecution)
    then Result:= true
    else DelVariable(Variable.Name);
end;

procedure TInteractiveExecuter.CreateObject(aExecution: TExecutionLine);
begin
  // create object  Type obj = new Type(...);
  var Diagram:= UMLForm.MainModul.Diagram as TRtfdDiagram;
  var aVariable:= VariableList.get(aExecution.Name);
  if assigned(aVariable) then begin
    Diagram.DeleteObject(aVariable.Name);
    DelVariable(aExecution.Name);
  end;
  aVariable:= nil;

  if CreateVariable(aExecution) and assigned(ComJava.getObject(aExecution.Name)) then
    Diagram.ShowObject(aExecution.Name)
  else if aVariable = nil then
    DelVariable(aExecution.Name);
end;

procedure TInteractiveExecuter.CreateArray(aExecution: TExecutionLine);
  var s: string; p: integer; Variable: TVariable;
begin
  // create variable:  Type[] Var = {1, 2, 3}; literal array
  Variable:= VariableList.get(aExecution.Name);
  if assigned(Variable) then begin
    if Variable.Typ <> aExecution.Typename then begin
      Variable.Typ:= aExecution.Typename;
      Variable.existing:= false;
    end;
  end else begin
    Variable:= TVariable.create(aExecution.Name, aExecution.Typename);
    VariableList.Add(Variable);
    AddVarToGrid(aExecution.Name, aExecution.Typename, '');
  end;
  s:= aExecution.Value;
  p:= pos(' ' + aExecution.Name + ' ', s);
  insert('_', s, p+1);
  insert('_', s, p+3);
  s:= s + #13#10 + '    ' + aExecution.Name + ' = _' + aExecution.Name + '_;';
  aExecution.Value:= s;
  if not Run(aExecution) then
    DelVariable(Variable.Name);
end;

procedure TInteractiveExecuter.Execute(const s: string);
  var i: integer; aFile: string; ok, Update: boolean; SLFiles: TStringlist;
      ExecutionParser: TExecutionParser;
      aExecution: TExecutionLine;
      Diagram: TRtfdDiagram;
begin
  if s = '' then exit;

  SLFiles:= FJava.getAllPathnames;
  ok:= true;
  for i:= 0 to SLFiles.Count - 1 do begin
    aFile:= SLFiles.Strings[i];
    if hasJavaExtension(aFile) then begin
      if not FJava.PreCompile(nil, aFile) then begin
        ok:= false; break;
      end;
    end;
  end;
  FreeAndNil(SLFiles);
  if not ok then exit;

  SL.Text:= s;
  if ComJava = ComJava.getFirstComJava
    then Path:= FConfiguration.TempDir
    else Path:= ExtractFilePath(UMLForm.Pathname);
  Path:= IncludeTrailingPathDelimiter(Path);

  FMessages.ChangeTab(FMessages.TabControlMessages.TabIndex);
  FMessages.TBExecute.Enabled:= false;

  SLFiles:= FJava.getAllClassnames;
  for i:= 0 to SLFiles.Count - 1 do
    ComJava.NewClass(ChangeFileExt(ExtractFileName(SLFiles.Strings[i]), ''), SLFiles.Strings[i]);
  FreeAndNil(SLFiles);

  if assigned(UMLForm) then
    LockWindow(UMLForm.Handle);
  ExecutionParser:= TExecutionParser.Create(true);
  Update:= true;
  try
    ExecutionParser.prepareExecute(SL.Text, ExecutionList);
    for i:= 0 to ExecutionList.Count - 1 do begin
      // i have got exceptions with ExecutionList.Count = 1 and i = 1 !
      try
        aExecution:= ExecutionList.get(i);
        if UpdateVariables then begin
          case aExecution.Kind of
            MethodCall:          getExpression(aExecution);     // java.lang.Math.random();
            Statement:           Run(aExecution);               //  must be run(aExecution)
               //while (i>0) i--;  new Auto() im StrictJavaMode
            Assignment:          AssignWithoutType(aExecution); // i = 5+2*i;
            VariableDeclaration,                                // int i;
            ObjectDeclaration:   DeclareVariable(aExecution);   // Car car1;
            VariableCreation:    createVariable(aExecution);    // int i = 6;
            ObjectCreation:      createObject(aExecution);      // Car car = new Car("a car");
            Expression:          getExpression(aExecution);     // i + 4;
            ArrayDeclaration:    createArray(aExecution);       // int[] p = {1,2,3,4,4+5};
          end
        end else
          Update:= false;
      except on e: exception do
        continue;
      end;
    end;
  finally
    Executionlist.Clear;
    if assigned(UMLForm) and (UMLForm.Pathname <> '') and assigned(UMLForm.MainModul) and assigned(UMLForm.MainModul.Diagram) then begin
      Diagram:= UMLForm.MainModul.Diagram as TRtfdDiagram;
      if FConfiguration.ShowAllNewObjects then
        Diagram.ShowAllNewObjects(Self);
      Diagram.UpdateAllObjects;
    end;
    if Update then
      UpdateVariables;
    FMessages.TBExecute.Enabled:= true;
    SL.Clear;
    if assigned(UMLForm) then
      UnlockWindow;
    FreeAndNil(ExecutionParser);
    // debug je2java
    DeleteFiles(Pathname);
    Application.ProcessMessages;
  end;
end;

function TInteractiveExecuter.NeedsSemicolon(const s: string): boolean;
  var ExecutionParser: TExecutionParser;
      Kind: TExecutionType;
begin
  SL.Text:= s;
  ExecutionParser:= TExecutionParser.Create(true);
  try
    ExecutionParser.PrepareExecute(SL.Text, ExecutionList);
    if ExecutionList.Count = 1
      then Kind:= ExecutionList.get(0).Kind
      else Kind:= None;
    Result:= (Kind in [ObjectCreation, VariableCreation, Assignment,
                       VariableDeclaration, ObjectDeclaration, MethodCall]);
    // not None, Statement, Expression
  finally
    FreeAndNil(ExecutionParser);
    SL.Clear;
  end;
end;

function TInteractiveExecuter.UpdateVariables: boolean;
  var i, j, p: integer; s: string;
      aSL: TStringList; aClass: TComJavaClass;
begin
  VariableList.setNotExisting;
  s:= ComJava.ExecuteCommand('getVariables');
  if Left(s, 3) = '+OK' then begin
    aSL:= Split(#4, Right(s, 5));
    for i:= 0 to aSL.Count - 1 do begin
      s:= aSL.Names[i];
      p:= Pos('|', s);
      if p > 0 then begin // new object detected
        aClassname:= copy(s, 1, p-1);
        delete(s, 1, p);
        if ComJava.ObjectList.IndexOf(s) = -1 then begin
          aClass:= ComJava.GetClass(aClassname);
          if not assigned(aClass) then begin
            {$IFDEF WIN32}
            aClass:= TComJavaClass.CreateWithHandle(aClassname, ComJava);
            {$ELSE}
            aClass:= TComJavaClass.CreateWithHandle(aClassname, ComJava, true);
            {$ENDIF}
            ComJava.ClassList.AddObject(aClassname, aClass);
          end;
          ComJava.AddObject(s, aClass);
        end;
      end;
      DeclaredToExisting(s);
      j:= 1;
      while (j < SGVariables.RowCount) and (SGVariables.Cells[0, j] <> s) do
        inc(j);
      if j < SGVariables.RowCount then
        SGVariables.Cells[2,j]:= aSL.ValueFromIndex[i];
    end;
    FreeAndNil(aSL);
    Result:= true;
  end else
    Result:= false;
end;

function TInteractiveExecuter.getVariables: string;
  var s: string; i: integer; Variable: TVariable;
begin
  s:= '';
  for i:= 0 to VariableList.Count - 1 do begin
    Variable:= VariableList.get(i);
    if Variable.existing
      then s:= s + Variable.get + #13#10
      else s:= s + '    ' + Variable.Typ + ' ' + Variable.Name + ';' + #13#10;
  end;
  Result:= s;
end;

procedure TInteractiveExecuter.ExistingToDeclared(const Name: string);
begin
  var Variable:= VariableList.get(Name);
  if assigned(Variable) then
    Variable.existing:= false;
end;

procedure TInteractiveExecuter.DeclaredToExisting(const Name: string);
begin
  var Variable:= VariableList.get(Name);
  if assigned(Variable) then
    Variable.existing:= true;
end;

function TInteractiveExecuter.PutVariables: string;
  var s: string; i: integer; Variable: TVariable;
begin
  s:= '';
  for i:= 0 to VariableList.Count - 1 do begin
    Variable:= VariableList.get(i);
    s:= s + Variable.put + #13#10
  end;
  Result:= s;
end;

procedure TInteractiveExecuter.AddVarToGrid(const Name, Typ, Value: string);
begin
  try
    if assigned(SGVariables) and (SGVariables.ColCount >= 3) then begin
      var i:= SGVariables.Cols[0].IndexOf(Name);
      if i = -1 then begin // new object
        if SGVariables.Cells[0, 1] <> '' then
          SGVariables.RowCount:= SGVariables.RowCount + 1;
        i:= SGVariables.RowCount - 1;
        SGVariables.Cells[0, i]:= Name;
      end;
      // update object
      SGVariables.Cells[1, i]:= Typ;
      SGVariables.Cells[2, i]:= Value;
    end else
      FConfiguration.Log('AddVarToGrid SGVariables = nil');
    if SGVariables.ColCount > 3 then
      FConfiguration.Log('AddVarToGrid SGVariables.ColCount = ' + IntToStr(SGVariables.ColCount));
  except on e: exception do
    FConfiguration.Log('TInteractiveExecuter.AddVarToGrid ', e);
  end;
end;

procedure TInteractiveExecuter.Clear;
  var i: integer;
begin
  try
    if assigned(SGVariables) then begin
      SGVariables.RowCount:= 2;
      for i:= 0 to 2 do
        SGVariables.Cells[i, 1]:= '';
    if VariableList.Count > 0 then
      VariableList.Clear;
    end;
  except
  end;
end;

procedure TInteractiveExecuter.DelVariable(const Name: string);
begin
  VariableList.delete(Name);
  var i:= SGVariables.Cols[0].IndexOf(Name);
  if i > -1 then begin
    for var j:= i to SGVariables.RowCount - 2 do
      SGVariables.Rows[j]:= SGVariables.Rows[j+1];
    if SGVariables.RowCount = 2 then begin
      SGVariables.Cells[0,1]:= '';
      SGVariables.Cells[1,1]:= '';
      SGVariables.Cells[2,1]:= '';
    end else
      SGVariables.RowCount:= SGVariables.RowCount-1;
  end;
end;

procedure TInteractiveExecuter.AddVariable(const Name, Typ, Value: string);
begin
  var Variable:= VariableList.get(Name);
  if assigned(Variable)
    then Variable.Typ:= Typ
    else VariableList.add(TVariable.create(Name, Typ));
  AddVarToGrid(Name, Typ, Value);
end;

//function TInteractiveExecuter.getShell: TStringList;
//  var aSL: TStringList;
//begin
//  aSL:= TStringList.Create;
//  aSL.Add('class Eval {');
//  aSL.Add('public static Wrap getOb(Object o) {return new Wrap(o);}');
//  aSL.Add('protected static Object getOb(final String s) {return new Object() {public String result = s;};}');
//  aSL.Add('protected static Object getOb(final boolean b) {return new Object() {public boolean result = b;};}');
//  aSL.Add('protected static Object getOb(final byte b) {return new Object() {public byte result = b;};}');
//  aSL.Add('protected static Object getOb(final char c) {return new Object() {public char result = c;};}');
//  aSL.Add('protected static Object getOb(final double d) {return new Object() {public double result = d;};}');
//  aSL.Add('protected static Object getOb(final float f) {return new Object() {public float result = f;};}');
//  aSL.Add('protected static Object getOb(final int i) {return new Object() {public int result = i;};}');
//  aSL.Add('protected static Object getOb(final long l) {return new Object() {public long result = l;};}');
//  aSL.Add('protected static Object getOb(final short s) {return new Object() {public short result = s;};}');
//  aSL.Add('}');
//  aSL.Add('class Wrap { public Object result; Wrap(Object result) { this.result = result; } }');
//  Result:= aSL;
//end;

end.
