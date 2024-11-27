{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde
  Gerhard Röhner

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

unit UJavaParser;

// Java-Grammar
// http://java.sun.com/docs/books/jls/first_edition/html/index.html
// http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html

// The syntax {x} on the right-hand side of a production denotes zero or more occurrences of x.
// The syntax [x] on the right-hand side of a production denotes zero or one occurrences of x. That is, x is an optional symbol.
// The phrase (one of) on the right-hand side of a production signifies that each of the symbols on the following line or lines is an alternative definition.

interface

uses Classes, uCodeParser, uModel, uModelEntity,
  UStructure, UBaseForm, uIntegrator, UJavaScanner, UUtils;

type

  TJavaImporter = class(TImportIntegrator)
  private
    Parser: TCodeParser;
  public
    procedure NeedPackageHandler(var AName: string; aPackage: string;
      var AStream: TStream; OnlyLookUp: Boolean = False);
    procedure ImportOneFile(const FileName: string; withoutNeedSource: boolean = false); override;
    procedure ImportOneEditor(const FileName: string; Form: TFForm);
    class function GetFileExtensions: TStringList; override;
  end;

  TJavaParser = class(TCodeParser)
  private
    FUnit: TUnitPackage; // nil for Execution-Parser
    FOM: TObjectModel;
    ClassImports: TStringList; // import java.util.Vector
    FullImports: TStringList; // import java.util.*
    UserImportClasses: TStringList; // import javavis.base.*;

    ModAbstract: Boolean;
    ModStatic: Boolean;
    ModFinal: Boolean;
    ModVisibility: TVisibility;
    FFilename: string;
    FSourcepath: string;
    Packagename: string;
    WithView: Boolean;
    FInner: Boolean;
    FWithoutNeedSource: boolean;
    ScopeDepth: integer;
    FAnonymClassNr: integer;
    Depth: integer;
    FrameType: integer;

    procedure ParseCompilationUnit;
    procedure ParseTypeDeclaration;

    procedure ParseClassDeclaration(line: integer; IsInner: Boolean = False;
      const ParentName: string = '');
    procedure ParseClassBody(C: TClass);
    procedure ParseInterfaceDeclaration(line: integer; IsInner: Boolean = False;
      const ParentName: string = '');
    procedure ParseEnumDeclaration;
    procedure ParseBlock(O: TOperation);
    procedure ParseIfStatement(O: TOperation);
    procedure ParseElseIfStatement(O: TOperation);
    procedure ParseDoStatement(O: TOperation);
    procedure ParseWhileStatement(O: TOperation);
    procedure ParseForStatement(O: TOperation);
    procedure ParseTryStatement(O: TOperation);
    procedure ParseSwitchStatement(O: TOperation);
    procedure ParseSynchronizedStatement(O: TOperation);
    procedure ParseReturnStatement(O: TOperation);
    procedure ParseThrowStatement(O: TOperation);
    procedure ParseBreakStatement(O: TOperation);
    procedure ParseYieldStatement(O: TOperation);
    procedure ParseContinueStatement(O: TOperation);
    procedure ParseAssertStatement(O: TOperation);
    procedure ParseAssignment(O: TOperation);
    procedure ParseLambdaBody(O: TOperation);
    procedure ParseNew(O: TOperation);
    procedure ParseMethodInvocation(const Typename: string; O: TOperation);
    procedure ParseLocalVariableDeclaration(Typename: string; O: TOperation);
    procedure ParseExpressionOrLocalVariableDeclarationStatement(O: TOperation);

    procedure DoOperation(O: TOperation; const ParentName, Typename: string);
    procedure DoAttribute(A: TAttribute; const aGeneric: string; O: TOperation = nil);
    procedure ParseArgumentList(O: TOperation); overload;

    function GetClassName(s: string): string;
    procedure ReadFullImport(const Import: string);
    procedure CollectClassesForImport(const Import, Sourcepath, Package: string;
      UserImports: TStringList);
    procedure CollectDirClassesForImport(const Import, Sourcepath, Package: string;
      UserImports: TStringList);

    procedure SetVisibility(M: TModelEntity);
    function NeedSource(const SourceName, aPackagename: string): Boolean;
    function ShowView(IsInner: Boolean): Boolean;
    function ThreadAbort: Boolean;
    procedure AddStructure(from, _to: integer); overload;
    procedure AddStructure(from, _to: integer;
      SingleStatement: Boolean); overload;
    procedure AddStructureDefault;
    procedure CloseStructure(line: integer); overload;
    procedure CloseStructure(line: integer; SingleStatement: Boolean); overload;
    procedure CloseStructureDefault;
  protected
    procedure SwapArrFromTo(var s1, s2: string);
    procedure SkipTo(ch: char);
    procedure Skip(ch: char);
    procedure SkipPair(const open, close: string);
    function ParseAnnotations(C: TClass = nil): string;
    function ParseModifiers: string;
    function IsMethodModifier(const Token: string): Boolean;
    procedure ParseLocalClassModifiers;
    function GetTypeName: string;
    function IsStatementBegin(const Token: string): Boolean;
    function IsIdentifier(s: string): Boolean;
    function IsReservedWord(const s: string): Boolean;
    function IsTypename(const s: string): Boolean;
    function IsExpressionStatementOperator(const Op: string): Boolean;
    function IsAssignmentOperator(const Op: string): Boolean;
    function IsOperator(const Op: string): Boolean;
    function NeedClassifier(const CName: string; TheClass: TModelEntityClass = nil): TClassifier;
    procedure ParseBlockStatement(O: TOperation; NewStructure: Boolean);

  public
    CountClasses: integer;
    Scanner: TJavaScanner;
    Structures: TStructures;
    Filo: TList;
    constructor Create(aWithView: Boolean); virtual;
    destructor Destroy; override;
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage;
      AOM: TObjectModel; FileName: string; inner: Boolean; withoutNeedSource: boolean); overload; override;
    function Token: string;
    function GetNextToken: string;
    function GetImportName(const Typ: string): string;
  end;

implementation

uses Windows, Dialogs, Controls, Types, SysUtils, StrUtils, Character, Zip,
  uCodeProvider, UConfiguration;

{ TJavaImporter }

procedure TJavaImporter.ImportOneFile(const FileName: string; withoutNeedSource: boolean);
begin
  var Str := CodeProvider.LoadStream(FileName);
  if Assigned(Str) then begin
    Parser := TJavaParser.Create(true);
    try
      Parser.NeedPackage := NeedPackageHandler;
      Parser.Thread := nil;
      Parser.ParseStream(Str, Model.ModelRoot, Model, FileName, False, withoutNeedSource);
    finally
      FreeAndNil(Parser);
    end;
  end;
end;

procedure TJavaImporter.ImportOneEditor(const FileName: string; Form: TFForm);
begin
  var Str:= CodeProvider.LoadStream(FileName, Form);
  if Assigned(Str) then begin
    Parser := TJavaParser.Create(true);
    try
      Parser.NeedPackage := NeedPackageHandler;
      Parser.Thread := nil;
      Parser.ParseStream(Str, Model.ModelRoot, Model, FileName, false, false);
    finally
      FreeAndNil(Parser);
    end;
  end;
end;

procedure TJavaImporter.NeedPackageHandler(var AName: string; aPackage: string;
  var AStream: TStream; OnlyLookUp: Boolean = False);
begin
  AStream := nil;
  var FileName := AName + '.java';
  if aPackage <> '' then begin
    aPackage := ReplaceStr(aPackage, '.', '\');
    FileName := aPackage + '\' + FileName;
  end;

  FileName := CodeProvider.LocateFile(FileName);
  // Dont read same file twice
  if (not OnlyLookUp) and (FileName <> '') and (FilesRead.IndexOf(FileName) = -1)
  then begin
    AStream := CodeProvider.LoadStream(FileName);
    FilesRead.Add(FileName);
    AName := FileName;
  end;
end;

class function TJavaImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.java'] := 'Java';
end;

{ TJavaParser }

const
  ReservedWords: array [0 .. 53] of string = ('abstract', 'assert', 'boolean',
    'break', 'byte', 'case', 'catch', 'char', 'class', 'cons', 'continue',
    'default', 'double', 'do', 'else', 'enum', 'extends', 'false', 'final',
    'finally', 'float', 'for', 'goto', 'if', 'implements', 'import',
    'instanceof', 'int', 'interface', 'long', 'native', 'new', 'null',
    'package', 'private', 'protected', 'public', 'return', 'short', 'static',
    'strictfp', 'super', 'switch', 'synchronized', 'this', 'throw', 'throws',
    'transient', 'true', 'try', 'void', 'volatile', 'while', 'yield');

constructor TJavaParser.Create(aWithView: Boolean);
begin
  inherited Create;
  Self.WithView := aWithView;
  ClassImports := TStringList.Create;
  FullImports := TStringList.Create;
  UserImportClasses := TStringList.Create;
  UserImportClasses.Sorted := true;
  UserImportClasses.Duplicates := dupIgnore;
  Scanner := TJavaScanner.Create;
  Structures := TStructures.Create;
  FConfiguration.ImportCache.Clear;
  Filo := TList.Create;
  ScopeDepth := 0;
  FAnonymClassNr := 0;
end;

destructor TJavaParser.Destroy;
begin
  FreeAndNil(ClassImports);
  FreeAndNil(FullImports);
  FreeAndNil(UserImportClasses);
  FreeAndNil(Scanner);
  FreeAndNil(Structures);
  FreeAndNil(Filo);
  // not FreeAndNil(FOM); as FOM is the output of the parser
  // not FreeAndNil(FUnit); as FUnit belongs to the model
  inherited;
end;

function TJavaParser.GetNextToken: string;
begin
  Result := Scanner.GetNextToken;

  {if Scanner.Line <> Line then begin
    line:= Scanner.Line;
    FMessages.OutputToTerminal(IntTostr(Line));
  end;}

end;

function TJavaParser.Token: string;
begin
  Result := Scanner.Token;
end;

procedure TJavaParser.SkipPair(const open, close: string);
begin
  Scanner.SkipPair(open, close);
end;

procedure TJavaParser.ParseStream(AStream: TStream; AModel: TAbstractPackage;
  AOM: TObjectModel; FileName: string; inner: Boolean; withoutNeedSource: boolean);
  var aClassifier: TClassifier; aClass: TClass;
begin
  Scanner.Init(AStream);
  FModel := AModel;
  FOM := AOM;
  FFilename := FileName;
  FSourcepath := ExtractFilepath(FileName);
  FInner := inner;
  FWithoutNeedSource:= withoutNeedSource;
  FUnit := (FModel as TLogicPackage).FindUnitPackage('Default');
  if not Assigned(FUnit) then
    FUnit := (FModel as TLogicPackage).AddUnit('Default');
  aClassifier:= FUnit.FindClass(Filename);
  if assigned(aClassifier) and aClassifier.SourceRead then begin
    if aClassifier is TClass then begin
      aClass:= aClassifier as TClass;
      aClass.IsVisible:= true;
      FUnit.AddClass(aClass);
    end;
    exit;
  end;
  ParseCompilationUnit;
end;

(*
  QualifiedIdentifier:
  Identifier { . Identifier }
*)

function TJavaParser.ParseModifiers: string;
(*
  ModifiersOpt:
  { Modifier }

  Modifier:
  public
  protected
  private
  static
  abstract
  final
  native
  synchronized
  transient
  volatile
  strictfp
  sealed
  non-sealed
  < T1, T2, ... >
*)
begin
  Result := '';
  ModVisibility := viPackage;
  ModAbstract := False;
  ModStatic := False;
  ModFinal := False;
  while true do begin
    if Token = 'public' then
      ModVisibility := viPublic
    else if Token = 'protected' then
      ModVisibility := viProtected
    else if Token = 'private' then
      ModVisibility := viPrivate
    else if Token = 'static' then
      ModStatic := true
    else if Token = 'abstract' then
      ModAbstract := true
    else if Token = 'final' then
      ModFinal := true
    else if Token = '<' then begin
      SkipPair('<', '>');
      break
    end
    else if (Token = 'non') and (GetNextToken = '-') and (GetNextToken = 'sealed') then
      Scanner.Token:= 'non-sealed'
    else if (Token = 'native') or (Token = 'transient') or (Token = 'default')
      or (Token = 'volatile') or (Token = 'strictfp') or (Token = 'sealed')
      or (Copy(Token, 1, 1) = '@') then
    else if Token = 'synchronized' then begin
      if Scanner.LookAheadToken = '(' then begin
        Scanner.Token := 'synchronized';
        break;
      end;
    end
    else
      break;
    Result := Result + ' ' + Token;
    GetNextToken;
  end;
end;

function TJavaParser.IsMethodModifier(const Token: string): Boolean;
// ClassModifie
begin
  Result := (Token = 'public') or (Token = 'protected') or (Token = 'private')
    or (Token = 'static');
end;

procedure TJavaParser.ParseLocalClassModifiers;
begin
  while (Token = 'abstract') or (Token = 'final') or (Token = 'strictfp') do
    GetNextToken;
end;

procedure TJavaParser.ParseCompilationUnit;

(*
  CompilationUnit:
  [package QualifiedIdentifier   ;  ]
  {ImportDeclaration}
  {TypeDeclaration}
*)
begin
  GetNextToken;
  if Token = 'package' then begin
    Packagename := GetNextToken;
    // FullImports.add(Packagename + '.');
    CollectDirClassesForImport(Packagename, FSourcepath, Packagename, UserImportClasses);
    if not FInner then
      FUnit.ImportStartline := Scanner.line + 1;
    Scanner.SkipToken(';');
  end else begin
    Packagename := '';
    if not FInner then
      FUnit.ImportStartline := 1;
  end;
  if not FInner then
    FUnit.ImportEndline := -1;

  if (Token = 'import') and not FInner then
    FUnit.ImportStartline := Scanner.line;
  while (Token = 'import') and not ThreadAbort do begin
    (*
      ImportDeclaration
      import Identifier {   .   Identifier } [   .     *   ] ;
    *)
    var s := GetNextToken;
    if s = 'static' then
      s := GetNextToken;

    if not FInner then
      FUnit.ImportEndline := Scanner.line;
    if GetNextToken = '*' then begin
      FullImports.Add(s);
      s := Copy(s, 1, length(s) - 1);
      if FConfiguration.FixImports then
        ReadFullImport(s)
      else if not FConfiguration.IsAPIPackage(s) then
        CollectDirClassesForImport(s, FSourcepath, Packagename, UserImportClasses);
      GetNextToken;
    end else
      ClassImports.Values[ExtractClassName(s)] := ExtractPackageName(s);
    GetNextToken;
  end;

  FullImports.Add('java.lang.');
  if not FInner then begin
    AddStrings(FUnit.FullImports, FullImports);
    AddStrings(FUnit.ClassImports, ClassImports);
  end;

  while not ThreadAbort and (Token <> '') do
    ParseTypeDeclaration;
end;

procedure TJavaParser.ParseTypeDeclaration;
  (*
    TypeDeclaration:
    ClassOrInterfaceDeclaration or RecordDeclaration
    ;

    ClassOrInterfaceDeclaration:
    ModifiersOpt (ClassDeclaration | InterfaceDeclaration)

    InterfaceDeclaration:
    interface Identifier [extends TypeList] InterfaceBody

    RecordDeclaration:
    {ClassModifier} `record` TypeIdentifier [TypeParameters]
    RecordHeader [SuperInterfaces] RecordBody

  *)
begin
  Structures.Clear;
  Structures.MaxDepth := 0;
  while Token <> '' do begin
    var line := Scanner.TokenLine;
    ParseAnnotations;
    ParseModifiers;
    if (Token = 'class') or (Token = 'record') then
      ParseClassDeclaration(line)
    else if Token = 'enum' then
      ParseEnumDeclaration
    else if (Token = 'interface') or (Token = '@interface') then
      ParseInterfaceDeclaration(line)
    else // if token = 'void' Method declaration from the structogram area
      GetNextToken;
  end;
  while Filo.Count > 0 do begin
    var Structure := TStructureEx(Filo[Filo.Count - 1]);
    Filo.Delete(Filo.Count - 1);
    Structure.FTextRect.BottomRight := Point(0, Scanner.line);
    Structures.Add(Structure);
  end;
end;

procedure TJavaParser.ParseClassDeclaration(line: integer;
  IsInner: Boolean = False; const ParentName: string = '');
(*

  ClassDeclaration:
  NormalClassDeclaration
  EnumDeclaration

  NormalClassDeclaration:
  {ClassModifier} class TypeIdentifier [TypeParameters]
    [Superclass] [Superinterfaces] [PermittedSubclasse] ClassBody

  EnumDeclaration:
  {ClassModifier} enum TypeIdentifier [Superinterfaces] EnumBody

  RecordDeclaration:
  {ClassModifier} record TypeIdentifier [TypeParameters]
    RecordHeader [SuperInterfaces] RecordBody

*)
var
  C: TClass;
  Int: TInterface;
  aclass: TClassifier;
  Impl, Ext, aClassname, aGeneric: string;
  anonym, isRecord: Boolean;

begin
  // enum is not supported
  Inc(CountClasses);
  Inc(ScopeDepth);
  isRecord:= (Token = 'record');
  if Token = '{' then begin // anonym inner class
    anonym := true;
    Inc(FAnonymClassNr);
    aClassname := ParentName + '$' + IntToStr(FAnonymClassNr);
  end else begin
    anonym := False;
    GetNextToken;
    if IsInner
      then aClassname := ParentName + '$' + Token
      else aClassname := Token;
    GetNextToken;
    // [TypeParameters]
    if Token = '<' then begin
      aGeneric := Scanner.GetGeneric;
      aClassname := aClassname + '<' + aGeneric + '>';
    end;
  end;

  if (Packagename <> '') and (Pos(Packagename, aClassname) <> 1) then
    aClassname := Packagename + '.' + aClassname;
  C := FUnit.MakeClass(aClassname, FFilename);
  C.Documentation.Description := ReplaceStr(Scanner.Comment, #13#10, '<br>');
  Scanner.Comment := '';
  C.Importname := aClassname;
  C.aGeneric := aGeneric;
  C.inner := IsInner;
  C.anonym := anonym;
  C.IsVisible := ShowView(IsInner) and not anonym;
  C.LineS := line;
  C.LineSE := Scanner.line;
  C.ScopeDepth := ScopeDepth;
  if C.IsVisible then // what is with C if C ist not visible?
    FUnit.AddClass(C);
  SetVisibility(C);

  // Superclass:
  // extends ClassType
  if (Token = 'extends') and not isRecord and not ThreadAbort then begin
    Ext := GetNextToken;
    if EndsWith(Ext, 'Application') then
      FrameType := 8
    else if EndsWith(Ext, 'JApplet') then
      FrameType := 7
    else if EndsWith(Ext, 'JDialog') then
      FrameType := 6
    else if EndsWith(Ext, 'JFrame') then
      FrameType := 5
    else if EndsWith(Ext, 'Applet') then
      FrameType := 4
    else if EndsWith(Ext, 'Dialog') then
      FrameType := 3
    else if EndsWith(Ext, 'Frame') then
      FrameType := 2
    else
      FrameType := 1;
    Ext := GetImportName(Ext);
    GetNextToken;
    if Token = '<' then
      Ext := Scanner.GetGeneric(Ext);
    if C.Importname <> Ext then begin
      aclass := NeedClassifier(Ext, TClass);
      if Assigned(aclass) and (aclass is TClass) then begin
        C.Ancestor := aclass as TClass;
        // we need superclass for e.g. code completion
        if Assigned(C.Ancestor) then
          C.Ancestor.Importname := GetImportName(C.Ancestor.Name);
      end;
    end;
  end;

  if isRecord then begin
    if Token = '(' then
      SkipTo(')');
    GetNextToken;
  end;

  // Superinterfaces:
  // implements InterfaceType {, InterfaceType}
  if Token = 'implements' then
    repeat
      Impl := GetNextToken;
      Impl := GetImportName(Impl);
      GetNextToken;
      if Token = '<' then
        Impl := ChangeGenericType(Scanner.GetGeneric(Impl));

      aclass := NeedClassifier(Impl, TInterface);
      if Assigned(aclass) and (aclass is TInterface) then begin
        Int := aclass as TInterface;
        C.AddImplements(Int);
        if ShowView(true) then
          C.ViewImplements(Int);
      end;
    until (Token <> ',') or ThreadAbort;

  // PermittedSubclasses:
  if Token = 'permits' then begin
    GetNextToken;
    while GetNextToken = ',' do
      GetNextToken;
  end;

  if (Token = '{') and not ThreadAbort then begin
    AddStructure(line, Scanner.line);
    ParseClassBody(C);
    C.LineE := Scanner.line;
    CloseStructureDefault;
    GetNextToken;
  end;
  Dec(ScopeDepth);
  C.SourceRead:= true;
end;

procedure TJavaParser.ParseClassBody(C: TClass);
(*
  ClassBody:
  { {ClassBodyDeclaration} }

  ClassBodyDeclaration:
  ClassMemberDeclaration    s.u.
  InstanceInitializer       ok  // thows three are no "members"
  StaticInitializer         ok
  ConstructorDeclaration

  ClassMemberDeclaration:
  FieldDeclaration
  MethodDeclaration
  ClassDeclaration          ok
  InterfaceDeclaration      ok
  ;

*)
var
  Operation, OpTemp: TOperation;
  Attribute: TAttribute;
  Typename, Ident, Generic, Annotation, Modifiers: string;
  LineS, line: integer;
  TypeClass: TClassifier;
begin
  GetNextToken;
  while true and not ThreadAbort do begin
    line := Scanner.TokenLine;
    Annotation := ParseAnnotations(C);
    Modifiers := ParseModifiers;
    // StaticInitializer:
    // static Block
    if (Modifiers = ' static') and (Token = '{') then begin
      Operation := C.AddOperationWithoutType('static initializer');
      Operation.LineS := Line;
      AddStructure(line, Scanner.line);
      ParseBlock(Operation);
      CloseStructureDefault;
      GetNextToken;
    end
    // InstanceInitializer:
    // Block
    else if (Modifiers = '') and (Token = '{') then begin
      Operation := C.AddOperationWithoutType('instance initializer');
      Operation.LineS := Line;
      AddStructure(line, Scanner.line);
      ParseBlock(Operation);
      CloseStructureDefault;
      GetNextToken;
    end else if Token = ';' then
      GetNextToken
    else if (Token = '}') or (Token = '') then
      break
      // ClassDeclaration:
    else if (Token = 'class') or (Token = 'record') then // inner class
      ParseClassDeclaration(line, true, withoutGeneric(C.Name))
    else if Token = 'enum' then
      ParseEnumDeclaration
      // InterfaceDeclaration:
    else if Token = 'interface' then
      ParseInterfaceDeclaration(line, true, C.Name)
      // Field-, Method- or Constructor-Declaration
      // FieldDeclaration:
      // {FieldModifier} UnannType VariableDeclaratorList ;
      // MethodDeclaration:
      // {MethodModifier} MethodHeader MethodBody
      // ConstructorDeclaration:
      // {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
    else begin
      // must be typename or constructor
      LineS := line;
      // ConstructorDeclarator:
      // [TypeParameters] SimpleTypeName ( [ReceiverParameter ,] [FormalParameterList] )

      // MethodDeclaration:
      // {MethodModifier} MethodHeader MethodBody
      // MethodHeader:
      // Result MethodDeclarator [Throws]
      // TypeParameters {Annotation} Result MethodDeclarator [Throws]

      // FieldDeclaration:
      // {FieldModifier} UnannType VariableDeclaratorList ;

      // TypeParameters:
      // < TypeParameterList >
      if Token = '<' then // optional TypeParameters
        SkipPair('<', '>');

      ParseAnnotations;

      Typename:= GetTypeName;
      if (Typename = GetClassName(C.Name)) and (Token = '(') then begin
        Ident := Typename; // constructor
        Typename := '';
      end else begin
        Ident:= Token;
        GetNextToken;
        while Token = '[]' do begin // for FieldDeclaration
          Ident := Ident + Token;
          GetNextToken;
        end;
      end;

      if Token = '(' then begin // new Method/Constructor, start of parameters
        TypeClass:= nil;
        if (Typename <> '') and (Typename <> 'void') then
          TypeClass:= NeedClassifier(GetImportName(Typename));
        OpTemp := C.MakeOperation(Ident, TypeClass);
        OpTemp.LineS := LineS;
        OpTemp.Annotation := Annotation;
        DoOperation(OpTemp, C.Name, Typename);
        Operation:= C.FindOperation(OpTemp);
        if Operation = nil then begin
          Operation:= OpTemp;
          C.AddOperation(Operation);
        end else
          FreeAndNil(OpTemp);

        GetNextToken; // ')'
        while Token = '[]' do
          GetNextToken;

        if Token = 'throws' then begin
          GetNextToken;
          GetImportName(Token);
          GetNextToken;
          while Token = ',' do begin
            GetNextToken;
            GetImportName(Token);
            GetNextToken;
          end;
        end;

        while (Token <> ';') and (Token <> '{') and (Token <> '') do
          GetNextToken;
        AddStructure(line, Scanner.TokenLine);
        // either ; for abstract method or { for body
        if not ThreadAbort and (Token = '{') then begin
          ParseBlock(Operation);
          CloseStructureDefault;
          GetNextToken;
        end else
          CloseStructureDefault;
      end else // new attribute
        // FieldDeclaration:
        // {FieldModifier} UnannType VariableDeclaratorList ;
        if not ThreadAbort and IsIdentifier(Ident) and IsTypename(Typename) and (Ident <> Typename)
        then begin
          SwapArrFromTo(Ident, Typename);
          TypeClass:= NeedClassifier(GetImportName(Typename));
          Attribute := C.AddAttribute(Ident, TypeClass);
          Attribute.LineS := LineS;
          DoAttribute(Attribute, Generic);
          while not ThreadAbort and (Token = ',') do begin
            GetNextToken;
            Ident := Token;
            GetNextToken;
            while Token = '[]' do begin
              Ident := Ident + Token;
              GetNextToken;
            end;
            DoAttribute(C.AddAttribute(Ident, TypeClass), Generic);
          end;
          // attribute-declaration ends with ;
          // erroneous attribute declaration is catched here
          while (Token <> '') and (Token <> ';') do
            GetNextToken;
        end;
      Scanner.Comment := '';
    end;
  end;
end;

procedure TJavaParser.SkipTo(ch: char);
begin
  Scanner.SkipTo(ch);
end;

procedure TJavaParser.Skip(ch: char);
begin
  Scanner.SkipTo(ch);
  GetNextToken;
end;

procedure TJavaParser.ParseInterfaceDeclaration(line: integer;
  IsInner: Boolean = False; const ParentName: string = '');
(*
  InterfaceDeclaration:
  interface Identifier [extends TypeList] InterfaceBody

  InterfaceBody:
  { {InterfaceBodyDeclaration} }

  InterfaceBodyDeclaration:
  ;
  ModifiersOpt InterfaceMemberDecl

  InterfaceMemberDecl:
  InterfaceMethodOrFieldDecl
  void Identifier VoidInterfaceMethodDeclaratorRest
  ClassOrInterfaceDeclaration

  InterfaceMethodOrFieldDecl:
  Type Identifier InterfaceMethodOrFieldRest

  InterfaceMethodOrFieldRest:
  ConstantDeclaratorsRest ;
  InterfaceMethodDeclaratorRest

  InterfaceMethodDeclaratorRest:
  FormalParameters BracketsOpt [throws QualifiedIdentifierList]   ;

  VoidInterfaceMethodDeclaratorRest:
  FormalParameters [throws QualifiedIdentifierList]   ;
*)
var
  Int, aInt: TInterface;
  aClassifier: TClassifier;
  Typename, Ident, Ext, s, aGeneric: string;
  Operation, OpTemp: TOperation;
  Attribute: TAttribute;
  LineS: integer;
  firstAncestor: Boolean;
  TypeClass: TClassifier;
begin
  Inc(CountClasses);
  inc(ScopeDepth);
  GetNextToken;
  if IsInner then
    s := ParentName + '$' + Token
  else if Packagename = '' then
    s := Token
  else
    s := Packagename + '.' + Token;
  GetNextToken;

  if Token = '<' then begin
    aGeneric := Scanner.GetGeneric;
    s := s + '<' + aGeneric + '>';
  end;
  Int := FUnit.MakeInterface(s, FFilename);
  Int.Documentation.Description := ReplaceStr(Scanner.Comment, #13#10, '<br>');
  Scanner.Comment := '';
  Int.aGeneric := aGeneric;
  Int.LineS := line;
  Int.LineSE := Scanner.line;
  Int.ScopeDepth := ScopeDepth;
  SetVisibility(Int);
  Int.IsVisible := ShowView(IsInner);
  if Int.IsVisible then
    FUnit.AddInterface(Int);

  firstAncestor := true;
  if Token = 'extends' then
    repeat
      Ext := GetImportName(GetNextToken);
      // if Packagename <> '' then Ext:= Packagename + '.' + Ext;
      GetNextToken;
      if Token = '<' then
        Ext := ChangeGenericType(Scanner.GetGeneric(Ext));
      aClassifier := NeedClassifier(Ext, TInterface);
      if Assigned(aClassifier) and (aClassifier is TInterface) then
      begin
        aInt := aClassifier as TInterface; // toDo
        if firstAncestor then
        begin
          Int.Ancestor := aInt;
          Int.Ancestor.Importname := GetImportName(Int.Ancestor.Name);
          firstAncestor := False;
        end;
        if Assigned(Int) then
        begin
          Int.AddExtends(aInt);
          if ShowView(true) then
            Int.ViewExtends(aInt);
        end;
      end;
    until Token <> ',';
  Int.LineSE := Scanner.line;
  if Token = '{' then begin
    AddStructure(line, Scanner.line);
    GetNextToken;
    while true do begin
      line:= Scanner.line;
      ParseAnnotations;
      ParseModifiers;
      if Token = ';' then // empty
        GetNextToken
      else if Token = 'class' then // Inner class
        ParseClassDeclaration(line, true, Int.Name)
      else if Token = 'enum' then
        ParseEnumDeclaration
      else if Token = 'interface' then // Inner interface
        ParseInterfaceDeclaration(line, true, Int.Name)
      else if (Token = '}') or (Token = '') then begin
        // end of interface declaration
        Int.LineE := Scanner.line;
        CloseStructureDefault;
        GetNextToken;
        break;
      end else begin
        // Must be type of attr or return type of operation
        LineS := line;
        Typename := GetTypeName;
        Ident := Token;
        if GetNextToken = '(' then begin
          // Operation
          if (Typename <> '') and (Typename <> 'void')
            then TypeClass:= NeedClassifier(GetImportName(Typename))
            else TypeClass:= nil;

          OpTemp := Int.MakeOperation(Ident, TypeClass);
          OpTemp.LineS := LineS;
          DoOperation(OpTemp, Int.Name, Typename);
          Operation:= Int.FindOperation(OpTemp);
          if Operation = nil then begin
            Operation:= OpTemp;
            Int.AddOperation(Operation)
          end else
            FreeAndNil(OpTemp);

          GetNextToken;
          if Token = '{' then begin // default methods
            AddStructure(line, Scanner.TokenLine);
            ParseBlock(Operation);
            CloseStructureDefault;
            GetNextToken;
          end else  begin
            // Skip Throws if present
            while (Token <> ';') and (Token <> '') do
              GetNextToken;
            AddStructure(line, Scanner.TokenLine);
            CloseStructureDefault;
          end
        end else if IsIdentifier(Ident) and IsTypename(Typename) and
          (Ident <> Typename) then
        begin
          SwapArrFromTo(Ident, Typename);
          TypeClass:= NeedClassifier(GetImportName(Typename));
          Attribute := Int.AddAttribute(Ident, TypeClass);
          Attribute.LineS:= LineS;
          DoAttribute(Attribute, aGeneric);
          while Token = ',' do begin
            GetNextToken;
            Ident := Token;
            GetNextToken;
            DoAttribute(Int.AddAttribute(Ident, TypeClass), aGeneric);
          end;
          Scanner.Comment := '';
        end;
      end;
    end;
  end;
  Dec(ScopeDepth);
  Int.SourceRead:= true;
end;

procedure TJavaParser.ParseEnumDeclaration;
(*
  EnumDeclaration:
  {ClassModifier} enum TypeIdentifier [Superinterfaces] EnumBody
*)
begin
  GetNextToken;
  GetNextToken;
  if Token = 'implements' then
    repeat
      GetNextToken;
      GetNextToken;
    until Token <> ',';

  if Token = '{' then
    SkipPair('{', '}');
end;

function TJavaParser.NeedClassifier(const CName: string; TheClass: TModelEntityClass = nil): TClassifier;
var
  PName, ShortName: string;
  ci: IModelIterator;
  cent: TModelEntity;

  function AddAClass(const CName: string): TClassifier;
  var
    C: TClass;
    I: TInterface;
    classpathname: string;
  begin
    Result := nil;
    if TheClass = TInterface then begin
      I := FUnit.MakeInterface(CName, '');
      I.Importname := CName;
      FUnit.AddInterfaceWithoutShowing(I);
      Result := TClassifier(I);
    end else begin
      C := FUnit.MakeClass(CName, '');
      if not Assigned(C) then
        exit;
      C.Importname := CName;
      classpathname := FSourcepath + WithoutArray(CName) + '.java';
      if FileExists(classpathname) then
        C.Pathname := classpathname;
      FUnit.AddClassWithoutShowing(C);
      Result := TClassifier(C);
    end;
  end;

begin
  Result := nil;
  if not assigned(FUnit) then
    exit;

  Result:= FUnit.FindClassifier(CName, TheClass, true);
  if Assigned(Result)
    then exit
    else if IsSimpleType(CName) or (CName = 'java.lang.String') then begin
      Result := AddAClass(CName);
      exit;
    end;

  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);
  if PName = '' then begin
    ci := FUnit.GetClassifiers;
    while ci.HasNext do begin
      cent := ci.next;
      if ExtractClassName(cent.Name) = CName then begin
        Result := (cent as TClassifier);
        exit;
      end;
    end;
  end;

  if not FWithoutNeedSource and NeedSource(ShortName, PName) then begin
    Result:= FUnit.FindClassifier(ShortName, TheClass, true);
    if Assigned(Result) then exit;
  end;
  Result:= AddAClass(CName);
  if FWithoutNeedSource = true then
    Result.Pathname:= '';
end;

procedure TJavaParser.SetVisibility(M: TModelEntity);
begin
  M.Visibility := ModVisibility;
  M.Static := ModStatic;
  M.IsFinal := ModFinal;
  M.IsAbstract := ModAbstract;
end;

procedure TJavaParser.SwapArrFromTo(var s1, s2: string);
begin
  var p := Pos('[]', s1);
  while p > 0 do begin
    s2 := s2 + '[]';
    Delete(s1, p, 2);
    p := Pos('[]', s1);
  end;
end;

procedure TJavaParser.DoOperation(O: TOperation; const ParentName, Typename: string);
var
  ParType, Ident: string;
  Param: TParameter;
begin
  O.Documentation.Description := Scanner.Comment;
  O.Documentation.LineS := Scanner.CommentLineS;
  O.Documentation.LineE := Scanner.CommentLineE;
  O.Spalte:= Scanner.LastTokenColumn;
  O.hasComment := (Pos('/*', O.Documentation.Description) + Pos('//',
    O.Documentation.Description) = 1);
  O.ParentName := ParentName;
  O.ScopeDepth:= ScopeDepth;
  SetVisibility(O);
  if (O.ReturnValue = nil) and (Typename <> '') and (Typename <> 'void') then
    O.ReturnValue := NeedClassifier(GetImportName(Typename));
  if Assigned(O.ReturnValue) then
    O.OperationType := otFunction
  else if (GetClassName(ParentName) = O.Name) and (Typename = '') then
    O.OperationType := otConstructor
  else
    O.OperationType := otProcedure;
  // Parameterlist
  GetNextToken;
  while (Token <> '') and (Token <> ')') do begin
    if Token = 'final' then
      GetNextToken;
    ParType := GetTypeName;
    Ident := Token;
    SwapArrFromTo(Ident, ParType);
    // if ParType = 'E' then p:= 'java.lang.Object' else p:= ParType;
    Param := O.AddParameter(Ident);
    Param.LineS := Scanner.line;
    Param.Spalte:= Scanner.TokenColumn;
    Param.TypeClassifier := NeedClassifier(GetImportName(ParType));
    GetNextToken;
    if Token = ',' then
      GetNextToken;
  end;
  Scanner.Comment := '';
  Scanner.CommentLineS := 0;
  Scanner.CommentLineE := 0;
  O.LineE := Scanner.line;
end;

function TJavaParser.ParseAnnotations(C: TClass = nil): string;
begin
  Result := '';
  while Token = '@' do
  begin
    GetNextToken;
    if ((Token = 'Before') or (Token = 'BeforeEach') or (Token = 'Test') or
      (Token = 'ParameterizedTest') or (Token = 'After') or
      (Token = 'AfterEach')) and
      ((Pos('org.junit.Assert', FUnit.FullImports.Text) > 0) or
      (Pos('org.junit.Assert', FUnit.ClassImports.Text) > 0) or
      (Pos('org.junit.jupiter.api.Assertions', FUnit.FullImports.Text) > 0) or
      (Pos('org.junit.jupiter.api.Assertions', FUnit.ClassImports.Text) > 0))
      and Assigned(C) then
    begin
      C.isJUnitTestClass := true;
      if (Token = 'Test') or (Token = 'ParameterizedTest') then
        Result := Token;
    end;
    GetNextToken;
    if Token = '(' then
      SkipPair('(', ')');
  end;
end;

procedure TJavaParser.ParseBlock(O: TOperation);
begin
  Inc(ScopeDepth);
  GetNextToken;
  repeat
    ParseBlockStatement(O, true);
  until (Token = '}') or (Token = '') or IsMethodModifier(Token);
  O.setAttributeScope(ScopeDepth, Scanner.line);
  O.LineE := Scanner.line;
  Dec(ScopeDepth);
end;

procedure TJavaParser.ParseIfStatement(O: TOperation);
  // IfThenStatement: if ( Expression ) Statement
  // IfThenElseStatement: if ( Expression ) StatementNoShortIf else Statement
begin
  var line := Scanner.line;
  GetNextToken;
  if Token = '(' then
    Scanner.SkipCondition
  else if Token = ')' then
    GetNextToken;

  var SingleStatement := (Token <> '{');
  if SingleStatement
    then AddStructure(line, Scanner.LastTokenLine, SingleStatement)
    else AddStructure(line, Scanner.line, SingleStatement);
  ParseBlockStatement(O, False);
  if not SingleStatement then
    GetNextToken;
  if (Token = ';') and (Scanner.lookAheadToken = 'else') then
    GetNextToken;
  if Token = 'else' then begin
    line := Scanner.line;
    if SingleStatement
      then CloseStructure(line, False)
      else CloseStructure(Scanner.LastTokenLine, False);
    GetNextToken;
    // else if gleiche Ebene wie einfaches else
    // andere Variante in Versionen 18.19 bis 20.05
    if Token = 'if' then
      ParseElseIfStatement(O)
    else begin
      SingleStatement := (Token <> '{');
      if SingleStatement
        then AddStructure(line, Scanner.LastTokenLine, SingleStatement)
        else AddStructure(line, Scanner.line, SingleStatement);
      ParseBlockStatement(O, False);
      if not SingleStatement then
        GetNextToken;
      CloseStructure(Scanner.LastTokenLine)
    end
  end
  else
    CloseStructure(Scanner.LastTokenLine, SingleStatement);
end;

procedure TJavaParser.ParseElseIfStatement(O: TOperation);
begin
  var line := Scanner.line;
  GetNextToken;
  if Token = '(' then
    Scanner.SkipCondition;
  var SingleStatement := (Token <> '{');
  if SingleStatement
    then AddStructure(line, Scanner.LastTokenLine)
    else AddStructure(line, Scanner.line);
  ParseBlockStatement(O, False);
  if not SingleStatement then
    GetNextToken;
  if (Token = ';') and (Scanner.LookAheadToken = 'else') then
    GetNextToken;

  if Token = 'else' then begin
    if SingleStatement
      then line := Scanner.line
      else line := Scanner.LastTokenLine;
    CloseStructure(line, False);
    GetNextToken;
    if Token = 'if' then
      ParseElseIfStatement(O)
    else begin
      SingleStatement := (Token <> '{');
      if SingleStatement
        then AddStructure(line, Scanner.LastTokenLine)
        else AddStructure(line, Scanner.line);
      ParseBlockStatement(O, False);
      if not SingleStatement then
        GetNextToken;
      CloseStructure(Scanner.LastTokenLine, SingleStatement);
    end;
  end
  else if SingleStatement then
    CloseStructure(Scanner.line - 1, true)
  else
    CloseStructure(Scanner.LastTokenLine, False);
end;

procedure TJavaParser.ParseTryStatement(O: TOperation);
// TryStatement: try Block Catches | try Block Catchesopt Finally
// Catches     : CatchClause | Catches CatchClause
// CatchClause : catch ( FormalParameter ) Block
// Finally     : finally Block
var
  Typename, Ident: string;
  line: integer;
begin
  line := Scanner.line;
  GetNextToken;
  if Token = '(' then
  begin // try with ressources
    GetNextToken;
    Typename := Token;
    GetImportName(Typename);
    Scanner.SkipCondition;
  end;
  AddStructure(line, Scanner.line);
  ParseBlock(O);
  CloseStructureDefault;
  GetNextToken;
  while Token = 'catch' do begin
    line := Scanner.line;
    GetNextToken;
    if Token = '(' then begin
      Typename:= GetImportName(GetNextToken);
      Ident:= getNextToken;
      if assigned(FUnit) then // not for ExecutionParser
        DoAttribute(O.AddAttribute(Ident, NeedClassifier(Typename)), '', O);
      Scanner.SkipCondition;
    end;
    AddStructure(line, Scanner.line);
    ParseBlock(O);
    CloseStructureDefault;
    GetNextToken;
  end;
  if Token = 'finally' then begin
    line := Scanner.LastTokenLine;
    GetNextToken;
    AddStructure(line, Scanner.line);
    ParseBlock(O);
    CloseStructureDefault;
    GetNextToken;
  end;
end;

procedure TJavaParser.ParseSwitchStatement(O: TOperation);
(*
  SwitchStatement: switch ( Expression ) SwitchBlock
  SwitchBlock: { SwitchBlockStatementGroupsopt SwitchLabelsopt }
  SwitchBlockStatementGroup: SwitchLabels BlockStatements
  BlockStatements: BlockStatement {Blockstatement}
  SwitchLabel: case ConstantExpreson | case EnumConstantName : | default:
*)

var
  line: integer; lToken: string;

  procedure TraditionelSwitch;
  begin
    while (Token = 'case') or (Token = 'default') do begin
      line := Scanner.line;
      GetNextToken;
      SkipTo(':');
      AddStructure(line, Scanner.line);
      GetNextToken;
      while (Token = 'case') or (Token = 'default') do begin
        CloseStructureDefault;
        line := Scanner.line;
        GetNextToken;
        SkipTo(':');
        AddStructure(line, Scanner.line);
        GetNextToken;
      end;
      if Token <> '}' then
        repeat
          ParseBlockStatement(O, true);
          if Token = ';' then
            GetNextToken;
        until (Token = '}') or (Token = '') or (Token = 'case') or
          (Token = 'default');
      CloseStructure(Scanner.LastTokenLine, true);
    end;
  end;

  procedure NewSwitch;
  begin
    while (Token = 'case') or (Token = 'default') do begin
      line := Scanner.line;
      AddStructure(line, Scanner.line);
      while (Token <> '->') and not Scanner.empty do
        GetNextToken;
      GetNextToken;
      var SingleStatement:= (Token <> '{');
      ParseBlockStatement(O, false);
      if SingleStatement and (Token = ';') or
         not SingleStatement and (Token = '}') then
           GetNextToken;
      CloseStructure(Scanner.LastTokenLine, SingleStatement);
    end;
  end;

begin
  line := Scanner.line;
  GetNextToken;
  if Token = '(' then
    Scanner.SkipCondition;
  AddStructure(line, Scanner.line);
  if Token = '{' then begin
    GetNextToken;
    if (Token = 'case') or (Token = 'default') then begin
      line := Scanner.line;
      lToken:= Scanner.LookAheadToken(':->');
      if lToken = ':'
        then TraditionelSwitch
        else NewSwitch;
    end;
  end;
  CloseStructureDefault;
  GetNextToken;
end;

procedure TJavaParser.ParseWhileStatement(O: TOperation);
// WhileStatement: while ( Expression ) Statement
begin
  var line := Scanner.line;
  GetNextToken;
  if Token = '(' then
    Scanner.SkipCondition
  else if Token = ')' then
    GetNextToken;
  var SingleStatement := (Token <> '{');
  if SingleStatement
    then AddStructure(line, Scanner.LastTokenLine)
    else AddStructure(line, Scanner.line);
  ParseBlockStatement(O, False);
  if not SingleStatement then
    GetNextToken;
  if SingleStatement then
    CloseStructure(Scanner.LastTokenLine, SingleStatement) // Scanner.Line?
  else
    CloseStructure(Scanner.LastTokenLine, SingleStatement);
end;

procedure TJavaParser.ParseDoStatement(O: TOperation);
// DoStatement: do Statement while ( Expression ) ;
begin
  var line := Scanner.line;
  GetNextToken;
  var SingleStatement := (Token <> '{');
  if SingleStatement
    then AddStructure(line, Scanner.LastTokenLine)
    else AddStructure(line, Scanner.line);
  ParseBlockStatement(O, False);
  GetNextToken;
  if SingleStatement then
    line := Scanner.line
  else
    line := Scanner.LastTokenLine;
  if Token = 'while' then
  begin
    GetNextToken;
    if Token = '(' then
      Scanner.SkipCondition;
  end;
  CloseStructure(Scanner.line, False);
  Structures[Structures.Count - 1].FBottomTop := line;
  if Token = ';' then
    GetNextToken;
end;

procedure TJavaParser.ParseForStatement(O: TOperation);
var
  Typename, Ident, Importname: string;
  SingleStatement: Boolean;
  line: integer;
  Attr: TAttribute;
  // ForStatement: for (type variable = ; Expressionopt ; ForUpdateopt ) Statement
  // ForStatement: for (Type Object : Collection) Statement
begin
  line := Scanner.line;
  GetNextToken;
  Inc(ScopeDepth);
  if Token = '(' then begin
    Typename := GetNextToken;
    if Token = '<' then
      SkipPair('<', '>');
    if Typename <> ';' then begin
      Ident := GetNextToken;
      if Ident = '(' then   // for (match (Token.Colon); ; match (Token.Comma)) {..}
        SkipPair('(', ')')
      else begin
        GetNextToken;
        if ((Token = ':') or (Token = '=')) and assigned(FUnit) then begin
          ModVisibility := viPackage;
          Importname:= GetImportName(Typename);
          Attr:= O.AddAttribute(Ident, NeedClassifier(Importname));
          DoAttribute(Attr, '', O);
          Attr.LineS:= Scanner.LastTokenLine;
        end;
      end;
    end;
    if Token <> '{' then
      Scanner.SkipPairStopAt('(', ')', '{}');
  end;
  SingleStatement:= (Token <> '{');
  if SingleStatement
    then AddStructure(line, Scanner.LastTokenLine)
    else AddStructure(line, Scanner.line);
  ParseBlockStatement(O, False);
  if not SingleStatement then
    GetNextToken;
  CloseStructure(Scanner.LastTokenLine, SingleStatement);
  O.setAttributeScope(ScopeDepth, Scanner.line);
  Dec(ScopeDepth);
end;

procedure TJavaParser.ParseBreakStatement(O: TOperation);
// 14.14
// BreakStatement: break Identifier opt ;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseYieldStatement(O: TOperation);
// YieldStatement: yield Expression;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseContinueStatement(O: TOperation);
// ContinueStatement: continue Identifieropt ;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseAssertStatement(O: TOperation);
// AssertStatement: assert Expression1 ; | asser Expression1 : Expression2 ;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseReturnStatement(O: TOperation);
// ReturnStatement: return [Expression] ;
begin
  GetNextToken;
  // ParseExpression(O);
  if Token = 'new' then
    ParseNew(O)
  else if IsIdentifier(Token) then begin
    GetNextToken;
    if Token = '(' then
      ParseArgumentList(O);
  end else if Token = '(' then begin
    SkipPair('(', ')');
    if Token = '->' then
      ParseLambdaBody(O);
  end;
  Skip(';');
end;

procedure TJavaParser.parseLambdaBody(O: TOperation);
begin
  var myfree:= false;
  if O = nil then begin
    O:= TOperation.Create(nil);
    myfree:= true;
  end;
  GetNextToken;
  if Token = '{' then begin
    AddStructure(Scanner.line, Scanner.line);
    ParseBlock(O);
    CloseStructureDefault;
  end else
    ParseBlockStatement(O, False);
  if myfree then
    FreeAndNil(O);
end;

procedure TJavaParser.ParseNew(O: TOperation);
  var Typename: string;
      C: TClass;
(*
ClassInstanceCreationExpression:
  UnqualifiedClassInstanceCreationExpression
  ExpressionName . UnqualifiedClassInstanceCreationExpression
  Primary . UnqualifiedClassInstanceCreationExpression

UnqualifiedClassInstanceCreationExpression:
  new [TypeArguments] ClassOrInterfaceTypeToInstantiate ( [ArgumentList] ) [ClassBody]

ClassOrInterfaceTypeToInstantiate:
  {Annotation} Identifier {. {Annotation} Identifier} [TypeArgumentsOrDiamond]

TypeArgumentsOrDiamond:
  TypeArguments
  <>
*)
begin
  GetNextToken;
  if Token = '<' then SkipPair('<', '>');
  Typename:= GetTypeName;
  NeedClassifier(GetImportName(WithoutArray(Typename)));
  if Token = '<' then
    SkipPair('<', '>')
  else if Token = '<>' then
    GetNextToken;
  if Token = '(' then begin
    ParseArgumentList(O);
    if Token = '{' then begin
      AddStructure(Scanner.line, Scanner.line);
      //if (O = nil) or (O.Owner = nil) then begin
        C:= TClass.create(nil);
        try
          ParseClassBody(C);
        finally
          FreeAndNil(C);
        end;
      //end;
      //else ParseClassBody(O.Owner as TClass);
      CloseStructureDefault;
    end;
  end else if Token = '{' then // array declaration
    SkipPair('{', '}');
end;

procedure TJavaParser.ParseThrowStatement(O: TOperation);
// ThrowStatement: throw Expression ;
begin
  GetNextToken;
  Skip(';');
end;

procedure TJavaParser.ParseSynchronizedStatement(O: TOperation);
// SynchronizedStatement: synchronized ( Expression ) Block
begin
  GetNextToken;
  if Token = '(' then
    Scanner.SkipCondition;
  AddStructure(Scanner.line, Scanner.line);
  ParseBlock(O);
  CloseStructureDefault;
  GetNextToken;
end;

function TJavaParser.IsStatementBegin(const Token: string): Boolean;
begin
  var s := Token;
  Result := (s = 'if') or (s = 'do') or (s = 'while') or (s = 'for') or
    (s = 'try') or (s = 'switch') or (s = 'synchronized') or (s = 'return') or
    (s = 'throw') or (s = 'break') or (s = 'continue') or (s = 'assert');
end;

procedure TJavaParser.ParseBlockStatement(O: TOperation; NewStructure: Boolean);
// BlockStatement:
// LocalVariableDeclarationStatement
// ClassDeclaration (local class, declared in a method/constructor)
// Statement
begin
  if (Token = '}') or (Token = '') then
    exit;
  ParseAnnotations;
  ParseLocalClassModifiers; // not private/protected/public/static

  // labled statement
  if Scanner.LookAheadToken = ':' then begin
    GetNextToken;
    GetNextToken;
  end;
  if Token = 'class' then
    ParseClassDeclaration(Scanner.line, true, O.ParentName)
  else if Token = 'enum' then
    ParseEnumDeclaration
    // local, inner class
  else if Token = 'if' then
    ParseIfStatement(O)
  else if Token = 'while' then
    ParseWhileStatement(O)
  else if Token = 'for' then
    ParseForStatement(O)
  else if Token = '{' then begin // Blockstatement
     if NewStructure then
      AddStructureDefault;
    ParseBlock(O);
    if NewStructure then begin
      CloseStructureDefault;
      GetNextToken;
    end;
  end
  else if Token = ';' then // EmptyStatement
    GetNextToken
  else if Token = 'assert' then
    ParseAssertStatement(O)
  else if Token = 'switch' then
    ParseSwitchStatement(O)
  else if Token = 'do' then
    ParseDoStatement(O)
  else if Token = 'break' then
    ParseBreakStatement(O)
  else if Token = 'yield' then
    ParseYieldStatement(O)
  else if Token = 'continue' then
    ParseContinueStatement(O)
  else if Token = 'return' then
    ParseReturnStatement(O)
  else if Token = 'synchronized' then
    ParseSynchronizedStatement(O)
  else if Token = 'throw' then
    ParseThrowStatement(O)
  else if Token = 'try' then
    ParseTryStatement(O)
  else
    ParseExpressionOrLocalVariableDeclarationStatement(O);
  O.HasSourceCode := true;
end;

procedure TJavaParser.ParseExpressionOrLocalVariableDeclarationStatement
  (O: TOperation);
begin
  // ExpressionStatement: StatementExpression ;
  // StatementExpression: Assignment | PreIncrementExpression | PreDecrementExpression
  // PostIncrementExpression | PostDecrementExpression
  // MethodInvocation | ClassInstanceCreationExpression

  // ClassInstanceCreationExpression: new ClassType ( ArgumentListopt )

  // Assignment: LeftHandSide AssignmentOperator AssignmentExpression

  if Token = 'new' then // ClassInstanceCreationExpression
    ParseNew(O)
  else if (Token = '++') or (Token = '--') then // PreIncDecrementExpression
    Skip(';')
  else begin
    var Typename := GetTypeName;
    if Typename = 'final' then begin
      Typename := GetTypeName;
      ParseLocalVariableDeclaration(Typename, O);
      SkipTo(';');
    end else if (Typename = '(') and (Token = '(') then begin // ((Typecast) object).method;
      GetNextToken;
      GetImportName(Token);
      Skip(';');
    end else begin
      while (length(Token) > 0) and CharInSet(Token[1], ['.', '[']) do begin
        Typename := Typename + Token;
        GetNextToken;
      end;
      if (Token = '++') or (Token = '--') then  // PostIncDecExpression
        Skip(';')
      else if Token = '(' then  // MethodInvocation
        ParseMethodInvocation(Typename, O)
      else if IsAssignmentOperator(Token) then // Assignment
        ParseAssignment(O)
      else if (Token <> ';') and (Token <> '') and (Token <> '}') and (Token <> ')')
        and (Token <> '(') and not IsStatementBegin(Token) and not IsOperator(Token) then begin
          ParseLocalVariableDeclaration(Typename, O); // Variable Declaration
          SkipTo(';');
        end;
    end;
  end;
end;

procedure TJavaParser.ParseLocalVariableDeclaration(Typename: string;
  O: TOperation);
(*
  14.4
  LocalVariableDeclarationStatement: LocalVariableType VariableDeclaratorList
  VariableDeclaratorList: VariableDeclarator {, VariableDeclarator}
  VariableDeclarator:  VariableDeclaratorId [= VariableInitializer]
  VariableDeclaratorId: Identifier [Dims]
  VariableInitializer: Expression | ArrayInitializer
*)
var
  Ident, Generic: string;
  Attribute: TAttribute; col: integer;
  TypeClass: TClassifier;
begin
  col:= Scanner.LastTokenColumn;
  if Token = '<' then begin
    Generic := Scanner.GetGeneric;
    Typename := Typename + '<' + Generic + '>';
  end else
    Generic := '';
  Ident:= Token;
  GetNextToken;
  while Token = '[]' do begin
    Ident := Ident + Token;
    GetNextToken;
  end;
  if IsIdentifier(Ident) and (IsSimpleType(Typename) or IsIdentifier(Typename))
    and (length(Token) > 0) and CharInSet(Token[1], ['=', ',', ';']) then begin
      ModVisibility := viPackage;
      TypeClass:= NeedClassifier(GetImportName(Typename));
      Attribute := O.AddAttribute(Ident, TypeClass);
      DoAttribute(Attribute, Generic, O);
      Attribute.Static := False;
      Attribute.Visibility := viPackage;
      Attribute.LineS := Scanner.TokenLine;
      Attribute.Spalte:= col;
      while Token = ',' do begin
        GetNextToken;
        Ident := Token;
        GetNextToken;
        Attribute := O.AddAttribute(Ident, TypeClass);
        DoAttribute(Attribute, Generic, O);
        Attribute.Static := False;
        Attribute.Visibility := viPackage;
        Attribute.LineS := Scanner.TokenLine;
        Attribute.Spalte:= col;
      end;
      Scanner.Comment := '';
  end;
end;

procedure TJavaParser.ParseAssignment(O: TOperation);
begin
  // this is part of ParseExpression
  GetNextToken;
  if Token = 'new' then
    ParseNew(O)
  else if IsIdentifier(Token) then
    while IsIdentifier(Token) do begin
      GetNextToken;
      if Token = '(' then
        ParseArgumentList(O);
      if Token = '.' then
        GetNextToken;
    end
  else if (Token = '(') then begin // lambda
    GetNextToken;
    if (Token = ')') and (Scanner.LookAheadToken = '->') then begin
      GetNextToken;
      parseLambdaBody(O);
    end
  end;
  SkipTo(';');
  GetNextToken;
end;

procedure TJavaParser.ParseMethodInvocation(const Typename: string; O: TOperation);
var
  Mi1, Mi2: IModelIterator;
  Attr: TModelEntity;
  p: integer;
  aObject: string;
  isClass: Boolean;
begin
  p := Pos('.', Typename);
  aObject := Copy(Typename, 1, p - 1);
  if aObject <> '' then begin
    isClass := true;
    Mi1 := O.GetAttributes;
    while Mi1.HasNext do begin
      Attr := Mi1.next;
      if Attr.Name = aObject then
        isClass := False;
    end;
    if (O.Owner is TClassifier) then begin
      Mi2:= ((O.Owner as TModelEntity) as TClassifier).GetAttributes;
      while Mi2.HasNext do begin
        Attr:= Mi2.next;
        if Attr.Name = WithoutArray(aObject) then
          isClass:= False;
      end;
      if isClass then begin
        aObject:= GetImportName(aObject);
        NeedClassifier(aObject);
      end;
    end;
  end;
  ParseArgumentList(O);
  while Token = '.' do begin
    GetNextToken;
    GetNextToken;
    if Token = '(' then
      ParseArgumentList(O);
  end;
end;

procedure TJavaParser.ParseArgumentList(O: TOperation);
var
  bracket: integer;
  aClassname: string;
  myfree: boolean;
begin
  myfree:= false;
  if O = nil then begin
    O:= TOperation.create(nil);
    myFree:= true;
  end;

  GetNextToken;
  bracket := 0;
  while (Token <> ')') and (Token <> '') do begin
    if Token = '(' then begin // Typecast | method call | lambda
      GetNextToken;
      Inc(bracket);
      if Token = '(' then  begin
        GetNextToken;
        if IsTypename(Token) then
          GetImportName(Token);
        SkipTo(')');
        GetNextToken;
      end
      else if Token = ')' then begin // lambda
        GetNextToken;
        Dec(bracket);
        if Token = '->' then
          parseLambdaBody(O);
      end else begin
        SkipPair('(', ')');
        Dec(bracket);
      end;
    end else if Token = 'new' then
      ParseNew(O)
    else if Token = 'switch' then
      ParseSwitchStatement(O)
    else if Token = '{' then begin
      AddStructure(Scanner.line, Scanner.line);
      ParseBlock(O);
      CloseStructureDefault;
    end else if (Pos('.', Token) > 0) and (Pos('"', Token) <> 1) then begin
      aClassname := Copy(Token, 1, Pos('.', Token) - 1);
      GetImportName(aClassname);
      GetNextToken;
    end else if Token = '::' then begin
      GetNextToken;
      GetNextToken;
    end else begin
      GetNextToken;
      while (bracket > 0) and (Token = ')') do begin
        GetNextToken;
        Dec(bracket);
      end;
    end;
  end;
  GetNextToken;
  if myfree then
    FreeAndNil(O);
end;

function TJavaParser.IsAssignmentOperator(const Op: string): Boolean;
begin
  Result := (Op = '=') or (Op = '*=') or (Op = '/=') or (Op = '%=') or
    (Op = '+=') or (Op = '-=') or (Op = '<<=') or (Op = '>>=') or (Op = '>>>=')
    or (Op = '&=') or (Op = '^=') or (Op = '|=');
end;

function TJavaParser.IsOperator(const Op: string): Boolean;
begin
  Result := (Op = '*') or (Op = '/') or (Op = '%') or (Op = '+')
    or (Op = '-') or (Op = '<<') or (Op = '>>') or (Op = '>>>') or (Op = '<')
    or (Op = '>') or (Op = '<=') or (Op = '>M') or (Op = 'instanceof')
    or (Op = '==') or (Op = '!=') or (Op = '&') or (Op = '^') or (Op = '|')
    or (Op = '&&') or (Op = '||') or (Op = '?') or (Op = ':');
end;

function TJavaParser.IsExpressionStatementOperator(const Op: string): Boolean;
begin
  Result := IsAssignmentOperator(Op) or (Op = '(') or (Op = '++') or
    (Op = '--');
end;

function TJavaParser.IsReservedWord(const s: string): Boolean;
var
  Left, Mid, Right: integer;
begin
  Result := true;
  Left := 0;
  Right := High(ReservedWords);

  while Left <= Right do
  begin
    Mid := (Left + Right) div 2;
    if ReservedWords[Mid] = s then
      exit;
    if ReservedWords[Mid] > s then
      Right := Mid - 1
    else
      Left := Mid + 1;
  end;
  Result := False;
end;

function TJavaParser.IsIdentifier(s: string): Boolean;
begin
  Result := False;
  var I := Pos('<', s);
  if I > 0 then
    Delete(s, I, length(s));
  if length(s) = 0 then
    exit;
  if not (s[1].isLetter or (Pos(s[1], '_$') > 0)) then
    exit;
  for I := 2 to length(s) do
    if not (s[i].isLetterOrDigit or (Pos(s[i], '_.[]<>') > 0)) then
      exit;
  if s[length(s)] = '.' then
    exit;
  Result := not IsReservedWord(s);
end;

function TJavaParser.IsTypename(const s: string): Boolean;
var
  ci, it: IModelIterator;
  Attr: TAttribute;
  cent: TModelEntity;
begin
  Result:= true;
  if IsSimpleType(s) then
    exit;
  Result:= not IsReservedWord(s);
  if Result and Assigned(FUnit) then begin
    ci:= FUnit.GetClassifiers;
    while ci.HasNext do begin
      cent:= ci.next;
      it:= (cent as TClassifier).GetAttributes;
      while it.HasNext do  begin
        Attr:= it.next as TAttribute;
        if Attr.Name = s then
          Result:= False;
      end
    end;
  end;
end;

procedure TJavaParser.DoAttribute(A: TAttribute; const aGeneric: string; O: TOperation = nil);

  var Pos: PChar; p: integer; aToken, s: string;

  function GetValue: string;
  begin
    var s := '';
    while Pos < Scanner.CurrPos - 1 do begin
      s := s + Pos^;
      Pos := Pos + 1;
    end;
    Result := s;
  end;

begin
  if Assigned(A) then begin
    //A.LineS := Scanner.line;
    A.Spalte := Scanner.LastTokenColumn;
    A.ScopeDepth := ScopeDepth;
    // VariableDeclarator:
    // VariableDeclaratorId [= VariableInitializer]
    if Token = '=' then begin
      Pos := Scanner.CurrPos;
      (*
        VariableInitializer:
        Expression      - new is part of Expression - parseExpression!
        ArrayInitializer

        ClassInstanceCreationExpression:
        UnqualifiedClassInstanceCreationExpression
        ExpressionName . UnqualifiedClassInstanceCreationExpression
        Primary . UnqualifiedClassInstanceCreationExpression
      *)
      aToken := GetNextToken;
      if (aToken = 'new') or EndsWith(aToken, '.new') then
        ParseNew(O)
      else if (aToken = 'switch') then
        ParseSwitchStatement(O)
      else if IsIdentifier(Token) then begin
        while IsIdentifier(Token) do begin
          GetNextToken;
          if Token = '(' then
            ParseArgumentList(nil);
          if Token = '.' then
            GetNextToken;
        end;
        if Token = '->' then
          ParseLambdaBody(nil);
      end else if (Token = '(') then begin // lambda
        SkipPair('(', ')');
        if Token = '->' then
          ParseLambdaBody(nil);
      end else begin
        aToken := Token;
        p := System.Pos('.', aToken);
        if p > 0 then begin
          Delete(aToken, p, length(aToken));
          GetImportName(aToken);
        end;
      end;

      // skip unknown part until , (next VariableDeclarator) or ; (end of VariableDeclaratorList)
      while (Token <> ',') and (Token <> ';') and (Token <> '') and (Token <> '}') do begin
        if Token = '{' then
          SkipPair('{', '}')
        else if Token = '(' then
          ParseArgumentList(nil) // Parameterlist
        else
          GetNextToken;
      end;
      s:= trim(GetValue);
      p:= System.Pos(#13#10, s);
      if p > 0 then
       s:= copy(s, 1, p-1);
      A.Value := s;
    end;

    A.LineE := Scanner.line;
    if Assigned(A.TypeClassifier) then
      A.TypeClassifier.aGeneric := aGeneric;
    if Assigned(A.Documentation) then
      A.Documentation.Description := Scanner.Comment;
    SetVisibility(A);
  end;
end;

function TJavaParser.GetImportName(const Typ: string): string;
var
  I, p: integer;
  SL: TStringList;
  WithoutArray, withoutGeneric, gen, innergen, arr, complete: string;
begin
  Result := Typ;
  if ThreadAbort or IsSimpleType(Typ) or
    IsSimpleType(UUtils.WithoutArray(Typ)) or
    ((Pos('.', Typ) > 0) and (Pos('...', Typ) = 0)) or (Typ = '') then
    exit;

  p := Pos('<', Typ);
  if p > 0 then begin
    gen := Copy(Typ, p, length(Typ));
    withoutGeneric := Copy(Typ, 1, p - 1);
  end else begin
    gen := '';
    withoutGeneric := Typ;
  end;
  WithoutArray := withoutGeneric;
  arr := '';
  SwapArrFromTo(WithoutArray, arr);

  if WithoutArray = 'String' then begin
    Result := 'java.lang.' + Typ;
    exit;
  end;

  if gen <> '' then begin
    innergen := Copy(gen, 2, length(gen) - 2);
    SL := Split(',', innergen);
    for I := 0 to SL.Count - 1 do
      GetImportName(SL.Strings[I]);
    FreeAndNil(SL);
  end;

  complete := FConfiguration.getCompleteClassname(FullImports, ClassImports,
    UserImportClasses, WithoutArray);
  if complete <> '' then begin
    Result := complete + arr + gen;
    FConfiguration.ImportCache.Add(Typ + '=' + Result);
    exit;
  end;

  if FConfiguration.FixImports then begin
    // search in classpath
    Result := FConfiguration.SearchClassInClasspath(WithoutArray, FSourcepath, Packagename);
    if Result <> '' then  begin
      if Pos(FConfiguration.JavaCache + '\', Result) = 1 then
        Delete(Result, 1, length(FConfiguration.JavaCache) + 1);
      if ExtractFilepath(Result) = FSourcepath then
        exit;
      Result := ReplaceStr(ChangeFileExt(Result, ''), '\', '.');
      FConfiguration.ImportCache.Add(WithoutArray + '=' + Result);
      exit;
    end;

    // search in AllClasses
    p := FConfiguration.AllClasses.IndexOfName(WithoutArray);
    if (p > -1) and (p < FConfiguration.AllClasses.Count) then
    begin
      Result := FConfiguration.AllClasses.ValueFromIndex[p];
      if (FrameType = 8) and (Pos('.awt', Result) + Pos('.swing.', Result) > 0)
      then
        Result := FConfiguration.AllClasses.ValueFromIndex[p + 1];
      FConfiguration.ImportCache.Add(WithoutArray + '=' + Result);
      exit;
    end;

    // search in AllInterfaces
    p := FConfiguration.AllInterfaces.IndexOfName(WithoutArray);
    if (p > -1) and (p < FConfiguration.AllInterfaces.Count) then
    begin
      Result := FConfiguration.AllInterfaces.ValueFromIndex[p];
      if (FrameType = 8) and (Pos('.awt', Result) + Pos('.swing.', Result) > 0)
      then
        Result := FConfiguration.AllInterfaces.ValueFromIndex[p + 1];
      FConfiguration.ImportCache.Add(WithoutArray + '=' + Result);
      exit;
    end;
  end;
end;

procedure TJavaParser.ReadFullImport(const Import: string);
begin
  if not FConfiguration.IsAPIPackage(Import) then
    CollectClassesForImport(Import, FSourcepath, Packagename,
      UserImportClasses);
end;

{ $WARNINGS OFF }
procedure TJavaParser.CollectClassesForImport(const Import, Sourcepath,
  Package: string; UserImports: TStringList);
var
  PackageDir, cp, cp1, classnam: string;
  p: integer;
  found: Boolean;

  procedure CollectInJarFile(const JarFilename, Importname: string);
  var
    JarFile: TZipFile;
    s: string;
    I: integer;
  begin
    JarFile:= TZipFile.Create;
    try
      try
        JarFile.open(JarFilename, zmRead);
        for I := 0 to JarFile.FileCount - 1 do begin
          s := JarFile.FileNames[I];
          if Pos(Importname, s) = 1 then begin
            found := true;
            classnam := ChangeFileExt(Copy(s, length(Importname) + 2,
              length(s)), '');
            if (Pos('\', classnam) = 0) and (Pos('$', classnam) = 0) then
              UserImports.Add(classnam + '=' +
                ReplaceStr(ChangeFileExt(s, ''), '\', '.'));
          end;
          if ThreadAbort then
            break;
        end;
      except
      end;
    finally
      FreeAndNil(JarFile);
    end;
  end;

  procedure CollectInDirectory(const cp1, Importname, Ext: string);
  var
    fn, path, s, srn: string;
    SR: TSearchRec;
  begin
    path := cp1 + Importname;
    fn := path + Ext;
    if FindFirst(fn, 0, SR) = 0 then
    begin
      found := true;
      Delete(path, 1, length(PackageDir));
      path := ReplaceStr(path, '\', '.');
      repeat
        srn := ChangeFileExt(SR.Name, '');
        s := path + '.' + srn;
        if Pos('$', s) = 0 then
          UserImports.Add(srn + '=' + s);
      until ThreadAbort or (FindNext(SR) <> 0);
    end;
    FindClose(SR);
  end;

begin
  found := False;
  PackageDir := IncludeTrailingPathDelimiter
    (FConfiguration.getPackageDirectoryRelativ(Sourcepath, Package));
  cp := UnHideBlanks(FConfiguration.getClassPathJarExpanded(Sourcepath,
    Package)) + ';';
  p := Pos(';', cp);
  while (p > 0) and not found do
  begin
    cp1 := Copy(cp, 1, p - 1);
    Delete(cp, 1, p);
    if (ExtractFileExt(cp1) = '.jar') and (ExtractFilename(cp1) <> 'rt.jar') and
      FileExists(cp1) then
      CollectInJarFile(cp1, ReplaceStr(Import, '.', '\'))
    else
    begin
      cp1 := withTrailingSlash(cp1);
      CollectInDirectory(cp1, ReplaceStr(Import, '.', '\'), '\*.class');
      CollectInDirectory(cp1, ReplaceStr(Import, '.', '\'), '\*.java');
    end;
    p := Pos(';', cp);
  end;
end;

procedure TJavaParser.CollectDirClassesForImport(const Import, Sourcepath,
  Package: string; UserImports: TStringList);
var
  PackageDir, cp, cp1: string;
  p: integer;
  found: Boolean;

  procedure CollectInDirectory(const cp1, Importname, Ext: string);
  var
    fn, path, s, srn: string;
    SR: TSearchRec;
  begin
    path := cp1 + Importname;
    fn := path + Ext;
    if FindFirst(fn, 0, SR) = 0 then
    begin
      found := true;
      Delete(path, 1, length(PackageDir));
      path := ReplaceStr(path, '\', '.');
      repeat
        srn := ChangeFileExt(SR.Name, '');
        s := path + '.' + srn;
        if Pos('$', s) = 0 then
          UserImports.Add(srn + '=' + s);
      until ThreadAbort or (FindNext(SR) <> 0);
    end;
    FindClose(SR);
  end;

begin
  found := False;
  PackageDir := IncludeTrailingPathDelimiter
    (FConfiguration.getPackageDirectorySecure(Sourcepath, Package));
  cp := UnHideBlanks(FConfiguration.getClassPath(Sourcepath, Package)) + ';';
  p := Pos(';', cp);
  while (p > 0) and not found do
  begin
    cp1 := Copy(cp, 1, p - 1);
    Delete(cp, 1, p);
    if not(ExtractFileExt(cp1) = '.jar') and not EndsWith(cp1, '*') then
    begin
      cp1 := withTrailingSlash(cp1);
      CollectInDirectory(cp1, ReplaceStr(Import, '.', '\'), '\*.class');
      CollectInDirectory(cp1, ReplaceStr(Import, '.', '\'), '\*.java');
    end;
    p := Pos(';', cp);
  end;
end;

function TJavaParser.GetTypeName: string;
begin
  Result := Scanner.GetTypeName;
end;

function TJavaParser.GetClassName(s: string): string;
begin
  Delete(s, 1, LastDelimiter('.', s));
  Delete(s, 1, LastDelimiter('$', s));
  var p := Pos('<', s);
  if p > 0 then // classname<generic>
    Delete(s, p, length(s));
  Result := s;
end;

function TJavaParser.NeedSource(const SourceName, aPackagename: string): Boolean;
var
  Str: TStream;
  Parser: TJavaParser;
  s: string;
begin
  Result := False;
  if Assigned(NeedPackage) then begin
    s := WithoutArray(SourceName);
    NeedPackage(s, aPackagename, Str);
    if Assigned(Str) then begin
      Parser := TJavaParser.Create(false);  // war false
      try
        Parser.NeedPackage := NeedPackage;
        if s <> SourceName then begin
          Parser.ParseStream(Str, FOM.ModelRoot, FOM, s, true, true);
          FOM.ModelRoot.Files.Add(s);
        end else
          Parser.ParseStream(Str, FOM.ModelRoot, FOM, FFilename, true, true);
      finally
        FreeAndNil(Parser);
      end;
      Result := true;
    end;
  end;
end;

function TJavaParser.ShowView(IsInner: Boolean): Boolean;
begin
  Result := WithView;
  if FConfiguration.ShowPublicOnly and (ModVisibility <> viPublic) then
    Result := False;
  Result := Result or FConfiguration.ShowAlways;
  if IsInner then
    Result := true;
end;

function TJavaParser.ThreadAbort: Boolean;
begin
  Result := Assigned(Thread) and Thread.Abort;
  {if Result then begin
    FJava.Memo1.Lines.Add('THREAD ABORT at line: ' + IntTostr(scanner.line));
    FJava.memo1.Lines.add('THREAD STATE: ' + IntToStr(Thread.State));
  end;
  }
end;

procedure TJavaParser.AddStructure(from, _to: integer);
begin
  Inc(Depth);
  if Depth > Structures.MaxDepth then
    Structures.MaxDepth := Depth;
  var Structure := Structures.NewStructure;
  Structure.FTextRect.TopLeft := Point((Depth - 1) * FConfiguration.Indent
    + 1, from);
  Structure.FTopBottom := _to;
  Structure.FTopBottomRight := Scanner.LastTokenColumn;
  Structure.FDepth := Depth;
  Filo.Add(Structure);
end;

procedure TJavaParser.AddStructure(from, _to: integer;
  SingleStatement: Boolean);
begin
  AddStructure(from, _to);
  var Structure := TStructureEx(Filo[Filo.Count - 1]);
  Structure.SingleStatement := SingleStatement;
end;

procedure TJavaParser.AddStructureDefault;
begin
  AddStructure(Scanner.TokenLine, Scanner.TokenLine);
end;

procedure TJavaParser.CloseStructure(line: integer);
begin
  if Filo.Count > 0 then begin
    var Structure := TStructureEx(Filo[Filo.Count - 1]);
    Filo.Delete(Filo.Count - 1);
    Structure.FTextRect.BottomRight :=
      Point((Depth - 1) * FConfiguration.Indent + 1, line);
    Structure.FBottomTop := line;
    Structures.Add(Structure);
    Dec(Depth);
  end;
end;

procedure TJavaParser.CloseStructure(line: integer; SingleStatement: Boolean);
begin
  CloseStructure(line);
  Structures[Structures.Count - 1].SingleStatement := SingleStatement;
end;

procedure TJavaParser.CloseStructureDefault;
begin
  CloseStructure(Scanner.TokenLine, False);
end;

initialization

Integrators.Register(TJavaImporter);

end.
