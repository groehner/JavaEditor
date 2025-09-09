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

// The syntax {x} on the right-hand side of a production denotes zero or more
// occurrences of x.
// The syntax [x] on the right-hand side of a production denotes zero or one
// occurrences of x. That is, x is an optional symbol.
// The phrase (one of) on the right-hand side of a production signifies that
// each of the symbols on the following Line or lines is an alternative definition.

interface

uses
  Classes,
  UCodeParser,
  UModel,
  UModelEntity,
  UStructure,
  UBaseForm,
  UIntegrator,
  UJavaScanner,
  UUtils;

type

  TJavaImporter = class(TImportIntegrator)
  private
    FParser: TCodeParser;
  public
    procedure NeedPackageHandler(var AName: string; Package: string;
      var AStream: TStream; OnlyLookUp: Boolean = False);
    procedure ImportOneFile(const FileName: string;
      WithoutNeedSource: Boolean = False); override;
    procedure ImportOneEditor(const FileName: string; Form: TFForm);
    class function GetFileExtensions: TStringList; override;
  end;

  TJavaParser = class(TCodeParser)
  private
    FUnit: TUnitPackage; // nil for Execution-Parser
    FOM: TObjectModel;
    FClassImports: TStringList; // import java.util.Vector
    FFullImports: TStringList; // import java.util.*
    FUserImportClasses: TStringList; // import javavis.base.*;
    FModAbstract: Boolean;
    FModStatic: Boolean;
    FModFinal: Boolean;
    FModVisibility: TVisibility;
    FFilename: string;
    FSourcepath: string;
    FPackagename: string;
    FWithView: Boolean;
    FInner: Boolean;
    FWithoutNeedSource: Boolean;
    FScopeDepth: Integer;
    FAnonymClassNr: Integer;
    FCountClasses: Integer;
    FDepth: Integer;
    FFilo: TList;
    FFrameType: Integer;
    FScanner: TJavaScanner;
    FStructures: TStructures;

    procedure ParseCompilationUnit;
    procedure ParseTypeDeclaration;
    procedure ParseClassDeclaration(Line: Integer; IsInner: Boolean = False;
      const Parentname: string = '');
    procedure ParseClassBody(AClass: TClass);
    procedure ParseInterfaceDeclaration(Line: Integer; IsInner: Boolean = False;
      const Parentname: string = '');
    procedure ParseEnumDeclaration;
    procedure ParseBlock(Operation: TOperation);
    procedure ParseIfStatement(Operation: TOperation);
    procedure ParseElseIfStatement(Operation: TOperation);
    procedure ParseDoStatement(Operation: TOperation);
    procedure ParseWhileStatement(Operation: TOperation);
    procedure ParseForStatement(Operation: TOperation);
    procedure ParseTryStatement(Operation: TOperation);
    procedure ParseSwitchStatement(Operation: TOperation);
    procedure ParseSynchronizedStatement(Operation: TOperation);
    procedure ParseReturnStatement(Operation: TOperation);
    procedure ParseThrowStatement(Operation: TOperation);
    procedure ParseBreakStatement(Operation: TOperation);
    procedure ParseYieldStatement(Operation: TOperation);
    procedure ParseContinueStatement(Operation: TOperation);
    procedure ParseAssertStatement(Operation: TOperation);
    procedure ParseAssignment(Operation: TOperation);
    procedure ParseLambdaBody(Operation: TOperation);
    procedure ParseNew(Operation: TOperation);
    procedure ParseMethodInvocation(const Typename: string;
      Operation: TOperation);
    procedure ParseLocalVariableDeclaration(Typename: string;
      Operation: TOperation);
    procedure ParseExpressionOrLocalVariableDeclarationStatement
      (Operation: TOperation);

    procedure DoOperation(Operation: TOperation;
      const Parentname, Typename: string);
    procedure DoAttribute(Attribute: TAttribute; const AGeneric: string;
      Operation: TOperation = nil);
    procedure ParseArgumentList(Operation: TOperation); overload;

    function GetClassName(Str: string): string;
    procedure ReadFullImport(const Import: string);
    procedure CollectClassesForImport(const Import, Sourcepath, Package: string;
      UserImports: TStringList);
    procedure CollectDirClassesForImport(const Import, Sourcepath,
      Package: string; UserImports: TStringList);

    procedure SetVisibility(Model: TModelEntity);
    function NeedSource(const SourceName, Packagename: string): Boolean;
    function ShowView(IsInner: Boolean): Boolean;
    function ThreadAbort: Boolean;
    procedure AddStructure(From, To_: Integer); overload;
    procedure AddStructure(From, To_: Integer;
      SingleStatement: Boolean); overload;
    procedure AddStructureDefault;
    procedure CloseStructure(Line: Integer); overload;
    procedure CloseStructure(Line: Integer; SingleStatement: Boolean); overload;
    procedure CloseStructureDefault;
  protected
    procedure SwapArrFromTo(var Str1, Str2: string);
    procedure SkipTo(Chr: Char);
    procedure Skip(Chr: Char);
    procedure SkipPair(const Open, Close: string);
    function ParseAnnotations(AClass: TClass = nil): string;
    function ParseModifiers: string;
    function IsMethodModifier(const Token: string): Boolean;
    procedure ParseLocalClassModifiers;
    function GetTypeName: string;
    function IsStatementBegin(const Token: string): Boolean;
    function IsIdentifier(Ident: string): Boolean;
    function IsReservedWord(const RWord: string): Boolean;
    function IsTypename(const AType: string): Boolean;
    function IsExpressionStatementOperator(const Operator: string): Boolean;
    function IsAssignmentOperator(const Operator: string): Boolean;
    function IsOperator(const Operator: string): Boolean;
    function NeedClassifier(const CName: string;
      TheClass: TModelEntityClass = nil): TClassifier;
    procedure ParseBlockStatement(Operation: TOperation; NewStructure: Boolean);
  public
    constructor Create(WithView: Boolean); virtual;
    destructor Destroy; override;
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage;
      AOM: TObjectModel; FileName: string; Inner: Boolean;
      WithoutNeedSource: Boolean); overload; override;
    function Token: string;
    function GetNextToken: string;
    function GetImportName(const Typ: string): string;
    property CountClasses: Integer read FCountClasses;
    property Filo: TList read FFilo;
    property Scanner: TJavaScanner read FScanner;
    property Structures: TStructures read FStructures;
  end;

implementation

uses
  Windows,
  Types,
  System.IOUtils,
  System.SysUtils,
  System.StrUtils,
  Character,
  Zip,

  UConfiguration;

{ TJavaImporter }

procedure TJavaImporter.ImportOneFile(const FileName: string;
  WithoutNeedSource: Boolean);
begin
  var
  AStream := CodeProvider.LoadStream(FileName);
  if not Assigned(AStream) then
    Exit;

  FParser := TJavaParser.Create(True);
  try
    FParser.NeedPackage := NeedPackageHandler;
    FParser.Thread := nil;
    FParser.ParseStream(AStream, Model.ModelRoot, Model, FileName, False,
      WithoutNeedSource);
  finally
    FParser.Free;
  end;
end;

procedure TJavaImporter.ImportOneEditor(const FileName: string; Form: TFForm);
begin
  var
  AStream := CodeProvider.LoadStream(FileName, Form);
  if not Assigned(AStream) then
    Exit;

  FParser := TJavaParser.Create(True);
  try
    FParser.NeedPackage := NeedPackageHandler;
    FParser.Thread := nil;
    FParser.ParseStream(AStream, Model.ModelRoot, Model, FileName,
      False, False);
  finally
    FreeAndNil(FParser);
  end;
end;

procedure TJavaImporter.NeedPackageHandler(var AName: string; Package: string;
  var AStream: TStream; OnlyLookUp: Boolean = False);
begin
  AStream := nil;
  var
  FileName := AName + '.java';
  if Package <> '' then
  begin
    Package := ReplaceStr(Package, '.', '\');
    FileName := Package + '\' + FileName;
  end;

  FileName := CodeProvider.LocateFile(FileName);
  // Dont read same file twice
  if (not OnlyLookUp) and (FileName <> '') and
    (FFilesRead.IndexOf(FileName) = -1) then
  begin
    AStream := CodeProvider.LoadStream(FileName);
    FFilesRead.Add(FileName);
    AName := FileName;
  end;
end;

class function TJavaImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.java'] := 'Java';
end;

{ TJavaParser }

const ReservedWords: array [0 .. 53] of string = ('abstract', 'assert',
    'boolean', 'break', 'byte', 'case', 'catch', 'char', 'class', 'cons',
    'continue', 'default', 'double', 'do', 'else', 'enum', 'extends', 'false',
    'final', 'finally', 'float', 'for', 'goto', 'if', 'implements', 'import',
    'instanceof', 'int', 'interface', 'long', 'native', 'new', 'null',
    'package', 'private', 'protected', 'public', 'return', 'short', 'static',
    'strictfp', 'super', 'switch', 'synchronized', 'this', 'throw', 'throws',
    'transient', 'true', 'try', 'void', 'volatile', 'while', 'yield');

constructor TJavaParser.Create(WithView: Boolean);
begin
  inherited Create;
  Self.FWithView := WithView;
  FClassImports := TStringList.Create;
  FFullImports := TStringList.Create;
  FUserImportClasses := TStringList.Create;
  FUserImportClasses.Sorted := True;
  FUserImportClasses.Duplicates := dupIgnore;
  FScanner := TJavaScanner.Create;
  FStructures := TStructures.Create;
  FConfiguration.ImportCache.Clear;
  FFilo := TList.Create;
  FScopeDepth := 0;
  FAnonymClassNr := 0;
end;

destructor TJavaParser.Destroy;
begin
  FreeAndNil(FClassImports);
  FreeAndNil(FFullImports);
  FreeAndNil(FUserImportClasses);
  FreeAndNil(FScanner);
  FreeAndNil(FStructures);
  FreeAndNil(FFilo);
  // not FreeAndNil(FOM); as FOM is the output of the parser
  // not FreeAndNil(FUnit); as FUnit belongs to the model
  inherited;
end;

function TJavaParser.GetNextToken: string;
begin
  Result := FScanner.GetNextToken;
end;

function TJavaParser.Token: string;
begin
  Result := FScanner.Token;
end;

procedure TJavaParser.SkipPair(const Open, Close: string);
begin
  FScanner.SkipPair(Open, Close);
end;

procedure TJavaParser.ParseStream(AStream: TStream; AModel: TAbstractPackage;
  AOM: TObjectModel; FileName: string; Inner: Boolean;
  WithoutNeedSource: Boolean);
var AClassifier: TClassifier; AClass: TClass;
begin
  FScanner.Init(AStream);
  FModel := AModel;
  FOM := AOM;
  FFilename := FileName;
  FSourcepath := ExtractFilePath(FileName);
  FInner := Inner;
  FWithoutNeedSource := WithoutNeedSource;
  FUnit := TLogicPackage(FModel).FindUnitPackage('Default');
  if not Assigned(FUnit) then
    FUnit := TLogicPackage(FModel).AddUnit('Default');
  AClassifier := FUnit.FindClass(FileName);
  if Assigned(AClassifier) and AClassifier.SourceRead then
  begin
    if AClassifier is TClass then
    begin
      AClass := TClass(AClassifier);
      AClass.IsVisible := True;
      FUnit.AddClass(AClass);
    end;
    Exit;
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
  FModVisibility := viPackage;
  FModAbstract := False;
  FModStatic := False;
  FModFinal := False;
  while True do
  begin
    if Token = 'public' then
      FModVisibility := viPublic
    else if Token = 'protected' then
      FModVisibility := viProtected
    else if Token = 'private' then
      FModVisibility := viPrivate
    else if Token = 'static' then
      FModStatic := True
    else if Token = 'abstract' then
      FModAbstract := True
    else if Token = 'final' then
      FModFinal := True
    else if Token = '<' then
    begin
      SkipPair('<', '>');
      Break;
    end
    else if (Token = 'non') and (GetNextToken = '-') and
      (GetNextToken = 'sealed') then
      FScanner.Token := 'non-sealed'
    else if (Token = 'native') or (Token = 'transient') or (Token = 'default')
      or (Token = 'volatile') or (Token = 'strictfp') or (Token = 'sealed') or
      (Copy(Token, 1, 1) = '@') then
    else if Token = 'synchronized' then
    begin
      if FScanner.LookAheadToken = '(' then
      begin
        FScanner.Token := 'synchronized';
        Break;
      end;
    end
    else
      Break;
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
  if Token = 'package' then
  begin
    FPackagename := GetNextToken;
    CollectDirClassesForImport(FPackagename, FSourcepath, FPackagename,
      FUserImportClasses);
    if not FInner then
      FUnit.ImportStartline := FScanner.Line + 1;
    FScanner.SkipToken(';');
  end
  else
  begin
    FPackagename := '';
    if not FInner then
      FUnit.ImportStartline := 1;
  end;
  if not FInner then
    FUnit.ImportEndline := -1;

  if (Token = 'import') and not FInner then
    FUnit.ImportStartline := FScanner.Line;
  while (Token = 'import') and not ThreadAbort do
  begin
    (*
      ImportDeclaration
      import Identifier {   .   Identifier } [   .     *   ] ;
    *)
    var
    Str := GetNextToken;
    if Str = 'static' then
      Str := GetNextToken;

    if not FInner then
      FUnit.ImportEndline := FScanner.Line;
    if GetNextToken = '*' then
    begin
      FFullImports.Add(Str);
      Str := Copy(Str, 1, Length(Str) - 1);
      if FConfiguration.FixImports then
        ReadFullImport(Str)
      else if not FConfiguration.IsAPIPackage(Str) then
        CollectDirClassesForImport(Str, FSourcepath, FPackagename,
          FUserImportClasses);
      GetNextToken;
    end
    else
      FClassImports.Values[ExtractClassName(Str)] := ExtractPackageName(Str);
    GetNextToken;
  end;

  FFullImports.Add('java.lang.');
  if not FInner then
  begin
    AddStrings(FUnit.FullImports, FFullImports);
    AddStrings(FUnit.ClassImports, FClassImports);
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
  FStructures.Clear;
  FStructures.MaxDepth := 0;
  while Token <> '' do
  begin
    var
    Line := FScanner.TokenLine;
    ParseAnnotations;
    ParseModifiers;
    if (Token = 'class') or (Token = 'record') then
      ParseClassDeclaration(Line)
    else if Token = 'enum' then
      ParseEnumDeclaration
    else if (Token = 'interface') or (Token = '@interface') then
      ParseInterfaceDeclaration(Line)
    else // if token = 'void' Method declaration From the structogram area
      GetNextToken;
  end;
  while FFilo.Count > 0 do
  begin
    var
    Structure := TStructureEx(FFilo.Last);
    FFilo.Delete(FFilo.Count - 1);
    Structure.FTextRect.BottomRight := Point(0, FScanner.Line);
    FStructures.Add(Structure);
  end;
end;

procedure TJavaParser.ParseClassDeclaration(Line: Integer;
  IsInner: Boolean = False; const Parentname: string = '');
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
var AClass: TClass; Intf: TInterface; AClassifier: TClassifier;
  Impl, Ext, AClassname, AGeneric: string; Anonym, IsRecord: Boolean;

begin
  // enum is not supported
  Inc(FCountClasses);
  Inc(FScopeDepth);
  IsRecord := (Token = 'record');
  if Token = '{' then
  begin // Anonym Inner class
    Anonym := True;
    Inc(FAnonymClassNr);
    AClassname := Parentname + '$' + IntToStr(FAnonymClassNr);
  end
  else
  begin
    Anonym := False;
    GetNextToken;
    if IsInner then
      AClassname := Parentname + '$' + Token
    else
      AClassname := Token;
    GetNextToken;
    // [TypeParameters]
    if Token = '<' then
    begin
      AGeneric := FScanner.GetGeneric;
      AClassname := AClassname + '<' + AGeneric + '>';
    end;
  end;

  if (FPackagename <> '') and (Pos(FPackagename, AClassname) <> 1) then
    AClassname := FPackagename + '.' + AClassname;
  AClass := FUnit.MakeClass(AClassname, FFilename);
  AClass.Documentation.Description := ReplaceStr(FScanner.Comment,
    #13#10, '<br>');
  FScanner.Comment := '';
  AClass.Importname := AClassname;
  AClass.Generic := AGeneric;
  AClass.Inner := IsInner;
  AClass.Anonym := Anonym;
  AClass.IsVisible := ShowView(IsInner) and not Anonym;
  AClass.LineS := Line;
  AClass.LineSE := FScanner.Line;
  AClass.ScopeDepth := FScopeDepth;
  if AClass.IsVisible then // what is with AClass if AClass ist not visible?
    FUnit.AddClass(AClass);
  SetVisibility(AClass);

  // Superclass:
  // extends ClassType
  if (Token = 'extends') and not IsRecord and not ThreadAbort then
  begin
    Ext := GetNextToken;
    if EndsWith(Ext, 'Application') then
      FFrameType := 8
    else if EndsWith(Ext, 'JApplet') then
      FFrameType := 7
    else if EndsWith(Ext, 'JDialog') then
      FFrameType := 6
    else if EndsWith(Ext, 'JFrame') then
      FFrameType := 5
    else if EndsWith(Ext, 'Applet') then
      FFrameType := 4
    else if EndsWith(Ext, 'Dialog') then
      FFrameType := 3
    else if EndsWith(Ext, 'Frame') then
      FFrameType := 2
    else
      FFrameType := 1;
    Ext := GetImportName(Ext);
    GetNextToken;
    if Token = '<' then
      Ext := FScanner.GetGeneric(Ext);
    if AClass.Importname <> Ext then
    begin
      AClassifier := NeedClassifier(Ext, TClass);
      if Assigned(AClassifier) and (AClassifier is TClass) then
      begin
        AClass.Ancestor := TClass(AClassifier);
        // we need superclass for e.g. code completion
        if Assigned(AClass.Ancestor) then
          AClass.Ancestor.Importname := GetImportName(AClass.Ancestor.Name);
      end;
    end;
  end;

  if IsRecord then
  begin
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
        Impl := ChangeGenericType(FScanner.GetGeneric(Impl));

      AClassifier := NeedClassifier(Impl, TInterface);
      if Assigned(AClassifier) and (AClassifier is TInterface) then
      begin
        Intf := TInterface(AClassifier);
        AClass.AddImplements(Intf);
        if ShowView(True) then
          AClass.ViewImplements(Intf);
      end;
    until (Token <> ',') or ThreadAbort;

  // PermittedSubclasses:
  if Token = 'permits' then
  begin
    GetNextToken;
    while GetNextToken = ',' do
      GetNextToken;
  end;

  if (Token = '{') and not ThreadAbort then
  begin
    AddStructure(Line, FScanner.Line);
    ParseClassBody(AClass);
    AClass.LineE := FScanner.Line;
    CloseStructureDefault;
    GetNextToken;
  end;
  Dec(FScopeDepth);
  AClass.SourceRead := True;
end;

procedure TJavaParser.ParseClassBody(AClass: TClass);
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
var Operation, OpTemp: TOperation; Attribute: TAttribute;
  Typename, Ident, Generic, Annotation, Modifiers: string; LineS, Line: Integer;
  TypeClass: TClassifier;
begin
  GetNextToken;
  while True and not ThreadAbort do
  begin
    Line := FScanner.TokenLine;
    Annotation := ParseAnnotations(AClass);
    Modifiers := ParseModifiers;
    // StaticInitializer:
    // static Block
    if (Modifiers = ' static') and (Token = '{') then
    begin
      Operation := AClass.AddOperationWithoutType('static initializer');
      Operation.LineS := Line;
      AddStructure(Line, FScanner.Line);
      ParseBlock(Operation);
      CloseStructureDefault;
      GetNextToken;
    end
    // InstanceInitializer:
    // Block
    else if (Modifiers = '') and (Token = '{') then
    begin
      Operation := AClass.AddOperationWithoutType('instance initializer');
      Operation.LineS := Line;
      AddStructure(Line, FScanner.Line);
      ParseBlock(Operation);
      CloseStructureDefault;
      GetNextToken;
    end
    else if Token = ';' then
      GetNextToken
    else if (Token = '}') or (Token = '') then
      Break
      // ClassDeclaration:
    else if (Token = 'class') or (Token = 'record') then // Inner class
      ParseClassDeclaration(Line, True, WithoutGeneric(AClass.Name))
    else if Token = 'enum' then
      ParseEnumDeclaration
      // InterfaceDeclaration:
    else if Token = 'interface' then
      ParseInterfaceDeclaration(Line, True, AClass.Name)
      // Field-, Method- or Constructor-Declaration
      // FieldDeclaration:
      // {FieldModifier} UnannType VariableDeclaratorList ;
      // MethodDeclaration:
      // {MethodModifier} MethodHeader MethodBody
      // ConstructorDeclaration:
      // {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
    else
    begin
      // must be typename or constructor
      LineS := Line;
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

      Typename := GetTypeName;
      if (Typename = GetClassName(AClass.Name)) and (Token = '(') then
      begin
        Ident := Typename; // constructor
        Typename := '';
      end
      else
      begin
        Ident := Token;
        GetNextToken;
        while Token = '[]' do
        begin // for FieldDeclaration
          Ident := Ident + Token;
          GetNextToken;
        end;
      end;

      if Token = '(' then
      begin // new Method/Constructor, start of parameters
        TypeClass := nil;
        if (Typename <> '') and (Typename <> 'void') then
          TypeClass := NeedClassifier(GetImportName(Typename));
        OpTemp := AClass.MakeOperation(Ident, TypeClass);
        OpTemp.LineS := LineS;
        OpTemp.Annotation := Annotation;
        DoOperation(OpTemp, AClass.Name, Typename);
        Operation := AClass.FindOperation(OpTemp);
        if not Assigned(Operation) then
        begin
          Operation := OpTemp;
          AClass.AddOperation(Operation);
        end
        else
          FreeAndNil(OpTemp);

        GetNextToken; // ')'
        while Token = '[]' do
          GetNextToken;

        if Token = 'throws' then
        begin
          GetNextToken;
          GetImportName(Token);
          GetNextToken;
          while Token = ',' do
          begin
            GetNextToken;
            GetImportName(Token);
            GetNextToken;
          end;
        end;

        while (Token <> ';') and (Token <> '{') and (Token <> '') do
          GetNextToken;
        AddStructure(Line, FScanner.TokenLine);
        // either ; for abstract method or { for body
        if not ThreadAbort and (Token = '{') then
        begin
          ParseBlock(Operation);
          CloseStructureDefault;
          GetNextToken;
        end
        else
          CloseStructureDefault;
      end
      else // new attribute
        // FieldDeclaration:
        // {FieldModifier} UnannType VariableDeclaratorList ;
        if not ThreadAbort and IsIdentifier(Ident) and IsTypename(Typename) and
          (Ident <> Typename) then
        begin
          SwapArrFromTo(Ident, Typename);
          TypeClass := NeedClassifier(GetImportName(Typename));
          Attribute := AClass.AddAttribute(Ident, TypeClass);
          Attribute.LineS := LineS;
          DoAttribute(Attribute, Generic);
          while not ThreadAbort and (Token = ',') do
          begin
            GetNextToken;
            Ident := Token;
            GetNextToken;
            while Token = '[]' do
            begin
              Ident := Ident + Token;
              GetNextToken;
            end;
            DoAttribute(AClass.AddAttribute(Ident, TypeClass), Generic);
          end;
          // attribute-declaration ends with ;
          // erroneous attribute declaration is catched here
          while (Token <> '') and (Token <> ';') do
            GetNextToken;
        end;
      FScanner.Comment := '';
    end;
  end;
end;

procedure TJavaParser.SkipTo(Chr: Char);
begin
  FScanner.SkipTo(Chr);
end;

procedure TJavaParser.Skip(Chr: Char);
begin
  FScanner.SkipTo(Chr);
  GetNextToken;
end;

procedure TJavaParser.ParseInterfaceDeclaration(Line: Integer;
  IsInner: Boolean = False; const Parentname: string = '');
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
  ConstantDeclaratorsRest
  InterfaceMethodDeclaratorRest

  InterfaceMethodDeclaratorRest:
  FormalParameters BracketsOpt [throws QualifiedIdentifierList]

  VoidInterfaceMethodDeclaratorRest:
  FormalParameters [throws QualifiedIdentifierList]
*)
var Intf, AInt: TInterface; AClassifier: TClassifier;
  Typename, Ident, Ext, Str, AGeneric: string; Operation, OpTemp: TOperation;
  Attribute: TAttribute; LineS: Integer; FirstAncestor: Boolean;
  TypeClass: TClassifier;
begin
  Inc(FCountClasses);
  Inc(FScopeDepth);
  GetNextToken;
  if IsInner then
    Str := Parentname + '$' + Token
  else if FPackagename = '' then
    Str := Token
  else
    Str := FPackagename + '.' + Token;
  GetNextToken;

  if Token = '<' then
  begin
    AGeneric := FScanner.GetGeneric;
    Str := Str + '<' + AGeneric + '>';
  end;
  Intf := FUnit.MakeInterface(Str, FFilename);
  Intf.Documentation.Description := ReplaceStr(FScanner.Comment,
    #13#10, '<br>');
  FScanner.Comment := '';
  Intf.Generic := AGeneric;
  Intf.LineS := Line;
  Intf.LineSE := FScanner.Line;
  Intf.ScopeDepth := FScopeDepth;
  SetVisibility(Intf);
  Intf.IsVisible := ShowView(IsInner);
  if Intf.IsVisible then
    FUnit.AddInterface(Intf);

  FirstAncestor := True;
  if Token = 'extends' then
    repeat
      Ext := GetImportName(GetNextToken);
      // if FPackagename <> '' then Ext:= FPackagename + '.' + Ext;
      GetNextToken;
      if Token = '<' then
        Ext := ChangeGenericType(FScanner.GetGeneric(Ext));
      AClassifier := NeedClassifier(Ext, TInterface);
      if Assigned(AClassifier) and (AClassifier is TInterface) then
      begin
        AInt := TInterface(AClassifier); // toDo
        if FirstAncestor then
        begin
          Intf.Ancestor := AInt;
          Intf.Ancestor.Importname := GetImportName(Intf.Ancestor.Name);
          FirstAncestor := False;
        end;
        if Assigned(Intf) then
        begin
          Intf.AddExtends(AInt);
          if ShowView(True) then
            Intf.ViewExtends(AInt);
        end;
      end;
    until Token <> ',';
  Intf.LineSE := FScanner.Line;
  if Token = '{' then
  begin
    AddStructure(Line, FScanner.Line);
    GetNextToken;
    while True do
    begin
      Line := FScanner.Line;
      ParseAnnotations;
      ParseModifiers;
      if Token = ';' then // empty
        GetNextToken
      else if Token = 'class' then // Inner class
        ParseClassDeclaration(Line, True, Intf.Name)
      else if Token = 'enum' then
        ParseEnumDeclaration
      else if Token = 'interface' then // Inner interface
        ParseInterfaceDeclaration(Line, True, Intf.Name)
      else if (Token = '}') or (Token = '') then
      begin
        // end of interface declaration
        Intf.LineE := FScanner.Line;
        CloseStructureDefault;
        GetNextToken;
        Break;
      end
      else
      begin
        // Must be type of attr or return type of operation
        LineS := Line;
        Typename := GetTypeName;
        Ident := Token;
        if GetNextToken = '(' then
        begin
          // Operation
          if (Typename <> '') and (Typename <> 'void') then
            TypeClass := NeedClassifier(GetImportName(Typename))
          else
            TypeClass := nil;

          OpTemp := Intf.MakeOperation(Ident, TypeClass);
          OpTemp.LineS := LineS;
          DoOperation(OpTemp, Intf.Name, Typename);
          Operation := Intf.FindOperation(OpTemp);
          if not Assigned(Operation) then
          begin
            Operation := OpTemp;
            Intf.AddOperation(Operation);
          end
          else
            FreeAndNil(OpTemp);

          GetNextToken;
          if Token = '{' then
          begin // default methods
            AddStructure(Line, FScanner.TokenLine);
            ParseBlock(Operation);
            CloseStructureDefault;
            GetNextToken;
          end
          else
          begin
            // Skip Throws if present
            while (Token <> ';') and (Token <> '') do
              GetNextToken;
            AddStructure(Line, FScanner.TokenLine);
            CloseStructureDefault;
          end;
        end
        else if IsIdentifier(Ident) and IsTypename(Typename) and
          (Ident <> Typename) then
        begin
          SwapArrFromTo(Ident, Typename);
          TypeClass := NeedClassifier(GetImportName(Typename));
          Attribute := Intf.AddAttribute(Ident, TypeClass);
          Attribute.LineS := LineS;
          DoAttribute(Attribute, AGeneric);
          while Token = ',' do
          begin
            GetNextToken;
            Ident := Token;
            GetNextToken;
            DoAttribute(Intf.AddAttribute(Ident, TypeClass), AGeneric);
          end;
          FScanner.Comment := '';
        end;
      end;
    end;
  end;
  Dec(FScopeDepth);
  Intf.SourceRead := True;
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

function TJavaParser.NeedClassifier(const CName: string;
  TheClass: TModelEntityClass = nil): TClassifier;
var PName, ShortName: string; ClassIte: IModelIterator; Cent: TModelEntity;

  function AddAClass(const CName: string): TClassifier;
  var AClass: TClass; Int: TInterface; Classpathname: string;
  begin
    Result := nil;
    if TheClass = TInterface then
    begin
      Int := FUnit.MakeInterface(CName, '');
      Int.Importname := CName;
      FUnit.AddInterfaceWithoutShowing(Int);
      Result := TClassifier(Int);
    end
    else
    begin
      AClass := FUnit.MakeClass(CName, '');
      if not Assigned(AClass) then
        Exit;
      AClass.Importname := CName;
      Classpathname := FSourcepath + WithoutArray(CName) + '.java';
      if FileExists(Classpathname) then
        AClass.Pathname := Classpathname;
      FUnit.AddClassWithoutShowing(AClass);
      Result := TClassifier(AClass);
    end;
  end;

begin
  Result := nil;
  if not Assigned(FUnit) then
    Exit;

  Result := FUnit.FindClassifier(CName, TheClass, True);
  if Assigned(Result) then
    Exit
  else if IsSimpleType(CName) or (CName = 'java.lang.String') then
  begin
    Result := AddAClass(CName);
    Exit;
  end;

  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);
  if PName = '' then
  begin
    ClassIte := FUnit.GetClassifiers;
    while ClassIte.HasNext do
    begin
      Cent := ClassIte.Next;
      if ExtractClassName(Cent.Name) = CName then
      begin
        Result := TClassifier(Cent);
        Exit;
      end;
    end;
  end;

  if not FWithoutNeedSource and NeedSource(ShortName, PName) then
  begin
    Result := FUnit.FindClassifier(ShortName, TheClass, True);
    if Assigned(Result) then
      Exit;
  end;
  Result := AddAClass(CName);
  if FWithoutNeedSource then
    Result.Pathname := '';
end;

procedure TJavaParser.SetVisibility(Model: TModelEntity);
begin
  Model.Visibility := FModVisibility;
  Model.Static := FModStatic;
  Model.IsFinal := FModFinal;
  Model.IsAbstract := FModAbstract;
end;

procedure TJavaParser.SwapArrFromTo(var Str1, Str2: string);
begin
  var
  Posi := Pos('[]', Str1);
  while Posi > 0 do
  begin
    Str2 := Str2 + '[]';
    Delete(Str1, Posi, 2);
    Posi := Pos('[]', Str1);
  end;
end;

procedure TJavaParser.DoOperation(Operation: TOperation;
  const Parentname, Typename: string);
var ParType, Ident: string; Param: TParameter;
begin
  Operation.Documentation.Description := FScanner.Comment;
  Operation.Documentation.LineS := FScanner.CommentLineS;
  Operation.Documentation.LineE := FScanner.CommentLineE;
  Operation.Spalte := FScanner.LastTokenColumn;
  Operation.HasComment := (Pos('/*', Operation.Documentation.Description) +
    Pos('//', Operation.Documentation.Description) = 1);
  Operation.Parentname := Parentname;
  Operation.ScopeDepth := FScopeDepth;
  SetVisibility(Operation);
  if (Operation.ReturnValue = nil) and (Typename <> '') and (Typename <> 'void')
  then
    Operation.ReturnValue := NeedClassifier(GetImportName(Typename));
  if Assigned(Operation.ReturnValue) then
    Operation.OperationType := otFunction
  else if (GetClassName(Parentname) = Operation.Name) and (Typename = '') then
    Operation.OperationType := otConstructor
  else
    Operation.OperationType := otProcedure;
  // Parameterlist
  GetNextToken;
  while (Token <> '') and (Token <> ')') do
  begin
    if Token = 'final' then
      GetNextToken;
    ParType := GetTypeName;
    Ident := Token;
    SwapArrFromTo(Ident, ParType);
    // if ParType = 'E' then p:= 'java.lang.Object' else p:= ParType;
    Param := Operation.AddParameter(Ident);
    Param.LineS := FScanner.Line;
    Param.Spalte := FScanner.TokenColumn;
    Param.TypeClassifier := NeedClassifier(GetImportName(ParType));
    GetNextToken;
    if Token = ',' then
      GetNextToken;
  end;
  FScanner.Comment := '';
  FScanner.CommentLineS := 0;
  FScanner.CommentLineE := 0;
  Operation.LineE := FScanner.Line;
end;

function TJavaParser.ParseAnnotations(AClass: TClass = nil): string;
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
      and Assigned(AClass) then
    begin
      AClass.IsJUnitTestClass := True;
      if (Token = 'Test') or (Token = 'ParameterizedTest') then
        Result := Token;
    end;
    GetNextToken;
    if Token = '(' then
      SkipPair('(', ')');
  end;
end;

procedure TJavaParser.ParseBlock(Operation: TOperation);
begin
  Inc(FScopeDepth);
  GetNextToken;
  repeat
    ParseBlockStatement(Operation, True);
  until (Token = '}') or (Token = '') or IsMethodModifier(Token);
  Operation.SetAttributeScope(FScopeDepth, FScanner.Line);
  Operation.LineE := FScanner.Line;
  Dec(FScopeDepth);
end;

procedure TJavaParser.ParseIfStatement(Operation: TOperation);
// IfThenStatement: if ( Expression ) Statement
// IfThenElseStatement: if ( Expression ) StatementNoShortIf else Statement
begin
  var
  Line := FScanner.Line;
  GetNextToken;
  if Token = '(' then
    FScanner.SkipCondition
  else if Token = ')' then
    GetNextToken;

  var
  SingleStatement := (Token <> '{');
  if SingleStatement then
    AddStructure(Line, FScanner.LastTokenLine, SingleStatement)
  else
    AddStructure(Line, FScanner.Line, SingleStatement);
  ParseBlockStatement(Operation, False);
  if not SingleStatement then
    GetNextToken;
  if (Token = ';') and (FScanner.LookAheadToken = 'else') then
    GetNextToken;
  if Token = 'else' then
  begin
    Line := FScanner.Line;
    if SingleStatement then
      CloseStructure(Line, False)
    else
      CloseStructure(FScanner.LastTokenLine, False);
    GetNextToken;
    // else if gleiche Ebene wie einfaches else
    // andere Variante in Versionen 18.19 bis 20.05
    if Token = 'if' then
      ParseElseIfStatement(Operation)
    else
    begin
      SingleStatement := (Token <> '{');
      if SingleStatement then
        AddStructure(Line, FScanner.LastTokenLine, SingleStatement)
      else
        AddStructure(Line, FScanner.Line, SingleStatement);
      ParseBlockStatement(Operation, False);
      if not SingleStatement then
        GetNextToken;
      CloseStructure(FScanner.LastTokenLine);
    end;
  end
  else
    CloseStructure(FScanner.LastTokenLine, SingleStatement);
end;

procedure TJavaParser.ParseElseIfStatement(Operation: TOperation);
begin
  var
  Line := FScanner.Line;
  GetNextToken;
  if Token = '(' then
    FScanner.SkipCondition;
  var
  SingleStatement := (Token <> '{');
  if SingleStatement then
    AddStructure(Line, FScanner.LastTokenLine)
  else
    AddStructure(Line, FScanner.Line);
  ParseBlockStatement(Operation, False);
  if not SingleStatement then
    GetNextToken;
  if (Token = ';') and (FScanner.LookAheadToken = 'else') then
    GetNextToken;

  if Token = 'else' then
  begin
    if SingleStatement then
      Line := FScanner.Line
    else
      Line := FScanner.LastTokenLine;
    CloseStructure(Line, False);
    GetNextToken;
    if Token = 'if' then
      ParseElseIfStatement(Operation)
    else
    begin
      SingleStatement := (Token <> '{');
      if SingleStatement then
        AddStructure(Line, FScanner.LastTokenLine)
      else
        AddStructure(Line, FScanner.Line);
      ParseBlockStatement(Operation, False);
      if not SingleStatement then
        GetNextToken;
      CloseStructure(FScanner.LastTokenLine, SingleStatement);
    end;
  end
  else if SingleStatement then
    CloseStructure(FScanner.Line - 1, True)
  else
    CloseStructure(FScanner.LastTokenLine, False);
end;

procedure TJavaParser.ParseTryStatement(Operation: TOperation);
// TryStatement: try Block Catches | try Block Catchesopt Finally
// Catches     : CatchClause | Catches CatchClause
// CatchClause : catch ( FormalParameter ) Block
// Finally     : finally Block
var Typename, Ident: string; Line: Integer;
begin
  Line := FScanner.Line;
  GetNextToken;
  if Token = '(' then
  begin // try with ressources
    GetNextToken;
    Typename := Token;
    GetImportName(Typename);
    FScanner.SkipCondition;
  end;
  AddStructure(Line, FScanner.Line);
  ParseBlock(Operation);
  CloseStructureDefault;
  GetNextToken;
  while Token = 'catch' do
  begin
    Line := FScanner.Line;
    GetNextToken;
    if Token = '(' then
    begin
      Typename := GetImportName(GetNextToken);
      Ident := GetNextToken;
      if Assigned(FUnit) then // not for ExecutionParser
        DoAttribute(Operation.AddAttribute(Ident, NeedClassifier(Typename)), '',
          Operation);
      FScanner.SkipCondition;
    end;
    AddStructure(Line, FScanner.Line);
    ParseBlock(Operation);
    CloseStructureDefault;
    GetNextToken;
  end;
  if Token = 'finally' then
  begin
    Line := FScanner.LastTokenLine;
    GetNextToken;
    AddStructure(Line, FScanner.Line);
    ParseBlock(Operation);
    CloseStructureDefault;
    GetNextToken;
  end;
end;

procedure TJavaParser.ParseSwitchStatement(Operation: TOperation);
(*
  SwitchStatement: switch ( Expression ) SwitchBlock
  SwitchBlock: { SwitchBlockStatementGroupsopt SwitchLabelsopt }
  SwitchBlockStatementGroup: SwitchLabels BlockStatements
  BlockStatements: BlockStatement {Blockstatement}
  SwitchLabel: case ConstantExpreson | case EnumConstantName : | default:
*)

var Line: Integer; LToken: string;

  procedure TraditionelSwitch;
  begin
    while (Token = 'case') or (Token = 'default') do
    begin
      Line := FScanner.Line;
      GetNextToken;
      SkipTo(':');
      AddStructure(Line, FScanner.Line);
      GetNextToken;
      while (Token = 'case') or (Token = 'default') do
      begin
        CloseStructureDefault;
        Line := FScanner.Line;
        GetNextToken;
        SkipTo(':');
        AddStructure(Line, FScanner.Line);
        GetNextToken;
      end;
      if Token <> '}' then
        repeat
          ParseBlockStatement(Operation, True);
          if Token = ';' then
            GetNextToken;
        until (Token = '}') or (Token = '') or (Token = 'case') or
          (Token = 'default');
      CloseStructure(FScanner.LastTokenLine, True);
    end;
  end;

  procedure NewSwitch;
  begin
    while (Token = 'case') or (Token = 'default') do
    begin
      Line := FScanner.Line;
      AddStructure(Line, FScanner.Line);
      while (Token <> '->') and not FScanner.Empty do
        GetNextToken;
      GetNextToken;
      var
      SingleStatement := (Token <> '{');
      ParseBlockStatement(Operation, False);
      if SingleStatement and (Token = ';') or not SingleStatement and
        (Token = '}') then
        GetNextToken;
      CloseStructure(FScanner.LastTokenLine, SingleStatement);
    end;
  end;

begin
  Line := FScanner.Line;
  GetNextToken;
  if Token = '(' then
    FScanner.SkipCondition;
  AddStructure(Line, FScanner.Line);
  if Token = '{' then
  begin
    GetNextToken;
    if (Token = 'case') or (Token = 'default') then
    begin
      Line := FScanner.Line;
      LToken := FScanner.LookAheadToken(':->');
      if LToken = ':' then
        TraditionelSwitch
      else
        NewSwitch;
    end;
  end;
  CloseStructureDefault;
  GetNextToken;
end;

procedure TJavaParser.ParseWhileStatement(Operation: TOperation);
// WhileStatement: while ( Expression ) Statement
begin
  var
  Line := FScanner.Line;
  GetNextToken;
  if Token = '(' then
    FScanner.SkipCondition
  else if Token = ')' then
    GetNextToken;
  var
  SingleStatement := (Token <> '{');
  if SingleStatement then
    AddStructure(Line, FScanner.LastTokenLine)
  else
    AddStructure(Line, FScanner.Line);
  ParseBlockStatement(Operation, False);
  if not SingleStatement then
    GetNextToken;
  if SingleStatement then
    CloseStructure(FScanner.LastTokenLine, SingleStatement) // FScanner.Line?
  else
    CloseStructure(FScanner.LastTokenLine, SingleStatement);
end;

procedure TJavaParser.ParseDoStatement(Operation: TOperation);
// DoStatement: do Statement while ( Expression ) ;
begin
  var
  Line := FScanner.Line;
  GetNextToken;
  var
  SingleStatement := (Token <> '{');
  if SingleStatement then
    AddStructure(Line, FScanner.LastTokenLine)
  else
    AddStructure(Line, FScanner.Line);
  ParseBlockStatement(Operation, False);
  GetNextToken;
  if SingleStatement then
    Line := FScanner.Line
  else
    Line := FScanner.LastTokenLine;
  if Token = 'while' then
  begin
    GetNextToken;
    if Token = '(' then
      FScanner.SkipCondition;
  end;
  CloseStructure(FScanner.Line, False);
  FStructures[FStructures.Count - 1].FBottomTop := Line;
  if Token = ';' then
    GetNextToken;
end;

procedure TJavaParser.ParseForStatement(Operation: TOperation);
var Typename, Ident, Importname: string; SingleStatement: Boolean;
  Line: Integer; Attr: TAttribute;
  // ForStatement: for (type variable = ; Expressionopt ; ForUpdateopt ) Statement
  // ForStatement: for (Type Object : Collection) Statement
begin
  Line := FScanner.Line;
  GetNextToken;
  Inc(FScopeDepth);
  if Token = '(' then
  begin
    Typename := GetNextToken;
    if Token = '<' then
      SkipPair('<', '>');
    if Typename <> ';' then
    begin
      Ident := GetNextToken;
      if Ident = '(' then
        // for (match (Token.Colon); ; match (Token.Comma)) {..}
        SkipPair('(', ')')
      else
      begin
        GetNextToken;
        if ((Token = ':') or (Token = '=')) and Assigned(FUnit) then
        begin
          FModVisibility := viPackage;
          Importname := GetImportName(Typename);
          Attr := Operation.AddAttribute(Ident, NeedClassifier(Importname));
          DoAttribute(Attr, '', Operation);
          Attr.LineS := FScanner.LastTokenLine;
        end;
      end;
    end;
    if Token <> '{' then
      FScanner.SkipPairStopAt('(', ')', '{}');
  end;
  SingleStatement := (Token <> '{');
  if SingleStatement then
    AddStructure(Line, FScanner.LastTokenLine)
  else
    AddStructure(Line, FScanner.Line);
  ParseBlockStatement(Operation, False);
  if not SingleStatement then
    GetNextToken;
  CloseStructure(FScanner.LastTokenLine, SingleStatement);
  Operation.SetAttributeScope(FScopeDepth, FScanner.Line);
  Dec(FScopeDepth);
end;

procedure TJavaParser.ParseBreakStatement(Operation: TOperation);
// 14.14
// BreakStatement: break Identifier opt ;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseYieldStatement(Operation: TOperation);
// YieldStatement: yield Expression;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseContinueStatement(Operation: TOperation);
// ContinueStatement: continue Identifieropt ;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseAssertStatement(Operation: TOperation);
// AssertStatement: assert Expression1 ; | asser Expression1 : Expression2 ;
begin
  SkipTo(';');
  Skip(';');
end;

procedure TJavaParser.ParseReturnStatement(Operation: TOperation);
// ReturnStatement: return [Expression] ;
begin
  GetNextToken;
  if Token = 'new' then
    ParseNew(Operation)
  else if IsIdentifier(Token) then
  begin
    GetNextToken;
    if Token = '(' then
      ParseArgumentList(Operation);
  end
  else if Token = '(' then
  begin
    SkipPair('(', ')');
    if Token = '->' then
      ParseLambdaBody(Operation);
  end;
  Skip(';');
end;

procedure TJavaParser.ParseLambdaBody(Operation: TOperation);
begin
  var
  MyFree := False;
  if not Assigned(Operation) then
  begin
    Operation := TOperation.Create(nil);
    MyFree := True;
  end;
  GetNextToken;
  if Token = '{' then
  begin
    AddStructure(FScanner.Line, FScanner.Line);
    ParseBlock(Operation);
    CloseStructureDefault;
  end
  else
    ParseBlockStatement(Operation, False);
  if MyFree then
    FreeAndNil(Operation);
end;

procedure TJavaParser.ParseNew(Operation: TOperation);
var Typename: string; AClass: TClass;
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
  if Token = '<' then
    SkipPair('<', '>');
  Typename := GetTypeName;
  NeedClassifier(GetImportName(WithoutArray(Typename)));
  if Token = '<' then
    SkipPair('<', '>')
  else if Token = '<>' then
    GetNextToken;
  if Token = '(' then
  begin
    ParseArgumentList(Operation);
    if Token = '{' then
    begin
      AddStructure(FScanner.Line, FScanner.Line);
      AClass := TClass.Create(nil);
      try
        ParseClassBody(AClass);
      finally
        AClass.Free;
      end;
      CloseStructureDefault;
    end;
  end
  else if Token = '{' then // array declaration
    SkipPair('{', '}');
end;

procedure TJavaParser.ParseThrowStatement(Operation: TOperation);
// ThrowStatement: throw Expression ;
begin
  GetNextToken;
  Skip(';');
end;

procedure TJavaParser.ParseSynchronizedStatement(Operation: TOperation);
// SynchronizedStatement: synchronized ( Expression ) Block
begin
  GetNextToken;
  if Token = '(' then
    FScanner.SkipCondition;
  AddStructure(FScanner.Line, FScanner.Line);
  ParseBlock(Operation);
  CloseStructureDefault;
  GetNextToken;
end;

function TJavaParser.IsStatementBegin(const Token: string): Boolean;
begin
  Result := (Token = 'if') or (Token = 'do') or (Token = 'while') or
    (Token = 'for') or (Token = 'try') or (Token = 'switch') or
    (Token = 'synchronized') or (Token = 'return') or (Token = 'throw') or
    (Token = 'break') or (Token = 'continue') or (Token = 'assert');
end;

procedure TJavaParser.ParseBlockStatement(Operation: TOperation;
  NewStructure: Boolean);
// BlockStatement:
// LocalVariableDeclarationStatement
// ClassDeclaration (local class, declared in a method/constructor)
// Statement
begin
  if (Token = '}') or (Token = '') then
    Exit;
  ParseAnnotations;
  ParseLocalClassModifiers; // not private/protected/public/static

  // labled statement
  if FScanner.LookAheadToken = ':' then
  begin
    GetNextToken;
    GetNextToken;
  end;
  if Token = 'class' then
    ParseClassDeclaration(FScanner.Line, True, Operation.Parentname)
  else if Token = 'enum' then
    ParseEnumDeclaration
    // local, Inner class
  else if Token = 'if' then
    ParseIfStatement(Operation)
  else if Token = 'while' then
    ParseWhileStatement(Operation)
  else if Token = 'for' then
    ParseForStatement(Operation)
  else if Token = '{' then
  begin // Blockstatement
    if NewStructure then
      AddStructureDefault;
    ParseBlock(Operation);
    if NewStructure then
    begin
      CloseStructureDefault;
      GetNextToken;
    end;
  end
  else if Token = ';' then // EmptyStatement
    GetNextToken
  else if Token = 'assert' then
    ParseAssertStatement(Operation)
  else if Token = 'switch' then
    ParseSwitchStatement(Operation)
  else if Token = 'do' then
    ParseDoStatement(Operation)
  else if Token = 'break' then
    ParseBreakStatement(Operation)
  else if Token = 'yield' then
    ParseYieldStatement(Operation)
  else if Token = 'continue' then
    ParseContinueStatement(Operation)
  else if Token = 'return' then
    ParseReturnStatement(Operation)
  else if Token = 'synchronized' then
    ParseSynchronizedStatement(Operation)
  else if Token = 'throw' then
    ParseThrowStatement(Operation)
  else if Token = 'try' then
    ParseTryStatement(Operation)
  else
    ParseExpressionOrLocalVariableDeclarationStatement(Operation);
  Operation.HasSourceCode := True;
end;

procedure TJavaParser.ParseExpressionOrLocalVariableDeclarationStatement
  (Operation: TOperation);
begin
  // ExpressionStatement: StatementExpression
  // StatementExpression: Assignment | PreIncrementExpression | PreDecrementExpression
  // PostIncrementExpression | PostDecrementExpression
  // MethodInvocation | ClassInstanceCreationExpression

  // ClassInstanceCreationExpression: new ClassType ( ArgumentListopt )

  // Assignment: LeftHandSide AssignmentOperator AssignmentExpression

  if Token = 'new' then // ClassInstanceCreationExpression
    ParseNew(Operation)
  else if (Token = '++') or (Token = '--') then // PreIncDecrementExpression
    Skip(';')
  else
  begin
    var
    Typename := GetTypeName;
    if Typename = 'final' then
    begin
      Typename := GetTypeName;
      ParseLocalVariableDeclaration(Typename, Operation);
      SkipTo(';');
    end
    else if (Typename = '(') and (Token = '(') then
    begin // ((Typecast) object).method;
      GetNextToken;
      GetImportName(Token);
      Skip(';');
    end
    else
    begin
      while (Length(Token) > 0) and CharInSet(Token[1], ['.', '[']) do
      begin
        Typename := Typename + Token;
        GetNextToken;
      end;
      if (Token = '++') or (Token = '--') then // PostIncDecExpression
        Skip(';')
      else if Token = '(' then // MethodInvocation
        ParseMethodInvocation(Typename, Operation)
      else if IsAssignmentOperator(Token) then // Assignment
        ParseAssignment(Operation)
      else if (Token <> ';') and (Token <> '') and (Token <> '}') and
        (Token <> ')') and (Token <> '(') and not IsStatementBegin(Token) and
        not IsOperator(Token) then
      begin
        ParseLocalVariableDeclaration(Typename, Operation);
        // Variable Declaration
        SkipTo(';');
      end;
    end;
  end;
end;

procedure TJavaParser.ParseLocalVariableDeclaration(Typename: string;
  Operation: TOperation);
(*
  14.4
  LocalVariableDeclarationStatement: LocalVariableType VariableDeclaratorList
  VariableDeclaratorList: VariableDeclarator {, VariableDeclarator}
  VariableDeclarator:  VariableDeclaratorId [= VariableInitializer]
  VariableDeclaratorId: Identifier [Dims]
  VariableInitializer: Expression | ArrayInitializer
*)
var Ident, Generic: string; Attribute: TAttribute; Col: Integer;
  TypeClass: TClassifier;
begin
  Col := FScanner.LastTokenColumn;
  if Token = '<' then
  begin
    Generic := FScanner.GetGeneric;
    Typename := Typename + '<' + Generic + '>';
  end
  else
    Generic := '';
  Ident := Token;
  GetNextToken;
  while Token = '[]' do
  begin
    Ident := Ident + Token;
    GetNextToken;
  end;
  if IsIdentifier(Ident) and (IsSimpleType(Typename) or IsIdentifier(Typename))
    and (Length(Token) > 0) and CharInSet(Token[1], ['=', ',', ';']) then
  begin
    FModVisibility := viPackage;
    TypeClass := NeedClassifier(GetImportName(Typename));
    Attribute := Operation.AddAttribute(Ident, TypeClass);
    DoAttribute(Attribute, Generic, Operation);
    Attribute.Static := False;
    Attribute.Visibility := viPackage;
    Attribute.LineS := FScanner.TokenLine;
    Attribute.Spalte := Col;
    while Token = ',' do
    begin
      GetNextToken;
      Ident := Token;
      GetNextToken;
      Attribute := Operation.AddAttribute(Ident, TypeClass);
      DoAttribute(Attribute, Generic, Operation);
      Attribute.Static := False;
      Attribute.Visibility := viPackage;
      Attribute.LineS := FScanner.TokenLine;
      Attribute.Spalte := Col;
    end;
    FScanner.Comment := '';
  end;
end;

procedure TJavaParser.ParseAssignment(Operation: TOperation);
begin
  // this is part of ParseExpression
  GetNextToken;
  if Token = 'new' then
    ParseNew(Operation)
  else if IsIdentifier(Token) then
    while IsIdentifier(Token) do
    begin
      GetNextToken;
      if Token = '(' then
        ParseArgumentList(Operation);
      if Token = '.' then
        GetNextToken;
    end
  else if (Token = '(') then
  begin // lambda
    GetNextToken;
    if (Token = ')') and (FScanner.LookAheadToken = '->') then
    begin
      GetNextToken;
      ParseLambdaBody(Operation);
    end;
  end;
  SkipTo(';');
  GetNextToken;
end;

procedure TJavaParser.ParseMethodInvocation(const Typename: string;
  Operation: TOperation);
var Mi1, Mi2: IModelIterator; Attr: TModelEntity;
  AObject: string; IsClass: Boolean;
begin
  AObject := Copy(Typename, 1, Pos('.', Typename) - 1);
  if AObject <> '' then
  begin
    IsClass := True;
    Mi1 := Operation.GetAttributes;
    while Mi1.HasNext do
    begin
      Attr := Mi1.Next;
      if Attr.Name = AObject then
        IsClass := False;
    end;
    if (Operation.Owner is TClassifier) then
    begin
      Mi2 := TClassifier(Operation.Owner).GetAttributes;
      while Mi2.HasNext do
      begin
        Attr := Mi2.Next;
        if Attr.Name = WithoutArray(AObject) then
          IsClass := False;
      end;
      if IsClass then
      begin
        AObject := GetImportName(AObject);
        NeedClassifier(AObject);
      end;
    end;
  end;
  ParseArgumentList(Operation);
  while Token = '.' do
  begin
    GetNextToken;
    GetNextToken;
    if Token = '(' then
      ParseArgumentList(Operation);
  end;
end;

procedure TJavaParser.ParseArgumentList(Operation: TOperation);
var Bracket: Integer; AClassname: string; MyFree: Boolean;
begin
  MyFree := False;
  if not Assigned(Operation) then
  begin
    Operation := TOperation.Create(nil);
    MyFree := True;
  end;

  GetNextToken;
  Bracket := 0;
  while (Token <> ')') and (Token <> '') do
  begin
    if Token = '(' then
    begin // Typecast | method call | lambda
      GetNextToken;
      Inc(Bracket);
      if Token = '(' then
      begin
        GetNextToken;
        if IsTypename(Token) then
          GetImportName(Token);
        SkipTo(')');
        GetNextToken;
      end
      else if Token = ')' then
      begin // lambda
        GetNextToken;
        Dec(Bracket);
        if Token = '->' then
          ParseLambdaBody(Operation);
      end
      else
      begin
        SkipPair('(', ')');
        Dec(Bracket);
      end;
    end
    else if Token = 'new' then
      ParseNew(Operation)
    else if Token = 'switch' then
      ParseSwitchStatement(Operation)
    else if Token = '{' then
    begin
      AddStructure(FScanner.Line, FScanner.Line);
      ParseBlock(Operation);
      CloseStructureDefault;
    end
    else if (Pos('.', Token) > 0) and (Pos('"', Token) <> 1) then
    begin
      AClassname := Copy(Token, 1, Pos('.', Token) - 1);
      GetImportName(AClassname);
      GetNextToken;
    end
    else if Token = '::' then
    begin
      GetNextToken;
      GetNextToken;
    end
    else
    begin
      GetNextToken;
      while (Bracket > 0) and (Token = ')') do
      begin
        GetNextToken;
        Dec(Bracket);
      end;
    end;
  end;
  GetNextToken;
  if MyFree then
    FreeAndNil(Operation);
end;

function TJavaParser.IsAssignmentOperator(const Operator: string): Boolean;
begin
  Result := (Operator = '=') or (Operator = '*=') or (Operator = '/=') or
    (Operator = '%=') or (Operator = '+=') or (Operator = '-=') or
    (Operator = '<<=') or (Operator = '>>=') or (Operator = '>>>=') or
    (Operator = '&=') or (Operator = '^=') or (Operator = '|=');
end;

function TJavaParser.IsOperator(const Operator: string): Boolean;
begin
  Result := (Operator = '*') or (Operator = '/') or (Operator = '%') or
    (Operator = '+') or (Operator = '-') or (Operator = '<<') or
    (Operator = '>>') or (Operator = '>>>') or (Operator = '<') or
    (Operator = '>') or (Operator = '<=') or (Operator = '>M') or
    (Operator = 'instanceof') or (Operator = '==') or (Operator = '!=') or
    (Operator = '&') or (Operator = '^') or (Operator = '|') or
    (Operator = '&&') or (Operator = '||') or (Operator = '?') or
    (Operator = ':');
end;

function TJavaParser.IsExpressionStatementOperator(const Operator
  : string): Boolean;
begin
  Result := IsAssignmentOperator(Operator) or (Operator = '(') or
    (Operator = '++') or (Operator = '--');
end;

function TJavaParser.IsReservedWord(const RWord: string): Boolean;
var Left, Mid, Right: Integer;
begin
  Result := True;
  Left := 0;
  Right := High(ReservedWords);

  while Left <= Right do
  begin
    Mid := (Left + Right) div 2;
    if ReservedWords[Mid] = RWord then
      Exit;
    if ReservedWords[Mid] > RWord then
      Right := Mid - 1
    else
      Left := Mid + 1;
  end;
  Result := False;
end;

function TJavaParser.IsIdentifier(Ident: string): Boolean;
begin
  Result := False;
  var
  Int := Pos('<', Ident);
  if Int > 0 then
    Delete(Ident, Int, Length(Ident));
  if Length(Ident) = 0 then
    Exit;
  if not(Ident[1].IsLetter or (Pos(Ident[1], '_$') > 0)) then
    Exit;
  for var I := 2 to Length(Ident) do
    if not(Ident[I].IsLetterOrDigit or (Pos(Ident[I], '_.[]<>') > 0)) then
      Exit;
  if Ident[Length(Ident)] = '.' then
    Exit;
  Result := not IsReservedWord(Ident);
end;

function TJavaParser.IsTypename(const AType: string): Boolean;
var ClassIte, Ite: IModelIterator; Attr: TAttribute; Cent: TModelEntity;
begin
  Result := True;
  if IsSimpleType(AType) then
    Exit;
  Result := not IsReservedWord(AType);
  if Result and Assigned(FUnit) then
  begin
    ClassIte := FUnit.GetClassifiers;
    while ClassIte.HasNext do
    begin
      Cent := ClassIte.Next;
      Ite := TClassifier(Cent).GetAttributes;
      while Ite.HasNext do
      begin
        Attr := TAttribute(Ite.Next);
        if Attr.Name = AType then
          Result := False;
      end;
    end;
  end;
end;

procedure TJavaParser.DoAttribute(Attribute: TAttribute; const AGeneric: string;
  Operation: TOperation = nil);

var Pos: PChar; Posi: Integer; AToken, Str: string;

  function GetValue: string;
  begin
    var
    Str := '';
    while Pos < FScanner.CurrPos - 1 do
    begin
      Str := Str + Pos^;
      Pos := Pos + 1;
    end;
    Result := Str;
  end;

begin
  if Assigned(Attribute) then
  begin
    Attribute.Spalte := FScanner.LastTokenColumn;
    Attribute.ScopeDepth := FScopeDepth;
    // VariableDeclarator:
    // VariableDeclaratorId [= VariableInitializer]
    if Token = '=' then
    begin
      Pos := FScanner.CurrPos;
      (*
        VariableInitializer:
        Expression      - new is part of Expression - parseExpression!
        ArrayInitializer

        ClassInstanceCreationExpression:
        UnqualifiedClassInstanceCreationExpression
        ExpressionName . UnqualifiedClassInstanceCreationExpression
        Primary . UnqualifiedClassInstanceCreationExpression
      *)
      AToken := GetNextToken;
      if (AToken = 'new') or EndsWith(AToken, '.new') then
        ParseNew(Operation)
      else if (AToken = 'switch') then
        ParseSwitchStatement(Operation)
      else if IsIdentifier(Token) then
      begin
        while IsIdentifier(Token) do
        begin
          GetNextToken;
          if Token = '(' then
            ParseArgumentList(nil);
          if Token = '.' then
            GetNextToken;
        end;
        if Token = '->' then
          ParseLambdaBody(nil);
      end
      else if (Token = '(') then
      begin // lambda
        SkipPair('(', ')');
        if Token = '->' then
          ParseLambdaBody(nil);
      end
      else
      begin
        AToken := Token;
        Posi := System.Pos('.', AToken);
        if Posi > 0 then
        begin
          Delete(AToken, Posi, Length(AToken));
          GetImportName(AToken);
        end;
      end;

      // skip unknown part until , (next VariableDeclarator) or ; (end of VariableDeclaratorList)
      while (Token <> ',') and (Token <> ';') and (Token <> '') and
        (Token <> '}') do
      begin
        if Token = '{' then
          SkipPair('{', '}')
        else if Token = '(' then
          ParseArgumentList(nil) // Parameterlist
        else
          GetNextToken;
      end;
      Str := Trim(GetValue);
      Posi := System.Pos(#13#10, Str);
      if Posi > 0 then
        Str := Copy(Str, 1, Posi - 1);
      Attribute.Value := Str;
    end;

    Attribute.LineE := FScanner.Line;
    if Assigned(Attribute.TypeClassifier) then
      Attribute.TypeClassifier.Generic := AGeneric;
    if Assigned(Attribute.Documentation) then
      Attribute.Documentation.Description := FScanner.Comment;
    SetVisibility(Attribute);
  end;
end;

function TJavaParser.GetImportName(const Typ: string): string;
var Posi: Integer; StringList: TStringList;
  WithoutArray, WithoutGeneric, Gen, InnerGen, Arr, Complete: string;
begin
  Result := Typ;
  if ThreadAbort or IsSimpleType(Typ) or IsSimpleType(UUtils.WithoutArray(Typ))
    or ((Pos('.', Typ) > 0) and (Pos('...', Typ) = 0)) or (Typ = '') then
    Exit;

  Posi := Pos('<', Typ);
  if Posi > 0 then
  begin
    Gen := Copy(Typ, Posi, Length(Typ));
    WithoutGeneric := Copy(Typ, 1, Posi - 1);
  end
  else
  begin
    Gen := '';
    WithoutGeneric := Typ;
  end;
  WithoutArray := WithoutGeneric;
  Arr := '';
  SwapArrFromTo(WithoutArray, Arr);

  if WithoutArray = 'String' then
  begin
    Result := 'java.lang.' + Typ;
    Exit;
  end;

  if Gen <> '' then
  begin
    InnerGen := Copy(Gen, 2, Length(Gen) - 2);
    StringList := Split(',', InnerGen);
    for var I := 0 to StringList.Count - 1 do
      GetImportName(StringList[I]);
    FreeAndNil(StringList);
  end;

  Complete := FConfiguration.GetCompleteClassname(FFullImports, FClassImports,
    FUserImportClasses, WithoutArray);
  if Complete <> '' then
  begin
    Result := Complete + Arr + Gen;
    FConfiguration.ImportCache.Add(Typ + '=' + Result);
    Exit;
  end;

  if FConfiguration.FixImports then
  begin
    // search in classpath
    Result := FConfiguration.SearchClassInClasspath(WithoutArray, FSourcepath,
      FPackagename);
    if Result <> '' then
    begin
      if Pos(FConfiguration.JavaCache + '\', Result) = 1 then
        Delete(Result, 1, Length(FConfiguration.JavaCache) + 1);
      if ExtractFilePath(Result) = FSourcepath then
        Exit;
      Result := ReplaceStr(ChangeFileExt(Result, ''), '\', '.');
      FConfiguration.ImportCache.Add(WithoutArray + '=' + Result);
      Exit;
    end;

    // search in AllClasses
    Posi := FConfiguration.AllClasses.IndexOfName(WithoutArray);
    if (Posi > -1) and (Posi < FConfiguration.AllClasses.Count) then
    begin
      Result := FConfiguration.AllClasses.ValueFromIndex[Posi];
      if (FFrameType = 8) and (Pos('.awt', Result) + Pos('.swing.', Result) > 0)
      then
        Result := FConfiguration.AllClasses.ValueFromIndex[Posi + 1];
      FConfiguration.ImportCache.Add(WithoutArray + '=' + Result);
      Exit;
    end;

    // search in AllInterfaces
    Posi := FConfiguration.AllInterfaces.IndexOfName(WithoutArray);
    if (Posi > -1) and (Posi < FConfiguration.AllInterfaces.Count) then
    begin
      Result := FConfiguration.AllInterfaces.ValueFromIndex[Posi];
      if (FFrameType = 8) and (Pos('.awt', Result) + Pos('.swing.', Result) > 0)
      then
        Result := FConfiguration.AllInterfaces.ValueFromIndex[Posi + 1];
      FConfiguration.ImportCache.Add(WithoutArray + '=' + Result);
      Exit;
    end;
  end;
end;

procedure TJavaParser.ReadFullImport(const Import: string);
begin
  if not FConfiguration.IsAPIPackage(Import) then
    CollectClassesForImport(Import, FSourcepath, FPackagename,
      FUserImportClasses);
end;

{ $WARNINGS OFF }
procedure TJavaParser.CollectClassesForImport(const Import, Sourcepath,
  Package: string; UserImports: TStringList);
var PackageDir, Cp1, Cp2, Classnam: string; Posi: Integer; Found: Boolean;

  procedure CollectInJarFile(const JarFilename, Importname: string);
  var JarFile: TZipFile; Str: string;
  begin
    JarFile := TZipFile.Create;
    try
      try
        JarFile.Open(JarFilename, zmRead);
        for var I := 0 to JarFile.FileCount - 1 do
        begin
          Str := JarFile.FileNames[I];
          if Pos(Importname, Str) = 1 then
          begin
            Found := True;
            Classnam := ChangeFileExt(Copy(Str, Length(Importname) + 2,
              Length(Str)), '');
            if (Pos('\', Classnam) = 0) and (Pos('$', Classnam) = 0) then
              UserImports.Add(Classnam + '=' + ReplaceStr(ChangeFileExt(Str,
                ''), '\', '.'));
          end;
          if ThreadAbort then
            Break;
        end;
      except
        on E: Exception do
          OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' +
            E.Message));
      end;
    finally
      JarFile.Free;
    end;
  end;

  procedure CollectInDirectory(const Cp2, Ext: string);
  var FileName, Filepath, PackageFilename: string;
  begin
    if not DirectoryExists(Cp2) then
      Exit;

    var
    FileNames := TDirectory.GetFiles(Cp2, Ext);
    Found := (Length(FileNames) > 0);
    for Filepath in FileNames do
    begin
      FileName := ChangeFileExt(ExtractFileName(Filepath), '');
      PackageFilename := Package + '.' + FileName;
      if Pos('$', PackageFilename) = 0 then
        UserImports.Add(FileName + '=' + PackageFilename);
    end;
  end;

begin
  Found := False;
  PackageDir := IncludeTrailingPathDelimiter
    (FConfiguration.GetPackageDirectoryRelativ(Sourcepath, Package));
  Cp1 := UnHideBlanks(FConfiguration.GetClassPathJarExpanded(Sourcepath,
    Package)) + ';';
  Posi := Pos(';', Cp1);
  while (Posi > 0) and not Found do
  begin
    Cp2 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    if (ExtractFileExt(Cp2) = '.jar') and (ExtractFileName(Cp2) <> 'rt.jar') and
      FileExists(Cp2) then
      CollectInJarFile(Cp2, ReplaceStr(Import, '.', '\'))
    else
    begin
      Cp2 := TPath.Combine(Cp2, ReplaceStr(Import, '.', '\'));
      CollectInDirectory(Cp2, '\*.class');
      CollectInDirectory(Cp2, '\*.java');
    end;
    Posi := Pos(';', Cp1);
  end;
end;

procedure TJavaParser.CollectDirClassesForImport(const Import, Sourcepath,
  Package: string; UserImports: TStringList);
var PackageDir, Cp1, Cp2: string; Posi: Integer; Found: Boolean;

  procedure CollectInDirectory(const Cp2, Ext: string);
  var FileName, Filepath, PackageFilename: string;
  begin
    if not DirectoryExists(Cp2) then
      Exit;

    var
    FileNames := TDirectory.GetFiles(Cp2, Ext);
    Found := (Length(FileNames) > 0);
    for Filepath in FileNames do
    begin
      FileName := ChangeFileExt(ExtractFileName(Filepath), '');
      PackageFilename := Package + '.' + FileName;
      if Pos('$', PackageFilename) = 0 then
        UserImports.Add(FileName + '=' + PackageFilename);
    end;
  end;

begin
  Found := False;
  PackageDir := IncludeTrailingPathDelimiter
    (FConfiguration.GetPackageDirectorySecure(Sourcepath, Package));
  Cp1 := UnHideBlanks(FConfiguration.GetClassPath(Sourcepath, Package)) + ';';
  Posi := Pos(';', Cp1);
  while (Posi > 0) and not Found do
  begin
    Cp2 := Copy(Cp1, 1, Posi - 1);
    Delete(Cp1, 1, Posi);
    if not(ExtractFileExt(Cp2) = '.jar') and not EndsWith(Cp2, '*') then
    begin
      Cp2 := TPath.Combine(Cp2, ReplaceStr(Import, '.', '\'));
      CollectInDirectory(Cp2, '*.class');
      CollectInDirectory(Cp2, '*.java');
    end;
    Posi := Pos(';', Cp1);
  end;
end;

function TJavaParser.GetTypeName: string;
begin
  Result := FScanner.GetTypeName;
end;

function TJavaParser.GetClassName(Str: string): string;
begin
  Delete(Str, 1, LastDelimiter('.', Str));
  Delete(Str, 1, LastDelimiter('$', Str));
  var
  Posi := Pos('<', Str);
  if Posi > 0 then // classname<generic>
    Delete(Str, Posi, Length(Str));
  Result := Str;
end;

function TJavaParser.NeedSource(const SourceName, Packagename: string): Boolean;
var Stream: TStream; Parser: TJavaParser; Str: string;
begin
  Result := False;
  if Assigned(NeedPackage) then
  begin
    Str := WithoutArray(SourceName);
    NeedPackage(Str, Packagename, Stream);
    if Assigned(Stream) then
    begin
      Parser := TJavaParser.Create(False);
      try
        Parser.NeedPackage := NeedPackage;
        if Str <> SourceName then
        begin
          Parser.ParseStream(Stream, FOM.ModelRoot, FOM, Str, True, True);
          FOM.ModelRoot.Files.Add(Str);
        end
        else
          Parser.ParseStream(Stream, FOM.ModelRoot, FOM, FFilename, True, True);
      finally
        Parser.Free;
      end;
      Result := True;
    end;
  end;
end;

function TJavaParser.ShowView(IsInner: Boolean): Boolean;
begin
  Result := FWithView;
  if FConfiguration.ShowPublicOnly and (FModVisibility <> viPublic) then
    Result := False;
  Result := Result or FConfiguration.ShowAlways;
  if IsInner then
    Result := True;
end;

function TJavaParser.ThreadAbort: Boolean;
begin
  Result := Assigned(Thread) and Thread.Abort;
end;

procedure TJavaParser.AddStructure(From, To_: Integer);
begin
  Inc(FDepth);
  if FDepth > FStructures.MaxDepth then
    FStructures.MaxDepth := FDepth;
  var
  Structure := FStructures.NewStructure;
  Structure.FTextRect.TopLeft := Point((FDepth - 1) * FConfiguration.Indent
    + 1, From);
  Structure.FTopBottom := To_;
  Structure.FTopBottomRight := FScanner.LastTokenColumn;
  Structure.FDepth := FDepth;
  FFilo.Add(Structure);
end;

procedure TJavaParser.AddStructure(From, To_: Integer;
  SingleStatement: Boolean);
begin
  AddStructure(From, To_);
  var
  Structure := TStructureEx(FFilo.Last);
  Structure.SingleStatement := SingleStatement;
end;

procedure TJavaParser.AddStructureDefault;
begin
  AddStructure(FScanner.TokenLine, FScanner.TokenLine);
end;

procedure TJavaParser.CloseStructure(Line: Integer);
begin
  if FFilo.Count > 0 then
  begin
    var
    Structure := TStructureEx(FFilo.Last);
    FFilo.Delete(FFilo.Count - 1);
    Structure.FTextRect.BottomRight :=
      Point((FDepth - 1) * FConfiguration.Indent + 1, Line);
    Structure.FBottomTop := Line;
    FStructures.Add(Structure);
    Dec(FDepth);
  end;
end;

procedure TJavaParser.CloseStructure(Line: Integer; SingleStatement: Boolean);
begin
  CloseStructure(Line);
  FStructures[FStructures.Count - 1].SingleStatement := SingleStatement;
end;

procedure TJavaParser.CloseStructureDefault;
begin
  CloseStructure(FScanner.TokenLine, False);
end;

initialization

Integrators.Register(TJavaImporter);

end.
