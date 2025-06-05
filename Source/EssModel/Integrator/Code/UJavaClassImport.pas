{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

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

unit UJavaClassImport;

interface

uses
  Classes,
  UIntegrator;

type
  TJavaClassImporter = class(TImportIntegrator)
  private
    procedure NeedPackageHandler(var AName: string; Packagename: string;
      var AStream: TStream; OnlyLookUp: Boolean = False);
  public
    procedure ImportOneFile(const Filename: string;
      WithoutNeedSource: Boolean = False); override;
    class function GetFileExtensions: TStringList; override;
  end;

implementation

uses
  SysUtils,
  UJavaClass,
  UConfiguration,
  UUtils,
  UModelEntity,
  UModel,
  UCodeParser;

type
  TJavaClassParser = class(TCodeParser)
  private
    FObjectModel: TObjectModel;
    FWithView: Boolean;
    function GetVisibility(Flags: Integer): TVisibility;
    function ExtractPackageName(const CName: string): string;
    function ExtractClassName(const CName: string): string;
    function GetFieldType(const Field: string; var Index: Integer): TClassifier;
    function NeedClassifier(const CName: string;
      TheClass: TModelEntityClass = nil): TClassifier;
    function ShowView(Visibility: TVisibility): Boolean;
  public
    constructor Create(FWithView: Boolean);
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage;
      AOM: TObjectModel; Filename: string; Inner: Boolean;
      WithoutNeedSource: Boolean = False); override;
  end;

  { TJavaClassImporter }

procedure TJavaClassImporter.ImportOneFile(const Filename: string;
  WithoutNeedSource: Boolean);
var
  Str: TStream;
  Parser: TJavaClassParser;
begin
  Str := CodeProvider.LoadStream(Filename);
  if Assigned(Str) then
  begin
    Parser := TJavaClassParser.Create(True);
    try
      Parser.NeedPackage := NeedPackageHandler;
      Parser.ParseStream(Str, Model.ModelRoot, Model, Filename, False);
    finally
      FreeAndNil(Parser);
    end;
  end;
end;

procedure TJavaClassImporter.NeedPackageHandler(var AName: string;
  Packagename: string; var AStream: TStream; OnlyLookUp: Boolean = False);
var
  Filename: string;
begin
  AStream := nil;
  Filename := AName + '.class';
  Filename := CodeProvider.LocateFile(Filename);
  // Dont read same file twice
  if (not OnlyLookUp) and (Filename <> '') and (FFilesRead.IndexOf(Filename) = -1)
  then
  begin
    AStream := CodeProvider.LoadStream(Filename);
    AName := Filename;
    FFilesRead.Add(Filename);
  end;
end;

class function TJavaClassImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.class'] := 'Java Class';
end;

{ TJavaClassParser }

constructor TJavaClassParser.Create(FWithView: Boolean);
begin
  inherited Create;
  Self.FWithView := FWithView;
end;

procedure TJavaClassParser.ParseStream(AStream: TStream;
  AModel: TAbstractPackage; AOM: TObjectModel; Filename: string; Inner: Boolean;
  WithoutNeedSource: Boolean);
var
  ClassFile: TClassFile;
  AUnit: TUnitPackage;
  AClass: TClass;
  Intf: TInterface;
  Str: string;

  procedure ParseOp(Operation: TOperation; Method: TMethodInfo);
  var
    Desc: string;
    Int, J: Integer;
  begin
    Operation.Visibility := GetVisibility(Method.AccessFlags);
    Operation.IsAbstract := TAccData.IsAbstract(Method.AccessFlags);
    Operation.Static := TAccData.IsStatic(Method.AccessFlags);
    Desc := Method.Descriptor.GetString;
    Int := 1;
    if Desc[Int] = '(' then
    begin
      // parameters
      J := 0;
      Inc(Int);
      while (Desc[Int] <> ')') and (Int < Length(Desc)) do
      begin
        Operation.AddParameter(Char(Ord('a') + J)).TypeClassifier :=
          GetFieldType(Desc, Int);
        Inc(J);
      end;
      Inc(Int);
    end;
    if Desc[Int] <> 'V' then
      Operation.ReturnValue := GetFieldType(Desc, Int);
    if Method.IsConstructor then
    begin
      Operation.OperationType := otConstructor;
      Operation.Name := AClass.GetShortType;
    end
    else if Assigned(Operation.ReturnValue) then
      Operation.OperationType := otFunction
    else
      Operation.OperationType := otProcedure;
  end;

  procedure ParseAttr(Attr: TAttribute; FieldInfo: TFieldInfo);
  var
    Int: Integer;
  begin
    Attr.Visibility := GetVisibility(FieldInfo.AccessFlags);
    Attr.Static := TAccData.IsStatic(FieldInfo.AccessFlags);
    Int := 1;
    Attr.TypeClassifier := GetFieldType(FieldInfo.Descriptor.GetString, Int);
  end;

begin
  FModel := AModel;
  FObjectModel := AOM;

  ClassFile := TClassFile.Create(AStream);
  try
    if not Assigned(ClassFile.Header) then
      Exit;

    AUnit := FObjectModel.ModelRoot.FindUnitPackage('Default');
    if not Assigned(AUnit) then
      AUnit := (FModel as TLogicPackage).AddUnit('Default');
    if TAccData.IsInterface(ClassFile.ClassDecl.AccessFlags) then
    begin
      // interface
      Intf := AUnit.MakeInterface(ClassFile.ClassName, Filename);
      Intf.Visibility := GetVisibility(ClassFile.ClassDecl.AccessFlags);
      AUnit.AddInterface(Intf);
      for var I := 0 to Length(ClassFile.ClassFields.ClassFields) - 1 do
      begin
        Str := ClassFile.ClassFields.ClassFields[I].Name.GetString;
        if (Length(Str) > 0) and (Str[1] <> '$') then
          ParseAttr(Intf.AddAttribute(Str, nil),
            ClassFile.ClassFields.ClassFields[I]);
      end;

      for var I := 0 to Length(ClassFile.ClassMethods.ClassMethods) - 1 do
      begin
        Str := ClassFile.ClassMethods.ClassMethods[I].Name.GetString;
        if (Length(Str) > 0) and (not CharInSet(Str[1], ['<', '$'])) then
          ParseOp(Intf.AddOperationWithoutType(Str),
            ClassFile.ClassMethods.ClassMethods[I]);
      end;

    end
    else
    begin
      AClass := AUnit.MakeClass(ClassFile.ClassName, Filename);
      AClass.Visibility := GetVisibility(ClassFile.ClassDecl.AccessFlags);
      AClass.IsVisible := ShowView(AClass.Visibility);
      AUnit.AddClass(AClass);

      // ancestor
      if Assigned(ClassFile.ClassDecl.SuperClass) then
      begin
        Str := TObjNameFormat.ToDotSeparator
          (ClassFile.ClassDecl.SuperClass.GetString);
        if Str <> 'java.lang.Object' then
          AClass.Ancestor := NeedClassifier(Str, TClass) as TClass;
      end;
      // implements
      for var I := 0 to Length(ClassFile.ClassDecl.Interfaces) - 1 do
      begin
        Intf := NeedClassifier(TObjNameFormat.ToDotSeparator
          (ClassFile.ClassDecl.Interfaces[I].GetString), TInterface)
          as TInterface;
        AClass.AddImplements(Intf);
      end;
      for var I := 0 to Length(ClassFile.ClassFields.ClassFields) - 1 do
      begin
        Str := ClassFile.ClassFields.ClassFields[I].Name.GetString;
        if (Length(Str) > 0) and (Str[1] <> '$') then
          ParseAttr(AClass.AddAttribute(Str, nil),
            ClassFile.ClassFields.ClassFields[I]);
      end;
      for var I := 0 to Length(ClassFile.ClassMethods.ClassMethods) - 1 do
      begin
        Str := ClassFile.ClassMethods.ClassMethods[I].Name.GetString;
        if Str = '<init>' then // Constructor has internal name '<init>'
          Str := AClass.Name;
        if (Length(Str) > 0) and (not CharInSet(Str[1], ['<', '$'])) then
          ParseOp(AClass.AddOperationWithoutType(Str),
            ClassFile.ClassMethods.ClassMethods[I]);
      end;
    end;

  finally
    FreeAndNil(ClassFile);
  end;
end;

// Translate java-visibility
function TJavaClassParser.GetVisibility(Flags: Integer): TVisibility;
begin
  Result := viPrivate;
  if TAccData.IsPublic(Flags) then
    Result := viPublic
  else if TAccData.IsPrivate(Flags) then
    Result := viPrivate
  else if TAccData.IsProtected(Flags) then
    Result := viProtected;
end;

function TJavaClassParser.ExtractPackageName(const CName: string): string;
var
  Int: Integer;
begin
  Int := LastDelimiter('.', CName);
  if Int = 0 then
    Result := 'Default'
  else
    Result := Copy(CName, 1, Int - 1);
end;

// Extract short class name
function TJavaClassParser.ExtractClassName(const CName: string): string;
var
  Int: Integer;
begin
  Int := LastDelimiter('.', CName);
  if Int = 0 then
    Result := CName
  else
    Result := Copy(CName, Int + 1, 255);
end;

function TJavaClassParser.NeedClassifier(const CName: string;
  TheClass: TModelEntityClass = nil): TClassifier;
var
  PName, ShortName: string;
  UnitPackage: TUnitPackage;
  Parser: TJavaClassParser;
  Str: TStream;
begin
  Result := nil;
  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);

  // First look in model
  UnitPackage := FObjectModel.ModelRoot.FindUnitPackage(PName);
  if Assigned(UnitPackage) then
    Result := UnitPackage.FindClassifier(ShortName, TheClass, True);

  // responsible for opening other classes too

  if not Assigned(Result) then
  begin
    // See if we can find the file that we need
    Str := nil;
    if Assigned(NeedPackage) then
      NeedPackage(ShortName, PName, Str);
    if Assigned(Str) then
    begin
      Parser := TJavaClassParser.Create(False);
      try
        Parser.NeedPackage := NeedPackage;
        Parser.ParseStream(Str, FObjectModel.ModelRoot, FObjectModel,
          ShortName, True);
        FObjectModel.ModelRoot.Files.Add(ShortName);
      finally
        FreeAndNil(Parser);
      end;
      UnitPackage := FObjectModel.ModelRoot.FindUnitPackage(PName);
      if Assigned(UnitPackage) then
        Result := UnitPackage.FindClassifier(CName, TheClass, True);
    end;
  end;

  if not Assigned(Result) then
  begin
    // Look in unknown package
    Result := FObjectModel.UnknownPackage.FindClassifier(CName, TheClass, True);
    if not Assigned(Result) then
    begin
      if not Assigned(TheClass) or (TheClass = TClass) then
        Result := FObjectModel.UnknownPackage.MakeClass(CName, '')
      else if TheClass = TInterface then
        Result := FObjectModel.UnknownPackage.MakeInterface(CName, '')
      else if TheClass = TDataType then
        Result := FObjectModel.UnknownPackage.AddDatatype(CName);
    end;
  end;

  if not Assigned(Result) then
    raise Exception.Create(ClassName + ' failed to locate ' + CName);
end;

function TJavaClassParser.GetFieldType(const Field: string; var Index: Integer)
  : TClassifier;
var
  DimCount, Int: Integer;
  Str: string;
  IsPrimitive: Boolean;
begin
  Result := nil;
  DimCount := 0;
  while Field[Index] = '[' do
  begin
    Inc(DimCount);
    Inc(Index);
  end;
  IsPrimitive := True;
  case Field[Index] of
    'B':
      Str := 'byte';
    'C':
      Str := 'char';
    'D':
      Str := 'double';
    'F':
      Str := 'float';
    'I':
      Str := 'int';
    'J':
      Str := 'long';
    'L':
      begin
        Inc(Index);
        Int := Index;
        while (Field[Int] <> ';') and (Int < Length(Field)) do
          Inc(Int);
        Str := TObjNameFormat.ToDotSeparator(Copy(Field, Index, Int - Index));
        Index := Int;
        IsPrimitive := False;
      end;
    'S':
      Str := 'short';
    'Z':
      Str := 'boolean';
  end;
  Inc(Index);
  for var I := 0 to DimCount - 1 do
    Str := Str + '[]';

  if Str <> '' then
  begin
    if IsPrimitive then
      Result := NeedClassifier(Str, TDataType)
    else
      Result := NeedClassifier(Str);
  end;
end;

function TJavaClassParser.ShowView(Visibility: TVisibility): Boolean;
begin
  Result := FWithView;
  if FConfiguration.ShowPublicOnly and (Visibility <> viPublic) then
    Result := False;
  Result := Result or FConfiguration.ShowAlways;
end;

initialization

Integrators.Register(TJavaClassImporter);

end.
