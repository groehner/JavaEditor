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

uses  Classes, uIntegrator;

type
  TJavaClassImporter = class(TImportIntegrator)
  private
    procedure NeedPackageHandler(var AName: string; Packagename: string;
                             var AStream: TStream; OnlyLookUp: Boolean = False);
  public
    procedure ImportOneFile(const FileName : string; withoutNeedSource: boolean = false); override;
    class function GetFileExtensions : TStringList; override;
  end;


implementation

uses uJavaClass, SysUtils, UConfiguration, UUtils, uModelEntity, uModel,
     uCodeProvider, uCodeParser;

type
  TJavaClassParser = class(TCodeParser)
  private
    OM : TObjectModel;
    WithView: boolean;
    function GetVisibility(flags : integer) : TVisibility;
    function ExtractPackageName(const CName : string) : string;
    function ExtractClassName(const CName : string) : string;
    function GetFieldType(const Field : string; var Index : integer) : TClassifier;
    function NeedClassifier(const CName :  string; TheClass : TModelEntityClass = nil) : TClassifier;
    function ShowView(Visibility: TVisibility): boolean;
  public
    constructor create(aWithView: boolean);
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel;
                filename: string; inner: boolean; withoutNeedSource: boolean = false); override;
  end;

{ TJavaClassImporter }

procedure TJavaClassImporter.ImportOneFile(const FileName : string; withoutNeedSource: boolean);
var
  Str : TStream;
  Parser : TJavaClassParser;
begin
  Str := CodeProvider.LoadStream(FileName);
  if Assigned(Str) then
  begin
    Parser := TJavaClassParser.Create(true);
    try
      Parser.NeedPackage := NeedPackageHandler;
      Parser.ParseStream(Str, Model.ModelRoot, Model, Filename, false);
    finally
      FreeAndNil(Parser);
    end;
  end;
end;

procedure TJavaClassImporter.NeedPackageHandler(var AName: string; Packagename: string;
                             var AStream: TStream; OnlyLookUp: Boolean = False);
var
  FileName : string;
begin
  AStream := nil;
  FileName := AName + '.class';
  FileName := CodeProvider.LocateFile(FileName);
  //Dont read same file twice
  if (not OnlyLookUp) and (FileName<>'') and (FilesRead.IndexOf(FileName)=-1) then
  begin
    AStream := CodeProvider.LoadStream(FileName);
    AName:= FileName;
    FilesRead.Add(FileName);
  end;
end;

class function TJavaClassImporter.GetFileExtensions: TStringList;
begin
  Result := TStringList.Create;
  Result.Values['.class'] := 'Java Class';
end;

{ TJavaClassParser }

constructor TJavaClassParser.create(aWithView: boolean);
begin
  inherited create;
  Self.WithView:= aWithView;
end;

procedure TJavaClassParser.ParseStream(AStream: TStream; AModel: TAbstractPackage; AOM: TObjectModel;
           filename: string; inner: boolean; withoutNeedSource: boolean);
var
  JC : TClassFile;
  U : TUnitPackage;
  C : TClass;
  Int : TInterface;
  I : integer;
  S : string;

  procedure ParseOp(Op : TOperation; Met : TMethodInfo);
  var
    Desc : string;
    I,J : integer;
  begin
    Op.Visibility := GetVisibility(Met.access_flags);
    Op.IsAbstract := TAccData.isAbstract(Met.access_flags);
    Op.Static     := TAccData.isStatic(Met.access_flags);
    Desc := Met.descriptor.GetString;
    I := 1;
    if Desc[I]='(' then
    begin
      //parameters
      J := 0;
      Inc(I);
      while (Desc[I]<>')') and (I<Length(Desc)) do begin
        Op.AddParameter( char( Ord('a') + J ) ).TypeClassifier := GetFieldType(Desc,I);
        inc(j);
      end;
      Inc(I);
    end;
    if Desc[I]<>'V' then
      Op.ReturnValue := GetFieldType(Desc,I);
    if Met.isConstructor then
    begin
      Op.OperationType := otConstructor;
      Op.Name := C.GetShortType;
    end else if Assigned(Op.ReturnValue) then
      Op.OperationType := otFunction
    else
      Op.OperationType := otProcedure
  end;

  procedure ParseAttr(Attr : TAttribute; Fi : TFieldInfo);
  var
    I : integer;
  begin
    Attr.Visibility := GetVisibility(Fi.access_flags);
    Attr.Static:= TAccData.isStatic(Fi.access_flags);
    I := 1;
    Attr.TypeClassifier := GetFieldType(Fi.descriptor.getString,I);
  end;

begin
  FModel := AModel;
  OM := AOM;

  JC := TClassFile.Create(AStream);
  try
    if JC.header = nil then exit;

    U := OM.ModelRoot.FindUnitPackage('Default');
    if not Assigned(U) then
      U := (FModel as TLogicPackage).AddUnit('Default');
    if TAccData.isInterface( JC.classDecl.accessFlags ) then
    begin
      //interface
      Int := U.MakeInterface( JC.ClassName, Filename);
      Int.Visibility := GetVisibility( JC.classDecl.accessFlags );
      U.AddInterface(Int);
      for I:=0 to Length(JC.classFields.classFields)-1 do
      begin
        S := JC.classFields.classFields[I].name.GetString;
        if (Length(S)>0) and (S[1]<>'$') then
          ParseAttr( Int.AddAttribute( S, nil ),JC.classFields.classFields[I] );
      end;

      for I:=0 to Length(JC.classMethods.classMethods)-1 do
      begin
        S := JC.classMethods.classMethods[I].name.GetString;
        if (Length(S) > 0) and (not CharInset(S[1], ['<','$'])) then
          ParseOp( Int.AddOperationWithoutType( S) ,JC.classMethods.classMethods[I]);
      end;

    end
    else
    begin
      //class
      //C := U.AddClass( ExtractClassName(JC.ClassName), Filename);
      C := U.MakeClass( JC.ClassName, Filename);
      C.Visibility := GetVisibility( JC.classDecl.accessFlags );
      C.IsVisible:= ShowView(C.Visibility);
      U.AddClass(C);

      //ancestor
      if Assigned(JC.classDecl.superClass) then begin
        S := TObjNameFormat.ToDotSeparator(JC.classDecl.superClass.getString);
        if S <> 'java.lang.Object' then
          C.Ancestor := NeedClassifier( S , TClass) as TClass;
      end;
      //implements
      for I := 0 to Length(JC.classDecl.interfaces)-1 do begin
        Int:= NeedClassifier( TObjNameFormat.toDotSeparator(JC.classDecl.interfaces[I].getString), TInterface ) as TInterface;
        C.AddImplements(Int);
      end;
      for I:=0 to Length(JC.classFields.classFields)-1 do
      begin
        S := JC.classFields.classFields[I].name.GetString;
        if (Length(S)>0) and (S[1]<>'$') then
          ParseAttr( C.AddAttribute( S, nil ),JC.classFields.classFields[I] );
      end;
      for I:=0 to Length(JC.classMethods.classMethods)-1 do
      begin
        S := JC.classMethods.classMethods[I].name.GetString;
        if S='<init>' then  //Constructor has internal name '<init>'
          S := C.Name;
        if (Length(S)>0) and (not CharInset(S[1], ['<','$'])) then
          ParseOp( C.AddOperationwithoutType( S ) ,JC.classMethods.classMethods[I]);
      end;
    end;

  finally
    FreeAndNil(JC);
  end;
end;

//Translate java-visibility
function TJavaClassParser.GetVisibility(flags: integer): TVisibility;
begin
  Result := viPrivate;
  if TAccData.isPublic( flags ) then
    Result := viPublic
  else if TAccData.isPrivate( flags ) then
    Result := viPrivate
  else if TAccData.isProtected( flags ) then
    Result := viProtected;
end;

function TJavaClassParser.ExtractPackageName(const CName: string): string;
var
  I : integer;
begin
  I := LastDelimiter('.',CName);
  if I=0 then
    Result := 'Default'
  else
    Result := Copy(CName,1,I-1);
end;

//Extract short class name
function TJavaClassParser.ExtractClassName(const CName: string): string;
var
  I : integer;
begin
  I := LastDelimiter('.',CName);
  if I=0 then
    Result := CName
  else
    Result := Copy(CName,I+1,255);
end;


function TJavaClassParser.NeedClassifier(const CName: string; TheClass : TModelEntityClass = nil): TClassifier;
var
  PName,ShortName : string;
  U : TUnitPackage;
  Parser : TJavaClassParser;
  Str : TStream;
begin
  Result := nil;
  PName := ExtractPackageName(CName);
  ShortName := ExtractClassName(CName);

  //First look in model
  U := OM.ModelRoot.FindUnitPackage(PName);
  if Assigned(U) then
    Result := U.FindClassifier(ShortName,TheClass,True);

  // responsible for opening other classes too

  if not Assigned(Result) then begin
    //See if we can find the file that we need
    Str := nil;
    if Assigned(NeedPackage) then
      NeedPackage( ShortName , PName, str);
    if Assigned(Str) then begin
      Parser := TJavaClassParser.Create(false);
      try
        Parser.NeedPackage := NeedPackage;
        Parser.ParseStream(Str, OM.ModelRoot, OM, Shortname, true);
        OM.ModelRoot.Files.Add(Shortname);
      finally
        FreeAndNil(Parser);
      end;
      U := OM.ModelRoot.FindUnitPackage(PName);
      if Assigned(U) then
        Result := U.FindClassifier(CName,TheClass,True);
    end;
  end;

  if not Assigned(Result) then begin
    //Look in unknown package
    Result := OM.UnknownPackage.FindClassifier(CName,TheClass,True);
    if not Assigned(Result) then
    begin
      if (TheClass = nil) or (TheClass = TClass) then
        Result := OM.UnknownPackage.MakeClass(CName, '')
      else if TheClass=TInterface then
        Result := OM.UnknownPackage.MakeInterface(CName, '')
      else if TheClass=TDataType then
        Result := OM.UnknownPackage.AddDataType(CName)
    end;
  end;

  if not Assigned(Result) then
    raise Exception.Create(ClassName + ' failed to locate ' + Cname);
end;

function TJavaClassParser.GetFieldType(const Field: string; var Index: integer): TClassifier;
var
  DimCount,I : integer;
  S : string;
  IsPrimitive : boolean;
begin
  Result := nil;
  DimCount := 0;
  while Field[Index]='[' do
  begin
    Inc(DimCount);
    Inc(Index);
  end;
  IsPrimitive := True;
  case Field[Index] of
    'B' : S := 'byte';
    'C' : S := 'char';
    'D' : S := 'double';
    'F' : S := 'float';
    'I' : S := 'int';
    'J' : S := 'long';
    'L' :
      begin
        Inc(Index);
        I := Index;
        while (Field[I]<>';') and (I<Length(Field)) do
          Inc(I);
        S := TObjNameFormat.toDotSeparator( Copy(Field,Index,I-Index) );
        Index := I;
        IsPrimitive := False;
      end;
    'S' : S := 'short';
    'Z' : S := 'boolean';
  end;
  Inc(Index);
  for I := 0 to DimCount-1 do
    S := S + '[]';

  if S<>'' then begin
    if IsPrimitive then
      Result := NeedClassifier( S , TDataType)
    else
      Result := NeedClassifier( S );
  end;
end;

function TJavaClassParser.ShowView(Visibility: TVisibility): boolean;
begin
  Result:= WithView;
  if FConfiguration.ShowPublicOnly and (Visibility <> viPublic) then Result:= false;
  Result:= Result or FConfiguration.ShowAlways;
end;

initialization
  Integrators.Register(TJavaClassImporter);

end.
