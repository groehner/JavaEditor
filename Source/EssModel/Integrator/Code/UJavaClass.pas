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

unit UJavaClass;

interface

uses Classes;

type
  TConstBase = class
  private
    FTag: Integer;
  public
    procedure Read(Input: TStream); virtual; abstract;
    procedure SetRef(const ObjAry: array of TConstBase); virtual;
    function GetString: string; virtual;
  end;

  TConstPool = class
  private
    FConstPoolCnt: Integer;
    FConstPool: array of TConstBase;
    function AllocConstEntry(FTag: Integer): TConstBase;
    procedure ResolveConstPool;
    procedure ReadConstPool(Input: TStream);
  public
    constructor Create(Input: TStream);
    destructor Destroy; override;
    function ConstPoolElem(FIndex: Integer): TConstBase;
  end;

  TConstUtf8 = class(TConstBase)
  private
    FStr: string;
  public
    procedure Read(Input: TStream); override;
    function GetString: string; override;
  end;

  TConstClassOrString = class(TConstBase)
  private
    FIndex: Integer;
    FUtf8: TConstUtf8;
  public
    procedure Read(Input: TStream); override;
    procedure SetRef(const ObjAry: array of TConstBase); override;
    function GetString: string; override;
  end;

  TConstLongConvert = class(TConstBase)
  private
    function ToLong(High, Low: Integer): Int64;
  protected
    function ReadLong(Input: TStream): Int64;
  end;

  TConstDouble = class(TConstLongConvert)
  private
    FDo: Double;
  public
    procedure Read(Input: TStream); override;
  end;

  TConstFloat = class(TConstBase)
  private
    FSin: Single;
  public
    procedure Read(Input: TStream); override;
  end;

  TConstInt = class(TConstBase)
  private
    FVal: Integer;
  public
    procedure Read(Input: TStream); override;
  end;

  TConstLong = class(TConstLongConvert)
  private
    FLongVal: Int64;
  public
    procedure Read(Input: TStream); override;
  end;

  TConstNameAndTypeInfo = class(TConstClassOrString)
  private
    FDescriptorIndex: Integer;
    FDescriptorUtf8: TConstUtf8;
  public
    procedure Read(Input: TStream); override;
    procedure SetRef(const ObjAry: array of TConstBase); override;
  end;

  TConstRef = class(TConstBase)
  private
    FIndex: Integer;
    FNameAndTypeIndex: Integer;
    FClassRef: TConstClassOrString;
    FNameRef: TConstNameAndTypeInfo;
  public
    procedure Read(Input: TStream); override;
    procedure SetRef(const ObjAry: array of TConstBase); override;
  end;

  TAccData = class
  public
    class function IsPublic(Val: Integer): Boolean;
    class function IsPrivate(Val: Integer): Boolean;
    class function IsProtected(Val: Integer): Boolean;
    class function IsStatic(Val: Integer): Boolean;
    class function IsFinal(Val: Integer): Boolean;
    class function IsSync(Val: Integer): Boolean;
    class function IsSuper(Val: Integer): Boolean;
    class function IsVolatile(Val: Integer): Boolean;
    class function IsTransient(Val: Integer): Boolean;
    class function IsNative(Val: Integer): Boolean;
    class function IsInterface(Val: Integer): Boolean;
    class function IsAbstract(Val: Integer): Boolean;
    class function IsStrict(Val: Integer): Boolean;
  end;

  TObjNameFormat = class
  public
    class function ToDotSeparator(const SlashName: string): string;
  end;

  TAttrInfo = class
  private
    FAttrName: string;
    FLen: Integer;
  public
    constructor Create(const Name: string; Length: Integer);
    function GetName: string;
  end;

  TAttrFactory = class
  private
    class procedure SkipData(FLen: Integer; Input: TStream);
  public
    class function AllocAttr(Input: TStream): TAttrInfo;
  end;

  TClassFileHeader = class
  private
    FMagic: LongWord;
    FMinorVersion: ShortInt;
    FMajorVersion: ShortInt;
  public
    constructor Create(Input: TStream);
  end;

  TConstBaseArr = array of TConstBase;

  TClassDeclSec = class
  private
    FAccessFlags: Integer;
    FThisClass: TConstBase;
    FSuperClass: TConstBase;
    FInterfaces: TConstBaseArr;
  public
    constructor Create(Input: TStream; ConstPoolSec: TConstPool);
    function GetClassName: string;
    property AccessFlags: Integer read FAccessFlags;
    property ThisClass: TConstBase read FThisClass;
    property SuperClass: TConstBase read FSuperClass;
    property Interfaces: TConstBaseArr read FInterfaces;
  end;

  TAttrInfoArr = array of TAttrInfo;

  TFieldInfo = class
  private
    FAccessFlags: Integer;
    FAttributes: TAttrInfoArr;
    FDescriptor: TConstUtf8;
    FName: TConstUtf8;
  public
    constructor Create(Input: TStream; ConstPoolSec: TConstPool);
    property AccessFlags: Integer read FAccessFlags;
    property Attributes: TAttrInfoArr read FAttributes;
    property Descriptor: TConstUtf8 read FDescriptor;
    property Name: TConstUtf8 read FName;
  end;

  TFieldInfoArr = array of TFieldInfo;

  TClassFieldsec = class
  private
    FClassFields: TFieldInfoArr;
  public
    constructor Create(Input: TStream; ConstPoolSec: TConstPool);
    property ClassFields: TFieldInfoArr read FClassFields;
  end;

  TMethodInfo = class
  private
    FAccessFlags: Integer;
    FAttributes: TAttrInfoArr;
    FDescriptor: TConstUtf8;
    FName: TConstUtf8;
  public
    constructor Create(Input: TStream; ConstPoolSec: TConstPool);
    function IsConstructor: Boolean;
    property AccessFlags: Integer read FAccessFlags;
    property Attributes: TAttrInfoArr read FAttributes;
    property Descriptor: TConstUtf8 read FDescriptor;
    property Name: TConstUtf8 read FName;
  end;

  TMethodInfoArr = array of TMethodInfo;

  TClassMethodsec = class
  private
    FClassMethods: TMethodInfoArr;
  public
    constructor Create(Input: TStream; ConstPoolSec: TConstPool);
    destructor Destroy; override;
    property ClassMethods: TMethodInfoArr read FClassMethods;
  end;

  TClassAttrsec = class
  private
    FClassAttrTab: array of TAttrInfo;
  public
    constructor Create(Input: TStream);
  end;

  TClassFile = class
  private
    FClassname: string;
    FClassAttrs: TClassAttrsec;
    FClassConstPool: TConstPool;
    FClassDecl: TClassDeclSec;
    FClassFields: TClassFieldsec;
    FClassMethods: TClassMethodsec;
    FHeader: TClassFileHeader;
  public
    constructor Create(Input: TStream);
    destructor Destroy; override;
    property AClassName: string read FClassname;
    property ClassAttrs: TClassAttrsec read FClassAttrs;
    property ClassConstPool: TConstPool read FClassConstPool;
    property ClassDecl: TClassDeclSec read FClassDecl;
    property ClassFields: TClassFieldsec read FClassFields;
    property ClassMethods: TClassMethodsec read FClassMethods;
    property Header: TClassFileHeader read FHeader;
  end;

implementation

uses
  SysUtils,
  UUtils;

const
  ACC_PUBLIC: Word = $0001;
  ACC_PRIVATE: Word = $0002;
  ACC_PROTECTED: Word = $0004;
  ACC_STATIC: Word = $0008;
  ACC_FINAL: Word = $0010;
  ACC_SYNC: Word = $0020;
  ACC_VOLATILE: Word = $0040;
  ACC_TRANSIENT: Word = $0080;
  ACC_NATIVE: Word = $0100;
  ACC_INTERFACE: Word = $0200;
  ACC_ABSTRACT: Word = $0400;
  ACC_STRICT: Word = $0800;

  CONSTANT_Class = 7;
  CONSTANT_Fieldref = 9;
  CONSTANT_Methodref = 10;
  CONSTANT_InterfaceMethodref = 11;
  CONSTANT_String = 8;
  CONSTANT_Integer = 3;
  CONSTANT_Float = 4;
  CONSTANT_Long = 5;
  CONSTANT_Double = 6;
  CONSTANT_NameAndType = 12;
  CONSTANT_Utf8 = 1;

function ReadU1(Input: TStream): Integer;
var
  ByteVal: Byte;
begin
  Input.Read(ByteVal, 1);
  Result := ByteVal;
end;

function ReadU2(Input: TStream): Integer;
var
  Tmp: array [0 .. 1] of Byte;
begin
  FillChar(Tmp, 2, 0);
  Input.Read(Tmp, 2);
  Result := (Tmp[0] shl 8) or Tmp[1];
end;

function ReadU4(Input: TStream): LongWord;
var
  Tmp: array [0 .. 3] of Byte;
begin
  // $BEBAFECA
  Input.Read(Tmp, 4);
  Result := (Tmp[0] shl 24) or (Tmp[1] shl 16) or (Tmp[2] shl 8) or Tmp[3];
end;

{ TClassFileHeader }

constructor TClassFileHeader.Create(Input: TStream);
begin
  FMagic := ReadU4(Input);
  FMinorVersion := ReadU2(Input);
  FMajorVersion := ReadU2(Input);
end;

{ TClassDeclSec }

constructor TClassDeclSec.Create(Input: TStream; ConstPoolSec: TConstPool);
var
  ThisClassIdx, SuperClassIdx, IinterfaceCnt, Idx: Integer;
begin
  FAccessFlags := ReadU2(Input);
  ThisClassIdx := ReadU2(Input);
  SuperClassIdx := ReadU2(Input);

  FThisClass := ConstPoolSec.ConstPoolElem(ThisClassIdx);
  FSuperClass := ConstPoolSec.ConstPoolElem(SuperClassIdx);

  IinterfaceCnt := ReadU2(Input);

  if IinterfaceCnt > 0 then
  begin
    SetLength(FInterfaces, IinterfaceCnt);
    for var I := 0 to IinterfaceCnt - 1 do
    begin
      Idx := ReadU2(Input);
      FInterfaces[I] := ConstPoolSec.ConstPoolElem(Idx);
    end;
  end;
end;

function TClassDeclSec.GetClassName: string;
var
  Name: string;
begin
  if Assigned(FThisClass) then
    if FThisClass is TConstClassOrString then
      Name := TObjNameFormat.ToDotSeparator(FThisClass.GetString);
  Result := Name;
end;

{ TFieldInfo }

constructor TFieldInfo.Create(Input: TStream; ConstPoolSec: TConstPool);
var
  NameIndex, DescIndex, AttrCnt: Integer;
  Obj: TConstBase;
begin
  FAccessFlags := ReadU2(Input);
  NameIndex := ReadU2(Input);
  DescIndex := ReadU2(Input);
  AttrCnt := ReadU2(Input);

  Obj := ConstPoolSec.ConstPoolElem(NameIndex);
  if Assigned(Obj) and (Obj is TConstUtf8) then
    FName := Obj as TConstUtf8;

  Obj := ConstPoolSec.ConstPoolElem(DescIndex);
  if Assigned(Obj) and (Obj is TConstUtf8) then
    FDescriptor := Obj as TConstUtf8;

  if AttrCnt > 0 then
  begin
    SetLength(FAttributes, AttrCnt);
    for var I := 0 to AttrCnt - 1 do
      FAttributes[I] := TAttrFactory.AllocAttr(Input);
  end;
end;

{ TClassFieldsec }

constructor TClassFieldsec.Create(Input: TStream; ConstPoolSec: TConstPool);
var
  FieldCnt: Integer;
begin
  FieldCnt := ReadU2(Input);
  if FieldCnt > 0 then
    SetLength(FClassFields, FieldCnt);

  for var I := 0 to FieldCnt - 1 do
    FClassFields[I] := TFieldInfo.Create(Input, ConstPoolSec);
end;

{ TMethodInfo }

constructor TMethodInfo.Create(Input: TStream; ConstPoolSec: TConstPool);
var
  NameIndex, DescIndex, AttrCnt: Integer;
  Obj: TConstBase;
begin
  FAccessFlags := ReadU2(Input);
  NameIndex := ReadU2(Input);
  DescIndex := ReadU2(Input);
  AttrCnt := ReadU2(Input);

  Obj := ConstPoolSec.ConstPoolElem(NameIndex);
  if Assigned(Obj) and (Obj is TConstUtf8) then
    FName := Obj as TConstUtf8;

  Obj := ConstPoolSec.ConstPoolElem(DescIndex);
  if Assigned(Obj) and (Obj is TConstUtf8) then
    FDescriptor := Obj as TConstUtf8;

  if AttrCnt > 0 then
  begin
    SetLength(FAttributes, AttrCnt);
    for var I := 0 to AttrCnt - 1 do
      FAttributes[I] := TAttrFactory.AllocAttr(Input);
  end;
end;

function TMethodInfo.IsConstructor: Boolean;
begin
  Result := (FName.GetString = '<init>');
end;

{ TClassMethodsec }

constructor TClassMethodsec.Create(Input: TStream; ConstPoolSec: TConstPool);
var
  MethodCnt: Integer;
begin
  MethodCnt := ReadU2(Input);
  if MethodCnt > 0 then
    SetLength(FClassMethods, MethodCnt);
  for var I := 0 to MethodCnt - 1 do
    FClassMethods[I] := TMethodInfo.Create(Input, ConstPoolSec);
end;

destructor TClassMethodsec.Destroy;
begin
  for var I := 0 to High(FClassMethods) do
    FreeAndNil(FClassMethods[I]);
  inherited;
end;

{ TClassAttrsec }

constructor TClassAttrsec.Create(Input: TStream);
var
  NumAttr: Integer;
begin
  NumAttr := ReadU2(Input);
  if NumAttr > 0 then
  begin
    SetLength(FClassAttrTab, NumAttr);
    for var I := 0 to NumAttr - 1 do
      FClassAttrTab[I] := TAttrFactory.AllocAttr(Input);
  end;
end;

{ TClassFile }

constructor TClassFile.Create(Input: TStream);
begin
  try
    FHeader := TClassFileHeader.Create(Input);
    if FHeader.FMagic = $CAFEBABE then
    begin
      FClassConstPool := TConstPool.Create(Input);
      FClassDecl := TClassDeclSec.Create(Input, FClassConstPool);
      FClassFields := TClassFieldsec.Create(Input, FClassConstPool);
      FClassname := FClassDecl.GetClassName;
      FClassMethods := TClassMethodsec.Create(Input, FClassConstPool);
      FClassAttrs := TClassAttrsec.Create(Input);
    end
    else
      FreeAndNil(FHeader);
  finally
    FreeAndNil(Input);
  end;
end;

destructor TClassFile.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FClassConstPool);
  FreeAndNil(FClassDecl);
  FreeAndNil(FClassFields);
  FreeAndNil(FClassMethods);
  FreeAndNil(FClassAttrs);
  inherited;
end;

{ TAttrInfo }

constructor TAttrInfo.Create(const Name: string; Length: Integer);
begin
  FAttrName := Name;
  FLen := Length;
end;

function TAttrInfo.GetName: string;
begin
  Result := FAttrName;
end;

{ TAttrFactory }

class function TAttrFactory.AllocAttr(Input: TStream)
  : TAttrInfo;
var
  Length: Integer;
  RetObj: TAttrInfo;
begin
  RetObj := nil;

  ReadU2(Input);
  Length := ReadU4(Input);

  // Skip all Attributes
  SkipData(Length, Input);

  Result := RetObj;
end;

class procedure TAttrFactory.SkipData(FLen: Integer; Input: TStream);
begin
  if (Input.Position >= Input.Size) and (FLen <> 0) then
    raise Exception.Create('Unexpected end of file');
  Input.Position := Input.Position + FLen;
end;

{ TConstBase }

function TConstBase.GetString: string;
begin
  Result := '**noname';
end;

procedure TConstBase.SetRef(const ObjAry: array of TConstBase);
begin
  // nothing
end;

{ TConstPool }

function TConstPool.AllocConstEntry(FTag: Integer): TConstBase;
begin
  Result := nil;
  case FTag of
    CONSTANT_Utf8:
      Result := TConstUtf8.Create;
    CONSTANT_Integer:
      Result := TConstInt.Create;
    CONSTANT_Float:
      Result := TConstFloat.Create;
    CONSTANT_Long:
      Result := TConstLong.Create;
    CONSTANT_Double:
      Result := TConstDouble.Create;
    CONSTANT_Class, CONSTANT_String:
      Result := TConstClassOrString.Create;
    CONSTANT_Fieldref, CONSTANT_Methodref, CONSTANT_InterfaceMethodref:
      Result := TConstRef.Create;
    CONSTANT_NameAndType:
      Result := TConstNameAndTypeInfo.Create;
  else
    ErrorMsg('allocConstEntry: bad FTag value = ' + IntToStr(FTag));
  end;

  if Assigned(Result) then
    Result.FTag := FTag;
end;

function TConstPool.ConstPoolElem(FIndex: Integer): TConstBase;
begin
  Result := nil;
  if (FIndex > 0) and (FIndex < Length(FConstPool)) then
    Result := FConstPool[FIndex];
end;

constructor TConstPool.Create(Input: TStream);
begin
  FConstPoolCnt := ReadU2(Input);

  SetLength(FConstPool, FConstPoolCnt);

  ReadConstPool(Input);
  ResolveConstPool;
end;

destructor TConstPool.Destroy;
begin
  for var I := 0 to High(FConstPool) do
    FreeAndNil(FConstPool[I]);
  inherited;
end;

procedure TConstPool.ReadConstPool(Input: TStream);
var
  Int, FTag: Integer;
  ConstObj: TConstBase;
begin
  Int := 1;
  while Int < FConstPoolCnt do
  begin
    FTag := ReadU1(Input);
    if FTag > 0 then
    begin
      ConstObj := AllocConstEntry(FTag);
      ConstObj.Read(Input);
      FConstPool[Int] := ConstObj;
      if (ConstObj is TConstLong) or (ConstObj is TConstDouble) then
      begin
        Inc(Int);
        FConstPool[Int] := nil;
      end;
    end;
    Inc(Int);
  end;
end;

procedure TConstPool.ResolveConstPool;
begin
  // FIndex 0 is not used
  for var I := 1 to FConstPoolCnt - 1 do
    if Assigned(FConstPool[I]) then
      FConstPool[I].SetRef(FConstPool);
end;

{ TConstClassOrString }

function TConstClassOrString.GetString: string;
begin
  Result := FUtf8.GetString;
end;

procedure TConstClassOrString.Read(Input: TStream);
begin
  FIndex := ReadU2(Input);
end;

procedure TConstClassOrString.SetRef(const ObjAry: array of TConstBase);
var
  Tmp: TConstBase;
begin
  Tmp := ObjAry[FIndex];
  if Tmp is TConstUtf8 then
    FUtf8 := Tmp as TConstUtf8;
end;

{ TConstLongConvert }

function TConstLongConvert.ReadLong(Input: TStream): Int64;
var
  High, Low: Integer;
begin
  High := ReadU4(Input);
  Low := ReadU4(Input);
  Result := ToLong(High, Low);
end;

function TConstLongConvert.ToLong(High, Low: Integer): Int64;
begin
  Result := (High shl 32) or Low;
end;

{ TConstDouble }

procedure TConstDouble.Read(Input: TStream);
var
  Int: Int64;
begin
  // Is this cast ok?
  Int := ReadLong(Input);
  Move(Int, FDo, SizeOf(FDo));
end;

{ TConstFloat }

procedure TConstFloat.Read(Input: TStream);
var
  Lon: LongWord;
begin
  Lon := ReadU4(Input);
  // Is this cast ok?
  Move(Lon, FSin, SizeOf(FSin));
end;

{ TConstInt }

procedure TConstInt.Read(Input: TStream);
var
  Lon: LongWord;
begin
  Lon := ReadU4(Input);
  FVal := Lon;
end;

{ TConstLong }

procedure TConstLong.Read(Input: TStream);
begin
  FLongVal := ReadLong(Input);
end;

{ TConstNameAndTypeInfo }

procedure TConstNameAndTypeInfo.Read(Input: TStream);
begin
  inherited Read(Input);
  FDescriptorIndex := ReadU2(Input);
end;

procedure TConstNameAndTypeInfo.SetRef(const ObjAry: array of TConstBase);
var
  Tmp: TConstBase;
begin
  inherited SetRef(ObjAry);
  Tmp := ObjAry[FDescriptorIndex];
  if Tmp is TConstUtf8 then
    FDescriptorUtf8 := Tmp as TConstUtf8;
end;

{ TConstRef }

procedure TConstRef.Read(Input: TStream);
begin
  FIndex := ReadU2(Input);
  FNameAndTypeIndex := ReadU2(Input);
end;

procedure TConstRef.SetRef(const ObjAry: array of TConstBase);
var
  Tmp: TConstBase;
begin
  Tmp := ObjAry[FIndex];
  if Tmp is TConstClassOrString then
    FClassRef := Tmp as TConstClassOrString;

  Tmp := ObjAry[FNameAndTypeIndex];
  if Tmp is TConstNameAndTypeInfo then
    FNameRef := Tmp as TConstNameAndTypeInfo;
end;

{ TConstUtf8 }

procedure TConstUtf8.Read(Input: TStream);
var
  OneChar: Word;
  FLen, CharCnt: Integer;
  OneByte, FirstByte: Byte;
  Tmp: Word;
begin
  FLen := ReadU2(Input);

  CharCnt := 0;
  while CharCnt < FLen do
  begin
    OneByte := ReadU1(Input);
    Inc(CharCnt);
    if (OneByte shr 7) = 1 then
    begin
      Tmp := (OneByte and $3F); // Bits 5..0 (six bits)
      FirstByte := OneByte;
      OneByte := ReadU1(Input);
      Inc(CharCnt);
      Tmp := (Tmp or ((OneByte and $3F) shl 6));
      if (FirstByte shr 4) = 2 + 4 + 8 then
      begin
        OneByte := ReadU1(Input);
        Inc(CharCnt);
        OneByte := (OneByte and $0F);
        Tmp := (Tmp or (OneByte shl 12));
      end;
      OneChar := Tmp;
    end
    else
      OneChar := OneByte;
    FStr := FStr + Char(Lo(OneChar));
  end;
end;

function TConstUtf8.GetString: string;
begin
  Result := FStr;
end;

{ TAccData }

class function TAccData.IsAbstract(Val: Integer): Boolean;
begin
  Result := (Val and ACC_ABSTRACT) <> 0;
end;

class function TAccData.IsFinal(Val: Integer): Boolean;
begin
  Result := (Val and ACC_FINAL) <> 0;
end;

class function TAccData.IsInterface(Val: Integer): Boolean;
begin
  Result := (Val and ACC_INTERFACE) <> 0;
end;

class function TAccData.IsNative(Val: Integer): Boolean;
begin
  Result := (Val and ACC_NATIVE) <> 0;
end;

class function TAccData.IsPrivate(Val: Integer): Boolean;
begin
  Result := (Val and ACC_PRIVATE) <> 0;
end;

class function TAccData.IsProtected(Val: Integer): Boolean;
begin
  Result := (Val and ACC_PROTECTED) <> 0;
end;

class function TAccData.IsPublic(Val: Integer): Boolean;
begin
  Result := (Val and ACC_PUBLIC) <> 0;
end;

class function TAccData.IsStatic(Val: Integer): Boolean;
begin
  Result := (Val and ACC_STATIC) <> 0;
end;

class function TAccData.IsStrict(Val: Integer): Boolean;
begin
  Result := (Val and ACC_STRICT) <> 0;
end;

class function TAccData.IsSuper(Val: Integer): Boolean;
begin
  Result := (Val and ACC_SYNC) <> 0; // sync and super share the same bit-flag
end;

class function TAccData.IsSync(Val: Integer): Boolean;
begin
  Result := (Val and ACC_SYNC) <> 0;
end;

class function TAccData.IsTransient(Val: Integer): Boolean;
begin
  Result := (Val and ACC_TRANSIENT) <> 0;
end;

class function TAccData.IsVolatile(Val: Integer): Boolean;
begin
  Result := (Val and ACC_VOLATILE) <> 0;
end;

{ TObjNameFormat }

class function TObjNameFormat.ToDotSeparator(const SlashName: string): string;
var
  Chr: Char;
begin
  Result := '';
  for var I := 1 to Length(SlashName) do
  begin
    Chr := SlashName[I];
    if Chr = '/' then
      Result := Result + '.'
    else if Chr <> ';' then
      Result := Result + Chr;
  end;
end;

end.
