unit UComJava2;

interface

uses Windows, Messages, Classes,
     JNI, JavaRuntime, UJniWrapper2, UThreads;

type

   TPipeHandles = record
     hRead, hWrite: THandle;
   end;

   TComJava2 = class
    private
      InputPipe: TPipeHandles;
      OutputPipe : TPipeHandles;
      ExecThreadTerminated: Boolean;
      aJavaCompiler: TJavaCompiler;
      aJavaRuntime: TJavaRuntime;
      ErrorMessage: string;
      HideThread: THideThread;
      hWndFJava: long;
    public
      ClassList: TStringList;
      ObjectList: TStringList;
      JavaFolder: string;
      Classpath: string;
      ConnectionAddress: string;
      ObjectLowerCaseLetter: string;
      LogfileExceptions: string;
      LogfileExceptionsOK: string;
      Version: string;
      ShowInheritedPrivateAttributes: string;
      LNGFileNotFound: string;
      LNGUnknownObject: string;
      LNGUnknownClass: string;
      LNGIncompatibelTypes: string;

      constructor Create;
      destructor Destroy; override;
      procedure SendMessageSynchron(const Str: string);
      procedure ClosePipeHandles(var Pipe: TPipeHandles);
      function ExecuteCommand(const cmd: string): string;
      procedure ShowInMemo(const Str: string);
      procedure processMessage(const command: string);
      procedure Close;

      // class methods
      function LoadClass(const AClassname, Pathname: string): string;
      function createClass(const AClassname: string): string;
      function getClass(const AClassname: string): TJavaClass;
      function ClassToJavaClass(aClass: jClass): TJavaClass;
      function hasSuperClass(const AClassname: string): string;
      function getSuperClass(const AClassname: string): string;
      function isInterface(const AClassname: string): string;
      function findClass(const AClassname: string): string;
      function GetAttributes(const AClassname: string): string;
      function getAttributeNames(const AClassname: string): string;
      function getRefreshedAttributeNames(const AClassname: string): string;
      function getMethods(const AClassname, _Static: string): string;
      function getConstructors(const AClassname: string): string;
      function getNewObjectName(const AClassname: string): string;
      function getNewClassname: string;
      function getPackage(const AClassname: string): string;

      // object methods
      function CreateObject(const AClassname, Objectname, Signature, Params: string): string;
      function GetObject(const Objectname: string): TJavaObject;
      function DeleteObject(const Objectname: string): string;

      function ToString(const Objectname: string): string; reintroduce;
      function getClassOf(const Objectname: string): string;
      function getAttributeValues(const Objectname, ArrayList: string): string;
      function DebugGetAttributeValues(const Objectname: string): string;
      function getObjectAttributeValues(const Objectname, Withtype, ArrayList: string): string;
      function getObjectAttributeNames(const Objectname: string): string;
      function getExpressionValue(const AClassname: string): string;
      function callMethod(const AClassname, Objectname, aMethodname, Returntype, Signature, Params, Methodtype: string): string;

      function getVariables(): string;
      function getAttributeValue(const AClassname, Objectname, Fieldname, Sig, IsStatic: string): string;
      function setAttributeValue(const AClassname, Objectname, Fieldname, Sig, value, IsStatic: string): string;

      procedure InitConsole;
      procedure IgnoreConsole;
      function ReadConsole: string;
      procedure HideConsole;
      procedure WriteToJava(const Str: ANSIString);

      function IsPackageClass(const AClassname: string): Boolean;
      function GetSignature(const Typ: string): string;
      function cookClass(const Pathname: string): string;
      function compile(const Parameter, Pathname: string): string;
      function setParam(const Nr, Param: string): string;
      function init: string;
      function logMemo(const Pathname, aVersion: string): string;
      function ShowHide: string;
      procedure ExecThreadOnTerminate(Sender: TObject);
      procedure logclasslist;
   end;

var myComJava2: TComJava2;

implementation

Uses SysUtils, Forms, Dialogs, Graphics, Contnrs, ExtCtrls,
     UUtils, UJe2Java;

constructor TComJava2.Create;
begin
  ObjectList:= TStringList.Create;
  ObjectList.CaseSensitive:= True;
  ClassList := TStringList.Create;
  ClassList.CaseSensitive:= True;
  InitConsole;
end;

procedure TComJava2.Close;
  var aObject: TObject;
begin
  ExecThreadTerminated:= True;
  for var Int:= ClassList.Count - 1 downto 0 do begin
    aObject:= TJavaClass(ClassList.Objects[Int]);
    FreeAndNil(aObject);
  end;
  ClassList.Clear;
  for var Int:= ObjectList.Count - 1 downto 0 do begin
    aObject:= TObject(ObjectList.Objects[Int]);
    FreeAndNil(aObject);
  end;
  ObjectList.Clear;
end;

destructor TComJava2.Destroy;
begin
  FreeAndNil(ClassList);
  FreeAndNil(ObjectList);
  FreeAndNil(aJavaCompiler);
  FreeAndNil(aJavaRuntime);
  FreeAndNil(HideThread);
  ClosePipeHandles(OutputPipe);
  ClosePipeHandles(InputPipe);
end;

function TComJava2.ExecuteCommand(const cmd: string): string;
begin
  Result:= '-ERR';
  var SL:= Split(#4, cmd);
  try
    try
      if SL[0] = 'loadClass' then
        Result:= LoadClass(SL[1], SL[2])
      else if SL[0] = 'createClass' then
        Result:= createClass(SL[1])
      else if SL[0] = 'createObject' then
        Result:= createObject(SL[1], SL[2], SL[3], SL[4])
      else if SL[0] = 'getAttributes' then
        Result:= GetAttributes(SL[1])
      else if SL[0] = 'getAttributeNames' then
        Result:= getAttributeNames(SL[1])
      else if SL[0] = 'getMethods' then
        Result:= getMethods(SL[1], SL[2])
      else if SL[0] = 'getConstructors' then
        Result:= getConstructors(SL[1])
      else if SL[0] = 'getAttributeValues' then
        Result:= getAttributeValues(SL[1], SL[2])
      else if SL[0] = 'getObjectAttributeValues' then
        Result:= getObjectAttributeValues(SL[1], SL[2], SL[3])
      else if SL[0] = 'getObjectAttributeNames' then
        Result:= getObjectAttributeNames(SL[1])
      else if SL[0] = 'getAttributeValue' then
        Result:= getAttributeValue(SL[1], SL[2], SL[3], SL[4], SL[5])
      else if SL[0] = 'setAttributeValue' then
        Result:= setAttributeValue(SL[1], SL[2], SL[3], SL[4], SL[5], SL[6])
      else if SL[0] = 'callMethod' then
        Result:= callMethod(SL[1], SL[2], SL[3], SL[4], SL[5], SL[6], SL[7])
      else if SL[0] = 'toString' then
        Result:= ToString(SL[1])
      else if SL[0] = 'hasSuperClass' then
        Result:= hasSuperClass(SL[1])
      else if SL[0] = 'getNewObjectName' then
        Result:= getNewObjectName(SL[1])
      else if SL[0] = 'getSuperClass' then
        Result:= getSuperClass(SL[1])
      else if SL[0] = 'findClass' then
        Result:= findClass(SL[1])
      else if SL[0] = 'getClassOf' then
        Result:= getClassOf(SL[1])
      else if SL[0] = 'isInterface' then
        Result:= IsInterface(SL[1])
      else if SL[0] = 'deleteObject' then
        Result:= DeleteObject(SL[1])
      else if SL[0] = 'cookClass' then
        Result:= cookClass(SL[1])
      else if SL[0] = 'compile' then
        Result:= compile(SL[1], SL[2])
      else if SL[0] = 'getExpressionValue' then
        Result:= getExpressionValue(SL[1])
      else if SL[0] = 'getVariables' then
        Result:= getVariables()
      else if SL[0] = 'init' then
        Result:= init
      else if SL[0] = 'setParam' then
        Result:= setParam(SL[1], SL[2])
      else if SL[0] = 'logMemo' then
        Result:= logMemo(SL[1], SL[2])
      else if SL[0] = 'ShowHide' then
        Result:= ShowHide
      else if SL[0] = 'DebugGetAttributeValues' then
        Result:= DebugGetAttributeValues(SL[1])
      else if SL[0] = 'getRefreshedAttributeNames' then
        Result:= getRefreshedAttributeNames(SL[1])
      else if SL[0] = 'getPackage' then
        Result:= getPackage(SL[1])
      else if SL[0] = 'term' then
        Result:= '+OK Java terminated';
    except on e: Exception do
      Result:= '-ERR: ' + E.Message;
  end
  finally
    FreeAndNil(SL);
  end;
end;

procedure TComJava2.processMessage(const command: string);
  var Str: string;
begin
  if Pos('setParam', command) <> 1 then
    ShowInMemo('> ' + command);
  try
    Str:= ExecuteCommand(command);
  except
    on e: Exception do
      Str:= '-ERR ' + e.Message;
  end;
  if Pos('setParam', command) <> 1 then begin
    ShowInMemo('< ' + Str);
    ShowInMemo(ReadConsole);
  end;
  SendMessageSynchron(Str);
end;

procedure TComJava2.SendMessageSynchron(const Str: string);
  var cds: TCopyDataStruct;
begin
  cds.dwData:= 0; // used to identify the message contents
  cds.cbData:= Length(Str)*sizeOf(Char);
  cds.lpData:= PChar(Str);
  Windows.SendMessage(hWndFJava, WM_COPYDATA, FJe2Java.Handle, LPARAM(@cds))
end;

procedure TComJava2.InitConsole;
  const BufSize = $4000;  // of ReadBuf
  var SecAttr: TSecurityAttributes;
begin
  // doesn't work if InitConsole is only called after the files have been opened
  // presumably java remembers the I/O console
  AllocConsole;
  HideConsole;
  SecAttr.nLength:= SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor:= nil;
  SecAttr.bInheritHandle:= True;

  // Java outputs to OutputPipe.hWrite
  // ConsoleThread reads from OutputPipe.hRead and shows Java-Output in Interpreter-Window
  if CreatePipe(OutputPipe.hRead, OutputPipe.hWrite, @SecAttr, BufSize) then begin
    SetStdHandle(STD_OUTPUT_HANDLE, OutputPipe.hWrite);
    SetStdHandle(STD_ERROR_HANDLE, OutputPipe.hWrite);
  end;

  // Java reads from InputPipe.hRead
  // Interpreter-Window reads from Keyboard and sends Input via InputPipe.hWrite to Java
  if CreatePipe(InputPipe.hRead, InputPipe.hWrite, @SecAttr, BufSize) then
    SetStdHandle(STD_INPUT_HANDLE, InputPipe.hRead);
end;

procedure TComJava2.IgnoreConsole;
  var Read: Cardinal; Buffer: AnsiString;
begin
  while PeekNamedPipe(OutputPipe.hRead, nil, 0, nil, @Read, nil) and (Read > 0) do begin
    SetLength(Buffer, Read);
    ReadFile(OutputPipe.hRead, Buffer[1], Read, Read, nil);
  end;
end;

procedure TComJava2.ClosePipeHandles(var Pipe: TPipeHandles);
begin
  with Pipe do begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead:= 0;
    hWrite:= 0;
  end;
end;

procedure TComJava2.ShowInMemo(const Str: string);
begin
  FJe2Java.ShowInMemo(Str);
end;

function TComJava2.LoadClass(const AClassname, Pathname: string): string;
begin
  var aJavaClass:= TJavaClass.CreateLoadClass(AClassname, Pathname);
  try
    // if there is an exception in LoadClass then LoadClass is aborted
    // the exeception is handled in TComJava2.NewMessage
    ShowInMemo('Loadclass ausgeführt.');
    if Assigned(aJavaClass.Handle) then begin
      aJavaClass.Global:= True;
      var Int:= ClassList.IndexOf(AClassname);
      if Int = -1 then
        ClassList.AddObject(AClassname, aJavaClass);
      Result:= '+OK'
    end else
      Result:= '-ERR Could not load ' + Pathname + #13#10 + aJavaClass.Error;
  except
    on e: Exception do begin
      ShowInMemo(e.message);
      ErrorMessage:= '-ERR ' + e.message;
      Result:= ErrorMessage;
    end;
  end;
end;

function TComJava2.createClass(const AClassname: string): string;
begin
  Result:= '+OK';
  var Int:= ClassList.IndexOf(AClassname);
  if Int = -1 then begin
    var aJavaClass:= TJavaClass.Create(AClassname);
    if aJavaClass.Valid then begin
      aJavaClass.Global:= True;
      ClassList.AddObject(AClassname, aJavaClass);
      logClasslist;
    end else begin
      Result:= '-ERR ' + aJavaClass.Error;
      FreeAndNil(aJavaClass);
    end
  end;
end;

function TComJava2.getClass(const AClassname: string): TJavaClass;
begin
  var Int:= ClassList.IndexOf(AClassname);
  if Int = -1 then begin
    ErrorMessage:= CreateClass(AClassname);
    Int:= ClassList.IndexOf(AClassname);
  end;
  if Int = -1
    then Result:= nil
    else Result:= TJavaClass(ClassList.Objects[Int]);
end;

function TComJava2.ClassToJavaClass(aClass: jClass): TJavaClass;
begin
  var aJavaClass:= TJavaClass.Create(aClass);
  if aJavaClass.Valid then begin
    var Int:= ClassList.IndexOf(aJavaClass.Name);
    if Int = -1 then begin
      aJavaClass.Global:= True;
      ClassList.AddObject(aJavaClass.Name, aJavaClass);
      logClasslist;
      Result:= aJavaClass;
    end else
      Result:= TJavaClass(ClassList.Objects[Int])
  end else begin
    FreeAndNil(aJavaClass);
    Result:= nil;
  end;
end;

function TComJava2.getObject(const Objectname: string): TJavaObject;
begin
  var Int:= ObjectList.IndexOf(Objectname);
  if Int = -1
    then Result:= nil
    else Result:= TJavaObject(ObjectList.Objects[Int]);
end;

function TComJava2.getAttributeValue(const AClassname, Objectname, Fieldname, Sig, IsStatic: string): string;
  var myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      myClass: TJavaClass; myObject: TJavaObject;
begin
  myClass:= GetClass(AClassname);
  myObject:= GetObject(Objectname);
  myAttribute:= TJavaAttribute.Create(myClass);
  try
    if IsStatic = 's'
      then aJavaValue:= myAttribute.GetStaticAttributeValue(Fieldname, Sig)
      else aJavaValue:= myAttribute.GetAttributeValue(myObject, Fieldname, Sig);
    if Assigned(aJavaValue)
      then Result:= '+OK ' + aJavaValue.AsString
      else Result:= '-ERR Unknown attribute ' + AClassname + '.' + Fieldname;
  finally
    FreeAndNil(myAttribute);
    FreeAndNil(aJavaValue);
  end;
end;

function TComJava2.setAttributeValue(const AClassname, Objectname, Fieldname, Sig, value, IsStatic: string): string;
  var myAttribute: TJavaAttribute; myJavaValue: TJavaValue;
      myClass: TJavaClass; myObject: TJavaObject;
begin
  myClass:= getClass(AClassname);
  myObject:= getObject(Objectname);
  myAttribute:= TJavaAttribute.Create(myClass);
  myJavaValue:= TJavaValue.Create(Value, Sig);
  try
    if IsStatic = 's'
      then myAttribute.SetStaticAttributeValue(Fieldname, Sig, myJavaValue.Value)
      else myAttribute.SetAttributeValue(myObject, Fieldname, Sig, myJavaValue.Value);
  finally
    FreeAndNil(myAttribute);
    FreeAndNil(myJavaValue);
  end;
  Result:= '+OK';
end;

function TComJava2.ToString(const Objectname: string): string;
  var myObject: TJavaObject;
begin
  myObject:= GetObject(Objectname);
  if Assigned(myObject)
    then Result:= '+OK ' + myObject.ToString
    else Result:= '-ERR';
end;

function TComJava2.hasSuperClass(const AClassname: string): string;
begin
  var myClass:= getClass(AClassname);
  if myClass = nil then
    TJavaClass.Create(AClassname);
  if myClass.Valid and myClass.hasSuperClass
    then Result:= '+OK '
    else Result:= '-NO ' + MyClass.Error;
end;

function TComJava2.getSuperClass(const AClassname: string): string;
begin
  var myClass:= getClass(AClassname);
  if Assigned(myClass)
    then Result:= '+OK ' + myClass.getSuperclassName
    else Result:= TJavaClass.getSuperClassFromName(AClassname);
end;

function TComJava2.findClass(const AClassname: string): string;
begin
  Result:= TJavaClass.FindClass(AClassname);
end;

function TComJava2.getClassOf(const Objectname: string): string;
begin
  var myObject:= getObject(Objectname);
  if Assigned(myObject) then begin
    var Str:= myObject.ClassRef.Name;  // Typ
    if Str = '' then begin
      Str:= getNewClassname;
      ClassList.AddObject(Str, myObject.ClassRef);
      logClasslist
    end;
    Result:= '+OK ' + Str + '|' + myObject.ToString;
  end else
    Result:= '-ERR Unknown object "' + Objectname + '"';
end;

function TComJava2.IsInterface(const AClassname: string): string;
begin
  Result:= TJavaClass.IsInterface(AClassname);
end;

function TComJava2.getMethods(const AClassname, _Static: string): string;
begin
  var aJavaClass:= getClass(AClassname);
  if not Assigned(aJavaClass) then
    aJavaClass:= TJavaClass.Create(AClassname); // maybe a system class
  if aJavaClass.Valid
    then Result:= aJavaClass.getMethods(_Static)
    else Result:= '-ERR ' + ReadConsole;
end;

function TComJava2.getConstructors(const AClassname: string): string;
begin
  var aJavaClass:= getClass(AClassname);
  if not Assigned(aJavaClass) then
    aJavaClass:= TJavaClass.Create(AClassname); // maybe a system class
  if aJavaClass.Valid
    then Result:= aJavaClass.getConstructors
    else Result:= '-ERR ' + ReadConsole;
end;

function TComJava2.GetAttributes(const AClassname: string): string;
begin
  var aJavaClass:= getClass(AClassname);
  if Assigned(aJavaClass)
    then Result:= '+OK ' + aJavaClass.GetAttributes
    else Result:= '-ERR ' + 'Unknown class: ' + AClassname;
end;

function TComJava2.getAttributeNames(const AClassname: string): string;
begin
  var aJavaClass:= getClass(AClassname);
  if Assigned(aJavaClass)
    then Result:= '+OK ' + aJavaClass.getAttributeNames
    else Result:= '-ERR ' + 'Unknown class: ' + AClassname;
end;

function TComJava2.getRefreshedAttributeNames(const AClassname: string): string;
begin
  var aJavaClass:= getClass(AClassname);
  if Assigned(aJavaClass)
    then Result:= '+OK ' + aJavaClass.getRefreshedAttributeNames
    else Result:= '-ERR ' + 'Unknown class: ' + AClassname;
end;

function TComJava2.getAttributeValues(const Objectname, ArrayList: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      Int: Integer; Str: string; AttributesList: TObjectList;

  function ArrayListToList(Str: string): string;
  begin
    Str:= getObjectAttributeValues(Str, '', ArrayList);
    Str:= Copy(Str, 7, Length(Str) - 7);
    var SL:= Split(',', Str);
    Str:= '[';
    for var j:= 0 to SL.Count - 1 do begin
      var Str1:= Trim(SL[j]);
      if (Str1 <> '') and (Str1 <> 'null') then
        Str:= Str + Str1 + ', ';
    end;
    if Str = '['
      then Str:= '[]'
      else Str:= Left(Str, Length(Str) - 2) + ']';
    FreeAndNil(SL);
    Result:= Str;
  end;

begin
  myObject:= getObject(Objectname);
  if Assigned(myObject) then
    if Assigned(myObject.ClassRef) then begin
      if not Assigned(myObject.ClassRef.AttributesList) then
        myObject.GetAttributes;
      AttributesList:= myObject.ClassRef.AttributesList;
      Str:= '';
      for Int:= 0 to AttributesList.Count - 1 do begin
        myAttribute:= TJavaAttribute(AttributesList.Items[Int]);
        if myAttribute.Static_
          then aJavaValue:= myAttribute.GetStaticAttributeValue()
          else aJavaValue:= myAttribute.GetAttributeValue(myObject);
        if Assigned(aJavaValue) then
          if (ArrayList = 'ArrayList') and (myAttribute.Typ = 'java.util.ArrayList')
            then Str:= Str + #4 + ArrayListToList(aJavaValue.AsString)
            else Str:= Str + #4 + aJavaValue.AsString
          else Str:= Str + #4'<error>';
        FreeAndNil(aJavaValue);
      end;
      Result:= '+OK ' + Str + #4 + myObject.ToString;
    end else
      Result:= '-ERR unknown class for object ' + Objectname
  else
    Result:= '-ERR unknown object ' + Objectname
end;

function TComJava2.DebugGetAttributeValues(const Objectname: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      Int: Integer; Str: string; AttributesList: TObjectList;
begin
  myObject:= getObject(Objectname);
  if Assigned(myObject) then
    if Assigned(myObject.ClassRef) then begin
      if Assigned(myObject.ClassRef.AttributesList) then begin
        myObject.ClassRef.AttributesList.Free;
        myObject.ClassRef.AttributesList:= nil;
        myObject.GetAttributes;
      end;
      AttributesList:= myObject.ClassRef.AttributesList;
      Str:= '';
      for Int:= 0 to AttributesList.Count - 1 do begin
        myAttribute:= TJavaAttribute(AttributesList.Items[Int]);
        if myAttribute.Static_
          then aJavaValue:= myAttribute.GetStaticAttributeValue()
          else aJavaValue:= myAttribute.GetAttributeValue(myObject);
        if Assigned(aJavaValue)
          then Str:= Str + #4 + myAttribute.Name + '=' + aJavaValue.AsString
          else Str:= Str + #4 + myAttribute.Name + '=' + '<error>';
        FreeAndNil(aJavaValue);
      end;
      Result:= '+OK ' + 'Debug ' + Objectname + ':  ' + Str;
    end else
      Result:= '-ERR unknown class for object ' + Objectname
  else
    Result:= '-ERR unknown object ' + Objectname
end;

function TComJava2.getObjectAttributeValues(const Objectname, WithType, ArrayList: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      Int, j: Integer; Str, Str1, Str2: string; SL: TStringList; AttributesList: TObjectList;

  procedure AddObject(const obj: string);
  begin
    var aJavaObject:= getObject(obj);
    if Assigned(aJavaObject) and (WithType = 'withtype')
      then Str:= Str + #4 + obj + '=' + aJavaObject.Classref.ImportTyp
      else Str:= Str + #4 + obj;
  end;

  procedure ArrayListToList(obj: string);
    var Str1, Str2: string;
  begin
    Str2:= '[';
    obj:= getObjectAttributeValues(obj, '', ArrayList);
    obj:= Copy(obj, 7, Length(obj) - 7);
    SL:= Split(',', obj);
    for var j:= 0 to SL.Count - 1 do begin
      Str1:= Trim(SL[j]);
      if (Str1 <> '') and (Str1 <> 'null') then begin
        var aJavaObject:= getObject(Str1);
        if Assigned(aJavaObject) and (WithType = 'withtype')
          then Str2:= Str2 + Str1 + '=' + aJavaObject.Classref.ImportTyp + ', '
          else Str2:= Str2 + Str1 + ', ';
      end;
    end;
    if Str2 = '['
      then Str2:= '[]'
      else Str2:= Left(Str2, Length(Str2) - 2) + ']';
    Str:= Str + #4 + Str2;
    FreeAndNil(SL);
  end;

begin
  Str:= '';
  myObject:= GetObject(Objectname);
  if Assigned(myObject) and Assigned(myObject.ClassRef) then begin
    if not Assigned(myObject.ClassRef.AttributesList) then
      myObject.GetAttributes;
    AttributesList:= myObject.ClassRef.AttributesList;
    for Int:= 0 to AttributesList.Count - 1 do begin
      myAttribute:= TJavaAttribute(AttributesList.Items[Int]);
      if myAttribute.isObject then begin
        if myAttribute.Static_
          then aJavaValue:= myAttribute.GetStaticAttributeValue()
          else aJavaValue:= myAttribute.GetAttributeValue(myObject);
        if Assigned(aJavaValue) then begin
          Str1:= aJavaValue.AsString;
          if (Pos('{', Str1) = 1) and (Withtype = 'withtype') then begin   // array
            Str1:= Copy(Str1, 2, Length(Str1) - 2);
            SL:= Split(',', Str1);
            for j:= 0 to SL.Count - 1 do begin
              Str2:= Trim(SL[j]);
              AddObject(Str2);
            end;
            FreeAndNil(SL);
          end else if (ArrayList = 'ArrayList') and (myAttribute.Typ = 'java.util.ArrayList') then
            ArrayListToList(Str1)
          else
            AddObject(Str1);
        end else
          AddObject('<error>');
        FreeAndNil(aJavaValue);
      end;
    end;
    Result:= '+OK ' + Str;
  end else
    Result:= '-ERR No object or no class';
end;

function TComJava2.getObjectAttributeNames(const Objectname: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute;
      Int: Integer; Str: string; AttributesList: TObjectList;
begin
  Str:= '';
  myObject:= GetObject(Objectname);
  if Assigned(myObject) and Assigned(myObject.ClassRef) then begin
    if not Assigned(myObject.ClassRef.AttributesList) then
      myObject.GetAttributes;
    AttributesList:= myObject.ClassRef.AttributesList;
    for Int:= 0 to AttributesList.Count - 1 do begin
      myAttribute:= TJavaAttribute(AttributesList.Items[Int]);
      if myAttribute.isObject then
        Str:= Str + #4 + myAttribute.Name;
    end;
    Result:= '+OK ' + Str;
  end else
    Result:= '-ERR No object or no class';
end;

function TComJava2.ReadConsole: string;
  var Read: Cardinal; Buffer: AnsiString;
begin
  Result:= '';
  while PeekNamedPipe(OutputPipe.hRead, nil, 0, nil, @Read, nil) and (Read > 0) do begin
    SetLength(Buffer, Read);
    ReadFile(OutputPipe.hRead, Buffer[1], Read, Read, nil);
    Result:= Result + string(Buffer);
  end;
end;

procedure TComJava2.HideConsole;
begin
  HideThread:= THideThread.Create;
end;

procedure TComJava2.WriteToJava(const Str: ANSIString);
  var Int: cardinal;
begin
  WriteFile(InputPipe.hWrite, Str[1], Length(Str), Int, nil);
end;

function TComJava2.CreateObject(const AClassname, Objectname, Signature, Params: string): string;
  var ConsoleThread: TConsoleThread; ExecThread: TExecThread;
      aJavaObject: TJavaObject; myClass: TJavaClass; theParams: TJavaParams;
      Error, aSignature: string;
begin
  aJavaObject:= nil;
  ConsoleThread:= nil;
  myClass:= getClass(AClassname);
  aSignature:= Copy(Signature, 2, Length(Signature)-3);
  theParams:= TJavaParams.CreateMakeParams(aSignature, Params);
  if theParams.IsValid then begin
    try
      try
        ConsoleThread:= TConsoleThread.Create(OutputPipe.hRead);
        ExecThread:= TExecThread.Create(myComJava2.JavaFolder);
        ExecThread.SetCreateObject(myClass, theParams); // Params;
        ExecThread.Start;
        ExecThread.WaitFor;
        aJavaObject:= ExecThread.aJavaObject;
        if aJavaObject.Error = '' then begin
          if ObjectList.IndexOf(Objectname) < 0 then begin
            ObjectList.AddObject(Objectname, aJavaObject);  // store of the java-objects
            aJavaObject.AddToProperties(Objectname);
          end;
        end else begin
          Error:= aJavaObject.Error;
          FreeAndNil(aJavaObject);
        end;
      except
        on e: Exception do
          Error:= e.Message;
      end;
    finally
      FreeAndNil(ExecThread);
      if Assigned(ConsoleThread) then begin
        ConsoleThread.Terminate;
        ConsoleThread.WaitFor;
        FreeAndNil(ConsoleThread);
      end;
      FreeAndNil(theParams);
    end;
  end else
    Error:= theParams.Error;
  if Assigned(aJavaObject)
    then Result:= '+OK ' + aJavaObject.ToString
    else Result:= '-ERR ' + Error + #13#10 + ReadConsole;
end;

function TComJava2.callMethod(const AClassname, Objectname, aMethodname, Returntype, Signature, Params, Methodtype: string): string;
  var ConsoleThread: TConsoleThread; ExecThread: TExecThread;
      aJavaObject: TJavaObject; theParams: TJavaParams; myClass: TJavaClass;
      aJavaTyp: TNumType; aJavaMethod: TJavaMethod; aJavaValue: TJavaValue;
      Posi: Integer; aReturnType: string;
begin
  Result:= '-ERR';
  myClass:= getClass(AClassname);
  if myClass = nil then begin
    Result:= '-ERR Unknown class: ' + AClassname;
    Exit;
  end;
  aJavaObject:= getObject(Objectname);
  if (aJavaObject = nil) and (Objectname <> '') then begin
    Result:= '-ERR Unknown object: ' + Objectname;
    Exit;
  end;
  aJavaTyp:= TypToNumType(ReturnType);
  // check, if Returntype exists, due to generic problem
  if (aJavaTyp in [ntObject, ntObjectArray, ntObjectObjectArray]) and (getClass(ReturnType) = nil)
    then aReturnType:= 'java.lang.Object'
    else aReturnType:= ReturnType;
  try
    Posi:= Pos(')', Signature);
    theParams:= TJavaParams.CreateMakeParams(Copy(Signature, 2, Posi-2), Params);
    if theParams.IsValid then begin
      aJavaMethod:= TJavaMethod.Create(myClass, aMethodName, TMethodAttribute(StrToInt(Methodtype)),
                                       aJavaTyp, theParams, aReturnType);
      try
        if aJavaMethod.isValid then begin
          ConsoleThread:= TConsoleThread.Create(OutputPipe.hRead);
          try
            ExecThread:= TExecThread.Create(myComJava2.JavaFolder);
            try
              ExecThread.SetMethodCall(aJavaMethod, theParams, aJavaObject, aJavaTyp);
              ExecThread.OnTerminate:= {$IFDEF LCL}@{$ENDIF} ExecThreadOnTerminate;
              ExecThreadTerminated:= False;
              ExecThread.Start;
              repeat
                Application.ProcessMessages;
                Sleep(5);
              until ExecThreadTerminated;
              aJavaValue:= ExecThread.aJavaValue;
              if aJavaValue.Error = ''
                then Result:= '+OK '  + aJavaValue.AsString
                else Result:= '-ERR ' + string(ConsoleThread.Buffer) + ConsoleThread.aException + aJavaValue.Error;
              aJavaValue.Free;
            finally
              FreeAndNil(ExecThread);
            end;
            for Posi:= 0 to 9 do begin
              sleep(10);
              Application.ProcessMessages;
            end;
            ConsoleThread.Terminate;
            ConsoleThread.WaitFor;
          finally
            FreeAndNil(ConsoleThread);
          end;
        end else
          Result:= '-ERR ' + aJavaMethod.Error;
      finally
        FreeAndNil(aJavaMethod);
      end;
    end else
      Result:= '-ERR ' + theParams.Error;
  finally
    FreeAndNil(theParams);
  end;
end;

function TComJava2.getExpressionValue(const AClassname: string): string;
  var Str, Objectname, ExprType: string; 
      myAttribute: TJavaAttribute;
      aJavaValue: TJavaValue;
      myObject: TJavaObject; SL: TStringList;
begin
  Str:= callMethod(AClassname, '', 'run', 'java.lang.Object', '()Ljava/lang/Object;', '', '0');
  if Left(Str, 3) = '+OK' then begin
    Objectname:= Right(Str, 5);
    myObject:= GetObject(Objectname);
    Str:= GetAttributes(myObject.ClassRef.Name);
    // public int Shell$7.result;
    if Left(Str, 3) = '+OK' then begin
      Str:= Right(Str, 5);
      SL:= Split('|', Str);
      ExprType:= SL[4];
      SL.Free;
      //myObject:= GetObject(Objectname);
      myAttribute:= TJavaAttribute.Create(myObject.ClassRef);
      aJavaValue:= myAttribute.GetAttributeValue(myObject, 'result', TypToSig(ExprType));
      try
        if Assigned(aJavaValue)
          then Result:= '+OK ' + aJavaValue.AsString  + '_|_' + ExprType
          else Result:= '-ERR Unknown attribute ' + AClassname + '.result';
      finally
        FreeAndNil(myAttribute);
        FreeAndNil(aJavaValue);
      end;
      DeleteObject(Objectname);
    end
    else
      Result:= Str;
  end else
    Result:= Str;
end;

function TComJava2.getVariables(): string;
  var Str, Str2, Str3, obj, Name, Value: string; Int, Posi, Posi1: Integer;
      aJavaClass : TJavaClass;
      aJavaMethod: TJavaMethod;
      aJValue: JValue;
      SystemProperties: TJavaObject;
      aJavaObject: TJavaObject;
begin
  // System.getProperties().list(System.out);
  // SystemProperties = java.lang.System.getProperties();
  // s = SystemProperties.toString();
  aJavaClass:= getClass('java.lang.System');
  aJavaMethod:= TJavaMethod.Create(aJavaClass, 'getProperties', static, ntObject, nil, 'Ljava/util/Properties;');
  try
    aJValue:= aJavaMethod.call(nil, nil);
    SystemProperties:= TJavaObject.createWithHandle(aJavaClass, aJValue.Str);
    try
      Str:= SystemProperties.ToString;
      Str2:= '';
      repeat
        Posi:= Pos('_lv', Str);
        if Posi > 0 then begin
          Delete(Str, 1, Posi+2);
          obj:= Copy(Str, 1, 2);
          Delete(Str, 1, 2);
          Posi1:= Pos('=', Str);
          Name:= Copy(Str, 1, Posi1-1);
          Posi:= Pos(', ', Str);
          Value:= Copy(Str, Posi1+1, Posi-Posi1-1);
          if obj = 'o_' then begin
            Int:= ObjectList.IndexOf(Name);
            if Int >= 0 then begin
              aJavaObject:= TJavaObject(ObjectList.Objects[Int]);
              Str3:= aJavaObject.ToString;
              if Str3 <> Value then begin  // Object changed
                aJavaObject.Destroy;
                ObjectList.Delete(Int);
                aJavaObject:= TJavaObject.CreateFromProperties(Name);
                ObjectList.AddObject(Name, aJavaObject);
              end;
              Str2:= Str2 + #4 + aJavaObject.ClassRef.Name + '|' + Name + '=' +Str3;
            end else begin
              aJavaObject:= TJavaObject.CreateFromProperties(Name);
              ObjectList.AddObject(Name, aJavaObject);
              Str2:= Str2 + #4 + aJavaObject.ClassRef.Name + '|' + Name + '=' + Value;
            end
          end else
            Str2:= Str2 + #4 + Name + '=' + Value;
        end;
      until Posi=0;
    finally
      FreeAndNil(SystemProperties);
    end;
  finally
    FreeAndNil(aJavaMethod);
  end;
  Result:= '+OK ' + Copy(Str2, 2, Length(Str2));
end;

procedure TComJava2.ExecThreadOnTerminate(Sender: TObject);
begin
  ExecThreadTerminated:= True;
end;

function TComJava2.IsPackageClass(const AClassname: string): Boolean;
begin
  Result:= (LastDelimiter('.', AClassname) > 0);
end;

function TComJava2.GetSignature(const Typ: string): string;
begin
  var aJavaClass:= getClass(Typ);
  if Assigned(aJavaClass)
    then Result:= aJavaClass.Signature
    else Result:= '';
end;

function TComJava2.getNewObjectName(const AClassname: string): string;
  var Int, Nr, j, k: Integer; Str, Name: string;
begin
  Name:= AClassname;
  if ObjectLowerCaseLetter = '-1' then
    // name:= LowerCase(copy(name, 1, 1)) + copy(name, 2, length(name));
    Name:= LowerCase(Name);
  Nr:= 1;
  for Int:= 0 to ObjectList.Count - 1 do begin
    Str:= ObjectList[Int];
    j:= Length(Str);
    // classnames can have digits too, example: Eval$5
    while (j > Length(AClassname)) and CharInSet(Str[j], ['0'..'9']) do
      Dec(j);
    if j < Length(Str) then begin
      k:= StrToInt(Copy(Str, j+1, Length(Str)));
      Str:= Copy(Str, 1, j);
      if (Str = Name) and (Nr <= k) then
        Nr:= k + 1;
    end;
  end;
  Result:= Name + IntToStr(Nr);
end;

function TComJava2.getNewClassname: string;
  var Int, Nr, j: Integer; Str, AClassname: string;
begin
  AClassname:= 'Class';
  Nr:= 1;
  for Int:= 0 to ClassList.Count - 1 do begin
    if Pos(AClassname, ClassList[Int]) = 0 then
      Continue;
    Str:= Right(ClassList[Int], Length(AClassname)+1);
    j:= StrToInt(Str);
    if Nr <= j then Inc(Nr);
  end;
  Result:= AClassname + IntToStr(Nr);
end;

function TComJava2.getPackage(const AClassname: string): string;
begin
  Result:= TJavaClass.getPackage(AClassname);
end;

function TComJava2.DeleteObject(const Objectname: string): string;
begin
  var Int:= ObjectList.IndexOf(Objectname);
  if Int > -1 then begin
    var aJavaObject:= TJavaObject(ObjectList.Objects[Int]);
    aJavaObject.DelFromProperties(Objectname);
    FreeAndNil(aJavaObject);
    ObjectList.Delete(Int);
  end;
  Result:= '+OK ';
end;

function TComJava2.cookClass(const Pathname: string): string;
begin
  Result:= TJavaClass.cookClass(Pathname);
end;

function TComJava2.Compile(const Parameter, Pathname: string): string;
begin
  if aJavaCompiler = nil then
    aJavaCompiler:= TJavaCompiler.Create;
  Result:= aJavaCompiler.compile(Classpath, Parameter, Pathname);
end;

function TComJava2.Init: string;
  var cp: string; VM: TJavaVM;
begin
  cp:= UnHideBlanks(Classpath);
  var Editorfolder:= withoutTrailingSlash(ExtractFilePath(ParamStr(0)));
  if Pos(Editorfolder, cp) = 0 then
    cp:= cp + ';' + Editorfolder;
  aJavaRuntime:= TJavaRuntime.GetDefault(JavaFolder);
  if aJavaRuntime.errormessage <> '' then begin
    Result:= '-ERR ' + aJavaRuntime.ErrorMessage;
    Exit;
  end;

  aJavaRuntime.processCommandLineOption('-cp ' + cp);
  // if debugging then we have to use different adresses in: jdwp=transport=dt_shmem,address=jdbconn
  if ConnectionAddress <> '' then begin
    aJavaRuntime.Debugging:= True;
    aJavaRuntime.ConnectionAddress:= ConnectionAddress;
  end else
    aJavaRuntime.Debugging:= False;
  VM:= aJavaRuntime.GetVM;

  // read debugging message: Listening for transport dt_shmem at address: jdbconn
  // s:= ReadConsole;

  ShowInMemo('CurrentDir: ' + GetCurrentDir);
  ShowInMemo('Classpath: ' + aJavaRuntime.Classpath);
  ShowInMemo('DLL: ' + aJavaRuntime.RuntimeLib);
  ShowInMemo('Debug: ' + ConnectionAddress);
  ShowInMemo(DateTimeToStr(Now()) + ' ' + GetComputerNetName);
  if VM = nil
    then Result:= '-ERR ' + aJavaRuntime.ErrorMessage
    else Result:= '+OK ';
end;

function TComJava2.setParam(const Nr, Param: string): string;
begin
  var ParamNr:= StrToInt(Nr);
  case ParamNr of
    0: hWndFJava:= StrToInt(Param);
    1: Classpath:= Param;
    2: begin
         JavaFolder:= withoutTrailingSlash(Param);
         ExpandPath(JavaFolder + '\bin;');
         SetEnvironmentVar('JAVA_HOME', JavaFolder);
       end;
    3: LNGFileNotFound:= Param;
    4: LNGUnknownObject:= Param;
    5: LNGUnknownClass:= Param;
    6: LNGIncompatibelTypes:= Param;
    7: ObjectLowerCaseLetter:= Param;
    8: ConnectionAddress:= Param;
    9: LogfileExceptions:= Param;
   10: LogfileExceptionsOK:= Param;
   11: Version:= Param;
   12: ShowInheritedPrivateAttributes:= Param;
  end;
  Result:= '+OK';
end;

function TComJava2.logMemo(const Pathname, aVersion: string): string;
  var F: TextFile; Int: Integer;
begin
  AssignFile(F, Pathname);
  try
    try
      Append(F);
      Writeln(F, DateTimeToStr(Now()) + ' ' + GetComputerNetName + ' Version: ' + aVersion);
      Writeln(F, 'logMemo(' + Pathname + ')');
      for Int:= 0 to FJe2Java.Memo1.Lines.Count - 1 do
        Writeln(F, FJe2Java.Memo1.Lines[Int]);
      Result:= '+OK';
    except
      on e: Exception do
        Result:=  '-ERR ' + e.message;
    end;
  finally
    CloseFile(F);
  end;
end;

function TComJava2.ShowHide: string;
begin
  FJe2Java.Show;
  Result:= '+OK'
end;

procedure TComJava2.logClasslist;
 // var i: integer;
begin
  {FJe2Java.Memo1.Lines.Add('--- Classlist');
  for I := 0 to Classlist.Count - 1 do
    FJe2Java.Memo1.Lines.Add(inttoStr(i) + ' ' +Classlist.Strings[i]);
  FJe2Java.Memo1.Lines.Add('--- ');
  }
end;

end.
