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
      ExecThreadTerminated: boolean;
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

      constructor create;
      destructor Destroy; override;
      procedure SendMessageSynchron(const s: string);
      procedure ClosePipeHandles(var Pipe: TPipeHandles);
      function ExecuteCommand(const cmd: string): string;
      procedure ShowInMemo(const s: string);
      procedure processMessage(const command: string);
      procedure Close;

      // class methods
      function LoadClass(const aClassname, Pathname: string): string;
      function createClass(const aClassname: string): string;
      function getClass(const aClassname: string): TJavaClass;
      function ClassToJavaClass(aClass: jClass): TJavaClass;
      function hasSuperClass(const aClassname: string): string;
      function getSuperClass(const aClassname: string): string;
      function isInterface(const aClassname: string): string;
      function findClass(const aClassname: string): string;
      function getAttributes(const aClassname: string): string;
      function getAttributeNames(const aClassname: string): string;
      function getRefreshedAttributeNames(const aClassname: string): string;
      function getMethods(const aClassname, _Static: string): string;
      function getConstructors(const aClassname: string): string;
      function getNewObjectName(const aClassname: string): string;
      function getNewClassname: string;
      function getPackage(const aClassname: string): string;

      // object methods
      function CreateObject(const aClassname, Objectname, Signature, Params: string): string;
      function GetObject(const Objectname: string): TJavaObject;
      function DeleteObject(const Objectname: string): string;

      function ToString(const Objectname: string): string; reintroduce;
      function getClassOf(const Objectname: string): string;
      function getAttributeValues(const Objectname, ArrayList: string): string;
      function DebugGetAttributeValues(const Objectname: string): string;
      function getObjectAttributeValues(const Objectname, Withtype, ArrayList: string): string;
      function getObjectAttributeNames(const Objectname: string): string;
      function getExpressionValue(const aClassname: string): string;
      function callMethod(const aClassname, Objectname, aMethodname, Returntype, Signature, Params, Methodtype: string): string;

      function getVariables(): string;
      function getAttributeValue(const aClassname, Objectname, Fieldname, Sig, IsStatic: string): string;
      function setAttributeValue(const aClassname, Objectname, Fieldname, Sig, value, IsStatic: string): string;

      procedure InitConsole;
      procedure IgnoreConsole;
      function ReadConsole: string;
      procedure HideConsole;
      procedure WriteToJava(const s: ANSIString);

      function IsPackageClass(const aClassname: string): boolean;
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

constructor TComJava2.create;
begin
  ObjectList:= TStringList.Create;
  ObjectList.CaseSensitive:= true;
  ClassList := TStringList.Create;
  ClassList.CaseSensitive:= true;
  InitConsole;
end;

procedure TComJava2.Close;
  var aObject: TObject;
begin
  ExecThreadTerminated:= true;
  for var i:= ClassList.Count - 1 downto 0 do begin
    aObject:= TJavaClass(ClassList.Objects[i]);
    FreeAndNil(aObject);
  end;
  ClassList.Clear;
  for var i:= ObjectList.Count - 1 downto 0 do begin
    aObject:= TObject(ObjectList.Objects[i]);
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
        Result:= getAttributes(SL[1])
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
    except on e: exception do
      Result:= '-ERR: ' + E.Message;
  end
  finally
    FreeAndNil(SL);
  end;
end;

procedure TComJava2.processMessage(const command: string);
  var s: string;
begin
  if Pos('setParam', command) <> 1 then
    ShowInMemo('> ' + command);
  try
    s:= ExecuteCommand(command);
  except
    on e: exception do
      s:= '-ERR ' + e.Message;
  end;
  if Pos('setParam', command) <> 1 then begin
    ShowInMemo('< ' + s);
    ShowInMemo(ReadConsole);
  end;
  SendMessageSynchron(s);
end;

procedure TComJava2.SendMessageSynchron(const s: string);
  var cds: TCopyDataStruct;
begin
  cds.dwData:= 0; // used to identify the message contents
  cds.cbData:= Length(s)*sizeOf(Char);
  cds.lpData:= PChar(s);
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
  SecAttr.bInheritHandle:= true;

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

procedure TComJava2.ShowInMemo(const s: string);
begin
  FJe2Java.ShowInMemo(s);
end;

function TComJava2.LoadClass(const aClassname, Pathname: string): string;
begin
  var aJavaClass:= TJavaClass.CreateLoadClass(aClassname, Pathname);
  try
    // if there is an exception in LoadClass then LoadClass is aborted
    // the exeception is handled in TComJava2.NewMessage
    ShowInMemo('Loadclass ausgeführt.');
    if assigned(aJavaClass.Handle) then begin
      aJavaClass.Global:= true;
      var i:= ClassList.IndexOf(aClassname);
      if i = -1 then
        ClassList.AddObject(aClassname, aJavaClass);
      Result:= '+OK'
    end else
      Result:= '-ERR Could not load ' + Pathname + #13#10 + aJavaClass.Error;
  except
    on e: exception do begin
      ShowInMemo(e.message);
      ErrorMessage:= '-ERR ' + e.message;
      Result:= ErrorMessage;
    end;
  end;
end;

function TComJava2.createClass(const aClassname: string): string;
begin
  Result:= '+OK';
  var i:= ClassList.IndexOf(aClassname);
  if i = -1 then begin
    var aJavaClass:= TJavaClass.Create(aClassname);
    if aJavaClass.Valid then begin
      aJavaClass.Global:= true;
      ClassList.AddObject(aClassname, aJavaClass);
      logClasslist;
    end else begin
      Result:= '-ERR ' + aJavaClass.Error;
      FreeAndNil(aJavaClass);
    end
  end;
end;

function TComJava2.getClass(const aClassname: string): TJavaClass;
begin
  var i:= ClassList.IndexOf(aClassname);
  if i = -1 then begin
    ErrorMessage:= CreateClass(aClassname);
    i:= ClassList.IndexOf(aClassname);
  end;
  if i = -1
    then Result:= nil
    else Result:= TJavaClass(ClassList.Objects[i]);
end;

function TComJava2.ClassToJavaClass(aClass: jClass): TJavaClass;
begin
  var aJavaClass:= TJavaClass.Create(aClass);
  if aJavaClass.Valid then begin
    var i:= ClassList.IndexOf(aJavaClass.Name);
    if i = -1 then begin
      aJavaClass.Global:= true;
      ClassList.AddObject(aJavaClass.Name, aJavaClass);
      logClasslist;
      Result:= aJavaClass;
    end else
      Result:= TJavaClass(ClassList.Objects[i])
  end else begin
    FreeAndNil(aJavaClass);
    Result:= nil;
  end;
end;

function TComJava2.getObject(const Objectname: string): TJavaObject;
begin
  var i:= ObjectList.IndexOf(Objectname);
  if i = -1
    then Result:= nil
    else Result:= TJavaObject(ObjectList.Objects[i]);
end;

function TComJava2.getAttributeValue(const aClassname, Objectname, Fieldname, Sig, IsStatic: string): string;
  var myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      myClass: TJavaClass; myObject: TJavaObject;
begin
  myClass:= GetClass(aClassname);
  myObject:= GetObject(Objectname);
  myAttribute:= TJavaAttribute.Create(myClass);
  try
    if IsStatic = 's'
      then aJavaValue:= myAttribute.GetStaticAttributeValue(Fieldname, Sig)
      else aJavaValue:= myAttribute.GetAttributeValue(myObject, Fieldname, Sig);
    if assigned(aJavaValue)
      then Result:= '+OK ' + aJavaValue.AsString
      else Result:= '-ERR Unknown attribute ' + aClassname + '.' + Fieldname;
  finally
    FreeAndNil(myAttribute);
    FreeAndNil(aJavaValue);
  end;
end;

function TComJava2.setAttributeValue(const aClassname, Objectname, Fieldname, Sig, value, IsStatic: string): string;
  var myAttribute: TJavaAttribute; myJavaValue: TJavaValue;
      myClass: TJavaClass; myObject: TJavaObject;
begin
  myClass:= getClass(aClassname);
  myObject:= getObject(Objectname);
  myAttribute:= TJavaAttribute.Create(myClass);
  myJavaValue:= TJavaValue.create(Value, Sig);
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
  if assigned(myObject)
    then Result:= '+OK ' + myObject.ToString
    else Result:= '-ERR';
end;

function TComJava2.hasSuperClass(const aClassname: string): string;
begin
  var myClass:= getClass(aClassname);
  if myClass = nil then
    TJavaClass.Create(aClassname);
  if myClass.Valid and myClass.hasSuperClass
    then Result:= '+OK '
    else Result:= '-NO ' + MyClass.Error;
end;

function TComJava2.getSuperClass(const aClassname: string): string;
begin
  var myClass:= getClass(aClassname);
  if assigned(myClass)
    then Result:= '+OK ' + myClass.getSuperclassName
    else Result:= TJavaClass.getSuperClassFromName(aClassname);
end;

function TComJava2.findClass(const aClassname: string): string;
begin
  Result:= TJavaClass.FindClass(aClassname);
end;

function TComJava2.getClassOf(const Objectname: string): string;
begin
  var myObject:= getObject(Objectname);
  if assigned(myObject) then begin
    var s:= myObject.ClassRef.Name;  // Typ
    if s = '' then begin
      s:= getNewClassname;
      ClassList.AddObject(s, myObject.ClassRef);
      logClasslist
    end;
    Result:= '+OK ' + s + '|' + myObject.ToString;
  end else
    Result:= '-ERR Unknown object "' + Objectname + '"';
end;

function TComJava2.IsInterface(const aClassname: string): string;
begin
  Result:= TJavaClass.IsInterface(aClassname);
end;

function TComJava2.getMethods(const aClassname, _Static: string): string;
begin
  var aJavaClass:= getClass(aClassname);
  if not assigned(aJavaClass) then
    aJavaClass:= TJavaClass.Create(aClassname); // maybe a system class
  if aJavaClass.Valid
    then Result:= aJavaClass.getMethods(_Static)
    else Result:= '-ERR ' + ReadConsole;
end;

function TComJava2.getConstructors(const aClassname: string): string;
begin
  var aJavaClass:= getClass(aClassname);
  if not assigned(aJavaClass) then
    aJavaClass:= TJavaClass.Create(aClassname); // maybe a system class
  if aJavaClass.Valid
    then Result:= aJavaClass.getConstructors
    else Result:= '-ERR ' + ReadConsole;
end;

function TComJava2.getAttributes(const aClassname: string): string;
begin
  var aJavaClass:= getClass(aClassname);
  if assigned(aJavaClass)
    then Result:= '+OK ' + aJavaClass.getAttributes
    else Result:= '-ERR ' + 'Unknown class: ' + aClassname;
end;

function TComJava2.getAttributeNames(const aClassname: string): string;
begin
  var aJavaClass:= getClass(aClassname);
  if assigned(aJavaClass)
    then Result:= '+OK ' + aJavaClass.getAttributeNames
    else Result:= '-ERR ' + 'Unknown class: ' + aClassname;
end;

function TComJava2.getRefreshedAttributeNames(const aClassname: string): string;
begin
  var aJavaClass:= getClass(aClassname);
  if assigned(aJavaClass)
    then Result:= '+OK ' + aJavaClass.getRefreshedAttributeNames
    else Result:= '-ERR ' + 'Unknown class: ' + aClassname;
end;

function TComJava2.getAttributeValues(const Objectname, ArrayList: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      i: integer; s: string; AttributesList: TObjectList;

  function ArrayListToList(s: string): string;
  begin
    s:= getObjectAttributeValues(s, '', ArrayList);
    s:= copy(s, 7, length(s) - 7);
    var SL:= split(',', s);
    s:= '[';
    for var j:= 0 to SL.Count - 1 do begin
      var s1:= trim(SL[j]);
      if (s1 <> '') and (s1 <> 'null') then
        s:= s + s1 + ', ';
    end;
    if s = '['
      then s:= '[]'
      else s:= Left(s, length(s) - 2) + ']';
    FreeAndNil(SL);
    Result:= s;
  end;

begin
  myObject:= getObject(Objectname);
  if assigned(myObject) then
    if assigned(myObject.ClassRef) then begin
      if not assigned(myObject.ClassRef.AttributesList) then
        myObject.getAttributes;
      AttributesList:= myObject.ClassRef.AttributesList;
      s:= '';
      for i:= 0 to AttributesList.Count - 1 do begin
        myAttribute:= TJavaAttribute(AttributesList.Items[i]);
        if myAttribute.Static_
          then aJavaValue:= myAttribute.GetStaticAttributeValue()
          else aJavaValue:= myAttribute.GetAttributeValue(myObject);
        if assigned(aJavaValue) then
          if (ArrayList = 'ArrayList') and (myAttribute.Typ = 'java.util.ArrayList')
            then s:= s + #4 + ArrayListToList(aJavaValue.AsString)
            else s:= s + #4 + aJavaValue.AsString
          else s:= s + #4'<error>';
        FreeAndNil(aJavaValue);
      end;
      Result:= '+OK ' + s + #4 + myObject.ToString;
    end else
      Result:= '-ERR unknown class for object ' + Objectname
  else
    Result:= '-ERR unknown object ' + Objectname
end;

function TComJava2.DebugGetAttributeValues(const Objectname: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      i: integer; s: string; AttributesList: TObjectList;
begin
  myObject:= getObject(Objectname);
  if assigned(myObject) then
    if assigned(myObject.ClassRef) then begin
      if assigned(myObject.ClassRef.AttributesList) then begin
        myObject.ClassRef.AttributesList.Free;
        myObject.ClassRef.AttributesList:= nil;
        myObject.getAttributes;
      end;
      AttributesList:= myObject.ClassRef.AttributesList;
      s:= '';
      for i:= 0 to AttributesList.Count - 1 do begin
        myAttribute:= TJavaAttribute(AttributesList.Items[i]);
        if myAttribute.Static_
          then aJavaValue:= myAttribute.GetStaticAttributeValue()
          else aJavaValue:= myAttribute.GetAttributeValue(myObject);
        if assigned(aJavaValue)
          then s:= s + #4 + myAttribute.Name + '=' + aJavaValue.AsString
          else s:= s + #4 + myAttribute.Name + '=' + '<error>';
        FreeAndNil(aJavaValue);
      end;
      Result:= '+OK ' + 'Debug ' + Objectname + ':  ' + s;
    end else
      Result:= '-ERR unknown class for object ' + Objectname
  else
    Result:= '-ERR unknown object ' + Objectname
end;

function TComJava2.getObjectAttributeValues(const Objectname, WithType, ArrayList: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute; aJavaValue: TJavaValue;
      i, j: integer; s, s1, s2: string; SL: TStringList; AttributesList: TObjectList;

  procedure AddObject(const obj: string);
  begin
    var aJavaObject:= getObject(obj);
    if assigned(aJavaObject) and (WithType = 'withtype')
      then s:= s + #4 + obj + '=' + aJavaObject.Classref.ImportTyp
      else s:= s + #4 + obj;
  end;

  procedure ArrayListToList(obj: string);
    var s1, s2: string;
  begin
    s2:= '[';
    obj:= getObjectAttributeValues(obj, '', ArrayList);
    obj:= copy(obj, 7, length(obj) - 7);
    SL:= split(',', obj);
    for var j:= 0 to SL.Count - 1 do begin
      s1:= trim(SL[j]);
      if (s1 <> '') and (s1 <> 'null') then begin
        var aJavaObject:= getObject(s1);
        if assigned(aJavaObject) and (WithType = 'withtype')
          then s2:= s2 + s1 + '=' + aJavaObject.Classref.ImportTyp + ', '
          else s2:= s2 + s1 + ', ';
      end;
    end;
    if s2 = '['
      then s2:= '[]'
      else s2:= Left(s2, length(s2) - 2) + ']';
    s:= s + #4 + s2;
    FreeAndNil(SL);
  end;

begin
  s:= '';
  myObject:= GetObject(Objectname);
  if assigned(myObject) and assigned(myObject.ClassRef) then begin
    if not assigned(myObject.ClassRef.AttributesList) then
      myObject.getAttributes;
    AttributesList:= myObject.ClassRef.AttributesList;
    for i:= 0 to AttributesList.Count - 1 do begin
      myAttribute:= TJavaAttribute(AttributesList.Items[i]);
      if myAttribute.isObject then begin
        if myAttribute.Static_
          then aJavaValue:= myAttribute.GetStaticAttributeValue()
          else aJavaValue:= myAttribute.GetAttributeValue(myObject);
        if assigned(aJavaValue) then begin
          s1:= aJavaValue.AsString;
          if (Pos('{', s1) = 1) and (Withtype = 'withtype') then begin   // array
            s1:= copy(s1, 2, length(s1) - 2);
            SL:= Split(',', s1);
            for j:= 0 to SL.Count - 1 do begin
              s2:= trim(SL[j]);
              AddObject(s2);
            end;
            FreeAndNil(SL);
          end else if (ArrayList = 'ArrayList') and (myAttribute.Typ = 'java.util.ArrayList') then
            ArrayListToList(s1)
          else
            AddObject(s1);
        end else
          AddObject('<error>');
        FreeAndNil(aJavaValue);
      end;
    end;
    Result:= '+OK ' + s;
  end else
    Result:= '-ERR No object or no class';
end;

function TComJava2.getObjectAttributeNames(const Objectname: string): string;
  var myObject: TJavaObject; myAttribute: TJavaAttribute;
      i: integer; s: string; AttributesList: TObjectList;
begin
  s:= '';
  myObject:= GetObject(Objectname);
  if assigned(myObject) and assigned(myObject.ClassRef) then begin
    if not assigned(myObject.ClassRef.AttributesList) then
      myObject.getAttributes;
    AttributesList:= myObject.ClassRef.AttributesList;
    for i:= 0 to AttributesList.Count - 1 do begin
      myAttribute:= TJavaAttribute(AttributesList.Items[i]);
      if myAttribute.isObject then
        s:= s + #4 + myAttribute.Name;
    end;
    Result:= '+OK ' + s;
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
    Result:= Result + String(Buffer);
  end;
end;

procedure TComJava2.HideConsole;
begin
  HideThread:= THideThread.Create;
end;

procedure TComJava2.WriteToJava(const s: ANSIString);
  var i: cardinal;
begin
  WriteFile(InputPipe.hWrite, s[1], length(s), i, nil);
end;

function TComJava2.CreateObject(const aClassname, Objectname, Signature, Params: string): string;
  var ConsoleThread: TConsoleThread; ExecThread: TExecThread;
      aJavaObject: TJavaObject; myClass: TJavaClass; theParams: TJavaParams;
      Error, aSignature: string;
begin
  aJavaObject:= nil;
  ConsoleThread:= nil;
  myClass:= getClass(aClassname);
  aSignature:= copy(Signature, 2, length(Signature)-3);
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
      if assigned(ConsoleThread) then begin
        ConsoleThread.Terminate;
        ConsoleThread.WaitFor;
        FreeAndNil(ConsoleThread);
      end;
      FreeAndNil(theParams);
    end;
  end else
    Error:= theParams.Error;
  if assigned(aJavaObject)
    then Result:= '+OK ' + aJavaObject.ToString
    else Result:= '-ERR ' + Error + #13#10 + ReadConsole;
end;

function TComJava2.callMethod(const aClassname, Objectname, aMethodname, Returntype, Signature, Params, Methodtype: string): string;
  var ConsoleThread: TConsoleThread; ExecThread: TExecThread;
      aJavaObject: TJavaObject; theParams: TJavaParams; myClass: TJavaClass;
      aJavaTyp: TNumType; aJavaMethod: TJavaMethod; aJavaValue: TJavaValue;
      p: integer; aReturnType: string;
begin
  Result:= '-ERR';
  myClass:= getClass(aClassname);
  if myClass = nil then begin
    Result:= '-ERR Unknown class: ' + aClassname;
    exit;
  end;
  aJavaObject:= getObject(Objectname);
  if (aJavaObject = nil) and (Objectname <> '') then begin
    Result:= '-ERR Unknown object: ' + Objectname;
    exit;
  end;
  aJavaTyp:= TypToNumType(ReturnType);
  // check, if Returntype exists, due to generic problem
  if (aJavaTyp in [ntObject, ntObjectArray, ntObjectObjectArray]) and (getClass(ReturnType) = nil)
    then aReturnType:= 'java.lang.Object'
    else aReturnType:= ReturnType;
  try
    p:= Pos(')', Signature);
    theParams:= TJavaParams.CreateMakeParams(copy(Signature, 2, p-2), Params);
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
              ExecThreadTerminated:= false;
              ExecThread.Start;
              repeat
                Application.ProcessMessages;
                Sleep(5);
              until ExecThreadTerminated;
              aJavaValue:= ExecThread.aJavaValue;
              if aJavaValue.Error = ''
                then Result:= '+OK '  + aJavaValue.AsString
                else Result:= '-ERR ' + String(ConsoleThread.Buffer) + ConsoleThread.aException + aJavaValue.Error;
              aJavaValue.Free;
            finally
              FreeAndNil(ExecThread);
            end;
            for p:= 0 to 9 do begin
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

function TComJava2.getExpressionValue(const aClassname: string): string;
  var s, Objectname, ExprType: string; 
      myAttribute: TJavaAttribute;
      aJavaValue: TJavaValue;
      myObject: TJavaObject; SL: TStringList;
begin
  s:= callMethod(aClassname, '', 'run', 'java.lang.Object', '()Ljava/lang/Object;', '', '0');
  if Left(s, 3) = '+OK' then begin
    Objectname:= Right(s, 5);
    myObject:= GetObject(Objectname);
    s:= getAttributes(myObject.ClassRef.Name);
    // public int Shell$7.result;
    if Left(s, 3) = '+OK' then begin
      s:= Right(s, 5);
      SL:= Split('|', s);
      ExprType:= SL[4];
      SL.Free;
      //myObject:= GetObject(Objectname);
      myAttribute:= TJavaAttribute.Create(myObject.ClassRef);
      aJavaValue:= myAttribute.GetAttributeValue(myObject, 'result', TypToSig(ExprType));
      try
        if assigned(aJavaValue)
          then Result:= '+OK ' + aJavaValue.AsString  + '_|_' + ExprType
          else Result:= '-ERR Unknown attribute ' + aClassname + '.result';
      finally
        FreeAndNil(myAttribute);
        FreeAndNil(aJavaValue);
      end;
      DeleteObject(Objectname);
    end
    else
      Result:= s;
  end else
    Result:= s;
end;

function TComJava2.getVariables(): string;
  var s, s2, s3, obj, Name, Value: string; i, p, p1: integer;
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
    SystemProperties:= TJavaObject.createWithHandle(aJavaClass, aJValue.s);
    try
      s:= SystemProperties.ToString;
      s2:= '';
      repeat
        p:= Pos('_lv', s);
        if p > 0 then begin
          delete(s, 1, p+2);
          obj:= copy(s, 1, 2);
          delete(s, 1, 2);
          p1:= Pos('=', s);
          Name:= copy(s, 1, p1-1);
          p:= Pos(', ', s);
          Value:= copy(s, p1+1, p-p1-1);
          if obj = 'o_' then begin
            i:= ObjectList.IndexOf(Name);
            if i >= 0 then begin
              aJavaObject:= TJavaObject(ObjectList.Objects[i]);
              s3:= aJavaObject.ToString;
              if s3 <> Value then begin  // Object changed
                aJavaObject.Destroy;
                ObjectList.Delete(i);
                aJavaObject:= TJavaObject.CreateFromProperties(Name);
                ObjectList.AddObject(Name, aJavaObject);
              end;
              s2:= s2 + #4 + aJavaObject.ClassRef.Name + '|' + Name + '=' +s3;
            end else begin
              aJavaObject:= TJavaObject.CreateFromProperties(Name);
              ObjectList.AddObject(Name, aJavaObject);
              s2:= s2 + #4 + aJavaObject.ClassRef.Name + '|' + Name + '=' + Value;
            end
          end else
            s2:= s2 + #4 + Name + '=' + Value;
        end;
      until p=0;
    finally
      FreeAndNil(SystemProperties);
    end;
  finally
    FreeAndNil(aJavaMethod);
  end;
  Result:= '+OK ' + copy(s2, 2, length(s2));
end;

procedure TComJava2.ExecThreadOnTerminate(Sender: TObject);
begin
  ExecThreadTerminated:= true;
end;

function TComJava2.IsPackageClass(const aClassname: string): boolean;
begin
  Result:= (LastDelimiter('.', aClassname) > 0);
end;

function TComJava2.GetSignature(const Typ: string): string;
begin
  var aJavaClass:= getClass(Typ);
  if assigned(aJavaClass)
    then Result:= aJavaClass.Signature
    else Result:= '';
end;

function TComJava2.getNewObjectName(const aClassname: string): string;
  var i, Nr, j, k: integer; s, name: string;
begin
  name:= aClassname;
  if ObjectLowerCaseLetter = '-1' then
    // name:= LowerCase(copy(name, 1, 1)) + copy(name, 2, length(name));
    name:= LowerCase(name);
  Nr:= 1;
  for i:= 0 to ObjectList.Count - 1 do begin
    s:= ObjectList[i];
    j:= length(s);
    // classnames can have digits too, example: Eval$5
    while (j > length(aClassname)) and CharInSet(s[j], ['0'..'9']) do
      dec(j);
    if j < length(s) then begin
      k:= StrToInt(copy(s, j+1, length(s)));
      s:= copy(s, 1, j);
      if (s = name) and (Nr <= k) then
        Nr:= k + 1;
    end;
  end;
  Result:= name + IntToStr(Nr);
end;

function TComJava2.getNewClassname: string;
  var i, Nr, j: integer; s, aClassname: string;
begin
  aClassname:= 'Class';
  Nr:= 1;
  for i:= 0 to ClassList.Count - 1 do begin
    if Pos(aClassname, ClassList[i]) = 0 then
      continue;
    s:= Right(ClassList[i], Length(aClassname)+1);
    j:= StrToInt(s);
    if Nr <= j then inc(Nr);
  end;
  Result:= aClassname + IntToStr(Nr);
end;

function TComJava2.getPackage(const aClassname: string): string;
begin
  Result:= TJavaClass.getPackage(aClassname);
end;

function TComJava2.DeleteObject(const Objectname: string): string;
begin
  var i:= ObjectList.IndexOf(Objectname);
  if i > -1 then begin
    var aJavaObject:= TJavaObject(ObjectList.Objects[i]);
    aJavaObject.DelFromProperties(Objectname);
    FreeAndNil(aJavaObject);
    ObjectList.Delete(i);
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
    aJavaCompiler:= TJavaCompiler.create;
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
    exit;
  end;

  aJavaRuntime.processCommandLineOption('-cp ' + cp);
  // if debugging then we have to use different adresses in: jdwp=transport=dt_shmem,address=jdbconn
  if ConnectionAddress <> '' then begin
    aJavaRuntime.Debugging:= true;
    aJavaRuntime.ConnectionAddress:= ConnectionAddress;
  end else
    aJavaRuntime.Debugging:= false;
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
  var F: TextFile; i: integer;
begin
  AssignFile(F, Pathname);
  try
    try
      Append(F);
      Writeln(F, DateTimeToStr(Now()) + ' ' + GetComputerNetName + ' Version: ' + aVersion);
      Writeln(F, 'logMemo(' + Pathname + ')');
      for i:= 0 to FJe2Java.Memo1.Lines.Count - 1 do
        Writeln(F, FJe2Java.Memo1.Lines[i]);
      Result:= '+OK';
    except
      on e: exception do
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
