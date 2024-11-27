unit UComJava1;

interface

uses Windows, Classes, Forms, ExtCtrls, Pipes, UJNIWrapper1;

type
  TComJava1 = class
  private
    Timer: TTimer;
    ClassImports: TStringList;
    FullImports: TStringList;
    JavaStartpath: string;
    ErrorMessage: string;
    PipeClient: TPipeClient;
    ConsolePipeClient: TPipeClient;
    Received: string;
    ConsoleReceived: string;
    ConnectID: string;
    Number: string;
    UMLForm: TForm;
    Path: string;
    cp: string;
    DebugJe2Java: boolean;
    DebuggerDisabled: boolean;
    ParameterSend: boolean;
    procedure OnPipeError(Sender: TObject; Pipe: HPIPE; PipeContext: TPipeContext; ErrorCode: Integer);
    procedure OnPipeDisconnect(Sender: TObject; Pipe: HPIPE);
    procedure OnTimeout(Sender: TObject);
    procedure receiveMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
    procedure receiveConsoleMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
    function sendCommandSynchron(const command: string): string;
    function sendCommandAsynchron(const command: string): string;
    function sendMessageAsynchronWithPipe(s: string): boolean;
    function Je2JavaReady: boolean;
    procedure SendParameter;
    procedure CalculatePathAndCp(const Command: string);
    procedure clearObjectsAndClasses;
    procedure StartJE2Java;
    function StartJava(const Command: string): string;
    procedure JavaTerminated;
  public
    ObjectList: TStringList;
    ClassList: TStringList;
    ErrorOccured: boolean;
    Sourcepath: string;
    Terminated: boolean;
    ProcessInformation: TProcessinformation;
    AnswerFromJe2Java: string;
    constructor create(aUMLForm: TForm; aNumber: integer);
    destructor Destroy; override;
    function NewClass(aClassname, Pathname: string): boolean;
    function NewAPIClass(aClassname: string): boolean;
    function GetClass(aClassname: string): TComJavaClass;
    function GetSignature(const Typ: string): string;
    function GetUniqueObjectName(Typ: string): string;
    function GetObject(const Objectname: string): TComJavaObject;
    procedure DeleteObject(const Objectname: string);
    function NewObject(const Objectname: string; myClass: TComJavaClass; myParams: TComJavaParams): TComJavaObject;
    function NewUnnamedObject(const Objectname: string): TComJavaObject;
    procedure AddObject(const Objectname: string; myClass: TComJavaClass);
    procedure JavaReset;
    procedure Transfer(ClassImp, FullImp: TStringList; const theSourcepath: string);
    procedure WriteToJava(s: string);
    function ExecuteCommand(const Command: string): string;
    function ExecuteCommandWithoutDebugger(const Command: string): string;
    function getFirstComJava: TComJava1;
    procedure setActiveComJava(aComJava: TComJava1);
  end;

  function getComJava: TComJava1;

implementation

uses Dialogs, SysUtils, Messages, DateUtils,
     JvGnugettext, UStringRessources, UUMLForm, UJava, UDlgAbout, UUtils,
     UMessages, UConfiguration, UDebugger, UUMLModule, URtfdDiagram;

var activeComJava: TComJava1;   // the active ComJava
    firstComJava: TComJava1;    // ComJava of Interactive

function getComJava: TComJava1;
begin
  if activeComJava = nil then
    activeComJava:= firstComJava;
  Result:= activeComJava;
end;

constructor TComJava1.create(aUMLForm: TForm; aNumber: integer);
begin
  Received:= '';
  AnswerFromJe2Java:= '';
  ErrorMessage:= '';
  DebuggerDisabled:= false;
  ParameterSend:= false;
  Self.UMLForm:= aUMLForm;
  Self.Number := IntToStr(aNumber);
  ObjectList  := TStringList.Create;
  ObjectList.CaseSensitive:= true;
  ClassList   := TStringList.Create;
  ClassList.CaseSensitive:= true;
  ClassImports:= TStringList.Create;
  FullImports := TStringList.Create;
  SetLength(TrueBoolStrs, 1);  TrueBoolStrs[0] := 'true';
  SetLength(FalseBoolStrs, 1); FalseBoolStrs[0]:= 'false';

  // Debug Je2Java
  // set DebugJe2Java to true to debug with two incarnations of Delphi
  // set  Application.ShowMainForm:= true; in je2java
  // start Je2Java with breakpoints
  // then start JavaEditor
  DebugJe2Java:= false;
  if DebugJe2Java
    then ConnectID:= ''
    else DateTimeToString(ConnectID, 'yyyymmddhhnnsszzz', Now);

  // callMethod, createObject and getVariables need asynchron communication
  // because they may have user-interaction and need to be done in a thread
  // asynchron communication is done via pipes
  // synchon communication is done with WM_CopyData which ist 100 times faster
  // as asynchron communication, which uses threads

  PipeClient:= TPipeClient.Create(nil);
  PipeClient.OnPipeMessage:= receiveMessage;
  PipeClient.OnPipeError:= OnPipeError;
  PipeClient.OnPipeDisconnect:= OnPipeDisconnect;
  PipeClient.PipeName:= 'je2java' + ConnectID;
  ConsolePipeClient:= TPipeClient.Create(nil);
  ConsolePipeClient.OnPipeMessage:= receiveConsoleMessage;
  ConsolePipeClient.PipeName:= 'javaconsole' + ConnectID;

  Timer:= TTimer.Create(nil);
  Timer.enabled:= false;
  Timer.Interval:= 3000;
  Timer.OnTimer:= OnTimeOut;
  JavaStartpath:= '';
  FillChar(ProcessInformation, SizeOf(ProcessInformation), #0);
  if firstComJava = nil then firstComJava:= Self;
  activeComJava:= Self;
end;

destructor TComJava1.Destroy;
begin
  if activeComJava = Self then activeComJava:= nil;
  ClearObjectsAndClasses;
  FreeAndNil(ClassImports);
  FreeAndNil(FullImports);
  FreeAndNil(ClassList);
  FreeAndNil(ObjectList);
  PipeClient.Disconnect;
  ConsolePipeClient.Disconnect;
  FreeAndNil(PipeClient);
  FreeAndNil(ConsolePipeClient);
  FreeAndNil(Timer);
  inherited;
end;

procedure TComJava1.SendParameter;
begin
  sendCommandSynchron('setParam'#4'0'#4 + IntToStr(FJava.Handle));
  sendCommandSynchron('setParam'#4'1'#4 + cp);
  sendCommandSynchron('setParam'#4'2'#4 + FConfiguration.JDKFolder);
  sendCommandSynchron('setParam'#4'3'#4 + _(LNGFileNotFound));
  sendCommandSynchron('setParam'#4'4'#4 + _('Unknown object'));
  sendCommandSynchron('setParam'#4'5'#4 + _(LNGUnknownClass));
  sendCommandSynchron('setParam'#4'6'#4 + _('Incompatible types: found %s but %s expected'));
  sendCommandSynchron('setParam'#4'7'#4 + BoolToStr(FConfiguration.ObjectLowerCaseLetter));
  sendCommandSynchron('setParam'#4'8'#4 + 'jdbconn' + Number);
  sendCommandSynchron('setParam'#4'9'#4 + FConfiguration.LogfileExceptions);
  sendCommandSynchron('setParam'#4'10'#4 + BoolToStr(FConfiguration.LogfileExceptionsOK, true));
  sendCommandSynchron('setParam'#4'11'#4 + UDlgAbout.Version);
  sendCommandSynchron('setParam'#4'12'#4 + BoolToStr(FConfiguration.ShowObjectsWithInheritedPrivateAttributes));
  ParameterSend:= true;
end;

procedure TComJava1.StartJE2Java;
  var SecAttr    : TSecurityAttributes;
      StartupInfo: TStartupInfo;
      Je2JavaStarted: boolean;
begin
  SecAttr.nLength:= SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor:= nil;
  SecAttr.bInheritHandle:= false;
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do begin
    cb:= SizeOf(StartupInfo);
    dwFlags     := STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
  end;
  ErrorOccured:= false;
  ErrorMessage:= '';
  var s:= ExtractFilepath(ParamStr(0)) + 'je2java.exe';
  if DebugJe2Java then
    Je2JavaStarted:= true
  else
    Je2JavaStarted:= CreateProcess(nil,    // lpApplicationName
                       PChar(s + ' ' + ConnectID),   // lpCommandLine
                       nil,                // lpProcessAttributes
                       nil,                // lpThreadAttributes
                       False,              // bInheritHandles
                       CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, // dwCreationFlags
                       nil,                // pEnvironment
                       PChar(Path),        // 'C:',       // nil {PChar(Verz)  // pCurrentDirectory
                       StartupInfo,        // lpStartupInfo,
                       ProcessInformation  // lpProcessInformation
                    );
  if Je2JavaStarted then begin
    repeat
      sleep(20);
    until Je2JavaReady;
    SendParameter;
    SendCommandSynchron('init');
    if Left(AnswerFromJe2Java, 4) = '-ERR' then begin
      ErrorMessage:= Right(AnswerFromJe2Java, 6);
      ErrorOccured:= true;
    end else begin
      if PipeClient.Connect(2000) and ConsolePipeClient.Connect(2000) then
        WriteToJava('__init__')
      else begin
        ErrorOccured:= true;
        ErrorMessage:= _('No connection to: ') + s;
      end
    end;
  end else begin
    ErrorOccured:= true;
    ErrorMessage:= SysErrorMessage(GetLastError) + ': ' + s;
  end;
end;

function TComJava1.StartJava(const Command: string): string;
begin
  Result:= '+OK';
  Terminated:= false;
  CalculatePathAndCp(Command);
  StartJE2Java;

  if ErrorOccured then
    Result:= '-ERR ' + ErrorMessage
  else begin
    JavaStartpath:= path;
    { for debugging purposes:
      SL:= TStringList.Create;
      SL.Text:= 'jdb -attach jdbconn1';
      SL.SaveToFile(FConfiguration.TempDir + 'jdb.bat');
      FreeAndNil(SL);
      myJavaCommands.ExecWithConsole(FConfiguration.TempDir + 'jdb.bat');
      'E:\Programmierung\Delphi\_JavaEditor\javaeditor16.26\App\Temp\'
    }
    // you can set a breakpoint here, uncomment StartDebuggerFor UMLWindow
    // and manually call "jdb -attach conn1" in a console window

    if not DebuggerDisabled and FJava.hasBreakpoints and not myDebugger.Running then
      myDebugger.StartDebuggerForUMLWindow(Sourcepath, Number);
  end;
  FJava.ImperativeUpdateMenuItems;
end;

function TComJava1.ExecuteCommand(const Command: string): string;
begin
  {$WARNINGS OFF}
  if (DebugHook <> 0) and (Pos('#', Command) > 0) then
    ErrorMsg('#-Error with: ' + Command);
  {$WARNINGS ON}
  AnswerFromJe2Java:= '';

  if not Je2JavaReady or DebugJe2Java and not ParameterSend then
    if Command = 'term'
      then Exit('+OK')
      else StartJava(Command);

  if ErrorOccured or (Left(AnswerFromJe2Java, 4) = '-ERR') then begin
    if AnswerFromJe2Java <> '' then
      ErrorMessage:= Right(AnswerFromJe2Java, 6);
    ErrorMsg(ErrorMessage);
    Result:= '-ERR ' + ErrorMessage;
    JavaReset;
  end else begin
    var SL:= Split(#4, Command);
    try
      if (SL[0] = 'callMethod') or (SL[0] = 'getVariables') or
         (SL[0] = 'createObject') or (SL[0] = 'loadClass')
        then Result:= sendCommandAsynchron(Command)
        else Result:= sendCommandSynchron(Command);
      if Result = '+OK Java terminated' then begin
        JavaReset;
        ErrorOccured:= false;
      end;
    finally
      FreeAndNil(SL);
    end;
  end;
end;

function TComJava1.ExecuteCommandWithoutDebugger(const Command: string): string;
begin
  DebuggerDisabled:= true;
  Result:= ExecuteCommand(Command);
  DebuggerDisabled:= false;
end;

procedure TComJava1.CalculatePathAndCp(const Command: string);
   var aClassname: string;
begin
  var SL:= Split(#4, Command);
  if (SL[0] = 'compile') or (SL[0] = 'loadClass')
    then Path:= ExtractFilePath(SL[2])
    else Path:= FJava.getActiveTDIFormPath;
  if Path = '' then Path:= '.';
  if SL.Count <= 1
    then aClassname:= ''
    else aClassname:= SL[1];
  FreeAndNil(SL);

  cp:= UnHideBlanks(FConfiguration.getClassPathJarExpanded(Path, ExtractPackageName(aClassname)));
  if pos(Path, cp) = 0 then
    cp:= Path + ';' + cp;
  cp:= FConfiguration.TempDir + ';' + cp;
  cp:= HideBlanks(cp);
end;

// receive asynchron message from je2java
procedure TComJava1.receiveMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
begin
  SetLength(Received, Stream.Size div 2);
  Stream.Read(Received[1], Stream.Size);
  if Received = 'Java terminated' then
    JavaTerminated;
end;

procedure TComJava1.receiveConsoleMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
begin
  try
    SetLength(ConsoleReceived, Stream.Size div 2);
    Stream.Read(ConsoleReceived[1], Stream.Size);
    FMessages.OutputToTerminal(ConsoleReceived);
    Application.ProcessMessages;
  except on e: Exception do
    FConfiguration.Log('TComJava1.receiveConsoleMessage', e);
  end;
end;

function TComJava1.Je2JavaReady: boolean;
begin
  var s1:= 'TFJe2Java';
  var s2:= 'FJe2Java' + ConnectID;
  Result:= (FindWindow(PChar(s1), PChar(s2)) > 0);
end;

function TComJava1.sendMessageAsynchronWithPipe(s: string): boolean;
begin
  Received:= '';
  Result:= PipeClient.Write(s[1], length(s)*SizeOf(Char));
end;

function TComJava1.sendCommandSynchron(const command: string): string;
var
  cds: TCopyDataStruct;
  hWnd: THandle;
begin
  AnswerFromJe2Java:= '';
  cds.cbData := Length(command)*sizeOf(Char);
  cds.lpData := PChar(command);
  hWnd := FindWindow(PChar('TFJe2Java'), PChar('FJe2Java' + ConnectID));
  Windows.sendMessage(hWnd, WM_COPYDATA, FJava.Handle, LPARAM(@cds));
  Result:= AnswerFromJe2Java;
end;

procedure TComJava1.OnTimeout(Sender: TObject);
begin
  ErrorOccured:= true;
  ErrorMessage:= 'Java timeout - reset Java';
end;

function TComJava1.sendCommandAsynchron(const command: string): string;
begin
  if sendMessageAsynchronWithPipe(Command) then begin
    ErrorMessage:= '';
    ErrorOccured:= false;
    myDebugger.ReadyForInput:= true;
    FJava.ImperativeUpdateMenuItems;
    if (Pos('createObject', command) = 1) or (Pos('loadClass', command) = 1) or
       (Pos('getVariables', command) = 1)
    then
      Timer.enabled:= true;
    repeat
      if myDebugger.Running then
        myDebugger.ReadDebuggerForUMLWindow;
      Application.ProcessMessages;
      Sleep(20);
    until (Received <> '') or ErrorOccured or Terminated;
    Timer.enabled:= false;
    myDebugger.ReadyForInput:= false;
    if ErrorOccured then
      Result:= '-ERR ' + ErrorMessage
    else if Terminated then
      Result:= '+OK Java terminated'
    else
      Result:= Received;
    FJava.ImperativeUpdateMenuItems;
  end else
    Result:= '-ERR Sorry, Java doesn''t work. Please reset Java.';
end;

procedure TComJava1.OnPipeError(Sender: TObject; Pipe: HPIPE; PipeContext:
    TPipeContext; ErrorCode: Integer);
begin
  // this error comes with unusable jvm.dll
  if not ErrorOccured then begin
    ErrorOccured:= true;
    var key:= FConfiguration.JDKFolder;
    var s:= key + '\jre\bin\client\jvm.dll';
    if not FileExists(s) then
      s:= key + '\jre\bin\server\jvm.dll';
    ErrorMessage:= 'Could not load ' + s + '. Probably 64-Bit-Version.'#13#10 +
                   SysErrorMessage(ErrorCode);
  end;
end;

procedure TComJava1.OnPipeDisconnect(Sender: TObject; Pipe: HPIPE);
begin
  ErrorOccured:= true;
  ErrorMessage:= 'Java disconnected';
  JavaReset;
end;

function TComJava1.GetUniqueObjectName(Typ: string): string;
begin
  Typ:= WithoutArray(GetShortType(Typ));
  Result:= ExecuteCommand('getNewObjectName'#4 + Typ)
end;

function TComJava1.NewClass(aClassname, Pathname: string): boolean;
begin
  Result:= true;
  aClassname:= WithoutGeneric(aClassname);
  var i:= ClassList.IndexOf(aClassname);
  if i = -1 then begin
    var aJavaClass:= TComJavaClass.CreateLoadClass(aClassname, Pathname, Self);
    if aJavaClass.IsValid
      then ClassList.AddObject(aClassname, aJavaClass)
      else begin
        if not Startswith(aJavaClass.Error, 'The JVM') then
          ErrorMsg(aJavaClass.Error);
        FreeAndNil(aJavaClass);
        Result:= false;
      end;
  end;
end;

function TComJava1.NewAPIClass(aClassname: string): boolean;
begin
  Result:= true;
  aClassname:= WithoutGeneric(aClassname);
  var i:= ClassList.IndexOf(aClassname);
  if i = -1 then begin
    var aJavaClass:= TComJavaClass.CreateClass(aClassname, Self);
    if aJavaClass.IsValid
      then ClassList.AddObject(aClassname, aJavaClass)
      else begin
        ErrorMsg(aJavaClass.Error);
        FreeAndNil(aJavaClass);
        Result:= false;
      end;
  end;
end;

function TComJava1.GetClass(aClassname: string): TComJavaClass;
begin
  aClassname:= WithoutGeneric(aClassname);
  var i:= ClassList.IndexOf(aClassname);
  if i = -1 then begin
    var s:= FConfiguration.getClasspathFromSourcepath(aClassname, Sourcepath);
    if FConfiguration.isAPIClass(aClassname) or
       FConfiguration.IsInClasspath(aClassname, JavaStartpath) then
      NewAPIClass(aClassname)
    else if s <> '' then
      NewClass(aClassname, s)
    else begin
      i:= ClassImports.IndexOfName(aClassname);
      if i <> -1 then begin
        aClassname:= ClassImports.ValueFromIndex[i] + '.' + aClassname;
        NewAPIClass(aClassname);
        end
      else
        for i:= 0 to FullImports.Count - 1 do begin
          s:= FullImports.Strings[i] + aClassname;
          if FConfiguration.IsAPIClass(s) or
             FConfiguration.IsInClasspath(s, JavaStartpath)
          then begin
            aClassname:= s;
            NewAPIClass(aClassname);
            break;
          end;
        end;
    end;
    i:= ClassList.IndexOf(aClassname);
  end;
  if (i > -1) and (i < ClassList.count)
    then Result:= TComJavaClass(ClassList.Objects[i])
    else Result:= nil;
end;

function TComJava1.GetSignature(const Typ: string): string;
begin
  var aJavaClass:= GetClass(Typ);
  if assigned(aJavaClass)
    then Result:= aJavaClass.Signature
    else Result:= '';
end;

procedure TComJava1.DeleteObject(const Objectname: string);
begin
  var i:= ObjectList.IndexOf(Objectname);
  if (i > -1) and (i < ObjectList.Count) then begin
    var aJavaObject:= TComJavaObject(ObjectList.Objects[i]);
    aJavaObject.Delete;
    FreeAndNil(aJavaObject);
    ObjectList.Delete(i);
  end;
end;

function TComJava1.GetObject(const Objectname: string): TComJavaObject;
begin
  var i:= ObjectList.IndexOf(Objectname);
  if (i >= 0) and (i < ObjectList.Count)
    then Result:= TComJavaObject(ObjectList.Objects[i])
    else Result:= nil;
end;

function TComJava1.NewObject(const Objectname: string; myClass: TComJavaClass; myParams: TComJavaParams): TComJavaObject;
begin
  var ComJavaObject:= TComJavaObject.create(Objectname, myClass, myParams, Self);
  if ComJavaObject.IsValid then
    ObjectList.AddObject(Objectname, ComJavaObject);  // store of the java-objects
  Result:= ComJavaObject;
end;

procedure TComJava1.AddObject(const Objectname: string; myClass: TComJavaClass);
begin
  var ComJavaObject:= TComJavaObject.createByName(Objectname, myClass, Self);
  if ComJavaObject.IsValid then
    ObjectList.AddObject(Objectname, ComJavaObject);  // store of the java-objects
end;

function TComJava1.NewUnnamedObject(const Objectname: string): TComJavaObject;
begin
  var ComJavaObject:= TComJavaObject.createNewUnnamedObject(Objectname, Self);
  if ComJavaObject.IsValid then
    ObjectList.AddObject(Objectname, ComJavaObject);  // store of the java-objects
  Result:= ComjavaObject;
end;

procedure TComJava1.Transfer(ClassImp, FullImp: TStringList; const theSourcepath: string);
begin
  if assigned(Classimports) then begin
    ClassImports.Clear;
    FullImports.Clear;
    ClassImports.Assign(ClassImp);
    FullImports.Assign(FullImp);
  end;
  Sourcepath:= theSourcepath
end;

procedure TComJava1.WriteToJava(s: string);
begin
  ConsolePipeClient.Write(s[1], length(s)*SizeOf(Char));
end;

procedure TComJava1.ClearObjectsAndClasses;
  var i: integer; aJavaObject: TComJavaObject; aObject: TObject;
begin
  for i:= 0 to ObjectList.Count - 1 do begin
    aJavaObject:= TComJavaObject(ObjectList.Objects[i]);
    FreeAndNil(aJavaObject);
  end;
  ObjectList.Clear;
  for i:= 0 to ClassList.Count - 1 do begin
    aObject:= ClassList.Objects[i];
    FreeAndNil(aObject);
  end;
  ClassList.Clear;
  FConfiguration.ImportCache.Clear;
  ErrorOccured:= false;
  ErrorMessage:= '';
end;

procedure TComJava1.JavaReset;
  var RealUMLForm: TFUMLForm;
      MModul: TDMUMLModule;
      RtfDiagram: TRtfdDiagram;
begin
  if Terminated then exit;
  Terminated:= true;
  TerminateTask(Processinformation);
  if assigned(myDebugger) then
    myDebugger.Terminate;

  ClearObjectsAndClasses;
  Application.ProcessMessages;
  Sleep(20);
  if assigned(UMLForm) and (UMLForm is TFUMLForm) then begin
    RealUMLForm:= UMLForm as TFUMLForm;
    MModul:= RealUMLForm.MainModul;
    if assigned(MModul) then begin
      RtfDiagram:= MModul.Diagram as TRtfdDiagram;
      RtfDiagram.DeleteObjects;
    end;
  end;
end;

procedure TComJava1.JavaTerminated;
begin
  ErrorOccured:= true;
  ErrorMessage:= 'Java terminated';
  JavaReset;
end;

procedure TComJava1.setActiveComJava(aComJava: TComJava1);
begin
  activeComJava:= aComJava;
end;

function TComJava1.getFirstComJava: TComJava1;
begin
  Result:= firstComJava;
end;


initialization
  activeComJava:= nil;
  firstComJava:= nil;

end.
