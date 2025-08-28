unit UComJava1;

interface

uses
  Windows,
  Classes,
  Forms,
  ExtCtrls,
  Pipes,
  UJniWrapper1;

type
  TComJava1 = class
  private
    FAnswerFromJe2Java: string;
    FTimer: TTimer;
    FClassImports: TStringList;
    FClassList: TStringList;
    FFullImports: TStringList;
    FJavaStartpath: string;
    FErrorMessage: string;
    FPipeClient: TPipeClient;
    FConsolePipeClient: TPipeClient;
    FReceived: string;
    FConsoleReceived: string;
    FConnectID: string;
    FNumber: string;
    FUMLForm: TForm;
    FPath: string;
    FCp: string;
    FDebugJe2Java: Boolean;
    FDebuggerDisabled: Boolean;
    FErrorOccured: Boolean;
    FObjectList: TStringList;
    FParameterSend: Boolean;
    FProcessInformation: TProcessInformation;
    FSourcepath: string;
    FTerminated: Boolean;
    procedure OnPipeError(Sender: TObject; Pipe: HPIPE;
      PipeContext: TPipeContext; ErrorCode: Integer);
    procedure OnPipeDisconnect(Sender: TObject; Pipe: HPIPE);
    procedure OnTimeout(Sender: TObject);
    procedure ReceiveMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
    procedure ReceiveConsoleMessage(Sender: TObject; Pipe: HPIPE;
      Stream: TStream);
    function SendCommandSynchron(const Command: string): string;
    function SendCommandAsynchron(const Command: string): string;
    function SendMessageAsynchronWithPipe(Str: string): Boolean;
    function Je2JavaReady: Boolean;
    procedure SendParameter;
    procedure CalculatePathAndCp(const Command: string);
    procedure ClearObjectsAndClasses;
    procedure StartJE2Java;
    function StartJava(const Command: string): string;
    procedure JavaTerminated;
  public
    constructor Create(UMLForm: TForm; Number: Integer);
    destructor Destroy; override;
    function NewClass(AClassname: string; const Pathname: string): Boolean;
    function NewAPIClass(AClassname: string): Boolean;
    function GetClass(AClassname: string): TComJavaClass;
    function GetSignature(const Typ: string): string;
    function GetUniqueObjectName(Typ: string): string;
    function GetObject(const Objectname: string): TComJavaObject;
    procedure DeleteObject(const Objectname: string);
    function NewObject(const Objectname: string; MyClass: TComJavaClass;
      MyParams: TComJavaParams): TComJavaObject;
    function NewUnnamedObject(const Objectname: string): TComJavaObject;
    procedure AddObject(const Objectname: string; MyClass: TComJavaClass);
    procedure JavaReset;
    procedure Transfer(ClassImp, FullImp: TStringList;
      const Sourcepath: string);
    procedure WriteToJava(Str: string);
    function ExecuteCommand(const Command: string): string;
    function ExecuteCommandWithoutDebugger(const Command: string): string;
    function GetFirstComJava: TComJava1;
    procedure SetActiveComJava(ComJava: TComJava1);
    property AnswerFromJe2Java: string read FAnswerFromJe2Java write FAnswerFromJe2Java;
    property ClassList: TStringList read FClassList;
    property ErrorOccured: Boolean read FErrorOccured;
    property ObjectList: TStringList read FObjectList;
    property ProcessInformation: TProcessInformation read FProcessInformation;
    property Sourcepath: string read FSourcepath write FSourcepath;
    property Terminated: Boolean read FTerminated;
  end;

function GetComJava: TComJava1;

implementation

uses
  SysUtils,
  Messages,
  JvGnugettext,
  UStringRessources,
  UUMLForm,
  UJava,
  UDlgAbout,
  UUtils,
  UMessages,
  UConfiguration,
  UDebugger,
  UUMLModule,
  URtfdDiagram;

var
  ActiveComJava: TComJava1; // the active ComJava
  FirstComJava: TComJava1; // ComJava of Interactive

function GetComJava: TComJava1;
begin
  if not Assigned(ActiveComJava) then
    ActiveComJava := FirstComJava;
  Result := ActiveComJava;
end;

constructor TComJava1.Create(UMLForm: TForm; Number: Integer);
begin
  FReceived := '';
  FAnswerFromJe2Java := '';
  FErrorMessage := '';
  FDebuggerDisabled := False;
  FParameterSend := False;
  Self.FUMLForm := UMLForm;
  Self.FNumber := IntToStr(Number);
  FObjectList := TStringList.Create;
  FObjectList.CaseSensitive := True;
  FClassList := TStringList.Create;
  FClassList.CaseSensitive := True;
  FClassImports := TStringList.Create;
  FFullImports := TStringList.Create;
  SetLength(TrueBoolStrs, 1);
  TrueBoolStrs[0] := 'true';
  SetLength(FalseBoolStrs, 1);
  FalseBoolStrs[0] := 'false';

  // Debug Je2Java
  // set FDebugJe2Java to true to debug with two incarnations of Delphi
  // set  Application.ShowMainForm:= true; in je2java
  // start Je2Java with breakpoints
  // then start JavaEditor
  FDebugJe2Java := False;
  if FDebugJe2Java then
    FConnectID := ''
  else
    DateTimeToString(FConnectID, 'yyyymmddhhnnsszzz', Now);

  // callMethod, createObject and getVariables need asynchron communication
  // because they may have user-interaction and need to be done in a thread
  // asynchron communication is done via pipes
  // synchon communication is done with WM_CopyData which ist 100 times faster
  // as asynchron communication, which uses threads

  FPipeClient := TPipeClient.Create(nil);
  FPipeClient.OnPipeMessage := ReceiveMessage;
  FPipeClient.OnPipeError := OnPipeError;
  FPipeClient.OnPipeDisconnect := OnPipeDisconnect;
  FPipeClient.PipeName := 'je2java' + FConnectID;
  FConsolePipeClient := TPipeClient.Create(nil);
  FConsolePipeClient.OnPipeMessage := ReceiveConsoleMessage;
  FConsolePipeClient.PipeName := 'javaconsole' + FConnectID;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 3000;
  FTimer.OnTimer := OnTimeout;
  FJavaStartpath := '';
  FillChar(FProcessInformation, SizeOf(FProcessInformation), #0);
  if not Assigned(FirstComJava) then
    FirstComJava := Self;
  ActiveComJava := Self;
end;

destructor TComJava1.Destroy;
begin
  if ActiveComJava = Self then
    ActiveComJava := nil;
  ClearObjectsAndClasses;
  FreeAndNil(FClassImports);
  FreeAndNil(FFullImports);
  FreeAndNil(FClassList);
  FreeAndNil(FObjectList);
  FPipeClient.Disconnect;
  FConsolePipeClient.Disconnect;
  FreeAndNil(FPipeClient);
  FreeAndNil(FConsolePipeClient);
  FreeAndNil(FTimer);
  inherited;
end;

procedure TComJava1.SendParameter;
begin
  SendCommandSynchron('setParam'#4'0'#4 + IntToStr(FJava.Handle));
  SendCommandSynchron('setParam'#4'1'#4 + FCp);
  SendCommandSynchron('setParam'#4'2'#4 + FConfiguration.JDKFolder);
  SendCommandSynchron('setParam'#4'3'#4 + _(LNGFileNotFound));
  SendCommandSynchron('setParam'#4'4'#4 + _('Unknown object'));
  SendCommandSynchron('setParam'#4'5'#4 + _(LNGUnknownClass));
  SendCommandSynchron('setParam'#4'6'#4 +
    _('Incompatible types: found %s but %s expected'));
  SendCommandSynchron('setParam'#4'7'#4 +
    BoolToStr(FConfiguration.ObjectLowerCaseLetter));
  SendCommandSynchron('setParam'#4'8'#4 + 'jdbconn' + FNumber);
  SendCommandSynchron('setParam'#4'9'#4 + FConfiguration.LogfileExceptions);
  SendCommandSynchron('setParam'#4'10'#4 +
    BoolToStr(FConfiguration.LogfileExceptionsOK, True));
  SendCommandSynchron('setParam'#4'11'#4 + UDlgAbout.Version);
  SendCommandSynchron('setParam'#4'12'#4 +
    BoolToStr(FConfiguration.ShowObjectsWithInheritedPrivateAttributes));
  FParameterSend := True;
end;

procedure TComJava1.StartJE2Java;
var
  SecAttr: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  Je2JavaStarted: Boolean;
begin
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor := nil;
  SecAttr.bInheritHandle := False;
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(StartupInfo);
    dwFlags := STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
  end;
  FErrorOccured := False;
  FErrorMessage := '';
  var
  Str := ExtractFilePath(ParamStr(0)) + 'je2java.exe';
  if FDebugJe2Java then
    Je2JavaStarted := True
  else
    Je2JavaStarted := CreateProcess(nil, // lpApplicationName
      PChar(Str + ' ' + FConnectID), // lpCommandLine
      nil, // lpProcessAttributes
      nil, // lpThreadAttributes
      False, // bInheritHandles
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, // dwCreationFlags
      nil, // pEnvironment
      PChar(FPath), // 'C:',       // nil {PChar(Verz)  // pCurrentDirectory
      StartupInfo, // lpStartupInfo,
      FProcessInformation // lpProcessInformation
      );
  if Je2JavaStarted then
  begin
    repeat
      Sleep(20);
    until Je2JavaReady;
    SendParameter;
    SendCommandSynchron('init');
    if Left(FAnswerFromJe2Java, 4) = '-ERR' then
    begin
      FErrorMessage := Right(FAnswerFromJe2Java, 6);
      FErrorOccured := True;
    end
    else
    begin
      if FPipeClient.Connect(2000) and FConsolePipeClient.Connect(2000) then
        WriteToJava('__init__')
      else
      begin
        FErrorOccured := True;
        FErrorMessage := _('No connection to: ') + Str;
      end;
    end;
  end
  else
  begin
    FErrorOccured := True;
    FErrorMessage := SysErrorMessage(GetLastError) + ': ' + Str;
  end;
end;

function TComJava1.StartJava(const Command: string): string;
begin
  Result := '+OK';
  FTerminated := False;
  CalculatePathAndCp(Command);
  StartJE2Java;

  if FErrorOccured then
    Result := '-ERR ' + FErrorMessage
  else
  begin
    FJavaStartpath := FPath;
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

    if not FDebuggerDisabled and FJava.HasBreakpoints and not MyDebugger.Running
    then
      MyDebugger.StartDebuggerForUMLWindow(FSourcepath, FNumber);
  end;
  FJava.ImperativeUpdateMenuItems;
end;

function TComJava1.ExecuteCommand(const Command: string): string;
begin
  if (DebugHook <> 0) and (Pos('#', Command) > 0) then
    ErrorMsg('#-Error with: ' + Command);
  FAnswerFromJe2Java := '';

  if not Je2JavaReady or FDebugJe2Java and not FParameterSend then
    if Command = 'term' then
      Exit('+OK')
    else
      StartJava(Command);

  if FErrorOccured or (Left(FAnswerFromJe2Java, 4) = '-ERR') then
  begin
    if FAnswerFromJe2Java <> '' then
      FErrorMessage := Right(FAnswerFromJe2Java, 6);
    ErrorMsg(FErrorMessage);
    Result := '-ERR ' + FErrorMessage;
    JavaReset;
  end
  else
  begin
    var
    StringList := Split(#4, Command);
    try
      if (StringList[0] = 'callMethod') or (StringList[0] = 'getVariables') or
        (StringList[0] = 'createObject') or (StringList[0] = 'loadClass') then
        Result := SendCommandAsynchron(Command)
      else
        Result := SendCommandSynchron(Command);
      if Result = '+OK Java terminated' then
      begin
        JavaReset;
        FErrorOccured := False;
      end;
    finally
      FreeAndNil(StringList);
    end;
  end;
end;

function TComJava1.ExecuteCommandWithoutDebugger(const Command: string): string;
begin
  FDebuggerDisabled := True;
  Result := ExecuteCommand(Command);
  FDebuggerDisabled := False;
end;

procedure TComJava1.CalculatePathAndCp(const Command: string);
var
  AClassname: string;
begin
  var
  StringList := Split(#4, Command);
  if (StringList[0] = 'compile') or (StringList[0] = 'loadClass') then
    FPath := ExtractFilePath(StringList[2])
  else
    FPath := FJava.GetActiveTDIFormPath;
  if FPath = '' then
    FPath := '.';
  if StringList.Count <= 1 then
    AClassname := ''
  else
    AClassname := StringList[1];
  FreeAndNil(StringList);

  FCp := UnHideBlanks(FConfiguration.GetClassPathJarExpanded(FPath,
    ExtractPackageName(AClassname)));
  if Pos(FPath, FCp) = 0 then
    FCp := FPath + ';' + FCp;
  FCp := FConfiguration.TempDir + ';' + FCp;
  FCp := HideBlanks(FCp);
end;

// receive asynchron message from je2java
procedure TComJava1.ReceiveMessage(Sender: TObject; Pipe: HPIPE;
  Stream: TStream);
begin
  SetLength(FReceived, Stream.Size div 2);
  Stream.Read(FReceived[1], Stream.Size);
  if FReceived = 'Java terminated' then
    JavaTerminated;
end;

procedure TComJava1.ReceiveConsoleMessage(Sender: TObject; Pipe: HPIPE;
  Stream: TStream);
begin
  try
    SetLength(FConsoleReceived, Stream.Size div 2);
    Stream.Read(FConsoleReceived[1], Stream.Size);
    FMessages.OutputToTerminal(FConsoleReceived);
    Application.ProcessMessages;
  except
    on E: Exception do
      FConfiguration.Log('TComJava1.receiveConsoleMessage', E);
  end;
end;

function TComJava1.Je2JavaReady: Boolean;
begin
  var
  Str1 := 'TFJe2Java';
  var
  Str2 := 'FJe2Java' + FConnectID;
  Result := (FindWindow(PChar(Str1), PChar(Str2)) > 0);
end;

function TComJava1.SendMessageAsynchronWithPipe(Str: string): Boolean;
begin
  FReceived := '';
  Result := FPipeClient.Write(Str[1], Length(Str) * SizeOf(Char));
end;

function TComJava1.SendCommandSynchron(const Command: string): string;
var
  CDS: TCopyDataStruct;
  HWnd: THandle;
begin
  FAnswerFromJe2Java := '';
  CDS.cbData := Length(Command) * SizeOf(Char);
  CDS.lpData := PChar(Command);
  HWnd := FindWindow(PChar('TFJe2Java'), PChar('FJe2Java' + FConnectID));
  Windows.SendMessage(HWnd, WM_COPYDATA, FJava.Handle, LPARAM(@CDS));
  Result := FAnswerFromJe2Java;
end;

procedure TComJava1.OnTimeout(Sender: TObject);
begin
  FErrorOccured := True;
  FErrorMessage := 'Java timeout - reset Java';
end;

function TComJava1.SendCommandAsynchron(const Command: string): string;
begin
  if SendMessageAsynchronWithPipe(Command) then
  begin
    FErrorMessage := '';
    FErrorOccured := False;
    MyDebugger.ReadyForInput := True;
    FJava.ImperativeUpdateMenuItems;
    if (Pos('createObject', Command) = 1) or (Pos('loadClass', Command) = 1) or
      (Pos('getVariables', Command) = 1) then
      FTimer.Enabled := True;
    repeat
      if MyDebugger.Running then
        MyDebugger.ReadDebuggerForUMLWindow;
      Application.ProcessMessages;  // is needed
      Sleep(20);
    until (FReceived <> '') or FErrorOccured or FTerminated;
    FTimer.Enabled := False;
    MyDebugger.ReadyForInput := False;
    if FErrorOccured then
      Result := '-ERR ' + FErrorMessage
    else if FTerminated then
      Result := '+OK Java terminated'
    else
      Result := FReceived;
    FJava.ImperativeUpdateMenuItems;
  end
  else
    Result := '-ERR Sorry, Java doesn''t work. Please reset Java.';
end;

procedure TComJava1.OnPipeError(Sender: TObject; Pipe: HPIPE;
  PipeContext: TPipeContext; ErrorCode: Integer);
begin
  // this error comes with unusable jvm.dll
  if not FErrorOccured then
  begin
    FErrorOccured := True;
    var
    Key := FConfiguration.JDKFolder;
    var
    Str := Key + '\jre\bin\client\jvm.dll';
    if not FileExists(Str) then
      Str := Key + '\jre\bin\server\jvm.dll';
    FErrorMessage := 'Could not load ' + Str +
      '. Probably 64-Bit-Version.'#13#10 + SysErrorMessage(ErrorCode);
  end;
end;

procedure TComJava1.OnPipeDisconnect(Sender: TObject; Pipe: HPIPE);
begin
  FErrorOccured := True;
  FErrorMessage := 'Java disconnected';
  JavaReset;
end;

function TComJava1.GetUniqueObjectName(Typ: string): string;
begin
  Typ := WithoutArray(GetShortType(Typ));
  Result := ExecuteCommand('getNewObjectName'#4 + Typ);
end;

function TComJava1.NewClass(AClassname: string; const Pathname: string): Boolean;
begin
  Result := True;
  AClassname := WithoutGeneric(AClassname);
  var
  Int := FClassList.IndexOf(AClassname);
  if Int = -1 then
  begin
    var
    JavaClass := TComJavaClass.CreateLoadClass(AClassname, Pathname, Self);
    if JavaClass.IsValid then
      FClassList.AddObject(AClassname, JavaClass)
    else
    begin
      if not StartsWith(JavaClass.Error, 'The JVM') then
        ErrorMsg(JavaClass.Error);
      FreeAndNil(JavaClass);
      Result := False;
    end;
  end;
end;

function TComJava1.NewAPIClass(AClassname: string): Boolean;
begin
  Result := True;
  AClassname := WithoutGeneric(AClassname);
  var
  Int := FClassList.IndexOf(AClassname);
  if Int = -1 then
  begin
    var
    JavaClass := TComJavaClass.CreateClass(AClassname, Self);
    if JavaClass.IsValid then
      FClassList.AddObject(AClassname, JavaClass)
    else
    begin
      ErrorMsg(JavaClass.Error);
      FreeAndNil(JavaClass);
      Result := False;
    end;
  end;
end;

function TComJava1.GetClass(AClassname: string): TComJavaClass;
begin
  AClassname := WithoutGeneric(AClassname);
  var
  Int := FClassList.IndexOf(AClassname);
  if Int = -1 then
  begin
    var
    Str := FConfiguration.GetClasspathFromSourcepath(AClassname, FSourcepath);
    if FConfiguration.IsAPIClass(AClassname) or FConfiguration.IsInClasspath
      (AClassname, FJavaStartpath) then
      NewAPIClass(AClassname)
    else if Str <> '' then
      NewClass(AClassname, Str)
    else
    begin
      Int := FClassImports.IndexOfName(AClassname);
      if Int <> -1 then
      begin
        AClassname := FClassImports.ValueFromIndex[Int] + '.' + AClassname;
        NewAPIClass(AClassname);
      end
      else
        for var I := 0 to FFullImports.Count - 1 do
        begin
          Str := FFullImports[I] + AClassname;
          if FConfiguration.IsAPIClass(Str) or FConfiguration.IsInClasspath(Str,
            FJavaStartpath) then
          begin
            AClassname := Str;
            NewAPIClass(AClassname);
            Break;
          end;
        end;
    end;
    Int := FClassList.IndexOf(AClassname);
  end;
  if (Int > -1) and (Int < FClassList.Count) then
    Result := TComJavaClass(FClassList.Objects[Int])
  else
    Result := nil;
end;

function TComJava1.GetSignature(const Typ: string): string;
begin
  var
  JavaClass := GetClass(Typ);
  if Assigned(JavaClass) then
    Result := JavaClass.Signature
  else
    Result := '';
end;

procedure TComJava1.DeleteObject(const Objectname: string);
begin
  var
  Int := FObjectList.IndexOf(Objectname);
  if (Int > -1) and (Int < FObjectList.Count) then
  begin
    var
    AJavaObject := TComJavaObject(FObjectList.Objects[Int]);
    AJavaObject.Delete;
    FreeAndNil(AJavaObject);
    FObjectList.Delete(Int);
  end;
end;

function TComJava1.GetObject(const Objectname: string): TComJavaObject;
begin
  var
  Int := FObjectList.IndexOf(Objectname);
  if (Int >= 0) and (Int < FObjectList.Count) then
    Result := TComJavaObject(FObjectList.Objects[Int])
  else
    Result := nil;
end;

function TComJava1.NewObject(const Objectname: string; MyClass: TComJavaClass;
  MyParams: TComJavaParams): TComJavaObject;
begin
  var
  ComJavaObject := TComJavaObject.Create(Objectname, MyClass, MyParams, Self);
  if ComJavaObject.IsValid then
    FObjectList.AddObject(Objectname, ComJavaObject);
  // store of the java-objects
  Result := ComJavaObject;
end;

procedure TComJava1.AddObject(const Objectname: string; MyClass: TComJavaClass);
begin
  var
  ComJavaObject := TComJavaObject.CreateByName(Objectname, MyClass, Self);
  if ComJavaObject.IsValid then
    FObjectList.AddObject(Objectname, ComJavaObject);
  // store of the java-objects
end;

function TComJava1.NewUnnamedObject(const Objectname: string): TComJavaObject;
begin
  var
  ComJavaObject := TComJavaObject.CreateNewUnnamedObject(Objectname, Self);
  if ComJavaObject.IsValid then
    FObjectList.AddObject(Objectname, ComJavaObject);
  // store of the java-objects
  Result := ComJavaObject;
end;

procedure TComJava1.Transfer(ClassImp, FullImp: TStringList;
  const Sourcepath: string);
begin
  if Assigned(FClassImports) then
  begin
    FClassImports.Clear;
    FFullImports.Clear;
    FClassImports.Assign(ClassImp);
    FFullImports.Assign(FullImp);
  end;
  Self.FSourcepath := Sourcepath;
end;

procedure TComJava1.WriteToJava(Str: string);
begin
  FConsolePipeClient.Write(Str[1], Length(Str) * SizeOf(Char));
end;

procedure TComJava1.ClearObjectsAndClasses;
var
  JavaObject: TComJavaObject;
  AObject: TObject;
begin
  for var I := 0 to FObjectList.Count - 1 do
  begin
    JavaObject := TComJavaObject(FObjectList.Objects[I]);
    FreeAndNil(JavaObject);
  end;
  FObjectList.Clear;
  for var I := 0 to FClassList.Count - 1 do
  begin
    AObject := FClassList.Objects[I];
    FreeAndNil(AObject);
  end;
  FClassList.Clear;
  FConfiguration.ImportCache.Clear;
  FErrorOccured := False;
  FErrorMessage := '';
end;

procedure TComJava1.JavaReset;
var
  RealUMLForm: TFUMLForm;
  MModul: TDMUMLModule;
  RtfDiagram: TRtfdDiagram;
begin
  if FTerminated then
    Exit;
  FTerminated := True;
  TerminateTask(FProcessInformation);
  if Assigned(MyDebugger) then
    MyDebugger.Terminate;

  ClearObjectsAndClasses;
  Application.ProcessMessages;
  Sleep(20);
  if Assigned(FUMLForm) and (FUMLForm is TFUMLForm) then
  begin
    RealUMLForm := TFUMLForm(FUMLForm);
    MModul := RealUMLForm.MainModul;
    if Assigned(MModul) then
    begin
      RtfDiagram := TRtfdDiagram(MModul.Diagram);
      RtfDiagram.DeleteObjects;
    end;
  end;
end;

procedure TComJava1.JavaTerminated;
begin
  FErrorOccured := True;
  FErrorMessage := 'Java terminated';
  JavaReset;
end;

procedure TComJava1.SetActiveComJava(ComJava: TComJava1);
begin
  ActiveComJava := ComJava;
end;

function TComJava1.GetFirstComJava: TComJava1;
begin
  Result := FirstComJava;
end;

initialization

ActiveComJava := nil;
FirstComJava := nil;

end.
