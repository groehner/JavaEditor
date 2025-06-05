unit UDebugger;

{ command summary
  NewCommand(0, 'run');
  NewCommand(0, 'cont');
  NewCommand(0, 'trace methods 0x1');
  NweCommand(0, 'exclude jdk.*');
  NewCommand(1, s);  ToCursor
  NewCommand(1, 'stop in ' + FJavaClass + '.init')
  NewCommand(1, 'stop in ' + FJavaClass + '.main');
  NewCommand(2, 'stop at ' + Classname + ':' + line
  NewCoomand(2, 'clear ' + Classname + ':' + line
  NewCommand(2, FToCursorBreakpoint);
  NewCommand(3, 'cont');
  NewCommand(4, 'where');      stack
  NewCommand(5, 'locals');     parameters and local variables
  NewCommand(6, 'dump this');  FAttributes
  NewCommand(7, 'dump ' + s);  expanded FAttributes
  NewCommand(8, 'dump ' + s);  expanded local variables
  NewCommand(9, 'dump ' + s);  expanded watches
  NewCommand(10, 'dump ' + LBWatches.Items[i]);
  NewCommand(11, 'print ' + LBWatches.Items[i]);
  NewCommand(12, 'eval ' + EAusdruck.Text);
  NewCommand(13, 'fields ' + FJavaClass)           In native or static method
  NewCommand(14, 'dump ' + FJavaClass + '.' + s);  static variable
  NewCommand(15, 'print this');  Sequence diagram Destination
  NewCommand(16, 'print this');  Sequence diagram Source
  NewCommand(17, 'locals');   parameters for Method entered
  NewCommand(18, 'locals');   parameter renaming for objects (instance of)
  NewCommand(19, 'locals');   LifeLine renaming
  NewCommand(20, 'print ' + Name);  get print Name for LifeLine-Renaming
  NewCommand(21, 'dump this');  Attributs renaming

  0: DebuggerStart(Lines);
  1: CheckValidBreakpoints(Lines);
  2: Lines.Clear; // DeleteBreakpoints
  3: StepCont(Lines);
  4: ShowStack(Lines);
  5: ShowParameterAndLocalVariables(Lines);
  6: ShowAttributes(Lines);
  7: ShowDetailedVariables(1, Lines);
  8: ShowDetailedVariables(2, Lines);
  9: ShowDetailedVariables(3, Lines);
  10: Watch(Lines);
  11: MonitoringType(Lines);
  12: EvaluateExpression(Lines);
  13: getStaticVariables(Lines);
  14: showStaticVariable(Lines);
  15: SequenceDiagramDestination(Lines);
  16: SequenceDiagramSource(Lines);
  17: SequenceDiagramParameter(Lines);
  18: SequenceDiagramParameterRenaming(Lines);
  19: SequenceDiagramChangeLifeLine(Lines);
  20: SequenceDiagramPrintName(Lines);
  21: SequenceDiagramChangeLifeLineAttributes(Lines);
}

interface

uses
  Windows,
  System.Classes,
  ComCtrls,
  UQueue,
  UEditorForm,
  USequenceForm,
  UUtils;

const
  // constants for sub-windows
  K_Interpreter = 0;
  K_Compiler = 1;
  K_Debugger = 2;
  K_Search = 3; // 6;
  K_Messages = 4; // 7;
  K_Interactive = 5; // 8;

type

  TDebugger = class
  private
    FLinesDebugger: TStringList;
    FAttributes: TTreeNodes;
    FLocalVariables: TTreeNodes;
    FWatchedExpressions: TTreeNodes;
    FInstanceOf2Participants: TStringList; // instance of List(id=541) -> liste1
    FStatus: Integer;
    FThread: string;
    FShowDetailed: Boolean;
    FJdbReady: Boolean;
    FBreakpointHit: Boolean;
    FCommands: TQueue;
    FWatched: Integer;
    FAbort: Boolean;
    FBreakpointAtMain: Boolean;
    FToCursorBreakpoint: string;
    FEditForm: TFEditForm;
    FJavaClass: string;
    FPipeOutput: TPipeHandles;
    FPipeOutput1: TPipeHandles;
    FPipeInput1: TPipeHandles;
    FPipeOutput2: TPipeHandles;
    FPipeInput2: TPipeHandles;
    FProcessInformation, FProcessInformation1, FProcessInformation2
      : TProcessInformation;
    FLogFile: TFileStream;
    FLogFilename: string;
    FPolicyFile: string;
    FDirectory: string;
    FReadyForInput: Boolean;
    FRunning: Boolean;
    FSequenceForm: TFSequenceForm;

    procedure ExecAndWaitPipeDebugger(const Applicationname, Commandline,
      Debugger, DebuggerParameter, Dir: string; GUI, JavaFX: Boolean);
    procedure StartOfDebugging;
    function StartProcess(const Applicationname, Commandline, Dir: string;
      JavaFX: Boolean; var SecAttr: TSecurityAttributes;
      var PipeInput, APipeOutput: TPipeHandles;
      var AProcessinformation: TProcessInformation;
      var StartupInfo: TStartupInfo): Boolean;
    function StartConsoleProcess(const Applicationname, Commandline,
      Dir: string; var AProcessinformation: TProcessInformation;
      var StartupInfo: TStartupInfo): Boolean;
    procedure ReadUserProgram(APipeOutput: TPipeHandles);
    procedure ReadAndExecuteDebugger(APipeOutput: TPipeHandles);
    procedure ReadAndExecuteDebuggerUMLWindow(APipeOutput: TPipeHandles);
    function GetExitCode(AProcessinformation: TProcessInformation;
      const Info: string): DWORD;
    function ReadBufferDebugger(APipeOutput: TPipeHandles; BytesRead: DWORD)
      : TStringList;
    function ReadBufferUserprogram(APipeOutput: TPipeHandles;
      BytesRead: DWORD): string;
    procedure OpenLogFile(const Application, Debugger: string);
    procedure GrantApplet;
    procedure UnGrantApplet;
    function GetThread(Lines: TStringList): string;
    function DebuggerReady(Lines: TStringList): Boolean;
    procedure DebuggerStart(Lines: TStringList);
    function PositionOn(Str: string): Boolean;
    procedure CheckValidBreakpoints(Lines: TStringList);
    procedure ShowDetailedVariables(Section: Integer; Lines: TStringList);
    procedure ShowAttributes(Lines: TStringList);
    procedure ShowParameterAndLocalVariables(Lines: TStringList);
    procedure ShowStack(Lines: TStringList);
    procedure StepCont(Lines: TStringList);
    procedure MonitoringType(Lines: TStringList);
    procedure EvaluateExpression(Lines: TStringList);
    procedure GetStaticVariables(Lines: TStringList);
    procedure ShowStaticVariable(Lines: TStringList);

    procedure SequenceDiagramDestination(Lines: TStringList);
    procedure SequenceDiagramSource(Lines: TStringList);
    procedure SequenceDiagramParameter(Lines: TStringList);
    function SequenceDiagramLocals(Lines: TStringList): TStringList;
    procedure SequenceDiagramParameterRenaming(Lines: TStringList);
    procedure SequenceDiagramChangeLifeLine(Lines: TStringList);
    procedure SequenceDiagramPrintName(Lines: TStringList);
    procedure SequenceDiagramChangeLifeLineAttributes(Lines: TStringList);

    procedure StartSetBreakpoints;
    function InstanceOfJava(const Str: string): Boolean;
    procedure ProcessDebuggerOutput(Lines: TStringList);
    procedure ToDebugger(AStatus: Integer; Str: string);
    function NextCommand: string;
    procedure ShowAll;
    procedure Log(const Str: string);
    function FilterLog(Str: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DebugApplet(JavaProgram: string);
    procedure DebugProgram(JavaProgram, CallParameter: string;
      const Package: string; GUI, JavaFX: Boolean);
    procedure ReadDebuggerForUMLWindow;
    procedure StartDebuggerForUMLWindow(const Sourcepath, Number: string);

    procedure Watch(Lines: TStringList); overload;
    procedure Watch; overload;
    procedure NewCommand(Status: Integer; const Str: string);
    procedure SwitchDetails;
    procedure ToUserProgram(const Str: string);
    procedure CloseNotify(Sender: TObject);
    function HasBreakpoints: Boolean;
    procedure RunToCursorBreakpoint(const Str: string);
    procedure Terminate;
    property BreakpointAtMain: Boolean read FBreakpointAtMain
      write FBreakpointAtMain;
    property ReadyForInput: Boolean read FReadyForInput write FReadyForInput;
    property Running: Boolean read FRunning;
    property SequenceForm: TFSequenceForm read FSequenceForm
      write FSequenceForm;
  end;

var
  MyDebugger: TDebugger;

implementation

uses
  Forms,
  StrUtils,
  SysUtils,
  System.Threading,
  JvGnugettext,
  UStringRessources,
  UJava,
  UDlgEvaluate,
  UDlgAbout,
  UMessages,
  UJavaCommands,
  UConfiguration,
  UWatches,
  UComJava1;

constructor TDebugger.Create;
begin
  FLogFile := nil;
  FRunning := False;
  FReadyForInput := True;
  FShowDetailed := False;
  FAttributes := FMessages.TVAttributes.Items;
  FLocalVariables := FMessages.TVLocalVariables.Items;
  FWatchedExpressions := FMessages.TVWatchedExpressions.Items;
  FCommands := TQueue.Create;
  FLinesDebugger := TStringList.Create;
  FInstanceOf2Participants := TStringList.Create;
  FInstanceOf2Participants.NameValueSeparator := '#';
  FPolicyFile := GetEnvironmentVar('USERPROFILE') + '\.java.policy';
end;

destructor TDebugger.Destroy;
begin
  inherited;
  FreeAndNil(FCommands);
  FreeAndNil(FLinesDebugger);
  FreeAndNil(FInstanceOf2Participants);
end;

procedure TDebugger.DebugApplet(JavaProgram: string);
var
  Dir, Filename: string;
begin
  StartOfDebugging;
  if not Assigned(FEditForm) then
    Exit;
  JavaProgram := ExpandFileName(JavaProgram);
  Dir := ExtractFilePath(JavaProgram);
  Filename := ChangeFileExt(ExtractFileName(JavaProgram), '.html');
  FLogFilename := FDirectory + 'DebuggerProtocol.txt';
  // ToDo what is FDirectory?
  TTask.Run(
    procedure
    begin
      GrantApplet;
      ExecAndWaitPipeDebugger('', '', FConfiguration.JavaAppletviewer,
        ' -debug ' + Filename, Dir, True, False);
      UnGrantApplet;
    end);
end;

procedure TDebugger.DebugProgram(JavaProgram, CallParameter: string;
const Package: string; GUI, JavaFX: Boolean);
var
  Applicationname, CallProgram, Classpath, Params, IntAsString: string;
begin
  StartOfDebugging;
  if not Assigned(FEditForm) then
    Exit;
  JavaProgram := ExpandFileName(JavaProgram);
  FDirectory := FConfiguration.GetPackageDirectorySecure(JavaProgram, Package);
  Classpath := FConfiguration.GetClassPath(JavaProgram, Package);
  FJavaClass := ChangeFileExt(ExtractFileName(JavaProgram), '');
  if Package <> '' then
    FJavaClass := Package + '.' + FJavaClass;
  CallParameter := Trim(CallParameter + ' ' +
    FConfiguration.GetJavaInterpreterParameter(JavaFX));
  FMessages.StatusMessage(_('Debug') + ' ' + JavaProgram);
  FLogFilename := FDirectory + 'DebuggerProtocol.txt';

  Applicationname := FConfiguration.JavaInterpreter;
  Randomize;
  IntAsString := IntToStr(Random(100000));
  if FConfiguration.GetJavaVersion >= 5 then
    Params := ' -agentlib:jdwp='
  else
    Params := ' -Xdebug -Xrunjdwp:';
  CallProgram := AddWithSpace(FConfiguration.JavaInterpreterParameter) + Params
    + 'transport=dt_shmem,address=jdbconn' + IntAsString + ',server=y,suspend=y'
    + ' -classpath ' + Classpath + AddWithSpace(CallParameter) + ' ' +
    FJavaClass;
  if JavaFX then
  begin
    MyJavaCommands.MakeRunJavaBat(FDirectory, Applicationname,
      CallProgram, False);
    Applicationname := '';
    CallProgram := FConfiguration.TempDir + 'RunJava.bat';
  end;

  TTask.Run(
    procedure
    begin
      ExecAndWaitPipeDebugger(Applicationname, CallProgram,
        FConfiguration.JavaDebugger, ' -attach jdbconn' + IntAsString,
        FDirectory, GUI, JavaFX);
    end);
end;

procedure TDebugger.ExecAndWaitPipeDebugger(const Applicationname, Commandline,
  Debugger, DebuggerParameter, Dir: string; GUI, JavaFX: Boolean);
var
  SecAttr1, SecAttr2: TSecurityAttributes;
  StartupInfo1, StartupInfo2: TStartupInfo;
  DwExitCode2: DWORD;
  Error: Boolean;
begin
  Error := False;
  OpenLogFile(Applicationname + ' ' + Commandline,
    Debugger + ' ' + DebuggerParameter);
  try
    try
      // start debuggie
      if Applicationname + Commandline <> '' then
      begin // Applets have Applicationname + Commandline = ''
        if GUI then
          Error := not StartProcess(Applicationname, Commandline, Dir, JavaFX,
            SecAttr1, FPipeInput1, FPipeOutput1, FProcessInformation1,
            StartupInfo1)
        else
          Error := not StartConsoleProcess(Applicationname, Commandline, Dir,
            FProcessInformation1, StartupInfo1);
        Sleep(400); // give program time to start
      end;

      if not Error and StartProcess(Debugger, DebuggerParameter, Dir, False,
        SecAttr2, FPipeInput2, FPipeOutput2, FProcessInformation2, StartupInfo2)
      then
      begin
        // FPipeInput2.hRead is the handle for the input in jdb, jdb receives input via FileWrite
        FRunning := True;
        StartSetBreakpoints;
        if Assigned(SequenceForm) then
        begin
          NewCommand(0, 'exclude java.*, javax.*, sun.*, com.sun.*, jdk.*');
          NewCommand(0, 'trace methods 0x1');
        end;
        NewCommand(0, 'run');
        FJdbReady := False; // auf Bereitschaft des Debuggers warten
        repeat
          Sleep(20);
          if GUI then
            ReadUserProgram(FPipeOutput1);
          ReadAndExecuteDebugger(FPipeOutput2);
          DwExitCode2 := GetExitCode(FProcessInformation2,
            '### Debugger ExitCode');
        until (DwExitCode2 <> STILL_ACTIVE) or FAbort;
      end
      else
        ErrorMsg(SysErrorMessage(GetLastError));
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    TThread.Synchronize(nil,
      procedure
      begin
        Terminate;
      end);
  end;
end;

procedure TDebugger.StartDebuggerForUMLWindow(const Sourcepath, Number: string);
var
  SecAttr2: TSecurityAttributes;
  StartupInfo2: TStartupInfo;
begin
  StartOfDebugging;
  FDirectory := Sourcepath;
  FLogFilename := FDirectory + 'DebuggerProtocol.txt';
  OpenLogFile('', FConfiguration.JavaDebugger + ' -attach jdbconn' + Number);
  // debug externally: see function TComJava1.StartJava(...)
  // --- start debugger ------------------------------------------------------
  if StartProcess(FConfiguration.JavaDebugger, ' -attach jdbconn' + Number, '.',
    False, SecAttr2, FPipeInput2, FPipeOutput2, FProcessInformation2,
    StartupInfo2) then
  begin
    FRunning := True;
    StartSetBreakpoints;
    if Assigned(SequenceForm) then
    begin
      NewCommand(0, 'exclude java.*, javax.*, sun.*, com.sun.*, jdk.*');
      NewCommand(0, 'trace methods');
    end;
    FJdbReady := False; // auf Bereitschaft des Debuggers warten
    ReadAndExecuteDebuggerUMLWindow(FPipeOutput2);
    MyDebugger.FReadyForInput := True;
    FJava.ImperativeUpdateMenuItems;
  end
  else
    ErrorMsg(SysErrorMessage(GetLastError));
end;

function TDebugger.StartProcess(const Applicationname, Commandline, Dir: string;
JavaFX: Boolean; var SecAttr: TSecurityAttributes;
var PipeInput, APipeOutput: TPipeHandles;
var AProcessinformation: TProcessInformation;
var StartupInfo: TStartupInfo): Boolean;
var
  CurrentDir: string;
  App: PChar;
begin
  SetEnvironmentVar('JAVA_TOOL_OPTIONS', '-Duser.language=en');
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor := nil;
  SecAttr.bInheritHandle := True;
  if not CreatePipe(PipeInput.hRead, PipeInput.hWrite, @SecAttr, BufSize) then
    ErrorMsg('Error on STDIN pipe creation: ' + SysErrorMessage(GetLastError));
  if not CreatePipe(APipeOutput.hRead, APipeOutput.hWrite, @SecAttr, BufSize)
  then
    ErrorMsg('Error on STDOUT pipe creation: ' + SysErrorMessage(GetLastError));
  FillChar(AProcessinformation, SizeOf(AProcessinformation), 0);
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(StartupInfo);
    dwFlags := STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
    hStdInput := PipeInput.hRead; // Handle for input in user program,
    hStdOutput := APipeOutput.hWrite;
    hStdError := APipeOutput.hWrite;
    wShowWindow := SW_HIDE;
  end;
  CurrentDir := Dir;
  if (CurrentDir = '') or not DirectoryExists(CurrentDir) then
    if Applicationname <> '' then
      CurrentDir := ExtractFilePath(Applicationname)
    else
      CurrentDir := ExtractFilePath(Commandline);
  if Applicationname = '' then
    App := nil
  else
    App := PChar(Applicationname);
  Result := CreateProcess(App, // lpApplicationName
  PChar(Commandline), // lpCommandLine
  nil, // lpProcessAttributes
  nil, // lpThreadAttributes
  True, // bInheritHandles
  // dwCreationFlags
  IDLE_PRIORITY_CLASS or CREATE_NEW_CONSOLE, nil, // pEnvironment
  PChar(CurrentDir), // pCurrentDirectory
  StartupInfo, // lpStartupInfo,
  AProcessinformation); // lpProcessInformation
end;

function TDebugger.StartConsoleProcess(const Applicationname, Commandline,
  Dir: string; var AProcessinformation: TProcessInformation;
var StartupInfo: TStartupInfo): Boolean;
var
  CurrentDir: string;
  App: PChar;
begin
  FillChar(AProcessinformation, SizeOf(AProcessinformation), 0);
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do
  begin
    cb := SizeOf(StartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_SHOW;
  end;
  CurrentDir := Dir;
  if (CurrentDir = '') or not DirectoryExists(CurrentDir) then
    if Applicationname <> '' then
      CurrentDir := ExtractFilePath(Applicationname)
    else
      CurrentDir := ExtractFilePath(Commandline);
  if Applicationname = '' then
    App := nil
  else
    App := PChar(Applicationname);
  Result := CreateProcess(App, // lpApplicationName
  PChar(Commandline), // lpCommandLine
  nil, // lpProcessAttributes
  nil, // lpThreadAttributes
  True, // bInheritHandles
  // dwCreationFlags
  IDLE_PRIORITY_CLASS or CREATE_NEW_CONSOLE, nil, // pEnvironment
  PChar(CurrentDir), // pCurrentDirectory
  StartupInfo, // lpStartupInfo,
  AProcessinformation); // lpProcessInformation
end;

function TDebugger.GetExitCode(AProcessinformation: TProcessInformation;
const Info: string): DWORD;
begin
  WaitForSingleObject(AProcessinformation.hProcess, 0);
  GetExitCodeProcess(AProcessinformation.hProcess, Result);
  if Result <> STILL_ACTIVE then
    Log(Info + ' = ' + IntToStr(Result));
end;

procedure TDebugger.ReadUserProgram(APipeOutput: TPipeHandles);
var
  OutputUserProgram: string;
  BytesRead: DWORD;
begin
  if PeekNamedPipe(APipeOutput.hRead, nil, 0, nil, @BytesRead, nil) and
    (BytesRead > 0) then
  begin
    OutputUserProgram := ReadBufferUserprogram(APipeOutput, BytesRead);
    if (Pos('Listening for transport', OutputUserProgram) = 0) and
      (Pos('Picked up JAVA_TOOL_OPTIONS: -Duser.language=en',
      OutputUserProgram) = 0) then
      TThread.Synchronize(nil,
        procedure
        begin
          FMessages.OutputToTerminal(OutputUserProgram);
          FMessages.ShowTab(K_Interpreter);
        end);
    Log('### Console');
    Log(OutputUserProgram);
    if Pos('Exception in thread', OutputUserProgram) > 0 then
    begin
      var
      StringList := TStringList.Create;
      StringList.Text := OutputUserProgram;
      while (StringList.Count > 0) and
        (Pos('Exception in thread', StringList[0]) = 0) do
        StringList.Delete(0);
      ErrorMsg(StringList.Text);
      FAbort := True;
      FreeAndNil(StringList);
    end;
  end;
end;

procedure TDebugger.ReadAndExecuteDebugger(APipeOutput: TPipeHandles);
var
  BytesRead: DWORD;
begin
  if PeekNamedPipe(APipeOutput.hRead, nil, 0, nil, @BytesRead, nil) and
    (BytesRead > 0) then
  begin
    FLinesDebugger := ReadBufferDebugger(APipeOutput, BytesRead);
    FJdbReady := DebuggerReady(FLinesDebugger);
    if FJdbReady then
      ProcessDebuggerOutput(FLinesDebugger);
  end;
  if FJdbReady then
    if FCommands.Empty then
    begin
      if not FReadyForInput then
      begin
        FReadyForInput := True;
        TThread.Synchronize(nil,
          procedure
          begin
            FJava.UpdateMenuItems(Self);
          end);
      end;
    end
    else
    begin
      FReadyForInput := False;
      NextCommand;
      FJdbReady := False;
    end;
end;

procedure TDebugger.ReadAndExecuteDebuggerUMLWindow(APipeOutput: TPipeHandles);
var
  DwExitCode2, BytesRead: DWORD;
  HasAnswer: Boolean;
begin
  HasAnswer := False;
  repeat
    Sleep(20);
    if PeekNamedPipe(APipeOutput.hRead, nil, 0, nil, @BytesRead, nil) and
      (BytesRead > 0) then
    begin
      FLinesDebugger := ReadBufferDebugger(APipeOutput, BytesRead);
      FJdbReady := DebuggerReady(FLinesDebugger);
      if FJdbReady then
        ProcessDebuggerOutput(FLinesDebugger);
    end;
    if FJdbReady then
      if FCommands.Empty then
        HasAnswer := True // got answer of last command
      else
      begin
        NextCommand;
        FJdbReady := False;
      end;
    DwExitCode2 := GetExitCode(FProcessInformation2, '### Debugger ExitCode');
  until (DwExitCode2 <> STILL_ACTIVE) or HasAnswer;
end;

procedure TDebugger.ToUserProgram(const Str: string);
var
  Count: DWORD;
  Enc: TEncoding;
  Buf: TBytes;
begin
  Enc := TEncoding.GetEncoding(1252);
  Buf := Enc.GetBytes(Str + #13#10);
  WriteFile(FPipeInput1.hWrite, Buf[0], Length(Buf), Count, nil);
  FreeAndNil(Enc);
end;

function TDebugger.ReadBufferDebugger(APipeOutput: TPipeHandles;
BytesRead: DWORD): TStringList;
var
  Str: string;
  Ansi: AnsiString;
begin
  SetLength(Ansi, BytesRead);
  ReadFile(APipeOutput.hRead, Ansi[1], BytesRead, BytesRead, nil);
  for var I := 1 to BytesRead do
    if (Ansi[I] = #9) or (Ansi[I] = #0) then
      Ansi[I] := #32;
  Str := string(Ansi);
  FLinesDebugger.Text := FLinesDebugger.Text + Str;
  Log('<<< FROM DEBUGGER ----- ' + #$0D#$0A + Str + #$0D#$0A +
    '### END OF DEBUGGER' + #$0D#$0A);
  Result := FLinesDebugger;
end;

function TDebugger.ReadBufferUserprogram(APipeOutput: TPipeHandles;
BytesRead: DWORD): string;
var
  Str: string;
  Ansi: AnsiString;
begin
  SetLength(Ansi, BytesRead);
  ReadFile(APipeOutput.hRead, Ansi[1], BytesRead, BytesRead, nil);
  for var I := 1 to BytesRead do
    if (Ansi[I] = #9) or (Ansi[I] = #0) then
      Ansi[I] := #32;
  Str := string(Ansi);
  if EndsWith(Str, #10) then
    Delete(Str, Length(Str), 1);
  if EndsWith(Str, #13) then
    Delete(Str, Length(Str), 1);
  if Pos('beliebige Taste', Str) > 0 then
    var Int := 5; // ToDo Umlaut problem
  Result := Str;
end;

procedure TDebugger.ToDebugger(AStatus: Integer; Str: string);
var
  Count: DWORD;
  Enc: TEncoding;
  Buf: TBytes;
begin
  Log(#$0D#$0A + '>>> TO DEBUGGER Status:' + IntToStr(AStatus) +
    ',  Command: ' + Str);
  Self.FStatus := AStatus;
  Enc := TEncoding.GetEncoding(1252);
  Buf := Enc.GetBytes(Str + #13#10);
  WriteFile(FPipeInput2.hWrite, Buf[0], Length(Buf), Count, nil);
  FreeAndNil(Enc);
end;

procedure TDebugger.OpenLogFile(const Application, Debugger: string);
begin
  if FConfiguration.DebuggerProtocol then
    try
      FreeAndNil(FLogFile);
      FJava.CloseFile(FLogFilename);
      FLogFile := TFileStream.Create(FLogFilename,
        fmCreate or fmShareExclusive);
      StreamWriteln(FLogFile, 'Windows: ' + TOSVersion.ToString);
      StreamWriteln(FLogFile, 'Java-Editor: ' + UDlgAbout.Version + #$D#$A);
      StreamWriteln(FLogFile, '----- Programcall');
      StreamWriteln(FLogFile, Application);
      StreamWriteln(FLogFile, '');
      StreamWriteln(FLogFile, '----- Debuggercall');
      StreamWriteln(FLogFile, Debugger);
      StreamWriteln(FLogFile, '');
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
end;

procedure TDebugger.ReadDebuggerForUMLWindow;
begin
  ReadAndExecuteDebuggerUMLWindow(FPipeOutput2);
end;

procedure TDebugger.StartOfDebugging;
begin
  FEditForm := FJava.GetActiveEditor;
  FMessages.ClearStack;
  FAttributes.Clear;
  FLocalVariables.Clear;
  FLinesDebugger.Clear;
  for var I := 1 to 3 do
    FMessages.Expanded[I].Clear;
  FMessages.ShowIt;
  FMessages.DeleteTab(K_Interpreter);
  FMessages.ShowTab(K_Debugger);
  FMessages.StatusMessage(_('Starting debugger, please wait...'));
  FJava.CompileButtonToStop(True);
  FAbort := False;
  FReadyForInput := False;
  FStatus := 0;
  FInstanceOf2Participants.Clear;
end;

procedure TDebugger.Terminate;
begin
  FReadyForInput := True;
  if not FRunning then
    Exit;
  FAttributes.Clear;
  FLocalVariables.Clear;
  FCommands.Clear;
  FRunning := False;
  FAbort := True;
  Application.ProcessMessages;
  Sleep(20);
  var
  ComJava := GetComJava;
  if Assigned(ComJava) and (ComJava.ProcessInformation.dwProcessId <> 0) then
    ComJava.JavaReset;
  TerminateTask(FProcessInformation);
  TerminateTask(FProcessInformation1);
  TerminateTask(FProcessInformation2);
  ClosePipeHandles(FPipeOutput);
  ClosePipeHandles(FPipeInput1);
  ClosePipeHandles(FPipeOutput1);
  ClosePipeHandles(FPipeInput2);
  ClosePipeHandles(FPipeOutput2);
  FBreakpointAtMain := False;
  FJava.CompileButtonToStop(False);
  FJava.RunButtonToStop(False);
  MyJavaCommands.ProcessRunning := False;
  FreeAndNil(FLogFile);
  if FConfiguration.DebuggerProtocol then
    FJava.Open(FLogFilename);
  if Assigned(FEditForm) then
    FEditForm.DeleteDebuglineMark;
  if Assigned(FMessages) then
  begin
    FMessages.ClearStack;
    FMessages.ShowWatchedExpressions;
    FMessages.StatusMessage(_('Debugger finished.'));
  end;
end;

procedure TDebugger.GrantApplet;
const
  GrantPermission = 'grant {permission java.security.AllPermission;};';
begin
  var
  StringList := TStringList.Create;
  try
    try
      if FileExists(FPolicyFile) then
        StringList.LoadFromFile(FPolicyFile);
      if StringList.IndexOf(GrantPermission) = -1 then
        StringList.Add(GrantPermission);
      StringList.SaveToFile(FPolicyFile);
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TDebugger.UnGrantApplet;
const
  GrantPermission = 'grant {permission java.security.AllPermission;};';
begin
  if FileExists(FPolicyFile) then
  begin
    var
    StringList := TStringList.Create;
    try
      try
        StringList.LoadFromFile(FPolicyFile);
        var
        Int := StringList.IndexOf(GrantPermission);
        if Int > -1 then
          StringList.Delete(Int);
        StringList.SaveToFile(FPolicyFile);
      except
        on e: Exception do
          ErrorMsg(e.Message);
      end;
    finally
      FreeAndNil(StringList);
    end;
  end;
end;

function TDebugger.GetThread(Lines: TStringList): string;
var
  Posi: Integer;
  Str: string;
begin
  // Format
  // Breakpoint hit: "thread=AWT-EventQueue-0", MenueLeisteTest.actionPerformed(),
  // Debugging in UML-Window, answer to command "where"
  // [1] Auto.tanken (Auto.java:36)'#$D#$A'Thread-1[1] '#$D#$A
  // [1] ConAuto.main (ConAuto.java:14)'#$D#$A'main[1] '#$D#$A
  // #$D#$A'AWT-EventQueue-1[1] '#$D#$A

  Result := 'main[1]';
  Str := Lines[Lines.Count - 1];
  if (Pos('Thread', Str) = 1) or (Pos('main', Str) = 1) or
    (Pos('AWT-EventQueue', Str) = 1) or
    (Pos('JavaFX Application Thread', Str) = 1) then
  begin
    Result := Trim(Str);
    Exit;
  end;

  if Pos('Breakpoint hit:', Lines.Text) = 0 then
    Exit;
  for var I := Lines.Count - 1 downto 0 do
  begin
    Str := Lines[I];
    Posi := Pos('"thread=', Str);
    if Posi > 0 then
    begin
      Delete(Str, 1, Posi + 7);
      Posi := Pos('"', Str);
      Delete(Str, Posi, Length(Str));
      Result := Str + '[1]';
      Exit;
    end;
  end;
end;

function TDebugger.DebuggerReady(Lines: TStringList): Boolean;
begin
  Result := False;
  if Lines.Count = 0 then
    Exit;
  FThread := GetThread(Lines);
  var
  Str := Lines[Lines.Count - 1];
  Result := (Pos(FThread, Str) > 0) or (Str = '> ') or (Str = '> '#$0D#$0A);
  if (Lines.Count = 1) and StartsWith(Lines.Text, 'Set deferred breakpoint ')
  then
    Result := True;
  if Pos('NoSuchMethodError', Lines.Text) > 0 then
  begin
    ErrorMsg(_('main() method missing!'));
    Result := False;
    FAbort := True;
  end;
  if Pos('Exception occurred', Lines.Text) > 0 then
  begin
    while (Lines.Count > 0) and (Pos('Exception occurred', Lines[0]) = 0) do
      Lines.Delete(0);
    ErrorMsg(Lines.Text);
    Result := False;
    FAbort := True;
  end;
  if Pos('Exception in thread', Lines.Text) > 0 then
  begin
    while (Lines.Count > 0) and (Pos('Exception in thread', Lines[0]) = 0) do
      Lines.Delete(0);
    ErrorMsg(Lines.Text);
    Result := False;
    FAbort := True;
  end;
end;

procedure TDebugger.DebuggerStart(Lines: TStringList);
var
  Posi: Integer;
  Str: string;
begin
  if Pos('Initializing jdb', Lines.Text) > 0 then
    FMessages.StatusMessage(_('Debugger started.'));
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos('Unable to set', Str);
    if (Posi > 0) and Assigned(FEditForm) then
      FEditForm.DeleteBreakpoint(Copy(Str, Posi, Length(Str)));
  end;
end;

function TDebugger.PositionOn(Str: string): Boolean;
var
  Line, Posi, Posi1, Posi2, Posi3: Integer;
  AClassname: string;
  MethodExited: Boolean;

  function ReadDigits(Str: string): string;
  var
    Int: Integer;
  begin
    Int := 1;
    while (Int <= Length(Str)) and (('0' <= Str[Int]) and (Str[Int] <= '9') or
      (Str[Int] = '.')) or (Str[Int] = '-') do
      Inc(Int);
    Str := Copy(Str, 1, Int - 1);
    Result := ReplaceStr(Str, '.', '');
  end;

begin
  Result := False;
  Posi := Pos(', line=', Str);
  if Posi > 0 then
  begin
    MethodExited := (Pos('Method exited', Str) > 0);
    // example: in package.class.method() we need package.class
    Posi1 := Posi - 1;
    while (Posi1 > 0) and (Str[Posi1] <> '.') do
      Dec(Posi1);
    Posi2 := Posi1;
    Dec(Posi1);
    while (Posi1 > 0) and (Str[Posi1] <> ' ') do
      Dec(Posi1);
    AClassname := Copy(Str, Posi1 + 1, Posi2 - (Posi1 + 1));
    if StartsWith(AClassname, 'jdk.') or StartsWith(AClassname, 'java.') then
    begin
      NewCommand(3, 'step up');
      Exit;
    end;

    // intern class, e.g. StackCalculator$3.actionPerformed
    // don't use inner classes to open editor with classname
    Posi3 := Pos('$', AClassname);
    if Posi3 > 0 then
      AClassname := Copy(AClassname, 1, Posi3 - 1);

    Str := Copy(Str, Posi + 7, 10);
    Str := ReadDigits(Str);
    Line := StrToInt(Str);
    if Line = -1 then
      NewCommand(3, 'cont');

    if FJava.OpenWindowWithClass(FDirectory, AClassname) then
      FEditForm := FJava.GetActiveEditor
    else
    begin // no sourcecode
      NewCommand(3, 'cont');
      Exit;
    end;
    MyJavaCommands.EditForm := FEditForm;

    if not MethodExited then
      FEditForm.SetDebuglineMark(Line);
    Result := True;
  end;
end;

procedure TDebugger.CheckValidBreakpoints(Lines: TStringList);
var
  Posi: Integer;
  Str: string;
begin
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos('Unable to set', Str);
    if (Posi > 0) and Assigned(FEditForm) then
      FEditForm.DeleteBreakpoint(Copy(Str, Posi, Length(Str)));
  end;
end;

procedure TDebugger.ShowDetailedVariables(Section: Integer; Lines: TStringList);
var
  Posi, Count: Integer;
  Str, TotalVariableFix: string;
  AttVarAus: TTreeNodes;
  CurNode, NewNode: TTreeNode;

  procedure EliminateSingleCommas;
  var
    Str, Str1, Str2: string;
  begin
    Str := Lines.Text;
    Str1 := #$D#$A', ';
    Str2 := ', '#$D#$A;
    Str := AnsiReplaceStr(Str, Str1, Str2);
    Str1 := ', ';
    Str2 := ','#$D#$A;
    Str := AnsiReplaceStr(Str, Str1, Str2);
    Str1 := #$D#$A#$D#$A;
    Str2 := #$D#$A;
    Str := AnsiReplaceStr(Str, Str1, Str2);
    Lines.Text := Str;
  end;

  function getArrayVariable(Str: string): string;
  begin
    var
    Posi := Pos('.', Str);
    while Posi > 0 do
    begin
      Delete(Str, 1, Posi);
      Posi := Pos('.', Str);
    end;
    Result := Str;
  end;

  function getInstanceOf(Str: string): string;
  var
    Posi: Integer;
  begin
    Posi := Pos(')', Str);
    if Posi > 0 then
      Delete(Str, Posi + 1, Length(Str));
    Result := Str;
  end;

  function getStartNodeArray(ANode: TTreeNode; Str: string): TTreeNode;
  var
    Posi: Integer;
    Str1: string;
  begin
    Str1 := Copy(Str, 1, Pos('[', Str) - 1);
    while Assigned(ANode) and (Pos(Str1 + ' ', ANode.Text) + Pos(Str1 + ':',
      ANode.Text) <> 1) do
      ANode := ANode.getNextSibling;

    Str1 := '';
    if Assigned(ANode) then
      repeat
        ANode := ANode.getFirstChild;
        Posi := Pos('][', Str);
        if Posi > 0 then
        begin
          Str1 := Str1 + Copy(Str, 1, Posi);
          Str := Copy(Str, Posi + 1, Length(Str));
        end
        else
        begin
          Str1 := Str1 + Str;
          Str := '';
        end;
        while Assigned(ANode) and (Pos(Str1 + ':', ANode.Text) <> 1) do
          ANode := ANode.getNextSibling;
      until (Str = '') or not Assigned(ANode);
    Result := ANode;
  end;

  function getStartNode(Str: string): TTreeNode;
  var
    ANode: TTreeNode;
    Str1: string;
    Posi: Integer;
  begin
    ANode := AttVarAus.GetFirstNode;
    repeat
      Posi := Pos('.', Str);
      if Posi > 0 then
      begin
        Str1 := Copy(Str, 1, Posi - 1);
        Str := Copy(Str, Posi + 1, Length(Str));
      end
      else if Pos('[', Str) > 0 then
      begin
        Result := getStartNodeArray(ANode, Str);
        Exit;
      end
      else
      begin
        Str1 := Str;
        Str := '';
      end;
      while Assigned(ANode) and (Pos(Str1 + ' ', ANode.Text) + Pos(Str1 + ':',
        ANode.Text) <> 1) do
        ANode := ANode.getNextSibling;
      if (Str <> '') and Assigned(ANode) then
        ANode := ANode.getFirstChild;
    until (Str = '') or not Assigned(ANode);
    Result := ANode;
  end;

begin
  Str := Trim(Lines[0]);
  Posi := Pos(' = {', Str);
  Count := 0;
  if Posi > 0 then
  begin
    TotalVariableFix := Copy(Str, 1, Posi - 1);
    case Section of
      1:
        AttVarAus := FAttributes;
      2:
        AttVarAus := FLocalVariables;
      3:
        AttVarAus := FWatchedExpressions;
    end;
    AttVarAus.BeginUpdate;
    EliminateSingleCommas;
    CurNode := getStartNode(TotalVariableFix);
    if Assigned(CurNode) then
    begin
      CurNode.DeleteChildren;
      for var J := 1 to Lines.Count - 1 do
      begin
        Str := Trim(Lines[J]);
        if (Str = '}') or (Pos('Internal exception', Str) > 0) then
          Break;
        if Str <> '' then
        begin
          if Pos('instance of ', Str) = 1 then
          begin
            Str := getArrayVariable(TotalVariableFix) + '[' + IntToStr(Count) +
              ']: ' + getInstanceOf(Str);
            NewNode := AttVarAus.AddChild(CurNode, Str);
            AttVarAus.AddChild(NewNode, '_' + Str);
            Inc(Count);
          end
          else if Pos('instance of ', Str) > 1 then
          begin
            NewNode := AttVarAus.AddChild(CurNode, Str);
            AttVarAus.AddChild(NewNode, '_' + Str);
          end
          else
            AttVarAus.AddChild(CurNode, Str);
        end;
      end;
      FMessages.SetDumpActive(True);
      CurNode.Expand(False);
      FMessages.SetDumpActive(False);
    end;
    AttVarAus.EndUpdate;
  end;
end;

procedure TDebugger.ShowAttributes(Lines: TStringList);
var
  Posi: Integer;
  Str: string;
  Node: TTreeNode;

  function IsJava(const Str: string): Boolean;
  begin
    Result := (Pos('java.', Str) = 1) or (Pos('javax.', Str) = 1) or
      (Pos('javafx.', Str) = 1);
  end;

begin
  FAttributes.Clear;
  if (Pos('In native or static method', Lines.Text) > 0) and
    (Pos('this = null', Lines.Text) > 0) then
    NewCommand(13, 'fields ' + FJavaClass)
  else
    for var I := 0 to Lines.Count - 1 do
    begin
      Str := Trim(Lines[I]);
      if (Pos('}', Str) > 0) or (Pos('No current thread', Str) > 0) then
        Break;
      if (Str = '') or (Pos('this =', Str) > 0) or (Pos(FThread, Str) > 0) or
        (Pos('ParseException', Str) > 0) then
        Continue;
      if not FShowDetailed and InstanceOfJava(Str) or IsJava(Str) then
        Continue;
      Posi := Pos(': ', Str);
      if Posi > 0 then
        Str := Copy(Str, 1, Posi - 1) + ' = ' + Copy(Str, Posi + 2,
          Length(Str));
      Node := FAttributes.Add(nil, Str);
      if Pos('instance of', Str) > 0 then
        FAttributes.AddChild(Node, 'dummy');
    end;
end;

procedure TDebugger.ShowParameterAndLocalVariables(Lines: TStringList);
var
  Str: string;
  Node: TTreeNode;
begin
  FLocalVariables.Clear;
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    if (Pos(' = ', Str) > 0) and (not InstanceOfJava(Str) or FShowDetailed) then
    begin
      Node := FLocalVariables.Add(nil, Str);
      if Pos('instance of', Str) > 0 then
        FLocalVariables.AddChild(Node, 'dummy');
    end;
  end;
end;

procedure TDebugger.ShowStack(Lines: TStringList);

  function IsJavaInStack(const Str: string): Boolean;
  begin
    Result := (Pos('] java.', Str) > 0) or (Pos('] javax.', Str) > 0) or
      (Pos('javafx.', Str) > 0) or (Pos('] com.sun.', Str) > 0);
  end;

begin
  FMessages.ClearStack;
  for var I := 0 to Lines.Count - 1 do
  begin
    var
    Str := Trim(Lines[I]);
    if (Str <> '') and (Pos(FThread, Str) = 0) and (Str[1] = '[') and
      (FShowDetailed or not IsJavaInStack(Str)) then
      FMessages.LBStack.Items.Add(Str);
  end;
end;

procedure TDebugger.StepCont(Lines: TStringList);
var
  Str, Str1, Str2: string;
begin
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    if Pos('line=', Str) > 0 then
    begin
      if PositionOn(Str) then
      begin
        Application.BringToFront;
        ShowAll;
        if FToCursorBreakpoint <> '' then
        begin
          NewCommand(2, FToCursorBreakpoint);
          FToCursorBreakpoint := '';
        end;
        FBreakpointHit := True;
      end
      else
        FBreakpointHit := False;
    end;
    if Assigned(SequenceForm) then
    begin
      if Pos('Method entered', Str) > 0 then
      begin
        Str1 := Lines[I + 1];
        if Pos('Breakpoint hit:', Str1) > 0 then
          Str2 := FilterLog(Copy(Str1, 16, Length(Str1)))
        else if Str = 'Method entered: ' then
          Str2 := FilterLog(Str1)
        else
          Str2 := FilterLog(Str);
        if Pos('JEClassLoader.loadClass', Str2) = 0 then
        begin
          SequenceForm.MethodEntered(Str2);
          if FConfiguration.SDShowParameter then
            NewCommand(17, 'locals');
          NewCommand(0, 'up');
          NewCommand(16, 'print this');
          NewCommand(0, 'down');
          NewCommand(15, 'print this');
        end
        else
          NewCommand(0, 'run'); // added via else
      end;
      if Pos('Method exited', Str) > 0 then
      begin
        if Pos('JEClassLoader.loadClass', Str) = 0 then
        begin
          SequenceForm.MethodExited(FilterLog(Str));
          NewCommand(16, 'print this');
          NewCommand(0, 'up');
          NewCommand(15, 'print this');
          NewCommand(0, 'down');
          NewCommand(0, 'next');
          NewCommand(18, 'locals');
          NewCommand(0, 'step');
          if Pos('.<init>()', Str) > 0 then
          begin
            NewCommand(19, 'locals');
            NewCommand(21, 'dump this');
          end;
        end
        else
          NewCommand(0, 'run');
      end;
    end;
  end;
end;

procedure TDebugger.Watch(Lines: TStringList);
var
  Posi: Integer;
  Variable, Str: string;
  Node: TTreeNode;
begin
  if FWatched = 0 then
    FWatchedExpressions.Clear;
  Node := nil;
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos('Name unknown', Str);
    if Posi > 0 then
    begin
      Delete(Str, 1, Posi + 13);
      FWatchedExpressions.Add(nil, Str + ' = ' + _(LNGUnknown));
      Break;
    end;
    Posi := Pos('ParseException', Str);
    if Posi > 0 then
    begin
      FWatchedExpressions.Add(nil, Str);
      Break;
    end;

    if (Str = '') or (Pos(FThread, Str) > 0) then
      Continue;
    Posi := Pos(' = {', Str);
    if Posi > 0 then
    begin
      Variable := Trim(Copy(Str, 1, Posi - 1));
      Node := FWatchedExpressions.Add(nil, Variable + ' = instance of');
    end
    else if Assigned(Node) then
      FWatchedExpressions.AddChild(Node, Trim(Str))
    else
      FWatchedExpressions.Add(nil, Trim(Str));
  end;
  Inc(FWatched);
end;

procedure TDebugger.MonitoringType(Lines: TStringList);
var
  Posi: Integer;
  Variable, Typ, Str: string;
begin
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos('Name unknown', Str);
    if Posi > 0 then
      Break;
    if (Str = '') or (Pos(FThread, Str) > 0) then
      Continue;
    Posi := Pos(' = "', Str);
    if Posi > 0 then
    begin
      Variable := Trim(Copy(Str, 1, Posi - 1));
      Delete(Str, 1, Posi + 3);
      Typ := Copy(Str, 1, Pos('@', Str) - 1);
      if Typ <> '' then
        for var K := 0 to FWatchedExpressions.Count - 1 do
          if FWatchedExpressions[K].Text = Variable + ' = instance of' then
          begin
            FWatchedExpressions[K].Text := Variable + ' = instance of ' + Typ;
            Exit;
          end;
    end;

    Posi := Pos(' = ', Str);
    if Posi > 0 then
    begin
      Variable := Trim(Copy(Str, 1, Posi + 2));
      for var K := 0 to FWatchedExpressions.Count - 1 do
        if StartsWith(FWatchedExpressions[K].Text, Variable) then
        begin
          FWatchedExpressions[K].Text := Lines[I];
          Exit;
        end;
    end;
  end;
end;

procedure TDebugger.EvaluateExpression(Lines: TStringList);
var
  Posi: Integer;
  Str: string;
begin
  FEvaluate.MEvaluate.Clear;
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos('Name unknown', Str);
    if Posi > 0 then
    begin
      Delete(Str, 1, Posi + 13);
      FEvaluate.MEvaluate.Lines.Add(Str + ' = ' + _(LNGUnknown));
      Break;
    end;
    if (Str <> '') and (Str <> '> ') and (Pos(FThread, Str) = 0) then
      FEvaluate.MEvaluate.Lines.Add(Str);
  end;
end;

procedure TDebugger.GetStaticVariables(Lines: TStringList);
var
   Posi: Integer;
  Str: string;
begin
  for var I := 1 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos(' ', Str);
    if Posi > 0 then
    begin
      Delete(Str, 1, Posi);
      if Str <> '' then
        NewCommand(14, 'dump ' + FJavaClass + '.' + Str);
    end;
  end;
end;

procedure TDebugger.ShowStaticVariable(Lines: TStringList);
var
  Str: string;
  Node: TTreeNode;
  Posi: Integer;
begin
  Posi := Pos('}', Lines.Text);
  if Posi > 0 then
  begin
    Str := Trim(Copy(Lines.Text, 1, Posi));
    Str := ReplaceStr(Str, #13#10, '');
  end
  else
    Str := Trim(Lines[0]);
  if (Str = '') or (Pos('this =', Str) > 0) or (Pos(FThread, Str) > 0) or
    (Pos('ParseException', Str) > 0) or InstanceOfJava(Str) then
    Exit;
  Node := FAttributes.Add(nil, Str);
  if Pos('instance of', Str) > 0 then
    FAttributes.AddChild(Node, Str);
end;

procedure TDebugger.SequenceDiagramSource(Lines: TStringList);
var
  Str, Participant: string;
  Posi: Integer;
begin
  Str := Lines.Text;
  Delete(Str, 1, Pos('this = ', Str) - 1);
  if Pos('this = null', Str) = 1 then
    Participant := 'Actor'
  else
  begin
    Posi := Pos('this = "', Str);
    if Posi > 0 then
    begin
      Delete(Str, 1, Posi + 7);
      Posi := Pos('"', Str);
      Delete(Str, Posi, Length(Str));
      Participant := Str;
    end
    else
      Participant := '<Error>';
  end;
  SequenceForm.MakeStartParticipant(Participant);
end;

procedure TDebugger.SequenceDiagramDestination(Lines: TStringList);
var
  Str, Participant: string;
  Posi: Integer;
begin
  Str := Lines.Text;
  Delete(Str, 1, Pos('this = ', Str) - 1);
  if Pos('this = null', Str) = 1 then
    Participant := 'Actor'
  else
  begin
    Posi := Pos('this = "', Str);
    if Posi > 0 then
    begin
      Delete(Str, 1, Posi + 7);
      Posi := Pos('"', Str);
      Delete(Str, Posi, Length(Str));
      Participant := Str;
    end
    else
      Participant := '<Error>';
  end;
  SequenceForm.MakeEndParticipant(Participant);
  SequenceForm.MakeConnection;
end;

procedure TDebugger.SequenceDiagramParameter(Lines: TStringList);
var
  Posi: Integer;
  Str, Parameter, Value: string;
begin
  Parameter := '';
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos(' = ', Str);
    if Posi > 0 then
    begin
      Value := Copy(Str, Posi + 3, Length(Str));
      Parameter := Parameter + Value + ', ';
    end
    else if Str = 'Local variables:' then
      Break;
  end;
  Delete(Parameter, Length(Parameter) - 1, 2);
  SequenceForm.AddParameter(Parameter);
end;

function TDebugger.SequenceDiagramLocals(Lines: TStringList): TStringList;
var
  Posi: Integer;
  Str, Name, Value: string;
begin
  Result := TStringList.Create;
  Result.NameValueSeparator := '#';
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos(' = ', Str);
    if Posi > 0 then
    begin
      Name := Copy(Str, 1, Posi - 1);
      Value := Copy(Str, Posi + 3, Length(Str));
      if Result.IndexOfName(Value) = -1 then
        Result.Add(Value + '#' + Name);
    end;
  end;
end;

procedure TDebugger.SequenceDiagramParameterRenaming(Lines: TStringList);
begin
  var
  Locals := SequenceDiagramLocals(Lines);
  if Locals.Count > 0 then
    SequenceForm.ChangeParameter(Locals);
  FreeAndNil(Locals);
end;

procedure TDebugger.SequenceDiagramPrintName(Lines: TStringList);
var
  Posi: Integer;
  Str, Name, Value: string;
begin
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos(' = ', Str);
    if Posi > 0 then
    begin
      Value := Copy(Str, Posi + 3, Length(Str));
      Value := ReplaceStr(Value, '"', '');
      Name := Copy(Str, 1, Posi - 1);
      if Pos('@', Value) > 0 then
        SequenceForm.ChangeLifeLineName(Value, Name);
    end;
  end;
end;

procedure TDebugger.SequenceDiagramChangeLifeLine(Lines: TStringList);
var
  Posi: Integer;
  Str, Name, Value: string;
begin
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos(' = ', Str);
    if Posi > 0 then
    begin
      Value := Copy(Str, Posi + 3, Length(Str));
      if Pos('instance of', Value) <> 1 then
        Continue;
      Name := Copy(Str, 1, Posi - 1);
      if FInstanceOf2Participants.IndexOfName(Value) = -1 then
      begin
        FInstanceOf2Participants.Add(Value + '#' + Name);
        NewCommand(20, 'print ' + Name);
      end;
    end;
  end;
end;

procedure TDebugger.SequenceDiagramChangeLifeLineAttributes(Lines: TStringList);
var
  Posi: Integer;
  Str, Name, Value: string;
begin
  for var I := 0 to Lines.Count - 1 do
  begin
    Str := Lines[I];
    Posi := Pos(': instance of ', Str);
    if Posi > 0 then
    begin
      Value := Copy(Str, Posi + 2, Length(Str));
      if Pos('instance of', Value) <> 1 then
        Continue;
      Name := Trim(Copy(Str, 1, Posi - 1));
      if FInstanceOf2Participants.IndexOfName(Value) = -1 then
      begin
        FInstanceOf2Participants.Add(Value + '#' + Name);
        NewCommand(20, 'print ' + Name);
      end;
    end;
  end;
end;

function TDebugger.NextCommand: string;
var
  StatusI: Integer;
  Command: string;
begin
  StatusI := FCommands.Front.Status;
  Command := FCommands.Front.Command;
  ToDebugger(StatusI, Command);
  FCommands.Remove;

  if (Command = 'cont') or (Command = 'run') or (Command = 'step') then
    FBreakpointHit := False;
  if FBreakpointHit then
    FMessages.StatusMessage(_('Breakpoint hit'))
  else
    FMessages.StatusMessage(_('Use program to hit a breakpoint.'));

  if (Command = 'cont') or (Command = 'run') then
    FMessages.DeleteDebuggingTreeViews;
  Result := Command;
end;

procedure TDebugger.StartSetBreakpoints;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FJava.SetBreakpoints;
    end);
  if BreakpointAtMain then
    if FJava.IsJavaApplet(FEditForm) then
      NewCommand(1, 'stop in ' + FJavaClass + '.init')
    else
      NewCommand(1, 'stop in ' + FJavaClass + '.main');
end;

function TDebugger.InstanceOfJava(const Str: string): Boolean;
begin
  Result := (Pos('instance of java.', Str) > 0) and
    (Pos('instance of java.util.', Str) = 0) or
    (Pos('instance of javax.', Str) > 0) or
    (Pos('instance of javafx.', Str) > 0) or
    (Pos('instance of je.JNumberField', Str) > 0) or
    (Pos('instance of je.NumberField', Str) > 0);
end;

procedure TDebugger.ProcessDebuggerOutput(Lines: TStringList);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      case FStatus of
        0:
          DebuggerStart(Lines);
        1:
          CheckValidBreakpoints(Lines);
        2:
          Lines.Clear; // DeleteBreakpoints
        3:
          ; // cont
        4:
          ShowStack(Lines);
        5:
          ShowParameterAndLocalVariables(Lines);
        6:
          ShowAttributes(Lines);
        7:
          ShowDetailedVariables(1, Lines);
        8:
          ShowDetailedVariables(2, Lines);
        9:
          ShowDetailedVariables(3, Lines);
        10:
          Watch(Lines);
        11:
          MonitoringType(Lines);
        12:
          EvaluateExpression(Lines);
        13:
          GetStaticVariables(Lines);
        14:
          ShowStaticVariable(Lines);
        15:
          SequenceDiagramDestination(Lines);
        16:
          SequenceDiagramSource(Lines);
        17:
          SequenceDiagramParameter(Lines);
        18:
          SequenceDiagramParameterRenaming(Lines);
        19:
          SequenceDiagramChangeLifeLine(Lines);
        20:
          SequenceDiagramPrintName(Lines);
        21:
          SequenceDiagramChangeLifeLineAttributes(Lines);
      end;
      StepCont(Lines);
      FStatus := 0;
      Lines.Clear;
    end);
end;

procedure TDebugger.NewCommand(Status: Integer; const Str: string);
begin
  if FCommands.Empty and FJdbReady and (Status in [1, 2])
  // set/unset breakpoint
  then
    ToDebugger(Status, Str)
  else
    FCommands.Enter(Status, Str);
end;

procedure TDebugger.Watch;
begin
  FMessages.ShowWatchedExpressions;
  if FRunning then
  begin
    FWatched := 0;
    for var I := 0 to FWatches.LBWatches.Count - 1 do
    begin
      NewCommand(10, 'dump ' + FWatches.LBWatches.Items[I]);
      NewCommand(11, 'print ' + FWatches.LBWatches.Items[I]);
    end;
  end;
end;

procedure TDebugger.ShowAll;
var
  DumpList: TStrings;

  function WithoutBrackets(Str: string): string;
  begin
    var
    Posi := Pos('{', Str);
    if Posi > 0 then
    begin
      Delete(Str, Posi, 1);
      Posi := Pos('}', Str);
      Delete(Str, Posi, 1);
    end;
    Result := Str;
  end;

  procedure Dumping(AttVarAus: Integer);
  var
    Posi: Integer;
    Str: string;
  begin
    DumpList := FMessages.Expanded[AttVarAus - 6];
    for var I := 0 to DumpList.Count - 1 do
    begin
      Str := DumpList[I];
      Posi := Pos(' | ', Str);
      if Posi > 0 then
        Str := Copy(Str, Posi + 3, Length(Str));
      Str := WithoutBrackets(Str);
      NewCommand(AttVarAus, 'dump ' + Str);
    end;
  end;

begin
  NewCommand(4, 'where');
  NewCommand(6, 'dump this');
  Dumping(7);
  NewCommand(5, 'locals');
  Dumping(8);
  FWatched := 0;
  for var I := 0 to FWatches.LBWatches.Items.Count - 1 do
  begin
    NewCommand(10, 'dump ' + FWatches.LBWatches.Items[I]);
    NewCommand(11, 'print ' + FWatches.LBWatches.Items[I]);
  end;
  Dumping(9);
  FMessages.ShowTab(K_Debugger);
end;

procedure TDebugger.SwitchDetails;
begin
  FShowDetailed := not FShowDetailed;
  if FRunning then
    ShowAll;
end;

function TDebugger.HasBreakpoints: Boolean;
begin
  Result := BreakpointAtMain or (FToCursorBreakpoint <> '');
end;

procedure TDebugger.RunToCursorBreakpoint(const Str: string);
begin
  NewCommand(1, Str);
  FToCursorBreakpoint := 'clear ' + Copy(Str, 9, Length(Str));
end;

procedure TDebugger.Log(const Str: string);
begin
  if Assigned(FLogFile) then
    StreamWriteln(FLogFile, Str);
end;

function TDebugger.FilterLog(Str: string): string;
var
  Posi, Posi2: Integer;
begin
  Posi := Pos('Method entered: ', Str);
  if Posi = 1 then
    Delete(Str, 1, 16);
  Posi := Pos('Method exited: ', Str);
  if Posi = 1 then
    Delete(Str, 1, 15);
  Posi := Pos('return value = ', Str);
  if Posi > 0 then
    Delete(Str, Posi, Length('return value = '));
  Posi := Pos('"thread=main"', Str);
  if Posi > 0 then
    Delete(Str, Posi, Length('"thread=main", '));
  Posi := Pos('"thread=Thread-0"', Str);
  if Posi > 0 then
    Delete(Str, Posi, Length('"thread=Thread-0", '));
  Posi2 := Pos(', JEClassLoader', Str);
  if Posi2 > 0 then
    Delete(Str, Posi, Posi2 - Posi + 2);
  Posi := Pos(', line=', Str);
  if Posi > 0 then
    Delete(Str, Posi, Length(Str));
  Result := Str;
end;

procedure TDebugger.CloseNotify(Sender: TObject);
begin
  FSequenceForm := nil;
end;

end.
