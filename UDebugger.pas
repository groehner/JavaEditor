unit UDebugger;

{ command summary
    NewCommand(0, 'run');
    NewCommand(0, 'cont');
    NewCommand(0, 'trace methods 0x1');
    NweCommand(0, 'exclude jdk.*');
    NewCommand(1, s);  ToCursor
    NewCommand(1, 'stop in ' + JavaClass + '.init')
    NewCommand(1, 'stop in ' + JavaClass + '.main');
    NewCommand(2, 'stop at ' + Classname + ':' + line
    NewCoomand(2, 'clear ' + Classname + ':' + line
    NewCommand(2, ToCursorBreakpoint);
    NewCommand(3, 'cont');
    NewCommand(4, 'where');      stack
    NewCommand(5, 'locals');     parameters and local variables
    NewCommand(6, 'dump this');  attributes
    NewCommand(7, 'dump ' + s);  expanded attributes
    NewCommand(8, 'dump ' + s);  expanded local variables
    NewCommand(9, 'dump ' + s);  expanded watches
    NewCommand(10, 'dump ' + LBWatches.Items[i]);
    NewCommand(11, 'print ' + LBWatches.Items[i]);
    NewCommand(12, 'eval ' + EAusdruck.Text);
    NewCommand(13, 'fields ' + JavaClass)           In native or static method
    NewCommand(14, 'dump ' + JavaClass + '.' + s);  static variable
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

uses Windows, System.Classes, ComCtrls,
     UQueue, UEditorForm, USequenceForm, UUtils;

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
     LinesDebugger: TStringList;
     Attributes: TTreeNodes;
     LocalVariables: TTreeNodes;
     WatchedExpressions: TTreeNodes;
     InstanceOf2Participants: TStringList;  // instance of List(id=541) -> liste1
     Status: integer;
     Thread: string;
   private
     ShowDetailed: Boolean;
     jdbReady: boolean;
     BreakpointHit: boolean;
     Commands: TQueue;
     Watched: Integer;
     Abort: boolean;
     ToCursorBreakpoint: string;
     EditForm: TFEditForm;
     JavaClass: string;
     PipeOutput: TPipeHandles;
     PipeOutput1: TPipeHandles;
     PipeInput1: TPipeHandles;
     PipeOutput2: TPipeHandles;
     PipeInput2: TPipeHandles;
     ProcessInformation,
     ProcessInformation1,
     ProcessInformation2: TProcessinformation;
     LogFile: TFileStream;
     LogFilename: string;
     PolicyFile: string;
     Directory: string;

     procedure ExecAndWaitPipeDebugger(const Applicationname, Commandline, Debugger, DebuggerParameter, Dir: string; GUI, JavaFX: boolean);
     procedure StartOfDebugging;
     function StartProcess(const ApplicationName, CommandLine, Dir: string; JavaFX: boolean;
           var SecAttr: TSecurityAttributes;
           var PipeInput, aPipeOutput: TPipehandles;
           var aProcessinformation: TProcessInformation;
           var StartupInfo: TStartupInfo): boolean;
     function StartConsoleProcess(const ApplicationName, CommandLine, Dir: string;
           var aProcessinformation: TProcessInformation;
           var StartupInfo: TStartupInfo): boolean;
     procedure readUserProgram(aPipeOutput: TPipeHandles);
     procedure readAndExecuteDebugger(aPipeOutput: TPipeHandles);
     procedure readAndExecuteDebuggerUMLWindow(aPipeOutput: TPipeHandles);
     function getExitCode(aProcessInformation: TProcessInformation; const Info: string): DWord;
     function ReadBufferDebugger(aPipeOutput: TPipeHandles; BytesRead: DWord): TStringList;
     function ReadBufferUserprogram(aPipeOutput: TPipeHandles; BytesRead: DWord): string;
     procedure OpenLogFile(const Application, Debugger: string);
     procedure GrantApplet;
     procedure UnGrantApplet;
     function getThread(Lines: TStringlist): string;
     function DebuggerReady(Lines: TStringlist): boolean;
     procedure DebuggerStart(Lines: TStringList);
     function PositionOn(s: string): boolean;
     procedure CheckValidBreakpoints(Lines: TStringList);
     procedure ShowDetailedVariables(Section: integer; Lines: TStringlist);
     procedure ShowAttributes(Lines: TStringlist);
     procedure ShowParameterAndLocalVariables(Lines: TStringList);
     procedure ShowStack(Lines: TStringList);
     procedure StepCont(Lines: TStringList);
     procedure MonitoringType(Lines: TStringList);
     procedure EvaluateExpression(Lines: TStringList);
     procedure getStaticVariables(Lines: TStringList);
     procedure showStaticVariable(Lines: TStringlist);

     procedure SequenceDiagramDestination(Lines: TStringlist);
     procedure SequenceDiagramSource(Lines: TStringlist);
     procedure SequenceDiagramParameter(Lines: TStringlist);
     function  SequenceDiagramLocals(Lines: TStringList): TStringList;
     procedure SequenceDiagramParameterRenaming(Lines: TStringlist);
     procedure SequenceDiagramChangeLifeLine(Lines: TStringList);
     procedure SequenceDiagramPrintName(Lines: TStringList);
     procedure SequenceDiagramChangeLifeLineAttributes(Lines: TStringList);

     procedure StartSetBreakpoints;
     function InstanceOfJava(const s: string): boolean;
     procedure ProcessDebuggerOutput(Lines: TStringList);
     procedure ToDebugger(aStatus: integer; s: string);
     function NextCommand: string;
     procedure ShowAll;
     procedure log(const s: string);
     function FilterLog(s: string): string;
  public
     BreakpointAtMain: Boolean;
     ReadyForInput: boolean;
     Running: Boolean;
     SequenceForm: TFSequenceForm;
     constructor Create;
     destructor Destroy; override;
     procedure DebugApplet(JavaProgram: string);
     procedure DebugProgram(JavaProgram, CallParameter: string; const Package: string; GUI, JavaFX: boolean);
     procedure ReadDebuggerForUMLWindow;
     procedure StartDebuggerForUMLWindow(const Sourcepath, Number: string);

     procedure Watch(Lines: TStringList); overload;
     procedure Watch; overload;
     procedure NewCommand(aStatus: integer; const s: string);
     procedure SwitchDetails;
     procedure ToUserProgram(const s: string);
     procedure CloseNotify(Sender : TObject);
     function hasBreakpoints: boolean;
     procedure RunToCursorBreakpoint(const s: string);
     procedure Terminate;
   end;

var MyDebugger: TDebugger;

implementation

uses Forms, Controls, StdCtrls, Dialogs, StrUtils, Graphics,
     SysUtils, System.Threading,
     JvGnugettext, UStringRessources,
     UJava, UDlgEvaluate, UDlgAbout, UMessages, UJavaCommands,
     UConfiguration, UWatches, UComJava1;

constructor TDebugger.Create;
begin
  LogFile:= nil;
  Running:= false;
  ReadyForInput:= true;
  ShowDetailed:= false;
  Attributes:= FMessages.TVAttributes.Items;
  LocalVariables:= FMessages.TVLocalVariables.Items;
  WatchedExpressions:= FMessages.TVWatchedExpressions.Items;
  Commands:= TQueue.Create;
  LinesDebugger:= TStringList.Create;
  InstanceOf2Participants:= TStringList.create;
  InstanceOf2Participants.NameValueSeparator:= '#';
  PolicyFile:= GetEnvironmentVar('USERPROFILE') + '\.java.policy';
end;

destructor TDebugger.Destroy;
begin
  FreeAndNil(Commands);
  FreeAndNil(LinesDebugger);
  FreeAndNil(InstanceOf2Participants);
end;

procedure TDebugger.DebugApplet(JavaProgram: string);
begin
  StartOfDebugging;
  if EditForm = nil then exit;
  JavaProgram:= ExpandFileName(JavaProgram);
  var Dir:= ExtractFilePath(JavaProgram);
  var Filename:= ChangeFileExt(ExtractFilename(JavaProgram), '.html');
  LogFilename:= Directory + 'DebuggerProtocol.txt';   // ToDo what is Directory?
  TTask.Run(
    procedure begin
      GrantApplet;
      ExecAndWaitPipeDebugger('', '', FConfiguration.JavaAppletviewer, ' -debug ' + Filename, Dir, true, false);
      UnGrantApplet;
    end);
end;

procedure TDebugger.DebugProgram(JavaProgram, CallParameter: string;
                                 const Package: string; GUI, JavaFX: Boolean);
  var ApplicationName, CallProgram, Classpath, Params, IntAsString: string;
begin
  StartOfDebugging;
  if EditForm = nil then exit;
  JavaProgram:= ExpandFileName(JavaProgram);
  Directory:= FConfiguration.getPackageDirectorysecure(JavaProgram, Package);
  Classpath:= FConfiguration.getClassPath(JavaProgram, Package);
  JavaClass:= ChangeFileExt(ExtractFilename(JavaProgram), '');
  if Package <> '' then
    JavaClass:= Package + '.' + JavaClass;
  CallParameter:= trim(CallParameter + ' ' + FConfiguration.getJavaInterpreterParameter(JavaFX));
  FMessages.StatusMessage(_('Debug') + ' ' + JavaProgram);
  LogFilename:= Directory + 'DebuggerProtocol.txt';

  ApplicationName:= FConfiguration.JavaInterpreter;
  Randomize;
  IntAsString:= IntToStr(random(100000));
  if FConfiguration.getJavaVersion >= 5
    then Params:= ' -agentlib:jdwp='
    else Params:= ' -Xdebug -Xrunjdwp:';
  CallProgram:= AddWithSpace(FConfiguration.JavaInterpreterParameter) +
                Params + 'transport=dt_shmem,address=jdbconn' + IntAsString + ',server=y,suspend=y' +
                ' -classpath ' + ClassPath + AddWithSpace(CallParameter) + ' ' + JavaClass;
  if JavaFX then begin
    myJavaCommands.MakeRunJavaBat(Directory, ApplicationName, CallProgram, false);
    ApplicationName:= '';
    CallProgram:= FConfiguration.TempDir + 'RunJava.bat';
  end;

  TTask.Run(
    procedure begin
      ExecAndWaitPipeDebugger(ApplicationName, CallProgram, FConfiguration.JavaDebugger,
        ' -attach jdbconn' + IntAsString, Directory, GUI, JavaFX);
    end);
end;

procedure TDebugger.ExecAndWaitPipeDebugger(const Applicationname, Commandline,
                Debugger, DebuggerParameter, Dir: string; GUI, JavaFX: Boolean);
  var SecAttr1, SecAttr2: TSecurityAttributes;
      StartupInfo1, StartupInfo2: TStartupInfo;
      dwExitCode2: DWord;
      error: boolean;
begin
  error:= false;
  OpenLogFile(Applicationname + ' ' + Commandline, Debugger + ' ' + DebuggerParameter);
  try
    try
      // start debuggie
      if Applicationname + Commandline <> '' then begin // Applets have Applicationname + Commandline = ''
        if GUI then
          error:= not startProcess(Applicationname, Commandline, Dir, JavaFX, SecAttr1,
                             PipeInput1, PipeOutput1, ProcessInformation1, StartupInfo1)
        else
          error:= not startConsoleProcess(Applicationname, Commandline, Dir,
                                          ProcessInformation1, StartupInfo1);
        Sleep(200); // give program time to start
      end;

      if not error and startProcess(Debugger, DebuggerParameter, Dir, false, SecAttr2, PipeInput2, PipeOutput2, ProcessInformation2, StartupInfo2) then begin
        // PipeInput2.hRead is the handle for the input in jdb, jdb receives input via FileWrite
        Running:= true;
        StartSetBreakpoints;
        if assigned(Sequenceform) then begin
          NewCommand(0, 'exclude java.*, javax.*, sun.*, com.sun.*, jdk.*');
          NewCommand(0, 'trace methods 0x1');
        end;
        NewCommand(0, 'run');
        jdbReady:= false; // auf Bereitschaft des Debuggers warten
        repeat
          Sleep(20);
          if GUI then
            readUserProgram(PipeOutput1);
          readAndExecuteDebugger(PipeOutput2);
          dwExitCode2:= getExitCode(ProcessInformation2, '### Debugger ExitCode');
        until (dwExitCode2 <> STILL_ACTIVE) or Abort;
      end else
        ErrorMsg(SysErrorMessage(GetLasterror));
    except on e: Exception do
      ErrorMsg(E.Message);
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
  var SecAttr2: TSecurityAttributes;
      StartupInfo2: TStartupInfo;
begin
  StartOfDebugging;
  Directory:= SourcePath;
  LogFilename:= Directory + 'DebuggerProtocol.txt';
  OpenLogFile('', FConfiguration.JavaDebugger + ' -attach jdbconn' + Number);
  // debug externally: see function TComJava1.StartJava(...)
  // --- start debugger ------------------------------------------------------
  if startProcess(FConfiguration.JavaDebugger, ' -attach jdbconn' + Number, '.', false, SecAttr2, PipeInput2, PipeOutput2, ProcessInformation2, StartupInfo2) then begin
    Running:= true;
    StartSetBreakpoints;
    if assigned(Sequenceform) then begin
      NewCommand(0, 'exclude java.*, javax.*, sun.*, com.sun.*, jdk.*');
      NewCommand(0, 'trace methods');
    end;
    jdbReady:= false; // auf Bereitschaft des Debuggers warten
    readAndExecuteDebuggerUMLWindow(PipeOutput2);
    myDebugger.ReadyForInput:= true;
    FJava.ImperativeUpdateMenuItems;
  end else
    ErrorMsg(SysErrorMessage(GetLasterror));
end;

function TDebugger.startProcess(const ApplicationName, CommandLine, Dir: string;
           JavaFX: boolean;
           var SecAttr: TSecurityAttributes;
           var PipeInput, aPipeOutput: TPipehandles;
           var aProcessinformation: TProcessInformation;
           var StartupInfo: TStartupInfo): boolean;
  var CurrentDir: string; app: PChar;
begin
  SetEnvironmentVar('JAVA_TOOL_OPTIONS', '-Duser.language=en');
  SecAttr.nLength:= SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor:= nil;
  SecAttr.bInheritHandle:= True;
  if not CreatePipe(PipeInput.hRead, PipeInput.hWrite, @SecAttr, BufSize) then
    ErrorMsg('Error on STDIN pipe creation: ' + SysErrorMessage(GetLastError));
  if not CreatePipe(aPipeOutput.hRead, aPipeOutput.hWrite, @SecAttr, BufSize) then
    ErrorMsg('Error on STDOUT pipe creation: ' + SysErrorMessage(GetLastError));
  FillChar(aProcessinformation, SizeOf(aProcessinformation), 0);
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do begin
    cb          := SizeOf(StartupInfo);
    dwFlags     := STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
    hStdInput   := PipeInput.hRead;    // Handle for input in user program,
    hStdOutput  := aPipeOutput.hWrite;
    hStdError   := aPipeOutput.hWrite;
    wShowWindow := SW_Hide;
  end;
  CurrentDir:= Dir;
  if (CurrentDir = '') or not DirectoryExists(CurrentDir) then
    if ApplicationName <> ''
      then CurrentDir:= ExtractFilePath(ApplicationName)
      else CurrentDir:= ExtractFilePath(CommandLine);
  if ApplicationName = ''
    then app:= nil
    else app:= PChar(Applicationname);
  Result:= CreateProcess(
           app,                  // lpApplicationName
           PChar(CommandLine),   // lpCommandLine
           nil,                  // lpProcessAttributes
           nil,                  // lpThreadAttributes
           true,                 // bInheritHandles
                                 // dwCreationFlags
           IDLE_PRIORITY_CLASS or CREATE_NEW_CONSOLE,
           nil,                  // pEnvironment
           PChar(CurrentDir),    // pCurrentDirectory
           StartupInfo,          // lpStartupInfo,
           aProcessInformation); // lpProcessInformation
end;

function TDebugger.startConsoleProcess(const ApplicationName, CommandLine, Dir: string;
           var aProcessinformation: TProcessInformation;
           var StartupInfo: TStartupInfo): boolean;
  var CurrentDir: string; app: PChar;
begin
  FillChar(aProcessinformation, SizeOf(aProcessinformation), 0);
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  with StartupInfo do begin
    cb          := SizeOf(StartupInfo);
    dwFlags     := STARTF_USESHOWWINDOW;
    wShowWindow := SW_Show;
  end;
  CurrentDir:= Dir;
  if (CurrentDir = '') or not DirectoryExists(CurrentDir) then
    if ApplicationName <> ''
      then CurrentDir:= ExtractFilePath(ApplicationName)
      else CurrentDir:= ExtractFilePath(CommandLine);
  if ApplicationName = ''
    then app:= nil
    else app:= PChar(Applicationname);
  Result:= CreateProcess(
           app,                  // lpApplicationName
           PChar(CommandLine),   // lpCommandLine
           nil,                  // lpProcessAttributes
           nil,                  // lpThreadAttributes
           true,                 // bInheritHandles
                                 // dwCreationFlags
           IDLE_PRIORITY_CLASS or CREATE_NEW_CONSOLE,
           nil,                  // pEnvironment
           PChar(CurrentDir),    // pCurrentDirectory
           StartupInfo,          // lpStartupInfo,
           aProcessInformation); // lpProcessInformation
end;

function TDebugger.getExitCode(aProcessInformation: TProcessInformation; const Info: string): DWord;
begin
  WaitForSingleObject(aProcessInformation.hProcess, 0);
  GetExitCodeProcess(aProcessInformation.hProcess, Result);
  if Result <> STILL_ACTIVE then
    log(Info + ' = ' + IntToStr(Result));
end;

procedure TDebugger.readUserProgram(aPipeOutput: TPipeHandles);
  var OutputUserProgram: string; BytesRead: DWord;
begin
  if PeekNamedPipe(aPipeOutput.hRead, nil, 0, nil, @BytesRead, nil) and (BytesRead > 0) then begin
    OutputUserprogram:= ReadBufferUserprogram(aPipeOutput, BytesRead);
    if (Pos('Listening for transport', OutputUserprogram) = 0) and
       (Pos('Picked up JAVA_TOOL_OPTIONS: -Duser.language=en', OutputUserprogram) = 0)
    then
      TThread.Synchronize(nil,
        procedure
        begin
          FMessages.OutputToTerminal(OutputUserprogram);
          FMessages.ShowTab(K_Interpreter);
        end);
    log('### Console');
    log(OutputUserprogram);
    if Pos('Exception in thread', OutputUserprogram) > 0 then begin
      var SL:= TStringList.Create;
      SL.Text:= OutputUserprogram;
      while (SL.Count > 0) and (Pos('Exception in thread', SL[0]) = 0)  do
        SL.Delete(0);
      ErrorMsg(SL.Text);
      Abort:= true;
      FreeAndNil(SL);
    end;
  end;
end;

procedure TDebugger.readAndExecuteDebugger(aPipeOutput: TPipeHandles);
  var BytesRead: DWord;
begin
  if PeekNamedPipe(aPipeOutput.hRead, nil, 0, nil, @BytesRead, nil) and (BytesRead > 0) then begin
    LinesDebugger:= ReadBufferDebugger(aPipeoutput, BytesRead);
    jdbReady:= DebuggerReady(LinesDebugger);
    if jdbReady then
      ProcessDebuggerOutput(LinesDebugger);
  end;
  if jdbReady then
    if Commands.Empty then begin
      if ReadyForInput = false then begin
        ReadyForInput:= true;
        TThread.Synchronize(nil,
          procedure
          begin
            FJava.UpdateMenuItems(Self);
          end);
      end
    end else begin
      ReadyForInput:= false;
      NextCommand;
      jdbReady:= false;
    end;
end;

procedure TDebugger.readAndExecuteDebuggerUMLWindow(aPipeOutput: TPipeHandles);
  var dwExitCode2, BytesRead: DWord; hasAnswer: boolean; Command: string;
begin
  hasAnswer:= false;
  repeat
    Sleep(20);
    if PeekNamedPipe(aPipeOutput.hRead, nil, 0, nil, @BytesRead, nil) and (BytesRead > 0) then begin
      LinesDebugger:= ReadBufferDebugger(aPipeOutput, BytesRead);
      jdbReady:= DebuggerReady(LinesDebugger);
      if jdbReady then
        ProcessDebuggerOutput(LinesDebugger);
    end;
    if jdbReady then
      if Commands.Empty then
        hasAnswer:= true  // got answer of last command
      else begin
        Command:= NextCommand;
        jdbReady:= false;
      end;
    dwExitCode2:= getExitCode(ProcessInformation2, '### Debugger ExitCode');
  until (dwExitCode2 <> STILL_ACTIVE) or hasAnswer;
end;

procedure TDebugger.ToUserProgram(const s: string);
  var w: DWord; Enc: TEncoding; buf: TBytes;
begin
  Enc:= TEncoding.GetEncoding(1252);
  buf:= Enc.GetBytes(s + #13#10);
  WriteFile(PipeInput1.hWrite, buf[0], Length(buf), w, nil);
  FreeAndNil(Enc);
end;

function TDebugger.ReadBufferDebugger(aPipeOutput: TPipeHandles; BytesRead: DWord): TStringList;
  var s: string; ansi: ANSIString;
      i: integer;
begin
  SetLength(ansi, BytesRead);
  ReadFile(aPipeOutput.hRead, ansi[1], BytesRead, BytesRead, nil);
  for i:= 1 to BytesRead do
    if (ansi[i] = #9) or (ansi[i] = #0) then ansi[i]:= #32;
  s:= string(ansi);
  LinesDebugger.Text:= LinesDebugger.Text + s;
  log('<<< FROM DEBUGGER ----- ' + #$0D#$0A + s + #$0D#$0A + '### END OF DEBUGGER' + #$0D#$0A);
  Result:= LinesDebugger;
end;

function TDebugger.ReadBufferUserprogram(aPipeOutput: TPipeHandles; BytesRead: DWord): string;
  var s: string; Ansi: ANSIString;
      i: integer;
begin
  SetLength(Ansi, BytesRead);
  ReadFile(aPipeOutput.hRead, Ansi[1], BytesRead, BytesRead, nil);
  for i:= 1 to BytesRead do
    if (Ansi[i] = #9) or (Ansi[i] = #0) then Ansi[i]:= #32;
  s:= string(Ansi);
  if EndsWith(s, #10) then Delete(s, length(s), 1);
  if EndsWith(s, #13) then Delete(s, length(s), 1);
  if Pos('beliebige Taste', s)> 0  then
    i:= 5;     // ToDo Umlaut problem
  Result:= s;
end;

procedure TDebugger.ToDebugger(aStatus: integer; s: string);
  var w: DWord; Enc: TEncoding; buf: TBytes;
begin
  log(#$0D#$0A + '>>> TO DEBUGGER Status:' + IntToStr(aStatus) + ',  Command: ' + s);
  Self.Status:= aStatus;
  Enc:= TEncoding.GetEncoding(1252);
  buf:= Enc.GetBytes(s + #13#10);
  WriteFile(PipeInput2.hWrite, buf[0], Length(buf), w, nil);
  FreeAndNil(Enc);
end;

procedure TDebugger.OpenLogFile(const Application, Debugger: string);
begin
  if FConfiguration.DebuggerProtocol then
    try
      FreeAndNil(LogFile);
      FJava.CloseFile(LogFilename);
      LogFile:= TFileStream.Create(LogFilename, fmCreate or fmShareExclusive);
      StreamWriteln(LogFile, 'Windows: ' + TOSVersion.ToString);
      StreamWriteln(LogFile, 'Java-Editor: ' + UDlgAbout.Version + #$D#$A);
      StreamWriteln(LogFile, '----- Programcall' );
      StreamWriteln(LogFile, Application);
      StreamWriteln(LogFile, '');
      StreamWriteln(LogFile, '----- Debuggercall' );
      StreamWriteln(LogFile, Debugger);
      StreamWriteln(LogFile, '');
    except on e: Exception do
      ErrorMsg(E.Message);
    end;
end;

procedure TDebugger.ReadDebuggerForUMLWindow;
begin
  readAndExecuteDebuggerUMLWindow(PipeOutput2);
end;

procedure TDebugger.StartOfDebugging;
begin
  EditForm:= FJava.getActiveEditor;
  FMessages.ClearStack;
  Attributes.Clear;
  LocalVariables.Clear;
  LinesDebugger.Clear;
  for var i:= 1 to 3 do FMessages.Expanded[i].clear;
  FMessages.ShowIt;
  FMessages.DeleteTab(K_Interpreter);
  FMessages.ShowTab(K_Debugger);
  FMessages.StatusMessage(_('Starting debugger, please wait...'));
  FJava.CompileButtonToStop(true);
  Abort:= false;
  ReadyForInput:= false;
  Status:= 0;
  InstanceOf2Participants.Clear;
end;

procedure TDebugger.Terminate;
begin
  ReadyForInput:= true;
  if not Running then exit;
  Attributes.Clear;
  LocalVariables.Clear;
  Commands.Clear;
  Running:= false;
  Abort:= true;
  Application.ProcessMessages;
  Sleep(20);
  var ComJava:= getComJava;
  if assigned(ComJava) and (ComJava.ProcessInformation.dwProcessID <> 0) then
    ComJava.JavaReset;
  TerminateTask(Processinformation);
  TerminateTask(Processinformation1);
  TerminateTask(Processinformation2);
  ClosePipeHandles(PipeOutput);
  ClosePipeHandles(PipeInput1);
  ClosePipeHandles(PipeOutput1);
  ClosePipeHandles(PipeInput2);
  ClosePipeHandles(PipeOutput2);
  BreakPointAtMain:= False;
  FJava.CompileButtonToStop(false);
  FJava.RunButtonToStop(false);
  myJavaCommands.ProcessRunning:= false;
  if assigned(LogFile) then
    FreeAndNil(LogFile);
  if FConfiguration.DebuggerProtocol then
    FJava.Open(LogFilename);
  if assigned(EditForm) then
    EditForm.DeleteDebuglineMark;
  if assigned(FMessages) then begin
    FMessages.ClearStack;
    FMessages.ShowWatchedExpressions;
    FMessages.StatusMessage(_('Debugger finished.'));
  end;
end;

procedure TDebugger.GrantApplet;
  const GrantPermission = 'grant {permission java.security.AllPermission;};';
begin
  var SL:= TStringList.Create;
  try
    try
      if FileExists(PolicyFile) then
        SL.LoadFromFile(PolicyFile);
      if SL.IndexOf(GrantPermission) = -1 then
        SL.Add(GrantPermission);
      SL.SaveToFile(PolicyFile);
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TDebugger.UngrantApplet;
  const GrantPermission = 'grant {permission java.security.AllPermission;};';
begin
  if FileExists(PolicyFile) then begin
    var SL:= TStringList.Create;
    try
      try
        SL.LoadFromFile(PolicyFile);
        var i:= SL.IndexOf(GrantPermission);
        if i > -1 then SL.Delete(i);
        SL.SaveToFile(PolicyFile);
      except
        on e: Exception do
          ErrorMsg(e.Message);
      end;
    finally
      FreeAndNil(SL);
    end;
  end;
end;

function TDebugger.getThread(Lines: TStringlist): string;
  var i, p: integer; s: string;
begin
  // Format
  // Breakpoint hit: "thread=AWT-EventQueue-0", MenueLeisteTest.actionPerformed(),
  // Debugging in UML-Window, answer to command "where"
  //  [1] Auto.tanken (Auto.java:36)'#$D#$A'Thread-1[1] '#$D#$A
  //  [1] ConAuto.main (ConAuto.java:14)'#$D#$A'main[1] '#$D#$A
  // #$D#$A'AWT-EventQueue-1[1] '#$D#$A

  Result:= 'main[1]';
  s:= Lines[Lines.Count-1];
  if (pos('Thread', s) = 1) or
     (pos('main', s) = 1) or
     (pos('AWT-EventQueue', s) = 1) or
     (pos('JavaFX Application Thread', s) = 1) then begin
    Result:= trim(s);
    exit;
  end;

  if Pos('Breakpoint hit:', Lines.Text) = 0 then
    exit;
  for i:= Lines.Count - 1 downto 0 do begin
    s:= Lines[i];
    p:= Pos('"thread=', s);
    if p > 0 then begin
      delete(s, 1, p+7);
      p:= Pos('"', s);
      delete(s, p, length(s));
      Result:= s + '[1]';
      Exit;
    end;
  end;
end;

function TDebugger.DebuggerReady(Lines: TStringlist): boolean;
begin
  Result:= false;
  if Lines.Count = 0 then exit;
  Thread:= getThread(Lines);
  var s:= Lines.Strings[Lines.Count-1];
  Result:= (Pos(Thread, s) > 0) or (s = '> ') or (s = '> '#$0D#$0A);
  if (Lines.Count = 1) and StartsWith(Lines.Text, 'Set deferred breakpoint ') then
    Result:= true;
  if pos('NoSuchMethodError', Lines.Text) > 0 then begin
    ErrorMsg(_('main() method missing!'));
    Result:= false;
    Abort:= true;
  end;
  if Pos('Exception occurred', Lines.Text)  > 0 then begin
    while (Lines.Count > 0) and (Pos('Exception occurred', Lines[0]) = 0)  do
      Lines.Delete(0);
    ErrorMsg(Lines.Text);
    Result:= false;
    Abort:= true;
  end;
  if Pos('Exception in thread', Lines.Text) > 0 then begin
    while (Lines.Count > 0) and (Pos('Exception in thread', Lines[0]) = 0)  do
      Lines.Delete(0);
    ErrorMsg(Lines.Text);
    Result:= false;
    Abort:= true;
  end;
end;

procedure TDebugger.DebuggerStart(Lines: TStringList);
  var i, p: Integer; s: string;
begin
  if Pos('Initializing jdb', Lines.Text) > 0 then
    FMessages.StatusMessage(_('Debugger started.'));
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos('Unable to set', s);
    if (p > 0) and assigned(EditForm) then
      EditForm.DeleteBreakPoint(Copy(s, p, length(s)));
  end;
end;

function TDebugger.PositionOn(s: string): boolean;
  var Line, p, p1, p2, q: Integer; aClassname: string;
      MethodExited: boolean;

  function ReadDigits(s: string): string;
    var i: Integer;
  begin
    i:= 1;
    while (i <= Length(s)) and (('0' <= s[i]) and (s[i] <= '9') or (s[i] = '.')) or (s[i] = '-') do
      inc(i);
    s:= Copy(s, 1, i-1);
    Result:= ReplaceStr(s, '.', '');
  end;

begin
  Result:= false;
  p:= Pos(', line=', s);
  if p > 0 then begin
    MethodExited:= (Pos('Method exited', s) > 0);
    // example: in package.class.method() we need package.class
    p1:= p - 1;
    while (p1 > 0) and (s[p1] <> '.') do
      dec(p1);
    p2:= p1;
    dec(p1);
    while (p1 > 0) and (s[p1] <> ' ') do
      dec(p1);
    aClassname:= Copy(s, p1+1, p2-(p1+1));
    if StartsWith(aClassname, 'jdk.') or StartsWith(aClassname, 'java.') then begin
      NewCommand(3, 'step up');
      exit;
    end;

    // intern class, e.g. StackCalculator$3.actionPerformed
    // don't use inner classes to open editor with classname
    q:= Pos('$', aClassname);
    if q > 0 then aClassname:= copy(aClassname, 1, q-1);

    s:= copy(s, p + 7, 10);
    s:= ReadDigits(s);
    Line:= StrToInt(s);
    if Line = -1 then
      NewCommand(3, 'cont');

    if FJava.OpenWindowWithClass(Directory, aClassname) then
      EditForm:= FJava.getActiveEditor
    else begin // no sourcecode
      NewCommand(3, 'cont');
      exit;
    end;
    myJavaCommands.EditForm:= EditForm;

    if not MethodExited then
      EditForm.SetDebugLineMark(Line);
    Result:= true;
  end;
end;

procedure TDebugger.CheckValidBreakpoints(Lines: TStringList);
  var i, p: Integer; s: string;
begin
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos('Unable to set', s);
    if (p > 0) and assigned(EditForm) then
      EditForm.DeleteBreakPoint(Copy(s, p, length(s)));
  end;
end;

procedure TDebugger.ShowDetailedVariables(Section: integer; Lines: TStringlist);
  var j, p, count: integer;
      s, TotalVariableFix: string;
      AttVarAus: TTreeNodes;
      CurNode, NewNode: TTreeNode;

  procedure EliminateSingleCommas;
    var s, s1, s2: string;
  begin
    s:= Lines.Text;
    s1:= #$D#$A', ';
    s2:= ', '#$D#$A;
    s:= ANSIReplaceStr(s, s1, s2);
    s1:= ', ';
    s2:= ','#$D#$A;
    s:= ANSIReplaceStr(s, s1, s2);
    s1:= #$D#$A#$D#$A;
    s2:= #$D#$A;
    s:= ANSIReplaceStr(s, s1, s2);
    Lines.Text:= s;
  end;

  function getArrayVariable(s: string): string;
  begin
    var p:= Pos('.', s);
    while p > 0 do begin
      delete(s, 1, p);
      p:= Pos('.', s);
    end;
    Result:= s;
  end;

  function getInstanceOf(s: string): string;
    var p: integer;
  begin
    p:= Pos(')', s);
    if p > 0 then
      delete(s, p+1, length(s));
    Result:= s;
  end;

  function getStartNodeArray(aNode: TTreeNode; s: string): TTreeNode;
    var p: integer; s1: string;
  begin
    s1:= copy(s, 1, Pos('[', s) - 1);
    while assigned(aNode) and
      (pos(s1 + ' ', aNode.Text) + pos(s1 + ':', aNode.Text) <> 1) do
      aNode:= aNode.getNextSibling;

    s1:= '';
    if assigned(aNode) then
      repeat
        aNode:= aNode.getFirstChild;
        p:= Pos('][', s);
        if p > 0 then begin
          s1:= s1 + copy(s, 1, p);
          s:= copy(s, p + 1, length(s));
        end else begin
          s1:= s1 + s;
          s:= '';
        end;
        while assigned(aNode) and (pos(s1 + ':', aNode.Text) <> 1) do
          aNode:= aNode.getNextSibling;
      until (s = '') or not assigned(aNode);
    Result:= aNode;
  end;

  function getStartNode(s: string): TTreeNode;
    var aNode: TTreeNode;
        s1: string;
        p: integer;
  begin
    aNode:= AttVarAus.GetFirstNode;
    repeat
      p:= Pos('.', s);
      if p > 0 then begin
        s1:= copy(s, 1, p-1);
        s:= copy(s, p+1, length(s));
      end else if Pos('[', s) > 0 then begin
        Result:= GetStartNodeArray(aNode, s);
        exit;
      end else begin
        s1:= s;
        s:= '';
      end;
      while assigned(aNode) and
        (pos(s1 + ' ', aNode.Text) + pos(s1 + ':', aNode.Text) <> 1) do
        aNode:= aNode.getNextSibling;
      if (s <> '') and Assigned(aNode) then aNode:= aNode.getFirstChild;
    until (s = '') or not Assigned(aNode);
    Result:= aNode;
  end;

begin
  s:= Trim(Lines[0]);
  p:= Pos(' = {', s);
  count:= 0;
  if p > 0 then begin
    TotalVariableFix:= Copy(s, 1, p-1);
    case Section of
      1: AttVarAus:= Attributes;
      2: AttVarAus:= LocalVariables;
      3: AttVarAus:= WatchedExpressions;
    end;
    AttVarAus.BeginUpdate;
    EliminateSingleCommas;
    CurNode:= getStartNode(TotalVariableFix);
    if assigned(CurNode) then begin
      CurNode.DeleteChildren;
      for j:=  1 to Lines.Count - 1 do begin
        s:= trim(Lines[j]);
        if (s = '}') or (Pos('Internal exception', s) > 0) then
          break;
        if s <> '' then begin
          if Pos('instance of ', s) = 1 then begin
            s:= getArrayVariable(TotalVariableFix) +
                '[' + IntToStr(count) + ']: ' + getInstanceOf(s);
            NewNode:= AttVarAus.AddChild(CurNode, s);
            AttVarAus.AddChild(NewNode, '_' + s);
            inc(count);
          end else if Pos('instance of ', s) > 1 then begin
            NewNode:= AttVarAus.AddChild(CurNode, s);
            AttVarAus.AddChild(NewNode, '_' + s);
          end else
            AttVarAus.AddChild(CurNode, s);
        end;
      end;
      FMessages.DumpActive:= true;
      CurNode.Expand(false);
      FMessages.DumpActive:= false;
    end;
    AttVarAus.EndUpdate;
  end;
end;

procedure TDebugger.ShowAttributes(Lines: TStringlist);
   var i, p: integer; s: string;
       Node: TTreeNode;

  function IsJava(const s: string): boolean;
  begin
    Result:= (Pos('java.', s) = 1) or (Pos('javax.', s) = 1) or (Pos('javafx.', s) = 1);
  end;

begin
  Attributes.Clear;
  if (Pos('In native or static method', Lines.Text) > 0) and (Pos('this = null', Lines.Text) > 0) then
    NewCommand(13, 'fields ' + JavaClass)
  else
    for i:= 0 to Lines.Count-1 do begin
      s:= Trim(Lines[i]);
      if (Pos('}', s) > 0) or (Pos('No current thread', s) > 0) then
        break;
      if (s = '') or (Pos('this =', s)  > 0) or (Pos(Thread, s) > 0) or
         (Pos('ParseException', s) > 0) then
        Continue;
      if not ShowDetailed and InstanceOfJava(s) or IsJava(s) then
        Continue;
      p:= Pos(': ', s);
      if p > 0 then s:= Copy(s, 1, p-1) + ' = ' + copy(s, p+2, length(s));
      Node:= Attributes.Add(nil, s);
      if Pos('instance of', s) > 0 then
        Attributes.AddChild(Node, 'dummy');
    end;
end;

procedure TDebugger.ShowParameterAndLocalVariables(Lines: TStringList);
  var i: Integer; s: string;
      Node: TTreeNode;
begin
  LocalVariables.Clear;
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    if (Pos(' = ', s) > 0) and (not InstanceOfJava(s) or ShowDetailed) then begin
      Node:= LocalVariables.Add(nil, s);
      if Pos('instance of', s) > 0 then
        LocalVariables.AddChild(Node, 'dummy');
    end;
  end;
end;

procedure TDebugger.ShowStack(Lines: TStringList);

  function IsJavaInStack(const s: string): boolean;
  begin
    Result:= (Pos('] java.', s) > 0) or (Pos('] javax.', s) > 0) or
             (Pos('javafx.', s) > 0) or (Pos('] com.sun.', s) > 0);
  end;

begin
  FMessages.ClearStack;
  for var i:= 0 to Lines.Count - 1 do begin
    var s:= Trim(Lines[i]);
    if (s <> '') and (Pos(Thread, s) = 0) and (s[1] = '[') and
       (ShowDetailed or not IsJavaInStack(s)) then
      FMessages.LBStack.Items.Add(s);
  end;
end;

procedure TDebugger.StepCont(Lines: TStringList);
  var i: Integer; s, s1, s2: string;
begin
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    if Pos('line=', s) > 0 then begin
      if PositionOn(s) then begin
        Application.BringToFront;
        ShowAll;
        if ToCursorBreakpoint <> '' then begin
          NewCommand(2, ToCursorBreakpoint);
          ToCursorBreakpoint:= ''
        end;
        BreakpointHit:= true;
      end else
        BreakpointHit:= false;
    end;
    if assigned(Sequenceform) then begin
      if Pos('Method entered', s) > 0 then begin
        s1:= Lines[i+1];
        if Pos('Breakpoint hit:', s1) > 0
          then s2:= FilterLog(copy(s1, 16, length(s1)))
        else if s = 'Method entered: '
          then s2:= FilterLog(s1)
          else s2:= FilterLog(s);
        if Pos('JEClassLoader.loadClass', s2) = 0 then begin
          Sequenceform.MethodEntered(s2);
          if FConfiguration.SDShowParameter then
            NewCommand(17, 'locals');
          NewCommand(0,  'up');
          NewCommand(16, 'print this');
          NewCommand(0,  'down');
          NewCommand(15, 'print this');
        end else
          NewCommand(0, 'run'); // added via else
      end;
      if Pos('Method exited', s) > 0 then begin
        if Pos('JEClassLoader.loadClass', s) = 0 then begin
          Sequenceform.MethodExited(FilterLog(s));
          NewCommand(16, 'print this');
          NewCommand(0,  'up');
          NewCommand(15, 'print this');
          NewCommand(0,  'down');
          NewCommand(0,  'next');
          NewCommand(18, 'locals');
          NewCommand(0,  'step');
          if Pos('.<init>()', s) > 0 then begin
            NewCommand(19, 'locals');
            NewCommand(21, 'dump this');
          end;
        end else
          NewCommand(0, 'run');
      end;
    end;
  end;
end;

procedure TDebugger.Watch(Lines: TStringList);
  var i, p: integer; Variable, s: string; Node: TTreeNode;
begin
  if Watched = 0 then
    WatchedExpressions.Clear;
  Node:= nil;
  for i:= 0 to Lines.Count-1 do begin
    s:= Lines[i];
    p:= Pos('Name unknown', s);
    if p > 0 then begin
      delete(s, 1, p + 13);
      WatchedExpressions.Add(nil, s + ' = ' + _(LNGUnknown));
      break;
    end;
    p:= Pos('ParseException', s);
    if p > 0 then begin
      WatchedExpressions.Add(nil, s);
      break;
    end;

    if (s = '') or (Pos(Thread, s) > 0) then Continue;
    p:= Pos(' = {', s);
    if p > 0 then begin
      Variable:= trim(copy(s, 1, p-1));
      Node:= WatchedExpressions.Add(nil, Variable + ' = instance of');
      end
    else if Assigned(Node) then
      WatchedExpressions.AddChild(Node, trim(s))
    else
      WatchedExpressions.Add(nil, trim(s));
  end;
  inc(Watched);
end;

procedure TDebugger.MonitoringType(Lines: TStringList);
  var i, k, p: integer; Variable, Typ, s: string;
begin
  for i:= 0 to Lines.Count-1 do begin
    s:= Lines[i];
    p:= Pos('Name unknown', s);
    if p > 0 then break;
    if (s = '') or (Pos(Thread, s) > 0) then Continue;
    p:= Pos(' = "', s);
    if p > 0 then begin
      Variable:= trim(copy(s, 1, p-1));
      delete(s, 1, p+3);
      Typ:= copy(s, 1, Pos('@', s) -1);
      if Typ <> '' then
        for k:= 0 to WatchedExpressions.Count - 1 do
          if WatchedExpressions.Item[k].Text = Variable + ' = instance of' then begin
            WatchedExpressions.Item[k].Text:= Variable + ' = instance of ' + Typ;
            exit;
          end;
    end;

    p:= Pos(' = ', s);
    if p > 0 then begin
      Variable:= trim(copy(s, 1, p+2));
      for k:= 0 to WatchedExpressions.Count - 1 do
        if StartsWith(WatchedExpressions.Item[k].Text, Variable) then begin
          WatchedExpressions.Item[k].Text:= Lines[i];
          exit;
        end;
    end;
  end;
end;

procedure TDebugger.EvaluateExpression(Lines: TStringList);
  var i, p: Integer; s: string;
begin
  FEvaluate.MEvaluate.Clear;
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos('Name unknown', s);
    if p > 0 then begin
      delete(s, 1, p + 13);
      FEvaluate.MEvaluate.Lines.Add(s + ' = ' + _(LNGUnknown));
      break;
    end;
    if (s <> '') and (s <> '> ') and (Pos(Thread, s) = 0) then
      FEvaluate.MEvaluate.Lines.Add(s);
  end;
end;

procedure TDebugger.getStaticVariables(Lines: TStringList);
  var i, p: Integer; s: string;
begin
  for i:= 1 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos(' ', s);
    if p > 0 then begin
      delete(s, 1, p);
      if s <> '' then
        NewCommand(14, 'dump ' + JavaClass + '.' + s);
    end;
  end;
end;

procedure TDebugger.showStaticVariable(Lines: TStringlist);
   var s: string; Node: TTreeNode; p: integer;
begin
  p:= Pos('}', Lines.Text);
  if p > 0 then begin
    s:= trim(copy(Lines.Text, 1, p));
    s:= ReplaceStr(s, #13#10, '')
  end else
    s:= trim(Lines[0]);
  if (s = '') or (Pos('this =', s)  > 0) or (Pos(Thread, s) > 0) or
     (Pos('ParseException', s) > 0) or InstanceOfJava(s)
  then
    exit;
  Node:= Attributes.Add(nil, s);
  if Pos('instance of', s) > 0 then
    Attributes.AddChild(Node, s);
end;

procedure TDebugger.SequenceDiagramSource(Lines: TStringlist);
   var s, participant: string; p: integer;
begin
  s:= Lines.Text;
  delete(s, 1, Pos('this = ', s)-1);
  if Pos('this = null', s) = 1
    then participant:= 'Actor'
  else begin
    p:= Pos('this = "', s);
    if p > 0 then begin
      delete(s, 1, p + 7);
      p:= Pos('"', s);
      delete(s, p, length(s));
      participant:= s;
    end else
      participant:= '<error>';
  end;
  SequenceForm.makeFromParticipant(participant);
end;

procedure TDebugger.SequenceDiagramDestination(Lines: TStringlist);
   var s, participant: string; p: integer;
begin
  s:= Lines.Text;
  delete(s, 1, Pos('this = ', s)-1);
  if Pos('this = null', s) = 1 then
    participant:= 'Actor'
  else begin
    p:= Pos('this = "', s);
    if p > 0 then begin
      delete(s, 1, p + 7);
      p:= Pos('"', s);
      delete(s, p, length(s));
      participant:= s;
    end else
      participant:= '<error>';
  end;
  SequenceForm.makeToParticipant(participant);
  Sequenceform.makeConnection;
end;

procedure TDebugger.SequenceDiagramParameter(Lines: TStringList);
  var i, p: Integer; s, Parameter, Value: string;
begin
  Parameter:= '';
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos(' = ', s);
    if p > 0 then begin
      Value:= copy(s, p+3, length(s));
      Parameter:= Parameter + Value + ', '
    end else if s = 'Local variables:' then
      break;
  end;
  Delete(Parameter, Length(Parameter)-1, 2);
  SequenceForm.addParameter(Parameter);
end;

function TDebugger.SequenceDiagramLocals(Lines: TStringList): TStringList;
  var i, p: Integer; s, Name, Value: string;
begin
  Result:= TStringList.Create;
  Result.NameValueSeparator:= '#';
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos(' = ', s);
    if p > 0 then begin
      Name:= copy(s, 1, p-1);
      Value:= copy(s, p+3, length(s));
      if Result.IndexOfName(Value) = -1 then
        Result.Add(Value + '#' + Name);
    end;
  end;
end;

procedure TDebugger.SequenceDiagramParameterRenaming(Lines: TStringList);
begin
  var Locals:= SequenceDiagramLocals(Lines);
  if Locals.Count > 0 then
    SequenceForm.changeParameter(Locals);
  FreeAndNil(Locals);
end;

procedure TDebugger.SequenceDiagramPrintName(Lines: TStringList);
  var i, p: Integer; s, Name, Value: string;
begin
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos(' = ', s);
    if p > 0 then begin
      Value:= copy(s, p+3, length(s));
      Value:= ReplaceStr(Value, '"', '');
      Name:= copy(s, 1, p-1);
      if Pos('@', Value) > 0 then
        Sequenceform.changeLifeLineName(Value, Name);
    end;
  end;
end;

procedure TDebugger.SequenceDiagramChangeLifeLine(Lines: TStringList);
  var i, p: Integer; s, Name, Value: string;
begin
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos(' = ', s);
    if p > 0 then begin
      Value:= copy(s, p+3, length(s));
      if Pos('instance of', Value) <> 1 then
        continue;
      Name:= copy(s, 1, p-1);
      if InstanceOf2Participants.IndexOfName(Value) = -1 then begin
        InstanceOf2Participants.Add(Value + '#' + Name);
        NewCommand(20, 'print ' + Name);
      end;
    end;
  end;
end;

procedure TDebugger.SequenceDiagramChangeLifeLineAttributes(Lines: TStringList);
  var i, p: Integer; s, Name, Value: string;
begin
  for i:= 0 to Lines.Count - 1 do begin
    s:= Lines[i];
    p:= Pos(': instance of ', s);
    if p > 0 then begin
      Value:= copy(s, p+2, length(s));
      if Pos('instance of', Value) <> 1 then
        continue;
      Name:= trim(copy(s, 1, p-1));
      if InstanceOf2Participants.IndexOfName(Value) = -1 then begin
        InstanceOf2Participants.Add(Value + '#' + Name);
        NewCommand(20, 'print ' + Name);
      end;
    end;
  end;
end;

function TDebugger.NextCommand: string;
  var StatusI: integer;
      Command: string;
begin
  StatusI:= Commands.Front.Status;
  Command:= Commands.Front.Command;
  ToDebugger(StatusI, Command);
  Commands.Remove;

  if (Command = 'cont') or (Command = 'run') or (Command = 'step') then
    BreakpointHit:= false;
  if BreakpointHit
    then FMessages.StatusMessage(_('Breakpoint hit'))
    else FMessages.StatusMessage(_('Use program to hit a breakpoint.'));

  if (Command = 'cont') or (Command = 'run') then
    FMessages.DeleteDebuggingTreeViews;
  Result:= Command;
end;

procedure TDebugger.StartSetBreakpoints;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FJava.SetBreakpoints;
    end);
  if BreakPointAtMain then
    if FJava.IsJavaApplet(EditForm)
      then NewCommand(1, 'stop in ' + JavaClass + '.init')
      else NewCommand(1, 'stop in ' + JavaClass + '.main');
end;

function TDebugger.InstanceOfJava(const s: string): boolean;
begin
  Result:= (Pos('instance of java.', s) > 0) and (Pos('instance of java.util.', s) = 0) or
           (Pos('instance of javax.', s) > 0) or
           (Pos('instance of javafx.', s) > 0) or
           (Pos('instance of je.JNumberField', s) > 0) or
           (Pos('instance of je.NumberField', s) > 0);
end;

procedure TDebugger.ProcessDebuggerOutput(Lines: TStringList);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      case Status of
        0: DebuggerStart(Lines);
        1: CheckValidBreakpoints(Lines);
        2: Lines.Clear; // DeleteBreakpoints
        3: ; // cont
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
      end;
      StepCont(Lines);
      Status:= 0;
      Lines.Clear;
    end);
end;

procedure TDebugger.NewCommand(aStatus: integer; const s: string);
begin
  if Commands.Empty and jdbReady and (aStatus in [1, 2]) // set/unset breakpoint
    then ToDebugger(aStatus, s)
    else Commands.Enter(aStatus, s);
end;

procedure TDebugger.Watch;
begin
  FMessages.ShowWatchedExpressions;
  if Running then begin
    Watched:= 0;
    for var i:= 0 to FWatches.LBWatches.Count - 1 do begin
      NewCommand(10, 'dump ' + FWatches.LBWatches.Items[i]);
      NewCommand(11, 'print ' + FWatches.LBWatches.Items[i]);
    end;
  end;
end;

procedure TDebugger.ShowAll;
  var i: Integer;
      DumpList: TStrings;

  function WithoutBrackets(s: string): string;
  begin
    var p:= Pos('{', s);
    if p > 0 then begin
      delete(s, p, 1);
      p:= Pos('}', s);
      delete(s, p, 1);
    end;
    Result:= s;
  end;  

  procedure Dumping(AttVarAus: integer);
    var i, p: integer; s: string;
  begin
    DumpList:= FMessages.Expanded[AttVarAus - 6];
    for i:= 0 to DumpList.Count - 1 do begin
      s:= DumpList.Strings[i];
      p:= Pos(' | ', s);
      if p > 0 then
        s:= copy(s, p+3, length(s));
      s:= WithoutBrackets(s);
      NewCommand(AttVarAus, 'dump ' + s);
    end;
  end;

begin
  NewCommand(4, 'where');
  NewCommand(6, 'dump this');
  Dumping(7);
  NewCommand(5, 'locals');
  Dumping(8);
  Watched:= 0;
  for i:= 0 to FWatches.LBWatches.Items.Count - 1 do begin
    NewCommand(10, 'dump ' + FWatches.LBWatches.Items[i]);
    NewCommand(11, 'print ' + FWatches.LBWatches.Items[i]);
  end;
  Dumping(9);
  FMessages.ShowTab(K_Debugger);
end;

procedure TDebugger.SwitchDetails;
begin
  ShowDetailed:= not ShowDetailed;
  if Running then
    ShowAll;
end;

function TDebugger.hasBreakpoints: boolean;
begin
  Result:= BreakpointAtMain or (ToCursorBreakpoint <> '');
end;

procedure TDebugger.RunToCursorBreakpoint(const s: string);
begin
  NewCommand(1, s);
  ToCursorBreakpoint:= 'clear ' + copy(s, 9, length(s));
end;

procedure TDebugger.log(const s: string);
begin
  if assigned(LogFile) then
    Streamwriteln(LogFile, s);
end;

function TDebugger.FilterLog(s: string): string;
  var p, q: integer;
begin
  p:= Pos('Method entered: ', s);
  if p = 1 then
    Delete(s, 1, 16);
  p:= Pos('Method exited: ', s);
  if p = 1 then
    Delete(s, 1, 15);
  p:= Pos('return value = ', s);
  if p > 0 then
    delete(s, p, length('return value = '));
  p:= Pos('"thread=main"', s);
  if p > 0 then
    delete(s, p, length('"thread=main", '));
  p:= Pos('"thread=Thread-0"', s);
  if p > 0 then
    delete(s, p, length('"thread=Thread-0", '));
  q:= Pos(', JEClassLoader', s);
  if q > 0 then
    delete(s, p, q-p+2);
  p:= Pos(', line=', s);
  if p > 0 then
    delete(s, p, length(s));
  Result:= s;
end;

procedure TDebugger.CloseNotify(Sender: TObject);
begin
  SequenceForm:= nil;
end;

end.
