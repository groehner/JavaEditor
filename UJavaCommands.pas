unit UJavaCommands;

interface

uses Windows, Classes, ComCtrls,
     UEditorForm, UModel, UComJava1, UUtils;

const
  BufSize = $4000;  // of ReadBuf

  // constants for sub-windows
  K_Interpreter = 0;
  K_Compiler = 1;
  K_Messages = 4;

type

   TJavaCommands = class
   private
     Reload: boolean;
     ErrFile: string;
     TVJUnitTests: TTreeView;
     OutputLines: TStringList;
     procedure CompileOneFile(Parameter, Pathname, Package: string);  // do it
   public
     ConsoleMode: integer;
     SuccessfullCompiled: Boolean;
     ManyCompiling: Boolean;
     ManyCompilingErrorOccured: boolean;
     ProcessInformation: TProcessinformation;
     PipeOutput: TPipeHandles;
     EditForm: TFEditForm;
     TerminateProcess: boolean;
     ProcessRunning: boolean;
     ProcessRunningComJava: TComJava1;

     constructor Create;
     destructor Destroy; override;
     procedure Compile(const Pathname, Package: string);
     procedure CompileForm(Form: TFEditForm);
     procedure CompileAll;

     procedure OutputOfCompiler(const Programm, Parameter: string);
     function HasValidClass(const Programm: string): boolean;
     function AcceptableError(const Pathname: string): boolean;
     procedure PasseCaretsAn(SL: TStringList);

     procedure MakeRunJavaBat(Directory, ApplicationName, CommandLine: string;
                              withPause: boolean = true);
     procedure Run(const CallParameter, JavaProgram, Package: string; Gui, JavaFX: Boolean);
     procedure Upload(const JavaProgram: string);
     procedure UploadRCX(const JavaProgram: string);
     procedure UploadNXT(const JavaProgram: string);
     procedure UploadEV3(const JavaProgram: string);
     procedure CallEV3(const Command: string);
     procedure MindstormsUtility(const bat: string);
     procedure EditBuffer(const Buffer: string);
     function ExecAndWait(const ApplicationName, CommandLine, Dir, Output: string;
                           WindowState: word; processMessage: boolean = true): boolean;
     function ExecWithoutWait(const ApplicationName, CommandLine, Dir: string; WindowState: word): boolean;
     procedure ExecWithPipe(const ApplicationName, CommandLine, Dir: string; JavaFX: boolean = false);
     procedure ExecWithConsoleBat(const Batchfile: string);
     function ShellExecuteFile(const FileName, Params, DefaultDir: string;
                               ShowCmd: Integer): THandle;
     procedure Terminate;
     procedure AppletViewer(Pathname: string);
     procedure SetManyCompiling(OnOff: boolean);
     procedure RunTests(aClass: TClass; const Method: string);
   end;

var myJavaCommands: TJavaCommands;

implementation

uses SysUtils, Forms, Controls, StdCtrls, Dialogs, StrUtils, Graphics, ShellApi,
     WideStrUtils, Threading, JvGnugettext, UStringRessources,
     UJava, UMessages, UBaseForm, UDlgAbout, UConfiguration,
     UJUnitTest, UModelEntity;

constructor TJavaCommands.Create;
begin
  OutputLines:= TStringList.Create;
  ErrFile:= FConfiguration.TempDir + 'error.txt';
  TVJUnitTests:= nil;
  ProcessRunning:= false;
  ProcessrunningComJava:= nil;
end;

destructor TJavaCommands.Destroy;
begin
  FreeAndNil(OutputLines);
  FreeAndNil(TVJUnitTests);
end;

procedure TJavaCommands.OutputOfCompiler(const Programm, Parameter: string);
  var s: string;
begin
  if FConfiguration.MindstormsMode
    then s:= Format(_(LNGCompileWith), [Programm]) + ' Lejos'
    else s:= Format(_(LNGCompileWith), [Programm]) + ' Java-Compiler';
  FMessages.OutputLineTo(K_Compiler, s);
  if FConfiguration.ShowCompilerCall then
    if FConfiguration.MindstormsMode and (FConfiguration.MindstormsVersion < 2)
      then FMessages.OutputLineTo(K_Compiler, FConfiguration.LejosCompiler + ' ' + Programm)
      else FMessages.OutputLineTo(K_Compiler, {FConfiguration.JavaCompiler + }'javac' +
             Parameter + ' ' + ExtractFilename(Programm));
  if not ManyCompilingErrorOccured then
    FMessages.StatusMessage(s);
  FMessages.Update;
  FMessages.LBCompiler.Update;
end;

procedure TJavaCommands.PasseCaretsAn(SL: TStringList);
    var i, p, i1, i2: integer; s, s1, s2: string;
begin
  FMessages.Canvas.Font.Assign(FMessages.MInterpreter.Font);
  for i:= 0 to SL.Count - 1 do begin
    s2:= SL.Strings[i];
    if trim(s2) = '^' then begin
      p:= Pos('^', s2);
      s1:= SL.Strings[i-1];
      delete(s1, p+1, length(s1));
      i1:= FMessages.Canvas.TextWidth(s1);
      while FMessages.Canvas.TextWidth(s2) < i1 do
        s2:= ' ' + s2;
      SL.Strings[i]:= s2;
      i1:= i;
      s:= SL.Strings[i1];
      while Pos('.java:', s) = 0 do begin
        dec(i1);
        s:= SL.Strings[i1];
      end;
      i2:= Pos('.java:', s) + 7;
      while s[i2] <> ':' do
        inc(i2);
      insert(IntToStr(p) + ':', s, i2 + 1);
      SL.Strings[i1]:= s;
    end;
  end;
end;

procedure TJavaCommands.CompileOneFile(Parameter, Pathname, Package: string);
  var Directory, Programname, Compiler: string;
      SL: TStringList;
      n: Integer;
      ComJava: TComJava1;

  procedure ShowSuccessfull(SL: TStringList);
  begin
    FMessages.OutputLineTo(K_Compiler, Pathname + ' ' + _(LNGSuccessfullCompiled));
    if not ManyCompilingErrorOccured then
      FMessages.StatusMessage(Pathname + ' ' + _(LNGSuccessfullCompiled), 1);
    PasseCaretsAn(SL);
    FMessages.LBCompiler.Items.AddStrings(SL);
  end;

  procedure ShowErrors(SL: TStringList);

    function HasErrorMarker: boolean;
    begin
      Result:= false;
      for var i:= 0 to SL.Count -1 do
        if trim(SL.Strings[i]) = '^' then
          exit(true);
    end;

  begin
    PasseCaretsAn(SL);
    FMessages.LBCompiler.Items.AddStrings(SL);
    if (pos('error', SL.Text) + pos('Error', SL.Text) +
        pos('file not found', SL.Text) = 0) and not hasErrorMarker
    then begin
      SuccessfullCompiled:= true;
      if not ManyCompilingErrorOccured then
        FMessages.StatusMessage(Pathname + ' ' + LNGSuccessfullCompiled, 1);
    end else begin
      if not FMessages.myIsVisible then
        FMessages.ShowIt;
      FMessages.ShowTab(K_Compiler);
      FMessages.StatusMessage(Pathname + ' ' + _('contains errors.'), 2);
      ManyCompilingErrorOccured:= true;
    end;
  end;

  procedure LogCompile(intern: boolean);
    var F: TextFile; s: string;
  begin
    if FConfiguration.LogfileCompilerOK then begin
      AssignFile(F, FConfiguration.LogfileCompiler);
      try
        Append(F);
        Writeln(F, DateTimeToStr(Now()) + ' ' + GetComputerNetName + ' Version: ' + UDlgAbout.Version);
        s:= FConfiguration.JavaCompiler;  if intern then s:= s + ' (intern)';
        Writeln(F, s + ' ' + Parameter + ' ' + Programname);
        if not SuccessfullCompiled then
          Writeln(F, SL.Text);
        CloseFile(F);
      except
        on e: exception do
          ErrorMsg(e.Message);
      end;
    end;
  end;

  // the bat method gives better error messages than the ExecAndWait method
  // but the bat method starts a cmd.exe which cannot use UNC paths
  procedure CompileWithProcess;
    var Path, Call, WinCodepage, OEMCodepage: string;
  begin
    SuccessfullCompiled:= false;
    WinCodepage:= IntToStr(GetACP);    // Codepage of Windows
    OEMCodepage:= IntToStr(GetOEMCP);  // Codepage of DOS
    Path:= FConfiguration.TempDir + 'CompileJava.bat';
    Call:= ExtractFilename(Compiler) + ' ' + Parameter + ' ' + Programname;
    if IsUNC(GetCurrentDir) then
      SetCurrentDir('C:\');
    var CompileJava:= TStringList.Create;
    try
      CompileJava.Add('@ECHO OFF');
      CompileJava.Add('REM ' + Path);
      // compatible to TEncoding.ANSI of .bat file
      CompileJava.Add('CHCP ' + WinCodepage + ' >NUL');
      if IsUNC(Directory) then begin
        CompileJava.Add('PUSHD ' + HideBlanks(Directory));
        CompileJava.Add(Call);
        CompileJava.Add('POPD');
      end else begin
        CompileJava.Add('CD ' + HideBlanks(Directory));
        CompileJava.Add(ExtractFileDrive(Directory));
        // No CD in BAT file because of codepage problems!   should be solved
        SetCurrentDir(Directory);
        CompileJava.Add(Call);
      end;
      try
        CompileJava.SaveToFile(Path, TEncoding.ANSI);
        if ExecAndWait(Path, '', Directory, ErrFile, SW_HIDE) then begin
          SuccessfullCompiled:= (GetFileSize(ErrFile) = 0);
          if not SuccessfullCompiled then
            SL.LoadFromFile(ErrFile);
          LogCompile(true);
        end else
          ErrorMsg(SysErrorMessage(getLastError));
      except
        on e: Exception do
          ErrorMsg(e.Message);
      end;
    finally
      FreeAndNil(CompileJava);
    end;
  end;

  procedure CompileRCXNXTWithProcess;
  begin
    Compiler:= FConfiguration.LejosCompiler;
    Parameter:= '';  // parameters are for Uploader only
    CompileWithProcess;
  end;

  procedure CompileEV3WithProcess;
  begin
    Compiler:= FConfiguration.JavaCompiler;
    CompileWithProcess;
  end;

  procedure CompileInternally;
  begin
    var s:= ComJava.ExecuteCommand('compile'#4 + FConfiguration.JavaCompilerParameter + #4 + Pathname);
    SuccessfullCompiled:= (Left(s, 3) = '+OK');
    SL.Text:= Right(s, 6);
    LogCompile(false);
  end;
   //.;C:\Program Files\Java\jre7\lib\ext\QTJava.zip

begin  // CompileOnFile
  if not FileExists(Pathname) then begin
    FMessages.StatusMessage(Format(_(LNGFileNotFound), [Pathname]), 2);
    exit;
  end;
  Compiler:= FConfiguration.JavaCompiler;
  Directory:= ExtractFilePath(Pathname);
  Programname:= ExtractFilename(Pathname);
  // getClassPath(Pathname, Package: string)
  if Package <> '' then begin
    Programname:= ReplaceStr(Package, '.', '/') + '/' + Programname;
    n:= CountChar('/', Programname);
    while n > 0 do begin
      Directory:= copy(Directory, 1, Length(Directory)-1);
      Directory:= copy(Directory, 1, LastDelimiter('\', Directory));
      dec(n);
    end;
  end;
  ComJava:= getComJava;

  Screen.Cursor:= crHourGlass;
  SL:= TStringList.Create;
  try
    if assigned(ComJava) and assigned(ComJava.ClassList) and
      (ComJava.ClassList.IndexOf(ChangeFileExt(Programname, '')) > -1) then  // TODO check
        Reload:= true;
    if (Compiler = FConfiguration.JavaCompiler) and FConfiguration.CompileInternally and
        not FConfiguration.MindstormsMode and not ComJava.ErrorOccured
    then begin
      CompileInternally;
      if ComJava.ErrorOccured then CompileWithProcess;
    end else if FConfiguration.MindstormsMode then
      if FConfiguration.MindstormsVersion = 2
        then CompileEV3WithProcess
        else CompileRCXNXTWithProcess
    else
      CompileWithProcess;
    SL.Add('');
    if SuccessfullCompiled
      then ShowSuccessfull(SL)
      else ShowErrors(SL);
  finally
    FreeAndNil(SL);
    Screen.Cursor:= crDefault;
  end;
end;

procedure TJavaCommands.Compile(const Pathname, Package: string);
begin
  FMessages.ChangeTab(K_Compiler);
  if not ManyCompiling then
    FMessages.DeleteTab(K_Compiler);
  Reload:= false;
  var E:= TFEditForm(FJava.getTDIWindowType(Pathname, '%E%'));
  if assigned(E) then
    E.InitShowCompileErrors
  else begin
    E:= FJava.OpenEditForm(Pathname, true);
    if E.Modified then FJava.DoSave(E, false);
  end;
  var CompilerCall:= FConfiguration.getCompilerCall(Pathname, Package, E.Encoding);
  OutputOfCompiler(Pathname, CompilerCall);
  CompileOneFile(CompilerCall, Pathname, Package);
  if Reload then getComJava.JavaReset;
end;

procedure TJavaCommands.CompileForm(Form: TFEditForm);
begin
  if assigned(Form) then
    Compile(Form.Pathname, Form.getPackage);
end;

procedure TJavaCommands.CompileAll;
  var i: Integer; CompilerCall: string;
      SL: TStringList; EForm: TFEditForm;
begin
  FMessages.ChangeTab(K_Compiler);
  FMessages.DeleteTab(K_Compiler);
  FMessages.StatusMessage('');
  setManyCompiling(true);
  Reload:= false;
  SL:= TStringList.Create;
  SL.Sorted:= true;
  for i:= 0 to FJava.TDIEditFormCount - 1 do begin
    EForm:= FJava.TDIEditFormGet(i);
    if EForm.IsJava then
      SL.Add(EForm.Pathname);
  end;

  for i:= 0 to SL.Count - 1 do begin
    EditForm:= FJava.getTDIWindow(SL[i]) as TFEditForm;
    if not EditForm.visible then continue;
    EditForm.InitShowCompileErrors;
    CompilerCall:= FConfiguration.getCompilerCall(EditForm.PathName, EditForm.getPackage, EditForm.Encoding);
    OutputOfCompiler(EditForm.PathName, CompilerCall);
    CompileOneFile(CompilerCall, EditForm.PathName, EditForm.getPackage);
    FMessages.LBCompiler.Items.Add('');
  end;
  if ManyCompilingErrorOccured then
    FJava.ShowCompileErrors;
  setManyCompiling(false);
  if Reload then getComJava.JavaReset;
  FreeAndNil(SL);
end;

function TJavaCommands.HasValidClass(const Programm: string): boolean;
  var Age1, Age2: TDateTime; Path: string;
begin
  Result:= false;
  FileAge(Programm, Age1);
  Path:= getClass(FConfiguration.getClasspath, FConfiguration.JavaCompilerParameter, Programm);
  if Path <> '' then begin
    FileAge(Path, Age2);
    Result:= Age1 <= Age2;
  end;
end;

function TJavaCommands.AcceptableError(const Pathname: string): boolean;
begin
  var s:= FMessages.getCompileError(Pathname);
  Result:= (Pos('1 error', s) > 0) and (Pos(': constructor ', s) > 0);
end;

procedure TJavaCommands.MakeRunJavaBat(Directory, ApplicationName,
                          CommandLine: string; withPause: boolean = true);
  var Path, Codepage, WinCodepage, OEMCodepage: string;
begin
  WinCodepage:= IntToStr(GetACP); // Codepage of Windows
  OEMCodepage:= IntToStr(GetOEMCP);  // Codepage of DOS
  Path:= FConfiguration.TempDir + 'RunJava.bat';
  CodePage:= FConfiguration.Codepage;
  var p:= Pos(' ', Codepage);
  if p > 0 then
    Codepage:= copy(Codepage, 1, p - 1);
  if isUNC(GetCurrentDir) then
    SetCurrentDir('C:\');
  var RunJava:= TStringList.Create;
  try
    RunJava.Add('@ECHO OFF');
    RunJava.Add('REM ' + Path);
    // compatible to TEncoding.ANSI of .bat file
    if Codepage = '' then
      Codepage:= WinCodepage;
    RunJava.Add('CHCP ' + Codepage + ' > NUL');
    if IsUNC(Directory) then begin
      RunJava.Add('PUSHD ' + HideBlanks(Directory));  // \\Server\Path\
      RunJava.Add(HideBlanks(ApplicationName) + AddWithSpace(CommandLine));
      RunJava.Add('POPD');
    end else begin
      RunJava.Add('CD ' + HideBlanks(Directory));
      RunJava.Add(ExtractFileDrive(Directory));
      RunJava.Add(HideBlanks(ApplicationName) + AddWithSpace(CommandLine));
    end;
    if withPause then begin
      RunJava.Add('@ECHO ON');
      RunJava.Add('PAUSE');
    end;
    try
      RunJava.SaveToFile(Path, TEncoding.ANSI);
    except on e: Exception do
      ErrorMsg(Format(_(LNGCanNotCreateFile), [Path, e.Message]));
    end;
  finally
    FreeAndNil(RunJava);
  end;
end;

procedure TJavaCommands.Run(const CallParameter, JavaProgram, Package: string; GUI, JavaFX: Boolean);
  var Path, Dir, ApplicationName, CommandLine, Classpath, CodePa, myProgram: string;
      n: integer;
begin
  Path:= ExpandFileName(JavaProgram);
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  FMessages.OutputLineTo(K_Interpreter, _('Starting') + ' ' + Path);

  Classpath:= FConfiguration.getClassPath(JavaProgram, Package);
  myProgram:= ChangeFileExt(ExtractFilename(Path), '');
  Dir:= ExtractFilePath(Path);
  if Package <> '' then begin
    myProgram:= Package + '.' + myProgram;
    n:= CountChar('.', Package) + 1;
    while n > 0 do begin
      Dir:= copy(Dir, 1, Length(Dir)-1);
      Dir:= copy(Dir, 1, LastDelimiter('\', Dir));
      dec(n);
    end;
  end;

  CodePa:= '';
  ApplicationName:= FConfiguration.JavaInterpreter;
  CommandLine:= CodePa + ' -classpath ' + ClassPath +
                AddWithSpace(FConfiguration.getJavaInterpreterParameter(JavaFX)) +
                AddWithSpace(myProgram) + AddWithSpace(CallParameter);
  if FConfiguration.ShowInterpreterCall then
    FMessages.OutputLineTo(K_Interpreter, ApplicationName + AddWithSpace(CommandLine));
  FMessages.OutputLineTo(K_Interpreter, #13#10);

  if ConsoleMode = 0 then
    if Gui then
      ConsoleMode:= 1
    else if FConfiguration.UseInterpreterWindowAsConsole then
      ConsoleMode:= 2
    else begin
      MakeRunJavaBat(Dir, ApplicationName, CommandLine);
      ConsoleMode:= 3
    end;

  case ConsoleMode of
    1: TTask.Create(
       procedure begin
         ExecWithPipe(ApplicationName, CommandLine, Dir, JavaFX); // without console for GUI programs
       end).Start;
    2: FMessages.Run(Classpath, myProgram, CallParameter);              // with interpreter window as console
    3: ExecWithConsoleBat(FConfiguration.TempDir + 'RunJava.bat');      // with cmd console
  end;
  ConsoleMode:= 0;
end;

procedure TJavaCommands.Upload(const JavaProgram: string);
begin
  case FConfiguration.MindstormsVersion of
    0: UploadRCX(JavaProgram);
    1: UploadNXT(JavaProgram);
    2: begin
         FConfiguration.MindstormsRun:= true;
         UploadEV3(JavaProgram);
       end;
  end;
end;

procedure TJavaCommands.UploadRCX(const JavaProgram: string);
  var s: string;
begin
  var Filepath:= ExpandFileName(JavaProgram);
  FMessages.ShowIt;
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  if JavaProgram = 'Lejos-Firmware' then begin
    s:= 'Download Lejos firmware' + #13#10 + '0%' +#13#10;
    MakeRunJavaBat(FConfiguration.TempDir, FConfiguration.RCXFlasher, '', false);
  end else begin
    s:= Format(_(LNGTransferToRCX), [Filepath]) + #13#10 + '0%' +#13#10;
    MakeRunJavaBat(FConfiguration.TempDir, FConfiguration.RCXUploader, ChangeFileExt(ExtractFilename(JavaProgram), ''), false);
  end;
  FMessages.OutputLineTo(K_Interpreter, s);
  ExecWithPipe('', FConfiguration.TempDir + 'RunJava.bat', ExtractFilePath(Filepath));
  with FMessages.MInterpreter do begin
    if (Pos('100%', Lines.Text) > 0) or (Pos('Firmware unlocked', Lines.Text) > 0)
      then s:= _(LNGSuccessfullTransfered)
      else s:= _(LNGErrorDuringTransferToRCX);
    FMessages.OutputLineTo(K_Interpreter, s);
  end;
end;

procedure TJavaCommands.UploadNXT(const JavaProgram: string);
  var Path, Line, Parameter: string;
begin
  Path:= ExpandFileName(JavaProgram);
  Parameter:= '';
  if FConfiguration.MindstormsVerbose then Parameter:= Parameter + ' -v ';
  if FConfiguration.MindstormsDebug   then Parameter:= Parameter + ' -g ';
  if FConfiguration.MindstormsRun     then Parameter:= Parameter + ' -r ';
  Parameter:= Parameter + ' ' + FConfiguration.MindstormsParameter + ' ';
  FMessages.ShowIt;
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  Line:= Format(FConfiguration.SetRCXNXTEV3(_(LNGTransferToRCX)), [Path]);
  FMessages.OutputLineTo(K_Interpreter, Line);
  FMessages.OutputLineTo(K_Interpreter, Line);
  ExecWithPipe('',
               HideBlanks(FConfiguration.LejosUploader) + ' ' + Parameter +
                          ChangeFileExt(ExtractFilename(Path), ''),
               ExtractFilePath(Path));
  with FMessages.MInterpreter do begin
    if Pos('Upload successful', Lines.Text) > 0
      then Line:= FConfiguration.SetRCXNXTEV3(_(LNGSuccessfullTransfered))
      else Line:= FConfiguration.SetRCXNXTEV3(_(LNGErrorDuringTransferToRCX));
    FMessages.OutputLineTo(K_Interpreter, Line);
  end;
end;

procedure TJavaCommands.UploadEV3(const JavaProgram: string);
  var Path, parameter: string;
begin
  Path:= ExpandFileName(JavaProgram);
  FJava.DoJarCreateEV3(Path);
  if FConfiguration.MindstormsRun
    then Parameter:= ' -r '
    else Parameter:= '';
  CallEV3(Parameter + HideBlanks(ChangeFileExt(Path, '.jar')));
end;

procedure TJavaCommands.CallEV3(const Command: string);
begin
  var ApplicationName:= FConfiguration.JavaInterpreter;
  var CommandLine:= ' -cp ' +
                    HideBlanks(FConfiguration.LejosVerzeichnis + '\lib\ev3\ev3classes.jar;' +
                      FConfiguration.JavaClasspathUser + ';' +
                      FConfiguration.JavaClasspathAdmin) +
                    ' JEControl -n ' + FConfiguration.MindstormsIP +
                    ' ' + Command;
  FMessages.OutputLineTo(K_Interpreter, ApplicationName + CommandLine);
  ExecWithPipe(ApplicationName, CommandLine, FConfiguration.EditorFolder);
end;

procedure TJavaCommands.MindstormsUtility(const bat: string);
begin
  FMessages.ShowIt;
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  var s:= FConfiguration.LejosVerzeichnis + '\bin\' + bat;
  FMessages.OutputLineTo(K_Interpreter, s);
  ExecWithoutWait(s, '', '', SW_Hide);
end;

procedure TJavaCommands.EditBuffer(const Buffer: string);
   var i, j, Line: integer; s, percent: string;
begin
  s:= Buffer;
  i:= length(s);
  while (i > 0) and (s[i] <> '%') do
    dec(i);
  j:= i - 1;
  while (j > 0) and ('0' <= s[j]) and (s[j] <= '9') do
    dec(j);
  percent:= Copy(s, j+1, i-j-1);
  if percent = '' then
    FMessages.OutputLineTo(K_Interpreter, s)
  else begin
    Line:= FMessages.MInterpreter.Lines.Count;
    FMessages.MInterpreter.Lines[Line-1]:=
      Copy('##################################################', 1,
           round(StrToInt(percent)/2)) + ' ' + percent + '%';
  end;
end;

// calls a program, waits until ready and collects the output to a file
function TJavaCommands.ExecAndWait(const ApplicationName, CommandLine, Dir, Output: string;
                     WindowState: word; processMessage: boolean = true): boolean;
  var
    StartupInfo: TStartupInfo;
    SecAttr: TSecurityAttributes;
    ErrorHandle: THandle;
    Cmd, CurrentDir: string;
    lw: Word;
begin
  if ProcessRunning then begin
    TerminateProcess:= true;
    exit(false);
  end;
  FJava.RunButtonToStop(true);
  ProcessRunning:= true;
  TerminateProcess:= false;
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor := nil;
  SecAttr.bInheritHandle:= true;
  if Output <> '' then begin
    DeleteFile(PChar(Output));
    ErrorHandle:= CreateFile(PChar(Output), GENERIC_WRITE, FILE_SHARE_READ, @SecAttr,
                             CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if ErrorHandle = INVALID_HANDLE_VALUE then begin
      ErrorMsg('DEBUG ExecAndWait: ' + SysErrorMessage(getLastError));
      ErrorHandle:= 0;
    end;
  end else
    ErrorHandle:= 0;

  FillChar(ProcessInformation, SizeOf(ProcessInformation), #0);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  with StartupInfo do begin
    cb:= SizeOf(StartupInfo);
    wShowWindow := WindowState;
    if ErrorHandle > 0 then begin
      dwFlags:= STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      hStdOutput:= ErrorHandle;
      hStdError := ErrorHandle;
    end else
      dwFlags:= STARTF_USESHOWWINDOW;
  end;
  Cmd:= trim(ApplicationName + ' ' + CommandLine);
  CurrentDir:= Dir;
  if (CurrentDir = '') or not SysUtils.DirectoryExists(CurrentDir) then
    if ApplicationName <> ''
      then CurrentDir:= ExtractFilePath(ApplicationName)
      else CurrentDir:= ExtractFilePath(CommandLine);
  if IsUNC(CurrentDir) then
    CurrentDir:= '.';
  try
    Result:= CreateProcess(nil, PChar(Cmd), nil, nil, true,
                         CREATE_NEW_CONSOLE OR NORMAL_PRIORITY_CLASS,
                         nil, PChar(CurrentDir), StartupInfo, ProcessInformation);
    if Result then
      repeat
        lw:= WaitForSingleObject(ProcessInformation.hProcess, 20);
      until (lw = WAIT_OBJECT_0) or TerminateProcess;
  finally
    TerminateTask(Processinformation);
    if ErrorHandle > 0 then
      CloseHandle(ErrorHandle);
    ProcessRunning:= false;
    FJava.RunButtonToStop(false);
  end;
  if processMessage then
    Application.ProcessMessages; // else wine produces IO-Error
    // during TFJava.FormCreate/TFConfiguration.Init/getJavaVersion
    // a ProcessMessags starts SystemExecuteMacro/TFJava.Open to early
end;

// runs an external program, no need to wait for or close external program
function TJavaCommands.ExecWithoutWait(const ApplicationName, CommandLine, Dir: string; WindowState: word): boolean;
  var
    StartupInfo: TStartupInfo;
    LocalProcessInformation: TProcessInformation;
    CmdLine, CurrentDir: string;
begin
  // doesn't start chm-files
  FillChar(LocalProcessInformation, SizeOf(LocalProcessInformation), #0);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  with StartupInfo do begin
    cb:= SizeOf(StartupInfo);
    dwFlags:= STARTF_USESHOWWINDOW;
    wShowWindow:= WindowState;
  end;
  CmdLine:= ApplicationName + ' ' + CommandLine;      // HideBlanks
  CurrentDir:= Dir;
  if (CurrentDir = '') or not SysUtils.DirectoryExists(CurrentDir) then
    if ApplicationName <> ''
      then CurrentDir:= ExtractFilePath(ApplicationName)
      else CurrentDir:= ExtractFilePath(CommandLine);
  try
    Result:= CreateProcess(nil, PChar(CmdLine), nil, nil, true,
               CREATE_NEW_CONSOLE OR NORMAL_PRIORITY_CLASS,
               nil, PChar(CurrentDir), StartupInfo, LocalProcessInformation);
    if not Result then
      ErrorMsg(SysErrorMessage(GetLastError));
  finally
    CloseProcessinformationHandle(LocalProcessInformation);
  end;
end;

procedure TJavaCommands.ExecWithPipe(const ApplicationName, CommandLine, Dir: string; JavaFX: boolean = false);
  var SecAttr    : TSecurityAttributes;
      StartupInfo: TStartupInfo;
      dwExitCode : DWORD;
      uBuffer: RawByteString;
      Buffer: string;
      Read: Cardinal;
      SL: TStringList;
      HWnd: THandle;
      Max: integer;
      CreateOK: boolean;
      CurrentDir, Cmd: string;
begin
  if ProcessRunning then begin
    TerminateProcess:= true;
    exit;
  end;
  OutputLines.Clear;
  SecAttr.nLength:= SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor:= nil;
  SecAttr.bInheritHandle:= true;
  with PipeOutput do
    if not CreatePipe(hRead, hWrite, @SecAttr, BufSize) then begin
      ErrorMsg('Error on STDOUT pipe creation: ' + SysErrorMessage(GetLastError));
      exit;
    end;

  TThread.Queue(nil, procedure
    begin
      FJava.RunButtonToStop(true);
    end);
  ProcessRunning:= true;
  TerminateProcess:= false;

  FillChar(ProcessInformation, SizeOf(ProcessInformation), #0);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  with StartupInfo do begin
    cb:= SizeOf(StartupInfo);
    wShowWindow:= SW_Hide;
    dwFlags    := STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
    hStdInput  := GetStdHandle(STD_INPUT_HANDLE);
    hStdOutput := PipeOutput.hWrite;
    hStdError  := PipeOutput.hWrite;
  end;
  if ApplicationName = ''
    then Cmd:= CommandLine
    else Cmd:= ApplicationName + ' ' + Commandline;
  CurrentDir:= Dir;
  if (CurrentDir = '') or not DirectoryExists(CurrentDir) then
    if ApplicationName <> ''
      then CurrentDir:= ExtractFilePath(ApplicationName)
      else CurrentDir:= ExtractFilePath(CommandLine);
  try
    try
      CreateOK:= CreateProcess(nil,             // lpApplicationName
                       PChar(Cmd),              // lpCommandLine
                       nil,                     // lpProcessAttributess
                       nil,                     // lpThreadAttributess
                       true,                    // bInheritHandles
                                                // dwCreationFlags
                       CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS OR CREATE_UNICODE_ENVIRONMENT,
                       nil,                     // pEnvironment
                       PChar(CurrentDir),       // pCurrentDirectory
                       StartupInfo,             // lpStartupInfo,
                       ProcessInformation);      // lpProcessInformation
      if CreateOK then begin
        waitForInputIdle(ProcessInformation.hProcess, MaxInt);
        if JavaFX then begin
          Max:= 0;
          repeat
            Sleep(100);
            HWnd:= FindWindow('GlassWndClass-GlassWindowClass-2', nil);
            inc(Max);
          until (Max = 100) or (Hwnd <> 0);
          ShowWindow(HWnd, SW_Show);
        end;
        repeat
          GetExitCodeProcess(ProcessInformation.hProcess, dwExitCode);
          Application.ProcessMessages;
          Sleep(20);
          if PeekNamedPipe(PipeOutput.hRead, nil, 0, nil, @Read, nil) and (Read > 0) then begin
            SetLength(uBuffer, Read);
            ReadFile(PipeOutput.hRead, uBuffer[1], Read, Read, nil);
            {$WARNINGS OFF}
            Buffer:= AnsiToUTF8(AnsiString(uBuffer));
            {$WARNINGS ON}
            if FConfiguration.MindstormsMode and (FConfiguration.MindstormsVersion = 0)
              then EditBuffer(Buffer)
            else if FConfiguration.AndroidMode then begin
              SL:= TStringList.Create;
              SL.Text:= Buffer;
              PasseCaretsAn(SL);
              FMessages.ChangeTab(K_Compiler);
              FMessages.OutputTo(K_Compiler, SL);
              FreeAndNil(SL);
            end else
              TThread.Queue(nil, procedure
                begin
                  FMessages.OutputToTerminal(Buffer);
                end);
      end;
        until (dwExitCode <> STILL_ACTIVE) or TerminateProcess
      end else
        ErrorMsg(Format(_(LNGUnabledToExecute), [Cmd]) + ' ' + SysErrorMessage(GetLastError));
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    TerminateTask(Processinformation);
    ClosePipeHandles(PipeOutput);
    ProcessRunning:= false;
    TThread.Queue(nil, procedure
      begin
        FJava.RunButtonToStop(false);
      end);
  end;
end;

procedure TJavaCommands.ExecWithConsoleBat(const Batchfile: string);
  var StartupInfo: TStartupInfo;
      dwExitCode: DWord;
begin
  if ProcessRunning then begin
    TerminateProcess:= true;
    exit;
  end;
  FJava.RunButtonToStop(true);
  ProcessRunning:= true;
  TerminateProcess:= false;
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  with StartupInfo do begin
    cb:= SizeOf(StartupInfo);
    dwFlags:= STARTF_USESHOWWINDOW;
    wShowWindow:= SW_ShowNormal;
  end;
  try
    if CreateProcess(nil,                     // lpApplicationName
                     PChar(Batchfile),        // lpCommandLine
                     nil,                     // lpProcessAttributess
                     nil,                     // lpThreadAttributess
                     true,                    // bInheritHandles
                                              // dwCreationFlags
                     CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                     nil,                     // pEnvironment
                     nil,                     // pCurrentDirectory
                     StartupInfo,             // lpStartupInfo,
                     ProcessInformation)      // lpProcessInformation
    then
      repeat
        GetExitCodeProcess(ProcessInformation.hProcess, dwExitCode);
        Application.ProcessMessages;
        Sleep(20);
      until (dwExitCode <> STILL_ACTIVE) or TerminateProcess
    else
      ErrorMsg(Format(_(LNGUnabledToExecute), [Batchfile]) + ' ' + SysErrorMessage(GetLastError));
  finally
    TerminateTask(Processinformation);
    ProcessRunning:= false;
    FJava.RunButtonToStop(false);
  end;
end;

function TJavaCommands.ShellExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
begin
  Result:= ShellExecute(Application.MainForm.Handle, 'open',
             PChar(FileName), PChar(Params), PChar(DefaultDir), ShowCmd);
end;

procedure TJavaCommands.Terminate;
begin
  if assigned(ProcessRunningComJava) then
    ProcessRunningComJava.JavaReset;
  TerminateProcess:= true;
end;

procedure TJavaCommands.Appletviewer(Pathname: string);
  var Filename, Params: string;
begin
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  FMessages.StatusMessage('Appletviewer: ' + Pathname);

  Pathname:= ExpandFileName(Pathname);
  Filename:= FConfiguration.JavaAppletviewer;
  Params  := ' -J-Djava.security.policy=file:'+
             HideBlanks(FConfiguration.EditorFolder + 'java.policy.applet') + ' ' +
             HideBlanks(Pathname);
  ChDir(ExtractFilePath(Pathname));
  if FConfiguration.ShowInterpreterCall then
    FMessages.OutputLineTo(K_Interpreter, Filename + ' ' + Params);
  ExecWithPipe(Filename, Params, '.');
end;

procedure TJavaCommands.SetManyCompiling(OnOff: boolean);
begin
  ManyCompiling:= OnOff;
  ManyCompilingErrorOccured:= false;
end;

procedure TJavaCommands.RunTests(aClass: TClass; const Method: string);
  var PictureNr, i, p: integer;
      Call, aClassname, Classpath, Output: string;
      It: IModelIterator;
      aMethod: TOperation;
      Node: TTreeNode;
      SL: TStringList;
begin
  aClassname:= ChangeFileExt(ExtractFileName(aClass.Pathname), '');
  Classpath:= FConfiguration.getClassPath;
  p:= Pos(FConfiguration.JUnitJarFile, Classpath);
  if p > 0 then
    Delete(Classpath, p, length(FConfiguration.JUnitJarFile));

  ErrFile:= FConfiguration.TempDir + 'error.txt';
  call:=  '-jar ' + HideBlanks(FConfiguration.JUnitJarFile) +
            ' -cp ' + Classpath + ' ' + FConfiguration.JUnitParameter +
            ' --disable-ansi-colors --details=tree';
  if Method <> 'Class'
    then Call:= Call + ' -m ' + aClassname + #4 + Method
    else Call:= Call + ' -c ' + aClassname;

  FMessages.LBMessages.Clear;
  FMessages.OutputLineTo(K_Messages, FConfiguration.JavaInterpreter + ' ' + Call);
  if ExecAndWait(FConfiguration.JavaInterpreter, Call, ExtractFilePath(aClass.Pathname), ErrFile, SW_HIDE) then begin
    FMessages.ShowTab(K_Messages);
    FMessages.ShowMessages(ErrFile);
    SL:= TStringList.Create;
    SL.LoadFromFile(ErrFile);

    with FJUnitTests do begin
      TVJUnitTests.Items.BeginUpdate;
      DeleteData;
      It:= aClass.GetOperations;
      PJUnit.Color:= clGreen;
      PictureNr:= 0;
      while It.HasNext do begin
        aMethod:= It.Next as TOperation;
        if (aMethod.Annotation <> 'Test') and (aMethod.Annotation <> 'ParameterizedTest') then continue;
        if (aMethod.Name <> Method) and (Method <> 'Class') then continue;
        i:= 1;
        p:= Pos('-- ' + aMethod.Name, SL.Strings[i]);
        while (p = 0) and (i < SL.count-1) do begin
          inc(i);
          p:= Pos('-- ' + aMethod.Name, SL.Strings[i]);
          if not IsWordInLine(aMethod.Name, SL.Strings[i]) then
            p:= 0;
        end;
        if i < SL.Count then begin
          if Pos('[OK]', SL.Strings[i]) > 0 then
            PictureNr:= 0
          else begin
            PictureNr:= 1;
            PJUnit.Color:= clRed;
          end;
          Output:= copy(SL.Strings[i], p + 3, 255);
        end;
        Node:= TVJUnitTests.Items.AddObject(nil, Output, TInteger.create(aMethod.LineS));
        Node.ImageIndex:= PictureNr;
        Node.SelectedIndex:= PictureNr;
        Node.HasChildren:= false;
      end;
      TVJUnitTests.Items.EndUpdate;
    end;
    FreeandNil(SL);
  end;
  FJava.UpdateLayoutRightDockPanel;
end;

end.
