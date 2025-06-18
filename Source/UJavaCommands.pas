unit UJavaCommands;

interface

uses
  Windows,
  Classes,
  ComCtrls,
  UEditorForm,
  UModel,
  UComJava1,
  UUtils;

const
  BufSize = $4000;  // of ReadBuf

  // constants for sub-windows
  K_Interpreter = 0;
  K_Compiler = 1;
  K_Messages = 4;

type

  TJavaCommands = class
  private
    FReload: Boolean;
    FErrFile: string;
    FTVJUnitTests: TTreeView;
    FOutputLines: TStringList;
    FConsoleMode: Integer;
    FSuccessfullCompiled: Boolean;
    FManyCompiling: Boolean;
    FPipeOutput: TPipeHandles;
    FEditForm: TFEditForm;
    FManyCompilingErrorOccured: Boolean;
    FTerminateProcess: Boolean;
    FProcessRunningComJava: TComJava1;
    FProcessInformation: TProcessInformation;
    FProcessRunning: Boolean;

    FCompileFilepath: string;
    FCompileFilename: string;
    FCompileDirectory: string;
    FCompileParameter: string;
    FCompilePackage: string;
    FCompiler: string;
    FCompileList: TStringList;

    procedure CompileOneFile;
    procedure CompileWithProcess;
    procedure CompileInternally;
    procedure LogCompile;
    function GetCompilerNumber: Integer;
    procedure ShowSuccessfull;
    procedure ShowErrors;
    procedure OutputCompileInfos(CompilerNr: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Compile(const Pathname, Package: string);
    procedure CompileForm(Form: TFEditForm);
    procedure CompileAll;

    function HasValidClass(const Programm: string): Boolean;
    function AcceptableError(const Pathname: string): Boolean;
    procedure AdjustCarets(StringList: TStringList);

    procedure MakeRunJavaBat(Directory, ApplicationName, CommandLine: string;
                             WithPause: Boolean = True);
    procedure Run(const CallParameter, JavaProgram, Package: string; Gui, JavaFX: Boolean);
    procedure Upload(const JavaProgram: string);
    procedure UploadRCX(const JavaProgram: string);
    procedure UploadNXT(const JavaProgram: string);
    procedure UploadEV3(const JavaProgram: string);
    procedure CallEV3(const Command: string);
    procedure MindstormsUtility(const Bat: string);
    procedure EditBuffer(const Buffer: string);
    function ExecAndWait(const ApplicationName, CommandLine, Dir, Output: string;
                          WindowState: Word; ProcessMessage: Boolean = True): Boolean;
    function ExecWithoutWait(const ApplicationName, CommandLine, Dir: string; WindowState: Word): Boolean;
    procedure ExecWithPipe(const ApplicationName, CommandLine, Dir: string; JavaFX: Boolean = False);
    procedure ExecWithConsoleBat(const Batchfile: string);
    function ShellExecuteFile(const FileName, Params, DefaultDir: string;
                              ShowCmd: Integer): THandle;
    procedure Terminate;
    procedure AppletViewer(Pathname: string);
    procedure SetManyCompiling(OnOff: Boolean);
    procedure RunTests(AClass: TClass; const Method: string);

    property ConsoleMode: Integer write FConsoleMode;
    property SuccessfullCompiled: Boolean read FSuccessfullCompiled;
    property EditForm: TFEditForm read FEditForm write FEditForm;
    property ManyCompiling: Boolean read FManyCompiling;
    property ManyCompilingErrorOccured: Boolean read FManyCompilingErrorOccured;
    property TerminateProcess: Boolean read FTerminateProcess write FTerminateProcess;
    property ProcessRunningComJava: TComJava1 write FProcessRunningComJava;
    property ProcessRunning: Boolean read FProcessRunning write FProcessRunning;
  end;

var MyJavaCommands: TJavaCommands;

implementation

uses
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  StrUtils,
  Graphics,
  ShellAPI,
  IOUtils,
  Threading,
  JvGnugettext,
  UStringRessources,
  UJava,
  UMessages,
  UDlgAbout,
  UConfiguration,
  UJUnitTest,
  UModelEntity;

constructor TJavaCommands.Create;
begin
  FOutputLines:= TStringList.Create;
  FTVJUnitTests:= nil;
  FProcessRunning:= False;
  FProcessRunningComJava:= nil;
end;

destructor TJavaCommands.Destroy;
begin
  inherited;
  FreeAndNil(FOutputLines);
  FreeAndNil(FTVJUnitTests);
end;

procedure TJavaCommands.OutputCompileInfos(CompilerNr: integer);
  var Compiler: string;
begin
  case CompilerNr of
    1: Compiler:= ' javac.exe';
    2: Compiler:= ' Lejos compiler';
    3: Compiler:= ' Intern compiler';
  end;
  FMessages.OutputLineTo(K_Compiler, Format(_(LNGCompileWith), [FCompileFilename]) + Compiler);
  if FConfiguration.ShowCompilerCall then
    FMessages.OutputLineTo(K_Compiler, Compiler + ' ' + FCompileParameter + ' ' +
      FCompileFilename);
  if not FManyCompilingErrorOccured then
    FMessages.StatusMessage(Compiler);
  FMessages.Update;
  FMessages.LBCompiler.Update;
end;

procedure TJavaCommands.AdjustCarets(StringList: TStringList);
  var Posi, Int1, Int2: Integer; Str, Str1, Str2: string;
begin
  FMessages.Canvas.Font.Assign(FMessages.MInterpreter.Font);
  for var I:= 0 to StringList.Count - 1 do begin
    Str2:= StringList[I];
    if Trim(Str2) = '^' then begin
      Posi:= Pos('^', Str2);
      Str1:= StringList[I-1];
      Delete(Str1, Posi+1, Length(Str1));
      Int1:= FMessages.Canvas.TextWidth(Str1);
      while FMessages.Canvas.TextWidth(Str2) < Int1 do
        Str2:= ' ' + Str2;
      StringList[I]:= Str2;
      Int1:= I;
      Str:= StringList[Int1];
      while Pos('.java:', Str) = 0 do begin
        Dec(Int1);
        Str:= StringList[Int1];
      end;
      Int2:= Pos('.java:', Str) + 7;
      while Str[Int2] <> ':' do
        Inc(Int2);
      Insert(Posi.ToString + ':', Str, Int2 + 1);
      StringList[Int1]:= Str;
    end;
  end;
end;

procedure TJavaCommands.LogCompile;
  var AFile: TextFile;
begin
  if FConfiguration.LogfileCompilerOK then begin
    AssignFile(AFile, FConfiguration.LogfileCompiler);
    try
      try
        Append(AFile);
        Writeln(AFile, DateTimeToStr(Now) + ' ' + GetComputerNetName + ' Version: ' + UDlgAbout.Version);
        Writeln(AFile, FCompiler + ' ' + FCompileParameter + ' ' + FCompileFilename);
        Writeln(AFile, FCompileList.Text);
      except
        on E: Exception do
          ErrorMsg(E.Message);
      end;
    finally
      CloseFile(AFile);
    end;
  end;
end;

// the bat method gives better error messages than the ExecAndWait method
// but the bat method starts a cmd.exe which cannot use UNC paths
procedure TJavaCommands.CompileWithProcess;
  var BatFile, Call, WinCodepage: string;
begin
  FSuccessfullCompiled:= False;
  WinCodepage:= GetACP.ToString;  // Codepage of Windows
  BatFile:= FConfiguration.TempDir + 'CompileJava.bat';
  Call:= ExtractFileName(FCompiler) + ' ' + FCompileParameter + ' ' + FCompileFilename;
  if IsUNC(GetCurrentDir) then
    SetCurrentDir('C:\');
  var CompileJava:= TStringList.Create;
  try
    CompileJava.Add('@ECHO OFF');
    CompileJava.Add('REM ' + BatFile);
    // compatible to TEncoding.ANSI of .bat file
    CompileJava.Add('CHCP ' + WinCodepage + ' >NUL');
    if IsUNC(FCompileDirectory) then begin
      CompileJava.Add('PUSHD ' + HideBlanks(FCompileDirectory));
      CompileJava.Add(Call);
      CompileJava.Add('POPD');
    end else begin
      CompileJava.Add('CD ' + HideBlanks(FCompileDirectory));
      CompileJava.Add(ExtractFileDrive(FCompileDirectory));
      // No CD in BAT file because of codepage problems!   should be solved
      SetCurrentDir(FCompileDirectory);
      CompileJava.Add(Call);
    end;
    try
      CompileJava.SaveToFile(BatFile, TEncoding.ANSI);
      if ExecAndWait(BatFile, '', FCompileDirectory, FErrFile, SW_HIDE) then begin
        FSuccessfullCompiled:= (GetFileSize(FErrFile) = 0);
        if not FSuccessfullCompiled then
          FCompileList.LoadFromFile(FErrFile);
      end else
        ErrorMsg(SysErrorMessage(GetLastError));
    except
      on E: Exception do
        ErrorMsg(Format(_(LNGCanNotWrite), [BatFile]) + E.Message);
    end;
  finally
    FreeAndNil(CompileJava);
  end;
end;

function TJavaCommands.GetCompilerNumber: Integer;
begin
  if FConfiguration.CompileInternally and not FConfiguration.MindstormsMode then
    Result:= 3
  else if FConfiguration.MindstormsMode and (FConfiguration.MindstormsVersion < 2) then
    Result:= 2
  else
    Result:= 1;
end;

procedure TJavaCommands.ShowSuccessfull;
begin
  FMessages.OutputLineTo(K_Compiler, FCompileFilepath + ' ' + _(LNGSuccessfullCompiled));
  if not FManyCompilingErrorOccured then
    FMessages.StatusMessage(FCompileFilepath + ' ' + _(LNGSuccessfullCompiled), 1);
  AdjustCarets(FCompileList);
  FMessages.LBCompiler.Items.AddStrings(FCompileList);
end;

procedure TJavaCommands.ShowErrors;

  function HasErrorMarker: Boolean;
  begin
    Result:= False;
    for var I:= 0 to FCompileList.Count -1 do
      if Trim(FCompileList[I]) = '^' then
        Exit(True);
  end;

begin
  AdjustCarets(FCompileList);
  FMessages.LBCompiler.Items.AddStrings(FCompileList);
  if (Pos('error', FCompileList.Text) + Pos('Error', FCompileList.Text) +
      Pos('file not found', FCompileList.Text) = 0) and not HasErrorMarker
  then begin
    FSuccessfullCompiled:= True;
    if not FManyCompilingErrorOccured then
      FMessages.StatusMessage(FCompileFilepath + ' ' + LNGSuccessfullCompiled, 1);
  end else begin
    if not FMessages.MyIsVisible then
      FMessages.ShowIt;
    FMessages.ShowTab(K_Compiler);
    FMessages.StatusMessage(FCompileFilepath + ' ' + _('contains errors.'), 2);
    FManyCompilingErrorOccured:= True;
  end;
end;

procedure TJavaCommands.CompileOneFile;
begin
  if not FileExists(FCompileFilepath) then begin
    FMessages.StatusMessage(Format(_(LNGFileNotFound), [FCompileFilepath]), 2);
    Exit;
  end;
  FErrFile:= TPath.Combine(FConfiguration.TempDir, 'error.txt');
  FCompiler:= FConfiguration.JavaCompiler;
  FCompileDirectory:= ExtractFilePath(FCompileFilepath);
  FCompileFilename:= ExtractFileName(FCompileFilepath);
  if FCompilePackage <> '' then begin
    FCompileFilename:= ReplaceStr(FCompilePackage, '.', '/') + '/' + FCompileFilename;
    var CountSlashes:= CountChar('/', FCompileFilename);
    while CountSlashes > 0 do begin
      FCompileDirectory:= Copy(FCompileDirectory, 1, Length(FCompileDirectory)-1);
      FCompileDirectory:= Copy(FCompileDirectory, 1, LastDelimiter('\', FCompileDirectory));
      Dec(CountSlashes);
    end;
  end;
  Screen.Cursor:= crHourGlass;
  FCompileList:= TStringList.Create;
  try
    var ComJava:= GetComJava;
    if Assigned(ComJava) and Assigned(ComJava.ClassList) and
      (ComJava.ClassList.IndexOf(ChangeFileExt(FCompileFilename, '')) > -1) then  // TODO check
        FReload:= True;
    var CompilerNum:= GetCompilerNumber;
    if CompilerNum = 3 then begin
      OutputCompileInfos(3);
      CompileInternally;
      if FSuccessfullCompiled
        then FCompiler:= _('Intern compiler')
        else begin
          OutputCompileInfos(1);
          CompileWithProcess;
        end;
    end else begin
      if CompilerNum = 2 then begin
        FCompiler:= FConfiguration.LejosCompiler;
        FCompileParameter:= '';  // parameters are for Uploader only
        OutputCompileInfos(2);
      end
      else
        OutputCompileInfos(1);
      CompileWithProcess;
    end;
    FCompileList.Add('');
    if FSuccessfullCompiled
      then ShowSuccessfull
      else ShowErrors;
    LogCompile;
  finally
    FreeAndNil(FCompileList);
    Screen.Cursor:= crDefault;
  end;
end;

procedure TJavaCommands.CompileInternally;
begin
  var ComJava:= GetComJava;
  var Str:= ComJava.ExecuteCommand('compile'#4 +
              FConfiguration.JavaCompilerParameter + #4 + FCompileFilepath);
  FSuccessfullCompiled:= (UUtils.Left(Str, 3) = '+OK');
  if FSuccessfullCompiled then
    FCompileList.Text:= Right(Str, 6);
end;

procedure TJavaCommands.Compile(const Pathname, Package: string);
begin
  FMessages.ChangeTab(K_Compiler);
  if not FManyCompiling then
    FMessages.DeleteTab(K_Compiler);
  FReload:= False;
  var E:= TFEditForm(FJava.GetTDIWindowType(Pathname, '%E%'));
  if Assigned(E) then
    E.InitShowCompileErrors
  else begin
    E:= FJava.OpenEditForm(Pathname, True);
    if E.Modified then FJava.DoSave(E, False);
  end;
  FCompileParameter:= FConfiguration.GetCompileParameter(Pathname, Package, E.Encoding);
  FCompileFilepath:= Pathname;
  FCompilePackage:= Package;
  CompileOneFile;
  if FReload then
    GetComJava.JavaReset;
end;

procedure TJavaCommands.CompileForm(Form: TFEditForm);
begin
  if Assigned(Form) then
    Compile(Form.Pathname, Form.GetPackage);
end;

procedure TJavaCommands.CompileAll;
  var FileList: TStringList;
      EForm: TFEditForm;
begin
  FMessages.ChangeTab(K_Compiler);
  FMessages.DeleteTab(K_Compiler);
  FMessages.StatusMessage('');
  SetManyCompiling(True);
  FReload:= False;
  FileList:= TStringList.Create;
  FileList.Sorted:= True;
  for var I:= 0 to FJava.TDIEditFormCount - 1 do begin
    EForm:= FJava.TDIEditFormGet(I);
    if EForm.IsJava then
      FileList.Add(EForm.Pathname);
  end;

  for var I:= 0 to FileList.Count - 1 do begin
    FEditForm:= FJava.GetTDIWindow(FileList[I]) as TFEditForm;
    if not FEditForm.Visible then
      Continue;
    FEditForm.InitShowCompileErrors;
    FCompileParameter:= FConfiguration.GetCompileParameter(FEditForm.Pathname,
                          FEditForm.GetPackage, FEditForm.Encoding);
    FCompileFilepath:= FEditForm.Pathname;
    FCompilePackage:= FEditForm.GetPackage;
    CompileOneFile;
    FMessages.LBCompiler.Items.Add('');
  end;
  if FManyCompilingErrorOccured then
    FJava.ShowCompileErrors;
  SetManyCompiling(False);
  if FReload then
    GetComJava.JavaReset;
  FreeAndNil(FileList);
end;

function TJavaCommands.HasValidClass(const Programm: string): Boolean;
  var Age1, Age2: TDateTime; Path: string;
begin
  Result:= False;
  FileAge(Programm, Age1);
  Path:= GetClass(FConfiguration.GetClassPath, FConfiguration.JavaCompilerParameter, Programm);
  if Path <> '' then begin
    FileAge(Path, Age2);
    Result:= Age1 <= Age2;
  end;
end;

function TJavaCommands.AcceptableError(const Pathname: string): Boolean;
begin
  var Str:= FMessages.GetCompileError(Pathname);
  Result:= (Pos('1 error', Str) > 0) and (Pos(': constructor ', Str) > 0);
end;

procedure TJavaCommands.MakeRunJavaBat(Directory, ApplicationName,
                          CommandLine: string; WithPause: Boolean = True);
  var Path, Codepage, WinCodepage: string;
begin
  WinCodepage:= GetACP.ToString; // Codepage of Windows
  Path:= FConfiguration.TempDir + 'RunJava.bat';
  Codepage:= FConfiguration.Codepage;
  var Posi:= Pos(' ', Codepage);
  if Posi > 0 then
    Codepage:= Copy(Codepage, 1, Posi - 1);
  if IsUNC(GetCurrentDir) then
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
    if WithPause then begin
      RunJava.Add('@ECHO ON');
      RunJava.Add('PAUSE');
    end;
    try
      RunJava.SaveToFile(Path, TEncoding.ANSI);
    except
      on E: Exception do
        ErrorMsg(Format(_(LNGCanNotCreateFile), [Path, E.Message]));
    end;
  finally
    FreeAndNil(RunJava);
  end;
end;

procedure TJavaCommands.Run(const CallParameter, JavaProgram, Package: string; GUI, JavaFX: Boolean);
  var Path, Dir, ApplicationName, CommandLine, Classpath, CodePa, MyProgram: string;
      Chars: Integer;
begin
  Path:= ExpandFileName(JavaProgram);
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  FMessages.OutputLineTo(K_Interpreter, _('Starting') + ' ' + Path);

  Classpath:= FConfiguration.GetClassPath(JavaProgram, Package);
  MyProgram:= ChangeFileExt(ExtractFileName(Path), '');
  Dir:= ExtractFilePath(Path);
  if Package <> '' then begin
    MyProgram:= Package + '.' + MyProgram;
    Chars:= CountChar('.', Package) + 1;
    while Chars > 0 do begin
      Dir:= Copy(Dir, 1, Length(Dir)-1);
      Dir:= Copy(Dir, 1, LastDelimiter('\', Dir));
      Dec(Chars);
    end;
  end;

  CodePa:= '';
  ApplicationName:= FConfiguration.JavaInterpreter;
  CommandLine:= CodePa + ' -classpath ' + Classpath +
                AddWithSpace(FConfiguration.GetJavaInterpreterParameter(JavaFX)) +
                AddWithSpace(MyProgram) + AddWithSpace(CallParameter);
  if FConfiguration.ShowInterpreterCall then
    FMessages.OutputLineTo(K_Interpreter, ApplicationName + AddWithSpace(CommandLine));
  FMessages.OutputLineTo(K_Interpreter, #13#10);

  if FConsoleMode = 0 then
    if GUI then
      FConsoleMode:= 1
    else if FConfiguration.UseInterpreterWindowAsConsole then
      FConsoleMode:= 2
    else begin
      MakeRunJavaBat(Dir, ApplicationName, CommandLine);
      FConsoleMode:= 3;
    end;

  case FConsoleMode of
    1: TTask.Create(
       procedure begin
         ExecWithPipe(ApplicationName, CommandLine, Dir, JavaFX); // without console for GUI programs
       end).Start;
    2: FMessages.Run(Classpath, MyProgram, CallParameter);              // with interpreter window as console
    3: ExecWithConsoleBat(FConfiguration.TempDir + 'RunJava.bat');      // with cmd console
  end;
  FConsoleMode:= 0;
end;

procedure TJavaCommands.Upload(const JavaProgram: string);
begin
  case FConfiguration.MindstormsVersion of
    0: UploadRCX(JavaProgram);
    1: UploadNXT(JavaProgram);
    2: begin
         FConfiguration.MindstormsRun:= True;
         UploadEV3(JavaProgram);
       end;
  end;
end;

procedure TJavaCommands.UploadRCX(const JavaProgram: string);
  var Str: string;
begin
  var Filepath:= ExpandFileName(JavaProgram);
  FMessages.ShowIt;
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  if JavaProgram = 'Lejos-Firmware' then begin
    Str:= 'Download Lejos firmware' + #13#10 + '0%' +#13#10;
    MakeRunJavaBat(FConfiguration.TempDir, FConfiguration.RCXFlasher, '', False);
  end else begin
    Str:= Format(_(LNGTransferToRCX), [Filepath]) + #13#10 + '0%' +#13#10;
    MakeRunJavaBat(FConfiguration.TempDir, FConfiguration.RCXUploader,
                   ChangeFileExt(ExtractFileName(JavaProgram), ''), False);
  end;
  FMessages.OutputLineTo(K_Interpreter, Str);
  ExecWithPipe('', FConfiguration.TempDir + 'RunJava.bat', ExtractFilePath(Filepath));
  with FMessages.MInterpreter do begin
    if (Pos('100%', Lines.Text) > 0) or (Pos('Firmware unlocked', Lines.Text) > 0)
      then Str:= _(LNGSuccessfullTransfered)
      else Str:= _(LNGErrorDuringTransferToRCX);
    FMessages.OutputLineTo(K_Interpreter, Str);
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
                          ChangeFileExt(ExtractFileName(Path), ''),
               ExtractFilePath(Path));
  with FMessages.MInterpreter do begin
    if Pos('Upload successful', Lines.Text) > 0
      then Line:= FConfiguration.SetRCXNXTEV3(_(LNGSuccessfullTransfered))
      else Line:= FConfiguration.SetRCXNXTEV3(_(LNGErrorDuringTransferToRCX));
    FMessages.OutputLineTo(K_Interpreter, Line);
  end;
end;

procedure TJavaCommands.UploadEV3(const JavaProgram: string);
  var Path, Parameter: string;
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

procedure TJavaCommands.MindstormsUtility(const Bat: string);
begin
  FMessages.ShowIt;
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  var Str:= FConfiguration.LejosVerzeichnis + '\bin\' + Bat;
  FMessages.OutputLineTo(K_Interpreter, Str);
  ExecWithoutWait(Str, '', '', SW_HIDE);
end;

procedure TJavaCommands.EditBuffer(const Buffer: string);
   var LengthS, LengthSMinus1, Line: Integer; Str, Percent: string;
begin
  Str:= Buffer;
  LengthS:= Length(Str);
  while (LengthS > 0) and (Str[LengthS] <> '%') do
    Dec(LengthS);
  LengthSMinus1:= LengthS - 1;
  while (LengthSMinus1 > 0) and ('0' <= Str[LengthSMinus1]) and (Str[LengthSMinus1] <= '9') do
    Dec(LengthSMinus1);
  Percent:= Copy(Str, LengthSMinus1+1, LengthS-LengthSMinus1-1);
  if Percent = '' then
    FMessages.OutputLineTo(K_Interpreter, Str)
  else begin
    Line:= FMessages.MInterpreter.Lines.Count;
    FMessages.MInterpreter.Lines[Line-1]:=
      Copy('##################################################', 1,
           Round(StrToInt(Percent)/2)) + ' ' + Percent + '%';
  end;
end;

// calls a program, waits until ready and collects the output to a file
function TJavaCommands.ExecAndWait(const ApplicationName, CommandLine, Dir, Output: string;
                     WindowState: Word; ProcessMessage: Boolean = True): Boolean;
  var
    StartupInfo: TStartupInfo;
    SecAttr: TSecurityAttributes;
    ErrorHandle: THandle;
    Cmd, CurrentDir: string;
    WaitObject: Word;
begin
  if FProcessRunning then begin
    FTerminateProcess:= True;
    Exit(False);
  end;
  FJava.RunButtonToStop(True);
  FProcessRunning:= True;
  FTerminateProcess:= False;
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor := nil;
  SecAttr.bInheritHandle:= True;
  if Output <> '' then begin
    DeleteFile(PChar(Output));
    ErrorHandle:= CreateFile(PChar(Output), GENERIC_WRITE, FILE_SHARE_READ, @SecAttr,
                             CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if ErrorHandle = INVALID_HANDLE_VALUE then begin
      ErrorMsg('DEBUG ExecAndWait: ' + SysErrorMessage(GetLastError));
      ErrorHandle:= 0;
    end;
  end else
    ErrorHandle:= 0;

  FillChar(FProcessInformation, SizeOf(FProcessInformation), #0);
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
  Cmd:= Trim(ApplicationName + ' ' + CommandLine);
  CurrentDir:= Dir;
  if (CurrentDir = '') or not SysUtils.DirectoryExists(CurrentDir) then
    if ApplicationName <> ''
      then CurrentDir:= ExtractFilePath(ApplicationName)
      else CurrentDir:= ExtractFilePath(CommandLine);
  if IsUNC(CurrentDir) then
    CurrentDir:= '.';
  try
    Result:= CreateProcess(nil, PChar(Cmd), nil, nil, True,
                         CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                         nil, PChar(CurrentDir), StartupInfo, FProcessInformation);
    if Result then
      repeat
        WaitObject:= WaitForSingleObject(FProcessInformation.hProcess, 20);
      until (WaitObject = WAIT_OBJECT_0) or FTerminateProcess;
  finally
    TerminateTask(FProcessInformation);
    if ErrorHandle > 0 then
      CloseHandle(ErrorHandle);
    FProcessRunning:= False;
    FJava.RunButtonToStop(False);
  end;
  if ProcessMessage then
    Application.ProcessMessages; // else wine produces IO-Error
    // during TFJava.FormCreate/TFConfiguration.Init/getJavaVersion
    // a ProcessMessags starts SystemExecuteMacro/TFJava.Open to early
end;

// runs an external program, no need to wait for or close external program
function TJavaCommands.ExecWithoutWait(const ApplicationName, CommandLine, Dir: string; WindowState: Word): Boolean;
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
    Result:= CreateProcess(nil, PChar(CmdLine), nil, nil, True,
               CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
               nil, PChar(CurrentDir), StartupInfo, LocalProcessInformation);
    if not Result then
      ErrorMsg(SysErrorMessage(GetLastError));
  finally
    CloseProcessinformationHandle(LocalProcessInformation);
  end;
end;

procedure TJavaCommands.ExecWithPipe(const ApplicationName, CommandLine, Dir: string; JavaFX: Boolean = False);
  var SecAttr    : TSecurityAttributes;
      StartupInfo: TStartupInfo;
      DWExitCode : DWORD;
      UBuffer: RawByteString;
      Buffer: string;
      Read: Cardinal;
      AndroidList: TStringList;
      HWnd: THandle;
      Max: Integer;
      CreateOK: Boolean;
      CurrentDir, Cmd: string;
begin
  if FProcessRunning then begin
    FTerminateProcess:= True;
    Exit;
  end;
  FOutputLines.Clear;
  SecAttr.nLength:= SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor:= nil;
  SecAttr.bInheritHandle:= True;
  with FPipeOutput do
    if not CreatePipe(hRead, hWrite, @SecAttr, BufSize) then begin
      ErrorMsg('Error on STDOUT pipe creation: ' + SysErrorMessage(GetLastError));
      Exit;
    end;

  TThread.Queue(nil, procedure
    begin
      FJava.RunButtonToStop(True);
    end);
  FProcessRunning:= True;
  FTerminateProcess:= False;

  FillChar(FProcessInformation, SizeOf(FProcessInformation), #0);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  with StartupInfo do begin
    cb:= SizeOf(StartupInfo);
    wShowWindow:= SW_HIDE;
    dwFlags    := STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
    hStdInput  := GetStdHandle(STD_INPUT_HANDLE);
    hStdOutput := FPipeOutput.hWrite;
    hStdError  := FPipeOutput.hWrite;
  end;
  if ApplicationName = ''
    then Cmd:= CommandLine
    else Cmd:= ApplicationName + ' ' + CommandLine;
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
                       True,                    // bInheritHandles
                                                // dwCreationFlags
                       CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS or CREATE_UNICODE_ENVIRONMENT,
                       nil,                     // pEnvironment
                       PChar(CurrentDir),       // pCurrentDirectory
                       StartupInfo,             // lpStartupInfo,
                       FProcessInformation);    // lpProcessInformation
      if CreateOK then begin
        WaitForInputIdle(FProcessInformation.hProcess, MaxInt);
        if JavaFX then begin
          Max:= 0;
          repeat
            Sleep(100);
            HWnd:= FindWindow('GlassWndClass-GlassWindowClass-2', nil);
            Inc(Max);
          until (Max = 100) or (HWnd <> 0);
          ShowWindow(HWnd, SW_SHOW);
        end;
        repeat
          GetExitCodeProcess(FProcessInformation.hProcess, DWExitCode);
          Application.ProcessMessages;
          Sleep(20);
          if PeekNamedPipe(FPipeOutput.hRead, nil, 0, nil, @Read, nil) and (Read > 0) then begin
            SetLength(UBuffer, Read);
            ReadFile(FPipeOutput.hRead, UBuffer[1], Read, Read, nil);
            Buffer:= string(AnsiToUtf8(string(UBuffer)));
            if FConfiguration.MindstormsMode and (FConfiguration.MindstormsVersion = 0)
              then EditBuffer(Buffer)
            else if FConfiguration.AndroidMode then begin
              AndroidList:= TStringList.Create;
              AndroidList.Text:= Buffer;
              AdjustCarets(AndroidList);
              FMessages.ChangeTab(K_Compiler);
              FMessages.OutputTo(K_Compiler, AndroidList);
              FreeAndNil(AndroidList);
            end else
              TThread.Queue(nil, procedure
                begin
                  FMessages.OutputToTerminal(Buffer);
                end);
          end;
        until (DWExitCode <> STILL_ACTIVE) or FTerminateProcess;
      end else
        ErrorMsg(Format(_(LNGUnabledToExecute), [Cmd]) + ' ' + SysErrorMessage(GetLastError));
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    TerminateTask(FProcessInformation);
    ClosePipeHandles(FPipeOutput);
    FProcessRunning:= False;
    TThread.Queue(nil, procedure
      begin
        FJava.RunButtonToStop(False);
      end);
  end;
end;

procedure TJavaCommands.ExecWithConsoleBat(const Batchfile: string);
  var StartupInfo: TStartupInfo;
      DWExitCode: DWORD;
begin
  if FProcessRunning then begin
    FTerminateProcess:= True;
    Exit;
  end;
  FJava.RunButtonToStop(True);
  FProcessRunning:= True;
  FTerminateProcess:= False;
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  with StartupInfo do begin
    cb:= SizeOf(StartupInfo);
    dwFlags:= STARTF_USESHOWWINDOW;
    wShowWindow:= SW_SHOWNORMAL;
  end;
  try
    if CreateProcess(nil,                     // lpApplicationName
                     PChar(Batchfile),        // lpCommandLine
                     nil,                     // lpProcessAttributess
                     nil,                     // lpThreadAttributess
                     True,                    // bInheritHandles
                                              // dwCreationFlags
                     CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                     nil,                     // pEnvironment
                     nil,                     // pCurrentDirectory
                     StartupInfo,             // lpStartupInfo,
                     FProcessInformation)      // lpProcessInformation
    then
      repeat
        GetExitCodeProcess(FProcessInformation.hProcess, DWExitCode);
        Application.ProcessMessages;
        Sleep(20);
      until (DWExitCode <> STILL_ACTIVE) or FTerminateProcess
    else
      ErrorMsg(Format(_(LNGUnabledToExecute), [Batchfile]) + ' ' + SysErrorMessage(GetLastError));
  finally
    TerminateTask(FProcessInformation);
    FProcessRunning:= False;
    FJava.RunButtonToStop(False);
  end;
end;

function TJavaCommands.ShellExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
begin
  Result:= ShellExecute(Application.MainForm.Handle, 'open',
             PChar(FileName), PChar(Params), PChar(DefaultDir), ShowCmd);
end;

procedure TJavaCommands.Terminate;
begin
  if Assigned(FProcessRunningComJava) then
    FProcessRunningComJava.JavaReset;
  FTerminateProcess:= True;
end;

procedure TJavaCommands.AppletViewer(Pathname: string);
  var FileName, Params: string;
begin
  FMessages.ShowTab(K_Interpreter);
  FMessages.DeleteTab(K_Interpreter);
  FMessages.StatusMessage('Appletviewer: ' + Pathname);

  Pathname:= ExpandFileName(Pathname);
  FileName:= FConfiguration.JavaAppletviewer;
  Params  := ' -J-Djava.security.policy=file:'+
             HideBlanks(FConfiguration.EditorFolder + 'java.policy.applet') + ' ' +
             HideBlanks(Pathname);
  ChDir(ExtractFilePath(Pathname));
  if FConfiguration.ShowInterpreterCall then
    FMessages.OutputLineTo(K_Interpreter, FileName + ' ' + Params);
  ExecWithPipe(FileName, Params, '.');
end;

procedure TJavaCommands.SetManyCompiling(OnOff: Boolean);
begin
  FManyCompiling:= OnOff;
  FManyCompilingErrorOccured:= False;
end;

procedure TJavaCommands.RunTests(AClass: TClass; const Method: string);
  var PictureNr, Idx, Posi: Integer;
      Call, AClassname, Classpath, Output: string;
      Ite: IModelIterator;
      AMethod: TOperation;
      Node: TTreeNode;
      StringList: TStringList;
begin
  AClassname:= ChangeFileExt(ExtractFileName(AClass.Pathname), '');
  Classpath:= FConfiguration.GetClassPath;
  Posi:= Pos(FConfiguration.JUnitJarFile, Classpath);
  if Posi > 0 then
    Delete(Classpath, Posi, Length(FConfiguration.JUnitJarFile));

  FErrFile:= FConfiguration.TempDir + 'error.txt';
  Call:=  '-jar ' + HideBlanks(FConfiguration.JUnitJarFile) +
            ' -cp ' + Classpath + ' ' + FConfiguration.JUnitParameter +
            ' --disable-ansi-colors --details=tree';
  if Method <> 'Class'
    then Call:= Call + ' -m ' + AClassname + #4 + Method
    else Call:= Call + ' -c ' + AClassname;

  FMessages.LBMessages.Clear;
  FMessages.OutputLineTo(K_Messages, FConfiguration.JavaInterpreter + ' ' + Call);
  if ExecAndWait(FConfiguration.JavaInterpreter, Call, ExtractFilePath(AClass.Pathname), FErrFile, SW_HIDE) then begin
    FMessages.ShowTab(K_Messages);
    FMessages.ShowMessages(FErrFile);
    StringList:= TStringList.Create;
    StringList.LoadFromFile(FErrFile);

    with FJUnitTests do begin
      FTVJUnitTests.Items.BeginUpdate;
      DeleteData;
      Ite:= AClass.GetOperations;
      PJUnit.Color:= clGreen;
      PictureNr:= 0;
      while Ite.HasNext do begin
        AMethod:= Ite.Next as TOperation;
        if (AMethod.Annotation <> 'Test') and (AMethod.Annotation <> 'ParameterizedTest') then
          Continue;
        if (AMethod.Name <> Method) and (Method <> 'Class') then
          Continue;
        Idx:= 1;
        Posi:= Pos('-- ' + AMethod.Name, StringList[Idx]);
        while (Posi = 0) and (Idx < StringList.Count-1) do begin
          Inc(Idx);
          Posi:= Pos('-- ' + AMethod.Name, StringList[Idx]);
          if not IsWordInLine(AMethod.Name, StringList[Idx]) then
            Posi:= 0;
        end;
        if Idx < StringList.Count then begin
          if Pos('[OK]', StringList[Idx]) > 0 then
            PictureNr:= 0
          else begin
            PictureNr:= 1;
            PJUnit.Color:= clRed;
          end;
          Output:= Copy(StringList[Idx], Posi + 3, 255);
        end;
        Node:= FTVJUnitTests.Items.AddObject(nil, Output, TInteger.Create(AMethod.LineS));
        Node.ImageIndex:= PictureNr;
        Node.SelectedIndex:= PictureNr;
        Node.HasChildren:= False;
      end;
      FTVJUnitTests.Items.EndUpdate;
    end;
    FreeAndNil(StringList);
  end;
  FJava.UpdateLayoutRightDockPanel;
end;

end.
