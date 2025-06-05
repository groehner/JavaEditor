{
Copyright (c) 1998-2001 Jonathan Revusky
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. All advertising materials mentioning features or use of this software
must display the following acknowledgement:
This product includes software developed by Jonathan Revusky
4. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

}

unit JavaRuntime;

// This unit is devoted to locating the JVM install directory
// and using the invocation API to create a JVM.
// All of the code here is Win32-specific heuristics.

interface

uses Windows, Messages, Classes, SysUtils, AnsiStrings, Dialogs,
     JNI, UJniWrapper2;

type

  PPJavaVM = ^PJavaVM;
  TGetDefaultArgs = function (args: Pointer): jint; stdcall;
  TCreateVM = function (vm: PPJavaVM; penv: PPJNIEnv; Posi: Pointer): jint; stdcall;
  TGetCreatedVMs = function (vmBuf: PPJavaVM; buflen: Integer; nVMs: PJInt): jint; stdcall;
  TExitProc = procedure (exitCode: jint); stdcall;
  TAbortProc = procedure; stdcall;
  TPrintf = function (filepointer: Pointer; const format: PAnsiChar; args: va_list): jint; stdcall;

  EJvmException = class(Exception);
  EJavaRuntimeNotFound = class(Exception);
  EJavaRuntimeCreation = class(Exception);
  EClasspathException = class(Exception);

  TClassPath = class(TStringList)
  private
    constructor Create;
    procedure addDir(const dir: string);
    procedure addPath(path: string);
  public
    function FullPath: string;
    class function getDefault: TClasspath;
  end;

  // class to encapsulate the location of the java runtime
  // and the use of the JNI invocation API.
  TJavaRuntime = class
  private
    FHotspot: Boolean;
    FJavaHome: string;
    FJavaPath: string;
    FRuntimeLib: string;
    FJavaVM:  TJavaVM;
    DLLHandle: THandle;
    vmargs: JDK1_1InitArgs;
    vmargs2: JavaVMInitArgs;
    ShowOptions: string;
    FClasspath: TClasspath;
    FProperties: TStrings;
    FExitProc: TExitProc;
    FAbortProc: TAbortProc;
    FPrintf: TPrintf;
    FDebugPort, FVerbose, FDisableAsyncGC, FVerboseGC, FEnableClassGC,
    FVerifyMode, FCheckSource, FMinHeapSize, FMaxHeapSize, FJavaStackSize,
    FNativeStackSize: Integer;
    FDebugging: Boolean;
    FConnectionAddress: string;
    function FindJava12: Boolean;
    function getClasspath: string;
    procedure setClasspath(const Str: string);
    procedure setNativeStackSize(Size: Integer);
    procedure setJavaStackSize(Size: Integer);
    procedure setMinHeapSize(Size: Integer);
    procedure setMaxHeapSize(Size: Integer);
    procedure setVerifyMode(Arg: Integer);
    procedure setCheckSource(arg: Integer);
    procedure setEnableClassGC(B: Boolean);
    procedure setVerboseGC(B:Boolean);
    procedure SetDisableAsyncGC(B: Boolean);
    procedure setVerbose(B: Boolean);
    procedure setDebugPort(Port: Integer);
    procedure setDebugging(Arg: Boolean);
    procedure setConnectionAddress(const ConnAddress: string);
    procedure setAbortProc(proc: TAbortProc);
    procedure setExitProc(proc: TExitProc);
    procedure setPrintf(printproc: TPrintf);
    procedure LoadJVMDLL;
    procedure SetJVMOptions;
  public
    ErrorMessage: string;
    // processes a command-line option
    procedure processCommandLineOption(const Str: string);
    // processes a bunch of command line options passed in a container.
    procedure processCommandLine(Options: TStrings);
    procedure addProperty(const Str: string);
    procedure addToClasspath(const filename: string);
    function GetVM: TJavaVM; //Instantiates the JVM
    procedure CallMain(const aClassName: string; args: TStrings);
    procedure CallExit(val: Integer);
    property RuntimeLib: string read FRuntimeLib;
    property JavaHome: string read FJavaHome;
    property Classpath: string read getClasspath write setClasspath;
    property Hotspot: Boolean read FHotspot write FHotspot;
    
    // write-only properties that only work before instantiating VM.
    property NativeStackSize: Integer write SetNativeStackSize;
    property JavaStackSize: Integer write SetJavaStackSize;
    property CheckSource: Integer write setCheckSource;
    property MinHeapSize: Integer write setMinHeapSize;
    property MaxHeapSize: Integer write setMaxHeapSize;
    property VerifyMode: Integer write setVerifyMode;
    property EnableClassGC: Boolean write setEnableClassGC;
    property VerboseGC: Boolean write setVerboseGC;
    property DisableAsyncGC: Boolean write setDisableAsyncGC;
    property Verbose: Boolean write setVerbose;
    property DebugPort: Integer write setDebugPort;
    property Debugging: Boolean write setDebugging;
    property ConnectionAddress: string write setConnectionAddress;
    property AbortProc: TAbortProc write setAbortProc;
    property ExitProc: TExitProc write setExitProc;
    property Printf: TPrintf write setPrintf;

    constructor Create(const JavaPath: string);
    destructor Destroy; override;
    class function GetDefault(const JavaPath: string): TJavaRuntime;
  end;

implementation

uses UUtils;

const
  BootClasspath: string = '';

var
  CreateVM: TCreateVM;
  GetCreatedVMs: TGetCreatedVMs;
  GetDefaultArgs: TGetDefaultArgs;
  cpath: TClasspath; // the singleton TClasspath instance.
  DefaultRuntime: TJavaRuntime = nil; // singleton JavaRuntime instance.

  procedure TJavaRuntime.LoadJVMDLL;
  begin
    if DLLHandle <> 0 then Exit; // already initialized.
    DLLHandle:= SafeLoadLibrary(FRuntimeLib);
    if DLLHandle = 0 then begin
      errormessage:= 'Could not load DLL ' + FRuntimeLib;
      Exit;
    end;
    @CreateVM      := getProcAddress(DLLHandle, 'JNI_CreateJavaVM');
    @GetDefaultArgs:= getProcAddress(DLLHandle, 'JNI_GetDefaultJavaVMInitArgs');
    @GetCreatedVMs := getProcAddress(DLLHandle, 'JNI_GetCreatedJavaVMs');
    if (@CreateVM = Nil) or (@GetDefaultArgs = Nil) or (@GetCreatedVMs = Nil) then begin
      errormessage:= 'Dynamic Link Library ' + FRuntimeLib + ' is not valid.';
      Exit;
    end;
    vmargs.version := $00010001;
    vmargs2.version:= $00010002;
    GetDefaultArgs(@vmargs);
  end;

  function TJavaRuntime.GetVM: TJavaVM;
    var
      PVM: PJavaVM;
      penv: PJNIEnv;
      args: Pointer;
  begin
    Result:= FJavaVM;
    if FJavaVM <> nil then Exit;
    PVM:= nil;
    PEnv:= nil;
    // not working on 64-Bit-Version, makes PipeError
    // on 64-Bit Systems with 64-Bit JDK and 32-Bit client/jvm.dll CreateVM doesn't work

    {$IFDEF  WIN32}
    if not FileIs32Bit(FRuntimeLib) then begin
      errormessage:= 'The JVM ' + FRuntimeLib + ' is 64-Bit and cannot be used!'#13#10'Install a separate 32-Bit JDK or a 64-Bit JavaEditor.';
      Exit;
    end;
    {$ENDIF}
    {$IFDEF WIN64}
    if FileIs32Bit(FRuntimeLib) then begin
      errormessage:= 'The JVM ' + FRuntimeLib + ' is 32-Bit and cannot be used!'#13#10'Install a separate 64-Bit JDK or a 32-Bit JavaEditor.';
      Exit;
    end;
    {$ENDIF}

    if @CreateVM = nil then begin
      LoadJVMDLL;
      if errormessage <> '' then Exit;
    end;

    SetJVMOptions;
    args:= @vmargs2;
    {$D-}
    // during debugging you get an error here
    // without debugging there is no error
    if CreateVM(@PVM, @penv, args) < 0 then begin
      errormessage:= 'Could not create JVM' + #13#10 +
                     'DLL: ' + RuntimeLib + #13#10 +
                     'Options: ' + ShowOptions;
      Exit;
    end;
    {$D+}
    TJavaVM.setThreadPenv(penv);
    FJavaVM:= TJavaVM.Create(PVM);
    Result:= FJavaVM;
  end;

  procedure TJavaRuntime.SetJVMOptions;
  var
    Int, Posi: Integer;
    Str: AnsiString;
    cp, cp1: string;
    PVMOption, PVO: PJavaVMOption;
  begin
    // 1 = classpath
    // 2 = -XX:ErrorFile
    VMArgs2.Noptions:= 2 + FProperties.Count;
    if (FVerbose <> 0) or (FVerboseGC <> 0) then Inc(VMArgs2.Noptions);
    if FVerboseGC <> 0 then Inc(VMArgs2.Noptions);
    if FMinHeapSize > 0 then Inc(VMArgs2.Noptions);
    if FMaxHeapSize > 0 then Inc(VMArgs2.Noptions);
    if BootClasspath <> '' then Inc(VMArgs2.Noptions);
    if FEnableClassGC <> 0 then Inc(VMArgs2.NOptions);
    if FDebugging then Inc(VMArgs2.NOptions);

    if Assigned(FExitProc) then Inc(VMargs2.NOptions);
    if Assigned(FAbortProc) then Inc(VMArgs2.NOptions);
    if Assigned(FPrintf) then Inc(VMArgs2.NOptions);

    vmargs2.ignoreUnrecognized:= True;
    PVMOption:= AllocMem(sizeof(JavaVMOption) * VMargs2.NOptions);
    PVO:= PVMOption;
    ShowOptions:= 'Anzahl = ' + IntToStr(VMargs2.NOptions) + #13#10;

    Str:= '-Djava.class.path=' + AnsiString(Classpath);
    PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar(Str));
    PVO^.extraInfo:= nil;
    Inc(PVO);

    ShowOptions:= ShowOptions + '-Djava.class.path='  + #13#10;
    cp:= Classpath + ';';
    Posi:= 1 ;
    while Posi > 0 do begin
      Posi:= Pos(';', cp);
      cp1:= Copy(cp, 1, Posi);
      Delete(cp, 1, Posi);
      ShowOptions:= ShowOptions + cp1 + #13#10;
    end;

    for Int:= 0 to FProperties.Count - 1 do begin
      Str:= '-D' + AnsiString(FProperties[Int]);
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar(Str));
      Inc(PVO);
      ShowOptions:= ShowOptions + string(Str) + #13#10;
    end;

    if (FVerbose <> 0) or (FVerboseGC <> 0) then begin
      Str:= '-verbose:';
      if FVerbose <> 0
        then Str:= Str + 'class';
      if FVerboseGC <> 0
        then Str:= Str + ',';
      if FVerboseGC <> 0
        then Str:= Str + 'gc';
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar(Str));
      Inc(PVO);
      ShowOptions:= ShowOptions + string(Str) + #13#10;
    end;

    if FMinHeapSize > 0 then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar(AnsiString('-Xms' + IntToStr(FMinHeapSize))));
      Inc(PVO);
      ShowOptions:= ShowOptions + '-Xms' + IntToStr(FMinHeapSize) + #13#10;
    end;

    if FMaxHeapSize > 0 then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar(AnsiString('-Xmx' + IntToStr(FMaxHeapSize))));
      Inc(PVO);
      ShowOptions:= ShowOptions + '-Xmx' + IntToStr(FMaxHeapSize) + #13#10;
    end;
      
    if FEnableClassGC <> 0 then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar('-Xnoclassgc'));
      Inc(PVO);
      ShowOptions:= ShowOptions + '-Xnoclassgc' + #13#10;
    end;
      
    if BootClasspath <> '' then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar(AnsiString('-Xbootclasspath/p:' + BootClasspath)));
      Inc(PVO);
      ShowOptions:= ShowOptions + '-Xbootclasspath/p:' + BootClasspath + #13#10;
    end;

    if FDebugging then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar(AnsiString('-agentlib:jdwp=transport=dt_shmem,address='+FConnectionAddress+',server=y,suspend=n')));
      Inc(PVO);
      ShowOptions:= ShowOptions + '-agentlib:jdwp=transport=dt_shmem,address='+FConnectionAddress+',server=y,suspend=n' + #13#10;
    end;

    if Assigned(FPrintf) then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar('exit'));
      PVO^.ExtraInfo:= @FPrintf;
      Inc(PVO);
      ShowOptions:= ShowOptions + 'exit' + #13#10;
    end;

    if Assigned(FExitProc) then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar('exit'));
      PVO^.ExtraInfo:= @FExitProc;
      Inc(PVO);
      ShowOptions:= ShowOptions + 'exit' + #13#10;
    end;

    if Assigned(FAbortProc) then begin
      PVO^.optionString:= AnsiStrings.StrNew(PAnsiChar('abort'));
      PVO^.ExtraInfo:= @FAbortProc;
      Inc(PVO);
      ShowOptions:= ShowOptions + 'abort' + #13#10;
    end;

    PVO^.OptionString:= AnsiStrings.StrNew(PAnsiChar('-XX:ErrorFile=./hs_err_pid<pid>.log'));
    PVO^.extraInfo:= nil;

    vmargs2.options:= PVMOption;
    vmargs2.version:= $00010002;
  end;

  //convenience wrappers.
  
  procedure TJavaRuntime.CallMain(const aClassName: string; args: TStrings);
  begin
    TJavaVM.CallMain(aClassName, args);
  end;
  
  procedure TJavaRuntime.CallExit(val: Integer);
  begin
    TJavaVm.CallExit(val);
  end;

  procedure TJavaRuntime.processCommandLineOption(const Str: string);
    var L: string;

    function extractSize(Str: string): Integer;
    begin
      if Str[Length(Str)] = 'k'
        then Result:= $400
      else
        if Str[Length(Str)] = 'm'
          then Result:= $100000
          else Result:= 1;
      if Result <> 1
        then Str:= Copy(Str, 1, Length(Str)-1);
      Result:= Result * StrToIntDef(Str, 0);
    end;

  begin
    L := LowerCase(Str);
    if (L = '-v') or (L = 'verbose') 
      then Verbose:= True
    else if (L = '-verbosegc') 
      then VerboseGC:= True
    else if (L = '-noasync') 
      then DisableAsyncGC:= True
    else if (L = '-noclassgc') 
      then EnableClassGC:= False
    else if (L = '-verify') 
      then VerifyMode:= 2
    else if (L = '-noverify') 
      then VerifyMode:= 0
    else if (L = '-verifyremote') 
      then VerifyMode:=1
    else if (L = '-nojit') 
      then addProperty('java.compiler=')
    else if Copy(L, 1, 3) = '-cp' 
      then FClasspath.addPath(Copy(Str, 5, Length(Str)))
    else if Copy(L, 1, 10) = '-classpath' 
      then FClasspath.addPath(Copy(Str, 12, Length(Str)))
    else if Copy(L, 1, 2) = '-d' 
      then addProperty(Copy(Str, 3, Length(Str)))
    else if Copy(L, 1, 3) = '-ms' 
      then MinHeapSize:= ExtractSize(Copy(L, 4, Length(L)))
    else if Copy(L, 1, 3) = '-mx' 
      then MaxHeapSize:= ExtractSize(Copy(L, 4, Length(L)))
    else if Copy(L, 1, 3) = '-ss' 
      then NativeStackSize:= ExtractSize(Copy(L, 4, Length(L)))
    else if Copy(L, 1, 3) = '-oss' 
      then NativeStackSize:= ExtractSize(Copy(L, 5, Length(L)));
  end;

  procedure TJavaRuntime.processCommandLine(Options: TStrings);
    var Int: Integer;
  begin
    for Int:= 0 to Options.Count-1 do
      processCommandLineOption(Options[Int]);
  end;

  class function TJavaRuntime.GetDefault(const JavaPath: string): TJavaRuntime;
  begin
    if DefaultRuntime = nil then
      try
        DefaultRuntime:= TJavaRuntime.Create(JavaPath);
      finally
      end;
    Result:= DefaultRuntime;
  end;
  
  procedure TJavaRuntime.addToClasspath(const filename: string);
  begin
    FClasspath.addDir(filename);
  end;

  function TJavaRuntime.getClasspath: string;
    var CPath: TClasspath;
  begin
    CPath:= TClasspath.getDefault;
    Result:= CPath.Fullpath;
  end;
  
  procedure TJavaRuntime.setClasspath(const Str: string);
  begin
    FClasspath:= TClasspath.getDefault;
    FClasspath.addPath(Str);
  end;

  constructor TJavaRuntime.Create(const JavaPath: string);
  begin
    FJavaPath:= JavaPath;
    if not FindJava12 then begin
      ErrorMessage:= 'Java 2 runtime not found. Looked for '#13#10
        + JavaPath + '\jre\bin\client\jvm.dll'#13#10
        + JavaPath + '\jre\bin\server\jvm.dll'#13#10
        + JavaPath + '\bin\client\jvm.dll'#13#10
        + JavaPath + '\bin\server\jvm.dll';
      Exit;
    end;
    ErrorMessage:= '';
    DefaultRuntime:= Self; // set the singleton
    FClasspath:= TClasspath.getDefault;
    FProperties:= TStringList.Create;
    FVerifyMode:= 1;
  end;

  destructor TJavaRuntime.Destroy;
    var PVMOptions, PVO: PJavaVMOption;
        Count: Integer;
  begin
    DefaultRuntime:= Nil;
    if dllHandle <> 0 then
      if FreeLibrary(dllHandle) then
        dllHandle:= 0;
    FreeAndNil(FClasspath);
    FreeAndNil(FProperties);

    PVMOptions:= vmargs2.options;
    Count:= vmargs2.NOptions;
    PVO:= PVMOptions;
    while Count > 0 do begin
      AnsiStrings.StrDispose(PVO.OptionString);
      Inc(PVO);
      Dec(Count);
    end;
    FreeMem(PVMOptions);
    FreeAndNil(FJavaVM);
    inherited Destroy;
  end;
  
  function TJavaRuntime.FindJava12: Boolean;
    var Str, Key: string;
  begin
    Result:= False;
    key:= FJavaPath;
    Str:= key + '\jre\bin\client\jvm.dll';
    if not FileExists(Str) then
      Str:= key + '\jre\bin\server\jvm.dll';
    if not FileExists(Str) then
      Str:= key + '\bin\client\jvm.dll';
    if not FileExists(Str) then
      Str:= key + '\bin\server\jvm.dll';
    if FileExists(Str) then begin
      Result:= True;
      FRuntimeLib:= Str;
      FJavaHome:= key;
    end
  end;

  procedure TJavaRuntime.SetNativeStackSize(Size: Integer);
  begin
    if Size > 0 then
       FNativeStackSize:= Size;
  end;

  procedure TJavaRuntime.SetJavaStackSize(Size: Integer);
  begin
    if Size > 0 then
       FJavaStackSize:= Size;
  end;
  
  procedure TJavaRuntime.setMinHeapSize(Size: Integer);
  begin
    if Size  > 0 then 
       FMinHeapSize:= Size;
  end;
  
  procedure TJavaRuntime.setMaxHeapSize(Size: Integer);
  begin
    if Size  > 0 then 
       FMaxHeapSize:= Size;
  end;
  
  procedure TJavaRuntime.setVerifyMode(Arg: Integer);
  begin
    FVerifyMode:= Arg;
  end;
  
  procedure TJavaRuntime.SetCheckSource(arg: Integer);
  begin
    FCheckSource:= arg;
  end;

  procedure TJavaRuntime.SetEnableClassGC(B: Boolean);
  begin
    FEnableClassGC:= Integer(B); 
  end;
  
  procedure TJavaRuntime.setVerboseGC(B:Boolean);
  begin
    FVerboseGC:= Integer(B);
  end;
  
  procedure TJavaRuntime.SetDisableAsyncGC(B: Boolean);
  begin
    FDisableAsyncGC:= Integer(B);
  end;
  
  procedure TJavaRuntime.setVerbose(B: Boolean);
  begin
    FVerbose:= Integer(B);
  end;
  
  procedure TJavaRuntime.setDebugPort(Port: Integer);
  begin
    FDebugPort:= Port;
  end;

  procedure TJavaRuntime.setDebugging(Arg: Boolean);
  begin
    FDebugging:= Arg;
  end;

  procedure TJavaRuntime.setConnectionAddress(const ConnAddress: string);
  begin
    FConnectionAddress:= ConnAddress;
  end;

  procedure TJavaRuntime.setAbortProc(proc: TAbortProc);
  begin
    FAbortproc:= proc;
  end;
  
  procedure TJavaRuntime.setExitProc(proc: TExitProc);
  begin
    FExitProc:= Proc;
  end;
  
  procedure TJavaRuntime.setPrintf(printproc: TPrintf);
  begin
    fprintf:= printproc;
  end;
  
  procedure TJavaRuntime.addProperty(const Str: string);
  begin
    FProperties.Add(Str);
  end;

  // -- TClassPath -------------------------------------------------------------

  class function TClasspath.getDefault: TClassPath;
  begin
    if cpath = nil then
      cpath:= TClasspath.Create;
    Result:= cpath;
  end;

  constructor TClasspath.Create;
  begin
    inherited;
    Duplicates:= dupIgnore;
  end;

  procedure TClasspath.addPath(Path: string);
  var
    Len: Integer;
    Dirs : TStringList;
    Int: Integer;
  begin
    Path:= Path + ';';
    Dirs:= TStringList.Create;
    try
      repeat
        Len:= Pos(';', Path);
        if Len > 1 then
          Dirs.Add(Copy(Path, 1, Len-1));
        Path:= Copy(Path, Len + 1, Length(Path));
      until Len=0;
      for Int:= Dirs.Count-1 downto 0 do
        addDir(Dirs[Int]);
    finally
      FreeAndNil(Dirs);
    end;
  end;
  
  procedure TClasspath.addDir(const dir: string);
  var
    Str: string;
    Int: Integer;
  begin
    if Dir = '.' then
      Add(Dir)
    else begin
      Str:= ExpandFileName(dir);
      if (Str[Length(Str)] = '\') and (Str[Length(Str)-1] <> ':') then
        Str:= Copy(Str, 1, Length(Str)-1);
      Int:= IndexOf(Str);
      if Int>=0 then
        Delete(Int);
      Add(Str);
    end;
  end;

  function TClasspath.FullPath: string;
    var Int: Integer;
  begin
    Result:= '';
    for Int:= Count downto 1 do begin
      if Int < Count then
        Result:= Result + ';';
      Result:= Result + Strings[Int-1];
    end;
  end;

end.
