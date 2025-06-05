unit UThreads;

interface

uses Windows, Messages, Classes, JNI, UJniWrapper2;

type
  // this thread is waiting for outputs from the java ouput pipe
  TConsoleThread = class(TThread)
  private
    ReadHandle: THandle;
  public
    Buffer: AnsiString;
    aException: string;
    constructor Create(JavaOutputReadHandle: THandle);
    procedure ShowLines;
    procedure Execute; override;
  end;

  // this thread is executing a java method
  TExecThread = class(TThread)
  private
    JavaVM: TJavaVM;
    aJavaMethod: TJavaMethod;
    theParams: TJavaParams;
    aJavaTyp: TNumType;
    aClass: TJavaClass;
    Mode: Integer;
  public
    // return values
    aJavaObject: TJavaObject;
    aJavaValue: TJavaValue;
    constructor Create(const Javapath: string);
    procedure SetMethodCall(bJavaMethod: TJavaMethod;
                            bParams: TJavaParams;
                            bJavaObject: TJavaObject;
                            bJavaTyp: TNumType);
    procedure SetCreateObject(myClass: TJavaClass;
                              myParams: TJavaParams);
    procedure Execute; override;
  end;

  // this thread is hiding the console window
  THideThread = class (TThread)
    ConsoleWindow: THandle;
    constructor Create;
    function GetConsoleWindow: THandle;
    procedure Execute; override;
  end;


implementation

  uses SysUtils, JavaRuntime, UUtils, UJe2Java, UComJava2;

  {--- ConsoleThread ----------------------------------------------------------}

  constructor TConsoleThread.Create(JavaOutputReadHandle: THandle);
  begin
    // read from the java output
    ReadHandle:= JavaOutputReadHandle;
    Buffer:= '';
    aException:= '';
    inherited Create(False);  // not suspended, execute immediatly startet
  end;

  procedure TConsoleThread.ShowLines;
  begin
    myComJava2.ShowInMemo(string(Buffer));
    if Pos('Exception', string(Buffer)) > 0
      then aException:= string(Buffer)
      else FJe2Java.WriteToConsolePipe(string(Buffer));
    Buffer:= '';
  end;

  procedure TConsoleThread.Execute;
    var Read: Cardinal;
  begin
    try
      repeat
        Read:= 0;
        if PeekNamedPipe(ReadHandle, nil, 0, nil, @Read, nil) and (Read > 0) then begin
          SetLength(Buffer, Read);
          ReadFile(ReadHandle, Buffer[1], Read, Read, nil);
          Synchronize(ShowLines);
        end;
      until Terminated;
    except
      on E: Exception do
        ErrorMsg('Error: ' + E.Message);
    end;
  end;

  {--- ExecThread -------------------------------------------------------------}

  constructor TExecThread.Create(const Javapath: string);
  begin
    inherited Create(True);  // suspended
    JavaVM:= TJavaRuntime.getDefault(Javapath).GetVM;
  end;

  procedure TExecThread.SetMethodCall(bJavaMethod: TJavaMethod;
                                      bParams: TJavaParams;
                                      bJavaObject: TJavaObject;
                                      bJavaTyp: TNumType);
  begin
    Self.aJavaMethod:= bJavaMethod;
    Self.theParams:= bParams;
    Self.aJavaObject:= bJavaObject;
    Self.aJavaTyp:= bJavaTyp;
    Mode:= 0;
  end;

  procedure TExecThread.SetCreateObject(myClass: TJavaClass; myParams: TJavaParams);
  begin
    Self.aClass:= myClass;
    Self.theParams:= myParams;
    Mode:= 1;
  end;

  procedure TExecThread.Execute;
    var Env: PJNIEnv;
        aJValue: JValue;
  begin
    try
      JavaVM.pvm^.AttachCurrentThread(JavaVM.pvm, @Env, nil);
      JavaVM.setThreadPEnv(Env);
      if Mode = 0 then begin
        aJValue   := aJavaMethod.Call(theParams, aJavaObject);
        aJavaValue:= TJavaValue.Create(aJValue, aJavaTyp, '');
        aJavaValue.Error:= aJavaMethod.Error;
        end
      else begin
        aJavaObject:= TJavaObject.Create(aClass, theParams);
        aJavaObject.Global:= True;
      end;
    except
      on E: Exception do
        ErrorMsg('Exec Error: ' + E.Message);
    end;
  end;

  {--- HideThread -------------------------------------------------------------}

  constructor THideThread.Create;
  begin
    ConsoleWindow:= 0;
    inherited Create(False);  // not suspended
  end;

  function THideThread.GetConsoleWindow: THandle;
    var Str: string;
  begin
    if ConsoleWindow = 0 then begin
      Setlength(Str, MAX_PATH + 1);
      if GetConsoleTitle(PChar(Str), MAX_PATH) <> 0 then begin
        Str[1]:= '$';
        SetConsoleTitle(PChar(Str));
        ConsoleWindow:= FindWindow(nil, PChar(Str));
        SetConsoleTitle('Console');
      end;
    end;
    Result:= ConsoleWindow;
  end;

  procedure THideThread.Execute;
  begin
    while ConsoleWindow = 0 do
      ConsoleWindow:= GetConsoleWindow;
    repeat
      ShowWindow(ConsoleWindow, SW_HIDE);
    until not IsWindowVisible(ConsoleWindow);
  end;

end.
