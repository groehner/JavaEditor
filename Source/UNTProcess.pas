unit UNTProcess;

interface

uses Windows;

type

  TNTProcess = class
  private
    fPID : Integer;
    procedure SetPID (value : Integer);
    function GetBaseName : string;
  public
    property PID : Integer read fPID write SetPID;
    property BaseName : string read GetBaseName;
  end;

  TNTProcessList = class
  private
    fPIDList : PInteger;
    fPIDCount : Integer;
    PSApiHandle: Integer;

    function GetPID (index : Integer) : Integer;
  protected
  public
    allOk: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
    procedure GetProcess (n : Integer; var process : TNTProcess);
    property Count : Integer read fPIDCount;
    property PID [index : Integer] : Integer read GetPID;
  end;

  procedure ProcessTerminate(const TermProcessID: string);
  procedure ProcessTerminateID(ProcessID: LongInt);

implementation

  uses SysUtils, TlHelp32;

  type
    TEnumProc = function (pidList : PInteger; cb : Integer; var cbNeeded : Integer): boolean; stdcall;
    TGetBase = function (hProcess : THandle; module : HInst; BaseName : Pchar; size : Integer) : Integer; stdcall;

  var
    EnumProcessesF: TEnumProc;
    GetModuleBaseNameF: TGetBase;

constructor TNTProcessList.Create;
begin
  inherited Create;
  allOk:= True;
  PSApiHandle:= LoadLibrary('PSAPI.DLL');
  if PSApiHandle <> 0 then begin
    @EnumProcessesF:= GetProcAddress(PSApiHandle, 'EnumProcesses');
    @GetModuleBaseNameF:= GetProcAddress(PSApiHandle, 'GetModuleBaseNameA');
  end else begin
    //ErrorMsg('Couldn''t load PSAPI.DLL!');
    allOk:= False;
  end;
  if allOk and (@EnumProcessesF = nil) then begin
    //ErrorMsg('Couldn''t load EnumProcesses from PSAPI.DLL!');
    allOk:= False;
  end;
  if allOk and (@GetModuleBaseNameF = nil) then begin
    //ErrorMsg('Couldn''t load GetModuleBaseNameA from PSAPI.DLL!');
    allOk:= False;
  end;
end;

destructor TNTProcessList.Destroy;
begin
  FreeLibrary(PSApiHandle);
  ReallocMem (fPIDList, 0);
  inherited
end;

procedure TNTProcessList.Refresh;
var cbNeeded : Integer;
begin
  ReallocMem (fPIDList, 65536);
  if not EnumProcessesF (fPIDList, 65536, cbNeeded) then cbNeeded := 0;
  ReallocMem (fPIDList, cbNeeded);
  fPIDCount := cbNeeded div sizeof (Integer);
end;

function TNTProcessList.GetPID (index : Integer) : Integer;
begin
  if (index >= 0) and (index < Count) then
    result := PInteger (PChar (fPIDList) + index * sizeof (Integer))^
  else
    raise ERangeError.Create ('PID index out of range');
end;

procedure TNTProcessList.GetProcess (n : Integer; var process : TNTProcess);
begin
  process.PID := PID [n]
end;

procedure TNTProcess.SetPID (value : Integer);
begin
  if fPID <> value then
    fPID := value;
end;

function TNTProcess.GetBaseName : string;
  var
    handle : THandle;
    szName : array [0..MAX_PATH - 1] of char;
begin
  handle := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  if handle <> 0 then
  try
    if GetModuleBaseNameF (handle, 0, szName, sizeof (szName)) > 0 then
      result := szName
    else
      result := 'System'
  finally
    CloseHandle (handle)
  end
  else
    if PID = 0 then
      result := 'Idle'
end;

procedure ProcessTerminate(const TermProcessID: string);
  const PROCESS_TERMINATE = 1;
  var  ProcHandle: THandle;
       ProcessID: LongInt;

  procedure Win9xGetProcessID(const Process: string);
    var
      ts: array[0..MAX_PATH] of char;
      snap: THandle;
      pe32: TPROCESSENTRY32;

     procedure AddProcess(pe32:TPROCESSENTRY32);
       var s: string;
      begin
        StrCopy(ts,pe32.szExeFile);
        s:= UpperCase(ts);
        if Pos(Process, s) > 0 then
          ProcessID:= pe32.th32ProcessID;
      end;

    begin
     snap:= 0;
     try
       snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
       if snap <> 0 then begin
         pe32.dwSize := SizeOf(TPROCESSENTRY32);
         if Process32First(snap, pe32) then begin
           AddProcess(pe32);
           while Process32Next(snap, pe32) do
             AddProcess(pe32);
         end;
       end;
     finally
       CloseHandle(snap);
     end;
  end;

  procedure WinNTGetProcessID(const Process: string);
    const PROCESS_TERMINATE = 1;
    var  i, n: Integer;
         s: string;
         NTProcessList: TNTProcessList;
         NTProcess: TNTProcess;
  begin
    NTProcessList:= TNTProcessList.Create;
    if not NTProcessList.allOk then begin
      ProcessID:= 55555;
      Exit;
    end;
    NTProcess:= TNTProcess.Create;
    NTProcessList.Refresh;
    n:= NTProcessList.Count - 1;
    for i:= 0 to n do begin
      NTProcessList.GetProcess (i, NTProcess);
      s:= Uppercase(NTProcess.BaseName);
      if s = Process then ProcessID:= NTProcess.PID;
    end;
    NTProcessList.Destroy;
    NTProcess.Destroy;
  end;

begin
  if Win32Platform >= VER_PLATFORM_WIN32_NT
    then WinNTGetProcessID(TermProcessID)
    else Win9xGetProcessID(TermProcessID);
  ProcHandle:= OpenProcess(PROCESS_TERMINATE, FALSE, DWORD(ProcessID));
  try
    if (ProcHandle <> 0) and TerminateProcess(ProcHandle, $FFFFFFFF) then
      WaitForSingleObject(ProcHandle, INFINITE);
  finally
   CloseHandle(ProcHandle);
  end;
end;

procedure ProcessTerminateID(ProcessID: LongInt);
  const PROCESS_TERMINATE = 1;
  var  ProcHandle: THandle;
begin
  ProcHandle:= OpenProcess(PROCESS_TERMINATE, FALSE, DWORD(ProcessID));
  try
    if (ProcHandle <> 0) and TerminateProcess(ProcHandle, $FFFFFFFF) then
      WaitForSingleObject(ProcHandle, INFINITE);
  finally
   CloseHandle(ProcHandle);
  end;
end;

end.
