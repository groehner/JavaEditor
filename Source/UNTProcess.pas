unit UNTProcess;

interface

uses Windows;

type

  TNTProcess = class
  private
    FPid: Integer;
    procedure SetPID(Value: Integer);
    function GetBaseName: string;
  public
    property PID: Integer read FPid write SetPID;
    property BaseName: string read GetBaseName;
  end;

  TNTProcessList = class
  private
    FAllOk: Boolean;
    FPIDList: PInteger;
    FPIDCount: Integer;
    FPSApiHandle: Integer;

    function GetPID(Index: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
    procedure GetProcess(Num: Integer; var Process: TNTProcess);
    property AllOk: Boolean read FAllOk;
    property Count: Integer read FPIDCount;
    property PID[Index: Integer]: Integer read GetPID;
  end;

procedure ProcessTerminate(const TermProcessID: string);
procedure ProcessTerminateID(ProcessID: LongInt);

implementation

uses
  SysUtils,
  TlHelp32;

type
  TEnumProc = function(PidList: PInteger; Cbi: Integer; var CBNeeded: Integer)
    : Boolean; stdcall;
  TGetBase = function(HProcess: THandle; Module: HINST; BaseName: PChar;
    Size: Integer): Integer; stdcall;

var
  EnumProcessesF: TEnumProc;
  GetModuleBaseNameF: TGetBase;

constructor TNTProcessList.Create;
begin
  inherited Create;
  FAllOk := True;
  FPSApiHandle := LoadLibrary('PSAPI.DLL');
  if FPSApiHandle <> 0 then
  begin
    @EnumProcessesF := GetProcAddress(FPSApiHandle, 'EnumProcesses');
    @GetModuleBaseNameF := GetProcAddress(FPSApiHandle, 'GetModuleBaseNameA');
  end
  else
    FAllOk := False;
  if FAllOk and not Assigned(@EnumProcessesF) then
    FAllOk := False;
  if AllOk and not Assigned(@GetModuleBaseNameF) then
    FAllOk := False;
end;

destructor TNTProcessList.Destroy;
begin
  FreeLibrary(FPSApiHandle);
  ReallocMem(FPIDList, 0);
  inherited;
end;

procedure TNTProcessList.Refresh;
var
  CBNeeded: Integer;
begin
  ReallocMem(FPIDList, 65536);
  if not EnumProcessesF(FPIDList, 65536, CBNeeded) then
    CBNeeded := 0;
  ReallocMem(FPIDList, CBNeeded);
  FPIDCount := CBNeeded div SizeOf(Integer);
end;

function TNTProcessList.GetPID(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < Count) then
    Result := PInteger(PChar(FPIDList) + Index * SizeOf(Integer))^
  else
    raise ERangeError.Create('PID Index out of range');
end;

procedure TNTProcessList.GetProcess(Num: Integer; var Process: TNTProcess);
begin
  Process.PID := PID[Num];
end;

procedure TNTProcess.SetPID(Value: Integer);
begin
  if FPid <> Value then
    FPid := Value;
end;

function TNTProcess.GetBaseName: string;
var
  Handle: THandle;
  SZName: array [0 .. MAX_PATH - 1] of Char;
begin
  Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
    False, PID);
  if Handle <> 0 then
    try
      if GetModuleBaseNameF(Handle, 0, SZName, SizeOf(SZName)) > 0 then
        Result := SZName
      else
        Result := 'System';
    finally
      CloseHandle(Handle);
    end
  else if PID = 0 then
    Result := 'Idle';
end;

procedure ProcessTerminate(const TermProcessID: string);
const
  PROCESS_TERMINATE = 1;

type
  CharArray = array[0 .. MAX_PATH] of Char;

var
  ProcHandle: THandle;
  ProcessID: LongInt;

  procedure AddProcess(PE32: TProcessEntry32; Process: string; Path: CharArray);
  begin
    StrCopy(Path, PE32.szExeFile);
    var Str := UpperCase(Path);
    if Pos(Process, Str) > 0 then
      ProcessID := PE32.th32ProcessID;
  end;

  procedure Win9xGetProcessID(const Process: string);
  var
    Path: CharArray;
    Snap: THandle;
    PE32: TProcessEntry32;
  begin
    Snap := 0;
    try
      Snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if Snap <> 0 then
      begin
        PE32.dwSize := SizeOf(TProcessEntry32);
        if Process32First(Snap, PE32) then
        begin
          AddProcess(PE32, Process, Path);
          while Process32Next(Snap, PE32) do
            AddProcess(PE32, Process, Path);
        end;
      end;
    finally
      CloseHandle(Snap);
    end;
  end;

  procedure WinNTGetProcessID(const Process: string);
  const
    PROCESS_TERMINATE = 1;
  var
    Str: string;
    NTProcessList: TNTProcessList;
    NTProcess: TNTProcess;
  begin
    NTProcessList := TNTProcessList.Create;
    if not NTProcessList.AllOk then
    begin
      ProcessID := 55555;
      Exit;
    end;
    NTProcess := TNTProcess.Create;
    NTProcessList.Refresh;
    for var I := 0 to NTProcessList.Count - 1 do
    begin
      NTProcessList.GetProcess(I, NTProcess);
      Str := UpperCase(NTProcess.BaseName);
      if Str = Process then
        ProcessID := NTProcess.PID;
    end;
    NTProcessList.Destroy;
    NTProcess.Destroy;
  end;

begin
  if Win32Platform >= VER_PLATFORM_WIN32_NT then
    WinNTGetProcessID(TermProcessID)
  else
    Win9xGetProcessID(TermProcessID);
  ProcHandle := OpenProcess(PROCESS_TERMINATE, False, DWORD(ProcessID));
  try
    if (ProcHandle <> 0) and TerminateProcess(ProcHandle, $FFFFFFFF) then
      WaitForSingleObject(ProcHandle, INFINITE);
  finally
    CloseHandle(ProcHandle);
  end;
end;

procedure ProcessTerminateID(ProcessID: LongInt);
const
  PROCESS_TERMINATE = 1;
var
  ProcHandle: THandle;
begin
  ProcHandle := OpenProcess(PROCESS_TERMINATE, False, DWORD(ProcessID));
  try
    if (ProcHandle <> 0) and TerminateProcess(ProcHandle, $FFFFFFFF) then
      WaitForSingleObject(ProcHandle, INFINITE);
  finally
    CloseHandle(ProcHandle);
  end;
end;

end.
