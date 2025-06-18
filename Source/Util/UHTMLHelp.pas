unit UHTMLHelp;

interface

function GetRootCHM(const Str: string): string;
function LoadFromCHM(const AFile: string): string;

implementation

uses
  Windows,
  SysUtils,
  ActiveX,
  ComObj,
  UUtils;

const
  CLSID_ITStorage: TGUID = (D1: $5D02926A; D2: $212E; D3: $11D0;
    D4: ($9D, $F9, $00, $A0, $C9, $22, $E6, $EC));
  IID_ITStorage: TGUID = (D1: $88CC31DE; D2: $27AB; D3: $11D0;
    D4: ($9D, $F9, $00, $A0, $C9, $22, $E6, $EC));

type
  SNB = PChar; // from objidl.h

  TCompactionLev = (COMPACT_DATA, COMPACT_DATA_AND_PATH);

  PItsControlData = ^TItsControlData;

  TITS_Control_Data = record
    cdwControlData: UINT;
    adwControlData: array [0 .. 0] of UINT;
  end;

  TItsControlData = TITS_Control_Data;

  IItsStorage = interface(IUnknown)
    function StgCreateDocFile(const pwcsName: PChar; GrfMode: DWORD;
      Reserved: DWORD; var PpstgOpen: IStorage): HRESULT; stdcall;

    function StgCreateDocFileOnILockBytes(Plkbyt: ILockBytes; GrfMode: DWORD;
      Reserved: DWORD; var PpstgOpen: IStorage): HRESULT; stdcall;

    function StgIsStorageFile(const pwcsName: PChar): HRESULT; stdcall;

    function StgIsStorageILockBytes(Plkbyt: ILockBytes): HRESULT; stdcall;

    function StgOpenStorage(const pwcsName: PChar; PstgPriority: IStorage;
      GrfMode: DWORD; snbExclude: SNB; Reserved: DWORD; var PpstgOpen: IStorage)
      : HRESULT; stdcall;

    function StgOpenStorageOnILockBytes(Plkbyt: ILockBytes;
      PstgPriority: IStorage; GrfMode: DWORD; snbExclude: SNB; Reserved: DWORD;
      var PpstgOpen: IStorage): HRESULT; stdcall;

    function StgSetTimes(const LpszName: PChar;
      const PcTime, PaTime, PmTime: TFileTime): HRESULT; stdcall;

    function SetControlData(PControlData: PItsControlData): HRESULT; stdcall;

    function DefaultControlData(var PpControlData: PItsControlData)
      : HRESULT; stdcall;

    function Compact(const pwcsName: PChar; ILev: TCompactionLev)
      : HRESULT; stdcall;
  end;

var
  GDumpBuffer: array of AnsiChar;

function GetRootCHM(const Str: string): string;
var
  ItsStorage: IItsStorage;
  Storage: IStorage;
  Enumerator: IEnumSTATSTG;
  StatStg: TStatStg;
  NumFetched: LongInt;
  HRes: HRESULT;
  Text: string;
  Filename: string;
begin
  Result := '';
  Filename := Str;
  try
    OleCheck(CoCreateInstance(CLSID_ITStorage, nil, CLSCTX_INPROC_SERVER,
      IID_ITStorage, ItsStorage));
    OleCheck(ItsStorage.StgOpenStorage(PChar(Filename), nil,
      STGM_READ or STGM_SHARE_DENY_WRITE, nil, 0, Storage));
    OleCheck(Storage.EnumElements(0, nil, 0, Enumerator));
    repeat
      HRes := Enumerator.Next(1, StatStg, @NumFetched);
      if (HRes = S_OK) and (StatStg.pwcsName <> '') then
      begin
        Text := StatStg.pwcsName;
        if (Pos('$', Text) = 0) and (StatStg.dwType = STGTY_STORAGE) then
          Result := Text;
        CoTaskMemFree(StatStg.pwcsName);
      end;
    until HRes <> S_OK;
    Result := '\' + Result;
  except
    on E: Exception do
      ErrorMsg('Exception: ' + E.Message + ' - Cannot read CHM root: ' + Str);
  end;
end;

function LoadFromCHM(const AFile: string): string;
const
  FILENAME_LENGTH = 400;
var
  ItsStorage: IItsStorage;
  Storage, SubStorage: IStorage;
  Stream: IStream;
  BytesRead: LongInt;
  TotalRead, Posi: Integer;
  CHMfile, Path, Str: string;
  WideS: string;
begin
  Result := '';
  try
    Posi := Pos('.CHM', UpperCase(AFile));
    CHMfile := Copy(AFile, 1, Posi + 3);
    Path := Copy(AFile, Posi + 5, Length(AFile));
    OleCheck(CoCreateInstance(CLSID_ITStorage, nil, CLSCTX_INPROC_SERVER,
      IID_ITStorage, ItsStorage));
    OleCheck(ItsStorage.StgOpenStorage(PChar(CHMfile), nil,
      STGM_READ or STGM_SHARE_DENY_WRITE, nil, 0, Storage));
    Posi := Pos('\', Path);
    while Posi > 0 do
    begin
      WideS := Copy(Path, 1, Posi - 1);
      Delete(Path, 1, Posi);
      OleCheck(Storage.OpenStorage(PWideChar(WideS), nil,
        STGM_READ or STGM_SHARE_DENY_WRITE, nil, 0, SubStorage));
      Storage := SubStorage;
      Posi := Pos('\', Path);
    end;
    WideS := Path;
    OleCheck(Storage.OpenStream(PWideChar(WideS), nil, STGM_READ or
      STGM_SHARE_EXCLUSIVE, 0, Stream));
    TotalRead := 0;
    SetLength(GDumpBuffer, 0);
    repeat
      SetLength(GDumpBuffer, TotalRead + 128);
      OleCheck(Stream.Read(@GDumpBuffer[TotalRead], 128, @BytesRead));
      TotalRead := TotalRead + BytesRead;
    until BytesRead <> 128;
    SetLength(GDumpBuffer, TotalRead);
    Str := string(PAnsiChar(GDumpBuffer));
    Result := Str;
  except
    on E: Exception do
      ErrorMsg('Exception: ' + E.Message + ' - Cannot load from CHM: ' + AFile);
  end;
end;

end.
