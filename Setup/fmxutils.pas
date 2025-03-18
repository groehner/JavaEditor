unit FmxUtils;

interface

uses SysUtils, Windows, Classes, Consts, Vcl.StdCtrls;

  function ExecuteFile(const FileName, Params, DefaultDir: string;
    ShowCmd: Integer): THandle;
  function IsAdmin: Boolean;
  function GetTempDir: String;
  function HideBlanks(s: string): string;
  function UnHideBlanks(s: String): String;
  function encodeQuotationMark(s: string): string;
  function decodeQuotationMark(s: string): string;
  function Split(Delimiter: Char; Input: string): TStringList;
  function withoutTrailingSlash(s: String): string;
  function GetSpecialFolderPath(CSIDLFolder: Integer): string;
  function isWin64: boolean;
  function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String; AllowDWord: Boolean = False): Boolean;
  function IsWebView2RuntimeNeeded(): boolean;

implementation

uses Dialogs, Forms, ShlObj, RTLConsts, ShellAPI, StrUtils, System.UITypes;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

function IsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;
var
  hAccessToken       : THandle;
  ptgGroups          : PTokenGroups;
  dwInfoBufferSize   : Cardinal;
  psidAdministrators : PSID;
  x                  : Integer;
begin
  Result := false;
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;
  if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY,
                         TRUE, hAccessToken) then begin
    if GetLastError <> ERROR_NO_TOKEN then
      Exit;
    if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
                            hAccessToken) then
      Exit;
  end;
  try
    GetTokenInformation(hAccessToken, TokenGroups, nil,
                        0, dwInfoBufferSize);
    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      Exit;
    GetMem(ptgGroups, dwInfoBufferSize);
    try
      if not GetTokenInformation(hAccessToken, TokenGroups, ptgGroups,
                                 dwInfoBufferSize, dwInfoBufferSize) then
        Exit;
      if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
             SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
             0, 0, 0, 0, 0, 0, psidAdministrators) then
        Exit;
      try
        for x := 0 to ptgGroups^.GroupCount - 1 do begin
          if EqualSid(psidAdministrators, ptgGroups^.Groups[x].Sid) then begin
            Result := true;
            Break;
          end;
        end;
      finally
        FreeSid(psidAdministrators);
      end;
    finally
      FreeMem(ptgGroups);
    end;
  finally
    CloseHandle(hAccessToken);
  end;
end; {Michael Winter}

function GetTempDir: String;
  var P: PChar;
begin
  P:= StrAlloc(255);
  GetTempPath(255, P);
  Result:= String(P);
end;

function HideBlanks(s: String): String;
begin
  if (Pos(' ', s) > 0) and (Pos('"', s) <> 1)
    then Result:= '"' + s + '"'
    else Result:= s;
end;

function UnHideBlanks(s: String): String;
begin
  if Pos('"', s) = 1
    then Result:= copy(s, 2, length(s) - 2)
    else Result:= s;
end;

function encodeQuotationMark(s: string): string;
begin
  Result:= ReplaceText(s, '"', '&quot;');
end;

function decodeQuotationMark(s: string): string;
begin
  Result:= ReplaceText(s, '&quot;', '"');
end;

function Split(Delimiter: Char; Input: string): TStringList;
  var p: integer; SL: TStringList;
begin
  SL:= TStringList.Create;
  p:= Pos(Delimiter, Input);
  while p > 0 do begin
    SL.Add(copy(Input, 1, p-1));
    delete(Input, 1, p);
    p:= Pos(Delimiter, Input);
  end;
  SL.Add(Input);
  Result:= SL;
end;

function withoutTrailingSlash(s: String): string;
begin
  if (copy(s, length(s), 1) = '/') or (copy(s, length(s), 1) = '\') then
    SetLength(s, Length(s)-1);
  Result:= s;
end;

function GetSpecialFolderPath(CSIDLFolder: Integer): string;
var
   FilePath: array [0..MAX_PATH] of char;
begin
  SHGetFolderPath(0, CSIDLFolder, 0, 0, FilePath);
  Result := FilePath;
end;

function isWin64: boolean;
var
  IsWow64ProcessFunc: function(hProcess: THandle; var Wow64Process: BOOL): BOOL; stdcall;
  Wow64Process: BOOL;
begin
  IsWow64ProcessFunc := GetProcAddress(GetModuleHandle(kernel32), 'IsWow64Process');
  Result := Assigned(IsWow64ProcessFunc) and
             IsWow64ProcessFunc(GetCurrentProcess, Wow64Process) and
             Wow64Process;
end;

function InternalRegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String;
  Type1, Type2, Type3: DWORD): Boolean;
var
  Typ, Size: DWORD;
  Len: Integer;
  S: String;
  ErrorCode: Longint;
label 1;
begin
  Result := False;
1:Size := 0;
  if (RegQueryValueEx(H, Name, nil, @Typ, nil, @Size) = ERROR_SUCCESS) and
     ((Typ = Type1) or (Typ = Type2) or ((Type3 <> REG_NONE) and (Typ = Type3))) then begin
    if Typ = REG_DWORD then begin
      var Data: DWORD;
      Size := SizeOf(Data);
      if (RegQueryValueEx(H, Name, nil, @Typ, @Data, @Size) = ERROR_SUCCESS) and
         (Typ = REG_DWORD) and (Size = Sizeof(Data)) then begin
        ResultStr := Data.ToString;
        Result := True;
      end;
    end else if Size = 0 then begin
      { It's an empty string with no null terminator.
        (Must handle those here since we can't pass a nil lpData pointer on
        the second RegQueryValueEx call.) }
      ResultStr := '';
      Result := True;
    end
    else begin
      { Paranoia: Impose reasonable upper limit on Size to avoid potential
        integer overflows below }
      if Cardinal(Size) >= Cardinal($70000000) then
        OutOfMemoryError;
      { Note: If Size isn't a multiple of SizeOf(S[1]), we have to round up
        here so that RegQueryValueEx doesn't overflow the buffer }
      Len := (Size + (SizeOf(S[1]) - 1)) div SizeOf(S[1]);
      SetString(S, nil, Len);
      ErrorCode := RegQueryValueEx(H, Name, nil, @Typ, @S[1], @Size);
      if ErrorCode = ERROR_MORE_DATA then begin
        { The data must've increased in size since the first RegQueryValueEx
          call. Start over. }
        goto 1;
      end;
      if (ErrorCode = ERROR_SUCCESS) and
         ((Typ = Type1) or (Typ = Type2) or (Typ = Type3)) then begin
        { If Size isn't a multiple of SizeOf(S[1]), we disregard the partial
          character, like RegGetValue }
        Len := Size div SizeOf(S[1]);
        { Remove any null terminators from the end and trim the string to the
          returned length.
          Note: We *should* find 1 null terminator, but it's possible for
          there to be more or none if the value was written that way. }
        while (Len <> 0) and (S[Len] = #0) do
          Dec(Len);
        { In a REG_MULTI_SZ value, each individual string is null-terminated,
          so add 1 null (back) to the end, unless there are no strings (Len=0) }
        if (Typ = REG_MULTI_SZ) and (Len <> 0) then
          Inc(Len);
        SetLength(S, Len);
        if (Typ = REG_MULTI_SZ) and (Len <> 0) then
          S[Len] := #0;
        ResultStr := S;
        Result := True;
      end;
    end;
  end;
end;

function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String; AllowDWord: Boolean): Boolean;
{ Queries the specified REG_SZ or REG_EXPAND_SZ registry key/value, and returns
  the value in ResultStr. Returns True if successful. When False is returned,
  ResultStr is unmodified. Optionally supports REG_DWORD. }
begin
  var Type3: DWORD;
  if AllowDWord then
    Type3 := REG_DWORD
  else
    Type3 := REG_NONE;
  Result := InternalRegQueryStringValue(H, Name, ResultStr, REG_SZ,
    REG_EXPAND_SZ, Type3);
end;

function IsWebView2RuntimeNeeded(): boolean;
{ See: https://learn.microsoft.com/en-us/microsoft-edge/webview2/concepts/distribution#detect-if-a-suitable-webview2-runtime-is-already-installed }
var
    Version: string;
    RuntimeNeeded: boolean;
    VerifyRuntime: boolean;
begin
  RuntimeNeeded := true;
  VerifyRuntime := false;
  { Since we are using an elevated installer I am not checking HKCU }
  if (IsWin64) then
  begin
    if (RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\WOW6432Node\Microsoft\EdgeUpdate\Clients\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}', Version, false)) or
      (RegQueryStringValue(HKEY_CURRENT_USER, 'Software\Microsoft\EdgeUpdate\Clients\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}', Version, false)) then
        VerifyRuntime := true;
  end
  else
  begin
    if (RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\EdgeUpdate\Clients\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}', Version, false)) or
      (RegQueryStringValue(HKEY_CURRENT_USER, 'Software\Microsoft\EdgeUpdate\Clients\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}', Version, false)) then
        VerifyRuntime := true;
  end;
  { Verify the version information }
  if VerifyRuntime and (Version <> '') and (Version <> '0.0.0.0') then
    RuntimeNeeded := false;
  Result := RuntimeNeeded;
end;






end.