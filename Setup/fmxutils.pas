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


end.