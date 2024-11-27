(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower ShellShock
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Sebastian Zierer (Unicode)
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: StFormat.pas 1.02                         *}
{*********************************************************}
{* ShellShock: Component Wrapper for shFormatDrive API   *}
{*********************************************************}

{$I SsDefine.inc}

{$I+} {I/O Checking On}
{$H+} {Huge strings}

unit StFormat;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  SysUtils, Forms, Classes, Controls, SsConst,
  SsBase;

{$Z+}
type
  TStFormatOptions = (fmtSystemOnly, fmtFull);

  TStFormatOptionsSet = set of TStFormatOptions;

  TStDisketteErrorEvent = procedure (Sender : TObject;
                                     var Retry : Boolean) of object;

  TStCustomFormatDrive = class(TSsShellComponent)

  protected {private}
    FDrive       : string;
    FOptions     : TStFormatOptionsSet;

    FOnDisketteError : TStDisketteErrorEvent;
    FOnFormatError   : TNotifyEvent;

    function CheckDiskette : Integer;
  protected

    procedure SetDrive(const Value : string);
{$Z-}

    {properties}
    property Drive : string
      read FDrive
      write SetDrive;

    property Options : TStFormatOptionsSet
      read FOptions
      write FOptions
      default [fmtFull];

    {events}
    property OnDisketteError : TStDisketteErrorEvent
      read FOnDisketteError
      write FOnDisketteError;

    property OnFormatError : TNotifyEvent
      read FOnFormatError
      write FOnFormatError;

{$Z+}
  public
    { Public declarations }
    constructor Create(AOwner : TComponent);
      override;
{$Z-}
    function Execute : Boolean;
  published
    { Published declarations }
  end;

  TStFormatDrive = class(TStCustomFormatDrive)
  public
    {properties}
    property Error;
    property ErrorString;

  published
    {properties}
    property Drive;
    property Options;

    {events}
    property OnDisketteError;
    property OnFormatError;
  end;

implementation

const
  StNoError = 0;
  StAbort   = 1;
  StRetry   = 2;

  { These constants are not defined in ShellApi.PAS. }
  SHFMT_OPT_FULL     = $0001;
  SHFMT_OPT_SYSONLY  = $0002;
  SHFMT_ID_DEFAULT   = $FFFF;

  SHFMT_NOFORMAT     = $FFFFFFFD; { The drive could not be formatted. }
  SHFMT_CANCEL       = $FFFFFFFE; { The format was cancelled.         }
  SHFMT_ERROR        = $FFFFFFFF; { Other format error.               }

function SHFormatDrive(hWnd : HWND; Drive, FormatID, Options : Integer)
                       : Integer; stdcall; external 'shell32.dll' {$IFDEF VERSION2010} delayed; platform {$ENDIF};
// Note  This function is available through Windows XP Service Pack 2 (SP2) and Windows Server 2003.
//    It might be altered or unavailable in subsequent versions of Windows.


function TStCustomFormatDrive.CheckDiskette : Integer;
var
  OldErrMode : Integer;
  OldDir     : array [0..MAX_PATH - 1] of Char;
  TempFile   : array [0..MAX_PATH - 1] of Char;
  Retry      : Boolean;
  Res        : DWORD;
begin
  Result := StNoError;
  if Assigned(FOnDisketteError) then begin
    { Catch diskette errors here. }
    OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    GetCurrentDirectory(Length(OldDir), OldDir);
    SetLastError(0);
    { First try to switch to FDrive. }
    SetCurrentDirectory(PChar(FDrive));
    { See if there was an error. }
    CheckSystemError(0);
    { No? Then try to create a file on the diskette. }
    if Error = 0 then begin
      GetTempFileName(PChar(FDrive), '~ST', 0, TempFile);
      Res := CreateFile(TempFile, GENERIC_WRITE, 0,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      if Res = INVALID_HANDLE_VALUE then
        CheckSystemError(0)
      else begin
        CloseHandle(Res);
        DeleteFile(TempFile);
      end;
    end;
    { Reset the current directory. }
    SetCurrentDirectory(OldDir);
    { Reset the error mode. }
    SetErrorMode(OldErrMode);
    { Fire the event. }
    Retry := False;
    if FError <> 0 then begin
      FOnDisketteError(Self, Retry);
      if Retry then begin
        Result := StRetry;
        if FError = 0 then
          Result := StNoError;
      end else
        Result := StAbort;
    end;
  end;
end;

constructor TStCustomFormatDrive.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FOptions := [fmtFull];
end;

function TStCustomFormatDrive.Execute : Boolean;
var
  Flags        : Word;
  ParentHandle : HWND;
  Err          : Integer;
  Res          : Integer;
  DriveNo      : Integer;
  OSVInfo      : TOsVersionInfo;
begin

  Result := False;

  { Check to see if the drive is valid and if it is a removeable drive. }
  Res := GetDriveType(PChar(FDrive));
  if Res <> DRIVE_REMOVABLE then
    raise ESsShellFormatError.CreateResTP(ssscShellFormatBadDrive, 0);

  { Check to see if the drive is ready. }
  Res := CheckDiskette;
  case Res of
    { Problem but user wants to retry so recurse. }
    StRetry :
      begin
        Result := Execute;
        Exit;
      end;
    { Problem and user wants to bail out. }
    StAbort : Exit;
  end;

  { Get a handle to the owning window. }
  if Owner is TWinControl then
    ParentHandle := (Owner as TWinControl).Handle
  else if Owner is TApplication then
    ParentHandle := (Owner as TApplication).Handle
  else
    ParentHandle := 0;

  Flags := 0;

  { If the OS is NT then don't add the SHFMT_OPT_SYSONLY flag. }
  OsVInfo.dwOSVersionInfoSize := SizeOf(TOsVersionInfo);
  GetVersionEx(OSVInfo);
  if OsVInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT then begin
    if fmtSystemOnly in FOptions then
      Flags := Flags or SHFMT_OPT_SYSONLY;
    if fmtFull in FOptions then
      Flags := Flags or SHFMT_OPT_FULL;
  end else
    { For some odd reason this is backwards in NT. }
    if not (fmtFull in FOptions) then
      Flags := Flags or SHFMT_OPT_FULL;
  { Convert drive letter to a drive number. }
  DriveNo := Ord(AnsiUpperCase(FDrive)[1]) - Ord('A');
  { Execute the Format dialog. }
  Err := SHFormatDrive(ParentHandle, DriveNo, SHFMT_ID_DEFAULT, Flags);
  if (Err < 0) then begin
    FError := Err;
    case FError of
      Integer(SHFMT_ERROR)    : Err := ssscShellFormatError;
      Integer(SHFMT_CANCEL)   : Err := ssscShellFormatCancel;
      Integer(SHFMT_NOFORMAT) : Err := ssscShellFormatNoFormat;
    end;
    FErrorString := ShellShockStr(Err);
    if Assigned(FOnFormatError) then
      FOnFormatError(Self);
  end else begin
    FError := 0;
    FErrorString := '';
    Result := True;
  end;
end;

procedure TStCustomFormatDrive.SetDrive(const Value : string);
var
  P : Integer;
begin
  if Value <> '' then begin
    FDrive := AnsiUpperCase(Value);
    { Fix-up drive letter if necessary. Won't catch every }
    { user error but will catch the most common ones.    }
    P := Pos('\', FDrive);
    if P <> 0 then
      Delete(FDrive, P, Length(FDrive) - P + 1);
    if (Length(FDrive) = 1) and (Pos(':', FDrive) = 0) then
      FDrive := FDrive + ':';
  end else
    FDrive := '';
end;


end.
