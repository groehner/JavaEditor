(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the 'License'); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
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
 * Contributor(s): Sebastian Zierer (Unicode)
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: SsBase.pas 1.02                           *}
{*********************************************************}
{* ShellShock: Base unit for ShellShock                  *}
{*********************************************************}

{$I SsDefine.inc}

unit SsBase;

interface

uses
  Windows, Classes, SysUtils,
  {$IFDEF VERSION6} {$WARN UNIT_PLATFORM OFF} {$ENDIF}
  {$IFNDEF VERSION2010} FileCtrl, {$ENDIF}
  {$IFDEF VERSION6} {$WARN UNIT_PLATFORM ON} {$ENDIF}
  ShlObj;

type
{$IFDEF CBuilder}
  TStHwnd = Integer;
{$ELSE}
  TStHwnd = HWND;
{$ENDIF}

  TStNotifyRegister = record
    Pidl         : PItemIDList;
    WatchSubTree : LONGBOOL;
  end;

const
  StMaxBlockSize = MaxLongInt;
  { These the following constants are not defined in ShellObj.PAS. }
  { Their use is not supported on all versions of Windows.         }
  CSIDL_Internet          = $0001;
  CSIDL_AltStartup        = $001d;
  CSIDL_Common_AltStartup = $001e;
  CSIDL_Common_Favorites  = $001f;
  CSIDL_Internet_Cache    = $0020;
  CSIDL_Cookies           = $0021;
  CSIDL_History           = $0022;
  CSIDL_Connections       = $0031;
  Bif_EditBox             = $0010;
  Bif_Validate            = $0020;
  { The following constants are not defined in Delphi 2's OLE2.PAS. }
  { Their use is not supported on all versions of Windows.          }
  {$IFNDEF VERSION3}
  Fof_NoErrorUI                 = $0400;
  CSIDL_Common_StartMenu        = $0016;
  CSIDL_Common_Programs         = $0017;
  CSIDL_Common_Startup          = $0018;
  CSIDL_Common_DesktopDirectory = $0019;
  CSIDL_AppData                 = $001a;
  CSIDL_PrintHood               = $001b;
  Bif_BrowseIncludeFiles        = $4000;
  IId_IPersistFile : TGUID = (
    D1:$0000010B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  IId_IShellLink : TGUID = (
    D1:$000214EE;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  {$ENDIF}

type
  TStSpecialRootFolder = (
    sfAltStartup, sfAppData, sfBitBucket, sfCommonAltStartup,
    sfCommonDesktopDir, sfCommonFavorites, sfCommonPrograms,
    sfCommonStartMenu, sfCommonStartup, sfConnections,
    sfControls, sfCookies,
    sfDesktop, sfDesktopDir, sfDrives, sfFavorites, sfFonts,
    sfHistory, sfInternet, sfInternetCache, sfNetHood,
    sfNetwork, sfNone, sfPersonal, sfPrinters, sfPrintHood, sfPrograms,
    sfRecentFiles, sfSendTo, sfStartMenu, sfStartup, sfTemplates);

const
  ShellFolders : array [TStSpecialRootFolder] of Integer =
    (CSIDL_ALTSTARTUP, CSIDL_APPDATA, CSIDL_BITBUCKET, CSIDL_COMMON_ALTSTARTUP,
     CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_COMMON_FAVORITES, CSIDL_COMMON_PROGRAMS,
     CSIDL_COMMON_STARTMENU, CSIDL_COMMON_STARTUP, CSIDL_Connections,
     CSIDL_CONTROLS, CSIDL_COOKIES,
     CSIDL_DESKTOP, CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_FAVORITES,
     CSIDL_FONTS, CSIDL_HISTORY, CSIDL_INTERNET, CSIDL_INTERNET_CACHE,
     CSIDL_NETHOOD, CSIDL_NETWORK, 0, CSIDL_PERSONAL, CSIDL_PRINTERS,
     CSIDL_PRINTHOOD, CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_SENDTO,
     CSIDL_STARTMENU, CSIDL_STARTUP, CSIDL_TEMPLATES);

type
  TIncludeItemFunc = function (
    const SR : TSearchRec; ForInclusion : Boolean) : Boolean;

  {base component for ShellShock non-visual components}
  TSsComponent = class(TComponent)
  protected {private}
    function GetVersion : string;
    procedure SetVersion(const Value : string);

  published
    property Version : string
      read GetVersion
      write SetVersion
      stored False;
  end;

  {base component for ShellShock shell components}
  TSsShellComponent = class(TSsComponent)
  protected {private}
    FShellVersion : Double;
    FError        : Integer;
    FErrorString  : string;

    FOnError      : TNotifyEvent;

    procedure CheckSystemError(ErrCode : Integer);

    {properties}
    property ShellVersion : Double
      read FShellVersion;

    property Error : Integer
      read FError
      write FError;

    property ErrorString : string
      read FErrorString;

    {events}
    property OnError : TNotifyEvent
      read FOnError
      write FOnError;

    {protected methods}
    procedure DoError;

  public
    constructor Create(AOwner : TComponent);
      override;
  end;

{$Z-}
{$IFNDEF VERSION2009}
  { Undocumented PIDL functions that we import. }
  TStILClone = function(Pidl : PItemIDList) : PItemIDList; stdcall;
  TStILCloneFirst = function(Pidl : PItemIDList) : PItemIDList; stdcall;
  TStILCombine = function (Pidl1, Pidl2 : PItemIDList) : PItemIDList; stdcall;
  TStILGetNext = function(Pidl : PItemIDList) : PItemIDList; stdcall;
  TStILFindLastID = function(Pidl : PItemIDList) : PItemIDList; stdcall;
  TStILIsEqual = function(Pidl1, Pidl2 : PItemIDList) : LongBool; stdcall;
  TStILRemoveLastID = function(Pidl : PItemIDList) : LongBool; stdcall;
  TStILGetSize = function(Pidl : PItemIDList) : Word; stdcall;
  TStILFree = procedure(Pidl : PItemIDList); stdcall;

  TStSHChangeNotifyRegister = function(HWnd : THandle; Flags : DWORD;
    EventMask : DWORD; MessageID : UINT; ItemCount : DWORD;
    var Items : TStNotifyRegister) : THandle; stdcall;
  TStSHChangeNotifyDeregister = function(
    HNotificationObject : THandle) : Boolean;  stdcall;
{$ENDIF}

  {-ShellShock exception class tree}
  ESsException = class(Exception)     {ancestor to all ShellShock exceptions}
    protected {private}
      FErrorCode : Longint;

    public
      constructor CreateResTP(Ident : LongInt; Dummy : Word);
      constructor CreateResFmtTP(Ident : Longint; const Args : array of const;
                                 Dummy : Word);
      property ErrorCode : LongInt
        read FErrorCode
        write FErrorCode;
  end;
  ESsExceptionClass = class of ESsException;

  ESsContainerError = class(ESsException);   {container exceptions}
  ESsStringError = class(ESsException);      {String class exceptions}
  ESsVersionInfoError = class(ESsException); {Version info exception}
  ESsShellError = class(ESsException);       {Shell version exception}
  ESsFileOpError = class(ESsException);      {Shell file operation exception}
  ESsTrayIconError = class(ESsException);    {Tray Icon exception}
  ESsDropFilesError = class(ESsException);   {Drop files exception}
  ESsShortcutError = class(ESsException);    {Shortcut exception}
  ESsShellFormatError = class(ESsException); {Format exception}
  ESsInvalidFolder = class(ESsException);    {Bad folder exception}
  ESsInvalidSortDir = class(ESsException);   {Bad sort direction exception}
  ESsBufStreamError =class(ESsException);    {Buffered stream errors}
  ESsRegExError = class(ESsException);       {RegEx errors}

  TStNode = class(TPersistent)
  protected {private}
    FData : Pointer;
  public
    constructor Create(AData : Pointer);
      virtual;
    property Data : Pointer
       read FData
       write FData;
  end;

  TStNodeClass = class of TStNode;

  TStContainer = class;

  TCompareFunc =
    function(Data1, Data2 : Pointer) : Integer;
  TStCompareEvent =
    procedure(Sender : TObject; Data1, Data2 : Pointer;  var Compare : Integer)
    of object;

  TDisposeDataProc =
    procedure(Data : Pointer);
  TStDisposeDataEvent =
    procedure(Sender : TObject; Data : Pointer)
    of object;

  TLoadDataFunc =
    function(Reader : TReader) : Pointer;
  TStLoadDataEvent =
    procedure(Sender : TObject; Reader : TReader; var Data : Pointer)
    of object;

  TStoreDataProc =
    procedure(Writer : TWriter; Data : Pointer);
  TStStoreDataEvent =
    procedure(Sender : TObject; Writer : TWriter; Data : Pointer)
    of object;

  TStringCompareFunc =
    function(const String1, String2 : string) : Integer;
  TStStringCompareEvent =
    procedure(Sender : TObject; const String1, String2 : string; var Compare : Integer)
    of object;

  TUntypedCompareFunc =
    function(const El1, El2) : Integer;
  TStUntypedCompareEvent =
    procedure(Sender : TObject; const El1, El2; var Compare : Integer)
    of object;

  TIterateFunc =
    function(Container : TStContainer; Node : TStNode; OtherData : Pointer) : Boolean;
  TIteratePointerFunc =
    function(Container : TStContainer; Data, OtherData : Pointer) : Boolean;
  TIterateUntypedFunc =
    function(Container : TStContainer; var Data; OtherData : Pointer) : Boolean;

  TStContainer = class(TPersistent)
  protected {private}
    {property instance variables}
    FCompare     : TCompareFunc;
    FDisposeData : TDisposeDataProc;
    FLoadData    : TLoadDataFunc;
    FStoreData   : TStoreDataProc;

    {event variables}
    FOnCompare     : TStCompareEvent;
    FOnDisposeData : TStDisposeDataEvent;
    FOnLoadData    : TStLoadDataEvent;
    FOnStoreData   : TStStoreDataEvent;

    {private instance variables}
    {$IFDEF ThreadSafe}
    conThreadSafe  : TRTLCriticalSection;
    {$ENDIF}

    procedure SetCompare(C : TCompareFunc);
    procedure SetDisposeData(D : TDisposeDataProc);
    procedure SetLoadData(L : TLoadDataFunc);
    procedure SetStoreData(Str : TStoreDataProc);

  protected
    conNodeClass : TStNodeClass;
    conNodeProt  : Integer;
    FCount       : Longint;

    {protected undocumented methods}
    function AssignPointers(Source : TPersistent; AssignData : TIteratePointerFunc) : Boolean;
    function AssignUntypedVars(Source : TPersistent; AssignData : TIterateUntypedFunc) : Boolean;
    procedure ForEachPointer(Action : TIteratePointerFunc; OtherData : Pointer);
      virtual;
    procedure ForEachUntypedVar(Action : TIterateUntypedFunc; OtherData : pointer);
      virtual;
    procedure GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
      virtual;
    procedure SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
      virtual;
    function StoresPointers : Boolean;
      virtual;
    function StoresUntypedVars : Boolean;
      virtual;

    {protected documented}
    procedure IncNodeProtection;
      {-Prevent container Destroy from destroying its nodes}
    procedure DecNodeProtection;
      {-Allow container Destroy to destroy its nodes}
    procedure EnterCS;
      {-Enter critical section for this instance}
    procedure LeaveCS;
      {-Leave critical section for this instance}
  public
    constructor CreateContainer(NodeClass : TStNodeClass; Dummy : Integer);
      {-Create an abstract container (called by descendants)}
    destructor Destroy;
      override;
      {-Destroy a collection, and perhaps its nodes}
    procedure Clear;
      virtual; abstract;
      {-Remove all elements from collection}
    procedure DisposeNodeData(Posi : TStNode);
      {-Destroy the data associated with a node}

    {wrapper methods for using events or proc/func pointers}
    function DoCompare(Data1, Data2 : Pointer) : Integer;
      virtual;
    procedure DoDisposeData(Data : Pointer);
      virtual;
    function DoLoadData(Reader : TReader) : Pointer;
      virtual;
    procedure DoStoreData(Writer : TWriter; Data : Pointer);
      virtual;

    procedure LoadFromFile(const FileName : string);
      dynamic;
      {-Create a container and its data from a file}
    procedure LoadFromStream(Str : TStream);
      dynamic; abstract;
      {-Create a container and its data from a stream}
    procedure StoreToFile(const FileName : string);
      dynamic;
      {-Create a container and its data from a file}
    procedure StoreToStream(Str : TStream);
      dynamic; abstract;
      {-Write a container and its data to a stream}

    property Count : LongInt
      {-Return the number of elements in the collection}
      read FCount;

    property Compare : TCompareFunc
      {-Set or read the node comparison function}
      read FCompare
      write SetCompare;

    property DisposeData : TDisposeDataProc
      {-Set or read the node data dispose function}
      read FDisposeData
      write SetDisposeData;

    property LoadData : TLoadDataFunc
      {-Set or read the node data load function}
      read FLoadData
      write SetLoadData;

    property StoreData : TStoreDataProc
      {-Set or read the node data load function}
      read FStoreData
      write SetStoreData;

    {events}
    property OnCompare : TStCompareEvent
      read FOnCompare
      write FOnCompare;

    property OnDisposeData : TStDisposeDataEvent
      read FOnDisposeData
      write FOnDisposeData;

    property OnLoadData : TStLoadDataEvent
      read FOnLoadData
      write FOnLoadData;

    property OnStoreData : TStStoreDataEvent
      read FOnStoreData
      write FOnStoreData;
  end;

  PVerTranslation = ^TVerTranslation;
  TVerTranslation = record
    Language : Word;
    CharSet  : Word;
  end;

  TSsCustomVersionInfo = class
  protected {private}
{$Z+}
    FFileMajorVersion    : LongInt;
    FFileMinorVersion    : LongInt;
    FFileName            : string;
    FFileOS              : LongInt;
    FFileVersion         : string;
    FFileVersionFloat    : Double;
    FProductMajorVersion : LongInt;
    FProductMinorVersion : LongInt;
    FProductVersion      : string;
    FProductVersionFloat : Double;
    VInfoLoaded          : Boolean;

    function GetFileMajorVersion: LongInt;
    function GetFileMinorVersion: LongInt;
    function GetFileOS: LongInt;
    function GetFileVersion : string;
    function GetFileVersionFloat : Double;
    function GetProductMajorVersion: LongInt;
    function GetProductMinorVersion: LongInt;
    function GetProductVersion : string;
    function GetProductVersionFloat : Double;
    procedure SetFileName(const Value : string);

    function LoadVersionInfo(const Key : string) : string;

  protected

{$Z-}
    {properties}
    property FileMajorVersion : LongInt
      read GetFileMajorVersion;

    property FileMinorVersion : LongInt
      read GetFileMinorVersion;

    property FileName : string
      read FFileName write SetFileName;

    property FileOS : LongInt
      read GetFileOS;

    property FileVersion : string
      read GetFileVersion;

    property FileVersionFloat : Double
      read GetFileVersionFloat;

    property ProductMajorVersion : LongInt
      read GetProductMajorVersion;

    property ProductMinorVersion : LongInt
      read GetProductMinorVersion;

    property ProductVersion : string
      read GetProductVersion;

    property ProductVersionFloat : Double
      read GetProductVersionFloat;

  public
{$Z+}
    constructor Create(AOwner : TComponent);
    destructor Destroy;
      override;
{$Z-}
  end;

  {$M+}
  TSsVersionInfo = class(TSsCustomVersionInfo)
  public
    {properties}
    property FileMajorVersion;
    property FileMinorVersion;
    property FileOS;
    property FileVersion;
    property FileVersionFloat;
    property ProductMajorVersion;
    property ProductMinorVersion;
    property ProductVersion;
    property ProductVersionFloat;

  published
    {properties}
    property FileName;
  end;
  {$M-}

  TAssignRowData = record
    RowNum : Integer;
    Data   : array [0..0] of Byte;
  end;

{---Generic node routines---}
function DestroyNode(Container : TStContainer; Node : TStNode;
                     OtherData : Pointer) : Boolean;
  {-Generic function to pass to iterator to destroy a container node}

{---Miscellaneous---}

function IsOrInheritsFrom(Root, Candidate : TClass) : Boolean;
  {-Return true if the classes are equal or Candidate is a descendant of Root}

procedure RaiseContainerError(Code : longint);
  {-Internal routine: raise an exception for a container}

procedure RaiseContainerErrorFmt(Code : Longint; Data : array of const);
  {-Internal routine: raise an exception for a container}


{general routine to raise a specific class of ShellShock exception}
procedure RaiseStError(ExceptionClass : ESsExceptionClass; Code : LongInt);

{general routines to raise a specific Win32 exception in ShellShock}
procedure RaiseStWin32Error(ExceptionClass : ESsExceptionClass; Code : LongInt);
procedure RaiseStWin32ErrorEx(ExceptionClass : ESsExceptionClass; Code : LongInt; Info : string);

var
  PidlFormat    : Word;
{$IFNDEF VERSION2009}
  Shell32Inst   : THandle;
  ILClone       : TStILClone;
  ILCloneFirst  : TStILCloneFirst;
  ILCombine     : TStILCombine;
  ILGetNext     : TStILGetNext;
  ILFindLastID  : TStILFindLastID;
  ILIsEqual     : TStILIsEqual;
  ILRemoveLastID: TStILRemoveLastID;
  ILGetSize     : TStILGetSize;
  ILFree        : TStILFree;
  SHChangeNotifyRegister      : TStSHChangeNotifyRegister;
  SHChangeNotifyDeregister    : TStSHChangeNotifyDeregister;
{$ENDIF}

  function GetSpecialFolderPath(Handle : TStHwnd; Folder : TStSpecialRootFolder) : string;
  procedure GetSpecialFolderFiles(Handle : TStHwnd;
                                  Folder : TStSpecialRootFolder;
                                  Files : TStrings);
  function GetParentPidl(Pidl: PItemIDList) : PItemIDList;
{$IFNDEF VERSION2009}
  procedure LoadILFunctions;
{$ENDIF}

{String routines}
function LeftPadChS(const Str : string; C : Char; Len : Integer) : string;
function LeftPadS(const Str : string; Len : Integer) : string;

function CharExistsS(const Str : string; C : Char) : Boolean;
function StrChPosS(const Posi : string; C : Char; var Pos : Cardinal) : Boolean;

function Long2StrL(L : LongInt) : string;

function TrimL(const Str : string) : string;

function AddBackSlashL(const DirName : string) : string;

function CommaizeL(L : LongInt) : string;

function CommaizeChL(L : Longint; Ch : Char) : string;

procedure EnumerateFiles(StartDir : string;
  FL : TStrings; SubDirs : Boolean; IncludeItem : TIncludeItemFunc);

function IsDirectory(const DirName : string) : Boolean;

implementation

uses SsConst;


procedure RaiseStError(ExceptionClass : ESsExceptionClass; Code : LongInt);
var
  E : ESsException;
begin
  E := ExceptionClass.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32Error(ExceptionClass : ESsExceptionClass; Code : LongInt);
var
  E : ESsException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code));
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32ErrorEx(ExceptionClass : ESsExceptionClass; Code : LongInt;
          Info : string);
var
  E : ESsException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code) + ' [' + Info + ']');
  E.ErrorCode := Code;
  raise E;
end;

constructor ESsException.CreateResTP(Ident : LongInt; Dummy : Word);
begin
  inherited Create(ShellShockStr(Ident));
end;

constructor ESsException.CreateResFmtTP(Ident : Longint;
            const Args : array of const; Dummy : Word);
begin
  inherited CreateFmt(ShellShockStr(Ident), Args);
end;

function AbstractCompare(Data1, Data2 : Pointer) : Integer; far;
begin
  raise ESsContainerError.CreateResTP(ssscNoCompare, 0);
end;

function DestroyNode(Container : TStContainer;
                     Node : TStNode;
                     OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

function IsOrInheritsFrom(Root, Candidate : TClass) : Boolean;
  begin
    Result := (Root = Candidate) or Candidate.InheritsFrom(Root);
  end;

procedure RaiseContainerError(Code : LongInt);
var
  E : ESsContainerError;
begin
  E := ESsContainerError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseContainerErrorFmt(Code : Longint; Data : array of const);
var
  E : ESsContainerError;
begin
  E := ESsContainerError.CreateResFmtTP(Code, Data, 0);
  E.ErrorCode := Code;
  raise E;
end;

{$IFNDEF HStrings}
function StNewStr(Str : string) : PShortString;
begin
  GetMem(Result, succ(Length(Str)));
  Result^ := Str;
end;

procedure StDisposeStr(PS : PShortString);
begin
  if (PS <> nil) then
    FreeMem(PS, succ(Length(PS^)));
end;
{$ENDIF}

{----------------------------------------------------------------------}

constructor TStNode.Create(AData : Pointer);
begin
  Data := AData;
end;

{----------------------------------------------------------------------}

function TStContainer.AssignPointers(Source : TPersistent;
                                     AssignData : TIteratePointerFunc) : Boolean;
begin
  Result := False;
  if (Source is TStContainer) then
    if TStContainer(Source).StoresPointers then
      begin
        Clear;
        TStContainer(Source).ForEachPointer(AssignData, Self);
        Result := True;
      end;
end;

function TStContainer.AssignUntypedVars(Source : TPersistent;
                                        AssignData : TIterateUntypedFunc) : Boolean;
var
  RowCount : Cardinal;
  ColCount : Cardinal;
  ElSize : Cardinal;
begin
  Result := False;
  if (Source is TStContainer) then
    if TStContainer(Source).StoresUntypedVars then
      begin
        Clear;
        TStContainer(Source).GetArraySizes(RowCount, ColCount, ElSize);
        SetArraySizes(RowCount, ColCount, ElSize);
        TStContainer(Source).ForEachUntypedVar(AssignData, Self);
        Result := True;
      end;
end;

procedure TStContainer.ForEachPointer(Action : TIteratePointerFunc;
                                      OtherData : pointer);
begin
  {do nothing}
end;

procedure TStContainer.ForEachUntypedVar(Action : TIterateUntypedFunc;
                                            OtherData : pointer);
begin
  {do nothing}
end;

procedure TStContainer.GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
begin
  RowCount := 0;
  ColCount := 0;
  ElSize := 0;
end;

procedure TStContainer.SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
begin
  {do nothing}
end;

procedure TStContainer.SetCompare(C : TCompareFunc);
begin
  FCompare := C;
end;

procedure TStContainer.SetDisposeData(D : TDisposeDataProc);
begin
  FDisposeData := D;
end;

procedure TStContainer.SetLoadData(L : TLoadDataFunc);
begin
  FLoadData := L;
end;

procedure TStContainer.SetStoreData(Str : TStoreDataProc);
begin
  FStoreData := Str;
end;

function TStContainer.StoresPointers : Boolean;
begin
  Result := False;
end;

function TStContainer.StoresUntypedVars : Boolean;
begin
  Result := False;
end;

constructor TStContainer.CreateContainer(NodeClass : TStNodeClass; Dummy : Integer);
begin
{$IFDEF ThreadSafe}
  Windows.InitializeCriticalSection(conThreadSafe);
{$ENDIF}

  FCompare := AbstractCompare;
  conNodeClass := NodeClass;

  inherited Create;
end;

procedure TStContainer.DecNodeProtection;
begin
  Dec(conNodeProt);
end;

destructor TStContainer.Destroy;
begin
  if conNodeProt = 0 then
    Clear;
{$IFDEF ThreadSafe}
  Windows.DeleteCriticalSection(conThreadSafe);
{$ENDIF}
  inherited Destroy;
end;

procedure TStContainer.DisposeNodeData(Posi : TStNode);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Assigned(Posi) then
      DoDisposeData(Posi.Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStContainer.DoCompare(Data1, Data2 : Pointer) : Integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
    FOnCompare(Self, Data1, Data2, Result)
  else if Assigned(FCompare) then
    Result := FCompare(Data1, Data2);
end;

procedure TStContainer.DoDisposeData(Data : Pointer);
begin
  if Assigned(FOnDisposeData) then
    FOnDisposeData(Self, Data)
  else if Assigned(FDisposeData) then
    FDisposeData(Data);
end;

function TStContainer.DoLoadData(Reader : TReader) : Pointer;
begin
  Result := nil;
  if Assigned(FOnLoadData) then
    FOnLoadData(Self, Reader, Result)
  else if Assigned(FLoadData) then
    Result := FLoadData(Reader)
  else
    RaiseContainerError(ssscNoLoadData);
end;

procedure TStContainer.DoStoreData(Writer : TWriter; Data : Pointer);
begin
  if Assigned(FOnStoreData) then
    FOnStoreData(Self, Writer, Data)
  else if Assigned(FStoreData) then
    FStoreData(Writer, Data)
  else
    RaiseContainerError(ssscNoStoreData);
end;

procedure TStContainer.EnterCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(conThreadSafe);
{$ENDIF}
end;

procedure TStContainer.IncNodeProtection;
begin
  Inc(conNodeProt);
end;

procedure TStContainer.LeaveCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(conThreadSafe);
{$ENDIF}
end;

procedure TStContainer.LoadFromFile(const FileName : string);
var
  Str : TStream;
begin
  Str := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(Str);
  finally
    Str.Free;
  end;
end;

procedure TStContainer.StoreToFile(const FileName : string);
var
  Str : TStream;
begin
  Str := TFileStream.Create(FileName, fmCreate);
  try
    StoreToStream(Str);
  finally
    Str.Free;
  end;
end;


{*** TSsComponent ***}

function TSsComponent.GetVersion : string;
begin
  Result := SsVersionStr;
end;

procedure TSsComponent.SetVersion(const Value : string);
begin
end;

{ String routines }
function LeftPadChS(const Str : string; C : Char; Len : Integer) : string;
  {-Pad a string on the left with a specified character.}
begin
  if Length(Str) >= Len then
    Result := Str
  else
    Result := StringOfChar(C, Len - Length(Str)) + Str;
end;

function LeftPadS(const Str : string; Len : Integer) : string;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChS(Str, ' ', Len);
end;

function CharExistsS(const Str : string; C : Char) : Boolean;
begin
  Result := Pos(C, Str) > 0;
end;

function StrChPosS(const Posi : string; C : Char; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
begin
  Pos := System.Pos(C, Posi);
  Result := Pos > 0;
end;

function Long2StrL(L : LongInt) : string;
  {-Convert an integer type to a string.}
begin
  Result:= IntToStr(L);
end;

function TrimL(const Str : string) : string;
  {-Return a string with leading and trailing white space removed.}
var
  Int : Longint;
begin
  Result := Str;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    SetLength(Result, Pred(Length(Result)));

  Int := 1;
  while (Int <= Length(Result)) and (Result[Int] <= ' ') do
    Inc(Int);
  Dec(Int);
  if Int > 0 then
    System.Delete(Result, 1, Int);
end;

function AddBackSlashL(const DirName : string) : string;
  {-Add a default backslash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then
    Exit;
  if ((Length(Result) = 2) and (Result[2] = ':')) or
     ((Length(Result) > 2) and (Result[Length(Result)] <> '\')) then
    Result := Result + '\';
end;

function CommaizeChL(L : Longint; Ch : Char) : string;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp : string;
  NumCommas, Len : Cardinal;
  Neg : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrL(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for var I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeL(L : LongInt) : string;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChL(L, ',');
end;

procedure EnumerateFiles(StartDir : string;
                         FL : TStrings;
                         SubDirs : Boolean;
                         IncludeItem : TIncludeItemFunc);

    procedure SearchBranch;
    var
      SR    : TSearchRec;
      Error : SmallInt;
      Dir   : string;
    begin
      Error := FindFirst('*.*', faAnyFile, SR);
      GetDir(0, Dir);
      if Dir[Length(Dir)] <> '\' then
        Dir := Dir + '\';
      while Error = 0 do
      begin
        try
          if (@IncludeItem = nil) or (IncludeItem(SR, True)) then
            FL.Add(Dir + SR.Name);
        except
          on EOutOfMemory do
          begin
            raise EOutOfMemory.Create('..\source\String list is full');
          end;
        end;
        Error := FindNext(SR);
      end;
      FindClose(SR);

      if SubDirs then
      begin
        Error := FindFirst('*.*', faAnyFile, SR);
        while Error = 0 do
        begin
          if ((SR.Attr and faDirectory = faDirectory) and
              (SR.Name <> '.') and (SR.Name <> '..')) then
          begin
            if (@IncludeItem = nil) or (IncludeItem(SR, False)) then
            begin
              ChDir(SR.Name);
              SearchBranch;
              ChDir('..');
            end;
          end;
          Error := FindNext(SR);
        end;
        FindClose(SR);
      end;
    end;

var
  OrgDir : string;

begin
  if IsDirectory(StartDir) then
  begin
    GetDir(0, OrgDir);
    try
      ChDir(StartDir);
      SearchBranch;
    finally
      ChDir(OrgDir);
    end;
  end else
    raise Exception.Create('Invalid starting directory');
end;

{!!.01 -- Rewritten}
function IsDirectory(const DirName : string) : Boolean;
{-Return true if DirName is a directory}
var
  Attrs : DWORD;
begin
  Result := False;
    Attrs := GetFileAttributes(PChar(DirName));
  if Attrs <> DWORD(-1) then
    Result := (FILE_ATTRIBUTE_DIRECTORY and Attrs <> 0);
end;
{!!.01 -- End Rewritten}

(*
function IsDirectory(const FName : string) : Boolean;
 {-Return true if FName is a directory}
var
  SLen : Cardinal;
  D, CurDir,
  CurDestDir : string;
  DiffDrive : Boolean;
begin
  Result := False;
  GetDir(0, CurDir);
  SLen := Length(FName);
  if SLen = 0 then Exit;
  if ((SLen > 3) or (FName[2] <> ':')) and (FName[SLen] = '\') then Exit;
  if (SLen >= 2) and (FName[2] = ':') and (FName[1] <> CurDir[1]) then
  begin
    {Checking on a different drive}
    DiffDrive := True;
    D := System.Copy(FName, 1, 2);
    try
      ChDir(D);
    except
      on EInOutError do Exit;
    end;
    GetDir(0, CurDestDir);
  end else
    DiffDrive := False;

  try
    ChDir(FName);
    Result := True;
  except
    on EInOutError do Result := False;
  end;

  if DiffDrive then
    ChDir(CurDestDir);

  ChDir(CurDir);
end;
*)

{$IFNDEF VERSION2009}
procedure LoadILFunctions;
begin
  Shell32Inst := LoadLibrary('shell32.dll');
  if Shell32Inst <> 0 then begin
    @ILClone        := GetProcAddress(Shell32Inst, PChar(18));           // Win2000
    @ILCloneFirst   := GetProcAddress(Shell32Inst, PChar(19));           // Win2000
    @ILCombine      := GetProcAddress(Shell32Inst, PChar(25));           // Win2000
    @ILGetNext      := GetProcAddress(Shell32Inst, PChar(153));          // Win2000
    @ILFindLastID   := GetProcAddress(Shell32Inst, PChar(16));           // Win2000
    @ILIsEqual      := GetProcAddress(Shell32Inst, PChar(21));           // Win2000
    @ILRemoveLastID := GetProcAddress(Shell32Inst, PChar(17));           // Win2000
    @ILGetSize      := GetProcAddress(Shell32Inst, PChar(152));          // Win2000
    @ILFree         := GetProcAddress(Shell32Inst, PChar(155));          // Win2000
    @SHChangeNotifyRegister   := GetProcAddress(Shell32Inst, PChar(2));  // deprecated as of Vista
    @SHChangeNotifyDeregister := GetProcAddress(Shell32Inst, PChar(4));  // deprecated as of Vista
  end;
end;
{$ENDIF}

function GetParentPidl(Pidl: PItemIDList) : PItemIDList;
var
  TempPidl : PItemIDList;
begin
  TempPidl := ILClone(Pidl);
  ILRemoveLastID(TempPidl);
  Result := ILFindLastID(TempPidl);
end;

function GetSpecialFolderPath(Handle : TStHwnd; Folder : TStSpecialRootFolder) : string;
var
  IDList : PItemIDList;
  Buff : array [0..MAX_PATH * 2{ - 1}] of Char;
begin
  if SHGetSpecialFolderLocation(
      Handle, ShellFolders[Folder], IDList) <> NOERROR then
    RaiseStError(ESsShellError, ssscShellVersionError);
  if not SHGetPathFromIDList(IDList, Buff) then
    RaiseStError(ESsShellError, ssscShellVersionError);
  Result := Buff;
end;

function FilterFunc(const SR : TSearchRec; B : Boolean) : Boolean; far;
begin
  if (SR.Attr and faDirectory = faDirectory) then
    Result := False
  else
    Result := True;
end;

procedure GetSpecialFolderFiles(Handle : TStHwnd;
                                Folder : TStSpecialRootFolder;
                                Files : TStrings);
var
  Str : string;
begin
  Str := GetSpecialFolderPath(Handle, Folder);
  if not DirectoryExists(Str) then
    RaiseStError(ESsShellError, ssscShellVersionError);
  EnumerateFiles(Str, Files, False, FilterFunc);
end;

constructor TSsShellComponent.Create(AOwner : TComponent);
var
  VI     : TSsVersionInfo;
  WinDir : array [0..MAX_PATH - 1] of Char;
begin
  inherited Create(AOwner);
  VI := TSsVersionInfo.Create(AOwner);
  try
    GetSystemDirectory(WinDir, MAX_PATH);
    VI.FileName := WinDir + '\shell32.dll';
    FShellVersion := VI.FileVersionFloat;
  finally
    VI.Free;
  end;
end;

procedure TSsShellComponent.CheckSystemError(ErrCode : Integer);
var
  Buff : array [0..1023] of Char;
begin
  if ErrCode <> 0 then
    FError := ErrCode
  else
    FError := GetLastError;
  if FError <> 0 then begin
    if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
        nil, FError, 0, Buff, Length(Buff), nil) <> 0) then
      FErrorString := Buff
    else
      FErrorString := '';
  end else
    FErrorString := '';
end;

procedure TSsShellComponent.DoError;
begin
  if Assigned(FOnError) then
    FOnError(Self);
end;

constructor TSsCustomVersionInfo.Create(AOwner : TComponent);
begin
  VInfoLoaded := False;
  SetFileName('');
end;

destructor TSsCustomVersionInfo.Destroy;
begin
  inherited Destroy;
end;

function TSsCustomVersionInfo.LoadVersionInfo(const Key : string) : string;
var
  Handle : DWORD;
  Res     : Boolean;
  Size    : Integer;
  Error   : LongInt;
  Data    : Pointer;
  Buffer  : Pointer;
  ErrCode : Integer;
  {$IFDEF VERSION4}
  Bytes   : Cardinal;
  {$ELSE}
  Bytes   : Integer;
  {$ENDIF}
  TempStr : array [0..259] of Char;
  BaseStr : string;
  InfoStr : string;
  Trans   : PVerTranslation;
  TrSize  : Integer;
  FixedInfo : TVSFixedFileInfo;

  function MakeFloat(Str : string) : Double;
  var
    Buff  : array [0..5] of Char;
    Count : Integer;
  begin
    Count := 0;
    FillChar(Buff, SizeOf(Buff), 0);
    Buff[0] := '0';
    { The file version string might be specified like }
    { 4.72.3105.0. Parse it down to just one decimal  }
    { place and create the floating point version #.  }
    for var I := 1 to Pred(Length(Str)) do begin
      if Str[I] = '.' then begin
        { Found the first period. Replace it with the DecimalSeparator }
        { constant so that StrToFloat works properly. }
        Str[I] := {$IFDEF VERSIONXE}FormatSettings.{$ENDIF}DecimalSeparator;
        Inc(Count);
        if (Count = 2) and (I <= Length(Buff)) then begin
          Move(Str[1], Buff, (I - 1) * SizeOf(Char));
          Break;
        end;
      end;
    end;
    Result := StrToFloat(Buff);
  end;

begin
  TrSize := 0;
  Size := GetFileVersionInfoSize(StrPCopy(TempStr, FFileName), Handle);
  if Size = 0 then begin
    { In Win32 GetFileVersionInfoSize might fail because the }
    { file is a 16-bit file or because the file does not     }
    { contain version info. }
    Error := GetLastError;
    if Error = ERROR_RESOURCE_TYPE_NOT_FOUND then
      RaiseStError(ESsVersionInfoError, ssscNoVerInfo);
    if Error = 0 then
      RaiseStError(ESsVersionInfoError, ssscVerInfoFail);
  end;

  { Allocate some memory and get version info block. }
  GetMem(Data, Size);
  Res := GetFileVersionInfo(TempStr, Handle, Size, Data);
  Trans  := nil;
  try
    if not Res then
      { Error. Raise an exception. }
      RaiseStError(ESsVersionInfoError, ssscVerInfoFail);

    { Get the translation value. We need it to get the version info. }
    Res := VerQueryValue(Data, '\VarFileInfo\Translation', Buffer, Bytes);
    if not Res then
      RaiseStError(ESsVersionInfoError, ssscVerInfoFail);
    TrSize := Bytes;
    GetMem(Trans, TrSize);
    Move(Buffer^, Trans^, TrSize);
    VInfoLoaded := True;

    { Build a base string including the translation value. }
    BaseStr := Format('StringFileInfo\%.4x%.4x\', [Trans^.Language, Trans^.CharSet]);

    { User-defined string. Get the string and exit. }
    if Key <> '' then begin
      InfoStr := BaseStr + Key;
      Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);

      if Res then begin
        Result := StrPas(PChar(Buffer));
        Exit;
      end else begin
        Result := '';
        RaiseStError(ESsVersionInfoError, ssscBadVerInfoKey);
      end;
    end;

    { Get the fixed version info. }
    Bytes := SizeOf(FixedInfo);
    FillChar(FixedInfo, Bytes, 0);
    { '\' is used to get the root block. }
    Res := VerQueryValue(Data, '\', Buffer, Bytes);
    if not Res then
      RaiseStError(ESsVersionInfoError, ssscVerInfoFail);

    Move(Buffer^, FixedInfo, Bytes);
    with FixedInfo do begin
      FFileMajorVersion := dwFileVersionMS;
      FFileMinorVersion := dwFileVersionLS;
      FProductMajorVersion := dwProductVersionMS;
      FProductMinorVersion := dwProductVersionLS;
      FFileOS := dwFileOS;
    end;

    { FileVersion }
    InfoStr := BaseStr + 'FileVersion';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then begin
      FFileVersion := StrPas(PChar(Buffer));
      { First try to convert the version number to a float as-is. }
      Val(FFileVersion, FFileVersionFloat, ErrCode);
      if ErrCode <> 0 then
        { Failed. Create the float with the local MakeFloat function. }
        try
          FFileVersionFloat := MakeFloat(FFileVersion);
        except
          FFileVersionFloat := 0;
        end;
    end else begin
      FFileVersion := '';
      FFileVersionFloat := 0;
    end;

    { ProductVersion }
    InfoStr := BaseStr + 'ProductVersion';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then begin
      FProductVersion := StrPas(PChar(Buffer));
      { First try to convert the product number to a float as-is. }
      Val(FProductVersion, FProductVersionFloat, ErrCode);
      if ErrCode <> 0 then
        { Failed. Create the float with the local MakeFloat function. }
        try
          FProductVersionFloat := MakeFloat(FProductVersion);
        except
          FProductVersionFloat := 0;
        end;
    end else begin
      FProductVersion := '';
      FProductVersionFloat := 0;
    end;
  finally
    FreeMem(Data, Size);
    FreeMem(Trans, TrSize);
  end;
end;

function TSsCustomVersionInfo.GetFileVersion : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileVersion;
end;

function TSsCustomVersionInfo.GetProductVersion : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductVersion;
end;

function TSsCustomVersionInfo.GetProductVersionFloat : Double;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductVersionFloat;
end;

function TSsCustomVersionInfo.GetFileVersionFloat : Double;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileVersionFloat;
end;

procedure TSsCustomVersionInfo.SetFileName(const Value : string);
var
  Buff : array [0..255] of Char;
begin
  if (Value <> '') then
    if not FileExists(Value) then
      RaiseStError(ESsVersionInfoError, ssscFileOpen);
  if FFileName <> Value then
    VInfoLoaded := False;
  FFileName := Value;
  { If FileName is an emtpy string then load the }
  { version info for the current process.        }
  if (FFileName = '') then
    if GetModuleFileName(0, Buff, Length(Buff)) = 0 then
      FFileName := ''
    else
      FFileName := StrPas(Buff);
end;

function TSsCustomVersionInfo.GetFileOS: LongInt;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileOS;
end;

function TSsCustomVersionInfo.GetFileMajorVersion: LongInt;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileMajorVersion;
end;

function TSsCustomVersionInfo.GetFileMinorVersion: LongInt;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileMinorVersion;
end;

function TSsCustomVersionInfo.GetProductMajorVersion: LongInt;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductMajorVersion;
end;

function TSsCustomVersionInfo.GetProductMinorVersion: LongInt;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductMinorVersion;
end;

initialization
  PidlFormat := RegisterClipboardFormat(CFSTR_SHELLIDLIST);
  {$IFNDEF VERSION2009}
  LoadILFunctions;  //SZ can be imported statically since Win2000; do not use LoadLibrary in initialization of a dll
  {$ENDIF}

finalization
  {$IFNDEF VERSION2009}
  if Shell32Inst <> 0 then begin
    FreeLibrary(Shell32Inst);
    Shell32Inst := 0;
  end;
  {$ENDIF}
end.


