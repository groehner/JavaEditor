unit UUtils;

interface

{
  var p, q1, q2, q3, fq: int64;

  queryPerformanceCounter(q1);
  do something
  queryPerformanceCounter(q2);
  queryPerformanceFrequency(fq);
  q3:= q2-q1;
  f:= q3/fq;

  Start: TDateTime; i: int64;
  begin
  Start:= Now;
  do something
  i:= MilliSecondsBetween(Now, Start);
  end;
}

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  Graphics,
  Controls,
  StdCtrls,
  Types,
  UITypes,
  SpTBXItem,
  RegularExpressions;

const
  CecLeft = 1; // Move cursor left one char
  CecRight = 2; // Move cursor right one char
  CecUp = 3; // Move cursor up one line
  CecDown = 4; // Move cursor down one line
  CecLineStart = 7; // Move cursor to beginning of line
  CecLineEnd = 8; // Move cursor to end of line
  CecDeleteLine = 507; // Delete current line

type
  EInvalidDest = class(EStreamError);
  EFCantMove = class(EStreamError);
  TVisibility = (viPrivate, viPackage, viProtected, viPublic, viPublished);

  TPipeHandles = record
    hRead, hWrite: THandle;
  end;

  TInteger = class
  private
    FInt: Integer;
  public
    constructor Create(AInt: Integer);
    property Int: Integer read FInt write FInt;
  end;

  TBoolEvent = procedure(Value: Boolean) of object;

  TMatchHelper = record helper for TMatch
  public
    function GroupIndex(Index: Integer): Integer;
    function GroupLength(Index: Integer): Integer;
    function GroupValue(Index: Integer): string;
  end;

  (* Helper method for forms *)
  TControlHelper = class helper for TControl
  public
    (* Scale a value according to the FCurrentPPI *)
    function PPIScale(ASize: Integer): Integer;
    (* Reverse PPI Scaling *)
    function PPIUnScale(ASize: Integer): Integer;
  end;

  (*
    Minimalist SmartPointer implementation based on a blog post by Barry Kelly:
    http://blog.barrkel.com/2008/11/reference-counted-pointers-revisited.html,
    https://stackoverflow.com/questions/30153682/why-does-this-optimization-of-a-smartpointer-not-work
  *)
  TSmartPtr = record
  private type
    TObjectHandle<T: class> = class(TInterfacedObject, TFunc<T>)
    private
      FValue: T;
    protected
      function Invoke: T;
    public
      constructor Create(AValue: T);
      destructor Destroy; override;
    end;
  public
    class function Make<T: class>(AValue: T): TFunc<T>; static;
  end;

function IsAscii(const Str: string): Boolean;
function ANSI2ASCII(const AText: string): string;
function IsHTML(const Pathname: string): Boolean;
function HasJavaExtension(const Pathname: string): Boolean;
function HasPascalExtension(const Pathname: string): Boolean;
function HasUMLExtension(const Pathname: string): Boolean;
function IsHTTP(const Pathname: string): Boolean;
function StripHttp(const Pathname: string): string;

function IsUNC(const Pathname: string): Boolean;
function IsCHM(const Pathname: string): Boolean;
function HasClassExtension(const Pathname: string): Boolean;
function GetClass(const Classpath, CompilerParameter, Pathname: string): string;

function ExtractFileNameEx(Str: string): string;
function ExtractFilePathEx(const Str: string): string;
function StripHttpParams(const Str: string): string;
function GetFileSize(const Filename: string): LongInt;
function IsWriteProtected(const Filename: string): Boolean;
function RemoveReadOnlyAttribute(const FileName: string): Boolean;
function SetReadOnlyAttribute(const FileName: string): Boolean;
function HasWriteAccess(const Directory: string): Boolean;
function AddFileExt(const Filename, AFilter, Ext: string;
  Filterindex: Integer): string;
function HideBlanks(const Str: string): string;
function UnHideBlanks(const Str: string): string;
function ValidFilename(Str: string): Boolean;
procedure LockFormUpdate(Form: TForm);
procedure UnlockFormUpdate(Form: TForm);
procedure ScrollEndListBox(ListBox: TListBox);
function GetDocumentsPath: string;
function DissolveUsername(const Str: string): string;
function CtrlPressed: Boolean;
function VistaOrBetter: Boolean;
function IsAdministrator: Boolean;
procedure RotateBitmap(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean;
  BkColor: TColor = clNone);

procedure TidyPath(const Dir: string);
function GetTempDir: string;
function GetExeForExtension(const Ext: string): string;
function GetRegisteredJavaEditor: string;
function HasAssociationWithJavaeditor(const Extension: string): Boolean;

procedure SendKeys(Window: HWND; const Str: string);
procedure SendKeysGlobal(const Str: string);
procedure SendCtrlKeyGlobal(Chr: Char);
procedure SendAltKeyGlobal(Chr: Char);
procedure SendKeyGlobal(Chr: Char);
procedure SendShortCutStringGlobal(Str: string);

function GetEnvironmentVar(const VarName: string): string;
procedure SetEnvironmentVar(const VarName, VarValue: string);
procedure ExpandPath(const Str: string);
function HasDefaultPrinter: Boolean;
procedure SetAnimation(Value: Boolean);
function GetAnimation: Boolean;

procedure SetPrinterIndex(Int: Integer);
procedure PrintBitmap(FormBitmap: TBitmap; PixelsPerInch: Integer;
  PrintScale: TPrintScale = poProportional);
function GetComputerNetName: string;
procedure LockWindow(Handle: THandle);
procedure UnlockWindow;
function HttpToWeb(Str: string): string;
function ToWeb(const Browser: string; Str: string): string;
function ToWindows(Str: string): string;
function ToRepository(const Str: string): string;
function ToBackSlash(const Str: string): string;
function ToHtml(const Str: string): string;

function WithTrailingSlash(const Str: string): string;
function EndsWith(const Str, Substr: string): Boolean;
function StartsWith(const Str, Substr: string): Boolean;
function StartsWithInsensitive(const Str, Substr: string): Boolean;

function WindowStateToStr(WindowState: TWindowState): string;
function StrToWindowState(const Str: string): TWindowState;
function UpperLower(const Str: string): string;
function LowerUpper(const Str: string): string;

function GlobalMinimizeName(Filename: string; Canvas: TCanvas;
  MaxLen: Integer): string;
procedure ComboBoxAdd(ComboBox: TComboBox);
procedure ComboBoxDelete2(ComboBox: TComboBox; const Str: string);
procedure ComboBoxInsert(ComboBox: TComboBox);
procedure ComboBoxInsert2(ComboBox: TComboBox; const Str: string);
function SaveComboBoxItems(Str: string): string;
function LoadComboBoxItems(Str: string): string;
function HideCrLf(const Str: string): string;
function UnHideCrLf(const Str: string): string;
function GetServer(Str: string): string;
procedure StreamWriteln(FileStream: TFileStream; const Str: string);
procedure StreamWritelnANSI(FileStream: TFileStream; const Str: string);
function DownloadFile(const SourceFile, DestFile: string): Boolean;
function DownloadURL(const AUrl, AFile: string): Boolean;

procedure DisableMI(MenuItem: TSpTBXItem);
procedure DisableTB(ToolButton: TSpTBXItem);
procedure SetEnabledMI(MenuItem: TSpTBXItem; Enabled: Boolean);
procedure SetEnabledTB(ToolButton: TSpTBXItem; Enabled: Boolean);
procedure SetVisibleMI(MenuItem: TSpTBXItem; Visible: Boolean);
function CountChar(Chr: Char; const Str: string): Integer;
procedure ErrorMsg(const Str: string);
procedure InformationMsg(const Str: string);
function GetShortType(Str: string): string;
function GetShortTypeWith(Str: string): string;
function GetShortMethod(Str: string): string;

function IsSimpleType(const Str: string): Boolean;
function IsSimpleTypeOrString(const Str: string): Boolean;
function WithoutGeneric(Str: string): string;
function GenericOf(const Str: string): string;
function WithoutArray(const Str: string): string;
function ChangeGenericType(const Str: string): string;
function ExtractPackageName(const CName: string): string;
function ExtractClassName(const CName: string): string;

function Split(Delimiter: Char; Input: string): TStringList;
function Left(const Str: string; Posi: Integer): string;
function Right(const Str: string; Posi: Integer): string;
function GetIndent(const Str: string): Integer;
function IsLower(Chr: Char): Boolean;
function RGB2Color(const Red, Green, Blue: Integer): Integer;
procedure Color2RGB(const Color: TColor; var Red, Green, Blue: Integer);
function ChangeColor(Color: TColor; Percent: Real): TColor;
function IsWordBreakChar(Chr: Char): Boolean; overload;
function IsAlpha(Chr: Char): Boolean;
function IsWordInLine(Word, Line: string): Boolean;

function IsDigit(Chr: Char): Boolean;
function GetFirstWord(Str: string): string;
function GetNextPart(var Str: string): string; overload;
function GetNextPart(var Str: string; Chr: Char): string; overload;
function WithoutThrows(const Str: string): string;
function IsJavaString(const Str: string): Boolean;
function IsJavaChar(const Str: string): Boolean;
procedure AddStrings(Dest, Source: TStringList);
function FileIs32Bit(const Filename: string): Boolean;

function EncodeQuotationMark(const Str: string): string;
procedure CreateMyFile(const Path: string);
function ChangeVowels(const Str: string): string;
function WithoutSpaces(const Str: string): string;
function OnlyCharsAndDigits(const Str: string): string;
function VisibilityAsString(Vis: TVisibility): string;
function String2Visibility(const Str: string): TVisibility;
function IsVisibility(const Str: string): Boolean;
function Visibility2ImageNumber(Vis: TVisibility): Integer;
function IsModifier(const Str: string): Boolean;
function GetFilterIndex(Filter: string; const Filename: string): Integer;
function GetLongPathName(const Pathname: string): string;
function FileExistsCaseSensitive(const Filename: TFileName): Boolean;
function IsMouseOverControl(const Control: TControl): Boolean;
function SameText(const Str1, Str2: string): Boolean;
function AddWithSpace(Str: string): string;
function Max3(Int1, Int2, Int3: Integer): Integer;
function Min3(Int1, Int2, Int3: Integer): Integer;
function GetProtocolAndDomain(Url: string): string;
function IsRunning(Process: THandle): Boolean;
procedure CloseProcessinformationHandle(var ProcessInformation
  : TProcessInformation);
procedure TerminateTask(var ProcessInformation: TProcessInformation);
procedure ClosePipeHandles(var Pipe: TPipeHandles);
function LeftSpaces(Str: string; TabW: Integer): Integer;
function ConvertLtGt(Str: string): string;
function IntToVal(IntX: Integer): string;
function FloatToVal(RealX: Real): string;
function MyColorToRGB(Color: TColor): string;
function PointToVal(Posi: TPoint): string;
function XYToVal(XPos, YPos: Integer): string;
function CanActuallyFocus(WinControl: TWinControl): Boolean;
procedure SetDefaultUIFont(const AFont: TFont);
procedure Log(Str: string);
function CompiledRegEx(Expr: string; Options: TRegExOptions = [roNotEmpty];
  UCP: Boolean = True): TRegEx;
function MyMulDiv(NNumber, NNumerator, NDenominator: Integer): Integer;
function StringTimesN(Str: string; Num: Integer): string;
function GetUsersWindowsLanguage: string;
function Obfuscate(const Str: string): string;
function StringZuSingle(Str: string): Single;
{ Styled MessageDlg (do not use TaskDialog) }
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt): Integer; overload;
function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: LongInt; DefaultButton: TMsgDlgBtn)
  : Integer; overload;
function HTMLEncode(const Str: string): string;
function ColorToHTML(Color: TColor): string;
function IsColorDark(AColor: TColor): Boolean;
function LightenColor(Color: TColor; Percentage: Integer): TColor;
function DarkenColor(Color: TColor; Percentage: Integer): TColor;
function DefaultCodeFontName: string;

implementation

uses
  Registry,
  Messages,
  Dialogs,
  UrlMon,
  WinInet,
  Printers,
  Math,
  StrUtils,
  Character,
  TlHelp32,
  FileCtrl,
  System.IOUtils,
  RegularExpressionsCore,
  RegularExpressionsAPI,
  Winapi.SHFolder,
  Winapi.ShLwApi;

function IsAscii(const Str: string): Boolean;
begin
  for var I := 1 to Length(Str) do
    if Ord(Str[I]) > 127 then
      Exit(False);
  Result := True;
end;

function ANSI2ASCII(const AText: string): string;
const
  MaxLength = 1024;
var
  PText: PChar;
begin
  PText := StrAlloc(MaxLength);
  StrPCopy(PText, AText);
  Result := StrPas(PText);
  StrDispose(PText);
end;

function IsHTML(const Pathname: string): Boolean;
begin
  Result := (Pos('.HTM', UpperCase(ExtractFileExt(Pathname))) > 0);
end;

function HasJavaExtension(const Pathname: string): Boolean;
begin
  Result := (ExtractFileExt(Pathname) = '.java');
end;

function HasPascalExtension(const Pathname: string): Boolean;
begin
  Result := (UpperCase(ExtractFileExt(Pathname)) = '.PAS');
end;

function HasUMLExtension(const Pathname: string): Boolean;
begin
  Result := (UpperCase(ExtractFileExt(Pathname)) = '.UML');
end;

function IsHTTP(const Pathname: string): Boolean;
begin
  var
  APath := UpperCase(Pathname);
  Result := (Pos('HTTP://', APath) + Pos('RES://', APath) + Pos('HTTPS://',
    APath) > 0);
end;

function StripHttp(const Pathname: string): string;
begin
  var
  Posi := 1;
  var
  APath := UpperCase(Pathname);
  if Pos('HTTP://', APath) = 1 then
    Posi := 8;
  if Pos('HTTPS://', APath) = 1 then
    Posi := 9;
  Result := Copy(Pathname, Posi, Length(Pathname));
end;

function IsUNC(const Pathname: string): Boolean;
begin
  Result := (Pos('\\', Pathname) = 1);
end;

function IsCHM(const Pathname: string): Boolean;
begin
  Result := (Pos('.CHM', UpperCase(Pathname)) > 0);
end;

function HasClassExtension(const Pathname: string): Boolean;
begin
  Result := (Pos('.class', Pathname) > 0);
end;

function GetClass(const Classpath, CompilerParameter, Pathname: string): string;
var
  Str, AClasspath, DStr, AClassname: string;
  Posi, Int: Integer;
begin
  Result := '';
  AClassname := ChangeFileExt(Pathname, '.class');
  if FileExists(AClassname) then
  begin
    Result := AClassname;
    Exit;
  end;
  Str := ExtractFileName(AClassname);
  AClasspath := Classpath + ';';
  Posi := Pos(';', AClasspath);
  repeat
    DStr := Copy(AClasspath, 1, Posi - 1);
    if Copy(DStr, Length(DStr), 1) <> '\' then
      DStr := DStr + '\';
    Delete(AClasspath, 1, Posi);
    if (DStr <> '.\') and FileExists(DStr + Str) then
    begin
      Result := DStr + Str;
      Exit;
    end;
    Posi := Pos(';', AClasspath);
  until Posi = 0;
  // the class files can be stored separately with the parameter -DStr
  // if it is a package, the folder structure must be taken into account  DStr:= CompilerParameter;
  Posi := Pos('-DStr ', DStr);
  if Posi > 0 then
  begin
    DStr := Trim(Copy(DStr, Posi + 3, Length(DStr)));
    Posi := Pos(' ', DStr);
    if Posi > 0 then
      DStr := Copy(DStr, 1, Posi - 1);
    Int := Length(AClassname);
    repeat
      if AClassname[Int] = '\' then
      begin
        Str := DStr + Copy(AClassname, Int, Length(AClassname));
        if FileExists(Str) then
        begin
          Result := Str;
          Exit;
        end;
      end;
      Dec(Int);
    until Int = 0;
  end;
end;

function ExtractFileNameEx(Str: string): string;
var
  Int: Integer;
begin
  if Pos('file:///', Str) > 0 then
  begin
    Delete(Str, 1, 8);
    Str := ReplaceStr(Str, '/', '\');
  end;
  Str := StripHttpParams(Str);
  if IsHTTP(Str) then
  begin
    Int := Length(Str);
    while (Str[Int] <> '/') do
      Dec(Int);
    Delete(Str, 1, Int);
    if Str = '' then
      Str := 'index.html';
  end
  else if IsCHM(Str) then
  begin
    Int := Pos('.CHM', UpperCase(Str));
    Delete(Str, 1, Int + 3);
    if Copy(Str, 1, 2) = '::' then
      Delete(Str, 1, 2);
    Str := ExtractFileName(ToWindows(Str));
  end
  else
    Str := ExtractFileName(Str);
  Result := Str;
end;

function StripHttpParams(const Str: string): string;
var
  Int: Integer;
  FName: string;
begin
  FName := ExtractFileName(Str);
  Int := Pos('?', FName);
  if Int > 0 then
    Delete(FName, Int, Length(FName));
  Int := Pos('#', FName);
  if Int > 0 then
    Delete(FName, Int, Length(FName));
  Result := ExtractFilePath(Str) + FName;
end;

function ExtractFilePathEx(const Str: string): string;
begin
  if IsHTTP(Str) then
  begin
    var
    Posi := Length(Str);
    while Str[Posi] <> '/' do
      Dec(Posi);
    Result := Copy(Str, 1, Posi);
  end
  else
    Result := ExtractFilePath(Str);
  Result := ExcludeTrailingPathDelimiter(Result);
end;

function GetFileSize(const Filename: string): LongInt;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(Filename), faAnyFile, SearchRec) = 0 then
    Result := SearchRec.Size
  else
    Result := -1;
  SysUtils.FindClose(SearchRec);
end;

{$WARNINGS OFF}
function IsWriteProtected(const Filename: string): Boolean;
var
  Attributes: TFileAttributes;
begin
  if not TFile.Exists(Filename) then
    Exit(False);
  Attributes := TFile.GetAttributes(Filename);
  Result := TFileAttribute.faReadOnly in Attributes;
end;

function RemoveReadOnlyAttribute(const FileName: string): Boolean;
var
  Attr: Integer;
begin
  Result := False;
  Attr := FileGetAttr(FileName);
  if Attr <> -1 then // -1 bedeutet, dass die Datei nicht existiert
  begin
    Result := FileSetAttr(FileName, Attr and not faReadOnly) = 0;
  end;
end;

function SetReadOnlyAttribute(const FileName: string): Boolean;
var
  Attr: Integer;
begin
  Result := False;
  Attr := FileGetAttr(FileName);
  if Attr <> -1 then
  begin
    Result := FileSetAttr(FileName, Attr or faReadOnly) = 0;
  end;
end;
{$WARNINGS ON}

function HasWriteAccess(const Directory: string): Boolean;
begin
  var
  Str := WithTrailingSlash(Directory) + 'wa_test.$$$';
  try
    var
    FileStream := TFileStream.Create(Str, fmCreate or fmShareExclusive);
    FreeAndNil(FileStream);
    Result := DeleteFile(PChar(Str));
  except
    Result := False;
  end;
end;

function AddFileExt(const Filename, AFilter, Ext: string;
  Filterindex: Integer): string;
var
  Int, Posi, Count: Integer;
  Filter: string;
begin
  Filter := AFilter;
  if ExtractFileExt(Filename) = '' then
  begin
    Filter := Filter + '|';
    Count := 2 * Filterindex - 1;
    Posi := 0;
    Int := 1;
    while Posi <> Count do
    begin
      if Filter[Int] = '|' then
        Inc(Posi);
      Inc(Int);
    end;
    Delete(Filter, 1, Int);
    Delete(Filter, Pos('|', Filter), Length(Filter));
    Posi := Pos(';', Filter);
    if Posi > 0 then
      Delete(Filter, Posi, Length(Filter));
    if Filter = '.*' then
      Result := Filename + Ext
    else
      Result := Filename + Filter;
  end
  else
    Result := Filename;
end;

function HideBlanks(const Str: string): string;
begin
  if (Pos(' ', Str) > 0) and (Pos('"', Str) <> 1) then
    Result := AnsiQuotedStr(Str, '"')
  else
    Result := Str;
end;

function UnHideBlanks(const Str: string): string;
begin
  Result := AnsiDequotedStr(Str, '"');
end;

function ValidFilename(Str: string): Boolean;
begin
  Str := UpperCase(ChangeFileExt(ExtractFileName(Str), ''));
  Result := (Str <> 'CON') and (Str <> 'PRN') and (Str <> 'AUX') and
    (Str <> 'NUL');
end;

function CtrlPressed: Boolean;
begin
  Result := (GetAsyncKeyState(VK_CONTROL) and $F000) <> 0;
end;

function VistaOrBetter: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion >= 6);
end;

procedure RotateBitmap(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean;
  BkColor: TColor = clNone);
var
  CoSin: Single;
  Str: Single;
  Tmp: TBitmap;
  OffsetX: Single;
  OffsetY: Single;
  Points: array [0 .. 2] of TPoint;
begin
  CoSin := Cos(Rads);
  Str := Sin(Rads);
  Tmp := TBitmap.Create;
  try
    Tmp.TransparentColor := Bmp.TransparentColor;
    Tmp.TransparentMode := Bmp.TransparentMode;
    Tmp.Transparent := Bmp.Transparent;
    Tmp.Canvas.Brush.Color := BkColor;
    if AdjustSize then
    begin
      Tmp.Width := Round(Bmp.Width * Abs(CoSin) + Bmp.Height * Abs(Str));
      Tmp.Height := Round(Bmp.Width * Abs(Str) + Bmp.Height * Abs(CoSin));
      OffsetX := (Tmp.Width - Bmp.Width * CoSin + Bmp.Height * Str) / 2;
      OffsetY := (Tmp.Height - Bmp.Width * Str - Bmp.Height * CoSin) / 2;
    end
    else
    begin
      Tmp.Width := Bmp.Width;
      Tmp.Height := Bmp.Height;
      OffsetX := (Bmp.Width - Bmp.Width * CoSin + Bmp.Height * Str) / 2;
      OffsetY := (Bmp.Height - Bmp.Width * Str - Bmp.Height * CoSin) / 2;
    end;
    Points[0].X := Round(OffsetX);
    Points[0].Y := Round(OffsetY);
    Points[1].X := Round(OffsetX + Bmp.Width * CoSin);
    Points[1].Y := Round(OffsetY + Bmp.Width * Str);
    Points[2].X := Round(OffsetX - Bmp.Height * Str);
    Points[2].Y := Round(OffsetY + Bmp.Height * CoSin);
    PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width,
      Bmp.Height, 0, 0, 0);
    Bmp.Assign(Tmp);
  finally
    FreeAndNil(Tmp);
  end;
end;

procedure TidyPath(const Dir: string);
var
  Str, Path: string;
  StringList: TStringList;
begin
  Path := GetEnvironmentVariable('Path');
  StringList := Split(';', Path);
  try
    for var I := StringList.Count - 1 downto 0 do
    begin
      Str := StringList[I] + '\javac.exe';
      if FileExists(Str) then
        StringList.Delete(I);
    end;
    Path := Dir + ReplaceStr(StringList.Text, #13#10, ';');
    SetEnvironmentVariable('Path', PChar(Path));
  finally
    FreeAndNil(StringList);
  end;
end;

function GetTempDir: string;
var
  Posi: PChar;
begin
  Posi := StrAlloc(255);
  GetTempPath(255, Posi);
  Result := string(Posi);
  StrDispose(Posi);
end;

function GetRegisteredJavaEditor: string;
begin
  var
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CLASSES_ROOT;
  Reg.Access := KEY_READ;
  if Reg.OpenKey('JavaEditor\shell\open\command', False) then
    Result := Reg.ReadString('')
  else
    Result := '';
  FreeAndNil(Reg);
end;

function GetExeForExtension(const Ext: string): string;
var
  Reg: TRegistry;
  Str: string;
  Posi: Integer;
begin
  Str := '';
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.Access := KEY_READ;
  try
    if Reg.OpenKey('SOFTWARE\Classes\' + Ext + '\shell\open\command', False)
    then
    begin
      { The open command has been found }
      Str := Reg.ReadString('');
      Reg.CloseKey;
    end
    else
    begin
      { perhaps there is a system file pointer }
      if Reg.OpenKey('SOFTWARE\Classes\' + Ext, False) then
      begin
        try
          Str := Reg.ReadString('');
        except
          Str := '';
        end;
        Reg.CloseKey;
        if Str <> '' then
        begin
          { A system file pointer was found }
          if Reg.OpenKey('SOFTWARE\Classes\' + Str + '\Shell\Open\command',
            False) then
            { The open command has been found }
            Str := Reg.ReadString('');
          Reg.CloseKey;
        end;
      end;
    end;
  except
    Str := '';
  end;
  { Delete any command line, quotes and spaces }
  if Pos('%', Str) > 0 then
    Delete(Str, Pos('%', Str), Length(Str));
  Str := Trim(Str);
  Posi := Pos('.EXE', UpperCase(Str));
  if Posi > 0 then
    Delete(Str, Posi + 4, Length(Str));
  Posi := Pos('"', Str);
  if Posi = 1 then
  begin
    Delete(Str, 1, 1);
    Posi := Pos('"', Str);
    if Posi > 0 then
      Delete(Str, Posi, Length(Str));
  end;
  if FileExists(Str) then
    Result := Str
  else
    Result := '';
  FreeAndNil(Reg);
end;

function HasAssociationWithJavaeditor(const Extension: string): Boolean;
begin
  var
  Str := UpperCase(GetExeForExtension(Extension));
  Result := Pos(UpperCase(ExtractFileName(ParamStr(0))), Str) > 0;
end;

procedure SendKeys(Window: HWND; const Str: string);
begin
  // VK_KEYs are send
  for var I := 1 to Length(Str) do
  begin
    PostMessage(Window, WM_KEYDOWN, Ord(Str[I]), 0);
    PostMessage(Window, WM_KEYUP, Ord(Str[I]), 0);
  end;
end;

procedure SendKeysGlobal(const Str: string);
var
  AWord: Word;
begin
  for var I := 1 to Length(Str) do
  begin
    AWord := VkKeyScan(Str[I]);
    if AWord > 255 then
    begin
      if AWord and 256 = 256 then
        keybd_event(VK_SHIFT, 0, 0, 0);
      if AWord and 512 = 512 then
        keybd_event(VK_CONTROL, 0, 0, 0);
      if AWord and 1024 = 1024 then
        keybd_event(VK_MENU, 0, 0, 0);
    end;
    keybd_event(AWord, 0, 0, 0);
    keybd_event(AWord, 0, KEYEVENTF_KEYUP, 0);
    if AWord > 255 then
    begin
      if AWord and 256 = 256 then
        keybd_event(VK_SHIFT, 0, KEYEVENTF_KEYUP, 0);
      if AWord and 512 = 512 then
        keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
      if AWord and 1024 = 1024 then
        keybd_event(VK_MENU, 0, KEYEVENTF_KEYUP, 0);
    end;
  end;
end;

procedure SendCtrlKeyGlobal(Chr: Char);
// SendCtrlKeyGlobal('C') sends Ctrl-C
var
  AWord: Word;
begin
  AWord := VkKeyScan(Chr);
  keybd_event(VK_CONTROL, 0, 0, 0);
  keybd_event(AWord, 0, 0, 0);
  keybd_event(AWord, 0, KEYEVENTF_KEYUP, 0);
  keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
end;

procedure SendAltKeyGlobal(Chr: Char);
// SendAltKeyGlobal('C') sends Alt-C
var
  AWord: Word;
begin
  AWord := VkKeyScan(Chr);
  keybd_event(VK_MENU, 0, 0, 0);
  keybd_event(AWord, 0, 0, 0);
  keybd_event(AWord, 0, KEYEVENTF_KEYUP, 0);
  keybd_event(VK_MENU, 0, KEYEVENTF_KEYUP, 0);
end;

procedure SendKeyGlobal(Chr: Char);
// SendKeyGlobal('C') sends 'C'
var
  AWord: Word;
begin
  AWord := VkKeyScan(Chr);
  keybd_event(AWord, 0, 0, 0);
  keybd_event(AWord, 0, KEYEVENTF_KEYUP, 0);
end;

// z. B. '<Ctrl+Shift+C>ABC123'
procedure SendShortCutStringGlobal(Str: string);
var
  Str1: string;
  Posi: Integer;

  procedure SendOneShortCutGlobal(const Str: string);
  var
    Chr: Char;
    AWord: Word;
    Ctrl, Shift, Alt: Boolean;
  begin
    Ctrl := Pos('Ctrl', Str) + Pos('Strg', Str) > 0;
    Shift := Pos('Shift', Str) + Pos('Umsch', Str) > 0;
    Alt := Pos('Alt', Str) > 0;
    Chr := Str[Length(Str)];
    AWord := VkKeyScan(Chr);
    if Shift then
      keybd_event(VK_SHIFT, 0, 0, 0);
    if Ctrl then
      keybd_event(VK_CONTROL, 0, 0, 0);
    if Alt then
      keybd_event(VK_MENU, 0, 0, 0);
    keybd_event(AWord, 0, 0, 0);
    keybd_event(AWord, 0, KEYEVENTF_KEYUP, 0);
    if Shift then
      keybd_event(VK_SHIFT, 0, KEYEVENTF_KEYUP, 0);
    if Ctrl then
      keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
    if Alt then
      keybd_event(VK_MENU, 0, KEYEVENTF_KEYUP, 0);
  end;

  procedure SendOneCharGlobal(Chr: Char);
  var
    AWord: Word;
  begin
    AWord := VkKeyScan(Chr);
    keybd_event(AWord, 0, 0, 0);
    keybd_event(AWord, 0, KEYEVENTF_KEYUP, 0);
  end;

begin
  while Length(Str) > 0 do
  begin
    Posi := Pos('<', Str);
    if Posi = 1 then
    begin
      Posi := Pos('>', Str);
      Str1 := Copy(Str, 2, Posi - 2);
      SendOneShortCutGlobal(Str1);
      Delete(Str, 1, Posi);
    end
    else
    begin
      SendOneCharGlobal(Str[1]);
      Delete(Str, 1, 1);
    end;
  end;
end;

procedure ExpandPath(const Str: string);
begin
  var
  Path := GetEnvironmentVariable('Path');
  if Pos(Str, Path) = 0 then
    SetEnvironmentVariable('Path', PChar(Str + Path));
end;

function GetEnvironmentVar(const VarName: string): string;
begin
  var
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
  end
  else
    Result := '';
end;

procedure SetEnvironmentVar(const VarName, VarValue: string);
begin
  SetEnvironmentVariable(PChar(VarName), PChar(VarValue));
end;

function HasDefaultPrinter: Boolean;
begin
  Result:= (-1 < Printer.PrinterIndex) and (Printer.PrinterIndex < Printer.Printers.Count);
end;

procedure ScrollEndListBox(ListBox: TListBox);
var
  MaxVisible, MaxLength, Width: Integer;
begin
  MaxVisible := ListBox.Height div ListBox.ItemHeight;
  if ListBox.Items.Count > MaxVisible then
    ListBox.TopIndex := ListBox.Items.Count - MaxVisible + 1;
  ListBox.Update;
  MaxLength := ListBox.Width;
  ListBox.Canvas.Font.Assign(ListBox.Font);
  for var I := 0 to ListBox.Items.Count - 1 do
  begin
    Width := ListBox.Canvas.TextWidth(ListBox.Items[I]) + 50;
    if Width > MaxLength then
      MaxLength := Width;
  end;
  SendMessage(ListBox.Handle, LB_SETHORIZONTALEXTENT, MaxLength, 0);
end;

var
  FLockFormUpdatePile: Integer;

procedure LockFormUpdate(Form: TForm);
begin
  if Assigned(Form) then
  begin
    if FLockFormUpdatePile = 0 then
      Form.Perform(WM_SETREDRAW, 0, 0);
    Inc(FLockFormUpdatePile);
  end;
end;

procedure UnlockFormUpdate(Form: TForm);
begin
  if Assigned(Form) then
  begin
    Dec(FLockFormUpdatePile);
    if FLockFormUpdatePile = 0 then
    begin
      Form.Perform(WM_SETREDRAW, 1, 0);
      RedrawWindow(Form.Handle, nil, 0, RDW_FRAME + RDW_INVALIDATE +
        RDW_ALLCHILDREN + RDW_NOINTERNALPAINT);
    end;
  end;
end;

procedure StrResetLength(var Str: string);
begin
  for var I := 0 to Length(Str) - 1 do
    if Str[I + 1] = #0 then
    begin
      SetLength(Str, I);
      Exit;
    end;
end;

function GetLocalUserName: string;
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  SetLength(Result, Count);
  if GetUserName(PChar(Result), Count) then
    StrResetLength(Result)
  else
    Result := '';
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  BOOL(Info.iMinAnimate) := Value;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;

function IsGroupMember(RelativeGroupID: DWORD): Boolean;
var
  PsidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
const
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
begin
  Result := not(Win32Platform = VER_PLATFORM_WIN32_NT);
  if Result then // Win9x and ME don't have user groups
    Exit;
  PsidAdmin := nil;
  TokenInfo := nil;
  HaveToken := False;
  try
    Token := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
{$IFDEF FPC}
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, RelativeGroupID, 0, 0, 0, 0, 0, 0,
        PsidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, @Count) or
        (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
        RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo,
        Count, @Count));
{$ELSE FPC}
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, RelativeGroupID, 0, 0, 0, 0, 0, 0,
        PsidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
        (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
        RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo,
        Count, Count));
{$ENDIF FPC}
      for var I := 0 to TokenInfo^.GroupCount - 1 do
      begin
{$RANGECHECKS OFF} // Groups is an array [0..0] of TSIDAndAttributes, ignore ERangeError
        Result := EqualSid(PsidAdmin, TokenInfo^.Groups[I].Sid);
        if Result then
        begin
          // consider denied ACE with Administrator SID
          Result := TokenInfo^.Groups[I].Attributes and
            SE_GROUP_USE_FOR_DENY_ONLY <> SE_GROUP_USE_FOR_DENY_ONLY;
          Break;
        end;
{$IFDEF RANGECHECKS_ON}
{$RANGECHECKS ON}
{$ENDIF RANGECHECKS_ON}
      end;
    end;
  finally
    if Assigned(TokenInfo) then
      FreeMem(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if Assigned(PsidAdmin) then
      FreeSid(PsidAdmin);
  end;
end;

procedure SetPrinterIndex(Int: Integer);
var
  Device, Driver, Port: string;
  Dest: PChar;
  DeviceMode: THandle;
begin
  try
    Printer.PrinterIndex := Int;
    // TSetupPrinter only seems to look to the DeviceMode.dmDeviceName field
    // when setting up the initial printer, hence we make sure this field
    // contains the correct value.
    // This is a bug fix around the error in the delphi library,
    // as it does not use the DevNames structure correctly
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    if DeviceMode <> 0 then
    begin
      Dest := GlobalLock(DeviceMode);
      Device := Dest;
      GlobalUnlock(DeviceMode);
    end;
  except on E: Exception do
      OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' + E.Message));
  end;
end;

procedure PrintBitmap(FormBitmap: TBitmap; PixelsPerInch: Integer;
  PrintScale: TPrintScale = poProportional);
var
  InfoSize: DWORD;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: LongInt;
  PrintWidth, PrintHeight: LongInt;
  Info: PBitmapInfo;
  Image: Pointer;
begin
  // analogous to TCustomForm.Print
  Printer.BeginDoc;
  try
    FormBitmap.Canvas.Lock;
    try
      with Printer do
      begin
        Bits := FormBitmap.Handle;
        GetDIBSizes(Bits, InfoSize, ImageSize);
        Info := AllocMem(InfoSize);
        try
          Image := AllocMem(ImageSize);
          try
            GetDIB(Bits, 0, Info^, Image^);
            with Info.bmiHeader do
            begin
              DIBWidth := biWidth;
              DIBHeight := biHeight;
            end;
            case PrintScale of
              poProportional:
                begin
                  PrintWidth :=
                    MulDiv(DIBWidth, GetDeviceCaps(Handle, LOGPIXELSX),
                    PixelsPerInch);
                  PrintHeight :=
                    MulDiv(DIBHeight, GetDeviceCaps(Handle, LOGPIXELSY),
                    PixelsPerInch);
                end;
              poPrintToFit:
                begin
                  PrintWidth := MulDiv(DIBWidth, PageHeight, DIBHeight);
                  if PrintWidth < PageWidth then
                    PrintHeight := PageHeight
                  else
                  begin
                    PrintWidth := PageWidth;
                    PrintHeight := MulDiv(DIBHeight, PageWidth, DIBWidth);
                  end;
                end;
            else
              PrintWidth := DIBWidth;
              PrintHeight := DIBHeight;
            end;
            StretchDIBits(Canvas.Handle, 0, 0, PrintWidth, PrintHeight, 0, 0,
              DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
          finally
            FreeMem(Image, ImageSize);
          end;
        finally
          FreeMem(Info, InfoSize);
        end;
      end;
    finally
      FormBitmap.Canvas.Unlock;
    end;
  finally
    Printer.EndDoc;
  end;
end;

function IsAdministrator: Boolean;
begin
  Result := IsGroupMember(DOMAIN_ALIAS_RID_ADMINS);
end;

// from System.SysUtils
function GetDocumentsPath: string;
const
  CSIDL_PERSONAL = $0005;
var
  LStr: array [0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;

function DissolveUsername(const Str: string): string;
var
  Posi: Integer;
  Username: string;
begin
  Result := Str;
  Posi := Pos('%USERNAME%', UpperCase(Result));
  if Posi > 0 then
  begin
    Delete(Result, Posi, 10);
    Username := GetLocalUserName;
    Insert(Username, Result, Posi);
  end;
end;

function GetComputerNetName: string;
begin
  Result := GetEnvironmentVariable('COMPUTERNAME');
end;

procedure LockWindow(Handle: THandle);
begin
  LockWindowUpdate(Handle);
end;

procedure UnlockWindow;
begin
  LockWindowUpdate(0);
end;

function HttpToWeb(Str: string): string;
begin
  var
  Posi := Pos('\', Str);
  while Posi > 0 do
  begin
    Str[Posi] := '/';
    Posi := Pos('\', Str);
  end;
  Result := Str;
end;

function CodiereLeerzeichen(Str: string): string;
begin
  for var I := Length(Str) downto 1 do
    if Str[I] = ' ' then
    begin
      Delete(Str, I, 1);
      Insert('%20', Str, I);
    end;
  Result := Str;
end;

function ToWeb(const Browser: string; Str: string): string;
var
  Posi: Integer;
  IsUNC, IsDrive: Boolean;
begin
  IsUNC := (Copy(Str, 1, 2) = '\\');
  IsDrive := (Copy(Str, 2, 1) = ':');
  Str := CodiereLeerzeichen(Str);
  Posi := Pos('\', Str);
  while Posi > 0 do
  begin
    Str[Posi] := '/';
    Posi := Pos('\', Str);
  end;
  if IsUNC then
    if Pos('OPERA', UpperCase(Browser)) > 0 then
      Str := 'file:' + Str
    else
      Str := 'file://localhost/' + Str;
  if IsDrive then
    Str := 'file:///' + Str;
  Result := Str;
end;

function ToWindows(Str: string): string;
begin
  var
  Posi := Pos('/', Str);
  while Posi > 0 do
  begin
    Str[Posi] := '\';
    Posi := Pos('/', Str);
  end;
  Result := Str;
end;

function ToRepository(const Str: string): string;
begin
  Result := Str;
  var
  Posi := Pos(':', Result);
  if Posi = 2 then
    Delete(Result, 2, 1);
end;

function WindowStateToStr(WindowState: TWindowState): string;
begin
  Result := IntToStr(Ord(WindowState));
end;

function StrToWindowState(const Str: string): TWindowState;
begin
  if Str = '0' then
    Result := wsNormal
  else if Str = '1' then
    Result := wsMinimized
  else
    Result := wsMaximized;
end;

function UpperLower(const Str: string): string;
begin
  Result := UpperCase(Copy(Str, 1, 1)) + Copy(Str, 2, Length(Str));
end;

function LowerUpper(const Str: string): string;
begin
  Result := LowerCase(Copy(Str, 1, 1)) + Copy(Str, 2, Length(Str));
end;

function WithTrailingSlash(const Str: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(Str);
  if IsHTTP(Result) then
    Result := Result + '/'
  else
    Result := IncludeTrailingPathDelimiter(Result);
end;

function GlobalMinimizeName(Filename: string; Canvas: TCanvas;
  MaxLen: Integer): string;
var
  Width, Posi: Integer;
  Server, Test: string;
begin
  if IsHTTP(Filename) then
  begin
    Width := Canvas.TextWidth(Filename);
    if Width > MaxLen then
    begin
      Delete(Filename, 1, 7);
      Posi := Pos('/', Filename);
      if Posi = 0 then
        Posi := Length(Filename) + 1;
      Server := Copy(Filename, 1, Posi - 1);
      Delete(Filename, 1, Posi - 1);
      repeat
        Posi := Pos('.', Server);
        if Posi = 0 then
          Server := ''
        else
          Server := Copy(Server, Posi + 1, Length(Server));
        Test := 'https://...' + Server + Filename;
        Width := Canvas.TextWidth(Test);
        if Width < MaxLen then
        begin
          Result := Test;
          Exit;
        end;
      until Server = '';
      repeat
        Posi := Pos('/', Filename);
        if Posi > 0 then
          Filename := Copy(Filename, Posi + 1, Length(Filename));
        Test := 'https://...' + Filename;
        Width := Canvas.TextWidth(Test);
        if Width < MaxLen then
        begin
          Result := Test;
          Exit;
        end;
      until Posi = 0;
      Result := 'http://.../' + Filename;
    end
    else
      Result := Filename;
  end
  else
    Result := MinimizeName(Filename, Canvas, MaxLen);
end;

procedure ComboBoxAdd(ComboBox: TComboBox);
begin
  var
  Str := ComboBox.Text;
  ComboBox.Hint := Str;
  if Str = '' then
    Exit;
  var
  Int := ComboBox.Items.IndexOf(Str);
  if Int >= 0 then
    ComboBox.Items.Delete(Int);
  ComboBox.Items.Insert(0, Str);
  if ComboBox.Items.Count > 7 then
    ComboBox.Items.Delete(7);
  ComboBox.ItemIndex := 0;
end;

procedure ComboBoxDelete2(ComboBox: TComboBox; const Str: string);
begin
  var
  Int := ComboBox.Items.IndexOf(Str);
  if Int >= 0 then
    ComboBox.Items.Delete(Int);
end;

procedure ComboBoxInsert(ComboBox: TComboBox);
begin
  var
  Str := Trim(ComboBox.Text);
  if (Str <> '') and (ComboBox.Items.IndexOf(Str) = -1) then
    ComboBox.Items.Add(Str);
end;

procedure ComboBoxInsert2(ComboBox: TComboBox; const Str: string);
begin
  if (Str <> '') and (ComboBox.Items.IndexOf(Str) = -1) then
    ComboBox.Items.Add(Str);
end;

function SaveComboBoxItems(Str: string): string;
begin
  var
  Posi := Pos(#13#10, Str);
  while Posi > 0 do
  begin
    Delete(Str, Posi, 2);
    Insert(';', Str, Posi);
    Posi := Pos(#13#10, Str);
  end;
  Result := Str;
end;

function LoadComboBoxItems(Str: string): string;
begin
  var
  Posi := Pos(';', Str);
  while Posi > 0 do
  begin
    Delete(Str, Posi, 1);
    Insert(#13#10, Str, Posi);
    Posi := Pos(';', Str);
  end;
  Result := Str;
end;

function HideCrLf(const Str: string): string;
begin
  Result := ReplaceStr(Str, #13#10, '\r\n');
end;

function UnHideCrLf(const Str: string): string;
begin
  Result := ReplaceStr(Str, '\r\n', #13#10);
end;

function GetServer(Str: string): string;
begin
  Result := '';
  if IsHTTP(Str) then
  begin
    Str := StripHttp(Str);
    var
    Posi := Pos('/', Str);
    if Posi = 0 then
      Result := Str + '/'
    else
      Result := Copy(Str, 1, Posi);
  end;
end;

procedure StreamWriteln(FileStream: TFileStream; const Str: string);
begin
  if Assigned(FileStream) then
  begin
    var
    UStr := UTF8String(Str + #13#10);
    FileStream.Write(Pointer(UStr)^, Length(UStr));
  end;
end;

procedure StreamWritelnANSI(FileStream: TFileStream; const Str: string);
// DOS Codepage = 850
// Windows Codepage = 1252
type
  DOSString = type AnsiString(850);
var
  DStr: DOSString;
begin
  if Assigned(FileStream) then
  begin
    DStr := DOSString(Str + #13#10);
    FileStream.Write(Pointer(DStr)^, Length(DStr));
  end;
end;

// needs URLMon
function DownloadFile(const SourceFile, DestFile: string): Boolean;
begin
  try
    Result := URLDownloadToFile(nil, PChar(SourceFile), PChar(DestFile),
      0, nil) = 0;
  except
    Result := False;
  end;
end;

// needs WinInet
function DownloadURL(const AUrl, AFile: string): Boolean;
var
  HSession: HINTERNET;
  HService: HINTERNET;
  LpBuffer: array [0 .. 1024 + 1] of Char;
  DwBytesRead: DWORD;
  FStream: TFileStream;
begin
  Result := False;
  try
    FStream := TFileStream.Create(AFile, fmCreate or fmShareExclusive);
    HSession := InternetOpen('Java-Editor', INTERNET_OPEN_TYPE_PRECONFIG,
      nil, nil, 0);
    try
      if Assigned(HSession) then
      begin // no chache usage
        HService := InternetOpenUrl(HSession, PChar(AUrl), nil, 0,
          INTERNET_FLAG_RELOAD, 0);
        if Assigned(HService) then
          try
            while True do
            begin
              DwBytesRead := 1024;
              InternetReadFile(HService, @LpBuffer, 1024, DwBytesRead);
              if DwBytesRead = 0 then
                Break;
              Application.ProcessMessages;
              FStream.Write(LpBuffer[0], DwBytesRead);
            end;
            Result := True;
          finally
            InternetCloseHandle(HService);
          end;
      end;
    finally
      InternetCloseHandle(HSession);
      FreeAndNil(FStream);
    end;
  except
    on e: Exception do
      ErrorMsg(e.Message);
  end;
end;

procedure DisableMI(MenuItem: TSpTBXItem);
begin
  if Assigned(MenuItem) and MenuItem.Enabled then
    MenuItem.Enabled := False;
end;

procedure DisableTB(ToolButton: TSpTBXItem);
begin
  if Assigned(ToolButton) and ToolButton.Enabled then
    ToolButton.Enabled := False;
end;

procedure SetEnabledMI(MenuItem: TSpTBXItem; Enabled: Boolean);
begin
  if Assigned(MenuItem) and (MenuItem.Enabled <> Enabled) then
    MenuItem.Enabled := Enabled;
end;

procedure SetEnabledTB(ToolButton: TSpTBXItem; Enabled: Boolean);
begin
  if Assigned(ToolButton) and (ToolButton.Enabled <> Enabled) then
    ToolButton.Enabled := Enabled;
end;

procedure SetVisibleMI(MenuItem: TSpTBXItem; Visible: Boolean);
begin
  if Assigned(MenuItem) and (MenuItem.Visible <> Visible) then
    MenuItem.Visible := Visible;
end;

function CountChar(Chr: Char; const Str: string): Integer;
begin
  Result := 0;
  for var I := 1 to Length(Str) do
    if Str[I] = Chr then
      Inc(Result);
end;

procedure ErrorMsg(const Str: string);
begin
  MessageDlg(Str, mtError, [mbOK], 0);
end;

procedure InformationMsg(const Str: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      MessageDlg(Str, mtInformation, [mbOK], 0);
    end);
end;

function GetShortType(Str: string): string;
begin
  var
  Posi := Pos('$', Str);
  if Posi > 0 then
    Delete(Str, 1, Posi);
  Delete(Str, 1, LastDelimiter('.', Str));
  Posi := Pos('<', Str);
  if Posi > 1 then
    Str := Copy(Str, 1, Posi - 1);
  Result := Str;
end;

function GetShortMethod(Str: string): string;
var
  Posi: Integer;
  Str1: string;
begin
  Posi := Pos('throws ', Str);
  if Posi > 0 then
    Str := Trim(Copy(Str, 1, Posi - 1));
  Posi := Pos('(', Str);
  Str1 := GetShortTypeWith(Left(Str, Posi - 1)) + '(';
  Str := Copy(Str, Posi + 1, Length(Str) - Posi - 1) + ',';
  Posi := Pos(',', Str);
  while Posi > 0 do
  begin
    Str1 := Str1 + GetShortTypeWith(Left(Str, Posi - 1)) + ', ';
    Delete(Str, 1, Posi);
    Posi := Pos(',', Str);
  end;
  Result := Left(Str1, -2) + ')';
end;

function GetShortTypeWith(Str: string): string;
begin
  var
  Posi := Pos('$', Str);
  if Posi > 0 then
    Delete(Str, 1, Posi);
  Delete(Str, 1, LastDelimiter('.', Str));
  Result := Str;
end;

const
  SimpleTypePascal: array [1 .. 8] of string = ('integer', 'double', 'boolean',
    'char', 'Real', 'word', 'byte', 'shortint');

  SimpleTypeJava: array [1 .. 8] of string = ('int', 'double', 'boolean',
    'char', 'float', 'long', 'byte', 'short');

function IsSimpleType(const Str: string): Boolean;
begin
  Result := True;
  for var I := 1 to 8 do
    if SimpleTypePascal[I] = Str then
      Exit;
  for var I := 1 to 8 do
    if SimpleTypeJava[I] = Str then
      Exit;
  Result := False;
end;

function IsSimpleTypeOrString(const Str: string): Boolean;
begin // pascal
  Result := IsSimpleType(Str) or (Str = 'String') or (Str = 'java.lang.String')
    or (LowerCase(Str) = 'string');
end;

function WithoutGeneric(Str: string): string;
var
  Posi1, Posi2: Integer;
begin
  Posi1 := Pos('<', Str);
  if Posi1 > 0 then
  begin
    Posi2 := Length(Str);
    while (Posi2 > 0) and (Str[Posi2] <> '>') do
      Dec(Posi2);
    Delete(Str, Posi1, Posi2 - Posi1 + 1);
  end;
  if Pos('...', Str) > 0 then
    Str := Copy(Str, 1, Pos('...', Str) - 1);
  Result := Str;
end;

function GenericOf(const Str: string): string;
begin
  Result := '';
  var
  Posi := Pos('<', Str);
  if Posi > 0 then
    Result := Copy(Str, Posi + 1, Length(Str) - Posi - 1);
end;

function WithoutArray(const Str: string): string;
begin
  Result := Str;
  var
  Int := Pos('[', Result) + Pos('...', Result);
  if Int > 0 then
    Result := Copy(Result, 1, Int - 1);
end;

function ChangeGenericType(const Str: string): string;
begin
  if Pos('<', Str) > 0 then
    Result := Copy(Str, 1, Pos('<', Str)) + 'T>'
  else
    Result := Str;
end;

function ExtractPackageName(const CName: string): string;
begin
  var
  Int := LastDelimiter('.', CName);
  if Int = 0 then
    Result := ''
  else
    Result := Copy(CName, 1, Int - 1);
end;

function ExtractClassName(const CName: string): string;
begin
  var
  Int := LastDelimiter('.', CName);
  if Int = 0 then
    Result := CName
  else
    Result := Copy(CName, Int + 1, 255);
end;

function ToBackSlash(const Str: string): string;
begin
  Result := Str;
  for var I := 1 to Length(Str) do
    if (Result[I] = '.') or (Result[I] = '/') then
      Result[I] := '\';
end;

function ToHtml(const Str: string): string;
begin
  Result := Str;
  var
  Posi := Pos('<', Result);
  if Posi > 0 then
  begin
    Delete(Result, Posi, 1);
    Insert('&lt;', Result, Posi);
  end;
  Posi := Pos('>', Result);
  if Posi > 0 then
  begin
    Delete(Result, Posi, 1);
    Insert('&gt;', Result, Posi);
  end;
end;

function Split(Delimiter: Char; Input: string): TStringList;
begin
  var
  StringList := TStringList.Create;
  var
  Posi := Pos(Delimiter, Input);
  while Posi > 0 do
  begin
    StringList.Add(Copy(Input, 1, Posi - 1));
    Delete(Input, 1, Posi);
    Posi := Pos(Delimiter, Input);
  end;
  StringList.Add(Input);
  Result := StringList;
end;

function Left(const Str: string; Posi: Integer): string;
begin
  if Posi >= 0 then
    Result := Copy(Str, 1, Posi)
  else
    Result := Copy(Str, 1, Length(Str) + Posi);
end;

function Right(const Str: string; Posi: Integer): string;
begin
  if Posi >= 0 then
    Result := Copy(Str, Posi, Length(Str))
  else
    Result := Copy(Str, Length(Str) + Posi + 1, Length(Str));
end;

function GetIndent(const Str: string): Integer;
begin
  var
  Len := Length(Str);
  var
  Int := 1;
  while (Int <= Len) and (Str[Int] <= ' ') do
    Inc(Int);
  Result := Int - 1;
end;

function IsLower(Chr: Char): Boolean;
begin
  Result := CharInSet(Chr, ['a' .. 'z']);
end;

function RGB2Color(const Red, Green, Blue: Integer): Integer;
begin
  // convert hexa-decimal values to RGB
  Result := Red + Green shl 8 + Blue shl 16;
end;

procedure Color2RGB(const Color: TColor; var Red, Green, Blue: Integer);
begin
  // convert hexa-decimal values to RGB
  Red := Color and $FF;
  Green := (Color shr 8) and $FF;
  Blue := (Color shr 16) and $FF;
end;

function ChangeColor(Color: TColor; Percent: Real): TColor;
var
  Red, Green, Blue: Integer;
begin
  Red := 0;
  Green := 0;
  Blue := 0;
  Color2RGB(Color, Red, Green, Blue);
  Red := Round(Red * Percent);
  if Red > 255 then
    Red := 255;
  Green := Round(Green * Percent);
  if Green > 255 then
    Green := 255;
  Blue := Round(Blue * Percent);
  if Blue > 255 then
    Blue := 255;
  Result := RGB2Color(Red, Green, Blue);
end;

function IsWordBreakChar(Chr: Char): Boolean; // fromSyneditSearch
begin
  case Chr of
    #0 .. #32, '.', ',', ';', ':', '"', '''', '´', '`', '°', '^', '!', '?', '&',
      '$', '@', '§', '%', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>', '-',
      '=', '+', '*', '/', '\', '|':
      Result := True;
  else
    Result := False;
  end;
end;

function IsAlpha(Chr: Char): Boolean;
begin
  Result := Chr.IsLetter or (Chr = '_');
end;

function IsWordInLine(Word, Line: string): Boolean;
var
  Posi, QPos: Integer;
begin
  Result := False;
  Posi := Pos(Word, Line);
  if Posi > 0 then
  begin
    QPos := Posi + Length(Word);
    Result := ((Posi = 1) or (Posi > 1) and IsWordBreakChar(Line[Posi - 1])) and
      ((QPos > Length(Line)) or IsWordBreakChar(Line[QPos]));
  end;
end;

function IsDigit(Chr: Char): Boolean;
begin
  Result := ('0' <= Chr) and (Chr <= '9');
end;

function GetFirstWord(Str: string): string;
begin
  Str := Trim(Str);
  var
  Int := 1;
  while (Int <= Length(Str)) and not IsWordBreakChar(Str[Int]) do
    Inc(Int);
  Result := Copy(Str, 1, Int - 1);
end;

function GetNextPart(var Str: string; Chr: Char): string;
var
  QPos, Posi, Bracket: Integer;
begin
  Posi := Pos(Chr, Str);
  if Posi = 0 then
    Posi := Length(Str) + 1;
  QPos := Pos('<', Str); // generic part
  if (0 < QPos) and (QPos < Posi) then
  begin
    Bracket := 1;
    Inc(QPos);
    while (QPos <= Length(Str)) and (Bracket > 0) do
    begin
      if Str[QPos] = '<' then
        Inc(Bracket)
      else if Str[QPos] = '>' then
        Dec(Bracket);
      Inc(QPos);
    end;
    while (QPos <= Length(Str)) and (Str[QPos] <> Chr) do
      Inc(QPos);
    Posi := QPos;
  end;
  Result := Copy(Str, 1, Posi - 1);
  Delete(Str, 1, Posi);
end;

function GetNextPart(var Str: string): string;
begin
  Result := GetNextPart(Str, ' ');
end;

function WithoutThrows(const Str: string): string;
begin
  Result := Str;
  var
  Posi := Pos(' throws', Str);
  if Posi > 0 then
    Result := Copy(Str, 1, Posi - 1);
end;

function IsJavaString(const Str: string): Boolean;
begin
  Result := (Length(Str) >= 2) and (Left(Str, 1) = '"') and
    (Right(Str, -1) = '"');
end;

function IsJavaChar(const Str: string): Boolean;
begin
  Result := (Length(Str) >= 2) and (Left(Str, 1) = '''') and
    (Right(Str, -1) = '''');
end;

procedure AddStrings(Dest, Source: TStringList);
begin
  if Assigned(Dest) and Assigned(Source) then
    for var I := 0 to Source.Count - 1 do
      if Dest.IndexOf(Source[I]) = -1 then
        Dest.Add(Source[I]);
end;

function FileIs32Bit(const Filename: string): Boolean;
const
  IMAGE_FILE_32BIT_MACHINE = $0100;
var
  FileStream: TFileStream;
  PEOffset, PESignature, Characteristics: Integer;
begin
  Result := False;
  PEOffset := 0;
  PESignature := 0;
  Characteristics := 0;
  FileStream := TFileStream.Create(Filename, fmShareDenyNone);
  try
    FileStream.Seek($3C, soFromBeginning);
    FileStream.Read(PEOffset, 4);
    if PEOffset > $3C then
    begin
      FileStream.Seek(PEOffset, soFromBeginning);
      FileStream.Read(PESignature, 4);
      if PESignature = $00004550 then
      begin
        FileStream.Seek(18, soFromCurrent);
        FileStream.Read(Characteristics, 2);
        Result := (Characteristics and
          IMAGE_FILE_32BIT_MACHINE = IMAGE_FILE_32BIT_MACHINE);
      end;
    end;
  finally
    FreeAndNil(FileStream);
  end;
end;

function EncodeQuotationMark(const Str: string): string;
begin
  Result := ReplaceStr(Str, '"', '&quot;');
end;

procedure CreateMyFile(const Path: string);
begin
  if (Path <> '') and not FileExists(Path) then
  begin
    var
    StringList := TStringList.Create;
    try
      try
        SysUtils.ForceDirectories(ExtractFilePath(Path));
        StringList.SaveToFile(Path);
      except
        on e: Exception do
          ErrorMsg(e.Message);
      end;
    finally
      FreeAndNil(StringList);
    end;
  end;
end;

function ChangeVowels(const Str: string): string;
const
  Vowels = 'ÄÖÜäöüß';
  Nowels = 'AOUaous';
var
  Posi: Integer;
begin
  Result := Str;
  for var I := 1 to Length(Vowels) do
  begin
    Posi := Pos(Vowels[I], Result);
    while Posi > 0 do
    begin
      Result[Posi] := Nowels[I];
      if I < 7 then
        Insert('e', Result, Posi + 1)
      else
        Insert('s', Result, Posi + 1);
      Posi := Pos(Vowels[I], Result);
    end;
  end;
end;

function OnlyCharsAndDigits(const Str: string): string;
begin
  Result := '';
  for var I := 1 to Length(Str) do
  begin
    if Str[I].IsLetterOrDigit then
      Result := Result + Str[I];
  end;
end;

function WithoutSpaces(const Str: string): string;
begin
  Result := ReplaceStr(Str, ' ', '');
end;

function VisibilityAsString(Vis: TVisibility): string;
begin
  case Vis of
    viPrivate:
      Result := 'private';
    viPackage:
      Result := '';
    viProtected:
      Result := 'protected';
    viPublic:
      Result := 'public';
    viPublished:
      Result := 'published';
  else
    Result := '';
  end;
end;

function String2Visibility(const Str: string): TVisibility;
begin
  if Str = 'private' then
    Result := viPrivate
  else if Str = 'protected' then
    Result := viProtected
  else if Str = 'public' then
    Result := viPublic
  else if Str = 'published' then
    Result := viPublished
  else
    Result := viPackage;
end;

function Visibility2ImageNumber(Vis: TVisibility): Integer;
begin
  case Vis of
    viPrivate:
      Result := 7;
    viProtected:
      Result := 8;
    viPackage:
      Result := 9;
    viPublic:
      Result := 10;
  else
    Result := 0;
  end;
end;

function IsVisibility(const Str: string): Boolean;
const
  Visibilities: array [1 .. 4] of string = ('private', 'protected',
    'public', '');
begin
  Result := False;
  for var I := 1 to 4 do
    if Str = Visibilities[I] then
      Exit(True);
end;

function IsModifier(const Str: string): Boolean;
const
  Modifiers: array [1 .. 8] of string = ('static', 'abstract', 'final',
    'native', 'synchronized', 'transient', 'volatile', 'strictfp');
begin
  Result := False;
  for var I := 1 to 8 do
    if Str = Modifiers[I] then
    begin
      Result := True;
      Exit;
    end;
end;

function GetFilterIndex(Filter: string; const Filename: string): Integer;
var
  Ext: string;
  Posi: Integer;
begin
  Result := 0;
  Ext := LowerCase(ExtractFileExt(Filename));
  if Ext = '' then
    Exit;
  repeat
    Posi := Pos('|*', Filter);
    Delete(Filter, 1, Posi + 1);
    Result := Result + 1;
    if Pos(Ext, Filter) = 1 then
      Exit;
    Posi := Pos('|', Filter);
    Delete(Filter, 1, Posi);
  until Filter = '';
  Result := 0;
end;

function GetLongPathName(const Pathname: string): string;
var
  Drive: string;
  Path: string;
  SearchRec: TSearchRec;
begin
  if Pathname = '' then
    Exit;
  Drive := ExtractFileDrive(Pathname);
  Path := Copy(Pathname, Length(Drive) + 1, Length(Pathname));
  if (Path = '') or (Path = '\') then
  begin
    Result := Pathname;
    if Result[Length(Result)] = '\' then
      Delete(Result, Length(Result), 1);
  end
  else
  begin
    Path := GetLongPathName(ExtractFileDir(Pathname));
    if FindFirst(Pathname, faAnyFile, SearchRec) = 0 then
    begin
      Result := Path + '\' + SearchRec.FindData.cFileName;
      SysUtils.FindClose(SearchRec);
    end
    else
      Result := Path + '\' + ExtractFileName(Pathname);
  end;
end;

function EndsWith(const Str, Substr: string): Boolean;
begin
  var
  SubTextLocation := Length(Str) - Length(Substr) + 1;
  if (SubTextLocation > 0) and (Str <> '') then
    Result := CompareStr(Copy(Str, SubTextLocation, Length(Str)), Substr) = 0
  else
    Result := False;
end;

function StartsWith(const Str, Substr: string): Boolean;
begin
  if Str <> '' then
    Result := CompareStr(Copy(Str, 1, Length(Substr)), Substr) = 0
  else
    Result := False;
end;

function StartsWithInsensitive(const Str, Substr: string): Boolean;
begin
  if Str <> '' then
    Result := CompareText(Copy(Str, 1, Length(Substr)), Substr) = 0
  else
    Result := False;
end;

function FileExistsCaseSensitive(const Filename: TFileName): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := FindFirst(Filename, faAnyFile, SearchRec) = 0;
  if Result then
    SysUtils.FindClose(SearchRec);
  Result := Result and (SearchRec.Attr and faDirectory = 0) and
    (SearchRec.Name = ExtractFileName(Filename));
end;

function IsMouseOverControl(const Control: TControl): Boolean;
var
  ARect: TRect;
  Posi1, Posi2: TPoint;
begin
  Posi1 := Control.ClientToScreen(Point(0, 0));
  Posi2 := Control.ClientToScreen(Point(Control.Width, Control.Height));
  ARect := Rect(Posi1.X, Posi1.Y, Posi2.X, Posi2.Y);
  Result := PtInRect(ARect, Mouse.CursorPos);
end;

function SameText(const Str1, Str2: string): Boolean;
// not case sensitive
begin
  Result := CompareText(Str1, Str2) = 0;
end;

function AddWithSpace(Str: string): string;
begin
  Str := Trim(Str);
  if Str = '' then
    Result := ''
  else
    Result := ' ' + Str;
end;

function Max3(Int1, Int2, Int3: Integer): Integer;
begin
  Result := Max(Int1, Max(Int2, Int3));
end;

function Min3(Int1, Int2, Int3: Integer): Integer;
begin
  Result := Min(Int1, Min(Int2, Int3));
end;

function getProtocol(Url: string): string;
begin
  var
  Posi := Pos('://', Url);
  if Posi > 0 then
    Result := Copy(Url, 1, Posi + 2)
  else
    Result := '';
end;

function GetProtocolAndDomain(Url: string): string;
begin
  var
  Protocol := getProtocol(Url);
  Delete(Url, 1, Length(Protocol));
  var
  Posi := Pos('/', Url);
  if Posi > 0 then
    Result := Protocol + Copy(Url, 1, Posi - 1)
  else
    Result := Protocol + Url;
end;

function _KillTask(const APID: Cardinal;
const AKillStructure: Boolean = False): Boolean;
var
  ProcessEntry: TProcessEntry32;
  AHandle: THandle;
  AWord: DWORD;
  TokenPriv: TTokenPrivileges;
  HToken: THandle;
  HProcess: Cardinal;
  Goon: Boolean;

  function IsWinNT: Boolean;
  var
    VersionInf: TOSVersionInfo;
  begin
    VersionInf.dwOSVersionInfoSize := SizeOf(VersionInf);
    Result := False;
    if GetVersionEx(VersionInf) then
      Result := VersionInf.dwPlatformId = VER_PLATFORM_WIN32_NT;
  end;

begin
  Result := False;
  ProcessEntry.dwSize := SizeOf(ProcessEntry);
  AHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  Goon := True;
  try
    if Process32First(AHandle, ProcessEntry) then
    begin
      Result := True;
      repeat
        if ProcessEntry.th32ProcessID = APID then
        begin
          if IsWinNT then
          begin
            if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES,
              HToken) then
            begin
              LookupPrivilegeValue(nil, 'SeDebugPrivilege',
                TokenPriv.Privileges[0].Luid);
              TokenPriv.PrivilegeCount := 1;
              TokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
              AWord := 0;
              AdjustTokenPrivileges(HToken, False, TokenPriv, 0,
                PTokenPrivileges(nil)^, AWord);
              CloseHandle(HToken);
            end;
          end;
          HProcess := OpenProcess(PROCESS_TERMINATE, False, ProcessEntry.th32ProcessID);
          Goon := False;
          if HProcess <> 0 then
            try
              Result := Windows.TerminateProcess(HProcess, 0) and Result;
              if WaitForSingleObject(HProcess, INFINITE) = WAIT_OBJECT_0 then
                Result := False;
            finally
              CloseHandle(HProcess);
            end;
        end
        else if AKillStructure and (ProcessEntry.th32ParentProcessID = APID) then
          Result := _KillTask(ProcessEntry.th32ProcessID, True) and Result;
      until (not Process32Next(AHandle, ProcessEntry)) or
        ((not Goon) and (not AKillStructure));
    end;
  finally
    CloseHandle(AHandle);
  end;
end;

function IsRunning(Process: THandle): Boolean;
var
  LWord: LongWord;
begin
  Result := (Process <> 0) and GetExitCodeProcess(Process, LWord) and
    (LWord = STILL_ACTIVE);
end;

procedure CloseProcessinformationHandle(var ProcessInformation
  : TProcessInformation);
begin
  if ProcessInformation.hProcess <> 0 then
    CloseHandle(ProcessInformation.hProcess);
  if ProcessInformation.hThread <> 0 then
    CloseHandle(ProcessInformation.hThread);
  FillChar(ProcessInformation, SizeOf(ProcessInformation), #0);
end;

procedure TerminateTask(var ProcessInformation: TProcessInformation);
begin
  if IsRunning(ProcessInformation.hProcess) then
    _KillTask(ProcessInformation.dwProcessId, True);
  CloseProcessinformationHandle(ProcessInformation);
end;

procedure ClosePipeHandles(var Pipe: TPipeHandles);
begin
  with Pipe do
  begin
    if hRead <> 0 then
      CloseHandle(hRead);
    if hWrite <> 0 then
      CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
end;

function LeftSpaces(Str: string; TabW: Integer): Integer;
begin
  var
  Posi := PWideChar(Str);
  if Assigned(Posi) then
  begin
    Result := 0;
    while (Posi^ >= #1) and (Posi^ <= #32) do
    begin
      if Posi^ = #9 then
        Inc(Result, TabW)
      else
        Inc(Result);
      Inc(Posi);
    end;
  end
  else
    Result := 0;
end;

function ConvertLtGt(Str: string): string;
begin
  Result := ReplaceStr(ReplaceStr(Str, '<', '&lt;'), '>', '&gt;');
end;

function IntToVal(IntX: Integer): string;
begin
  Result := '"' + IntToStr(IntX) + '"';
end;

function FloatToVal(RealX: Real): string;
begin
  Result := '"' + FloatToStr(RealX) + '"';
  var
  Posi := Pos(',', Result);
  if Posi > 0 then
    Result[Posi] := '.';
end;

function MyColorToRGB(Color: TColor): string;
var
  ColorInt, Red, Green, Blue: Integer;
begin
  ColorInt := ColorToRGB(Color);
  Red := GetRValue(ColorInt);
  Green := GetGValue(ColorInt);
  Blue := GetBValue(ColorInt);
  Result := 'rgb(' + IntToStr(Red) + ',' + IntToStr(Green) + ',' +
    IntToStr(Blue) + ')';
end;

function PointToVal(Posi: TPoint): string;
begin
  Result := IntToStr(Posi.X) + ',' + IntToStr(Posi.Y) + ' ';
end;

function XYToVal(XPos, YPos: Integer): string;
begin
  Result := IntToStr(XPos) + ',' + IntToStr(YPos) + ' ';
end;

function CanActuallyFocus(WinControl: TWinControl): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  if Assigned(WinControl) and not WinControl.Focused then
  begin
    Form := GetParentForm(WinControl);
    if Assigned(Form) and Form.Enabled and Form.Visible then
      Result := WinControl.CanFocus;
  end;
end;

procedure SetDefaultUIFont(const AFont: TFont);
const
  UIFont = 'Segoe UI';
begin
  if CheckWin32Version(6) and not SameText(AFont.Name, UIFont) and
    (Screen.Fonts.IndexOf(UIFont) >= 0) then
  begin
    AFont.Size := 9;
    AFont.Name := UIFont;
  end;
end;

procedure Log(Str: string);
begin
  var
  StringList := TStringList.Create;
  StringList.LoadFromFile('C:\temp\log.txt');
  StringList.Add(Str);
  StringList.SaveToFile('C:\temp\log.txt');
  FreeAndNil(StringList);
end;

function CompiledRegEx(Expr: string; Options: TRegExOptions = [roNotEmpty];
UCP: Boolean = True): TRegEx;
begin
  try
    Result:= TRegEx.Create(Expr, Options);
    if UCP then
      Result.AddRawOptions(PCRE_UCP);
    Result.Study([preJIT]);
  except
    on e: ERegularExpressionError do
    begin
      MessageDlg(Format('Invalid Regular Expression: %s', [e.Message]), mtError,
        [mbOK], 0);
      Abort;
    end;
    else
      raise;
  end;
end;

function MyMulDiv(NNumber, NNumerator, NDenominator: Integer): Integer;
begin
  Result := MulDiv(NNumber, NNumerator, NDenominator);
end;

function StringTimesN(Str: string; Num: Integer): string;
begin
  Result := '';
  for var I := 1 to Num do
    Result := Result + Str;
end;

function GetUsersWindowsLanguage: string;
var
  WinLanguage: array [0 .. 50] of Char;
begin
  VerLanguageName(GetUserDefaultUILanguage, WinLanguage, 50);
  Result := WinLanguage;
end;

function Obfuscate(const Str: string): string;
// Reversible string obfuscation using the ROT13 algorithm
begin
  Result := Str;
  for var I := 1 to Length(Str) do
    case Ord(Str[I]) of
      Ord('A') .. Ord('M'), Ord('a') .. Ord('m'):
        Result[I] := Chr(Ord(Str[I]) + 13);
      Ord('N') .. Ord('Z'), Ord('n') .. Ord('z'):
        Result[I] := Chr(Ord(Str[I]) - 13);
      Ord('0') .. Ord('4'):
        Result[I] := Chr(Ord(Str[I]) + 5);
      Ord('5') .. Ord('9'):
        Result[I] := Chr(Ord(Str[I]) - 5);
    end;
end;

function StringZuSingle(Str: string): Single;
var
  FormatSetting: TFormatSettings;
begin
  // Feste FormatSettings definieren
  FormatSetting := TFormatSettings.Create;
  FormatSetting.DecimalSeparator := '.';
  // Dezimaltrennzeichen explizit auf Punkt setzen

  // Ersetze alle Kommata durch einen Punkt
  Str := StringReplace(Str, ',', '.', [rfReplaceAll]);

  // Versuche die Konvertierung
  try
    Result := StrToFloat(Str, FormatSetting);
  except
    on e: EConvertError do
      Result := 0.0;
  end;
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
Buttons: TMsgDlgButtons; HelpCtx: LongInt): Integer;
begin
  UseLatestCommonDialogs := False;
  Result := Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx);
  UseLatestCommonDialogs := True;
end;

function StyledMessageDlg(const Msg: string; DlgType: TMsgDlgType;
Buttons: TMsgDlgButtons; HelpCtx: LongInt; DefaultButton: TMsgDlgBtn): Integer;
begin
  UseLatestCommonDialogs := False;
  Result := Dialogs.MessageDlg(Msg, DlgType, Buttons, HelpCtx, DefaultButton);
  UseLatestCommonDialogs := True;
end;

function HTMLEncode(const Str: string): string;
var
  Chr: Char;
  StringBuild: TStringBuilder;
begin
  if Str = '' then
    Exit('');

  StringBuild := TStringBuilder.Create(Length(Str) * 2);
  // Initial capacity estimate
  try
    for Chr in Str do
    begin
      case Chr of
        '&':
          StringBuild.Append('&amp;');
        '"':
          StringBuild.Append('&quot;');
        '<':
          StringBuild.Append('&lt;');
        '>':
          StringBuild.Append('&gt;');
      else
        StringBuild.Append(Chr);
      end;
    end;
    Result := StringBuild.ToString;
  finally
    StringBuild.Free;
  end;
end;

function ColorToHTML(Color: TColor): string;
var
  ColorR: TColorRef;
begin
  ColorR := ColorToRGB(Color);
  Result := Format('#%.2x%.2x%.2x', [GetRValue(ColorR), GetGValue(ColorR),
    GetBValue(ColorR)]);
end;

function IsColorDark(AColor: TColor): Boolean;
var
  ACol: LongInt;
begin
  ACol := ColorToRGB(AColor) and $00FFFFFF;
  Result := ((2.99 * GetRValue(ACol) + 5.87 * GetGValue(ACol) + 1.14 *
    GetBValue(ACol)) < $400);
end;

function LightenColor(Color: TColor; Percentage: Integer): TColor;
begin
  Result := Winapi.ShLwApi.ColorAdjustLuma(ColorToRGB(Color),
    Percentage * 10, True);
end;

function DarkenColor(Color: TColor; Percentage: Integer): TColor;
begin
  Result := Winapi.ShLwApi.ColorAdjustLuma(ColorToRGB(Color),
    -Percentage * 10, True);
end;

function DefaultCodeFontName: string;
begin
    if CheckWin32Version(6) then
    begin
      if Screen.Fonts.IndexOf('Cascadia Code') >= 0 then
        Result := 'Cascadia Code'
      else
        Result := 'Consolas';
    end
    else
      Result := 'Courier New';
end;

{ TInteger }

constructor TInteger.Create(AInt: Integer);
begin
  inherited Create;
  Self.Int := AInt;
end;

{ TMatchHelper }

function TMatchHelper.GroupIndex(Index: Integer): Integer;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Index
  else
    Result := -1;
end;

function TMatchHelper.GroupLength(Index: Integer): Integer;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Length
  else
    Result := 0;
end;

function TMatchHelper.GroupValue(Index: Integer): string;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Value
  else
    Result := '';
end;

{ TFormHelper }

function TControlHelper.PPIScale(ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, FCurrentPPI, 96);
end;

function TControlHelper.PPIUnScale(ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, 96, FCurrentPPI);
end;

{ TSmartPointer }

constructor TSmartPtr.TObjectHandle<T>.Create(AValue: T);
begin
  FValue := AValue;
end;

destructor TSmartPtr.TObjectHandle<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TSmartPtr.TObjectHandle<T>.Invoke: T;
begin
  Result := FValue;
end;

class function TSmartPtr.Make<T>(AValue: T): TFunc<T>;
begin
  Result := TObjectHandle<T>.Create(AValue);
end;

end.
