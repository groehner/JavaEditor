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

uses Windows, Menus, SysUtils, Classes, Forms, Graphics, Controls, StdCtrls,
     {$WARNINGS OFF} FileCtrl {$WARNINGS ON}, ComCtrls, Buttons, Types, UITypes,
     SpTBXItem,
     RegularExpressions, RegularExpressionsCore, RegularExpressionsAPI;

const
  ecLeft            = 1;    // Move cursor left one char
  ecRight           = 2;    // Move cursor right one char
  ecUp              = 3;    // Move cursor up one line
  ecDown            = 4;    // Move cursor down one line
  ecLineStart       = 7;    // Move cursor to beginning of line
  ecLineEnd         = 8;    // Move cursor to end of line
  ecDeleteLine      = 507;  // Delete current line

type
  EInvalidDest = class(EStreamError);
  EFCantMove = class(EStreamError);
  TVisibility = (viPrivate, viPackage, viProtected, viPublic, viPublished);
  TPipeHandles = record
    hRead, hWrite: THandle;
  end;

  TBoolEvent = procedure (Value: Boolean) of object;

  TMatchHelper = record helper for TMatch
  public
    function GroupIndex(Index: integer): integer;
    function GroupLength(Index: integer): integer;
    function GroupValue(Index: integer): string;
  end;

  (*  Helper method for forms *)
  TControlHelper = class helper for TControl
  public
    (* Scale a value according to the FCurrentPPI *)
    function PPIScale(ASize: integer): integer;
    (* Reverse PPI Scaling  *)
    function PPIUnScale(ASize: integer): integer;
  end;

function isAscii(const s: string): boolean;
function ANSI2ASCII(const aText: string): string;
function IsHTML(const Pathname: string): Boolean;
function hasJavaExtension(const Pathname: string): Boolean;
function hasPascalExtension(const Pathname: string): Boolean;
function hasUMLExtension(const Pathname: string): Boolean;
function IsHTTP(const Pathname: string): Boolean;
function StripHttp(const Pathname: string): string;

function IsUNC(const Pathname: string): Boolean;
function IsCHM(const Pathname: string): Boolean;
function hasClassExtension(const Pathname: string): Boolean;
function getClass(const Classpath, CompilerParameter, Pathname: string): string;

function ExtractFileNameEx(s: string): string;
function ExtractFilePathEx(const s: string): string;
function StripHttpParams(const s: string): string;
function GetFileSize(const FileName: string): LongInt;
function IsWriteProtected(const Filename: string): boolean;
function HasWriteAccess(const Directory: string): boolean;
function AddFileExt(const filename, aFilter, ext: string; filterindex: integer): string;
function HideBlanks(const s: string): string;
function UnHideBlanks(const s: string): string;
function ValidFilename(s: string): boolean;
procedure LockFormUpdate(F: TForm);
procedure UnlockFormUpdate(F: TForm);
procedure ScrollEndListBox(LB: TListBox);
function GetDocumentsPath: string;
function GetHomePath: string;
function dissolveUsername(const s: string): string;
function CtrlPressed: boolean;
function VistaOrBetter: boolean;
function IsAdministrator: Boolean;
procedure RotateBitmap(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean;
  BkColor: TColor = clNone);

procedure TidyPath(const Dir: string);
function GetTempDir: string;
function GetExeForExtension(const Ext:string): string;
function getRegisteredJavaEditor: string;
function HasAssociationWithJavaeditor(const Extension: string): boolean;

procedure SendKeys(Window: HWnd; const s: string);
procedure SendKeysGlobal(const s: string);
procedure SendCtrlKeyGlobal(ch: char);
procedure SendAltKeyGlobal(ch: char);
procedure SendKeyGlobal(ch: char);
procedure SendShortCutStringGlobal(s: string);

function  GetEnvironmentVar(const VarName: string): string;
procedure SetEnvironmentVar(const VarName, VarValue: string);
procedure ExpandPath(const s: string);
function hasDefaultPrinter: boolean;
procedure SetAnimation(Value: boolean);
function GetAnimation: boolean;

procedure SetPrinterIndex(i : Integer );
procedure PrintBitmap(FormBitmap: TBitmap; PixelsPerInch: integer; PrintScale: TPrintScale = poProportional);
function GetComputerNetName: string;
procedure LockWindow(Handle: THandle);
procedure UnlockWindow;
function HttpToWeb(s: string): string;
function ToWeb(const Browser: string; s: string): string;
function ToWindows(s: string): string;
function ToRepository(const s: string): string;
function toBackSlash(const s : string) : string;
function toHtml(const s: string): string;

function withoutTrailingSlash(const s: string): string;
function withTrailingSlash(const s: string): string;
function EndsWith(const Str, Substr: string): Boolean;
function StartsWith(const Str, Substr: string): boolean;
function StartsWithInsensitive(const Str, Substr: string): boolean;

function WindowStateToStr(W: TWindowState): string;
function StrToWindowState(const s: string): TWindowState;
function UpperLower(const s: string): string;
function LowerUpper(const s: string): string;

function GlobalMinimizeName(Filename: string; Canvas: TCanvas; MaxLen: integer): string;
procedure ComboBoxAdd(ComboBox: TComboBox);
procedure ComboBoxDelete2(ComboBox: TComboBox; const s: string);
procedure ComboBoxInsert(ComboBox: TComboBox);
procedure ComboBoxInsert2(ComboBox: TComboBox; const s: string);
function SaveComboBoxItems(s: string): string;
function LoadComboBoxItems(s: string): string;
function HideCrLf(const s: string): string;
function UnHideCrLf(const s: string): string;
function getServer(s: string): string;
procedure StreamWriteln(FS: TFileStream; const s: string);
procedure StreamWritelnANSI(FS: TFileStream; const s: string);
function DownloadFile(const SourceFile, DestFile: string): Boolean;
function DownloadURL(const aUrl, aFile: string): boolean;

procedure DisableMI(MenuItem: TSpTBXItem);
procedure DisableTB(ToolButton: TSpTBXItem);
procedure SetEnabledMI(MenuItem: TSpTBXItem; Enabled: boolean);
procedure SetEnabledTB(ToolButton: TSpTBXItem; Enabled: boolean);
procedure SetVisibleMI(MenuItem: TSpTBXItem; Visible: boolean);
function CountChar(c: char; const s: string): integer;
procedure ErrorMsg(const s: string);
procedure InformationMsg(const s: string);
function getShortType(s: string): string;
function getShortTypeWith(s: string): string;
function getShortMethod(s: string): string;

function IsSimpleType(const s: string): boolean;
function IsSimpleTypeOrString(const s: string): boolean;
function WithoutGeneric(s: string): string;
function GenericOf(const s: string): string;
function WithoutArray(const s: string): string;
function ChangeGenericType(const s: string): string;
function ExtractPackageName(const CName: string): string;
function ExtractClassName(const CName: string): string;

function Split(Delimiter: Char; Input: string): TStringList;
function left(const s: string; p: integer): string;
function right(const s: string; p: integer): string;
function getIndent(const s: string): integer;
function isLower(c: char): boolean;
function RGB2Color(const R, G, B: Integer): Integer;
procedure Color2RGB(const Color: TColor; var R, G, B: Integer);
function ChangeColor(Color: TColor; percent: real): TColor;
function IsWordBreakChar(C: Char): Boolean; overload;
function IsAlpha(ch: Char): boolean;
function IsWordInLine(Word, Line: string): boolean;

function IsDigit(c: char): boolean;
function getFirstWord(s: string): string;
function GetNextPart(var s: string): string; overload;
function GetNextPart(var s: string; ch: char): string; overload;
function withoutThrows(const s: string): string;
//procedure myFreeAndNil(var Obj);
function IsJavaString(const s: string): boolean;
function IsJavaChar(const s: string): boolean;
procedure AddStrings(Dest, Source: TStringList);
function FileIs32Bit(const Filename: string): boolean;

function encodeQuotationMark(const s: string): string;
procedure CreateMyFile(const Path: string);
function changeVowels(const s: string): string;
function WithoutSpaces(const s: string): string;
function OnlyCharsAndDigits(const s: string): string;
function VisibilityAsString(vis: TVisibility) : string;
function String2Visibility(const s: string): TVisibility;
function IsVisibility(const s: string): boolean;
function Visibility2ImageNumber(vis: TVisibility): integer;
function IsModifier(const s: string): boolean;
function getFilterIndex(Filter: string; const Filename: string): integer;
function GetLongPathName(const PathName: string): string;
function FileExistsCaseSensitive(const Filename: TFileName): Boolean;
function IsMouseOverControl(const ctrl: TControl): boolean;
function sametext(const x, y : string) : boolean;
function AddWithSpace(s: string): string;
function max3(m1, m2, m3: integer): integer;
function min3(m1, m2, m3: integer): integer;
function getProtocolAndDomain(url: string): string;
function IsRunning(Process: THandle): Boolean;
procedure CloseProcessinformationHandle(var ProcessInformation: TProcessinformation);
procedure TerminateTask(var Processinformation: TProcessinformation);
procedure ClosePipeHandles(var Pipe: TPipeHandles);
function LeftSpaces(s: string; tabW: integer): Integer;
function ConvertLtGt(s: string): string;
function IntToVal(x: integer): string;
function FloatToVal(x: real): string;
function myColorToRGB(Color: TColor): string;
function PointToVal(P: TPoint): string;
function XYToVal(x, y: integer): string;
function CanActuallyFocus(WinControl: TWinControl): Boolean;
procedure SetDefaultUIFont(const AFont: TFont);
procedure Log(S: string);
function CompiledRegEx(Expr : string; Options: TRegExOptions = [roNotEmpty];
  UCP : Boolean = True): TRegEx;
function myMulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
function StringTimesN(s: string; n: integer): string;

implementation

uses Registry, Messages, Dialogs, URLMon, WinInet, Printers, Math, StrUtils,
     Character, TlHelp32, Winapi.SHFolder;

function isAscii(const s: string): boolean;
begin
  for var i:= 1 to Length(s) do
    if Ord(s[i]) > 127 then
      Exit(false);
  Result:= true;
end;

function ANSI2ASCII(const aText: string):string;
  const MaxLength = 1024;
  var PText : PChar;
begin
  PText:= StrAlloc(MaxLength);
  StrPCopy(PText, aText);
  Result:= StrPas(PText);
  StrDispose(PText);
end;

function IsHTML(const Pathname: string): Boolean;
begin
  Result:= (Pos('.HTM', UpperCase(ExtractFileExt(Pathname))) > 0);
end;

function hasJavaExtension(const Pathname: string): Boolean;
begin
  Result:= (ExtractFileExt(Pathname) = '.java');
end;

function hasPascalExtension(const Pathname: string): Boolean;
begin
  Result:= (Uppercase(ExtractFileExt(Pathname)) = '.PAS');
end;

function hasUMLExtension(const Pathname: string): Boolean;
begin
  Result:= (Uppercase(ExtractFileExt(Pathname)) = '.UML');
end;

function IsHTTP(const Pathname: string): Boolean;
begin
  var aPath:= Uppercase(Pathname);
  Result:= (pos('HTTP://', aPath) + pos('RES://', aPath) + pos('HTTPS://', aPath) > 0);
end;

function StripHttp(const Pathname: string): string;
begin
  var p:= 1;
  var aPath:= Uppercase(Pathname);
  if Pos('HTTP://', aPath) = 1
    then p:= 8;
  if Pos('HTTPS://', aPath) = 1
    then p:= 9;
  Result:= copy(Pathname, p, Length(Pathname));
end;


function IsUNC(const Pathname: string): boolean;
begin
  Result:= (pos('\\', Pathname) = 1);
end;

function IsCHM(const Pathname: string): boolean;
begin
  Result:= (pos('.CHM', UpperCase(Pathname)) > 0);
end;

function hasClassExtension(const Pathname: string): boolean;
begin
  Result:= (pos('.class', Pathname) > 0);
end;

function getClass(const Classpath, CompilerParameter, Pathname: string): string;
  var s, cp, d, aClassname: string;
      p, i: Integer;
begin
  Result:= '';
  aClassname:= ChangeFileExt(Pathname, '.class');
  if FileExists(aClassname) then begin Result:= aClassname; exit end;
  s:= ExtractFilename(aClassname);
  cp:= Classpath + ';';
  p:= Pos(';', cp);
  repeat
    d:= Copy(cp, 1, p-1);
    if Copy(d, Length(d), 1) <> '\' then d:= d + '\';
    Delete(cp, 1, p);
    if (d <> '.\') and FileExists(d + s) then begin
      Result:= d + s; Exit end;
    p:= Pos(';', cp);
  until p = 0;
  // the class files can be stored separately with the parameter -d
  // if it is a package, the folder structure must be taken into account  d:= CompilerParameter;
  p:= Pos('-d ', d);
  if p > 0 then begin
    d:= trim(copy(d, p+3, length(d)));
    p:= Pos(' ', d);
    if p > 0 then d:= copy(d, 1, p-1);
    i:= length(aClassname);
    repeat
      if aClassname[i] = '\' then begin
        s:= d + copy(aClassname, i, length(aClassname));
        if FileExists(s) then begin
          Result:= s; exit
        end;
      end;
      dec(i);
    until i = 0;
  end;
end;

function ExtractFileNameEx(s: string): string;
  var i: Integer;
begin
  if Pos('file:///', s) > 0 then begin
    delete(s, 1, 8);
    s:= ReplaceStr(s, '/', '\');
  end;
  s:= StripHttpParams(s);
  if IsHttp(s) then begin
    i:= Length(s);
    while (s[i] <> '/') do dec(i);
    Delete(s, 1, i);
    if s = '' then s:= 'index.html';
  end else if IsCHM(s) then begin
    i:= Pos('.CHM', Uppercase(s));
    delete(s, 1, i+3);
    if copy(s, 1, 2) = '::' then
      delete(s, 1, 2);
    s:= ExtractFileName(toWindows(s))
  end else
    s:= ExtractFileName(s);
  Result:= s;
end;

function StripHttpParams(const s: string): string;
    var i: integer; fname: string;
begin
  fname:= ExtractFilename(s);
  i:= Pos('?', fname);
  if i > 0 then delete(fname, i, length(fname));
  i:= Pos('#', fname);
  if i > 0 then delete(fname, i, length(fname));
  Result:= ExtractFilePath(s) + fname;
end;

function ExtractFilePathEx(const s: string): string;
begin
  if IsHttp(s) then begin
    var p:= length(s);
    while s[p] <> '/' do dec(p);
    Result:= copy(s, 1, p);
  end else
    Result:= ExtractFilePath(s);
  Result:= withoutTrailingSlash(Result);
end;

function GetFileSize(const FileName: string): LongInt;
  var SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0
    then Result:= SearchRec.Size
    else Result:= -1;
  SysUtils.FindClose(SearchRec);
end;

{$WARNINGS OFF}
function HasAttr(const FileName: string; Attr: Word): Boolean;
begin
  var Fileattr:= FileGetAttr(FileName);
  Result := (FileAttr and Attr) = Attr;
end;

function IsWriteProtected(const Filename: string): boolean;
begin
  Result:= FileExists(Filename) and HasAttr(Filename, faReadOnly);
end;

function HasWriteAccess(const Directory: string): boolean;
begin
  var s:= withTrailingSlash(Directory) + 'wa_test.$$$';
  try
    var FS:= TFileStream.Create(s, fmCreate or fmShareExclusive);
    FreeAndNil(FS);
    Result:= DeleteFile(PChar(s));
  except
    Result:= false;
  end;
end;

{$WARNINGS ON}

function AddFileExt(const filename, aFilter, ext: string; filterindex: integer): string;
  var i, p, Count: integer; filter: string;
begin
  filter:= aFilter;
  if ExtractFileExt(filename) = '' then begin
    filter:= filter + '|';
    Count:= 2*filterindex - 1;
    p:= 0; i:= 1;
    while p <> Count do begin
      if filter[i] = '|' then inc(p);
      inc(i);
    end;
    delete(filter, 1, i);
    delete(filter, Pos('|', filter), length(filter));
    p:= Pos(';', filter);
    if p > 0 then delete(filter, p, length(filter));
    if filter = '.*'
      then Result:= filename + ext
      else Result:= filename + filter;
  end
  else
    Result:= filename;
end;

function HideBlanks(const s: string): string;
begin
  if (Pos(' ', s) > 0) and (Pos('"', s) <> 1)
    then Result:= AnsiQuotedStr(s, '"')
    else Result:= s;
end;

function UnHideBlanks(const s: string): string;
begin
  Result:= AnsiDequotedStr(s, '"');
end;

function ValidFilename(s: string): boolean;
begin
  s:= Uppercase(ChangeFileExt(ExtractFilename(s), ''));
  Result:= (s <> 'CON') and (s <> 'PRN') and (s <> 'AUX') and (s <> 'NUL');
end;

function CtrlPressed: boolean;
begin
  Result:= (GetAsyncKeyState(VK_CONTROL) and $f000) <> 0;
end;

function VistaOrBetter: boolean;
begin
  result:= (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6);
end;

procedure RotateBitmap(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean;
  BkColor: TColor = clNone);
var
  C: Single;
  S: Single;
  Tmp: TBitmap;
  OffsetX: Single;
  OffsetY: Single;
  Points: array[0..2] of TPoint;
begin
  C := Cos(Rads);
  S := Sin(Rads);
  Tmp := TBitmap.Create;
  try
    Tmp.TransparentColor := Bmp.TransparentColor;
    Tmp.TransparentMode := Bmp.TransparentMode;
    Tmp.Transparent := Bmp.Transparent;
    Tmp.Canvas.Brush.Color := BkColor;
    if AdjustSize then
    begin
      Tmp.Width := Round(Bmp.Width * Abs(C) + Bmp.Height * Abs(S));
      Tmp.Height := Round(Bmp.Width * Abs(S) + Bmp.Height * Abs(C));
      OffsetX := (Tmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      OffsetY := (Tmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
    end
    else
    begin
      Tmp.Width := Bmp.Width;
      Tmp.Height := Bmp.Height;
      OffsetX := (Bmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      OffsetY := (Bmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
    end;
    Points[0].X := Round(OffsetX);
    Points[0].Y := Round(OffsetY);
    Points[1].X := Round(OffsetX + Bmp.Width * C);
    Points[1].Y := Round(OffsetY + Bmp.Width * S);
    Points[2].X := Round(OffsetX - Bmp.Height * S);
    Points[2].Y := Round(OffsetY + Bmp.Height * C);
    PlgBlt(Tmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width,
      Bmp.Height, 0, 0, 0);
    Bmp.Assign(Tmp);
  finally
    FreeAndNil(Tmp);
  end;
end;

procedure TidyPath(const Dir: string);
  var s, path: string; i: integer; SL: TStringList;
begin
  path:= GetEnvironmentVariable('PATH');
  SL:= Split(';', Path);
  try
    for i:= SL.Count - 1 downto 0 do begin
      s:= SL.Strings[i] + '\javac.exe';
      if FileExists(s) then
        SL.Delete(i);
    end;
    path:= Dir + ReplaceStr(SL.Text, #13#10, ';');
    SetEnvironmentVariable('PATH', PChar(path));
  finally
    FreeAndNil(SL);
  end;
end;

function GetTempDir: string;
  var P: PChar;
begin
  P:= StrAlloc(255);
  GetTempPath(255, P);
  Result:= string(P);
  StrDispose(P);
end;

function getRegisteredJavaEditor: string;
begin
  var reg:= TRegistry.Create;
  reg.RootKey:= HKEY_CLASSES_ROOT;
  reg.Access:= KEY_READ;
  if reg.OpenKey('JavaEditor\shell\open\command', false)
    then Result:= reg.ReadString('')
    else Result:= '';
  FreeAndNil(reg);
end;

function GetExeForExtension(const Ext: string): string;
 var
   reg: TRegistry;
   s  : string;
   p  : Integer;
 begin
   s:= '';
   reg:= TRegistry.Create;
   reg.RootKey:= HKEY_LOCAL_MACHINE;
   reg.Access:= KEY_READ;
   try
     if reg.OpenKey('SOFTWARE\Classes\' + ext + '\shell\open\command', false) then
      begin
       {The open command has been found}
       s:= reg.ReadString('');
       reg.CloseKey;
     end
     else begin
       {perhaps there is a system file pointer}
       if reg.OpenKey('SOFTWARE\Classes\' + ext, false) then begin
         try
           s:= reg.ReadString('');
         except
           s:= '';
         end;
         reg.CloseKey;
         if s <> '' then begin
           {A system file pointer was found}
           if reg.OpenKey('SOFTWARE\Classes\' + s + '\Shell\Open\command', false) then
             {The open command has been found}
             s:= reg.ReadString('');
           reg.CloseKey;
         end;
       end;
     end;
   except
     s:= '';
   end;
   {Delete any command line, quotes and spaces}
   if Pos('%', s) > 0 then
     Delete(s, Pos('%', s), length(s));
   s:= Trim(s);
   p:= Pos('.EXE', UpperCase(s));
   if p > 0 then Delete(s, p + 4, Length(s));
   p:= Pos('"', s);
   if p = 1 then begin
     Delete(s, 1, 1);
     p:= Pos('"', s);
     if p > 0 then Delete(s, p, length(s));
   end;
   if FileExists(s)
     then Result:= s
     else Result:= '';
   FreeAndNil(reg);
 end;

 function HasAssociationWithJavaeditor(const Extension: string): boolean;
 begin
   var s:= Uppercase(GetExeForExtension(Extension));
   Result:= Pos(UpperCase(ExtractFilename(ParamStr(0))), s) > 0;
 end;

  procedure SendKeys(Window: HWnd; const s: string);
  begin
    // VK_KEYs are send
    for var i:= 1 to Length(s) do begin
      PostMessage(Window, wm_KeyDown, ord(s[i]), 0);
      PostMessage(Window, wm_KeyUp, ord(s[i]), 0);
    end;
  end;

  procedure SendKeysGlobal(const s: string);
    var i: Integer; w: word;
  begin
    for i:= 1 to length(s) do begin
      w:= VkKeyScan(s[i]);
      if w > 255 then begin
        if w and  256 =  256 then keybd_event(vk_shift, 0,0,0);
        if w and  512 =  512 then keybd_event(vk_control, 0,0,0);
        if w and 1024 = 1024 then keybd_event(vk_menu, 0,0,0);
      end;
      keybd_event(w,0,0,0);
      keybd_event(w,0,KEYEVENTF_KEYUP,0);
      if w > 255 then begin
        if w and  256 =  256 then keybd_event(vk_shift, 0, KEYEVENTF_KEYUP,0);
        if w and  512 =  512 then keybd_event(vk_control, 0, KEYEVENTF_KEYUP,0);
        if w and 1024 = 1024 then keybd_event(vk_menu, 0, KEYEVENTF_KEYUP,0);
      end;
    end;
  end;

  procedure SendCtrlKeyGlobal(ch: char);
    // SendCtrlKeyGlobal('C') sends Ctrl-C
    var w: word;
  begin
    w:= VkKeyScan(ch);
    keybd_event(vk_control, 0,0,0);
    keybd_event(w,0,0,0);
    keybd_event(w,0,KEYEVENTF_KEYUP,0);
    keybd_event(vk_control, 0,KEYEVENTF_KEYUP,0);
  end;

  procedure SendAltKeyGlobal(ch: char);
    // SendAltKeyGlobal('C') sends Alt-C
    var w: word;
  begin
    w:= VkKeyScan(ch);
    keybd_event(vk_menu, 0,0,0);
    keybd_event(w,0,0,0);
    keybd_event(w,0,KEYEVENTF_KEYUP,0);
    keybd_event(vk_menu, 0,KEYEVENTF_KEYUP,0);
  end;

  procedure SendKeyGlobal(ch: char);
    // SendKeyGlobal('C') sends 'C'
    var  w:word;
  begin
    w:= VkKeyScan(ch);
    keybd_event(w,0,0,0);
    keybd_event(w,0,KEYEVENTF_KEYUP,0);
  end;

  // z. B. '<Ctrl+Shift+C>ABC123'
  procedure SendShortCutStringGlobal(s: string);
    var s1: string; p: integer;

    procedure SendOneShortCutGlobal(const s: string);
      var Ch: char; w: word;
          Ctrl, Shift, Alt: boolean;
    begin
      Ctrl := Pos('Ctrl', s) + Pos('Strg', s) > 0;
      Shift:= Pos('Shift', s) + Pos('Umsch', s) > 0;
      Alt  := Pos('Alt', s) > 0;
      Ch:= s[length(s)];
      w:= VkKeyScan(ch);
      if Shift then keybd_event(vk_shift, 0,0,0);
      if Ctrl  then keybd_event(vk_control, 0,0,0);
      if Alt   then keybd_event(vk_menu, 0,0,0);
      keybd_event(w, 0, 0, 0);
      keybd_event(w, 0, KEYEVENTF_KEYUP, 0);
      if Shift then keybd_event(vk_shift, 0,KEYEVENTF_KEYUP,0);
      if Ctrl  then keybd_event(vk_control, 0,KEYEVENTF_KEYUP,0);
      if Alt   then keybd_event(vk_menu, 0,KEYEVENTF_KEYUP,0);
    end;

    procedure SendOneCharGlobal(ch: char);
      var w: word;
    begin
      w:= VkKeyScan(ch);
      keybd_event(w, 0, 0, 0);
      keybd_event(w, 0, KEYEVENTF_KEYUP, 0);
    end;

  begin
    while length(s) > 0 do begin
      p:= Pos('<', s);
      if p = 1 then begin
        p:= Pos('>', s);
        s1:= copy(s, 2, p-2);
        SendOneShortCutGlobal(s1);
        delete(s, 1, p);
        end
      else begin
        SendOneCharGlobal(s[1]);
        delete(s, 1, 1);
      end;
    end;
  end;

procedure ExpandPath(const s: string);
begin
  var path:= GetEnvironmentVariable('PATH');
  if pos(s, path) = 0 then
    SetEnvironmentVariable('PATH', PChar(s + path));
end;

function GetEnvironmentVar(const VarName: string): string;
begin
  var BufSize:= GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then begin
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
  end else
    Result := '';
end;

procedure SetEnvironmentVar(const VarName, VarValue: string);
begin
  SetEnvironmentVariable(PChar(VarName), PChar(VarValue));
end;

function hasDefaultPrinter: boolean;
var
  ResStr: array[0..255] of Char;
  DefaultPrinter: string;
begin
  GetProfileString('Windows', 'device', '', ResStr, 255);
  DefaultPrinter:= StrPas(ResStr);
  Result:= (DefaultPrinter <> '');
end;

procedure ScrollEndListBox(LB: TListBox);
  var MaxVisible, MaxLength, i, w: integer;
begin
  MaxVisible:= LB.Height div LB.ItemHeight;
  if LB.Items.Count > MaxVisible then
    LB.TopIndex:= LB.Items.Count - MaxVisible + 1;
  LB.Update;
  MaxLength:= LB.Width;
  LB.Canvas.Font.Assign(LB.Font);
  for i:= 0 to LB.Items.Count -1 do begin
    w:= LB.Canvas.TextWidth(LB.Items[i]) + 50;
    if  w > MaxLength then
      MaxLength:= w;
  end;
  SendMessage(LB.Handle, LB_SetHorizontalExtent, MaxLength, 0);
end;

var FLockFormUpdatePile : integer;

procedure LockFormUpdate(F: TForm);
begin
  if assigned(F) then begin
    if FLockFormUpdatePile = 0 then
      F.Perform(WM_SetRedraw, 0, 0);
    inc(FLockFormUpdatePile);
  end;
end;

procedure UnlockFormUpdate(F: TForm);
begin
  if assigned(F) then begin
    dec(FLockFormUpdatePile);
    if FLockFormUpdatePile = 0 then begin
      F.Perform(WM_SetRedraw, 1, 0);
      RedrawWindow(F.Handle, nil, 0, RDW_FRAME + RDW_INVALIDATE +
        RDW_ALLCHILDREN + RDW_NOINTERNALPAINT);
    end;
  end;
end;

procedure StrResetLength(var s: string);
begin
  for var i:= 0 to Length(s) - 1 do
    if s[i + 1] = #0 then begin
      SetLength(s, i);
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

procedure SetAnimation(Value: boolean);
  var Info: TAnimationInfo;
begin
  Info.cbSize:= SizeOf(TAnimationInfo);
  Bool(Info.iMinAnimate):= Value;
  SystemParametersinfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

function GetAnimation: boolean;
  var Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0)
    then Result := Info.iMinAnimate <> 0
    else Result := false;
end;

const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID  = ($00000020);
  DOMAIN_ALIAS_RID_ADMINS           = ($00000220);

function IsGroupMember(RelativeGroupID: DWORD): Boolean;
var
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  I: Integer;
const
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
begin
  Result := not (Win32Platform = VER_PLATFORM_WIN32_NT);
  if Result then // Win9x and ME don't have user groups
    Exit;
  psidAdmin := nil;
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
        psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, @Count) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
         RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, @Count));
      {$ELSE FPC}
      {$WARNINGS OFF}
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, RelativeGroupID, 0, 0, 0, 0, 0, 0,
        psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
         RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count));
      {$WARNINGS ON}
      {$ENDIF FPC}
      for I := 0 to TokenInfo^.GroupCount - 1 do
      begin
        {$RANGECHECKS OFF} // Groups is an array [0..0] of TSIDAndAttributes, ignore ERangeError
        Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid);
        if Result then
        begin
          //consider denied ACE with Administrator SID
          Result := TokenInfo^.Groups[I].Attributes and SE_GROUP_USE_FOR_DENY_ONLY
              <> SE_GROUP_USE_FOR_DENY_ONLY;
          Break;
        end;
        {$IFDEF RANGECHECKS_ON}
        {$RANGECHECKS ON}
        {$ENDIF RANGECHECKS_ON}
      end;
    end;
  finally
    if TokenInfo <> nil then
      FreeMem(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;

procedure SetPrinterIndex(i: integer);
  var Device, Driver, Port: array[0..79] of char;
      Dest: PChar; DeviceMode: THandle;
begin
  try
    Printer.PrinterIndex := i;
    // TSetupPrinter only seems to look to the DeviceMode.dmDeviceName field
    // when setting up the initial printer, hence we make sure this field
    // contains the correct value.
    // This is a bug fix around the error in the delphi library,
    // as it does not use the DevNames structure correctly
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    if DeviceMode <> 0 then begin
      Dest:= GlobalLock(DeviceMode);
      StrCopy(Dest, Device);
      GlobalUnlock(DeviceMode);
    end;
  except
  end;
end;

procedure PrintBitmap(FormBitmap: TBitmap; PixelsPerInch: integer; PrintScale: TPrintScale = poProportional);
var
  InfoSize: DWORD;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;
  PrintWidth, PrintHeight: Longint;
  Info: PBitmapInfo;
  Image: Pointer;
begin
  // analogous to TCustomForm.Print
  Printer.BeginDoc;
  try
    FormBitmap.Canvas.Lock;
    try
      with Printer do begin
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
                PrintWidth := MulDiv(DIBWidth, GetDeviceCaps(Handle,
                  LOGPIXELSX), PixelsPerInch);
                PrintHeight := MulDiv(DIBHeight, GetDeviceCaps(Handle,
                  LOGPIXELSY), PixelsPerInch);
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
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;

function GetHomePath: string;
const
  CSIDL_APPDATA = $001A;
var
  LStr: array[0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, @LStr) = S_OK then
    Result := LStr;
end;

function dissolveUsername(const s: string): string;
  var p: integer; Username: string;
begin
  Result:= s;
  p:= Pos('%USERNAME%', Uppercase(Result));
  if p > 0 then begin
    Delete(Result, p, 10);
    Username:= GetLocalUsername;
    Insert(Username, Result, p);
  end;
end;

function GetComputerNetName: string;
begin
  Result:= GetEnvironmentVariable('COMPUTERNAME');
end;

procedure LockWindow(Handle: THandle);
begin
  LockWindowUpdate(Handle);
end;

procedure UnlockWindow;
begin
  LockWindowUpdate(0);
end;

function HttpToWeb(s: string): string;
begin
  var p:= pos('\', s);
  while p > 0 do begin
    s[p]:= '/';
    p:= pos('\', s);
  end;
  Result:= s;
end;

function CodiereLeerzeichen(s: string): string;
begin
  for var i:= Length(s) downto 1 do
    if s[i] = ' ' then begin
      delete(s, i, 1);
      insert('%20', s, i);
    end;
  Result:= s;
end;

function toWeb(const Browser: string; s: string): string;
  var p: integer;
      IsUNC, IsDrive: boolean;
begin
  IsUNC:= (Copy(s, 1, 2) = '\\');
  IsDrive:= (Copy(s, 2, 1) = ':');
  s:= CodiereLeerzeichen(s);
  p:= pos('\', s);
  while p > 0 do begin
    s[p]:= '/';
    p:= pos('\', s);
  end;
  if IsUNC then
    if Pos('OPERA', Uppercase(Browser)) > 0
      then s:= 'file:' + s
      else s:= 'file://localhost/' + s;
  if IsDrive then
    s:= 'file:///' + s;
  Result:= s;
end;

function ToWindows(s: string): string;
begin
  var p:= pos('/', s);
  while p > 0 do begin
    s[p]:= '\';
    p:= pos('/', s);
  end;
  Result:= s;
end;

function ToRepository(const s: string): string;
begin
  Result:= s;
  var p:= pos(':', Result);
  if p = 2 then
    delete(Result, 2, 1);
end;

function WindowStateToStr(W: TWindowState): string;
begin
  Result:= IntToStr(Ord(W));
end;

function StrToWindowState(const s: string): TWindowState;
begin
  if s = '0' then Result:= wsNormal else
  if s = '1' then Result:= wsMinimized else
                  Result:= wsMaximized;
end;

function UpperLower(const s: string): string;
begin
  Result:= UpperCase(Copy(s, 1, 1)) + Copy(s, 2, length(s));
end;

function LowerUpper(const s: string): string;
begin
  Result:= LowerCase(Copy(s, 1, 1)) + Copy(s, 2, length(s));
end;

{$WARNINGS OFF}
function withTrailingSlash(const s: string): string;
begin
  Result:= withoutTrailingSlash(s);
  if IsHttp(Result)
    then Result:= Result + '/'
    else Result:= IncludeTrailingPathDelimiter(Result);
end;
{$WARNINGS ON}

function withoutTrailingSlash(const s: string): string;
begin
  Result:= s;
  if (copy(Result, length(Result), 1) = '/') or (copy(Result, length(Result), 1) = '\') then
    SetLength(Result, Length(Result)-1);
end;

function GlobalMinimizeName(Filename: string; Canvas: TCanvas; MaxLen: integer): string;
  var l, p: integer; Server, test: string;
begin
  if IsHttp(Filename) then begin
    l:= Canvas.TextWidth(Filename);
    if l > MaxLen then begin
      delete(Filename, 1, 7);
      p:= Pos('/', Filename);
      if p = 0 then p:= length(Filename) + 1;
      Server:= copy(Filename, 1, p-1);
      delete(Filename, 1, p-1);
      repeat
        p:= Pos('.', Server);
        if p = 0
          then Server:= ''
          else Server:= copy(Server, p+1, Length(Server));
        test:= 'https://...' + Server + Filename;
        l:= Canvas.TextWidth(test);
        if l < MaxLen then begin
          Result:= test;
          exit;
        end;
      until Server = '';
      repeat
        p:= Pos('/', Filename);
        if p > 0
          then Filename:= copy(Filename, p+1, Length(Filename));
        test:= 'https://...' + Filename;
        l:= Canvas.TextWidth(test);
        if l < MaxLen then begin
          Result:= test;
          exit;
        end;
      until p = 0;
      Result:= 'http://.../' + Filename;
    end else
      Result:= Filename;
  end else
    Result:= MinimizeName(Filename, Canvas, MaxLen);
end;

procedure ComboBoxAdd(ComboBox: TComboBox);
begin
  var s:= ComboBox.Text;
  ComboBox.Hint:= s;
  if s = '' then exit;
  var i:= ComboBox.Items.IndexOf(s);
  if i >= 0 then
    ComboBox.Items.Delete(i);
  ComboBox.Items.Insert(0, s);
  if ComboBox.Items.Count > 7 then
    ComboBox.Items.Delete(7);
  ComboBox.ItemIndex:= 0;
end;

procedure ComboBoxDelete2(ComboBox: TComboBox; const s: string);
begin
  var i:= ComboBox.Items.IndexOf(s);
  if i >= 0 then
    ComboBox.Items.Delete(i);
end;

procedure ComboBoxInsert(ComboBox: TComboBox);
begin
  var s:= Trim(ComboBox.Text);
  if (s <> '') and (ComboBox.Items.IndexOf(s) = -1) then
    ComboBox.Items.Add(s);
end;

procedure ComboBoxInsert2(ComboBox: TComboBox; const s: string);
begin
  if (s <> '') and (ComboBox.Items.IndexOf(s) = -1) then
    ComboBox.Items.Add(s);
end;

function SaveComboBoxItems(s: string): string;
begin
  var p:= Pos(#13#10, s);
  while p > 0 do begin
    delete(s, p, 2);
    insert(';', s, p);
    p:= Pos(#13#10, s);
  end;
  Result:= s;
end;

function LoadComboBoxItems(s: string): string;
begin
  var p:= Pos(';', s);
  while p > 0 do begin
    delete(s, p, 1);
    insert(#13#10, s, p);
    p:= Pos(';', s);
  end;
  Result:= s;
end;

function HideCrLf(const s: string): string;
begin
  Result:= ReplaceStr(s, #13#10, '\r\n');
end;

function UnhideCrLf(const s: string): string;
begin
  Result:= ReplaceStr(s, '\r\n', #13#10);
end;

function getServer(s: string): string;
begin
  Result:= '';
  if IsHttp(s) then begin
    s:= StripHttp(s);
    var p:= Pos('/', s);
    if p = 0
      then Result:= s + '/'
      else Result:= copy(s, 1, p);
  end;    
end;

procedure StreamWriteln(FS: TFileStream; const s: string);
begin
  if assigned(FS) then begin
    var us:= UTF8String(s + #13#10);
    FS.write(Pointer(us)^, length(us));
  end;
end;

procedure StreamWritelnANSI(FS: TFileStream; const s: string);
  // DOS Codepage = 850
  // Windows Codepage = 1252
  type DOSString = type AnsiString(850);
  var ds: DOSString;
begin
  if assigned(FS) then begin
    ds:= DOSString(s + #13#10);
    FS.write(Pointer(ds)^, length(ds));
  end;
end;

// needs URLMon
function DownloadFile(const SourceFile, DestFile: string): Boolean;
begin
  try
    Result:= UrlDownloadToFile(nil, PChar(SourceFile), PChar(DestFile), 0, nil) = 0;
  except
    Result:= False;
  end;
end;

// needs WinInet
function DownloadURL(const aUrl, aFile: string): boolean;
var
  hSession: HINTERNET;
  hService: HINTERNET;
  lpBuffer: array[0..1024 + 1] of Char;
  dwBytesRead: DWORD;
  fstream: TFileStream;
begin
  Result:= false;
  try
    fstream:= TFileStream.Create(aFile, fmCreate or fmShareExclusive);
    hSession:= InternetOpen('Java-Editor', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    try
      if Assigned(hSession) then begin    // no chache usage
        hService := InternetOpenUrl(hSession, PChar(aUrl), nil, 0, INTERNET_FLAG_RELOAD, 0);
        if Assigned(hService) then
          try
            while true do begin
              dwBytesRead := 1024;
              InternetReadFile(hService, @lpBuffer, 1024, dwBytesRead);
              if dwBytesRead = 0 then break;
              Application.ProcessMessages;
              fstream.Write(lpBuffer[0], dwBytesRead);
            end;
            Result := True;
          finally
            InternetCloseHandle(hService);
          end;
      end;
    finally
      InternetCloseHandle(hSession);
      FreeAndNil(fstream);
    end;
  except
    on e: Exception do
      ErrorMsg(e.Message);
  end;
end;

procedure DisableMI(MenuItem: TSpTBXItem);
begin
  if assigned (MenuItem) and MenuItem.Enabled then
    MenuItem.Enabled:= false;
end;

procedure DisableTB(ToolButton: TSpTBXItem);
begin
  if assigned(ToolButton) and ToolButton.Enabled then
    ToolButton.Enabled:= false;
end;

procedure SetEnabledMI(MenuItem: TSpTBXItem; Enabled: boolean);
begin
  if assigned(MenuItem) and (MenuItem.Enabled <> Enabled) then
    MenuItem.Enabled:= Enabled;
end;

procedure SetEnabledTB(ToolButton: TSpTBXItem; Enabled: boolean);
begin
  if assigned(ToolButton) and (ToolButton.Enabled <> Enabled) then
    ToolButton.Enabled:= Enabled;
end;

procedure SetVisibleMI(MenuItem: TSpTBXItem; Visible: boolean);
begin
  if assigned(MenuItem) and (MenuItem.Visible <> Visible) then
    MenuItem.Visible:= Visible;
end;

function CountChar(c: char; const s: string): integer;
begin
  Result:= 0;
  for var i:= 1 to length(s) do
    if s[i] = c then inc(Result);
end;

procedure ErrorMsg(const s: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      MessageDlg(s, mtError, [mbOk], 0);
    end);
end;

procedure InformationMsg(const s: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      MessageDlg(s, mtInformation, [mbOk], 0);
    end);
end;

function GetShortType(s: string): string;
begin
  var p:= Pos('$', s);
  if p > 0 then delete(s, 1, p);
  delete(s, 1, LastDelimiter('.', s));
  p:= Pos('<', s);
  if p > 1 then
    s:= copy(s, 1, p-1);
  Result:= s;
end;

function GetShortMethod(s: string): string;
  var p: integer; s1: string;
begin
  p:= Pos('throws ', s);
  if p > 0 then s:= trim(copy(s, 1, p-1));
  p:= Pos('(', s);
  s1:= getShortTypeWith(Left(s, p-1)) + '(';
  s:= copy(s, p+1, Length(s) - p -1) + ',';
  p:= Pos(',', s);
  while p > 0 do begin
    s1:= s1 + GetShortTypeWith(Left(s, p-1)) + ', ';
    delete(s, 1, p);
    p:= Pos(',', s);
  end;
  Result:= Left(s1, -2) + ')';
end;

function GetShortTypeWith(s: string): string;
begin
  var p:= Pos('$', s);
  if p > 0 then delete(s, 1, p);
  delete(s, 1, LastDelimiter('.', s));
  Result:= s;
end;
        
const
  simpleTypePascal: array[1..8] of string =
         ('integer', 'double', 'boolean', 'char', 'real', 'word', 'byte', 'shortint');

  simpleTypeJava: array[1..8] of string =
         ('int', 'double', 'boolean', 'char', 'float', 'long', 'byte', 'short');

function IsSimpleType(const s: string): boolean;
  var i: integer;
begin
  Result:= true;
  for i:= 1 to 8 do
    if simpleTypePascal[i] = s then exit;
  for i:= 1 to 8 do
    if simpletypeJava[i] = s then exit;
  Result:= false;
end;

function IsSimpleTypeOrString(const s: string): boolean;
begin                                                                         // pascal
  Result:= IsSimpleType(s) or (s = 'String') or (s = 'java.lang.String') or (Lowercase(s) = 'string');
end;

function WithoutGeneric(s: string): string;
  var p1, p2: integer;
begin
  p1:= Pos('<', s);
  if p1 > 0 then begin
    p2:= length(s);
    while (p2 > 0) and (s[p2] <> '>') do
      dec(p2);
    delete(s, p1, p2-p1+1);
  end;
  if Pos('...', s) > 0
    then s:= copy(s, 1, Pos('...', s) -1);
  Result:= s;
end;

function GenericOf(const s: string): string;
begin
  Result:= '';
  var p:= Pos('<', s);
  if p > 0 then
    Result:= copy(s, p+1, length(s) - p - 1);
end;

function WithoutArray(const s: string): string;
begin
  Result:= s;
  var i:= Pos('[', Result) + Pos('...', Result);
  if i > 0
    then Result:= copy(Result, 1, i-1)
end;

function ChangeGenericType(const s: string): string;
begin
  if Pos('<', s) > 0
    then Result:= copy(s, 1, Pos('<', s)) + 'T>'
    else Result:= s;
end;

function ExtractPackageName(const CName: string): string;
begin
  var i:= LastDelimiter('.', CName);
  if i = 0
    then Result:= ''
    else Result:= Copy(CName, 1, i-1);
end;

function ExtractClassName(const CName: string): string;
begin
  var i:= LastDelimiter('.', CName);
  if i = 0
    then Result:= CName
    else Result:= Copy(CName, i+1, 255);
end;

function toBackSlash(const s: string) : string;
begin
  Result:= S;
  for var i:= 1 to length(s) do
    if (Result[i] = '.') or (Result[i] = '/') then
      Result[i] := '\';
end;

function toHtml(const s: string): string;
begin
  Result:= s;
  var p:= Pos('<', Result);
  if p > 0 then begin
    delete(Result, p, 1);
    insert('&lt;', Result, p);
  end;
  p:= Pos('>', Result);
  if p > 0 then begin
    delete(Result, p, 1);
    insert('&gt;', Result, p);
  end;
end;

function Split(Delimiter: Char; Input: string): TStringList;
begin
  var SL:= TStringList.Create;
  var p:= Pos(Delimiter, Input);
  while p > 0 do begin
    SL.Add(copy(Input, 1, p-1));
    delete(Input, 1, p);
    p:= Pos(Delimiter, Input);
  end;
  SL.Add(Input);
  Result:= SL;  
end;

function left(const s: string; p: integer): string;
begin
  if p >= 0
    then Result:= copy(s, 1, p)
    else Result:= copy(s, 1, length(s)+p);
end;

function right(const s: string; p: integer): string;
begin
  if p >= 0
    then Result:= copy(s, p, length(s))
    else Result:= copy(s, length(s)+p+1, length(s));
end;

function getIndent(const s: string): integer;
begin
  var l:= Length(s);
  var i:= 1;
  while (i <= l) and (s[i] <= ' ') do
    inc(i);
  Result:= i-1;
end;

function isLower(c: char): boolean;
begin
  Result:= CharInSet(c, ['a'..'z']);
end;

function RGB2Color(const R, G, B: Integer): Integer;
begin
  // convert hexa-decimal values to RGB
  Result := R + G shl 8 + B shl 16;
end;

procedure Color2RGB(const Color: TColor; var R, G, B: Integer);
begin
  // convert hexa-decimal values to RGB
  R:= Color and $FF;
  G:= (Color shr 8) and $FF;
  B:= (Color shr 16) and $FF;
end;

function ChangeColor(Color: TColor; percent: real): TColor;
  var r, g, b: Integer;
begin
  r:= 0; g:= 0; b:= 0;
  Color2RGB(Color, r, g, b);
  r:= round(r * percent); if r > 255 then r:= 255;
  g:= round(g * percent); if g > 255 then g:= 255;
  b:= round(b * percent); if b > 255 then b:= 255;
  Result:= RGB2Color(r, g, b);
end;

function IsWordBreakChar(C: Char): Boolean;  // fromSyneditSearch
begin
  case C of
    #0..#32, '.', ',', ';', ':', '"', '''', '´', '`', '°', '^', '!', '?', '&',
    '$', '@', '§', '%', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>',
    '-', '=', '+', '*', '/', '\', '|':
      Result := True;
    else
      Result := False;
  end;
end;

function IsAlpha(ch: Char): boolean;
begin
  Result:= ch.isLetter or (ch = '_');
end;

function IsWordInLine(Word, Line: string): boolean;
  var p, q: integer;
begin
  Result:= false;
  p:= Pos(Word, Line);
  if p > 0 then begin
    q:= p + length(Word);
    Result:= ((p = 1) or (p > 1) and IsWordBreakChar(Line[p-1])) and
             ((q > length(Line)) or IsWordBreakChar(Line[q]));
  end;
end;

function IsDigit(C: Char): Boolean;
begin
  Result:= ('0' <= C) and (C <= '9');
end;

function getFirstWord(s: string): string;
begin
  s:= trim(s);
  var i:= 1;
  while (i <= length(s)) and not IsWordBreakChar(s[i]) do
    inc(i);
  Result:= copy(s, 1, i-1);
end;

function GetNextPart(var s: string; ch: char): string;
  var q, p, bracket: integer;
begin
  p:= Pos(ch, s);
  if p = 0 then p:= length(s)+1;
  q:= Pos('<', s); // generic part
  if (0 < q) and (q < p) then begin
    bracket:= 1;
    inc(q);
    while (q <= length(s)) and (bracket > 0) do begin
      if s[q] = '<' then inc(bracket) else
      if s[q] = '>' then dec(bracket);
      inc(q);
    end;
    while (q <= length(s)) and (s[q] <> ch) do
      inc(q);
    p:= q;
  end;
  Result:= copy(s, 1, p-1);
  delete(s, 1, p);
end;

function GetNextPart(var s: string): string;
begin
  Result:= GetNextPart(s, ' ');
end;

function withoutThrows(const s: string): string;
begin
  Result:= s;
  var p:= Pos(' throws', s);
  if p > 0 then
    Result:= copy(s, 1, p-1);
end;

function IsJavaString(const s: string): boolean;
begin
  Result:= (length(s) >= 2) and (Left(s, 1) = '"') and (Right(s, -1) = '"');
end;

function IsJavaChar(const s: string): boolean;
begin
  Result:= (length(s) >= 2) and (Left(s, 1) = '''') and (Right(s, -1) = '''');
end;

procedure AddStrings(Dest, Source: TStringList);
begin
  if assigned(Dest) and assigned(Source) then
    for var i:= 0 to Source.count - 1 do
      if Dest.IndexOf(Source.Strings[i]) = -1 then
        Dest.Add(Source.Strings[i]);
end;


function FileIs32Bit(const Filename: string): boolean;
  const IMAGE_FILE_32BIT_MACHINE = $0100;
  var f: TFileStream; PEOffset, PESignature, Characteristics: integer;
begin
  Result:= false;
  PEOffset:= 0;
  PESignature:= 0;
  Characteristics:= 0;
  f:= TFileStream.Create(Filename, fmShareDenyNone);
  try
    f.seek($3C, soFromBeginning);
    f.read(PEOffset, 4);
    if PEOffset > $3C then begin
      f.seek(PEOffset, soFromBeginning);
      f.read(PESignature, 4);
      if PESignature = $00004550 then begin
        f.seek(18, soFromCurrent);
        f.read(Characteristics, 2);
        Result:= (Characteristics AND IMAGE_FILE_32BIT_MACHINE = IMAGE_FILE_32BIT_MACHINE);
      end;
    end;
  finally
    FreeAndNil(f);
  end;
end;

function encodeQuotationMark(const s: string): string;
begin
  Result:= ReplaceStr(s, '"', '&quot;');
end;

procedure CreateMyFile(const Path: string);
begin
  if (Path <> '') and not FileExists(Path) then begin
    var SL:= TStringList.Create;
    try
      try
        Sysutils.ForceDirectories(ExtractFilePath(Path));
        SL.SaveToFile(Path);
      except on e: Exception do
         Errormsg(e.Message);
      end;
    finally
      FreeAndNil(SL);
    end;
  end;
end;

function changeVowels(const s: string): string;
  const Vowels = 'ÄÖÜäöüß';
  const Nowels = 'AOUaous';
  var i, p: integer;
begin
  Result:= s;
  for i:= 1 to Length(Vowels) do begin
    p:= Pos(Vowels[i], Result);
    while p > 0 do begin
      Result[p]:= Nowels[i];
      if i < 7
        then insert('e', Result, p+1)
        else insert('s', Result, p+1);
      p:= Pos(Vowels[i], Result);
    end;
  end;
end;

function OnlyCharsAndDigits(const s: string): string;
begin
  Result:= '';
  for var i:= 1 to length(s) do begin
    if s[i].IsLetterOrDigit then
      Result:= Result + s[i];
  end;
end;

function WithoutSpaces(const s: string): string;
begin
  Result:= ReplaceStr(s, ' ', '');
end;

function VisibilityAsString(vis: TVisibility) : string;
begin
  case vis of
    viPrivate:   Result:= 'private';
    viPackage:   Result:= '';
    viProtected: Result:= 'protected';
    viPublic:    Result:= 'public';
    viPublished: Result:= 'published';
  else
    Result:= '';
  end;
end;

function String2Visibility(const s: string): TVisibility;
begin
  if s = 'private'   then Result:= viPrivate else
  if s = 'protected' then Result:= viProtected else
  if s = 'public'    then Result:= viPublic else
  if s = 'published' then Result:= viPublished else
                          Result:= viPackage;
end;

function Visibility2ImageNumber(vis: TVisibility): integer;
begin
  case vis of
    viPrivate:   Result:= 7;
    viProtected: Result:= 8;
    viPackage:   Result:= 9;
    viPublic:    Result:= 10;
  else           Result:= 0;
  end;
end;

function IsVisibility(const s: string): boolean;
  const visibilities: array[1..4] of string =
         ('private', 'protected', 'public', '');
  var i: integer;
begin
  Result:= false;
  for i:= 1 to 4 do
    if s = visibilities[i] then begin
      Result:= true;
      exit;
    end;
end;

function IsModifier(const s: string): boolean;
  const Modifiers : array[1..8] of string =
    ('static', 'abstract', 'final', 'native', 'synchronized', 'transient',
      'volatile', 'strictfp');
begin
  Result:= false;
  for var i:= 1 to 8 do
    if s = Modifiers[i] then begin
      Result:= true;
      exit;
    end;
end;

function getFilterIndex(Filter: string; const Filename: string): integer;
  var Ext: string; p: integer;
begin
  Result:= 0;
  Ext:= lowercase(ExtractFileExt(Filename));
  if Ext = '' then exit;
  repeat
    p:= Pos('|*', Filter);
    delete(Filter, 1, p+1);
    Result:= Result + 1;
    if Pos(Ext, Filter) = 1 then
      exit;
    p:= Pos('|', Filter);
    delete(Filter, 1, p);
  until Filter = '';
  Result:= 0;
end;

function GetLongPathName(const PathName: string): string;
var
  Drive: string;
  Path: string;
  SearchRec: TSearchRec;
begin
  if PathName = '' then
    Exit;
  Drive := ExtractFileDrive(PathName);
  Path := Copy(PathName, Length(Drive) + 1, Length(PathName));
  if (Path = '') or (Path = '\') then
  begin
    Result := PathName;
    if Result[Length(Result)] = '\' then
      Delete(Result, Length(Result), 1);
  end else begin
    Path := GetLongPathName(ExtractFileDir(PathName));
    if FindFirst(PathName, faAnyFile, SearchRec) = 0 then begin
      {$WARNINGS OFF}
      Result := Path + '\' + SearchRec.FindData.cFileName;
      {$WARNINGS ON}
      SysUtils.FindClose(SearchRec);
    end else
      Result := Path + '\' + ExtractFileName(PathName);
  end;
end;

function EndsWith(const Str, Substr: string): boolean;
begin
  var SubTextLocation := Length(Str) - Length(Substr) + 1;
  if (SubTextLocation > 0) and (Str <> '')
    then Result:= CompareStr(copy(Str, SubTextLocation, Length(Str)), SubStr) = 0
    else Result:= False;
end;

function StartsWith(const Str, Substr: string): boolean;
begin
  if Str <> ''
    then Result:= CompareStr(copy(Str, 1, Length(SubStr)), SubStr) = 0
    else Result:= False;
end;

function StartsWithInsensitive(const Str, Substr: string): boolean;
begin
  if Str <> ''
    then Result:= CompareText(copy(Str, 1, Length(SubStr)), SubStr) = 0
    else Result:= False;
end;

function FileExistsCaseSensitive(const Filename: TFileName): Boolean;
  var SR: TSearchRec;
begin
  Result:= FindFirst(Filename, faAnyFile, SR) = 0;
  if Result then Sysutils.FindClose(SR);
  Result:= Result and (SR.Attr and faDirectory = 0) and (SR.Name = ExtractFileName(Filename));
end;

function IsMouseOverControl(const ctrl: TControl): boolean;
var
  sr: TRect;
  p1, p2: TPoint;
begin
  p1:= ctrl.ClientToScreen(Point(0, 0));
  p2:= ctrl.ClientToScreen(Point(ctrl.Width, ctrl.Height));
  sr:= Rect(p1.x, p1.y, p2.x, p2.y);
  Result:= PtInRect(sr, Mouse.CursorPos);
end;

function sametext(const x, y: string): boolean;
  // not case sensitive
begin
  Result:= comparetext(x, y) = 0;
end;

function AddWithSpace(s: string): string;
begin
  s:= trim(s);
  if s = ''
    then Result:= ''
    else Result:= ' ' + s;
end;

function max3(m1, m2, m3: integer): integer;
begin
  Result:= max(m1, max(m2, m3));
end;

function min3(m1, m2, m3: integer): integer;
begin
  Result:= min(m1, min(m2, m3));
end;

function getProtocol(url: string): string;
begin
  var p:= Pos('://', url);
  if p > 0
    then Result:= copy(url, 1, p + 2)
    else Result:= '';
end;

function getProtocolAndDomain(url: string): string;
begin
  var protocol:= getProtocol(url);
  delete(url, 1, length(protocol));
  var p:= Pos('/', url);
  if p > 0
    then Result:= protocol + copy(url, 1, p-1)
    else Result:= protocol + url;
end;

function _KillTask(const APID: Cardinal;
                   const AKillStructure: Boolean = False): Boolean;
var
   P:        TProcessEntry32;
   H:        THandle;
   D:        DWORD;
   TP:       TTokenPrivileges;
   hToken:   THandle;
   hProcess: Cardinal;
   Goon:     Boolean;

   function IsWinNT: Boolean;
     var VI: TOSVersionInfo;
   begin
     VI.dwOSVersionInfoSize:= SizeOf(VI);
     Result:= False;
     if GetVersionEx(VI) then
        Result:= VI.dwPlatformId = VER_PLATFORM_WIN32_NT;
   end;

begin
   Result   := False;
   P.dwSize := SizeOf(P);
   H        := CreateToolHelp32Snapshot(TH32CS_SnapProcess, 0);
   Goon     := True;
   try
      if Process32First(H, P) then begin
         Result := True;
         repeat
            if P.th32ProcessID = APID then begin
               if IsWinNT then begin
                  if OpenProcessToken(GetCurrentProcess,
                                      TOKEN_ADJUST_PRIVILEGES, hToken) then begin
                     LookupPrivilegeValue(nil, 'SeDebugPrivilege',
                                          TP.Privileges[0].Luid);
                     TP.PrivilegeCount           := 1;
                     TP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
                     D                           := 0;
                     AdjustTokenPrivileges(hToken, False, TP, 0,
                                           PTokenPrivileges(nil)^, D);
                     CloseHandle(hToken);
                  end;
               end;
               hProcess := OpenProcess(Process_Terminate, False,
                                       P.th32ProcessID);
               Goon     := False;
               if hProcess <> 0 then
                  try
                     Result:= Windows.TerminateProcess(hProcess, 0) and Result;
                     if WaitForSingleObject(hProcess, INFINITE) = WAIT_OBJECT_0 then
                        Result := False;
                  finally
                     CloseHandle(hProcess);
                  end;
            end else if (AKillStructure) and (P.th32ParentProcessID = APID) then
               Result := _KillTask(P.th32ProcessID, True) and Result;
         until (not Process32Next(H, P)) or ((not Goon) and (not AKillStructure));
      end;
   finally
      CloseHandle(H);
   end;
end;

function IsRunning(Process: THandle): Boolean;
var
  C: LongWord;
begin
  Result:= (Process <> 0) and GetExitCodeProcess(Process, C) and (C = STILL_ACTIVE);
end;

procedure CloseProcessinformationHandle(var ProcessInformation: TProcessinformation);
begin
  if Processinformation.hProcess <> 0 then
    CloseHandle(ProcessInformation.hProcess);
  if Processinformation.hThread <> 0 then
    CloseHandle(ProcessInformation.hThread);
  FillChar(Processinformation, SizeOf(Processinformation), #0);
end;

procedure TerminateTask(var Processinformation: TProcessinformation);
begin
  if IsRunning(ProcessInformation.hProcess) then
    _KillTask(Processinformation.dwProcessID, true);
  CloseProcessinformationHandle(ProcessInformation);
end;

procedure ClosePipeHandles(var Pipe: TPipeHandles);
begin
  with Pipe do begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead:= 0;
    hWrite:= 0;
  end;
end;

function LeftSpaces(s: string; tabW: integer): Integer;
begin
  var p:= PWideChar(s);
  if Assigned(p) then begin
    Result:= 0;
    while (p^ >= #1) and (p^ <= #32) do begin
      if p^ = #9
        then Inc(Result, TabW)
        else Inc(Result);
      Inc(p);
    end;
  end else
    Result := 0;
end;

function ConvertLtGt(s: string): string;
begin
  Result:= ReplaceStr(ReplaceStr(s, '<', '&lt;'), '>', '&gt;');
end;

function IntToVal(x: integer): string;
begin
  Result:= '"' + IntToStr(x) + '"';
end;

function FloatToVal(x: real): string;
begin
  Result:= '"' + FloatToStr(x) + '"';
  var p:= Pos(',', Result);
  if p > 0 then
    Result[p]:= '.';
end;

function myColorToRGB(Color: TColor): string;
  var ColorInt, r, g, b: integer;
begin
  ColorInt:= ColorToRGB(Color);
  r:= GetRValue(ColorInt);
  g:= GetGValue(ColorInt);
  b:= GetBValue(ColorInt);
  Result:= 'rgb(' + IntToStr(r) + ',' + IntToStr(g) + ',' + IntToStr(b) + ')';
end;

function PointToVal(P: TPoint): string;
begin
  Result:= IntToStr(P.x) + ',' + IntToStr(P.y) + ' ';
end;

function XYToVal(x, y: integer): string;
begin
  Result:= IntToStr(x) + ',' + IntToStr(y) + ' ';
end;

function CanActuallyFocus(WinControl: TWinControl): Boolean;
var
  Form: TCustomForm;
begin
  Result:= False;
  if Assigned(WinControl) and not WinControl.Focused then begin
    Form:= GetParentForm(WinControl);
    if Assigned(Form) and Form.Enabled and Form.Visible then
      Result:= WinControl.CanFocus;
  end;
end;

procedure SetDefaultUIFont(const AFont: TFont);
const
  UIFont = 'Segoe UI';
begin
  if CheckWin32Version(6)
    and not SameText(AFont.Name, UIFont)
    and (Screen.Fonts.IndexOf(UIFont) >= 0) then
  begin
    AFont.Size := 9;
    AFont.Name := UIFont;
  end;
end;

procedure Log(S: string);
begin
  var SL:= TStringList.Create;
  SL.LoadFromFile('C:\temp\log.txt');
  SL.Add(s);
  SL.SaveToFile('C:\temp\log.txt');
  FreeAndNil(SL);
end;

function CompiledRegEx(Expr : string; Options: TRegExOptions = [roNotEmpty];
  UCP : Boolean = True): TRegEx;
begin
  try
    Result.Create(Expr, Options);
    if UCP then
      Result.AddRawOptions(PCRE_UCP);
    Result.Study([preJIT])
  except
    on E: ERegularExpressionError do
      begin
        MessageDlg(Format('Invalid Regular Expression: %s', [E.Message]),
          mtError, [mbOK], 0);
        Abort;
      end
    else
      raise;
  end;
end;

function myMulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  Result:= MulDiv(nNumber, nNumerator, nDenominator);
end;

function StringTimesN(s: string; n: integer): string;
  var i: integer;
begin
  Result:= '';
  for i:= 1 to n do
    Result:= Result + s;
end;

{ TMatchHelper }

function TMatchHelper.GroupIndex(Index: integer): integer;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Index
  else
    Result := -1;
end;

function TMatchHelper.GroupLength(Index: integer): integer;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Length
  else
    Result := 0;
end;

function TMatchHelper.GroupValue(Index: integer): string;
begin
  if Index < Groups.Count then
    Result := Groups[Index].Value
  else
    Result := '';
end;

{ TFormHelper }

function TControlHelper.PPIScale(ASize: integer): integer;
begin
   Result := MulDiv(ASize, FCurrentPPI, 96);
end;

function TControlHelper.PPIUnScale(ASize: integer): integer;
begin
   Result := MulDiv(ASize, 96, FCurrentPPI);
end;

end.

