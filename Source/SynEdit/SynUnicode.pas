{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynUnicode.pas by Maël Hörz, released 2004-05-30.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

------------------------------------------------------------------------------}

unit SynUnicode;

{$I SynEdit.inc}

interface

uses
  Windows,
  Types,
  Classes,
  SysUtils;

const
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  UTF16BOMLE: array[0..1] of Byte = ($FF, $FE);
  UTF16BOMBE: array[0..1] of Byte = ($FE, $FF);
  UTF32BOMLE: array[0..3] of Byte = ($FF, $FE, $00, $00);
  UTF32BOMBE: array[0..3] of Byte = ($00, $00, $FE, $FF);

const
  // constants describing range of the Unicode Private Use Area (Unicode 3.2)
  PrivateUseLow = WideChar($E000);
  PrivateUseHigh = WideChar($F8FF);
  // filler char: helper for painting wide glyphs
  FillerChar = PrivateUseLow;

const
  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar(#10);
  WideLineFeed = WideChar(#10);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideCR = WideChar(#13);
  WideCarriageReturn = WideChar(#13);
  WideCRLF = string(#13#10);
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

type
  TFontCharSet = 0..255;

function SynCharNext(Posi: PWideChar): PWideChar; overload;
function SynCharNext(Posi: PWideChar; out Element: string): PWideChar; overload;
function SynUniElementsCount(Str: string) : Integer;

{ functions taken from JCLUnicode.pas }
procedure StrSwapByteOrder(Str: PWideChar);
function CharSetFromLocale(Language: LCID): TFontCharSet;
function CodePageFromLocale(Language: LCID): Integer;
function KeyboardCodePage: Word;

{ functions providing same behavior on Win9x and WinNT based systems}
function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;

{ Unicode streaming-support }
type
  TSynEncoding = (seUTF8, seUTF16LE, seUTF16BE, seAnsi);
  TSynEncodings = set of TSynEncoding;

function IsAnsiOnly(const WS: string): Boolean;
function IsUTF8(Stream: TStream; out WithBOM: Boolean; BytesToCheck: Integer = $4000): Boolean; overload;
function IsUTF8(const FileName: string; out WithBOM: Boolean; BytesToCheck: Integer = $4000): Boolean; overload;
function IsUTF8(const Bytes: TBytes; Start: Integer = 0; BytesToCheck: Integer = $4000): Boolean; overload;
function GetEncoding(const FileName: string; out WithBOM: Boolean): TEncoding; overload;
function GetEncoding(Stream: TStream; out WithBOM: Boolean): TEncoding; overload;

function ClipboardProvidesText: Boolean;
function GetClipboardText: string;
procedure SetClipboardText(const Text: string);

{ misc functions }
function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;

var
  UserLocaleName: array [0..LOCALE_NAME_MAX_LENGTH - 1] of Char;

implementation

uses
  Math,
  Clipbrd;

function SynCharNext(Posi: PWideChar): PWideChar;
begin
  Result := Windows.CharNext(Posi);
end;

function SynCharNext(Posi: PWideChar; out Element: string): PWideChar; overload;
var
  Start : PWideChar;
begin
  Start := Posi;
  Result := Windows.CharNext(Posi);
  SetString(Element, Start, Result - Start);
end;

function SynUniElementsCount(Str: string) : Integer;
var
  Posi : PWideChar;
begin
  Result := 0;
  Posi := PWideChar(Str);
  while Posi^ <> #0 do
  begin
    Posi := Windows.CharNext(Posi);
    Inc(Result);
  end;
end;

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string
procedure StrSwapByteOrder(Str: PWideChar);
var
  Posi: PWord;
begin
  Posi := PWord(Str);
  while Posi^ <> 0 do
  begin
    Posi^ := MakeWord(HiByte(Posi^), LoByte(Posi^));
    Inc(Posi);
  end;
end;

function TranslateCharsetInfoEx(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall;
  external 'gdi32.dll' Name 'TranslateCharsetInfo';

function CharSetFromLocale(Language: LCID): TFontCharSet;
var
  CP: Cardinal;
  CSI: TCharsetInfo;
begin
  CP:= CodePageFromLocale(Language);
  TranslateCharsetInfoEx(Pointer(CP), CSI, TCI_SRCCODEPAGE);
  Result:= CSI.ciCharset;
end;

// determines the code page for a given locale
function CodePageFromLocale(Language: LCID): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

function GetTextSize(DC: HDC; Str: PWideChar; Count: Integer): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;

  begin
    GetTextExtentPoint32W(DC, Str, Count, Result);
  end;
end;

function IsAnsiOnly(const WS: string): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(WS), Length(WS), nil, 0,
    nil, @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

function IsUTF8(const FileName: string; out WithBOM: Boolean; BytesToCheck: Integer): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsUTF8(Stream, WithBOM, BytesToCheck);
  finally
    Stream.Free;
  end;
end;

// checks for a BOM in UTF-8 format or searches the first 4096 bytes for
// typical UTF-8 octet sequences
function IsUTF8(Stream: TStream; out WithBOM: Boolean; BytesToCheck: Integer): Boolean;
var
  Buffer: TBytes;
  BufferSize: Integer;
  BomLen: Integer;
  Encoding: TEncoding;
begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result

  // start analysis at actual Stream.Position
  BufferSize := Min(BytesToCheck, Stream.Size - Stream.Position);

  // if no special characteristics are found it is not UTF-8
  Result := False;
  WithBOM := False;

  if BufferSize > 0 then
  begin
    SetLength(Buffer, BufferSize);
    Stream.Read(Buffer, 0, BufferSize);
    Stream.Seek(-BufferSize, soFromCurrent);

    { first search for BOM }
    Encoding := nil;
    BomLen := TEncoding.GetBufferEncoding(Buffer, Encoding);
    WithBOM := BOMLen > 0;
    if Encoding = TEncoding.UTF8 then
      Exit(True)
    else if WithBom then
      Exit(False);

    { Now check the content for UTF8 sequences }
    Result := IsUtf8(Buffer, 0, BytesToCheck);
  end;
end;

function IsUTF8(const Bytes: TBytes; Start: Integer; BytesToCheck: Integer): Boolean; overload;
const
  MinimumCountOfUTF8Strings = 1;
var
   Len, Int, FoundUTF8Strings: Integer;

  // 3 trailing bytes are the maximum in valid UTF-8 streams,
  // so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    Inc(Int);
    while (Int < Len) and (Result < 4) do
    begin
      if Bytes[Int] in [$80..$BF] then
        Inc(Result)
      else
        Break;
      Inc(Int);
    end;
  end;

begin
   {  NOTE: There is no 100% save way to detect UTF-8 streams. The bigger
            MinimumCountOfUTF8Strings, the lower is the probability of
            a false positive. On the other hand, a big MinimumCountOfUTF8Strings
            makes it unlikely to detect files with only little usage of non
            US-ASCII chars, like usual in European languages. }
    Result := False;
    Len := Min(Start + BytesToCheck, Length(Bytes));
    FoundUTF8Strings := 0;
    Int := Start;
    while Int < Len do
    begin
      case Bytes[Int] of
        $00..$7F: // skip US-ASCII characters as they could belong to various charsets
          ;
        $C2..$DF:
          if CountOfTrailingBytes = 1 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $E0:
          begin
            Inc(Int);
            if (Int < Len) and (Bytes[Int] in [$A0..$BF]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $E1..$EC, $EE..$EF:
          if CountOfTrailingBytes = 2 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $ED:
          begin
            Inc(Int);
            if (Int < Len) and (Bytes[Int] in [$80..$9F]) and (CountOfTrailingBytes = 1) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F0:
          begin
            Inc(Int);
            if (Int < Len) and (Bytes[Int] in [$90..$BF]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $F1..$F3:
          if CountOfTrailingBytes = 3 then
            Inc(FoundUTF8Strings)
          else
            Break;
        $F4:
          begin
            Inc(Int);
            if (Int < Len) and (Bytes[Int] in [$80..$8F]) and (CountOfTrailingBytes = 2) then
              Inc(FoundUTF8Strings)
            else
              Break;
          end;
        $C0, $C1, $F5..$FF: // invalid UTF-8 bytes
          Break;
        $80..$BF: // trailing bytes are consumed when handling leading bytes,
                   // any occurence of "orphaned" trailing bytes is invalid UTF-8
          Break;
      end;

      if FoundUTF8Strings >= MinimumCountOfUTF8Strings then
      begin
        Result := True;
        Break;
      end;

      Inc(Int);
    end;
end;

function GetEncoding(const FileName: string; out WithBOM: Boolean): TEncoding;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetEncoding(Stream, WithBOM);
  finally
    Stream.Free;
  end;
end;

function GetEncoding(Stream: TStream; out WithBOM: Boolean): TEncoding;

  function TBytesEqual(A, B: TBytes; Len: Integer): Boolean;
  var
    Int: Integer;
  begin
    Result := True;
    for Int := 0 to Len - 1 do
      if A[Int] <> B[Int] then Exit(False)
  end;

var
  Buffer: TBytes;
  Size: Integer;
  Preamble: TBytes;
begin
  // if Stream is nil, let Delphi raise the exception, by accessing Stream,
  // to signal an invalid result

  // start analysis at actual Stream.Position
  Size := Stream.Size - Stream.Position;

  // if no special characteristics are found it is probably ANSI
  Result := TEncoding.ANSI;

  if IsUTF8(Stream, WithBOM) then Exit(TEncoding.UTF8);

  { try to detect UTF-16 by finding a BOM in UTF-16 format }

  // Check for Unicode
  Preamble := TEncoding.Unicode.GetPreamble;
  if Size >= Length(Preamble) then
  begin
    Setlength(Buffer, Length(Preamble));
    Stream.Read(Buffer, 0, Length(Preamble));
    Stream.Seek(-Length(Preamble), soFromCurrent);
    if TBytesEqual(Preamble, Buffer, Length(Preamble)) then
    begin
      WithBOM := True;
      Exit(TEncoding.Unicode);
    end;
  end;
  // Check for BigEndianUnicode
  Preamble := TEncoding.BigEndianUnicode.GetPreamble;
  if Size >= Length(Preamble) then
  begin
    Setlength(Buffer, Length(Preamble));
    Stream.Read(Buffer, 0, Length(Preamble));
    Stream.Seek(-Length(Preamble), soFromCurrent);
    if TBytesEqual(Preamble, Buffer, Length(Preamble)) then
    begin
      WithBOM := True;
      Exit(TEncoding.BigEndianUnicode);
    end;
  end;
end;

function ClipboardProvidesText: Boolean;
begin
  Result := IsClipboardFormatAvailable(CF_UNICODETEXT);
end;

function GetClipboardText: string;
begin
  Result := Clipboard.AsText;
end;

procedure SetClipboardText(const Text: string);
begin
  Clipboard.AsText := Text;
end;

function IsWideCharMappableToAnsi(const WC: WideChar): Boolean;
var
  UsedDefaultChar: BOOL;
begin
  WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(@WC), 1, nil, 0, nil,
    @UsedDefaultChar);
  Result := not UsedDefaultChar;
end;

initialization
  Assert(Win32Platform = VER_PLATFORM_WIN32_NT, 'Unsupported Windows version');
  if LCIDToLocaleName(GetUserDefaultLCID, UserLocaleName, LOCALE_NAME_MAX_LENGTH, 0) = 0 then
    RaiseLastOSError;

end.
