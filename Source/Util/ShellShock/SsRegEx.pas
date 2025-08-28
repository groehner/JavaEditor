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
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: SsRegEx.pas 1.02                          *}
{*********************************************************}
{* ShellShock: ShellShock Regular Expression Engine      *}
{*********************************************************}

{$I SsDefine.inc}

unit SsRegEx;

interface

uses
  Classes, SsStrms;

const
  StWordDelimString : string = #9#32'!"&()*+,-./:;<=>?@[\]^`{|}~';
  StHexDigitString  : string = '0123456789ABCDEF';

type
  TMatchPosition = packed record
    StartPos : Cardinal;
    EndPos   : Cardinal;
    Length   : Cardinal;
    LineNum  : Cardinal;
  end;

  TStOutputOption = (ooUnselected, ooModified, ooCountOnly);
  TStOutputOptions = set of TStOutputOption;

  TStTokens = (tknNil, tknLitChar, tknCharClass, tknNegCharClass,
               tknClosure, tknMaybeOne, tknAnyChar, tknBegOfLine,
               tknEndOfLine, tknGroup, tknBegTag, tknEndTag, tknDitto);

  PStPatRecord = ^TStPatRecord;
  TStPatRecord = packed record
    StrPtr        : PString;
    NestedPattern : PStPatRecord;
    NextPattern   : PStPatRecord;
    Token         : TStTokens;
    OneChar       : Char;
    NextOK        : Boolean;
  end;

  TStTagLevel = -1..9;
  TStFlag     = array[0..1023] of TStTagLevel;

  TStOnRegExProgEvent = procedure(Sender : TObject; Percent : Word) of object;
  TStOnMatchEvent = procedure(Sender     : TObject;
                              REPosition : TMatchPosition) of object;


  TStNodeHeap = class
  private
    FFreeList : PStPatRecord;

  protected
    procedure nhClearHeap;
    function nhDeepCloneNode(aNode : PStPatRecord) : PStPatRecord;

  public
    constructor Create;
    destructor Destroy; override;

    function AllocNode : PStPatRecord;
    procedure FreeNode(aNode : PStPatRecord);

    function CloneNode(aNode : PStPatRecord) : PStPatRecord;
  end;


  TStStreamRegEx = class(TObject)
  protected {private}
    FAvoid            : Boolean;
    FIgnoreCase       : Boolean;
    FInTextStream     : TStTextStream;
    FInFileSize       : Cardinal;
    FInputStream      : TStream;

    FInLineBuf        : PChar;
    FInLineCount      : Cardinal;
    FInLineNum        : Cardinal;
    FInLineTermChar   : Char;
    FInLineTerminator : TStLineTerminator;
    FInLineLength     : Integer;
    FLineNumbers      : Boolean;
    FLinesPerSec      : Cardinal;

    FMatchCount       : Cardinal;

    FMatchPatSL       : TStringList;
    FMatchPatStr      : PChar;
    FMatchPatPtr      : PStPatRecord;

    FMaxLineLength    : Cardinal;

    FNodes            : TStNodeHeap;

    FOnMatch          : TStOnMatchEvent;
    FOutLineLength    : Integer;
    FOutLineTermChar  : Char;
    FOutLineTerminator: TStLineTerminator;

    FReplaceCount     : Cardinal;
    FReplacePatSL     : TStringList;
    FReplacePatStr    : PChar;
    FReplacePatPtr    : PStPatRecord;

    FOnProgress       : TStOnRegExProgEvent;
    FOutputStream     : TStream;
    FOutTextStream    : TStTextStream;
    FOutLineBuf       : PChar;

    FOutputOptions    : TStOutputOptions;

    FSelAvoidPatSL    : TStringList;
    FSelAvoidPatStr   : PChar;
    FSelAvoidPatPtr   : PStPatRecord;

    FSelectCount      : Cardinal;

  protected
    { Protected declarations }

    procedure AddTokenToPattern(var PatRec : PStPatRecord;
                                LastPatRec : PStPatRecord;
                                     Token : TStTokens;
                                         Str : string);
    procedure AddTokenToReplace(var PatRec : PStPatRecord;
                                LastPatRec : PStPatRecord;
                                     Token : TStTokens;
                                     Str     : string);
    function  AppendS(Dest, Str1, Str2 : PChar; Count : Cardinal) : PChar;
    function  BuildAllPatterns : Boolean;
    function  BuildPatternStr(var PStr  : PChar;
                              var Len   : Integer;
                                  SL    : TStringList) : Boolean;
    function  ConvertMaskToRegEx(const Str : string) : string;
    procedure DisposeItems(var Data : PStPatRecord);

    procedure InsertLineNumber(Dest : PChar;
                               const Str : PChar; LineNum : Integer);
    function  GetPattern(var Pattern : PChar;
                         var PatList : PStPatRecord) : Boolean;
    function  GetReplace(Pattern     : PChar;
                         var PatList : PStPatRecord) : Boolean;
    function  MakePattern(var Pattern : PChar;
                              Start   : Integer;
                              Delim   : Char;
                          var TagOn   : Boolean;
                          var PatList : PStPatRecord) : Integer;
    function  MakeReplacePattern(Pattern     : PChar;
                                 Start       : Integer;
                                 Delim       : Char;
                                 var PatList : PStPatRecord) : Integer;
    function  FindMatch(var Buf        : PChar;
                            PatPtr     : PStPatRecord;
                        var REPosition : TMatchPosition) : Boolean;
    function  MatchOnePatternElement(var Buf    : PChar;
                                     var Int      : Integer;
                                     var TagOn  : Boolean;
                                     var TagNum : Integer;
                                       PatPtr   : PStPatRecord) : Boolean;
    function  ProcessLine(Buf           : PChar;
                          Len           : Integer;
                          LineNum       : Integer;
                          CheckOnly     : Boolean;
                          var REPosition: TMatchPosition) : Boolean;
    function  SearchMatchPattern(var Buf    : PChar;
                                     OffSet : Integer;
                                 var TagOn  : Boolean;
                                 var TagNum : Integer;
                                     PatPtr : PStPatRecord) : Integer;
    procedure SetMatchPatSL(Value : TStringList);
    procedure SetOptions(Value : TStOutputOptions);
    procedure SetReplacePatSL(Value : TStringList);
    procedure SetSelAvoidPatSL(Value : TStringList);
    procedure SubLine(Buf : PChar);
    function  SubLineFindTag(Buf         : PChar;
                             Int           : Integer;
                             IEnd        : Integer;
                             TagNum      : Integer;
                             var Flags   : TStFlag;
                             var IStart  : Integer;
                             var IStop   : Integer) : Boolean;
    function  SubLineMatchOne(Buf        : PChar;
                              var Flags  : TStFlag;
                              var TagOn  : Boolean;
                              var Int      : Integer;
                              var TagNum : Integer;
                              PatPtr     : PStPatRecord) : Boolean;
    function  SubLineMatchPattern(Buf        : PChar;
                                  var Flags  : TStFlag;
                                  var TagOn  : Boolean;
                                  var TagNum : Integer;
                                  OffSet     : Integer;
                                  PatPtr     : PStPatRecord) : Integer;
    procedure SubLineWrite(Buf       : PChar;
                           Str         : PChar;
                           RepRec    : PStPatRecord;
                           Int,
                           IEnd      : Integer;
                           var Flags : TStFlag);

  public
    property InputStream : TStream
      read FInputStream
      write FInputStream;

    property OutputStream : TStream
      read FOutputStream
      write FOutputStream;

    constructor Create;
    destructor Destroy; override;

    function CheckString(const Str : string;
                         var REPosition : TMatchPosition) : Boolean;
    function DOSMasksToRegEx(Masks : string) : Boolean;
    function Execute : Boolean;
    function ReplaceString(var Str : string;
                           var REPosition : TMatchPosition) : Boolean;

    property Avoid : Boolean
      read FAvoid
      write FAvoid;

    property IgnoreCase : Boolean
      read FIgnoreCase
      write FIgnoreCase;

    property InFixedLineLength : Integer
      read FInLineLength
      write FInLineLength;

    property InLineTermChar : Char
      read FInLineTermChar
      write FInLineTermChar;

    property InLineTerminator : TStLineTerminator
      read FInLineTerminator
      write FInLineTerminator;

    property LineCount : Cardinal
      read FInLineCount;

    property LineNumbers : Boolean
      read FLineNumbers
      write FLineNumbers;

    property LinesMatched : Cardinal
      read FMatchCount;

    property LinesPerSecond : Cardinal
      read FLinesPerSec;

    property LinesReplaced : Cardinal
      read FReplaceCount;

    property LinesSelected : Cardinal
      read FSelectCount;

    property MatchPattern : TStringList
      read FMatchPatSL
      write SetMatchPatSL;

    property MaxLineLength : Cardinal
      read FMaxLineLength
      write FMaxLineLength;

    property OnMatch : TStOnMatchEvent
      read FOnMatch
      write FOnMatch;

    property OnProgress : TStOnRegExProgEvent
      read FOnProgress
      write FOnProgress;

    property OutFixedLineLength : Integer
      read FOutLineLength
      write FOutLineLength;

    property OutLineTermChar : Char
      read FOutLineTermChar
      write FOutLineTermChar;

    property OutLineTerminator : TStLineTerminator
      read FOutLineTerminator
      write FOutLineTerminator;

    property OutputOptions : TStOutputOptions
      read FOutputOptions
      write SetOptions;

    property ReplacePattern : TStringList
      read FReplacePatSL
      write SetReplacePatSL;

    property SelAvoidPattern : TStringList
      read FSelAvoidPatSL
      write SetSelAvoidPatSL;
  end;

  {$M+}
  TStRegEx = class
  protected {private}
    FAvoid            : Boolean;
    FIgnoreCase       : Boolean;
    FInFileSize       : Cardinal;
    FInFileStream     : TFileStream;
    FInLineCount      : Cardinal;

    FInLineTermChar   : Char;
    FInLineTerminator : TStLineTerminator;
    FInFixedLineLength: Integer;
    FInputFile        : string;

    FLineNumbers      : Boolean;
    FLinesPerSec      : Cardinal;

    FMatchCount       : Cardinal;

    FMatchPatSL       : TStringList;
    FMatchPatStr      : PChar;
    FMatchPatPtr      : PStPatRecord;

    FMaxLineLength    : Cardinal;

    FNodes            : TStNodeHeap;

    FOnProgress       : TStOnRegExProgEvent;
    FOnMatch          : TStOnMatchEvent;

    FOutFileStream    : TFileStream;
    FOutTextStream    : TStAnsiTextStream;
    FOutLineBuf       : PChar;

    FOutFixedLineLength : Integer;
    FOutLineTermChar  : Char;
    FOutLineTerminator: TStLineTerminator;

    FOutputFile       : string;
    FOutputOptions    : TStOutputOptions;

    FReplaceCount     : Cardinal;
    FReplacePatSL     : TStringList;
    FReplacePatStr    : PChar;
    FReplacePatPtr    : PStPatRecord;

    FSelAvoidPatSL    : TStringList;
    FSelAvoidPatStr   : PChar;
    FSelAvoidPatPtr   : PStPatRecord;

    FSelectCount      : Cardinal;

    FStream           : TStStreamRegEx;

  protected
    procedure SetMatchPatSL(Value : TStringList);
    procedure SetOptions(Value : TStOutputOptions);
    procedure SetReplacePatSL(Value : TStringList);
    procedure SetSelAvoidPatSL(Value : TStringList);
    procedure SetStreamProperties;
  public
    constructor Create(AOwner : TComponent);
    destructor Destroy; override;

    function CheckString(const Str : string;
                         var REPosition : TMatchPosition) : Boolean;
    function DOSMasksToRegEx(Masks : string) : Boolean;
    function Execute : Boolean;
    function ReplaceString(var Str : string;
                           var REPosition : TMatchPosition) : Boolean;

  published
    property Avoid : Boolean
      read FAvoid
      write FAvoid;

    property IgnoreCase : Boolean
      read FIgnoreCase
      write FIgnoreCase;

    property InFixedLineLength : Integer
      read FInFixedLineLength
      write FInFixedLineLength;

    property InLineTermChar : Char
      read FInLineTermChar
      write FInLineTermChar;

    property InLineTerminator : TStLineTerminator
      read FInLineTerminator
      write FInLineTerminator;

    property InputFile : string
      read FInputFile
      write FInputFile;

    property LineNumbers : Boolean
      read FLineNumbers
      write FLineNumbers;

    property LineCount : Cardinal
      read FInLineCount;

    property LinesMatched : Cardinal
      read FMatchCount;

    property LinesPerSecond : Cardinal
      read FLinesPerSec;

    property LinesReplaced : Cardinal
      read FReplaceCount;

    property LinesSelected : Cardinal
      read FSelectCount;

    property MatchPattern : TStringList
      read FMatchPatSL
      write SetMatchPatSL;

    property MaxLineLength : Cardinal
      read FMaxLineLength
      write FMaxLineLength;

    property OnMatch : TStOnMatchEvent
      read FOnMatch
      write FOnMatch;

    property OnProgress : TStOnRegExProgEvent
      read FOnProgress
      write FOnProgress;

    property OutFixedLineLength : Integer
      read FOutFixedLineLength
      write FOutFixedLineLength;

    property OutLineTermChar : Char
      read FOutLineTermChar
      write FOutLineTermChar;

    property OutLineTerminator : TStLineTerminator
      read FOutLineTerminator
      write FOutLineTerminator;

    property OutputFile : string
      read FOutputFile
      write FOutputFile;

    property OutputOptions : TStOutputOptions
      read FOutputOptions
      write SetOptions;

    property ReplacePattern : TStringList
      read FReplacePatSL
      write SetReplacePatSL;

    property SelAvoidPattern : TStringList
      read FSelAvoidPatSL
      write SetSelAvoidPatSL;
  end;
  {$M-}


implementation

uses Windows, SysUtils, SsConst, SsBase;

const
  Null           = #0;
  EndStr         = #0;
  NewLine        = #13#10;
  Dash           = '-';
  Esc            = '\';
  Any            = '.';  {was '?'}
  Closure        = '*';
  ClosurePlus    = '+';
  MaybeOne       = '?';  {was '!'}
  Bol            = '^';
  Eol            = '$';
  Ccl            = '[';
  Negate         = '^';
  CclEnd         = ']';
  BTag           = '{';
  ETag           = '}';
  BGroup         = '(';
  EGroup         = ')';
  Alter          = '|';  {was #}
  Ditto          = '&';
  lSpace         = 's';
  lNewline       = 'n';
  lTab           = 't';
  lBackSpace     = 'b';
  lReturn        = 'r';
  lFeed          = 'l';
  lHex           = 'h';
  lWordDelim     = 'w';
  lNil           = 'z';


function CleanUpCase(Str : string) : string;
{-convert string to uppercase and remove duplicates}
var
  K  : Cardinal;
  C  : Char;
begin
  Result := '';
  Str := AnsiUpperCase(Str);
  for var I := 1 to Length(Str) do begin
    C := Str[I];
    if not StrChPosS(Result, C, K) then
      Result := Result + C;
  end;
end;


procedure AppendChar(C : Char; var Str : string);
 {-append a character C onto string S}
begin
  Str := Str + C;
end;


function IsAlphaNum(C : Char) : Boolean;
begin
  Result := IsCharAlphaNumeric(C);
end;


procedure ExpandDash(Delim       : Char;
                     var Pattern : PChar;
                     var Int       : Integer;
                     var Str       : string);
{-expand the innards of the character class, including dashes}
{stop when endc is found}
{return a string S with the expansion}
var
  C,
  CLeft,
  CNext  : Char;
  K      : Integer;

begin
  while (Pattern[Int] <> Delim) and (Pattern[Int] <> EndStr) do begin
    C := Pattern[Int];
    if (C = Esc) then begin
      if (Pattern[Succ(Int)] <> EndStr) then begin
        Int := Succ(Int);
        C := Pattern[Int];
        case C of
          lSpace      : AppendChar(#32, Str);
          lTab        : AppendChar(#9,  Str);
          lBackSpace  : AppendChar(#8,  Str);
          lReturn     : AppendChar(#13, Str);
          lFeed       : AppendChar(#10, Str);
        else
          AppendChar(C, Str);
        end;
      end else
        {escape must be the character}
        AppendChar(Esc, Str);
    end else if (C <> Dash) then
      {literal character}
      AppendChar(C, Str)
    else if ((Length(Str) = 0) or (Pattern[Succ(Int)] = Delim)) then
      {literal dash at begin or end of class}
      AppendChar(Dash, Str)
    else begin
      {dash in middle of class}
      CLeft := Pattern[Pred(Int)];
      CNext := Pattern[Succ(Int)];
      if IsAlphaNum(CLeft) and IsAlphaNum(CNext) and (CLeft <= CNext) then begin
        {legal dash to be expanded}
        for K := (Ord(CLeft)+1) to Ord(CNext) do
          AppendChar(Chr(K), Str);
        {move over the end of dash character}
        Int := Succ(Int);
      end else
        {dash must be a literal}
        AppendChar(Dash, Str);
    end;
    Int := Succ(Int);
  end;
end;


function GetCharacterClass(var Pattern : PChar;
                           var Int       : Integer;
                           var Str       : string;
                           var AToken  : TStTokens) : Boolean;
{-expand a character class starting at position I of Pattern into a string S}
{return a token type (tknCharClass or tknNegCharClass)}
{return I pointing at the end of class character}
{return true if successful}

begin
{skip over start of class character}
  Int := Succ(Int);
  if (Pattern[Int] = Negate) then begin
    AToken := tknNegCharClass;
    Int := Succ(Int);
  end else
    AToken := tknCharClass;
  {expand the character class}
  Str := '';
  ExpandDash(CclEnd, Pattern, Int, Str);
  Result := (Pattern[Int] = CclEnd);
end;





{******************************************************************************}
{                           TStNodeHeap Implementation                         }
{******************************************************************************}

constructor TStNodeHeap.Create;
begin
  inherited Create;

  New(FFreeList);
  FillChar(FFreeList^, sizeof(TStPatRecord), 0);
end;


destructor TStNodeHeap.Destroy;
begin
  nhClearHeap;
  Dispose(FFreeList);

  inherited Destroy;
end;


function TStNodeHeap.AllocNode : PStPatRecord;
begin
  if not Assigned(FFreeList^.NextPattern) then
    New(Result)
  else begin
    Result := FFreeList^.NextPattern;
    FFreeList^.NextPattern := Result^.NextPattern;
  end;
  FillChar(Result^, sizeof(TStPatRecord), 0);
end;


function TStNodeHeap.CloneNode(aNode : PStPatRecord) : PStPatRecord;
begin
  {allocate a new node}
  Result := AllocNode;

  {copy fields}
  Result^.Token         := aNode^.Token;
  Result^.OneChar       := aNode^.OneChar;
  Result^.NextOK        := aNode^.NextOK;
  if Assigned(aNode^.StrPtr) then begin
    New(Result^.StrPtr);
    Result^.StrPtr^     := aNode^.StrPtr^;
  end else
    Result^.StrPtr      := nil;

  {deep clone the nested node}
  if Assigned(aNode^.NestedPattern) then
    Result^.NestedPattern := nhDeepCloneNode(aNode^.NestedPattern);
end;


procedure TStNodeHeap.FreeNode(aNode : PStPatRecord);
begin
  if Assigned(aNode) then begin
    aNode^.NextPattern := FFreeList^.NextPattern;
    FFreeList^.NextPattern := aNode;
  end;
end;


procedure TStNodeHeap.nhClearHeap;
var
  Walker,
  Temp    : PStPatRecord;
begin
  Walker := FFreeList^.NextPattern;
  FFreeList^.NextPattern := nil;
  while Assigned(Walker) do begin
    Temp := Walker;
    Walker := Walker^.NextPattern;
    Dispose(Temp);
  end;
end;


function TStNodeHeap.nhDeepCloneNode(aNode : PStPatRecord) : PStPatRecord;
begin
  {allocate a new node}
  Result := AllocNode;

  {copy fields}
  Result^.Token         := aNode^.Token;
  Result^.OneChar       := aNode^.OneChar;
  Result^.NextOK        := aNode^.NextOK;
  if Assigned(aNode^.StrPtr) then begin
    New(Result^.StrPtr);
    Result^.StrPtr^     := aNode^.StrPtr^;
  end else
    Result^.StrPtr      := nil;

  {recursively deepclone the next and nested nodes}
  if Assigned(aNode^.NextPattern) then
    Result^.NextPattern := nhDeepCloneNode(aNode^.NextPattern);
  if Assigned(aNode^.NestedPattern) then
    Result^.NestedPattern := nhDeepCloneNode(aNode^.NestedPattern);
end;


{******************************************************************************}
{                           TStStreamRegEx Implementation                      }
{******************************************************************************}


constructor TStStreamRegEx.Create;
begin
  inherited Create;

  FAvoid          := False;
  FIgnoreCase     := False;
  FLineNumbers    := False;
  FOutputOptions  := [];

  FInLineTerminator := ltCRLF;
  FInLineTermChar   := #10;
  FInLineLength     := 80;

  FOutLineTerminator := ltCRLF;
  FOutLineTermChar   := #10;
  FOutLineLength     := 80;

  FMaxLineLength := 1024;

  FMatchPatSL    := TStringList.Create;
  FMatchPatPtr   := nil;
  FSelAvoidPatSL := TStringList.Create;
  FSelAvoidPatPtr:= nil;
  FReplacePatSL  := TStringList.Create;
  FReplacePatPtr := nil;

  FInputStream      := nil;
  FInTextStream     := nil;
  FOutputStream     := nil;
  FOutTextStream    := nil;

  FNodes    := TStNodeHeap.Create;
end;


procedure TStStreamRegEx.DisposeItems(var Data : PStPatRecord);
var
  Walker, Temp : PStPatRecord;
begin
  if Assigned(Data) then begin
    Walker := Data;
    while Assigned(Walker) do begin
      Temp := Walker;

      if Assigned(Walker^.StrPtr) then
        Dispose(Walker^.StrPtr);

      if Assigned(Walker^.NestedPattern) then
        DisposeItems(Walker^.NestedPattern);

      Walker := Walker^.NextPattern;
      FNodes.FreeNode(Temp);
    end;
    Data := nil;
  end;
end;


destructor TStStreamRegEx.Destroy;
begin
  DisposeItems(FMatchPatPtr);
  DisposeItems(FSelAvoidPatPtr);
  DisposeItems(FReplacePatPtr);

  FNodes.Free;
  FNodes := nil;

  if (Assigned(FMatchPatStr)) then begin
    FreeMem(FMatchPatStr, StrLen(FMatchPatStr) + 1);
    FMatchPatStr := nil;
  end;

  if (Assigned(FReplacePatStr)) then
    FreeMem(FReplacePatStr, StrLen(FReplacePatStr) + 1);
  FReplacePatStr := nil;

  if (Assigned(FSelAvoidPatStr)) then
    FreeMem(FSelAvoidPatStr, StrLen(FSelAvoidPatStr) + 1);
  FSelAvoidPatStr := nil;

  FMatchPatSL.Free;
  FMatchPatSL := nil;

  FReplacePatSL.Free;
  FReplacePatSL := nil;

  FSelAvoidPatSL.Free;
  FSelAvoidPatSL := nil;

  inherited Destroy;
end;


function TStStreamRegEx.AppendS(Dest, Str1, Str2 : PChar;
                                Count : Cardinal) : PChar;
var
  Remaining : Cardinal;
  Int         : Cardinal;
begin
  Result := Dest;
  Int := StrLen(Str1);
  Remaining := MaxLineLength - Int;
  if (Remaining < StrLen(Str2)) then
    Count := Remaining;
  Move(Str1[0], Dest[0], Int * SizeOf(Char));
  Move(Str2[0], Dest[Int], Count * SizeOf(Char));
  Int := Int + Count;
  Dest[Int] := #0;
end;


function TStStreamRegEx.BuildAllPatterns : Boolean;
var
  Len : Integer;
begin
  if (FMatchPatSL.Count > 0) then begin
    DisposeItems(FMatchPatPtr);

    if (BuildPatternStr(FMatchPatStr, Len, FMatchPatSL)) then begin
      if (Len > 0) then
        GetPattern(FMatchPatStr, FMatchPatPtr)
      else
        DisposeItems(FMatchPatPtr);
      Result := True;
    end else begin
      DisposeItems(FMatchPatPtr);
      Result := False;
    end;
  end else begin
    DisposeItems(FMatchPatPtr);
    Result := True;
  end;

  if Result then begin
    if (FSelAvoidPatSL.Count > 0) then begin
      DisposeItems(FSelAvoidPatPtr);
      if (BuildPatternStr(FSelAvoidPatStr, Len, FSelAvoidPatSL)) then begin
        if (Len > 0) then
          GetPattern(FSelAvoidPatStr, FSelAvoidPatPtr)
        else
          DisposeItems(FSelAvoidPatPtr);
        Result := True;
      end else begin
        DisposeItems(FSelAvoidPatPtr);
        Result := False;
      end;
    end else begin
      DisposeItems(FSelAvoidPatPtr);
      Result := True;
    end;
  end;

  if Result then begin
    if (FReplacePatSL.Count > 0) then begin
      DisposeItems(FReplacePatPtr);
      if (BuildPatternStr(FReplacePatStr, Len, FReplacePatSL)) then begin
        if (Len > 0) then
          GetReplace(FReplacePatStr, FReplacePatPtr)
        else
          DisposeItems(FReplacePatPtr);
        Result := True;
      end else begin
        DisposeItems(FReplacePatPtr);
        Result := False;
      end;
    end else begin
      DisposeItems(FReplacePatPtr);
      Result := True;
    end;
  end;
end;



function TStStreamRegEx.BuildPatternStr(var PStr  : PChar;
                                  var Len   : Integer;
                                      SL    : TStringList) : Boolean;
var
  J: Integer;
begin
  Len := 0;
  for var I := 0 to pred(SL.Count) do
    Len := Len + Length(TrimL(SL[I]));

  if (Len = 0) then
    Result := True
  else begin
    if Assigned(PStr) then
      FreeMem(PStr, StrLen(PStr)+1);
    GetMem(PStr, (Len+1) * SizeOf(Char));
    PStr[Len] := EndStr;
    J := 0;
    for var I := 0 to pred(SL.Count) do begin
      Move(SL[I][1], PStr[J], Length(TrimL(SL[I])) * SizeOf(Char));
      Inc(J, Length(TrimL(SL[I])));
    end;
    Result := True;
  end;
end;


function TStStreamRegEx.CheckString(const Str : string;
                                    var REPosition : TMatchPosition) : Boolean;
var
  Tmp : PChar;
  Int   : Integer;
  Len : Integer;
  OK  : Boolean;
begin
  Int := Length(Str);
  GetMem(Tmp, (Int+3) * SizeOf(Char));
  try
    Move(Str[1], Tmp[0], Int * SizeOf(Char));
    Tmp[Int]   := #13;
    Tmp[Int+1] := #10;
    Tmp[Int+2] := EndStr;

    if (FMatchPatSL.Count > 0) then begin
      OK := BuildPatternStr(FMatchPatStr, Len, FMatchPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FMatchPatStr, FMatchPatPtr)
        else
          DisposeItems(FMatchPatPtr);
      end else
        DisposeItems(FMatchPatPtr);
    end else
      DisposeItems(FMatchPatPtr);

    if (FSelAvoidPatSL.Count > 0) then begin
      OK := BuildPatternStr(FSelAvoidPatStr, Len, FSelAvoidPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FSelAvoidPatStr, FSelAvoidPatPtr)
        else
          DisposeItems(FSelAvoidPatPtr);
      end;
    end else
      DisposeItems(FSelAvoidPatPtr);

    FMatchCount    := 0;
    FSelectCount   := 0;
    FReplaceCount  := 0;
    FInLineCount   := 0;
    FLinesPerSec   := 0;

    REPosition.LineNum := 1;
    if Assigned(FSelAvoidPatPtr) or Assigned(FMatchPatPtr) then
      Result := ProcessLine(Tmp, Int, 1, True, REPosition)
    else begin
      Result := False;
      RaiseStError(ESsRegExError, ssscNoPatterns);
    end;
  finally
    FreeMem(Tmp, Int+3);
  end;
end;


function TStStreamRegEx.ReplaceString(var Str : string;
                                      var REPosition : TMatchPosition) : Boolean;
var
  Tmp : PChar;
  Int   : Integer;
  Len : Integer;
  OK  : Boolean;

      function ProcessString(var Str          : string;
                                 Len        : Integer;
                                 LineNum    : Integer;
                             var REPosition : TMatchPosition) : Boolean;
      var
        TmpBuf : PChar;
        ABuf   : PChar;
        L      : Integer;
      begin
        L := Length(Str)+1;
        GetMem(TmpBuf, (MaxLineLength+1) * SizeOf(Char));
        GetMem(ABuf, L * SizeOf(Char));
        try
          StrPCopy(ABuf, Str);
          if Assigned(FSelAvoidPatPtr) then begin
            if not Avoid then
              Result := FindMatch(ABuf, FSelAvoidPatPtr, REPosition)
            else
              Result := not(FindMatch(ABuf, FSelAvoidPatPtr, REPosition));
          end else
            Result := True;

          if Result then begin
            {met select criterion, perhaps by default}
            FSelectCount := Succ(FSelectCount);
            if Assigned(FReplacePatPtr) then begin
              Result := FindMatch(ABuf, FMatchPatPtr, REPosition);
              if Result then begin
                TmpBuf[0] := #0;
                SubLine(ABuf);
                Str := StrPas(FOutLineBuf);
              end;
            end;
          end;
        finally
          FreeMem(TmpBuf, MaxLineLength+1);
          FreeMem(ABuf, L);
        end;
      end;


begin
  Int := Length(Str);
  GetMem(Tmp, (Int+3) * SizeOf(Char));
  try
    Move(Str[1], Tmp[0], Int * SizeOf(Char));
    Tmp[Int]   := #13;
    Tmp[Int+1] := #10;
    Tmp[Int+2] := EndStr;

    if (FMatchPatSL.Count > 0) then begin
      OK := BuildPatternStr(FMatchPatStr, Len, FMatchPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FMatchPatStr, FMatchPatPtr)
        else
          DisposeItems(FMatchPatPtr);
      end else
        DisposeItems(FMatchPatPtr);
    end else
      DisposeItems(FMatchPatPtr);

    if (FSelAvoidPatSL.Count > 0) then begin
      OK := BuildPatternStr(FSelAvoidPatStr, Len, FSelAvoidPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FSelAvoidPatStr, FSelAvoidPatPtr)
        else
          DisposeItems(FSelAvoidPatPtr);
      end;
    end else
      DisposeItems(FSelAvoidPatPtr);

    if (FReplacePatSL.Count > 0) then begin
      OK := BuildPatternStr(FReplacePatStr, Len, FReplacePatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FReplacePatStr, FReplacePatPtr)
        else
          DisposeItems(FReplacePatPtr);
      end else
        DisposeItems(FReplacePatPtr);
    end else
      DisposeItems(FReplacePatPtr);

    FMatchCount    := 0;
    FSelectCount   := 0;
    FReplaceCount  := 0;
    FInLineCount   := 0;
    FLinesPerSec   := 0;

    GetMem(FInLineBuf, (MaxLineLength+3) * SizeOf(Char));
    GetMem(FOutLineBuf, (MaxLineLength+3) * SizeOf(Char));
    try
      REPosition.LineNum := 1;
      if Assigned(FSelAvoidPatPtr) or Assigned(FMatchPatPtr) and
          (Assigned(FReplacePatPtr))then begin
        Result := ProcessString(Str, Int, 1, REPosition);
      end else begin
        Result := False;
        RaiseStError(ESsRegExError, ssscNoPatterns);
      end;
    finally
      FreeMem(FInLineBuf, MaxLineLength+3);
      FreeMem(FOutLineBuf, MaxLineLength+3);
    end;
  finally
    FreeMem(Tmp, Int+3);
  end;
end;


function TStStreamRegEx.ConvertMaskToRegEx(const Str : string) : string;
var
  Int      : Integer;
  TS     : string;
begin
  Int := 1;
  while (Int <= Length(Str)) do begin
    if (Int = 1) then begin
      if not CharInSet(Str[1], ['*', '?']) then begin
        TS := '((^[' ;
        TS := TS + Str[1] + '])';
        Inc(Int);
      end else
        TS := '(';
    end;

    if not CharInSet(Str[Int], ['*', '?', '.', '\']) then
      TS := TS + Str[Int]
    else begin
      if (Str[Int] = '*') then
        TS := TS + '.*'
      else if (Str[Int] = '?') then begin
        if (Int = 1) then
          TS := TS + '(^.)'
        else
          TS := TS + '.?';
      end else begin
        TS := TS + '\' + Str[Int];
      end;
    end;
    Inc(Int);
  end;
  Result := TS + '\n)';
end;


function TStStreamRegEx.DOSMasksToRegEx(Masks : string) : Boolean;
var
  SL : TStringList;
  Str  : string;
  K  : Cardinal;
  Len: Integer;
begin
  SL := TStringList.Create;
  try
    if StrChPosS(Masks, ';', K) then begin
      while (K > 0) do begin
        Str := Copy(Masks, 1, K-1);
        if (Length(Str) > 0) then begin
          if (SL.Count = 0) then
            SL.Add(ConvertMaskToRegEx(Str))
          else
            SL.Add('|' + ConvertMaskToRegEx(Str));
        end;
        Delete(Masks, 1, K);
        if not (StrChPosS(Masks, ';', K)) then
          Break;
      end;
      if (Length(Masks) > 0) then
        SL.Add('|' + ConvertMaskToRegEx(Masks));
    end else begin
      if (Length(Masks) > 0) then
        SL.Add(ConvertMaskToRegEx(Masks));
    end;

    if (SL.Count > 0) then begin
      FMatchPatSL.Clear;
      FMatchPatSL.Assign(SL);
      DisposeItems(FMatchPatPtr);
      FMatchPatPtr := nil;
      if (BuildPatternStr(FMatchPatStr, Len, FMatchPatSL)) then begin
        if (Len > 0) then
          GetPattern(FMatchPatStr, FMatchPatPtr)
        else begin
          DisposeItems(FMatchPatPtr);
          FMatchPatPtr := nil;
        end;
      end else begin
        DisposeItems(FMatchPatPtr);
        FMatchPatPtr := nil;
      end;
      Result := True;
    end else
      Result := False;
  finally
    SL.Free;
  end;
end;



function TStStreamRegEx.Execute : Boolean;
var
  Len       : TStMemSize;
  LineNum   : Integer;
  ATime     : TDateTime;
  PC        : Cardinal;
  LPC       : Cardinal;
  BytesRead : Cardinal;
  REPosition: TMatchPosition;
  Found     : Boolean;
begin
  if (FMatchPatSL.Count = 0) and
     (FReplacePatSL.Count = 0) and (FSelAvoidPatSL.Count = 0) then
    RaiseStError(ESsRegExError, ssscNoPatterns);

  if (not (BuildAllPatterns)) then
    RaiseStError(ESsRegExError, ssscPatternError);

  if not Assigned(FMatchPatPtr) and not Assigned(FSelAvoidPatPtr) and not Assigned(FReplacePatPtr) then
    RaiseStError(ESsRegExError, ssscNoPatterns);

  if (not (Assigned(FInputStream))) or
     ((not (Assigned(FOutputStream)) and (not (ooCountOnly in OutputOptions)))) then
    RaiseStError(ESsRegExError, ssscStreamsNil);

  FInTextStream := nil;
  FOutTextStream := nil;
  try
    FInTextStream := TStTextStream.Create(FInputStream);
    FInTextStream.LineTermChar := FInLineTermChar;
    FInTextStream.LineTerminator := FInLineTerminator;
    FInTextStream.FixedLineLength := FInLineLength;
    FInFileSize := FInTextStream.Size;

    if not (ooCountOnly in OutputOptions) then begin
      FOutTextStream := TStTextStream.Create(FOutputStream);
      FOutTextStream.LineTermChar := FOutLineTermChar;
      FOutTextStream.LineTerminator := FOutLineTerminator;
      FOutTextStream.FixedLineLength := FInLineLength;
    end;

    FMatchCount    := 0;
    FSelectCount   := 0;
    FReplaceCount  := 0;
    FInLineCount   := 0;
    FLinesPerSec   := 0;
    BytesRead      := 0;
    LPC            := 0;

    FInTextStream.Position := 0;
    FInLineBuf := nil;
    FOutLineBuf := nil;
    try
      GetMem(FInLineBuf, (MaxLineLength+3) * SizeOf(Char));
      GetMem(FOutLineBuf, (MaxLineLength+3) * SizeOf(Char));

      LineNum := 1;
      ATime := Now;
      while not FInTextStream.AtEndOfStream do begin
        Len := FInTextStream.ReadLineArray(FInLineBuf, MaxLineLength);
        Inc(BytesRead, Len);

        FInLineBuf[Len]   := #13;
        FInLineBuf[Len+1] := #10;
        FInLineBuf[Len+2] := EndStr;
        REPosition.LineNum := LineNum;
        Found := ProcessLine(FInLineBuf, Len, LineNum, False, REPosition);
        if (FInFileSize > 0) then begin
          PC := Round(BytesRead / FInFileSize * 100);
          {avoid calling with every line - when OnProgress is assigned}
          {performance is considerably reduced anyway, don't add to it}
          if (PC > LPC) then begin
            LPC := PC;
            if (Assigned(FOnProgress)) then
              FOnProgress(Self, PC);
          end;
        end;
        if (Assigned(FOnMatch)) and (Found) then
          FOnMatch(Self, REPosition);

        Inc(LineNum);
      end;
      ATime := (Now - ATime) * 86400;
      FInLineCount := LineNum-1;
      if (ATime > 0) then
        FLinesPerSec := Trunc(FInLineCount / ATime)
      else
        FLinesPerSec := 0;
      if (Assigned(FOnProgress)) then
        FOnProgress(Self, 100);
      Result := (FMatchCount > 0) or (FSelectCount > 0);
    finally
      FreeMem(FInLineBuf, MaxLineLength+3);
      FreeMem(FOutLineBuf, MaxLineLength+3);
    end;
  finally
    FInTextStream.Free;
    FInTextStream := nil;
    FOutTextStream.Free;
    FOutTextStream := nil;
  end;
end;


procedure TStStreamRegEx.AddTokenToPattern(var PatRec : PStPatRecord;
                                           LastPatRec : PStPatRecord;
                                                Token : TStTokens;
                                                    Str : string);
{-add a token record to the pattern list}
{-S contains a literal character or an expanded character class}


begin
  PatRec := FNodes.AllocNode;
  PatRec^.Token := Token;        {save token type}
  PatRec^.NextOK := False;       {default to non-alternation}

  LastPatRec^.NextPattern := PatRec;  {hook up the previous token}
  case Token of
    tknNil, tknAnyChar, tknBegOfLine, tknEndOfLine, tknGroup, tknBegTag, tknEndTag :
      begin
        PatRec^.OneChar := Null;
        PatRec^.StrPtr := nil;
      end;
    tknLitChar :
      begin
        if IgnoreCase then
          PatRec^.OneChar := AnsiUpperCase(Str[1])[1]
        else
          PatRec^.OneChar := Str[1];
        PatRec^.StrPtr := nil;
      end;
    tknCharClass, tknNegCharClass :
      begin
        PatRec^.OneChar := Null;
        if FIgnoreCase then
          Str := CleanUpCase(Str);
        New(PatRec^.StrPtr);
        PatRec^.StrPtr^ := Str;
      end;
  else
    RaiseStError(ESsRegExError, ssscUnknownError);
  end;
end;


function TStStreamRegEx.MakePattern(var Pattern : PChar;
                                        Start   : Integer;
                                        Delim   : Char;
                                    var TagOn   : Boolean;
                                    var PatList : PStPatRecord) : Integer;
var
  Int              : Integer;
  NextLastPatRec,
  LastPatRec,
  TempPatRec,
  PatRec         : PStPatRecord;
  Done           : Boolean;
  AChar          : Char;
  TmpStr         : string;
  AToken         : TStTokens;
  GroupStartPos,
  GroupEndPos    : Integer;

begin
  PatList := FNodes.AllocNode;
  PatList^.Token  := tknNil;    {put a nil token at the beginning}
  PatList^.NextOK := False;
  LastPatRec := PatList;
  NextLastPatRec := nil;

  Int := Start;                 {start point of pattern string}
  Done := False;
  while not(Done) and (Pattern[Int] <> Delim) and (Pattern[Int] <> EndStr) do begin
    AChar := Pattern[Int];
    if (AChar = Any) then
      AddTokenToPattern(PatRec, LastPatRec, tknAnyChar, AChar)
    else if (AChar = Bol) then
      AddTokenToPattern(PatRec, LastPatRec, tknBegOfLine, '')
    else if (AChar = Eol) then
      AddTokenToPattern(PatRec, LastPatRec, tknEndOfLine, '')
    else if (AChar = Ccl) then begin
      Done := (GetCharacterClass(Pattern, Int, TmpStr, AToken) = False);
      if Done then
        RaiseStError(ESsRegExError, ssscExpandingClass);
      AddTokenToPattern(PatRec, LastPatRec, AToken, TmpStr);
    end else if (AChar = Alter) then begin
      if not Assigned(NextLastPatRec) or
         ((NextLastPatRec^.Token <> tknClosure) and
          (NextLastPatRec^.Token <> tknMaybeOne)) then begin
        {flag the current token as non-critical, i.e., "next is OK"}
        LastPatRec^.NextOK := True;
      end else begin
        {alternation immediately after a closure is probably not desired}
        {e.g., [a-z]*|[0-9] would internally produce ([a-z]|[0-9])*}
        Done := True;
        RaiseStError(ESsRegExError, ssscAlternationFollowsClosure);
      end;
    end else if (AChar = BGroup) then begin
      GroupStartPos := Int+1;
      AddTokenToPattern(PatRec, LastPatRec, tknGroup, '');
      {recursive branch off the list}
      Int := MakePattern(Pattern, Succ(Int), EGroup, TagOn, TempPatRec);
      if (Int > 0) then begin
        GroupEndPos := Int-1;
        if (Pattern[Int+1] <> EndStr) then begin
          if CharInSet(Pattern[Int+1], [Closure, ClosurePlus]) then begin
            if  ((((GroupEndPos - GroupStartPos) = 1) or
                (((GroupEndPos - GroupStartPos) = 2) and (Pattern[GroupStartPos] = Esc))) and
                CharInSet(Pattern[GroupEndPos], [Closure, MaybeOne])) then begin
              Done := True;
              RaiseStError(ESsRegExError, ssscClosureMaybeEmpty);
            end else
              PatRec^.NestedPattern := TempPatRec;
          end else
            PatRec^.NestedPattern := TempPatRec;
        end else
          PatRec^.NestedPattern := TempPatRec;
      end else begin
        {didn't find egroup}
        Done := True;
        RaiseStError(ESsRegExError, ssscUnbalancedParens);
      end;
    end else if ((AChar = BTag) and (not(TagOn))) then begin
      AddTokenToPattern(PatRec, LastPatRec, tknBegTag, '');
      TagOn := True;
    end else if ((AChar = ETag) and (TagOn)) then begin
      AddTokenToPattern(PatRec, LastPatRec, tknEndTag,  '');
      TagOn := False;
    end else if (((AChar = Closure) or (AChar = ClosurePlus) or
                  (AChar = MaybeOne)) and (Int > Start)) then begin
      if ((LastPatRec^.Token in [tknBegOfLine, tknEndOfLine, tknMaybeOne, tknClosure]) or
          (NextLastPatRec^.Token = tknClosure)) then begin
        {error, can't have closure after any of these}
        Done := True;
        RaiseStError(ESsRegExError, ssscFollowingClosure);
      end else begin
        if (AChar = ClosurePlus) then begin
          {insert an extra copy of the last token before the closure}
          TempPatRec := FNodes.CloneNode(LastPatRec);
          NextLastPatRec^.NextPattern := TempPatRec;
          TempPatRec^.NextPattern := LastPatRec;
          NextLastPatRec := TempPatRec;
        end;
        {insert the closure between next to last and last token}
        TempPatRec := FNodes.AllocNode;
        NextLastPatRec^.NextPattern := TempPatRec;
        if (AChar = MaybeOne) then
          TempPatRec^.Token := tknMaybeOne
        else
          TempPatRec^.Token := tknClosure;
        TempPatRec^.OneChar := Null;

        TempPatRec^.NextPattern := LastPatRec;
        TempPatRec^.NextOK := False;
        {set j and lastj back into sequence}
        PatRec := LastPatRec;
        LastPatRec := TempPatRec;
      end;
    end else begin
      if (AChar = Esc) then begin
        {skip over escape character}
        Int := Succ(Int);
        AChar := Pattern[Int];
        case AChar of
          lSpace     : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #32);
          lNewline   :
            begin
              AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #13);
              LastPatRec  := PatRec;
              AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #10);
            end;
          lTab       : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #9);
          lBackSpace : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #8);
          lReturn    : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #13);
          lFeed      : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #10);
          lWordDelim : AddTokenToPattern(PatRec, LastPatRec, tknCharClass, StWordDelimString);
          lHex       : AddTokenToPattern(PatRec, LastPatRec, tknCharClass, StHexDigitString);
        else
          AddTokenToPattern(PatRec, LastPatRec, tknLitChar,AChar);
        end;
      end else
        AddTokenToPattern(PatRec, LastPatRec, tknLitChar, AChar);
    end;
    NextLastPatRec := LastPatRec;
    LastPatRec  := PatRec;
    if not(Done) then
      Int := Succ(Int);
  end; {of looking through pattern string}

  if ((Done) or (Pattern[Int] <> Delim)) then begin
    Result := 0;
    RaiseStError(ESsRegExError, ssscPatternError);
  end else
    Result := Int;
end;


function TStStreamRegEx.GetPattern(var Pattern : PChar;
                                   var PatList : PStPatRecord) : Boolean;
{-convert a Pattern PAnsiChar into a pattern list, pointed to by patlist}
{-return true if successful}
var
  TagOn          : Boolean;
begin
  TagOn := False;
  Result := (MakePattern(Pattern, 0, EndStr, TagOn, PatList) > 0);
  if TagOn then begin
    GetPattern := False;
    RaiseStError(ESsRegExError, ssscUnbalancedTag);
  end;
end;


procedure TStStreamRegEx.AddTokenToReplace(var PatRec : PStPatRecord;
                                           LastPatRec : PStPatRecord;
                                                Token : TStTokens;
                                                Str     : string);
{-add a token record to the pattern list}
{S contains a literal character or an expanded character class}
begin
  PatRec := FNodes.AllocNode;
  PatRec^.Token := Token;                    {save token type}
  PatRec^.NextOK  := False;                  {default to non-alternation}
  LastPatRec^.NextPattern := PatRec;         {hook up the previous token}
  if (Token = tknLitChar) or (Token = tknDitto) then begin
    PatRec^.OneChar := Str[1];
    PatRec^.StrPtr := nil;
  end else
    RaiseStError(ESsRegExError, ssscUnknownError);
end;


function TStStreamRegEx.MakeReplacePattern(Pattern     : PChar;
                                           Start       : Integer;
                                           Delim       : Char;
                                           var PatList : PStPatRecord) : Integer;
{-make a pattern list from arg[i], starting at start, ending at delim}
{return 0 is error, last char position in arg if OK}
var
  Int          : Integer;
  PatRec,
  LastPatRec : PStPatRecord;
  Done       : Boolean;
  AChar      : Char;

begin
  PatList := FNodes.AllocNode;
  PatList^.Token     := tknNil;    {put a nil token at the beginning}
  PatList^.NextOK    := False;
  LastPatRec := PatList;
  Int := Start;                    {start point of pattern string}
  Done := False;
  while not(Done) and (Pattern[Int] <> Delim) and (Pattern[Int] <> EndStr) do begin
    AChar := Pattern[Int];
    if (AChar = Ditto) then
      AddTokenToReplace(PatRec, LastPatRec, tknDitto, '0')
    else begin
      if (AChar = Esc) then begin
        {skip over escape character}
        Int := Succ(Int);
        AChar := Pattern[Int];
        if (AChar >= '1') and (AChar <= '9') then
          {a tagged ditto}
          AddTokenToReplace(PatRec, LastPatRec, tknDitto, AChar)
        else case AChar of
          lSpace       : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #32);
          lNewline     :
            begin
              AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #13);
              LastPatRec := PatRec;
              AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #10);
            end;
          lTab         : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #9);
          lBackSpace   : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #8);
          lReturn      : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #13);
          lFeed        : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #10);
          lNil         : ;
        else
          AddTokenToReplace(PatRec, LastPatRec, tknLitChar, AChar);
        end;
      end else
        AddTokenToReplace(PatRec, LastPatRec, tknLitChar, AChar);
    end;
    LastPatRec := PatRec;
    if not(Done) then
      Inc(Int);
  end; {of looking through pattern string}

  if Done or (Pattern[Int] <> Delim) then begin
    Result := 0;
    RaiseStError(ESsRegExError, ssscPatternError);
  end else
    Result := Int;
end;


function TStStreamRegEx.GetReplace(Pattern     : PChar;
                                   var PatList : PStPatRecord) : Boolean;
begin
  Result := (MakeReplacePattern(Pattern, 0, EndStr, PatList) > 0);
end;


function TStStreamRegEx.MatchOnePatternElement(var Buf    : PChar;
                                               var Int      : Integer;
                                               var TagOn  : Boolean;
                                               var TagNum : Integer;
                                               PatPtr   : PStPatRecord) : Boolean;
{-match one pattern element at pattern pointed to by PatPtr, Buf[I]}
var
  Advance  : -1..255;
  AToken   : TStTokens;
  PatPos   : Integer;
  K        : Cardinal;
  C        : Char;
begin
  Advance := -1;
  AToken := PatPtr^.Token;
  if FIgnoreCase then
    C := AnsiUpperCase(Buf[Int])[1]
  else
    C := Buf[Int];

  if (C <> EndStr) then begin
    if (AToken = tknLitChar) then begin
      if (C = PatPtr^.OneChar) then
        Advance := 1;
    end else if (AToken = tknCharClass) then begin
      if (StrChPosS(PatPtr^.StrPtr^, C, K)) then
        Advance := 1;
    end else if (AToken = tknNegCharClass) then begin
      if (not CharInSet(C, [#13, #10])) then begin
        if not (StrChPosS(PatPtr^.StrPtr^, C, K)) then
          Advance := 1;
      end;
    end else if (AToken = tknAnyChar) then begin
      if not CharInSet(C, [#13, #10]) then
        Advance := 1;
    end else if (AToken = tknBegOfLine) then begin
      if (Int = 0) then
        Advance := 0;
    end else if (AToken = tknEndOfLine) then begin
      if (C = #13) and (Buf[Succ(Int)] = #10) then
        Advance := 0;
    end else if (AToken = tknNil) then begin
      Advance := 0;
    end else if (AToken = tknBegTag) then begin
      Advance := 0;
      if not(TagOn) then begin
        TagNum := Succ(TagNum);
        TagOn := True;
      end;
    end else if (AToken = tknEndTag) then begin
      Advance := 0;
      TagOn := False;
    end else if (AToken = tknGroup) then begin
      {we treat a group as a "character", but allow advance of multiple chars}
      {recursive call to SearchMatchPattern}
      PatPos := SearchMatchPattern(Buf, Int, TagOn, TagNum, PatPtr^.NestedPattern);
      if (PatPos >= Int) then begin
        Int := PatPos;
        Advance := 0;
      end;
    end;
  end else begin
    {at end of line}
    {end tag marks match}
    if (AToken = tknEndTag) then
      Advance := 0;
  end;

  if (Advance >= 0) then begin
    {ignore tag words here, since they are not used}
    Result := True;
    Inc(Int, Advance);
  end else
    Result := False;
end;


function TStStreamRegEx.SearchMatchPattern(var Buf    : PChar;
                                               OffSet : Integer;
                                           var TagOn  : Boolean;
                                           var TagNum : Integer;
                                           PatPtr : PStPatRecord) : Integer;
{-look for match of pattern list starting at PatPtr with Buf[offset...]}
{-return the last position that matched}
var
  Int      : Integer;
  K      : Integer;
  PatRec : PStPatRecord;
  Done   : Boolean;
  AToken : TStTokens;

begin
  Done := False;
  PatRec    := PatPtr;
  while not(Done) and Assigned(PatRec) do begin
    AToken := PatRec^.Token;
    if (AToken = tknClosure) then begin
      {a closure}
      PatRec := PatRec^.NextPattern; {step past the closure in the pattern list}
      Int := OffSet;                   {leave the current line position unchanged}
      {match as many as possible}
      while not(Done) and (Buf[Int] <> EndStr) do begin
       if not(MatchOnePatternElement(Buf, Int, TagOn, TagNum, PatRec)) then
          Done := True;
      end;
      {I points to the location that caused a non-match}
      {match rest of pattern against rest of input}
      {shrink closure by one after each failure}
      Done := False;
      K := -1;
      while not(Done) and (Int >= OffSet) do begin
        K := SearchMatchPattern(Buf, Int, TagOn, TagNum, PatRec^.NextPattern);
        if (K > -1) then
          Done := True
        else
          Dec(Int);
      end;
      OffSet := K;   {if k=-1 then failure else success}
      Done := True;
    end else if (AToken = tknMaybeOne) then begin
      {a 0 or 1 closure}
      PatRec := PatRec^.NextPattern;   {step past the closure marker}
      {match or no match is ok, but advance lin cursor if matched}
      MatchOnePatternElement(Buf, OffSet, TagOn, TagNum, PatRec);
      {advance to the next pattern token}
      PatRec := PatRec^.NextPattern;
    end else if not(MatchOnePatternElement(Buf, OffSet,
                                           TagOn, TagNum, PatRec)) then begin
      if PatRec^.NextOK then begin
        {we get another chance because of alternation}
        PatRec := PatRec^.NextPattern;
      end else begin
        OffSet := -1;
        Done := True;
      end;
    end else begin
      {skip over alternates if we matched already}
      while (PatRec^.NextOK) and Assigned(PatRec^.NextPattern) do
        PatRec := PatRec^.NextPattern;
      {move to the next non-alternate}
      PatRec := PatRec^.NextPattern;
    end;
  end;
  Result := OffSet;
end;


function TStStreamRegEx.FindMatch(var Buf        : PChar;
                                      PatPtr     : PStPatRecord;
                                  var REPosition : TMatchPosition) : Boolean;
var
  Int,
  LPos,
  TagNum : Integer;
  TagOn  : Boolean;

begin
  LPos   := -1;
  Int      := 0;
  TagNum := 0;
  TagOn := False;
  Result := False;
  REPosition.Length := 0;
  while (Buf[Int] <> EndStr) and (LPos = -1) do begin
    LPos := SearchMatchPattern(Buf, Int, TagOn, TagNum, PatPtr);
    Result := (LPos > -1);
    if (Result) then begin
      REPosition.StartPos := Int+1;
      RePosition.EndPos   := LPos;
      RePosition.Length   := REPosition.EndPos - REPosition.StartPos + 1;
    end;
    Inc(Int);
  end;
end;



procedure TStStreamRegEx.InsertLineNumber(Dest    : PChar;
                                    const Str : PChar;
                                    LineNum : Integer);
var
  Count : Cardinal;
  SI    : string;
begin
  Dest[0] := #0;
  Count := StrLen(Str);
  if (Count > MaxLineLength - 8) then
    Count := MaxLineLength - 8;
  SI := LeftPadS(IntToStr(LineNum), 6) + '  ';
  Move(SI[1], Dest[0], 8 * SizeOf(Char));
  Move(Str^, Dest[8], Count * SizeOf(Char));
  Dest[Count+8] := #0;
end;



function TStStreamRegEx.ProcessLine(    Buf       : PChar;
                                        Len       : Integer;
                                        LineNum   : Integer;
                                        CheckOnly : Boolean;
                                    var REPosition: TMatchPosition) : Boolean;
var
  Tmp : PChar;
begin
  GetMem(Tmp, (MaxLineLength+1) * SizeOf(Char));
  try
    if Assigned(FSelAvoidPatPtr) then begin
      if (not Avoid) then
        Result := FindMatch(Buf, FSelAvoidPatPtr, REPosition)
      else if (Avoid) then
        Result := not(FindMatch(Buf, FSelAvoidPatPtr, REPosition))
      else
        Result := True;
    end else
      Result := True;

    if Result then begin
      {met select criterion, perhaps by default}
      FSelectCount := Succ(FSelectCount);
      if Assigned(FReplacePatPtr) and (not CheckOnly) then begin
        if ooModified in FOutputOptions then begin
          {we only want to replace and output lines that have a match}
          Result := FindMatch(Buf, FMatchPatPtr, REPosition);
        end;
        if Result then begin
          Tmp[0] := #0;
          SubLine(Buf);
          if (not (ooCountOnly in FOutputOptions)) then begin
            if (LineNumbers) then
              InsertLineNumber(Tmp, FOutlineBuf, LineNum)
            else
              StrCopy(Tmp, FOutlineBuf);
            Tmp[StrLen(Tmp)-2] := #0;
            FOutTextStream.WriteLineZ(Tmp);
          end;
          {subline keeps a count of matched lines and replaced patterns}
        end;
      end else if Assigned(FMatchPatPtr) then begin
        Result := FindMatch(Buf, FMatchPatPtr, REPosition);
        {met match criterion}
        if Result then begin
          FMatchCount := Succ(FMatchCount);
          if (not CheckOnly) then begin
            if (not (ooCountOnly in FOutputOptions)) then begin
              Buf[Len] := #0;
              if (LineNumbers) then
                InsertLineNumber(Tmp, Buf, LineNum)
              else
                StrCopy(Tmp, Buf);
              Tmp[StrLen(Tmp)] := #0;
              FOutTextStream.WriteLineZ(Tmp);
            end;
          end;
        end;
      end else begin
        {we are neither matching nor replacing, just selecting}
        {output the selected line}
        if (not CheckOnly) then begin
          if (not (ooCountOnly in FOutputOptions)) then begin
            Buf[Len] := #0;
            if (LineNumbers) then
              InsertLineNumber(Tmp, Buf, LineNum)
            else
              StrCopy(Tmp, Buf);
            Tmp[StrLen(Tmp)] := #0;
            FOutTextStream.WriteLineZ(Tmp);
          end;
        end;
      end;
    end else begin
      {non-selected line, do we write it?}
      if (ooUnselected in FOutputOptions) and
         (not (ooCountOnly in FOutputOptions)) then begin
        Buf[Len] := #0;
        if (LineNumbers) then
          InsertLineNumber(Tmp, Buf, LineNum)
        else
          StrCopy(Tmp, Buf);
        Tmp[StrLen(Tmp)] := #0;
        FOutTextStream.WriteLineZ(Tmp);
      end;
    end;
  finally
    FreeMem(Tmp, MaxLineLength+1);
  end;
end;



procedure TStStreamRegEx.SetMatchPatSL(Value : TStringList);
begin
  FMatchPatSL.Assign(Value);
  DisposeItems(FMatchPatPtr);
end;



procedure TStStreamRegEx.SetOptions(Value : TStOutputOptions);
begin
  if (Value <> FOutputOptions) then begin
    FOutputOptions := Value;
    if (ooCountOnly in FOutputOptions) then
      FOutputOptions := [ooCountOnly];
  end;
end;



procedure TStStreamRegEx.SetReplacePatSL(Value : TStringList);
begin
  FReplacePatSL.Assign(Value);
  DisposeItems(FReplacePatPtr);
end;



procedure TStStreamRegEx.SetSelAvoidPatSL(Value : TStringList);
begin
  FSelAvoidPatSL.Assign(Value);
  DisposeItems(FSelAvoidPatPtr);
end;


function TStStreamRegEx.SubLineMatchOne(Buf        : PChar;
                                        var Flags  : TStFlag;
                                        var TagOn  : Boolean;
                                        var Int      : Integer;
                                        var TagNum : Integer;
                                        PatPtr     : PStPatRecord) : Boolean;
var
  Advance  : -1..255;
  lToken   : TStTokens;
  PatPos   : Integer;
  K        : Cardinal;
  C        : Char;
begin
  Advance := -1;
  lToken := PatPtr^.Token;
  if FIgnoreCase then
    C := AnsiUpperCase(Buf[Int])[1]
  else
    C := Buf[Int];

  if (C <> EndStr) then begin
    if (lToken = tknLitChar) then begin
      if (C = PatPtr^.OneChar) then
        Advance := 1;
    end else if (lToken = tknCharClass) then begin
      if (StrChPosS(PatPtr^.StrPtr^, C, K)) then
        Advance := 1;
    end else if (lToken = tknNegCharClass) then begin
      if (Pos(C, NewLine) = 0) then begin
        if not (StrChPosS(PatPtr^.StrPtr^, C, K)) then
          Advance := 1;
      end;
    end else if (lToken = tknAnyChar) then begin
      if (not CharInSet(C, [#13, #10])) then
        Advance := 1;
    end else if (lToken = tknBegOfLine) then begin
      if (Int = 0) then
        Advance := 0;
    end else if (lToken = tknEndOfLine) then begin
      if (C = #13) and (Buf[Succ(Int)] = #10) then begin
        Advance := 0;
      end;
    end else if (lToken = tknNil) then begin
      Advance := 0;
    end else if (lToken = tknBegTag) then begin
      Advance := 0;
      if not(TagOn) then begin
        Inc(TagNum);
        TagOn := True;
      end;
    end else if (lToken = tknEndTag) then begin
      Advance := 0;
      TagOn := False;
    end else if (lToken = tknGroup) then begin
      {we treat a group as a "character", but allow advance of multiple chars}

      PatPos := SubLineMatchPattern(Buf, Flags, TagOn, TagNum,
                                    Int, PatPtr^.NestedPattern);
      if (PatPos >= Int) then begin
        Int := PatPos;
        Advance := 0;
      end;
    end;
  end else begin
    {at end of line}
    {end tag marks match}
    if (lToken = tknEndTag) then
      Advance := 0;
  end;

  if (Advance > 0) then begin
    {we had a match at this (these) character position(s)}
    {set the match flags}
    if (TagOn) then
      Flags[Int] := TagNum
    else
      Flags[Int] := 0;
    Inc(Int, Advance);
    Result := True;
  end else if (Advance = 0) then begin
    Result := True;
  end else begin
    {this character didn't match}
    Result := False;
    Flags[Int] := -1;
  end;
end;



function TStStreamRegEx.SubLineMatchPattern(Buf        : PChar;
                                            var Flags  : TStFlag;
                                            var TagOn  : Boolean;
                                            var TagNum : Integer;
                                            OffSet     : Integer;
                                            PatPtr     : PStPatRecord) : Integer;
{-look for match of pattern list starting at PatPtr with Buf[offset...]}
{return the last position that matched}
var
  Int,
  LocTag   : Integer;
  PatPos   : Integer;
  PatRec   : PStPatRecord;
  Done     : Boolean;
  AToken   : TStTokens;
  OldTagOn : Boolean;
  OldTagNum: Integer;
begin
  Done := False;
  PatRec := PatPtr;
  while not(Done) and Assigned(PatRec) do begin
    AToken := PatRec^.Token;
    if (AToken = tknClosure) then begin
      {a closure}
      PatRec := PatRec^.NextPattern; {step past the closure in the pattern list}
      Int := OffSet;                   {leave the current line position unchanged}
      LocTag := TagNum;
      {match as many as possible}
      while not(Done) and (Buf[Int] <> EndStr) do begin
{        if not(SubLineMatchOne(Buf, Flags, TagOn,
                                LocTag, I, PatRec)) then}
        if not(SubLineMatchOne(Buf, Flags, TagOn,
                               Int, LocTag, PatRec)) then
          Done := True;
      end;
      {i points to the location that caused a non-match}
      {match rest of pattern against rest of input}
      {shrink closure by one after each failure}
      Done := False;
      PatPos := -1;
      while not(Done) and (Int >= OffSet) do begin
        OldTagOn := TagOn;
        OldTagNum := LocTag;
        PatPos := SubLineMatchPattern(Buf, Flags, TagOn,
                                      LocTag, Int, PatRec^.NextPattern);
        if (PatPos > -1) then
          Done := True
        else begin
          Int := Pred(Int);
          TagOn := OldTagOn;
          LocTag := OldTagNum;
        end;
      end;
      OffSet := PatPos;            {if k=-1 then failure else success}
      TagNum := LocTag;
      Done   := True;
    end else if (AToken = tknMaybeOne) then begin
      {a 0 or 1 closure}
      PatRec := PatRec^.NextPattern;    {step past the closure marker}
      {match or no match is ok, but advance lin cursor if matched}
      SubLineMatchOne(Buf, Flags, TagOn, OffSet, TagNum, PatRec);
      {advance to the next pattern token}
      PatRec := PatRec^.NextPattern;
    end else if not(SubLineMatchOne(Buf, Flags, TagOn,
                                    OffSet, TagNum, PatRec)) then begin
      if PatRec^.NextOK then begin
        {we get another chance because of alternation}
        PatRec := PatRec^.NextPattern;
      end else begin
        OffSet := -1;
        Done := True;
      end;
    end else begin
      {skip over alternates if we matched already}
      while PatRec^.NextOK and Assigned(PatRec^.NextPattern) do
        PatRec := PatRec^.NextPattern;
      {move to the next non-alternate}
      PatRec := PatRec^.NextPattern;
    end;
  end;
  Result := OffSet;
end;


function TStStreamRegEx.SubLineFindTag(Buf         : PChar;
                                       Int           : Integer;
                                       IEnd        : Integer;
                                       TagNum      : Integer;
                                       var Flags   : TStFlag;
                                       var IStart  : Integer;
                                       var IStop   : Integer) : Boolean;
{-find the tagged match region}
{return true if it is found}
begin
  IStart := Int;
  while (Buf[IStart] <> EndStr) and (Flags[IStart] <> TagNum) do
    Inc(IStart);
  if (Flags[IStart] = TagNum) then begin
    Result := True;
    IStop := IStart;
    while (Flags[IStop] = TagNum) and (IStop < IEnd) do
      Inc(IStop);
  end else
    Result := False;
end;  {findtag}



procedure TStStreamRegEx.SubLineWrite(Buf       : PChar;
                                      Str         : PChar;
                                      RepRec    : PStPatRecord;
                                      Int,
                                      IEnd      : Integer;
                                      var Flags : TStFlag);
{-Write the output line with replacements}
var
  TagNum,
  IStart,
  IStop     : Integer;
  PatRec    : PStPatRecord;
  Token     : TStTokens;
begin  {writesub}
  {scan the replacement list}
  Str[0] := #0;
  PatRec := RepRec;
  while Assigned(PatRec) do begin
    Token := PatRec^.Token;
    if (Token = tknDitto) then begin
      TagNum := Ord(PatRec^.OneChar)-Ord('0');
      if (TagNum = 0) then begin
        {untagged ditto}
        {add the entire matched region}
        AppendS(Str, Str, @Buf[Int], IEnd-Int);
      end else begin
        {tagged ditto}
        {find the tagged region}

        if SubLineFindTag(Buf, Int, IEnd, TagNum, Flags, IStart, IStop) then begin
          {add the tagged region}
          AppendS(Str, Str, @Buf[IStart], IStop-IStart);
        end else begin
           {else couldn't find tagged word, don't append anything}
        end;
      end;
    end else if (Token = tknLitChar) then
      AppendS(Str, Str, @PatRec^.OneChar, 1);
    PatRec := PatRec^.NextPattern;
  end;
end;



procedure TStStreamRegEx.SubLine(Buf : PChar);
var
  Int,
  M,
  NumToAdd,
  TagNum,
  Lastm      : Integer;

  Flags      : TStFlag;
  TagOn,
  DidReplace : Boolean;
  ALine      : PChar;
begin
  DidReplace := False;
  LastM  := -1;
  Int := 0;

  GetMem(ALine, (MaxLineLength+1) * SizeOf(Char));
  try
    FOutLineBuf[0] := #0;
    FillChar(ALine^, (MaxLineLength+1) * SizeOf(Char), #0);
    while (Buf[Int] <> EndStr) do begin
      TagNum := 0;
      TagOn := False;

      M := SubLineMatchPattern(Buf, Flags, TagOn, TagNum, Int, FMatchPatPtr);
      if (M > -1) and (M <> Int) and (LastM <> M) then begin
        {keep track of count}
        DidReplace := True;
        Inc(FReplaceCount);
        {replace matched text}

        SubLineWrite(Buf, ALine, FReplacePatPtr, Int, M, Flags);
        LastM := M;
        AppendS(FOutLineBuf, FOutLineBuf, ALine, StrLen(ALine));
      end;

      if (M = -1) or (M = Int) then begin
        {no match or null match, append the character}
          if (Buf[Int] = #13) then
            NumToAdd := 2
          else
            NumToAdd := 1;
        AppendS(FOutLineBuf, FOutLineBuf, @Buf[Int], NumToAdd);
        Int := Int + NumToAdd;
      end else                    {skip matched text}
        Int := M;

    end;
    if DidReplace then
      Inc(FMatchCount);
  finally
    FreeMem(ALine, MaxLineLength+1)
  end;
end;


{******************************************************************************}
{                           TStRegEx Implementation                            }
{******************************************************************************}

constructor TStRegEx.Create(AOwner : TComponent);
begin
  FAvoid          := False;
  FIgnoreCase     := False;
  FLineNumbers    := False;
  FOutputOptions  := [];

  FInLineTerminator := ltCRLF;
  FInLineTermChar   := #10;
  FInFixedLineLength:= 80;

  FOutLineTerminator  := ltCRLF;
  FOutLineTermChar    := #10;
  FOutFixedLineLength := 80;      {not used straight away}

  FMaxLineLength := 1024;

  FMatchPatSL    := TStringList.Create;
  FMatchPatPtr   := nil;
  FSelAvoidPatSL := TStringList.Create;
  FSelAvoidPatPtr:= nil;
  FReplacePatSL  := TStringList.Create;
  FReplacePatPtr := nil;

  FInFileStream  := nil;
  FOutFileStream := nil;

  FStream := TStStreamRegEx.Create;
end;


destructor TStRegEx.Destroy;
begin
  FMatchPatSL.Free;
  FMatchPatSL := nil;

  FReplacePatSL.Free;
  FReplacePatSL := nil;

  FSelAvoidPatSL.Free;
  FSelAvoidPatSL := nil;

  FStream.Free;
  FStream := nil;

  inherited Destroy;
end;


function TStRegEx.CheckString(const Str : string;
                              var REPosition : TMatchPosition) : Boolean;
begin
  if (Assigned(FStream)) then begin
    SetStreamProperties;
    Result := FStream.CheckString(Str, REPosition);
  end else
    Result := False;
end;


function TStRegEx.ReplaceString(var Str : string;
                                var REPosition : TMatchPosition) : Boolean;
begin
  if (Assigned(FStream)) then begin
    SetStreamProperties;
    Result := FStream.ReplaceString(Str, REPosition);
  end else
    Result := False;
end;


function TStRegEx.DOSMasksToRegEx(Masks : string) : Boolean;
begin
  if (Assigned(FStream)) then begin
    SetStreamProperties;
    Result := FStream.DOSMasksToRegEx(Masks);
    if (Result) then
      FMatchPatSL.Assign(FStream.FMatchPatSL);
  end else
    Result := False;
end;


function TStRegEx.Execute : Boolean;
begin
  Result := False;
  try
    if (not FileExists(FInputFile)) then
      RaiseStError(ESsRegExError, ssscInFileNotFound);

    try
      FInFileStream := TFileStream.Create(FInputFile,
                                          fmOpenRead or fmShareDenyWrite);
      FStream.InputStream := FInFileStream
    except
      RaiseStError(ESsRegExError, ssscREInFileError);
      Exit;
    end;

    if not (ooCountOnly in OutputOptions) then begin
      if (FileExists(FOutputFile)) then
        try
          SysUtils.DeleteFile(FOutputFile);
        except
          RaiseStError(ESsRegExError, ssscOutFileDelete);
          Exit;
        end;

      FOutFileStream := nil;
      FStream.OutputStream := nil;
      try
        FOutFileStream := TFileStream.Create(FOutputFile, fmCreate);
        FStream.OutputStream := FOutFileStream
      except
        RaiseStError(ESsRegExError, ssscOutFileCreate);
        Exit;
      end;
    end;

    SetStreamProperties;
    Result := FStream.Execute;

    FMatchCount    := FStream.FMatchCount;
    FSelectCount   := FStream.FSelectCount;
    FReplaceCount  := FStream.FReplaceCount;
    FInLineCount   := FStream.FInLineCount;
    FLinesPerSec   := FStream.FLinesPerSec;
  finally
    FInFileStream.Free;
    FInFileStream := nil;

    FOutFileStream.Free;
    FOutFileStream := nil;
  end;
end;



procedure TStRegEx.SetMatchPatSL(Value : TStringList);
begin
  FMatchPatSL.Assign(Value);
end;



procedure TStRegEx.SetOptions(Value : TStOutputOptions);
begin
  if (Value <> FOutputOptions) then begin
    FOutputOptions := Value;
    if (ooCountOnly in FOutputOptions) then
      FOutputOptions := [ooCountOnly];
  end;
end;



procedure TStRegEx.SetReplacePatSL(Value : TStringList);
begin
  FReplacePatSL.Assign(Value);
end;



procedure TStRegEx.SetSelAvoidPatSL(Value : TStringList);
begin
  FSelAvoidPatSL.Assign(Value);
end;



procedure TStRegEx.SetStreamProperties;
begin
  if (not Assigned(FStream)) then Exit;

  FStream.InLineTermChar    := FInLineTermChar;
  FStream.InLineTerminator  := FInLineTerminator;
  FStream.InFixedLineLength := FInFixedLineLength;

  FStream.InLineTermChar    := FOutLineTermChar;
  FStream.InLineTerminator  := FOutLineTerminator;
  FStream.InFixedLineLength := FOutFixedLineLength;

  FStream.Avoid          := FAvoid;
  FStream.IgnoreCase     := FIgnoreCase;
  FStream.LineNumbers    := FLineNumbers;
  FStream.MatchPattern   := FMatchPatSL;
  FStream.OnMatch        := FOnMatch;
  FStream.OnProgress     := FOnProgress;
  FStream.OutputOptions  := FOutputOptions;
  FStream.ReplacePattern := FReplacePatSL;
  FStream.SelAvoidPattern:= FSelAvoidPatSL;

  FStream.FMatchCount    := 0;
  FStream.FSelectCount   := 0;
  FStream.FReplaceCount  := 0;
  FStream.FInLineCount   := 0;
  FStream.FLinesPerSec   := 0;
end;


end.

