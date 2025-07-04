{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterRuby.pas, released 2001-11-13.
The Initial Author of this file is Stefan Ascher.
Portions by Jan Verhoeven (http://jansfreeware.com/jfdelphi.htm)
"Heredoc" syntax highlighting implementation by Marko Njezic.
Unicode translation by Ma�l H�rz.
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

$Id: SynHighlighterRuby.pas,v 1.10.2.9 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Ruby highlighter for SynEdit)
@author(Stefan Ascher <stievie2002@yahoo.com>)
@created(21 May 2001)
@lastmod(2001-11-13)
The SynHighlighterVisualLisp unit provides SynEdit with a Ruby highlighter.
}

unit SynHighlighterRuby;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey,
    tkSpace, tkString, tkSymbol, tkUnknown);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnknown, rsHeredoc, rsIndentedHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnknown);
{$ENDIF}

type
  TSynRubySyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength: Byte;
    fHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSecondKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fSecondKeys: TStrings;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
    procedure SetSecondKeys(const Value: TStrings);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function IsKeyword(const AKeyword: string): Boolean; override;
    function IsSecondKeyWord(aToken: string): Boolean;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SecondKeyAttri: TSynHighlighterAttributes read fSecondKeyAttri
      write fSecondKeyAttri;
    property SecondKeyWords: TStrings read fSecondKeys write SetSecondKeys;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst;

const
  RubyKeysCount = 43;
  RubyKeys: array[1..RubyKeysCount] of string = (
    'alias', 'attr', 'begin', 'break', 'case', 'class', 'def', 'do', 'else',
    'elsif', 'end', 'ensure', 'exit', 'extend', 'false', 'for', 'gets', 'if',
    'in', 'include', 'load', 'loop', 'module', 'next', 'nil', 'not', 'print',
    'private', 'public', 'puts', 'raise', 'redo', 'require', 'rescue', 'retry',
    'return', 'self', 'then', 'true', 'unless', 'when', 'while', 'yield');

function TSynRubySyn.IsKeyword(const AKeyword: string): Boolean;
var
  First, Last, Int, Compare: Integer;
  Token: string;
begin
  First := 0;
  Last := fKeywords.Count - 1;
  Result := False;
  Token := SysUtils.AnsiUpperCase(AKeyword);

  while First <= Last do
  begin
    Int := (First + Last) shr 1;
    Compare := CompareStr(fKeywords[Int], Token);
    if Compare = 0 then
    begin
      Result := True;
      Break;
    end
    else if Compare < 0 then
      First := Int + 1
    else
      Last := Int - 1;
  end;
end; { IsKeyWord }

function TSynRubySyn.IsSecondKeyWord(aToken: string): Boolean;
var
  First, Last, Int, Compare: Integer;
  Token: string;
begin
  First := 0;
  Last := fSecondKeys.Count - 1;
  Result := False;
  Token := SysUtils.AnsiUpperCase(aToken);
  while First <= Last do
  begin
    Int := (First + Last) shr 1;
    Compare := CompareStr(fSecondKeys[Int], Token);
    if Compare = 0 then
    begin
      Result := True;
      Break;
    end
    else if Compare < 0 then
      First := Int + 1
    else
      Last := Int - 1;
  end;
end; { IsSecondKeyWord }

constructor TSynRubySyn.Create(AOwner: TComponent);
var
  Int: Integer;
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;
  fSecondKeys := TStringList.Create;
  TStringList(fSecondKeys).Sorted := True;
  TStringList(fSecondKeys).Duplicates := dupIgnore;
  if not (csDesigning in ComponentState) then
    for Int := 1 to RubyKeysCount do
      fKeyWords.Add(RubyKeys[Int]);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Foreground := clMaroon;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  AddAttribute(fKeyAttri);
  fSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_FriendlyAttrSecondReservedWord);
  AddAttribute(fSecondKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clGreen;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clPurple;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Foreground := clBlue;
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterRuby;
end; { Create }

destructor TSynRubySyn.Destroy;
begin
  fKeyWords.Free;
  fSecondKeys.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynRubySyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRubySyn.PointCommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynRubySyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
end;

procedure TSynRubySyn.IdentProc;
begin
  while IsIdentChar(fLine[Run]) do Inc(Run);
  if IsKeyWord(GetToken) then
  begin
    fTokenId := tkKey;
    Exit;
  end
  else fTokenId := tkIdentifier;
  if IsSecondKeyWord(GetToken) then
    fTokenId := tkSecondKey
  else
    fTokenId := tkIdentifier;
end;

procedure TSynRubySyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynRubySyn.LowerProc;
{$IFDEF SYN_HEREDOC}
var
  Int, Len, SkipRun: Integer;
  IndentedHeredoc: Boolean;
  QuoteChar: WideChar;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  if FLine[Run + 1] = '<' then
  begin
    fTokenID := tkSymbol;

    SkipRun := 0;
    QuoteChar := #0;
    if (FLine[Run + 2] = '-') and (FLine[Run + 3] in
      [WideChar('"'), WideChar(''''), WideChar('`')]) then
    begin
      SkipRun := 2;
      QuoteChar := FLine[Run + 3];
    end
    else
    if (FLine[Run + 2] in [WideChar('-'), WideChar('"'), WideChar(''''), WideChar('`')]) then
    begin
      SkipRun := 1;
      if FLine[Run + 2] <> '-' then
        QuoteChar := FLine[Run + 2];
    end;
    IndentedHeredoc := (SkipRun > 0) and (FLine[Run + 2] = '-');

    if IsIdentChar(FLine[Run + SkipRun + 2]) then
    begin
      Inc(Run, 2);

      Int := Run;
      while IsIdentChar(FLine[SkipRun + Int]) do Inc(Int);
      Len := Int - Run;

      if Len > 255 then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if (QuoteChar <> #0) and (FLine[Run + SkipRun + Len] <> QuoteChar) then
      begin
        fTokenID := tkUnknown;
        Exit;
      end;

      if IndentedHeredoc then
        fRange := rsIndentedHeredoc
      else
        fRange := rsHeredoc;
      fHeredocLength := Len;
      fHeredocChecksum := CalcFCS(FLine[Run + SkipRun], Len);

      Inc(Run, SkipRun + Len);
      fTokenID := tkString;
    end
    else
      Inc(Run, 2);
  end
  else
{$ENDIF}
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynRubySyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynRubySyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynRubySyn.RoundOpenProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynRubySyn.SlashProc;
begin
  case FLine[Run] of
    '/':
      begin
        Inc(Run);
        fTokenId := tkSymbol;
      end;
    '*':
      begin
        Inc(Run);
        fTokenId := tkSymbol;
      end;
  else
    begin
      fTokenID := tkComment;
      while FLine[Run] <> #0 do
      begin
        case FLine[Run] of
          #10, #13: Break;
        end;
        Inc(Run);
      end;
    end;
  end;
end;

procedure TSynRubySyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynRubySyn.StringProc;
var
  QuoteChar: WideChar;
begin
// Ha, ha, Strings in Ruby (could be anything)!!!!

//There are three more ways to construct string literals: %q, %Q, and ``here
//documents.''
//
//%q and %Q start delimited single- and double-quoted strings.
//
//%q/general single-quoted string/ � general single-quoted string
//%Q!general double-quoted string! � general double-quoted string
//%Q{Seconds/day: #{24*60*60}}     � Seconds/day: 86400
//
//The character following the ``q'' or ``Q'' is the delimiter. If it is an
//opening bracket, brace, parenthesis, or less-than sign, the string is read
//until the matching close symbol is found. Otherwise the string is read until
//the next occurrence of the same delimiter.

  fTokenID := tkString;
  QuoteChar := FLine[Run];      // either " or '
  if (FLine[Run + 1] = QuoteChar) and (FLine[Run + 2] = QuoteChar)
    then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = QuoteChar;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynRubySyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynRubySyn.HeredocProc;

  procedure SkipToEOL;
  begin
    case FLine[Run] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    else
      repeat
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;

var
  Int : Integer;
begin
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;

  if fRange = rsIndentedHeredoc then
    while FLine[Run] in [WideChar(#9), WideChar(#32)] do Inc(Run);

  if ((Run = 0) and (fRange = rsHeredoc)) or (fRange = rsIndentedHeredoc) then
  begin
    Int := 0;

    while not IsLineEnd(FLine[Run + Int]) do
    begin
      if Int > fHeredocLength then
      begin
        SkipToEOL;
        Exit;
      end;
      Inc(Int);
    end;

    if Int <> fHeredocLength then
    begin
      SkipToEOL;
      Exit;
    end;

    if (CalcFCS(FLine[Run], Int) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      Run := Run + Int;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynRubySyn.Next;
begin
  fTokenPos := Run;
{$IFDEF SYN_HEREDOC}
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
    HeredocProc
  else
{$ENDIF}
    NextProcedure;
  inherited;
end;

procedure TSynRubySyn.NextProcedure;
begin
  case fLine[Run] of
    '<': LowerProc;
    '#': SlashProc;
    '{': BraceOpenProc;
    ';': PointCommaProc;
    #13: CRProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '(': RoundOpenProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    #34, #39: StringProc;
    else UnknownProc;
  end;
end;

function TSynRubySyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynRubySyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynRubySyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynRubySyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynRubySyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSecondKey: Result := fSecondKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynRubySyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynRubySyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynRubySyn.SetRange(Value: Pointer);
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer := TRangePointer(Value);
  fRange := TRangeState(RangePointer.Range);
  fHeredocLength := 0;
  fHeredocChecksum := 0;
  if fRange in [rsHeredoc, rsIndentedHeredoc] then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

procedure TSynRubySyn.SetSecondKeys(const Value: TStrings);
var
  Int: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for Int := 0 to Value.Count - 1 do
        Value[Int] := SysUtils.AnsiUpperCase(Value[Int]);
      Value.EndUpdate;
    end;
  fSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynRubySyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterRuby;
end;

class function TSynRubySyn.GetLanguageName: string;
begin
  Result := SYNS_LangRuby;
end;

function TSynRubySyn.GetSampleSource: string;
begin
  Result :=
    '# Factorial'+#13#10+
    'def fact(n)'+#13#10+
    '  if n == 0'+#13#10+
    '    1'+#13#10+
    '  else'+#13#10+
    '    n * fact(n-1)'+#13#10+
    '  end'+#13#10+
    'end'+#13#10+
    'print fact(ARGV[0].to_i), "\n"';
end;

class function TSynRubySyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangRuby;
end;

initialization
  RegisterPlaceableHighlighter(TSynRubySyn);
end.
