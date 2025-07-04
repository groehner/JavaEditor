{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPHP.pas, released 2000-04-21.
The Original Code is based on the wmPHPSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Willo van der Merwe.
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

$Id: SynHighlighterPHP.pas,v 1.22.2.7 2005/12/16 20:09:37 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a PHP syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(1999, converted to SynEdit 2000-04-21)
@lastmod(2000-06-23)
The SynHighlighterPHP unit provides SynEdit with a PHP syntax highlighter.
Thanks to Martin Waldenburg.
}

unit SynHighlighterPHP;

{$I SynEdit.inc}

interface

uses
  SynEditHighlighter,
  Classes,
  SynEditCodeFolding;

type
  TtkTokenKind = (tkSymbol, tkKey, tkComment, tkDocument, tkIdentifier, tkNull,
    tkNumber, tkSpace, tkString, tkUnknown, tkVariable);

{$IFDEF SYN_HEREDOC}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsString96, rsComment, rsDocument, rsVarExpansion,
    rsHeredoc);

  TRangePointer = packed record
    case Boolean of
      True: (Ptr: Pointer);
      False: (Range: Byte; Length: Byte; Checksum: Word);
    end;
{$ELSE}
  TRangeState = (rsUnKnown, rsString39, rsString34, rsString96, rsComment, rsDocument, rsVarExpansion);
{$ENDIF}

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
//  TSynPHPSyn = class(TSynCustomHighlighter)
  TSynPHPSyn = class(TSynCustomCodeFoldingHighlighter)
  private
    fRange: TRangeState;
{$IFDEF SYN_HEREDOC}
    fHeredocLength: Byte;
    fHeredocChecksum: Word;
{$ENDIF}
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..420] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fDocumentAttri: TSynHighlighterAttributes;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AndSymbolProc;
    procedure AtSymbolProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure MultiplyProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure PoundProc;
    procedure QuestionProc;
    procedure RemainderSymbolProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StringProc;
    procedure VarExpansionProc;
    procedure TildeProc;
    procedure VariableProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure AnsiCProc;
    procedure String39Proc;
    procedure String34Proc;
    procedure String96Proc;
{$IFDEF SYN_HEREDOC}
    procedure HeredocProc;
{$ENDIF}
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
    procedure NextProcedure;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
//++ CodeFolding
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); override;
//-- CodeFolding
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DocumentAttri: TSynHighlighterAttributes read fDocumentAttri
      write fDocumentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
  end;

implementation

uses
  Windows,
  Graphics,
  SysUtils,
  SynEditStrConst;

const

{ expanded keyword list }
 KeyWords: array[0..109] of string = (
    '__autoload', '__call', '__callstatic', '__class__', '__clone',
    '__construct', '__debuginfo', '__destruct', '__dir__', '__file__',
    '__function__', '__get', '__halt_compiler', '__invoke', '__isset',
    '__line__', '__method__', '__namespace__', '__set', '__set_state',
    '__sleep', '__tostring', '__trait__', '__unset', '__wakeup', 'abstract',
    'and', 'array', 'as', 'binary', 'bool', 'boolean', 'break', 'callable',
    'case', 'catch', 'cfunction', 'class', 'clone', 'const', 'continue',
    'declare', 'default', 'die', 'do', 'double', 'echo', 'else', 'elseif',
    'empty', 'enddeclare', 'endfor', 'endforeach', 'endif', 'endswitch',
    'endwhile', 'eval', 'exception', 'exit', 'extends', 'false', 'final',
    'finally', 'float', 'for', 'foreach', 'function', 'global', 'goto', 'if',
    'implements', 'include', 'include_once', 'instanceof', 'insteadof', 'int',
    'integer', 'interface', 'isset', 'list', 'namespace', 'new', 'null',
    'object', 'old_function', 'or', 'parent', 'print', 'private', 'protected',
    'public', 'real', 'require', 'require_once', 'return', 'self', 'static',
    'string', 'switch', 'throw', 'trait', 'true', 'try', 'unset', 'use', 'var',
    'void', 'while', 'xor', 'yield'
  );

  KeyIndices: array[0..420] of Integer = (
    -1, -1, 83, -1, -1, -1, -1, 9, -1, -1, -1, -1, -1, -1, -1, -1, 44, -1, -1,
    -1, -1, -1, 5, -1, -1, 13, -1, -1, -1, 89, -1, 27, 59, -1, -1, -1, -1, -1,
    24, -1, -1, -1, -1, -1, -1, -1, 28, -1, -1, 100, -1, -1, -1, -1, -1, 103,
    40, 85, -1, -1, -1, 55, 74, -1, -1, -1, -1, -1, -1, -1, 92, -1, -1, -1, -1,
    -1, -1, -1, 0, -1, -1, -1, -1, 88, 47, 94, 46, -1, -1, -1, 54, -1, 34, 106,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 6, -1, 49, -1, -1, 87, -1,
    -1, -1, 95, 65, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 43, -1, 32, -1, -1,
    -1, -1, -1, -1, 60, -1, 109, 63, -1, 19, 25, -1, -1, -1, 93, -1, -1, 56, -1,
    81, 102, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, -1, -1, -1, -1, -1, -1,
    -1, -1, 71, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 99, -1, -1, 62, -1, -1, -1, 86, -1, -1, 105, 2, 77, 61, 20, -1, 76, 10,
    -1, -1, 96, -1, -1, 52, -1, -1, 1, -1, 7, 82, -1, -1, 11, -1, -1, -1, -1,
    -1, -1, 66, -1, -1, -1, -1, -1, -1, 68, -1, -1, -1, 107, 37, -1, 45, -1, 91,
    -1, 22, -1, -1, -1, -1, -1, 17, -1, -1, -1, 16, 73, -1, 41, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 48, -1, -1, 53, -1, -1, -1, 67, -1,
    -1, 38, -1, -1, -1, 101, -1, 79, -1, -1, 57, -1, 51, -1, -1, -1, 21, -1, 15,
    -1, -1, -1, -1, -1, 70, -1, -1, 84, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 75, -1, -1, -1, 14, -1, 39, -1, -1, -1, -1, -1, -1, -1, -1, 90,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 35, -1, -1, 50, -1, 30,
    -1, 26, 104, -1, -1, -1, -1, 108, -1, -1, -1, 29, -1, -1, -1, 69, 58, 31,
    -1, 12, -1, -1, -1, -1, -1, -1, 64, -1, -1, -1, -1, -1, 36, 80, -1, -1, -1,
    -1, 97, -1, 78, 4, -1, -1, -1, 23, -1, 42, -1, 98, -1, 8, 18, -1, -1, -1,
    -1, -1, -1, 72, -1
  );

{$Q-}
function TSynPHPSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 818 + Ord(Str^) * 366;
    Inc(Str);
  end;
  Result := Result mod 421;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynPHPSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynPHPSyn.InitIdent;
var
  Int: Integer;
begin
  for Int := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[Int] = -1 then
      fIdentFuncTable[Int] := AltFunc;

  for Int := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[Int] = nil then
      fIdentFuncTable[Int] := KeyWordFunc;
end;

function TSynPHPSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynPHPSyn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier
end;

constructor TSynPHPSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := False;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDocumentAttri := TSynHighlighterAttributes.Create(SYNS_AttrDocumentation, SYNS_FriendlyAttrDocumentation);
  fDocumentAttri.Style := [fsItalic];
  AddAttribute(fDocumentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_FriendlyAttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterPHP;
  fRange := rsUnknown;
end;

procedure TSynPHPSyn.AndSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {and assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '&':                               {conditional and}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {and}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.AtSymbolProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.BraceCloseProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.BraceOpenProc;
begin
  Inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
end;

procedure TSynPHPSyn.ColonProc;
begin
  Inc(Run);                            {colon - conditional}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.CommaProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.EqualProc;
begin
  case FLine[Run + 1] of
    '=':                               {logical equal}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {Hash operator}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {assign}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.GreaterProc;
begin
  case FLine[Run + 1] of
    '=':                               {greater than or equal to}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {greater than}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do Inc(Run);
end;

procedure TSynPHPSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynPHPSyn.LowerProc;
{$IFDEF SYN_HEREDOC}
var
  Int, Len : Integer;
{$ENDIF}
begin
  case FLine[Run + 1] of
    '=':                               {less than or equal to}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '<':
      begin
        fTokenID := tkSymbol;
{$IFDEF SYN_HEREDOC}
        if (FLine[Run + 2] = '<') and IsIdentChar(FLine[Run + 3]) then
        begin
          Inc(Run, 3);

          Int := Run;
          while IsIdentChar(FLine[Int]) do Inc(Int);
          Len := Int - Run;

          if Len > 255 then
          begin
            fTokenID := tkUnknown;
            Exit;
          end;

          fRange := rsHeredoc;
          fHeredocLength := Len;
          fHeredocChecksum := CalcFCS(FLine[Run], Len);

          Inc(Run, Len);
          fTokenID := tkString;
        end
        else
{$ENDIF}
        if FLine[Run + 2] = '=' then   {shift left assign}
        begin
          Inc(Run, 3)
        end
        else                           {shift left}
        begin
          Inc(Run, 2);
        end;
      end;
  else                                 {less than}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MinusProc;
begin
  case FLine[Run + 1] of
    '=':                               {subtract assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '-':                               {decrement}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>':                               {Class operator}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {subtract}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.MultiplyProc;
begin
  case FLine[Run + 1] of
    '=':                               {multiply assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {multiply}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NotSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {not equal}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {logical complement}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynPHPSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', '-', 'l', 'L', 'x', 'X', 'A'..'F', 'a'..'f':
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

procedure TSynPHPSyn.OrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {inclusive or assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '|':                               {conditional or}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {inclusive or}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PlusProc;
begin
  case FLine[Run + 1] of
    '=':                               {add assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '+':                               {increment}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {add}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.PointProc;
begin
  Inc(Run);                            {point}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.PoundProc;
begin
  repeat
    Inc(Run);
  until IsLineEnd(Run);
  fTokenID := tkComment;
end;

procedure TSynPHPSyn.QuestionProc;
begin
  fTokenID := tkSymbol;                {question mark - conditional}
  Inc(Run);
end;

procedure TSynPHPSyn.RemainderSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {remainder assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {remainder}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.RoundCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.RoundOpenProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

//-- CodeFolding
procedure TSynPHPSyn.ScanForFoldRanges(FoldRanges: TSynFoldRanges;
  LinesToScan: TStrings; FromLine, ToLine: Integer);
var
  CurLine: string;
  Line: Integer;

  function LineHasChar(Line: Integer; character: Char;
  StartCol : Integer): Boolean; // faster than Pos!
  var
    Int: Integer;
  begin
    Result := False;
    for Int := StartCol to Length(CurLine) do begin
      if CurLine[Int] = character then begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, Int) <> fCommentAttri then begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;

  function FindBraces(Line: Integer) : Boolean;
  var
    Col : Integer;
  begin
    Result := False;

    for Col := 1 to Length(CurLine) do
    begin
      // We've found a starting character
      if CurLine[col] = '{' then
      begin
        // Char must have proper highlighting (ignore stuff inside comments...)
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, Col) <> fCommentAttri then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '}', col + 1) then begin
            FoldRanges.StartFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          Break;
        end;
      end else if CurLine[col] = '}' then
      begin
        if GetHighlighterAttriAtRowCol(LinesToScan, Line, Col) <> fCommentAttri then
        begin
          // And ignore lines with both opening and closing chars in them
          if not LineHasChar(Line, '{', col + 1) then begin
            FoldRanges.StopFoldRange(Line + 1, 1);
            Result := True;
          end;
          // Skip until a newline
          Break;
        end;
      end;
    end; // for Col
  end;

  function FoldRegion(Line: Integer): Boolean;
  var
    Str : string;
  begin
    Result := False;
    Str := TrimLeft(CurLine);
    if UpperCase(Copy(Str, 1, 9)) = '//#REGION' then
    begin
      FoldRanges.StartFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end
    else if UpperCase(Copy(Str, 1, 12)) = '//#ENDREGION' then
    begin
      FoldRanges.StopFoldRange(Line + 1, FoldRegionType);
      Result := True;
    end;
  end;

begin
  for Line := FromLine to ToLine do
  begin
    // Deal first with Multiline comments (Fold Type 2)
    if TRangeState(GetLineRange(LinesToScan, Line)) in [rsComment, rsDocument] then
    begin
      if not (TRangeState(GetLineRange(LinesToScan, Line - 1)) in [rsComment, rsDocument]) then
        FoldRanges.StartFoldRange(Line + 1, 2)
      else
        FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end
    else if TRangeState(GetLineRange(LinesToScan, Line - 1)) in [rsComment, rsDocument] then
    begin
      FoldRanges.StopFoldRange(Line + 1, 2);
      Continue;
    end;

    CurLine := LinesToScan[Line];

    // Skip empty lines
    if CurLine = '' then begin
      FoldRanges.NoFoldInfo(Line + 1);
      Continue;
    end;

    // Find Fold regions
    if FoldRegion(Line) then
      Continue;

    // Find an braces on this line  (Fold Type 1)
    if not FindBraces(Line) then
      FoldRanges.NoFoldInfo(Line + 1);
  end; // while Line
end;
//-- CodeFolding

procedure TSynPHPSyn.SemiColonProc;
begin
  Inc(Run);                            {semicolon}
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SlashProc;
begin
  case FLine[Run + 1] of
    '/':                               {c++ style comments}
      begin
        Inc(Run, 2);
        fTokenID := tkComment;
        while not IsLineEnd(Run) do
          Inc(Run);
      end;
    '*':
      begin
        if (fLine[Run+2] = '*') and (fLine[Run+3] <> '/') then     {documentation comment}
        begin
          fRange := rsDocument;
          fTokenID := tkDocument;
          Inc(Run);
        end
        else                           {c style comment}
        begin
          fRange := rsComment;
          fTokenID := tkComment;
          Inc(Run);
        end;

        Inc(Run);
        while not IsLineEnd(Run) do
          if fLine[Run] = '*' then
          begin
            if fLine[Run + 1] = '/' then
            begin
              fRange := rsUnKnown;
              Inc(Run, 2);
              Break;
            end
            else
              Inc(Run)
          end
          else
            Inc(Run);
      end;
    '=':                               {division assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {division}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynPHPSyn.SquareCloseProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.SquareOpenProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPHPSyn.StringProc;

  function IsEscaped: Boolean;
  var
    iFirstSlashPos: Integer;
  begin
    iFirstSlashPos := Run -1;
    while (iFirstSlashPos > 0) and (FLine[iFirstSlashPos] = '\') do
      Dec(iFirstSlashPos);
    Result := (Run - iFirstSlashPos + 1) mod 2 <> 0;
  end;

var
  iCloseChar: WideChar;
begin
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;
  case fRange of
    rsString39: iCloseChar := #39;
    rsString34: iCloseChar := #34;
    rsString96: iCloseChar := '`';
  else
    iCloseChar := #0;
  end;
  while not IsLineEnd(Run) do
  begin
    if (FLine[Run] = iCloseChar) and not IsEscaped then
      Break;
    if (FLine[Run] = '$') and (iCloseChar = '"') and
      ((FLine[Run + 1] = '{') or IsIdentChar(FLine[Run + 1])) then
    begin
      if (Run > 1) and (FLine[Run -1] = '{') then { complex syntax }
        Dec(Run);
      if not IsEscaped then
      begin
        { break the token to process the variable }
        fRange := rsVarExpansion;
        Exit;
      end
      else if FLine[Run] = '{' then
        Inc(Run); { restore Run if we previously deincremented it }
    end;
    Inc(Run);
  end;
  if (FLine[Run] = iCloseChar) then
    fRange := rsUnKnown;
  if not IsLineEnd(Run) then Inc(Run);
end;

procedure TSynPHPSyn.VarExpansionProc;
type
  TExpansionSyntax = (esNormal, esComplex, esBrace);
var
  iSyntax: TExpansionSyntax;
  iOpenBraces: Integer;
  iOpenBrackets: Integer;
  iTempRun: Integer;
begin
  fRange := rsString34; { var expansion only occurs in double quoted strings }
  FTokenID := tkVariable;
  if FLine[Run] = '{' then
  begin
    iSyntax := esComplex;
    Inc(Run, 2); { skips '{$' }
  end
  else
  begin
    Inc( Run );
    if FLine[Run] = '{' then
    begin
      iSyntax := esBrace;
      Inc(Run);
    end
    else
      iSyntax := esNormal;
  end;
  if iSyntax in [esBrace, esComplex] then
  begin
    iOpenBraces := 1;
    while not IsLineEnd(Run) do
    begin
      if FLine[Run] = '}' then
      begin
        Dec(iOpenBraces);
        if iOpenBraces = 0 then
        begin
          Inc(Run);
          Break;
        end;
      end;
      if FLine[Run] = '{' then
        Inc(iOpenBraces);
      Inc(Run);
    end;
  end
  else
  begin
    while IsIdentChar(FLine[Run]) do
      Inc(Run);
    iOpenBrackets := 0;
    iTempRun := Run;
    { process arrays and objects }
    while not IsLineEnd(iTempRun) do
    begin
      if FLine[iTempRun] = '[' then
      begin
        Inc( iTempRun );
        if FLine[iTempRun] = #39 then
        begin
          Inc(iTempRun);
          while not IsLineEnd(iTempRun) and (FLine[iTempRun] <> #39) do
            Inc(iTempRun);
          if (FLine[iTempRun] = #39) and (fLine[iTempRun +1 ] = ']') then
          begin
            Inc(iTempRun, 2);
            Run := iTempRun;
            Continue;
          end
          else
            Break;
        end
        else
          Inc(iOpenBrackets);
      end
      else if (FLine[iTempRun] = '-') and (FLine[iTempRun +1] = '>') then
        Inc(iTempRun, 2)
      else
        Break;

      if not IsIdentChar(FLine[iTempRun]) then
        Break
      else
        repeat
          Inc(iTempRun);
        until not IsIdentChar(FLine[iTempRun]);

      while FLine[iTempRun] = ']' do
      begin
        if iOpenBrackets = 0 then
          Break;
        Dec(iOpenBrackets);
        Inc(iTempRun);
      end;
      if iOpenBrackets = 0 then
        Run := iTempRun;
    end;
  end;
end;

procedure TSynPHPSyn.TildeProc;
begin
  Inc(Run);                            {bitwise complement}
  fTokenId := tkSymbol;
end;

procedure TSynPHPSyn.VariableProc;
begin
(*
  fTokenID := tkVariable;
  inc(Run);
  while IsIdentChar(fLine[Run]) do inc(Run);
*)
{begin}
  if IsIdentChar(fLine[Run+1]) then
  begin
    Inc(Run);
    { checking function name }
    if fLine[Run-1] = '@' then
    begin
      fTokenID := IdentKind((fLine + Run));
      { isn't function, must be variable }
      if FTokenID = tkIdentifier then
        fTokenID := tkVariable;
    end
    { rest are variables }
    else
      fTokenID := tkVariable;
    while IsIdentChar(fLine[Run]) do
    begin
      Inc(Run);
    end;
  end
  else
  begin
    fTokenID := tkSymbol;
    Inc(Run);
  end;
{end}
end;

procedure TSynPHPSyn.XOrSymbolProc;
begin
  case FLine[Run + 1] of
    '=':                               {xor assign}
      begin
        Inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else                                 {xor}
    begin
      Inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPHPSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPHPSyn.AnsiCProc;
begin
  if fRange = rsComment then
    fTokenID := tkComment
  else
    fTokenID := tkDocument;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;
    #10:
      begin
        LFProc;
        Exit;
      end;
    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while not IsLineEnd(Run) do
    if FLine[Run] = '*' then
    begin
      if fLine[Run + 1] = '/' then
      begin
        Inc(Run, 2);
        fRange := rsUnKnown;
        Break;
      end
      else
        Inc(Run);
    end
    else
      Inc(Run);
end;

procedure TSynPHPSyn.String39Proc;
begin
  fRange := rsString39;
  Inc( Run );
  StringProc;
end;

procedure TSynPHPSyn.String34Proc;
begin
  fRange := rsString34;
  Inc( Run );
  StringProc;
end;

{$IFDEF SYN_HEREDOC}
procedure TSynPHPSyn.HeredocProc;

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
  Int: Integer;
begin
  if IsLineEnd(Run) and (fTokenPos = Run) then
  begin
    NextProcedure;
    Exit;
  end;
  fTokenID := tkString;

  if Run = 0 then
  begin
    Int := 0;

    while not (IsLineEnd(FLine[Int]) or (FLine[Int] = ';')) do
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

    if (CalcFCS(FLine[0], Int) = fHeredocChecksum) then
    begin
      fRange := rsUnknown;
      Run := Int;
      Exit;
    end;
  end;

  SkipToEOL;
end;
{$ENDIF}

procedure TSynPHPSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment, rsDocument: AnsiCProc;
    rsString39, rsString34: StringProc;
    rsVarExpansion: VarExpansionProc;
{$IFDEF SYN_HEREDOC}
    rsHeredoc: HeredocProc;
{$ENDIF}
    else
    begin
      fRange := rsUnknown;
      NextProcedure;
    end;
  end;

  // ensure that one call of Next is enough to reach next token
  if (fOldRun = Run) and not GetEol then Next;

  inherited;
end;

procedure TSynPHPSyn.NextProcedure;
begin
  case fLine[Run] of
    '&': AndSymbolProc;
    #39: String39Proc; // single quote
    '@': AtSymbolProc;
    '}': BraceCloseProc;
    '{': BraceOpenProc;
    #13: CRProc;
    ':': ColonProc;
    ',': CommaProc;
    '=': EqualProc;
    '>': GreaterProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    #10: LFProc;
    '<': LowerProc;
    '-': MinusProc;
    '*': MultiplyProc;
    '!': NotSymbolProc;
    #0: NullProc;
    '0'..'9': NumberProc;
    '|': OrSymbolProc;
    '+': PlusProc;
    '.', '\': PointProc;
    '#': PoundProc;
    '?': QuestionProc;
    '%': RemainderSymbolProc;
    ')': RoundCloseProc;
    '(': RoundOpenProc;
    ';': SemiColonProc;
    '/': SlashProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    ']': SquareCloseProc;
    '[': SquareOpenProc;
    #34: String34Proc; // double quote
    '`': String96Proc;
    '~': TildeProc;
    '$': VariableProc;
    '^': XOrSymbolProc;
    else UnknownProc;
  end;
end;

function TSynPHPSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynPHPSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynPHPSyn.GetRange: Pointer;
{$IFDEF SYN_HEREDOC}
var
  RangePointer: TRangePointer;
{$ENDIF}
begin
{$IFDEF SYN_HEREDOC}
  RangePointer.Range := Ord(fRange);
  RangePointer.Length := 0;
  RangePointer.Checksum := 0;
  if fRange = rsHeredoc then
  begin
    RangePointer.Length := fHeredocLength;
    RangePointer.Checksum := fHeredocChecksum;
  end;
  Result := RangePointer.Ptr;
{$ELSE}
  Result := Pointer(fRange);
{$ENDIF}
end;

function TSynPHPSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPHPSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDocument: Result := fDocumentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynPHPSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynPHPSyn.ResetRange;
begin
  fRange := rsUnknown;
{$IFDEF SYN_HEREDOC}
  fHeredocLength := 0;
  fHeredocChecksum := 0;
{$ENDIF}
end;

procedure TSynPHPSyn.SetRange(Value: Pointer);
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
  if fRange = rsHeredoc then
  begin
    fHeredocLength := RangePointer.Length;
    fHeredocChecksum := RangePointer.Checksum;
  end;
{$ELSE}
  fRange := TRangeState(Value);
{$ENDIF}
end;

function TSynPHPSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPHP;
end;

class function TSynPHPSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPHP;
end;

function TSynPHPSyn.GetSampleSource: string;
begin
  Result := '// Syntax highlighting'#13#10+
            'function printNumber()'#13#10+
            '{'#13#10+
            '  $number = 1234;'#13#10+
            '  print "The number is $number";'#13#10+
            '  for ($i = 0; $i <= $number; $i++)'#13#10+
            '  {'#13#10+
            '    $x++;'#13#10+
            '    $x--;'#13#10+
            '    $x += 1.0;'#13#10+
            '  }'#13#10+
            '}';

end;

class function TSynPHPSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangPHP;
end;

function TSynPHPSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  Result := IsCharAlphaNumeric(AChar) or CharInSet(AChar, ['_', '$']);
end;

function TSynPHPSyn.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  case AChar of
    #0..#32, '.', ',', ';', ':', '"', '''', '+', '`', '-', '^', '!', '?', '&',
    '@', '�', '%', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>',
    '=', '*', '/', '\', '|':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TSynPHPSyn.String96Proc;
begin
  fRange := rsString96;
  Inc( Run );
  StringProc;
end;

initialization
  RegisterPlaceableHighlighter(TSynPHPSyn);
end.
