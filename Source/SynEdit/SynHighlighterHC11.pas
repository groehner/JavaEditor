{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHC11.pas, released 2000-04-21.
The Original Code is based on the CIHC11Syn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Nils Springob.
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

$Id: SynHighlighterHC11.pas,v 1.13.2.5 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a 68HC11 Assembler Language syntax highlighter for SynEdit)
@author(Nils Springob <delphi.nils@crazy-idea.de>, converted to SynEdit by Bruno Mikkelsen <btm@scientist.com>)
@created(January 2000, converted to SynEdit April 21, 2000)
@lastmod(2000-06-23)
The SynHighlighterHC11 unit provides SynEdit with a 68HC11 Assembler (.asm) highlighter.
The highlighter supports all 68HC11 op codes.
Thanks to Martin Waldenburg, David Muir, Hideo Koiso and Nick Hoddinott.
}

unit SynHighlighterHC11;

{$I SynEdit.inc}

interface

uses
  Graphics,
  SynEditHighlighter,
  SynEditTypes,
  SynHighlighterHashEntries,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TkwKeyWordType = (kwNone, kwOperand, kwOperandOver, kwNoOperand);

  PHashListEntry = ^THashListEntry;
  THashListEntry = record
    Next: PHashListEntry;
    Token: string;
    Kind: TtkTokenKind;
    Op: Boolean;
  end;

  TSynHC11Syn = class(TSynCustomHighLighter)
  private
    FTokenID: TtkTokenKind;
    FKeyWordType: TkwKeyWordType;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: string; AKind: Integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure SymAsciiCharProc;
    procedure SymbolProc;
    procedure SymDollarProc;
    procedure SymCRProc;
    procedure SymIdentProc;
    procedure SymLFProc;
    procedure SymPercentProc;
    procedure SymNullProc;
    procedure SymNumberProc;
    procedure SymSpaceProc;
    procedure SymStarProc;
    procedure SymStringProc;
    procedure SymUnknownProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirecAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
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
  SynEditStrConst;

const
  { TODO: seems as if the Ansi version ignores the underscores and therfore
    highlights more KeyWords than this(=Unicode) version.
    Also the SampleSource uses EQU_ and EQU, so it isn't clear what is
    the correct syntax: with other without the underscores.
  }
  KeyWords: string = (
    'ABA,ABX,ABY,ADCA_,ADCB_,ADDA_,ADDB_,ADDD_,ANDA_,ANDB_,ASLA,ASLB,' +
    'ASL_,ASLD,ASRA,ASRB,ASR_,BCC_,BCLR_,BCS_,BEQ_,BGE_,BGT_,BHI_,BHS' +
    '_,BITA_,BITB_,BLE_,BLO_,BLS_,BLT_,BMI_,BNE_,BPL_,BRA_,BRCLR_,BRN' +
    '_,BRSET_,BSET_,BSR_,BVC_,BVS_,CBA,CLC,CLI,CLRA,CLRB,CLR_,CLV,CMP' +
    'A_,CMPB_,COMA,COMB,COM_,CPD_,CPX_,CPY_,DAA,DECA,DECB,DEC_,DES,DE' +
    'X,DEY,EORA_,EORB_,FDIV,IDIV,INCA,INCB,INC_,INS,INX,INY,JMP_,JSR_' +
    ',LDAA_,LDAB_,LDD_,LDS_,LDX_,LDY_,LSLA,LSLB,LSL_,LSLD,LSRA,LSRB,L' +
    'SR_,LSRD,MUL,NEGA,NEGB,NEG_,NOP,ORAA_,ORAB_,PSHA,PSHB,PSHX,PSHY,' +
    'PULA,PULB,PULX,PULY,ROLA,ROLB,ROL_,RORA,RORB,ROR_,RTI,RTS,SBA,SB' +
    'CA_,SBCB_,SEC,SEI,SEV,STAA_,STAB_,STD_,STOP,STS_,STX_,STY_,SUBA_' +
    ',SUBB_,SUBD_,SWI,TAB,TAP,TBA,TEST,' +
    'TPA,TSTA,TSTB,TST_,TSX,TSY,TXS,TYS,WAI,XGDX,XGDY,' + // end commands
    'FCC_,FCB_,BSZ_,FDB_' // codegenerating directives
  );

  Directives: string = (
    'EQU_,OPT_,PAGE,ORG_,RMB_,END'  // directives
  );

procedure TSynHC11Syn.DoAddKeyword(AKeyword: string; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynHC11Syn.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 1 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 28 + Ord(Str^) - Ord('0');
      '_': Result := 27;
      else Result := 0;
    end
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
{$IFOPT Q-}
    Result := 7 * Result + GetOrd;
{$ELSE}
    Result := (7 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
  end;
  Result := Result and $FF; // 255
  fStringLen := Str - fToIdent;
end;

function TSynHC11Syn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      Break
    else if Entry.KeywordLen = fStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        Exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSynHC11Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fKeywords := TSynHashEntryList.Create;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(fInvalidAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(fDirecAttri);
  SetAttributesOnChange(DefHighlightChange);

  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkDirective), Directives, IsIdentChar, DoAddKeyword);
  fDefaultFilter := SYNS_FilterAsm68HC11;
end; { Create }

destructor TSynHC11Syn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynHC11Syn.SymAsciiCharProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13:
      begin
        FKeyWordType:=kwNone;
        Break;
      end;
    end;
    Inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynHC11Syn.SymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynHC11Syn.SymDollarProc;
begin
  fTokenID := tkNumber;
  Inc(Run);
  while CharInSet(FLine[Run], ['0'..'9', 'A'..'F', 'a'..'f']) do
  begin
    Inc(Run);
  end;
end;

procedure TSynHC11Syn.SymCRProc;
begin
  fTokenID := tkSpace;
  FKeyWordType := kwNone;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynHC11Syn.SymIdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do Inc(Run);
end;

procedure TSynHC11Syn.SymLFProc;
begin
  FKeyWordType := kwNone;
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynHC11Syn.SymPercentProc;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while CharInSet(FLine[Run], ['0'..'1']) do
    Inc(Run);
end;

procedure TSynHC11Syn.SymNullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynHC11Syn.SymNumberProc;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while CharInSet(FLine[Run], ['0'..'9']) do
    Inc(Run);
end;

procedure TSynHC11Syn.SymSpaceProc;
begin
  Inc(Run);
  if FKeyWordType in [kwOperandOver, kwNoOperand] then
  begin
    FKeyWordType := kwNone;
    fTokenID := tkComment;
    while not IsLineEnd(Run) do
      Inc(Run);
  end
  else
  begin
    if FKeyWordType = kwOperand then
      FKeyWordType := kwOperandOver;
    fTokenID := tkSpace;
    while (fLine[Run] <= #32) and not IsLineEnd(Run) do
      Inc(Run);
  end;
end;

procedure TSynHC11Syn.SymStarProc;
begin
  Inc(Run);
  if FKeyWordType = kwOperandOver then
    fTokenID := tkSymbol
  else
  begin
    fTokenID := tkComment;
    while not IsLineEnd(Run) do
      Inc(Run);
  end;
end;

procedure TSynHC11Syn.SymStringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then Inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: Break;
    end;
    Inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynHC11Syn.SymUnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynHC11Syn.Next;
begin
  fTokenPos := Run;
  case fLine[Run] of
    #39: SymAsciiCharProc;
    '$': SymDollarProc;
    #13: SymCRProc;
    'A'..'Z', 'a'..'z', '_': SymIdentProc;
    #10: SymLFProc;
    '%': SymPercentProc;
    #0: SymNullProc;
    '0'..'9': SymNumberProc;
    #1..#9, #11, #12, #14..#32: SymSpaceProc;
    '*': SymStarProc;
    #34: SymStringProc;
    '#', ':', ',', ';', '(', ')': SymbolProc;
    else SymUnknownProc;
  end;
  inherited;
end;

function TSynHC11Syn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynHC11Syn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynHC11Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynHC11Syn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynHC11Syn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHC11Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterAsm68HC11;
end;

class function TSynHC11Syn.GetLanguageName: string;
begin
  Result := SYNS_Lang68HC11;
end;

function TSynHC11Syn.GetSampleSource: string;
begin
  Result :=
    '* TX.ASM'#13#10 +
    'MAINORG EQU_    $F800'#13#10 +
    '        ORG     $F800'#13#10 +
    'MAIN    EQU     *        ;Start assembling here'#13#10 +
    '        STAA    SCCR2'#13#10 +
    'loop:'#13#10 +
    '        LDAA    #$05'#13#10 +
    ' BRA loop    ;Do it again'#13#10 +
    ' ORG $FFFE   ;Reset vector interrupt setup'#13#10 +
    ' END';
end;

class function TSynHC11Syn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLang68HC11;
end;

initialization
  RegisterPlaceableHighlighter(TSynHC11Syn);
end.
