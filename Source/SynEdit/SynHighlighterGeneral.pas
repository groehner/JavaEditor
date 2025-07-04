{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterGeneral.pas, released 2000-04-07.
The Original Code is based on the mwGeneralSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions written by Martin Waldenburg are copyright 1999 Martin Waldenburg.
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

$Id: SynHighlighterGeneral.pas,v 1.12 2011/04/14 15:12:54 Egg Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a customizable highlighter for SynEdit)
@author(Martin Waldenburg, converted to SynEdit by Michael Hieke)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterGeneral unit provides a customizable highlighter for SynEdit.
}

unit SynHighlighterGeneral;

{$I SynEdit.inc}

interface

uses
  Windows,
  SynEditHighlighter,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkPreprocessor, tkSpace, tkString, tkSymbol, tkUnknown);

  TCommentStyle = (csAnsiStyle, csPasStyle, csCStyle, csAsmStyle, csBasStyle,
    csCPPStyle);
  TCommentStyles = set of TCommentStyle;

  TRangeState = (rsANil, rsAnsi, rsPasStyle, rsCStyle, rsUnKnown);

  TStringDelim = (sdSingleQuote, sdDoubleQuote, sdSingleAndDoubleQuote);

  TGetTokenAttributeEvent = procedure (attribute : TSynHighlighterAttributes) of object;

const
   cDefaultIdentChars = '_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                         'abcdefghijklmnopqrstuvwxyz';

type
  TSynGeneralSyn = class(TSynCustomHighlighter)
  private
    fIdentChars: string;
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPreprocessorAttri: TSynHighlighterAttributes;                         
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fComments: TCommentStyles;
    fStringDelim: TStringDelim;
    fDetectPreprocessor: Boolean;
    fOnGetTokenAttribute: TGetTokenAttributeEvent;
    FStringMultiLine : Boolean;
    procedure AsciiCharProc;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure SetKeyWords(const Value: TStrings);
    procedure SetComments(Value: TCommentStyles);
    function GetStringDelim: TStringDelim;
    procedure SetStringDelim(const Value: TStringDelim);
    function GetIdentifierChars: string;
    procedure SetIdentifierChars(const Value: string);
    function StoreIdentChars : Boolean;
    procedure SetDetectPreprocessor(Value: Boolean);
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
    function IsStringDelim(aChar : WideChar) : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetCharBeforeToken(offset : Integer = -1) : WideChar;
    function GetCharAfterToken(offset : Integer = 1) : WideChar;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    function IsKeyword(const AKeyword: string): Boolean; override;
    function IsWordBreakChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; override;
    property OnGetTokenAttribute : TGetTokenAttributeEvent read fOnGetTokenAttribute write fOnGetTokenAttribute;
    property StringMultiLine : Boolean read FStringMultiLine write FStringMultiLine;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property Comments: TCommentStyles read fComments write SetComments default [];
    property DetectPreprocessor: Boolean read fDetectPreprocessor
      write SetDetectPreprocessor;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property IdentifierChars: string read GetIdentifierChars
      write SetIdentifierChars stored StoreIdentChars;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property KeyWords: TStrings read fKeyWords write SetKeyWords;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PreprocessorAttri: TSynHighlighterAttributes
      read fPreprocessorAttri write fPreprocessorAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property StringDelim: TStringDelim read GetStringDelim write SetStringDelim
      default sdSingleQuote;
  end;

implementation

uses
  SysUtils,
  Graphics,
  SynEditStrConst;

function TSynGeneralSyn.IsIdentChar(AChar: WideChar): Boolean;
var
  Int: Integer;
begin
  Result := False;
  for Int := 1 to Length(fIdentChars) do
    if AChar = fIdentChars[Int] then
    begin
      Result := True;
      Exit;
    end;
end;

function TSynGeneralSyn.IsKeyword(const AKeyword: string): Boolean;
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
    Compare := CompareText(fKeywords[Int], Token);
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

function TSynGeneralSyn.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  Result := inherited IsWordBreakChar(AChar) and not IsIdentChar(AChar);
end;

constructor TSynGeneralSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
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
  fPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  AddAttribute(fPreprocessorAttri);
  SetAttributesOnChange(DefHighlightChange);

  fStringDelim := sdSingleQuote;
  fIdentChars := cDefaultIdentChars;
  fRange := rsUnknown;
end; { Create }

destructor TSynGeneralSyn.Destroy;
begin
  fKeyWords.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynGeneralSyn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then
      begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynGeneralSyn.PasStyleProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if fLine[Run] = '}' then
      begin
        fRange := rsUnKnown;
        Inc(Run);
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynGeneralSyn.CStyleProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
      begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        Break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynGeneralSyn.AsciiCharProc;
begin
  if fDetectPreprocessor then
  begin
    fTokenID := tkPreprocessor;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
  begin
    fTokenID := tkString;
    repeat
      Inc(Run);
    until not CharInSet(fLine[Run], ['0'..'9']);
  end;
end;

procedure TSynGeneralSyn.BraceOpenProc;
begin
  if csPasStyle in fComments then
  begin
    fTokenID := tkComment;
    fRange := rsPasStyle;
    Inc(Run);
    while FLine[Run] <> #0 do
      case FLine[Run] of
        '}':
          begin
            fRange := rsUnKnown;
            Inc(Run);
            Break;
          end;
        #10: Break;

        #13: Break;
      else
        Inc(Run);
      end;
  end
  else
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynGeneralSyn.PointCommaProc;
begin
  if (csASmStyle in fComments) or (csBasStyle in fComments) then
  begin
    fTokenID := tkComment;
    fRange := rsUnknown;
    Inc(Run);
    while FLine[Run] <> #0 do
    begin
      fTokenID := tkComment;
      Inc(Run);
    end;
  end
  else
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynGeneralSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynGeneralSyn.IdentProc;
begin
  while IsIdentChar(fLine[Run]) do Inc(Run);
  if IsKeyWord(GetToken) then
    fTokenId := tkKey
  else
    fTokenId := tkIdentifier;
end;

procedure TSynGeneralSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsIntegerChar do Inc(Run);
end;

procedure TSynGeneralSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynGeneralSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynGeneralSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E', 'x':
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
      'x': begin // handle C style hex numbers
             IntegerProc;
             Break;
           end;
      '.':
        if FLine[Run + 1] = '.' then Break;
    end;
    Inc(Run);
  end;
end;

procedure TSynGeneralSyn.RoundOpenProc;
begin
  Inc(Run);
  if csAnsiStyle in fComments then
  begin
    case fLine[Run] of
      '*':
        begin
          fTokenID := tkComment;
          fRange := rsAnsi;
          Inc(Run);
          while fLine[Run] <> #0 do
            case fLine[Run] of
              '*':
                if fLine[Run + 1] = ')' then
                begin
                  fRange := rsUnKnown;
                  Inc(Run, 2);
                  Break;
                end else Inc(Run);
              #10: Break;
              #13: Break;
            else Inc(Run);
            end;
        end;
      '.':
        begin
          Inc(Run);
          fTokenID := tkSymbol;
        end;
    else
      begin
        FTokenID := tkSymbol;
      end;
    end;
  end else fTokenId := tkSymbol;
end;

procedure TSynGeneralSyn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '/':
      begin
        if csCPPStyle in fComments then
        begin
          fTokenID := tkComment;
          Inc(Run);
          while FLine[Run] <> #0 do
          begin
            case FLine[Run] of
              #10, #13: Break;
            end;
            Inc(Run);
          end;
        end
        else
          fTokenId := tkSymbol;
      end;
    '*':
      begin
        if csCStyle in fComments then
        begin
          fTokenID := tkComment;
          fRange := rsCStyle;
          Inc(Run);
          while fLine[Run] <> #0 do
            case fLine[Run] of
              '*':
                if fLine[Run + 1] = '/' then
                begin
                  fRange := rsUnKnown;
                  Inc(Run, 2);
                  Break;
                end else Inc(Run);
              #10, #13:
                Break;
              else
                Inc(Run);
            end;
        end
        else
          fTokenId := tkSymbol;
      end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynGeneralSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynGeneralSyn.StringProc;
var
   delim : WideChar;
begin
  fTokenID := tkString;
  if IsStringDelim(fLine[Run + 1]) and IsStringDelim(fLine[Run + 2]) then
    Inc(Run, 2);
  delim:=fLine[Run];
  repeat
    case FLine[Run] of
      #0 :  Break;
      #10, #13: if not StringMultiLine then Break;
    end;
    Inc(Run);
  until FLine[Run] = delim;
  if FLine[Run] <> #0 then Inc(Run);
end;

procedure TSynGeneralSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynGeneralSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
  else
    if IsStringDelim(fLine[Run]) then
      StringProc
    else
      case fLine[Run] of
        '#': AsciiCharProc;
        '{': BraceOpenProc;
        ';': PointCommaProc;
        #13: CRProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '$': IntegerProc;
        #10: LFProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        '(': RoundOpenProc;
        '/': SlashProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynGeneralSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
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

function TSynGeneralSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynGeneralSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynGeneralSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

// GetCharBeforeToken
//
function TSynGeneralSyn.GetCharBeforeToken(offset : Integer = -1) : WideChar;
begin
   if fTokenPos+offset>=0 then
      Result:=FLine[fTokenPos+offset]
   else Result:=#0;
end;

// GetCharAfterToken
//
function TSynGeneralSyn.GetCharAfterToken(offset : Integer = 1) : WideChar;
begin
   Result:=FLine[fTokenPos+offset];
end;

function TSynGeneralSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPreprocessor: Result := fPreprocessorAttri;                         
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
  if Assigned(fOnGetTokenAttribute) then
    fOnGetTokenAttribute(Result);
end;

function TSynGeneralSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynGeneralSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynGeneralSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynGeneralSyn.SetKeyWords(const Value: TStrings);
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

  TStringList(fKeyWords).Sorted:=False;
  fKeyWords.Assign(Value);
  TStringList(fKeyWords).Sorted:=True;

  DefHighLightChange(nil);
end;

procedure TSynGeneralSyn.SetComments(Value: TCommentStyles);
begin
  if fComments <> Value then
  begin
    fComments := Value;
    DefHighLightChange(Self);
  end;
end;

class function TSynGeneralSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneral;
end;

function TSynGeneralSyn.LoadFromRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r:= TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then begin
      if r.ValueExists('KeyWords') then KeyWords.Text:= r.ReadString('KeyWords');
      Result := inherited LoadFromRegistry(RootKey, Key);
    end
    else Result := False;
  finally r.Free; end;
end;

function TSynGeneralSyn.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
begin
  r:= TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,True) then
    begin
      r.WriteString('KeyWords', KeyWords.Text);
      Result := inherited SaveToRegistry(RootKey, Key);
    end
    else Result := False;
  finally r.Free; end;
end;

function TSynGeneralSyn.GetStringDelim: TStringDelim;
begin
  Result:=fStringDelim;
end;

procedure TSynGeneralSyn.SetStringDelim(const Value: TStringDelim);
begin
   fStringDelim:=Value;
end;

function TSynGeneralSyn.GetIdentifierChars: string;
begin
  Result := fIdentChars;
end;

procedure TSynGeneralSyn.SetIdentifierChars(const Value: string);
begin
  fIdentChars := Value;
end;

function TSynGeneralSyn.StoreIdentChars : Boolean;
begin
   Result := (fIdentChars<>cDefaultIdentChars);
end;

procedure TSynGeneralSyn.SetDetectPreprocessor(Value: Boolean);
begin
  if Value <> fDetectPreprocessor then
  begin
    fDetectPreprocessor := Value;
    DefHighlightChange(Self);
  end;
end;

class function TSynGeneralSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangGeneral;
end;

// IsStringDelim
//
function TSynGeneralSyn.IsStringDelim(aChar : WideChar) : Boolean;
begin
   case fStringDelim of
      sdSingleQuote : Result:=(aChar='''');
      sdDoubleQuote : Result:=(aChar='"');
   else
      Result:=(aChar='''') or (aChar='"');
   end;
end;

initialization
  RegisterPlaceableHighlighter(TSynGeneralSyn);
end.
