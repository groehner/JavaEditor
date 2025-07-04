{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterMulti.pas, released 2000-06-23.
The Original Code is based on mwMultiSyn.pas by Willo van der Merwe, part of the
mwEdit component suite.
Unicode translation by Ma�l H�rz.

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

$Id: SynHighlighterMulti.pas,v 1.34.2.11 2008/09/14 16:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Multiple-highlighter syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1999, converted to SynEdit 2000-06-23)
@lastmod(2000-06-23)
The SynHighlighterMulti unit provides SynEdit with a multiple-highlighter syntax highlighter.
This highlighter can be used to highlight text in which several languages are present, such as HTML.
For example, in HTML as well as HTML tags there can also be JavaScript and/or VBScript present.
}

unit SynHighlighterMulti;

{$I SynEdit.inc}

interface

uses
  Windows,
  Classes,
  SynEditHighlighter;

type
  TOnCheckMarker = procedure (Sender: TObject; var StartPos, MarkerLen: Integer;
    var MarkerText: string; Line: Integer; const LineStr: string) of object;

  TScheme = class(TCollectionItem)
  private
    fEndExpr: string;
    fStartExpr: string;
    fHighlighter: TSynCustomHighLighter;
    fMarkerAttri: TSynHighlighterAttributes;
    fSchemeName: TComponentName;
    fCaseSensitive: Boolean;
    fOnCheckStartMarker: TOnCheckMarker;
    fOnCheckEndMarker: TOnCheckMarker;
    function ConvertExpression(const Value: string): string;
    procedure MarkerAttriChanged(Sender: TObject);
    procedure SetMarkerAttri(const Value: TSynHighlighterAttributes);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetEndExpr(const Value: string);
    procedure SetStartExpr(const Value: string);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive
      default True;
    property StartExpr: string read fStartExpr write SetStartExpr;
    property EndExpr: string read fEndExpr write SetEndExpr;
    property Highlighter: TSynCustomHighlighter read fHighlighter
      write SetHighlighter;
    property MarkerAttri: TSynHighlighterAttributes read fMarkerAttri
      write SetMarkerAttri;
    property SchemeName: TComponentName read fSchemeName write fSchemeName;
    property OnCheckStartMarker: TOnCheckMarker read fOnCheckStartMarker write fOnCheckStartMarker;
    property OnCheckEndMarker: TOnCheckMarker read fOnCheckEndMarker write fOnCheckEndMarker;
  end;

  TgmSchemeClass = class of TScheme;

  TSynMultiSyn = class;

  TSchemes = class(TCollection)
  private
    fOwner: TSynMultiSyn;
    function GetItems(Index: Integer): TScheme;
    procedure SetItems(Index: Integer; const Value: TScheme);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aOwner: TSynMultiSyn);
    property Items[aIndex: Integer]: TScheme read GetItems write SetItems;
      default;
  end;

  TMarker = class
  protected
    fScheme: Integer;
    fStartPos: Integer;
    fMarkerLen: Integer;
    fMarkerText: string;
    fIsOpenMarker: Boolean;
  public
    constructor Create(aScheme, aStartPos, aMarkerLen: Integer;
      aIsOpenMarker: Boolean; const aMarkerText: string);
  end;


  TRangeOperation = (roGet, roSet);

  TRangeUNativeInt = NativeUInt;
  TRangeProc = procedure (Operation: TRangeOperation; var Range: TRangeUNativeInt) of object;

  TCustomRangeEvent = procedure (Sender: TSynMultiSyn; Operation: TRangeOperation;
    var Range: pointer) of object;

  {
  * Usage notes *
    If you don't need to nest MultiSyns as Schemes, just as DefaultHighlighter,
  you can nest up to 2 MultiSyns, each of them containing up to 7 Schemes. This
  is the way MultiSyn works best. (implemented in NewRangeProc)
    If you need to use a MultiSyn nested as Scheme, then you can nest up to
  5 MultiSyns, but Ranges aren't persisted across occurrences of Schemes that
  have multiple lines. (implemented in OldRangeProc)
    Clarification: when I say "you can nest up to X" MultiSyns, I mean having
  X+1 levels of MultiSyns.

  MultiSyn doesn't work by default with dynamic highlighters; you must use
  OnCustomRange. This is because dynamic highlighters' Ranges are pointers,
  but MultiSyn needs Ranges to be ordinal values smaller than 16 (4 bits).

  OnCustomRange:
    When Operation is roGet, user should store in the 'Range' parameter the
    information to allow restoring the current state of the highlighter.
    When Operation is roSet, user should restore highlighter state (CurrScheme,
    DefaultHighlighter.Range and, if the case, Schemes[CurrScheme].Range)
    according to 'Range' value.
  CurrScheme:
    Index of the scheme that is currently parsing. DefaultHighlighter maps to -1.

  * Implementation notes *
  fTmpRange:
    Using the OldRangeProc, fTmpRange was the only way to restore the Range
    of the DefaultHighlighter after a Scheme spanned across multiple lines.
    With the NewRangeProc, the only use for it is restoring DefaultHighLighter's
    Range in case a nested MultiSyn uses the highlighter too.
  }

  TSynMultiSyn = class(TSynCustomHighlighter)
  private
    fRangeProc: TRangeProc;
    fDefaultLanguageName: string;
    fMarkers: TList;
    fMarker: TMarker;
    fNextMarker: Integer;
    fCurrScheme: Integer;
    fTmpRange: pointer;
    fOnCustomRange: TCustomRangeEvent;
    fLineStr: string;
    procedure SetDefaultHighlighter(const Value: TSynCustomHighLighter);
    function GetMarkers(Index: Integer): TMarker;
    property Markers[Index: Integer]: TMarker read GetMarkers;
    procedure DoCheckMarker(Scheme:TScheme; StartPos, MarkerLen: Integer;
      const MarkerText: string; Start: Boolean; Line: Integer;
      const LineStr: string);
    procedure SetOnCustomRange(const Value: TCustomRangeEvent);
  protected
    fSchemes: TSchemes;
    fDefaultHighlighter: TSynCustomHighLighter;
    fLineNumber: Integer;
    fSampleSource: string;
    procedure Loaded; override;
    procedure SetSchemes(const Value: TSchemes);
    procedure ClearMarkers;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetAttribCount: Integer; override;
    function GetAttribute(Index: Integer): TSynHighlighterAttributes; override;
    procedure HookHighlighter(aHL: TSynCustomHighlighter);
    procedure UnhookHighlighter(aHL: TSynCustomHighlighter);
    procedure Notification(aComp: TComponent; aOp: TOperation); override;
    function GetSampleSource: string; override;
    procedure SetSampleSource(Value: string); override;
    procedure DoSetLine(const Value: string; LineNumber: Integer); override;
    //
    procedure OldRangeProc(Operation: TRangeOperation; var Range: TRangeUNativeInt);
    procedure NewRangeProc(Operation: TRangeOperation; var Range: TRangeUNativeInt);
    procedure UserRangeProc(Operation: TRangeOperation; var Range: TRangeUNativeInt);
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetEol: Boolean; override;
    function GetExpandedToken: string; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function UpdateRangeProcs: Boolean;
    property CurrScheme: Integer read fCurrScheme write fCurrScheme;
    property CurrLine: string read fLineStr;
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
  published
    property Schemes: TSchemes read fSchemes write SetSchemes;
    property DefaultHighlighter: TSynCustomHighLighter read fDefaultHighlighter
      write SetDefaultHighlighter;
    property DefaultLanguageName: string read fDefaultLanguageName
      write fDefaultLanguageName;
    property OnCustomRange: TCustomRangeEvent read fOnCustomRange write SetOnCustomRange;
  end;

implementation

uses
  Graphics,
  SynEditStrConst,
  RegularExpressions,
  SysUtils;

procedure CheckExpression(const Expr: string);
var
  Parser: TRegEx;
begin
  // will raise an exception if the expression is incorrect
  Parser := TRegEx.Create(Expr, [roNotEmpty, roCompiled]);
end;

{ TMarker }

constructor TMarker.Create(aScheme, aStartPos,
  aMarkerLen: Integer; aIsOpenMarker: Boolean; const aMarkerText: string);
begin
  fScheme := aScheme;
  fStartPos := aStartPos;
  fMarkerLen := aMarkerLen;
  fIsOpenMarker := aIsOpenMarker;
  fMarkerText := aMarkerText;
end;

{ TSynMultiSyn }

procedure TSynMultiSyn.ClearMarkers;
var
  Int: Integer;
begin
  for Int := 0 to fMarkers.Count - 1 do
    TObject(fMarkers[Int]).Free;
  fMarkers.Clear;
end;

constructor TSynMultiSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSchemes := TSchemes.Create(Self);
  fCurrScheme := -1;
  fMarkers := TList.Create;
  fRangeProc := NewRangeProc;
end;

destructor TSynMultiSyn.Destroy;
begin
  ClearMarkers;
  { unhook notification handlers }
  Schemes.Clear;
  DefaultHighlighter := nil;
  inherited Destroy;
  fSchemes.Free;
  fMarkers.Free;
end;

function TSynMultiSyn.GetAttribCount: Integer;
var
  Int: Integer;
begin
  Result := Schemes.Count;
  if DefaultHighlighter <> nil then
    Inc(Result, DefaultHighlighter.AttrCount);
  for Int := 0 to Schemes.Count - 1 do
    if Schemes[Int].Highlighter <> nil then
      Inc(Result, Schemes[Int].Highlighter.AttrCount);
end;

function TSynMultiSyn.GetAttribute(Index: Integer): TSynHighlighterAttributes;
var
  Int: Integer;
  HL: TSynCustomHighlighter;
begin
  if Index < Schemes.Count then
    Result := Schemes[Index].MarkerAttri
  else
  begin
    Dec(Index, Schemes.Count);
    if DefaultHighlighter <> nil then
      if Index < DefaultHighlighter.AttrCount then
      begin
        Result := DefaultHighlighter.Attribute[Index];
        Exit;
      end
      else
        Dec(Index, DefaultHighlighter.AttrCount);
    for Int := 0 to Schemes.Count - 1 do
    begin
      HL := Schemes[Int].Highlighter;
      if HL <> nil then
        if Index < HL.AttrCount then
        begin
          Result := HL.Attribute[Index];
          Exit;
        end
        else
          Dec(Index, HL.AttrCount);
    end;
    Result := nil;
  end;
end;

function TSynMultiSyn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
var
  HL: TSynCustomHighlighter;
begin
  if (CurrScheme >= 0) and (Schemes[CurrScheme].Highlighter <> nil) then
    HL := Schemes[CurrScheme].Highlighter
  else
    HL := DefaultHighlighter;
  { the typecast to TSynMultiSyn is only necessary because the
  GetDefaultAttribute method is protected.
  And don't worry: this really works }
  if HL <> nil then
    Result := TSynMultiSyn(HL).GetDefaultAttribute(Index)
  else
    Result := nil;
end;

function TSynMultiSyn.GetEol: Boolean;
begin
  if fMarker <> nil then
    Result := False
  else if fCurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.GetEol
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetEol
  else
    Result := Run > fLineLen + 1;
end;

class function TSynMultiSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneralMulti;
end;

function TSynMultiSyn.GetMarkers(Index: Integer): TMarker;
begin
  Result := TMarker(fMarkers[Index]);
end;

procedure TSynMultiSyn.OldRangeProc(Operation: TRangeOperation; var Range: TRangeUNativeInt);
const
  MaxNestedMultiSyn = 6;
  { number of bits of the Range that will be used to store the SchemeIndex }
  SchemeIndexSize = 4;
  MaxSchemeCount = (1 shl SchemeIndexSize) - 1;
  { number of bits of the Range that will be used to store the SchemeRange }
  SchemeRangeSize = 8;
  MaxSchemeRange = (1 shl SchemeRangeSize) - 1;
var
  iHL: TSynCustomHighlighter;
  iSchemeIndex: cardinal;
  iSchemeRange: cardinal;
begin
  if Operation = roGet then
  begin
    if (fCurrScheme < 0) then
      iHL := DefaultHighlighter
    else
      iHL := Schemes[fCurrScheme].Highlighter;
    iSchemeIndex := fCurrScheme + 2;
    Assert(iSchemeIndex <= MaxSchemeCount);
    if iHL <> nil then
    begin
      iSchemeRange := cardinal(iHL.GetRange);
      Assert((iSchemeRange <= MaxSchemeRange) or (iHL is TSynMultiSyn));
    end
    else
      iSchemeRange := 0;
    { checks the limit of nested MultiSyns }
    Assert(iSchemeRange shr ((MaxNestedMultiSyn - 1) * SchemeIndexSize + SchemeRangeSize) = 0);
    iSchemeRange := (iSchemeRange shl SchemeIndexSize) or iSchemeIndex;
    Range := iSchemeRange;
  end
  else
  begin
    if Range = 0 then
      Exit;
    iSchemeRange := cardinal(Range);
    fCurrScheme := Integer(iSchemeRange and MaxSchemeCount) - 2;
    iSchemeRange := iSchemeRange shr SchemeIndexSize;
    if (CurrScheme < 0) then
    begin
      if DefaultHighlighter <> nil then
        DefaultHighlighter.SetRange(pointer(iSchemeRange));
    end
    else
      Schemes[CurrScheme].Highlighter.SetRange(pointer(iSchemeRange));
  end;
end;

function TSynMultiSyn.GetToken: string;
begin
  if DefaultHighlighter = nil then
    Result := fLineStr
  else
    Result := inherited GetToken;
end;

function TSynMultiSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if fMarker <> nil then
    Result := Schemes[fMarker.fScheme].MarkerAttri
  else if CurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.GetTokenAttribute
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenAttribute
  else
    Result := nil;
end;

function TSynMultiSyn.GetTokenKind: Integer;
begin
  if fMarker <> nil then
    Result := 0
  else if fCurrScheme >= 0 then
    Result := Schemes[fCurrScheme].Highlighter.GetTokenKind
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenKind
  else
    Result := 0;
end;

procedure TSynMultiSyn.HookHighlighter(aHL: TSynCustomHighlighter);
begin
  aHL.FreeNotification(Self);
  aHL.HookAttrChangeEvent(DefHighlightChange);
end;

procedure TSynMultiSyn.Next;
var
  iToken, TmpLine, ExpandedTmpLine: string;
  iHL: TSynCustomHighlighter;
begin
  if DefaultHighlighter = nil then
  begin
    if Run > 0 then
      Inc(Run)
    else
      Run := Length(fLineStr) + 1;
    inherited;
    Exit;
  end;

  if (fNextMarker < fMarkers.Count) and (Run + 1 >= Markers[fNextMarker].fStartPos) then
  begin
    fMarker := Markers[fNextMarker];
    if fMarker.fIsOpenMarker then
    begin
      fCurrScheme := fMarker.fScheme;
      fTmpRange := DefaultHighlighter.GetRange;
      Schemes[CurrScheme].Highlighter.ResetRange;
    end;
    Inc(fNextMarker);
    fTokenPos := Run;
    Inc(Run, fMarker.fMarkerLen);
    inherited;
    Exit;
  end;

  if Run = 0 then
  begin
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;

    if fMarkers.Count = 0 then
      TmpLine := fLineStr
    else
      TmpLine := Copy(fLineStr, 1, Markers[fNextMarker].fStartPos - 1);
      
    if fExpandedLine <> nil then
    begin
      if fMarkers.Count = 0 then
        ExpandedTmpLine := fExpandedLineStr
      else
        ExpandedTmpLine := Copy(fExpandedLineStr, 1,
          PosToExpandedPos(Markers[fNextMarker].fStartPos - 1));
      iHL.SetLineExpandedAtWideGlyphs(TmpLine, ExpandedTmpLine, fLineNumber);
    end
    else
      iHL.SetLine(TmpLine, fLineNumber);
  end
  else if fMarker <> nil then
  begin
    if not fMarker.fIsOpenMarker then
    begin
      fCurrScheme := -1;
      DefaultHighlighter.SetRange(fTmpRange);
    end;
    fMarker := nil;

    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;

    if fNextMarker < fMarkers.Count then
      TmpLine := Copy(fLineStr, Run + 1, Markers[fNextMarker].fStartPos - Run - 1)
    else
      TmpLine := Copy(fLineStr, Run + 1, MaxInt);

    if fExpandedLine <> nil then
    begin
      if fNextMarker < fMarkers.Count then
        ExpandedTmpLine := Copy(fExpandedLineStr, ExpandedRun + 1,
          PosToExpandedPos(Markers[fNextMarker].fStartPos - Run - 1))
      else
        ExpandedTmpLine := Copy(fExpandedLineStr, ExpandedRun + 1, MaxInt);

      iHL.SetLineExpandedAtWideGlyphs(TmpLine, ExpandedTmpLine, fLineNumber);
    end
    else
      iHL.SetLine(TmpLine, fLineNumber);
  end
  else
  begin
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;
    iHL.Next;
  end;

  fTokenPos := iHL.GetTokenPos;
  iToken := iHL.GetToken;
  if fNextMarker > 0 then
    with Markers[fNextMarker - 1] do
      Inc(fTokenPos, fStartPos + fMarkerLen - 1);
  Inc(Run, (fTokenPos - Run) + Length(iToken));
  inherited;
end;

procedure TSynMultiSyn.Notification(aComp: TComponent; aOp: TOperation);
var
  Int: Integer;
begin
  inherited;
  // 'opRemove' doesn't mean the component is being destroyed. It means it's
  // being removed from its Owner's list of Components.
  if (aOp = opRemove) and (aComp is TSynCustomHighlighter) and
    (csDestroying in aComp.ComponentState) then
  begin
    if DefaultHighlighter = aComp then
      DefaultHighlighter := nil;
    for Int := 0 to Schemes.Count - 1 do
      if Schemes[Int].Highlighter = aComp then
        Schemes[Int].Highlighter := nil;
  end;
end;

procedure TSynMultiSyn.ResetRange;
begin
  fCurrScheme := -1;
  if DefaultHighlighter <> nil then
  begin
    DefaultHighlighter.ResetRange;
    fTmpRange := DefaultHighlighter.GetRange;
  end;
end;

procedure TSynMultiSyn.SetDefaultHighlighter(
  const Value: TSynCustomHighLighter);
const
  sDefaultHlSetToSelf = 'A SynMultiSyn cannot be its own DefaultHighlighter.';
begin
  if DefaultHighlighter <> Value then begin
    if Value = Self then
      raise Exception.Create(sDefaultHlSetToSelf);
    if DefaultHighlighter <> nil then
      UnhookHighlighter(DefaultHighlighter);
    fDefaultHighlighter := Value;
    if DefaultHighlighter <> nil then
      HookHighlighter(DefaultHighlighter);
    DefHighlightChange(Self);
  end;
end;

procedure TSynMultiSyn.DoCheckMarker(Scheme:TScheme; StartPos, MarkerLen: Integer;
  const MarkerText: string; Start: Boolean; Line: Integer;
  const LineStr: string);
var
  aStartPos: Integer;
  aMarkerLen: Integer;
  aMarkerText: string;
begin
  aStartPos := StartPos;
  aMarkerLen := MarkerLen;
  aMarkerText := MarkerText;
  if Start and Assigned(Scheme.OnCheckStartMarker) then
    Scheme.OnCheckStartMarker(Self, aStartPos, aMarkerLen, aMarkerText, Line, LineStr)
  else if not Start and Assigned(Scheme.OnCheckEndMarker) then
    Scheme.OnCheckEndMarker(Self, aStartPos, aMarkerLen, aMarkerText, Line, LineStr);
  if (aMarkerText <> '') and (aMarkerLen > 0) then
  begin
    fMarkers.Add(TMarker.Create(Scheme.Index, aStartPos, aMarkerLen, Start,
      aMarkerText));
  end;
end;

procedure TSynMultiSyn.SetSchemes(const Value: TSchemes);
begin
  fSchemes.Assign(Value);
end;

procedure TSynMultiSyn.UnhookHighlighter(aHL: TSynCustomHighlighter);
begin
  aHL.UnhookAttrChangeEvent(DefHighlightChange);
  aHL.RemoveFreeNotification(Self);
end;

function TSynMultiSyn.GetSampleSource: string;
begin
  Result := fSampleSource;
end;

procedure TSynMultiSyn.SetSampleSource(Value: string);
begin
  fSampleSource := Value;
end;

function TSynMultiSyn.LoadFromRegistry(RootKey: HKEY;
  Key: string): Boolean;
var
  r: TBetterRegistry;
  Int: Integer;
begin
  if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.LoadFromRegistry(RootKey, Key + '\DefaultHighlighter')
  else
    Result := False;
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    for Int := 0 to Schemes.Count-1 do
      if (Schemes[Int].SchemeName <> '') and
        r.OpenKeyReadOnly(Key + '\' + Schemes[Int].SchemeName) then
      begin
        Result := Schemes[Int].MarkerAttri.LoadFromRegistry(r) and Result;
        r.CloseKey;
        Result := (Schemes[Int].Highlighter <> nil) and
          Schemes[Int].Highlighter.LoadFromRegistry(RootKey,
          Key + '\' + Schemes[Int].SchemeName) and Result;
      end
      else
        Result := False;
  finally
    r.Free;
  end;
end;

function TSynMultiSyn.SaveToRegistry(RootKey: HKEY; Key: string): Boolean;
var
  r: TBetterRegistry;
  Int: Integer;
begin
  if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.SaveToRegistry(RootKey, Key + '\DefaultHighlighter')
  else
    Result := False;
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    for Int := 0 to Schemes.Count-1 do
      if (Schemes[Int].SchemeName <> '') and
        r.OpenKey(Key + '\' + Schemes[Int].SchemeName, True) then
      begin
        Result := Schemes[Int].MarkerAttri.SaveToRegistry(r) and Result;
        r.CloseKey;
        Result := (Schemes[Int].Highlighter <> nil) and
          Schemes[Int].Highlighter.SaveToRegistry(RootKey,
          Key + '\' + Schemes[Int].SchemeName) and Result;
      end
      else
        Result := False;
  finally
    r.Free;
  end;
end;

function TSynMultiSyn.GetRange: Pointer;
begin
  Result := nil;
  fRangeProc(roGet, TRangeUNativeInt(Result));
end;

procedure TSynMultiSyn.SetRange(Value: Pointer);
begin
  fRangeProc(roSet, TRangeUNativeInt(Value));
end;

procedure TSynMultiSyn.NewRangeProc(Operation: TRangeOperation; var Range: TRangeUNativeInt);
const
  SchemeIndexSize = 3;
  MaxSchemeCount = (1 shl SchemeIndexSize) - 1;
  SchemeRangeSize = 4;
  MaxSchemeRange = (1 shl SchemeRangeSize) - 1;
begin
  if Operation = roGet then
  begin
    if DefaultHighlighter <> nil then
      Range := cardinal(DefaultHighlighter.GetRange)
    else
      Range := 0;
    if CurrScheme >= 0 then
    begin
      Assert(cardinal(Schemes[CurrScheme].Highlighter.GetRange) <= MaxSchemeRange);
      Range := Range shl SchemeRangeSize;
      Range := Range or cardinal(Schemes[CurrScheme].Highlighter.GetRange);
    end;
    Assert(CurrScheme <= MaxSchemeCount);
    Range := Range shl SchemeIndexSize;
    Range := Range or cardinal(CurrScheme + 1);
  end
  else
  begin
    CurrScheme := Integer(Range and MaxSchemeCount) - 1;
    Range := Range shr SchemeIndexSize;
    if CurrScheme >= 0 then
    begin
      Schemes[CurrScheme].Highlighter.SetRange(pointer(Range and MaxSchemeRange));
      Range := Range shr SchemeRangeSize;
    end;
    if DefaultHighlighter <> nil then
    begin
      fTmpRange := pointer(Range);
      DefaultHighlighter.SetRange(fTmpRange);
    end;
  end;
end;

function TSynMultiSyn.UpdateRangeProcs: Boolean;
// determines the appropriate RangeProcs and returns whether they were changed
var
  Int: Integer;
  OldProc: TRangeProc;
begin
  OldProc := fRangeProc;
  if Assigned(OnCustomRange) then
    fRangeProc := UserRangeProc
  else begin
    fRangeProc := NewRangeProc;
    for Int := 0 to Schemes.Count -1 do
      if Schemes[Int].Highlighter is TSynMultiSyn then
      begin
        fRangeProc := OldRangeProc;
        Break;
      end;
  end;
  Result := TMethod(OldProc).Code <> TMethod(fRangeProc).Code;
  if Result then
    DefHighlightChange(Self);
end;

procedure TSynMultiSyn.UserRangeProc(Operation: TRangeOperation; var Range: TRangeUNativeInt);
begin
  OnCustomRange(Self, Operation, pointer(Range));
  if (Operation = roSet) and (DefaultHighlighter <> nil) then
    fTmpRange := DefaultHighlighter.GetRange;
end;

procedure TSynMultiSyn.SetOnCustomRange(const Value: TCustomRangeEvent);
begin
  if (TMethod(OnCustomRange).Code <> TMethod(Value).Code) or
    (TMethod(OnCustomRange).Data <> TMethod(Value).Data) then
  begin
    fOnCustomRange := Value;
    UpdateRangeProcs;
  end;
end;

procedure TSynMultiSyn.Loaded;
begin
  inherited;
  DefHighlightChange(Self);
end;

function TSynMultiSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  if CurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.IsIdentChar(AChar)
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.IsIdentChar(AChar)
  else
    Result := inherited IsIdentChar(AChar);
end;

class function TSynMultiSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangGeneralMulti;
end;

procedure TSynMultiSyn.DoSetLine(const Value: string; LineNumber: Integer);
var
  iParser: TRegEx;
  Match : TMatch;
  iScheme: TScheme;
  iExpr: string;
  iLine: string;
  iEaten: Integer;
  Int: Integer;
begin
  ClearMarkers;

  iEaten := 0;
  iLine := Value;
  if CurrScheme >= 0
  then
    iScheme := fSchemes[CurrScheme]
  else
    iScheme := nil;
  while iLine <> '' do
    if iScheme <> nil then
    begin
      if iScheme.CaseSensitive then
        iParser.Create(iScheme.EndExpr, [roNotEmpty, roCompiled])
      else
        iParser.Create(iScheme.EndExpr, [roNotEmpty, roIgnoreCase, roCompiled]);
      Match := iParser.Match(iLine);

      if Match.Success then
      begin
        iExpr := Copy(Value, Match.Index + iEaten, Match.Length);
        DoCheckMarker(iScheme, Match.Index + iEaten, Match.Length,
          iExpr, False, LineNumber, Value);
        Delete(iLine, 1, Match.Index - 1 + Match.Length);
        Inc(iEaten, Match.Index - 1 + Match.Length);
        iScheme := nil;
      end
      else
        Break;
    end
    else
    begin
      for Int := 0 to Schemes.Count - 1 do
      begin
        iScheme := Schemes[Int];
        if (iScheme.StartExpr = '') or (iScheme.EndExpr = '') or
          (iScheme.Highlighter = nil) or (not iScheme.Highlighter.Enabled) then
        begin
          Continue;
        end;
        if iScheme.CaseSensitive then
          iParser.Create(iScheme.StartExpr, [roNotEmpty, roCompiled])
        else
          iParser.Create(iScheme.StartExpr, [roNotEmpty, roIgnoreCase, roCompiled]);
        Match := iParser.Match(iLine);

        if Match.Success then
        begin
          iExpr := Copy(Value, Match.Index + iEaten, Match.Length);
          DoCheckMarker(iScheme, Match.Index + iEaten, Match.Length,
            iExpr, True, LineNumber, Value);
          Delete(iLine, 1, Match.Index - 1 + Match.Length);
          Inc(iEaten, Match.Index - 1 + Match.Length);
          Break;
        end;
      end; {for}
      if Int >= Schemes.Count then
        Break;
    end; {else}

  fLineStr := Value;
  fLine := PWideChar(fLineStr);
  fCasedLineStr := '';
  fCasedLine := PWideChar(fLineStr);

  fMarker := nil;
  Run := 0;
  ExpandedRun := 0;
  fOldRun := Run;
  fTokenPos := 0;
  fExpandedTokenPos := 0;
  fNextMarker := 0;
  fLineNumber := LineNumber;
end;

function TSynMultiSyn.GetExpandedToken: string;
begin
  if (DefaultHighlighter = nil) and (fExpandedLine <> nil) then
    Result := fExpandedLineStr
  else
    Result := inherited GetExpandedToken;
end;

{ TSchemes }

constructor TSchemes.Create(aOwner: TSynMultiSyn);
begin
  inherited Create(TScheme);
  fOwner := aOwner;
end;

function TSchemes.GetItems(Index: Integer): TScheme;
begin
  Result := inherited Items[Index] as TScheme;
end;

function TSchemes.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

procedure TSchemes.SetItems(Index: Integer; const Value: TScheme);
begin
  inherited Items[Index] := Value;
end;

procedure TSchemes.Update(Item: TCollectionItem);
begin
  if Item <> nil then
    fOwner.DefHighlightChange(Item)
  else // pass the MultiSyn as the Sender so Editors reparse their text
    fOwner.DefHighlightChange(fOwner);
end;

{ TScheme }

function TScheme.ConvertExpression(const Value: string): string;
begin
  if not CaseSensitive then
    Result := SysUtils.AnsiUpperCase(Value)
  else
    Result := Value;
end;

constructor TScheme.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fCaseSensitive := True;
  fMarkerAttri := TSynHighlighterAttributes.Create(SYNS_AttrMarker, SYNS_FriendlyAttrMarker);
  fMarkerAttri.OnChange := MarkerAttriChanged;
  MarkerAttri.Background := clYellow;
  MarkerAttri.Style := [fsBold];
  MarkerAttri.InternalSaveDefaultValues;
end;

destructor TScheme.Destroy;
begin
  { unhook notification handlers }
  Highlighter := nil;
  inherited Destroy;
  fMarkerAttri.Free;
end;

procedure TScheme.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

function TScheme.GetDisplayName: string;
begin
  if SchemeName <> '' then
    Result := SchemeName
  else
    Result := inherited GetDisplayName;
end;

procedure TScheme.MarkerAttriChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TScheme.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    fCaseSensitive := Value;
    Changed(True);
  end;
end;

procedure TScheme.SetDisplayName(const Value: string);
begin
  SchemeName := Value;
end;

procedure TScheme.SetEndExpr(const Value: string);
var
  OldValue: string;
begin
  if fEndExpr <> Value then
  begin
    if Value <> '' then
      CheckExpression(Value);
    OldValue := fEndExpr;
    fEndExpr := Value;
    if ConvertExpression(OldValue) <> ConvertExpression(Value) then
      Changed(True);
  end;
end;

procedure TScheme.SetHighlighter(const Value: TSynCustomHighLighter);
var
  iOwner: TSynMultiSyn;
  iAlreadyRepainted: Boolean;
begin
  if Highlighter <> Value then
  begin
    iOwner := TSchemes(Collection).fOwner;
    if (Highlighter <> nil) and (Highlighter <> iOwner) then
      iOwner.UnhookHighlighter(Highlighter);
    fHighlighter := Value;
    if (Highlighter <> nil) and (Highlighter <> iOwner) then
      iOwner.HookHighlighter(Highlighter);
    if Highlighter is TSynMultiSyn then
      iAlreadyRepainted := iOwner.UpdateRangeProcs
    else
      iAlreadyRepainted := False;
    if not iAlreadyRepainted then
      Changed(True);
  end;
end;

procedure TScheme.SetMarkerAttri(const Value: TSynHighlighterAttributes);
begin
  fMarkerAttri.Assign(Value);
end;

procedure TScheme.SetStartExpr(const Value: string);
var
  OldValue: string;
begin
  if fStartExpr <> Value then
  begin
    if Value <> '' then
      CheckExpression(Value);
    OldValue := fStartExpr;
    fStartExpr := Value;
    if ConvertExpression(Value) <> ConvertExpression(OldValue) then
      Changed(True);
  end;
end;

end.
