﻿{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditSearch.pas, released 2000-04-07.

The Original Code is based on the mwEditSearch.pas file from the mwEdit
component suite by Martin Waldenburg and other developers.
Portions created by Martin Waldenburg are Copyright 1999 Martin Waldenburg.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditSearch.pas,v 1.12.2.6 2009/09/29 00:16:46 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditSearch;

{$I SynEdit.inc}

interface

uses
  Classes,
  SynEditTypes,
  SynEditMiscClasses;

type
  TSynEditSearch = class(TSynEditSearchCustom)
  private
    Run: PWideChar;
    Origin: PWideChar;
    TheEnd: PWideChar;
    Pat, CasedPat: string;
    fCount: Integer;
    fTextLen: Integer;
    Look_At: Integer;
    PatLen, PatLenSucc: Integer;
    Shift: array[WideChar] of Integer;
    fCaseSensitive: Boolean;
    fWhole: Boolean;
    fResults: TList;
    fShiftInitialized: Boolean;
    FTextToSearch: string;
    function GetFinished: Boolean;
    procedure InitShiftTable;
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    function TestWholeWord: Boolean;
    procedure SetPattern(const Value: string); override;
    function GetPattern: string; override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function FindAll(const NewText: string): Integer; override;
    function Replace(const aOccurrence, aReplacement: string): string; override;
    function FindFirst(const NewText: string): Integer;
    procedure FixResults(First, Delta: Integer);
    function Next: Integer;
    property Count: Integer read fCount write fCount;
    property Finished: Boolean read GetFinished;
    property Pattern read CasedPat;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property Whole: Boolean read fWhole write fWhole;
  end;

implementation

uses
  SysUtils;

constructor TSynEditSearch.Create(aOwner: TComponent);
begin
  inherited;
  fResults := TList.Create;
end;

function TSynEditSearch.GetFinished: Boolean;
begin
  Result := (Run >= TheEnd) or (PatLen >= fTextLen);
end;

function TSynEditSearch.GetResult(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < fResults.Count) then
    Result := Integer(fResults[Index]);
end;

function TSynEditSearch.GetResultCount: Integer;
begin
  Result := fResults.Count;
end;

procedure TSynEditSearch.FixResults(First, Delta: Integer);
var
  Int: Integer;
begin
  if (Delta <> 0) and (fResults.Count > 0) then begin
    Int := Pred(fResults.Count);
    while Int >= 0 do begin
      if Integer(fResults[Int]) <= First then Break;
      fResults[Int] := pointer(Integer(fResults[Int]) - Delta);
      Dec(Int);
    end;
  end;
end;

procedure TSynEditSearch.InitShiftTable;
var
  C: WideChar;
  Int: Integer;
begin
  PatLen := Length(Pat);
  if Patlen = 0 then raise Exception.Create('Pattern is empty');
  PatLenSucc := PatLen + 1;
  Look_At := 1;
  for C := Low(WideChar) to High(WideChar) do Shift[C] := PatLenSucc;
  for Int := 1 to PatLen do Shift[Pat[Int]] := PatLenSucc - Int;
  while Look_at < PatLen do
  begin
    if Pat[PatLen] = Pat[PatLen - Look_at] then Break;
    Inc(Look_at);
  end;
  fShiftInitialized := True;
end;                                

// TODO: would be more intelligent to use IsWordBreakChar for SynEdit
function IsWordBreakChar(C: WideChar): Boolean;
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

function TSynEditSearch.TestWholeWord: Boolean;
var
  Test: PWideChar;
begin
  Test := Run - PatLen;

  Result := ((Test < Origin) or IsWordBreakChar(Test[0])) and
    ((Run >= TheEnd) or IsWordBreakChar(Run[1]));
end;

function TSynEditSearch.Next: Integer;
var
  Int: Integer;
  J: PWideChar;
begin
  Result := 0;
  Inc(Run, PatLen);
  while Run < TheEnd do
  begin
    if Pat[Patlen] <> Run^ then
      Inc(Run, Shift[(Run + 1)^])
    else
    begin
      J := Run - PatLen + 1;
      Int := 1;
      while Pat[Int] = J^ do
      begin
        if Int = PatLen then
        begin
          if fWhole and not TestWholeWord then Break;
          Inc(fCount);
          Result := Run - Origin - Patlen + 2;
          Exit;
        end;
        Inc(Int);
        Inc(J);
      end;
      Inc(Run, Look_At);
      if Run >= TheEnd then
        Break;
      Inc(Run, Shift[Run^] - 1);
    end;
  end;
end;

destructor TSynEditSearch.Destroy;
begin
  fResults.Free;
  inherited Destroy;
end;

procedure TSynEditSearch.SetPattern(const Value: string);
begin
  if Pat <> Value then
  begin
    CasedPat := Value;
    if CaseSensitive then
      Pat := CasedPat
    else
      Pat := SysUtils.AnsiLowerCase(CasedPat);
    fShiftInitialized := False;
  end;
  fCount := 0;
end;

procedure TSynEditSearch.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    fCaseSensitive := Value;
    if fCaseSensitive then
      Pat := CasedPat
    else
      Pat := SysUtils.AnsiLowerCase(CasedPat);
    fShiftInitialized := False;
  end;
end;

function TSynEditSearch.FindAll(const NewText: string): Integer;
var
  Found: Integer;
begin
  // never shrink Capacity
  fResults.Count := 0;
  Found := FindFirst(NewText);
  while Found > 0 do
  begin
    fResults.Add(Pointer(Found));
    Found := Next;
  end;
  Result := fResults.Count;
end;

function TSynEditSearch.Replace(const aOccurrence, aReplacement: string): string;
begin
  Result := aReplacement;
end;                     

function TSynEditSearch.FindFirst(const NewText: string): Integer;
begin
  if not fShiftInitialized then
    InitShiftTable;
  Result := 0;
  fTextLen := Length(NewText);
  if fTextLen >= PatLen then
  begin
    if CaseSensitive then
      FTextToSearch := NewText
    else
      FTextToSearch := SysUtils.AnsiLowerCase(NewText);
    Origin := PWideChar(FTextToSearch);
    TheEnd := Origin + fTextLen;
    Run := (Origin - 1);
    Result := Next;
  end;
end;

function TSynEditSearch.GetLength(Index: Integer): Integer;
begin
  Result := PatLen;  
end;

function TSynEditSearch.GetPattern: string;
begin
  Result := CasedPat; 
end;

procedure TSynEditSearch.SetOptions(const Value: TSynSearchOptions);
begin
  CaseSensitive := ssoMatchCase in Value;
  Whole := ssoWholeWord in Value;
end;

end.

