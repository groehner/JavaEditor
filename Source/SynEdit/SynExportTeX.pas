{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynExportTeX.pas, released 2002-09-12.

The Original Code is partly based on the mwHTMLExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Ascher Stefan.
Portions created by Ascher Stefan are Copyright 2002 Ascher Stefan.
Unicode translation by Ma�l H�rz.
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

$Id: SynExportTeX.pas,v 1.8.2.5 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
- LaTeX 2e doesn't support Unicode, so this exporter doesn't either.
  (There are solutions like the package utc.sty but still they don't allow mixing
  of different languages like Arabic and Chinese.
  We'll have to wait for LaTeX 3.)
-------------------------------------------------------------------------------}

unit SynExportTeX;

{$I SynEdit.inc}

interface

uses
  Windows,
  Graphics,
  SynEditExport,
  SynEditHighlighter,
  SynUnicode,
  Classes;

type
  TSynExporterTeX = class(TSynCustomExporter)
  private
    fMargin: Integer;
    fLastAttri: TSynHighlighterAttributes;
    function AttriToCommand(Attri: TSynHighlighterAttributes;
      UniqueAttriName: string): string;
    function AttriToCommandCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
    function CommandNameCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
    function GetCommandName(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes): string;
    function GetNewCommands: string;
    function MakeValidName(Name: string): string;
  protected
    fCreateTeXFragment: Boolean;
    fTabWidth: Integer;
    fPageStyleEmpty: Boolean;
    
    // overriding these abstract methods (though they are never called for this
    // specific highlighter) to prevent abstract instance warnings
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged: Boolean;
      ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged: Boolean;
      ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttribute(BackgroundChanged: Boolean;
      ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;

    procedure FormatNewLine; override;
    procedure FormatToken(Token: string); override;
    function GetFooter: string; override;
    function GetFormatName: string; override;
    function GetHeader: string; override;
    function ReplaceReservedChar(AChar: WideChar): string; override;
    procedure SetTokenAttribute(Attri: TSynHighlighterAttributes); override;
    function UseBom: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function SupportedEncodings: TSynEncodings; override;
  published
    property Margin: Integer read fMargin write fMargin default 2;
    property TabWidth: Integer read fTabWidth write fTabWidth default 2;
    property Color;
    property CreateTeXFragment: Boolean read fCreateTeXFragment
      write fCreateTeXFragment default False;
    property PageStyleEmpty: Boolean read fPageStyleEmpty write fPageStyleEmpty
      default False;
    property DefaultFilter;
    property Encoding;
    property Font;
    property Highlighter;
    property Title;
    property UseBackground;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst,
  SysUtils;


// DotDecSepFormat always formats with a dot as decimal separator.
// This is necessary because LaTeX expects a dot, but VCL's Format is
// language-dependent, i.e. with another locale set, the separator can be
// different (for example a comma).
function DotDecSepFormat(const Format: string; const Args: array of const): string;
var
  pSettings: TFormatSettings;
begin
  pSettings := FormatSettings;
  pSettings.DecimalSeparator := '.';
  Result := SysUtils.Format(Format, Args, pSettings);
end;

function ColorToTeX(AColor: TColor): string;
const
  f = '%1.2g';
  f2 = '%s,%s,%s';
var
  RGBColor: LongWord;
  RValue, GValue, BValue: string;
begin
  RGBColor := ColorToRGB(AColor);
  RValue := DotDecSepFormat(f, [GetRValue(RGBColor) / 255]);
  GValue := DotDecSepFormat(f, [GetGValue(RGBColor) / 255]);
  BValue := DotDecSepFormat(f, [GetBValue(RGBColor) / 255]);
  Result := Format(f2, [RValue, GValue, BValue]);
end;

{ TSynExporterTeX }

constructor TSynExporterTeX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMargin := 2;
  fTabWidth := 2;
  fPageStyleEmpty := False;
  fDefaultFilter := SYNS_FilterTeX;
  FEncoding := seAnsi;
end;

function TSynExporterTeX.AttriToCommandCallback(
  Highlighter: TSynCustomHighlighter; Attri: TSynHighlighterAttributes;
  UniqueAttriName: string; Params: array of Pointer): Boolean;
var
  Commands: ^string;
begin
  Commands := Params[0];
  Commands^ := Commands^ + AttriToCommand(Attri, UniqueAttriName) + SLineBreak;
  Result := True; // we want all attributes => tell EnumHighlighterAttris to continue
end;

function TSynExporterTeX.AttriToCommand(Attri: TSynHighlighterAttributes;
  UniqueAttriName: string): string;
const
  NewCommand    = '\newcommand{\%s}[1]{%s#1%s}';
  SBold         = '\textbf{';
  SItalic       = '\textit{';
  SUnderline    = '\uln{';
  SColor        = '\textcolor[rgb]{%s}{';
  SBackColor    = '\colorbox[rgb]{%s}{';
var
  Formatting: string;
  BracketCount: Integer;
begin
  BracketCount := 0;
  with Attri do
  begin
    if fsBold in Style then
    begin
      Formatting := Formatting + SBold;
      Inc(BracketCount);
    end;
    if fsItalic in Style then
    begin
      Formatting := Formatting + SItalic;
      Inc(BracketCount);
    end;
    if fsUnderline in Style then
    begin
      Formatting := Formatting + SUnderline;
      Inc(BracketCount);
    end;
    if (Foreground <> clBlack) and (Foreground <> clNone)  then
    begin
      Formatting := Formatting + Format(SColor, [ColorToTeX(Foreground)]);
      Inc(BracketCount);
    end;
    if fUseBackground and (Background <> clNone) then
    begin
      Formatting := Formatting + Format(SBackColor, [ColorToTeX(Background)]);
      Inc(BracketCount);
    end;
    Result := Format(NewCommand, [MakeValidName(UniqueAttriName), Formatting,
      StringOfChar('}', BracketCount)])
  end;
end;

function TSynExporterTeX.CommandNameCallback(
  Highlighter: TSynCustomHighlighter; Attri: TSynHighlighterAttributes;
  UniqueAttriName: string; Params: array of Pointer): Boolean;
var
  AttriToFind: TSynHighlighterAttributes;
  CommandName: ^string;
begin
  AttriToFind := Params[0];
  CommandName := Params[1];

  if Attri = AttriToFind then
  begin
    CommandName^ := MakeValidName(UniqueAttriName);
    Result := False; // found => inform EnumHighlighterAttris to stop searching
  end
  else
    Result := True;
end;

procedure TSynExporterTeX.FormatToken(Token: string);
var
  CommandName: string;
begin
  CommandName := GetCommandName(Highlighter, fLastAttri);
  AddData('\' + CommandName + '{' + Token + '}');
end;

procedure TSynExporterTeX.FormatNewLine;
begin
  AddData('\\' + SLineBreak);
end;

// do nothing with these
procedure TSynExporterTeX.FormatAfterLastAttribute;
begin
end;

procedure TSynExporterTeX.FormatAttributeDone;
begin
end;

procedure TSynExporterTeX.FormatAttributeInit;
begin
end;

procedure TSynExporterTeX.FormatBeforeFirstAttribute;
begin
end;

function TSynExporterTeX.GetCommandName(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes): string;
begin
  EnumHighlighterAttris(Highlighter, False, CommandNameCallback, [Attri, @Result]);
end;

function TSynExporterTeX.GetFooter: string;
begin
  if not fCreateTeXFragment then
    Result := SLineBreak + '\end{ttfamily}' + SLineBreak + '\end{document}'
  else
    Result := SLineBreak + '\end{ttfamily}';
end;

function TSynExporterTeX.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatTeX;
end;

function TSynExporterTeX.GetHeader: string;
const
  TeXHeader   = '\documentclass[a4paper, %dpt]{article}' + SLineBreak +
                '\usepackage[a4paper, margin=%dcm]{geometry}' + SLineBreak +
                '\usepackage[T1]{fontenc}' + SLineBreak +
                '\usepackage{color}' + SLineBreak +
                '\usepackage{alltt}' + SLineBreak +
                '\usepackage{times}' + SLineBreak +
                '\usepackage{ulem}' + SLineBreak +
                // It is recommennded to use AnsiNew on Windows
                '\usepackage[ansinew]{inputenc}' + SLineBreak +
                '%s' + SLineBreak; // New Commands
  TeXHeader2  = '%% Generated by SynEdit TeX exporter' + SLineBreak + SLineBreak +
                '\begin{document}';
  EmptyPage   = '\pagestyle{empty}';
  TeXDocument = '\begin{ttfamily}' + SLineBreak +
                '\noindent' + SLineBreak;
var
  PageStyle: string;
begin
  if not fCreateTeXFragment then
  begin
    if fPageStyleEmpty then
      PageStyle := SLineBreak + EmptyPage
    else
      PageStyle := '';
    Result := Format(TeXHeader + SLineBreak + SLineBreak,
      [Font.Size, fMargin, GetNewCommands]);
    Result := Result + '\title{' + Title + '}' + SLineBreak + TeXHeader2 +
      SLineBreak + PageStyle;
  end;
  Result := Result + TeXDocument;
end;

function TSynExporterTeX.GetNewCommands: string;
const
  FixedCommands = '%% Special Characters' + SLineBreak +
                  '\newcommand\SPC{\hspace*{0.6em}}' + SLineBreak +
                  '\newcommand\TAB{\hspace*{%sem}}' + SLineBreak +
                  '\newcommand\BS{\mbox{\char 92}}' + SLineBreak +   // Backslash
                  '\newcommand\TLD{\mbox{\char 126}}' + SLineBreak + // ~
                  '\newcommand\CIR{\mbox{\char 94}}' + SLineBreak  + // ^
                  '\newcommand\HYP{\mbox{\char 45}}' + SLineBreak  + // a simple -
                  '\newcommand\QOT{\mbox{\char 34}}' + SLineBreak  + // "
                  '\newcommand{\uln}[1]{\bgroup \markoverwith{\hbox{\_}}\ULon{{#1}}}' + SLineBreak +
                  '%% Highlighter Attributes' + SLineBreak;
  f = '%1.1g';
var
  tw: string;
  Commands: string;
begin
  tw := DotDecSepFormat(f, [fTabWidth * 0.6]);
  Result := Format(FixedCommands, [tw]);

  EnumHighlighterAttris(Highlighter, True, AttriToCommandCallback, [@Commands]);
  Result := Result + Commands;
end;

function TSynExporterTeX.MakeValidName(Name: string): string;
var
  Int: Integer;
begin
  Result := Name;
  
  for Int := Length(Result) downto 1 do
  if CharInSet(Result[Int], ['1'..'9']) then
    Result[Int] := Char(Ord('A') + Ord(Result[Int]) - Ord('1'))
  else if Result[Int] = '0' then
    Result[Int] := 'Z'
  else if not CharInSet(Result[Int], ['a'..'z', 'A'..'Z']) then
    Delete(Result, Int, 1);
end;

function TSynExporterTeX.ReplaceReservedChar(AChar: WideChar): string;
begin
  case AChar of
    '{': Result := '\{';
    '}': Result := '\}';
    '\': Result := '\BS ';
    '~': Result := '\TLD ';
    '^': Result := '\CIR ';
    ' ': Result := '\SPC ';
    #9: Result := '\TAB ';
    '-': Result := '\HYP ';
    '"': Result := '\QOT ';
    '@': Result := '$@$';
    '$': Result := '\$';
    '&': Result := '\&';
    '<': Result := '$<$';
    '>': Result := '$>$';
    '_': Result := '\_';
    '#': Result := '\#';
    '%': Result := '\%';
    else Result := '';
  end;
end;

procedure TSynExporterTeX.SetTokenAttribute(Attri: TSynHighlighterAttributes);
begin
  fLastAttri := Attri;
end;

function TSynExporterTeX.SupportedEncodings: TSynEncodings;
begin
  Result := [seAnsi];
end;

function TSynExporterTeX.UseBom: Boolean;
begin
  Result := False;
end;

end.

