﻿{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
The Original Code is: SynExportHTML.pas, released 2000-04-16.
The Original Code is partly based on the mwHTMLExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by James D. Jacobson are Copyright 1999 Martin Waldenburg.
Changes to emit XHTML 1.0 Strict complying code by Maël Hörz.
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
$Id: SynExportHTML.pas,v 1.19.2.7 2008/09/14 16:24:59 maelh Exp $
You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net
Known Issues:
-------------------------------------------------------------------------------}

unit SynExportHTML;

{$I SynEdit.inc}

interface

uses
  Graphics,
  Classes,
  SynEditExport,
  SynEditHighlighter,
  SynUnicode,
  System.Generics.Collections;

type
  TSynExporterHTML = class(TSynCustomExporter)
  private
    FStyleNameCache: TDictionary<TSynHighlighterAttributes, string>;
    FStyleValueCache: TDictionary<TSynHighlighterAttributes, string>;
    FSuppressFragmentInfo: Boolean;
    function AttriToCSS(Attri: TSynHighlighterAttributes;
      UniqueAttriName: string): string;
    function AttriToCSSCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
    function ColorToHTML(AColor: TColor): string;
    function GetStyleName(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes): string;
    function MakeValidName(Name: string): string;
    function StyleNameCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
    function AttriToInlineCSSCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
    function AttriToInlineCSS(Attri: TSynHighlighterAttributes): string;
  protected
    // CreateHTMLFragment is used to indicate that this is for the clipboard "HTML Format" output.
    // Note: SynEdit's default OLE clipboard handling bypasses SynEditExport's clipboard handling.
    FCreateHTMLFragment: Boolean;   // True if format should be "HTML Format", always uses inline css.
    FInlineCSS: Boolean;
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: Boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: Boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttribute(BackgroundChanged,
      ForegroundChanged: Boolean; FontStylesChanged: TFontStyles); override;
    procedure FormatNewLine; override;
    function GetFooter: string; override;
    function GetFormatName: string; override;
    function GetHeader: string; override;
    function ReplaceReservedChar(AChar: WideChar): string; override;
    function UseBom: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SupportedEncodings: TSynEncodings; override;
    procedure Clear; override;
  published
    property Color;
    property CreateHTMLFragment: Boolean read FCreateHTMLFragment
      write FCreateHTMLFragment default False;
    property InlineCSS: Boolean read FInlineCSS
      write FInlineCSS default False;
    property SuppressFragmentInfo: Boolean read FSuppressFragmentInfo
      write FSuppressFragmentInfo default False;
    property DefaultFilter;
    property Encoding;
    property Font;
    property Highlighter;
    property Title;
    property UseBackground;
  end;

implementation

uses
  Windows,
  SynEditMiscProcs,
  SynEditStrConst,
  SysUtils;

{ TSynExporterHTML }

const
  DetailLength = 105;  // This is the fixed length of the filled-in DetailSection.
  DetailSection = 'Version:0.9'#13#10 +
                  'StartHTML:%.10d'#13#10 +
                  'EndHTML:%.10d'#13#10 +
                  'StartFragment:%.10d'#13#10 +
                  'EndFragment:%.10d'#13#10;

  DocType       = '<!DOCTYPE html>' + sLineBreak;
  HTMLStartText =  '<html>' + sLineBreak;
  HeadStartText = '<head>' + sLineBreak + '<meta charset="utf-8">' + sLineBreak;

  StyleStartText = '<style>' + sLineBreak + '<!--' + sLineBreak;
  BodyStyleTextFormat = 'body { color: %s; background-color: %s; }' + sLineBreak;
  StyleEndText = '-->' + sLineBreak + '</style>' + sLineBreak;

  HeadEndText = '</head>' + sLineBreak;
  BodyStartText   = '<body>' + sLineBreak;

  FragmentStartText = '<!--StartFragment-->';
  FragmentEndText =   '<!--EndFragment-->';

  BodyEndText   = '</body>' + sLineBreak;
  HTMLEndText   = '</html>';


constructor TSynExporterHTML.Create(AOwner: TComponent);
const
  CF_HTML = 'HTML Format';
begin
  inherited Create(AOwner);
  FStyleNameCache := TDictionary<TSynHighlighterAttributes, string>.Create;
  FStyleValueCache := TDictionary<TSynHighlighterAttributes, string>.Create;
  fClipboardFormat := RegisterClipboardFormat(CF_HTML);
  fDefaultFilter := SYNS_FilterHTML;
  FEncoding := seUTF8;
  FCreateHTMLFragment := False;
  FInlineCSS := False;
end;

destructor TSynExporterHTML.Destroy;
begin
  FStyleNameCache.Free;
  FStyleValueCache.Free;
  inherited;
end;

function TSynExporterHTML.AttriToInlineCSS(Attri: TSynHighlighterAttributes): string;
begin
  Result := '';
  if UseBackground and (Attri.Background <> clNone) then
    Result := Result + 'background-color: ' + ColorToHTML(Attri.Background) + '; ';
  if Attri.Foreground <> clNone then
    Result := Result + 'color: ' + ColorToHTML(Attri.Foreground) + '; ';
  if fsBold in Attri.Style then
    Result := Result + 'font-weight: bold; ';
  if fsItalic in Attri.Style then
    Result := Result + 'font-style: italic; ';
  if fsUnderline in Attri.Style then
    Result := Result + 'text-decoration: underline; ';
  if fsStrikeOut in Attri.Style then
    Result := Result + 'text-decoration: line-through; ';
end;

function TSynExporterHTML.AttriToInlineCSSCallback(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes; UniqueAttriName: string;
  Params: array of Pointer): Boolean;
var
  StyleText: string;
begin
  StyleText := AttriToInlineCSS(Attri);
  FStyleValueCache.Add(Attri, StyleText);
  Result := True;  // Get all attributes.
end;

function TSynExporterHTML.AttriToCSS(Attri: TSynHighlighterAttributes;
  UniqueAttriName: string): string;
var
  StyleName: string;
begin
  if not FStyleNameCache.TryGetValue(Attri, StyleName) then
    Exit('');  // Skip any styles that weren't in the data.
  Result := '.' + StyleName + ' { ';
  Result := Result + AttriToInlineCSS(Attri);
  Result := Result + '}';
end;

function TSynExporterHTML.AttriToCSSCallback(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes; UniqueAttriName: string;
  Params: array of Pointer): Boolean;
var
  Styles: ^string;
  StyleText: string;
begin
  Styles := Params[0];
  StyleText := AttriToCSS(Attri, UniqueAttriName);
  if StyleText <> '' then
    Styles^ := Styles^ + StyleText + #13#10;
  Result := True; // we want all attributes => tell EnumHighlighterAttris to continue
end;

procedure TSynExporterHTML.Clear;
begin
  inherited;
  if Assigned(FStyleNameCache) then
    FStyleNameCache.Clear;
  if Assigned(FStyleValueCache) then
    FStyleValueCache.Clear;
end;

function TSynExporterHTML.ColorToHTML(AColor: TColor): string;
var
  RGBColor: Integer;
  RGBValue: byte;
const
  Digits: array[0..15] of Char = '0123456789ABCDEF';
begin
  RGBColor := ColorToRGB(AColor);
  Result := '#000000';
  RGBValue := GetRValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[2] := Digits[RGBValue shr  4];
    Result[3] := Digits[RGBValue and 15];
  end;
  RGBValue := GetGValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[4] := Digits[RGBValue shr  4];
    Result[5] := Digits[RGBValue and 15];
  end;
  RGBValue := GetBValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[6] := Digits[RGBValue shr  4];
    Result[7] := Digits[RGBValue and 15];
  end;
end;

procedure TSynExporterHTML.FormatAfterLastAttribute;
begin
  AddData('</span>' + sLineBreak + '</pre>');
end;

procedure TSynExporterHTML.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
begin
  AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
var
  StyleName: string;
  StyleValue: string;
begin
  if FCreateHTMLFragment or FInlineCSS then
  begin
    FStyleValueCache.TryGetValue(Highlighter.GetTokenAttribute, StyleValue);
    if StyleValue <> '' then
      AddData('<span style="' + StyleValue + '">')
    else
      AddData('<span>');
  end
  else
  begin
    StyleName := GetStyleName(Highlighter, Highlighter.GetTokenAttribute);
    AddData('<span class="' + StyleName + '">');
  end;
end;

procedure TSynExporterHTML.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: Boolean; FontStylesChanged: TFontStyles);
var
  StyleName: string;
  StyleValue: string;
begin
  // Cache all our CSS values.
  EnumHighlighterAttris(Highlighter, True, AttriToInlineCSSCallback, []);
  AddData('<pre style="font-family: ' + FFont.Name + ', ''Courier New'', monospace; font-size: ' +
    FFont.Size.ToString + 'pt;">' + sLineBreak);
  if FCreateHTMLFragment or FInlineCSS then
  begin
    FStyleValueCache.TryGetValue(Highlighter.GetTokenAttribute, StyleValue);
    if StyleValue <> '' then
      AddData('<span style="' + StyleValue + '">')
    else
      AddData('<span>');
  end
  else
  begin
    StyleName := GetStyleName(Highlighter, Highlighter.GetTokenAttribute);
    AddData('<span class="' + StyleName + '">');
  end;
end;

procedure TSynExporterHTML.FormatNewLine;
begin
  AddData(sLineBreak);
end;

function TSynExporterHTML.GetFooter: string;
begin
  Result := '';
  if FCreateHTMLFragment and not FSuppressFragmentInfo then
    Result := Result + FragmentEndText;
  if not FCreateHTMLFragment then
    Result := Result + sLineBreak + BodyEndText + HTMLEndText;
end;

function TSynExporterHTML.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatHTML;
end;

function TSynExporterHTML.GetHeader: string;
var
  Styles: string;
  Header: string;
  StartHTMLPos: Integer;
  EndHTMLPos: Integer;
  StartFragmentPos: Integer;
  EndFragmentPos: Integer;
begin
  EnumHighlighterAttris(Highlighter, True, AttriToCSSCallback, [@Styles]);
  Header := HTMLStartText;
  if not FCreateHTMLFragment then
  begin
    Header := DocType + Header +  HeadStartText;
    Header := Header + '<title>' + Title + '</title>'#13#10;
    if not InlineCSS then
    begin
      Header := Header + StyleStartText;
      Header := Header +
        Format(BodyStyleTextFormat, [ColorToHtml(fFont.Color), ColorToHTML(fBackgroundColor)]);
      Header := Header + Styles;
      Header := Header + StyleEndText;
    end;
    Header := Header + HeadEndText;
  end;
  Header := Header + BodyStartText;
  Result := '';
  if not FCreateHTMLFragment then
  begin
    Result := Header;
  end
  else
  begin
    if not FSuppressFragmentInfo then
    begin
      StartHTMLPos := DetailLength;
      StartFragmentPos := DetailLength + Header.Length + FragmentStartText.Length;
      EndFragmentPos := StartFragmentPos + GetBufferSize;
      EndHTMLPos := EndFragmentPos + GetFooter.Length;
      Result := Format(DetailSection,
        [StartHTMLPos, EndHTMLPos, StartFragmentPos, EndFragmentPos]);
      Result := Result + Header + FragmentStartText;
    end;
  end;
end;

function TSynExporterHTML.GetStyleName(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes): string;
begin
  if not FStyleNameCache.TryGetValue(Attri, Result) then
  begin
    EnumHighlighterAttris(Highlighter, False, StyleNameCallback, [Attri, @Result]);
    FStyleNameCache.Add(Attri, Result);
  end;
end;

function TSynExporterHTML.MakeValidName(Name: string): string;
var
  Int: Integer;
begin
  Result := LowerCase(Name);
  for Int := Length(Result) downto 1 do
    if CharInSet(Result[Int], ['.', '_']) then
      Result[Int] := '-'
    else if not CharInSet(Result[Int], ['a'..'z', '0'..'9', '-']) then
      Delete(Result, Int, 1);
end;

function TSynExporterHTML.ReplaceReservedChar(AChar: WideChar): string;
begin
  case AChar of
    '&': Result := '&amp;';
    '<': Result := '&lt;';
    '>': Result := '&gt;';
    '"': Result := '&quot;';
    ' ': Result := '&nbsp;';
    else Result := '';
  end
end;

function TSynExporterHTML.StyleNameCallback(Highlighter: TSynCustomHighlighter;
    Attri: TSynHighlighterAttributes; UniqueAttriName: string;
    Params: array of Pointer): Boolean;
var
  AttriToFind: TSynHighlighterAttributes;
  StyleName: ^string;
begin
  AttriToFind := Params[0];
  StyleName := Params[1];

  if Attri = AttriToFind then
  begin
    StyleName^ := MakeValidName(UniqueAttriName);
    Result := False; // found => inform EnumHighlighterAttris to stop searching
  end
  else
    Result := True;
end;

function TSynExporterHTML.UseBom: Boolean;
begin
  // do not include seUTF8 as some browsers have problems with UTF-8-BOM
  Result := Encoding in [seUTF16LE, seUTF16BE];
end;

function TSynExporterHTML.SupportedEncodings: TSynEncodings;
begin
  Result := [seUTF8, seUTF16LE, seUTF16BE];
end;

end.
