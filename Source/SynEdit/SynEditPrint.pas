{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrint.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
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

$Id: SynEditPrint.pas,v 1.34.2.12 2008/09/23 14:02:08 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  Wrapping across page boundaries is not supported
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
CONTENTS:
  Print controller component.
    Allows setting margins, headers and footers.

  Design time properties:
    Header        : Class property to set properties for headers -
                    see CSynEditHeaderFooter.pas
    Footer        : Class property to set properties for footers -
                    see CSynEditHeaderFooter.pas
    Margins       : Class property to set properties for margins -
                    see CSynEditPrintMargins.pas
    Lines         : The lines that should be printed (see also SynEdit the
                    property below)
    Font          : The font the lines should be printed in (see also SynEdit
                    the property below)
    Title         : A title - can be referenced in headers/footers by using the
                    $TITLE$ macro
    Wrap          : Wrap text to margins
    Highlight     : Highlight text
    Colors        : Print in colors
    LineNumbers   : Print line numbers
    LineOffset    : Value added to linenumbers when printing
    PageOffset    : Value added to pagenumbers when printing
    OnPrintLine   : Fired when a line is printed
    OnPrintStatus : Fired at Beginning, End and when a new page is started
    Highlighter   : The highlighter used for highlighting the text (see also the
                    SynEdit property below)
    LineNumbersInMargin : If true line numbers are printed in the left margin,
                          else left margin is increased by width of line
                          number text.
    SelectedOnly  : Print only the selected Area
  Run-time properties:
    DocTitle    : Used to display the document name in the print queue monitor
    PrinterInfo : Read only. Returns info on printer (used internally)
    PageCount   : Returns the total number of pages;
    SynEdit     : By setting SynEdit to a specific TSynEdit component, the
                  properties Lines, Font and Highlighter are automatically
                  set to the corresponding values of the TSynEdit component
  Run-time methods:
    UpdatePages   : Used internally by the TSynEditPrintPreview component
    PrintToCanvas : Used internally by the TSynEditPrintPreview component
    Print         : Prints the contents of the Lines property
    PrintRange(StartPage,EndPage) : Prints the specified page-range (both inclusive)
-------------------------------------------------------------------------------}

unit SynEditPrint;

{$M+}
{$I SynEdit.inc}

interface

uses
  Windows,
  Graphics,
  SynEdit,
  SynEditTypes,
  SynEditPrintTypes,
  SynEditPrintHeaderFooter,
  SynEditPrinterInfo,
  SynEditPrintMargins,
  SynEditMiscProcs,
  SynEditHighlighter,
  Classes;

type
  TPageLine = class
  public
    FirstLine: Integer;
  end;
  //The actual print controller object
  TSynEditPrint = class(TComponent)
  private
    FCopies: Integer;                                                           
    FFooter: TFooter;
    FHeader: THeader;
    FLines: TStrings;
    FMargins: TSynEditPrintMargins;
    FPageCount: Integer;
    FFont: TFont;
    FTitle: string;
    FDocTitle: string;
    FPrinterInfo: TSynEditPrinterInfo;
    FPages: TList;
    FCanvas: TCanvas;
    FCharWidth: Integer;
    FMaxLeftChar: Integer;
    FWrap: Boolean;
    FOnPrintLine: TPrintLineEvent;
    FOnPrintStatus: TPrintStatusEvent;
    FYPos: Integer;
    FLineHeight: Integer;
    FHighlight: Boolean;
    FColors: Boolean;
    FHighlighter: TSynCustomHighlighter;
    FOldFont: TFont;
    FSynOK: Boolean;
    FLineNumbers: Boolean;
    FLineNumber: Integer;
    FLineOffset: Integer;
    FAbort: Boolean;
    FPrinting: Boolean;
    FDefaultBG: TColor;
    FPageOffset: Integer;
    FRangesOK: Boolean;
    FMaxWidth: Integer;
    FMaxCol: Integer;
    FPagesCounted: Boolean;
    FLineNumbersInMargin: Boolean;
    FTabWidth: Integer;
    fFontColor: TColor;                                                         
    fSelectedOnly: Boolean;                                                     
    fSelAvail: Boolean;
    fSelMode: TSynSelectionMode;
    fBlockBegin: TBufferCoord;
    fBlockEnd: TBufferCoord;
    FETODist: PIntArray;
    procedure CalcPages;
    procedure SetLines(const Value: TStrings);
    procedure SetFont(const Value: TFont);
    procedure SetCharWidth(const Value: Integer);
    procedure SetMaxLeftChar(const Value: Integer);
    procedure PrintPage(Num: Integer);
    procedure WriteLine(const Text: string);
    procedure WriteLineNumber;
    procedure HandleWrap(const Text: string; MaxWidth: Integer);
    procedure TextOut(const Text: string; AList: TList);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetPixelsPrInch;
    procedure InitPrint;
    procedure InitRanges;
    function GetPageCount: Integer;
    procedure SetSynEdit(const Value: TCustomSynEdit);
    procedure SetFooter(const Value: TFooter);
    procedure SetHeader(const Value: THeader);
    procedure SetMargins(const Value: TSynEditPrintMargins);
    function ClipLineToRect(Str: string; R: TRect): string;
    function ExpandAtWideGlyphs(const Str: string): string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    procedure PrintStatus(Status: TSynPrintStatus; PageNumber: Integer;
      var Abort: Boolean); virtual;
    procedure PrintLine(LineNumber, PageNumber: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdatePages(ACanvas: TCanvas);
    procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
    procedure Print;
    procedure PrintRange(StartPage, EndPage: Integer);
    property PrinterInfo: TSynEditPrinterInfo read FPrinterInfo;
    property PageCount: Integer read GetPageCount;
    property SynEdit: TCustomSynEdit write SetSynEdit;

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property Copies: Integer read FCopies write FCopies;
    property Header: THeader read FHeader write SetHeader;
    property Footer: TFooter read FFooter write SetFooter;
    property Margins: TSynEditPrintMargins read FMargins write SetMargins;
    property Lines: TStrings read FLines write SetLines;
    property Font: TFont read FFont write SetFont;
    property Title: string read FTitle write FTitle;
    property DocTitle: string read FDocTitle write FDocTitle;
    property Wrap: Boolean read FWrap write FWrap default True;
    property Highlight: Boolean read FHighlight write FHighlight default True;
    property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly       
      default False;
    property Colors: Boolean read FColors write FColors default False;
    property LineNumbers: Boolean read FLineNumbers write FLineNumbers
      default False;
    property LineOffset: Integer read FLineOffset write FLineOffset default 0;
    property PageOffset: Integer read FPageOffset write FPageOffset default 0;
    property OnPrintLine: TPrintLineEvent read FOnPrintLine write FOnPrintLine;
    property OnPrintStatus: TPrintStatusEvent read FOnPrintStatus
      write FOnPrintStatus;
    property Highlighter: TSynCustomHighlighter read FHighlighter
      write SetHighlighter;
    property LineNumbersInMargin: Boolean read FLineNumbersInMargin
      write FLineNumbersInMargin default False;
    property TabWidth: Integer read fTabWidth write fTabWidth;                  
    property Color: TColor read fDefaultBG write fDefaultBG;                    
  end;

implementation

uses
  Math,
  SysUtils,
  Printers,
  UITypes,
  SynUnicode;

{ TSynEditPrint }

constructor TSynEditPrint.Create(AOwner: TComponent);
begin
  inherited;
  FCopies := 1;
  FFooter := TFooter.Create;
  FHeader := THeader.Create;
  FLines := TStringList.Create;
  FMargins := TSynEditPrintMargins.Create;
  FPrinterInfo := TSynEditPrinterInfo.Create;
  FFont := TFont.Create;
  FOldFont := TFont.Create;
  MaxLeftChar := 1024;
  FWrap := True;
  FHighlight := True;
  FColors := False;
  FLineNumbers := False;
  FLineOffset := 0;
  FPageOffset := 0;
  FLineNumbersInMargin := False;
  FPages := TList.Create;
  FTabWidth := 8;                                                     
  FDefaultBG := clWhite;                                                        
end;

destructor TSynEditPrint.Destroy;
var
  Int: Integer;
begin
  FFooter.Free;
  FHeader.Free;
  FLines.Free;
  FMargins.Free;
  FPrinterInfo.Free;
  FFont.Free;
  FOldFont.Free;
  for Int := 0 to FPages.Count - 1 do
    TPageLine(FPages[Int]).Free;
  FPages.Free;
  FreeMem(FETODist);
  inherited;
end;

procedure TSynEditPrint.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

procedure TSynEditPrint.SetLines(const Value: TStrings);
var
  Int, j: Integer;
  ConvertTabsProc: TConvertTabsProc;
  TmpString: string;
begin
  ConvertTabsProc := GetBestConvertTabsProc(FTabWidth);
  with FLines do
  begin
    BeginUpdate;
    try
      Clear;
      for Int := 0 to Value.Count - 1 do
      begin
        TmpString := ConvertTabsProc(Value[Int], FTabWidth);
        j := Pos(#9, TmpString);
        while j > 0 do
        begin
          TmpString[j] := ' ';
          j := Pos(#9, TmpString);
        end;
        Add(TmpString);
      end;
    finally
      EndUpdate;
    end;
  end;
  FRangesOK := False;
  FPagesCounted := False;
end;

procedure TSynEditPrint.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FPagesCounted := False;
end;

procedure TSynEditPrint.SetCharWidth(const Value: Integer);
begin
  FCharWidth := Value;
end;

procedure TSynEditPrint.SetMaxLeftChar(const Value: Integer);
begin
  FMaxLeftChar := Value;
end;

procedure TSynEditPrint.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  FHighlighter := Value;
  FRangesOK := False;
  FPagesCounted := False;
end;

// Inserts filling chars into a string containing chars that display as glyphs
// wider than an average glyph. (This is often the case with Asian glyphs, which
// are usually wider than latin glpyhs)
// This is only to simplify paint-operations and has nothing to do with
// multi-byte chars.
function TSynEditPrint.ExpandAtWideGlyphs(const Str: string): string;
var
  Int, j, CountOfAvgGlyphs: Integer;
begin
  FCanvas.Font := Font;

  j := 0;
  SetLength(Result, Length(Str) * 2); // speed improvement
  for Int := 1 to Length(Str) do
  begin
    Inc(j);
    // Introduce a small tolerance Issue 54
    CountOfAvgGlyphs := Ceil(FCanvas.TextWidth(Str[Int]) / fCharWidth - 0.04);

    if j + CountOfAvgGlyphs > Length(Result) then
      SetLength(Result, Length(Result) + 128);

    // insert CountOfAvgGlyphs filling chars
    while CountOfAvgGlyphs > 1 do
    begin
      Result[j] := FillerChar;
      Inc(j);
      Dec(CountOfAvgGlyphs);
    end;

    Result[j] := Str[Int];
  end;

  SetLength(Result, j);
end;

procedure TSynEditPrint.InitPrint;
{ Initialize Font.PixelsPerInch, Character widths, Margins, Total Page count,
  headers and footers}
var
  TmpSize: Integer;
  TmpTextMetrics: TTextMetric;
begin
//  FDefaultBG := FCanvas.Brush.Color;                                          
  fFontColor := FFont.Color;
  FCanvas.Font.Assign(FFont);
  if not FPrinting then
  begin
    SetPixelsPrInch;
    TmpSize := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch := FFont.PixelsPerInch;
    FCanvas.Font.Size := TmpSize;
  end;
  // Calculate TextMetrics with the (probably) most wider text styles so text is
  // never clipped (although potentially wasting space)
  FCanvas.Font.Style := [fsBold, fsItalic, fsUnderline, fsStrikeOut];
  GetTextMetrics(FCanvas.Handle, TmpTextMetrics);
  CharWidth := TmpTextMetrics.tmAveCharWidth;
  FLineHeight := TmpTextMetrics.tmHeight + TmpTextMetrics.tmExternalLeading;
  FCanvas.Font.Style := FFont.Style;
  FMargins.InitPage(FCanvas, 1, FPrinterInfo, FLineNumbers, FLineNumbersInMargin,
    FLines.Count - 1 + FLineOffset);
  CalcPages;
  FHeader.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
  FFooter.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
  FSynOK := Highlight and Assigned(FHighLighter) and (FLines.Count > 0);
end;

procedure TSynEditPrint.SetPixelsPrInch;
var
  TmpSize: Integer;
begin
  FHeader.SetPixPrInch(FPrinterInfo.YPixPrInch);
  FFooter.SetPixPrInch(FPrinterInfo.YPixPrInch);
  //This should be necessary - else size would be changed...
  TmpSize := FFont.Size;
  FFont.PixelsPerInch := FPrinterInfo.YPixPrInch;
  FFont.Size := TmpSize;
end;

procedure TSynEditPrint.InitRanges;
//Initialize ranges in Highlighter
var
  Int: Integer;
begin
  if not FRangesOK and Assigned(FHighlighter) and (Lines.Count > 0) then
  begin
    FHighlighter.ResetRange;
    FLines.Objects[0] := fHighlighter.GetRange;
    Int := 1;
    while Int < Lines.Count do
    begin
      FHighlighter.SetLine(FLines[Int - 1], Int - 1);
      FHighlighter.NextToEol;
      FLines.Objects[Int] := FHighlighter.GetRange;
      Inc(Int);
    end;
    FRangesOK := True;
  end;
end;

// Calculates the total number of pages
procedure TSynEditPrint.CalcPages;
var
  AStr, Text: string;
  StrWidth: Integer;
  Int, j: Integer;
  AList: TList;
  YPos: Integer;
  PageLine: TPageLine;

  //Counts the number of lines a line is wrapped to
  procedure CountWrapped;
  var
    j: Integer;
  begin
    for j := 0 to AList.Count - 1 do
      YPos := YPos + FLineHeight;
  end;

var
  iStartLine, iEndLine: Integer;
  iSelStart, iSelLen: Integer;
begin
  InitRanges;
  for Int := 0 to FPages.Count - 1 do
    TPageLine(FPages[Int]).Free;
  FPages.Clear;
  FMaxWidth := FMargins.PRight - FMargins.PLeft;
  AStr := '';
  FMaxCol := 0;
  while FCanvas.TextWidth(AStr) < FMaxWidth do
  begin
    AStr := AStr + 'W';
    FMaxCol := FMaxCol + 1;
  end;
  FMaxCol := FMaxCol - 1;
  {FTestString is used to Calculate MaxWidth when prewiewing and printing -
   else the length is not calculated correctly when prewiewing and the
   zoom is different from 0.25,0.5,1,2,4 (as for example 1.20) - WHY???}
//  fTestString := StringofChar('W', FMaxCol);
  AStr := StringofChar('W', FMaxCol);
  FMaxWidth := FCanvas.TextWidth(AStr);
  FPageCount := 1;
  PageLine := TPageLine.Create;
  PageLine.FirstLine := 0;
  FPages.Add(PageLine);
  YPos := FMargins.PTop;
  if SelectedOnly then
  begin
    iStartLine := fBlockBegin.Line -1;
    iEndLine := fBlockEnd.Line -1;
  end
  else begin
    iStartLine := 0;
    iEndLine := Lines.Count -1;
  end;
  for Int := iStartLine to iEndLine do
  begin
    if not fSelectedOnly or (fSelMode = smLine) then
      Text := Lines[Int]
    else
    begin
      if (fSelMode = smColumn) or (Int = fBlockBegin.Line -1) then
        iSelStart := fBlockBegin.Char
      else
        iSelStart := 1;
      if (fSelMode = smColumn) or (Int = fBlockEnd.Line -1) then
        iSelLen := fBlockEnd.Char  - iSelStart
      else
        iSelLen := MaxInt;
      Text := Copy( Lines[Int], iSelStart, iSelLen );
    end;
      {if new page then increase FPageCount and save the top-line number in
       FPages}
    if YPos + FLineHeight > FMargins.PBottom then
    begin
      YPos := FMargins.PTop;
      FPageCount := FPageCount + 1;
      PageLine := TPageLine.Create;
      PageLine.FirstLine := Int;
      FPages.Add(PageLine);
    end;
    StrWidth := FCanvas.TextWidth(Text);
    {Check for wrap}
    if Wrap and (StrWidth > FMaxWidth) then begin                          
      AList := TList.Create;
      if WrapTextEx(Text, [' ', '-', #9, ','], FMaxCol, AList) then
        CountWrapped
      else
      begin
              {If WrapTextToList didn't succed with the first set of breakchars
               then try this one:}
        if WrapTextEx(Text, [';', ')', '.'], FMaxCol, AList) then
          CountWrapped
        else
        begin
                  {If WrapTextToList didn't succed at all, then do it the
                   primitive way}
          while Length(Text) > 0 do
          begin
            AStr := Copy(Text, 1, FMaxCol);
            Delete(Text, 1, FMaxCol);
            if Length(Text) > 0 then
              YPos := YPos + FLineHeight;
          end;
        end;
      end;
      for j := 0 to AList.Count - 1 do
        TWrapPos(AList[j]).Free;
      AList.Free;
    end;
    YPos := YPos + FLineHeight;
  end;
  FPagesCounted := True;
end;

{ Writes the line number. FMargins. PLeft is the position of the left margin
  (which is automatically incremented by the length of the linenumber text, if
  the linenumbers should not be placed in the margin) }
procedure TSynEditPrint.WriteLineNumber;
var
  AStr: string;
begin
  SaveCurrentFont;
  AStr := IntToStr(FLineNumber + FLineOffset) + ': ';
  FCanvas.Brush.Color := FDefaultBG; 
  FCanvas.Font.Style := [];
  FCanvas.Font.Color := clBlack;
  FCanvas.TextOut(FMargins.PLeft - FCanvas.TextWidth(AStr), FYPos, AStr);
  RestoreCurrentFont;
end;

procedure TSynEditPrint.HandleWrap(const Text: string; MaxWidth: Integer);
var
  AStr: string;
  AList: TList;
  j: Integer;

  procedure WrapPrimitive;
  var
    Int: Integer;
    WrapPos: TWrapPos;
  begin
    Int := 1;
    while Int <= Length(Text) do
    begin
      AStr := '';
      while (Length(AStr) < FMaxCol) and (Int <= Length(Text)) do
      begin
        AStr := AStr + Text[Int];
        Int := Int + 1;
      end;
      WrapPos := TWrapPos.Create;
      WrapPos.Index := Int - 1;
      AList.Add(WrapPos);
      if (Length(AStr) - Int) <= FMaxCol then
        Break;
    end;
  end;
  
begin
  AStr := '';
  //First try to break the string at the following chars:
  AList := TList.Create;
  if WrapTextEx(Text, [' ', '-', #9, ','], FMaxCol, AList) then
    TextOut(Text, AList)
  else
  begin
      //Then try to break the string at the following chars:
    if WrapTextEx(Text, [';', ')', '.'], FMaxCol, AList) then
      TextOut(Text, AList)
    else
    begin
      WrapPrimitive;
      TextOut(Text, AList)
    end;
  end;
  for j := 0 to AList.Count - 1 do
    TWrapPos(Alist[j]).Free;
  AList.Free;
end;

procedure TSynEditPrint.SaveCurrentFont;
begin
  FOldFont.Assign(FCanvas.Font);
end;

procedure TSynEditPrint.RestoreCurrentFont;
begin
  FCanvas.Font.Assign(FOldFont);
end;

function TSynEditPrint.ClipLineToRect(Str: string; R: TRect): string;
begin
 while FCanvas.TextWidth(Str) > FMaxWidth do
    SetLength(Str, Length(Str) - 1);

  Result := Str;
end;

//Does the actual printing
procedure TSynEditPrint.TextOut(const Text: string; AList: TList);
var
  Token: string;
  TokenPos: Integer;
  Attr: TSynHighlighterAttributes;
  AColor: TColor;
  TokenStart: Integer;
  LCount: Integer;
  Handled: Boolean;
  aStr: string;
  Int, WrapPos, OldWrapPos: Integer;
  Lines: TStringList;
  ClipRect: TRect;
  sLine, sLineExpandedAtWideGlyphs: string;
  ExpandedPos: Integer;

  procedure InitETODist(CharWidth: Integer; const Text: string);
  var
    Size: TSize;
    Int: Integer;
  begin
    ReallocMem(FETODist, Length(Text) * SizeOf(Integer));
    for Int := 0 to Length(Text) - 1 do
    begin
      Size := GetTextSize(FCanvas.Handle, @Text[Int + 1], 1);
      // Introduce a small tolerance (#54)
      FETODist[Int] := Ceil(Size.cx / CharWidth - 0.04) * CharWidth;
    end;
  end;
  
  procedure ClippedTextOut(X, Y: Integer; Text: string);
  begin
    Text := ClipLineToRect(Text, ClipRect);
    InitETODist(FCharWidth, Text);
    Windows.ExtTextOutW(FCanvas.Handle, X, Y, 0, nil, PWideChar(Text),
      Length(Text), PInteger(FETODist));
  end;

  procedure SplitToken;
  var
    AStr: string;
    Last: Integer;
    FirstPos: Integer;
    TokenEnd: Integer;
  begin
    Last := TokenPos;
    FirstPos := TokenPos;
    TokenEnd := TokenPos + Length(Token);
    while (LCount < AList.Count) and (TokenEnd > TWrapPos(AList[LCount]).Index) do
    begin
      AStr := Copy(Text, Last + 1, TWrapPos(AList[LCount]).Index - Last);
      Last := TWrapPos(AList[LCount]).Index;
      ExpandedPos := FHighlighter.PosToExpandedPos(FirstPos);
      ClippedTextOut(FMargins.PLeft + ExpandedPos * FCharWidth, FYPos, AStr);
      FirstPos := 0;
      LCount := LCount + 1;
      FYPos := FYPos + FLineHeight;
    end;
    AStr := Copy(Text, Last + 1, TokenEnd - Last);
    ExpandedPos := FHighlighter.PosToExpandedPos(FirstPos);
    ClippedTextOut(FMargins.PLeft + ExpandedPos * FCharWidth, FYPos, AStr);
    //Ready for next token:
    TokenStart := TokenPos + Length(Token) - Length(AStr);
  end;
begin
  with FMargins do
    ClipRect := Rect(PLeft, PTop, PRight, PBottom);

  if FSynOK then
  begin
    SaveCurrentFont;
    FHighlighter.SetRange(FLines.Objects[FLineNumber - 1]);
    sLine := Text;
    sLineExpandedAtWideGlyphs := ExpandAtWideGlyphs(sLine);
    FHighlighter.SetLineExpandedAtWideGlyphs(sLine, sLineExpandedAtWideGlyphs, FLineNumber);

    Token := '';
    TokenStart := 0;
    LCount := 0;
    while not FHighLighter.GetEol do
    begin
      Token := FHighLighter.GetToken;
      TokenPos := FHighLighter.GetTokenPos;
      Attr := FHighLighter.GetTokenAttribute;
      if Assigned(Attr) then
      begin
        FCanvas.Font.Style := Attr.Style;
        if FColors then
        begin
          AColor := Attr.Foreground;
          if AColor = clNone then
            AColor := FFont.Color;
          FCanvas.Font.Color := AColor;
          AColor := Attr.Background;
          if AColor = clNone then
            AColor := FDefaultBG;
          FCanvas.Brush.Color := AColor;
        end
        else
        begin
          FCanvas.Font.Color := fFontColor;                                     
          FCanvas.Brush.Color := FDefaultBG;
        end;
      end
      else
      begin
        FCanvas.Font.Color := fFontColor;                                       
        FCanvas.Brush.Color := FDefaultBG;
      end;
      Handled := False;
      if Assigned(AList) then
      begin
        if (LCount < AList.Count) then
        begin
          //Split between tokens:
          if (TokenPos >= TWrapPos(AList[LCount]).Index) then
          begin
            LCount := LCount + 1;
            TokenStart := TokenPos;
            FYPos := FYPos + FLineHeight;
          end
          else
          begin
            //Split in the middle of a token:
            if (TokenPos + Length(Token) > TWrapPos(AList[LCount]).Index) then begin
              Handled := True;
              SplitToken;
            end;
          end;
        end;
      end;
      if not Handled then
      begin
        ExpandedPos := FHighLighter.PosToExpandedPos(TokenPos - TokenStart);
        ClippedTextOut(FMargins.PLeft + ExpandedPos * FCharWidth, FYPos, Token);
      end;
      FHighLighter.Next;
    end;
    RestoreCurrentFont;
  end
  else
  begin
    Lines := TStringList.Create;
    try
      OldWrapPos := 0;
      if Assigned(AList) then
        for Int := 0 to AList.Count - 1 do
        begin
          WrapPos := TWrapPos(AList[Int]).Index;
          if Int = 0 then
            AStr := Copy(Text, 1, WrapPos)
          else
            AStr := Copy(Text, OldWrapPos + 1, WrapPos - OldWrapPos);
          Lines.Add(AStr);
          OldWrapPos := WrapPos;
        end;
      if Length(Text) > 0 then
        Lines.Add(Copy(Text, OldWrapPos + 1, MaxInt));

      for Int := 0 to Lines.Count - 1 do
      begin
        ClippedTextOut(FMargins.PLeft, FYPos, Lines[Int]);
        if Int < Lines.Count - 1 then
          FYPos := FYPos + FLineHeight;
      end;
    finally
      Lines.Free;
    end
  end
end;

procedure TSynEditPrint.WriteLine(const Text: string);
var
  StrWidth: Integer;
begin
  if FLineNumbers then WriteLineNumber;
  StrWidth := FCanvas.TextWidth(Text);
  {Note that MaxWidth is calculated, using FTestString found in CalcPages -
   else the length is not calculated correctly when prewiewing and the
   zoom is different from 0.25,0.5,1,2,4 (as for example 1.20) - WHY???
  }
  if Wrap and (StrWidth > FMaxWidth) then
    HandleWrap(Text, FMaxWidth)
  else
    TextOut(Text, nil);
  FYPos := FYPos + FLineHeight;
end;

procedure TSynEditPrint.PrintPage(Num: Integer);
//Prints a page
var
  Int, iEnd: Integer;
  iSelStart, iSelLen: Integer;
begin
  PrintStatus(psNewPage, Num, FAbort);
  if not FAbort then
  begin
    FCanvas.Brush.Color := Color;
    with FMargins do
      FCanvas.FillRect(Rect(PLeft, PTop, PRight, PBottom));
    FMargins.InitPage(FCanvas, Num, FPrinterInfo, FLineNumbers,
      FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
    FHeader.Print(FCanvas, Num + FPageOffset);
    if FPages.Count > 0 then
    begin
      FYPos := FMargins.PTop;
      if Num = FPageCount then
        iEnd := FLines.Count - 1
      else
        iEnd := TPageLine(FPages[Num]).FirstLine - 1;
      for Int := TPageLine(FPages[Num - 1]).FirstLine to iEnd do
      begin
        FLineNumber := Int + 1;
        if (not fSelectedOnly or ((Int >= fBlockBegin.Line - 1) and (Int <= fBlockEnd.Line - 1))) then begin
          if (not fSelectedOnly or (fSelMode = smLine)) then
            WriteLine(Lines[Int])
          else
          begin
            if (fSelMode = smColumn) or (Int = fBlockBegin.Line -1) then
              iSelStart := fBlockBegin.Char
            else
              iSelStart := 1;
            if (fSelMode = smColumn) or (Int = fBlockEnd.Line -1) then
              iSelLen := fBlockEnd.Char  - iSelStart
            else
              iSelLen := MaxInt;
            WriteLine( Copy( Lines[Int], iSelStart, iSelLen ) );
          end;
          PrintLine(Int + 1, Num);
        end;
      end;
    end;
    FFooter.Print(FCanvas, Num + FPageOffset);
  end;
end;

procedure TSynEditPrint.UpdatePages(ACanvas: TCanvas);
//Update pages (called explicitly by preview component)
begin
  FCanvas := ACanvas;
  FPrinterInfo.UpdatePrinter;
  InitPrint;
end;

procedure TSynEditPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
//Print to specified canvas. Used by preview component
begin
  FAbort := False;
  FPrinting := False;
  FCanvas := ACanvas;
  PrintPage(PageNumber);
end;

procedure TSynEditPrint.Print;
begin
  PrintRange(1, -1);
end;

procedure TSynEditPrint.PrintRange(StartPage, EndPage: Integer);
//Prints the pages in the specified range
var
  Int, ii: Integer;
begin
  if fSelectedOnly and not fSelAvail then
    Exit;

  FPrinting := True;
  FAbort := False;
  // The next part sets the document title that is used by the printer queue.
  if FDocTitle <> '' then
    Printer.Title := FDocTitle
  else
    Printer.Title := FTitle;
  Printer.BeginDoc;
  PrintStatus(psBegin, StartPage, FAbort);
  UpdatePages(Printer.Canvas);

  for ii:=1 to Copies do
  begin
    Int := StartPage;
    if EndPage < 0 then
      EndPage := FPageCount;
    while (Int <= EndPage) and (not FAbort) do begin
      PrintPage(Int);
      if ((Int < EndPage) or (ii<Copies)) and not FAbort then
        Printer.NewPage;
      Int := Int + 1;
    end;
  end;
  if not FAbort then
    PrintStatus(psEnd, EndPage, FAbort);
  Printer.EndDoc;
  FPrinting := False;
end;

procedure TSynEditPrint.PrintLine(LineNumber, PageNumber: Integer);
//Fires the OnPrintLine event
begin
  if Assigned(FOnPrintLine) then
    FOnPrintLine(Self, LineNumber, PageNumber);
end;

procedure TSynEditPrint.PrintStatus(Status: TSynPrintStatus;
  PageNumber: Integer; var Abort: Boolean);
//Fires the OnPrintStatus event
begin
  Abort := False;
  if Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, Status, PageNumber, Abort);
  if Abort then begin
    if FPrinting then
      Printer.Abort;
  end;
end;

function TSynEditPrint.GetPageCount: Integer;
{Returns total page count. If pages hasn't been counted before,
 then a UpdatePages is called with a temporary canvas}
var
  TmpCanvas: TCanvas;
  DC: HDC;
begin
  Result := 0;
  if FPagesCounted then
    Result := FPageCount
  else begin
    TmpCanvas := TCanvas.Create;
    try
      DC := GetDC(0);
      try
        if DC <> 0 then
        begin
          TmpCanvas.Handle := DC;
          UpdatePages(TmpCanvas);
          TmpCanvas.Handle := 0;
          Result := FPageCount;
          FPagesCounted := True;
        end;
      finally
        ReleaseDC(0, DC);
      end;
    finally
      TmpCanvas.Free;
    end;
  end;
end;

procedure TSynEditPrint.SetSynEdit(const Value: TCustomSynEdit);
begin
//  Lines := Value.Lines;
  HighLighter := Value.Highlighter;
  Font := Value.Font;
  FTabWidth := Value.TabWidth;
  Lines := Value.Lines;
  fSelAvail := Value.SelAvail;
  fBlockBegin := Value.BlockBegin;
  fBlockEnd := Value.BlockEnd;
  fSelMode := Value.SelectionMode;
end;

procedure TSynEditPrint.LoadFromStream(AStream: TStream);
var
  Len, BufferSize: Integer;
  Buffer: PWideChar;
begin
  FHeader.LoadFromStream(AStream);
  FFooter.LoadFromStream(AStream);
  FMargins.LoadFromStream(AStream);
  with AStream do
  begin
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div sizeof(WideChar)] := #0;
      FTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div sizeof(WideChar)] := #0;
      FDocTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(FWrap, SizeOf(FWrap));
    Read(FHighlight, SizeOf(FHighlight));
    Read(FColors, SizeOf(FColors));
    Read(FLineNumbers, SizeOf(FLineNumbers));
    Read(FLineOffset, SizeOf(FLineOffset));
    Read(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TSynEditPrint.SaveToStream(AStream: TStream);
var
  aLen: Integer;
begin
  FHeader.SaveToStream(AStream);
  FFooter.SaveToStream(AStream);
  FMargins.SaveToStream(AStream);
  with AStream do
  begin
    aLen := Length(FTitle);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(FTitle)^, aLen * sizeof(WideChar));
    aLen := Length(FDocTitle);
    Write(aLen, SizeOf(aLen));
    Write(PWideChar(FDocTitle)^, aLen * sizeof(WideChar));
    Write(FWrap, SizeOf(FWrap));
    Write(FHighlight, SizeOf(FHighlight));
    Write(FColors, SizeOf(FColors));
    Write(FLineNumbers, SizeOf(FLineNumbers));
    Write(FLineOffset, SizeOf(FLineOffset));
    Write(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TSynEditPrint.SetFooter(const Value: TFooter);
begin
  FFooter.Assign(Value);
end;

procedure TSynEditPrint.SetHeader(const Value: THeader);
begin
  FHeader.Assign(Value);
end;

procedure TSynEditPrint.SetMargins(const Value: TSynEditPrintMargins);
begin
  FMargins.Assign(Value);
end;

end.

