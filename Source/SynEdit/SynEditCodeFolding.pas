{ -------------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

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
  ------------------------------------------------------------------------------- }
unit SynEditCodeFolding;
{
   Introduction
   ============
   This unit adds code folding support for SynEdit.
   It blends well with the Synedit highligting infrastructure and provides
   fast and efficient code folding that can cope with files with tens of
   thousands of line without lags.

   Converting existing highlighters
   ================================

   To support code folding a Highlighter must inherit from
   TSynCustomCodeFoldingHighlighter and implement one abstact procedure
   ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer);
   For each line ScanForFoldRanges needs to call one of the following:
      FoldRanges.StartFoldRange
      FoldRanges.StopFoldRange
      FoldRanges.NoFoldInfo
   ScanForFoldRanges is called after the standard highlighter scanning has taken
   place so one can use the Range information stored inside LinesToScan, which
   is  a TSynEditStringList, to avoid duplicating effort.

   Initally two hightlighters have been converted SynHighlighterJScript and
   SynHighlighterPython, to serve as examples of adding code folding suppot to
   brace-based and indentation-based languagges.

   Alternatively, code folding support can be provided just by implementing
   the SynEdit OnScanForFoldRangesEvent event.

   Demo of Coding Folding
   ======================
   A Folding demo has been added that demonstrates the use of the JScript and
   Python highlighters as well as the use of the OnScanForFoldRangesEvent event
   to support code folding in C++ files.

   Synedit Commants and Shortcuts
   =========
   The following commands have been added:
     ecFoldAll, ecUnfoldAll, ecFoldNearest, ecUnfoldNearest, ecFoldLevel1,
     ecFoldLevel2, ecFoldLevel3,, ecUnfoldLevel1, ecUnfoldLevel2,
     ecUnfoldLevel3, ecFoldRegions

    The default customisable shortcuts are:
      AddKey(ecFoldAll, VK_OEM_MINUS, [ssCtrl, ssShift]);   //- _
      AddKey(ecUnfoldAll,  VK_OEM_PLUS, [ssCtrl, ssShift]); //= +
      AddKey(ecFoldNearest, VK_OEM_2, [ssCtrl]);  // Divide //'/'
      AddKey(ecUnfoldNearest, VK_OEM_2, [ssCtrl, ssShift]);
      AddKey(ecFoldLevel1, ord('K'), [ssCtrl], Ord('1'), [ssCtrl]);
      AddKey(ecFoldLevel2, ord('K'), [ssCtrl], Ord('2'), [ssCtrl]);
      AddKey(ecFoldLevel3, ord('K'), [ssCtrl], Ord('3'), [ssCtrl]);
      AddKey(ecUnfoldLevel1, ord('K'), [ssCtrl, ssShift], Ord('1'), [ssCtrl, ssShift]);
      AddKey(ecUnfoldLevel2, ord('K'), [ssCtrl, ssShift], Ord('2'), [ssCtrl, ssShift]);
      AddKey(ecUnfoldLevel3, ord('K'), [ssCtrl, ssShift], Ord('3'), [ssCtrl, ssShift]);

   Limitations
   ===========
   -  Code folding can not be used simultaneously with Wordwrap.  Synedit takes
      care of that.
   -  The code uses generic collections, so it cannot be used with Delphi
      versions prior to Delphi 2009.

   Improvements
   ============
   Although the code folding infrastructure is fairly complete, improvements
   can be made in providing the use with more painting options
   (folding hints etc.)

   Technical details
   =================
   The main code folding structure is TSynFoldRanges.  It contains a public
   TList<TSynFoldRange> (sorted by starting line numbers).  This list is used by
   Synedit to paint the gutter and lines, fold and unfold ranges etc.
   Internally, TSynFoldRange maintains a TList<TLineFoldInfo> that is modified
   during scanning.  The TList<TSynFoldRange> is reconstructed from the
   TList<TLineFoldInfo> only when it is necessary.

}
interface

uses
  Graphics,
  Classes,
  SysUtils,
  System.Generics.Collections,
  SynEditHighlighter;

type
  // Custom COde Folding Exception
  TSynCodeFoldingException = class(Exception)
  end;

  // A single fold
  // Important: FromLine, ToLine are 1-based
  TSynFoldRange = record
    FromLine: Integer; // Beginning line
    ToLine: Integer; // End line
    FoldType: Integer;  // Could be used by some complex highlighters
    Indent : Integer;   // Only used for Indent based folding (Python)
    Collapsed: Boolean; // Is collapsed?
  private
    function GetLinesCollapsed: Integer;
  public
    procedure Move(Count: Integer);
    property LinesCollapsed: Integer read GetLinesCollapsed;
    constructor Create(AFromLine : Integer; AToLine : Integer = -1;
      AFoldType : Integer = 1; AIndent : Integer = -1;
      ACollapsed : Boolean = False);
  end;

  PSynFoldRange = ^TSynFoldRange;

  {Support for indendation based code folding as in Python, F#}
  TSynCodeFoldingMode = (cfmStandard, cfmIndentation);

  TSynFoldRanges = class(TObject)
  {
    The main code folding data structure.
    Scanning affects the fFoldInfoList data structure
    SynEdit Painting is based on the fRanges structure
    fRanges gets updated from fFoldInfoList when needed
    Both fRanges and fFoldInfoList are kept sorted by FromLine
    Line indices in both fRanges and fFoldInfoList are 1-based
  }
  private
    type
      TFoldOpenClose = (focOpen, focClose, focCloseOpen);

      TLineFoldInfo = record
        Line : Integer;
        FoldOpenClose : TFoldOpenClose;
        FoldType : Integer;
        Indent : Integer;
        constructor Create(ALine : Integer;
          AFoldOpenClose : TFoldOpenClose = focOpen;
          AFoldType : Integer = 1; AIndent : Integer = -1);
      end;
  private
    fCodeFoldingMode : TSynCodeFoldingMode;
    fRangesNeedFixing : Boolean;
    fRanges: TList<TSynFoldRange>;
    fCollapsedState : TList<Integer>;
    fFoldInfoList : TList<TLineFoldInfo>;
    function Get(Index: Integer): TSynFoldRange;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    {utility routines}
    function FoldStartAtLine(Line: Integer): Boolean; overload;
    function FoldStartAtLine(Line: Integer; out Index: Integer): Boolean; overload;
    function CollapsedFoldStartAtLine(Line: Integer): Boolean; overload;
    function CollapsedFoldStartAtLine(Line: Integer; out Index: Integer): Boolean; overload;
    function FoldEndAtLine(Line: Integer)  : Boolean; overload;
    function FoldEndAtLine(Line: Integer; out Index: Integer)  : Boolean; overload;
    function FoldAroundLineEx(Line: Integer; WantCollapsed, AcceptFromLine,
      AcceptToLine: Boolean; out Index: Integer): Boolean;
    function CollapsedFoldAroundLine(Line: Integer): Boolean; overload;
    function CollapsedFoldAroundLine(Line: Integer; out Index: Integer): Boolean; overload;
    function FoldAroundLine(Line: Integer) : Boolean; overload;
    function FoldAroundLine(Line: Integer; out Index: Integer) : Boolean; overload;
    function FoldHidesLine(Line: Integer) : Boolean; overload;
    function FoldHidesLine(Line: Integer; out Index: Integer) : Boolean; overload;
    function FoldsAtLevel(Level : Integer) : TArray<Integer>;
    function FoldsOfType(aType : Integer) : TArray<Integer>;

    {Scanning support}
    procedure StoreCollapsedState; overload;
    procedure RestoreCollapsedState; overload;
    procedure StoreCollapsedState(Stream: TStream); overload;
    procedure RestoreCollapsedState(Stream: TStream); overload;
    procedure StartScanning;
    function  StopScanning(Lines : TStrings) : Boolean; // Returns True of Ranges were updated
    procedure AddLineInfo(ALine: Integer; AFoldType: Integer;
      AFoldOpenClose: TFoldOpenClose;  AIndent : Integer);
    procedure StartFoldRange(ALine : Integer; AFoldType : Integer; AIndent : Integer = -1);
    procedure StopFoldRange(ALine : Integer; AFoldType : Integer;  AIndent : Integer = -1);
    procedure StopStartFoldRange(ALine: Integer; AFoldType: Integer;  AIndent: Integer = -1);
    procedure NoFoldInfo(ALine : Integer);
    function  GetIndentLevel(Line : Integer) : Integer;
    procedure RecreateFoldRanges(Lines : TStrings);

    // plugin notifications and support routines
    function FoldLineToRow(Line: Integer): Integer;
    function FoldRowToLine(Row: Integer): Integer;
    function LinesInserted(aIndex: Integer; aCount: Integer): Integer;
    function LinesDeleted(aIndex: Integer; aCount: Integer): Integer;
    function LinesPutted(aIndex: Integer; aCount: Integer): Integer;
    procedure Reset;

    {Access to the internal FoldRange list routines}
    procedure AddByParts(AFoldType: Integer; AFromLine: Integer; AToLine: Integer = -1);
    procedure AddFoldRange(FoldRange: TSynFoldRange);
    property  CodeFoldingMode : TSynCodeFoldingMode
              read fCodeFoldingMode write fCodeFoldingMode;
    property Count: Integer read GetCount;
    property FoldRange[Index : Integer] : TSynFoldRange read Get; default;
    property Ranges: TList<TSynFoldRange> read fRanges;
  end;

  TSynCodeFoldingChangeEvent = procedure(Sender: TObject) of object;

  TSynCodeFolding = class(TPersistent)
    { Class to store and expose to the designer Code Folding properties }
  private
    fIndentGuides: Boolean;
    fCollapsedLineColor: TColor;
    fFolderBarLinesColor: TColor;
    fIndentGuidesColor: TColor;
    fShowCollapsedLine: Boolean;
    fShowHintMark : Boolean;
    fGutterShapeSize :  Integer;
    fOnChange : TSynCodeFoldingChangeEvent;
    procedure SetIndentGuides(const Value: Boolean);
    procedure SetCollapsedLineColor(const Value: TColor);
    procedure SetFolderBarLinesColor(const Value: TColor);
    procedure SetIndentGuidesColor(const Value: TColor);
    procedure SetShowCollapsedLine(const Value: Boolean);
    procedure SetShowHintMark(const Value: Boolean);
    procedure SetGutterShapeSize(const Value: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    function ScaledGutterShapeSize(PPI: Integer): Integer;
    property OnChange: TSynCodeFoldingChangeEvent read fOnChange write fOnChange;
  published
    // Size of the gutter shapes in pixels at 96 PPI
    property  GutterShapeSize: Integer read fGutterShapeSize
      write SetGutterShapeSize default 11;
    property CollapsedLineColor: TColor read fCollapsedLineColor
      write SetCollapsedLineColor default clGrayText;
    property FolderBarLinesColor: TColor read fFolderBarLinesColor
      write SetFolderBarLinesColor default clGrayText;
    property IndentGuidesColor: TColor read fIndentGuidesColor
      write SetIndentGuidesColor default clGray;
    property IndentGuides: Boolean read fIndentGuides write SetIndentGuides
      default True;
    property ShowCollapsedLine: Boolean read fShowCollapsedLine
      write SetShowCollapsedLine default False;
    property ShowHintMark: Boolean read fShowHintMark
      write SetShowHintMark default True;
  end;

  TSynCustomCodeFoldingHighlighter = class(TSynCustomHighlighter)
  protected
    // Utility functions
    function GetLineRange(Lines: TStrings; Line : Integer) : Pointer;
    function GetHighlighterAttriAtRowCol(const Lines : TStrings;
      const Line: Integer; const Char: Integer): TSynHighlighterAttributes;
    function GetHighlighterAttriAtRowColEx(const Lines : TStrings;
      const Line, Char: Integer;  var Token: string;
      var TokenType, Start: Integer; var Attri: TSynHighlighterAttributes): Boolean;
    function TabWidth(LinesToScan: TStrings) : Integer;
  public
    // Called when a Highlighter is assigned to Synedit;
    // No need to override except to change the SynCodeFoldingMode
    procedure InitFoldRanges(FoldRanges : TSynFoldRanges); virtual;
    // Called after Highlighter ranges have been set
    procedure ScanForFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings; FromLine: Integer; ToLine: Integer); virtual; abstract;
    // Called immediately after FoldRanges have been recreated
    // Override only if some finetuning of the FoldRanges is need.
    procedure AdjustFoldRanges(FoldRanges: TSynFoldRanges;
      LinesToScan: TStrings); virtual;
  end;

  const
    FoldRegionType: Integer = 99;

implementation

uses
  Winapi.Windows,
  System.Math,
  System.Generics.Defaults,
  SynEditTextBuffer;

{ TSynEditFoldRanges }

function TSynFoldRanges.CollapsedFoldAroundLine(Line: Integer): Boolean;
var
  Index: Integer;
begin
  Result := CollapsedFoldAroundLine(Line, Index);
end;

function TSynFoldRanges.CollapsedFoldAroundLine(Line: Integer;
  out Index: Integer): Boolean;
begin
  Result := FoldAroundLineEx(Line, True, False, False, Index);
end;

function TSynFoldRanges.CollapsedFoldStartAtLine(Line: Integer): Boolean;
var
  Index: Integer;
begin
  Result := CollapsedFoldStartAtLine(Line, Index);
end;

function TSynFoldRanges.CollapsedFoldStartAtLine(Line: Integer;
  out Index: Integer): Boolean;
begin
  Result := fRanges.BinarySearch(TSynFoldRange.Create(Line), Index);
  if Result then
    Result := Result and fRanges[Index].Collapsed;
end;

constructor TSynFoldRanges.Create;
begin
  inherited;
  fCodeFoldingMode := cfmStandard;

  fRanges := TList<TSynFoldRange>.Create(TComparer<TSynFoldRange>.Construct(
    function(const L, R: TSynFoldRange): Integer
    begin
      Result := L.FromLine - R.FromLine;
    end));

  fCollapsedState := TList<Integer>.Create;

  fFoldInfoList := TList<TLineFoldInfo>.Create(TComparer<TLineFoldInfo>.Construct(
    function(const L, R: TLineFoldInfo): Integer
    begin
      Result := L.Line - R.Line;
    end));
end;

destructor TSynFoldRanges.Destroy;
begin
  fRanges.Free;
  fCollapsedState.Free;
  fFoldInfoList.Free;
  inherited;
end;

function TSynFoldRanges.FoldAroundLine(Line: Integer): Boolean;
var
  Index: Integer;
begin
  Result := FoldAroundLine(Line, Index);
end;

function TSynFoldRanges.FoldAroundLine(Line: Integer;
  out Index: Integer): Boolean;
begin
  Result := FoldAroundLineEx(Line, False, False, False, Index);
end;

function TSynFoldRanges.FoldAroundLineEx(Line: Integer;
  WantCollapsed, AcceptFromLine, AcceptToLine: Boolean;
  out Index: Integer): Boolean;
var
  Int: Integer;
begin
  Result := False;
  for Int := 0 to fRanges.Count - 1 do
  begin
    with fRanges.List[Int] do
    begin
      if ((FromLine < Line) or ((FromLine <= Line) and AcceptFromLine)) and
        ((ToLine > Line) or ((ToLine >= Line) and AcceptToLine)) and
        (Collapsed = WantCollapsed) then
      begin
        Index := Int;
        Result := True;
      end;
      if FromLine > Line then
        Exit;
    end;
  end;
end;

function TSynFoldRanges.FoldEndAtLine(Line: Integer;
  out Index: Integer): Boolean;
var
  Int: Integer;
begin
  Result := False;
  for Int := 0 to fRanges.Count - 1 do
    with fRanges.List[Int] do
      if (ToLine = Line) then
      begin
        Index := Int;
        Result := True;
        Break;
      end
      else if FromLine > Line then
        Break; // sorted by line. don't bother scanning further
end;

function TSynFoldRanges.FoldEndAtLine(Line: Integer): Boolean;
var
  Index: Integer;
begin
   Result := FoldEndAtLine(Line, Index);
end;

function TSynFoldRanges.FoldHidesLine(Line: Integer): Boolean;
var
  Index: Integer;
begin
  Result := FoldHidesLine(Line, Index);
end;

function TSynFoldRanges.FoldHidesLine(Line: Integer;
  out Index: Integer): Boolean;
begin
  Result := FoldAroundLineEx(Line, True, False, True, Index);
end;

function TSynFoldRanges.FoldLineToRow(Line: Integer): Integer;
var
  Int: Integer;
  CollapsedTo: Integer;
begin
  Result := Line;
  CollapsedTo := 0;
  for Int := 0 to fRanges.Count - 1 do
    with fRanges.List[Int] do
    begin
      // fold after line
      if FromLine >= Line then
        Break;

      if Collapsed then
      begin
        // Line is found after fold
        if ToLine < Line then
        begin
          Dec(Result, Max(ToLine - Max(FromLine, CollapsedTo), 0));
          CollapsedTo := Max(CollapsedTo, ToLine);
          // Inside fold
        end
        else
        begin
          Dec(Result, Line - Max(FromLine, CollapsedTo));
          Break;
        end;
      end;
    end;
end;

function TSynFoldRanges.FoldRowToLine(Row: Integer): Integer;
var
  Int: Integer;
  CollapsedTo: Integer;
begin
  Result := Row;
  CollapsedTo := 0;
  for Int := 0 to fRanges.Count - 1 do
    with fRanges.List[Int] do
    begin
      if FromLine >= Result then
        Break;

      if Collapsed then
      begin
        Inc(Result, Max(ToLine - Max(FromLine, CollapsedTo), 0));
        CollapsedTo := Max(CollapsedTo, ToLine);
      end;
    end;
end;

function TSynFoldRanges.FoldsAtLevel(Level: Integer): TArray<Integer>;
{
   Returns an array of indices of folds with level = Level
   ignoring fold ranges of type FoldRegionType
}
var
  Int : Integer;
  FRStack : TList<Integer>;
  ResultList : TList<Integer>;

   procedure RemoveClosed(Line : Integer);
   var
     j : Integer;
   begin
     for j := FRStack.Count-1 downto 0 do
       if fRanges.List[FRStack[j]].ToLine <= Line then
         FRStack.Delete(j);
   end;
begin
  FRStack := TList<Integer>.Create;
  ResultList := TList<Integer>.Create;
  try
    for Int := 0 to fRanges.Count - 1 do
    begin
      if fRanges.List[Int].FoldType = FoldRegionType then
        Continue;
      RemoveClosed(fRanges.List[Int].FromLine);
      FRStack.Add(Int);
      if FRStack.Count = Level then
        ResultList.Add(Int);
    end;
    Result := ResultList.ToArray;
  finally
    FRStack.Free;
    ResultList.Free;
  end;
end;

function TSynFoldRanges.FoldsOfType(aType: Integer): TArray<Integer>;
{
   Returns an array of indices of folds with FoldType = aType
}
var
  Int : Integer;
  ResultList : TList<Integer>;
begin
  ResultList := TList<Integer>.Create;
  try
    for Int := 0 to fRanges.Count - 1 do
    begin
      if fRanges.List[Int].FoldType = aType then
        ResultList.Add(Int);
    end;
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TSynFoldRanges.FoldStartAtLine(Line: Integer): Boolean;
var
  Index: Integer;
begin
  Result := FoldStartAtLine(Line, Index);
end;

function TSynFoldRanges.FoldStartAtLine(Line: Integer; out Index: Integer): Boolean;
{
  If Result is False it Returns the First Index with Line greater than Line
}
begin
  Result := fRanges.BinarySearch(TSynFoldRange.Create(Line), Index);
end;

procedure TSynFoldRanges.AddByParts(AFoldType: Integer; AFromLine: Integer;
  AToLine: Integer);
var
  Index : Integer;
  FR : TSynFoldRange;
begin
  // Insert keeping the list sorted
  FR := TSynFoldRange.Create(AFromLine, AToLine, AFoldType);
  if FoldStartAtLine(AFromLine, Index) then
    fRanges.List[Index] := FR
  else
    fRanges.Insert(Index, FR);
end;

procedure TSynFoldRanges.AddFoldRange(FoldRange: TSynFoldRange);
begin
  fRanges.Add(FoldRange);
end;

procedure TSynFoldRanges.AddLineInfo(ALine: Integer; AFoldType: Integer;
  AFoldOpenClose: TFoldOpenClose; AIndent : Integer);
var
  LineFoldInfo: TLineFoldInfo;
  Index: Integer;
begin
  LineFoldInfo := TLineFoldInfo.Create(ALine, AFoldOpenClose, AFoldType, AIndent);

  // Insert keeping the list sorted
  if fFoldInfoList.BinarySearch(LineFoldInfo, Index) then
  begin
    fFoldInfoList.Insert(Index+1, LineFoldInfo);
    fRangesNeedFixing := True;

    {if (fFoldInfoList.List[Index].FoldOpenClose <> AFoldOpenClose) or
       (fFoldInfoList.List[Index].FoldType <> AFoldType) or
       (fFoldInfoList.List[Index].Indent <> AIndent) then
    begin
      fFoldInfoList.List[Index].FoldOpenClose := AFoldOpenClose;
      fFoldInfoList.List[Index].FoldType := AFoldType;
      fFoldInfoList.List[Index].Indent := AIndent;
      fRangesNeedFixing := True;
    end;}
  end
  else begin
    fFoldInfoList.Insert(Index, LineFoldInfo);
    fRangesNeedFixing := True;
  end;
end;

function TSynFoldRanges.Get(Index: Integer): TSynFoldRange;
begin
  Result := TSynFoldRange(fRanges[Index]);
end;

function TSynFoldRanges.GetCount: Integer;
begin
  Result := fRanges.Count;
end;


function TSynFoldRanges.GetIndentLevel(Line: Integer): Integer;
var
  Index : Integer;
  Int : Integer;
begin
  Result := -1;
  fFoldInfoList.BinarySearch(TLineFoldInfo.Create(Line), Index);
  // Search above Line
  for Int := Index - 1 downto 0 do
    if fFoldInfoList.List[Int].Indent >= 0 then begin
      Result := fFoldInfoList.List[Int].Indent;
      Break
    end;
end;

function TSynFoldRanges.LinesDeleted(aIndex, aCount: Integer): Integer;
{
  Adjust fFoldInfoList and fRanges
  aIndex is 0-based fFoldInfoList and fRanges are 1-based
  If needed recreate fRanges
}
var
  Int : Integer;
begin
  fRangesNeedFixing := False;

  Result := aCount;
  // Adjust fFoldInfoList
  // aIndex is 0-based fFoldInfoList is 1-based
  for Int := fFoldInfoList.Count - 1 downto 0 do
    with fFoldInfoList.List[Int] do
      if Line > aIndex + aCount then
         Dec(fFoldInfoList.List[Int].Line, aCount)
      else if Line > aIndex then begin
        fRangesNeedFixing := True;
        fFoldInfoList.Delete(Int);
      end else
         Break;

  for Int := fRanges.Count - 1 downto 0 do
    with fRanges.List[Int] do
      if (FromLine > aIndex + aCount) then
        // Move after affected area
        Ranges.List[Int].Move(-aCount)
      else if FromLine > aIndex then
      begin
        fRangesNeedFixing := True;
        fRanges.Delete(Int);
      end else if ToLine > aIndex + aCount then
        Dec(fRanges.List[Int].ToLine, aCount)
      else if ToLine > aIndex then
        Dec(fRanges.List[Int].ToLine, ToLine - aIndex)
end;

function TSynFoldRanges.LinesInserted(aIndex, aCount: Integer): Integer;
{
  Adjust fFoldInfoList and fRanges
  aIndex is 0-based fFoldInfoList and fRanges are 1-based
}
var
  Int: Integer;
begin
  Result := aCount;
  for Int := fFoldInfoList.Count - 1 downto 0 do
    with fFoldInfoList.List[Int] do
      if Line > aIndex then
         Inc(fFoldInfoList.List[Int].Line, aCount)
      else
         Break;

  for Int := fRanges.Count - 1 downto 0 do
    with fRanges.List[Int] do
    begin
      if (FromLine > aIndex) then // insertion of count lines above FromLine
        fRanges.List[Int].Move(aCount)
      else if (ToLine > aIndex) then
        Inc(fRanges.List[Int].ToLine,  aCount);
    end;
end;

function TSynFoldRanges.LinesPutted(aIndex, aCount: Integer): Integer;
begin
   Result := 1;
end;

procedure TSynFoldRanges.NoFoldInfo(ALine: Integer);
var
  Index: Integer;
begin
  if fFoldInfoList.BinarySearch(TLineFoldInfo.Create(ALine), Index)
  then
  begin
    // we have deleted an existing fold open or close mark
    fRangesNeedFixing := True;
    fFoldInfoList.Delete(Index);
  end
  else if (Index < fFoldInfoList.Count) and
    (fFoldInfoList.List[Index].Line = ALine + 1) and
    (fFoldInfoList.List[Index].FoldOpenClose = focCloseOpen)
  then
    fRangesNeedFixing := True;
end;

procedure TSynFoldRanges.RecreateFoldRanges(Lines: TStrings);
var
  OpenFoldStack: TList<Integer>;
  LFI: TLineFoldInfo;
  PFoldRange: PSynFoldRange;
  I: Integer;
  Line: Integer;
begin
   { TODO: Account for type }
  fRanges.Clear;

  OpenFoldStack := TList<Integer>.Create;
  try
    for LFI in fFoldInfoList do
    begin
      if LFI.FoldOpenClose in [focClose, focCloseOpen] then
      begin
        if LFI.Indent >= 0 then begin
          for I := OpenFoldStack.Count - 1 downto 0 do
          begin
            // Close all Fold Ranges with greater Indent
            PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
            if (PFoldRange^.Indent >= LFI.Indent) then begin
              PFoldRange^.ToLine := LFI.Line - 1; // Do not include Line
              OpenFoldStack.Delete(I);
              if PFoldRange^.FromLine = PFoldRange^.ToLine then
                fRanges.Remove(PFoldRange^);
            end;
          end;
        end
        else
          for I := OpenFoldStack.Count - 1 downto 0 do
          begin
            PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
            if (PFoldRange^.FoldType = LFI.FoldType) then begin
              PFoldRange^.ToLine := IfThen(LFI.FoldOpenClose = focClose,
                LFI.Line, LFI.Line - 1);
              OpenFoldStack.Delete(I);
              Break;
            end;
          end;
      end;

      if LFI.FoldOpenClose in [focOpen, focCloseOpen] then
      begin
        if LFI.Indent >= 0 then begin
          for I := OpenFoldStack.Count - 1 downto 0 do
          begin
            // Close all Fold Ranges with less Indent
            PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
            if (PFoldRange^.Indent >= LFI.Indent) then begin
              PFoldRange^.ToLine := LFI.Line - 1; // Do not include Line
              OpenFoldStack.Delete(I);
              if PFoldRange^.FromLine = PFoldRange^.ToLine then
                fRanges.Remove(PFoldRange^);
            end;
          end;
        end;
        fRanges.Add(TSynFoldRange.Create(LFI.Line, LFI.Line, LFI.FoldType, LFI.Indent));
        OpenFoldStack.Add(FRanges.Count -1);
      end;
    end;

    if CodeFoldingMode = cfmIndentation then
    begin
      // close all open indent based folds
      for I := OpenFoldStack.Count - 1 downto 0 do
      begin
        // Close all Fold Ranges with less Indent
        PFoldRange := @fRanges.List[OpenFoldStack.List[I]];
        if (PFoldRange^.Indent >= 0) then begin
          PFoldRange^.ToLine := Lines.Count; //
          OpenFoldStack.Delete(I);
        end;
      end;
      // Adjust LineTo for Indent based folds with empty lines in the end
      for I := fRanges.Count - 1 downto 0 do begin
        PFoldRange := @fRanges.List[I];
        if PFoldRange^.Indent >= 0 then
        begin
          Line := PFoldRange^.ToLine;
          while (Line > PFoldRange^.FromLine) and (TrimLeft(Lines[Line-1]) = '') do
          begin
            Dec(PFoldRange^.ToLine);
            Dec(Line);
          end;
          if PFoldRange^.FromLine = PFoldRange^.ToLine then
            fRanges.Delete(I);
        end;
      end;
    end;
  finally
    OpenFoldStack.Free;
  end;
end;

procedure TSynFoldRanges.Reset;
begin
  fRanges.Clear;
  fCollapsedState.Clear;
  fFoldInfoList.Clear;
  fRangesNeedFixing := False;
end;

procedure TSynFoldRanges.RestoreCollapsedState(Stream: TStream);
var
  Size, Line, Index : Integer;
begin
  Size := Stream.Size;
  while Stream.Position < Size do begin
    Stream.ReadData(Line);
    if FoldStartAtLine(Line, Index) then
      fRanges.List[Index].Collapsed := True;
  end;
end;

procedure TSynFoldRanges.RestoreCollapsedState;
var
  Int, Index : Integer;
begin
  for Int in fCollapsedState do begin
    if FoldStartAtLine(Int, Index) then
      fRanges.List[Index].Collapsed := True;
  end;
  fCollapsedState.Clear;
end;

procedure TSynFoldRanges.StartFoldRange(ALine, AFoldType: Integer;  AIndent : Integer);
begin
  AddLineInfo(ALine, AFoldType, focOpen, AIndent);
end;

procedure TSynFoldRanges.StartScanning;
begin
end;

procedure TSynFoldRanges.StopFoldRange(ALine, AFoldType: Integer; AIndent : Integer);
begin
  AddLineInfo(ALine, AFoldType, focClose, AIndent);
end;

procedure TSynFoldRanges.StopStartFoldRange(ALine, AFoldType: Integer; AIndent: Integer);
begin
  AddLineInfo(ALine, AFoldType, focCloseOpen, AIndent);
end;

function TSynFoldRanges.StopScanning(Lines : TStrings) : Boolean;
{
  Returns true if fold ranges changed
  Recreates FoldRanges if the Synedit lines are not updating
}
begin
  Result := fRangesNeedFixing;

  if Result then begin
    StoreCollapsedState;
    RecreateFoldRanges(Lines);
    RestoreCollapsedState;
    fRangesNeedFixing := False;
  end;
end;

procedure TSynFoldRanges.StoreCollapsedState(Stream: TStream);
var
  FoldRange : TSynFoldRange;
begin
  for FoldRange in fRanges do
    if FoldRange.Collapsed then
       Stream.WriteData(FoldRange.FromLine);
end;

procedure TSynFoldRanges.StoreCollapsedState;
var
  FoldRange : TSynFoldRange;
begin
  fCollapsedState.Clear;
  for FoldRange in fRanges do
    if FoldRange.Collapsed then
       fCollapsedState.Add(FoldRange.FromLine);
end;

{ TSynEditFoldRange }

constructor TSynFoldRange.Create(AFromLine, AToLine, AFoldType: Integer;
  AIndent : Integer; ACollapsed: Boolean);
begin
  FromLine := AFromLine;
  ToLine := AToLine;
  FoldType := AFoldType;
  Indent := AIndent;
  Collapsed := ACollapsed;
end;

function TSynFoldRange.GetLinesCollapsed: Integer;
begin
  if Collapsed then
    Result := ToLine - FromLine
  else
    Result := 0;
end;

procedure TSynFoldRange.Move(Count: Integer);
begin
  Inc(FromLine, Count);
  Inc(ToLine, Count);
end;

procedure TSynCodeFolding.Assign(Source: TPersistent);
begin
 if Source is TSynCodeFolding then
 begin
   fIndentGuides := TSynCodeFolding(Source).fIndentGuides;
   fCollapsedLineColor := TSynCodeFolding(Source).fCollapsedLineColor;
   fFolderBarLinesColor := TSynCodeFolding(Source).fFolderBarLinesColor;
   fIndentGuidesColor := TSynCodeFolding(Source).fIndentGuidesColor;
   fShowCollapsedLine := TSynCodeFolding(Source).fShowCollapsedLine;
   fShowHintMark := TSynCodeFolding(Source).fShowHintMark;
   fGutterShapeSize := TSynCodeFolding(Source).fGutterShapeSize;
 end
 else
   inherited Assign(Source);
end;

constructor TSynCodeFolding.Create;
begin
  fIndentGuides := True;
  fCollapsedLineColor := clGrayText;
  fFolderBarLinesColor := clGrayText;
  fIndentGuidesColor := clGray;
  fShowCollapsedLine := False;
  fShowHintMark := True;
  fGutterShapeSize := 11;
end;

function TSynCodeFolding.ScaledGutterShapeSize(PPI: Integer): Integer;
{ Always returns an odd number }
begin
  Result := MulDiv(fGutterShapeSize, PPI, 96);
  if not Odd(Result) then
    Dec(Result);
end;

{ TSynFoldRanges.TLineFoldInfo }

constructor TSynFoldRanges.TLineFoldInfo.Create(ALine: Integer;
  AFoldOpenClose: TFoldOpenClose; AFoldType: Integer; AIndent : Integer);
begin
    Line := ALine;
    FoldOpenClose := AFoldOpenClose;
    FoldType := AFoldType;
    Indent := AIndent;
end;

{ TSynCustomCodeFoldingHighlighter }

procedure TSynCustomCodeFoldingHighlighter.AdjustFoldRanges(
  FoldRanges: TSynFoldRanges; LinesToScan: TStrings);
begin
  // Do nothing
end;

function TSynCustomCodeFoldingHighlighter.GetHighlighterAttriAtRowCol(
  const Lines: TStrings; const Line: Integer;
  const Char: Integer): TSynHighlighterAttributes;
var
  Token: string;
  TokenType, Start: Integer;
begin
  GetHighlighterAttriAtRowColEx(Lines, Line, Char, Token, TokenType,
    Start, Result);
end;

function TSynCustomCodeFoldingHighlighter.GetHighlighterAttriAtRowColEx(
  const Lines: TStrings; const Line, Char: Integer; var Token: string;
  var TokenType, Start: Integer; var Attri: TSynHighlighterAttributes): Boolean;
var
  LineText: string;
begin
  if  (Line >= 0) and (Line < Lines.Count) then
  begin
    LineText := Lines[Line];
    if Line = 0 then
      ResetRange
    else
      SetRange(TSynEditStringList(Lines).Ranges[Line - 1]);
    SetLine(LineText, Line);
    if (Char > 0) and (Char <= Length(LineText)) then
      while not GetEol do
      begin
        Start := GetTokenPos + 1;
        Token := GetToken;
        if (Char >= Start) and (Char < Start + Length(Token)) then
        begin
          Attri := GetTokenAttribute;
          TokenType := GetTokenKind;
          Result := True;
          Exit;
        end;
        Next;
      end;
  end;
  Token := '';
  Attri := nil;
  Result := False;
end;

function TSynCustomCodeFoldingHighlighter.GetLineRange(Lines: TStrings;
  Line: Integer): Pointer;
begin
  if (Line >= 0) and (Line < Lines.Count) then
    Result := TSynEditStringList(Lines).Ranges[Line]
  else
    Result := nil;
end;

procedure TSynCustomCodeFoldingHighlighter.InitFoldRanges(
  FoldRanges: TSynFoldRanges);
begin
  FoldRanges.CodeFoldingMode := cfmStandard;
end;

function TSynCustomCodeFoldingHighlighter.TabWidth(
  LinesToScan: TStrings): Integer;
begin
  Result := TSynEditStringList(LinesToScan).TabWidth;
end;

{ TSynCodeFolding }

procedure TSynCodeFolding.SetCollapsedLineColor(const Value: TColor);
begin
  if fCollapsedLineColor <> Value then begin
    fCollapsedLineColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetFolderBarLinesColor(const Value: TColor);
begin
  if fFolderBarLinesColor <> Value then begin
    fFolderBarLinesColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetGutterShapeSize(const Value: Integer);
var
  NewValue: Integer;
begin
  NewValue := Value;
  if fGutterShapeSize <> NewValue then begin
    fGutterShapeSize := NewValue;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetIndentGuides(const Value: Boolean);
begin
  if fIndentGuides <> Value then begin
    fIndentGuides := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetIndentGuidesColor(const Value: TColor);
begin
  if fIndentGuidesColor <> Value then begin
    fIndentGuidesColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetShowHintMark(const Value: Boolean);
begin
  if fShowHintMark <> Value then begin
    fShowHintMark := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynCodeFolding.SetShowCollapsedLine(const Value: Boolean);
begin
  if fShowCollapsedLine <> Value then begin
    fShowCollapsedLine := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;


end.
