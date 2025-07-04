{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPrintHeaderFooter.pas, released 2000-06-01.

The Initial Author of the Original Code is Morten J. Skovrup.
Portions written by Morten J. Skovrup are copyright 2000 Morten J. Skovrup.
Portions written by Michael Hieke are copyright 2000 Michael Hieke.
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

$Id: SynEditPrintHeaderFooter.pas,v 1.10.2.7 2008/09/23 14:02:08 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
CONTENTS:
  Classes handling info about headers and footers.

  THeaderFooterItem:
    Class handling an item in a header or footer. An item has a text,Font,
    LineNumber and Alignment (i.e. two items can be on the same line but have
    different fonts). Used internally.

  THeaderFooter:
    Collection of THeaderFooterItem's
    Design-time properties:
      FrameTypes : Frame around header or footer - can be any combination of:
                   ftLine   : Line under header or line above footer
                   ftBox    : Box around header or footer
                   ftShaded : Filled box (without frame) around header or footer.
      ShadedColor : Fill color if ftShaded is in FrameTypes
      LineColor   : Color of line or box if ftLine or ftBox is in FrameTypes
      DefaultFont : Default font for THeaderFooterItem's. This can be used to
                    set the header/footer font once for all items.
      RomanNumbers : Print page numbers as Roman numbers.
      MirrorPosition : Mirror position of left/right aligned THeaderFooterItem's
                       Can be used when printing 2-sided.
    Run-time methods:
      function Add(Text: string; Font: TFont;
                   Alignment: TAlignment;
                   LineNumber: Integer) : Integer;
        Add a THeaderFooterItem. If Font is nil or not specified then DefaultFont
        is used. Returned value is the index of the added item.
        The Text parameter can contain the following macros:
          $PAGECOUNT$  : Print total number of pages
          $PAGENUM$    : Print current page number
          $TITLE$      : Print the title
          $DATE$       : Print the date
          $TIME$       : Print the time
          $DATETIME$   : Print the date and then the time
          $TIMEDATE$   : Print the time and then the date
      procedure Delete(Index : Integer);
        Delete THeaderFooterItem with index Index.
      procedure Clear;
        Clear all THeaderFooterItems.
      function Count : Integer;
        Returns number of THeaderFooterItems.
      function Get(Index : Integer) : THeaderFooterItem;
        Returns THeaderFooterItem with Index.
      procedure SetPixPrInch(Value : Integer);
        Corrects the PixPerInch property of fonts. Used internally by
        TSynEditPrint.
      procedure InitPrint(ACanvas : TCanvas;NumPages : Integer; Title : string;
                          Margins : TSynEditPrintMargins);
        Prepares the header or footer for printing. Used internally by
        TSynEditPrint.
      procedure Print(ACanvas : TCanvas; PageNum : Integer = 0);
        Prints the header or footer. Used internally by TSynEditPrint.

-------------------------------------------------------------------------------}

unit SynEditPrintHeaderFooter;
{$M+}

{$I SynEdit.inc}

interface

uses
  Graphics,
  Classes,
  SynEditPrintTypes,
  SynEditPrintMargins;

type
  //An item in a header or footer. An item has a text,Font,LineNumber and
  //Alignment (i.e. two items can be on the same line but have different
  //fonts).
  THeaderFooterItem = class
  private
    FText: string;
    FFont: TFont;
    FLineNumber: Integer;
    FAlignment: TAlignment;
        {Used to store the original Index when the item was added - the index
         might change when the list is sorted}
    FIndex: Integer;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    function GetText(NumPages, PageNum: Integer; Roman: Boolean;
      Title, ATime, ADate: string): string;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  public
    property Alignment: TAlignment read FAlignment write FAlignment;
    property AsString: string read GetAsString write SetAsString;
    property Font: TFont read FFont write SetFont;
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property Text: string read FText write FText;
  end;

  THeaderFooterType = (hftHeader, hftFooter);

  //Used internally to calculate line height and font-base-line for header and
  //footer
  TLineInfo = class
  public
    LineHeight: Integer;
    MaxBaseDist: Integer;
  end;

  //The header/footer class
  THeaderFooter = class(TPersistent)
  private
    FType: THeaderFooterType; // Indicates if header or footer
    FFrameTypes: TFrameTypes;
    FShadedColor: TColor;
    FLineColor: TColor;
    FItems: TList;
    FDefaultFont: TFont;
    FDate, FTime: string;
    FNumPages: Integer;
    FTitle: string;
    FMargins: TSynEditPrintMargins;
    FFrameHeight: Integer;
    FOldPen: TPen;
    FOldBrush: TBrush;
    FOldFont: TFont;
    FRomanNumbers: Boolean;
    FLineInfo: TList;
    FLineCount: Integer;
    FMirrorPosition: Boolean;
    procedure SetDefaultFont(const Value: TFont);
    procedure DrawFrame(ACanvas: TCanvas);
    procedure CalcHeight(ACanvas: TCanvas);
    procedure SaveFontPenBrush(ACanvas: TCanvas);
    procedure RestoreFontPenBrush(ACanvas: TCanvas);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Text: string; Font: TFont; Alignment: TAlignment;
      LineNumber: Integer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;
    function Get(Index: Integer): THeaderFooterItem;
    procedure SetPixPrInch(Value: Integer);
    procedure InitPrint(ACanvas: TCanvas; NumPages: Integer; Title: string;
      Margins: TSynEditPrintMargins);
    procedure Print(ACanvas: TCanvas; PageNum: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure FixLines;
    property AsString: string read GetAsString write SetAsString;
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property FrameTypes: TFrameTypes read FFrameTypes write FFrameTypes
    default [ftLine];
    property ShadedColor: TColor read FShadedColor write FShadedColor
    default clSilver;
    property LineColor: TColor read FLineColor write FLineColor default clBlack;
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    property RomanNumbers: Boolean read FRomanNumbers write FRomanNumbers
    default False;
    property MirrorPosition: Boolean read FMirrorPosition write FMirrorPosition
    default False;
  end;

  //The header and footer - does nothing but set the value of FType in
  //THeaderFooter
  THeader = class(THeaderFooter)
  public
    constructor Create;
  end;

  TFooter = class(THeaderFooter)
  public
    constructor Create;
  end;

implementation

uses
  Math,
  Windows,
  SysUtils,
  UITypes,
  SynEditMiscProcs;

// Helper routine for AsString processing.
function GetFirstEl(var Value: string; Delim: WideChar): string;
var
  Posi: Integer;
begin
  Posi := Pos(Delim, Value);
  if Posi = 0 then
    Posi := Length(Value) + 1;
  Result := Copy(Value, 1, Posi - 1);
  Delete(Value, 1, Posi);
end;


{ THeaderFooterItem }

constructor THeaderFooterItem.Create;
begin
  inherited;
  FFont := TFont.Create;
end;

destructor THeaderFooterItem.Destroy;
begin
  inherited;
  FFont.Free;
end;

// Returns string representation of THeaderFooterItem to alleviate storing
// items into external storage (registry, ini file).
function THeaderFooterItem.GetAsString: string;
begin
  Result :=
    EncodeString(FText) + '/' +
    IntToStr(FFont.Charset) + '/' +
    IntToStr(FFont.Color) + '/' +
    IntToStr(FFont.Height) + '/' +
    EncodeString(FFont.Name) + '/' +
    IntToStr(Ord(FFont.Pitch)) + '/' +
    IntToStr(FFont.PixelsPerInch) + '/' +
    IntToStr(FFont.Size) + '/' +
    IntToStr(byte(FFont.Style)) + '/' +
    IntToStr(FLineNumber) + '/' +
    IntToStr(Ord(FAlignment));
end;


{ This is basically copied from original SynEditPrint.pas. Returns the
  header/footer text with macros expanded }
function THeaderFooterItem.GetText(NumPages, PageNum: Integer;
  Roman: Boolean; Title, ATime, ADate: string): string;
var
  Len, Start, Run: Integer;
  AStr: string;

  procedure DoAppend(AText: string);
  begin
    Result := Result + AText;
  end;
  procedure TryAppend(var First: Integer; After: Integer);
  begin
    if After > First then
    begin
      DoAppend(Copy(AStr, First, After - First));
      First := After;
    end;
  end;
  function TryExecuteMacro: Boolean;
  var
    Macro: string;
  begin
    Result := True;
    Macro := SysUtils.AnsiUpperCase(Copy(FText, Start, Run - Start + 1));
    if Macro = '$PAGENUM$' then
    begin
      if Roman then
        DoAppend(IntToRoman(PageNum))
      else
        DoAppend(IntToStr(PageNum));
      Exit;
    end;
    if Macro = '$PAGECOUNT$' then
    begin
      if Roman then
        DoAppend(IntToRoman(NumPages))
      else
        DoAppend(IntToStr(NumPages));
      Exit;
    end;
    if Macro = '$TITLE$' then
    begin
      DoAppend(Title);
      Exit;
    end;
    if Macro = '$DATE$' then
    begin
      DoAppend(ADate);
      Exit;
    end;
    if Macro = '$TIME$' then
    begin
      DoAppend(ATime);
      Exit;
    end;
    if Macro = '$DATETIME$' then
    begin
      DoAppend(ADate + ' ' + ATime);
      Exit;
    end;
    if Macro = '$TIMEDATE$' then
    begin
      DoAppend(ATime + ' ' + ADate);
      Exit;
    end;
    Result := False;
  end;

begin
  Result := '';
  AStr := FText;
  if Trim(AStr) = '' then
    Exit;
  // parse the line
  Len := Length(AStr);
  if Len > 0 then
  begin
      // start with left-aligned text
    Start := 1;
    Run := 1;
    while Run <= Len do
    begin
          // test for embedded macro
      if AStr[Run] = '$' then
      begin
        TryAppend(Start, Run);
        Inc(Run);
          // search for next '$' which could mark the end of a macro
        while Run <= Len do begin
          if AStr[Run] = '$' then
          begin
            // if this is a macro execute it and skip the chars from output
            if TryExecuteMacro then
            begin
              Inc(Run); // also the '$'
              Start := Run;
              Break;
            end
            else
            begin
                // this '$' might again be the start of a macro
              TryAppend(Start, Run);
              Inc(Run);                                                         //ek 2001-08-02
            end;
          end
          else
            Inc(Run);
        end;
      end
      else
        Inc(Run);
    end;
    TryAppend(Start, Run);
  end;
end;

procedure THeaderFooterItem.LoadFromStream(AStream: TStream);
var
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  AName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  Len, BufferSize: Integer;
  Buffer: Pointer;
begin
  with AStream do
  begin
    Read(Len, sizeof(Len));
    BufferSize := Len * sizeof(WideChar);
    GetMem(Buffer, BufferSize + sizeof(WideChar));
    try
      Read(Buffer^, BufferSize);
      PWideChar(Buffer)[BufferSize div sizeof(WideChar)] := #0;
      FText := PWideChar(Buffer);
    finally
      FreeMem(Buffer);
    end;
    Read(FLineNumber, sizeof(FLineNumber));
    // font
    Read(aCharset, sizeof(aCharset));
    Read(aColor, sizeof(aColor));
    Read(aHeight, sizeof(aHeight));
    Read(BufferSize, sizeof(BufferSize));
    GetMem(Buffer, BufferSize + 1);
    try
      Read(Buffer^, BufferSize);
      PAnsiChar(Buffer)[BufferSize div sizeof(AnsiChar)] := #0;
      AName := string(PAnsiChar(Buffer));
    finally
      FreeMem(Buffer);
    end;
    Read(aPitch, sizeof(aPitch));
    Read(aSize, sizeof(aSize));
    Read(aStyle, sizeof(aStyle));
    FFont.Charset := aCharset;
    FFont.Color := aColor;
    FFont.Height := aHeight;
    FFont.Name := AName;
    FFont.Pitch := aPitch;
    FFont.Size := aSize;
    FFont.Style := aStyle;
    Read(FAlignment, sizeof(FAlignment));
  end;
end;

procedure THeaderFooterItem.SaveToStream(AStream: TStream);
var
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  AName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen: Integer;
begin
  with AStream do
  begin
    aLen := Length(FText);
    Write(aLen, sizeof(aLen));
    Write(PWideChar(FText)^, aLen * sizeof(WideChar));
    Write(FLineNumber, sizeof(FLineNumber));
    // font
    aCharset := FFont.Charset;
    aColor   := FFont.Color;
    aHeight  := FFont.Height;
    AName    := FFont.Name;
    aPitch   := FFont.Pitch;
    aSize    := FFont.Size;
    aStyle   := FFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(AName);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(AnsiString(AName))^, aLen);
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));
    Write(FAlignment, SizeOf(FAlignment));
  end;
end;

procedure THeaderFooterItem.SetAsString(const Value: string);
var
  Str: string;
  sty: TFontStyles;
begin
  Str := Value;
  FText := DecodeString(GetFirstEl(Str, '/'));
  FFont.Charset := StrToIntDef(GetFirstEl(Str, '/'), 0);
  FFont.Color := StrToIntDef(GetFirstEl(Str, '/'), 0);
  FFont.Height := StrToIntDef(GetFirstEl(Str, '/'), 0);
  FFont.Name := DecodeString(GetFirstEl(Str, '/'));
  FFont.Pitch := TFontPitch(StrToIntDef(GetFirstEl(Str, '/'), 0));
  FFont.PixelsPerInch := StrToIntDef(GetFirstEl(Str, '/'), 0);
  FFont.Size := StrToIntDef(GetFirstEl(Str, '/'), 0);
  byte(sty) := StrToIntDef(GetFirstEl(Str, '/'), 0);
  FFont.Style := sty;
  FLineNumber := StrToIntDef(GetFirstEl(Str, '/'), 0);
  FAlignment := TAlignment(StrToIntDef(GetFirstEl(Str, '/'), 0));
end;

procedure THeaderFooterItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{ THeaderFooter }

constructor THeaderFooter.Create;
begin
  inherited;
  FFrameTypes := [ftLine];
  FShadedColor := clSilver;
  FLineColor := clBlack;
  FItems := TList.Create;
  FDefaultFont := TFont.Create;
  FOldPen := TPen.Create;
  FOldBrush := TBrush.Create;
  FOldFont := TFont.Create;
  FRomanNumbers := False;
  FMirrorPosition := False;
  FLineInfo := TList.Create;
  with FDefaultFont do
  begin
    Name := 'Arial';
    Size := 10;
    Color := clBlack;
  end;
end;

destructor THeaderFooter.Destroy;
var
  Int: Integer;
begin
  Clear;
  FItems.Free;
  FDefaultFont.Free;
  FOldPen.Free;
  FOldBrush.Free;
  FOldFont.Free;
  for Int := 0 to FLineInfo.Count - 1 do
    TLineInfo(FLineInfo[Int]).Free;
  FLineInfo.Free;
  inherited;
end;

function THeaderFooter.Add(Text: string; Font: TFont;
  Alignment: TAlignment; LineNumber: Integer): Integer;
var
  AItem: THeaderFooterItem;
begin
  AItem := THeaderFooterItem.Create;
  if Font = nil then
    AItem.Font := FDefaultFont
  else
    AItem.Font := Font;
  AItem.Alignment := Alignment;
  AItem.LineNumber := LineNumber;
  AItem.FIndex := FItems.Add(AItem);
  AItem.Text := Text;
  Result := AItem.FIndex;
end;

procedure THeaderFooter.Delete(Index: Integer);
var
  Int: Integer;
begin
  for Int := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[Int]).FIndex = Index then
    begin
      FItems.Delete(Int);
      Break;
    end;
  end;
end;

procedure THeaderFooter.Clear;
var
  Int: Integer;
begin
  for Int := 0 to FItems.Count - 1 do
    THeaderFooterItem(FItems[Int]).Free;
  FItems.Clear;
end;

procedure THeaderFooter.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

{ Counts number of lines in header/footer and changes the line-number so they
  start with 1 (the user might add header/footer items starting at line 2) }
procedure THeaderFooter.FixLines;
var
  Int, CurLine: Integer;
  LineInfo: TLineInfo;
begin
  for Int := 0 to FLineInfo.Count - 1 do
    TLineInfo(FLineInfo[Int]).Free;
  FLineInfo.Clear;
  CurLine := 0;
  FLineCount := 0;
  for Int := 0 to FItems.Count - 1 do
  begin
    if THeaderFooterItem(FItems[Int]).LineNumber <> CurLine then
    begin
      CurLine := THeaderFooterItem(FItems[Int]).LineNumber;
      FLineCount := FLineCount + 1;
      LineInfo := TLineInfo.Create;
      FLineInfo.Add(LineInfo);
    end;
    THeaderFooterItem(FItems[Int]).LineNumber := FLineCount;
  end;
end;

{ Calculates the hight of the header/footer, finds the line height for each line
  and calculates the font baseline where text is to be written }
procedure THeaderFooter.CalcHeight(ACanvas: TCanvas);
var
  Int, CurLine: Integer;
  AItem: THeaderFooterItem;
  FOrgHeight: Integer;
  TextMetric: TTextMetric;
begin
  FFrameHeight := -1;
  if FItems.Count <= 0 then Exit;

  CurLine := 1;
  FFrameHeight := 0;
  FOrgHeight := FFrameHeight;
  for Int := 0 to FItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(FItems[Int]);
    if AItem.LineNumber <> CurLine then
    begin
      CurLine := AItem.LineNumber;
      FOrgHeight := FFrameHeight;
    end;
    ACanvas.Font.Assign(AItem.Font);
    GetTextMetrics(ACanvas.Handle, TextMetric);
    with TLineInfo(FLineInfo[CurLine - 1]), TextMetric do
    begin
      LineHeight := Max(LineHeight, ACanvas.TextHeight('W'));
      MaxBaseDist := Max(MaxBaseDist, tmHeight - tmDescent);
    end;
    FFrameHeight := Max(FFrameHeight, FOrgHeight + ACanvas.TextHeight('W'));
  end;
  FFrameHeight := FFrameHeight + 2 * FMargins.PHFInternalMargin;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
//Used to sort header/footer items
begin
  Result := THeaderFooterItem(Item1).LineNumber - THeaderFooterItem(Item2).LineNumber;
  if Result = 0 then
    Result := Integer(Item1) - Integer(Item2);
end;

procedure THeaderFooter.SetPixPrInch(Value: Integer);
var
  Int, TmpSize: Integer;
  AFont: TFont;
begin
  for Int := 0 to FItems.Count - 1 do
  begin
    AFont := THeaderFooterItem(FItems[Int]).Font;
    TmpSize := AFont.Size;
    AFont.PixelsPerInch := Value;
    AFont.Size := TmpSize;
  end;
end;

procedure THeaderFooter.InitPrint(ACanvas: TCanvas; NumPages: Integer; Title: string;
  Margins: TSynEditPrintMargins);
begin
  SaveFontPenBrush(ACanvas);
  FDate := DateToStr(Now);
  FTime := TimeToStr(Now);
  FNumPages := NumPages;
  FMargins := Margins;
  FTitle := Title;
  FItems.Sort(CompareItems);
  FixLines;
  CalcHeight(ACanvas);
  RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.SaveFontPenBrush(ACanvas: TCanvas);
begin
  FOldFont.Assign(ACanvas.Font);
  FOldPen.Assign(ACanvas.Pen);
  FOldBrush.Assign(ACanvas.Brush);
end;

procedure THeaderFooter.RestoreFontPenBrush(ACanvas: TCanvas);
begin
  ACanvas.Font.Assign(FOldFont);
  ACanvas.Pen.Assign(FOldPen);
  ACanvas.Brush.Assign(FOldBrush);
end;

procedure THeaderFooter.DrawFrame(ACanvas: TCanvas);
//Draws frame around header/footer
begin
  if (FrameTypes = []) then Exit;
  with ACanvas, FMargins do begin
    Pen.Color := LineColor;
    Brush.Color := ShadedColor;
    if ftShaded in FrameTypes then
      Brush.Style := bsSolid
    else
      Brush.Style := bsClear;
    if ftBox in FrameTypes then
      Pen.Style := psSolid
    else
      Pen.Style := psClear;
    if FrameTypes * [ftBox, ftShaded] <> [] then begin
      if FType = hftHeader then
        Rectangle(PLeft, PHeader - FFrameHeight, PRight, PHeader)
      else
        Rectangle(PLeft, PFooter, PRight, PFooter + FFrameHeight);
    end;
    if ftLine in FrameTypes then begin
      Pen.Style := psSolid;
      if FType = hftHeader then begin
        MoveTo(PLeft, PHeader);
        LineTo(PRight, PHeader);
      end
      else begin
        MoveTo(PLeft, PFooter);
        LineTo(PRight, PFooter);
      end
    end;
  end;
end;

procedure THeaderFooter.Print(ACanvas: TCanvas; PageNum: Integer);
var
  Int, X, Y, CurLine: Integer;
  AStr: string;
  AItem: THeaderFooterItem;
  OldAlign: UINT;
  TheAlignment: TAlignment;
begin
  if (FFrameHeight <= 0) then Exit; // No header/footer
  SaveFontPenBrush(ACanvas);
  DrawFrame(ACanvas);
  ACanvas.Brush.Style := bsClear;
  if FType = hftHeader then
    Y := FMargins.PHeader - FFrameHeight
  else
    Y := FMargins.PFooter;
  Y := Y + FMargins.PHFInternalMargin; // Add the specified internal margin

  CurLine := 1;
  for Int := 0 to FItems.Count - 1 do
  begin
    AItem := THeaderFooterItem(FItems[Int]);
    ACanvas.Font := AItem.Font;
    if AItem.LineNumber <> CurLine then
    begin
      Y := Y + TLineInfo(FLineInfo[CurLine - 1]).LineHeight;
      CurLine := AItem.LineNumber;
    end;
    AStr := AItem.GetText(FNumPages, PageNum, FRomanNumbers, FTitle, FTime, FDate);
      //Find the alignment of the header/footer item - check for MirrorPosition
    TheAlignment := AItem.Alignment;
    if MirrorPosition and ((PageNum mod 2) = 0) then
    begin
      case AItem.Alignment of
        taRightJustify: TheAlignment := taLeftJustify;
        taLeftJustify: TheAlignment := taRightJustify;
      end;
    end;
      //Find X-position of text
    with FMargins do begin
      X := PLeftHFTextIndent;
      case TheAlignment of
        taRightJustify: X := PRightHFTextIndent - ACanvas.TextWidth(AStr);
        taCenter: X := (PLeftHFTextIndent + PRightHFTextIndent - ACanvas.TextWidth(AStr)) div 2;
      end;
    end;
      {Aligning at base line - Fonts can have different size in headers and footers}
    OldAlign := SetTextAlign(ACanvas.Handle, TA_BASELINE);
    ExtTextOutW(ACanvas.Handle, X, Y + TLineInfo(FLineInfo[CurLine - 1]).MaxBaseDist,
      0, nil, PWideChar(AStr), Length(AStr), nil);
    SetTextAlign(ACanvas.Handle, OldAlign);
  end;
  RestoreFontPenBrush(ACanvas);
end;

procedure THeaderFooter.Assign(Source: TPersistent);
var
  Src: THeaderFooter;
  Int: Integer;
begin
  if (Source <> nil) and (Source is THeaderFooter) then begin
    Src := THeaderFooter(Source);
    Clear;
    FType := Src.FType;
    FFrameTypes := Src.FFrameTypes;
    FShadedColor := Src.FShadedColor;
    FLineColor := Src.FLineColor;
    for Int := 0 to Src.FItems.Count - 1 do begin
      with THeaderFooterItem(Src.FItems[Int]) do
        Add(Text, Font, Alignment, LineNumber);
    end;
    FDefaultFont.Assign(Src.FDefaultFont);
    FRomanNumbers := Src.FRomanNumbers;
    FMirrorPosition := Src.FMirrorPosition;
  end else
    inherited Assign(Source);
end;

function THeaderFooter.Count: Integer;
begin
  Result := FItems.Count;
end;

function THeaderFooter.Get(Index: Integer): THeaderFooterItem;
begin
  Result := THeaderFooterItem(FItems[Index]);
end;

function THeaderFooter.GetAsString: string;
var
  Int: Integer;
begin
  FixLines;
  Result := '';
  for Int := 0 to FItems.Count - 1 do begin
    if Result <> '' then Result := Result + '/';
    Result := Result + EncodeString(THeaderFooterItem(FItems[Int]).AsString);
  end; //for
end;

procedure THeaderFooter.SetAsString(const Value: string);
var
  item: THeaderFooterItem;
  Str: string;
begin
  Clear;
  item := THeaderFooterItem.Create;
  try
    Str := Value;
    while Str <> '' do
    begin
      item.AsString := DecodeString(GetFirstEl(Str, '/'));
      Add(item.Text, item.Font, item.Alignment, item.LineNumber);
    end; 
  finally
    item.Free;
  end;
end;

procedure THeaderFooter.LoadFromStream(AStream: TStream);
var
  Num, Int: Integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  AName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  bufSize: Integer;
  buffer: PAnsiChar;
begin
  with AStream do begin
    // read header/footer properties first
    Read(FFrameTypes, SizeOf(FFrameTypes));
    Read(FShadedColor, SizeOf(FShadedColor));
    Read(FLineColor, SizeOf(FLineColor));
    Read(FRomanNumbers, SizeOf(FRomanNumbers));
    Read(FMirrorPosition, SizeOf(FMirrorPosition));
    // font
    Read(aCharset, SizeOf(aCharset));
    Read(aColor, SizeOf(aColor));
    Read(aHeight, SizeOf(aHeight));
    Read(bufSize, SizeOf(bufSize));
    GetMem(buffer, bufSize+1);
    try
      Read(buffer^, bufSize);
      buffer[bufSize] := #0;
      AName := string(buffer);
    finally
      FreeMem(buffer);
    end;
    Read(aPitch, SizeOf(aPitch));
    Read(aSize, SizeOf(aSize));
    Read(aStyle, SizeOf(aStyle));
    FDefaultFont.Charset := aCharset;
    FDefaultFont.Color   := aColor;
    FDefaultFont.Height  := aHeight;
    FDefaultFont.Name    := AName;
    FDefaultFont.Pitch   := aPitch;
    FDefaultFont.Size    := aSize;
    FDefaultFont.Style   := aStyle;
    // now read in the items
    Read(Num, SizeOf(Num));
    while Num > 0 do
    begin
      // load headerfooter items from stream
      Int := Add('', nil, taLeftJustify, 1);
      Get(Int).LoadFromStream(AStream);
      Dec(Num);
    end;
  end;
end;

procedure THeaderFooter.SaveToStream(AStream: TStream);
var
  Int, Num: Integer;
  aCharset: TFontCharset;
  aColor: TColor;
  aHeight: Integer;
  AName: TFontName;
  aPitch: TFontPitch;
  aSize: Integer;
  aStyle: TFontStyles;
  aLen : Integer;
begin
  with AStream do begin
    // write the header/footer properties first
    Write(FFrameTypes, SizeOf(FFrameTypes));
    Write(FShadedColor, SizeOf(FShadedColor));
    Write(FLineColor, SizeOf(FLineColor));
    Write(FRomanNumbers, SizeOf(FRomanNumbers));
    Write(FMirrorPosition, SizeOf(FMirrorPosition));
    // font
    aCharset := FDefaultFont.Charset;
    aColor   := FDefaultFont.Color;
    aHeight  := FDefaultFont.Height;
    AName    := FDefaultFont.Name;
    aPitch   := FDefaultFont.Pitch;
    aSize    := FDefaultFont.Size;
    aStyle   := FDefaultFont.Style;
    Write(aCharset, SizeOf(aCharset));
    Write(aColor, SizeOf(aColor));
    Write(aHeight, SizeOf(aHeight));
    aLen := Length(AName);
    Write(aLen, SizeOf(aLen));
    Write(PAnsiChar(AnsiString(AName))^, Length(AName));
    Write(aPitch, SizeOf(aPitch));
    Write(aSize, SizeOf(aSize));
    Write(aStyle, SizeOf(aStyle));

    // now write the items
    Num := Count;
    Write(Num, SizeOf(Num));
    for Int := 0 to Num - 1 do
      Get(Int).SaveToStream(AStream);
  end;
end;

{ THeader }

constructor THeader.Create;
begin
  inherited;
  FType := hftHeader;
end;

{ TFooter }

constructor TFooter.Create;
begin
  inherited;
  FType := hftFooter;
end;

end.

