{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTextBuffer.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Unicode translation by Maël Hörz.
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

Known Issues:
-------------------------------------------------------------------------------}
//todo: Avoid calculating expanded string unncessarily (just calculate expandedLength instead).

unit SynEditTextBuffer;

{$I SynEdit.inc}

interface

uses
  SynEditTypes,
  SynEditMiscProcs,
  Classes,
  SysUtils;

type
  TSynEditRange = pointer;

  TSynEditStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TSynEditStringFlags = set of TSynEditStringFlag;

  PSynEditStringRec = ^TSynEditStringRec;
  TSynEditStringRec = record
    FString: string;
    fObject: TObject;
    fRange: TSynEditRange;
    fExpandedLength: Integer;
    fCharIndex : Integer;
    fFlags: TSynEditStringFlags;
  end;

  TSynEditTwoWideChars = record
    One, Two : WideChar;
  end;
  PSynEditTwoWideChars = ^TSynEditTwoWideChars;

const
  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  MaxSynEditStrings = MaxInt div SynEditStringRecSize;

  NullRange = TSynEditRange(-1);

type
  PSynEditStringRecList = ^TSynEditStringRecList;
  TSynEditStringRecList = array[0..MaxSynEditStrings - 1] of TSynEditStringRec;

  TStringListChangeEvent = procedure(Sender: TObject; Index: Integer;
    Count: Integer) of object;

  TExpandAtWideGlyphsFunc = function (const Str: string): string of object;

  TSynEditStringList = class(TStrings)
  private
    fList: PSynEditStringRecList;
    fCount: Integer;
    fCapacity: Integer;
    fFileFormat: TSynEditFileFormat;
    fConvertTabsProc: TConvertTabsProcEx;
    fIndexOfLongestLine: Integer;
    fTabWidth: Integer;
    fExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc;
    fCharIndexesAreValid : Boolean;
    fDetectUTF8: Boolean;
    fUTF8CheckLen: Integer;
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    fOnCleared: TNotifyEvent;
    fOnDeleted: TStringListChangeEvent;
    fOnInserted: TStringListChangeEvent;
    fOnPutted: TStringListChangeEvent;
    fOnInfoLoss: TSynInfoLossEvent;
    function ExpandString(Index: Integer): string;
    function GetExpandedString(Index: Integer): string;
    function GetExpandedStringLength(Index: Integer): Integer;
    function GetLengthOfLongestLine: Integer;
    function GetRange(Index: Integer): TSynEditRange;
    procedure Grow;
    procedure InsertItem(Index: Integer; const Str: string);
    procedure PutRange(Index: Integer; ARange: TSynEditRange);
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const Str: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetTabWidth(Value: Integer);
    procedure SetUpdateState(Updating: Boolean); override;
    procedure UpdateCharIndexes;
  public
    constructor Create(AExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc);
    destructor Destroy; override;
    function Add(const Str: string): Integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure DeleteLines(Index, NumLines: Integer);
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const Str: string); override;
    procedure InsertLines(Index, NumLines: Integer);
    procedure InsertStrings(Index: Integer; NewStrings: TStrings);
    procedure InsertText(Index: Integer; NewText: string);
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); override;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); override;
    function GetSeparatedText(Separators: string): string;
    procedure FontChanged;
    function LineCharLength(Index : Integer) : Integer;
    function LineCharIndex(Index : Integer) : Integer;
    procedure SetTextAndFileFormat(const Value: string);
    procedure SetEncoding(const Value: TEncoding); override;

    property FileFormat: TSynEditFileFormat read fFileFormat write fFileFormat;
    property ExpandedStrings[Index: Integer]: string read GetExpandedString;
    property ExpandedStringLengths[Index: Integer]: Integer read GetExpandedStringLength;
    property LengthOfLongestLine: Integer read GetLengthOfLongestLine;
    property Ranges[Index: Integer]: TSynEditRange read GetRange write PutRange;
    property TabWidth: Integer read fTabWidth write SetTabWidth;
    property UTF8CheckLen: Integer read fUTF8CheckLen write fUTF8CheckLen;
    property DetectUTF8: Boolean read fDetectUTF8 write fDetectUTF8;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TStringListChangeEvent read fOnDeleted write fOnDeleted;
    property OnInserted: TStringListChangeEvent read fOnInserted
      write fOnInserted;
    property OnPutted: TStringListChangeEvent read fOnPutted write fOnPutted;
    property OnInfoLoss: TSynInfoLossEvent read fOnInfoLoss write fOnInfoLoss;
  end;

  ESynEditStringList = class(Exception);

  // Note: several undo entries can be chained together via the ChangeNumber
  // see also TCustomSynEdit.[Begin|End]UndoBlock methods
  TSynChangeReason = (crInsert,
    crDelete, crSilentDelete,
    crLineBreak, crIndent, crUnindent,
    crAutoCompleteBegin, crAutoCompleteEnd,
    crPasteBegin, crPasteEnd, // for pasting, since it might do a lot of operations
    crSpecialBegin, crSpecialEnd,
    crCaret,      // just restore the Caret, allowing better Undo behavior
    crSelection,  // restore Selection
    crNothing,    // can be used to break group undo
    crGroupBreak,
    crDeleteAll,
    crWhiteSpaceAdd // for undo/redo of adding a character past EOL and repositioning the caret
    );

  TSynEditUndoItem = class(TPersistent)
  protected
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TBufferCoord;
    fChangeEndPos: TBufferCoord;
    fChangeStr: string;
    fChangeNumber: Integer;                                                     
  public
    procedure Assign(Source: TPersistent); override;
    property ChangeReason: TSynChangeReason read fChangeReason;
    property ChangeSelMode: TSynSelectionMode read fChangeSelMode;
    property ChangeStartPos: TBufferCoord read fChangeStartPos;
    property ChangeEndPos: TBufferCoord read fChangeEndPos;
    property ChangeStr: string read fChangeStr;
    property ChangeNumber: Integer read fChangeNumber;
  end;

  TSynEditUndoList = class(TPersistent)
  protected
    fBlockChangeNumber: Integer;
    fBlockCount: Integer;
    fFullUndoImposible: Boolean;
    fItems: TList;
    fLockCount: Integer;
    fMaxUndoActions: Integer;
    fNextChangeNumber: Integer;
    fInitialChangeNumber: Integer;
    fInsideRedo: Boolean;
    fOnAddedUndo: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: Boolean;
    function GetItemCount: Integer;
    procedure SetMaxUndoActions(Value: Integer);
    procedure SetInitialState(const Value: Boolean);
    function GetInitialState: Boolean;
    function GetItems(Index: Integer): TSynEditUndoItem;
    procedure SetItems(Index: Integer; const Value: TSynEditUndoItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(AReason: TSynChangeReason; const AStart, AEnd: TBufferCoord;
      const ChangeText: string; SelMode: TSynSelectionMode);
    procedure BeginBlock;                                                       
    procedure Clear;
    procedure EndBlock;
    procedure Lock;
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;
    function LastChangeReason: TSynChangeReason;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AddGroupBreak;
    procedure DeleteItem(AIndex: Integer);
    property BlockChangeNumber: Integer read fBlockChangeNumber
      write fBlockChangeNumber;
    property CanUndo: Boolean read GetCanUndo;
    property FullUndoImpossible: Boolean read fFullUndoImposible;
    property InitialState: Boolean read GetInitialState write SetInitialState;
    property Items[Index: Integer]: TSynEditUndoItem read GetItems write SetItems;
    property ItemCount: Integer read GetItemCount;
    property BlockCount: Integer read fBlockCount;
    property MaxUndoActions: Integer read fMaxUndoActions
      write SetMaxUndoActions;
    property InsideRedo: Boolean read fInsideRedo write fInsideRedo;
    property OnAddedUndo: TNotifyEvent read fOnAddedUndo write fOnAddedUndo;
  end;

implementation

uses SynUnicode;

resourcestring
  SListIndexOutOfBounds = 'Invalid stringlist index %d';
  SInvalidCapacity = 'Stringlist capacity cannot be smaller than count';

{ TSynEditStringList }

procedure ListIndexOutOfBounds(Index: Integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create(AExpandAtWideGlyphsFunc: TExpandAtWideGlyphsFunc);
begin
  inherited Create;
  fExpandAtWideGlyphsFunc := AExpandAtWideGlyphsFunc;
  fFileFormat := sffDos;
  fIndexOfLongestLine := -1;
  TabWidth := 8;
  fUTF8CheckLen := -1;
  Options := Options - [soWriteBOM, soTrailingLineBreak];
  fDetectUTF8 := True;
end;

destructor TSynEditStringList.Destroy;
begin
  fOnChange := nil;
  fOnChanging := nil;
  inherited Destroy;
  if fCount <> 0 then
    Finalize(fList^[0], fCount);
  fCount := 0;
  SetCapacity(0);
end;

function TSynEditStringList.Add(const Str: string): Integer;
begin
  BeginUpdate;
  Result := fCount;
  InsertItem(Result, Str);
  if Assigned(OnInserted) then
    OnInserted(Self, Result, 1);
  EndUpdate;
end;

procedure TSynEditStringList.AddStrings(Strings: TStrings);
var
  Int, FirstAdded: Integer;
begin
  if Strings.Count > 0 then begin
    fIndexOfLongestLine := -1;
    BeginUpdate;
    try
      Int := fCount + Strings.Count;
      if Int > fCapacity then
        SetCapacity((Int + 15) and (not 15));
      FirstAdded := fCount;
      for Int := 0 to Strings.Count - 1 do begin
        with fList^[fCount] do begin
          Pointer(fString) := nil;
          fString := Strings[Int];
          fObject := Strings.Objects[Int];
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
        end;
        Inc(fCount);
      end;
      if Assigned(OnInserted) then
        OnInserted(Self, FirstAdded, Strings.Count);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.SetEncoding(const Value: TEncoding);
begin
  inherited;
end;

procedure TSynEditStringList.Clear;
begin
  if fCount <> 0 then begin
    BeginUpdate;
    Finalize(fList^[0], fCount);
    fCount := 0;
    SetCapacity(0);
    if Assigned(fOnCleared) then
      fOnCleared(Self);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;
end;

procedure TSynEditStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  Finalize(fList^[Index]);
  Dec(fCount);
  if Index < fCount then begin
    System.Move(fList^[Index + 1], fList^[Index],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;
  if Assigned(fOnDeleted) then
    fOnDeleted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: Integer;
begin
  if NumLines > 0 then begin
    if (Index < 0) or (Index >= fCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := fCount - (Index + NumLines);
    if LinesAfter < 0 then
      NumLines := fCount - Index;
    Finalize(fList^[Index], NumLines);

    if LinesAfter > 0 then begin
      BeginUpdate;
      try
        System.Move(fList^[Index + NumLines], fList^[Index],
          LinesAfter * SynEditStringRecSize);
      finally
        EndUpdate;
      end;
    end;
    Dec(fCount, NumLines);
    if Assigned(fOnDeleted) then
      fOnDeleted( Self, Index, NumLines );
  end;
end;

procedure TSynEditStringList.Exchange(Index1, Index2: Integer);
var
  Temp: TSynEditStringRec;
begin
  if (Index1 < 0) or (Index1 >= fCount) then
    ListIndexOutOfBounds(Index1);
  if (Index2 < 0) or (Index2 >= fCount) then
    ListIndexOutOfBounds(Index2);
  BeginUpdate;
  Temp := fList^[Index1];
  fList^[Index1] := fList^[Index2];
  fList^[Index2] := Temp;
  if fIndexOfLongestLine = Index1 then
    fIndexOfLongestLine := Index2
  else if fIndexOfLongestLine = Index2 then
    fIndexOfLongestLine := Index1;
  EndUpdate;
end;

function TSynEditStringList.ExpandString(Index: Integer): string;
var
  HasTabs: Boolean;
begin
  with fList^[Index] do
    if Length(FString) = 0 then
    begin
      Result := '';
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Include(fFlags, sfHasNoTabs);
      fExpandedLength := 0;
    end
    else
    begin
      Result := fConvertTabsProc(fstring, fTabWidth, HasTabs);
      fExpandedLength := Length(fExpandAtWideGlyphsFunc(Result));
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      if HasTabs then
        Include(fFlags, sfHasTabs)
      else
        Include(fFlags, sfHasNoTabs);
    end;
end;

function TSynEditStringList.Get(Index: Integer): string;
begin
  if Cardinal(Index)<Cardinal(fCount) then
    Result := fList^[Index].fString
  else
    Result := '';
end;

procedure TSynEditStringList.UpdateCharIndexes;
var
  Int, n : Integer;
  Posi : PSynEditStringRec;
begin
  fCharIndexesAreValid:=True;
  if fCount=0 then Exit;
  Posi:=@fList^[0];
  n:=0;
  for Int:=1 to fCount do begin
    Posi.fCharIndex:=n;
    Inc(n, Length(Posi.FString));
    Inc(Posi);
  end;
end;

function TSynEditStringList.LineCharLength(Index : Integer) : Integer;
begin
  if Cardinal(Index)<Cardinal(fCount) then
    Result:=Length(fList^[Index].fString)
  else Result:=0;
end;

function TSynEditStringList.LineCharIndex(Index : Integer) : Integer;
begin
  if Cardinal(Index)<Cardinal(fCount) then begin
    if not fCharIndexesAreValid then
      UpdateCharIndexes;
    Result:=fList^[Index].fCharIndex;
  end else Result:=0;
end;

function TSynEditStringList.GetCapacity: Integer;
begin
  Result := fCapacity;
end;

function TSynEditStringList.GetCount: Integer;
begin
  Result := fCount;
end;

function TSynEditStringList.GetExpandedString(Index: Integer): string;
begin
  if (Index >= 0) and (Index < fCount) then
  begin
    if sfHasNoTabs in fList^[Index].fFlags then
      Result := Get(Index)
    else
      Result := ExpandString(Index);
  end else
    Result := '';
end;

function TSynEditStringList.GetExpandedStringLength(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < fCount) then
  begin
    if sfExpandedLengthUnknown in fList^[Index].fFlags then
      Result := Length( ExpandedStrings[index] )
    else
      Result := fList^[Index].fExpandedLength;
  end
  else
    Result := 0;
end;

function TSynEditStringList.GetLengthOfLongestLine: Integer;
var
  Int, MaxLen: Integer;
  PRec: PSynEditStringRec;
begin
  if fIndexOfLongestLine < 0 then
  begin
    MaxLen := 0;
    if fCount > 0 then
    begin
      PRec := @fList^[0];
      for Int := 0 to fCount - 1 do
      begin
        if sfExpandedLengthUnknown in PRec^.fFlags then
          ExpandString(Int);
        if PRec^.fExpandedLength > MaxLen then
        begin
          MaxLen := PRec^.fExpandedLength;
          fIndexOfLongestLine := Int;
        end;
        Inc(PRec);
      end;
    end;
  end;
  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < fCount) then
    Result := fList^[fIndexOfLongestLine].fExpandedLength
  else
    Result := 0;
end;

function TSynEditStringList.GetObject(Index: Integer): TObject;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fObject
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: Integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fRange
  else
    Result := nil;
end;

function TSynEditStringList.GetSeparatedText(Separators: string): string;
{Optimized by Eric Grange}
var
  Int, L, Size, LineBreakSize: Integer;
  Posi, PLineBreak: PChar;
  PRec: PSynEditStringRec;
begin
  if fCount = 0 then begin
     Result := '';
     Exit;
  end;
  LineBreakSize := Length(Separators);
  PLineBreak := Pointer(Separators);

  // compute buffer size
  Size :=   (fCount-1) * LineBreakSize
          + LineCharIndex( fCount-1 )
          + Length( fList^[fCount-1].FString );
  SetLength(Result, Size);

  Posi := Pointer(Result);
  PRec := @fList^[0];

  // handle 1st line separately (to avoid trailing line break)
  L := Length(PRec.FString);
  if L <> 0 then
  begin
    System.Move(Pointer(PRec.FString)^, Posi^, L * SizeOf(Char));
    Inc(Posi, L);
  end;
  Inc(PRec);

  for Int := 1 to fCount-1 do
  begin
    case LineBreakSize of
      0 : ;
      1 : begin
        Posi^ := PLineBreak^;
        Inc(Posi);
      end;
      2 : begin
        PSynEditTwoWideChars(Posi)^ := PSynEditTwoWideChars(PLineBreak)^;
        Inc(Posi, 2);
      end;
    else
      System.Move(PLineBreak^, Posi^, LineBreakSize * SizeOf(Char));
      Inc(Posi, LineBreakSize);
    end;
    if Pointer( PRec.FString ) <> nil then
    begin
      L := Length(PRec.FString);
      System.Move(Pointer(PRec.FString)^, Posi^, L * SizeOf(Char));
      Inc(Posi, L);
    end;
    Inc(PRec);
  end;
end;

procedure TSynEditStringList.Grow;
var
  Delta: Integer;
begin
  if fCapacity > 64 then
    Delta := fCapacity div 4
  else
    Delta := 16;
  SetCapacity(fCapacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: Integer; const Str: string);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, Str);
  if Assigned(fOnInserted) then
    fOnInserted( Self, Index, 1 );
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: Integer; const Str: string);
begin
  BeginUpdate;
  if fCount = fCapacity then
    Grow;
  if Index < fCount then
  begin
    System.Move(fList^[Index], fList^[Index + 1],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;                                                    
  with fList^[Index] do
  begin
    Pointer(fString) := nil;
    fString := Str;
    fObject := nil;
    fRange := NullRange;
    fExpandedLength := -1;
    fFlags := [sfExpandedLengthUnknown];
  end;
  Inc(fCount);
  EndUpdate;
end;

procedure TSynEditStringList.InsertLines(Index, NumLines: Integer);
var
  c_Line: Integer;
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  if NumLines > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(fCount + NumLines);
      if Index < fCount then
      begin
        System.Move(fList^[Index], fList^[Index + NumLines],
          (fCount - Index) * SynEditStringRecSize);
      end;
      for c_Line := Index to Index + NumLines -1 do
        with fList^[c_Line] do
        begin
          Pointer(fString) := nil;
          fObject := nil;
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
        end;
      Inc(fCount, NumLines);
      if Assigned(OnInserted) then
        OnInserted(Self, Index, NumLines);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: Integer;
  NewStrings: TStrings);
var
  Int, Cnt: Integer;
begin
  Cnt := NewStrings.Count;
  if Cnt = 0 then Exit;

  BeginUpdate;
  try
    InsertLines(Index, Cnt);
    for Int := 0 to Cnt - 1 do
      Strings[Index + Int] := NewStrings[Int];
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.InsertText(Index: Integer;
  NewText: string);
var
  TmpStringList: TStringList;
begin
  if NewText = '' then Exit;

  TmpStringList := TStringList.Create;
  try
    TmpStringList.Text := NewText;
    InsertStrings(Index, TmpStringList);
  finally
    TmpStringList.Free;
  end;
end;

procedure TSynEditStringList.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
  DecodedText: string;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer, 0, Size);
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, DefaultEncoding);
    WriteBOM := Size > 0; // Keep WriteBom in case the stream is saved
    // If the encoding is ANSI and DetectUtf8 is True try to Detect UTF8
    if (Encoding = TEncoding.ANSI) and DetectUTF8 and IsUTF8(Buffer, Size) then
      Encoding := TEncoding.UTF8;
    SetEncoding(Encoding); // Keep Encoding in case the stream is saved
    DecodedText := Encoding.GetString(Buffer, Size, Length(Buffer) - Size);
    SetLength(Buffer, 0); // Free the buffer here to reduce memory footprint
    SetTextAndFileFormat(DecodedText);
  finally
    EndUpdate;
  end;
end;

procedure TSynEditStringList.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Cancel: Boolean;
  OldLineBreak: string;
  Str: string;
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := DefaultEncoding;

  OldLineBreak := LineBreak;
  try
    LineBreak := LineBreakFromFileFormat(fFileFormat);
    Str := GetTextStr;
  finally
    LineBreak := OldLineBreak;
  end;

  Cancel := False;
  if (Encoding = TEncoding.ANSI) and Assigned(fOnInfoLoss) and not IsAnsiOnly(Str) then
  begin
    fOnInfoLoss(Encoding, Cancel);
    if Cancel then
      Exit;
    if Encoding <> TEncoding.ANSI then
      SetEncoding(Encoding);
  end;

  Buffer := Encoding.GetBytes(Str);
  if WriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

procedure TSynEditStringList.Put(Index: Integer; const Str: string);
begin
  if (Index = 0) and (fCount = 0) or (fCount = Index) then
    Add(Str)
  else begin
    if Cardinal(Index)>=Cardinal(fCount) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    fIndexOfLongestLine := -1;
    with fList^[Index] do begin
      Include(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      fString := Str;
    end;
    if Assigned(fOnPutted) then
      fOnPutted( Self, Index, 1 );
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index)>=Cardinal(fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  fList^[Index].fObject := AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: Integer; ARange: TSynEditRange);
begin
  if Cardinal(Index)>=Cardinal(fCount) then
    ListIndexOutOfBounds(Index);
  fList^[Index].fRange := ARange;
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < Count then
    EListError.Create( SInvalidCapacity );
  ReallocMem(fList, NewCapacity * SynEditStringRecSize);
  fCapacity := NewCapacity;
end;

procedure TSynEditStringList.SetTabWidth(Value: Integer);
var
  Int: Integer;
begin
  if Value <> fTabWidth then begin
    fTabWidth := Value;
    fConvertTabsProc := GetBestConvertTabsProcEx(fTabWidth);
    fIndexOfLongestLine := -1;
    for Int := 0 to fCount - 1 do
      with fList^[Int] do begin
        fExpandedLength := -1;
        Exclude(fFlags, sfHasNoTabs);
        Include(fFlags, sfExpandedLengthUnknown);
      end;
  end;
end;

procedure TSynEditStringList.SetTextAndFileFormat(const Value: string);
var
  Str: string;
  Size: Integer;
  Posi, Start, Pmax: PWideChar;
  fCR, fLF, fLINESEPARATOR: Boolean;
begin
  fLINESEPARATOR := False;
  fCR := False;
  fLF := False;
  BeginUpdate;
  try
    Clear;
    Posi := Pointer(Value);
    if Posi <> nil then
    begin
      Size := Length(Value);
      Pmax := @Value[Size];
      while (Posi <= Pmax) do
      begin
        Start := Posi;
        while not (Ord(Posi^) in [0, $A, $D]) and (Posi^ <> WideLineSeparator) do
        begin
          Inc(Posi);
        end;
        if Posi<>Start then
        begin
          SetString(Str, Start, Posi - Start);
          InsertItem(fCount, Str);
        end
        else
          InsertItem(fCount, '');
        if Posi^ = #0 then
          Inc(Posi);
        if Posi^ = WideLineSeparator then
        begin
          fLINESEPARATOR := True;
          Inc(Posi);
        end;
        if Posi^ = WideCR then
        begin
          fCR := True;
          Inc(Posi);
        end;
        if Posi^ = WideLF then
        begin
          fLF := True;
          Inc(Posi);
        end;
      end;
      // keep the old format of the file
      if not TrailingLineBreak and
        (CharInSet(Value[Size], [#10, #13]) or (Value[Size] = WideLineSeparator))
      then
        InsertItem(fCount, '');
    end;
    if Assigned(OnInserted) and (fCount > 0) then
      OnInserted(Self, 0, fCount);
  finally
    EndUpdate;
  end;
  if fLINESEPARATOR then
    FileFormat := sffUnicode
  else if fCR and not fLF then
    FileFormat := sffMac
  else if fLF and not fCR then
    FileFormat := sffUnix
  else
    FileFormat := sffDos;
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  fCharIndexesAreValid:=False;
  if Updating then begin
    if Assigned(fOnChanging) then
      fOnChanging(Self);
  end else begin
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

procedure TSynEditStringList.FontChanged;
var
  Int: Integer;
begin
  fIndexOfLongestLine := -1;
  for Int := 0 to fCount - 1 do
    with fList^[Int] do
    begin
      fExpandedLength := -1;
      Exclude(fFlags, sfHasNoTabs);
      Include(fFlags, sfExpandedLengthUnknown);
    end;
end;

{ TSynEditUndoItem }

procedure TSynEditUndoItem.Assign(Source: TPersistent);
begin
  if (Source is TSynEditUndoItem) then
  begin
    fChangeReason:=TSynEditUndoItem(Source).fChangeReason;
    fChangeSelMode:=TSynEditUndoItem(Source).fChangeSelMode;
    fChangeStartPos:=TSynEditUndoItem(Source).fChangeStartPos;
    fChangeEndPos:=TSynEditUndoItem(Source).fChangeEndPos;
    fChangeStr:=TSynEditUndoItem(Source).fChangeStr;
    fChangeNumber:=TSynEditUndoItem(Source).fChangeNumber;
  end
  else
    inherited Assign(Source);
end;


{ TSynEditUndoList }

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  fItems := TList.Create;
  fMaxUndoActions := 1024;
  fNextChangeNumber := 1;
  fInsideRedo := False;
end;

destructor TSynEditUndoList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

procedure TSynEditUndoList.Assign(Source: TPersistent);
var
  Int: Integer;
  UndoItem: TSynEditUndoItem;
begin
  if (Source is TSynEditUndoList) then
  begin
    Clear;
    for Int:=0 to TSynEditUndoList(Source).fItems.Count-1 do
    begin
      UndoItem:=TSynEditUndoItem.Create;
      UndoItem.Assign(TSynEditUndoList(Source).fItems[Int]);
      fItems.Add(UndoItem);
    end;
    fBlockChangeNumber:=TSynEditUndoList(Source).fBlockChangeNumber;
    fBlockCount:=TSynEditUndoList(Source).fBlockCount;
    fFullUndoImposible:=TSynEditUndoList(Source).fFullUndoImposible;
    fLockCount:=TSynEditUndoList(Source).fLockCount;
    fMaxUndoActions:=TSynEditUndoList(Source).fMaxUndoActions;
    fNextChangeNumber:=TSynEditUndoList(Source).fNextChangeNumber;
    fInsideRedo:=TSynEditUndoList(Source).fInsideRedo;
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; const AStart,
  AEnd: TBufferCoord; const ChangeText: string; SelMode: TSynSelectionMode);
var
  NewItem: TSynEditUndoItem;
begin
  if fLockCount = 0 then begin
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do begin
        fChangeReason := AReason;
        fChangeSelMode := SelMode;
        fChangeStartPos := AStart;
        fChangeEndPos := AEnd;
        fChangeStr := ChangeText;
        if fBlockChangeNumber <> 0 then
          fChangeNumber := fBlockChangeNumber
        else begin
          fChangeNumber := fNextChangeNumber;
          if fBlockCount = 0 then begin
            Inc(fNextChangeNumber);
            if fNextChangeNumber = 0 then
              Inc(fNextChangeNumber);
          end;
        end;
      end;
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TSynEditUndoList.BeginBlock;
begin
  Inc(fBlockCount);
  fBlockChangeNumber := fNextChangeNumber;
end;

procedure TSynEditUndoList.Clear;
var
  Int: Integer;
begin
  for Int := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[Int]).Free;
  fItems.Clear;
  fFullUndoImposible := False;
end;

procedure TSynEditUndoList.EndBlock;
var
  iBlockID: Integer;
begin
  if fBlockCount > 0 then begin
    Dec(fBlockCount);
    if fBlockCount = 0 then begin
      iBlockID := fBlockChangeNumber;
      fBlockChangeNumber := 0;
      Inc(fNextChangeNumber);
      if fNextChangeNumber = 0 then
        Inc(fNextChangeNumber);
      if (fItems.Count > 0) and (PeekItem.ChangeNumber = iBlockID) and
        Assigned(OnAddedUndo) then
      begin
        OnAddedUndo( Self );
      end;
    end;
  end;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoItem;
begin
  if fItems.Count > fMaxUndoActions then 
  begin
    fFullUndoImposible := True;                                                 
    while fItems.Count > fMaxUndoActions do begin
      Item := fItems[0];
      Item.Free;
      fItems.Delete(0);
    end;
  end;
end;

function TSynEditUndoList.GetCanUndo: Boolean;
begin
  Result := fItems.Count > 0;
end;

function TSynEditUndoList.GetItemCount: Integer;
begin
  Result := fItems.Count;
end;

procedure TSynEditUndoList.Lock;
begin
  Inc(fLockCount);
end;

function TSynEditUndoList.PeekItem: TSynEditUndoItem;
var
  iLast: Integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then
    Result := fItems[iLast];
end;

function TSynEditUndoList.PopItem: TSynEditUndoItem;
var
  iLast: Integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then begin
    Result := fItems[iLast];
    fItems.Delete(iLast);
  end;
end;

procedure TSynEditUndoList.PushItem(Item: TSynEditUndoItem);
begin
  if Assigned(Item) then begin
    fItems.Add(Item);
    EnsureMaxEntries;
    if (Item.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TSynEditUndoList.SetMaxUndoActions(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fMaxUndoActions then begin
    fMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

procedure TSynEditUndoList.Unlock;
begin
  if fLockCount > 0 then
    Dec(fLockCount);
end;

function TSynEditUndoList.LastChangeReason: TSynChangeReason;
begin
  if fItems.Count = 0 then
    Result := crNothing
  else
    Result := TSynEditUndoItem(fItems[fItems.Count - 1]).fChangeReason;
end;

procedure TSynEditUndoList.AddGroupBreak;
var
  vDummy: TBufferCoord;
begin
  //Add the GroupBreak even if ItemCount = 0. Since items are stored in
  //reverse order in TCustomSynEdit.fRedoList, a GroupBreak could be lost.
  if LastChangeReason <> crGroupBreak then
  begin
    AddChange(crGroupBreak, vDummy, vDummy, '', smNormal);
  end;
end;

procedure TSynEditUndoList.SetInitialState(const Value: Boolean);
begin
  if Value then
  begin
    if ItemCount = 0 then
      fInitialChangeNumber := 0
    else
      fInitialChangeNumber := PeekItem.ChangeNumber;
  end
  else
    if ItemCount = 0 then
    begin
      if fInitialChangeNumber = 0 then
        fInitialChangeNumber := -1;
    end
    else if PeekItem.ChangeNumber = fInitialChangeNumber then
      fInitialChangeNumber := -1;
end;

function TSynEditUndoList.GetInitialState: Boolean;
begin
  if ItemCount = 0 then
    Result := fInitialChangeNumber = 0
  else
    Result := PeekItem.ChangeNumber = fInitialChangeNumber;
end;

function TSynEditUndoList.GetItems(Index: Integer): TSynEditUndoItem;
begin
  Result := TSynEditUndoItem(fItems[Index]);
end;

procedure TSynEditUndoList.SetItems(Index: Integer;
  const Value: TSynEditUndoItem);
begin
  fItems[Index] := Value;
end;

procedure TSynEditUndoList.DeleteItem(AIndex: Integer);
begin
  TSynEditUndoItem(fItems[AIndex]).Free;
  fItems.Delete(AIndex);
end;

end.
