(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower ShellShock
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: StStrms.pas 1.02                          *}
{*********************************************************}
{* ShellShock: Specialized Stream Classes for ShellShock *}
{*********************************************************}

unit SsStrms;

interface

{$I SsDefine.inc}

uses
  Windows, SysUtils,
  {$IFDEF UNICODE}
  Generics.Collections,
  {$ENDIF}
  Classes, AnsiStrings;

type
  TStMemSize = Integer;

//  {$IFNDEF VERSION3}
//  TStSetStreamSize = procedure(aStream  : TStream;
//                               aNewSize : longint) of object;
//  {$ENDIF}

  TStBufferedStream = class(TStream)
    private
      FBufCount: TStMemSize;   {count of valid bytes in buffer}
      FBuffer  : {$IFDEF UNICODE}PByte{$ELSE}PAnsiChar{$ENDIF};    {buffer into underlying stream}
      FBufOfs  : longint;      {offset of buffer in underlying stream}
      FBufPos  : TStMemSize;   {current position in buffer}
      FBufSize : TStMemSize;   {size of buffer}
      FDirty   : Boolean;      {has data in buffer been changed?}
      FSize    : longint;      {size of underlying stream}
      FStream  : TStream;      {underlying stream}
//      {$IFNDEF VERSION3}
//      FOnSetStreamSize : TStSetStreamSize;
//                               {event to set underlying stream's size}
//      {$ENDIF}
    protected
      procedure bsSetStream(aValue : TStream);

      procedure bsInitForNewStream; virtual;
      function bsReadAnsiChar(var aCh : AnsiChar) : Boolean;
      function bsReadByte(var AByte: Byte): Boolean;
      function bsReadWideChar(var ACh: Char): Boolean;
      procedure bsReadFromStream;
      procedure bsWriteToStream;

//      {$IFDEF VERSION3}
      procedure SetSize(const NewSize : Int64); override;
      procedure SetSize(NewSize: Integer); override;

//      {$ENDIF}
    public
      constructor Create(aStream : TStream);
      constructor CreateEmpty;
      destructor Destroy; override;

      function Read(var Buffer; Count : longint) : longint; override;
      function Seek(Offset : longint; Origin : word) : longint; override;
      function Write(const Buffer; Count : longint) : longint; override;
//      {$IFNDEF VERSION3}
//      procedure SetSize(NewSize : longint);
//      {$ENDIF}

      property FastSize : longint read FSize;
      property Stream : TStream read FStream write bsSetStream;

//      {$IFNDEF VERSION3}
//      property OnSetStreamSize : TStSetStreamSize
//                 read FOnSetStreamSize write FOnSetStreamSize;
//      {$ENDIF}
   end;

type
  TStLineTerminator = ( {possible line terminators...}
     ltNone,            {..no terminator, ie fixed length lines}
     ltCR,              {..carriage return (#13)}
     ltLF,              {..line feed (#10)}
     ltCRLF,            {..carriage return/line feed (#13/#10)}
     ltOther);          {..another character}

  TStAnsiTextStream = class(TStBufferedStream)
    private
      FLineEndCh   : AnsiChar;
      FLineLen     : Integer;
      FLineTerm    : TStLineTerminator;
      FFixedLine   : PAnsiChar;
      FLineCount   : longint;
      FLineCurrent : longint;
      FLineCurOfs  : longint;
      FLineIndex   : TList;
      FLineInxStep : longint;
      FLineInxTop  : Integer;
    protected
      function atsGetLineCount : longint;

      procedure atsSetLineTerm(aValue : TStLineTerminator);
      procedure atsSetLineEndCh(aValue : AnsiChar);
      procedure atsSetLineLen(aValue : Integer);

      procedure atsGetLine(var aStartPos : longint;
                           var aEndPos   : longint;
                           var aLen      : longint);
      procedure atsResetLineIndex;

      procedure bsInitForNewStream; override;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      function AtEndOfStream : Boolean;

      function ReadLine : AnsiString;
      function ReadLineArray(aCharArray : PAnsiChar; aLen : TStMemSize)
                                                          : TStMemSize;
      function ReadLineZ(aSt : PAnsiChar; aMaxLen : TStMemSize) : PAnsiChar;

      function SeekNearestLine(aOffset : longint) : longint;
      function SeekLine(aLineNum : longint) : longint;

      procedure WriteLine(const aSt : AnsiString);
      procedure WriteLineArray(aCharArray : PAnsiChar; aLen : TStMemSize);
      procedure WriteLineZ(aSt : PAnsiChar);

      property FixedLineLength : Integer
                  read FLineLen write atsSetLineLen;
      property LineCount : longint
                  read atsGetLineCount;
      property LineTermChar : AnsiChar
                  read FLineEndCh write atsSetLineEndCh;
      property LineTerminator : TStLineTerminator
                  read FLineTerm write atsSetLineTerm;
  end;

  {$IFDEF UNICODE}
  TStTextStream = class(TStBufferedStream)
  private
    FLineEndCh   : Char;
    FLineLen     : Integer;
    FLineTerm    : TStLineTerminator;
    FFixedLine   : PChar;
    FLineCount   : longint;
    FLineCurrent : longint;
    FLineCurOfs  : longint;
    FLineIndex   : TList<Int64>;
    FLineInxStep : longint;
    FLineInxTop  : Integer;
    FEncoding: TEncoding;
  protected
    function atsGetLineCount: longint;

    procedure atsSetLineTerm(aValue : TStLineTerminator);
    procedure atsSetLineEndCh(aValue : Char);
    procedure atsSetLineLen(aValue : Integer);

    procedure atsGetLine(out aStartPos : longint;
                         out aEndPos   : longint;
                         out aLen      : longint);
    procedure atsResetLineIndex;

    procedure bsInitForNewStream; override;
    function bsReadChar(var aCh : Char) : Boolean;
  public
    constructor Create(aStream : TStream);
    destructor Destroy; override;

    function AtEndOfStream : Boolean;

    function ReadLine : string;
    function ReadLineArray(aCharArray : PChar; aLen : TStMemSize): TStMemSize;
    function ReadLineZ(aSt : PChar; aMaxLen : TStMemSize) : PChar;

    function SeekNearestLine(aOffset : longint) : longint;
    function SeekLine(aLineNum : longint) : longint;

    procedure WriteLine(const aSt : string);
    procedure WriteLineArray(aCharArray : PChar; aLen : TStMemSize);
    procedure WriteLineZ(aSt : PChar);

    property FixedLineLength : Integer read FLineLen write atsSetLineLen;
    property LineCount : longint read atsGetLineCount;
    property LineTermChar : Char read FLineEndCh write atsSetLineEndCh;
    property LineTerminator : TStLineTerminator read FLineTerm write atsSetLineTerm;
  end;
  {$ELSE}
  TStTextStream = class(TstAnsiTextStream)
  end;
  {$ENDIF}

  TStMemoryMappedFile = class(TStream)
  protected {private}
    FBuffer     : Pointer;
    FHeaderSize : Word;
    FDataSize   : Cardinal;
    FHandle     : THandle;
    FMapObj     : THandle;
    FMaxHi      : Cardinal;
    FMaxLo      : Cardinal;
    FMutex      : THandle;
    FPos        : Cardinal;
    FReadOnly   : Boolean;
    FSharedData : Boolean;

  protected
    function GetDataSize : Cardinal;

  public
    constructor Create(FileName   : string;
                       MaxSize    : Cardinal;
                       ReadOnly   : Boolean;
                       SharedData : Boolean);
    destructor Destroy; override;

    function Read(var Buffer; Count : Longint) : Longint; override;
    function Seek(Offset : Longint; Origin : Word) : Longint; override;
    function Write(const Buffer; Count : Longint) : Longint; override;

    property DataSize : Cardinal
      read GetDataSize;

    property MaxSize : Cardinal
      read FMaxLo;

    property Position : Cardinal
      read FPos;

    property ReadOnly : Boolean
      read FReadOnly;

    property SharedData : Boolean
      read FSharedData;
  end;


implementation

uses SsBase, SsConst;

const
  LineTerm : array [TStLineTerminator] of
               array [0..1] of AnsiChar =
                 ('', #13, #10, #13#10, '');

const
  LineIndexCount = 1024;
  LineIndexMax   = pred(LineIndexCount);


{--- Helper routines ---------------------------------------------------------}

function MinLong(A, B : longint) : longint;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;


{-----------------------------------------------------------------------------}
{                          TStBufferedStream                                  }
{-----------------------------------------------------------------------------}

constructor TStBufferedStream.Create(aStream : TStream);
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := 4096;
  GetMem(FBuffer, FBufSize);

  {save the stream}
  if not Assigned(aStream) then
    RaiseStError(ESsBufStreamError, ssscNilStream);
  FStream := aStream;

  bsInitForNewStream;
end;

{-----------------------------------------------------------------------------}

constructor TStBufferedStream.CreateEmpty;
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := 4096;
  GetMem(FBuffer, FBufSize);

  bsInitForNewStream;
end;

{-----------------------------------------------------------------------------}

destructor TStBufferedStream.Destroy;
begin
  if Assigned(FBuffer) then begin
    if FDirty and Assigned(FStream) then
      bsWriteToStream;
    FreeMem(FBuffer, FBufSize);
  end;

  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsInitForNewStream;
begin
  if Assigned(FStream) then
    FSize := FStream.Size
  else
    FSize := 0;
  FBufCount := 0;
  FBufOfs := 0;
  FBufPos := 0;
  FDirty := False;
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.bsReadAnsiChar(var aCh : AnsiChar) : Boolean;
begin
  {is there anything to read?}
  if (FSize = (FBufOfs + FBufPos)) then begin
    Result := False;
    Exit;
  end;
  {if we get here, we'll definitely read a character}
  Result := True;
  {make sure that the buffer has some data in it}
  if (FBufCount = 0) then
    bsReadFromStream
  else if (FBufPos = FBufCount) then begin
    if FDirty then
      bsWriteToStream;
    FBufPos := 0;
    Inc(FBufOfs, FBufSize);
    bsReadFromStream;
  end;
  {get the next character}
  aCh := AnsiChar(FBuffer[FBufPos]);
  Inc(FBufPos);
end;

function TStBufferedStream.bsReadByte(var AByte: Byte): Boolean;
begin
  {is there anything to read?}
  if (FSize = (FBufOfs + FBufPos)) then begin
    Result := False;
    Exit;
  end;
  {if we get here, we'll definitely read a character}
  Result := True;
  {make sure that the buffer has some data in it}
  if (FBufCount = 0) then
    bsReadFromStream
  else if (FBufPos = FBufCount) then begin
    if FDirty then
      bsWriteToStream;
    FBufPos := 0;
    Inc(FBufOfs, FBufSize);
    bsReadFromStream;
  end;
  {get the next character}
  aByte := Byte(FBuffer[FBufPos]);
  Inc(FBufPos);
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsReadFromStream;
var
  NewPos : longint;
begin
  {assumptions: FBufOfs is where to read the buffer
                FBufSize is the number of bytes to read
                FBufCount will be the number of bytes read}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
    RaiseStError(ESsBufStreamError, ssscNoSeekForRead);
  FBufCount := FStream.Read(FBuffer^, FBufSize);
end;

function TStBufferedStream.bsReadWideChar(var ACh: Char): Boolean;
begin
  Result := Read(aCh, 2) = 2;
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsSetStream(aValue : TStream);
begin
  if (aValue <> FStream) then begin
    {if the buffer is dirty, flush it to the current stream}
    if FDirty and Assigned(FStream) then
      bsWriteToStream;
    {remember the stream and initialize all fields}
    FStream := aValue;
    bsInitForNewStream;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsWriteToStream;
var
  NewPos       : longint;
  BytesWritten : longint;
begin
  {assumptions: FDirty is true
                FBufOfs is where to write the buffer
                FBufCount is the number of bytes to write
                FDirty will be set false afterwards}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
    RaiseStError(ESsBufStreamError, ssscNoSeekForWrite);
  BytesWritten := FStream.Write(FBuffer^, FBufCount);
  if (BytesWritten <> FBufCount) then
    RaiseStError(ESsBufStreamError, ssscCannotWrite);
  FDirty := False;
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.Read(var Buffer; Count : longint) : longint;
var
  BytesToGo   : longint;
  BytesToRead : longint;
  BufAsBytes  : TByteArray absolute Buffer;
  DestPos     : longint;
begin
  if not Assigned(FStream) then
    RaiseStError(ESsBufStreamError, ssscNilStream);
  {calculate the number of bytes we could read if possible}
  BytesToGo := MinLong(Count, FSize - (FBufOfs + FBufPos));
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to read some data after all?}
  if (BytesToGo > 0) then begin
    {make sure that the buffer has some data in it}
    if (FBufCount = 0) then
      bsReadFromStream;
    {read as much as we can from the current buffer}
    BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
    {transfer that number of bytes}
    Move(FBuffer[FBufPos], BufAsBytes[0], BytesToRead);
    {update our counters}
    Inc(FBufPos, BytesToRead);
    Dec(BytesToGo, BytesToRead);
    {if we have more bytes to read then we've reached the end of the
     buffer and so we need to read another, and another, etc}
    DestPos := 0;
    while BytesToGo > 0 do begin
      {if the current buffer is dirty, write it out}
      if FDirty then
        bsWriteToStream;
      {position and read the next buffer}
      FBufPos := 0;
      Inc(FBufOfs, FBufSize);
      bsReadFromStream;
      {calculate the new destination position, and the number of bytes
       to read from this buffer}
      Inc(DestPos, BytesToRead);
      BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
      {transfer that number of bytes}
      Move(FBuffer[FBufPos], BufAsBytes[DestPos], BytesToRead);
      {update our counters}
      Inc(FBufPos, BytesToRead);
      Dec(BytesToGo, BytesToRead);
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.Seek(Offset : longint; Origin : word) : longint;
var
  NewPos : longint;
  NewOfs : longint;
begin
  if not Assigned(FStream) then
    RaiseStError(ESsBufStreamError, ssscNilStream);
  {optimization: to help code that just wants the current stream
   position (ie, reading the Position property), check for this as a
   special case}
  if (Offset = 0) and (Origin = soFromCurrent) then begin
    Result := FBufOfs + FBufPos;
    Exit;
  end;
  {calculate the desired position}
  case Origin of
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := (FBufOfs + FBufPos) + Offset;
    soFromEnd       : NewPos := FSize + Offset;
  else
    RaiseStError(ESsBufStreamError, ssscBadOrigin);
    NewPos := 0; {to fool the compiler's warning--we never get here}
  end;
  {force the new position to be valid}
  if (NewPos < 0) then
    NewPos := 0
  else if (NewPos > FSize) then
    NewPos := FSize;
  {calculate the offset for the buffer}
  NewOfs := (NewPos div FBufSize) * FBufSize;
  {if the offset differs, we have to move the buffer window}
  if (NewOfs <> FBufOfs) then begin
    {check to see whether we have to write the current buffer to the
     original stream first}
    if FDirty then
      bsWriteToStream;
    {mark the buffer as empty}
    FBufOfs := NewOfs;
    FBufCount := 0;
  end;
  {set the position within the buffer}
  FBufPos := NewPos - FBufOfs;
  Result := NewPos;
end;

procedure TStBufferedStream.SetSize(NewSize: Integer);
begin
  SetSize(Int64(NewSize)); // call the 64 bit sibling
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.SetSize(const NewSize : Int64);
var
  NewPos : Int64;
begin
  {get rid of the simple case first where the new size and the old
   size are the same}
  if (NewSize = FSize) then
    Exit;
  {if the buffer is dirty, write it out}
  if FDirty then
    bsWriteToStream;
  {now set the size of the underlying stream}
  {$IFDEF VERSION3}
  FStream.Size := NewSize;
  {$ELSE}
  if not Assigned(FOnSetStreamSize) then
    RaiseStError(ESsBufStreamError, ssscCannotSetSize);
  FOnSetStreamSize(FStream, NewSize);
  {$ENDIF}
  {patch up the buffer fields so that the buffered stream points to
   somewhere in the newly resized stream}
  NewPos := FBufOfs + FBufPos;
  if (NewPos > NewSize) then
    NewPos := NewSize;
  bsInitForNewStream;
  Seek(NewPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.Write(const Buffer; Count : longint) : longint;
var
  BytesToGo   : longint;
  BytesToWrite: longint;
  BufAsBytes  : TByteArray absolute Buffer;
  DestPos     : longint;
begin
  if not Assigned(FStream) then
    RaiseStError(ESsBufStreamError, ssscNilStream);
  {calculate the number of bytes we should be able to write}
  BytesToGo := Count;
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to write some data?}
  if (BytesToGo > 0) then begin
    {try and make sure that the buffer has some data in it}
    if (FBufCount = 0) then
      bsReadFromStream;
    {write as much as we can to the current buffer}
    BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
    {transfer that number of bytes}
    Move(BufAsBytes[0], FBuffer[FBufPos], BytesToWrite);
    FDirty := True;
    {update our counters}
    Inc(FBufPos, BytesToWrite);
    if (FBufCount < FBufPos) then begin
      FBufCount := FBufPos;
      FSize := FBufOfs + FBufPos;
    end;
    Dec(BytesToGo, BytesToWrite);
    {if we have more bytes to write then we've reached the end of the
     buffer and so we need to write another, and another, etc}
    DestPos := 0;
    while BytesToGo > 0 do begin
      {as the current buffer is dirty, write it out}
      bsWriteToStream;
      {position and read the next buffer, if required}
      FBufPos := 0;
      Inc(FBufOfs, FBufSize);
      if (FBufOfs < FSize) then
        bsReadFromStream
      else
        FBufCount := 0;
      {calculate the new destination position, and the number of bytes
       to write to this buffer}
      Inc(DestPos, BytesToWrite);
      BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
      {transfer that number of bytes}
      Move(BufAsBytes[DestPos], FBuffer[0], BytesToWrite);
      FDirty := True;
      {update our counters}
      Inc(FBufPos, BytesToWrite);
      if (FBufCount < FBufPos) then begin
        FBufCount := FBufPos;
        FSize := FBufOfs + FBufPos;
      end;
      Dec(BytesToGo, BytesToWrite);
    end;
  end;
end;

{-----------------------------------------------------------------------------}
{                           TStAnsiTextStream                                 }
{-----------------------------------------------------------------------------}

constructor TStAnsiTextStream.Create(aStream : TStream);
begin
  inherited Create(aStream);

  {set up the line index variables}
  atsResetLineIndex;
end;

{-----------------------------------------------------------------------------}

destructor TStAnsiTextStream.Destroy;
begin
  {if needed, free the fixed line buffer}
  if Assigned(FFixedLine) then
    FreeMem(FFixedLine, FixedLineLength);
  {free the line index}
  FLineIndex.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.AtEndOfStream : Boolean;
begin
  Result := FSize = (FBufOfs + FBufPos);
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsGetLine(var aStartPos : longint;
                                       var aEndPos   : longint;
                                       var aLen      : longint);
var
  Done   : Boolean;
  Ch     : AnsiChar;
  PrevCh : AnsiChar;
begin
  if (LineTerminator = ltNone) then begin
    aStartPos := FBufOfs + FBufPos;
    aEndPos := Seek(aStartPos + FixedLineLength, soFromBeginning);
    aLen := aEndPos - aStartPos;
  end
  else begin
    aStartPos := FBufOfs + FBufPos;
    Ch := #0;
    Done := False;
    while not Done do begin
      PrevCh := Ch;
      if not bsReadAnsiChar(Ch) then begin
        Done := True;
        aEndPos := FBufOfs + FBufPos;
        aLen := aEndPos - aStartPos;
      end
      else begin
        case LineTerminator of
          ltNone : {this'll never get hit};
          ltCR   : if (Ch = #13) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
          ltLF   : if (Ch = #10) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
          ltCRLF : if (Ch = #10) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     if PrevCh = #13 then
                       aLen := aEndPos - aStartPos - 2
                     else
                       aLen := aEndPos - aStartPos - 1;
                   end;
          ltOther: if (Ch = LineTermChar) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
        else
          RaiseStError(ESsBufStreamError, ssscBadTerminator);
        end;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.atsGetLineCount : longint;
begin
  if FLineCount < 0 then
    Result := MaxLongInt
  else
    Result := FLineCount;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsResetLineIndex;
begin
  {make sure we have a line index}
  if not Assigned(FLineIndex) then begin
    FLineIndex := TList.Create;  {create the index: even elements are}
    FLineIndex.Count := LineIndexCount * 2; {linenums, odd are offsets}

    {if we didn't have a line index, set up some reasonable defaults}
    FLineTerm := ltCRLF;  {normal Windows text file terminator}
    FLineEndCh := #10;    {not used straight away}
    FLineLen := 80;       {not used straight away}
  end;
  FLineIndex[0] := pointer(0); {the first line is line 0 and...}
  FLineIndex[1] := pointer(0); {...it starts at position 0}
  FLineInxTop := 0;            {the top valid index}
  FLineInxStep := 1;           {step count before add a line to index}
  FLineCount := -1;            {number of lines (-1 = don't know)}
  FLineCurrent := 0;           {current line}
  FLineCurOfs := 0;            {current line offset}
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsSetLineTerm(aValue : TStLineTerminator);
begin
  if (aValue <> LineTerminator) and ((FBufOfs + FBufPos) = 0) then begin
    {if there was no terminator, free the line buffer}
    if (LineTerminator = ltNone) then begin
      FreeMem(FFixedLine, FixedLineLength);
      FFixedLine := nil;
    end;
    {set the new value}
    FLineTerm := aValue;
    {if there is no terminator now, allocate the line buffer}
    if (LineTerminator = ltNone) then begin
      GetMem(FFixedLine, FixedLineLength);
    end;
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsSetLineEndCh(aValue : AnsiChar);
begin
  if ((FBufOfs + FBufPos) = 0) then begin
    FLineEndCh := aValue;
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsSetLineLen(aValue : Integer);
begin
  if (aValue <> FixedLineLength) and ((FBufOfs + FBufPos) = 0) then begin
    {validate the new length first}
    if (aValue < 1) or (aValue > 1024) then
      RaiseStError(ESsBufStreamError, ssscBadLineLength);

    {set the new value; note that if there is no terminator we need to
     free the old line buffer, and then allocate a new one}
    if (LineTerminator = ltNone) then
      FreeMem(FFixedLine, FixedLineLength);
    FLineLen := aValue;
    if (LineTerminator = ltNone) then
      GetMem(FFixedLine, FixedLineLength);
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.bsInitForNewStream;
begin
  inherited bsInitForNewStream;
  atsResetLineIndex;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.ReadLine : AnsiString;
var
  CurPos : longint;
  EndPos : longint;
  Len    : longint;
  StLen  : longint;
begin
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    SetLength(Result, StLen);
    if (Len < StLen) then
      FillChar(Result[Len+1], StLen-Len, ' ');
  end
  else {LineTerminator is not ltNone} begin
    SetLength(Result, Len);
  end;
  {read the line}
  Seek(CurPos, soFromBeginning);
  Read(Result[1], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.ReadLineArray(aCharArray : PAnsiChar;
                                         aLen       : TStMemSize)
                                                    : TStMemSize;
var
  CurPos : longint;
  EndPos : longint;
  Len    : longint;
  StLen  : longint;
begin
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    if (StLen > aLen) then
      StLen := aLen;
    if (Len < StLen) then
      FillChar(aCharArray[Len], StLen-Len, ' ');
    Result := StLen;
  end
  else {LineTerminator is not ltNone} begin
    if (Len > aLen) then
      Len := aLen;
    Result := Len;
  end;
  Seek(CurPos, soFromBeginning);
  Read(aCharArray[0], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.ReadLineZ(aSt : PAnsiChar; aMaxLen : TStMemSize) : PAnsiChar;
var
  CurPos : longint;
  EndPos : longint;
  Len    : longint;
  StLen  : longint;
begin
  Result := aSt;
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    if (StLen > aMaxLen) then
      StLen := aMaxLen;
    if (Len < StLen) then
      FillChar(Result[Len], StLen-Len, ' ');
    Result[StLen] := #0;
  end
  else {LineTerminator is not ltNone} begin
    if (Len > aMaxLen) then
      Len := aMaxLen;
    Result[Len] := #0;
  end;
  Seek(CurPos, soFromBeginning);
  Read(Result[0], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.SeekNearestLine(aOffset : longint) : longint;
var
  CurLine : longint;
  CurOfs  : longint;
  CurPos  : longint;
  EndPos  : longint;
  Len     : longint;
  Done    : Boolean;
  L, R, M : Integer;
begin
  {if the offset we want is for the current line, reposition at the
   current line offset, return the current line number and exit}
  if (aOffset = FLineCurOfs) then begin
    Seek(FLineCurOfs, soFromBeginning);
    Result := FLineCurrent;
    Exit;
  end;
  {if the offset requested is less than or equal to zero, just
   position at line zero (ie, the start of the stream)}
  if (aOffset <= 0) then begin
    Seek(0, soFromBeginning);
    FLineCurrent := 0;
    FLineCurOfs := 0;
    Result := 0;
    Exit;
  end;
  {if the offset requested is greater than or equal to the size of the
   stream, position at the end of the stream (note that if we don't
   know the number of lines in the stream yet, FLineCount is set to
   -1 and we can't take this shortcut because we need to return the
   true value)}
  if (FLineCount >= 0) and (aOffset >= FSize) then begin
    Seek(0, soFromEnd);
    FLineCurrent := FLineCount;
    FLineCurOfs := FSize;
    Result := FLineCount;
    Exit;
  end;
  {if the offset requested is greater than the top item in the
   line index, we shall have to build up the index until we get to the
   line we require, or just beyond}
  if (aOffset > longint(FLineIndex[FLineInxTop+1])) then begin
    {position at the last known line offset}
    CurLine := longint(FLineIndex[FLineInxTop]);
    CurOfs := longint(FLineIndex[FLineInxTop+1]);
    Seek(CurOfs, soFromBeginning);
    Done := False;
    {continue reading lines in chunks of FLineInxStep and add an index
     entry for each chunk}
    while not Done do begin
      for var I := 0 to pred(FLineInxStep) do begin
        atsGetLine(CurPos, EndPos, Len);
        Inc(CurLine);
        CurOfs := EndPos;
        if (EndPos = FSize) then begin
          Done := True;
          Break;
        end;
      end;
      if Done then
        FLineCount := CurLine
      else begin
        Inc(FLineInxTop, 2);
        if (FLineInxTop = (LineIndexCount * 2)) then begin
          {we've exhausted the space in the index: rescale}
          FLineInxTop := FLineInxTop div 2;
          for var I := 0 to pred(FLineInxTop) do begin
            if Odd(I) then
              FLineIndex.Exchange((I*2)-1, I)
            else
              FLineIndex.Exchange(I*2, I);
          end;
          FLineInxStep := FLineInxStep * 2;
        end;
        FLineIndex[FLineInxTop] := pointer(CurLine);
        FLineIndex[FLineInxTop+1] := pointer(CurOfs);
        if (aOffset <= CurOfs) then
          Done := True;
      end;
    end;
  end;
  {we can now work out where the nearest item in the index is to the
   line we require}
  L := 1;
  R := FLineInxTop+1;
  while (L <= R) do begin
    M := (L + R) div 2;
    if not Odd(M) then
      Inc(M);
    if (aOffset < longint(FLineIndex[M])) then
      R := M - 2
    else if (aOffset > longint(FLineIndex[M])) then
      L := M + 2
    else begin
      FLineCurrent := longint(FLineIndex[M-1]);
      FLineCurOfs := longint(FLineIndex[M]);
      Seek(FLineCurOfs, soFromBeginning);
      Result := FLineCurrent;
      Exit;
    end;
  end;
  {the item at L-2 will have the nearest smaller offset than the
   one we want, hence the nearest smaller line is at L-3; start here
   and read through the stream forwards}
  CurLine := longint(FLineIndex[L-3]);
  Seek(longint(FLineIndex[L-2]), soFromBeginning);
  while True do begin
    atsGetLine(CurPos, EndPos, Len);
    Inc(CurLine);
    if (EndPos > aOffset) then begin
      FLineCurrent := CurLine - 1;
       FLineCurOfs := CurPos;
      Seek(CurPos, soFromBeginning);
      Result := CurLine - 1;
      Exit;
    end
    else if (CurLine = FLineCount) or (EndPos = aOffset) then begin
      FLineCurrent := CurLine;
      FLineCurOfs := EndPos;
      Seek(EndPos, soFromBeginning);
      Result := CurLine;
      Exit;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.SeekLine(aLineNum : longint) : longint;
var
  CurLine : longint;
  CurOfs  : longint;
  CurPos  : longint;
  EndPos  : longint;
  Len     : longint;
  Done    : Boolean;
  L, R, M : Integer;
begin
  {if the line number we want is the current line, reposition at the
   current line offset, return the current line number and exit}
  if (aLineNum = FLineCurrent) then begin
    Seek(FLineCurOfs, soFromBeginning);
    Result := FLineCurrent;
    Exit;
  end;
  {if the line number requested is less than or equal to zero, just
   position at line zero (ie, the start of the stream)}
  if (aLineNum <= 0) then begin
    Seek(0, soFromBeginning);
    FLineCurrent := 0;
    FLineCurOfs := 0;
    Result := 0;
    Exit;
  end;
  {if the line number requested is greater than or equal to the line
   count, position at the end of the stream (note that if we don't
   know the number of lines in the stream yet, FLineCount is set to
   -1)}
  if (FLineCount >= 0) and (aLineNum > FLineCount) then begin
    Seek(0, soFromEnd);
    FLineCurrent := FLineCount;
    FLineCurOfs := FSize;
    Result := FLineCount;
    Exit;
  end;
  {if the line number requested is greater than the top item in the
   line index, we shall have to build up the index until we get to the
   line we require, or just beyond}
  if (aLineNum > longint(FLineIndex[FLineInxTop])) then begin
    {position at the last known line offset}
    CurLine := longint(FLineIndex[FLineInxTop]);
    CurOfs := longint(FLineIndex[FLineInxTop+1]);
    Seek(CurOfs, soFromBeginning);
    Done := False;
    {continue reading lines in chunks of FLineInxStep and add an index
     entry for each chunk}
    while not Done do begin
      for var I := 0 to pred(FLineInxStep) do begin
        atsGetLine(CurPos, EndPos, Len);
        Inc(CurLine);
        CurOfs := EndPos;
        if (EndPos = FSize) then begin
          Done := True;
          Break;
        end;
      end;
      if Done then
        FLineCount := CurLine
      else begin
        Inc(FLineInxTop, 2);
        if (FLineInxTop = (LineIndexCount * 2)) then begin
          {we've exhausted the space in the index: rescale}
          FLineInxTop := FLineInxTop div 2;
          for var I := 0 to pred(FLineInxTop) do begin
            if Odd(I) then
              FLineIndex.Exchange(I*2-1, I)
            else
              FLineIndex.Exchange(I*2, I);
          end;
          FLineInxStep := FLineInxStep * 2;
        end;
        FLineIndex[FLineInxTop] := pointer(CurLine);
        FLineIndex[FLineInxTop+1] := pointer(CurOfs);
        if (aLineNum <= CurLine) then
          Done := True;
      end;
    end;
  end;
  {we can now work out where the nearest item in the index is to the
   line we require}
  L := 0;
  R := FLineInxTop;
  while (L <= R) do begin
    M := (L + R) div 2;
    if Odd(M) then
      Dec(M);
    if (aLineNum < longint(FLineIndex[M])) then
      R := M - 2
    else if (aLineNum > longint(FLineIndex[M])) then
      L := M + 2
    else begin
      FLineCurrent := longint(FLineIndex[M]);
      FLineCurOfs := longint(FLineIndex[M+1]);
      Seek(FLineCurOfs, soFromBeginning);
      Result := FLineCurrent;
      Exit;
    end;
  end;
  {the item at L-2 will have the nearest smaller line number than the
   one we want; start here and read through the stream forwards}
  CurLine := longint(FLineIndex[L-2]);
  Seek(longint(FLineIndex[L-1]), soFromBeginning);
  while True do begin
    atsGetLine(CurPos, EndPos, Len);
    Inc(CurLine);
    if (CurLine = FLineCount) or (CurLine = aLineNum) then begin
      FLineCurrent := CurLine;
      FLineCurOfs := EndPos;
      Seek(EndPos, soFromBeginning);
      Result := CurLine;
      Exit;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.WriteLine(const aSt : AnsiString);
begin
  WriteLineArray(@aSt[1], Length(aSt));
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.WriteLineArray(aCharArray : PAnsiChar;
                                           aLen       : TStMemSize);
var
  C : AnsiChar;
begin
  if not Assigned(aCharArray) then
    aLen := 0;
  if (LineTerminator = ltNone) then begin
    if (aLen >= FixedLineLength) then
      Write(aCharArray[0], FixedLineLength)
    else begin
      FillChar(FFixedLine[aLen], FixedLineLength-aLen, ' ');
      if (aLen > 0) then
        Move(aCharArray[0], FFixedLine[0], aLen);
      Write(FFixedLine[0], FixedLineLength);
    end;
  end
  else begin
    if (aLen > 0) then
      Write(aCharArray[0], aLen);
    case LineTerminator of
      ltNone : {this'll never get hit};
      ltCR   : Write(LineTerm[ltCR], 1);
      ltLF   : Write(LineTerm[ltLF], 1);
      ltCRLF : Write(LineTerm[ltCRLF], 2);
      ltOther: begin
                 C := LineTermChar;
                 Write(C, 1);
               end;
    else
      RaiseStError(ESsBufStreamError, ssscBadTerminator);
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.WriteLineZ(aSt : PAnsiChar);
var
  LenSt : TStMemSize;
begin
  if not Assigned(aSt) then
    LenSt := 0
  else
    LenSt := AnsiStrings.StrLen(aSt);
  WriteLineArray(aSt, LenSt);
end;


{-----------------------------------------------------------------------------}
{                           TStMemoryMappedFile                               }
{-----------------------------------------------------------------------------}

constructor TStMemoryMappedFile.Create(FileName   : string;
                                       MaxSize    : Cardinal;
                                       ReadOnly   : Boolean;
                                       SharedData : Boolean);
var
  RO1,
  RO2,
  RO3,
  RO4,
  FHi    : DWORD;
  SetSize: Boolean;
begin
  inherited Create;

  FMutex := CreateMutex(nil, False, nil);
  FSharedData := SharedData;
  if (FSharedData) then
    FHeaderSize := SizeOf(Word) + SizeOf(Cardinal)
  else
    FHeaderSize := 0;

  FReadOnly := ReadOnly;
  if (SharedData) then
    FReadOnly := False;
  if (FReadOnly) then begin
    RO1 := GENERIC_READ;
    RO2 := FILE_ATTRIBUTE_READONLY;
    RO3 := PAGE_READONLY;
    RO4 := FILE_MAP_READ;
    FMaxHi := 0;
    FMaxLo := 0;
  end else begin
    RO1 := GENERIC_READ or GENERIC_WRITE;
    RO2 := FILE_ATTRIBUTE_NORMAL;
    RO3 := PAGE_READWRITE;
    RO4 := FILE_MAP_WRITE;
    FMaxHi := 0;
    FMaxLo := MaxSize;
  end;

  if (not SharedData) then begin
    FHandle := CreateFile(PChar(FileName),
                          RO1,
                          FILE_SHARE_READ or FILE_SHARE_WRITE,
                          nil,
                          OPEN_ALWAYS,
                          RO2,
                          0);

    if (FHandle = INVALID_HANDLE_VALUE) then
      RaiseStError(ESsBufStreamError, ssscCreateFileFailed);

    {reset FMaxLo if file is read/write and less < FileSize}
    {the result is that the file size cannot be changed but the contents can}
    {still be modified}
    FDataSize := GetFileSize(FHandle, @FHi);
    if (FDataSize <> $FFFFFFFF) then begin
      if (not ReadOnly) and (FDataSize > FMaxLo) then
        FMaxLo := FDataSize;
    end else begin
      CloseHandle(FHandle);
      RaiseStError(ESsBufStreamError, ssscGetSizeFailed);
    end;
  end else
    FDataSize := 0;

  if (not SharedData) then begin
    FMapObj := CreateFileMapping(FHandle, nil, RO3, FMaxHi, FMaxLo, nil);
    SetSize := False;
  end else begin
    if (FMaxLo > (High(Cardinal) - FHeaderSize)) then
      FMaxLo := High(Cardinal) - FHeaderSize
    else
      FMaxLo := FMaxLo + FHeaderSize;
    FMapObj := CreateFileMapping(THandle($FFFFFFFF), nil, RO3,
                                 FMaxHi, FMaxLo, 'STMMFILE1');
    SetSize := (GetLastError = ERROR_ALREADY_EXISTS);
  end;

  if (FMapObj = INVALID_HANDLE_VALUE) then
    RaiseStError(ESsBufStreamError, ssscFileMappingFailed);

  FBuffer := MapViewOfFile(FMapObj, RO4, 0, 0, FMaxLo);
  if (not Assigned(FBuffer)) then
    RaiseStError(ESsBufStreamError, ssscCreateViewFailed);

  if (SharedData) then begin
    if (SetSize) then
      Move(PByteArray(FBuffer)[SizeOf(Word)-1], FDataSize, SizeOf(Cardinal))
    else begin
      Move(FHeaderSize, PByteArray(FBuffer)[0], SizeOf(Word));
      FDataSize := 0;
      Move(FDataSize, PByteArray(FBuffer)[SizeOf(Word)-1], SizeOf(Cardinal));
    end;
  end;
  {set position to beginning}
  FPos := FHeaderSize;
end;

{-----------------------------------------------------------------------------}

destructor TStMemoryMappedFile.Destroy;
begin
{Close the View and Mapping object}
  UnmapViewOfFile(FBuffer);
  FBuffer := nil;
  CloseHandle(FMapObj);

  if (not SharedData) then begin
{set the file pointer to the end of the actual data}
    SetFilePointer(FHandle, FDataSize, nil, FILE_BEGIN);
{set the EOF marker to the end of actual data}
    SetEndOfFile(FHandle);
    CloseHandle(FHandle);
  end;

  {now the Mutex can be cleared}
  CloseHandle(FMutex);
  FMutex := 0;

  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.GetDataSize : Cardinal;
begin
  Move(PByteArray(FBuffer)[SizeOf(Word)-1], FDataSize, SizeOf(Cardinal));
  Result := FDataSize;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.Read(var Buffer; Count : Longint) : Longint;
var
  ByteArray : TByteArray absolute Buffer;
begin
  {check to make sure that the read does not go beyond the actual data}
  if (((FPos-FHeaderSize) + DWORD(Count)) > FDataSize) then
    Count := FDataSize - FPos + FHeaderSize;

  if (SharedData) then begin
    WaitForSingleObject(FMutex, INFINITE);
    try
      Move(PByteArray(FBuffer)[FPos], ByteArray[0], Count);
      Inc(FPos, Count);
      Result := Count;
    finally
      ReleaseMutex(FMutex);
    end;
  end else begin
    Move(PByteArray(FBuffer)[FPos], ByteArray[0], Count);
    Inc(FPos, Count);
    Result := Count;
  end;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.Write(const Buffer; Count : Longint) : Longint;
var
  ByteArray : TByteArray absolute Buffer;
begin
  if (ReadOnly) then begin
    Result := 0;
    Exit;
  end;

  {check that the write does not go beyond the maximum file size}
  if ((FPos + DWORD(Count)) > pred(FMaxLo)) then
    Count := pred(FMaxLo - FPos);

  if (SharedData) then begin
    WaitForSingleObject(FMutex, INFINITE);
    try
      Move(ByteArray[0], PByteArray(FBuffer)[FPos], Count);
      Inc(FPos, Count);
      {if the write went beyond the previous end of data, update FDataSize}
      if ((FPos-FHeaderSize) > FDataSize) then
        FDataSize := FPos-FHeaderSize;
      Move(FDataSize, PByteArray(FBuffer)[SizeOf(Word)-1], SizeOf(Cardinal));
      Result := Count;
    finally
      ReleaseMutex(FMutex);
    end;
  end else begin
    Move(ByteArray[0], PByteArray(FBuffer)[FPos], Count);
    Inc(FPos, Count);
    {if the write went beyond the previous end of data, update FDataSize}
    if ((FPos-FHeaderSize) > FDataSize) then
      FDataSize := FPos-FHeaderSize;
    Move(FDataSize, PByteArray(FBuffer)[SizeOf(Word)-1], SizeOf(Cardinal));
    Result := Count;
  end;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.Seek(Offset : Longint; Origin : Word) : Longint;
begin
  if (SharedData) then begin
    WaitForSingleObject(FMutex, INFINITE);
    try
      case Origin of
        {$WARNINGS OFF}
        soFromBeginning : FPos := Offset + FHeaderSize;
        soFromCurrent   : FPos := FPos + Offset + FHeaderSize;
        {the seek should be based on actual data, not the mapped size since}
        {the 'data' between FDataSize and the mapped size is undefined}
        soFromEnd       : FPos := FDataSize + Offset + FHeaderSize;
        {$WARNINGS ON}
      else
        RaiseStError(ESsBufStreamError, ssscBadOrigin);
      end;

      {force the new position to be valid}
      if ((FPos-FHeaderSize) > FDataSize) then
        FPos := FDataSize + FHeaderSize;
      Result := FPos;
    finally
      ReleaseMutex(FMutex);
    end;
  end else begin
    {$WARNINGS OFF}
    case Origin of
      soFromBeginning : FPos := Offset + FHeaderSize;
      soFromCurrent   : FPos := FPos + Offset + FHeaderSize;
      {the seek should be based on actual data, not the mapped size since}
      {the 'data' between FDataSize and the mapped size is undefined}
      soFromEnd       : FPos := FDataSize + Offset + FHeaderSize;
    else
      RaiseStError(ESsBufStreamError, ssscBadOrigin);
    end;
    {$WARNINGS ON}

    {force the new position to be valid}
    if ((FPos-FHeaderSize) > FDataSize) then
      FPos := FDataSize + FHeaderSize;
    Result := FPos;
  end;
end;

{-----------------------------------------------------------------------------}

{$IFDEF UNICODE}

{ TStTextStream }

function TStTextStream.AtEndOfStream: Boolean;
begin
  Result := FSize = (FBufOfs + FBufPos);
end;

procedure TStTextStream.atsGetLine(out aStartPos, aEndPos, aLen: Integer);
var
  Done   : Boolean;
  Ch     : Char;
  PrevCh : Char;
begin
  if (LineTerminator = ltNone) then begin
    aStartPos := FBufOfs + FBufPos;
    aEndPos := Seek(aStartPos + FixedLineLength, soFromBeginning);
    aLen := aEndPos - aStartPos;
  end
  else begin
    aStartPos := FBufOfs + FBufPos;
    Ch := #0;
    Done := False;
    while not Done do begin
      PrevCh := Ch;
      if not bsReadChar(Ch) then begin
        Done := True;
        aEndPos := FBufOfs + FBufPos;
        aLen := aEndPos - aStartPos;
      end
      else begin
        case LineTerminator of
          ltNone : {this'll never get hit};
          ltCR   : if (Ch = #13) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
          ltLF   : if (Ch = #10) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
          ltCRLF : if (Ch = #10) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     if PrevCh = #13 then
                       aLen := aEndPos - aStartPos - 2
                     else
                       aLen := aEndPos - aStartPos - 1;
                   end;
          ltOther: if (Ch = LineTermChar) then begin
                     Done := True;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
        else
          RaiseStError(ESsBufStreamError, ssscBadTerminator);
        end;
      end;
    end;
  end;
end;

function TStTextStream.atsGetLineCount: longint;
begin
  if FLineCount < 0 then
    Result := High(Result)
  else
    Result := FLineCount;
end;

procedure TStTextStream.atsResetLineIndex;
begin
  {make sure we have a line index}
  if not Assigned(FLineIndex) then begin
    FLineIndex := TList<Int64>.Create;  {create the index: even elements are}

    {if we didn't have a line index, set up some reasonable defaults}
    FLineTerm := ltCRLF;  {normal Windows text file terminator}
    FLineEndCh := #10;    {not used straight away}
    FLineLen := 80;       {not used straight away}
  end;
  FLineIndex.Add(0);      {the first line is line 0 and it starts at position 0}
  FLineInxTop := 0;            {the top valid index}
  FLineInxStep := 1;           {step count before add a line to index}
  FLineCount := -1;            {number of lines (-1 = don't know)}
  FLineCurrent := 0;           {current line}
  FLineCurOfs := 0;            {current line offset}
end;

procedure TStTextStream.atsSetLineEndCh(aValue: Char);
begin
  if ((FBufOfs + FBufPos) = 0) then begin
    FLineEndCh := aValue;
    atsResetLineIndex;
  end;
end;

procedure TStTextStream.atsSetLineLen(aValue: Integer);
begin
  if (aValue <> FixedLineLength) and ((FBufOfs + FBufPos) = 0) then begin
    {validate the new length first}
    if (aValue < 1) or (aValue > 1024) then
      RaiseStError(ESsBufStreamError, ssscBadLineLength);

    {set the new value; note that if there is no terminator we need to
     free the old line buffer, and then allocate a new one}
    if (LineTerminator = ltNone) then
      FreeMem(FFixedLine);
    FLineLen := aValue;
    if (LineTerminator = ltNone) then
      GetMem(FFixedLine, FixedLineLength);
    atsResetLineIndex;
  end;
end;

procedure TStTextStream.atsSetLineTerm(aValue: TStLineTerminator);
begin
  if (aValue <> LineTerminator) and ((FBufOfs + FBufPos) = 0) then begin
    {if there was no terminator, free the line buffer}
    if (LineTerminator = ltNone) then begin
      FreeMem(FFixedLine);
      FFixedLine := nil;
    end;
    {set the new value}
    FLineTerm := aValue;
    {if there is no terminator now, allocate the line buffer}
    if (LineTerminator = ltNone) then begin
      GetMem(FFixedLine, FixedLineLength);
    end;
    atsResetLineIndex;
  end;
end;

procedure TStTextStream.bsInitForNewStream;
begin
  inherited bsInitForNewStream;
  atsResetLineIndex;
end;

function TStTextStream.bsReadChar(var aCh: Char): Boolean;
var
  tmp: AnsiChar;

  function SwapWord(Int: Char): Char;
  begin
    Result := Char((Byte(Int) shl 8) or (Byte(Int) shr 8))  // Rr changed uncheckde
  end;

begin
  if FEncoding.IsSingleByte then
  begin
    Result := bsReadAnsiChar(tmp);
    if Result then
      aCh := Char(tmp);
  end
  else if FEncoding = TEncoding.Unicode then
    Result := bsReadChar(aCh)
  else if FEncoding = TEncoding.BigEndianUnicode then
  begin
    Result := bsReadChar(aCh);
    SwapWord(aCh);
  end
  else if FEncoding = TEncoding.UTF8 then
    raise EStreamError.Create('TFU8 Encoding is not supported yet.')
  else
    raise EStreamError.Create('Encoding is not supported yet.');
end;

constructor TStTextStream.Create(aStream: TStream);
begin
  inherited Create(aStream);

  {set up the line index variables}
  atsResetLineIndex;
end;

destructor TStTextStream.Destroy;
begin
  {if needed, free the fixed line buffer}
  if Assigned(FFixedLine) then
    FreeMem(FFixedLine);
  {free the line index}
  FLineIndex.Free;
  inherited Destroy;
end;

function TStTextStream.ReadLine: string;
var
  CurPos : longint;
  EndPos : longint;
  Len    : longint;
  StLen  : longint;
  tmp    : TBytes;
begin
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    SetLength(tmp, StLen);
    if (Len < StLen) then
      FillChar(tmp[Len+1], StLen-Len, ' ');     // ~~~ TODO
  end
  else {LineTerminator is not ltNone} begin
    SetLength(tmp, Len);
  end;
  {read the line}
  Seek(CurPos, soFromBeginning);
  Read(tmp[0], Len);
  Seek(EndPos, soFromBeginning);
  Result := FEncoding.GetString(tmp);
end;

function TStTextStream.ReadLineArray(aCharArray: PChar;
  aLen: TStMemSize): TStMemSize;
var
  Str: string;
begin
  Str := Copy(ReadLine, 1, aLen);
  Result := Length(Str);
  StrPLCopy(aCharArray, Str, aLen);
end;

function TStTextStream.ReadLineZ(aSt: PChar; aMaxLen: TStMemSize): PChar;
var
  Len: Integer;
begin
  Len := ReadLineArray(aSt, aMaxLen);
  aSt[Len] := #0;
  Result := aSt;
end;

function TStTextStream.SeekLine(aLineNum: Integer): longint;
begin
  Result:= 0;
end;

function TStTextStream.SeekNearestLine(aOffset: Integer): longint;
begin
  Result:= 0;
end;

procedure TStTextStream.WriteLine(const aSt: string);
begin

end;

procedure TStTextStream.WriteLineArray(aCharArray: PChar; aLen: TStMemSize);
begin

end;

procedure TStTextStream.WriteLineZ(aSt: PChar);
begin

end;

{$ENDIF}

end.


