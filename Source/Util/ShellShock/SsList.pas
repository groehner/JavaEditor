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
 *   Sebastian Zierer (Unicode)
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: SsList.pas 1.02                           *}
{*********************************************************}
{* ShellShock: Linked list class                         *}
{*********************************************************}

{Notes:
  Nodes stored in the list can be of type TStListNode or of a derived type.
  Pass the node class to the list constructor.

  TStList is a doubly-linked list that can be scanned backward just as
  efficiently as forward.

  The list retains the index and node of the last node found by Nth (or by
  the indexed array property). This makes For loops that scan a list much
  faster and speeds up random calls to Nth by about a factor of two.
}

unit SsList;

interface

{$I SsDefine.inc}

uses
  Windows, Classes, SsBase;

type
  TStListNode = class(TStNode)
  {.Z+}
    protected
      FNext : TStListNode;     {Next node}
      FPrev : TStListNode;     {Previous node}

  {.Z-}
    public
      constructor Create(AData : Pointer); override;
        {-Initialize node}
  end;

  TStList = class(TStContainer)
  {.Z+}
  protected
    {property instance variables}
    FHead : TStListNode;     {Start of list}
    FTail : TStListNode;     {End of list}

    {private instance variables}
    lsLastI : LongInt;        {Last index requested from Nth}
    lsLastP : TStListNode;    {Last node returned by Nth}

    {protected undocumented methods}
    procedure ForEachPointer(Action : TIteratePointerFunc;
                             OtherData : pointer);
      override;
  function StoresPointers : Boolean;
    override;
  {.Z-}
  public
    constructor Create(NodeClass : TStNodeClass); virtual;
      {-Initialize an empty list}

    procedure LoadFromStream(Str : TStream); override;
      {-Create a list and its data from a stream}
    procedure StoreToStream(Str : TStream); override;
      {-Write a list and its data to a stream}

    procedure Clear; override;
      {-Remove all nodes from container but leave it instantiated}

    function Append(Data : Pointer) : TStListNode;
      {-Add a new node to the end of a list}
    function Insert(Data : Pointer) : TStListNode;
      {-Insert a new node at the start of a list}
    function Place(Data : Pointer; Posi : TStListNode) : TStListNode;
      {-Place a new node into a list after an existing node P}
    function PlaceBefore(Data : Pointer; Posi : TStListNode) : TStListNode;
      {-Place a new node into a list before an existing node P}
    function InsertSorted(Data : Pointer) : TStListNode;
      {-Insert a new node in sorted order}
    procedure MoveToHead(Posi : TStListNode);
      {-Move P to the head of the list}

    procedure Assign(Source: TPersistent); override;
      {-Assign another container's contents to this one}
    procedure Join(Posi : TStListNode; L : TStList);
      {-Join list L after P in the current list. L is freed}
    function Split(Posi : TStListNode) : TStList;
      {-Split list, creating a new list that starts with P}

    procedure Sort;
      {-Put the list into sorted order}

    procedure Delete(Posi : TStListNode);
      {-Remove an element and dispose of its contents}

    function Next(Posi : TStListNode) : TStListNode;
      {-Return the node after P, nil if none}
    function Prev(Posi : TStListNode) : TStListNode;
      {-Return the node before P, nil if none}
    function Nth(Index : LongInt) : TStListNode;
      {-Return the Index'th node in the list, Index >= 0 (cached)}
    function NthFrom(Posi : TStListNode; Index : LongInt) : TStListNode;
      {-Return the Index'th node from P, either direction}
    function Posn(Posi : TStListNode) : LongInt;
      {-Return the ordinal position of an element in the list}
    function Distance(Posi1, Posi2 : TStListNode) : LongInt;
      {-Return the number of nodes separating P1 and P2 (signed)}
    function Find(Data : Pointer) : TStListNode;
      {-Return the first node whose data equals Data}
    function Iterate(Action : TIterateFunc; Up : Boolean;
                     OtherData : Pointer) : TStListNode;
      {-Call Action for all the nodes, returning the last node visited}

    property Head : TStListNode
      {-Return the head node}
      read FHead;
    property Tail : TStListNode
      {-Return the tail node}
      read FTail;
    property Items[Index : LongInt] : TStListNode
      {-Return the Index'th node, 0-based}
      read Nth;
      default;
  end;

  {.Z+}
  TStListClass = class of TStList;
  {.Z-}

{======================================================================}

implementation

uses SsConst;

{$IFDEF ThreadSafe}
var
  ClassCritSect : TRTLCriticalSection;
{$ENDIF}

procedure EnterClassCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(ClassCritSect);
{$ENDIF}
end;

procedure LeaveClassCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(ClassCritSect);
{$ENDIF}
end;

constructor TStListNode.Create(AData : Pointer);
begin
  inherited Create(AData);
  {FNext := nil;}
  {FPrev := nil;}
end;

{----------------------------------------------------------------------}

function FindNode(Container : TStContainer;
                  Node : TStNode;
                  OtherData : Pointer) : Boolean; far;
begin
  Result := (Node.Data <> OtherData);
end;

function AssignData(Container : TStContainer;
                    Data, OtherData : Pointer) : Boolean; far;
  var
    OurList : TStList absolute OtherData;
  begin
    OurList.Append(Data);
    Result := True;
  end;

{----------------------------------------------------------------------}

function TStList.Append(Data : Pointer) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TStListNode(conNodeClass.Create(Data));
    N.FPrev := FTail;
    {N.FNext := nil;}
    if not Assigned(FHead) then begin
      {Special case for first node}
      FHead := N;
      FTail := N;
    end else begin
      {Add at end of existing list}
      FTail.FNext := N;
      FTail := N;
    end;
    Inc(FCount);
    Result := N;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.Assign(Source: TPersistent);
  begin
    {$IFDEF ThreadSafe}
    EnterCS;
    try
    {$ENDIF}
      {The only containers that we allow to be assigned to a linked list are
         - another ShellShock linked list (TStList)
         - a ShellShock binary search tree (TStTree)
         - a ShellShock collection (TStCollection, TStSortedCollection)}
      if not AssignPointers(Source, AssignData) then
        inherited Assign(Source);
    {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;{try..finally}
    {$ENDIF}
  end;

procedure TStList.Clear;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count > 0 then begin
      Iterate(DestroyNode, True, nil);
      FCount := 0;
    end;
    FHead := nil;
    FTail := nil;
    lsLastI := -1;
    lsLastP := nil;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

constructor TStList.Create(NodeClass : TStNodeClass);
begin
  CreateContainer(NodeClass, 0);
  Clear;
end;

procedure TStList.Delete(Posi : TStListNode);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (not Assigned(Posi)) or (Count <= 0) then
      Exit;
    if not (Posi is conNodeClass) then
      RaiseContainerError(ssscBadType);

    with Posi do begin
      {Fix pointers of surrounding nodes}
      if Assigned(FNext) then
        FNext.FPrev := FPrev;
      if Assigned(FPrev) then
        FPrev.FNext := FNext;
    end;

    {Fix head and tail of list}
    if FTail = Posi then
      FTail := FTail.FPrev;
    if FHead = Posi then
      FHead := FHead.FNext;

    {Dispose of the node}
    DisposeNodeData(Posi);
    Posi.Free;
    Dec(FCount);
    lsLastI := -1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Distance(Posi1, Posi2 : TStListNode) : LongInt;
var
  Int : LongInt;
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Count forward}
    Int := 0;
    N := Posi1;
    while Assigned(N) and (N <> Posi2) do begin
      Inc(Int);
      N := N.FNext;
    end;
    if N = Posi2 then begin
      Result := Int;
      Exit;
    end;

    {Count backward}
    Int := 0;
    N := Posi1;
    while Assigned(N) and (N <> Posi2) do begin
      Dec(Int);
      N := N.FPrev;
    end;
    if N = Posi2 then begin
      Result := Int;
      Exit;
    end;

    {Not on same list}
    Result := MaxLongInt;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Find(Data : Pointer) : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Result := Iterate(FindNode, True, Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.ForEachPointer(Action : TIteratePointerFunc;
                                 OtherData : pointer);
var
  N : TStListNode;
  Posi : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := FHead;
    while Assigned(N) do begin
      Posi := N.FNext;
      if Action(Self, N.Data, OtherData) then
        N := Posi
      else
        Exit;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Insert(Data : Pointer) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TStListNode(conNodeClass.Create(Data));
    {N.FPrev := nil;}
    N.FNext := FHead;
    if not Assigned(FHead) then
      {Special case for first node}
      FTail := N
    else
      {Add at start of existing list}
      FHead.FPrev := N;
    FHead := N;
    Inc(FCount);
    lsLastI := -1;
    Result := N;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.InsertSorted(Data : Pointer) : TStListNode;
var
  N : TStListNode;
  Posi : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TStListNode(conNodeClass.Create(Data));
    Result := N;
    Inc(FCount);
    lsLastI := -1;

    if not Assigned(FHead) then begin
      {First element added to list}
      FHead := N;
      FTail := N;
    end else begin
      Posi := FHead;
      while Assigned(Posi) do begin
        if DoCompare(N.Data, Posi.Data) < 0 then begin
          if not Assigned(Posi.FPrev) then begin
            {New head}
            FHead := N;
          end else begin
            Posi.FPrev.FNext := N;
            N.FPrev := Posi.FPrev;
          end;
          Posi.FPrev := N;
          N.FNext := Posi;
          Exit;
        end;
        Posi := Posi.FNext;
      end;
      {New tail}
      FTail.FNext := N;
      N.FPrev := FTail;
      FTail := N;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Iterate(Action : TIterateFunc; Up : Boolean;
                         OtherData : Pointer) : TStListNode;
var
  N : TStListNode;
  Posi : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Up then begin
      N := FHead;
      while Assigned(N) do begin
        Posi := N.FNext;
        if Action(Self, N, OtherData) then
          N := Posi
        else begin
          Result := N;
          Exit;
        end;
      end;
    end else begin
      N := FTail;
      while Assigned(N) do begin
        Posi := N.FPrev;
        if Action(Self, N, OtherData) then
          N := Posi
        else begin
          Result := N;
          Exit;
        end;
      end;
    end;
    Result := nil;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.Join(Posi : TStListNode; L : TStList);
var
  N : TStListNode;
  Q : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  L.EnterCS;
  try
{$ENDIF}
    if Assigned(L) then begin
      if Assigned(Posi) and (L.Count > 0) then begin
        {Patch the list into the current one}
        N := L.Head;
        Q := Posi.FNext;

        Posi.FNext := N;
        N.FPrev := Posi;

        if Assigned(Q) then begin
          N := L.Tail;
          N.FNext := Q;
          Q.FPrev := N;
        end;

        Inc(FCount, L.Count);
        lsLastI := -1;
      end;

      {Free L (but not its nodes)}
      L.IncNodeProtection;
      L.Free;
    end;
{$IFDEF ThreadSafe}
  finally
    L.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

procedure TStList.LoadFromStream(Str : TStream);
var
  Data : pointer;
  Reader : TReader;
  StreamedClass : TPersistentClass;
  StreamedNodeClass : TPersistentClass;
  StreamedClassName : string;
  StreamedNodeClassName : string;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Clear;
    Reader := TReader.Create(Str, 1024);
    try
      with Reader do
        begin
          StreamedClassName := ReadString;
          StreamedClass := GetClass(StreamedClassName);
          if not Assigned(StreamedClass) then
            RaiseContainerErrorFmt(ssscUnknownClass, [StreamedClassName]);
          if (not IsOrInheritsFrom(StreamedClass, Self.ClassType)) or
              (not IsOrInheritsFrom(TStList, StreamedClass)) then
            RaiseContainerError(ssscWrongClass);
          StreamedNodeClassName := ReadString;
          StreamedNodeClass := GetClass(StreamedNodeClassName);
          if not Assigned(StreamedNodeClass) then
            RaiseContainerErrorFmt(ssscUnknownNodeClass, [StreamedNodeClassName]);
          if (not IsOrInheritsFrom(StreamedNodeClass, conNodeClass)) or
              (not IsOrInheritsFrom(TStListNode, StreamedNodeClass)) then
            RaiseContainerError(ssscWrongNodeClass);
          ReadListBegin;
          while not EndOfList do
            begin
              Data := DoLoadData(Reader);
              Append(Data);
            end;
          ReadListEnd;
        end;
    finally
      Reader.Free;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.MoveToHead(Posi : TStListNode);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Assigned(Posi) then
      if Posi <> Head then begin
        with Posi do begin
          {Fix pointers of surrounding nodes}
          if FTail = Posi then
            FTail := FTail.FPrev
          else
            FNext.FPrev := FPrev;
          FPrev.FNext := FNext;

          FNext := FHead;
          FPrev := nil;
        end;
        FHead.FPrev := Posi;
        FHead := Posi;
     end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Next(Posi : TStListNode) : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Result := Posi.FNext;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Nth(Index : LongInt) : TStListNode;
var
  MinI : LongInt;
  MinP : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (Index < 0) or (Index >= FCount) then
      Result := nil
    else begin
      MinI := Index;
      MinP := FHead;
      if lsLastI >= 0 then
        {scan the fewest possible nodes}
        if Index <= lsLastI then begin
          if lsLastI-Index < Index then begin
            MinI := Index-lsLastI;
            MinP := lsLastP;
          end;
        end else if Index-lsLastI < FCount-1-Index then begin
          MinI := Index-lsLastI;
          MinP := lsLastP;
        end else begin
          MinI := Index-(FCount-1);
          MinP := FTail;
        end;

      Result := NthFrom(MinP, MinI);
      lsLastI := Index;
      lsLastP := Result;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.NthFrom(Posi : TStListNode; Index : LongInt) : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Assigned(Posi) then begin
      if not (Posi is conNodeClass) then
        RaiseContainerError(ssscBadType);
      if Index > 0 then begin
        for var I := 1 to Index do begin
          Posi := Posi.FNext;
          if not Assigned(Posi) then
            Break;
        end;
      end else begin
        for var I := 1 to -Index do begin
          Posi := Posi.FPrev;
          if not Assigned(Posi) then
            Break;
        end;
      end;
    end;
    Result := Posi;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Place(Data : Pointer; Posi : TStListNode) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if not Assigned(Posi) then
      Result := Insert(Data)
    else if Posi = FTail then
      Result := Append(Data)
    else begin
      N := TStListNode(conNodeClass.Create(Data));
      N.FPrev := Posi;
      N.FNext := Posi.FNext;
      Posi.FNext.FPrev := N;
      Posi.FNext := N;
      Inc(FCount);
      lsLastI := -1;
      Result := N;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.PlaceBefore(Data : Pointer; Posi : TStListNode) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (not Assigned(Posi)) or (Posi = Head) then
      {Place the new element at the start of the list}
      Result := Insert(Data)
    else begin
      {Patch in the new element}
      N := TStListNode(conNodeClass.Create(Data));
      N.FNext := Posi;
      N.FPrev := Posi.FPrev;
      Posi.FPrev.FNext := N;
      Posi.FPrev := N;
      lsLastI := -1;
      Inc(FCount);
      Result := N;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Posn(Posi : TStListNode) : LongInt;
var
  Int : LongInt;
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if not Assigned(Posi) then
      Result := -1
    else begin
      if not (Posi is conNodeClass) then
        RaiseContainerError(ssscBadType);
      Int := 0;
      N := FHead;
      while Assigned(N) do begin
        if Posi = N then begin
          Result := Int;
          Exit;
        end;
        Inc(Int);
        N := N.FNext;
      end;
      Result := -1;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Prev(Posi : TStListNode) : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Result := Posi.FPrev;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.Sort;
const
  StackSize = 32;
type
  Stack = array[0..StackSize-1] of TStListNode;
var
  L : TStListNode;
  R : TStListNode;
  PL : TStListNode;
  PR : TStListNode;
  PivotData : Pointer;
  TmpData : Pointer;
  Dist : LongInt;
  DistL : LongInt;
  DistR : LongInt;
  StackP : Integer;
  LStack : Stack;
  RStack : Stack;
  DStack : array[0..StackSize-1] of LongInt;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Need at least 2 elements to sort}
    if Count <= 1 then
      Exit;
    lsLastI := -1;

    {Initialize the stacks}
    StackP := 0;
    LStack[0] := FHead;
    RStack[0] := FTail;
    DStack[0] := Count-1;

    {Repeatedly take top partition from stack}
    repeat

      {Pop the stack}
      L := LStack[StackP];
      R := RStack[StackP];
      Dist := DStack[StackP];
      Dec(StackP);

      if L <> R then
        {Sort current partition}
        repeat

          {Load the pivot element}
          PivotData := NthFrom(L, Dist div 2).Data;
          PL := L;
          PR := R;
          DistL := Dist;
          DistR := Dist;

          {Swap items in sort order around the pivot index}
          repeat
            while DoCompare(PL.Data, PivotData) < 0 do begin
              PL := PL.FNext;
              Dec(Dist);
              Dec(DistR);
            end;
            while DoCompare(PivotData, PR.Data) < 0 do begin
              PR := PR.FPrev;
              Dec(Dist);
              Dec(DistL);
            end;
            if Dist >= 0 then begin
              if PL <> PR then begin
                {Swap the two elements}
                TmpData := PL.Data;
                PL.Data := PR.Data;
                PR.Data := TmpData;
              end;
              if Assigned(PL.FNext) then begin
                PL := PL.FNext;
                Dec(Dist);
                Dec(DistR);
              end;
              if Assigned(PR.FPrev) then begin
                PR := PR.FPrev;
                Dec(Dist);
                Dec(DistL);
              end;
            end;
          until Dist < 0;

          {Decide which partition to sort next}
          if DistL < DistR then begin
            {Right partition is bigger}
            if DistR > 0 then begin
              {Stack the request for sorting right partition}
              Inc(StackP);
              LStack[StackP] := PL;
              RStack[StackP] := R;
              DStack[StackP] := DistR;
            end;
            {Continue sorting left partition}
            R := PR;
            Dist := DistL;
          end else begin
            {Left partition is bigger}
            if DistL > 0 then begin
              {Stack the request for sorting left partition}
              Inc(StackP);
              LStack[StackP] := L;
              RStack[StackP] := PR;
              DStack[StackP] := DistL;
            end;
            {Continue sorting right partition}
            L := PL;
            Dist := DistR;
          end;

        until Dist <= 0;
    until StackP < 0;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Split(Posi : TStListNode) : TStList;
var
  Int : LongInt;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Int := Posn(Posi);
    if Int < 0 then begin
      Result := nil;
      Exit;
    end;

    {Create and initialize the new list}
    Result := TStListClass(ClassType).Create(conNodeClass);
    Result.Compare := Compare;
    Result.OnCompare := OnCompare;
    Result.DisposeData := DisposeData;
    Result.OnDisposeData := OnDisposeData;
    Result.LoadData := LoadData;
    Result.OnLoadData := OnLoadData;
    Result.StoreData := StoreData;
    Result.OnStoreData := OnStoreData;
    Result.FHead := Posi;
    Result.FTail := FTail;
    Result.FCount := Count-Int;
    Result.lsLastI := -1;

    {Truncate the old list}
    if Assigned(Posi.FPrev) then begin
      Posi.FPrev.FNext := nil;
      FTail := Posi.FPrev;
      Posi.FPrev := nil;
    end;
    if Posi = FHead then
      FHead := nil;
    FCount := Int;
    lsLastI := -1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.StoresPointers : Boolean;
begin
  Result := True;
end;

procedure TStList.StoreToStream(Str : TStream);
var
  Writer : TWriter;
  Walker : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Writer := TWriter.Create(Str, 1024);
    try
      with Writer do
        begin
          WriteString(Self.ClassName);
          WriteString(conNodeClass.ClassName);
          WriteListBegin;
          Walker := Head;
          while Walker <> nil do
            begin
              DoStoreData(Writer, Walker.Data);
              Walker := Next(Walker);
            end;
          WriteListEnd;
        end;
    finally
      Writer.Free;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{$IFDEF ThreadSafe}
initialization
  Windows.InitializeCriticalSection(ClassCritSect);
finalization
  Windows.DeleteCriticalSection(ClassCritSect);
{$ENDIF}
end.
