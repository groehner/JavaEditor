unit UTree;   // translated

interface

type

  TNode = class
    Key: Word;
    OffX, OffY: Integer;
    Collision: boolean;
    Data : string;
    Left : TNode;
    Right: TNode;
    Up   : TNode;
    constructor Create(Data: string);
  end;

  TTree = class
  private
    Root: TNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert(const Data: string);
    function  InOrder: string;
    function InsertKey(Key: Word; const Def: string; x, y: Integer;
                       Collision:boolean): boolean;
    function getNode(Key: Word): TNode;
    procedure Delete;
  end;

implementation

uses SysUtils;

constructor TNode.Create(Data: string);
begin
  Key:= 0;
  OffX:= 0;
  OffY:= 0;
  Collision:= false;
  self.Data:= Data;
  Left := nil;
  Right:= nil;
  Up   := nil;
end;

constructor TTree.Create;
begin
  Root:= nil;
end;

procedure TTree.Insert(const Data: string);
begin
  var Node:= TNode.Create(Data);
  if Root = nil then
    Root:= Node
  else begin
    var Cursor:= Root;
    repeat
      if Data < Cursor.Data then
        if Cursor.Left = nil then begin
          Cursor.Left:= Node;
          Node.Up:= Cursor;
          break
        end else
          Cursor:= Cursor.Left
      else
        if Cursor.Right = nil then begin
          Cursor.Right:= Node;
          Node.Up:= Cursor;
          break
        end else
          Cursor:= Cursor.Right
    until false;
  end;
end;

function TTree.InsertKey(Key: Word; const Def: string; x, y: Integer;
                         Collision: boolean): boolean;
begin
  Result:= true;
  var Node:= TNode.Create(Def);
  Node.Key:= Key;
  Node.OffX:= x;
  Node.OffY:= y;
  Node.Collision:= Collision;

  if Root = nil then
    Root:= Node
  else begin
    var Cursor:= Root;
    repeat
      if Key = Cursor.Key then begin
        Cursor.Data:= Def;
        Cursor.OffX:= x;
        Cursor.OffY:= y;
        FreeAndNil(Node);
        Result:= false;
        break;
      end;
      if Key < Cursor.Key then
        if Cursor.Left = nil then begin
          Cursor.Left:= Node;
          Node.Up:= Cursor;
          break
        end else
          Cursor:= Cursor.Left
      else
        if Cursor.Right = nil then begin
          Cursor.Right:= Node;
          Node.Up:= Cursor;
          break
        end else
          Cursor:= Cursor.Right
    until false;
  end;
end;

function TTree.Inorder: string;

  function InorderTree(Node: TNode): string;
    var LeftTree, RightTree: string;
  begin
    if Node = nil then
      Result:= ''
    else begin
      LeftTree := InorderTree(Node.Left);
      RightTree:= InorderTree(Node.Right);
      Result:= LeftTree + ' ' + Node.Data + ' ' + RightTree;
    end;
  end;

begin
  Result:= InorderTree(Root);
end;

function TTree.getNode(Key: Word): TNode;
begin
  var Cursor:= Root;
  while (Cursor <> nil) and (Cursor.Key <> Key) do begin
    if Key < Cursor.Key
      then Cursor:= Cursor.Left
      else Cursor:= Cursor.Right;
    if Cursor = nil then break;
  end;
  Result:= Cursor;
end;

procedure TTree.Delete;

  procedure Delete(Node: TNode);
  begin
    if Node.Left <> nil then Delete(Node.Left);
    if Node.Right <> nil then Delete(Node.Right);
    FreeAndNil(Node);
  end;

begin
  if Root <> nil then
    Delete(Root);
  Root:= nil;
end;

destructor TTree.Destroy;
begin
  Delete;
end;

end.
