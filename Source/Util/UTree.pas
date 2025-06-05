unit UTree; // translated

interface

type

  TNode = class
    Key: Word;
    OffX, OffY: Integer;
    Collision: Boolean;
    Data: string;
    Left: TNode;
    Right: TNode;
    Upp: TNode;
    constructor Create(Data: string);
  end;

  TTree = class
  private
    FRoot: TNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert(const Data: string);
    function InOrder: string;
    function InsertKey(Key: Word; const Def: string; XPos, YPos: Integer;
      Collision: Boolean): Boolean;
    function GetNode(Key: Word): TNode;
    procedure Delete;
  end;

implementation

uses SysUtils;

constructor TNode.Create(Data: string);
begin
  Key := 0;
  OffX := 0;
  OffY := 0;
  Collision := False;
  Self.Data := Data;
  Left := nil;
  Right := nil;
  Upp := nil;
end;

constructor TTree.Create;
begin
  FRoot := nil;
end;

procedure TTree.Insert(const Data: string);
begin
  var
  Node := TNode.Create(Data);
  if not Assigned(FRoot) then
    FRoot := Node
  else
  begin
    var
    Cursor := FRoot;
    repeat
      if Data < Cursor.Data then
        if not Assigned(Cursor.Left) then
        begin
          Cursor.Left := Node;
          Node.Upp := Cursor;
          Break;
        end
        else
          Cursor := Cursor.Left
      else if not Assigned(Cursor.Right) then
      begin
        Cursor.Right := Node;
        Node.Upp := Cursor;
        Break;
      end
      else
        Cursor := Cursor.Right;
    until False;
  end;
end;

function TTree.InsertKey(Key: Word; const Def: string; XPos, YPos: Integer;
  Collision: Boolean): Boolean;
begin
  Result := True;
  var
  Node := TNode.Create(Def);
  Node.Key := Key;
  Node.OffX := XPos;
  Node.OffY := YPos;
  Node.Collision := Collision;

  if not Assigned(FRoot) then
    FRoot := Node
  else
  begin
    var
    Cursor := FRoot;
    repeat
      if Key = Cursor.Key then
      begin
        Cursor.Data := Def;
        Cursor.OffX := XPos;
        Cursor.OffY := YPos;
        FreeAndNil(Node);
        Result := False;
        Break;
      end;
      if Key < Cursor.Key then
        if not Assigned(Cursor.Left) then
        begin
          Cursor.Left := Node;
          Node.Upp := Cursor;
          Break;
        end
        else
          Cursor := Cursor.Left
      else if not Assigned(Cursor.Right) then
      begin
        Cursor.Right := Node;
        Node.Upp := Cursor;
        Break;
      end
      else
        Cursor := Cursor.Right;
    until False;
  end;
end;

function TTree.InOrder: string;

  function InorderTree(Node: TNode): string;
  var
    LeftTree, RightTree: string;
  begin
    if not Assigned(Node) then
      Result := ''
    else
    begin
      LeftTree := InorderTree(Node.Left);
      RightTree := InorderTree(Node.Right);
      Result := LeftTree + ' ' + Node.Data + ' ' + RightTree;
    end;
  end;

begin
  Result := InorderTree(FRoot);
end;

function TTree.GetNode(Key: Word): TNode;
begin
  var
  Cursor := FRoot;
  while Assigned(Cursor) and (Cursor.Key <> Key) do
  begin
    if Key < Cursor.Key then
      Cursor := Cursor.Left
    else
      Cursor := Cursor.Right;
    if not Assigned(Cursor) then
      Break;
  end;
  Result := Cursor;
end;

procedure TTree.Delete;

  procedure Delete(Node: TNode);
  begin
    if Assigned(Node.Left) then
      Delete(Node.Left);
    if Assigned(Node.Right) then
      Delete(Node.Right);
    FreeAndNil(Node);
  end;

begin
  if Assigned(FRoot) then
    Delete(FRoot);
  FRoot := nil;
end;

destructor TTree.Destroy;
begin
  Delete;
  inherited;
end;

end.
