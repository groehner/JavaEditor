unit UQueue;

interface

type

  TNode = class
    Status: integer;
    Command: string;
    Next: TNode;
    constructor create;
  end;

  TQueue = class
    First: TNode;
    Last: TNode;
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
    procedure Enter(status: integer; const command: string);
    procedure Remove;
    function  Front: TNode;
    function  Empty: Boolean;
    procedure Clear;
  end;

implementation

uses SysUtils, UUtils;

constructor TNode.create;
begin
  Status:= 0;
  Command:= '';
  Next:= nil;
end;

constructor TQueue.Create;
begin
  First:= nil;
  Last:= nil;
end;

function TQueue.ToString: string;
begin
  var s:= '';
  var Cursor:= First;
  while Cursor <> nil do begin
    s:= s + SysUtils.IntToStr(Cursor.Status) + '|' + Cursor.Command + '  ';
    Cursor:= Cursor.Next;
  end;
  Result:= s;
end;

procedure TQueue.Enter(status: integer; const command: string);
begin
  var Node:= TNode.Create;
  if not Empty then
    Last.Next:= Node;
  Node.Status:= Status;
  Node.Command:= command;
  Node.Next:= nil;
  Last:= Node;
  if First = nil then
    First:= Last
end;

procedure TQueue.Remove;
begin
  if Empty then
    ErrorMsg('Fatal Error: Queue is empty!')
  else begin
    var Node:= First;
    First := First.Next;
    FreeAndNil(Node);
    if First = nil then Last:= nil;
  end;
end;

function TQueue.Front: TNode;
begin
  Result:= nil;
  if Empty
    then ErrorMsg('Fatal Error: Queue is empty.')
    else Result:= First;
end;

function TQueue.Empty: Boolean;
begin
  Result:= (First = nil);
end;

procedure TQueue.Clear;
begin
  while not empty do
    remove;
end;

destructor TQueue.Destroy;
begin
  Clear;
end;

end.
