unit UQueue;

interface

type

  TNode = class
    Status: Integer;
    Command: string;
    Next: TNode;
    constructor Create;
  end;

  TQueue = class
  private
    First: TNode;
    Last: TNode;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string; override;
    procedure Enter(Status: Integer; const Command: string);
    procedure Remove;
    function  Front: TNode;
    function  Empty: Boolean;
    procedure Clear;
  end;

implementation

uses SysUtils, UUtils;

constructor TNode.Create;
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
  var Str := '';
  var Cursor := First;
  while Assigned(Cursor) do begin
    Str := Str + SysUtils.IntToStr(Cursor.Status) + '|' + Cursor.Command + '  ';
    Cursor := Cursor.Next;
  end;
  Result := Str;
end;

procedure TQueue.Enter(Status: Integer; const Command: string);
begin
  var Node := TNode.Create;
  if not Empty then
    Last.Next := Node;
  Node.Status := Status;
  Node.Command := Command;
  Node.Next := nil;
  Last := Node;
  if not Assigned(First) then
    First := Last;
end;

procedure TQueue.Remove;
begin
  if Empty then
    ErrorMsg('Fatal Error: Queue is empty!')
  else begin
    var Node := First;
    First := First.Next;
    FreeAndNil(Node);
    if not Assigned(First) then
      Last := nil;
  end;
end;

function TQueue.Front: TNode;
begin
  Result := nil;
  if Empty
    then ErrorMsg('Fatal Error: Queue is empty.')
    else Result := First;
end;

function TQueue.Empty: Boolean;
begin
  Result := not Assigned(First);
end;

procedure TQueue.Clear;
begin
  while not Empty do
    Remove;
end;

destructor TQueue.Destroy;
begin
  Clear;
  inherited;
end;

end.
