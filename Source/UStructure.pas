unit UStructure;

interface

uses Classes, Types;

const EndStructureWidth = 10;

type

(*
This could be optimized by making the structure a record instead of a
full fledged class. I opted for the class because I was thinking I may
add some additional features later on that would be more maintainable
with the structure as a class element. Some optimization of class versus
record was achieved by pooling the structure classes instead of creating
new objects each time.

TextRect is used to hold the start and end text positions where the structure
line should be drawn. GraphicRect is where the position should be drawn in
graphical coordinates. TextRect is only updated if the a change is made to the text
and is updated in Build Structure. GraphicRect is updated on each paint operation
since we always need correct graphical coordinates to determine how lines
should be invalidated. For example, when InvalidateRect is called the invalidation
rectangle may need to be increased so the entire structure line can be repainted.
*)


TStructureEx = class
  public
    FDepth: integer;
    FTopBottom: integer;
    FTopBottomRight: integer;
    FBottomTop: integer;
    FGraphicRect: TRect;
    FTextRect: TRect;
    FFoldedTextRect: TRect;
    FSingleStatement: boolean;
    FHidden: boolean;
    function ToString: string; override;
    function clone: TStructureEx;
    property GraphicRect: TRect read FGraphicRect write FGraphicRect;
    property TextRect: TRect read FTextRect write FTextRect;
    property FoldedTextRect: TRect read FFoldedTextRect write FFoldedTextRect;
    property Hidden: boolean read FHidden write FHidden;
    property SingleStatement: boolean read FSingleStatement write FSingleStatement;
    procedure decTopBottom;
    procedure decBottom;
end;

TStructures = class
  private
    FList: TList;
    FPooled: TList;
    FMaxDepth: Integer;
    function GetCount: Integer;
    function GetStructure(Index: Integer): TStructureEx;
  public
    constructor Create;
    destructor Destroy; override;
    function NewStructure: TStructureEx;
    procedure Add(Value: TStructureEx);
    procedure Delete(Index: Integer);
    procedure Clear;
    function ToString: string; override;
    function Clone: TStructures;
    property Count: Integer read GetCount;
    property Structure[Index: Integer]: TStructureEx read GetStructure; default;
    property MaxDepth: Integer read FMaxDepth write FMaxDepth;
end;

implementation

uses SysUtils;

function TStructureEx.toString: string;
begin
  var s:= 'L:' + IntToStr(FTextRect.Left) + ' T:' + IntToStr(FTextRect.Top) +
          ' R:' + IntTostr(FTextRect.Right) + ' B:' + IntTostr(FTextRect.Bottom) +
          ' De:' + IntTostr(FDepth);
  Result:= s;
end;

function TStructureEx.clone: TStructureEx;
begin
  Result:= TStructureEx.Create;
  Result.FDepth:= FDepth;
  Result.FTopBottom:= FTopBottom;
  Result.FTopBottomRight:= FTopBottomRight;
  Result.FBottomTop:= FBottomTop;
  Result.FGraphicRect:= FGraphicRect;
  Result.FTextRect:= FTextRect;
  Result.FSingleStatement:= FSingleStatement;
  Result.FoldedTextRect:= FFoldedTextRect;
  Result.Hidden:= FHidden;
end;

procedure TStructureEx.decTopBottom;
begin
  dec(FFoldedTextRect.Top);
  dec(FFoldedTextRect.Bottom);
end;

procedure TStructureEx.decBottom;
begin
  dec(FFoldedTextRect.Bottom);
end;

{ TStructures }
constructor TStructures.Create;
begin
  FList  := TList.Create;
  FPooled:= TList.Create;
end;

destructor TStructures.Destroy;
  var struct: TStructureEx;
begin
  Clear;
  while FList.Count > 0 do begin
    struct:= TStructureEx(FList[0]);
    FreeAndNil(struct);
    FList.Delete(0);
  end;
  FreeAndNil(FList);
  while FPooled.Count > 0 do begin
    struct:= TStructureEx(FPooled[0]);
    FreeAndNil(struct);
    FPooled.Delete(0);
  end;
  FreeAndNil(FPooled);
end;

procedure TStructures.Delete(Index: Integer);
begin
  FPooled.Add(Structure[Index]);
  FList.Delete(Index);
end;

procedure TStructures.Add(Value: TStructureEx);
begin
  FList.Add(Value);
end;

function TStructures.GetCount: Integer;
begin
  Result:= FList.Count
end;

function TStructures.GetStructure(Index: Integer): TStructureEx;
begin
  if (0 <= Index) and (Index < FList.Count)
    then Result:= TStructureEx(FList[Index])
    else Result:= nil;
end;

procedure TStructures.Clear;
begin
  while FList.Count > 0 do
    Delete(0);
end;

function TStructures.NewStructure: TStructureEx;
begin
  if FPooled.Count > 0 then begin
    Result:= TStructureEx(FPooled[FPooled.Count-1]);
    FPooled.Delete(FPooled.Count-1);
  end
  else
    Result:= TStructureEx.Create;
end;

function TStructures.toString: string;
begin
  var s:= '';
  for var i := 0 to FList.Count-1 do begin
    var aStructure:= GetStructure(i);
    if assigned(aStructure)
      then s:= s + aStructure.toString + #13#10
      else s:= s + '<structure index error>' + #13#10
  end;
  Result:= s;
end;

function TStructures.Clone: TStructures;
begin
  Result:= TStructures.Create;
  for var i:= 0 to Count - 1 do begin
    var aStructure:= getStructure(i);
    if assigned(aStructure) then
      Result.Add(aStructure.clone);
  end;
end;

end.
