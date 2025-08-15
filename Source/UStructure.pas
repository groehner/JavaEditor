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
    FDepth: Integer;
    FTopBottom: Integer;
    FTopBottomRight: Integer;
    FBottomTop: Integer;
    FGraphicRect: TRect;
    FTextRect: TRect;
    FFoldedTextRect: TRect;
    FSingleStatement: Boolean;
    FHidden: Boolean;
    function ToString: string; override;
    function Clone: TStructureEx;
    property GraphicRect: TRect read FGraphicRect write FGraphicRect;
    property TextRect: TRect read FTextRect write FTextRect;
    property FoldedTextRect: TRect read FFoldedTextRect write FFoldedTextRect;
    property Hidden: Boolean read FHidden write FHidden;
    property SingleStatement: Boolean read FSingleStatement write FSingleStatement;
    procedure DecTopBottom;
    procedure DecBottom;
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

function TStructureEx.ToString: string;
begin
  var Str:= 'L:' + IntToStr(FTextRect.Left) + ' T:' + IntToStr(FTextRect.Top) +
          ' R:' + IntToStr(FTextRect.Right) + ' B:' + IntToStr(FTextRect.Bottom) +
          ' De:' + IntToStr(FDepth);
  Result:= Str;
end;

function TStructureEx.Clone: TStructureEx;
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

procedure TStructureEx.DecTopBottom;
begin
  Dec(FFoldedTextRect.Top);
  Dec(FFoldedTextRect.Bottom);
end;

procedure TStructureEx.DecBottom;
begin
  Dec(FFoldedTextRect.Bottom);
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

function TStructures.ToString: string;
begin
  var Str:= '';
  for var Int := 0 to FList.Count-1 do begin
    var aStructure:= GetStructure(Int);
    if Assigned(aStructure)
      then Str:= Str + aStructure.ToString + #13#10
      else Str:= Str + '<structure index error>' + #13#10
  end;
  Result:= Str;
end;

function TStructures.Clone: TStructures;
begin
  Result:= TStructures.Create;
  for var Int:= 0 to Count - 1 do begin
    var aStructure:= getStructure(Int);
    if Assigned(aStructure) then
      Result.Add(aStructure.Clone);
  end;
end;

end.
