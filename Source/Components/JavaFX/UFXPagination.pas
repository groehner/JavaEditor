unit UFXPagination;

interface

uses
  Classes,
  UFXComponents;

type

  TFXPagination = class(TFXControl)
  private
    FCurrentPageIndex: Integer;
    FMaxPageIndicatorCount: Integer;
    FPageCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
  published
    property CurrentPageIndex: Integer read FCurrentPageIndex
      write FCurrentPageIndex;
    property MaxPageIndicatorCount: Integer read FMaxPageIndicatorCount
      write FMaxPageIndicatorCount;
    property PageCount: Integer read FPageCount write FPageCount;
  end;

implementation

uses
  Graphics,
  UGUIDesigner;

constructor TFXPagination.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 143;
  PrefWidth := 120;
  PrefHeight := 80;
  Background := clWhite;
  FCurrentPageIndex := 0;
  FMaxPageIndicatorCount := 5;
  FPageCount := 10;
  JavaType := 'Pagination';
end;

procedure TFXPagination.Paint;
begin
  Canvas.Brush.Color := Background;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  // shows image only for dpi=96
  FGUIDesigner.vilPagination.Draw(Canvas, (Width - PPIScale(125)) div 2,
    Height - PPIScale(45), 0);
end;

procedure TFXPagination.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private Pagination ' + Name + ' = new Pagination();');
  MakeAttribut('PageCount', '10');
end;

function TFXPagination.GetAttributes(ShowAttributes: Integer): string;
const
  PaginationAttributes = '|CurrentPageIndex|MaxPageIndicatorCount|PageCount';
begin
  Result := PaginationAttributes + inherited GetAttributes(ShowAttributes);
end;

end.
