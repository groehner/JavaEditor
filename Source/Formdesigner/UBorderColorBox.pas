unit UBorderColorBox;

interface

uses Windows, Classes, Controls, Graphics, StdCtrls;

const
  NoColorSelected = TColor($FF000000);
  SColorBoxCustomCaption = 'Custom...';

type

  TBorderColorBox = class(TComboBox)
  private
    FListSelected: Boolean;
    FSelectedColor: TColor;
    function GetSelected: TColor;
    procedure SetSelected(const AColor: TColor);
    function GetColor(Index: Integer): TColor;
  protected
    procedure CloseUp; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function PickCustomColor: Boolean; virtual;
    procedure Select; override;
  public
    constructor Create3(AOwner: TComponent; AParent: TWinControl;
      aLeft, aTop: Integer);
    procedure PopulateList;
    property Colors[Index: Integer]: TColor read GetColor;
    property Selected: TColor read GetSelected write SetSelected
      default clBlack;
  end;

implementation

uses SysUtils, Dialogs, UITypes, ULink;

{ TBorderColorBox }

constructor TBorderColorBox.Create3(AOwner: TComponent; AParent: TWinControl;
  aLeft, aTop: Integer);
begin
  inherited Create(AOwner);
  inherited Style := csOwnerDrawFixed;
  FSelectedColor := clBlack;
  Parent := AParent;
  Width := 170;
  Height := 22;
  Top := aTop;
  Left := aLeft;
  PopulateList;
end;

procedure TBorderColorBox.CloseUp;
begin
  inherited CloseUp;
  FListSelected := True;
end;

procedure TBorderColorBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red, Green, Blue, Alpha: Byte;
    end;

  begin
    if (TColorQuad(AColor).Red > 192) or (TColorQuad(AColor).Green > 192) or
      (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if odSelected in State then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  LRect: TRect;
  LBackground: TColor;
begin
  with Canvas do
  begin
    FillRect(Rect);
    LBackground := Brush.Color;
    LRect := Rect;
    LRect.Right := LRect.Bottom - LRect.Top + LRect.Left;
    InflateRect(LRect, -1, -1);
    Brush.Color := Colors[Index];
    FillRect(LRect);
    Brush.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    FrameRect(LRect);
    Brush.Color := LBackground;
    Rect.Left := LRect.Right + 5;
    TextRect(Rect, Rect.Left, Rect.Top + (Rect.Bottom - Rect.Top -
      TextHeight(Items[Index])) div 2, Items[Index]);
  end;
end;

function TBorderColorBox.GetSelected: TColor;
begin
  if HandleAllocated then
    if ItemIndex <> -1 then
      Result := Colors[ItemIndex]
    else
      Result := NoColorSelected
  else
    Result := FSelectedColor;
end;

procedure TBorderColorBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FListSelected := False;
  inherited KeyDown(Key, Shift);
end;

procedure TBorderColorBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = #13) and (ItemIndex = 0) then
  begin
    { If the user picked a custom color, force a select event to happen
      so the user can handle it }
    if PickCustomColor and Assigned(OnSelect) then
      OnSelect(Self);
    Key := #0;
  end;
end;

function TBorderColorBox.PickCustomColor: Boolean;
var
  LColor: TColor;
begin
  with TColorDialog.Create(nil) do
    try
      LColor := ColorToRGB(TColor(Items.Objects[0]));
      Color := LColor;
      CustomColors.Text := Format('ColorA=%.8x', [LColor]);
      Result := Execute;
      if Result then
      begin
        Items.Objects[0] := TObject(Color);
        Self.Invalidate;
      end;
    finally
      Free;
    end;
end;

procedure TBorderColorBox.PopulateList;
var
  LColor: TColor;
  SL: TStringList;
  Str: string;
begin
  Items.BeginUpdate;
  Items.Clear;
  SL := TStringList.Create;
  try
    SL.Text := JavaColorsText;
    for var I := 1 to SL.Count - 1 do
    begin
      Str := Java2DelphiColors(SL.Strings[I]);
      LColor := StringToColor(Str);
      Items.AddObject(SL.Strings[I], TObject(LColor));
    end;
    Items.InsertObject(0, SColorBoxCustomCaption, TObject(clBlack));
  finally
    Items.EndUpdate;
    FreeAndNil(SL);
  end;
end;

procedure TBorderColorBox.Select;
begin
  if FListSelected then
  begin
    FListSelected := False;
    if (ItemIndex = 0) and not PickCustomColor then
      Exit;
  end;
  inherited Select;
end;

procedure TBorderColorBox.SetSelected(const AColor: TColor);
var
  Int, Index: Integer;
begin
  if HandleAllocated then
  begin
    Int := Items.IndexOfObject(TObject(AColor));
    if (Int = -1) and (AColor <> NoColorSelected) then
    begin
      Items.Objects[0] := TObject(AColor);
      Int := 0;
    end
    else if (Int = 0) then
    begin
      { Look for the color anywhere else but the first color before
        defaulting to selecting the "custom color". }
      for Index := 1 to Items.Count - 1 do
      begin
        if Items.Objects[Index] = TObject(AColor) then
        begin
          Int := Index;
          Break;
        end;
      end;
    end;
    if (ItemIndex = 0) and (Int = 0) then
      Invalidate { Refresh the color shown }
    else
      ItemIndex := Int;
  end;
  FSelectedColor := AColor;
end;

function TBorderColorBox.GetColor(Index: Integer): TColor;
begin
  Result := TColor(Items.Objects[Index]);
end;

end.
