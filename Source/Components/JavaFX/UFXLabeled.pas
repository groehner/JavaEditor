unit UFXLabeled;

interface

uses
  Classes,
  Graphics,
  UFXComponents;

type

  TContentDisplay = (_CD_BOTTOM, _CD_CENTER, _CD_GRAPHIC_ONLY, _CD_LEFT,
    _CD_RIGHT, _CD_TEXT_ONLY, _CD_TOP);

  TFXLabeled = class(TFXControl)
  private
    FAlignment: TAlignment;
    FContentDisplay: TContentDisplay;
    FEllipsisString: string;
    FGraphic: string;
    FGraphicTextGap: Integer;
    FLineSpacing: Double;
    FMnemonicParsing: Boolean;
    FTextAlignment: TTextAlignment;
    FTextFill: TColor;
    FUnderline: Boolean;
    FWrapText: Boolean;
    procedure SetLineSpacing(AValue: Double);
    procedure SetUnderline(AValue: Boolean);
    procedure SetTextFill(AColor: TColor);
    procedure SetGraphic(const AValue: string);
    procedure SetGraphicTextGap(AValue: Integer);
    procedure SetTextAlignment(AValue: TTextAlignment);
    procedure SetAlignment(AValue: TAlignment);
    procedure SetContentDisplay(AValue: TContentDisplay);
  protected
    FText: string;
    FTopSpace: Integer;
    FLeftSpace: Integer;
    FRightSpace: Integer;
    FBottomSpace: Integer;
    procedure SetText(const AValue: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure Paint; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    function GetPicNr: Integer; virtual;
    procedure MakeFont; override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property ContentDisplay: TContentDisplay read FContentDisplay
      write SetContentDisplay;
    property EllipsisString: string read FEllipsisString write FEllipsisString;
    property LineSpacing: Double read FLineSpacing write SetLineSpacing;
    property Underline: Boolean read FUnderline write SetUnderline;
    property TextFill: TColor read FTextFill write SetTextFill;
    property Graphic: string read FGraphic write SetGraphic;
    property GraphicTextGap: Integer read FGraphicTextGap
      write SetGraphicTextGap;
    property MnemonicParsing: Boolean read FMnemonicParsing
      write FMnemonicParsing;
    property Text: string read FText write SetText;
    property TextAlignment: TTextAlignment read FTextAlignment
      write SetTextAlignment;
    property WrapText: Boolean read FWrapText write FWrapText;
  end;

implementation

uses
  SysUtils,
  Controls,
  GIFImg,
  jpeg,
  pngimage,
  Math,
  UGUIDesigner,
  UJEComponents,
  UUtils;

{ --- TFXLabeled --------------------------------------------------------------- }

constructor TFXLabeled.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := CENTER_LEFT;
  FContentDisplay := _CD_LEFT;
  FEllipsisString := '...';
  FGraphicTextGap := 4;
  FLineSpacing := 0;
  FTextAlignment := _TA_LEFT;
  FTextFill := $333333;
  FLeftSpace := 0;
  FRightSpace := 0;
  FTopSpace := 0;
  FBottomSpace := 0;
  Background := clBtnFace;
end;

procedure TFXLabeled.Paint;
var
  TextX, TextY, TextWidth, TextHeight, XPos, YPos, WPos, HPos, GeW, GeH, GeX,
    GeY, MaxW, MaxH, Mgw, Mtw, Gtg, BeX, BeY: Integer;
  Str, Pathname, Ext: string;
  Png: TPngImage;
  Gif: TGIFImage;
  Jpg: TJPEGImage;
  Bmp: Graphics.TBitmap;

  function Shorten(Str: string; Width: Integer): string;
  var
    Ellip: Integer;
  begin
    Ellip := Canvas.TextWidth(EllipsisString);
    TextWidth := Canvas.TextWidth(Str);
    if TextWidth > Width then
    begin
      while (TextWidth > Width - Ellip) and (Str <> '') do
      begin
        Str := UUtils.Left(Str, Length(Str) - 1);
        TextWidth := Canvas.TextWidth(Str);
      end;
      Str := Str + EllipsisString;
      TextWidth := TextWidth + Ellip;
    end;
    Result := Str;
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color := FTextFill;
  HPos := Height - FTopSpace - FBottomSpace;
  WPos := Width - FLeftSpace - FRightSpace;
  TextWidth := Canvas.TextWidth(Text);
  TextHeight := Canvas.TextHeight('Hg');
  Str := Text;

  Pathname := FGUIDesigner.GetPath + 'images\' +
    Copy(Graphic, 8, Length(Graphic));
  if (ContentDisplay = _CD_TEXT_ONLY) or (Graphic = '') or
    not FileExists(Pathname) then
  begin
    // without graphic
    Str := Shorten(Str, WPos);
    case Alignment of
      TOP_LEFT:
        begin
          XPos := 0;
          YPos := 1;
        end;
      TOP_CENTER:
        begin
          XPos := (WPos - TextWidth) div 2;
          YPos := 1;
        end;
      TOP_RIGHT:
        begin
          XPos := WPos - 5 - TextWidth;
          YPos := 1;
        end;
      BASELINE_LEFT, CENTER_LEFT:
        begin
          XPos := 0;
          YPos := (HPos - TextHeight) div 2;
        end;
      BASELINE_CENTER, CENTER:
        begin
          XPos := (WPos - TextWidth) div 2;
          YPos := (HPos - TextHeight) div 2;
        end;
      BASELINE_RIGHT, CENTER_RIGHT:
        begin
          XPos := WPos - 5 - TextWidth;
          YPos := (HPos - TextHeight) div 2;
        end;
      BOTTOM_LEFT:
        begin
          XPos := 0;
          YPos := HPos - 1 - TextHeight;
        end;
      BOTTOM_CENTER:
        begin
          XPos := (WPos - TextWidth) div 2;
          YPos := HPos - 1 - TextHeight;
        end;
      BOTTOM_RIGHT:
        begin
          XPos := WPos - 5 - TextWidth;
          YPos := HPos - 1 - TextHeight;
        end;
    else
      begin
        XPos := 0;
        YPos := 0;
      end;
    end;
    Canvas.Font.Color := TextFill;
    Canvas.TextOut(FLeftSpace + XPos, FTopSpace + YPos, Str);
  end
  else
  begin
    // with graphic
    Ext := UpperCase(ExtractFileExt(Graphic));
    Bmp := Graphics.TBitmap.Create;
    if Ext = '.Png' then
    begin
      Png := TPngImage.Create;
      Png.LoadFromFile(Pathname);
      Bmp.Assign(Png);
      FreeAndNil(Png);
    end
    else if Ext = '.Gif' then
    begin
      Gif := TGIFImage.Create;
      Gif.LoadFromFile(Pathname);
      Bmp.Assign(Gif.Bitmap);
      FreeAndNil(Gif);
    end
    else if (Ext = '.Jpg') or (Ext = 'JPEG') then
    begin
      Jpg := TJPEGImage.Create;
      Jpg.LoadFromFile(Pathname);
      Bmp.Assign(Jpg);
      FreeAndNil(Jpg);
    end;

    GeW := Bmp.Width;
    GeH := Bmp.Height;

    if ContentDisplay = _CD_GRAPHIC_ONLY then
    begin
      Str := '';
      TextWidth := 0;
      Gtg := 0;
    end
    else
      Gtg := GraphicTextGap;

    case ContentDisplay of
      _CD_TOP, _CD_BOTTOM:
        begin
          Str := Shorten(Str, WPos);
          MaxW := Max(TextWidth, GeW);
          MaxH := GeH + Gtg + TextHeight;
        end;
      _CD_GRAPHIC_ONLY:
        begin
          MaxW := GeW;
          MaxH := GeH;
        end;
      _CD_LEFT, _CD_RIGHT:
        begin
          Str := Shorten(Str, WPos - Gtg - GeW);
          MaxW := GeW + Gtg + TextWidth;
          MaxH := Max(TextHeight, GeH);
        end;
      _CD_CENTER:
        begin
          Str := Shorten(Str, WPos);
          MaxW := Max(GeW, TextWidth);
          MaxH := Max(TextHeight, GeH);
        end;
    else
      begin
        MaxW := 0;
        MaxH := 0;
      end;
    end;
    if MaxW > WPos then
    begin
      Width := FLeftSpace + MaxW;
      WPos := MaxW;
    end;

    // BeX, BeY = position of compound box of graphic and text
    // MaxW, MaxH = size of compound box
    case Alignment of
      TOP_LEFT:
        begin
          BeX := 0;
          BeY := 0;
        end;
      TOP_CENTER:
        begin
          BeX := (WPos - MaxW) div 2;
          BeY := 0;
        end;
      TOP_RIGHT:
        begin
          BeX := WPos - MaxW;
          BeY := 0;
        end;
      CENTER_LEFT, BASELINE_LEFT:
        begin
          BeX := 0;
          BeY := (HPos - MaxH) div 2;
        end;
      CENTER, BASELINE_CENTER:
        begin
          BeX := (WPos - MaxW) div 2;
          BeY := (HPos - MaxH) div 2;
        end;
      CENTER_RIGHT, BASELINE_RIGHT:
        begin
          BeX := WPos - MaxW;
          BeY := (HPos - MaxH) div 2;
        end;
      BOTTOM_LEFT:
        begin
          BeX := 0;
          BeY := HPos - MaxH;
        end;
      BOTTOM_CENTER:
        begin
          BeX := (WPos - MaxW) div 2;
          BeY := HPos - MaxH;
        end;
      BOTTOM_RIGHT:
        begin
          BeX := WPos - MaxW;
          BeY := HPos - MaxH;
        end;
    else
      begin
        BeX := 0;
        BeY := 0;
      end;
    end;

    Mgw := (MaxW - GeW) div 2;
    Mtw := (MaxW - TextWidth) div 2;

    case ContentDisplay of
      _CD_TOP:
        begin
          GeX := Mgw;
          GeY := 0;
          TextX := Mtw;
          TextY := GeH + Gtg;
        end;
      _CD_BOTTOM:
        begin
          GeX := Mgw;
          GeY := MaxH - GeH;
          TextX := Mtw;
          TextY := 0;
        end;
      _CD_LEFT:
        begin
          GeX := 0;
          GeY := 0;
          TextX := GeW + Gtg;
          TextY := (MaxH - TextHeight) div 2;
        end;
      _CD_RIGHT:
        begin
          GeX := MaxW - GeW;
          GeY := 0;
          TextX := 0;
          TextY := (MaxH - TextHeight) div 2;
        end;
      _CD_CENTER:
        begin
          GeX := Mgw;
          GeY := 0;
          TextX := Mtw;
          TextY := (MaxH - TextHeight) div 2;
        end;
    else
      begin
        GeX := 0;
        GeY := 0;
        TextX := 0;
        TextY := 0;
      end;
    end;

    GeX := BeX + GeX;
    GeY := BeY + GeY;
    TextX := BeX + TextX;
    TextY := BeY + TextY;

    if Underline then
      Canvas.Font.Style := [fsUnderline];
    if ContentDisplay <> _CD_GRAPHIC_ONLY then
      Canvas.TextOut(FLeftSpace + TextX, FTopSpace + TextY, Str);
    Canvas.Font.Style := [];
    Canvas.Draw(FLeftSpace + GeX, FTopSpace + GeY, Bmp);
    FreeAndNil(Bmp);
  end;
end;

function TFXLabeled.GetPicNr: Integer;
begin
  Result := 0;
end;

procedure TFXLabeled.MakeFont;
begin
  DoMakeFont;
end;

procedure TFXLabeled.SetLineSpacing(AValue: Double);
begin
  if AValue <> FLineSpacing then
  begin
    FLineSpacing := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetUnderline(AValue: Boolean);
begin
  if AValue <> FUnderline then
  begin
    FUnderline := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetTextFill(AColor: TColor);
begin
  if AColor <> FTextFill then
  begin
    FTextFill := AColor;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetGraphicTextGap(AValue: Integer);
begin
  if AValue <> FGraphicTextGap then
  begin
    FGraphicTextGap := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetGraphic(const AValue: string);
begin
  if AValue <> FGraphic then
  begin
    FGraphic := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetTextAlignment(AValue: TTextAlignment);
begin
  if AValue <> FTextAlignment then
  begin
    FTextAlignment := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetAlignment(AValue: TAlignment);
begin
  if AValue <> FAlignment then
  begin
    FAlignment := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetContentDisplay(AValue: TContentDisplay);
begin
  if AValue <> FContentDisplay then
  begin
    FContentDisplay := AValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'ContentDisplay' then
  begin
    InsertImport('javafx.scene.control.*');
    MakeAttribut(Attr, 'ContentDisplay.' + Value);
  end
  else if Attr = 'TextAlignment' then
  begin
    InsertImport('javafx.scene.text.*');
    MakeAttribut(Attr, 'TextAlignment.' + Value);
  end
  else
    inherited;
end;

function TFXLabeled.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|ContentDisplay|Graphic|Text|Font';
  Attributes2 = Attributes1 +
    '|Alignment|LineSpacing|Underline|TextFill|EllipsisString' +
    '|GraphicTextGap|MnemonicParsing|TextAlignment|WrapText';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
end;

procedure TFXLabeled.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'Graphic', NewName + 'Graphic', True);
end;

procedure TFXLabeled.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

end.
