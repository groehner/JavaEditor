unit UFXLabeled;

interface

uses Classes, Graphics, UFXComponents;

type

  TContentDisplay = (_CD_BOTTOM, _CD_CENTER, _CD_GRAPHIC_ONLY, _CD_LEFT,
                     _CD_RIGHT, _CD_TEXT_ONLY, _CD_TOP);

  TFXLabeled = class (TFXControl)
  private
    FAlignment: TAlignment;
    FContentDisplay: TContentDisplay;
    FEllipsisString: string;
    FGraphic: string;
    FGraphicTextGap: integer;
    FLineSpacing: double;
    FMnemonicParsing: boolean;
    FTextAlignment: TTextAlignment;
    FTextFill: TColor;
    FUnderline: boolean;
    FWrapText: boolean;
    procedure setLineSpacing(aValue: double);
    procedure setUnderline(aValue: boolean);
    procedure setTextFill(aColor: TColor);
    procedure setGraphic(const aValue: string);
    procedure setGraphicTextGap(aValue: integer);
    procedure setTextAlignment(aValue: TTextAlignment);
    procedure setAlignment(aValue: TAlignment);
    procedure setContentDisplay(aValue: TContentDisplay);
  protected
    FText: string;
    TopSpace: integer;
    LeftSpace: integer;
    RightSpace: integer;
    BottomSpace: integer;
    procedure setText(const aValue: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure Paint; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    function getPicNr: integer; virtual;
  published
    property Alignment: TAlignment read FAlignment write setAlignment;
    property ContentDisplay: TContentDisplay read FContentDisplay write setContentDisplay;
    property EllipsisString: string read FEllipsisString write FEllipsisString;
    property LineSpacing: double read FLineSpacing write setLineSpacing;
    property Underline: boolean read FUnderline write setUnderline;
    property TextFill: TColor read FTextFill write setTextFill;
    property Graphic: string read FGraphic write setGraphic;
    property GraphicTextGap: integer read FGraphicTextGap write setGraphicTextGap;
    property MnemonicParsing: boolean read FMnemonicParsing write FMnemonicParsing;
    property Text: string read FText write setText;
    property TextAlignment: TTextAlignment read FTextAlignment write setTextAlignment;
    property WrapText: boolean read FWrapText write FWrapText;
  end;

implementation

uses SysUtils, Controls, GIFImg, jpeg, pngimage, Math,
     UGuiDesigner, UJEComponents, UUtils;

{--- TFXLabeled ---------------------------------------------------------------}

constructor TFXLabeled.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FAlignment:= CENTER_LEFT;
  FContentDisplay:= _CD_LEFT;
  FEllipsisString:= '...';
  FGraphicTextGap:= 4;
  FLineSpacing:= 0;
  FTextAlignment:= _TA_LEFT;
  FTextFill:= $333333;
  LeftSpace:= 0;
  RightSpace:= 0;
  TopSpace:= 0;
  BottomSpace:= 0;
  Background:= clBtnFace;
end;

procedure TFXLabeled.Paint;
  var tx, ty, tw, th, x, y, w, h, gw, gh, gx, gy, maxw, maxh, mgw, mtw, gtg, bx, by: integer;
      s, pathname, ext: string;
      png: TPngImage;
      gif: TGifImage;
      jpg: TJPEGImage;
      bmp: Graphics.TBitmap;

  function Shorten(s: string; w: integer): string;
    var e: integer;
  begin
    e:= Canvas.TextWidth(EllipsisString);
    tw:= Canvas.TextWidth(s);
    if tw > w then begin
      while (tw > w - e) and (s <> '') do begin
        s:= UUtils.Left(s, Length(s)-1);
        tw:= Canvas.TextWidth(s);
      end;
      s:= s + EllipsisString;
      tw:= tw + e;
    end;
    Result:= s;
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color:= FTextFill;
  h:= Height - TopSpace - BottomSpace;
  w:= Width - LeftSpace - RightSpace;
  tw:= Canvas.TextWidth(Text);
  th:= Canvas.TextHeight('Hg');
  s:= Text;

  pathname:= FGuiDesigner.getPath + 'images\' + copy(Graphic, 8, length(Graphic));
  if (ContentDisplay = _CD_TEXT_ONLY) or (Graphic = '') or not FileExists(pathname) then begin
    // without graphic
    s:= Shorten(s, w);
    case Alignment of
      TOP_LEFT:      begin x:= 0; y:= 1; end;
      TOP_CENTER:    begin x:= (w - tw) div 2; y:= 1 end;
      TOP_RIGHT:     begin x:= w - 5 - tw; y:= 1 end;
      BASELINE_LEFT,
      CENTER_LEFT:   begin x:= 0; y:= (h - th) div 2; end;
      BASELINE_CENTER,
      CENTER:        begin x:= (w - tw) div 2; y:= (h - th) div 2; end;
      BASELINE_RIGHT,
      CENTER_RIGHT:  begin x:= w - 5 - tw; y:= (h - th) div 2; end;
      BOTTOM_LEFT:   begin x:= 0; y:= h - 1 - th end;
      BOTTOM_CENTER: begin x:= (w - tw) div 2; y:= h - 1 - th end;
      BOTTOM_RIGHT:  begin x:= w - 5 - tw; y:= h - 1 - th; end;
      else           begin x:= 0; y:= 0; end;
    end;
    Canvas.Font.Color:= TextFill;
    Canvas.TextOut(LeftSpace + x, TopSpace + y, s);
  end else begin
    // with graphic
    ext:= Uppercase(ExtractFileExt(Graphic));
    bmp:= Graphics.TBitmap.Create;
    if ext = '.PNG' then begin
      png:= TPngImage.Create;
      png.LoadFromFile(pathname);
      bmp.Assign(png);
      FreeAndNil(png);
    end else if ext = '.GIF' then begin
      gif:= TGifImage.Create;
      gif.LoadFromFile(Pathname);
      bmp.Assign(gif.Bitmap);
      FreeAndNil(gif);
    end else if (ext = '.JPG') or (ext = 'JPEG') then begin
      jpg:= TJPEGImage.Create;
      jpg.LoadFromFile(Pathname);
      bmp.Assign(jpg);
      FreeAndNil(jpg);
    end;

    gw:= bmp.Width;
    gh:= bmp.Height;

    if ContentDisplay = _CD_GRAPHIC_ONLY then begin
      s:= '';
      tw:= 0;
      gtg:= 0;
    end else
      gtg:= GraphicTextGap;

    case ContentDisplay of
      _CD_TOP, _CD_BOTTOM: begin
        s:= Shorten(s, w);
        maxw:= max(tw, gw);
        maxh:= gh + gtg + th;
      end;
      _CD_GRAPHIC_ONLY: begin
        maxw:= gw;
        maxh:= gh;
      end;
      _CD_LEFT, _CD_RIGHT: begin
        s:= Shorten(s, w - gtg - gw);
        maxw:= gw + gtg + tw;
        maxh:= max(th, gh);
      end;
      _CD_CENTER: begin
        s:= Shorten(s, w);
        maxw:= max(gw, tw);
        maxh:= max(th, gh);
      end;
      else begin
        maxw:= 0;
        maxh:= 0;
      end;
    end;
    if maxw > w then begin
      width:= LeftSpace + maxw;
      w:= maxw;
    end;

    // bx, by = position of compound box of graphic and text
    // maxw, maxh = size of compound box
    case Alignment of
      TOP_LEFT: begin
        bx:= 0;
        by:= 0;
      end;
      TOP_CENTER: begin
        bx:= (w - maxw) div 2;
        by:= 0;
      end;
      TOP_RIGHT: begin
        bx:= w - maxw;
        by:= 0;
      end;
      CENTER_LEFT,
      BASELINE_LEFT: begin
        bx:= 0;
        by:= (h - maxh) div 2;
      end;
      CENTER,
      BASELINE_CENTER: begin
        bx:= (w - maxw) div 2;
        by:= (h - maxh) div 2;
      end;
      CENTER_RIGHT,
      BASELINE_RIGHT: begin
        bx:= w - maxw;
        by:= (h - maxh) div 2;
      end;
      BOTTOM_LEFT: begin
        bx:= 0;
        by:= h - maxh;
      end;
      BOTTOM_CENTER: begin
        bx:= (w - maxw) div 2;
        by:= h - maxh;
      end;
      BOTTOM_RIGHT: begin
        bx:= w - maxw;
        by:= h - maxh;
      end;
      else begin
        bx:= 0;
        by:= 0;
      end;
    end;

    mgw:= (maxw - gw) div 2;
    mtw:= (maxw - tw) div 2;    

    case ContentDisplay of
      _CD_TOP: begin
        gx:= mgw;
        gy:= 0;
        tx:= mtw;
        ty:= gh + gtg;
      end;
      _CD_BOTTOM: begin
        gx:= mgw;
        gy:= maxh - gh;
        tx:= mtw;
        ty:= 0;
      end;
      _CD_LEFT: begin
        gx:= 0;
        gy:= 0;
        tx:= gw + gtg;
        ty:= (maxh - th) div 2;
      end;
      _CD_RIGHT: begin
        gx:= maxw - gw;
        gy:= 0;
        tx:= 0;
        ty:= (maxh - th) div 2;
      end;
      _CD_CENTER: begin
        gx:= mgw;
        gy:= 0;
        tx:= mtw;
        ty:=  (maxh - th) div 2;
      end;
      else begin
        gx:= 0;
        gy:= 0;
        tx:= 0;
        ty:= 0;
      end;
    end;

    gx:= bx + gx;
    gy:= by + gy;
    tx:= bx + tx;
    ty:= by + ty;


    if Underline then Canvas.Font.Style:= [fsUnderline];
    if ContentDisplay <> _CD_GRAPHIC_ONLY then
      Canvas.TextOut(LeftSpace + tx, TopSpace + ty, s);
    Canvas.Font.Style:= [];
    Canvas.Draw(LeftSpace + gx, TopSpace + gy, bmp);
    FreeAndNil(bmp);
  end;
end;

function TFXLabeled.getPicNr: integer;
begin
  Result:= 0;
end;

procedure TFXLabeled.setLineSpacing(aValue: double);
begin
  if aValue <> FLineSpacing then begin
    FLineSpacing:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.setUnderline(aValue: boolean);
begin
  if aValue <> FUnderline then begin
    FUnderline:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.setTextFill(aColor: TColor);
begin
  if aColor <> FTextFill then begin
    FTextFill:= aColor;
    Invalidate;
  end;
end;

procedure TFXLabeled.setGraphicTextGap(aValue: integer);
begin
  if aValue <> FGraphicTextGap then begin
    FGraphicTextGap:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.setGraphic(const aValue: string);
begin
  if aValue <> FGraphic then begin
    FGraphic:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.setTextAlignment(aValue: TTextAlignment);
begin
  if aValue <> FTextAlignment then begin
    FTextAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.setAlignment(aValue: TAlignment);
begin
  if aValue <> FAlignment then begin
    FAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.setContentDisplay(aValue: TContentDisplay);
begin
  if aValue <> FContentDisplay then begin
    FContentDisplay:= aValue;
    Invalidate;
  end;
end;

procedure TFXLabeled.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'ContentDisplay' then begin
    InsertImport('javafx.scene.control.*');
    MakeAttribut(Attr, 'ContentDisplay.' + Value);
  end else if Attr = 'TextAlignment' then begin
    InsertImport('javafx.scene.text.*');
    MakeAttribut(Attr, 'TextAlignment.' + Value);
  end else
    inherited;
end;

function TFXLabeled.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|ContentDisplay|Graphic|Text|Font';
        Attributes2 =  Attributes1 +
                      '|Alignment|LineSpacing|Underline|TextFill|EllipsisString' +
                      '|GraphicTextGap|MnemonicParsing|TextAlignment|WrapText';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes);
end;

procedure TFXLabeled.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'Graphic' , NewName + 'Graphic', true);
end;

procedure TFXLabeled.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private Image ' + Name + 'Graphic');
end;

end.
