unit UJLabel;

interface

uses
  Classes, StdCtrls, UAComponents, UJComponents;

type

  TVertAlignment = (TOP, CENTER, BOTTOM);

  TJLabel = class (TSwingComponent)
  private
    FText: string;
    FVertAlignment: TVertAlignment;
    FHorzAlignment: THorzAlignment;
    FIcon: string;
    FDisabledIcon: string;
    FHorizontalTextPosition: THorzAlignment;
    FVerticalTextPosition: TVertAlignment;
    FIconTextGap: integer;
    FLabelFor: string;
    FDisplayedMnemonic: TShortCut;
    FDisplayedMnemonicIndex: integer;
    procedure setText(const aValue: string);
    procedure setHorzAlignment(aValue: THorzAlignment);
    procedure setVertAlignment(aValue: TVertAlignment);
    procedure setIcon(const aValue: string);
    procedure setDisabledIcon(const aValue: string);
    procedure setHorizontalTextPosition(aValue: THorzAlignment);
    procedure setVerticalTextPosition(aValue: TVertAlignment);
    procedure setIconTextGap(aValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aLabel: TLabel);
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure SizeToText; override;
    procedure NameFromText; override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property Text: string read FText write setText;
    property Icon: string read FIcon write setIcon;
    property DisabledIcon: string read FDisabledIcon write setDisabledIcon;
    property HorizontalAlignment: THorzAlignment read FHorzAlignment write setHorzAlignment;
    property VerticalAlignment: TVertAlignment read FVertAlignment write setVertAlignment;
    property HorizontalTextPosition: THorzAlignment read FHorizontalTextPosition write setHorizontalTextPosition;
    property VerticalTextPosition: TVertAlignment read FVerticalTextPosition write setVerticalTextPosition;
    property IconTextGap: integer read FIconTextGap write setIconTextGap;
    property LabelFor: string read FLabelFor write FLabelFor;
    property DisplayedMnemonic: TShortCut read FDisplayedMnemonic write FDisplayedMnemonic;
    property DisplayedMnemonicIndex: integer read FDisplayedMnemonicIndex write FDisplayedMnemonicIndex;
  end;

implementation

uses SysUtils, Math, Graphics, Controls, UITypes,
     Vcl.Imaging.GIFImg, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
     UUtils, UGuiDesigner;

{--- TJLabel ------------------------------------------------------------------}

constructor TJLabel.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +1;
  Width:= 80;
  Height:= 24;
  FText:= 'Text';
  HorizontalAlignment:= UAComponents.LEFT; // LEADING
  VerticalAlignment  := CENTER;
  HorizontalTextPosition:= UAComponents.RIGHT; // TRAILING;
  VerticalTextPosition:= CENTER;
  IconTextGap:= 4;
  Background:= clBtnFace;
  JavaType:= 'JLabel';
end;

constructor TJLabel.createFrom(aLabel: TLabel);
begin
  Create(aLabel.Owner);
  CreateFromJ(aLabel);
  Text:= aLabel.Caption;
  Font:= aLabel.Font;
  Foreground:= Font.Color;
  Background:= aLabel.Color;
  case aLabel.Alignment of
    taLeftJustify : FHorzAlignment:= UAComponents.LEFT;
    taRightJustify: FHorzAlignment:= UAComponents.RIGHT;
    taCenter      : FHorzAlignment:= UAComponents.CENTER;
  end;
end;

function TJLabel.getAttributes(ShowAttributes: integer): string;
  const
    Label1 = '|Text|Font|Icon|LabelFor';
    Label2 = '|DisabledIcon|HorizontalAlignment|VerticalAlignment|HorizontalTextPosition' +
             '|VerticalTextPosition|IconTextGap|DisplayedMnemonic|DisplayedMnemonicIndex';
begin
  if ShowAttributes = 1
    then Result:= Label1
    else Result:= Label1 + Label2;
  Result:= Result + inherited;
end;

procedure TJLabel.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalAlignment') or (Attr = 'VerticalAlignment') or
     (Attr = 'HorizontalTextPosition') or (Attr = 'VerticalTextPosition') then
    MakeAttribut(Attr, 'SwingConstants.' + Value)
  else if (Attr = 'Icon') or (Attr = 'DisabledIcon') then
    MakeIcon(Attr, Value, 'JLabel')
  else if Attr = 'LabelFor' then
    MakeAttribut('LabelFor', Value)
  else
    inherited;
end;

procedure TJLabel.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JLabel ' + Name + ' = new JLabel();');
  MakeAttribut('Text', asString(Text));
end;

procedure TJLabel.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'DisabledIcon' , NewName + 'DisabledIcon', true);
  Partner.ReplaceWord(OldName + 'Icon' , NewName + 'Icon', true);
end;

procedure TJLabel.SizeToText;
begin
  SizeToText(Text);
end;

procedure TJLabel.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('l' + Text);
end;

procedure TJLabel.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'DisabledIcon');
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'Icon');
end;

procedure TJLabel.Paint;
  var tx, ty, ix, iy, tw, th, itg, dx, dy, x1, x2, y1, y2: integer;
      s, pathname, ext: string;
      png: TPngImage;
      gif: TGifImage;
      jpg: TJPEGImage;
      bmp: TBitmap;
begin
  CanvasFontAssign;
  Canvas.Pen.Color:= Background;
  if Background = ColorNone
    then Canvas.Brush.Color:= (Parent as TWinControl).Brush.Color
    else Canvas.Brush.Color:= Background;
  s:= FText;
  Canvas.Font.Color:= Foreground;

  Canvas.Rectangle(Rect(0, 0, Width, Height));
  tw:= Canvas.TextWidth(s);
  if tw > Width  then begin
    while (tw > Width - 19) and (s <> '') do begin
      s:= UUtils.Left(s, Length(s)-1);
      tw:= Canvas.TextWidth(s);
    end;
    s:= s + '...';
  end;

  tw:= Canvas.TextWidth(s);
  tx:= 0;
  case FHorzAlignment of
    UAComponents.CENTER: tx:= (Width - tw) div 2;
    UAComponents.LEFT  : tx:= 0;
    UAComponents.RIGHT : tx:= Width - tw;
  end;
  th:= Canvas.TextHeight(s);
  ty:= 0;
  case FVertAlignment of
    CENTER: ty:= (Height-2 - th) div 2 + 2;
    UJLabel.TOP   : ty:= 1;
    BOTTOM: ty:= Height - th;
  end;

  pathname:= FGuiDesigner.getPath + 'images\' + copy(Icon, 8, length(Icon));

  if not FileExists(pathname) then begin
    Canvas.TextRect(Rect(0, 0, Width, Height), tx, ty, s);
    exit;
  end;

  ext:= Uppercase(ExtractFileExt(Icon));
  bmp:= TBitmap.Create;
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

  ix:= 0;
  case FHorzAlignment of
    UAComponents.CENTER: ix:= (Width - bmp.Width) div 2;
    UAComponents.LEFT  : ix:= 0;
    UAComponents.RIGHT : ix:= Width - bmp.Width;
  end;
  iy:= 0;
  case FVertAlignment of
    CENTER: iy:= (Height - bmp.Height) div 2;
    UJLabel.TOP   : iy:= 0;
    BOTTOM: iy:= Height - bmp.Height;
  end;
  if FText = '' then begin
    Canvas.Draw(ix, iy, bmp);
    FreeAndNil(bmp);
    exit;
  end;

  Canvas.Brush.Style:= bsClear;
  itg:= FIconTextGap;
  case FHorizontalTextPosition of
    UAComponents.CENTER: tx:= ix + (bmp.Width - tw) div 2;
    UAComponents.LEFT  : tx:= ix - itg - tw;
    UAComponents.RIGHT : tx:= ix + bmp.Width + itg;
  end;
  if tx < 0 then begin
    dx:= min(-tx, Width-ix-bmp.width);
    inc(tx, dx);
    inc(ix, dx);
  end else begin
    dx:= tx + tw - Width;
    dx:= min(dx, ix);
    if dx > 0 then begin
      dec(tx, dx);
      dec(ix, dx);
    end;
  end;

  case FVerticalTextPosition of
    CENTER: ty:= iy + (bmp.Height - th) div 2;
    UJLabel.TOP   :
      if FHorizontalTextPosition = UAComponents.CENTER then begin
        ty:= iy - itg - th;
        if ty < 0 then begin
          iy:= iy - ty;
          ty:= 0;
        end;
      end
        else ty:= iy;
    BOTTOM:
      if FHorizontalTextPosition = UAComponents.CENTER then begin
        ty:= iy + bmp.Height + itg;
        dy:= ty + th - Height;
        if dy > 0 then begin
          dec(iy, dy);
          dec(ty, dy);
        end;
      end
        else ty:= iy + bmp.Height - th;
  end;

  if FHorzAlignment = UAComponents.CENTER then begin
    x1:= min(tx, ix);
    x2:= max(tx + tw, ix + bmp.width);
    dx:= (Width - (x2 - x1)) div 2 - x1;
    inc(tx, dx);
    inc(ix, dx);
  end;
  if FVertAlignment = CENTER then begin
    y1:= min(ty, iy);
    y2:= max(ty + th, iy + bmp.Height);
    dy:= (Height - (y2 - y1)) div 2 - y1;
    inc(ty, dy);
    inc(iy, dy);
  end;
  
  Canvas.Draw(ix, iy, bmp);
  Canvas.TextRect(Rect(0, 0, Width, Height), tx, ty, FText);
  FreeAndNil(bmp);
end;

procedure TJLabel.setHorzAlignment(aValue: THorzAlignment);
begin
  if aValue <> FHorzAlignment then begin
    FHorzAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TJLabel.setVertAlignment(aValue: TVertAlignment);
begin
  if aValue <> FVertAlignment then begin
    FVertAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TJLabel.setIcon(const aValue: string);
begin
  if aValue <> FIcon then begin
    FIcon:= aValue;
    Invalidate;
  end;
end;

procedure TJLabel.setDisabledIcon(const aValue: string);
begin
  if aValue <> FDisabledIcon then begin
    FDisabledIcon:= aValue;
    Invalidate;
  end;
end;

procedure TJLabel.setHorizontalTextPosition(aValue: THorzAlignment);
begin
  if aValue <> FHorizontalTextPosition then begin
    FHorizontalTextPosition:= aValue;
    Invalidate;
  end;
end;

procedure TJLabel.setVerticalTextPosition(aValue: TVertAlignment);
begin
  if aValue <> FVerticalTextPosition then begin
    FVerticalTextPosition:= aValue;
    Invalidate;
  end;
end;

procedure TJLabel.setIconTextGap(aValue: integer);
begin
  if aValue <> FIconTextGap then begin
    FIconTextGap:= aValue;
    Invalidate;
  end;
end;

procedure TJLabel.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

end.
