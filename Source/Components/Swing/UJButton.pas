unit UJButton;

{ Classes
  TAbstractButton = class (TSwingComponent)
    TJButton
    TJToggleButton
}

interface

uses
  Classes, StdCtrls, UAComponents, UJComponents, UJLabel;

type

  TAbstractButton = class (TSwingComponent)
  private
    FText: string;
    FMnemonic: TShortCut;
    FDisplayedMnemonicIndex: integer;
    FSelected: boolean;
    FBorderPainted: boolean;
    FFocusPainted: boolean;
    FContentAreaFilled: boolean;

    FVertAlignment: TVertAlignment;
    FHorzAlignment: THorzAlignment;
    FIcon: string;
    FDisabledIcon: string;

    FPressedIcon: string;
    FSelectedIcon: string;
    FDisabledSelectedIcon: string;
    FRolloverIcon: string;
    FRolloverSelectedIcon: string;

    FRolloverEnabled: boolean;
    FHorizontalTextPosition: THorzAlignment;
    FVerticalTextPosition: TVertAlignment;
    FIconTextGap: integer;

    procedure setText(const aValue: string);
    procedure setBorderPainted(aValue: boolean);
    procedure setContentAreaFilled(aValue: boolean);
    procedure setHorzAlignment(aValue: THorzAlignment);
    procedure setVertAlignment(aValue: TVertAlignment);
    procedure setIcon(const aValue: string);

    procedure setHorizontalTextPosition(aValue: THorzAlignment);
    procedure setVerticalTextPosition(aValue: TVertAlignment);
    procedure setIconTextGap(aValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SizeToText; override;
    procedure Paint; override;
    procedure DeleteComponent; override;
  published
    property Text: string read FText write setText;
    property Mnemonic: TShortCut read FMnemonic write FMnemonic;
    property DisplayedMnemonicIndex: integer read FDisplayedMnemonicIndex write FDisplayedMnemonicIndex;
    property Selected: boolean read FSelected write FSelected;
    property BorderPainted: boolean read FBorderPainted write setBorderPainted;
    property FocusPainted: boolean read FFocusPainted write FFocusPainted;

    property ContentAreaFilled: boolean read FContentAreaFilled write setContentAreaFilled;

    property Icon: string read FIcon write setIcon;
    property DisabledIcon: string read FDisabledIcon write FDisabledIcon;
    property PressedIcon: string read FPressedIcon write FPressedIcon;
    property SelectedIcon: string read FSelectedIcon write FSelectedIcon;
    property DisabledSelectedIcon: string read FDisabledSelectedIcon write FDisabledSelectedIcon;
    property RolloverIcon: string read FRolloverIcon write FRolloverIcon;
    property RolloverSelectedIcon: string read FRolloverSelectedIcon write FRolloverSelectedIcon;

    property HorizontalAlignment: THorzAlignment read FHorzAlignment write setHorzAlignment;
    property VerticalAlignment: TVertAlignment read FVertAlignment write setVertAlignment;
    property HorizontalTextPosition: THorzAlignment read FHorizontalTextPosition write setHorizontalTextPosition;
    property VerticalTextPosition: TVertAlignment read FVerticalTextPosition write setVerticalTextPosition;
    property IconTextGap: integer read FIconTextGap write setIconTextGap;
    property RolloverEnabled: boolean read FRolloverEnabled write FRolloverEnabled;
    property Border;
  end;

  TJButton = class(TAbstractButton)
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(aButton: TButton);
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NameFromText; override;
    procedure NewControl; override;
  end;

  TJToggleButton = class (TAbstractButton)
  public
    constructor Create(AOwner: TComponent); override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    procedure NameFromText; override;
    procedure NewControl; override;
  end;


implementation

uses Windows, SysUtils, Math, Graphics, Controls, UITypes,
     Vcl.Imaging.GIFImg, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
     UUtils, UGuiDesigner;

{--- TAbstractButton ----------------------------------------------------------}

constructor TAbstractButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Width:= 80;
  Height:= 24;
  FBorderPainted:= true;
  FContentAreaFilled:= true;
  HorizontalAlignment:= UAComponents.CENTER;
  VerticalAlignment  := CENTER;
  HorizontalTextPosition:= UAComponents.RIGHT; // TRAILING;
  VerticalTextPosition:= CENTER;
  IconTextGap:= 4;
end;

function TAbstractButton.getAttributes(ShowAttributes: integer): string;
  const
    Button1 = '|Text|Selected|Icon|IconTextGap';
    Button2 = '|Mnemonic|DisabledIcon|PressedIcon|SelectedIcon|RolloverIcon' +
              '|HorizontalAlignment|VerticalAlignment|Border';
    Button3 = '|DisplayedMnemonicIndex|BorderPainted|FocusPainted|ContentAreaFilled' +
              '|DisabledSelectedIcon|RolloverSelectedIcon|HorizontalTextPosition' +
              '|VerticalTextPosition|RolloverEnabled';
begin
  case ShowAttributes of
    1: Result:= Button1;
    2: Result:= Button1 + Button2;
  else Result:= Button1 + Button2 + Button3;
  end;
  Result:= Result + inherited;
end;

procedure TAbstractButton.setAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalAlignment') or (Attr = 'VerticalAlignment') or
     (Attr = 'HorizontalTextPosition') or (Attr = 'VerticalTextPosition') then
    MakeAttribut(Attr, 'SwingConstants.' + Value)
  else if Attr = 'Mnemonic' then
    MakeKeyEvent(Attr, Value)
  else
    inherited;
end;

function TAbstractButton.getEvents(ShowEvents: integer): string;
begin
  Result:= '|actionPerformed|stateChanged';
  if ShowEvents = 3 then
    Result:= Result + '|itemStateChanged';
  Result:= Result + inherited;
end;

procedure TAbstractButton.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'DisabledIcon' , NewName + 'DisabledIcon', true);
  Partner.ReplaceWord(OldName + 'DisabledSelectedIcon' , NewName + 'DisabledSelectedIcon', true);
  Partner.ReplaceWord(OldName + 'Icon' , NewName + 'Icon', true);
  Partner.ReplaceWord(OldName + 'PressedIcon' , NewName + 'PressedIcon', true);
  Partner.ReplaceWord(OldName + 'RolloverIcon' , NewName + 'RolloverIcon', true);
  Partner.ReplaceWord(OldName + 'RolloverSelectedIcon' , NewName + 'RolloverSelectedIcon', true);
  Partner.ReplaceWord(OldName + 'SelectedIcon' , NewName + 'SelectedIcon', true);
end;

procedure TAbstractButton.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'DisabledIcon');
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'DisabledSelectedIcon');
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'Icon');
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'PressedIcon');
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'RolloverIcon');
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'RolloverSelectedIcon');
  Partner.DeleteAttribute('private ImageIcon ' + Name + 'SelectedIcon');
end;

procedure TAbstractButton.SizeToText;
begin
  SizeToText(Text);
end;

procedure TAbstractButton.Paint;
  var tx, ty, ix, iy, tw, th, itg, dx, dy, x1, x2, y1, y2: integer;
      boWidth, boHeight: integer;
      s, pathname, ext: string;
      boRect: TRect;
      png: TPngImage;
      gif: TGifImage;
      jpg: TJPEGImage;
      bmp: TBitmap;
begin
  Border.Show(Self, Canvas);
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Width:= 2;
  boRect:= Border.getClientRect;
  boWidth:= boRect.Right - boRect.Left;
  boHeight:= boRect.Bottom - boRect.Top;
  if FBorderPainted
    then Canvas.Pen.Color:= DarkShadow
    else Canvas.Pen.Color:= Background;
  if FSelected and (Self is TJToggleButton)
    then Canvas.Brush.Color:= SelectionColor
    else Canvas.Brush.Color:= Background;
  if FContentAreaFilled then
    Canvas.Rectangle(boRect)
  else if FBorderPainted then begin
    Canvas.Brush.Color:= Canvas.Pen.Color;
    Canvas.FrameRect(boRect);
  end;

  s:= FText;
  tw:= Canvas.TextWidth(s);
  if tw > boWidth - 10 then begin
    while (tw > boWidth - 19) and (length(s) > 0) do begin
      s:= UUtils.Left(s, Length(s)-1);
      tw:= Canvas.TextWidth(s);
    end;
    s:= s + '...';
  end;

  tw:= Canvas.TextWidth(s);
  tx:= 0;
  case FHorzAlignment of
    UAComponents.CENTER: tx:= (boWidth - tw) div 2;
    UAComponents.LEFT  : tx:= 0;
    UAComponents.RIGHT : tx:= boWidth - tw;
  end;
  inc(tx, boRect.Left);
  th:= Canvas.TextHeight(s);
  ty:= 0;
  case FVertAlignment of
    CENTER: ty:= (boHeight-2 - th) div 2 + 2;
    UJLabel.TOP   : ty:= 1;
    BOTTOM: ty:= boHeight - th;
  end;
  inc(ty, boRect.top);

  pathname:= FGuiDesigner.getPath + 'images\' + copy(Icon, 8, length(Icon));

  if not FileExists(pathname) then begin
    Canvas.TextRect(Rect(tx, ty, tx+tw, ty+th), tx, ty, s);
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
    gif.LoadFromFile(pathname);
    bmp.Assign(gif.Bitmap);
    FreeAndNil(gif);
  end else if (ext = '.JPG') or (ext = 'JPEG') then begin
    jpg:= TJPEGImage.Create;
    jpg.LoadFromFile(pathname);
    bmp.Assign(jpg);
    FreeAndNil(jpg);
  end;

  ix:= 0;
  case FHorzAlignment of
    UAComponents.CENTER: ix:= (boWidth - bmp.Width) div 2;
    UAComponents.LEFT  : ix:= 0;
    UAComponents.RIGHT : ix:= boWidth - bmp.Width;
  end;
  inc(ix, boRect.Left);
  iy:= 0;
  case FVertAlignment of
    CENTER: iy:= (boHeight - bmp.Height) div 2;
    UJLabel.TOP   : iy:= 0;
    BOTTOM: iy:= boHeight - bmp.Height;
  end;
  inc(iy, boRect.Top);
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
    dx:= min(-tx, boWidth-ix-bmp.width) + boRect.Left;
    inc(tx, dx);
    inc(ix, dx);
  end else begin // vertical correction
    dx:= tx + tw - (boRect.Left + boWidth);
    dx:= min(dx, ix);
    if dx > 0 then begin
      dec(tx, dx);
      dec(ix, dx);
    end;
  end;
  case FVerticalTextPosition of
    CENTER:
      ty:= iy + (bmp.Height - th) div 2;
    UJLabel.TOP:
      if FHorizontalTextPosition = UAComponents.CENTER then begin
        ty:= iy - itg - th;
        if ty < boRect.Top then begin
          iy:= iy - (ty - boRect.Top);
          ty:= boRect.Top;
        end;
      end
        else ty:= iy;
    BOTTOM:
      if FHorizontalTextPosition = UAComponents.CENTER then begin
        ty:= iy + bmp.Height + itg;
        dy:= ty + th - (boRect.Top + boHeight);
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
    dx:= (boWidth - (x2 - x1)) div 2 - (x1 - boRect.Left);
    inc(tx, dx);
    inc(ix, dx);
  end;
  if FVertAlignment = CENTER then begin
    y1:= min(ty, iy);
    y2:= max(ty + th, iy + bmp.Height);
    dy:= (boHeight - (y2 - y1)) div 2 - (y1 - boRect.Top);
    inc(ty, dy);
    inc(iy, dy);
  end;

  if (ix < 4) or (tx < 4) then begin
    inc(ix, 4);
    inc(tx, 4);
  end else
  if (Width - ix - bmp.Width < 4) or (Width - tx - tw < 4) then begin
    dec(ix, 4);
    dec(tx, 4);
  end;
  if (iy < 4) or (ty < 4) then begin
    inc(iy, 4);
    inc(ty, 4);
  end else
  if (Height - iy - bmp.Height < 4) or (Height - ty - th < 4) then begin
    dec(iy, 4);
    dec(ty, 4);
  end;

  Canvas.Draw(ix, iy, bmp);
  Canvas.TextRect(Rect(0, 0, Width, Height), tx, ty, s);
  FreeAndNil(bmp);
end;

procedure TAbstractButton.setBorderPainted(aValue: boolean);
begin
  if aValue <> FBorderPainted then begin
    FBorderPainted:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setContentAreaFilled(aValue: boolean);
begin
  if aValue <> FContentAreaFilled then begin
    FContentAreaFilled:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setHorzAlignment(aValue: THorzAlignment);
begin
  if aValue <> FHorzAlignment then begin
    FHorzAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setVertAlignment(aValue: TVertAlignment);
begin
  if aValue <> FVertAlignment then begin
    FVertAlignment:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setIcon(const aValue: string);
begin
  if aValue <> FIcon then begin
    FIcon:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setHorizontalTextPosition(aValue: THorzAlignment);
begin
  if aValue <> FHorizontalTextPosition then begin
    FHorizontalTextPosition:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setVerticalTextPosition(aValue: TVertAlignment);
begin
  if aValue <> FVerticalTextPosition then begin
    FVerticalTextPosition:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setIconTextGap(aValue: integer);
begin
  if aValue <> FIconTextGap then begin
    FIconTextGap:= aValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.setText(const aValue: string);
begin
  if aValue <> FText then begin
    FText:= aValue;
    Invalidate;
  end;
end;

{--- TJButton -----------------------------------------------------------------}

constructor TJButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +4;
  FText:= 'Button';
  JavaType:= 'JButton';
end;

constructor TJButton.CreateFrom(aButton: TButton);
begin
  Create(aButton.Owner);
  CreateFromJ(aButton);
  Text:= aButton.Caption;
  Font:= aButton.Font;
  Foreground:= Font.Color;
end;

procedure TJButton.setAttribute(Attr, Value, Typ: string);
begin
  if Right(Attr, -4) = 'Icon' then
    MakeIcon(Attr, Value, 'JButton')
  else
    inherited;
end;

procedure TJButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('b' + Text);
end;

procedure TJButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JButton ' + Name + ' = new JButton();');
  MakeAttribut('Text', asString(FText));
  MakeAttribut('Margin', 'new Insets(2, 2, 2, 2)');
  AddListener('actionPerformed');
end;

{--- TJToggleButton -----------------------------------------------------------------}

constructor TJToggleButton.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +25;
  FText:= 'ToggleButton';
  JavaType:= 'JToggleButton';
end;

procedure TJToggleButton.setAttribute(Attr, Value, Typ: string);
begin
  if Right(Attr, -4) = 'Icon' then
    MakeIcon(Attr, Value, 'JToggleButton')
  else
    inherited;
end;

procedure TJToggleButton.NameFromText;
begin
  if Text <> '' then
    MakeUniqueName('tb' + Text);
end;

procedure TJToggleButton.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JToggleButton ' + Name + ' = new JToggleButton();');
  MakeAttribut('Text', asString(Text));
  MakeAttribut('Margin', 'new Insets(2, 2, 2, 2)');
  AddListener('actionPerformed');
end;

end.

