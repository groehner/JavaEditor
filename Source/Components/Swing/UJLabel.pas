unit UJLabel;

interface

uses
  Classes,
  StdCtrls,
  UAComponents,
  UJComponents;

type

  TVertAlignment = (TOP, CENTER, BOTTOM);

  TJLabel = class(TSwingComponent)
  private
    FText: string;
    FVertAlignment: TVertAlignment;
    FHorzAlignment: THorzAlignment;
    FIcon: string;
    FDisabledIcon: string;
    FHorizontalTextPosition: THorzAlignment;
    FVerticalTextPosition: TVertAlignment;
    FIconTextGap: Integer;
    FLabelFor: string;
    FDisplayedMnemonic: TShortCut;
    FDisplayedMnemonicIndex: Integer;
    procedure SetText(const AValue: string);
    procedure SetHorzAlignment(AValue: THorzAlignment);
    procedure SetVertAlignment(AValue: TVertAlignment);
    procedure SetIcon(const AValue: string);
    procedure SetDisabledIcon(const AValue: string);
    procedure SetHorizontalTextPosition(AValue: THorzAlignment);
    procedure SetVerticalTextPosition(AValue: TVertAlignment);
    procedure SetIconTextGap(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFrom(ALabel: TLabel);
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure SizeToText; override;
    procedure NameFromText; override;
    procedure DeleteComponent; override;
    procedure Paint; override;
  published
    property Text: string read FText write SetText;
    property Icon: string read FIcon write SetIcon;
    property DisabledIcon: string read FDisabledIcon write SetDisabledIcon;
    property HorizontalAlignment: THorzAlignment read FHorzAlignment
      write SetHorzAlignment;
    property VerticalAlignment: TVertAlignment read FVertAlignment
      write SetVertAlignment;
    property HorizontalTextPosition: THorzAlignment read FHorizontalTextPosition
      write SetHorizontalTextPosition;
    property VerticalTextPosition: TVertAlignment read FVerticalTextPosition
      write SetVerticalTextPosition;
    property IconTextGap: Integer read FIconTextGap write SetIconTextGap;
    property LabelFor: string read FLabelFor write FLabelFor;
    property DisplayedMnemonic: TShortCut read FDisplayedMnemonic
      write FDisplayedMnemonic;
    property DisplayedMnemonicIndex: Integer read FDisplayedMnemonicIndex
      write FDisplayedMnemonicIndex;
  end;

implementation

uses
  SysUtils,
  Math,
  Graphics,
  Controls,
  UITypes,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage,
  UUtils,
  UGUIDesigner;

{ --- TJLabel ------------------------------------------------------------------ }

constructor TJLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +1;
  Width := 80;
  Height := 24;
  FText := 'Text';
  HorizontalAlignment := UAComponents.Left; // LEADING
  VerticalAlignment := CENTER;
  HorizontalTextPosition := UAComponents.Right; // TRAILING
  VerticalTextPosition := CENTER;
  IconTextGap := 4;
  Background := clBtnFace;
  JavaType := 'JLabel';
end;

constructor TJLabel.CreateFrom(ALabel: TLabel);
begin
  Create(ALabel.Owner);
  CreateFromJ(ALabel);
  Text := ALabel.Caption;
  Font := ALabel.Font;
  Foreground := Font.Color;
  Background := ALabel.Color;
  case ALabel.Alignment of
    taLeftJustify:
      FHorzAlignment := UAComponents.Left;
    taRightJustify:
      FHorzAlignment := UAComponents.Right;
    taCenter:
      FHorzAlignment := UAComponents.CENTER;
  end;
end;

function TJLabel.GetAttributes(ShowAttributes: Integer): string;
const
  Label1 = '|Text|Font|Icon|LabelFor';
  Label2 = '|DisabledIcon|HorizontalAlignment|VerticalAlignment|HorizontalTextPosition'
    + '|VerticalTextPosition|IconTextGap|DisplayedMnemonic|DisplayedMnemonicIndex';
begin
  if ShowAttributes = 1 then
    Result := Label1
  else
    Result := Label1 + Label2;
  Result := Result + inherited;
end;

procedure TJLabel.SetAttribute(Attr, Value, Typ: string);
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
  MakeAttribut('Text', AsString(Text));
end;

procedure TJLabel.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'DisabledIcon',
    NewName + 'DisabledIcon', True);
  FPartner.ReplaceWord(OldName + 'Icon', NewName + 'Icon', True);
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
  FPartner.DeleteAttribute('private ImageIcon ' + Name + 'DisabledIcon');
  FPartner.DeleteAttribute('private ImageIcon ' + Name + 'Icon');
end;

procedure TJLabel.Paint;
var
  TextX, TextY, IXPos, IYPos, TextWidth, TextHeight, Itg, DeltaX, DeltaY, X1Pos,
    X2Pos, Y1Pos, Y2Pos: Integer;
  Str, Pathname, Ext: string;
  Png: TPngImage;
  Gif: TGIFImage;
  Jpg: TJPEGImage;
  Bmp: TBitmap;
begin
  CanvasFontAssign;
  Canvas.Pen.Color := Background;
  if Background = ColorNone then
    Canvas.Brush.Color := Parent.Brush.Color
  else
    Canvas.Brush.Color := Background;
  Str := FText;
  Canvas.Font.Color := Foreground;

  Canvas.Rectangle(Rect(0, 0, Width, Height));
  TextWidth := Canvas.TextWidth(Str);
  if TextWidth > Width then
  begin
    while (TextWidth > Width - 19) and (Str <> '') do
    begin
      Str := UUtils.Left(Str, Length(Str) - 1);
      TextWidth := Canvas.TextWidth(Str);
    end;
    Str := Str + '...';
  end;

  TextWidth := Canvas.TextWidth(Str);
  TextX := 0;
  case FHorzAlignment of
    UAComponents.CENTER:
      TextX := (Width - TextWidth) div 2;
    UAComponents.Left:
      TextX := 0;
    UAComponents.Right:
      TextX := Width - TextWidth;
  end;
  TextHeight := Canvas.TextHeight(Str);
  TextY := 0;
  case FVertAlignment of
    CENTER:
      TextY := (Height - 2 - TextHeight) div 2 + 2;
    UJLabel.TOP:
      TextY := 1;
    BOTTOM:
      TextY := Height - TextHeight;
  end;

  Pathname := FGUIDesigner.getPath + 'images\' + Copy(Icon, 8, Length(Icon));

  if not FileExists(Pathname) then
  begin
    Canvas.TextRect(Rect(0, 0, Width, Height), TextX, TextY, Str);
    Exit;
  end;

  Ext := UpperCase(ExtractFileExt(Icon));
  Bmp := TBitmap.Create;
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

  IXPos := 0;
  case FHorzAlignment of
    UAComponents.CENTER:
      IXPos := (Width - Bmp.Width) div 2;
    UAComponents.Left:
      IXPos := 0;
    UAComponents.Right:
      IXPos := Width - Bmp.Width;
  end;
  IYPos := 0;
  case FVertAlignment of
    CENTER:
      IYPos := (Height - Bmp.Height) div 2;
    UJLabel.TOP:
      IYPos := 0;
    BOTTOM:
      IYPos := Height - Bmp.Height;
  end;
  if FText = '' then
  begin
    Canvas.Draw(IXPos, IYPos, Bmp);
    FreeAndNil(Bmp);
    Exit;
  end;

  Canvas.Brush.Style := bsClear;
  Itg := FIconTextGap;
  case FHorizontalTextPosition of
    UAComponents.CENTER:
      TextX := IXPos + (Bmp.Width - TextWidth) div 2;
    UAComponents.Left:
      TextX := IXPos - Itg - TextWidth;
    UAComponents.Right:
      TextX := IXPos + Bmp.Width + Itg;
  end;
  if TextX < 0 then
  begin
    DeltaX := Min(-TextX, Width - IXPos - Bmp.Width);
    Inc(TextX, DeltaX);
    Inc(IXPos, DeltaX);
  end
  else
  begin
    DeltaX := TextX + TextWidth - Width;
    DeltaX := Min(DeltaX, IXPos);
    if DeltaX > 0 then
    begin
      Dec(TextX, DeltaX);
      Dec(IXPos, DeltaX);
    end;
  end;

  case FVerticalTextPosition of
    CENTER:
      TextY := IYPos + (Bmp.Height - TextHeight) div 2;
    UJLabel.TOP:
      if FHorizontalTextPosition = UAComponents.CENTER then
      begin
        TextY := IYPos - Itg - TextHeight;
        if TextY < 0 then
        begin
          IYPos := IYPos - TextY;
          TextY := 0;
        end;
      end
      else
        TextY := IYPos;
    BOTTOM:
      if FHorizontalTextPosition = UAComponents.CENTER then
      begin
        TextY := IYPos + Bmp.Height + Itg;
        DeltaY := TextY + TextHeight - Height;
        if DeltaY > 0 then
        begin
          Dec(IYPos, DeltaY);
          Dec(TextY, DeltaY);
        end;
      end
      else
        TextY := IYPos + Bmp.Height - TextHeight;
  end;

  if FHorzAlignment = UAComponents.CENTER then
  begin
    X1Pos := Min(TextX, IXPos);
    X2Pos := Max(TextX + TextWidth, IXPos + Bmp.Width);
    DeltaX := (Width - (X2Pos - X1Pos)) div 2 - X1Pos;
    Inc(TextX, DeltaX);
    Inc(IXPos, DeltaX);
  end;
  if FVertAlignment = CENTER then
  begin
    Y1Pos := Min(TextY, IYPos);
    Y2Pos := Max(TextY + TextHeight, IYPos + Bmp.Height);
    DeltaY := (Height - (Y2Pos - Y1Pos)) div 2 - Y1Pos;
    Inc(TextY, DeltaY);
    Inc(IYPos, DeltaY);
  end;
  Canvas.Draw(IXPos, IYPos, Bmp);
  Canvas.TextRect(Rect(0, 0, Width, Height), TextX, TextY, FText);
  FreeAndNil(Bmp);
end;

procedure TJLabel.SetHorzAlignment(AValue: THorzAlignment);
begin
  if AValue <> FHorzAlignment then
  begin
    FHorzAlignment := AValue;
    Invalidate;
  end;
end;

procedure TJLabel.SetVertAlignment(AValue: TVertAlignment);
begin
  if AValue <> FVertAlignment then
  begin
    FVertAlignment := AValue;
    Invalidate;
  end;
end;

procedure TJLabel.SetIcon(const AValue: string);
begin
  if AValue <> FIcon then
  begin
    FIcon := AValue;
    Invalidate;
  end;
end;

procedure TJLabel.SetDisabledIcon(const AValue: string);
begin
  if AValue <> FDisabledIcon then
  begin
    FDisabledIcon := AValue;
    Invalidate;
  end;
end;

procedure TJLabel.SetHorizontalTextPosition(AValue: THorzAlignment);
begin
  if AValue <> FHorizontalTextPosition then
  begin
    FHorizontalTextPosition := AValue;
    Invalidate;
  end;
end;

procedure TJLabel.SetVerticalTextPosition(AValue: TVertAlignment);
begin
  if AValue <> FVerticalTextPosition then
  begin
    FVerticalTextPosition := AValue;
    Invalidate;
  end;
end;

procedure TJLabel.SetIconTextGap(AValue: Integer);
begin
  if AValue <> FIconTextGap then
  begin
    FIconTextGap := AValue;
    Invalidate;
  end;
end;

procedure TJLabel.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

end.
