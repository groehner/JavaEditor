unit UJButton;

{ Classes
  TAbstractButton = class (TSwingComponent)
  TJButton
  TJToggleButton
}

interface

uses
  Classes,
  StdCtrls,
  UAComponents,
  UJComponents,
  UJLabel;

type

  TAbstractButton = class(TSwingComponent)
  private
    FText: string;
    FMnemonic: TShortCut;
    FDisplayedMnemonicIndex: Integer;
    FSelected: Boolean;
    FBorderPainted: Boolean;
    FFocusPainted: Boolean;
    FContentAreaFilled: Boolean;

    FVerticalAlignment: TVerticalAlignment;
    FHorizontalAlignment: THorizontalAlignment;
    FIcon: string;
    FDisabledIcon: string;

    FPressedIcon: string;
    FSelectedIcon: string;
    FDisabledSelectedIcon: string;
    FRolloverIcon: string;
    FRolloverSelectedIcon: string;

    FRolloverEnabled: Boolean;
    FHorizontalTextPosition: THorizontalAlignment;
    FVerticalTextPosition: TVerticalAlignment;
    FIconTextGap: Integer;

    procedure SetText(const AValue: string);
    procedure SetBorderPainted(AValue: Boolean);
    procedure SetContentAreaFilled(AValue: Boolean);
    procedure SetHorizontalAlignment(AValue: THorizontalAlignment);
    procedure SetVerticalAlignment(AValue: TVerticalAlignment);
    procedure SetIcon(const AValue: string);

    procedure SetHorizontalTextPosition(AValue: THorizontalAlignment);
    procedure SetVerticalTextPosition(AValue: TVerticalAlignment);
    procedure SetIconTextGap(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure SizeToText; override;
    procedure Paint; override;
    procedure DeleteComponent; override;
  published
    property Text: string read FText write SetText;
    property Mnemonic: TShortCut read FMnemonic write FMnemonic;
    property DisplayedMnemonicIndex: Integer read FDisplayedMnemonicIndex
      write FDisplayedMnemonicIndex;
    property Selected: Boolean read FSelected write FSelected;
    property BorderPainted: Boolean read FBorderPainted write SetBorderPainted;
    property FocusPainted: Boolean read FFocusPainted write FFocusPainted;

    property ContentAreaFilled: Boolean read FContentAreaFilled
      write SetContentAreaFilled;

    property Icon: string read FIcon write SetIcon;
    property DisabledIcon: string read FDisabledIcon write FDisabledIcon;
    property PressedIcon: string read FPressedIcon write FPressedIcon;
    property SelectedIcon: string read FSelectedIcon write FSelectedIcon;
    property DisabledSelectedIcon: string read FDisabledSelectedIcon
      write FDisabledSelectedIcon;
    property RolloverIcon: string read FRolloverIcon write FRolloverIcon;
    property RolloverSelectedIcon: string read FRolloverSelectedIcon
      write FRolloverSelectedIcon;

    property HorizontalAlignment: THorizontalAlignment read FHorizontalAlignment
      write SetHorizontalAlignment;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment
      write SetVerticalAlignment;
    property HorizontalTextPosition: THorizontalAlignment read FHorizontalTextPosition
      write SetHorizontalTextPosition;
    property VerticalTextPosition: TVerticalAlignment read FVerticalTextPosition
      write SetVerticalTextPosition;
    property IconTextGap: Integer read FIconTextGap write SetIconTextGap;
    property RolloverEnabled: Boolean read FRolloverEnabled
      write FRolloverEnabled;
    property Border;
  end;

  TJButton = class(TAbstractButton)
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NameFromText; override;
    procedure NewControl; override;
  end;

  TJToggleButton = class(TAbstractButton)
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    procedure NameFromText; override;
    procedure NewControl; override;
  end;

implementation

uses
  Windows,
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

{ --- TAbstractButton ---------------------------------------------------------- }

constructor TAbstractButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 80;
  Height := 24;
  FBorderPainted := True;
  FContentAreaFilled := True;
  HorizontalAlignment := UAComponents.CENTER;
  VerticalAlignment := CENTER;
  HorizontalTextPosition := UAComponents.Right; // TRAILING
  VerticalTextPosition := CENTER;
  IconTextGap := 4;
end;

function TAbstractButton.GetAttributes(ShowAttributes: Integer): string;
const
  Button1 = '|Text|Selected|Icon|IconTextGap';
  Button2 = '|Mnemonic|DisabledIcon|PressedIcon|SelectedIcon|RolloverIcon' +
    '|HorizontalAlignment|VerticalAlignment|Border';
  Button3 = '|DisplayedMnemonicIndex|BorderPainted|FocusPainted|ContentAreaFilled'
    + '|DisabledSelectedIcon|RolloverSelectedIcon|HorizontalTextPosition' +
    '|VerticalTextPosition|RolloverEnabled';
begin
  case ShowAttributes of
    1:
      Result := Button1;
    2:
      Result := Button1 + Button2;
  else
    Result := Button1 + Button2 + Button3;
  end;
  Result := Result + inherited;
end;

procedure TAbstractButton.SetAttribute(Attr, Value, Typ: string);
begin
  if (Attr = 'HorizontalAlignment') or (Attr = 'VerticalAlignment') or
    (Attr = 'HorizontalTextPosition') or (Attr = 'VerticalTextPosition') then
    MakeAttribut(Attr, 'SwingConstants.' + Value)
  else if Attr = 'Mnemonic' then
    MakeKeyEvent(Attr, Value)
  else
    inherited;
end;

function TAbstractButton.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|actionPerformed|stateChanged';
  if ShowEvents = 3 then
    Result := Result + '|itemStateChanged';
  Result := Result + inherited;
end;

procedure TAbstractButton.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'DisabledIcon',
    NewName + 'DisabledIcon', True);
  FPartner.ReplaceWord(OldName + 'DisabledSelectedIcon',
    NewName + 'DisabledSelectedIcon', True);
  FPartner.ReplaceWord(OldName + 'Icon', NewName + 'Icon', True);
  FPartner.ReplaceWord(OldName + 'PressedIcon', NewName + 'PressedIcon', True);
  FPartner.ReplaceWord(OldName + 'RolloverIcon',
    NewName + 'RolloverIcon', True);
  FPartner.ReplaceWord(OldName + 'RolloverSelectedIcon',
    NewName + 'RolloverSelectedIcon', True);
  FPartner.ReplaceWord(OldName + 'SelectedIcon',
    NewName + 'SelectedIcon', True);
end;

procedure TAbstractButton.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private ImageIcon ' + Name + 'DisabledIcon');
  FPartner.DeleteAttribute('private ImageIcon ' + Name +
    'DisabledSelectedIcon');
  FPartner.DeleteAttribute('private ImageIcon ' + Name + 'Icon');
  FPartner.DeleteAttribute('private ImageIcon ' + Name + 'PressedIcon');
  FPartner.DeleteAttribute('private ImageIcon ' + Name + 'RolloverIcon');
  FPartner.DeleteAttribute('private ImageIcon ' + Name +
    'RolloverSelectedIcon');
  FPartner.DeleteAttribute('private ImageIcon ' + Name + 'SelectedIcon');
end;

procedure TAbstractButton.SizeToText;
begin
  SizeToText(Text);
end;

procedure TAbstractButton.Paint;
var
  TextX, TextY, IXPos, IYPos, TextWidth, TextHeight, Itg, DeltaX, DeltaY, X1Pos,
    X2Pos, Y1Pos, Y2Pos: Integer;
  BoWidth, BoHeight: Integer;
  Str, Pathname, Ext: string;
  BoRect: TRect;
  Png: TPngImage;
  Gif: TGIFImage;
  Jpg: TJPEGImage;
  Bmp: TBitmap;
begin
  Border.Show(Self, Canvas);
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Width := 2;
  BoRect := Border.GetClientRect;
  BoWidth := BoRect.Right - BoRect.Left;
  BoHeight := BoRect.Bottom - BoRect.Top;
  if FBorderPainted then
    Canvas.Pen.Color := DarkShadow
  else
    Canvas.Pen.Color := Background;
  if FSelected and (Self is TJToggleButton) then
    Canvas.Brush.Color := SelectionColor
  else
    Canvas.Brush.Color := Background;
  if FContentAreaFilled then
    Canvas.Rectangle(BoRect)
  else if FBorderPainted then
  begin
    Canvas.Brush.Color := Canvas.Pen.Color;
    Canvas.FrameRect(BoRect);
  end;

  Str := FText;
  TextWidth := Canvas.TextWidth(Str);
  if TextWidth > BoWidth - 10 then
  begin
    while (TextWidth > BoWidth - 19) and (Length(Str) > 0) do
    begin
      Str := UUtils.Left(Str, Length(Str) - 1);
      TextWidth := Canvas.TextWidth(Str);
    end;
    Str := Str + '...';
  end;

  TextWidth := Canvas.TextWidth(Str);
  TextX := 0;
  case FHorizontalAlignment of
    UAComponents.Center:
      TextX := (BoWidth - TextWidth) div 2;
    UAComponents.Left:
      TextX := 0;
    UAComponents.Right:
      TextX := BoWidth - TextWidth;
  end;
  Inc(TextX, BoRect.Left);
  TextHeight := Canvas.TextHeight(Str);
  TextY := 0;
  case FVerticalAlignment of
    CENTER:
      TextY := (BoHeight - 2 - TextHeight) div 2 + 2;
    UJLabel.Top:
      TextY := 1;
    Bottom:
      TextY := BoHeight - TextHeight;
  end;
  Inc(TextY, BoRect.Top);

  Pathname := FGUIDesigner.GetPath + 'images\' + Copy(Icon, 8, Length(Icon));

  if not FileExists(Pathname) then
  begin
    Canvas.TextRect(Rect(TextX, TextY, TextX + TextWidth, TextY + TextHeight),
      TextX, TextY, Str);
    Exit;
  end;

  Ext := UpperCase(ExtractFileExt(Icon));
  Bmp := TBitmap.Create;
  if Ext = '.PNG' then
  begin
    Png := TPngImage.Create;
    Png.LoadFromFile(Pathname);
    Bmp.Assign(Png);
    FreeAndNil(Png);
  end
  else if Ext = '.GIF' then
  begin
    Gif := TGIFImage.Create;
    Gif.LoadFromFile(Pathname);
    Bmp.Assign(Gif.Bitmap);
    FreeAndNil(Gif);
  end
  else if (Ext = '.JPG') or (Ext = 'JPEG') then
  begin
    Jpg := TJPEGImage.Create;
    Jpg.LoadFromFile(Pathname);
    Bmp.Assign(Jpg);
    FreeAndNil(Jpg);
  end;

  IXPos := 0;
  case FHorizontalAlignment of
    UAComponents.CENTER:
      IXPos := (BoWidth - Bmp.Width) div 2;
    UAComponents.Left:
      IXPos := 0;
    UAComponents.Right:
      IXPos := BoWidth - Bmp.Width;
  end;
  Inc(IXPos, BoRect.Left);
  IYPos := 0;
  case FVerticalAlignment of
    CENTER:
      IYPos := (BoHeight - Bmp.Height) div 2;
    UJLabel.Top:
      IYPos := 0;
    Bottom:
      IYPos := BoHeight - Bmp.Height;
  end;
  Inc(IYPos, BoRect.Top);
  if FText = '' then
  begin
    Canvas.Draw(IXPos, IYPos, Bmp);
    FreeAndNil(Bmp);
    Exit;
  end;

  Canvas.Brush.Style := bsClear;
  Itg := FIconTextGap;
  case FHorizontalTextPosition of
    UAComponents.Left:
      TextX := IXPos - Itg - TextWidth;
    UAComponents.Center:
      TextX := IXPos + (Bmp.Width - TextWidth) div 2;
    UAComponents.Right:
      TextX := IXPos + Bmp.Width + Itg;
  end;
  if TextX < 0 then
  begin
    DeltaX := Min(-TextX, BoWidth - IXPos - Bmp.Width) + BoRect.Left;
    Inc(TextX, DeltaX);
    Inc(IXPos, DeltaX);
  end
  else
  begin // vertical correction
    DeltaX := TextX + TextWidth - (BoRect.Left + BoWidth);
    DeltaX := Min(DeltaX, IXPos);
    if DeltaX > 0 then
    begin
      Dec(TextX, DeltaX);
      Dec(IXPos, DeltaX);
    end;
  end;
  case FVerticalTextPosition of
    UJLabel.Top:
      if FHorizontalTextPosition = UAComponents.CENTER then
      begin
        TextY := IYPos - Itg - TextHeight;
        if TextY < BoRect.Top then
        begin
          IYPos := IYPos - (TextY - BoRect.Top);
          TextY := BoRect.Top;
        end;
      end
      else
        TextY := IYPos;
    Center:
      TextY := IYPos + (Bmp.Height - TextHeight) div 2;
    Bottom:
      if FHorizontalTextPosition = UAComponents.CENTER then
      begin
        TextY := IYPos + Bmp.Height + Itg;
        DeltaY := TextY + TextHeight - (BoRect.Top + BoHeight);
        if DeltaY > 0 then
        begin
          Dec(IYPos, DeltaY);
          Dec(TextY, DeltaY);
        end;
      end
      else
        TextY := IYPos + Bmp.Height - TextHeight;
  end;
  if FHorizontalAlignment = UAComponents.CENTER then
  begin
    X1Pos := Min(TextX, IXPos);
    X2Pos := Max(TextX + TextWidth, IXPos + Bmp.Width);
    DeltaX := (BoWidth - (X2Pos - X1Pos)) div 2 - (X1Pos - BoRect.Left);
    Inc(TextX, DeltaX);
    Inc(IXPos, DeltaX);
  end;
  if FVerticalAlignment = CENTER then
  begin
    Y1Pos := Min(TextY, IYPos);
    Y2Pos := Max(TextY + TextHeight, IYPos + Bmp.Height);
    DeltaY := (BoHeight - (Y2Pos - Y1Pos)) div 2 - (Y1Pos - BoRect.Top);
    Inc(TextY, DeltaY);
    Inc(IYPos, DeltaY);
  end;

  if (IXPos < 4) or (TextX < 4) then
  begin
    Inc(IXPos, 4);
    Inc(TextX, 4);
  end
  else if (Width - IXPos - Bmp.Width < 4) or (Width - TextX - TextWidth < 4)
  then
  begin
    Dec(IXPos, 4);
    Dec(TextX, 4);
  end;
  if (IYPos < 4) or (TextY < 4) then
  begin
    Inc(IYPos, 4);
    Inc(TextY, 4);
  end
  else if (Height - IYPos - Bmp.Height < 4) or (Height - TextY - TextHeight < 4)
  then
  begin
    Dec(IYPos, 4);
    Dec(TextY, 4);
  end;

  Canvas.Draw(IXPos, IYPos, Bmp);
  Canvas.TextRect(Rect(0, 0, Width, Height), TextX, TextY, Str);
  FreeAndNil(Bmp);
end;

procedure TAbstractButton.SetBorderPainted(AValue: Boolean);
begin
  if AValue <> FBorderPainted then
  begin
    FBorderPainted := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetContentAreaFilled(AValue: Boolean);
begin
  if AValue <> FContentAreaFilled then
  begin
    FContentAreaFilled := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetHorizontalAlignment(AValue: THorizontalAlignment);
begin
  if AValue <> FHorizontalAlignment then
  begin
    FHorizontalAlignment := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetVerticalAlignment(AValue: TVerticalAlignment);
begin
  if AValue <> FVerticalAlignment then
  begin
    FVerticalAlignment := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetIcon(const AValue: string);
begin
  if AValue <> FIcon then
  begin
    FIcon := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetHorizontalTextPosition(AValue: THorizontalAlignment);
begin
  if AValue <> FHorizontalTextPosition then
  begin
    FHorizontalTextPosition := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetVerticalTextPosition(AValue: TVerticalAlignment);
begin
  if AValue <> FVerticalTextPosition then
  begin
    FVerticalTextPosition := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetIconTextGap(AValue: Integer);
begin
  if AValue <> FIconTextGap then
  begin
    FIconTextGap := AValue;
    Invalidate;
  end;
end;

procedure TAbstractButton.SetText(const AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    Invalidate;
  end;
end;

{ --- TJButton ----------------------------------------------------------------- }

constructor TJButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +4;
  FText := 'Button';
  JavaType := 'JButton';
end;

procedure TJButton.SetAttribute(Attr, Value, Typ: string);
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
  MakeAttribut('Text', AsString(FText));
  MakeAttribut('Margin', 'new Insets(2, 2, 2, 2)');
  AddListener('actionPerformed');
end;

{ --- TJToggleButton ----------------------------------------------------------------- }

constructor TJToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +25;
  FText := 'ToggleButton';
  JavaType := 'JToggleButton';
end;

procedure TJToggleButton.SetAttribute(Attr, Value, Typ: string);
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
  InsertNewVariable('private JToggleButton ' + Name +
    ' = new JToggleButton();');
  MakeAttribut('Text', AsString(Text));
  MakeAttribut('Margin', 'new Insets(2, 2, 2, 2)');
  AddListener('actionPerformed');
end;

end.
