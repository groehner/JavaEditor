unit UFXImageView;

interface

uses
  Classes,
  UFXComponents;

type
  TFXImageView = class(TFXNode)
  private
    FFitHeight: Double;
    FFitWidth: Double;
    FImage: string;
    FPreserveRatio: Boolean;
    FSmooth: Boolean;
    FXPos: Double;
    FYPos: Double;
    FNewLoaded: Boolean;
    procedure SetImage(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure SetFitWidthHeight;
  published
    property FitHeight: Double read FFitHeight write FFitHeight;
    property FitWidth: Double read FFitWidth write FFitWidth;
    property Image: string read FImage write SetImage;
    property PreserveRatio: Boolean read FPreserveRatio write FPreserveRatio;
    property Smooth: Boolean read FSmooth write FSmooth;
    property X: Double read FXPos write FXPos;
    property Y: Double read FYPos write FYPos;
  end;

implementation

uses
  Graphics,
  Controls,
  SysUtils,
  GIFImg,
  jpeg,
  pngimage,
  UObjectInspector,
  UGUIDesigner;

{ --- TFXImageView ------------------------------------------------------------- }

constructor TFXImageView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +147;
  Background := clBtnFace;
  FNewLoaded := False;
  Image := '(Icon)';
  JavaType := 'ImageView';
end;

procedure TFXImageView.Paint;
var
  Pathname, Ext: string;
  Bmp: TBitmap;
  Png: TPngImage;
  Gif: TGIFImage;
  Jpg: TJPEGImage;
begin
  if Image = '(Icon)' then
  begin
    Canvas.Pen.Color := Background;
    Canvas.Brush.Color := Background;
    Canvas.Rectangle(Rect(0, 0, Width, Height));
    Exit;
  end;

  Pathname := FGUIDesigner.GetPath + 'images\' + Copy(Image, 8, Length(Image));
  if not FileExists(Pathname) then
  begin
    Canvas.TextOut(0, 0, 'missing: ' + Pathname);
    Exit;
  end;
  Ext := UpperCase(ExtractFileExt(Image));
  Bmp := Graphics.TBitmap.Create;
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
  if FNewLoaded then
  begin
    Width := Bmp.Width;
    Height := Bmp.Height;
    FitWidth := Width;
    FitHeight := Height;
    SetFitWidthHeight;
    FNewLoaded := False;
  end;
  Canvas.StretchDraw(Rect(0, 0, Width, Height), Bmp);
  FreeAndNil(Bmp);
end;

procedure TFXImageView.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ImageView ' + Name + ' = new ImageView();');
  InsertImport('javafx.scene.image.*');
end;

procedure TFXImageView.SetFitWidthHeight;
begin
  FitWidth := Width;
  FitHeight := Height;
  MakeAttribut('FitWidth', Format('%g', [FitWidth]));
  MakeAttribut('FitHeight', Format('%g', [FitHeight]));
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXImageView.SetPositionAndSize;
begin
  FXPos := Left;
  FYPos := Top;
  MakeAttribut('X', Format('%g', [FXPos]));
  MakeAttribut('Y', Format('%g', [FYPos]));
  SetFitWidthHeight;
end;

procedure TFXImageView.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Image' then
    MakeGraphic(Attr, Value, 'ImageView')
  else if Attr = 'FitWidth' then
  begin
    Width := StrToInt(Value);
    SetFitWidthHeight;
  end
  else if Attr = 'FitHeight' then
  begin
    Height := StrToInt(Value);
    SetFitWidthHeight;
  end
  else
    inherited;
  if Attr = 'Y' then
    Top := Round(FYPos)
  else if Attr = 'X' then
    Left := Round(FXPos);
  FObjectInspector.UpdatePropertyInspector;
end;

function TFXImageView.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|FitHeight|FitWidth|Image';
  Attributes2 = Attributes1 + '|PreserveRatio|Smooth|X|Y';
var
  Posi: Integer;
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
  Posi := Pos('|LayoutX|LayoutY', Result);
  if Posi > 0 then
    Delete(Result, Posi, 16);
end;

procedure TFXImageView.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'Image', NewName + 'Image', True);
end;

procedure TFXImageView.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private Image ' + Name + 'Image');
end;

procedure TFXImageView.SetImage(const AValue: string);
begin
  if AValue <> FImage then
  begin
    FImage := AValue;
    Invalidate;
  end;
end;

end.
