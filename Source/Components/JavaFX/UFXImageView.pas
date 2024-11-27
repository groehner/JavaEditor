unit UFXImageView;

interface

uses
  Classes, UFXComponents;

type
  TFXImageView = class (TFXNode)
  private
    FFitHeight: double;
    FFitWidth: double;
    FImage: string;
    FPreserveRatio: boolean;
    FSmooth: boolean;
    FX: double;
    FY: double;
    newLoaded: boolean;
    procedure setImage(const aValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure setFitWidthHeight;
  published
    property FitHeight: double read FFitHeight write FFitHeight;
    property FitWidth: double read FFitWidth write FFitWidth;
    property Image: string read FImage write setImage;
    property PreserveRatio: boolean read FPreserveRatio write FPreserveRatio;
    property Smooth: boolean read FSmooth write FSmooth;
    property X: double read FX write FX;
    property Y: double read FY write FY;
  end;

implementation

uses Graphics, Controls, SysUtils, GIFImg, jpeg, pngimage,
     UObjectInspector, UGuiDesigner;

{--- TFXImageView -------------------------------------------------------------}

constructor TFXImageView.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +147;
  Background:= clBtnFace;
  newLoaded:= false;
  Image:= '(Icon)';
  JavaType:= 'ImageView';
end;

procedure TFXImageView.Paint;
  var pathname, ext: string; bmp: TBitmap;
      png: TPngImage;
      gif: TGifImage; jpg: TJPEGImage;
begin
  if Image = '(Icon)' then begin
    Canvas.Pen.Color:= Background;
    Canvas.Brush.Color:= Background;
    Canvas.Rectangle(Rect(0, 0, Width, Height));
    exit;
  end;

  pathname:= FGuiDesigner.getPath + 'images\' + copy(Image, 8, length(Image));
  if not FileExists(pathname) then begin
    Canvas.TextOut(0, 0, 'missing: ' + pathname);
    exit;
  end;
  ext:= Uppercase(ExtractFileExt(Image));
  bmp:= Graphics.TBitmap.Create;
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
  if newLoaded then begin
    Width:= bmp.Width;
    Height:= bmp.Height;
    FitWidth:= Width;
    FitHeight:= Height;
    setFitWidthHeight;
    newLoaded:= false;
  end;
  Canvas.StretchDraw(Rect(0, 0, width, height), bmp);
  FreeAndNil(bmp);
end;

procedure TFXImageView.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private ImageView ' + Name + ' = new ImageView();');
  InsertImport('javafx.scene.image.*');
end;

procedure TFXImageView.setFitWidthHeight;
begin
  FitWidth:= Width;
  FitHeight:= Height;
  MakeAttribut('FitWidth', Format('%g',[FitWidth]));
  MakeAttribut('FitHeight', Format('%g',[FitHeight]));
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXImageView.SetPositionAndSize;
begin
  X:= Left;
  Y:= Top;
  MakeAttribut('X', Format('%g',[X]));
  Makeattribut('Y', Format('%g',[Y]));
  setFitWidthHeight;
end;

procedure TFXImageView.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Image' then
    MakeGraphic(Attr, Value, 'ImageView')
  else if Attr = 'FitWidth' then begin
    Width:= StrToInt(Value);
    setFitWidthHeight;
  end else if Attr = 'FitHeight' then begin
    Height:= StrToInt(Value);
    setFitWidthHeight;
  end else
    inherited;
  if Attr = 'Y' then
    Top:= Round(FY)
  else if Attr = 'X' then
    Left:= Round(FX);
  FObjectInspector.UpdatePropertyInspector;
end;

function TFXImageView.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|FitHeight|FitWidth|Image';
        Attributes2 = Attributes1 + '|PreserveRatio|Smooth|X|Y';
  var p: integer;
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes);
  p:= Pos('|LayoutX|LayoutY', Result);
  if p > 0 then
    delete(Result, p, 16);
end;

procedure TFXImageView.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'Image' , NewName + 'Image', true);
end;

procedure TFXImageView.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private Image ' + Name + 'Image');
end;

procedure TFXImageView.setImage(const aValue: string);
begin
  if aValue <> FImage then begin
    FImage:= aValue;
    Invalidate;
  end;
end;

end.
