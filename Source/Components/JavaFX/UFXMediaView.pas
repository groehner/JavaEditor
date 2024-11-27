unit UFXMediaView;

interface

uses
  Classes, UFXComponents;

type
  TFXMediaView = class (TFXNode)
  private
    FFitHeight: double;
    FFitWidth: double;
    FMedia: string;
    FPreserveRatio: boolean;
    FSmooth: boolean;
    FX: double;
    FY: double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property FitHeight: double read FFitHeight write FFitHeight;
    property FitWidth: double read FFitWidth write FFitWidth;
    property Media: string read FMedia write FMedia;
    property PreserveRatio: boolean read FPreserveRatio write FPreserveRatio;
    property Smooth: boolean read FSmooth write FSmooth;
    property X: double read FX write FX;
    property Y: double read FY write FY;
  end;

implementation

uses Controls, Graphics, SysUtils, UObjectInspector;

{--- TFXMediaView --------------------------------------------------------------}

constructor TFXMediaView.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= +150;
  Background:= clBtnFace;
  FMedia:= 'file:/C:/Windows/Media/Ring01.wav';
  JavaType:= 'MediaView';
end;

procedure TFXMediaView.Paint;
begin
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
end;

procedure TFXMediaView.NewControl;
begin
  InsertImport('javafx.scene.media.*');
  InsertNewVariable('private Media ' + Name + 'Media = new Media("file:/C:/Windows/Media/Ring01.wav");');
  InsertNewVariable('private MediaPlayer ' + Name + 'MediaPlayer = new MediaPlayer(' + Name + 'Media);');
  InsertNewVariable('private MediaView ' + Name + ' = new MediaView(' + Name + 'MediaPlayer);');
  DefaultComponent;
end;

procedure TFXMediaView.SetAttribute(Attr, Value, Typ: string);
  var key, s: string;
begin
  if Attr = 'Media' then begin
    key:= 'private MediaPlayer ' + Name + 'MediaPlayer';
    s:= Indent1 + Key + ' = new MediaPlayer(' + Name + 'Media);';
    Partner.ReplaceAttribute(key, s);
    key:= 'private Media ' + Name + 'Media';
    s:= Indent1 + Key + ' = new Media("' + Value + '");';
    Partner.ReplaceAttribute(key, s);
  end;
  inherited;
  if Attr = 'Y' then
    Top:= Round(FY)
   else if Attr = 'X' then
    Left:= Round(FX);
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXMediaView.SetPositionAndSize;
  var sX, sY: string;
begin
  X:= Left;
  Y:= Top + Canvas.TextHeight(Text);
  sX:= Format('%g',[X]);
  sY:= Format('%g',[Y]);
  ChangeAttributValue(Name + '.setX(', Name + '.setX(' + sX + ');');
  ChangeAttributValue(Name + '.setY(', Name + '.setY(' + sY + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

function TFXMediaView.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|FitHeight|FitWidth|Media';
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

procedure TFXMediaView.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + 'Media' , NewName + 'Media', true);
  Partner.ReplaceWord(OldName + 'MediaPlayer' , NewName + 'MediaPlayer', true);
end;

procedure TFXMediaView.DeleteComponent;
begin
  inherited;
  Partner.DeleteAttribute('private Media ' + Name + 'Media');
  Partner.DeleteAttribute('private MediaPlayer ' + Name + 'MediaPlayer');
end;

end.
