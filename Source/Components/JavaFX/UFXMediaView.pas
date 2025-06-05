unit UFXMediaView;

interface

uses
  Classes,
  UFXComponents;

type
  TFXMediaView = class(TFXNode)
  private
    FFitHeight: Double;
    FFitWidth: Double;
    FMedia: string;
    FPreserveRatio: Boolean;
    FSmooth: Boolean;
    FXPos: Double;
    FYPos: Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetPositionAndSize; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property FitHeight: Double read FFitHeight write FFitHeight;
    property FitWidth: Double read FFitWidth write FFitWidth;
    property Media: string read FMedia write FMedia;
    property PreserveRatio: Boolean read FPreserveRatio write FPreserveRatio;
    property Smooth: Boolean read FSmooth write FSmooth;
    property X: Double read FXPos write FXPos;
    property Y: Double read FYPos write FYPos;
  end;

implementation

uses
  Controls,
  Graphics,
  SysUtils,
  UObjectInspector;

{ --- TFXMediaView -------------------------------------------------------------- }

constructor TFXMediaView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +150;
  Background := clBtnFace;
  FMedia := 'file:/C:/Windows/Media/Ring01.wav';
  JavaType := 'MediaView';
end;

procedure TFXMediaView.Paint;
begin
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
end;

procedure TFXMediaView.NewControl;
begin
  InsertImport('javafx.scene.media.*');
  InsertNewVariable('private Media ' + Name +
    'Media = new Media("file:/C:/Windows/Media/Ring01.wav");');
  InsertNewVariable('private MediaPlayer ' + Name +
    'MediaPlayer = new MediaPlayer(' + Name + 'Media);');
  InsertNewVariable('private MediaView ' + Name + ' = new MediaView(' + Name +
    'MediaPlayer);');
  DefaultComponent;
end;

procedure TFXMediaView.SetAttribute(Attr, Value, Typ: string);
var
  Key, Str: string;
begin
  if Attr = 'Media' then
  begin
    Key := 'private MediaPlayer ' + Name + 'MediaPlayer';
    Str := Indent1 + Key + ' = new MediaPlayer(' + Name + 'Media);';
    FPartner.ReplaceAttribute(Key, Str);
    Key := 'private Media ' + Name + 'Media';
    Str := Indent1 + Key + ' = new Media("' + Value + '");';
    FPartner.ReplaceAttribute(Key, Str);
  end;
  inherited;
  if Attr = 'Y' then
    Top := Round(FYPos)
  else if Attr = 'X' then
    Left := Round(FXPos);
  FObjectInspector.UpdatePropertyInspector;
end;

procedure TFXMediaView.SetPositionAndSize;
var
  StrX, StrY: string;
begin
  X := Left;
  Y := Top + Canvas.TextHeight(Text);
  StrX := Format('%g', [X]);
  StrY := Format('%g', [Y]);
  ChangeAttributValue(Name + '.setX(', Name + '.setX(' + StrX + ');');
  ChangeAttributValue(Name + '.setY(', Name + '.setY(' + StrY + ');');
  FObjectInspector.UpdatePropertyInspector;
end;

function TFXMediaView.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|FitHeight|FitWidth|Media';
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

procedure TFXMediaView.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + 'Media', NewName + 'Media', True);
  FPartner.ReplaceWord(OldName + 'MediaPlayer', NewName + 'MediaPlayer', True);
end;

procedure TFXMediaView.DeleteComponent;
begin
  inherited;
  FPartner.DeleteAttribute('private Media ' + Name + 'Media');
  FPartner.DeleteAttribute('private MediaPlayer ' + Name + 'MediaPlayer');
end;

end.
