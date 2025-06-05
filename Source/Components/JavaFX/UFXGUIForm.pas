unit UFXGUIForm;

interface

uses
  Windows,
  Classes,
  UGUIForm;

type

  TFXGUIForm = class(TFGUIForm)
  private
    // stage attributes
    FXPos: Integer;
    FYPos: Integer;
    FOpacity: Double;
    FFullScreen: Boolean;
    FAlwaysOnTop: Boolean;
    FIconified: Boolean;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FMinWidth: Integer;
    FMinHeight: Integer;

    // width/height inherited
    FcloseRequest: string;
    Fhidden: string;
    Fhiding: string;
    Fshowing: string;
    Fshown: string;
    function ToJavaFXColor(Col: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(const FileName: string; State: string); override;
    function GetAttributes(ShowAttributes: Integer): string;
    function GetEvents(ShowEvents: Integer): string;
    procedure SetAttribute(Attr, Value, Typ: string); override;
  published
    property Opacity: Double read FOpacity write FOpacity;
    property X: Integer read FXPos write FXPos;
    property Y: Integer read FYPos write FYPos;
    property FullScreen: Boolean read FFullScreen write FFullScreen;
    property AlwaysOnTop: Boolean read FAlwaysOnTop write FAlwaysOnTop;
    property Iconified: Boolean read FIconified write FIconified;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MinWidth: Integer read FMinWidth write FMinWidth;

    property closeRequest: string read FcloseRequest write FcloseRequest;
    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

implementation

uses
  SysUtils,
  JvGnugettext,
  UStringRessources,
  UConfiguration,
  UEditorForm;

{ --- TFXGUIForm --------------------------------------------------------------- }

constructor TFXGUIForm.Create(AOwner: TComponent);
begin
  inherited;
  Tag := 180;
end;

procedure TFXGUIForm.Open(const FileName: string; State: string);
begin
  inherited Open(FileName, State);
  Title := ChangeFileExt(ExtractFileName(Pathname), '');
end;

function TFXGUIForm.GetAttributes(ShowAttributes: Integer): string;
const
  Show1 = '|Opacity|Width|Height|Resizable|Title|Background';
  Show2 = Show1 + '|FullScreen|AlwaysOnTop|Iconified';
  Show3 = Show2 + '|MaxHeight|MaxWidth|MinHeight|MinWidth';
begin
  case ShowAttributes of
    1:
      Result := Show1 + '|';
    2:
      Result := Show2 + '|';
  else
    Result := Show3 + '|';
  end;
end;

function TFXGUIForm.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|closeRequest|hidden|hiding|showing|shown|';
end;

procedure TFXGUIForm.SetAttribute(Attr, Value, Typ: string);
var
  Str1, Str2: string;
begin
  if Attr = 'Title' then
    Caption := Title;
  Str2 := '';
  Str1 := 'primaryStage.set' + Attr;
  if (Typ = 'Integer') or (Typ = 'Boolean') or (Typ = 'Double') then
    Str2 := FConfiguration.Indent2 + Str1 + '(' + Value + ');'
  else if Typ = 'string' then
    Str2 := FConfiguration.Indent2 + Str1 + '("' + Value + '");'
  else if Attr = 'Background' then
    (Partner as TFEditForm).SetFXBackgroundAsString('root', 'root',
      ToJavaFXColor(Value));
  if Str2 <> '' then
    (Partner as TFEditForm).SetAttributValue(_(LNGEndComponents), Str1,
      Str2, 1);
end;

function TFXGUIForm.ToJavaFXColor(Col: string): string;
begin
  if Copy(Col, 1, 2) = '0x' then
    Result := 'Color.web("' + Col + '")'
  else if Col = '(NONE)' then
    Result := ''
  else
    Result := 'Color.' + Col;
end;

end.
