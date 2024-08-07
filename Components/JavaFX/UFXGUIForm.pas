unit UFXGUIForm;

interface

uses
  Windows, Classes, UGuiForm;

type

  TFXGuiForm = class(TFGUIForm)
  private
    // stage attributes
    FX: integer;
    FY: integer;
    FOpacity: double;
    FFullScreen: boolean;
    FAlwaysOnTop: boolean;
    FIconified: boolean;
    FMaxWidth: integer;
    FMaxHeight: integer;
    FMinWidth: integer;
    FMinHeight: integer;

    // width/height inherited
    FcloseRequest: string;
    Fhidden: string;
    Fhiding: string;
    Fshowing: string;
    Fshown: string;
  private
    function toJavaFXColor(col: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(const Filename: string);
    function getAttributes(ShowAttributes: integer): string;
    function getEvents(ShowEvents: integer): string;
    procedure setAttribute(Attr, Value, Typ: string); override;
  published
    property Opacity: double read FOpacity write FOpacity;
    property X: integer read FX write FX;
    property Y: integer read FY write FY;
    property FullScreen: boolean read FFullScreen write FFullScreen;
    property AlwaysOnTop: boolean read FAlwaysOnTop write FAlwaysOnTop;
    property Iconified: boolean read FIconified write FIconified;
    property MaxHeight: integer read FMaxHeight write FMaxHeight;
    property MaxWidth: integer read FMaxWidth write FMaxWidth;
    property MinHeight: integer read FMinHeight write FMinHeight;
    property MinWidth: integer read FMinWidth write FMinWidth;

    property closeRequest: string read FcloseRequest write FcloseRequest;
    property hidden: string read Fhidden write Fhidden;
    property hiding: string read Fhiding write Fhiding;
    property showing: string read Fshowing write Fshowing;
    property shown: string read Fshown write Fshown;
  end;

implementation

uses SysUtils, JvGnugettext, UStringRessources,
     UConfiguration, UEditorForm;

{--- TFXGuiForm ---------------------------------------------------------------}

constructor TFXGuiForm.Create(AOwner: TComponent);
begin
  inherited;
  Tag:= 180;
end;

procedure TFXGuiForm.Open(const Filename: string);
begin
  inherited Open(Filename, '', 8);
  Title:= ChangeFileExt(ExtractFilename(Pathname), '');
end;

function TFXGuiForm.getAttributes(ShowAttributes: integer): string;
  const show1 = '|Opacity|Width|Height|Resizable|Title|Background';
        show2 = show1 + '|FullScreen|AlwaysOnTop|Iconified';
        show3 = show2 + '|MaxHeight|MaxWidth|MinHeight|MinWidth';
begin
  case ShowAttributes of
    1: Result:= show1 + '|';
    2: Result:= show2 + '|';
  else Result:= show3 + '|';
  end;
end;

function TFXGuiForm.getEvents(ShowEvents: integer): string;
begin
  Result:= '|closeRequest|hidden|hiding|showing|shown|';
end;

procedure TFXGuiForm.setAttribute(Attr, Value, Typ: string);
  var s1, s2: string;
begin
  if Attr = 'Title' then
    Caption:= Title;
  s2:= '';
  s1:= 'primaryStage.set' + Attr;
  if (Typ = 'Integer') or (Typ = 'Boolean') or (Typ = 'Double') then
    s2:= FConfiguration.Indent2 + s1 + '(' + Value + ');'
  else if Typ = 'string' then
    s2:= FConfiguration.Indent2 + s1 + '("' + Value + '");'
  else if Attr = 'Background' then
    (Partner as TFEditForm).SetFXBackgroundAsString('root', 'root', toJavaFXColor(Value));
  if s2 <> '' then
    (Partner as TFEditForm).setAttributValue(_(LNGEndComponents), s1, s2, 1);
end;

function TFXGuiForm.toJavaFXColor(col: string): string;
begin
  if copy(col, 1, 2) = '0x' then
    Result:= 'Color.web("' + col + '")'
  else if col = '(NONE)' then
    Result:= ''
  else
    Result:= 'Color.' + col;
end;

end.
