unit UFXFileChooser;

interface

uses
  Classes,
  UFXComponents;

type

  // TOptionType = (OK, INPUT, DEFAULT, YES_NO, OK_CANCEL, YES_NO_CANCEL);
  // TMessageType = (ERROR, INFORMATION, WARNING, QUESTION, PLAIN);

  TFXDirectoryChooser = class(TFXNode)
  private
    FInitialDirectory: string;
    FTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure MakeFont; override;
  published
    property InitialDirectory: string read FInitialDirectory
      write FInitialDirectory;
    property Title: string read FTitle write FTitle;
  end;

  TFXFileSaveChooser = class(TFXDirectoryChooser)
  private
    FInitialFileName: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property InitialFileName: string read FInitialFileName
      write FInitialFileName;
  end;

  TFXFileOpenChooser = class(TFXFileSaveChooser)
  private
    FMultipleFiles: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure OpenDialog(Multiple: Boolean);
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property MultipleFiles: Boolean read FMultipleFiles write FMultipleFiles;
  end;

implementation

uses
  Controls,
  UJava;

{ --- DirectoryChooser --------------------------------------------------------- }

constructor TFXDirectoryChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 146;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'DirectoryChooser';
end;

procedure TFXDirectoryChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilFXControls.Draw(Canvas, 5, 2, 16);
end;

procedure TFXDirectoryChooser.NewControl;
var
  Str: string;
begin
  InsertImport('java.io.File');
  InsertImport('javafx.stage.DirectoryChooser');
  InsertNewVariable('private DirectoryChooser ' + Name +
    ' = new DirectoryChooser();');
  Str := Indent1 + 'public File ' + Name + '_openDirectory() {' + CrLf + Indent2
    + 'return ' + Name + '.showDialog(null);' + CrLf + Indent1 + '}' +
    CrLf + CrLf;
  FPartner.InsertProcedure(Str);
end;

procedure TFXDirectoryChooser.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'InitialDirectory' then
    if Value = '' then
      MakeAttribut(Attr, '')
    else
      MakeAttribut(Attr, 'new File(' + AsString(Value) + ')')
  else
    inherited;
end;

function TFXDirectoryChooser.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|InitialDirectory|Title|' + inherited;
end;

function TFXDirectoryChooser.GetEvents(ShowEvents: Integer): string;
begin
  Result := '';
end;

procedure TFXDirectoryChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_openDirectory',
    NewName + '_openDirectory', True);
end;

procedure TFXDirectoryChooser.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_openDirectory', False);
end;

procedure TFXDirectoryChooser.MakeFont;
begin
  // no font
end;

{ --- FileSaveChooser ---------------------------------------------------------- }

constructor TFXFileSaveChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 145;
  Height := 28;
  Width := 32;
  JavaType := 'FileChooser';
end;

procedure TFXFileSaveChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilFXControls.Draw(Canvas, 5, 2, 15);
end;

procedure TFXFileSaveChooser.NewControl;
var
  Str: string;
begin
  InsertImport('java.io.File');
  InsertImport('javafx.stage.FileChooser');
  InsertNewVariable('private FileChooser ' + Name + ' = new FileChooser();');
  Str := Indent1 + 'public File ' + Name + '_saveFile() {' + CrLf + Indent2 +
    'return ' + Name + '.showSaveDialog(null);' + CrLf + Indent1 + '}' +
    CrLf + CrLf;
  FPartner.InsertProcedure(Str);
end;

procedure TFXFileSaveChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_saveFile', NewName + '_saveFile', True);
end;

procedure TFXFileSaveChooser.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_saveFile', False);
end;

function TFXFileSaveChooser.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|InitialFileName' + inherited GetAttributes(ShowAttributes);
end;

{ --- FileOpenChooser ---------------------------------------------------------- }

constructor TFXFileOpenChooser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 144;
  Height := 28;
  Width := 32;
  Sizeable := False;
  JavaType := 'FileChooser';
end;

procedure TFXFileOpenChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilFXControls.Draw(Canvas, 5, 2, 14);
end;

procedure TFXFileOpenChooser.NewControl;
begin
  InsertNewVariable('private FileChooser ' + Name + ' = new FileChooser();');
  InsertImport('java.io.File');
  InsertImport('javafx.stage.FileChooser');
  OpenDialog(False);
end;

procedure TFXFileOpenChooser.OpenDialog(Multiple: Boolean);
var
  Str: string;
begin
  FPartner.DeleteMethod(Name + '_openFile');
  if Multiple then
    Str := Indent1 + 'public List<File> ' + Name + '_openFile() {' + CrLf +
      Indent2 + 'return ' + Name + '.showOpenMultipleDialog(null);' + CrLf
  else
    Str := Indent1 + 'public File ' + Name + '_openFile() {' + CrLf + Indent2 +
      'return ' + Name + '.showOpenDialog(null);' + CrLf;
  Str := Str + Indent1 + '}' + CrLf + CrLf;
  FPartner.InsertProcedure(Str);
  if Multiple then
    InsertImport('java.util.List');
end;

procedure TFXFileOpenChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  FPartner.ReplaceWord(OldName + '_openFile', NewName + '_openFile', True);
end;

procedure TFXFileOpenChooser.DeleteComponent;
begin
  inherited;
  FPartner.DeleteMethod(Name + '_openFile', False);
end;

procedure TFXFileOpenChooser.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MultipleFiles' then
    OpenDialog(Value = 'true')
  else
    inherited;
end;

function TFXFileOpenChooser.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|MultipleFiles' + inherited GetAttributes(ShowAttributes);
end;

end.
