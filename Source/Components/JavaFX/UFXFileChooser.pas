unit UFXFileChooser;

interface

uses
  Classes, UFXComponents;

type

  //TOptionType = (OK, INPUT, DEFAULT, YES_NO, OK_CANCEL, YES_NO_CANCEL);
  //TMessageType = (ERROR, INFORMATION, WARNING, QUESTION, PLAIN);

  TFXDirectoryChooser = class(TFXNode)
  private
    FInitialDirectory: string;
    FTitle: string;
  public
    constructor Create (AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
    procedure MakeFont; override;
  published
    property InitialDirectory: string read FInitialDirectory write FInitialDirectory;
    property Title: string read FTitle write FTitle;
  end;

  TFXFileSaveChooser = class(TFXDirectoryChooser)
  private
    FInitialFileName: string;
  public
    constructor Create (AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property InitialFileName: string read FInitialFileName write FInitialFileName;
  end;

  TFXFileOpenChooser = class (TFXFileSaveChooser)
  private
    FMultipleFiles: boolean;
  public
    constructor Create (AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure OpenDialog(Multiple: boolean);
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property MultipleFiles: boolean read FMultipleFiles write FMultipleFiles;
  end;

implementation

uses SysUtils, Graphics, Controls, UJava;

{--- DirectoryChooser ---------------------------------------------------------}

constructor TFXDirectoryChooser.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 146;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'DirectoryChooser';
end;

procedure TFXDirectoryChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilFXControls.Draw(Canvas, 5, 2, 16);
end;

procedure TFXDirectoryChooser.NewControl;
  var s: string;
begin
  InsertImport('java.io.File');
  InsertImport('javafx.stage.DirectoryChooser');
  InsertNewVariable('private DirectoryChooser ' + Name + ' = new DirectoryChooser();');
  s:= Indent1 + 'public File ' + Name + '_openDirectory() {'+ CrLf +
      Indent2 + 'return ' + Name + '.showDialog(null);' + CrLf +
      Indent1 + '}' + CrLf + CrLf;
  Partner.InsertProcedure(s);
end;

procedure TFXDirectoryChooser.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'InitialDirectory' then
    if Value = ''
      then MakeAttribut(Attr, '')
      else MakeAttribut(Attr, 'new File(' + asString(Value) + ')')
  else
    inherited
end;

function TFXDirectoryChooser.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|InitialDirectory|Title|' + inherited;
end;

function TFXDirectoryChooser.getEvents(ShowEvents: integer): string;
begin
  Result:= '';
end;

procedure TFXDirectoryChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_openDirectory' , NewName + '_openDirectory', true);
end;

procedure TFXDirectoryChooser.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name+ '_openDirectory', false);
end;

procedure TFXDirectoryChooser.MakeFont;
begin
  // no font
end;

{--- FileSaveChooser ----------------------------------------------------------}

constructor TFXFileSaveChooser.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 145;
  Height:= 28;
  Width:= 32;
  JavaType:= 'FileChooser';
end;

procedure TFXFileSaveChooser.Paint;
begin
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  FJava.vilFXControls.Draw(Canvas, 5, 2, 15);
end;

procedure TFXFileSaveChooser.NewControl;
  var s: string;
begin
  InsertImport('java.io.File');
  InsertImport('javafx.stage.FileChooser');
  InsertNewVariable('private FileChooser ' + Name + ' = new FileChooser();');
  s:= Indent1 + 'public File ' + Name + '_saveFile() {'+ CrLf +
      Indent2 + 'return ' + Name + '.showSaveDialog(null);' + CrLf +
      Indent1 + '}' + CrLf + CrLf;
  Partner.InsertProcedure(s);
end;

procedure TFXFileSaveChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_saveFile' , NewName + '_saveFile', true);
end;

procedure TFXFileSaveChooser.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_saveFile', false);
end;

function TFXFileSaveChooser.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|InitialFileName' + inherited getAttributes(ShowAttributes);
end;

{--- FileOpenChooser ----------------------------------------------------------}

constructor TFXFileOpenChooser.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  Tag:= 144;
  Height:= 28;
  Width:= 32;
  Sizeable:= false;
  JavaType:= 'FileChooser';
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
  OpenDialog(false);
end;

procedure TFXFileOpenChooser.OpenDialog(Multiple: boolean);
  var s: string;
begin
  Partner.DeleteMethod(Name+ '_openFile');
  if Multiple
    then s:= Indent1 + 'public List<File> ' + Name + '_openFile() {'+ CrLf +
             Indent2 + 'return ' + Name + '.showOpenMultipleDialog(null);' + CrLf
    else s:= Indent1 + 'public File ' + Name + '_openFile() {'+ CrLf +
             Indent2 + 'return ' + Name + '.showOpenDialog(null);' + CrLf;
  s:= s + Indent1 + '}' + CrLf + CrLf;
  Partner.InsertProcedure(s);
  if Multiple then
    InsertImport('java.util.List');
end;

procedure TFXFileOpenChooser.Rename(const OldName, NewName, Events: string);
begin
  inherited;
  Partner.ReplaceWord(OldName + '_openFile' , NewName + '_openFile', true);
end;

procedure TFXFileOpenChooser.DeleteComponent;
begin
  inherited;
  Partner.DeleteMethod(Name + '_openFile', false);
end;

procedure TFXFileOpenChooser.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'MultipleFiles' then
    OpenDialog(Value = 'true')
  else
    inherited
end;

function TFXFileOpenChooser.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|MultipleFiles' + inherited getAttributes(ShowAttributes);
end;

end.
