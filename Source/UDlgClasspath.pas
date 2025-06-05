unit UDlgClasspath;

interface

uses
  Forms,
  StdCtrls,
  CheckLst,
  Vcl.Controls,
  System.Classes;

type
  TFClasspath = class(TForm)
    BNewJarFile: TButton;
    BNewFolder: TButton;
    BSave: TButton;
    BDelete: TButton;
    CLBPfade: TCheckListBox;
    CBAllJarFiles: TCheckBox;
    BNewAllClasses: TButton;
    procedure BNewJarFileClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BNewFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBAllJarFilesClick(Sender: TObject);
    procedure CLBPfadeClick(Sender: TObject);
    procedure BNewAllClassesClick(Sender: TObject);
  private
    FClasspathUser: string;
    FClasspathAll: string;
    FJavaDoc: Boolean;
  public
    function HasIndexHTML(Str: string): Boolean;
    procedure Initialize(Str: string; JavaDoc: Boolean);
    property ClasspathAll: string read FClasspathAll;
    property ClasspathUser: string read FClasspathUser;
    property JavaDoc: Boolean read FJavaDoc;
  end;

implementation

uses
  SysUtils,
  Dialogs,
  JvGnugettext,
  UStringRessources,
  UConfiguration,
  UUtils;

{$R *.dfm}

procedure TFClasspath.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  CBAllJarFiles.Enabled := False;
end;

function TFClasspath.HasIndexHTML(Str: string): Boolean;
begin
  Result := (Pos('\allclasses-frame.html', Str) + Pos('\allclasses-index.html',
    Str) > 0);
end;

procedure TFClasspath.Initialize(Str: string; JavaDoc: Boolean);
var
  Dir: string;
  Count, Posi: Integer;
  DirOk: Boolean;
begin
  Self.FJavaDoc := JavaDoc;
  CLBPfade.Clear;
  CLBPfade.MultiSelect := True;
  Count := 0;
  if Str <> '' then
    repeat
      Posi := Pos(';', Str);
      if Posi > 0 then
      begin
        Dir := Trim(Copy(Str, 1, Posi - 1));
        Delete(Str, 1, Posi);
      end
      else
      begin
        Dir := Str;
        Str := '';
      end;
      Posi := Pos('#', Dir);
      if Posi = 2 then
      begin
        DirOk := Dir[1] = '1';
        Delete(Dir, 1, 2);
      end
      else
        DirOk := True;
      Posi := Pos('  <-- ', Dir);
      if Posi > 0 then
        Delete(Dir, Posi, Length(Dir));
      Dir := ExcludeTrailingPathDelimiter(Dir);
      if JavaDoc and not HasIndexHTML(Dir) then
      begin
        if FileExists(Dir + '\allclasses-index.html') then
          Dir := Dir + '\allclasses-index.html'
        else if FileExists(Dir + '\allclasses-frame.html') then
          Dir := Dir + '\allclasses-frame.html';
      end;

      if FileExists(Dir) then
        CLBPfade.AddItem(Dir, nil)
      else if EndsWith(Dir, '*') then
        if DirectoryExists(Copy(Dir, 1, Length(Dir) - 1)) then
          CLBPfade.AddItem(Dir, nil)
        else
          CLBPfade.AddItem(Dir + '  <-- ' + _(LNGDoesNotExist), nil)
      else
      begin
        Dir := Dir + '\';
        if DirectoryExists(Dir) then
          CLBPfade.AddItem(Dir, nil)
        else
          CLBPfade.AddItem(Dir + '  <-- ' + _(LNGDoesNotExist), nil);
      end;
      CLBPfade.Checked[Count] := DirOk;
      Inc(Count);
    until Str = '';
end;

procedure TFClasspath.BSaveClick(Sender: TObject);
var
  Posi: Integer;
  Str: string;
begin
  FClasspathUser := '';
  FClasspathAll := '';
  for var I := 0 to CLBPfade.Count - 1 do
  begin
    if CLBPfade.Checked[I] then
    begin
      Str := CLBPfade.Items[I];
      Posi := Pos('  <-- ', Str);
      if Posi > 0 then
        Delete(Str, Posi, Length(Str));
      if JavaDoc and not HasIndexHTML(Str) then
        Str := Str + '\allclasses-index.html';
      FClasspathUser := ClasspathUser + ';' + Str;
      FClasspathAll := ClasspathAll + '1#' + CLBPfade.Items[I] + ';';
    end
    else
      FClasspathAll := ClasspathAll + '0#' + CLBPfade.Items[I] + ';';
  end;
  FClasspathUser := Copy(ClasspathUser, 2, Length(ClasspathUser));
end;

procedure TFClasspath.CBAllJarFilesClick(Sender: TObject);
begin
  var Int := CLBPfade.ItemIndex;
  if Int > -1 then
  begin
    var Str := CLBPfade.Items[Int];
    if not(Pos('  <-- ', Str) > 0) or HasIndexHTML(Str) or FileExists(Str) then
    begin
      if EndsWith(Str, '\*') and not CBAllJarFiles.Checked then
        Delete(Str, Length(Str), 1)
      else if DirectoryExists(Str) and CBAllJarFiles.Checked then
        Str := WithTrailingSlash(Str) + '*';
      CLBPfade.Items[Int] := Str;
    end;
  end;
  CLBPfade.ItemIndex := Int;
end;

procedure TFClasspath.CLBPfadeClick(Sender: TObject);
begin
  var Int := CLBPfade.ItemIndex;
  if Int > -1 then
  begin
    var Str := CLBPfade.Items[Int];
    if ((Pos('  <-- ', Str) > 0) or HasIndexHTML(Str) or FileExists(Str)) then
    begin
      CBAllJarFiles.Checked := False;
      CBAllJarFiles.Enabled := False;
    end
    else
    begin
      CBAllJarFiles.Checked := EndsWith(Str, '\*');
      CBAllJarFiles.Enabled := True;
    end;
  end;
end;

procedure TFClasspath.BDeleteClick(Sender: TObject);
begin
  CLBPfade.DeleteSelected;
end;

procedure TFClasspath.BNewJarFileClick(Sender: TObject);
begin
  with FConfiguration.ODSelect do
  begin
    InitialDir := FConfiguration.Sourcepath;
    FileName := '';
    Filter := 'jar-' + _(LNGFile) + '|*.jar|' + _(LNGAll) + '|*.*';
    Options := Options + [ofAllowMultiSelect];
    if Execute then
      for var I := 0 to Files.Count - 1 do
      begin
        CLBPfade.AddItem(Files[I], nil);
        CLBPfade.Checked[CLBPfade.Count - 1] := True;
      end;
    Options := Options - [ofAllowMultiSelect];
  end;
  BringToFront;
end;

procedure TFClasspath.BNewAllClassesClick(Sender: TObject);
begin
  with FConfiguration.ODSelect do
  begin
    InitialDir := FConfiguration.Sourcepath;
    FileName := 'allclasses-index.html';
    Filter := 'html-' + _(LNGFile) + '|*.html|' + _(LNGAll) + '|*.*';
    Options := Options + [ofAllowMultiSelect];
    if Execute then
      for var I := 0 to Files.Count - 1 do
      begin
        CLBPfade.AddItem(Files[I], nil);
        CLBPfade.Checked[CLBPfade.Count - 1] := True;
      end;
    Options := Options - [ofAllowMultiSelect];
  end;
  BringToFront;
end;

procedure TFClasspath.BNewFolderClick(Sender: TObject);
begin
  with FConfiguration.FolderDialog do
  begin
    DefaultFolder := GetDocumentsPath;
    if Execute then
    begin
      CLBPfade.AddItem(FileName, nil);
      CLBPfade.Checked[CLBPfade.Count - 1] := True;
    end;
  end;
  BringToFront;
end;

end.
