unit UDlgClasspath;

interface

uses
  Forms, StdCtrls, CheckLst, Vcl.Controls, System.Classes;

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
  public
    ClasspathUser: string;
    ClasspathAll: string;
    JavaDoc: boolean;
    function HasIndexHTML(s: string): boolean;
    procedure Initialize(s: string; JavaDoc: boolean);
  end;

implementation

uses SysUtils, Dialogs, JvGnugettext,
     UStringRessources, UConfiguration, UUtils;

{$R *.dfm}

procedure TFClasspath.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  CBAllJarFiles.Enabled:= false;
end;

function TFClasspath.HasIndexHTML(s: string): boolean;
begin
  Result:= (Pos('\allclasses-frame.html', s) + Pos('\allclasses-index.html', s) > 0);
end;

procedure TFClasspath.Initialize(s: string; JavaDoc: boolean);
  var dir: string; Count, p: integer; ok: boolean;
begin
  Self.JavaDoc:= JavaDoc;
  CLBPfade.Clear;
  CLBPfade.MultiSelect:= true;
  Count:= 0;
  if s <> '' then
    repeat
      p:= Pos(';', s);
      if p > 0 then begin
        dir:= Trim(Copy(s, 1, p-1));
        delete(s, 1, p);
      end else begin
        dir:= s;
        s:= '';
      end;
      p:= Pos('#', dir);
      if p = 2 then begin
        ok:= dir[1] = '1';
        delete(dir, 1, 2);
      end else
        ok:= true;
      p:= Pos('  <-- ', dir);
      if p > 0 then delete(dir, p, length(dir));
      dir:= withoutTrailingSlash(dir);
      if JavaDoc and not HasIndexHTML(dir) then begin
        if FileExists(dir + '\allclasses-index.html') then
          dir:= dir + '\allclasses-index.html'
        else if FileExists(dir + '\allclasses-frame.html') then
          dir:= dir + '\allclasses-frame.html'
      end;

      if FileExists(dir) then
        CLBPfade.addItem(dir, nil)
      else if endsWith(dir, '*') then
        if DirectoryExists(copy(dir, 1, length(dir)-1))
          then CLBPfade.addItem(dir, nil)
          else CLBPfade.addItem(dir + '  <-- ' + _(LNGDoesNotExist), nil)
      else begin
        dir:= dir + '\';
        if DirectoryExists(dir)
          then CLBPfade.addItem(dir, nil)
          else CLBPfade.addItem(dir + '  <-- ' + _(LNGDoesNotExist), nil);
      end;
      CLBPfade.Checked[Count]:= ok;
      inc(Count);
    until s = '';
end;

procedure TFClasspath.BSaveClick(Sender: TObject);
  var i, p: integer; s: string;
begin
  ClasspathUser:= '';
  ClasspathAll:= '';
  for i:= 0 to CLBPfade.Count - 1 do begin
    if CLBPfade.Checked[i] then begin
      s:= CLBPfade.Items[i];
      p:= pos('  <-- ', s);
      if p > 0 then delete(s, p, length(s));
      if JavaDoc and not HasIndexHTML(s) then
        s:= s + '\allclasses-index.html';
      ClasspathUser:= ClasspathUser + ';' + s;
      ClasspathAll:= ClasspathAll + '1#' + CLBPfade.Items[i] + ';';
    end else
      ClasspathAll:= ClasspathAll + '0#' + CLBPfade.Items[i] + ';';
  end;
  ClasspathUser:= copy(ClasspathUser, 2, length(ClasspathUser));
end;

procedure TFClasspath.CBAllJarFilesClick(Sender: TObject);
begin
  var i:= CLBPfade.ItemIndex;
  if i > -1 then begin
    var s:= CLBPfade.Items[i];
    if not (pos('  <-- ', s) > 0) or HasIndexHTML(s) or
       FileExists(s)
    then begin
      if endswith(s, '\*') and not CBAlljarFiles.Checked then
        delete(s, length(s), 1)
      else if DirectoryExists(s) and CBAllJarFiles.Checked then
        s:= withTrailingSlash(s) + '*';
      CLBPfade.Items[i]:= s;
    end;
  end;
  CLBPfade.ItemIndex:= i;
end;

procedure TFClasspath.CLBPfadeClick(Sender: TObject);
begin
  var i:= CLBPfade.ItemIndex;
  if i > -1 then begin
    var s:= CLBPfade.Items[i];
    if ((pos('  <-- ', s) > 0) or HasIndexHTML(s) or
       FileExists(s))
    then begin
      CBAllJarFiles.Checked:= false;
      CBAllJarFiles.Enabled:= false;
    end
    else begin
      CBAllJarFiles.Checked:= endswith(s, '\*');
      CBAllJarFiles.Enabled:= true;
    end;
  end;
end;

procedure TFClasspath.BDeleteClick(Sender: TObject);
begin
  CLBPfade.DeleteSelected;
end;

procedure TFClasspath.BNewJarFileClick(Sender: TObject);
begin
  with FConfiguration.ODSelect do begin
    InitialDir:= FConfiguration.Sourcepath;
    Filename:= '';
    Filter:= 'jar-' + _(LNGFile) + '|*.jar|' + _(LNGAll) + '|*.*';
    Options:= Options +  [ofAllowMultiSelect];
    if Execute then
      for var i:= 0 to Files.Count - 1 do begin
        CLBPfade.AddItem(Files.Strings[i], nil);
        CLBPfade.Checked[CLBPfade.Count-1]:= true;
      end;
    Options:= Options - [ofAllowMultiSelect];
  end;
  BringToFront;
end;

procedure TFClasspath.BNewAllClassesClick(Sender: TObject);
begin
  with FConfiguration.ODSelect do begin
    InitialDir:= FConfiguration.Sourcepath;
    Filename:= 'allclasses-index.html';
    Filter:= 'html-' + _(LNGFile) + '|*.html|' + _(LNGAll) + '|*.*';
    Options:= Options +  [ofAllowMultiSelect];
    if Execute then
      for var i:= 0 to Files.Count - 1 do begin
        CLBPfade.AddItem(Files.Strings[i], nil);
        CLBPfade.Checked[CLBPfade.Count-1]:= true;
      end;
    Options:= Options - [ofAllowMultiSelect];
  end;
  BringToFront;
end;

procedure TFClasspath.BNewFolderClick(Sender: TObject);
begin
  {$WARNINGS OFF}
  with FConfiguration.FolderDialog do begin
    DefaultFolder:= GetDocumentsPath;
    if Execute then begin
      CLBPfade.AddItem(Filename, nil);
      CLBPfade.Checked[CLBPfade.Count-1]:= true;
    end;
  end;
  {$WARNINGS ON}
  BringToFront;
end;

end.
