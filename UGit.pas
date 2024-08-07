unit UGit;

interface

uses
  Forms, StdCtrls, ComCtrls, UFrmEditor, System.Classes, Vcl.Controls;

type
  TFGit = class(TForm)
    LVRevisions: TListView;
    BOK: TButton;
    BCancel: TButton;
    procedure LVRevisionsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure GitCall(const call, folder: string);
    procedure ShowGUI(const Path: string);
    procedure ShowConsole(const Path: string);
    procedure ShowViewer(const Path: string);
    function IsRepository(const Dir: string): boolean;
    procedure Execute(Tag: integer; aEditor: TFEditForm);
  end;

var
  FGit: TFGit = nil;

implementation

uses Windows, SysUtils, Dialogs, JvGnugettext, UStringRessources,
     UConfiguration, UMessages, UJavaCommands, UUtils;

{$R *.dfm}

function TFGit.IsRepository(const Dir: string): boolean;
begin
  Result:= DirectoryExists(withTrailingSlash(Dir) + '.git');
end;

procedure TFGit.LVRevisionsDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFGit.ShowGUI(const Path: string);
begin
  myJavaCommands.ShellExecuteFile(FConfiguration.GitFolder + '\cmd\git-gui.exe', '', Path, SW_SHOWNORMAL)
end;

procedure TFGit.ShowViewer(const Path: string);
begin
  myJavaCommands.ShellExecuteFile(FConfiguration.GitFolder + '\cmd\gitk.exe', '', Path, SW_SHOWNORMAL)
end;

procedure TFGit.ShowConsole(const Path: string);
begin
  myJavaCommands.ShellExecuteFile(FConfiguration.GitFolder + '\bin\bash.exe', '', Path, SW_SHOWNORMAL)
end;

procedure TFGit.GitCall(const call, folder: string);
  var git: string; clone: boolean;
begin
  git:= FConfiguration.GitFolder + '\bin\git.exe';
  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  FMessages.OutputLineTo(K_Messages, 'git ' + call);
  Screen.Cursor:= crHourGlass;
  clone:= (Pos('clone', call) > 0);
  if clone then FMessages.OutputLineTo(K_Messages, 'Please wait!');
  if myJavaCommands.ExecAndWait(git, call, folder, FConfiguration.TempDir + 'error.txt', SW_Hide) then
    FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
  if clone then FMessages.OutputLineTo(K_Messages, 'Done!');
  Screen.Cursor:= crDefault;
end;

procedure TFGit.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  with LVRevisions do begin
    Columns.Items[0].Caption:= _(LNGRevision);
    Columns.Items[1].Caption:= _(LNGAuthor);
    Columns.Items[2].Caption:= _(LNGDate);
    Columns.Items[3].Caption:= _(LNGMessage);
  end;
  Caption:= _(LNGRevisions);
end;

procedure TFGit.Execute(Tag: integer; aEditor: TFEditForm);
  var m, p, f: string;
begin
  if (aEditor = nil) and (Tag in [1, 2, 5, 6, 7]) then
    exit;

  if aEditor.Modified then
    aEditor.Save(false);
  f:= ExtractFileName(aEditor.Pathname);
  p:= ExtractFilePath(aEditor.Pathname);

  case Tag of
    1: GitCall('status', p);
    2: GitCall('add ' + f, p);
    3: if InputQuery('Commit', 'Message', m) then
         FGit.GitCall('commit -m "' + m + '"', FConfiguration.GitLocalRepository);
    4: GitCall('log --stat', FConfiguration.GitLocalRepository);
    5: GitCall('reset HEAD ' + f, p);
    6: GitCall('checkout -- ' + f, p);
    7: GitCall('rm ' + f, p);
    8: GitCall('remote -v', FConfiguration.GitLocalRepository);
    9: GitCall('fetch ' + FConfiguration.GitRemoteRepository, FConfiguration.GitLocalRepository);
   10: GitCall('push origin master', FConfiguration.GitLocalRepository);
   11: ShowGui(FConfiguration.GitLocalRepository);
   12: ShowViewer(FConfiguration.GitLocalRepository);
   13: ShowConsole(FConfiguration.GitLocalRepository);
  end;
end;

end.
