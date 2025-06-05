unit UGit;

interface

uses
  Forms,
  StdCtrls,
  ComCtrls,
  System.Classes,
  Vcl.Controls,
  UEditorForm;

type
  TFGit = class(TForm)
    LVRevisions: TListView;
    BOK: TButton;
    BCancel: TButton;
    procedure LVRevisionsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure GitCall(const Call, Folder: string);
    procedure ShowGUI(const Path: string);
    procedure ShowConsole(const Path: string);
    procedure ShowViewer(const Path: string);
    function IsRepository(const Dir: string): Boolean;
    procedure Execute(Tag: Integer; AEditor: TFEditForm);
  end;

var
  FGit: TFGit = nil;

implementation

uses
  Windows,
  SysUtils,
  Dialogs,
  JvGnugettext,
  UStringRessources,
  UConfiguration,
  UMessages,
  UJavaCommands,
  UUtils;

{$R *.dfm}

function TFGit.IsRepository(const Dir: string): Boolean;
begin
  Result := DirectoryExists(WithTrailingSlash(Dir) + '.git');
end;

procedure TFGit.LVRevisionsDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFGit.ShowGUI(const Path: string);
begin
  MyJavaCommands.ShellExecuteFile(FConfiguration.GitFolder + '\cmd\git-gui.exe',
    '', Path, SW_SHOWNORMAL);
end;

procedure TFGit.ShowViewer(const Path: string);
begin
  MyJavaCommands.ShellExecuteFile(FConfiguration.GitFolder + '\cmd\gitk.exe',
    '', Path, SW_SHOWNORMAL);
end;

procedure TFGit.ShowConsole(const Path: string);
begin
  MyJavaCommands.ShellExecuteFile(FConfiguration.GitFolder + '\bin\bash.exe',
    '', Path, SW_SHOWNORMAL);
end;

procedure TFGit.GitCall(const Call, Folder: string);
var
  Git: string;
  Clone: Boolean;
begin
  Git := FConfiguration.GitFolder + '\bin\git.exe';
  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  FMessages.OutputLineTo(K_Messages, 'git ' + Call);
  Screen.Cursor := crHourGlass;
  Clone := (Pos('clone', Call) > 0);
  if Clone then
    FMessages.OutputLineTo(K_Messages, 'Please wait!');
  if MyJavaCommands.ExecAndWait(Git, Call, Folder, FConfiguration.TempDir +
    'error.txt', SW_HIDE) then
    FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
  if Clone then
    FMessages.OutputLineTo(K_Messages, 'Done!');
  Screen.Cursor := crDefault;
end;

procedure TFGit.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  with LVRevisions do
  begin
    Columns[0].Caption := _(LNGRevision);
    Columns[1].Caption := _(LNGAuthor);
    Columns[2].Caption := _(LNGDate);
    Columns[3].Caption := _(LNGMessage);
  end;
  Caption := _(LNGRevisions);
end;

procedure TFGit.Execute(Tag: Integer; AEditor: TFEditForm);
var
  AMessage, Posi, AFile: string;
begin
  if not Assigned(AEditor) and (Tag in [1, 2, 5, 6, 7]) then
    Exit;

  if Assigned(AEditor) then begin
    if AEditor.Modified then
      AEditor.Save(False);
    AFile := ExtractFileName(AEditor.Pathname);
    Posi := ExtractFilePath(AEditor.Pathname);
  end;

  case Tag of
    1:
      GitCall('status', Posi);
    2:
      GitCall('add ' + AFile, Posi);
    3:
      if InputQuery('Commit', 'Message', AMessage) then
        FGit.GitCall('commit -Message "' + AMessage + '"',
          FConfiguration.GitLocalRepository);
    4:
      GitCall('log --stat', FConfiguration.GitLocalRepository);
    5:
      GitCall('reset HEAD ' + AFile, Posi);
    6:
      GitCall('checkout -- ' + AFile, Posi);
    7:
      GitCall('rm ' + AFile, Posi);
    8:
      GitCall('remote -v', FConfiguration.GitLocalRepository);
    9:
      GitCall('fetch ' + FConfiguration.GitRemoteRepository,
        FConfiguration.GitLocalRepository);
    10:
      GitCall('push origin master', FConfiguration.GitLocalRepository);
    11:
      ShowGUI(FConfiguration.GitLocalRepository);
    12:
      ShowViewer(FConfiguration.GitLocalRepository);
    13:
      ShowConsole(FConfiguration.GitLocalRepository);
  end;
end;

end.
