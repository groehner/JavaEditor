unit USubversion;

interface

uses
  Forms,
  StdCtrls,
  ComCtrls,
  System.Classes,
  Vcl.Controls;

type
  TFSubversion = class(TForm)
    LVRevisions: TListView;
    BOK: TButton;
    BCancel: TButton;
    procedure LVRevisionsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure ForFile(const AFile: string);
    function GetRevision: string;
    function GetRepositoryURL(Str: string): string;
    function IsRepository(const Dir: string): Boolean;
    procedure CallSVN(Prog: string; const Call, Dir: string);
  end;

var
  FSubversion: TFSubversion = nil;

implementation

uses
  Windows,
  SysUtils,
  JvGnugettext,
  UStringRessources,
  UConfiguration,
  UMessages,
  UJavaCommands,
  UUtils;

{$R *.dfm}

procedure TFSubversion.ForFile(const AFile: string);
var
  AMessage, Rev, Aut, Str: string;
  Posi, Idx: Integer;
  ListItem: TListItem;
begin
  LVRevisions.Items.Clear;
  CallSVN('\svn.exe', 'log ' + AFile, ExtractFilePath(AFile));
  Idx := 2;
  while Idx < FMessages.LBMessages.Items.Count - 1 do
  begin
    Str := FMessages.LBMessages.Items[Idx];
    ListItem := LVRevisions.Items.Add;
    Posi := Pos(' | ', Str);
    Rev := Copy(Str, 2, Posi - 2);
    ListItem.Caption := Rev;
    Delete(Str, 1, Posi + 2);
    Posi := Pos(' | ', Str);
    Aut := Copy(Str, 1, Posi - 1);
    ListItem.SubItems.Add(Aut);
    Delete(Str, 1, Posi + 2);
    Posi := Pos(':', Str);
    Delete(Str, Posi + 5, Length(Str));
    ListItem.SubItems.Add(Str);
    AMessage := FMessages.LBMessages.Items[Idx + 2];
    ListItem.SubItems.Add(AMessage);
    Inc(Idx, 4);
  end;
end;

function TFSubversion.GetRevision: string;
begin
  var
  Idx := LVRevisions.ItemIndex;
  Result := LVRevisions.Items[Idx].Caption;
end;

function TFSubversion.IsRepository(const Dir: string): Boolean;
begin
  Result := False;
  var
  Str := WithTrailingSlash(Dir) + 'README.txt';
  if FileExists(Str) then
  begin
    var
    StringList := TStringList.Create;
    try
      StringList.LoadFromFile(Str);
      Result := Pos('This is a Subversion repository;', StringList.Text) > 0;
    finally
      FreeAndNil(StringList);
    end;
  end;
end;

function TFSubversion.GetRepositoryURL(Str: string): string;
begin
  Str := ExcludeTrailingPathDelimiter(ExtractFilePath(Str));
  Result := ToWeb('IE', FConfiguration.SVNRepository + '/' + ToRepository(Str));
end;

procedure TFSubversion.LVRevisionsDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFSubversion.CallSVN(Prog: string; const Call, Dir: string);
begin
  Prog := FConfiguration.SVNFolder + Prog;
  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  FMessages.OutputLineTo(K_Messages, Prog + ' ' + Call);
  Screen.Cursor := crHourGlass;
  if MyJavaCommands.ExecAndWait(Prog, Call, Dir, FConfiguration.TempDir +
    'error.txt', SW_HIDE) then
    FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
  Screen.Cursor := crDefault;
end;

procedure TFSubversion.FormCreate(Sender: TObject);
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

end.
