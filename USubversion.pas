unit USubversion;

interface

uses
  Forms, StdCtrls, ComCtrls, System.Classes, Vcl.Controls;

type
  TFSubversion = class(TForm)
    LVRevisions: TListView;
    BOK: TButton;
    BCancel: TButton;
    procedure LVRevisionsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure ForFile(const aFile: string);
    function getRevision: string;
    function getRepositoryURL(s: string): string;
    function IsRepository(const dir: string): boolean;
    procedure CallSVN(prog: string; const call, Dir: string);
  end;

var
  FSubversion: TFSubversion = nil;

implementation

uses Windows, SysUtils, JvGnugettext, UStringRessources,
     UConfiguration, UMessages, UJavaCommands, UUtils;

{$R *.dfm}

procedure TFSubversion.ForFile(const aFile: string);
  var m, rev, aut, pa, s: string; p, i: integer; ListItem: TListItem;
begin
  LVRevisions.Items.Clear;
  pa:= ExtractFilePath(aFile);
  CallSVN('\svn.exe', 'log ' + aFile, pa);
  i:= 2;
  while i < FMessages.LBMessages.Items.Count - 1 do begin
    s:= FMessages.LBMessages.Items[i];
    ListItem := LVRevisions.Items.Add;
    p:= Pos(' | ', s);
    rev:= copy(s, 2, p-2);
    ListItem.Caption:= rev;
    delete(s, 1, p + 2);
    p:= Pos(' | ', s);
    aut:= copy(s, 1, p-1);
    ListItem.SubItems.Add(aut);
    delete(s, 1, p + 2);
    p:= Pos(':', s);
    delete(s, p+5, length(s));
    ListItem.SubItems.Add(s);
    m:= FMessages.LBMessages.Items[i+2];
    ListItem.SubItems.Add(m);
    inc(i, 4);
  end;
end;

function TFSubversion.getRevision: string;
begin
  var i:= LVRevisions.ItemIndex;
  Result:= LVRevisions.Items[i].Caption;
end;

function TFSubversion.IsRepository(const dir: string): boolean;
begin
  Result:= false;
  var s:= withTrailingSlash(Dir) + 'README.txt';
  if FileExists(s) then begin
    var SL:= TStringList.Create;
    try
      SL.LoadFromFile(s);
      Result:= Pos('This is a Subversion repository;', SL.Text) > 0;
    finally
      FreeAndNil(SL);
    end;
  end;
end;

function TFSubversion.getRepositoryURL(s: string): string;
begin
  s:= withoutTrailingSlash(ExtractFilePath(s));
  Result:= toWeb('IE', FConfiguration.SVNRepository + '/' + toRepository(s));
end;

procedure TFSubversion.LVRevisionsDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFSubversion.CallSVN(prog: string; const call, dir: string);
begin
  prog:= FConfiguration.SVNFolder + prog;
  FMessages.ShowTab(K_Messages);
  FMessages.DeleteTab(K_Messages);
  FMessages.OutputLineTo(K_Messages, prog + ' ' + call);
  Screen.Cursor:= crHourGlass;
  if myJavaCommands.ExecAndWait(prog, call, dir,
       FConfiguration.TempDir + 'error.txt', SW_Hide) then
    FMessages.ShowMessages(FConfiguration.TempDir + 'error.txt');
  Screen.Cursor:= crDefault;
end;

procedure TFSubversion.FormCreate(Sender: TObject);
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

end.
