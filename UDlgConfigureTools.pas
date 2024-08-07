unit UDlgConfigureTools;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, Buttons, Vcl.Controls;

type
  TFConfigureTools = class(TForm)
    LBTools: TListBox;
    LTools: TLabel;
    LTitle: TLabel;
    ETitle: TEdit;
    EProgram: TEdit;
    LProgram: TLabel;
    LParameter: TLabel;
    EParameter: TEdit;
    BProgram: TButton;
    BParameter: TButton;
    OpenDialog: TOpenDialog;
    BDelete: TButton;
    BSave: TButton;
    SPUp: TSpeedButton;
    SPDown: TSpeedButton;
    BClose: TButton;
    CBWait: TCheckBox;
    BNew: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BParameterClick(Sender: TObject);
    procedure BProgramClick(Sender: TObject);
    procedure LBToolsClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure SPUpClick(Sender: TObject);
    procedure SPDownClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure ToolsMenuClick(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
  private
    Titles: TStringlist;
    Programs: TStringlist;
    Parameters: TStringlist;
    Waits: TStringlist;
    MaxTools: integer;
    ToolsOffset: integer;
  public
    procedure SelectTool(i: integer);
    procedure SaveTools;
    procedure MakeToolsMenu;
    procedure DoToolsCommand(const Title, Prog, dir, Param, Wait: string);
  end;

implementation

{$R *.dfm}

uses Windows, SysUtils, Menus, JvGnugettext, SpTBXItem,
     UJava, UMessages, UJavaCommands, StrUtils, UUtils, UConfiguration;

procedure TFConfigureTools.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToolsOffset:= FJava.MITools.Count;
  Titles:= TStringList.Create;
  Programs:= TStringlist.Create;
  Parameters:= TStringlist.Create;
  Waits:= TStringlist.Create;
  MaxTools:= FConfiguration.ReadIntegerU('Tools', 'MaxTools', 0);
  for var i:= 1 to MaxTools do begin
    Titles.Add(FConfiguration.ReadStringU('Tools', 'Title' + IntToStr(i), ''));
    Programs.Add(FConfiguration.ReadStringU('Tools', 'Program' + IntToStr(i), ''));
    Parameters.Add(FConfiguration.ReadStringU('Tools', 'Parameter' + IntToStr(i), ''));
    Waits.Add(FConfiguration.ReadStringU('Tools', 'Wait' + IntToStr(i), ''));
  end;
  if MaxTools > 0 then begin
    LBTools.Items:= Titles;
    SelectTool(0);
    MakeToolsMenu;
  end;
end;

procedure TFConfigureTools.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Titles);
  FreeAndNil(Programs);
  FreeAndNil(Parameters);
  FreeAndNil(Waits);
end;

procedure TFConfigureTools.BCloseClick(Sender: TObject);
begin
  Close;
  MakeToolsMenu;
  SaveTools;
end;

procedure TFConfigureTools.BDeleteClick(Sender: TObject);
begin
  var i:= LBTools.ItemIndex;
  if (0 <= i) and (i < LBTools.Count) then begin
    Titles.Delete(i);
    Programs.Delete(i);
    Parameters.Delete(i);
    Waits.Delete(i);
    Dec(MaxTools);
    LBTools.Items:= Titles;
    ETitle.Text:= '';
    EProgram.Text:= '';
    EParameter.Text:= '';
    CBWait.Checked:= false;
    SelectTool(i);
  end;
end;

procedure TFConfigureTools.BNewClick(Sender: TObject);
begin
  ETitle.Text:= '';
  EProgram.Text:= '';
  EParameter.Text:= '%ACTIVEWINDOW%';
  CBWait.Checked:= false;
end;

procedure TFConfigureTools.BParameterClick(Sender: TObject);
begin
  EParameter.Text:= '%ACTIVEWINDOW%';
end;

procedure TFConfigureTools.BProgramClick(Sender: TObject);
begin
  with OpenDialog do begin
    FilterIndex:= 1;
    InitialDir:= GetEnvironmentVariable('PROGRAMFILES');
    if Execute then
      EProgram.Text:= Filename;
  end;
end;

procedure TFConfigureTools.BSaveClick(Sender: TObject);
begin
  if (ETitle.Text <> '') and (EProgram.Text <> '') then begin
    var i:= LBTools.Items.IndexOf(ETitle.Text);
    if i >= 0 then begin // Update
      Programs.Strings[i]:= EProgram.Text;
      Parameters.Strings[i]:= EParameter.Text;
      Waits.Strings[i]:= BoolToStr(CBWait.Checked);
    end else begin
      Titles.Add(ETitle.Text);
      Programs.Add(EProgram.Text);
      Parameters.Add(EParameter.Text);
      Waits.Add(BoolToStr(CBWait.Checked));
      inc(MaxTools);
      LBTools.Items:= Titles;
    end;
  end;
end;

procedure TFConfigureTools.MakeToolsMenu;
begin
  var i:= FJava.MITools.Count;
  while i > ToolsOffset do begin
    FJava.MITools.Delete(i-1);
    dec(i);
  end;
  for i:= 1 to MaxTools do begin
    var NewItem:= TSpTBXItem.Create(Self);
    NewItem.Caption:= Titles[i-1];
    NewItem.Tag:= i-1;
    NewItem.OnClick:= ToolsMenuClick;
    FJava.MITools.Add(NewItem);
  end;
end;

procedure TFConfigureTools.LBToolsClick(Sender: TObject);
begin
  SelectTool(LBTools.ItemIndex);
end;

procedure TFConfigureTools.SelectTool(i: integer);
begin
  if (0 <= i) and (i < LBTools.Count) then begin
    LBTools.ItemIndex:= i;
    ETitle.Text:= Titles.Strings[i];
    EProgram.Text:= Programs.Strings[i];
    EParameter.Text:= Parameters.Strings[i];
    CBWait.Checked:= (Waits.Strings[i] = '-1');
  end;
end;

procedure TFConfigureTools.SPDownClick(Sender: TObject);
begin
  var i:= LBTools.ItemIndex;
  if (0 <= i) and (i < LBTools.Count-1) then begin
    Titles.Exchange(i, i+1);
    Programs.Exchange(i, i+1);
    Parameters.Exchange(i, i+1);
    LBTools.Items:= Titles;
    SelectTool(i+1);
  end;
end;

procedure TFConfigureTools.SPUpClick(Sender: TObject);
begin
  var i:= LBTools.ItemIndex;
  if (1 <= i) and (i < LBTools.Count) then begin
    Titles.Exchange(i, i-1);
    Programs.Exchange(i, i-1);
    Parameters.Exchange(i, i-1);
    LBTools.Items:= Titles;
    SelectTool(i-1);
  end;
end;

procedure TFConfigureTools.SaveTools;
begin
  FConfiguration.WriteIntegerU('Tools', 'MaxTools', MaxTools);
  for var i:= 1 to MaxTools do begin
    FConfiguration.WriteStringU('Tools', 'Title' + IntToStr(i), Titles[i-1]);
    FConfiguration.WriteStringU('Tools', 'Program' + IntToStr(i), Programs[i-1]);
    FConfiguration.WriteStringU('Tools', 'Parameter' + IntToStr(i), Parameters[i-1]);
    FConfiguration.WriteStringU('Tools', 'Wait' + IntToStr(i), Waits[i-1]);
  end;
end;

procedure TFConfigureTools.ToolsMenuClick(Sender: TObject);
  var i: integer; param, AktDir, pname: string;
begin
  i:= (Sender as TSpTBXItem).Tag;
  param:= Parameters[i];
  if Assigned(FJava.EditorForm) then begin
    if FJava.EditorForm.Modified then FJava.EditorForm.Save(false);
    pname:= FJava.EditorForm.Pathname;
    param:= ReplaceStr(param, '%ACTIVEWINDOW%', HideBlanks(pname));
    AktDir:= ExtractFilepath(pname);
  end else begin
    param:= ReplaceStr(param, '%ACTIVEWINDOW%', '');
    AktDir:= GetTempDir;
  end;
  DoToolsCommand(Titles[i], Programs[i], AktDir, param, Waits[i]);
end;

procedure TFConfigureTools.DoToolsCommand(const Title, Prog, dir, Param, Wait: string);
  const K_Messages = 4;
begin
  FMessages.ShowIt;
  FMessages.DeleteTab(K_Messages);
  FMessages.ShowTab(K_Messages);
  try
    Screen.Cursor:= crHourGlass;
    var ErrFile:= FConfiguration.TempDir + 'error.txt';
    FMessages.OutputLineTo(K_Messages, Title + ' ' + Param);
    if Wait = '-1' then begin
      myJavaCommands.ExecAndWait(Prog, Param, dir, ErrFile, SW_Show);
      FMessages.ShowMessages(ErrFile);
    end else begin
      SetCurrentDir(dir);
      myJavaCommands.ExecWithoutWait(Prog, Param, dir, SW_Show);
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

end.
