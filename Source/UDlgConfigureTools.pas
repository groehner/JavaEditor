unit UDlgConfigureTools;

interface

uses
  Classes,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  Vcl.Controls;

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
    FTitles: TStringList;
    FPrograms: TStringList;
    FParameters: TStringList;
    FWaits: TStringList;
    FMaxTools: Integer;
    FToolsOffset: Integer;
  public
    procedure SelectTool(Int: Integer);
    procedure SaveTools;
    procedure MakeToolsMenu;
    procedure DoToolsCommand(const Title, Prog, Dir, Param, Wait: string);
  end;

implementation

{$R *.dfm}

uses
  Windows,
  SysUtils,
  StrUtils,
  JvGnugettext,
  SpTBXItem,
  UJava,
  UMessages,
  UJavaCommands,
  UUtils,
  UConfiguration;

procedure TFConfigureTools.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FToolsOffset := FJava.MITools.Count;
  FTitles := TStringList.Create;
  FPrograms := TStringList.Create;
  FParameters := TStringList.Create;
  FWaits := TStringList.Create;
  FMaxTools := FConfiguration.ReadIntegerU('Tools', 'MaxTools', 0);
  for var I := 1 to FMaxTools do
  begin
    FTitles.Add(FConfiguration.ReadStringU('Tools',
      'Title' + IntToStr(I), ''));
    FPrograms.Add(FConfiguration.ReadStringU('Tools',
      'Program' + IntToStr(I), ''));
    FParameters.Add(FConfiguration.ReadStringU('Tools',
      'Parameter' + IntToStr(I), ''));
    FWaits.Add(FConfiguration.ReadStringU('Tools',
      'Wait' + IntToStr(I), ''));
  end;
  if FMaxTools > 0 then
  begin
    LBTools.Items := FTitles;
    SelectTool(0);
    MakeToolsMenu;
  end;
end;

procedure TFConfigureTools.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTitles);
  FreeAndNil(FPrograms);
  FreeAndNil(FParameters);
  FreeAndNil(FWaits);
end;

procedure TFConfigureTools.BCloseClick(Sender: TObject);
begin
  Close;
  MakeToolsMenu;
  SaveTools;
end;

procedure TFConfigureTools.BDeleteClick(Sender: TObject);
begin
  var Int := LBTools.ItemIndex;
  if (0 <= Int) and (Int < LBTools.Count) then
  begin
    FTitles.Delete(Int);
    FPrograms.Delete(Int);
    FParameters.Delete(Int);
    FWaits.Delete(Int);
    Dec(FMaxTools);
    LBTools.Items := FTitles;
    ETitle.Text := '';
    EProgram.Text := '';
    EParameter.Text := '';
    CBWait.Checked := False;
    SelectTool(Int);
  end;
end;

procedure TFConfigureTools.BNewClick(Sender: TObject);
begin
  ETitle.Text := '';
  EProgram.Text := '';
  EParameter.Text := '%ACTIVEWINDOW%';
  CBWait.Checked := False;
end;

procedure TFConfigureTools.BParameterClick(Sender: TObject);
begin
  EParameter.Text := '%ACTIVEWINDOW%';
end;

procedure TFConfigureTools.BProgramClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    FilterIndex := 1;
    InitialDir := GetEnvironmentVariable('PROGRAMFILES');
    if Execute then
      EProgram.Text := FileName;
  end;
end;

procedure TFConfigureTools.BSaveClick(Sender: TObject);
begin
  if (ETitle.Text <> '') and (EProgram.Text <> '') then
  begin
    var Int := LBTools.Items.IndexOf(ETitle.Text);
    if Int >= 0 then
    begin // Update
      FPrograms[Int] := EProgram.Text;
      FParameters[Int] := EParameter.Text;
      FWaits[Int] := BoolToStr(CBWait.Checked);
    end
    else
    begin
      FTitles.Add(ETitle.Text);
      FPrograms.Add(EProgram.Text);
      FParameters.Add(EParameter.Text);
      FWaits.Add(BoolToStr(CBWait.Checked));
      Inc(FMaxTools);
      LBTools.Items := FTitles;
    end;
  end;
end;

procedure TFConfigureTools.MakeToolsMenu;
begin
  var Int := FJava.MITools.Count;
  while Int > FToolsOffset do
  begin
    FJava.MITools.Delete(Int - 1);
    Dec(Int);
  end;
  for var I := 1 to FMaxTools do
  begin
    var NewItem := TSpTBXItem.Create(Self);
    NewItem.Caption := FTitles[I - 1];
    NewItem.Tag := I - 1;
    NewItem.OnClick := ToolsMenuClick;
    FJava.MITools.Add(NewItem);
  end;
end;

procedure TFConfigureTools.LBToolsClick(Sender: TObject);
begin
  SelectTool(LBTools.ItemIndex);
end;

procedure TFConfigureTools.SelectTool(Int: Integer);
begin
  if (0 <= Int) and (Int < LBTools.Count) then
  begin
    LBTools.ItemIndex := Int;
    ETitle.Text := FTitles[Int];
    EProgram.Text := FPrograms[Int];
    EParameter.Text := FParameters[Int];
    CBWait.Checked := (FWaits[Int] = '-1');
  end;
end;

procedure TFConfigureTools.SPDownClick(Sender: TObject);
begin
  var Int := LBTools.ItemIndex;
  if (0 <= Int) and (Int < LBTools.Count - 1) then
  begin
    FTitles.Exchange(Int, Int + 1);
    FPrograms.Exchange(Int, Int + 1);
    FParameters.Exchange(Int, Int + 1);
    LBTools.Items := FTitles;
    SelectTool(Int + 1);
  end;
end;

procedure TFConfigureTools.SPUpClick(Sender: TObject);
begin
  var Int := LBTools.ItemIndex;
  if (1 <= Int) and (Int < LBTools.Count) then
  begin
    FTitles.Exchange(Int, Int - 1);
    FPrograms.Exchange(Int, Int - 1);
    FParameters.Exchange(Int, Int - 1);
    LBTools.Items := FTitles;
    SelectTool(Int - 1);
  end;
end;

procedure TFConfigureTools.SaveTools;
begin
  FConfiguration.WriteIntegerU('Tools', 'MaxTools', FMaxTools);
  for var I := 1 to FMaxTools do
  begin
    FConfiguration.WriteStringU('Tools', 'Title' + IntToStr(I),
      FTitles[I - 1]);
    FConfiguration.WriteStringU('Tools', 'Program' + IntToStr(I),
      FPrograms[I - 1]);
    FConfiguration.WriteStringU('Tools', 'Parameter' + IntToStr(I),
      FParameters[I - 1]);
    FConfiguration.WriteStringU('Tools', 'Wait' + IntToStr(I),
      FWaits[I - 1]);
  end;
end;

procedure TFConfigureTools.ToolsMenuClick(Sender: TObject);
var
  Param, AktDir, Pname: string;
begin
  var Int := (Sender as TSpTBXItem).Tag;
  Param := FParameters[Int];
  if Assigned(FJava.EditorForm) then
  begin
    if FJava.EditorForm.Modified then
      FJava.EditorForm.Save(False);
    Pname := FJava.EditorForm.Pathname;
    Param := ReplaceStr(Param, '%ACTIVEWINDOW%', HideBlanks(Pname));
    AktDir := ExtractFilePath(Pname);
  end
  else
  begin
    Param := ReplaceStr(Param, '%ACTIVEWINDOW%', '');
    AktDir := GetTempDir;
  end;
  DoToolsCommand(FTitles[Int], FPrograms[Int], AktDir, Param, FWaits[Int]);
end;

procedure TFConfigureTools.DoToolsCommand(const Title, Prog, Dir, Param,
  Wait: string);
const
  K_Messages = 4;
begin
  FMessages.ShowIt;
  FMessages.DeleteTab(K_Messages);
  FMessages.ShowTab(K_Messages);
  try
    Screen.Cursor := crHourGlass;
    var
    ErrFile := FConfiguration.TempDir + 'error.txt';
    FMessages.OutputLineTo(K_Messages, Title + ' ' + Param);
    if Wait = '-1' then
    begin
      MyJavaCommands.ExecAndWait(Prog, Param, Dir, ErrFile, SW_SHOW);
      FMessages.ShowMessages(ErrFile);
    end
    else
    begin
      SetCurrentDir(Dir);
      MyJavaCommands.ExecWithoutWait(Prog, Param, Dir, SW_SHOW);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
