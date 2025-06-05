unit UDlgGrepSearch;

interface

uses
  Forms,
  StdCtrls,
  Vcl.Controls,
  System.Classes,
  UEditorForm;

type
  TFGrepSearch = class(TForm)
    LSearch: TLabel;
    GBSearchOptions: TGroupBox;
    CBSearchCaseSensitive: TCheckBox;
    GBWhere: TGroupBox;
    RBOpenFiles: TRadioButton;
    RBDirectories: TRadioButton;
    GBDirectories: TGroupBox;
    LDirectory: TLabel;
    CBInclude: TCheckBox;
    BOK: TButton;
    BCancel: TButton;
    CBSearchWholeWords: TCheckBox;
    RBCurrentOnly: TRadioButton;
    CBSearchRegSearch: TCheckBox;
    CBDirectory: TComboBox;
    CBSearchText: TComboBox;
    BSelect: TButton;
    LFilemask: TLabel;
    CBFileMask: TComboBox;
    LReplace: TLabel;
    CBReplaceText: TComboBox;
    CBReplace: TCheckBox;
    LSearchRegSearch: TLabel;
    CBExcludeCommentsAndStrings: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RBDirectoriesClick(Sender: TObject);
    procedure BSelectClick(Sender: TObject);
    procedure LSearchRegSearchMouseEnter(Sender: TObject);
    procedure LSearchRegSearchMouseLeave(Sender: TObject);
    procedure LSearchRegSearchClick(Sender: TObject);
  public
    procedure DirEnable(New: Boolean);
    procedure ShowSearchDialog(EditForm: TFEditForm);
  end;

implementation

uses
  SysUtils,
  Graphics,
  JvGnugettext,
  UJava,
  UConfiguration,
  UUtils,
  UGrepResults,
  USearchOptions,
  UMessages;

{$R *.DFM}

procedure TFGrepSearch.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  if not Assigned(myGrepResults) then
    myGrepResults:= TFGrepResults.Create(FMessages.TVSearch);
end;

procedure TFGrepSearch.DirEnable(New: Boolean);
begin
  CBDirectory.Enabled:= New;
  CBFileMask.Enabled:= New;
  CBInclude.Enabled:= New;
  if New then begin
    CBDirectory.Color:= clWindow;
    CBFileMask.Color := clWindow;
  end else begin
    CBDirectory.Color:= clBtnFace;
    CBFileMask.Color := clBtnFace;
  end;
  CBReplaceText.Enabled:= CBReplace.Checked;
  if CBReplaceText.Enabled
    then CBReplaceText.Color:= clWindow
    else CBReplaceText.Color:= clBtnFace;
end;

procedure TFGrepSearch.RBDirectoriesClick(Sender: TObject);
begin
  DirEnable(RBDirectories.Checked);
end;

procedure TFGrepSearch.BSelectClick(Sender: TObject);
  var Dir: string;
begin
  Dir:= FConfiguration.ExtendPath(CBDirectory);
  if not SysUtils.DirectoryExists(Dir) then
    if Assigned(FJava.EditorForm)
      then Dir:= ExtractFilePath(FJava.EditorForm.Pathname)
      else Dir:= '';
  FConfiguration.FolderDialog.DefaultFolder:= Dir;
  if FConfiguration.FolderDialog.Execute then begin
    FConfiguration.ShortenPath(CBDirectory, FConfiguration.FolderDialog.FileName);
    RBDirectories.Checked:= True;
    DirEnable(True);
  end;
  if CanFocus then SetFocus;
end;

procedure TFGrepSearch.ShowSearchDialog(Editform: TFEditForm);
begin
  MySearchOptions.LoadToForm(Self);
  if Assigned(Editform) and not MySearchOptions.RegEx then
    CBSearchText.Text:= Editform.Editor.GetSearchText(CBSearchText.Text);
  RBCurrentOnly.Enabled:= Assigned(Editform);
  RBOpenFiles.Enabled := FJava.HasEditforms;
  if RBCurrentOnly.Checked and not RBCurrentOnly.Enabled then
    RBOpenFiles.Checked:= True;
  if RBOpenFiles.Checked and not RBOpenFiles.Enabled then
    RBDirectories.Checked:= True;
  DirEnable(RBDirectories.Checked);
  ActiveControl:= CBSearchText;

  if ShowModal = mrOk then begin
    ComboBoxAdd(CBSearchText);
    ComboBoxAdd(CBReplaceText);
    FConfiguration.ComboBoxAddEx(CBDirectory);
    ComboBoxAdd(CBFileMask);
    MySearchOptions.SaveFromForm(Self);
    if MySearchOptions.GrepAction = 3 then begin
      MySearchOptions.Directory:= WithTrailingSlash(MySearchOptions.Directory);
      if MySearchOptions.Directory = '\' then Exit;
      MySearchOptions.Filemask:= Trim(MySearchOptions.Filemask);
    end;
    MySearchOptions.SaveSearchOptions;

    FMessages.ShowIt;
    FMessages.SetMinHeight(200);
    FMessages.ShowTab(3);
    if MySearchOptions.SearchText <> '' then
      myGrepResults.Execute;
  end;
end;

procedure TFGrepSearch.LSearchRegSearchMouseEnter(Sender: TObject);
begin
  Screen.Cursor:= crHandPoint;
end;

procedure TFGrepSearch.LSearchRegSearchMouseLeave(Sender: TObject);
begin
  Screen.Cursor:= crDefault;
end;

procedure TFGrepSearch.LSearchRegSearchClick(Sender: TObject);
begin
  MySearchOptions.ShowRegSearchHelp;
end;

end.

