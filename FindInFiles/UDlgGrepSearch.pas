unit UDlgGrepSearch;

interface

uses
  Forms, StdCtrls, UFrmEditor, Vcl.Controls, System.Classes;

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

uses SysUtils, Graphics, Dialogs,
     UJava, UConfiguration, UUtils,
     JvGnugettext, UGrepResults, USearchOptions, UMessages;

{$R *.DFM}


procedure TFGrepSearch.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  if myGrepResults = nil then
    myGrepResults:= TFGrepResults.Create(FMessages.TVSearch);
end;

procedure TFGrepSearch.DirEnable(New: Boolean);
begin
  CBDirectory.Enabled:= New;
  CBFilemask.Enabled:= New;
  CBInclude.Enabled:= New;
  if New then begin
    cbDirectory.Color:= clWindow;
    cbFilemask.Color := clWindow
  end else begin
    cbDirectory.Color:= clBtnFace;
    cbFilemask.Color := clBtnFace;
  end;
  CBReplaceText.Enabled:= cbReplace.Checked;
  if CBReplaceText.Enabled
    then CBReplaceText.Color:= clWindow
    else CBReplaceText.Color:= clBtnFace;
end;

procedure TFGrepSearch.RBDirectoriesClick(Sender: TObject);
begin
  DirEnable(rbDirectories.Checked);
end;

procedure TFGrepSearch.BSelectClick(Sender: TObject);
  var Dir: string;
begin
  Dir:= FConfiguration.ExtendPath(cbDirectory);
  if not SysUtils.DirectoryExists(Dir) then
    if assigned(FJava.EditorForm)
      then Dir:= ExtractFilePath(FJava.EditorForm.Pathname)
      else Dir:= '';
  {$WARNINGS OFF}
  FConfiguration.FolderDialog.DefaultFolder:= Dir;
  if FConfiguration.FolderDialog.Execute then begin
    FConfiguration.ShortenPath(cbDirectory, FConfiguration.FolderDialog.Filename);
    RBDirectories.Checked:= true;
    DirEnable(true);
  end;
   {$WARNINGS ON}
  if CanFocus then SetFocus;
end;

procedure TFGrepSearch.ShowSearchDialog(Editform: TFEditform);
begin
  mySearchOptions.LoadToForm(Self);
  //if assigned(Editform) then
  //  CBDirectory.Text:= ExtractFilePath(Editform.Pathname);
  if assigned(Editform) and not mySearchOptions.RegEx then
    CBSearchText.Text:= Editform.Editor.GetSearchText(CBSearchText.Text);
  rbCurrentOnly.Enabled:= assigned(Editform);
  rbOpenFiles.Enabled  := FJava.hasEditforms;
  if rbCurrentOnly.Checked and not rbCurrentOnly.Enabled then
    rbOpenFiles.Checked:= true;
  if rbOpenFiles.Checked and not rbOpenFiles.Enabled then
    rbDirectories.Checked:= true;
  DirEnable(rbDirectories.Checked);
  ActiveControl:= CBSearchText;

  if ShowModal = mrOK then begin
    ComboBoxAdd(cbSearchText);
    ComboBoxAdd(cbReplaceText);
    FConfiguration.ComboBoxAddEx(cbDirectory);
    ComboBoxAdd(cbFilemask);
    mySearchOptions.SaveFromForm(Self);
    if mySearchOptions.GrepAction = 3 then begin
      mySearchOptions.Directory:= withTrailingSlash(MySearchOptions.Directory);
      if mySearchOptions.Directory = '\' then exit;
      mySearchOptions.Filemask:= Trim(MySearchOptions.Filemask);
    end;
    mySearchOptions.SaveSearchOptions;

    FMessages.ShowIt;
    FMessages.SetMinHeight(200);
    FMessages.ShowTab(3);
    if mySearchOptions.SearchText <> '' then
      myGrepResults.Execute;
  end;
end;

procedure TFGrepSearch.LSearchRegSearchMouseEnter(Sender: TObject);
begin
  Screen.Cursor:= crHandpoint;
end;

procedure TFGrepSearch.LSearchRegSearchMouseLeave(Sender: TObject);
begin
  Screen.Cursor:= crDefault;
end;

procedure TFGrepSearch.LSearchRegSearchClick(Sender: TObject);
begin
  mySearchOptions.ShowRegSearchHelp;
end;

end.

