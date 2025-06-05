unit UDlgSearch;

interface

uses
  StdCtrls,
  ExtCtrls,
  Forms,
  Controls,
  System.Classes,
  USynEditEx;

type
  TFSearch = class(TForm)
    CBSearchText: TComboBox;
    RGSearchDirection: TRadioGroup;
    BOK: TButton;
    BCancel: TButton;
    LSearch: TLabel;
    GBSearchOptions: TGroupBox;
    CBSearchCaseSensitive: TCheckBox;
    CBSearchWholeWords: TCheckBox;
    CBSearchFromCursor: TCheckBox;
    CBSearchSelectionOnly: TCheckBox;
    CBSearchRegSearch: TCheckBox;
    LSearchRegSearch: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LSearchRegSearchMouseEnter(Sender: TObject);
    procedure LSearchRegSearchMouseLeave(Sender: TObject);
    procedure LSearchRegSearchClick(Sender: TObject);
  public
    procedure ShowSearchDialog(Editor: TSynEditEx);
  end;

implementation

uses
  JvGnugettext,
  UJava,
  UUtils,
  URegExSearch,
  USearchOptions;

{$R *.DFM}

procedure TFSearch.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFSearch.ShowSearchDialog(Editor: TSynEditEx);
begin
  MySearchOptions.LoadToForm(Self);
  if not MySearchOptions.RegEx then
    CBSearchText.Text := Editor.GetSearchText(CBSearchText.Text);
  if Editor.SelEnd = Editor.SelStart then
    CBSearchSelectionOnly.Checked := False;
  if (Editor.CaretX = 1) and (Editor.CaretY = 1) then
    RGSearchDirection.ItemIndex := 0;
  ActiveControl := CBSearchText;
  if ShowModal = mrOk then
  begin
    ComboBoxAdd(CBSearchText);
    MySearchOptions.SaveFromForm(Self);
    if MySearchOptions.RegEx then
    begin
      if not Assigned(MyRegExSearch) then
        MyRegExSearch := TRegExSearch.Create;
      MyRegExSearch.PrepareRegSearch(Editor);
    end;
    FJava.DoSearchReplaceText(Editor, False);
    MySearchOptions.SystemFromCursor := True;
  end;
end;

procedure TFSearch.LSearchRegSearchMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TFSearch.LSearchRegSearchMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TFSearch.LSearchRegSearchClick(Sender: TObject);
begin
  MySearchOptions.ShowRegSearchHelp;
end;

end.
