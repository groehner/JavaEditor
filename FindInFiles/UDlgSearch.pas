unit UDlgSearch;

interface

uses
  StdCtrls, ExtCtrls, Forms, Controls, USynEditEx, System.Classes;

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

uses JvGnugettext,
     UJava, UUtils, URegExSearch, USearchOptions;

{$R *.DFM}

procedure TFSearch.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;         

procedure TFSearch.ShowSearchDialog(Editor: TSynEditEx);
begin
  MySearchOptions.LoadToForm(Self);
  if not mySearchOptions.RegEx then CBSearchText.Text:= Editor.GetSearchText(CBSearchText.Text);
  if Editor.SelEnd = Editor.SelStart then CBSearchSelectionOnly.Checked:= false;
  if (Editor.CaretX = 1) and (Editor.CaretY = 1) then RGSearchDirection.ItemIndex:= 0;
  ActiveControl:= CBSearchText;
  if ShowModal = mrOK then begin
    ComboBoxAdd(CBSearchText);
    MySearchOptions.SaveFromForm(Self);
    if MySearchOptions.RegEx then begin
      if MyRegExSearch = nil then MyRegExSearch:= TRegExSearch.Create;
      MyRegExSearch.PrepareRegSearch(Editor);
    end;
    FJava.DoSearchReplaceText(Editor, false);
    MySearchOptions.fFromCursor:= true;
  end;
end;

procedure TFSearch.LSearchRegSearchMouseEnter(Sender: TObject);
begin
  Screen.Cursor:= crHandpoint;
end;

procedure TFSearch.LSearchRegSearchMouseLeave(Sender: TObject);
begin
  Screen.Cursor:= crDefault;
end;

procedure TFSearch.LSearchRegSearchClick(Sender: TObject);
begin
  mySearchOptions.ShowRegSearchHelp;
end;

end.

