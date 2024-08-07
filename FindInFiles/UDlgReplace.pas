unit UDlgReplace;

interface

uses
  StdCtrls, ExtCtrls, Forms, USynEditEx, Vcl.Controls, System.Classes;

type
  TFReplace = class(TForm)
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
    LReplace: TLabel;
    CBReplaceText: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure LSearchRegSearchMouseEnter(Sender: TObject);
    procedure LSearchRegSearchMouseLeave(Sender: TObject);
    procedure LSearchRegSearchClick(Sender: TObject);
  public
    procedure ShowReplaceDialog(Editor: TSynEditEx);
  end;

implementation

uses JvGnugettext,
     UUtils, UJava, URegExSearch, USearchOptions;

{$R *.DFM}

procedure TFReplace.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFReplace.ShowReplaceDialog(Editor: TSynEditEx);
begin
  MySearchOptions.LoadToForm(Self);
  if not mySearchOptions.RegEx then CBSearchText.Text:= Editor.GetSearchText(CBSearchText.Text);
  if Editor.SelEnd = Editor.SelStart then CBSearchSelectionOnly.Checked:= false;
  if (Editor.CaretX = 1) and (Editor.CaretY = 1) then RGSearchDirection.ItemIndex:= 0;
  ActiveControl:= CBSearchText;
  if ShowModal = mrOK then begin
    ComboBoxAdd(CBSearchText);
    ComboBoxAdd(CBReplaceText);
    MySearchOptions.SaveFromForm(Self);
    if MySearchOptions.RegEx then begin
      if MyRegExSearch = nil then MyRegExSearch:= TRegExSearch.Create;
      MyRegExSearch.PrepareRegSearch(Editor);
    end;
    FJava.DoSearchReplaceText(Editor, true);
    MySearchOptions.fFromCursor:= true;
  end;
end;

procedure TFReplace.LSearchRegSearchMouseEnter(Sender: TObject);
begin
  Screen.Cursor:= crHandpoint;
end;

procedure TFReplace.LSearchRegSearchMouseLeave(Sender: TObject);
begin
  Screen.Cursor:= crDefault;
end;

procedure TFReplace.LSearchRegSearchClick(Sender: TObject);
begin
  mySearchOptions.ShowRegSearchHelp;
end;

end.


