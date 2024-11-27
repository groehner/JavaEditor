unit USearchOptions;

interface

uses Forms;

type
    TSearchOptions = class         // Tag number of component
    public
      SearchText: string;          // Tag = 1
      SearchTextHistory: string;   // Tag = 1
      ReplaceText: string;         // Tag = 2
      ReplaceTextHistory: string;  // Tag = 2

      Replace: Boolean;            // Tag = 3
      CaseSensitive: boolean;      // Tag = 4
      WholeWords: boolean;         // Tag = 5
      FromCursor: boolean;         // Tag = 6, user setting
      SelectionOnly: boolean;      // Tag = 7
      RegEx: boolean;              // Tag = 8
      Backwards: boolean;          // Tag = 9
      fFromCursor: boolean;        // system setting for SearchAgain

      GrepAction: integer;         // Tag = 10, 11, 12
      Directory: string;           // Tag = 13
      DirectoryHistory: string;    // Tag = 13
      Filemask: string;            // Tag = 14
      FilemaskHistory: string;     // Tag = 14
      IncludeSubdirs: boolean;     // Tag = 15
      constructor Create;
      destructor Destroy; override;
      procedure SaveSearchOptions;
      procedure LoadToForm(Form: TForm);
      procedure SaveFromForm(Form: TForm);
      procedure ShowRegSearchHelp;
    end;

var
  mySearchOptions: TSearchOptions;

implementation

uses Classes, Windows, StdCtrls, ExtCtrls, SysUtils, StrUtils,
     UConfiguration, UJavaCommands, UUtils;

constructor TSearchOptions.create;
begin
  SearchText:= FConfiguration.ReadStringU('SearchReplace', 'SearchText', '');
  SearchTextHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'SearchTextHistory', ''));
  ReplaceText:= FConfiguration.ReadStringU('SearchReplace', 'ReplaceText', '');
  ReplaceTextHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'ReplaceTextHistory', ''));

  Replace:= FConfiguration.ReadBoolU('SearchReplace', 'Replace', false);
  CaseSensitive:= FConfiguration.ReadBoolU('SearchReplace', 'CaseSensitive', false);
  WholeWords:= FConfiguration.ReadBoolU('SearchReplace', 'WholeWords', false);
  FromCursor:= FConfiguration.ReadBoolU('SearchReplace', 'FromCursor', false);
  SelectionOnly:= FConfiguration.ReadBoolU('SearchReplace', 'SelectionOnly', false);
  RegEx:= FConfiguration.ReadBoolU('SearchReplace', 'RegEx', false);
  Backwards:= false;

  GrepAction:= FConfiguration.ReadIntegerU('SearchReplace', 'Where', 1);
  Directory:= FConfiguration.ReadStringU('SearchReplace', 'Directory', '');
  DirectoryHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'DirectoryHistory', ''));
  Filemask:= FConfiguration.ReadStringU('SearchReplace', 'Filemask', '*.java');
  FilemaskHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'FilemaskHistory', ''));
  IncludeSubdirs:= FConfiguration.ReadBoolU('SearchReplace', 'IncludeSubdirs', true);
end;

destructor TSearchOptions.Destroy;
begin
  inherited;
end;

procedure TSearchOptions.LoadToForm(Form: TForm);
  var i: integer; Component: TComponent;
begin
  with Form do
    for i:= 0 to ComponentCount -1 do begin
      Component:= Components[i];
      case Component.Tag of
        1: with Component as TComboBox do begin
             Text:= SearchText;
             Items.Text:= SearchTextHistory;
           end;
        2: with Component as TComboBox do begin
             Text:= ReplaceText;
             Items.Text:= ReplaceTextHistory;
           end;
        3: (Component as TCheckBox).Checked:= Replace;
        4: (Component as TCheckBox).Checked:= CaseSensitive;
        5: (Component as TCheckBox).Checked:= WholeWords;
        6: (Component as TCheckBox).Checked:= FromCursor;
        7: (Component as TCheckBox).Checked:= SelectionOnly;
        8: (Component as TCheckBox).Checked:= RegEx;
        9: if Backwards
             then (Component as TRadioGroup).ItemIndex:= 1
             else (Component as TRadioGroup).ItemIndex:= 0;
       10: (Component as TRadioButton).Checked:= (GrepAction = 1);
       11: (Component as TRadioButton).Checked:= (GrepAction = 2);
       12: (Component as TRadioButton).Checked:= (GrepAction = 3);
       13: with Component as TComboBox do begin
             Text:= Directory;
             Items.Text:= DirectoryHistory;
             FConfiguration.ShortenPath(Component as TComboBox, Directory);
           end;
       14: with Component as TComboBox do begin
             Text:= Filemask;
             Items.Text:= FilemaskHistory;
           end;
       15: (Component as TCheckBox).Checked:= IncludeSubdirs;
    end;
  end;
end;

procedure TSearchOptions.SaveFromForm(Form: TForm);
  var i: integer; Component: TComponent;
begin
  with Form do
    for i:= 0 to ComponentCount -1 do begin
      Component:= Components[i];
      case Component.Tag of
        1: with Component as TComboBox do begin
             SearchText:= Text;
             SearchTextHistory:= Items.Text;
           end;
        2: with Component as TComboBox do begin
             ReplaceText:= Text;
             ReplaceTextHistory:= Items.Text;
           end;
        3: Replace:= (Component as TCheckBox).Checked;
        4: CaseSensitive:= (Component as TCheckBox).Checked;
        5: WholeWords:= (Component as TCheckBox).Checked;
        6: FromCursor:= (Component as TCheckBox).Checked;
        7: SelectionOnly:= (Component as TCheckBox).Checked;
        8: RegEx:= (Component as TCheckBox).Checked;
        9: Backwards:= ((Component as TRadioGroup).ItemIndex = 1);
       10: if (Component as TRadioButton).Checked then GrepAction:= 1;
       11: if (Component as TRadioButton).Checked then GrepAction:= 2;
       12: if (Component as TRadioButton).Checked then GrepAction:= 3;
       13: with Component as TComboBox do begin
             Directory:= FConfiguration.ExtendPath(Component as TComboBox);
             DirectoryHistory:= Items.Text;
           end;
       14: with Component as TComboBox do begin
             Filemask:= Text;
             FilemaskHistory:= Items.Text;
           end;
       15: IncludeSubdirs:= (Component as TCheckBox).Checked;
    end;
  end;
  if SelectionOnly and FromCursor then FromCursor:= false;
  fFromCursor:= FromCursor;
end;

procedure TSearchOptions.SaveSearchOptions;
begin
  FConfiguration.WriteStringU('SearchReplace', 'SearchText', SearchText);
  FConfiguration.WriteStringU('SearchReplace', 'SearchTextHistory', SaveComboBoxItems(SearchTextHistory));
  FConfiguration.WriteStringU('SearchReplace', 'ReplaceText', ReplaceText);
  FConfiguration.WriteStringU('SearchReplace', 'ReplaceTextHistory', SaveComboBoxItems(ReplaceTextHistory));

  FConfiguration.WriteBoolU('SearchReplace', 'Replace', Replace);
  FConfiguration.WriteBoolU('SearchReplace', 'CaseSensitive', CaseSensitive);
  FConfiguration.WriteBoolU('SearchReplace', 'WholeWords', WholeWords);
  FConfiguration.WriteBoolU('SearchReplace', 'FromCursor', FromCursor);
  FConfiguration.WriteBoolU('SearchReplace', 'SelectionOnly', SelectionOnly);
  FConfiguration.WriteBoolU('SearchReplace', 'RegEx', RegEx);

  FConfiguration.WriteIntegerU('SearchReplace', 'Where', GrepAction);
  FConfiguration.WriteStringU('SearchReplace', 'Directory', Directory);
  FConfiguration.WriteStringU('SearchReplace', 'DirectoryHistory', SaveComboBoxItems(DirectoryHistory));
  FConfiguration.WriteStringU('SearchReplace', 'Filemask', Filemask);
  FConfiguration.WriteStringU('SearchReplace', 'FilemaskHistory', SaveComboBoxItems(FilemaskHistory));
  FConfiguration.WriteBoolU('SearchReplace', 'IncludeSubdirs', IncludeSubdirs);
end;

procedure TSearchOptions.ShowRegSearchHelp;
  var s: string;
begin
  if LeftStr(FConfiguration.LanguageCode, 2) = 'de'
    then s:= FConfiguration.EditorFolder + 'docs/TRegExpr_DE.pdf'
    else s:= FConfiguration.EditorFolder + 'docs/TRegExpr_EN.pdf';
  myJavaCommands.ShellExecuteFile(s, '', '', SW_SHOWNORMAL)
end;

end.

