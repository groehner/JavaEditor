unit USearchOptions;

interface

uses Forms;

type
    TSearchOptions = class         // Tag number of component
    private
      FBackwards: Boolean;         // Tag = 9
      FCaseSensitive: Boolean;     // Tag = 4
      FDirectory: string;          // Tag = 13
      FDirectoryHistory: string;   // Tag = 13
      FExcludeCommentsAndStrings: Boolean; // Tag = 16
      FFilemask: string;           // Tag = 14
      FFilemaskHistory: string;    // Tag = 14
      FFromCursor: Boolean;        // Tag = 6
      FGrepAction: Integer;        // Tag = 10, 11, 12
      FIncludeSubdirs: Boolean;    // Tag = 15
      FRegEx: Boolean;             // Tag = 8
      FReplace: Boolean;           // Tag = 3
      FReplaceText: string;        // Tag = 2
      FReplaceTextHistory: string; // Tag = 2
      FSearchText: string;         // Tag = 1
      FSearchTextHistory: string;  // Tag = 1
      FSelectionOnly: Boolean;     // Tag = 7
      FSystemFromCursor: Boolean;  // system setting for SearchAgain
      FWholeWords: Boolean;        // Tag = 5
    public
      constructor Create;
      procedure SaveSearchOptions;
      procedure LoadToForm(Form: TForm);
      procedure SaveFromForm(Form: TForm);
      procedure ShowRegSearchHelp;
      property Backwards: Boolean read FBackwards;
      property CaseSensitive: Boolean read FCaseSensitive;
      property Directory: string read FDirectory write  FDirectory;
      property DirectoryHistory: string read FDirectoryHistory write  FDirectoryHistory;
      property ExcludeCommentsAndStrings: Boolean read FExcludeCommentsAndStrings;
      property Filemask: string read FFilemask write FFilemask;
      property FilemaskHistory: string read FFilemaskHistory;
      property FromCursor: Boolean read FFromCursor;
      property GrepAction: Integer read FGrepAction;
      property IncludeSubdirs: Boolean read FIncludeSubdirs;
      property RegEx: Boolean read FRegEx;
      property Replace: Boolean read FReplace;
      property ReplaceText: string read FReplaceText;
      property ReplaceTextHistory: string read FReplaceTextHistory;
      property SearchText: string read FSearchText;
      property SearchTextHistory: string read FSearchTextHistory;
      property SelectionOnly: Boolean read FSelectionOnly;
      property SystemFromCursor: Boolean read FSystemFromCursor write FSystemFromCursor;
      property WholeWords: Boolean read FWholeWords;
    end;

var
  MySearchOptions: TSearchOptions;

implementation

uses
  Classes,
  Windows,
  StdCtrls,
  ExtCtrls,
  StrUtils,
  UConfiguration,
  UJavaCommands,
  UUtils;

constructor TSearchOptions.Create;
begin
  FSearchText:= FConfiguration.ReadStringU('SearchReplace', 'FSearchText', '');
  FSearchTextHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'FSearchTextHistory', ''));
  FReplaceText:= FConfiguration.ReadStringU('SearchReplace', 'FReplaceText', '');
  FReplaceTextHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'FReplaceTextHistory', ''));

  FReplace:= FConfiguration.ReadBoolU('SearchReplace', 'FReplace', False);
  FCaseSensitive:= FConfiguration.ReadBoolU('SearchReplace', 'FCaseSensitive', False);
  FWholeWords:= FConfiguration.ReadBoolU('SearchReplace', 'FWholeWords', False);
  FFromCursor:= FConfiguration.ReadBoolU('SearchReplace', 'FFromCursor', False);
  FSelectionOnly:= FConfiguration.ReadBoolU('SearchReplace', 'FSelectionOnly', False);
  FExcludeCommentsAndStrings:= FConfiguration.ReadBoolU('SearchReplace', 'FExcludeCommentsAndStrings', False);
  FRegEx:= FConfiguration.ReadBoolU('SearchReplace', 'FRegEx', False);
  FBackwards:= False;

  FGrepAction:= FConfiguration.ReadIntegerU('SearchReplace', 'Where', 1);
  FDirectory:= FConfiguration.ReadStringU('SearchReplace', 'FDirectory', '');
  FDirectoryHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'FDirectoryHistory', ''));
  FFilemask:= FConfiguration.ReadStringU('SearchReplace', 'FFilemask', '*.java');
  FFilemaskHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'FFilemaskHistory', ''));
  FIncludeSubdirs:= FConfiguration.ReadBoolU('SearchReplace', 'FIncludeSubdirs', True);
end;

procedure TSearchOptions.LoadToForm(Form: TForm);
  var Component: TComponent;
begin
  with Form do
    for var I:= 0 to ComponentCount -1 do begin
      Component:= Components[I];
      case Component.Tag of
        1: with Component as TComboBox do begin
             Text:= FSearchText;
             Items.Text:= FSearchTextHistory;
           end;
        2: with Component as TComboBox do begin
             Text:= FReplaceText;
             Items.Text:= FReplaceTextHistory;
           end;
        3: (Component as TCheckBox).Checked:= FReplace;
        4: (Component as TCheckBox).Checked:= FCaseSensitive;
        5: (Component as TCheckBox).Checked:= FWholeWords;
        6: (Component as TCheckBox).Checked:= FFromCursor;
        7: (Component as TCheckBox).Checked:= FSelectionOnly;
       16: (Component as TCheckBox).Checked:= FExcludeCommentsAndStrings;
        8: (Component as TCheckBox).Checked:= FRegEx;
        9: if FBackwards
             then (Component as TRadioGroup).ItemIndex:= 1
             else (Component as TRadioGroup).ItemIndex:= 0;
       10: (Component as TRadioButton).Checked:= (FGrepAction = 1);
       11: (Component as TRadioButton).Checked:= (FGrepAction = 2);
       12: (Component as TRadioButton).Checked:= (FGrepAction = 3);
       13: with Component as TComboBox do begin
             Text:= FDirectory;
             Items.Text:= FDirectoryHistory;
             FConfiguration.ShortenPath(Component as TComboBox, FDirectory);
           end;
       14: with Component as TComboBox do begin
             Text:= FFilemask;
             Items.Text:= FFilemaskHistory;
           end;
       15: (Component as TCheckBox).Checked:= FIncludeSubdirs;
    end;
  end;
end;

procedure TSearchOptions.SaveFromForm(Form: TForm);
  var Component: TComponent;
begin
  with Form do
    for var I:= 0 to ComponentCount - 1 do begin
      Component:= Components[I];
      case Component.Tag of
        1: with Component as TComboBox do begin
             FSearchText:= Text;
             FSearchTextHistory:= Items.Text;
           end;
        2: with Component as TComboBox do begin
             FReplaceText:= Text;
             FReplaceTextHistory:= Items.Text;
           end;
        3: FReplace:= (Component as TCheckBox).Checked;
        4: FCaseSensitive:= (Component as TCheckBox).Checked;
        5: FWholeWords:= (Component as TCheckBox).Checked;
        6: FFromCursor:= (Component as TCheckBox).Checked;
        7: FSelectionOnly:= (Component as TCheckBox).Checked;
       16: FExcludeCommentsAndStrings:= (Component as TCheckBox).Checked;
        8: FRegEx:= (Component as TCheckBox).Checked;
        9: FBackwards:= ((Component as TRadioGroup).ItemIndex = 1);
       10: if (Component as TRadioButton).Checked then FGrepAction:= 1;
       11: if (Component as TRadioButton).Checked then FGrepAction:= 2;
       12: if (Component as TRadioButton).Checked then FGrepAction:= 3;
       13: with Component as TComboBox do begin
             FDirectory:= FConfiguration.ExtendPath(Component as TComboBox);
             FDirectoryHistory:= Items.Text;
           end;
       14: with Component as TComboBox do begin
             FFilemask:= Text;
             FFilemaskHistory:= Items.Text;
           end;
       15: FIncludeSubdirs:= (Component as TCheckBox).Checked;
    end;
  end;
  if FSelectionOnly and FFromCursor then FFromCursor:= False;
  FSystemFromCursor:= FFromCursor;
end;

procedure TSearchOptions.SaveSearchOptions;
begin
  FConfiguration.WriteStringU('SearchReplace', 'FSearchText', FSearchText);
  FConfiguration.WriteStringU('SearchReplace', 'FSearchTextHistory', SaveComboBoxItems(FSearchTextHistory));
  FConfiguration.WriteStringU('SearchReplace', 'FReplaceText', FReplaceText);
  FConfiguration.WriteStringU('SearchReplace', 'FReplaceTextHistory', SaveComboBoxItems(FReplaceTextHistory));

  FConfiguration.WriteBoolU('SearchReplace', 'FReplace', FReplace);
  FConfiguration.WriteBoolU('SearchReplace', 'FCaseSensitive', FCaseSensitive);
  FConfiguration.WriteBoolU('SearchReplace', 'FWholeWords', FWholeWords);
  FConfiguration.WriteBoolU('SearchReplace', 'FFromCursor', FFromCursor);
  FConfiguration.WriteBoolU('SearchReplace', 'FSelectionOnly', FSelectionOnly);
  FConfiguration.WriteBoolU('SearchReplace', 'FExcludeCommentsAndStrings', FExcludeCommentsAndStrings);
  FConfiguration.WriteBoolU('SearchReplace', 'FRegEx', FRegEx);

  FConfiguration.WriteIntegerU('SearchReplace', 'Where', FGrepAction);
  FConfiguration.WriteStringU('SearchReplace', 'FDirectory', FDirectory);
  FConfiguration.WriteStringU('SearchReplace', 'FDirectoryHistory', SaveComboBoxItems(FDirectoryHistory));
  FConfiguration.WriteStringU('SearchReplace', 'FFilemask', FFilemask);
  FConfiguration.WriteStringU('SearchReplace', 'FFilemaskHistory', SaveComboBoxItems(FFilemaskHistory));
  FConfiguration.WriteBoolU('SearchReplace', 'FIncludeSubdirs', FIncludeSubdirs);
end;

procedure TSearchOptions.ShowRegSearchHelp;
  var Str: string;
begin
  if LeftStr(FConfiguration.LanguageCode, 2) = 'de'
    then Str:= FConfiguration.EditorFolder + 'docs/TRegExpr_DE.pdf'
    else Str:= FConfiguration.EditorFolder + 'docs/TRegExpr_EN.pdf';
  MyJavaCommands.ShellExecuteFile(Str, '', '', SW_SHOWNORMAL);
end;

end.

