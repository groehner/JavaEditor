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
  FSearchText:= FConfiguration.ReadStringU('SearchReplace', 'SearchText', '');
  FSearchTextHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'SearchTextHistory', ''));
  FReplaceText:= FConfiguration.ReadStringU('SearchReplace', 'ReplaceText', '');
  FReplaceTextHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'ReplaceTextHistory', ''));

  FReplace:= FConfiguration.ReadBoolU('SearchReplace', 'Replace', False);
  FCaseSensitive:= FConfiguration.ReadBoolU('SearchReplace', 'CaseSensitive', False);
  FWholeWords:= FConfiguration.ReadBoolU('SearchReplace', 'WholeWords', False);
  FFromCursor:= FConfiguration.ReadBoolU('SearchReplace', 'FromCursor', False);
  FSelectionOnly:= FConfiguration.ReadBoolU('SearchReplace', 'SelectionOnly', False);
  FExcludeCommentsAndStrings:= FConfiguration.ReadBoolU('SearchReplace', 'ExcludeCommentsAndStrings', False);
  FRegEx:= FConfiguration.ReadBoolU('SearchReplace', 'RegEx', False);
  FBackwards:= False;

  FGrepAction:= FConfiguration.ReadIntegerU('SearchReplace', 'Where', 1);
  FDirectory:= FConfiguration.ReadStringU('SearchReplace', 'Directory', '');
  FDirectoryHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'DirectoryHistory', ''));
  FFilemask:= FConfiguration.ReadStringU('SearchReplace', 'Filemask', '*.java');
  FFilemaskHistory:= LoadComboBoxItems(FConfiguration.ReadStringU('SearchReplace', 'FilemaskHistory', ''));
  FIncludeSubdirs:= FConfiguration.ReadBoolU('SearchReplace', 'IncludeSubdirs', True);
end;

procedure TSearchOptions.LoadToForm(Form: TForm);
  var Component: TComponent;
begin
  with Form do
    for var I:= 0 to ComponentCount -1 do begin
      Component:= Components[I];
      case Component.Tag of
        1: begin
             TComboBox(Component).Text:= FSearchText;
             TComboBox(Component).Items.Text:= FSearchTextHistory;
           end;
        2: begin
             TComboBox(Component).Text:= FReplaceText;
             TComboBox(Component).Items.Text:= FReplaceTextHistory;
           end;
        3: TCheckBox(Component).Checked:= FReplace;
        4: TCheckBox(Component).Checked:= FCaseSensitive;
        5: TCheckBox(Component).Checked:= FWholeWords;
        6: TCheckBox(Component).Checked:= FFromCursor;
        7: TCheckBox(Component).Checked:= FSelectionOnly;
       16: TCheckBox(Component).Checked:= FExcludeCommentsAndStrings;
        8: TCheckBox(Component).Checked:= FRegEx;
        9: if FBackwards
             then TRadioGroup(Component).ItemIndex:= 1
             else TRadioGroup(Component).ItemIndex:= 0;
       10: TRadioButton(Component).Checked:= (FGrepAction = 1);
       11: TRadioButton(Component).Checked:= (FGrepAction = 2);
       12: TRadioButton(Component).Checked:= (FGrepAction = 3);
       13: begin
             TComboBox(Component).Text:= FDirectory;
             TComboBox(Component).Items.Text:= FDirectoryHistory;
             FConfiguration.ShortenPath(TComboBox(Component), FDirectory);
           end;
       14: begin
             TComboBox(Component).Text:= FFilemask;
             TComboBox(Component).Items.Text:= FFilemaskHistory;
           end;
       15: TCheckBox(Component).Checked:= FIncludeSubdirs;
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
        1: begin
             FSearchText:= TComboBox(Component).Text;
             FSearchTextHistory:= TComboBox(Component).Items.Text;
           end;
        2: begin
             FReplaceText:= TComboBox(Component).Text;
             FReplaceTextHistory:= TComboBox(Component).Items.Text;
           end;
        3: FReplace:= TCheckBox(Component).Checked;
        4: FCaseSensitive:= TCheckBox(Component).Checked;
        5: FWholeWords:= TCheckBox(Component).Checked;
        6: FFromCursor:= TCheckBox(Component).Checked;
        7: FSelectionOnly:= TCheckBox(Component).Checked;
       16: FExcludeCommentsAndStrings:= TCheckBox(Component).Checked;
        8: FRegEx:= TCheckBox(Component).Checked;
        9: FBackwards:= (TRadioGroup(Component).ItemIndex = 1);
       10: if TRadioButton(Component).Checked then FGrepAction:= 1;
       11: if TRadioButton(Component).Checked then FGrepAction:= 2;
       12: if TRadioButton(Component).Checked then FGrepAction:= 3;
       13: begin
             FDirectory:= FConfiguration.ExtendPath(TComboBox(Component));
             FDirectoryHistory:= TComboBox(Component).Items.Text;
           end;
       14: begin
             FFilemask:= TComboBox(Component).Text;
             FFilemaskHistory:= TComboBox(Component).Items.Text;
           end;
       15: FIncludeSubdirs:= TCheckBox(Component).Checked;
    end;
  end;
  if FSelectionOnly and FFromCursor then FFromCursor:= False;
  FSystemFromCursor:= FFromCursor;
end;

procedure TSearchOptions.SaveSearchOptions;
begin
  FConfiguration.WriteStringU('SearchReplace', 'SearchText', FSearchText);
  FConfiguration.WriteStringU('SearchReplace', 'SearchTextHistory', SaveComboBoxItems(FSearchTextHistory));
  FConfiguration.WriteStringU('SearchReplace', 'ReplaceText', FReplaceText);
  FConfiguration.WriteStringU('SearchReplace', 'ReplaceTextHistory', SaveComboBoxItems(FReplaceTextHistory));

  FConfiguration.WriteBoolU('SearchReplace', 'Replace', FReplace);
  FConfiguration.WriteBoolU('SearchReplace', 'CaseSensitive', FCaseSensitive);
  FConfiguration.WriteBoolU('SearchReplace', 'WholeWords', FWholeWords);
  FConfiguration.WriteBoolU('SearchReplace', 'FromCursor', FFromCursor);
  FConfiguration.WriteBoolU('SearchReplace', 'SelectionOnly', FSelectionOnly);
  FConfiguration.WriteBoolU('SearchReplace', 'ExcludeCommentsAndStrings', FExcludeCommentsAndStrings);
  FConfiguration.WriteBoolU('SearchReplace', 'RegEx', FRegEx);

  FConfiguration.WriteIntegerU('SearchReplace', 'Where', FGrepAction);
  FConfiguration.WriteStringU('SearchReplace', 'Directory', FDirectory);
  FConfiguration.WriteStringU('SearchReplace', 'DirectoryHistory', SaveComboBoxItems(FDirectoryHistory));
  FConfiguration.WriteStringU('SearchReplace', 'Filemask', FFilemask);
  FConfiguration.WriteStringU('SearchReplace', 'FilemaskHistory', SaveComboBoxItems(FFilemaskHistory));
  FConfiguration.WriteBoolU('SearchReplace', 'IncludeSubdirs', FIncludeSubdirs);
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

