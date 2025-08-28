unit UGrepResults;

interface

uses
  Classes,
  ComCtrls,
  SynEdit;

type
  TSearchResult = class(TCollectionItem)
  private
    FText: string;
    FLine: Integer;
    FSPos: Integer;
    FEPos: Integer;
  published
    property Text: string read FText write FText;
    property Line: Integer read FLine write FLine;
    property SPos: Integer read FSPos write FSPos;
    property EPos: Integer read FEPos write FEPos;
  end;

  TSearchResults = class(TCollection)
  private
    FFileName: string;
    function GetItem(Index: Integer): TSearchResult;
    procedure SetItem(Index: Integer; Value: TSearchResult);
  public
    constructor Create;
    function Add: TSearchResult;
    property FileName: string read FFileName write FFileName;
    property Items[Index: Integer]: TSearchResult read GetItem
      write SetItem; default;
  end;

  TFGrepResults = class
  private
    FAborting: Boolean;
    FTvResults: TTreeView;
    FResults: TSearchResults;
    FSearching: Boolean;
    FEditor: TSynEdit;
    FFileName: string;
    FFileCount: Integer;
    FTheNode: TTreeNode;
    FMyCaretXY: TBufferCoord;
    FMyTopLine: Integer;
    FOnFoundEvent: TReplaceTextEvent;
    procedure Statistic;
    procedure BeginSearch(MyEditor: TSynEdit; const Pathname: string);
    procedure EndSearch;
    procedure DoSearchInFile(const Pathname: string);
    procedure DoSearch(Editor: TSynEdit; const Pathname: string);
    procedure DoSearchReplace(const SearchText, ReplaceText: string);
    function IsWholeWord(const Line: string; ScanPos, Len: Integer): Boolean;
    function IsCommentOrString(ScanPos, LineNo: Integer): Boolean;
  public
    constructor Create(TreeView: TTreeView);
    procedure Execute;
    procedure FoundIt(Sender: TObject; const ASearch, AReplace: string;
      Line, Column: Integer; var Action: TSynReplaceAction);
    procedure OpenSource(Node: TTreeNode);
    procedure DeleteSearchResults;
    property Aborting: Boolean read FAborting write FAborting;
  end;

var
  MyGrepResults: TFGrepResults;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  Forms,
  JvGnugettext,
  SynEditTypes,
  SynEditHighlighter,
  UStringRessources,
  USynEditEx,
  UJava,
  UEditorForm,
  UConfiguration,
  UUtils,
  UMessages,
  USearchOptions,
  URegExSearch,
  UDlgSearchAbort,
  UITypes;

{ --- TSearchResults ----------------------------------------------------------- }

constructor TSearchResults.Create;
begin
  inherited Create(TSearchResult);
end;

function TSearchResults.Add: TSearchResult;
begin
  Result := TSearchResult(inherited Add);
end;

function TSearchResults.GetItem(Index: Integer): TSearchResult;
begin
  Result := TSearchResult(inherited GetItem(Index));
end;

procedure TSearchResults.SetItem(Index: Integer; Value: TSearchResult);
begin
  inherited SetItem(Index, Value);
end;

{ --- TFGrepResults ------------------------------------------------------------ }

constructor TFGrepResults.Create(TreeView: TTreeView);
begin
  FTvResults := TreeView;
  FTvResults.ShowButtons := True;
end;

procedure TFGrepResults.FoundIt(Sender: TObject;
  const ASearch, AReplace: string; Line, Column: Integer;
  var Action: TSynReplaceAction);
var
  AResult: TSearchResult;
  Str: string;
begin
  Application.ProcessMessages;
  if not Assigned(FResults) or (FResults.FFileName <> FFileName) then
  begin
    FResults := TSearchResults.Create;
    FResults.FFileName := FFileName;
    FTheNode := FTvResults.Items.AddObject(nil, FFileName, FResults);
  end;

  AResult := FResults.Add;
  AResult.Line := Line;
  AResult.SPos := Column;

  if MySearchOptions.Replace then
  begin
    AResult.Text := FEditor.Lines[Line - 1];
    for var I := FResults.Count - 2 downto 0 do
      if FResults[I].Line = Line then
        FResults[I].Text := AResult.Text
      else
        Break;
    AResult.EPos := Column + Length(AReplace);
    Action := raReplace;
  end
  else
  begin
    AResult.Text := FEditor.Lines[Line - 1];
    AResult.EPos := Column + Length(ASearch);
    Action := raSkip;
  end;
  Str := IntToStr(AResult.Line) + ': ' + Trim(AResult.Text);
  FTvResults.Items.AddChildObject(FTheNode, Str, AResult);
  FTheNode.Expand(False);
end;

procedure TFGrepResults.DoSearchInFile(const Pathname: string);
begin
  var
  MyEditor := TSynEditEx.Create(nil);
  try
    MyEditor.Parent := FConfiguration;
    MyEditor.SearchEngine := FJava.SynEditSearch;
    MyEditor.Lines.LoadFromFile(Pathname);
    if MySearchOptions.ExcludeCommentsAndStrings then
      MyEditor.Highlighter := FConfiguration.GetHighlighter(Pathname);
    DoSearch(MyEditor, Pathname);
    if MySearchOptions.Replace and MyEditor.Modified then
      MyEditor.Lines.SaveToFile(Pathname, MyEditor.Lines.Encoding);
  finally
    MyEditor.Free;
  end;
end;

procedure TFGrepResults.DoSearch(Editor: TSynEdit; const Pathname: string);
begin
  BeginSearch(Editor, Pathname);
  if MySearchOptions.RegEx then
    MyRegExSearch.DoGrepRegSearchReplace(FEditor)
  else
    DoSearchReplace(MySearchOptions.SearchText, MySearchOptions.ReplaceText);
  EndSearch;
end;

procedure TFGrepResults.BeginSearch(MyEditor: TSynEdit; const Pathname: string);
begin
  FFileName := Pathname;
  FMessages.StatusMessage(_('Search for') + ' "' + MySearchOptions.SearchText +
    '" in ' + FFileName);
  FEditor := MyEditor;
  FMyTopLine := FEditor.TopLine;
  FMyCaretXY := FEditor.CaretXY;
  FOnFoundEvent := FEditor.OnReplaceText;
  FEditor.OnReplaceText := FoundIt;
  LockWindow(FJava.Handle);
end;

procedure TFGrepResults.EndSearch;
begin
  FEditor.OnReplaceText := FOnFoundEvent;
  FEditor.TopLine := FMyTopLine;
  FEditor.CaretXY := FMyCaretXY;
  FEditor.EnsureCursorPosVisible;
  UnlockWindow;
  FJava.Invalidate;
end;

procedure TFGrepResults.Execute;

  procedure CurrentOnlyGrep;
  begin
    FResults := nil;
    BeginSearch(FJava.EditorForm.Editor, FJava.EditorForm.Pathname);
    if MySearchOptions.RegEx then
      MyRegExSearch.DoGrepRegSearchReplace(FEditor)
    else
      DoSearchReplace(MySearchOptions.SearchText, MySearchOptions.ReplaceText);
    EndSearch;
    FFileCount := 1;
  end;

  procedure OpenFilesGrep;
  var
    Form: TFEditForm;
  begin
    FResults := nil;
    for var I := 0 to FJava.TDIEditFormCount - 1 do
    begin
      Form := FJava.TDIEditFormGet(I);
      BeginSearch(Form.Editor, Form.Pathname);
      if MySearchOptions.RegEx then
        MyRegExSearch.DoGrepRegSearchReplace(FEditor)
      else
        DoSearchReplace(MySearchOptions.SearchText,
          MySearchOptions.ReplaceText);
      EndSearch;
      Inc(FFileCount);
      Application.ProcessMessages;
      if FAborting then
        Exit;
    end;
  end;

  procedure DirGrep(const Dir, Mask: string);
  var
    EditForm: TFEditForm;
  begin
    var
    Filenames := TDirectory.GetFiles(Dir, Mask, TSearchOption.soAllDirectories);
    for var Pathname in Filenames do
    begin
      FResults := nil;
      EditForm := FJava.GetEditForm(Pathname);
      if Assigned(EditForm) then
        DoSearch(EditForm.Editor, Pathname)
      else
        DoSearchInFile(Pathname);
      Inc(FFileCount);
      Application.ProcessMessages;
      if Aborting then
        Break;
    end;
  end;

begin
  if FSearching then
  begin
    InformationMsg(_('Still searching!'));
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  FSearching := True;
  try
    FFileCount := 0;
    if MySearchOptions.RegEx and not Assigned(MyRegExSearch) then
      MyRegExSearch := TRegExSearch.Create;
    DeleteSearchResults;
    FAborting := False;
    case MySearchOptions.GrepAction of
      1:
        CurrentOnlyGrep;
      2:
        with TFSearchAbort.Create(Application) do
        begin
          ShowWith(MySearchOptions.SearchText);
          Application.ProcessMessages;
          OpenFilesGrep;
          Free;
        end;
      3:
        with TFSearchAbort.Create(Application) do
        begin
          ShowWith(MySearchOptions.SearchText);
          Application.ProcessMessages;
          if MySearchOptions.Filemask = '' then
            DirGrep(MySearchOptions.Directory, '*.java')
          else
            DirGrep(MySearchOptions.Directory,
              UpperCase(MySearchOptions.Filemask));
          Free;
        end;
    end;
    Statistic;
  finally
    FSearching := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFGrepResults.DeleteSearchResults;
begin
  var
  Node := FTvResults.Items.GetFirstNode;
  while Assigned(Node) do
  begin
    FResults := TSearchResults(Node.Data);
    FreeAndNil(FResults);
    Node := Node.getNextSibling;
  end;
  FTvResults.Items.Clear;
  FTheNode := nil;
end;

procedure TFGrepResults.Statistic;
var
  Counts, Files: Integer;
  Node: TTreeNode;
begin
  FTvResults.Items.BeginUpdate;
  Files := 0;
  Counts := 0;
  Node := FTvResults.Items.GetFirstNode;
  while Assigned(Node) do
  begin
    FResults := TSearchResults(Node.Data);
    Inc(Files);
    Inc(Counts, FResults.Count);
    Node.Text := Node.Text + '  (' + IntToStr(FResults.Count) + ')';
    Node := Node.getNextSibling;
  end;
  FTvResults.Items.EndUpdate;
  if Counts = 0 then
    InformationMsg(Format(_(LNGSearchTextNotFound),
      [MySearchOptions.SearchText]));
  FMessages.StatusMessage
    (Format(_('%d occurrences of "%s" in %d of total %d files'),
    [Counts, MySearchOptions.SearchText, Files, FFileCount]));
end;

procedure TFGrepResults.OpenSource(Node: TTreeNode);
var
  Result: TSearchResult;
  CurrentFileName, Str: string;
  BufferCoord: TBufferCoord;
  SearchResult: TSearchResults;
begin
  if not Assigned(Node) then
    Exit;
  if Node.Parent = nil then
  begin
    Str := Node.Text;
    Delete(Str, Pos(' (', Str) - 1, Length(Str));
    FJava.ChangeWindowWithPositioning(Str, 1, 1, False);
    Exit;
  end;
  Result := TSearchResult(Node.Data);
  if not Assigned(Result) then
    Exit;
  SearchResult := TSearchResults(Result.Collection);
  CurrentFileName := SearchResult.FFileName;

  FJava.ChangeWindowWithPositioning(CurrentFileName, Result.SPos,
    Result.Line, False);
  BufferCoord.Char := Result.SPos;
  BufferCoord.Line := Result.Line;
  FJava.EditorForm.Editor.BlockBegin := BufferCoord;
  BufferCoord.Char := Result.EPos;
  FJava.EditorForm.Editor.BlockEnd := BufferCoord;
end;

function TFGrepResults.IsWholeWord(const Line: string; ScanPos, Len: Integer)
  : Boolean;
begin
  var
  Pos1 := ScanPos;
  var
  Pos2 := ScanPos + Len - 1;
  Result := (((Pos1 - 1 = 0) or IsWordBreakChar(Line[Pos1 - 1])) and
    ((Pos2 + 1 > Length(Line)) or IsWordBreakChar(Line[Pos2 + 1])));
end;

function TFGrepResults.IsCommentOrString(ScanPos, LineNo: Integer): Boolean;
var
  Token: string;
  Attr: TSynHighlighterAttributes;
begin
  FEditor.GetHighlighterAttriAtRowCol(BufferCoord(ScanPos, LineNo + 1),
    Token, Attr);
  Result := (Attr = FEditor.Highlighter.CommentAttribute) or
    (Attr = FEditor.Highlighter.StringAttribute) or
    (Attr.Name = 'Preprocessor'); // compiler directives
end;

procedure TFGrepResults.DoSearchReplace(const SearchText, ReplaceText: string);
var
  Positions: array of Integer;
  StackPos: Integer;

  procedure Search(LineNo: Integer);
  var
    UsedSearchLine, UsedSearchText: string;
    Action: TSynReplaceAction;
  begin
    StackPos := 0;
    UsedSearchLine := FEditor.Lines[LineNo];
    UsedSearchText := SearchText;
    if not MySearchOptions.CaseSensitive then
    begin
      UsedSearchLine := WideUpperCase(UsedSearchLine);
      UsedSearchText := WideUpperCase(SearchText);
    end;
    var
    ScanPos := 1;
    repeat
      ScanPos := Pos(UsedSearchText, UsedSearchLine, ScanPos);
      if ScanPos > 0 then
      begin
        if (MySearchOptions.WholeWords and
            IsWholeWord(UsedSearchLine, ScanPos, Length(SearchText)) or
            not MySearchOptions.WholeWords)
        and
           (MySearchOptions.ExcludeCommentsAndStrings and
            not IsCommentOrString(ScanPos, LineNo) or
            not MySearchOptions.ExcludeCommentsAndStrings)
        then begin
          FoundIt(Self, SearchText, ReplaceText, LineNo + 1, ScanPos, Action);
          if MySearchOptions.Replace then
          begin
            Positions[StackPos] := ScanPos;
            Inc(StackPos);
          end;
        end;
        Inc(ScanPos, Length(SearchText));
      end;
    until ScanPos = 0;
  end;

  procedure Replace(LineNo: Integer);
  begin
    if MySearchOptions.Replace and (StackPos > 0) then
    begin
      FEditor.BeginUpdate;
      var
      ReplaceLine := FEditor.Lines[LineNo];
      while StackPos > 0 do
      begin
        Dec(StackPos);
        Delete(ReplaceLine, Positions[StackPos], Length(SearchText));
        Insert(ReplaceText, ReplaceLine, Positions[StackPos]);
      end;
      FEditor.Lines[LineNo] := ReplaceLine;
      FEditor.Modified := True;
      FEditor.EndUpdate;
    end;
  end;

begin
  SetLength(Positions, 100);
  for var LineNo := 0 to FEditor.Lines.Count - 1 do
  begin
    Search(LineNo);
    Replace(LineNo);
  end;
end;

end.
