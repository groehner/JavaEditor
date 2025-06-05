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
    procedure Statistic;
    procedure DoSearchReplace(const SearchText, ReplaceText: string);
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
  SysUtils,
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

procedure TFGrepResults.Execute;
var
  OnFoundEvent: TReplaceTextEvent;
  MyCaretXY: TBufferCoord;
  MyTopLine: Integer;

  procedure BeginSearch(MyEditor: TSynEdit; const Pathname: string);
  begin
    FFileName := Pathname;
    FMessages.StatusMessage(_('Search for') + ' "' + MySearchOptions.SearchText
      + '" in ' + FFileName);
    FEditor := MyEditor;
    MyTopLine := FEditor.TopLine;
    MyCaretXY := FEditor.CaretXY;
    OnFoundEvent := FEditor.OnReplaceText;
    FEditor.OnReplaceText := FoundIt;
    LockWindow(FJava.Handle);
  end;

  procedure EndSearch;
  begin
    FEditor.OnReplaceText := OnFoundEvent;
    FEditor.TopLine := MyTopLine;
    FEditor.CaretXY := MyCaretXY;
    FEditor.EnsureCursorPosVisible;
    UnlockWindow;
    FJava.Invalidate;
  end;

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
    Search: TSearchRec;
    Result: Integer;
    MyEditor: TSynEditEx;
    Pathname: string;
    EditForm: TFEditForm;
  begin
    { First do sub-directories if option is selected }
    Result := FindFirst(Dir + '*.*', faDirectory, Search);
    try
      while (Result = 0) and not FAborting do
      begin
        if (Search.Attr and faDirectory) <> 0 then
        begin
          if MySearchOptions.IncludeSubdirs then
            if (Search.Name <> '.') and (Search.Name <> '..') then
              DirGrep(Dir + Search.Name + '\', UpperCase(Mask));
        end
        else if ((Pos(UpperCase(ExtractFileExt(Search.Name)), Mask) > 0) or
          (Mask = '*.*')) and ((Search.Attr and faReadOnly) = 0) then
        begin
          FResults := nil;
          Pathname := Dir + Search.Name;
          EditForm := TFEditForm(FJava.GetTDIWindowType(Pathname, '%E%'));
          if Assigned(EditForm) then
          begin
            BeginSearch(EditForm.Editor, Pathname);
            if MySearchOptions.RegEx then
              MyRegExSearch.DoGrepRegSearchReplace(FEditor)
            else
              DoSearchReplace(MySearchOptions.SearchText,
                MySearchOptions.ReplaceText);
            EndSearch;
          end
          else
          begin
            MyEditor := TSynEditEx.Create(nil);
            try
              MyEditor.Parent := FConfiguration;
              MyEditor.SearchEngine := FJava.SynEditSearch;
              try
                MyEditor.Lines.LoadFromFile(Pathname);
              except
                on e: Exception do
                  ErrorMsg(e.Message + #13#10 + _('File') + ' ' + Pathname + ' '
                    + _('cannot be opened'));
              end;
              if MySearchOptions.ExcludeCommentsAndStrings then
                MyEditor.Highlighter := FConfiguration.GetHighlighter(Pathname);
              BeginSearch(MyEditor, Pathname);
              if MySearchOptions.RegEx then
                MyRegExSearch.DoGrepRegSearchReplace(FEditor)
              else
                DoSearchReplace(MySearchOptions.SearchText,
                  MySearchOptions.ReplaceText);
              EndSearch;
              if MySearchOptions.Replace and MyEditor.Modified then
                MyEditor.Lines.SaveToFile(Pathname, MyEditor.Lines.Encoding);
            finally
              FreeAndNil(MyEditor);
            end;
          end;
          Inc(FFileCount);
          Application.ProcessMessages;
        end;
        Result := FindNext(Search);
      end;
    finally
      FindClose(Search);
    end;
  end;

begin
  if FSearching then
  begin
    InformationMsg(_('Still FSearching!'));
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  try
    FSearching := True;
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
            DirGrep(MySearchOptions.Directory, '*.JAVA')
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

procedure TFGrepResults.DoSearchReplace(const SearchText, ReplaceText: string);
var
  Action: TSynReplaceAction;
  UsedSearchLine, ReplaceLine, UsedSearchText: string;
  Positions: array of Integer;
  StackPos, ScanPos: Integer;

  function IsWholeWord(ScanPos: Integer): Boolean;
  begin
    var
    Posi := ScanPos;
    var
    QPos := ScanPos + Length(SearchText) - 1;
    Result := (((Posi - 1 = 0) or IsWordBreakChar(UsedSearchLine[Posi - 1])) and
      ((QPos + 1 > Length(UsedSearchLine)) or
      IsWordBreakChar(UsedSearchLine[QPos + 1])));
  end;

  function IsCommentOrString(ScanPos, LineNo: Integer): Boolean;
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

begin
  SetLength(Positions, 100);
  for var LineNo := 0 to FEditor.Lines.Count - 1 do
  begin
    StackPos := 0;
    UsedSearchLine := FEditor.Lines[LineNo];
    UsedSearchText := SearchText;
    if not MySearchOptions.CaseSensitive then
    begin
      UsedSearchLine := WideUpperCase(UsedSearchLine);
      UsedSearchText := WideUpperCase(SearchText);
    end;
    ScanPos := 1;
    repeat
      ScanPos := Pos(UsedSearchText, UsedSearchLine, ScanPos);
      if ScanPos > 0 then
      begin
        if not(MySearchOptions.WholeWords and not IsWholeWord(ScanPos)) and
          not(MySearchOptions.ExcludeCommentsAndStrings and
          IsCommentOrString(ScanPos, LineNo)) then
        begin
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
    if MySearchOptions.Replace and (StackPos > 0) then
    begin
      FEditor.BeginUpdate;
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
end;

end.
