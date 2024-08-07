unit UGrepResults;

interface

uses
  Classes, ComCtrls, SynEdit;

type
  TSearchResult = class(TCollectionItem)
  private
    fText: string;
    fLine: Integer;
    fSPos: Integer;
    fEPos: Integer;
  published
    property Text: string  read fText write fText;
    property Line: integer read fLine write fLine;
    property SPos: integer read fSPos write fSPos;
    property EPos: integer read fEPos write fEPos;
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
    property Items[Index: Integer]: TSearchResult read GetItem write SetItem; default;
  end;

  TFGrepResults = class
  private
    tvResults: TTreeView;
    Results: TSearchResults;
    Searching: Boolean;
    Editor: TSynEdit;
    Filename: string;
    FileCount: Integer;
    theNode: TTreeNode;
    procedure Statistic;
    procedure DoSearchReplace(const SearchText, ReplaceText: string);
  public
    Aborting: boolean;
    constructor Create(TreeView: TTreeView);
    procedure Execute;
    procedure FoundIt(Sender: TObject; const ASearch, AReplace: string;
                      Line, Column: Integer; var Action: TSynReplaceAction);
    procedure OpenSource(Node: TTreeNode);
    procedure DeleteSearchResults;
  end;

var
  myGrepResults: TFGrepResults;

implementation

uses
  SysUtils, Controls, Forms, USynEditEx, JvGnugettext, UStringRessources,
  UJava, UFrmEditor, UConfiguration, UUtils, UMessages,
  USearchOptions, URegExSearch, UDlgSearchAbort, SynEditTypes, UITypes;

{--- TSearchResults -----------------------------------------------------------}

constructor TSearchResults.Create;
begin
  inherited Create(TSearchResult);
end;

function TSearchResults.Add: TSearchResult;
begin
  Result:= TSearchResult(inherited Add);
end;

function TSearchResults.GetItem(Index: Integer): TSearchResult;
begin
  Result:= TSearchResult(inherited GetItem(Index));
end;

procedure TSearchResults.SetItem(Index: Integer; Value: TSearchResult);
begin
  inherited SetItem(Index, Value);
end;

{--- TFGrepResults ------------------------------------------------------------}

constructor TFGrepResults.Create(TreeView: TTreeView);
begin
  tvResults:= TreeView;
  tvResults.ShowButtons:= true;
end;

procedure TFGrepResults.FoundIt(Sender: TObject; const ASearch, AReplace: string;
                                Line, Column: Integer; var Action: TSynReplaceAction);
  var AResult: TSearchResult;
      s: string; i: integer;
begin
  Application.ProcessMessages;
  if (Results = nil) or (Results.FileName <> FileName) then begin
    Results:= TSearchResults.Create;
    Results.FileName:= FileName;
    theNode:= tvResults.Items.AddObject(nil, FileName, Results);
  end;

  AResult:= Results.Add;
  AResult.Line:= Line;
  AResult.SPos:= Column;

  if MySearchOptions.Replace then begin
    AResult.Text:= Editor.Lines[Line-1];
    for i:= Results.Count - 2 downto 0 do
      if Results.Items[i].Line = Line
        then Results.Items[i].Text:= AResult.Text
        else break;
    AResult.EPos:= Column + Length(AReplace);
    Action:= raReplace;
    end
  else begin
    AResult.Text:= Editor.Lines[Line-1];
    AResult.EPos:= Column + Length(ASearch);
    Action:= raSkip;
  end;
  s:= IntToStr(AResult.Line) + ': ' + trim(AResult.Text);
  tvResults.Items.AddChildObject(theNode, s, AResult);
  theNode.Expand(false);
end;

procedure TFGrepResults.Execute;
  var OnFoundEvent: TReplaceTextEvent;
      myCaretXY: TBufferCoord;
      myTopLine: integer;

  procedure BeginSearch(myEditor: TSynEdit; const Pathname: string);
  begin
    Filename:= Pathname;
    FMessages.StatusMessage(_('Search for:') + ' ' + Filename);
    Editor:= myEditor;
    myTopLine:= Editor.TopLine;
    myCaretXY:= Editor.CaretXY;
    OnFoundEvent:= Editor.OnReplaceText;
    Editor.OnReplaceText:= FoundIt;
    LockWindow(FJava.Handle);
  end;

  procedure EndSearch;
  begin
    Editor.OnReplaceText:= OnFoundEvent;
    Editor.TopLine:= myTopLine;
    Editor.CaretXY:= myCaretXY;
    Editor.EnsureCursorPosVisible;
    UnlockWindow;
    FJava.invalidate;
  end;

  procedure CurrentOnlyGrep;
  begin
    Results:= nil;
    BeginSearch(FJava.EditorForm.Editor, FJava.EditorForm.Pathname);
    if mySearchOptions.RegEx
      then MyRegExSearch.DoGrepRegSearchReplace(Editor)
      else DoSearchReplace(MySearchOptions.SearchText, MySearchOptions.ReplaceText);
    EndSearch;
    FileCount:= 1;
  end;                                 

  procedure OpenFilesGrep;
    var i: integer; aForm: TFEditForm;
  begin
    Results:= nil;
    for i:= 0 to FJava.TDIEditFormCount - 1 do begin
      aForm:= FJava.TDIEditFormGet(i);
      BeginSearch(aForm.Editor, aForm.Pathname);
      if mySearchOptions.RegEx
        then MyRegExSearch.DoGrepRegSearchReplace(Editor)
        else DoSearchReplace(MySearchOptions.SearchText, MySearchOptions.ReplaceText);
      EndSearch;
      inc(FileCount);
      Application.ProcessMessages;
      if Aborting then exit;
    end;
  end;

  procedure DirGrep(const Dir, Mask: string);
    var
      Search: TSearchRec;
      Result: Integer;
      myEditor: TSynEditEx;
      Pathname: string;
      EditForm: TFEditForm;
  begin
    { First do sub-directories if option is selected }
    Result:= FindFirst(Dir + '*.*', FaDirectory, Search);
    try
      while (Result = 0) and not Aborting do begin
        if (Search.Attr and FaDirectory) <> 0 then begin
          if MySearchOptions.IncludeSubdirs then
            if (Search.Name <> '.') and (Search.Name <> '..') then
              DirGrep(Dir + Search.Name + '\', Uppercase(Mask));
        end
        else if (Pos(Uppercase(ExtractFileExt(Search.Name)), Mask) > 0) or (Mask = '*.*') then begin
          Results:= nil;
          Pathname:= Dir + Search.Name;
          EditForm:= TFEditForm(FJava.getTDIWindowType(Pathname, '%E%'));
          if assigned(EditForm) then begin
            BeginSearch(EditForm.Editor, Pathname);
            if mySearchOptions.RegEx
              then MyRegExSearch.DoGrepRegSearchReplace(Editor)
              else DoSearchReplace(MySearchOptions.SearchText, MySearchOptions.ReplaceText);
            EndSearch;
          end else begin
            myEditor:= TSynEditEx.Create(nil);
            try
              myEditor.Parent:= FConfiguration;
              myEditor.SearchEngine:= FJava.SynEditSearch;
              try
                myEditor.Lines.LoadFromFile(Pathname);
              except on e: exception do
                ErrorMsg(e.Message + #13#10+ 'File ' + Pathname + ' cannot be opened');
              end;
              BeginSearch(myEditor, Pathname);
              myEditor.ReplaceTabs(FConfiguration.TabWidth);
              if mySearchOptions.RegEx
                then MyRegExSearch.DoGrepRegSearchReplace(Editor)
                else DoSearchReplace(MySearchOptions.SearchText, MySearchOptions.ReplaceText);
              EndSearch;
              if MySearchOptions.Replace and myEditor.Modified then
                myEditor.Lines.SaveToFile(Pathname, myEditor.Lines.Encoding);
            finally
             FreeAndNil(myEditor);
            end;
          end;
          inc(FileCount);
          Application.ProcessMessages;
        end;
        Result:= FindNext(Search);
      end;
    finally
      FindClose(Search);
    end;
  end;

begin
  if Searching then begin
    InformationMsg(_('Still searching!'));
    exit;
  end;
  Screen.Cursor:= crHourGlass;
  try
    Searching:= True;
    FileCount:= 0;
    if mySearchOptions.RegEx and (MyRegExSearch = nil) then
      MyRegExSearch:= TRegExSearch.Create;
    DeleteSearchResults;
    Aborting:= false;
    case MySearchOptions.GrepAction of
      1: CurrentOnlyGrep;
      2: with TFSearchAbort.Create(Application) do begin
           ShowWith(MySearchOptions.SearchText);
           Application.ProcessMessages;
           OpenFilesGrep;
           Free;
         end;
      3: with TFSearchAbort.Create(Application) do begin
           ShowWith(MySearchOptions.SearchText);
           Application.ProcessMessages;
           if MySearchOptions.Filemask = ''
             then DirGrep(MySearchOptions.Directory, '*.JAVA')
             else DirGrep(MySearchOptions.Directory, Uppercase(MySearchOptions.Filemask));
           Free;
         end;
    end;
    Statistic;
  finally
    Searching:= False;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFGrepResults.DeleteSearchResults;
begin
  var Node:= tvResults.Items.GetFirstNode;
  while assigned(Node) do begin
    Results:= TSearchResults(Node.Data);
    FreeAndNil(Results);
    Node:= Node.getNextSibling;
  end;
  tvResults.Items.Clear;
  theNode:= nil;
end;

procedure TFGrepResults.Statistic;
  var Counts, Files: integer;
      Node: TTreeNode; 
begin
  tvResults.Items.BeginUpdate;
  Files:= 0;
  Counts:= 0;
  Node:= tvResults.Items.GetFirstNode;
  while assigned(Node) do begin
    Results:= TSearchResults(Node.Data);
    inc(Files);
    inc(Counts, Results.Count);
    Node.Text:= Node.Text + '  (' + IntToStr(Results.Count) + ')';
    Node:= Node.getNextSibling;
  end;
  tvResults.Items.EndUpdate;
  if Counts = 0 then
    InformationMsg(Format(_(LNGSearchTextNotFound), [MySearchOptions.SearchText]));
  FMessages.StatusMessage(Format(_('%d occurrences in %d of total %d files'), [Counts, Files, FileCount]));
end;

procedure TFGrepResults.OpenSource(Node: TTreeNode);
var
  Result: TSearchResult;
  CurrentFileName, s: string;
  R: TBufferCoord;
  SR: TSearchResults;
begin
  if not Assigned(Node) then exit;
  if Node.Parent = nil then begin
    s:= Node.Text;
    delete(s, Pos(' (', s) - 1, length(s));
    FJava.ChangeWindowWithPositioning(s, 1, 1, false);
    exit;
  end;
  Result:= TSearchResult(Node.Data);
  if Result = nil then exit;
  SR:= TSearchResults(Result.Collection);
  CurrentFileName:= SR.FileName;

  FJava.ChangeWindowWithPositioning(CurrentFileName, Result.SPos, Result.Line, false);
  R.Char:= Result.SPos;
  R.Line:= Result.Line;
  FJava.EditorForm.Editor.BlockBegin:= R;
  R.Char:= Result.EPos;
  FJava.EditorForm.Editor.BlockEnd:= R;
end;

procedure TFGrepResults.DoSearchReplace(const SearchText, ReplaceText: string);
  var i, p: integer; Action: TSynReplaceAction;
      UpperSearchText, LineSearchText: string; Flags: TReplaceFlags;

  procedure DoWholeWord(p, i: integer);
    var s1, s2, re: string; q, scanned: integer;
        us1, uSearchText: string;

    function IsWholeWord(p, q: integer): boolean;
    begin
      p:= scanned + p;
      q:= scanned + q;
      Result:= (((p - 1 = 0) or IsWordBreakChar(s2[p-1])) and
                ((q + 1 > length(s2)) or IsWordBreakChar(s2[q+1])))
    end;

  begin
    scanned:= 0;
    s1:= Editor.Lines[i];
    s2:= s1;
    if not mySearchOptions.CaseSensitive then begin
      us1:= WideUpperCase(s1);
      uSearchText:= WideUppercase(SearchText);
    end;
    q:= p + length(SearchText) - 1;
    while not IsWholeWord(p, q) do begin
      inc(scanned, q);
      delete(s1, 1, q);
      delete(us1, 1, q);
      if mySearchOptions.CaseSensitive
        then p:= Pos(SearchText, s1)
        else p:= Pos(uSearchText, us1);
      if p = 0 then exit;
      q:= p + length(SearchText) - 1;
    end;
    FoundIt(Self, SearchText, ReplaceText, i+1, scanned + p, Action);
    if mySearchOptions.Replace then begin
      re:= copy(s2, 1, scanned);
      while p > 0 do begin
        if IsWholeWord(p, q)
          then re:= re + copy(s1, 1, p-1) + ReplaceText
          else re:= re + copy(s1, 1, q);
        inc(scanned, q);
        delete(s1, 1, q);
        delete(us1, 1, q);
        if mySearchOptions.CaseSensitive
          then p:= Pos(SearchText, s1)
          else p:= Pos(uSearchText, us1);
        q:= p + length(SearchText) - 1;
      end;
      Editor.Lines[i]:= re + s1;
    end;
  end;

begin
  Editor.BeginUpdate;
  if not mySearchOptions.CaseSensitive then begin
    UpperSearchText:= WideUpperCase(SearchText);
    Flags:= [rfReplaceAll, rfIgnoreCase];
  end else
    Flags:= [rfReplaceAll];

  for i:= 0 to Editor.Lines.Count - 1 do begin
    if mySearchOptions.CaseSensitive then
      p:= System.Pos(Searchtext, Editor.Lines[i])
    else begin
      LineSearchText:= WideUpperCase(Editor.Lines[i]);
      p:= System.Pos(UpperSearchText, LineSearchText);
    end;
    if p > 0 then
      if mySearchOptions.WholeWords
        then DoWholeWord(p, i)
      else begin
        if mySearchOptions.Replace then begin
          Editor.Lines[i]:= StringReplace(Editor.Lines[i], SearchText, ReplaceText, Flags);
          Editor.Modified:= true;
        end;
        FoundIt(Self, SearchText, ReplaceText, i+1, p, Action);
      end;
  end;
  Editor.EndUpdate;
end;

end.

