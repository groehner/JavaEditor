unit URegExSearch;

interface

uses
  RegularExpressions,
  SynEdit;

type
  TRegExSearch = class
  private
    FAEditor: TSynEdit;
    FCurrentLine: Integer;
    FEndLine: Integer;
    FInputString: string;
    FMyRegExOptions: TRegExOptions;
    FMyRegExpr: TRegEx;
    FPosLine: Integer;
    FReplaceText: string;
    FStartLine: Integer;
    FWholeWords: Boolean;
  public
    procedure PrepareRegSearch(Editor: TSynEdit);
    procedure PrepareGrepRegSearch(Editor: TSynEdit);
    procedure DoRegSearchReplace(Replace: Boolean);
    procedure DoGrepRegSearchReplace(Editor: TSynEdit);
    property AEditor: TSynEdit read FAEditor;
    property CurrentLine: Integer read FCurrentLine;
    property EndLine: Integer read FEndLine;
    property InputString: string read FInputString;
    property MyRegExOptions: TRegExOptions read FMyRegExOptions;
    property MyRegExpr: TRegEx read FMyRegExpr;
    property PosLine: Integer read FPosLine;
    property ReplaceText: string read FReplaceText;
    property StartLine: Integer read FStartLine;
    property WholeWords: Boolean read FWholeWords;
  end;

var
  MyRegExSearch: TRegExSearch = nil;

implementation

uses
  Windows,
  Controls,
  SynEditTypes,
  UJava,
  UDlgConfirmReplace,
  USearchOptions,
  UGrepResults;

function PointToDisplay(Posi: TPoint): TDisplayCoord;
begin
  Result.Column := Posi.X;
  Result.Row := Posi.Y;
end;

procedure TRegExSearch.PrepareRegSearch(Editor: TSynEdit);
begin
  FAEditor := Editor;
  FMyRegExOptions := [roNotEmpty, roCompiled, roIgnoreCase];
  if MySearchOptions.CaseSensitive then
    Exclude(FMyRegExOptions, roIgnoreCase);
  FMyRegExpr := TRegEx.Create(MySearchOptions.SearchText, FMyRegExOptions);

  FPosLine := 1;
  if MySearchOptions.Backwards then
  begin
    FStartLine := Editor.Lines.Count;
    FEndLine := 0;
    if MySearchOptions.FromCursor then
      FStartLine := Editor.CaretY;
    if MySearchOptions.SelectionOnly then
    begin
      FStartLine := Editor.BlockEnd.Line;
      if Editor.SelText = '' then
        FEndLine := FStartLine
      else
        FEndLine := Editor.BlockBegin.Line - 1;
    end;
  end
  else
  begin
    FStartLine := 1;
    FEndLine := Editor.Lines.Count + 1;
    if MySearchOptions.FromCursor then
    begin
      FStartLine := Editor.CaretY;
      FPosLine := Editor.CaretX;
    end;
    if MySearchOptions.SelectionOnly then
    begin
      FStartLine := Editor.BlockBegin.Line;
      FPosLine := Editor.BlockBegin.Char;
      if Editor.SelText = '' then
        FEndLine := FStartLine
      else
        FEndLine := Editor.BlockEnd.Line + 1;
    end;
  end;
  FInputString := FAEditor.Lines[FStartLine - 1];
  FCurrentLine := FStartLine;
end;

procedure TRegExSearch.PrepareGrepRegSearch(Editor: TSynEdit);
begin
  FMyRegExOptions := [roNotEmpty, roCompiled, roIgnoreCase];
  if MySearchOptions.CaseSensitive then
    Exclude(FMyRegExOptions, roIgnoreCase);
  FMyRegExpr := TRegEx.Create(MySearchOptions.SearchText, FMyRegExOptions);

  FInputString := Editor.Lines[0];
  FAEditor := Editor;
  FReplaceText := MySearchOptions.ReplaceText;
  FWholeWords := MySearchOptions.WholeWords;
  FPosLine := 1;
  FStartLine := 1;
  FEndLine := Editor.Lines.Count + 1;
  FCurrentLine := FStartLine;
end;

procedure TRegExSearch.DoRegSearchReplace(Replace: Boolean);
var
  EditRect: TRect;
  Match: TMatch;
  SearchText, ReplaceText: string;
  Str1, Str2: string;
  AAction: TSynReplaceAction;
  From, To_: TBufferCoord;
  APos: TDisplayCoord;
begin
  if MySearchOptions.SearchText = '' then
    Exit;
  AAction := raReplace;
  while FCurrentLine <> FEndLine do
  begin
    while (FInputString <> '') and (FPosLine <= Length(FInputString)) and
      FMyRegExpr.IsMatch(FInputString, FPosLine) do
    begin
      Match := FMyRegExpr.Match(FInputString, FPosLine);
      FPosLine := Match.Index + Match.Length;
      if MySearchOptions.WholeWords then
      begin
        Str1 := FAEditor.Lines[FCurrentLine - 1];
        From := FAEditor.WordStartEx(BufferCoord(Match.Index, FCurrentLine));
        To_ := FAEditor.WordEndEx(BufferCoord(Match.Index, FCurrentLine));
        Str2 := Copy(Str1, From.Char, To_.Char - From.Char);
        Str1 := Copy(Str1, Match.Index, Match.Length);
        if Str1 <> Str2 then
          Continue; // continue with inner while-loop
      end;
      with FAEditor do
      begin
        SelStart := RowColToCharIndex(BufferCoord(Match.Index, FCurrentLine));
        SelLength := Match.Length;
      end;
      if not Replace then
        Exit;
      SearchText := FAEditor.SelText;
      ReplaceText := FMyRegExpr.Replace(MySearchOptions.SearchText,
        MySearchOptions.ReplaceText);
      if AAction = raReplace then
      begin
        APos := DisplayCoord(Match.Index, FCurrentLine);
        APos := PointToDisplay
          (FAEditor.ClientToScreen(FAEditor.RowColumnToPixels(APos)));
        EditRect := FJava.ClientRect;
        EditRect.TopLeft := FJava.ClientToScreen(EditRect.TopLeft);
        EditRect.BottomRight := FJava.ClientToScreen(EditRect.BottomRight);
        with TFConfirmReplace.Create(FJava) do
        begin
          PrepareShow(EditRect, APos.Column, APos.Row,
            APos.Row + FAEditor.LineHeight, SearchText);
          case ShowModal of
            mrYes:
              AAction := raReplace;
            mrYesToAll:
              AAction := raReplaceAll;
            mrNo:
              AAction := raSkip;
          else
            AAction := raCancel;
          end;
          Free;
        end;
      end;
      case AAction of
        raReplace:
          FAEditor.SelText := ReplaceText;
        raReplaceAll:
          FAEditor.SelText := ReplaceText;
        raSkip:
          AAction := raReplace;
        raCancel:
          begin
            FAEditor.SelEnd := FAEditor.SelStart;
            Exit;
          end;
      end;
      if Pos(#13#10, ReplaceText) > 0 then
      begin
        Inc(FEndLine);
        Break;
      end;
    end;
    FPosLine := 1;
    if MySearchOptions.Backwards then
      Dec(FCurrentLine)
    else
      Inc(FCurrentLine);
    if FCurrentLine <> FEndLine then
      FInputString := FAEditor.Lines[FCurrentLine - 1];
  end;
  FAEditor.SelEnd := FAEditor.SelStart;
  if AAction = raReplace then
    FJava.NotFound(FAEditor, MySearchOptions.Backwards);
end;

procedure TRegExSearch.DoGrepRegSearchReplace(Editor: TSynEdit);
var
  Str: string;
  Action: TSynReplaceAction;
  Match: TMatch;
begin
  PrepareGrepRegSearch(Editor);
  while FCurrentLine <> FEndLine do
  begin
    while (FInputString <> '') and (FPosLine <= Length(FInputString)) and
      FMyRegExpr.IsMatch(FInputString, FPosLine) do
    begin
      Match := FMyRegExpr.Match(FInputString, FPosLine);
      FPosLine := Match.Index + Match.Length;
      with FAEditor do
      begin
        SelStart := RowColToCharIndex(BufferCoord(Match.Index, FCurrentLine));
        SelLength := Match.Index;
      end;
      if FWholeWords then
      begin
        Str := FAEditor.Lines[FCurrentLine - 1];
        Str := Copy(Str, Match.Index, Match.Length);
        if Str <> FAEditor.WordAtCursor then
          Continue; // continue with inner while-loop
      end;
      if MySearchOptions.Replace then
      begin
        Str := FMyRegExpr.Replace(FInputString, ReplaceText);
        MyGrepResults.FoundIt(Self, FAEditor.SelText, Str, FCurrentLine,
          Match.Index, Action);
        FAEditor.SelText := Str;
      end
      else
        MyGrepResults.FoundIt(Self, FAEditor.SelText, '', FCurrentLine,
          Match.Index, Action);
    end;
    FPosLine := 1;
    Inc(FCurrentLine);
    if FCurrentLine <> FEndLine then
      FInputString := FAEditor.Lines[FCurrentLine - 1];
  end;
  FAEditor.SelEnd := FAEditor.SelStart;
end;

end.
