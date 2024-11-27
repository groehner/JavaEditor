unit URegExSearch;

interface

uses RegularExpressions, SynEdit;

type
  TRegExSearch = class
  public
    myRegExpr: TRegEx;
    myRegExOptions: TRegExOptions;
    InputString: string;
    StartLine: integer;
    CurrentLine: integer;
    EndLine: integer;
    PosLine: integer;
    AEditor: TSynEdit;
    WholeWords: boolean;
    ReplaceText: string;
    procedure PrepareRegSearch(Editor: TSynEdit);
    procedure PrepareGrepRegSearch(Editor: TSynEdit);
    procedure DoRegSearchReplace(Replace: boolean);
    procedure DoGrepRegSearchReplace(Editor: TSynEdit);
  end;

var myRegExSearch: TRegExSearch = nil;

implementation

uses Windows, Controls, SynEditTypes,
     UJava, UDlgConfirmReplace, USearchOptions, UGrepResults;

function PointToDisplay(P: TPoint): TDisplayCoord;
begin
  Result.Column:= P.x;
  Result.Row:= P.Y;
end;

procedure TRegExSearch.PrepareRegSearch(Editor: TSynEdit);
begin
  AEditor:= Editor;
  myRegExOptions:= [roNotEmpty, roCompiled, roIgnoreCase];
  if mySearchOptions.CaseSensitive then
    exclude(myRegExOptions, roIgnoreCase);
  myRegExpr:= TRegEx.Create(MySearchOptions.SearchText, myRegExOptions);

  PosLine:= 1;
  if MySearchOptions.Backwards then begin
    StartLine:= Editor.Lines.Count;
    EndLine  := 0;
    if MySearchOptions.FromCursor then
      StartLine:= Editor.CaretY;
    if MySearchOptions.SelectionOnly then begin
      StartLine:= Editor.BlockEnd.Line;
      if Editor.SelText = ''
        then EndLine:= StartLine
        else EndLine:= Editor.BlockBegin.Line - 1;
      end;
    end
  else begin
    StartLine:= 1;
    EndLine  := Editor.Lines.Count+1;
    if MySearchOptions.FromCursor then begin
      StartLine:= Editor.CaretY;
      PosLine  := Editor.CaretX;
    end;
    if MySearchOptions.SelectionOnly then begin
      StartLine:=  Editor.BlockBegin.Line;
      PosLine  := Editor.BlockBegin.Char;
      if Editor.SelText = ''
        then EndLine:= StartLine
        else EndLine:= Editor.BlockEnd.Line + 1;
    end;
  end;
  InputString:= AEditor.Lines[StartLine-1];
  CurrentLine:= StartLine;
end;

procedure TRegExSearch.PrepareGrepRegSearch(Editor: TSynEdit);
begin
  myRegExOptions:= [roNotEmpty, roCompiled, roIgnoreCase];
  if MySearchOptions.CaseSensitive then
    exclude(myRegExOptions, roIgnoreCase);
  myRegExpr:= TRegEx.Create(MySearchOptions.SearchText, myRegExOptions);

  InputString:= Editor.Lines[0];
  AEditor:= Editor;
  ReplaceText:= MySearchOptions.ReplaceText;
  WholeWords:= MySearchOptions.WholeWords;
  PosLine  := 1;
  StartLine:= 1;
  EndLine  := Editor.Lines.Count + 1;
  CurrentLine:= StartLine;
end;

procedure TRegExSearch.DoRegSearchReplace(Replace: boolean);
  var EditRect: TRect;
      aMatch: TMatch;
      SearchText, aReplaceText: string; s1, s2: string;
      aAction: TSynReplaceAction;
      from, _to: TBufferCoord;
      APos: TDisplayCoord;
begin
  if MySearchOptions.SearchText = '' then exit;
  aAction:= raReplace;
  while CurrentLine <> EndLine do begin
    while (InputString <> '') and (PosLine <= Length(InputString)) and myRegExpr.IsMatch(InputString, PosLine) do begin
      aMatch:= myRegExpr.Match(InputString, PosLine);
      PosLine:= aMatch.Index + aMatch.Length;
      if MySearchOptions.WholeWords then begin
        s1:= AEditor.Lines[CurrentLine-1];
        from:= AEditor.WordStartEx(BufferCoord(aMatch.Index, CurrentLine));
        _to:= AEditor.WordEndEx(BufferCoord(aMatch.Index, CurrentLine));
        s2:= copy(s1, from.Char, _to.Char - from.Char);
        s1:= copy(s1, aMatch.Index, aMatch.Length);
        if s1 <> s2 then continue;  // continue with inner while-loop
      end;
      with AEditor do begin
        SelStart:= RowColToCharIndex(BufferCoord(aMatch.Index, CurrentLine));
        SelLength:= aMatch.Length;
      end;
      if not Replace then exit;
      SearchText:= AEditor.SelText;
      aReplaceText:= myRegExpr.Replace(MySearchOptions.SearchText, MySearchOptions.ReplaceText);
      if aAction = raReplace then begin
        APos:= DisplayCoord(aMatch.Index, CurrentLine);
        APos:= PointToDisplay(AEditor.ClientToScreen(AEditor.RowColumnToPixels(APos)));
        EditRect:= FJava.ClientRect;
        EditRect.TopLeft:= FJava.ClientToScreen(EditRect.TopLeft);
        EditRect.BottomRight:= FJava.ClientToScreen(EditRect.BottomRight);
        with TFConfirmReplace.Create(FJava) do begin
          PrepareShow(EditRect, APos.Column, APos.Row, APos.Row + AEditor.LineHeight, SearchText);
          case ShowModal of
            mrYes:      aAction:= raReplace;
            mrYesToAll: aAction:= raReplaceAll;
            mrNo:       aAction:= raSkip;
            else        aAction:= raCancel;
          end;
          Free;
        end;
      end;
      case aAction of
        raReplace:    AEditor.SelText:= aReplaceText;
        raReplaceAll: AEditor.SelText:= aReplaceText;
        raSkip:       aAction:= raReplace;
        raCancel:     begin AEditor.SelEnd:= AEditor.SelStart; exit end;
      end;
      if Pos(#13#10, aReplaceText) > 0 then begin
        inc(EndLine);
        break;
      end;
      
    end;
    PosLine:= 1;
    if MySearchOptions.Backwards
      then dec(CurrentLine)
      else inc(CurrentLine);
    if CurrentLine <> EndLine then
      InputString:= AEditor.Lines[CurrentLine-1];
  end;
  AEditor.SelEnd:= AEditor.SelStart;
  if aAction = raReplace then
    FJava.NotFound(AEditor, MySearchOptions.Backwards);
end;

procedure TRegExSearch.DoGrepRegSearchReplace(Editor: TSynEdit);
  var s: string;
      Action: TSynReplaceAction;
      aMatch: TMatch;
begin
  PrepareGrepRegSearch(Editor);
  while CurrentLine <> EndLine do begin
    while (InputString <> '') and (PosLine <= Length(InputString)) and myRegExpr.IsMatch(InputString, PosLine) do begin
      aMatch:= myRegExpr.Match(InputString, PosLine);
      PosLine:= aMatch.Index + aMatch.Length;
      with AEditor do begin
        SelStart:= RowColToCharIndex(BufferCoord(aMatch.Index, CurrentLine));
        SelLength:= aMatch.Index;
      end;
      if WholeWords then begin
        s:= AEditor.Lines[CurrentLine-1];
        s:= copy(s, aMatch.Index, aMatch.Length);
        if s <> AEditor.WordAtCursor then continue;  // continue with inner while-loop
      end;
      if MySearchOptions.Replace then begin
        s:= myRegExpr.Replace(InputString, ReplaceText);
        myGrepResults.FoundIt(Self, AEditor.SelText, s, CurrentLine, aMatch.Index, Action);
        AEditor.SelText:= s;
        end
      else
        myGrepResults.FoundIt(Self, AEditor.SelText, '', CurrentLine, aMatch.Index, Action);
    end;
    PosLine:= 1;
    inc(CurrentLine);
    if CurrentLine <> EndLine then
      InputString:= AEditor.Lines[CurrentLine-1];
  end;
  AEditor.SelEnd:= AEditor.SelStart;
end;

end.

