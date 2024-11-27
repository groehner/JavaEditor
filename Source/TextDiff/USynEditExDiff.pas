unit USynEditExDiff;

interface

uses Classes, Graphics, ExtCtrls,
     USynEditEx, SynEdit, SynEditTextBuffer;

type

  TLineObj = class
  public
    Spezial: boolean;
    BackClr: TColor;
    Tag: longint;
  end;

  TSynEditExDiff = class(TSynEditEx)
    private
      myOwner: TComponent;
      fCurrLongestLineLen: integer; //needed for horizontal scrollbar
      fLineModClr: TColor;
      fYellowGray: TColor;
      fSilveryGray: TColor;
      ModifiedStrs: array[boolean] of string;
      InsertModeStrs: array[boolean] of string;
    private
      procedure DeleteObjects(from, _to: integer);
    public
      Encoding: string;   // ANSI, UTF-8, UTF-16
      LineBreak: string;  // Windows, Unix, Mac
      withColoredLines: boolean;
      Nr: integer;
      Pathname: string;
      EditFormNr: integer;
      PCaption: TPanel;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure DoUndo;
      procedure DoRedo;
      procedure Enter(Sender: TObject);
      procedure DoExit1(Sender: TObject);
      function  GetLineObj(index: integer): TLineObj;
      procedure InsertItem(index: integer; const s: string; LineObject: TLineObj);
      procedure SynEditorSpecialLineColors(Sender: TObject;
                  Line: Integer; var Special: Boolean; var FG, BG: TColor);
      procedure GutterTextEvent(Sender: TObject; aLine: Integer;
                  var aText: string);
      procedure SynEditorReplaceText(Sender: TObject;
        const ASearch, AReplace: string; Line, Column: Integer;
        var aAction: TSynReplaceAction);
      procedure CodeEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
      procedure Load(Lines12: TSynEditStringList; const aPathname: string);
      procedure Save(WithBackup: boolean);
      procedure SetHighlighter;
      procedure LinesClearAll;
      procedure CreateObjects; overload;
      procedure CreateObjects(from, _to, aTag: integer); overload;
      procedure SetEncoding(aEncoding: string);
      function EncodingAsString(const aEncoding: string): string;
      function LinebreakAsString: string;
      function LinebreakAsCtrls(const s: string): string;
      procedure UpdateState;
      procedure ShowFilename;
      procedure SetModified(value: boolean);
      function  CopyToClipboardFT(from, _to: integer): boolean;
      procedure PasteFromClipboardFT(emptyClipboard: boolean; from, _to, aNr: integer);
      procedure ChangeStyle; override;
    end;

implementation

uses SysUtils, Windows, Controls, Forms, Clipbrd,
     JvGnugettext, UStringRessources,
     UTextDiffform, UDlgConfirmReplace, UBaseForm, UUtils,
     SynEditTypes, {$WARNINGS OFF} FileCtrl, {$WARNINGS ON}
     UConfiguration, UJava;

constructor TSynEditExDiff.Create(AOwner: TComponent);
begin
  inherited;
  myOwner:= AOwner;
  fCurrLongestLineLen:= 60;
  withColoredLines:= false;
  Nr:= 0;
  Pathname:= '';
  EditFormNr:= -1;
  ModifiedStrs[false]:= '';
  ModifiedStrs[true]:= _(LNGModified);
  InsertModeStrs[false]:= _(LNGModusOverwrite);
  InsertModeStrs[true]:= _(LNGModusInsert);
  SearchEngine:= FJava.SynEditSearch;
  Gutter.ShowLineNumbers:= true;
  Gutter.DigitCount:= 0;
  Gutter.LeftOffset:= 4;
  Gutter.Autosize:= true;
  MaxUndo:= 300;
  TabWidth:= 2;
  WantTabs:= True;
  Options:= [eoAutoIndent, eoDragDropEditing, eoScrollPastEol, eoShowScrollHint,
             eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces,
             eoSmartTabDelete, eoGroupUndo, eoKeepCaretX, eoEnhanceHomeKey];
  Font.Assign(FConfiguration.EditFont);
  Gutter.Font.Assign(FConfiguration.Font);
  Gutter.Font.Height:= Font.Height + 2;
  OnKeyUp:= CodeEditKeyUp;
  OnStatusChange:= EditorStatusChange;
  OnReplaceText:= SynEditorReplaceText;
  OnEnter:= Enter;
  OnExit:= DoExit1;
  ChangeStyle;
end;

destructor TSynEditExDiff.Destroy;
begin
  OnStatusChange:= nil;
  OnEnter:= nil;
  OnExit:= nil;
  LinesClearAll;
  inherited;
end;

procedure TSynEditExDiff.DoUndo;
begin
  Undo;
  CreateObjects;
end;

procedure TSynEditExDiff.DoRedo;
begin
  Redo;
  CreateObjects;
end;

procedure TSynEditExDiff.Enter(Sender: TObject);
begin
  Gutter.Color:= fYellowGray;
  EditorStatusChange(Sender, [scAll]);
  if withColoredLines
    then (myOwner as TFTextDiff).ShowDiffState;
end;

procedure TSynEditExDiff.DoExit1(Sender: TObject);
begin
  inherited;
  Gutter.Color:= fSilveryGray; // silvery gray
end;

procedure TSynEditExDiff.CodeEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with myOwner as TFTextDiff do
    if FilesCompared and (Key in [VK_Up, VK_Down, VK_Prior, VK_Next]) then
      SyncScroll(Self, sbVertical) else
    if (Key = VK_Return) or ([ssCtrl] = Shift) and (Key = Ord('V'))
      then CreateObjects;
end;

function TSynEditExDiff.GetLineObj(index: integer): TLineObj;
begin
  if Lines.count = 0 then
    result:= nil
  else begin
    if (index < 0) or (index >= Lines.count) then
      raise Exception.Create('TLines.GetLineObj() - index out of bounds.');
    result:= TLineObj(Lines.objects[index]);
  end;  
end;

procedure TSynEditExDiff.InsertItem(index: integer; const s: string; LineObject: TLineObj);
begin
  inherited Lines.InsertObject(index, s, LineObject);
end;

procedure TSynEditExDiff.SynEditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if (Line > Lines.Count) or not withColoredLines then
    exit;
  var Lo1:= GetLineObj(Line-1);
  if Lo1 = nil then begin
     // BG:= clRed;      // good for debugging
    BG:= fLineModClr;
    Special:= true;
  end else
   with Lo1 do
    if Spezial then begin
      BG:= BackClr;
      Special:= true;
    end;
end;

procedure TSynEditExDiff.UpdateState;
begin
  with FJava do begin
    SetEnabledMI(MIUndo, CanUndo);
    SetEnabledTB(TBUndo, MIUndo.Enabled);
    SetEnabledMI(MIRedo, CanRedo);
    SetEnabledTB(TBRedo, MIRedo.Enabled);

    var Selection:= SelAVail;
    SetEnabledMI(MICut, Selection);
    SetEnabledMI(MICopy, Selection);
    SetEnabledMI(MICopyRTF, Selection);
    SetEnabledMI(MICopyHTML, Selection);
    SetEnabledMI(MICopyHTMLAsText, Selection);
    SetEnabledMI(MICopyNumbered, Selection);
    SetEnabledMI(MICopyRtfNumbered, Selection);
    SetEnabledMI(MIPaste, CanPaste);
  end;
  with myOwner as TFTextDiff do begin
    SetEnabledMI(MIUndo,CanUndo);
    SetEnabledMI(MIRedo, CanRedo);
    SetEnabledMI(MICut, SelAvail);
    SetEnabledMI(MICopy,SelAvail);
    SetEnabledMI(MIPaste, CanPaste);
  end;  
end;

procedure TSynEditExDiff.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if myOwner = nil then exit;

  (MyOwner as TFTextDiff).Nr:= Nr;
  (MyOwner as TFTextDiff).liLineColumn.Caption:= Format(' %4d : %3d ', [CaretY, CaretX]);
  if Changes * [scModified] <> [] then begin
    (MyOwner as TFTextDiff).liModified.Caption:= ModifiedStrs[Modified];
    ShowFilename;
    if EditFormNr = -1 then begin
      var aForm:= TFForm(FJava.getTDIWindowType(Pathname, '%E%'));
      if assigned(aForm) then EditFormNr:= aForm.Number;
    end;
    if EditFormNr > -1 then FJava.TabModified(EditFormNr, Modified);
  end;
  (MyOwner as TFTextDiff).liInsOvr.Caption:= InsertModeStrs[InsertMode];
  (MyOwner as TFTextDiff).liEncoding.Caption:= ' ' + Encoding + '/' + LinebreakAsString + ' ';
  UpdateState;
end;

procedure TSynEditExDiff.GutterTextEvent(Sender: TObject; aLine: Integer;
           var aText: string);
begin
  if withColoredLines then begin
    var LineObject:= GetLineObj(aLine-1);
    if LineObject = nil then exit;
    with LineObject do
      if Tag = 0
        then aText:= ''
        else aText:= InttoStr(Tag);
  end else
    aText:= IntToStr(aLine);
end;

procedure TSynEditExDiff.Load(Lines12: TSynEditStringList; const aPathname: string);
begin
  try
    Lines12.LoadFromFile(aPathname);
  except on e: Exception do
    FConfiguration.Log('TSynEditExDiff.Load: ' + aPathname, e);
  end;
  Lines.BeginUpdate;
  LinesClearAll;
  Lines.Assign(Lines12);
  Lines.EndUpdate;
  self.Pathname:= aPathname;
  Encoding:= EncodingAsString(Lines12.Encoding.EncodingName);
  Linebreak:= Lines12.LineBreak;
  withColoredLines:= false;
  SetHighlighter;
  SetModified(false);
  (myOwner as TFTextDiff).setActiveControl(Self);
end;

procedure TSynEditExDiff.Save(WithBackup: boolean);
  var i: integer;
      BackupName, Ext: string;
      aLine: TLineObj;
begin
  if withColoredLines then begin
    BeginUpdate;
    for i:= Lines.Count - 1 downto 0 do begin
      aLine:= GetLineObj(i);
      if assigned(aLine) then
        if aLine.Tag = 0 then begin
          FreeAndNil(aLine);
          Lines.Delete(i);
          end
        else
          aLine.Spezial:= false;
    end;
    withColoredLines:= false;
    EndUpdate;
    Invalidate;
  end;

  if Modified then
    try
      if WithBackup then begin
        BackupName:= Pathname;
        Ext:= ExtractFileExt(Pathname);
        if length(Ext) > 1 then Ext[2]:= '~';
        BackupName:= ChangeFileExt(BackupName, Ext);
        if FileExists(BackupName) then
          SysUtils.DeleteFile(BackupName);
        if FileExists(Pathname) then
          RenameFile(Pathname, BackupName);
      end;
      {if Encoding = seUTF8
        then SaveToFile(Lines, Pathname, Encoding, false)
        else SaveToFile(Lines, Pathname, Encoding, true);}
      Lines.SaveToFile(Pathname); // TODO
      SetModified(false);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
end;

procedure TSynEditExDiff.SetHighlighter;
begin
  var s:= Lowercase(ExtractFileExt(Pathname));
  Highlighter:= FConfiguration.GetHighlighter(s);
  if Highlighter = nil then
    OnPaintTransient:= nil;
end;

function TSynEditExDiff.EncodingAsString(const aEncoding: string): string;
begin
  Result:= aEncoding;
  if Pos('ANSI', Result) > 0 then Result:= 'ANSI' else
  if Pos('ASCII', Result) > 0 then Result:= 'ASCII' else
  if Pos('UTF-8', Result) > 0 then Result:= 'UTF-8' else
  if Pos('UTF-16', Result) > 0 then Result:= 'UTF-16' else
  if Pos('Unicode', Result) > 0 then Result:= 'UTF-16';
end;

procedure TSynEditExDiff.SetEncoding(aEncoding: string);
begin
  Self.Encoding:= EncodingAsString(aEncoding);
  var p:= Pos('/', aEncoding);
  delete(aEncoding, 1, p);
  Linebreak:= LinebreakAsCtrls(aEncoding);
end;

function TSynEditExDiff.LinebreakAsString: string;
begin
  if LineBreak = #13#10 then Result:= 'Windows' else
  if LineBreak = #10 then Result:= 'Unix'
  else Result:= 'Mac';
end;

function TSynEditExDiff.LinebreakAsCtrls(const s: string): string;
begin
  if s = 'Windows' then Result:= #13#10 else
  if s = 'Unix' then Result:= #10
  else Result:= #13;
end;

procedure TSynEditExDiff.LinesClearAll;
begin
  DeleteObjects(0, Lines.Count-1);
  Lines.Clear;
  OnSpecialLineColors:= nil;
  OnGutterGetText:= nil;
end;

procedure TSynEditExDiff.DeleteObjects(from, _to: integer);
begin
  for var i:= from to _to do begin
    var LineObject:= GetLineObj(i);
    if assigned(LineObject) then begin
      FreeAndNil(LineObject);
      Lines.objects[i]:= nil
    end;  
  end;
end;

procedure TSynEditExDiff.CreateObjects;
  var LineObject: TLineObj;
begin
  if not withColoredLines then exit;
  Lines.BeginUpdate;
  var i:= 0;
  while (i < Lines.Count) and assigned(GetLineObj(i)) do
    inc(i);
  while (i < Lines.Count) and (GetLineObj(i) = nil) do begin
    LineObject:= TLineObj.Create;
    LineObject.Spezial:= true;
    LineObject.BackClr:= fLineModClr;
    LineObject.Tag:= i+1;
    Lines.Objects[i]:= LineObject;
    inc(i);
  end;
  Lines.EndUpdate;
  Invalidate;
end;

procedure TSynEditExDiff.CreateObjects(from, _to, aTag: integer);
  var LineObject: TLineObj;
begin
  for var i:= from to _to do
    if GetLineObj(i) = nil then begin
      LineObject:= TLineObj.Create;
      LineObject.Spezial:= true;
      LineObject.BackClr:= fLineModClr;
      LineObject.Tag:= aTag;
      if aTag > 0 then inc(aTag);
      Lines.Objects[i]:= LineObject;
    end;
end;

function PointToDisplay(P: TPoint): TDisplayCoord;
begin
  Result.Column:= P.x;
  Result.Row:= P.Y;
end;

procedure TSynEditExDiff.SynEditorReplaceText(Sender: TObject;
  const ASearch, AReplace: string; Line, Column: Integer;
  var aAction: TSynReplaceAction);
begin
  if ASearch = AReplace then
    aAction := raSkip
  else begin
    var APos := DisplayCoord(Column, Line);
    APos := PointToDisplay(ClientToScreen(RowColumnToPixels(APos)));
    var EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    with TFConfirmReplace.Create(Application) do begin
      PrepareShow(EditRect, APos.Column, APos.Row, APos.Row + LineHeight, ASearch);
      case ShowModal of
        mrYes:      aAction := raReplace;
        mrYesToAll: aAction := raReplaceAll;
        mrNo:       aAction := raSkip;
        else        aAction := raCancel;
      end;
      Free;
    end;
  end;
end;

procedure TSynEditExDiff.ShowFilename;
  var s: string; aCanvas: TCanvas;
begin
  aCanvas:= (myOwner as TFTextDiff).Canvas;
  aCanvas.Font.Assign(PCaption.Font);
  s:= ' ' + Pathname;
  if Modified then s:= s + '*';
  PCaption.Caption:= MinimizeName(s, aCanvas, PCaption.Width);
end;

procedure TSynEditExDiff.SetModified(value: boolean);
begin
  Modified:= value;
  ShowFilename;
end;

function TSynEditExDiff.CopyToClipboardFT(from, _to: integer): boolean;
begin
  SelStart:= RowColToCharIndex(BufferCoord(1, from+1));
  if _to + 2 > Lines.Count
    then SelEnd:= RowColToCharIndex(BufferCoord(length(Lines[Lines.count-1])+1, _to+1))
    else SelEnd:= RowColToCharIndex(BufferCoord(1, _to+2)) - 2;
  Result:= (SelLength = 0);
  if Result
    then Clipboard.Clear
    else CopyToClipboard;
  SelLength:= 0;
end;

procedure TSynEditExDiff.PasteFromClipboardFT(emptyClipboard: boolean; from, _to, aNr: integer);
begin
  DeleteObjects(from, _to);
  SelStart:= RowColToCharIndex(BufferCoord(1, from+1));
  if _to + 2 > Lines.Count
    then SelEnd:= RowColToCharIndex(BufferCoord(length(Lines[Lines.count-1])+1, _to+1))
    else SelEnd:= RowColToCharIndex(BufferCoord(1, _to+2)) - 2;
  try
    if emptyClipboard then begin
      CutToClipboard;
      Clipboard.Clear
    end else
      PasteFromClipboard;
  except
  end;
  CreateObjects(from, _to, aNr);
  SetModified(true);
  Invalidate;
end;

procedure TSynEditExDiff.ChangeStyle;
begin
  if FConfiguration.isDark then begin
    fLineModClr:= $878787;
    fYellowGray:= $519595;
    fSilveryGray:= $777777;
  end else begin
    fLineModClr:= $E0E0E0;
    fYellowGray:= $97734F;
    fSilveryGray:= $16A231;
  end;
  inherited;
  EditorStatusChange(Self, []);
end;

end.
