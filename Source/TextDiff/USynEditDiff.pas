{ -------------------------------------------------------------------------------
  Unit:     USynEditDiff
  Author:   Gerhard Röhner
  Date:     2011
  Purpose:  SynEdit descendent for TextDiff
  ------------------------------------------------------------------------------- }

unit USynEditDiff;

interface

uses
  Classes,
  Graphics,
  ExtCtrls,
  Forms,
  SynEdit,
  SynEditTextBuffer;

type

  TLineObj = class
  private
    FBackClr: TColor;
    FSpezial: Boolean;
    FTag: LongInt;
  public
    property BackClr: TColor read FBackClr write FBackClr;
    property Spezial: Boolean read FSpezial write FSpezial;
    property Tag: LongInt read FTag write FTag;
  end;

  TSynEditDiff = class(TSynEdit)
  private
    FMyOwner: TComponent;
    FCurrLongestLineLen: Integer; // needed for horizontal scrollbar
    FEditFormNr: Integer;
    FLineModClr: TColor;
    FYellowGray: TColor;
    FSilveryGray: TColor;
    FModifiedStrs: array [Boolean] of string;
    FInsertModeStrs: array [Boolean] of string;
    FEncoding: string; // ANSI, UTF-8, UTF-16
    FLineBreak: string; // Windows, Unix, Mac
    FNumber: Integer;
    FPathname: string;
    FPCaption: TPanel;
    FWithColoredLines: Boolean;
    procedure DeleteObjects(From, Till: Integer);
    procedure DoExit1(Sender: TObject);
    procedure SynEditorReplaceText(Sender: TObject;
      const ASearch, AReplace: string; Line, Column: Integer;
      var AAction: TSynReplaceAction);
    procedure CodeEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SetHighlighter;
    procedure CreateObjects(From, Till, Tag: Integer); overload;
    function EncodingAsString(const Encoding: string): string;
    function LinebreakAsString: string;
    procedure UpdateState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoUndo;
    procedure DoRedo;
    procedure Enter(Sender: TObject);
    function GetLineObj(Index: Integer): TLineObj;
    procedure InsertItem(Index: Integer; const Str: string;
      LineObject: TLineObj);
    procedure SynEditorSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var Foreground, Background: TColor);
    procedure GutterTextEvent(Sender: TObject; ALine: Integer;
      var AText: string);
    procedure Load(Lines12: TSynEditStringList; const APathname: string);
    procedure Save(WithBackup: Boolean);
    procedure LinesClearAll;
    procedure CreateObjects; overload;
    procedure ShowFilename;
    procedure SetModified(Value: Boolean);
    function CopyToClipboardFT(From, Till: Integer): Boolean;
    procedure PasteFromClipboardFT(EmptyClipboard: Boolean; From, Till, Num: Integer);
    procedure ChangeStyle;
    procedure SyncScroll(Sender: TObject; ScrollBar: TScrollBarKind);

    property Number: Integer read FNumber write FNumber;
    property Pathname: string read FPathname;
    property PCaption: TPanel read FPCaption write FPCaption;
    property WithColoredLines: Boolean read FWithColoredLines
      write FWithColoredLines;
  end;

implementation

uses
  SysUtils,
  Windows,
  Controls,
  Clipbrd,
  FileCtrl,
  JvGnugettext,
  UStringRessources,
  UTextDiffForm,
  UDlgConfirmReplace,
  UUtils,
  SynEditTypes,
  UConfiguration,
  UJava;

constructor TSynEditDiff.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMyOwner := AOwner;
  FCurrLongestLineLen := 60;
  FWithColoredLines := False;
  FNumber := 0;
  FPathname := '';
  FEditFormNr := -1;
  FModifiedStrs[False] := '';
  FModifiedStrs[True] := _(LNGModified);
  FInsertModeStrs[False] := _(LNGModusOverwrite);
  FInsertModeStrs[True] := _(LNGModusInsert);
  SearchEngine := FJava.SynEditSearch;
  Gutter.ShowLineNumbers := True;
  Gutter.DigitCount := 0;
  Gutter.AutoSize := True;
  MaxUndo := 300;
  TabWidth := 2;
  WantTabs := True;
  Options := [eoAutoIndent, eoDragDropEditing, eoSmartTabs, eoTabIndent,
    eoTabsToSpaces, eoTrimTrailingSpaces, eoSmartTabDelete, eoGroupUndo,
    eoKeepCaretX, eoEnhanceHomeKey];
  ScrollOptions := [eoScrollPastEol, eoShowScrollHint];
  Font.Assign(FConfiguration.Font);
  Gutter.Font.Assign(FConfiguration.Font);
  Gutter.Font.Height := Font.Height + 2;
  OnKeyUp := CodeEditKeyUp;
  OnStatusChange := EditorStatusChange;
  OnReplaceText := SynEditorReplaceText;
  OnEnter := Enter;
  OnExit := DoExit1;
  OnScroll := SyncScroll;
  ChangeStyle;
end;

destructor TSynEditDiff.Destroy;
begin
  OnStatusChange := nil;
  OnEnter := nil;
  OnExit := nil;
  TSynEditStringList(Lines).OnCleared := nil;
  LinesClearAll;
  inherited;
end;

procedure TSynEditDiff.DoUndo;
begin
  Undo;
  CreateObjects;
end;

procedure TSynEditDiff.DoRedo;
begin
  Redo;
  CreateObjects;
end;

procedure TSynEditDiff.Enter(Sender: TObject);
begin
  Gutter.Color := FYellowGray;
  EditorStatusChange(Sender, [scAll]);
  if FWithColoredLines then
    (FMyOwner as TFTextDiff).ShowDiffState;
end;

procedure TSynEditDiff.DoExit1(Sender: TObject);
begin
  Gutter.Color := FSilveryGray;
end;

procedure TSynEditDiff.CodeEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with FMyOwner as TFTextDiff do
    if FilesCompared and (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) then
      SyncScroll(Self, sbVertical)
    else if (Key = VK_RETURN) or ([ssCtrl] = Shift) and (Key = Ord('V')) then
      CreateObjects;
end;

function TSynEditDiff.GetLineObj(Index: Integer): TLineObj;
begin
  if Lines.Count = 0 then
    Result := nil
  else
  begin
    if (Index < 0) or (Index >= Lines.Count) then
      raise Exception.Create('TLines.GetLineObj() - index out of bounds.');
    Result := TLineObj(Lines.Objects[Index]);
  end;
end;

procedure TSynEditDiff.InsertItem(Index: Integer; const Str: string;
  LineObject: TLineObj);
begin
  inherited Lines.InsertObject(Index, Str, LineObject);
end;

procedure TSynEditDiff.SynEditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var Foreground, Background: TColor);
begin
  if (Line > Lines.Count) or not FWithColoredLines then
    Exit;
  var
  Lo1 := GetLineObj(Line - 1);
  if not Assigned(Lo1) then
  begin
    // Background:= clRed;      // good for debugging
    Background := FLineModClr;
    Special := True;
  end
  else
    with Lo1 do
      if Spezial then
      begin
        Background := BackClr;
        Special := True;
      end;
end;

procedure TSynEditDiff.UpdateState;
begin
  with FJava do
  begin
    SetEnabledMI(MIUndo, CanUndo);
    SetEnabledTB(TBUndo, MIUndo.Enabled);
    SetEnabledMI(MIRedo, CanRedo);
    SetEnabledTB(TBRedo, MIRedo.Enabled);

    var
    Selection := SelAvail;
    SetEnabledMI(MICut, Selection);
    SetEnabledMI(MICopy, Selection);
    SetEnabledMI(MICopyRTF, Selection);
    SetEnabledMI(MICopyHTML, Selection);
    SetEnabledMI(MICopyHTMLAsText, Selection);
    SetEnabledMI(MICopyNumbered, Selection);
    SetEnabledMI(MICopyRtfNumbered, Selection);
    SetEnabledMI(MIPaste, CanPaste);
  end;
  with FMyOwner as TFTextDiff do
  begin
    SetEnabledMI(MIUndo, CanUndo);
    SetEnabledMI(MIRedo, CanRedo);
    SetEnabledMI(MICut, SelAvail);
    SetEnabledMI(MICopy, SelAvail);
    SetEnabledMI(MIPaste, CanPaste);
  end;
end;

procedure TSynEditDiff.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if not Assigned(FMyOwner) then
    Exit;

  (FMyOwner as TFTextDiff).CodeEditNumber := FNumber;
  (FMyOwner as TFTextDiff).liLineColumn.Caption :=
    Format(' %4d : %3d ', [CaretY, CaretX]);
  if Changes * [scModified] <> [] then
  begin
    (FMyOwner as TFTextDiff).liModified.Caption := FModifiedStrs[Modified];
    ShowFilename;
    if FEditFormNr = -1 then
    begin
      var
      AForm := FJava.GetTDIWindowType(FPathname, '%E%');
      if Assigned(AForm) then
        FEditFormNr := AForm.Number;
    end;
    if FEditFormNr > -1 then
      FJava.TabModified(FEditFormNr, Modified);
  end;
  (FMyOwner as TFTextDiff).liInsOvr.Caption := FInsertModeStrs[InsertMode];
  (FMyOwner as TFTextDiff).liEncoding.Caption := ' ' + FEncoding + '/' +
    LinebreakAsString + ' ';
  UpdateState;
end;

procedure TSynEditDiff.GutterTextEvent(Sender: TObject; ALine: Integer;
  var AText: string);
begin
  if FWithColoredLines then
  begin
    var
    LineObject := GetLineObj(ALine - 1);
    if not Assigned(LineObject) then
      Exit;
    with LineObject do
      if Tag = 0 then
        AText := ''
      else
        AText := IntToStr(Tag);
  end
  else
    AText := IntToStr(ALine);
end;

procedure TSynEditDiff.Load(Lines12: TSynEditStringList;
  const APathname: string);
begin
  try
    Lines12.LoadFromFile(APathname);
  except
    on e: Exception do
      ErrorMsg('TSynEditDiff.Load: ' + APathname);
  end;
  Lines.BeginUpdate;
  LinesClearAll;
  Lines.Assign(Lines12);
  Lines.EndUpdate;
  Self.FPathname := APathname;

  FEncoding := EncodingAsString(Lines12.Encoding.EncodingName);
  FLineBreak := Lines12.LineBreak;

  FWithColoredLines := False;
  SetHighlighter;
  SetModified(False);
end;

procedure TSynEditDiff.Save(WithBackup: Boolean);
var
  BackupName, Ext: string;
  Line: TLineObj;
begin
  if FWithColoredLines then
  begin
    BeginUpdate;
    for var I := Lines.Count - 1 downto 0 do
    begin
      Line := GetLineObj(I);
      if Assigned(Line) then
        if Line.FTag = 0 then
        begin
          FreeAndNil(Line);
          Lines.Delete(I);
        end
        else
          Line.FSpezial := False;
    end;
    FWithColoredLines := False;
    EndUpdate;
    Invalidate;
  end;

  if Modified then
    try
      if WithBackup then
      begin
        BackupName := FPathname;
        Ext := ExtractFileExt(FPathname);
        if Length(Ext) > 1 then
          Ext[2] := '~';
        BackupName := ChangeFileExt(BackupName, Ext);
        if FileExists(BackupName) then
          SysUtils.DeleteFile(BackupName);
        if FileExists(FPathname) then
          RenameFile(FPathname, BackupName);
      end;
      Lines.SaveToFile(FPathname);
      SetModified(False);
    except
      on E: Exception do
        ErrorMsg(Format(_(LNGCanNotWrite), [FPathname]));
    end;
end;

procedure TSynEditDiff.SetHighlighter;
begin
  var Str := LowerCase(ExtractFileExt(FPathname));
  Highlighter := FConfiguration.GetHighlighter(Str);
  if Highlighter = nil then
    OnPaintTransient := nil;
end;

function TSynEditDiff.EncodingAsString(const Encoding: string): string;
begin
  Result := Encoding;
  if Pos('ANSI', Result) > 0 then
    Result := 'ANSI'
  else if Pos('ASCII', Result) > 0 then
    Result := 'ASCII'
  else if Pos('UTF-8', Result) > 0 then
    Result := 'UTF-8'
  else if Pos('UTF-16', Result) > 0 then
    Result := 'UTF-16'
  else if Pos('Unicode', Result) > 0 then
    Result := 'UTF-16';
end;

function TSynEditDiff.LinebreakAsString: string;
begin
  if FLineBreak = #13#10 then
    Result := 'Windows'
  else if FLineBreak = #10 then
    Result := 'Unix'
  else
    Result := 'Mac';
end;

procedure TSynEditDiff.LinesClearAll;
begin
  DeleteObjects(0, Lines.Count - 1);
  Lines.Clear;
  OnSpecialLineColors := nil;
  OnGutterGetText := nil;
end;

procedure TSynEditDiff.DeleteObjects(From, Till: Integer);
var
  LineObject: TLineObj;
begin
  IncPaintLock;
  for var I := From to Till do
  begin
    LineObject := GetLineObj(I);
    if Assigned(LineObject) then
    begin
      LineObject.Free;
      Lines.Objects[I] := nil;
    end;
  end;
  DecPaintLock;
end;

procedure TSynEditDiff.CreateObjects;
var
  LineObject: TLineObj;
begin
  if not FWithColoredLines then
    Exit;
  Lines.BeginUpdate;
  var Int := 0;
  while (Int < Lines.Count) and Assigned(GetLineObj(Int)) do
    Inc(Int);
  while (Int < Lines.Count) and (GetLineObj(Int) = nil) do
  begin
    LineObject := TLineObj.Create;
    LineObject.FSpezial := True;
    LineObject.FBackClr := FLineModClr;
    LineObject.FTag := Int + 1;
    Lines.Objects[Int] := LineObject;
    Inc(Int);
  end;
  Lines.EndUpdate;
  Invalidate;
end;

procedure TSynEditDiff.CreateObjects(From, Till, Tag: Integer);
var
  LineObject: TLineObj;
begin
  for var I := From to Till do
    if GetLineObj(I) = nil then
    begin
      LineObject := TLineObj.Create;
      LineObject.FSpezial := True;
      LineObject.FBackClr := FLineModClr;
      LineObject.FTag := Tag;
      if Tag > 0 then
        Inc(Tag);
      Lines.Objects[I] := LineObject;
    end;
end;

procedure TSynEditDiff.SynEditorReplaceText(Sender: TObject;
  const ASearch, AReplace: string; Line, Column: Integer;
  var AAction: TSynReplaceAction);

  function PointToDisplay(Posi: TPoint): TDisplayCoord;
  begin
    Result.Column := Posi.X;
    Result.Row := Posi.Y;
  end;

begin
  if ASearch = AReplace then
    AAction := raSkip
  else
  begin
    var
    APos := DisplayCoord(Column, Line);
    APos := PointToDisplay(ClientToScreen(RowColumnToPixels(APos)));
    var
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    with TFConfirmReplace.Create(Application) do
    begin
      PrepareShow(EditRect, APos.Column, APos.Row,
        APos.Row + LineHeight, ASearch);
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
end;

procedure TSynEditDiff.ShowFilename;
var
  Str: string;
  ACanvas: TCanvas;
begin
  ACanvas := (FMyOwner as TFTextDiff).Canvas;
  ACanvas.Font.Assign(FPCaption.Font);
  Str := ' ' + FPathname;
  if Modified then
    Str := Str + '*';
  FPCaption.Caption := MinimizeName(Str, ACanvas, FPCaption.Width);
end;

procedure TSynEditDiff.SetModified(Value: Boolean);
begin
  Modified := Value;
  ShowFilename;
end;

function TSynEditDiff.CopyToClipboardFT(From, Till: Integer): Boolean;
begin
  SelStart := RowColToCharIndex(BufferCoord(1, From + 1));
  if Till + 2 > Lines.Count then
    SelEnd := RowColToCharIndex(BufferCoord(Length(Lines[Lines.Count - 1]) + 1,
      Till + 1))
  else
    SelEnd := RowColToCharIndex(BufferCoord(1, Till + 2)) - 2;
  Result := (SelLength = 0);
  if Result then
    Clipboard.Clear
  else
    CopyToClipboard;
  SelLength := 0;
end;

procedure TSynEditDiff.PasteFromClipboardFT(EmptyClipboard: Boolean;
  From, Till, Num: Integer);
begin
  DeleteObjects(From, Till);
  SelStart := RowColToCharIndex(BufferCoord(1, From + 1));
  if Till + 2 > Lines.Count then
    SelEnd := RowColToCharIndex(BufferCoord(Length(Lines[Lines.Count - 1]) + 1,
      Till + 1))
  else
    SelEnd := RowColToCharIndex(BufferCoord(1, Till + 2)) - 2;
  try
    if EmptyClipboard then
    begin
      CutToClipboard;
      Clipboard.Clear;
    end
    else
      PasteFromClipboard;
  except on e: Exception do
    ErrorMsg(e.Message);
  end;
  CreateObjects(From, Till, Num);
  SetModified(True);
  Invalidate;
end;

procedure TSynEditDiff.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    FLineModClr := $878787;
    FYellowGray := $519595;
    FSilveryGray := $777777;
  end
  else
  begin
    FLineModClr := $E0E0E0;
    FYellowGray := $97734F;
    FSilveryGray := $16A231;
  end;
end;

procedure TSynEditDiff.SyncScroll(Sender: TObject; ScrollBar: TScrollBarKind);
begin
  (FMyOwner as TFTextDiff).SyncScroll(Self, ScrollBar);
end;

end.
